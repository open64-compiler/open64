/*

  Copyright (C) 2000, 2001 Silicon Graphics, Inc.  All Rights Reserved.

  This program is free software; you can redistribute it and/or modify it
  under the terms of version 2 of the GNU General Public License as
  published by the Free Software Foundation.

  This program is distributed in the hope that it would be useful, but
  WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  

  Further, this software is distributed without any warranty that it is
  free of the rightful claim of any third person regarding infringement 
  or the like.  Any license provided herein, whether implied or 
  otherwise, applies only to this software file.  Patent licenses, if 
  any, provided herein do not apply to combinations of this program with 
  other software, or any other product whatsoever.  

  You should have received a copy of the GNU General Public License along
  with this program; if not, write the Free Software Foundation, Inc., 59
  Temple Place - Suite 330, Boston MA 02111-1307, USA.

  Contact information:  Silicon Graphics, Inc., 1600 Amphitheatre Pky,
  Mountain View, CA 94043, or:

  http://www.sgi.com

  For further information regarding this notice, see:

  http://oss.sgi.com/projects/GenInfo/NoticeExplan

*/


/* ====================================================================
 * ====================================================================
 *
 * Module: wn2c_pragma.c
 * $Revision: 1.2 $
 * $Date: 02/11/07 23:42:00-00:00 $
 * $Author: fchow@keyresearch.com $
 * $Source: /scratch/mee/2.4-65/kpro64-pending/be/whirl2c/SCCS/s.wn2c_pragma.cxx $
 *
 * Revision history:
 *  12-Aug-96 - Original Version
 *
 * Description:
 *
 *   Translate a pragma WN node to Fortran!  The corresponding header
 *   declaration for for WN2C_pragma() can be found in wn2c_pragma.h.
 *
 * ====================================================================
 * ====================================================================
 */

#ifdef _KEEP_RCS_ID
/*REFERENCED*/
static char *rcs_id = "$Source: /scratch/mee/2.4-65/kpro64-pending/be/whirl2c/SCCS/s.wn2c_pragma.cxx $ $Revision: 1.2 $";
#endif /* _KEEP_RCS_ID */

#include "alloca.h"
#include "whirl2c_common.h"
#include "w2cf_parentize.h"  /* For W2CF_Get_Parent */
#include "pf_cg.h"
#include "region_util.h"     /* For RID and RID_map */
#include "PUinfo.h"          /* In be/whirl2c directory */
#include "wn2c.h"
#include "st2c.h"
#include "ty2c.h"
#include "tcon2c.h"
#include "wn2c_pragma.h"


extern BOOL Run_w2fc_early;     /* Defined in be.so */
extern BOOL W2C_Emit_Omp;      /* Defined in w2c_driver.cxx */


#define WN_pragma_nest(wn) WN_pragma_arg1(wn)
#define WN_max_nest_level(wn) WN_pragma_arg2(wn)
#define WN_mp_schedtype(wn) (WN_PRAGMA_SCHEDTYPE_KIND)WN_pragma_arg1(wn)

#define EMIT_ARG_NUMBERS1(tokens, val1) \
   Append_Arg_Numbers((tokens), (val1), -1)

#define EMIT_ARG_NUMBERS2(tokens, val1, val2) \
   Append_Arg_Numbers((tokens), (val1), (val2))

#define PARENTHESIZE_ARG_NUMBERS1(tokens, val1) \
   Append_Token_Special((tokens), '('); \
   EMIT_ARG_NUMBERS1((tokens), (val1)); \
   Append_Token_Special((tokens), ')')

#define PARENTHESIZE_ARG_NUMBERS2(tokens, val1, val2) \
   Append_Token_Special((tokens), '('); \
   EMIT_ARG_NUMBERS2((tokens), (val1), (val2)); \
   Append_Token_Special((tokens), ')')


typedef struct Array_Distribution
{
   INT       current_dimension;  /* Enumerated dimension number in C order */
   const WN *base;               /* PRAGMA starting description of this dim */
   const WN *cyclic_expr;        /* XPRAGMA holding a cyclic expr (or NULL) */
   const WN *dimension_bound;    /* XPRAGMA holding the bounds expr */
} ARRAY_DISTRIBUTION;


#define MAX_PRAGMAS_TO_SKIP 50
static struct Set_Of_Pragmas_To_Skip
{
   INT start, end;
   const WN *array[MAX_PRAGMAS_TO_SKIP];
} Pragmas_To_Skip = {0, 0, 
		     {NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,
		      NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,
		      NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,
		      NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,
		      NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL}};


typedef struct Local_Preg
{
   const ST *st;
   PREG_IDX  preg_idx;
} LOCAL_PREG;


/* ======================= Omp utilities ======================= */
/* ================================================================ */

inline BOOL
WN2C_is_omp(const WN *pragma)
{
   return (WN_pragma_omp(pragma) ||
	   (W2C_Emit_Omp && WN_pragma_compiler_generated(pragma)));
}


/* ======================= Static Functions ======================= */
/* ================================================================ */


static void 
WN2C_Stmt_Newline(TOKEN_BUFFER tokens,
		  SRCPOS       srcpos)
{
   if (W2C_Emit_Linedirs)
      Append_Srcpos_Directive(tokens, srcpos);
   Append_Indented_Newline(tokens, 1);
   if (W2C_File[W2C_LOC_FILE] != NULL)
      Append_Srcpos_Map(tokens, srcpos);
} /* WN2C_Stmt_Newline */


static void
WN2C_Append_Pragma_Newline(TOKEN_BUFFER tokens, SRCPOS srcpos)
{
   UINT current_indent = Current_Indentation();

   Set_Current_Indentation(0);
   WN2C_Stmt_Newline(tokens, srcpos);
   Append_Token_String(tokens, "#pragma");
   Set_Current_Indentation(current_indent);
} /* WN2C_Append_Pragma_Newline */


inline void
Append_Clause_Newline(TOKEN_BUFFER tokens, CONTEXT context)
{
   if (!CONTEXT_omp(context))
      WN2C_Append_Pragma_Newline(tokens, CONTEXT_srcpos(context));
} // Append_Clause_Newline


static void 
Append_Reduction_Operator(TOKEN_BUFFER tokens, OPERATOR op)
{
  /* 
   * appends a symbol representing an OMP reduction operator.
   */

  const char * p;

  switch(op) 
    {
    case OPR_ADD:
      p = "+";
      break;

    case OPR_SUB:
      p = "-";
      break;

    case OPR_MPY:
      p = "*";
      break;

    case OPR_BAND:
      p = "&";
      break;

    case OPR_BIOR:
      p = "|";
      break;

    case OPR_BXOR:
       p = "^";
      break;

    case OPR_LAND:
      p = "&&";
      break;

    case OPR_LIOR:
      p = "||";
      break;

    default:
      p = "?" ;
  }   
  Append_Token_String(tokens, p);
  Append_Token_Special(tokens,':');
} // Append_Reduction_Operator


static BOOL
Preg_Is_In_Clause_List(const WN *clause_list, const ST *preg_st, PREG_IDX preg_idx)
{
   /* Returns TRUE when the given preg is already referenced in a LOCAL,
    * LASTLOCAL or SHARED clause in the given clause list; otherwise we
    * return FALSE.
    */
   BOOL found = FALSE;

   while (!found && clause_list != NULL)
   {
      switch (WN_pragma(clause_list))
      {
      case WN_PRAGMA_LOCAL:
      case WN_PRAGMA_LASTLOCAL:
      case WN_PRAGMA_SHARED:
      case WN_PRAGMA_FIRSTPRIVATE:
      case WN_PRAGMA_REDUCTION:
	 if (WN_operator(clause_list) != OPR_XPRAGMA &&
	     WN_st(clause_list) == preg_st && 
	     WN_pragma_arg1(clause_list) == preg_idx)
	 {
	    found = TRUE;
	 }
	 break;
      default:
	 break;
      }
      clause_list = WN_next(clause_list);
   }
   return found;
} /* Preg_Is_In_Clause_List */


static void
Get_Implicit_Locals(WN_PRAGMA_ID  kind,              /* in */
		    const WN     *wn,                /* in */
		    const WN     *clauses,           /* in */
		    LOCAL_PREG  **ptr_to_local_list, /* out */
		    UINT         *next_local,        /* in/out */
		    UINT         *max_locals)        /* in/out */
{
   /* Puts the implicit locals into the local_list.  Every element of the
    * local list must represent a unique preg.
    */
   OPERATOR  opr = WN_operator(wn);
   const ST *st;
   PREG_IDX  preg_idx;

   /* Get the preg attributes (st, offset) if this is a preg reference
    */
   switch (opr)
   {
   case OPR_LDA:
      st = WN_st(wn);
      preg_idx = WN_lda_offset(wn);
      break;
   case OPR_LDID:
      st = WN_st(wn);
      preg_idx = WN_load_offset(wn);
      break;
   case OPR_STID:
      st = WN_st(wn);
      preg_idx = WN_store_offset(wn);
      break;
   default:
      st = NULL;
      preg_idx = 0;
   }

   /* Add a preg reference to the local_list.
    */
   if (st != NULL &&
       ST_sym_class(st) == CLASS_PREG &&
       !Preg_Is_In_Clause_List(clauses, st, preg_idx))
   {
      /* Unless the preg is already in the local list, add it.
       */
      INT  i;
      BOOL found = FALSE;
      LOCAL_PREG *local_list = *ptr_to_local_list;

      for (i = 0; !found && i < *next_local; i++)
	 if (local_list[i].st == st && local_list[i].preg_idx == preg_idx)
	    found = TRUE;

      if (!found)
      {
         if (*next_local >= *max_locals)
         {
            /* Need to reallocate the local_list buffer.  Use increments
             * of 200 elements for each reallocation.
             */
            *max_locals += 200;
            local_list = TYPE_ALLOC_N(LOCAL_PREG, *max_locals);

            /* Copy old values into new list, and free up the old list.
             */
            if (*ptr_to_local_list != NULL)
            {
               for (i = 0; i < *next_local; i++)
                  local_list[i] = (*ptr_to_local_list)[i];
               FREE(*ptr_to_local_list);
            }
            *ptr_to_local_list = local_list;
         }

	 local_list[*next_local].st = st;
	 local_list[*next_local].preg_idx = preg_idx;
	 (*next_local)++;
      }
   }

   /* Look for preg references in kids
    */
   if (!OPCODE_is_leaf(WN_opcode(wn)))
   {
      if (opr == OPR_REGION)
      {
	 /* Skip a pdo or a parallel_do inside a parallel region, since
	  * such nested regions will be handled independently.
	  *
	  * NO LONGER DO THIS, SINCE WE NO LONGER DO IMPLICIT SEARCHES
          * ON SUCH NESTED CONSTRUCTS.
	  *
	  * WN *pragma = WN_first(WN_region_pragmas(wn));
	  * if (kind != WN_PRAGMA_PARALLEL_BEGIN         ||
	  *   pragma == NULL                           ||
	  *   (WN_pragma(pragma) != WN_PRAGMA_PDO_BEGIN      && /may occur /
          *    WN_pragma(pragma) != WN_PRAGMA_PARALLEL_BEGIN && /impossible?/
          *    WN_pragma(pragma) != WN_PRAGMA_PARALLEL_DO    && /impossible?/
          *    WN_pragma(pragma) != WN_PRAGMA_DOACROSS))        /impossible?/
	  *{
	  */
	 Get_Implicit_Locals(kind, WN_region_body(wn), clauses, 
			     ptr_to_local_list, next_local, max_locals);
      }
      else if (opr == OPR_BLOCK)
      {
	 const WN *kid = WN_first(wn);
	 while (kid)
	 {
	    Get_Implicit_Locals(kind, kid, clauses, 
				ptr_to_local_list, next_local, max_locals);
	    kid = WN_next(kid);
	 }
      }
      else
      {
	 INT       kidno;
	 const WN *kid;
	 for (kidno=0; kidno < WN_kid_count(wn); kidno++)
	 {
	    kid = WN_kid (wn, kidno);
	    if (kid) 
	    { 
	       Get_Implicit_Locals(kind, kid, clauses, 
				   ptr_to_local_list, next_local, max_locals);
	    }
	 }
      }
   }
} /* Get_Implicit_Locals */


static void
Append_Implicit_Locals(TOKEN_BUFFER tokens,
		       WN_PRAGMA_ID region_kind,
		       const WN    *region_body,
		       const WN    *region_clauses,
		       CONTEXT      context)
{
   /* This will append implicit LOCAL clauses to the tokens, assuming
    * the regular clauses already have been appended to the tokens.
    */
   LOCAL_PREG *local_list = NULL;
   UINT        i, number_of_locals = 0, max_number_of_locals = 0;

   /* Get the list of implicit locals.
    */
   Get_Implicit_Locals(region_kind, region_body, region_clauses,
		       &local_list, &number_of_locals, &max_number_of_locals);

   /* Add make the implicit LOCAL clauses explicit in the token buffer
    */
   if (number_of_locals > 0)
   {
      Append_Clause_Newline(tokens, context);
      if (CONTEXT_omp(context))
	 Append_Token_String(tokens, "private");
      else
	 Append_Token_String(tokens, "local");
      Append_Token_Special(tokens, '(');
      for (i = 0; i < number_of_locals; i++)
      {
	 if (i > 0)
	    Append_Token_Special(tokens, ',');

	 ST2C_Use_Preg(tokens,
		       ST_type(local_list[i].st),
		       local_list[i].preg_idx,
		       context);
      }
      Append_Token_Special(tokens, ')');
   }

   if (local_list != NULL)
      FREE(local_list);
} /* Append_Implicit_Locals */


static void
WN2C_Value_Reference(TOKEN_BUFFER tokens, 
		     const WN *   expression, 
		     BOOL         prepend)
{
   CONTEXT      context = INIT_CONTEXT;
   TOKEN_BUFFER expr_tokens = New_Token_Buffer();
   TY_IDX       ty = WN_Tree_Type(expression);

   /* Emit memory reference
    */
   if (TY_Is_Pointer(ty))
   {
      TY_IDX object_ty;

      WN2C_memref_lhs(expr_tokens, 
		      &object_ty,               /* Type referenced (out) */
		      expression,               /* address */
		      0,                        /* offset in object */
		      ty,                       /* ref type of object */
		      TY_pointed(ty),           /* type of stored object */
		      TY_mtype(TY_pointed(ty)), /* base-type of object */
		      context);
   }
   else
      (void)WN2C_translate(expr_tokens, expression, context);

   if (prepend)
      Prepend_And_Reclaim_Token_List(tokens, &expr_tokens);
   else
      Append_And_Reclaim_Token_List(tokens, &expr_tokens);
} /* WN2C_Value_Reference */

static void
WN2C_Append_Value_Reference(TOKEN_BUFFER tokens, 
			    const WN *   expression, 
			    BOOL         prepend = FALSE)
{
   WN2C_Value_Reference(tokens, expression, FALSE/*prepend*/);
}

static void
WN2C_Prepend_Value_Reference(TOKEN_BUFFER tokens, 
			     const WN *   expression, 
			     BOOL         prepend = FALSE)
{
   WN2C_Value_Reference(tokens, expression, TRUE/*prepend*/);
}


static void
Append_MP_Schedtype(TOKEN_BUFFER tokens, WN_PRAGMA_SCHEDTYPE_KIND kind)
{
   switch (kind)
   {
   case WN_PRAGMA_SCHEDTYPE_RUNTIME:
      Append_Token_String(tokens, "runtime");
      break;
   case WN_PRAGMA_SCHEDTYPE_SIMPLE:
      Append_Token_String(tokens, "simple");
      break;
   case WN_PRAGMA_SCHEDTYPE_INTERLEAVE:
      Append_Token_String(tokens, "interleave");
      break;
   case WN_PRAGMA_SCHEDTYPE_DYNAMIC:
      Append_Token_String(tokens, "dynamic");
      break;
   case WN_PRAGMA_SCHEDTYPE_GSS:
      Append_Token_String(tokens, "gss");
      break;
   case WN_PRAGMA_SCHEDTYPE_PSEUDOLOWERED:
      Append_Token_String(tokens, "pseudolowered");
      break;
   default:
      Is_True(FALSE, 
	      ("Unexpected MP scheduling type (%d) in Append_MP_Schedtype()",
	       kind));
      break;
   }
} /* Append_MP_Schedtype */


static void
Append_Arg_Numbers(TOKEN_BUFFER tokens,
                   INT32        val1,
                   INT32        val2)
{
   if (val1 != -1)
      Append_Token_String(tokens, Number_as_String(val1, "%lld"));

   if (val2 != -1)
   {
      Append_Token_Special(tokens, ',');
      Append_Token_String(tokens, Number_as_String(val2, "%lld"));
   }
} /* Append_Arg_Numbers */

         
static void
Append_Prefetch_Attributes(TOKEN_BUFFER tokens, 
                           const WN    *prefetch,
                           INT32        size)
{
   INT pflag = WN_prefetch_flag(prefetch);

   /* Emit memory reference
    */
   Append_Token_Special(tokens, '=');
   WN2C_Append_Value_Reference(tokens, WN_kid0(prefetch));

   /* Emit stride and level clauses
    */
   Append_Token_Special(tokens, ',');
   if (PF_GET_STRIDE_1L(pflag) > 0)
   {
      if (PF_GET_STRIDE_2L(pflag) > 0)
      {
         Append_Token_String(tokens, 
            Concat2_Strings("stride=",
            Concat2_Strings(Number_as_String(PF_GET_STRIDE_1L(pflag), "%lld"),
            Concat2_Strings(",",
                        Number_as_String(PF_GET_STRIDE_2L(pflag), "%lld")))));
         Append_Token_Special(tokens, ',');
         Append_Token_String(tokens, "level=1,2");
      }
      else
      {
         Append_Token_String(tokens, 
            Concat2_Strings("stride=",
               Number_as_String(PF_GET_STRIDE_1L(pflag), "%lld")));
         Append_Token_Special(tokens, ',');
         Append_Token_String(tokens, "level=1");
      }
   }
   else if (PF_GET_STRIDE_2L(pflag) > 0)
   {
      Append_Token_String(tokens, 
            Concat2_Strings("stride=,",
               Number_as_String(PF_GET_STRIDE_2L(pflag), "%lld")));
         Append_Token_Special(tokens, ',');
         Append_Token_String(tokens, "level=,2");
   }
   else
   {
      Append_Token_String(tokens, "stride=");
      Append_Token_Special(tokens, ',');
      Append_Token_String(tokens, "level=");
   }

   /* Emit a kind clause
    */
   Append_Token_Special(tokens, ',');
   if (PF_GET_READ(pflag))
      Append_Token_String(tokens, "kind=rd");
   else
      Append_Token_String(tokens, "kind=wr");

   /* Emit a size clause
    */
   if (size > 0)
   {
      Append_Token_Special(tokens, ',');
      Append_Token_String(tokens, 
         Concat2_Strings("size=", Number_as_String(size, "%lld")));
   }
} /* Append_Prefetch_Attributes */


static void
Append_Distribution(TOKEN_BUFFER tokens, const WN **apragma, WN_PRAGMA_ID id)
{
   INT32               dim, num_dims;
   ARRAY_DISTRIBUTION  distr[MAX_PRAGMAS_TO_SKIP];
   const WN           *wn = *apragma;
   CONTEXT             context = INIT_CONTEXT;

   Is_True(WN_operator(wn) == OPR_PRAGMA, 
	   ("Unexpected operator (%d) in Append_Distribution()",
	    WN_operator(wn)));

   /* Accumulate the distribution kind for each dimension.
    */
   for (num_dims = 0; 
	(WN_operator(wn) == OPR_PRAGMA &&
	 WN_pragma(wn) == id           &&
	 num_dims == WN_pragma_index(wn));
	num_dims++)
   {
      /* In reverese order of pragma sequence */
      distr[num_dims].current_dimension = WN_pragma_index(wn);
      distr[num_dims].base = wn;
      if (WN_pragma_distr_type(wn) == DISTRIBUTE_CYCLIC_EXPR)
         distr[num_dims].cyclic_expr = wn = WN_next(wn);
      distr[num_dims].dimension_bound = wn = WN_next(wn);
      wn = WN_next(wn);
   }

   /* Skip two stores, which are generated purely for dependency analysis
    * purposes.
    */
   if (WN_operator(wn)==OPR_STID && ST_sym_class(WN_st(wn))==CLASS_PREG)
   {
      wn = WN_next(wn);
      if (WN_operator(wn)==OPR_STID && ST_sym_class(WN_st(wn))==CLASS_PREG)
         wn = WN_next(wn);
   }
   *apragma = wn;

   /* Translate the sequence of distribution kinds, in C order, i.e.
    * in the order of the WHIRL representation.
    */
   for (dim = 0; dim < num_dims; dim++)
   {
      Append_Token_Special(tokens, '[');
      switch (WN_pragma_distr_type(distr[dim].base))
      {
      case DISTRIBUTE_STAR:
         Append_Token_Special(tokens, '*');
         break;

      case DISTRIBUTE_BLOCK:
         Append_Token_String(tokens, "block");
         break;

      case DISTRIBUTE_CYCLIC_EXPR:
         Append_Token_String(tokens, "cyclic");
         Append_Token_Special(tokens, '(');
         WN2C_translate(tokens, WN_kid0(distr[dim].cyclic_expr), context);
         Append_Token_Special(tokens, ')');
         break;

      case DISTRIBUTE_CYCLIC_CONST:
         Append_Token_String(tokens, "cyclic");
         PARENTHESIZE_ARG_NUMBERS1(tokens, 
                                   WN_pragma_preg(distr[dim].base));
         break;

      default:
         Append_Token_String(tokens, "unknown_distribution");
         break;
      }
      Append_Token_Special(tokens, ']');

   } /* For each dimension */
} /* Append_Distribution */


static void
Append_A_Clause_Symbol(TOKEN_BUFFER tokens, const WN *clause, WN_OFFSET ofst)
{
   CONTEXT         context = INIT_CONTEXT;
   const ST *const st = WN_st(clause);

   if (ST_sym_class(st) == CLASS_PREG)
   {
      INT32 preg_num = WN_pragma_arg1(clause);

      ST2C_Use_Preg(tokens, ST_type(st), preg_num, context);
   }
   else
   {
      TY_IDX       object_ty;
      const TY_IDX base_ty = ST_type(st);
      TOKEN_BUFFER sym_tokens = New_Token_Buffer();

      WN2C_stid_lhs(sym_tokens,
		    &object_ty,
		    st,        /* base variable */
		    ofst,      /* base offset */
		    base_ty,   /* type of reference */
		    TY_mtype(base_ty),
		    context);
      Append_And_Reclaim_Token_List(tokens, &sym_tokens);
   }
} /* Append_A_Clause_Symbol */


static void
Append_Clause_Symbols(TOKEN_BUFFER tokens,
		      WN_PRAGMA_ID id,
                      const WN   **next)
{
   /* Loop through the pragmas, and emit a ',' separated list
    * of the ST attributes for all contiguous pragmas with the 
    * given "id".  Terminate upon reaching a pragma with a
    * different "id" or the end of the pragma list. Set *next to
    * point to the next node after the last one processed here.
    */
   const WN *clause;

   Is_True(WN_operator(*next) == OPR_PRAGMA, 
	   ("Unexpected operator (%d) in Append_Clause_Symbols()", 
	    WN_operator(*next)));

   Append_Token_Special(tokens, '(');
   for (clause = *next;
	(clause != NULL                    && 
	 WN_operator(clause) == OPR_PRAGMA && 
	 WN_pragma(clause) == id);
	clause = WN_next(clause))
   {
      if (clause != *next)
	 Append_Token_Special(tokens, ',');

      Append_A_Clause_Symbol(tokens,  clause, 0);
   }
   Append_Token_Special(tokens, ')');
   *next = clause;
} /* Append_Clause_Symbols */


static void
Append_Reduction_Clause(TOKEN_BUFFER tokens,
			WN_PRAGMA_ID id,
			const WN   **next)
{
   /* Loop through the pragmas, and emit a ',' separated list
    * of the reduction operator and ST attributes for all 
    * contiguous pragmas with the given "id".  
    */
   const WN *       clause;
   const WN * const first_clause = *next;

   Is_True(WN_operator(first_clause) == OPR_PRAGMA, 
	   ("Unexpected operator (%d) in Append_Reduction_Clause()", 
	    WN_operator(first_clause)));

   Append_Token_String(tokens, "reduction (");
   for (clause = first_clause;
	(clause != NULL                    && 
	 WN_operator(clause) == OPR_PRAGMA && 
	 WN_pragma(clause) == id);
	clause = WN_next(clause))
   {
      if (WN2C_is_omp(clause) &&  
	  WN_pragma(clause) == WN_PRAGMA_REDUCTION &&
	  WN_pragma_arg2(clause) != OPERATOR_UNKNOWN) 
      {
	 if (first_clause != clause) 
	    Append_Token_String(tokens, "), reduction (");
	 Append_Reduction_Operator(tokens, (OPERATOR) WN_pragma_arg2(clause));
	 
      } 
      else if (clause != first_clause) 
	 Append_Token_Special(tokens, ',');

      Append_A_Clause_Symbol(tokens, clause, 0);
   }

   Append_Token_Special(tokens, ')');
   *next = clause;

} /* Append_Reduction_Clause */


static void
Append_Clause_Expressions(TOKEN_BUFFER tokens,
			  WN_PRAGMA_ID id,
			  const WN   **next,
			  BOOL         reverse_order = FALSE)
{
   /* Loop through the pragmas, and emit a ',' separated list
    * of the ST attributes for all contiguous pragmas with the 
    * given "id".  Terminate upon reaching a pragma with a
    * different "id" or the end of the pragma list. Set *next to
    * point to the next node after the last one processed here.
    */
   TOKEN_BUFFER     clause_tokens = New_Token_Buffer();
   const WN *       clause;
   const WN * const first_clause = *next;

   Is_True(WN_operator(first_clause) == OPR_XPRAGMA,
	   ("Unexpected operator %d in Append_Clause_Expressions()", 
	    WN_operator(first_clause)));

   Append_Token_Special(tokens, '(');
   for (clause = first_clause;
	(clause != NULL && 
	 WN_operator(clause) == OPR_XPRAGMA && 
	 WN_pragma(clause) == id);
	clause = WN_next(clause))
   {
      if (clause != first_clause)
      {
	 if (reverse_order)
	    Prepend_Token_Special(clause_tokens,  ',');
	 else
	    Append_Token_Special(clause_tokens, ',');
      }

      if (id == WN_PRAGMA_ONTO && 
	  WN_operator(WN_kid0(clause)) == OPR_INTCONST &&
	  WN_const_val(WN_kid0(clause)) == 0)
      {
	 /* Special case!
	  */
	 if (reverse_order)
	    Prepend_Token_Special(clause_tokens,  '*');
	 else
	    Append_Token_Special(clause_tokens, '*');
      }
      else
      {
	 if (reverse_order)
	    WN2C_Prepend_Value_Reference(tokens, WN_kid0(clause));
	 else
	    WN2C_Append_Value_Reference(tokens, WN_kid0(clause));
      }
   }
   Append_And_Reclaim_Token_List(tokens, &clause_tokens);
   Append_Token_Special(tokens, ')');
   *next = clause;
} /* Append_Clause_Expressions */

	
static void
Append_Array_Segment(TOKEN_BUFFER tokens,
		     WN_PRAGMA_ID id,
		     const WN   **next)
{
   /* Loop through the pragmas, and emit a ',' separated list
    * of the ST attributes for all contiguous pragmas with the 
    * given "id".  Terminate upon reaching a pragma with a
    * different "id" or the end of the pragma list. Set *next to
    * point to the next node after the last one processed here.
    */
   const WN *clause;

   Is_True(WN_operator(*next) == OPR_XPRAGMA,
	   ("Unexpected operator %d in Append_Array_Segment()", 
	    WN_operator(*next)));

   Append_Token_Special(tokens, '(');
   for (clause = *next;
	(clause != NULL && 
	 WN_operator(clause) == OPR_XPRAGMA && 
	 WN_pragma(clause) == id);
	clause = WN_next(clause))
   {
      if (clause != *next)
	 Append_Token_Special(tokens, ',');

      Append_A_Clause_Symbol(tokens,  clause, 0);
      Append_Token_Special(tokens, '(');
      EMIT_ARG_NUMBERS1(tokens, 0);
      Append_Token_Special(tokens, ':');
      WN2C_Append_Value_Reference(tokens, WN_kid0(clause));
      Append_Token_Special(tokens, '-'); // Subtract 1, since expr is at base 1
      EMIT_ARG_NUMBERS1(tokens, 1);
      Append_Token_Special(tokens, ')');
   }
   Append_Token_Special(tokens, ')');

   *next = clause;
} /* Append_Array_Segment */

	    
static void
Append_Nest_Clauses(TOKEN_BUFFER tokens, 
		    const WN    *nest_region, 
		    INT          nest_levels,
		    CONTEXT      context)
{
   BOOL         pattern_error = FALSE;
   INT          nest;
   const ST    *idx_var;
   TY_IDX       idx_ty;
   const WN    *next_stmt = nest_region;
   WN_PRAGMA_ID nest_kind = WN_PRAGMA_UNDEFINED;
   TOKEN_BUFFER nest_tokens = New_Token_Buffer();

   Is_True(next_stmt != NULL &&
	   WN_operator(next_stmt) == OPR_REGION &&
	   WN_first(WN_region_pragmas(next_stmt)) != NULL,
	   ("Unexpected top-level structure for Append_Nest_Clauses()"));

   nest_kind = 
      (WN_PRAGMA_ID)WN_pragma(WN_first(WN_region_pragmas(next_stmt)));

   Append_Clause_Newline(tokens, context);
   Append_Token_String(nest_tokens, "nest");
   Append_Token_Special(nest_tokens, '(');
   CONTEXT_reset(context);
   for (nest = 1; !pattern_error && nest <= nest_levels; nest++)
   {
      /* Get the next nested loop, assuming next_stmt at this point
       * refers to a region.
       */
      next_stmt = WN_first(WN_region_body(next_stmt));
      while (next_stmt != NULL && WN_operator(next_stmt) != OPR_DO_LOOP)
	 next_stmt = WN_next(next_stmt);

      if (next_stmt == NULL)
	 pattern_error = TRUE;
      else
      {
	 /* Write out the index variable (or preg).
	  */
	 idx_var = WN_st(WN_index(next_stmt));
	 idx_ty = ST_type(idx_var);
	 if (ST_sym_class(idx_var) == CLASS_PREG)
	 {
	    ST2C_Use_Preg(nest_tokens,
			  idx_ty,
			  WN_idname_offset(WN_index(next_stmt)),
			  context);
	 }
	 else
	 {
	    TY_IDX       object_ty;
	    TOKEN_BUFFER sym_tokens = New_Token_Buffer();

	    WN2C_stid_lhs(sym_tokens,
			  &object_ty,
			  idx_var,          /* base variable */
			  WN_idname_offset(WN_index(next_stmt)),
			  idx_ty,           /* type of reference */
			  TY_mtype(idx_ty),
			  context);
	    Append_And_Reclaim_Token_List(nest_tokens, &sym_tokens);
	 }
	 
	 /* Emit separator, and search for the next nested region, if 
	  * any is expected.
	  */
	 if (nest < nest_levels)
	 {
	    Append_Token_Special(nest_tokens, ',');

	    next_stmt = WN_first(WN_do_body(next_stmt));
	    while (next_stmt != NULL && 
		   WN_operator(next_stmt) != OPR_REGION)
	       next_stmt = WN_next(next_stmt);

	    if (next_stmt == NULL                              ||
		WN_first(WN_region_pragmas(next_stmt)) == NULL ||
		WN_pragma(WN_first(WN_region_pragmas(next_stmt))) != 
		nest_kind)
	       pattern_error = TRUE;
	 }
      }
   }
   Append_Token_Special(nest_tokens, ')');

   if (!pattern_error)
      Append_And_Reclaim_Token_List(tokens, &nest_tokens);
} /* Append_Nest_Clauses */


static void
Skip_Pragma_Clauses(const WN **clause_list,  
                    CONTEXT    context)
{
   /* Also change Append_Pragma_Clauses() when changing this.
    */
   const WN *clause = *clause_list;
   BOOL      more;

   more = (clause != NULL && 
	   (WN_operator(clause) == OPR_PRAGMA ||
	    WN_operator(clause) == OPR_XPRAGMA));

   while (more)
   {
      switch (WN_pragma(clause))
      {
      case WN_PRAGMA_AFFINITY:
      case WN_PRAGMA_DATA_AFFINITY:
      case WN_PRAGMA_THREAD_AFFINITY:
      case WN_PRAGMA_CHUNKSIZE:
      case WN_PRAGMA_IF:
      case WN_PRAGMA_LASTLOCAL:
      case WN_PRAGMA_LOCAL:
      case WN_PRAGMA_MPSCHEDTYPE:
      case WN_PRAGMA_ORDERED:
      case WN_PRAGMA_REDUCTION:
      case WN_PRAGMA_SHARED:
      case WN_PRAGMA_ONTO:
      case WN_PRAGMA_LASTTHREAD:
      case WN_PRAGMA_MPNUM:
      case WN_PRAGMA_SYNC_DOACROSS:
      case WN_PRAGMA_FIRSTPRIVATE:
      case WN_PRAGMA_NOWAIT: // We put "nowait" on the pragma itself for C
         clause = WN_next(clause);
         break;

      default:
	 more = FALSE;
	 break;
      } /* switch */

      more = (more &&
	      clause != NULL && 
	      (WN_operator(clause) == OPR_PRAGMA ||
	       WN_operator(clause) == OPR_XPRAGMA));
   } /* for each attribute pragma */

   *clause_list = clause;
} /* Skip_Pragma_Clauses */

 
static void 
Skip_Ignored_Clauses(const WN *following_clauses, const WN **next_clause)
{
   BOOL skipped = TRUE;

   while (skipped && *next_clause != following_clauses)
   {
      switch (WN_pragma(*next_clause))
      {
      case WN_PRAGMA_DATA_AFFINITY:
      case WN_PRAGMA_THREAD_AFFINITY:
      case WN_PRAGMA_MPNUM:
      case WN_PRAGMA_SYNC_DOACROSS:
	 *next_clause = WN_next(*next_clause);
	 break;
      default:
	 skipped = FALSE;
	 break;
      }
   }
} /* Skip_Ignored_Clauses */


static void
Append_Pragma_Clauses(TOKEN_BUFFER tokens, 
		      const WN   **clause_list,  
		      CONTEXT      context)
{
   /* Loop through the sequence of pragmas, emitting those representing
    * attributes to another (already emitted) pragma.  Terminate upon
    * reaching a non-attribute pragma or the end of the pragma list.
    * Also change Skip_Pragma_Clauses() when changing this.  Update the
    * clause_list, such that it denotes the item following the last one
    * processed here.  In C we can always emit clauses on a separate line,
    * as a subsequent pragma, so we take that approach here, with exception
    * of "omp" pragmas.
    */
   const WN   *next;
   const WN   *clause = *clause_list;
   const WN   *wn_after_clauses = *clause_list;

   Skip_Pragma_Clauses(&wn_after_clauses, context);
   while (clause != wn_after_clauses)
   {
      /* Note: In C we do not separate clauses by commas.
       */
      BOOL omp = CONTEXT_omp(context) || WN2C_is_omp(clause);
      
      next = clause;
      switch (WN_pragma(clause))
      {
      case WN_PRAGMA_DATA_AFFINITY:
      case WN_PRAGMA_THREAD_AFFINITY:
      case WN_PRAGMA_MPNUM:
      case WN_PRAGMA_SYNC_DOACROSS:
	 break; /* Ignore these */

      case WN_PRAGMA_NOWAIT:
	 Append_Clause_Newline(tokens, context);
	 Append_Token_String(tokens, "nowait");
	 clause = WN_next(clause);
	 break;

      case WN_PRAGMA_AFFINITY:
	 Append_Clause_Newline(tokens, context);
	 Append_Token_String(tokens, "affinity");
	 Append_Clause_Expressions(tokens, WN_PRAGMA_AFFINITY, &clause);

	 Append_Token_Special(tokens, '=');
	 if (WN_pragma(clause) == WN_PRAGMA_DATA_AFFINITY)
	    Append_Token_String(tokens, "data");
	 else if (WN_pragma(clause) == WN_PRAGMA_THREAD_AFFINITY)
	    Append_Token_String(tokens, "thread");
	 else
	    Is_True(FALSE, 
		    ("Unexpected clause kind (%d) in Append_Pragma_Clauses()",
		     WN_pragma(clause)));

         /* Process the expression associated with the thread/data affinity
          * pragma.
          */
	 Append_Token_Special(tokens, '(');
	 WN2C_Append_Value_Reference(tokens, WN_kid0(clause));
	 Append_Token_Special(tokens, ')');
	 clause = WN_next(clause);
	 break;

      case WN_PRAGMA_CHUNKSIZE:
	 Append_Clause_Newline(tokens, context);
	 Append_Token_String(tokens, "chunksize");
	 Append_Clause_Expressions(tokens, WN_PRAGMA_CHUNKSIZE, &clause);
	 break;

      case WN_PRAGMA_IF:
	 Append_Clause_Newline(tokens, context);
	 Append_Token_String(tokens, "if");
	 Append_Clause_Expressions(tokens, WN_PRAGMA_IF, &clause);
	 break;

      case WN_PRAGMA_LASTLOCAL:
	 Append_Clause_Newline(tokens, context);
	 if (omp)
	    Append_Token_String(tokens, "lastprivate");
	 else
	    Append_Token_String(tokens, "lastlocal");
	 Append_Clause_Symbols(tokens, WN_PRAGMA_LASTLOCAL, &clause);
	 break;

      case WN_PRAGMA_LOCAL:
	 Append_Clause_Newline(tokens, context);
	 if (omp)
	    Append_Token_String(tokens, "private");
	 else
	    Append_Token_String(tokens, "local");
	 if (WN_operator(clause) == OPR_XPRAGMA)
	 {
	    Append_Array_Segment(tokens, WN_PRAGMA_LOCAL, &clause);
	 }
	 else
	 {
	    Append_Clause_Symbols(tokens, WN_PRAGMA_LOCAL, &clause);
	 }
	 break;

      case WN_PRAGMA_MPSCHEDTYPE:
	 /* Can be both a clause and a pragma */
	 if (omp)
	 {
	    Append_Clause_Newline(tokens, context);
	    Append_Token_String(tokens, "schedule");
	    Append_Token_Special(tokens, '(');
	    Append_MP_Schedtype(tokens, WN_mp_schedtype(clause));
	    if (WN_next(clause) != NULL &&
		WN_pragma(WN_next(clause)) == WN_PRAGMA_CHUNKSIZE)
	    {
	       clause = WN_next(clause);
	       Append_Token_Special(tokens, ',');
	       WN2C_Append_Value_Reference(tokens, WN_kid0(clause));
	    }
	    Append_Token_Special(tokens, ')');
	    clause = WN_next(clause);
	 }
	 else
	 {
	    Append_Token_String(tokens, "schedtype");
	    Append_Token_Special(tokens, '(');
	    Append_MP_Schedtype(tokens, WN_mp_schedtype(clause));
	    Append_Token_Special(tokens, ')');
	 }
	 break;

      case WN_PRAGMA_ORDERED:
	 Append_Clause_Newline(tokens, context);
	 if (omp)
	    Append_Token_String(tokens, "(ordered)");
	 else
	    Append_Token_String(tokens, "ordered");
	 break;

      case WN_PRAGMA_REDUCTION:
	 Append_Clause_Newline(tokens, context);
	 if (WN_operator(clause) == OPR_XPRAGMA)
	 {
	    Append_Token_String(tokens, "reduction");
	    Append_Clause_Expressions(tokens, WN_PRAGMA_REDUCTION, &clause);
	 }
	 else
	 {
	    Append_Reduction_Clause(tokens, WN_PRAGMA_REDUCTION, &clause);
	 }
	 break;

      case WN_PRAGMA_SHARED:
	 Append_Clause_Newline(tokens, context);
	 Append_Token_String(tokens, "shared");
	 Append_Clause_Symbols(tokens, WN_PRAGMA_SHARED, &clause);
	 break;

      case WN_PRAGMA_ONTO:
	 Append_Clause_Newline(tokens, context);
	 Append_Token_String(tokens, "onto");
         Append_Clause_Expressions(tokens, WN_PRAGMA_ONTO, &clause);
	 break;

      case WN_PRAGMA_LASTTHREAD:
	 Append_Clause_Newline(tokens, context);
	 Append_Token_String(tokens, "lastthread");
	 Append_Clause_Symbols(tokens, WN_PRAGMA_LASTTHREAD, &clause);
	 break;

      case WN_PRAGMA_FIRSTPRIVATE:
	 Append_Token_String(tokens, "firstprivate");
	 Append_Clause_Symbols(tokens, WN_PRAGMA_FIRSTPRIVATE, &clause);
	 break;

      default:
	 Is_True(FALSE,
		 ("Unexpected pragma id in Append_Pragma_Clauses()"));
	 break;
      } /* switch */

      /* See if we have already advanced to the next pragma, e.g. as a result
       * of calling Append_Clause_Expressions() or Append_Clause_Symbols(),
       * and if not so, then advance to the next pragma.
       */
      if (next == clause)
         clause = WN_next(clause);

      Skip_Ignored_Clauses(wn_after_clauses, &clause);
   } /* for each attribute pragma */

   *clause_list = clause;
} /* Append_Pragma_Clauses */


static void
Emit_To_PUinfo_Pragmas(const WN **next, CONTEXT context)
{
   /* This is a special handler for pragmas that must be taken out of
    * a statement list context and instead must be appended to the 
    * PUinfo_pragmas list.
    */
   TOKEN_BUFFER tokens = New_Token_Buffer();

   Is_True(WN_operator(*next) == OPR_PRAGMA ||
	   WN_operator(*next) == OPR_XPRAGMA, 
	   ("Unexpected operator (%d) in WN2C_pragma()", WN_operator(*next)));

   switch (WN_pragma(*next))
   {
   case WN_PRAGMA_DISTRIBUTE:
      WN2C_Append_Pragma_Newline(tokens, CONTEXT_srcpos(context));
      Append_Token_String(tokens, "distribute");
      Append_A_Clause_Symbol(tokens, *next, 0/*ofst*/);
      Append_Distribution(tokens, next, WN_PRAGMA_DISTRIBUTE);
      Append_Pragma_Clauses(tokens, next, context);
      break;

   case WN_PRAGMA_DISTRIBUTE_RESHAPE:
      WN2C_Append_Pragma_Newline(tokens, CONTEXT_srcpos(context));
      Append_Token_String(tokens, "distribute reshape");
      Append_A_Clause_Symbol(tokens, *next, 0/*ofst*/);
      Append_Distribution(tokens, next, WN_PRAGMA_DISTRIBUTE_RESHAPE);
      Append_Pragma_Clauses(tokens, next, context);
      break;

   default:
      Is_True(FALSE,
              ("Unexpected pragma id in Emit_To_PUinfo_Pragmas()"));
      break;
   }
   Prepend_And_Reclaim_Token_List(PUinfo_pragmas, &tokens);
} /* Emit_To_PUinfo_Pragmas */


static const WN *
Get_Enclosing_Parallel_Region(const WN *construct)
{
   WN *found_parallel = NULL;

   construct = W2CF_Get_Parent(construct);
   while (found_parallel == NULL && construct != NULL)
   {
      if (WN_operator(construct) == OPR_REGION)
      {
	 WN *pragma = WN_first(WN_region_pragmas(construct));
	 if (WN_pragma(pragma) == WN_PRAGMA_PARALLEL_BEGIN)
	    found_parallel = pragma;
      }
      construct = W2CF_Get_Parent(construct);
   }
   return found_parallel;
} /* Get_Enclosing_Parallel_Region */


static void
WN2C_process_pragma(TOKEN_BUFFER tokens, const WN **next, CONTEXT context)
{
   /* This procedure will translate the "next" pragma and and any associated
    * clauses, such that "next" end up pointing to the WN* after the pragma
    * and clauses.
    */
   const WN *apragma = *next;
   const WN *this_pragma = apragma;
   const WN *first_clause;
   const WN *surrounding_region;

   Is_True(WN_operator(apragma) == OPR_PRAGMA ||
           WN_operator(apragma) == OPR_XPRAGMA,
	   ("Invalid operator for WN2C_process_pragma()"));

   // Set the context to correctly handle nested clauses for this
   // pragma.
   //
   if (WN2C_is_omp(apragma))
      CONTEXT_set_omp(context);
   
   switch (WN_pragma(apragma))
   {
   case WN_PRAGMA_INLINE_DEPTH:
      WN2C_Append_Pragma_Newline(tokens, CONTEXT_srcpos(context));
      Append_Token_String(tokens,"inline_depth");
      Append_Token_Special(tokens,'(');
      EMIT_ARG_NUMBERS1(tokens, WN_pragma_arg1(apragma));
      Append_Token_Special(tokens,')');
      break;

   case WN_PRAGMA_AGGRESSIVE_INNER_LOOP_FISSION:
      WN2C_Append_Pragma_Newline(tokens, CONTEXT_srcpos(context));
      Append_Token_String(tokens,"aggressive inner loop fission");
      break;

   case WN_PRAGMA_FISSION:
      WN2C_Append_Pragma_Newline(tokens, CONTEXT_srcpos(context));
      Append_Token_String(tokens,"fission");
      PARENTHESIZE_ARG_NUMBERS1(tokens, WN_pragma_arg1(apragma));
      break;

   case WN_PRAGMA_FISSIONABLE:
      WN2C_Append_Pragma_Newline(tokens, CONTEXT_srcpos(context));
      Append_Token_String(tokens,"fissionable");
      break;

   case WN_PRAGMA_FUSE:
      WN2C_Append_Pragma_Newline(tokens, CONTEXT_srcpos(context));
      Append_Token_String(tokens,"fuse");
      PARENTHESIZE_ARG_NUMBERS2(tokens, 
                                WN_pragma_arg1(apragma),
                                WN_pragma_arg2(apragma));
      break;

   case WN_PRAGMA_FUSEABLE:
      WN2C_Append_Pragma_Newline(tokens, CONTEXT_srcpos(context));
      Append_Token_String(tokens,"fusable");
      break;

   case WN_PRAGMA_NO_FISSION:
      WN2C_Append_Pragma_Newline(tokens, CONTEXT_srcpos(context));
      Append_Token_String(tokens,"no fission");
      break;

   case WN_PRAGMA_NO_FUSION:
      WN2C_Append_Pragma_Newline(tokens, CONTEXT_srcpos(context));
      Append_Token_String(tokens,"no fusion");
      break; 

   case WN_PRAGMA_INTERCHANGE:
      WN2C_Append_Pragma_Newline(tokens, CONTEXT_srcpos(context));
      Append_Token_String(tokens, "interchange");
      Append_Clause_Symbols(tokens, (WN_PRAGMA_ID)WN_pragma(apragma),
                            &apragma);
      break;

   case WN_PRAGMA_NO_INTERCHANGE:
      WN2C_Append_Pragma_Newline(tokens, CONTEXT_srcpos(context));
      Append_Token_String(tokens, "no interchange");
      break;

   case WN_PRAGMA_BLOCKING_SIZE:
      WN2C_Append_Pragma_Newline(tokens, CONTEXT_srcpos(context));
      Append_Token_String(tokens, "blocking size");
      PARENTHESIZE_ARG_NUMBERS2(tokens, 
                                WN_pragma_arg1(apragma),
                                WN_pragma_arg2(apragma));
      break;

   case WN_PRAGMA_NO_BLOCKING:
      WN2C_Append_Pragma_Newline(tokens, CONTEXT_srcpos(context));
      Append_Token_String(tokens, "no blocking");
      break;

   case WN_PRAGMA_UNROLL:
      WN2C_Append_Pragma_Newline(tokens, CONTEXT_srcpos(context));
      Append_Token_String(tokens, "unroll");
      PARENTHESIZE_ARG_NUMBERS2(tokens, 
                                WN_pragma_arg1(apragma), 
                                WN_pragma_arg2(apragma));
      break;

   case WN_PRAGMA_BLOCKABLE:
      WN2C_Append_Pragma_Newline(tokens, CONTEXT_srcpos(context));
      Append_Token_String(tokens, "blockable");
      Append_Clause_Symbols(tokens, (WN_PRAGMA_ID)WN_pragma(apragma),
                            &apragma);
      break;

   case WN_PRAGMA_PREFETCH:
      WN2C_Append_Pragma_Newline(tokens, CONTEXT_srcpos(context));
      Append_Token_String(tokens, "prefetch");
      PARENTHESIZE_ARG_NUMBERS2(tokens, 
                                WN_pragma_arg1(apragma),
                                WN_pragma_arg2(apragma));
      break;

   case WN_PRAGMA_PREFETCH_MANUAL:
      WN2C_Append_Pragma_Newline(tokens, CONTEXT_srcpos(context));
      Append_Token_String(tokens, "prefetch_manual");
      PARENTHESIZE_ARG_NUMBERS1(tokens, WN_pragma_arg1(apragma));
      break;

   case WN_PRAGMA_PREFETCH_REF:
      if (WN_next(apragma) != NULL && 
          WN_operator(WN_next(apragma)) == OPR_PREFETCH)
      {
         WN2C_Append_Pragma_Newline(tokens, CONTEXT_srcpos(context));
         Append_Token_String(tokens, "prefetch_ref");
         Append_Prefetch_Attributes(tokens, 
                                    WN_next(apragma),
                                    WN_pragma_arg2(apragma));
      }
      break;

   case WN_PRAGMA_PREFETCH_REF_DISABLE:
      WN2C_Append_Pragma_Newline(tokens, CONTEXT_srcpos(context));
      Append_Token_String(tokens, "prefetch_ref_disable");
      Append_Token_Special(tokens, '=');
      Append_A_Clause_Symbol(tokens, apragma, 0/*ofst*/);
      if (WN_pragma_arg2(apragma) > 0)
      {
         Append_Token_Special(tokens, ',');
         Append_Token_String(tokens, "size");
         Append_Token_Special(tokens, '=');
         EMIT_ARG_NUMBERS1(tokens, WN_pragma_arg2(apragma));
      }         
      break;
      
   case WN_PRAGMA_DISTRIBUTE:
      Emit_To_PUinfo_Pragmas(&apragma, context);
      break;
      
   case WN_PRAGMA_REDISTRIBUTE:
      WN2C_Append_Pragma_Newline(tokens, CONTEXT_srcpos(context));
      Append_Token_String(tokens, "redistribute");
      Append_A_Clause_Symbol(tokens, apragma, 0/*ofst*/);
      Append_Distribution(tokens, &apragma, WN_PRAGMA_REDISTRIBUTE);
      Append_Pragma_Clauses(tokens, &apragma, context);
      break;
      
   case WN_PRAGMA_DISTRIBUTE_RESHAPE:
      Emit_To_PUinfo_Pragmas(&apragma, context);
      break;
      
   case WN_PRAGMA_DYNAMIC:
      WN2C_Append_Pragma_Newline(tokens, CONTEXT_srcpos(context));
      Append_Token_String(tokens, "dynamic");
      Append_A_Clause_Symbol(tokens, apragma, 0/*ofst*/);
      break;

   case WN_PRAGMA_IVDEP:
      WN2C_Append_Pragma_Newline(tokens, CONTEXT_srcpos(context));
      Append_Token_String(tokens, "ivdep");
      break;

   case WN_PRAGMA_DOACROSS:
      /* Ignore deeper nests.
       */
      if (WN_pragma_nest(apragma) <= 0 && 
	  !Ignore_Synchronized_Construct(apragma, context))
      {
	 surrounding_region = W2CF_Get_Parent(W2CF_Get_Parent(apragma));
	 first_clause = WN_next(apragma);

         WN2C_Append_Pragma_Newline(tokens, CONTEXT_srcpos(context));
         Append_Token_String(tokens, "parallel");
	 WN2C_Stmt_Newline(tokens, CONTEXT_srcpos(context));
	 Append_Token_Special(tokens, '{');
         WN2C_Append_Pragma_Newline(tokens, CONTEXT_srcpos(context));
         Append_Token_String(tokens, "pfor");
	 if (WN_max_nest_level(apragma) > 1)
	    Append_Nest_Clauses(tokens, 
				surrounding_region, 
				WN_max_nest_level(apragma),
				context);
	 apragma = first_clause;
         Append_Pragma_Clauses(tokens, &apragma, context);
	 Append_Implicit_Locals(tokens, 
				WN_PRAGMA_DOACROSS, 
				WN_region_body(surrounding_region),
				first_clause,
				context);
	 Increment_Indentation();
      }
      else
      {
	 apragma = WN_next(apragma);
         Skip_Pragma_Clauses(&apragma, context);
      }
      break;

   case WN_PRAGMA_MPSCHEDTYPE:
      /* Can be both a clause and a pragma.
       */
      WN2C_Append_Pragma_Newline(tokens, CONTEXT_srcpos(context));
      if (CONTEXT_omp(context))
      {
	 Append_Clause_Newline(tokens, context);
	 Append_Token_String(tokens, "schedule");
	 Append_Token_Special(tokens, '(');
	 Append_MP_Schedtype(tokens, WN_mp_schedtype(apragma));
	 if (WN_next(apragma) != NULL &&
	     WN_pragma(WN_next(apragma)) == WN_PRAGMA_CHUNKSIZE)
	 {
	    apragma = WN_next(apragma);
	    Append_Token_Special(tokens, ',');
	    WN2C_Append_Value_Reference(tokens, WN_kid0(apragma));
	 }
	 Append_Token_Special(tokens, ')');
	 apragma = WN_next(apragma);
      }
      else
      {
	 Append_Token_String(tokens, "mp_schedtype");
	 Append_Token_Special(tokens, '(');
	 Append_MP_Schedtype(tokens, WN_mp_schedtype(apragma));
	 Append_Token_Special(tokens, ')');
      }
      break;

   case WN_PRAGMA_BARRIER:
      WN2C_Append_Pragma_Newline(tokens, CONTEXT_srcpos(context));
      if (CONTEXT_omp(context))
	 Append_Token_String(tokens, "omp barrier");
      else
	 Append_Token_String(tokens, "synchronize");
      break;

   case WN_PRAGMA_COPYIN:
      WN2C_Append_Pragma_Newline(tokens, CONTEXT_srcpos(context));
      if (CONTEXT_omp(context))
	 Append_Token_String(tokens, "omp copyin");
      else
	 Append_Token_String(tokens, "copyin");
      if (WN_operator(apragma) == OPR_XPRAGMA)
         Append_Clause_Expressions(tokens, 
				   (WN_PRAGMA_ID)WN_pragma(apragma),
                                   &apragma);
      else
      {
	 ST2C_use_translate(tokens, WN_st(apragma), context);
      }
      break;

   case WN_PRAGMA_CRITICAL_SECTION_BEGIN:
      WN2C_Append_Pragma_Newline(tokens, CONTEXT_srcpos(context));
      if (CONTEXT_omp(context))
	 Append_Token_String(tokens, "omp critical");
      else
	 Append_Token_String(tokens, "critical");
      if (WN_operator(apragma) == OPR_XPRAGMA)
	 Append_Clause_Expressions(tokens,
				   (WN_PRAGMA_ID)WN_pragma(apragma),
                                   &apragma);
      WN2C_Stmt_Newline(tokens, CONTEXT_srcpos(context));
      Append_Token_Special(tokens, '{');
      Increment_Indentation();
      break;

   case WN_PRAGMA_CRITICAL_SECTION_END:
      Decrement_Indentation();
      WN2C_Stmt_Newline(tokens, CONTEXT_srcpos(context));
      Append_Token_Special(tokens, '}');
      break;

   case WN_PRAGMA_ORDERED_BEGIN:
      WN2C_Append_Pragma_Newline(tokens, CONTEXT_srcpos(context));
      Append_Token_String(tokens, "omp ordered");
      if (WN_operator(apragma) == OPR_XPRAGMA)
	 Append_Clause_Expressions(tokens,
				   (WN_PRAGMA_ID)WN_pragma(apragma),
                                   &apragma);
      WN2C_Stmt_Newline(tokens, CONTEXT_srcpos(context));
      Append_Token_Special(tokens, '{');
      Increment_Indentation();
      break;

   case WN_PRAGMA_ORDERED_END:
      Decrement_Indentation();
      WN2C_Stmt_Newline(tokens, CONTEXT_srcpos(context));
      Append_Token_Special(tokens, '}');
      break;

   case WN_PRAGMA_ATOMIC:
      WN2C_Append_Pragma_Newline(tokens, CONTEXT_srcpos(context));
      Append_Token_String(tokens, "omp atomic");
      if (WN_operator(apragma) == OPR_XPRAGMA)
         Append_Clause_Expressions(tokens,
				   (WN_PRAGMA_ID)WN_pragma(apragma),
                                   &apragma);
      break;

   case WN_PRAGMA_PARALLEL_BEGIN:
      if (!Ignore_Synchronized_Construct(apragma, context))
      {
	 surrounding_region = W2CF_Get_Parent(W2CF_Get_Parent(apragma));
	 first_clause = WN_next(apragma);

	 WN2C_Append_Pragma_Newline(tokens, CONTEXT_srcpos(context));
	 if (CONTEXT_omp(context))
	    Append_Token_String(tokens, "omp parallel");
	 else
	    Append_Token_String(tokens, "parallel");
	 apragma = first_clause;
	 Append_Pragma_Clauses(tokens, &apragma, context);
	 Append_Implicit_Locals(tokens, 
				WN_PRAGMA_PARALLEL_BEGIN, 
				WN_region_body(surrounding_region),
				first_clause,
				context);
	 WN2C_Stmt_Newline(tokens, CONTEXT_srcpos(context));
	 Append_Token_Special(tokens, '{');
	 Increment_Indentation();
      }
      break;

   case WN_PRAGMA_PARALLEL_DO:
      /* Ignore deeper nests.
       */
      if (WN_pragma_nest(apragma) <= 0 &&
	  !Ignore_Synchronized_Construct(apragma, context))
      {
	 surrounding_region = W2CF_Get_Parent(W2CF_Get_Parent(apragma));
	 first_clause = WN_next(apragma);

         WN2C_Append_Pragma_Newline(tokens, CONTEXT_srcpos(context));
	 if (CONTEXT_omp(context))
	 {
	    Append_Token_String(tokens, "omp parallel for");
	 }
	 else
	 {
	    Append_Token_String(tokens, "parallel");
	    WN2C_Stmt_Newline(tokens, CONTEXT_srcpos(context));
	    Append_Token_Special(tokens, '{');
	    WN2C_Append_Pragma_Newline(tokens, CONTEXT_srcpos(context));
	    Append_Token_String(tokens, "pfor");
	 }
	 if (WN_max_nest_level(apragma) > 1)
	    Append_Nest_Clauses(tokens, 
				surrounding_region, 
				WN_max_nest_level(apragma),
				context);
	 apragma = first_clause;
         Append_Pragma_Clauses(tokens, &apragma, context);
	 Append_Implicit_Locals(tokens, 
				WN_PRAGMA_PARALLEL_DO, 
				WN_region_body(surrounding_region),
				first_clause,
				context);
	  
	 if (CONTEXT_omp(context))
	 {
	    WN2C_Stmt_Newline(tokens, CONTEXT_srcpos(context));
	    Append_Token_Special(tokens, '{');
	 }
	 Increment_Indentation();
      }
      else
      {
	 apragma = WN_next(apragma);
         Skip_Pragma_Clauses(&apragma, context);
      }
      break;

   case WN_PRAGMA_PDO_BEGIN:
      /* Ignore deeper nests.
       */
      if (WN_pragma_nest(apragma) == 0 &&
	  !Ignore_Synchronized_Construct(apragma, context))
      {
	 surrounding_region = W2CF_Get_Parent(W2CF_Get_Parent(apragma));
	 first_clause = WN_next(apragma);

         WN2C_Append_Pragma_Newline(tokens, CONTEXT_srcpos(context));
	 if (CONTEXT_omp(context)) 
	    Append_Token_String(tokens, "omp for");
	 else
	    Append_Token_String(tokens, "pfor");
	 if (WN_max_nest_level(apragma) > 1)
	    Append_Nest_Clauses(tokens, 
				surrounding_region, 
				WN_max_nest_level(apragma),
				context);
	 apragma = first_clause;
         Append_Pragma_Clauses(tokens, &apragma, context);

	 /* Turn this off for now, since we also need to avoid declaring
	  * as local variables declared as shared in the enclosing
	  * parallel region.
	  *
	  * Append_Implicit_Locals(tokens, 
	  *             WN_PRAGMA_PARALLEL_DO 
	  *             WN_region_body(surrounding_region),
	  *		first_clause,
	  *		context);
	  */
      }
      else
      {
	 apragma = WN_next(apragma);
         Skip_Pragma_Clauses(&apragma, context);
      }
      break;

   case WN_PRAGMA_PARALLEL_SECTIONS:
   case WN_PRAGMA_PSECTION_BEGIN:
      WN2C_Append_Pragma_Newline(tokens, CONTEXT_srcpos(context));
      if (CONTEXT_omp(context)) 
	 Append_Token_String(tokens, "omp parallel sections");
      else
	 Append_Token_String(tokens, "psections");
      apragma = WN_next(apragma);
      Append_Pragma_Clauses(tokens, &apragma, context);
      WN2C_Stmt_Newline(tokens, CONTEXT_srcpos(context));
      Append_Token_Special(tokens, '{');
      Increment_Indentation();
      break;

   case WN_PRAGMA_ENTER_GATE:
      WN2C_Append_Pragma_Newline(tokens, CONTEXT_srcpos(context));
      Append_Token_String(tokens, "enter gate");
      break;

   case WN_PRAGMA_EXIT_GATE:
      WN2C_Append_Pragma_Newline(tokens, CONTEXT_srcpos(context));
      Append_Token_String(tokens, "exit gate");
      break;
      
   case WN_PRAGMA_INDEPENDENT_BEGIN:
      WN2C_Append_Pragma_Newline(tokens, CONTEXT_srcpos(context));
      Append_Token_String(tokens, "independent");
      apragma = WN_next(apragma);
      Append_Pragma_Clauses(tokens, &apragma, context);
      WN2C_Stmt_Newline(tokens, CONTEXT_srcpos(context));
      Append_Token_Special(tokens, '{');
      Increment_Indentation();
      break;

   case WN_PRAGMA_INDEPENDENT_END:
      Decrement_Indentation();
      WN2C_Stmt_Newline(tokens, CONTEXT_srcpos(context));
      Append_Token_Special(tokens, '}');
      break;

   case WN_PRAGMA_SECTION:
      WN2C_Append_Pragma_Newline(tokens, CONTEXT_srcpos(context));
      if (CONTEXT_omp(context)) 
	 Append_Token_String(tokens, "omp section");
      else
	 Append_Token_String(tokens, "section");
      break;

   case WN_PRAGMA_SINGLE_PROCESS_BEGIN:
      if (!Ignore_Synchronized_Construct(apragma, context))
      {
	 WN2C_Append_Pragma_Newline(tokens, CONTEXT_srcpos(context));
	 if (CONTEXT_omp(context))
	    Append_Token_String(tokens, "omp single");
	 else
	    Append_Token_String(tokens, "one processor");
	 apragma = WN_next(apragma);
	 Append_Pragma_Clauses(tokens, &apragma, context);
	 WN2C_Stmt_Newline(tokens, CONTEXT_srcpos(context));
	 Append_Token_Special(tokens, '{');
	 Increment_Indentation();
      }
      break;

    case WN_PRAGMA_MASTER_BEGIN:
      if (!Ignore_Synchronized_Construct(apragma, context))
      {
	 WN2C_Append_Pragma_Newline(tokens, CONTEXT_srcpos(context));
	 if (CONTEXT_omp(context))
	   Append_Token_String(tokens, "omp master");
	 else
	   Append_Token_String(tokens, "master process");
      }
      break;

   case WN_PRAGMA_NUMTHREADS:
      /* Should only appear for C, but if we ever see it, we also emit
       * it for Fortran.
       */
      WN2C_Append_Pragma_Newline(tokens, CONTEXT_srcpos(context));
      Append_Token_String(tokens, "numthreads");
      Append_Clause_Expressions(tokens,
				(WN_PRAGMA_ID)WN_pragma(apragma),
                                &apragma);
      break;

   case WN_PRAGMA_PAGE_PLACE:
      WN2C_Append_Pragma_Newline(tokens, CONTEXT_srcpos(context));
      Append_Token_String(tokens, "page_place");
      Append_Clause_Expressions(tokens,
				(WN_PRAGMA_ID)WN_pragma(apragma),
                                &apragma);
      break;

   case WN_PRAGMA_NORECURRENCE:
      WN2C_Append_Pragma_Newline(tokens, CONTEXT_srcpos(context));
      Append_Token_String(tokens, "no recurrence");
      break;

   case WN_PRAGMA_NEXT_SCALAR:
      WN2C_Append_Pragma_Newline(tokens, CONTEXT_srcpos(context));
      Append_Token_String(tokens, "next scalar");
      break;
      
   default:
      /* The others are always clauses that are processed as part of other
       * pragmas, or they are not to be emitted.
       */
      break;

   } /* switch on pragma cases */

   /* See if we have already advanced to the next pragma, e.g. as a result
    * of calling Append_Pragma_Clauses() or Append_Clause_Symbols(),
    * and if not so, then advance to the next pragma.
    */
   if (apragma == *next)
      *next = WN_next(apragma);
   else
      *next = apragma;

} /* WN2C_process_pragma */


/* ====================== Exported Functions ====================== */
/* ================================================================ */


BOOL
WN2C_Skip_Pragma_Stmt(const WN *wn)
{
   /* This assumes that any pragma related nodes to be skipped will be
    * accessed in sequence, and that this routine will be called at most
    * once per such node.
    */
   BOOL found = (Pragmas_To_Skip.array[Pragmas_To_Skip.start] == wn);

   if (found)
   {
      if (Pragmas_To_Skip.end - Pragmas_To_Skip.start == 1)
      {
	 Pragmas_To_Skip.start = Pragmas_To_Skip.end = 0;
	 Pragmas_To_Skip.array[0] = NULL;
      }
      else
      {
	 Pragmas_To_Skip.start++;
      }
   }
   return found;
} /* WN2C_Skip_Pragma_Stmt */


STATUS
WN2C_pragma(TOKEN_BUFFER tokens, const WN *wn, CONTEXT context)
{
   const WN *skip;
   const WN *next = wn;

   Is_True(WN_operator(wn) == OPR_PRAGMA || WN_operator(wn) == OPR_XPRAGMA,
	   ("Invalid operator for WN2C_pragma()"));

   WN2C_process_pragma(tokens, &next, context);

   Is_True(Pragmas_To_Skip.end == 0,
	   ("Unexpected index for Pragmas_To_Skip in WN2C_pragma()"));

   /* For pragmas inlined in code, we need to keep track of the pragmas
    * that have already been processed.
    */
   for (skip = WN_next(wn); skip != next; skip = WN_next(skip))
   {
      Is_True(Pragmas_To_Skip.end < MAX_PRAGMAS_TO_SKIP,
	      ("Too many pragmas in sequence in WN2C_pragma()"));

      Pragmas_To_Skip.array[Pragmas_To_Skip.end++] = skip;
   }

   return EMPTY_STATUS;
} /* WN2C_pragma */


STATUS 
WN2C_pragma_list_begin(TOKEN_BUFFER tokens,
                       const WN    *first_pragma,
                       CONTEXT      context)
{
   const WN *next_pragma = first_pragma;

   while (next_pragma != NULL)
   {
      if (WN_operator(next_pragma) == OPR_PRAGMA ||
	  WN_operator(next_pragma) == OPR_XPRAGMA)
	 WN2C_process_pragma(tokens, &next_pragma, context);
      else
	 next_pragma = WN_next(next_pragma);
   }
   return EMPTY_STATUS;
} /* WN2C_pragma_list_begin */


STATUS 
WN2C_pragma_list_end(TOKEN_BUFFER tokens, 
                     const WN    *first_pragma,
                     CONTEXT context)
{
   /* Skip code inserted into the pragma region (may occur for C++).
    */
   while (first_pragma != NULL                  &&
	  WN_operator(first_pragma) != OPR_PRAGMA &&
	  WN_operator(first_pragma) != OPR_XPRAGMA)
   {
      first_pragma = WN_next(first_pragma);
   }

   if (first_pragma != NULL)
   {
      Is_True(WN_operator(first_pragma) == OPR_PRAGMA ||
	      WN_operator(first_pragma) == OPR_XPRAGMA, 
	      ("Unexpected operator (%d) in WN2C_pragma_list_end()",
	       WN_operator(first_pragma)));
      
      switch (WN_pragma(first_pragma))
      {

      case WN_PRAGMA_PARALLEL_BEGIN:
	 if (!Ignore_Synchronized_Construct(first_pragma, context))
	 {
	    Decrement_Indentation();
	    WN2C_Stmt_Newline(tokens, CONTEXT_srcpos(context));
	    Append_Token_Special(tokens, '}');
	 }
	 break;

      case WN_PRAGMA_DOACROSS:
      case WN_PRAGMA_PARALLEL_DO:
	 if (WN_pragma_nest(first_pragma) <= 0 &&
	     !Ignore_Synchronized_Construct(first_pragma, context))
	 {
	    Decrement_Indentation();
	    WN2C_Stmt_Newline(tokens, CONTEXT_srcpos(context));
	    Append_Token_Special(tokens, '}');
	 }
	 break;

      case WN_PRAGMA_PARALLEL_SECTIONS:
      case WN_PRAGMA_PSECTION_BEGIN:
	 Decrement_Indentation();
	 WN2C_Stmt_Newline(tokens, CONTEXT_srcpos(context));
	 Append_Token_Special(tokens, '}');
	 break;

      case WN_PRAGMA_SINGLE_PROCESS_BEGIN:
	 if (!Ignore_Synchronized_Construct(first_pragma, context))
	 {
	    Decrement_Indentation();
	    WN2C_Stmt_Newline(tokens, CONTEXT_srcpos(context));
	    Append_Token_Special(tokens, '}');
      }
      break;

      default:
         break; /* Not a region that needs an END pragma (in C, a '}'). */
      }
   }
   return EMPTY_STATUS;
} /* WN2C_pragma_list_end */


BOOL
Ignore_Synchronized_Construct(const WN *construct_pragma,  
			      CONTEXT   context)
{
   /* This can be TRUE for DOACROSS, PARALLEL, and any paralellization
    * related construct that may occur within a parallel region.
    * It only applies for mplist (i.e. when Run_w2fc_early).
    */
   BOOL ignore_construct;

   Is_True(WN_operator(construct_pragma) == OPR_PRAGMA,
	   ("Unexpected WHIRL tree in Ignore_Synchronized_Construct"));

   if (!Run_w2fc_early)
      ignore_construct = FALSE;
   else
   {
      if (WN_pragma(construct_pragma) != WN_PRAGMA_DOACROSS)
	 construct_pragma = Get_Enclosing_Parallel_Region(construct_pragma);

      if (construct_pragma == NULL)
	 ignore_construct = FALSE;
      else
      {
	 const WN *clause = WN_next(construct_pragma);
	 const WN *beyond_last_clause = clause;

	 Skip_Pragma_Clauses(&beyond_last_clause, context);
	 while (clause != beyond_last_clause && 
		WN_pragma(clause) != WN_PRAGMA_SYNC_DOACROSS)
	    clause = WN_next(clause);
	 ignore_construct = (clause != beyond_last_clause);
      }
   }
   return ignore_construct;
} /* Ignore_Synchronized_Construct */

