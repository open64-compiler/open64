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
 * Module: wn2f_pragma.c
 * $Revision: 1.1 $
 * $Date: 2005/07/27 02:13:42 $
 * $Author: kevinlo $
 * $Source: /depot/CVSROOT/javi/src/sw/cmplr/be/whirl2f/wn2f_pragma.cxx,v $
 *
 * Revision history:
 *  12-Aug-96 - Original Version
 *
 * Description:
 *
 *   Translate a pragma WN node to Fortran!  The corresponding header
 *   declaration for for WN2F_pragma() can be found in wn2f_pragma.h.
 *
 * ====================================================================
 * ====================================================================
 */

#ifdef _KEEP_RCS_ID
/*REFERENCED*/
static char *rcs_id = "$Source: /depot/CVSROOT/javi/src/sw/cmplr/be/whirl2f/wn2f_pragma.cxx,v $ $Revision: 1.1 $";
#endif

#include "alloca.h"
#include "whirl2f_common.h"
#include "w2cf_parentize.h"  /* For W2CF_Get_Parent */
#include "const.h"           /* For FOR_ALL_CONSTANTS */
#include "pf_cg.h"
#include "region_util.h"     /* For RID and RID_map */
#include "PUinfo.h"          /* In be/whirl2c directory */
#include "wn2f.h"
#include "st2f.h"
#include "ty2f.h"
#include "tcon2f.h"
#include "wn2f_pragma.h"

extern BOOL    Run_w2fc_early;      /* Defined in be.so */
extern BOOL    W2F_Emit_Omp;        /* Emitting OMP spellings of pragmas */


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
   INT current_dimension;  /* Enumerated dimension number in C order */
   WN *base;               /* PRAGMA starting description of this dimension */
   WN *cyclic_expr;        /* XPRAGMA holding a cyclic expression (or NULL) */
   WN *dimension_bound;    /* XPRAGMA holding the bounds expression */
} ARRAY_DISTRIBUTION;


#define MAX_PRAGMAS_TO_SKIP 50
static struct Set_Of_Pragmas_To_Skip
{
   INT start, end;
   WN *array[MAX_PRAGMAS_TO_SKIP];
} Pragmas_To_Skip = {0, 0, 
		     {NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,
		      NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,
		      NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,
		      NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,
		      NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL}};


typedef struct Local_Preg /* Used in Get_Implicit_Locals() */
{
   ST *     st;
   PREG_IDX preg_idx;
} LOCAL_PREG;


/* ======================= Omp utilities ======================= */
/* ================================================================ */

inline BOOL
WN2F_is_omp(const WN *pragma)
{
   return (WN_pragma_omp(pragma) ||
	   (W2F_Emit_Omp && WN_pragma_compiler_generated(pragma)));
}

/* ======================= Static Functions ======================= */
/* ================================================================ */


static void 
WN2F_Directive_Newline(TOKEN_BUFFER tokens,
		       const char  *directive_prefix,
		       SRCPOS       srcpos)
{
   Append_F77_Directive_Newline(tokens, directive_prefix);
   if (W2F_File[W2F_LOC_FILE] != NULL)
      Append_Srcpos_Map(tokens, srcpos);
} /* WN2F_Directive_Newline */


static void 
WN2F_Append_Pragma_Preamble(TOKEN_BUFFER tokens,WN * apragma)
{
  /* 
   * appends a PAR or OMP depending on the setting of
   * the omp flag on the pragma node
   *
   */
 
  if (WN2F_is_omp(apragma))
    Append_Token_String(tokens, "OMP");
  else
    Append_Token_String(tokens, "PAR");
}

static void 
Append_Reduction_Operator(TOKEN_BUFFER tokens,OPERATOR op)
{
  /* 
   * appends a symbol representing an OMP reduction operator.
   */

  const char * p;

  switch(op) 
    {
    case OPR_MAX:
      p = "MAX";
      break;

    case OPR_MIN:
      p = "MIN";
      break;

    case OPR_BAND:
      p = "IAND";
      break;

    case OPR_BIOR:
      p = "IOR";
      break;

    case OPR_BXOR:
      p = "IEOR";
      break;

    case OPR_LAND:
      p = ".AND.";
      break;

    case OPR_LIOR:
      p = ".OR.";
      break;

    case OPR_EQ:
      p = ".EQV.";
      break;

    case OPR_NE:
      p = ".NEQV.";
      break;

    case OPR_ADD:
      p = "+";
      break;

    case OPR_MPY:
       p = "*";
      break;

    case OPR_SUB:
      p = "-";
      break;

    default:
      p = "?" ;
  }   
  Append_Token_String(tokens, p);
  Append_Token_Special(tokens,':');
}

static BOOL
Is_Valid_Doacross(WN *doacross)
{
   /* Return TRUE if the enclosing region has a body containing nothing
    * other than an OPR_DO_LOOP node.  Only when returning TRUE may a 
    * DOACROSS directive be emipwdtted prior to the region body.
    */
   const WN *region = W2CF_Get_Parent(W2CF_Get_Parent(doacross));
   WN *region_body;

   ASSERT_DBG_FATAL(WN_operator(region) == OPR_REGION, 
		    (DIAG_W2F_UNEXPECTED_OPC, "Is_Valid_Doacross"));

   region_body = WN_region_body(region);
   return (WN_operator(WN_first(region_body)) == OPR_DO_LOOP &&
	   WN_first(region_body) == WN_last(region_body));
} /* Is_Valid_Doacross */


static void
Put_Pragma_Start_With_Caveats(TOKEN_BUFFER tokens, WN *apragma, BOOL warn)
{
  /* It may be transformations have stuffed some code before
   * a doacross, pdo or parallel do. This is not strictly allowed
   * when compiling (ie: w2f output), so warn about the 
   * misplaced items, if warn is set. eg: if ENDDO etc comes through,
   * omit the warning.
   */

  if (Is_Valid_Doacross(apragma))
    WN2F_Directive_Newline(tokens, "C$", WN_Get_Linenum(apragma));
  else
    {
      WN2F_Directive_Newline(tokens,"C<misplaced>$", WN_Get_Linenum(apragma));

      if ( warn ) 
	{
	  if (WN_pragma(apragma) == WN_PRAGMA_DOACROSS)
	    ASSERT_WARN(FALSE, (DIAG_W2F_MISPLACED_PRAGMA, "DOACROSS"));
	  else if (WN_pragma(apragma) == WN_PRAGMA_PDO_BEGIN)
	    ASSERT_WARN(FALSE, (DIAG_W2F_MISPLACED_PRAGMA, "PDO"));
	  else
	    ASSERT_WARN(FALSE, (DIAG_W2F_MISPLACED_PRAGMA, "PARALLEL DO"));
	}
    }
}

static BOOL
Preg_Is_In_Clause_List(const WN *clause_list, ST *preg_st, PREG_IDX preg_idx)
{
   /* Returns TRUE when the given preg is already referenced in a LOCAL,
    * LASTLOCAL, SHARED or REDUCTION clause in the given clause list; 
    * otherwise we return FALSE.
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
		    LOCAL_PREG  **ptr_to_local_list, /* in/out */
		    UINT         *next_local,        /* in/out */
		    UINT         *max_locals)        /* in/out */
{
   /* Puts the implicit locals into the local_list.  Every element of the
    * local list must represent a unique preg.
    */
   OPERATOR opr = WN_operator(wn);
   ST      *st;
   PREG_IDX preg_idx;

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
      INT         i;
      BOOL        found = FALSE;
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

         /* Enter new element into the local_list.
          */
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
	 const WN* kid = WN_first(wn);
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
	 const WN* kid;
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
		       const WN    *region_clauses)
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
      if (region_clauses != NULL)
	 Append_Token_Special(tokens, ',');

      Append_Token_String(tokens, "local");
      Append_Token_Special(tokens, '(');
      for (i = 0; i < number_of_locals; i++)
      {
	 if (i > 0)
	    Append_Token_Special(tokens, ',');

	 ST2F_Use_Preg(tokens,
		       ST_type(local_list[i].st), local_list[i].preg_idx);
      }
      Append_Token_Special(tokens, ')');
   }

   if (local_list != NULL)
      FREE(local_list);
} /* Append_Implicit_Locals */


static BOOL
WN2F_pragma_list_nowait(WN *first_pragma)
{
   WN  *wn;
   BOOL nowait = FALSE;

   for (wn = first_pragma; !nowait && wn != NULL; wn = WN_next(wn))
      if ((WN_operator(wn) == OPR_PRAGMA || WN_operator(wn) == OPR_XPRAGMA) &&
	  WN_pragma(wn) == WN_PRAGMA_NOWAIT)
         nowait = TRUE;

   return nowait;
} /* WN2F_pragma_list_nowait */


static void
WN2F_Append_Value_Reference(TOKEN_BUFFER tokens, WN *expression)
{
   WN2F_CONTEXT context = INIT_WN2F_CONTEXT;

   /* Emit memory reference
    */
   if (TY_Is_Pointer(WN_Tree_Type(expression)))
      set_WN2F_CONTEXT_deref_addr(context);
   (void)WN2F_translate(tokens, expression, context);
} // WN2F_Append_Value_Reference


static void
WN2F_Prepend_Value_Reference(TOKEN_BUFFER tokens, WN *expression)
{
   WN2F_CONTEXT context = INIT_WN2F_CONTEXT;
   TOKEN_BUFFER expr_tokens = New_Token_Buffer();

   /* Emit memory reference
    */
   if (TY_Is_Pointer(WN_Tree_Type(expression)))
      set_WN2F_CONTEXT_deref_addr(context);
   (void)WN2F_translate(expr_tokens, expression, context);
   Prepend_And_Reclaim_Token_List(tokens, &expr_tokens);
} // WN2F_Prepend_Value_Reference


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
      Append_Token_String(tokens, "interleaved");
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
      ASSERT_DBG_FATAL(FALSE, 
		       (DIAG_W2F_UNEXPECTED_OPC, "Append_MP_Schedtype"));
      break;
   }
} /* Append_MP_Schedtype */


static void
Append_Arg_Numbers(TOKEN_BUFFER tokens,
                   INT32        val1,
                   INT32        val2)
{
   if (val1 != -1)
      Append_Token_String(tokens, WHIRL2F_number_as_name(val1));

   if (val2 != -1)
   {
      Append_Token_Special(tokens, ',');
      Append_Token_String(tokens, WHIRL2F_number_as_name(val2));
   }
} /* Append_Arg_Numbers */

         
static void
Append_Prefetch_Attributes(TOKEN_BUFFER tokens, 
                           WN          *prefetch,
                           INT32        size)
{
   INT pflag = WN_prefetch_flag(prefetch);

   /* Emit memory reference
    */
   Append_Token_Special(tokens, '=');
   WN2F_Append_Value_Reference(tokens, WN_kid0(prefetch));

   /* Emit stride and level clauses
    */
   Append_Token_Special(tokens, ',');
   if (PF_GET_STRIDE_1L(pflag) > 0)
   {
      if (PF_GET_STRIDE_2L(pflag) > 0)
      {
         Append_Token_String(tokens, 
            Concat2_Strings("stride=",
            Concat2_Strings(WHIRL2F_number_as_name(PF_GET_STRIDE_1L(pflag)),
            Concat2_Strings(",",
                        WHIRL2F_number_as_name(PF_GET_STRIDE_2L(pflag))))));
         Append_Token_Special(tokens, ',');
         Append_Token_String(tokens, "level=1,2");
      }
      else
      {
         Append_Token_String(tokens, 
            Concat2_Strings("stride=",
                            WHIRL2F_number_as_name(PF_GET_STRIDE_1L(pflag))));
         Append_Token_Special(tokens, ',');
         Append_Token_String(tokens, "level=1");
      }
   }
   else if (PF_GET_STRIDE_2L(pflag) > 0)
   {
      Append_Token_String(tokens, 
            Concat2_Strings("stride=,",
                            WHIRL2F_number_as_name(PF_GET_STRIDE_2L(pflag))));
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
         Concat2_Strings("size=", WHIRL2F_number_as_name(size)));
   }
} /* Append_Prefetch_Attributes */


static void
Append_Distribution(TOKEN_BUFFER tokens, WN **apragma, WN_PRAGMA_ID id)
{
   INT32               dim, num_dims;
   ARRAY_DISTRIBUTION  distr[MAX_PRAGMAS_TO_SKIP];
   WN                 *wn = *apragma;
   WN2F_CONTEXT        context = INIT_WN2F_CONTEXT;

   ASSERT_DBG_FATAL(WN_operator(wn) == OPR_PRAGMA, 
		    (DIAG_W2F_UNEXPECTED_OPC, "Append_Distribution"));

   /* Accumulate the distribution kind for each dimension.
    */
   for (num_dims = 0; 
	(WN_operator(wn) == OPR_PRAGMA &&
	 WN_pragma(wn) == id               &&
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
   if (WN_operator(wn)==OPR_STID && ST_class(WN_st(wn))==CLASS_PREG)
   {
      wn = WN_next(wn);
      if (WN_operator(wn)==OPR_STID && ST_class(WN_st(wn))==CLASS_PREG)
         wn = WN_next(wn);
   }
   *apragma = wn;

   /* Translate the sequence of distribution kinds, in Fortran order, i.e.
    * in reverse order from WHIRL representation.
    */
   Append_Token_Special(tokens, '(');
   for (dim = num_dims-1; dim >= 0; dim--)
   {
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
         WN2F_translate(tokens, WN_kid0(distr[dim].cyclic_expr), context);
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

      if (dim > 0)
         Append_Token_Special(tokens, ',');

   } /* For each dimension */
   Append_Token_Special(tokens, ')');

} /* Append_Distribution */


static void
Append_A_Clause_Symbol(TOKEN_BUFFER tokens, WN *clause, WN_OFFSET ofst)
{
   ST * const st = WN_st(clause);

   if (ST_class(st) == CLASS_PREG)
   {
      INT32 preg_num = WN_pragma_arg1(clause);

      ST2F_Use_Preg(tokens, ST_type(st), preg_num);
   }
   else
   {
      const TY_IDX base_ty = ST_type(st);
      WN2F_CONTEXT context = INIT_WN2F_CONTEXT;

      WN2F_Offset_Symref(tokens,
                         st,                       /* base variable */
                         Stab_Pointer_To(base_ty), /* base addr */
                         base_ty,                  /* type of reference */
                         ofst,                     /* base offset */
                         context);
   }
} /* Append_A_Clause_Symbol */


static void
Append_Clause_Symbols(TOKEN_BUFFER tokens,
		      WN_PRAGMA_ID id,
		      WN         **next)
{
   /* Loop through the pragmas, and emit a ',' separated list
    * of the ST attributes for all contiguous pragmas with the 
    * given "id".  Terminate upon reaching a pragma with a
    * different "id" or the end of the pragma list. Set *next to
    * point to the next node after the last one processed here.
    */
   WN  *clause;

   ASSERT_DBG_FATAL(WN_operator(*next) == OPR_PRAGMA, 
		    (DIAG_W2F_UNEXPECTED_OPC, "Append_Clause_Symbols"));

   Append_Token_Special(tokens, '(');
   for (clause = *next;
	(clause != NULL && 
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
			WN         **next)
{
   /* Loop through the pragmas, and emit a ',' separated list
    * of the reduction operator and ST attributes for all 
    * contiguous pragmas with the given "id".  
    */
   WN  *       clause;
   WN  * const first_clause = *next;

   ASSERT_DBG_FATAL(WN_operator(first_clause) == OPR_PRAGMA, 
		    (DIAG_W2F_UNEXPECTED_OPC, "Append_Reduction_Clause"));

   Append_Token_String(tokens, "reduction (");
   for (clause = first_clause;
	(clause != NULL && 
	 WN_operator(clause) == OPR_PRAGMA && 
	 WN_pragma(clause) == id);
	clause = WN_next(clause))
   {
      if (WN2F_is_omp(clause) &&  
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
			  WN         **next,
			  BOOL         reverse_order = FALSE)
{
   /* Loop through the pragmas, and emit a ',' separated list
    * of the ST attributes for all contiguous pragmas with the 
    * given "id".  Terminate upon reaching a pragma with a
    * different "id" or the end of the pragma list. Set *next to
    * point to the next node after the last one processed here.
    */
   TOKEN_BUFFER clause_tokens = New_Token_Buffer();
   WN *         clause;
   WN * const   first_clause = *next;

   ASSERT_DBG_FATAL(WN_operator(first_clause) == OPR_XPRAGMA, 
		    (DIAG_W2F_UNEXPECTED_OPC, "Append_Clause_Expressions"));

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
	    WN2F_Prepend_Value_Reference(tokens, WN_kid0(clause));
	 else
	    WN2F_Append_Value_Reference(tokens, WN_kid0(clause));
      }
   } // for each clause
   Append_And_Reclaim_Token_List(tokens, &clause_tokens);
   Append_Token_Special(tokens, ')');
   *next = clause;
} /* Append_Clause_Expressions */

	
static void
Append_Array_Segment(TOKEN_BUFFER tokens,
		     WN_PRAGMA_ID id,
		     WN         **next)
{
   /* Loop through the pragmas, and emit a ',' separated list
    * of the ST attributes for all contiguous pragmas with the 
    * given "id".  Terminate upon reaching a pragma with a
    * different "id" or the end of the pragma list. Set *next to
    * point to the next node after the last one processed here.
    */
   WN *clause;

   ASSERT_DBG_FATAL(WN_operator(*next) == OPR_XPRAGMA,
		    (DIAG_W2F_UNEXPECTED_OPC, "Append_Array_Segment"));

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
      EMIT_ARG_NUMBERS1(tokens, 1);
      Append_Token_Special(tokens, ':');
      WN2F_Append_Value_Reference(tokens, WN_kid0(clause));
      Append_Token_Special(tokens, ')');
   }
   Append_Token_Special(tokens, ')');

   *next = clause;
} /* Append_Array_Segment */

	    
static void
Append_Nest_Clauses(TOKEN_BUFFER tokens, 
		    const WN    *nest_region, 
		    INT          nest_levels,
		    WN2F_CONTEXT context)
{
   BOOL         pattern_error = FALSE;
   INT          nest;
   ST          *idx_var;
   TY_IDX       idx_ty;
   const WN    *next_stmt = nest_region;
   WN_PRAGMA_ID nest_kind = WN_PRAGMA_UNDEFINED;
   TOKEN_BUFFER nest_tokens = New_Token_Buffer();

   ASSERT_DBG_FATAL(next_stmt != NULL &&
		    WN_operator(next_stmt) == OPR_REGION &&
		    WN_first(WN_region_pragmas(next_stmt)) != NULL,
		    (DIAG_W2F_UNEXPECTED_OPC, "Append_Nest_Clauses"));

   nest_kind = 
      (WN_PRAGMA_ID)WN_pragma(WN_first(WN_region_pragmas(next_stmt)));

   Append_Token_String(nest_tokens, "nest");
   Append_Token_Special(nest_tokens, '(');
   reset_WN2F_CONTEXT(context);
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
	 if (ST_class(idx_var) == CLASS_PREG)
	 {
	    ST2F_Use_Preg(nest_tokens, 
			  idx_ty, 
			  WN_idname_offset(WN_index(next_stmt)));
	 }
	 else
	 {
	    WN2F_Offset_Symref(nest_tokens,
			       idx_var,                 /* base variable */
			       Stab_Pointer_To(idx_ty), /* base addr */
			       idx_ty,                  /* type of ref */
			       0,                       /* base offset */
			       context);
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
Skip_Pragma_Clauses(WN         **clause_list,  
                    WN2F_CONTEXT context)
{
   /* Also change Append_Pragma_Clauses() when changing this.
    */
   WN  *clause = *clause_list;
   BOOL more;

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
Skip_Ignored_Clauses(WN *following_clauses, WN **next_clause)
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
		      WN         **clause_list,  
		      WN2F_CONTEXT context)
{
   /* Loop through the sequence of pragmas, emitting those representing
    * attributes to another (already emitted) pragma.  Terminate upon
    * reaching a non-attribute pragma or the end of the pragma list.
    * Also change Skip_Pragma_Clauses() when changing this.  Update the
    * clause_list, such that it denotes the item following the last one
    * processed here.
    */
   WN         *next;
   WN         *clause = *clause_list;
   WN         *wn_after_clauses = *clause_list;

   Skip_Pragma_Clauses(&wn_after_clauses, context);
   while (clause != wn_after_clauses)
   {
      BOOL ignored_clause = FALSE;

      next = clause;
      switch (WN_pragma(clause))
      {
      case WN_PRAGMA_DATA_AFFINITY:
      case WN_PRAGMA_THREAD_AFFINITY:
      case WN_PRAGMA_MPNUM:
      case WN_PRAGMA_SYNC_DOACROSS:
      case WN_PRAGMA_DEFAULT:
	 ignored_clause = TRUE;
	 break; /* Ignore and do not follow with comma */

      case WN_PRAGMA_AFFINITY:
	 Append_Token_String(tokens, "affinity");
	 Append_Clause_Expressions(tokens, WN_PRAGMA_AFFINITY, &clause);

	 Append_Token_Special(tokens, '=');
	 if (WN_pragma(clause) == WN_PRAGMA_DATA_AFFINITY)
	    Append_Token_String(tokens, "data");
	 else if (WN_pragma(clause) == WN_PRAGMA_THREAD_AFFINITY)
	    Append_Token_String(tokens, "thread");
	 else
	     ASSERT_DBG_FATAL(FALSE, 
			      (DIAG_W2F_UNEXPECTED_OPC, 
			       "Append_Pragma_Clauses"));

         /* Process the expression associated with the thread/data affinity
          * pragma.
          */
	 Append_Token_Special(tokens, '(');
	 WN2F_Append_Value_Reference(tokens, WN_kid0(clause));
	 Append_Token_Special(tokens, ')');
	 clause = WN_next(clause);
	 break;

      case WN_PRAGMA_CHUNKSIZE:
	 Append_Token_String(tokens, "chunk");
	 Append_Token_Special(tokens, '=');
	 Append_Clause_Expressions(tokens, WN_PRAGMA_CHUNKSIZE, &clause);
	 break;

      case WN_PRAGMA_IF:
	 Append_Token_String(tokens, "if");
	 Append_Clause_Expressions(tokens, WN_PRAGMA_IF, &clause);
	 break;

      case WN_PRAGMA_LASTLOCAL:
	 if (WN2F_is_omp(clause))
	    Append_Token_String(tokens, "lastprivate");
	 else
	    Append_Token_String(tokens, "lastlocal");
	 Append_Clause_Symbols(tokens, WN_PRAGMA_LASTLOCAL, &clause);
	 break;

      case WN_PRAGMA_LOCAL:
	 if (WN2F_is_omp(clause))
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
	 if (WN2F_is_omp(clause))
	 {
	    Append_Token_String(tokens, "schedule");
	    Append_Token_Special(tokens, '(');
	    Append_MP_Schedtype(tokens, WN_mp_schedtype(clause));
	    if (WN_next(clause) != NULL &&
		WN_pragma(WN_next(clause)) == WN_PRAGMA_CHUNKSIZE)
	    {
	       clause = WN_next(clause);
	       Append_Token_Special(tokens, ',');
	       WN2F_Append_Value_Reference(tokens, WN_kid0(clause));
	    }
	    Append_Token_Special(tokens, ')');
	    clause = WN_next(clause);
	 }
	 else
	 {
	    Append_Token_String(tokens, "mp_schedtype");
	    Append_Token_Special(tokens, '=');
	    Append_MP_Schedtype(tokens, WN_mp_schedtype(clause));
	 }
	 break;

      case WN_PRAGMA_ORDERED:
	 if (WN2F_is_omp(clause))
	     Append_Token_String(tokens, "ordered");
	 else 
	     Append_Token_String(tokens, "(ordered)");
	 break;

      case WN_PRAGMA_REDUCTION:
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
	 Append_Token_String(tokens, "shared");
	 Append_Clause_Symbols(tokens, WN_PRAGMA_SHARED, &clause);
	 break;

      case WN_PRAGMA_ONTO:
	 Append_Token_String(tokens, "onto");
         Append_Clause_Expressions(tokens, WN_PRAGMA_ONTO, &clause,
				   TRUE/*reverse_order*/);
	 break;

      case WN_PRAGMA_LASTTHREAD:
	 Append_Token_String(tokens, "lastthread");
	 Append_Clause_Symbols(tokens, WN_PRAGMA_LASTTHREAD, &clause);
	 break;

      case WN_PRAGMA_FIRSTPRIVATE:
	 Append_Token_String(tokens, "firstprivate");
	 Append_Clause_Symbols(tokens, WN_PRAGMA_FIRSTPRIVATE, &clause);
	 break;

      default:
	 ASSERT_WARN(FALSE,
		     (DIAG_W2F_UNEXPECTED_PRAGMA, " Append_Pragma_Clauses"));
	 break;
      } /* switch */

      /* See if we have already advanced to the next pragma, e.g. as a result
       * of calling Append_Clause_Expressions() or Append_Clause_Symbols(),
       * and if not so, then advance to the next pragma.
       */
      if (next == clause)
         clause = WN_next(clause);

      Skip_Ignored_Clauses(wn_after_clauses, &clause);
      if (clause != wn_after_clauses && !ignored_clause)
	    Append_Token_Special(tokens, ','); /* separate by commas */
   } /* for each attribute pragma */

   *clause_list = clause;
} /* Append_Pragma_Clauses */


static void
Emit_To_PUinfo_Pragmas(WN **next, WN2F_CONTEXT context)
{
   /* This is a special handler for pragmas that must be taken out of
    * a statement list context and instead must be appended to the 
    * PUinfo_pragmas list.
    */
   TOKEN_BUFFER tokens = New_Token_Buffer();

   ASSERT_DBG_FATAL(WN_operator(*next) == OPR_PRAGMA ||
		    WN_operator(*next) == OPR_XPRAGMA, 
		    (DIAG_W2F_UNEXPECTED_OPC, "WN2F_pragma"));

   switch (WN_pragma(*next))
   {
   case WN_PRAGMA_DISTRIBUTE:
      WN2F_Directive_Newline(tokens, "C$", WN_Get_Linenum(*next));
      Append_Token_String(tokens, "DISTRIBUTE");
      Append_A_Clause_Symbol(tokens, *next, 0/*ofst*/);
      Append_Distribution(tokens, next, WN_PRAGMA_DISTRIBUTE);
      Append_Pragma_Clauses(tokens, next, context);
      break;

   case WN_PRAGMA_DISTRIBUTE_RESHAPE:
      WN2F_Directive_Newline(tokens, "C$", WN_Get_Linenum(*next));
      Append_Token_String(tokens, "DISTRIBUTE RESHAPE");
      Append_A_Clause_Symbol(tokens, *next, 0/*ofst*/);
      Append_Distribution(tokens, next, WN_PRAGMA_DISTRIBUTE_RESHAPE);
      Append_Pragma_Clauses(tokens, next, context);
      break;

   default:
      ASSERT_WARN(FALSE,
                  (DIAG_W2F_UNEXPECTED_PRAGMA, "Emit_To_PUinfo_Pragmas"));
      break;
   }
   Prepend_And_Reclaim_Token_List(PUinfo_pragmas, &tokens);
} /* Emit_To_PUinfo_Pragmas */


static WN *
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
WN2F_process_pragma(TOKEN_BUFFER tokens, WN **next, WN2F_CONTEXT context)
{
   /* This procedure will translate the "next" pragma and and any associated
    * clauses, such that "next" end up pointing to the WN* after the pragma
    * and clauses.
    */
   WN       *apragma = *next;
   WN       *this_pragma = apragma;
   WN       *first_clause;
   const WN *surrounding_region;

   ASSERT_DBG_FATAL(WN_operator(apragma) == OPR_PRAGMA ||
		    WN_operator(apragma) == OPR_XPRAGMA, 
		    (DIAG_W2F_UNEXPECTED_OPC, "WN2F_process_pragma"));

   switch (WN_pragma(apragma))
   {
   case WN_PRAGMA_INLINE_DEPTH:
      WN2F_Directive_Newline(tokens, "C*$*", WN_Get_Linenum(apragma));
      Append_Token_String(tokens,"INLINE_DEPTH");
      Append_Token_Special(tokens,'=');
      EMIT_ARG_NUMBERS1(tokens, WN_pragma_arg1(apragma));
      break;

   case WN_PRAGMA_AGGRESSIVE_INNER_LOOP_FISSION:
      WN2F_Directive_Newline(tokens, "C*$*", WN_Get_Linenum(apragma));
      Append_Token_String(tokens,"AGGRESSIVE INNER LOOP FISSION");
      break;

   case WN_PRAGMA_FISSION:
      WN2F_Directive_Newline(tokens, "C*$*", WN_Get_Linenum(apragma));
      Append_Token_String(tokens,"FISSION");
      PARENTHESIZE_ARG_NUMBERS1(tokens, WN_pragma_arg1(apragma));
      break;

   case WN_PRAGMA_FISSIONABLE:
      WN2F_Directive_Newline(tokens, "C*$*", WN_Get_Linenum(apragma));
      Append_Token_String(tokens,"FISSIONABLE");
      break;

   case WN_PRAGMA_FUSE:
      WN2F_Directive_Newline(tokens, "C*$*", WN_Get_Linenum(apragma));
      Append_Token_String(tokens,"FUSE");
      PARENTHESIZE_ARG_NUMBERS2(tokens, 
                                WN_pragma_arg1(apragma),
                                WN_pragma_arg2(apragma));
      break;

   case WN_PRAGMA_FUSEABLE:
      WN2F_Directive_Newline(tokens, "C*$*", WN_Get_Linenum(apragma));
      Append_Token_String(tokens,"FUSABLE");
      break;

   case WN_PRAGMA_NO_FISSION:
      WN2F_Directive_Newline(tokens, "C*$*", WN_Get_Linenum(apragma));
      Append_Token_String(tokens,"NO FISSION");
      break;

   case WN_PRAGMA_NO_FUSION:
      WN2F_Directive_Newline(tokens, "C*$*", WN_Get_Linenum(apragma));
      Append_Token_String(tokens,"NO FUSION");
      break; 

   case WN_PRAGMA_INTERCHANGE:
      WN2F_Directive_Newline(tokens, "C*$*", WN_Get_Linenum(apragma));
      Append_Token_String(tokens, "INTERCHANGE");
      Append_Clause_Symbols(tokens, (WN_PRAGMA_ID)WN_pragma(apragma),
                            &apragma);
      break;

   case WN_PRAGMA_NO_INTERCHANGE:
      WN2F_Directive_Newline(tokens, "C*$*", WN_Get_Linenum(apragma));
      Append_Token_String(tokens, "NO INTERCHANGE");
      break;

   case WN_PRAGMA_BLOCKING_SIZE:
      WN2F_Directive_Newline(tokens, "C*$*", WN_Get_Linenum(apragma));
      Append_Token_String(tokens, "BLOCKING SIZE");
      PARENTHESIZE_ARG_NUMBERS2(tokens, 
                                WN_pragma_arg1(apragma),
                                WN_pragma_arg2(apragma));
      break;

   case WN_PRAGMA_NO_BLOCKING:
      WN2F_Directive_Newline(tokens, "C*$*", WN_Get_Linenum(apragma));
      Append_Token_String(tokens, "NO BLOCKING");
      break;

   case WN_PRAGMA_UNROLL:
      WN2F_Directive_Newline(tokens, "C*$*", WN_Get_Linenum(apragma));
      Append_Token_String(tokens, "UNROLL");
      PARENTHESIZE_ARG_NUMBERS2(tokens, 
                                WN_pragma_arg1(apragma), 
                                WN_pragma_arg2(apragma));
      break;

   case WN_PRAGMA_BLOCKABLE:
      WN2F_Directive_Newline(tokens, "C*$*", WN_Get_Linenum(apragma));
      Append_Token_String(tokens, "BLOCKABLE");
      Append_Clause_Symbols(tokens, (WN_PRAGMA_ID)WN_pragma(apragma),
                            &apragma);
      break;

   case WN_PRAGMA_PREFETCH:
      WN2F_Directive_Newline(tokens, "C*$*", WN_Get_Linenum(apragma));
      Append_Token_String(tokens, "PREFETCH");
      PARENTHESIZE_ARG_NUMBERS2(tokens, 
                                WN_pragma_arg1(apragma),
                                WN_pragma_arg2(apragma));
      break;

   case WN_PRAGMA_PREFETCH_MANUAL:
      WN2F_Directive_Newline(tokens, "C*$*", WN_Get_Linenum(apragma));
      Append_Token_String(tokens, "PREFETCH_MANUAL");
      PARENTHESIZE_ARG_NUMBERS1(tokens, WN_pragma_arg1(apragma));
      break;

   case WN_PRAGMA_PREFETCH_REF:
      if (WN_next(apragma) != NULL && 
          WN_operator(WN_next(apragma)) == OPR_PREFETCH)
      {
         WN2F_Directive_Newline(tokens, "C*$*", WN_Get_Linenum(apragma));
         Append_Token_String(tokens, "PREFETCH_REF");
         Append_Prefetch_Attributes(tokens, 
                                    WN_next(apragma),
                                    WN_pragma_arg2(apragma));
      }
      break;

   case WN_PRAGMA_PREFETCH_REF_DISABLE:
      WN2F_Directive_Newline(tokens, "C*$*", WN_Get_Linenum(apragma));
      Append_Token_String(tokens, "PREFETCH_REF_DISABLE");
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
      WN2F_Directive_Newline(tokens, "C$", WN_Get_Linenum(apragma));
      Append_Token_String(tokens, "REDISTRIBUTE");
      Append_A_Clause_Symbol(tokens, apragma, 0/*ofst*/);
      Append_Distribution(tokens, &apragma, WN_PRAGMA_REDISTRIBUTE);
      Append_Pragma_Clauses(tokens, &apragma, context);
      break;
      
   case WN_PRAGMA_DISTRIBUTE_RESHAPE:
      Emit_To_PUinfo_Pragmas(&apragma, context);
      break;
      
   case WN_PRAGMA_DYNAMIC:
      WN2F_Directive_Newline(tokens, "C$", WN_Get_Linenum(apragma));
      Append_Token_String(tokens, "DYNAMIC");
      Append_A_Clause_Symbol(tokens, apragma, 0/*ofst*/);
      break;

   case WN_PRAGMA_IVDEP:
      WN2F_Directive_Newline(tokens, "CDIR$", WN_Get_Linenum(apragma));
      Append_Token_String(tokens, "IVDEP");
      break;

   case WN_PRAGMA_DOACROSS:
      /* Ignore deeper nests.
       */
      if (WN_pragma_nest(apragma) <= 0 && 
	  !Ignore_Synchronized_Construct(apragma, context))
      {
	 surrounding_region = W2CF_Get_Parent(W2CF_Get_Parent(apragma));
	 first_clause = WN_next(apragma);

	 Put_Pragma_Start_With_Caveats(tokens,apragma,TRUE);
         Append_Token_String(tokens, "DOACROSS");
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
				first_clause);
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
      WN2F_Directive_Newline(tokens, "C$", WN_Get_Linenum(apragma));
      if (WN2F_is_omp(apragma))
      {
	 Append_Token_String(tokens, "SCHEDULE");
	 Append_Token_Special(tokens, '(');
	 Append_MP_Schedtype(tokens, WN_mp_schedtype(apragma));
	 if (WN_next(apragma) != NULL &&
	     WN_pragma(WN_next(apragma)) == WN_PRAGMA_CHUNKSIZE)
	 {
	    apragma = WN_next(apragma);
	    Append_Token_Special(tokens, ',');
	    WN2F_Append_Value_Reference(tokens, WN_kid0(apragma));
	 }
	 Append_Token_Special(tokens, ')');
	 apragma = WN_next(apragma);
      }
      else
      {
	 Append_Token_String(tokens, "MP_SCHEDTYPE");
	 Append_Token_Special(tokens, '=');
	 Append_MP_Schedtype(tokens, WN_mp_schedtype(apragma));
      }
      break;

   case WN_PRAGMA_BARRIER:
      WN2F_Directive_Newline(tokens, "C$", WN_Get_Linenum(apragma));
      WN2F_Append_Pragma_Preamble(tokens,apragma) ;
      Append_Token_String(tokens, "BARRIER");
      break;

   case WN_PRAGMA_COPYIN:
      WN2F_Directive_Newline(tokens, "C$", WN_Get_Linenum(apragma));
      if (WN2F_is_omp(apragma))
	WN2F_Append_Pragma_Preamble(tokens,apragma) ;
      Append_Token_String(tokens, "COPYIN");
      if (WN_operator(apragma) == OPR_XPRAGMA)
         Append_Clause_Expressions(tokens,
				   (WN_PRAGMA_ID)WN_pragma(apragma),
                                   &apragma);
      else
      {
         /* A common symbol */
         Append_Token_Special(tokens, '/');
	 ST2F_use_translate(tokens, WN_st(apragma));
         Append_Token_Special(tokens, '/');
      }
      break;

   case WN_PRAGMA_CRITICAL_SECTION_BEGIN:
      WN2F_Directive_Newline(tokens, "C$", WN_Get_Linenum(apragma));
      WN2F_Append_Pragma_Preamble(tokens,apragma) ;
      if (WN2F_is_omp(apragma))
	  Append_Token_String(tokens, "CRITICAL");
      else
	  Append_Token_String(tokens, "CRITICAL SECTION");
      if (WN_operator(apragma) == OPR_XPRAGMA)
	 Append_Clause_Expressions(tokens,
				   (WN_PRAGMA_ID)WN_pragma(apragma),
                                   &apragma);
      break;

   case WN_PRAGMA_CRITICAL_SECTION_END:
      WN2F_Directive_Newline(tokens, "C$", WN_Get_Linenum(apragma));
      WN2F_Append_Pragma_Preamble(tokens,apragma) ;
      if (WN2F_is_omp(apragma))
	  Append_Token_String(tokens, "END CRITICAL");
      else
	  Append_Token_String(tokens, "END CRITICAL SECTION");
      break;

   case WN_PRAGMA_ORDERED_BEGIN:
      WN2F_Directive_Newline(tokens, "C$", WN_Get_Linenum(apragma));
      WN2F_Append_Pragma_Preamble(tokens,apragma) ;
      Append_Token_String(tokens, "ORDERED");
      if (WN_operator(apragma) == OPR_XPRAGMA)
	 Append_Clause_Expressions(tokens,
				   (WN_PRAGMA_ID)WN_pragma(apragma),
                                   &apragma);
      break;

   case WN_PRAGMA_ORDERED_END:
      WN2F_Directive_Newline(tokens, "C$", WN_Get_Linenum(apragma));
      WN2F_Append_Pragma_Preamble(tokens,apragma) ;
      Append_Token_String(tokens, "END ORDERED");
      break;

   case WN_PRAGMA_ATOMIC:
      WN2F_Directive_Newline(tokens, "C$", WN_Get_Linenum(apragma));
      WN2F_Append_Pragma_Preamble(tokens,apragma) ;
      Append_Token_String(tokens, "ATOMIC");
      if (WN_operator(apragma) == OPR_XPRAGMA)
	 Append_Clause_Expressions(tokens,
				   (WN_PRAGMA_ID)WN_pragma(apragma),
                                   &apragma);
      break;

   case WN_PRAGMA_PARALLEL_BEGIN:
      /* Ignore deeper nests.
       */
      if (!Ignore_Synchronized_Construct(apragma, context))
      {
	 surrounding_region = W2CF_Get_Parent(W2CF_Get_Parent(apragma));
	 first_clause = WN_next(apragma);
      
	 WN2F_Directive_Newline(tokens, "C$", WN_Get_Linenum(apragma));

	 WN2F_Append_Pragma_Preamble(tokens,apragma) ;
	 Append_Token_String(tokens, "PARALLEL");
	 apragma = first_clause;
	 Append_Pragma_Clauses(tokens, &apragma, context);
	 Append_Implicit_Locals(tokens, 
				WN_PRAGMA_PARALLEL_BEGIN, 
				WN_region_body(surrounding_region),
				first_clause);
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
	 Put_Pragma_Start_With_Caveats(tokens,apragma,TRUE);
	 WN2F_Append_Pragma_Preamble(tokens,apragma) ;
         Append_Token_String(tokens, "PARALLEL DO");

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
				first_clause);
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
      if (WN_pragma_nest(apragma) <= 0 &&
	  !Ignore_Synchronized_Construct(apragma, context))
      {
	 surrounding_region = W2CF_Get_Parent(W2CF_Get_Parent(apragma));
	 first_clause = WN_next(apragma);

	 Put_Pragma_Start_With_Caveats(tokens,apragma,TRUE);
	 WN2F_Append_Pragma_Preamble(tokens,apragma) ;
	 if (WN2F_is_omp(apragma)) 
	   Append_Token_String(tokens, "DO");
	 else
	   Append_Token_String(tokens, "PDO");

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
	  *		WN_PRAGMA_PDO_BEGIN,
	  *		WN_region_body(surrounding_region),
	  *		first_clause);
	  */
      }
      else
      {
	 apragma = WN_next(apragma);
         Skip_Pragma_Clauses(&apragma, context);
      }
      break;

      /* region construct => construct id on region..*/

   case WN_PRAGMA_PARALLEL_SECTIONS:
   case WN_PRAGMA_PSECTION_BEGIN:
      WN2F_Directive_Newline(tokens, "C$", WN_Get_Linenum(apragma));
      WN2F_Append_Pragma_Preamble(tokens,apragma);
      if (WN2F_is_omp(apragma)) 
	 Append_Token_String(tokens, "PARALLEL SECTIONS");
      else
	 Append_Token_String(tokens, "PSECTIONS");
      apragma = WN_next(apragma);
      Append_Pragma_Clauses(tokens, &apragma, context);
      break;

   case WN_PRAGMA_SECTION:
      WN2F_Directive_Newline(tokens, "C$", WN_Get_Linenum(apragma));
      WN2F_Append_Pragma_Preamble(tokens,apragma);
      Append_Token_String(tokens, "SECTION");
      break;

      /* region construct => construct id on region..*/

   case WN_PRAGMA_SINGLE_PROCESS_BEGIN:
      if (!Ignore_Synchronized_Construct(apragma, context))
      {
	 WN2F_Directive_Newline(tokens, "C$", WN_Get_Linenum(apragma));
	 WN2F_Append_Pragma_Preamble(tokens,apragma) ;
	 if (WN2F_is_omp(apragma))
	   Append_Token_String(tokens, "SINGLE");
	 else
	   Append_Token_String(tokens, "SINGLE PROCESS");
	 apragma = WN_next(apragma);
	 Append_Pragma_Clauses(tokens, &apragma, context);
      }
      break;

      /* region construct => construct id on region..*/

    case WN_PRAGMA_MASTER_BEGIN:
      if (!Ignore_Synchronized_Construct(apragma, context))
      {
	 WN2F_Directive_Newline(tokens, "C$", WN_Get_Linenum(apragma));
	 WN2F_Append_Pragma_Preamble(tokens,apragma) ;
	 if (WN2F_is_omp(apragma))
	   Append_Token_String(tokens, "MASTER");
	 else
	   Append_Token_String(tokens, "MASTER PROCESS");
      }
      break;

   case WN_PRAGMA_NUMTHREADS:
      /* Should only appear for C, but if we ever see it, we also emit
       * it for Fortran.
       */
      WN2F_Directive_Newline(tokens, "C$", WN_Get_Linenum(apragma));
      Append_Token_String(tokens, "NUMTHREADS");
      Append_Clause_Expressions(tokens,
				(WN_PRAGMA_ID)WN_pragma(apragma),
                                &apragma);
      break;

   case WN_PRAGMA_PAGE_PLACE:
      WN2F_Directive_Newline(tokens, "C$", WN_Get_Linenum(apragma));
      Append_Token_String(tokens, "PAGE_PLACE");
      Append_Clause_Expressions(tokens,
				(WN_PRAGMA_ID)WN_pragma(apragma),
                                &apragma);
      break;

   case WN_PRAGMA_NORECURRENCE:
      WN2F_Directive_Newline(tokens, "CDIR$", WN_Get_Linenum(apragma));
      Append_Token_String(tokens, "NO RECURRENCE");
      break;

   case WN_PRAGMA_NEXT_SCALAR:
      WN2F_Directive_Newline(tokens, "CDIR$", WN_Get_Linenum(apragma));
      Append_Token_String(tokens, "NEXT SCALAR");
      break;

   case WN_PRAGMA_KAP_CONCURRENTIZE:
      WN2F_Directive_Newline(tokens, "C*$*", WN_Get_Linenum(apragma));
      Append_Token_String(tokens, "CONCURRENTIZE");
      break;

   case WN_PRAGMA_KAP_NOCONCURRENTIZE:
      WN2F_Directive_Newline(tokens, "C*$*", WN_Get_Linenum(apragma));
      Append_Token_String(tokens, "NO CONCURRENTIZE");
      break;

   case WN_PRAGMA_KAP_ASSERT_PERMUTATION:
      WN2F_Directive_Newline(tokens, "C*$*", WN_Get_Linenum(apragma));
      Append_Token_String(tokens, "ASSERT PERMUTATION");
      Append_A_Clause_Symbol(tokens, apragma, 0/*ofst*/);
      break;

   case WN_PRAGMA_CRI_CNCALL:
   case WN_PRAGMA_KAP_ASSERT_CONCURRENT_CALL:
      WN2F_Directive_Newline(tokens, "C*$*", WN_Get_Linenum(apragma));
      Append_Token_String(tokens, "ASSERT CONCURRENT CALL");
      break;

   case WN_PRAGMA_KAP_ASSERT_DO:
      WN2F_Directive_Newline(tokens, "C*$*", WN_Get_Linenum(apragma));
      if (WN_pragma_arg1(apragma) == ASSERT_DO_CONCURRENT)
	 Append_Token_String(tokens, "ASSERT DO (CONCURRENT)");
      else
	 Append_Token_String(tokens, "ASSERT DO (SERIAL)");
      break;

   case WN_PRAGMA_KAP_ASSERT_DOPREFER:
      WN2F_Directive_Newline(tokens, "C*$*", WN_Get_Linenum(apragma));
      if (WN_pragma_arg1(apragma) == ASSERT_DO_CONCURRENT)
	 Append_Token_String(tokens, "ASSERT DO PREFER (CONCURRENT)");
      else
	 Append_Token_String(tokens, "ASSERT DO PREFER (SERIAL)");
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

} /* WN2F_process_pragma */


/* ====================== Exported Functions ====================== */
/* ================================================================ */


BOOL
WN2F_Skip_Pragma_Stmt(WN *wn)
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
} /* WN2F_Skip_Pragma_Stmt */


WN2F_STATUS
WN2F_pragma(TOKEN_BUFFER tokens, WN *wn, WN2F_CONTEXT context)
{
   WN *skip;
   WN *next = wn;

   ASSERT_DBG_FATAL(WN_operator(wn) == OPR_PRAGMA ||
		    WN_operator(wn) == OPR_XPRAGMA, 
		    (DIAG_W2F_UNEXPECTED_OPC, "WN2F_pragma"));

   WN2F_process_pragma(tokens, &next, context);

   ASSERT_FATAL(Pragmas_To_Skip.end == 0,
		(DIAG_W2F_BUFFER_ERROR,
		 "Unexpected index for Pragmas_To_Skip in WN2F_pragma()"));

   /* For pragmas inlined in code, we need to keep track of the pragmas
    * that have already been processed.
    */
   for (skip = WN_next(wn); skip != next; skip = WN_next(skip))
   {
      ASSERT_FATAL(Pragmas_To_Skip.end < MAX_PRAGMAS_TO_SKIP,
		   (DIAG_W2F_BUFFER_ERROR,
		    "Too many pragmas in sequence in WN2F_pragma()"));

      Pragmas_To_Skip.array[Pragmas_To_Skip.end++] = skip;
   }

   return EMPTY_WN2F_STATUS;
} /* WN2F_pragma */


WN2F_STATUS 
WN2F_pragma_list_begin(TOKEN_BUFFER tokens,
                       WN          *first_pragma,
                       WN2F_CONTEXT context)
{
   /* This is called for a region or a func_entry, which has a separate
    * block to hold a pragma-list.
    */
   WN *next_pragma = first_pragma;

   while (next_pragma != NULL)
   {
      if (WN_operator(next_pragma) == OPR_PRAGMA ||
	  WN_operator(next_pragma) == OPR_XPRAGMA)
	 WN2F_process_pragma(tokens, &next_pragma, context);
      else
	 next_pragma = WN_next(next_pragma);
   }
   return EMPTY_WN2F_STATUS;
} /* WN2F_pragma_list_begin */


WN2F_STATUS 
WN2F_pragma_list_end(TOKEN_BUFFER tokens, 
                     WN          *first_pragma,
                     WN2F_CONTEXT context)
{
   /* This is called for a region or a func_entry, which has a separate
    * block to hold a pragma-list.
    */
   BOOL emitted = TRUE;

   /* Skip code inserted into the pragma region (may occur for C++, so
    * why not for other languages?).
    */
   while (first_pragma != NULL                  &&
	  WN_operator(first_pragma) != OPR_PRAGMA &&
	  WN_operator(first_pragma) != OPR_XPRAGMA)
   {
      first_pragma = WN_next(first_pragma);
   }

   if (first_pragma != NULL)
   {
      ASSERT_DBG_FATAL(WN_operator(first_pragma) == OPR_PRAGMA ||
                       WN_operator(first_pragma) == OPR_XPRAGMA, 
                       (DIAG_W2F_UNEXPECTED_OPC, "WN2F_pragma_list_end"));

      switch (WN_pragma(first_pragma))
      {
      case WN_PRAGMA_PARALLEL_BEGIN:
	 if (!Ignore_Synchronized_Construct(first_pragma, context))
	 {
	    WN2F_Directive_Newline(tokens, "C$", WN_Get_Linenum(first_pragma));
	    WN2F_Append_Pragma_Preamble(tokens,first_pragma);
	    Append_Token_String(tokens, "END PARALLEL");
	 }
         break;

      case WN_PRAGMA_DOACROSS:
      case WN_PRAGMA_PARALLEL_DO:
	 break;
	 
      case WN_PRAGMA_PDO_BEGIN:
	 if (WN_pragma_nest(first_pragma) <= 0 &&
	     !Ignore_Synchronized_Construct(first_pragma, context))
	 {
	    Put_Pragma_Start_With_Caveats(tokens,first_pragma,FALSE);
	    WN2F_Append_Pragma_Preamble(tokens,first_pragma);
	    if (WN2F_is_omp(first_pragma)) 
	      Append_Token_String(tokens, "END DO");
	    else
	      Append_Token_String(tokens, "END PDO");
	 }
         break;

      case WN_PRAGMA_PARALLEL_SECTIONS:
      case WN_PRAGMA_PSECTION_BEGIN:
         WN2F_Directive_Newline(tokens, "C$", WN_Get_Linenum(first_pragma));
	 WN2F_Append_Pragma_Preamble(tokens,first_pragma);
         Append_Token_String(tokens, "END PSECTION");
         break;

      case WN_PRAGMA_SINGLE_PROCESS_BEGIN:
	 if (!Ignore_Synchronized_Construct(first_pragma, context))
	 {
	    WN2F_Directive_Newline(tokens, "C$", WN_Get_Linenum(first_pragma));
	    WN2F_Append_Pragma_Preamble(tokens,first_pragma);
	    if (WN2F_is_omp(first_pragma)) 
	      Append_Token_String(tokens, "END SINGLE");
	    else 
	      Append_Token_String(tokens, "END SINGLE PROCESS");
	 }
         break;

      case WN_PRAGMA_MASTER_BEGIN:
	 if (!Ignore_Synchronized_Construct(first_pragma, context))
	 {
	    WN2F_Directive_Newline(tokens, "C$", WN_Get_Linenum(first_pragma));
	    WN2F_Append_Pragma_Preamble(tokens,first_pragma);
	    Append_Token_String(tokens, "END MASTER");
	 }
         break;

      default:
         emitted = FALSE;
         break; /* Not a region that needs an END pragma */
      }

      if (emitted && WN2F_pragma_list_nowait(first_pragma))
         Append_Token_String(tokens, "nowait");
   }
   return EMPTY_WN2F_STATUS;
} /* WN2F_pragma_list_end */


BOOL
Ignore_Synchronized_Construct(WN          *construct_pragma,  
			      WN2F_CONTEXT context)
{
   /* This can be TRUE for DOACROSS, PARALLEL, and any paralellization
    * related construct that may occur within a parallel region.
    * It only applies for mplist (i.e. when Run_w2fc_early).
    */
   BOOL ignore_construct;

   Is_True(WN_operator(construct_pragma) == OPR_PRAGMA,
	   ("Unexpected WHIRL tree in Ignore_Synchronized_Construct"));

   if (!Run_w2fc_early)
   {
      ignore_construct = FALSE;
   }
   else
   {
      if (WN_pragma(construct_pragma) != WN_PRAGMA_DOACROSS)
	 construct_pragma = Get_Enclosing_Parallel_Region(construct_pragma);

      if (construct_pragma == NULL)
	 ignore_construct = FALSE;
      else
      {
	 WN *clause = WN_next(construct_pragma);
	 WN *beyond_last_clause = clause;

	 Skip_Pragma_Clauses(&beyond_last_clause, context);
	 while (clause != beyond_last_clause && 
		WN_pragma(clause) != WN_PRAGMA_SYNC_DOACROSS)
	    clause = WN_next(clause);
	 ignore_construct = (clause != beyond_last_clause);
      }
   }
   return ignore_construct;
} /* Ignore_Synchronized_Construct */

