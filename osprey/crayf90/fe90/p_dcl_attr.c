/*
 *  Copyright (C) 2006. QLogic Corporation. All Rights Reserved.
 */

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



static char USMID[] = "\n@(#)5.0_pl/sources/p_dcl_attr.c	5.2	06/17/99 09:28:10\n";

# include "defines.h"		/* Machine dependent ifdefs */

# include "host.m"		/* Host machine dependent macros.*/
# include "host.h"		/* Host machine dependent header.*/
# include "target.m"		/* Target machine dependent macros.*/
# include "target.h"		/* Target machine dependent header.*/

# include "globals.m"
# include "tokens.m"
# include "sytb.m"
# include "p_globals.m"
# include "debug.m"

# include "globals.h"
# include "tokens.h"
# include "sytb.h"
# include "p_globals.h"


/*****************************************************************\
|* function prototypes of static functions declared in this file *|
\*****************************************************************/
#ifdef KEY /* Bug 14150 */
static int parse_attrs(boolean (*func) (boolean, int, int, int));
#else /* defined(BUILD_OS_DARWIN) */
static void parse_attrs(boolean (*func) (boolean, int, int, int));
#endif /* KEY Bug 14150 */


/******************************************************************************\
|*									      *|
|* Description:								      *|
|*      This is a generic parser, used by all the attribute statements,       *|
|*      except for PUBLIC and PRIVATE.  It parses the statements, and         *|
|*      calls the appropriate merge routine to update the symbol table.       *|
|*									      *|
|* Input parameters:							      *|
|*	merge_function - pointer to the merge function to call		      *|
|*									      *|
|* Output parameters:							      *|
|*	NONE								      *|
|*									      *|
|* Returns:								      *|
|*	NONE								      *|
|*									      *|
\******************************************************************************/

#ifdef KEY /* Bug 14150 */
static int
#else /* KEY Bug 14150 */
static void
#endif /* KEY Bug 14150 */
parse_attrs(boolean (*merge_function) ())

{
   int		array_idx;
   int		attr_idx;
   boolean	blk_err		= FALSE;
   int		column;
   boolean	found_attr;
   boolean	found_end	= FALSE;
   int		line;
   int		name_idx;
   int		new_sb_idx;
   int		sb_idx;
#ifdef KEY /* Bug 14150 */
   int		count = 0;
#endif /* KEY Bug 14150 */


   TRACE (Func_Entry, "parse_attrs", NULL);

   if (LA_CH_VALUE == COLON &&
       matched_specific_token(Tok_Punct_Colon_Colon, Tok_Class_Punct)) {

      /* Intentionally blank */
   }

   if ((STMT_OUT_OF_ORDER(curr_stmt_category, stmt_type) ||
        STMT_CANT_BE_IN_BLK(stmt_type, CURR_BLK)) && iss_blk_stk_err()) {

      /* block error issued by check. */

      blk_err = TRUE;
   }
   else {
      curr_stmt_category = Declaration_Stmt_Cat;
   }

   do {
      if (MATCHED_TOKEN_CLASS(Tok_Class_Id)) {	/* TOKEN is the id */
         line		= TOKEN_LINE(token);
         column		= TOKEN_COLUMN(token);
         attr_idx	= srch_sym_tbl(TOKEN_STR(token), TOKEN_LEN(token),
                                       &name_idx);
         found_attr	= TRUE;

         if (attr_idx == NULL_IDX) {
            found_attr			= FALSE;
#ifdef KEY /* Bug 14110 */
	    /* Handle the case of "volatile x" when we haven't seen any other
	     * declaration for "x" in the current scope, but there's an "x" in
	     * the host scope. If we see no further declaration in the current
	     * scope, then this will refer to the host-associated "x" instead
	     * of creating a local one. */
	    int junk_idx;
            if (merge_function == merge_volatile &&
	      srch_host_sym_tbl(TOKEN_STR(token), TOKEN_LEN(token), &junk_idx,
	        FALSE) &&
	      (LA_CH_VALUE == COMMA || LA_CH_VALUE == EOS)) {
	      surprise_volatile(memcpy(malloc(TOKEN_LEN(token) + sizeof '\0'),
	        TOKEN_STR(token), TOKEN_LEN(token)));
	      int save_la_ch_value = LA_CH_VALUE;
	      NEXT_LA_CH;

	      if (save_la_ch_value == COMMA) {
		continue;
	      }
	      else if (save_la_ch_value == EOS) {
	        break;
	      }
	    }
#endif /* KEY Bug 14110 */
            attr_idx			= ntr_sym_tbl(&token, name_idx);
            LN_DEF_LOC(name_idx)	= TRUE;      /* Can't be host assoc */

            /* The merge functions set the implicit type - if needed */
         }
         else if (AT_REFERENCED(attr_idx) == Char_Rslt_Bound_Ref) {
            AT_ATTR_LINK(attr_idx)	= NULL_IDX;
            LN_DEF_LOC(name_idx)	= TRUE;
         }

         if (AT_OBJ_CLASS(attr_idx) == Data_Obj) {
            ATD_SEEN_OUTSIDE_IMP_DO(attr_idx) = TRUE;
         }

         if (LA_CH_VALUE == LPAREN) {

            switch (stmt_type) {
      
               case Allocatable_Stmt:
               case Automatic_Stmt:
               case Dimension_Stmt:
               case Pointer_Stmt:
               case Target_Stmt:
                  array_idx = parse_array_spec(attr_idx);

                  merge_dimension(attr_idx, line, column, array_idx);

                  if (!found_attr) {
                     SET_IMPL_TYPE(attr_idx);
                  }
                  found_attr = TRUE;  /* Have to merge with dimension */
                  break;

               default:
                  if (parse_err_flush(Find_Rparen, ", or " EOS_STR)) {
                     NEXT_LA_CH;	/* Get Rparen */
                  }
                  break;

            }  /* End switch */
         }
         else if (stmt_type == Dimension_Stmt) {

           /* DIMENSION needs dim spec */

# ifdef _F_MINUS_MINUS

            if ((!cmd_line_flags.co_array_fortran) || LA_CH_VALUE != LBRKT) {
               parse_err_flush(Find_Comma, "(");
               AT_DCL_ERR(attr_idx) = TRUE;
            }
# else
            parse_err_flush(Find_Comma, "(");
            AT_DCL_ERR(attr_idx) = TRUE;
# endif
         }

# ifdef _F_MINUS_MINUS

         if (LA_CH_VALUE == LBRKT && cmd_line_flags.co_array_fortran &&
             (stmt_type == Allocatable_Stmt ||
              stmt_type == Automatic_Stmt ||
              stmt_type == Dimension_Stmt ||
              stmt_type == Pointer_Stmt ||
              stmt_type == Target_Stmt)) {
            array_idx	= parse_pe_array_spec(attr_idx);
            merge_co_array(found_attr, line, column, attr_idx, array_idx);
         }
# endif

         if (stmt_type != Dimension_Stmt) {
            (*merge_function) (found_attr, line, column, attr_idx);
#ifdef KEY /* Bug 14150 */
            count += 1;
#endif /* KEY Bug 14150 */
         }

         AT_DCL_ERR(attr_idx) = AT_DCL_ERR(attr_idx) | blk_err;

         if ((cif_flags & XREF_RECS) != 0) {
            cif_usage_rec(attr_idx,
                          AT_Tbl_Idx,
                          line,
                          column,
                          CIF_Symbol_Declaration);
         }
      }
      else if (LA_CH_VALUE == SLASH &&
               (stmt_type == Save_Stmt ||
#ifdef KEY /* Bug 14150 */
                stmt_type == Bind_Stmt ||
#endif /* KEY Bug 14150 */
                stmt_type == Volatile_Stmt)) {

         NEXT_LA_CH;		/* Pick up slash */

         if (MATCHED_TOKEN_CLASS(Tok_Class_Id)) {
            sb_idx   = srch_stor_blk_tbl(TOKEN_STR(token),
                                         TOKEN_LEN(token),
                                         curr_scp_idx);

            if (sb_idx == NULL_IDX) {
               sb_idx	= ntr_stor_blk_tbl(TOKEN_STR(token),
                                           TOKEN_LEN(token),
                                           TOKEN_LINE(token),
                                           TOKEN_COLUMN(token),
                                           Common);
               SB_COMMON_NEEDS_OFFSET(sb_idx)	= TRUE;
            }
            else if (SB_USE_ASSOCIATED(sb_idx) || SB_HOST_ASSOCIATED(sb_idx)) {

               /* Common block has been use or host associated into this scp. */
               /* Make an entry for this block and hide the associated block  */
               /* behind it.  storage_blk_resolution will resolve the blocks. */

               new_sb_idx = ntr_stor_blk_tbl(TOKEN_STR(token),
                                             TOKEN_LEN(token),
                                             TOKEN_LINE(token),
                                             TOKEN_COLUMN(token),
                                             Common);
               SB_MERGED_BLK_IDX(sb_idx)		= new_sb_idx;
               SB_COMMON_NEEDS_OFFSET(new_sb_idx)	= TRUE;
               SB_HIDDEN(sb_idx)			= TRUE;
               SB_DEF_MULT_SCPS(sb_idx)			= TRUE;
               sb_idx					= new_sb_idx;
            }

            SB_DCL_ERR(sb_idx)	= SB_DCL_ERR(sb_idx) | blk_err;

            if (stmt_type == Save_Stmt) {

               if (SB_SAVED(sb_idx)) {

                   /* Cannot set SAVE twice for same common block name */

                  PRINTMSG(TOKEN_LINE(token), 110, Error, TOKEN_COLUMN(token),
                           SB_NAME_PTR(sb_idx));
               }

               SB_SAVED(sb_idx)		= TRUE;
            }
#ifdef KEY /* Bug 14150 */
            else if (stmt_type == Bind_Stmt) {
	       set_binding_label(SB_Tbl_Idx, sb_idx, &new_binding_label);
	       count += 1;
	       /* Make sure no common object in this block has been
	        * equivalenced */
	       for (int cobj_idx = SB_FIRST_ATTR_IDX(sb_idx);
		 cobj_idx != NULL_IDX;
		 cobj_idx = ATD_NEXT_MEMBER_IDX(cobj_idx)) {
		 if (ATD_EQUIV(cobj_idx)) {
		   /* Seems most useful to refer to line containing "common"
		    * even if a declaration for the common object appeared
		    * earlier than that */
		   PRINTMSG(TOKEN_LINE(token), 550, Error, TOKEN_COLUMN(token),
		     AT_OBJ_NAME_PTR(cobj_idx), "BIND", "EQUIVALENCE",
		     SB_DEF_LINE(sb_idx));
		 }
	       }
	    }
#endif /* KEY Bug 14150 */
            else {  /* Volatile_Stmt */
               SB_VOLATILE(sb_idx)	= TRUE;
            }

            if ((cif_flags & XREF_RECS) != 0) {
               cif_sb_usage_rec(sb_idx,
                                TOKEN_LINE(token),
                                TOKEN_COLUMN(token),
                                CIF_Symbol_Declaration);
            }

            if (LA_CH_VALUE == SLASH) {
               NEXT_LA_CH;		/* Pick up slash */
            }
            else {
               parse_err_flush(Find_Comma, "/");
            }
         }
         else {
            parse_err_flush(Find_Comma, "common-block-name");
         }
      }
      else {	/* Looking for id or common block name */
         parse_err_flush(Find_Comma, ((stmt_type == Save_Stmt ||
                                       stmt_type == Volatile_Stmt) ?
                                      "object-name or /" : "object-name"));
      }

      if (LA_CH_VALUE != COMMA && LA_CH_VALUE != EOS) {
         parse_err_flush(Find_Comma, ", or " EOS_STR);
      }

      if (LA_CH_VALUE == COMMA) {
         NEXT_LA_CH;				/* Pick up comma */
      }
      else if (LA_CH_VALUE == EOS) {
         found_end = TRUE;
         NEXT_LA_CH;				/* Pick up EOS */
      }
   }  /* End while */
   while (!found_end);

   TRACE (Func_Exit, "parse_attrs", NULL);

#ifdef KEY /* Bug 14150 */
   return count;
#else /* KEY Bug 14150 */
   return;
#endif /* KEY Bug 14150 */


}  /* parse_attrs */


/******************************************************************************\
|*									      *|
|* Description:								      *|
|*       Parse the PUBLIC and PRIVATE statements.                             *|
|*       BNF  - PUBLIC  [[::] access-id-list]                                 *|
|*              PRIVATE [[::] access-id-list]                                 *|
|*              access-id IS use-name OR generic-spec                         *|
|*									      *|
|* Input parameters:							      *|
|*	NONE								      *|
|*									      *|
|* Output parameters:							      *|
|*	NONE								      *|
|*									      *|
|* Returns:								      *|
|*	NONE								      *|
|*									      *|
\******************************************************************************/

void parse_access_stmt()

{
   access_type		access;
   int			attr_idx;
   boolean		found_end;


   TRACE (Func_Entry, "parse_access_stmt", NULL);

   access = (TOKEN_VALUE(token) == Tok_Kwd_Private) ? Private : Public;

   if (CURR_BLK == Derived_Type_Blk && access == Private) {

      if (LA_CH_VALUE == EOS) {

         if (ATT_PRIVATE_CPNT(CURR_BLK_NAME)) {

            /* The PRIVATE statement may only be specified once in a dt def */

            PRINTMSG(TOKEN_LINE(token), 41, Error, TOKEN_COLUMN(token),
                     "PRIVATE", AT_OBJ_NAME_PTR(CURR_BLK_NAME));
         }
         else if (ATT_FIRST_CPNT_IDX(CURR_BLK_NAME) != NULL_IDX) {

            /* PRIVATE must be specified before any components are */

            PRINTMSG(TOKEN_LINE(token), 8, Error, TOKEN_COLUMN(token),
                     "PRIVATE", AT_OBJ_NAME_PTR(CURR_BLK_NAME));
         }

         if (ATP_PGM_UNIT(SCP_ATTR_IDX(curr_scp_idx)) == Module) {
            ATT_PRIVATE_CPNT(CURR_BLK_NAME) = TRUE;
         } 
         else {
            iss_blk_stk_err();
         }
      }
      else {
         parse_err_flush(Find_EOS, EOS_STR);
      }
      curr_stmt_category = Declaration_Stmt_Cat;
   }
   else {

      if (LA_CH_VALUE == EOS) {

         if (CURR_BLK == Module_Blk) {

            if (AT_ACCESS_SET(SCP_ATTR_IDX(curr_scp_idx))) {

               /* Issue error.  Don't allow access to change. */

               PRINTMSG(TOKEN_LINE(token), 656, Error, TOKEN_COLUMN(token),
                        AT_OBJ_NAME_PTR(SCP_ATTR_IDX(curr_scp_idx)));
               access = (access_type) AT_PRIVATE(SCP_ATTR_IDX(curr_scp_idx));
            }

            AT_ACCESS_SET(SCP_ATTR_IDX(curr_scp_idx))	= TRUE;
            AT_PRIVATE(SCP_ATTR_IDX(curr_scp_idx))	= access;
         }
         else {
            /* Intentionally blank.  If this is not a MODULE, it will be  */
            /* caught at the end of the routine in block checking.        */
         }
      }
      else {
         found_end	= FALSE;

         if (LA_CH_VALUE == COLON) {		       /* Pick up optional :: */
            matched_specific_token(Tok_Punct_Colon_Colon, Tok_Class_Punct);
         }

         do {   /* parse_generic_spec issues CIF records */
            if (parse_generic_spec()) {
               attr_idx = generic_spec_semantics();

               if (CURR_BLK == Module_Blk) {
                  merge_access(attr_idx, TOKEN_LINE(token),
                                         TOKEN_COLUMN(token), access);
               }
            }

            if (LA_CH_VALUE != COMMA && LA_CH_VALUE != EOS) {
               parse_err_flush(Find_Comma, ", or " EOS_STR);
            }

            if (LA_CH_VALUE == COMMA) {
               NEXT_LA_CH;			/* Skip Comma */
            }
            else if (LA_CH_VALUE == EOS) {
               found_end = TRUE;
            }
         }  
         while (!found_end);
      }

      if ((CURR_BLK != Module_Blk ||
           STMT_OUT_OF_ORDER(curr_stmt_category, stmt_type)) &&
         iss_blk_stk_err()) {
         /* Block error - intentionally left blank */
      }
      else {
         curr_stmt_category = Declaration_Stmt_Cat;
      }
   }
   NEXT_LA_CH;					/* Always will be EOS */

   TRACE (Func_Exit, "parse_access_stmt", NULL);
   return;

}  /* parse_access_stmt */


/******************************************************************************\
|*									      *|
|* Description:								      *|
|*      BNF       - ALLOCATABLE [::] array-name [(deferred-shape-spec-list)]  *|
|*                               [,array-name [(deferred-shape-spec-list)]..  *|
|*									      *|
|* Input parameters:							      *|
|*	NONE								      *|
|*									      *|
|* Output parameters:							      *|
|*	NONE								      *|
|*									      *|
|* Returns:								      *|
|*	NONE								      *|
|*									      *|
\******************************************************************************/

void parse_allocatable_stmt (void)

{
   TRACE (Func_Entry, "parse_allocatable_stmt", NULL);

   parse_attrs(merge_allocatable);

   TRACE (Func_Exit, "parse_allocatable_stmt", NULL);

   return;

}  /* parse_allocatable_stmt */

/******************************************************************************\
|*                                                                            *|
|* Description:								      *|
|*      BNF       - AUTOMATIC [::] object-name 				      *|
|*                                                                            *|
|* Input parameters:							      *|
|*	NONE								      *|
|*									      *|
|* Output parameters:							      *|
|*	NONE								      *|
|*									      *|
|* Returns:								      *|
|*	NONE								      *|
|*									      *|
\******************************************************************************/

void parse_automatic_stmt (void)

{
   TRACE (Func_Entry, "parse_automatic_stmt", NULL);

   PRINTMSG(stmt_start_line, 1253, Ansi, stmt_start_col, "AUTOMATIC");
   
   parse_attrs(merge_automatic);

   TRACE (Func_Exit, "parse_automatic_stmt", NULL);

   return;

}  /* parse_automatic_stmt */


/******************************************************************************\
|*									      *|
|* Description:								      *|
|*      BNF       - DIMENSION [::] array-name (array-spec)                    *|
|*                                 [,array-name (array_spec)]...              *|
|*									      *|
|* Input parameters:							      *|
|*	NONE								      *|
|*									      *|
|* Output parameters:							      *|
|*	NONE								      *|
|*									      *|
|* Returns:								      *|
|*	NONE								      *|
|*									      *|
\******************************************************************************/

void parse_dimension_stmt (void)

{
   TRACE (Func_Entry, "parse_dimension_stmt", NULL);

   parse_attrs(NULL);

   TRACE (Func_Exit, "parse_dimension_stmt", NULL);

   return;

}  /* parse_dimension_stmt */

/******************************************************************************\
|*                                                                            *|
|* Description:                                                               *|
|*      Parse the external statement                                          *|
|*      BNF       - EXTERNAL external-name-list                               *|
|*                                                                            *|
|* Input parameters:                                                          *|
|*      NONE                                                                  *|
|*                                                                            *|
|* Output parameters:                                                         *|
|*      NONE                                                                  *|
|*                                                                            *|
|* Returns:                                                                   *|
|*      NONE                                                                  *|
|*                                                                            *|
\******************************************************************************/

void parse_external_stmt (void)

{
   TRACE (Func_Entry, "parse_external_stmt", NULL);

   parse_attrs(merge_external);

   TRACE (Func_Exit, "parse_external_stmt", NULL);

   return;

}  /* parse_external_stmt */

/******************************************************************************\
|*									      *|
|* Description:								      *|
|*      BNF    - INTENT(intent_spec) [::] dummy-arg-name-list                 *|
|*                                                                            *|
|* Input parameters:							      *|
|*	NONE								      *|
|*									      *|
|* Output parameters:							      *|
|*	NONE								      *|
|*									      *|
|* Returns:								      *|
|*	NONE								      *|
|*									      *|
\******************************************************************************/

void parse_intent_stmt (void)

{
   int		stmt_number;

   TRACE (Func_Entry, "parse_intent_stmt", NULL);

   stmt_number = statement_number;

   if (LA_CH_VALUE != LPAREN) {
      parse_err_flush(Find_EOS, "(");
      NEXT_LA_CH;				/* Pick up EOS */
   }
   else {
      colon_recovery	= TRUE;			/* Can recover at :: */
      new_intent	= parse_intent_spec();
      colon_recovery	= FALSE;

      if (new_intent != Intent_Unseen) {
         parse_attrs(merge_intent);

         if (cif_flags & MISC_RECS) {

            if (new_intent == Intent_In) {
               cif_stmt_type_rec(TRUE, CIF_Intent_In_Stmt, stmt_number);
            }
            else if (new_intent == Intent_Out) {
               cif_stmt_type_rec(TRUE, CIF_Intent_Out_Stmt, stmt_number);
            }
            else {
               cif_stmt_type_rec(TRUE, CIF_Intent_Inout_Stmt, stmt_number);
            }
         }
      }
      else {
         parse_err_flush(Find_EOS, NULL);
         NEXT_LA_CH;				/* Pick up EOS */
      }
   }

   TRACE (Func_Exit, "parse_intent_stmt", NULL);

   return;

}  /* parse_intent_stmt */


/******************************************************************************\
|*                                                                            *|
|* Description:                                                               *|
|*      Parse the intrinsic statement                                         *|
|*      BNF       - INTRINSIC intrinsic-name-list                             *|
|*                                                                            *|
|* Input parameters:                                                          *|
|*      NONE                                                                  *|
|*                                                                            *|
|* Output parameters:                                                         *|
|*      NONE                                                                  *|
|*                                                                            *|
|* Returns:                                                                   *|
|*      NONE                                                                  *|
|*                                                                            *|
\******************************************************************************/

void parse_intrinsic_stmt (void)

{
   TRACE (Func_Entry, "parse_intrinsic_stmt", NULL);

   parse_attrs(merge_intrinsic);

   TRACE (Func_Exit, "parse_intrinsic_stmt", NULL);

   return;

}  /* parse_intrinsic_stmt */

/******************************************************************************\
|*                                                                            *|
|* Description:                                                               *|
|*      Parse the optional statement                                          *|
|*      BNF       - OPTIONAL [::] dummy-arg-name-list                         *|
|*                                                                            *|
|* Input parameters:                                                          *|
|*      NONE                                                                  *|
|*                                                                            *|
|* Output parameters:                                                         *|
|*      NONE                                                                  *|
|*                                                                            *|
|* Returns:                                                                   *|
|*      NONE                                                                  *|
|*                                                                            *|
\******************************************************************************/

void parse_optional_stmt(void)

{
   TRACE (Func_Entry, "parse_optional_stmt", NULL);

   parse_attrs(merge_optional);

   TRACE (Func_Exit, "parse_optional_stmt", NULL);

   return;

}  /* parse_optional_stmt */


/******************************************************************************\
|*									      *|
|* Description:								      *|
|*      BNF       - POINTER   or  POINTER(CRI-pointer-name, CRI-pointee-name) *|
|*									      *|
|* Input parameters:							      *|
|*	NONE								      *|
|*									      *|
|* Output parameters:							      *|
|*	NONE								      *|
|*									      *|
|* Returns:								      *|
|*	NONE								      *|
|*									      *|
\******************************************************************************/

void parse_pointer_stmt (void)

{
#ifdef KEY /* Bug 10177 */
   int		array_idx = 0;
#else /* KEY Bug 10177 */
   int		array_idx;
#endif /* KEY Bug 10177 */
   int		attr_idx;
   int		name_idx;
   boolean	parse_err;
   int		pointer_idx;
   token_type	pointee_name;
   token_type	pointer_name;
   boolean	semantic_err;

# if defined(_NO_CRAY_CHARACTER_PTR)
   int		lparen_col;
   int		lparen_line;
# endif



   TRACE (Func_Entry, "parse_pointer_stmt", NULL);

   if (LA_CH_VALUE != LPAREN) {		/* Fortran 90 POINTER */
      parse_attrs(merge_pointer);
      goto EXIT;
   }

   /*  CRI pointer statement */

   if ((STMT_OUT_OF_ORDER(curr_stmt_category, Pointer_Stmt) ||
        STMT_CANT_BE_IN_BLK(Pointer_Stmt, CURR_BLK)) && iss_blk_stk_err()) {
      /* Intentionally blank */
   }
   else {
      curr_stmt_category = Declaration_Stmt_Cat;
      PRINTMSG(stmt_start_line, 134, Ansi, stmt_start_col);
   }

   do {
      parse_err		= FALSE;
      semantic_err	= FALSE;

      if (LA_CH_VALUE == LPAREN) {

# if defined(_NO_CRAY_CHARACTER_PTR)
         lparen_line = LA_CH_LINE;
         lparen_col  = LA_CH_COLUMN; 
# endif

         NEXT_LA_CH;				/* Skip LPAREN */

         if (MATCHED_TOKEN_CLASS(Tok_Class_Id)) {
            pointer_name = token;
   
            if (LA_CH_VALUE == COMMA) {
               NEXT_LA_CH;			/* Skip COMMA */

               if (MATCHED_TOKEN_CLASS(Tok_Class_Id)) {
                  pointee_name	= token;
                  array_idx	= (LA_CH_VALUE == LPAREN) ? 
                                  parse_array_spec(AT_WORK_IDX) : NULL_IDX;

                  if (LA_CH_VALUE != RPAREN) {
                     parse_err_flush(Find_Rparen, ")");
                     parse_err = TRUE;
                  }
               }
               else {
                  parse_err_flush(Find_Rparen, "pointee name");
                  parse_err = TRUE;
               }
            }
            else {
               parse_err_flush(Find_Rparen, ",");
               parse_err = TRUE;
            }
         }
         else {
            parse_err_flush(Find_Rparen, "Cray pointer name");
            parse_err = TRUE;
         }

         if (LA_CH_VALUE == RPAREN) {
            NEXT_LA_CH;			/* Skip RPAREN */
         }

         if (LA_CH_VALUE != COMMA && LA_CH_VALUE != EOS) {
            parse_err_flush(Find_Comma, ", or " EOS_STR);
            parse_err = TRUE;
         }

         if (LA_CH_VALUE == COMMA) {
            NEXT_LA_CH;
         }
      }
      else {
         parse_err_flush(Find_Lparen, "(");
         parse_err = TRUE;
      }
 
      if (parse_err) {
         continue;
      }

      attr_idx = srch_sym_tbl(TOKEN_STR(pointee_name), 
                              TOKEN_LEN(pointee_name), &name_idx);

      if (attr_idx == NULL_IDX) {
         attr_idx			= ntr_sym_tbl(&pointee_name, name_idx);
         LN_DEF_LOC(name_idx)		= TRUE;      /* Can't be host assoc */
         SET_IMPL_TYPE(attr_idx);
         ATD_CLASS(attr_idx)		= CRI__Pointee;
      }
      else if (AT_OBJ_CLASS(attr_idx) == Data_Obj &&
               TYP_TYPE(ATD_TYPE_IDX(attr_idx)) == Character) {

         if (fnd_semantic_err(Obj_Cri_Ch_Pointee,
                              TOKEN_LINE(pointee_name),
                              TOKEN_COLUMN(pointee_name),
                              attr_idx,
                              TRUE)) {

            semantic_err	= TRUE;

            CREATE_ERR_ATTR(attr_idx,
                            TOKEN_LINE(pointee_name),
                            TOKEN_COLUMN(pointee_name),
                            Data_Obj);
            SET_IMPL_TYPE(attr_idx);
         }
         else {
# ifndef _EXTENDED_CRI_CHAR_POINTER
            if (TYP_CHAR_CLASS(ATD_TYPE_IDX(attr_idx)) != Assumed_Size_Char) {
               PRINTMSG(TOKEN_LINE(pointee_name), 1390, Warning,
                        TOKEN_COLUMN(pointee_name),
                        AT_OBJ_NAME_PTR(attr_idx));

               /* change to Assumed_Size_Char */

               CLEAR_TBL_NTRY(type_tbl, TYP_WORK_IDX);
               TYP_TYPE(TYP_WORK_IDX)        = Character;
               TYP_LINEAR(TYP_WORK_IDX)      = Character_1;
               TYP_DESC(TYP_WORK_IDX)        = Default_Typed;
               TYP_DCL_VALUE(TYP_WORK_IDX)   = 0;
               TYP_CHAR_CLASS(TYP_WORK_IDX)  = Assumed_Size_Char;
               ATD_TYPE_IDX(attr_idx)        = ntr_type_tbl();
            }
# endif

            if (AT_REFERENCED(attr_idx) == Char_Rslt_Bound_Ref) {
               AT_ATTR_LINK(attr_idx)	= NULL_IDX;
               LN_DEF_LOC(name_idx)	= TRUE;
            }
         }

# if defined(_NO_CRAY_CHARACTER_PTR)
         PRINTMSG(lparen_line, 541, Error, lparen_col);
# endif

      }
      else if (fnd_semantic_err(Obj_Cri_Pointee,
                                TOKEN_LINE(pointee_name),
                                TOKEN_COLUMN(pointee_name),
                                attr_idx,
                                TRUE)) {
         CREATE_ERR_ATTR(attr_idx,
                         TOKEN_LINE(pointee_name),
                         TOKEN_COLUMN(pointee_name),
                         Data_Obj);
         SET_IMPL_TYPE(attr_idx);
         semantic_err	= TRUE;
      }

# if !defined(_POINTEES_CAN_BE_STRUCT)
      else if (TYP_TYPE(ATD_TYPE_IDX(attr_idx)) == Structure) {
         PRINTMSG (TOKEN_LINE(pointee_name), 651, Error,
                   TOKEN_COLUMN(pointee_name), 
                   AT_OBJ_NAME_PTR(attr_idx));
         CREATE_ERR_ATTR(attr_idx,
                         TOKEN_LINE(pointee_name),
                         TOKEN_COLUMN(pointee_name),
                         Data_Obj);
         SET_IMPL_TYPE(attr_idx);
         semantic_err	= TRUE;
      }
# endif
      else if (AT_REFERENCED(attr_idx) == Char_Rslt_Bound_Ref) {
         AT_ATTR_LINK(attr_idx)	= NULL_IDX;
         LN_DEF_LOC(name_idx)	= TRUE;
      }

      if (AT_OBJ_CLASS(attr_idx) == Data_Obj) {
         ATD_SEEN_OUTSIDE_IMP_DO(attr_idx) = TRUE;
      }


      ATD_CLASS(attr_idx)   = CRI__Pointee;

      if ((cif_flags & XREF_RECS) != 0) {
         cif_usage_rec(attr_idx,
                       AT_Tbl_Idx,
                       TOKEN_LINE(pointee_name),
                       TOKEN_COLUMN(pointee_name),
                       CIF_Symbol_Declaration);
      }

      if (array_idx != NULL_IDX) {
         merge_dimension(attr_idx,
                         TOKEN_LINE(pointee_name),
                         TOKEN_COLUMN(pointee_name),
                         array_idx);
      }

      pointer_idx = srch_sym_tbl(TOKEN_STR(pointer_name), 
                                 TOKEN_LEN(pointer_name), &name_idx);

      if (pointer_idx == NULL_IDX) {
         pointer_idx			= ntr_sym_tbl(&pointer_name, name_idx);
         LN_DEF_LOC(name_idx)		= TRUE;      /* Can't be host assoc */
      }
      else if (fnd_semantic_err(Obj_Cri_Ptr, 
                                TOKEN_LINE(pointer_name),
                                TOKEN_COLUMN(pointer_name),
                                pointer_idx,
                                TRUE)) {
         semantic_err = TRUE;
         CREATE_ERR_ATTR(pointer_idx,
                         TOKEN_LINE(pointer_name),
                         TOKEN_COLUMN(pointer_name),
                         Data_Obj);
      }
      else if (AT_REFERENCED(pointer_idx) == Char_Rslt_Bound_Ref) {
         AT_ATTR_LINK(pointer_idx)	= NULL_IDX;
         LN_DEF_LOC(name_idx)		= TRUE;
      }

      if (AT_OBJ_CLASS(pointer_idx) == Data_Obj) {
         ATD_SEEN_OUTSIDE_IMP_DO(pointer_idx) = TRUE;
      }

      if ((cif_flags & XREF_RECS) != 0) {
         cif_usage_rec(pointer_idx,
                       AT_Tbl_Idx,
                       TOKEN_LINE(pointer_name),
                       TOKEN_COLUMN(pointer_name),
                       CIF_Symbol_Declaration);
      }

      if (TYP_TYPE(ATD_TYPE_IDX(attr_idx)) != Character) {
         ATD_TYPE_IDX(pointer_idx)	= CRI_Ptr_8;
      }
      else {
         ATD_TYPE_IDX(pointer_idx)	= CRI_Ch_Ptr_8;
      }

      AT_TYPED(pointer_idx)	= TRUE;
      ATD_PTR_IDX(attr_idx)	= pointer_idx;

      if (semantic_err) {
         AT_DCL_ERR(pointer_idx)= TRUE;
         AT_DCL_ERR(attr_idx)	= TRUE;
      }
   }  /* End while */
   while (LA_CH_VALUE != EOS); 

   NEXT_LA_CH;				/* Skip EOS */

EXIT:

   TRACE (Func_Exit, "parse_pointer_stmt", NULL);

   return;

}  /* parse_pointer_stmt */


/******************************************************************************\
|*                                                                            *|
|* Description:								      *|
|*      BNF       - SAVE [::] object-name OR /common-block-name/              *|
|*                                                                            *|
|* Input parameters:							      *|
|*	NONE								      *|
|*									      *|
|* Output parameters:							      *|
|*	NONE								      *|
|*									      *|
|* Returns:								      *|
|*	NONE								      *|
|*									      *|
\******************************************************************************/

void parse_save_stmt (void)

{
   TRACE (Func_Entry, "parse_save_stmt", NULL);

   if (LA_CH_VALUE == EOS) {

      if ((STMT_CANT_BE_IN_BLK(Save_Stmt, CURR_BLK) ||
           STMT_OUT_OF_ORDER(curr_stmt_category, Save_Stmt)) && 
          iss_blk_stk_err()) {
         /* Block stack error - intentionally blank */
      }
      else {
         if (ATP_SAVE_ALL(SCP_ATTR_IDX(curr_scp_idx))) {
            PRINTMSG(TOKEN_LINE(token), 133, Ansi, TOKEN_COLUMN(token));
         }
         ATP_SAVE_ALL(SCP_ATTR_IDX(curr_scp_idx))	= TRUE;
         curr_stmt_category				= Declaration_Stmt_Cat;

         if (ATP_STACK_DIR(SCP_ATTR_IDX(curr_scp_idx))) {

            /* A SAVE with no save entity list has been specified in this */
            /* program unit.  SAVE overrides STACK.  Issue warning.       */

            PRINTMSG(TOKEN_LINE(token), 1144, Warning,
                     TOKEN_COLUMN(token),
                     "STACK");
            ATP_STACK_DIR(SCP_ATTR_IDX(curr_scp_idx))	= FALSE;
         }
      }
      NEXT_LA_CH;			/* Pick up EOS */
   }
   else {

      if (ATP_SAVE_ALL(SCP_ATTR_IDX(curr_scp_idx))) {
         PRINTMSG (stmt_start_line, 133, Ansi, stmt_start_col);
      }

      parse_attrs(merge_save);
   }

   TRACE (Func_Exit, "parse_save_stmt", NULL);

   return;

}  /* parse_save_stmt */


/******************************************************************************\
|*									      *|
|* Description:								      *|
|*      BNF       - TARGET [::] object-name [(array-spec)]                    *|
|*                                 [,object-name [(array_spec)]..             *|
|*									      *|
|* Input parameters:							      *|
|*	NONE								      *|
|*									      *|
|* Output parameters:							      *|
|*	NONE								      *|
|*									      *|
|* Returns:								      *|
|*	NONE								      *|
|*									      *|
\******************************************************************************/

void parse_target_stmt (void)

{
   TRACE (Func_Entry, "parse_target_stmt", NULL);

   parse_attrs(merge_target);

   TRACE (Func_Exit, "parse_target_stmt", NULL);

   return;

}  /* parse_target_stmt */

/******************************************************************************\
|*                                                                            *|
|* Description:								      *|
|*      BNF       - VOLATILE [::] object-name OR /common-block-name/          *|
|*                                                                            *|
|* Input parameters:							      *|
|*	NONE								      *|
|*									      *|
|* Output parameters:							      *|
|*	NONE								      *|
|*									      *|
|* Returns:								      *|
|*	NONE								      *|
|*									      *|
\******************************************************************************/

void parse_volatile_stmt (void)

{
   TRACE (Func_Entry, "parse_volatile_stmt", NULL);

#ifdef KEY /* Bug 14110 */
   PRINTMSG(stmt_start_line, 1685, Ansi, stmt_start_col, "VOLATILE");
#else /* KEY Bug 14110 */
   PRINTMSG(stmt_start_line, 1253, Ansi, stmt_start_col, "VOLATILE");
#endif /* KEY Bug 14110 */
   
   parse_attrs(merge_volatile);

   TRACE (Func_Exit, "parse_volatile_stmt", NULL);

   return;

}  /* parse_volatile_stmt */
#ifdef KEY /* Bug 14150 */
/*
 *	BNF is
 *		language-binding-spec [ :: ] bind-entity-list
 *	where bind-entity-list contains
 *		entity-name
 *      or
 *		/ common-block-name /
 */
void parse_bind_stmt (void) {
   TRACE (Func_Entry, "parse_bind_stmt", NULL);
   parse_language_binding_spec(&new_binding_label);

   if (1 < parse_attrs(merge_bind) && BIND_SPECIFIES_NAME(new_binding_label)) {
     PRINTMSG(stmt_start_line, 1689, Error, stmt_start_col);
   }

   TRACE (Func_Exit, "parse_bind_stmt", NULL);

   return;

}  /* parse_bind_stmt */

void parse_value_stmt (void)

{
   TRACE (Func_Entry, "parse_value_stmt", NULL);

   PRINTMSG(stmt_start_line, 1685, Ansi, stmt_start_col, "VALUE");
   
   parse_attrs(merge_value);

   TRACE (Func_Exit, "parse_value_stmt", NULL);

   return;

}  /* parse_value_stmt */
#endif /* KEY Bug 14150 */
