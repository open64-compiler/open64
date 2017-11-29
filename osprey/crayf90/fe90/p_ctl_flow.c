/*
 *  Copyright (C) 2006. QLogic Corporation. All Rights Reserved.
 */

/*
 * Copyright 2004, 2005, 2006 PathScale, Inc.  All Rights Reserved.
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



static char USMID[] = "\n@(#)5.0_pl/sources/p_ctl_flow.c	5.11	10/12/99 10:54:10\n";

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
|* Function prototypes of static functions declared in this file *|
\*****************************************************************/

static   boolean  change_subscript (opnd_type *);
static   boolean  loop_top_is_perfectly_nested (char *);
static	 int      match_blk (blk_cntxt_type, boolean);
static	 boolean  parse_label_list (int);
static	 void  	  process_blockable_dir (void);
static	 void  	  process_interchange_dir (void);
static	 void  	  check_mp_dir_nesting (void);
static   void	  gen_if_ir(int, int, int, int);


/****************************************************\
|* Static variables declared and used in this file. *|
\****************************************************/

/* The following entities are used in DO loop processing. */

static	int	last_unlbld_loop_line = 0;
static	int	last_unlbld_loop_num;

boolean	star_expected = FALSE;


/******************************************************************************\
|*									      *|
|* Description:								      *|
|*	BNF        - ALLOCATE ( allocation-list [, STAT = stat-variable] )    *|
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

void parse_allocate_stmt (void)

{
   boolean      had_stat        = FALSE;
   int          ir_idx;
   int          list1_idx;
   int          list2_idx = 0;
   opnd_type    opnd;
   boolean      parsed_ok = TRUE;
   token_type   stat_token;
#ifdef KEY /* Bug 4897 */
   boolean	first_trip = TRUE;
#endif /* KEY Bug 4897 */


   TRACE (Func_Entry, "parse_allocate_stmt", NULL);

   if (LA_CH_VALUE != LPAREN) {
      parse_err_flush(Find_EOS, "(");
      goto EXIT;
   }

   if (!CURR_BLK_NO_EXEC || !iss_blk_stk_err()) {
      curr_stmt_category = Executable_Stmt_Cat;
   }

   NTR_IR_TBL(ir_idx);
   SH_STMT_TYPE(curr_stmt_sh_idx) = Allocate_Stmt;
   SH_IR_IDX(curr_stmt_sh_idx)    = ir_idx;
   
   IR_OPR(ir_idx)                 = Allocate_Opr;
   IR_TYPE_IDX(ir_idx)            = TYPELESS_DEFAULT_TYPE;
   IR_COL_NUM(ir_idx)             = TOKEN_COLUMN(token);
   IR_LINE_NUM(ir_idx)            = TOKEN_LINE(token);
   IR_FLD_L(ir_idx)               = IL_Tbl_Idx;
   IR_LIST_CNT_L(ir_idx)          = 0;
   
   do {
      NEXT_LA_CH;

      if (MATCHED_TOKEN_CLASS(Tok_Class_Id)) {

#ifdef KEY /* Bug 4897 */
	/* On first trip through this loop, "stat=" is not allowed */
        if (first_trip == TRUE) {
	  first_trip = FALSE;
	  }
	else {
#endif /* KEY Bug 4897 */
         if (strcmp(TOKEN_STR(token),"STAT") == 0) {
            stat_token = token;

            if (LA_CH_VALUE == EQUAL) {
               NEXT_LA_CH;
               had_stat = TRUE;

               if (MATCHED_TOKEN_CLASS(Tok_Class_Id)) {
                  /* have stat var */
                  /* do that stat stuff */

                  parsed_ok = parse_deref(&opnd, NULL_IDX) && parsed_ok; 
                  COPY_OPND(IR_OPND_R(ir_idx), opnd);

                  mark_attr_defined(&opnd);
                  continue;
               } 
               else {

                  parse_err_flush(Find_Comma_Rparen,
                                  "scalar integer STAT variable");
                  continue;
               }
            } /* if (matched_specific_token(Tok_Punct_Eq, Tok_Class_Punct)) */
         } /* if (strcmp(TOKEN_STR(token),"STAT")) */
#ifdef KEY /* Bug 4897 */
        }
#endif /* KEY Bug 4897 */

         NTR_IR_LIST_TBL(list1_idx);

         if (list2_idx) {
            IL_NEXT_LIST_IDX(list2_idx) = list1_idx;
            IL_PREV_LIST_IDX(list1_idx) = list2_idx;
         }
         else {
            IR_IDX_L(ir_idx) = list1_idx;
         }
         list2_idx = list1_idx;

         (IR_LIST_CNT_L(ir_idx))++;
         
         /* have token */
         if (had_stat) {
            PRINTMSG(TOKEN_LINE(stat_token),203, Error,
                     TOKEN_COLUMN(stat_token), NULL);
         }

         star_expected = TRUE;
         if (parse_deref(&opnd, NULL_IDX)) {
            parsed_ok = change_subscript(&opnd) && parsed_ok;
         }
         else {
            parsed_ok = FALSE;
         }
         star_expected = FALSE;

         COPY_OPND(IL_OPND(list1_idx), opnd);

         if (LA_CH_VALUE != COMMA && LA_CH_VALUE != RPAREN) {

            parse_err_flush(Find_Comma_Rparen,", or )");
         }

         if (LA_CH_VALUE == EOS) {
            goto EXIT;
         }

      } else {
         parse_err_flush(Find_EOS, "allocation object or STAT = ");
         goto EXIT;
      }
   }
   while (LA_CH_VALUE == COMMA);

   if (LA_CH_VALUE != RPAREN) {
      parse_err_flush(Find_EOS, ")");
   }
   else {
      NEXT_LA_CH;
   }

EXIT:
      
   if (LA_CH_VALUE != EOS) {
      parse_err_flush(Find_EOS, EOS_STR);
   } 

   matched_specific_token(Tok_EOS, Tok_Class_Punct);

   TRACE (Func_Exit, "parse_allocate_stmt", NULL);

   return;

}  /* parse_allocate_stmt */


/******************************************************************************\
|*                                                                            *|
|* Description:                                                               *|
|*      This procedure parses the ASSIGN statement:			      *|
|*      								      *|
|*           ASSIGN label TO scalar-int-variable 			      *|
|*                                                                            *|
|* Input parameters:                                                          *|
|*      NONE                                                                  *|
|*                                                                            *|
|* Output parameters:                                                         *|
|*      NONE								      *|
|*                                                                            *|
|* Global data changed:							      *|
|*      curr_stmt_category						      *|
|*									      *|
|* Returns:                                                                   *|
|*      NONE								      *|
|*                                                                            *|
\******************************************************************************/

void    parse_assign_stmt(void)

{
   opnd_type 		asg_var_opnd;
   token_type	        asg_var_token;
   int                  attr_idx;
   int 	                name_idx;
   int 			ir_idx;


   TRACE (Func_Entry, "parse_assign_stmt", NULL);

   if ( ! CURR_BLK_NO_EXEC  ||  ! iss_blk_stk_err() ) {
      curr_stmt_category = Executable_Stmt_Cat;
   }

   /* ASSIGN statement is obsolescent.			                      */

   PRINTMSG(stmt_start_line, 1568, Ansi, stmt_start_col, "ASSIGN");

   NTR_IR_TBL(ir_idx);
   SH_IR_IDX(curr_stmt_sh_idx) = ir_idx;
   IR_OPR(ir_idx)              = Asg_Opr;
   IR_TYPE_IDX(ir_idx)         = TYPELESS_DEFAULT_TYPE;
   IR_LINE_NUM(ir_idx)         = TOKEN_LINE(token);
   IR_COL_NUM(ir_idx)          = TOKEN_COLUMN(token);

   /* Have the keyword ASSIGN.  The next token must be a label.               */

   if (MATCHED_TOKEN_CLASS(Tok_Class_Label)) {

      if (TOKEN_ERR(token)) {
         parse_err_flush(Find_EOS, NULL);
         goto EXIT;
      }

      attr_idx = srch_sym_tbl(TOKEN_STR(token), TOKEN_LEN(token), &name_idx); 
  
      if (attr_idx == NULL_IDX) {
         attr_idx                = ntr_sym_tbl(&token, name_idx);
         AT_OBJ_CLASS(attr_idx)  = Label;
         AT_REFERENCED(attr_idx) = Referenced;
         LN_DEF_LOC(name_idx)    = TRUE;
         build_fwd_ref_entry(attr_idx, Assign_Ref);
      }
      else if (! AT_DCL_ERR(attr_idx) ) {

         if (AT_DEFINED(attr_idx) ) {
                 
            /* If the ASSIGN stmt is:  "10   ASSIGN 10 TO var", need to set   */
	    /* ATL_EXECUTABLE now so label_ref_semantics will work correctly. */

            if (stmt_label_idx != NULL_IDX  &&  
                ATL_DEF_STMT_IDX(attr_idx) == curr_stmt_sh_idx) {
               ATL_EXECUTABLE(attr_idx) = TRUE;
            }

            label_ref_semantics(attr_idx, Assign_Ref, NULL_IDX,
                                TOKEN_LINE(token), TOKEN_COLUMN(token));
         }
         else {
            build_fwd_ref_entry(attr_idx, Assign_Ref);
         }
      }
      else {
         SH_ERR_FLG(curr_stmt_sh_idx) = TRUE; 
      }

      ATL_IN_ASSIGN(attr_idx) = TRUE;
      AT_REFERENCED(attr_idx) = Referenced;

      IR_LINE_NUM_L(ir_idx) = TOKEN_LINE(token);
      IR_COL_NUM_L(ir_idx)  = TOKEN_COLUMN(token);
      IR_FLD_L(ir_idx)      = AT_Tbl_Idx;
      IR_IDX_L(ir_idx)      = attr_idx;

      if (cif_flags & XREF_RECS) {
         cif_usage_rec(attr_idx, AT_Tbl_Idx,
                       IR_LINE_NUM_L(ir_idx), IR_COL_NUM_L(ir_idx),
                       CIF_Label_Referenced_In_ASSIGN);
      }

      if (matched_specific_token(Tok_Kwd_To, Tok_Class_Keyword)) {

         if (MATCHED_TOKEN_CLASS(Tok_Class_Id)) {
            asg_var_token = token;
        
            if (parse_deref(&asg_var_opnd, NULL_IDX)) {
   
               if (OPND_FLD(asg_var_opnd) == AT_Tbl_Idx) {
                  COPY_OPND(IR_OPND_R(ir_idx), asg_var_opnd);
                
                  if (AT_OBJ_CLASS(OPND_IDX(asg_var_opnd)) == Data_Obj) {
                     ATD_IN_ASSIGN(OPND_IDX(asg_var_opnd)) = TRUE;
                  }

                  mark_attr_defined(&asg_var_opnd);

                  if (LA_CH_VALUE != EOS) {
                     parse_err_flush(Find_EOS, EOS_STR);
                  }
               }
               else {
                  PRINTMSG(TOKEN_LINE(asg_var_token), 172, Error, 
                           TOKEN_COLUMN(asg_var_token));
                  parse_err_flush(Find_EOS, NULL);
                  SH_ERR_FLG(curr_stmt_sh_idx) = TRUE;  
               }
            }
            else {     /* Id found but error later in the reference.         */
               parse_err_flush(Find_EOS, NULL);
            }
        }
        else {     /* No Id found at all.                                    */
           parse_err_flush(Find_EOS, "scalar-int-variable");
        }
      }
      else {
         parse_err_flush(Find_EOS, "TO");
      }
   }
   else {
      parse_err_flush(Find_EOS, "label");
   }

EXIT:

   matched_specific_token(Tok_EOS, Tok_Class_Punct);

   TRACE (Func_Exit, "parse_assign_stmt", NULL);

   return;

}  /* parse_assign_stmt */
#ifdef KEY /* Bug 3018 */

/******************************************************************************\
|*									      *|
|* Description:								      *|
|*      This handles the G77 intrinsics which have both a function form and a *|
|*      subroutine form. When we find ourselves about to call as a subroutine *|
|*      an intrinsic which appears in the intrin_tbl as a function, we call   *|
|*      this function "switch_to_subroutine" to search                        *|
|*      the host symbol table for the same name with INTRIN_SUBR_SUFFIX       *|
|*      appended. If we find such a name, we use that instead: thus a         *|
|*      function call to "etime" uses the ETIME entry, but a subroutine call  *|
|*      to "etime" uses the ETIME:Subroutine entry.                           *|
|*      If we don't find a subroutine version of the intrinsic, we return     *|
|*      NULL and let the compiler proceed with its (dubious) original         *|
|*      behavior: if you didn't explicitly declare it to be an intrinsic,     *|
|*      you get an external subroutine call rather than an error message      *|
|*      telling you that the intrinsic is only available as a function.       *|
|*      Might be nice to fix that someday.                                    *|
|*									      *|
|* Input parameters:							      *|
|*	Intrinsic name (in global variable "token")                           *|
|*									      *|
|* Output parameters:							      *|
|*	Host name index for name with suffix appended			      *|
|*	Local name index for name with suffix appended			      *|
|*									      *|
|* Returns:								      *|
|*	Host attribute index for subroutine version of the intrinsic, or      *|
|*      NULL_IDX if there isn't one                                               *|
|*									      *|
\******************************************************************************/
int switch_to_subroutine(int *host_name_idx, int *name_idx)
{
  /* srch_host_sym_tbl appears to take an ordinary char*, but actually it
   * assumes the char* lies inside a token in which the trailing chars are
   * zeroed so that it can compare words rather than bytes. */
  token_type subr_name = token;
  strcat(TOKEN_STR(subr_name), INTRIN_SUBR_SUFFIX);
  TOKEN_LEN(subr_name) += ((sizeof INTRIN_SUBR_SUFFIX) - 1);
  int host_attr_idx = srch_host_sym_tbl(
    TOKEN_STR(subr_name), TOKEN_LEN(subr_name), host_name_idx, TRUE);
  if (host_attr_idx != NULL_IDX) {
    token = subr_name;
    srch_sym_tbl(TOKEN_STR(token), TOKEN_LEN(token), name_idx);
  }
  return host_attr_idx;
}
#endif /* KEY Bug 3018 */

/******************************************************************************\
|*									      *|
|* Description:								      *|
|*	BRIEF DESCRIPTION OF THIS FUNCTION'S PURPOSE			      *|
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

void parse_call_stmt (void)

{
   boolean   amb_ref = FALSE;
   int       attr_idx;
   int       call_idx;
   int       col;
   int       host_attr_idx;
   int       host_name_idx;
   int	     line;
   int       name_idx;
   opnd_type opnd;
   boolean   parsed_ok = TRUE;


   TRACE (Func_Entry, "parse_call_stmt", NULL);

   if (!CURR_BLK_NO_EXEC || !iss_blk_stk_err()) {
      curr_stmt_category = Executable_Stmt_Cat;
   }

   if (cmd_line_flags.debug_lvl == Debug_Lvl_1) {  /* -ez  -G1 */
      gen_debug_lbl_stmt(curr_stmt_sh_idx, Ldbg_Stmt_Lbl, NULL_IDX);
   }

   NTR_IR_TBL(call_idx);
   SH_IR_IDX(curr_stmt_sh_idx) = call_idx;

   IR_OPR(call_idx)            = Call_Opr;
   IR_TYPE_IDX(call_idx)       = TYPELESS_DEFAULT_TYPE;
   IR_COL_NUM(call_idx)        = TOKEN_COLUMN(token);
   IR_LINE_NUM(call_idx)       = TOKEN_LINE(token);

   if (MATCHED_TOKEN_CLASS(Tok_Class_Id)) {
      line = TOKEN_LINE(token);
      col = TOKEN_COLUMN(token);

      attr_idx = srch_sym_tbl(TOKEN_STR(token), 
                              TOKEN_LEN(token),
                              &name_idx);
#ifdef KEY
      if (AT_OBJ_CLASS(attr_idx) == Pgm_Unit &&
          strncasecmp(TOKEN_STR(token), "omp_",4) == 0){
         host_attr_idx = srch_host_sym_tbl(TOKEN_STR(token),
                                           TOKEN_LEN(token), 
                                           &host_name_idx,
                                           TRUE);
         if (host_attr_idx)
           attr_idx = NULL_IDX;
      }
#endif

#ifdef KEY /* Bug 3018 */
      /* If we picked up an intrinsic function due to an "intrinsic"
       * declaration, ignore that in case it's a G77 intrinsic which comes in
       * both function and subroutine forms. Setting "attr_idx" to null forces
       * us to follow the logic for the ambiguous case (that is, the case with
       * no explicit declaration) within which we attempt to switch to the
       * subroutine form. If we can't, we'll issue an error.
       */
      int explicit_intrinsic_decl = FALSE;
      if (AT_IS_INTRIN(attr_idx) &&
	  AT_OBJ_CLASS(attr_idx) == Interface &&
	  ATI_INTERFACE_CLASS(attr_idx) == Generic_Function_Interface) {
	 explicit_intrinsic_decl = TRUE;
         attr_idx = NULL_IDX;
      }
#endif /* KEY Bug 3018 */
      if (attr_idx != NULL_IDX) {
         host_attr_idx = attr_idx;

         if (! LN_DEF_LOC(name_idx)) {
            amb_ref = TRUE;

            while (AT_ATTR_LINK(host_attr_idx) != NULL_IDX) {
               host_attr_idx = AT_ATTR_LINK(host_attr_idx);
            }
         }
      }
      else { /* any other reference is ambiguous */
         amb_ref = TRUE;

         /* search host sym tab */

         host_attr_idx = srch_host_sym_tbl(TOKEN_STR(token),
                                           TOKEN_LEN(token), 
                                           &host_name_idx,
                                           TRUE);

         /* if we are copying info down from the host scope */

         if (host_attr_idx != NULL_IDX) { 

#ifdef KEY /* Bug 9872 */
            /* The parentheses are optional in a call statement when the
	     * actual argument list is empty, so I don't understand why
	     * this code ignores the intrinsic interface when the parens
	     * are omitted. It causes a bug whenever all the arguments to
	     * an intrinsic are omitted, because a subsequent call with
	     * keyword arguments can't find the keywords. Perhaps this was
	     * discovered for "random_seed", but it applies equally to
	     * "date_and_time", and maybe to others. If the purpose of the code
	     * ever becomes evident and we have to restore it, we will need
	     * to add "date_and_time" to the set of "strcmp"ed exceptions. */
#else /* KEY Bug 9872 */
            if (LA_CH_VALUE != LPAREN &&
                AT_IS_INTRIN(host_attr_idx) &&
                AT_OBJ_CLASS(host_attr_idx) == Interface &&
                (strcmp(AT_OBJ_NAME_PTR(host_attr_idx), "SYNCHRONIZE") != 0) &&
                (strcmp(AT_OBJ_NAME_PTR(host_attr_idx), "RANDOM_SEED") != 0)) {
               host_attr_idx = NULL_IDX;
            }
#endif /* KEY Bug 9872 */

            /* We don't want to copy down the host attr if the host */
            /* attr is a FUNCTION attr.  We are dealing with a CALL */
            /* at this point.                                       */

            if (AT_IS_INTRIN(host_attr_idx) &&
                AT_OBJ_CLASS(host_attr_idx) == Interface &&
                ATI_INTERFACE_CLASS(host_attr_idx) == 
                                                   Generic_Function_Interface) {
#ifdef KEY /* Bug 3018 */
	      int switch_attr_idx = switch_to_subroutine(&host_name_idx,
	        &name_idx);
	      if (NULL_IDX == switch_attr_idx) {
		/* If we saw an explicit "intrinsic" declaration, then
		 * the act of calling a function as a subroutine is an
		 * error. Otherwise (see bug 4367) it's a warning, and
		 * we treat it if we had seen an "external" declaration.
		 */
		if (explicit_intrinsic_decl) {
		  if (fnd_semantic_err(Obj_Use_Extern_Subr, 
					    line,
					    col,
					    host_attr_idx,
					    TRUE)) {
		     parse_err_flush(Find_EOS, NULL);
		     parsed_ok = FALSE;
		     goto EXIT;
		  }
		}
		else {
		  PRINTMSG(line, 1675, Warning, col,
		    AT_OBJ_NAME_PTR(host_attr_idx));
		  host_attr_idx = NULL_IDX;
		}
	      }
	      else {
		host_attr_idx = switch_attr_idx;
	      }
#else
	      host_attr_idx = NULL_IDX;
#endif /* KEY Bug 3018 */
            }
         }

         if (host_attr_idx != NULL_IDX) { /* copy the attr into the local scp */
            attr_idx = ntr_host_in_sym_tbl(&token, 
                                           name_idx,
                                           host_attr_idx, 
                                           host_name_idx, 
                                           TRUE);

            AT_ATTR_LINK(attr_idx) = host_attr_idx;

            while (AT_ATTR_LINK(host_attr_idx) != NULL_IDX) {
               host_attr_idx = AT_ATTR_LINK(host_attr_idx);
            }

            if (AT_IS_INTRIN(host_attr_idx) &&
                AT_OBJ_CLASS(host_attr_idx) == Interface) {

               if (ATI_FIRST_SPECIFIC_IDX(host_attr_idx) == NULL_IDX) {
                  complete_intrinsic_definition(host_attr_idx);
               }

               /* copy intrinsic attr to the local scope from the 0th scope */
               /* and break the link to the host scope.                     */
               COPY_ATTR_NTRY(attr_idx, host_attr_idx);
               AT_CIF_SYMBOL_ID(attr_idx)	= 0;
               AT_ATTR_LINK(attr_idx)		= NULL_IDX;
               host_attr_idx			= NULL_IDX;
               AT_DEF_LINE(attr_idx)		= TOKEN_LINE(token);
               AT_DEF_COLUMN(attr_idx)		= TOKEN_COLUMN(token);
               attr_idx				= srch_sym_tbl(TOKEN_STR(token),
                                                               TOKEN_LEN(token),
                                                               &name_idx);
            }
         }
      }

      if (attr_idx == NULL_IDX) {
         attr_idx			= ntr_sym_tbl(&token, name_idx);
         AT_OBJ_CLASS(attr_idx)		= Pgm_Unit;
         ATP_PGM_UNIT(attr_idx)		= Subroutine;
         ATP_SCP_IDX(attr_idx)		= curr_scp_idx;
         MAKE_EXTERNAL_NAME(attr_idx, 
                            AT_NAME_IDX(attr_idx),
                            AT_NAME_LEN(attr_idx));
         ATP_PROC(attr_idx)		= Unknown_Proc;
      }
      else if (!amb_ref) {

         if (AT_OBJ_CLASS(attr_idx) == Pgm_Unit &&
             !AT_NOT_VISIBLE(attr_idx) &&
             (ATP_PGM_UNIT(attr_idx) == Subroutine ||
              ATP_PGM_UNIT(attr_idx) == Pgm_Unknown)) {
             ATP_PGM_UNIT(attr_idx) = Subroutine;
         }
         else if (fnd_semantic_err(Obj_Use_Extern_Subr, 
                                   line,
                                   col,
                                   attr_idx,
                                   TRUE)) {
            parse_err_flush(Find_EOS, NULL);
            parsed_ok = FALSE;
            goto EXIT;
         }
         else if (AT_OBJ_CLASS(attr_idx) == Data_Obj) {
            chg_data_obj_to_pgm_unit(attr_idx, Subroutine, Extern_Proc);
         }
      }
      else {
         if (AT_OBJ_CLASS(attr_idx) == Interface) {
            /* do nothing if it is an interface */
         }
         else if (AT_REFERENCED(attr_idx) == Not_Referenced &&
                  AT_OBJ_CLASS(attr_idx) == Data_Obj) {

            /* assumes that if not referenced then we just put it in */

            chg_data_obj_to_pgm_unit(attr_idx, Subroutine, Extern_Proc);
         }
      }

      AT_REFERENCED(attr_idx) = Referenced;
      IR_FLD_L(call_idx)      = AT_Tbl_Idx;
      IR_IDX_L(call_idx)      = attr_idx;
      IR_LINE_NUM_L(call_idx) = line;
      IR_COL_NUM_L(call_idx)  = col;

      if (LA_CH_VALUE == LPAREN) {
         parsed_ok = parse_actual_arg_spec(&opnd, FALSE, attr_idx) && 
                     parsed_ok;
         COPY_OPND(IR_OPND_R(call_idx), opnd);
      }
      else {

         if (LA_CH_VALUE != EOS) {
            parse_err_flush(Find_EOS, "( or "EOS_STR);
            parsed_ok = FALSE;
         }
         else {
            IR_FLD_R(call_idx)      = IL_Tbl_Idx;
            IR_IDX_R(call_idx)      = NULL_IDX;
            IR_LIST_CNT_R(call_idx) = 0;
         }
      }
   }
   else {
      parse_err_flush(Find_EOS, "SUBROUTINE NAME");
      parsed_ok = FALSE;
   }

EXIT:
   
   if (LA_CH_VALUE != EOS) {
      parse_err_flush(Find_EOS, EOS_STR);
      parsed_ok = FALSE;
   }

   matched_specific_token(Tok_EOS, Tok_Class_Punct);

   SH_ERR_FLG(curr_stmt_sh_idx) = ! parsed_ok;

   TRACE (Func_Exit, "parse_call_stmt", NULL);

   return;

}  /* parse_call_stmt */


/******************************************************************************\
|*									      *|
|* Description:								      *|
|*	Parse a CASE statement of a SELECT CASE construct.		      *|
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

void parse_case_stmt (void)

{
   int		blk_idx;
   int		case_ir_idx;
   int		case_lbl_idx;
   int		cont_stmt_sh_idx;
#ifdef KEY /* Bug 10177 */
   int		expr_start_col = 0;
   int		expr_start_line = 0;
   boolean	fnd_colon = 0;
#else /* KEY Bug 10177 */
   int		expr_start_col;
   int		expr_start_line;
   boolean	fnd_colon;
#endif /* KEY Bug 10177 */
   boolean	fnd_default		= FALSE;
   boolean	fnd_name		= FALSE;
   int		ir_idx;
   boolean	matched_blk		= FALSE;
   int		num_cases		= 0;
   opnd_type	opnd;
   int		range_ir_idx;
   int		save_curr_stmt_sh_idx;


   TRACE (Func_Entry, "parse_case_stmt", NULL);

   check_for_vestigial_task_blks();

   /* PDGCS wants a label to precede each CASE in a CASE construct.  If a     */
   /* case-selector contains more than one case-value-range, only one label   */
   /* is generated to precede the first CASE SH.  Insert a CONTINUE stmt      */
   /* ahead of the CASE SH to define the label needed by PDGCS.  If the CASE  */
   /* stmt is labeled, insert the CONTINUE ahead of the user label.	      */

   save_curr_stmt_sh_idx = curr_stmt_sh_idx;

   if (SH_LABELED(curr_stmt_sh_idx)) {
      curr_stmt_sh_idx = SH_PREV_IDX(curr_stmt_sh_idx);
   }

   /* Back up before the debug label, if one has been generated in p_driver. */

   if (cmd_line_flags.debug_lvl == Debug_Lvl_0 &&
       SH_IR_IDX(curr_stmt_sh_idx) != NULL_IDX &&
       IR_OPR(SH_IR_IDX(curr_stmt_sh_idx)) == Label_Opr) {  /* -ed  -G0 */

      curr_stmt_sh_idx = SH_PREV_IDX(curr_stmt_sh_idx);
   }

   gen_sh(Before, Continue_Stmt, stmt_start_line, stmt_start_col,
          FALSE, TRUE, TRUE);

   cont_stmt_sh_idx = SH_PREV_IDX(curr_stmt_sh_idx);

   case_lbl_idx = gen_internal_lbl(stmt_start_line);

   NTR_IR_TBL(ir_idx);
   SH_IR_IDX(cont_stmt_sh_idx) = ir_idx;
   IR_OPR(ir_idx)              = Label_Opr;
   /* LRR - bhj put in short typeless as type idx */
   IR_TYPE_IDX(ir_idx)         = TYPELESS_DEFAULT_TYPE;
   IR_LINE_NUM(ir_idx)         = stmt_start_line;
   IR_COL_NUM(ir_idx)          = stmt_start_col;
   IR_LINE_NUM_L(ir_idx)       = stmt_start_line;
   IR_COL_NUM_L(ir_idx)        = stmt_start_col;
   IR_FLD_L(ir_idx)            = AT_Tbl_Idx;
   IR_IDX_L(ir_idx)            = case_lbl_idx;
     
   AT_REFERENCED(case_lbl_idx)	  = Referenced;
   AT_DEFINED(case_lbl_idx)	  = TRUE;
   AT_DEF_LINE(case_lbl_idx)	  = stmt_start_line;
   ATL_DEF_STMT_IDX(case_lbl_idx) = cont_stmt_sh_idx;

   curr_stmt_sh_idx = save_curr_stmt_sh_idx;

   /* Generate the Case IR.  By default, set the right operand to point at    */
   /* the constant 0 to indicate that there is only a single case value for   */
   /* this case.  If a comma is encountered, the right operand will be        */
   /* switched to point to the constant 1.  PDGCS needs this flag. 	      */

   NTR_IR_TBL(case_ir_idx);
   SH_IR_IDX(curr_stmt_sh_idx) = case_ir_idx;
   IR_OPR(case_ir_idx)         = Case_Opr;
   /* LRR - bhj put in short typeless as type idx */
   IR_TYPE_IDX(case_ir_idx)    = TYPELESS_DEFAULT_TYPE;
   IR_LINE_NUM(case_ir_idx)    = stmt_start_line;
   IR_COL_NUM(case_ir_idx)     = stmt_start_col;
   IR_FLD_R(case_ir_idx)       = CN_Tbl_Idx;
   IR_IDX_R(case_ir_idx)       = CN_INTEGER_ZERO_IDX;
   IR_LINE_NUM_R(case_ir_idx)  = stmt_start_line;
   IR_COL_NUM_R(case_ir_idx)   = stmt_start_col;

   if (LA_CH_VALUE == LPAREN) {

      /* ATL_CASE_LABEL is to be set to TRUE only for non-DEFAULT CASE stmts. */

      ATL_CASE_LABEL(case_lbl_idx) = TRUE;


      do {
 
         /* First trip through, eat the left paren.  All subsequent trips,    */
         /* eat the comma.						      */

         NEXT_LA_CH;

         opnd = null_opnd;

         if (LA_CH_VALUE != COLON) {
            strcpy(parse_operand_insert, "operand or :");
         
            /* Capture the first character of the case-value expression for   */
            /* better diagnostics in the Semantics Pass.		      */

            expr_start_line = LA_CH_LINE;
            expr_start_col  = LA_CH_COLUMN;

            if (! parse_expr(&opnd)) {
               parse_err_flush(Find_Rparen, NULL);
               break;
            }

            fnd_colon = FALSE;
         }

         if (LA_CH_VALUE == COLON) {
            fnd_colon = TRUE;
            NTR_IR_TBL(range_ir_idx);
            /* LRR - bhj put in short typeless as type idx */
            IR_FLD_L(case_ir_idx)     = IR_Tbl_Idx;
            IR_IDX_L(case_ir_idx)     = range_ir_idx;
            IR_OPR(range_ir_idx)      = Case_Range_Opr;
            IR_TYPE_IDX(range_ir_idx) = TYPELESS_DEFAULT_TYPE;
            IR_LINE_NUM(range_ir_idx) = LA_CH_LINE;
            IR_COL_NUM(range_ir_idx)  = LA_CH_COLUMN;

            if (OPND_FLD(opnd) != NO_Tbl_Idx) {
               COPY_OPND(IR_OPND_L(range_ir_idx), opnd);
               IR_LINE_NUM_L(range_ir_idx) = expr_start_line;
               IR_COL_NUM_L(range_ir_idx)  = expr_start_col;
            } 

            NEXT_LA_CH;

            if (LA_CH_VALUE != COMMA  &&  LA_CH_VALUE != RPAREN) {
               strcpy(parse_operand_insert, "operand, comma, or )");

               expr_start_line = LA_CH_LINE;
               expr_start_col  = LA_CH_COLUMN;

               if (parse_expr(&opnd)) {
                  COPY_OPND(IR_OPND_R(range_ir_idx), opnd);
                  IR_LINE_NUM_R(range_ir_idx) = expr_start_line;
                  IR_COL_NUM_R(range_ir_idx)  = expr_start_col;
               }
               else {
                  parse_err_flush(Find_Rparen, NULL);
               }
            }
            else {

               if (OPND_FLD(opnd) == NO_Tbl_Idx) {
                  PRINTMSG(IR_LINE_NUM(range_ir_idx), 789, Error, 
                           IR_COL_NUM(range_ir_idx));
                  continue;
               }
          
            }
         }
         else {
            COPY_OPND(IR_OPND_L(case_ir_idx), opnd);
            IR_LINE_NUM_L(case_ir_idx) = expr_start_line;
            IR_COL_NUM_L(case_ir_idx)  = expr_start_col;
         }

         ++num_cases;

         /* Link this CASE stmt to its parent SELECT CASE stmt.  The Block    */
         /* Stack checks for complete validity are made later.  Assume we     */
         /* have correct cases for now (and if we don't, not setting          */
         /* SH_PARENT_BLK_IDX won't make any difference any how).             */
   
         if (CURR_BLK == Select_Blk) {
            SH_PARENT_BLK_IDX(curr_stmt_sh_idx) = CURR_BLK_FIRST_SH_IDX;
         }
         else if (CURR_BLK == Case_Blk  &&
                  BLK_TYPE(blk_stk_idx - 1) == Select_Blk) {
            SH_PARENT_BLK_IDX(curr_stmt_sh_idx) =
               BLK_FIRST_SH_IDX(blk_stk_idx - 1);
         }
    
         /* If there is another case-value, generate an SH for it.  Indicate  */
         /* that this CASE stmt has multiple case-values by setting the right */
         /* operand index to point to 1.				      */

         if (LA_CH_VALUE == COMMA) {
            IR_IDX_R(case_ir_idx) = CN_INTEGER_ONE_IDX;

            gen_sh(After, Case_Stmt, stmt_start_line, stmt_start_col, FALSE,
                   FALSE, TRUE);

            NTR_IR_TBL(case_ir_idx);
            SH_IR_IDX(curr_stmt_sh_idx) = case_ir_idx;
            IR_OPR(case_ir_idx)         = Case_Opr;
            /* LRR - bhj put in short typeless as type idx */
            IR_TYPE_IDX(case_ir_idx)    = TYPELESS_DEFAULT_TYPE;
            IR_LINE_NUM(case_ir_idx)    = stmt_start_line;
            IR_COL_NUM(case_ir_idx)     = stmt_start_col;
            IR_FLD_R(case_ir_idx)       = CN_Tbl_Idx;
            IR_IDX_R(case_ir_idx)       = CN_INTEGER_ZERO_IDX;
            IR_LINE_NUM_R(case_ir_idx)  = stmt_start_line;
            IR_COL_NUM_R(case_ir_idx)   = stmt_start_col;
         }

      } while (LA_CH_VALUE == COMMA);

      if (LA_CH_VALUE == RPAREN) {
         NEXT_LA_CH;
      }
      else {
         parse_err_flush(Find_EOS,
                         (fnd_colon) ? "comma or )" : "comma, :, or )");
      }

      fnd_name = MATCHED_TOKEN_CLASS(Tok_Class_Id);
   }
   else if (matched_specific_token(Tok_Kwd_Default, Tok_Class_Keyword)) {
      fnd_name    = MATCHED_TOKEN_CLASS(Tok_Class_Id);
      fnd_default = TRUE;
   }
   else {
      parse_err_flush(Find_EOS, "( or DEFAULT");
   }

   if (LA_CH_VALUE != EOS) {
      parse_err_flush(Find_EOS, EOS_STR);
   }


   if (STMT_LEGAL_IN_BLK(Case_Stmt, CURR_BLK)) {

      matched_blk = (fnd_name) ? (CURR_BLK_NAME != NULL_IDX &&
                                 (compare_names(TOKEN_ID(token).words,
                                             TOKEN_LEN(token),
                                             AT_OBJ_NAME_LONG(CURR_BLK_NAME),
                                             AT_NAME_LEN(CURR_BLK_NAME)) == 0)):
                                 TRUE;
   }

   if (matched_blk) {

      if ((cif_flags & XREF_RECS) && fnd_name && matched_blk) {
         cif_usage_rec(CURR_BLK_NAME, AT_Tbl_Idx,
   		       TOKEN_LINE(token), TOKEN_COLUMN(token),
   		       CIF_Construct_Name_Reference);
      }
      curr_stmt_category = Executable_Stmt_Cat;
      blk_idx            = (CURR_BLK == Select_Blk) ?
                              blk_stk_idx : blk_stk_idx - 1;
   }
   else {
      blk_idx = blk_match_err(Select_Blk, fnd_name, FALSE);

      if (blk_idx == NULL_IDX) {
         goto EXIT;
      }

      if (CURR_BLK == Case_Blk) {
         blk_idx = blk_stk_idx - 1;
      }
      else if (CURR_BLK == Select_Blk) {
         blk_idx = blk_stk_idx;
      } 
      else {
         goto EXIT;
      }
   }

   BLK_NUM_CASES(blk_idx) += num_cases;

   if (fnd_default) {

      if (BLK_FND_DEFAULT(blk_idx)  &&  ! BLK_ERR(blk_idx)) {

         /* This SELECT has a default CASE already.			      */

         PRINTMSG(stmt_start_line, 159, Error, stmt_start_col,
                  BLK_DEF_LINE(blk_idx));
      }
      else {
         BLK_CASE_DEFAULT_LBL_LINE_NUM(blk_idx) = stmt_start_line;
         BLK_CASE_DEFAULT_LBL_COL_NUM(blk_idx)  = stmt_start_col;
         BLK_CASE_DEFAULT_LBL_FLD(blk_idx)      = AT_Tbl_Idx;
         BLK_CASE_DEFAULT_LBL_IDX(blk_idx)      = case_lbl_idx;
      }
 
      BLK_FND_DEFAULT(blk_idx) = TRUE;
   }

   if (CURR_BLK == Case_Blk) {

      /* Reuse the Block Stack frame.					      */

      CURR_BLK_DEF_LINE	  = stmt_start_line;
      CURR_BLK_DEF_COLUMN = stmt_start_col;

      /* Generate a GO TO at the end of the previous CASE block to branch to  */
      /* the end of the construct.  					      */

      save_curr_stmt_sh_idx = curr_stmt_sh_idx;
      curr_stmt_sh_idx      = cont_stmt_sh_idx;

      gen_sh(Before, Goto_Stmt, SH_GLB_LINE(SH_PREV_IDX(curr_stmt_sh_idx)),
             SH_COL_NUM(SH_PREV_IDX(curr_stmt_sh_idx)), FALSE, FALSE, TRUE);

      curr_stmt_sh_idx = SH_PREV_IDX(curr_stmt_sh_idx);

      NTR_IR_TBL(ir_idx);
      SH_IR_IDX(curr_stmt_sh_idx) = ir_idx;
      IR_OPR(ir_idx)              = Br_Uncond_Opr;
      /* LRR - bhj put in short typeless as type idx */
      IR_TYPE_IDX(ir_idx)         = TYPELESS_DEFAULT_TYPE;
      IR_LINE_NUM(ir_idx)         = SH_GLB_LINE(curr_stmt_sh_idx);
      IR_COL_NUM(ir_idx)          = SH_COL_NUM(curr_stmt_sh_idx);
      IR_LINE_NUM_R(ir_idx)       = IR_LINE_NUM(ir_idx); 
      IR_COL_NUM_R(ir_idx)        = IR_COL_NUM(ir_idx); 
      IR_FLD_R(ir_idx)            = AT_Tbl_Idx;
      IR_IDX_R(ir_idx)            = BLK_LABEL(blk_idx);

      curr_stmt_sh_idx = save_curr_stmt_sh_idx;
   }
   else {
      PUSH_BLK_STK(Case_Blk);
      CURR_BLK_ERR  = BLK_ERR(blk_idx);
      CURR_BLK_NAME = BLK_NAME(blk_idx);
   }

   CURR_BLK_FIRST_SH_IDX = curr_stmt_sh_idx;
   LINK_TO_PARENT_BLK;

   if (cif_flags & MISC_RECS) {
      cif_stmt_type_rec(TRUE,
                        (fnd_default) ? CIF_Case_Default_Stmt : CIF_Case_Stmt,
                        statement_number);
   }

EXIT:

   NEXT_LA_CH;
   strcpy(parse_operand_insert, "operand");

   TRACE (Func_Exit, "parse_case_stmt", NULL);

   return;

}  /* parse_case_stmt */


/******************************************************************************\
|*									      *|
|* Description:								      *|
|*	BRIEF DESCRIPTION OF THIS FUNCTION'S PURPOSE			      *|
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

void parse_continue_stmt (void)

{
   TRACE (Func_Entry, "parse_continue_stmt", NULL);

   if (LA_CH_VALUE != EOS) {
      parse_err_flush(Find_EOS, EOS_STR);
   }

   if (!CURR_BLK_NO_EXEC || !iss_blk_stk_err()) {
      curr_stmt_category = Executable_Stmt_Cat;
   }

   NEXT_LA_CH;

   TRACE (Func_Exit, "parse_continue_stmt", NULL);

   return;

}  /* parse_continue_stmt */


/******************************************************************************\
|*									      *|
|* Description:								      *|
|*	Parse the CYCLE statement.  If a matching DO construct is found,      *|
|*      generate the internal cycle point label and a Br_Uncond IR to jump to *|
|*      it.								      *|
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

void parse_cycle_stmt (void)

{
   int		blk_idx;
   boolean	fnd_name	= FALSE;
   int		ir_idx;


   TRACE (Func_Entry, "parse_cycle_stmt", NULL);

   if (LA_CH_VALUE != EOS) {
      fnd_name = MATCHED_TOKEN_CLASS(Tok_Class_Id);

      if (fnd_name) {

         if (LA_CH_VALUE != EOS) {
            parse_err_flush(Find_EOS, EOS_STR);
         }

      }
      else {
         parse_err_flush(Find_EOS, "construct-name or EOS");
         goto EXIT;
      }

   }

   blk_idx = match_blk(Do_Blk, fnd_name);

   if (blk_idx != NULL_IDX) {

      if (! CURR_BLK_NO_EXEC  ||  ! iss_blk_stk_err()) {
         curr_stmt_category		 = Executable_Stmt_Cat;
         SH_P2_SKIP_ME(curr_stmt_sh_idx) = TRUE;
         BLK_CYCLE_STMT(blk_idx)	 = TRUE;

         NTR_IR_TBL(ir_idx);
         SH_IR_IDX(curr_stmt_sh_idx) = ir_idx;
         IR_OPR(ir_idx)              = Br_Uncond_Opr;
         /* LRR - bhj put in short typeless as type idx */
         IR_TYPE_IDX(ir_idx)         = TYPELESS_DEFAULT_TYPE;
         IR_LINE_NUM(ir_idx)         = stmt_start_line;
         IR_COL_NUM(ir_idx)          = stmt_start_col;
         IR_LINE_NUM_R(ir_idx)       = stmt_start_line;
         IR_COL_NUM_R(ir_idx)        = stmt_start_col;
         IR_FLD_R(ir_idx)            = AT_Tbl_Idx;
         IR_IDX_R(ir_idx)            = gen_loop_lbl_name(blk_idx, Cycle_Lbl);

         if (fnd_name  &&  (cif_flags & XREF_RECS)) {
            cif_usage_rec(BLK_NAME(blk_idx), AT_Tbl_Idx, 
			  TOKEN_LINE(token), TOKEN_COLUMN(token),
			  CIF_Construct_Name_Reference);
         }
      }  

   }
   else {
      PRINTMSG(stmt_start_line, 262, Error, stmt_start_col, "CYCLE");
   }

EXIT:

   NEXT_LA_CH;
   
   TRACE (Func_Exit, "parse_cycle_stmt", NULL);

   return;

}  /* parse_cycle_stmt */


/******************************************************************************\
|*									      *|
|* Description:								      *|
|*	BNF       - DEALLOCATE ( allocation-list [, STAT = stat-variable] )   *|
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

void parse_deallocate_stmt (void)

{
   boolean      had_stat        = FALSE;
   int          ir_idx;
   int          list1_idx;
   int          list2_idx	= 0;
   opnd_type    opnd;
   boolean      parsed_ok = TRUE;
   token_type   stat_token;


   TRACE (Func_Entry, "parse_deallocate_stmt", NULL);

   if (LA_CH_VALUE != LPAREN) {
      parse_err_flush(Find_EOS, "(");
      goto EXIT;
   }

   if (!CURR_BLK_NO_EXEC || !iss_blk_stk_err()) {
      curr_stmt_category = Executable_Stmt_Cat;
   }

   NTR_IR_TBL(ir_idx);
   SH_STMT_TYPE(curr_stmt_sh_idx) = Deallocate_Stmt;
   SH_IR_IDX(curr_stmt_sh_idx)    = ir_idx;

   IR_OPR(ir_idx)                 = Deallocate_Opr;
   IR_TYPE_IDX(ir_idx)            = TYPELESS_DEFAULT_TYPE;
   IR_COL_NUM(ir_idx)             = TOKEN_COLUMN(token);
   IR_LINE_NUM(ir_idx)            = TOKEN_LINE(token);
   IR_FLD_L(ir_idx)               = IL_Tbl_Idx;
   IR_LIST_CNT_L(ir_idx)          = 0;

   do {
      NEXT_LA_CH;

      if (MATCHED_TOKEN_CLASS(Tok_Class_Id)) {

         if (strcmp(TOKEN_STR(token),"STAT") == 0) {
            stat_token = token;

            if (LA_CH_VALUE == EQUAL) {
               NEXT_LA_CH;
               had_stat = TRUE;

               if (MATCHED_TOKEN_CLASS(Tok_Class_Id)) {
                  /* have stat var */
                  /* do that stat stuff */

                  parsed_ok = parse_deref(&opnd, NULL_IDX) && parsed_ok;
                  COPY_OPND(IR_OPND_R(ir_idx), opnd);

                  mark_attr_defined(&opnd);
                  continue;
               } 
               else {

                  parse_err_flush(Find_Comma_Rparen,
                                  "scalar integer STAT variable");
                  continue;
               }
            } /* if (matched_specific_token(Tok_Punct_Eq, Tok_Class_Punct)) */
         } /* if (strcmp(TOKEN_STR(token),"STAT")) */
	
         NTR_IR_LIST_TBL(list1_idx);

         if (list2_idx) {
            IL_NEXT_LIST_IDX(list2_idx) = list1_idx;
            IL_PREV_LIST_IDX(list1_idx) = list2_idx;
         }
         else {
            IR_IDX_L(ir_idx) = list1_idx;
         }
         list2_idx = list1_idx;

         (IR_LIST_CNT_L(ir_idx))++;

         /* have token */
         if (had_stat) {
            PRINTMSG(TOKEN_LINE(stat_token), 203, Error,
                     TOKEN_COLUMN(stat_token), NULL);
         }

         if (parse_deref(&opnd, NULL_IDX)) {
            parsed_ok = change_subscript(&opnd) && parsed_ok;
         }
         else {
            parsed_ok = FALSE;
         }

         COPY_OPND(IL_OPND(list1_idx), opnd);

         if (LA_CH_VALUE != COMMA && LA_CH_VALUE != RPAREN) {

            parse_err_flush(Find_Comma_Rparen,", or )");
         }
         
         if (LA_CH_VALUE == EOS) {
            goto EXIT;
         }

      } else {
         parse_err_flush(Find_EOS, "allocation object or STAT = ");
         goto EXIT;
      }
   }
   while (LA_CH_VALUE == COMMA);

   if (LA_CH_VALUE != RPAREN) {
      parse_err_flush(Find_EOS, ")");
   }
   else {
      NEXT_LA_CH;
   }

EXIT:

   if (LA_CH_VALUE != EOS) {
      parse_err_flush(Find_EOS, EOS_STR);
   }

   matched_specific_token(Tok_EOS, Tok_Class_Punct);

   TRACE (Func_Exit, "parse_deallocate_stmt", NULL);

   return;

}  /* parse_deallocate_stmt */


/******************************************************************************\
|*									      *|
|* Description:								      *|
|*	This function parses all forms of the DO statement.                   *|
|*									      *|
|*      R819  label-do-stmt  is  [do-construct-name:] DO label [loop-control] *|
|*      R820  nonlabel-do-stmt  is  [do-construct-name:] DO [loop-control]    *|
|*      R821  loop-control   is  [,] do-variable = scalar-numeric-expr,       *|
|*                                   scalar-numeric-expr, scalar-numeric-expr *|
|*                           or  [,] WHILE(scalar-logical-expr)               *|
|*									      *|
|*      The leading token possibilities are:				      *|
|* 									      *|
|*                                     DO				      *|
|*                                     |				      *|
|*                   -----------------------------------------------------    *|
|*                   |                      |          |         |       |    *|
|*                 label		    ,        WHILE   variable   EOS   *|
|*                   |                      |				      *|
|*        ----------------------        ----------			      *|
|*        |          |         |        |        |			      *|
|*        ,       variable   WHILE   variable  WHILE			      *|
|*        |          							      *|
|*   ------------   							      *|
|*   |          |							      *|
|*  variable  WHILE 							      *|
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

void parse_do_stmt (void)

{
   int		blk_idx;
   int		cif_il_idx;
   boolean	comma_found	= FALSE;
   opnd_type    do_variable;
   int		expr_start_col;
   int		expr_start_line;
   int		idx;
#ifdef KEY /* Bug 10177 */
   int 		il_idx = 0;
   int 		il_idx_2 = 0;
#else /* KEY Bug 10177 */
   int 		il_idx;
   int 		il_idx_2;
#endif /* KEY Bug 10177 */
   boolean	label_found	= FALSE;
   int		last_il_idx;
   int		loop_info_idx;
   int  	loop_control_il_idx;
   opnd_type	loop_expr;
   int		loop_labels_il_idx;
   int		mp_nest_list_idx = NULL_IDX;
#ifdef KEY /* Bug 10177 */
   int		mp_prev_idx = 0;
#else /* KEY Bug 10177 */
   int		mp_prev_idx;
#endif /* KEY Bug 10177 */
   int		name_idx;
   opnd_type	while_expr;


   TRACE (Func_Entry, "parse_do_stmt", NULL);

   if (! CURR_BLK_NO_EXEC  ||  ! iss_blk_stk_err()) {

      if (cdir_switches.doall_sh_idx != NULL_IDX) {
         PUSH_BLK_STK (Doall_Blk);
         BLK_IS_PARALLEL_REGION(blk_stk_idx)	= TRUE;
         CURR_BLK_FIRST_SH_IDX     = cdir_switches.doall_sh_idx;
         cdir_switches.doall_sh_idx = NULL_IDX;
         cdir_switches.doall_region = TRUE;
      }
      else if (cdir_switches.doacross_sh_idx != NULL_IDX) {
         /* need BLK stack stuff here BHJ */
         PUSH_BLK_STK (SGI_Doacross_Blk);
         BLK_IS_PARALLEL_REGION(blk_stk_idx) 	= TRUE;
         CURR_BLK_FIRST_SH_IDX     = cdir_switches.doacross_sh_idx;
         mp_nest_list_idx = IR_IDX_L(SH_IR_IDX(
                                        cdir_switches.doacross_sh_idx));

         idx = 0;

         while (idx < MP_DIR_NEST_IDX) {
            mp_nest_list_idx =
                       IL_NEXT_LIST_IDX(mp_nest_list_idx);
            idx++;
         }

         if (IL_FLD(mp_nest_list_idx) == NO_Tbl_Idx) {
            NTR_IR_LIST_TBL(idx);
            IL_FLD(mp_nest_list_idx) = IL_Tbl_Idx;
            IL_IDX(mp_nest_list_idx) = idx;
            IL_LIST_CNT(mp_nest_list_idx) = 1;
            mp_prev_idx = mp_nest_list_idx;
            mp_nest_list_idx = idx;
         }
         else {
            mp_nest_list_idx = NULL_IDX;
         }

         cdir_switches.dir_nest_check_sh_idx = cdir_switches.doacross_sh_idx;
         cdir_switches.doacross_sh_idx = NULL_IDX;
      }
      else if (cdir_switches.paralleldo_sh_idx != NULL_IDX) {
         /* need BLK stack stuff here BHJ */
         PUSH_BLK_STK (SGI_Parallel_Do_Blk);
         BLK_IS_PARALLEL_REGION(blk_stk_idx) 	= TRUE;
         CURR_BLK_FIRST_SH_IDX     = cdir_switches.paralleldo_sh_idx;
         mp_nest_list_idx = IR_IDX_L(SH_IR_IDX(
                                        cdir_switches.paralleldo_sh_idx));

         idx = 0;

         while (idx < MP_DIR_NEST_IDX) {
            mp_nest_list_idx =
                       IL_NEXT_LIST_IDX(mp_nest_list_idx);
            idx++;
         }

         if (IL_FLD(mp_nest_list_idx) == NO_Tbl_Idx) {
            NTR_IR_LIST_TBL(idx);
            IL_FLD(mp_nest_list_idx) = IL_Tbl_Idx;
            IL_IDX(mp_nest_list_idx) = idx;
            IL_LIST_CNT(mp_nest_list_idx) = 1;
            mp_prev_idx = mp_nest_list_idx;
            mp_nest_list_idx = idx;
         }
         else {
            mp_nest_list_idx = NULL_IDX;
         }

         cdir_switches.dir_nest_check_sh_idx = cdir_switches.paralleldo_sh_idx;
         cdir_switches.paralleldo_sh_idx = NULL_IDX;
      }
      else if (cdir_switches.pdo_sh_idx != NULL_IDX) {
         /* need BLK stack stuff here BHJ */
         PUSH_BLK_STK (SGI_Pdo_Blk);
         BLK_IS_PARALLEL_REGION(blk_stk_idx) 	= TRUE;
         CURR_BLK_FIRST_SH_IDX     = cdir_switches.pdo_sh_idx;
         mp_nest_list_idx = IR_IDX_L(SH_IR_IDX(
                                        cdir_switches.pdo_sh_idx));

         idx = 0;

         while (idx < MP_DIR_NEST_IDX) {
            mp_nest_list_idx =
                       IL_NEXT_LIST_IDX(mp_nest_list_idx);
            idx++;
         }

         if (IL_FLD(mp_nest_list_idx) == NO_Tbl_Idx) {
            NTR_IR_LIST_TBL(idx);
            IL_FLD(mp_nest_list_idx) = IL_Tbl_Idx;
            IL_IDX(mp_nest_list_idx) = idx;
            IL_LIST_CNT(mp_nest_list_idx) = 1;
            mp_prev_idx = mp_nest_list_idx;
            mp_nest_list_idx = idx;
         }
         else {
            mp_nest_list_idx = NULL_IDX;
         }

         cdir_switches.dir_nest_check_sh_idx = cdir_switches.pdo_sh_idx;
         cdir_switches.pdo_sh_idx = NULL_IDX;
      }
      else if (cdir_switches.dopar_sh_idx != NULL_IDX) {
         PUSH_BLK_STK (Do_Parallel_Blk);
         CURR_BLK_FIRST_SH_IDX     = cdir_switches.dopar_sh_idx;
         cdir_switches.dopar_sh_idx = NULL_IDX;
      }
      else if (cdir_switches.do_omp_sh_idx != NULL_IDX) {
         PUSH_BLK_STK (Open_Mp_Do_Blk);
         BLK_IS_PARALLEL_REGION(blk_stk_idx)	= TRUE;
         mp_nest_list_idx = IR_IDX_L(SH_IR_IDX(
                                        cdir_switches.do_omp_sh_idx));

         idx = 0;

         while (idx < OPEN_MP_NEST_IDX) {
            mp_nest_list_idx =
                       IL_NEXT_LIST_IDX(mp_nest_list_idx);
            idx++;
         }

         if (IL_FLD(mp_nest_list_idx) == NO_Tbl_Idx) {
            NTR_IR_LIST_TBL(idx);
            IL_FLD(mp_nest_list_idx) = IL_Tbl_Idx;
            IL_IDX(mp_nest_list_idx) = idx;
            IL_LIST_CNT(mp_nest_list_idx) = 1;
            mp_prev_idx = mp_nest_list_idx;
            mp_nest_list_idx = idx;
         }
         else {
            mp_nest_list_idx = NULL_IDX;
         }

         CURR_BLK_FIRST_SH_IDX     = cdir_switches.do_omp_sh_idx;
         cdir_switches.do_omp_sh_idx = NULL_IDX;
      }
      else if (cdir_switches.paralleldo_omp_sh_idx != NULL_IDX) {
         PUSH_BLK_STK (Open_Mp_Parallel_Do_Blk);
         BLK_IS_PARALLEL_REGION(blk_stk_idx) 	= TRUE;
         mp_nest_list_idx = IR_IDX_L(SH_IR_IDX(
                                        cdir_switches.paralleldo_omp_sh_idx));

         idx = 0;

         while (idx < OPEN_MP_NEST_IDX) {
            mp_nest_list_idx =
                       IL_NEXT_LIST_IDX(mp_nest_list_idx);
            idx++;
         }

         if (IL_FLD(mp_nest_list_idx) == NO_Tbl_Idx) {
            NTR_IR_LIST_TBL(idx);
            IL_FLD(mp_nest_list_idx) = IL_Tbl_Idx;
            IL_IDX(mp_nest_list_idx) = idx;
            IL_LIST_CNT(mp_nest_list_idx) = 1;
            mp_prev_idx = mp_nest_list_idx;
            mp_nest_list_idx = idx;
         }
         else {
            mp_nest_list_idx = NULL_IDX;
         }

         CURR_BLK_FIRST_SH_IDX     = cdir_switches.paralleldo_omp_sh_idx;
         cdir_switches.paralleldo_omp_sh_idx = NULL_IDX;
      }

      PUSH_BLK_STK (Do_Blk);
      curr_stmt_category = Executable_Stmt_Cat;
   }
   else {

      if (cdir_switches.doall_sh_idx != NULL_IDX) {
         PUSH_BLK_STK (Doall_Blk);
         BLK_IS_PARALLEL_REGION(blk_stk_idx)	= TRUE;
         CURR_BLK_FIRST_SH_IDX     = cdir_switches.doall_sh_idx;
         cdir_switches.doall_sh_idx = NULL_IDX;
         cdir_switches.doall_region = TRUE;
      }
      else if (cdir_switches.doacross_sh_idx != NULL_IDX) {
         /* need BLK stack stuff here BHJ */
         PUSH_BLK_STK (SGI_Doacross_Blk);
         BLK_IS_PARALLEL_REGION(blk_stk_idx) 	= TRUE;
         CURR_BLK_FIRST_SH_IDX     = cdir_switches.doacross_sh_idx;
         cdir_switches.dir_nest_check_sh_idx = cdir_switches.doacross_sh_idx;
         cdir_switches.doacross_sh_idx = NULL_IDX;
      }
      else if (cdir_switches.paralleldo_sh_idx != NULL_IDX) {
         /* need BLK stack stuff here BHJ */
         PUSH_BLK_STK (SGI_Parallel_Do_Blk);
         BLK_IS_PARALLEL_REGION(blk_stk_idx) 	= TRUE;
         CURR_BLK_FIRST_SH_IDX     = cdir_switches.paralleldo_sh_idx;
         cdir_switches.dir_nest_check_sh_idx = cdir_switches.paralleldo_sh_idx;
         cdir_switches.paralleldo_sh_idx = NULL_IDX;
      }
      else if (cdir_switches.pdo_sh_idx != NULL_IDX) {
         /* need BLK stack stuff here BHJ */
         PUSH_BLK_STK (SGI_Pdo_Blk);
         BLK_IS_PARALLEL_REGION(blk_stk_idx) 	= TRUE;
         CURR_BLK_FIRST_SH_IDX     = cdir_switches.pdo_sh_idx;
         cdir_switches.dir_nest_check_sh_idx = cdir_switches.pdo_sh_idx;
         cdir_switches.pdo_sh_idx = NULL_IDX;
      }
      else if (cdir_switches.dopar_sh_idx != NULL_IDX) {
         PUSH_BLK_STK (Do_Parallel_Blk);
         CURR_BLK_FIRST_SH_IDX     = cdir_switches.dopar_sh_idx;
         cdir_switches.dopar_sh_idx = NULL_IDX;
      }
      else if (cdir_switches.do_omp_sh_idx != NULL_IDX) {
         PUSH_BLK_STK (Open_Mp_Do_Blk);
         BLK_IS_PARALLEL_REGION(blk_stk_idx)	= TRUE;
         CURR_BLK_FIRST_SH_IDX     = cdir_switches.do_omp_sh_idx;
         cdir_switches.do_omp_sh_idx = NULL_IDX;
      }
      else if (cdir_switches.paralleldo_omp_sh_idx != NULL_IDX) {
         PUSH_BLK_STK (Open_Mp_Parallel_Do_Blk);
         BLK_IS_PARALLEL_REGION(blk_stk_idx) 	= TRUE;
         CURR_BLK_FIRST_SH_IDX     = cdir_switches.paralleldo_omp_sh_idx;
         cdir_switches.paralleldo_omp_sh_idx = NULL_IDX;
      }

      PUSH_BLK_STK (Do_Blk);
      CURR_BLK_ERR = TRUE;
   }

   CURR_BLK_FIRST_SH_IDX = curr_stmt_sh_idx;
   LINK_TO_PARENT_BLK;

   /* Generate the Loop_Info IR.  It's left operand will eventually point at  */
   /* the SH for the stmt that ends the loop.  It's right operand will point  */
   /* at a series of ILs.  The first IL will point at a list of ILs that      */
   /* represent the loop control info (for iterative or WHILE loops); for an  */
   /* infinite loop, there is no loop control info so the first IL is null.   */
   /* The second IL will point at a list of ILs for the top, bottom, and skip */
   /* labels for an iterative or WHILE loop; for an infinite loop, the IL     */
   /* will point directly at the top label.				      */
   /* If Miscellaneous CIF records are being produced, a third IL exists in   */
   /* the chain attached to the Loop_Info IR.  This IL points at a list of    */
   /* ILs for the DO construct name and the loop label.			      */

   NTR_IR_TBL(loop_info_idx);
   SH_IR_IDX(curr_stmt_sh_idx) = loop_info_idx;
   IR_OPR(loop_info_idx)       = Loop_Info_Opr;
   /* LRR - bhj put in short typeless as type idx */
   IR_TYPE_IDX(loop_info_idx)  = TYPELESS_DEFAULT_TYPE;
   IR_LINE_NUM(loop_info_idx)  = stmt_start_line;
   IR_COL_NUM(loop_info_idx)   = stmt_start_col;

   NTR_IR_LIST_TBL(loop_control_il_idx);
   IR_LIST_CNT_R(loop_info_idx) = 1;
   IR_FLD_R(loop_info_idx)      = IL_Tbl_Idx;
   IR_IDX_R(loop_info_idx)      = loop_control_il_idx;

   NTR_IR_LIST_TBL(loop_labels_il_idx);
   IL_NEXT_LIST_IDX(loop_control_il_idx) = loop_labels_il_idx;
   IL_PREV_LIST_IDX(loop_labels_il_idx)  = loop_control_il_idx;
   ++IR_LIST_CNT_R(loop_info_idx);

   if (cif_flags & MISC_RECS) {
      NTR_IR_LIST_TBL(cif_il_idx);
      IL_NEXT_LIST_IDX(loop_labels_il_idx) = cif_il_idx;
      IL_PREV_LIST_IDX(cif_il_idx)         = loop_labels_il_idx;
      ++IR_LIST_CNT_R(loop_info_idx);

      /* Always generate the ILs for the construct name and loop label if     */
      /* these "labels" are to be saved.				      */

      NTR_IR_LIST_TBL(il_idx);
      NTR_IR_LIST_TBL(il_idx_2);
      IL_NEXT_LIST_IDX(il_idx)   = il_idx_2;
      IL_PREV_LIST_IDX(il_idx_2) = il_idx;

      IL_FLD(cif_il_idx)      = IL_Tbl_Idx;
      IL_IDX(cif_il_idx)      = il_idx;
      IL_LIST_CNT(cif_il_idx) = 2;
   }

   if (stmt_construct_idx != NULL_IDX) {
      CURR_BLK_NAME			   = stmt_construct_idx;
      ATL_CLASS(stmt_construct_idx)	   = Lbl_Construct;
      ATL_DEF_STMT_IDX(stmt_construct_idx) = curr_stmt_sh_idx;
      SH_ERR_FLG(curr_stmt_sh_idx)	   = SH_ERR_FLG(curr_stmt_sh_idx) |
                                                AT_DCL_ERR(stmt_construct_idx);

      /* Save the construct name's Attr index in the second IL attached to the*/
      /* CIF IL node.  It's needed for production of the Loop Definition      */
      /* record in the Semantics Pass.  (Can't output the record now because  */
      /* the DO-variable of an iterative DO hasn't been resolved.)	      */

      if (cif_flags & MISC_RECS) {
         IL_FLD(il_idx_2) = AT_Tbl_Idx;
         IL_IDX(il_idx_2) = stmt_construct_idx;
         IL_LINE_NUM(il_idx_2) = stmt_start_line;
         IL_COL_NUM(il_idx_2)  = stmt_start_col;
      }

      stmt_construct_idx = NULL_IDX;
   }
   
   if (MATCHED_TOKEN_CLASS(Tok_Class_Label)  &&  ! TOKEN_ERR(token)) {

      CURR_BLK_LABEL = srch_sym_tbl(TOKEN_STR(token), TOKEN_LEN(token),
                                    &name_idx); 

      if (CURR_BLK_LABEL == NULL_IDX) {
         CURR_BLK_LABEL	              		= ntr_sym_tbl(&token, name_idx);
         AT_OBJ_CLASS(CURR_BLK_LABEL)		= Label;
         LN_DEF_LOC(name_idx)			= TRUE;

         if (cmd_line_flags.debug_lvl == Debug_Lvl_1) {  /* -ez  -G1 */
            ATL_DEBUG_CLASS(CURR_BLK_LABEL)	= Ldbg_User_Lbl;
         }
      }
      else if (AT_DEFINED(CURR_BLK_LABEL)) {
         PRINTMSG(TOKEN_LINE(token), 248, Error, TOKEN_COLUMN(token));
         parse_err_flush(Find_EOS, NULL);
         goto EXIT;
      }

      AT_REFERENCED(CURR_BLK_LABEL) = Referenced;
      label_found                   = TRUE;

      build_fwd_ref_entry(CURR_BLK_LABEL, Do_Ref); 

      blk_idx = blk_stk_idx - 1;
      
      while (BLK_IS_PARALLEL_REGION(blk_idx) ||
             BLK_TYPE(blk_idx) == Do_Parallel_Blk  ||
             BLK_TYPE(blk_idx) == Wait_Blk ||
             BLK_TYPE(blk_idx) == SGI_Region_Blk ||
             BLK_TYPE(blk_idx) == Open_Mp_Section_Blk) {
         blk_idx--;
      }

      if (BLK_TYPE(blk_idx) == Do_Blk  &&
          BLK_LABEL(blk_idx) == CURR_BLK_LABEL) {

         if (BLK_LOOP_NUM(blk_idx) == MAX_BLK_LOOP_NUM) {
            PRINTMSG(stmt_start_line, 1324, Internal,  stmt_start_col,
                     MAX_BLK_LOOP_NUM);
         }
         else {
            BLK_LOOP_NUM(blk_stk_idx) = BLK_LOOP_NUM(blk_idx) + 1;
         }
      }
      else {
         BLK_LOOP_NUM(blk_stk_idx) = 1;
      }

      if (cif_flags & XREF_RECS) {
         cif_usage_rec(CURR_BLK_LABEL, AT_Tbl_Idx,
		       TOKEN_LINE(token), TOKEN_COLUMN(token),
		       CIF_Do_Loop_Label);
      }

      /* Save the loop label's Attr index in the first IL attached to the     */
      /* CIF IL node.  It's also needed for production of the Loop Definition */
      /* record in the Semantics Pass.					      */

      if (cif_flags & MISC_RECS) {
         IL_FLD(il_idx) = AT_Tbl_Idx;
         IL_IDX(il_idx) = CURR_BLK_LABEL;
         IL_LINE_NUM(il_idx) = stmt_start_line;
         IL_COL_NUM(il_idx)  = stmt_start_col;
      }
   }
   else {

      if (last_unlbld_loop_line == stmt_start_line) {

         if (last_unlbld_loop_num == MAX_BLK_LOOP_NUM) {
            PRINTMSG(stmt_start_line, 1324, Internal,  stmt_start_col,
                     MAX_BLK_LOOP_NUM);
         }
         else {
            BLK_LOOP_NUM(blk_stk_idx) = ++last_unlbld_loop_num;
         }
      }
      else {
         last_unlbld_loop_line = stmt_start_line;
         last_unlbld_loop_num  = 1;
         BLK_LOOP_NUM(blk_stk_idx) = 1;
      }
   }

   if (LA_CH_VALUE == COMMA) {
      NEXT_LA_CH;
      comma_found = TRUE;
   }

   
   if (matched_specific_token(Tok_Kwd_While, Tok_Class_Keyword)) {

      /* -------------------------------------------------------------------- */
      /*                             DO WHILE   		              */
      /* -------------------------------------------------------------------- */

      if (LA_CH_VALUE == LPAREN) {
         NEXT_LA_CH;
         SH_STMT_TYPE(curr_stmt_sh_idx) = Do_While_Stmt;
         stmt_type                      = Do_While_Stmt;
         BLK_DO_TYPE(blk_stk_idx)       = While_Loop;

         expr_start_line = LA_CH_LINE;
         expr_start_col  = LA_CH_COLUMN;

         if (parse_expr(&while_expr)) {
     
            if (LA_CH_VALUE == RPAREN) {
               NEXT_LA_CH;

               if (LA_CH_VALUE == EOS) {

                  NTR_IR_LIST_TBL(il_idx);
                  IL_LIST_CNT(loop_control_il_idx) = 1;
                  IL_FLD(loop_control_il_idx)      = IL_Tbl_Idx;
                  IL_IDX(loop_control_il_idx)      = il_idx;
                  COPY_OPND(IL_OPND(il_idx), while_expr);
                  IL_LINE_NUM(il_idx)              = expr_start_line;
                  IL_COL_NUM(il_idx)               = expr_start_col;

                  /* Generate the name of the skip label.                     */
                  /* Attach it to the loop labels IL node.		      */

                  BLK_SKIP_LBL_IDX(blk_stk_idx) =
                     gen_loop_lbl_name(blk_stk_idx, Skip_Lbl);

                  if (BLK_SKIP_LBL_IDX(blk_stk_idx) == NULL_IDX) {
                     goto EXIT;
                  }

# ifndef _HIGH_LEVEL_DO_LOOP_FORM

                  NTR_IR_LIST_TBL(il_idx);
                  IL_LIST_CNT(loop_labels_il_idx) = 1;
                  IL_FLD(loop_labels_il_idx)      = IL_Tbl_Idx;
                  IL_IDX(loop_labels_il_idx)      = il_idx;

                  IL_LINE_NUM(il_idx) = stmt_start_line;
                  IL_COL_NUM(il_idx)  = stmt_start_col;
                  IL_FLD(il_idx)      = AT_Tbl_Idx;
                  IL_IDX(il_idx)      = BLK_SKIP_LBL_IDX(blk_stk_idx);


                  /* Generate the name of the top-of-loop label.              */
                  /* Attach it to the loop labels IL node ahead of the skip   */
                  /* label IL. 						      */

                  NTR_IR_LIST_TBL(il_idx);
                  IL_NEXT_LIST_IDX(il_idx) = IL_IDX(loop_labels_il_idx);
                  IL_PREV_LIST_IDX(IL_IDX(loop_labels_il_idx)) = il_idx;

                  ++IL_LIST_CNT(loop_labels_il_idx);
                  IL_IDX(loop_labels_il_idx) = il_idx;

                  IL_LINE_NUM(il_idx) = stmt_start_line;
                  IL_COL_NUM(il_idx)  = stmt_start_col;
                  IL_FLD(il_idx)      = AT_Tbl_Idx;
                  IL_IDX(il_idx)      = gen_loop_lbl_name(blk_stk_idx, Top_Lbl);

                  AT_DEFINED(IL_IDX(il_idx))  = TRUE;

# endif

                  if (cif_flags & MISC_RECS) {
                     cif_stmt_type_rec(TRUE, 
                                       (CURR_BLK_LABEL == NULL_IDX) ?
                                          CIF_Do_Unlabeled_While_Stmt :
					  CIF_Do_Labeled_While_Stmt,
                                          statement_number);
                  }
               }
               else {
                  parse_err_flush(Find_EOS, EOS_STR);
               }
            }
            else {
               parse_err_flush(Find_EOS, ")");
            }
         }
         else {
            parse_err_flush(Find_EOS, NULL);
         }
      }
      else if (LA_CH_CLASS == Ch_Class_Letter  ||
               LA_CH_CLASS == Ch_Class_Digit   ||
               LA_CH_VALUE == USCORE           || 
               LA_CH_VALUE == DOLLAR           ||
#ifdef KEY /* Bug 4690 */
	       /* We have reached the "=" in something like "do while = 1, 2" */
               LA_CH_VALUE == EQUAL  ||
#endif /* KEY Bug 4690 */
               LA_CH_VALUE == AT_SIGN) {
         reset_lex(TOKEN_BUF_IDX(token), TOKEN_STMT_NUM(token));
         goto CHECK_FOR_VARIABLE;
      }
      else {
         parse_err_flush(Find_EOS, "( or =");
      }

      goto EXIT;
   }


CHECK_FOR_VARIABLE:

   if (MATCHED_TOKEN_CLASS(Tok_Class_Id)) {

      /* -------------------------------------------------------------------- */
      /*                               Iterative DO			      */
      /* -------------------------------------------------------------------- */

      SH_STMT_TYPE(curr_stmt_sh_idx) = Do_Iterative_Stmt;
      stmt_type                      = Do_Iterative_Stmt;
      BLK_DO_TYPE(blk_stk_idx)       = Iterative_Loop;

      expr_start_line = TOKEN_LINE(token);
      expr_start_col  = TOKEN_COLUMN(token);

      if (parse_deref(&do_variable, NULL_IDX)) {

         if (mp_nest_list_idx != NULL_IDX) {
            COPY_OPND(IL_OPND(mp_nest_list_idx), do_variable);
         }

         COPY_OPND(BLK_DO_VAR_OPND(blk_stk_idx), do_variable);
         BLK_DO_VAR_LINE_NUM(blk_stk_idx) = expr_start_line;
         BLK_DO_VAR_COL_NUM(blk_stk_idx)  = expr_start_col;
      
         NTR_IR_LIST_TBL(last_il_idx);
         IL_LIST_CNT(loop_control_il_idx) = 1;
         IL_FLD(loop_control_il_idx)      = IL_Tbl_Idx;
         IL_IDX(loop_control_il_idx)      = last_il_idx;
         COPY_OPND(IL_OPND(last_il_idx), BLK_DO_VAR_OPND(blk_stk_idx));
         
         if (BLK_DO_VAR_FLD(blk_stk_idx) == AT_Tbl_Idx) {

            /* If the DO-variable is a function result, we need to set       */
            /* AT_DEFINED in the functions' Attr now because                 */
            /* prog_unit_semantics checks this right away in the Semantics   */ 
            /* Pass driver (if the flag is not set, a warning is issued about*/
            /* the function result not being defined).			     */

            if (AT_OBJ_CLASS(OPND_IDX(do_variable)) == Data_Obj  &&
                ATD_CLASS(OPND_IDX(do_variable)) == Function_Result) {
               AT_DEFINED(ATD_FUNC_IDX(OPND_IDX(do_variable))) = TRUE;
            }


            blk_idx = blk_stk_idx - 1;

            while (BLK_TYPE(blk_idx) > Module_Proc_Blk) {
         
               if (BLK_TYPE(blk_idx) == Do_Blk) {
            
                  if ((BLK_DO_VAR_FLD(blk_idx) ==
                       BLK_DO_VAR_FLD(blk_stk_idx))       && 
                      (BLK_DO_VAR_IDX(blk_idx) == 
                       BLK_DO_VAR_IDX(blk_stk_idx))       &&
                      ! BLK_ERR(blk_idx)) {
                     PRINTMSG(TOKEN_LINE(token), 517, Error,
                              TOKEN_COLUMN(token));
                     break;
                  }
               }

               --blk_idx;
            }
         }

# if defined(CDIR_INTERCHANGE)
         process_interchange_dir();
# endif

# if defined(CDIR_BLOCKABLE)
         process_blockable_dir();
# endif

# if (defined(_TARGET_OS_IRIX) || defined(_TARGET_OS_LINUX) || defined(_TARGET_OS_DARWIN))
         check_mp_dir_nesting();
# endif

      }
      else {
         parse_err_flush(Find_EOS, NULL);
         goto EXIT;
      }

      if (LA_CH_VALUE == EQUAL) {
         NEXT_LA_CH;
      }
      else {
         parse_err_flush(Find_EOS, "=");
         goto EXIT;
      }
     
      /* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -  */
      /* Parse the start expression.					      */
      /* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -  */

      expr_start_line = LA_CH_LINE;
      expr_start_col  = LA_CH_COLUMN;

      if (parse_expr(&loop_expr)) {

         /* Note:  parse_expr can return TRUE even though an error message is */
         /* produced (like if the start value is a constant with an invalid   */
         /* kind parameter).  This will cause the DO SH to be marked in       */
         /* error but that's OK.					      */

         if (! SH_ERR_FLG(curr_stmt_sh_idx)) {
            NTR_IR_LIST_TBL(il_idx);
            IL_NEXT_LIST_IDX(last_il_idx) = il_idx;
            IL_PREV_LIST_IDX(il_idx)      = last_il_idx;
            COPY_OPND(IL_OPND(il_idx), loop_expr);
            IL_LINE_NUM(il_idx)           = expr_start_line;
            IL_COL_NUM(il_idx)            = expr_start_col;
            last_il_idx                   = il_idx;
            ++IL_LIST_CNT(loop_control_il_idx);
         }
         else {
            parse_err_flush(Find_EOS, NULL);
            goto EXIT;
         }
      }
      else {
         parse_err_flush(Find_EOS, NULL);
         goto EXIT;
      }

      if (LA_CH_VALUE == COMMA) {
         NEXT_LA_CH;
      }
      else {
         parse_err_flush(Find_EOS, ",");
         goto EXIT;
      }

      /* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -  */
      /* Parse the end expression.					      */
      /* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -  */

      expr_start_line = LA_CH_LINE;
      expr_start_col  = LA_CH_COLUMN;

      if (parse_expr(&loop_expr)) {

         /* parse_expr could return TRUE because the expression is	      */
         /* sytactically correct but an error message could still have been   */
         /* issued (like for a constant that has an invalid type parameter    */
         /* value attached to it) so SH_ERR_FLG of the current stmt must also */
         /* be checked. 						      */

         if (! SH_ERR_FLG(curr_stmt_sh_idx)) {
            NTR_IR_LIST_TBL(il_idx);
            IL_NEXT_LIST_IDX(last_il_idx) = il_idx;
            IL_PREV_LIST_IDX(il_idx)      = last_il_idx;
            COPY_OPND(IL_OPND(il_idx), loop_expr);
            IL_LINE_NUM(il_idx)           = expr_start_line;
            IL_COL_NUM(il_idx)            = expr_start_col;
            last_il_idx                   = il_idx;
            ++IL_LIST_CNT(loop_control_il_idx);
         }
         else {
            parse_err_flush(Find_EOS, NULL);
            goto EXIT;
         }
      }
      else {
         parse_err_flush(Find_EOS, NULL);
         goto EXIT;
      }

      /* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -  */
      /* Parse the inc expression.					      */
      /* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -  */

      if (LA_CH_VALUE == COMMA) {
         NEXT_LA_CH;
         expr_start_line = LA_CH_LINE;
         expr_start_col  = LA_CH_COLUMN;
           
         if (parse_expr(&loop_expr)) {

            if (LA_CH_VALUE == EOS) {
                  
               /* parse_expr could return TRUE because the expression is      */
               /* sytactically correct but an error message could still have  */
               /* been issued (like for a constant that has an invalid type   */
               /* parameter value attached to it) so SH_ERR_FLG of the        */
               /* current stmt must also be checked.			      */
           
               if (! SH_ERR_FLG(curr_stmt_sh_idx)) {
                  NTR_IR_LIST_TBL(il_idx);
                  IL_NEXT_LIST_IDX(last_il_idx) = il_idx;
                  IL_PREV_LIST_IDX(il_idx)      = last_il_idx;
                  COPY_OPND(IL_OPND(il_idx), loop_expr);
                  IL_LINE_NUM(il_idx)           = expr_start_line;
                  IL_COL_NUM(il_idx)            = expr_start_col;
                  ++IL_LIST_CNT(loop_control_il_idx);
               }
               else {
                  parse_err_flush(Find_EOS, NULL);
               }
            }
            else {
               parse_err_flush(Find_EOS, EOS_STR);
            }
         }
         else {
            parse_err_flush(Find_EOS, NULL);
         }
      }
      else if (LA_CH_VALUE == EOS) {
         NTR_IR_LIST_TBL(il_idx);
         IL_NEXT_LIST_IDX(last_il_idx) = il_idx;
         IL_PREV_LIST_IDX(il_idx)      = last_il_idx;
         IL_LINE_NUM(il_idx)           = expr_start_line;
         IL_COL_NUM(il_idx)            = expr_start_col;
         IL_FLD(il_idx)                = CN_Tbl_Idx;
         IL_IDX(il_idx)                = CN_INTEGER_ONE_IDX;
         ++IL_LIST_CNT(loop_control_il_idx);
      } 
      else {
         parse_err_flush(Find_EOS, ", or EOS");
      }

      /* Generate the name of the skip label.  (It gets entered in the	      */
      /* dictionary and the current block stack entry.)	 It's generated now   */
      /* so that we can tell if there are overlapped blocking stmts (see      */
      /* gen_loop_lbl_name for details).  If there are overlapped blocking    */
      /* stmts, we'll never get to the interface so don't bother doing        */
      /* anything more for this DO loop.				      */
      /* Note:  The skip label is generated even when doing "one-trip" DO     */
      /* loops (so overlapping block errors will be caught).  The label won't */
      /* be used.							      */

# ifndef _HIGH_LEVEL_DO_LOOP_FORM

      NTR_IR_LIST_TBL(il_idx);
      IL_LIST_CNT(loop_labels_il_idx) = 1;
      IL_FLD(loop_labels_il_idx)      = IL_Tbl_Idx;
      IL_IDX(loop_labels_il_idx)      = il_idx;

# endif

      BLK_SKIP_LBL_IDX(blk_stk_idx) = gen_loop_lbl_name(blk_stk_idx, Skip_Lbl);

      if (BLK_SKIP_LBL_IDX(blk_stk_idx) != NULL_IDX) {

# ifndef _HIGH_LEVEL_DO_LOOP_FORM

         IL_FLD(il_idx)      = AT_Tbl_Idx;
         IL_IDX(il_idx)      = BLK_SKIP_LBL_IDX(blk_stk_idx);
         IL_LINE_NUM(il_idx) = expr_start_line;
         IL_COL_NUM(il_idx)  = expr_start_col;

# endif

      }
      else {
         goto EXIT;
      }

     
# ifndef _HIGH_LEVEL_DO_LOOP_FORM

      /* Generate the name of the top-of-loop label.  (It gets entered in the */
      /* dictionary and the current block stack entry.)  Put its IL ahead of  */
      /* the IL for the skip label.					      */

      NTR_IR_LIST_TBL(il_idx);
      ++IL_LIST_CNT(loop_labels_il_idx);
      IL_NEXT_LIST_IDX(il_idx) = IL_IDX(loop_labels_il_idx);
      IL_PREV_LIST_IDX(IL_IDX(loop_labels_il_idx)) = il_idx;
      IL_IDX(loop_labels_il_idx)                   = il_idx;

      BLK_TOP_LBL_IDX(blk_stk_idx) = gen_loop_lbl_name(blk_stk_idx, Top_Lbl);

      IL_LINE_NUM(il_idx) = stmt_start_line;
      IL_COL_NUM(il_idx)  = stmt_start_col;
      IL_FLD(il_idx)      = AT_Tbl_Idx;
      IL_IDX(il_idx)      = BLK_TOP_LBL_IDX(blk_stk_idx);

# endif

      if (cif_flags & MISC_RECS) {
         cif_stmt_type_rec(TRUE, 
                           (CURR_BLK_LABEL == NULL_IDX) ?
                              CIF_Do_Unlabeled_Iterative_Stmt :
		              CIF_Do_Labeled_Iterative_Stmt,
                           statement_number);
      }

      goto EXIT;
    }


   /* ----------------------------------------------------------------------- */
   /* Is this an "infinite" DO loop (that is, "DO ... END DO" or	      */
   /* "DO <label> ... <label> <term-stmt>")?				      */
   /* ----------------------------------------------------------------------- */

   if (LA_CH_VALUE == EOS) {

      if (comma_found) {
         parse_err_flush(Find_EOS, "do-variable or WHILE");
      }
      else {
         SH_STMT_TYPE(curr_stmt_sh_idx) = Do_Infinite_Stmt;
         stmt_type                      = Do_Infinite_Stmt;
         BLK_DO_TYPE(blk_stk_idx)       = Infinite_Loop;

         /* Generate the name of the top-of-loop label.  		      */

         NTR_IR_LIST_TBL(il_idx);
         IL_LIST_CNT(loop_labels_il_idx) = 1;
         IL_FLD(loop_labels_il_idx)      = IL_Tbl_Idx;
         IL_IDX(loop_labels_il_idx)      = il_idx;

# ifndef _ACSET

         IL_LINE_NUM(il_idx) = stmt_start_line;
         IL_COL_NUM(il_idx)  = stmt_start_col;
         IL_FLD(il_idx)      = AT_Tbl_Idx;
         IL_IDX(il_idx)      = gen_loop_lbl_name(blk_stk_idx, Top_Lbl);

# endif

         if (cif_flags & MISC_RECS) {
            cif_stmt_type_rec(TRUE, 
                              (CURR_BLK_LABEL == NULL_IDX) ?
                                 CIF_Do_Unlabeled_Infinite_Stmt :
                                 CIF_Do_Labeled_Infinite_Stmt,
                              statement_number);
         }
      }
   }
   else {

      if (comma_found) {
         parse_err_flush(Find_EOS, "do-variable or WHILE");
      }
      else if (label_found) {
         PRINTMSG(LA_CH_LINE, 513, Error, LA_CH_COLUMN, LA_CH_VALUE);
         parse_err_flush(Find_EOS, NULL);
      }
      else {
         PRINTMSG(LA_CH_LINE, 515, Error, LA_CH_COLUMN, LA_CH_VALUE);
         parse_err_flush(Find_EOS, NULL);
      }
   } 

EXIT:

   if (mp_nest_list_idx != NULL_IDX &&
       IL_FLD(mp_nest_list_idx) == NO_Tbl_Idx) {

      /* error situation, free the list entry */
      FREE_IR_LIST_NODE(mp_nest_list_idx);
      IL_OPND(mp_prev_idx) = null_opnd;
   }

   NEXT_LA_CH;

   /* Clear out the shortloop flags. They will be reset in pass 2.	      */

   cdir_switches.shortloop    = FALSE;
   cdir_switches.shortloop128 = FALSE;

   TRACE (Func_Exit, "parse_do_stmt", NULL);

   return;

}  /* parse_do_stmt */


/******************************************************************************\
|*									      *|
|* Description:								      *|
|*	This function handles the following syntax:			      *|
|*	   else-stmt       => ELSE [if-construct-name]			      *|
|*	   else-if-stmt    => ELSE IF (scalar-logical-expr) THEN	      *|
|*                                 [if-construct-name]			      *|
|*	   elsewhere-stmt  => ELSE WHERE				      *|
|*									      *|
|*	Note that ELSE WHERE will require consulting the block stack to       *|
|*	determine if it is actually an ELSEWHERE stmt or an ELSE stmt with a  *|
|*      construct name of WHERE.					      *|
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

void parse_else_stmt (void)

{
   int			blk_idx;
   blk_cntxt_type	blk_type		= If_Else_Blk;
   opnd_type            cond_expr;


   blk_cntxt_type	err_blk;
#ifdef KEY /* Bug 10177 */
   int			expr_start_line = 0;
   int			expr_start_col = 0;
#else /* KEY Bug 10177 */
   int			expr_start_line;
   int			expr_start_col;
#endif /* KEY Bug 10177 */
   boolean		found_name		= FALSE;
   int			il_idx_1;
   int			il_idx_2;
   int			ir_idx;
   boolean		matched_blk		= FALSE;
#ifdef KEY /* Bug 10177 */
   int			name_idx = 0;
   boolean		prev_clause_in_err;
   int			sh_idx = 0;
#else /* KEY Bug 10177 */
   int			name_idx;
   boolean		prev_clause_in_err;
   int			sh_idx;
#endif /* KEY Bug 10177 */


   TRACE (Func_Entry, "parse_else_stmt", NULL);

   check_for_vestigial_task_blks();

   /* ----------------------------------------------------------------------- */
   /* ELSE  (with no if-construct-name)					      */
   /* ----------------------------------------------------------------------- */

   if (LA_CH_VALUE == EOS) {
      matched_blk = STMT_LEGAL_IN_BLK(Else_Stmt, CURR_BLK);

      if (cif_flags & MISC_RECS) {
         cif_stmt_type_rec(TRUE, CIF_Else_Stmt, statement_number);
      }
   }

   else if (MATCHED_TOKEN_CLASS(Tok_Class_Keyword)) {

      /* -------------------------------------------------------------------- */
      /* ELSE IF, or ELSEWHERE, or ELSE with if-construct-name                */
      /* -------------------------------------------------------------------- */

      if (TOKEN_VALUE(token) == Tok_Kwd_If  &&  LA_CH_VALUE == LPAREN) {

         /* ----------------------------------------------------------------- */
         /* ELSE IF							      */
         /* ----------------------------------------------------------------- */

	 blk_type			= If_Else_If_Blk;
         stmt_type			= Else_If_Stmt;
         SH_STMT_TYPE(curr_stmt_sh_idx)	= Else_If_Stmt;

         NEXT_LA_CH;

         expr_start_line = LA_CH_LINE;
         expr_start_col  = LA_CH_COLUMN;

         if (parse_expr(&cond_expr)) {
            
            if (LA_CH_VALUE == RPAREN) {
               NEXT_LA_CH;

               if (matched_specific_token(Tok_Kwd_Then, Tok_Class_Keyword)) {
               
                  if (LA_CH_VALUE != EOS) {
      
                     if ( (found_name = MATCHED_TOKEN_CLASS(Tok_Class_Id)) ) {
                     
                        if (LA_CH_VALUE != EOS) {
                           parse_err_flush(Find_EOS, EOS_STR);
                        }
                     }
                     else {
                        parse_err_flush(Find_EOS, "if-construct-name");
                     }
                  }
               }
               else {
                  parse_err_flush(Find_EOS, "THEN");
               }
            }
            else {
               parse_err_flush(Find_EOS, ")");
            }
         }
         else {
            parse_err_flush(Find_EOS, NULL);
         }

         if (SH_ERR_FLG(curr_stmt_sh_idx)) {
            goto EXIT;
         }

         if (cif_flags & MISC_RECS) {
            cif_stmt_type_rec(TRUE, CIF_Else_If_Stmt, statement_number);
         }
    
         if (STMT_LEGAL_IN_BLK(Else_If_Stmt, CURR_BLK)) {
            matched_blk	= (found_name) ? (CURR_BLK_NAME != NULL_IDX  &&
                                 (compare_names(TOKEN_ID(token).words,
                                             TOKEN_LEN(token),
                                             AT_OBJ_NAME_LONG(CURR_BLK_NAME),
                                             AT_NAME_LEN(CURR_BLK_NAME)) == 0)):
                                 TRUE;
         }
      }
      else if (TOKEN_VALUE(token) == Tok_Kwd_Where  &&  LA_CH_VALUE == LPAREN) {
         /* ----------------------------------------------------------------- */
         /* ELSE WHERE (mask) [construct_name]                                */
         /* ----------------------------------------------------------------- */

         blk_type                       = Where_Else_Mask_Blk;
         stmt_type                      = Else_Where_Mask_Stmt;
         SH_STMT_TYPE(curr_stmt_sh_idx) = Else_Where_Mask_Stmt;

         NTR_IR_TBL(ir_idx);
         IR_OPR(ir_idx) = Else_Where_Mask_Opr;
         SH_IR_IDX(curr_stmt_sh_idx) = ir_idx;
         IR_LINE_NUM(ir_idx) = TOKEN_LINE(token);
         IR_COL_NUM(ir_idx) = TOKEN_COLUMN(token);
         IR_TYPE_IDX(ir_idx) = TYPELESS_DEFAULT_TYPE;

         NEXT_LA_CH;

         expr_start_line = LA_CH_LINE;
         expr_start_col  = LA_CH_COLUMN;

         if (parse_expr(&cond_expr)) {

            COPY_OPND(IR_OPND_L(ir_idx), cond_expr);

            if (LA_CH_VALUE == RPAREN) {
               NEXT_LA_CH;

               if (LA_CH_VALUE != EOS) {

                  if ( (found_name = MATCHED_TOKEN_CLASS(Tok_Class_Id)) ) {

                     if (LA_CH_VALUE != EOS) {
                        parse_err_flush(Find_EOS, EOS_STR);
                     }
                  }
                  else {
                     parse_err_flush(Find_EOS, "where-construct-name");
                  }
               }
            }
            else {
               parse_err_flush(Find_EOS, ")");
            }
         }
         else {
            parse_err_flush(Find_EOS, NULL);
         }

         if (SH_ERR_FLG(curr_stmt_sh_idx)) {
            goto EXIT;
         }

         if (cif_flags & MISC_RECS) {
            cif_stmt_type_rec(TRUE, CIF_Elsewhere_Stmt, statement_number);
         }

         if (STMT_LEGAL_IN_BLK(Else_Where_Mask_Stmt, CURR_BLK)) {
            SH_PARENT_BLK_IDX(curr_stmt_sh_idx) = CURR_BLK_FIRST_SH_IDX;

            matched_blk = (found_name) ? (CURR_BLK_NAME != NULL_IDX  &&
                                 (compare_names(TOKEN_ID(token).words,
                                             TOKEN_LEN(token),
                                             AT_OBJ_NAME_LONG(CURR_BLK_NAME),
                                             AT_NAME_LEN(CURR_BLK_NAME)) == 0)):
                                 TRUE;
         }
      }
      else if (TOKEN_VALUE(token) == Tok_Kwd_Where &&
               (CURR_BLK == Where_Then_Blk ||
                CURR_BLK == Where_Else_Mask_Blk)) {

         /* -------------------------------------------------------------- */
         /* ELSEWHERE [construct_name]                                     */
         /* -------------------------------------------------------------- */

         stmt_type			= Else_Where_Stmt;
         SH_STMT_TYPE(curr_stmt_sh_idx)	= Else_Where_Stmt;
         blk_type			= Where_Else_Blk;
         SH_PARENT_BLK_IDX(curr_stmt_sh_idx) = CURR_BLK_FIRST_SH_IDX;

         NTR_IR_TBL(ir_idx);
         IR_OPR(ir_idx) = Else_Where_Opr;
         SH_IR_IDX(curr_stmt_sh_idx) = ir_idx;
         IR_LINE_NUM(ir_idx) = TOKEN_LINE(token);
         IR_COL_NUM(ir_idx) = TOKEN_COLUMN(token);
         IR_TYPE_IDX(ir_idx) = TYPELESS_DEFAULT_TYPE;


         if (LA_CH_VALUE != EOS) {

            if ( (found_name = MATCHED_TOKEN_CLASS(Tok_Class_Id)) ) {

               if (LA_CH_VALUE != EOS) {
                  parse_err_flush(Find_EOS, EOS_STR);
               }
            }
            else {
               parse_err_flush(Find_EOS, "where-construct-name");
            }
         }

         matched_blk = (found_name) ? (CURR_BLK_NAME != NULL_IDX  &&
                              (compare_names(TOKEN_ID(token).words,
                                          TOKEN_LEN(token),
                                          AT_OBJ_NAME_LONG(CURR_BLK_NAME),
                                          AT_NAME_LEN(CURR_BLK_NAME)) == 0)):
                              TRUE;

         if (cif_flags & MISC_RECS) {
            cif_stmt_type_rec(TRUE, CIF_Elsewhere_Stmt, statement_number);
         }
      }
      else if (TOKEN_VALUE(token) == Tok_Kwd_Where && LA_CH_VALUE == EOS) {

         if (STMT_LEGAL_IN_BLK(Else_Stmt, CURR_BLK)  &&
             (CURR_BLK_NAME != NULL_IDX  &&
              (compare_names(TOKEN_ID(token).words,
                             TOKEN_LEN(token),
                             AT_OBJ_NAME_LONG(CURR_BLK_NAME),
                             AT_NAME_LEN(CURR_BLK_NAME)) == 0)) ) {

            /* -------------------------------------------------------------- */
            /* ELSE with if-construct-name WHERE                              */
            /* -------------------------------------------------------------- */

            matched_blk	= TRUE; 
            found_name	= TRUE;

            if (cif_flags & MISC_RECS) {
               cif_stmt_type_rec(TRUE, CIF_Else_Stmt, statement_number);
            }
         }
         else if (match_blk(Where_Then_Blk, FALSE) >=
                  match_blk(If_Blk, FALSE)) {

            /* We got to this "else if" due to an error case:  either the ELSE*/
            /* is not valid in the current block, or the ELSE is valid but    */
            /* WHERE does not match the if-construct-name on the if-then-stmt.*/
            /* Reaching this point means match_blk found a Where_Then_Blk     */
            /* before an If_Blk, or neither a matching If_Blk nor a matching  */
            /* Where_Then_Blk was found.				      */

            stmt_type				= Else_Where_Stmt;
            SH_STMT_TYPE(curr_stmt_sh_idx)	= Else_Where_Stmt;
            blk_type				= Where_Else_Blk;

            if (cif_flags & MISC_RECS) {
               cif_stmt_type_rec(TRUE, CIF_Elsewhere_Stmt, statement_number);
            }
         }
         else {

            /* We've found a matching if-then-stmt but it either has no if-   */
            /* construct-name, or it does but it's not WHERE.                 */

            found_name = TRUE;

            if (cif_flags & MISC_RECS) {
               cif_stmt_type_rec(TRUE, CIF_Else_Stmt, statement_number);
            }
         }
      }
      else {

         /* ----------------------------------------------------------------- */
         /* ELSE statement with construct-name starting with WHERE or IF.     */
         /* ----------------------------------------------------------------- */

         found_name = TRUE;

         if (TOKEN_VALUE(token) != Tok_Id) {    /* In case its WHEREABC */
            reset_lex(TOKEN_BUF_IDX(token), TOKEN_STMT_NUM(token));
            MATCHED_TOKEN_CLASS(Tok_Class_Id);
         }

         if (CURR_BLK == If_Then_Blk  ||  CURR_BLK == If_Else_If_Blk) {
            matched_blk	= CURR_BLK_NAME != NULL_IDX  &&
                               (compare_names(TOKEN_ID(token).words,
                                              TOKEN_LEN(token),
                                              AT_OBJ_NAME_LONG(CURR_BLK_NAME),
                                              AT_NAME_LEN(CURR_BLK_NAME)) == 0);
         }

         if (cif_flags & MISC_RECS) {
            cif_stmt_type_rec(TRUE, CIF_Else_Stmt, statement_number);
         }

	 if (LA_CH_VALUE != EOS) {
            parse_err_flush(Find_EOS, EOS_STR);
	 }
      }
   }
   else {
      parse_err_flush(Find_EOS, "IF, WHERE, a construct name or EOS");
   }

   /* ----------------------------------------------------------------------- */
   /* If the ELSE, ELSE IF, or ELSEWHERE is in a valid location, adjust the   */
   /* block stack and generate the IR (actually for the previous clause) to   */
   /* jump around the clause currently being processed.  Otherwise, recover   */
   /* from the misplaced statement.				              */
   /* ----------------------------------------------------------------------- */

   if (matched_blk) {
      curr_stmt_category = Executable_Stmt_Cat;

      if ((cif_flags & XREF_RECS)  &&  found_name) {
         cif_usage_rec(CURR_BLK_NAME, AT_Tbl_Idx,
		       TOKEN_LINE(token), TOKEN_COLUMN(token),
		       CIF_Construct_Name_Reference);
      }

      if (blk_type != Where_Else_Blk  &&  
          blk_type != Where_Else_Mask_Blk &&
          ! SH_ERR_FLG(curr_stmt_sh_idx)) {

         /* If we are currently processing the ELSE stmt or the first ELSE IF */
         /* stmt of this IF construct, grab the branch-around label from the  */
         /* If_Blk block stack entry and replace it with the label that will  */
         /* be used by all following clauses for the "end IF" label.  For an  */
         /* ELSE, this just ends up being its branch-around label.	      */
         /*								      */
         /* Otherwise, we are processing the second (or beyond) ELSE IF or an */
         /* ELSE following such an ELSE IF.  Since the ELSE IF/ELSE currently */
         /* being processed has not yet been pushed on the block stack, the   */
         /* current block stack entry is for the previous ELSE IF.  Thus, the */
         /* label being plucked out of the block stack entry is the branch-   */
         /* around label for the previous ELSE IF which defines the beginning */
         /* of the current ELSE IF or ELSE clause.		              */

# ifdef _DEBUG
         if (BLK_TYPE(blk_stk_idx - 1) != If_Blk) {
            PRINTMSG(stmt_start_line, 160, Internal, 0, blk_stk_idx);
         }
# endif


      }

      prev_clause_in_err = SH_ERR_FLG(CURR_BLK_FIRST_SH_IDX);

      if (blk_type != Where_Else_Blk &&
          blk_type != Where_Else_Mask_Blk) {
         blk_idx = blk_stk_idx;

         if (CURR_BLK == If_Then_Blk) {
            --blk_idx;
         }

         sh_idx = BLK_FIRST_SH_IDX(blk_idx);
      }

      if (blk_type == Where_Else_Blk ||
          blk_type == Where_Else_Mask_Blk) {
         name_idx = CURR_BLK_NAME;
      }

      POP_BLK_STK;
      PUSH_BLK_STK(blk_type);
      CURR_BLK_FIRST_SH_IDX = curr_stmt_sh_idx;

      if (blk_type == Where_Else_Blk ||
          blk_type == Where_Else_Mask_Blk) {
         CURR_BLK_NAME = name_idx;
         CURR_BLK_NO_EXEC = TRUE;
      }
      else {
         CURR_BLK_NAME = BLK_NAME(blk_stk_idx - 1);
         LINK_TO_PARENT_BLK;

         if (SH_ERR_FLG(SH_PARENT_BLK_IDX(curr_stmt_sh_idx))  ||
             prev_clause_in_err) {
            SH_ERR_FLG(curr_stmt_sh_idx) = TRUE;
         }
         
         /* If the ELSE IF (and IF) are OK, generate a Br_True IR to hold the */
         /* conditional expression and branch-around label.  The .NOT.        */
         /* subtree is added by the Semantics Pass if the expression is OK.   */
         /* Since the SH_PARENT_BLK_IDX field is needed to link to actual     */
         /* parent blocks for invalid label reference detection and since     */
         /* there is no more room in an SH for another SH index, we need to   */
         /* hold the "previous IF construct part" SH index elsewhere.  For an */
         /* ELSE IF, generate a Br_True IR and attach two ILs to the right    */
         /* operand.  Save the "previous IF construct part" SH index in the   */
         /* second IL.  The Semantics Pass will put the branch-around label   */
         /* in the first IL.  For an ELSE, generate an If IR and save the SH  */
         /* index in the left operand.					      */
 
         if (! SH_ERR_FLG(curr_stmt_sh_idx)) {
            NTR_IR_TBL(ir_idx);
            SH_IR_IDX(curr_stmt_sh_idx) = ir_idx;
            IR_TYPE_IDX(ir_idx)         = LOGICAL_DEFAULT_TYPE;
            IR_LINE_NUM(ir_idx)         = expr_start_line;
            IR_COL_NUM(ir_idx)          = expr_start_col;

            if (blk_type == If_Else_If_Blk) {
               IR_OPR(ir_idx)              = Br_True_Opr;
               COPY_OPND(IR_OPND_L(ir_idx), cond_expr);
               NTR_IR_LIST_TBL(il_idx_1);
               IR_LIST_CNT_R(ir_idx) = 1;
               IR_FLD_R(ir_idx)      = IL_Tbl_Idx;
               IR_IDX_R(ir_idx)      = il_idx_1;
               NTR_IR_LIST_TBL(il_idx_2);
               IR_LIST_CNT_R(ir_idx) = 2;
               IL_NEXT_LIST_IDX(il_idx_1) = il_idx_2;
               IL_PREV_LIST_IDX(il_idx_2) = il_idx_1;
               IL_LINE_NUM(il_idx_2)      = stmt_start_line;
               IL_COL_NUM(il_idx_2)       = stmt_start_col;
               IL_FLD(il_idx_2)           = SH_Tbl_Idx;
               IL_IDX(il_idx_2)           = sh_idx;
            }
            else {
               IR_OPR(ir_idx)        = Else_Opr;
               IR_LINE_NUM(ir_idx)   = stmt_start_line;
               IR_COL_NUM(ir_idx)    = stmt_start_col;
               IR_FLD_L(ir_idx)      = SH_Tbl_Idx;
               IR_IDX_L(ir_idx)      = sh_idx;
            }
           

         }
      }
   }
   else {
      err_blk = (blk_type == Where_Else_Blk ||
                 blk_type == Where_Else_Mask_Blk) ? Where_Then_Blk : If_Blk;
      blk_idx = blk_match_err(err_blk, found_name, FALSE);

      /* blk_match_err moves the matched block to the current block */

      if (blk_idx != NULL_IDX) {
         if (blk_type == Where_Else_Blk ||
             blk_type == Where_Else_Mask_Blk) {
            name_idx = CURR_BLK_NAME;
         }

         POP_BLK_STK;
         PUSH_BLK_STK(blk_type);
         CURR_BLK_FIRST_SH_IDX = curr_stmt_sh_idx;

         if (blk_type == Where_Else_Blk ||
             blk_type == Where_Else_Mask_Blk) {
            CURR_BLK_NAME = name_idx;
            CURR_BLK_NO_EXEC = TRUE;
         }
         else {
            CURR_BLK_NAME = BLK_NAME(blk_stk_idx - 1);
            LINK_TO_PARENT_BLK;
            SH_ERR_FLG(curr_stmt_sh_idx) = TRUE;
         }

      }
   }

EXIT:

   NEXT_LA_CH;
   
   TRACE (Func_Exit, "parse_else_stmt", NULL);

   return;

}  /* parse_else_stmt */


/******************************************************************************\
|*									      *|
|* Description:								      *|
|*      Parse the EXIT statement.  If a matching DO construct is found,       *|
|*      generate the internal exit point label and a Br_Uncond IR to jump to  *|
|*      it.  								      *|
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

void parse_exit_stmt (void)

{
   int		blk_idx;
   boolean	fnd_name	= FALSE;
   int		ir_idx;


   TRACE (Func_Entry, "parse_exit_stmt", NULL);

   if (LA_CH_VALUE != EOS) {
      fnd_name = MATCHED_TOKEN_CLASS(Tok_Class_Id);

      if (fnd_name) {

         if (LA_CH_VALUE != EOS) {
            parse_err_flush(Find_EOS, EOS_STR);
         }

      }
      else {
         parse_err_flush(Find_EOS, "construct-name or EOS");
         goto EXIT;
      }

   }

   blk_idx = match_blk(Do_Blk, fnd_name);

   if (blk_idx != NULL_IDX) {

      if (! CURR_BLK_NO_EXEC  ||  ! iss_blk_stk_err()) {
         curr_stmt_category = Executable_Stmt_Cat;
         SH_P2_SKIP_ME(curr_stmt_sh_idx) = TRUE;
         BLK_EXIT_STMT(blk_idx) = TRUE;

         NTR_IR_TBL(ir_idx);
         SH_IR_IDX(curr_stmt_sh_idx) = ir_idx;
         IR_OPR(ir_idx)              = Br_Uncond_Opr;
         /* LRR - bhj put in short typeless as type idx */
         IR_TYPE_IDX(ir_idx)         = TYPELESS_DEFAULT_TYPE;
         IR_LINE_NUM(ir_idx)         = stmt_start_line;
         IR_COL_NUM(ir_idx)          = stmt_start_col;
         IR_LINE_NUM_R(ir_idx)       = stmt_start_line;
         IR_COL_NUM_R(ir_idx)        = stmt_start_col;
         IR_FLD_R(ir_idx)            = AT_Tbl_Idx;
         IR_IDX_R(ir_idx)            = gen_loop_lbl_name(blk_idx, Exit_Lbl);

         if (fnd_name  &&  (cif_flags & XREF_RECS)) {
            cif_usage_rec(BLK_NAME(blk_idx), AT_Tbl_Idx,
			  TOKEN_LINE(token), TOKEN_COLUMN(token),
			  CIF_Construct_Name_Reference);
         }
      }

   }
   else {
      PRINTMSG(stmt_start_line, 262, Error, stmt_start_col, "EXIT");
   }

EXIT:

   NEXT_LA_CH;
   
   TRACE (Func_Exit, "parse_exit_stmt", NULL);

   return;

}  /* parse_exit_stmt */

/******************************************************************************\
|*                                                                            *|
|* Description:                                                               *|
|*      This function parses both the FORALL construct statement and the      *|
|*      FORALL statement.  						      *|
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

void parse_forall (void)

{
   opnd_type    an_opnd;
#ifdef KEY /* Bug 10177 */
   int          blk_stk_start = 0;
#else /* KEY Bug 10177 */
   int          blk_stk_start;
#endif /* KEY Bug 10177 */
   int          expr_start_line;
   int          expr_start_col;
   int          forall_ir_idx;
   int          forall_sh_idx;
   boolean	found_colon;
   boolean	found_comma;
   int          i;
   int          il_idx;
   int		index_name_il_idx;
   int		mask_il_idx;
   int		stride_il_idx;
   int		subscript1_il_idx;
   int		subscript2_il_idx;
   int		triplet_spec_il_idx;


   TRACE (Func_Entry, "parse_forall", NULL);

   /* Generate the Forall IR.  If the FORALL turns out to be a FORALL         */
   /* construct, the left operand will eventually point at the END FORALL SH. */
   /* The right operand will point at an IL list.  If you think of the ILs    */
   /* in a horizontal list, there will be one IL for each		      */
   /* <forall-triplet-spec> and one (at the end of the chain) for the         */
   /* <scalar-mask-expr> (if such an expression is present).  Each of the ILs */
   /* for the triplet specs points at a chain of ILs (think of them as being  */
   /* vertical below the triplet spec ILs) that hold the info for the index   */
   /* name, both subscript expressions, and the stride expression.  If the    */
   /* user does not specify the stride, the compiler will supply the default  */
   /* 1 so the list for each triplet spec is always complete.		      */

   NTR_IR_TBL(forall_ir_idx);
   SH_IR_IDX(curr_stmt_sh_idx) = forall_ir_idx;
   IR_OPR(forall_ir_idx)       = Forall_Opr;
   IR_TYPE_IDX(forall_ir_idx)  = TYPELESS_DEFAULT_TYPE;
   IR_LINE_NUM(forall_ir_idx)  = stmt_start_line;
   IR_COL_NUM(forall_ir_idx)   = stmt_start_col;


   /* ----------------------------------------------------------------------- */
   /* Parse the <forall-header>:                                              */
   /*   ( <forall-triplet-spec-list> [ , <scalar-mask-expr> ] )               */
   /*									      */
   /*   <forall-triplet-spec>  is  <index-name> = <subscript> : <subscript>   */
   /*						    [ : <stride> ]            */
   /* ----------------------------------------------------------------------- */

   /* Look for the left paren and then the <index-name>.  If there is         */
   /* anything wrong, don't try to recover to the right paren (in the hopes   */
   /* of being able to parse the <forall-assignment-stmt> if this is a FORALL */
   /* stmt because the subscript, stride, and mask can all be expressions     */
   /* that could contain right parens.  Don't check here for the <index-name> */
   /* being a (unqualified) name.  Like the DO-variable check, this is done   */
   /* in semantic processing.  Also, don't check for the <index-name>         */
   /* duplicating an <index-name> of an outer FORALL until we know we've got  */
   /* a valid FORALL.							      */

   if (LA_CH_VALUE == LPAREN) {
      NEXT_LA_CH;

      if (MATCHED_TOKEN_CLASS(Tok_Class_Id)) {
         expr_start_line = TOKEN_LINE(token);
         expr_start_col  = TOKEN_COLUMN(token);

         if (parse_deref(&an_opnd, NULL_IDX)) {
            NTR_IR_LIST_TBL(triplet_spec_il_idx);
            IR_LIST_CNT_R(forall_ir_idx) = 1;
            IR_FLD_R(forall_ir_idx)      = IL_Tbl_Idx;
            IR_IDX_R(forall_ir_idx)      = triplet_spec_il_idx;

            NTR_IR_LIST_TBL(index_name_il_idx);
            IL_FLD(triplet_spec_il_idx) = IL_Tbl_Idx;
            IL_IDX(triplet_spec_il_idx) = index_name_il_idx;
            IL_LIST_CNT(triplet_spec_il_idx) = 1;

            COPY_OPND(IL_OPND(index_name_il_idx), an_opnd);
         }
         else {
            parse_err_flush(Find_EOS, NULL);
            goto EXIT;
         }
      }
      else {
         parse_err_flush(Find_EOS, "index-name");
         goto EXIT;
      }
   }
   else {
      parse_err_flush(Find_EOS, "(");
      goto EXIT;
   }

NEXT_TRIPLET_SPEC:

   if (LA_CH_VALUE == EQUAL) {
      NEXT_LA_CH;
   }
   else {
      parse_err_flush(Find_EOS, "=");
      goto EXIT;
   }


   /* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */
   /* Parse the first subscript expression.                                   */
   /* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

   /* Capture the beginning of the expression so the IL line and column will  */
   /* point at the beginning of the expression as opposed to the line and     */
   /* column from the root IR of the expression.  			      */

   expr_start_line = LA_CH_LINE;
   expr_start_col  = LA_CH_COLUMN;


   if (parse_expr(&an_opnd)) {

      /* Note:  parse_expr can return TRUE even though an error message is    */
      /* produced (like if the subscript is a constant with an invalid kind   */
      /* parameter).  This will cause the FORALL SH to be marked in error     */
      /* but that's OK.		                                              */

      if (! SH_ERR_FLG(curr_stmt_sh_idx)) {
         NTR_IR_LIST_TBL(subscript1_il_idx);
         IL_NEXT_LIST_IDX(index_name_il_idx) = subscript1_il_idx;
         IL_PREV_LIST_IDX(subscript1_il_idx) = index_name_il_idx;
         COPY_OPND(IL_OPND(subscript1_il_idx), an_opnd);
         IL_LINE_NUM(subscript1_il_idx)      = expr_start_line;
         IL_COL_NUM(subscript1_il_idx)       = expr_start_col;
         ++IL_LIST_CNT(triplet_spec_il_idx);
      }
      else {
         parse_err_flush(Find_EOS, NULL);
         goto EXIT;
      }
   }
   else {
      parse_err_flush(Find_EOS, NULL);
      goto EXIT;
   }

   
   if (LA_CH_VALUE == COLON) {
      NEXT_LA_CH;
   }
   else {
      parse_err_flush(Find_EOS, ":");
      goto EXIT;
   }


   /* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */
   /* Parse the second subscript expression.                                  */
   /* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

   expr_start_line = LA_CH_LINE;
   expr_start_col  = LA_CH_COLUMN;

   if (parse_expr(&an_opnd)) {

      if (! SH_ERR_FLG(curr_stmt_sh_idx)) {
         NTR_IR_LIST_TBL(subscript2_il_idx);
         IL_NEXT_LIST_IDX(subscript1_il_idx) = subscript2_il_idx;
         IL_PREV_LIST_IDX(subscript2_il_idx) = subscript1_il_idx;
         COPY_OPND(IL_OPND(subscript2_il_idx), an_opnd);
         IL_LINE_NUM(subscript2_il_idx)      = expr_start_line;
         IL_COL_NUM(subscript2_il_idx)       = expr_start_col;
         ++IL_LIST_CNT(triplet_spec_il_idx);
      }
      else {
         parse_err_flush(Find_EOS, NULL);
         goto EXIT;
      }
   }
   else {
      parse_err_flush(Find_EOS, NULL);
      goto EXIT;
   }

   
   /* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */
   /* If the stride expression is present, parse it.  Else supply the 1.      */
   /* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

   /* Set up some flags to allow us to issue reasonably specific error	      */
   /* recovery messages.						      */

   found_colon     = FALSE;
   found_comma     = FALSE;


   NTR_IR_LIST_TBL(stride_il_idx);
   IL_NEXT_LIST_IDX(subscript2_il_idx) = stride_il_idx;
   IL_PREV_LIST_IDX(stride_il_idx)     = subscript2_il_idx;
   ++IL_LIST_CNT(triplet_spec_il_idx);

   if (LA_CH_VALUE == COLON) {
      NEXT_LA_CH;
      found_colon = TRUE;

      expr_start_line = LA_CH_LINE;
      expr_start_col  = LA_CH_COLUMN;

      if (parse_expr(&an_opnd)) {

         if (! SH_ERR_FLG(curr_stmt_sh_idx)) {
            COPY_OPND(IL_OPND(stride_il_idx), an_opnd);
            IL_LINE_NUM(stride_il_idx) = expr_start_line;
            IL_COL_NUM(stride_il_idx)  = expr_start_col;
         }
         else {
            parse_err_flush(Find_EOS, NULL);
            goto EXIT;
         }
      }
      else {
         parse_err_flush(Find_EOS, NULL);
         goto EXIT;
      }
   }
   else {
      IL_LINE_NUM(stride_il_idx) = LA_CH_LINE;
      IL_COL_NUM(stride_il_idx)  = LA_CH_COLUMN;
      IL_FLD(stride_il_idx)      = CN_Tbl_Idx;
      IL_IDX(stride_il_idx)      = CN_INTEGER_ONE_IDX;
   }


   /* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */
   /* If the next token is a comma, figure out if it is followed by another   */
   /* <forall-triplet-spec> or the <scalar-mask-expr>.  The only way to tell  */
   /* is if an equal sign follows whatever follows the comma.  Nice language  */
   /* design, eh?							      */
   /* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

   if (LA_CH_VALUE == COMMA) {
      NEXT_LA_CH;
      found_colon = TRUE;      /* Fake this if FALSE so insert is meaningful. */
      found_comma = TRUE;

      expr_start_line = LA_CH_LINE;
      expr_start_col  = LA_CH_COLUMN;


      /* If the next token is a valid <index-name> or a valid token to start  */
      /* a mask expr, reset the source location so parse_expr will start at   */
      /* the right place (so LA_CH is set up correctly).  Else issue a        */
      /* message and bail now.				                      */

      if (MATCHED_TOKEN_CLASS(Tok_Class_Opnd)) {
         reset_lex(TOKEN_BUF_IDX(token), TOKEN_STMT_NUM(token));
      }
      else if (MATCHED_TOKEN_CLASS(Tok_Class_Op)) {
         reset_lex(TOKEN_BUF_IDX(token), TOKEN_STMT_NUM(token));
      }
#ifdef KEY /* Bug 8564 */
      else if (LA_CH_VALUE == LPAREN) {
         reset_lex(TOKEN_BUF_IDX(token), TOKEN_STMT_NUM(token));
      }
#endif /* KEY Bug 8564 */
      else {
         parse_err_flush(Find_EOS, "index-name or mask expression");
         goto EXIT;
      }

      if (parse_expr(&an_opnd)) {

         if (! SH_ERR_FLG(curr_stmt_sh_idx)) {

            if (LA_CH_VALUE == EQUAL) {
               il_idx = triplet_spec_il_idx;
               NTR_IR_LIST_TBL(triplet_spec_il_idx);
               ++IR_LIST_CNT_R(forall_ir_idx);
               IL_NEXT_LIST_IDX(il_idx)              = triplet_spec_il_idx;
               IL_PREV_LIST_IDX(triplet_spec_il_idx) = il_idx;

               NTR_IR_LIST_TBL(index_name_il_idx);
               IL_FLD(triplet_spec_il_idx) = IL_Tbl_Idx;
               IL_IDX(triplet_spec_il_idx) = index_name_il_idx;
               IL_LIST_CNT(triplet_spec_il_idx) = 1;

               COPY_OPND(IL_OPND(index_name_il_idx), an_opnd);

               goto NEXT_TRIPLET_SPEC;
            }
            else {
               NTR_IR_LIST_TBL(mask_il_idx);
               ++IR_LIST_CNT_R(forall_ir_idx);
               IL_NEXT_LIST_IDX(triplet_spec_il_idx) = mask_il_idx;
               IL_PREV_LIST_IDX(mask_il_idx) = triplet_spec_il_idx;

               COPY_OPND(IL_OPND(mask_il_idx), an_opnd);
               IL_LINE_NUM(mask_il_idx) = expr_start_line;
               IL_COL_NUM(mask_il_idx)  = expr_start_col;
            }
         }
         else {
            parse_err_flush(Find_EOS, NULL);
            goto EXIT;
         }
      }
      else {
         parse_err_flush(Find_EOS, NULL);
         goto EXIT;
      }
   }


   if (LA_CH_VALUE == RPAREN) {
      NEXT_LA_CH;
   }
   else {

      if (! found_colon) {
         parse_err_flush(Find_EOS, "colon, comma, or right parenthesis"); 
      }
      else if (! found_comma) {
         parse_err_flush(Find_EOS, "comma or )"); 
      }
      else if (OPND_FLD(an_opnd) == AT_Tbl_Idx) {
         parse_err_flush(Find_EOS, "= or )"); 
      }
      else {
         parse_err_flush(Find_EOS, ")"); 
      }

      goto EXIT;
   }

   forall_sh_idx = curr_stmt_sh_idx;

   if (LA_CH_VALUE == EOS) {
      NEXT_LA_CH;

      /* The FORALL is a FORALL construct.  Push it onto the Blocking Stmt    */
      /* stack.  We'll use common code later to search the stack for its      */
      /* parent (if there is one).  Set CURR_BLK_NO_EXEC to TRUE to force     */
      /* other stmt parsers to call iss_blk_stk_err to check to see if the    */
      /* stmt is allowed in a FORALL construct.				      */

      if (! CURR_BLK_NO_EXEC  ||  CURR_BLK == Forall_Blk) {
         PUSH_BLK_STK(Forall_Blk);
      }
      else {
         iss_blk_stk_err();
         PUSH_BLK_STK(Forall_Blk);
         CURR_BLK_ERR = TRUE;
      }

      if (stmt_construct_idx != NULL_IDX) {
         CURR_BLK_NAME                        = stmt_construct_idx;
         ATL_CLASS(stmt_construct_idx)        = Lbl_Construct;
         ATL_DEF_STMT_IDX(stmt_construct_idx) = curr_stmt_sh_idx;
         SH_ERR_FLG(curr_stmt_sh_idx)         =
            SH_ERR_FLG(curr_stmt_sh_idx)  ||  AT_DCL_ERR(stmt_construct_idx);
         stmt_construct_idx                        = NULL_IDX;
      }

      curr_stmt_category    = Executable_Stmt_Cat;
      CURR_BLK_FIRST_SH_IDX = curr_stmt_sh_idx;
      CURR_BLK_NO_EXEC      = TRUE;
      blk_stk_start         = blk_stk_idx - 1;
   }
   else {

      /* The tokens following the right paren are assumed to be a	      */
      /* <forall-assignment-stmt>.  Call MATCHED_TOKEN_CLASS to force the     */
      /* current token to be the first token of the FORALL assignment stmt.   */
      /* Parse the assignment stmt.  determine_stmt_type fills in the stmt    */
      /* type, line number, and column number.				      */
 
      stmt_type                      = Forall_Stmt;
      SH_STMT_TYPE(curr_stmt_sh_idx) = Forall_Stmt;

      if (! CURR_BLK_NO_EXEC  ||  CURR_BLK == Forall_Blk) {
         curr_stmt_category = Executable_Stmt_Cat;
      }
      else {
         iss_blk_stk_err();
         parse_err_flush(Find_EOS, NULL);
         goto EXIT;
      }


      if (MATCHED_TOKEN_CLASS(Tok_Class_Keyword)) {
         gen_sh(After, Null_Stmt, 0, 0, FALSE, FALSE, FALSE);

         determine_stmt_type();

         if (stmt_type == Assignment_Stmt) {

            /* The action statement needs to have its statement */
            /* number be 1 greater than the if statement.       */

            /*  KAY - This needs to be checked to see if this is okay here */

            INCREMENT_STATEMENT_NUMBER;
            in_action_stmt_of_if_where_or_forall	= TRUE;
   
            (*stmt_parsers[Assignment_Stmt])();

            in_action_stmt_of_if_where_or_forall	= FALSE;

            if (SH_ERR_FLG(curr_stmt_sh_idx)) {
               SH_ERR_FLG(forall_sh_idx) = TRUE;
               goto EXIT;
            }

            blk_stk_start = blk_stk_idx;

            /* generate an end forall after the assignment */

            gen_sh(After, End_Forall_Stmt, SH_GLB_LINE(forall_sh_idx), 
                   SH_COL_NUM(forall_sh_idx), FALSE, FALSE, FALSE);

            SH_PARENT_BLK_IDX(curr_stmt_sh_idx) = forall_sh_idx;
            IR_FLD_L(SH_IR_IDX(forall_sh_idx)) = SH_Tbl_Idx;
            IR_IDX_L(SH_IR_IDX(forall_sh_idx)) = curr_stmt_sh_idx;
            IR_LINE_NUM_L(SH_IR_IDX(forall_sh_idx)) =
                                SH_GLB_LINE(forall_sh_idx);
            IR_COL_NUM_L(SH_IR_IDX(forall_sh_idx)) =
                                SH_COL_NUM(forall_sh_idx);

         }
         else {
            PRINTMSG(stmt_start_line, 365, Error, stmt_start_col);
            parse_err_flush(Find_EOS, NULL);
            NEXT_LA_CH;
            SH_ERR_FLG(forall_sh_idx) = TRUE;
         }
      }
      else {
         parse_err_flush(Find_EOS, "identifier or EOS");
         goto EXIT;
      }
   }


   /* Now look back through the Blocking Stmt stack to see if we can find an  */
   /* outer FORALL constructs.  If we find one, link the current FORALL to    */
   /* to the outer one so we can find it in the Semantics Pass.		      */

   for (i = blk_stk_start;  i > 0;  --i) {

      if (BLK_TYPE(i) == Forall_Blk) {
         SH_PARENT_BLK_IDX(forall_sh_idx) = BLK_FIRST_SH_IDX(i);
         break;
      }
   }

EXIT:

   if (LA_CH_VALUE == EOS) {
      NEXT_LA_CH;
   }

   TRACE (Func_Exit, "parse_forall", NULL);

   return;

}  /* parse_forall */

/******************************************************************************\
|*                                                                            *|
|* Description:                                                               *|
|*      This procedure parses the three GO TO statement forms:                *|
|*									      *|
|*        Unconditional:  GO TO label					      *|
|*									      *|
|*        Computed:       GO TO (label-list) [,] scalar-int-expr              *|
|*									      *|
|*        Assigned:       GO TO scalar-int-variable [ [,] (label-list) ]      *|
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

void	parse_goto_stmt(void)

{
   int 		attr_idx;
   int		col;
   boolean      comma_found		= FALSE;
   int		host_attr_idx;
   int		host_name_idx;
   int		ir_idx;
   boolean 	label_list_found	= FALSE;
   int		lbl_attr_idx;
   int		line;
   int		name_idx;
   opnd_type    opnd;


   TRACE (Func_Entry, "parse_goto_stmt", NULL);

   /* Have the keyword GO.  The next token must be TO.                        */
   /* Keep parsing to expose as many syntax errors as possible even though    */
   /* the statement might be out of context.  No labels will be checked       */
   /* (because error flags are set) in order to prevent cascading errors.     */

   line = TOKEN_LINE(token);
   col  = TOKEN_COLUMN(token);

   if (matched_specific_token(Tok_Kwd_To, Tok_Class_Keyword)) {
      if ( ! CURR_BLK_NO_EXEC  ||  ! iss_blk_stk_err() ) {
         curr_stmt_category = Executable_Stmt_Cat;
      }
   }
   else {
      parse_err_flush(Find_EOS, "TO");
      goto EXIT;
   }

   /* Let's rashly assume that the stmt is correctly formed and allocate      */
   /* the branch IR now so we have a place to hang things.                    */

   NTR_IR_TBL(ir_idx);
   SH_IR_IDX(curr_stmt_sh_idx) = ir_idx;
   IR_LINE_NUM(ir_idx) = line;
   IR_COL_NUM(ir_idx)  = col;


   /* ----------------------------------------------------------------------- */
   /* Unconditional GO TO?                          GO TO label               */
   /* ----------------------------------------------------------------------- */

   if (LA_CH_CLASS == Ch_Class_Digit) {

      MATCHED_TOKEN_CLASS(Tok_Class_Label);

      if ( ! TOKEN_ERR(token)) {
      
         if (LA_CH_VALUE == EOS) {

            /* We've found an unconditional GO TO.  Check the reference to    */
            /* the label and complete the Br_Uncond_Opr IR.                   */

            lbl_attr_idx = check_label_ref();

            IR_OPR(ir_idx)        = Br_Uncond_Opr;
            /* LRR - bhj put in short typeless as type idx */
            IR_TYPE_IDX(ir_idx)   = TYPELESS_DEFAULT_TYPE;
            IR_LINE_NUM(ir_idx)   = TOKEN_LINE(token);
            IR_COL_NUM(ir_idx)    = TOKEN_COLUMN(token);
            IR_LINE_NUM_R(ir_idx) = TOKEN_LINE(token);
            IR_COL_NUM_R(ir_idx)  = TOKEN_COLUMN(token);
            IR_FLD_R(ir_idx)      = AT_Tbl_Idx;
            IR_IDX_R(ir_idx)      = lbl_attr_idx;
            AT_REFERENCED(lbl_attr_idx) = Referenced;

            if (cif_flags & MISC_RECS) {
               cif_stmt_type_rec(TRUE, 
                                 CIF_Go_To_Unconditional_Stmt, 
                                 statement_number);
            }
         }
         else {

            /* Have garbage at the end of the statement.                      */

            parse_err_flush(Find_EOS, EOS_STR);
         }
      }
      else {
         
         /* Something is wrong with the label (like it's too long).  Message  */
         /* has already been issued.  Flush.                                  */

         parse_err_flush(Find_EOS, NULL);
      }

      goto EXIT;
   }


   /* ----------------------------------------------------------------------- */
   /* Computed GO TO?                GO TO (label-list) [,] scalar-int-expr   */
   /* ----------------------------------------------------------------------- */

   if (LA_CH_VALUE == LPAREN) {
      
      IR_OPR(ir_idx) = Br_Index_Opr;
      /* LRR - bhj put in integer as type idx */
      IR_TYPE_IDX(ir_idx)         = INTEGER_DEFAULT_TYPE;
     
      /* Parse the label-list.  If bad, the label-list parser has already     */
      /* issued messages so give up.					      */

      if (parse_label_list(ir_idx)) {
#ifdef KEY /* Bug 318, 321 */
	 PRINTMSG(stmt_start_line, 1568, Ansi, stmt_start_col, "computed GOTO");
#endif /* KEY Bug 318, 321 */

         /* Recognize and toss the comma following the label-list, if the     */
         /* comma exists.				     		      */

         if (LA_CH_VALUE == COMMA) {
            NEXT_LA_CH;
         }

         /* Parse the expression.  The Semantics Pass will verify that it's   */
         /* scalar and type integer.   					      */

         if (parse_expr(&opnd)) {

            COPY_OPND(IR_OPND_L(ir_idx), opnd);

            if (LA_CH_VALUE != EOS) {

               /* Have garbage at the end of the statement.                   */

               parse_err_flush(Find_EOS, EOS_STR);
            }
         }
         else {
            parse_err_flush(Find_EOS, NULL);
         }
      }

      if (cif_flags & MISC_RECS) {
         cif_stmt_type_rec(TRUE, CIF_Go_To_Computed_Stmt, statement_number);
      }

      goto EXIT;
   }
 

   /* ----------------------------------------------------------------------- */
   /* Must be an assigned GO TO.   GO TO scalar-int-var [ [,] (label-list) ]  */
   /* ----------------------------------------------------------------------- */

   if ( ! MATCHED_TOKEN_CLASS(Tok_Class_Id) ) {
      PRINTMSG(LA_CH_LINE, 339, Error, LA_CH_COLUMN,
               (LA_CH_CLASS == Ch_Class_EOS) ? "EOS" : &LA_CH_VALUE);
      parse_err_flush(Find_EOS, NULL);
      goto EXIT;
   }
 
   IR_OPR(ir_idx) = Br_Asg_Opr;
   /* LRR - bhj put in Integer as type idx */
   IR_TYPE_IDX(ir_idx)   = INTEGER_DEFAULT_TYPE;

   /* Duplicate the bare bones of parse_deref to find (or enter) the Attr     */
   /* entry and get it marked as being referenced.			      */
   /* parse_deref can't be used directly because it would try to parse the    */
   /* "lbl (2,3)"  in     GO TO lbl (2,3)    as an array element designator.  */

   attr_idx = srch_sym_tbl(TOKEN_STR(token), TOKEN_LEN(token), &name_idx); 

   if (attr_idx == NULL_IDX) {
      host_attr_idx =
         srch_host_sym_tbl(TOKEN_STR(token), 
                           TOKEN_LEN(token), 
                           &host_name_idx,
                           TRUE);
      
      if (host_attr_idx == NULL_IDX) {
         attr_idx = ntr_sym_tbl(&token, name_idx);
         SET_IMPL_TYPE(attr_idx);
      }
      else {
         attr_idx = ntr_host_in_sym_tbl(&token, name_idx,
                                        host_attr_idx, host_name_idx,
                                        TRUE);
      }
   }
 
   if (AT_DCL_ERR(attr_idx)) {
      parse_err_flush(Find_EOS, NULL);
      SH_ERR_FLG(curr_stmt_sh_idx) = TRUE;
      goto EXIT;
   }

   AT_REFERENCED(attr_idx) = Referenced;

   IR_FLD_L(ir_idx)      = AT_Tbl_Idx;
   IR_IDX_L(ir_idx)      = attr_idx;
   IR_LINE_NUM_L(ir_idx) = TOKEN_LINE(token);
   IR_COL_NUM_L(ir_idx)  = TOKEN_COLUMN(token);

   /* If the optional comma exists, absorb it and note its presence.          */

   if (LA_CH_VALUE == COMMA) {
      comma_found = TRUE;
      NEXT_LA_CH;
   }

   /* If the comma exists, the label-list must exist.  If the label-list      */
   /* exists, parse it.							      */

   if (LA_CH_VALUE == LPAREN) {
      label_list_found = TRUE;
      
      if ( ! parse_label_list(ir_idx) ) {
         goto EXIT;
      }
   }
   else {
      if (comma_found) {
         parse_err_flush(Find_EOS, "(");
         goto EXIT;
      }
   }

   /* If the next token is the EOS, we're home free.  If not, there are two   */
   /* possibilities:						              */
   /*  									      */
   /*   - If the label-list exists, then not finding the EOS means there is   */
   /*     junk at the end of the statement (following the label-list).        */
   /*									      */
   /*   - Otherwise, there is junk following the label variable.              */

   if (LA_CH_VALUE == EOS) {
      PRINTMSG(stmt_start_line, 1568, Ansi, stmt_start_col, "assign GOTO");
   }
   else {

      if (label_list_found) {
         parse_err_flush(Find_EOS, EOS_STR);
      }
      else {
         parse_err_flush(Find_EOS, ", or ( or EOS");
      }
  
   }

   if (cif_flags & MISC_RECS) {
      cif_stmt_type_rec(TRUE, CIF_Go_To_Assigned_Stmt, statement_number);
   }

EXIT:

   NEXT_LA_CH;

   TRACE (Func_Exit, "parse_goto_stmt", NULL);

   return;

}  /* parse_goto_stmt */


/******************************************************************************\
|*									      *|
|* Description:								      *|
|*	This function parses all forms of statements beginning with the key-  *|
|*      word IF:						 	      *|
|*               [if-construct-name:] IF (scalar-logical-expr) THEN	      *|
|*		 IF (scalar-logical-expr) action-stmt			      *|
|*               IF (scalar-numeric-expr) label, label, label  {obsolescent}  *|
|*		 IF (scalar-logical-expr) label, label         {outmoded}     *|
|*		 IF (scalar-numeric-expr) label, label         {outmoded}     *|
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
|* Algorithm notes:							      *|
|*   This procedure is called recursively to process the action-stmt of a     *|
|*   logical IF if the action-stmt is an arithmetic IF or one of the outmoded *|
|*   IF forms (or, erroneously, another logical IF or IF construct).          *|
|*									      *|
\******************************************************************************/

void parse_if_stmt (void)

{
   opnd_type 	cond_expr;
   int		expr_start_line;
   int		expr_start_col;
   int		if_stmt_sh_idx;
   int		ir_idx;
   int		lbl_attr_idx;
   int		neg_il_idx;
   int		pos_il_idx;
   int		save_if_stmt_start_line;
   int		save_if_stmt_start_col;
   int		sh_idx;
   boolean	stmt_parsers_called;
   int		zero_il_idx;


   TRACE (Func_Entry, "parse_if_stmt", NULL);

   /* ----------------------------------------------------------------------- */
   /* Parse "(conditional-expr)".					      */
   /* ----------------------------------------------------------------------- */

   if (LA_CH_VALUE == LPAREN) {
      NEXT_LA_CH;

      expr_start_line = LA_CH_LINE;
      expr_start_col  = LA_CH_COLUMN;

      parse_expr(&cond_expr);

      if (LA_CH_VALUE != RPAREN) {

         if ( ! parse_err_flush(Find_Rparen, ")") ) {
            parse_err_flush(Find_EOS, NULL);
            goto EXIT;
         }
      }
   }
   else {
      parse_err_flush(Find_EOS, "(");
      goto EXIT;
   }

   NEXT_LA_CH;


   /* ----------------------------------------------------------------------- */
   /* Now figure out what kind of IF we're parsing.                           */
   /* ----------------------------------------------------------------------- */

   if (MATCHED_TOKEN_CLASS(Tok_Class_Keyword)) {
      
      if (TOKEN_VALUE(token) == Tok_Kwd_Then) {
 
         if (LA_CH_VALUE != EOS) {

            /* Identifier starting with THEN or bad character following THEN? */

            if (LA_CH_VALUE == USCORE  ||  LA_CH_CLASS == Ch_Class_Letter  ||
                LA_CH_CLASS == Ch_Class_Digit) {
               reset_lex(TOKEN_BUF_IDX(token), TOKEN_STMT_NUM(token));
               MATCHED_TOKEN_CLASS(Tok_Class_Id);
               goto PARSE_LOGICAL_IF;
            }
            else if (LA_CH_VALUE == LPAREN  ||  LA_CH_VALUE == PERCENT  ||
                     LA_CH_VALUE == EQUAL) {
               goto PARSE_LOGICAL_IF;
            }
            else {

               /* Have THEN <bad-char>.  Let it go through so that a Block    */
               /* Stack entry will be created so a later END IF will have an  */
               /* entry to match.					      */
 
               parse_err_flush(Find_EOS, EOS_STR); 
            }
         }

         /* ----------------------------------------------------------------- */
         /* IF THEN of an IF construct				     	      */
         /* ----------------------------------------------------------------- */

         if (if_stmt_lbl_idx == NULL_IDX) {
  
            /* It's not the action-stmt of a logical IF (which would be an    */
            /* error).						              */

            if ( ! CURR_BLK_NO_EXEC  ||  ! iss_blk_stk_err() ) {
               curr_stmt_category = Executable_Stmt_Cat;
               PUSH_BLK_STK (If_Blk);
            }
            else {
               PUSH_BLK_STK (If_Blk);
               CURR_BLK_ERR = TRUE;
            }

            CURR_BLK_FIRST_SH_IDX = curr_stmt_sh_idx;
            LINK_TO_PARENT_BLK;

            if (stmt_construct_idx != NULL_IDX) {
               CURR_BLK_NAME		            = stmt_construct_idx;
               ATL_CLASS(stmt_construct_idx)        = Lbl_Construct;
               ATL_DEF_STMT_IDX(stmt_construct_idx) = curr_stmt_sh_idx;
               SH_ERR_FLG(curr_stmt_sh_idx)	    =
                  SH_ERR_FLG(curr_stmt_sh_idx) | AT_DCL_ERR(stmt_construct_idx);
               stmt_construct_idx		    = NULL_IDX;
            }

            /* Generate a Br_True IR to hold the conditional expression and   */
            /* branch-around label.  The .NOT. subtree is added by the        */
            /* Semantics Pass if the expression is OK.  The label's Attr entry*/
            /* fields are completed as a part of END IF processing.           */

            if ( ! SH_ERR_FLG(curr_stmt_sh_idx) ) {
               NTR_IR_TBL(ir_idx);
               SH_IR_IDX(curr_stmt_sh_idx) = ir_idx;
               IR_OPR(ir_idx)              = Br_True_Opr;
               IR_TYPE_IDX(ir_idx)         = LOGICAL_DEFAULT_TYPE;
               IR_LINE_NUM(ir_idx)         = expr_start_line;
               IR_COL_NUM(ir_idx)          = expr_start_col;
               COPY_OPND(IR_OPND_L(ir_idx), cond_expr);
            }

            /* Generate a Then_Stmt SH and block stack entry.  This is needed */
            /* now to check the validity of label references.  It'll be       */
            /* thrown away by the Semantics Pass.			      */

            gen_sh(After, Then_Stmt, TOKEN_LINE(token), TOKEN_COLUMN(token),
                   SH_ERR_FLG(curr_stmt_sh_idx), FALSE, FALSE);
 
            PUSH_BLK_STK(If_Then_Blk);
            blk_stk[blk_stk_idx]	      = blk_stk[blk_stk_idx - 1];
            CURR_BLK			      = If_Then_Blk;
            CURR_BLK_FIRST_SH_IDX             = curr_stmt_sh_idx;
            LINK_TO_PARENT_BLK;

            if (cif_flags & MISC_RECS) {
               cif_stmt_type_rec(TRUE, CIF_If_Construct, statement_number);
            }
         }
         else {

            /* The IF-THEN stmt of an IF construct can not be the action-stmt */
            /* of a logical IF.						      */
     
            PRINTMSG(stmt_start_line, 365, Error, stmt_start_col);
         }

         goto EXIT;

      }

      /* -------------------------------------------------------------------- */
      /* Must be parsing a logical IF.				      	      */
      /* Note that the identifier following the closing paren of the          */
      /* conditional expression might have started with the letters THEN      */
      /* (token scanning was reset above).				      */
      /*--------------------------------------------------------------------- */

PARSE_LOGICAL_IF:

      stmt_type		             = If_Stmt;
      SH_STMT_TYPE(curr_stmt_sh_idx) = If_Stmt;
      if_stmt_sh_idx 		     = curr_stmt_sh_idx;
      save_if_stmt_start_line        = stmt_start_line;
      save_if_stmt_start_col         = stmt_start_col;

      if ( ! CURR_BLK_NO_EXEC  ||  ! iss_blk_stk_err() ) {
         curr_stmt_category = Executable_Stmt_Cat;
      }

      if (if_stmt_lbl_idx == NULL_IDX) {
         if_stmt_lbl_idx = gen_internal_lbl(stmt_start_line);
      }
      else {
         PRINTMSG(stmt_start_line, 365, Error, stmt_start_col);
         parse_err_flush(Find_EOS, NULL);
         NEXT_LA_CH;
         goto EXIT;
      }

      if (cif_flags & MISC_RECS) {
         cif_stmt_type_rec(TRUE, CIF_If_Logical_Stmt, statement_number);
      }


      /* If the IF stmt to this point is OK, generate the Br_True IR to hold  */
      /* the index to the conditional expression tree (temporarily) and the   */
      /* index of the compiler-generated branch-around label.  This label is  */
      /* not needed for the high-level form of the IF but it's presence is    */
      /* used as a flag to determine whether or not an IF stmt is being       */
      /* parsed.  It is better to just not perturb the code and let the       */
      /* Semantics Pass ignore this label when in high-level IF mode.         */

      if ( ! SH_ERR_FLG(curr_stmt_sh_idx) ) {
         NTR_IR_TBL(ir_idx);
         SH_IR_IDX(if_stmt_sh_idx)      = ir_idx;
         IR_OPR(ir_idx)	                = Br_True_Opr;
         IR_TYPE_IDX(ir_idx)            = LOGICAL_DEFAULT_TYPE;
         IR_LINE_NUM(ir_idx)   	        = expr_start_line;
         IR_COL_NUM(ir_idx)	        = expr_start_col;

         COPY_OPND(IR_OPND_L(ir_idx), cond_expr);
         IR_LINE_NUM_R(ir_idx)          = expr_start_line;
         IR_COL_NUM_R(ir_idx) 	        = expr_start_col;
         IR_FLD_R(ir_idx)	        = AT_Tbl_Idx;
         IR_IDX_R(ir_idx)	        = if_stmt_lbl_idx;
         AT_REFERENCED(if_stmt_lbl_idx) = Referenced;
      }


      /* Now parse the action-stmt.					      */
      /* determine_stmt_type fills in the stmt type, line number, column      */
      /* number, and whether or not the statement is labeled.                 */

      gen_sh(After, Null_Stmt, 0, 0, FALSE, FALSE, FALSE);

      SH_ACTION_STMT(curr_stmt_sh_idx)	= TRUE;
      stmt_parsers_called		= FALSE;
 
      determine_stmt_type();

      if (stmt_type == Where_Cstrct_Stmt ||
          stmt_type == End_Stmt ||
          stmt_type == Forall_Cstrct_Stmt) {

         /* Where_Cstrct_Stmt                                       */
         /* Procedure determine_stmt_type does not distinguish      */
         /* between a WHERE stmt and a WHERE construct so we must   */
         /* actually parse the WHERE to determine what it is.       */

         /* End_Stmt                                                */
         /* The stmt has to be parsed because determine_stmt_type   */
         /* can't tell a real END statement from an ENDFILE stmt.   */

         /* Forall_Cstrct_Stmt                                      */
         /* Procedure determine_stmt_type does not distinguish      */
         /* between a FORALL stmt and a FORALL construct so we      */
         /* must actually parse the FORALL to determine what it is. */

         INCREMENT_STATEMENT_NUMBER;
         stmt_parsers_called			= TRUE;
         in_action_stmt_of_if_where_or_forall	= TRUE;
   
         (*stmt_parsers[stmt_type])();

         in_action_stmt_of_if_where_or_forall	= FALSE;
      }

      switch (stmt_type) {
         case Allocatable_Stmt:
         case Automatic_Stmt:
#ifdef KEY /* Bug 14150 */
         case Bind_Stmt:
#endif /* KEY Bug 14150 */
         case Common_Stmt:
         case Contains_Stmt:
         case Cpnt_Decl_Stmt:
         case Data_Stmt:
         case Derived_Type_Stmt:
         case Dimension_Stmt:
         case Directive_Stmt:
         case Equivalence_Stmt:
         case External_Stmt:
         case Format_Stmt:
         case Implicit_Stmt:
         case Implicit_None_Stmt:
#ifdef KEY /* Bug 11741 */
         case Import_Stmt:
#endif /* KEY Bug 11741 */
#ifdef KEY /* Bug 10572 */
         case Enum_Stmt:
         case Enumerator_Stmt:
#endif /* KEY Bug 10572 */
         case Intent_Stmt:
         case Interface_Stmt:
         case Intrinsic_Stmt:
         case Module_Proc_Stmt:
         case Namelist_Stmt:
         case Optional_Stmt:
         case Parameter_Stmt:
         case Pointer_Stmt:
         case Private_Stmt:
         case Public_Stmt:
         case Save_Stmt:
         case Sequence_Stmt:
         case Stmt_Func_Stmt:
         case Target_Stmt:
         case Task_Common_Stmt:
         case Type_Decl_Stmt:
         case Use_Stmt:
         case Volatile_Stmt:
         case Blockdata_Stmt:
         case Elemental_Stmt:
         case Function_Stmt:
         case Module_Stmt:
         case Program_Stmt:
         case Pure_Stmt:
         case Recursive_Stmt:
         case Subroutine_Stmt:
#ifdef KEY /* Bug 14150 */
	 case Value_Stmt:
#endif /* KEY Bug 14150 */

            PRINTMSG(stmt_start_line, 365, Error, stmt_start_col);
            parse_err_flush(Find_EOS, NULL);
            NEXT_LA_CH;
            SH_ERR_FLG(if_stmt_sh_idx)	= TRUE;
            stmt_type			= If_Stmt;
            break;

         case Where_Cstrct_Stmt:
         case Forall_Cstrct_Stmt:

            /* Pop the Block Stack (carefully) and mark all statement    */
            /* headers back to the IF statement header as being in error */
            /* so the Semantics Pass won't try to process them.		 */

            if (CURR_BLK == Where_Then_Blk) {
               POP_BLK_STK;
            }

            sh_idx = curr_stmt_sh_idx;

            while (SH_STMT_TYPE(sh_idx) != If_Stmt) {
               SH_ERR_FLG(sh_idx)	= TRUE;
               sh_idx			= SH_PREV_IDX(sh_idx);
            }

            /* Intentional fall through */

         case Case_Stmt:
         case Do_Iterative_Stmt:
         case Do_While_Stmt:
         case Do_Infinite_Stmt:
         case Else_Stmt:
         case Entry_Stmt:
         case Select_Stmt:

            PRINTMSG(stmt_start_line, 365, Error, stmt_start_col);

            if (stmt_type != Where_Cstrct_Stmt) {
               parse_err_flush(Find_EOS, NULL);
               NEXT_LA_CH;
            }

            SH_ERR_FLG(if_stmt_sh_idx)	= TRUE;
            stmt_type			= If_Stmt;
            goto EXIT;

         case Allocate_Stmt:
         case Arith_If_Stmt:
         case Assign_Stmt:
         case Assignment_Stmt:
         case Backspace_Stmt:
         case Buffer_Stmt:
         case Call_Stmt:
         case Close_Stmt:
         case Continue_Stmt:
         case Cycle_Stmt:
         case Deallocate_Stmt:
         case Decode_Stmt:
         case Else_If_Stmt:
         case Else_Where_Stmt:
         case Encode_Stmt:
         case Exit_Stmt:
         case Goto_Stmt:
         case If_Cstrct_Stmt:
         case If_Stmt:
         case Inquire_Stmt:
         case Nullify_Stmt:
         case Open_Stmt:
         case Outmoded_If_Stmt:
         case Pause_Stmt:
         case Print_Stmt:
         case Read_Stmt:
         case Return_Stmt:
         case Rewind_Stmt:
         case Stop_Stmt:
         case Then_Stmt:
         case Write_Stmt:
         case Endfile_Stmt:
         case Where_Stmt:
         case Forall_Stmt:


            if (!stmt_parsers_called) {

               /* The action statement needs to have its statement */
               /* number be 1 greater than the if statement.       */

               INCREMENT_STATEMENT_NUMBER;
               in_action_stmt_of_if_where_or_forall	= TRUE;
   
               (*stmt_parsers[stmt_type])();

               in_action_stmt_of_if_where_or_forall	= FALSE;
            }

            /* If the IF portion and the action-stmt are OK, generate a      */
            /* CONTINUE stmt to define the branch-around label.  Otherwise,  */
            /* mark the If_Stmt SH in error if the action-stmt is bad.       */
            /* Note:  The Semantics Pass will fill in AT_DEF_LINE and        */
            /* AT_DEF_COL for the label with the info from the stmt          */
            /* following the compiler-generated CONTINUE statement.          */
   
            gen_if_ir(if_stmt_lbl_idx, 
                      if_stmt_sh_idx,
                      save_if_stmt_start_line,
                      save_if_stmt_start_col);

            stmt_type = If_Stmt;
            break;

         case End_Stmt:
         case End_Blockdata_Stmt:
         case End_Do_Stmt:
         case End_Function_Stmt:
         case End_If_Stmt:
         case End_Interface_Stmt:
#ifdef KEY /* Bug 10572 */
         case End_Enum_Stmt:
#endif /* KEY Bug 10572 */
         case End_Module_Stmt:
         case End_Program_Stmt:
         case End_Select_Stmt:
         case End_Subroutine_Stmt:
         case End_Type_Stmt:
         case End_Where_Stmt:
         case End_Forall_Stmt:

            PRINTMSG(stmt_start_line, 365, Error, stmt_start_col);
            SH_ERR_FLG(if_stmt_sh_idx)  = TRUE;
            stmt_type = If_Stmt;
            break;

         case Null_Stmt:
         case Type_Init_Stmt:
         case Label_Def:
         case Construct_Def:
         case Automatic_Base_Calc_Stmt:
         case Automatic_Base_Size_Stmt:
         case End_Parallel_Stmt:
         case End_Do_Parallel_Stmt:
         case End_Parallel_Case_Stmt:
         case Parallel_Case_Stmt:
         case End_Guard_Stmt:
         case Statement_Num_Stmt:
         case SGI_Section_Stmt:
         case SGI_End_Psection_Stmt:
         case SGI_End_Pdo_Stmt:
         case SGI_End_Parallel_Stmt:
         case SGI_End_Critical_Section_Stmt:
         case SGI_End_Single_Process_Stmt:
         case SGI_Region_End_Stmt:
         case Open_MP_Section_Stmt:
         case Open_MP_End_Parallel_Stmt:
         case Open_MP_End_Do_Stmt:
         case Open_MP_End_Parallel_Sections_Stmt:
         case Open_MP_End_Sections_Stmt:
         case Open_MP_End_Section_Stmt:
         case Open_MP_End_Single_Stmt:
         case Open_MP_End_Parallel_Do_Stmt:
         case Open_MP_End_Master_Stmt:
         case Open_MP_End_Critical_Stmt:
         case Open_MP_End_Ordered_Stmt:
     
# if defined(_DEBUG)
                
            /* ELSE IF and ELSE WHERE should never be seen because they    */
	    /* should be classified as ELSE.		                   */
            /* stmt_type = If_Cstrct_Stmt should never be seen because it  */
            /* will be caught above in IF construct processing.	     	   */
            /* stmt_type = Then_Stmt should never be seen because THEN     */
            /* is not a beginning-of-stmt keyword.			   */
            /* None of the END stmt types except End_Stmt should be seen   */
            /* because they should all be classified as just END.	   */
                   
            PRINTMSG(stmt_start_line, 366, Internal, stmt_start_col);
# endif
            break;

      }  /* End switch (stmt_type)  */

      goto EXIT;   /* Get out so other IFs aren't deeply nested else's. */

   }     /* if (MATCHED_TOKEN_CLASS(Tok_Class_Keyword))			      */
      

   /* ----------------------------------------------------------------------- */
   /* If the token following the right paren is a label, we are parsing an    */
   /* (obsolescent) arithmetic IF, an outmoded (CFT77 extension) indirect     */
   /* logical IF, or an outmoded (CFT77 extension) two-branch arithmetic IF.  */
   /* ----------------------------------------------------------------------- */

   if (LA_CH_CLASS == Ch_Class_Digit) {

      if ( ! CURR_BLK_NO_EXEC  ||  ! iss_blk_stk_err() ) {
         curr_stmt_category = Executable_Stmt_Cat;
      }

      stmt_type                      = Arith_If_Stmt;
      SH_STMT_TYPE(curr_stmt_sh_idx) = Arith_If_Stmt;

      /* Attach a Br_Aif_Opr IR entry to the SH to hold the conditional       */
      /* expression and the list of labels.				      */

      NTR_IR_TBL(ir_idx);
      IR_OPR(ir_idx)      = Br_Aif_Opr;
      /* LRR - bhj put in Integer for type idx */
      IR_TYPE_IDX(ir_idx) = INTEGER_DEFAULT_TYPE;
      IR_LINE_NUM(ir_idx) = expr_start_line;
      IR_COL_NUM(ir_idx)  = expr_start_col;
      COPY_OPND(IR_OPND_L(ir_idx), cond_expr);

      SH_IR_IDX(curr_stmt_sh_idx) = ir_idx;

      /* MATCHED_TOKEN_CLASS is normally invoked as a function but we need    */
      /* not check its result because we already know the token starts with   */
      /* a digit.							      */

      MATCHED_TOKEN_CLASS(Tok_Class_Label);

      if (TOKEN_ERR(token)) {
         parse_err_flush(Find_EOS, NULL);
         goto EXIT;
      }

      /* Produce label list in the order:  zero-label, positive-label,        */
      /* negative-label.						      */

      lbl_attr_idx = check_label_ref();

      NTR_IR_LIST_TBL(neg_il_idx);
      IL_LINE_NUM(neg_il_idx) = TOKEN_LINE(token);
      IL_COL_NUM(neg_il_idx)  = TOKEN_COLUMN(token);
      IL_FLD(neg_il_idx)      = AT_Tbl_Idx;
      IL_IDX(neg_il_idx)      = lbl_attr_idx;
      AT_REFERENCED(lbl_attr_idx) = Referenced;
   
      IR_FLD_R(ir_idx) = IL_Tbl_Idx;
      IR_IDX_R(ir_idx) = neg_il_idx;
      ++IR_LIST_CNT_R(ir_idx);

      if (LA_CH_VALUE == COMMA) {
         NEXT_LA_CH;

         if (MATCHED_TOKEN_CLASS(Tok_Class_Label)) {

            if (TOKEN_ERR(token)) {
               parse_err_flush(Find_EOS, NULL);
               goto EXIT;
            }

            lbl_attr_idx = check_label_ref();

            NTR_IR_LIST_TBL(zero_il_idx);
            IL_LINE_NUM(zero_il_idx) = TOKEN_LINE(token);
            IL_COL_NUM(zero_il_idx)  = TOKEN_COLUMN(token);
            IL_FLD(zero_il_idx)      = AT_Tbl_Idx;
            IL_IDX(zero_il_idx)      = lbl_attr_idx;
            AT_REFERENCED(lbl_attr_idx) = Referenced;
   
            IL_NEXT_LIST_IDX(zero_il_idx) = neg_il_idx;
            IL_PREV_LIST_IDX(neg_il_idx)  = zero_il_idx;

            IR_IDX_R(ir_idx) = zero_il_idx;
            ++IR_LIST_CNT_R(ir_idx);

            if (LA_CH_VALUE == COMMA) {
               NEXT_LA_CH;

               if (MATCHED_TOKEN_CLASS(Tok_Class_Label)) {

                  if (TOKEN_ERR(token)) {
                     parse_err_flush(Find_EOS, NULL);
                     goto EXIT;
                  }

                  lbl_attr_idx = check_label_ref();

                  NTR_IR_LIST_TBL(pos_il_idx);
                  IL_LINE_NUM(pos_il_idx) = TOKEN_LINE(token);
                  IL_COL_NUM(pos_il_idx)  = TOKEN_COLUMN(token);
                  IL_FLD(pos_il_idx)      = AT_Tbl_Idx;
                  IL_IDX(pos_il_idx)      = lbl_attr_idx;
                  AT_REFERENCED(lbl_attr_idx) = Referenced;
   
                  IL_NEXT_LIST_IDX(zero_il_idx) = pos_il_idx;
                  IL_PREV_LIST_IDX(pos_il_idx)  = zero_il_idx;
                  IL_NEXT_LIST_IDX(pos_il_idx)  = neg_il_idx;
                  IL_PREV_LIST_IDX(neg_il_idx)  = pos_il_idx;
      
                  ++IR_LIST_CNT_R(ir_idx);

                  if (LA_CH_VALUE == EOS) {
                    
                     /* ----------------------------------------------------- */
                     /* Arithmetic IF.					      */
                     /* ----------------------------------------------------- */

#ifdef KEY /* Bug 318, 321 */
                     PRINTMSG(stmt_start_line, 112, Ansi, stmt_start_col);
#else /* KEY Bug 318, 321 */
                     PRINTMSG(stmt_start_line, 112, Comment, stmt_start_col);
#endif /* KEY Bug 318, 321 */

                     if (cif_flags & MISC_RECS) {
                        cif_stmt_type_rec(TRUE, 
                                          CIF_If_Arithmetic_Stmt, 
                                          statement_number);
                     }
                  }
                  else {
                     parse_err_flush(Find_EOS, EOS_STR);
                  }
               }
               else {
                  parse_err_flush(Find_EOS, "label");
               }
            }
            else {
            
               if (LA_CH_VALUE == EOS) {

                  /* -------------------------------------------------------- */
                  /* Outmoded indirect logical IF or outmoded two-branch      */
                  /* arithmetic IF.					      */
                  /* -------------------------------------------------------- */

                  stmt_type                      = Outmoded_If_Stmt;
                  SH_STMT_TYPE(curr_stmt_sh_idx) = Outmoded_If_Stmt;

                  PRINTMSG(stmt_start_line, 406, Ansi, stmt_start_col);

                  if (cif_flags & MISC_RECS) {
                     gen_sh(Before, Statement_Num_Stmt, statement_number, 0,
                            FALSE, FALSE, TRUE);
                  }
               }
               else {
                  parse_err_flush(Find_EOS, ", or EOS");
               }
            }
         }
         else {
            parse_err_flush(Find_EOS, "label");
         }
      }
      else {
         parse_err_flush(Find_EOS, ",");
      }
   }
   else {

      /* Expecting the keyword THEN, beginning of an action-stmt, or a label. */

      PRINTMSG(LA_CH_LINE, 377, Error, LA_CH_COLUMN);
      parse_err_flush(Find_EOS, NULL);
   }

EXIT:

   if (stmt_type == If_Stmt) {
      if_stmt_lbl_idx = NULL_IDX;
   }
   else {
            
      if (stmt_type == If_Cstrct_Stmt  &&  SH_ERR_FLG(curr_stmt_sh_idx)  && 
          stmt_label_idx != NULL_IDX) {
         AT_DCL_ERR(stmt_label_idx) = TRUE;
      }

      NEXT_LA_CH;
   }
   
   TRACE (Func_Exit, "parse_if_stmt", NULL);

   return;

}  /* parse_if_stmt */


/******************************************************************************\
|*									      *|
|* Description:								      *|
|*	BRIEF DESCRIPTION OF THIS FUNCTION'S PURPOSE			      *|
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

void parse_nullify_stmt (void)

{
   int       ir_idx;
   int       list1_idx;
   int       list2_idx = NULL_IDX;
   int       num_items = 0;
   opnd_type opnd;
   boolean   parsed_ok = TRUE;

   TRACE (Func_Entry, "parse_nullify_stmt", NULL);

   if (LA_CH_VALUE != LPAREN) {
      parse_err_flush(Find_EOS, "(");
      goto EXIT;
   }


   if (!CURR_BLK_NO_EXEC || !iss_blk_stk_err()) {
      curr_stmt_category = Executable_Stmt_Cat;
   }

   NTR_IR_TBL(ir_idx);
   SH_IR_IDX(curr_stmt_sh_idx) = ir_idx;
   IR_COL_NUM(ir_idx)          = TOKEN_COLUMN(token);
   IR_LINE_NUM(ir_idx)         = TOKEN_LINE(token);
   IR_OPR(ir_idx)              = Nullify_Opr;
   IR_TYPE_IDX(ir_idx)         = TYPELESS_DEFAULT_TYPE;
   IR_FLD_L(ir_idx)            = IL_Tbl_Idx;

   do {
      NEXT_LA_CH;
      num_items++;
   
      NTR_IR_LIST_TBL(list1_idx);

      if (list2_idx) {
         IL_NEXT_LIST_IDX(list2_idx) = list1_idx;
         IL_PREV_LIST_IDX(list1_idx) = list2_idx;
      }
      else {
         IR_IDX_L(ir_idx) = list1_idx;
      }
      list2_idx = list1_idx;

      if (MATCHED_TOKEN_CLASS(Tok_Class_Id)) {
         parsed_ok = parse_deref(&opnd, NULL_IDX) && parsed_ok;
         COPY_OPND(IL_OPND(list1_idx), opnd);

         mark_attr_defined(&opnd);
      }
      else {
         parsed_ok = FALSE;
         parse_err_flush(Find_Comma_Rparen, "IDENTIFIER");
      }
   }
   while (LA_CH_VALUE == COMMA);

   IR_LIST_CNT_L(ir_idx) = num_items;

   if (LA_CH_VALUE != RPAREN) {
      parse_err_flush(Find_EOS, ")");
   }
   else {
      NEXT_LA_CH;
   }

EXIT:
   
   if (LA_CH_VALUE != EOS) {
      parse_err_flush(Find_EOS, EOS_STR);
   }

   matched_specific_token(Tok_EOS, Tok_Class_Punct);

   TRACE (Func_Exit, "parse_nullify_stmt", NULL);

   return;

}  /* parse_nullify_stmt */


/******************************************************************************\
|*									      *|
|* Description:								      *|
|*	Parse a RETURN statement.                               	      *|
|*      BNF : return-stmt     is RETURN [scalar-int-expr]                     *|
|*      Alternate return specifiers are not allowed in FUNCTION subprograms.  *|
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

void parse_return_stmt (void)

{
   int          ir_idx;
   opnd_type    opnd;


   TRACE (Func_Entry, "parse_return_stmt", NULL);

   /* Check if in an executable SUBROUTINE or FUNCTION block. */

   if (CURR_BLK_NO_EXEC || 
       (ATP_PGM_UNIT(SCP_ATTR_IDX(curr_scp_idx)) != Function &&
        ATP_PGM_UNIT(SCP_ATTR_IDX(curr_scp_idx)) != Subroutine)) {
      iss_blk_stk_err();
   }

   if (cmd_line_flags.debug_lvl == Debug_Lvl_1) {  /* -ez  -G1 */
      gen_debug_lbl_stmt(curr_stmt_sh_idx, Ldbg_Start_Epilogue, NULL_IDX);
   }

   curr_stmt_category = Executable_Stmt_Cat;

   NTR_IR_TBL(ir_idx);
   SH_IR_IDX(curr_stmt_sh_idx) = ir_idx;

   IR_OPR(ir_idx)      = Return_Opr;
   IR_TYPE_IDX(ir_idx) = TYPELESS_DEFAULT_TYPE;
   IR_LINE_NUM(ir_idx) = LA_CH_LINE;
   IR_COL_NUM(ir_idx)  = LA_CH_COLUMN;

   if (LA_CH_VALUE != EOS) {

      /* Alternate return specifiers are obsolescent. */

      PRINTMSG(IR_LINE_NUM(ir_idx), 371,
#ifdef KEY /* Bug 318, 321 */
        Ansi,
#else /* KEY Bug 318, 321 */
        Comment,
#endif /* KEY Bug 318, 321 */
	IR_COL_NUM(ir_idx));

      /* Check for alternate return specifier in a FUNCTION subprogram. */

      if (ATP_PGM_UNIT(SCP_ATTR_IDX(curr_scp_idx)) == Function) {
         PRINTMSG(IR_LINE_NUM(ir_idx), 370, Error, IR_COL_NUM(ir_idx));
      }

      /* Parse the expression.  The Semantics Pass will verify that it's  */
      /* scalar and type integer.                                         */

      if (parse_expr(&opnd)) {

         COPY_OPND(IR_OPND_L(ir_idx), opnd);

         if (LA_CH_VALUE != EOS) {

            /* Have garbage at the end of the statement. */

            parse_err_flush(Find_EOS, EOS_STR);
         }
      }
   }

   NEXT_LA_CH;

   TRACE (Func_Exit, "parse_return_stmt", NULL);

   return;

}  /* parse_return_stmt */


/******************************************************************************\
|*									      *|
|* Description:								      *|
|*	This function parses the SELECT CASE statement.			      *|
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

void parse_select_stmt (void)

{
   opnd_type	expr_opnd;
   int		first_select_ir_idx;
   int		second_select_ir_idx;


   TRACE (Func_Entry, "parse_select_stmt", NULL);

   if (matched_specific_token(Tok_Kwd_Case, Tok_Class_Keyword) &&
       LA_CH_VALUE == LPAREN) {
      NEXT_LA_CH;
   }
   else if (TOKEN_VALUE(token) == Tok_Kwd_Case) {
      parse_err_flush(Find_EOS, "(");
      goto EXIT;
   }
   else {
      parse_err_flush(Find_EOS, "CASE");
      goto EXIT;
   }

   if (CURR_BLK_NO_EXEC  &&  iss_blk_stk_err()) {
      PUSH_BLK_STK (Select_Blk);
      CURR_BLK_ERR = TRUE;
   }
   else {
      PUSH_BLK_STK (Select_Blk);
      curr_stmt_category = Executable_Stmt_Cat;
   }

   CURR_BLK_FIRST_SH_IDX                 = curr_stmt_sh_idx;
   CURR_BLK_NO_EXEC	                 = TRUE;
   BLK_CASE_DEFAULT_LBL_FLD(blk_stk_idx) = NO_Tbl_Idx;
   LINK_TO_PARENT_BLK;

   if (stmt_construct_idx != NULL_IDX) {
      CURR_BLK_NAME			   = stmt_construct_idx;
      ATL_CLASS(stmt_construct_idx)	   = Lbl_Construct;
      ATL_DEF_STMT_IDX(stmt_construct_idx) = curr_stmt_sh_idx;
      SH_ERR_FLG(curr_stmt_sh_idx)	   =
         SH_ERR_FLG(curr_stmt_sh_idx)  ||  AT_DCL_ERR(stmt_construct_idx);
      stmt_construct_idx			= NULL_IDX;
   }
   
   /* Generate a dummy Select IR and attach it to the Select_Stmt SH.  This   */
   /* dummy IR is needed because we need a place to temporarily hang the      */
   /* sorted CASE list in the Semantics Pass.  This dummy IR is removed by    */
   /* the END SELECT code in the Semantics Pass.  After the following two     */
   /* Select IR generations we have:					      */
   /*									      */
   /*       Select_Stmt SH  ---> Select IR				      */
   /*                              -----------> (actual) Select IR            */
   /*                              -----------> sorted CASE list will go here */

   NTR_IR_TBL(first_select_ir_idx);
   SH_IR_IDX(curr_stmt_sh_idx) = first_select_ir_idx;
   IR_OPR(first_select_ir_idx) = Select_Opr;
   /* LRR - bhj used short typeless for type idx */
   IR_TYPE_IDX(first_select_ir_idx) = TYPELESS_DEFAULT_TYPE;

   if (parse_expr(&expr_opnd)) {
      NTR_IR_TBL(second_select_ir_idx);
      IR_OPR(second_select_ir_idx)      = Select_Opr;
      /* LRR - bhj used short typeless for type idx */
      IR_TYPE_IDX(second_select_ir_idx) = TYPELESS_DEFAULT_TYPE;
      IR_LINE_NUM(second_select_ir_idx) = stmt_start_line;
      IR_COL_NUM(second_select_ir_idx)  = stmt_start_col;
      COPY_OPND(IR_OPND_L(second_select_ir_idx), expr_opnd);
     
      IR_FLD_L(first_select_ir_idx) = IR_Tbl_Idx;
      IR_IDX_L(first_select_ir_idx) = second_select_ir_idx;

      CURR_BLK_LABEL = gen_internal_lbl(stmt_start_line);
      AT_REFERENCED(CURR_BLK_LABEL) = Referenced;
   }

   if (LA_CH_VALUE == RPAREN) {
      NEXT_LA_CH;
   }
   else {
      parse_err_flush(Find_EOS, ")");
   }

   if (LA_CH_VALUE != EOS) {
      parse_err_flush(Find_EOS, EOS_STR);
   }
    
   CURR_BLK_ERR = SH_ERR_FLG(curr_stmt_sh_idx);

EXIT:

   NEXT_LA_CH;

   TRACE (Func_Exit, "parse_select_stmt", NULL);

   return;

}  /* parse_select_stmt */


/******************************************************************************\
|*									      *|
|* Description:								      *|
|*	Parse the STOP statement or the PAUSE statement.                      *|
|*      BNF :                                                                 *|
|*            pause_stmt   is PAUSE [stop-code]                               *|
|*            stop_stmt    is STOP [stop-code]                                *|
|*            stop_code    is scalar-char-constant                            *|
|*                         or digit [digit [digit [ digit [ digit ] ] ] ]     *|
|*									      *|
|*      NOTE: CFT77 allows an arbitrary character expression for the          *|
|*            stop-code.  CFT90 will be extended to support this.             *|
|*									      *|
|*									      *|
|*									      *|
|*									      *|
|*									      *|
|*									      *|
|*									      *|
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

void parse_stop_pause_stmt (void)

{
   opnd_type    opnd;
   int          ir_idx;


   TRACE (Func_Entry, "parse_stop_pause_stmt", NULL);

   if (cmd_line_flags.debug_lvl == Debug_Lvl_1) {  /* -ez  -G1 */
      gen_debug_lbl_stmt(curr_stmt_sh_idx, Ldbg_Exit_Point, NULL_IDX);
   }

   if (!CURR_BLK_NO_EXEC || !iss_blk_stk_err()) {
      curr_stmt_category = Executable_Stmt_Cat;
   }

   if (ATP_PURE(SCP_ATTR_IDX(curr_scp_idx)) ||
       ATP_ELEMENTAL(SCP_ATTR_IDX(curr_scp_idx))) {
      PRINTMSG(TOKEN_LINE(token), 1262, Error,
               TOKEN_COLUMN(token),
               stmt_type_str[stmt_type],
               ATP_PURE(SCP_ATTR_IDX(curr_scp_idx)) ? "pure" : "elemental",
               AT_OBJ_NAME_PTR(SCP_ATTR_IDX(curr_scp_idx)));
   }

   NTR_IR_TBL(ir_idx);
   SH_IR_IDX(curr_stmt_sh_idx) = ir_idx;
   IR_LINE_NUM(ir_idx)         = LA_CH_LINE;
   IR_COL_NUM(ir_idx)          = LA_CH_COLUMN;

   if (stmt_type == Stop_Stmt) {
      IR_OPR(ir_idx) = Stop_Opr;
   }
   else {
      IR_OPR(ir_idx) = Pause_Opr;

      /* The PAUSE statement is a deleted feature. */

      PRINTMSG(TOKEN_LINE(token), 1568, Ansi, TOKEN_COLUMN(token), "PAUSE");
   }

   if (LA_CH_VALUE != EOS) {

      if (LA_CH_CLASS == Ch_Class_Digit) {
         matched_specific_token(Tok_Const_Int, Tok_Class_Int_Spec);

         if (LA_CH_VALUE != EOS) {
            /* Have garbage at the end of the statement. */
            parse_err_flush(Find_EOS, EOS_STR);
         }
         else {
            IR_FLD_L(ir_idx)      = CN_Tbl_Idx;
            IR_IDX_L(ir_idx)      = TOKEN_CONST_TBL_IDX(token);
            IR_COL_NUM_L(ir_idx)  = TOKEN_COLUMN(token);
            IR_LINE_NUM_L(ir_idx) = TOKEN_LINE(token);
         }
      }

      /* Parse the expression.  The Semantics Pass will verify rank and */
      /* type.                                                          */
      else if (parse_expr(&opnd)) {

         COPY_OPND(IR_OPND_L(ir_idx), opnd);

         if (LA_CH_VALUE != EOS) {
            /* Have garbage at the end of the statement. */
            parse_err_flush(Find_EOS, EOS_STR);
         }
      }
   }

   /* Get statement header for the RETURN which will be generated following */
   /* the CALL to $STOP                                                     */
   if (stmt_type == Stop_Stmt &&
       ! cdir_switches.parallel_region &&
       ! cdir_switches.doall_region &&
       ! cdir_switches.guard) {

      gen_sh(After, Return_Stmt, stmt_start_line, stmt_start_col,
             SH_ERR_FLG(curr_stmt_sh_idx), FALSE, TRUE);

      NTR_IR_TBL(ir_idx);
      SH_IR_IDX(curr_stmt_sh_idx) = ir_idx;
      SH_P2_SKIP_ME(curr_stmt_sh_idx) = TRUE;

      IR_OPR(ir_idx)      = Return_Opr;
      IR_TYPE_IDX(ir_idx) = TYPELESS_DEFAULT_TYPE;
      IR_LINE_NUM(ir_idx) = stmt_start_line;
      IR_COL_NUM(ir_idx)  = stmt_start_col;
   }


   NEXT_LA_CH;

   TRACE (Func_Exit, "parse_stop_pause_stmt", NULL);

   return;

}  /* parse_stop_pause_stmt */


/******************************************************************************\
|*									      *|
|* Description:								      *|
|*	BRIEF DESCRIPTION OF THIS FUNCTION'S PURPOSE			      *|
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

void parse_where_stmt (void)

{
   int			col;
   int			ir_idx;
   int			line;
   opnd_type		opnd;
   int			save_curr_stmt_sh_idx;


   TRACE (Func_Entry, "parse_where_stmt", NULL);

   if (LA_CH_VALUE != LPAREN) {
      parse_err_flush(Find_EOS, "(");
      goto EXIT;
   }

   NTR_IR_TBL(ir_idx);
   IR_OPR(ir_idx)      = Where_Opr;
   IR_TYPE_IDX(ir_idx) = TYPELESS_DEFAULT_TYPE;
   IR_LINE_NUM(ir_idx) = TOKEN_LINE(token);
   IR_COL_NUM(ir_idx)  = TOKEN_COLUMN(token);

   SH_IR_IDX(curr_stmt_sh_idx) = ir_idx;

   if (CURR_BLK == Where_Then_Blk ||
       CURR_BLK == Where_Else_Blk ||
       CURR_BLK == Where_Else_Mask_Blk) {

      SH_PARENT_BLK_IDX(curr_stmt_sh_idx) = CURR_BLK_FIRST_SH_IDX;
   }

   /* if this is a Where stmt, the right opnd will hold the assignment stmt, */
   /* otherwise, it is a where construct stmt and the right opnd is null.    */

   /* swallow ( */
   NEXT_LA_CH;

   parse_expr(&opnd);

   if (LA_CH_VALUE != RPAREN) {
      if (! parse_err_flush(Find_Rparen, ")")) {
         goto EXIT;
      }
   }

   /* swallow ) */
   NEXT_LA_CH;

   COPY_OPND(IR_OPND_L(ir_idx), opnd);

   if (LA_CH_VALUE != EOS) {
      /* Have where_stmt */

      stmt_type = Where_Stmt;
      SH_STMT_TYPE(curr_stmt_sh_idx) = Where_Stmt;

      if (cif_flags & MISC_RECS) {
         cif_stmt_type_rec(TRUE, CIF_Where_Stmt, statement_number);
      }

      if (CURR_BLK_NO_EXEC &&  
          CURR_BLK != Forall_Blk &&
          CURR_BLK != Where_Then_Blk &&
          CURR_BLK != Where_Else_Blk &&
          CURR_BLK != Where_Else_Mask_Blk) {
         iss_blk_stk_err();
         parse_err_flush(Find_EOS, NULL);
         goto EXIT;
      }

      /* We do not know if this will be an Assignment Statement */
      /* or an Array_Assignment_Stmt so we need to generate a   */
      /* place holder for the statement number.  We will carry  */
      /* it on a Statement_Num_Stmt following the Where_Stmt.   */
      /* It MUST be processed in where_stmt_semantics so as not */
      /* to mess up cif statement processing.                   */
      
      INCREMENT_STATEMENT_NUMBER;

      gen_sh(After, Statement_Num_Stmt, stmt_start_line, stmt_start_col,
             FALSE, FALSE, TRUE);
      SH_PARENT_BLK_IDX(curr_stmt_sh_idx)	= statement_number;
      curr_stmt_category			= Executable_Stmt_Cat;
      save_curr_stmt_sh_idx 			= curr_stmt_sh_idx;
      line					= LA_CH_LINE;
      col					= LA_CH_COLUMN;
      in_action_stmt_of_if_where_or_forall	= TRUE;

      if (MATCHED_TOKEN_CLASS(Tok_Class_Keyword)) {
         curr_stmt_sh_idx	= ntr_sh_tbl();

         determine_stmt_type();

         if (stmt_type == Assignment_Stmt) {

            (*stmt_parsers[Assignment_Stmt])();

            IR_FLD_R(ir_idx) = IR_Tbl_Idx;
            IR_IDX_R(ir_idx) = SH_IR_IDX(curr_stmt_sh_idx);

            FREE_SH_NODE(curr_stmt_sh_idx);
            curr_stmt_sh_idx = save_curr_stmt_sh_idx;
            in_action_stmt_of_if_where_or_forall	= FALSE;

            goto NO_EOS;
         }
         else {

            FREE_SH_NODE(curr_stmt_sh_idx);
            curr_stmt_sh_idx = save_curr_stmt_sh_idx;

            PRINTMSG(line, 102, Error, col);
            parse_err_flush(Find_EOS, NULL);
            in_action_stmt_of_if_where_or_forall	= FALSE;
            goto EXIT;
         }
      }
      else {
         PRINTMSG(line, 102, Error, col);
         parse_err_flush(Find_EOS, NULL);
         in_action_stmt_of_if_where_or_forall	= FALSE;
         goto EXIT;
      }
   }
   else {
      /* else have where_construct_stmt */

      IR_OPR(ir_idx) = Where_Cnstrct_Opr;

      if (cif_flags & MISC_RECS) {
         cif_stmt_type_rec(TRUE, CIF_Where_Construct, statement_number);
      }

      if (! CURR_BLK_NO_EXEC ||  
          CURR_BLK == Forall_Blk ||
          CURR_BLK == Where_Then_Blk ||
          CURR_BLK == Where_Else_Blk ||
          CURR_BLK == Where_Else_Mask_Blk) {
         PUSH_BLK_STK (Where_Then_Blk);
      }
      else {
         iss_blk_stk_err();
         PUSH_BLK_STK (Where_Then_Blk);
         CURR_BLK_ERR = TRUE;
      }

      if (stmt_construct_idx != NULL_IDX) {
         CURR_BLK_NAME                        = stmt_construct_idx;
         ATL_CLASS(stmt_construct_idx)        = Lbl_Construct;
         ATL_DEF_STMT_IDX(stmt_construct_idx) = curr_stmt_sh_idx;
         SH_ERR_FLG(curr_stmt_sh_idx)         =
            SH_ERR_FLG(curr_stmt_sh_idx)  ||  AT_DCL_ERR(stmt_construct_idx);
         stmt_construct_idx                        = NULL_IDX;
      }

      curr_stmt_category    = Executable_Stmt_Cat;
      CURR_BLK_NO_EXEC	    = TRUE;
      CURR_BLK_FIRST_SH_IDX = curr_stmt_sh_idx;
   }

EXIT:

   NEXT_LA_CH;

NO_EOS:
   
   TRACE (Func_Exit, "parse_where_stmt", NULL);
   
   return;

}  /* parse_where_stmt */


/******************************************************************************\
|*									      *|
|* Description:								      *|
|*	This searches the blk_stk for the specified block and name.  If not   *|
|*	found, it returns NULL_IDX and makes sure blk_stk[NULL_IDX] is clear. *|
|*									      *|
|* Input parameters:							      *|
|*	blk_type   -> The type of block being searched for.		      *|
|*	found_name -> TRUE if there is a name to match.   The actual name is  *|
|*	              in token.                                               *|
|*									      *|
|* Output parameters:							      *|
|*	NONE								      *|
|*									      *|
|* Returns:								      *|
|*	Index of the matched block.  It will return NULL_IDX if there is no   *|
|*	     match, but it will also clear blk_stk[NULL_IDX] so that calling  *|
|*	     routines do not have to check for the NULL_IDX.                  *|
|*									      *|
\******************************************************************************/

static int match_blk(blk_cntxt_type	the_blk,
		     boolean		fnd_name)

{
   int		 blk_idx;


   TRACE (Func_Entry, "match_blk", NULL);

   for (blk_idx = blk_stk_idx; blk_idx > NULL_IDX; blk_idx--) {

      if (BLK_TYPE(blk_idx) == the_blk) {

         if (!fnd_name || 
             (BLK_NAME(blk_idx) != NULL_IDX && 
                             (compare_names(TOKEN_ID(token).words,
                                        TOKEN_LEN(token),
                                        AT_OBJ_NAME_LONG(BLK_NAME(blk_idx)),
                                        AT_NAME_LEN(BLK_NAME(blk_idx))) == 0))){
            break;
         }
      }
   }

   if (blk_idx == NULL_IDX) {
      CLEAR_TBL_NTRY(blk_stk, NULL_IDX);
   }

   TRACE (Func_Exit, "match_blk", NULL);

   return(blk_idx);

}  /* match_blk */


/******************************************************************************\
|*                                                                            *|
|* Description:                                                               *|
|*      This procedure parses the label-list for the computed and assigned GO *|
|*      TO statements.  						      *|
|*                                                                            *|
|* Input parameters:                                                          *|
|*      ir_idx : index to the current IR entry                                *|
|*                                                                            *|
|* Output parameters:                                                         *|
|*      NONE								      *|
|*                                                                            *|
|* Returns:                                                                   *|
|*      A boolean value that indicates whether or not the label-list was      *|
|*      parsed correctly.  If an error occurs, the statement is flushed to    *|
|*      the EOS so the caller should discontinue parsing.                     *|
|*                                                                            *|
\******************************************************************************/

static boolean  parse_label_list(int	       ir_idx)

{
   int		lbl_attr_idx;
   int		list_tail		= NULL_IDX;
   int		new_lbl;
   boolean	parsed_correctly	= TRUE;

   
   TRACE (Func_Entry, "parse_label_list", NULL);

   /* The left paren has already been seen by the caller.  Just absorb it.    */

   NEXT_LA_CH;

   /* Loop through the labels and commas in the list until the right paren    */
   /* is found.  The list must not be empty.  Save each label reference in    */
   /* a List entry.  The List entries are linked in order of label appearance */
   /* because the index expression of the computed GO TO selects them by      */
   /* ordinal position.          				              */

   while (LA_CH_VALUE != EOS) {

      if (MATCHED_TOKEN_CLASS(Tok_Class_Label)) {

         if ( ! TOKEN_ERR(token)) {
            lbl_attr_idx = check_label_ref();

            NTR_IR_LIST_TBL(new_lbl);

            if (list_tail != NULL_IDX) {
               IL_NEXT_LIST_IDX(list_tail) = new_lbl;
               IL_PREV_LIST_IDX(new_lbl)   = list_tail;
               list_tail                   = new_lbl;
            }
            else {
               IR_FLD_R(ir_idx) = IL_Tbl_Idx;
               IR_IDX_R(ir_idx) = new_lbl;
               list_tail        = new_lbl;
            }

            IL_LINE_NUM(new_lbl) = TOKEN_LINE(token);
            IL_COL_NUM(new_lbl)  = TOKEN_COLUMN(token);
            IL_FLD(new_lbl)      = AT_Tbl_Idx;
            IL_IDX(new_lbl)      = lbl_attr_idx;
            AT_REFERENCED(lbl_attr_idx) = Referenced;

            IR_LIST_CNT_R(ir_idx)++;
         }

         if (LA_CH_VALUE == COMMA) {
            NEXT_LA_CH;
         }
         else if (LA_CH_VALUE == RPAREN) {
                 NEXT_LA_CH;
                 goto EXIT;
              }
              else {
                 parse_err_flush(Find_EOS, ", or )");
                 parsed_correctly = FALSE;
                 goto EXIT;
              }
      
      }
      else {
         parse_err_flush(Find_EOS, "statement label");
         parsed_correctly = FALSE;
         goto EXIT;
      }

   }  /* while */

   parse_err_flush(Find_EOS, "statement label");
   parsed_correctly = FALSE;

EXIT:
   
   TRACE (Func_Exit, "parse_label_list", NULL);

   return(parsed_correctly);

}  /* parse_label_list */


/******************************************************************************\
|*									      *|
|* Description:								      *|
|*	Look for a final subscript opr on a reference tree in an              *|
|*      ALLOCATE allocation object and change to a Alloc_Obj_Opr.             *|
|*      Also check for stride in extent specs.                                *|
|*									      *|
|* Input parameters:							      *|
|*	opnd - root of subtree to check.                                      *|
|*									      *|
|* Output parameters:							      *|
|*	opnd - checked tree.                                                  *|
|*									      *|
|* Returns:								      *|
|*	TRUE if no error with extent spec.                                    *|
|*									      *|
\******************************************************************************/

static boolean change_subscript(opnd_type	*top_opnd)

{
   boolean      allocate;
   int		column;
   int		ir_idx;
   int          ir2_idx;
   int		line;
   int		list_idx;
   int		list2_idx;
   int          msg_num;
   boolean	ok		= TRUE;


   TRACE (Func_Entry, "change_subscript", NULL);

   if (stmt_type == Deallocate_Stmt) {
      allocate = FALSE;
      msg_num  = 428;
   }
   else {
      allocate = TRUE;
      msg_num  = 201;
   }

   if (OPND_FLD((*top_opnd)) == CN_Tbl_Idx) {

      /* error .. can't have constant in allocate stmt */

      find_opnd_line_and_column(top_opnd, &line, &column);

      PRINTMSG(line, msg_num, Error, column);
      ok = FALSE;
   }
   else if (OPND_FLD((*top_opnd)) == AT_Tbl_Idx) {
      NTR_IR_TBL(ir_idx);
      IR_OPR(ir_idx)        = (allocate ? Alloc_Obj_Opr : Dealloc_Obj_Opr);
      IR_TYPE_IDX(ir_idx)   = TYPELESS_DEFAULT_TYPE;
      IR_LINE_NUM(ir_idx)   = OPND_LINE_NUM((*top_opnd));
      IR_COL_NUM(ir_idx)    = OPND_COL_NUM((*top_opnd));

      COPY_OPND(IR_OPND_L(ir_idx), (*top_opnd));
     
      OPND_FLD((*top_opnd)) = IR_Tbl_Idx;
      OPND_IDX((*top_opnd)) = ir_idx;
   }
   else if (OPND_FLD((*top_opnd)) == IR_Tbl_Idx) {
      ir_idx = OPND_IDX((*top_opnd));
   
      switch (IR_OPR(ir_idx)) {

         case Subscript_Opr :

            if (allocate) {
               IR_OPR(ir_idx) = Alloc_Obj_Opr;

               list_idx = IR_IDX_R(ir_idx);

               while (list_idx != NULL_IDX) {

                  if (IL_FLD(list_idx) == IR_Tbl_Idx           &&
                      IR_OPR(IL_IDX(list_idx)) == Triplet_Opr) {

                     ir_idx                = IL_IDX(list_idx);
                     IL_FLD(list_idx)      = IL_Tbl_Idx;
                     IL_LIST_CNT(list_idx) = 2;
                     IL_IDX(list_idx)      = IR_IDX_L(ir_idx);

                     list2_idx             = IL_IDX(list_idx);
                     list2_idx             = IL_NEXT_LIST_IDX(list2_idx);

                     if (IL_FLD(IR_IDX_L(ir_idx)) == NO_Tbl_Idx) {
                        /* bad allocate shape spec, no lower bound */
                        ok = FALSE;
                        PRINTMSG(IR_LINE_NUM(ir_idx), 1558, Error,
                                 IR_COL_NUM(ir_idx),
                                 "lower bound");
                     }
                     else if (IL_FLD(list2_idx) == NO_Tbl_Idx &&
                              ! IL_PE_SUBSCRIPT(list_idx)) {
                        /* bad allocate shape spec, no upper bound */
                        ok = FALSE;
                        PRINTMSG(IR_LINE_NUM(ir_idx), 1558, Error,
                                 IR_COL_NUM(ir_idx),
                                 "upper bound");
                     }
                     else if (IL_FLD(IL_NEXT_LIST_IDX(list2_idx)) 
                                                       != NO_Tbl_Idx) {
                        /* error .. stride is supplied */
                        ok = FALSE;
                        find_opnd_line_and_column((opnd_type *)&IL_OPND(
                                                   IL_NEXT_LIST_IDX(list2_idx)),
                                                   &line,
                                                   &column);
                        PRINTMSG(line, 401, Error, column);
                     }
   
                     FREE_IR_NODE(ir_idx);
      
                     IL_NEXT_LIST_IDX(list2_idx) = NULL_IDX;
   
                  }
   
                  list_idx = IL_NEXT_LIST_IDX(list_idx);
               }
            }
            else {
               /* deallocate can't have subscript at end */
               PRINTMSG(IR_LINE_NUM(ir_idx), 430, Error,
                        IR_COL_NUM(ir_idx));
               ok = FALSE;
            }
            break;          
   
         case Struct_Opr    :

            /* implies right opnd is AT */
            NTR_IR_TBL(ir2_idx);
            IR_OPR(ir2_idx)        = (allocate ? Alloc_Obj_Opr : 
                                                 Dealloc_Obj_Opr);
            IR_TYPE_IDX(ir2_idx)   = TYPELESS_DEFAULT_TYPE;
            IR_LINE_NUM(ir2_idx)   = IR_LINE_NUM_R(ir_idx);
            IR_COL_NUM(ir2_idx)    = IR_COL_NUM_R(ir_idx);

            COPY_OPND(IR_OPND_L(ir2_idx), (*top_opnd));
     
            OPND_FLD((*top_opnd))  = IR_Tbl_Idx;
            OPND_IDX((*top_opnd))  = ir2_idx;

            break;
            
         case Substring_Opr :

            PRINTMSG(IR_LINE_NUM(ir_idx), 405, Error, IR_COL_NUM(ir_idx));
            ok = FALSE;
            break;

         default            :
            PRINTMSG(IR_LINE_NUM(ir_idx), msg_num, Error, IR_COL_NUM(ir_idx));
            ok = FALSE;
            break;
      }
   }
   else { /* something weird */
      find_opnd_line_and_column(top_opnd, &line, &column);
      PRINTMSG(line, 626, Internal, column,
               "CN_Tbl_Idx, AT_Tbl_Idx, or IR_Tbl_Idx",
               "change_subscript");
   }

   TRACE (Func_Exit, "change_subscript", NULL);

   return(ok);

}  /* change_subscript */


/******************************************************************************\
|*									      *|
|* Description:								      *|
|*	Construct the name for an internal DO loop label in a token.          *|
|*									      *|
|* Input parameters:							      *|
|*	blk_idx : the index of the Block Stack entry for which the label is   *|
|*                  to be generated           				      *|
|*      lbl_pos : indicates if the name is being generated for the top,       *|
|*                  cycle, exit, or skip label                    	      *|
|*									      *|
|* Output parameters:							      *|
|*	NONE								      *|
|*									      *|
|* Returns:								      *|
|*	The index to the Attribute entry for the internal label, or NULL_IDX  *|
|*      if the Attribute entry already exists.				      *|
|*									      *|
\******************************************************************************/

int gen_loop_lbl_name(int		blk_idx,
         	      lbl_pos_type	lbl_pos)

{
   int		attr_idx;
   char		lbl_name[MAX_ID_LEN];   /* A max as reasonable as any other.  */
   char		lbl_pos_name[4][8] = { "top", "cycle", "exit", "bot" };
   token_type   lbl_token;
   int		name_idx;
   char		num_buf[8];
   

   TRACE (Func_Entry, "gen_loop_lbl_name", NULL);

   if (BLK_LABEL(blk_idx) != NULL_IDX) {
      strcpy(lbl_name, AT_OBJ_NAME_PTR(BLK_LABEL(blk_idx)));
   }
   else {
      sprintf(num_buf, "%d", BLK_DEF_LINE(blk_idx));
      strcpy(lbl_name, num_buf);
      strcat(lbl_name, "u");
   }

   strcat(lbl_name, lbl_pos_name[lbl_pos]);
   sprintf(num_buf, "%d", BLK_LOOP_NUM(blk_idx)); 
   strcat(lbl_name, num_buf);

BACK:

   CREATE_ID(TOKEN_ID(lbl_token), lbl_name, strlen(lbl_name));
   TOKEN_LEN(lbl_token) = strlen(lbl_name);

   attr_idx = srch_sym_tbl(TOKEN_STR(lbl_token), TOKEN_LEN(lbl_token),
                           &name_idx);

   if (attr_idx == NULL_IDX) {
      attr_idx                  = ntr_sym_tbl(&lbl_token, name_idx);
      AT_OBJ_CLASS(attr_idx)    = Label;
      ATL_CLASS(attr_idx)       = Lbl_Internal;
      ATL_EXECUTABLE(attr_idx)  = TRUE;

      if (lbl_pos == Top_Lbl  && 
          (stmt_type == Do_Iterative_Stmt  ||  stmt_type == Do_While_Stmt)) {
         ATL_TOP_OF_LOOP(attr_idx) = TRUE;
      }
        
      if (cmd_line_flags.debug_lvl == Debug_Lvl_1) {  /* -ez -G1 */
         ATL_DEBUG_CLASS(attr_idx) = Ldbg_Loop_Lbl;
      }
   }
   else if (lbl_pos == Top_Lbl  ||  lbl_pos == Skip_Lbl) {

      sprintf(num_buf, "%d", BLK_FIRST_SH_IDX(blk_idx));
      strcat(lbl_name, num_buf);
      goto BACK;
   }

   TRACE (Func_Exit, "gen_loop_lbl_name", NULL);
 
   return(attr_idx);

}  /* gen_loop_lbl_name */


/******************************************************************************\
|*									      *|
|* Description:								      *|
|*	Do all the processing necessary if a C*$* BLOCKABLE directive is in   *|
|*      effect for this loop.						      *|
|*									      *|
|* Input parameters:							      *|
|*	NONE								      *|
|*									      *|
|* Output parameters:							      *|
|*	NONE								      *|
|*									      *|
|* Returns:								      *|
|*	NOTHING								      *|
|*									      *|
\******************************************************************************/

static void process_blockable_dir(void)

{
   int		blk_idx;
   int		dir_il_idx;
   int		i;
   int		il_idx;
   int		il_idx_2;
   int		ir_idx;


   TRACE (Func_Entry, "process_blockable_dir", NULL);

   /* If a C*$* BLOCKABLE directive was specified ahead of this loop, make    */
   /* sure the DO-variable for this (outer) loop is a member of the           */
   /* DO-variable list and, if so, capture some information about the	      */
   /* directive.						              */

   if (cdir_switches.blockable_sh_idx != NULL_IDX) {
      BLK_BLOCKABLE_DIR_SH_IDX(blk_stk_idx) = cdir_switches.blockable_sh_idx;
      ir_idx                                =
         SH_IR_IDX(cdir_switches.blockable_sh_idx);
      BLK_BLOCKABLE_NUM_LCVS(blk_stk_idx)   = IR_LIST_CNT_L(ir_idx);
      cdir_switches.blockable_sh_idx        = NULL_IDX;
      dir_il_idx                            = IR_IDX_L(ir_idx);

      for (i = 1;  i <= IR_LIST_CNT_L(ir_idx);  ++i) {

         if (IL_IDX(dir_il_idx) == OPND_IDX(BLK_DO_VAR_OPND(blk_stk_idx))) {
            break;
         }
         else {
            dir_il_idx = IL_NEXT_LIST_IDX(dir_il_idx);
         }
      }

      if (i <= IR_LIST_CNT_L(ir_idx)) {
         NTR_IR_LIST_TBL(il_idx);
         IR_FLD_R(ir_idx)      = IL_Tbl_Idx;
         IR_IDX_R(ir_idx)      = il_idx;
         IR_LIST_CNT_R(ir_idx) = 1;

         IL_LINE_NUM(il_idx) = TOKEN_LINE(token); 
         IL_COL_NUM(il_idx)  = TOKEN_COLUMN(token);
         IL_FLD(il_idx)      = CN_Tbl_Idx; 
         IL_IDX(il_idx)      = C_INT_TO_CN(CG_INTEGER_DEFAULT_TYPE, i);
      }
      else {
         PRINTMSG(TOKEN_LINE(token), 1379, Error, TOKEN_COLUMN(token),
                  "BLOCKABLE");
         SH_ERR_FLG(BLK_BLOCKABLE_DIR_SH_IDX(blk_stk_idx)) = TRUE;
         BLK_BLOCKABLE_DIR_SH_IDX(blk_stk_idx)             = NULL_IDX;
         BLK_BLOCKABLE_NUM_LCVS(blk_stk_idx)               = 0;
      }
   }
   else {

      /* Try to find an interative DO Block Stack entry that is the subject   */
      /* of a BLOCKABLE directive so we know whether or not we should check   */
      /* to see if the current loop is perfectly nested.                      */

      for (blk_idx = blk_stk_idx - 1;  blk_idx > 1;  --blk_idx) {
               
         if (BLK_TYPE(blk_idx) == Do_Blk  &&
             SH_STMT_TYPE(BLK_FIRST_SH_IDX(blk_idx)) == Do_Iterative_Stmt) {
            break;
         }
      }

      if (blk_idx > 1  &&  BLK_BLOCKABLE_NUM_LCVS(blk_idx) > 1) {

         /* If the current nest is subject to a BLOCKABLE directive, set up   */
         /* the current and parent Block Stack entries to reflect the new     */
         /* loop and make sure the DO-variable of this loop is in the         */
         /* DO-variable list of the BLOCKABLE directive.		      */

         if (BLK_BLOCKABLE_NUM_LCVS(blk_stk_idx - 1) > 1) {
            BLK_BLOCKABLE_NUM_LCVS(blk_stk_idx) =
               BLK_BLOCKABLE_NUM_LCVS(blk_stk_idx - 1) - 1;
      
            blk_idx = blk_stk_idx - 1;
 
            while (BLK_BLOCKABLE_DIR_SH_IDX(blk_idx) == NULL_IDX) {
               --blk_idx; 
            }

            ir_idx     = SH_IR_IDX(BLK_BLOCKABLE_DIR_SH_IDX(blk_idx));
            dir_il_idx = IR_IDX_L(ir_idx);

            for (i = 1;  i <= IR_LIST_CNT_L(ir_idx);  ++i) {

               if (IL_IDX(dir_il_idx) ==
                      OPND_IDX(BLK_DO_VAR_OPND(blk_stk_idx))) {
                  break;
               }
               else {
                  dir_il_idx = IL_NEXT_LIST_IDX(dir_il_idx);
               }
            }

            if (i <= IR_LIST_CNT_L(ir_idx)) {

               /* We found the current DO-variable in the DO-variable list of */
               /* the BLOCKABLE directive.  Add its position to the position  */
               /* list.							      */

               il_idx = IR_IDX_R(ir_idx);

               while (IL_NEXT_LIST_IDX(il_idx) != NULL_IDX) {
                  il_idx = IL_NEXT_LIST_IDX(il_idx);
               }

               NTR_IR_LIST_TBL(il_idx_2);                  
               IL_NEXT_LIST_IDX(il_idx)   = il_idx_2;
               IL_PREV_LIST_IDX(il_idx_2) = il_idx;
               ++IR_LIST_CNT_R(ir_idx);
               IL_LINE_NUM(il_idx_2) = TOKEN_LINE(token);
               IL_COL_NUM(il_idx_2)  = TOKEN_COLUMN(token);
               IL_FLD(il_idx_2)      = CN_Tbl_Idx;
               IL_IDX(il_idx_2)      = C_INT_TO_CN(CG_INTEGER_DEFAULT_TYPE, i);
            } 
            else {
               PRINTMSG(TOKEN_LINE(token), 1379, Error, TOKEN_COLUMN(token),
                        "BLOCKABLE");
               BLK_BLOCKABLE_NUM_LCVS(blk_stk_idx)   = 0;

               blk_idx = blk_stk_idx - 1;

               for (blk_idx = blk_stk_idx;  blk_idx > 1;  --blk_idx) {
                  BLK_BLOCKABLE_NUM_LCVS(blk_idx) = 0;

                  if (BLK_BLOCKABLE_DIR_SH_IDX(blk_idx) != NULL_IDX) {
                     SH_ERR_FLG(BLK_BLOCKABLE_DIR_SH_IDX(blk_idx)) = TRUE;
                     BLK_BLOCKABLE_DIR_SH_IDX(blk_idx)             = NULL_IDX;
                  }
               }
            }
         }
      }
   }

   TRACE (Func_Exit, "process_blockable_dir", NULL);

   return;  

}  /* process_blockable_dir */


/******************************************************************************\
|*									      *|
|* Description:								      *|
|*	Do all the processing necessary if a C*$* INTERCHANGE directive       *|
|*      is in effect for this loop.					      *|
|*									      *|
|* Input parameters:							      *|
|*	NONE								      *|
|*									      *|
|* Output parameters:							      *|
|*	NONE								      *|
|*									      *|
|* Returns:								      *|
|*	TRUE if the current DO statement immediately follows a preceding      *|
|*      iterative DO statement.						      *|
|*									      *|
\******************************************************************************/

static void process_interchange_dir(void)

{
   int		blk_idx;
   int		dir_il_idx;
   int		i;
   int		il_idx;
   int		il_idx_2;
   int		ir_idx;
   int		sh_idx;


   TRACE (Func_Entry, "process_interchange_dir", NULL);

   /* If a C*$* INTERCHANGE directive was specified ahead of this loop, make  */
   /* sure the DO-variable for this (outer) loop is a member of the           */
   /* DO-variable list and, if so, capture some information about the	      */
   /* directive.						              */

   if (cdir_switches.interchange_sh_idx != NULL_IDX) {
      sh_idx = SH_PREV_IDX(BLK_FIRST_SH_IDX(blk_stk_idx));

      while (SH_COMPILER_GEN(sh_idx)  ||
             (SH_STMT_TYPE(sh_idx) == Directive_Stmt  &&
              IR_OPR(SH_IR_IDX(sh_idx)) != Interchange_Dir_Opr)) {
         sh_idx = SH_PREV_IDX(sh_idx);
      }

      if (sh_idx != cdir_switches.interchange_sh_idx) {

         /* Found a statement between the interchange dir and the do loop. */
 
         PRINTMSG(SH_GLB_LINE(sh_idx), 1381, Error, 0, "INTERCHANGE");
         SH_ERR_FLG(cdir_switches.interchange_sh_idx) = TRUE;
         cdir_switches.interchange_sh_idx             = NULL_IDX;
      }
      else { 
         BLK_INTERCHANGE_DIR_SH_IDX(blk_stk_idx) =
                                              cdir_switches.interchange_sh_idx;
         ir_idx  = SH_IR_IDX(cdir_switches.interchange_sh_idx);
         BLK_INTERCHANGE_NUM_LCVS(blk_stk_idx) = IR_LIST_CNT_L(ir_idx);
         cdir_switches.interchange_sh_idx      = NULL_IDX;
         dir_il_idx                            = IR_IDX_L(ir_idx);

         for (i = 1;  i <= IR_LIST_CNT_L(ir_idx);  ++i) {

            if (IL_IDX(dir_il_idx) == OPND_IDX(BLK_DO_VAR_OPND(blk_stk_idx))) {
               break;
            }
            else {
               dir_il_idx = IL_NEXT_LIST_IDX(dir_il_idx);
            }
         }

         if (i <= IR_LIST_CNT_L(ir_idx)) {

            /* Holds how many things are available in the list. */

            NTR_IR_LIST_TBL(il_idx);
            IR_FLD_R(ir_idx)      = IL_Tbl_Idx;
            IR_IDX_R(ir_idx)      = il_idx;
            IR_LIST_CNT_R(ir_idx) = 1;

            IL_LINE_NUM(il_idx) = TOKEN_LINE(token); 
            IL_COL_NUM(il_idx)  = TOKEN_COLUMN(token);
            IL_FLD(il_idx)      = CN_Tbl_Idx; 
            IL_IDX(il_idx)      = C_INT_TO_CN( CG_INTEGER_DEFAULT_TYPE, i);
         }
         else {

            /* Didn't find the do var in the interchange do var list */

            PRINTMSG(TOKEN_LINE(token), 1379, Error, TOKEN_COLUMN(token),
                     "INTERCHANGE");
            SH_ERR_FLG(BLK_INTERCHANGE_DIR_SH_IDX(blk_stk_idx)) = TRUE;
            BLK_INTERCHANGE_DIR_SH_IDX(blk_stk_idx) = NULL_IDX;
            BLK_INTERCHANGE_NUM_LCVS(blk_stk_idx)   = 0;
         }
      }
   }
   else {

      /* Try to find an interative DO Block Stack entry that is the subject   */
      /* of an INTERCHANGE directive so we know whether or not we should      */
      /* check to see if the current loop is perfectly nested.                */

      for (blk_idx = blk_stk_idx - 1;  blk_idx > 1;  --blk_idx) {
               
         if (BLK_TYPE(blk_idx) == Do_Blk  &&
             SH_STMT_TYPE(BLK_FIRST_SH_IDX(blk_idx)) == Do_Iterative_Stmt) {
            break;
         }
      }

      if (blk_idx > 1  &&  BLK_INTERCHANGE_NUM_LCVS(blk_idx) > 1) {

         if (loop_top_is_perfectly_nested("INTERCHANGE")) {
   
            /* The DO statement is perfectly nested within the containing DO  */
            /* loop.  If the current nest is subject to an INTERCHANGE	      */
            /* directive, set up the current and parent Block Stack entries   */
            /* to reflect the new loop and make sure the DO-variable of this  */
            /* loop is in the DO-variable list of the INTERCHANGE directive.  */

            if (BLK_INTERCHANGE_NUM_LCVS(blk_stk_idx - 1) > 1) {
               BLK_INTERCHANGE_NUM_LCVS(blk_stk_idx) =
                  BLK_INTERCHANGE_NUM_LCVS(blk_stk_idx - 1) - 1;
         
               blk_idx = blk_stk_idx - 1;
 
               while (BLK_INTERCHANGE_DIR_SH_IDX(blk_idx) == NULL_IDX) {
                  --blk_idx; 
               }

               ir_idx     = SH_IR_IDX(BLK_INTERCHANGE_DIR_SH_IDX(blk_idx));
               dir_il_idx = IR_IDX_L(ir_idx);

               for (i = 1;  i <= IR_LIST_CNT_L(ir_idx);  ++i) {

                  if (IL_IDX(dir_il_idx) ==
                         OPND_IDX(BLK_DO_VAR_OPND(blk_stk_idx))) {
                     break;
                  }
                  else {
                     dir_il_idx = IL_NEXT_LIST_IDX(dir_il_idx);
                  }
               }

               if (i <= IR_LIST_CNT_L(ir_idx)) {

                  /* We found the current DO-variable in the DO-variable list */
                  /* of the INTERCHANGE directive.  Add its position to the   */
                  /* position list.					      */

                  il_idx = IR_IDX_R(ir_idx);

                  while (IL_NEXT_LIST_IDX(il_idx) != NULL_IDX) {
                     il_idx = IL_NEXT_LIST_IDX(il_idx);
                  }

                  NTR_IR_LIST_TBL(il_idx_2);                  
                  IL_NEXT_LIST_IDX(il_idx)   = il_idx_2;
                  IL_PREV_LIST_IDX(il_idx_2) = il_idx;
                  ++IR_LIST_CNT_R(ir_idx);
                  IL_LINE_NUM(il_idx_2) = TOKEN_LINE(token);
                  IL_COL_NUM(il_idx_2)  = TOKEN_COLUMN(token);
                  IL_FLD(il_idx_2)      = CN_Tbl_Idx;
                  IL_IDX(il_idx_2)   = C_INT_TO_CN(CG_INTEGER_DEFAULT_TYPE, i);
               } 
               else {
                  PRINTMSG(TOKEN_LINE(token), 1379, Error, TOKEN_COLUMN(token),
                           "INTERCHANGE");
                  BLK_INTERCHANGE_NUM_LCVS(blk_stk_idx)   = 0;

                  blk_idx = blk_stk_idx - 1;

                  for (blk_idx = blk_stk_idx;  blk_idx > 1;  --blk_idx) {
                     BLK_INTERCHANGE_NUM_LCVS(blk_idx) = 0;
                     BLK_HAS_NESTED_LOOP(blk_idx)      = FALSE;

                     if (BLK_INTERCHANGE_DIR_SH_IDX(blk_idx) != NULL_IDX) {
                        SH_ERR_FLG(BLK_INTERCHANGE_DIR_SH_IDX(blk_idx)) = TRUE;
                        BLK_INTERCHANGE_DIR_SH_IDX(blk_idx)             =
                           NULL_IDX;
                     }
                  }
               }
            }
         }
      }
   }

   TRACE (Func_Exit, "process_interchange_dir", NULL);

   return;  

}  /* process_interchange_dir */

# if (defined(_TARGET_OS_IRIX) || defined(_TARGET_OS_LINUX) || defined(_TARGET_OS_DARWIN))

/******************************************************************************\
|*									      *|
|* Description:								      *|
|*	Do the processing to check for perfectly nested loops for PDO,        *|
|*      DOACROSS, and PARALLEL DO directives. 				      *|
|*									      *|
|* Input parameters:							      *|
|*	NONE								      *|
|*									      *|
|* Output parameters:							      *|
|*	NONE								      *|
|*									      *|
|* Returns:								      *|
|*	NOTHING								      *|
|*									      *|
\******************************************************************************/

static void check_mp_dir_nesting(void)

{
   int		blk_idx;
   int		col;
   int		dir_list_idx;
   int		i;
   int		line;
   int		list_idx;
   int		ir_idx;
   char		str[80];


   TRACE (Func_Entry, "check_mp_dir_nesting", NULL);

   /* If an -mp directive was specified ahead of this loop, make  	      */
   /* sure the DO-variable for this (outer) loop is a member of the           */
   /* DO-variable list and, if so, capture some information about the	      */
   /* directive.						              */

   if (cdir_switches.dir_nest_check_sh_idx != NULL_IDX) {
      BLK_DIR_NEST_CHECK_SH_IDX(blk_stk_idx) = 
                                     cdir_switches.dir_nest_check_sh_idx;
      ir_idx = SH_IR_IDX(cdir_switches.dir_nest_check_sh_idx);

      list_idx = IR_IDX_L(ir_idx);

      for (i = 0; i < MP_DIR_NEST_IDX; i++) {
         list_idx = IL_NEXT_LIST_IDX(list_idx);
      }

# ifdef _DEBUG
      if (list_idx == NULL_IDX ||
          IL_FLD(list_idx) != IL_Tbl_Idx) {
         PRINTMSG(SH_GLB_LINE(cdir_switches.dir_nest_check_sh_idx),
                  626, Internal, 
                  SH_COL_NUM(cdir_switches.dir_nest_check_sh_idx),
                  "IL_Tbl_Idx", "check_mp_dir_nesting");
      }
# endif

      BLK_DIR_NEST_CHECK_NUM_LCVS(blk_stk_idx) = IL_LIST_CNT(list_idx);
      cdir_switches.dir_nest_check_sh_idx         = NULL_IDX;
      dir_list_idx                             = IL_IDX(list_idx);

      /* this must be the first do var in the NEST clause */

      if (IL_IDX(dir_list_idx) != OPND_IDX(BLK_DO_VAR_OPND(blk_stk_idx))) {
             
         find_opnd_line_and_column(&IL_OPND(dir_list_idx), &line, &col);

         PRINTMSG(line, 1416, Error, col);
         SH_ERR_FLG(BLK_DIR_NEST_CHECK_SH_IDX(blk_stk_idx)) = TRUE;
         BLK_DIR_NEST_CHECK_SH_IDX(blk_stk_idx) = NULL_IDX;
         BLK_DIR_NEST_CHECK_NUM_LCVS(blk_stk_idx)   = 0;
      }
   }
   else {

      for (blk_idx = blk_stk_idx - 1;  blk_idx > 1;  --blk_idx) {
               
         if (BLK_TYPE(blk_idx) == Do_Blk  &&
             SH_STMT_TYPE(BLK_FIRST_SH_IDX(blk_idx)) == Do_Iterative_Stmt) {
            break;
         }
      }

      if (blk_idx > 1  &&  BLK_DIR_NEST_CHECK_NUM_LCVS(blk_idx) > 1) {

         blk_idx = blk_stk_idx - 1;

         while (blk_idx > 1 &&
                BLK_DIR_NEST_CHECK_SH_IDX(blk_idx) == NULL_IDX) {
            --blk_idx;
         }

         if (BLK_DIR_NEST_CHECK_SH_IDX(blk_idx) != NULL_IDX) {
            switch (IR_OPR(SH_IR_IDX(BLK_DIR_NEST_CHECK_SH_IDX(blk_idx)))) {
            case Pdo_Par_Opr:
               strcpy(str, "PDO");
               break;

            case Parallel_Do_Par_Opr:
               strcpy(str, "PARALLEL DO");
               break;

            case Doacross_Dollar_Opr:
               strcpy(str, "DOACROSS");
               break;

            default:
               strcpy(str, "DO");
               break;
            }
         }
         else {
            strcpy(str, "DO");
         }

         if (loop_top_is_perfectly_nested(str)) {
   
            /* The DO statement is perfectly nested within the containing DO  */
            /* loop. Set up the current and parent Block Stack entries   */
            /* to reflect the new loop and make sure the DO-variable of this  */
            /* loop is in the proper position in the NEST clause.  */

            if (BLK_DIR_NEST_CHECK_NUM_LCVS(blk_stk_idx - 1) > 1) {
               BLK_DIR_NEST_CHECK_NUM_LCVS(blk_stk_idx) =
                  BLK_DIR_NEST_CHECK_NUM_LCVS(blk_stk_idx - 1) - 1;
         
               blk_idx = blk_stk_idx - 1;
 
               while (blk_idx > 1 &&
                      BLK_DIR_NEST_CHECK_SH_IDX(blk_idx) == NULL_IDX) {
                  --blk_idx; 
               }

# ifdef _DEBUG
               if (blk_idx <= 1 ||
                   BLK_DIR_NEST_CHECK_SH_IDX(blk_idx) == NULL_IDX) {
                  PRINTMSG(SH_GLB_LINE(cdir_switches.dir_nest_check_sh_idx),
                           626, Internal,
                           SH_COL_NUM(cdir_switches.dir_nest_check_sh_idx),
                           "BLK_DIR_NEST_CHECK_SH_IDX", "check_mp_dir_nesting");
               }
# endif

               ir_idx     = SH_IR_IDX(BLK_DIR_NEST_CHECK_SH_IDX(blk_idx));

               /* Find the NEST clause */

               list_idx = IR_IDX_L(ir_idx);

               for (i = 0; i < MP_DIR_NEST_IDX; i++) {
                  list_idx = IL_NEXT_LIST_IDX(list_idx);
               }

# ifdef _DEBUG
               if (list_idx == NULL_IDX ||
                   IL_FLD(list_idx) != IL_Tbl_Idx) {
                  PRINTMSG(SH_GLB_LINE(cdir_switches.dir_nest_check_sh_idx),
                           626, Internal,
                           SH_COL_NUM(cdir_switches.dir_nest_check_sh_idx),
                           "IL_Tbl_Idx", "check_mp_dir_nesting");
               }
# endif

               dir_list_idx = IL_IDX(list_idx);

               for (i = 1;  
                    i < ((IL_LIST_CNT(list_idx) - 
                          BLK_DIR_NEST_CHECK_NUM_LCVS(blk_stk_idx)) + 1);
                    i++) {
 
                  dir_list_idx = IL_NEXT_LIST_IDX(dir_list_idx);
               }

               if (IL_IDX(dir_list_idx) != 
                          OPND_IDX(BLK_DO_VAR_OPND(blk_stk_idx))) {
             
                  find_opnd_line_and_column(&IL_OPND(dir_list_idx),&line,&col);

                  PRINTMSG(line, 1416, Error, col);
                  BLK_DIR_NEST_CHECK_NUM_LCVS(blk_stk_idx)   = 0;
                  blk_idx = blk_stk_idx - 1;

                  for (blk_idx = blk_stk_idx;  blk_idx > 1;  --blk_idx) {
                     BLK_DIR_NEST_CHECK_NUM_LCVS(blk_idx) = 0;
                     BLK_HAS_NESTED_LOOP(blk_idx)      = FALSE;

                     if (BLK_DIR_NEST_CHECK_SH_IDX(blk_idx) != NULL_IDX) {
                        SH_ERR_FLG(BLK_DIR_NEST_CHECK_SH_IDX(blk_idx)) = TRUE;
                        BLK_DIR_NEST_CHECK_SH_IDX(blk_idx)             =
                           NULL_IDX;
                     }
                  }
               }
            }
         }
      }
   }

   TRACE (Func_Exit, "check_mp_dir_nesting", NULL);

   return;  

}  /* check_mp_dir_nesting */

# endif

/******************************************************************************\
|*									      *|
|* Description:								      *|
|*	Check to see if the current iterative DO statement immediately        *|
|*      a preceding iterative DO statement.  That is, check to see if the     *|
|*      top of the current loop is perfectly nested within its containing     *|
|*      iterative DO loop.			      			      *|
|*									      *|
|* Input parameters:							      *|
|*	NONE								      *|
|*									      *|
|* Output parameters:							      *|
|*	NONE								      *|
|*									      *|
|* Returns:								      *|
|*	TRUE if the current DO statement immediately follows a preceding      *|
|*      iterative DO statement.						      *|
|*									      *|
\******************************************************************************/

static boolean loop_top_is_perfectly_nested(char *str)

{
   int		blk_idx;
   boolean	perfectly_nested	= FALSE;
   int		sh_idx;


   TRACE (Func_Entry, "loop_top_is_perfectly_nested", NULL);

   blk_idx = blk_stk_idx - 1;

   if (BLK_TYPE(blk_idx) == Do_Blk  &&
       SH_STMT_TYPE(BLK_FIRST_SH_IDX(blk_idx)) == Do_Iterative_Stmt) {
      sh_idx = SH_PREV_IDX(curr_stmt_sh_idx);

      while (SH_COMPILER_GEN(sh_idx)) {
         sh_idx = SH_PREV_IDX(sh_idx);
      } 

      if (SH_STMT_TYPE(sh_idx) == Do_Iterative_Stmt) {
         BLK_HAS_NESTED_LOOP(blk_idx) = TRUE;
         perfectly_nested = TRUE;
      }
   }
   else {
      sh_idx = BLK_FIRST_SH_IDX(blk_idx);
   } 

   if (! perfectly_nested) {
      PRINTMSG(SH_GLB_LINE(sh_idx), 1380, Error, SH_COL_NUM(sh_idx), str);

      for (blk_idx = blk_stk_idx;  blk_idx > 1;  --blk_idx) {

         if (BLK_TYPE(blk_idx) == Do_Blk) {
            BLK_INTERCHANGE_NUM_LCVS(blk_idx) = 0;
            BLK_DIR_NEST_CHECK_NUM_LCVS(blk_idx) = 0;
            BLK_HAS_NESTED_LOOP(blk_idx)      = 0;

            if (BLK_DIR_NEST_CHECK_SH_IDX(blk_idx) != NULL_IDX) {
               SH_ERR_FLG(BLK_DIR_NEST_CHECK_SH_IDX(blk_idx)) = TRUE;
               BLK_DIR_NEST_CHECK_SH_IDX(blk_idx)             = NULL_IDX;
            }

            if (BLK_INTERCHANGE_DIR_SH_IDX(blk_idx) != NULL_IDX) {
               SH_ERR_FLG(BLK_INTERCHANGE_DIR_SH_IDX(blk_idx)) = TRUE;
               BLK_INTERCHANGE_DIR_SH_IDX(blk_idx)             = NULL_IDX;
               break;
            }
         }
      }
   }

   TRACE (Func_Exit, "loop_top_is_perfectly_nested", NULL);

   return(perfectly_nested);

}  /* loop_top_is_perfectly_nested */


/******************************************************************************\
|*									      *|
|* Description:								      *|
|*	Create the endif opr for if processing.                               *|
|*									      *|
|* Input parameters:							      *|
|*	NONE								      *|
|*									      *|
|* Output parameters:							      *|
|*	NONE								      *|
|*									      *|
|* Returns:								      *|
|*	NOTHING								      *|
|*									      *|
\******************************************************************************/

static void gen_if_ir(int	if_stmt_lbl_idx,
		      int	if_stmt_sh_idx,
		      int	save_if_stmt_start_line,
		      int	save_if_stmt_start_col)

{
   int		ir_idx;


   TRACE (Func_Entry, "gen_if_ir", NULL);

   if (!SH_ERR_FLG(curr_stmt_sh_idx)  && !SH_ERR_FLG(if_stmt_sh_idx)) {

# if defined(_HIGH_LEVEL_IF_FORM)

      /* Generate an End_If_Stmt SH and mark it as compiler-generated  */
      /* (so that the IF stmt looks like a user IF construct so that   */
      /* all the IR generated to represent the <action-stmt> can be    */
      /* bracketed.  No need to generate a fake Then_Stmt SH because   */
      /* the Semantics Pass just deletes it anyway.		       */
      /* Do NOT pop the block stack entry.                             */

      gen_sh(After, End_If_Stmt, stmt_start_line, stmt_start_col,
             FALSE,                       /* Error flag.             */
             FALSE,                       /* Labeled.                */
             TRUE);                       /* Compiler-generated.     */
      SH_P2_SKIP_ME(curr_stmt_sh_idx) = TRUE;

      NTR_IR_TBL(ir_idx);
      IR_OPR(ir_idx)			= Endif_Opr;
      IR_TYPE_IDX(ir_idx)		= TYPELESS_DEFAULT_TYPE;
      IR_LINE_NUM(ir_idx)		= stmt_start_line;
      IR_COL_NUM(ir_idx)		= stmt_start_col;
      IR_FLD_L(ir_idx)			= SH_Tbl_Idx;
      IR_IDX_L(ir_idx)			= if_stmt_sh_idx;
      SH_IR_IDX(curr_stmt_sh_idx)	= ir_idx;
# else
      stmt_start_line = save_if_stmt_start_line;  
      stmt_start_col  = save_if_stmt_start_col;  

      gen_sh(After, Continue_Stmt, stmt_start_line, stmt_start_col,
             FALSE, TRUE, TRUE);

      NTR_IR_TBL(ir_idx);
      SH_IR_IDX(curr_stmt_sh_idx)       = ir_idx;
      IR_OPR(ir_idx)                    = Label_Opr;

      /* LRR - bhj put in short typeless for type idx */

      IR_TYPE_IDX(ir_idx)               = TYPELESS_DEFAULT_TYPE;
      IR_LINE_NUM(ir_idx)               = stmt_start_line;
      IR_COL_NUM(ir_idx)                = stmt_start_col;
      IR_FLD_L(ir_idx)                  = AT_Tbl_Idx;
      IR_IDX_L(ir_idx)                  = if_stmt_lbl_idx;
      IR_LINE_NUM_L(ir_idx)             = stmt_start_line;
      IR_COL_NUM_L(ir_idx)              = stmt_start_col;
      AT_DEFINED(if_stmt_lbl_idx)       = TRUE;
      ATL_DEF_STMT_IDX(if_stmt_lbl_idx) = curr_stmt_sh_idx;

# endif
   }
   else if (SH_ERR_FLG(curr_stmt_sh_idx)) {
      SH_ERR_FLG(if_stmt_sh_idx) = TRUE;
   }
   TRACE (Func_Exit, "gen_if_ir", NULL);

   return;
}
