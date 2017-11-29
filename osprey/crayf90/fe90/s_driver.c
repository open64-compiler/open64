/*
 * Copyright (C) 2010 Advanced Micro Devices, Inc.  All Rights Reserved.
 */

/*
 * Copyright (C) 2007, 2008. PathScale, LLC. All Rights Reserved.
 */
/*
 *  Copyright (C) 2006, 2007. QLogic Corporation. All Rights Reserved.
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



static char USMID[] = "\n@(#)5.0_pl/sources/s_driver.c	5.13	10/26/99 13:48:21\n";

# include "defines.h"		/* Machine dependent ifdefs */

# include "host.m"		/* Host machine dependent macros.*/
# include "host.h"		/* Host machine dependent header.*/
# include "target.m"		/* Target machine dependent macros.*/
# include "target.h"		/* Target machine dependent header.*/

# include "globals.m"
# include "tokens.m"
# include "sytb.m"
# include "s_globals.m"
# include "debug.m"

# include "globals.h"
# include "tokens.h"
# include "sytb.h"
# include "s_globals.h"
# include "s_driver.h"


/*****************************************************************\
|* Function prototypes of static functions declared in this file *|
\*****************************************************************/

static void	attr_link_resolution(void);
static void	check_and_allocate_common_storage(int);
static boolean	compare_global_args(int, int, int, int, int);
static boolean	compare_global_array(int, int, int);
static boolean	compare_global_derived_type(int, int, int);
static boolean	compare_global_type_rank(int, int, int, int, boolean);
#ifdef KEY /* Bug 5089 */
static void     decl_semantics_driver(boolean ieee_save);
#else /* KEY Bug 5089 */
static void     decl_semantics_driver(void);
#endif /* KEY Bug 5089 */
static void     free_stmt_tmp_tbl(void);
static void	final_attr_semantics(int);
static void	final_decl_semantics(void);
static void	final_equivalence_semantics(void);
static void	find_host_associated_attrs_in_il(int);
static void	find_host_associated_attrs_in_ir(int);
static void     init_call_structs(void);
static void	pgm_unit_semantics(void);
static void     reset_stmt_tmp_tbl(void);
static void	storage_blk_resolution(void);

# if (defined(_TARGET_OS_IRIX) || defined(_TARGET_OS_LINUX) || defined(_TARGET_OS_DARWIN))
static void	gen_user_code_start_opr(void);
static void	insert_global_sh(void);
# endif

# ifdef _SEPARATE_FUNCTION_RETURNS
static void	check_multiple_entry_func(void);
# endif


/***********************************\
|* Globals used only in this file  *|
\***********************************/

static	int	symbolic_constant_array_list;


/******************************************************************************\
|*									      *|
|* Description:								      *|
|*	This procedure is the semantics pass driver.  			      *|
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
|* Algorithm note:							      *|
|*      The global variable curr_scp_idx is saved and restored so that it can *|
|*      be used by the Semantics Pass routines and other utility routines     *|
|*      are used by both passes.  					      *|
|*									      *|
\******************************************************************************/

void semantics_pass_driver (void)

{
   int		save_curr_scp_idx;
   

   TRACE (Func_Entry, "semantics_pass_driver", NULL);

   /*  init_semantics_pass();  */

   init_call_structs();

   reset_stmt_tmp_tbl();

   /* reinitialize cdir_switches */

   init_directive(2);

   save_curr_scp_idx	= curr_scp_idx;
   pgm_unit_start_line	= SH_GLB_LINE(SCP_FIRST_SH_IDX(curr_scp_idx));

#ifdef KEY /* Bug 5089 */
   decl_semantics_driver(FALSE);
#else /* KEY Bug 5089 */
   decl_semantics_driver();
#endif /* KEY Bug 5089 */

   curr_scp_idx = save_curr_scp_idx;

# if (defined(_TARGET_OS_IRIX) || defined(_TARGET_OS_LINUX) || defined(_TARGET_OS_DARWIN))
   if (insert_global_directives &&
       global_stmt_sh_idx != NULL_IDX) {

      insert_global_sh();
   }
# endif
   pgm_unit_semantics();

   curr_scp_idx = save_curr_scp_idx;

   PRINT_EQV_TBL;

   TBL_FREE(equiv_tbl);

   /* free up the call site tables */

   if (arg_list != NULL) {
      MEM_FREE(arg_list);
      arg_list		= NULL;
      arg_list_size	= 0;
   }

   if (arg_info_list != NULL) {
      MEM_FREE(arg_info_list);
      arg_info_list	= NULL;
      arg_info_list_size	= 0;
   }

   /* free up the derived type compare table. */

   if (dt_cmp_tbl != NULL) {
      MEM_FREE(dt_cmp_tbl);
      dt_cmp_tbl	= NULL;
   }

   TRACE (Func_Exit, "semantics_pass_driver", NULL);

   return;

}  /*  semantics_pass_driver  */


/******************************************************************************\
|*									      *|
|* Description:								      *|
|*	This procedure visits all SCP entries for the current scope and any   *|
|*      contained scopes.  It drives the semantic analysis of all statements  *|
|*      by calling a semantic routine for each statement type (if the State-  *|
|*      ment Header is not marked in error).				      *|
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
|* Algorithm note:							      *|
|*      If the current scope contains a child scope, this procedure is called *|
|*      recursively to process the child's statements.  However, if the       *|
|*      current scope has a sibling scope, the statements are processed by    *|
|*      simply jumping to the top of this procedure.  Recursion is not used   *|
|*      for sibling scopes in order to reduce recursion on anticipated large  *|
|*      modules that implement programming libraries with many module         *|
|*      procedures in a single module (see for example the ISO Varying String *|
|*      Module). 							      *|
|*									      *|
\******************************************************************************/

static void pgm_unit_semantics (void)

{
#ifdef KEY /* Bug 10177 */
   boolean      actual_arg = FALSE;
   boolean      func_defined = FALSE;
   boolean      func_ptr_defined = FALSE;
#else /* KEY Bug 10177 */
   boolean      actual_arg;
   boolean      func_defined;
   boolean      func_ptr_defined;
#endif /* KEY Bug 10177 */
   int		idx;
   boolean	inline_it;
   boolean      is_function;
   int		pgm_attr_idx;
   int		save_curr_scp_idx;
   int		sh_idx;


   TRACE (Func_Entry, "pgm_unit_semantics", NULL);

PROCESS_SIBLING:

   TRACE (PU_Start, NULL, "Semantics");

   ATP_SCP_ALIVE(SCP_ATTR_IDX(curr_scp_idx))	= TRUE;
   idx						= SCP_ENTRY_IDX(curr_scp_idx);

   while (idx) {
      ATP_SCP_ALIVE(AL_ATTR_IDX(idx))		= TRUE;
      idx					= AL_NEXT_IDX(idx);
   }

   if (! SCP_IN_ERR(curr_scp_idx) ) {

      /* clear out the stmt_tmp_tbl for reusing short lived tmps. */

      free_stmt_tmp_tbl();

      curr_stmt_sh_idx	= SCP_FIRST_SH_IDX(curr_scp_idx);
      comp_phase	= Pass2_Semantics;

      while (curr_stmt_sh_idx != NULL_IDX) {
      
         if (SH_STMT_TYPE(curr_stmt_sh_idx) == Statement_Num_Stmt) {

            /* Set statement_number from the SH_PARENT_BLK_IDX field, get the */
            /* line and column for the last character of a DO loop            */
            /* (stmt_end_line and stmt_end_col are only used to produce the   */
            /* CIF Loop Definition record as of now), and delete the          */
            /* Statement_Number SH.					      */

            stmt_end_line    = SH_GLB_LINE(curr_stmt_sh_idx);
            stmt_end_col     = SH_COL_NUM(curr_stmt_sh_idx);
            statement_number = SH_PARENT_BLK_IDX(curr_stmt_sh_idx);
            sh_idx = curr_stmt_sh_idx;
            SH_PREV_IDX(SH_NEXT_IDX(sh_idx)) = SH_PREV_IDX(sh_idx);
            SH_NEXT_IDX(SH_PREV_IDX(sh_idx)) = SH_NEXT_IDX(sh_idx);
            curr_stmt_sh_idx = SH_NEXT_IDX(sh_idx);
            FREE_SH_NODE(sh_idx);
            continue;
         }

         TRACE_NEW_STMT ("Semantics");

         sh_idx = curr_stmt_sh_idx;

         if (!SH_ERR_FLG(curr_stmt_sh_idx)     &&
             !SH_P2_SKIP_ME(curr_stmt_sh_idx)) {
            stmt_type       = SH_STMT_TYPE(curr_stmt_sh_idx);
            stmt_start_line = SH_GLB_LINE(curr_stmt_sh_idx);
            stmt_start_col  = SH_COL_NUM(curr_stmt_sh_idx);

            (*stmt_semantics[SH_STMT_TYPE(curr_stmt_sh_idx)])();
         }
         else if (SH_STMT_TYPE(curr_stmt_sh_idx) == End_Where_Stmt) {
            /* must go to the end where stmt semantics routine anyway */
            /* since it does clean up for the where block (and nothing*/
            /* else ).                                                */

            stmt_type       = SH_STMT_TYPE(curr_stmt_sh_idx);
            stmt_start_line = SH_GLB_LINE(curr_stmt_sh_idx);
            stmt_start_col  = SH_COL_NUM(curr_stmt_sh_idx);

            (*stmt_semantics[SH_STMT_TYPE(curr_stmt_sh_idx)])();
         }

         /* reset expression descriptor tables to zero */
         
         arg_info_list_base = NULL_IDX;
         arg_info_list_top  = NULL_IDX;

         if (SH_DOALL_LOOP_END(sh_idx)) {
            doall_end_semantics();
         }

         if (SH_LOOP_END(sh_idx)) {
            gen_loop_end_ir();
         }
             
         curr_stmt_sh_idx = SH_NEXT_IDX(curr_stmt_sh_idx);
      }

      final_decl_semantics();

      PRINT_DBG_SYTB;		/* Print scp if SCP_DBG_PRINT_SYTB = TRUE */
      PRINT_DBG_STMT;		/* Print scp if SCP_DBG_PRINT_STMT = TRUE */
   }
   else if (cif_flags & BASIC_RECS) {

      /* CIF still wants output, even if the scope is in error.              */
      /* Check CIF option to see if symbol table needs to be written to CIF. */
      /* Need to use BASIC_RECS to output the Entry Info and Common Block    */
      /* records if the user just specifies "-cf".			     */

      cif_send_sytb();
   }

   if (SCP_FIRST_CHILD_IDX(curr_scp_idx) != NULL_IDX) {
      save_curr_scp_idx		= curr_scp_idx;
      curr_scp_idx		= SCP_FIRST_CHILD_IDX(curr_scp_idx);
      pgm_unit_semantics();
      curr_scp_idx		= save_curr_scp_idx;
   }

   /* if this scope is a function, check if the result var has been defined */
   /* if the function is a pointer, check that the pointer as been assigned */
   /* or allocated.  Clear ATP_SCP_ALIVE.                                   */

   pgm_attr_idx			= SCP_ATTR_IDX(curr_scp_idx);
   ATP_SCP_ALIVE(pgm_attr_idx)	= FALSE;
   is_function			= FALSE;

   if (ATP_PGM_UNIT(pgm_attr_idx) == Function  &&
       ! AT_DCL_ERR(pgm_attr_idx)              &&
       ! SCP_IN_ERR(curr_scp_idx))              {

      is_function	= TRUE;
#ifdef KEY /* Bug 7856 */
      /* If the result type is a default-initialized structure, suppress the
       * warning about not defining the function result. This errs in the
       * direction of silence if some but not all components of the structure
       * are default-initialized, but that's better than a spurious warning in
       * the likely case where all are initialized. */
      int typ_idx_tmp = NULL_IDX;
      func_defined = AT_DEFINED(pgm_attr_idx) ||
        (Derived_Type == AT_OBJ_CLASS(
	  typ_idx_tmp = TYP_IDX(ATD_TYPE_IDX(ATP_RSLT_IDX(pgm_attr_idx)))) &&
	ATT_DEFAULT_INITIALIZED(typ_idx_tmp));
#else /* KEY Bug 7856 */
      func_defined	= AT_DEFINED(pgm_attr_idx);
#endif /* KEY Bug 7856 */
      actual_arg	= AT_ACTUAL_ARG(pgm_attr_idx) || 
                          AT_ACTUAL_ARG(ATP_RSLT_IDX(pgm_attr_idx));
      func_ptr_defined	= ATD_PTR_ASSIGNED(ATP_RSLT_IDX(pgm_attr_idx));
   }

   idx	  = SCP_ENTRY_IDX(curr_scp_idx);

   inline_it = (opt_flags.inline_lvl > Inline_Lvl_0) || 
                ATP_MAY_INLINE(pgm_attr_idx);

   /* We keep more ir than we actually write out.  In the case of   */
   /* internal procedures, we want to use the current compile.      */  

   /* KAY  To get rid of the forward reference problem, we need     */
   /* to search for the internal procedures in the inline file,     */
   /* like we do with the module procedures.  We can get the name   */
   /* of the internal procedure's parent from the mangled procedure */
   /* name and search for it and then fill in the ATP_FIRST_SH_IDX. */

   while (idx) {

      if (is_function) {
         func_defined     |= AT_DEFINED(AL_ATTR_IDX(idx));
         actual_arg 	  |= AT_ACTUAL_ARG(AL_ATTR_IDX(idx)) ||
                             AT_ACTUAL_ARG(ATP_RSLT_IDX(AL_ATTR_IDX(idx)));
         func_ptr_defined |=ATD_PTR_ASSIGNED(ATP_RSLT_IDX(AL_ATTR_IDX(idx)));
      }

      ATP_SCP_ALIVE(AL_ATTR_IDX(idx))	 = FALSE;
      ATP_FIRST_SH_IDX(AL_ATTR_IDX(idx)) = (inline_it) ? 
                               SCP_FIRST_SH_IDX(curr_scp_idx) : NULL_IDX;
      idx = AL_NEXT_IDX(idx);
       
   }

   if (is_function && !actual_arg) {

      if (!func_defined) {
         PRINTMSG(AT_DEF_LINE(ATP_RSLT_IDX(pgm_attr_idx)), 287, Warning, 
                  AT_DEF_COLUMN(ATP_RSLT_IDX(pgm_attr_idx)),
                  AT_OBJ_NAME_PTR(ATP_RSLT_IDX(pgm_attr_idx)));
      }
      else if (ATD_POINTER(ATP_RSLT_IDX(pgm_attr_idx)) && !func_ptr_defined){
         PRINTMSG(AT_DEF_LINE(ATP_RSLT_IDX(pgm_attr_idx)), 918, Warning, 
                  AT_DEF_COLUMN(ATP_RSLT_IDX(pgm_attr_idx)),
                  AT_OBJ_NAME_PTR(ATP_RSLT_IDX(pgm_attr_idx)));
      }
   }

   if (ATP_PGM_UNIT(pgm_attr_idx) != Module) {
      ATP_FIRST_SH_IDX(pgm_attr_idx) = inline_it?SCP_FIRST_SH_IDX(curr_scp_idx):
                                                NULL_IDX;
   }

   if (SCP_SIBLING_IDX(curr_scp_idx) != NULL_IDX) {
      curr_scp_idx = SCP_SIBLING_IDX(curr_scp_idx);
      goto PROCESS_SIBLING;
   }

   TRACE (Func_Exit, "pgm_unit_semantics", NULL);

   return;

}  /*  pgm_unit_semantics  */

#ifdef KEY /* Bug 5089 */
/*
 * scp_idx	index into scp_tbl for a particular scope
 * returns	TRUE if that scope, or one of its parents, uses an intrinsic
 *		IEEE module; and if the scope indicated by scp_idx is a
 *		function or subroutine, and therefore needs to save and
 *		restore FP state
 */
static boolean
scp_or_parent_uses_ieee(int scp_idx) {
  for (int s = scp_idx; s; s = SCP_PARENT_IDX(s)) {
    if (SCP_USES_IEEE(s)) {
      int attr_idx = SCP_ATTR_IDX(scp_idx);
      pgm_unit_type put = ATP_PGM_UNIT(attr_idx);
      /* F2003 requires save/restore of FP state only for function or
       * subroutine, but with -apo the FP state doesn't start cleanly, so as
       * a favor to the user we call this in the main program too so the
       * flags start out clear.
       */
      if (Pgm_Unit == AT_OBJ_CLASS(attr_idx) &&
	(Function == put || Program == put || Subroutine == put)) {
	return TRUE;
      }
    }
  }
  return FALSE;
}
#endif /* KEY Bug 5089 */
/******************************************************************************\
|*									      *|
|* Description:								      *|
|*	This is the driver for decl_semantics. All scopes are processed.      *|
|*	NOTE:  The assumption is made that we go from outer scope to inner    *|
|*	       scope.  decl_semantics, name resolution and assign storage     *|
|*	       all require this.                                              *|
|*									      *|
|* Input parameters:							      *|
|*	ieee_save	scope accesses ieee_* intrinsic modules via host      *|
|*			association, thus needs to save and restore FPU state *|
|*									      *|
|* Output parameters:							      *|
|*	NONE								      *|
|*									      *|
|* Returns:								      *|
|*	NOTHING								      *|
|*									      *|
\******************************************************************************/

#ifdef KEY /* Bug 5089 */
static void decl_semantics_driver(boolean ieee_save)
#else /* KEY Bug 5089 */
static void decl_semantics_driver(void)
#endif /* KEY Bug 5089 */

{
   int          idx;
   int          save_curr_scp_idx;

   TRACE (Func_Entry, "decl_semantics_driver", NULL);

PROCESS_SIBLING:

   comp_phase                                   = Decl_Semantics;
   ATP_SCP_ALIVE(SCP_ATTR_IDX(curr_scp_idx))    = TRUE;
   idx                                          = SCP_ENTRY_IDX(curr_scp_idx);

   while (idx) {
      ATP_SCP_ALIVE(AL_ATTR_IDX(idx))           = TRUE;
      idx                                       = AL_NEXT_IDX(idx);
   }

# if (defined(_TARGET_OS_IRIX) || defined(_TARGET_OS_LINUX) || defined(_TARGET_OS_DARWIN)) 
   gen_user_code_start_opr();
# endif

   if (! SCP_IN_ERR(curr_scp_idx) ) {
      attr_link_resolution();
      curr_stmt_sh_idx  = SCP_FIRST_SH_IDX(curr_scp_idx);
      stmt_start_line   = SH_GLB_LINE(curr_stmt_sh_idx);
      stmt_start_col    = SH_COL_NUM(curr_stmt_sh_idx);
      need_new_sh       = TRUE;

      decl_semantics();
#ifdef KEY /* Bug 5089 */
      if (ieee_save || scp_or_parent_uses_ieee(curr_scp_idx)) {
	ieee_save = TRUE;
        gen_ieee_save_and_restore(curr_scp_idx, stmt_start_line,
	  stmt_start_col);
      }
#endif /* KEY Bug 5089 */

      if (cif_flags & BASIC_RECS) {
         cif_scope_info_rec();
      }

# ifdef _SEPARATE_FUNCTION_RETURNS
      if (ATP_PGM_UNIT(SCP_ATTR_IDX(curr_scp_idx)) == Function &&
          SCP_ALT_ENTRY_CNT(curr_scp_idx) != 0                 &&
          !ATD_IM_A_DOPE(ATP_RSLT_IDX(SCP_ATTR_IDX(curr_scp_idx))) &&
          ATD_ARRAY_IDX(ATP_RSLT_IDX(SCP_ATTR_IDX(curr_scp_idx))) == NULL_IDX &&
          TYP_TYPE(ATD_TYPE_IDX(ATP_RSLT_IDX(SCP_ATTR_IDX(curr_scp_idx)))) 
                                                      != Structure &&
          TYP_TYPE(ATD_TYPE_IDX(ATP_RSLT_IDX(SCP_ATTR_IDX(curr_scp_idx)))) 
                                                      != Character) {
 
         check_multiple_entry_func();
      }
# endif

   }

   if (SCP_FIRST_CHILD_IDX(curr_scp_idx) != NULL_IDX) {
      save_curr_scp_idx         = curr_scp_idx;
      curr_scp_idx              = SCP_FIRST_CHILD_IDX(curr_scp_idx);
#ifdef KEY /* Bug 5089 */
      decl_semantics_driver(ieee_save);
#else /* KEY Bug 5089 */
      decl_semantics_driver();
#endif /* KEY Bug 5089 */
      curr_scp_idx              = save_curr_scp_idx;
   }

   ATP_SCP_ALIVE(SCP_ATTR_IDX(curr_scp_idx)) = FALSE;

   idx = SCP_ENTRY_IDX(curr_scp_idx);

   while (idx) {

      ATP_SCP_ALIVE(AL_ATTR_IDX(idx))   = FALSE;
      idx                               = AL_NEXT_IDX(idx);
   }

   if (SCP_SIBLING_IDX(curr_scp_idx) != NULL_IDX) {
      curr_scp_idx = SCP_SIBLING_IDX(curr_scp_idx);
      goto PROCESS_SIBLING;
   }


   TRACE (Func_Exit, "decl_semantics_driver", NULL);

   return;

}  /* decl_semantics_driver */

/******************************************************************************\
|*                                                                            *|
|* Description:                                                               *|
|*      This procedure should never be called.  Its only purpose is to issue  *|
|*      an internal error message if a bad (0) value of curr_stmt_sh_idx is   *|
|*      encountered.							      *|
|*                                                                            *|
|* Input parameters:                                                          *|
|*      NONE                                                                  *|
|*                                                                            *|
|* Output parameters:                                                         *|
|*      NONE                                                                  *|
|*                                                                            *|
|* Returns:                                                                   *|
|*      NOTHING                                                               *|
|*                                                                            *|
\******************************************************************************/

void illegal_stmt_type (void)  

{

   TRACE (Func_Entry, "illegal_stmt_type", NULL);

   PRINTMSG(SH_GLB_LINE(curr_stmt_sh_idx), 263, Internal, 0);

   TRACE (Func_Exit, "illegal_stmt_type", NULL);

   return;

}  /* illegal_stmt_type */


/******************************************************************************\
|*                                                                            *|
|* Description:                                                               *|
|*      This procedure should never be called.  Its only purpose is to issue  *|
|*      an internal error message if a Statement Header is encountered for    *|
|*      which there is no semantic routine.    				      *|
|*                                                                            *|
|* Input parameters:                                                          *|
|*      NONE                                                                  *|
|*                                                                            *|
|* Output parameters:                                                         *|
|*      NONE                                                                  *|
|*                                                                            *|
|* Returns:                                                                   *|
|*      NOTHING                                                               *|
|*                                                                            *|
\******************************************************************************/

void no_semantics_routine (void)

{

   TRACE (Func_Entry, "no_semantics_routine", NULL);

   PRINTMSG(SH_GLB_LINE(curr_stmt_sh_idx), 278, Internal, 0,
            stmt_type_str[stmt_type]);

   TRACE (Func_Exit, "no_semantics_routine", NULL);

   return;

}  /* no_semantics_routine */

/******************************************************************************\
|*									      *|
|* Description:								      *|
|*	This routine goes through the local name table for the current scope. *|
|*	If the name is not locally defined(LN_DEF_LOC = FALSE), the host     *|
|*	symbol tables are researched to make sure that AT_ATTR_LINK is        *|
|*	pointing to the correct attribute entry.                              *|
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
static void	attr_link_resolution(void)
{
   int		attr_idx;
   int		host_idx;
   int		host_name_idx;
   int		local_attr_idx;
   int		local_name_idx;
   int		name_idx;
   int		rslt_idx;
   int		save_curr_scp_idx;
   boolean	save_host_dcl_err;
   int		sn_idx;
   int		ultimate_idx;
   int		ultimate_scp_idx;


   TRACE (Func_Entry, "attr_link_resolution", NULL);

   /* Do not need to go thru SCP_ATTR_LIST, because everything on that list */
   /* should be resolved.  At the end of pass1, it should be tmps and       */
   /* library calls.                                                        */

   for (name_idx = SCP_LN_FW_IDX(curr_scp_idx) + 1; 
        name_idx < SCP_LN_LW_IDX(curr_scp_idx); name_idx++) {

# ifdef _DEBUG
      if (name_idx < 0 || name_idx > loc_name_tbl_idx) {
         PRINTMSG(stmt_start_line, 34, Internal, stmt_start_col); 
      }
# endif

      attr_idx = LN_ATTR_IDX(name_idx);

# ifdef _DEBUG
      if (attr_idx <= 0 || attr_idx > attr_tbl_idx) {
         PRINTMSG(stmt_start_line, 34, Internal, stmt_start_col); 
      }

      if (LN_NAME_IDX(name_idx) != AT_NAME_IDX(attr_idx)) {
         PRINTMSG(AT_DEF_LINE(attr_idx), 516, Internal,
                  AT_DEF_COLUMN(attr_idx),
                  AT_OBJ_NAME_PTR(attr_idx),
                  name_idx,
                  attr_idx);
      }
# endif

      if (AT_REFERENCED(attr_idx) != Not_Referenced) {
         AT_REFERENCED(attr_idx) = Referenced;
      }

      if (ATP_PGM_UNIT(SCP_ATTR_IDX(curr_scp_idx)) == Module &&
          !AT_ACCESS_SET(attr_idx)) {

         /* Set to default access */

         AT_PRIVATE(attr_idx) = AT_PRIVATE(SCP_ATTR_IDX(curr_scp_idx));
      }
#ifdef KEY
      if (LN_DEF_LOC(name_idx) || 
          (AT_OBJ_CLASS(attr_idx) == Interface && 
           strncasecmp(AT_OBJ_NAME_PTR(attr_idx), "omp_",4)==0)) {
#else
      if (LN_DEF_LOC(name_idx)) { 
#endif
         continue;
      }

      host_idx = srch_host_sym_tbl(LN_NAME_PTR(name_idx),
                                   LN_NAME_LEN(name_idx),
                                   &host_name_idx,
                                   FALSE);

      if (host_idx == NULL_IDX) {
         AT_ATTR_LINK(attr_idx)	= NULL_IDX;	
         continue;
      }
      else if (IS_STMT_ENTITY(host_idx)) {

         /* Don't host associate a stmt entity. */

         AT_ATTR_LINK(attr_idx) = NULL_IDX;
         continue;
      }

      if (AT_OBJ_CLASS(attr_idx) == Derived_Type) {

         /* Derived type host association */

         if ((AT_OBJ_CLASS(host_idx) != Derived_Type &&
              !AT_DCL_ERR(attr_idx)) ||
            AT_NOT_VISIBLE(attr_idx)) {
            save_host_dcl_err = AT_DCL_ERR(host_idx);
            fnd_semantic_err(Obj_Use_Derived_Type,
                             AT_DEF_LINE(attr_idx),
                             AT_DEF_COLUMN(attr_idx),
                             host_idx,
                             TRUE);
            AT_DCL_ERR(attr_idx) = TRUE; 
            AT_DCL_ERR(host_idx) = save_host_dcl_err; 
            host_idx		 = NULL_IDX;  /* Break link */
         }
         else if (AT_OBJ_CLASS(host_idx) == Derived_Type) {
            AT_HOST_ASSOCIATED(attr_idx)	= TRUE;
            AT_HOST_ASSOCIATED(host_idx)	= TRUE;
            ATT_SCP_IDX(attr_idx)		= ATT_SCP_IDX(host_idx);
         }
      }
      else if (AT_OBJ_CLASS(attr_idx) == Pgm_Unit) {
         ultimate_idx = host_idx;

         while (AT_ATTR_LINK(ultimate_idx)) {
            ultimate_idx = AT_ATTR_LINK(ultimate_idx);
         }

         /* Find the scope of the ultimate_idx */

         save_curr_scp_idx	= curr_scp_idx;
         ultimate_scp_idx	= curr_scp_idx;

         while (1) {  /* If scope is an interface block we're not here. */
            curr_scp_idx	= SCP_PARENT_IDX(curr_scp_idx);

            if (curr_scp_idx == 0) {  /* Intrinsic scope - exit */
               ultimate_scp_idx	= NULL_IDX;
               break;
            }

            local_attr_idx = srch_sym_tbl(AT_OBJ_NAME_PTR(ultimate_idx),
                                          AT_NAME_LEN(ultimate_idx),
                                          &local_name_idx);

            if (local_attr_idx == ultimate_idx) {
               ultimate_scp_idx	= curr_scp_idx;
               break;
            }
         }

         curr_scp_idx		= save_curr_scp_idx;
         ATP_SCP_IDX(attr_idx)	= ultimate_scp_idx;

         /* if we can change the ultimate attr to a pgm unit we do */

         if (AT_OBJ_CLASS(ultimate_idx) == Data_Obj &&
             ! AT_USE_ASSOCIATED(ultimate_idx)) {

            if (!fnd_semantic_err((ATP_PGM_UNIT(attr_idx) == Subroutine ?
                                                           Obj_Use_Extern_Subr :
                                                           Obj_Use_Extern_Func),
                                   AT_DEF_LINE(ultimate_idx),
                                   AT_DEF_COLUMN(ultimate_idx),
                                   ultimate_idx,
                                   FALSE)) {  /* Check - don't issue message */

               if (ATP_PGM_UNIT(attr_idx) == Function &&
                   ATD_CLASS(ultimate_idx) != Dummy_Argument &&
                   TYP_TYPE(ATD_TYPE_IDX(ultimate_idx)) == Character &&
                   TYP_CHAR_CLASS(ATD_TYPE_IDX(ultimate_idx)) ==
                                            Assumed_Size_Char) {

                   /* This would be an illegal situation, so treat */
                   /* as if fnd_semantic_err returned TRUE.        */

                   /* Intentionally blank */
               }
               else {
                  chg_data_obj_to_pgm_unit(ultimate_idx, (pgm_unit_type)
                                           ATP_PGM_UNIT(attr_idx),
                                           Extern_Proc);
                  ATP_SCP_IDX(ultimate_idx)	= ultimate_scp_idx;

                  if (ATP_PGM_UNIT(ultimate_idx) == Function) {
                     rslt_idx = ATP_RSLT_IDX(ultimate_idx);
   
                     if (ATD_ARRAY_IDX(rslt_idx) != NULL_IDX ||
                         ATD_IM_A_DOPE(rslt_idx) ||
                         TYP_TYPE(ATD_TYPE_IDX(rslt_idx)) == Structure ||
                         TYP_TYPE(ATD_TYPE_IDX(rslt_idx)) == Character) {
   
                        ATP_EXTRA_DARG(ultimate_idx) = TRUE;
   
                        if (ATP_EXPL_ITRFC(ultimate_idx)) {
                           ATD_STOR_BLK_IDX(rslt_idx) = 
                                     SCP_SB_DARG_IDX(ATP_SCP_IDX(ultimate_idx));

                           /* Insert the function result as the zero'th darg */

                           if (ATP_FIRST_IDX(ultimate_idx) == NULL_IDX) {
                              NTR_SN_TBL(sn_idx);
                           }
                           else {
                              sn_idx = ATP_FIRST_IDX(ultimate_idx) - 1;
                           }
                           ATP_FIRST_IDX(ultimate_idx)	= sn_idx;
                           ATP_NUM_DARGS(ultimate_idx) += 1;
                           SN_NAME_LEN(sn_idx)	 = AT_NAME_LEN(rslt_idx);
                           SN_NAME_IDX(sn_idx)	 = AT_NAME_IDX(rslt_idx);
                           SN_ATTR_IDX(sn_idx)	 = rslt_idx;
                           SN_LINE_NUM(sn_idx)	 = AT_DEF_LINE(rslt_idx);
                           SN_COLUMN_NUM(sn_idx) = AT_DEF_COLUMN(rslt_idx);
                        }
                     }
                  }
               }
            }
         }
      }

      if (attr_idx == host_idx) {
         PRINTMSG(AT_DEF_LINE(attr_idx), 72, Internal, AT_DEF_COLUMN(attr_idx),
                  AT_OBJ_NAME_PTR(attr_idx), attr_idx);
      }

      AT_ATTR_LINK(attr_idx)	= host_idx;

      host_associated_attr_semantics(attr_idx, FALSE);
   }

   TRACE (Func_Exit, "attr_link_resolution", NULL);

   return;

}  /* attr_link_resolution */

/******************************************************************************\
|*									      *|
|* Description:								      *|
|*	This routine handles the storage blocks for host associated attrs.    *|
|*	All static storage blocks that will be host associated are copied     *|
|*	and linked into the current scope.  There is no link in either        *|
|*	direction.  The attr still points to the original storage block.      *|
|*	The storage block needs to be put into the current scope so that it   *|
|*	can be resolved by storage_blk_resolution before final_decl_semantics *|
|*	During the PDGCS interface, when an attribute is sent across that     *|
|*	references a storage block, not in the current scope, the current     *|
|*	is searched for the storage block.  Then this is the block sent to    *|
|*	PDG.  PDG_SB_IDX is updated for both the blocks so that the search    *|
|*	only has to be done once per block in a program unit.                 *|
|*	See send_stor_blk in i_cvrt.c for more details.                       *|
|*									      *|
|* Input parameters:							      *|
|*	attr_idx => The host associated attr with the stor blk that needs     *|
|*	            resolving.                                                *|
|*									      *|
|* Output parameters:							      *|
|*	NONE								      *|
|*									      *|
|* Returns:								      *|
|*	NONE								      *|
|*									      *|
\******************************************************************************/
void	host_associated_attr_semantics(int	attr_idx,
				       boolean	add_to_attr_list)

{
   int		bd_idx;
   boolean	defined;
   int		dim;
   int		eq_idx;
   int		first_eq;
   int		group_idx;
   int		il_idx;
   int		local_attr_idx;
   int		local_sb_idx;
   id_str_type	name;
   int		name_idx;
   char	       *name_ptr;
   int		new_attr_idx;
   int		new_host_assoc		= FALSE;
   int		new_scp;
   int		new_sn_idx;
   int		referenced;
   int		sb_idx;
   int		sn_idx;
   int		type_idx;


   TRACE (Func_Entry, "host_associated_attr_semantics", NULL);

   /* Do not need to accumulate referenced and defined flags from     */
   /* intermediate attrs, because when each attr is processed, its    */
   /* flags are set into the original attr.  Save the flags, so they  */
   /* can be set if the attr is host associated.                      */

   referenced		= AT_REFERENCED(attr_idx);
   defined		= AT_DEFINED(attr_idx);
   local_attr_idx	= attr_idx;

   while (AT_ATTR_LINK(attr_idx) != NULL_IDX) {
      attr_idx	= AT_ATTR_LINK(attr_idx);
   }

   switch (AT_OBJ_CLASS(attr_idx)) {
   case Data_Obj:

      if (ATD_CLASS(attr_idx) == Constant) {

         /* If this is a structure or array, make sure the tmp associated */
         /* with the structure constructor is host associated.   At the   */
         /* moment stuff needs to be filled in for the temp, so just make */
         /* sure that the storage block gets created in the local scope.  */

         if (ATD_FLD(attr_idx) == AT_Tbl_Idx) {
            host_associated_attr_semantics(ATD_CONST_IDX(attr_idx), TRUE);

            if (referenced) {
               AT_REFERENCED(ATD_CONST_IDX(attr_idx)) = Referenced;
            }
         }
         break;
      }

# if (defined(_TARGET_OS_IRIX) || defined(_TARGET_OS_LINUX) || defined(_TARGET_OS_DARWIN))
      if (ATD_IM_A_DOPE(attr_idx)                                   &&
             ATD_CLASS(attr_idx)                     == Dummy_Argument &&
             ATD_ARRAY_IDX(attr_idx)                                   &&
             BD_ARRAY_CLASS(ATD_ARRAY_IDX(attr_idx)) == Assumed_Shape &&
             ATD_SF_ARG_IDX(attr_idx) != NULL_IDX) {

         host_associated_attr_semantics(ATD_SF_ARG_IDX(attr_idx), TRUE);

         if (referenced) {
            AT_REFERENCED(ATD_SF_ARG_IDX(attr_idx)) = Referenced;
         }
      }
# endif

      sb_idx = ATD_STOR_BLK_IDX(attr_idx);

      if (sb_idx == NULL_IDX || SB_SCP_IDX(sb_idx) == curr_scp_idx) {
         break;
      }

      /* The only FUNCTION results that can be host associated are those  */
      /* that are parents of the current program unit.  If this is a      */
      /* reference to a pgm_unit defined in an interface block or in a    */
      /* sibling, this is a call to the program unit.  That causes new    */
      /* tmps to be created.                                              */

      if (ATD_CLASS(attr_idx) == Function_Result && 
          !ATP_SCP_ALIVE(ATD_FUNC_IDX(attr_idx))) {
          break;
      }
         
      switch (SB_BLK_TYPE(sb_idx)) {
      case Common:
      case Task_Common:
      case Threadprivate:

         /* These are NOT host associated.  The storage block is copied into */
         /* the scope and these are treated as if the block was declared in  */
         /* each program unit.  Copy the attr down and break the link.       */

         local_sb_idx = srch_stor_blk_tbl(SB_NAME_PTR(sb_idx),
                                          SB_NAME_LEN(sb_idx),
                                          curr_scp_idx);
         if (local_sb_idx != NULL_IDX && 
             SB_HOST_ASSOCIATED(local_sb_idx) &&
             SB_ORIG_SCP_IDX(sb_idx) == SB_ORIG_SCP_IDX(local_sb_idx) &&
             SB_ORIG_SCP_IDX(sb_idx) != NULL_IDX) {

            /* This storage block has already been host associated into this */
            /* scope.  So do nothing.  Intentionally blank.                  */
         }
         else {
            TBL_REALLOC_CK(stor_blk_tbl, 1);
            stor_blk_tbl[stor_blk_tbl_idx]		= stor_blk_tbl[sb_idx];
            SB_ORIG_SCP_IDX(stor_blk_tbl_idx)		= SB_SCP_IDX(sb_idx);
            SB_SCP_IDX(stor_blk_tbl_idx)		= curr_scp_idx;
            SB_HOST_ASSOCIATED(stor_blk_tbl_idx)	= TRUE;
            SB_COMMON_NEEDS_OFFSET(stor_blk_tbl_idx)	= FALSE;
        
            if (local_sb_idx != NULL_IDX) { 
               SB_HIDDEN(stor_blk_tbl_idx)		= TRUE;
               SB_MERGED_BLK_IDX(stor_blk_tbl_idx)	= local_sb_idx;

               if (!SB_USE_ASSOCIATED(local_sb_idx) || 
                   !SB_USE_ASSOCIATED(sb_idx) ||
                    SB_HAS_RENAMES(local_sb_idx) ||
                    SB_HAS_RENAMES(sb_idx) ||
                   (compare_names(AT_OBJ_NAME_LONG(SB_MODULE_IDX(local_sb_idx)),
                                  AT_NAME_LEN(SB_MODULE_IDX(local_sb_idx)),
                                  AT_OBJ_NAME_LONG(SB_MODULE_IDX(sb_idx)),
                                  AT_NAME_LEN(SB_MODULE_IDX(sb_idx))) != 0)) {
                  SB_DEF_MULT_SCPS(stor_blk_tbl_idx)	= TRUE;
                  SB_DEF_MULT_SCPS(sb_idx)		= TRUE;
               }
            }
            else if (SB_MODULE(stor_blk_tbl_idx)) {

               if (SB_USE_ASSOCIATED(stor_blk_tbl_idx)) {
                  ADD_ATTR_TO_LOCAL_LIST(SB_MODULE_IDX(stor_blk_tbl_idx));
               }
            }
            local_sb_idx			= stor_blk_tbl_idx;
         }
         break;

      case Static:
      case Static_Local:
      case Static_Named:
         if (SB_BLK_TYPE(sb_idx) == Static) {

            if (referenced) {
               AT_REFERENCED(attr_idx) = Referenced;
               AT_REF_IN_CHILD(attr_idx) = TRUE;
            }
         }

         /* These are NOT host associated.  This item needs to be in the     */
         /* host associated storage block for its scope.                     */

         if (!SB_USE_ASSOCIATED(sb_idx) &&
             (SB_BLK_TYPE(sb_idx) == Static_Local ||
              SB_BLK_TYPE(sb_idx) == Static_Named)) {
            new_scp = SB_SCP_IDX(sb_idx);

            if (SB_BLK_TYPE(sb_idx) == Static_Named) {

               if (SCP_SB_HOSTED_DATA_IDX(new_scp) == NULL_IDX) {
                  sb_idx = ntr_stor_blk_tbl(
                              SB_NAME_PTR(SCP_SB_STATIC_INIT_IDX(curr_scp_idx)),
                              SB_NAME_LEN(SCP_SB_STATIC_INIT_IDX(curr_scp_idx)),
                              AT_DEF_LINE(attr_idx),
                              AT_DEF_COLUMN(attr_idx),
                              Static);
   
                  name_ptr	= SB_NAME_PTR(sb_idx);
                  name_ptr[1]	= 'H';
                  name_ptr[2]	= 'O';
                  name_ptr[3]	= 'S';
                  name_ptr[4]	= 'T';

                  SB_SCP_IDX(sb_idx) = SB_SCP_IDX(ATD_STOR_BLK_IDX(attr_idx));
                  SB_ORIG_SCP_IDX(sb_idx) = 
                                  SB_ORIG_SCP_IDX(ATD_STOR_BLK_IDX(attr_idx));
                  SB_HOSTED_STATIC(sb_idx)		= TRUE;
                  SCP_SB_HOSTED_DATA_IDX(new_scp)	= sb_idx;
                  local_sb_idx				= NULL_IDX;
               }
               else {
                  sb_idx	= SCP_SB_HOSTED_DATA_IDX(new_scp);
                  local_sb_idx	= srch_stor_blk_tbl(SB_NAME_PTR(sb_idx),
                                                            SB_NAME_LEN(sb_idx),
                                                            curr_scp_idx);
               }
            }
            else if (SCP_SB_HOSTED_STATIC_IDX(new_scp) == NULL_IDX) {
               sb_idx = ntr_stor_blk_tbl(
                                   SB_NAME_PTR(SCP_SB_STATIC_IDX(curr_scp_idx)),
                                   SB_NAME_LEN(SCP_SB_STATIC_IDX(curr_scp_idx)),
                                   AT_DEF_LINE(attr_idx),
                                   AT_DEF_COLUMN(attr_idx),
                                   Static);

               name_ptr		= SB_NAME_PTR(sb_idx);
               name_ptr[1]	= 'H';
               name_ptr[2]	= 'O';
               name_ptr[3]	= 'S';
               name_ptr[4]	= 'T';

               SB_SCP_IDX(sb_idx) = SB_SCP_IDX(ATD_STOR_BLK_IDX(attr_idx));
               SB_ORIG_SCP_IDX(sb_idx) = 
                                  SB_ORIG_SCP_IDX(ATD_STOR_BLK_IDX(attr_idx));
               SB_HOSTED_STATIC(sb_idx)		= TRUE;
               SCP_SB_HOSTED_STATIC_IDX(new_scp)= sb_idx;
               local_sb_idx			= NULL_IDX;
            }
            else {
               sb_idx			= SCP_SB_HOSTED_STATIC_IDX(new_scp);
               local_sb_idx		= srch_stor_blk_tbl(SB_NAME_PTR(sb_idx),
                                                            SB_NAME_LEN(sb_idx),
                                                            curr_scp_idx);
            }

            ATD_STOR_BLK_IDX(attr_idx)	= sb_idx;

            /* We've switched to a new storage block.  If this object is */
            /* equivalenced, we need to switch everything in this group  */
            /* to this new storage block.                                */

            if (ATD_EQUIV(attr_idx)) {
               group_idx = SCP_FIRST_EQUIV_GRP(new_scp);

               while (group_idx != NULL_IDX) {
                  eq_idx	= group_idx;
                  first_eq	= eq_idx;
                  group_idx	= EQ_NEXT_EQUIV_GRP(group_idx);

                  while (eq_idx != NULL_IDX) {

                     if (EQ_ATTR_IDX(eq_idx) == attr_idx) { /* Found */
                        eq_idx		= first_eq;
                        group_idx	= NULL_IDX;

                        while (eq_idx != NULL_IDX) {
                           host_associated_attr_semantics(EQ_ATTR_IDX(eq_idx),
                                                          FALSE);
                           eq_idx = EQ_NEXT_EQUIV_OBJ(eq_idx);
                        }
                     }
                     else {
                        eq_idx = EQ_NEXT_EQUIV_OBJ(eq_idx);
                     }
                  }
               }
            }
         }
         else {
            local_sb_idx = srch_stor_blk_tbl(SB_NAME_PTR(sb_idx),
                                             SB_NAME_LEN(sb_idx),
                                             curr_scp_idx);
         }

         if (local_sb_idx != NULL_IDX && 
             SB_HOST_ASSOCIATED(local_sb_idx) &&
             SB_ORIG_SCP_IDX(sb_idx) == SB_ORIG_SCP_IDX(local_sb_idx) &&
             SB_ORIG_SCP_IDX(sb_idx) != NULL_IDX) {

            /* This storage block has already been host associated into this */
            /* scope.  So do nothing.  Intentionally blank.                  */
         }
         else {
            TBL_REALLOC_CK(stor_blk_tbl, 1);
            stor_blk_tbl[stor_blk_tbl_idx]		= stor_blk_tbl[sb_idx];
            SB_ORIG_SCP_IDX(stor_blk_tbl_idx)		= SB_SCP_IDX(sb_idx);
            SB_SCP_IDX(stor_blk_tbl_idx)		= curr_scp_idx;
            SB_HOST_ASSOCIATED(stor_blk_tbl_idx)	= TRUE;
            SB_COMMON_NEEDS_OFFSET(stor_blk_tbl_idx)	= FALSE;
        
            if (local_sb_idx != NULL_IDX) { 
               SB_HIDDEN(stor_blk_tbl_idx)		= TRUE;
               SB_MERGED_BLK_IDX(stor_blk_tbl_idx)	= local_sb_idx;

               if (!SB_USE_ASSOCIATED(local_sb_idx) || 
                   !SB_USE_ASSOCIATED(sb_idx) ||
                    SB_HAS_RENAMES(local_sb_idx) ||
                    SB_HAS_RENAMES(sb_idx) ||
                   (compare_names(AT_OBJ_NAME_LONG(SB_MODULE_IDX(local_sb_idx)),
                                  AT_NAME_LEN(SB_MODULE_IDX(local_sb_idx)),
                                  AT_OBJ_NAME_LONG(SB_MODULE_IDX(sb_idx)),
                                  AT_NAME_LEN(SB_MODULE_IDX(sb_idx))) != 0)) {
                  SB_DEF_MULT_SCPS(stor_blk_tbl_idx)	= TRUE;
                  SB_DEF_MULT_SCPS(sb_idx)		= TRUE;
               }
            }
            else if (SB_MODULE(stor_blk_tbl_idx)) {

               if (SB_USE_ASSOCIATED(stor_blk_tbl_idx)) {
                  ADD_ATTR_TO_LOCAL_LIST(SB_MODULE_IDX(stor_blk_tbl_idx));
               }
            }
            local_sb_idx = stor_blk_tbl_idx;
         }
         break;

      case Stack:

         if (!AT_HOST_ASSOCIATED(local_attr_idx)) {
            AT_HOST_ASSOCIATED(attr_idx)	= TRUE;
            AT_HOST_ASSOCIATED(local_attr_idx)	= TRUE;
            new_host_assoc			= TRUE;
            new_scp				= SB_SCP_IDX(sb_idx);

            if (SCP_SB_HOSTED_STACK_IDX(new_scp) == NULL_IDX) {
               CREATE_ID(name, sb_name[Stack_Host_Blk], sb_len[Stack_Host_Blk]);
               sb_idx = ntr_stor_blk_tbl(name.string,
                                         sb_len[Stack_Host_Blk],
                                         AT_DEF_LINE(attr_idx),
                                         AT_DEF_COLUMN(attr_idx),
                                         Stack);
               SB_SCP_IDX(sb_idx)		= new_scp;
               SB_HOSTED_STACK(sb_idx)		= TRUE;
               SCP_SB_HOSTED_STACK_IDX(new_scp)	= sb_idx;
            }
            ATD_STOR_BLK_IDX(attr_idx)	= SCP_SB_HOSTED_STACK_IDX(new_scp);
         }

         /* If this is a compiler tmp, it should be a host associated   */
         /* bounds tmp.  That means it is only referenced in the child. */

         if (defined && ATD_CLASS(attr_idx) != Compiler_Tmp) {
            AT_DEFINED(attr_idx)	= TRUE;
            AT_DEF_IN_CHILD(attr_idx)	= TRUE;
         }

         if (referenced) {
            AT_REFERENCED(attr_idx)	= Referenced;
            AT_REF_IN_CHILD(attr_idx)	= TRUE;
         }

         break;

      case Equivalenced:

         if (!AT_HOST_ASSOCIATED(local_attr_idx)) {
            new_host_assoc			= TRUE;
            AT_HOST_ASSOCIATED(attr_idx)	= TRUE;
            AT_HOST_ASSOCIATED(local_attr_idx)	= TRUE;
         }

         AT_DEFINED(attr_idx)		= AT_DEFINED(attr_idx) | defined;
         AT_DEF_IN_CHILD(attr_idx)	= AT_DEF_IN_CHILD(attr_idx) | defined;
         SB_HOSTED_STACK(sb_idx)	= TRUE;

         if (referenced) {
            AT_REFERENCED(attr_idx)	= Referenced;
            AT_REF_IN_CHILD(attr_idx)	= TRUE;
         }

         break;

      case Formal:

         if (!AT_HOST_ASSOCIATED(local_attr_idx)) {
            new_host_assoc			= TRUE;
            AT_HOST_ASSOCIATED(attr_idx)	= TRUE;
            AT_HOST_ASSOCIATED(local_attr_idx)	= TRUE;
         }

         AT_DEFINED(attr_idx)		= AT_DEFINED(attr_idx) | defined;
         AT_DEF_IN_CHILD(attr_idx)	= AT_DEF_IN_CHILD(attr_idx) | defined;

         if (referenced) {
            AT_REFERENCED(attr_idx)	= Referenced;
            AT_REF_IN_CHILD(attr_idx)	= TRUE;
         }
         break;

      case Based:

         if (ATD_AUTOMATIC(attr_idx)) {
            host_associated_attr_semantics(ATD_AUTO_BASE_IDX(attr_idx), TRUE);
         }
         else {       /* Should be a Cray_Pointee */
            host_associated_attr_semantics(ATD_PTR_IDX(attr_idx), TRUE);
         }

         if (!AT_HOST_ASSOCIATED(local_attr_idx)) {
            new_host_assoc			= TRUE;
            AT_HOST_ASSOCIATED(attr_idx)	= TRUE;
            AT_HOST_ASSOCIATED(local_attr_idx)	= TRUE;
         }

         AT_DEFINED(attr_idx)		= AT_DEFINED(attr_idx) | defined;
         AT_DEF_IN_CHILD(attr_idx)	= AT_DEF_IN_CHILD(attr_idx) | defined;

         if (referenced) {
            AT_REFERENCED(attr_idx)	= Referenced;
            AT_REF_IN_CHILD(attr_idx)	= TRUE;
         }

         /* Carry the local based storage on the local attr, so that it can */
         /* be passed through the interface.  We don't want to use the host */

         ATD_STOR_BLK_IDX(local_attr_idx) = SCP_SB_BASED_IDX(curr_scp_idx);
         break;

      default:

         if (!AT_HOST_ASSOCIATED(local_attr_idx)) {
            new_host_assoc			= TRUE;
            AT_HOST_ASSOCIATED(attr_idx)	= TRUE;
            AT_HOST_ASSOCIATED(local_attr_idx)	= TRUE;
         }
         AT_DEFINED(attr_idx)		= AT_DEFINED(attr_idx) | defined;
         AT_DEF_IN_CHILD(attr_idx)	= AT_DEF_IN_CHILD(attr_idx) | defined;

         if (referenced) {
            AT_REFERENCED(attr_idx)	= Referenced;
            AT_REF_IN_CHILD(attr_idx)	= TRUE;
         }
         break;
      }  /* End switch */

      if (new_host_assoc) {  /* This attr is now host associated */

         if (ATD_CLASS(attr_idx) == Variable &&
             ATD_FLD(attr_idx) != NO_Tbl_Idx) {

            /* This has data initialized tmps associated with it.  These can */
            /* get here if the attr is use associated and then hosted.       */

            if (ATD_FLD(attr_idx) == AT_Tbl_Idx) {
               host_associated_attr_semantics(ATD_VARIABLE_TMP_IDX(attr_idx), 
                                              TRUE);
            }
            else if (ATD_FLD(attr_idx) == IL_Tbl_Idx) {

               /* Must be structure - Have a list of tmps */

               il_idx = ATD_VARIABLE_TMP_IDX(attr_idx);

               while (il_idx != NULL_IDX) {
                  host_associated_attr_semantics(IL_IDX(il_idx), TRUE); 
                  il_idx = IL_NEXT_LIST_IDX(il_idx);
               }
            }
         }

         type_idx = ATD_TYPE_IDX(attr_idx);
   
         if (TYP_TYPE(type_idx) == Character &&
             TYP_FLD(type_idx) == AT_Tbl_Idx) {
            host_associated_attr_semantics(TYP_IDX(type_idx), TRUE);
         }
   
         bd_idx = ATD_ARRAY_IDX(attr_idx);
   
         if (bd_idx != NULL_IDX &&
             BD_ARRAY_CLASS(bd_idx) != Deferred_Shape &&
             BD_ARRAY_SIZE(bd_idx) != Constant_Size) {
   
            for (dim = 1; dim <= BD_RANK(bd_idx); dim++) {
   
               if (BD_LB_FLD(bd_idx, dim) == AT_Tbl_Idx) {
                  host_associated_attr_semantics(BD_LB_IDX(bd_idx, dim), TRUE);
               }
   
               if (BD_UB_FLD(bd_idx, dim) == AT_Tbl_Idx) {
                  host_associated_attr_semantics(BD_UB_IDX(bd_idx, dim), TRUE);
               }
   
               if (BD_XT_FLD(bd_idx, dim) == AT_Tbl_Idx) {
                  host_associated_attr_semantics(BD_XT_IDX(bd_idx, dim), TRUE);
               }
      
               if (BD_SM_FLD(bd_idx, dim) == AT_Tbl_Idx) {
                  host_associated_attr_semantics(BD_SM_IDX(bd_idx, dim), TRUE);
               }
            }
   
            if (BD_LEN_FLD(bd_idx) == AT_Tbl_Idx) {
               host_associated_attr_semantics(BD_LEN_IDX(bd_idx), TRUE);
            }
         }
      }
      break;

   case Pgm_Unit:

      /* If ATP_SCP_ALIVE is set, then we are host associating the host's     */
      /* function result or we are calling the host.  Since we don't know for */
      /* sure what is going on, assume the function result is host associated.*/

      AT_DEFINED(attr_idx)	= AT_DEFINED(attr_idx) | defined;
      AT_DEF_IN_CHILD(attr_idx)	= AT_DEF_IN_CHILD(attr_idx) | defined;

      if (referenced) {
         AT_REFERENCED(attr_idx)	= Referenced;
         AT_REF_IN_CHILD(attr_idx)	= TRUE;
      }

      if (ATP_PGM_UNIT(attr_idx) == Function &&
          ATP_SCP_ALIVE(attr_idx) && !ATP_RSLT_NAME(attr_idx)) {
         host_associated_attr_semantics(ATP_RSLT_IDX(attr_idx), FALSE);
      }
      break;

   case Namelist_Grp:

      COPY_ATTR_NTRY(local_attr_idx, attr_idx);

      /* Note that AT_CIF_SYMBOL_ID remains as the symbol id of the name in   */
      /* the host.  We need this to make all references resolve to the host   */
      /* so that CIF processing only sees one namelist group name (the one in */
      /* the host).							      */

      AT_ATTR_LINK(local_attr_idx)		= NULL_IDX;
      AT_REFERENCED(local_attr_idx)		= referenced;
      AT_DEFINED(local_attr_idx)		= defined;
      AT_HOST_ASSOCIATED(local_attr_idx)	= TRUE;

      if (ATN_NAMELIST_DESC(attr_idx) != NULL_IDX) {
         host_associated_attr_semantics(ATN_NAMELIST_DESC(attr_idx), TRUE);
      }

      sn_idx		= ATN_FIRST_NAMELIST_IDX(attr_idx);
      new_sn_idx	= NULL_IDX;

      while (sn_idx != NULL_IDX) {

         if (new_sn_idx == NULL_IDX) {
            NTR_SN_TBL(new_sn_idx);
            ATN_FIRST_NAMELIST_IDX(local_attr_idx)	= new_sn_idx;
         }
         else {
            NTR_SN_TBL(name_idx);
            SN_SIBLING_LINK(new_sn_idx)	= name_idx;
            new_sn_idx			= name_idx;
         }

         sec_name_tbl[new_sn_idx]	= sec_name_tbl[sn_idx];

         local_attr_idx = srch_sym_tbl(AT_OBJ_NAME_PTR(SN_ATTR_IDX(sn_idx)),
                                       AT_NAME_LEN(SN_ATTR_IDX(sn_idx)),
                                       &name_idx);

         if (local_attr_idx != NULL_IDX && 
             AT_ATTR_LINK(local_attr_idx) != NULL_IDX) {

            new_attr_idx = AT_ATTR_LINK(local_attr_idx);

            while (AT_ATTR_LINK(new_attr_idx) != NULL_IDX) {
               new_attr_idx = AT_ATTR_LINK(new_attr_idx);
            }

            if (new_attr_idx != SN_ATTR_IDX(sn_idx)) {

               /* If the attr indexes are equal, the attr_idx has already */
               /* been host associated into this scope, so use that attr. */
               /* Otherwise, host associate this attr into this scope.    */

               NTR_ATTR_TBL(local_attr_idx);
               AT_ATTR_LINK(local_attr_idx) = SN_ATTR_IDX(sn_idx);
               host_associated_attr_semantics(SN_ATTR_IDX(sn_idx), FALSE);
            }
         }
         else {

            /* Something by this name has not been host associated into   */
            /* this scope, so attr link this and host associate it in.    */

            NTR_ATTR_TBL(local_attr_idx);
            AT_ATTR_LINK(local_attr_idx) = SN_ATTR_IDX(sn_idx);
            host_associated_attr_semantics(SN_ATTR_IDX(sn_idx), FALSE);
         }

         SN_ATTR_IDX(new_sn_idx)	= local_attr_idx;
         sn_idx				= SN_SIBLING_LINK(sn_idx);
      }

      break;

   case Interface:

      /* KAY - What does this mean.  If there is a program by the same name  */
      /*       as the interface, how do we know its been referenced?   Plus  */
      /*       interfaces are concatted together.  Research this.            */
      /*       What about referenced and defined?                            */

      break;

   case Stmt_Func:

      /* Need to check the statement function body for anything else that is */
      /* host associated.  ie:  Traverse the IR.                             */

      switch (ATS_SF_FLD(attr_idx)) {
      case AT_Tbl_Idx:
         host_associated_attr_semantics(ATS_SF_IDX(attr_idx), TRUE);
         break;

      case IR_Tbl_Idx:
         find_host_associated_attrs_in_ir(ATS_SF_IDX(attr_idx));
         break;

      case IL_Tbl_Idx:
         find_host_associated_attrs_in_il(ATS_SF_IDX(attr_idx));
         break;
      }
      break;
   }

   if (add_to_attr_list) {
      ADD_ATTR_TO_LOCAL_LIST(local_attr_idx);
   }
   
   TRACE (Func_Exit, "host_associated_attr_semantics", NULL);

   return;

}  /* host_associated_attr_semantics */

/******************************************************************************\
|*									      *|
|* Description:								      *|
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

static	void	find_host_associated_attrs_in_ir(int	ir_idx)

{

   TRACE (Func_Entry, "find_host_associated_attrs_in_ir", NULL);

   switch (IR_FLD_L(ir_idx)) {
   case AT_Tbl_Idx:
      host_associated_attr_semantics(IR_IDX_L(ir_idx), TRUE);
      break;

   case IR_Tbl_Idx:
      find_host_associated_attrs_in_ir(IR_IDX_L(ir_idx));
      break;

   case IL_Tbl_Idx:
      find_host_associated_attrs_in_il(IR_IDX_L(ir_idx));
      break;

   case CN_Tbl_Idx:
   case NO_Tbl_Idx:
   case SH_Tbl_Idx:
      break;
   }

   switch (IR_FLD_R(ir_idx)) {
   case AT_Tbl_Idx:
      host_associated_attr_semantics(IR_IDX_R(ir_idx), TRUE);
      break;

   case IR_Tbl_Idx:
      find_host_associated_attrs_in_ir(IR_IDX_R(ir_idx));
      break;

   case IL_Tbl_Idx:
      find_host_associated_attrs_in_il(IR_IDX_R(ir_idx));
      break;

   case CN_Tbl_Idx:
   case NO_Tbl_Idx:
   case SH_Tbl_Idx:
      break;
   }

   TRACE (Func_Exit, "find_host_associated_attrs_in_ir", NULL);

   return;

}  /* find_host_associated_attrs_in_ir */

/******************************************************************************\
|*									      *|
|* Description:								      *|
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

static	void	find_host_associated_attrs_in_il(int	list_idx)

{
   TRACE (Func_Entry, "find_host_associated_attrs_in_il", NULL);

   while (list_idx != NULL_IDX) {

      switch (IL_FLD(list_idx)) {
      case AT_Tbl_Idx:
         host_associated_attr_semantics(IL_IDX(list_idx), TRUE);
         break;

      case IR_Tbl_Idx:
         find_host_associated_attrs_in_ir(IL_IDX(list_idx));
         break;

      case IL_Tbl_Idx:
         find_host_associated_attrs_in_il(IL_IDX(list_idx));
         break;

      case NO_Tbl_Idx:
      case SH_Tbl_Idx:
      case CN_Tbl_Idx:
         break;
      }
      list_idx = IL_NEXT_LIST_IDX(list_idx);
   }

   TRACE (Func_Exit, "find_host_associated_attrs_in_il", NULL);

   return;

}  /* find_host_associated_attrs_in_il */

/******************************************************************************\
|*									      *|
|* Description:								      *|
|*	initialize the zero'd arg_info struct and the exp_desc struct for     *|
|*      call list processing and expr_semantics.                              *|
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

static void init_call_structs(void)

{
   int		i;

   TRACE (Func_Entry, "init_call_structs", NULL);

   init_exp_desc.type_idx         = TYPELESS_DEFAULT_TYPE;
   init_exp_desc.rank             = 0;
   init_exp_desc.cif_id           = 0;
   init_exp_desc.type             = Typeless;
   init_exp_desc.linear_type	  = Err_Res;
   init_exp_desc.kind0seen        = FALSE;
   init_exp_desc.kind0D0seen      = FALSE;
   init_exp_desc.percent_val_arg  = FALSE;
   init_exp_desc.constant         = FALSE;
   init_exp_desc.foldable         = FALSE;
   init_exp_desc.will_fold_later  = FALSE;
   init_exp_desc.pointer          = FALSE;
   init_exp_desc.target           = FALSE;
   init_exp_desc.vector_subscript = FALSE;
   init_exp_desc.reference        = FALSE;
   init_exp_desc.constructor      = FALSE;
   init_exp_desc.component        = FALSE;
   init_exp_desc.section          = FALSE;
   init_exp_desc.label            = FALSE;
   init_exp_desc.array_elt        = FALSE;
   init_exp_desc.assumed_shape    = FALSE;
   init_exp_desc.assumed_size     = FALSE;
   init_exp_desc.allocatable      = FALSE;
   init_exp_desc.dope_vector      = FALSE;
   init_exp_desc.tmp_reference    = FALSE;
   init_exp_desc.has_constructor  = FALSE;
   init_exp_desc.optional_darg    = FALSE;
   init_exp_desc.pe_dim_ref       = FALSE;
   init_exp_desc.contig_array     = FALSE;
   init_exp_desc.shape_known      = FALSE;
   init_exp_desc.tree_has_ranf    = FALSE;
   init_exp_desc.has_symbolic	  = FALSE;
   init_exp_desc.dist_reshape_ref = FALSE;
   init_exp_desc.constructor_size_level = Unknown_Expr_Size;

   init_exp_desc.char_len = null_opnd;

   for (i = 0; i < 7; i++) {
      init_exp_desc.shape[i] = null_opnd;
   }

   init_arg_info.ed = init_exp_desc;

   init_arg_info.kwd                 = NULL_IDX;
   init_arg_info.line                = 0;
   init_arg_info.col                 = 0;
   init_arg_info.association         = 0;
   init_arg_info.arg_opnd            = null_opnd;
   init_arg_info.pgm_unit            = FALSE;
   init_arg_info.maybe_modified      = TRUE;

   TRACE (Func_Exit, "init_call_structs", NULL);

   return;

}  /* init_call_structs */

/******************************************************************************\
|*									      *|
|* Description:								      *|
|*	Check for a pending align cdir for a referenced user label.           *|
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

void label_def_stmt_semantics(void)

{
   int		label_idx;

   TRACE (Func_Entry, "label_def_stmt_semantics", NULL);

   label_idx = IR_IDX_L(SH_IR_IDX(curr_stmt_sh_idx));

   if (ATL_CLASS(label_idx) == Lbl_User &&
       AT_REFERENCED(label_idx) == Referenced && cdir_switches.align) {

      ATL_ALIGN(label_idx)	= TRUE;
      cdir_switches.align	= FALSE;
   }

   if (! cdir_switches.vector) {
      ATL_NOVECTOR(label_idx)  = TRUE;
   }

# if (defined(_TARGET_OS_IRIX) || defined(_TARGET_OS_LINUX) || defined(_TARGET_OS_DARWIN))
   if (cdir_switches.notask_region) {
      ATL_NOTASK(label_idx)    = TRUE;
   }
# else
   if (! cdir_switches.task) {
      ATL_NOTASK(label_idx)    = TRUE;
   }
# endif

   if (! cdir_switches.vsearch) {
      ATL_NOVSEARCH(label_idx) = TRUE;
   }

   if (cdir_switches.bl) {
      ATL_BL(label_idx) = TRUE;
   }

   if (! cdir_switches.recurrence) {
      ATL_NORECURRENCE(label_idx) = TRUE;
   }

   if (cdir_switches.pattern) {
      ATL_PATTERN(label_idx) = TRUE;
   }

   TRACE (Func_Exit, "label_def_stmt_semantics", NULL);

   return;

}  /* label_def_stmt_semantics */

/******************************************************************************\
|*									      *|
|* Description:								      *|
|*	This assigns storage offsets at the end of the semantics pass.        *|
|*	This assumes parents are processed before children.  This also        *|
|*	assumes that tmps are assigned after all declared variables.  This is *|
|*	needed for the data initialization array optimization.  ATD_TMP_IDX   *|
|*	on the compiler tmps with ATD_DATA_INIT set is a pointer to the       *|
|*	variable that is being data initialized.                              *|
|*	If CIF records are being generated, it generates OBJECT records for   *|
|*	everything in the local name table that CIF wants.                    *|
|*									      *|
|* Input parameters:							      *|
|*	 NONE								      *|
|*									      *|
|* Output parameters:							      *|
|*	 NONE								      *|
|*									      *|
|* Returns:								      *|
|*	 NONE								      *|
|*									      *|
\******************************************************************************/
static 	void	final_decl_semantics(void)

{
   int			al_idx;
   int			attr_idx;
   int			name_idx;
   int			symbolic_constant	= NULL_IDX;


# if defined(_ASSIGN_OFFSETS_TO_HOSTED_STACK)
   size_offset_type     length;
   int                  list_idx;
   size_offset_type     result;
   int                  sb_idx;
# endif


   TRACE (Func_Entry, "final_decl_semantics", NULL);

   /* This holds a list of all arrays whose bounds are based on symbolic */
   /* constants that need to have offsets assigned.  (Except for common  */
   /* blocks.)  This ensures that in static and module blocks, all the   */
   /* symbolic constant based arrays are at the end of the block.  Then  */
   /* anything in the block that is not a symbolic constant based array  */
   /* can be equivalenced or data initialized.                           */

   symbolic_constant_array_list	= NULL_IDX;

   /* Mark the start of the assign label chain, in case this procedure      */
   /* ends up going out for USE processing and coming back in for inlining. */
   /* We can then find the start of the chain without having a scope table. */

   if (SCP_ASSIGN_LBL_CHAIN(curr_scp_idx) != NULL_IDX) {
      ATL_ASG_LBL_CHAIN_START(SCP_ASSIGN_LBL_CHAIN(curr_scp_idx))	= TRUE;
   }

   /* Do final processing of equivalence groups, it there are no errors.    */

   if (num_prog_unit_errors == 0) {
      final_equivalence_semantics();
   }

   storage_blk_resolution();

   for (name_idx = SCP_LN_FW_IDX(curr_scp_idx) + 1; 
        name_idx < SCP_LN_LW_IDX(curr_scp_idx); name_idx++) {

      attr_idx = LN_ATTR_IDX(name_idx);

      if (!AT_DCL_ERR(attr_idx)) {

         if (AT_OBJ_CLASS(attr_idx) == Data_Obj &&
             ATD_SYMBOLIC_CONSTANT(attr_idx) &&
             (ATD_CLASS(attr_idx) == Constant ||
              ATD_CLASS(attr_idx) == Variable)) {
            symbolic_constant = attr_idx;
         }
         else {
            final_attr_semantics(attr_idx);
         }
      }
   }


   if (symbolic_constant != NULL_IDX &&
       (ATD_CLASS(symbolic_constant) == Constant ||
        AT_REFERENCED(symbolic_constant) == Not_Referenced)) {

      /* Remove N$PES from the name table here.  This is needed so N$PES   */
      /* doesn't cause problems in MODULE processing.                      */

      srch_sym_tbl(AT_OBJ_NAME_PTR(symbolic_constant),
                   AT_NAME_LEN(symbolic_constant),
                   &name_idx);

      remove_ln_ntry(name_idx);
   }

   al_idx	= SCP_ATTR_LIST(curr_scp_idx);

   while (al_idx != NULL_IDX) {

      if (!AT_DCL_ERR(AL_ATTR_IDX(al_idx))) {
         final_attr_semantics(AL_ATTR_IDX(al_idx));
      }

      al_idx = AL_NEXT_IDX(al_idx);
   }

   al_idx	= symbolic_constant_array_list;

   while (al_idx != NULL_IDX) {
      assign_offset(AL_ATTR_IDX(al_idx));  /* symbolic constants */
      al_idx = AL_NEXT_IDX(al_idx);
   }

# if defined(_ASSIGN_OFFSETS_TO_HOSTED_STACK)

   if (SCP_SB_HOSTED_STACK_IDX(curr_scp_idx) != NULL_IDX) {

      /* Have stack items that are hosted.  Need to add a tmp for the */
      /* block length.  Any time this hosted stack is used, the tmp   */
      /* must be host associated as well.  Special case is the zero   */
      /* block length.  We do not need a tmp, because if nothing is   */
      /* hosted, the block doesn't get in.  If one or more things are */
      /* hosted, then the length will still be correct at zero.       */

      sb_idx			= SCP_SB_HOSTED_STACK_IDX(curr_scp_idx);

      /* KAY - If this is set to AT_Tbl_Idx, it should not be 0. */

      if (SB_LEN_FLD(sb_idx) == AT_Tbl_Idx ||
          fold_relationals(SB_LEN_IDX(sb_idx), CN_INTEGER_ZERO_IDX, Ne_Opr)) {

         result.idx	= SB_LEN_IDX(sb_idx);
         result.fld	= SB_LEN_FLD(sb_idx);

         align_bit_length(&result, TARGET_BITS_PER_WORD);

         if (result.fld == NO_Tbl_Idx) {
            result.fld	= CN_Tbl_Idx;
            result.idx	= ntr_const_tbl(result.type_idx,
                                        FALSE, 
                                        result.constant);
         }

         SB_LEN_FLD(sb_idx)	= result.fld;
         SB_LEN_IDX(sb_idx)	= result.idx;
         attr_idx		= gen_compiler_tmp(SB_DEF_LINE(sb_idx),
                                                   SB_DEF_COLUMN(sb_idx),
                                                   Priv, TRUE);

         ATD_TYPE_IDX(attr_idx)		= TYPELESS_DEFAULT_TYPE;
         ATD_STOR_BLK_IDX(attr_idx)	= sb_idx;
         ATD_OFFSET_ASSIGNED(attr_idx)	= TRUE;
         AT_REFERENCED(attr_idx)	= Referenced;  /* Force thru PDGCS */
         AT_REF_IN_CHILD(attr_idx)	= TRUE;        /* Force thru PDGCS */
         NTR_ATTR_LIST_TBL(list_idx);
         AL_ATTR_IDX(list_idx)		= attr_idx;
         SB_LAST_ATTR_LIST(sb_idx)	= list_idx;

         /* This must be at least one word length, because  */
         /* the bit length has been word aligned.           */

         length.fld	= CN_Tbl_Idx;
         length.idx	= CN_INTEGER_BITS_PER_WORD_IDX;

         if (!size_offset_binary_calc(&result, &length, Minus_Opr, &result)) {
            AT_DCL_ERR(attr_idx) = TRUE;
         }

         if (result.fld == NO_Tbl_Idx) {
            ATD_OFFSET_FLD(attr_idx) = CN_Tbl_Idx;
            ATD_OFFSET_IDX(attr_idx) = ntr_const_tbl(result.type_idx,
                                                     FALSE,
                                                     result.constant);
         }
         else {
            ATD_OFFSET_FLD(attr_idx) = result.fld;
            ATD_OFFSET_IDX(attr_idx) = result.idx;
         }
      }
   }
# endif

   /* Check CIF option to see if symbol table needs to be written to CIF.  */
   /* Need to use BASIC_RECS to output the Entry Info and Common Block     */
   /* records if the user just specifies "-cf".			           */

   if (cif_flags & BASIC_RECS) {
      cif_send_sytb();
   }

   TRACE (Func_Exit, "final_decl_semantics", NULL);

   return;

}  /* final_decl_semantics */

/******************************************************************************\
|*                                                                            *|
|* Description:                                                               *|
|*      Allocates storage and offsets for all COMMON blocks.                  *|
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
static void	final_attr_semantics(int	attr_idx)

{
   		int			al_idx;
   		int			darg_idx;
   		int			i;
   		int			il_idx;
   		int			local_attr_idx;
   		int			rslt_idx;
   		int			sb_idx;
   		int			sn_idx;

   		int			type_idx;

# if defined(_ASSIGN_OFFSETS_TO_HOSTED_STACK)
   static	int			tmp_scp_idx	= NULL_IDX;
# endif

# if defined(_TMP_GIVES_COMMON_LENGTH)
   		size_offset_type	length;
   		size_offset_type	result;
		size_offset_type	size;
   		size_offset_type	zero;
# endif

   TRACE (Func_Entry, "final_attr_semantics", NULL);

   if (AT_ATTR_LINK(attr_idx) == NULL_IDX || AT_IGNORE_ATTR_LINK(attr_idx)) {

      switch (AT_OBJ_CLASS(attr_idx)) {
      case Data_Obj:

         if (ATD_EQUIV_LIST(attr_idx) != NULL_IDX) {
            free_attr_list(ATD_EQUIV_LIST(attr_idx));
            ATD_EQUIV_LIST(attr_idx)	= NULL_IDX;
         }

         if (ATD_NO_ENTRY_LIST(attr_idx) != NULL_IDX) {
            free_attr_list(ATD_NO_ENTRY_LIST(attr_idx));
            ATD_NO_ENTRY_LIST(attr_idx)	= NULL_IDX;
         }

         switch (ATD_CLASS(attr_idx)) {
         case Constant:

# ifdef _DEBUG
            if (ATD_FLD(attr_idx) == NO_Tbl_Idx) {
               PRINTMSG(AT_DEF_LINE(attr_idx), 893, Internal, 
                        AT_DEF_COLUMN(attr_idx),
                        "ATD_CONST_IDX",
                        "ATD_FLD",
                        "attr_tbl",
                        attr_idx);
            }
# endif

            /* Mark the overlay tmp as Referenced.  Have to delay until this */
            /* point, because the tmp is not created yet.  Can't do it when  */
            /* the tmp is created, because this also must be done for        */
            /* use associated constants.                                     */

            if (ATD_FLD(attr_idx) == AT_Tbl_Idx && 
               AT_REFERENCED(attr_idx) != Not_Referenced) {
               AT_REFERENCED(ATD_CONST_IDX(attr_idx)) = Referenced;
            }

            attr_idx = NULL_IDX;
            break;

         case Struct_Component:
            attr_idx = NULL_IDX;
            break;

         case Function_Result:  /* These are done, when the Function is */
            attr_idx = NULL_IDX;
            break;

         case Compiler_Tmp:
# ifdef _DEBUG
            if (ATD_FLD(attr_idx) == NO_Tbl_Idx &&
                ATD_TMP_IDX(attr_idx) != NULL_IDX) {
               PRINTMSG(AT_DEF_LINE(attr_idx), 893, Internal, 
                        AT_DEF_COLUMN(attr_idx),
                        "ATD_TMP_IDX",
                        "ATD_FLD",
                        "attr_tbl",
                        attr_idx);
            }

# endif
            if (ATD_TMP_INIT_NOT_DONE(attr_idx) &&
                ATP_PGM_UNIT(SCP_ATTR_IDX(curr_scp_idx)) == Module) {
 
                /* Have a default init temp.  Generate the init */
                /* in the module, so it is ready to be used     */
                /* wherever the module is used.                 */

               insert_init_stmt_for_tmp(attr_idx);
            }

            sb_idx = ATD_STOR_BLK_IDX(attr_idx);

# if defined(_TMP_GIVES_COMMON_LENGTH)

            if (AT_REFERENCED(attr_idx) == Not_Referenced &&
                !ATD_OFFSET_ASSIGNED(attr_idx) &&
                sb_idx != NULL_IDX &&
                (SB_USE_ASSOCIATED(sb_idx) || SB_HOST_ASSOCIATED(sb_idx)) ) {

               /* If block length is zero, we do not need the tmp, so do  */
               /* not set AT_REFERENCED and it will not get sent across.  */

               if (SB_LEN_FLD(sb_idx) == CN_Tbl_Idx &&
                   fold_relationals(SB_LEN_IDX(sb_idx), 
                                    CN_INTEGER_ZERO_IDX, Ne_Opr)) {
                  size.fld	= CN_Tbl_Idx;
                  size.idx	= CN_INTEGER_BITS_PER_WORD_IDX;
                  length.fld	= SB_LEN_FLD(sb_idx);
                  length.idx	= SB_LEN_IDX(sb_idx);

                  size_offset_binary_calc(&length, &size, Mod_Opr, &size);

                  /* Size should always be less than TARGET_BITS_PER_WORD */

                  zero.fld	= CN_Tbl_Idx;
                  zero.idx	= CN_INTEGER_ZERO_IDX;

                  size_offset_logical_calc(&size, &zero, Eq_Opr, &result);

                  if (THIS_IS_TRUE(result.constant, result.type_idx)) {
                     size.idx	= CN_INTEGER_BITS_PER_WORD_IDX;
                     size.fld	= CN_Tbl_Idx;
                  }

                  CLEAR_TBL_NTRY(type_tbl, TYP_WORK_IDX);
                  TYP_TYPE(TYP_WORK_IDX)	= Typeless;
                  TYP_BIT_LEN(TYP_WORK_IDX)	= (size.fld == CN_Tbl_Idx) ?
                      CN_INT_TO_C(size.idx) : F_INT_TO_C(size.constant,
                                                TYP_LINEAR(size.type_idx));
                  ATD_TYPE_IDX(attr_idx)	= ntr_type_tbl();

                  if (!size_offset_binary_calc(&length,
                                               &size,
                                                Minus_Opr, 
                                               &result)) {
                     AT_DCL_ERR(attr_idx) = TRUE;
                  }

                  if (result.fld == NO_Tbl_Idx) {
                     ATD_OFFSET_FLD(attr_idx) = CN_Tbl_Idx;
                     ATD_OFFSET_IDX(attr_idx) = ntr_const_tbl(result.type_idx,
                                                              FALSE,
                                                              result.constant);
                  }
                  else {
                     ATD_OFFSET_FLD(attr_idx) = result.fld;
                     ATD_OFFSET_IDX(attr_idx) = result.idx;
                  }
                
                  ATD_OFFSET_ASSIGNED(attr_idx)	= TRUE;
                  AT_REFERENCED(attr_idx)	= Referenced;
               }

# ifdef _DEBUG
               if (ATD_OFFSET_ASSIGNED(attr_idx) &&
                   ATD_OFFSET_FLD(attr_idx) == CN_Tbl_Idx &&
                   fold_relationals(ATD_OFFSET_IDX(attr_idx),
                                    CN_INTEGER_ZERO_IDX,
                                    Lt_Opr)) {
                  PRINTMSG(AT_DEF_LINE(attr_idx), 1004, Internal, 
                           AT_DEF_COLUMN(attr_idx),
                           AT_OBJ_NAME_PTR(attr_idx),
                           attr_idx);
               }
# endif

                /* KAY - Is this in the correct spot * */

               if (ATD_DEFINING_ATTR_IDX(attr_idx) == NULL_IDX &&
                   ATD_FLD(attr_idx) == IR_Tbl_Idx &&
                   IR_FLD_R(ATD_TMP_IDX(attr_idx)) == AT_Tbl_Idx &&
                   AT_OBJ_CLASS(IR_IDX_R(ATD_TMP_IDX(attr_idx))) == Data_Obj &&
                   ATD_CLASS(IR_IDX_R(ATD_TMP_IDX(attr_idx))) == Compiler_Tmp) {
                  ATD_DEFINING_ATTR_IDX(attr_idx) =
                         ATD_DEFINING_ATTR_IDX(IR_IDX_R(ATD_TMP_IDX(attr_idx)));
               }

               attr_idx				= NULL_IDX;
            }
# endif
            break;

         case Variable:

            if (ATD_SYMBOLIC_CONSTANT(attr_idx) && 
                AT_REFERENCED(attr_idx) == Referenced) {
               PRINTMSG(AT_DEF_LINE(attr_idx), 1229, Ansi, 
                        AT_DEF_COLUMN(attr_idx),
                        AT_OBJ_NAME_PTR(attr_idx));
            }
               
            if (ATD_FLD(attr_idx) != NO_Tbl_Idx) {

               /* This has data initialized tmps associated with it */

               if (ATD_FLD(attr_idx) == AT_Tbl_Idx) {
                  final_attr_semantics(ATD_VARIABLE_TMP_IDX(attr_idx));
               }
               else if (ATD_FLD(attr_idx) == IL_Tbl_Idx) {

                  /* Must be structure - Have a list of tmps */

                  il_idx = ATD_VARIABLE_TMP_IDX(attr_idx);

                  while (il_idx != NULL_IDX) {
                     final_attr_semantics(IL_IDX(il_idx)); 
                     il_idx = IL_NEXT_LIST_IDX(il_idx);
                  }
               }
            }

            /* Intentional fall through */

         default:
            sb_idx	= ATD_STOR_BLK_IDX(attr_idx);

            if (sb_idx != NULL_IDX) {
               type_idx	= ATD_TYPE_IDX(attr_idx);

               if (SB_VOLATILE(sb_idx)) {
                  ATD_VOLATILE(attr_idx) = TRUE;
               }

               if (ATD_EQUIV_IN_BNDS_EXPR(attr_idx) && 
                   !AT_HOST_ASSOCIATED(attr_idx) &&
                   !AT_USE_ASSOCIATED(attr_idx) &&
                   !SB_IS_COMMON(sb_idx) &&
                   !ATD_SYMBOLIC_CONSTANT(attr_idx)) {

                  /* If final_equivalence_semantics isn't called, then */
                  /* we may issue this message in bogus situations.    */

                  if (SCP_FIRST_EQUIV_GRP(curr_scp_idx) == NULL_IDX ||
                      num_prog_unit_errors == 0) {
                     PRINTMSG(AT_DEF_LINE(attr_idx), 521, Error,
                              AT_DEF_COLUMN(attr_idx),
                              AT_OBJ_NAME_PTR(attr_idx));
                  }
               }

               if (SB_AUXILIARY(sb_idx)) {

                  if (AT_NAMELIST_OBJ(attr_idx) && SB_IS_COMMON(sb_idx)) {
                     PRINTMSG(AT_DEF_LINE(attr_idx), 663, Error,
                              AT_DEF_COLUMN(attr_idx),
                              AT_OBJ_NAME_PTR(attr_idx),
                              SB_NAME_PTR(sb_idx));
                  }

                  if (!ATD_AUXILIARY(attr_idx) && 
                      SB_BLK_TYPE(sb_idx) != Formal) {

                     /* Formal dargs do not cause all the dargs to become aux */

                     if (TYP_TYPE(type_idx) == Character) {
                        PRINTMSG(AT_DEF_LINE(attr_idx), 535, Error,
                                 AT_DEF_COLUMN(attr_idx),
                                 AT_OBJ_NAME_PTR(attr_idx));
                        AT_DCL_ERR(attr_idx)  = TRUE;
                     }
                     else if (TYP_TYPE(type_idx) == Structure &&
                              (ATT_POINTER_CPNT(TYP_IDX(type_idx)) ||
#ifdef KEY /* Bug 6845 */
                              ATT_ALLOCATABLE_CPNT(TYP_IDX(type_idx)) ||
#endif /* KEY Bug 6845 */
                               ATT_CHAR_CPNT(TYP_IDX(type_idx)))) {
                        PRINTMSG(AT_DEF_LINE(attr_idx), 536, Error,
                                 AT_DEF_COLUMN(attr_idx),
                                 AT_OBJ_NAME_PTR(attr_idx),
                                 AT_OBJ_NAME_PTR(TYP_IDX(type_idx)));
                        AT_DCL_ERR(attr_idx)  = TRUE;
                     }
                     else if (ATD_TARGET(attr_idx) ||
                              ATD_DATA_INIT(attr_idx) ||
                              ATD_POINTER(attr_idx) ||
                              TYP_TYPE(type_idx) == CRI_Ptr) {
                        fnd_semantic_err(Obj_Auxiliary, 
                                         AT_DEF_LINE(attr_idx),
                                         AT_DEF_COLUMN(attr_idx), 
                                         attr_idx,
                                         TRUE);
                     }
                     else {
                        ATD_AUXILIARY(attr_idx)	= TRUE;
                     }
                  }
               }
            }
            break;
         }

         break;

      case Pgm_Unit:

         if (attr_idx != SCP_ATTR_IDX(curr_scp_idx) &&
             ATP_IN_INTERFACE_BLK(attr_idx) &&
             !AT_HOST_ASSOCIATED(attr_idx) &&
             !AT_USE_ASSOCIATED(attr_idx)) {

            attr_idx = NULL_IDX;
            break;
         }

         switch (ATP_PGM_UNIT(attr_idx)) {
         case Function:
         case Pgm_Unknown:
         case Subroutine:

            if (ATP_GLOBAL_ATTR_IDX(attr_idx) == NULL_IDX &&
                ATP_EXPL_ITRFC(attr_idx) &&
                !AT_COMPILER_GEND(attr_idx) &&
                !ATP_NAME_IN_STONE(attr_idx) &&
                (ATP_PROC(attr_idx) == Unknown_Proc ||
                 ATP_PROC(attr_idx) == Extern_Proc ||
                 ATP_PROC(attr_idx) == Imported_Proc) &&
                !AT_IS_INTRIN(attr_idx) &&
                (attr_idx != glb_tbl_idx[Main_Attr_Idx])) {

               /* This has not been entered or resolved globally yet.  */
               /* This routine checks for this name in the global name */
               /* table.  It enters it, if it doesn't exist or checks  */
               /* for semantics errors if it does exist.               */

               check_global_pgm_unit(attr_idx);
            }

            if (ATP_NO_ENTRY_LIST(attr_idx) != NULL_IDX) {
               free_attr_list(ATP_NO_ENTRY_LIST(attr_idx));
               ATP_NO_ENTRY_LIST(attr_idx)	= NULL_IDX;
            }

            if (ATP_PROC(attr_idx) == Module_Proc) {

               if (ATP_PGM_UNIT(SCP_ATTR_IDX(curr_scp_idx)) == Module &&
                   !AT_PRIVATE(attr_idx) && !AT_DCL_ERR(attr_idx)) {

                  /* The function result type and all dummy argument types */
                  /* must be public types, if the procedure is public.     */

                  if (ATP_PGM_UNIT(attr_idx) == Function) {
                     type_idx = ATD_TYPE_IDX(ATP_RSLT_IDX(attr_idx));

                     if (TYP_TYPE(type_idx) == Structure &&
                         AT_PRIVATE(TYP_IDX(type_idx)) &&
                        !AT_USE_ASSOCIATED(TYP_IDX(type_idx)) ) {

                        /* Issue error if the Module procedure is PUBLIC,  */
                        /* but its function result is a PRIVATE type.      */
                        /* Unless interp 161 applies.                      */

                        PRINTMSG(AT_DEF_LINE(attr_idx), 684, Error,
                                 AT_DEF_COLUMN(attr_idx),
                                 AT_OBJ_NAME_PTR(attr_idx));
                        AT_DCL_ERR(attr_idx)	= TRUE;
                     }
                  }

                  for (i = (ATP_EXTRA_DARG(attr_idx) ? 1 : 0);
                       i < ATP_NUM_DARGS(attr_idx); i++) {

                     darg_idx = SN_ATTR_IDX(ATP_FIRST_IDX(attr_idx) + i);

                     if (AT_DCL_ERR(darg_idx)) {
                        continue;
                     }


                     /* Issue error if the Module procedure is PUBLIC, but */
                     /* one of its dummy arguments is a PRIVATE type.      */

                     if (AT_OBJ_CLASS(darg_idx) == Interface) {
                        darg_idx = ATI_PROC_IDX(darg_idx);
                     }

                     if (darg_idx != NULL_IDX &&
                        AT_OBJ_CLASS(darg_idx) == Pgm_Unit) {

                        if (ATP_PGM_UNIT(darg_idx) == Function) {
                           darg_idx = ATP_RSLT_IDX(darg_idx);
                        }
                        else {
                           darg_idx = NULL_IDX;
                        }
                     }

                     if (darg_idx != NULL_IDX &&
                         TYP_TYPE(ATD_TYPE_IDX(darg_idx)) == Structure &&
                         AT_PRIVATE(TYP_IDX(ATD_TYPE_IDX(darg_idx))) &&
                         !AT_USE_ASSOCIATED(TYP_IDX(ATD_TYPE_IDX(darg_idx))) ) {
                        PRINTMSG(AT_DEF_LINE(darg_idx), 685, Error,
                                 AT_DEF_COLUMN(darg_idx),
                                 AT_OBJ_NAME_PTR(attr_idx),
                                 AT_OBJ_NAME_PTR(darg_idx));
                        AT_DCL_ERR(attr_idx)	= TRUE;
                     }
                  }
               }
            }

            if (!AT_USE_ASSOCIATED(attr_idx)) {

               if (ATP_PROC(attr_idx) == Unknown_Proc) {
                  ATP_PROC(attr_idx) = Extern_Proc;
               }

               if (ATP_EXT_NAME_IDX(attr_idx) == NULL_IDX) {
# ifdef _DEBUG
                  PRINTMSG(AT_DEF_LINE(attr_idx), 193, Internal,
                           AT_DEF_COLUMN(attr_idx),
                           0, "ATP_EXT_NAME_IDX", attr_idx);
# endif
                  MAKE_EXTERNAL_NAME(attr_idx,
                                     AT_NAME_IDX(attr_idx),
                                     AT_NAME_LEN(attr_idx));
               }

               ATP_ALL_INTENT_IN(attr_idx) = TRUE;

               sn_idx = (ATP_EXTRA_DARG(attr_idx) && ATP_EXPL_ITRFC(attr_idx)) ?
                         ATP_FIRST_IDX(attr_idx)+1: ATP_FIRST_IDX(attr_idx);

               for (;sn_idx < (ATP_FIRST_IDX(attr_idx)+ATP_NUM_DARGS(attr_idx));
                     sn_idx++) {

                  if (AT_OBJ_CLASS(SN_ATTR_IDX(sn_idx)) != Data_Obj ||
                      ATD_CLASS(SN_ATTR_IDX(sn_idx)) != Dummy_Argument ||
                      ATD_INTENT(SN_ATTR_IDX(sn_idx)) != Intent_In) {
                     ATP_ALL_INTENT_IN(attr_idx) = FALSE;
                     break;
                  }
               }
            }

            if (ATP_HAS_ALT_RETURN(attr_idx)) {

               if (ATP_RSLT_IDX(attr_idx) == NULL_IDX) {

                  /* Create a function result as required */
                  /* by the PDGCS interface.              */

                  NTR_ATTR_TBL(rslt_idx);
                  COPY_ATTR_NTRY(rslt_idx, attr_idx);
                  CLEAR_VARIANT_ATTR_INFO(rslt_idx, Data_Obj);
                  ATD_CLASS(rslt_idx)	     = Function_Result;
                  ATD_TYPE_IDX(rslt_idx)     = CG_INTEGER_DEFAULT_TYPE;
                  ATD_STOR_BLK_IDX(rslt_idx) = SCP_SB_STACK_IDX(curr_scp_idx);
                  ATP_RSLT_IDX(attr_idx)     = rslt_idx;
               }
               attr_idx	= NULL_IDX;
            }
            else {
               attr_idx = ATP_RSLT_IDX(attr_idx);
            }
            break;

         case Blockdata:
         case Program:

            if (ATP_NO_ENTRY_LIST(attr_idx) != NULL_IDX) {
               free_attr_list(ATP_NO_ENTRY_LIST(attr_idx));
               ATP_NO_ENTRY_LIST(attr_idx)	= NULL_IDX;
            }

            /* Intentional fall through to next case */

         case Module:

            if (ATP_GLOBAL_ATTR_IDX(attr_idx) == NULL_IDX &&
                !AT_COMPILER_GEND(attr_idx) &&
                (attr_idx != glb_tbl_idx[Main_Attr_Idx]) &&
                (ATP_PGM_UNIT(attr_idx) != Module ||
                 ATP_MODULE_STR_IDX(attr_idx) == NULL_IDX)) {

               /* This has not been entered or resolved globally yet.  */
               /* This routine checks for this name in the global name */
               /* table.  It enters it, if it doesn't exist or checks  */
               /* for semantics errors if it does exist.               */

               check_global_pgm_unit(attr_idx);
            }

            if (ATP_EXT_NAME_IDX(attr_idx) == NULL_IDX) {
               MAKE_EXTERNAL_NAME(attr_idx,
                                  AT_NAME_IDX(attr_idx),
                                  AT_NAME_LEN(attr_idx));
            }
            attr_idx = NULL_IDX;
            break;

         }  /* End switch */
         break;

      case Interface:

         attr_idx = ATI_PROC_IDX(attr_idx);

         if (attr_idx != NULL_IDX) {

            /* If we're in the module and processing a module procedure */
            /* and this procedure is declared inside this module, then  */
            /* skip processing until we see it in its own declaration.  */

            if (ATP_PROC(attr_idx) == Module_Proc &&
                ATP_PGM_UNIT(SCP_ATTR_IDX(curr_scp_idx)) == Module &&
                !AT_USE_ASSOCIATED(attr_idx)) {
               attr_idx = NULL_IDX;
            }
            else {
               attr_idx = ATP_RSLT_IDX(attr_idx);
            }
         }
         break;

      case Stmt_Func:

         if (!ATS_SF_SEMANTICS_DONE(attr_idx)) {
            stmt_func_semantics(attr_idx);
         }
         attr_idx = NULL_IDX;
         break;

      default:
         attr_idx = NULL_IDX;
         break;
      }

      if (attr_idx == NULL_IDX) { 
         goto EXIT;
      }

      if (!ATD_OFFSET_ASSIGNED(attr_idx)) {

# ifdef _DEBUG
         if (ATD_CLASS(attr_idx) == Compiler_Tmp &&
             ATD_STOR_BLK_IDX(attr_idx) == NULL_IDX &&
             !ATD_SYMBOLIC_CONSTANT(attr_idx)) {
            PRINTMSG(AT_DEF_LINE(attr_idx), 836, Internal,
                     AT_DEF_COLUMN(attr_idx),
                     AT_OBJ_NAME_PTR(attr_idx));
         }
# endif

         if (ATD_STOR_BLK_IDX(attr_idx) == NULL_IDX) {
            assign_storage_blk(attr_idx);
         }

         switch (SB_BLK_TYPE(ATD_STOR_BLK_IDX(attr_idx))) {

         case Static:
         case Static_Local:
         case Static_Named:

            if (ATD_ARRAY_IDX(attr_idx) != NULL_IDX &&
                BD_ARRAY_SIZE(ATD_ARRAY_IDX(attr_idx))==Symbolic_Constant_Size){
               NTR_ATTR_LIST_TBL(al_idx);
               AL_ATTR_IDX(al_idx) 		= attr_idx;
               AL_NEXT_IDX(al_idx) 		= symbolic_constant_array_list;
               symbolic_constant_array_list	= al_idx;
            }
            else {
               assign_offset(attr_idx);  /* assign offsets to static storage */
               ATD_OFFSET_ASSIGNED(attr_idx) = TRUE;
            }
            break;

         case Stack:

            if (SB_HOSTED_STACK(ATD_STOR_BLK_IDX(attr_idx))) {

               if (ATD_ARRAY_IDX(attr_idx) != NULL_IDX &&
                   BD_ARRAY_SIZE(ATD_ARRAY_IDX(attr_idx)) == 
                                               Symbolic_Constant_Size) {
                  NTR_ATTR_LIST_TBL(al_idx);
                  AL_ATTR_IDX(al_idx) 		= attr_idx;
                  AL_NEXT_IDX(al_idx) 		= symbolic_constant_array_list;
                  symbolic_constant_array_list	= al_idx;
               }
               else {

# if defined(_ASSIGN_OFFSETS_TO_HOSTED_STACK)
                  assign_offset(attr_idx);      /* Assign to hosted stack */
                  ATD_OFFSET_ASSIGNED(attr_idx) = TRUE;
# else
                  /* Check so the item does not exceed max storage size */

                  stor_bit_size_of(attr_idx, TRUE, FALSE);
# endif
               }
            }
            else if (!AT_DCL_ERR(attr_idx)) {

               /* Check so the item does not exceed max storage size */

               stor_bit_size_of(attr_idx, TRUE, FALSE);
            }
            break;

         case Equivalenced:
            break;

         case Task_Common:
         case Threadprivate:

            if (ATD_CLASS(attr_idx) == Compiler_Tmp &&
                ATD_DATA_INIT(attr_idx) &&
                ATD_FLD(attr_idx) == AT_Tbl_Idx) {
               ATD_OFFSET_FLD(attr_idx) = ATD_OFFSET_FLD(ATD_TMP_IDX(attr_idx));
               ATD_OFFSET_IDX(attr_idx) = ATD_OFFSET_IDX(ATD_TMP_IDX(attr_idx));
               ATD_OFFSET_ASSIGNED(attr_idx) = TRUE;
            }
            else {
               if (! ATD_OFFSET_ASSIGNED(attr_idx)) {
                  assign_offset(attr_idx);   /* Assign to task common */
                  ATD_OFFSET_ASSIGNED(attr_idx) = TRUE;
               }
            }

            break;

         case Based:
         case Formal:
         case Common:

            if (ATD_CLASS(attr_idx) == Compiler_Tmp &&
                ATD_DATA_INIT(attr_idx) && 
                ATD_FLD(attr_idx) == AT_Tbl_Idx) {
               ATD_OFFSET_FLD(attr_idx) = ATD_OFFSET_FLD(ATD_TMP_IDX(attr_idx));
               ATD_OFFSET_IDX(attr_idx) = ATD_OFFSET_IDX(ATD_TMP_IDX(attr_idx));
               ATD_OFFSET_ASSIGNED(attr_idx) = TRUE;
            }
            break;

         default:
            break;
         }
      }

      sb_idx = ATD_STOR_BLK_IDX(attr_idx);

      if (SB_MERGED_BLK_IDX(sb_idx) != NULL_IDX) {
         sb_idx				= SB_MERGED_BLK_IDX(sb_idx);
         ATD_STOR_BLK_IDX(attr_idx)	= sb_idx;
      }

      if (SB_DEF_MULT_SCPS(sb_idx) || SB_HAS_RENAMES(sb_idx)) {
         ATD_EQUIV(attr_idx) = TRUE;
      }

# if defined(_ASSIGN_OFFSETS_TO_HOSTED_STACK)

      if (SB_HOSTED_STACK(sb_idx) && 
          SB_SCP_IDX(sb_idx) != curr_scp_idx &&
          tmp_scp_idx != curr_scp_idx) {

         /* This is the host associated stack and it is host      */
         /* associated into this scope.  This is the first time   */
         /* this storage block has been seen in this scope        */
         /* because tmp_scp_idx != curr_scp_idx.  Add the storage */
         /* blocks tmp to this scope and set tmp_scp_idx to this  */
         /* scope.  When the scope change, tmp_scp_idx will not   */
         /* change until the tmp has been added to the new scope. */
         /* The storage block's tmp is a tmp whose offset is one  */
         /* storage word size less than the total length of the   */
         /* block, with a length of one storage word size.  This  */
         /* way, the ccg/rcg will get the length of the storage   */
         /* block correct.  (We do not pass them length, they     */
         /* calculate it themselves.)                             */

         if (SB_LAST_ATTR_LIST(sb_idx) != NULL_IDX) {
            ADD_ATTR_TO_LOCAL_LIST(AL_ATTR_IDX(SB_LAST_ATTR_LIST(sb_idx)));
            tmp_scp_idx	= curr_scp_idx;
         }
      }
# endif

# ifdef _DEBUG
      if ((ATD_CLASS(attr_idx) == Variable ||
           ATD_CLASS(attr_idx) == Function_Result ||
           ATD_CLASS(attr_idx) == Compiler_Tmp) &&
          ATD_OFFSET_ASSIGNED(attr_idx) &&
          ATD_OFFSET_FLD(attr_idx) == CN_Tbl_Idx &&
          fold_relationals(ATD_OFFSET_IDX(attr_idx),
                           CN_INTEGER_ZERO_IDX,
                           Lt_Opr)) {
         PRINTMSG(AT_DEF_LINE(attr_idx), 1004, Internal, 
                  AT_DEF_COLUMN(attr_idx),
                  AT_OBJ_NAME_PTR(attr_idx),
                  attr_idx);
      }
# endif
   }
   else {
      local_attr_idx	= attr_idx;

      while (AT_ATTR_LINK(attr_idx) != NULL_IDX &&
             ! AT_IGNORE_ATTR_LINK(attr_idx)) {
         attr_idx = AT_ATTR_LINK(attr_idx);
      }

# if defined(_F_MINUS_MINUS)

      if (AT_OBJ_CLASS(attr_idx) == Data_Obj && 
          ATD_PE_ARRAY_IDX(attr_idx) &&
          (ATP_PURE(SCP_ATTR_IDX(curr_scp_idx)) ||
           ATP_ELEMENTAL(SCP_ATTR_IDX(curr_scp_idx)))) {
         PRINTMSG(AT_DEF_LINE(local_attr_idx), 1580, Error, 
                  AT_DEF_COLUMN(local_attr_idx),
                  AT_OBJ_NAME_PTR(SCP_ATTR_IDX(curr_scp_idx)),
                  AT_OBJ_NAME_PTR(attr_idx));
      }
# endif

      if (AT_OBJ_CLASS(attr_idx) == Data_Obj &&
          ATD_STOR_BLK_IDX(attr_idx) != NULL_IDX) {
         sb_idx = ATD_STOR_BLK_IDX(attr_idx);

# if defined(_ASSIGN_OFFSETS_TO_HOSTED_STACK)

         if (SB_HOSTED_STACK(sb_idx) && 
             SB_SCP_IDX(sb_idx) != curr_scp_idx &&
             tmp_scp_idx != curr_scp_idx) {

            /* This is the host associated stack and it is host      */
            /* associated into this scope.  This is the first time   */
            /* this storage block has been seen in this scope        */
            /* because tmp_scp_idx != curr_scp_idx.  Add the storage */
            /* blocks tmp to this scope and set tmp_scp_idx to this  */
            /* scope.  When the scope change, tmp_scp_idx will not   */
            /* change until the tmp has been added to the new scope. */
            /* The storage block's tmp is a tmp whose offset is one  */
            /* storage word size less than the total length of the   */
            /* block, with a length of one storage word size.  This  */
            /* way, the ccg/rcg will get the length of the storage   */
            /* block correct.  (We do not pass them length, they     */
            /* calculate it themselves.)                             */

            /* WARNING - This tmp_scp scheme will only work if there */
            /*           is 1 hosted stack block.                    */

            if (SB_LAST_ATTR_LIST(sb_idx) != NULL_IDX) {
               ADD_ATTR_TO_LOCAL_LIST(AL_ATTR_IDX(SB_LAST_ATTR_LIST(sb_idx)));
               tmp_scp_idx	= curr_scp_idx;
            }
         }
# endif

         if (ATD_AUXILIARY(attr_idx) || SB_AUXILIARY(sb_idx)) {
            PRINTMSG(AT_DEF_LINE(attr_idx), 607, Error,
                     AT_DEF_COLUMN(attr_idx),
                     AT_OBJ_NAME_PTR(attr_idx));
            AT_DCL_ERR(attr_idx)	= TRUE;
            AT_DCL_ERR(local_attr_idx)	= TRUE;
         }

# ifdef _DEBUG

         /* Make sure if this is on the stack that it has been moved */
         /* to a host associated storage block.                      */

         if (SB_BLK_TYPE(sb_idx) == Stack &&
             SCP_SB_HOSTED_STACK_IDX(SB_SCP_IDX(sb_idx)) != sb_idx) {
            PRINTMSG(AT_DEF_LINE(attr_idx), 850, Internal,
                     AT_DEF_COLUMN(attr_idx),
                     AT_OBJ_NAME_PTR(attr_idx));
         }
# endif
      }
   }

EXIT:

   TRACE (Func_Exit, "final_attr_semantics", NULL);

   return;

}  /* final_attr_semantics */

/******************************************************************************\
|*                                                                            *|
|* Description:                                                               *|
|*      Allocates storage and offsets for all COMMON blocks.                  *|
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
static void	check_and_allocate_common_storage(int	sb_idx)

{
   size_offset_type	adjust_by;
   int			attr_idx;
   boolean     		equived;
   int			group;
   int			item;
   size_offset_type	largest_len;
   size_offset_type	left;
   size_offset_type	logical_result;
   int			name_idx;
   size_offset_type	new_len;
   int			next_attr_idx;
   size_offset_type	result;
   size_offset_type	save_offset;

# if !defined(_TARGET_DOUBLE_ALIGN)
   size_offset_type	right;
# else
   boolean		equal_zero;
   boolean		save_dalign_opt;
# endif

# if !defined(_ERROR_DUPLICATE_GLOBALS)
   boolean		issue_message;
# endif


   TRACE (Func_Entry, "check_and_allocate_common_storage", NULL);

# if defined(_ERROR_DUPLICATE_GLOBALS)

   attr_idx = srch_sym_tbl(SB_NAME_PTR(sb_idx),
                           SB_NAME_LEN(sb_idx),
                           &name_idx);

   if (attr_idx == NULL_IDX) {
      attr_idx = srch_host_sym_tbl(SB_NAME_PTR(sb_idx),
                                   SB_NAME_LEN(sb_idx),
                                   &name_idx,
                                   FALSE);
   }

   if (attr_idx != NULL_IDX) {

      switch (AT_OBJ_CLASS(attr_idx)) {
      case Data_Obj:

         if (ATD_CLASS(attr_idx) == Constant) {

            if (SB_USE_ASSOCIATED(sb_idx)) {
               PRINTMSG(AT_DEF_LINE(attr_idx), 1033, Ansi,
                        AT_DEF_COLUMN(attr_idx),
                        SB_NAME_PTR(sb_idx));
            }
            else if (SB_HOST_ASSOCIATED(sb_idx)) {
               PRINTMSG(AT_DEF_LINE(attr_idx), 1032, Ansi,
                        AT_DEF_COLUMN(attr_idx),
                        SB_NAME_PTR(sb_idx));
            }
            else {
               PRINTMSG(AT_DEF_LINE(attr_idx), 547, Ansi,
                        AT_DEF_COLUMN(attr_idx),
                        SB_NAME_PTR(sb_idx));
            }
         }
         break;

      case Pgm_Unit:

         if (ATP_PROC(attr_idx) == Intrin_Proc &&
             AT_REFERENCED(attr_idx) >= Dcl_Bound_Ref) {

            if (SB_USE_ASSOCIATED(sb_idx)) {
               PRINTMSG(AT_DEF_LINE(attr_idx), 1031, Error,
                        AT_DEF_COLUMN(attr_idx),
                        SB_NAME_PTR(sb_idx));
            }
            else if (SB_HOST_ASSOCIATED(sb_idx)) {
               PRINTMSG(AT_DEF_LINE(attr_idx), 1030, Error,
                        AT_DEF_COLUMN(attr_idx),
                        SB_NAME_PTR(sb_idx));
            }
            else {
               PRINTMSG(AT_DEF_LINE(attr_idx), 1005, Error,
                        AT_DEF_COLUMN(attr_idx),
                        SB_NAME_PTR(sb_idx));
            }
         }
         break;

      case Interface:

         if (AT_IS_INTRIN(attr_idx) && 
             AT_REFERENCED(attr_idx) >= Dcl_Bound_Ref) {

            if (SB_USE_ASSOCIATED(sb_idx)) {
               PRINTMSG(AT_DEF_LINE(attr_idx), 1031, Error,
                        AT_DEF_COLUMN(attr_idx),
                        SB_NAME_PTR(sb_idx));
            }
            else if (SB_HOST_ASSOCIATED(sb_idx)) {
               PRINTMSG(AT_DEF_LINE(attr_idx), 1030, Error,
                        AT_DEF_COLUMN(attr_idx),
                        SB_NAME_PTR(sb_idx));
            }
            else {
               PRINTMSG(AT_DEF_LINE(attr_idx), 1005, Error,
                        AT_DEF_COLUMN(attr_idx),
                        SB_NAME_PTR(sb_idx));
            }
         }
         break;
      }
   }

# else

   issue_message = GET_MESSAGE_TBL(message_warning_tbl, 1033) ||
                   GET_MESSAGE_TBL(message_error_tbl, 1033) ||
                   GET_MESSAGE_TBL(message_warning_tbl, 1032) ||
                   GET_MESSAGE_TBL(message_error_tbl, 1032) ||
                   GET_MESSAGE_TBL(message_warning_tbl, 547) ||
                   GET_MESSAGE_TBL(message_error_tbl, 547) ||
                   GET_MESSAGE_TBL(message_warning_tbl, 1029) ||
                   GET_MESSAGE_TBL(message_error_tbl, 1029) ||
                   GET_MESSAGE_TBL(message_warning_tbl, 1028) ||
                   GET_MESSAGE_TBL(message_error_tbl, 1028) ||
                   GET_MESSAGE_TBL(message_warning_tbl, 714) ||
                   GET_MESSAGE_TBL(message_error_tbl, 714);


   if (issue_message || on_off_flags.issue_ansi_messages) {
      attr_idx = srch_sym_tbl(SB_NAME_PTR(sb_idx),
                              SB_NAME_LEN(sb_idx),
                              &name_idx);

      if (attr_idx == NULL_IDX) {
         attr_idx = srch_host_sym_tbl(SB_NAME_PTR(sb_idx),
                                      SB_NAME_LEN(sb_idx),
                                      &name_idx,
                                      FALSE);
      }

      if (attr_idx != NULL_IDX) {

         switch (AT_OBJ_CLASS(attr_idx)) {
         case Data_Obj:

            if (ATD_CLASS(attr_idx) == Constant) {

               if (SB_USE_ASSOCIATED(sb_idx)) {
                  PRINTMSG(AT_DEF_LINE(attr_idx), 1033, Ansi,
                           AT_DEF_COLUMN(attr_idx),
                           SB_NAME_PTR(sb_idx));
               }
               else if (SB_HOST_ASSOCIATED(sb_idx)) {
                  PRINTMSG(AT_DEF_LINE(attr_idx), 1032, Ansi,
                           AT_DEF_COLUMN(attr_idx),
                           SB_NAME_PTR(sb_idx));
               }
               else {
                  PRINTMSG(AT_DEF_LINE(attr_idx), 547, Ansi,
                           AT_DEF_COLUMN(attr_idx),
                           SB_NAME_PTR(sb_idx));
               }
            }
            break;

         case Pgm_Unit:

            if (ATP_PROC(attr_idx) == Intrin_Proc &&
                AT_REFERENCED(attr_idx) >= Dcl_Bound_Ref) {

               if (SB_USE_ASSOCIATED(sb_idx)) {
                  PRINTMSG(AT_DEF_LINE(attr_idx), 1029, Ansi,
                           AT_DEF_COLUMN(attr_idx),
                           SB_NAME_PTR(sb_idx));
               }
               else if (SB_HOST_ASSOCIATED(sb_idx)) {
                  PRINTMSG(AT_DEF_LINE(attr_idx), 1028, Ansi,
                           AT_DEF_COLUMN(attr_idx),
                           SB_NAME_PTR(sb_idx));
               }
               else {
                  PRINTMSG(AT_DEF_LINE(attr_idx), 714, Ansi,
                           AT_DEF_COLUMN(attr_idx),
                           SB_NAME_PTR(sb_idx));
               }
            }
            break;

         case Interface:

            if (AT_IS_INTRIN(attr_idx) && 
                AT_REFERENCED(attr_idx) >= Dcl_Bound_Ref) {

               if (SB_USE_ASSOCIATED(sb_idx)) {
                  PRINTMSG(AT_DEF_LINE(attr_idx), 1029, Ansi,
                           AT_DEF_COLUMN(attr_idx),
                           SB_NAME_PTR(sb_idx));
               }
               else if (SB_HOST_ASSOCIATED(sb_idx)) {
                  PRINTMSG(AT_DEF_LINE(attr_idx), 1028, Ansi,
                           AT_DEF_COLUMN(attr_idx),
                           SB_NAME_PTR(sb_idx));
               }
               else {
                  PRINTMSG(AT_DEF_LINE(attr_idx), 714, Ansi,
                           AT_DEF_COLUMN(attr_idx),
                           SB_NAME_PTR(sb_idx));
               }
            }
            break;
         }
      }
   }
# endif

   if (SB_USE_ASSOCIATED(sb_idx) || !SB_COMMON_NEEDS_OFFSET(sb_idx)) {
      goto EXIT;
   }

   if (SB_FIRST_ATTR_IDX(sb_idx) == NULL_IDX && !SB_DCL_ERR(sb_idx)) {

      if (SB_SAVED(sb_idx)) {

         /* The common block was declared in a save statement, but */
         /* not as an actual common block.                         */

         PRINTMSG(SB_DEF_LINE(sb_idx), 688, Error,
                  SB_DEF_COLUMN(sb_idx), 
                  SB_NAME_PTR(sb_idx));
         SB_DCL_ERR(sb_idx)	= TRUE;
      }
      else if (SB_BLK_TYPE(sb_idx) == Threadprivate) {
         PRINTMSG(SB_DEF_LINE(sb_idx), 1502, Error,
                  SB_DEF_COLUMN(sb_idx), 
                  SB_NAME_PTR(sb_idx),
                  "THREAD_PRIVATE");
         SB_DCL_ERR(sb_idx)	= TRUE;
      }
      else if (SB_CACHE_ALIGN(sb_idx)) {
         PRINTMSG(SB_DEF_LINE(sb_idx), 1168, Error,
                  SB_DEF_COLUMN(sb_idx), 
                  SB_NAME_PTR(sb_idx));
         SB_DCL_ERR(sb_idx)	= TRUE;
      }
      else if (SB_SECTION_GP(sb_idx)) {
         PRINTMSG(SB_DEF_LINE(sb_idx), 1502, Error,
                  SB_DEF_COLUMN(sb_idx), 
                  SB_NAME_PTR(sb_idx),
                  "SECTION_GP");
         SB_DCL_ERR(sb_idx)	= TRUE;
      }
      else if (SB_SECTION_NON_GP(sb_idx)) {
         PRINTMSG(SB_DEF_LINE(sb_idx), 1502, Error,
                  SB_DEF_COLUMN(sb_idx), 
                  SB_NAME_PTR(sb_idx),
                  "SECTION_NON_GP");
         SB_DCL_ERR(sb_idx)	= TRUE;
      }
      else if (SB_DCL_COMMON_DIR(sb_idx)) {
         SB_DCL_ERR(sb_idx)	= TRUE;
         PRINTMSG(SB_DEF_LINE(sb_idx), 1128, Error,
                  SB_DEF_COLUMN(sb_idx), 
                  SB_NAME_PTR(sb_idx));
      }
      else if (SB_BLK_TYPE(sb_idx) == Task_Common) {
         SB_DCL_ERR(sb_idx)	= TRUE;
         PRINTMSG(SB_DEF_LINE(sb_idx), 690, Error,
                  SB_DEF_COLUMN(sb_idx), 
                  SB_NAME_PTR(sb_idx));
      }
   }

   if (SB_DCL_COMMON_DIR(sb_idx) && SB_BLK_TYPE(sb_idx) == Task_Common) {
      SB_DCL_ERR(sb_idx)	= TRUE;
      PRINTMSG(SB_DEF_LINE(sb_idx), 1129, Error,
               SB_DEF_COLUMN(sb_idx), 
               SB_NAME_PTR(sb_idx));
   }

   attr_idx	= SB_FIRST_ATTR_IDX(sb_idx);
   equived	= FALSE;

   while (attr_idx != NULL_IDX && !equived) {
      equived	= equived || ATD_EQUIV(attr_idx);
      attr_idx	= ATD_NEXT_MEMBER_IDX(attr_idx);
   }

   if (SB_PAD_BLK(sb_idx) && equived) { /* -a pad and equiv don't go together */
      PRINTMSG(SB_DEF_LINE(sb_idx), 1351, Warning,
               SB_DEF_COLUMN(sb_idx), 
               SB_BLANK_COMMON(sb_idx) ?
               "" : SB_NAME_PTR(sb_idx));
      SB_PAD_BLK(sb_idx)= FALSE;
   }

   next_attr_idx	= SB_FIRST_ATTR_IDX(sb_idx);
   largest_len.fld	= SB_LEN_FLD(sb_idx);
   largest_len.idx	= SB_LEN_IDX(sb_idx);

   while (next_attr_idx != NULL_IDX) {
      attr_idx			= next_attr_idx;
      next_attr_idx		= ATD_NEXT_MEMBER_IDX(attr_idx);

      if (AT_DCL_ERR(attr_idx)) {

         /* Error - Do not attempt to assign offset. */

      }
      else if (!ATD_EQUIV(attr_idx) || num_prog_unit_errors != 0) {

         /* We do not do equivalence processing if we found any errors in */
         /* this program unit.  Error recovery doesn't work too well.     */

#ifdef KEY /* Bug 14150 */
         assign_bind_c_offset(attr_idx,
	   AT_OBJ_CLASS(attr_idx) == Data_Obj && ATD_IN_COMMON(attr_idx) &&
	   SB_BIND_ATTR(ATD_STOR_BLK_IDX(attr_idx)));
#else /* KEY Bug 14150 */
         assign_offset(attr_idx);       /* Equivalence */
#endif /* KEY Bug 14150 */
         ATD_OFFSET_ASSIGNED(attr_idx)	= TRUE;
         ATD_EQUIV(attr_idx)		= equived;
      }
      else {

         if (ATD_OFFSET_IDX(attr_idx) == NULL_IDX) {
            ATD_OFFSET_IDX(attr_idx) = CN_INTEGER_ZERO_IDX;
            ATD_OFFSET_FLD(attr_idx) = CN_Tbl_Idx;
         }

         save_offset.fld	= ATD_OFFSET_FLD(attr_idx);
         save_offset.idx	= ATD_OFFSET_IDX(attr_idx);

# if defined(_TARGET_DOUBLE_ALIGN)
         save_dalign_opt	= cmd_line_flags.dalign;
         cmd_line_flags.dalign	= FALSE;

         /* Offset will not get daligned, because dalign flag = FALSE */
         /* If daligning is necessary, it will be done by hand here.  */

         assign_offset(attr_idx);   /* Equivalence */

         cmd_line_flags.dalign	= save_dalign_opt;
         left.fld		= ATD_OFFSET_FLD(attr_idx);
         left.idx		= ATD_OFFSET_IDX(attr_idx);

         if (!size_offset_binary_calc(&left,
                                      &save_offset, 
                                       Minus_Opr,
                                      &adjust_by)) {
            AT_DCL_ERR(attr_idx) = TRUE;
         }

# else
         assign_offset(attr_idx);  /* Equivalence */

         left.fld		= ATD_OFFSET_FLD(attr_idx);
         left.idx		= ATD_OFFSET_IDX(attr_idx);

         if (!size_offset_binary_calc(&left,
                                      &save_offset, 
                                       Minus_Opr,
                                      &adjust_by)) {
            AT_DCL_ERR(attr_idx) = TRUE;
         }

         if (ATD_OFFSET_ASSIGNED(attr_idx)) {
            right.fld	= CN_Tbl_Idx;
            right.idx	= CN_INTEGER_ZERO_IDX;

            size_offset_logical_calc(&adjust_by, &right, Eq_Opr, &result);

             if (THIS_IS_TRUE(result.constant, result.type_idx)) {

               /* Offset for this attr is assigned already, because it is in */
               /* a previous equivalence group.  This offset should agree    */
               /* with the new offset just assigned.  If it doesn't continue */
               /* and issue an error later, when we can give a better line   */
               /* and column number.  If it is zero, continue to next attr.  */

               continue;
            }
         }
# endif

         group = SCP_FIRST_EQUIV_GRP(curr_scp_idx);

         while (group != NULL_IDX) {
            item		= group;

            while (item != NULL_IDX) {

# if _DEBUG
               if (!ATD_EQUIV(EQ_ATTR_IDX(item)) && 
                   !AT_DCL_ERR(EQ_ATTR_IDX(item)) &&
                   ATD_CLASS(EQ_ATTR_IDX(item)) == Variable) {
                  PRINTMSG(AT_DEF_LINE(EQ_ATTR_IDX(item)), 
                           1019, 
                           Internal, 
                           AT_DEF_COLUMN(EQ_ATTR_IDX(item)),
                           AT_OBJ_NAME_PTR(EQ_ATTR_IDX(item)));
               }
# endif

               if (EQ_ATTR_IDX(item) == attr_idx) {
                  goto FOUND;
               }
               item = EQ_NEXT_EQUIV_OBJ(item);
            }
            group = EQ_NEXT_EQUIV_GRP(group);
         }

         ATD_OFFSET_ASSIGNED(attr_idx)	= TRUE;

         /* This is a data initialized item.  It is equivalenced to a tmp. */
         /* The tmps offset gets set in final_attr_semantics.              */

         continue;    

FOUND:

         if (ATD_OFFSET_ASSIGNED(attr_idx)) {

            if (fold_relationals(ATD_OFFSET_IDX(attr_idx),
                                 save_offset.idx,
                                 Ne_Opr)) {
                PRINTMSG(EQ_LINE_NUM(item), 862, Error,
                         EQ_COLUMN_NUM(item),
                         AT_OBJ_NAME_PTR(attr_idx));
            }
            continue;
         }

# if defined(_TARGET_DOUBLE_ALIGN)

         else {
            if (EQ_DALIGN_ME(item)) { 
               C_TO_F_INT(result.constant, TARGET_BITS_PER_WORD * 2,
                          CG_INTEGER_DEFAULT_TYPE);
               result.fld		= NO_Tbl_Idx;
               result.type_idx		= CG_INTEGER_DEFAULT_TYPE;

               if (!size_offset_binary_calc(&adjust_by,
                                            &result,
                                             Mod_Opr,
                                            &result)) {
                  AT_DCL_ERR(attr_idx)	= TRUE;
               }

               left.fld	= CN_Tbl_Idx;
               left.idx	= CN_INTEGER_ZERO_IDX;

               size_offset_logical_calc(&left, &result, Eq_Opr, &result);

               equal_zero = THIS_IS_TRUE(result.constant, result.type_idx);

               if ((equal_zero && EQ_DALIGN_SHIFT(item)) || 
                   (!equal_zero && !EQ_DALIGN_SHIFT(item))) { 

                  if (cmd_line_flags.dalign) {

                     /* If offset % TARGET_BITS == 0, the new offset is on a  */
                     /* double word boundary.  If !EQ_DALIGN_SHIFT then the   */
                     /* equivalence group is on a double word boundary.  What */
                     /* the above if statement says is that if both the new   */
                     /* offset and the equivalence group are on a double word */
                     /* boundary - do nothing.  If neither one is on a double */
                     /* word boundary, do nothing.  But if one is on a double */
                     /* word boundary and the other is not, adjust new offset.*/

                     result.fld	= CN_Tbl_Idx;
                     result.idx	= CN_INTEGER_BITS_PER_WORD_IDX;

                     if (!size_offset_binary_calc(&adjust_by,
                                                  &result,
                                                   Plus_Opr,
                                                  &adjust_by)) {
                        AT_DCL_ERR(attr_idx) = TRUE;
                     }

                     left.fld	= ATD_OFFSET_FLD(attr_idx);
                     left.idx	= ATD_OFFSET_IDX(attr_idx);

                     if (!size_offset_binary_calc(&left,
                                                  &result,
                                                   Plus_Opr,
                                                  &result)) {
                        AT_DCL_ERR(attr_idx) = TRUE;
                     }

                     if (result.fld == NO_Tbl_Idx) {
                        ATD_OFFSET_FLD(attr_idx) = CN_Tbl_Idx;
                        ATD_OFFSET_IDX(attr_idx) = ntr_const_tbl(
                                                             result.type_idx,
                                                             FALSE,
                                                             result.constant);
                     }
                     else {
                        ATD_OFFSET_FLD(attr_idx) = result.fld;
                        ATD_OFFSET_IDX(attr_idx) = result.idx;
                     }

                     result.fld	= CN_Tbl_Idx;
                     result.idx	= CN_INTEGER_BITS_PER_WORD_IDX;
                     left.fld	= SB_LEN_FLD(sb_idx);
                     left.idx	= SB_LEN_IDX(sb_idx);

                     if (!size_offset_binary_calc(&left,
                                                  &result,
                                                   Plus_Opr,
                                                  &result)) {
                        AT_DCL_ERR(attr_idx) = TRUE;
                     }

                     if (result.fld == NO_Tbl_Idx) {
                        SB_LEN_FLD(sb_idx) = CN_Tbl_Idx;
                        SB_LEN_IDX(sb_idx) = ntr_const_tbl(result.type_idx,
                                                           FALSE,
                                                           result.constant);
                     }
                     else {
                        SB_LEN_FLD(sb_idx)  = result.fld;
                        SB_LEN_IDX(sb_idx)  = result.idx;
                     }

# if ! (defined(_TARGET_OS_IRIX) || defined(_TARGET_OS_LINUX) || defined(_TARGET_OS_DARWIN))

                    /* -a dalign is always on for IRIX and there is no */
                    /* way to shut it off, so we do not need to issue  */
                    /* this warning for IRIX.                          */

                     PRINTMSG(AT_DEF_LINE(attr_idx), 1013, Warning, 
                              AT_DEF_COLUMN(attr_idx),
                              AT_OBJ_NAME_PTR(attr_idx),
                              SB_BLANK_COMMON(sb_idx) ?
                              "" : SB_NAME_PTR(sb_idx));
# endif
                  } 
                  else {  /* Cannot double align */
                     PRINTMSG(AT_DEF_LINE(attr_idx), 1161, Caution, 
                              AT_DEF_COLUMN(attr_idx),
                              AT_OBJ_NAME_PTR(attr_idx),
                              SB_BLANK_COMMON(sb_idx) ?
                              "" : SB_NAME_PTR(sb_idx));
                  } 
               }
            }
         }
# endif

         ATD_OFFSET_ASSIGNED(attr_idx)	= TRUE;
         item				= group;

         while (item != NULL_IDX) {

            if (!ATD_OFFSET_ASSIGNED(EQ_ATTR_IDX(item))) {

               if (ATD_OFFSET_IDX(EQ_ATTR_IDX(item)) == NULL_IDX) {
                  ATD_OFFSET_FLD(EQ_ATTR_IDX(item)) = CN_Tbl_Idx;
                  ATD_OFFSET_IDX(EQ_ATTR_IDX(item)) = ntr_const_tbl(
                                                         adjust_by.type_idx,
                                                         FALSE,
                                                         adjust_by.constant);
               }
               else {
                  left.fld	= ATD_OFFSET_FLD(EQ_ATTR_IDX(item));
                  left.idx	= ATD_OFFSET_IDX(EQ_ATTR_IDX(item));

                  if (!size_offset_binary_calc(&left,
                                               &adjust_by,
                                                Plus_Opr,
                                               &result)) {
                     AT_DCL_ERR(EQ_ATTR_IDX(item)) = TRUE;
                  }

                  if (result.fld == NO_Tbl_Idx) {
                     ATD_OFFSET_FLD(EQ_ATTR_IDX(item)) = CN_Tbl_Idx;
                     ATD_OFFSET_IDX(EQ_ATTR_IDX(item)) = ntr_const_tbl(
                                                             result.type_idx,
                                                             FALSE,
                                                             result.constant);
                  }
                  else {
                     ATD_OFFSET_FLD(EQ_ATTR_IDX(item)) = result.fld;
                     ATD_OFFSET_IDX(EQ_ATTR_IDX(item)) = result.idx;
                  }
               }

               ATD_OFFSET_ASSIGNED(EQ_ATTR_IDX(item))	 = TRUE;

               if (fold_relationals(ATD_OFFSET_IDX(EQ_ATTR_IDX(item)),
                                    CN_INTEGER_ZERO_IDX,
                                    Lt_Opr)) {
                  PRINTMSG(SB_DEF_LINE(sb_idx), 526, Error,
                           SB_DEF_COLUMN(sb_idx),
                           SB_BLANK_COMMON(sb_idx) ?
                           "" : SB_NAME_PTR(sb_idx),
                           AT_OBJ_NAME_PTR(EQ_ATTR_IDX(item)));
                  ATD_OFFSET_IDX(EQ_ATTR_IDX(item))	= CN_INTEGER_ZERO_IDX;
                  ATD_OFFSET_FLD(EQ_ATTR_IDX(item))	= CN_Tbl_Idx;
                  AT_DCL_ERR(EQ_ATTR_IDX(item))		= TRUE;
               }

               new_len	= stor_bit_size_of(EQ_ATTR_IDX(item), TRUE, FALSE);
               left.fld	= ATD_OFFSET_FLD(EQ_ATTR_IDX(item));
               left.idx	= ATD_OFFSET_IDX(EQ_ATTR_IDX(item));

               if (!size_offset_binary_calc(&left, &new_len, Plus_Opr,&result)){
                  AT_DCL_ERR(EQ_ATTR_IDX(item)) = TRUE;
               }

               size_offset_logical_calc(&result, 
                                        &largest_len,
                                         Gt_Opr,
                                        &logical_result);

               if (THIS_IS_TRUE(logical_result.constant,
                                logical_result.type_idx)) {
                  largest_len = result;
               }
            }
            else if (!ATD_IN_COMMON(EQ_ATTR_IDX(item))) {
               left.fld	= EQ_OFFSET_FLD(item);
               left.idx	= EQ_OFFSET_IDX(item);

               if (!size_offset_binary_calc(&left,&adjust_by,Plus_Opr,&result)){
                  AT_DCL_ERR(EQ_ATTR_IDX(item)) = TRUE;
               }

               left.fld	= ATD_OFFSET_FLD(EQ_ATTR_IDX(item));
               left.idx	= ATD_OFFSET_IDX(EQ_ATTR_IDX(item));

               size_offset_logical_calc(&left, &result, Ne_Opr,&logical_result);

               if (THIS_IS_TRUE(logical_result.constant,
                                logical_result.type_idx)) {
                  PRINTMSG(EQ_LINE_NUM(item), 862, Error,
                           EQ_COLUMN_NUM(item),
                           AT_OBJ_NAME_PTR(EQ_ATTR_IDX(item)));
               }
            }
            item = EQ_NEXT_EQUIV_OBJ(item);
         }
      }
   }
   left.fld	= SB_LEN_FLD(sb_idx);
   left.idx	= SB_LEN_IDX(sb_idx);

   size_offset_logical_calc(&largest_len, &left, Gt_Opr, &logical_result);

   if (!THIS_IS_TRUE(logical_result.constant,
                     logical_result.type_idx)) {
      largest_len.idx	= SB_LEN_IDX(sb_idx);
      largest_len.fld	= SB_LEN_FLD(sb_idx);
   }

   align_bit_length(&largest_len, TARGET_BITS_PER_WORD);

   if (largest_len.fld == NO_Tbl_Idx) {
      largest_len.fld	= CN_Tbl_Idx;
      largest_len.idx	= ntr_const_tbl(largest_len.type_idx,
                                        FALSE,
                                        largest_len.constant);
   }

   SB_LEN_FLD(sb_idx) = largest_len.fld;
   SB_LEN_IDX(sb_idx) = largest_len.idx;

   SB_COMMON_NEEDS_OFFSET(sb_idx) = FALSE;

   if (cmd_line_flags.taskcommon && !SB_DCL_COMMON_DIR(sb_idx)) {

      /* Switch all common blocks to task common */

      SB_BLK_TYPE(sb_idx)	= Task_Common;
      SB_RUNTIME_INIT(sb_idx)	= FALSE;
   }

EXIT:

   TRACE (Func_Exit, "check_and_allocate_common_storage", NULL);

   return;

}  /* check_and_allocate_common_storage */

/******************************************************************************\
|*                                                                            *|
|* Description:                                                               *|
|*      Resolves multiple storage blocks in the current scope to one block.   *|
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
static void	storage_blk_resolution()
{
   int			attr_idx;
   int			ga_idx;
   int			gac_idx;
   int			ga_pgm_idx;
   int			host_sb_idx;
   msg_severities_type	msg_level;
   int			name_idx;
   size_offset_type	result;
   boolean		same_common_block;
   int			same_sb_idx;
   int			sb_idx;

# if !defined(_SEPARATE_NONCOMMON_EQUIV_GROUPS)
   int			group;
   int			item;
   id_str_type		name;
   int			new_sb_idx;
   int			np_idx;
   size_offset_type	offset;
# endif

# if defined(_TARGET_DOUBLE_ALIGN)
   size_offset_type	left;
# endif


   TRACE (Func_Entry, "storage_blk_resolution", NULL);

   /* Set the data initialized flag for the static initialization stor blk */

   for (sb_idx = 1; sb_idx <= stor_blk_tbl_idx; sb_idx++) {

      if (SB_SCP_IDX(sb_idx) != curr_scp_idx) {
         continue;
      }

      if (SB_IS_COMMON(sb_idx)) {
         SB_PAD_BLK(sb_idx) = cmd_line_flags.pad;

         if (cmd_line_flags.pad_amount != 0) {
            SB_PAD_AMOUNT(sb_idx)	= cmd_line_flags.pad_amount;
            SB_PAD_AMOUNT_SET(sb_idx)	= TRUE;
         }

         check_and_allocate_common_storage(sb_idx);
  
         if (!SB_HIDDEN(sb_idx) && !SB_HOST_ASSOCIATED(sb_idx)) {

            if (srch_global_name_tbl(SB_NAME_PTR(sb_idx), 
                                     SB_NAME_LEN(sb_idx),
                                     &name_idx)) {

               gac_idx	= GN_ATTR_IDX(name_idx);

               if (GA_OBJ_CLASS(gac_idx) != Common_Block) {

                  /* Have a common and program unit with same name.  The    */
                  /* common entry is always first and then points to the    */
                  /* program unit.  Add a global attr for the common block. */

                  ga_pgm_idx	= gac_idx;
                  gac_idx	= ntr_common_in_global_attr_tbl(sb_idx, 
                                                                name_idx);

                  GAC_PGM_UNIT_IDX(gac_idx) = ga_pgm_idx;
                  GN_ATTR_IDX(name_idx)	    = gac_idx;

# if defined(_ERROR_DUPLICATE_GLOBALS)
                  msg_level = Error;
# else
                  msg_level = (GAP_PGM_UNIT(ga_pgm_idx) == Module) ?
                              Error : Ansi;
# endif
                  PRINTMSG(SB_DEF_LINE(sb_idx), 1006, msg_level,
                           SB_DEF_COLUMN(sb_idx),
                           SB_NAME_PTR(sb_idx),
                           pgm_unit_str[GAP_PGM_UNIT(ga_pgm_idx)]);
               }
               else {
                  same_common_block = !SB_EQUIVALENCED(sb_idx) && 
                                      !GAC_EQUIVALENCED(gac_idx);

                  /* Common block used in another program unit. */

                  if (SB_AUXILIARY(sb_idx) ^ GAC_AUXILIARY(gac_idx)) {
                     same_common_block	= FALSE;
                     PRINTMSG(SB_DEF_LINE(sb_idx), 1276, Warning,
                              SB_DEF_COLUMN(sb_idx),
                              SB_NAME_PTR(sb_idx),
                              "AUXILIARY");
                  }

                  if ((SB_BLK_TYPE(sb_idx) == Task_Common && 
                      !GAC_TASK_COMMON(gac_idx)) ||
                      (SB_BLK_TYPE(sb_idx) != Task_Common && 
                       GAC_TASK_COMMON(gac_idx))) {
                     same_common_block	= FALSE;
          
                     PRINTMSG(SB_DEF_LINE(sb_idx), 1276, Warning,
                              SB_DEF_COLUMN(sb_idx),
                              SB_NAME_PTR(sb_idx),
                              "TASK_COMMON");
                  }

                  if (SB_ALIGN_SYMBOL(sb_idx) ^ GAC_ALIGN_SYMBOL(gac_idx)) {
                     same_common_block	= FALSE;
		     char *fal = file_and_line(GA_DEF_LINE(gac_idx));
                     PRINTMSG(SB_DEF_LINE(sb_idx), 1602, Warning,
                              SB_DEF_COLUMN(sb_idx),
                              SB_NAME_PTR(sb_idx),
                              "ALIGN_SYMBOL", fal);
		     free(fal);
                  }

                  if (SB_FILL_SYMBOL(sb_idx) ^ GAC_FILL_SYMBOL(gac_idx)) {
                     same_common_block	= FALSE;
		     char *fal = file_and_line(GA_DEF_LINE(gac_idx));
                     PRINTMSG(SB_DEF_LINE(sb_idx), 1602, Warning,
                              SB_DEF_COLUMN(sb_idx),
                              SB_NAME_PTR(sb_idx),
                              "FILL_SYMBOL", fal);
		     free(fal);
                  }

                  if (SB_SECTION_GP(sb_idx) ^ GAC_SECTION_GP(gac_idx)) {
                     same_common_block	= FALSE;
		     char *fal = file_and_line(GA_DEF_LINE(gac_idx));
                     PRINTMSG(SB_DEF_LINE(sb_idx), 1602, Warning,
                              SB_DEF_COLUMN(sb_idx),
                              SB_NAME_PTR(sb_idx),
                              "SECTION_GP", fal);
		     free(fal);
                  }
#ifdef KEY /* Bug 14150 */
                  if (SB_BIND_ATTR(sb_idx) ^ GA_BIND_ATTR(gac_idx)) {
                     same_common_block	= FALSE;
		     char *fal = file_and_line(GA_DEF_LINE(gac_idx));
                     PRINTMSG(SB_DEF_LINE(sb_idx), 1602, Warning,
                              SB_DEF_COLUMN(sb_idx),
                              SB_NAME_PTR(sb_idx),
                              "BIND", fal);
		     free(fal);
                  }
		  else if (SB_BIND_ATTR(sb_idx)) {
		    const char *ga_binding_label = GA_BINDING_LABEL(gac_idx);
		    int ga_binding_label_len = strlen(ga_binding_label);
		    if (SB_EXT_NAME_LEN(sb_idx) != ga_binding_label_len ||
		      strncmp(ga_binding_label, SB_EXT_NAME_PTR(sb_idx),
		        ga_binding_label_len)) {
		      char *fal = file_and_line(GA_DEF_LINE(gac_idx));
		      PRINTMSG(SB_DEF_LINE(sb_idx), 1700, Error,
			SB_DEF_COLUMN(sb_idx), SB_NAME_PTR(sb_idx),
			ga_binding_label, fal);
		      free(fal);
		    }
		  }
#endif /* KEY Bug 14150 */

                  if (SB_SECTION_NON_GP(sb_idx) ^ GAC_SECTION_NON_GP(gac_idx)) {
                     same_common_block	= FALSE;
		     char *fal = file_and_line(GA_DEF_LINE(gac_idx));
                     PRINTMSG(SB_DEF_LINE(sb_idx), 1602, Warning,
                              SB_DEF_COLUMN(sb_idx),
                              SB_NAME_PTR(sb_idx),
                              "SECTION_NON_GP", fal);
		     free(fal);
                  }

                  if (SB_CACHE_ALIGN(sb_idx) ^ GAC_CACHE_ALIGN(gac_idx)) {
                     same_common_block	= FALSE;
		     char *fal = file_and_line(GA_DEF_LINE(gac_idx));
                     PRINTMSG(SB_DEF_LINE(sb_idx), 1602, Warning,
                              SB_DEF_COLUMN(sb_idx),
                              SB_NAME_PTR(sb_idx),
                              "CACHE_ALIGN", fal);
		     free(fal);
                  }

                  /* Check to make sure all the members have the same type */
                  /* kind and rank.   We'll check for same name too.       */

                  attr_idx	= SB_FIRST_ATTR_IDX(sb_idx);
                  ga_idx	= GAC_FIRST_MEMBER_IDX(gac_idx);

                  while (attr_idx != NULL_IDX && ga_idx != NULL_IDX) {

                     /* We are looking at all Common block objects, so */
                     /* we can assume that these are all Variables.    */

                     if (!compare_global_type_rank(ga_idx,
                                                   NULL_IDX,
                                                   attr_idx,
                                                   NULL_IDX,
                                                   TRUE)) {
                        same_common_block	= FALSE;
                        break;
                     }
                     attr_idx	= ATD_NEXT_MEMBER_IDX(attr_idx);
                     ga_idx	= GAD_NEXT_IDX(ga_idx);
                  }

                  if (attr_idx != NULL_IDX || ga_idx != NULL_IDX) {
                     same_common_block	= FALSE;
                  }

                  if (!same_common_block) {
                     GAC_FOUND_DIFFS(gac_idx)		= TRUE;
                     SB_DUPLICATE_COMMON(sb_idx)	= FALSE;
                  }
                  else if (!GAC_FOUND_DIFFS(gac_idx)) {
                     SB_DUPLICATE_COMMON(sb_idx)	= TRUE;
                  }
               }
            }
            else {
               ntr_global_name_tbl(NULL_IDX, sb_idx, name_idx);
            }
         }
      }
      else if (cmd_line_flags.taskcommon) {

         /* All module blocks and all static blocks must be switched to      */
         /* taskcommon if the -a taskcommon commandline option is specified. */

         if (SB_MODULE(sb_idx) ||
             SB_BLK_TYPE(sb_idx) == Static ||
             SB_BLK_TYPE(sb_idx) == Static_Named ||
             SB_BLK_TYPE(sb_idx) == Static_Local) {
            SB_BLK_TYPE(sb_idx) = Task_Common;
         }
      }
      else if (cmd_line_flags.static_threadprivate) {

         /* All module blocks and all static blocks must be switched */
         /* to taskcommon if the -a static_threadprivate commandline */
         /* option is specified.                                     */

         if (SB_MODULE(sb_idx) ||
             SB_BLK_TYPE(sb_idx) == Static ||
             SB_BLK_TYPE(sb_idx) == Static_Named ||
             SB_BLK_TYPE(sb_idx) == Static_Local) {
            SB_BLK_TYPE(sb_idx) = Threadprivate;
         }
      }


      if (SB_BLK_TYPE(sb_idx) == Equivalenced && SB_HOSTED_STACK(sb_idx)) {

# if defined(_DEBUG)

         if (SB_LEN_FLD(sb_idx) != CN_Tbl_Idx) {
            PRINTMSG(SB_DEF_LINE(sb_idx), 1201, Internal, SB_DEF_COLUMN(sb_idx),
                     SB_NAME_PTR(sb_idx));
         }
# endif

         result.fld	= SB_LEN_FLD(sb_idx);
         result.idx	= SB_LEN_IDX(sb_idx);

         align_bit_length(&result, TARGET_BITS_PER_WORD);

         if (result.fld == NO_Tbl_Idx) {
            SB_LEN_FLD(sb_idx) = CN_Tbl_Idx;
            SB_LEN_IDX(sb_idx) = ntr_const_tbl(result.type_idx,
                                               FALSE,
                                               result.constant);
         }
         else {
            SB_LEN_FLD(sb_idx) = result.fld;
            SB_LEN_IDX(sb_idx) = result.idx;
         }

# if !defined(_SEPARATE_NONCOMMON_EQUIV_GROUPS)

         /* Host associated stack equivalence group.  Merge this group with  */
         /* the host associated stack.  If there is no hosted group, just    */
         /* make this one the hosted stack group.                            */

         if (SCP_SB_HOSTED_STACK_IDX(curr_scp_idx) == NULL_IDX) {
            CREATE_ID(name, sb_name[Stack_Host_Blk], sb_len[Stack_Host_Blk]);
            NTR_NAME_POOL(&(name.words[0]), sb_len[Stack_Host_Blk], np_idx);
            SB_NAME_IDX(sb_idx)			  = np_idx;
            SB_NAME_LEN(sb_idx)			  = sb_len[Stack_Host_Blk];
            SB_BLK_TYPE(sb_idx)			  = Stack;
            SB_RUNTIME_INIT(sb_idx)		  = TRUE;
            SCP_SB_HOSTED_STACK_IDX(curr_scp_idx) = sb_idx;
         }
         else {  /* Merge this group with the already existing @STACK_HOST    */
                 /* group.  Adjust all offsets and ATD_STOR_BLK_IDX's.        */

            new_sb_idx	= SCP_SB_HOSTED_STACK_IDX(curr_scp_idx);

            offset.fld	= SB_LEN_FLD(new_sb_idx);
            offset.idx	= SB_LEN_IDX(new_sb_idx);

            align_bit_length(&offset, TARGET_BITS_PER_WORD);

            group	= SCP_FIRST_EQUIV_GRP(curr_scp_idx);

            while (ATD_STOR_BLK_IDX(EQ_ATTR_IDX(group)) != sb_idx) {
               group = EQ_NEXT_EQUIV_GRP(group);
            }
            item = group;

# if defined(_TARGET_DOUBLE_ALIGN)

            if (EQ_DALIGN_ME(item)) {
               C_TO_F_INT(result.constant,
                          TARGET_BITS_PER_WORD * 2,
                          CG_INTEGER_DEFAULT_TYPE);
               result.fld		= NO_Tbl_Idx;
               result.type_idx		= CG_INTEGER_DEFAULT_TYPE;
               left.fld			= CN_Tbl_Idx;
               left.idx			= CN_INTEGER_ZERO_IDX;

               size_offset_binary_calc(&offset, &result, Mod_Opr, &result);

               size_offset_logical_calc(&result, &left, Ne_Opr, &result);

               if (THIS_IS_TRUE(result.constant, result.type_idx)) {

                  /* This is not on a double word boundary */

                  result.idx	= CN_INTEGER_BITS_PER_WORD_IDX;
                  result.fld	= CN_Tbl_Idx;

                  /* Rather than padding, remove the 32-bit pad */
                  /* added to the front of this equivalence group. */

                  size_offset_binary_calc(&offset,
                                          &result,
                                          EQ_DALIGN_SHIFT(item) ? Minus_Opr :
                                                                  Plus_Opr,
                                          &offset);
               }
            }
# endif

            while (item != NULL_IDX) {

               if (ATD_STOR_BLK_IDX(EQ_ATTR_IDX(item)) == sb_idx) {
                  ATD_STOR_BLK_IDX(EQ_ATTR_IDX(item))	= new_sb_idx;

                  result.fld = ATD_OFFSET_FLD(EQ_ATTR_IDX(item));
                  result.idx = ATD_OFFSET_IDX(EQ_ATTR_IDX(item));

                  size_offset_binary_calc(&result, &offset, Plus_Opr, &result);

                  if (result.fld == NO_Tbl_Idx) {
                     ATD_OFFSET_FLD(EQ_ATTR_IDX(item)) = CN_Tbl_Idx;
                     ATD_OFFSET_IDX(EQ_ATTR_IDX(item)) = ntr_const_tbl(
                                                             result.type_idx,
                                                             FALSE,
                                                             result.constant);
                  }
                  else {
                     ATD_OFFSET_FLD(EQ_ATTR_IDX(item)) = result.fld;
                     ATD_OFFSET_IDX(EQ_ATTR_IDX(item)) = result.idx;
                  }
               }
               item = EQ_NEXT_EQUIV_OBJ(item);
            }

            result.fld	= SB_LEN_FLD(sb_idx);
            result.idx	= SB_LEN_IDX(sb_idx);

            size_offset_binary_calc(&result, &offset, Plus_Opr, &result);

            if (result.fld == NO_Tbl_Idx) {
               SB_LEN_FLD(new_sb_idx) = CN_Tbl_Idx;
               SB_LEN_IDX(new_sb_idx) = ntr_const_tbl(result.type_idx,
                                                      FALSE,
                                                      result.constant);
            }
            else {
               SB_LEN_FLD(new_sb_idx) = result.fld;
               SB_LEN_IDX(new_sb_idx) = result.idx;
            }
         }
# endif

         continue;
      }

      if (SB_HIDDEN(sb_idx)) {

         /* If two blocks are USE associated from different modules, the   */
         /* second one is marked hidden and SB_MERGED_BLK_IDX indexes to   */
         /* the first storage block.  This resolves SB_MERGED_BLK_IDX,     */
         /* SB_DEF_MULT_SCPS and SB_LEN_IDX.                               */
        
         same_sb_idx = SB_MERGED_BLK_IDX(sb_idx);

         while (SB_MERGED_BLK_IDX(same_sb_idx) != NULL_IDX) {
            same_sb_idx = SB_MERGED_BLK_IDX(same_sb_idx);
         }

         SB_MERGED_BLK_IDX(sb_idx) = same_sb_idx;

         if (SB_IS_COMMON(sb_idx)) {

            if (SB_COMMON_NEEDS_OFFSET(same_sb_idx)) {
               check_and_allocate_common_storage(same_sb_idx);
            }

            if (SB_HOST_ASSOCIATED(sb_idx)) {

               /* Find the common block for the original scope.  It should  */
               /* have all its offsets assigned and the block length should */
               /* be correct.  Copy down the block length so it can be      */
               /* compared to this blocks length.  It can then be updated   */
               /* if necessary.                                             */

               host_sb_idx = srch_stor_blk_tbl(SB_NAME_PTR(sb_idx),
                                               SB_NAME_LEN(sb_idx),
                                               SB_ORIG_SCP_IDX(sb_idx));

               SB_LEN_FLD(sb_idx)	= SB_LEN_FLD(host_sb_idx);
               SB_LEN_IDX(sb_idx)	= SB_LEN_IDX(host_sb_idx);
            }
         }

         if (SB_AUXILIARY(sb_idx) != SB_AUXILIARY(same_sb_idx)) {

            if (!SB_DCL_ERR(same_sb_idx)) {
               PRINTMSG(SB_DEF_LINE(same_sb_idx), 942, Error, 
                        SB_DEF_COLUMN(same_sb_idx),
                        SB_NAME_PTR(same_sb_idx));
            }
            SB_DCL_ERR(same_sb_idx)	= TRUE;
         }
         else if (SB_BLK_TYPE(sb_idx) != SB_BLK_TYPE(same_sb_idx)) {

            if (!SB_DCL_ERR(same_sb_idx)) {
               PRINTMSG(SB_DEF_LINE(same_sb_idx), 941, Error, 
                        SB_DEF_COLUMN(same_sb_idx),
                        SB_NAME_PTR(same_sb_idx));
            }
            SB_DCL_ERR(same_sb_idx)	= TRUE;
         }
      
         if (SB_DEF_MULT_SCPS(sb_idx)) {
            SB_DEF_MULT_SCPS(same_sb_idx) = TRUE;
         }

         if (SB_HAS_RENAMES(sb_idx)) {
            SB_HAS_RENAMES(same_sb_idx) = TRUE;
         }

         if (fold_relationals(SB_LEN_IDX(sb_idx),
                              SB_LEN_IDX(same_sb_idx),
                              Gt_Opr)) {
            SB_LEN_FLD(same_sb_idx)	= SB_LEN_FLD(sb_idx);
            SB_LEN_IDX(same_sb_idx)	= SB_LEN_IDX(sb_idx);
         }
      }
      else if (SB_USE_ASSOCIATED(sb_idx) || SB_HOST_ASSOCIATED(sb_idx)) {

# if defined(_TMP_GIVES_COMMON_LENGTH)
         if (SB_BLK_TYPE(sb_idx) == Static || SB_IS_COMMON(sb_idx)) 
# else
         if (SB_PAD_BLK(sb_idx)) 
# endif
				{
            if (SB_LEN_FLD(sb_idx) == AT_Tbl_Idx ||
                fold_relationals(CN_INTEGER_ZERO_IDX,
                                 SB_LEN_IDX(sb_idx),
                                 Ne_Opr)) {

              /* Create a tmp that resides at the last word of this block.    */
              /* This is created so that ccg gets the block length correct.   */
              /* When a static block (static, common or module) is USE        */
              /* associated or HOST associated, not everything in the storage */
              /* block is associated (brought into the current scope), so ccg */
              /* has no way of knowing what the block length is.  This way    */
              /* ccg can figure it out correctly.  The tmp is marked as       */
              /* unreferenced, but is sent across the interface.  This tmp is */
              /* NOT sent out to the module information tables.  If a tmp is  */
              /* needed for this storage block, when the module information   */
              /* table is included, another one will be made.  The offset is  */
              /* assigned in final_attr_semantics, because all the blocks     */
              /* have not merged yet.  The type is TYPELESS_DEFAULT_TYPE      */
              /* because we need a type that is the length of one word on     */
              /* whatever machine this is.                                    */

               attr_idx = gen_compiler_tmp(SB_DEF_LINE(sb_idx),
                                           SB_DEF_COLUMN(sb_idx),
                                           Priv, TRUE);

               ATD_TYPE_IDX(attr_idx)		= TYPELESS_DEFAULT_TYPE;
               AT_REFERENCED(attr_idx)		= Not_Referenced;
               ATD_STOR_BLK_IDX(attr_idx)	= sb_idx;
               ADD_ATTR_TO_LOCAL_LIST(attr_idx);
            }
         }

         if (SB_HOST_ASSOCIATED(sb_idx)) {

            /* BLK_LEN needs to be updated - so get the original sb_idx from */
            /* the parent and change BLK_LEN if the original is bigger.  It  */
            /* would be possible for the block length of the original to be  */
            /* greater than that of the host, but we want to allow that.     */
            /* The only way this can happen is if a common block is declared */
            /* in the original and the child with different lengths and      */
            /* different variable names.  Then if the child host associates  */
            /* a variable from the original procedures common block, both    */
            /* versions of the common block get into this procedure with     */
            /* different lengths.  About this time, it is time to issue a    */
            /* warning.   Actually that case will not get here, because the  */
            /* host associated version will be hidden.  So there probably is */
            /* no way to have the original block length be smaller than the  */
            /* child block length.                                           */

            host_sb_idx = srch_stor_blk_tbl(SB_NAME_PTR(sb_idx),
                                            SB_NAME_LEN(sb_idx),
                                            SB_ORIG_SCP_IDX(sb_idx));

            if (fold_relationals(SB_LEN_IDX(host_sb_idx),
                                 SB_LEN_IDX(sb_idx),
                                 Gt_Opr)) {
               SB_LEN_FLD(sb_idx)	= SB_LEN_FLD(host_sb_idx);
               SB_LEN_IDX(sb_idx)	= SB_LEN_IDX(host_sb_idx);
            }
         }
      }
      else if (SB_PAD_BLK(sb_idx)) {

         /* Need to add a tmp to the end of a padded block so that we can */
         /* have padding at the end of a block.                           */

         if (SB_LEN_FLD(sb_idx) == AT_Tbl_Idx ||
             fold_relationals(CN_INTEGER_ZERO_IDX,
                              SB_LEN_IDX(sb_idx),
                              Ne_Opr)) {

            /* Create a tmp that resides at the last word of this block.    */
            /* This is created so that ccg gets the block length correct.   */
            /* If padding is added, ccg has no way of knowing what the      */
            /* block length is.  This way ccg can figure it out correctly.  */
            /* The tmp is marked as unreferenced, but is sent across the    */
            /* interface.  This tmp is NOT sent out to the module           */
            /* information tables.  If a tmp is needed for this storage     */
            /* block, when the module information table is included,        */
            /* another one will be made.  The offset is assigned in         */
            /* final_attr_semantics, because all the blocks have not merged */
            /* yet.  The type is TYPELESS_DEFAULT_TYPE because we need a    */
            /* type that is the length of one word on the specific hardware.*/

            attr_idx = gen_compiler_tmp(SB_DEF_LINE(sb_idx),
                                        SB_DEF_COLUMN(sb_idx),
                                        Priv, TRUE);

            ATD_TYPE_IDX(attr_idx)	= TYPELESS_DEFAULT_TYPE;
            AT_REFERENCED(attr_idx)	= Not_Referenced;
            ATD_STOR_BLK_IDX(attr_idx)	= sb_idx;
            ADD_ATTR_TO_LOCAL_LIST(attr_idx);
         }
      }
   }

   TRACE (Func_Exit, "storage_blk_resolution", NULL);

   return;

}  /* storage_blk_resolution */

/******************************************************************************\
|*									      *|
|* Description:								      *|
|*	This procedure does semantics stuff for interface blocks.	      *|
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
void interface_semantics_pass_driver (void)

{
   int		sb_idx;

   TRACE (Func_Entry, "interface_semantics_pass_driver", NULL);

   /* The interface is pulled back into the parent's scope to be used for */
   /* semantic checking and to allow the caller to allocate the function  */
   /* result for a non-scalar, character, structure, or dope vector rslt. */
   /* This allows several assumptions.  First, if this is a SUBROUTINE    */
   /* there is no result, so the parent routine will not need to allocate */
   /* a function result, because of this NO IR will be generated using    */
   /* any of the dummy arguments.  The only information need by the       */
   /* parent is for semantic checking.  That information is in the        */
   /* parent's scope already, because the interface subroutine is in the  */
   /* parent's scope.  Since this SUBROUTINE interface block causes no IR */
   /* to be generated, nothing goes on the ATP_PGM_SPEC_LIST so the list  */
   /* creation can be skipped.  assign_storage_offsets must be called     */
   /* because it does semantic checking.  The storage block table must    */
   /* be gone through and all storage blocks assigned to this scope are   */
   /* switched to scope 0, because this scope is going to be removed.     */
   /* This effectively removes them from the storage block tbl.           */

   /* If this is a FUNCTION that is non-scalar, character, structure or a */
   /* dope vector, the parent will generate code using things declared in */
   /* this interface block, when it generates code to allocate space for  */
   /* the function result at the call site.  ATP_PGM_SPEC_LIST is a list  */
   /* of all common variables, module variables, dummy procedures and     */
   /* tmps needed to determine the length of the function result.  Any    */
   /* storage blocks accessed are moved to the parent's scope.  Storage   */
   /* block resolution in the parent's scope is done in assign_storage_   */
   /* offsets when it is called for the parent.  (Any tmps generated      */
   /* defaulted to the parent's stack when they were created.)  Since     */
   /* none of the dummy args are on the ATP_PGM_SPEC_LIST the darg block  */
   /* can be removed.  (The dargs are used as stmt function dargs if they */
   /* are needed to calculate the result's length.  See bounds resolution */
   /* for more details.)  After ATP_PGM_SPEC_LIST is created, the storage */
   /* table is gone thru and any blocks still left are moved to scope 0,  */
   /* so they don't cause problems later after this scope is removed.     */

   decl_semantics();
   final_decl_semantics();

   PRINT_DBG_SYTB;		/* Print scp if SCP_DBG_PRINT_SYTB = TRUE */

   for (sb_idx = 1; sb_idx <= stor_blk_tbl_idx; sb_idx++) {

      if (SB_SCP_IDX(sb_idx) == curr_scp_idx) {
         SB_SCP_IDX(sb_idx)		= NULL_IDX;
         SB_ORIG_SCP_IDX(sb_idx)	= NULL_IDX;
      }
   }

   ATP_SCP_ALIVE(SCP_ATTR_IDX(curr_scp_idx)) = FALSE;

   TRACE (Func_Exit, "interface_semantics_pass_driver", NULL);

   return;

   }  /* interface_semantics_pass_driver */

/******************************************************************************\
|*									      *|
|* Description:								      *|
|*	Clear the stmt_tmp_tbl, freeing any list nodes.                       *|
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

static void free_stmt_tmp_tbl(void)

{

   int		i;
   int		k;
   int		list_idx;
   int		list2_idx;

   TRACE (Func_Entry, "free_stmt_tmp_tbl", NULL);

   for (i = 0; i < Num_Linear_Types; i++) {

      if (stmt_tmp_tbl[i].scalar_tmps_head < 0) {
         continue;
      }

      if (stmt_tmp_tbl[i].scalar_tmps_head > 0) {
         list_idx = stmt_tmp_tbl[i].scalar_tmps_head;

         while (list_idx) {
            list2_idx = list_idx;
            list_idx = IL_NEXT_LIST_IDX(list_idx);
            FREE_IR_LIST_NODE(list2_idx);
         }
      }

      stmt_tmp_tbl[i].scalar_tmps_head = NULL_IDX;
      stmt_tmp_tbl[i].scalar_tmps_tail = NULL_IDX;

      for (k = 0; k < 8; k++) {

         if (stmt_tmp_tbl[i].dope_vector_tmps_head[k] > 0) {
            list_idx = stmt_tmp_tbl[i].dope_vector_tmps_head[k];
  
            while (list_idx) {
               list2_idx = list_idx;
               list_idx = IL_NEXT_LIST_IDX(list_idx);
               FREE_IR_LIST_NODE(list2_idx);
            }
         }

         stmt_tmp_tbl[i].dope_vector_tmps_head[k] = NULL_IDX;
         stmt_tmp_tbl[i].dope_vector_tmps_tail[k] = NULL_IDX;
      }
   }

   TRACE (Func_Exit, "free_stmt_tmp_tbl", NULL);

   return;

}  /* free_stmt_tmp_tbl */

/******************************************************************************\
|*									      *|
|* Description:								      *|
|*	Reset the stmt_tmp_tbl to it's original values.                       *|
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

static void reset_stmt_tmp_tbl(void)

{
   int		i;
   int		k;


   TRACE (Func_Entry, "reset_stmt_tmp_tbl", NULL);

   for (i = 0; i < Num_Linear_Types; i++) {
      stmt_tmp_tbl[i].scalar_tmps_head = init_stmt_tmp_tbl[i].scalar_tmps_head;
      stmt_tmp_tbl[i].scalar_tmps_tail = init_stmt_tmp_tbl[i].scalar_tmps_tail;

      for (k = 0; k < 8; k++) {
         stmt_tmp_tbl[i].dope_vector_tmps_head[k] = 
                   init_stmt_tmp_tbl[i].dope_vector_tmps_head[k];
         stmt_tmp_tbl[i].dope_vector_tmps_tail[k] = 
                   init_stmt_tmp_tbl[i].dope_vector_tmps_tail[k];

      }
   }

   TRACE (Func_Exit, "reset_stmt_tmp_tbl", NULL);

   return;

}  /* reset_stmt_tmp_tbl */

/******************************************************************************\
|*									      *|
|* Description:								      *|
|*	Check for global name definitions.  This routine searches the name    *|
|*	table and then compares or enters the global name table.  This        *|
|*	routine should only be called for definition or partial definition    *|
|*	situations.  References are handled in check_call_for_global_def      *|
|*	This routine does check for common block/global name conflicts.       *|
|*									      *|
|* Input parameters:							      *|
|*	attr_idx -> Attribute table entry of global to check.                 *|
|*									      *|
|* Output parameters:							      *|
|*	NONE								      *|
|*									      *|
|* Returns:								      *|
|*	NOTHING								      *|
|*									      *|
\******************************************************************************/

int	check_global_pgm_unit(int	attr_idx)

{
   int			ga_common_idx;
   int			ga_pgm_idx;
   msg_severities_type	msg_level;
   int			name_idx;
   int			new_ga_idx;
   int			ref_ga_idx;


   TRACE (Func_Entry, "check_global_pgm_unit", NULL);

   if (srch_global_name_tbl(AT_OBJ_NAME_PTR(attr_idx), 
                            AT_NAME_LEN(attr_idx),
                            &name_idx)) {

      if (GA_OBJ_CLASS(GN_ATTR_IDX(name_idx)) == Common_Block) {
         ga_common_idx	= GN_ATTR_IDX(name_idx);
         ga_pgm_idx	= GAC_PGM_UNIT_IDX(ga_common_idx);
      }
      else {
         ga_common_idx	= NULL_IDX;
         ga_pgm_idx	= GN_ATTR_IDX(name_idx);
      }

      if (ga_common_idx != NULL_IDX && ga_pgm_idx == NULL_IDX) {  

         /* This is a common block name - issue a message */

# if defined(_ERROR_DUPLICATE_GLOBALS)
         msg_level = Error;
# else
         msg_level = (ATP_PGM_UNIT(attr_idx) == Module) ? Error : Ansi;
# endif
         PRINTMSG(AT_DEF_LINE(attr_idx), 1006, msg_level,
                  AT_DEF_COLUMN(attr_idx),
                  AT_OBJ_NAME_PTR(attr_idx),
                  pgm_unit_str[ATP_PGM_UNIT(attr_idx)]);
      }

      if (ga_pgm_idx == NULL_IDX) {  /* No previous global entry as pgm. */
         ga_pgm_idx	= ntr_global_attr_tbl(attr_idx, name_idx);

         /* Must be a common block or we wouldn't be here. */

         GAC_PGM_UNIT_IDX(ga_common_idx)	= ga_pgm_idx;

         fill_in_global_attr_ntry(ga_pgm_idx, attr_idx, NULL_IDX);
      }
      else if (GAP_PGM_UNIT_DEFINED(ga_pgm_idx) && 
               GAP_NEXT_PGM_UNIT_IDX(ga_pgm_idx) == NULL_IDX) {

         /* Found a definition.  Not the interface but the actual definition */
         /* If we actually have the definition, it will be the only entry.   */

         global_name_semantics(ga_pgm_idx,
                               NULL_IDX,
                               NULL_IDX,
                               NULL_IDX,
                               attr_idx);  /* Know this is a definition,  */
                                           /* because we are passing only */
                                           /* an attr_idx.                */
      }
      else if (GA_DEFINED(ga_pgm_idx) && 
               !GAP_PGM_UNIT_DEFINED(ga_pgm_idx) &&
                GAP_IN_INTERFACE_BLK(ga_pgm_idx)) {

         /* This is an interface definition.                              */
         /* Compare - Replace if this is the true definition.             */

         global_name_semantics(ga_pgm_idx,
                               NULL_IDX,
                               NULL_IDX,
                               NULL_IDX,
                               attr_idx);  /* Know this is a definition,  */

         if (ATP_EXPL_ITRFC(attr_idx) && 
             !SCP_IS_INTERFACE(curr_scp_idx) &&
             (attr_idx == SCP_ATTR_IDX(curr_scp_idx) ||
             (ATP_SCP_ALIVE(attr_idx) && ATP_ALT_ENTRY(attr_idx)))) {

            /* This is a definition.  Replace the interface block */

            new_ga_idx = ntr_global_attr_tbl(attr_idx, name_idx);

            fill_in_global_attr_ntry(new_ga_idx, attr_idx, NULL_IDX);

            if (GA_OBJ_CLASS(GN_ATTR_IDX(name_idx)) == Common_Block) {
               GAC_PGM_UNIT_IDX(GN_ATTR_IDX(name_idx)) = new_ga_idx;
            }
            else {
               GN_ATTR_IDX(name_idx) = new_ga_idx;
            }
         }
      }
      else if (GA_DEFINED(ga_pgm_idx) && 
               !GAP_PGM_UNIT_DEFINED(ga_pgm_idx)) {
 
         /* A partial definition.  Just enter it.  KAY */

         new_ga_idx = ntr_global_attr_tbl(attr_idx, name_idx);
         fill_in_global_attr_ntry(new_ga_idx, attr_idx, NULL_IDX);

         GAP_NEXT_PGM_UNIT_IDX(new_ga_idx) = ga_pgm_idx;

         if (GA_OBJ_CLASS(GN_ATTR_IDX(name_idx)) == Common_Block) {
            GAC_PGM_UNIT_IDX(GN_ATTR_IDX(name_idx)) = new_ga_idx;
         }
         else {
            GN_ATTR_IDX(name_idx) = new_ga_idx;
         }
      }
      else {

         /* Have one or more references or interface block definitions.   */
         /* If this is a true definition, then compare all previous       */
         /* references and interface definitions.  Then remove them and   */
         /* add the definition.  No free list, so let the memory go.      */

         ref_ga_idx	= ga_pgm_idx;	

         while (ref_ga_idx != NULL_IDX) {
            global_name_semantics(ref_ga_idx, /* Reference */
                                  NULL_IDX,
                                  NULL_IDX,
                                  NULL_IDX,
                                  attr_idx);  /* Know this is a definition,  */
                                              /* because we are passing only */
                                              /* an attr_idx.                */
            ref_ga_idx	= GAP_NEXT_PGM_UNIT_IDX(ref_ga_idx);
         }

         new_ga_idx = ntr_global_attr_tbl(attr_idx, name_idx);
         fill_in_global_attr_ntry(new_ga_idx, attr_idx, NULL_IDX);

         if (GA_OBJ_CLASS(GN_ATTR_IDX(name_idx)) == Common_Block) {
            GAC_PGM_UNIT_IDX(GN_ATTR_IDX(name_idx)) = new_ga_idx;
         }
         else {
            GN_ATTR_IDX(name_idx) = new_ga_idx;
         }
      }
   }
   else {  /* Enter a definition */
      ntr_global_name_tbl(attr_idx, NULL_IDX, name_idx);
   }

   TRACE (Func_Exit, "check_global_pgm_unit", NULL);

   return(name_idx);

}  /* check_global_pgm_unit */

/******************************************************************************\
|*									      *|
|* Description:								      *|
|*	Check for mutiple entries in functions that are to be returned in     *|
|*      registers on solaris. Then generate the multiple return stmts that    *|
|*      are to replace all the original returns.                              *|
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

# ifdef _SEPARATE_FUNCTION_RETURNS
static void check_multiple_entry_func(void)

{
   int			al_idx;
   int			attr_idx;
   int			branch_idx;
   int			col;
   boolean		has_conflict = FALSE;
   int			i;
   int			ir_idx;
   int			label_idx;
   int			line;
   int			list_idx;
   int			prev_type_idx;
   int			save_curr_stmt_sh_idx;
   int			tmp_idx;
   int			type_idx;


   TRACE (Func_Entry, "check_multiple_entry_func", NULL);

   save_curr_stmt_sh_idx = curr_stmt_sh_idx;
   attr_idx = SCP_ATTR_IDX(curr_scp_idx);
   prev_type_idx = ATD_TYPE_IDX(ATP_RSLT_IDX(attr_idx));

   al_idx = SCP_ENTRY_IDX(curr_scp_idx);

   for (i = 0; i < SCP_ALT_ENTRY_CNT(curr_scp_idx); i++) {

      attr_idx = AL_ATTR_IDX(al_idx);
      type_idx = ATD_TYPE_IDX(ATP_RSLT_IDX(attr_idx));

      if (TYP_TYPE(type_idx) != TYP_TYPE(prev_type_idx)) {
         has_conflict = TRUE;
         break;
      }
      else if ((TYP_TYPE(type_idx) == Real ||
                TYP_TYPE(type_idx) == Complex) &&
               TYP_LINEAR(type_idx) != TYP_LINEAR(prev_type_idx)) {

         has_conflict = TRUE;
         break;
      }
       
      al_idx = AL_NEXT_IDX(al_idx);
   }

   if (has_conflict) {

      /* get main pgm unit attr */

      attr_idx = SCP_ATTR_IDX(curr_scp_idx);
      type_idx = ATD_TYPE_IDX(ATP_RSLT_IDX(attr_idx));
      line = SH_GLB_LINE(SCP_LAST_SH_IDX(curr_scp_idx));
      col  = SH_COL_NUM(SCP_LAST_SH_IDX(curr_scp_idx));
   
      set_up_which_entry_tmp();

      tmp_idx = SCP_WHICH_ENTRY_TMP(curr_scp_idx);

      /* gen the final branch to label for returns */

      label_idx = gen_internal_lbl(line);
      curr_stmt_sh_idx = SCP_LAST_SH_IDX(curr_scp_idx);
      gen_sh(After, Continue_Stmt, line, col, FALSE, TRUE, TRUE);
   
      NTR_IR_TBL(ir_idx);
      SH_IR_IDX(curr_stmt_sh_idx) = ir_idx;
      IR_OPR(ir_idx)              = Label_Opr;
      IR_TYPE_IDX(ir_idx)         = TYPELESS_DEFAULT_TYPE;
      IR_LINE_NUM(ir_idx)         = line;
      IR_COL_NUM(ir_idx)          = col;
      IR_FLD_L(ir_idx)            = AT_Tbl_Idx;
      IR_IDX_L(ir_idx)            = label_idx;
      IR_COL_NUM_L(ir_idx)        = col;
      IR_LINE_NUM_L(ir_idx)       = line;

      AT_DEFINED(label_idx)       = TRUE;
      ATL_DEF_STMT_IDX(label_idx) = curr_stmt_sh_idx;
   
      SH_P2_SKIP_ME(curr_stmt_sh_idx) = TRUE;

      SCP_RETURN_LABEL(curr_scp_idx) = label_idx;

      /* set up index branch stmt */

      NTR_IR_TBL(branch_idx);
      IR_OPR(branch_idx) = Br_Index_Opr;
      IR_TYPE_IDX(branch_idx) = CG_INTEGER_DEFAULT_TYPE;
      IR_LINE_NUM(branch_idx)   = line;
      IR_COL_NUM(branch_idx)    = col;
      IR_FLD_L(branch_idx)      = AT_Tbl_Idx;
      IR_IDX_L(branch_idx)      = tmp_idx;
      IR_LINE_NUM_L(branch_idx) = line;
      IR_COL_NUM_L(branch_idx)  = col;

      gen_sh(After, Goto_Stmt, line, col, FALSE, FALSE, TRUE);
      SH_IR_IDX(curr_stmt_sh_idx) = branch_idx;
      SH_P2_SKIP_ME(curr_stmt_sh_idx) = TRUE;

      SCP_LAST_SH_IDX(curr_scp_idx) = curr_stmt_sh_idx;

      /* add the return and label to return code */

      NTR_IR_LIST_TBL(list_idx);
      IR_FLD_R(branch_idx) = IL_Tbl_Idx;
      IR_IDX_R(branch_idx) = list_idx;
      IR_LIST_CNT_R(branch_idx) = 1;
     
      label_idx = gen_internal_lbl(line);
      curr_stmt_sh_idx = SCP_LAST_SH_IDX(curr_scp_idx);
      gen_sh(After, Continue_Stmt, line, col, FALSE, TRUE, TRUE);

      NTR_IR_TBL(ir_idx);
      SH_IR_IDX(curr_stmt_sh_idx) = ir_idx;
      IR_OPR(ir_idx)              = Label_Opr;
      IR_TYPE_IDX(ir_idx)         = TYPELESS_DEFAULT_TYPE;
      IR_LINE_NUM(ir_idx)         = line;
      IR_COL_NUM(ir_idx)          = col;
      IR_FLD_L(ir_idx)            = AT_Tbl_Idx;
      IR_IDX_L(ir_idx)            = label_idx;
      IR_COL_NUM_L(ir_idx)        = col;
      IR_LINE_NUM_L(ir_idx)       = line;

      AT_DEFINED(label_idx)       = TRUE;
      ATL_DEF_STMT_IDX(label_idx) = curr_stmt_sh_idx;

      SH_P2_SKIP_ME(curr_stmt_sh_idx) = TRUE;

      IL_FLD(list_idx) = AT_Tbl_Idx;
      IL_IDX(list_idx) = label_idx;
      IL_LINE_NUM(list_idx) = line;
      IL_COL_NUM(list_idx)  = col;
      
      NTR_IR_TBL(ir_idx);
      IR_OPR(ir_idx) = Return_Opr;
      IR_TYPE_IDX(ir_idx) = type_idx;
      IR_LINE_NUM(ir_idx) = line;
      IR_COL_NUM(ir_idx) = col;

      IR_FLD_R(ir_idx) = AT_Tbl_Idx;
      IR_IDX_R(ir_idx) = ATP_RSLT_IDX(attr_idx);
      IR_LINE_NUM_R(ir_idx) = line;
      IR_COL_NUM_R(ir_idx)  = col;

      gen_sh(After, Return_Stmt, line, col, FALSE, FALSE, TRUE);
      SH_IR_IDX(curr_stmt_sh_idx) = ir_idx;
      SH_P2_SKIP_ME(curr_stmt_sh_idx) = TRUE;

      SCP_LAST_SH_IDX(curr_scp_idx) = curr_stmt_sh_idx;

      al_idx = SCP_ENTRY_IDX(curr_scp_idx);

      for (i = 0; i < SCP_ALT_ENTRY_CNT(curr_scp_idx); i++) {
      
         attr_idx = AL_ATTR_IDX(al_idx);
         type_idx = ATD_TYPE_IDX(ATP_RSLT_IDX(attr_idx));


         /* add the return and label to return code */
         NTR_IR_LIST_TBL(IL_NEXT_LIST_IDX(list_idx));
         list_idx = IL_NEXT_LIST_IDX(list_idx);
         IR_LIST_CNT_R(branch_idx) += 1;

         label_idx = gen_internal_lbl(line);
         curr_stmt_sh_idx = SCP_LAST_SH_IDX(curr_scp_idx);
         gen_sh(After, Continue_Stmt, line, col, FALSE, TRUE, TRUE);

         NTR_IR_TBL(ir_idx);
         SH_IR_IDX(curr_stmt_sh_idx) = ir_idx;
         IR_OPR(ir_idx)              = Label_Opr;
         IR_TYPE_IDX(ir_idx)         = TYPELESS_DEFAULT_TYPE;
         IR_LINE_NUM(ir_idx)         = line;
         IR_COL_NUM(ir_idx)          = col;
         IR_FLD_L(ir_idx)            = AT_Tbl_Idx;
         IR_IDX_L(ir_idx)            = label_idx;
         IR_COL_NUM_L(ir_idx)        = col;
         IR_LINE_NUM_L(ir_idx)       = line;

         AT_DEFINED(label_idx)       = TRUE;
         ATL_DEF_STMT_IDX(label_idx) = curr_stmt_sh_idx;

         SH_P2_SKIP_ME(curr_stmt_sh_idx) = TRUE;

         IL_FLD(list_idx) = AT_Tbl_Idx;
         IL_IDX(list_idx) = label_idx;
         IL_LINE_NUM(list_idx) = line;
         IL_COL_NUM(list_idx)  = col;

         NTR_IR_TBL(ir_idx);
         IR_OPR(ir_idx) = Return_Opr;
         IR_TYPE_IDX(ir_idx) = type_idx;;
         IR_LINE_NUM(ir_idx) = line;
         IR_COL_NUM(ir_idx) = col;

         IR_FLD_R(ir_idx) = AT_Tbl_Idx;
         IR_IDX_R(ir_idx) = ATP_RSLT_IDX(attr_idx);
         IR_LINE_NUM_R(ir_idx) = line;
         IR_COL_NUM_R(ir_idx)  = col;

         gen_sh(After, Return_Stmt, line, col, FALSE, FALSE, TRUE);
         SH_IR_IDX(curr_stmt_sh_idx) = ir_idx;
         SH_P2_SKIP_ME(curr_stmt_sh_idx) = TRUE;

         SCP_LAST_SH_IDX(curr_scp_idx) = curr_stmt_sh_idx;

         al_idx = AL_NEXT_IDX(al_idx);
      }
   }
   

   curr_stmt_sh_idx = save_curr_stmt_sh_idx;

   TRACE (Func_Exit, "check_multiple_entry_func", NULL);

   return;

}  /* check_multiple_entry_func */
# endif

/******************************************************************************\
|*                                                                            *|
|* Description:                                                               *|
|*      Make all offsets in the group zero based.                             *|
|*                                                                            *|
|* Input parameters:                                                          *|
|*      eq_idx -> equiv table index for this group.                           *|
|*                                                                            *|
|* Output parameters:                                                         *|
|*      NONE                                                                  *|
|*                                                                            *|
|* Returns:                                                                   *|
|*      NONE                                                                  *|
|*                                                                            *|
\******************************************************************************/
static void     final_equivalence_semantics(void)
{
   int			attr_idx;
   size_offset_type	base;
   boolean		base_is_zero;
   int			eq_idx;
   int			group;
   int			item;
   size_offset_type	left;
   size_offset_type	length;
   size_offset_type	new_len;
   size_offset_type	new_offset;
   boolean		new_offset_ne_zero;
   size_offset_type	result;
   int			sb_idx;
   int			type_idx;
   size_offset_type	zero;
   boolean		dalign_offset_ok;
   boolean		dalign_shift_offset;
   int			t_idx;


   TRACE (Func_Entry, "final_equivalence_semantics", NULL);

   group = SCP_FIRST_EQUIV_GRP(curr_scp_idx);

   while (group != NULL_IDX) {

      if (EQ_SEMANTICS_DONE(group)) {
         group = EQ_NEXT_EQUIV_GRP(group);
         continue;
      }
      base.fld		= CN_Tbl_Idx;
      base.idx		= CN_INTEGER_ZERO_IDX;
      base_is_zero	= TRUE;
      eq_idx		= group;
      item		= eq_idx;

      /* Find the smallest offset for the entire group. */

      while (item != NULL_IDX) {

         if (EQ_OFFSET_IDX(item) != CN_INTEGER_ZERO_IDX &&
             fold_relationals(EQ_OFFSET_IDX(item), base.idx, Lt_Opr)) {
            base.idx		= EQ_OFFSET_IDX(item);
            base.fld		= EQ_OFFSET_FLD(item);
            base_is_zero	= FALSE;
         }
 
         item = EQ_NEXT_EQUIV_OBJ(item);
      }

      type_idx	= INTEGER_DEFAULT_TYPE;

      if (!base_is_zero) {
         result.idx		= CN_INTEGER_NEG_ONE_IDX;
         result.fld		= CN_Tbl_Idx;
         result.type_idx	= CG_INTEGER_DEFAULT_TYPE;

         size_offset_binary_calc(&base, &result, Mult_Opr, &base);
      }

      /* Only need to word align Static, because this is the only storage   */
      /* group that equivalenced items get added to.  For stack, each one   */
      /* is a different group.  For Common, offsets are assigned later.     */

      sb_idx = ATD_STOR_BLK_IDX(EQ_ATTR_IDX(group));

      if (SB_BLK_TYPE(sb_idx) == Static_Local ||
          SB_BLK_TYPE(sb_idx) == Static_Named ||
          SB_BLK_TYPE(sb_idx) == Static) { /* word align prev @DATA boundary */

         if (SB_LEN_FLD(sb_idx) == AT_Tbl_Idx ||
             SB_LEN_IDX(sb_idx) != CN_INTEGER_ZERO_IDX) {

            result.idx	= SB_LEN_IDX(sb_idx);
            result.fld	= SB_LEN_FLD(sb_idx);

            align_bit_length(&result, TARGET_BITS_PER_WORD);

            if (result.fld == NO_Tbl_Idx) {
               SB_LEN_FLD(sb_idx)	= CN_Tbl_Idx;
               SB_LEN_IDX(sb_idx)	= ntr_const_tbl(result.type_idx,
                                                        FALSE,
                                                        result.constant);
            }
            else {
               SB_LEN_FLD(sb_idx)	= result.fld;
               SB_LEN_IDX(sb_idx)	= result.idx;
            }

            base_is_zero	= FALSE;

            size_offset_binary_calc(&result, &base, Plus_Opr, &base);
         }
      }

      item			= eq_idx;

# if defined(_TARGET_DOUBLE_ALIGN)
      dalign_offset_ok		= FALSE;
      dalign_shift_offset	= FALSE;
# endif

      while (item != NULL_IDX) {
         attr_idx			= EQ_ATTR_IDX(item);

         if (!base_is_zero) {
            result.fld	= EQ_OFFSET_FLD(item);
            result.idx	= EQ_OFFSET_IDX(item);

            size_offset_binary_calc(&result, &base, Plus_Opr, &new_offset);

            if (new_offset.fld	== NO_Tbl_Idx) {
               EQ_OFFSET_FLD(item)	= CN_Tbl_Idx;
               EQ_OFFSET_IDX(item)	= ntr_const_tbl(new_offset.type_idx,
                                                        FALSE,
                                                        new_offset.constant);
            }
            else {
               EQ_OFFSET_FLD(item)	= new_offset.fld;
               EQ_OFFSET_IDX(item)	= new_offset.idx;
            }
         }

         type_idx		= ATD_TYPE_IDX(attr_idx);

         if (SB_HOSTED_STACK(sb_idx)) {
            AT_HOST_ASSOCIATED(attr_idx) = TRUE;
         }

         /* KAY - This needs fixing.  */

         if (TYP_TYPE(type_idx) != Character) {

            result.fld	= CN_Tbl_Idx;
            result.idx	= CN_INTEGER_BITS_PER_WORD_IDX;
            left.fld	= EQ_OFFSET_FLD(item);
            left.idx	= EQ_OFFSET_IDX(item);
            zero.fld	= CN_Tbl_Idx;
            zero.idx	= CN_INTEGER_ZERO_IDX;

            size_offset_binary_calc(&left, &result, Mod_Opr, &result);

            size_offset_logical_calc(&zero, &result, Ne_Opr, &result);
       
            new_offset_ne_zero = THIS_IS_TRUE(result.constant, result.type_idx);

            if (TYP_TYPE(type_idx) == Structure) {

               if (ATT_NUMERIC_CPNT(TYP_IDX(type_idx)) && new_offset_ne_zero) {

# if defined(_TARGET_PACK_HALF_WORD_TYPES)

                  t_idx = ATD_TYPE_IDX(SN_ATTR_IDX(
                                       ATT_FIRST_CPNT_IDX(attr_idx)));

                  if (PACK_HALF_WORD_TEST_CONDITION(t_idx)) {
                     C_TO_F_INT(result.constant,
                                TARGET_BITS_PER_WORD/2,
                                CG_INTEGER_DEFAULT_TYPE);
                     result.fld		= NO_Tbl_Idx;
                     result.type_idx	= CG_INTEGER_DEFAULT_TYPE;
                     left.fld		= EQ_OFFSET_FLD(item);
                     left.idx		= EQ_OFFSET_IDX(item);

                     size_offset_binary_calc(&left, &result, Mod_Opr, &result);
   
                     size_offset_logical_calc(&zero, &result, Ne_Opr, &result);

                     if (THIS_IS_TRUE(result.constant, result.type_idx)) {
                        PRINTMSG(EQ_LINE_NUM(item), 527, Error,
                                EQ_COLUMN_NUM(item),
                                AT_OBJ_NAME_PTR(attr_idx));
                     }
                  }
                  else {
                     PRINTMSG(EQ_LINE_NUM(item), 527, Error,
                              EQ_COLUMN_NUM(item),
                              AT_OBJ_NAME_PTR(attr_idx));
                  }

# elif defined(_INTEGER_1_AND_2)

                  if (on_off_flags.integer_1_and_2) {

                     t_idx = ATD_TYPE_IDX(SN_ATTR_IDX(
                                          ATT_FIRST_CPNT_IDX(attr_idx)));

                     if (PACK_8_BIT_TEST_CONDITION(t_idx)) {
                        C_TO_F_INT(result.constant, 8, CG_INTEGER_DEFAULT_TYPE);
                        result.fld		= NO_Tbl_Idx;
                        result.type_idx		= CG_INTEGER_DEFAULT_TYPE;
                        left.fld		= EQ_OFFSET_FLD(item);
                        left.idx		= EQ_OFFSET_IDX(item);

                        size_offset_binary_calc(&left,&result,Mod_Opr,&result);
   
                        size_offset_logical_calc(&zero,&result,Ne_Opr,&result);

                        if (THIS_IS_TRUE(result.constant, result.type_idx)) {
                           PRINTMSG(EQ_LINE_NUM(item), 527, Error,
                                   EQ_COLUMN_NUM(item),
                                   AT_OBJ_NAME_PTR(attr_idx));
                        }
                     }
                     else if (PACK_16_BIT_TEST_CONDITION(t_idx)) {
                        C_TO_F_INT(result.constant, 16,CG_INTEGER_DEFAULT_TYPE);
                        result.fld		= NO_Tbl_Idx;
                        result.type_idx		= CG_INTEGER_DEFAULT_TYPE;
                        left.fld		= EQ_OFFSET_FLD(item);
                        left.idx		= EQ_OFFSET_IDX(item);

                        size_offset_binary_calc(&left,&result,Mod_Opr,&result);
                        size_offset_logical_calc(&zero,&result,Ne_Opr,&result);

                        if (THIS_IS_TRUE(result.constant, result.type_idx)) {
                           PRINTMSG(EQ_LINE_NUM(item), 527, Error,
                                   EQ_COLUMN_NUM(item),
                                   AT_OBJ_NAME_PTR(attr_idx));
                        }
                     }
                     else {
                        PRINTMSG(EQ_LINE_NUM(item), 527, Error,
                                 EQ_COLUMN_NUM(item),
                                 AT_OBJ_NAME_PTR(attr_idx));
                     }
                  }
# else
                  PRINTMSG(EQ_LINE_NUM(item), 527, Error,
                          EQ_COLUMN_NUM(item),
                          AT_OBJ_NAME_PTR(attr_idx));
# endif
               }
            }
            else if (new_offset_ne_zero) {

# if defined(_TARGET_PACK_HALF_WORD_TYPES)

               if (PACK_HALF_WORD_TEST_CONDITION(type_idx)) {

                  C_TO_F_INT(result.constant,
                             TARGET_BITS_PER_WORD/2,
                             CG_INTEGER_DEFAULT_TYPE);
                  result.fld		= NO_Tbl_Idx;
                  result.type_idx	= CG_INTEGER_DEFAULT_TYPE;
                  left.fld		= EQ_OFFSET_FLD(item);
                  left.idx		= EQ_OFFSET_IDX(item);
                  zero.fld		= CN_Tbl_Idx;
                  zero.idx		= CN_INTEGER_ZERO_IDX;

                  size_offset_binary_calc(&left, &result, Mod_Opr, &result);
   
                  size_offset_logical_calc(&zero, &result, Ne_Opr, &result);

                  if (THIS_IS_TRUE(result.constant, result.type_idx)) {
                     PRINTMSG(EQ_LINE_NUM(item), 527, Error,
                             EQ_COLUMN_NUM(item),
                             AT_OBJ_NAME_PTR(attr_idx));
                  }
               }
               else {
                  PRINTMSG(EQ_LINE_NUM(item), 527, Error,
                           EQ_COLUMN_NUM(item),
                           AT_OBJ_NAME_PTR(attr_idx));
               }
# elif defined(_INTEGER_1_AND_2)

               if (on_off_flags.integer_1_and_2) {

                  if (PACK_8_BIT_TEST_CONDITION(type_idx)) {
                     C_TO_F_INT(result.constant, 8, CG_INTEGER_DEFAULT_TYPE);
                     result.fld		= NO_Tbl_Idx;
                     result.type_idx	= CG_INTEGER_DEFAULT_TYPE;
                     left.fld		= EQ_OFFSET_FLD(item);
                     left.idx		= EQ_OFFSET_IDX(item);
                     zero.fld		= CN_Tbl_Idx;
                     zero.idx		= CN_INTEGER_ZERO_IDX;

                     size_offset_binary_calc(&left, &result, Mod_Opr, &result);
   
                     size_offset_logical_calc(&zero, &result, Ne_Opr, &result);

                     if (THIS_IS_TRUE(result.constant, result.type_idx)) {
                        PRINTMSG(EQ_LINE_NUM(item), 527, Error,
                                EQ_COLUMN_NUM(item),
                                AT_OBJ_NAME_PTR(attr_idx));
                     }
                  }
                  else if (PACK_16_BIT_TEST_CONDITION(type_idx)) {
                     C_TO_F_INT(result.constant, 16, CG_INTEGER_DEFAULT_TYPE);
                     result.fld		= NO_Tbl_Idx;
                     result.type_idx	= CG_INTEGER_DEFAULT_TYPE;
                     left.fld		= EQ_OFFSET_FLD(item);
                     left.idx		= EQ_OFFSET_IDX(item);
                     zero.fld		= CN_Tbl_Idx;
                     zero.idx		= CN_INTEGER_ZERO_IDX;
   
                     size_offset_binary_calc(&left, &result, Mod_Opr, &result);
   
                     size_offset_logical_calc(&zero, &result, Ne_Opr, &result);

                     if (THIS_IS_TRUE(result.constant, result.type_idx)) {
                        PRINTMSG(EQ_LINE_NUM(item), 527, Error,
                                EQ_COLUMN_NUM(item),
                                AT_OBJ_NAME_PTR(attr_idx));
                     }
                  }
                  else {
                     PRINTMSG(EQ_LINE_NUM(item), 527, Error,
                              EQ_COLUMN_NUM(item),
                              AT_OBJ_NAME_PTR(attr_idx));
                  }
               }
# else
               PRINTMSG(EQ_LINE_NUM(item), 527, Error,
                        EQ_COLUMN_NUM(item),
                        AT_OBJ_NAME_PTR(attr_idx));
# endif
            }
         }

# if defined(_TARGET_DOUBLE_ALIGN)

         if (EQ_DO_NOT_DALIGN(eq_idx)) {

            /* Intentionally left blank */
         }
         else if (DALIGN_TEST_CONDITION(type_idx)) {
            C_TO_F_INT(result.constant,
                       TARGET_BITS_PER_WORD * 2,
                       CG_INTEGER_DEFAULT_TYPE);
            result.fld		= NO_Tbl_Idx;
            result.type_idx	= CG_INTEGER_DEFAULT_TYPE;
            left.fld		= EQ_OFFSET_FLD(item);
            left.idx		= EQ_OFFSET_IDX(item);
            zero.fld		= CN_Tbl_Idx;
            zero.idx		= CN_INTEGER_ZERO_IDX;

            size_offset_binary_calc(&left, &result, Mod_Opr, &result);

            size_offset_logical_calc(&zero, &result, Ne_Opr, &result);

            if (THIS_IS_TRUE(result.constant, result.type_idx)) {

               /* If dalign_offset_ok, something else is in this equivalence */
               /* group that needs daligning and it is aligned.  This item   */
               /* needs to have 32 bit padding added to the start of the     */
               /* equivalence group to be daligned.  If we do this, then the */
               /* first item will get shifted off of a double word boundary. */
               /* If -a dalign is specified, issue an error message.  Other- */
               /* wise issue a caution message.                              */

               if (dalign_offset_ok) {
                  PRINTMSG(EQ_LINE_NUM(item), 1008, 
                           (cmd_line_flags.dalign) ? Error : Caution,
                           EQ_COLUMN_NUM(item), AT_OBJ_NAME_PTR(attr_idx));
               }
               else {
                  dalign_shift_offset	= TRUE;
                  EQ_DALIGN_ME(eq_idx)	= TRUE;
               }
            }
            else if (dalign_shift_offset) {
          
               /* If dalign_shift_offset, something else is in this equiv    */
               /* group that needs daligning.  This item needs a 32 bit pad  */
               /* added to the start of the equivalence group to be daligned.*/
               /* If we do this, then this item will get shifted off a       */
               /* double word boundary.  If -a dalign is specified, issue an */
               /* error message.  Otherwise issue a caution message.         */

               PRINTMSG(EQ_LINE_NUM(item), 1008, 
                        (cmd_line_flags.dalign) ? Error : Caution,
                        EQ_COLUMN_NUM(item), AT_OBJ_NAME_PTR(attr_idx));
            }
            else {
               dalign_offset_ok		= TRUE;
               EQ_DALIGN_ME(eq_idx)	= TRUE;
            }
         }
# endif

         ATD_OFFSET_FLD(attr_idx) = EQ_OFFSET_FLD(item);
         ATD_OFFSET_IDX(attr_idx) = EQ_OFFSET_IDX(item);
 
         if (!SB_IS_COMMON(sb_idx)) {

            if (!ATD_OFFSET_ASSIGNED(attr_idx)) {
               ATD_OFFSET_ASSIGNED(attr_idx) = TRUE;

               new_len = stor_bit_size_of(attr_idx, TRUE, FALSE);

               align_bit_length(&new_len, TARGET_BITS_PER_WORD);

               result.fld	= ATD_OFFSET_FLD(attr_idx);
               result.idx	= ATD_OFFSET_IDX(attr_idx);
               length.fld	= SB_LEN_FLD(sb_idx);
               length.idx	= SB_LEN_IDX(sb_idx);

               size_offset_binary_calc(&result, &new_len, Plus_Opr, &new_len);
               size_offset_logical_calc(&new_len, &length, Gt_Opr, &result);
   
               if (THIS_IS_TRUE(result.constant, result.type_idx)) {
                  SB_LEN_IDX(sb_idx) = ntr_const_tbl(new_len.type_idx,
                                                     FALSE,
                                                     new_len.constant);
               }
            }
         }
         item = EQ_NEXT_EQUIV_OBJ(item);
      }


# if defined(_TARGET_DOUBLE_ALIGN)

      /* Need to go through equivalence group again and make sure EQ_DALIGN */
      /* are set correctly on all members in the group.                     */

      if (dalign_shift_offset && !dalign_offset_ok) {

         /* If this is a common block, this storage block needs to shift */
         /* to be double aligned.  If this is not a common block, this   */
         /* block has been shifted by TARGET_BITS_PER_WORD so that       */
         /* everything is double aligned.  This is used if equivalence   */
         /* blocks are merged because of host association.               */
      
         EQ_DALIGN_SHIFT(eq_idx)	= TRUE;

         /* Adjust everything by 32 bits - unless this is a common block.*/
         /* If this is a common block and dalign is on, we will adjust.  */
         /* later.  If dalign is off, we will not adjust.  This is       */
         /* handled in allocate_common_storage.                          */

         if (!SB_IS_COMMON(sb_idx)) {
            item = eq_idx;

            while (item != NULL_IDX) {
               EQ_DALIGN_ME(item)	= TRUE;
               result.fld		= CN_Tbl_Idx;
               result.idx		= CN_INTEGER_BITS_PER_WORD_IDX;
               left.fld			= EQ_OFFSET_FLD(item);
               left.idx			= EQ_OFFSET_IDX(item);

               if (!size_offset_binary_calc(&left, &result, Plus_Opr, &result)){
                  AT_DCL_ERR(EQ_ATTR_IDX(item)) = TRUE;
               }

               if (result.fld == NO_Tbl_Idx) {
                  ATD_OFFSET_FLD(EQ_ATTR_IDX(item)) = CN_Tbl_Idx;
                  ATD_OFFSET_IDX(EQ_ATTR_IDX(item)) = ntr_const_tbl(
                                                          result.type_idx,
                                                          FALSE,
                                                          result.constant);
               }
               else {
                  ATD_OFFSET_FLD(EQ_ATTR_IDX(item)) = result.fld;
                  ATD_OFFSET_IDX(EQ_ATTR_IDX(item)) = result.idx;
               }
               item = EQ_NEXT_EQUIV_OBJ(item);
            }

            result.fld		= CN_Tbl_Idx;
            result.idx		= CN_INTEGER_BITS_PER_WORD_IDX;
            left.fld		= SB_LEN_FLD(sb_idx);
            left.idx		= SB_LEN_IDX(sb_idx);

            if (!size_offset_binary_calc(&left, &result, Plus_Opr, &result)) {
               AT_DCL_ERR(EQ_ATTR_IDX(item)) = TRUE;
            }

            if (result.fld == NO_Tbl_Idx) {
               SB_LEN_FLD(sb_idx) = CN_Tbl_Idx;
               SB_LEN_IDX(sb_idx) = ntr_const_tbl(result.type_idx,
                                                  FALSE,
                                                  result.constant);
            }
            else {
               SB_LEN_FLD(sb_idx) = result.fld;
               SB_LEN_IDX(sb_idx) = result.idx;
            }
         }
         else {
            item = eq_idx;

            while (item != NULL_IDX) {
               EQ_DALIGN_SHIFT(item)	= TRUE;
               EQ_DALIGN_ME(item)	= TRUE;
               item			= EQ_NEXT_EQUIV_OBJ(item);
            }
         }
      }
      else if (dalign_shift_offset || dalign_offset_ok) {
         item = eq_idx;

         while (item != NULL_IDX) {
            EQ_DALIGN_ME(item)	= TRUE;
            item		= EQ_NEXT_EQUIV_OBJ(item);
         }
      }
# endif

      group = EQ_NEXT_EQUIV_GRP(group);
   }

   TRACE (Func_Exit, "final_equivalence_semantics", NULL);

   return;

}  /* final_equivalence_semantics */

/******************************************************************************\
|*                                                                            *|
|* Description:                                                               *|
|*      This routine creates an integer temp that will tell which entry point *|
|*      you came in on.                                                       *|
|*      temp == 1 then main entry point. temp > 1 then, alternate entry.      *|
|*                                                                            *|
|* Input parameters:                                                          *|
|*      NONE                                                                  *|
|*                                                                            *|
|* Output parameters:                                                         *|
|*      NONE                                                                  *|
|*                                                                            *|
|* Returns:                                                                   *|
|*      NOTHING                                                               *|
|*                                                                            *|
\******************************************************************************/

void set_up_which_entry_tmp(void)

{

   int                  al_idx;
   int                  asg_idx;
   int                  attr_idx;
   int                  col;
   int                  i;
   int                  line;
   int                  save_curr_stmt_sh_idx;
   long_type            the_constant;
   int                  tmp_idx;


   TRACE (Func_Entry, "set_up_which_entry_tmp", NULL);

   if (SCP_WHICH_ENTRY_TMP(curr_scp_idx) == NULL_IDX) {

      save_curr_stmt_sh_idx = curr_stmt_sh_idx;

      /* get main pgm unit attr */

      attr_idx = SCP_ATTR_IDX(curr_scp_idx);
      line = SH_GLB_LINE(SCP_LAST_SH_IDX(curr_scp_idx));
      col  = SH_COL_NUM(SCP_LAST_SH_IDX(curr_scp_idx));

      /* create the index temp */

      tmp_idx = gen_compiler_tmp(line, col, Priv, TRUE);

      SCP_WHICH_ENTRY_TMP(curr_scp_idx) = tmp_idx;

      AT_SEMANTICS_DONE(tmp_idx)   = TRUE;
      ATD_TYPE_IDX(tmp_idx)        = CG_INTEGER_DEFAULT_TYPE;
      ATD_STOR_BLK_IDX(tmp_idx)    = SCP_SB_STACK_IDX(curr_scp_idx);

      /* gen assignment to index temp at entry */

      the_constant = 1;

      NTR_IR_TBL(asg_idx);
      IR_OPR(asg_idx)        = Asg_Opr;
      IR_TYPE_IDX(asg_idx)   = CG_INTEGER_DEFAULT_TYPE;
      IR_LINE_NUM(asg_idx)   = line;
      IR_COL_NUM(asg_idx)    = col;
      IR_FLD_L(asg_idx)      = AT_Tbl_Idx;
      IR_IDX_L(asg_idx)      = tmp_idx;
      IR_LINE_NUM_L(asg_idx) = line;
      IR_COL_NUM_L(asg_idx)  = col;
      IR_LINE_NUM_R(asg_idx) = line;
      IR_COL_NUM_R(asg_idx)  = col;
      IR_FLD_R(asg_idx)      = CN_Tbl_Idx;
      IR_IDX_R(asg_idx)      = CN_INTEGER_ONE_IDX;

      curr_stmt_sh_idx = SCP_FIRST_SH_IDX(curr_scp_idx);
      gen_sh(After, Assignment_Stmt, line, col, FALSE, FALSE, TRUE);
      SH_IR_IDX(curr_stmt_sh_idx) = asg_idx;
      SH_P2_SKIP_ME(curr_stmt_sh_idx) = TRUE;

      al_idx = SCP_ENTRY_IDX(curr_scp_idx);

      for (i = 0; i < SCP_ALT_ENTRY_CNT(curr_scp_idx); i++) {

         attr_idx = AL_ATTR_IDX(al_idx);

         the_constant++;

         /* gen assignment to index temp at entry */

         NTR_IR_TBL(asg_idx);
         IR_OPR(asg_idx)        = Asg_Opr;
         IR_TYPE_IDX(asg_idx)   = CG_INTEGER_DEFAULT_TYPE;
         IR_LINE_NUM(asg_idx)   = line;
         IR_COL_NUM(asg_idx)    = col;
         IR_FLD_L(asg_idx)      = AT_Tbl_Idx;
         IR_IDX_L(asg_idx)      = tmp_idx;
         IR_LINE_NUM_L(asg_idx) = line;
         IR_COL_NUM_L(asg_idx)  = col;
         IR_LINE_NUM_R(asg_idx) = line;
         IR_COL_NUM_R(asg_idx)  = col;
         IR_FLD_R(asg_idx)      = CN_Tbl_Idx;
         IR_IDX_R(asg_idx)      = C_INT_TO_CN(CG_INTEGER_DEFAULT_TYPE,
                                              the_constant);

         curr_stmt_sh_idx = ATP_FIRST_SH_IDX(attr_idx);
         gen_sh(After, Assignment_Stmt, line, col, FALSE, FALSE, TRUE);
         SH_IR_IDX(curr_stmt_sh_idx) = asg_idx;
         SH_P2_SKIP_ME(curr_stmt_sh_idx) = TRUE;

         al_idx = AL_NEXT_IDX(al_idx);
      }

      curr_stmt_sh_idx = save_curr_stmt_sh_idx;
   }


   TRACE (Func_Exit, "set_up_which_entry_tmp", NULL);

   return;

}  /* set_up_which_entry_tmp */

/******************************************************************************\
|*									      *|
|* Description:								      *|
|*	<description>							      *|
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

# if (defined(_TARGET_OS_IRIX) || defined(_TARGET_OS_LINUX) || defined(_TARGET_OS_DARWIN)) 
static void gen_user_code_start_opr(void)

{

   int		idx;
   int		ir_idx;
   int		save_curr_stmt_sh_idx;

   TRACE (Func_Entry, "gen_user_code_start_opr", NULL);

   save_curr_stmt_sh_idx = curr_stmt_sh_idx;
   curr_stmt_sh_idx = SCP_FIRST_SH_IDX(curr_scp_idx);

   NTR_IR_TBL(ir_idx);
   IR_OPR(ir_idx) = User_Code_Start_Opr;
   IR_TYPE_IDX(ir_idx) = TYPELESS_DEFAULT_TYPE;
   IR_LINE_NUM(ir_idx) = SH_GLB_LINE(curr_stmt_sh_idx);
   IR_COL_NUM(ir_idx) = 1;

   gen_sh(After, Directive_Stmt, SH_GLB_LINE(curr_stmt_sh_idx), 1, 
          FALSE, FALSE, TRUE);
   
   SH_P2_SKIP_ME(curr_stmt_sh_idx) = TRUE;
   SH_IR_IDX(curr_stmt_sh_idx) = ir_idx;
   
   idx = SCP_ENTRY_IDX(curr_scp_idx);

   while (idx) {
      curr_stmt_sh_idx = ATP_FIRST_SH_IDX(AL_ATTR_IDX(idx));

      NTR_IR_TBL(ir_idx);
      IR_OPR(ir_idx) = User_Code_Start_Opr;
      IR_TYPE_IDX(ir_idx) = TYPELESS_DEFAULT_TYPE;
      IR_LINE_NUM(ir_idx) = SH_GLB_LINE(curr_stmt_sh_idx);
      IR_COL_NUM(ir_idx) = 1;

      gen_sh(After, Directive_Stmt, SH_GLB_LINE(curr_stmt_sh_idx), 1, 
             FALSE, FALSE, TRUE);
   
      SH_P2_SKIP_ME(curr_stmt_sh_idx) = TRUE;
      SH_IR_IDX(curr_stmt_sh_idx) = ir_idx;

      idx = AL_NEXT_IDX(idx);
   }


   curr_stmt_sh_idx = save_curr_stmt_sh_idx;

   TRACE (Func_Exit, "gen_user_code_start_opr", NULL);

   return;

}  /* gen_user_code_start_opr */
# endif

/******************************************************************************\
|*									      *|
|* Description:								      *|
|*	<description>							      *|
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

# if (defined(_TARGET_OS_IRIX) || defined(_TARGET_OS_LINUX) || defined(_TARGET_OS_DARWIN))
static void insert_global_sh(void)

{
   int		gl_sh_idx;
   int		save_curr_stmt_sh_idx;
   int		sh_idx;

   TRACE (Func_Entry, "insert_global_sh", NULL);

   save_curr_stmt_sh_idx = curr_stmt_sh_idx;
   curr_stmt_sh_idx = SCP_FIRST_SH_IDX(curr_scp_idx);
   gl_sh_idx = global_stmt_sh_idx;

   while (gl_sh_idx) {

      sh_idx = copy_from_gl_subtree(gl_sh_idx, SH_Tbl_Idx);

      SH_NEXT_IDX(sh_idx) = SH_NEXT_IDX(curr_stmt_sh_idx);
      if (SH_NEXT_IDX(sh_idx) != NULL_IDX) {
         SH_PREV_IDX(SH_NEXT_IDX(sh_idx)) = sh_idx;
      }
      SH_PREV_IDX(sh_idx) = curr_stmt_sh_idx;
      SH_NEXT_IDX(curr_stmt_sh_idx) = sh_idx;
      curr_stmt_sh_idx = sh_idx;

      gl_sh_idx = GL_SH_NEXT_IDX(gl_sh_idx);
   }

   curr_stmt_sh_idx = save_curr_stmt_sh_idx;

   TRACE (Func_Exit, "insert_global_sh", NULL);

   return;

}  /* insert_global_sh */
# endif

#ifdef KEY /* Bug 14150 */
/*
 * def_ga_idx		Index into global_attr_tbl for definition
 * ref_bind_attr	True if reference has "bind" attribute
 * ref_binding_label	Value of reference binding label
 * ref_binding_label_len	Length of reference binding label
 * ref_line		Reference source line number
 * ref_column		Reference source column number
 * ref_name_ptr		Reference name
 * line_name		Line/column of other appearance
 */
static void
check_bind_attr(int def_ga_idx, int ref_bind_attr,
  const char *ref_binding_label, int ref_binding_label_len, int ref_line,
  int ref_column, const char *ref_name_ptr, const char *line_name) {
  if (GA_BIND_ATTR(def_ga_idx) ^ ref_bind_attr) {
     PRINTMSG(ref_line, 1624, Error, ref_column,
	      ref_name_ptr,
	      line_name,
	      "BIND");
  }
  else if (ref_bind_attr) {
    const char *ga_binding_label = GA_BINDING_LABEL(def_ga_idx);
    int ga_binding_label_len = strlen(ga_binding_label);
    if (ref_binding_label_len != ga_binding_label_len ||
      strncmp(ga_binding_label, ref_binding_label, ref_binding_label_len)) {
      PRINTMSG(ref_line, 1700, Error, ref_column, ref_name_ptr,
	ga_binding_label, line_name);
    }
  }
}
#endif /* KEY Bug 14150 */
/******************************************************************************\
|*									      *|
|* Description:								      *|
|*	Compares two global names to look for differences.   They can be      *|
|*	two definitions, two references or a reference and a definition.      *|
|*									      *|
|* Input parameters:							      *|
|*	def_ga_idx -> Global_attr tbl entry describing the definition.        *|
|*	              GA_REFERENCED and GA_DEFINED are used to decide if this *|
|*	              is a definition or a reference.  GA_DEFINED rules.      *|
|*	ref_ga_idx -> Global_attr tbl entry describing the reference.         *|
|*	list_idx   -> IR list table index describing local ref actual args    *|
|*	spec_idx   -> Global_attr tbl entry describing the reference.         *|
|*	              If ref_ga_idx is non-NULL then we use GA_DEFINED and    *|
|*	              GA_REFERENCED to decide if this is a definition or a    *|
|*	              reference.  If ref_ga_idx is NULL, then this is a ref.  *|
|*	attr_idx   -> Attr index describing a definition.                     *|
|*									      *|
|*	def_ga_idx is required.  ref_ga_idx, list_idx or attr_idx is required.*|
|*	It is incorrect to specify more than one of those three.              *|
|*									      *|
|* Output parameters:							      *|
|*	NONE								      *|
|*									      *|
|* Returns:								      *|
|*	NONE								      *|
|*									      *|
\******************************************************************************/

void	global_name_semantics(int	def_ga_idx,
			      int	ref_ga_idx,
			      int	list_idx,
			      int	spec_idx,
			      int	attr_idx)

{
   uint			act_file_line;
#ifdef KEY /* Bug 10177 */
   int			arg_attr_idx = 0;
#else /* KEY Bug 10177 */
   int			arg_attr_idx;
#endif /* KEY Bug 10177 */
   int			def_arg_idx;
   boolean		def_defined;
   int			gl_idx;
   int			i;
#ifdef KEY /* Bug 10177 */
   int			il_idx = 0;
   int			info_idx = 0;
#else /* KEY Bug 10177 */
   int			il_idx;
   int			info_idx;
#endif /* KEY Bug 10177 */
   char			line_name[256];
   msg_severities_type	msg_level;
   int			msg_num;
   boolean		need_expl_itrfc;
#ifdef KEY /* Bug 10177 */
   int			next_il_idx = 0;
#else /* KEY Bug 10177 */
   int			next_il_idx;
#endif /* KEY Bug 10177 */
   int			ref_arg_class;
   boolean		ref_arg_class_known;
   int			ref_arg_column;
#ifdef KEY /* Bug 10177 */
   int			ref_arg_idx = 0;
#else /* KEY Bug 10177 */
   int			ref_arg_idx;
#endif /* KEY Bug 10177 */
   int			ref_arg_line;
   char		       *ref_arg_name_ptr;
#ifdef KEY /* Bug 10177 */
   int			ref_array_elt = 0;
#else /* KEY Bug 10177 */
   int			ref_array_elt;
#endif /* KEY Bug 10177 */
   int			ref_column;
   boolean		ref_defined;
   boolean		ref_elemental;
#ifdef KEY /* Bug 14150 */
   boolean		ref_bind_attr = 0;
   const char *		ref_binding_label = 0;
   int			ref_binding_label_len = 0;
#endif /* KEY Bug 14150 */
   boolean		ref_global_dir;
#ifdef KEY /* Bug 10177 */
   int			ref_hollerith = 0;
#else /* KEY Bug 10177 */
   int			ref_hollerith;
#endif /* KEY Bug 10177 */
   boolean		ref_in_interface;
   int			ref_line;
#ifdef KEY /* Bug 10177 */
   int			ref_linear_type = 0;
#else /* KEY Bug 10177 */
   int			ref_linear_type;
#endif /* KEY Bug 10177 */
   char		       *ref_name_ptr;
   boolean		ref_nosideeffects;
   int			ref_num_dargs;
   int			ref_pgm_unit;
   boolean		ref_pure;
   int			ref_rank;
   boolean		ref_recursive;
   int			ref_rslt_idx;
#ifdef KEY /* Bug 10177 */
   int			ref_type = 0;
#else /* KEY Bug 10177 */
   int			ref_type;
#endif /* KEY Bug 10177 */
   boolean		ref_vfunction;
   boolean		same;
   int			type_idx;


   TRACE (Func_Entry, "global_name_semantics", NULL);

   need_expl_itrfc	= FALSE;
   def_defined		= GA_DEFINED(def_ga_idx);  /* ATP_EXPL_ITRFC */
   line_name[0]		= '\0';

   GLOBAL_LINE_TO_FILE_LINE(GA_DEF_LINE(def_ga_idx),
                            gl_idx,
                            act_file_line);
   sprintf(line_name, "%d (%s)", act_file_line, GL_FILE_NAME_PTR(gl_idx));

   if (ref_ga_idx != NULL_IDX) {

      /* We do not carry the extra argument in the global attr table. */

      ref_arg_idx		= GAP_FIRST_IDX(ref_ga_idx);
      ref_arg_idx--;

      ref_rslt_idx		= GAP_RSLT_IDX(ref_ga_idx);
      ref_pgm_unit		= GAP_PGM_UNIT(ref_ga_idx);
      ref_num_dargs		= GAP_NUM_DARGS(ref_ga_idx);
      ref_name_ptr		= GA_OBJ_NAME_PTR(ref_ga_idx);
      ref_defined		= GA_DEFINED(ref_ga_idx);
      ref_line			= GA_DEF_LINE(ref_ga_idx);
      ref_column		= GA_DEF_COLUMN(ref_ga_idx);
      ref_in_interface		= !GAP_PGM_UNIT_DEFINED(ref_ga_idx);
      ref_elemental		= GAP_ELEMENTAL(ref_ga_idx);
      ref_nosideeffects		= GAP_NOSIDE_EFFECTS(ref_ga_idx);
      ref_pure			= GAP_PURE(ref_ga_idx);
      ref_recursive		= GAP_RECURSIVE(ref_ga_idx);
      ref_vfunction		= GAP_VFUNCTION(ref_ga_idx);
      ref_global_dir		= GAP_GLOBAL_DIR(ref_ga_idx);
#ifdef KEY /* Bug 14150 */
      ref_bind_attr		= GA_BIND_ATTR(ref_ga_idx);
      ref_binding_label		= GA_BINDING_LABEL(ref_ga_idx);
#endif /* KEY Bug 14150 */

      if (ref_rslt_idx == NULL_IDX) {
         ref_rank	= 0;
      }
      else {
         ref_linear_type	= GT_LINEAR_TYPE(GAD_TYPE_IDX(ref_rslt_idx));
         ref_type		= GT_TYPE(GAD_TYPE_IDX(ref_rslt_idx));
         ref_rank		= GAD_RANK(ref_rslt_idx);

         if (ref_defined &&
            (GAD_POINTER(ref_rslt_idx) || ref_rank != 0 ||
#ifdef KEY /* Bug 14110 */
            GAD_VOLATILE(ref_rslt_idx) ||
#endif /* KEY Bug 14110 */
            (ref_type == Character && 
             GT_CHAR_CLASS(GAD_TYPE_IDX(ref_rslt_idx)) == Var_Len_Char))) {
            need_expl_itrfc	= TRUE;
         }
      }
   }
   else if (attr_idx != NULL_IDX) {
      ref_pgm_unit		= ATP_PGM_UNIT(attr_idx);
      ref_name_ptr		= AT_OBJ_NAME_PTR(attr_idx);
      ref_defined		= ATP_EXPL_ITRFC(attr_idx);
      ref_line			= AT_DEF_LINE(attr_idx);
      ref_column		= AT_DEF_COLUMN(attr_idx);
      ref_in_interface		= ATP_IN_INTERFACE_BLK(attr_idx);
      ref_elemental		= ATP_ELEMENTAL(attr_idx);
      ref_nosideeffects		= ATP_NOSIDE_EFFECTS(attr_idx);
      ref_pure			= ATP_PURE(attr_idx);
      ref_recursive		= ATP_RECURSIVE(attr_idx);
      ref_vfunction		= ATP_VFUNCTION(attr_idx);
      ref_global_dir		= FALSE;
#ifdef KEY /* Bug 14150 */
      ref_bind_attr		= AT_BIND_ATTR(attr_idx);
      ref_binding_label		= ATP_EXT_NAME_PTR(attr_idx);
      ref_binding_label_len	= ATP_EXT_NAME_LEN(attr_idx);
#endif /* KEY Bug 14150 */

      /* Skip past the extra argument if necessary. */

      if (ref_pgm_unit == Module) {
         ref_rslt_idx		= NULL_IDX;
         ref_num_dargs		= 0;
         ref_arg_idx		= NULL_IDX;
         ref_rank		= 0;
      }
      else {
         ref_rslt_idx		= ATP_RSLT_IDX(attr_idx);
         ref_num_dargs		= ATP_NUM_DARGS(attr_idx);
         ref_arg_idx		= ATP_FIRST_IDX(attr_idx);

         /* Set ref_arg_idx to one more than the number of dargs.  One */
         /* is subtracted from it at the start of the darg loop.       */

         if (ref_defined && ATP_EXTRA_DARG(attr_idx)) {
            ref_num_dargs--;
         }
         else {
            ref_arg_idx--;
         }

         if (ref_rslt_idx == NULL_IDX) {
            ref_rank		= 0;
            ref_linear_type	= Err_Res;
            ref_type		= Integer;  /* Default */
         }
         else {
            ref_linear_type	= TYP_LINEAR(ATD_TYPE_IDX(ref_rslt_idx));
            ref_type		= TYP_TYPE(ATD_TYPE_IDX(ref_rslt_idx));
            ref_rank		= (ATD_ARRAY_IDX(ref_rslt_idx) != NULL_IDX) ?
                                   BD_RANK(ATD_ARRAY_IDX(ref_rslt_idx)) : 0;

            if (ref_defined &&
                (ATD_POINTER(ref_rslt_idx) || ref_rank != 0 ||
                (ref_type == Character && 
                 TYP_CHAR_CLASS(ATD_TYPE_IDX(ref_rslt_idx)) == Var_Len_Char))) {
               need_expl_itrfc	= TRUE;
            }
         }
      }
   }
   else {
      next_il_idx		= list_idx;
      ref_pgm_unit		= ATP_PGM_UNIT(spec_idx);
      ref_rslt_idx		= ATP_RSLT_IDX(spec_idx);
      ref_num_dargs		= ATP_NUM_DARGS(spec_idx);
      ref_name_ptr		= AT_OBJ_NAME_PTR(spec_idx);
      ref_defined		= ATP_EXPL_ITRFC(spec_idx);
      ref_line			= stmt_start_line;
      ref_column		= stmt_start_col;
      ref_in_interface		= ATP_IN_INTERFACE_BLK(spec_idx);
      ref_elemental		= ATP_ELEMENTAL(spec_idx);
      ref_nosideeffects		= ATP_NOSIDE_EFFECTS(spec_idx);
      ref_pure			= ATP_PURE(spec_idx);
      ref_recursive		= ATP_RECURSIVE(spec_idx);
      ref_vfunction		= ATP_VFUNCTION(spec_idx);
      ref_global_dir		= FALSE;
#ifdef KEY /* Bug 14150 */
      ref_bind_attr		= AT_BIND_ATTR(spec_idx);
      ref_binding_label		= ATP_EXT_NAME_PTR(spec_idx);
      ref_binding_label_len	= ATP_EXT_NAME_LEN(spec_idx);
#endif /* KEY Bug 14150 */

      if (ref_defined && ATP_EXTRA_DARG(spec_idx)) {
         ref_num_dargs--;
      }

      if (ref_rslt_idx == NULL_IDX) {
         ref_rank	 = 0;
      }
      else {
         ref_linear_type = TYP_LINEAR(ATD_TYPE_IDX(ref_rslt_idx));
         ref_type	 = TYP_TYPE(ATD_TYPE_IDX(ref_rslt_idx));
         ref_rank	 = (ATD_ARRAY_IDX(ref_rslt_idx) != NULL_IDX) ?
                           	BD_RANK(ATD_ARRAY_IDX(ref_rslt_idx)) : 0;

         /* Skip past the extra argument if necessary. */

         if (next_il_idx != NULL_IDX &&
#ifdef KEY /* Bug 5089 */
             FUNCTION_MUST_BE_SUBROUTINE(spec_idx, ref_rslt_idx)
#else /* KEY Bug 5089 */
             FUNCTION_MUST_BE_SUBROUTINE(ref_rslt_idx)
#endif /* KEY Bug 5089 */
	     ) {
            next_il_idx	= IL_NEXT_LIST_IDX(next_il_idx);
         }
      }
   }

#ifdef KEY /* Bug 14150 */
   check_bind_attr(def_ga_idx, ref_bind_attr, ref_binding_label,
     ref_binding_label_len, ref_line, ref_column, ref_name_ptr, line_name);
#endif /* KEY Bug 14150 */

   if ((GAP_PGM_UNIT(def_ga_idx) != ref_pgm_unit) ||
       (GAP_PGM_UNIT_DEFINED(def_ga_idx) && ref_defined && !ref_in_interface)){

      if (ref_global_dir || GAP_GLOBAL_DIR(def_ga_idx)) {
         goto EXIT;  /* Specified in a global directive - only */
      }

      /* The two program units are not the same, as in one is a FUNCTION */
      /* and one is a SUBROUTINE, OR they both are the same, but there   */
      /* are two definitions.                                            */

# if defined(_ERROR_DUPLICATE_GLOBALS)
      msg_level = Error;
# else
      msg_level = (GAP_PGM_UNIT(def_ga_idx) == Module || 
                   ref_pgm_unit == Module) ? Error : Warning;
# endif
#ifdef KEY /* Bug 7405 */
      /* One of our customers wants to cheat and call a function as a 
       * subroutine, which is harmless.
       *
       * If explicitly defined as a function but referenced as a subroutine,
       * reduce error to a warning because we know it's harmless. If not
       * explicitly defined anywhere, and referenced sometimes as a function
       * but other times as a subroutine, reduce error to warning because we
       * can't tell.
       *
       * If explicitly defined as a subroutine but referenced as a function,
       * issue error because we know it's harmful.
       *
       * When reference precedes definition, the code leading to this point
       * uses def_ga_idx for the reference and ref_xxx for the definition,
       * so we need to test both combinations. Also note that F90-style
       * definitions (interface blocks, "contains") are checked elsewhere,
       * so loosening the checking here affects only F77-style code and
       * doesn't bypass any required F90 constraint checks. */
      pgm_unit_type def_pgm_unit = GAP_PGM_UNIT(def_ga_idx);
      if (def_defined) {
        if ((def_pgm_unit == Function) && (ref_pgm_unit == Subroutine)) {
	  msg_level = Warning;
	}
      }
      else if (ref_defined) {
        if ((ref_pgm_unit == Function) && (def_pgm_unit == Subroutine)) {
	  msg_level = Warning;
	}
      }
      else { /* Not explicitly defined */
        if (def_pgm_unit != ref_pgm_unit) {
	  msg_level = Warning;
	}
      }
#endif /* KEY Bug 7405 */

      if (def_defined) {
         msg_num = (ref_defined) ? 1282 : 1293;
      }
      else {
         msg_num = 1620;
      }

      PRINTMSG(ref_line, msg_num, msg_level, ref_column,
               ref_name_ptr,
               pgm_unit_str[GAP_PGM_UNIT(def_ga_idx)],
               line_name,
               pgm_unit_str[ref_pgm_unit]);
   
      /* If the program units are different, other checks make no sense. */

      goto EXIT;
   }

   if (!def_defined && !ref_defined) {  /* Two references */

      if (GAP_VFUNCTION(def_ga_idx) ^ ref_vfunction) {
         PRINTMSG(ref_line, 1625, Warning, ref_column,
                  ref_name_ptr,
                  line_name,
                  "VFUNCTION");
      }

      if (GAP_NOSIDE_EFFECTS(def_ga_idx) ^ ref_nosideeffects) {
         PRINTMSG(ref_line, 1625, Warning, ref_column,
                  ref_name_ptr,
                  line_name,
                  "NOSIDE EFFECTS");
      }

      /* Cannot check dargs or result types for two references. */
      /* These may be interlanguage calls.                      */

      goto EXIT;  
   }



   /* Check type and rank of the function result if Function */

   if (GAP_RSLT_IDX(def_ga_idx) != NULL_IDX && 
       ref_rslt_idx != NULL_IDX &&
       GAP_PGM_UNIT(def_ga_idx) == Function) {

      if (ref_ga_idx != NULL_IDX) {
         same		= compare_global_type_rank(GAP_RSLT_IDX(def_ga_idx),
                                                   GAP_RSLT_IDX(ref_ga_idx),
                                                   NULL_IDX,
                                                   NULL_IDX,
                                                   FALSE);
      }
      else if (attr_idx != NULL_IDX) {
         same		= compare_global_type_rank(GAP_RSLT_IDX(def_ga_idx),
                                                   NULL_IDX,
                                                   ATP_RSLT_IDX(attr_idx),
                                                   NULL_IDX,
                                                   FALSE);
      }
      else {
         same		= compare_global_type_rank(GAP_RSLT_IDX(def_ga_idx),
                                                   NULL_IDX,
                                                   ATP_RSLT_IDX(spec_idx),
                                                   NULL_IDX,
                                                   FALSE);
      }                                  

      if (!same) { 

         if (def_defined) {
            msg_level = Warning;

# if defined(_ERROR_DUPLICATE_GLOBALS)

            if (ref_defined) {
               msg_level = Error;
            }
# endif
            PRINTMSG(ref_line, 1294, msg_level, ref_column,
                     ref_name_ptr,
                     line_name,
                     GA_OBJ_NAME_PTR(GAP_RSLT_IDX(def_ga_idx)));
         }
         else {
            msg_num	= (ref_defined) ? 1618 : 1617;
            msg_level	= (msg_num == 1617) ? Caution : Warning;
            PRINTMSG(ref_line, msg_num, msg_level, ref_column,
                     ref_name_ptr,
                     line_name);
         }
      }
   }

   /* If list_idx is non-NULL, we do not have a number of dargs. */
   /* To get it, we need to count the number of list items.      */

   if (list_idx == NULL_IDX &&
       (ref_defined || def_defined) &&
       ref_num_dargs != GAP_NUM_DARGS(def_ga_idx)) {
      msg_level = Warning;

# if defined(_ERROR_DUPLICATE_GLOBALS)

      if (def_defined && ref_defined) {
         msg_level = Error;
      }
# endif
      PRINTMSG(ref_line, 1295, msg_level, ref_column,
               ref_name_ptr,
               line_name,
               GAP_NUM_DARGS(def_ga_idx),
               ref_num_dargs);
      goto EXIT;
   }

   /* Check ELEMENTAL, PURE, VFUNCTION, NOSIDE EFFECTS and RECURSIVE */

   if (ref_defined && def_defined) {

      if (GAP_ELEMENTAL(def_ga_idx) ^ ref_elemental) {
         PRINTMSG(ref_line, 1624, Warning, ref_column,
                  ref_name_ptr,
                  line_name,
                  "ELEMENTAL");
      }

#ifdef KEY /* Bug 14150 */
      /* May want to experiment with testing GAP_ELEMENTAL here, too, since
       * it seems not be detected on forward reference */
      if (GA_BIND_ATTR(def_ga_idx)) {
        need_expl_itrfc = TRUE;
      }
#endif /* KEY Bug 14150 */

      /* There is a rule in f95 before NOTE 12.4 that states that */
      /* the interface may specify a procedure that is not pure   */
      /* if the procedure is defined to be pure.                  */

      if (GAP_PURE(def_ga_idx) ^ ref_pure) {

         if (GAP_PURE(def_ga_idx) &&  ref_in_interface ||
             ref_pure && GAP_IN_INTERFACE_BLK(def_ga_idx)) {

            /* Intentionally blank */
         }
         else {
            PRINTMSG(ref_line, 1624, Warning, ref_column,
                     ref_name_ptr, 
                     line_name,
                     "PURE");
         }
      }

      if (GAP_RECURSIVE(def_ga_idx) ^ ref_recursive) {
         PRINTMSG(ref_line, 1624, Warning, ref_column,
                  ref_name_ptr,
                  line_name,
                  "RECURSIVE");
      }

   }

   def_arg_idx = GAP_FIRST_IDX(def_ga_idx);

   def_arg_idx--;        /* Set up so we can increment correctly */

   for (i = 0; i < GAP_NUM_DARGS(def_ga_idx); i++ ) {
      def_arg_idx++;

      if (ref_ga_idx != NULL_IDX) {
         ref_arg_idx++;
         ref_arg_line		= GA_DEF_LINE(ref_arg_idx);
         ref_arg_column		= GA_DEF_COLUMN(ref_arg_idx);
         ref_arg_name_ptr	= GA_OBJ_NAME_PTR(ref_arg_idx);
         ref_arg_class		= GA_OBJ_CLASS(ref_arg_idx);
         ref_arg_class_known	= TRUE;

         if (GA_OPTIONAL(ref_arg_idx)) {
            need_expl_itrfc	= TRUE;
         }

         if (ref_arg_class == Data_Obj) {
            ref_arg_class_known	= GAD_CLASS(ref_arg_idx) != Atd_Unknown;
            ref_rank		= GAD_RANK(ref_arg_idx);
            ref_array_elt	= GAD_ARRAY_ELEMENT_REF(ref_arg_idx);
            ref_linear_type	= GT_LINEAR_TYPE(GAD_TYPE_IDX(ref_arg_idx));
            ref_type		= GT_TYPE(GAD_TYPE_IDX(ref_arg_idx));
            ref_hollerith	= (GAD_CLASS(ref_ga_idx) == Constant) ?
                                  GAD_HOLLERITH(ref_ga_idx) : Not_Hollerith;

            if (GAD_POINTER(ref_arg_idx) || GAD_TARGET(ref_arg_idx) ||
#ifdef KEY /* Bug 14110 */
                GAD_VOLATILE(ref_arg_idx) ||
#endif /* KEY Bug 14110 */
                GAD_ASSUMED_SHAPE_ARRAY(ref_arg_idx)) {
               need_expl_itrfc	= TRUE;
            }
         }
      }
      else if (attr_idx != NULL_IDX) {
         ref_arg_idx++;
         arg_attr_idx		= SN_ATTR_IDX(ref_arg_idx);

         if (SN_LINE_NUM(ref_arg_idx) != 0) {
            ref_arg_line	= SN_LINE_NUM(ref_arg_idx);
            ref_arg_column	= SN_COLUMN_NUM(ref_arg_idx);
         }
         else {
            ref_arg_line	= AT_DEF_LINE(arg_attr_idx);
            ref_arg_column	= AT_DEF_COLUMN(arg_attr_idx);
         }
         ref_arg_name_ptr	= AT_OBJ_NAME_PTR(arg_attr_idx);
         ref_arg_class		= AT_OBJ_CLASS(arg_attr_idx);
         ref_arg_class_known	= TRUE;

         if (AT_OPTIONAL(arg_attr_idx)) {
            need_expl_itrfc	= TRUE;
         }

         if (ref_arg_class == Data_Obj) {
            ref_arg_class_known	= ATD_CLASS(arg_attr_idx) != Atd_Unknown;
            ref_rank		= (ATD_ARRAY_IDX(arg_attr_idx) == NULL_IDX) ?
                                       0 : BD_RANK(ATD_ARRAY_IDX(arg_attr_idx));
            ref_array_elt	= ATD_ARRAY_IDX(arg_attr_idx) == NULL_IDX;
            ref_linear_type	= TYP_LINEAR(ATD_TYPE_IDX(arg_attr_idx));
            ref_type		= TYP_TYPE(ATD_TYPE_IDX(arg_attr_idx));
            ref_hollerith	= (ATD_CLASS(arg_attr_idx) != Constant) ?
                                  Not_Hollerith :
                                 CN_HOLLERITH_TYPE(ATD_CONST_IDX(arg_attr_idx));

            if (ATD_POINTER(arg_attr_idx) || ATD_TARGET(arg_attr_idx) ||
#ifdef KEY /* Bug 14110 */
                ATD_VOLATILE(arg_attr_idx) ||
#endif /* KEY Bug 14110 */
                (ATD_ARRAY_IDX(arg_attr_idx) != NULL_IDX &&
                 BD_ARRAY_CLASS(ATD_ARRAY_IDX(arg_attr_idx)) == Assumed_Shape)){
               need_expl_itrfc	= TRUE;
            }
         }
      }
      else {
         il_idx			= next_il_idx;

         if (il_idx == NULL_IDX) {               /* Out of reference args */
            PRINTMSG(ref_line, 1295, Warning, ref_column,
                     ref_name_ptr,
                     line_name,
                     GAP_NUM_DARGS(def_ga_idx),
                     i+1);  /* Number of dargs */
            goto EXIT;
         }

         info_idx		= IL_ARG_DESC_IDX(il_idx);
         next_il_idx		= IL_NEXT_LIST_IDX(il_idx);
         ref_arg_line		= arg_info_list[info_idx].line;
         ref_arg_column		= arg_info_list[info_idx].col;

         if (IL_FLD(il_idx) == AT_Tbl_Idx) {
            ref_arg_name_ptr	= AT_OBJ_NAME_PTR(IL_IDX(il_idx));
            ref_arg_class	= AT_OBJ_CLASS(IL_IDX(il_idx));

            if (ref_arg_class == Data_Obj) {
               ref_arg_class_known = ATD_CLASS(IL_IDX(il_idx)) != Atd_Unknown;
            }
            else {
               ref_arg_class_known = TRUE;
            }
         }

         /* KAY - Another hole - what if this is a constant or an expression?*/
         else {
            ref_arg_name_ptr	= " ";
            ref_arg_class	= 0;
            ref_arg_class_known = arg_info_list[info_idx].pgm_unit;
         }

         if (!arg_info_list[info_idx].pgm_unit) {
            ref_rank		= arg_info_list[info_idx].ed.rank;
            ref_array_elt	= arg_info_list[info_idx].ed.array_elt;
            ref_linear_type	= arg_info_list[info_idx].ed.linear_type;
            ref_type		= arg_info_list[info_idx].ed.type;
            ref_hollerith	= (IL_FLD(list_idx) == CN_Tbl_Idx) ?
                                  CN_HOLLERITH_TYPE(IL_IDX(list_idx)) :
                                  Not_Hollerith;
         }
      }

      if (GA_OBJ_CLASS(def_arg_idx) == Data_Obj) {

         if (GA_COMPILER_GEND(def_arg_idx) &&
             GAD_CLASS(def_arg_idx) == Dummy_Argument) {  /* Alt return */

            if (ref_defined) {

               if (ref_arg_class != Data_Obj) {
                  PRINTMSG(ref_arg_line, 1296, Warning, ref_arg_column,
                           ref_name_ptr,
                           line_name,
                           i + 1);         /* darg number */
                  continue;
               }

               if ((attr_idx != NULL_IDX || spec_idx != NULL_IDX) &&
                    AT_COMPILER_GEND(ref_arg_idx) &&
                    ATD_CLASS(ref_arg_idx) == Dummy_Argument) {

                  /* Ok - intentionally blank */
               }
               else if (ref_ga_idx != NULL_IDX && 
                        GA_COMPILER_GEND(ref_arg_idx) &&
                        GAD_CLASS(ref_arg_idx) == Dummy_Argument) {

                  /* Ok - intentionally blank */
               }
               else {
                  PRINTMSG(ref_arg_line, 1296, Warning, ref_arg_column,
                           ref_name_ptr,
                           line_name,
                           i + 1);         /* darg number */
                  continue;
               }
            }
            else if (ref_arg_class != Label) {
               PRINTMSG(ref_arg_line, 1296, Warning, ref_arg_column,
                        ref_name_ptr,
                        line_name);
               continue;
            }

            continue;  /* No more checks for this darg */
         }

         if (GAD_CLASS(def_arg_idx) != Atd_Unknown &&
             ref_arg_class != Data_Obj) {

            /* Dummy is data object. Actual is procedure   */

            /* If it is unknown - there is not enough info */
            /* to decide if this is a Pgm_Unit or Data_Obj */

            if (def_defined) { /* Def is definition, other is ref or def */
               PRINTMSG(ref_arg_line, 1297, Caution, ref_arg_column,
                        ref_name_ptr,
                        line_name,
                        GA_OBJ_NAME_PTR(def_arg_idx));
            }
            else { /* Assume reference is defined */
               PRINTMSG(ref_arg_line, 1300, Caution, ref_arg_column,
                        ref_name_ptr,
                        line_name,
                        ref_arg_name_ptr);
            }
            continue;
         }

         if (!GAD_IGNORE_TKR(def_arg_idx) && ref_rank != GAD_RANK(def_arg_idx)){

             /* ranks are different */

            if (ref_rank == 0) {               /* The second is scalar */

               if ((!def_defined && GAD_ARRAY_ELEMENT_REF(def_arg_idx)) || 
                   ref_array_elt) {

                  /* If the first is a reference and the arg is an array  */
                  /* element reference then the second can be a scalar.   */
                  /* If the second is a reference and the arg is an array */
                  /* element reference, then the first can be an array.   */
               }
               else if (def_defined) {  /* First is an array. */
                  PRINTMSG(ref_arg_line, 1615, Warning, ref_arg_column,
                           ref_name_ptr,
                           line_name,
                           GA_OBJ_NAME_PTR(def_arg_idx));
                  continue;
               }
               else {
                  PRINTMSG(ref_arg_line, 1619, Caution, ref_arg_column,
                           ref_name_ptr,
                           line_name,
                           i+1);  /* Arg number */
                  continue;
               }
            }
            else if (GAD_RANK(def_arg_idx) == 0) {

               /* One is scalar, the second is an array */

               if ((!def_defined && GAD_ARRAY_ELEMENT_REF(def_arg_idx)) || 
                   ref_array_elt) {

                  /* If the first is a reference and the arg is an array  */
                  /* element reference then the second can be an array.   */
                  /* If the second is a reference and the arg is an array */
                  /* element reference, then the first can be a scalar.   */
               }
               else if (def_defined) { /* def/def or def/ref */
                  PRINTMSG(ref_arg_line, 1278, Warning, ref_arg_column,
                           ref_name_ptr,
                           line_name,
                           GA_OBJ_NAME_PTR(def_arg_idx));
               }
               else {
                  PRINTMSG(ref_arg_line, 1616, Caution, ref_arg_column,
                           ref_name_ptr,
                           line_name,
                           i+1);  /* Arg number */
               }
               continue;
            }
         }

         if (GAD_IGNORE_TKR(def_arg_idx)) {

            /* intentionally blank */
            /* This dummy arg will match any type, so skip */
            /* the type and kind type checking below.      */
         }
         else {
            type_idx	= GAD_TYPE_IDX(def_arg_idx);
            same	= TRUE;

            if (GT_TYPE(type_idx) == ref_type &&
                GT_LINEAR_TYPE(type_idx) == ref_linear_type) {

               if (GT_TYPE(type_idx) == Structure) {

                  if (ref_ga_idx != NULL_IDX) {
                     same = compare_global_derived_type(
                                    GT_STRUCT_IDX(type_idx),
                                    GT_STRUCT_IDX(GAD_TYPE_IDX(ref_arg_idx)),
                                    NULL_IDX);
                  }
                  else if (attr_idx != NULL_IDX) {
                     same = compare_global_derived_type(
                                           GT_STRUCT_IDX(type_idx),
                                           NULL_IDX,
                                           TYP_IDX(ATD_TYPE_IDX(arg_attr_idx)));
                  }
                  else {
                     same = compare_global_derived_type(
                                  GT_STRUCT_IDX(type_idx),
                                  NULL_IDX,
                                  TYP_IDX(arg_info_list[info_idx].ed.type_idx));
                  }
               }
            }
            else if (GT_TYPE(type_idx) == Character && ref_type == Character) {
               same = TRUE;
            }
            else if (!ref_defined && !def_defined) {

               /* Two references.  Compare both ways.  We can be the most */
               /* lenient with this type of comparison.                   */

               same = compare_global_args(GT_TYPE(type_idx),
                                          GT_LINEAR_TYPE(type_idx),
                                          ref_type,
                                          ref_linear_type,
                                          ref_hollerith);

               if (!same) {

                  /* This could be considered kludgy.  We compare this both */
                  /* ways rather than duplicating the code.  If either way  */
                  /* compares we consider it the same.                      */

                  same = compare_global_args(ref_type,
                                             ref_linear_type,
                                             GT_TYPE(type_idx),
                                             GT_LINEAR_TYPE(type_idx),
                                             GAD_CLASS(def_arg_idx) == Constant?
                                                 GAD_HOLLERITH(def_arg_idx):
                                                 Not_Hollerith);
               }
            }
            else if (ref_defined && def_defined) {

               /* Comparing two definitions - Can be most strict */

               same = FALSE;
            }
            else {  /* A reference and a definition */

               if (def_defined) {
                  same = compare_global_args(GT_TYPE(type_idx),
                                             GT_LINEAR_TYPE(type_idx),
                                             ref_type,
                                             ref_linear_type,
                                             ref_hollerith);
               }
               else { /* Ref is defined */
                  same = compare_global_args(ref_type,
                                             ref_linear_type,
                                             GT_TYPE(type_idx),
                                             GT_LINEAR_TYPE(type_idx),
                                             GAD_CLASS(def_arg_idx) == Constant?
                                                 GAD_HOLLERITH(def_arg_idx):
                                                 Not_Hollerith);
               }
            }

            if (!same) {

               if (def_defined) {
                  PRINTMSG(ref_arg_line, 1279, Warning, ref_arg_column,
                           ref_name_ptr,
                           line_name,
                           GA_OBJ_NAME_PTR(def_arg_idx));
               }
               else {
                  PRINTMSG(ref_arg_line, 1301, Caution, ref_arg_column,
                           ref_name_ptr,
                           line_name,
                           i+1);  /* Arg number */
               }
            }
         }
      }
      else if (GA_OBJ_CLASS(def_arg_idx) == Label) {
      }
      else if (GA_OBJ_CLASS(def_arg_idx) == Pgm_Unit) {

         if (ref_arg_class != Pgm_Unit && ref_arg_class_known) {

            if (def_defined) {
               PRINTMSG(ref_arg_line, 1660, Caution, ref_arg_column,
                        ref_name_ptr,
                        line_name,
                        GA_OBJ_NAME_PTR(def_arg_idx));
            }
            else {
               PRINTMSG(ref_arg_line, 1661, Caution, ref_arg_column,
                        ref_name_ptr,
                        line_name,
                        ref_arg_name_ptr);
            }
            continue;
         }

         if (ref_ga_idx != NULL_IDX) {
            ref_pgm_unit	= GAP_PGM_UNIT(ref_arg_idx);
         }
         else if (attr_idx != NULL_IDX) {
            ref_pgm_unit	= ATP_PGM_UNIT(arg_attr_idx);
         }
         else if (IL_FLD(il_idx) == AT_Tbl_Idx) {
            ref_pgm_unit	= ATP_PGM_UNIT(IL_IDX(il_idx));
         }
         else {   /* KAY - We should issue a message if this is an expr. */
            ref_pgm_unit	= Pgm_Unknown;
         }

         if (ref_pgm_unit == Function) {

            if (ref_ga_idx != NULL_IDX) {
               ref_rslt_idx	= GAP_RSLT_IDX(ref_arg_idx);
            }
            else if (attr_idx != NULL_IDX) {
               ref_rslt_idx	= ATP_RSLT_IDX(arg_attr_idx);
            }
            else {
               ref_rslt_idx	= ATP_RSLT_IDX(IL_IDX(il_idx));
            }

            if (GAP_PGM_UNIT(def_arg_idx) == Function) {

               if (ref_rslt_idx == NULL_IDX || 
                   GAP_RSLT_IDX(def_arg_idx) == NULL_IDX) {

                  /* One or both results missing - Intentionally blank */

               }
               else {

                  if (ref_ga_idx != NULL_IDX) {
                     same = compare_global_type_rank(GAP_RSLT_IDX(def_arg_idx),
                                                     ref_rslt_idx,
                                                     NULL_IDX,
                                                     NULL_IDX,
                                                     FALSE);
                  }
                  else { /* Attr_idx & list_idx are both set to an attr index */
                     same = compare_global_type_rank(GAP_RSLT_IDX(def_arg_idx),
                                                     NULL_IDX,
                                                     ref_rslt_idx,
                                                     NULL_IDX,
                                                     FALSE);
                  }

                  if (!same) {

                     if (def_defined) {
                        PRINTMSG(ref_arg_line, 1298, Warning, ref_arg_column,  
                                 ref_name_ptr,
                                 line_name,
                                 GA_OBJ_NAME_PTR(def_arg_idx),
                                 ref_arg_name_ptr);
                     }
                     else {
                        PRINTMSG(ref_arg_line, 1614, Caution, ref_arg_column,  
                                 ref_name_ptr,
                                 line_name,
                                 i + 1);  /* Arg number */
                     }
                  }
               }
            }
            else if (GAP_PGM_UNIT(def_arg_idx) != Pgm_Unknown) {
               PRINTMSG(ref_arg_line, 1299, Warning, ref_arg_column,
                        ref_name_ptr,
                        line_name,
                        pgm_unit_str[GAP_PGM_UNIT(def_arg_idx)],
                        GA_OBJ_NAME_PTR(def_arg_idx));
            }
         }
         else if (ref_pgm_unit == Subroutine) {

            if (GAP_PGM_UNIT(def_arg_idx) == Subroutine ||
                GAP_PGM_UNIT(def_arg_idx) == Pgm_Unknown) {

               /* Intentionally blank */
            }
            else {
               PRINTMSG(ref_arg_line, 1299, Warning, ref_arg_column,
                        ref_name_ptr,
                        line_name,
                        pgm_unit_str[GAP_PGM_UNIT(def_arg_idx)],
                        GA_OBJ_NAME_PTR(def_arg_idx));
            }
         }  /* else Pgm_Unknown should match. */
      }
   }  /* End for */

   if (list_idx != NULL_IDX && next_il_idx != NULL_IDX) {

      /* More reference args than definition dargs */

      il_idx = next_il_idx;

      while (il_idx != NULL_IDX) {
         i++;
         il_idx = IL_NEXT_LIST_IDX(il_idx);
      }
      PRINTMSG(ref_line, 1295, Warning, ref_column,
               ref_name_ptr,
               line_name,
               GAP_NUM_DARGS(def_ga_idx),
               i);  /* Number of dargs */
   }

   if (def_defined && ref_defined) {

      /* Intentionally blank */
   }
   else if (def_defined && GAP_NEEDS_EXPL_ITRFC(def_ga_idx)) {
       PRINTMSG(ref_line, 1277, Error, ref_column, 
                ref_name_ptr,
                "defined",
                line_name);
    }
    else if (need_expl_itrfc) {  /* Ref is defined */
       PRINTMSG(ref_line, 1277, Error, ref_column, 
                ref_name_ptr,
                "referenced",
                line_name);
    }


EXIT:

   TRACE (Func_Exit, "global_name_semantics", NULL);

   return;

}  /* global_name_semantics */

/******************************************************************************\
|*                                                                            *|
|* Description:                                                               *|
|*      compares a global and local attr for type, kind type, and rank.       *|
|*	This is used for global semantics.  One dummy argument is a local     *|
|*	attribute entry and one dummy argument is a global attribute entry.   *|
|*                                                                            *|
|* Input parameters:                                                          *|
|*                                                                            *|
|* Output parameters:                                                         *|
|*      NONE                                                                  *|
|*                                                                            *|
|* Returns:                                                                   *|
|*      TRUE is same in all three categories.                                 *|
|*      FALSE otherwise.                                                      *|
|*                                                                            *|
\******************************************************************************/
static	boolean  compare_global_type_rank(int		def_ga_idx,
					  int		ref_ga_idx,
					  int		attr_idx,
					  int		il_idx,
					  boolean	full_array_compare)
{
   int		array_idx;
   int		gt_idx;
   int		info_idx;
   int		ref_linear_type;
   int		ref_rank;
   int		ref_type;
   int		ref_type_idx;
   boolean	same;


   TRACE (Func_Entry, "compare_global_type_rank", NULL);

   /* One of the comparisons is always a global entry, but the second   */
   /* comparison can be a global entry, an attr table entry or from IR. */
   /* Gather the information that we need for checking.                 */

   if (il_idx != NULL_IDX) {
      info_idx		= IL_ARG_DESC_IDX(il_idx);
      ref_type_idx	= arg_info_list[info_idx].ed.type_idx;
      ref_linear_type	= arg_info_list[info_idx].ed.linear_type;
      ref_type		= arg_info_list[info_idx].ed.type;
      ref_rank		= arg_info_list[info_idx].ed.rank;
      array_idx		= NULL_IDX;    /* Do not have an array index. */
   }
   else if (attr_idx != NULL_IDX) {
      array_idx		= ATD_ARRAY_IDX(attr_idx);
      ref_rank		= array_idx != NULL_IDX ? BD_RANK(array_idx) : 0;
      ref_type_idx	= ATD_TYPE_IDX(attr_idx);
      ref_linear_type	= TYP_LINEAR(ref_type_idx);
      ref_type		= TYP_TYPE(ref_type_idx);
   }
   else {
      array_idx		= GAD_ARRAY_IDX(ref_ga_idx);
      ref_rank		= GAD_RANK(ref_ga_idx);
      ref_type_idx	= GAD_TYPE_IDX(ref_ga_idx);
      ref_linear_type	= GT_LINEAR_TYPE(ref_type_idx);
      ref_type		= GT_TYPE(ref_type_idx);
   }

   same		= TRUE;
   gt_idx	= GAD_TYPE_IDX(def_ga_idx);

   if (ref_rank != GAD_RANK(def_ga_idx) || ref_type != GT_TYPE(gt_idx)) {
      same = FALSE;
   }
   else if (ref_type == Structure) {

      if (il_idx != NULL_IDX || attr_idx != NULL_IDX) {
         same = compare_global_derived_type(GT_STRUCT_IDX(gt_idx),
                                            NULL_IDX,
                                            TYP_IDX(ref_type_idx));
      }
      else {
         same = compare_global_derived_type(GT_STRUCT_IDX(ref_type_idx), 
                                            GT_STRUCT_IDX(gt_idx),
                                            NULL_IDX);
      }
   }
   else if (ref_type != Character &&
            ref_linear_type != GT_LINEAR_TYPE(gt_idx)) {
      same = FALSE;
   }

   if (same && full_array_compare && array_idx != NULL_IDX) {

      if (attr_idx != NULL_IDX) {
         same = compare_global_array(GAD_ARRAY_IDX(def_ga_idx),
                                     NULL_IDX,
                                     array_idx);
      }
      else {
         same = compare_global_array(GAD_ARRAY_IDX(def_ga_idx),
                                     array_idx,
                                     NULL_IDX);
      }
   }

   TRACE (Func_Exit, "compare_global_type_rank", NULL);

   return(same);

}  /* compare_global_type_rank */

/******************************************************************************\
|*                                                                            *|
|* Description:                                                               *|
|*      Compare two derived types.  The first is always from the global       *|
|*      tables.  The second can be from the global or local attr table.       *|
|*                                                                            *|
|* Input parameters:                                                          *|
|*      ga_idx   ->  Index to a global derived type to be compared.           *|
|*      ga2_idx  ->  Index to a second global derived type to be compared.    *|
|*      attr_idx ->  Index to a local derived type to be compared.            *|
|*                                                                            *|
|* Output parameters:                                                         *|
|*      NONE                                                                  *|
|*                                                                            *|
|* Returns:                                                                   *|
|*      TRUE if they are the same, else FALSE.                                *|
|*                                                                            *|
\******************************************************************************/
static	boolean	compare_global_derived_type(int	ga_idx,
					    int	ga2_idx,
					    int	attr_idx)

{
#ifdef KEY /* Bug 10177 */
   int		 cpnt_idx = 0;
#else /* KEY Bug 10177 */
   int		 cpnt_idx;
#endif /* KEY Bug 10177 */
   int		 ga_cpnt_idx;
   int		 ga_type_idx;
   int		 ga_struct_idx;
   int		 len1;
   int		 len2;
   int		 mod_idx1;
   int		 mod_idx2;
   long		*name1;
   long		*name2;
   int		 num_cpnts;
   boolean	 same;
   boolean	 self_ptr;
#ifdef KEY /* Bug 10177 */
   int		 sn_idx = 0;
#else /* KEY Bug 10177 */
   int		 sn_idx;
#endif /* KEY Bug 10177 */
   int		 struct_idx;
   long_type	*the_constant;
   int		 the_type_idx;
   int		 type_idx;
   int		 type_linear;


   TRACE (Func_Entry, "compare_global_derived_type", NULL);

   if (attr_idx != NULL_IDX) {

      while (AT_ATTR_LINK(attr_idx) != NULL_IDX) {
         attr_idx = AT_ATTR_LINK(attr_idx);
      }

      /* Check to see if this is the same type and has */
      /* entered into the global type table already.   */

      if (GT_STRUCT_IDX(ATT_GLOBAL_TYPE_IDX(attr_idx)) == ga_idx) {
         same = TRUE;
         goto DONE;
      }

      if (AT_USE_ASSOCIATED(attr_idx)) {
         name2		= AT_ORIG_NAME_LONG(attr_idx);
         len2		= AT_ORIG_NAME_LEN(attr_idx);
         mod_idx2	= AT_MODULE_IDX(attr_idx);
      }
      else {
         name2		= AT_OBJ_NAME_LONG(attr_idx);
         len2		= AT_NAME_LEN(attr_idx);
         mod_idx2	= NULL_IDX;
      }
   }
   else {

      /* Check to see if this is the same type and has been entered. */

      if (ga2_idx == ga_idx) {
         same = TRUE;
         goto DONE;
      }
      if (GA_USE_ASSOCIATED(ga2_idx)) {
         name2		= GA_ORIG_NAME_LONG(ga2_idx);
         len2		= GA_ORIG_NAME_LEN(ga2_idx);
         mod_idx2	= GA_MODULE_IDX(ga2_idx);
      }
      else {
         name2		= GA_OBJ_NAME_LONG(ga2_idx);
         len2		= GA_NAME_LEN(ga2_idx);
         mod_idx2	= NULL_IDX;
      }
   }

   if (GA_USE_ASSOCIATED(ga_idx)) {
      name1	= GA_ORIG_NAME_LONG(ga_idx);
      len1	= GA_ORIG_NAME_LEN(ga_idx);
      mod_idx1	= GA_MODULE_IDX(ga_idx);
   }
   else {
      name1	= GA_OBJ_NAME_LONG(ga_idx);
      len1	= GA_NAME_LEN(ga_idx);
      mod_idx1	= NULL_IDX;
   }

   if (compare_names(name1, len1, name2, len2) != 0) {
      same = FALSE;
      goto DONE;
   }

   if (ga2_idx != NULL_IDX) {  /* Global to global comparison */

      if (mod_idx1 == mod_idx2 && mod_idx1 != NULL_IDX) {
         same = TRUE;
         goto DONE;
      }

      same = (!GAT_PRIVATE_CPNT(ga2_idx) && !GAT_PRIVATE_CPNT(ga_idx) &&
               GAT_SEQUENCE_SET(ga2_idx) && GAT_SEQUENCE_SET(ga_idx) &&
               GAT_NUM_CPNTS(ga2_idx) == GAT_NUM_CPNTS(ga_idx) &&
               compare_target_consts(GAT_STRUCT_BIT_LEN(ga2_idx),
                                     GAT_STRUCT_LIN_TYPE(ga2_idx),
                                     GAT_STRUCT_BIT_LEN(ga_idx),
                                     GAT_STRUCT_LIN_TYPE(ga_idx),
                                     Eq_Opr));

      cpnt_idx = GAT_FIRST_CPNT_IDX(ga2_idx);
   }
   else {

      if (mod_idx1 != NULL_IDX && mod_idx2 != NULL_IDX) {

         /* Both are from modules.  Check to see if this has been entered */
         /* into the global tables.  If not, these still may be the same. */

         if (ATP_GLOBAL_ATTR_IDX(mod_idx2) == mod_idx1) {
            same = TRUE;
            goto DONE;
         }

         name1	= GA_OBJ_NAME_LONG(mod_idx1);
         len1	= GA_NAME_LEN(mod_idx1);
         name2	= AT_OBJ_NAME_LONG(mod_idx2);
         len2	= AT_NAME_LEN(mod_idx2);

         if (compare_names(name1, len1, name2, len2) == 0) {

            /* They are from the same module */
            /* Shortcut - set the modules index to the global table. */

            ATP_GLOBAL_ATTR_IDX(mod_idx2) = mod_idx1;
            same = TRUE;
            goto DONE;
         }
      }

      same = (!ATT_PRIVATE_CPNT(attr_idx) && !GAT_PRIVATE_CPNT(ga_idx) &&
               ATT_SEQUENCE_SET(attr_idx) && GAT_SEQUENCE_SET(ga_idx) &&
               ATT_NUM_CPNTS(attr_idx) == GAT_NUM_CPNTS(ga_idx) &&
               compare_target_consts(
                     &CN_CONST(ATT_STRUCT_BIT_LEN_IDX(attr_idx)),
                      TYP_LINEAR(CN_TYPE_IDX(ATT_STRUCT_BIT_LEN_IDX(attr_idx))),
                      GAT_STRUCT_BIT_LEN(ga_idx),
                      GAT_STRUCT_LIN_TYPE(ga_idx),
                      Eq_Opr));
      sn_idx = ATT_FIRST_CPNT_IDX(attr_idx);
   }

   if (!same) goto DONE;

   ga_cpnt_idx	= GAT_FIRST_CPNT_IDX(ga_idx);
   num_cpnts	= GAT_NUM_CPNTS(ga_idx);

   while (num_cpnts > 0) {
      ga_type_idx	= GAD_TYPE_IDX(ga_cpnt_idx);

      if (ga2_idx == NULL_IDX) {
         cpnt_idx	= SN_ATTR_IDX(sn_idx);
         sn_idx		= SN_SIBLING_LINK(sn_idx);
         type_idx	= ATD_TYPE_IDX(cpnt_idx);
         type_linear	= TYP_LINEAR(type_idx);

         same = ATD_POINTER(cpnt_idx) == GAD_POINTER(ga_cpnt_idx) &&
                TYP_TYPE(type_idx) == GT_TYPE(ga_type_idx) &&
                (compare_names(AT_OBJ_NAME_LONG(cpnt_idx),
                               AT_NAME_LEN(cpnt_idx),
                               GA_OBJ_NAME_LONG(ga_cpnt_idx),
                               GA_NAME_LEN(ga_cpnt_idx)) == 0);

         same = same && compare_global_array(GAD_ARRAY_IDX(ga_cpnt_idx), 
                                             NULL_IDX,
                                             ATD_ARRAY_IDX(cpnt_idx));
      }
      else {
         type_idx	= GAD_TYPE_IDX(cpnt_idx);
         type_linear	= GT_LINEAR_TYPE(type_idx);

         same = GAD_POINTER(cpnt_idx) == GAD_POINTER(ga_cpnt_idx) &&
                GT_TYPE(type_idx) == GT_TYPE(ga_type_idx) &&
                (compare_names(GA_OBJ_NAME_LONG(cpnt_idx),
                               GA_NAME_LEN(cpnt_idx),
                               GA_OBJ_NAME_LONG(ga_cpnt_idx),
                               GA_NAME_LEN(ga_cpnt_idx)) == 0);

         same = same && compare_global_array(GAD_ARRAY_IDX(cpnt_idx), 
                                             GAD_ARRAY_IDX(ga_cpnt_idx),
                                             NULL_IDX);
      }

      if (!same) goto DONE;

      /* Components, so they must be constants */

      if (GT_TYPE(ga_type_idx) == Character) {

         if (ga2_idx == NULL_IDX) {
            the_constant = &CN_CONST(TYP_IDX(type_idx));
            the_type_idx = CN_TYPE_IDX(TYP_IDX(type_idx));
         }
         else {
            the_constant = GT_LENGTH(ga_type_idx);
            the_type_idx = GT_LENGTH_LIN_TYPE(ga_type_idx);
         }
         same = compare_target_consts(the_constant,
                                  the_type_idx,
                                  GT_LENGTH(GAD_TYPE_IDX(ga_cpnt_idx)),
                                  GT_LENGTH_LIN_TYPE(GAD_TYPE_IDX(ga_cpnt_idx)),
                                  Eq_Opr);
      }
      else if (GT_TYPE(ga_type_idx) == Structure) {

         if (ga2_idx == NULL_IDX) {
            struct_idx		= TYP_IDX(type_idx);
            ga_struct_idx	= NULL_IDX;
            self_ptr		= (struct_idx == attr_idx);
         }
         else {
            struct_idx		= NULL_IDX;
            ga_struct_idx	= GT_STRUCT_IDX(type_idx);
            self_ptr		= (struct_idx == ga2_idx);
         }
   
         if (GT_STRUCT_IDX(ga_type_idx) == ga_idx && self_ptr) {
   
            /* Pointers to self - intentionally blank.   Note:  ga2_idx */
            /* or attr_idx will be NULL.  They both cannot be set.      */
         }
         else if (( self_ptr && GT_STRUCT_IDX(ga_type_idx) != ga_idx) ||
                  (!self_ptr && GT_STRUCT_IDX(ga_type_idx) == ga_idx)) {
            same = FALSE;
            goto DONE;
         }
         else {
            same = compare_global_derived_type(GT_STRUCT_IDX(ga_type_idx),
                                               ga_struct_idx,
                                               struct_idx);
         }
      }
      else {
         same = (type_linear == GT_LINEAR_TYPE(ga_type_idx));
      }
      ga_cpnt_idx++;
      num_cpnts--;
   }  

DONE: 

   TRACE (Func_Exit, "compare_global_derived_type", NULL);

   return(same);

}  /* compare_global_derived_type */

/******************************************************************************\
|*                                                                            *|
|* Description:                                                               *|
|*      Compare two arrays, The first one is from the global bounds table,    *|
|*      The second one can be local or global.                                *|
|*                                                                            *|
|* Input parameters:                                                          *|
|*      gb_idx  -> Index to global bounds table entry to compare.             *|
|*      gb2_idx -> Index to another global bounds table entry to compare.     *|
|*      bd_idx  -> Index to local bounds table entry to compare.              *|
|*                                                                            *|
|* Output parameters:                                                         *|
|*      NONE                                                                  *|
|*                                                                            *|
|* Returns:                                                                   *|
|*      TRUE if they are the same, else FALSE.                                *|
|*                                                                            *|
\******************************************************************************/

static boolean compare_global_array(int	gb_idx,
				    int	gb2_idx,
				    int	bd_idx)
{
   int		dim;
   boolean 	same;


   TRACE (Func_Entry, "compare_global_array", NULL);

   if (gb2_idx != NULL_IDX) {  /* Global to global compare */

      if (gb2_idx == gb_idx) {
         same = TRUE;
      }
      else if (gb2_idx == NULL_IDX || gb_idx == NULL_IDX) {
         same = FALSE;  /* One is NULL and one is not NULL */
      }
      else {
         same = GB_RANK(gb2_idx) == GB_RANK(gb_idx) &&
                GB_ARRAY_CLASS(gb2_idx) == GB_ARRAY_CLASS(gb_idx) &&
                GB_ARRAY_SIZE(gb2_idx) == GB_ARRAY_SIZE(gb_idx);


         if (same && GB_ARRAY_CLASS(gb2_idx) == Explicit_Shape &&
                     GB_ARRAY_SIZE(gb2_idx) == Constant_Size) {

            for (dim = 1; dim <= GB_RANK(gb2_idx); dim++) {
               same = compare_target_consts(GB_LOWER_BOUND(gb_idx, dim),
                                     GT_LINEAR_TYPE(GB_LB_TYPE(gb_idx,dim)),
                                     GB_LOWER_BOUND(gb2_idx, dim),
                                     GT_LINEAR_TYPE(GB_LB_TYPE(gb2_idx,dim)),
                                     Eq_Opr) &&
                      compare_target_consts(GB_UPPER_BOUND(gb_idx, dim),
                                     GT_LINEAR_TYPE(GB_UB_TYPE(gb_idx,dim)),
                                     GB_UPPER_BOUND(gb2_idx, dim),
                                     GT_LINEAR_TYPE(GB_UB_TYPE(gb2_idx,dim)),
                                     Eq_Opr);
               if (!same) break;
            }
         }
      }
   }

   /* Global to local compare */

   else if (bd_idx == NULL_IDX || gb_idx == NULL_IDX) {
      same = (bd_idx == NULL_IDX && gb_idx == NULL_IDX);
   }
   else if (BD_GLOBAL_IDX(bd_idx) == gb_idx) {
      same = TRUE;
   }
   else {  /* Compare the header, but not the line and column numbers */
      same = BD_RANK(bd_idx) == GB_RANK(gb_idx) &&
             BD_ARRAY_CLASS(bd_idx) == GB_ARRAY_CLASS(gb_idx) &&
             BD_ARRAY_SIZE(bd_idx) == GB_ARRAY_SIZE(gb_idx);

      if (same && BD_ARRAY_CLASS(bd_idx) == Explicit_Shape &&
                  BD_ARRAY_SIZE(bd_idx) == Constant_Size) {

         for (dim = 1; dim <= BD_RANK(bd_idx); dim++) {
             same = compare_target_consts(&CN_CONST(BD_LB_IDX(bd_idx, dim)),
                                         CN_TYPE_IDX(BD_LB_IDX(bd_idx, dim)),
                                         GB_LOWER_BOUND(gb_idx, dim),
                                         GT_LINEAR_TYPE(GB_LB_TYPE(gb_idx,dim)),
                                         Eq_Opr) &&
                    compare_target_consts(&CN_CONST(BD_UB_IDX(bd_idx, dim)),
                                         CN_TYPE_IDX(BD_UB_IDX(bd_idx, dim)),
                                         GB_UPPER_BOUND(gb_idx, dim),
                                         GT_LINEAR_TYPE(GB_UB_TYPE(gb_idx,dim)),
                                         Eq_Opr);
            if (!same) break;
         }
      }
   }

   TRACE (Func_Exit, "compare_global_array", NULL);

   return(same);

}  /* compare_global_array */

/******************************************************************************\
|*                                                                            *|
|* Description:                                                               *|
|*      Compare the type of two args.  One is a def and one is a ref.         *|
|*      If you have two ref's.  Call this routine twice.  If one or the       *|
|*      other is the SAME then the two references are okay.                   *|
|*                                                                            *|
|* Input parameters:                                                          *|
|*	def_type		-> definition type 			      *|
|*	def_linear_type		-> definition linear type		      *|
|*	ref_type		-> reference type			      *|
|*	ref_linear_type		-> reference linear type		      *|
|*	ref_hollerith		-> reference hollerith info		      *|
|*	def_ga_struct_idx	-> definition structure index		      *|
|*	ref_ga_struct_idx	-> reference global structure index	      *|
|*	ref_at_struct_idx	-> reference attr structure index	      *|
|*	NOTE:  If the type is a structure, then either ref_ga_struct_idx or   *|
|*	       ref_at_struct_idx should be set, but not both.                 *|
|*                                                                            *|
|* Output parameters:                                                         *|
|*      NONE                                                                  *|
|*                                                                            *|
|* Returns:                                                                   *|
|*      TRUE if they are the same, else FALSE.                                *|
|*                                                                            *|
\******************************************************************************/

static boolean compare_global_args(int	def_type,
				   int	def_linear_type,
				   int	ref_type,
				   int	ref_linear_type,
				   int	ref_hollerith)
{
   boolean	same;

   TRACE (Func_Entry, "compare_global_args", NULL);

   if (ref_linear_type == Short_Typeless_Const &&
       (def_type == Integer || 
        def_type == Real || 
        def_type == Complex)) {
      same = TRUE;
   }
   else if (ref_type == Typeless && 
            (def_type == Integer || def_type == Real) &&
            num_host_wds[ref_linear_type] == num_host_wds[def_linear_type]) {
      same = TRUE;
   }
   else if (ref_linear_type == Short_Typeless_Const &&
            (ref_hollerith == H_Hollerith || 
             ref_hollerith == L_Hollerith) &&
             def_type == Character) {
      same = TRUE;
   }
   else if ((ref_type == Integer && def_type == CRI_Ptr) ||
            (ref_type == CRI_Ptr && def_type == Integer)) {
      same = TRUE;
   }
   else {
      same = FALSE;
   }

   TRACE (Func_Exit, "compare_global_args", NULL);

   return(same);

}  /* compare_global_args */
