/*
 * Copyright (C) 2010 Advanced Micro Devices, Inc.  All Rights Reserved.
 */
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



static char USMID[] = "\n@(#)5.0_pl/sources/p_directiv.c	5.12	10/12/99 10:54:10\n";

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
# include "s_globals.h"

# include "p_directiv.h"

/*****************************************************************\
|* function prototypes of static functions declared in this file *|
\*****************************************************************/

static void	check_do_open_mp_nesting(void);
static void	check_ordered_open_mp_nesting(void);
static boolean	check_section_open_mp_context(void);
static boolean	directive_region_error(directive_stmt_type, int, int);
static boolean	parse_assert_directive(void);
static void	parse_auxiliary_dir(void);
static void	parse_cache_align_name_list(opnd_type *);
static void	parse_cache_bypass_dir(opnd_type *);
static void	parse_cache_noalloc(void);
static void	parse_common_dirs(sb_type_type);
static void	parse_copy_assumed_shape_dir(void);
static void	parse_dir_directives(void);
static void	parse_dir_var_list(void);
static void	parse_distribution_dir(boolean);
static void	parse_doall_cmic(void);
static void	parse_dollar_directives(void);
static void	parse_doparallel_cmic(void);
static void	parse_fill_align_symbol(void);
static void	parse_id_directive(void);
static void	parse_ignore_tkr(void);
static void	parse_inline_always_never(boolean);
static void	parse_int_or_star_list(opnd_type *);
static void	parse_mic_directives(void);
static void	parse_mp_directive(mp_directive_type);
static void	parse_name_dir(void);
static void	parse_nosideeffects_dir(void);
static void	parse_par_directives(void);
static void	parse_parallel_cmic(void);
static void	parse_permutation_mic(void);
static void	parse_prefetch_ref(void);
static void	parse_redistribute_dir(void);
static void	parse_reference_list(opnd_type *);
static void	parse_sgi_dir_inline(boolean);
static void	parse_slash_common_dirs(void);
static void	parse_star_directives(void);
static void	parse_star_dir_directives(void);
static void	parse_symmetric_dir(void);
static void	parse_var_common_list(opnd_type *, boolean);
static boolean	parse_var_name_list(opnd_type *);
static void	parse_vfunction_dir(void);
static void	parse_open_mp_directives(void);
static void	parse_open_mp_clauses(open_mp_directive_type);
static int	update_fld_type(fld_type, int,int);


/******************************************************************************\
|*									      *|
|* Description:								      *|
|*	Set the defaults according to the command line arguments for all the  *|
|*      cdir switches. This is called for every new compile unit.             *|
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

void	init_directive(int	pass)

{
   int		attr_idx;
   int		list_idx1;
   int		list_idx2;
   int		type_idx;


   TRACE (Func_Entry, "init_directive", NULL);

   /* 0 means optimizer sets unroll count.  1 means no unrolling.  If  */
   /* the default level is set to 2, then automatic unrolling happens. */
   /* If the default level is set to 1, we only unroll those loops     */
   /* for which the user specifies the UNROLL directive.               */

   cdir_switches.unroll_count_idx = (opt_flags.unroll_lvl == Unroll_Lvl_2) ? 
                                     CN_INTEGER_ZERO_IDX : CN_INTEGER_ONE_IDX;
   cdir_switches.vector		  = (opt_flags.vector_lvl > Vector_Lvl_0); 
   cdir_switches.task		  = (opt_flags.task_lvl > Task_Lvl_0);

   cdir_switches.notask_region	  = FALSE;

   /* Inline1 means recognize directives only.  So cdir_switches.do_inline   */
   /* only gets specified if !DIR$ INLINE is see when level is inline_lvl_1. */

   cdir_switches.do_inline	= FALSE;
   cdir_switches.noinline	= FALSE;

   /* If split level is set to 2, automatic splitting happens.  If split */
   /* level is 1, then it can only be turned on with directives.         */

   cdir_switches.split		= (opt_flags.split_lvl == Split_Lvl_2); 

   cdir_switches.align			= FALSE;
   cdir_switches.bl			= opt_flags.bottom_load;
   cdir_switches.bounds			= cmd_line_flags.runtime_bounds;
   cdir_switches.concurrent		= FALSE;
   cdir_switches.ivdep			= FALSE;
   cdir_switches.mark			= opt_flags.mark;
   cdir_switches.stream			= (opt_flags.stream_lvl >=Stream_Lvl_1);
   cdir_switches.nextscalar		= FALSE;
   cdir_switches.no_internal_calls	= FALSE;
   cdir_switches.nointerchange		= opt_flags.nointerchange;
   cdir_switches.pattern		= opt_flags.pattern;
   cdir_switches.preferstream		= FALSE;
   cdir_switches.preferstream_nocinv	= FALSE;
   cdir_switches.prefertask		= FALSE;
   cdir_switches.prefervector		= FALSE;
   cdir_switches.recurrence		= opt_flags.recurrence;
   cdir_switches.shortloop		= FALSE;
   cdir_switches.shortloop128		= FALSE;
   cdir_switches.unroll_dir		= FALSE;
   cdir_switches.vsearch		= opt_flags.vsearch;

   /* If maxcpus TRUE, then there is an opnd hanging */
   /* off cdir_switches.maxcpu_opnd                  */

   cdir_switches.maxcpus		= FALSE;
   cdir_switches.parallel_region	= FALSE;
   cdir_switches.doall_region		= FALSE;
   cdir_switches.casedir		= FALSE;
   cdir_switches.guard			= FALSE;
   cdir_switches.guard_has_flag		= FALSE;
   cdir_switches.guard_in_par_reg	= FALSE;
   cdir_switches.do_parallel		= FALSE;
   cdir_switches.autoscope		= FALSE;
#ifdef KEY /* Bug 10441 */
   cdir_switches.single                 = FALSE;
#endif /* KEY Bug 10441 */
   cdir_switches.safevl_idx		= const_safevl_idx;
   cdir_switches.concurrent_idx		= NULL_IDX;
   cdir_switches.blockable_sh_idx	= NULL_IDX;
   cdir_switches.cache_bypass_ir_idx	= NULL_IDX;
   cdir_switches.doall_sh_idx		= NULL_IDX;
   cdir_switches.dir_nest_check_sh_idx	= NULL_IDX;
   cdir_switches.doacross_sh_idx	= NULL_IDX;
   cdir_switches.dopar_sh_idx		= NULL_IDX;
   cdir_switches.getfirst_list_idx	= NULL_IDX;
   cdir_switches.interchange_sh_idx	= NULL_IDX;
   cdir_switches.lastlocal_list_idx	= NULL_IDX;
   cdir_switches.lastthread_list_idx	= NULL_IDX;
   cdir_switches.mark_dir_idx		= NULL_IDX;
   cdir_switches.paralleldo_sh_idx	= NULL_IDX;
   cdir_switches.pdo_sh_idx		= NULL_IDX;
   cdir_switches.private_list_idx	= NULL_IDX;
   cdir_switches.reduction_list_idx	= NULL_IDX;
   cdir_switches.shared_list_idx	= NULL_IDX;

   cdir_switches.inline_here_sgi	= FALSE;
   cdir_switches.noinline_here_sgi	= FALSE;
   cdir_switches.inline_here_list_idx	= NULL_IDX;
   cdir_switches.noinline_here_list_idx = NULL_IDX;

   cdir_switches.firstprivate_list_idx  = NULL_IDX;
   cdir_switches.copyin_list_idx	= NULL_IDX;
   cdir_switches.copyprivate_list_idx = NULL_IDX; /* by jhs, 02/7/22 */
   cdir_switches.lastprivate_list_idx	= NULL_IDX;
   cdir_switches.default_scope_list_idx = NULL_IDX;
   cdir_switches.do_omp_sh_idx		= NULL_IDX;
   cdir_switches.paralleldo_omp_sh_idx	= NULL_IDX;

   cdir_switches.wait_list_idx		= NULL_IDX;
   cdir_switches.send_list_idx		= NULL_IDX;

   cdir_switches.blockable_count	= 0;
   cdir_switches.blockable_group	= 0;
   cdir_switches.interchange_count	= 0;
   cdir_switches.interchange_group	= 0;
   cdir_switches.interchange_level	= 0;

   if (pass > 1) {
      list_idx1 = cdir_switches.bounds_il_list;

      while (list_idx1) {
         attr_idx = IL_IDX(list_idx1);
         ATD_BOUNDS_CHECK(attr_idx) = FALSE;

         list_idx2 = list_idx1;
         list_idx1 = IL_NEXT_LIST_IDX(list_idx1);
         FREE_IR_LIST_NODE(list_idx2);
      }

      list_idx1 = cdir_switches.nobounds_il_list;

      while (list_idx1) {
         attr_idx = IL_IDX(list_idx1);
         ATD_NOBOUNDS_CHECK(attr_idx) = FALSE;

         list_idx2 = list_idx1;
         list_idx1 = IL_NEXT_LIST_IDX(list_idx1);
         FREE_IR_LIST_NODE(list_idx2);
      }
   }

   cdir_switches.bounds_il_list	   = NULL_IDX;
   cdir_switches.nobounds_il_list  = NULL_IDX;

   cdir_switches.mp_schedtype_opnd = null_opnd;

   if (global_schedtype_value >= 0) {
      OPND_LINE_NUM(cdir_switches.mp_schedtype_opnd) = global_schedtype_line;
      OPND_COL_NUM(cdir_switches.mp_schedtype_opnd) = global_schedtype_col;
      OPND_FLD(cdir_switches.mp_schedtype_opnd) = CN_Tbl_Idx;
      OPND_IDX(cdir_switches.mp_schedtype_opnd) = C_INT_TO_CN(
                                                        CG_INTEGER_DEFAULT_TYPE,
                                                        global_schedtype_value);
   }

   cdir_switches.chunk_opnd = null_opnd;
   cdir_switches.first_sh_blk_stk = null_opnd;

   directive_state = 0;

   if (pass == 1) {
      cdir_switches.implicit_use_idx	= cmd_line_flags.implicit_use_idx;
      cdir_switches.flow		= on_off_flags.flowtrace_option;
      cdir_switches.code		= FALSE;

      if (!opt_flags.set_allfastint_option &&
          !opt_flags.set_fastint_option &&
          !opt_flags.set_nofastint_option) {
# ifdef _TARGET_HAS_FAST_INTEGER
         opt_flags.set_fastint_option = TRUE;
# endif
      }

      if (opt_flags.mark && opt_flags.mark_name.string != NULL) {
         CLEAR_TBL_NTRY(type_tbl, TYP_WORK_IDX);
         TYP_CHAR_CLASS(TYP_WORK_IDX)	= Const_Len_Char;
         TYP_TYPE(TYP_WORK_IDX)		= Character;
         TYP_LINEAR(TYP_WORK_IDX)	= CHARACTER_DEFAULT_TYPE;
         TYP_FLD(TYP_WORK_IDX)		= CN_Tbl_Idx;
         TYP_IDX(TYP_WORK_IDX)	        = C_INT_TO_CN(CG_INTEGER_DEFAULT_TYPE,
                                            strlen(opt_flags.mark_name.string));
         type_idx			= ntr_type_tbl();

         cdir_switches.mark_cmdline_idx	= ntr_const_tbl(type_idx,
                                                        FALSE,
                                  (long_type *) &(opt_flags.mark_name.words));
      }
      else {
         cdir_switches.mark_cmdline_idx	= NULL_IDX;
      }
   }

   TRACE (Func_Exit, "init_directive", NULL);

   return;

}  /* init_directive */

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
void parse_directive_stmt (void)
{

   TRACE (Func_Entry, "parse_directive_stmt", NULL);

   /*****  NOTE:  THE INPUT STREAM TO THIS POINT IS "CDIR$", "CDIR@", *****/
   /*****  NOTE:  "!DIR$", "!DIR@", or "CMIC$", "!MIC$".  THE SCANNER *****/
   /*****  NOTE:  WILL RETURN Tok_Kwd_Dir FOR ANY OF THE ABOVE TOKENS.*****/
   /*****  NOTE:  THIS ROUTINE MUST EXAMINE THE TOKEN STRING FIELD    *****/
   /*****  NOTE:  TO DISTINGUISH BETWEEN COMPILER DIRECTIVES AND      *****/
   /*****  NOTE:  MICRO TASKING DIRECTIVES.  THE TOKEN STRING WILL    *****/
   /*****  NOTE:  NOT CONTAIN THE LEADING "C" OR "!" CHARACTERS.      *****/

#ifdef KEY /* Bug 4067 */
   if (cmd_line_flags.disregard_all_directives) {
       parse_err_flush(Find_EOS, NULL);
       NEXT_LA_CH;
       goto EXIT;
   }
#endif /* KEY Bug 4067 */

   /* If the first statement of the first program unit is being parsed, don't */
   /* buffer up any message pertaining to directives that precede this first  */
   /* statement (they don't belong to the program unit).                      */

   /* set need_new_sh to false for all the directives that don't need a */
   /* statement.                                                        */

   need_new_sh = FALSE;

   if (cif_need_unit_rec  &&  cif_first_pgm_unit) {
      c_i_f = cif_actual_file;
   }

   if (TOKEN_STR(token)[0] == 'M') {

      if (MATCHED_TOKEN_CLASS(Tok_Class_Mic_Kwd)) {

#        if defined(_ACCEPT_TASK)

# if (defined(_TARGET_OS_IRIX) || defined(_TARGET_OS_LINUX)) || defined(_TARGET_OS_DARWIN)
            if ((cdir_switches.task == FALSE || 
                 cmd_line_flags.disregard_all_mics) &&
                TOKEN_VALUE(token) != Tok_Mic_Cncall &&
                TOKEN_VALUE(token) != Tok_Mic_Permutation) {
# else
            if (cdir_switches.task == FALSE || 
                cmd_line_flags.disregard_all_mics) {
# endif
               parse_err_flush(Find_EOS, NULL);
               NEXT_LA_CH;
               goto EXIT;
            }
            parse_mic_directives();

#        else
            PRINTMSG(TOKEN_LINE(token), 801, Warning, TOKEN_COLUMN(token));
            parse_err_flush(Find_EOS, NULL);
            NEXT_LA_CH;
            goto EXIT;
#        endif
      }
      else {
         PRINTMSG(TOKEN_LINE(token), 1356, Warning, TOKEN_COLUMN(token));
         parse_err_flush(Find_EOS, NULL);
         NEXT_LA_CH;
         goto EXIT;
      }
   }
   else if (TOKEN_STR(token)[0] == '$' &&
            TOKEN_STR(token)[1] == 'O' &&
            TOKEN_STR(token)[2] == 'M' &&
            TOKEN_STR(token)[3] == 'P') {

      if (MATCHED_TOKEN_CLASS(Tok_Class_Open_Mp_Dir_Kwd)) {

# if defined(_TARGET_OS_MAX)
         PRINTMSG(TOKEN_LINE(token), 801, Warning, TOKEN_COLUMN(token));
         parse_err_flush(Find_EOS, NULL);
         NEXT_LA_CH;
         goto EXIT;
# else

         if (cmd_line_flags.disregard_all_omps) {

            /* Do not attempt to recognize any omp directives. */

            parse_err_flush(Find_EOS, NULL);
            NEXT_LA_CH;
            goto EXIT;
         }

         parse_open_mp_directives();
# endif
      }
      else {
         /* no error, just treat as comment */
         parse_err_flush(Find_EOS, NULL);
         NEXT_LA_CH;
         goto EXIT;
      }
   }
# if (defined(_TARGET_OS_IRIX) || defined(_TARGET_OS_LINUX)) || defined(_TARGET_OS_DARWIN)
   else if (TOKEN_STR(token)[0] == '$' &&
            TOKEN_STR(token)[1] == 'S' &&
            TOKEN_STR(token)[2] == 'G' &&
            TOKEN_STR(token)[3] == 'I') {

      if (MATCHED_TOKEN_CLASS(Tok_Class_Open_Mp_Dir_Kwd)) {
         parse_open_mp_directives();
      }
      else {
         /* no error, just treat as comment */
         parse_err_flush(Find_EOS, NULL);
         NEXT_LA_CH;
         goto EXIT;
      }
   }
# endif
   else if (TOKEN_STR(token)[0] == '$') {

      if (TOKEN_LEN(token) > 1 && TOKEN_STR(token)[1] == 'P') {

         if (MATCHED_TOKEN_CLASS(Tok_Class_SGI_Dir_Kwd)) {
            parse_par_directives();
         }
         else {
            /* no error, just treat as comment */
            parse_err_flush(Find_EOS, NULL);
            NEXT_LA_CH;
            goto EXIT;
         }
      }
      else if (MATCHED_TOKEN_CLASS(Tok_Class_SGI_Dir_Kwd)) {
         parse_dollar_directives();
      }
      else {
         /* no error, just treat as comment */
         parse_err_flush(Find_EOS, NULL);
         NEXT_LA_CH;
         goto EXIT;
      }
   }
   else if (TOKEN_STR(token)[0] == '*') {

      if (MATCHED_TOKEN_CLASS(Tok_Class_SGI_Dir_Kwd)) {
         parse_star_directives();
      }
      else {
         /* no error, just treat as comment */
         parse_err_flush(Find_EOS, NULL);
         NEXT_LA_CH;
         goto EXIT;
      }
   }

# ifdef _DEBUG

   else if (TOKEN_STR(token)[1] == 'B') {   /* !DBG */

      if (!MATCHED_TOKEN_CLASS(Tok_Class_Dbg_Kwd)) {
         parse_err_flush(Find_EOS, NULL);
         NEXT_LA_CH;
         goto EXIT;
      }
      parse_dir_directives();
   }

# endif

   else if (MATCHED_TOKEN_CLASS(Tok_Class_Dir_Kwd)) {

      if (cmd_line_flags.disregard_all_dirs) {

         /* Do not attempt to recognize any dir$ directives. */

         parse_err_flush(Find_EOS, NULL);
         NEXT_LA_CH;
         goto EXIT;
      }
      parse_dir_directives();
   }
   else {
      PRINTMSG(TOKEN_LINE(token), 1356, Warning, TOKEN_COLUMN(token));
      parse_err_flush(Find_EOS, NULL);
      NEXT_LA_CH;
      goto EXIT;
   }

EXIT:

   if (cif_need_unit_rec  &&  cif_first_pgm_unit) {
      c_i_f = cif_tmp_file;
   }
   
   TRACE (Func_Exit, "parse_directive_stmt", NULL);

   return;

}  /* parse_directive_stmt */

/******************************************************************************\
|*									      *|
|* Description:								      *|
|*	Generate the IR for directives.                                       *|
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

int	gen_directive_ir(operator_type	operator)

{
   int		ir_idx;
//Bug# 1204
#ifdef KEY
   int          tmp_ir_idx;
#endif


   TRACE (Func_Entry, "gen_directive_ir", NULL);

   need_new_sh = TRUE;

   if (SH_IR_IDX(curr_stmt_sh_idx)) {
#ifdef KEY
      tmp_ir_idx = SH_NEXT_IDX(curr_stmt_sh_idx);
#endif
#ifdef KEY /* Bug 7498 */
      /* ntr_sh_tbl() may change the value of sh_tbl, which is used inside
       * SH_NEXT_IDX(), so we need an ANSI C sequence point in between. */
      int new_stmt                              = ntr_sh_tbl();
      SH_NEXT_IDX(curr_stmt_sh_idx)		= new_stmt;
#else /* KEY Bug 7498 */
      SH_NEXT_IDX(curr_stmt_sh_idx)		= ntr_sh_tbl();
#endif /* KEY Bug 7498 */
      SH_PREV_IDX(SH_NEXT_IDX(curr_stmt_sh_idx))= curr_stmt_sh_idx;
      curr_stmt_sh_idx				= SH_NEXT_IDX(curr_stmt_sh_idx);
      SH_STMT_TYPE(curr_stmt_sh_idx)		= Directive_Stmt;
#ifdef KEY
      SH_NEXT_IDX(curr_stmt_sh_idx) = tmp_ir_idx;
      SH_PREV_IDX(tmp_ir_idx) = curr_stmt_sh_idx;
#endif
   }

   SH_GLB_LINE(curr_stmt_sh_idx)= TOKEN_LINE(token);
   SH_COL_NUM(curr_stmt_sh_idx)	= TOKEN_COLUMN(token);

   NTR_IR_TBL(ir_idx);
   IR_OPR(ir_idx)		= operator;

   /* must have a type idx */

   IR_TYPE_IDX(ir_idx)		= TYPELESS_DEFAULT_TYPE;
   IR_LINE_NUM(ir_idx)		= TOKEN_LINE(token);
   IR_COL_NUM(ir_idx)		= TOKEN_COLUMN(token);

   SH_IR_IDX(curr_stmt_sh_idx)	= ir_idx;

   TRACE (Func_Exit, "gen_directive_ir", NULL);

   return(ir_idx);

}  /* gen_directive_ir */


/******************************************************************************\
|*                                                                            *|
|* Description:                                                               *|
|*      This routine parses the CDIR$ COPY_ASSUMED_SHAPE line.                *|
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

static void parse_copy_assumed_shape_dir(void)

{
   int          attr_idx;
   int          head_list_idx = NULL_IDX;
   int          list_idx;
   int          name_idx;


   TRACE (Func_Entry, "parse_copy_assumed_shape_dir", NULL);

   do {
      if (MATCHED_TOKEN_CLASS(Tok_Class_Id)) {
         attr_idx = srch_sym_tbl(TOKEN_STR(token), TOKEN_LEN(token),
                                 &name_idx);

         if (attr_idx == NULL_IDX) {
            attr_idx                         = ntr_sym_tbl(&token, name_idx);
            LN_DEF_LOC(name_idx)             = TRUE;
            AT_OBJ_CLASS(attr_idx)	     = Data_Obj;
            SET_IMPL_TYPE(attr_idx);
         }
         else if (fnd_semantic_err(Obj_Copy_Assumed_Shape,
                                   TOKEN_LINE(token),
                                   TOKEN_COLUMN(token),
                                   attr_idx,
                                   TRUE)) {
            goto NEXT;
         }

         if (AT_REFERENCED(attr_idx) == Char_Rslt_Bound_Ref) {
            AT_ATTR_LINK(attr_idx)   = NULL_IDX;
            LN_DEF_LOC(name_idx)     = TRUE;
         }

         ATD_COPY_ASSUMED_SHAPE(attr_idx) = TRUE;

         if (head_list_idx == NULL_IDX) {

            /* place on the head list on scope */

            NTR_IR_LIST_TBL(head_list_idx);

            IL_NEXT_LIST_IDX(head_list_idx)=SCP_COPY_ASSUMED_LIST(curr_scp_idx);

            if (IL_NEXT_LIST_IDX(head_list_idx) != NULL_IDX) {
               IL_PREV_LIST_IDX(IL_NEXT_LIST_IDX(head_list_idx))=head_list_idx;
            }

            SCP_COPY_ASSUMED_LIST(curr_scp_idx) = head_list_idx;

            IL_FLD(head_list_idx)	= IL_Tbl_Idx;
            IL_LIST_CNT(head_list_idx)	= 0;
         }

         NTR_IR_LIST_TBL(list_idx);
         IL_NEXT_LIST_IDX(list_idx)	= IL_IDX(head_list_idx);

         if (IL_NEXT_LIST_IDX(list_idx) != NULL_IDX) {
            IL_PREV_LIST_IDX(IL_NEXT_LIST_IDX(list_idx)) = list_idx;
         }

         IL_LIST_CNT(head_list_idx)++;
         IL_IDX(head_list_idx)	= list_idx;
         IL_FLD(list_idx)	= AT_Tbl_Idx;
         IL_IDX(list_idx)	= attr_idx;
         IL_LINE_NUM(list_idx)	= TOKEN_LINE(token);
         IL_COL_NUM(list_idx)	= TOKEN_COLUMN(token);
      }
      else if (!parse_err_flush(Find_Comma, "variable name")) {
         break;                 /* Couldn't recover.  Hit EOS */
      }

NEXT:

      if (LA_CH_VALUE == COMMA) {
         NEXT_LA_CH;
      }
      else if (LA_CH_VALUE == EOS ||
               !parse_err_flush(Find_Comma, ", or "EOS_STR)) {
         break;
      }
      else {  /* Issued error and recovered at a comma */
         NEXT_LA_CH;
      }
   }
   while (TRUE);

   NEXT_LA_CH;          /* Pick up EOS */

   TRACE (Func_Exit, "parse_copy_assumed_shape_dir", NULL);

   return;

}  /* parse_copy_assumed_shape_dir */

/******************************************************************************\
|*                                                                            *|
|* Description:                                                               *|
|*      This routine parses the !DIR$ IGNORE_TKR directive.		      *|
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

static void parse_ignore_tkr(void)

{
   int          attr_idx;
   int          name_idx;


   TRACE (Func_Entry, "parse_ignore_tkr", NULL);

   do {
      if (MATCHED_TOKEN_CLASS(Tok_Class_Id)) {
         attr_idx = srch_sym_tbl(TOKEN_STR(token),
                                 TOKEN_LEN(token),
                                &name_idx);

         if (attr_idx == NULL_IDX) {
            attr_idx			= ntr_sym_tbl(&token, name_idx);
            LN_DEF_LOC(name_idx)	= TRUE;
            AT_OBJ_CLASS(attr_idx)	= Data_Obj;
            ATD_CLASS(attr_idx)		= Dummy_Argument;
            ATD_IGNORE_TKR(attr_idx)	= TRUE;
            SET_IMPL_TYPE(attr_idx);
         }
         else if (!fnd_semantic_err(Obj_Ignore_TKR,
                                    TOKEN_LINE(token),
                                    TOKEN_COLUMN(token),
                                    attr_idx,
                                    TRUE)) {

            if (AT_REFERENCED(attr_idx) == Char_Rslt_Bound_Ref) {
               AT_ATTR_LINK(attr_idx)   = NULL_IDX;
               LN_DEF_LOC(name_idx)     = TRUE;
            }

#ifdef KEY /* Bug 14150 */
	 /* Now that we allow ignore_tkr on a dummy argument, don't set class
	  * if we already know it */
         if (AT_OBJ_CLASS(attr_idx) == Pgm_Unit) {
	 }
	 else
#endif /* KEY Bug 14150 */
            ATD_CLASS(attr_idx)		= Dummy_Argument;
            ATD_IGNORE_TKR(attr_idx)	= TRUE;
         }
      }
      else if (!parse_err_flush(Find_Comma, "dummy-argument name")) {
         break;                 /* Couldn't recover.  Hit EOS */
      }

      if (LA_CH_VALUE == COMMA) {
         NEXT_LA_CH;
      }
      else if (LA_CH_VALUE == EOS ||
               !parse_err_flush(Find_Comma, ", or "EOS_STR)) {
         break;
      }
      else {  /* Issued error and recovered at a comma */
         NEXT_LA_CH;
      }
   }
   while (TRUE);

   NEXT_LA_CH;          /* Pick up EOS */

   TRACE (Func_Exit, "parse_ignore_tkr", NULL);

   return;

}  /* parse_ignore_tkr */

/******************************************************************************\
|*									      *|
|* Description:								      *|
|*	This routine parses the CDIR$ AUXILIARY line.                         *|
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

static void parse_auxiliary_dir(void)

{
   int		attr_idx;
   int		name_idx;
   int		sb_idx;


   TRACE (Func_Entry, "parse_auxiliary_dir", NULL);

   do {
      if (MATCHED_TOKEN_CLASS(Tok_Class_Id)) {
         attr_idx = srch_sym_tbl(TOKEN_STR(token), TOKEN_LEN(token),
                                 &name_idx);

         if (attr_idx == NULL_IDX) {
            attr_idx			= ntr_sym_tbl(&token, name_idx);
            LN_DEF_LOC(name_idx)	= TRUE;
            ATD_AUXILIARY(attr_idx)	= TRUE;
            SET_IMPL_TYPE(attr_idx);
         }
         else if (!fnd_semantic_err(Obj_Auxiliary, 
                                    TOKEN_LINE(token),
                                    TOKEN_COLUMN(token),
                                    attr_idx,
                                    TRUE)) {

            if (AT_REFERENCED(attr_idx) == Char_Rslt_Bound_Ref) {
               AT_ATTR_LINK(attr_idx)	= NULL_IDX;
               LN_DEF_LOC(name_idx)	= TRUE;
            }

            ATD_AUXILIARY(attr_idx)	= TRUE;

            if (ATD_IN_COMMON(attr_idx)) {
               sb_idx	= ATD_STOR_BLK_IDX(attr_idx);

               if (SB_BLANK_COMMON(sb_idx)) {
                  PRINTMSG(TOKEN_LINE(token), 534, Error,
                           TOKEN_COLUMN(token),
                           AT_OBJ_NAME_PTR(attr_idx));
               }
               else if (SB_BLK_TYPE(sb_idx) == Task_Common) {
                  PRINTMSG(TOKEN_LINE(token), 537, Error,
                           TOKEN_COLUMN(token),
                           AT_OBJ_NAME_PTR(attr_idx),
                           SB_NAME_PTR(sb_idx));
               }
               else {
                  SB_AUXILIARY(sb_idx) = TRUE;
               }
            }
         }
      }
      else if (!parse_err_flush(Find_Comma, "variable name")) {
         break;			/* Couldn't recover.  Hit EOS */
      }

      if (LA_CH_VALUE == COMMA) {
         NEXT_LA_CH;
      }
      else if (LA_CH_VALUE == EOS ||
               !parse_err_flush(Find_Comma, ", or "EOS_STR)) {
         break;
      }
      else {  /* Issued error and recovered at a comma */
         NEXT_LA_CH;
      }
   }
   while (TRUE);

   NEXT_LA_CH;		/* Pick up EOS */

   TRACE (Func_Exit, "parse_auxiliary_dir", NULL);

   return;

}  /* parse_auxiliary_dir */

/******************************************************************************\
|*									      *|
|* Description:								      *|
|*	This routine parses the CDIR$ CACHE_BYPASS line.                      *|
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
static void parse_cache_bypass_dir(opnd_type	*opnd)

{
   int		column;
   int		line;
   int		list_idx	= NULL_IDX;
   opnd_type	opnd2;


   TRACE (Func_Entry, "parse_cache_bypass_dir", NULL);

   do {
      if (MATCHED_TOKEN_CLASS(Tok_Class_Id)) {

         if (!parse_deref(&opnd2, NULL_IDX)) {
            parse_err_flush(Find_Comma, NULL);
         }
         else {
            find_opnd_line_and_column(&opnd2, &line, &column);

            if (OPND_FLD(opnd2) != AT_Tbl_Idx) {
               PRINTMSG(line, 1319, Error, column);
            }
            else {

               if (list_idx == NULL_IDX) {
                  NTR_IR_LIST_TBL(list_idx);
                  COPY_OPND(IL_OPND(list_idx), opnd2);
                  OPND_FLD((*opnd))		= IL_Tbl_Idx;
                  OPND_IDX((*opnd))		= list_idx;
                  OPND_LIST_CNT((*opnd))	= 1;
               }
               else {
                  NTR_IR_LIST_TBL(IL_NEXT_LIST_IDX(list_idx));
                  IL_PREV_LIST_IDX(IL_NEXT_LIST_IDX(list_idx)) = list_idx;
                  (OPND_LIST_CNT((*opnd)))++;
                  list_idx			= IL_NEXT_LIST_IDX(list_idx);
                  COPY_OPND(IL_OPND(list_idx), opnd2);
               }
            }
         }
      }
      else if (!parse_err_flush(Find_Comma, "array name")) {
         break;			/* Couldn't recover.  Hit EOS */
      }

      if (LA_CH_VALUE == COMMA) {
         NEXT_LA_CH;
      }
      else if (LA_CH_VALUE == EOS ||
               !parse_err_flush(Find_Comma, ", or "EOS_STR)) {
         break;
      }
      else {  /* Issued error and recovered at a comma */
         NEXT_LA_CH;
      }
   }
   while (TRUE);

   NEXT_LA_CH;		/* Pick up EOS */

   TRACE (Func_Exit, "parse_cache_bypass_dir", NULL);

   return;

}  /* parse_cache_bypass_dir */

/******************************************************************************\
|*									      *|
|* Description:								      *|
|*	This routine parses the CDIR$ NO SIDE EFFECTS line.                   *|
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
static void parse_nosideeffects_dir(void)

{
   int		attr_idx;
   int		name_idx;


   TRACE (Func_Entry, "parse_nosideeffects_dir", NULL);

   do {
      if (MATCHED_TOKEN_CLASS(Tok_Class_Id)) {
         attr_idx = srch_sym_tbl(TOKEN_STR(token), TOKEN_LEN(token),
                                 &name_idx);

         if (attr_idx == NULL_IDX) {
            attr_idx			= ntr_sym_tbl(&token, name_idx);
            LN_DEF_LOC(name_idx)	= TRUE;
            AT_OBJ_CLASS(attr_idx)	= Pgm_Unit;
            ATP_NOSIDE_EFFECTS(attr_idx)= TRUE;
            MAKE_EXTERNAL_NAME(attr_idx,
                               AT_NAME_IDX(attr_idx),
                               AT_NAME_LEN(attr_idx));
            ATP_PROC(attr_idx)		= Extern_Proc;
            ATP_SCP_IDX(attr_idx)	= curr_scp_idx;
         }
         else if (!fnd_semantic_err(Obj_No_Side_Effects,
                                    TOKEN_LINE(token),
                                    TOKEN_COLUMN(token),
                                    attr_idx,
                                    TRUE)) {

            if (AT_REFERENCED(attr_idx) == Char_Rslt_Bound_Ref) {
               AT_ATTR_LINK(attr_idx)	= NULL_IDX;
               LN_DEF_LOC(name_idx)	= TRUE;
            }

            if (AT_OBJ_CLASS(attr_idx) == Data_Obj) { 

               if (ATD_CLASS(attr_idx) == Function_Result) {
                  attr_idx	= ATD_FUNC_IDX(attr_idx);
               }
               else {
                  chg_data_obj_to_pgm_unit(attr_idx, 
                                           Pgm_Unknown,
                                           Extern_Proc);
               }
            }
            ATP_NOSIDE_EFFECTS(attr_idx)= TRUE;
         }
      }
      else if (!parse_err_flush(Find_Comma, "procedure name")) {
         break;			/* Couldn't recover.  Hit EOS */
      }

      if (LA_CH_VALUE == COMMA) {
         NEXT_LA_CH;
      }
      else if (LA_CH_VALUE == EOS ||
               !parse_err_flush(Find_Comma, ", or "EOS_STR)) {
         break;
      }
      else {  /* Issued error and recovered at a comma */
         NEXT_LA_CH;
      }
   }
   while (TRUE);

   NEXT_LA_CH;		/* Pick up EOS */

   TRACE (Func_Exit, "parse_nosideeffects_dir", NULL);

   return;

}  /* parse_nosideeffects_dir */

/******************************************************************************\
|*									      *|
|* Description:								      *|
|*	This routine parses the CDIR$ VFUNCTION line.                         *|
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
static void parse_vfunction_dir(void)

{
   int		attr_idx;
   int		name_idx;
   int		rslt_idx;


   TRACE (Func_Entry, "parse_vfunction_dir", NULL);

   /* In cft77, vfunction acts just like it was an EXTERNAL statement.  */
   /* This implementation does the same thing.  Vfunctions may not be   */
   /* specified for internal or module procedures.                      */

   do {
      if (MATCHED_TOKEN_CLASS(Tok_Class_Id)) {
         attr_idx = srch_sym_tbl(TOKEN_STR(token), TOKEN_LEN(token),
                                 &name_idx);

         if (attr_idx == NULL_IDX) {
            attr_idx			= ntr_sym_tbl(&token, name_idx);
            LN_DEF_LOC(name_idx)	= TRUE;
            AT_OBJ_CLASS(attr_idx)	= Pgm_Unit;
            ATP_NOSIDE_EFFECTS(attr_idx)= TRUE;
            MAKE_EXTERNAL_NAME(attr_idx,
                               AT_NAME_IDX(attr_idx),
                               AT_NAME_LEN(attr_idx));
            ATP_PROC(attr_idx)		= Extern_Proc;
            ATP_PGM_UNIT(attr_idx)	= Function;
            ATP_VFUNCTION(attr_idx)	= TRUE;
            ATP_SCP_IDX(attr_idx)	= curr_scp_idx;

            CREATE_FUNC_RSLT(attr_idx, rslt_idx);
            SET_IMPL_TYPE(rslt_idx);
         }
         else if (!fnd_semantic_err(Obj_Vfunction,
                                    TOKEN_LINE(token),
                                    TOKEN_COLUMN(token),
                                    attr_idx,
                                    TRUE)) {

            if (AT_REFERENCED(attr_idx) == Char_Rslt_Bound_Ref) {
               AT_ATTR_LINK(attr_idx)	= NULL_IDX;
               LN_DEF_LOC(name_idx)	= TRUE;
            }

            if (AT_OBJ_CLASS(attr_idx) == Data_Obj) { /* Switch to Function*/
               chg_data_obj_to_pgm_unit(attr_idx,
                                        Function,
                                        Extern_Proc);
               ATP_PGM_UNIT(attr_idx)	= Function;
               ATP_VFUNCTION(attr_idx)	= TRUE;
            }
            else if (AT_OBJ_CLASS(attr_idx) == Pgm_Unit) {

               if (ATP_PGM_UNIT(attr_idx) != Function) {
                  ATP_PGM_UNIT(attr_idx)	= Function;
                  CREATE_FUNC_RSLT(attr_idx, rslt_idx);
                  SET_IMPL_TYPE(rslt_idx);
               }
               ATP_PROC(attr_idx)	= Extern_Proc;
               ATP_VFUNCTION(attr_idx)	= TRUE;
            }
         }
      }
      else if (!parse_err_flush(Find_Comma, "procedure name")) {
         break;			/* Couldn't recover.  Hit EOS */
      }

      if (LA_CH_VALUE == COMMA) {
         NEXT_LA_CH;
      }
      else if (LA_CH_VALUE == EOS ||
               !parse_err_flush(Find_Comma, ", or "EOS_STR)) {
         break;
      }
      else {  /* Issued error and recovered at a comma */
         NEXT_LA_CH;
      }
   }
   while (TRUE);

   NEXT_LA_CH;		/* Pick up EOS */

   TRACE (Func_Exit, "parse_vfunction_dir", NULL);

   return;

}  /* parse_vfunction_dir */

/******************************************************************************\
|*									      *|
|* Description:								      *|
|*	This routine parses !DIR$ TASKCOMMON, !DIR$ COMMON and                *|
|*	!$OMP THREADPRIVATE                                                   *|
|*									      *|
|* Input parameters:							      *|
|*	Common means this is specified with the common directive.             *|
|*	Task_Common means this is specified with the task common directive.   *|
|*									      *|
|* Output parameters:							      *|
|*	NONE								      *|
|*									      *|
|* Returns:								      *|
|*	NOTHING								      *|
|*									      *|
\******************************************************************************/
static void parse_common_dirs(sb_type_type	blk_type)

{
   int		new_sb_idx;
   int		sb_idx;


   TRACE (Func_Entry, "parse_common_dirs", NULL);

   do {
      if (MATCHED_TOKEN_CLASS(Tok_Class_Id)) {
         sb_idx = srch_stor_blk_tbl(TOKEN_STR(token),
                                    TOKEN_LEN(token),
                                    curr_scp_idx);

         if (sb_idx == NULL_IDX) {
            sb_idx = ntr_stor_blk_tbl(TOKEN_STR(token),
                                      TOKEN_LEN(token),
                                      TOKEN_LINE(token),
                                      TOKEN_COLUMN(token),
                                      blk_type);

            SB_COMMON_NEEDS_OFFSET(sb_idx)	= TRUE;
         }
         else if (SB_BLK_TYPE(sb_idx) == Threadprivate) {
            PRINTMSG(TOKEN_LINE(token), 1486, Error, TOKEN_COLUMN(token),
                     SB_NAME_PTR(sb_idx));
         }
         else if (SB_USE_ASSOCIATED(sb_idx) || SB_HOST_ASSOCIATED(sb_idx)) {

            /* Common block has been use or host associated into this scope. */
            /* Make an entry for this block and hide the associated block    */
            /* storage_blk_resolution will resolve the blocks.               */

            new_sb_idx = ntr_stor_blk_tbl(TOKEN_STR(token),
                                          TOKEN_LEN(token),
                                          TOKEN_LINE(token),
                                          TOKEN_COLUMN(token),
                                          blk_type);

            SB_COMMON_NEEDS_OFFSET(new_sb_idx)	= TRUE;
            SB_MERGED_BLK_IDX(sb_idx)		= new_sb_idx;
            SB_HIDDEN(sb_idx)			= TRUE;
            SB_DEF_MULT_SCPS(sb_idx)		= TRUE;
            sb_idx                      	= new_sb_idx;
         }
         else {
            SB_BLK_TYPE(sb_idx)			= blk_type;
            SB_RUNTIME_INIT(stor_blk_tbl_idx)	= FALSE;
         }

         /* Else block has been declared already.  Mark block type */

         SB_IS_COMMON(sb_idx)		= TRUE;

         if (blk_type == Common) {
            SB_DCL_COMMON_DIR(sb_idx)	= TRUE;
         }
      }
      else if (LA_CH_VALUE == SLASH) {
         NEXT_LA_CH;

         if (LA_CH_VALUE == SLASH) {
            PRINTMSG(TOKEN_LINE(token), 1481, Error, TOKEN_COLUMN(token),
                     TOKEN_STR(token), blk_type == Common ? "COMMON" :
                                                            "TASK COMMON");
            NEXT_LA_CH;
         }
         else {
            parse_err_flush(Find_Comma, "common block name");
         }
      }
      else if (!parse_err_flush(Find_Comma, "common block name")) {
         break;			/* Couldn't recover.  Hit EOS */
      }

      if (LA_CH_VALUE == COMMA) {
         NEXT_LA_CH;
      }
      else if (LA_CH_VALUE == EOS ||
               !parse_err_flush(Find_Comma, ", or "EOS_STR)) {
         break;
      }
      else {  /* Issued error and recovered at a comma */
         NEXT_LA_CH;
      }
   }
   while (TRUE);

   NEXT_LA_CH;		/* Pick up EOS */

   TRACE (Func_Exit, "parse_common_dirs", NULL);

   return;

}  /* parse_common_dirs */

/******************************************************************************\
|*									      *|
|* Description:								      *|
|*	This routine parses !$OMP THREADPRIVATE                               *|
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
static void parse_slash_common_dirs(void)

{
   int		sb_idx;
   int		attr_idx;
   int		name_idx;
   token_values_type	token_value;


   TRACE (Func_Entry, "parse_slash_common_dirs", NULL);

   if (LA_CH_VALUE != LPAREN) {
      parse_err_flush(Find_EOS, "(/common-block-name/)");
      return;
   }

   NEXT_LA_CH;  /* eat ( */

   do {

      if (LA_CH_VALUE == SLASH) {        /* must be common block */
         NEXT_LA_CH;    /* eat slash */

         if (MATCHED_TOKEN_CLASS(Tok_Class_Id)) {
            sb_idx = srch_stor_blk_tbl(TOKEN_STR(token),
                                       TOKEN_LEN(token),
                                       curr_scp_idx);

            if (sb_idx == NULL_IDX) {
               sb_idx = ntr_stor_blk_tbl(TOKEN_STR(token),
                                         TOKEN_LEN(token),
                                         TOKEN_LINE(token),
                                         TOKEN_COLUMN(token),
                                         Threadprivate);

               SB_COMMON_NEEDS_OFFSET(sb_idx)	= TRUE;
	       SB_IS_COMMON(sb_idx)	= TRUE;
            }
            else if (SB_USE_ASSOCIATED(sb_idx)) {

               if (SB_BLK_TYPE(sb_idx) != Threadprivate) {
                  PRINTMSG(TOKEN_LINE(token), 1485, Error, TOKEN_COLUMN(token),
                           SB_NAME_PTR(sb_idx));
               }
            }
            else if (SB_HOST_ASSOCIATED(sb_idx)) {
               PRINTMSG(TOKEN_LINE(token), 1485, Error, TOKEN_COLUMN(token),
                        SB_NAME_PTR(sb_idx));
            }

            if (SB_BLK_TYPE(sb_idx) != Common &&
                SB_BLK_TYPE(sb_idx) != Threadprivate) {

               /* Must be a common block - not taskcommon or auxiliary */

               PRINTMSG(TOKEN_LINE(token), 1486, Error, TOKEN_COLUMN(token),
                        SB_NAME_PTR(sb_idx));
            }
            else { /* Else block has been declared already.  Mark block type */
               SB_BLK_TYPE(sb_idx)	= Threadprivate;
               SB_RUNTIME_INIT(sb_idx)	= FALSE;
               SB_IS_COMMON(sb_idx)	= TRUE;

               /* Mark elements of the common block as thread private.
                */
               attr_idx	= SB_FIRST_ATTR_IDX(sb_idx);

               while (attr_idx != NULL_IDX) {
                  ATD_TASK_PRIVATE(attr_idx) = TRUE;
                  attr_idx	= ATD_NEXT_MEMBER_IDX(attr_idx);
               }
            }

            if (LA_CH_VALUE == SLASH) {
               NEXT_LA_CH;   /* eat slash */
            }
            else if (!parse_err_flush(Find_Comma_Slash, "/")) {
               break;
            }
            else if (LA_CH_VALUE == SLASH) {
               NEXT_LA_CH;
            }
         }
         else if (LA_CH_VALUE == SLASH) {
            NEXT_LA_CH;
            PRINTMSG(TOKEN_LINE(token), 1481, Error, TOKEN_COLUMN(token),
                     TOKEN_STR(token), "THREADPRIVATE");
         }
         else if (!parse_err_flush(Find_Comma_Rparen, "common-block-name")) {
            break;
         }
      }
      /* the following is added by jhs, 02.9.9 */
      else if(MATCHED_TOKEN_CLASS(Tok_Class_Id)){
	 token_value = TOKEN_VALUE(token);
	 attr_idx = srch_sym_tbl(TOKEN_STR(token),
			    TOKEN_LEN(token),
		    	    &name_idx);
	 if(attr_idx == NULL_IDX){
	    attr_idx = ntr_sym_tbl(&token, name_idx);
	    LN_DEF_LOC(name_idx) = FALSE;
	    AT_OBJ_CLASS(attr_idx) = Data_Obj;
	    SET_IMPL_TYPE(attr_idx);
	 }
	 /* Note that symbols in blocks are now marked to be thread private
	  * since it is possible that some symbols can be referenced as thread
	  * private and some as global.
	  */
	 ATD_TASK_PRIVATE(attr_idx) = TRUE;
#ifdef KEY
         if (ATD_STOR_BLK_IDX(attr_idx) == NULL_IDX) {
            assign_storage_blk(attr_idx);
         }
         SB_BLK_TYPE(ATD_STOR_BLK_IDX(attr_idx)) = Threadprivate;
#endif
#ifdef KEY /* Bug 9029 */
	 /* The current function (parse_slash_common_dirs) is called only if
	  * the token is Tok_Open_Mp_Dir_Threadprivate, so none of the
	  * following code is relevant, and calling fnd_semantic_err with
	  * Obj_Section_Non_Gp causes spurious errors when the identifier is
	  * a module variable or when it is allocatable. Probably we need to
	  * add an Attr_Threadprivate to the enumerations used in nameres.h,
	  * and then add the corresponding table entries, and thereby make
	  * sure that the variable cited in the THREADPRIVATE directive is
	  * allowed by the OpenMP spec (e.g. it must not be in common, it must
	  * not be equivalenced, and it must either have the SAVE attribute
	  * or be a module variable.
	  * But for now, we just disable the fnd_semantic_err check without
	  * adding a new, more correct check.
	  */
#else /* KEY Bug 9029 */
	 if(!fnd_semantic_err((token_value == Tok_SGI_Dir_Section_Gp) ?
				 Obj_Section_Gp : Obj_Section_Non_Gp,
				 TOKEN_LINE(token),
				 TOKEN_COLUMN(token),
				 attr_idx,
				 TRUE)){
	    if(AT_OBJ_CLASS(attr_idx) == Pgm_Unit &&
	      ATP_PGM_UNIT(attr_idx) == Module){
	      if(token_value == Tok_SGI_Dir_Section_Gp){
	        if(attr_idx != SCP_ATTR_IDX(curr_scp_idx)){
		    PRINTMSG(TOKEN_LINE(token), 1491, Error,
		    	TOKEN_COLUMN(token),
		    	 "SECTION_GP" );
		}
		else if(SB_SECTION_NON_GP(SCP_SB_STATIC_IDX(curr_scp_idx))){
		    PRINTMSG(TOKEN_LINE(token), 1490, Error,
			TOKEN_COLUMN(token),
			AT_OBJ_NAME_PTR(attr_idx),
			"SECTION_GP", "SECTION_NON_GP");
		}
	      }
	      else if(token_value == Tok_SGI_Dir_Section_Non_Gp){
		if(attr_idx != SCP_ATTR_IDX(curr_scp_idx)){
		    PRINTMSG(TOKEN_LINE(token), 1491, Error,
			TOKEN_COLUMN(token),
			"SECTION_NON_GP");
		}
		else if(SB_SECTION_GP(SCP_SB_STATIC_IDX(curr_scp_idx))){
		    PRINTMSG(TOKEN_LINE(token), 1490, Error,
			TOKEN_COLUMN(token),
			AT_OBJ_NAME_PTR(attr_idx),
			"SECTION_NON_GP", "SECTION_GP");
		}
	      }
	    }
	 
            if(token_value == Tok_SGI_Dir_Section_Gp)
	      ATD_SECTION_GP(attr_idx) = TRUE;
	    else if(token_value == Tok_SGI_Dir_Section_Non_Gp)
	      ATD_SECTION_NON_GP(attr_idx) = TRUE;
	  }
#endif /* KEY Bug 9029 */
      }
      /* the above is added by jhs, 02.9.9 */
      else if (!parse_err_flush(Find_Comma_Rparen, "/common-block-name/ or identifier")) {
         break;
      }

      if (LA_CH_VALUE == COMMA) {
         NEXT_LA_CH;
      }
      else {
         break;
      }
   }
   while (TRUE);

   if (LA_CH_VALUE == RPAREN || parse_err_flush(Find_Rparen, ")")) {
      NEXT_LA_CH;
   }

   if (LA_CH_VALUE != EOS) {
      parse_err_flush(Find_EOS, EOS_STR);
   }
      
   TRACE (Func_Exit, "parse_slash_common_dirs", NULL);

   return;

}  /* parse_slash_common_dirs */

/******************************************************************************\
|*									      *|
|* Description:								      *|
|*	Parse the list following a suppress dir, if there is one.             *|
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

static void parse_dir_var_list(void)

{
   int		ir_idx;
   int		list_idx = NULL_IDX;
   opnd_type    opnd;


   TRACE (Func_Entry, "parse_dir_var_list", NULL);

   ir_idx = SH_IR_IDX(curr_stmt_sh_idx);

   do {

      if (MATCHED_TOKEN_CLASS(Tok_Class_Id)) {

         if (! parse_deref(&opnd, NULL_IDX)) {
            parse_err_flush(Find_Comma, NULL);
         }
         else {

            if (list_idx) {
               NTR_IR_LIST_TBL(IL_NEXT_LIST_IDX(list_idx));
               IL_PREV_LIST_IDX(IL_NEXT_LIST_IDX(list_idx)) = list_idx;
               list_idx = IL_NEXT_LIST_IDX(list_idx);
               IR_LIST_CNT_L(ir_idx)++;
            }
            else {
               NTR_IR_LIST_TBL(list_idx);
               IR_IDX_L(ir_idx) = list_idx;
               IR_FLD_L(ir_idx) = IL_Tbl_Idx;
               IR_LIST_CNT_L(ir_idx) = 1;
            }

            COPY_OPND(IL_OPND(list_idx), opnd);
         }
      }
      else if (!parse_err_flush(Find_Comma, "variable name")) {
         break;
      }

      if (LA_CH_VALUE == COMMA) {
         NEXT_LA_CH;
      }
      else if (LA_CH_VALUE == EOS ||
               !parse_err_flush(Find_Comma, ", or "EOS_STR)) {
         break;
      }
      else {  /* Issued error and recovered at a comma */
         NEXT_LA_CH;
      }
   }
   while (TRUE);

   NEXT_LA_CH;          /* Pick up EOS */

   TRACE (Func_Exit, "parse_dir_var_list", NULL);

   return;

}  /* parse_dir_var_list */

/******************************************************************************\
|*									      *|
|* Description:								      *|
|*	This routine parses the do all cmic. The ir it produces looks like .. *|
|*                                                                            *|
|*                        (Doall_Cmic_Opr)                                    *|
|*                       /                                                    *|
|*                      |- IF condition                                       *|
|*                      |- SHARED var list                                    *|
|*                      |- PRIVATE var list                                   *|
|*                      |- GETFIRST var list                                  *|
|*                      |- const one if AUTOSCOPE                             *|
|*                      |- CONTROL var list                                   *|
|*                      |- const one if SAVELAST                              *|
|*                      |- MAXCPUS value                                      *|
|*                      |- WORK DISTRIBUTION value (in const table)           *|
|*                      |- expression for work distribution                   *|
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

static void parse_doall_cmic(void)

{
   int		i;
   int		ir_idx;
   int		list_array[10];
   int		list_idx;
   opnd_type    opnd;


   TRACE (Func_Entry, "parse_doall_cmic", NULL);

   ir_idx = SH_IR_IDX(curr_stmt_sh_idx);

   for (i = 0; i < 10; i++) {
      NTR_IR_LIST_TBL(list_array[i]);
      if (i >= 1) {
         IL_NEXT_LIST_IDX(list_array[i - 1]) = list_array[i];
         IL_PREV_LIST_IDX(list_array[i]) = list_array[i - 1];
      }
   }

   IR_FLD_L(ir_idx) = IL_Tbl_Idx;
   IR_IDX_L(ir_idx) = list_array[0];
   IR_LIST_CNT_L(ir_idx) = 10;

   while (LA_CH_VALUE != EOS) {

      if (MATCHED_TOKEN_CLASS(Tok_Class_Dir_Kwd)) {

         switch (TOKEN_VALUE(token)) {

            case Tok_Dir_If:

               if (LA_CH_VALUE == LPAREN) {

                  if (IL_IDX(list_array[0]) != NULL_IDX) {
                     PRINTMSG(LA_CH_LINE, 680, Error, LA_CH_COLUMN,
                              "DOALL");
                     parse_err_flush(Find_EOS, NULL);
                     goto EXIT;
                  }

                  NEXT_LA_CH;
                  parse_expr(&opnd);

                  COPY_OPND(IL_OPND(list_array[0]), opnd);

                  if (LA_CH_VALUE == RPAREN) {
                     NEXT_LA_CH;
                  }
                  else {
                     parse_err_flush(Find_EOS, ")");
                     goto EXIT;
                  }
               }
               else {
                  parse_err_flush(Find_EOS, "(");
                  goto EXIT;
               }
               break;

            case Tok_Dir_Shared:

               if (LA_CH_VALUE == LPAREN) {
                  NEXT_LA_CH;
                  parse_var_name_list(&opnd);

                  if (IL_IDX(list_array[1]) == NULL_IDX) {
                     COPY_OPND(IL_OPND(list_array[1]), opnd);
                  }
                  else {
                     /* find the end of list */

                     list_idx = IL_IDX(list_array[1]);
                     while (IL_NEXT_LIST_IDX(list_idx)) {
                        list_idx = IL_NEXT_LIST_IDX(list_idx);
                     }

                     /* append the new list */
                     IL_NEXT_LIST_IDX(list_idx) = OPND_IDX(opnd);
                     IL_PREV_LIST_IDX(OPND_IDX(opnd)) = list_idx;
                     IL_LIST_CNT(list_array[1]) += OPND_LIST_CNT(opnd);
                  }

                  if (LA_CH_VALUE == RPAREN) {
                     NEXT_LA_CH;
                  } 
                  else {
                     parse_err_flush(Find_EOS, ")");
                     goto EXIT;
                  }
               }
               else {
                  parse_err_flush(Find_EOS, "(");
                  goto EXIT;
               }

               break;

            case Tok_Dir_Private:

               if (LA_CH_VALUE == LPAREN) {
                  NEXT_LA_CH;
                  parse_var_name_list(&opnd);

                  if (IL_IDX(list_array[2]) == NULL_IDX) {
                     COPY_OPND(IL_OPND(list_array[2]), opnd);
                  }
                  else {
                     /* find the end of list */

                     list_idx = IL_IDX(list_array[2]);
                     while (IL_NEXT_LIST_IDX(list_idx)) {
                        list_idx = IL_NEXT_LIST_IDX(list_idx);
                     }

                     /* append the new list */
                     IL_NEXT_LIST_IDX(list_idx) = OPND_IDX(opnd);
                     IL_PREV_LIST_IDX(OPND_IDX(opnd)) = list_idx;
                     IL_LIST_CNT(list_array[2]) += OPND_LIST_CNT(opnd);
                  }

                  if (LA_CH_VALUE == RPAREN) {
                     NEXT_LA_CH;
                  }   
                  else {
                     parse_err_flush(Find_EOS, ")");
                     goto EXIT;
                  }
               }
               else {
                  parse_err_flush(Find_EOS, "(");
                  goto EXIT;
               }

               break;

            case Tok_Dir_Getfirst:

               if (LA_CH_VALUE == LPAREN) {
                  NEXT_LA_CH;
                  parse_var_name_list(&opnd);

                  if (IL_IDX(list_array[3]) == NULL_IDX) {
                     COPY_OPND(IL_OPND(list_array[3]), opnd);
                  }
                  else {
                     /* find the end of list */

                     list_idx = IL_IDX(list_array[3]);
                     while (IL_NEXT_LIST_IDX(list_idx)) {
                        list_idx = IL_NEXT_LIST_IDX(list_idx);
                     }

                     /* append the new list */
                     IL_NEXT_LIST_IDX(list_idx) = OPND_IDX(opnd);
                     IL_PREV_LIST_IDX(OPND_IDX(opnd)) = list_idx;
                     IL_LIST_CNT(list_array[3]) += OPND_LIST_CNT(opnd);
                  }

                  if (LA_CH_VALUE == RPAREN) {
                     NEXT_LA_CH;
                  }
                  else {
                     parse_err_flush(Find_EOS, ")");
                     goto EXIT;
                  }
               }
               else {
                  parse_err_flush(Find_EOS, "(");
                  goto EXIT;
               }

               break;

            case Tok_Dir_Autoscope:

# if (defined(_TARGET_OS_IRIX) || defined(_TARGET_OS_LINUX)) || defined(_TARGET_OS_DARWIN)
               PRINTMSG(TOKEN_LINE(token), 1415, Error, TOKEN_COLUMN(token));
# else
               IL_FLD(list_array[4]) = CN_Tbl_Idx;
               IL_IDX(list_array[4]) = CN_INTEGER_ONE_IDX;
               IL_LINE_NUM(list_array[4]) = TOKEN_LINE(token);
               IL_COL_NUM(list_array[4])  = TOKEN_COLUMN(token);
# endif

               break;

            case Tok_Dir_Control:

               if (LA_CH_VALUE == LPAREN) {
                  NEXT_LA_CH;
                  parse_var_name_list(&opnd);

                  if (IL_IDX(list_array[5]) == NULL_IDX) {
                     COPY_OPND(IL_OPND(list_array[5]), opnd);
                  }
                  else {
                     /* find the end of list */

                     list_idx = IL_IDX(list_array[5]);
                     while (IL_NEXT_LIST_IDX(list_idx)) {
                        list_idx = IL_NEXT_LIST_IDX(list_idx);
                     }

                     /* append the new list */
                     IL_NEXT_LIST_IDX(list_idx) = OPND_IDX(opnd);
                     IL_PREV_LIST_IDX(OPND_IDX(opnd)) = list_idx;
                     IL_LIST_CNT(list_array[5]) += OPND_LIST_CNT(opnd);
                  }

                  if (LA_CH_VALUE == RPAREN) {
                     NEXT_LA_CH;
                  }   
                  else {
                     parse_err_flush(Find_EOS, ")");
                     goto EXIT;
                  }
               }
               else {
                  parse_err_flush(Find_EOS, "(");
                  goto EXIT;
               }

               break;

            case Tok_Dir_Savelast:

               IL_FLD(list_array[6]) = CN_Tbl_Idx;
               IL_IDX(list_array[6]) = CN_INTEGER_ONE_IDX;
               IL_LINE_NUM(list_array[6]) = TOKEN_LINE(token);
               IL_COL_NUM(list_array[6])  = TOKEN_COLUMN(token);

               break;

            case Tok_Dir_Maxcpus:

# if (defined(_TARGET_OS_IRIX) || defined(_TARGET_OS_LINUX)) || defined(_TARGET_OS_DARWIN)
               PRINTMSG(TOKEN_LINE(token), 1436, Warning,
                        TOKEN_COLUMN(token), "MAXCPUS");
               
# endif
               if (LA_CH_VALUE == LPAREN) {
                  NEXT_LA_CH;
                  parse_expr(&opnd);
                  COPY_OPND(IL_OPND(list_array[7]), opnd);

                  if (LA_CH_VALUE == RPAREN) {
                     NEXT_LA_CH;
                  }
                  else {
                     parse_err_flush(Find_EOS, ")");
                     goto EXIT;
                  }
               }
               else {
                  parse_err_flush(Find_EOS, "(");
                  goto EXIT;
               }
# if (defined(_TARGET_OS_IRIX) || defined(_TARGET_OS_LINUX)) || defined(_TARGET_OS_DARWIN)
               IL_OPND(list_array[7]) = null_opnd;
# endif
               break;

            case Tok_Dir_Single:

               if (IL_FLD(list_array[8]) != NO_Tbl_Idx) {
                  PRINTMSG(TOKEN_LINE(token), 800, Error, TOKEN_COLUMN(token));
                  parse_err_flush(Find_EOS, NULL);
                  goto EXIT;
               }

               IL_FLD(list_array[8]) = CN_Tbl_Idx;
               IL_IDX(list_array[8]) = C_INT_TO_CN(CG_INTEGER_DEFAULT_TYPE,
                                                   CMIC_WORK_DIST_SINGLE);
               IL_LINE_NUM(list_array[8]) = TOKEN_LINE(token);
               IL_COL_NUM(list_array[8])  = TOKEN_COLUMN(token);

               break;

            case Tok_Dir_Chunksize:

               if (IL_FLD(list_array[8]) != NO_Tbl_Idx) {
                  PRINTMSG(TOKEN_LINE(token), 800, Error, TOKEN_COLUMN(token));
                  parse_err_flush(Find_EOS, NULL);
                  goto EXIT;
               }

               IL_FLD(list_array[8]) = CN_Tbl_Idx;
               IL_IDX(list_array[8]) = C_INT_TO_CN(CG_INTEGER_DEFAULT_TYPE,
                                                   CMIC_WORK_DIST_CHUNKSIZE);
               IL_LINE_NUM(list_array[8]) = TOKEN_LINE(token);
               IL_COL_NUM(list_array[8])  = TOKEN_COLUMN(token);

               if (LA_CH_VALUE == LPAREN) {
                  NEXT_LA_CH;

                  if (parse_expr(&opnd)) {
                     COPY_OPND(IL_OPND(list_array[9]), opnd);
                  }

                  if (LA_CH_VALUE == RPAREN) {
                     NEXT_LA_CH;
                  }
                  else {
                     parse_err_flush(Find_EOS, ")");
                     goto EXIT;
                  }
               }
               else {
                  parse_err_flush(Find_EOS, "(");
                  goto EXIT;
               }
                  

               break;

            case Tok_Dir_Numchunks:

# if (defined(_TARGET_OS_IRIX) || defined(_TARGET_OS_LINUX)) || defined(_TARGET_OS_DARWIN)
               PRINTMSG(TOKEN_LINE(token), 1436, Warning,
                        TOKEN_COLUMN(token), "NUMCHUNKS");
# endif


               if (IL_FLD(list_array[8]) != NO_Tbl_Idx) {
                  PRINTMSG(TOKEN_LINE(token), 800, Error, TOKEN_COLUMN(token));
                  parse_err_flush(Find_EOS, NULL);
                  goto EXIT;
               }

               IL_FLD(list_array[8]) = CN_Tbl_Idx;
               IL_IDX(list_array[8]) = C_INT_TO_CN(CG_INTEGER_DEFAULT_TYPE,
                                                   CMIC_WORK_DIST_NUMCHUNKS);
               IL_LINE_NUM(list_array[8]) = TOKEN_LINE(token);
               IL_COL_NUM(list_array[8])  = TOKEN_COLUMN(token);

               if (LA_CH_VALUE == LPAREN) {
                  NEXT_LA_CH;

                  if (parse_expr(&opnd)) {
                     COPY_OPND(IL_OPND(list_array[9]), opnd);
                  }

                  if (LA_CH_VALUE == RPAREN) {
                     NEXT_LA_CH;
                  }
                  else {
                     parse_err_flush(Find_EOS, ")");
                     goto EXIT;
                  }
               }  
               else {
                  parse_err_flush(Find_EOS, "(");
                  goto EXIT;
               }

# if (defined(_TARGET_OS_IRIX) || defined(_TARGET_OS_LINUX)) || defined(_TARGET_OS_DARWIN)
               IL_OPND(list_array[8]) = null_opnd;
# endif
               break;

            case Tok_Dir_Guided:

               if (IL_FLD(list_array[8]) != NO_Tbl_Idx) {
                  PRINTMSG(TOKEN_LINE(token), 800, Error, TOKEN_COLUMN(token));
                  parse_err_flush(Find_EOS, NULL);
                  goto EXIT;
               }

               IL_FLD(list_array[8]) = CN_Tbl_Idx;
               IL_IDX(list_array[8]) = C_INT_TO_CN(CG_INTEGER_DEFAULT_TYPE,
                                                   CMIC_WORK_DIST_GUIDED);
               IL_LINE_NUM(list_array[8]) = TOKEN_LINE(token);
               IL_COL_NUM(list_array[8])  = TOKEN_COLUMN(token);

               if (LA_CH_VALUE == '(') {

                  if (parse_expr(&opnd)) {
                     COPY_OPND(IL_OPND(list_array[9]), opnd);
                  }
               }
               else {
                  IL_FLD(list_array[9]) = CN_Tbl_Idx;
                  IL_IDX(list_array[9]) = const_safevl_idx;
                  IL_LINE_NUM(list_array[9]) = TOKEN_LINE(token);
                  IL_COL_NUM(list_array[9])  = TOKEN_COLUMN(token);
               }

               break;

            case Tok_Dir_Vector:

               if (IL_FLD(list_array[8]) != NO_Tbl_Idx) {
                  PRINTMSG(TOKEN_LINE(token), 800, Error, TOKEN_COLUMN(token));
                  parse_err_flush(Find_EOS, NULL);
                  goto EXIT;
               }

               IL_FLD(list_array[8]) = CN_Tbl_Idx;
               IL_IDX(list_array[8]) = C_INT_TO_CN(CG_INTEGER_DEFAULT_TYPE,
                                                   CMIC_WORK_DIST_VECTOR);
               IL_LINE_NUM(list_array[8]) = TOKEN_LINE(token);
               IL_COL_NUM(list_array[8])  = TOKEN_COLUMN(token);

               break;

            case Tok_Dir_Ncpus_Chunks :

# ifdef _TARGET_OS_SOLARIS

               if (IL_FLD(list_array[8]) != NO_Tbl_Idx) {
                  PRINTMSG(TOKEN_LINE(token), 800, Error, TOKEN_COLUMN(token));
                  parse_err_flush(Find_EOS, NULL);
                  goto EXIT;
               }

               IL_FLD(list_array[8]) = CN_Tbl_Idx;
               IL_IDX(list_array[8]) = C_INT_TO_CN(CG_INTEGER_DEFAULT_TYPE,
                                                   CMIC_WORK_DIST_NCPUS_CHUNK);
               IL_LINE_NUM(list_array[8]) = TOKEN_LINE(token);
               IL_COL_NUM(list_array[8])  = TOKEN_COLUMN(token);

# else
               PRINTMSG(TOKEN_LINE(token), 1140, Warning, TOKEN_COLUMN(token));
# endif
               break;

            default:
               parse_err_flush(Find_EOS, NULL);
               PRINTMSG(TOKEN_LINE(token), 798, Error, TOKEN_COLUMN(token));
               break;
         }
      }
      else {
         parse_err_flush(Find_EOS, "parameter");
      }

      if (LA_CH_VALUE == COMMA) {
         NEXT_LA_CH;
      }
   }

   if (IL_FLD(list_array[8]) == NO_Tbl_Idx) {
      IL_FLD(list_array[8]) = CN_Tbl_Idx;

# ifdef _TARGET_OS_SOLARIS
      IL_IDX(list_array[8]) = C_INT_TO_CN(CG_INTEGER_DEFAULT_TYPE,
                                          CMIC_WORK_DIST_NCPUS_CHUNKS);
# elif (defined(_TARGET_OS_IRIX) || defined(_TARGET_OS_LINUX)) || defined(_TARGET_OS_DARWIN)
      IL_IDX(list_array[8]) = CN_INTEGER_ZERO_IDX;
# else
      IL_IDX(list_array[8]) = C_INT_TO_CN(CG_INTEGER_DEFAULT_TYPE,
                                          CMIC_WORK_DIST_SINGLE);
# endif
      IL_LINE_NUM(list_array[8]) = TOKEN_LINE(token);
      IL_COL_NUM(list_array[8])  = TOKEN_COLUMN(token);
   }

EXIT:

   TRACE (Func_Exit, "parse_doall_cmic", NULL);

   return;

}  /* parse_doall_cmic */

/******************************************************************************\
|*									      *|
|* Description:								      *|
|*	This routine parses the variable lists that are possibly within       *|
|*      paranthesis and have only variable names, not subobjects.             *|
|*									      *|
|* Input parameters:							      *|
|*	NONE								      *|
|*									      *|
|* Output parameters:							      *|
|*	list_opnd - points to list of attrs.                                  *|
|*									      *|
|* Returns:								      *|
|*	TRUE if no errors were encountered.				      *|
|*									      *|
\******************************************************************************/

static boolean parse_var_name_list(opnd_type   *list_opnd)

{
   int		column;
   int		line;
   int		list_idx = NULL_IDX;
   opnd_type	opnd;
   boolean      result   = TRUE;


   TRACE (Func_Entry, "parse_var_name_list", NULL);

   while (TRUE) {

      if (MATCHED_TOKEN_CLASS(Tok_Class_Id)) {
         parse_deref(&opnd, NULL_IDX);

         if (OPND_FLD(opnd) != AT_Tbl_Idx) {
            result = FALSE;
            find_opnd_line_and_column(&opnd, &line, &column);
            PRINTMSG(line, 1374, Error, column);
         }
         else {

            if (list_idx == NULL_IDX) {
               NTR_IR_LIST_TBL(list_idx);
               OPND_FLD((*list_opnd)) = IL_Tbl_Idx;
               OPND_IDX((*list_opnd)) = list_idx;
               OPND_LIST_CNT((*list_opnd)) = 1;
            }
            else {
               NTR_IR_LIST_TBL(IL_NEXT_LIST_IDX(list_idx));
               IL_PREV_LIST_IDX(IL_NEXT_LIST_IDX(list_idx)) = list_idx;
               (OPND_LIST_CNT((*list_opnd)))++;
               list_idx = IL_NEXT_LIST_IDX(list_idx);
            }

            COPY_OPND(IL_OPND(list_idx), opnd);
         }
      }
      else {
         parse_err_flush(Find_Comma_Rparen, "IDENTIFIER");
         result = FALSE;
      }

      if (LA_CH_VALUE != COMMA) {
         break;
      }

      NEXT_LA_CH;
   }

   TRACE (Func_Exit, "parse_var_name_list", NULL);

   return(result);

}  /* parse_var_name_list */

/******************************************************************************\
|*									      *|
|* Description:								      *|
|*	This routine parses the arguments to the DO PARALLEL cmic             *|
|*                                                                            *|
|*                             (Doparallel_Cmic_Opr)                          *|
|*                            /                                               *|
|*                           |- WORK DISTRIBUTION                             *|
|*                           |- work distribution opnd                        *|
|*                                                                            *|
|*                                                                            *|
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

static void parse_doparallel_cmic(void)

{
   int          i;
   int          ir_idx;
   int          list_array[2];
   opnd_type    opnd;


   TRACE (Func_Entry, "parse_doparallel_cmic", NULL);

   ir_idx = SH_IR_IDX(curr_stmt_sh_idx);

   for (i = 0; i < 2; i++) {
      NTR_IR_LIST_TBL(list_array[i]);
      if (i >= 1) {
         IL_NEXT_LIST_IDX(list_array[i - 1]) = list_array[i];
         IL_PREV_LIST_IDX(list_array[i]) = list_array[i - 1];
      }
   }

   IR_FLD_L(ir_idx) = IL_Tbl_Idx;
   IR_IDX_L(ir_idx) = list_array[0];
   IR_LIST_CNT_L(ir_idx) = 2;

   IL_OPND(list_array[0]) = null_opnd;

   if (LA_CH_VALUE == EOS) {
      goto EXIT;
   }

   if (MATCHED_TOKEN_CLASS(Tok_Class_Dir_Kwd)) {
      switch (TOKEN_VALUE(token)) {
      case Tok_Dir_Single:

         if (IL_FLD(list_array[0]) != NO_Tbl_Idx) {
            PRINTMSG(TOKEN_LINE(token), 1139, Error, TOKEN_COLUMN(token));
            parse_err_flush(Find_EOS, NULL);
            goto EXIT;
         }

         IL_FLD(list_array[0]) = CN_Tbl_Idx;
         IL_IDX(list_array[0]) = C_INT_TO_CN(CG_INTEGER_DEFAULT_TYPE,
                                             CMIC_WORK_DIST_SINGLE);
         IL_LINE_NUM(list_array[0]) = TOKEN_LINE(token);
         IL_COL_NUM(list_array[0])  = TOKEN_COLUMN(token);

         break;

      case Tok_Dir_Chunksize:

         if (IL_FLD(list_array[0]) != NO_Tbl_Idx) {
            PRINTMSG(TOKEN_LINE(token), 1139, Error, TOKEN_COLUMN(token));
            parse_err_flush(Find_EOS, NULL);
            goto EXIT;
         }

         IL_FLD(list_array[0]) = CN_Tbl_Idx;
         IL_IDX(list_array[0]) = C_INT_TO_CN(CG_INTEGER_DEFAULT_TYPE,
                                             CMIC_WORK_DIST_CHUNKSIZE);
         IL_LINE_NUM(list_array[0]) = TOKEN_LINE(token);
         IL_COL_NUM(list_array[0])  = TOKEN_COLUMN(token);

         if (LA_CH_VALUE == LPAREN) {
            NEXT_LA_CH;

            if (parse_expr(&opnd)) {
               COPY_OPND(IL_OPND(list_array[1]), opnd);
            }

            if (LA_CH_VALUE == RPAREN) {
               NEXT_LA_CH;
            }
            else {
               parse_err_flush(Find_EOS, ")");
               goto EXIT;
            }
         }
         else {
            parse_err_flush(Find_EOS, "(");
            goto EXIT;
         }

         break;

      case Tok_Dir_Numchunks:

# if (defined(_TARGET_OS_IRIX) || defined(_TARGET_OS_LINUX)) || defined(_TARGET_OS_DARWIN)
         PRINTMSG(TOKEN_LINE(token), 1436, Warning,
                  TOKEN_COLUMN(token), "NUMCHUNKS");
# endif

         if (IL_FLD(list_array[0]) != NO_Tbl_Idx) {
            PRINTMSG(TOKEN_LINE(token), 1139, Error, TOKEN_COLUMN(token));
            parse_err_flush(Find_EOS, NULL);
            goto EXIT;
         }

         IL_FLD(list_array[0]) = CN_Tbl_Idx;
         IL_IDX(list_array[0]) = C_INT_TO_CN(CG_INTEGER_DEFAULT_TYPE,
                                             CMIC_WORK_DIST_NUMCHUNKS);
         IL_LINE_NUM(list_array[0]) = TOKEN_LINE(token);
         IL_COL_NUM(list_array[0])  = TOKEN_COLUMN(token);

         if (LA_CH_VALUE == LPAREN) {
            NEXT_LA_CH;

            if (parse_expr(&opnd)) {
               COPY_OPND(IL_OPND(list_array[1]), opnd);
            }

            if (LA_CH_VALUE == RPAREN) {
               NEXT_LA_CH;
            }
            else {
               parse_err_flush(Find_EOS, ")");
               goto EXIT;
            }
         }
         else {
            parse_err_flush(Find_EOS, "(");
            goto EXIT;
         }

# if (defined(_TARGET_OS_IRIX) || defined(_TARGET_OS_LINUX)) || defined(_TARGET_OS_DARWIN)
         IL_OPND(list_array[0]) = null_opnd;
# endif
         break;

      case Tok_Dir_Guided:

         if (IL_FLD(list_array[0]) != NO_Tbl_Idx) {
            PRINTMSG(TOKEN_LINE(token), 1139, Error, TOKEN_COLUMN(token));
            parse_err_flush(Find_EOS, NULL);
            goto EXIT;
         }

         IL_FLD(list_array[0]) = CN_Tbl_Idx;
         IL_IDX(list_array[0]) = C_INT_TO_CN(CG_INTEGER_DEFAULT_TYPE,
                                             CMIC_WORK_DIST_GUIDED);
         IL_LINE_NUM(list_array[0]) = TOKEN_LINE(token);
         IL_COL_NUM(list_array[0])  = TOKEN_COLUMN(token);

         if (LA_CH_VALUE == '(') {
            if (parse_expr(&opnd)) {
               COPY_OPND(IL_OPND(list_array[1]), opnd);
            }
         }
         else {
            IL_FLD(list_array[1]) = CN_Tbl_Idx;
            IL_IDX(list_array[1]) = const_safevl_idx;
            IL_LINE_NUM(list_array[1]) = TOKEN_LINE(token);
            IL_COL_NUM(list_array[1])  = TOKEN_COLUMN(token);
         }

         break;

      case Tok_Dir_Vector:

         if (IL_FLD(list_array[0]) != NO_Tbl_Idx) {
            PRINTMSG(TOKEN_LINE(token), 1139, Error, TOKEN_COLUMN(token));
            parse_err_flush(Find_EOS, NULL);
            goto EXIT;
         }

         IL_FLD(list_array[0]) = CN_Tbl_Idx;
         IL_IDX(list_array[0]) = C_INT_TO_CN(CG_INTEGER_DEFAULT_TYPE,
                                             CMIC_WORK_DIST_VECTOR);
         IL_LINE_NUM(list_array[0]) = TOKEN_LINE(token);
         IL_COL_NUM(list_array[0])  = TOKEN_COLUMN(token);

         break;

      case Tok_Dir_Ncpus_Chunks :

# ifdef _TARGET_OS_SOLARIS

         if (IL_FLD(list_array[0]) != NO_Tbl_Idx) {
            PRINTMSG(TOKEN_LINE(token), 1139, Error, TOKEN_COLUMN(token));
            parse_err_flush(Find_EOS, NULL);
            goto EXIT;
         }

         IL_FLD(list_array[0]) = CN_Tbl_Idx;
         IL_IDX(list_array[0]) = C_INT_TO_CN(CG_INTEGER_DEFAULT_TYPE,
                                             CMIC_WORK_DIST_NCPUS_CHUNKS);
         IL_LINE_NUM(list_array[0]) = TOKEN_LINE(token);
         IL_COL_NUM(list_array[0])  = TOKEN_COLUMN(token);

# else
         PRINTMSG(TOKEN_LINE(token), 1140, Warning, TOKEN_COLUMN(token));
# endif
         break;


      default:
         parse_err_flush(Find_EOS, NULL);
         PRINTMSG(TOKEN_LINE(token), 808, Error, TOKEN_COLUMN(token));
         break;
      }
   }
   else {
      parse_err_flush(Find_EOS, "parameter");
   }

   if (LA_CH_VALUE != EOS) {
      parse_err_flush(Find_EOS, EOS_STR);
   }

EXIT:

   if (IL_FLD(list_array[0]) == NO_Tbl_Idx) {
      IL_FLD(list_array[0]) = CN_Tbl_Idx;

# if (defined(_TARGET_OS_IRIX) || defined(_TARGET_OS_LINUX)) || defined(_TARGET_OS_DARWIN)
      IL_IDX(list_array[0]) = CN_INTEGER_ZERO_IDX;
# else
      IL_IDX(list_array[0]) = C_INT_TO_CN(CG_INTEGER_DEFAULT_TYPE,
                                          CMIC_WORK_DIST_SINGLE);
# endif
      IL_LINE_NUM(list_array[0]) = TOKEN_LINE(token);
      IL_COL_NUM(list_array[0])  = TOKEN_COLUMN(token);
   }
      
   TRACE (Func_Exit, "parse_doparallel_cmic", NULL);

   return;

}  /* parse_doparallel_cmic */

/******************************************************************************\
|*									      *|
|* Description:								      *|
|*	This routine parses the parameters for the PARALLEL cmic.             *|
|*                                                                            *|
|*                        (Parallel_Cmic_Opr)                                 *|
|*                       /                                                    *|
|*                      |- IF condition                                       *|
|*                      |- SHARED var list                                    *|
|*                      |- PRIVATE var list                                   *|
|*                      |- GETFIRST var list                                  *|
|*                      |- const one if AUTOSCOPE                             *|
|*                      |- CONTROL var list                                   *|
|*                      |- MAXCPUS value                                      *|
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

static void parse_parallel_cmic(void)

{
   int          i;
   int          ir_idx;
   int          list_array[7];
   int		list_idx;
   opnd_type    opnd;


   TRACE (Func_Entry, "parse_parallel_cmic", NULL);

   ir_idx = SH_IR_IDX(curr_stmt_sh_idx);

   for (i = 0; i < 7; i++) {
      NTR_IR_LIST_TBL(list_array[i]);
      if (i >= 1) {
         IL_NEXT_LIST_IDX(list_array[i - 1]) = list_array[i];
         IL_PREV_LIST_IDX(list_array[i]) = list_array[i - 1];
      }
   }

   IR_FLD_L(ir_idx) = IL_Tbl_Idx;
   IR_IDX_L(ir_idx) = list_array[0];
   IR_LIST_CNT_L(ir_idx) = 7;

   while (LA_CH_VALUE != EOS) {

      if (MATCHED_TOKEN_CLASS(Tok_Class_Dir_Kwd)) {

         switch (TOKEN_VALUE(token)) {

            case Tok_Dir_If:

               if (LA_CH_VALUE == LPAREN) {

                  if (IL_IDX(list_array[0]) != NULL_IDX) {
                     PRINTMSG(LA_CH_LINE, 680, Error, LA_CH_COLUMN,
                              "PARALLEL");
                     parse_err_flush(Find_EOS, NULL);
                     goto EXIT;
                  }

                  NEXT_LA_CH;
                  parse_expr(&opnd);
                  COPY_OPND(IL_OPND(list_array[0]), opnd);

                  if (LA_CH_VALUE == RPAREN) {
                     NEXT_LA_CH;
                  }
                  else {
                     parse_err_flush(Find_EOS, ")");
                     goto EXIT;
                  }
               }
               else {
                  parse_err_flush(Find_EOS, "(");
                  goto EXIT;
               }
               break;

            case Tok_Dir_Shared:

               if (LA_CH_VALUE == LPAREN) {
                  NEXT_LA_CH;
                  parse_var_name_list(&opnd);

                  if (IL_IDX(list_array[1]) == NULL_IDX) {
                     COPY_OPND(IL_OPND(list_array[1]), opnd);
                  }
                  else {
                     /* find the end of list */

                     list_idx = IL_IDX(list_array[1]);
                     while (IL_NEXT_LIST_IDX(list_idx)) {
                        list_idx = IL_NEXT_LIST_IDX(list_idx);
                     }

                     /* append the new list */
                     IL_NEXT_LIST_IDX(list_idx) = OPND_IDX(opnd);
                     IL_PREV_LIST_IDX(OPND_IDX(opnd)) = list_idx;
                     IL_LIST_CNT(list_array[1]) += OPND_LIST_CNT(opnd);
                  }

                  if (LA_CH_VALUE == RPAREN) {
                     NEXT_LA_CH;
                  }
                  else {
                     parse_err_flush(Find_EOS, ")");
                     goto EXIT;
                  }
               }
               else {
                  parse_err_flush(Find_EOS, "(");
                  goto EXIT;
               }

               break;

            case Tok_Dir_Private:

               if (LA_CH_VALUE == LPAREN) {
                  NEXT_LA_CH;
                  parse_var_name_list(&opnd);

                  if (IL_IDX(list_array[2]) == NULL_IDX) {
                     COPY_OPND(IL_OPND(list_array[2]), opnd);
                  }
                  else {
                     /* find the end of list */

                     list_idx = IL_IDX(list_array[2]);
                     while (IL_NEXT_LIST_IDX(list_idx)) {
                        list_idx = IL_NEXT_LIST_IDX(list_idx);
                     }

                     /* append the new list */
                     IL_NEXT_LIST_IDX(list_idx) = OPND_IDX(opnd);
                     IL_PREV_LIST_IDX(OPND_IDX(opnd)) = list_idx;
                     IL_LIST_CNT(list_array[2]) += OPND_LIST_CNT(opnd);
                  }

                  if (LA_CH_VALUE == RPAREN) {
                     NEXT_LA_CH;
                  }
                  else {
                     parse_err_flush(Find_EOS, ")");
                     goto EXIT;
                  }
               }
               else {
                  parse_err_flush(Find_EOS, "(");
                  goto EXIT;
               }

               break;

            case Tok_Dir_Getfirst:

               if (LA_CH_VALUE == LPAREN) {
                  NEXT_LA_CH;
                  parse_var_name_list(&opnd);

                  if (IL_IDX(list_array[3]) == NULL_IDX) {
                     COPY_OPND(IL_OPND(list_array[3]), opnd);
                  }
                  else {
                     /* find the end of list */

                     list_idx = IL_IDX(list_array[3]);
                     while (IL_NEXT_LIST_IDX(list_idx)) {
                        list_idx = IL_NEXT_LIST_IDX(list_idx);
                     }

                     /* append the new list */
                     IL_NEXT_LIST_IDX(list_idx) = OPND_IDX(opnd);
                     IL_PREV_LIST_IDX(OPND_IDX(opnd)) = list_idx;
                     IL_LIST_CNT(list_array[3]) += OPND_LIST_CNT(opnd);
                  }

                  if (LA_CH_VALUE == RPAREN) {
                     NEXT_LA_CH;
                  }
                  else {
                     parse_err_flush(Find_EOS, ")");
                     goto EXIT;
                  }
               }
               else {
                  parse_err_flush(Find_EOS, "(");
                  goto EXIT;
               }

               break;

            case Tok_Dir_Autoscope:

# if (defined(_TARGET_OS_IRIX) || defined(_TARGET_OS_LINUX)) || defined(_TARGET_OS_DARWIN)
               PRINTMSG(TOKEN_LINE(token), 1415, Error, TOKEN_COLUMN(token));
# else

               IL_FLD(list_array[4]) = CN_Tbl_Idx;
               IL_IDX(list_array[4]) = CN_INTEGER_ONE_IDX;
               IL_LINE_NUM(list_array[4]) = TOKEN_LINE(token);
               IL_COL_NUM(list_array[4])  = TOKEN_COLUMN(token);

# endif
               break;

            case Tok_Dir_Control:

               if (LA_CH_VALUE == LPAREN) {
                  NEXT_LA_CH;
                  parse_var_name_list(&opnd);

                  if (IL_IDX(list_array[5]) == NULL_IDX) {
                     COPY_OPND(IL_OPND(list_array[5]), opnd);
                  }
                  else {
                     /* find the end of list */

                     list_idx = IL_IDX(list_array[5]);
                     while (IL_NEXT_LIST_IDX(list_idx)) {
                        list_idx = IL_NEXT_LIST_IDX(list_idx);
                     }

                     /* append the new list */
                     IL_NEXT_LIST_IDX(list_idx) = OPND_IDX(opnd);
                     IL_PREV_LIST_IDX(OPND_IDX(opnd)) = list_idx;
                     IL_LIST_CNT(list_array[5]) += OPND_LIST_CNT(opnd);
                  }

                  if (LA_CH_VALUE == RPAREN) {
                     NEXT_LA_CH;
                  }
                  else {
                     parse_err_flush(Find_EOS, ")");
                     goto EXIT;
                  }
               }
               else {
                  parse_err_flush(Find_EOS, "(");
                  goto EXIT;
               }

               break;

            case Tok_Dir_Maxcpus:

               if (LA_CH_VALUE == LPAREN) {
                  NEXT_LA_CH;
                  parse_expr(&opnd);
                  COPY_OPND(IL_OPND(list_array[6]), opnd);

                  if (LA_CH_VALUE == RPAREN) {
                     NEXT_LA_CH;
                  }
                  else {
                     parse_err_flush(Find_EOS, ")");
                     goto EXIT;
                  }
               }
               else {
                  parse_err_flush(Find_EOS, "(");
                  goto EXIT;
               }
               break;

            default:
               parse_err_flush(Find_EOS, NULL);
               PRINTMSG(TOKEN_LINE(token), 809, Error, TOKEN_COLUMN(token));
               break;
         }
      }
      else {
         parse_err_flush(Find_EOS, "parameter");
      }

      if (LA_CH_VALUE == COMMA) {
         NEXT_LA_CH;
      }
   }

EXIT:

   TRACE (Func_Exit, "parse_parallel_cmic", NULL);

   return;

}  /* parse_parallel_cmic */

/******************************************************************************\
|*									      *|
|* Description:								      *|
|*	Tasking directive block checks for end and contains stmts.            *|
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

void do_cmic_blk_checks(void)

{


   TRACE (Func_Entry, "do_cmic_blk_checks", NULL);

   if (cdir_switches.doall_sh_idx != NULL_IDX) {
      PRINTMSG(SH_GLB_LINE(cdir_switches.doall_sh_idx), 1219, Error,
               SH_COL_NUM(cdir_switches.doall_sh_idx),
               "DO ALL");
   }

   /* BHJ need new message here */

   if (cdir_switches.doacross_sh_idx != NULL_IDX) {
      PRINTMSG(SH_GLB_LINE(cdir_switches.doacross_sh_idx), 1219, Error,
               SH_COL_NUM(cdir_switches.doacross_sh_idx),
               "DOACROSS");
   }

   cdir_switches.no_internal_calls = FALSE;
   cdir_switches.parallel_region   = FALSE;
   cdir_switches.doall_region      = FALSE;
   cdir_switches.casedir           = FALSE;
   cdir_switches.guard             = FALSE;
   cdir_switches.guard_has_flag    = FALSE;
   cdir_switches.guard_in_par_reg  = FALSE;
   cdir_switches.do_parallel       = FALSE;

   cdir_switches.doall_sh_idx = NULL_IDX;
   cdir_switches.doacross_sh_idx = NULL_IDX;
   cdir_switches.dopar_sh_idx = NULL_IDX;

   TRACE (Func_Exit, "do_cmic_blk_checks", NULL);

   return;

}  /* do_cmic_blk_checks */

/******************************************************************************\
|*                                                                            *|
|* Description:                                                               *|
|*      This routine parses the variable and/or common block lists that are   *|
|*      on a CACHE_ALIGN cdir. No subobjects are allowed.                     *|
|*                                                                            *|
|* Input parameters:                                                          *|
|*      NONE                                                                  *|
|*                                                                            *|
|* Output parameters:                                                         *|
|*      opnd - points to list of attrs.                                       *|
|*                                                                            *|
|* Returns:                                                                   *|
|*      NOTHING                                                               *|
|*                                                                            *|
\******************************************************************************/

static void parse_cache_align_name_list(opnd_type *list_opnd)

{
   int		col;
   int		line;
   int          list_idx = NULL_IDX;
   opnd_type    opnd;
   int          sb_idx;


   TRACE (Func_Entry, "parse_cache_align_name_list", NULL);

   while(TRUE) {
      if (LA_CH_VALUE == SLASH) {
         /* must be common block */
         NEXT_LA_CH;    /* eat slash */

         if (MATCHED_TOKEN_CLASS(Tok_Class_Id)) {

            if (LA_CH_VALUE == SLASH) {
               NEXT_LA_CH;   /* eat slash */
               sb_idx = srch_stor_blk_tbl(TOKEN_STR(token),
                                          TOKEN_LEN(token),
                                          curr_scp_idx);

               if (sb_idx == NULL_IDX) {
                  sb_idx = ntr_stor_blk_tbl(TOKEN_STR(token),
                                            TOKEN_LEN(token),
                                            TOKEN_LINE(token),
                                            TOKEN_COLUMN(token),
                                            Common);
                  SB_BLANK_COMMON(sb_idx)        = FALSE;
                  SB_COMMON_NEEDS_OFFSET(sb_idx) = TRUE;
                  SB_IS_COMMON(sb_idx)           = TRUE;
               }

               if (SB_CACHE_ALIGN(sb_idx)) {
                  /* already specified in CACHE_ALIGN cdir */
                  PRINTMSG(TOKEN_LINE(token), 1065, Error,
                           TOKEN_COLUMN(token), SB_NAME_PTR(sb_idx));
               }
               else {
                  SB_CACHE_ALIGN(sb_idx) = TRUE;
               }
            }
            else {
               parse_err_flush(Find_EOS, "/");
            }
         }
         else {
            parse_err_flush(Find_EOS, "common-block-name");
         }
      }
      else if (MATCHED_TOKEN_CLASS(Tok_Class_Id)) {
         parse_deref(&opnd, NULL_IDX);

         if (OPND_FLD(opnd) != AT_Tbl_Idx) {
            find_opnd_line_and_column(&opnd, &line, &col);
            PRINTMSG(line, 1487, Error, col, "CACHE_ALIGN");
         }
         else {
            if (list_idx == NULL_IDX) {
               NTR_IR_LIST_TBL(list_idx);
               OPND_FLD((*list_opnd)) = IL_Tbl_Idx;
               OPND_IDX((*list_opnd)) = list_idx;
               OPND_LIST_CNT((*list_opnd)) = 1;
            }
            else {
               NTR_IR_LIST_TBL(IL_NEXT_LIST_IDX(list_idx));
               IL_PREV_LIST_IDX(IL_NEXT_LIST_IDX(list_idx)) = list_idx;
               (OPND_LIST_CNT((*list_opnd)))++;
               list_idx = IL_NEXT_LIST_IDX(list_idx);
            }
            COPY_OPND(IL_OPND(list_idx), opnd);
         }
      }
      else {
         parse_err_flush(Find_EOS, "IDENTIFIER");
      }

      if (LA_CH_VALUE != COMMA) {
         break;
      }
      NEXT_LA_CH;
   }

   TRACE (Func_Exit, "parse_cache_align_name_list", NULL);

   return;

}  /* parse_cache_align_name_list */

/******************************************************************************\
|*									      *|
|* Description:								      *|
|*	This routine parses the CDIR$ NAME line.	                      *|
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
static void parse_name_dir(void)

{
   int		attr_idx;
   int		column;
   int		idx;
   long		length;
   int		line;
   char	       *name;
   int		name_idx;
   opnd_type	opnd;


   TRACE (Func_Entry, "parse_name_dir", NULL);

   if (LA_CH_VALUE != LPAREN) {
      parse_err_flush(Find_EOS, "(");
      NEXT_LA_CH;  /* pick up EOS */
      return;
   }

   NEXT_LA_CH;  /* Pick up Lparen */

   do {
      if (MATCHED_TOKEN_CLASS(Tok_Class_Id)) {
         attr_idx = srch_sym_tbl(TOKEN_STR(token), TOKEN_LEN(token),
                                 &name_idx);

         if (attr_idx == NULL_IDX) {
            attr_idx			= ntr_sym_tbl(&token, name_idx);
            LN_DEF_LOC(name_idx)	= TRUE;
            AT_OBJ_CLASS(attr_idx)	= Pgm_Unit;
            ATP_PROC(attr_idx)		= Extern_Proc;
            ATP_NAME_IN_STONE(attr_idx)	= TRUE;
            ATP_SCP_IDX(attr_idx)	= curr_scp_idx;
         }
         else if (!fnd_semantic_err(Obj_Name,
                                    TOKEN_LINE(token),
                                    TOKEN_COLUMN(token),
                                    attr_idx,
                                    TRUE)) {

            if (AT_REFERENCED(attr_idx) == Char_Rslt_Bound_Ref) {
               AT_ATTR_LINK(attr_idx)	= NULL_IDX;
               LN_DEF_LOC(name_idx)	= TRUE;
            }

            if (AT_OBJ_CLASS(attr_idx) == Data_Obj) {
               chg_data_obj_to_pgm_unit(attr_idx,
                                        Pgm_Unknown,
                                        Extern_Proc);
               ATP_NAME_IN_STONE(attr_idx)	= TRUE;
            }
            else if (AT_OBJ_CLASS(attr_idx) == Pgm_Unit) {
               ATP_PROC(attr_idx)		= Extern_Proc;
               ATP_NAME_IN_STONE(attr_idx)	= TRUE;
            }
         }
         else {
            CREATE_ERR_ATTR(attr_idx,
                            TOKEN_LINE(token),
                            TOKEN_COLUMN(token),
                            Pgm_Unit);
            ATP_PROC(attr_idx)		= Extern_Proc;
            ATP_NAME_IN_STONE(attr_idx)	= TRUE;
         }

         if (LA_CH_VALUE == EQUAL) {
            NEXT_LA_CH;

            if (LA_CH_VALUE == QUOTE ||
                LA_CH_VALUE == DBL_QUOTE) {

               if (parse_operand(&opnd)) {
                  find_opnd_line_and_column(&opnd, &line, &column);

                  if (OPND_FLD(opnd)!= CN_Tbl_Idx ||
                      TYP_TYPE(CN_TYPE_IDX(OPND_IDX(opnd))) != Character) {
                     PRINTMSG(line, 1111, Error, column);
                     AT_DCL_ERR(attr_idx)		= TRUE;
                     ATP_EXT_NAME_LEN(attr_idx)	= AT_NAME_LEN(attr_idx);
                     ATP_EXT_NAME_IDX(attr_idx)	= AT_NAME_IDX(attr_idx);
                  }
                  else {
                     length = (long) CN_INT_TO_C(TYP_IDX(
                                                 CN_TYPE_IDX(OPND_IDX(opnd))));

                     NTR_NAME_POOL((long *) &(CN_CONST(OPND_IDX(opnd))), 
                                   (int) length, name_idx);

                     ATP_EXT_NAME_IDX(attr_idx) = name_idx;
                     ATP_EXT_NAME_LEN(attr_idx) = length;
                     name			= ATP_EXT_NAME_PTR(attr_idx);

                     for (idx = 0; 
                          idx < (WORD_LEN(length)*TARGET_BYTES_PER_WORD)-length;
                          idx++) {
                        *(name + length + idx) = '\0';
                     }
                  }
               }
               else {
                  parse_err_flush(Find_Rparen, NULL);
                  AT_DCL_ERR(attr_idx)		= TRUE;
                  ATP_EXT_NAME_LEN(attr_idx)	= AT_NAME_LEN(attr_idx);
                  ATP_EXT_NAME_IDX(attr_idx)	= AT_NAME_IDX(attr_idx);
               }
            }
            else {
               PRINTMSG(LA_CH_LINE, 1111, Error, LA_CH_COLUMN);
               parse_err_flush(Find_Rparen, NULL);
               AT_DCL_ERR(attr_idx)		= TRUE;
               ATP_EXT_NAME_LEN(attr_idx)	= AT_NAME_LEN(attr_idx);
               ATP_EXT_NAME_IDX(attr_idx)	= AT_NAME_IDX(attr_idx);
            }
         }
         else {	
            parse_err_flush(Find_Rparen, "=");
            AT_DCL_ERR(attr_idx)	= TRUE;
            ATP_EXT_NAME_LEN(attr_idx)	= AT_NAME_LEN(attr_idx);
            ATP_EXT_NAME_IDX(attr_idx)	= AT_NAME_IDX(attr_idx);
         }
      }
      else if (!parse_err_flush(Find_Comma, "procedure name")) {
         break;			/* Couldn't recover.  Hit EOS */
      }

      if (LA_CH_VALUE == COMMA) {
         NEXT_LA_CH;
      }
      else {
         break;
      }
   }
   while (TRUE);

   if (LA_CH_VALUE == RPAREN || parse_err_flush(Find_Rparen, ", or )")) {
      NEXT_LA_CH;
   }

   if (LA_CH_VALUE != EOS) {
      parse_err_flush(Find_EOS, EOS_STR);
   }
      
   NEXT_LA_CH;		/* Pick up EOS */

   TRACE (Func_Exit, "parse_name_dir", NULL);

   return;

}  /* parse_name_dir */

/******************************************************************************\
|*									      *|
|* Description:								      *|
|*	This routine parses the CMIC$ PERMUTATION line.                       *|
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
static void parse_permutation_mic(void)

{
   int		attr_idx;
   int		name_idx;

# if (defined(_TARGET_OS_IRIX) || defined(_TARGET_OS_LINUX)) || defined(_TARGET_OS_DARWIN)
   int		ir_idx;
   int		list_idx = NULL_IDX;
# endif


   TRACE (Func_Entry, "parse_permutation_mic", NULL);

   if (LA_CH_VALUE != LPAREN) {
      parse_err_flush(Find_EOS, "(");
      return;
   }

# if (defined(_TARGET_OS_IRIX) || defined(_TARGET_OS_LINUX)) || defined(_TARGET_OS_DARWIN)
   ir_idx = SH_IR_IDX(curr_stmt_sh_idx);
   IR_OPR(ir_idx) = Assert_Star_Opr;

   IR_FLD_L(ir_idx) = CN_Tbl_Idx;
   IR_IDX_L(ir_idx) = C_INT_TO_CN(CG_INTEGER_DEFAULT_TYPE,
                                  ASSERT_PERMUTATION);
   IR_LINE_NUM_L(ir_idx) = IR_LINE_NUM(ir_idx);
   IR_COL_NUM_L(ir_idx) = IR_COL_NUM(ir_idx);
# endif

   NEXT_LA_CH;  /* Pick up Lparen */

   do {

      if (MATCHED_TOKEN_CLASS(Tok_Class_Id)) {
         attr_idx = srch_sym_tbl(TOKEN_STR(token), TOKEN_LEN(token),
                                 &name_idx);

         if (attr_idx == NULL_IDX) {
            attr_idx			= ntr_sym_tbl(&token, name_idx);
            LN_DEF_LOC(name_idx)	= TRUE;
            AT_OBJ_CLASS(attr_idx)	= Data_Obj;
            ATD_PERMUTATION(attr_idx)	= TRUE;
         }
         else if (AT_OBJ_CLASS(attr_idx) != Data_Obj) {
            PRINTMSG(AT_DEF_LINE(attr_idx), 1126, Error, 
                     AT_DEF_COLUMN(attr_idx),
                     AT_OBJ_NAME_PTR(attr_idx));
            AT_DCL_ERR(attr_idx)	= TRUE;
         }
         else {
            ATD_PERMUTATION(attr_idx)	= TRUE;

            if (AT_REFERENCED(attr_idx) == Char_Rslt_Bound_Ref) {
               AT_ATTR_LINK(attr_idx)	= NULL_IDX;
               LN_DEF_LOC(name_idx)	= TRUE;
            }
         }

# if (defined(_TARGET_OS_IRIX) || defined(_TARGET_OS_LINUX)) || defined(_TARGET_OS_DARWIN)
         if (list_idx == NULL_IDX) {
            NTR_IR_LIST_TBL(list_idx);
            IR_FLD_R(ir_idx) = IL_Tbl_Idx;
            IR_IDX_R(ir_idx) = list_idx;
            IR_LIST_CNT_R(ir_idx) = 1;
         }
         else {
            NTR_IR_LIST_TBL(IL_NEXT_LIST_IDX(list_idx));
            IL_PREV_LIST_IDX(IL_NEXT_LIST_IDX(list_idx)) = list_idx;
            list_idx = IL_NEXT_LIST_IDX(list_idx);
            (IR_LIST_CNT_R(ir_idx))++;
         }

         IL_FLD(list_idx) = AT_Tbl_Idx;
         IL_IDX(list_idx) = attr_idx;
         IL_LINE_NUM(list_idx) = TOKEN_LINE(token);
         IL_COL_NUM(list_idx) = TOKEN_COLUMN(token);
# endif
      }
      else if (!parse_err_flush(Find_Comma, "array name")) {
         break;			/* Couldn't recover.  Hit EOS */
      }

      if (LA_CH_VALUE == COMMA) {
         NEXT_LA_CH;
      }
      else {
         break;
      }
   }
   while (TRUE);

   if (LA_CH_VALUE == RPAREN || parse_err_flush(Find_Rparen, ", or )")) {
      NEXT_LA_CH;
   }

   if (LA_CH_VALUE != EOS) {
      parse_err_flush(Find_EOS, EOS_STR);
   }

   TRACE (Func_Exit, "parse_permutation_mic", NULL);

   return;

}  /* parse_permutation_mic */

/******************************************************************************\
|*									      *|
|* Description:								      *|
|*	This routine parses the CDIR$ INLINE ALWAYS and INLINE NEVER line.    *|
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
static void parse_inline_always_never(boolean	always)

{
   boolean	amb_ref;
   int		attr_idx;
   int		host_attr_idx;
   int		host_name_idx;
   int		name_idx;


   TRACE (Func_Entry, "parse_inline_always_never", NULL);

   do {
      if (MATCHED_TOKEN_CLASS(Tok_Class_Id)) {
         amb_ref  = FALSE;
         attr_idx = srch_sym_tbl(TOKEN_STR(token),
                                 TOKEN_LEN(token),
                                 &name_idx);

         if (attr_idx != NULL_IDX) {
            host_attr_idx = attr_idx;

            if (!LN_DEF_LOC(name_idx)) {
               amb_ref = TRUE;

               while (AT_ATTR_LINK(host_attr_idx) != NULL_IDX) {
                  host_attr_idx = AT_ATTR_LINK(host_attr_idx);
               }
            }
         }
         else { /* any other reference is ambiguous */
            amb_ref		= TRUE;
            host_attr_idx	= srch_host_sym_tbl(TOKEN_STR(token),
                                                    TOKEN_LEN(token),
                                                    &host_name_idx,
                                                    TRUE);

            if (host_attr_idx != NULL_IDX) { 

               if (AT_IS_INTRIN(host_attr_idx) &&
                   ATI_FIRST_SPECIFIC_IDX(host_attr_idx) == NULL_IDX) {
                   complete_intrinsic_definition(host_attr_idx);
                   attr_idx = srch_sym_tbl(TOKEN_STR(token),
                                           TOKEN_LEN(token),
                                           &name_idx);
               }

               /* copy the attr into the local scp */

               attr_idx = ntr_host_in_sym_tbl(&token,
                                              name_idx,
                                              host_attr_idx,
                                              host_name_idx,
                                              TRUE);

               if (AT_IS_INTRIN(host_attr_idx)) {
                  COPY_VARIANT_ATTR_INFO(host_attr_idx,
                                         attr_idx,
                                         Interface);

                  AT_IS_INTRIN(attr_idx)	= TRUE;
                  AT_ATTR_LINK(attr_idx)	= NULL_IDX;
                  AT_ELEMENTAL_INTRIN(attr_idx) = 
                                           AT_ELEMENTAL_INTRIN(host_attr_idx);
                  AT_DEF_LINE(attr_idx)         = TOKEN_LINE(token);
                  AT_DEF_COLUMN(attr_idx)       = TOKEN_COLUMN(token);
               }
               else if (AT_OBJ_CLASS(attr_idx) != Interface) {
                  AT_ATTR_LINK(attr_idx) = host_attr_idx;

                  while (AT_ATTR_LINK(host_attr_idx) != NULL_IDX) {
                     host_attr_idx = AT_ATTR_LINK(host_attr_idx);
                  }
               }
            }
         }

         if (attr_idx == NULL_IDX) {
            attr_idx			= ntr_sym_tbl(&token, name_idx);
            AT_OBJ_CLASS(attr_idx)	= Pgm_Unit;
            ATP_PGM_UNIT(attr_idx)	= Pgm_Unknown;
            ATP_SCP_IDX(attr_idx)	= curr_scp_idx;
            ATP_PROC(attr_idx)		= Unknown_Proc;
            MAKE_EXTERNAL_NAME(attr_idx,
                               AT_NAME_IDX(attr_idx),
                               AT_NAME_LEN(attr_idx));
         }
         else if (!amb_ref) {

            /* Allow the inline directive with user specified intrinsics */
            /* We will check for user specified intrinsics in decl_sem   */

            if (fnd_semantic_err(Obj_Inline,
                                 TOKEN_LINE(token),
                                 TOKEN_COLUMN(token),
                                 attr_idx,
                                 TRUE)) {

               goto NEXT;
            }
         }

         if (AT_OBJ_CLASS(attr_idx) == Pgm_Unit &&
             (ATP_INLINE_ALWAYS(attr_idx) || ATP_INLINE_NEVER(attr_idx))) {

            if ((always && ATP_INLINE_NEVER(attr_idx)) ||
                (!always && ATP_INLINE_ALWAYS(attr_idx))) {
               PRINTMSG(AT_DEF_LINE(attr_idx), 1147, Error, 
                        AT_DEF_COLUMN(attr_idx),
                        AT_OBJ_NAME_PTR(attr_idx));
            }
         }
         else {

            if (AT_OBJ_CLASS(attr_idx) == Interface) {

               if (ATI_INLINE_ALWAYS(attr_idx) || ATI_INLINE_NEVER(attr_idx)) {

                  if ((always && ATI_INLINE_NEVER(attr_idx)) ||
                      (!always && ATI_INLINE_ALWAYS(attr_idx))) {
                     PRINTMSG(AT_DEF_LINE(attr_idx), 1147, Error, 
                              AT_DEF_COLUMN(attr_idx),
                              AT_OBJ_NAME_PTR(attr_idx));
                  }
               }
               else if (always) {
                  ATI_INLINE_ALWAYS(attr_idx)	= TRUE;
               }
               else {
                  ATI_INLINE_NEVER(attr_idx)	= TRUE;
               }
            }
            else {

               if (AT_OBJ_CLASS(attr_idx) == Data_Obj) { /* Switch to Function*/
                  chg_data_obj_to_pgm_unit(attr_idx, Pgm_Unknown, Unknown_Proc);
               }

               if (always) {
                  ATP_INLINE_ALWAYS(attr_idx)	= TRUE;
               }
               else {
                  ATP_INLINE_NEVER(attr_idx)	= TRUE;
               }
            }
         }
      }
      else if (!parse_err_flush(Find_Comma, "procedure name")) {
         break;			/* Couldn't recover.  Hit EOS */
      }

NEXT:

      if (LA_CH_VALUE == COMMA) {
         NEXT_LA_CH;
      }
      else if (LA_CH_VALUE == EOS ||
               !parse_err_flush(Find_Comma, ", or "EOS_STR)) {
         break;
      }
      else {  /* Issued error and recovered at a comma */
         NEXT_LA_CH;
      }
   }
   while (TRUE);

   NEXT_LA_CH;		/* Pick up EOS */

   TRACE (Func_Exit, "parse_inline_always_never", NULL);

   return;

}  /* parse_inline_always_never */

/******************************************************************************\
|*									      *|
|* Description:								      *|
|*	Check an index to see if it needs a type update.		      *|
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
static int	update_fld_type(fld_type	fld,
				int		idx,
				int		new_type)

{
   int		new_idx;
   long_type	the_constant[MAX_WORDS_FOR_INTEGER];
   int		type_idx;


   TRACE (Func_Entry, "update_fld_type", NULL);

   switch (fld) {
   case CN_Tbl_Idx:

      if (CN_TYPE_IDX(idx) == INTEGER_DEFAULT_TYPE) {
         type_idx = new_type;

         if (folder_driver((char *)CN_CONST(idx),
                           INTEGER_DEFAULT_TYPE,
                           NULL,
                           NULL_IDX,
                           the_constant,
                           &type_idx,
                           stmt_start_line,
                           stmt_start_col,
                           1,
                           Cvrt_Opr)) {
            new_idx = ntr_const_tbl(new_type,
                                    FALSE,
                                    the_constant);
         }
      }
      break;

   case AT_Tbl_Idx:

      if (AT_OBJ_CLASS(idx) == Data_Obj) {

         switch (ATD_CLASS(idx)) {
         case Constant:

            if (ATD_TYPE_IDX(idx) == INTEGER_DEFAULT_TYPE) {
               new_idx		  = update_fld_type(CN_Tbl_Idx,
                                                    ATD_CONST_IDX(idx),
                                                    new_type);
               ATD_CONST_IDX(idx) = new_idx;
            }
            break;

         case Function_Result:
         case Atd_Unknown:
         case Dummy_Argument:
         case CRI__Pointee:
         case Struct_Component:
            break;

         case Compiler_Tmp:
            new_idx = update_fld_type((fld_type) ATD_FLD(idx),
                                      ATD_TMP_IDX(idx),
                                      new_type);

            if (ATD_FLD(idx) == CN_Tbl_Idx) {
               ATD_TMP_IDX(idx) = new_idx;
            }
            break;
         }  /* End switch */

         if (ATD_TYPE_IDX(idx) == INTEGER_DEFAULT_TYPE) {
            ATD_TYPE_IDX(idx)	= new_type;
         }
      }
      else if (AT_OBJ_CLASS(idx) == Pgm_Unit &&
               ATP_PGM_UNIT(idx) == Function &&
               ATP_RSLT_IDX(idx) != NULL_IDX &&
               ATD_TYPE_IDX(ATP_RSLT_IDX(idx)) == INTEGER_DEFAULT_TYPE) {
         ATD_TYPE_IDX(ATP_RSLT_IDX(idx))	= new_type;
      }
      new_idx	  = NULL_IDX;

      break;

   case IR_Tbl_Idx:

      new_idx = update_fld_type(IR_FLD_L(idx), IR_IDX_L(idx), new_type);

      if (IR_FLD_L(idx) == CN_Tbl_Idx) {
         IR_IDX_L(idx) = new_idx;
      }

      new_idx = update_fld_type(IR_FLD_R(idx), IR_IDX_R(idx), new_type);

      if (IR_FLD_R(idx) == CN_Tbl_Idx) {
         IR_IDX_R(idx) = new_idx;
      }

      new_idx = NULL_IDX;

      if (IR_TYPE_IDX(idx) == INTEGER_DEFAULT_TYPE) {
         IR_TYPE_IDX(idx) = new_type;
      }
     
      break;

   case IL_Tbl_Idx:

      while (idx != NULL_IDX) {
         new_idx = update_fld_type(IL_FLD(idx), IL_IDX(idx), new_type);

         if (IL_FLD(idx) == CN_Tbl_Idx) {
            IL_IDX(idx) = new_idx;
         }
         idx	= IL_NEXT_LIST_IDX(idx);
      }
      new_idx	= NULL_IDX;
      break;

   case NO_Tbl_Idx:
   case SH_Tbl_Idx:
      new_idx	= NULL_IDX;
      break;

   }  /* End switch */

   TRACE (Func_Exit, "update_fld_type", NULL);

   return(new_idx);

}  /* update_fld_type */

/******************************************************************************\
|*									      *|
|* Description:								      *|
|*	This routine parses the CDIR$ SYMMETRIC line.                         *|
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
static void parse_symmetric_dir(void)

{
   int		attr_idx;
   int		name_idx;


   TRACE (Func_Entry, "parse_symmetric_dir", NULL);

   do {
      if (MATCHED_TOKEN_CLASS(Tok_Class_Id)) {
         attr_idx = srch_sym_tbl(TOKEN_STR(token), TOKEN_LEN(token),
                                 &name_idx);

         if (attr_idx == NULL_IDX) {
            attr_idx			= ntr_sym_tbl(&token, name_idx);
            LN_DEF_LOC(name_idx)	= TRUE;
            AT_OBJ_CLASS(attr_idx)	= Data_Obj;
            ATD_SYMMETRIC(attr_idx)	= TRUE;
            ATD_CLASS(attr_idx)		= Variable;
            SET_IMPL_TYPE(attr_idx);
         }
         else if (!fnd_semantic_err(Obj_Symmetric, 
                                    TOKEN_LINE(token),
                                    TOKEN_COLUMN(token),
                                    attr_idx,
                                    TRUE)) {

            if (AT_REFERENCED(attr_idx) == Char_Rslt_Bound_Ref) {
               AT_ATTR_LINK(attr_idx)	= NULL_IDX;
               LN_DEF_LOC(name_idx)	= TRUE;
            }

            ATD_SYMMETRIC(attr_idx)	= TRUE;
            ATD_CLASS(attr_idx)		= Variable;
         }
      }
      else if (!parse_err_flush(Find_Comma, "procedure name")) {
         break;			/* Couldn't recover.  Hit EOS */
      }

      if (LA_CH_VALUE == COMMA) {
         NEXT_LA_CH;
      }
      else if (LA_CH_VALUE == EOS ||
               !parse_err_flush(Find_Comma, ", or "EOS_STR)) {
         break;
      }
      else {  /* Issued error and recovered at a comma */
         NEXT_LA_CH;
      }
   }
   while (TRUE);

   NEXT_LA_CH;		/* Pick up EOS */

   TRACE (Func_Exit, "parse_symmetric_dir", NULL);

   return;

}  /* parse_symmetric_dir */

/******************************************************************************\
|*									      *|
|* Description:								      *|
|*	This routine parses the !DIR$ directives on one line of source.       *|
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
static void parse_dir_directives(void)
{

   int			blk_idx;
   int			buf_idx;
   int			cdir_info_idx;
   int			ir_idx;
   int			label_idx;
   int			list_idx;
   opnd_type		opnd;
   operator_type	opr;
   int			stmt_num;
   int			type_idx;

# if (defined(_TARGET_OS_IRIX) || defined(_TARGET_OS_LINUX)) || defined(_TARGET_OS_DARWIN)
   int			cvrt_idx;
# endif


   TRACE (Func_Entry, "parse_dir_directives", NULL);

   for (;;) {

      if (TOKEN_VALUE(token) > Tok_Dir_Start &&
          TOKEN_VALUE(token) < Tok_Dir_End &&
          disregard_directive[TOKEN_VALUE(token) - Tok_Dir_Start]) {

         /* Some CDIR$s have a list associated with them.  In such a case,    */
         /* if the CDIR$ is being ignored, we can flush to the end of the     */
         /* line because no other CDIR$ can follow it on the line.  In all    */
         /* other cases, we can only flush to the next comma so that a        */
         /* following CDIR$ can be processed (if one exists).		      */

         /* There are probably several more CDIR$s that need to be added to   */
         /* "list" group once we implement the MPP/CRAFT CDIR$s.	      */
    
         switch (TOKEN_VALUE(token)) {

            case Tok_Dir_Auxiliary:
            case Tok_Dir_Blockable:
            case Tok_Dir_Blockingsize:
            case Tok_Dir_Bounds:
            case Tok_Dir_Cache_Align:
            case Tok_Dir_Cache_Noalloc:
            case Tok_Dir_Cncall:
            case Tok_Dir_Common:
            case Tok_Dir_Inline_Always:
            case Tok_Dir_Inline_Never:
            case Tok_Dir_Maxcpus:
            case Tok_Dir_Nobounds:
            case Tok_Dir_Numcpus:
            case Tok_Dir_Cache_Bypass:
            case Tok_Dir_Nosideeffects:
            case Tok_Dir_Permutation:
            case Tok_Dir_Suppress:
            case Tok_Dir_Symmetric:
            case Tok_Dir_Taskcommon:
            case Tok_Dir_Vfunction:
               parse_err_flush(Find_EOS, NULL);
               break;

            default:
               parse_err_flush(Find_Comma, NULL);
         }
         
         goto CONTINUE;
      }

      if (TOKEN_VALUE(token) <= Tok_Dir_Start ||
          TOKEN_VALUE(token) >= Tok_Dir_End) {
         PRINTMSG(TOKEN_LINE(token), 790, Warning, TOKEN_COLUMN(token));
         parse_err_flush(Find_EOS, NULL);
         goto CONTINUE;  /* Invalid token */
      }

      cdir_info_idx	= TOKEN_VALUE(token) - Tok_Dir_Start;

      /* The following determines if the directive is allowed on the */
      /* platform that this compiler is built for.  The table is in  */
      /* p_directiv.h and the target information is in target.m      */

      if (!cdir_info[cdir_info_idx].on_platform) {
         PRINTMSG(TOKEN_LINE(token), cdir_info[cdir_info_idx].msg_num, Warning, 
                  TOKEN_COLUMN(token));
         parse_err_flush(Find_EOS, NULL);
         goto CONTINUE;
      }

      if (cdir_info[cdir_info_idx].issue_795 &&
          curr_stmt_category < Dir_Integer_Stmt_Cat) {
         PRINTMSG(TOKEN_LINE(token), 795, Warning,
                  TOKEN_COLUMN(token), cdir_info[cdir_info_idx].name);
         parse_err_flush(Find_EOS, NULL);
         goto CONTINUE;
      }

      if (cdir_info[cdir_info_idx].issue_531 &&
          curr_stmt_category >= Executable_Stmt_Cat) {
         PRINTMSG(TOKEN_LINE(token), 531, Error,
                  TOKEN_COLUMN(token), cdir_info[cdir_info_idx].name);
         parse_err_flush(Find_EOS, NULL);
         goto CONTINUE;
      }

      switch (TOKEN_VALUE(token)) {
      case Tok_Dir_Align:

         if (opt_flags.scalar_lvl == Scalar_Lvl_0) {
            parse_err_flush(Find_Comma, NULL);
         }
         else {
            ir_idx = gen_directive_ir(Align_Cdir_Opr);
         }
         break;


      case Tok_Dir_Auxiliary:
         parse_auxiliary_dir();
         goto EXIT;


      case Tok_Dir_Bl:

         if (opt_flags.scalar_lvl == Scalar_Lvl_0 || !opt_flags.bottom_load) {
            parse_err_flush(Find_Comma, NULL);
         }
         else {
            cdir_switches.bl	= TRUE;
            ir_idx		= gen_directive_ir(Bl_Cdir_Opr);
         }
         break;

      case Tok_Dir_Blockable:
      case Tok_Dir_Blockingsize:
      case Tok_Dir_Interchange:
         parse_star_dir_directives();
         goto EXIT;

      case Tok_Dir_Bounds:
         cdir_switches.bounds	= TRUE;
         ir_idx		 	= gen_directive_ir(Bounds_Cdir_Opr);
            
         if (LA_CH_VALUE != EOS) {
            parse_var_name_list(&opnd);
            COPY_OPND(IR_OPND_L(ir_idx), opnd);

            if (LA_CH_VALUE != EOS) {
               parse_err_flush(Find_EOS, EOS_STR);
            }
            NEXT_LA_CH; /* pick up EOS */
         }
         else {
            NEXT_LA_CH; /* pick up EOS */
         }

         goto EXIT;
   

      case Tok_Dir_Cache_Align:

         ir_idx = gen_directive_ir(Cachealign_Cdir_Opr);

         if (LA_CH_VALUE != EOS) {
            parse_cache_align_name_list(&opnd);
            COPY_OPND(IR_OPND_L(ir_idx), opnd);
         }
         else {
            parse_err_flush(Find_EOS, "IDENTIFIER");
         }
         break;


      case Tok_Dir_Cache_Bypass:
         ir_idx = gen_directive_ir(Cache_Bypass_Cdir_Opr);
         parse_cache_bypass_dir(&opnd);
         COPY_OPND(IR_OPND_L(ir_idx), opnd);
         goto EXIT;

      case Tok_Dir_Cache_Noalloc:
         parse_cache_noalloc();
         goto EXIT;


      case Tok_Dir_Cncall:

         /* this is duplicate code, taken from Tok_Mic_Cncall */

         ATP_HAS_TASK_DIRS(SCP_ATTR_IDX(curr_scp_idx)) = TRUE;
         ir_idx                          = gen_directive_ir(Cncall_Cmic_Opr);

         if (LA_CH_VALUE != EOS && LA_CH_VALUE != COMMA) {

            /* Arguments are specified on the CNCALL line.  Issue caution */
            /* message and ignore the arguments.  Because there is a list,*/
            /* cncall must be the only directive on the line, so flush.   */

            PRINTMSG(LA_CH_LINE, 1123, Caution, LA_CH_COLUMN);
            parse_err_flush(Find_EOS, NULL);
         }

         break;

      case Tok_Dir_Common:
         parse_common_dirs(Common);
         goto EXIT;


      case Tok_Dir_Concurrent:

         if (LA_CH_VALUE != EOS && LA_CH_VALUE != COMMA) {
            buf_idx	= LA_CH_BUF_IDX;
            stmt_num	= LA_CH_STMT_NUM;
   
            if (MATCHED_TOKEN_CLASS(Tok_Class_Id) &&
                TOKEN_LEN(token) == 13 &&
                strncmp("SAFE_DISTANCE", TOKEN_STR(token), 13) == IDENTICAL) {

               ir_idx = gen_directive_ir(Concurrent_Cdir_Opr);

               if (LA_CH_VALUE == EQUAL) {
                  NEXT_LA_CH;
                  
                  if (!parse_expr(&opnd)) {
                     parse_err_flush(Find_EOS, NULL);
                  }
                  else {
                     COPY_OPND(IR_OPND_L(ir_idx), opnd);
                  }
               }
               else {
                  parse_err_flush(Find_EOS, "=");
               }
            }
            else {
               reset_lex(buf_idx, stmt_num);
               parse_err_flush(Find_EOS, "SAFE_DISTANCE = ");
            }
         }
         else {
            ir_idx = gen_directive_ir(Concurrent_Cdir_Opr);
         }
         break;


      case Tok_Dir_Copy_Assumed_Shape:

         if (LA_CH_VALUE != EOS) {
            parse_copy_assumed_shape_dir();
         }
         else { /* set the global flag */
            SCP_COPY_ASSUMED_SHAPE(curr_scp_idx) = TRUE;

            if (SCP_COPY_ASSUMED_LIST(curr_scp_idx) == NULL_IDX) {
               NTR_IR_LIST_TBL(list_idx);
               IL_LINE_NUM(list_idx)			= TOKEN_LINE(token);
               IL_COL_NUM(list_idx)			= TOKEN_COLUMN(token);
               SCP_COPY_ASSUMED_LIST(curr_scp_idx)	= list_idx;
            }

            NEXT_LA_CH;          /* Pick up EOS */
         }

         goto EXIT;


      case Tok_Dir_Eject:

         if ((cif_flags & MISC_RECS) != 0) {
            cif_directive_rec(CIF_Eject, 
                              TOKEN_LINE(token),
                              TOKEN_COLUMN(token));
         }
         break;


      case Tok_Dir_Flow:
         cdir_switches.flow = TRUE;
         break;


      case Tok_Dir_Free:
      case Tok_Dir_Fixed:

         /* all semantics are done in src_input.c */
         /* context checks should be done here.   */

         parse_err_flush(Find_EOS, NULL);
         break;
   
   
      case Tok_Dir_Id:
         parse_id_directive();
         break;


      case Tok_Dir_Ignore_TKR:

         if (LA_CH_VALUE != EOS) {
            parse_ignore_tkr();
         }
         else { /* set the global flag */
            SCP_IGNORE_TKR(curr_scp_idx) = TRUE;
            NEXT_LA_CH;
         }

         goto EXIT;


      case Tok_Dir_Inline:
      case Tok_Dir_Inline_Always:
      case Tok_Dir_Inline_Never:

         if (opt_flags.inline_lvl == Inline_Lvl_0 && !dump_flags.preinline) {
            parse_err_flush(Find_EOS, NULL);
            break;
         }

         if (TOKEN_VALUE(token) == Tok_Dir_Inline) {
            cdir_switches.do_inline	= TRUE;
            ir_idx			= gen_directive_ir(Inline_Cdir_Opr);
         }
         else {
            parse_inline_always_never(TOKEN_VALUE(token) == 
                                      Tok_Dir_Inline_Always);
            goto EXIT;
         }
         break;
   

      case Tok_Dir_Ivdep:


#        if defined(_ACCEPT_VECTOR)

            /* On some non-vector platforms we accept IVDEP */

            if (!cdir_switches.vector) {
               parse_err_flush(Find_Comma, NULL);
               break;
            }
#        endif

         if (LA_CH_VALUE != EOS && LA_CH_VALUE != COMMA) {
            buf_idx	= LA_CH_BUF_IDX;
            stmt_num	= LA_CH_STMT_NUM;
   
            if (MATCHED_TOKEN_CLASS(Tok_Class_Id) &&
                TOKEN_LEN(token) == 6 &&
                strncmp("SAFEVL", TOKEN_STR(token), 6) == IDENTICAL) {

#              if defined(_TARGET_OS_MAX) || (defined(_TARGET_OS_IRIX) || defined(_TARGET_OS_LINUX)) || defined(_TARGET_OS_DARWIN)

                  /* If SAFEVL specified, issue warning and ignore ivdep */

                  PRINTMSG(TOKEN_LINE(token), 1317,Warning,TOKEN_COLUMN(token));
#              else
                  ir_idx = gen_directive_ir(Ivdep_Cdir_Opr);
#              endif

               if (LA_CH_VALUE == EQUAL) {
                  NEXT_LA_CH;
                     
                  if (!parse_expr(&opnd)) {
                     parse_err_flush(Find_EOS, NULL);
                  }
                  else {

#                    if !defined(_TARGET_OS_MAX) && !(defined(_TARGET_OS_IRIX) || defined(_TARGET_OS_LINUX)) || defined(_TARGET_OS_DARWIN)
                        COPY_OPND(IR_OPND_L(ir_idx), opnd);
#                    endif
                  }
               }
               else {
                  parse_err_flush(Find_EOS, "=");
               }
            }
            else {
               reset_lex(buf_idx,stmt_num);

#              if !defined(_TARGET_OS_MAX)
                  parse_err_flush(Find_EOS, "SAFEVL = ");
#              endif
            }
         }
         else {
            ir_idx = gen_directive_ir(Ivdep_Cdir_Opr);
         }
         break;

   
      case Tok_Dir_List:

         if ((cif_flags & MISC_RECS) != 0) {
            cif_directive_rec(CIF_List, 
                              TOKEN_LINE(token),
                              TOKEN_COLUMN(token));
         }
         break;


      case Tok_Dir_Mark:

         if (!opt_flags.mark) {
            parse_err_flush(Find_Comma, NULL);
            break;
         }

         cdir_switches.mark	= TRUE;
         ir_idx			= gen_directive_ir(Mark_Cdir_Opr);

         if (LA_CH_VALUE == EQUAL) {
            NEXT_LA_CH;

            if (!MATCHED_TOKEN_CLASS(Tok_Class_Id)) {
               parse_err_flush(Find_EOS, NULL);
               NEXT_LA_CH;
            }
            else {
               CLEAR_TBL_NTRY(type_tbl, TYP_WORK_IDX);
               TYP_CHAR_CLASS(TYP_WORK_IDX)	= Const_Len_Char;
               TYP_TYPE(TYP_WORK_IDX)	= Character;
               TYP_LINEAR(TYP_WORK_IDX)	= CHARACTER_DEFAULT_TYPE;
               TYP_FLD(TYP_WORK_IDX)	= CN_Tbl_Idx;
               TYP_IDX(TYP_WORK_IDX) 	= C_INT_TO_CN(CG_INTEGER_DEFAULT_TYPE,
                                                      TOKEN_LEN(token));
               type_idx			= ntr_type_tbl();
               IR_LINE_NUM_L(ir_idx)	= TOKEN_LINE(token);
               IR_COL_NUM_L(ir_idx)	= TOKEN_COLUMN(token);
               IR_FLD_L(ir_idx)		= CN_Tbl_Idx;
               IR_IDX_L(ir_idx)		= ntr_const_tbl(type_idx,
                                                        FALSE,
                                       (long_type *) &(TOKEN_ID(token).words));
            }
         }
         else {
            IR_FLD_L(ir_idx)		= CN_Tbl_Idx;
            IR_IDX_L(ir_idx)		= cdir_switches.mark_cmdline_idx;
            IR_LINE_NUM_L(ir_idx)	= TOKEN_LINE(token);
            IR_COL_NUM_L(ir_idx)	= TOKEN_COLUMN(token);
         }
         break;

      case Tok_Dir_Modinline:
      case Tok_Dir_Nomodinline:

         if (!opt_flags.modinline) {
            parse_err_flush(Find_Comma, NULL);
         }
         else if (ATP_PGM_UNIT(SCP_ATTR_IDX(MAIN_SCP_IDX)) != Module) {
            PRINTMSG(TOKEN_LINE(token), 1169, Warning, TOKEN_COLUMN(token));
         }
         else {
            ATP_MAY_INLINE(SCP_ATTR_IDX(curr_scp_idx)) =
                                    TOKEN_VALUE(token) == Tok_Dir_Modinline;
         }
         break;


      case Tok_Dir_Name:
         parse_name_dir();
         goto EXIT;


      case Tok_Dir_Nextscalar:

         if (!cdir_switches.vector) {
            parse_err_flush(Find_Comma, NULL);
         }
         else {
            ir_idx = gen_directive_ir(Nextscalar_Cdir_Opr);
         }
         break;


      case Tok_Dir_Nobl:

         if (opt_flags.scalar_lvl == Scalar_Lvl_0  || !opt_flags.bottom_load) {
            parse_err_flush(Find_Comma, NULL);
         }
         else {
            cdir_switches.bl	= FALSE;
            ir_idx		= gen_directive_ir(Nobl_Cdir_Opr);
         }
         break;

      case Tok_Dir_Noblocking:
         ir_idx = gen_directive_ir(Noblocking_Dir_Opr);
         break;

      case Tok_Dir_Nobounds:

         cdir_switches.bounds	= FALSE;
         ir_idx			= gen_directive_ir(Nobounds_Cdir_Opr);

         if (LA_CH_VALUE != EOS) {
            parse_var_name_list(&opnd);
            COPY_OPND(IR_OPND_L(ir_idx), opnd);

            if (LA_CH_VALUE != EOS) {
               parse_err_flush(Find_EOS, EOS_STR);
            }
            NEXT_LA_CH; /* pick up EOS */
         }
         else {
            NEXT_LA_CH; /* pick up EOS */
         }

         goto EXIT;

   
      case Tok_Dir_Noflow:
         cdir_switches.flow = FALSE;
         break;


      case Tok_Dir_Noinline:

         if (opt_flags.inline_lvl == Inline_Lvl_0 && !dump_flags.preinline) {
            parse_err_flush(Find_Comma, NULL);
         }
         else {
            cdir_switches.do_inline	= FALSE;
            ir_idx			= gen_directive_ir(Noinline_Cdir_Opr);
         }
         break;


      case Tok_Dir_Nointerchange:

         /* Use the same operator for both the MIPS and Cray versions. */

         ir_idx	     = gen_directive_ir(Nointerchange_Dir_Opr);
         break;


      case Tok_Dir_Nolist:

         if ((cif_flags & MISC_RECS) != 0) {
            cif_directive_rec(CIF_Nolist, 
                              TOKEN_LINE(token),
                              TOKEN_COLUMN(token));
         }
         break;


      case Tok_Dir_Nomark:

         if (opt_flags.mark) {
            cdir_switches.mark	= FALSE;
            ir_idx		= gen_directive_ir(Nomark_Cdir_Opr);
         }
         else {
            parse_err_flush(Find_Comma, NULL);
         }
         break;


      case Tok_Dir_Nopattern:

         if (!opt_flags.pattern) {
            parse_err_flush(Find_Comma, NULL);
         }
         else {
            cdir_switches.pattern	= FALSE;
            ir_idx			= gen_directive_ir(Nopattern_Cdir_Opr);
         }
         break;


      case Tok_Dir_Norecurrence:

         if (!opt_flags.recurrence) {
            parse_err_flush(Find_Comma, NULL);
         }
         else {
            cdir_switches.recurrence = FALSE;
            ir_idx	     	  = gen_directive_ir(Norecurrence_Cdir_Opr);
         }
         break;


      case Tok_Dir_Nosideeffects:
         parse_nosideeffects_dir();
         goto EXIT;
   
   
      case Tok_Dir_Nosplit:

         if (opt_flags.split_lvl == Split_Lvl_0) {
            parse_err_flush(Find_Comma, NULL);
         }
         else {
            ir_idx = gen_directive_ir(Nosplit_Cdir_Opr);
         }
         break;


      case Tok_Dir_Nostream:

         if (opt_flags.stream_lvl == Stream_Lvl_0) {
            parse_err_flush(Find_Comma, NULL);
         }
         else {
            cdir_switches.stream	= FALSE;
            ir_idx			= gen_directive_ir(Nostream_Dir_Opr);
         }
         break;


      case Tok_Dir_Notask:

# if !(defined(_TARGET_OS_IRIX) || defined(_TARGET_OS_LINUX) || defined(_TARGET_OS_DARWIN))
         if (opt_flags.task_lvl == Task_Lvl_0) {
            parse_err_flush(Find_Comma, NULL);
            break;
         }
# endif

         cdir_switches.task = FALSE;
         cdir_switches.notask_region = TRUE;

         /* check block stack for containing do loops */

         blk_idx = blk_stk_idx;

         while (BLK_TYPE(blk_idx) >= Do_Blk && blk_idx > 0) {

            if (BLK_TYPE(blk_idx) == Do_Blk) {
               ATL_NOTASK(BLK_TOP_LBL_IDX(blk_idx)) = TRUE;
            }

            blk_idx--;
         }

         ir_idx = gen_directive_ir(Notask_Cdir_Opr);
         break;


      case Tok_Dir_Nounroll:

         if (opt_flags.unroll_lvl == Unroll_Lvl_0) {
            parse_err_flush(Find_Comma, NULL);
         }
         else {
            ir_idx = gen_directive_ir(Nounroll_Cdir_Opr);
         }
         break;


      case Tok_Dir_Novector:

         if (opt_flags.vector_lvl == Vector_Lvl_0) {
            parse_err_flush(Find_Comma, NULL);
            break;
         }

         cdir_switches.vector = FALSE;

         /* check block stack for containing do loops */

         blk_idx = blk_stk_idx;

         while (BLK_TYPE(blk_idx) >= Do_Blk && blk_idx > 0) {

            if (BLK_TYPE(blk_idx) == Do_Blk) {
               ATL_NOVECTOR(BLK_TOP_LBL_IDX(blk_idx)) = TRUE;
            }

            blk_idx--;
         }
   
         ir_idx = gen_directive_ir(Novector_Cdir_Opr);
         break;


      case Tok_Dir_Novsearch:

         if (!opt_flags.vsearch || !cdir_switches.vector) {
            parse_err_flush(Find_Comma, NULL);
         }
         else {
            cdir_switches.vsearch = FALSE;
            ir_idx		  = gen_directive_ir(Novsearch_Cdir_Opr);
         }
         break;


      case Tok_Dir_Numcpus:

         /* this is duplicate code, taken from Tok_Mic_Numcpus */

         ATP_HAS_TASK_DIRS(SCP_ATTR_IDX(curr_scp_idx)) = TRUE;
         ir_idx                          = gen_directive_ir(Numcpus_Cmic_Opr);

         if (LA_CH_VALUE != LPAREN) {  /* Expected value */
            PRINTMSG(LA_CH_LINE, 1124, Error, LA_CH_COLUMN);
            parse_err_flush(Find_EOS, NULL);
         }
         else {
            NEXT_LA_CH;
            parse_expr(&opnd);
            COPY_OPND(IR_OPND_L(ir_idx), opnd);

            if (LA_CH_VALUE != RPAREN) {
               parse_err_flush(Find_EOS, ")");
            }
            else {
               NEXT_LA_CH;  /* Pick up Rparen. */
            }

# if (defined(_TARGET_OS_IRIX) || defined(_TARGET_OS_LINUX) || defined(_TARGET_OS_DARWIN))
            /* turn this into a call */

            COPY_OPND(IR_OPND_R(ir_idx), IR_OPND_L(ir_idx));

            if (glb_tbl_idx[Set_Numthreads_Attr_Idx] == NULL_IDX) {
               glb_tbl_idx[Set_Numthreads_Attr_Idx] = create_lib_entry_attr(
                                                        SET_NUMTHREADS_ENTRY,
                                                        SET_NUMTHREADS_NAME_LEN,
                                                        IR_LINE_NUM(ir_idx),
                                                        IR_COL_NUM(ir_idx));
            }

            ADD_ATTR_TO_LOCAL_LIST(glb_tbl_idx[Set_Numthreads_Attr_Idx]);

            IR_FLD_L(ir_idx) = AT_Tbl_Idx;
            IR_IDX_L(ir_idx) = glb_tbl_idx[Set_Numthreads_Attr_Idx];
            IR_LINE_NUM_L(ir_idx) = IR_LINE_NUM(ir_idx);
            IR_COL_NUM_L(ir_idx) = IR_COL_NUM(ir_idx);

            NTR_IR_TBL(cvrt_idx);
            IR_OPR(cvrt_idx) = Cvrt_Opr;
            IR_TYPE_IDX(cvrt_idx) = Integer_4;
            IR_LINE_NUM(cvrt_idx) = IR_LINE_NUM(ir_idx);
            IR_COL_NUM(cvrt_idx)  = IR_COL_NUM(ir_idx);

            COPY_OPND(IR_OPND_L(cvrt_idx), IR_OPND_R(ir_idx));

            NTR_IR_LIST_TBL(list_idx);
            IR_FLD_R(ir_idx) = IL_Tbl_Idx;
            IR_IDX_R(ir_idx) = list_idx;
            IR_LIST_CNT_R(ir_idx) = 1;
            IL_FLD(list_idx) = IR_Tbl_Idx;
            IL_IDX(list_idx) = cvrt_idx;

            SH_STMT_TYPE(curr_stmt_sh_idx) = Call_Stmt;
            IR_OPR(ir_idx) = Call_Opr;
# endif
         }

         break;


      case Tok_Dir_Pattern:

         if (!opt_flags.pattern) {
            parse_err_flush(Find_Comma, NULL);
         }
         else {
            cdir_switches.pattern	= TRUE;
            ir_idx			= gen_directive_ir(Pattern_Cdir_Opr);
         }
         break;

     case Tok_Dir_Permutation:

         /* this is duplicate code, taken from Tok_Mic_Permutation */

         /* ATP_HAS_TASK_DIRS(SCP_ATTR_IDX(curr_scp_idx)) = TRUE; */
         ir_idx = gen_directive_ir(Permutation_Cmic_Opr);
         parse_permutation_mic();
         break;


      case Tok_Dir_Preferstream:

         if (!cdir_switches.stream) {
            parse_err_flush(Find_Comma, NULL);
         }
         else {
            cdir_switches.preferstream		= TRUE;
            cdir_switches.preferstream_nocinv	= FALSE;
            opr					= Preferstream_Dir_Opr;

            if (LA_CH_VALUE != EOS && LA_CH_VALUE != COMMA) {

               if (MATCHED_TOKEN_CLASS(Tok_Class_Dir_Kwd) &&
                   TOKEN_VALUE(token) == Tok_Dir_Nocinv) {
                  cdir_switches.preferstream_nocinv = TRUE;
                  opr	= Preferstream_Nocinv_Dir_Opr;
               }
               else {
                  parse_err_flush(Find_EOS, "NOCINV");
               }
            }
            ir_idx = gen_directive_ir(opr);
         }
         break;


      case Tok_Dir_Prefertask:

#        if !(defined(_TARGET_OS_IRIX) || defined(_TARGET_OS_LINUX) || defined(_TARGET_OS_DARWIN))

            if (!cdir_switches.task) {
               parse_err_flush(Find_Comma, NULL);
               break;
            }
#        endif
   
         cdir_switches.prefertask = TRUE;
         ir_idx			  = gen_directive_ir(Prefertask_Cdir_Opr);

#        if (defined(_TARGET_OS_IRIX) || defined(_TARGET_OS_LINUX) || defined(_TARGET_OS_DARWIN))
            IR_OPR(ir_idx)	= Assert_Star_Opr;
            IR_FLD_L(ir_idx)	= CN_Tbl_Idx;
            IR_IDX_L(ir_idx)	= C_INT_TO_CN(CG_INTEGER_DEFAULT_TYPE,
                                              ASSERT_DOPREFER);
            IR_LINE_NUM_L(ir_idx)	= IR_LINE_NUM(ir_idx);
            IR_COL_NUM_L(ir_idx)	= IR_COL_NUM(ir_idx);

            IR_FLD_R(ir_idx)	= CN_Tbl_Idx;
            IR_IDX_R(ir_idx)	= C_INT_TO_CN(CG_INTEGER_DEFAULT_TYPE,
                                              DOPREFER_CONCURRENT);
            IR_LINE_NUM_R(ir_idx)	= IR_LINE_NUM(ir_idx);
            IR_COL_NUM_R(ir_idx)	= IR_COL_NUM(ir_idx);
#        endif
         break;


      case Tok_Dir_Prefervector:

         if (!cdir_switches.vector) {
            parse_err_flush(Find_Comma, NULL);
         }
         else {
            cdir_switches.prefervector = TRUE;
            ir_idx = gen_directive_ir(Prefervector_Cdir_Opr);
         }
         break;


      case Tok_Dir_Recurrence:

         if (LA_CH_VALUE != EOS && LA_CH_VALUE != COMMA) {
            buf_idx	= LA_CH_BUF_IDX;
            stmt_num	= LA_CH_STMT_NUM;
   
            if (LA_CH_VALUE == '1') {
               NEXT_LA_CH;

               if (LA_CH_VALUE == '2') {
                  NEXT_LA_CH;

                  if (LA_CH_VALUE == '8') {
                     NEXT_LA_CH;

                     if (LA_CH_VALUE == EOS) {
                        PRINTMSG(TOKEN_LINE(token), 801, Warning,
                                 TOKEN_COLUMN(token));
                        parse_err_flush(Find_EOS, NULL);
                        break;
                     }
                  }
               }
            }
            reset_lex(buf_idx,stmt_num);
         }
     
         if (!opt_flags.recurrence) {
            parse_err_flush(Find_Comma, NULL);
         }
         else {
            cdir_switches.recurrence	= TRUE;
            ir_idx			= gen_directive_ir(Recurrence_Cdir_Opr);
         }
         break;


      case Tok_Dir_Shortloop:

         if (!cdir_switches.vector) {
            parse_err_flush(Find_Comma, NULL);
            break;
         }
   
         ir_idx = gen_directive_ir(Shortloop_Cdir_Opr);

         if (LA_CH_VALUE != EOS    && LA_CH_VALUE != COMMA) {
            buf_idx	= LA_CH_BUF_IDX;
            stmt_num	= LA_CH_STMT_NUM;
   
            if (LA_CH_VALUE == '1') {
               NEXT_LA_CH;

               if (LA_CH_VALUE == '2') {
                  NEXT_LA_CH;

                  if (LA_CH_VALUE == '8') {
                     NEXT_LA_CH;

                     if (LA_CH_VALUE == EOS) {

#                       if defined(_ACCEPT_DIR_SHORTLOOP128)
                           IR_OPR(ir_idx)		= Shortloop128_Cdir_Opr;
                           cdir_switches.shortloop128	= TRUE;
#	                else
                           PRINTMSG(TOKEN_LINE(token), 801, Warning, 
                                    TOKEN_COLUMN(token));
#                       endif
                        break;
                     }
                  }
               }
            }
            reset_lex(buf_idx,stmt_num);
            parse_err_flush(Find_EOS, "128 or "EOS_STR);
         }
         else {
            cdir_switches.shortloop = TRUE;
         }
         break;


      case Tok_Dir_Split:

         if (opt_flags.split_lvl == Split_Lvl_0) {
            parse_err_flush(Find_Comma, NULL);
         }
         else {
            ir_idx = gen_directive_ir(Split_Cdir_Opr);
         }
         break;


      case Tok_Dir_Stack:

         if (CURR_BLK == Interface_Body_Blk || CURR_BLK == Interface_Blk) {

            /* Illegal to specify directive in an interface */

            PRINTMSG(TOKEN_LINE(token), 1404, Warning, TOKEN_COLUMN(token));
            parse_err_flush(Find_EOS, NULL);
            break;
         }

         ATP_STACK_DIR(SCP_ATTR_IDX(curr_scp_idx))	= TRUE;

         if (ATP_PGM_UNIT(SCP_ATTR_IDX(curr_scp_idx)) == Module) {

            /* Illegal to specify directive in a MODULE */
   
            PRINTMSG(TOKEN_LINE(token), 1405, Warning, TOKEN_COLUMN(token));
         }

         if (ATP_SAVE_ALL(SCP_ATTR_IDX(curr_scp_idx))) {

            /* A SAVE with no save entity list has been specified in this */
            /* program unit.  SAVE overrides STACK.  Issue warning.       */

            PRINTMSG(TOKEN_LINE(token), 1144, Warning, TOKEN_COLUMN(token),
                     "STACK");
            ATP_STACK_DIR(SCP_ATTR_IDX(curr_scp_idx))	= FALSE;
         }
         break;


      case Tok_Dir_Stream:

         if (opt_flags.stream_lvl > Stream_Lvl_0) {
            cdir_switches.stream	= TRUE;
            ir_idx			= gen_directive_ir(Stream_Dir_Opr);
         }
         break;


      case Tok_Dir_Suppress:

         ir_idx			= gen_directive_ir(Suppress_Opr);
         IR_LIST_CNT_L(ir_idx)	= 0;

         if (LA_CH_VALUE != EOS) {
            parse_dir_var_list();
         }
         else {
            NEXT_LA_CH;  /* pick up EOS */
         }

         label_idx			= gen_internal_lbl(stmt_start_line);
         IR_FLD_R(ir_idx)		= AT_Tbl_Idx;
         IR_IDX_R(ir_idx)		= label_idx;
         IR_LINE_NUM_R(ir_idx)		= stmt_start_line;
         IR_COL_NUM_R(ir_idx)		= stmt_start_col;
         AT_DEFINED(label_idx)		= TRUE;
         ATL_DEF_STMT_IDX(label_idx)	= curr_stmt_sh_idx;
         goto EXIT;


      case Tok_Dir_Symmetric:

         if (ATP_PGM_UNIT(SCP_ATTR_IDX(curr_scp_idx)) == Module) {

            /* Illegal to specify directive in a MODULE */

            PRINTMSG(TOKEN_LINE(token), 1233, Error, TOKEN_COLUMN(token),
                     "SYMMETRIC");
            parse_err_flush(Find_EOS, NULL);
            break;
         }

         if (LA_CH_VALUE == EOS) {
            ATP_SYMMETRIC(SCP_ATTR_IDX(curr_scp_idx)) = TRUE;
         }
         else {
            parse_symmetric_dir();
            goto EXIT;
         }
         break;


      case Tok_Dir_System_Module:

         if (ATP_PGM_UNIT(SCP_ATTR_IDX(curr_scp_idx)) != Module) {
            PRINTMSG(TOKEN_LINE(token), 1508, Error,
                     TOKEN_COLUMN(token), "SYSTEM_MODULE");
         }
         else {
            ATP_SYSTEM_MODULE(SCP_ATTR_IDX(curr_scp_idx))	= TRUE;
            SCP_IMPL_NONE(curr_scp_idx)				= TRUE;
         }
         break;


      case Tok_Dir_Task:

# if !(defined(_TARGET_OS_IRIX) || defined(_TARGET_OS_LINUX) || defined(_TARGET_OS_DARWIN))
         if (opt_flags.task_lvl == Task_Lvl_0) {
            parse_err_flush(Find_Comma, NULL);
         }
         else {
            cdir_switches.task	= TRUE;
            ir_idx		= gen_directive_ir(Task_Cdir_Opr);
         }
# else
         cdir_switches.task	= TRUE;
         cdir_switches.notask_region	= FALSE;
         ir_idx		= gen_directive_ir(Task_Cdir_Opr);
# endif
         break;


      case Tok_Dir_Taskcommon:
         parse_common_dirs(Task_Common);
         goto EXIT;


      case Tok_Dir_Unroll:

         if (opt_flags.unroll_lvl == Unroll_Lvl_0) {
            parse_err_flush(Find_Comma, NULL);
            break;
         }

         /* If count is zero, the optimizer does automatic unrolling */

         ir_idx			= gen_directive_ir(Unroll_Cdir_Opr);
         IR_LINE_NUM_L(ir_idx)	= TOKEN_LINE(token);
         IR_COL_NUM_L(ir_idx)	= TOKEN_COLUMN(token);
         IR_FLD_L(ir_idx)	= NO_Tbl_Idx;
         IR_IDX_L(ir_idx)	= NULL_IDX;

         if (LA_CH_VALUE != EOS) {

            if (!parse_expr(&opnd)) {
               parse_err_flush(Find_EOS, NULL);
            }
            else {
               COPY_OPND(IR_OPND_L(ir_idx), opnd);
            }
         }
         break;


      case Tok_Dir_Uses_Eregs:

         if (CURR_BLK == Interface_Body_Blk || CURR_BLK == Interface_Blk) {

            /* Illegal to specify directive in an interface */

            PRINTMSG(TOKEN_LINE(token), 1404, Warning, TOKEN_COLUMN(token));
            parse_err_flush(Find_EOS, NULL);
            break;
         }

         ATP_USES_EREGS(SCP_ATTR_IDX(curr_scp_idx))	= TRUE;

         if (ATP_PGM_UNIT(SCP_ATTR_IDX(curr_scp_idx)) == Module) {

            /* Illegal to specify directive in a MODULE */

            PRINTMSG(TOKEN_LINE(token), 1405, Warning, TOKEN_COLUMN(token));
         }
         break;


      case Tok_Dir_Vector:

         if (opt_flags.vector_lvl == Vector_Lvl_0) {
            parse_err_flush(Find_Comma, NULL);
         }
         else {
            cdir_switches.vector = TRUE;
            ir_idx		 = gen_directive_ir(Vector_Cdir_Opr);
         }
         break;


      case Tok_Dir_Vfunction:
         parse_vfunction_dir();
         goto EXIT;


      case Tok_Dir_Vsearch:

         if (!opt_flags.vsearch || !cdir_switches.vector) {
            parse_err_flush(Find_Comma, NULL);
         }
         else {
            cdir_switches.vsearch	= TRUE;
            ir_idx			= gen_directive_ir(Vsearch_Cdir_Opr);
         }
         break;

     /* Craft sprs - unsupported - skip if -xmpp specified. */

      case Tok_Dir_Doshared:
      case Tok_Dir_Endmaster:
      case Tok_Dir_Geometry:
      case Tok_Dir_Parallel_Only:
      case Tok_Dir_Pe_Resident:
      case Tok_Dir_Pe_Private:
      case Tok_Dir_Serial_Only:
      case Tok_Dir_Shared:
      case Tok_Dir_Unknown:
      case Tok_Dir_Unknown_Shared:
            parse_err_flush(Find_EOS, NULL);  /* Flush - has comma list */

            /* Fall through */

      case Tok_Dir_Atomicupdate:
      case Tok_Dir_Barrier:
      case Tok_Dir_Critical:
      case Tok_Dir_Endcritical:
      case Tok_Dir_Master:
      case Tok_Dir_Nobarrier:

         if (!cmd_line_flags.disregard_all_mpp_cdirs) {
            PRINTMSG(TOKEN_LINE(token), 801, Warning, TOKEN_COLUMN(token));
            parse_err_flush(Find_EOS, NULL);
         }

         break;

# ifdef _DEBUG

      case Tok_Dbg_Sytb:
         SCP_DBG_PRINT_SYTB(curr_scp_idx) = TRUE;
         break;

      case Tok_Dbg_Stmt:
         SCP_DBG_PRINT_STMT(curr_scp_idx) = TRUE;
         break;
# endif

      default:

         /* Intentionally blank */
         break;

      }  /* end switch */

CONTINUE:

      if (LA_CH_VALUE == COMMA) {
         NEXT_LA_CH;

         if (!MATCHED_TOKEN_CLASS(Tok_Class_Dir_Kwd)) {
            PRINTMSG(TOKEN_LINE(token), 1356, Warning, TOKEN_COLUMN(token));
            parse_err_flush(Find_EOS, NULL);
            NEXT_LA_CH;
            break;
         }
      }
      else {
         break;
      }
   }  /* End for */

   /* Flush past all unimplemented dirs */

   if (LA_CH_VALUE != EOS) {
      PRINTMSG(LA_CH_LINE, 790, Warning, LA_CH_COLUMN);
      parse_err_flush(Find_EOS, NULL);
   }

   NEXT_LA_CH;

EXIT:

   TRACE (Func_Exit, "parse_dir_directives", NULL);

   return;

}  /* parse_dir_directives */

/******************************************************************************\
|*									      *|
|* Description:								      *|
|*	This routine parses the !MIC$ directives on one line of source.       *|
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
static void parse_mic_directives(void)

{
   int		ir_idx;
   boolean	ok		= TRUE;
   opnd_type	opnd;

# if (defined(_TARGET_OS_IRIX) || defined(_TARGET_OS_LINUX) || defined(_TARGET_OS_DARWIN))
   int		cvrt_idx;
   int		list_idx;
# endif

# if (_ACCEPT_MIC_SEND)
   int		blk_idx;
   int		column;
   int		do_blk_idx;
   boolean	found_do;
   int		line;
   opnd_type	point_opnd;
# endif


   TRACE (Func_Entry, "parse_mic_directives", NULL);

   for (;;) {

      if (TOKEN_VALUE(token) > Tok_Mic_Start &&
          TOKEN_VALUE(token) < Tok_Mic_End &&
          disregard_mics[TOKEN_VALUE(token) - Tok_Mic_Start]) {
    
         switch (TOKEN_VALUE(token)) {

            case Tok_Mic_Cncall:
            case Tok_Mic_Guard:
            case Tok_Mic_End_Guard:
            case Tok_Mic_Numcpus:
            case Tok_Mic_Permutation:
            case Tok_Mic_Send:
            case Tok_Mic_Wait:
               parse_err_flush(Find_EOS, NULL);
               break;

            default:
               parse_err_flush(Find_Comma, NULL);
         }
         
         goto CONTINUE;
      }

      switch (TOKEN_VALUE(token)) {

      case Tok_Mic_Case:

         ATP_HAS_TASK_DIRS(SCP_ATTR_IDX(curr_scp_idx)) = TRUE;
         ir_idx				 = gen_directive_ir(Case_Cmic_Opr);

         if (! cdir_switches.parallel_region) {
            /* error .. not in parallel region */
            PRINTMSG(IR_LINE_NUM(ir_idx), 785, Error, IR_COL_NUM(ir_idx),
                     "CASE");
         }
         else {

            if (remove_do_parallel_blk(TRUE, "CASE", IR_LINE_NUM(ir_idx),
                                       IR_COL_NUM(ir_idx))) {
            }

            SH_STMT_TYPE(curr_stmt_sh_idx) = Parallel_Case_Stmt;
            stmt_type = Parallel_Case_Stmt;

            if (cdir_switches.casedir) {
               end_parallel_case_blk(FALSE);
            }

            SET_DIRECTIVE_STATE(Case_Region);
            cdir_switches.casedir = TRUE;

            PUSH_BLK_STK (Parallel_Case_Blk);
            BLK_IS_PARALLEL_REGION(blk_stk_idx)	= TRUE;
   
            CURR_BLK_FIRST_SH_IDX     = curr_stmt_sh_idx;
            LINK_TO_PARENT_BLK;
         }

         break;


      case Tok_Mic_End_Case:

         ATP_HAS_TASK_DIRS(SCP_ATTR_IDX(curr_scp_idx)) = TRUE;
         ir_idx				 = gen_directive_ir(Endcase_Cmic_Opr);

         if (! cdir_switches.parallel_region) {
            /* error .. not in parallel region */
            PRINTMSG(IR_LINE_NUM(ir_idx), 785, Error, IR_COL_NUM(ir_idx),
                     "END CASE");
         }
         else {

            cdir_switches.casedir = FALSE;
            SH_STMT_TYPE(curr_stmt_sh_idx) = End_Parallel_Case_Stmt;
            stmt_type = End_Parallel_Case_Stmt;
   
            end_parallel_case_blk(FALSE);
         }

         CLEAR_DIRECTIVE_STATE(Case_Region);
         break;


      case Tok_Mic_Cncall:

         /* this code is duplicated for Tok_Dir_Cncall */

         ATP_HAS_TASK_DIRS(SCP_ATTR_IDX(curr_scp_idx)) = TRUE;
         ir_idx				 = gen_directive_ir(Cncall_Cmic_Opr);

         if (LA_CH_VALUE != EOS && LA_CH_VALUE != COMMA) {

            /* Arguments are specified on the CNCALL line.  Issue caution */
            /* message and ignore the arguments.  Because there is a list,*/
            /* cncall must be the only directive on the line, so flush.   */

            PRINTMSG(LA_CH_LINE, 1123, Caution, LA_CH_COLUMN);
            parse_err_flush(Find_EOS, NULL);
         }

         break;


      case Tok_Mic_Do_All:

         ATP_HAS_TASK_DIRS(SCP_ATTR_IDX(curr_scp_idx)) = TRUE;
         ir_idx				 = gen_directive_ir(Doall_Cmic_Opr);

         parse_doall_cmic();

         if (cdir_switches.parallel_region ||
             cdir_switches.guard_in_par_reg) {
            /* error .. already parallel region */
            PRINTMSG(IR_LINE_NUM(ir_idx), 814, Error, IR_COL_NUM(ir_idx));
         }
         else {
            SET_DIRECTIVE_STATE(Doall_Region);
            cdir_switches.doall_sh_idx = curr_stmt_sh_idx;
         }

         break;


      case Tok_Mic_Do_Parallel:

         ATP_HAS_TASK_DIRS(SCP_ATTR_IDX(curr_scp_idx)) = TRUE;
         ir_idx = gen_directive_ir(Doparallel_Cmic_Opr);

         parse_doparallel_cmic();

         if (! cdir_switches.parallel_region) {
            /* error .. not in parallel region */
            PRINTMSG(IR_LINE_NUM(ir_idx), 785, Error, IR_COL_NUM(ir_idx),
                     "DO PARALLEL");
         }
         else if (cdir_switches.casedir) {
            /* error .. can't be inside parallel case */
            PRINTMSG(IR_LINE_NUM(ir_idx), 1312, Error, IR_COL_NUM(ir_idx));
         }
         else if (remove_do_parallel_blk(TRUE, "DO PARALLEL", 
                                   IR_LINE_NUM(ir_idx), IR_COL_NUM(ir_idx))) {
            /* error issued by remove_do_parallel_blk */
         }
         else {
            SET_DIRECTIVE_STATE(Do_Parallel_Region);
            cdir_switches.do_parallel = TRUE;
            cdir_switches.dopar_sh_idx = curr_stmt_sh_idx;
         }

         break;


      case Tok_Mic_End_Do:

         ATP_HAS_TASK_DIRS(SCP_ATTR_IDX(curr_scp_idx)) = TRUE;
         ir_idx				 = gen_directive_ir(Enddo_Cmic_Opr);

         if (! cdir_switches.parallel_region) {
            /* error .. not in parallel region */
            PRINTMSG(IR_LINE_NUM(ir_idx), 785, Error, IR_COL_NUM(ir_idx),
                     "END DO");
         }
         else {

            CLEAR_DIRECTIVE_STATE(Do_Parallel_Region);
            cdir_switches.do_parallel = FALSE;
            SH_STMT_TYPE(curr_stmt_sh_idx) = End_Do_Parallel_Stmt;
            stmt_type = End_Do_Parallel_Stmt;

            end_do_parallel_blk(FALSE);
         }

         break;


      case Tok_Mic_Guard:

         ATP_HAS_TASK_DIRS(SCP_ATTR_IDX(curr_scp_idx)) = TRUE;
         ir_idx				 = gen_directive_ir(Guard_Cmic_Opr);

         if (LA_CH_VALUE != EOS) {
            ok = parse_expr(&opnd);
            COPY_OPND(IR_OPND_L(ir_idx), opnd);
            cdir_switches.guard_has_flag = TRUE;

            if (LA_CH_VALUE != EOS) {
               parse_err_flush(Find_EOS,EOS_STR);
            }
         }
         else {
            cdir_switches.guard_has_flag = FALSE;
         }

         if (cdir_switches.guard) {
            /* error .. missing end guard */
            PRINTMSG(IR_LINE_NUM(ir_idx), 815, Error, IR_COL_NUM(ir_idx));
         }
         else {
            
            SET_DIRECTIVE_STATE(Guard_Region);
            cdir_switches.guard            = TRUE;
            cdir_switches.guard_in_par_reg = cdir_switches.parallel_region;
            cdir_switches.parallel_region  = FALSE;

            PUSH_BLK_STK (Guard_Blk);
            BLK_IS_PARALLEL_REGION(blk_stk_idx)	= TRUE;

            CURR_BLK_FIRST_SH_IDX     = curr_stmt_sh_idx;
            LINK_TO_PARENT_BLK;
         }

         break;


      case Tok_Mic_End_Guard:

         ATP_HAS_TASK_DIRS(SCP_ATTR_IDX(curr_scp_idx)) = TRUE;
         ir_idx				 = gen_directive_ir(Endguard_Cmic_Opr);

         ok = TRUE;

         if (LA_CH_VALUE != EOS) {
            ok = parse_expr(&opnd);
            COPY_OPND(IR_OPND_L(ir_idx), opnd);

            if (! cdir_switches.guard_has_flag) {
               /* error .. guards don't match */
               PRINTMSG(IR_LINE_NUM(ir_idx), 816, Error, IR_COL_NUM(ir_idx));
               ok = FALSE;
            }

            if (LA_CH_VALUE != EOS) {
               parse_err_flush(Find_EOS,EOS_STR);
            }
         }
         else if (cdir_switches.guard_has_flag) {
            /* error .. guards don't match */
            PRINTMSG(IR_LINE_NUM(ir_idx), 816, Error, IR_COL_NUM(ir_idx));
            ok = FALSE;
         }

         CLEAR_DIRECTIVE_STATE(Guard_Region);

         if (ok) {

            cdir_switches.guard = FALSE;
            cdir_switches.parallel_region = cdir_switches.guard_in_par_reg;
            cdir_switches.guard_in_par_reg = FALSE;
	
            SH_STMT_TYPE(curr_stmt_sh_idx) = End_Guard_Stmt;
            stmt_type = End_Guard_Stmt;

            end_guard_blk(FALSE);
         }

         break;
   
      case Tok_Mic_Numcpus:

         /* this code is duplicated for Tok_Dir_Numcpus */

         ATP_HAS_TASK_DIRS(SCP_ATTR_IDX(curr_scp_idx)) = TRUE;
         ir_idx				 = gen_directive_ir(Numcpus_Cmic_Opr);

         if (LA_CH_VALUE != LPAREN) {  /* Expected value */
            PRINTMSG(LA_CH_LINE, 1124, Error, LA_CH_COLUMN);
            parse_err_flush(Find_EOS, NULL);
         }
         else {
            NEXT_LA_CH;
            ok = parse_expr(&opnd);
            COPY_OPND(IR_OPND_L(ir_idx), opnd);

            if (LA_CH_VALUE != RPAREN) {
               parse_err_flush(Find_EOS, ")");
            }
            else {
               NEXT_LA_CH;  /* Pick up Rparen. */
            }

# if (defined(_TARGET_OS_IRIX) || defined(_TARGET_OS_LINUX) || defined(_TARGET_OS_DARWIN))
            /* turn this into a call */

            COPY_OPND(IR_OPND_R(ir_idx), IR_OPND_L(ir_idx));

            if (glb_tbl_idx[Set_Numthreads_Attr_Idx] == NULL_IDX) {
               glb_tbl_idx[Set_Numthreads_Attr_Idx] = create_lib_entry_attr(
                                                        SET_NUMTHREADS_ENTRY,
                                                        SET_NUMTHREADS_NAME_LEN,
                                                        IR_LINE_NUM(ir_idx),
                                                        IR_COL_NUM(ir_idx));
            }
         
            ADD_ATTR_TO_LOCAL_LIST(glb_tbl_idx[Set_Numthreads_Attr_Idx]);

            IR_FLD_L(ir_idx) = AT_Tbl_Idx;
            IR_IDX_L(ir_idx) = glb_tbl_idx[Set_Numthreads_Attr_Idx];
            IR_LINE_NUM_L(ir_idx) = IR_LINE_NUM(ir_idx);
            IR_COL_NUM_L(ir_idx) = IR_COL_NUM(ir_idx);

            NTR_IR_TBL(cvrt_idx);
            IR_OPR(cvrt_idx) = Cvrt_Opr;
            IR_TYPE_IDX(cvrt_idx) = Integer_4;
            IR_LINE_NUM(cvrt_idx) = IR_LINE_NUM(ir_idx);
            IR_COL_NUM(cvrt_idx)  = IR_COL_NUM(ir_idx);

            COPY_OPND(IR_OPND_L(cvrt_idx), IR_OPND_R(ir_idx));

            NTR_IR_LIST_TBL(list_idx);
            IR_FLD_R(ir_idx) = IL_Tbl_Idx;
            IR_IDX_R(ir_idx) = list_idx;
            IR_LIST_CNT_R(ir_idx) = 1;
            IL_FLD(list_idx) = IR_Tbl_Idx;
            IL_IDX(list_idx) = cvrt_idx;
            
            SH_STMT_TYPE(curr_stmt_sh_idx) = Call_Stmt;
            IR_OPR(ir_idx) = Call_Opr;
# endif
         }

         break;


      case Tok_Mic_Parallel:

         ATP_HAS_TASK_DIRS(SCP_ATTR_IDX(curr_scp_idx)) = TRUE;
         ir_idx				 = gen_directive_ir(Parallel_Cmic_Opr);

         parse_parallel_cmic();

         if (cdir_switches.parallel_region ||
             cdir_switches.guard_in_par_reg) {
            /* error .. already in a parallel_region */
            PRINTMSG(IR_LINE_NUM(ir_idx), 818, Error, IR_COL_NUM(ir_idx));
         }
         else {
            SET_DIRECTIVE_STATE(Parallel_Region);
            cdir_switches.parallel_region   = TRUE;
            PUSH_BLK_STK (Parallel_Blk);
            BLK_IS_PARALLEL_REGION(blk_stk_idx)	= TRUE;
            CURR_BLK_FIRST_SH_IDX     = curr_stmt_sh_idx;
            LINK_TO_PARENT_BLK;
         }

         break;


      case Tok_Mic_End_Parallel:

         ATP_HAS_TASK_DIRS(SCP_ATTR_IDX(curr_scp_idx)) = TRUE;
         ir_idx = gen_directive_ir(Endparallel_Cmic_Opr);

         CLEAR_DIRECTIVE_STATE(Parallel_Region);
         cdir_switches.parallel_region   = FALSE;
         cdir_switches.do_parallel       = FALSE;
         cdir_switches.guard_in_par_reg  = FALSE;

         SH_STMT_TYPE(curr_stmt_sh_idx) = End_Parallel_Stmt;
         stmt_type = End_Parallel_Stmt;
         end_parallel_blk(FALSE);

         break;


      case Tok_Mic_Permutation:

         /* this code is duplicated for Tok_Dir_Permutation */

         ATP_HAS_TASK_DIRS(SCP_ATTR_IDX(curr_scp_idx)) = TRUE;
         ir_idx	= gen_directive_ir(Permutation_Cmic_Opr);
         parse_permutation_mic();
         break;

      case Tok_Mic_Wait:

# if defined(_ACCEPT_MIC_WAIT)

         ATP_HAS_TASK_DIRS(SCP_ATTR_IDX(curr_scp_idx))	= TRUE;

         line				= TOKEN_LINE(token);
         column				= TOKEN_COLUMN(token);
         ir_idx				= gen_directive_ir(Wait_Cmic_Opr);
         OPND_LINE_NUM(opnd)		= LA_CH_LINE;
         OPND_COL_NUM(opnd)		= LA_CH_COLUMN;
         OPND_FLD(opnd)			= CN_Tbl_Idx;
         OPND_IDX(opnd)			= CN_INTEGER_ONE_IDX;
         OPND_LINE_NUM(point_opnd)	= LA_CH_LINE;
         OPND_COL_NUM(point_opnd)	= LA_CH_COLUMN;
         point_opnd			= null_opnd;

         if (LA_CH_VALUE == EOS) {

            /* Intentionally blank */
         }
         else if (MATCHED_TOKEN_CLASS(Tok_Class_Mic_Kwd)) {

            if (TOKEN_VALUE(token) == Tok_Mic_Point) {

               if (LA_CH_VALUE == LPAREN) {
                  NEXT_LA_CH;
                  ok = parse_expr(&point_opnd);

                  if (LA_CH_VALUE != RPAREN) {
                     parse_err_flush(Find_EOS, ")");
                  }
                  else {
                     NEXT_LA_CH;  /* Pick up Rparen. */
                  }

                  if (LA_CH_VALUE != EOS) {

                     if (!MATCHED_TOKEN_CLASS(Tok_Class_Mic_Kwd) ||
                         TOKEN_VALUE(token) != Tok_Mic_Span) {
                        parse_err_flush(Find_EOS, "SPAN or EOS");
                     }
                     else if (LA_CH_VALUE == LPAREN) {
                        NEXT_LA_CH;
                        ok = parse_expr(&opnd);
 
                        if (LA_CH_VALUE != RPAREN) {
                           parse_err_flush(Find_EOS, ")");
                        }
                        else {
                           NEXT_LA_CH;  /* Pick up Rparen. */
                        }

                        if (LA_CH_VALUE != EOS) {
                           parse_err_flush(Find_EOS, "EOS");
                        }
                     }
                     else {
                        parse_err_flush(Find_EOS, "(");
                     }
                  }
               }
               else {
                  parse_err_flush(Find_EOS, "(");
               }
            }
            else if (TOKEN_VALUE(token) == Tok_Mic_Span) {

               if (LA_CH_VALUE == LPAREN) {
                  NEXT_LA_CH;
                  ok = parse_expr(&opnd);

                  if (LA_CH_VALUE != RPAREN) {
                     parse_err_flush(Find_EOS, ")");
                  }
                  else {
                     NEXT_LA_CH;  /* Pick up Rparen. */
                  }

                  if (LA_CH_VALUE != EOS) {
                     parse_err_flush(Find_EOS, "EOS");
                  }
               }
               else {
                  parse_err_flush(Find_EOS, "(");
               }
            }
            else {
               parse_err_flush(Find_EOS, "POINT, SPAN or EOS");
            }
         }
         else {
            parse_err_flush(Find_EOS, "POINT, SPAN or EOS");
         }

         COPY_OPND(IR_OPND_L(ir_idx), point_opnd);
         COPY_OPND(IR_OPND_R(ir_idx), opnd);

         /* This directive must be specified within a doall or doparallel */
         /* region.  Search the block stack to make sure one exists.      */

         blk_idx	= blk_stk_idx;
         do_blk_idx	= NULL_IDX;
         found_do	= FALSE;

         while (blk_idx > 0) {

            if (BLK_TYPE(blk_idx) == Do_Parallel_Blk ||
                BLK_TYPE(blk_idx) == Doall_Blk) {
               do_blk_idx	= blk_idx;
               break;
            }

            if (BLK_TYPE(blk_idx) == Do_Blk) {
               found_do = TRUE;
            }

            if (BLK_TYPE(blk_idx) == Case_Blk || 
                BLK_TYPE(blk_idx) == Guard_Blk) {

               /* Issue error.  Wait cannot be specified within a CASE region */
               /* or a GUARD region.  Continue to check for doall/doparallel. */

               PRINTMSG(line, 1519, Error, column,
                        (BLK_TYPE(blk_idx) == Case_Blk) ? "CASE" : "GUARD");
            }
            blk_idx--;
         }

         if (do_blk_idx == NULL_IDX) {  /* Did not find the block */

            /* Issue error - Need to be in doparallel or doall region.*/

            PRINTMSG(line, 1520, Error, column, "WAIT");
         }

         if (!found_do) {  /* Issue error - Need to be in a do block. */
            PRINTMSG(line, 1385, Error, column, "WAIT");
         }

# else
         PRINTMSG(TOKEN_LINE(token), 801, Warning, TOKEN_COLUMN(token));
         parse_err_flush(Find_EOS, NULL);
# endif

         break;

      case Tok_Mic_Send:

# if (_ACCEPT_MIC_SEND)

         ATP_HAS_TASK_DIRS(SCP_ATTR_IDX(curr_scp_idx)) = TRUE;

         line	= TOKEN_LINE(token);
         column	= TOKEN_COLUMN(token);

         ir_idx				= gen_directive_ir(Send_Cmic_Opr);
         OPND_LINE_NUM(opnd)		= LA_CH_LINE;
         OPND_COL_NUM(opnd)		= LA_CH_COLUMN;
         opnd				= null_opnd;
         OPND_LINE_NUM(point_opnd)	= LA_CH_LINE;
         OPND_COL_NUM(point_opnd)	= LA_CH_COLUMN;
         point_opnd			= null_opnd;

         if (LA_CH_VALUE == EOS) {

            /* Intentionally blank */
         }
         else if (MATCHED_TOKEN_CLASS(Tok_Class_Mic_Kwd)) {

            if (TOKEN_VALUE(token) == Tok_Mic_Point) {

               if (LA_CH_VALUE == LPAREN) {
                  NEXT_LA_CH;
                  ok = parse_expr(&point_opnd);

                  if (LA_CH_VALUE != RPAREN) {
                     parse_err_flush(Find_EOS, ")");
                  }
                  else {
                     NEXT_LA_CH;  /* Pick up Rparen. */
                  }

                  if (LA_CH_VALUE != EOS) {

                     if (!MATCHED_TOKEN_CLASS(Tok_Class_Mic_Kwd) ||
                         TOKEN_VALUE(token) != Tok_Mic_If) {
                        reset_lex(TOKEN_BUF_IDX(token), TOKEN_STMT_NUM(token));
                        parse_err_flush(Find_EOS, "IF or EOS");
                     }
                     else if (LA_CH_VALUE == LPAREN) {
                        NEXT_LA_CH;
                        ok = parse_expr(&opnd);

                        if (LA_CH_VALUE != RPAREN) {
                           parse_err_flush(Find_EOS, ")");
                        }
                        else {
                           NEXT_LA_CH;  /* Pick up Rparen. */
                        }

                        if (LA_CH_VALUE != EOS) {
                           parse_err_flush(Find_EOS, "EOS");
                        }
                     }
                     else {
                        parse_err_flush(Find_EOS, "(");
                     }
                  }
               }
               else {
                  parse_err_flush(Find_EOS, "(");
               }
            }
            else if (TOKEN_VALUE(token) == Tok_Mic_If) {

               if (LA_CH_VALUE == LPAREN) {
                  NEXT_LA_CH;
                  ok = parse_expr(&opnd);

                  if (LA_CH_VALUE != RPAREN) {
                     parse_err_flush(Find_EOS, ")");
                  }
                  else {
                     NEXT_LA_CH;  /* Pick up Rparen. */
                  }

                  if (LA_CH_VALUE != EOS) {
                     parse_err_flush(Find_EOS, "EOS");
                  }
               }
               else {
                  parse_err_flush(Find_EOS, "(");
               }
            }
            else {
               parse_err_flush(Find_EOS, "POINT, IF or EOS");
            }
         }
         else {
            parse_err_flush(Find_EOS, "POINT, IF or EOS");
         }

         COPY_OPND(IR_OPND_L(ir_idx), point_opnd);
         COPY_OPND(IR_OPND_R(ir_idx), opnd);

         /* This directive must be specified within a doall or doparallel */
         /* region.  Search the block stack to make sure one exists.      */
         /* SENDS's should have a wait, but this is checking in case      */
         /* there is a SEND without a WAIT that is not in a doparallel    */

         blk_idx	= blk_stk_idx;
         do_blk_idx	= NULL_IDX;
         found_do	= FALSE;

         while (blk_idx > 0) {

            if (BLK_TYPE(blk_idx) == Do_Parallel_Blk ||
                BLK_TYPE(blk_idx) == Doall_Blk) {
               do_blk_idx	= blk_idx;
               break;
            }

            if (BLK_TYPE(blk_idx) == Do_Blk) {
               found_do = TRUE;
            }
            blk_idx--;
         }

         if (do_blk_idx == NULL_IDX) {  /* Did not find the block */

            /* Issue error - Need to be in doparallel or doall region.*/

            PRINTMSG(line, 1520, Error, column, "SEND");
         }

         if (!found_do) {  /* Issue error - Need to be in a do block. */
            PRINTMSG(line, 1385, Error, column, "SEND");
         }
      

# else
         PRINTMSG(TOKEN_LINE(token), 801, Warning, TOKEN_COLUMN(token));
         parse_err_flush(Find_EOS, NULL);
# endif

         break;


      case Tok_Mic_Continue:
      case Tok_Mic_Taskcommon:

         PRINTMSG(TOKEN_LINE(token), 801, Warning, TOKEN_COLUMN(token));
         parse_err_flush(Find_EOS, NULL);
         break;

      default:
         PRINTMSG(TOKEN_LINE(token), 790, Warning, TOKEN_COLUMN(token));
         parse_err_flush(Find_EOS, NULL);

      }  /* end switch */

CONTINUE:

      if (LA_CH_VALUE == COMMA) {
         NEXT_LA_CH;

         if (!MATCHED_TOKEN_CLASS(Tok_Class_Mic_Kwd)) {
            parse_err_flush(Find_EOS, NULL);
            NEXT_LA_CH;
            break;
         }
      }
      else {
         break;
      }
   }  /* End for */

   /* Flush past all unimplemented dirs */

   if (LA_CH_VALUE != EOS) {
      PRINTMSG(LA_CH_LINE, 790, Warning, LA_CH_COLUMN);
      parse_err_flush(Find_EOS, NULL);
   }

   NEXT_LA_CH;

   TRACE (Func_Exit, "parse_mic_directives", NULL);

   return;

}  /* parse_mic_directives */

/******************************************************************************\
|*									      *|
|* Description:								      *|
|*	This routine parses the C$PAR directives on one line of source.       *|
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
static void parse_par_directives(void)
{
   int		ir_idx;
   opnd_type	opnd;
   boolean	paren = FALSE;
   int		sh_idx;

   TRACE (Func_Entry, "parse_par_directives", NULL);

   if (TOKEN_VALUE(token) > Tok_SGI_Dir_Start &&
       TOKEN_VALUE(token) < Tok_SGI_Dir_End &&
       disregard_mips[TOKEN_VALUE(token) - Tok_SGI_Dir_Start]) {
      goto EXIT;
   }

   switch (TOKEN_VALUE(token)) {

   case Tok_SGI_Dir_Parallel:
      ATP_HAS_TASK_DIRS(SCP_ATTR_IDX(curr_scp_idx)) = TRUE;
      ir_idx = gen_directive_ir(Parallel_Par_Opr);

      parse_mp_directive(Parallel);

      if (directive_region_error(Sgi_Parallel_Dir,
                                 IR_LINE_NUM(ir_idx),
                                 IR_COL_NUM(ir_idx))) {
      }
      else {
         SET_DIRECTIVE_STATE(Sgi_Parallel_Region);
         PUSH_BLK_STK (SGI_Parallel_Blk);
         BLK_IS_PARALLEL_REGION(blk_stk_idx)	= TRUE;
         CURR_BLK_FIRST_SH_IDX     = curr_stmt_sh_idx;
         LINK_TO_PARENT_BLK;
      }

      break;

   case Tok_SGI_Dir_Paralleldo:
      ATP_HAS_TASK_DIRS(SCP_ATTR_IDX(curr_scp_idx)) = TRUE;
      ir_idx = gen_directive_ir(Parallel_Do_Par_Opr);

      parse_mp_directive(Parallel_Do);

      if (directive_region_error(Parallel_Do_Dir,
                                 IR_LINE_NUM(ir_idx),
                                 IR_COL_NUM(ir_idx))) {
      }
      else {
         SET_DIRECTIVE_STATE(Parallel_Do_Region);
         cdir_switches.paralleldo_sh_idx = curr_stmt_sh_idx;
      }

      break;

   case Tok_SGI_Dir_Pdo:
      ATP_HAS_TASK_DIRS(SCP_ATTR_IDX(curr_scp_idx)) = TRUE;
      ir_idx = gen_directive_ir(Pdo_Par_Opr);

      parse_mp_directive(Pdo);

      if (directive_region_error(Pdo_Dir,
                                 IR_LINE_NUM(ir_idx),
                                 IR_COL_NUM(ir_idx))) {
      }
      else {
         SET_DIRECTIVE_STATE(Pdo_Region);
         cdir_switches.pdo_sh_idx = curr_stmt_sh_idx;
      }

      break;

   case Tok_SGI_Dir_Barrier:

      ATP_HAS_TASK_DIRS(SCP_ATTR_IDX(curr_scp_idx)) = TRUE;
      ir_idx = gen_directive_ir(Barrier_Par_Opr);

      if (directive_region_error(Barrier_Dir,
                                 IR_LINE_NUM(ir_idx),
                                 IR_COL_NUM(ir_idx))) {
      }

      break;


   case Tok_SGI_Dir_Criticalsection:
      ATP_HAS_TASK_DIRS(SCP_ATTR_IDX(curr_scp_idx)) = TRUE;
      ir_idx = gen_directive_ir(Critical_Section_Par_Opr);

      if (LA_CH_VALUE != EOS) {

         if (LA_CH_VALUE == LPAREN) {
            paren = TRUE;
            NEXT_LA_CH;
         }

         if (MATCHED_TOKEN_CLASS(Tok_Class_Id)) {

            if (! parse_deref(&opnd, NULL_IDX)) {
               parse_err_flush(Find_Rparen, NULL);
            }
            else {
               COPY_OPND(IR_OPND_L(ir_idx), opnd);
            }
         }
         else {
            parse_err_flush(Find_Rparen, "IDENTIFIER");
         }

         if (paren) {
            if (LA_CH_VALUE == RPAREN) {
               NEXT_LA_CH;
            }
            else {
               parse_err_flush(Find_EOS, ")");
            }
         }
      }

      if (directive_region_error(Critical_Section_Dir,
                                 IR_LINE_NUM(ir_idx),
                                 IR_COL_NUM(ir_idx))) {
      }

      SET_DIRECTIVE_STATE(Critical_Section_Region);
      PUSH_BLK_STK (SGI_Critical_Section_Blk);
      BLK_IS_PARALLEL_REGION(blk_stk_idx)	= TRUE;
      CURR_BLK_FIRST_SH_IDX     = curr_stmt_sh_idx;
      LINK_TO_PARENT_BLK;
      break;

   case Tok_SGI_Dir_Endcriticalsection:
      ATP_HAS_TASK_DIRS(SCP_ATTR_IDX(curr_scp_idx)) = TRUE;
      ir_idx = gen_directive_ir(End_Critical_Section_Par_Opr);

      if (directive_region_error(End_Critical_Section_Dir,
                                 IR_LINE_NUM(ir_idx),
                                 IR_COL_NUM(ir_idx))) {
      }

      CLEAR_DIRECTIVE_STATE(Critical_Section_Region);
      SH_STMT_TYPE(curr_stmt_sh_idx) = SGI_End_Critical_Section_Stmt;
      stmt_type = SGI_End_Critical_Section_Stmt;
      end_critical_section_blk(FALSE);
      break;

   case Tok_SGI_Dir_Singleprocess:
      ATP_HAS_TASK_DIRS(SCP_ATTR_IDX(curr_scp_idx)) = TRUE;
      ir_idx = gen_directive_ir(Singleprocess_Par_Opr);

      parse_mp_directive(Singleprocess);

      if (directive_region_error(Single_Process_Dir,
                                 IR_LINE_NUM(ir_idx),
                                 IR_COL_NUM(ir_idx))) {
      }

      SET_DIRECTIVE_STATE(Single_Process_Region);
      PUSH_BLK_STK (SGI_Single_Process_Blk);
      BLK_IS_PARALLEL_REGION(blk_stk_idx)	= TRUE;
      CURR_BLK_FIRST_SH_IDX     = curr_stmt_sh_idx;
      LINK_TO_PARENT_BLK;
      break;

   case Tok_SGI_Dir_Endsingleprocess:
      ATP_HAS_TASK_DIRS(SCP_ATTR_IDX(curr_scp_idx)) = TRUE;
      ir_idx = gen_directive_ir(End_Singleprocess_Par_Opr);

      if (directive_region_error(End_Single_Process_Dir,
                                 IR_LINE_NUM(ir_idx),
                                 IR_COL_NUM(ir_idx))) {
      }

      CLEAR_DIRECTIVE_STATE(Single_Process_Region);
      IR_LINE_NUM_L(ir_idx) = TOKEN_LINE(token);
      IR_COL_NUM_L(ir_idx) = TOKEN_COLUMN(token);
      IR_FLD_L(ir_idx) = CN_Tbl_Idx;

      if (LA_CH_VALUE != EOS) {
         if (MATCHED_TOKEN_CLASS(Tok_Class_SGI_Dir_Kwd) &&
             TOKEN_VALUE(token) == Tok_SGI_Dir_Nowait) {

           IR_IDX_L(ir_idx) = CN_INTEGER_ONE_IDX;
         }
         else {
            parse_err_flush(Find_EOS, EOS_STR);
         }
      }
      else {
        IR_IDX_L(ir_idx) = CN_INTEGER_ZERO_IDX;
      }

      SH_STMT_TYPE(curr_stmt_sh_idx) = SGI_End_Single_Process_Stmt;
      stmt_type = SGI_End_Single_Process_Stmt;
      end_single_process_blk(FALSE);
      break;


   case Tok_SGI_Dir_Endpsections:
   case Tok_SGI_Dir_Endpsection:
      ATP_HAS_TASK_DIRS(SCP_ATTR_IDX(curr_scp_idx)) = TRUE;
      ir_idx = gen_directive_ir(End_Psection_Par_Opr);

      if (directive_region_error(End_Psection_Dir,
                                 IR_LINE_NUM(ir_idx),
                                 IR_COL_NUM(ir_idx))) {
      }

      CLEAR_DIRECTIVE_STATE(Parallel_Section_Region);
      IR_LINE_NUM_L(ir_idx) = TOKEN_LINE(token);
      IR_COL_NUM_L(ir_idx) = TOKEN_COLUMN(token);
      IR_FLD_L(ir_idx) = CN_Tbl_Idx;

      if (LA_CH_VALUE != EOS) {
         if (MATCHED_TOKEN_CLASS(Tok_Class_SGI_Dir_Kwd) &&
             TOKEN_VALUE(token) == Tok_SGI_Dir_Nowait) {

           IR_IDX_L(ir_idx) = CN_INTEGER_ONE_IDX;
         }
         else {
            parse_err_flush(Find_EOS, EOS_STR);
         }
      }
      else {
        IR_IDX_L(ir_idx) = CN_INTEGER_ZERO_IDX;
      }

      SH_STMT_TYPE(curr_stmt_sh_idx) = SGI_End_Psection_Stmt;
      stmt_type = SGI_End_Psection_Stmt;
      end_psection_blk(FALSE);

      break;

   case Tok_SGI_Dir_Endparallel:
      ATP_HAS_TASK_DIRS(SCP_ATTR_IDX(curr_scp_idx)) = TRUE;
      ir_idx = gen_directive_ir(End_Parallel_Par_Opr);

      if (directive_region_error(Sgi_End_Parallel_Dir,
                                 IR_LINE_NUM(ir_idx),
                                 IR_COL_NUM(ir_idx))) {
      }

      CLEAR_DIRECTIVE_STATE(Sgi_Parallel_Region);
      SH_STMT_TYPE(curr_stmt_sh_idx) = SGI_End_Parallel_Stmt;
      stmt_type = SGI_End_Parallel_Stmt;
      end_SGI_parallel_blk(FALSE);
      break;

   case Tok_SGI_Dir_Endpdo:
      ATP_HAS_TASK_DIRS(SCP_ATTR_IDX(curr_scp_idx)) = TRUE;
      ir_idx = gen_directive_ir(End_Pdo_Par_Opr);

      if (LA_CH_VALUE != EOS) {
         if (MATCHED_TOKEN_CLASS(Tok_Class_SGI_Dir_Kwd) &&
             TOKEN_VALUE(token) == Tok_SGI_Dir_Nowait) {

            IR_LINE_NUM_L(ir_idx) = TOKEN_LINE(token);
            IR_COL_NUM_L(ir_idx) = TOKEN_COLUMN(token);
            IR_FLD_L(ir_idx) = CN_Tbl_Idx;
            IR_IDX_L(ir_idx) = CN_INTEGER_ONE_IDX;
         }
         else {
            parse_err_flush(Find_EOS, EOS_STR);
         }
      }

      SH_STMT_TYPE(curr_stmt_sh_idx) = SGI_End_Pdo_Stmt;

      if (SH_IR_IDX(SH_PREV_IDX(curr_stmt_sh_idx)) != NULL_IDX &&
          IR_OPR(SH_IR_IDX(SH_PREV_IDX(curr_stmt_sh_idx))) == End_Pdo_Par_Opr &&
          SH_COMPILER_GEN(SH_PREV_IDX(curr_stmt_sh_idx))) {

         /* remove the CG end pdo */
         sh_idx = SH_PREV_IDX(curr_stmt_sh_idx);
         COPY_OPND(IR_OPND_R(ir_idx), IR_OPND_R(SH_IR_IDX(sh_idx)));

         SH_PREV_IDX(SH_NEXT_IDX(sh_idx)) = SH_PREV_IDX(sh_idx);
         SH_NEXT_IDX(SH_PREV_IDX(sh_idx)) = SH_NEXT_IDX(sh_idx);

         FREE_IR_NODE(SH_IR_IDX(sh_idx));
         FREE_SH_NODE(sh_idx);
         break;
      }

      if (directive_region_error(End_Pdo_Dir,
                                 IR_LINE_NUM(ir_idx),
                                 IR_COL_NUM(ir_idx))) {
      }

      CLEAR_DIRECTIVE_STATE(Pdo_Region);

      stmt_type = SGI_End_Pdo_Stmt;
      end_pdo_blk(FALSE);

      break;


   case Tok_SGI_Dir_Psection:
   case Tok_SGI_Dir_Psections:
      ATP_HAS_TASK_DIRS(SCP_ATTR_IDX(curr_scp_idx)) = TRUE;
      ir_idx = gen_directive_ir(Psection_Par_Opr);

      if (directive_region_error(Psection_Dir,
                                 IR_LINE_NUM(ir_idx),
                                 IR_COL_NUM(ir_idx))) {
      }

      parse_mp_directive(Psection);
      SET_DIRECTIVE_STATE(Parallel_Section_Region);
      PUSH_BLK_STK (SGI_Psection_Blk);
      BLK_IS_PARALLEL_REGION(blk_stk_idx)	= TRUE;
      CURR_BLK_FIRST_SH_IDX     = curr_stmt_sh_idx;
      LINK_TO_PARENT_BLK;
      break;

   case Tok_SGI_Dir_Section:
      ATP_HAS_TASK_DIRS(SCP_ATTR_IDX(curr_scp_idx)) = TRUE;
      ir_idx = gen_directive_ir(Section_Par_Opr);

      if (directive_region_error(Section_Dir,
                                 IR_LINE_NUM(ir_idx),
                                 IR_COL_NUM(ir_idx))) {
      }

      if (remove_pdo_blk(TRUE, "SECTION", IR_LINE_NUM(ir_idx),
                                 IR_COL_NUM(ir_idx))) {
      }

      SH_STMT_TYPE(curr_stmt_sh_idx) = SGI_Section_Stmt;
      stmt_type = SGI_Section_Stmt;

      if (CURR_BLK == SGI_Section_Blk) {
         end_psection_blk(FALSE);
      }

      PUSH_BLK_STK (SGI_Section_Blk);
      BLK_IS_PARALLEL_REGION(blk_stk_idx)	= TRUE;
      CURR_BLK_FIRST_SH_IDX     = curr_stmt_sh_idx;
      LINK_TO_PARENT_BLK;
      break;

   default:
      /* treat as comment */
      parse_err_flush(Find_EOS, NULL);

   }  /* end switch */

   /* Flush past all unimplemented dirs */

   if (LA_CH_VALUE != EOS) {
      PRINTMSG(LA_CH_LINE, 790, Warning, LA_CH_COLUMN);
      parse_err_flush(Find_EOS, NULL);
   }

EXIT:

   NEXT_LA_CH;

   TRACE (Func_Exit, "parse_par_directives", NULL);

   return;

}  /* parse_par_directives */

/******************************************************************************\
|*									      *|
|* Description:								      *|
|*	This routine parses the C$ directives on one line of source.          *|
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
static void parse_dollar_directives(void)
{
   int		ir_idx;
   int		list_idx;
   opnd_type	opnd;
#ifdef KEY /* Bug 10177 */
   long		the_constant = 0;
#else /* KEY Bug 10177 */
   long		the_constant;
#endif /* KEY Bug 10177 */


   TRACE (Func_Entry, "parse_dollar_directives", NULL);

   if (TOKEN_VALUE(token) > Tok_SGI_Dir_Start &&
       TOKEN_VALUE(token) < Tok_SGI_Dir_End &&
       disregard_mips[TOKEN_VALUE(token) - Tok_SGI_Dir_Start]) {
      goto EXIT;
   }

   switch (TOKEN_VALUE(token)) {

   case Tok_SGI_Dir_Distribute_Reshape:

      if (dump_flags.dsm) {
         parse_distribution_dir(TRUE);
      } 
      else {
         parse_err_flush(Find_EOS, NULL);
      }
      break;

   case Tok_SGI_Dir_Distribute:

      if (dump_flags.dsm) {
         parse_distribution_dir(FALSE);
      } 
      else {
         parse_err_flush(Find_EOS, NULL);
      }
      break;

   case Tok_SGI_Dir_Redistribute:
      if (dump_flags.dsm) {
         parse_redistribute_dir();
      }
      else {
         parse_err_flush(Find_EOS, NULL);
      }
      break;

   case Tok_SGI_Dir_Dynamic:
      if (dump_flags.dsm) {
         if (parse_var_name_list(&opnd)) {
            ir_idx = gen_directive_ir(Dynamic_Dollar_Opr);
            COPY_OPND(IR_OPND_L(ir_idx), opnd);
         }
      }
      else {
         parse_err_flush(Find_EOS, NULL);
      }
      break;

   case Tok_SGI_Dir_Page_Place:

      if (dump_flags.dsm) {
         ir_idx = gen_directive_ir(Page_Place_Dollar_Opr);

         NTR_IR_LIST_TBL(list_idx);
         IR_FLD_L(ir_idx) = IL_Tbl_Idx;
         IR_IDX_L(ir_idx) = list_idx;
         IR_LIST_CNT_L(ir_idx) = 3;
 
         NTR_IR_LIST_TBL(IL_NEXT_LIST_IDX(list_idx));
         IL_PREV_LIST_IDX(IL_NEXT_LIST_IDX(list_idx)) = list_idx;
         list_idx = IL_NEXT_LIST_IDX(list_idx);

         NTR_IR_LIST_TBL(IL_NEXT_LIST_IDX(list_idx));
         IL_PREV_LIST_IDX(IL_NEXT_LIST_IDX(list_idx)) = list_idx;

         list_idx = IR_IDX_L(ir_idx);

         if (LA_CH_VALUE == LPAREN) {
            NEXT_LA_CH;

            if (MATCHED_TOKEN_CLASS(Tok_Class_Id)) {
               parse_deref(&opnd, NULL_IDX);
               COPY_OPND(IL_OPND(list_idx), opnd);
            }
            else {
               parse_err_flush(Find_EOS, "IDENTIFIER");
               goto EXIT;
            }

            if (LA_CH_VALUE == COMMA) {
               NEXT_LA_CH;
            }
            else {
               parse_err_flush(Find_EOS, ",");
               goto EXIT;
            }

            list_idx = IL_NEXT_LIST_IDX(list_idx);

            parse_expr(&opnd);

            COPY_OPND(IL_OPND(list_idx), opnd);

            if (LA_CH_VALUE == COMMA) {
               NEXT_LA_CH;
            }
            else {
               parse_err_flush(Find_EOS, ",");
               goto EXIT;
            }

            list_idx = IL_NEXT_LIST_IDX(list_idx);

            parse_expr(&opnd);

            COPY_OPND(IL_OPND(list_idx), opnd);

            if (LA_CH_VALUE == RPAREN) {
               NEXT_LA_CH;
            }
            else {
               parse_err_flush(Find_EOS, ")");
            }
         }
         else {
            parse_err_flush(Find_EOS, "(");
         }
      }
      else {
         parse_err_flush(Find_EOS, NULL);
      }
      break;

   case Tok_SGI_Dir_Copyin:

      ir_idx = gen_directive_ir(Copyin_Dollar_Opr);

      if (directive_region_error(Copyin_Dir,
                                 IR_LINE_NUM(ir_idx),
                                 IR_COL_NUM(ir_idx))) {
      }

      parse_var_common_list(&opnd, TRUE);
      COPY_OPND(IR_OPND_L(ir_idx), opnd);
      break;

   case Tok_SGI_Dir_Doacross:
      ATP_HAS_TASK_DIRS(SCP_ATTR_IDX(curr_scp_idx)) = TRUE;
      ir_idx = gen_directive_ir(Doacross_Dollar_Opr);

      parse_mp_directive(Doacross);

      if (directive_region_error(Doacross_Dir,
                                 IR_LINE_NUM(ir_idx),
                                 IR_COL_NUM(ir_idx))) {
      }
      else {
         SET_DIRECTIVE_STATE(Doacross_Region);
         cdir_switches.doacross_sh_idx = curr_stmt_sh_idx;
      }

      break;


   case Tok_SGI_Dir_Chunk:
      if (LA_CH_VALUE == EQUAL) {
         NEXT_LA_CH;

         parse_expr(&opnd);
         COPY_OPND(cdir_switches.chunk_opnd, opnd);
      }
      else {
         parse_err_flush(Find_EOS, "=");
         goto EXIT;
      }
      break;

   case Tok_SGI_Dir_Mp_Schedtype:

      if (LA_CH_VALUE == EQUAL) {

         NEXT_LA_CH;

         if (MATCHED_TOKEN_CLASS(Tok_Class_SGI_Dir_Kwd)) {

            switch (TOKEN_VALUE(token)) {
               case Tok_SGI_Dir_Simple:
                  the_constant = MP_SCHEDTYPE_SIMPLE;
                  break;
               case Tok_SGI_Dir_Static:
                  the_constant = MP_SCHEDTYPE_SIMPLE;
                  break;
               case Tok_SGI_Dir_Dynamic:
                  the_constant = MP_SCHEDTYPE_DYNAMIC;
                  break;
               case Tok_SGI_Dir_Interleaved:
                  the_constant = MP_SCHEDTYPE_INTERLEAVED;
                  break;
               case Tok_SGI_Dir_Interleave:
                  the_constant = MP_SCHEDTYPE_INTERLEAVED;
                  break;
               case Tok_SGI_Dir_Runtime:
                  the_constant = MP_SCHEDTYPE_RUNTIME;
                  break;
               case Tok_SGI_Dir_Gss:
                  the_constant = MP_SCHEDTYPE_GUIDED;
                  break;
               case Tok_SGI_Dir_Guided:
                  the_constant = MP_SCHEDTYPE_GUIDED;
                  break;
               default:
                  parse_err_flush(Find_EOS, "MP_SCHEDTYPE mode");
                  break;
            }


            OPND_LINE_NUM(cdir_switches.mp_schedtype_opnd) = TOKEN_LINE(token);
            OPND_COL_NUM(cdir_switches.mp_schedtype_opnd) = TOKEN_COLUMN(token);
            OPND_FLD(cdir_switches.mp_schedtype_opnd) = CN_Tbl_Idx;
            OPND_IDX(cdir_switches.mp_schedtype_opnd) =
                                           C_INT_TO_CN(CG_INTEGER_DEFAULT_TYPE,
                                                       the_constant);

            if (directives_are_global) {
               global_schedtype_value = the_constant;
               global_schedtype_line = TOKEN_LINE(token);
               global_schedtype_col = TOKEN_COLUMN(token);
            }
         }
         else {
            parse_err_flush(Find_EOS, "MP_SCHEDTYPE mode");
         }

      }
      else {
         parse_err_flush(Find_EOS, "=");
         goto EXIT;
      }
      break;

   default:
      parse_err_flush(Find_EOS, NULL);

   }  /* end switch */

   /* Flush past all unimplemented dirs */

   if (LA_CH_VALUE != EOS) {
      PRINTMSG(LA_CH_LINE, 790, Warning, LA_CH_COLUMN);
      parse_err_flush(Find_EOS, NULL);
   }

EXIT:

   NEXT_LA_CH;

   TRACE (Func_Exit, "parse_dollar_directives", NULL);

   return;

}  /* parse_dollar_directives */


/******************************************************************************\
|*									      *|
|* Description:								      *|
|*	This routine parses the C*$* directives on one line of source.        *|
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

static void parse_star_directives(void)
{
   int			attr_idx;
   int			blk_idx;
   int			column;
   int			ir_idx;
   int			line;
   boolean		loop_dir	= FALSE;
   int			name_idx;
   opnd_type		opnd;
#ifdef KEY /* Bug 10177 */
   operator_type	opr = Null_Opr;
#else /* KEY Bug 10177 */
   operator_type	opr;
#endif /* KEY Bug 10177 */
   int			save_column_num;
   int			save_line_num;


   TRACE (Func_Entry, "parse_star_directives", NULL);

   if (TOKEN_VALUE(token) > Tok_SGI_Dir_Start &&
       TOKEN_VALUE(token) < Tok_SGI_Dir_End &&
       disregard_mips[TOKEN_VALUE(token) - Tok_SGI_Dir_Start]) {
      goto EXIT;
   }

   switch (TOKEN_VALUE(token)) {

   case Tok_SGI_Dir_Aggressiveinner:
      loop_dir	= TRUE;
      opr	= Aggressiveinnerloopfission_Opr;
      break;

   case Tok_SGI_Dir_Blockingsize:
      parse_star_dir_directives();
      goto EXIT;

   case Tok_SGI_Dir_Assert:

      if (! parse_assert_directive()) {
         goto EXIT;
      }
      break;

   case Tok_SGI_Dir_Align_Symbol:

      if (curr_stmt_category < Dir_Integer_Stmt_Cat) {
         PRINTMSG(TOKEN_LINE(token), 795, Warning,
                  TOKEN_COLUMN(token), "ALIGN_SYMBOL");
         parse_err_flush(Find_EOS, NULL);
         break;
      }

      if (curr_stmt_category >= Executable_Stmt_Cat) {
         PRINTMSG(TOKEN_LINE(token), 531, Error,
                  TOKEN_COLUMN(token), 
                  "ALIGN_SYMBOL");
         parse_err_flush(Find_EOS, NULL);
         break;
      }

      ir_idx = gen_directive_ir(Align_Symbol_Star_Opr);
      parse_fill_align_symbol();
      break;

   case Tok_SGI_Dir_Fill_Symbol:

      if (curr_stmt_category < Dir_Integer_Stmt_Cat) {
         PRINTMSG(TOKEN_LINE(token), 795, Warning,
                  TOKEN_COLUMN(token), "FILL_SYMBOL");
         parse_err_flush(Find_EOS, NULL);
         break;
      }

      if (curr_stmt_category >= Executable_Stmt_Cat) {
         PRINTMSG(TOKEN_LINE(token), 531, Error,
                  TOKEN_COLUMN(token), 
                  "FILL_SYMBOL");
         parse_err_flush(Find_EOS, NULL);
         break;
      }

      ir_idx = gen_directive_ir(Fill_Symbol_Star_Opr);
      parse_fill_align_symbol();
      break;

   case Tok_SGI_Dir_Blockable:
      parse_star_dir_directives();
      goto EXIT;

   case Tok_SGI_Dir_Concurrentize:
      ir_idx = gen_directive_ir(Concurrentize_Star_Opr);

      if (directives_are_global) {
         /* copy the assert into the global ir table */
         gen_gl_sh(After, Directive_Stmt, IR_LINE_NUM(ir_idx), 
                   IR_COL_NUM(ir_idx),
                   FALSE, FALSE, TRUE);
         GL_SH_IR_IDX(curr_gl_stmt_sh_idx) = copy_to_gl_subtree(ir_idx,
                                                                IR_Tbl_Idx);
      }
      break;

   case Tok_SGI_Dir_Fissionable:
      loop_dir	= TRUE;
      opr	= Fissionable_Star_Opr;
      break;

   case Tok_SGI_Dir_Flush:
      ir_idx = gen_directive_ir(Flush_Star_Opr);

      if (LA_CH_VALUE == LPAREN) {
         NEXT_LA_CH;

         parse_var_name_list(&opnd);
         COPY_OPND(IR_OPND_L(ir_idx), opnd);

         if (LA_CH_VALUE == RPAREN) {
            NEXT_LA_CH;
         }
         else {
            parse_err_flush(Find_EOS, ")");
            goto EXIT;
         }
      }
      break;

   case Tok_SGI_Dir_Fusable:
      loop_dir  = TRUE;
      opr       = Fusable_Star_Opr;
      break;

   case Tok_SGI_Dir_Fission:
      ir_idx = gen_directive_ir(Fission_Star_Opr);

      if (LA_CH_VALUE == LPAREN) {
         NEXT_LA_CH;

         parse_expr(&opnd);
         COPY_OPND(IR_OPND_L(ir_idx), opnd);

         if (LA_CH_VALUE == RPAREN) {
            NEXT_LA_CH;
         }
         else {
            parse_err_flush(Find_EOS, ")");
            goto EXIT;
         }
      }
      else {
         IR_LINE_NUM_L(ir_idx) = TOKEN_LINE(token);
         IR_COL_NUM_L(ir_idx) = TOKEN_COLUMN(token);
         IR_FLD_L(ir_idx) = CN_Tbl_Idx;
         IR_IDX_L(ir_idx) = CN_INTEGER_ONE_IDX;
      }
      break;

   case Tok_SGI_Dir_Fuse:
      ir_idx = gen_directive_ir(Fuse_Star_Opr);

      if (LA_CH_VALUE == LPAREN) {
         NEXT_LA_CH;

         parse_expr(&opnd);
         COPY_OPND(IR_OPND_L(ir_idx), opnd);

         if (LA_CH_VALUE == COMMA) {
            NEXT_LA_CH;

            parse_expr(&opnd);
            COPY_OPND(IR_OPND_R(ir_idx), opnd);
         }
         else {
            /* default value is 0 for second arg */

            IR_LINE_NUM_R(ir_idx) = TOKEN_LINE(token);
            IR_COL_NUM_R(ir_idx) = TOKEN_COLUMN(token);
            IR_FLD_R(ir_idx) = CN_Tbl_Idx;
            IR_IDX_R(ir_idx) = CN_INTEGER_ZERO_IDX;
         }

         if (LA_CH_VALUE == RPAREN) {
            NEXT_LA_CH;
         }
         else {
            parse_err_flush(Find_EOS, ")");
            goto EXIT;
         }
      }
      else {
         /* defaults are 2 and 0 */

         IR_LINE_NUM_L(ir_idx) = TOKEN_LINE(token);
         IR_COL_NUM_L(ir_idx) = TOKEN_COLUMN(token);
         IR_FLD_L(ir_idx) = CN_Tbl_Idx;
         IR_IDX_L(ir_idx) = CN_INTEGER_TWO_IDX;

         IR_LINE_NUM_R(ir_idx) = TOKEN_LINE(token);
         IR_COL_NUM_R(ir_idx) = TOKEN_COLUMN(token);
         IR_FLD_R(ir_idx) = CN_Tbl_Idx;
         IR_IDX_R(ir_idx) = CN_INTEGER_ZERO_IDX;
      }
      break;

   case Tok_SGI_Dir_Inline:
   case Tok_SGI_Dir_Ipa:
      parse_sgi_dir_inline(TRUE);
      break;

   case Tok_SGI_Dir_Noinline:
   case Tok_SGI_Dir_Noipa:
      parse_sgi_dir_inline(FALSE);
      break;

   case Tok_SGI_Dir_Interchange:
      parse_star_dir_directives();
      goto EXIT;

   case Tok_SGI_Dir_Noblocking:
      loop_dir	= TRUE;
      opr	= Noblocking_Dir_Opr;
      break;

   case Tok_SGI_Dir_Noconcurrentize:
      ir_idx = gen_directive_ir(Noconcurrentize_Star_Opr);

      if (directives_are_global) {
         /* copy the assert into the global ir table */
         gen_gl_sh(After, Directive_Stmt, IR_LINE_NUM(ir_idx), 
                   IR_COL_NUM(ir_idx),
                   FALSE, FALSE, TRUE);
         GL_SH_IR_IDX(curr_gl_stmt_sh_idx) = copy_to_gl_subtree(ir_idx,
                                                                IR_Tbl_Idx);
      }
      break;

   case Tok_SGI_Dir_Nointerchange:
      loop_dir	= TRUE;
      opr	= Nointerchange_Dir_Opr;
      break;

   case Tok_SGI_Dir_Nofission:
      loop_dir	= TRUE;
      opr	= Nofission_Star_Opr;
      break;

   case Tok_SGI_Dir_Nofusion:
      loop_dir	= TRUE;
      opr	= Nofusion_Star_Opr;
      break;

   case Tok_SGI_Dir_Opaque:
      loop_dir	= TRUE;
      opr	= Opaque_Star_Opr;

      if (directive_region_error(Opaque_Dir,
                                 TOKEN_LINE(token),
                                 TOKEN_COLUMN(token))) {
      }
      break;

   case Tok_SGI_Dir_Optional:

      if (curr_stmt_category < Dir_Integer_Stmt_Cat) {
         PRINTMSG(TOKEN_LINE(token), 795, Warning,
                  TOKEN_COLUMN(token), "OPTIONAL");
         parse_err_flush(Find_EOS, NULL);
         break;
      }

      if (directive_region_error(Optional_Dir,
                                 TOKEN_LINE(token),
                                 TOKEN_COLUMN(token))) {
         break;
      }

      if (LA_CH_VALUE == LPAREN) {
         NEXT_LA_CH;

         if (MATCHED_TOKEN_CLASS(Tok_Class_Id)) {
            attr_idx = srch_sym_tbl(TOKEN_STR(token), TOKEN_LEN(token),
                                    &name_idx);

            if (attr_idx == NULL_IDX) {
               attr_idx                    = ntr_sym_tbl(&token, name_idx);
               LN_DEF_LOC(name_idx)        = TRUE;
               AT_OBJ_CLASS(attr_idx)      = Pgm_Unit;
               MAKE_EXTERNAL_NAME(attr_idx,
                                  AT_NAME_IDX(attr_idx),
                                  AT_NAME_LEN(attr_idx));
               ATP_PROC(attr_idx)          = Extern_Proc;
               ATP_SCP_IDX(attr_idx)       = curr_scp_idx;
               ATP_OPTIONAL_DIR(attr_idx)  = TRUE;
            }
            else if (!fnd_semantic_err(Obj_Optional_Dir,
                                       TOKEN_LINE(token),
                                       TOKEN_COLUMN(token),
                                       attr_idx,
                                       TRUE)) {

               if (AT_REFERENCED(attr_idx) == Char_Rslt_Bound_Ref) {
                  AT_ATTR_LINK(attr_idx)	= NULL_IDX;
                  LN_DEF_LOC(name_idx)	= TRUE;
               }

               if (AT_OBJ_CLASS(attr_idx) == Data_Obj) { /* Switch to Function*/
                  chg_data_obj_to_pgm_unit(attr_idx,
                                           Function,
                                           Extern_Proc);
                  ATP_OPTIONAL_DIR(attr_idx)	= TRUE;
               }
               else if (AT_OBJ_CLASS(attr_idx) == Pgm_Unit) {
                  ATP_OPTIONAL_DIR(attr_idx)	= TRUE;
               }
            }
         }
         else {
            parse_err_flush(Find_EOS, "procedure name");
            goto EXIT;
         }

         if (LA_CH_VALUE == RPAREN) {
            NEXT_LA_CH;
         }
         else {
            parse_err_flush(Find_EOS, ")");
            goto EXIT;
         }
      }
      else {
         parse_err_flush(Find_EOS, "(");
         goto EXIT;
      }
      break;

#ifdef KEY /* Bug 2660 */
   /* Syntax is c*$*options "somestring" */
   case Tok_SGI_Dir_Options:
      ir_idx = gen_directive_ir(Options_Dir_Opr);
      if (QUOTE == LA_CH_VALUE || DBL_QUOTE == LA_CH_VALUE) {
        parse_expr(&opnd);
	COPY_OPND(IR_OPND_L(ir_idx), opnd);
      }
      else {
	parse_err_flush(Find_EOS, "\"");
	goto EXIT;
      }
      break;
#endif /* KEY Bug 2660 */

   case Tok_SGI_Dir_Regionbegin:
      ir_idx = gen_directive_ir(Regionbegin_Star_Opr);

      if (directive_region_error(Regionbegin_Dir,
                                 IR_LINE_NUM(ir_idx),
                                 IR_COL_NUM(ir_idx))) {
      }

      SET_DIRECTIVE_STATE(Region_Region);
      PUSH_BLK_STK (SGI_Region_Blk);
      CURR_BLK_FIRST_SH_IDX     = curr_stmt_sh_idx;
      LINK_TO_PARENT_BLK;
      break;

   case Tok_SGI_Dir_Regionend:
      ir_idx = gen_directive_ir(Regionend_Star_Opr);

      if (directive_region_error(Regionend_Dir,
                                 IR_LINE_NUM(ir_idx),
                                 IR_COL_NUM(ir_idx))) {
      }

      CLEAR_DIRECTIVE_STATE(Region_Region);
      SH_STMT_TYPE(curr_stmt_sh_idx) = SGI_Region_End_Stmt;
      stmt_type = SGI_Region_End_Stmt;
      end_region_blk(FALSE);
      break;

   case Tok_SGI_Dir_Section_Non_Gp:

      if (LA_CH_VALUE != LPAREN) {
         parse_err_flush(Find_EOS, "(");
      }
      else {
         NEXT_LA_CH;

         ir_idx = gen_directive_ir(Section_Nongp_Star_Opr);

         parse_var_common_list(&opnd, FALSE);

         COPY_OPND(IR_OPND_L(ir_idx), opnd);

         if (LA_CH_VALUE != RPAREN) {
            parse_err_flush(Find_EOS, ")");
         }
         else {
            NEXT_LA_CH;
         }
      }
      break;

   case Tok_SGI_Dir_Section_Gp:

      if (LA_CH_VALUE != LPAREN) {
         parse_err_flush(Find_EOS, "(");
      }
      else {
         NEXT_LA_CH;

         ir_idx = gen_directive_ir(Section_Gp_Star_Opr);

         parse_var_common_list(&opnd, FALSE);

         COPY_OPND(IR_OPND_L(ir_idx), opnd);

         if (LA_CH_VALUE != RPAREN) {
            parse_err_flush(Find_EOS, ")");
         }
         else {
            NEXT_LA_CH;
         }
      }

      break;


   case Tok_SGI_Dir_Prefetch_Manual:
      ir_idx = gen_directive_ir(Prefetch_Manual_Star_Opr);

      if (LA_CH_VALUE == LPAREN) {
         NEXT_LA_CH;

         parse_expr(&opnd);
         COPY_OPND(IR_OPND_L(ir_idx), opnd);

         if (LA_CH_VALUE == RPAREN) {
            NEXT_LA_CH;
         }
         else {
            parse_err_flush(Find_EOS, ")");
            goto EXIT;
         }
      }
      else {
         parse_err_flush(Find_EOS, "(");
         goto EXIT;
      }
      break;

   case Tok_SGI_Dir_Prefetch_Ref:
      ir_idx = gen_directive_ir(Prefetch_Ref_Star_Opr);
      parse_prefetch_ref();
      break;

   case Tok_SGI_Dir_Prefetch:
      ir_idx = gen_directive_ir(Prefetch_Star_Opr);

      if (LA_CH_VALUE == LPAREN) {
         NEXT_LA_CH;

         parse_expr(&opnd);
         COPY_OPND(IR_OPND_L(ir_idx), opnd);

         if (LA_CH_VALUE == COMMA) {
            NEXT_LA_CH;

            parse_expr(&opnd);
            COPY_OPND(IR_OPND_R(ir_idx), opnd);
         }
         else {
            /* the default value of -1 is set in s_directiv.c */
         }

         if (LA_CH_VALUE == RPAREN) {
            NEXT_LA_CH;
         }
         else {
            parse_err_flush(Find_EOS, ")");
            goto EXIT;
         }
      }
      else {
         parse_err_flush(Find_EOS, "(");
         goto EXIT;
      }
      break;

   case Tok_SGI_Dir_Prefetch_Ref_Disable:
      ir_idx = gen_directive_ir(Prefetch_Ref_Disable_Star_Opr);

      if (LA_CH_VALUE == EQUAL) {
         NEXT_LA_CH;

         if (MATCHED_TOKEN_CLASS(Tok_Class_Id)) {
            parse_deref(&opnd, NULL_IDX);

            if (OPND_FLD(opnd) != AT_Tbl_Idx) {
               find_opnd_line_and_column(&opnd, &line, &column);
               PRINTMSG(line, 1374, Error, column);
            }
            else {
               COPY_OPND(IR_OPND_L(ir_idx), opnd);
            }
         }
         else {
            parse_err_flush(Find_EOS, "array name");
            goto EXIT;
         }

         if (LA_CH_VALUE == COMMA) {
            NEXT_LA_CH;

            if (MATCHED_TOKEN_CLASS(Tok_Class_SGI_Dir_Kwd) &&
                TOKEN_VALUE(token) == Tok_SGI_Dir_Size) {

               if (LA_CH_VALUE == EQUAL) {
                  NEXT_LA_CH;

                  parse_expr(&opnd);
                  COPY_OPND(IR_OPND_R(ir_idx), opnd);
               }
               else {
                  parse_err_flush(Find_EOS, "=");
                  goto EXIT;
               }
            }
            else {
               parse_err_flush(Find_EOS, "SIZE");
               goto EXIT;
            }
         }
      }
      else {
         parse_err_flush(Find_EOS, "=");
         goto EXIT;
      }
      break;

   case Tok_SGI_Dir_Unroll:
      ir_idx = gen_directive_ir(Unroll_Star_Opr);

      if (LA_CH_VALUE == LPAREN) {
         NEXT_LA_CH;

         parse_expr(&opnd);
         COPY_OPND(IR_OPND_L(ir_idx), opnd);

         if (LA_CH_VALUE == COMMA) {
            NEXT_LA_CH;

            /* parse, but ignore weight parameter ,n2 */
            parse_expr(&opnd);
         }

         if (LA_CH_VALUE == RPAREN) {
            NEXT_LA_CH;
         }
         else {
            parse_err_flush(Find_EOS, ")");
            goto EXIT;
         }
      }
      else {
         parse_err_flush(Find_EOS, "(");
         goto EXIT;
      }
      break;

   case Tok_SGI_Dir_Limit:
   case Tok_SGI_Dir_Minconcurrent:
      PRINTMSG(TOKEN_LINE(token), 801, Warning, TOKEN_COLUMN(token));
      parse_err_flush(Find_EOS, NULL);
      break;

   default:
      /* treat as comment */
      parse_err_flush(Find_EOS, NULL);

   }  /* end switch */

   if (loop_dir) {

      if (curr_stmt_category < Dir_Integer_Stmt_Cat) {
         PRINTMSG(TOKEN_LINE(token), 795, Warning, TOKEN_COLUMN(token),
                  TOKEN_STR(token));
         parse_err_flush(Find_EOS, NULL);
      }
      else {
         ir_idx = gen_directive_ir(opr);
      }
   }

   /* Flush past all unimplemented dirs */

   if (LA_CH_VALUE != EOS) {
      PRINTMSG(LA_CH_LINE, 790, Warning, LA_CH_COLUMN);
      parse_err_flush(Find_EOS, NULL);
   }

EXIT:

   NEXT_LA_CH;

   TRACE (Func_Exit, "parse_star_directives", NULL);

   return;

}  /* parse_star_directives */

/******************************************************************************\
|*                                                                            *|
|* Description:                                                               *|
|*      This routine parses the PREFETCH_REF directive.                       *|
|*      The ir it produces looks like ..                                      *|
|*                                                                            *|
|*                        (Prefetch_Ref_Star_Opr)                             *|
|*                       /                                                    *|
|*                      |- array ref                                          *|
|*                      |- stride list (2)                                    *|
|*                      |- level list  (2)                                    *|
|*                      |- kind                                               *|
|*                      |- size                                               *|
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

static void parse_prefetch_ref(void)

{
   int		buf_idx;
   int          column;
   int          i;
   int          ir_idx;
   int          line;
   int          list_array[5];
   int          list_idx;
   opnd_type    opnd;
   int		stmt_num;


   TRACE (Func_Entry, "parse_prefetch_ref", NULL);

   ir_idx = SH_IR_IDX(curr_stmt_sh_idx);

   for (i = 0; i < 5; i++) {
      NTR_IR_LIST_TBL(list_array[i]);
      if (i >= 1) {
         IL_NEXT_LIST_IDX(list_array[i - 1]) = list_array[i];
         IL_PREV_LIST_IDX(list_array[i]) = list_array[i - 1];
      }
   }

   IR_FLD_L(ir_idx) = IL_Tbl_Idx;
   IR_IDX_L(ir_idx) = list_array[0];
   IR_LIST_CNT_L(ir_idx) = 5;

   if (LA_CH_VALUE == EQUAL) {
      NEXT_LA_CH;

      if (MATCHED_TOKEN_CLASS(Tok_Class_Id)) {
         parse_deref(&opnd, NULL_IDX);
         COPY_OPND(IL_OPND(list_array[0]), opnd);
      }
      else {
         parse_err_flush(Find_EOS, "array name");
         goto EXIT;
      }
   }
   else {
      parse_err_flush(Find_EOS, "=");
      goto EXIT;
   }

   while (LA_CH_VALUE == COMMA) {
      NEXT_LA_CH;

      if (MATCHED_TOKEN_CLASS(Tok_Class_SGI_Dir_Kwd)) {

         if (LA_CH_VALUE == EQUAL) {
            NEXT_LA_CH;
         }
         else {
            parse_err_flush(Find_EOS, "=");
            goto EXIT;
         }

         switch (TOKEN_VALUE(token)) {
            case Tok_SGI_Dir_Stride:
               if (IL_IDX(list_array[1]) != NULL_IDX) {
                  PRINTMSG(TOKEN_LINE(token), 1360, Error, TOKEN_COLUMN(token),
                           "STRIDE", "PREFETCH_REF");
                  parse_err_flush(Find_EOS, NULL);
                  goto EXIT;
               }

               /* I assume I will get digits here */
               parse_expr(&opnd);
               NTR_IR_LIST_TBL(list_idx);
               IL_FLD(list_array[1]) = IL_Tbl_Idx;
               IL_LIST_CNT(list_array[1]) = 1;
               IL_IDX(list_array[1]) = list_idx;

               COPY_OPND(IL_OPND(list_idx), opnd);

               if (LA_CH_VALUE == COMMA) {
                  buf_idx = LA_CH_BUF_IDX;
                  stmt_num = LA_CH_STMT_NUM;

                  NEXT_LA_CH;
                  if (isdigit(LA_CH_VALUE)) {
                     parse_expr(&opnd);
                     NTR_IR_LIST_TBL(IL_NEXT_LIST_IDX(list_idx));
                     IL_PREV_LIST_IDX(IL_NEXT_LIST_IDX(list_idx)) = list_idx;
                     list_idx = IL_NEXT_LIST_IDX(list_idx);
                     IL_LIST_CNT(list_array[1]) += 1;
                     COPY_OPND(IL_OPND(list_idx), opnd);
                  }
                  else {
                     reset_lex(buf_idx, stmt_num);
                  }
               }
               break;

            case Tok_SGI_Dir_Level:
               if (IL_IDX(list_array[2]) != NULL_IDX) {
                  PRINTMSG(TOKEN_LINE(token), 1360, Error, TOKEN_COLUMN(token),
                           "LEVEL", "PREFETCH_REF");
                  parse_err_flush(Find_EOS, NULL);
                  goto EXIT;
               }

               /* I assume I will get digits here */
               parse_expr(&opnd);
               NTR_IR_LIST_TBL(list_idx);
               IL_FLD(list_array[2]) = IL_Tbl_Idx;
               IL_LIST_CNT(list_array[2]) = 1;
               IL_IDX(list_array[2]) = list_idx;

               COPY_OPND(IL_OPND(list_idx), opnd);

               if (LA_CH_VALUE == COMMA) {
                  buf_idx = LA_CH_BUF_IDX;
                  stmt_num = LA_CH_STMT_NUM;

                  NEXT_LA_CH;
                  if (isdigit(LA_CH_VALUE)) {
                     parse_expr(&opnd);
                     NTR_IR_LIST_TBL(IL_NEXT_LIST_IDX(list_idx));
                     IL_PREV_LIST_IDX(IL_NEXT_LIST_IDX(list_idx)) = list_idx;
                     list_idx = IL_NEXT_LIST_IDX(list_idx);
                     IL_LIST_CNT(list_array[2]) += 1;
                     COPY_OPND(IL_OPND(list_idx), opnd);
                  }
                  else {
                     reset_lex(buf_idx, stmt_num);
                  }
               }
               break;

            case Tok_SGI_Dir_Kind:
               if (IL_IDX(list_array[3]) != NULL_IDX) {
                  PRINTMSG(TOKEN_LINE(token), 1360, Error, TOKEN_COLUMN(token),
                           "KIND", "PREFETCH_REF");
                  parse_err_flush(Find_EOS, NULL);
                  goto EXIT;
               }

               if (LA_CH_VALUE == 'R') {
                  line = LA_CH_LINE;
                  column = LA_CH_COLUMN;
                  NEXT_LA_CH;
                  if (LA_CH_VALUE == 'D') {
                     NEXT_LA_CH;
                     IL_FLD(list_array[3]) = CN_Tbl_Idx;
                     IL_IDX(list_array[3]) = CN_INTEGER_ZERO_IDX;
                     IL_LINE_NUM(list_array[3]) = line;
                     IL_COL_NUM(list_array[3]) = column;
                  }
                  else {
                     parse_err_flush(Find_EOS, "RD or WR");
                     goto EXIT;
                  }
               }
               else if (LA_CH_VALUE == 'W') {
                  line = LA_CH_LINE;
                  column = LA_CH_COLUMN;
                  NEXT_LA_CH;
                  if (LA_CH_VALUE == 'R') {
                     NEXT_LA_CH;
                     IL_FLD(list_array[3]) = CN_Tbl_Idx;
                     IL_IDX(list_array[3]) = CN_INTEGER_ONE_IDX;
                     IL_LINE_NUM(list_array[3]) = line;
                     IL_COL_NUM(list_array[3]) = column;
                  }
                  else {
                     parse_err_flush(Find_EOS, "RD or WR");
                     goto EXIT;
                  }
               }
               else {
                  parse_err_flush(Find_EOS, "RD or WR");
                  goto EXIT;
               }
               break;

            case Tok_SGI_Dir_Size:
               if (IL_IDX(list_array[4]) != NULL_IDX) {
                  PRINTMSG(TOKEN_LINE(token), 1360, Error, TOKEN_COLUMN(token),
                           "SIZE", "PREFETCH_REF");
                  parse_err_flush(Find_EOS, NULL);
                  goto EXIT;
               }

               parse_expr(&opnd);
               COPY_OPND(IL_OPND(list_array[4]), opnd);
               break;

            default:
               parse_err_flush(Find_EOS, "PREFETCH_REF clause");
               goto EXIT;
         }
      }
      else {
         parse_err_flush(Find_EOS, "PREFETCH_REF clause");
         goto EXIT;
      }
   }

   line = IR_LINE_NUM(ir_idx);
   column = IR_COL_NUM(ir_idx);

   if (IL_FLD(list_array[1]) == NO_Tbl_Idx) {
      NTR_IR_LIST_TBL(list_idx);
      IL_FLD(list_array[1]) = IL_Tbl_Idx;
      IL_LIST_CNT(list_array[1]) = 1;
      IL_IDX(list_array[1]) = list_idx;

      IL_FLD(list_idx) = CN_Tbl_Idx;
      IL_IDX(list_idx) = CN_INTEGER_ONE_IDX;
      IL_LINE_NUM(list_idx) = line;
      IL_COL_NUM(list_idx) = column;
   }

   if (IL_FLD(list_array[2]) == NO_Tbl_Idx) {
      NTR_IR_LIST_TBL(list_idx);
      IL_FLD(list_array[2]) = IL_Tbl_Idx;
      IL_LIST_CNT(list_array[2]) = 1;
      IL_IDX(list_array[2]) = list_idx;

      IL_FLD(list_idx) = CN_Tbl_Idx;
      IL_IDX(list_idx) = CN_INTEGER_TWO_IDX;
      IL_LINE_NUM(list_idx) = line;
      IL_COL_NUM(list_idx) = column;
   }

   if (IL_FLD(list_array[3]) == NO_Tbl_Idx) {
      IL_FLD(list_array[3]) = CN_Tbl_Idx;
      IL_IDX(list_array[3]) = CN_INTEGER_ONE_IDX;
      IL_LINE_NUM(list_array[3]) = line;
      IL_COL_NUM(list_array[3]) = column;
   }
EXIT:

   TRACE (Func_Exit, "parse_prefetch_ref", NULL);

   return;

}  /* parse_prefetch_ref */

/******************************************************************************\
|*									      *|
|* Description:								      *|
|*	This routine parses the mp directive.                                 *|
|*      The ir it produces looks like ..                                      *|
|*                                                                            *|
|*                        (mp directive operator)                             *|
|*                       /                                                    *|
|*                      |- IF condition                                       *|
|*                      |- SHARE | SHARED var list                            *|
|*                      |- LASTLOCAL var list                                 *|
|*                      |- REDUCTION var list                                 *|
|*                      |- MP_SCHEDTYPE value (in const table)                *|
|*                      |- CHUNK expression (also BLOCKED)                    *|
|*                      |- AFFINITY index_var list                            *|
|*                      |- IS THREAD constant (THREAD == 1, DATA == 0)        *|
|*                      |- THREAD/DATA list                                   *|
|*                      |- LOCAL | PRIVATE var list                           *|
|*                      |- ONTO list                                          *|
|*                      |- NEST list                                          *|
|*                      |- LASTTHREAD opnd                                    *|
|*			|- ORDERED constant (ORDERED == 1, else NO_Tbl_Idx)   *|
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

static void parse_mp_directive(mp_directive_type directive)

{
   int		column;
   int		i;
   int		ir_idx;
   int		line;
   int		list_array[MP_DIR_LIST_CNT];
   int		list_idx;
   int		list2_idx;
   opnd_type    opnd;
   boolean	seen_nest = FALSE;
#ifdef KEY /* Bug 10177 */
   long		the_constant = 0;
#else /* KEY Bug 10177 */
   long		the_constant;
#endif /* KEY Bug 10177 */


   TRACE (Func_Entry, "parse_mp_directive", NULL);

   ir_idx = SH_IR_IDX(curr_stmt_sh_idx);

   for (i = 0; i < MP_DIR_LIST_CNT; i++) {
      NTR_IR_LIST_TBL(list_array[i]);
      if (i >= 1) {
         IL_NEXT_LIST_IDX(list_array[i - 1]) = list_array[i];
         IL_PREV_LIST_IDX(list_array[i]) = list_array[i - 1];
      }
   }

   IR_FLD_L(ir_idx) = IL_Tbl_Idx;
   IR_IDX_L(ir_idx) = list_array[0];
   IR_LIST_CNT_L(ir_idx) = MP_DIR_LIST_CNT;

   while (LA_CH_VALUE != EOS) {

      if (LA_CH_VALUE == LPAREN) {
         /* must be (ORDERED) */
         NEXT_LA_CH;

         if (MATCHED_TOKEN_CLASS(Tok_Class_SGI_Dir_Kwd)) {

            if (TOKEN_VALUE(token) == Tok_SGI_Dir_Ordered) {

               if (! clause_allowed[directive][Ordered_Clause]) {
                  PRINTMSG(TOKEN_LINE(token), 1370, Error, TOKEN_COLUMN(token),
                           "ORDERED", mp_dir_str[directive]);
                  parse_err_flush(Find_EOS, NULL);
                  goto EXIT;
               }

               if (IL_IDX(list_array[MP_DIR_ORDERED_IDX]) != NULL_IDX) {
                  PRINTMSG(TOKEN_LINE(token), 1360, Error, TOKEN_COLUMN(token),
                           "ORDERED", mp_dir_str[directive]);
                  parse_err_flush(Find_EOS, NULL);
                  goto EXIT;
               }


               IL_FLD(list_array[MP_DIR_ORDERED_IDX]) = CN_Tbl_Idx;
               IL_LINE_NUM(list_array[MP_DIR_ORDERED_IDX]) = TOKEN_LINE(token);
               IL_COL_NUM(list_array[MP_DIR_ORDERED_IDX]) = TOKEN_COLUMN(token);
               IL_IDX(list_array[MP_DIR_ORDERED_IDX]) = CN_INTEGER_ONE_IDX;

               if (LA_CH_VALUE == RPAREN) {
                  NEXT_LA_CH;
            }
            else {
                  parse_err_flush(Find_EOS, ")");
               }
            }
            else {
               parse_err_flush(Find_EOS, "ORDERED clause");
            }
         }
         else {
            parse_err_flush(Find_EOS, "mp clause");
         }
      }
      else if (MATCHED_TOKEN_CLASS(Tok_Class_SGI_Dir_Kwd)) {

         switch (TOKEN_VALUE(token)) {

            case Tok_SGI_Dir_If:

               if (! clause_allowed[directive][If_Clause]) {
                  PRINTMSG(TOKEN_LINE(token), 1370, Error, TOKEN_COLUMN(token),
                           "IF", mp_dir_str[directive]);
                  parse_err_flush(Find_EOS, NULL);
                  goto EXIT;
               }

               if (IL_IDX(list_array[MP_DIR_IF_IDX]) != NULL_IDX) {
                  PRINTMSG(TOKEN_LINE(token), 1360, Error, TOKEN_COLUMN(token),
                           "IF", mp_dir_str[directive]);
                  parse_err_flush(Find_EOS, NULL);
                  goto EXIT;
               }

               if (LA_CH_VALUE == LPAREN) {
                  NEXT_LA_CH;
                  parse_expr(&opnd);

                  COPY_OPND(IL_OPND(list_array[MP_DIR_IF_IDX]), opnd);

                  if (LA_CH_VALUE == RPAREN) {
                     NEXT_LA_CH;
                  }
                  else {
                     parse_err_flush(Find_EOS, ")");
                     goto EXIT;
                  }
               }
               else {
                  parse_err_flush(Find_EOS, "(");
                  goto EXIT;
               }
               break;

            case Tok_SGI_Dir_Share:
            case Tok_SGI_Dir_Shared:

               if (! clause_allowed[directive][Share_Clause]) {
                  PRINTMSG(TOKEN_LINE(token), 1370, Error, TOKEN_COLUMN(token),
                           "SHARE", mp_dir_str[directive]);
                  parse_err_flush(Find_EOS, NULL);
                  goto EXIT;
               }

               if (LA_CH_VALUE == LPAREN) {
                  NEXT_LA_CH;
                  parse_var_name_list(&opnd);

                  if (IL_IDX(list_array[MP_DIR_SHARE_IDX]) == NULL_IDX) {
                     COPY_OPND(IL_OPND(list_array[MP_DIR_SHARE_IDX]), opnd);
                  }
                  else {
                     /* find the end of list */

                     list_idx = IL_IDX(list_array[MP_DIR_SHARE_IDX]);
                     while (IL_NEXT_LIST_IDX(list_idx)) {
                        list_idx = IL_NEXT_LIST_IDX(list_idx);
                     }

                     /* append the new list */
                     IL_NEXT_LIST_IDX(list_idx) = OPND_IDX(opnd);
                     IL_PREV_LIST_IDX(OPND_IDX(opnd)) = list_idx;
                     IL_LIST_CNT(list_array[MP_DIR_SHARE_IDX]) += 
                                                          OPND_LIST_CNT(opnd);
                  }

                  if (LA_CH_VALUE == RPAREN) {
                     NEXT_LA_CH;
                  } 
                  else {
                     parse_err_flush(Find_EOS, ")");
                     goto EXIT;
                  }
               }
               else {
                  parse_err_flush(Find_EOS, "(");
                  goto EXIT;
               }

               break;

            case Tok_SGI_Dir_Lastlocal:

               if (! clause_allowed[directive][Lastlocal_Clause]) {
                  PRINTMSG(TOKEN_LINE(token), 1370, Error, TOKEN_COLUMN(token),
                           "LASTLOCAL", mp_dir_str[directive]);
                  parse_err_flush(Find_EOS, NULL);
                  goto EXIT;
               }

               if (LA_CH_VALUE == LPAREN) {
                  NEXT_LA_CH;
                  parse_var_name_list(&opnd);

                  if (IL_IDX(list_array[MP_DIR_LASTLOCAL_IDX]) == NULL_IDX) {
                     COPY_OPND(IL_OPND(list_array[MP_DIR_LASTLOCAL_IDX]), 
                               opnd);
                  }
                  else {
                     /* find the end of list */

                     list_idx = IL_IDX(list_array[MP_DIR_LASTLOCAL_IDX]);
                     while (IL_NEXT_LIST_IDX(list_idx)) {
                        list_idx = IL_NEXT_LIST_IDX(list_idx);
                     }

                     /* append the new list */
                     IL_NEXT_LIST_IDX(list_idx) = OPND_IDX(opnd);
                     IL_PREV_LIST_IDX(OPND_IDX(opnd)) = list_idx;
                     IL_LIST_CNT(list_array[MP_DIR_LASTLOCAL_IDX]) += 
                                                       OPND_LIST_CNT(opnd);
                  }

                  if (LA_CH_VALUE == RPAREN) {
                     NEXT_LA_CH;
                  }
                  else {
                     parse_err_flush(Find_EOS, ")");
                     goto EXIT;
                  }
               }
               else {
                  parse_err_flush(Find_EOS, "(");
                  goto EXIT;
               }

               break;

            case Tok_SGI_Dir_Reduction:

               if (! clause_allowed[directive][Reduction_Clause]) {
                  PRINTMSG(TOKEN_LINE(token), 1370, Error, TOKEN_COLUMN(token),
                           "REDUCTION", mp_dir_str[directive]);
                  parse_err_flush(Find_EOS, NULL);
                  goto EXIT;
               }

               if (LA_CH_VALUE == LPAREN) {
                  NEXT_LA_CH;
                  parse_reference_list(&opnd);

                  if (IL_IDX(list_array[MP_DIR_REDUCTION_IDX]) == NULL_IDX) {
                     COPY_OPND(IL_OPND(list_array[MP_DIR_REDUCTION_IDX]), 
                               opnd);
                  }
                  else {
                     /* find the end of list */

                     list_idx = IL_IDX(list_array[MP_DIR_REDUCTION_IDX]);
                     while (IL_NEXT_LIST_IDX(list_idx)) {
                        list_idx = IL_NEXT_LIST_IDX(list_idx);
                     }

                     /* append the new list */
                     IL_NEXT_LIST_IDX(list_idx) = OPND_IDX(opnd);
                     IL_PREV_LIST_IDX(OPND_IDX(opnd)) = list_idx;
                     IL_LIST_CNT(list_array[MP_DIR_REDUCTION_IDX]) += 
                                                        OPND_LIST_CNT(opnd);
                  }

                  if (LA_CH_VALUE == RPAREN) {
                     NEXT_LA_CH;
                  }
                  else {
                     parse_err_flush(Find_EOS, ")");
                     goto EXIT;
                  }
               }
               else {
                  parse_err_flush(Find_EOS, "(");
                  goto EXIT;
               }

               break;

            case Tok_SGI_Dir_Mp_Schedtype:

               if (! clause_allowed[directive][Mp_Schedtype_Clause]) {
                  PRINTMSG(TOKEN_LINE(token), 1370, Error, TOKEN_COLUMN(token),
                           "MP_SCHEDTYPE", mp_dir_str[directive]);
                  parse_err_flush(Find_EOS, NULL);
                  goto EXIT;
               }

               if (IL_IDX(list_array[MP_DIR_MP_SCHEDTYPE_IDX]) != NULL_IDX) {
                  PRINTMSG(TOKEN_LINE(token), 1360, Error, TOKEN_COLUMN(token),
                           "MP_SCHEDTYPE", mp_dir_str[directive]);
                  parse_err_flush(Find_EOS, NULL);
                  goto EXIT;
               }

               if (LA_CH_VALUE == EQUAL) {

                  NEXT_LA_CH;
 
                  if (MATCHED_TOKEN_CLASS(Tok_Class_SGI_Dir_Kwd)) {

                     switch (TOKEN_VALUE(token)) {
                        case Tok_SGI_Dir_Simple:
                           the_constant = MP_SCHEDTYPE_SIMPLE;
                           break;
                        case Tok_SGI_Dir_Static:
                           the_constant = MP_SCHEDTYPE_SIMPLE;
                           break;
                        case Tok_SGI_Dir_Dynamic:
                           the_constant = MP_SCHEDTYPE_DYNAMIC;
                           break;
                        case Tok_SGI_Dir_Interleaved:
                           the_constant = MP_SCHEDTYPE_INTERLEAVED;
                           break;
                        case Tok_SGI_Dir_Interleave:
                           the_constant = MP_SCHEDTYPE_INTERLEAVED;
                           break;
                        case Tok_SGI_Dir_Runtime:
                           the_constant = MP_SCHEDTYPE_RUNTIME;
                           break;
                        case Tok_SGI_Dir_Gss:
                           the_constant = MP_SCHEDTYPE_GUIDED;
                           break;
                        case Tok_SGI_Dir_Guided:
                           the_constant = MP_SCHEDTYPE_GUIDED;
                           break;

                        default:
                           parse_err_flush(Find_EOS, "MP_SCHEDTYPE mode");
                           break;
                     }

                    
                     IL_LINE_NUM(list_array[MP_DIR_MP_SCHEDTYPE_IDX]) = 
                                        TOKEN_LINE(token);
                     IL_COL_NUM(list_array[MP_DIR_MP_SCHEDTYPE_IDX]) = 
                                        TOKEN_COLUMN(token);
                     IL_FLD(list_array[MP_DIR_MP_SCHEDTYPE_IDX]) = 
                                        CN_Tbl_Idx;
                     IL_IDX(list_array[MP_DIR_MP_SCHEDTYPE_IDX]) = 
                                            C_INT_TO_CN(CG_INTEGER_DEFAULT_TYPE,
                                                        the_constant);
                  }
                  else {
                     parse_err_flush(Find_EOS, "MP_SCHEDTYPE mode");
                  }

               }
               else {
                  parse_err_flush(Find_EOS, "=");
                  goto EXIT;
               }

               break;

            case Tok_SGI_Dir_Chunk:

               if (! clause_allowed[directive][Chunk_Clause]) {
                  PRINTMSG(TOKEN_LINE(token), 1370, Error, TOKEN_COLUMN(token),
                           "CHUNK", mp_dir_str[directive]);
                  parse_err_flush(Find_EOS, NULL);
                  goto EXIT;
               }

               if (IL_IDX(list_array[MP_DIR_CHUNK_IDX]) != NULL_IDX) {
                  PRINTMSG(TOKEN_LINE(token), 1360, Error, TOKEN_COLUMN(token),
                           "CHUNK or BLOCKED", mp_dir_str[directive]);
                  parse_err_flush(Find_EOS, NULL);
                  goto EXIT;
               }

               if (LA_CH_VALUE == EQUAL) {
                  NEXT_LA_CH;

                  parse_expr(&opnd);
                  COPY_OPND(IL_OPND(list_array[MP_DIR_CHUNK_IDX]),
                            opnd);
               }
               else {
                  parse_err_flush(Find_EOS, "=");
                  goto EXIT;
               }
               break;

            case Tok_SGI_Dir_Blocked:

               if (! clause_allowed[directive][Blocked_Clause]) {
                  PRINTMSG(TOKEN_LINE(token), 1370, Error, TOKEN_COLUMN(token),
                           "BLOCKED", mp_dir_str[directive]);
                  parse_err_flush(Find_EOS, NULL);
                  goto EXIT;
               }

               if (IL_IDX(list_array[MP_DIR_CHUNK_IDX]) != NULL_IDX) {
                  PRINTMSG(TOKEN_LINE(token), 1360, Error, TOKEN_COLUMN(token),
                           "CHUNK or BLOCKED", mp_dir_str[directive]);
                  parse_err_flush(Find_EOS, NULL);
                  goto EXIT;
               }
 
               if (LA_CH_VALUE == LPAREN) {
                  NEXT_LA_CH;
                  parse_expr(&opnd);
                  COPY_OPND(IL_OPND(list_array[MP_DIR_CHUNK_IDX]),
                            opnd);

                  if (LA_CH_VALUE == RPAREN) {
                     NEXT_LA_CH;
                  }
                  else {
                     parse_err_flush(Find_EOS, ")");
                     goto EXIT;
                  }
               }
               else {
                  parse_err_flush(Find_EOS, "(");
                  goto EXIT;
               }
               break;

            case Tok_SGI_Dir_Affinity:

               if (! clause_allowed[directive][Affinity_Clause]) {
                  PRINTMSG(TOKEN_LINE(token), 1370, Error, TOKEN_COLUMN(token),
                           "AFFINITY", mp_dir_str[directive]);
                  parse_err_flush(Find_EOS, NULL);
                  goto EXIT;
               }

               if (IL_IDX(list_array[MP_DIR_AFFINITY_IDX]) != NULL_IDX) {
                  PRINTMSG(TOKEN_LINE(token), 1360, Error, TOKEN_COLUMN(token),
                           "AFFINITY", mp_dir_str[directive]);
                  parse_err_flush(Find_EOS, NULL);
                  goto EXIT;
               }

               if (LA_CH_VALUE == LPAREN) {
                  NEXT_LA_CH;
                  parse_var_name_list(&opnd);

                  COPY_OPND(IL_OPND(list_array[MP_DIR_AFFINITY_IDX]), opnd);

                  if (LA_CH_VALUE == RPAREN) {
                     NEXT_LA_CH;
                  }
                  else {
                     parse_err_flush(Find_EOS, ")");
                     goto EXIT;
                  }
               }
               else {
                  parse_err_flush(Find_EOS, "(");
                  goto EXIT;
               }

               if (LA_CH_VALUE == EQUAL) {

                  NEXT_LA_CH;

                  if (MATCHED_TOKEN_CLASS(Tok_Class_SGI_Dir_Kwd)) {

                     IL_FLD(list_array[MP_DIR_IS_THREAD_IDX]) = CN_Tbl_Idx;
                     IL_LINE_NUM(list_array[MP_DIR_IS_THREAD_IDX]) = 
                                                 TOKEN_LINE(token);
                     IL_COL_NUM(list_array[MP_DIR_IS_THREAD_IDX]) = 
                                                 TOKEN_COLUMN(token);

                     switch (TOKEN_VALUE(token)) {
                        case Tok_SGI_Dir_Data:
                           IL_IDX(list_array[MP_DIR_IS_THREAD_IDX]) =
                                               CN_INTEGER_ZERO_IDX;
                           break;
                        case Tok_SGI_Dir_Thread:
                           IL_IDX(list_array[MP_DIR_IS_THREAD_IDX]) =
                                               CN_INTEGER_ONE_IDX;

                           break;

                        default:
                           parse_err_flush(Find_EOS, "DATA or THREAD");
                           break;
                     }

                     if (LA_CH_VALUE == LPAREN) {

                        NEXT_LA_CH;

                        parse_expr(&opnd);
      
                        COPY_OPND(IL_OPND(list_array[
                                     MP_DIR_THREAD_DATA_IDX]), opnd);

                        if (LA_CH_VALUE == RPAREN) {
                           NEXT_LA_CH;
                        }
                        else {
                           parse_err_flush(Find_EOS, ")");
                           goto EXIT;
                        }
                     }
                     else {
                        parse_err_flush(Find_EOS, "(");
                        goto EXIT;
                     }
                  }
                  else {
                     parse_err_flush(Find_EOS, "DATA or THREAD");
                  }

               }
               else {
                  parse_err_flush(Find_EOS, "=");
                  goto EXIT;
               }

               if (! dump_flags.dsm) {
                  opnd = null_opnd;

                  COPY_OPND(IL_OPND(list_array[MP_DIR_AFFINITY_IDX]),
                            opnd);
                  COPY_OPND(IL_OPND(list_array[MP_DIR_THREAD_DATA_IDX]),
                            opnd);
                  COPY_OPND(IL_OPND(list_array[MP_DIR_IS_THREAD_IDX]),
                            opnd);
               }
               break;

            case Tok_SGI_Dir_Local:
            case Tok_SGI_Dir_Private:

               if (! clause_allowed[directive][Local_Clause]) {
                  PRINTMSG(TOKEN_LINE(token), 1370, Error, TOKEN_COLUMN(token),
                           "LOCAL or PRIVATE", mp_dir_str[directive]);
                  parse_err_flush(Find_EOS, NULL);
                  goto EXIT;
               }

               if (LA_CH_VALUE == LPAREN) {
                  NEXT_LA_CH;
                  parse_var_name_list(&opnd);

                  if (IL_IDX(list_array[MP_DIR_LOCAL_IDX]) == NULL_IDX) {
                     COPY_OPND(IL_OPND(list_array[MP_DIR_LOCAL_IDX]), opnd);
                  }
                  else {
                     /* find the end of list */

                     list_idx = IL_IDX(list_array[MP_DIR_LOCAL_IDX]);
                     while (IL_NEXT_LIST_IDX(list_idx)) {
                        list_idx = IL_NEXT_LIST_IDX(list_idx);
                     }

                     /* append the new list */
                     IL_NEXT_LIST_IDX(list_idx) = OPND_IDX(opnd);
                     IL_PREV_LIST_IDX(OPND_IDX(opnd)) = list_idx;
                     IL_LIST_CNT(list_array[MP_DIR_LOCAL_IDX]) += 
                                                         OPND_LIST_CNT(opnd);
                  }

                  if (LA_CH_VALUE == RPAREN) {
                     NEXT_LA_CH;
                  }   
                  else {
                     parse_err_flush(Find_EOS, ")");
                     goto EXIT;
                  }
               }
               else {
                  parse_err_flush(Find_EOS, "(");
                  goto EXIT;
               }

               break;

            case Tok_SGI_Dir_Onto:
               if (! clause_allowed[directive][Onto_Clause]) {
                  PRINTMSG(TOKEN_LINE(token), 1370, Error, TOKEN_COLUMN(token),
                           "ONTO", mp_dir_str[directive]);
                  parse_err_flush(Find_EOS, NULL);
                  goto EXIT;
               }

               if (seen_nest) {

                  if (IL_IDX(list_array[MP_DIR_ONTO_IDX]) != NULL_IDX) {
                     PRINTMSG(TOKEN_LINE(token), 1360, Error, 
                              TOKEN_COLUMN(token),
                              "ONTO", mp_dir_str[directive]);
                     parse_err_flush(Find_EOS, NULL);
                     goto EXIT;
                  }

                  if (LA_CH_VALUE == LPAREN) {
                     NEXT_LA_CH;
                     parse_int_or_star_list(&opnd);
   
                     COPY_OPND(IL_OPND(list_array[MP_DIR_ONTO_IDX]), opnd);
   
                     if (LA_CH_VALUE == RPAREN) {
                        NEXT_LA_CH;
                     }
                     else {
                        parse_err_flush(Find_EOS, ")");
                        goto EXIT;
                     }
                  }
                  else {
                     parse_err_flush(Find_EOS, "(");
                     goto EXIT;
                  }

                  list_idx = list_array[MP_DIR_ONTO_IDX];

                  list2_idx = list_array[MP_DIR_NEST_IDX];

                  if (IL_FLD(list2_idx) != IL_Tbl_Idx ||
                      IL_LIST_CNT(list2_idx) != IL_LIST_CNT(list_idx)) {

                     /* error, onto count must equal nest count */

                     find_opnd_line_and_column(&IL_OPND(IL_IDX(list_idx)),
                                               &line, &column);
         
                     PRINTMSG(line, 1369, Error, column);
                  }
                  else if (IL_LIST_CNT(list2_idx) == 1) {
                     /* error, onto count must equal nest count */

                     find_opnd_line_and_column(&IL_OPND(IL_IDX(list_idx)),
                                               &line, &column);
         
                     PRINTMSG(line, 1377, Error, column);
                  }
               }
               else {
                  PRINTMSG(TOKEN_LINE(token), 1361, Error, TOKEN_COLUMN(token),
                           mp_dir_str[directive]);
                  parse_err_flush(Find_EOS, NULL);
                  goto EXIT;
               }
               break;

            case Tok_SGI_Dir_Nest:

               if (! clause_allowed[directive][Nest_Clause]) {
                  PRINTMSG(TOKEN_LINE(token), 1370, Error, TOKEN_COLUMN(token),
                           "NEST", mp_dir_str[directive]);
                  parse_err_flush(Find_EOS, NULL);
                  goto EXIT;
               }

               if (seen_nest) {
                  PRINTMSG(TOKEN_LINE(token), 1360, Error, TOKEN_COLUMN(token),
                           "NEST", mp_dir_str[directive]);
                  parse_err_flush(Find_EOS, NULL);
                  goto EXIT;
               }

               seen_nest = TRUE;

               if (LA_CH_VALUE == LPAREN) {
                  NEXT_LA_CH;
                  parse_var_name_list(&opnd);

                  COPY_OPND(IL_OPND(list_array[MP_DIR_NEST_IDX]), opnd);

                  if (LA_CH_VALUE == RPAREN) {
                     NEXT_LA_CH;
                  }
                  else {
                     parse_err_flush(Find_EOS, ")");
                     goto EXIT;
                  }
               }
               else {
                  parse_err_flush(Find_EOS, "(");
                  goto EXIT;
               }

               break;

            case Tok_SGI_Dir_Lastthread:

               if (! clause_allowed[directive][Lastthread_Clause]) {
                  PRINTMSG(TOKEN_LINE(token), 1370, Error, TOKEN_COLUMN(token),
                           "LASTTHREAD", mp_dir_str[directive]);
                  parse_err_flush(Find_EOS, NULL);
                  goto EXIT;
               }

               if (IL_IDX(list_array[MP_DIR_LASTTHREAD_IDX]) != NULL_IDX) {
                  PRINTMSG(TOKEN_LINE(token), 1360, Error, TOKEN_COLUMN(token),
                           "LASTTHREAD", mp_dir_str[directive]);
                  parse_err_flush(Find_EOS, NULL);
                  goto EXIT;
               }

               if (LA_CH_VALUE == LPAREN) {
                  NEXT_LA_CH;

                  if (MATCHED_TOKEN_CLASS(Tok_Class_Id)) {
   
                     if (! parse_deref(&opnd, NULL_IDX)) {
                        parse_err_flush(Find_Rparen, NULL);
                     }
                     else if (OPND_FLD(opnd) != AT_Tbl_Idx) {
                        find_opnd_line_and_column(&opnd, &line, &column);
                        PRINTMSG(line, 1376, Error, column);
                     }
                     else {
                        COPY_OPND(IL_OPND(list_array[MP_DIR_LASTTHREAD_IDX]),
                                  opnd);
                     }
                  }
                  else {
                     parse_err_flush(Find_Rparen, "IDENTIFIER");
                  }

                  if (LA_CH_VALUE == RPAREN) {
                     NEXT_LA_CH;
                  }
                  else {
                     parse_err_flush(Find_EOS, ")");
                     goto EXIT;
                  }
               }
               else {
                  parse_err_flush(Find_EOS, "(");
                  goto EXIT;
               }
               break;

            /* MODE clauses, same as MP_SCHEDTYPE */

            case Tok_SGI_Dir_Simple:
               if (! clause_allowed[directive][Mode_Clause]) {
                  PRINTMSG(TOKEN_LINE(token), 1370, Error, TOKEN_COLUMN(token),
                           "MODE", mp_dir_str[directive]);
                  parse_err_flush(Find_EOS, NULL);
                  goto EXIT;
               }

               if (IL_IDX(list_array[MP_DIR_MP_SCHEDTYPE_IDX]) != NULL_IDX) {
                  PRINTMSG(TOKEN_LINE(token), 1360, Error, TOKEN_COLUMN(token),
                           "MODE or MP_SCHEDTYPE", mp_dir_str[directive]);
                  parse_err_flush(Find_EOS, NULL);
                  goto EXIT;
               }

               IL_LINE_NUM(list_array[MP_DIR_MP_SCHEDTYPE_IDX]) =
                                        TOKEN_LINE(token);
               IL_COL_NUM(list_array[MP_DIR_MP_SCHEDTYPE_IDX]) =
                                        TOKEN_COLUMN(token);
               IL_FLD(list_array[MP_DIR_MP_SCHEDTYPE_IDX]) = CN_Tbl_Idx;
               IL_IDX(list_array[MP_DIR_MP_SCHEDTYPE_IDX]) =
                                           C_INT_TO_CN(CG_INTEGER_DEFAULT_TYPE,
                                                       MP_SCHEDTYPE_SIMPLE);

               break;

            case Tok_SGI_Dir_Static:
               if (! clause_allowed[directive][Mode_Clause]) {
                  PRINTMSG(TOKEN_LINE(token), 1370, Error, TOKEN_COLUMN(token),
                           "MODE", mp_dir_str[directive]);
                  parse_err_flush(Find_EOS, NULL);
                  goto EXIT;
               }

               if (IL_IDX(list_array[MP_DIR_MP_SCHEDTYPE_IDX]) != NULL_IDX) {
                  PRINTMSG(TOKEN_LINE(token), 1360, Error, TOKEN_COLUMN(token),
                           "MODE or MP_SCHEDTYPE", mp_dir_str[directive]);
                  parse_err_flush(Find_EOS, NULL);
                  goto EXIT;
               }

               IL_LINE_NUM(list_array[MP_DIR_MP_SCHEDTYPE_IDX]) =
                                        TOKEN_LINE(token);
               IL_COL_NUM(list_array[MP_DIR_MP_SCHEDTYPE_IDX]) =
                                        TOKEN_COLUMN(token);
               IL_FLD(list_array[MP_DIR_MP_SCHEDTYPE_IDX]) = CN_Tbl_Idx;
               IL_IDX(list_array[MP_DIR_MP_SCHEDTYPE_IDX]) =
                                           C_INT_TO_CN(CG_INTEGER_DEFAULT_TYPE,
                                                       MP_SCHEDTYPE_SIMPLE);

               break;

            case Tok_SGI_Dir_Dynamic:
               if (! clause_allowed[directive][Mode_Clause]) {
                  PRINTMSG(TOKEN_LINE(token), 1370, Error, TOKEN_COLUMN(token),
                           "MODE", mp_dir_str[directive]);
                  parse_err_flush(Find_EOS, NULL);
                  goto EXIT;
               }

               if (IL_IDX(list_array[MP_DIR_MP_SCHEDTYPE_IDX]) != NULL_IDX) {
                  PRINTMSG(TOKEN_LINE(token), 1360, Error, TOKEN_COLUMN(token),
                           "MODE or MP_SCHEDTYPE", mp_dir_str[directive]);
                  parse_err_flush(Find_EOS, NULL);
                  goto EXIT;
               }

               IL_LINE_NUM(list_array[MP_DIR_MP_SCHEDTYPE_IDX]) =
                                        TOKEN_LINE(token);
               IL_COL_NUM(list_array[MP_DIR_MP_SCHEDTYPE_IDX]) =
                                        TOKEN_COLUMN(token);
               IL_FLD(list_array[MP_DIR_MP_SCHEDTYPE_IDX]) = CN_Tbl_Idx;
               IL_IDX(list_array[MP_DIR_MP_SCHEDTYPE_IDX]) =
                                            C_INT_TO_CN(CG_INTEGER_DEFAULT_TYPE,
                                                        MP_SCHEDTYPE_DYNAMIC);

               break;

            case Tok_SGI_Dir_Interleaved:
               if (! clause_allowed[directive][Mode_Clause]) {
                  PRINTMSG(TOKEN_LINE(token), 1370, Error, TOKEN_COLUMN(token),
                           "MODE", mp_dir_str[directive]);
                  parse_err_flush(Find_EOS, NULL);
                  goto EXIT;
               }

               if (IL_IDX(list_array[MP_DIR_MP_SCHEDTYPE_IDX]) != NULL_IDX) {
                  PRINTMSG(TOKEN_LINE(token), 1360, Error, TOKEN_COLUMN(token),
                           "MODE or MP_SCHEDTYPE", mp_dir_str[directive]);
                  parse_err_flush(Find_EOS, NULL);
                  goto EXIT;
               }

               IL_LINE_NUM(list_array[MP_DIR_MP_SCHEDTYPE_IDX]) =
                                        TOKEN_LINE(token);
               IL_COL_NUM(list_array[MP_DIR_MP_SCHEDTYPE_IDX]) =
                                        TOKEN_COLUMN(token);
               IL_FLD(list_array[MP_DIR_MP_SCHEDTYPE_IDX]) = CN_Tbl_Idx;
               IL_IDX(list_array[MP_DIR_MP_SCHEDTYPE_IDX]) =
                                        C_INT_TO_CN(CG_INTEGER_DEFAULT_TYPE,
                                                    MP_SCHEDTYPE_INTERLEAVED);

               break;

            case Tok_SGI_Dir_Interleave:
               if (! clause_allowed[directive][Mode_Clause]) {
                  PRINTMSG(TOKEN_LINE(token), 1370, Error, TOKEN_COLUMN(token),
                           "MODE", mp_dir_str[directive]);
                  parse_err_flush(Find_EOS, NULL);
                  goto EXIT;
               }

               if (IL_IDX(list_array[MP_DIR_MP_SCHEDTYPE_IDX]) != NULL_IDX) {
                  PRINTMSG(TOKEN_LINE(token), 1360, Error, TOKEN_COLUMN(token),
                           "MODE or MP_SCHEDTYPE", mp_dir_str[directive]);
                  parse_err_flush(Find_EOS, NULL);
                  goto EXIT;
               }

               IL_LINE_NUM(list_array[MP_DIR_MP_SCHEDTYPE_IDX]) =
                                        TOKEN_LINE(token);
               IL_COL_NUM(list_array[MP_DIR_MP_SCHEDTYPE_IDX]) =
                                        TOKEN_COLUMN(token);
               IL_FLD(list_array[MP_DIR_MP_SCHEDTYPE_IDX]) = CN_Tbl_Idx;
               IL_IDX(list_array[MP_DIR_MP_SCHEDTYPE_IDX]) =
                                         C_INT_TO_CN(CG_INTEGER_DEFAULT_TYPE,
                                                     MP_SCHEDTYPE_INTERLEAVED);

               break;

            case Tok_SGI_Dir_Runtime:
               if (! clause_allowed[directive][Mode_Clause]) {
                  PRINTMSG(TOKEN_LINE(token), 1370, Error, TOKEN_COLUMN(token),
                           "MODE", mp_dir_str[directive]);
                  parse_err_flush(Find_EOS, NULL);
                  goto EXIT;
               }

               if (IL_IDX(list_array[MP_DIR_MP_SCHEDTYPE_IDX]) != NULL_IDX) {
                  PRINTMSG(TOKEN_LINE(token), 1360, Error, TOKEN_COLUMN(token),
                           "MODE or MP_SCHEDTYPE", mp_dir_str[directive]);
                  parse_err_flush(Find_EOS, NULL);
                  goto EXIT;
               }

               IL_LINE_NUM(list_array[MP_DIR_MP_SCHEDTYPE_IDX]) =
                                        TOKEN_LINE(token);
               IL_COL_NUM(list_array[MP_DIR_MP_SCHEDTYPE_IDX]) =
                                        TOKEN_COLUMN(token);
               IL_FLD(list_array[MP_DIR_MP_SCHEDTYPE_IDX]) = CN_Tbl_Idx;
               IL_IDX(list_array[MP_DIR_MP_SCHEDTYPE_IDX]) =
                                            C_INT_TO_CN(CG_INTEGER_DEFAULT_TYPE,
                                                        MP_SCHEDTYPE_RUNTIME);

               break;

            case Tok_SGI_Dir_Gss:
               if (! clause_allowed[directive][Mode_Clause]) {
                  PRINTMSG(TOKEN_LINE(token), 1370, Error, TOKEN_COLUMN(token),
                           "MODE", mp_dir_str[directive]);
                  parse_err_flush(Find_EOS, NULL);
                  goto EXIT;
               }

               if (IL_IDX(list_array[MP_DIR_MP_SCHEDTYPE_IDX]) != NULL_IDX) {
                  PRINTMSG(TOKEN_LINE(token), 1360, Error, TOKEN_COLUMN(token),
                           "MODE or MP_SCHEDTYPE", mp_dir_str[directive]);
                  parse_err_flush(Find_EOS, NULL);
                  goto EXIT;
               }

               IL_LINE_NUM(list_array[MP_DIR_MP_SCHEDTYPE_IDX]) =
                                        TOKEN_LINE(token);
               IL_COL_NUM(list_array[MP_DIR_MP_SCHEDTYPE_IDX]) =
                                        TOKEN_COLUMN(token);
               IL_FLD(list_array[MP_DIR_MP_SCHEDTYPE_IDX]) = CN_Tbl_Idx;
               IL_IDX(list_array[MP_DIR_MP_SCHEDTYPE_IDX]) =
                                            C_INT_TO_CN(CG_INTEGER_DEFAULT_TYPE,
                                                        MP_SCHEDTYPE_GUIDED);

               break;

            case Tok_SGI_Dir_Guided:
               if (! clause_allowed[directive][Mode_Clause]) {
                  PRINTMSG(TOKEN_LINE(token), 1370, Error, TOKEN_COLUMN(token),
                           "MODE", mp_dir_str[directive]);
                  parse_err_flush(Find_EOS, NULL);
                  goto EXIT;
               }

               if (IL_IDX(list_array[MP_DIR_MP_SCHEDTYPE_IDX]) != NULL_IDX) {
                  PRINTMSG(TOKEN_LINE(token), 1360, Error, TOKEN_COLUMN(token),
                          "MODE or MP_SCHEDTYPE", mp_dir_str[directive]);
                  parse_err_flush(Find_EOS, NULL);
                  goto EXIT;
               }

               IL_LINE_NUM(list_array[MP_DIR_MP_SCHEDTYPE_IDX]) =
                                        TOKEN_LINE(token);
               IL_COL_NUM(list_array[MP_DIR_MP_SCHEDTYPE_IDX]) =
                                        TOKEN_COLUMN(token);
               IL_FLD(list_array[MP_DIR_MP_SCHEDTYPE_IDX]) = CN_Tbl_Idx;
               IL_IDX(list_array[MP_DIR_MP_SCHEDTYPE_IDX]) =
                                            C_INT_TO_CN(CG_INTEGER_DEFAULT_TYPE,
                                                        MP_SCHEDTYPE_GUIDED);

               break;

            default:
               PRINTMSG(TOKEN_LINE(token), 1517, Error, TOKEN_COLUMN(token),
                        "mp");
               parse_err_flush(Find_EOS, NULL);
               break;
         }
      }
      else {
         parse_err_flush(Find_EOS, "mp clause");
      }

      if (LA_CH_VALUE == COMMA) {
         NEXT_LA_CH;
      }
   }


   if (clause_allowed[directive][Chunk_Clause] &&
       IL_IDX(list_array[MP_DIR_CHUNK_IDX]) == NULL_IDX &&
       OPND_FLD(cdir_switches.chunk_opnd) != NO_Tbl_Idx) {

      COPY_OPND(IL_OPND(list_array[MP_DIR_CHUNK_IDX]),
                cdir_switches.chunk_opnd);
   }

   if (clause_allowed[directive][Mp_Schedtype_Clause] &&
       IL_IDX(list_array[MP_DIR_MP_SCHEDTYPE_IDX]) == NULL_IDX &&
       OPND_FLD(cdir_switches.mp_schedtype_opnd) != NO_Tbl_Idx) {

      COPY_OPND(IL_OPND(list_array[MP_DIR_MP_SCHEDTYPE_IDX]),
                cdir_switches.mp_schedtype_opnd);
   }


EXIT:

   TRACE (Func_Exit, "parse_mp_directive", NULL);

   return;

}  /* parse_mp_directive */

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

static void parse_int_or_star_list(opnd_type *list_opnd)

{
   int          list_idx = NULL_IDX;
   opnd_type    opnd;


   TRACE (Func_Entry, "parse_int_or_star_list", NULL);

   while(TRUE) {

      if (LA_CH_VALUE == STAR) {
         OPND_FLD(opnd) = CN_Tbl_Idx;
         OPND_IDX(opnd) = CN_INTEGER_ZERO_IDX;
         OPND_LINE_NUM(opnd) = LA_CH_LINE;
         OPND_COL_NUM(opnd) = LA_CH_COLUMN;
         NEXT_LA_CH;
      }
      else {
         parse_expr(&opnd);
      }

      if (list_idx == NULL_IDX) {
         NTR_IR_LIST_TBL(list_idx);
         OPND_FLD((*list_opnd)) = IL_Tbl_Idx;
         OPND_IDX((*list_opnd)) = list_idx;
         OPND_LIST_CNT((*list_opnd)) = 1;
      }
      else {
         NTR_IR_LIST_TBL(IL_NEXT_LIST_IDX(list_idx));
         IL_PREV_LIST_IDX(IL_NEXT_LIST_IDX(list_idx)) = list_idx;
         (OPND_LIST_CNT((*list_opnd)))++;
         list_idx = IL_NEXT_LIST_IDX(list_idx);
      }
      COPY_OPND(IL_OPND(list_idx), opnd);

      if (LA_CH_VALUE != COMMA) {
         break;
      }
      NEXT_LA_CH;
   }


   TRACE (Func_Exit, "parse_int_or_star_list", NULL);

   return;

}  /* parse_int_or_star_list */

/******************************************************************************\
|*                                                                            *|
|* Description:                                                               *|
|*      This routine parses the variable lists that are possibly within       *|
|*      paranthesis and have only variable refs, not expressions.             *|
|*                                                                            *|
|* Input parameters:                                                          *|
|*      NONE                                                                  *|
|*                                                                            *|
|* Output parameters:                                                         *|
|*      opnd - points to list of attrs.                                       *|
|*                                                                            *|
|* Returns:                                                                   *|
|*      NOTHING                                                               *|
|*                                                                            *|
\******************************************************************************/

static void parse_reference_list(opnd_type *list_opnd)

{
   int          list_idx = NULL_IDX;
   opnd_type    opnd;


   TRACE (Func_Entry, "parse_reference_list", NULL);

   while(TRUE) {

      if (MATCHED_TOKEN_CLASS(Tok_Class_Id)) {
         parse_deref(&opnd, NULL_IDX);

         if (list_idx == NULL_IDX) {
            NTR_IR_LIST_TBL(list_idx);
            OPND_FLD((*list_opnd)) = IL_Tbl_Idx;
            OPND_IDX((*list_opnd)) = list_idx;
            OPND_LIST_CNT((*list_opnd)) = 1;
         }
         else {
            NTR_IR_LIST_TBL(IL_NEXT_LIST_IDX(list_idx));
            IL_PREV_LIST_IDX(IL_NEXT_LIST_IDX(list_idx)) = list_idx;
            (OPND_LIST_CNT((*list_opnd)))++;
            list_idx = IL_NEXT_LIST_IDX(list_idx);
         }
         COPY_OPND(IL_OPND(list_idx), opnd);
      }
      else {
         parse_err_flush(Find_Comma_Rparen, "IDENTIFIER");
      }

      if (LA_CH_VALUE != COMMA) {
         break;
      }
      NEXT_LA_CH;
   }

   TRACE (Func_Exit, "parse_reference_list", NULL);

   return;

}  /* parse_reference_list */

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

static void	parse_var_common_list(opnd_type	 *list_opnd,
				      boolean	  subobjects_allowed)

{
   int          	attr_idx;
   int          	column;
   int          	line;
   int          	list_idx	= NULL_IDX;
   int			name_idx;
   opnd_type    	opnd;
   int			sb_idx;
   token_values_type	token_value;


   TRACE (Func_Entry, "parse_var_common_list", NULL);

   token_value	= TOKEN_VALUE(token);

   while(TRUE) {

      if (LA_CH_VALUE == SLASH) {	/* must be common block */
         NEXT_LA_CH;    /* eat slash */

         if (MATCHED_TOKEN_CLASS(Tok_Class_Id)) {

            if (LA_CH_VALUE == SLASH) {
               NEXT_LA_CH;   /* eat slash */

               sb_idx = srch_stor_blk_tbl(TOKEN_STR(token),
                                          TOKEN_LEN(token),
                                          curr_scp_idx);

               if (sb_idx == NULL_IDX) {
                  sb_idx = ntr_stor_blk_tbl(TOKEN_STR(token),
                                            TOKEN_LEN(token),
                                            TOKEN_LINE(token),
                                            TOKEN_COLUMN(token),
                                            Common);
                  SB_COMMON_NEEDS_OFFSET(sb_idx) = TRUE;
                  SB_IS_COMMON(sb_idx)           = TRUE;
               }

               switch (token_value) {
               case Tok_SGI_Dir_Section_Gp:
                  SB_SECTION_GP(sb_idx)		= TRUE;
                  break;
               case Tok_SGI_Dir_Section_Non_Gp:
                  SB_SECTION_NON_GP(sb_idx)	= TRUE;
                  break;
               }

               if (list_idx == NULL_IDX) {
                  NTR_IR_LIST_TBL(list_idx);
                  OPND_FLD((*list_opnd)) = IL_Tbl_Idx;
                  OPND_IDX((*list_opnd)) = list_idx;
                  OPND_LIST_CNT((*list_opnd)) = 1;
               }
               else {
                  NTR_IR_LIST_TBL(IL_NEXT_LIST_IDX(list_idx));
                  IL_PREV_LIST_IDX(IL_NEXT_LIST_IDX(list_idx)) = list_idx;
                  (OPND_LIST_CNT((*list_opnd)))++;
                  list_idx = IL_NEXT_LIST_IDX(list_idx);
               }

               IL_IDX(list_idx)		= sb_idx;
               IL_FLD(list_idx)		= SB_Tbl_Idx;
               IL_LINE_NUM(list_idx)	= TOKEN_LINE(token);
               IL_COL_NUM(list_idx)	= TOKEN_COLUMN(token);
            }
            else {
               parse_err_flush(Find_Rparen, "/");
            }
         }
         else {
            parse_err_flush(Find_Comma_Rparen, "common-block-name");
         }
      }
      else if (MATCHED_TOKEN_CLASS(Tok_Class_Id)) {
         OPND_LINE_NUM(opnd)	= TOKEN_LINE(token);
         OPND_COL_NUM(opnd)	= TOKEN_COLUMN(token);

         if (token_value == Tok_SGI_Dir_Section_Gp) {
            attr_idx = srch_sym_tbl(TOKEN_STR(token),
                                    TOKEN_LEN(token),
                                    &name_idx);

           if (attr_idx == NULL_IDX) {
               attr_idx			= ntr_sym_tbl(&token, name_idx);
               LN_DEF_LOC(name_idx)	= TRUE;
               AT_OBJ_CLASS(attr_idx)	= Data_Obj;
               ATD_SECTION_GP(attr_idx) = TRUE;
               SET_IMPL_TYPE(attr_idx);
               OPND_IDX(opnd)		= attr_idx;
               OPND_FLD(opnd)		= AT_Tbl_Idx;
            }
            else if (fnd_semantic_err(Obj_Section_Gp,
                                      OPND_LINE_NUM(opnd),
                                      OPND_COL_NUM(opnd),
                                      attr_idx,
                                      TRUE)) {
                goto NEXT;
            }
             
            if (AT_OBJ_CLASS(attr_idx) == Pgm_Unit &&
                ATP_PGM_UNIT(attr_idx) == Module) {

               /* Specifying just the module name means that the directive */
               /* applies to the module's static storage.                  */

               if (attr_idx != SCP_ATTR_IDX(curr_scp_idx)) {

                  /* Must be the current module */

                  PRINTMSG(TOKEN_LINE(token), 1491, Error,
                           TOKEN_COLUMN(token),
                           "SECTION_GP");
               }
               else if (SB_SECTION_NON_GP(SCP_SB_STATIC_IDX(curr_scp_idx))) {
                  PRINTMSG(TOKEN_LINE(token), 1490, Error,
                           TOKEN_COLUMN(token),
                           AT_OBJ_NAME_PTR(attr_idx),
                           "SECTION_GP", "SECTION_NON_GP");
               }
               else {
                  SB_SECTION_GP(SCP_SB_STATIC_IDX(curr_scp_idx)) = TRUE;
               }

               OPND_IDX(opnd) = SCP_SB_STATIC_IDX(curr_scp_idx);
               OPND_FLD(opnd) = SB_Tbl_Idx;
            }
            else {
               ATD_SECTION_GP(attr_idx) = TRUE;
               OPND_IDX(opnd)		= attr_idx;
               OPND_FLD(opnd)		= AT_Tbl_Idx;
            }
         }
         else if (token_value == Tok_SGI_Dir_Section_Non_Gp) {
            attr_idx = srch_sym_tbl(TOKEN_STR(token),
                                    TOKEN_LEN(token),
                                    &name_idx);

           if (attr_idx == NULL_IDX) {
               attr_idx				= ntr_sym_tbl(&token, name_idx);
               LN_DEF_LOC(name_idx)		= TRUE;
               AT_OBJ_CLASS(attr_idx)		= Data_Obj;
               ATD_SECTION_NON_GP(attr_idx)	= TRUE;
               SET_IMPL_TYPE(attr_idx);
               OPND_IDX(opnd)			= attr_idx;
               OPND_FLD(opnd)			= AT_Tbl_Idx;
            }
            else if (fnd_semantic_err(Obj_Section_Non_Gp,
                                      OPND_LINE_NUM(opnd),
                                      OPND_COL_NUM(opnd),
                                      attr_idx,
                                      TRUE)) {
                goto NEXT;
            }

            if (AT_OBJ_CLASS(attr_idx) == Pgm_Unit &&
                ATP_PGM_UNIT(attr_idx) == Module) {

               /* Specifying just the module name means that the directive */
               /* applies to the module's static storage.                  */

               if (attr_idx != SCP_ATTR_IDX(curr_scp_idx)) {

                  /* Must be the current module */

                  PRINTMSG(TOKEN_LINE(token), 1491, Error,
                           TOKEN_COLUMN(token),
                           "SECTION_NON_GP");
               }
               else if (SB_SECTION_GP(SCP_SB_STATIC_IDX(curr_scp_idx))) {
                  PRINTMSG(TOKEN_LINE(token), 1490, Error,
                           TOKEN_COLUMN(token),
                           AT_OBJ_NAME_PTR(attr_idx),
                           "SECTION_NON_GP", "SECTION_GP");
               }
               else {
                  SB_SECTION_NON_GP(SCP_SB_STATIC_IDX(curr_scp_idx)) = TRUE;
               }
               OPND_IDX(opnd) = SCP_SB_STATIC_IDX(curr_scp_idx);
               OPND_FLD(opnd) = SB_Tbl_Idx;
            }
            else {
               ATD_SECTION_NON_GP(attr_idx) = TRUE;
               OPND_IDX(opnd)		= attr_idx;
               OPND_FLD(opnd)		= AT_Tbl_Idx;
            }
         }
         else {
            parse_deref(&opnd, NULL_IDX);
            find_opnd_line_and_column(&opnd, &line, &column);

            if (!subobjects_allowed && OPND_FLD(opnd) != AT_Tbl_Idx) {
               PRINTMSG(line, 802, Error, column);
               goto NEXT;
            }
         }

         if (list_idx == NULL_IDX) {
            NTR_IR_LIST_TBL(list_idx);
            OPND_FLD((*list_opnd))	= IL_Tbl_Idx;
            OPND_IDX((*list_opnd))	= list_idx;
            OPND_LIST_CNT((*list_opnd))	= 1;
         }
         else {
            NTR_IR_LIST_TBL(IL_NEXT_LIST_IDX(list_idx));
            IL_PREV_LIST_IDX(IL_NEXT_LIST_IDX(list_idx)) = list_idx;
            (OPND_LIST_CNT((*list_opnd)))++;
            list_idx = IL_NEXT_LIST_IDX(list_idx);
         }

         COPY_OPND(IL_OPND(list_idx), opnd);
      }
      else {
         parse_err_flush(Find_Comma_Rparen, "IDENTIFIER");
      }

NEXT:

      if (LA_CH_VALUE != COMMA) {
         break;
      }
      NEXT_LA_CH;
   }

   TRACE (Func_Exit, "parse_var_common_list", NULL);

   return;

}  /* parse_var_common_list */

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

static void parse_fill_align_symbol(void)

{
   int			align_symbol;
   int			attr_idx;
   int			ir_idx;
   int			name_idx;
   opnd_type		opnd;


   TRACE (Func_Entry, "parse_fill_align_symbol", NULL);

   ir_idx	= SH_IR_IDX(curr_stmt_sh_idx);
   align_symbol	= (TOKEN_VALUE(token) == Tok_SGI_Dir_Align_Symbol);
   expr_mode    = Specification_Expr;

   if (LA_CH_VALUE == LPAREN) {
      NEXT_LA_CH;

      if (MATCHED_TOKEN_CLASS(Tok_Class_Id)) {
         attr_idx = srch_sym_tbl(TOKEN_STR(token), TOKEN_LEN(token),
                                 &name_idx);

         if (attr_idx == NULL_IDX) {
            attr_idx			= ntr_sym_tbl(&token, name_idx);
            LN_DEF_LOC(name_idx)	= TRUE;
            AT_OBJ_CLASS(attr_idx)	= Data_Obj;
            SET_IMPL_TYPE(attr_idx);

            if (align_symbol) {
               ATD_ALIGN_SYMBOL(attr_idx)	= TRUE;
            }
            else {
               ATD_FILL_SYMBOL(attr_idx)	= TRUE;
            }

            IR_IDX_L(ir_idx)		= attr_idx;
            IR_FLD_L(ir_idx)		= AT_Tbl_Idx;
            IR_LINE_NUM_L(ir_idx)	= TOKEN_LINE(token);
            IR_COL_NUM_L(ir_idx)	= TOKEN_COLUMN(token);
         }
         else if (!fnd_semantic_err(align_symbol ? Obj_Align_Symbol :
                                                   Obj_Fill_Symbol,
                                    TOKEN_LINE(token),
                                    TOKEN_COLUMN(token),
                                    attr_idx,
                                    TRUE)) {

            if (AT_REFERENCED(attr_idx) == Char_Rslt_Bound_Ref) {
               AT_ATTR_LINK(attr_idx)	= NULL_IDX;
               LN_DEF_LOC(name_idx)	= TRUE;
            }

            if (align_symbol) {
               ATD_ALIGN_SYMBOL(attr_idx)	= TRUE;
            }
            else {
               ATD_FILL_SYMBOL(attr_idx)	= TRUE;
            }

            IR_IDX_L(ir_idx)		= attr_idx;
            IR_FLD_L(ir_idx)		= AT_Tbl_Idx;
            IR_LINE_NUM_L(ir_idx)	= TOKEN_LINE(token);
            IR_COL_NUM_L(ir_idx)	= TOKEN_COLUMN(token);
         }

         if (LA_CH_VALUE == LPAREN) {
            PRINTMSG(LA_CH_LINE, 1487, Error, LA_CH_COLUMN,
                     align_symbol ? "ALIGN_SYMBOL": "FILL_SYMBOL");
            parse_err_flush(Find_Rparen, NULL);

            if (LA_CH_VALUE == RPAREN) {
               NEXT_LA_CH;
            }
         }
      }
      else {
         parse_err_flush(Find_EOS, "variable-name");
         goto EXIT;
      }

      if (LA_CH_VALUE == COMMA) {
         NEXT_LA_CH;

         if (isdigit(LA_CH_VALUE)) {
            parse_expr(&opnd);
            COPY_OPND(IR_OPND_R(ir_idx), opnd);
         }
         else if (MATCHED_TOKEN_CLASS(Tok_Class_SGI_Dir_Kwd)) {

            switch (TOKEN_VALUE(token)) {
            case Tok_SGI_Dir_L1cacheline:
               IR_IDX_R(ir_idx)	= CN_INTEGER_NEG_ONE_IDX;
               break;

            case Tok_SGI_Dir_L2cacheline:
               IR_IDX_R(ir_idx)	= C_INT_TO_CN(CG_INTEGER_DEFAULT_TYPE, -2);
               break;

            case Tok_SGI_Dir_Page:
               IR_IDX_R(ir_idx)	= C_INT_TO_CN(CG_INTEGER_DEFAULT_TYPE, -3);
               break;

            default:
               parse_err_flush(Find_EOS, "L1cacheline, L2cacheline, or page");
               IR_IDX_R(ir_idx)	= CN_INTEGER_ZERO_IDX;
               break;
            }

            IR_FLD_R(ir_idx)		= CN_Tbl_Idx;
            IR_LINE_NUM_R(ir_idx)	= TOKEN_LINE(token);
            IR_COL_NUM_R(ir_idx)	= TOKEN_COLUMN(token);
         }
         else {
            parse_err_flush(Find_Rparen, "L1cacheline or L2cacheline or page");
         }

         if (LA_CH_VALUE == RPAREN) {
            NEXT_LA_CH;
         }
         else {
            parse_err_flush(Find_EOS, ")");
         }
      }
      else {
         parse_err_flush(Find_EOS, ",L1cacheline or L2cacheline or page");
      }
   }
   else {
      parse_err_flush(Find_EOS, "(");
   }

EXIT:

   expr_mode = Regular_Expr;

   TRACE (Func_Exit, "parse_fill_align_symbol", NULL);

   return;

}  /* parse_fill_align_symbol */

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

static void parse_sgi_dir_inline(boolean	turn_on)

{
   enum scope_entry {
			Here,
			Routine,
			Global
		    };

   typedef	enum	scope_entry	scope_type;

   boolean   		amb_ref		= FALSE;
#ifdef KEY /* Bug 10177 */
   int       		attr_idx = 0;
   int       		column = 0;
#else /* KEY Bug 10177 */
   int       		attr_idx;
   int       		column;
#endif /* KEY Bug 10177 */
   int       		host_attr_idx;
   int       		host_name_idx;
   boolean		inline_dir	= FALSE;
#ifdef KEY /* Bug 10177 */
   int			ir_idx = 0;
   int       		line = 0;
#else /* KEY Bug 10177 */
   int			ir_idx;
   int       		line;
#endif /* KEY Bug 10177 */
   int			list_idx;
   int       		name_idx;
   scope_type		scope		= Here;


   TRACE (Func_Entry, "parse_sgi_dir_inline", NULL);

   /*  NOTE - Currently !*$* IPA and !*$* INLINE are treated the same and  */
   /*         do the same things.  The only difference in this routine is  */
   /*         that we state the correct name when issuing semantic errors. */

   if (TOKEN_VALUE(token) == Tok_SGI_Dir_Inline ||
       TOKEN_VALUE(token) == Tok_SGI_Dir_Noinline) {
      inline_dir = TRUE;
   }

   if (LA_CH_VALUE != LPAREN) {

      if (MATCHED_TOKEN_CLASS(Tok_Class_SGI_Dir_Kwd)) {

         switch (TOKEN_VALUE(token)) {
            case Tok_SGI_Dir_Here:
               scope = Here;
               break;

            case Tok_SGI_Dir_Routine:
               scope = Routine;
               break;

            case Tok_SGI_Dir_Global:
               scope = Global;
               break;

            default:
               parse_err_flush(Find_EOS, "HERE, ROUTINE, or GLOBAL");
               goto EXIT;
         }
      }
   }

   if (scope == Here) {
      ir_idx = gen_directive_ir(turn_on ? Inline_Here_Star_Opr :
                                          Noinline_Here_Star_Opr);

      /* set this so an End_Inline_Here_Star_Opr is generated after */
      /* the next user statement.                                   */

      cdir_switches.inline_here_sgi = TRUE;
   }
   else if (scope == Routine) {
      ir_idx = gen_directive_ir(turn_on ? Inline_Routine_Star_Opr :
                                          Noinline_Routine_Star_Opr);
   }
   else if (scope == Global) {
      ir_idx = gen_directive_ir(turn_on ? Inline_Global_Star_Opr :
                                          Noinline_Global_Star_Opr);
   }

   if (LA_CH_VALUE == LPAREN) {
      NEXT_LA_CH;

      while (TRUE) {      /* have list */

         if (MATCHED_TOKEN_CLASS(Tok_Class_Id)) {
            line	= TOKEN_LINE(token);
            column	= TOKEN_COLUMN(token);

            attr_idx	= srch_sym_tbl(TOKEN_STR(token),
                                       TOKEN_LEN(token),
                                      &name_idx);

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
               amb_ref		= TRUE;
               host_attr_idx	= srch_host_sym_tbl(TOKEN_STR(token),
                                                    TOKEN_LEN(token),
                                                   &host_name_idx,
                                                    TRUE);

               if (host_attr_idx != NULL_IDX) { 

                  if (AT_IS_INTRIN(host_attr_idx) &&
                      ATI_FIRST_SPECIFIC_IDX(host_attr_idx) == NULL_IDX) {
                      complete_intrinsic_definition(host_attr_idx);
                      attr_idx = srch_sym_tbl(TOKEN_STR(token),
                                              TOKEN_LEN(token),
                                              &name_idx);
                  }

                  attr_idx = ntr_host_in_sym_tbl(&token,
                                                 name_idx,
                                                 host_attr_idx,
                                                 host_name_idx,
                                                 TRUE);

                  if (AT_IS_INTRIN(host_attr_idx)) {
                     COPY_VARIANT_ATTR_INFO(host_attr_idx,
                                            attr_idx,
                                            Interface);

                     AT_IS_INTRIN(attr_idx)	= TRUE;
                     AT_ATTR_LINK(attr_idx)	= NULL_IDX;
                     AT_ELEMENTAL_INTRIN(attr_idx) = 
                                           AT_ELEMENTAL_INTRIN(host_attr_idx);
                     AT_DEF_LINE(attr_idx)         = TOKEN_LINE(token);
                     AT_DEF_COLUMN(attr_idx)       = TOKEN_COLUMN(token);
                  }
                  else if (AT_OBJ_CLASS(attr_idx) != Interface) {
                     AT_ATTR_LINK(attr_idx) = host_attr_idx;
   
                     while (AT_ATTR_LINK(host_attr_idx) != NULL_IDX) {
                           host_attr_idx = AT_ATTR_LINK(host_attr_idx);
                     }
                  }
               }
            }

            if (attr_idx == NULL_IDX) {
               attr_idx                       = ntr_sym_tbl(&token, name_idx);
               AT_OBJ_CLASS(attr_idx)         = Pgm_Unit;
               ATP_PGM_UNIT(attr_idx)         = Pgm_Unknown;
               ATP_SCP_IDX(attr_idx)	      = curr_scp_idx;
               MAKE_EXTERNAL_NAME(attr_idx,
                                  AT_NAME_IDX(attr_idx),
                                  AT_NAME_LEN(attr_idx));
               ATP_PROC(attr_idx)             = Unknown_Proc;
            }
            else if (!amb_ref) {
      
               /* Allow the inline directive with user specified intrinsics */
               /* We will check for user specified intrinsics in decl_sem   */


               if (AT_OBJ_CLASS(attr_idx) == Interface && scope == Global) {

                  /* Allow the inline directive with generic */
                  /* interface.  Do not allow with GLOBAL.   */

                  PRINTMSG(line, 1654, Error, column,
                           AT_OBJ_NAME_PTR(attr_idx),
                           (inline_dir) ? "INLINE" : "IPA");
                  parse_err_flush(Find_EOS, NULL);
                  goto EXIT;
               }

               if (fnd_semantic_err((inline_dir ? Obj_Inline : Obj_Ipa),
                                     line,
                                     column,
                                     attr_idx,
                                     TRUE)) {
                  parse_err_flush(Find_EOS, NULL);
                  goto EXIT;
               }
            }
         }

         if (AT_OBJ_CLASS(attr_idx) == Data_Obj) { /* Switch to Function*/
             chg_data_obj_to_pgm_unit(attr_idx,
                                      Pgm_Unknown,
                                      Unknown_Proc);
         }

         if (AT_OBJ_CLASS(attr_idx) == Interface) {

            /* Set on the interface for now.  This will be */
            /* set on the specifics in decl_semantics.     */

            if (scope == Routine) {

               if (turn_on) {
                  ATI_SGI_ROUTINE_INLINE(attr_idx)   = TRUE;
                  ATI_SGI_ROUTINE_NOINLINE(attr_idx) = FALSE;
               }
               else {
                  ATI_SGI_ROUTINE_NOINLINE(attr_idx) = TRUE;
                  ATI_SGI_ROUTINE_INLINE(attr_idx)   = FALSE;
                  ATI_IPA_DIR_SPECIFIED(attr_idx)    = TRUE;
               }
            }
         }
         else if (scope == Routine) {

            if (turn_on) {
               ATP_SGI_ROUTINE_INLINE(attr_idx) = TRUE;
               ATP_SGI_ROUTINE_NOINLINE(attr_idx) = FALSE;
            }
            else {
               ATP_SGI_ROUTINE_NOINLINE(attr_idx) = TRUE;
               ATP_SGI_ROUTINE_INLINE(attr_idx) = FALSE;
            }
         }
         else if (scope == Global) {

            /* clear any routine dirs we've seen so far */

            ATP_SGI_ROUTINE_INLINE(attr_idx) = FALSE;
            ATP_SGI_ROUTINE_NOINLINE(attr_idx) = FALSE;

            if (turn_on) {
               ATP_SGI_GLOBAL_INLINE(attr_idx) = TRUE;
               ATP_SGI_GLOBAL_NOINLINE(attr_idx) = FALSE;
            }
            else {
               ATP_SGI_GLOBAL_NOINLINE(attr_idx) = TRUE;
               ATP_SGI_GLOBAL_INLINE(attr_idx) = FALSE;
            }

            host_attr_idx = AT_ATTR_LINK(attr_idx);

            while (host_attr_idx) {
               ATP_SGI_GLOBAL_INLINE(host_attr_idx) = 
                                     ATP_SGI_GLOBAL_INLINE(attr_idx);
               ATP_SGI_GLOBAL_NOINLINE(host_attr_idx) = 
                                     ATP_SGI_GLOBAL_NOINLINE(attr_idx);

               ATP_SGI_ROUTINE_INLINE(host_attr_idx) = FALSE;
               ATP_SGI_ROUTINE_NOINLINE(host_attr_idx) = FALSE;

               host_attr_idx = AT_ATTR_LINK(host_attr_idx);
            }
         }

         /* add to list */

         NTR_IR_LIST_TBL(list_idx);

         if (IR_FLD_L(ir_idx) == NO_Tbl_Idx) {
            IR_FLD_L(ir_idx) = IL_Tbl_Idx;
            IR_IDX_L(ir_idx) = list_idx;
            IR_LIST_CNT_L(ir_idx) = 1;
         }
         else {
            IL_NEXT_LIST_IDX(list_idx) = IR_IDX_L(ir_idx);
            IL_PREV_LIST_IDX(IR_IDX_L(ir_idx)) = list_idx;
            IR_IDX_L(ir_idx) = list_idx;
            IR_LIST_CNT_L(ir_idx) += 1;
         }

         IL_FLD(list_idx) = AT_Tbl_Idx;
         IL_IDX(list_idx) = attr_idx;
         IL_LINE_NUM(list_idx) = line;
         IL_COL_NUM(list_idx) = column;

         if (LA_CH_VALUE == COMMA) {
            NEXT_LA_CH;
         }
         else {
            break;
         }
      }

      if (LA_CH_VALUE == RPAREN) {
         NEXT_LA_CH;
      }
      else {
         parse_err_flush(Find_EOS, ")");
         goto EXIT;
      }
   }
   else if (scope == Global) { /* applies to all pgm units */

      if (turn_on) {
         inline_global_sgi = TRUE;
         noinline_global_sgi = FALSE;
      }
      else {
         noinline_global_sgi = TRUE;
         inline_global_sgi = FALSE;
      }
   }
   else if (scope == Routine) {

      if (turn_on) {
         SCP_INLINE_SGI(curr_scp_idx) = TRUE;
         SCP_NOINLINE_SGI(curr_scp_idx) = FALSE;
      }
      else {
         SCP_NOINLINE_SGI(curr_scp_idx) = TRUE;
         SCP_INLINE_SGI(curr_scp_idx) = FALSE;
      }
   }

EXIT:

   if (LA_CH_VALUE != EOS) {
      parse_err_flush(Find_EOS, EOS_STR);
   }

   TRACE (Func_Exit, "parse_sgi_dir_inline", NULL);

   return;

}  /* parse_sgi_dir_inline */

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

static void parse_distribution_dir(boolean	reshape)
				   

{
   int		attr_idx;
#ifdef KEY /* Bug 10177 */
   int		bd_idx = 0;
#else /* KEY Bug 10177 */
   int		bd_idx;
#endif /* KEY Bug 10177 */
   int		name_idx;
   int		onto_col;
   int		onto_line;
   int		onto_rank;
   opnd_type	opnd;
   int		rank;


   TRACE (Func_Entry, "parse_distribution_dir", NULL);

   while (TRUE) {

      if (MATCHED_TOKEN_CLASS(Tok_Class_Id)) {
         attr_idx = srch_sym_tbl(TOKEN_STR(token), TOKEN_LEN(token),
                                 &name_idx);

         if (attr_idx == NULL_IDX) {
            attr_idx                    = ntr_sym_tbl(&token, name_idx);
            LN_DEF_LOC(name_idx)        = TRUE;
            SET_IMPL_TYPE(attr_idx);
         }
         else {

            /* Do error checking */

            if (AT_REFERENCED(attr_idx) == Char_Rslt_Bound_Ref) {
               AT_ATTR_LINK(attr_idx)   = NULL_IDX;
               LN_DEF_LOC(name_idx)     = TRUE;
            }
         }

         if (LA_CH_VALUE == LPAREN) {
            rank                          = 0;
            bd_idx                        = reserve_array_ntry(7);
            BD_LINE_NUM(bd_idx)           = TOKEN_LINE(token);
            BD_COLUMN_NUM(bd_idx)         = TOKEN_COLUMN(token);
            BD_DIST_NTRY(bd_idx)          = TRUE;
            BD_DISTRIBUTE_RESHAPE(bd_idx) = reshape;

            do {
               rank++;
               NEXT_LA_CH;

               if (LA_CH_VALUE == STAR) {
                  NEXT_LA_CH;  /* Get star */

                  BD_DISTRIBUTION(bd_idx, rank) = Star_Distribution;
               }
               else if (MATCHED_TOKEN_CLASS(Tok_Class_SGI_Dir_Kwd)) {

                  if (TOKEN_VALUE(token) == Tok_SGI_Dir_Block) {
                     BD_DISTRIBUTION(bd_idx, rank) = Block_Distribution;
                  }
                  else if (TOKEN_VALUE(token) == Tok_SGI_Dir_Cyclic) {
                     BD_DISTRIBUTION(bd_idx, rank) = Cyclic_Distribution;

                     if (LA_CH_VALUE == LPAREN) { /* Have expression */
                        NEXT_LA_CH;

                        if (! parse_expr(&opnd)) {
                           BD_DCL_ERR(bd_idx) = TRUE;
                           parse_err_flush(Find_EOS, NULL);
                           goto EXIT;
                        }

                        BD_CYCLIC_FLD(bd_idx, rank) = OPND_FLD(opnd);
                        BD_CYCLIC_IDX(bd_idx, rank) = OPND_IDX(opnd);

                        if (LA_CH_VALUE != RPAREN) {
                           parse_err_flush(Find_EOS, ")");
                           BD_DCL_ERR(bd_idx) = TRUE;
                           goto EXIT;
                        }
                        else {
                           NEXT_LA_CH;
                        }
                     }
                  }
                  else {
                     parse_err_flush(Find_EOS, "BLOCK, CYCLIC or *");
                     BD_DCL_ERR(bd_idx) = TRUE;
                     goto EXIT;
                  }
               }
               else {
                  parse_err_flush(Find_EOS, "BLOCK, CYCLIC or *");
                  BD_DCL_ERR(bd_idx) = TRUE;
                  goto EXIT;
               }
            }
            while (LA_CH_VALUE == COMMA);

            if (LA_CH_VALUE != RPAREN) {
               parse_err_flush(Find_EOS, ")");
               BD_DCL_ERR(bd_idx) = TRUE;
               goto EXIT;
            }
            else {
               NEXT_LA_CH;
            }

            if (LA_CH_VALUE == COMMA) {
               /* intentionally blank */
            }
            else if (MATCHED_TOKEN_CLASS(Tok_Class_SGI_Dir_Kwd) &&
                     TOKEN_VALUE(token) == Tok_SGI_Dir_Onto) {

               onto_line = TOKEN_LINE(token);
               onto_col = TOKEN_COLUMN(token);

               if (LA_CH_VALUE == LPAREN) {
                  onto_rank       = 0;

                  do {
                     onto_rank++;
                     NEXT_LA_CH;

                     while (onto_rank <= rank &&
                            BD_DISTRIBUTION(bd_idx, onto_rank) ==
                                              Star_Distribution) {
                        /* no ONTO constants for Star_Distribution */
                        onto_rank++;
                     }

                     if (onto_rank > rank) {
                        /* too many ONTO values */
                        PRINTMSG(LA_CH_LINE, 1398, Error, LA_CH_COLUMN,
                                 "many");
                        parse_err_flush(Find_EOS, NULL);
                        BD_DCL_ERR(bd_idx) = TRUE;
                        goto EXIT;
                     }

                     if (LA_CH_VALUE == STAR) {
                        NEXT_LA_CH;
                        BD_ONTO_FLD(bd_idx,onto_rank) = CN_Tbl_Idx;
                        BD_ONTO_IDX(bd_idx,onto_rank) = CN_INTEGER_ZERO_IDX;
                     }
                     else {
                        parse_expr(&opnd);
                        BD_ONTO_FLD(bd_idx,onto_rank) = OPND_FLD(opnd);
                        BD_ONTO_IDX(bd_idx,onto_rank) = OPND_IDX(opnd);
                     }
                  }
                  while (LA_CH_VALUE == COMMA);

                  if (LA_CH_VALUE != RPAREN) {
                     parse_err_flush(Find_EOS, ")");
                     BD_DCL_ERR(bd_idx) = TRUE;
                     goto EXIT;
                  }
                  else {
                     NEXT_LA_CH;
                  }
               }
               else {
                  parse_err_flush(Find_EOS, "(");
                  BD_DCL_ERR(bd_idx) = TRUE;
                  goto EXIT;
               }

               while (onto_rank < rank) {
                  onto_rank++;

                  if (BD_DISTRIBUTION(bd_idx, onto_rank) !=
                                                 Star_Distribution) {
                     PRINTMSG(onto_line, 1398, Error, onto_col, "few");
                     parse_err_flush(Find_EOS, NULL);
                     BD_DCL_ERR(bd_idx) = TRUE;
                     goto EXIT;
                  }
               }
            }
            else if (LA_CH_VALUE != EOS) {
               parse_err_flush(Find_EOS, "ONTO or EOS");
               BD_DCL_ERR(bd_idx) = TRUE;
               goto EXIT;
            }

            BD_RANK(bd_idx)                = rank;
            ATD_DISTRIBUTION_IDX(attr_idx) = ntr_array_in_bd_tbl(bd_idx);
         }
         else {
            parse_err_flush(Find_EOS, "(");
            BD_DCL_ERR(bd_idx) = TRUE;
            goto EXIT;
         }
      }
      else {
         parse_err_flush(Find_EOS, "array name");
         BD_DCL_ERR(bd_idx) = TRUE;
         goto EXIT;
      }

      if (LA_CH_VALUE == COMMA) {
         NEXT_LA_CH;
      }
      else {
         break;
      }
   }

EXIT:


   TRACE (Func_Exit, "parse_distribution_dir", NULL);

   return;

}  /* parse_distribution_dir */

/******************************************************************************\
|*									      *|
|* Description:								      *|
|*                                                                            *|
|*            (Redistribute_Dollar_Opr)                                       *|
|*           /                         \                                      *|
|*       attr_idx                       |-> Dist_Spec list                    *|
|*                                      |                                     *|
|*                                      |-> Onto list                         *|
|*                                                                            *|
|*                                                                            *|
|*       One statement is created for each array specified.                   *|
|*                                                                            *|
|*       Keep this in synch with any changes to parse_distribution_dir.       *|
|*                                                                            *|
|*                                                                            *|
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

static void parse_redistribute_dir(void)
				   

{
   int		attr_idx;
   int		ir_idx;
   int		list_idx;
#ifdef KEY /* Bug 10177 */
   int		list_idx2 = 0;
   int		list_idx3 = 0;
#else /* KEY Bug 10177 */
   int		list_idx2;
   int		list_idx3;
#endif /* KEY Bug 10177 */
   int		name_idx;
   int		onto_col;
   int		onto_line;
   int		onto_rank;
   opnd_type	opnd;
   int		rank;


   TRACE (Func_Entry, "parse_redistribute_dir", NULL);

   while (TRUE) {

      if (MATCHED_TOKEN_CLASS(Tok_Class_Id)) {
         ir_idx = gen_directive_ir(Redistribute_Dollar_Opr);

         attr_idx = srch_sym_tbl(TOKEN_STR(token), TOKEN_LEN(token),
                                 &name_idx);

         if (attr_idx == NULL_IDX) {
            attr_idx                    = ntr_sym_tbl(&token, name_idx);
            LN_DEF_LOC(name_idx)        = TRUE;
            SET_IMPL_TYPE(attr_idx);
         }
         else {

            /* Do error checking */

            if (AT_REFERENCED(attr_idx) == Char_Rslt_Bound_Ref) {
               AT_ATTR_LINK(attr_idx)   = NULL_IDX;
               LN_DEF_LOC(name_idx)     = TRUE;
            }
         }

         IR_FLD_L(ir_idx) = AT_Tbl_Idx;
         IR_IDX_L(ir_idx) = attr_idx;
         IR_LINE_NUM_L(ir_idx) = TOKEN_LINE(token);
         IR_COL_NUM_L(ir_idx) = TOKEN_COLUMN(token);

         NTR_IR_LIST_TBL(list_idx);
         IR_FLD_R(ir_idx) = IL_Tbl_Idx;
         IR_IDX_R(ir_idx) = list_idx;
         IR_LIST_CNT_R(ir_idx) = 1;

         if (LA_CH_VALUE == LPAREN) {
            rank                          = 0;

            do {

               if (IL_FLD(list_idx) == NO_Tbl_Idx) {
                  NTR_IR_LIST_TBL(list_idx2);
                  IL_FLD(list_idx) = IL_Tbl_Idx;
                  IL_IDX(list_idx) = list_idx2;
                  IL_LIST_CNT(list_idx) = 1;
               }
               else {
                  NTR_IR_LIST_TBL(IL_NEXT_LIST_IDX(list_idx2));
                  IL_PREV_LIST_IDX(IL_NEXT_LIST_IDX(list_idx2)) = list_idx2;  
                  list_idx2 = IL_NEXT_LIST_IDX(list_idx2);
                  IL_LIST_CNT(list_idx) += 1;
               }

               rank++;
               NEXT_LA_CH;

               IL_DISTRIBUTION_VARIANT(list_idx2) = TRUE;

               if (LA_CH_VALUE == STAR) {
                  NEXT_LA_CH;  /* Get star */

                  IL_DISTRIBUTION(list_idx2) = Star_Distribution;
               }
               else if (MATCHED_TOKEN_CLASS(Tok_Class_SGI_Dir_Kwd)) {

                  if (TOKEN_VALUE(token) == Tok_SGI_Dir_Block) {
                     IL_DISTRIBUTION(list_idx2) = Block_Distribution;
                  }
                  else if (TOKEN_VALUE(token) == Tok_SGI_Dir_Cyclic) {
                     IL_DISTRIBUTION(list_idx2) = Cyclic_Distribution;

                     if (LA_CH_VALUE == LPAREN) { /* Have expression */
                        NEXT_LA_CH;

                        if (! parse_expr(&opnd)) {
                           parse_err_flush(Find_EOS, NULL);
                           goto EXIT;
                        }

                        COPY_OPND(IL_OPND(list_idx2), opnd);

                        if (LA_CH_VALUE != RPAREN) {
                           parse_err_flush(Find_EOS, ")");
                           goto EXIT;
                        }
                        else {
                           NEXT_LA_CH;
                        }
                     }
                  }
                  else {
                     parse_err_flush(Find_EOS, "BLOCK, CYCLIC or *");
                     goto EXIT;
                  }
               }
               else {
                  parse_err_flush(Find_EOS, "BLOCK, CYCLIC or *");
                  goto EXIT;
               }
            }
            while (LA_CH_VALUE == COMMA);

            if (LA_CH_VALUE != RPAREN) {
               parse_err_flush(Find_EOS, ")");
               goto EXIT;
            }
            else {
               NEXT_LA_CH;
            }

            if (LA_CH_VALUE == COMMA) {
               /* intentionally blank */
            }
            else if (MATCHED_TOKEN_CLASS(Tok_Class_SGI_Dir_Kwd) &&
                     TOKEN_VALUE(token) == Tok_SGI_Dir_Onto) {

               NTR_IR_LIST_TBL(IL_NEXT_LIST_IDX(list_idx));
               IL_PREV_LIST_IDX(IL_NEXT_LIST_IDX(list_idx)) = list_idx;
               list_idx2 = IL_IDX(list_idx);
               list_idx = IL_NEXT_LIST_IDX(list_idx);
               IR_LIST_CNT_R(ir_idx) = 2;

               onto_line = TOKEN_LINE(token);
               onto_col = TOKEN_COLUMN(token);

               if (LA_CH_VALUE == LPAREN) {
                  onto_rank       = 0;

                  do {

                     if (IL_FLD(list_idx) == NO_Tbl_Idx) {
                        NTR_IR_LIST_TBL(list_idx3);
                        IL_FLD(list_idx) = IL_Tbl_Idx;
                        IL_IDX(list_idx) = list_idx3;
                        IL_LIST_CNT(list_idx) = 1;
                     }
                     else {
                        NTR_IR_LIST_TBL(IL_NEXT_LIST_IDX(list_idx3));
                        IL_PREV_LIST_IDX(IL_NEXT_LIST_IDX(list_idx3)) = 
                                                         list_idx3;
                        list_idx3 = IL_NEXT_LIST_IDX(list_idx3);
                        IL_LIST_CNT(list_idx) += 1;
                     }

                     onto_rank++;
                     NEXT_LA_CH;

                     while (onto_rank <= rank &&
                            list_idx2 != NULL_IDX &&
                            IL_DISTRIBUTION(list_idx2) == Star_Distribution) {
                        /* no ONTO constants for Star_Distribution */
                        onto_rank++;

                        NTR_IR_LIST_TBL(IL_NEXT_LIST_IDX(list_idx3));
                        IL_PREV_LIST_IDX(IL_NEXT_LIST_IDX(list_idx3)) =
                                                         list_idx3;
                        list_idx3 = IL_NEXT_LIST_IDX(list_idx3);
                        IL_LIST_CNT(list_idx) += 1;

                        if (list_idx2 != NULL_IDX) {
                           list_idx2 = IL_NEXT_LIST_IDX(list_idx2);
                        }
                     }

                     if (onto_rank > rank) {
                        /* too many ONTO values */
                        PRINTMSG(LA_CH_LINE, 1398, Error, LA_CH_COLUMN,
                                 "many");
                        parse_err_flush(Find_EOS, NULL);
                        goto EXIT;
                     }

                     if (LA_CH_VALUE == STAR) {
                        IL_FLD(list_idx3) = CN_Tbl_Idx;
                        IL_IDX(list_idx3) = CN_INTEGER_ZERO_IDX;
                        IL_LINE_NUM(list_idx3) = LA_CH_LINE;
                        IL_COL_NUM(list_idx3) = LA_CH_COLUMN;
                        NEXT_LA_CH;
                     }
                     else {
                        parse_expr(&opnd);
                        COPY_OPND(IL_OPND(list_idx3), opnd);
                     }

                     if (list_idx2 != NULL_IDX) {
                        list_idx2 = IL_NEXT_LIST_IDX(list_idx2);
                     }
                  }
                  while (LA_CH_VALUE == COMMA);

                  if (LA_CH_VALUE != RPAREN) {
                     parse_err_flush(Find_EOS, ")");
                     goto EXIT;
                  }
                  else {
                     NEXT_LA_CH;
                  }
               }
               else {
                  parse_err_flush(Find_EOS, "(");
                  goto EXIT;
               }

               while (onto_rank < rank) {
                  onto_rank++;

                  if (IL_DISTRIBUTION(list_idx2) != Star_Distribution) {
                     PRINTMSG(onto_line, 1398, Error, onto_col, "few");
                     parse_err_flush(Find_EOS, NULL);
                     goto EXIT;
                  }

                  if (list_idx2 != NULL_IDX) {
                     list_idx2 = IL_NEXT_LIST_IDX(list_idx2);
                  }
               }
            }
            else if (LA_CH_VALUE != EOS) {
               parse_err_flush(Find_EOS, "ONTO or EOS");
               goto EXIT;
            }
         }
         else {
            parse_err_flush(Find_EOS, "(");
            goto EXIT;
         }
      }
      else {
         parse_err_flush(Find_EOS, "array name");
         goto EXIT;
      }

      if (LA_CH_VALUE == COMMA) {
         NEXT_LA_CH;
      }
      else {
         break;
      }
   }

EXIT:


   TRACE (Func_Exit, "parse_redistribute_dir", NULL);

   return;

}  /* parse_redistribute_dir */

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

static boolean parse_assert_directive(void)

{
   int		column;
   int		ir_idx;
   int		line;
   boolean	ok = TRUE;
   opnd_type	opnd;
   long		the_constant;
   long		the_constant2;


   TRACE (Func_Entry, "parse_assert_directive", NULL);

   if (MATCHED_TOKEN_CLASS(Tok_Class_SGI_Dir_Kwd)) {

      ir_idx = gen_directive_ir(Assert_Star_Opr);
      IR_LINE_NUM_L(ir_idx) = TOKEN_LINE(token);
      IR_COL_NUM_L(ir_idx) = TOKEN_COLUMN(token);
      line = TOKEN_LINE(token);
      column = TOKEN_COLUMN(token);

      switch (TOKEN_VALUE(token)) {
         case Tok_SGI_Dir_Argumentaliasing:
            the_constant = ASSERT_ARGUMENTALIASING;
            break;

         case Tok_SGI_Dir_Noargumentaliasing:
            the_constant = ASSERT_NOARGUMENTALIASING;
            break;

         case Tok_SGI_Dir_Boundsviolations:
            the_constant = ASSERT_BOUNDSVIOLATIONS;
            break;

         case Tok_SGI_Dir_Noboundsviolations:
            the_constant = ASSERT_NOBOUNDSVIOLATIONS;
            break;

         case Tok_SGI_Dir_Concurrentcall:
            the_constant = ASSERT_CONCURRENTCALL;
            break;

         case Tok_SGI_Dir_Noconcurrentcall:
            the_constant = ASSERT_NOCONCURRENTCALL;
            break;

         case Tok_SGI_Dir_Norecurrence:
            the_constant = ASSERT_NORECURRENCE;

            if (LA_CH_VALUE == LPAREN) {
               NEXT_LA_CH;

               parse_var_name_list(&opnd);
               COPY_OPND(IR_OPND_R(ir_idx), opnd);

               if (LA_CH_VALUE == RPAREN) {
                  NEXT_LA_CH;
               }
               else {
                  parse_err_flush(Find_EOS, ")");
                  ok = FALSE;
               }
            }
            else {
               parse_err_flush(Find_EOS, "(");
               ok = FALSE;
            }

            break;

         case Tok_SGI_Dir_Doprefer:
            the_constant = ASSERT_DOPREFER;

            if (LA_CH_VALUE == LPAREN) {
               NEXT_LA_CH;

               if (MATCHED_TOKEN_CLASS(Tok_Class_SGI_Dir_Kwd)) {
                  switch (TOKEN_VALUE(token)) {
                  case Tok_SGI_Dir_Concur:
                  case Tok_SGI_Dir_Concurrent:
                     the_constant2 = DOPREFER_CONCURRENT;
                     break;

                  case Tok_SGI_Dir_Serial:
                     the_constant2 = DOPREFER_SERIAL;
                     break;

                  case Tok_SGI_Dir_Tile:
                  case Tok_SGI_Dir_Vector:
                     the_constant2 = DOPREFER_VECTOR;
                     break;

                  default:
                     the_constant2 = 0;
                     parse_err_flush(Find_EOS, "PREFERENCE");
                     ok = FALSE;
                     break;
                  }

                  IR_LINE_NUM_R(ir_idx) = TOKEN_LINE(token);
                  IR_COL_NUM_R(ir_idx) = TOKEN_COLUMN(token);
                  IR_FLD_R(ir_idx) = CN_Tbl_Idx;
                  IR_IDX_R(ir_idx) = C_INT_TO_CN(CG_INTEGER_DEFAULT_TYPE,
                                                 the_constant2);

                  if (LA_CH_VALUE != RPAREN) {
                     parse_err_flush(Find_EOS, ")");
                  }
                  else {
                     NEXT_LA_CH;
                  }

               }
               else {
                  parse_err_flush(Find_EOS, "PREFERENCE");
                  ok = FALSE;
               }
            }
            else {
               parse_err_flush(Find_EOS, "(");
               ok = FALSE;
            }
            break;

         case Tok_SGI_Dir_Equivalencehazard:
            the_constant = ASSERT_EQUIVALENCEHAZARD;
            break;

         case Tok_SGI_Dir_Noequivalencehazard:
            the_constant = ASSERT_NOEQUIVALENCEHAZARD;
            break;

         case Tok_SGI_Dir_Lastvalueneeded:
            the_constant = ASSERT_LASTVALUENEEDED;
            PRINTMSG(TOKEN_LINE(token), 801, Warning, TOKEN_COLUMN(token));
            parse_err_flush(Find_EOS, NULL);
            break;

         case Tok_SGI_Dir_Lastvaluesneeded:
            the_constant = ASSERT_LASTVALUESNEEDED;
            PRINTMSG(TOKEN_LINE(token), 801, Warning, TOKEN_COLUMN(token));
            parse_err_flush(Find_EOS, NULL);
            break;

         case Tok_SGI_Dir_Nolastvalueneeded:
            the_constant = ASSERT_NOLASTVALUENEEDED;
            PRINTMSG(TOKEN_LINE(token), 801, Warning, TOKEN_COLUMN(token));
            parse_err_flush(Find_EOS, NULL);
            break;

         case Tok_SGI_Dir_Nolastvaluesneeded:
            the_constant = ASSERT_NOLASTVALUESNEEDED;
            PRINTMSG(TOKEN_LINE(token), 801, Warning, TOKEN_COLUMN(token));
            parse_err_flush(Find_EOS, NULL);
            break;

         case Tok_SGI_Dir_Permutation:
            the_constant = ASSERT_PERMUTATION;

            if (LA_CH_VALUE == LPAREN) {
               NEXT_LA_CH;

               if (MATCHED_TOKEN_CLASS(Tok_Class_Id)) {
                  ok = parse_deref(&opnd, NULL_IDX);

                  if (OPND_FLD(opnd) != AT_Tbl_Idx) {
                     ok = FALSE;
                     find_opnd_line_and_column(&opnd, &line, &column);
                     PRINTMSG(line, 1374, Error, column);
                  }
                  else {
                     COPY_OPND(IR_OPND_R(ir_idx), opnd);
                  }

                  if (LA_CH_VALUE == RPAREN) {
                     NEXT_LA_CH;
                  }
                  else {
                     parse_err_flush(Find_EOS, "(");
                     ok = FALSE;
                  }
               }
               else {
                  parse_err_flush(Find_EOS, "IDENTIFIER");
                  ok = FALSE;
               }
            }
            else {
               parse_err_flush(Find_EOS, "(");
               ok = FALSE;
            }
            break;

         case Tok_SGI_Dir_Relation:
            the_constant = ASSERT_RELATION;
            PRINTMSG(TOKEN_LINE(token), 801, Warning, TOKEN_COLUMN(token));
            parse_err_flush(Find_EOS, NULL);
            break;

         case Tok_SGI_Dir_Nosync:
            the_constant = ASSERT_NOSYNC;
            PRINTMSG(TOKEN_LINE(token), 801, Warning, TOKEN_COLUMN(token));
            parse_err_flush(Find_EOS, NULL);
            break;

         case Tok_SGI_Dir_Temporariesforconstantarguments:
            the_constant = ASSERT_TEMPORARIESFORCONSTANTARGUMENTS;
            break;

         case Tok_SGI_Dir_Notemporariesforconstantarguments:
            the_constant = ASSERT_NOTEMPORARIESFORCONSTANTARGUMENTS;
            /* flush the remaining characters, the token is too long */
            parse_err_flush(Find_EOS, NULL);
            break;

         case Tok_SGI_Dir_Do:
            the_constant = ASSERT_DO;

            if (LA_CH_VALUE == LPAREN) {
               NEXT_LA_CH;

               if (MATCHED_TOKEN_CLASS(Tok_Class_SGI_Dir_Kwd)) {
                  switch (TOKEN_VALUE(token)) {
                  case Tok_SGI_Dir_Concur:
                  case Tok_SGI_Dir_Concurrent:
                     the_constant2 = DOPREFER_CONCURRENT;
                     break;

                  case Tok_SGI_Dir_Serial:
                     the_constant2 = DOPREFER_SERIAL;
                     break;

                  case Tok_SGI_Dir_Tile:
                  case Tok_SGI_Dir_Vector:
                     the_constant2 = DOPREFER_VECTOR;
                     break;

                  default:
                     the_constant2 = 0;
                     parse_err_flush(Find_EOS, "PREFERENCE");
                     ok = FALSE;
                     break;
                  }

                  IR_LINE_NUM_R(ir_idx) = TOKEN_LINE(token);
                  IR_COL_NUM_R(ir_idx) = TOKEN_COLUMN(token);
                  IR_FLD_R(ir_idx) = CN_Tbl_Idx;
                  IR_IDX_R(ir_idx) = C_INT_TO_CN(CG_INTEGER_DEFAULT_TYPE,
                                                 the_constant2);

                  if (LA_CH_VALUE != RPAREN) {
                     parse_err_flush(Find_EOS, ")");
                  }
                  else {
                     NEXT_LA_CH;
                  }

               }
               else {
                  parse_err_flush(Find_EOS, "PREFERENCE");
                  ok = FALSE;
               }
            }
            else {
               parse_err_flush(Find_EOS, "(");
               ok = FALSE;
            }
            break;

         case Tok_SGI_Dir_Benign:
            the_constant = ASSERT_BENIGN;
            break;

         case Tok_SGI_Dir_Dependence:
            the_constant = ASSERT_DEPENDENCE;
            break;

         case Tok_SGI_Dir_Frequency:
            the_constant = ASSERT_FREQUENCY;
            break;

         case Tok_SGI_Dir_Ignoreanydependences:
            the_constant = ASSERT_IGNOREANYDEPENDENCES;
            break;

         case Tok_SGI_Dir_Ignoreanydependence:
            the_constant = ASSERT_IGNOREANYDEPENDENCE;
            break;

         case Tok_SGI_Dir_Ignoreassumeddependences:
            the_constant = ASSERT_IGNOREASSUMEDDEPENDENCES;
            break;

         case Tok_SGI_Dir_Ignoreassumeddependence:
            the_constant = ASSERT_IGNOREASSUMEDDEPENDENCE;
            break;

         case Tok_SGI_Dir_Nointerchange:
            the_constant = ASSERT_NOINTERCHANGE;
            break;

         case Tok_SGI_Dir_Usecompress:
            the_constant = ASSERT_USECOMPRESS;
            break;

         case Tok_SGI_Dir_Useexpand:
            the_constant = ASSERT_USEEXPAND;
            break;

         case Tok_SGI_Dir_Usecontrolledstore:
            the_constant = ASSERT_USECONTROLLEDSTORE;
            break;

         case Tok_SGI_Dir_Usegather:
            the_constant = ASSERT_USEGATHER;
            break;

         case Tok_SGI_Dir_Usescatter:
            the_constant = ASSERT_USESCATTER;
            break;

         default:
            PRINTMSG(TOKEN_LINE(token), 1354, Warning, TOKEN_COLUMN(token),
                     TOKEN_STR(token));
            parse_err_flush(Find_EOS, NULL);
            ok = FALSE;
            goto EXIT;
      }

      IR_FLD_L(ir_idx) = CN_Tbl_Idx;
      IR_IDX_L(ir_idx) = C_INT_TO_CN(CG_INTEGER_DEFAULT_TYPE, the_constant);

      if (directives_are_global) {
         /* copy the assert into the global ir table */
         gen_gl_sh(After, Directive_Stmt, line, column,
                   FALSE, FALSE, TRUE);
         GL_SH_IR_IDX(curr_gl_stmt_sh_idx) = copy_to_gl_subtree(ir_idx,
                                                                IR_Tbl_Idx);
      }
   }
   else {
      parse_err_flush(Find_EOS, "ASSERTION");
      ok = FALSE;
   }

EXIT:

   TRACE (Func_Exit, "parse_assert_directive", NULL);

   return(ok);

}  /* parse_assert_directive */

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

static boolean directive_region_error(directive_stmt_type	dir,
				      int			line,
				      int			col)

{
   int			count = 0;
   boolean		error = FALSE;
   int			region;
   long			mask;
   char			str[80];
   char			str2[80];

   TRACE (Func_Entry, "directive_region_error", NULL);

   if ((directive_cant_be_in[dir] & directive_state) != 0) {
      mask = (directive_cant_be_in[dir] & directive_state);
      error = TRUE;
      
      for (region = 0; region < Last_Region; region++) {
         if (((mask >> region) & 1) != 0) {
            break;
         }
      }

# ifdef _DEBUG
      if (region == Last_Region) {
         PRINTMSG(stmt_start_line, 626, Internal, stmt_start_col,
                  "region error", "directive_region_error");
      }
# endif

      PRINTMSG(line, 1411, Error, col,
               directive_stmt_str[dir],
               directive_region_str[region]);
   }
   else if (directive_must_be_in[dir] != 0 &&
            (directive_must_be_in[dir] & directive_state) == 0) {
      error = TRUE;
      str[0] = '\0';

      for (region = 0; region < Last_Region; region++) {
         if (((directive_must_be_in[dir] >> region) & 1) != 0) {
            count++;
            if (count > 1) {
               sprintf(str2, ", or %s", directive_region_str[region]);
            }
            else {
               sprintf(str2, "%s", directive_region_str[region]);
            }
            strcat(str, str2);
         }
      }

      PRINTMSG(line, 1412, Error, col,
               directive_stmt_str[dir],
               str);
   }

   TRACE (Func_Exit, "directive_region_error", NULL);

   return(error);

}  /* directive_region_error */

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

static void parse_id_directive(void)

{
   int			column;
   int			init_idx;
   size_offset_type	length;
   int			line;
   int			list_idx;
   id_str_type		name;
   opnd_type		opnd;
   opnd_type		opnd2;
   size_offset_type	result;
   int			sb_idx;
   int			tmp_idx;


   TRACE (Func_Entry, "parse_id_directive", NULL);

   if (parse_expr(&opnd)) {
      find_opnd_line_and_column(&opnd, &line, &column);

      if (OPND_FLD(opnd)!= CN_Tbl_Idx ||
          TYP_TYPE(CN_TYPE_IDX(OPND_IDX(opnd))) != Character) {
         PRINTMSG(line, 874, Error, column);
      }
      else { /* get temp, initialize, put in named static block */
         tmp_idx = gen_compiler_tmp(line, column, Shared, TRUE);
         ATD_TYPE_IDX(tmp_idx) = CN_TYPE_IDX(OPND_IDX(opnd));
         ATD_TMP_SEMANTICS_DONE(tmp_idx) = TRUE;

         CREATE_ID(name, sb_name[What_Blk], sb_len[What_Blk]);

         sb_idx	= srch_stor_blk_tbl(name.string, sb_len[What_Blk],curr_scp_idx);

         if (sb_idx == NULL_IDX) {
            sb_idx = ntr_stor_blk_tbl(name.string, 
                                      sb_len[What_Blk],
                                      line,
                                      column,
# if (defined(_TARGET_OS_IRIX) || defined(_TARGET_OS_LINUX) || defined(_TARGET_OS_DARWIN))
                                      Coment); /* different class on IRIX */
# else
                                      Static_Named);
# endif
            SB_SAVED(sb_idx) = TRUE;
         }

         ATD_STOR_BLK_IDX(tmp_idx) = sb_idx;

# if (defined(_TARGET_OS_IRIX) || defined(_TARGET_OS_LINUX) || defined(_TARGET_OS_DARWIN))
         /* Attach the string in CDIR$ ID "string" to the compiler temp. */
         ATD_TMP_IDX(tmp_idx) = OPND_IDX(opnd);
         ATD_FLD(tmp_idx) = CN_Tbl_Idx;
# endif

# if defined(_DEBUG)
         if (SB_LEN_FLD(sb_idx) != CN_Tbl_Idx) {
            PRINTMSG(line, 1201, Internal, column, SB_NAME_PTR(sb_idx));
         }
# endif
         ATD_OFFSET_ASSIGNED(tmp_idx)	= TRUE;
         ATD_OFFSET_IDX(tmp_idx)	= SB_LEN_IDX(sb_idx);
         ATD_OFFSET_FLD(tmp_idx)	= SB_LEN_FLD(sb_idx);

         /* 8 times number of chars to get bit length. */

         result.idx	= CN_INTEGER_CHAR_BIT_IDX;
         result.fld	= CN_Tbl_Idx;
         length.idx	= TYP_IDX(ATD_TYPE_IDX(tmp_idx));
         length.fld	= TYP_FLD(ATD_TYPE_IDX(tmp_idx));

         if (!size_offset_binary_calc(&length, &result, Mult_Opr, &result)) {
            AT_DCL_ERR(tmp_idx)	= TRUE;
         }

         length.idx	= SB_LEN_IDX(sb_idx);
         length.fld	= SB_LEN_FLD(sb_idx);

         if (!size_offset_binary_calc(&length, &result, Plus_Opr, &result)) {
            AT_DCL_ERR(tmp_idx)	= TRUE;
         }

         if (result.fld == NO_Tbl_Idx) {
            SB_LEN_FLD(sb_idx) = CN_Tbl_Idx;
            SB_LEN_IDX(sb_idx) = ntr_const_tbl(result.type_idx,
                                               FALSE,
                                               result.constant);
         }
         else {
            SB_LEN_IDX(sb_idx) = result.idx;
            SB_LEN_FLD(sb_idx) = result.fld;
         }

         OPND_FLD(opnd2)	= AT_Tbl_Idx;
         OPND_IDX(opnd2)	= tmp_idx;
         OPND_LINE_NUM(opnd2)	= line;
         OPND_COL_NUM(opnd2)	= column;

         gen_whole_substring(&opnd2, 0);

         /* create data init stmt */

         NTR_IR_TBL(init_idx);
         IR_OPR(init_idx)	= Init_Opr;
# if (defined(_TARGET_OS_IRIX) || defined(_TARGET_OS_LINUX) || defined(_TARGET_OS_DARWIN))
         IR_OPR(init_idx)	= Null_Opr;
# endif

         /* must have a type idx */

         IR_TYPE_IDX(init_idx)		= ATD_TYPE_IDX(tmp_idx);
         IR_LINE_NUM(init_idx)		= line;
         IR_COL_NUM(init_idx)		= column;
         IR_LINE_NUM_R(init_idx)	= line;
         IR_COL_NUM_R(init_idx)		= column;
         COPY_OPND(IR_OPND_L(init_idx), opnd2);

         NTR_IR_LIST_TBL(list_idx);
         IR_FLD_R(init_idx)		= IL_Tbl_Idx;
         IR_IDX_R(init_idx)		= list_idx;
         IR_LIST_CNT_R(init_idx)	= 3;

         COPY_OPND(IL_OPND(list_idx), opnd);

         NTR_IR_LIST_TBL(IL_NEXT_LIST_IDX(list_idx));
         IL_PREV_LIST_IDX(IL_NEXT_LIST_IDX(list_idx)) = list_idx;
         list_idx = IL_NEXT_LIST_IDX(list_idx);

         IL_FLD(list_idx)	= CN_Tbl_Idx;
         IL_IDX(list_idx)	= CN_INTEGER_ONE_IDX;
         IL_LINE_NUM(list_idx)	= line;
         IL_COL_NUM(list_idx)	= column;

         NTR_IR_LIST_TBL(IL_NEXT_LIST_IDX(list_idx));
         IL_PREV_LIST_IDX(IL_NEXT_LIST_IDX(list_idx)) = list_idx;
         list_idx = IL_NEXT_LIST_IDX(list_idx);

         IL_FLD(list_idx)	= CN_Tbl_Idx;
         IL_IDX(list_idx)	= CN_INTEGER_ZERO_IDX;
         IL_LINE_NUM(list_idx)	= line;
         IL_COL_NUM(list_idx)	= column;

         gen_sh(Before, Assignment_Stmt, line, column,
                FALSE, FALSE, TRUE);
         SH_IR_IDX(SH_PREV_IDX(curr_stmt_sh_idx))	= init_idx;
         SH_P2_SKIP_ME(SH_PREV_IDX(curr_stmt_sh_idx))	= TRUE;
      }
   }
   else {
      parse_err_flush(Find_EOS, NULL);
   }

   TRACE (Func_Exit, "parse_id_directive", NULL);

   return;

}  /* parse_id_directive */

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

static void parse_open_mp_directives(void)

{
   int			ir_idx;
   int			list_idx;
   opnd_type		opnd;
   int			sh_idx;
   int			type_idx;


   TRACE (Func_Entry, "parse_open_mp_directives", NULL);

   if (TOKEN_VALUE(token) > Tok_Open_Mp_Dir_Start &&
       TOKEN_VALUE(token) < Tok_Open_Mp_Dir_End &&
       disregard_open_mp[TOKEN_VALUE(token) - Tok_Open_Mp_Dir_Start]) {
      parse_err_flush(Find_EOS, NULL);
      goto EXIT;
   }

   switch (TOKEN_VALUE(token)) {
      case Tok_Open_Mp_Dir_Critical:
         ATP_HAS_TASK_DIRS(SCP_ATTR_IDX(curr_scp_idx)) = TRUE;
         ir_idx = gen_directive_ir(Critical_Open_Mp_Opr);

         if (LA_CH_VALUE == LPAREN) {
            NEXT_LA_CH;

            if (MATCHED_TOKEN_CLASS(Tok_Class_Id)) {

               CLEAR_TBL_NTRY(type_tbl, TYP_WORK_IDX);
               TYP_TYPE(TYP_WORK_IDX)    = Character;
               TYP_LINEAR(TYP_WORK_IDX)  = CHARACTER_DEFAULT_TYPE;
               TYP_DESC(TYP_WORK_IDX)    = Default_Typed;
               TYP_CHAR_CLASS(TYP_WORK_IDX)= Const_Len_Char;
               TYP_FLD(TYP_WORK_IDX)     = CN_Tbl_Idx;
               TYP_IDX(TYP_WORK_IDX) = C_INT_TO_CN(CG_INTEGER_DEFAULT_TYPE,
                                                   TOKEN_LEN(token));
               type_idx                  = ntr_type_tbl();

               IR_LINE_NUM_L(ir_idx) = TOKEN_LINE(token);
               IR_COL_NUM_L(ir_idx) = TOKEN_COLUMN(token);
               IR_FLD_L(ir_idx) = CN_Tbl_Idx;
               IR_IDX_L(ir_idx) = ntr_const_tbl(type_idx, TRUE, NULL);

               strcpy((char *)&CN_CONST(IR_IDX_L(ir_idx)),
                      TOKEN_STR(token));
            }
            else {
               parse_err_flush(Find_EOS, "IDENTIFIER");
            }

            if (LA_CH_VALUE == RPAREN) {
               NEXT_LA_CH;
            }
            else {
               parse_err_flush(Find_EOS, ")");
               goto EXIT;
            }
         }
         else if (LA_CH_VALUE != EOS) {
            parse_err_flush(Find_EOS, "( or EOS");
         }

         if (directive_region_error(Critical_Open_Mp_Dir,
                                    IR_LINE_NUM(ir_idx),
                                    IR_COL_NUM(ir_idx))) {
            break;
         }

         SET_DIRECTIVE_STATE(Open_Mp_Critical_Region);
         PUSH_BLK_STK (Open_Mp_Critical_Blk);
         BLK_IS_PARALLEL_REGION(blk_stk_idx)	= TRUE;
         CURR_BLK_FIRST_SH_IDX     = curr_stmt_sh_idx;
         LINK_TO_PARENT_BLK;
         break;


      case Tok_Open_Mp_Dir_Endcritical:
         ATP_HAS_TASK_DIRS(SCP_ATTR_IDX(curr_scp_idx)) = TRUE;
         ir_idx = gen_directive_ir(Endcritical_Open_Mp_Opr);

         if (LA_CH_VALUE == LPAREN) {
            NEXT_LA_CH;

            if (MATCHED_TOKEN_CLASS(Tok_Class_Id)) {

               CLEAR_TBL_NTRY(type_tbl, TYP_WORK_IDX);
               TYP_TYPE(TYP_WORK_IDX)    = Character;
               TYP_LINEAR(TYP_WORK_IDX)  = CHARACTER_DEFAULT_TYPE;
               TYP_DESC(TYP_WORK_IDX)    = Default_Typed;
               TYP_CHAR_CLASS(TYP_WORK_IDX)= Const_Len_Char;
               TYP_FLD(TYP_WORK_IDX)     = CN_Tbl_Idx;
               TYP_IDX(TYP_WORK_IDX) = C_INT_TO_CN(CG_INTEGER_DEFAULT_TYPE,
                                                   TOKEN_LEN(token));
               type_idx                  = ntr_type_tbl();

               IR_LINE_NUM_L(ir_idx) = TOKEN_LINE(token);
               IR_COL_NUM_L(ir_idx) = TOKEN_COLUMN(token);
               IR_FLD_L(ir_idx) = CN_Tbl_Idx;
               IR_IDX_L(ir_idx) = ntr_const_tbl(type_idx, TRUE, NULL);

               strcpy((char *)&CN_CONST(IR_IDX_L(ir_idx)),
                      TOKEN_STR(token));
            }
            else {
               parse_err_flush(Find_EOS, "IDENTIFIER");
            }

            if (LA_CH_VALUE == RPAREN) {
               NEXT_LA_CH;
            }
            else {
               parse_err_flush(Find_EOS, ")");
               goto EXIT;
            }
         }
         else if (LA_CH_VALUE != EOS) {
            parse_err_flush(Find_EOS, "( or EOS");
         }

         if (directive_region_error(Endcritical_Open_Mp_Dir,
                                    IR_LINE_NUM(ir_idx),
                                    IR_COL_NUM(ir_idx))) {
            break;
         }

         CLEAR_DIRECTIVE_STATE(Open_Mp_Critical_Region);
         SH_STMT_TYPE(curr_stmt_sh_idx) = Open_MP_End_Critical_Stmt;
         stmt_type = Open_MP_End_Critical_Stmt;

         if (CURR_BLK == Open_Mp_Critical_Blk &&
             IR_FLD_L(SH_IR_IDX(CURR_BLK_FIRST_SH_IDX)) == CN_Tbl_Idx) {

            /* must be named */
            if (IR_FLD_L(ir_idx) != CN_Tbl_Idx ||
                strcmp((char *)&CN_CONST(IR_IDX_L(SH_IR_IDX(
                                           CURR_BLK_FIRST_SH_IDX))),
                       (char *)&CN_CONST(IR_IDX_L(ir_idx))) != 0) {

               PRINTMSG(IR_LINE_NUM(ir_idx), 1472, Error, IR_COL_NUM(ir_idx));
            }
         }
         end_open_mp_critical_blk(FALSE);
         break;


      case Tok_Open_Mp_Dir_Do:
         ATP_HAS_TASK_DIRS(SCP_ATTR_IDX(curr_scp_idx)) = TRUE;
         ir_idx = gen_directive_ir(Do_Open_Mp_Opr);

         parse_open_mp_clauses(Do_Omp);

         if (directive_region_error(Do_Open_Mp_Dir,
                                    IR_LINE_NUM(ir_idx),
                                    IR_COL_NUM(ir_idx))) {
            break;
         }

         cdir_switches.do_omp_sh_idx = curr_stmt_sh_idx;

         check_do_open_mp_nesting();

         SET_DIRECTIVE_STATE(Open_Mp_Do_Region);
         /* blk is pushed in p_ctl_flow.c */
         break;

      case Tok_Open_Mp_Dir_Enddo:
         ATP_HAS_TASK_DIRS(SCP_ATTR_IDX(curr_scp_idx)) = TRUE;
         ir_idx = gen_directive_ir(Enddo_Open_Mp_Opr);

         if (LA_CH_VALUE != EOS) {
            if (MATCHED_TOKEN_CLASS(Tok_Class_Open_Mp_Dir_Kwd)) {
               if (TOKEN_VALUE(token) == Tok_Open_Mp_Dir_Nowait) {
                  IR_FLD_L(ir_idx) = CN_Tbl_Idx;
                  IR_IDX_L(ir_idx) = CN_INTEGER_ONE_IDX;
                  IR_LINE_NUM_L(ir_idx) = TOKEN_LINE(token);
                  IR_COL_NUM_L(ir_idx) = TOKEN_COLUMN(token);
               }
               else {
                  parse_err_flush(Find_EOS, "NOWAIT");
               }
            }
         }

         if (SH_IR_IDX(SH_PREV_IDX(curr_stmt_sh_idx)) != NULL_IDX &&
             IR_OPR(SH_IR_IDX(SH_PREV_IDX(curr_stmt_sh_idx))) == 
                                              Enddo_Open_Mp_Opr &&
             SH_COMPILER_GEN(SH_PREV_IDX(curr_stmt_sh_idx))) {

            sh_idx = SH_PREV_IDX(curr_stmt_sh_idx);
            COPY_OPND(IR_OPND_R(ir_idx), 
                      IR_OPND_R(SH_IR_IDX(sh_idx)));

            /* remove the CG end do */

            SH_PREV_IDX(SH_NEXT_IDX(sh_idx)) = SH_PREV_IDX(sh_idx);
            SH_NEXT_IDX(SH_PREV_IDX(sh_idx)) = SH_NEXT_IDX(sh_idx);

            FREE_IR_NODE(SH_IR_IDX(sh_idx));
            FREE_SH_NODE(sh_idx);
            break;
         }

         if (directive_region_error(Enddo_Open_Mp_Dir,
                                    IR_LINE_NUM(ir_idx),
                                    IR_COL_NUM(ir_idx))) {
            break;
         }

         CLEAR_DIRECTIVE_STATE(Open_Mp_Do_Region);
         SH_STMT_TYPE(curr_stmt_sh_idx) = Open_MP_End_Do_Stmt;
         stmt_type = Open_MP_End_Do_Stmt;
         end_open_mp_do_blk(FALSE);
         break;

      case Tok_Open_Mp_Dir_Endparallel:
         ATP_HAS_TASK_DIRS(SCP_ATTR_IDX(curr_scp_idx)) = TRUE;
         ir_idx = gen_directive_ir(Endparallel_Open_Mp_Opr);

         if (directive_region_error(Endparallel_Open_Mp_Dir,
                                    IR_LINE_NUM(ir_idx),
                                    IR_COL_NUM(ir_idx))) {
            break;
         }

         CLEAR_DIRECTIVE_STATE(Open_Mp_Parallel_Region);
         SH_STMT_TYPE(curr_stmt_sh_idx) = Open_MP_End_Parallel_Stmt;
         stmt_type = Open_MP_End_Parallel_Stmt;
         end_open_mp_parallel_blk(FALSE);
         break;

      case Tok_Open_Mp_Dir_Endparalleldo:
         ATP_HAS_TASK_DIRS(SCP_ATTR_IDX(curr_scp_idx)) = TRUE;
         ir_idx = gen_directive_ir(Endparalleldo_Open_Mp_Opr);

         if (SH_IR_IDX(SH_PREV_IDX(curr_stmt_sh_idx)) != NULL_IDX &&
             IR_OPR(SH_IR_IDX(SH_PREV_IDX(curr_stmt_sh_idx))) ==
                                              Endparalleldo_Open_Mp_Opr &&
             SH_COMPILER_GEN(SH_PREV_IDX(curr_stmt_sh_idx))) {

            /* remove the CG end do */

            sh_idx = SH_PREV_IDX(curr_stmt_sh_idx);
            COPY_OPND(IR_OPND_R(ir_idx), 
                      IR_OPND_R(SH_IR_IDX(sh_idx)));

            SH_PREV_IDX(SH_NEXT_IDX(sh_idx)) = SH_PREV_IDX(sh_idx);
            SH_NEXT_IDX(SH_PREV_IDX(sh_idx)) = SH_NEXT_IDX(sh_idx);

            FREE_IR_NODE(SH_IR_IDX(sh_idx));
            FREE_SH_NODE(sh_idx);
            break;
         }

         if (directive_region_error(Endparalleldo_Open_Mp_Dir,
                                    IR_LINE_NUM(ir_idx),
                                    IR_COL_NUM(ir_idx))) {
            break;
         }

         CLEAR_DIRECTIVE_STATE(Open_Mp_Parallel_Do_Region);
         SH_STMT_TYPE(curr_stmt_sh_idx) = Open_MP_End_Parallel_Do_Stmt;
         stmt_type = Open_MP_End_Parallel_Do_Stmt;
         end_open_mp_parallel_do_blk(FALSE);
         break;

      case Tok_Open_Mp_Dir_Endparallelsections:
         ATP_HAS_TASK_DIRS(SCP_ATTR_IDX(curr_scp_idx)) = TRUE;
         ir_idx = gen_directive_ir(Endparallelsections_Open_Mp_Opr);

         if (directive_region_error(Endparallelsections_Open_Mp_Dir,
                                    IR_LINE_NUM(ir_idx),
                                    IR_COL_NUM(ir_idx))) {
            break;
         }

         if (CURR_BLK == Open_Mp_Section_Blk) {
            end_open_mp_section_blk(FALSE);
            CLEAR_DIRECTIVE_STATE(Open_Mp_Section_Region);
         }

         CLEAR_DIRECTIVE_STATE(Open_Mp_Parallel_Sections_Region);
         SH_STMT_TYPE(curr_stmt_sh_idx) = Open_MP_End_Parallel_Sections_Stmt;
         stmt_type = Open_MP_End_Parallel_Sections_Stmt;
         end_open_mp_parallel_sections_blk(FALSE);
         break;

      case Tok_Open_Mp_Dir_Endmaster:
         ATP_HAS_TASK_DIRS(SCP_ATTR_IDX(curr_scp_idx)) = TRUE;
         ir_idx = gen_directive_ir(Endmaster_Open_Mp_Opr);

         if (directive_region_error(Endmaster_Open_Mp_Dir,
                                    IR_LINE_NUM(ir_idx),
                                    IR_COL_NUM(ir_idx))) {
            break;
         }

         CLEAR_DIRECTIVE_STATE(Open_Mp_Master_Region);
         SH_STMT_TYPE(curr_stmt_sh_idx) = Open_MP_End_Master_Stmt;
         stmt_type = Open_MP_End_Master_Stmt;
         end_open_mp_master_blk(FALSE);
         break;

      case Tok_Open_Mp_Dir_Endordered:
         ATP_HAS_TASK_DIRS(SCP_ATTR_IDX(curr_scp_idx)) = TRUE;
         ir_idx = gen_directive_ir(Endordered_Open_Mp_Opr);

         if (directive_region_error(Endordered_Open_Mp_Dir,
                                    IR_LINE_NUM(ir_idx),
                                    IR_COL_NUM(ir_idx))) {
            break;
         }

         CLEAR_DIRECTIVE_STATE(Open_Mp_Ordered_Region);
         SH_STMT_TYPE(curr_stmt_sh_idx) = Open_MP_End_Ordered_Stmt;
         stmt_type = Open_MP_End_Ordered_Stmt;
         end_open_mp_ordered_blk(FALSE);
         break;

      case Tok_Open_Mp_Dir_Endsections:
         ATP_HAS_TASK_DIRS(SCP_ATTR_IDX(curr_scp_idx)) = TRUE;
         ir_idx = gen_directive_ir(Endsections_Open_Mp_Opr);

         if (LA_CH_VALUE != EOS) {
            if (MATCHED_TOKEN_CLASS(Tok_Class_Open_Mp_Dir_Kwd)) {
               if (TOKEN_VALUE(token) == Tok_Open_Mp_Dir_Nowait) {
                  IR_FLD_L(ir_idx) = CN_Tbl_Idx;
                  IR_IDX_L(ir_idx) = CN_INTEGER_ONE_IDX;
                  IR_LINE_NUM_L(ir_idx) = TOKEN_LINE(token);
                  IR_COL_NUM_L(ir_idx) = TOKEN_COLUMN(token);
               }
               else {
                  parse_err_flush(Find_EOS, "NOWAIT");
               }
            }
         }

         if (directive_region_error(Endsections_Open_Mp_Dir,
                                    IR_LINE_NUM(ir_idx),
                                    IR_COL_NUM(ir_idx))) {
            break;
         }

         if (CURR_BLK == Open_Mp_Section_Blk) {
            end_open_mp_section_blk(FALSE);
            CLEAR_DIRECTIVE_STATE(Open_Mp_Section_Region);
         }

         CLEAR_DIRECTIVE_STATE(Open_Mp_Sections_Region);
         SH_STMT_TYPE(curr_stmt_sh_idx) = Open_MP_End_Sections_Stmt;
         stmt_type = Open_MP_End_Sections_Stmt;
         end_open_mp_sections_blk(FALSE);
         break;

      case Tok_Open_Mp_Dir_Endsingle:
         ATP_HAS_TASK_DIRS(SCP_ATTR_IDX(curr_scp_idx)) = TRUE;
         ir_idx = gen_directive_ir(Endsingle_Open_Mp_Opr);

	/* the following are added by jhs, 02/7/21 */
	  NTR_IR_LIST_TBL(list_idx);
	  IR_FLD_L(ir_idx) = IL_Tbl_Idx;
	  IR_IDX_L(ir_idx) = list_idx;
	  IR_LIST_CNT_L(ir_idx) = 2;

	  NTR_IR_LIST_TBL(IL_NEXT_LIST_IDX(list_idx));
	  IL_PREV_LIST_IDX(IL_NEXT_LIST_IDX(list_idx)) = list_idx;


	   while (LA_CH_VALUE != EOS) {

	      if (MATCHED_TOKEN_CLASS(Tok_Class_Open_Mp_Dir_Kwd)) {

	         switch (TOKEN_VALUE(token)) {
	            case Tok_Open_Mp_Dir_Nowait:
	               /* only one NOWAIT clause allowed */
	               if (IL_IDX(list_idx) != NULL_IDX) {
	                  PRINTMSG(TOKEN_LINE(token), 1360, Error, TOKEN_COLUMN(token),
	                           "NOWAIT", "END SINGLE");
	                  parse_err_flush(Find_EOS, NULL);
	                  goto EXIT;
	               }
	               /* COPYPRIVATE clause can't on the same END SINGLE directive*/
	               if (IL_IDX(IL_NEXT_LIST_IDX(list_idx)) != NULL_IDX) {
	                  PRINTMSG(TOKEN_LINE(token), 1360, Error, TOKEN_COLUMN(token),
	                           "COPYPRIVATE", "END SINGLE");
	                  parse_err_flush(Find_EOS, NULL);
	                  goto EXIT;
	               }

	            	 IL_FLD(list_idx) = CN_Tbl_Idx;
	               IL_IDX(list_idx) = CN_INTEGER_ONE_IDX;
	               IL_LINE_NUM(list_idx) = TOKEN_LINE(token);
	               IL_COL_NUM(list_idx) = TOKEN_COLUMN(token);
	         	 break;

	           case Tok_Open_Mp_Dir_Copyprivate:
	           	 /* NOWAIT clause can't on the same END SINGLE directive*/
	               if (IL_IDX(list_idx) != NULL_IDX) {
	                  PRINTMSG(TOKEN_LINE(token), 1360, Error, TOKEN_COLUMN(token),
	                           "NOWAIT", "END SINGLE");
	                  parse_err_flush(Find_EOS, NULL);
	                  goto EXIT;
	               }

	               if (LA_CH_VALUE == LPAREN) {
	                  NEXT_LA_CH;
	                  parse_var_common_list(&opnd, FALSE);

	                  if (IL_IDX(IL_NEXT_LIST_IDX(list_idx)) == NULL_IDX) {
	                     COPY_OPND(IL_OPND(IL_NEXT_LIST_IDX(list_idx)), opnd);
	                  }
	                  else {
	                     /* find the end of list */
	                     int copyprivate_list_idx;

	                     copyprivate_list_idx = IL_IDX(IL_NEXT_LIST_IDX(list_idx));
	                     while (IL_NEXT_LIST_IDX(copyprivate_list_idx)) {
	                        copyprivate_list_idx = IL_NEXT_LIST_IDX(copyprivate_list_idx);
	                     }

	                     /* append the new list */
	                     IL_NEXT_LIST_IDX(copyprivate_list_idx) = OPND_IDX(opnd);
	                     IL_PREV_LIST_IDX(OPND_IDX(opnd)) = copyprivate_list_idx;
	                     IL_LIST_CNT(IL_NEXT_LIST_IDX(list_idx)) += OPND_LIST_CNT(opnd);
	                  }

	                  if (LA_CH_VALUE == RPAREN) {
	                     NEXT_LA_CH;
	                  }
	                  else {
	                     parse_err_flush(Find_EOS, ")");
	                     goto EXIT;
	                  }
	               }
	               else {
	                  parse_err_flush(Find_EOS, "(");
	                  goto EXIT;
	               }

	         	 break;

	           default:
	               PRINTMSG(TOKEN_LINE(token), 1517, Error, TOKEN_COLUMN(token),
	                        "OpenMP");
	               parse_err_flush(Find_EOS, NULL);
	               break;
	          }
	      }
	      else {
	         parse_err_flush(Find_EOS, "END SINGLE clause");
	      }

	      if (LA_CH_VALUE == COMMA) {
	         NEXT_LA_CH;
	      }
	   }
         /* the above are added by jhs, 02/7/21 */
         if (directive_region_error(Endsingle_Open_Mp_Dir,
                                    IR_LINE_NUM(ir_idx),
                                    IR_COL_NUM(ir_idx))) {
            break;
         }

         CLEAR_DIRECTIVE_STATE(Open_Mp_Single_Region);
         SH_STMT_TYPE(curr_stmt_sh_idx) = Open_MP_End_Single_Stmt;
         stmt_type = Open_MP_End_Single_Stmt;
         end_open_mp_single_blk(FALSE);
         break;

      case Tok_Open_Mp_Dir_Master:
         ATP_HAS_TASK_DIRS(SCP_ATTR_IDX(curr_scp_idx)) = TRUE;
         ir_idx = gen_directive_ir(Master_Open_Mp_Opr);

         if (directive_region_error(Master_Open_Mp_Dir,
                                    IR_LINE_NUM(ir_idx),
                                    IR_COL_NUM(ir_idx))) {
            break;
         }

         SET_DIRECTIVE_STATE(Open_Mp_Master_Region);
         PUSH_BLK_STK (Open_Mp_Master_Blk);
         BLK_IS_PARALLEL_REGION(blk_stk_idx)	= TRUE;
         CURR_BLK_FIRST_SH_IDX     = curr_stmt_sh_idx;
         LINK_TO_PARENT_BLK;
         break;

      case Tok_Open_Mp_Dir_Ordered:
         ATP_HAS_TASK_DIRS(SCP_ATTR_IDX(curr_scp_idx)) = TRUE;
         ir_idx = gen_directive_ir(Ordered_Open_Mp_Opr);

         if (directive_region_error(Ordered_Open_Mp_Dir,
                                    IR_LINE_NUM(ir_idx),
                                    IR_COL_NUM(ir_idx))) {
            break;
         }

         check_ordered_open_mp_nesting();

         SET_DIRECTIVE_STATE(Open_Mp_Ordered_Region);
         PUSH_BLK_STK (Open_Mp_Ordered_Blk);
         BLK_IS_PARALLEL_REGION(blk_stk_idx)	= TRUE;
         CURR_BLK_FIRST_SH_IDX     = curr_stmt_sh_idx;
         LINK_TO_PARENT_BLK;
         break;

      case Tok_Open_Mp_Dir_Parallel:
         ATP_HAS_TASK_DIRS(SCP_ATTR_IDX(curr_scp_idx)) = TRUE;
         ir_idx = gen_directive_ir(Parallel_Open_Mp_Opr);

         parse_open_mp_clauses(Parallel_Omp);

         if (directive_region_error(Parallel_Open_Mp_Dir,
                                    IR_LINE_NUM(ir_idx),
                                    IR_COL_NUM(ir_idx))) {
            break;
         }

         SET_DIRECTIVE_STATE(Open_Mp_Parallel_Region);
         PUSH_BLK_STK (Open_Mp_Parallel_Blk);
         BLK_IS_PARALLEL_REGION(blk_stk_idx) = TRUE;
         CURR_BLK_FIRST_SH_IDX     = curr_stmt_sh_idx;
         LINK_TO_PARENT_BLK;
         break;

      case Tok_Open_Mp_Dir_Paralleldo:
         ATP_HAS_TASK_DIRS(SCP_ATTR_IDX(curr_scp_idx)) = TRUE;
         ir_idx = gen_directive_ir(Paralleldo_Open_Mp_Opr);

         parse_open_mp_clauses(Parallel_Do_Omp);

         if (directive_region_error(Paralleldo_Open_Mp_Dir,
                                    IR_LINE_NUM(ir_idx),
                                    IR_COL_NUM(ir_idx))) {
            break;
         }

         cdir_switches.paralleldo_omp_sh_idx = curr_stmt_sh_idx;

         SET_DIRECTIVE_STATE(Open_Mp_Parallel_Do_Region);
         /* blk is pushed in p_ctl_flow.c */
         break;

      case Tok_Open_Mp_Dir_Parallelsections:
         ATP_HAS_TASK_DIRS(SCP_ATTR_IDX(curr_scp_idx)) = TRUE;
         ir_idx = gen_directive_ir(Parallelsections_Open_Mp_Opr);

         parse_open_mp_clauses(Parallel_Sections_Omp);

         if (directive_region_error(Parallelsections_Open_Mp_Dir,
                                    IR_LINE_NUM(ir_idx),
                                    IR_COL_NUM(ir_idx))) {
            break;
         }

         SET_DIRECTIVE_STATE(Open_Mp_Parallel_Sections_Region);
         PUSH_BLK_STK (Open_Mp_Parallel_Sections_Blk);
         BLK_IS_PARALLEL_REGION(blk_stk_idx)	= TRUE;
         CURR_BLK_FIRST_SH_IDX     = curr_stmt_sh_idx;
         LINK_TO_PARENT_BLK;

         /* push on a Section block */
         SET_DIRECTIVE_STATE(Open_Mp_Section_Region);
         PUSH_BLK_STK (Open_Mp_Section_Blk);
         BLK_IS_PARALLEL_REGION(blk_stk_idx)	= TRUE;
         CURR_BLK_FIRST_SH_IDX     = curr_stmt_sh_idx;
         LINK_TO_PARENT_BLK;
         break;


      case Tok_Open_Mp_Dir_Section:
         ATP_HAS_TASK_DIRS(SCP_ATTR_IDX(curr_scp_idx)) = TRUE;
         ir_idx = gen_directive_ir(Section_Open_Mp_Opr);

         if (directive_region_error(Section_Open_Mp_Dir,
                                    IR_LINE_NUM(ir_idx),
                                    IR_COL_NUM(ir_idx))) {
            break;
         }

         if (! check_section_open_mp_context()) {
            break;
         }

         if (CURR_BLK == Open_Mp_Section_Blk) {
            end_open_mp_section_blk(FALSE);
            CLEAR_DIRECTIVE_STATE(Open_Mp_Section_Region);
         }

         SET_DIRECTIVE_STATE(Open_Mp_Section_Region);
         PUSH_BLK_STK (Open_Mp_Section_Blk);
         BLK_IS_PARALLEL_REGION(blk_stk_idx)	= TRUE;
         CURR_BLK_FIRST_SH_IDX     = curr_stmt_sh_idx;
         LINK_TO_PARENT_BLK;
         break;

      case Tok_Open_Mp_Dir_Sections:
         ATP_HAS_TASK_DIRS(SCP_ATTR_IDX(curr_scp_idx)) = TRUE;
         ir_idx = gen_directive_ir(Sections_Open_Mp_Opr);

         parse_open_mp_clauses(Sections_Omp);

         if (directive_region_error(Sections_Open_Mp_Dir,
                                    IR_LINE_NUM(ir_idx),
                                    IR_COL_NUM(ir_idx))) {
            break;
         }

         SET_DIRECTIVE_STATE(Open_Mp_Sections_Region);

         check_do_open_mp_nesting();

         PUSH_BLK_STK (Open_Mp_Sections_Blk);
         BLK_IS_PARALLEL_REGION(blk_stk_idx)	= TRUE;
         CURR_BLK_FIRST_SH_IDX     = curr_stmt_sh_idx;
         LINK_TO_PARENT_BLK;

         /* push on a Section block */
         SET_DIRECTIVE_STATE(Open_Mp_Section_Region);
         PUSH_BLK_STK (Open_Mp_Section_Blk);
         BLK_IS_PARALLEL_REGION(blk_stk_idx)	= TRUE;
         CURR_BLK_FIRST_SH_IDX     = curr_stmt_sh_idx;
         LINK_TO_PARENT_BLK;
         break;

      case Tok_Open_Mp_Dir_Single:
         ATP_HAS_TASK_DIRS(SCP_ATTR_IDX(curr_scp_idx)) = TRUE;
         ir_idx = gen_directive_ir(Single_Open_Mp_Opr);

         parse_open_mp_clauses(Single_Omp);

         if (directive_region_error(Single_Open_Mp_Dir,
                                    IR_LINE_NUM(ir_idx),
                                    IR_COL_NUM(ir_idx))) {
            break;
         }

         SET_DIRECTIVE_STATE(Open_Mp_Single_Region);

         check_do_open_mp_nesting();

         PUSH_BLK_STK (Open_Mp_Single_Blk);
         BLK_IS_PARALLEL_REGION(blk_stk_idx)	= TRUE;
         CURR_BLK_FIRST_SH_IDX     = curr_stmt_sh_idx;
         LINK_TO_PARENT_BLK;
         break;
	 
      case Tok_Open_Mp_Dir_Workshare:
	 ATP_HAS_TASK_DIRS(SCP_ATTR_IDX(curr_scp_idx)) = TRUE;
	 ir_idx = gen_directive_ir(Workshare_Open_Mp_Opr);

	 parse_open_mp_clauses(Workshare_Omp);

	 if (directive_region_error(Workshare_Open_Mp_Dir,
				    IR_LINE_NUM(ir_idx),
				    IR_COL_NUM(ir_idx))) {
	    break;
	 }

	 SET_DIRECTIVE_STATE(Open_Mp_Workshare_Region);

	 check_do_open_mp_nesting();

	 PUSH_BLK_STK(Open_Mp_Workshare_Blk);
	 BLK_IS_PARALLEL_REGION(blk_stk_idx) = TRUE;
	 CURR_BLK_FIRST_SH_IDX = curr_stmt_sh_idx;
	 LINK_TO_PARENT_BLK;
	 break;

      case Tok_Open_Mp_Dir_Parallelworkshare:
	 ATP_HAS_TASK_DIRS(SCP_ATTR_IDX(curr_scp_idx)) = TRUE;
	 ir_idx = gen_directive_ir(Parallelworkshare_Open_Mp_Opr);

	 parse_open_mp_clauses(Parallel_Workshare_Omp);

	 if (directive_region_error(Parallelworkshare_Open_Mp_Dir,
				   IR_LINE_NUM(ir_idx),
				   IR_COL_NUM(ir_idx))){
	    break;
	 }

	 SET_DIRECTIVE_STATE(Open_Mp_Parallel_Workshare_Region);
	 PUSH_BLK_STK(Open_Mp_Parallel_Workshare_Blk);
	 BLK_IS_PARALLEL_REGION(blk_stk_idx) = TRUE;
	 CURR_BLK_FIRST_SH_IDX = curr_stmt_sh_idx;
	 LINK_TO_PARENT_BLK;
	 break;

      case Tok_Open_Mp_Dir_Endworkshare:
	 ATP_HAS_TASK_DIRS(SCP_ATTR_IDX(curr_scp_idx)) = TRUE;
	 ir_idx = gen_directive_ir(Endworkshare_Open_Mp_Opr);

	 if (LA_CH_VALUE != EOS) {
	    if (MATCHED_TOKEN_CLASS(Tok_Class_Open_Mp_Dir_Kwd)) {
	       if (TOKEN_VALUE(token) == Tok_Open_Mp_Dir_Nowait) {
		  IR_FLD_L(ir_idx) = CN_Tbl_Idx;
		  IR_IDX_L(ir_idx) = CN_INTEGER_ONE_IDX;
		  IR_LINE_NUM_L(ir_idx) = TOKEN_LINE(token);
		  IR_COL_NUM_L(ir_idx) = TOKEN_COLUMN(token);
	       }
	       else{
		  parse_err_flush(Find_EOS, "NOWAIT");
	       }
	    }
	 }

	 if (directive_region_error(Endworkshare_Open_Mp_Dir,
				    IR_LINE_NUM(ir_idx),
				    IR_COL_NUM(ir_idx))) {
	    break;
	 }

	 CLEAR_DIRECTIVE_STATE(Open_Mp_Workshare_Region);
	 SH_STMT_TYPE(curr_stmt_sh_idx) = Open_MP_End_Workshare_Stmt;
	 stmt_type = Open_MP_End_Workshare_Stmt;
	 end_open_mp_workshare_blk(FALSE);
	 break;

      case Tok_Open_Mp_Dir_Endparallelworkshare:
	 ATP_HAS_TASK_DIRS(SCP_ATTR_IDX(curr_scp_idx)) = TRUE;
	 ir_idx = gen_directive_ir(Endparallelworkshare_Open_Mp_Opr);

	 if(directive_region_error(Endparallelworkshare_Open_Mp_Dir,
				   IR_LINE_NUM(ir_idx),
				   IR_COL_NUM(ir_idx))){
	    break;
	 }

	 CLEAR_DIRECTIVE_STATE(Open_Mp_Parallel_Workshare_Region);
	 SH_STMT_TYPE(curr_stmt_sh_idx) = Open_MP_End_Parallel_Workshare_Stmt;
	 stmt_type = Open_MP_End_Parallel_Workshare_Stmt;
	 end_open_mp_parallel_workshare_blk(FALSE);
	 break;

      case Tok_Open_Mp_Dir_Atomic:
         ATP_HAS_TASK_DIRS(SCP_ATTR_IDX(curr_scp_idx)) = TRUE;
         ir_idx = gen_directive_ir(Atomic_Open_Mp_Opr);

         if (directive_region_error(Atomic_Open_Mp_Dir,
                                    IR_LINE_NUM(ir_idx),
                                    IR_COL_NUM(ir_idx))) {
            break;
         }

         break;

      case Tok_Open_Mp_Dir_Barrier:
         ATP_HAS_TASK_DIRS(SCP_ATTR_IDX(curr_scp_idx)) = TRUE;
         ir_idx = gen_directive_ir(Barrier_Open_Mp_Opr);

         if (directive_region_error(Barrier_Open_Mp_Dir,
                                    IR_LINE_NUM(ir_idx),
                                    IR_COL_NUM(ir_idx))) {
            break;
         }

         break;

      case Tok_Open_Mp_Dir_Flush:
         ATP_HAS_TASK_DIRS(SCP_ATTR_IDX(curr_scp_idx)) = TRUE;
         ir_idx = gen_directive_ir(Flush_Open_Mp_Opr);

         if (LA_CH_VALUE == LPAREN) {
            NEXT_LA_CH;

#ifdef KEY /* Bug 6075 */
	    parse_var_common_list(&opnd, FALSE);
#else /* KEY Bug 6075 */
            parse_var_name_list(&opnd);
#endif /* KEY Bug 6075 */
            COPY_OPND(IR_OPND_L(ir_idx), opnd);

            if (LA_CH_VALUE == RPAREN) {
               NEXT_LA_CH;
            }
            else {
               parse_err_flush(Find_EOS, ")");
               goto EXIT;
            }
         }

         if (directive_region_error(Flush_Open_Mp_Dir,
                                    IR_LINE_NUM(ir_idx),
                                    IR_COL_NUM(ir_idx))) {
            break;
         }

         break;

      case Tok_Open_Mp_Dir_Threadprivate:

         ATP_HAS_TASK_DIRS(SCP_ATTR_IDX(curr_scp_idx)) = TRUE;

         if (curr_stmt_category < Dir_Integer_Stmt_Cat) {
            PRINTMSG(TOKEN_LINE(token), 795, Warning,
                     TOKEN_COLUMN(token), "THREADPRIVATE");
            parse_err_flush(Find_EOS, NULL);
            break;
         }

         if (curr_stmt_category >= Executable_Stmt_Cat) {
            PRINTMSG(TOKEN_LINE(token), 531, Error,
                     TOKEN_COLUMN(token), 
                     "THREADPRIVATE");
            parse_err_flush(Find_EOS, NULL);
            break;
         }

         parse_slash_common_dirs();
         break;

      case Tok_Open_Mp_Dir_Distribute:
         if (! omp_extension_prefix(TOKEN_LINE(token))) {
            /* token not on !$sgi line */
            PRINTMSG(TOKEN_LINE(token), 1518, Warning,
                     TOKEN_COLUMN(token),
                     "DISTRIBUTE directive", "!$SGI");
         }

         if (dump_flags.dsm) {
            parse_distribution_dir(FALSE);
         }
         else {
            parse_err_flush(Find_EOS, NULL);
         }
         break;

      case Tok_Open_Mp_Dir_Distribute_Reshape:
         if (! omp_extension_prefix(TOKEN_LINE(token))) {
            /* token not on !$sgi line */
            PRINTMSG(TOKEN_LINE(token), 1518, Warning,
                     TOKEN_COLUMN(token),
                     "DISTRIBUTE_RESHAPE directive", "!$SGI");
         }

         if (dump_flags.dsm) {
            parse_distribution_dir(TRUE);
         }
         else {
            parse_err_flush(Find_EOS, NULL);
         }
         break;

      case Tok_Open_Mp_Dir_Dynamic:
         if (! omp_extension_prefix(TOKEN_LINE(token))) {
            /* token not on !$sgi line */
            PRINTMSG(TOKEN_LINE(token), 1518, Warning,
                     TOKEN_COLUMN(token),
                     "DYNAMIC directive", "!$SGI");
         }

         if (dump_flags.dsm) {
            if (parse_var_name_list(&opnd)) {
               ir_idx = gen_directive_ir(Dynamic_Dollar_Opr);
               COPY_OPND(IR_OPND_L(ir_idx), opnd);
            }
         }
         else {
            parse_err_flush(Find_EOS, NULL);
         }
         break;

      case Tok_Open_Mp_Dir_Page_Place:
         if (! omp_extension_prefix(TOKEN_LINE(token))) {
            /* token not on !$sgi line */
            PRINTMSG(TOKEN_LINE(token), 1518, Warning,
                     TOKEN_COLUMN(token),
                     "PAGE_PLACE directive", "!$SGI");
         }

         if (dump_flags.dsm) {
            ir_idx = gen_directive_ir(Page_Place_Dollar_Opr);

            NTR_IR_LIST_TBL(list_idx);
            IR_FLD_L(ir_idx) = IL_Tbl_Idx;
            IR_IDX_L(ir_idx) = list_idx;
            IR_LIST_CNT_L(ir_idx) = 3;

            NTR_IR_LIST_TBL(IL_NEXT_LIST_IDX(list_idx));
            IL_PREV_LIST_IDX(IL_NEXT_LIST_IDX(list_idx)) = list_idx;
            list_idx = IL_NEXT_LIST_IDX(list_idx);

            NTR_IR_LIST_TBL(IL_NEXT_LIST_IDX(list_idx));
            IL_PREV_LIST_IDX(IL_NEXT_LIST_IDX(list_idx)) = list_idx;

            list_idx = IR_IDX_L(ir_idx);

            if (LA_CH_VALUE == LPAREN) {
               NEXT_LA_CH;

               if (MATCHED_TOKEN_CLASS(Tok_Class_Id)) {
                  parse_deref(&opnd, NULL_IDX);
                  COPY_OPND(IL_OPND(list_idx), opnd);
               }
               else {
                  parse_err_flush(Find_EOS, "IDENTIFIER");
                  goto EXIT;
               }

               if (LA_CH_VALUE == COMMA) {
                  NEXT_LA_CH;
               }
               else {
                  parse_err_flush(Find_EOS, ",");
                  goto EXIT;
               }
   
               list_idx = IL_NEXT_LIST_IDX(list_idx);
   
               parse_expr(&opnd);

               COPY_OPND(IL_OPND(list_idx), opnd);

               if (LA_CH_VALUE == COMMA) {
                  NEXT_LA_CH;
               }
               else {
                  parse_err_flush(Find_EOS, ",");
                  goto EXIT;
               }

               list_idx = IL_NEXT_LIST_IDX(list_idx);

               parse_expr(&opnd);

               COPY_OPND(IL_OPND(list_idx), opnd);

               if (LA_CH_VALUE == RPAREN) {
                  NEXT_LA_CH;
               }
               else {
                  parse_err_flush(Find_EOS, ")");
               }
            }
            else {
               parse_err_flush(Find_EOS, "(");
            }
         }
         else {
            parse_err_flush(Find_EOS, NULL);
         }
         break;

      case Tok_Open_Mp_Dir_Redistribute:
         if (! omp_extension_prefix(TOKEN_LINE(token))) {
            /* token not on !$sgi line */
            PRINTMSG(TOKEN_LINE(token), 1518, Warning,
                     TOKEN_COLUMN(token),
                     "REDISTRIBUTE directive", "!$SGI");
         }

         if (dump_flags.dsm) {
            parse_redistribute_dir();
         }
         else {
            parse_err_flush(Find_EOS, NULL);
         }
         break;


      default:
         PRINTMSG(TOKEN_LINE(token), 790, Warning, TOKEN_COLUMN(token));
         parse_err_flush(Find_EOS, NULL);
         break;
   }

   if (LA_CH_VALUE != EOS) {
      parse_err_flush(Find_EOS, EOS_STR);
   }

EXIT:

   NEXT_LA_CH;

   TRACE (Func_Exit, "parse_open_mp_directives", NULL);

   return;

}  /* parse_open_mp_directives */

/******************************************************************************\
|*                                                                            *|
|* Description:                                                               *|
|*      This routine parses the open mp directive.                            *|
|*      The ir it produces looks like ..                                      *|
|*                                                                            *|
|*                        (open mp directive operator)                        *|
|*                       /                                                    *|
|*                      |- IF condition                                       *|
|*                      |- NUM_THREADS expr                          *|
|*                      |- PRIVATE var list                                   *|
|*                      |- SHARED var list                                    *|
|*                      |- FIRSTPRIVATE var list                              *|
|*                      |- DEFAULT scope value (CN_Tbl_Idx)                   *|
|*                      |- COPYIN var list                                    *|
|*                      |- REDUCTION opr | intrinsic list                     *|
|*                      |- REDUCTION var list list                            *|
|*                      |- LASTPRIVATE var list                               *|
|*                      |- ORDERED constant (ORDERED == 1, else NO_Tbl_Idx)   *|
|*                      |- SCHEDULE type (CN_Tbl_Idx)                         *|
|*                      |- SCHEDULE chunk (CN_Tbl_Idx)                        *|
|*                      |- AFFINITY index_var list                            *|
|*                      |- IS THREAD constant (THREAD == 1, DATA == 0)        *|
|*                      |- THREAD/DATA list                                   *|
|*                      |- ONTO list                                          *|
|*                      |- NEST list                                          *|
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

static void parse_open_mp_clauses(open_mp_directive_type directive)

{
   int          i;
   int          ir_idx;
   int          list_array[OPEN_MP_LIST_CNT];
   int          list_idx;
   opnd_type    opnd;
   int		opr_ir_idx;
   long		the_constant;

# if (defined(_TARGET_OS_IRIX) || defined(_TARGET_OS_LINUX) || defined(_TARGET_OS_DARWIN))
   int          column;
   int          line;
   int          list2_idx;
   boolean      seen_nest = FALSE;
# endif


   TRACE (Func_Entry, "parse_open_mp_clauses", NULL);

   ir_idx = SH_IR_IDX(curr_stmt_sh_idx);

   for (i = 0; i < OPEN_MP_LIST_CNT; i++) {
      NTR_IR_LIST_TBL(list_array[i]);
      if (i >= 1) {
         IL_NEXT_LIST_IDX(list_array[i - 1]) = list_array[i];
         IL_PREV_LIST_IDX(list_array[i]) = list_array[i - 1];
      }
   }

   IR_FLD_L(ir_idx) = IL_Tbl_Idx;
   IR_IDX_L(ir_idx) = list_array[0];
   IR_LIST_CNT_L(ir_idx) = OPEN_MP_LIST_CNT;

   while (LA_CH_VALUE != EOS) {

      if (MATCHED_TOKEN_CLASS(Tok_Class_Open_Mp_Dir_Kwd)) {

         switch (TOKEN_VALUE(token)) {

            case Tok_Open_Mp_Dir_If:

               if (! open_mp_clause_allowed[directive][If_Omp_Clause]) {
                  PRINTMSG(TOKEN_LINE(token), 1370, Error, TOKEN_COLUMN(token),
                           "IF", open_mp_dir_str[directive]);
                  parse_err_flush(Find_EOS, NULL);
                  goto EXIT;
               }

               /* only one IF clause allowed */

               if (IL_IDX(list_array[OPEN_MP_IF_IDX]) != NULL_IDX) {
                  PRINTMSG(TOKEN_LINE(token), 1360, Error, TOKEN_COLUMN(token),
                           "IF", open_mp_dir_str[directive]);
                  parse_err_flush(Find_EOS, NULL);
                  goto EXIT;
               }

               if (LA_CH_VALUE == LPAREN) {
                  NEXT_LA_CH;
                  parse_expr(&opnd);

                  COPY_OPND(IL_OPND(list_array[OPEN_MP_IF_IDX]), opnd);

                  if (LA_CH_VALUE == RPAREN) {
                     NEXT_LA_CH;
                  }
                  else {
                     parse_err_flush(Find_EOS, ")");
                     goto EXIT;
                  }
               }
               else {
                  parse_err_flush(Find_EOS, "(");
                  goto EXIT;
               }
               break;
               
            case Tok_Open_Mp_Dir_Num_Threads: /* by jhs, 02/7/20 */
               if (! open_mp_clause_allowed[directive][Num_Threads_Omp_Clause]) {
                  PRINTMSG(TOKEN_LINE(token), 1370, Error, TOKEN_COLUMN(token),
                           "NUM_THREADS", open_mp_dir_str[directive]);
                  parse_err_flush(Find_EOS, NULL);
                  goto EXIT;
               }

               /* only one NUM_THREADS clause allowed */

               if (IL_IDX(list_array[OPEN_MP_NUM_THREADS]) != NULL_IDX) {
                  PRINTMSG(TOKEN_LINE(token), 1360, Error, TOKEN_COLUMN(token),
                           "NUM_THREADS", open_mp_dir_str[directive]);
                  parse_err_flush(Find_EOS, NULL);
                  goto EXIT;
               }

               if (LA_CH_VALUE == LPAREN) {
                  NEXT_LA_CH;
                  parse_expr(&opnd);

                  COPY_OPND(IL_OPND(list_array[OPEN_MP_NUM_THREADS]), opnd);

                  if (LA_CH_VALUE == RPAREN) {
                     NEXT_LA_CH;
                  }
                  else {
                     parse_err_flush(Find_EOS, ")");
                     goto EXIT;
                  }
               }
               else {
                  parse_err_flush(Find_EOS, "(");
                  goto EXIT;
               }
            	 break;

            case Tok_Open_Mp_Dir_Private:

               if (! open_mp_clause_allowed[directive][Private_Omp_Clause]) {
                  PRINTMSG(TOKEN_LINE(token), 1370, Error, TOKEN_COLUMN(token),
                           "PRIVATE", open_mp_dir_str[directive]);
                  parse_err_flush(Find_EOS, NULL);
                  goto EXIT;
               }

               if (LA_CH_VALUE == LPAREN) {
                  NEXT_LA_CH;
                  parse_var_common_list(&opnd, FALSE);

                  if (IL_IDX(list_array[OPEN_MP_PRIVATE_IDX]) == NULL_IDX) {
                     COPY_OPND(IL_OPND(list_array[OPEN_MP_PRIVATE_IDX]), opnd);
                  }
                  else {
                     /* find the end of list */

                     list_idx = IL_IDX(list_array[OPEN_MP_PRIVATE_IDX]);
                     while (IL_NEXT_LIST_IDX(list_idx)) {
                        list_idx = IL_NEXT_LIST_IDX(list_idx);
                     }

                     /* append the new list */
                     IL_NEXT_LIST_IDX(list_idx) = OPND_IDX(opnd);
                     IL_PREV_LIST_IDX(OPND_IDX(opnd)) = list_idx;
                     IL_LIST_CNT(list_array[OPEN_MP_PRIVATE_IDX]) +=
                                                         OPND_LIST_CNT(opnd);
                  }

                  if (LA_CH_VALUE == RPAREN) {
                     NEXT_LA_CH;
                  }
                  else {
                     parse_err_flush(Find_EOS, ")");
                     goto EXIT;
                  }
               }
               else {
                  parse_err_flush(Find_EOS, "(");
                  goto EXIT;
               }

               break;

            case Tok_Open_Mp_Dir_Shared:

               if (! open_mp_clause_allowed[directive][Shared_Omp_Clause]) {
                  PRINTMSG(TOKEN_LINE(token), 1370, Error, TOKEN_COLUMN(token),
                           "SHARED", open_mp_dir_str[directive]);
                  parse_err_flush(Find_EOS, NULL);
                  goto EXIT;
               }

               if (LA_CH_VALUE == LPAREN) {
                  NEXT_LA_CH;
#ifdef KEY /* Bug 6075 */
                  parse_var_common_list(&opnd, FALSE);
#else /* KEY Bug 6075 */
                  parse_var_name_list(&opnd);
#endif /* KEY Bug 6075 */

                  if (IL_IDX(list_array[OPEN_MP_SHARED_IDX]) == NULL_IDX) {
                     COPY_OPND(IL_OPND(list_array[OPEN_MP_SHARED_IDX]), opnd);
                  }
                  else {
                     /* find the end of list */

                     list_idx = IL_IDX(list_array[OPEN_MP_SHARED_IDX]);
                     while (IL_NEXT_LIST_IDX(list_idx)) {
                        list_idx = IL_NEXT_LIST_IDX(list_idx);
                     }

                     /* append the new list */
                     IL_NEXT_LIST_IDX(list_idx) = OPND_IDX(opnd);
                     IL_PREV_LIST_IDX(OPND_IDX(opnd)) = list_idx;
                     IL_LIST_CNT(list_array[OPEN_MP_SHARED_IDX]) +=
                                                          OPND_LIST_CNT(opnd);
                  }

                  if (LA_CH_VALUE == RPAREN) {
                     NEXT_LA_CH;
                  }
                  else {
                     parse_err_flush(Find_EOS, ")");
                     goto EXIT;
                  }
               }
               else {
                  parse_err_flush(Find_EOS, "(");
                  goto EXIT;
               }

               break;

            case Tok_Open_Mp_Dir_Firstprivate:

               if (! open_mp_clause_allowed[directive]
                                           [Firstprivate_Omp_Clause]) {
                  PRINTMSG(TOKEN_LINE(token), 1370, Error, TOKEN_COLUMN(token),
                           "FIRSTPRIVATE", open_mp_dir_str[directive]);
                  parse_err_flush(Find_EOS, NULL);
                  goto EXIT;
               }

               if (LA_CH_VALUE == LPAREN) {
                  NEXT_LA_CH;
                  parse_var_common_list(&opnd, FALSE);

                  if (IL_IDX(list_array[OPEN_MP_FIRSTPRIVATE_IDX]) == 
                                                                  NULL_IDX) {

                     COPY_OPND(IL_OPND(list_array[OPEN_MP_FIRSTPRIVATE_IDX]),
                               opnd);
                  }
                  else {
                     /* find the end of list */

                     list_idx = IL_IDX(list_array[OPEN_MP_FIRSTPRIVATE_IDX]);
                     while (IL_NEXT_LIST_IDX(list_idx)) {
                        list_idx = IL_NEXT_LIST_IDX(list_idx);
                     }

                     /* append the new list */
                     IL_NEXT_LIST_IDX(list_idx) = OPND_IDX(opnd);
                     IL_PREV_LIST_IDX(OPND_IDX(opnd)) = list_idx;
                     IL_LIST_CNT(list_array[OPEN_MP_FIRSTPRIVATE_IDX]) +=
                                                       OPND_LIST_CNT(opnd);
                  }

                  if (LA_CH_VALUE == RPAREN) {
                     NEXT_LA_CH;
                  }
                  else {
                     parse_err_flush(Find_EOS, ")");
                     goto EXIT;
                  }
               }
               else {
                  parse_err_flush(Find_EOS, "(");
                  goto EXIT;
               }

               break;

            case Tok_Open_Mp_Dir_Default:

               if (! open_mp_clause_allowed[directive][Default_Omp_Clause]) {
                  PRINTMSG(TOKEN_LINE(token), 1370, Error, TOKEN_COLUMN(token),
                           "DEFAULT", open_mp_dir_str[directive]);
                  parse_err_flush(Find_EOS, NULL);
                  goto EXIT;
               }

               /* only one DEFAULT clause allowed */

               if (IL_IDX(list_array[OPEN_MP_DEFAULT_IDX]) != NULL_IDX) {
                  PRINTMSG(TOKEN_LINE(token), 1360, Error, TOKEN_COLUMN(token),
                           "DEFAULT", open_mp_dir_str[directive]);
                  parse_err_flush(Find_EOS, NULL);
                  goto EXIT;
               }

               if (LA_CH_VALUE == LPAREN) {
                  NEXT_LA_CH;

                  if (MATCHED_TOKEN_CLASS(Tok_Class_Open_Mp_Dir_Kwd)) {

                     switch (TOKEN_VALUE(token)) {
                        case Tok_Open_Mp_Dir_Private:
                           the_constant = OPEN_MP_DEFAULT_PRIVATE;
                           break;

                        case Tok_Open_Mp_Dir_Shared:
                           the_constant = OPEN_MP_DEFAULT_SHARED;
                           break;

                        case Tok_Open_Mp_Dir_None:
                           the_constant = OPEN_MP_DEFAULT_NONE;
                           break;

                        default:
                           parse_err_flush(Find_EOS, 
                                           "PRIVATE, SHARED, or NONE");
                           goto EXIT;
                     }

                     IL_FLD(list_array[OPEN_MP_DEFAULT_IDX]) = CN_Tbl_Idx;
                     IL_IDX(list_array[OPEN_MP_DEFAULT_IDX]) = 
                                           C_INT_TO_CN(CG_INTEGER_DEFAULT_TYPE,
                                                       the_constant);

                     IL_LINE_NUM(list_array[OPEN_MP_DEFAULT_IDX]) = 
                                                         TOKEN_LINE(token);
                     IL_COL_NUM(list_array[OPEN_MP_DEFAULT_IDX]) = 
                                                         TOKEN_COLUMN(token);

                     if (LA_CH_VALUE == RPAREN) {
                        NEXT_LA_CH;
                     }
                     else {
                        parse_err_flush(Find_EOS, ")");
                        goto EXIT;
                     }
                  }
                  else {
                     parse_err_flush(Find_EOS, "PRIVATE, SHARED, or NONE");
                  }
               }
               else {
                  parse_err_flush(Find_EOS, "(");
                  goto EXIT;
               }

               break;

            case Tok_Open_Mp_Dir_Copyin:

               if (! open_mp_clause_allowed[directive][Copyin_Omp_Clause]) {
                  PRINTMSG(TOKEN_LINE(token), 1370, Error, TOKEN_COLUMN(token),
                           "COPYIN", open_mp_dir_str[directive]);
                  parse_err_flush(Find_EOS, NULL);
                  goto EXIT;
               }

               if (LA_CH_VALUE == LPAREN) {
                  NEXT_LA_CH;
                  parse_var_common_list(&opnd, FALSE);

                  if (IL_IDX(list_array[OPEN_MP_COPYIN_IDX]) == NULL_IDX) {
                     COPY_OPND(IL_OPND(list_array[OPEN_MP_COPYIN_IDX]),
                               opnd);
                  }
                  else {
                     /* find the end of list */

                     list_idx = IL_IDX(list_array[OPEN_MP_COPYIN_IDX]);
                     while (IL_NEXT_LIST_IDX(list_idx)) {
                        list_idx = IL_NEXT_LIST_IDX(list_idx);
                     }

                     /* append the new list */
                     IL_NEXT_LIST_IDX(list_idx) = OPND_IDX(opnd);
                     IL_PREV_LIST_IDX(OPND_IDX(opnd)) = list_idx;
                     IL_LIST_CNT(list_array[OPEN_MP_COPYIN_IDX]) +=
                                                       OPND_LIST_CNT(opnd);
                  }

                  if (LA_CH_VALUE == RPAREN) {
                     NEXT_LA_CH;
                  }
                  else {
                     parse_err_flush(Find_EOS, ")");
                     goto EXIT;
                  }
               }
               else {
                  parse_err_flush(Find_EOS, "(");
                  goto EXIT;
               }

               break;


            case Tok_Open_Mp_Dir_Reduction:

               if (! open_mp_clause_allowed[directive][Reduction_Omp_Clause]) {
                  PRINTMSG(TOKEN_LINE(token), 1370, Error, TOKEN_COLUMN(token),
                           "REDUCTION", open_mp_dir_str[directive]);
                  parse_err_flush(Find_EOS, NULL);
                  goto EXIT;
               }

               if (LA_CH_VALUE == LPAREN) {
                  NEXT_LA_CH;
                  /* determine operator */

                  NTR_IR_TBL(opr_ir_idx);
                  IR_LINE_NUM(opr_ir_idx) = LA_CH_LINE;
                  IR_COL_NUM(opr_ir_idx) = LA_CH_COLUMN;
                  IR_TYPE_IDX(opr_ir_idx) = INTEGER_DEFAULT_TYPE;

                  if (LA_CH_CLASS == Ch_Class_Letter) {

                     if (MATCHED_TOKEN_CLASS(Tok_Class_Id)) {

                        if (TOKEN_STR(token)[0] == 'M') {
                           if (strcmp(TOKEN_STR(token), "MAX") == 0) {
                              IR_OPR(opr_ir_idx) = Max_Opr;
                           }
                           else if (strcmp(TOKEN_STR(token), "MIN") == 0) {
                              IR_OPR(opr_ir_idx) = Min_Opr;
                           }
                           else {
                              parse_err_flush(Find_EOS, 
                                    "MAX, MIN, IAND, IOR, IEOR");
                              goto EXIT;
                           }
                        }
                        else if (TOKEN_STR(token)[0] == 'I') {
                           if (strcmp(TOKEN_STR(token), "IAND") == 0) {
                              IR_OPR(opr_ir_idx) = Band_Opr;
                           }
                           else if (strcmp(TOKEN_STR(token), "IOR") == 0) {
                              IR_OPR(opr_ir_idx) = Bor_Opr;
                           }
                           else if (strcmp(TOKEN_STR(token), "IEOR") == 0) {
                              IR_OPR(opr_ir_idx) = Bneqv_Opr;
                           }
                           else {
                              parse_err_flush(Find_EOS,
                                    "MAX, MIN, IAND, IOR, IEOR");
                              goto EXIT;
                           }
                        }
                        else {
                           parse_err_flush(Find_EOS, 
                                 "MAX, MIN, IAND, IOR, IEOR");
                           goto EXIT;
                        }
                     }
                     else {
                        parse_err_flush(Find_EOS, 
                              "MAX, MIN, IAND, IOR, IEOR");
                        goto EXIT;
                     }
                  }
                  else if (MATCHED_TOKEN_CLASS(Tok_Class_Op)) {
                     switch (TOKEN_VALUE(token)) {
                     case Tok_Op_Add:
                        IR_OPR(opr_ir_idx) = Plus_Opr;
                        break;

                     case Tok_Op_Sub:
                        IR_OPR(opr_ir_idx) = Minus_Opr;
                        break;

                     case Tok_Op_Mult:
                        IR_OPR(opr_ir_idx) = Mult_Opr;
                        break;

                     case Tok_Op_And:
                        IR_OPR(opr_ir_idx) = And_Opr;
                        break;

                     case Tok_Op_Or:
                        IR_OPR(opr_ir_idx) = Or_Opr;
                        break;

                     case Tok_Op_Eqv:
                        IR_OPR(opr_ir_idx) = Eqv_Opr;
                        break;

                     case Tok_Op_Neqv:
                        IR_OPR(opr_ir_idx) = Neqv_Opr;
                        break;

                     default:
                        parse_err_flush(Find_EOS, 
                              "+, *, -, .AND., .OR., .EQV., or .NEQV.");
                        goto EXIT;
                     }
                  }
                  else {
                     parse_err_flush(Find_EOS, "operator or intrinsic");
                     goto EXIT;
                  }

                  if (IL_IDX(list_array[OPEN_MP_REDUCTION_OPR_IDX]) ==
                                                                    NULL_IDX) {
                     NTR_IR_LIST_TBL(list_idx);
                     IL_FLD(list_array[OPEN_MP_REDUCTION_OPR_IDX]) =
                                                                  IL_Tbl_Idx;
                     IL_IDX(list_array[OPEN_MP_REDUCTION_OPR_IDX]) =
                                                                  list_idx;
                     IL_LIST_CNT(list_array[OPEN_MP_REDUCTION_OPR_IDX]) = 1;

                  }
                  else {
                     /* find the end of list */

                     list_idx = IL_IDX(list_array[OPEN_MP_REDUCTION_OPR_IDX]);
                     while (IL_NEXT_LIST_IDX(list_idx)) {
                        list_idx = IL_NEXT_LIST_IDX(list_idx);
                     }

                     NTR_IR_LIST_TBL(IL_NEXT_LIST_IDX(list_idx));
                     IL_PREV_LIST_IDX(IL_NEXT_LIST_IDX(list_idx)) = list_idx;
                     list_idx = IL_NEXT_LIST_IDX(list_idx);
                     IL_LIST_CNT(list_array[OPEN_MP_REDUCTION_OPR_IDX]) += 1;
                  }

                  IL_FLD(list_idx) = IR_Tbl_Idx;
                  IL_IDX(list_idx) = opr_ir_idx;

                  if (LA_CH_VALUE == COLON) {
                     NEXT_LA_CH;
                  }
                  else {
                     parse_err_flush(Find_EOS, ":");
                     goto EXIT;
                  }

                  /* parse var list */
                  parse_var_name_list(&opnd);

                  if (IL_IDX(list_array[OPEN_MP_REDUCTION_LIST_IDX]) == 
                                                                    NULL_IDX) {
                     NTR_IR_LIST_TBL(list_idx);
                     IL_FLD(list_array[OPEN_MP_REDUCTION_LIST_IDX]) = 
                                                                  IL_Tbl_Idx;
                     IL_IDX(list_array[OPEN_MP_REDUCTION_LIST_IDX]) = 
                                                                  list_idx;
                     IL_LIST_CNT(list_array[OPEN_MP_REDUCTION_LIST_IDX]) = 1;

                  }
                  else {
                     /* find the end of list */

                     list_idx = IL_IDX(list_array[OPEN_MP_REDUCTION_LIST_IDX]);
                     while (IL_NEXT_LIST_IDX(list_idx)) {
                        list_idx = IL_NEXT_LIST_IDX(list_idx);
                     }

                     NTR_IR_LIST_TBL(IL_NEXT_LIST_IDX(list_idx));
                     IL_PREV_LIST_IDX(IL_NEXT_LIST_IDX(list_idx)) = list_idx;
                     list_idx = IL_NEXT_LIST_IDX(list_idx);
                     IL_LIST_CNT(list_array[OPEN_MP_REDUCTION_LIST_IDX]) += 1;
                  }

                  COPY_OPND(IL_OPND(list_idx), opnd);

                  if (LA_CH_VALUE == RPAREN) {
                     NEXT_LA_CH;
                  }
                  else {
                     parse_err_flush(Find_EOS, ")");
                     goto EXIT;
                  }
               }
               else {
                  parse_err_flush(Find_EOS, "(");
                  goto EXIT;
               }

               break;

            case Tok_Open_Mp_Dir_Lastprivate:

               if (! open_mp_clause_allowed[directive][Lastprivate_Omp_Clause]){
                  PRINTMSG(TOKEN_LINE(token), 1370, Error, TOKEN_COLUMN(token),
                           "LASTPRIVATE", open_mp_dir_str[directive]);
                  parse_err_flush(Find_EOS, NULL);
                  goto EXIT;
               }

               if (LA_CH_VALUE == LPAREN) {
                  NEXT_LA_CH;
                  parse_var_common_list(&opnd, FALSE);

                  if (IL_IDX(list_array[OPEN_MP_LASTPRIVATE_IDX]) == NULL_IDX) {
                     COPY_OPND(IL_OPND(list_array[OPEN_MP_LASTPRIVATE_IDX]),
                               opnd);
                  }
                  else {
                     /* find the end of list */

                     list_idx = IL_IDX(list_array[OPEN_MP_LASTPRIVATE_IDX]);
                     while (IL_NEXT_LIST_IDX(list_idx)) {
                        list_idx = IL_NEXT_LIST_IDX(list_idx);
                     }

                     /* append the new list */
                     IL_NEXT_LIST_IDX(list_idx) = OPND_IDX(opnd);
                     IL_PREV_LIST_IDX(OPND_IDX(opnd)) = list_idx;
                     IL_LIST_CNT(list_array[OPEN_MP_LASTPRIVATE_IDX]) +=
                                                       OPND_LIST_CNT(opnd);
                  }

                  if (LA_CH_VALUE == RPAREN) {
                     NEXT_LA_CH;
                  }
                  else {
                     parse_err_flush(Find_EOS, ")");
                     goto EXIT;
                  }
               }
               else {
                  parse_err_flush(Find_EOS, "(");
                  goto EXIT;
               }

               break;

            case Tok_Open_Mp_Dir_Ordered:

               if (! open_mp_clause_allowed[directive][Ordered_Omp_Clause]) {
                  PRINTMSG(TOKEN_LINE(token), 1370, Error, TOKEN_COLUMN(token),
                           "ORDERED", open_mp_dir_str[directive]);
                  parse_err_flush(Find_EOS, NULL);
                  goto EXIT;
               }

               /* only one ORDERED clause allowed */

               if (IL_IDX(list_array[OPEN_MP_ORDERED_IDX]) != NULL_IDX) {
                  PRINTMSG(TOKEN_LINE(token), 1360, Error, TOKEN_COLUMN(token),
                           "ORDERED", open_mp_dir_str[directive]);
                  parse_err_flush(Find_EOS, NULL);
                  goto EXIT;
               }

               IL_LINE_NUM(list_array[OPEN_MP_ORDERED_IDX]) = 
                                                       TOKEN_LINE(token);
               IL_COL_NUM(list_array[OPEN_MP_ORDERED_IDX]) = 
                                                       TOKEN_COLUMN(token);
               IL_FLD(list_array[OPEN_MP_ORDERED_IDX]) = CN_Tbl_Idx;

               IL_IDX(list_array[OPEN_MP_ORDERED_IDX]) = CN_INTEGER_ONE_IDX;

               break;

            case Tok_Open_Mp_Dir_Schedule:

               if (! open_mp_clause_allowed[directive][Schedule_Omp_Clause]) {
                  PRINTMSG(TOKEN_LINE(token), 1370, Error, TOKEN_COLUMN(token),
                           "SCHEDULE", open_mp_dir_str[directive]);
                  parse_err_flush(Find_EOS, NULL);
                  goto EXIT;
               }

               /* only one SCHEDULE clause allowed */

               if (IL_IDX(list_array[OPEN_MP_SCHEDULE_TYPE_IDX]) != NULL_IDX) {
                  PRINTMSG(TOKEN_LINE(token), 1360, Error, TOKEN_COLUMN(token),
                           "SCHEDULE", open_mp_dir_str[directive]);
                  parse_err_flush(Find_EOS, NULL);
                  goto EXIT;
               }

               if (LA_CH_VALUE == LPAREN) {
                  NEXT_LA_CH;

                  if (MATCHED_TOKEN_CLASS(Tok_Class_Open_Mp_Dir_Kwd)) {

                     switch (TOKEN_VALUE(token)) {
                        case Tok_Open_Mp_Dir_Static:
                           the_constant = OPEN_MP_SCHEDULE_STATIC;
                           break;

                        case Tok_Open_Mp_Dir_Dynamic:
                           the_constant = OPEN_MP_SCHEDULE_DYNAMIC;
                           break;

                        case Tok_Open_Mp_Dir_Guided:
                           the_constant = OPEN_MP_SCHEDULE_GUIDED;
                           break;

                        case Tok_Open_Mp_Dir_Runtime:
                           the_constant = OPEN_MP_SCHEDULE_RUNTIME;
                           break;

                        default:
                           parse_err_flush(Find_EOS,"SCHEDULE type");
                           goto EXIT;
                     }

                     IL_FLD(list_array[OPEN_MP_SCHEDULE_TYPE_IDX]) =CN_Tbl_Idx;
                     IL_IDX(list_array[OPEN_MP_SCHEDULE_TYPE_IDX]) =
                                           C_INT_TO_CN(CG_INTEGER_DEFAULT_TYPE,
                                                       the_constant);

                     IL_LINE_NUM(list_array[OPEN_MP_SCHEDULE_TYPE_IDX]) =
                                                         TOKEN_LINE(token);
                     IL_COL_NUM(list_array[OPEN_MP_SCHEDULE_TYPE_IDX]) =
                                                         TOKEN_COLUMN(token);

                     if (LA_CH_VALUE == COMMA) {
                        NEXT_LA_CH;
                        parse_expr(&opnd);
                        COPY_OPND(IL_OPND(list_array[
                                  OPEN_MP_SCHEDULE_CHUNK_IDX]), opnd);
                     }

                     if (LA_CH_VALUE == RPAREN) {
                        NEXT_LA_CH;
                     }
                     else {
                        parse_err_flush(Find_EOS, ")");
                        goto EXIT;
                     }
                  }
                  else {
                     parse_err_flush(Find_EOS, "SCHEDULE type");
                     goto EXIT;
                  }
               }
               else {
                  parse_err_flush(Find_EOS, "(");
                  goto EXIT;
               }
               break;

# if (defined(_TARGET_OS_IRIX) || defined(_TARGET_OS_LINUX) || defined(_TARGET_OS_DARWIN))
            case Tok_Open_Mp_Dir_Affinity:

               if (! open_mp_clause_allowed[directive][Affinity_Omp_Clause]) {
                  PRINTMSG(TOKEN_LINE(token), 1370, Error, TOKEN_COLUMN(token),
                           "AFFINITY", open_mp_dir_str[directive]);
                  parse_err_flush(Find_EOS, NULL);
                  goto EXIT;
               }

               if (IL_IDX(list_array[OPEN_MP_AFFINITY_IDX]) != NULL_IDX) {
                  PRINTMSG(TOKEN_LINE(token), 1360, Error, TOKEN_COLUMN(token),
                           "AFFINITY", open_mp_dir_str[directive]);
                  parse_err_flush(Find_EOS, NULL);
                  goto EXIT;
               }

               if (! omp_extension_prefix(TOKEN_LINE(token))) {
                  /* token not on !$sgi line */
                  PRINTMSG(TOKEN_LINE(token), 1518, Warning,
                           TOKEN_COLUMN(token),
                           "AFFINITY clause", "!$SGI");
               }

               if (LA_CH_VALUE == LPAREN) {
                  NEXT_LA_CH;
                  parse_var_name_list(&opnd);

                  COPY_OPND(IL_OPND(list_array[OPEN_MP_AFFINITY_IDX]), opnd);

                  if (LA_CH_VALUE == RPAREN) {
                     NEXT_LA_CH;
                  }
                  else {
                     parse_err_flush(Find_EOS, ")");
                     goto EXIT;
                  }
               }
               else {
                  parse_err_flush(Find_EOS, "(");
                  goto EXIT;
               }

               if (LA_CH_VALUE == EQUAL) {

                  NEXT_LA_CH;

                  if (MATCHED_TOKEN_CLASS(Tok_Class_Open_Mp_Dir_Kwd)) {

                     IL_FLD(list_array[OPEN_MP_IS_THREAD_IDX]) = CN_Tbl_Idx;
                     IL_LINE_NUM(list_array[OPEN_MP_IS_THREAD_IDX]) =
                                                 TOKEN_LINE(token);
                     IL_COL_NUM(list_array[OPEN_MP_IS_THREAD_IDX]) =
                                                 TOKEN_COLUMN(token);

                     switch (TOKEN_VALUE(token)) {
                        case Tok_Open_Mp_Dir_Data:
                           IL_IDX(list_array[OPEN_MP_IS_THREAD_IDX]) =
                                               CN_INTEGER_ZERO_IDX;
                           break;
                        case Tok_Open_Mp_Dir_Thread:
                           IL_IDX(list_array[OPEN_MP_IS_THREAD_IDX]) =
                                               CN_INTEGER_ONE_IDX;

                           break;

                        default:
                           parse_err_flush(Find_EOS, "DATA or THREAD");
                           break;
                     }

                     if (LA_CH_VALUE == LPAREN) {

                        NEXT_LA_CH;

                        parse_expr(&opnd);

                        COPY_OPND(IL_OPND(list_array[
                                     OPEN_MP_THREAD_DATA_IDX]), opnd);

                        if (LA_CH_VALUE == RPAREN) {
                           NEXT_LA_CH;
                        }
                        else {
                           parse_err_flush(Find_EOS, ")");
                           goto EXIT;
                        }
                     }
                     else {
                        parse_err_flush(Find_EOS, "(");
                        goto EXIT;
                     }
                  }
                  else {
                     parse_err_flush(Find_EOS, "DATA or THREAD");
                  }

               }
               else {
                  parse_err_flush(Find_EOS, "=");
                  goto EXIT;
               }

               if (! dump_flags.dsm) {
                  opnd = null_opnd;

                  COPY_OPND(IL_OPND(list_array[OPEN_MP_AFFINITY_IDX]),
                            opnd);
                  COPY_OPND(IL_OPND(list_array[OPEN_MP_THREAD_DATA_IDX]),
                            opnd);
                  COPY_OPND(IL_OPND(list_array[OPEN_MP_IS_THREAD_IDX]),
                            opnd);
               }
               break;

            case Tok_Open_Mp_Dir_Onto:
               if (! open_mp_clause_allowed[directive][Onto_Omp_Clause]) {
                  PRINTMSG(TOKEN_LINE(token), 1370, Error, TOKEN_COLUMN(token),
                           "ONTO", open_mp_dir_str[directive]);
                  parse_err_flush(Find_EOS, NULL);
                  goto EXIT;
               }


               if (! omp_extension_prefix(TOKEN_LINE(token))) {
                  /* token not on !$sgi line */
                  PRINTMSG(TOKEN_LINE(token), 1518, Warning,
                           TOKEN_COLUMN(token),
                           "ONTO clause", "!$SGI");
               }

               if (seen_nest) {

                  if (IL_IDX(list_array[OPEN_MP_ONTO_IDX]) != NULL_IDX) {
                     PRINTMSG(TOKEN_LINE(token), 1360, Error,
                              TOKEN_COLUMN(token),
                              "ONTO", open_mp_dir_str[directive]);
                     parse_err_flush(Find_EOS, NULL);
                     goto EXIT;
                  }

                  if (LA_CH_VALUE == LPAREN) {
                     NEXT_LA_CH;
                     parse_int_or_star_list(&opnd);

                     COPY_OPND(IL_OPND(list_array[OPEN_MP_ONTO_IDX]), opnd);

                     if (LA_CH_VALUE == RPAREN) {
                        NEXT_LA_CH;
                     }
                     else {
                        parse_err_flush(Find_EOS, ")");
                        goto EXIT;
                     }
                  }
                  else {
                     parse_err_flush(Find_EOS, "(");
                     goto EXIT;
                  }

                  list_idx = list_array[OPEN_MP_ONTO_IDX];

                  list2_idx = list_array[OPEN_MP_NEST_IDX];

                  if (IL_FLD(list2_idx) != IL_Tbl_Idx ||
                      IL_LIST_CNT(list2_idx) != IL_LIST_CNT(list_idx)) {

                     /* error, onto count must equal nest count */

                     find_opnd_line_and_column(&IL_OPND(IL_IDX(list_idx)),
                                               &line, &column);

                     PRINTMSG(line, 1369, Error, column);
                  }
                  else if (IL_LIST_CNT(list2_idx) == 1) {
                     /* error, onto count must equal nest count */

                     find_opnd_line_and_column(&IL_OPND(IL_IDX(list_idx)),
                                               &line, &column);

                     PRINTMSG(line, 1377, Error, column);
                  }
               }
               else {
                  PRINTMSG(TOKEN_LINE(token), 1361, Error, TOKEN_COLUMN(token),
                           open_mp_dir_str[directive]);
                  parse_err_flush(Find_EOS, NULL);
                  goto EXIT;
               }
               break;

            case Tok_Open_Mp_Dir_Nest:

               if (! open_mp_clause_allowed[directive][Nest_Omp_Clause]) {
                  PRINTMSG(TOKEN_LINE(token), 1370, Error, TOKEN_COLUMN(token),
                           "NEST", open_mp_dir_str[directive]);
                  parse_err_flush(Find_EOS, NULL);
                  goto EXIT;
               }

               if (seen_nest) {
                  PRINTMSG(TOKEN_LINE(token), 1360, Error, TOKEN_COLUMN(token),
                           "NEST", open_mp_dir_str[directive]);
                  parse_err_flush(Find_EOS, NULL);
                  goto EXIT;
               }


               if (! omp_extension_prefix(TOKEN_LINE(token))) {
                  /* token not on !$sgi line */
                  PRINTMSG(TOKEN_LINE(token), 1518, Warning,
                           TOKEN_COLUMN(token),
                           "NEST clause", "!$SGI");
               }

               seen_nest = TRUE;

               if (LA_CH_VALUE == LPAREN) {
                  NEXT_LA_CH;
                  parse_var_name_list(&opnd);

                  COPY_OPND(IL_OPND(list_array[OPEN_MP_NEST_IDX]), opnd);

                  if (LA_CH_VALUE == RPAREN) {
                     NEXT_LA_CH;
                  }
                  else {
                     parse_err_flush(Find_EOS, ")");
                     goto EXIT;
                  }
               }
               else {
                  parse_err_flush(Find_EOS, "(");
                  goto EXIT;
               }

               break;
# endif

            default:
               PRINTMSG(TOKEN_LINE(token), 1517, Error, TOKEN_COLUMN(token),
                        "OpenMP");
               parse_err_flush(Find_EOS, NULL);
               goto EXIT;
         }
      }
      else {
         parse_err_flush(Find_EOS, "OpenMP clause");
      }

      if (LA_CH_VALUE == COMMA) {
         NEXT_LA_CH;
      }
   }

   if (open_mp_clause_allowed[directive][Schedule_Omp_Clause] &&
       IL_IDX(list_array[OPEN_MP_SCHEDULE_TYPE_IDX]) == NULL_IDX &&
       OPND_FLD(cdir_switches.mp_schedtype_opnd) != NO_Tbl_Idx) {

      COPY_OPND(IL_OPND(list_array[OPEN_MP_SCHEDULE_TYPE_IDX]),
                cdir_switches.mp_schedtype_opnd);
   }

EXIT:

   TRACE (Func_Exit, "parse_open_mp_clauses", NULL);

   return;

}  /* parse_open_mp_clauses */

/******************************************************************************\
|*									      *|
|* Description:								      *|
|*	Check for nesting of DO, SECTIONS, and SINGLE open mp directives.     *|
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

static void check_do_open_mp_nesting(void)

{
   int		blk_idx;
   int		ir_idx;

   TRACE (Func_Entry, "check_do_open_mp_nesting", NULL);

   ir_idx = SH_IR_IDX(curr_stmt_sh_idx);

   blk_idx = blk_stk_idx;

   while (blk_idx > 0) {
      if (BLK_TYPE(blk_idx) == Open_Mp_Parallel_Blk) {
         break;
      }

      if (BLK_TYPE(blk_idx) == Open_Mp_Do_Blk ||
          BLK_TYPE(blk_idx) == Open_Mp_Parallel_Do_Blk) {

         if (blk_idx < blk_stk_idx &&
             BLK_TYPE(blk_idx + 1) == Do_Blk &&
             BLK_DEF_LINE(blk_idx) == BLK_DEF_LINE(blk_idx + 1)) {

            PRINTMSG(IR_LINE_NUM(ir_idx), 1474, Error,
                     IR_COL_NUM(ir_idx));
            break;
         }
         else {
            /* this is a block that should have been closed */
            if (BLK_TYPE(blk_idx) == Open_Mp_Do_Blk) {
               CLEAR_DIRECTIVE_STATE(Open_Mp_Do_Region);
            }
            else {
               CLEAR_DIRECTIVE_STATE(Open_Mp_Parallel_Do_Region);
            }
            move_blk_to_end(blk_idx);
            POP_BLK_STK;
         }
      }
      else if (BLK_TYPE(blk_idx) == Open_Mp_Sections_Blk ||
               BLK_TYPE(blk_idx) == Open_Mp_Single_Blk ||
               BLK_TYPE(blk_idx) == Open_Mp_Parallel_Sections_Blk ||
	       BLK_TYPE(blk_idx) == Open_Mp_Workshare_Blk ||
	       BLK_TYPE(blk_idx) == Open_Mp_Parallel_Workshare_Blk ) {

         PRINTMSG(IR_LINE_NUM(ir_idx), 1474, Error,
                  IR_COL_NUM(ir_idx));
         break;
      }
      blk_idx--;
   }

   TRACE (Func_Exit, "check_do_open_mp_nesting", NULL);

   return;

}  /* check_do_open_mp_nesting */

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

static void check_ordered_open_mp_nesting(void)

{
   int          blk_idx;
   int		i;
   int          ir_idx;
   int		list_idx;

   TRACE (Func_Entry, "check_ordered_open_mp_nesting", NULL);

   ir_idx = SH_IR_IDX(curr_stmt_sh_idx);

   blk_idx = blk_stk_idx;

   while (blk_idx > 0) {
      if (BLK_TYPE(blk_idx) == Open_Mp_Parallel_Blk ||
          BLK_TYPE(blk_idx) == Open_Mp_Parallel_Sections_Blk) {

         /* did not bind to a DO or PARALLEL DO */
         PRINTMSG(IR_LINE_NUM(ir_idx), 1506, Error, 
                  IR_COL_NUM(ir_idx));
         break;
      }

      if (BLK_TYPE(blk_idx) == Open_Mp_Do_Blk ||
          BLK_TYPE(blk_idx) == Open_Mp_Parallel_Do_Blk) {

         /* check if ORDERED is specified. */

         list_idx = IR_IDX_L(SH_IR_IDX(BLK_FIRST_SH_IDX(blk_idx)));
         
         for (i = 0; i < OPEN_MP_ORDERED_IDX; i++) {
            list_idx = IL_NEXT_LIST_IDX(list_idx);
         }

         if (IL_FLD(list_idx) == NO_Tbl_Idx) {
            PRINTMSG(IR_LINE_NUM(ir_idx), 1507, Error, 
                     IR_COL_NUM(ir_idx));
         }

         break;
      }
      blk_idx--;
   }

   TRACE (Func_Exit, "check_ordered_open_mp_nesting", NULL);

   return;

}  /* check_ordered_open_mp_nesting */

/******************************************************************************\
|*                                                                            *|
|* Description:                                                               *|
|*      <description>                                                         *|
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

static boolean check_section_open_mp_context(void)

{
   int          blk_idx;
   int          ir_idx;
   boolean	ok = TRUE;

   TRACE (Func_Entry, "check_section_open_mp_context", NULL);

   ir_idx = SH_IR_IDX(curr_stmt_sh_idx);

   blk_idx = blk_stk_idx;

   while (blk_idx > 0) {
      if (BLK_TYPE(blk_idx) == Open_Mp_Sections_Blk ||
          BLK_TYPE(blk_idx) == Open_Mp_Parallel_Sections_Blk) {

         goto FOUND;
      }
      blk_idx--;
   }

   PRINTMSG(IR_LINE_NUM(ir_idx), 1412, Error, IR_COL_NUM(ir_idx),
            "C$OMP SECTION",
            "C$OMP PARALLEL SECTIONS region, or C$OMP SECTIONS region");

   ok = FALSE;

FOUND:

   TRACE (Func_Exit, "check_section_open_mp_context", NULL);

   return(ok);

}  /* check_section_open_mp_context */

/******************************************************************************\
|*									      *|
|* Description:								      *|
|*	This routine parses the !DIR$ CACHE_NOALLOCATE                        *|
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
static void parse_cache_noalloc(void)

{
   int		attr_idx;
   int		name_idx;


   TRACE (Func_Entry, "parse_cache_noalloc", NULL);

   do {
      if (MATCHED_TOKEN_CLASS(Tok_Class_Id)) {
         attr_idx = srch_sym_tbl(TOKEN_STR(token), TOKEN_LEN(token),
                                 &name_idx);

         if (attr_idx == NULL_IDX) {
            attr_idx			= ntr_sym_tbl(&token, name_idx);
            LN_DEF_LOC(name_idx)	= TRUE;
            AT_OBJ_CLASS(attr_idx)	= Data_Obj;
            ATD_CACHE_NOALLOC(attr_idx)	= TRUE;
         }
         else if (!fnd_semantic_err(Obj_No_Side_Effects,
                                    TOKEN_LINE(token),
                                    TOKEN_COLUMN(token),
                                    attr_idx,
                                    TRUE)) {

            if (AT_REFERENCED(attr_idx) == Char_Rslt_Bound_Ref) {
               AT_ATTR_LINK(attr_idx)	= NULL_IDX;
               LN_DEF_LOC(name_idx)	= TRUE;
            }

         }
      }
      else if (!parse_err_flush(Find_Comma, "procedure name")) {
         break;			/* Couldn't recover.  Hit EOS */
      }

      if (LA_CH_VALUE == COMMA) {
         NEXT_LA_CH;
      }
      else if (LA_CH_VALUE == EOS ||
               !parse_err_flush(Find_Comma, ", or "EOS_STR)) {
         break;
      }
      else {  /* Issued error and recovered at a comma */
         NEXT_LA_CH;
      }
   }
   while (TRUE);

   NEXT_LA_CH;		/* Pick up EOS */

   TRACE (Func_Exit, "parse_cache_noalloc", NULL);

   return;

}  /* parse_cache_noalloc */


/******************************************************************************\
|*									      *|
|* Description:								      *|
|*	This routine parses directives that are common to !DIR$ and !*$*      *|
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
static void parse_star_dir_directives(void)

{
   int		blk_idx;
   int		ir_idx;
   opnd_type	opnd;
   int		save_line_num;
   int		save_column_num;

	
   TRACE (Func_Exit, "parse_star_dir_directives", NULL);

   switch (TOKEN_VALUE(token)) {
   case Tok_SGI_Dir_Blockingsize:
   case Tok_Dir_Blockingsize:

      ir_idx = gen_directive_ir(Blockingsize_Dir_Opr);

      if (LA_CH_VALUE == LPAREN) {
         NEXT_LA_CH;

         if (LA_CH_VALUE != COMMA) {
            parse_expr(&opnd);
            COPY_OPND(IR_OPND_L(ir_idx), opnd);
         }
         else {
            IR_FLD_L(ir_idx)		= CN_Tbl_Idx;
            IR_IDX_L(ir_idx)		= CN_INTEGER_NEG_ONE_IDX;
            IR_LINE_NUM_L(ir_idx)	= LA_CH_LINE;
            IR_COL_NUM_L(ir_idx)	= LA_CH_COLUMN;
         }

         if (LA_CH_VALUE == COMMA) {
            NEXT_LA_CH;

            parse_expr(&opnd);
            COPY_OPND(IR_OPND_R(ir_idx), opnd);
         }
         else {
            IR_FLD_R(ir_idx)		= CN_Tbl_Idx;
            IR_IDX_R(ir_idx)		= CN_INTEGER_NEG_ONE_IDX;
            IR_LINE_NUM_R(ir_idx)	= LA_CH_LINE;
            IR_COL_NUM_R(ir_idx)	= LA_CH_COLUMN;
         }

         if (LA_CH_VALUE == RPAREN) {
            NEXT_LA_CH;
         }
         else {
            parse_err_flush(Find_EOS, ")");
            goto EXIT;
         }
      }
      else {
         parse_err_flush(Find_EOS, "(");
         goto EXIT;
      }
      break;
   

   case Tok_SGI_Dir_Blockable:
   case Tok_Dir_Blockable:

      if (LA_CH_VALUE == LPAREN) {
         save_line_num   = LA_CH_LINE;
         save_column_num = LA_CH_COLUMN;

         NEXT_LA_CH;

         if (parse_var_name_list(&opnd)) {

            if (LA_CH_VALUE == RPAREN) {

               if (OPND_LIST_CNT(opnd) > 1) {

                  /* Check to see if the another (preceding) BLOCKABLE        */
                  /* directive was already specified for the following (or    */
                  /* current) loop nest.  That is, if blockable_sh_idx is     */
                  /* null, there could still be an BLOCKABLE directive on a   */
                  /* containing loop so we have to look back through the Block*/
                  /* Stack to find out if this is true or not.  If            */
                  /* blockable_sh_idx is *not* null, it must mean we haven't  */
                  /* encountered the outer DO yet; just another directive.    */

                  if (cdir_switches.blockable_sh_idx == NULL_IDX) {

                     for (blk_idx = blk_stk_idx;  blk_idx > 1;  --blk_idx) {

                        if (BLK_TYPE(blk_idx) == Do_Blk) {
                           break;
                        }
                     }

                     if (blk_idx > 1 &&
                         SH_STMT_TYPE(BLK_FIRST_SH_IDX(blk_idx)) ==
                                                           Do_Iterative_Stmt &&
                         BLK_BLOCKABLE_NUM_LCVS(blk_idx) > 1) {

                        for ( ;  blk_idx > 1;  --blk_idx) {

                           if (BLK_BLOCKABLE_DIR_SH_IDX(blk_idx) != NULL_IDX) {
                              PRINTMSG(stmt_start_line, 1387, Error, 0,
                                       "BLOCKABLE");
                              SH_ERR_FLG(BLK_BLOCKABLE_DIR_SH_IDX(blk_idx)) =
                                 TRUE;
                              break;
                           }
                        }
                     }
                     else {
                        ir_idx = gen_directive_ir(Blockable_Dir_Opr);
                        COPY_OPND(IR_OPND_L(ir_idx), opnd);
                        cdir_switches.blockable_sh_idx = curr_stmt_sh_idx;
                        cdir_switches.blockable_group++;
                     }
                  }
                  else {
                     PRINTMSG(stmt_start_line, 1387, Error, 0, "BLOCKABLE");
                     SH_ERR_FLG(cdir_switches.blockable_sh_idx) = TRUE;
                  }
               }
               else {
                  PRINTMSG(save_line_num, 1375, Error, save_column_num);
               }
            }
         }
         else {
            parse_err_flush(Find_EOS, NULL);
            goto EXIT;
         }

         if (LA_CH_VALUE == RPAREN) {
            NEXT_LA_CH;
         }
         else {
            parse_err_flush(Find_EOS, ", or )");
            goto EXIT;
         }
      }
      else {
         parse_err_flush(Find_EOS, "(");
         goto EXIT;
      }
      break;

   case Tok_SGI_Dir_Interchange:
   case Tok_Dir_Interchange:

      if (LA_CH_VALUE == LPAREN) {
         save_line_num   = LA_CH_LINE;
         save_column_num = LA_CH_COLUMN;

         NEXT_LA_CH;

         if (parse_var_name_list(&opnd)) {

            if (LA_CH_VALUE == RPAREN) {

               if (OPND_LIST_CNT(opnd) > 1) {

                  /* Check to see if the another (preceding) INTERCHANGE      */
                  /* directive was already specified for the following (or    */
                  /* current) loop nest.  That is, if interchange_sh_idx is   */
                  /* null, there could still be an INTERCHANGE directive on a */
                  /* containing loop so we have to look back through the Block*/
                  /* stack to find out if this is true or not.  If            */
                  /* interchange_sh_idx is *not* null, it must mean we haven't*/
                  /* encountered the outer DO yet; just another directive.    */

                  if (cdir_switches.interchange_sh_idx == NULL_IDX) {

                     for (blk_idx = blk_stk_idx;  blk_idx > 1;  --blk_idx) {
        
                        if (BLK_TYPE(blk_idx) == Do_Blk) {
                           break;
                        }
                     }

                     if (blk_idx > 1                                 &&
                         SH_STMT_TYPE(BLK_FIRST_SH_IDX(blk_idx)) == 
                            Do_Iterative_Stmt                        &&
                         BLK_INTERCHANGE_NUM_LCVS(blk_idx) > 1) {

                        for ( ;  blk_idx > 1;  --blk_idx) {

                           if (BLK_INTERCHANGE_DIR_SH_IDX(blk_idx) !=
                                  NULL_IDX) {
                              PRINTMSG(stmt_start_line, 1387, Error, 0,
                                       "INTERCHANGE");
                              SH_ERR_FLG(BLK_INTERCHANGE_DIR_SH_IDX(blk_idx)) =
                                 TRUE;
                              break;
                           }
                        }
                     }
                     else {
                        ir_idx = gen_directive_ir(Interchange_Dir_Opr);
                        COPY_OPND(IR_OPND_L(ir_idx), opnd);
                        cdir_switches.interchange_sh_idx = curr_stmt_sh_idx;
                        cdir_switches.interchange_group++;
                     }
                  }
                  else {
                     PRINTMSG(stmt_start_line, 1387, Error, 0, "INTERCHANGE");
                     SH_ERR_FLG(cdir_switches.interchange_sh_idx) = TRUE;
                  }
               }
               else {
                  PRINTMSG(save_line_num, 1375, Error, save_column_num);
               }
            }
         }
         else {
            parse_err_flush(Find_EOS, NULL);
            goto EXIT;
         }

         if (LA_CH_VALUE == RPAREN) {
            NEXT_LA_CH;
         }
         else {
            parse_err_flush(Find_EOS, ", or )");
            goto EXIT;
         }
      }
      else {
         parse_err_flush(Find_EOS, "(");
         goto EXIT;
      }
   }

   if (LA_CH_VALUE != EOS) {
      PRINTMSG(LA_CH_LINE, 790, Warning, LA_CH_COLUMN);
      parse_err_flush(Find_EOS, NULL);
   }

   NEXT_LA_CH;

EXIT:

   TRACE (Func_Exit, "parse_star_dir_directives", NULL);

   return;

}  /* parse_star_dir_directives */
