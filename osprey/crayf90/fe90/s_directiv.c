/*
 * Copyright (C) 2009 Advanced Micro Devices, Inc.  All Rights Reserved.
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



static char USMID[] = "\n@(#)5.0_pl/sources/s_directiv.c	5.12	10/28/99 10:03:56\n";

# include "defines.h"           /* Machine dependent ifdefs */

# include "host.m"              /* Host machine dependent macros.*/
# include "host.h"              /* Host machine dependent header.*/
# include "target.m"            /* Target machine dependent macros.*/
# include "target.h"            /* Target machine dependent header.*/

# include "globals.m"
# include "tokens.m"
# include "sytb.m"
# include "s_globals.m"
# include "debug.m"

# include "globals.h"
# include "tokens.h"
# include "sytb.h"
# include "s_globals.h"

#ifdef KEY /* Bug 4889 */
/* Set to the sh_idx when we see an OpenMP "do" or "paralleldo" directive,
 * cleared when we reach the "do" statement itself.
 */
int			 inside_paralleldo;
/* Set to the sh_idx when we see an OpenMP "parallel" directive; cleared when
 * we reach the OpenMP "endparallel"
 */
int			 inside_parallel;
#endif /* KEY Bug 4889 */

/*****************************************************************\
|* function prototypes of static functions declared in this file *|
\*****************************************************************/

static void	add_common_blk_objects_to_list(int, int);
static boolean	assert_semantics(void);
static boolean	attr_is_in_list(int, int);
static void	doall_cmic_semantics(void);
static void	doparallel_cmic_semantics(void);
static void	end_blk_mp_semantics(boolean);
static void	set_mp_task_flags(int, boolean);
static void	endparallel_cmic_semantics(void);
static boolean	has_been_reprivatized(int);
static void	mp_directive_semantics(mp_directive_type);
static boolean	multiple_clause_err(int, int);
static void	open_mp_directive_semantics(open_mp_directive_type);
static void	open_mp_copyprivate_semantics();
static void	parallel_cmic_semantics(void);
static int	pop_task_blk(void);
static boolean	power_o_two(int);
static void	prefetch_ref_semantics(void);
static void	push_task_blk(int);
static void	set_open_mp_task_flags(int, boolean);
static void	wait_send_semantics(void);

/******************************************************************************\
|*									      *|
|* Description:								      *|
|*	Pass 2 processing for some directive stmts.                           *|
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

void directive_stmt_semantics(void)

{
   int			attr_idx;
   int			column;
   expr_arg_type	exp_desc;
   int			host_attr_idx;
   int			idx;
   int			il_idx;
   int			ir_idx;
   opnd_type            l_opnd;
   int			line;
   int			list_idx;
   int			name_idx;
   int			new_il_idx;
   boolean		null_point;
   long64		num_cpus;
   long			num_cpu_value;
   boolean		ok			= TRUE;
   int			old_ir_idx;
   opnd_type		opnd;
   int			prev_idx;
   expr_mode_type	save_expr_mode;
   int			sn_idx;


   TRACE (Func_Entry, "directive_stmt_semantics", NULL);

   ir_idx = SH_IR_IDX(curr_stmt_sh_idx);

   switch(IR_OPR(ir_idx)) {

      case Aggressiveinnerloopfission_Opr:
         cdir_switches.aggressiveinnerloopfission = TRUE;
         break;

      case Align_Cdir_Opr:
         cdir_switches.align = TRUE;
         break;


      case Bl_Cdir_Opr:
         cdir_switches.bl = TRUE;
         break;


      case Blockable_Dir_Opr:
         cdir_switches.blockable_sh_idx = curr_stmt_sh_idx;
         cdir_switches.blockable_group++;
         cdir_switches.blockable_count = 
                                 IR_LIST_CNT_L(SH_IR_IDX(curr_stmt_sh_idx));
         break;


      case Bounds_Cdir_Opr:
      case Nobounds_Cdir_Opr:

         if (IR_FLD_L(ir_idx) == IL_Tbl_Idx) {
            list_idx = IR_IDX_L(ir_idx);

            while (list_idx) {

               /* set in_call_list to TRUE so we don't get msg about */
               /* assumed size array use.                            */

               in_call_list = TRUE;

               COPY_OPND(opnd, IL_OPND(list_idx));
               xref_state = CIF_Symbol_Reference;
               exp_desc.rank = 0;
               ok &= expr_semantics(&opnd, &exp_desc);
               in_call_list = FALSE;

               attr_idx = find_left_attr(&opnd);

               find_opnd_line_and_column(&opnd, &line, &column);

               if (attr_idx == NULL_IDX ||
                   AT_OBJ_CLASS(attr_idx) != Data_Obj) {

                  PRINTMSG(line, 1141, Error, column, 
                       (IR_OPR(ir_idx) == Bounds_Cdir_Opr ?
                                    "BOUNDS" : "NOBOUNDS"));
               }

               IL_FLD(list_idx) = AT_Tbl_Idx;
               IL_IDX(list_idx) = attr_idx;
               IL_LINE_NUM(list_idx) = line;
               IL_COL_NUM(list_idx) = column;

               list_idx = IL_NEXT_LIST_IDX(list_idx);
            }
         }

         bounds_cdir_handler(ir_idx);

         break;


      case Cachealign_Cdir_Opr :

         if (IR_FLD_L(ir_idx) == IL_Tbl_Idx &&
             IR_LIST_CNT_L(ir_idx) > 0) {

            list_idx = IR_IDX_L(ir_idx);

            while (list_idx) {

               /* set in_call_list to TRUE so we don't get msg about */
               /* assumed size array use.                            */

               in_call_list = TRUE;

               COPY_OPND(opnd, IL_OPND(list_idx));
               exp_desc.rank = 0;
               xref_state = CIF_Symbol_Reference;
               ok = expr_semantics(&opnd, &exp_desc);

               attr_idx = find_left_attr(&opnd);

               if (AT_OBJ_CLASS(attr_idx) != Data_Obj ||
                   ATD_CLASS(attr_idx) != Variable ||
                   ATD_IN_COMMON(attr_idx)) {

                  find_opnd_line_and_column(&opnd, &line, &column);
                  PRINTMSG(line, 1067, Error, column);
               }
               else if (ATD_CACHE_ALIGN(attr_idx)) {
                  find_opnd_line_and_column(&opnd, &line, &column);
                  PRINTMSG(line, 1065, Error, column);
               }
               else {
                  ATD_CACHE_ALIGN(attr_idx) = TRUE;
               }

               list_idx = IL_NEXT_LIST_IDX(list_idx);
            }

            in_call_list = FALSE;
         }

         break;

      case Cache_Bypass_Cdir_Opr:

         if (IR_FLD_L(ir_idx) == IL_Tbl_Idx) { /* Should contain an IL list */
            cdir_switches.cache_bypass_ir_idx	= ir_idx;

            if (IR_LIST_CNT_L(ir_idx) > 0) {
               list_idx = IR_IDX_L(ir_idx);

               while (list_idx) {

                  /* set in_call_list to TRUE so we don't get msg about */
                  /* assumed size array use.                            */

                  in_call_list = TRUE;

                  COPY_OPND(opnd, IL_OPND(list_idx));
                  exp_desc.rank	= 0;
                  xref_state	= CIF_Symbol_Reference;
                  ok		= expr_semantics(&opnd, &exp_desc);

                  attr_idx	= find_left_attr(&opnd);

                  if (AT_OBJ_CLASS(attr_idx) == Interface &&
                      ATI_PROC_IDX(attr_idx) != NULL_IDX) {
                     attr_idx = ATI_PROC_IDX(attr_idx);
                  }

                  if (AT_OBJ_CLASS(attr_idx) == Pgm_Unit &&
                      ATP_PGM_UNIT(attr_idx) == Function &&
                      !ATP_RSLT_NAME(attr_idx)) {
                     attr_idx = ATP_RSLT_IDX(attr_idx);
                  }

                  if (AT_OBJ_CLASS(attr_idx) != Data_Obj ||
                      ATD_ARRAY_IDX(attr_idx) == NULL_IDX) {
                     find_opnd_line_and_column(&opnd, &line, &column);
                     PRINTMSG(line, 1318, Error, column, 
                              AT_OBJ_NAME_PTR(attr_idx));
                  }
                  else if (TYP_LINEAR(ATD_TYPE_IDX(attr_idx)) != Integer_8 &&
                           TYP_LINEAR(ATD_TYPE_IDX(attr_idx)) != Real_8 &&
                           TYP_LINEAR(ATD_TYPE_IDX(attr_idx)) != Logical_8 &&
                           TYP_LINEAR(ATD_TYPE_IDX(attr_idx)) != Complex_8) {
                     find_opnd_line_and_column(&opnd, &line, &column);
                     PRINTMSG(line, 1320, Error, column, 
                              AT_OBJ_NAME_PTR(attr_idx));
                  }
                  else {
                     ATD_CACHE_BYPASS_ARRAY(attr_idx)	= TRUE;
                  }
                  list_idx = IL_NEXT_LIST_IDX(list_idx);
               }
            }
            in_call_list = FALSE;
         }
         break;

      case Cncall_Cmic_Opr:
         cdir_switches.cncall = TRUE;
         break;

      case Concurrentize_Star_Opr:
         break;

      case Noconcurrentize_Star_Opr:
         break;

      case Fissionable_Star_Opr:
         cdir_switches.fissionable = TRUE;
         break;

      case Flush_Star_Opr:
         list_idx = IR_IDX_L(ir_idx);

         while (list_idx != NULL_IDX) {
            if (IL_FLD(list_idx) == AT_Tbl_Idx) {
               attr_idx = IL_IDX(list_idx);
               AT_LOCKED_IN(attr_idx) = TRUE;

               while (AT_ATTR_LINK(attr_idx)) {
                  attr_idx = AT_ATTR_LINK(attr_idx);
                  AT_LOCKED_IN(attr_idx) = TRUE;
               }

               if (AT_OBJ_CLASS(attr_idx) != Data_Obj) {
                  PRINTMSG(IL_LINE_NUM(list_idx), 1480, Error,
                           IL_COL_NUM(list_idx));
               }

               IL_IDX(list_idx) = attr_idx;
            }

            list_idx = IL_NEXT_LIST_IDX(list_idx);
         }
         break;

      case Fusable_Star_Opr:
         cdir_switches.fusable = TRUE;
         break;

      case Inline_Cdir_Opr:
         cdir_switches.do_inline = TRUE;
         break;

      case Interchange_Dir_Opr:
         cdir_switches.interchange_sh_idx = curr_stmt_sh_idx;;
         cdir_switches.interchange_group++;
         cdir_switches.interchange_count =
                                 IR_LIST_CNT_L(SH_IR_IDX(curr_stmt_sh_idx));
         break;

      case Ivdep_Cdir_Opr:
         cdir_switches.ivdep = TRUE;

         if (IR_FLD_L(ir_idx) != NO_Tbl_Idx) {
            COPY_OPND(opnd, IR_OPND_L(ir_idx));
            exp_desc.rank = 0;
            xref_state = CIF_Symbol_Reference;
            ok = expr_semantics(&opnd, &exp_desc);

            if (OPND_FLD(opnd) != CN_Tbl_Idx ||
                exp_desc.rank  != 0          ||
                exp_desc.type  != Integer)   {
               find_opnd_line_and_column(&opnd, &line, &column);
               PRINTMSG(line, 796, Error, column);
            }
            else if (compare_cn_and_value(OPND_IDX(opnd), 1, Lt_Opr) ||
                     compare_cn_and_value(OPND_IDX(opnd), 1024, Gt_Opr)) {
               find_opnd_line_and_column(&opnd, &line, &column);
               PRINTMSG(line, 796, Error, column);
            }
            else {
               cdir_switches.safevl_idx = OPND_IDX(opnd);
            }
         }
         else {
            cdir_switches.safevl_idx = const_safevl_idx;
         }

         break;


      case Concurrent_Cdir_Opr:

         cdir_switches.concurrent = TRUE;

         if (IR_FLD_L(ir_idx) != NO_Tbl_Idx) {
            COPY_OPND(opnd, IR_OPND_L(ir_idx));
            exp_desc.rank	= 0;
            xref_state		= CIF_Symbol_Reference;
            ok			= expr_semantics(&opnd, &exp_desc);

            if (OPND_FLD(opnd) != CN_Tbl_Idx ||
                exp_desc.rank  != 0          ||
                exp_desc.type  != Integer)   {
               find_opnd_line_and_column(&opnd, &line, &column);
               PRINTMSG(line, 1422, Error, column);
            }
            else if (fold_relationals(OPND_IDX(opnd),
                                      CN_INTEGER_ONE_IDX,
                                      Lt_Opr)) {

               /* Value must be >= 1 */

               find_opnd_line_and_column(&opnd, &line, &column);
               PRINTMSG(line, 1422, Error, column);
            }
            else {
               cdir_switches.concurrent_idx = OPND_IDX(opnd);
            }
         }
         break;

      case Mark_Cdir_Opr:
         cdir_switches.mark = TRUE;

         if (IR_FLD_L(ir_idx) == CN_Tbl_Idx) {
            cdir_switches.mark_dir_idx	= IR_IDX_L(ir_idx);
         }
         break;

      case Nextscalar_Cdir_Opr:
         cdir_switches.nextscalar = TRUE;
         break;

      case Noblocking_Dir_Opr:
         cdir_switches.noblocking = TRUE;
         break;

      case Nofission_Star_Opr:
         cdir_switches.nofission = TRUE;
         break;

      case Nofusion_Star_Opr:
         cdir_switches.nofusion = TRUE;
         break;

      case Nointerchange_Dir_Opr:
         cdir_switches.nointerchange = TRUE;
         break;

      case Nomark_Cdir_Opr:
         cdir_switches.mark		= FALSE;
         cdir_switches.mark_dir_idx	= NULL_IDX;
         break;


      case Nobl_Cdir_Opr:
         cdir_switches.bl = FALSE;
         break;

      case Noinline_Cdir_Opr:
         cdir_switches.do_inline = FALSE;
         break;

      case Nopattern_Cdir_Opr:
         cdir_switches.pattern = FALSE;
         break;


      case Norecurrence_Cdir_Opr:
         cdir_switches.recurrence = FALSE;
         break;


      case Nosplit_Cdir_Opr:
         cdir_switches.split = FALSE;
         break;


      case Nostream_Dir_Opr:
         cdir_switches.stream = FALSE;
         break;


      case Notask_Cdir_Opr:
         cdir_switches.task = FALSE;
         cdir_switches.notask_region = TRUE;
         break;


      case Nounroll_Cdir_Opr:

         /* 1 means NO unrolling */

         cdir_switches.unroll_count_idx = CN_INTEGER_ONE_IDX;
         cdir_switches.unroll_dir	= TRUE;
         break;


      case Novector_Cdir_Opr:
         cdir_switches.vector = FALSE;
         break;


      case Novsearch_Cdir_Opr:
         cdir_switches.vsearch = FALSE;
         break;

      case Opaque_Star_Opr:
         cdir_switches.opaque = TRUE;
         break;


      case Pattern_Cdir_Opr:
         cdir_switches.pattern = TRUE;
         break;


      case Permutation_Cmic_Opr:
         cdir_switches.permutation = TRUE;
         break;


      case Preferstream_Nocinv_Dir_Opr:
         cdir_switches.preferstream_nocinv = TRUE;

         /* Intentional fall through */

      case Preferstream_Dir_Opr:
         cdir_switches.preferstream = TRUE;
         break;


      case Prefertask_Cdir_Opr:
         cdir_switches.prefertask = TRUE;
         break;


      case Prefervector_Cdir_Opr:
         cdir_switches.prefervector = TRUE;
         break;


      case Recurrence_Cdir_Opr:
         cdir_switches.recurrence = TRUE;
         break;


      case Shortloop_Cdir_Opr:
         cdir_switches.shortloop = TRUE;

         if (cdir_switches.shortloop128) {
            cdir_switches.shortloop128 = FALSE;
         }

         break;


      case Split_Cdir_Opr:
         cdir_switches.split = TRUE;
         break;


      case Shortloop128_Cdir_Opr:
         cdir_switches.shortloop128 = TRUE;

         if (cdir_switches.shortloop) {
            cdir_switches.shortloop = FALSE;
         }

         break;


      case Stream_Dir_Opr:
         cdir_switches.stream = TRUE;
         break;


      case Suppress_Opr:
         list_idx = IR_IDX_L(ir_idx);

         while (list_idx) {
            /* set in_call_list to TRUE so we don't get msg about */
            /* assumed size array use.                            */

            in_call_list = TRUE;

            COPY_OPND(opnd, IL_OPND(list_idx));
            exp_desc.rank = 0;
            xref_state = CIF_Symbol_Reference;
            ok = expr_semantics(&opnd, &exp_desc);

            while (OPND_FLD(opnd) == IR_Tbl_Idx &&
                   (IR_OPR(OPND_IDX(opnd)) == Whole_Substring_Opr ||
                    IR_OPR(OPND_IDX(opnd)) == Whole_Subscript_Opr ||
                    (IR_OPR(OPND_IDX(opnd)) == Subscript_Opr &&
                     IR_FLD_R(OPND_IDX(opnd)) == IL_Tbl_Idx &&
                     IL_PE_SUBSCRIPT(IR_IDX_R(OPND_IDX(opnd)))) ||
                    IR_OPR(OPND_IDX(opnd)) == Dv_Deref_Opr)) {

               COPY_OPND(opnd, IR_OPND_L(OPND_IDX(opnd)));
            }

            if (OPND_FLD(opnd) != AT_Tbl_Idx) {
               find_opnd_line_and_column(&opnd, &line, &column);
               PRINTMSG(line, 1487, Error, column, "SUPPRESS");
            }

            COPY_OPND(IL_OPND(list_idx), opnd);

            list_idx = IL_NEXT_LIST_IDX(list_idx);
         }

         in_call_list = FALSE;
         break;


      case Task_Cdir_Opr:
         cdir_switches.task = TRUE;
         cdir_switches.notask_region = FALSE;
         break;


      case Unroll_Cdir_Opr:

         if (IR_FLD_L(ir_idx) != NO_Tbl_Idx) {
            COPY_OPND(opnd, IR_OPND_L(ir_idx));
            save_expr_mode	= expr_mode;
            exp_desc.rank	= 0;
            xref_state		= CIF_Symbol_Reference;
            expr_mode		= Initialization_Expr;
            ok			= expr_semantics(&opnd, &exp_desc);
            COPY_OPND(IR_OPND_L(ir_idx), opnd);

            if (OPND_FLD(opnd) != CN_Tbl_Idx ||
                exp_desc.rank  != 0          ||
                exp_desc.type  != Integer)   {
               find_opnd_line_and_column(&opnd, &line, &column);
               PRINTMSG(line, 1105, Error, column);
               IR_IDX_L(ir_idx) = CN_INTEGER_ONE_IDX;
               IR_FLD_L(ir_idx)	= CN_Tbl_Idx;
            }
            else if (fold_relationals(OPND_IDX(opnd),
                                      CN_INTEGER_ZERO_IDX,
                                      Eq_Opr)) {

               /* Directive is "UNROLL 0".  Force it to no unrolling,  */
               /* so send an unroll count of 1 to pdgcs!!              */

               IR_IDX_L(ir_idx)		= CN_INTEGER_ONE_IDX;
               IR_FLD_L(ir_idx)		= CN_Tbl_Idx;
            }
            else if (compare_cn_and_value(OPND_IDX(opnd), 0, Lt_Opr) ||
                     compare_cn_and_value(OPND_IDX(opnd), 1024, Gt_Opr)) {
               find_opnd_line_and_column(&opnd, &line, &column);
               PRINTMSG(line, 1105, Error, column);
               IR_IDX_L(ir_idx) = CN_INTEGER_ONE_IDX;
               IR_FLD_L(ir_idx)	= CN_Tbl_Idx;
            }

            cdir_switches.unroll_count_idx	= IR_IDX_L(ir_idx);
            cdir_switches.unroll_dir		= TRUE;
            expr_mode				= save_expr_mode;
         }
         else {
            cdir_switches.unroll_count_idx	= CN_INTEGER_ZERO_IDX;
            cdir_switches.unroll_dir		= TRUE;
#ifdef KEY /* Bug 7489 */
	    IR_IDX_L(ir_idx)		= CN_INTEGER_ZERO_IDX;
	    IR_FLD_L(ir_idx)		= CN_Tbl_Idx;
#endif /* KEY Bug 7489 */
         }
         break;


      case Vector_Cdir_Opr:
         cdir_switches.vector = TRUE;
         break;


      case Vsearch_Cdir_Opr:
         cdir_switches.vsearch = TRUE;
         break;


      /* -------------------------------------------------------------------- */
      /*								      */
      /*            			 CMIC$'s                              */
      /*								      */
      /* -------------------------------------------------------------------- */

      /* -------------------------------------------------------------------- */
      /*                                 DOALL				      */
      /* -------------------------------------------------------------------- */

      case Doall_Cmic_Opr:

         doall_cmic_semantics();
         break;


      /* -------------------------------------------------------------------- */
      /*                              DOPARALLEL			      */
      /* -------------------------------------------------------------------- */

      case Doparallel_Cmic_Opr:

         doparallel_cmic_semantics();
         break;

      case Enddo_Cmic_Opr:
         if (IR_OPR(SH_IR_IDX(SH_NEXT_IDX(curr_stmt_sh_idx))) ==
                                        Endparallel_Cmic_Opr) {
            /* use a const 1 as a flag to prevent the extra barrier on irix */
            IR_FLD_L(ir_idx) = CN_Tbl_Idx;
            IR_IDX_L(ir_idx) = CN_INTEGER_ONE_IDX;
            IR_LINE_NUM_L(ir_idx) = IR_LINE_NUM(ir_idx);
            IR_COL_NUM_L(ir_idx) = IR_COL_NUM(ir_idx);
         }

         wait_send_semantics();
         break;


      /* -------------------------------------------------------------------- */
      /*           	GUARD   		END GUARD		      */
      /* -------------------------------------------------------------------- */

      case Guard_Cmic_Opr:
      case Endguard_Cmic_Opr:

         if (IR_FLD_L(ir_idx) != NO_Tbl_Idx) {

            COPY_OPND(opnd, IR_OPND_L(ir_idx));
            exp_desc.rank = 0;
            xref_state    = CIF_Symbol_Reference;
            ok = expr_semantics(&opnd, &exp_desc);
            IR_FLD_L(ir_idx) = AT_Tbl_Idx;
            idx = create_tmp_asg(&opnd,
                                 &exp_desc,
                                 &l_opnd,
                                 Intent_In,
                                 FALSE,
                                 FALSE);
            IR_IDX_L(ir_idx) = idx;
            IR_LINE_NUM_L(ir_idx) = IR_LINE_NUM(ir_idx);
            IR_COL_NUM_L(ir_idx)  = IR_COL_NUM(ir_idx);
         }

         break;


      /* -------------------------------------------------------------------- */
      /*         	   	     END PARALLEL			      */
      /* -------------------------------------------------------------------- */

      case Endparallel_Cmic_Opr:

          endparallel_cmic_semantics();
          break;


      /* -------------------------------------------------------------------- */
      /*         	   	     NUMCPUS				      */
      /* -------------------------------------------------------------------- */

      case Numcpus_Cmic_Opr:

         if (cdir_switches.parallel_region) {

            /* Numcpus is illegal within a parallel region */

            PRINTMSG(stmt_start_line, 1121, Error, stmt_start_col);
         }

         COPY_OPND(opnd, IR_OPND_L(ir_idx));
         exp_desc.rank = 0;
         xref_state = CIF_Symbol_Reference;
         ok = expr_semantics(&opnd, &exp_desc);
         COPY_OPND(IR_OPND_L(ir_idx), opnd);
         num_cpus = CN_INT_TO_C(IR_IDX_L(ir_idx));

         if (IR_FLD_L(ir_idx) == CN_Tbl_Idx && (num_cpus < 1 || num_cpus > 64)){

            if (num_cpus < 1) {
               num_cpu_value	= 1;
               IR_IDX_L(ir_idx)	= CN_INTEGER_ONE_IDX;
            }
            else {
               num_cpu_value	= 64;
               IR_IDX_L(ir_idx) = C_INT_TO_CN(CG_INTEGER_DEFAULT_TYPE,
                                              num_cpu_value);
            }

            PRINTMSG(stmt_start_line, 1122, Warning, 
                     stmt_start_col,
                     (long) num_cpus,
                     num_cpu_value);
         }

         break;


      /* -------------------------------------------------------------------- */
      /*         	   	     PARALLEL				      */
      /* -------------------------------------------------------------------- */

      case Parallel_Cmic_Opr:
         
         parallel_cmic_semantics();
         break;

      /* -------------------------------------------------------------------- */
      /*         	   	     SEND				      */
      /* -------------------------------------------------------------------- */

      case Send_Cmic_Opr:

         NTR_IR_LIST_TBL(new_il_idx);

         IL_FLD(new_il_idx)		= IR_Tbl_Idx;
         IL_IDX(new_il_idx)		= ir_idx;
         IL_LINE_NUM(new_il_idx)	= IR_LINE_NUM(ir_idx);
         IL_COL_NUM(new_il_idx)		= IR_COL_NUM(ir_idx);

         if (IR_FLD_L(ir_idx) != NO_Tbl_Idx) {  /* POINT */
            COPY_OPND(opnd, IR_OPND_L(ir_idx));
            exp_desc.rank = 0;
            xref_state    = CIF_Symbol_Reference;
            ok = expr_semantics(&opnd, &exp_desc);

            if (exp_desc.type != Integer || exp_desc.rank != 0) {
               find_opnd_line_and_column(&opnd, &line, &column);
               PRINTMSG(line, 1431, Error, column, "POINT", "SEND");
            }

            COPY_OPND(IR_OPND_L(ir_idx), opnd);
         }

         if (IR_FLD_R(ir_idx) != NO_Tbl_Idx) {  /* IF */
            COPY_OPND(opnd, IR_OPND_R(ir_idx));
            exp_desc.rank = 0;
            xref_state    = CIF_Symbol_Reference;
            ok = expr_semantics(&opnd, &exp_desc);
            find_opnd_line_and_column(&opnd, &line, &column);

            if (ok && (exp_desc.type != Logical || exp_desc.rank != 0)) {
               PRINTMSG(line, 1433, Error, column, "IF", "SEND");
            }
            COPY_OPND(IR_OPND_R(ir_idx), opnd);

            IR_FLD_R(ir_idx) = AT_Tbl_Idx;
            idx = create_tmp_asg(&opnd,
                                 &exp_desc,
                                 &l_opnd,
                                 Intent_In,
                                 FALSE,
                                 FALSE);
            IR_IDX_R(ir_idx)		= idx;
            IR_LINE_NUM_R(ir_idx)	= line;
            IR_COL_NUM_R(ir_idx)	= column;
         }

         if (cdir_switches.send_list_idx == NULL_IDX) {
            cdir_switches.send_list_idx = new_il_idx;
         }
         else {
            il_idx	= cdir_switches.send_list_idx;

            while (il_idx != NULL_IDX) {
               prev_idx	= il_idx;
               il_idx	= IL_NEXT_LIST_IDX(il_idx);
            }

            IL_NEXT_LIST_IDX(prev_idx) = new_il_idx;
         }
         break;

      /* -------------------------------------------------------------------- */
      /*         	   	     WAIT				      */
      /* -------------------------------------------------------------------- */

      case Wait_Cmic_Opr:

         /* Create a list of all the wait cmics for semantic checking. */

         NTR_IR_LIST_TBL(new_il_idx);

         IL_FLD(new_il_idx)		= IR_Tbl_Idx;
         IL_IDX(new_il_idx)		= ir_idx;
         IL_LINE_NUM(new_il_idx)	= IR_LINE_NUM(ir_idx);
         IL_COL_NUM(new_il_idx)		= IR_COL_NUM(ir_idx);

         if (IR_FLD_L(ir_idx) != NO_Tbl_Idx) {  /* POINT */
            COPY_OPND(opnd, IR_OPND_L(ir_idx));
            exp_desc.rank = 0;
            xref_state    = CIF_Symbol_Reference;
            ok = expr_semantics(&opnd, &exp_desc);

            if (exp_desc.type != Integer || exp_desc.rank != 0) {
               find_opnd_line_and_column(&opnd, &line, &column);
               PRINTMSG(line, 1431, Error, column, "POINT", "WAIT");
            }

            COPY_OPND(IR_OPND_L(ir_idx), opnd);
            null_point	= FALSE;
         }
         else {
            null_point	= TRUE;
         }

         COPY_OPND(opnd, IR_OPND_R(ir_idx));
         exp_desc.rank	= 0;
         xref_state	= CIF_Symbol_Reference;
         ok		= expr_semantics(&opnd, &exp_desc);

         if (exp_desc.type != Integer || exp_desc.rank != 0 ||
             OPND_FLD(opnd) != CN_Tbl_Idx) {
            find_opnd_line_and_column(&opnd, &line, &column);
            PRINTMSG(line, 1532, Error, column);
            IR_FLD_R(ir_idx) = CN_Tbl_Idx;
            IR_IDX_R(ir_idx) = CN_INTEGER_ONE_IDX;
         }
         else {
            COPY_OPND(IR_OPND_R(ir_idx), opnd);
         }

         if (cdir_switches.wait_list_idx == NULL_IDX) {

            /* This is the first WAIT - no checking necessary */

            cdir_switches.wait_list_idx = new_il_idx;
         }
         else {

            /* Check each point to make sure it is unique as we add to list */

            il_idx	= cdir_switches.wait_list_idx;

            while (il_idx != NULL_IDX) {
               prev_idx		= il_idx;
               old_ir_idx	= IL_IDX(il_idx);

               if (IR_FLD_L(old_ir_idx) == NO_Tbl_Idx) {

                  if (null_point) { /* They both are POINTless.  */
                     PRINTMSG(IR_LINE_NUM(ir_idx), 1521, Error, 
                              IR_COL_NUM(ir_idx));
                     ok	= FALSE;
                     break;
                  }
               }
               else if (IR_FLD_L(ir_idx) == CN_Tbl_Idx && 
                        IR_FLD_L(old_ir_idx) == CN_Tbl_Idx &&
                        fold_relationals(IR_IDX_L(ir_idx), 
                                         IR_IDX_L(old_ir_idx),
                                         Eq_Opr)) {

                  /* Issue message.  Same POINT value is not allowed. */

                  find_opnd_line_and_column(&(IR_OPND_L(ir_idx)), 
                                            &line, &column);
                  PRINTMSG(line, 1521, Error, column);
                  ok	= FALSE;
                  break;
               }
               il_idx = IL_NEXT_LIST_IDX(il_idx);
            }

            if (ok) {
               IL_NEXT_LIST_IDX(prev_idx) = new_il_idx;
            }
         }
         break;



      /* -------------------------------------------------------------------- */
      /*								      */
      /*            			 C$'s                                 */
      /*								      */
      /* -------------------------------------------------------------------- */

      /* -------------------------------------------------------------------- */
      /*         	   	     DOACROSS				      */
      /* -------------------------------------------------------------------- */

      case Doacross_Dollar_Opr:
         mp_directive_semantics(Doacross);
         break;

      /* -------------------------------------------------------------------- */
      /*         	   	     COPYIN  				      */
      /* -------------------------------------------------------------------- */

      case Copyin_Dollar_Opr:

         if (cdir_switches.doall_sh_idx != NULL_IDX ||
             cdir_switches.doacross_sh_idx != NULL_IDX ||
             cdir_switches.parallel_region ||
             cdir_switches.guard_in_par_reg) {

            PRINTMSG(IR_LINE_NUM(ir_idx), 1395, Error, IR_COL_NUM(ir_idx));
         }

         list_idx = IR_IDX_L(ir_idx);

         while (list_idx) {
            if (IL_FLD(list_idx) != SB_Tbl_Idx &&
                IL_FLD(list_idx) != NO_Tbl_Idx) {
               COPY_OPND(opnd, IL_OPND(list_idx));
               xref_state = CIF_Symbol_Reference;
               exp_desc.rank = 0;
               ok &= expr_semantics(&opnd, &exp_desc);
               COPY_OPND(IL_OPND(list_idx), opnd);

               find_opnd_line_and_column(&opnd, &line, &column);
               attr_idx = find_left_attr(&opnd);

               if (! exp_desc.reference ||
                   AT_OBJ_CLASS(attr_idx) != Data_Obj ||
                   ! ATD_IN_COMMON(attr_idx) ||
                   ATD_STOR_BLK_IDX(attr_idx) == NULL_IDX ||
                   SB_BLANK_COMMON(ATD_STOR_BLK_IDX(attr_idx))) {
                 /* error */

                 PRINTMSG(line, 1394, Error, column);
               }
            }
            else {
               /* common block */
            }
            list_idx = IL_NEXT_LIST_IDX(list_idx);
         }
         break;


      /* -------------------------------------------------------------------- */
      /*         	   	     DYNAMIC 				      */
      /* -------------------------------------------------------------------- */

      case Dynamic_Dollar_Opr:

         list_idx = IR_IDX_L(ir_idx);

         while (list_idx) {
            if (IL_FLD(list_idx) == AT_Tbl_Idx) {
               attr_idx = IL_IDX(list_idx);
               AT_LOCKED_IN(attr_idx) = TRUE;

               while (AT_ATTR_LINK(attr_idx)) {
                  attr_idx = AT_ATTR_LINK(attr_idx);
                  AT_LOCKED_IN(attr_idx) = TRUE;
               }

               if (AT_OBJ_CLASS(attr_idx) != Data_Obj ||
                   ATD_ARRAY_IDX(attr_idx) == NULL_IDX) {

                  find_opnd_line_and_column(&IL_OPND(list_idx), &line, &column);
                  PRINTMSG(line, 1396, Error, column, "C$DYNAMIC");
               }

               IL_IDX(list_idx) = attr_idx;
            }
            list_idx = IL_NEXT_LIST_IDX(list_idx);
         }
         break;

      /* -------------------------------------------------------------------- */
      /*         	   	     PAGE_PLACE		      */
      /* -------------------------------------------------------------------- */

      case Page_Place_Dollar_Opr:
         list_idx = IR_IDX_L(ir_idx);

         COPY_OPND(opnd, IL_OPND(list_idx));
         xref_state = CIF_Symbol_Reference;
         exp_desc.rank = 0;
         ok &= expr_semantics(&opnd, &exp_desc);
         COPY_OPND(IL_OPND(list_idx), opnd);

         /* must be a reference */

         list_idx = IL_NEXT_LIST_IDX(list_idx);

         COPY_OPND(opnd, IL_OPND(list_idx));
         xref_state = CIF_Symbol_Reference;
         exp_desc.rank = 0;
         ok &= expr_semantics(&opnd, &exp_desc);
         COPY_OPND(IL_OPND(list_idx), opnd);

         if (exp_desc.type != Integer ||
             exp_desc.rank != 0) {

            find_opnd_line_and_column(&opnd, &line, &column);
            PRINTMSG(line, 1397, Error, column);
         }

         list_idx = IL_NEXT_LIST_IDX(list_idx);

         COPY_OPND(opnd, IL_OPND(list_idx));
         xref_state = CIF_Symbol_Reference;
         exp_desc.rank = 0;
         ok &= expr_semantics(&opnd, &exp_desc);
         COPY_OPND(IL_OPND(list_idx), opnd);

         if (exp_desc.type != Integer ||
             exp_desc.rank != 0) {

            find_opnd_line_and_column(&opnd, &line, &column);
            PRINTMSG(line, 1397, Error, column);
         }

         break;

      /* -------------------------------------------------------------------- */
      /*         	   	     REDISTRIBUTE                             */
      /* -------------------------------------------------------------------- */

      case Redistribute_Dollar_Opr:
         attr_idx = IR_IDX_L(ir_idx);
         AT_LOCKED_IN(attr_idx) = TRUE;

         while (AT_ATTR_LINK(attr_idx)) {
            attr_idx = AT_ATTR_LINK(attr_idx);
            AT_LOCKED_IN(attr_idx) = TRUE;
         }

         if (AT_OBJ_CLASS(attr_idx) != Data_Obj ||
             ATD_ARRAY_IDX(attr_idx) == NULL_IDX) {

            find_opnd_line_and_column(&IR_OPND_L(ir_idx), &line, &column);
            PRINTMSG(line, 1396, Error, column, "C$REDISTRIBUTE");
         }

         IR_IDX_L(ir_idx) = attr_idx;

         list_idx = IL_IDX(IR_IDX_R(ir_idx));
 
         while (list_idx) {
            if (IL_FLD(list_idx) != NO_Tbl_Idx) {
               COPY_OPND(opnd, IL_OPND(list_idx));
               xref_state = CIF_Symbol_Reference;
               exp_desc.rank = 0;
               ok &= expr_semantics(&opnd, &exp_desc);
               COPY_OPND(IL_OPND(list_idx), opnd);

               if (exp_desc.type != Integer ||
                   exp_desc.rank != 0) {
      
                  find_opnd_line_and_column(&opnd, &line, &column);
                  PRINTMSG(line, 1397, Error, column);
               }
            }
            list_idx = IL_NEXT_LIST_IDX(list_idx);
         }

         list_idx = IL_NEXT_LIST_IDX(IR_IDX_R(ir_idx));

         if (list_idx) {
            list_idx = IL_IDX(list_idx);

            while(list_idx) {
               if (IL_FLD(list_idx) != NO_Tbl_Idx) {
                  COPY_OPND(opnd, IL_OPND(list_idx));
                  xref_state = CIF_Symbol_Reference;
                  exp_desc.rank = 0;
                  ok &= expr_semantics(&opnd, &exp_desc);
                  COPY_OPND(IL_OPND(list_idx), opnd);

                  find_opnd_line_and_column(&opnd, &line, &column);

                  if (OPND_FLD(opnd) != CN_Tbl_Idx ||
                      exp_desc.type != Integer) {
                     /* error, must be a constant */
                     PRINTMSG(line, 1368, Error, column);
                  }
                  else if (compare_cn_and_value(OPND_IDX(opnd),
                                                0,
                                                Lt_Opr)) {

                     /* error, must be greater than zero */
                     PRINTMSG(line, 1368, Error, column);
                  }
               }

               list_idx = IL_NEXT_LIST_IDX(list_idx);
            }
         }

         break;

      /* -------------------------------------------------------------------- */
      /*         	   	     PDO     				      */
      /* -------------------------------------------------------------------- */

      case Pdo_Par_Opr:
         mp_directive_semantics(Pdo);
         break;

      /* -------------------------------------------------------------------- */
      /*         	   	     PARALLEL DO			      */
      /* -------------------------------------------------------------------- */

      case Parallel_Do_Par_Opr:
         mp_directive_semantics(Parallel_Do);
         break;

      /* -------------------------------------------------------------------- */
      /*         	   	     PARALLEL				      */
      /* -------------------------------------------------------------------- */

      case Parallel_Par_Opr:
         mp_directive_semantics(Parallel);
         break;

      /* -------------------------------------------------------------------- */
      /*         	   	     PSECTION				      */
      /* -------------------------------------------------------------------- */

      case Psection_Par_Opr:
         mp_directive_semantics(Psection);
         break;

      /* -------------------------------------------------------------------- */
      /*         	   	     SINGLEPROCESS			      */
      /* -------------------------------------------------------------------- */

      case Singleprocess_Par_Opr:
         mp_directive_semantics(Singleprocess);
         break;

      case Section_Par_Opr:
         break;

      case End_Pdo_Par_Opr:
         end_blk_mp_semantics(FALSE);
         break;

      case End_Parallel_Par_Opr:
         end_blk_mp_semantics(FALSE);
         break;

      case Barrier_Par_Opr:
         break;

      case Critical_Section_Par_Opr:
         if (IR_FLD_L(ir_idx) != NO_Tbl_Idx) {
            COPY_OPND(opnd, IR_OPND_L(ir_idx));
            xref_state = CIF_Symbol_Reference;
            exp_desc.rank = 0;
            ok &= expr_semantics(&opnd, &exp_desc);
            COPY_OPND(IR_OPND_L(ir_idx), opnd);
         }
         break;

      case End_Critical_Section_Par_Opr:
         break;

      case End_Psection_Par_Opr:
         end_blk_mp_semantics(FALSE);
         break;

      case End_Singleprocess_Par_Opr:
         end_blk_mp_semantics(FALSE);
         break;

      /* -------------------------------------------------------------------- */
      /*								      */
      /*            			 C*$*'s                               */
      /*								      */
      /* -------------------------------------------------------------------- */


      case Blockingsize_Dir_Opr:
         if (IR_FLD_L(ir_idx) != NO_Tbl_Idx) {
            COPY_OPND(opnd, IR_OPND_L(ir_idx));
            xref_state = CIF_Symbol_Reference;
            exp_desc.rank = 0;
            ok &= expr_semantics(&opnd, &exp_desc);


            COPY_OPND(IR_OPND_L(ir_idx), opnd);

            
         }

         if (IR_FLD_R(ir_idx) != NO_Tbl_Idx) {
            COPY_OPND(opnd, IR_OPND_R(ir_idx));
            xref_state = CIF_Symbol_Reference;
            exp_desc.rank = 0;
            ok &= expr_semantics(&opnd, &exp_desc);
            COPY_OPND(IR_OPND_R(ir_idx), opnd);
         }
         break;

      case Assert_Star_Opr:
         ok = assert_semantics();
         break;

      case Fission_Star_Opr:
         if (IR_FLD_L(ir_idx) != NO_Tbl_Idx) {
            COPY_OPND(opnd, IR_OPND_L(ir_idx));
            xref_state = CIF_Symbol_Reference;
            exp_desc.rank = 0;
            ok &= expr_semantics(&opnd, &exp_desc);
            COPY_OPND(IR_OPND_L(ir_idx), opnd);
         }
         break;

      case Fuse_Star_Opr:
         if (IR_FLD_L(ir_idx) != NO_Tbl_Idx) {
            COPY_OPND(opnd, IR_OPND_L(ir_idx));
            xref_state = CIF_Symbol_Reference;
            exp_desc.rank = 0;
            ok &= expr_semantics(&opnd, &exp_desc);
            COPY_OPND(IR_OPND_L(ir_idx), opnd);
         }
         break;

      case Regionbegin_Star_Opr:
         break;

      case Regionend_Star_Opr:
         break;

      case Section_Nongp_Star_Opr:
      case Section_Gp_Star_Opr:
         list_idx = IR_IDX_L(ir_idx);

         while (list_idx != NULL_IDX) {

            if (IL_FLD(list_idx) == AT_Tbl_Idx) {

               if (ATD_IN_COMMON(IL_IDX(list_idx))) {
                  PRINTMSG(IL_LINE_NUM(list_idx), 1440, Error,
                           IL_COL_NUM(list_idx),
                           SB_BLANK_COMMON(ATD_STOR_BLK_IDX(IL_IDX(list_idx))) ?
                           "" : SB_NAME_PTR(ATD_STOR_BLK_IDX(IL_IDX(list_idx))),
                           AT_OBJ_NAME_PTR(IL_IDX(list_idx)),
                           (IR_OPR(ir_idx) == Section_Gp_Star_Opr) ?
                               "SECTION_GP": "SECTION_NON_GP");
               }
               else if (ATP_PGM_UNIT(SCP_ATTR_IDX(curr_scp_idx)) == Module) {
                  PRINTMSG(IL_LINE_NUM(list_idx), 1547, Error,
                           IL_COL_NUM(list_idx),
                           AT_OBJ_NAME_PTR(SCP_ATTR_IDX(curr_scp_idx)),
                           AT_OBJ_NAME_PTR(IL_IDX(list_idx)),
                           (IR_OPR(ir_idx) == Section_Gp_Star_Opr) ?
                               "SECTION_GP": "SECTION_NON_GP");
               }
               else if (ATD_STOR_BLK_IDX(IL_IDX(list_idx)) == NULL_IDX ||
                  (SB_BLK_TYPE(ATD_STOR_BLK_IDX(IL_IDX(list_idx))) != Static &&
                   SB_BLK_TYPE(ATD_STOR_BLK_IDX(IL_IDX(list_idx))) != 
                                                Static_Local &&
                   SB_BLK_TYPE(ATD_STOR_BLK_IDX(IL_IDX(list_idx))) != 
                                                Static_Named)) {

                  if (!AT_DCL_ERR(IL_IDX(list_idx))) {
                     PRINTMSG(IL_LINE_NUM(list_idx), 1497, Error,
                              IL_COL_NUM(list_idx),
                              AT_OBJ_NAME_PTR(IL_IDX(list_idx)),
                              (IR_OPR(ir_idx) == Section_Gp_Star_Opr) ?
                                  "SECTION_GP": "SECTION_NON_GP");
                  }
               }
            }
            else if (IL_FLD(list_idx) == SB_Tbl_Idx) {
               /* may need to do something with the storage block idx */

               if (IR_OPR(ir_idx) == Section_Gp_Star_Opr &&
                   SB_BLK_TYPE(IL_IDX(list_idx)) == Threadprivate) {
                  PRINTMSG(IL_LINE_NUM(list_idx), 1645, Error,
                           IL_COL_NUM(list_idx),
                           SB_NAME_PTR(IL_IDX(list_idx)));
               }
            }

            list_idx = IL_NEXT_LIST_IDX(list_idx);
         }
         break;

      case Unroll_Star_Opr:
         if (IR_FLD_L(ir_idx) != NO_Tbl_Idx) {
            COPY_OPND(opnd, IR_OPND_L(ir_idx));
            xref_state = CIF_Symbol_Reference;
            exp_desc.rank = 0;
            ok &= expr_semantics(&opnd, &exp_desc);
            COPY_OPND(IR_OPND_L(ir_idx), opnd);

#ifdef TARG_IA64
            if (OPND_FLD(opnd) != CN_Tbl_Idx ||
                exp_desc.rank  != 0          ||
                exp_desc.type  != Integer)   {
               find_opnd_line_and_column(&opnd, &line, &column);
               PRINTMSG(line, 1105, Error, column);
               IR_IDX_L(ir_idx) = CN_INTEGER_ONE_IDX;
               IR_FLD_L(ir_idx)	= CN_Tbl_Idx;
            }
            else if (fold_relationals(OPND_IDX(opnd),
                                      CN_INTEGER_ZERO_IDX,
                                      Eq_Opr)) {

               /* Directive is "UNROLL 0".  Force it to no unrolling,  */
               /* so send an unroll count of 1 to pdgcs!!              */

               IR_IDX_L(ir_idx)		= CN_INTEGER_ONE_IDX;
               IR_FLD_L(ir_idx)		= CN_Tbl_Idx;
            }
            else if (compare_cn_and_value(OPND_IDX(opnd), 0, Lt_Opr) ||
                     compare_cn_and_value(OPND_IDX(opnd), 1024, Gt_Opr)) {
               find_opnd_line_and_column(&opnd, &line, &column);
               PRINTMSG(line, 1105, Error, column);
               IR_IDX_L(ir_idx) = CN_INTEGER_ONE_IDX;
               IR_FLD_L(ir_idx)	= CN_Tbl_Idx;
            }

            cdir_switches.unroll_count_idx	= IR_IDX_L(ir_idx);
            cdir_switches.unroll_dir		= TRUE;
         }
         else {
            cdir_switches.unroll_count_idx	= CN_INTEGER_ZERO_IDX;
            cdir_switches.unroll_dir		= TRUE;
#ifdef KEY /* Bug 7489 */
	    IR_IDX_L(ir_idx)		= CN_INTEGER_ZERO_IDX;
	    IR_FLD_L(ir_idx)		= CN_Tbl_Idx;
#endif /* KEY Bug 7489 */
#endif /* TARG_IA64 */
         }
         break;

      case Prefetch_Manual_Star_Opr:
         if (IR_FLD_L(ir_idx) != CN_Tbl_Idx ||
             (compare_cn_and_value(IR_IDX_L(ir_idx),
                                   0,
                                   Ne_Opr) &&
              compare_cn_and_value(IR_IDX_L(ir_idx),
                                   1,
                                   Ne_Opr))) {

            find_opnd_line_and_column(&IR_OPND_L(ir_idx), &line, &column);
            PRINTMSG(line, 1378, Error, column, "PREFETCH_MANUAL");
         }
         break;

      case Prefetch_Ref_Star_Opr:
         prefetch_ref_semantics();
         break;

#ifdef KEY /* Bug 2660 */
      /* Syntax is c*$*options "somestring" */
      case Options_Dir_Opr:
         if (IR_FLD_L(ir_idx) != CN_Tbl_Idx) {
            find_opnd_line_and_column(&IR_OPND_L(ir_idx), &line, &column);
            PRINTMSG(line, 1378, Error, column, "OPTIONS");
	 }
         break;
#endif /* KEY Bug 2660 */

      case Prefetch_Star_Opr:
         if (IR_FLD_L(ir_idx) != CN_Tbl_Idx ||
             (compare_cn_and_value(IR_IDX_L(ir_idx),
                                   0,
                                   Ne_Opr) &&
              compare_cn_and_value(IR_IDX_L(ir_idx),
                                   1,
                                   Ne_Opr) &&
              compare_cn_and_value(IR_IDX_L(ir_idx),
                                   2,
                                   Ne_Opr))) {

            find_opnd_line_and_column(&IR_OPND_L(ir_idx), &line, &column);
            PRINTMSG(line, 1378, Error, column, "PREFETCH");
         }

         if (IR_FLD_R(ir_idx) != NO_Tbl_Idx) {
            if (IR_FLD_R(ir_idx) != CN_Tbl_Idx ||
                (compare_cn_and_value(IR_IDX_R(ir_idx),
                                      0,
                                      Ne_Opr) &&
                 compare_cn_and_value(IR_IDX_R(ir_idx),
                                      1,
                                      Ne_Opr) &&
                 compare_cn_and_value(IR_IDX_R(ir_idx),
                                      2,
                                      Ne_Opr))) {

               find_opnd_line_and_column(&IR_OPND_R(ir_idx), &line, &column);
               PRINTMSG(line, 1378, Error, column, "PREFETCH");
            }
         }
         else {
            IR_FLD_R(ir_idx) = CN_Tbl_Idx;
            IR_IDX_R(ir_idx) = CN_INTEGER_NEG_ONE_IDX;
            IR_LINE_NUM_R(ir_idx) = IR_LINE_NUM(ir_idx);
            IR_COL_NUM_R(ir_idx)  = IR_COL_NUM(ir_idx);;
         }

         break;

      case Prefetch_Ref_Disable_Star_Opr:
# ifdef _DEBUG
         if (IR_FLD_L(ir_idx) != AT_Tbl_Idx) {
            PRINTMSG(IR_LINE_NUM(ir_idx), 626, Internal, IR_COL_NUM(ir_idx),
                     "AT_Tbl_Idx", "directive_stmt_semantics");
         }
# endif
         attr_idx = IR_IDX_L(ir_idx);
         AT_LOCKED_IN(attr_idx) = TRUE;

         while (AT_ATTR_LINK(attr_idx)) {
            attr_idx = AT_ATTR_LINK(attr_idx);
            AT_LOCKED_IN(attr_idx) = TRUE;
         }

         IR_IDX_L(ir_idx) = attr_idx;

         if (AT_OBJ_CLASS(attr_idx) != Data_Obj ||
             ATD_ARRAY_IDX(attr_idx) == NULL_IDX) {

            find_opnd_line_and_column(&IR_OPND_L(ir_idx), &line, &column);
            PRINTMSG(line, 1382, Error, column);
         }

         if (IR_FLD_R(ir_idx) != NO_Tbl_Idx) {
            COPY_OPND(opnd, IR_OPND_R(ir_idx));
            xref_state = CIF_Symbol_Reference;
            exp_desc.rank = 0;
            ok &= expr_semantics(&opnd, &exp_desc);
            COPY_OPND(IR_OPND_R(ir_idx), opnd);

            if (OPND_FLD(opnd) != CN_Tbl_Idx) {
               find_opnd_line_and_column(&IR_OPND_R(ir_idx), &line, &column);
               PRINTMSG(line, 1383, Error, column, "PREFETCH_REF_DISABLE");
            }
         }
         break;

      case Align_Symbol_Star_Opr:
      case Fill_Symbol_Star_Opr:

# ifdef _DEBUG
         if (IR_FLD_L(ir_idx) != AT_Tbl_Idx && IR_FLD_L(ir_idx) != SB_Tbl_Idx) {
            PRINTMSG(IR_LINE_NUM(ir_idx), 626, Internal, IR_COL_NUM(ir_idx),
                     "AT_Tbl_Idx or SB_Tbl_Idx", "directive_stmt_semantics");
         }
# endif

         COPY_OPND(opnd, IR_OPND_R(ir_idx));

         xref_state	= CIF_Symbol_Reference;
         exp_desc.rank	= 0;
         ok	       &= expr_semantics(&opnd, &exp_desc);

         COPY_OPND(IR_OPND_R(ir_idx), opnd);

         if (OPND_FLD(opnd) != CN_Tbl_Idx ||
             TYP_TYPE(CN_TYPE_IDX(OPND_IDX(opnd))) != Integer ||
             (compare_cn_and_value(OPND_IDX(opnd),
                                   -1,
                                   Ne_Opr) &&
              compare_cn_and_value(OPND_IDX(opnd),
                                   -2,
                                   Ne_Opr) &&
              compare_cn_and_value(OPND_IDX(opnd),
                                   -3,
                                   Ne_Opr) &&
              ! power_o_two(OPND_IDX(opnd)))) {
            
            find_opnd_line_and_column(&opnd, &line, &column);
            PRINTMSG(line, 1386, Error, column,
                (IR_OPR(ir_idx) == Align_Symbol_Star_Opr ? 
                       "ALIGN_SYMBOL" : "FILL_SYMBOL"));
         }

         break;

      case Inline_Here_Star_Opr:

         if (IR_FLD_L(ir_idx) == NO_Tbl_Idx) {
            cdir_switches.inline_here_sgi = TRUE;
            cdir_switches.noinline_here_sgi = FALSE;

            if (cdir_switches.noinline_here_list_idx != NULL_IDX) {
               list_idx = cdir_switches.noinline_here_list_idx;
               cdir_switches.noinline_here_list_idx = NULL_IDX;

               while (list_idx) {

                  if (IL_FLD(list_idx) == AT_Tbl_Idx) {

                     if (AT_OBJ_CLASS(IL_IDX(list_idx)) == Pgm_Unit) {
                        ATP_SGI_LOCAL_NOINLINE(IL_IDX(list_idx)) = FALSE;
                     }
                     else if (AT_OBJ_CLASS(IL_IDX(list_idx)) == Interface) {
                        sn_idx = ATI_FIRST_SPECIFIC_IDX(IL_IDX(list_idx));

                        while (sn_idx != NULL_IDX) {

                           if (!AT_IS_INTRIN(SN_ATTR_IDX(sn_idx))) {
                              ATP_SGI_LOCAL_NOINLINE(SN_ATTR_IDX(sn_idx))=FALSE;
                           }
                           sn_idx = SN_SIBLING_LINK(sn_idx);
                        }
                     }
                  }
                  list_idx = IL_NEXT_LIST_IDX(list_idx);
               }
            }

            if (cdir_switches.inline_here_list_idx != NULL_IDX) {
               list_idx = cdir_switches.inline_here_list_idx;
               cdir_switches.inline_here_list_idx = NULL_IDX;

               while (list_idx) {

                  if (IL_FLD(list_idx) == AT_Tbl_Idx) {

                     if (AT_OBJ_CLASS(IL_IDX(list_idx)) == Pgm_Unit) {
                        ATP_SGI_LOCAL_INLINE(IL_IDX(list_idx)) = FALSE;
                     }
                     else if (AT_OBJ_CLASS(IL_IDX(list_idx)) == Interface) {
                        sn_idx = ATI_FIRST_SPECIFIC_IDX(IL_IDX(list_idx));

                        while (sn_idx != NULL_IDX) {

                           if (!AT_IS_INTRIN(SN_ATTR_IDX(sn_idx))) {
                              ATP_SGI_LOCAL_INLINE(SN_ATTR_IDX(sn_idx)) = FALSE;
                           }
                           sn_idx = SN_SIBLING_LINK(sn_idx);
                        }
                     }
                  }
                  list_idx = IL_NEXT_LIST_IDX(list_idx);
               }
            }
         }
         else {
            cdir_switches.inline_here_list_idx = IR_IDX_L(ir_idx);
            list_idx = IR_IDX_L(ir_idx);

            while (list_idx) {

               if (IL_FLD(list_idx) == AT_Tbl_Idx) {

                  if (AT_OBJ_CLASS(IL_IDX(list_idx)) == Pgm_Unit) {
                     ATP_SGI_LOCAL_INLINE(IL_IDX(list_idx)) = TRUE;
                     ATP_SGI_LOCAL_NOINLINE(IL_IDX(list_idx)) = FALSE;
                  }
                  else if (AT_OBJ_CLASS(IL_IDX(list_idx)) == Interface) {
                     sn_idx = ATI_FIRST_SPECIFIC_IDX(IL_IDX(list_idx));

                     while (sn_idx != NULL_IDX) {

                        if (!AT_IS_INTRIN(SN_ATTR_IDX(sn_idx))) {
                           ATP_SGI_LOCAL_INLINE(SN_ATTR_IDX(sn_idx)) = TRUE;
                           ATP_SGI_LOCAL_NOINLINE(SN_ATTR_IDX(sn_idx)) = FALSE;
                        }
                        sn_idx = SN_SIBLING_LINK(sn_idx);
                     }
                  }
               }
               list_idx = IL_NEXT_LIST_IDX(list_idx);
            }
         }
         break;

      case Noinline_Here_Star_Opr:

         if (IR_FLD_L(ir_idx) == NO_Tbl_Idx) {
            cdir_switches.noinline_here_sgi = TRUE;
            cdir_switches.inline_here_sgi = FALSE;

            if (cdir_switches.noinline_here_list_idx != NULL_IDX) {
               list_idx = cdir_switches.noinline_here_list_idx;
               cdir_switches.noinline_here_list_idx = NULL_IDX;

               while (list_idx) {

                  if (IL_FLD(list_idx) == AT_Tbl_Idx) {
                     
                     if (AT_OBJ_CLASS(IL_IDX(list_idx)) == Pgm_Unit) {
                        ATP_SGI_LOCAL_NOINLINE(IL_IDX(list_idx)) = FALSE;
                     }
                     else if (AT_OBJ_CLASS(IL_IDX(list_idx)) == Interface) {
                        sn_idx = ATI_FIRST_SPECIFIC_IDX(IL_IDX(list_idx));

                        while (sn_idx != NULL_IDX) {

                           if (!AT_IS_INTRIN(SN_ATTR_IDX(sn_idx))) {
                              ATP_SGI_LOCAL_NOINLINE(SN_ATTR_IDX(sn_idx))=FALSE;
                           }
                           sn_idx = SN_SIBLING_LINK(sn_idx);
                        }
                     }
                  }
                  list_idx = IL_NEXT_LIST_IDX(list_idx);
               }
            }

            if (cdir_switches.inline_here_list_idx != NULL_IDX) {
               list_idx = cdir_switches.inline_here_list_idx;
               cdir_switches.inline_here_list_idx = NULL_IDX;

               while (list_idx) {
                  if (IL_FLD(list_idx) == AT_Tbl_Idx) {

                     if (AT_OBJ_CLASS(IL_IDX(list_idx)) == Pgm_Unit) {
                        ATP_SGI_LOCAL_INLINE(IL_IDX(list_idx)) = FALSE;
                     }
                     else if (AT_OBJ_CLASS(IL_IDX(list_idx)) == Interface) {
                        sn_idx = ATI_FIRST_SPECIFIC_IDX(IL_IDX(list_idx));

                        while (sn_idx != NULL_IDX) {

                           if (!AT_IS_INTRIN(SN_ATTR_IDX(sn_idx))) {
                              ATP_SGI_LOCAL_INLINE(SN_ATTR_IDX(sn_idx)) = FALSE;
                           }
                           sn_idx = SN_SIBLING_LINK(sn_idx);
                        }
                     }
                  }
                  list_idx = IL_NEXT_LIST_IDX(list_idx);
               }
            }
         }
         else {
            cdir_switches.noinline_here_list_idx = IR_IDX_L(ir_idx);
            list_idx = IR_IDX_L(ir_idx);

            while (list_idx) {

               if (IL_FLD(list_idx) == AT_Tbl_Idx) {

                  if (AT_OBJ_CLASS(IL_IDX(list_idx)) == Pgm_Unit) {
                     ATP_SGI_LOCAL_NOINLINE(IL_IDX(list_idx)) = TRUE;
                     ATP_SGI_LOCAL_INLINE(IL_IDX(list_idx)) = FALSE;
                  }
                  else if (AT_OBJ_CLASS(IL_IDX(list_idx)) == Interface) {
                     sn_idx = ATI_FIRST_SPECIFIC_IDX(IL_IDX(list_idx));

                     while (sn_idx != NULL_IDX) {

                        if (!AT_IS_INTRIN(SN_ATTR_IDX(sn_idx))) {
                           ATP_SGI_LOCAL_NOINLINE(SN_ATTR_IDX(sn_idx)) = TRUE;
                           ATP_SGI_LOCAL_INLINE(SN_ATTR_IDX(sn_idx)) = FALSE;
                        }
                        sn_idx = SN_SIBLING_LINK(sn_idx);
                     }
                  }
               }
               list_idx = IL_NEXT_LIST_IDX(list_idx);
            }
         }
         break;

      case End_Inline_Here_Star_Opr:

         cdir_switches.noinline_here_sgi = FALSE;
         cdir_switches.inline_here_sgi = FALSE;

         if (cdir_switches.noinline_here_list_idx != NULL_IDX) {
            list_idx = cdir_switches.noinline_here_list_idx;
            cdir_switches.noinline_here_list_idx = NULL_IDX;

            while (list_idx) {

               if (IL_FLD(list_idx) == AT_Tbl_Idx) {

                  if (AT_OBJ_CLASS(IL_IDX(list_idx)) == Pgm_Unit) {
                     ATP_SGI_LOCAL_NOINLINE(IL_IDX(list_idx)) = FALSE;
                  }
                  else if (AT_OBJ_CLASS(IL_IDX(list_idx)) == Interface) {
                     sn_idx = ATI_FIRST_SPECIFIC_IDX(IL_IDX(list_idx));

                     while (sn_idx != NULL_IDX) {

                        if (!AT_IS_INTRIN(SN_ATTR_IDX(sn_idx))) {
                           ATP_SGI_LOCAL_NOINLINE(SN_ATTR_IDX(sn_idx)) = FALSE;
                        }
                        sn_idx = SN_SIBLING_LINK(sn_idx);
                     }
                  }
               }
               list_idx = IL_NEXT_LIST_IDX(list_idx);
            }
         }

         if (cdir_switches.inline_here_list_idx != NULL_IDX) {
            list_idx = cdir_switches.inline_here_list_idx;
            cdir_switches.inline_here_list_idx = NULL_IDX;

            while (list_idx) {

               if (IL_FLD(list_idx) == AT_Tbl_Idx) {

                  if (AT_OBJ_CLASS(IL_IDX(list_idx)) == Pgm_Unit) {
                     ATP_SGI_LOCAL_INLINE(IL_IDX(list_idx)) = FALSE;
                  }
                  else if (AT_OBJ_CLASS(IL_IDX(list_idx)) == Interface) {
                     sn_idx = ATI_FIRST_SPECIFIC_IDX(IL_IDX(list_idx));

                     while (sn_idx != NULL_IDX) {

                        if (!AT_IS_INTRIN(SN_ATTR_IDX(sn_idx))) {
                           ATP_SGI_LOCAL_INLINE(SN_ATTR_IDX(sn_idx)) = FALSE;
                        }
                        sn_idx = SN_SIBLING_LINK(sn_idx);
                     }
                  }
               }
               list_idx = IL_NEXT_LIST_IDX(list_idx);
            }
         }
         break;


      case Inline_Routine_Star_Opr:

         if (IR_FLD_L(ir_idx) == NO_Tbl_Idx) {
            SCP_INLINE_SGI(curr_scp_idx) = TRUE;
            SCP_NOINLINE_SGI(curr_scp_idx) = FALSE;
         }
         else {
            list_idx = IR_IDX_L(ir_idx);

            while (list_idx) {
               attr_idx = IL_IDX(list_idx);

               if (AT_OBJ_CLASS(attr_idx) == Pgm_Unit) {
                  ATP_SGI_ROUTINE_INLINE(attr_idx) = TRUE;
                  ATP_SGI_ROUTINE_NOINLINE(attr_idx) = FALSE;
               }   
               else if (AT_OBJ_CLASS(attr_idx) == Interface) {
                  sn_idx = ATI_FIRST_SPECIFIC_IDX(attr_idx);

                  while (sn_idx != NULL_IDX) {

                     if (!AT_IS_INTRIN(SN_ATTR_IDX(sn_idx))) {
                        ATP_SGI_ROUTINE_INLINE(SN_ATTR_IDX(sn_idx)) = TRUE;
                        ATP_SGI_ROUTINE_NOINLINE(SN_ATTR_IDX(sn_idx)) = FALSE;
                     }
                     sn_idx = SN_SIBLING_LINK(sn_idx);
                  }
               }
               list_idx = IL_NEXT_LIST_IDX(list_idx);
            }
         }
         break;

      case Noinline_Routine_Star_Opr:

         if (IR_FLD_L(ir_idx) == NO_Tbl_Idx) {
            SCP_NOINLINE_SGI(curr_scp_idx) = TRUE;
            SCP_INLINE_SGI(curr_scp_idx) = FALSE;
         }
         else {
            list_idx = IR_IDX_L(ir_idx);

            while (list_idx) {
               attr_idx = IL_IDX(list_idx);

               if (AT_OBJ_CLASS(attr_idx) == Pgm_Unit) {
                  ATP_SGI_ROUTINE_NOINLINE(attr_idx) = TRUE;
                  ATP_SGI_ROUTINE_INLINE(attr_idx) = FALSE;
               }
               else if (AT_OBJ_CLASS(attr_idx) == Interface) {
                  sn_idx = ATI_FIRST_SPECIFIC_IDX(attr_idx);

                  while (sn_idx != NULL_IDX) {

                     if (!AT_IS_INTRIN(SN_ATTR_IDX(sn_idx))) {
                        ATP_SGI_ROUTINE_NOINLINE(SN_ATTR_IDX(sn_idx)) = TRUE;
                        ATP_SGI_ROUTINE_INLINE(SN_ATTR_IDX(sn_idx)) = FALSE;
                     }
                     sn_idx = SN_SIBLING_LINK(sn_idx);
                  }
               }

               list_idx = IL_NEXT_LIST_IDX(list_idx);
            }
         } 
         break;

      case Inline_Global_Star_Opr:

         if (IR_FLD_L(ir_idx) == NO_Tbl_Idx) {
            inline_global_sgi = TRUE;
            noinline_global_sgi = FALSE;
         }
         else {
            list_idx = IR_IDX_L(ir_idx);
            while (list_idx) {
               attr_idx = IL_IDX(list_idx);

               if (srch_global_name_tbl(AT_OBJ_NAME_PTR(attr_idx),
                                        AT_NAME_LEN(attr_idx),
                                       &name_idx)) {

               }
               else {
                  ntr_global_name_tbl(attr_idx, NULL_IDX, name_idx);
                  GA_OBJ_CLASS(GN_ATTR_IDX(name_idx)) = Pgm_Unit;
                  GAP_GLOBAL_DIR(GN_ATTR_IDX(name_idx))	= TRUE;
               }

               GAP_INLINE_STATE(GN_ATTR_IDX(name_idx))	= Inline_Sgi;

               /* clear any routine dirs we've seen so far */

               ATP_SGI_ROUTINE_INLINE(attr_idx) = FALSE;
               ATP_SGI_ROUTINE_NOINLINE(attr_idx) = FALSE;

               ATP_SGI_GLOBAL_INLINE(attr_idx) = TRUE;
               ATP_SGI_GLOBAL_NOINLINE(attr_idx) = FALSE;

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

               list_idx = IL_NEXT_LIST_IDX(list_idx);
            }
         }

         if (! SH_COMPILER_GEN(curr_stmt_sh_idx)) {
            gen_gl_sh(After, Directive_Stmt, line, column,
                      FALSE, FALSE, TRUE);
            GL_SH_IR_IDX(curr_gl_stmt_sh_idx) = copy_to_gl_subtree(ir_idx,
                                                                   IR_Tbl_Idx);
         }
         break;

      case Noinline_Global_Star_Opr:

         if (IR_FLD_L(ir_idx) == NO_Tbl_Idx) {
            noinline_global_sgi = TRUE;
            inline_global_sgi = FALSE;
         }
         else {
            list_idx = IR_IDX_L(ir_idx);
            while (list_idx) {
               attr_idx = IL_IDX(list_idx);

               if (srch_global_name_tbl(AT_OBJ_NAME_PTR(attr_idx),
                                        AT_NAME_LEN(attr_idx),
                                       &name_idx)) {

               }
               else {
                  ntr_global_name_tbl(attr_idx, NULL_IDX, name_idx);
                  GAP_GLOBAL_DIR(GN_ATTR_IDX(name_idx))	= TRUE;
               }

               GAP_INLINE_STATE(GN_ATTR_IDX(name_idx))	= Noinline_Sgi;

               /* clear any routine dirs we've seen so far */

               ATP_SGI_ROUTINE_INLINE(attr_idx) = FALSE;
               ATP_SGI_ROUTINE_NOINLINE(attr_idx) = FALSE;

               ATP_SGI_GLOBAL_NOINLINE(attr_idx) = TRUE;
               ATP_SGI_GLOBAL_INLINE(attr_idx) = FALSE;

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

               list_idx = IL_NEXT_LIST_IDX(list_idx);
            }
         }

         if (! SH_COMPILER_GEN(curr_stmt_sh_idx)) {
            gen_gl_sh(After, Directive_Stmt, line, column,
                      FALSE, FALSE, TRUE);
            GL_SH_IR_IDX(curr_gl_stmt_sh_idx) = copy_to_gl_subtree(ir_idx,
                                                                   IR_Tbl_Idx);
         }
         break;


      case Atomic_Open_Mp_Opr:
         break;

      case Barrier_Open_Mp_Opr:
         break;

      case Critical_Open_Mp_Opr:
         break;

      case Do_Open_Mp_Opr:
#ifdef KEY /* Bug 4889 */
         inside_paralleldo = curr_stmt_sh_idx;
#endif /* KEY Bug 4889 */
         open_mp_directive_semantics(Do_Omp);
         break;

      case Endcritical_Open_Mp_Opr:
         break;

      case Enddo_Open_Mp_Opr:
#ifdef KEY /* Bug 4889 */
         inside_paralleldo = NULL_IDX;
#endif /* KEY Bug 4889 */
         end_blk_mp_semantics(TRUE);
         break;

      case Endparallel_Open_Mp_Opr:
#ifdef KEY /* Bug 4889 */
	 inside_parallel = NULL_IDX;
#endif /* KEY Bug 4889 */
         end_blk_mp_semantics(TRUE);
         break;

      case Endparalleldo_Open_Mp_Opr:
#ifdef KEY /* Bug 4889 */
	 inside_paralleldo = NULL_IDX;
#endif /* KEY Bug 4889 */
         end_blk_mp_semantics(TRUE);
         break;

      case Endparallelsections_Open_Mp_Opr:
         end_blk_mp_semantics(TRUE);
         break;

      case Endmaster_Open_Mp_Opr:
         break;

      case Endordered_Open_Mp_Opr:
         break;

      case Endsections_Open_Mp_Opr:
         end_blk_mp_semantics(TRUE);
         break;

      case Endsingle_Open_Mp_Opr:
      	  //open_mp_copyprivate_semantics(); /* by jhs, 02/7/22 */
         end_blk_mp_semantics(TRUE);
#ifdef KEY /* Bug 10441 */
         cdir_switches.single = FALSE;
#endif /* KEY Bug 10441 */
         break;

      case Flush_Open_Mp_Opr:
         list_idx = IR_IDX_L(ir_idx);

         while (list_idx != NULL_IDX) {
            if (IL_FLD(list_idx) == AT_Tbl_Idx) {
               attr_idx = IL_IDX(list_idx);
               AT_LOCKED_IN(attr_idx) = TRUE;

               while (AT_ATTR_LINK(attr_idx)) {
                  attr_idx = AT_ATTR_LINK(attr_idx);
                  AT_LOCKED_IN(attr_idx) = TRUE;
               }

               if (AT_OBJ_CLASS(attr_idx) != Data_Obj) {
                  PRINTMSG(IL_LINE_NUM(list_idx), 1480, Error,
                           IL_COL_NUM(list_idx));
               }

               IL_IDX(list_idx) = attr_idx;
            }

            list_idx = IL_NEXT_LIST_IDX(list_idx);
         }
         break;

      case Master_Open_Mp_Opr:
         break;

      case Ordered_Open_Mp_Opr:
         break;

      case Parallel_Open_Mp_Opr:
#ifdef KEY /* Bug 4889 */
	 inside_parallel = curr_stmt_sh_idx;
#endif /* KEY Bug 4889 */
         open_mp_directive_semantics(Parallel_Omp);
         break;

      case Paralleldo_Open_Mp_Opr:
#ifdef KEY /* Bug 4889 */
	 inside_paralleldo = curr_stmt_sh_idx;
#endif /* KEY Bug 4889 */
         open_mp_directive_semantics(Parallel_Do_Omp);
         break;

      case Parallelsections_Open_Mp_Opr:
         open_mp_directive_semantics(Parallel_Sections_Omp);
         break;

      case Section_Open_Mp_Opr:
         break;

      case Sections_Open_Mp_Opr:
         open_mp_directive_semantics(Sections_Omp);
         break;

      case Single_Open_Mp_Opr:
#ifdef KEY /* Bug 10441 */
         cdir_switches.single = TRUE;
#endif /* KEY Bug 10441 */
         open_mp_directive_semantics(Single_Omp);
         break;
         
      case  Parallelworkshare_Open_Mp_Opr:
        open_mp_directive_semantics(Parallel_Workshare_Omp);
        break;
        
      case Endparallelworkshare_Open_Mp_Opr:
        end_blk_mp_semantics(TRUE);
        break;

   }

   TRACE (Func_Exit, "directive_stmt_semantics", NULL);

   return;

}  /* directive_stmt_semantics */

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

static void doall_cmic_semantics(void)

{
   int			attr_idx;
   int			column;
   expr_arg_type	exp_desc;
   int			getfirst_list_idx;
   int			idx;
   int			ir_idx;
   int			line;
   int			list_idx;
   int			list2_idx;
   int			list3_idx;
   opnd_type		l_opnd;
   opnd_type		opnd;
   int			private_list_idx;
   int			save_curr_stmt_sh_idx;
   int			shared_list_idx;
   long64		value;

# if (defined(_TARGET_OS_IRIX) || defined(_TARGET_OS_LINUX) || defined(_TARGET_OS_DARWIN))
   int			max_idx;
   opnd_type		opnd2;
   char			string[13];
# endif


   TRACE (Func_Entry, "doall_cmic_semantics", NULL);

   ir_idx = SH_IR_IDX(curr_stmt_sh_idx);

   if (cdir_switches.doall_sh_idx != NULL_IDX ||
       cdir_switches.doacross_sh_idx != NULL_IDX ||
       cdir_switches.parallel_region ||
       cdir_switches.guard_in_par_reg) {

      /* error .. already in a parallel_region */
      PRINTMSG(IR_LINE_NUM(ir_idx), 814, Error, IR_COL_NUM(ir_idx));
   }

   cdir_switches.doall_sh_idx = curr_stmt_sh_idx;

   /* pull stmt header out of list */
   remove_sh(curr_stmt_sh_idx);
   save_curr_stmt_sh_idx = SH_PREV_IDX(curr_stmt_sh_idx);

   SH_PREV_IDX(cdir_switches.doall_sh_idx) = NULL_IDX;
   SH_NEXT_IDX(cdir_switches.doall_sh_idx) = NULL_IDX;

   list_idx = IR_IDX_L(ir_idx);

   /* process if condition */

   if (IL_FLD(list_idx) != NO_Tbl_Idx) {
     COPY_OPND(opnd, IL_OPND(list_idx));
     exp_desc.rank = 0;
     xref_state = CIF_Symbol_Reference;
     expr_semantics(&opnd, &exp_desc);

     find_opnd_line_and_column(&opnd, &line, &column);
     if (exp_desc.type != Logical ||
         exp_desc.rank != 0)      {
        PRINTMSG(line, 803, Error, column);
     }

     IL_FLD(list_idx) = AT_Tbl_Idx;
     idx = create_tmp_asg(&opnd,
                          &exp_desc,
                          &l_opnd,
                          Intent_In,
                          FALSE,
                          FALSE);
     IL_IDX(list_idx) = idx;
     IL_LINE_NUM(list_idx) = line;
     IL_COL_NUM(list_idx) = column;
  }

  /* process SHARED var list */

  list_idx = IL_NEXT_LIST_IDX(list_idx);
  cdir_switches.shared_list_idx = list_idx;

  if (IL_FLD(list_idx) != NO_Tbl_Idx) {

     list2_idx = IL_IDX(list_idx);

     while (list2_idx) {

        attr_idx = IL_IDX(list2_idx);
        AT_LOCKED_IN(attr_idx) = TRUE;

        while (AT_ATTR_LINK(attr_idx)) {
           attr_idx = AT_ATTR_LINK(attr_idx);
           AT_LOCKED_IN(attr_idx) = TRUE;
        }

        IL_IDX(list2_idx) = attr_idx;

        if (AT_OBJ_CLASS(attr_idx) == Pgm_Unit   &&
            ATP_PROC(attr_idx)     == Dummy_Proc) {
           ATP_TASK_SHARED(attr_idx) = TRUE;
        }
        else if (AT_OBJ_CLASS(attr_idx) != Data_Obj ||
                 ATD_CLASS(attr_idx)    == Constant) {
           PRINTMSG(IL_LINE_NUM(list2_idx), 804, Caution,
                    IL_COL_NUM(list2_idx),
                    AT_OBJ_NAME_PTR(attr_idx),
                    "SHARED", "DO ALL");

           /* remove the attr from the list */

           if (list2_idx == IL_IDX(cdir_switches.shared_list_idx)) {

               /* head of the list */

              list2_idx = IL_NEXT_LIST_IDX(list2_idx);
              IL_IDX(cdir_switches.shared_list_idx) = list2_idx;
              IL_IDX(list_idx) = list2_idx;
              IL_PREV_LIST_IDX(list2_idx) = NULL_IDX;
              IL_LIST_CNT(list_idx)--;
              continue;
           }
           else {
              IL_NEXT_LIST_IDX(IL_PREV_LIST_IDX(list2_idx)) =
                        IL_NEXT_LIST_IDX(list2_idx);
              IL_PREV_LIST_IDX(IL_NEXT_LIST_IDX(list2_idx)) =
                        IL_PREV_LIST_IDX(list2_idx);

              list2_idx = IL_NEXT_LIST_IDX(list2_idx);
              IL_LIST_CNT(list_idx)--;
              continue;
           }
        }
        else {
           ATD_TASK_SHARED(attr_idx) = TRUE;
           ATD_WAS_SCOPED(attr_idx) = TRUE;
        }

        shared_list_idx = IL_IDX(cdir_switches.shared_list_idx);

        while (shared_list_idx != list2_idx &&
               shared_list_idx != NULL_IDX) {

           if (attr_idx == IL_IDX(shared_list_idx)) {

               /* take this out of the list */

              IL_NEXT_LIST_IDX(IL_PREV_LIST_IDX(list2_idx)) =
                     IL_NEXT_LIST_IDX(list2_idx);
              IL_PREV_LIST_IDX(IL_NEXT_LIST_IDX(list2_idx)) =
                     IL_PREV_LIST_IDX(list2_idx);

              list2_idx = IL_PREV_LIST_IDX(list2_idx);
              IL_LIST_CNT(list_idx)--;
              break;
           }
           shared_list_idx = IL_NEXT_LIST_IDX(shared_list_idx);
        }

        list2_idx = IL_NEXT_LIST_IDX(list2_idx);
     }
  }

  /* process PRIVATE var list */

  list_idx = IL_NEXT_LIST_IDX(list_idx);
  cdir_switches.private_list_idx = list_idx;

  if (IL_FLD(list_idx) != NO_Tbl_Idx) {

     list2_idx = IL_IDX(list_idx);

     while (list2_idx) {

        attr_idx = IL_IDX(list2_idx);
        AT_LOCKED_IN(attr_idx) = TRUE;

        while (AT_ATTR_LINK(attr_idx)) {
           attr_idx = AT_ATTR_LINK(attr_idx);
           AT_LOCKED_IN(attr_idx) = TRUE;
        }

        IL_IDX(list2_idx) = attr_idx;

# if (defined(_TARGET_OS_IRIX) || defined(_TARGET_OS_LINUX) || defined(_TARGET_OS_DARWIN))
        if (AT_OBJ_CLASS(attr_idx) == Data_Obj &&
            (ATD_ALLOCATABLE(attr_idx) ||
             ATD_CLASS(attr_idx) == CRI__Pointee ||
             ATD_POINTER(attr_idx))) {

           if (ATD_ALLOCATABLE(attr_idx)) {
              strcpy(string, "ALLOCATABLE");
           }
           else if (ATD_POINTER(attr_idx)) {
              strcpy(string, "POINTER");
           }
           else {
              strcpy(string, "Cray Pointee");
           }

           PRINTMSG(IL_LINE_NUM(list2_idx), 1446, Error,
                    IL_COL_NUM(list2_idx),
                    string,
                    AT_OBJ_NAME_PTR(attr_idx),
                    "DOALL");

        }
        else
# endif
        if (AT_OBJ_CLASS(attr_idx) != Data_Obj ||
            ATD_CLASS(attr_idx)    == Constant) {
           PRINTMSG(IL_LINE_NUM(list2_idx), 804, Caution,
                    IL_COL_NUM(list2_idx),
                    AT_OBJ_NAME_PTR(attr_idx),
                    "PRIVATE", "DO ALL");

           /* remove the attr from the list */

           if (list2_idx == IL_IDX(cdir_switches.private_list_idx)) {

              /* head of the list */

              list2_idx = IL_NEXT_LIST_IDX(list2_idx);
              IL_IDX(cdir_switches.private_list_idx) = list2_idx;
              IL_IDX(list_idx) = list2_idx;
              IL_PREV_LIST_IDX(list2_idx) = NULL_IDX;
              IL_LIST_CNT(list_idx)--;
              continue;
           }
           else {
              IL_NEXT_LIST_IDX(IL_PREV_LIST_IDX(list2_idx)) =
                        IL_NEXT_LIST_IDX(list2_idx);
              IL_PREV_LIST_IDX(IL_NEXT_LIST_IDX(list2_idx)) =
                        IL_PREV_LIST_IDX(list2_idx);

              list2_idx = IL_NEXT_LIST_IDX(list2_idx);
              IL_LIST_CNT(list_idx)--;
              continue;
           }
        }
        else {
           ATD_TASK_PRIVATE(attr_idx) = TRUE;
           ATD_WAS_SCOPED(attr_idx) = TRUE;

           if (ATD_CLASS(attr_idx) == Variable &&
               ATD_AUTOMATIC(attr_idx) &&
               ATD_AUTO_BASE_IDX(attr_idx) != NULL_IDX &&
               ! ATD_TASK_PRIVATE(ATD_AUTO_BASE_IDX(attr_idx))) {

              ATD_TASK_PRIVATE(ATD_AUTO_BASE_IDX(attr_idx)) = TRUE;

              NTR_IR_LIST_TBL(list3_idx);
              IL_PREV_LIST_IDX(IL_IDX(list_idx)) = list3_idx;
              IL_NEXT_LIST_IDX(list3_idx) = IL_IDX(list_idx);
              IL_IDX(list_idx) = list3_idx;
              IL_LIST_CNT(list_idx)++;

              IL_FLD(list3_idx) = AT_Tbl_Idx;
              IL_IDX(list3_idx) = ATD_AUTO_BASE_IDX(attr_idx);
              IL_LINE_NUM(list3_idx) = IL_LINE_NUM(list2_idx);
              IL_COL_NUM(list3_idx) = IL_COL_NUM(list2_idx);
           }
        }

        private_list_idx = IL_IDX(cdir_switches.private_list_idx);

        while (private_list_idx != list2_idx &&
               private_list_idx != NULL_IDX) {

           if (attr_idx == IL_IDX(private_list_idx)) {

              /* take this out of the list */

              IL_NEXT_LIST_IDX(IL_PREV_LIST_IDX(list2_idx)) =
                     IL_NEXT_LIST_IDX(list2_idx);
              IL_PREV_LIST_IDX(IL_NEXT_LIST_IDX(list2_idx)) =
                     IL_PREV_LIST_IDX(list2_idx);

              list2_idx = IL_PREV_LIST_IDX(list2_idx);
              IL_LIST_CNT(list_idx)--;
              goto CONTINUE;
           }
           private_list_idx = IL_NEXT_LIST_IDX(private_list_idx);
        }


        shared_list_idx = IL_IDX(cdir_switches.shared_list_idx);

        while (shared_list_idx) {

           if (attr_idx == IL_IDX(shared_list_idx)) {

              /* error, cannot have var in shared and private */

              PRINTMSG(IL_LINE_NUM(list2_idx), 805, Error,
                       IL_COL_NUM(list2_idx),
                       AT_OBJ_NAME_PTR(attr_idx));
              break;
           }
           shared_list_idx = IL_NEXT_LIST_IDX(shared_list_idx);
        }

CONTINUE:
        list2_idx = IL_NEXT_LIST_IDX(list2_idx);
     }
  }

  /* process GETFIRST var list */

  list_idx = IL_NEXT_LIST_IDX(list_idx);
  cdir_switches.getfirst_list_idx = list_idx;

  if (IL_FLD(list_idx) != NO_Tbl_Idx) {

     list2_idx = IL_IDX(list_idx);

     while (list2_idx) {

        attr_idx = IL_IDX(list2_idx);
        AT_LOCKED_IN(attr_idx) = TRUE;

        while (AT_ATTR_LINK(attr_idx)) {
           attr_idx = AT_ATTR_LINK(attr_idx);
           AT_LOCKED_IN(attr_idx) = TRUE;
        }

        IL_IDX(list2_idx) = attr_idx;

        if (AT_OBJ_CLASS(attr_idx) != Data_Obj ||
            ATD_CLASS(attr_idx)    == Constant) {
           PRINTMSG(IL_LINE_NUM(list2_idx), 804, Caution,
                    IL_COL_NUM(list2_idx),
                    AT_OBJ_NAME_PTR(attr_idx),
                    "GETFIRST", "DO ALL");

           /* remove the attr from the list */

           if (list2_idx == IL_IDX(cdir_switches.getfirst_list_idx)) {

              /* head of the list */

              list2_idx = IL_NEXT_LIST_IDX(list2_idx);
              IL_IDX(cdir_switches.getfirst_list_idx) = list2_idx;
              IL_IDX(list_idx) = list2_idx;
              IL_PREV_LIST_IDX(list2_idx) = NULL_IDX;
              IL_LIST_CNT(list_idx)--;
              continue;
           }
           else {
              IL_NEXT_LIST_IDX(IL_PREV_LIST_IDX(list2_idx)) =
                        IL_NEXT_LIST_IDX(list2_idx);
              IL_PREV_LIST_IDX(IL_NEXT_LIST_IDX(list2_idx)) =
                        IL_PREV_LIST_IDX(list2_idx);

              list2_idx = IL_NEXT_LIST_IDX(list2_idx);
              IL_LIST_CNT(list_idx)--;
              continue;
           }
        }
        else {
           ATD_TASK_GETFIRST(attr_idx) = TRUE;

           if (ATD_CLASS(attr_idx) == Variable &&
               ATD_AUTOMATIC(attr_idx) &&
               ATD_AUTO_BASE_IDX(attr_idx) != NULL_IDX &&
               ! ATD_TASK_PRIVATE(ATD_AUTO_BASE_IDX(attr_idx))) {

              ATD_TASK_GETFIRST(ATD_AUTO_BASE_IDX(attr_idx)) = TRUE;

              NTR_IR_LIST_TBL(list3_idx);
              IL_PREV_LIST_IDX(IL_IDX(list_idx)) = list3_idx;
              IL_NEXT_LIST_IDX(list3_idx) = IL_IDX(list_idx);
              IL_IDX(list_idx) = list3_idx;
              IL_LIST_CNT(list_idx)++;

              IL_FLD(list3_idx) = AT_Tbl_Idx;
              IL_IDX(list3_idx) = ATD_AUTO_BASE_IDX(attr_idx);
              IL_LINE_NUM(list3_idx) = IL_LINE_NUM(list2_idx);
              IL_COL_NUM(list3_idx) = IL_COL_NUM(list2_idx);
           }
        }

        getfirst_list_idx = IL_IDX(cdir_switches.getfirst_list_idx);

        while (getfirst_list_idx != list2_idx &&
               getfirst_list_idx != NULL_IDX) {

           if (attr_idx == IL_IDX(getfirst_list_idx)) {

              /* take this out of the list */

              IL_NEXT_LIST_IDX(IL_PREV_LIST_IDX(list2_idx)) =
                     IL_NEXT_LIST_IDX(list2_idx);
              IL_PREV_LIST_IDX(IL_NEXT_LIST_IDX(list2_idx)) =
                     IL_PREV_LIST_IDX(list2_idx);

              list2_idx = IL_PREV_LIST_IDX(list2_idx);
              IL_LIST_CNT(list_idx)--;
              goto CONTINUE2;
           }
           getfirst_list_idx = IL_NEXT_LIST_IDX(getfirst_list_idx);
        }


        shared_list_idx = IL_IDX(cdir_switches.shared_list_idx);

        while (shared_list_idx) {

           if (attr_idx == IL_IDX(shared_list_idx)) {

              /* error, cannot have var in shared and getfirst */

              PRINTMSG(IL_LINE_NUM(list2_idx), 1314, Error,
                       IL_COL_NUM(list2_idx),
                       AT_OBJ_NAME_PTR(attr_idx),
                       "SHARED", "GETFIRST");
              break;
           }
           shared_list_idx = IL_NEXT_LIST_IDX(shared_list_idx);
        }

        private_list_idx = IL_IDX(cdir_switches.private_list_idx);

        while (private_list_idx) {

           if (attr_idx == IL_IDX(private_list_idx)) {

              /* error, cannot have var in private and getfirst */

              PRINTMSG(IL_LINE_NUM(list2_idx), 1314, Error,
                       IL_COL_NUM(list2_idx),
                       AT_OBJ_NAME_PTR(attr_idx),
                       "PRIVATE", "GETFIRST");
              break;
           }
           private_list_idx = IL_NEXT_LIST_IDX(private_list_idx);
        }


CONTINUE2:
        list2_idx = IL_NEXT_LIST_IDX(list2_idx);
     }
  }


   /* AUTOSCOPE */

   list_idx = IL_NEXT_LIST_IDX(list_idx);

   if (IL_FLD(list_idx) == CN_Tbl_Idx) {
      cdir_switches.autoscope = TRUE;
   }

   /* process CONTROL var list */

   list_idx = IL_NEXT_LIST_IDX(list_idx);

   if (IL_FLD(list_idx) != NO_Tbl_Idx) {

      list2_idx = IL_IDX(list_idx);

      while (list2_idx) {

         attr_idx = IL_IDX(list2_idx);
         AT_LOCKED_IN(attr_idx) = TRUE;

         while (AT_ATTR_LINK(attr_idx)) {
            attr_idx = AT_ATTR_LINK(attr_idx);
            AT_LOCKED_IN(attr_idx) = TRUE;
         }

         IL_IDX(list2_idx) = attr_idx;

         if (AT_OBJ_CLASS(attr_idx) != Data_Obj ||
             ATD_CLASS(attr_idx)    == Constant) {
            PRINTMSG(IL_LINE_NUM(list2_idx), 804, Caution,
                     IL_COL_NUM(list2_idx),
                     AT_OBJ_NAME_PTR(attr_idx),
                     "CONTROL", "DO ALL");

            /* remove the attr from the list */
            if (list2_idx == IL_IDX(list_idx)) {
               /* head of the list */
               list2_idx = IL_NEXT_LIST_IDX(list2_idx);
               IL_IDX(list_idx) = list2_idx;
               IL_PREV_LIST_IDX(list2_idx) = NULL_IDX;
               IL_LIST_CNT(list_idx)--;
               continue;
            }
            else {
               IL_NEXT_LIST_IDX(IL_PREV_LIST_IDX(list2_idx)) =
                         IL_NEXT_LIST_IDX(list2_idx);
               IL_PREV_LIST_IDX(IL_NEXT_LIST_IDX(list2_idx)) =
                         IL_PREV_LIST_IDX(list2_idx);

               list2_idx = IL_NEXT_LIST_IDX(list2_idx);
               IL_LIST_CNT(list_idx)--;
               continue;
            }
         }

         list2_idx = IL_NEXT_LIST_IDX(list2_idx);
      }
   }

   /* skip SAVELAST */

   list_idx = IL_NEXT_LIST_IDX(list_idx);

   /* process MAXCPUS value */

   list_idx = IL_NEXT_LIST_IDX(list_idx);

   if (IL_FLD(list_idx) != NO_Tbl_Idx) {
      COPY_OPND(opnd, IL_OPND(list_idx));
      exp_desc.rank = 0;
      xref_state = CIF_Symbol_Reference;
      expr_semantics(&opnd, &exp_desc);

      find_opnd_line_and_column(&opnd, &line, &column);

      if (exp_desc.type != Integer ||
          exp_desc.rank != 0)      {
         PRINTMSG(line, 806, Error, column);
      }

      IL_FLD(list_idx) = AT_Tbl_Idx;
      idx = create_tmp_asg(&opnd,
                           &exp_desc,
                           &l_opnd,
                           Intent_In,
                           FALSE,
                           FALSE);
      IL_IDX(list_idx) = idx;
      IL_LINE_NUM(list_idx) = line;
      IL_COL_NUM(list_idx) = column;
   }
   else if (cdir_switches.maxcpus) {
      COPY_OPND(IL_OPND(list_idx), cdir_switches.maxcpus_opnd);
      cdir_switches.maxcpus      = FALSE;
   }

   /* skip WORK DISTRIBUTION */

   list_idx = IL_NEXT_LIST_IDX(list_idx);

   /* process work distribution expression */

   list_idx = IL_NEXT_LIST_IDX(list_idx);

   if (IL_FLD(list_idx) != NO_Tbl_Idx) {
      COPY_OPND(opnd, IL_OPND(list_idx));
      exp_desc.rank = 0;
      xref_state = CIF_Symbol_Reference;
      expr_semantics(&opnd, &exp_desc);

      find_opnd_line_and_column(&opnd, &line, &column);

      value = (IL_FLD(IL_PREV_LIST_IDX(list_idx)) != CN_Tbl_Idx) ? 0 :
                  CN_INT_TO_C(IL_IDX(IL_PREV_LIST_IDX(list_idx)));

      if (exp_desc.type != Integer || exp_desc.rank != 0)      {
         PRINTMSG(line, 806, Error, column);
      }
      else if (OPND_FLD(opnd) == CN_Tbl_Idx &&
               IL_FLD(IL_PREV_LIST_IDX(list_idx)) == CN_Tbl_Idx &&
               compare_cn_and_value(OPND_IDX(opnd),
                                    0,
                                    Le_Opr)) {

         if (value == CMIC_WORK_DIST_CHUNKSIZE) {
            PRINTMSG(line, 1499, Error, column, "CHUNKSIZE");
         }
         else if (value == CMIC_WORK_DIST_NUMCHUNKS) {
            PRINTMSG(line, 1499, Error, column, "NUMCHUNKS");
         }
      }
# if (defined(_TARGET_OS_IRIX) || defined(_TARGET_OS_LINUX) || defined(_TARGET_OS_DARWIN))
      else if (OPND_FLD(opnd) != CN_Tbl_Idx && OPND_FLD(opnd) != NO_Tbl_Idx &&
               (value == CMIC_WORK_DIST_CHUNKSIZE ||
                value == CMIC_WORK_DIST_NUMCHUNKS)) {

         /* generate max(1,value) */

         NTR_IR_TBL(max_idx);
         IR_OPR(max_idx) = Max_Opr;
         IR_TYPE_IDX(max_idx) = exp_desc.type_idx;
         IR_LINE_NUM(ir_idx) = line;
         IR_COL_NUM(ir_idx) = column;

         OPND_FLD(opnd2) = CN_Tbl_Idx;
         OPND_IDX(opnd2) = CN_INTEGER_ONE_IDX;
         OPND_LINE_NUM(opnd2) = line;
         OPND_COL_NUM(opnd2) = column;

         cast_opnd_to_type_idx(&opnd2, exp_desc.type_idx);

         NTR_IR_LIST_TBL(list2_idx);
         IR_FLD_L(max_idx) = IL_Tbl_Idx;
         IR_LIST_CNT_L(max_idx) = 2;
         IR_IDX_L(max_idx) = list2_idx;

         COPY_OPND(IL_OPND(list2_idx), opnd);

         NTR_IR_LIST_TBL(IL_NEXT_LIST_IDX(list2_idx));
         IL_PREV_LIST_IDX(IL_NEXT_LIST_IDX(list2_idx)) = list2_idx;
         list2_idx = IL_NEXT_LIST_IDX(list2_idx);

         COPY_OPND(IL_OPND(list2_idx), opnd2);

         OPND_FLD(opnd) = IR_Tbl_Idx;
         OPND_IDX(opnd) = max_idx;
      }
# endif

      IL_FLD(list_idx) = AT_Tbl_Idx;
      idx = create_tmp_asg(&opnd,
                           &exp_desc,
                           &l_opnd,
                           Intent_In,
                           FALSE,
                           FALSE);
      IL_IDX(list_idx) = idx;
      IL_LINE_NUM(list_idx) = line;
      IL_COL_NUM(list_idx) = column;
   }

   curr_stmt_sh_idx = save_curr_stmt_sh_idx;


   TRACE (Func_Exit, "doall_cmic_semantics", NULL);

   return;

}  /* doall_cmic_semantics */

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

static void doparallel_cmic_semantics(void)

{
   int			column;
   expr_arg_type	exp_desc;
   int			idx;
   int			ir_idx;
   int			line;
   int			list_idx;
   opnd_type		l_opnd;
   opnd_type		opnd;
   int			save_curr_stmt_sh_idx;
   long64		value;

# if (defined(_TARGET_OS_IRIX) || defined(_TARGET_OS_LINUX) || defined(_TARGET_OS_DARWIN))
   int			list2_idx;
   int			max_idx;
   opnd_type		opnd2;
# endif


   TRACE (Func_Entry, "doparallel_cmic_semantics", NULL);

   ir_idx = SH_IR_IDX(curr_stmt_sh_idx);

   cdir_switches.dopar_sh_idx = curr_stmt_sh_idx;

   /* pull stmt header out of list */
   remove_sh(curr_stmt_sh_idx);
   save_curr_stmt_sh_idx = SH_PREV_IDX(curr_stmt_sh_idx);

   SH_PREV_IDX(cdir_switches.dopar_sh_idx) = NULL_IDX;
   SH_NEXT_IDX(cdir_switches.dopar_sh_idx) = NULL_IDX;

   list_idx = IR_IDX_L(ir_idx);

   /* skip WORK DISTRIBUTION */

   /* process work distribution expression */

   list_idx = IL_NEXT_LIST_IDX(list_idx);

   if (IL_FLD(list_idx) != NO_Tbl_Idx) {
      COPY_OPND(opnd, IL_OPND(list_idx));
      exp_desc.rank = 0;
      xref_state = CIF_Symbol_Reference;
      expr_semantics(&opnd, &exp_desc);

      find_opnd_line_and_column(&opnd, &line, &column);

      value = (IL_FLD(IL_PREV_LIST_IDX(list_idx)) != CN_Tbl_Idx) ? 0 :
                  CN_INT_TO_C(IL_IDX(IL_PREV_LIST_IDX(list_idx)));

      if (exp_desc.type != Integer ||
          exp_desc.rank != 0)      {
         PRINTMSG(line, 806, Error, column);
      }
      else if (OPND_FLD(opnd) == CN_Tbl_Idx &&
               IL_FLD(IL_PREV_LIST_IDX(list_idx)) == CN_Tbl_Idx &&
               compare_cn_and_value(OPND_IDX(opnd),
                                    0,
                                    Le_Opr)) {

         if (value == CMIC_WORK_DIST_CHUNKSIZE) {
            PRINTMSG(line, 1499, Error, column, "CHUNKSIZE");
         }
         else if (value == CMIC_WORK_DIST_NUMCHUNKS) {
            PRINTMSG(line, 1499, Error, column, "NUMCHUNKS");
         }
      }
# if (defined(_TARGET_OS_IRIX) || defined(_TARGET_OS_LINUX) || defined(_TARGET_OS_DARWIN))
      else if (OPND_FLD(opnd) != CN_Tbl_Idx && OPND_FLD(opnd) != NO_Tbl_Idx &&
               (value == CMIC_WORK_DIST_CHUNKSIZE ||
                value == CMIC_WORK_DIST_NUMCHUNKS)) {

         /* generate max(1,value) */

         NTR_IR_TBL(max_idx);
         IR_OPR(max_idx) = Max_Opr;
         IR_TYPE_IDX(max_idx) = exp_desc.type_idx;
         IR_LINE_NUM(ir_idx) = line;
         IR_COL_NUM(ir_idx) = column;

         OPND_FLD(opnd2) = CN_Tbl_Idx;
         OPND_IDX(opnd2) = CN_INTEGER_ONE_IDX;
         OPND_LINE_NUM(opnd2) = line;
         OPND_COL_NUM(opnd2) = column;

         cast_opnd_to_type_idx(&opnd2, exp_desc.type_idx);

         NTR_IR_LIST_TBL(list2_idx);
         IR_FLD_L(max_idx) = IL_Tbl_Idx;
         IR_LIST_CNT_L(max_idx) = 2;
         IR_IDX_L(max_idx) = list2_idx;

         COPY_OPND(IL_OPND(list2_idx), opnd);

         NTR_IR_LIST_TBL(IL_NEXT_LIST_IDX(list2_idx));
         IL_PREV_LIST_IDX(IL_NEXT_LIST_IDX(list2_idx)) = list2_idx;
         list2_idx = IL_NEXT_LIST_IDX(list2_idx);

         COPY_OPND(IL_OPND(list2_idx), opnd2);

         OPND_FLD(opnd) = IR_Tbl_Idx;
         OPND_IDX(opnd) = max_idx;
      }
# endif


      IL_FLD(list_idx) = AT_Tbl_Idx;
      idx = create_tmp_asg(&opnd,
                           &exp_desc,
                           &l_opnd,
                           Intent_In,
                           FALSE,
                           FALSE);
      IL_IDX(list_idx) = idx;
      IL_LINE_NUM(list_idx) = line;
      IL_COL_NUM(list_idx) = column;
   }

   curr_stmt_sh_idx = save_curr_stmt_sh_idx;

   TRACE (Func_Exit, "doparallel_cmic_semantics", NULL);

   return;

}  /* doparallel_cmic_semantics */

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

static void endparallel_cmic_semantics(void)

{
   int			list_idx;

   TRACE (Func_Entry, "endparallel_cmic_semantics", NULL);

   cdir_switches.no_internal_calls = FALSE;
   cdir_switches.parallel_region   = FALSE;
   cdir_switches.autoscope         = FALSE;

   if (cdir_switches.private_list_idx &&
       IL_FLD(cdir_switches.private_list_idx) != NO_Tbl_Idx) {

      list_idx = IL_IDX(cdir_switches.private_list_idx);

      while (list_idx) {
         if (IL_FLD(list_idx) == AT_Tbl_Idx &&
             AT_OBJ_CLASS(IL_IDX(list_idx)) == Data_Obj) {
   
            ATD_TASK_PRIVATE(IL_IDX(list_idx)) = FALSE;
         }
         list_idx = IL_NEXT_LIST_IDX(list_idx);
      }
   }

   if (cdir_switches.getfirst_list_idx &&
       IL_FLD(cdir_switches.getfirst_list_idx) != NO_Tbl_Idx) {

      list_idx = IL_IDX(cdir_switches.getfirst_list_idx);

      while (list_idx) {
         if (IL_FLD(list_idx) == AT_Tbl_Idx &&
             AT_OBJ_CLASS(IL_IDX(list_idx)) == Data_Obj) {
  
            ATD_TASK_GETFIRST(IL_IDX(list_idx)) = FALSE;
         }
         list_idx = IL_NEXT_LIST_IDX(list_idx);
      }
   }


   if (cdir_switches.shared_list_idx &&
       IL_FLD(cdir_switches.shared_list_idx) != NO_Tbl_Idx) {

      list_idx = IL_IDX(cdir_switches.shared_list_idx);

      while (list_idx) {
         if (IL_FLD(list_idx) == AT_Tbl_Idx &&
             AT_OBJ_CLASS(IL_IDX(list_idx)) == Data_Obj) {

            ATD_TASK_SHARED(IL_IDX(list_idx)) = FALSE;
         }
         else if (IL_FLD(list_idx) == AT_Tbl_Idx &&
                  AT_OBJ_CLASS(IL_IDX(list_idx)) == Pgm_Unit &&
                  ATP_PROC(IL_IDX(list_idx)) == Dummy_Proc) {

            ATP_TASK_SHARED(IL_IDX(list_idx)) = FALSE;
         }
         list_idx = IL_NEXT_LIST_IDX(list_idx);
      }
   }

   cdir_switches.getfirst_list_idx = NULL_IDX;
   cdir_switches.private_list_idx = NULL_IDX;
   cdir_switches.shared_list_idx  = NULL_IDX;

   TRACE (Func_Exit, "endparallel_cmic_semantics", NULL);

   return;

}  /* endparallel_cmic_semantics */

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

static void parallel_cmic_semantics(void)

{
   int                  attr_idx;
   int                  column;
   expr_arg_type        exp_desc;
   int                  getfirst_list_idx;
   int                  idx;
   int                  ir_idx;
   int                  line;
   int                  list_idx;
   int                  list2_idx;
   int                  list3_idx;
   opnd_type            l_opnd;
   opnd_type            opnd;
   int                  private_list_idx;
   int                  shared_list_idx;

# if (defined(_TARGET_OS_IRIX) || defined(_TARGET_OS_LINUX) || defined(_TARGET_OS_DARWIN))
   char			string[13];
# endif


   TRACE (Func_Entry, "parallel_cmic_semantics", NULL);

   ir_idx = SH_IR_IDX(curr_stmt_sh_idx);

   if (cdir_switches.doall_sh_idx != NULL_IDX ||
       cdir_switches.doacross_sh_idx != NULL_IDX ||
       cdir_switches.parallel_region ||
       cdir_switches.guard_in_par_reg) {

      /* error .. already in a parallel_region */
      PRINTMSG(IR_LINE_NUM(ir_idx), 818, Error, IR_COL_NUM(ir_idx));
   }

   list_idx = IR_IDX_L(ir_idx);

   /* process if condition */

   if (IL_FLD(list_idx) != NO_Tbl_Idx) {
      COPY_OPND(opnd, IL_OPND(list_idx));
      exp_desc.rank = 0;
      xref_state = CIF_Symbol_Reference;
      expr_semantics(&opnd, &exp_desc);

      find_opnd_line_and_column(&opnd, &line, &column);

      if (exp_desc.type != Logical ||
          exp_desc.rank != 0)      {
         PRINTMSG(line, 803, Error, column);
      }

      IL_FLD(list_idx) = AT_Tbl_Idx;
      idx = create_tmp_asg(&opnd,
                          &exp_desc,
                          &l_opnd,
                          Intent_In,
                          FALSE,
                          FALSE);
      IL_IDX(list_idx) = idx;

      IL_LINE_NUM(list_idx) = line;
      IL_COL_NUM(list_idx) = column;
   }

   /* process SHARED var list */

   list_idx = IL_NEXT_LIST_IDX(list_idx);
   cdir_switches.shared_list_idx = list_idx;

   if (IL_FLD(list_idx) != NO_Tbl_Idx) {

      list2_idx = IL_IDX(list_idx);

      while (list2_idx) {

         attr_idx = IL_IDX(list2_idx);
         AT_LOCKED_IN(attr_idx) = TRUE;

         while (AT_ATTR_LINK(attr_idx)) {
            attr_idx = AT_ATTR_LINK(attr_idx);
            AT_LOCKED_IN(attr_idx) = TRUE;
         }

         IL_IDX(list2_idx) = attr_idx;

         if (AT_OBJ_CLASS(attr_idx) == Pgm_Unit   &&
             ATP_PROC(attr_idx)     == Dummy_Proc) {
            ATP_TASK_SHARED(attr_idx) = TRUE;
         }
         else if (AT_OBJ_CLASS(attr_idx) != Data_Obj ||
                  ATD_CLASS(attr_idx)    == Constant) {
            PRINTMSG(IL_LINE_NUM(list2_idx), 804, Caution,
                     IL_COL_NUM(list2_idx),
			    AT_OBJ_NAME_PTR(attr_idx),
                     "SHARED", "PARALLEL");

            /* remove the attr from the list */
            if (list2_idx == IL_IDX(cdir_switches.shared_list_idx)) {
               /* head of the list */
               list2_idx = IL_NEXT_LIST_IDX(list2_idx);
               IL_IDX(cdir_switches.shared_list_idx) = list2_idx;
               IL_IDX(list_idx) = list2_idx;
               IL_PREV_LIST_IDX(list2_idx) = NULL_IDX;
               IL_LIST_CNT(list_idx)--;
               continue;
            }
            else {
               IL_NEXT_LIST_IDX(IL_PREV_LIST_IDX(list2_idx)) =
                         IL_NEXT_LIST_IDX(list2_idx);
               IL_PREV_LIST_IDX(IL_NEXT_LIST_IDX(list2_idx)) =
                         IL_PREV_LIST_IDX(list2_idx);

               list2_idx = IL_NEXT_LIST_IDX(list2_idx);
               IL_LIST_CNT(list_idx)--;
               continue;
            }
         }
         else {
            ATD_TASK_SHARED(attr_idx) = TRUE;
            ATD_WAS_SCOPED(attr_idx) = TRUE;
         }

         shared_list_idx = IL_IDX(cdir_switches.shared_list_idx);

         while (shared_list_idx != list2_idx &&
                shared_list_idx != NULL_IDX) {

            if (attr_idx == IL_IDX(shared_list_idx)) {
               /* take this out of the list */
               IL_NEXT_LIST_IDX(IL_PREV_LIST_IDX(list2_idx)) =
                      IL_NEXT_LIST_IDX(list2_idx);
               IL_PREV_LIST_IDX(IL_NEXT_LIST_IDX(list2_idx)) =
                      IL_PREV_LIST_IDX(list2_idx);

               list2_idx = IL_PREV_LIST_IDX(list2_idx);
               IL_LIST_CNT(list_idx)--;
               break;
            }
            shared_list_idx = IL_NEXT_LIST_IDX(shared_list_idx);
         }

         list2_idx = IL_NEXT_LIST_IDX(list2_idx);
      }
   }

   /* process PRIVATE var list */

   list_idx = IL_NEXT_LIST_IDX(list_idx);
   cdir_switches.private_list_idx = list_idx;

   if (IL_FLD(list_idx) != NO_Tbl_Idx) {

      list2_idx = IL_IDX(list_idx);

      while (list2_idx) {

         attr_idx = IL_IDX(list2_idx);
         AT_LOCKED_IN(attr_idx) = TRUE;

         while (AT_ATTR_LINK(attr_idx)) {
            attr_idx = AT_ATTR_LINK(attr_idx);
            AT_LOCKED_IN(attr_idx) = TRUE;
         }

         IL_IDX(list2_idx) = attr_idx;

# if (defined(_TARGET_OS_IRIX) || defined(_TARGET_OS_LINUX) || defined(_TARGET_OS_DARWIN))
        if (AT_OBJ_CLASS(attr_idx) == Data_Obj &&
            (ATD_ALLOCATABLE(attr_idx) ||
             ATD_CLASS(attr_idx) == CRI__Pointee ||
             ATD_POINTER(attr_idx))) {

           if (ATD_ALLOCATABLE(attr_idx)) {
              strcpy(string, "ALLOCATABLE");
           }
           else if (ATD_POINTER(attr_idx)) {
              strcpy(string, "POINTER");
           }
           else {
              strcpy(string, "Cray Pointee");
           }

           PRINTMSG(IL_LINE_NUM(list2_idx), 1446, Error,
                    IL_COL_NUM(list2_idx),
                    string,
                    "PARALLEL");

        }
        else
# endif
         if (AT_OBJ_CLASS(attr_idx) != Data_Obj ||
             ATD_CLASS(attr_idx)    == Constant) {
            PRINTMSG(IL_LINE_NUM(list2_idx), 804, Caution,
                     IL_COL_NUM(list2_idx),
			    AT_OBJ_NAME_PTR(attr_idx),
                     "PRIVATE", "PARALLEL");

            /* remove the attr from the list */
            if (list2_idx == IL_IDX(cdir_switches.private_list_idx)) {
               /* head of the list */
               list2_idx = IL_NEXT_LIST_IDX(list2_idx);
               IL_IDX(cdir_switches.private_list_idx) = list2_idx;
               IL_IDX(list_idx) = list2_idx;
               IL_PREV_LIST_IDX(list2_idx) = NULL_IDX;
               IL_LIST_CNT(list_idx)--;
               continue;
            }
            else {
               IL_NEXT_LIST_IDX(IL_PREV_LIST_IDX(list2_idx)) =
                         IL_NEXT_LIST_IDX(list2_idx);
               IL_PREV_LIST_IDX(IL_NEXT_LIST_IDX(list2_idx)) =
                         IL_PREV_LIST_IDX(list2_idx);

               list2_idx = IL_NEXT_LIST_IDX(list2_idx);
               IL_LIST_CNT(list_idx)--;
               continue;
            }
         }
         else {
            ATD_TASK_PRIVATE(attr_idx) = TRUE;
            ATD_WAS_SCOPED(attr_idx) = TRUE;

            if (ATD_CLASS(attr_idx) == Variable &&
                ATD_AUTOMATIC(attr_idx) &&
                ATD_AUTO_BASE_IDX(attr_idx) != NULL_IDX &&
                ! ATD_TASK_PRIVATE(ATD_AUTO_BASE_IDX(attr_idx))) {

               ATD_TASK_PRIVATE(ATD_AUTO_BASE_IDX(attr_idx)) = TRUE;

               NTR_IR_LIST_TBL(list3_idx);
               IL_PREV_LIST_IDX(IL_IDX(list_idx)) = list3_idx;
               IL_NEXT_LIST_IDX(list3_idx) = IL_IDX(list_idx);
               IL_IDX(list_idx) = list3_idx;
               IL_LIST_CNT(list_idx)++;

               IL_FLD(list3_idx) = AT_Tbl_Idx;
               IL_IDX(list3_idx) = ATD_AUTO_BASE_IDX(attr_idx);
               IL_LINE_NUM(list3_idx) = IL_LINE_NUM(list2_idx);
               IL_COL_NUM(list3_idx) = IL_COL_NUM(list2_idx);
            }
         }

         private_list_idx = IL_IDX(cdir_switches.private_list_idx);

         while (private_list_idx != list2_idx &&
                private_list_idx != NULL_IDX) {

            if (attr_idx == IL_IDX(private_list_idx)) {
               /* take this out of the list */
               IL_NEXT_LIST_IDX(IL_PREV_LIST_IDX(list2_idx)) =
                      IL_NEXT_LIST_IDX(list2_idx);
               IL_PREV_LIST_IDX(IL_NEXT_LIST_IDX(list2_idx)) =
                      IL_PREV_LIST_IDX(list2_idx);

               list2_idx = IL_PREV_LIST_IDX(list2_idx);
               IL_LIST_CNT(list_idx)--;
               goto CONTINUE3;
            }
            private_list_idx = IL_NEXT_LIST_IDX(private_list_idx);
         }

         shared_list_idx = IL_IDX(cdir_switches.shared_list_idx);

         while (shared_list_idx) {

            if (attr_idx == IL_IDX(shared_list_idx)) {
               /* error, cannot have var in shared and private */
               PRINTMSG(IL_LINE_NUM(list2_idx), 805, Error,
                        IL_COL_NUM(list2_idx),
                        AT_OBJ_NAME_PTR(attr_idx));
               break;
            }
            shared_list_idx = IL_NEXT_LIST_IDX(shared_list_idx);
         }

CONTINUE3:
         list2_idx = IL_NEXT_LIST_IDX(list2_idx);
      }
   }

  /* process GETFIRST var list */

  list_idx = IL_NEXT_LIST_IDX(list_idx);
  cdir_switches.getfirst_list_idx = list_idx;

  if (IL_FLD(list_idx) != NO_Tbl_Idx) {

     list2_idx = IL_IDX(list_idx);

     while (list2_idx) {

        attr_idx = IL_IDX(list2_idx);
        AT_LOCKED_IN(attr_idx) = TRUE;

        while (AT_ATTR_LINK(attr_idx)) {
           attr_idx = AT_ATTR_LINK(attr_idx);
           AT_LOCKED_IN(attr_idx) = TRUE;
        }

        IL_IDX(list2_idx) = attr_idx;

        if (AT_OBJ_CLASS(attr_idx) != Data_Obj ||
            ATD_CLASS(attr_idx)    == Constant) {
           PRINTMSG(IL_LINE_NUM(list2_idx), 804, Caution,
                    IL_COL_NUM(list2_idx),
                    AT_OBJ_NAME_PTR(attr_idx),
                    "GETFIRST", "PARALLEL");

           /* remove the attr from the list */

           if (list2_idx == IL_IDX(cdir_switches.getfirst_list_idx)) {

              /* head of the list */

              list2_idx = IL_NEXT_LIST_IDX(list2_idx);
              IL_IDX(cdir_switches.getfirst_list_idx) = list2_idx;
              IL_IDX(list_idx) = list2_idx;
              IL_PREV_LIST_IDX(list2_idx) = NULL_IDX;
              IL_LIST_CNT(list_idx)--;
              continue;
           }
           else {
              IL_NEXT_LIST_IDX(IL_PREV_LIST_IDX(list2_idx)) =
                        IL_NEXT_LIST_IDX(list2_idx);
              IL_PREV_LIST_IDX(IL_NEXT_LIST_IDX(list2_idx)) =
                        IL_PREV_LIST_IDX(list2_idx);

              list2_idx = IL_NEXT_LIST_IDX(list2_idx);
              IL_LIST_CNT(list_idx)--;
              continue;
           }
        }
        else {
           ATD_TASK_GETFIRST(attr_idx) = TRUE;

           if (ATD_CLASS(attr_idx) == Variable &&
               ATD_AUTOMATIC(attr_idx) &&
               ATD_AUTO_BASE_IDX(attr_idx) != NULL_IDX &&
               ! ATD_TASK_PRIVATE(ATD_AUTO_BASE_IDX(attr_idx))) {

              ATD_TASK_GETFIRST(ATD_AUTO_BASE_IDX(attr_idx)) = TRUE;

              NTR_IR_LIST_TBL(list3_idx);
              IL_PREV_LIST_IDX(IL_IDX(list_idx)) = list3_idx;
              IL_NEXT_LIST_IDX(list3_idx) = IL_IDX(list_idx);
              IL_IDX(list_idx) = list3_idx;
              IL_LIST_CNT(list_idx)++;

              IL_FLD(list3_idx) = AT_Tbl_Idx;
              IL_IDX(list3_idx) = ATD_AUTO_BASE_IDX(attr_idx);
              IL_LINE_NUM(list3_idx) = IL_LINE_NUM(list2_idx);
              IL_COL_NUM(list3_idx) = IL_COL_NUM(list2_idx);
           }
        }

        getfirst_list_idx = IL_IDX(cdir_switches.getfirst_list_idx);

        while (getfirst_list_idx != list2_idx &&
               getfirst_list_idx != NULL_IDX) {

           if (attr_idx == IL_IDX(getfirst_list_idx)) {

              /* take this out of the list */

              IL_NEXT_LIST_IDX(IL_PREV_LIST_IDX(list2_idx)) =
                     IL_NEXT_LIST_IDX(list2_idx);
              IL_PREV_LIST_IDX(IL_NEXT_LIST_IDX(list2_idx)) =
                     IL_PREV_LIST_IDX(list2_idx);

              list2_idx = IL_PREV_LIST_IDX(list2_idx);
              IL_LIST_CNT(list_idx)--;
              goto CONTINUE4;
           }
           getfirst_list_idx = IL_NEXT_LIST_IDX(getfirst_list_idx);
        }


        shared_list_idx = IL_IDX(cdir_switches.shared_list_idx);

        while (shared_list_idx) {

           if (attr_idx == IL_IDX(shared_list_idx)) {

              /* error, cannot have var in shared and getfirst */

              PRINTMSG(IL_LINE_NUM(list2_idx), 1314, Error,
                       IL_COL_NUM(list2_idx),
                       AT_OBJ_NAME_PTR(attr_idx),
                       "SHARED", "GETFIRST");
              break;
           }
           shared_list_idx = IL_NEXT_LIST_IDX(shared_list_idx);
        }

        private_list_idx = IL_IDX(cdir_switches.private_list_idx);

        while (private_list_idx) {

           if (attr_idx == IL_IDX(private_list_idx)) {

              /* error, cannot have var in private and getfirst */

              PRINTMSG(IL_LINE_NUM(list2_idx), 1314, Error,
                       IL_COL_NUM(list2_idx),
                       AT_OBJ_NAME_PTR(attr_idx),
                       "PRIVATE", "GETFIRST");
              break;
           }
           private_list_idx = IL_NEXT_LIST_IDX(private_list_idx);
        }


CONTINUE4:
        list2_idx = IL_NEXT_LIST_IDX(list2_idx);
     }
  }


   /* AUTOSCOPE */

   list_idx = IL_NEXT_LIST_IDX(list_idx);

   if (IL_FLD(list_idx) == CN_Tbl_Idx) {
      cdir_switches.autoscope = TRUE;
   }

   /* process CONTROL var list */

   list_idx = IL_NEXT_LIST_IDX(list_idx);

   if (IL_FLD(list_idx) != NO_Tbl_Idx) {

      list2_idx = IL_IDX(list_idx);

      while (list2_idx) {

         attr_idx = IL_IDX(list2_idx);
         AT_LOCKED_IN(attr_idx) = TRUE;

         while (AT_ATTR_LINK(attr_idx)) {
            attr_idx = AT_ATTR_LINK(attr_idx);
            AT_LOCKED_IN(attr_idx) = TRUE;
         }

         IL_IDX(list2_idx) = attr_idx;

         if (AT_OBJ_CLASS(attr_idx) != Data_Obj ||
             ATD_CLASS(attr_idx)    == Constant) {
            PRINTMSG(IL_LINE_NUM(list2_idx), 804, Caution,
                     IL_COL_NUM(list2_idx),
			    AT_OBJ_NAME_PTR(attr_idx),
                     "CONTROL", "PARALLEL");

            /* remove the attr from the list */
            if (list2_idx == IL_IDX(list_idx)) {
               /* head of the list */
               list2_idx = IL_NEXT_LIST_IDX(list2_idx);
               IL_IDX(list_idx) = list2_idx;
               IL_PREV_LIST_IDX(list2_idx) = NULL_IDX;
               IL_LIST_CNT(list_idx)--;
               continue;
            }
            else {
               IL_NEXT_LIST_IDX(IL_PREV_LIST_IDX(list2_idx)) =
                         IL_NEXT_LIST_IDX(list2_idx);
               IL_PREV_LIST_IDX(IL_NEXT_LIST_IDX(list2_idx)) =
                         IL_PREV_LIST_IDX(list2_idx);

               list2_idx = IL_NEXT_LIST_IDX(list2_idx);
               IL_LIST_CNT(list_idx)--;
               continue;
            }
         }

         list2_idx = IL_NEXT_LIST_IDX(list2_idx);
      }
   }

   /* process MAXCPUS value */

   list_idx = IL_NEXT_LIST_IDX(list_idx);

   if (IL_FLD(list_idx) != NO_Tbl_Idx) {
      COPY_OPND(opnd, IL_OPND(list_idx));
      exp_desc.rank = 0;
      xref_state = CIF_Symbol_Reference;
      expr_semantics(&opnd, &exp_desc);

      find_opnd_line_and_column(&opnd, &line, &column);
      if (exp_desc.type != Integer ||
          exp_desc.rank != 0)      {
         PRINTMSG(line, 806, Error, column);
      }

      IL_FLD(list_idx) = AT_Tbl_Idx;
      idx = create_tmp_asg(&opnd,
                           &exp_desc,
                           &l_opnd,
                           Intent_In,
                           FALSE,
                           FALSE);
      IL_IDX(list_idx) = idx;
      IL_LINE_NUM(list_idx) = line;
      IL_COL_NUM(list_idx) = column;
   }
   else if (cdir_switches.maxcpus) {
      COPY_OPND(IL_OPND(list_idx), cdir_switches.maxcpus_opnd);
      cdir_switches.maxcpus	= FALSE;
   }

   cdir_switches.no_internal_calls = TRUE;
   cdir_switches.parallel_region = TRUE;
   TRACE (Func_Exit, "parallel_cmic_semantics", NULL);

   return;

}  /* parallel_cmic_semantics */

/******************************************************************************\
|*									      *|
|* Description:								      *|
|*      The ir looks like this coming in ...                                  *|
|*                                                                            *|
|*                        (mp_directive_opr)                                  *|
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
|*                      |- ORDERED constant (ORDERED == 1, else NO_Tbl_Idx)   *|
|*									      *|
|*	Not all clauses are valid for all directives.                         *|
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

static void mp_directive_semantics(mp_directive_type directive)

{
   int			attr_idx;
   int			column;
   expr_arg_type	exp_desc;
   int			i;
   int			idx;
   int			ir_idx;
   int			line;
   int          	list_array[MP_DIR_LIST_CNT];
   int			list_idx;
   int			list2_idx;
   int			list3_idx;
   opnd_type		l_opnd;
   opnd_type		opnd;
   int			orig_sh_idx;
   int			save_curr_stmt_sh_idx;
   boolean		save_error_flag;
   char			string[13];


   TRACE (Func_Entry, "mp_directive_semantics", NULL);

   ir_idx = SH_IR_IDX(curr_stmt_sh_idx);
   save_curr_stmt_sh_idx = curr_stmt_sh_idx;
   orig_sh_idx = curr_stmt_sh_idx;
   save_error_flag = SH_ERR_FLG(curr_stmt_sh_idx);

   list_idx = IR_IDX_L(ir_idx);

   for (i = 0; i < MP_DIR_LIST_CNT; i++) {
      list_array[i] = list_idx;
      list_idx = IL_NEXT_LIST_IDX(list_idx);
   }

   if (directive == Doacross ||
       directive == Parallel_Do ||
       directive == Pdo) {

      /* pull stmt header out of list */
      remove_sh(curr_stmt_sh_idx);
      save_curr_stmt_sh_idx = SH_PREV_IDX(curr_stmt_sh_idx);

      switch (directive) {
      case Doacross:
         cdir_switches.doacross_sh_idx = curr_stmt_sh_idx;
         SH_PREV_IDX(cdir_switches.doacross_sh_idx) = NULL_IDX;
         SH_NEXT_IDX(cdir_switches.doacross_sh_idx) = NULL_IDX;
         break;

      case Parallel_Do:
         cdir_switches.paralleldo_sh_idx = curr_stmt_sh_idx;
         SH_PREV_IDX(cdir_switches.paralleldo_sh_idx) = NULL_IDX;
         SH_NEXT_IDX(cdir_switches.paralleldo_sh_idx) = NULL_IDX;
         break;

      case Pdo:
         cdir_switches.pdo_sh_idx = curr_stmt_sh_idx;
         SH_PREV_IDX(cdir_switches.pdo_sh_idx) = NULL_IDX;
         SH_NEXT_IDX(cdir_switches.pdo_sh_idx) = NULL_IDX;
         break;
      }
   }
   else {
      cdir_switches.parallel_region = TRUE;
   }

   if (clause_allowed[directive][If_Clause]) {
      list_idx = list_array[MP_DIR_IF_IDX];

      /* process IF condition */

      if (IL_FLD(list_idx) != NO_Tbl_Idx) {
         COPY_OPND(opnd, IL_OPND(list_idx));
         exp_desc.rank = 0;
         xref_state = CIF_Symbol_Reference;
         expr_semantics(&opnd, &exp_desc);

         find_opnd_line_and_column(&opnd, &line, &column);
         if (exp_desc.type != Logical ||
             exp_desc.rank != 0)      {
            PRINTMSG(line, 803, Error, column);
         }

         IL_FLD(list_idx) = AT_Tbl_Idx;
         idx = create_tmp_asg(&opnd,
                              &exp_desc,
                              &l_opnd,
                              Intent_In,
                              FALSE,
                              FALSE);
         IL_IDX(list_idx) = idx;
         IL_LINE_NUM(list_idx) = line;
         IL_COL_NUM(list_idx) = column;
      }
   }

   if (clause_allowed[directive][Chunk_Clause]) {
      /* process CHUNK expression */

      list_idx = list_array[MP_DIR_CHUNK_IDX];

      if (IL_FLD(list_idx) != NO_Tbl_Idx) {
         COPY_OPND(opnd, IL_OPND(list_idx));
         exp_desc.rank = 0;
         xref_state = CIF_Symbol_Reference;
         expr_semantics(&opnd, &exp_desc);

         find_opnd_line_and_column(&opnd, &line, &column);

         if (exp_desc.type != Integer ||
             exp_desc.rank != 0)      {
            PRINTMSG(line, 1364, Error, column);
         }

         IL_FLD(list_idx) = AT_Tbl_Idx;
         idx = create_tmp_asg(&opnd,
                              &exp_desc,
                              &l_opnd,
                              Intent_In,
                              FALSE,
                              FALSE);
         IL_IDX(list_idx) = idx;
         IL_LINE_NUM(list_idx) = line;
         IL_COL_NUM(list_idx) = column;
      }
   }

   if (directive != Doacross &&
       directive != Parallel_Do) {

      push_task_blk(curr_stmt_sh_idx);
   }

   cdir_switches.lastlocal_list_idx = list_array[MP_DIR_LASTLOCAL_IDX];
   cdir_switches.private_list_idx = list_array[MP_DIR_LOCAL_IDX];
   cdir_switches.shared_list_idx = list_array[MP_DIR_SHARE_IDX];
   cdir_switches.reduction_list_idx = list_array[MP_DIR_REDUCTION_IDX];
   cdir_switches.lastthread_list_idx = list_array[MP_DIR_LASTTHREAD_IDX];
    
   if (clause_allowed[directive][Share_Clause]) {
      /* process SHARED var list */
    
      list_idx = list_array[MP_DIR_SHARE_IDX];
    
      if (IL_FLD(list_idx) != NO_Tbl_Idx) {
    
	 list2_idx = IL_IDX(list_idx);
    
	 while (list2_idx) {
    
	    attr_idx = IL_IDX(list2_idx);
	    AT_LOCKED_IN(attr_idx) = TRUE;
    
	    while (AT_ATTR_LINK(attr_idx)) {
	       attr_idx = AT_ATTR_LINK(attr_idx);
	       AT_LOCKED_IN(attr_idx) = TRUE;
	    }
    
	    IL_IDX(list2_idx) = attr_idx;
    
	    if (AT_OBJ_CLASS(attr_idx) == Pgm_Unit   &&
		ATP_PROC(attr_idx)     == Dummy_Proc) {
	       ATP_TASK_SHARED(attr_idx) = TRUE;
	    }
	    else if (AT_OBJ_CLASS(attr_idx) != Data_Obj ||
		     ATD_CLASS(attr_idx)    == Constant) {
	       PRINTMSG(IL_LINE_NUM(list2_idx), 804, Caution,
			IL_COL_NUM(list2_idx),
			AT_OBJ_NAME_PTR(attr_idx),
			"SHARE", mp_dir_str[directive]);
    
	       /* remove the attr from the list */
    
	       if (list2_idx == IL_IDX(cdir_switches.shared_list_idx)) {
    
		   /* head of the list */
    
		  list2_idx = IL_NEXT_LIST_IDX(list2_idx);
		  IL_IDX(cdir_switches.shared_list_idx) = list2_idx;
		  IL_IDX(list_idx) = list2_idx;
		  IL_PREV_LIST_IDX(list2_idx) = NULL_IDX;
		  IL_LIST_CNT(list_idx)--;
		  continue;
	       }
	       else {
		  IL_NEXT_LIST_IDX(IL_PREV_LIST_IDX(list2_idx)) =
			    IL_NEXT_LIST_IDX(list2_idx);
		  IL_PREV_LIST_IDX(IL_NEXT_LIST_IDX(list2_idx)) =
			    IL_PREV_LIST_IDX(list2_idx);
    
		  list2_idx = IL_NEXT_LIST_IDX(list2_idx);
		  IL_LIST_CNT(list_idx)--;
		  continue;
	       }
	    }
	    else if (! ATD_TASK_PRIVATE(attr_idx) &&
		     ! ATD_TASK_LASTTHREAD(attr_idx) &&
		     ! ATD_TASK_LASTLOCAL(attr_idx)) {
    
	       /* ATD_TASK_REDUCTION is allowed for SHARED */
    
               ATD_TASK_SHARED(attr_idx) = TRUE;
               ATD_WAS_SCOPED(attr_idx) = TRUE;

               if (ATD_CLASS(attr_idx) == Variable &&
                   ATD_AUTOMATIC(attr_idx) &&
                   ATD_AUTO_BASE_IDX(attr_idx) != NULL_IDX &&
                   ! ATD_TASK_SHARED(ATD_AUTO_BASE_IDX(attr_idx))) {

                  ATD_TASK_SHARED(ATD_AUTO_BASE_IDX(attr_idx)) = TRUE;

                  NTR_IR_LIST_TBL(list3_idx);
                  IL_PREV_LIST_IDX(IL_IDX(list_idx)) = list3_idx;
                  IL_NEXT_LIST_IDX(list3_idx) = IL_IDX(list_idx);
                  IL_IDX(list_idx) = list3_idx;
                  IL_LIST_CNT(list_idx)++;

                  IL_FLD(list3_idx) = AT_Tbl_Idx;
                  IL_IDX(list3_idx) = ATD_AUTO_BASE_IDX(attr_idx);
                  IL_LINE_NUM(list3_idx) = IL_LINE_NUM(list2_idx);
                  IL_COL_NUM(list3_idx) = IL_COL_NUM(list2_idx);
	       }
	    }
	    else {
	       PRINTMSG(IL_LINE_NUM(list2_idx), 1362, Error, 
			IL_COL_NUM(list2_idx), 
			AT_OBJ_NAME_PTR(attr_idx));
	    }
    
	    list2_idx = IL_NEXT_LIST_IDX(list2_idx);
	 }
      }
   }
 
   if (clause_allowed[directive][Lastlocal_Clause]) {
      /* process LASTLOCAL var list */

      list_idx = list_array[MP_DIR_LASTLOCAL_IDX];

      if (IL_FLD(list_idx) != NO_Tbl_Idx) {

         list2_idx = IL_IDX(list_idx);

         while (list2_idx) {

            attr_idx = IL_IDX(list2_idx);
            AT_LOCKED_IN(attr_idx) = TRUE;

            while (AT_ATTR_LINK(attr_idx)) {
               attr_idx = AT_ATTR_LINK(attr_idx);
               AT_LOCKED_IN(attr_idx) = TRUE;
            }

            IL_IDX(list2_idx) = attr_idx;

            if (AT_OBJ_CLASS(attr_idx) != Data_Obj ||
                ATD_CLASS(attr_idx)    == Constant) {

               PRINTMSG(IL_LINE_NUM(list2_idx), 804, Caution,
                        IL_COL_NUM(list2_idx),
                        AT_OBJ_NAME_PTR(attr_idx),
                        "LASTLOCAL", mp_dir_str[directive]);

               /* remove the attr from the list */

               if (list2_idx == IL_IDX(cdir_switches.lastlocal_list_idx)) {

                   /* head of the list */

                  list2_idx = IL_NEXT_LIST_IDX(list2_idx);
                  IL_IDX(cdir_switches.lastlocal_list_idx) = list2_idx;
                  IL_IDX(list_idx) = list2_idx;
                  IL_PREV_LIST_IDX(list2_idx) = NULL_IDX;
                  IL_LIST_CNT(list_idx)--;
                  continue;
               }
               else {
                  IL_NEXT_LIST_IDX(IL_PREV_LIST_IDX(list2_idx)) =
                            IL_NEXT_LIST_IDX(list2_idx);
                  IL_PREV_LIST_IDX(IL_NEXT_LIST_IDX(list2_idx)) =
                            IL_PREV_LIST_IDX(list2_idx);

                  list2_idx = IL_NEXT_LIST_IDX(list2_idx);
                  IL_LIST_CNT(list_idx)--;
                  continue;
               }
            }
            else if (! ATD_TASK_PRIVATE(attr_idx) &&
                     ! ATD_TASK_LASTTHREAD(attr_idx) &&
                     ! ATD_TASK_SHARED(attr_idx) &&
                     ! ATD_TASK_REDUCTION(attr_idx)) {

               ATD_TASK_LASTLOCAL(attr_idx) = TRUE;

               if (ATD_CLASS(attr_idx) == Variable &&
                   ATD_AUTOMATIC(attr_idx) &&
                   ATD_AUTO_BASE_IDX(attr_idx) != NULL_IDX &&
                   ! ATD_TASK_LASTLOCAL(ATD_AUTO_BASE_IDX(attr_idx))) {

                  ATD_TASK_LASTLOCAL(ATD_AUTO_BASE_IDX(attr_idx)) = TRUE;

                  NTR_IR_LIST_TBL(list3_idx);
                  IL_PREV_LIST_IDX(IL_IDX(list_idx)) = list3_idx;
                  IL_NEXT_LIST_IDX(list3_idx) = IL_IDX(list_idx);
                  IL_IDX(list_idx) = list3_idx;
                  IL_LIST_CNT(list_idx)++;

                  IL_FLD(list3_idx) = AT_Tbl_Idx;
                  IL_IDX(list3_idx) = ATD_AUTO_BASE_IDX(attr_idx);
                  IL_LINE_NUM(list3_idx) = IL_LINE_NUM(list2_idx);
                  IL_COL_NUM(list3_idx) = IL_COL_NUM(list2_idx);
               }
            }
            else {
               PRINTMSG(IL_LINE_NUM(list2_idx), 1362, Error,
                        IL_COL_NUM(list2_idx),
                        AT_OBJ_NAME_PTR(attr_idx));
            }

            list2_idx = IL_NEXT_LIST_IDX(list2_idx);
         }
      }
   }

   if (clause_allowed[directive][Local_Clause]) {
      /* process LOCAL var list */
    
      list_idx = list_array[MP_DIR_LOCAL_IDX];
    
      if (IL_FLD(list_idx) != NO_Tbl_Idx) {
    
	 list2_idx = IL_IDX(list_idx);
    
	 while (list2_idx) {
    
	    attr_idx = IL_IDX(list2_idx);
	    AT_LOCKED_IN(attr_idx) = TRUE;
    
	    while (AT_ATTR_LINK(attr_idx)) {
	       attr_idx = AT_ATTR_LINK(attr_idx);
	       AT_LOCKED_IN(attr_idx) = TRUE;
	    }
    
	    IL_IDX(list2_idx) = attr_idx;
    
            if (AT_OBJ_CLASS(attr_idx) == Data_Obj &&
                (ATD_ALLOCATABLE(attr_idx) ||
                 ATD_CLASS(attr_idx) == CRI__Pointee ||
                 ATD_POINTER(attr_idx))) {

               if (ATD_ALLOCATABLE(attr_idx)) {
                  strcpy(string, "ALLOCATABLE");
               }
               else if (ATD_POINTER(attr_idx)) {
                  strcpy(string, "POINTER");
               }
               else {
                  strcpy(string, "Cray Pointee");
               }

               PRINTMSG(IL_LINE_NUM(list2_idx), 1430, Error,
                        IL_COL_NUM(list2_idx),
                        string,
                        AT_OBJ_NAME_PTR(attr_idx),
                        mp_dir_str[directive]);
 
            }
	    else if (AT_OBJ_CLASS(attr_idx) != Data_Obj ||
		     ATD_CLASS(attr_idx)    == Constant) {
    
	       PRINTMSG(IL_LINE_NUM(list2_idx), 804, Caution,
			IL_COL_NUM(list2_idx),
			AT_OBJ_NAME_PTR(attr_idx),
			"LOCAL", mp_dir_str[directive]);
    
	       /* remove the attr from the list */
    
	       if (list2_idx == IL_IDX(cdir_switches.private_list_idx)) {
    
		   /* head of the list */
    
		  list2_idx = IL_NEXT_LIST_IDX(list2_idx);
		  IL_IDX(cdir_switches.private_list_idx) = list2_idx;
		  IL_IDX(list_idx) = list2_idx;
		  IL_PREV_LIST_IDX(list2_idx) = NULL_IDX;
		  IL_LIST_CNT(list_idx)--;
		  continue;
	       }
	       else {
		  IL_NEXT_LIST_IDX(IL_PREV_LIST_IDX(list2_idx)) =
			    IL_NEXT_LIST_IDX(list2_idx);
		  IL_PREV_LIST_IDX(IL_NEXT_LIST_IDX(list2_idx)) =
			    IL_PREV_LIST_IDX(list2_idx);
    
		  list2_idx = IL_NEXT_LIST_IDX(list2_idx);
		  IL_LIST_CNT(list_idx)--;
		  continue;
	       }
	    }
	    else if (! ATD_TASK_SHARED(attr_idx) &&
		     ! ATD_TASK_LASTLOCAL(attr_idx) &&
		     ! ATD_TASK_LASTTHREAD(attr_idx) &&
		     ! ATD_TASK_REDUCTION(attr_idx)) {
    
               ATD_TASK_PRIVATE(attr_idx) = TRUE;
               ATD_WAS_SCOPED(attr_idx) = TRUE;

               if (ATD_CLASS(attr_idx) == Variable &&
                   ATD_AUTOMATIC(attr_idx) &&
                   ATD_AUTO_BASE_IDX(attr_idx) != NULL_IDX &&
                   ! ATD_TASK_PRIVATE(ATD_AUTO_BASE_IDX(attr_idx))) {

                  ATD_TASK_PRIVATE(ATD_AUTO_BASE_IDX(attr_idx)) = TRUE;

                  NTR_IR_LIST_TBL(list3_idx);
                  IL_PREV_LIST_IDX(IL_IDX(list_idx)) = list3_idx;
                  IL_NEXT_LIST_IDX(list3_idx) = IL_IDX(list_idx);
                  IL_IDX(list_idx) = list3_idx;
                  IL_LIST_CNT(list_idx)++;

                  IL_FLD(list3_idx) = AT_Tbl_Idx;
                  IL_IDX(list3_idx) = ATD_AUTO_BASE_IDX(attr_idx);
                  IL_LINE_NUM(list3_idx) = IL_LINE_NUM(list2_idx);
                  IL_COL_NUM(list3_idx) = IL_COL_NUM(list2_idx);
	       }
	    }
	    else {
	       PRINTMSG(IL_LINE_NUM(list2_idx), 1362, Error, 
			IL_COL_NUM(list2_idx), 
			AT_OBJ_NAME_PTR(attr_idx));
	    }
    
	    list2_idx = IL_NEXT_LIST_IDX(list2_idx);
	 }
      }
   }

   if (clause_allowed[directive][Lastthread_Clause]) {
      list_idx = list_array[MP_DIR_LASTTHREAD_IDX];

      if (IL_FLD(list_idx) == AT_Tbl_Idx) {

         attr_idx = IL_IDX(list_idx);
         AT_LOCKED_IN(attr_idx) = TRUE;

         while (AT_ATTR_LINK(attr_idx)) {
            attr_idx = AT_ATTR_LINK(attr_idx);
            AT_LOCKED_IN(attr_idx) = TRUE;
         }

         IL_IDX(list_idx) = attr_idx;

         if (! ATD_TASK_PRIVATE(attr_idx) &&
             ! ATD_TASK_LASTLOCAL(attr_idx) &&
             ! ATD_TASK_SHARED(attr_idx) &&
             ! ATD_TASK_REDUCTION(attr_idx)) {

            ATD_TASK_LASTTHREAD(attr_idx) = TRUE;
         }
         else {
            PRINTMSG(IL_LINE_NUM(list_idx), 1362, Error,
                     IL_COL_NUM(list_idx),
                     AT_OBJ_NAME_PTR(attr_idx));
         }
      }
   }

   /* no calls to expr_semantics can be made before the NEST processing */

   if (clause_allowed[directive][Nest_Clause]) {
      /* process NEST var list */

      list_idx = list_array[MP_DIR_NEST_IDX];

      if (IL_FLD(list_idx) != NO_Tbl_Idx) {

         list2_idx = IL_IDX(list_idx);

         while (list2_idx) {

            attr_idx = IL_IDX(list2_idx);
            AT_LOCKED_IN(attr_idx) = TRUE;

            while (AT_ATTR_LINK(attr_idx)) {
               attr_idx = AT_ATTR_LINK(attr_idx);
               AT_LOCKED_IN(attr_idx) = TRUE;
            }

            if (! ATD_TASK_PRIVATE(attr_idx) &&
                ! ATD_TASK_LASTLOCAL(attr_idx)) {

               NTR_IR_LIST_TBL(list3_idx);
               IL_NEXT_LIST_IDX(list3_idx) =
                         IL_IDX(cdir_switches.lastlocal_list_idx);
               if (IL_IDX(cdir_switches.lastlocal_list_idx) != NULL_IDX) {
                  IL_PREV_LIST_IDX(IL_IDX(cdir_switches.lastlocal_list_idx)) =
                                          list3_idx;
               }
               IL_IDX(cdir_switches.lastlocal_list_idx) = list3_idx;
               IL_FLD(cdir_switches.lastlocal_list_idx) = IL_Tbl_Idx;
               IL_LIST_CNT(cdir_switches.lastlocal_list_idx)++;
               IL_FLD(list3_idx) = AT_Tbl_Idx;
               IL_IDX(list3_idx) = attr_idx;
               ATD_TASK_LASTLOCAL(attr_idx) = TRUE;
            }

            IL_IDX(list2_idx) = attr_idx;
            list2_idx = IL_NEXT_LIST_IDX(list2_idx);
         }
      }
   }

   if (clause_allowed[directive][Reduction_Clause]) {
      /* process REDUCTION var list */
    
      list_idx = list_array[MP_DIR_REDUCTION_IDX];
    
      if (IL_FLD(list_idx) != NO_Tbl_Idx) {
    
	 list2_idx = IL_IDX(list_idx);
    
	 while (list2_idx) {
    
	    COPY_OPND(opnd, IL_OPND(list2_idx));
	    xref_state = CIF_Symbol_Reference;
	    exp_desc.rank = 0;
	    expr_semantics(&opnd, &exp_desc);
    
	    find_opnd_line_and_column(&opnd, &line, &column);
	    attr_idx = find_left_attr(&opnd);
    
	    if (exp_desc.rank != 0) {
	       PRINTMSG(line, 1363, Error, column);
	    }
	    else if (AT_OBJ_CLASS(attr_idx) != Data_Obj ||
		     ATD_CLASS(attr_idx)    == Constant) {
    
	       PRINTMSG(line, 804, Caution, column,
			AT_OBJ_NAME_PTR(attr_idx),
			"REDUCTION", mp_dir_str[directive]);
    
	       /* remove the attr from the list */
    
	       if (list2_idx == IL_IDX(cdir_switches.reduction_list_idx)) {
    
		   /* head of the list */
    
		  list2_idx = IL_NEXT_LIST_IDX(list2_idx);
		  IL_IDX(cdir_switches.reduction_list_idx) = list2_idx;
		  IL_IDX(list_idx) = list2_idx;
		  IL_PREV_LIST_IDX(list2_idx) = NULL_IDX;
		  IL_LIST_CNT(list_idx)--;
		  continue;
	       }
	       else {
		  IL_NEXT_LIST_IDX(IL_PREV_LIST_IDX(list2_idx)) =
			    IL_NEXT_LIST_IDX(list2_idx);
		  IL_PREV_LIST_IDX(IL_NEXT_LIST_IDX(list2_idx)) =
			    IL_PREV_LIST_IDX(list2_idx);
    
		  list2_idx = IL_NEXT_LIST_IDX(list2_idx);
		  IL_LIST_CNT(list_idx)--;
		  continue;
	       }
	    }
	    else if (! ATD_TASK_PRIVATE(attr_idx) &&
		     ! ATD_TASK_LASTTHREAD(attr_idx) &&
		     ! ATD_TASK_LASTLOCAL(attr_idx)) {
    
	       /* ATD_TASK_REDUCTION is allowed for SHARED */
    
	       ATD_TASK_REDUCTION(attr_idx) = TRUE;
	    }
	    else {
	       PRINTMSG(line, 1362, Error, column,
			AT_OBJ_NAME_PTR(attr_idx));
	    }
    
	    list2_idx = IL_NEXT_LIST_IDX(list2_idx);
	 }
      }
   }
 
   if (clause_allowed[directive][Affinity_Clause]) {
      /* process AFFINITY var list */

      list_idx = list_array[MP_DIR_AFFINITY_IDX];

      if (IL_FLD(list_idx) != NO_Tbl_Idx) {

         list2_idx = IL_IDX(list_idx);
         list3_idx = list_array[MP_DIR_NEST_IDX];
         list3_idx = IL_IDX(list3_idx);

         while (list2_idx) {

            attr_idx = IL_IDX(list2_idx);
            AT_LOCKED_IN(attr_idx) = TRUE;

            while (AT_ATTR_LINK(attr_idx)) {
               attr_idx = AT_ATTR_LINK(attr_idx);
               AT_LOCKED_IN(attr_idx) = TRUE;
            }

            IL_IDX(list2_idx) = attr_idx;

            if (list3_idx == NULL_IDX || 
                IL_IDX(list3_idx) != attr_idx) {
               find_opnd_line_and_column(&IL_OPND(list2_idx), &line, &column);

               PRINTMSG(line, 1417, Error, column);
               break;
            }
            list2_idx = IL_NEXT_LIST_IDX(list2_idx);
            list3_idx = IL_NEXT_LIST_IDX(list3_idx);
         }


         list_idx = list_array[MP_DIR_THREAD_DATA_IDX];

# ifdef _DEBUG
         if (IL_FLD(list_idx) == NO_Tbl_Idx ||
             IL_FLD(list_array[MP_DIR_IS_THREAD_IDX]) != CN_Tbl_Idx) {

            PRINTMSG(stmt_start_line, 626, Internal, stmt_start_col,
                     "THREAD/DATA list item", "mp_directive_semantics");
         }
# endif

         if (compare_cn_and_value(IL_IDX(list_array[MP_DIR_IS_THREAD_IDX]),
                                  0,
                                  Eq_Opr)) {
            /* DATA */
            COPY_OPND(opnd, IL_OPND(list_idx));
            exp_desc.rank = 0;
            xref_state = CIF_Symbol_Reference;
            expr_semantics(&opnd, &exp_desc);
            COPY_OPND(IL_OPND(list_idx), opnd);

            if (! exp_desc.array_elt) {
               /* error, must be array element */
               find_opnd_line_and_column(&opnd, &line, &column);

               PRINTMSG(line, 1372, Error, column);
            }

            list2_idx = list_array[MP_DIR_ONTO_IDX];
            if (IL_FLD(list2_idx) != NO_Tbl_Idx) {
               /* can't have ONTO with DATA affinity. */
               find_opnd_line_and_column(&IL_OPND(list2_idx), &line, &column);

               PRINTMSG(line, 1418, Error, column);
            }
         }
         else {
            /* THREAD */
            COPY_OPND(opnd, IL_OPND(list_idx));
            exp_desc.rank = 0;
            xref_state = CIF_Symbol_Reference;
            expr_semantics(&opnd, &exp_desc);
            COPY_OPND(IL_OPND(list_idx), opnd);

            if (exp_desc.type != Integer ||
                exp_desc.rank != 0) {
               /* error, must be array element */
               find_opnd_line_and_column(&opnd, &line, &column);

               PRINTMSG(line, 1371, Error, column);
            }
         }
      }
   }

   if (clause_allowed[directive][Onto_Clause]) {
      /* process ONTO var list */

      list_idx = list_array[MP_DIR_ONTO_IDX];

      if (IL_FLD(list_idx) == IL_Tbl_Idx) {
	 list_idx = IL_IDX(list_idx);

	 while (list_idx != NULL_IDX) {

	    COPY_OPND(opnd, IL_OPND(list_idx));
	    exp_desc.rank = 0;
	    xref_state = CIF_Symbol_Reference;
	    expr_semantics(&opnd, &exp_desc);
	    COPY_OPND(IL_OPND(list_idx), opnd);

	    find_opnd_line_and_column(&opnd, &line, &column);

	    if (OPND_FLD(opnd) != CN_Tbl_Idx ||
                exp_desc.type != Integer) {
	       /* error, must be a constant */
	       PRINTMSG(line, 1368, Error, column);
	    }
	    else if (compare_cn_and_value(OPND_IDX(opnd),
					  0,
					  Lt_Opr)) {

	       /* error, must be greater than zero */
	       PRINTMSG(line, 1368, Error, column);
	    }

	    list_idx = IL_NEXT_LIST_IDX(list_idx);
	 }
      }
   }

 
   curr_stmt_sh_idx = save_curr_stmt_sh_idx;

   /* restore error flag on curr_stmt_sh_idx. */
   SH_ERR_FLG(orig_sh_idx) = save_error_flag;


   TRACE (Func_Exit, "mp_directive_semantics", NULL);

   return;

}  /* mp_directive_semantics */

/******************************************************************************\
|*                                                                            *|
|* Description:                                                               *|
|*      The ir looks like this coming in ...                                  *|
|*                                                                            *|
|*                        (mp_directive_opr)                                  *|
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
|*                      |- ORDERED constant (ORDERED == 1, else NO_Tbl_Idx)   *|
|*                                                                            *|
|*      Not all clauses are valid for all directives.                         *|
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

static void set_mp_task_flags(int       ir_idx,
                              boolean   flag)

{
   int                  attr_idx;
#ifdef KEY /* Bug 10177 */
   mp_directive_type    directive = Doacross;
#else /* KEY Bug 10177 */
   mp_directive_type    directive;
#endif /* KEY Bug 10177 */
   int                  i;
   int                  list_array[MP_DIR_LIST_CNT];
   int                  list_idx;
   int                  list2_idx;


   TRACE (Func_Entry, "set_mp_task_flags", NULL);

   list_idx = IR_IDX_L(ir_idx);

   for (i = 0; i < MP_DIR_LIST_CNT; i++) {
      list_array[i] = list_idx;
      list_idx = IL_NEXT_LIST_IDX(list_idx);
   }

   switch (IR_OPR(ir_idx)) {
   case Pdo_Par_Opr:
      directive = Pdo;
      break;

   case Parallel_Par_Opr:
      directive = Parallel;
      break;

   case Psection_Par_Opr:
      directive = Psection;
      break;

   case Singleprocess_Par_Opr:
      directive = Singleprocess;
      break;

   default:
# ifdef _DEBUG
      PRINTMSG(IR_LINE_NUM(ir_idx), 626, Internal, IR_COL_NUM(ir_idx),
               "valid parallel region operator", "set_mp_task_flags");
# endif
      break;
   }


   if (clause_allowed[directive][Share_Clause]) {
      /* process SHARED var list */

      list_idx = list_array[MP_DIR_SHARE_IDX];

      cdir_switches.shared_list_idx = (flag ? list_idx : NULL_IDX) ;

      if (IL_FLD(list_idx) != NO_Tbl_Idx) {

         list2_idx = IL_IDX(list_idx);

         while (list2_idx) {

            if (IL_FLD(list2_idx) == AT_Tbl_Idx &&
                AT_OBJ_CLASS(IL_IDX(list2_idx)) == Data_Obj) {

               ATD_TASK_SHARED(IL_IDX(list2_idx)) = flag;
            }

            list2_idx = IL_NEXT_LIST_IDX(list2_idx);
         }
      }
   }


   if (clause_allowed[directive][Lastlocal_Clause]) {
      /* process LASTLOCAL var list */

      list_idx = list_array[MP_DIR_LASTLOCAL_IDX];

      cdir_switches.lastlocal_list_idx = (flag ? list_idx : NULL_IDX) ;

      if (IL_FLD(list_idx) != NO_Tbl_Idx) {

         list2_idx = IL_IDX(list_idx);

         while (list2_idx) {

            if (IL_FLD(list2_idx) == AT_Tbl_Idx &&
                AT_OBJ_CLASS(IL_IDX(list2_idx)) == Data_Obj) {

               ATD_TASK_LASTLOCAL(IL_IDX(list2_idx)) = flag;
            }

            list2_idx = IL_NEXT_LIST_IDX(list2_idx);
         }
      }
   }

   if (clause_allowed[directive][Local_Clause]) {
      /* process LOCAL var list */

      list_idx = list_array[MP_DIR_LOCAL_IDX];

      cdir_switches.private_list_idx = (flag ? list_idx : NULL_IDX) ;

      if (IL_FLD(list_idx) != NO_Tbl_Idx) {

         list2_idx = IL_IDX(list_idx);

         while (list2_idx) {

            if (IL_FLD(list2_idx) == AT_Tbl_Idx &&
                AT_OBJ_CLASS(IL_IDX(list2_idx)) == Data_Obj) {

               ATD_TASK_PRIVATE(IL_IDX(list2_idx)) = flag;
            }

            list2_idx = IL_NEXT_LIST_IDX(list2_idx);
         }
      }
   }

   if (clause_allowed[directive][Lastthread_Clause]) {
      /* process LASTTHREAD var list */

      list_idx = list_array[MP_DIR_LASTTHREAD_IDX];

      cdir_switches.lastthread_list_idx = (flag ? list_idx : NULL_IDX) ;

      if (IL_FLD(list_idx) != NO_Tbl_Idx) {

         list2_idx = IL_IDX(list_idx);

         while (list2_idx) {

            if (IL_FLD(list2_idx) == AT_Tbl_Idx &&
                AT_OBJ_CLASS(IL_IDX(list2_idx)) == Data_Obj) {

               ATD_TASK_LASTTHREAD(IL_IDX(list2_idx)) = flag;
            }

            list2_idx = IL_NEXT_LIST_IDX(list2_idx);
         }
      }
   }

   if (clause_allowed[directive][Reduction_Clause]) {
      /* process REDUCTION var list */

      list_idx = list_array[MP_DIR_REDUCTION_IDX];

      cdir_switches.reduction_list_idx = (flag ? list_idx : NULL_IDX) ;

      if (IL_FLD(list_idx) != NO_Tbl_Idx) {

         list2_idx = IL_IDX(list_idx);

         while (list2_idx) {

            attr_idx = find_left_attr(&IL_OPND(list2_idx));
            ATD_TASK_REDUCTION(attr_idx) = flag;
            list2_idx = IL_NEXT_LIST_IDX(list2_idx);
         }
      }
   }

   cdir_switches.parallel_region   = flag;

   TRACE (Func_Exit, "set_mp_task_flags", NULL);

   return;

}  /* set_mp_task_flags */

/******************************************************************************\
|*									      *|
|* Description:								      *|
|*      This routine handles semantics for the PREFETCH_REF directive.        *|
|*      The incoming ir looks like ...                                        *|
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

static void prefetch_ref_semantics(void)

{
   int          	column;
   expr_arg_type	exp_desc;
   int          	i;
   int          	ir_idx;
   int          	line;
   int          	list_array[5];
   int          	list_idx;
   opnd_type    	opnd;


   TRACE (Func_Entry, "prefetch_ref_semantics", NULL);

   ir_idx = SH_IR_IDX(curr_stmt_sh_idx);

   list_idx = IR_IDX_L(ir_idx);

   for (i = 0; i < 5; i++) {
      list_array[i] = list_idx;
      list_idx = IL_NEXT_LIST_IDX(list_idx);
   }

   /* array ref */

   if (IL_FLD(list_array[0]) != NO_Tbl_Idx) {
      COPY_OPND(opnd, IL_OPND(list_array[0]));
      xref_state = CIF_Symbol_Reference;
      exp_desc.rank = 0;
      expr_semantics(&opnd, &exp_desc);
      COPY_OPND(IL_OPND(list_array[0]), opnd);
   }

   /* stride (optional) */

   if (IL_FLD(list_array[1]) == IL_Tbl_Idx) {
      list_idx = IL_IDX(list_array[1]);

      while (list_idx != NULL_IDX) {
         COPY_OPND(opnd, IL_OPND(list_idx));
         xref_state = CIF_Symbol_Reference;
         exp_desc.rank = 0;
         expr_semantics(&opnd, &exp_desc);
         COPY_OPND(IL_OPND(list_idx), opnd);

         list_idx = IL_NEXT_LIST_IDX(list_idx);
      }
   }
   else {
      /* default = 1 */
      NTR_IR_LIST_TBL(list_idx);
      IL_FLD(list_idx) = CN_Tbl_Idx;
      IL_IDX(list_idx) = CN_INTEGER_ONE_IDX;
      IL_LINE_NUM(list_idx) = stmt_start_line;
      IL_COL_NUM(list_idx) = stmt_start_col;

      IL_FLD(list_array[1]) = IL_Tbl_Idx;
      IL_IDX(list_array[1]) = list_idx;
      IL_LIST_CNT(list_array[1]) = 1;
   }

   /* level (optional) */

   if (IL_FLD(list_array[2]) == IL_Tbl_Idx) {
      list_idx = IL_IDX(list_array[2]);

      while (list_idx != NULL_IDX) {
         COPY_OPND(opnd, IL_OPND(list_idx));
         xref_state = CIF_Symbol_Reference;
         exp_desc.rank = 0;
         expr_semantics(&opnd, &exp_desc);
         COPY_OPND(IL_OPND(list_idx), opnd);

         if (OPND_FLD(opnd) != CN_Tbl_Idx ||
             (compare_cn_and_value(OPND_IDX(opnd),
                                   1,
                                   Ne_Opr) &&
              compare_cn_and_value(OPND_IDX(opnd),
                                   2,
                                   Ne_Opr))) {

            find_opnd_line_and_column(&IL_OPND(list_idx), &line, &column);
            PRINTMSG(line, 1384, Error, column);
         }

         list_idx = IL_NEXT_LIST_IDX(list_idx);
      }
   }
   else {
      /* default = 2 */
      NTR_IR_LIST_TBL(list_idx);
      IL_FLD(list_idx) = CN_Tbl_Idx;
      IL_IDX(list_idx) = CN_INTEGER_TWO_IDX;
      IL_LINE_NUM(list_idx) = stmt_start_line;
      IL_COL_NUM(list_idx) = stmt_start_col;

      IL_FLD(list_array[2]) = IL_Tbl_Idx;
      IL_IDX(list_array[2]) = list_idx;
      IL_LIST_CNT(list_array[2]) = 1;
   }

   /* don't need to look at KIND */

   /* size (optional) */

   if (IL_FLD(list_array[4]) != NO_Tbl_Idx) {
      COPY_OPND(opnd, IL_OPND(list_array[4]));
      xref_state = CIF_Symbol_Reference;
      exp_desc.rank = 0;
      expr_semantics(&opnd, &exp_desc);
      COPY_OPND(IL_OPND(list_array[4]), opnd);

      if (OPND_FLD(opnd) != CN_Tbl_Idx) {
         find_opnd_line_and_column(&opnd, &line, &column);
         PRINTMSG(line, 1383, Error, column, "PREFETCH_REF");
      }
   }


   TRACE (Func_Exit, "prefetch_ref_semantics", NULL);

   return;

}  /* prefetch_ref_semantics */

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
 
void doall_end_semantics(void)

{
   int		attr_idx;
   int		list_idx;
   opnd_type	opnd;

   TRACE (Func_Entry, "doall_end_semantics", NULL);

   cdir_switches.no_internal_calls = FALSE;
   cdir_switches.parallel_region   = FALSE;
   cdir_switches.autoscope         = FALSE;

   if (cdir_switches.private_list_idx &&
       IL_FLD(cdir_switches.private_list_idx) != NO_Tbl_Idx) {

      list_idx = IL_IDX(cdir_switches.private_list_idx);

      while (list_idx) {
         if (IL_FLD(list_idx) == AT_Tbl_Idx &&
             AT_OBJ_CLASS(IL_IDX(list_idx)) == Data_Obj) {

            ATD_TASK_PRIVATE(IL_IDX(list_idx)) = FALSE;
         }
         list_idx = IL_NEXT_LIST_IDX(list_idx);
      }
   }

   if (cdir_switches.shared_list_idx &&
       IL_FLD(cdir_switches.shared_list_idx) != NO_Tbl_Idx) {

      list_idx = IL_IDX(cdir_switches.shared_list_idx);

      while (list_idx) {
         if (IL_FLD(list_idx) == AT_Tbl_Idx &&
             AT_OBJ_CLASS(IL_IDX(list_idx)) == Data_Obj) {

            ATD_TASK_SHARED(IL_IDX(list_idx)) = FALSE;
         }
         else if (IL_FLD(list_idx) == AT_Tbl_Idx             &&
                  AT_OBJ_CLASS(IL_IDX(list_idx)) == Pgm_Unit &&
                  ATP_PROC(IL_IDX(list_idx)) == Dummy_Proc)  {

            ATP_TASK_SHARED(IL_IDX(list_idx)) = FALSE;
         }
         list_idx = IL_NEXT_LIST_IDX(list_idx);
      }
   }

   if (cdir_switches.getfirst_list_idx &&
       IL_FLD(cdir_switches.getfirst_list_idx) != NO_Tbl_Idx) {

      list_idx = IL_IDX(cdir_switches.getfirst_list_idx);

      while (list_idx) {
         if (IL_FLD(list_idx) == AT_Tbl_Idx &&
             AT_OBJ_CLASS(IL_IDX(list_idx)) == Data_Obj) {

            ATD_TASK_GETFIRST(IL_IDX(list_idx)) = FALSE;
         }
         list_idx = IL_NEXT_LIST_IDX(list_idx);
      }
   }

   if (cdir_switches.lastlocal_list_idx &&
       IL_FLD(cdir_switches.lastlocal_list_idx) != NO_Tbl_Idx) {

      list_idx = IL_IDX(cdir_switches.lastlocal_list_idx);

      while (list_idx) {
         if (IL_FLD(list_idx) == AT_Tbl_Idx &&
             AT_OBJ_CLASS(IL_IDX(list_idx)) == Data_Obj) {

            ATD_TASK_LASTLOCAL(IL_IDX(list_idx)) = FALSE;
         }
         list_idx = IL_NEXT_LIST_IDX(list_idx);
      }
   }

   if (cdir_switches.reduction_list_idx &&
       IL_FLD(cdir_switches.reduction_list_idx) != NO_Tbl_Idx) {

      list_idx = IL_IDX(cdir_switches.reduction_list_idx);

      while (list_idx) {
         if (IL_FLD(list_idx) == AT_Tbl_Idx &&
             AT_OBJ_CLASS(IL_IDX(list_idx)) == Data_Obj) {

            ATD_TASK_REDUCTION(IL_IDX(list_idx)) = FALSE;
         }
         list_idx = IL_NEXT_LIST_IDX(list_idx);
      }
   }

   if (cdir_switches.lastthread_list_idx &&
       IL_FLD(cdir_switches.lastthread_list_idx) != NO_Tbl_Idx) {

      COPY_OPND(opnd, IL_OPND(cdir_switches.lastthread_list_idx));
      attr_idx = find_left_attr(&opnd);
      ATD_TASK_REDUCTION(attr_idx) = FALSE;
   }

   cdir_switches.getfirst_list_idx = NULL_IDX;
   cdir_switches.private_list_idx = NULL_IDX;
   cdir_switches.shared_list_idx  = NULL_IDX;
   cdir_switches.lastlocal_list_idx  = NULL_IDX;
   cdir_switches.reduction_list_idx  = NULL_IDX;
   cdir_switches.lastthread_list_idx  = NULL_IDX;

   wait_send_semantics();

   TRACE (Func_Exit, "doall_end_semantics", NULL);

   return;

}  /* doall_end_semantics */

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

static boolean power_o_two(int	idx)

{
   int		i;
   int		k;
   int		cnt = 0;
   long_type	the_constant;
   int		words;


   TRACE (Func_Entry, "power_o_two", NULL);

# ifdef _DEBUG
   if (TYP_TYPE(CN_TYPE_IDX(idx)) != Integer) {
      PRINTMSG(stmt_start_line, 626, Internal, stmt_start_col,
               "Integer constant", "power_o_two");
   }
# endif


   /* BRIANJ KAYKAY */
   words = num_host_wds[TYP_LINEAR(CN_TYPE_IDX(idx))];

   for (k = 0; k < words; k++) {
      the_constant = CP_CONSTANT(CN_POOL_IDX(idx) + k);

      for (i = 0; i < TARGET_BITS_PER_WORD; i++) {
         if (((the_constant >> i) & 1) != 0) {
            cnt++;
         }
      }
   }
   
   TRACE (Func_Exit, "power_o_two", NULL);

   return(cnt == 1);

}  /* power_o_two */

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

static boolean assert_semantics(void)

{
   int			attr_idx;
   int			ir_idx;
   int			list_idx;
   boolean		ok = TRUE;


   TRACE (Func_Entry, "assert_semantics", NULL);

   ir_idx = SH_IR_IDX(curr_stmt_sh_idx);

   switch (CN_INT_TO_C(IR_IDX_L(ir_idx))) {
      case ASSERT_NORECURRENCE:
         list_idx = IR_IDX_R(ir_idx);
         while (list_idx) {
            attr_idx = IL_IDX(list_idx);
            AT_LOCKED_IN(attr_idx) = TRUE;

            while (AT_ATTR_LINK(attr_idx)) {
               attr_idx = AT_ATTR_LINK(attr_idx);
               AT_LOCKED_IN(attr_idx) = TRUE;
            }

            IL_IDX(list_idx) = attr_idx;

            list_idx = IL_NEXT_LIST_IDX(list_idx);
         }
         break;

      case ASSERT_DOPREFER:
      case ASSERT_DO:
         break;

      case ASSERT_PERMUTATION:
#ifdef KEY /* Bug 12497 */
         if (IR_FLD_R(ir_idx) == IL_Tbl_Idx) {
	    attr_idx = IL_IDX(IR_IDX_R(ir_idx));
	 }
	 else
#endif /* KEY Bug 12497 */
         attr_idx = IR_IDX_R(ir_idx);
         while (AT_ATTR_LINK(attr_idx)) {
            attr_idx = AT_ATTR_LINK(attr_idx);
            AT_LOCKED_IN(attr_idx) = TRUE;
         }

#ifdef KEY /* Bug 12497 */
         if (IR_FLD_R(ir_idx) == IL_Tbl_Idx) {
	    IL_IDX(IR_IDX_R(ir_idx)) = attr_idx;
	 }
	 else
#endif /* KEY Bug 12497 */
         IR_IDX_R(ir_idx) = attr_idx;
         break;

      case ASSERT_ARGUMENTALIASING:
      case ASSERT_NOARGUMENTALIASING:
      case ASSERT_BOUNDSVIOLATIONS:
      case ASSERT_NOBOUNDSVIOLATIONS:
      case ASSERT_CONCURRENTCALL:
      case ASSERT_NOCONCURRENTCALL:
      case ASSERT_EQUIVALENCEHAZARD:
      case ASSERT_NOEQUIVALENCEHAZARD:
      case ASSERT_LASTVALUENEEDED:
      case ASSERT_LASTVALUESNEEDED:
      case ASSERT_NOLASTVALUENEEDED:
      case ASSERT_NOLASTVALUESNEEDED:
      case ASSERT_RELATION:
      case ASSERT_NOSYNC:
      case ASSERT_TEMPORARIESFORCONSTANTARGUMENTS:
      case ASSERT_NOTEMPORARIESFORCONSTANTARGUMENTS:
      case ASSERT_BENIGN:
      case ASSERT_DEPENDENCE:
      case ASSERT_FREQUENCY:
      case ASSERT_IGNOREANYDEPENDENCES:
      case ASSERT_IGNOREANYDEPENDENCE:
      case ASSERT_IGNOREASSUMEDDEPENDENCES:
      case ASSERT_IGNOREASSUMEDDEPENDENCE:
      case ASSERT_NOINTERCHANGE:
      case ASSERT_USECOMPRESS:
      case ASSERT_USEEXPAND:
      case ASSERT_USECONTROLLEDSTORE:
      case ASSERT_USEGATHER:
      case ASSERT_USESCATTER:
         /* intentionally blank */
         break;
   }

   TRACE (Func_Exit, "assert_semantics", NULL);

   return(ok);

}  /* assert_semantics */

/******************************************************************************\
|*									      *|
|* Description:								      *|
|*      The ir looks like this coming in ...                                  *|
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
|*                      |- COPYPRIVATE var list                               *|
|*                      |- SCHEDULE type (CN_Tbl_Idx)                         *|
|*                      |- SCHEDULE chunk (CN_Tbl_Idx)                        *|
|*                      |- AFFINITY index_var list                            *|
|*                      |- IS THREAD constant (THREAD == 1, DATA == 0)        *|
|*                      |- THREAD/DATA list                                   *|
|*                      |- ONTO list                                          *|
|*                      |- NEST list                                          *|
|*									      *|
|*	Not all clauses are valid for all directives.                         *|
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

static void open_mp_directive_semantics(open_mp_directive_type directive)

{
   int			attr_idx;
   int			column;
   expr_arg_type	exp_desc;
   int			i;
   int			idx;
   int			ir_idx;
   int			line;
   int          	list_array[OPEN_MP_LIST_CNT];
   int			list_idx;
   int			list2_idx;
   int			list3_idx;
   opnd_type		l_opnd;
   boolean		ok;
   opnd_type		opnd;
   int			orig_sh_idx;
   int			save_curr_stmt_sh_idx;
   boolean		save_error_flag;
   boolean		work_sharing_dir = FALSE;
   long64		value;


   TRACE (Func_Entry, "open_mp_directive_semantics", NULL);


   ir_idx = SH_IR_IDX(curr_stmt_sh_idx);
   save_curr_stmt_sh_idx = curr_stmt_sh_idx;
   orig_sh_idx = curr_stmt_sh_idx;
   save_error_flag = SH_ERR_FLG(curr_stmt_sh_idx);

   list_idx = IR_IDX_L(ir_idx);

   for (i = 0; i < OPEN_MP_LIST_CNT; i++) {
      list_array[i] = list_idx;
      list_idx = IL_NEXT_LIST_IDX(list_idx);
   }

   if (directive == Do_Omp ||
       directive == Sections_Omp ||
       directive == Single_Omp ||
       directive == Workshare_Omp) {
      work_sharing_dir = TRUE;
   }
    
   if (directive == Do_Omp ||
       directive == Parallel_Do_Omp) {

     /* pull stmt header out of list */
      remove_sh(curr_stmt_sh_idx);
      save_curr_stmt_sh_idx = SH_PREV_IDX(curr_stmt_sh_idx);

      switch (directive) {
      case Do_Omp:
         cdir_switches.do_omp_sh_idx = curr_stmt_sh_idx;
         SH_PREV_IDX(cdir_switches.do_omp_sh_idx) = NULL_IDX;
         SH_NEXT_IDX(cdir_switches.do_omp_sh_idx) = NULL_IDX;
         break;

      case Parallel_Do_Omp:
         cdir_switches.paralleldo_omp_sh_idx = curr_stmt_sh_idx;
         SH_PREV_IDX(cdir_switches.paralleldo_omp_sh_idx) = NULL_IDX;
         SH_NEXT_IDX(cdir_switches.paralleldo_omp_sh_idx) = NULL_IDX;
         break;
      }
   }

   /* process the clauses that capture an expression with create_tmp_asg */
   /* first (before push_task_blk) so that any temps get placed on the   */
   /* private (or shared) lists of containing parallel blocks.           */

   if (open_mp_clause_allowed[directive][If_Omp_Clause]) {
      list_idx = list_array[OPEN_MP_IF_IDX];

      /* process IF condition */

      if (IL_FLD(list_idx) != NO_Tbl_Idx) {
         COPY_OPND(opnd, IL_OPND(list_idx));
         exp_desc.rank = 0;
         xref_state = CIF_Symbol_Reference;
         expr_semantics(&opnd, &exp_desc);

         find_opnd_line_and_column(&opnd, &line, &column);
         if (exp_desc.type != Logical ||
             exp_desc.rank != 0)      {
            PRINTMSG(line, 1511, Error, column);
         }

         IL_FLD(list_idx) = AT_Tbl_Idx;
         idx = create_tmp_asg(&opnd,
                              &exp_desc,
                              &l_opnd,
                              Intent_In,
                              FALSE,
                              FALSE);
         IL_IDX(list_idx) = idx;
         IL_LINE_NUM(list_idx) = line;
         IL_COL_NUM(list_idx) = column;
      }
   }

   /* by jhs, 02/7/20 */
   if (open_mp_clause_allowed[directive][Num_Threads_Omp_Clause]) {
      list_idx = list_array[OPEN_MP_NUM_THREADS];

      /* process NUM_THREADS expression */

      if (IL_FLD(list_idx) != NO_Tbl_Idx) {
         COPY_OPND(opnd, IL_OPND(list_idx));
         exp_desc.rank = 0;
         xref_state = CIF_Symbol_Reference;
         expr_semantics(&opnd, &exp_desc);

         find_opnd_line_and_column(&opnd, &line, &column);
         if (exp_desc.type != Integer ||
             exp_desc.rank != 0)      {
            PRINTMSG(line, 1672, Error, column);
         }
         else if (OPND_FLD(opnd) == CN_Tbl_Idx &&
                     compare_cn_and_value(OPND_IDX(opnd),
                                          0,
                                          Le_Opr)) {

               PRINTMSG(line, 1673, Error, column);
         }

         IL_FLD(list_idx) = AT_Tbl_Idx;
         idx = create_tmp_asg(&opnd,
                              &exp_desc,
                              &l_opnd,
                              Intent_In,
                              FALSE,
                              FALSE);
         IL_IDX(list_idx) = idx;
         IL_LINE_NUM(list_idx) = line;
         IL_COL_NUM(list_idx) = column;
      }
      else if (cdir_switches.maxcpus) {
         COPY_OPND(IL_OPND(list_idx), cdir_switches.maxcpus_opnd);
         cdir_switches.maxcpus      = FALSE;
      }
   }

   if (open_mp_clause_allowed[directive][Schedule_Omp_Clause]) {
      /* process SCHEDULE CHUNK expression */

      list_idx	= list_array[OPEN_MP_SCHEDULE_CHUNK_IDX];
      list2_idx = list_array[OPEN_MP_SCHEDULE_TYPE_IDX];

      if (IL_FLD(list2_idx) != NO_Tbl_Idx) {
         value	= CN_INT_TO_C(IL_IDX(list2_idx));

         switch (value) {
            case OPEN_MP_SCHEDULE_STATIC:
               break;

            case OPEN_MP_SCHEDULE_DYNAMIC:
               if (IL_FLD(list_idx) == NO_Tbl_Idx) {
                  IL_FLD(list_idx) = CN_Tbl_Idx;
                  IL_IDX(list_idx) = CN_INTEGER_ONE_IDX;
                  IL_LINE_NUM(list_idx) = IL_LINE_NUM(list2_idx);
                  IL_COL_NUM(list_idx) = IL_COL_NUM(list2_idx);
               }
               break;

            case OPEN_MP_SCHEDULE_GUIDED:
               if (IL_FLD(list_idx) == NO_Tbl_Idx) {
                  IL_FLD(list_idx) = CN_Tbl_Idx;
                  IL_IDX(list_idx) = CN_INTEGER_ONE_IDX;
                  IL_LINE_NUM(list_idx) = IL_LINE_NUM(list2_idx);
                  IL_COL_NUM(list_idx) = IL_COL_NUM(list2_idx);
               }
               break;

            case OPEN_MP_SCHEDULE_RUNTIME:
               if (IL_FLD(list_idx) != NO_Tbl_Idx) {
                  find_opnd_line_and_column(&IL_OPND(list_idx), &line, &column);
                  PRINTMSG(line, 1475, Error, column);
               }
               break;

         }

         if (IL_FLD(list_idx) != NO_Tbl_Idx) {
            COPY_OPND(opnd, IL_OPND(list_idx));
            exp_desc.rank = 0;
            xref_state = CIF_Symbol_Reference;
            ok = expr_semantics(&opnd, &exp_desc);

            find_opnd_line_and_column(&opnd, &line, &column);

            if (exp_desc.type != Integer ||
                exp_desc.rank != 0)      {
               PRINTMSG(line, 1364, Error, column);
            }
            else if (OPND_FLD(opnd) == CN_Tbl_Idx &&
                     compare_cn_and_value(OPND_IDX(opnd),
                                          0,
                                          Le_Opr)) {

               PRINTMSG(line, 1560, Error, column);
            }

            IL_FLD(list_idx) = AT_Tbl_Idx;
            idx = create_tmp_asg(&opnd,
                                 &exp_desc,
                                 &l_opnd,
                                 Intent_In,
                                 FALSE,
                                 FALSE);
            IL_IDX(list_idx) = idx;
            IL_LINE_NUM(list_idx) = line;
            IL_COL_NUM(list_idx) = column;
         }
      }
   }

   if (directive != Do_Omp &&
       directive != Parallel_Do_Omp) {
      cdir_switches.parallel_region = TRUE;
   }

   push_task_blk(curr_stmt_sh_idx);

   if (open_mp_clause_allowed[directive][Shared_Omp_Clause]) {
      /* process SHARED var list */
    
      list_idx = list_array[OPEN_MP_SHARED_IDX];
      cdir_switches.shared_list_idx = list_idx;
    
      if (IL_FLD(list_idx) != NO_Tbl_Idx) {
    
	 list2_idx = IL_IDX(list_idx);
    
	 while (list2_idx) {
    
#ifdef KEY /* Bug 6075 */
            if (IL_FLD(list2_idx) == AT_Tbl_Idx) {
#endif /* KEY Bug 6075 */

	       attr_idx = IL_IDX(list2_idx);
	       AT_LOCKED_IN(attr_idx) = TRUE;
       
	       while (AT_ATTR_LINK(attr_idx)) {
		  attr_idx = AT_ATTR_LINK(attr_idx);
		  AT_LOCKED_IN(attr_idx) = TRUE;
	       }
       
	       IL_IDX(list2_idx) = attr_idx;

       
	       if (AT_OBJ_CLASS(attr_idx) == Pgm_Unit   &&
		   ATP_PROC(attr_idx)     == Dummy_Proc) {
		  ATP_TASK_SHARED(attr_idx) = TRUE;
	       }
	       else if (AT_OBJ_CLASS(attr_idx) != Data_Obj) {
		  PRINTMSG(IL_LINE_NUM(list2_idx), 1473, Error,
			   IL_COL_NUM(list2_idx), 
			   AT_OBJ_NAME_PTR(attr_idx),
			   "SHARED", open_mp_dir_str[directive]);
	       }
	       else if (ATD_CLASS(attr_idx) == Constant) {
		  PRINTMSG(IL_LINE_NUM(list2_idx), 1473, Error,
			   IL_COL_NUM(list2_idx), 
			   AT_OBJ_NAME_PTR(attr_idx),
			   "SHARED", open_mp_dir_str[directive]);
	       }
	       else if (AT_OBJ_CLASS(attr_idx) == Data_Obj &&
			ATD_CLASS(attr_idx) == CRI__Pointee) {
		  PRINTMSG(IL_LINE_NUM(list2_idx), 1477, Error,
			   IL_COL_NUM(list2_idx),
			   AT_OBJ_NAME_PTR(attr_idx));
	       }
	       else if (multiple_clause_err(attr_idx, OPEN_MP_SHARED_IDX)) {
		  PRINTMSG(IL_LINE_NUM(list2_idx), 1476, Error,
			   IL_COL_NUM(list2_idx), 
			   AT_OBJ_NAME_PTR(attr_idx));
	       }
	       else {
		  ATD_TASK_SHARED(attr_idx) = TRUE;
		  ATD_WAS_SCOPED(attr_idx) = TRUE;

		  if (ATD_CLASS(attr_idx) == Variable &&
		      ATD_AUTOMATIC(attr_idx) &&
		      ATD_AUTO_BASE_IDX(attr_idx) != NULL_IDX &&
		      ! ATD_TASK_SHARED(ATD_AUTO_BASE_IDX(attr_idx))) {

		     ATD_TASK_SHARED(ATD_AUTO_BASE_IDX(attr_idx)) = TRUE;

		     NTR_IR_LIST_TBL(list3_idx);
		     IL_PREV_LIST_IDX(IL_IDX(list_idx)) = list3_idx;
		     IL_NEXT_LIST_IDX(list3_idx) = IL_IDX(list_idx);
		     IL_IDX(list_idx) = list3_idx;
		     IL_LIST_CNT(list_idx)++;

		     IL_FLD(list3_idx) = AT_Tbl_Idx;
		     IL_IDX(list3_idx) = ATD_AUTO_BASE_IDX(attr_idx);
		     IL_LINE_NUM(list3_idx) = IL_LINE_NUM(list2_idx);
		     IL_COL_NUM(list3_idx) = IL_COL_NUM(list2_idx);
		  }
	       }
#ifdef KEY /* Bug 6075 */
            }
	    else {
               /* SB_Tbl_Idx here */
               add_common_blk_objects_to_list(list2_idx, list_idx);
	    }
#endif /* KEY Bug 6075 */
    
	    list2_idx = IL_NEXT_LIST_IDX(list2_idx);
	 }
      }
   }
 
   if (open_mp_clause_allowed[directive][Private_Omp_Clause]) {
      /* process PRIVATE var list */
    
      list_idx = list_array[OPEN_MP_PRIVATE_IDX];
      cdir_switches.private_list_idx = list_idx;
    
      if (IL_FLD(list_idx) != NO_Tbl_Idx) {
    
	 list2_idx = IL_IDX(list_idx);
    
	 while (list2_idx) {
    
            if (IL_FLD(list2_idx) == AT_Tbl_Idx) {
	       attr_idx = IL_IDX(list2_idx);
	       AT_LOCKED_IN(attr_idx) = TRUE;
       
	       while (AT_ATTR_LINK(attr_idx)) {
		  attr_idx = AT_ATTR_LINK(attr_idx);
		  AT_LOCKED_IN(attr_idx) = TRUE;
	       }
       
	       IL_IDX(list2_idx) = attr_idx;
       
	       if (AT_OBJ_CLASS(attr_idx) != Data_Obj) {
		  PRINTMSG(IL_LINE_NUM(list2_idx), 1473, Error,
			   IL_COL_NUM(list2_idx),
			   AT_OBJ_NAME_PTR(attr_idx),
			   "PRIVATE", open_mp_dir_str[directive]);
	       }
	       else if (ATD_CLASS(attr_idx) == Constant) {
		  PRINTMSG(IL_LINE_NUM(list2_idx), 1473, Error,
			   IL_COL_NUM(list2_idx),
			   AT_OBJ_NAME_PTR(attr_idx),
			   "PRIVATE", open_mp_dir_str[directive]);
	       }
               else if (ATD_CLASS(attr_idx) == CRI__Pointee) {
                  PRINTMSG(IL_LINE_NUM(list2_idx), 1477, Error,
                           IL_COL_NUM(list2_idx),
                           AT_OBJ_NAME_PTR(attr_idx));
               }
               else if (ATD_ARRAY_IDX(attr_idx) != NULL_IDX &&
                        (BD_ARRAY_CLASS(ATD_ARRAY_IDX(attr_idx)) == 
                                                            Assumed_Size ||
                         BD_ARRAY_CLASS(ATD_ARRAY_IDX(attr_idx)) == 
                                                            Assumed_Shape)) {

                  PRINTMSG(IL_LINE_NUM(list2_idx), 1482, Error,
                           IL_COL_NUM(list2_idx),
                           (BD_ARRAY_CLASS(ATD_ARRAY_IDX(attr_idx)) == 
                             Assumed_Size ? "Assumed size" : "Assumed shape"),
                           AT_OBJ_NAME_PTR(attr_idx));
               }
	       else if (multiple_clause_err(attr_idx, OPEN_MP_PRIVATE_IDX)) {
		  PRINTMSG(IL_LINE_NUM(list2_idx), 1476, Error,
			   IL_COL_NUM(list2_idx),
			   AT_OBJ_NAME_PTR(attr_idx));
	       }
               else if (work_sharing_dir &&
                        has_been_reprivatized(attr_idx)) {
                  PRINTMSG(IL_LINE_NUM(list2_idx), 1651, Error,
                           IL_COL_NUM(list2_idx),
                           "Privatized",
                           AT_OBJ_NAME_PTR(attr_idx));
               }
               else if (ATD_CLASS(attr_idx) == Dummy_Argument   &&
                        ATD_INTENT(attr_idx) == Intent_In) {
                  PRINTMSG(IL_LINE_NUM(list2_idx), 1492, Error,
                           IL_COL_NUM(list2_idx),
                           AT_OBJ_NAME_PTR(attr_idx),
                           "PRIVATE");
               }
               else if (ATD_PURE(attr_idx)) {
                  PRINTMSG(IL_LINE_NUM(list2_idx), 1493, Error,
                           IL_COL_NUM(list2_idx),
                           AT_OBJ_NAME_PTR(attr_idx),
                           "PRIVATE");
               }
	       else {
		  ATD_TASK_PRIVATE(attr_idx) = TRUE;
		  ATD_WAS_SCOPED(attr_idx) = TRUE;

                  if (ATD_CLASS(attr_idx) == Variable &&
                      ATD_AUTOMATIC(attr_idx) &&
                      ATD_AUTO_BASE_IDX(attr_idx) != NULL_IDX &&
                      ! ATD_TASK_PRIVATE(ATD_AUTO_BASE_IDX(attr_idx))) {

                     ATD_TASK_PRIVATE(ATD_AUTO_BASE_IDX(attr_idx)) = TRUE;

                     NTR_IR_LIST_TBL(list3_idx);
                     IL_PREV_LIST_IDX(IL_IDX(list_idx)) = list3_idx;
                     IL_NEXT_LIST_IDX(list3_idx) = IL_IDX(list_idx);
                     IL_IDX(list_idx) = list3_idx;
                     IL_LIST_CNT(list_idx)++;

                     IL_FLD(list3_idx) = AT_Tbl_Idx;
                     IL_IDX(list3_idx) = ATD_AUTO_BASE_IDX(attr_idx);
                     IL_LINE_NUM(list3_idx) = IL_LINE_NUM(list2_idx);
                     IL_COL_NUM(list3_idx) = IL_COL_NUM(list2_idx);
                  }
	       }
            }
            else {
               /* SB_Tbl_Idx here */
               add_common_blk_objects_to_list(list2_idx, list_idx);
            }
    
	    list2_idx = IL_NEXT_LIST_IDX(list2_idx);
	 }
      }
   }

   if (open_mp_clause_allowed[directive][Firstprivate_Omp_Clause]) {
      /* process FIRSTPRIVATE var list */
    
      list_idx = list_array[OPEN_MP_FIRSTPRIVATE_IDX];
      cdir_switches.firstprivate_list_idx = list_idx;
    
      if (IL_FLD(list_idx) != NO_Tbl_Idx) {
    
	 list2_idx = IL_IDX(list_idx);
    
	 while (list2_idx) {
    
            if (IL_FLD(list2_idx) == AT_Tbl_Idx) {
	       attr_idx = IL_IDX(list2_idx);
	       AT_LOCKED_IN(attr_idx) = TRUE;
       
	       while (AT_ATTR_LINK(attr_idx)) {
		  attr_idx = AT_ATTR_LINK(attr_idx);
		  AT_LOCKED_IN(attr_idx) = TRUE;
	       }
       
	       IL_IDX(list2_idx) = attr_idx;
       
	       if (AT_OBJ_CLASS(attr_idx) != Data_Obj) {
		  PRINTMSG(IL_LINE_NUM(list2_idx), 1473, Error,
			   IL_COL_NUM(list2_idx),
			   AT_OBJ_NAME_PTR(attr_idx),
			   "FIRSTPRIVATE", open_mp_dir_str[directive]);
	       }
               else if (ATD_CLASS(attr_idx) == Constant) {
                  PRINTMSG(IL_LINE_NUM(list2_idx), 1473, Error,
                           IL_COL_NUM(list2_idx),
                           AT_OBJ_NAME_PTR(attr_idx),
                           "FIRSTPRIVATE", open_mp_dir_str[directive]);
               }
               else if (ATD_CLASS(attr_idx) == CRI__Pointee) {
                  PRINTMSG(IL_LINE_NUM(list2_idx), 1477, Error,
                           IL_COL_NUM(list2_idx),
                           AT_OBJ_NAME_PTR(attr_idx));
               }
               else if (TYP_TYPE(ATD_TYPE_IDX(attr_idx)) == CRI_Ptr ||
                        TYP_TYPE(ATD_TYPE_IDX(attr_idx)) == CRI_Ch_Ptr) {

                  PRINTMSG(IL_LINE_NUM(list2_idx), 1478, Error,
                           IL_COL_NUM(list2_idx),
                           "Cray pointer",
                           AT_OBJ_NAME_PTR(attr_idx));
               }
               else if (ATD_POINTER(attr_idx)) {

                  PRINTMSG(IL_LINE_NUM(list2_idx), 1478, Error,
                           IL_COL_NUM(list2_idx),
                           "Pointer",
                           AT_OBJ_NAME_PTR(attr_idx));
               }
               else if (ATD_ALLOCATABLE(attr_idx)) {

                  PRINTMSG(IL_LINE_NUM(list2_idx), 1478, Error,
                           IL_COL_NUM(list2_idx),
                           "Allocatable array",
                           AT_OBJ_NAME_PTR(attr_idx));
               }
               else if (ATD_ARRAY_IDX(attr_idx) != NULL_IDX &&
                        (BD_ARRAY_CLASS(ATD_ARRAY_IDX(attr_idx)) == 
                                                           Assumed_Size ||
                         BD_ARRAY_CLASS(ATD_ARRAY_IDX(attr_idx)) == 
                                                           Assumed_Shape)) {

                  PRINTMSG(IL_LINE_NUM(list2_idx), 1482, Error,
                           IL_COL_NUM(list2_idx),
                            (BD_ARRAY_CLASS(ATD_ARRAY_IDX(attr_idx)) == 
                                                           Assumed_Size ?
                           "Assumed size" : "Assumed shape"),
                           AT_OBJ_NAME_PTR(attr_idx));
               }
	       else if (multiple_clause_err(attr_idx, 
                                                  OPEN_MP_FIRSTPRIVATE_IDX)) {
		  PRINTMSG(IL_LINE_NUM(list2_idx), 1476, Error,
			   IL_COL_NUM(list2_idx),
			   AT_OBJ_NAME_PTR(attr_idx));
	       }
               else if (work_sharing_dir &&
                        has_been_reprivatized(attr_idx)) {
                  PRINTMSG(IL_LINE_NUM(list2_idx), 1651, Error,
                           IL_COL_NUM(list2_idx),
                           "Privatized",
                           AT_OBJ_NAME_PTR(attr_idx));
               }
               else if (ATD_CLASS(attr_idx) == Dummy_Argument   &&
                        ATD_INTENT(attr_idx) == Intent_In) {
                  PRINTMSG(IL_LINE_NUM(list2_idx), 1492, Error,
                           IL_COL_NUM(list2_idx),
                           AT_OBJ_NAME_PTR(attr_idx),
                           "FIRSTPRIVATE");
               }
               else if (ATD_PURE(attr_idx)) {
                  PRINTMSG(IL_LINE_NUM(list2_idx), 1493, Error,
                           IL_COL_NUM(list2_idx),
                           AT_OBJ_NAME_PTR(attr_idx),
                           "FIRSTPRIVATE");
               }
	       else {
		  ATD_TASK_FIRSTPRIVATE(attr_idx) = TRUE;

                  if (ATD_CLASS(attr_idx) == Variable &&
                      ATD_AUTOMATIC(attr_idx) &&
                      ATD_AUTO_BASE_IDX(attr_idx) != NULL_IDX &&
                      ! ATD_TASK_FIRSTPRIVATE(ATD_AUTO_BASE_IDX(attr_idx))) {

                     ATD_TASK_FIRSTPRIVATE(ATD_AUTO_BASE_IDX(attr_idx)) = TRUE;

                     NTR_IR_LIST_TBL(list3_idx);
                     IL_PREV_LIST_IDX(IL_IDX(list_idx)) = list3_idx;
                     IL_NEXT_LIST_IDX(list3_idx) = IL_IDX(list_idx);
                     IL_IDX(list_idx) = list3_idx;
                     IL_LIST_CNT(list_idx)++;

                     IL_FLD(list3_idx) = AT_Tbl_Idx;
                     IL_IDX(list3_idx) = ATD_AUTO_BASE_IDX(attr_idx);
                     IL_LINE_NUM(list3_idx) = IL_LINE_NUM(list2_idx);
                     IL_COL_NUM(list3_idx) = IL_COL_NUM(list2_idx);
                  }
	       }
            }
            else {
               /* SB_Tbl_Idx here */
               add_common_blk_objects_to_list(list2_idx, list_idx);
            }
    
	    list2_idx = IL_NEXT_LIST_IDX(list2_idx);
	 }
      }
   }

   if (open_mp_clause_allowed[directive][Copyin_Omp_Clause]) {
      /* process COPYIN var list */
    
      list_idx = list_array[OPEN_MP_COPYIN_IDX];
      cdir_switches.copyin_list_idx = list_idx;
    
      if (IL_FLD(list_idx) != NO_Tbl_Idx) {
    
	 list2_idx = IL_IDX(list_idx);
    
	 while (list2_idx) {
    
            if (IL_FLD(list2_idx) == AT_Tbl_Idx) {
	       attr_idx = IL_IDX(list2_idx);
	       AT_LOCKED_IN(attr_idx) = TRUE;
    
	       while (AT_ATTR_LINK(attr_idx)) {
	          attr_idx = AT_ATTR_LINK(attr_idx);
	          AT_LOCKED_IN(attr_idx) = TRUE;
	       }
    
	       IL_IDX(list2_idx) = attr_idx;
    
               if (AT_OBJ_CLASS(attr_idx) != Data_Obj) {
                  PRINTMSG(IL_LINE_NUM(list2_idx), 1473, Error,
                           IL_COL_NUM(list2_idx),
                           AT_OBJ_NAME_PTR(attr_idx),
                           "COPYIN", open_mp_dir_str[directive]);
               }
               else if (ATD_CLASS(attr_idx) == Constant) {
                  PRINTMSG(IL_LINE_NUM(list2_idx), 1473, Error,
                           IL_COL_NUM(list2_idx),
                           AT_OBJ_NAME_PTR(attr_idx),
                           "COPYIN", open_mp_dir_str[directive]);
               }
               else if (ATD_CLASS(attr_idx) == CRI__Pointee) {
                  PRINTMSG(IL_LINE_NUM(list2_idx), 1477, Error,
                           IL_COL_NUM(list2_idx),
                           AT_OBJ_NAME_PTR(attr_idx));
               }
               else if (TYP_TYPE(ATD_TYPE_IDX(attr_idx)) == CRI_Ptr ||
                        TYP_TYPE(ATD_TYPE_IDX(attr_idx)) == CRI_Ch_Ptr) {

                  PRINTMSG(IL_LINE_NUM(list2_idx), 1484, Error,
                           IL_COL_NUM(list2_idx),
                           "Cray pointer",
                           AT_OBJ_NAME_PTR(attr_idx),
                           open_mp_dir_str[directive]);
               }
// Bug 4516
#ifndef KEY
               else if (ATD_POINTER(attr_idx)) {

                  PRINTMSG(IL_LINE_NUM(list2_idx), 1484, Error,
                           IL_COL_NUM(list2_idx),
                           "Pointer",
                           AT_OBJ_NAME_PTR(attr_idx),
                           open_mp_dir_str[directive]);
               }
#endif
               else if (ATD_ALLOCATABLE(attr_idx)) {

                  PRINTMSG(IL_LINE_NUM(list2_idx), 1484, Error,
                           IL_COL_NUM(list2_idx),
                           "Allocatable array",
                           AT_OBJ_NAME_PTR(attr_idx),
                           open_mp_dir_str[directive]);
               }
               else if (multiple_clause_err(attr_idx, OPEN_MP_COPYIN_IDX)) {
                  PRINTMSG(IL_LINE_NUM(list2_idx), 1476, Error,
                           IL_COL_NUM(list2_idx),
                           AT_OBJ_NAME_PTR(attr_idx));
               }
               else {
	          ATD_TASK_COPYIN(attr_idx) = TRUE;

                  if (ATD_CLASS(attr_idx) == Variable &&
                      ATD_AUTOMATIC(attr_idx) &&
                      ATD_AUTO_BASE_IDX(attr_idx) != NULL_IDX &&
                      ! ATD_TASK_COPYIN(ATD_AUTO_BASE_IDX(attr_idx))) {

                     ATD_TASK_COPYIN(ATD_AUTO_BASE_IDX(attr_idx)) = TRUE;

                     NTR_IR_LIST_TBL(list3_idx);
                     IL_PREV_LIST_IDX(IL_IDX(list_idx)) = list3_idx;
                     IL_NEXT_LIST_IDX(list3_idx) = IL_IDX(list_idx);
                     IL_IDX(list_idx) = list3_idx;
                     IL_LIST_CNT(list_idx)++;

                     IL_FLD(list3_idx) = AT_Tbl_Idx;
                     IL_IDX(list3_idx) = ATD_AUTO_BASE_IDX(attr_idx);
                     IL_LINE_NUM(list3_idx) = IL_LINE_NUM(list2_idx);
                     IL_COL_NUM(list3_idx) = IL_COL_NUM(list2_idx);
                  }
	       }
            }
            else {
               /*  SB_Tbl_Idx here */
               add_common_blk_objects_to_list(list2_idx, list_idx);
            }
    
	    list2_idx = IL_NEXT_LIST_IDX(list2_idx);
	 }
      }
   }

   if (open_mp_clause_allowed[directive][Lastprivate_Omp_Clause]) {
      /* process LASTPRIVATE var list */
    
      list_idx = list_array[OPEN_MP_LASTPRIVATE_IDX];
      cdir_switches.lastprivate_list_idx = list_idx;
    
      if (IL_FLD(list_idx) != NO_Tbl_Idx) {
    
	 list2_idx = IL_IDX(list_idx);
    
	 while (list2_idx) {
    
            if (IL_FLD(list2_idx) == AT_Tbl_Idx) {
	       attr_idx = IL_IDX(list2_idx);
	       AT_LOCKED_IN(attr_idx) = TRUE;
       
	       while (AT_ATTR_LINK(attr_idx)) {
	          attr_idx = AT_ATTR_LINK(attr_idx);
	          AT_LOCKED_IN(attr_idx) = TRUE;
	       }
    
	       IL_IDX(list2_idx) = attr_idx;
       
               if (AT_OBJ_CLASS(attr_idx) != Data_Obj) {
                  PRINTMSG(IL_LINE_NUM(list2_idx), 1473, Error,
                           IL_COL_NUM(list2_idx),
                           AT_OBJ_NAME_PTR(attr_idx),
                           "LASTPRIVATE", open_mp_dir_str[directive]);
               }
               else if (ATD_CLASS(attr_idx) == Constant) {
                  PRINTMSG(IL_LINE_NUM(list2_idx), 1473, Error,
                           IL_COL_NUM(list2_idx),
                           AT_OBJ_NAME_PTR(attr_idx),
                           "LASTPRIVATE", open_mp_dir_str[directive]);
               }
               else if (ATD_CLASS(attr_idx) == CRI__Pointee) {
                  PRINTMSG(IL_LINE_NUM(list2_idx), 1477, Error,
                           IL_COL_NUM(list2_idx),
                           AT_OBJ_NAME_PTR(attr_idx));
               }
               else if (TYP_TYPE(ATD_TYPE_IDX(attr_idx)) == CRI_Ptr ||
                        TYP_TYPE(ATD_TYPE_IDX(attr_idx)) == CRI_Ch_Ptr) {

                  PRINTMSG(IL_LINE_NUM(list2_idx), 1478, Error,
                           IL_COL_NUM(list2_idx),
                           "Cray pointer",
                           AT_OBJ_NAME_PTR(attr_idx));
               }
               else if (ATD_POINTER(attr_idx)) {

                  PRINTMSG(IL_LINE_NUM(list2_idx), 1478, Error,
                           IL_COL_NUM(list2_idx),
                           "Pointer",
                           AT_OBJ_NAME_PTR(attr_idx));
               }
               else if (ATD_ALLOCATABLE(attr_idx)) {

                  PRINTMSG(IL_LINE_NUM(list2_idx), 1478, Error,
                           IL_COL_NUM(list2_idx),
                           "Allocatable array",
                           AT_OBJ_NAME_PTR(attr_idx));
               }
               else if (ATD_ARRAY_IDX(attr_idx) != NULL_IDX &&
                        (BD_ARRAY_CLASS(ATD_ARRAY_IDX(attr_idx)) == 
                                                           Assumed_Size ||
                         BD_ARRAY_CLASS(ATD_ARRAY_IDX(attr_idx)) == 
                                                           Assumed_Shape)) {

                  PRINTMSG(IL_LINE_NUM(list2_idx), 1482, Error,
                           IL_COL_NUM(list2_idx),
                            (BD_ARRAY_CLASS(ATD_ARRAY_IDX(attr_idx)) == 
                                                           Assumed_Size ?
                           "Assumed size" : "Assumed shape"),
                           AT_OBJ_NAME_PTR(attr_idx));
               }
               else if (multiple_clause_err(attr_idx, OPEN_MP_LASTPRIVATE_IDX)){
                  PRINTMSG(IL_LINE_NUM(list2_idx), 1476, Error,
                           IL_COL_NUM(list2_idx),
                           AT_OBJ_NAME_PTR(attr_idx));
               }
               else if (work_sharing_dir &&
                        has_been_reprivatized(attr_idx)) {
                  PRINTMSG(IL_LINE_NUM(list2_idx), 1651, Error,
                           IL_COL_NUM(list2_idx),
                           "Privatized",
                           AT_OBJ_NAME_PTR(attr_idx));
               }
               else if (ATD_CLASS(attr_idx) == Dummy_Argument   &&
                        ATD_INTENT(attr_idx) == Intent_In) {
                  PRINTMSG(IL_LINE_NUM(list2_idx), 1492, Error,
                           IL_COL_NUM(list2_idx),
                           AT_OBJ_NAME_PTR(attr_idx),
                           "LASTPRIVATE");
               }
               else if (ATD_PURE(attr_idx)) {
                  PRINTMSG(IL_LINE_NUM(list2_idx), 1493, Error,
                           IL_COL_NUM(list2_idx),
                           AT_OBJ_NAME_PTR(attr_idx),
                           "LASTPRIVATE");
               }
               else {
	          ATD_TASK_LASTPRIVATE(attr_idx) = TRUE;

                  if (ATD_CLASS(attr_idx) == Variable &&
                      ATD_AUTOMATIC(attr_idx) &&
                      ATD_AUTO_BASE_IDX(attr_idx) != NULL_IDX &&
                      ! ATD_TASK_LASTPRIVATE(ATD_AUTO_BASE_IDX(attr_idx))) {

                     ATD_TASK_LASTPRIVATE(ATD_AUTO_BASE_IDX(attr_idx)) = TRUE;

                     NTR_IR_LIST_TBL(list3_idx);
                     IL_PREV_LIST_IDX(IL_IDX(list_idx)) = list3_idx;
                     IL_NEXT_LIST_IDX(list3_idx) = IL_IDX(list_idx);
                     IL_IDX(list_idx) = list3_idx;
                     IL_LIST_CNT(list_idx)++;

                     IL_FLD(list3_idx) = AT_Tbl_Idx;
                     IL_IDX(list3_idx) = ATD_AUTO_BASE_IDX(attr_idx);
                     IL_LINE_NUM(list3_idx) = IL_LINE_NUM(list2_idx);
                     IL_COL_NUM(list3_idx) = IL_COL_NUM(list2_idx);
                  }
	       }
            }
            else {
               /* SB_Tbl_Idx here */
               add_common_blk_objects_to_list(list2_idx, list_idx);
            }
    
	    list2_idx = IL_NEXT_LIST_IDX(list2_idx);
	 }
      }
   }

   if (open_mp_clause_allowed[directive][Reduction_Omp_Clause]) {
      /* process REDUCTION var list */
    
      list_idx = list_array[OPEN_MP_REDUCTION_LIST_IDX];
      cdir_switches.reduction_list_idx = list_idx;
    
      if (IL_FLD(list_idx) != NO_Tbl_Idx) {
    
         list_idx = IL_IDX(list_idx);
         while (list_idx) {

	    list2_idx = IL_IDX(list_idx);
    
	    while (list2_idx) {

               attr_idx = IL_IDX(list2_idx);
               AT_LOCKED_IN(attr_idx) = TRUE;

               while (AT_ATTR_LINK(attr_idx)) {
                  attr_idx = AT_ATTR_LINK(attr_idx);
                  AT_LOCKED_IN(attr_idx) = TRUE;
               }

               IL_IDX(list2_idx) = attr_idx;
    
               if (AT_OBJ_CLASS(attr_idx) != Data_Obj) {
                  PRINTMSG(IL_LINE_NUM(list2_idx), 1473, Error,
                           IL_COL_NUM(list2_idx),
                           AT_OBJ_NAME_PTR(attr_idx),
			   "REDUCTION", open_mp_dir_str[directive]);
               }
               else if (ATD_CLASS(attr_idx) == Constant) {
                  PRINTMSG(IL_LINE_NUM(list2_idx), 1473, Error,
                           IL_COL_NUM(list2_idx),
                           AT_OBJ_NAME_PTR(attr_idx),
                           "REDUCTION", open_mp_dir_str[directive]);
               }
               /* the following is deleted by jhs, 02.9.10 */
	       /* the above is deleted by jhs, 02.9.10 */
               else if (ATD_CLASS(attr_idx) == CRI__Pointee) {
                  PRINTMSG(IL_LINE_NUM(list2_idx), 1477, Error,
                           IL_COL_NUM(list2_idx),
                           AT_OBJ_NAME_PTR(attr_idx));
               }
               else if (TYP_TYPE(ATD_TYPE_IDX(attr_idx)) == CRI_Ptr ||
                        TYP_TYPE(ATD_TYPE_IDX(attr_idx)) == CRI_Ch_Ptr) {

                  PRINTMSG(IL_LINE_NUM(list2_idx), 1484, Error,
                           IL_COL_NUM(list2_idx),
                           "Cray pointer",
                           AT_OBJ_NAME_PTR(attr_idx),
                           open_mp_dir_str[directive]);
               }
               else if (ATD_POINTER(attr_idx)) {

                  PRINTMSG(IL_LINE_NUM(list2_idx), 1484, Error,
                           IL_COL_NUM(list2_idx),
                           "Pointer",
                           AT_OBJ_NAME_PTR(attr_idx),
                           open_mp_dir_str[directive]);
               }
               else if (ATD_ALLOCATABLE(attr_idx)) {

                  PRINTMSG(IL_LINE_NUM(list2_idx), 1484, Error,
                           IL_COL_NUM(list2_idx),
                           "Allocatable array",
                           AT_OBJ_NAME_PTR(attr_idx),
                           open_mp_dir_str[directive]);
               }
               else if (multiple_clause_err(attr_idx, 
                                            OPEN_MP_REDUCTION_LIST_IDX)) {
                  PRINTMSG(IL_LINE_NUM(list2_idx), 1476, Error,
                           IL_COL_NUM(list2_idx),
                           AT_OBJ_NAME_PTR(attr_idx));
               }
               else if (work_sharing_dir &&
                        has_been_reprivatized(attr_idx)) {
                  PRINTMSG(IL_LINE_NUM(list2_idx), 1651, Error,
                           IL_COL_NUM(list2_idx),
                           "Reduction",
                           AT_OBJ_NAME_PTR(attr_idx));
               }
               else if (ATD_CLASS(attr_idx) == Dummy_Argument   &&
                        ATD_INTENT(attr_idx) == Intent_In) {
                  PRINTMSG(IL_LINE_NUM(list2_idx), 1492, Error,
                           IL_COL_NUM(list2_idx),
                           AT_OBJ_NAME_PTR(attr_idx),
                           "REDUCTION");
               }
               else if (ATD_PURE(attr_idx)) {
                  PRINTMSG(IL_LINE_NUM(list2_idx), 1493, Error,
                           IL_COL_NUM(list2_idx),
                           AT_OBJ_NAME_PTR(attr_idx),
                           "REDUCTION");
               }
               else {
	          ATD_TASK_REDUCTION(attr_idx) = TRUE;

                if (ATD_CLASS(attr_idx) == Variable &&
                    ATD_AUTOMATIC(attr_idx) &&
                    ATD_AUTO_BASE_IDX(attr_idx) != NULL_IDX &&
                    ! ATD_TASK_REDUCTION(ATD_AUTO_BASE_IDX(attr_idx))) {

                     ATD_TASK_REDUCTION(ATD_AUTO_BASE_IDX(attr_idx)) = TRUE;

                     NTR_IR_LIST_TBL(list3_idx);
                     IL_PREV_LIST_IDX(IL_IDX(list_idx)) = list3_idx;
                     IL_NEXT_LIST_IDX(list3_idx) = IL_IDX(list_idx);
                     IL_IDX(list_idx) = list3_idx;
                     IL_LIST_CNT(list_idx)++;

                     IL_FLD(list3_idx) = AT_Tbl_Idx;
                     IL_IDX(list3_idx) = ATD_AUTO_BASE_IDX(attr_idx);
                     IL_LINE_NUM(list3_idx) = IL_LINE_NUM(list2_idx);
                     IL_COL_NUM(list3_idx) = IL_COL_NUM(list2_idx);
              }

	       }
       
	       list2_idx = IL_NEXT_LIST_IDX(list2_idx);
	    }

            list_idx = IL_NEXT_LIST_IDX(list_idx);

         }
      }
   }
 
   /* no calls to expr_semantics can be made before the NEST processing */

   if (open_mp_clause_allowed[directive][Nest_Omp_Clause]) {
      /* process NEST var list */

      list_idx = list_array[OPEN_MP_NEST_IDX];

      if (IL_FLD(list_idx) != NO_Tbl_Idx) {

         list2_idx = IL_IDX(list_idx);

         while (list2_idx) {

            attr_idx = IL_IDX(list2_idx);
            AT_LOCKED_IN(attr_idx) = TRUE;

            while (AT_ATTR_LINK(attr_idx)) {
               attr_idx = AT_ATTR_LINK(attr_idx);
               AT_LOCKED_IN(attr_idx) = TRUE;
            }


            IL_IDX(list2_idx) = attr_idx;
            list2_idx = IL_NEXT_LIST_IDX(list2_idx);
         }
      }
   }

   if (open_mp_clause_allowed[directive][Affinity_Omp_Clause]) {
      /* process AFFINITY var list */

      list_idx = list_array[OPEN_MP_AFFINITY_IDX];

      if (IL_FLD(list_idx) != NO_Tbl_Idx) {

         list2_idx = IL_IDX(list_idx);
         list3_idx = list_array[OPEN_MP_NEST_IDX];
         list3_idx = IL_IDX(list3_idx);

         while (list2_idx) {

            attr_idx = IL_IDX(list2_idx);
            AT_LOCKED_IN(attr_idx) = TRUE;

            while (AT_ATTR_LINK(attr_idx)) {
               attr_idx = AT_ATTR_LINK(attr_idx);
               AT_LOCKED_IN(attr_idx) = TRUE;
            }

            IL_IDX(list2_idx) = attr_idx;

            if (list3_idx == NULL_IDX ||
                IL_IDX(list3_idx) != attr_idx) {
               find_opnd_line_and_column(&IL_OPND(list2_idx), &line, &column);

               PRINTMSG(line, 1417, Error, column);
               break;
            }
            list2_idx = IL_NEXT_LIST_IDX(list2_idx);
            list3_idx = IL_NEXT_LIST_IDX(list3_idx);
         }


         list_idx = list_array[OPEN_MP_THREAD_DATA_IDX];

# ifdef _DEBUG
         if (IL_FLD(list_idx) == NO_Tbl_Idx ||
             IL_FLD(list_array[OPEN_MP_IS_THREAD_IDX]) != CN_Tbl_Idx) {

            PRINTMSG(stmt_start_line, 626, Internal, stmt_start_col,
                     "THREAD/DATA list item", "open_mp_directive_semantics");
         }
# endif

         if (compare_cn_and_value(IL_IDX(list_array[OPEN_MP_IS_THREAD_IDX]),
                                  0,
                                  Eq_Opr)) {
            /* DATA */
            COPY_OPND(opnd, IL_OPND(list_idx));
            exp_desc.rank = 0;
            xref_state = CIF_Symbol_Reference;
            expr_semantics(&opnd, &exp_desc);
            COPY_OPND(IL_OPND(list_idx), opnd);

            if (! exp_desc.array_elt) {
               /* error, must be array element */
               find_opnd_line_and_column(&opnd, &line, &column);

               PRINTMSG(line, 1372, Error, column);
            }

            list2_idx = list_array[OPEN_MP_ONTO_IDX];
            if (IL_FLD(list2_idx) != NO_Tbl_Idx) {
               /* can't have ONTO with DATA affinity. */
               find_opnd_line_and_column(&IL_OPND(list2_idx), &line, &column);

               PRINTMSG(line, 1418, Error, column);
            }
         }
         else {
            /* THREAD */
            COPY_OPND(opnd, IL_OPND(list_idx));
            exp_desc.rank = 0;
            xref_state = CIF_Symbol_Reference;
            expr_semantics(&opnd, &exp_desc);
            COPY_OPND(IL_OPND(list_idx), opnd);

            if (exp_desc.type != Integer ||
                exp_desc.rank != 0) {
               /* error, must be array element */
               find_opnd_line_and_column(&opnd, &line, &column);

               PRINTMSG(line, 1371, Error, column);
            }
         }
      }
   }

   if (open_mp_clause_allowed[directive][Onto_Omp_Clause]) {
      /* process ONTO var list */

      list_idx = list_array[OPEN_MP_ONTO_IDX];

      if (IL_FLD(list_idx) == IL_Tbl_Idx) {
         list_idx = IL_IDX(list_idx);

         while (list_idx != NULL_IDX) {

            COPY_OPND(opnd, IL_OPND(list_idx));
            exp_desc.rank = 0;
            xref_state = CIF_Symbol_Reference;
            expr_semantics(&opnd, &exp_desc);
            COPY_OPND(IL_OPND(list_idx), opnd);

            find_opnd_line_and_column(&opnd, &line, &column);

            if (OPND_FLD(opnd) != CN_Tbl_Idx ||
                exp_desc.type != Integer) {
               /* error, must be a constant */
               PRINTMSG(line, 1368, Error, column);
            }
            else if (compare_cn_and_value(OPND_IDX(opnd),
                                          0,
                                          Lt_Opr)) {

               /* error, must be greater than zero */
               PRINTMSG(line, 1368, Error, column);
            }

            list_idx = IL_NEXT_LIST_IDX(list_idx);
         }
      }
   }

   if (open_mp_clause_allowed[directive][Copyprivate_Omp_Clause]) {
      /* process COPYPRIVATE var list */
      list_idx = list_array[OPEN_MP_COPYPRIVATE_IDX];
      cdir_switches.copyprivate_list_idx = list_idx;
      if (IL_FLD(list_idx) != NO_Tbl_Idx) {
         list2_idx = IL_IDX(list_idx);
         while (list2_idx) {
            if (IL_FLD(list2_idx) == AT_Tbl_Idx) {
               attr_idx = IL_IDX(list2_idx);
               AT_LOCKED_IN(attr_idx) = TRUE;
               while (AT_ATTR_LINK(attr_idx)) {
                  attr_idx = AT_ATTR_LINK(attr_idx);
                  AT_LOCKED_IN(attr_idx) = TRUE;
               }
               IL_IDX(list2_idx) = attr_idx;
               if (AT_OBJ_CLASS(attr_idx) != Data_Obj) {
                  PRINTMSG(IL_LINE_NUM(list2_idx), 1473, Error,
                           IL_COL_NUM(list2_idx),
                           AT_OBJ_NAME_PTR(attr_idx),
                           "COPYPRIVATE", open_mp_dir_str[directive]);
               }
               else if (ATD_CLASS(attr_idx) == Constant) {
                  PRINTMSG(IL_LINE_NUM(list2_idx), 1473, Error,
                           IL_COL_NUM(list2_idx),
                           AT_OBJ_NAME_PTR(attr_idx),
                           "COPYPRIVATE", open_mp_dir_str[directive]);
               }
               else if (ATD_CLASS(attr_idx) == CRI__Pointee) {
                  PRINTMSG(IL_LINE_NUM(list2_idx), 1477, Error,
                           IL_COL_NUM(list2_idx),
                           AT_OBJ_NAME_PTR(attr_idx));
               }
               else if (TYP_TYPE(ATD_TYPE_IDX(attr_idx)) == CRI_Ptr ||
                        TYP_TYPE(ATD_TYPE_IDX(attr_idx)) == CRI_Ch_Ptr) {
                  PRINTMSG(IL_LINE_NUM(list2_idx), 1478, Error,
                           IL_COL_NUM(list2_idx),
                           "Cray pointer",
                           AT_OBJ_NAME_PTR(attr_idx));
               }
               else if (ATD_POINTER(attr_idx)) {
                  PRINTMSG(IL_LINE_NUM(list2_idx), 1478, Error,
                           IL_COL_NUM(list2_idx),
                           "Pointer",
                           AT_OBJ_NAME_PTR(attr_idx));
               }
               else if (ATD_ALLOCATABLE(attr_idx)) {
                  PRINTMSG(IL_LINE_NUM(list2_idx), 1478, Error,
                           IL_COL_NUM(list2_idx),
                           "Allocatable array",
                           AT_OBJ_NAME_PTR(attr_idx));
               }
               else if (ATD_ARRAY_IDX(attr_idx) != NULL_IDX &&
                        (BD_ARRAY_CLASS(ATD_ARRAY_IDX(attr_idx)) ==
                                                           Assumed_Size ||
                         BD_ARRAY_CLASS(ATD_ARRAY_IDX(attr_idx)) ==
                                                           Assumed_Shape)) {
                  PRINTMSG(IL_LINE_NUM(list2_idx), 1482, Error,
                           IL_COL_NUM(list2_idx),
                            (BD_ARRAY_CLASS(ATD_ARRAY_IDX(attr_idx)) ==
                                                           Assumed_Size ?
                           "Assumed size" : "Assumed shape"),
                           AT_OBJ_NAME_PTR(attr_idx));
               }
               else if (multiple_clause_err(attr_idx,
                                                  OPEN_MP_COPYPRIVATE_IDX)) {
                  PRINTMSG(IL_LINE_NUM(list2_idx), 1476, Error,
                           IL_COL_NUM(list2_idx),
                           AT_OBJ_NAME_PTR(attr_idx));
               }
               else if (work_sharing_dir &&
                        has_been_reprivatized(attr_idx)) {
                  PRINTMSG(IL_LINE_NUM(list2_idx), 1651, Error,
                           IL_COL_NUM(list2_idx),
                           "Privatized",
                           AT_OBJ_NAME_PTR(attr_idx));
               }
               else if (ATD_CLASS(attr_idx) == Dummy_Argument   &&
                        ATD_INTENT(attr_idx) == Intent_In) {
                  PRINTMSG(IL_LINE_NUM(list2_idx), 1492, Error,
                           IL_COL_NUM(list2_idx),
                           AT_OBJ_NAME_PTR(attr_idx),
                           "COPYPRIVATE");
               }
               else if (ATD_PURE(attr_idx)) {
                  PRINTMSG(IL_LINE_NUM(list2_idx), 1493, Error,
                           IL_COL_NUM(list2_idx),
                           AT_OBJ_NAME_PTR(attr_idx),
                           "COPYPRIVATE");
               }
               else {
                  ATD_TASK_COPYPRIVATE(attr_idx) = TRUE;
                  if (ATD_CLASS(attr_idx) == Variable &&
                      ATD_AUTOMATIC(attr_idx) &&
                      ATD_AUTO_BASE_IDX(attr_idx) != NULL_IDX &&
                      ! ATD_TASK_COPYPRIVATE(ATD_AUTO_BASE_IDX(attr_idx))) {
                     ATD_TASK_COPYPRIVATE(ATD_AUTO_BASE_IDX(attr_idx)) = TRUE;
                     NTR_IR_LIST_TBL(list3_idx);
                     IL_PREV_LIST_IDX(IL_IDX(list_idx)) = list3_idx;
                     IL_NEXT_LIST_IDX(list3_idx) = IL_IDX(list_idx);
                     IL_IDX(list_idx) = list3_idx;
                     IL_LIST_CNT(list_idx)++;
                     IL_FLD(list3_idx) = AT_Tbl_Idx;
                     IL_IDX(list3_idx) = ATD_AUTO_BASE_IDX(attr_idx);
                     IL_LINE_NUM(list3_idx) = IL_LINE_NUM(list2_idx);
                     IL_COL_NUM(list3_idx) = IL_COL_NUM(list2_idx);
                  }
               }
            }
            else {
               /* SB_Tbl_Idx here */
               add_common_blk_objects_to_list(list2_idx, list_idx);
            }
            list2_idx = IL_NEXT_LIST_IDX(list2_idx);
         }
      }
   }
  
   if (open_mp_clause_allowed[directive][Default_Omp_Clause]) {
      /* save the DEFAULT scope list idx */

      list_idx = list_array[OPEN_MP_DEFAULT_IDX];

      if (IL_FLD(list_idx) != NO_Tbl_Idx) {
         cdir_switches.default_scope_list_idx = list_idx;
      }
   }
 
   curr_stmt_sh_idx = save_curr_stmt_sh_idx;

   /* restore error flag on curr_stmt_sh_idx. */
   SH_ERR_FLG(orig_sh_idx) = save_error_flag;

   TRACE (Func_Exit, "open_mp_directive_semantics", NULL);

   return;

}  /* open_mp_directive_semantics */

/* the following are added by jhs, 02/7/21 */
/******************************************************************************\
|*									      *|
|* Description:								      *|
|*      The ir looks like this coming in ...                                  *|
|*                      |- COPYPRIVATE var list                              *|
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
static void open_mp_copyprivate_semantics()

{
   int			attr_idx;
   int			ir_idx;
   int			list_idx;
   int			list2_idx;
   int			list3_idx;
   int			save_curr_stmt_sh_idx;
   boolean		save_error_flag;


   TRACE (Func_Entry, "open_mp_copyprivate_semantics", NULL);


   save_curr_stmt_sh_idx = curr_stmt_sh_idx;
   save_error_flag = SH_ERR_FLG(curr_stmt_sh_idx);

   ir_idx = SH_IR_IDX(curr_stmt_sh_idx);
   list_idx = IL_NEXT_LIST_IDX(IR_IDX_L(ir_idx));

   cdir_switches.parallel_region = TRUE;
   cdir_switches.copyprivate_list_idx = list_idx;
    
      if (IL_FLD(list_idx) != NO_Tbl_Idx) {
    
	 list2_idx = IL_IDX(list_idx);
    
	 while (list2_idx) {
    
            if (IL_FLD(list2_idx) == AT_Tbl_Idx) {
	       attr_idx = IL_IDX(list2_idx);
	       AT_LOCKED_IN(attr_idx) = TRUE;
       
	       while (AT_ATTR_LINK(attr_idx)) {
		  attr_idx = AT_ATTR_LINK(attr_idx);
		  AT_LOCKED_IN(attr_idx) = TRUE;
	       }
       
	       IL_IDX(list2_idx) = attr_idx;
       
	       if (AT_OBJ_CLASS(attr_idx) != Data_Obj) {
		  PRINTMSG(IL_LINE_NUM(list2_idx), 1473, Error,
			   IL_COL_NUM(list2_idx),
			   AT_OBJ_NAME_PTR(attr_idx),
			   "COPYPRIVATE", "End Single");
	       }
               else if (ATD_CLASS(attr_idx) == Constant) {
                  PRINTMSG(IL_LINE_NUM(list2_idx), 1473, Error,
                           IL_COL_NUM(list2_idx),
                           AT_OBJ_NAME_PTR(attr_idx),
                           "COPYPRIVATE", "End Single");
               }
               else if (ATD_CLASS(attr_idx) == CRI__Pointee) {
                  PRINTMSG(IL_LINE_NUM(list2_idx), 1477, Error,
                           IL_COL_NUM(list2_idx),
                           AT_OBJ_NAME_PTR(attr_idx));
               }
               else if (TYP_TYPE(ATD_TYPE_IDX(attr_idx)) == CRI_Ptr ||
                        TYP_TYPE(ATD_TYPE_IDX(attr_idx)) == CRI_Ch_Ptr) {

                  PRINTMSG(IL_LINE_NUM(list2_idx), 1478, Error,
                           IL_COL_NUM(list2_idx),
                           "Cray pointer",
                           AT_OBJ_NAME_PTR(attr_idx));
               }
               else if (ATD_POINTER(attr_idx)) {

                  PRINTMSG(IL_LINE_NUM(list2_idx), 1478, Error,
                           IL_COL_NUM(list2_idx),
                           "Pointer",
                           AT_OBJ_NAME_PTR(attr_idx));
               }
               else if (ATD_ALLOCATABLE(attr_idx)) {

                  PRINTMSG(IL_LINE_NUM(list2_idx), 1478, Error,
                           IL_COL_NUM(list2_idx),
                           "Allocatable array",
                           AT_OBJ_NAME_PTR(attr_idx));
               }
               else if (ATD_ARRAY_IDX(attr_idx) != NULL_IDX &&
                        (BD_ARRAY_CLASS(ATD_ARRAY_IDX(attr_idx)) == 
                                                           Assumed_Size ||
                         BD_ARRAY_CLASS(ATD_ARRAY_IDX(attr_idx)) == 
                                                           Assumed_Shape)) {

                  PRINTMSG(IL_LINE_NUM(list2_idx), 1482, Error,
                           IL_COL_NUM(list2_idx),
                            (BD_ARRAY_CLASS(ATD_ARRAY_IDX(attr_idx)) == 
                                                           Assumed_Size ?
                           "Assumed size" : "Assumed shape"),
                           AT_OBJ_NAME_PTR(attr_idx));
               }
               else if (has_been_reprivatized(attr_idx)) {
                  PRINTMSG(IL_LINE_NUM(list2_idx), 1651, Error,
                           IL_COL_NUM(list2_idx),
                           "Privatized",
                           AT_OBJ_NAME_PTR(attr_idx));
               }
               else if (ATD_CLASS(attr_idx) == Dummy_Argument   &&
                        ATD_INTENT(attr_idx) == Intent_In) {
                  PRINTMSG(IL_LINE_NUM(list2_idx), 1492, Error,
                           IL_COL_NUM(list2_idx),
                           AT_OBJ_NAME_PTR(attr_idx),
                           "COPYPRIVATE");
               }
               else if (ATD_PURE(attr_idx)) {
                  PRINTMSG(IL_LINE_NUM(list2_idx), 1493, Error,
                           IL_COL_NUM(list2_idx),
                           AT_OBJ_NAME_PTR(attr_idx),
                           "COPYPRIVATE");
               }
	       else {
		  ATD_TASK_COPYPRIVATE(attr_idx) = TRUE;

                  if (ATD_CLASS(attr_idx) == Variable &&
                      ATD_AUTOMATIC(attr_idx) &&
                      ATD_AUTO_BASE_IDX(attr_idx) != NULL_IDX &&
                      ! ATD_TASK_COPYPRIVATE(ATD_AUTO_BASE_IDX(attr_idx))) {

                     ATD_TASK_COPYPRIVATE(ATD_AUTO_BASE_IDX(attr_idx)) = TRUE;

                     NTR_IR_LIST_TBL(list3_idx);
                     IL_PREV_LIST_IDX(IL_IDX(list_idx)) = list3_idx;
                     IL_NEXT_LIST_IDX(list3_idx) = IL_IDX(list_idx);
                     IL_IDX(list_idx) = list3_idx;
                     IL_LIST_CNT(list_idx)++;

                     IL_FLD(list3_idx) = AT_Tbl_Idx;
                     IL_IDX(list3_idx) = ATD_AUTO_BASE_IDX(attr_idx);
                     IL_LINE_NUM(list3_idx) = IL_LINE_NUM(list2_idx);
                     IL_COL_NUM(list3_idx) = IL_COL_NUM(list2_idx);
                  }
	       }
            }
            else {
               /* SB_Tbl_Idx here */
               add_common_blk_objects_to_list(list2_idx, list_idx);
            }
    
	    list2_idx = IL_NEXT_LIST_IDX(list2_idx);
	 }
      }

   /* restore */
   curr_stmt_sh_idx = save_curr_stmt_sh_idx;
   SH_ERR_FLG(curr_stmt_sh_idx) = save_error_flag;

   TRACE (Func_Exit, "open_mp_copyprivate_semantics", NULL);

   return;

}  /* open_mp_copyprivate_semantics */

/* the above are added by jhs, 02/7/21 */

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

static void end_blk_mp_semantics(boolean	open_mp)

{
   int                  ir_idx;
   int			list_idx;

   TRACE (Func_Entry, "end_blk_mp_semantics", NULL);

# if defined _DEBUG
   if (IR_FLD_R(SH_IR_IDX(curr_stmt_sh_idx)) != SH_Tbl_Idx) {
      PRINTMSG(stmt_start_line, 626, Internal, stmt_start_col,
               "SH_Tbl_Idx", "end_blk_mp_semantics");
   }
# endif

   /* get back to start stmt of block */

   if (SH_ERR_FLG(IR_IDX_R(SH_IR_IDX(curr_stmt_sh_idx)))) {
      goto EXIT;
   }

   ir_idx = SH_IR_IDX(IR_IDX_R(SH_IR_IDX(curr_stmt_sh_idx)));

   if (open_mp) {
      set_open_mp_task_flags(ir_idx, FALSE);
   }
   else {
      set_mp_task_flags(ir_idx, FALSE);
   }


   pop_task_blk();

   if (OPND_FLD(cdir_switches.first_sh_blk_stk) == IL_Tbl_Idx) {
      list_idx = OPND_IDX(cdir_switches.first_sh_blk_stk);
      /* find the end and process each blk backwards */

      while (IL_NEXT_LIST_IDX(list_idx) != NULL_IDX) {
         list_idx = IL_NEXT_LIST_IDX(list_idx);
       }

      while (list_idx) {
         ir_idx = SH_IR_IDX(IL_IDX(list_idx));

         if (open_mp) {
            set_open_mp_task_flags(ir_idx, TRUE);
         }
         else {
            set_mp_task_flags(ir_idx, TRUE);
         }

         list_idx = IL_PREV_LIST_IDX(list_idx);
      }
   }

EXIT:

   TRACE (Func_Exit, "end_blk_mp_semantics", NULL);

   return;

}  /* end_blk_mp_semantics */

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

static void set_open_mp_task_flags(int		ir_idx,
                                   boolean	flag)

{
   int                  	attr_idx;
#ifdef KEY /* Bug 10177 */
   open_mp_directive_type	directive = Doacross;
#else /* KEY Bug 10177 */
   open_mp_directive_type	directive;
#endif /* KEY Bug 10177 */
   int                  	i;
   int                  	list_array[OPEN_MP_LIST_CNT];
   int                  	list_idx;
   int                  	list2_idx;


   TRACE (Func_Entry, "set_open_mp_task_flags", NULL);

   list_idx = IR_IDX_L(ir_idx);

   for (i = 0; i < OPEN_MP_LIST_CNT; i++) {
      list_array[i] = list_idx;
      list_idx = IL_NEXT_LIST_IDX(list_idx);
   }

   switch (IR_OPR(ir_idx)) {
   case Do_Open_Mp_Opr:
      directive = Do_Omp;
      break;

   case Parallel_Open_Mp_Opr:
      directive = Parallel_Omp;
      break;

   case Paralleldo_Open_Mp_Opr:
      directive = Parallel_Do_Omp;
      break;

   case Parallelsections_Open_Mp_Opr:
      directive = Parallel_Sections_Omp;
      break;

   case Sections_Open_Mp_Opr:
      directive = Sections_Omp;
      break;

   case Single_Open_Mp_Opr:
      directive = Single_Omp;
      break;

   }

   if (open_mp_clause_allowed[directive][Shared_Omp_Clause]) {
      /* process SHARED var list */

      list_idx = list_array[OPEN_MP_SHARED_IDX];

      cdir_switches.shared_list_idx = (flag ? list_idx : NULL_IDX) ;

      if (IL_FLD(list_idx) != NO_Tbl_Idx) {

         list2_idx = IL_IDX(list_idx);

         while (list2_idx) {

#ifdef KEY /* Bug 6075 */
            if (IL_FLD(list2_idx) == AT_Tbl_Idx) {
#endif /* KEY Bug 6075 */
	       attr_idx = IL_IDX(list2_idx);

	       ATD_TASK_SHARED(attr_idx) = flag;
#ifdef KEY /* Bug 6075 */
            }
#endif /* KEY Bug 6075 */

            list2_idx = IL_NEXT_LIST_IDX(list2_idx);
         }
      }
   }

   if (open_mp_clause_allowed[directive][Private_Omp_Clause]) {
      /* process PRIVATE var list */

      list_idx = list_array[OPEN_MP_PRIVATE_IDX];

      cdir_switches.private_list_idx = (flag ? list_idx : NULL_IDX) ;

      if (IL_FLD(list_idx) != NO_Tbl_Idx) {

         list2_idx = IL_IDX(list_idx);

         while (list2_idx) {

            if (IL_FLD(list2_idx) == AT_Tbl_Idx) {
               attr_idx = IL_IDX(list2_idx);

               ATD_TASK_PRIVATE(attr_idx) = flag;
            }

            list2_idx = IL_NEXT_LIST_IDX(list2_idx);
         }
      }
   }

   if (open_mp_clause_allowed[directive][Firstprivate_Omp_Clause]) {
      /* process FIRSTPRIVATE var list */

      list_idx = list_array[OPEN_MP_FIRSTPRIVATE_IDX];

      cdir_switches.firstprivate_list_idx = (flag ? list_idx : NULL_IDX) ;

      if (IL_FLD(list_idx) != NO_Tbl_Idx) {

         list2_idx = IL_IDX(list_idx);

         while (list2_idx) {

            if (IL_FLD(list2_idx) == AT_Tbl_Idx) {
               attr_idx = IL_IDX(list2_idx);
               ATD_TASK_FIRSTPRIVATE(attr_idx) = flag;
            }

            list2_idx = IL_NEXT_LIST_IDX(list2_idx);
         }
      }
   }

   if (open_mp_clause_allowed[directive][Copyin_Omp_Clause]) {
      /* process COPYIN var list */

      list_idx = list_array[OPEN_MP_COPYIN_IDX];

      cdir_switches.copyin_list_idx = (flag ? list_idx : NULL_IDX) ;

      if (IL_FLD(list_idx) != NO_Tbl_Idx) {

         list2_idx = IL_IDX(list_idx);

         while (list2_idx) {

            if (IL_FLD(list2_idx) == AT_Tbl_Idx) {
               attr_idx = IL_IDX(list2_idx);
               ATD_TASK_COPYIN(attr_idx) = flag;
            }

            list2_idx = IL_NEXT_LIST_IDX(list2_idx);
         }
      }
   }

   if (open_mp_clause_allowed[directive][Lastprivate_Omp_Clause]) {
      /* process LASTPRIVATE var list */

      list_idx = list_array[OPEN_MP_LASTPRIVATE_IDX];

      cdir_switches.lastprivate_list_idx = (flag ? list_idx : NULL_IDX) ;

      if (IL_FLD(list_idx) != NO_Tbl_Idx) {

         list2_idx = IL_IDX(list_idx);

         while (list2_idx) {

            if (IL_FLD(list2_idx) == AT_Tbl_Idx) {
               attr_idx = IL_IDX(list2_idx);
               ATD_TASK_LASTPRIVATE(attr_idx) = flag;
            }

            list2_idx = IL_NEXT_LIST_IDX(list2_idx);
         }
      }
   }

   if (open_mp_clause_allowed[directive][Reduction_Omp_Clause]) {
      /* process REDUCTION var list */

      list_idx = list_array[OPEN_MP_REDUCTION_LIST_IDX];

      cdir_switches.reduction_list_idx = (flag ? list_idx : NULL_IDX) ;

      if (IL_FLD(list_idx) != NO_Tbl_Idx) {

         list_idx = IL_IDX(list_idx);
         while (list_idx) {

            list2_idx = IL_IDX(list_idx);

            while (list2_idx) {

               attr_idx = IL_IDX(list2_idx);
               ATD_TASK_REDUCTION(attr_idx) = flag;

               list2_idx = IL_NEXT_LIST_IDX(list2_idx);
            }

            list_idx = IL_NEXT_LIST_IDX(list_idx);

         }
      }
   }


   if (open_mp_clause_allowed[directive][Copyprivate_Omp_Clause]) {
      /* process COPYPRIVATE var list */
      list_idx = list_array[OPEN_MP_COPYPRIVATE_IDX];
      cdir_switches.copyprivate_list_idx = (flag ? list_idx : NULL_IDX) ;
      if (IL_FLD(list_idx) != NO_Tbl_Idx) {
         list2_idx = IL_IDX(list_idx);
         while (list2_idx) {
            if (IL_FLD(list2_idx) == AT_Tbl_Idx) {
               attr_idx = IL_IDX(list2_idx);
               ATD_TASK_COPYPRIVATE(attr_idx) = flag;
            }
            list2_idx = IL_NEXT_LIST_IDX(list2_idx);
         }
      }
   }


   if (open_mp_clause_allowed[directive][Default_Omp_Clause]) {
      /* process the DEFAULT scope list idx */

      list_idx = list_array[OPEN_MP_DEFAULT_IDX];

      if (IL_FLD(list_idx) != NO_Tbl_Idx) {
         cdir_switches.default_scope_list_idx = (flag ? list_idx : NULL_IDX) ;
      }
   }

   cdir_switches.parallel_region = flag;

   TRACE (Func_Exit, "set_open_mp_task_flags", NULL);

   return;

}  /* set_open_mp_task_flags */

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

static void push_task_blk(int	sh_idx)

{
   int	list_idx;

   TRACE (Func_Entry, "push_task_blk", NULL);

   NTR_IR_LIST_TBL(list_idx);

   if (OPND_FLD(cdir_switches.first_sh_blk_stk) == NO_Tbl_Idx) {
      OPND_FLD(cdir_switches.first_sh_blk_stk) = IL_Tbl_Idx;
      OPND_IDX(cdir_switches.first_sh_blk_stk) = list_idx;
      OPND_LIST_CNT(cdir_switches.first_sh_blk_stk) = 1;
   }
   else {
      IL_NEXT_LIST_IDX(list_idx) = OPND_IDX(cdir_switches.first_sh_blk_stk);
      IL_PREV_LIST_IDX(OPND_IDX(cdir_switches.first_sh_blk_stk)) = list_idx;
      OPND_IDX(cdir_switches.first_sh_blk_stk) = list_idx;
      OPND_LIST_CNT(cdir_switches.first_sh_blk_stk) += 1;
   }

   IL_FLD(list_idx) = SH_Tbl_Idx;
   IL_IDX(list_idx) = sh_idx;

   TRACE (Func_Exit, "push_task_blk", NULL);

   return;

}  /* push_task_blk */

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

static int pop_task_blk(void)

{
   int		sh_idx = NULL_IDX;
   int		list_idx;
   int		trash_list_idx;

   TRACE (Func_Entry, "pop_task_blk", NULL);

   if (OPND_FLD(cdir_switches.first_sh_blk_stk) == IL_Tbl_Idx) {
      list_idx = OPND_IDX(cdir_switches.first_sh_blk_stk);
      sh_idx = IL_IDX(list_idx);

      trash_list_idx = list_idx;

      list_idx = IL_NEXT_LIST_IDX(list_idx);

      FREE_IR_LIST_NODE(trash_list_idx);

      OPND_IDX(cdir_switches.first_sh_blk_stk) = list_idx;
      OPND_LIST_CNT(cdir_switches.first_sh_blk_stk) -= 1;
 
      if (list_idx) {
         IL_PREV_LIST_IDX(list_idx) = NULL_IDX;
      }
      else {
         OPND_FLD(cdir_switches.first_sh_blk_stk) = NO_Tbl_Idx;
         OPND_IDX(cdir_switches.first_sh_blk_stk) = NULL_IDX;
      }
   }

   TRACE (Func_Exit, "pop_task_blk", NULL);

   return(sh_idx);

}  /* pop_task_blk */

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

static boolean multiple_clause_err(int		attr_idx,
                                  int		clause_idx)

{
   boolean	issue_err = FALSE;
   int		i;
   int		list_idx;
   int		test_clause_idx = -1;

   TRACE (Func_Entry, "multiple_clause_err", NULL);

   if (ATD_TASK_SHARED(attr_idx) && 
       clause_idx != OPEN_MP_SHARED_IDX) {
      test_clause_idx = OPEN_MP_SHARED_IDX;
   }
   else if (ATD_TASK_PRIVATE(attr_idx) && 
            clause_idx != OPEN_MP_PRIVATE_IDX) {
      test_clause_idx = OPEN_MP_PRIVATE_IDX;
   }
   else if (ATD_TASK_FIRSTPRIVATE(attr_idx) && 
            clause_idx != OPEN_MP_COPYPRIVATE_IDX &&
            clause_idx != OPEN_MP_LASTPRIVATE_IDX &&
            clause_idx != OPEN_MP_FIRSTPRIVATE_IDX) {
      test_clause_idx = OPEN_MP_FIRSTPRIVATE_IDX;
   }
   else if (ATD_TASK_LASTPRIVATE(attr_idx) && 
            clause_idx != OPEN_MP_COPYPRIVATE_IDX &&
            clause_idx != OPEN_MP_LASTPRIVATE_IDX &&
            clause_idx != OPEN_MP_FIRSTPRIVATE_IDX) {
      test_clause_idx = OPEN_MP_LASTPRIVATE_IDX;
   }
   else if (ATD_TASK_COPYIN(attr_idx) && 
            clause_idx != OPEN_MP_COPYIN_IDX) {
      test_clause_idx = OPEN_MP_COPYIN_IDX;
   }
   else if (ATD_TASK_REDUCTION(attr_idx)) {
      test_clause_idx = OPEN_MP_REDUCTION_LIST_IDX;
   }
   else if (ATD_TASK_COPYPRIVATE(attr_idx) &&
            clause_idx != OPEN_MP_COPYPRIVATE_IDX &&
            clause_idx != OPEN_MP_LASTPRIVATE_IDX &&
            clause_idx != OPEN_MP_FIRSTPRIVATE_IDX) {
      test_clause_idx = OPEN_MP_COPYPRIVATE_IDX;
   }


   if (test_clause_idx >= 0) {

      list_idx = IR_IDX_L(SH_IR_IDX(curr_stmt_sh_idx));

      for (i = 0; i < test_clause_idx; i++) {
         list_idx = IL_NEXT_LIST_IDX(list_idx);
      }

      if (list_idx != NULL_IDX &&
          IL_FLD(list_idx) == IL_Tbl_Idx &&
          attr_is_in_list(IL_IDX(list_idx), attr_idx)) {
   
         issue_err = TRUE;
      }
   }

   TRACE (Func_Exit, "multiple_clause_err", NULL);

   return(issue_err);

}  /* multiple_clause_err */

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

static boolean attr_is_in_list(int	list_idx,
                               int	attr_idx)

{
   boolean	its_here = FALSE;
   int		list_idx2;

   TRACE (Func_Entry, "attr_is_in_list", NULL);

   if (IL_FLD(list_idx) == IL_Tbl_Idx) {

      while (list_idx) {
         list_idx2 = IL_IDX(list_idx);

         while (list_idx2) {
            if (IL_FLD(list_idx2) == AT_Tbl_Idx &&
                IL_IDX(list_idx2) == attr_idx) {
               its_here = TRUE;
               break;
            }
            list_idx2 = IL_NEXT_LIST_IDX(list_idx2);
         }
      
         list_idx = IL_NEXT_LIST_IDX(list_idx);
      }
   }
   else {

      while (list_idx) {
      
         if (IL_FLD(list_idx) == AT_Tbl_Idx &&
             IL_IDX(list_idx) == attr_idx) {
            its_here = TRUE;
            break;
         }
         list_idx = IL_NEXT_LIST_IDX(list_idx);
      }
   }


   TRACE (Func_Exit, "attr_is_in_list", NULL);

   return(its_here);

}  /* attr_is_in_list */

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

static void add_common_blk_objects_to_list(int		sb_list_idx,
                                           int		head_list_idx)

{
   int		attr_idx;
   int		col;
   int		line;
   int		list_idx;
   int		prev_list_idx;

   TRACE (Func_Entry, "add_common_blk_objects_to_list", NULL);

   find_opnd_line_and_column(&IL_OPND(sb_list_idx), &line, &col);

# if defined(_DEBUG)
   if (IL_FLD(sb_list_idx) != SB_Tbl_Idx) {
      PRINTMSG(stmt_start_line, 626, Internal, stmt_start_col,
               "SB_Tbl_Idx", "add_common_blk_objects_to_list");
   }
   else if (IL_FLD(head_list_idx) != IL_Tbl_Idx) {
      PRINTMSG(stmt_start_line, 626, Internal, stmt_start_col,
               "IL_Tbl_Idx", "add_common_blk_objects_to_list");
   }
# endif

   attr_idx = SB_FIRST_ATTR_IDX(IL_IDX(sb_list_idx));

   prev_list_idx = sb_list_idx;

   while (attr_idx) {
      NTR_IR_LIST_TBL(list_idx);

      IL_NEXT_LIST_IDX(list_idx) = IL_NEXT_LIST_IDX(prev_list_idx);

      if (IL_NEXT_LIST_IDX(list_idx)) {
        IL_PREV_LIST_IDX(IL_NEXT_LIST_IDX(list_idx)) = list_idx;
      }

      IL_NEXT_LIST_IDX(prev_list_idx) = list_idx;
      IL_PREV_LIST_IDX(list_idx) = prev_list_idx;

      IL_LIST_CNT(head_list_idx)++;
      prev_list_idx = list_idx;

      IL_FLD(list_idx) = AT_Tbl_Idx;
      IL_IDX(list_idx) = attr_idx;
      IL_LINE_NUM(list_idx) = line;
      IL_COL_NUM(list_idx) = col;
      attr_idx = ATD_NEXT_MEMBER_IDX(attr_idx);
   }

   TRACE (Func_Exit, "add_common_blk_objects_to_list", NULL);

   return;

}  /* add_common_blk_objects_to_list */

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

static boolean has_been_reprivatized(int attr_idx)

{
   int		i;
   int		ir_idx;
   int          list_array[OPEN_MP_LIST_CNT];
   int		list_idx;
   boolean	reprivatized = FALSE;
   int		sh_idx;

   TRACE (Func_Entry, "has_been_reprivatized", NULL);

   if (OPND_FLD(cdir_switches.first_sh_blk_stk) == IL_Tbl_Idx &&
       OPND_LIST_CNT(cdir_switches.first_sh_blk_stk) > 1) {

      list_idx = OPND_IDX(cdir_switches.first_sh_blk_stk);
      list_idx = IL_NEXT_LIST_IDX(list_idx);
      sh_idx = IL_IDX(list_idx);
      ir_idx = SH_IR_IDX(sh_idx);

# ifdef _DEBUG
      if (IR_OPR(ir_idx) != Parallel_Open_Mp_Opr) {
         PRINTMSG(stmt_start_line, 626, Internal,stmt_start_col,
                  "Parallel_Open_Mp_Opr",
                  "has_been_reprivatized");
      }
# endif

      list_idx = IR_IDX_L(ir_idx);

      for (i = 0; i < OPEN_MP_LIST_CNT; i++) {
         list_array[i] = list_idx;
         list_idx = IL_NEXT_LIST_IDX(list_idx);
      }

      if (ATD_TASK_PRIVATE(attr_idx)) {
         list_idx = list_array[OPEN_MP_PRIVATE_IDX];

         if (list_idx != NULL_IDX &&
             IL_FLD(list_idx) == IL_Tbl_Idx &&
             attr_is_in_list(IL_IDX(list_idx), attr_idx)) {
            reprivatized = TRUE;
            goto EXIT;
         }
      }

      if (ATD_TASK_FIRSTPRIVATE(attr_idx)) {
         list_idx = list_array[OPEN_MP_FIRSTPRIVATE_IDX];

         if (list_idx != NULL_IDX &&
             IL_FLD(list_idx) == IL_Tbl_Idx &&
             attr_is_in_list(IL_IDX(list_idx), attr_idx)) {
            reprivatized = TRUE;
            goto EXIT;
         }
      }

      if (ATD_TASK_LASTPRIVATE(attr_idx)) {
         list_idx = list_array[OPEN_MP_LASTPRIVATE_IDX];

         if (list_idx != NULL_IDX &&
             IL_FLD(list_idx) == IL_Tbl_Idx &&
             attr_is_in_list(IL_IDX(list_idx), attr_idx)) {
            reprivatized = TRUE;
            goto EXIT;
         }
      }

      if (ATD_TASK_REDUCTION(attr_idx)) {
         list_idx = list_array[OPEN_MP_REDUCTION_LIST_IDX];

         if (list_idx != NULL_IDX &&
             IL_FLD(list_idx) == IL_Tbl_Idx &&
             attr_is_in_list(IL_IDX(list_idx), attr_idx)) {
            reprivatized = TRUE;
            goto EXIT;
         }
      }
   }

EXIT:

   TRACE (Func_Exit, "has_been_reprivatized", NULL);

   return(reprivatized);

}  /* has_been_reprivatized */

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
static void wait_send_semantics(void)

{
   int		column;
   boolean	first_span;
   int		il_idx;
   int		line;
   int		matched;
   int		max_idx;
   long		max_waits;
   long_type	num[MAX_WORDS_FOR_INTEGER];
   long		num_waits	= 0;
   boolean	pointless_wait	= FALSE;
   int		prev_idx;
   boolean	remove;
   long_type	result[MAX_WORDS_FOR_NUMERIC];
   long_type	result1[MAX_WORDS_FOR_NUMERIC];
   int		send_il_idx;
   opnd_type	span_opnd;
   int		type_idx;
   int		type_idx1;
   boolean	variable_send;

   long		max_num_waits	= 65L;


   TRACE (Func_Entry, "wait_send_semantics", NULL);

   if (cdir_switches.wait_list_idx == NULL_IDX &&
       cdir_switches.send_list_idx == NULL_IDX) {

      /* There are no wait/send directives. */

      return;
   }

   OPND_FLD(span_opnd)		= NO_Tbl_Idx;
   OPND_IDX(span_opnd)		= NULL_IDX;
   OPND_LINE_NUM(span_opnd)	= stmt_start_line;
   OPND_COL_NUM(span_opnd)	= stmt_start_col;

   /* For each wait, find one or more sends that match via point */
   /* Check the span to see what the max number of waits is and  */
   /* then check for the maximum number of waits.                */

   if (cdir_switches.wait_list_idx != NULL_IDX) {
      il_idx			= cdir_switches.wait_list_idx;
      variable_send		= FALSE;
      first_span		= TRUE;

      while (il_idx != NULL_IDX) {
         num_waits++;      /* Include all waits, including pointless waits */

         if (first_span) {  /* All the rest must match this one. */
            first_span	= FALSE;

            /* All the rest of the spans must match this one.  The default */
            /* is one, if no SPAN is specified and is set during parse.    */
            /* It must be in the range of 1 to 64.                         */

            COPY_OPND(span_opnd, IR_OPND_R(IL_IDX(il_idx)));

            type_idx	= CG_LOGICAL_DEFAULT_TYPE;

            folder_driver((char *) &CN_CONST(OPND_IDX(span_opnd)),
                                   CN_TYPE_IDX(OPND_IDX(span_opnd)),
                          (char *) &CN_CONST(CN_INTEGER_ONE_IDX),
                                   CN_TYPE_IDX(CN_INTEGER_ONE_IDX),
                                   result,
                                   &type_idx,
                                   OPND_LINE_NUM(span_opnd),
                                   OPND_COL_NUM(span_opnd),
                                   2,
                                   Lt_Opr);

            if (THIS_IS_TRUE(result, type_idx)) {
               find_opnd_line_and_column(&span_opnd, &line, &column);
               PRINTMSG(line, 1532, Error, column);
               OPND_FLD(span_opnd)	= CN_Tbl_Idx;
               OPND_IDX(span_opnd)	= CN_INTEGER_ONE_IDX;
            }
            else {
               C_TO_F_INT(num, 64, CG_INTEGER_DEFAULT_TYPE);
               type_idx	= CG_LOGICAL_DEFAULT_TYPE;

               folder_driver((char *) &CN_CONST(OPND_IDX(span_opnd)),
                                      CN_TYPE_IDX(OPND_IDX(span_opnd)),
                             (char *) &num,
                                      CG_INTEGER_DEFAULT_TYPE,
                                      result,
                                      &type_idx,
                                      OPND_LINE_NUM(span_opnd),
                                      OPND_COL_NUM(span_opnd),
                                      2,
                                      Gt_Opr);

               if (THIS_IS_TRUE(result, type_idx)) {
                  find_opnd_line_and_column(&span_opnd, &line, &column);
                  PRINTMSG(line, 1532, Error, column);
                  OPND_FLD(span_opnd)	= CN_Tbl_Idx;
                  OPND_IDX(span_opnd)	= CN_INTEGER_ONE_IDX;
               }
            }
         }
         else if (IR_FLD_R(IL_IDX(il_idx)) == CN_Tbl_Idx &&
                  OPND_FLD(span_opnd) == CN_Tbl_Idx) {

            if (fold_relationals(IR_IDX_R(IL_IDX(il_idx)),
                                 OPND_IDX(span_opnd),
                                 Ne_Opr)) {
               find_opnd_line_and_column(&IR_OPND_R(IL_IDX(il_idx)),
                                         &line, &column);
               PRINTMSG(line, 1525, Error, column);
            }
         }
         else if (!compare_opnds(&(IR_OPND_R(IL_IDX(il_idx))), &span_opnd)) {
            find_opnd_line_and_column(&IR_OPND_R(IL_IDX(il_idx)),
                                      &line, &column);
            PRINTMSG(line, 1525, Error, column);
         }

         /* Remove matching sends from the send list. */

         send_il_idx	= cdir_switches.send_list_idx;
         prev_idx	= NULL_IDX;
         matched	= FALSE;

         while (send_il_idx != NULL_IDX) {

            if (IR_FLD_L(IL_IDX(send_il_idx)) == IR_FLD_L(IL_IDX(il_idx))) {
               remove	= FALSE;

               switch (IR_FLD_L(IL_IDX(il_idx))) {
               case NO_Tbl_Idx:   /* Pointless */
                  remove		= TRUE;
                  pointless_wait	= TRUE;
                  break;

               case CN_Tbl_Idx:   /* Constant - must be same */
                  remove = fold_relationals(IR_IDX_L(IL_IDX(il_idx)), 
                                            IR_IDX_L(IL_IDX(send_il_idx)),
                                            Eq_Opr);
                  break;

               default:  /* Variable */
                  remove	= TRUE;
                  variable_send	= TRUE;
                  break;
               }

               if (remove) {
                  matched	= TRUE;

                  if (prev_idx == NULL_IDX) {
                     cdir_switches.send_list_idx=IL_NEXT_LIST_IDX(send_il_idx);
                  }
                  else {
                     IL_NEXT_LIST_IDX(prev_idx) = IL_NEXT_LIST_IDX(send_il_idx);
                  }
               }
            }
            send_il_idx = IL_NEXT_LIST_IDX(send_il_idx);
         }

         if (!matched && !variable_send) {

            if (IR_FLD_L(IL_IDX(il_idx)) == NO_Tbl_Idx) {
               line	= IL_LINE_NUM(il_idx);
               column	= IL_COL_NUM(il_idx);
            }
            else {
               find_opnd_line_and_column(&IR_OPND_L(IL_IDX(il_idx)),
                                         &line, &column);
            }
            PRINTMSG(line, 1527, Error, column, "WAIT", "SEND");
         }
         il_idx = IL_NEXT_LIST_IDX(il_idx);
      }
   }

   send_il_idx = cdir_switches.send_list_idx;

   while (send_il_idx != NULL_IDX) { 

      /* Have a send without a wait.  Issue error.  */

      PRINTMSG(IR_LINE_NUM(IL_IDX(send_il_idx)), 1527, Error, 
               IR_COL_NUM(IL_IDX(send_il_idx)), "SEND", "WAIT");
      send_il_idx = IL_NEXT_LIST_IDX(send_il_idx);
   }

   if (OPND_FLD(span_opnd) == CN_Tbl_Idx) {

      /* See the explanation for message 1526.  It describes what is */
      /* being checked in the next if and else clause.               */

      if (fold_relationals(OPND_IDX(span_opnd),
                           CN_INTEGER_ONE_IDX,
                           Eq_Opr)) {

         if (num_waits > (pointless_wait ? max_num_waits : max_num_waits - 1)) {
            find_opnd_line_and_column(&span_opnd, &line, &column);
            PRINTMSG(line, 1526, Error, column,
                     (pointless_wait ? max_num_waits : (max_num_waits-1)));
         }
         max_waits = pointless_wait ? max_num_waits : (max_num_waits - 1);
         max_idx   = C_INT_TO_CN(CG_INTEGER_DEFAULT_TYPE, max_waits);
      }
      else { /* Adjust maximum number of waits based on span value. */
         type_idx	= CG_INTEGER_DEFAULT_TYPE;
         C_TO_F_INT(num, 64, CG_INTEGER_DEFAULT_TYPE);
         folder_driver((char *) &num,
                                CG_INTEGER_DEFAULT_TYPE,
                       (char *) &CN_CONST(OPND_IDX(span_opnd)),
                                CN_TYPE_IDX(OPND_IDX(span_opnd)),
                                result,
                                &type_idx,
                                OPND_LINE_NUM(span_opnd),
                                OPND_COL_NUM(span_opnd),
                                2,
                                Div_Opr);

         if (!pointless_wait) {

            /* If not a pointess wait. Subtract one from the maximum */

            type_idx1	= CG_INTEGER_DEFAULT_TYPE;
            folder_driver((char *) &result,
                                   type_idx,
                          (char *) &CN_CONST(CN_INTEGER_ONE_IDX),
                                   CN_TYPE_IDX(CN_INTEGER_ONE_IDX),
                                   result1,
                                   &type_idx1,
                                   OPND_LINE_NUM(span_opnd),
                                   OPND_COL_NUM(span_opnd),
                                   2,
                                   Minus_Opr);

            max_idx	= ntr_const_tbl(type_idx1,
                                        FALSE,
                                        result1);
         }
         else {
            max_idx	= ntr_const_tbl(type_idx,
                                        FALSE,
                                        result);
         }

         type_idx	= CG_LOGICAL_DEFAULT_TYPE;
         C_TO_F_INT(num, num_waits, CG_INTEGER_DEFAULT_TYPE);

         folder_driver((char *) &num_waits,
                                CG_INTEGER_DEFAULT_TYPE,
                       (char *) &CN_CONST(max_idx),
                                CN_TYPE_IDX(max_idx),
                                result,
                                &type_idx,
                                OPND_LINE_NUM(span_opnd),
                                OPND_COL_NUM(span_opnd),
                                2,
                                Gt_Opr);

         if (THIS_IS_TRUE(result, type_idx)) {
            find_opnd_line_and_column(&span_opnd, &line, &column);
            PRINTMSG(line, 1526, Error, column, CN_INT_TO_C(max_idx));
         }
      }

      if (cdir_switches.wait_list_idx != NULL_IDX) {
         il_idx			= cdir_switches.wait_list_idx;

         while (il_idx != NULL_IDX) {

            if (IR_FLD_L(IL_IDX(il_idx)) == CN_Tbl_Idx) {

               /* Check that point value does not exceed max number of waits */

               type_idx	= CG_LOGICAL_DEFAULT_TYPE;

               folder_driver((char *)&CN_CONST(IR_IDX_L(IL_IDX(il_idx))),
                                      CN_TYPE_IDX(IR_IDX_L(IL_IDX(il_idx))),
                             (char *)&CN_CONST(max_idx),
                                      CN_TYPE_IDX(max_idx),
                                      result,
                                      &type_idx,
                                      IR_LINE_NUM_L(IR_IDX_L(IL_IDX(il_idx))),
                                      IR_COL_NUM_L(IR_IDX_L(IL_IDX(il_idx))),
                                      2,
                                      Gt_Opr);

               if (THIS_IS_TRUE(result, type_idx)) {
                  find_opnd_line_and_column(&IR_OPND_L(IL_IDX(il_idx)),
                                            &line, &column);
                  PRINTMSG(line, 1528, Error, column, CN_INT_TO_C(max_idx));
               }
            }
            il_idx	= IL_NEXT_LIST_IDX(il_idx);
         }
      }
   }

   cdir_switches.wait_list_idx	= NULL_IDX;
   cdir_switches.send_list_idx	= NULL_IDX;

   TRACE (Func_Exit, "wait_send_semantics", NULL);

   return;

}  /* wait_send_semantics */

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

void bounds_cdir_handler(int	ir_idx)

{
   int		attr_idx;
   int		col;
   int		line;
   int		list_idx1;
   int		list_idx2;

   TRACE (Func_Entry, "bounds_cdir_handler", NULL);

   line = IR_LINE_NUM(ir_idx);
   col = IR_COL_NUM(ir_idx);

   if (IR_OPR(ir_idx) == Bounds_Cdir_Opr) {

      if (IR_FLD_L(ir_idx) == IL_Tbl_Idx) {
         list_idx1 = IR_IDX_L(ir_idx);

         while (list_idx1) {
            attr_idx = IL_IDX(list_idx1);

            /* if ATD_NOBOUNDS_CHECK set, clear and remove from nobounds list */

            if (ATD_NOBOUNDS_CHECK(attr_idx)) {
               ATD_NOBOUNDS_CHECK(attr_idx) = FALSE;
               list_idx2 = cdir_switches.nobounds_il_list;

               while (list_idx2 != NULL_IDX) {
                  if (IL_IDX(list_idx2) == attr_idx) {
                     /* remove the attr from the list */

                     if (list_idx2 == cdir_switches.nobounds_il_list) {
                        cdir_switches.nobounds_il_list =
                                               IL_NEXT_LIST_IDX(list_idx2);
                        if (cdir_switches.nobounds_il_list) {
                           IL_PREV_LIST_IDX(cdir_switches.nobounds_il_list) =
                                                               NULL_IDX;
                        }
                     }
                     else {
                        IL_NEXT_LIST_IDX(IL_PREV_LIST_IDX(list_idx2)) =
                           IL_NEXT_LIST_IDX(list_idx2);
                        if (IL_NEXT_LIST_IDX(list_idx2)) {
                           IL_PREV_LIST_IDX(IL_NEXT_LIST_IDX(list_idx2)) =
                                       IL_PREV_LIST_IDX(list_idx2);
                        }
                     }
                     FREE_IR_LIST_NODE(list_idx2);

                     break;
                  }
                  list_idx2 = IL_NEXT_LIST_IDX(list_idx2);
               }
            }

            /* now add to bounds list if not already there */

            if (ATD_BOUNDS_CHECK(attr_idx) == FALSE) {
               ATD_BOUNDS_CHECK(attr_idx) = TRUE;

               NTR_IR_LIST_TBL(list_idx2);
               IL_FLD(list_idx2) = AT_Tbl_Idx;
               IL_IDX(list_idx2) = attr_idx;
               IL_LINE_NUM(list_idx2) = line;
               IL_COL_NUM(list_idx2) = col;

               IL_NEXT_LIST_IDX(list_idx2) = cdir_switches.bounds_il_list;
               IL_PREV_LIST_IDX(IL_NEXT_LIST_IDX(list_idx2)) = list_idx2;
               cdir_switches.bounds_il_list = list_idx2;
            }

            list_idx1 = IL_NEXT_LIST_IDX(list_idx1);
         }
      }
      else {
         cdir_switches.bounds = TRUE;

         /* clear the NOBOUNDS flag on all attrs in the nobounds list */

         list_idx1 = cdir_switches.nobounds_il_list;
         cdir_switches.nobounds_il_list = NULL_IDX;

         while (list_idx1) {
            attr_idx = IL_IDX(list_idx1);
            ATD_NOBOUNDS_CHECK(attr_idx) = FALSE;

            list_idx2 = list_idx1;
            list_idx1 = IL_NEXT_LIST_IDX(list_idx1);
            FREE_IR_LIST_NODE(list_idx2);
         }
      }
   }
   else if (IR_OPR(ir_idx) == Nobounds_Cdir_Opr) {
      if (IR_FLD_L(ir_idx) == IL_Tbl_Idx) {
         list_idx1 = IR_IDX_L(ir_idx);

         while (list_idx1) {
            attr_idx = IL_IDX(list_idx1);

            /* if ATD_BOUNDS_CHECK set, clear and remove from bounds list */

            if (ATD_BOUNDS_CHECK(attr_idx)) {
               ATD_BOUNDS_CHECK(attr_idx) = FALSE;
               list_idx2 = cdir_switches.bounds_il_list;

               while (list_idx2 != NULL_IDX) {
                  if (IL_IDX(list_idx2) == attr_idx) {
                     /* remove the attr from the list */

                     if (list_idx2 == cdir_switches.bounds_il_list) {
                        cdir_switches.bounds_il_list =
                                               IL_NEXT_LIST_IDX(list_idx2);
                        if (cdir_switches.bounds_il_list) {
                           IL_PREV_LIST_IDX(cdir_switches.bounds_il_list) =
                                                               NULL_IDX;
                        }
                     }
                     else {
                        IL_NEXT_LIST_IDX(IL_PREV_LIST_IDX(list_idx2)) =
                           IL_NEXT_LIST_IDX(list_idx2);
                        if (IL_NEXT_LIST_IDX(list_idx2)) {
                           IL_PREV_LIST_IDX(IL_NEXT_LIST_IDX(list_idx2)) =
                                       IL_PREV_LIST_IDX(list_idx2);
                        }
                     }
                     FREE_IR_LIST_NODE(list_idx2);

                     break;
                  }
                  list_idx2 = IL_NEXT_LIST_IDX(list_idx2);
               }
            }

            /* now add to nobounds list if not already there */

            if (ATD_NOBOUNDS_CHECK(attr_idx) == FALSE) {
               ATD_NOBOUNDS_CHECK(attr_idx) = TRUE;

               NTR_IR_LIST_TBL(list_idx2);
               IL_FLD(list_idx2) = AT_Tbl_Idx;
               IL_IDX(list_idx2) = attr_idx;
               IL_LINE_NUM(list_idx2) = line;
               IL_COL_NUM(list_idx2) = col;

               IL_NEXT_LIST_IDX(list_idx2) = cdir_switches.nobounds_il_list;
               IL_PREV_LIST_IDX(IL_NEXT_LIST_IDX(list_idx2)) = list_idx2;
               cdir_switches.nobounds_il_list = list_idx2;
            }

            list_idx1 = IL_NEXT_LIST_IDX(list_idx1);
         }
      }
      else {
         cdir_switches.bounds = FALSE;

         /* clear the BOUNDS flag on all attrs in the nobounds list */

         list_idx1 = cdir_switches.bounds_il_list;
         cdir_switches.bounds_il_list = NULL_IDX;

         while (list_idx1) {
            attr_idx = IL_IDX(list_idx1);
            ATD_BOUNDS_CHECK(attr_idx) = FALSE;

            list_idx2 = list_idx1;
            list_idx1 = IL_NEXT_LIST_IDX(list_idx1);
            FREE_IR_LIST_NODE(list_idx2);
         }
      }
   }
# ifdef _DEBUG
   else {
      PRINTMSG(line, 626, Internal, col,
               "Bounds_Cdir_Opr or Nobounds_Cdir_Opr",
               "bounds_cdir_handler");
   }
# endif

   TRACE (Func_Exit, "bounds_cdir_handler", NULL);

   return;

}  /* bounds_cdir_handler */
