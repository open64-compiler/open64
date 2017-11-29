/*
 *  Copyright (C) 2006, 2007. QLogic Corporation. All Rights Reserved.
 */

/*
 * Copyright 2003, 2004, 2005, 2006 PathScale, Inc.  All Rights Reserved.
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



static char USMID[] = "\n@(#)5.0_pl/sources/s_ctl_flow.c	5.13	10/12/99 10:54:10\n";

# include "defines.h"		/* Machine dependent ifdefs */

# include "host.m"		/* Host machine dependent macros.*/
# include "host.h"		/* Host machine dependent header.*/
# include "target.m"		/* Target machine dependent macros.*/
# include "target.h"		/* Target machine dependent header.*/
# include "fmath.h"             /* Get HUGE values for various kind types.    */

# include "globals.m"
# include "tokens.m"
# include "sytb.m"
# include "s_globals.m"
# include "debug.m"

# include "globals.h"
# include "tokens.h"
# include "sytb.h"
# include "s_globals.h"


/*****************************************************************\
|* Function prototypes of static functions declared in this file *|
\*****************************************************************/

static void	case_value_range_semantics (int, int, int);
static void	chk_for_unlabeled_stmt (void);
static boolean	do_loop_expr_semantics (int, int, opnd_type *);
static void	insert_on_left (int, int, int);
static void	setup_interchange_level_list(opnd_type);
static void	clear_cdir_switches(void);
static void     short_circuit_high_level_if(void);
static boolean  check_stat_variable(int, opnd_type *, int);
static void     asg_opnd_to_tmp(int, opnd_type *, int, int, sh_position_type);
static void     gen_Dv_Set_stmt(opnd_type *, operator_type, int, opnd_type *,
                                sh_position_type);
static boolean	check_forall_triplet_for_index(opnd_type *);
static boolean	gen_forall_max_expr(int, opnd_type *);
static void	gen_forall_branch_around(opnd_type *);
static boolean	gen_forall_tmp_bd_entry(expr_arg_type *,int *, int, int);
static void	determine_lb_ub(int, int, int);
static boolean	forall_mask_needs_tmp(opnd_type *);
static void	process_attr_links(opnd_type *);
static int	gen_forall_derived_type(int, int, int, int);

# ifndef _HIGH_LEVEL_DO_LOOP_FORM
static int	calculate_iteration_count (int, int, int, int, int);
static int	convert_to_do_var_type (int, int); 
# endif

/***************************************\
|* Static variables used in this file. *|
\***************************************/

# ifndef _HIGH_LEVEL_DO_LOOP_FORM
static int      preamble_start_sh_idx;
static int      preamble_end_sh_idx;
# endif

static int	dt_counter = 0;

extern void     (*stmt_semantics[]) ();

extern boolean	processing_do_var;
extern boolean  has_present_opr;

# ifdef _WHIRL_HOST64_TARGET64
extern int double_stride;
# endif /* _WHIRL_HOST64_TARGET64 */


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

void allocate_stmt_semantics (void)

{
#ifdef KEY /* Bug 10177 */
   int			alloc_obj_idx = 0;
#else /* KEY Bug 10177 */
   int			alloc_obj_idx;
#endif /* KEY Bug 10177 */
   int			attr_idx;
   int		  	bd_idx;
#ifdef KEY /* Bug 10177 */
   int		  	bd_list_idx = 0;
#else /* KEY Bug 10177 */
   int		  	bd_list_idx;
#endif /* KEY Bug 10177 */
   int			cn_idx;
   int		  	col;
   opnd_type      	dope_opnd;
   int		  	dv_idx;
   expr_arg_type  	exp_desc;
   boolean              has_pe_ref = FALSE;
   boolean              has_normal_ref = FALSE;
   int		  	i;
   int		  	ir_idx;
   int		  	lb_list_idx;
   opnd_type		len_opnd;
   int		  	line;
   int		  	list_idx;
   int		  	list_idx2;
   int		  	loc_idx;
   int		  	max_idx;
   int		  	mult_idx;
   opnd_type      	opnd;
   opnd_type      	opnd2;
   int			pe_bd_idx = NULL_IDX;
   int		  	plus_idx;
   opnd_type      	prev_xt_opnd;
   int			ptee_bd_idx = NULL_IDX;
   int		  	save_curr_stmt_sh_idx;
   boolean        	semantically_correct	= TRUE;
   int		  	stat_col;
   int            	stat_line;
   int		  	stat_list_idx;
   opnd_type      	stat_opnd;
   size_offset_type	stride;
   opnd_type      	stride_opnd;
   int		  	tmp_idx;
   int		  	ub_list_idx;
   opnd_type      	xt_opnd;


   TRACE (Func_Entry, "allocate_stmt_semantics", NULL);

   ir_idx = SH_IR_IDX(curr_stmt_sh_idx);

   /* check stat var */

   NTR_IR_LIST_TBL(stat_list_idx);
   IL_FLD(stat_list_idx) = CN_Tbl_Idx;
   IL_IDX(stat_list_idx) = CN_INTEGER_ZERO_IDX;
   IL_LINE_NUM(stat_list_idx) = IR_LINE_NUM(ir_idx);
   IL_COL_NUM(stat_list_idx) = IR_COL_NUM(ir_idx);

   if (IR_FLD_R(ir_idx) != NO_Tbl_Idx) {
      check_stat_variable(ir_idx, &stat_opnd, stat_list_idx);
      find_opnd_line_and_column(&stat_opnd, &stat_line, &stat_col);
   }

   list_idx = IR_IDX_L(ir_idx);

   while (list_idx != NULL_IDX) {

      COPY_OPND(opnd, IL_OPND(list_idx));
      exp_desc.rank = 0;
      xref_state    = CIF_Symbol_Modification;
      semantically_correct = expr_semantics(&opnd, &exp_desc)
                             && semantically_correct;
      COPY_OPND(IL_OPND(list_idx), opnd);

      if (exp_desc.rank != 0) {
         find_opnd_line_and_column((opnd_type *) &IL_OPND(list_idx),
                                   &line,
                                   &col);

         PRINTMSG(line, 404, Error, col);
         semantically_correct = FALSE;
      }

      if (IR_FLD_R(ir_idx) != NO_Tbl_Idx                              &&
          OPND_FLD(stat_opnd) != NO_Tbl_Idx                           &&
          cmp_ref_trees(&stat_opnd,
                        (opnd_type *)&IR_OPND_L(IL_IDX(list_idx)))) {

         /* stat var can't alloc obj in same stmt */
         PRINTMSG(stat_line, 413, Error, stat_col);
         semantically_correct = FALSE;
      }

      attr_idx	= find_left_attr(&opnd);

      if (AT_OBJ_CLASS(attr_idx) == Data_Obj && ATD_PURE(attr_idx)) {
         semantically_correct = FALSE;
         find_opnd_line_and_column(&opnd, &line, &col);
         PRINTMSG(line, 1270, Error, col,
                  AT_OBJ_NAME_PTR(attr_idx),
                  ATP_PURE(SCP_ATTR_IDX(curr_scp_idx)) ? "pure" : "elemental");
         goto EXIT;
      }

      if (!semantically_correct) {
         goto EXIT;
      }

      attr_idx				= find_base_attr(&opnd, &line, &col);
      ATD_PTR_ASSIGNED(attr_idx)	= TRUE;
      bd_idx				= ATD_ARRAY_IDX(attr_idx);

# ifdef _F_MINUS_MINUS
      if (ATD_ALLOCATABLE(attr_idx) &&
          ATD_PE_ARRAY_IDX(attr_idx) != NULL_IDX) {
         pe_bd_idx = ATD_PE_ARRAY_IDX(ATD_VARIABLE_TMP_IDX(attr_idx));
         ptee_bd_idx = ATD_ARRAY_IDX(ATD_VARIABLE_TMP_IDX(attr_idx));
         has_pe_ref = TRUE;
      }
      else {
         has_normal_ref = TRUE;
         pe_bd_idx = NULL_IDX;
         ptee_bd_idx = NULL_IDX;
      }
# endif

      /* fill in bound info for each dimension */

      while (OPND_FLD(opnd) == IR_Tbl_Idx &&
             (IR_OPR(OPND_IDX(opnd)) == Whole_Substring_Opr ||
              IR_OPR(OPND_IDX(opnd)) == Whole_Subscript_Opr)) {
         COPY_OPND(opnd, IR_OPND_L(OPND_IDX(opnd)));
      }

      if (OPND_FLD(opnd)         == IR_Tbl_Idx     &&
          IR_OPR(OPND_IDX(opnd)) == Alloc_Obj_Opr) {

         alloc_obj_idx = OPND_IDX(opnd);
         COPY_OPND(dope_opnd, IR_OPND_L(OPND_IDX(opnd)));

         bd_list_idx = IR_IDX_R(OPND_IDX(opnd));

         if (OPND_FLD(dope_opnd) == IR_Tbl_Idx &&
             IR_OPR(OPND_IDX(dope_opnd)) == Dv_Deref_Opr) {
    
            COPY_OPND(dope_opnd, IR_OPND_L(OPND_IDX(dope_opnd)));
         }
         else {
            find_opnd_line_and_column(&opnd, &line, &col);
            PRINTMSG(line, 626, Internal, col,
                     "Dv_Deref_Opr", "allocate_stmt_semantics");
         }
      }
      else {
         find_opnd_line_and_column(&opnd, &line, &col);
         PRINTMSG(line, 626, Internal, col,
                  "Alloc_Obj_Opr", "allocate_stmt_semantics");
      }

      find_opnd_line_and_column(&dope_opnd, &line, &col);

      if (bd_idx || pe_bd_idx) {

         /* set the a_contig flag to TRUE */

         OPND_FLD(opnd2) = CN_Tbl_Idx;
         OPND_IDX(opnd2) = CN_INTEGER_ONE_IDX;
         OPND_LINE_NUM(opnd2) = line;
         OPND_COL_NUM(opnd2)  = col;

         gen_Dv_Set_stmt(&dope_opnd, Dv_Set_A_Contig, 0, &opnd2, Before);

         for (i = 1; i <= IR_LIST_CNT_R(OPND_IDX(opnd)); i++) {

            if (IL_FLD(bd_list_idx) == IL_Tbl_Idx) {
               /* have a colon */

               if (IL_FLD(IL_IDX(bd_list_idx)) == NO_Tbl_Idx) {
                  /* have just upper bound */
                  lb_list_idx = NULL_IDX;
               }
               else {
                  lb_list_idx = IL_IDX(bd_list_idx);
               }

               if (IL_FLD(IL_NEXT_LIST_IDX(IL_IDX(bd_list_idx)))
                                                      == NO_Tbl_Idx) {

                  /* have :*    */
                  ub_list_idx = NULL_IDX;
               }
               else {
                  ub_list_idx = IL_NEXT_LIST_IDX(IL_IDX(bd_list_idx));
               }
            }
            else if (IL_FLD(bd_list_idx) == NO_Tbl_Idx) {
               /* have [*] */
               lb_list_idx = NULL_IDX;
               ub_list_idx = NULL_IDX;
            }
            else {
               /* have just upper bound */
               lb_list_idx = NULL_IDX;
               ub_list_idx = bd_list_idx;
            }

            if (! IL_PE_SUBSCRIPT(bd_list_idx)) {
               if (lb_list_idx == NULL_IDX) {
                  OPND_FLD(opnd2) = CN_Tbl_Idx;
                  OPND_IDX(opnd2) = CN_INTEGER_ONE_IDX;
                  OPND_LINE_NUM(opnd2) = line;
                  OPND_COL_NUM(opnd2)  = col;
               }
               else {
                  COPY_OPND(opnd2, IL_OPND(lb_list_idx));
               }

               gen_Dv_Set_stmt(&dope_opnd, Dv_Set_Low_Bound, i, &opnd2, Before);
            }

            if (pe_bd_idx) {
               if (IL_PE_SUBSCRIPT(bd_list_idx)) {
                  tmp_idx = BD_LB_IDX(pe_bd_idx, i - BD_RANK(bd_idx));
               }
               else {
                  tmp_idx = BD_LB_IDX(ptee_bd_idx, i);
               }

               if (lb_list_idx == NULL_IDX) {
                  OPND_FLD(opnd2) = CN_Tbl_Idx;
                  OPND_IDX(opnd2) = CN_INTEGER_ONE_IDX;
                  OPND_LINE_NUM(opnd2) = line;
                  OPND_COL_NUM(opnd2)  = col;
               }
               else {
                  COPY_OPND(opnd2, IL_OPND(lb_list_idx));
               }

               asg_opnd_to_tmp(tmp_idx, &opnd2, line, col, Before);

               if (IL_PE_SUBSCRIPT(bd_list_idx)) {
                  tmp_idx = BD_UB_IDX(pe_bd_idx, i - BD_RANK(bd_idx));
               }
               else {
                  tmp_idx = BD_UB_IDX(ptee_bd_idx, i);
               }

               if (ub_list_idx != NULL_IDX) {
                  asg_opnd_to_tmp(tmp_idx, &IL_OPND(ub_list_idx),
                                  line, col, Before);
               }
            }

            if (ub_list_idx == NULL_IDX) {
               /* intentionally blank */
            }
            else if (lb_list_idx) {
               /* make expression for extent */
               /* upper - lower + 1 */
               plus_idx = gen_ir(IR_Tbl_Idx,
                              gen_ir(IL_FLD(ub_list_idx), IL_IDX(ub_list_idx),
                                  Minus_Opr, SA_INTEGER_DEFAULT_TYPE, line, col,
                                     IL_FLD(lb_list_idx), IL_IDX(lb_list_idx)),
                               Plus_Opr, SA_INTEGER_DEFAULT_TYPE, line, col,
                                 CN_Tbl_Idx, CN_INTEGER_ONE_IDX);

               NTR_IR_TBL(max_idx);
               IR_OPR(max_idx) = Max_Opr;
               IR_TYPE_IDX(max_idx) = SA_INTEGER_DEFAULT_TYPE;
               IR_LINE_NUM(max_idx) = line;
               IR_COL_NUM(max_idx) = col;
               IR_FLD_L(max_idx) = IL_Tbl_Idx;
               IR_LIST_CNT_L(max_idx) = 2;

               NTR_IR_LIST_TBL(list_idx2);
               IR_IDX_L(max_idx) = list_idx2;
               IL_FLD(list_idx2) = CN_Tbl_Idx;
               IL_IDX(list_idx2) = CN_INTEGER_ZERO_IDX;
               IL_LINE_NUM(list_idx2) = line;
               IL_COL_NUM(list_idx2) = col;

               NTR_IR_LIST_TBL(IL_NEXT_LIST_IDX(list_idx2));
               IL_PREV_LIST_IDX(IL_NEXT_LIST_IDX(list_idx2)) = list_idx2;
               list_idx2 = IL_NEXT_LIST_IDX(list_idx2);

               IL_FLD(list_idx2) = IR_Tbl_Idx;
               IL_IDX(list_idx2) = plus_idx;

               OPND_FLD(xt_opnd) = IR_Tbl_Idx;
               OPND_IDX(xt_opnd) = max_idx;

               exp_desc.rank        = 0;
               xref_state           = CIF_No_Usage_Rec;
               semantically_correct = expr_semantics(&xt_opnd, &exp_desc);
            }
            else {
               /* use upper bound for extent */

               NTR_IR_TBL(max_idx);
               IR_OPR(max_idx) = Max_Opr;
               IR_TYPE_IDX(max_idx) = SA_INTEGER_DEFAULT_TYPE;
               IR_LINE_NUM(max_idx) = line;
               IR_COL_NUM(max_idx) = col;
               IR_FLD_L(max_idx) = IL_Tbl_Idx;
               IR_LIST_CNT_L(max_idx) = 2;

               NTR_IR_LIST_TBL(list_idx2);
               IR_IDX_L(max_idx) = list_idx2;
               IL_FLD(list_idx2) = CN_Tbl_Idx;
               IL_IDX(list_idx2) = CN_INTEGER_ZERO_IDX;
               IL_LINE_NUM(list_idx2) = line;
               IL_COL_NUM(list_idx2) = col;

               NTR_IR_LIST_TBL(IL_NEXT_LIST_IDX(list_idx2));
               IL_PREV_LIST_IDX(IL_NEXT_LIST_IDX(list_idx2)) = list_idx2;
               list_idx2 = IL_NEXT_LIST_IDX(list_idx2);

               COPY_OPND(IL_OPND(list_idx2), IL_OPND(ub_list_idx));

               OPND_FLD(xt_opnd) = IR_Tbl_Idx;
               OPND_IDX(xt_opnd) = max_idx;

               exp_desc.rank        = 0;
               xref_state           = CIF_No_Usage_Rec;
               semantically_correct = expr_semantics(&xt_opnd, &exp_desc);
            }

            if (! IL_PE_SUBSCRIPT(bd_list_idx)) {
               gen_Dv_Set_stmt(&dope_opnd, Dv_Set_Extent, i, &xt_opnd, Before);
            }

            if (pe_bd_idx) {
               if (IL_PE_SUBSCRIPT(bd_list_idx)) {
                  tmp_idx = BD_XT_IDX(pe_bd_idx, i - BD_RANK(bd_idx));
               }
               else {
                  tmp_idx = BD_XT_IDX(ptee_bd_idx, i);
               }

               if (ub_list_idx == NULL_IDX) {
                  OPND_FLD(xt_opnd) = CN_Tbl_Idx;
                  OPND_IDX(xt_opnd) = CN_INTEGER_ONE_IDX;
                  OPND_LINE_NUM(xt_opnd) = line;
                  OPND_COL_NUM(xt_opnd) = col;
               }
               else {
                  asg_opnd_to_tmp(tmp_idx, &xt_opnd, line, col, Before);
               }

               if (i == 1 ||
                   i == BD_RANK(bd_idx) + 1) {

                  COPY_OPND(len_opnd, xt_opnd);
               }
               else {
                  mult_idx = gen_ir(OPND_FLD(len_opnd), OPND_IDX(len_opnd),
                                 Mult_Opr, SA_INTEGER_DEFAULT_TYPE,line,col,
                                    OPND_FLD(xt_opnd), OPND_IDX(xt_opnd));

                  OPND_FLD(len_opnd)       = IR_Tbl_Idx;
                  OPND_IDX(len_opnd)       = mult_idx;
               }

               if (i == BD_RANK(bd_idx) ||
                   i == BD_RANK(bd_idx) + BD_RANK(pe_bd_idx)) {

                  if (IL_PE_SUBSCRIPT(bd_list_idx)) {
                     tmp_idx = BD_LEN_IDX(pe_bd_idx);
                  }
                  else {
                     tmp_idx = BD_LEN_IDX(ptee_bd_idx);
                  }
                  exp_desc.rank            = 0;
                  xref_state               = CIF_No_Usage_Rec;
                  semantically_correct = expr_semantics(&len_opnd, &exp_desc) &&
                                         semantically_correct;

                  asg_opnd_to_tmp(tmp_idx, &len_opnd, line, col, Before);
               }
            }


            if (i == 1) {
# ifdef _WHIRL_HOST64_TARGET64
               double_stride = 1;
# endif /* _WHIRL_HOST64_TARGET64 */

               set_stride_for_first_dim(ATD_TYPE_IDX(attr_idx), &stride);

# ifdef _WHIRL_HOST64_TARGET64
               double_stride = 0;
# endif /* _WHIRL_HOST64_TARGET64 */

               gen_opnd(&stride_opnd, stride.idx, stride.fld, line, col);
            }
            else if (pe_bd_idx &&
                     i == BD_RANK(bd_idx) + 1) {
              gen_opnd(&stride_opnd, CN_INTEGER_ONE_IDX, CN_Tbl_Idx, line, col);
            }
            else {
               /* Create Stride * Extent */
               mult_idx = gen_ir(OPND_FLD(stride_opnd), OPND_IDX(stride_opnd),
                              Mult_Opr, SA_INTEGER_DEFAULT_TYPE,line,col,
                                 OPND_FLD(prev_xt_opnd),OPND_IDX(prev_xt_opnd));

               OPND_FLD(stride_opnd)    = IR_Tbl_Idx;
               OPND_IDX(stride_opnd)    = mult_idx;
               exp_desc.rank            = 0;
               xref_state               = CIF_No_Usage_Rec;

               semantically_correct = expr_semantics(&stride_opnd, &exp_desc) &&
                                      semantically_correct;
            }

            if (! IL_PE_SUBSCRIPT(bd_list_idx)) {
               gen_Dv_Set_stmt(&dope_opnd, Dv_Set_Stride_Mult, i,
                               &stride_opnd, Before);
            }

            if (pe_bd_idx) {
               if (IL_PE_SUBSCRIPT(bd_list_idx)) {
                  tmp_idx = BD_SM_IDX(pe_bd_idx, i - BD_RANK(bd_idx));
               }
               else {
                  tmp_idx = BD_SM_IDX(ptee_bd_idx, i);
               }

               asg_opnd_to_tmp(tmp_idx, &stride_opnd, line, col, Before);
            }


            COPY_OPND(prev_xt_opnd, xt_opnd);
            bd_list_idx = IL_NEXT_LIST_IDX(bd_list_idx);
         }
      }

      if (pe_bd_idx) {
         /* set the ptr to BASE dope vector */

         save_curr_stmt_sh_idx = curr_stmt_sh_idx;

         tmp_idx = ATD_PTR_IDX(ATD_VARIABLE_TMP_IDX(attr_idx));

         dv_idx = gen_ir(OPND_FLD(dope_opnd), OPND_IDX(dope_opnd),
                       Dv_Access_Base_Addr, CG_INTEGER_DEFAULT_TYPE,line,col,
                         NO_Tbl_Idx, NULL_IDX);

         OPND_FLD(opnd2) = IR_Tbl_Idx;
         OPND_IDX(opnd2) = dv_idx;

         asg_opnd_to_tmp(tmp_idx, &opnd2, line, col, After);

         curr_stmt_sh_idx = save_curr_stmt_sh_idx;
      }

      /* fill in new dope vectors */

      if (TYP_TYPE(ATD_TYPE_IDX(attr_idx)) == Structure           &&
          (ATT_POINTER_CPNT(TYP_IDX(ATD_TYPE_IDX(attr_idx))) ||
#ifdef KEY /* Bug 6845 */
          ATT_ALLOCATABLE_CPNT(TYP_IDX(ATD_TYPE_IDX(attr_idx))) ||
#endif /* KEY Bug 6845 */
           ATT_DEFAULT_INITIALIZED(TYP_IDX(ATD_TYPE_IDX(attr_idx))))) {

         COPY_OPND(opnd, IR_OPND_L(alloc_obj_idx));

         if (ATD_ARRAY_IDX(attr_idx) != NULL_IDX) {
            semantically_correct = gen_whole_subscript(&opnd, &exp_desc)
                                   && semantically_correct;
         }

         save_curr_stmt_sh_idx = curr_stmt_sh_idx;

         process_cpnt_inits(&opnd,
                            TYP_IDX(ATD_TYPE_IDX(attr_idx)),
                            gen_dv_whole_def_init,
			    Asg_Opr,
                            After);

         curr_stmt_sh_idx = save_curr_stmt_sh_idx;
      }

      /* replace allocate obj with loc of dope vector */

      NTR_IR_TBL(loc_idx);
      IR_OPR(loc_idx) = Aloc_Opr;
      IR_TYPE_IDX(loc_idx) = CRI_Ptr_8;
      IR_LINE_NUM(loc_idx) = line;
      IR_COL_NUM(loc_idx)  = col;
      COPY_OPND(IR_OPND_L(loc_idx), dope_opnd);

      IL_FLD(list_idx) = IR_Tbl_Idx;
      IL_IDX(list_idx) = loc_idx;

      list_idx = IL_NEXT_LIST_IDX(list_idx);
   }

   if (glb_tbl_idx[Allocate_Attr_Idx] == NULL_IDX) {
      glb_tbl_idx[Allocate_Attr_Idx] = create_lib_entry_attr(ALLOCATE_LIB_ENTRY,
                                                             ALLOCATE_NAME_LEN,
                                                             line,
                                                             col);
   }

   ADD_ATTR_TO_LOCAL_LIST(glb_tbl_idx[Allocate_Attr_Idx]);

   if (has_pe_ref && has_normal_ref) {
      /* must pull the normal refs off on their own call */
      gen_split_alloc(ir_idx,
                      glb_tbl_idx[Allocate_Attr_Idx],
                      stat_list_idx);
   }

# ifdef _ALLOCATE_IS_CALL
   set_up_allocate_as_call(ir_idx,
                           glb_tbl_idx[Allocate_Attr_Idx],
                           stat_list_idx,
                           has_pe_ref);
# else

   NTR_IR_LIST_TBL(list_idx);
   IR_FLD_R(ir_idx) = IL_Tbl_Idx;
   IR_IDX_R(ir_idx) = list_idx;
   IR_LIST_CNT_R(ir_idx) = 3;

   IL_FLD(list_idx) = AT_Tbl_Idx;
   IL_IDX(list_idx) = glb_tbl_idx[Allocate_Attr_Idx];
   IL_LINE_NUM(list_idx) = line;
   IL_COL_NUM(list_idx)  = col;
 
   NTR_IR_LIST_TBL(IL_NEXT_LIST_IDX(list_idx));
   IL_PREV_LIST_IDX(IL_NEXT_LIST_IDX(list_idx)) = list_idx;
   list_idx = IL_NEXT_LIST_IDX(list_idx);

   IL_FLD(list_idx) = CN_Tbl_Idx;
   IL_IDX(list_idx) = gen_alloc_header_const(Integer_8,
                                             IR_LIST_CNT_L(ir_idx),
                                             has_pe_ref,
                                             &cn_idx);
   IL_LINE_NUM(list_idx) = line;
   IL_COL_NUM(list_idx)  = col;

   IL_NEXT_LIST_IDX(list_idx) = stat_list_idx;
   IL_PREV_LIST_IDX(stat_list_idx) = list_idx;

# endif


EXIT:

   TRACE (Func_Exit, "allocate_stmt_semantics", NULL);

   return;

}  /* allocate_stmt_semantics */


/******************************************************************************\
|*									      *|
|* Description:								      *|
|*	Check the conditional expression to make sure that it is of an        *|
|*      acceptable numeric type and that it is scalar.			      *|
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

void arith_if_stmt_semantics (void)

{
   int			br_aif_idx;
   int			col;
   opnd_type		cond_expr;
   expr_arg_type        exp_desc;
   int			line;


   TRACE (Func_Entry, "arith_if_stmt_semantics", NULL);
   
   /* If the arithmetic IF is followed by a stmt that is not labeled, issue   */
   /* a warning message (at the following stmt) that the stmt can not be      */
   /* reached.							              */

   chk_for_unlabeled_stmt();

   /* The conditional expression must be scalar and of a numeric type other   */
   /* than complex.							      */
   
   br_aif_idx = SH_IR_IDX(curr_stmt_sh_idx);
   COPY_OPND(cond_expr, IR_OPND_L(br_aif_idx));
   exp_desc.rank = 0;
   xref_state    = CIF_Symbol_Reference;

   if (expr_semantics(&cond_expr, &exp_desc)) {

      COPY_OPND(IR_OPND_L(br_aif_idx), cond_expr);

      find_opnd_line_and_column(&cond_expr, &line, &col);

      if (exp_desc.type != Integer  &&  exp_desc.type != Real) {

         /* CRI extension:  The "type" of the expression may be typeless.     */
         /* PDGCS treats the expression (result) as an integer.               */
         /* If the expression is a typeless constant that is longer than a    */
         /* word, truncate it and reenter it as an integer.                   */

         if (exp_desc.type != Typeless) {
            PRINTMSG(line, 409, Error, col);
         }
         else if (exp_desc.linear_type == Long_Typeless) {
            IR_IDX_L(br_aif_idx) =
               ntr_const_tbl(CG_INTEGER_DEFAULT_TYPE,
                             FALSE,
                             &CN_CONST(IR_IDX_L(br_aif_idx)));
         }
         else if (exp_desc.linear_type == Short_Typeless_Const) {
            IR_IDX_L(br_aif_idx) = 
               cast_typeless_constant(IR_IDX_L(br_aif_idx),
                                      INTEGER_DEFAULT_TYPE,
                                      line,
                                      col);
         }
      } 

      if (exp_desc.rank != 0) {
         PRINTMSG(IR_LINE_NUM(br_aif_idx), 410, Error,
                  IR_COL_NUM(br_aif_idx));
      }

   }

   TRACE (Func_Exit, "arith_if_stmt_semantics", NULL);

   return;

}  /* arith_if_stmt_semantics */


/******************************************************************************\
|*                                                                            *|
|* Description:                                                               *|
|*      This procedure performs semantic checks on an ASSIGN statement:       *|
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
|* Algorithm notes:							      *|
|*      The semantic checks made in this routine are very similar to those    *|
|*      made in goto_stmt_semantics for the assigned GO TO.  If you make a    *|
|*      change here, chances are the same (or similar) change will need to be *|
|*      made to the assigned GO TO code.				      *|
|*                                                                            *|
\******************************************************************************/

void    assign_stmt_semantics (void)

{
   expr_arg_type	asg_var_desc;
   opnd_type		asg_var_opnd;
   int 			attr_idx;
   int			column;
   int			ir_idx;
   int			label_idx;
   int			line;
   int			loc_idx;
   int			msg_num;

# if (defined(_TARGET_OS_IRIX) || defined(_TARGET_OS_LINUX) || defined(_TARGET_OS_DARWIN))
   int			tmp_idx;
# endif

	
   TRACE (Func_Entry, "assign_stmt_semantics", NULL);

   ir_idx            = SH_IR_IDX(curr_stmt_sh_idx);
   COPY_OPND(asg_var_opnd, IR_OPND_R(ir_idx));
   asg_var_desc.rank = 0;
   xref_state        = CIF_Symbol_Reference;

   if (expr_semantics(&asg_var_opnd, &asg_var_desc)) {

      switch (OPND_FLD(asg_var_opnd)) {

         case AT_Tbl_Idx:
            COPY_OPND(IR_OPND_R(ir_idx), asg_var_opnd);
            attr_idx = OPND_IDX(asg_var_opnd);

            if (AT_OBJ_CLASS(attr_idx) == Data_Obj                          &&
# ifdef _TARGET_OS_MAX   
                /* addresses on MPP are > 32 bits !!  */
                TYP_LINEAR(ATD_TYPE_IDX(attr_idx)) == Integer_8		    &&
# else 
                TYP_LINEAR(ATD_TYPE_IDX(attr_idx)) == INTEGER_DEFAULT_TYPE  &&
# endif
                asg_var_desc.rank == 0) { 

               IR_TYPE_IDX(ir_idx) = INTEGER_DEFAULT_TYPE;

               if ( ! check_for_legal_define(&asg_var_opnd)) {
                  /* intentionally blank */
               }
               else {
      
                  /* If the ASSIGN label is OK and it's defined on an         */
                  /* executable stmt and it doesn't already exist in the      */
                  /* ASSIGN label chain then add it to (the beginning of) the */
                  /* chain.  This chain is needed by PDGCS (the interface     */
                  /* gives it to them at each assigned GOTO).		      */
                  /* Note:  Can't use ATL_NEXT_ASG_LBL_IDX being NULL_IDX to  */
                  /* determine whether or not the label already exists in the */
                  /* chain because this field is NULL_IDX in the last entry   */
                  /* in the chain.  If the last label appeared in a second    */
                  /* ASSIGN stmt, the code would add it to the chain again.   */
 
                  label_idx = IR_IDX_L(ir_idx);

                  if (! AT_DCL_ERR(label_idx)  &&  ATL_EXECUTABLE(label_idx)  &&
                      ! ATL_IN_ASSIGN_LBL_CHAIN(label_idx)) {
                     ATL_NEXT_ASG_LBL_IDX(label_idx)     =
                        SCP_ASSIGN_LBL_CHAIN(curr_scp_idx);
                     SCP_ASSIGN_LBL_CHAIN(curr_scp_idx) = label_idx;
                     ATL_IN_ASSIGN_LBL_CHAIN(label_idx)  = TRUE;
                  }

                  if (! AT_DCL_ERR(label_idx)  &&
                      ATL_CLASS(label_idx) == Lbl_Format) {
                     IR_OPR(ir_idx)         = Asg_Opr;

# if (defined(_TARGET_OS_IRIX) || defined(_TARGET_OS_LINUX) || defined(_TARGET_OS_DARWIN))

                     if (storage_bit_size_tbl[asg_var_desc.linear_type] !=
                            storage_bit_size_tbl[SA_INTEGER_DEFAULT_TYPE]) {

                        if (ATD_ASSIGN_TMP_IDX(attr_idx) == NULL_IDX) {

                           tmp_idx = gen_compiler_tmp(stmt_start_line, 
                                                      stmt_start_col,
                                                      Shared, TRUE);
                           AT_SEMANTICS_DONE(tmp_idx)= TRUE;
                           ATD_TYPE_IDX(tmp_idx)     = SA_INTEGER_DEFAULT_TYPE;
                           ATD_STOR_BLK_IDX(tmp_idx) =
                                              SCP_SB_STACK_IDX(curr_scp_idx);
                           ATD_ASSIGN_TMP_IDX(attr_idx) = tmp_idx;
                        }
                        else {
                           tmp_idx = ATD_ASSIGN_TMP_IDX(attr_idx);
                        }

                        IR_FLD_L(ir_idx) = AT_Tbl_Idx;
                        IR_IDX_L(ir_idx) = tmp_idx;
                     }
                     else {
                        COPY_OPND(IR_OPND_L(ir_idx), IR_OPND_R(ir_idx));
                     }
# else
                     COPY_OPND(IR_OPND_L(ir_idx), IR_OPND_R(ir_idx));
# endif
                     NTR_IR_TBL(loc_idx);
                     IR_OPR(loc_idx)        = Aloc_Opr;
                     IR_TYPE_IDX(loc_idx)   = CRI_Ptr_8;
                     IR_LINE_NUM(loc_idx)   = IR_LINE_NUM(ir_idx);
                     IR_COL_NUM(loc_idx)    = IR_COL_NUM(ir_idx);
                     IR_FLD_R(ir_idx)       = IR_Tbl_Idx;
                     IR_IDX_R(ir_idx)       = loc_idx;
# ifdef _ACSET
                     /* For ACSET, ATL_FORMAT_TMP holds the CN idx */
                     IR_FLD_L(loc_idx)      = CN_Tbl_Idx;
# else
                     IR_FLD_L(loc_idx)      = AT_Tbl_Idx;
# endif

                     IR_IDX_L(loc_idx)      = ATL_FORMAT_TMP(label_idx);
                     IR_LINE_NUM_L(loc_idx) = IR_LINE_NUM(ir_idx);
                     IR_COL_NUM_L(loc_idx)  = IR_COL_NUM(ir_idx);
                  }
               }
            }
            else {
# if defined(_TARGET_OS_MAX)
               msg_num =  (TYP_LINEAR(ATD_TYPE_IDX(attr_idx)) != Integer_8 &&
                           asg_var_desc.rank == 0) ? 1666 : 142;
# else
               msg_num = 142;
# endif

               PRINTMSG(IR_LINE_NUM_R(ir_idx), msg_num, Error,
                        IR_COL_NUM_R(ir_idx),
                        AT_OBJ_NAME_PTR(attr_idx));
            }

            break;


         case CN_Tbl_Idx:
            find_opnd_line_and_column(&asg_var_opnd, &line, &column);
            PRINTMSG(line, 569, Error, column,
                     AT_OBJ_NAME_PTR(IR_IDX_R(ir_idx)));
            break;

         case IR_Tbl_Idx:
            /* Only case should be a Whole_Subscript IR.                      */

            PRINTMSG(IR_LINE_NUM_R(ir_idx), 142, Error, IR_COL_NUM_R(ir_idx),
                     AT_OBJ_NAME_PTR(IR_IDX_R(ir_idx)));
            break;

         default:
            find_opnd_line_and_column(&asg_var_opnd, &line, &column);
            PRINTMSG(line, 179, Internal, column,
                     "assign_stmt_semantics");

      }
   }

   TRACE (Func_Exit, "assign_stmt_semantics", NULL);

   return;

}  /* assign_stmt_semantics */


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

void call_stmt_semantics (void)

{
   expr_arg_type  exp_desc;
   opnd_type	  opnd;

   TRACE (Func_Entry, "call_stmt_semantics", NULL);

   OPND_FLD(opnd) = IR_Tbl_Idx;
   OPND_IDX(opnd) = SH_IR_IDX(curr_stmt_sh_idx);

   exp_desc = init_exp_desc;

   xref_state = CIF_Symbol_Reference;
   call_list_semantics(&opnd, &exp_desc, FALSE);

   SH_IR_IDX(curr_stmt_sh_idx) = OPND_IDX(opnd);
       
   TRACE (Func_Exit, "call_stmt_semantics", NULL);

   return;

}  /* call_stmt_semantics */


/******************************************************************************\
|*									      *|
|* Description:								      *|
|*	This function performs semantics analysis on a CASE statement's       *|
|*      case values.							      *|
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

void case_stmt_semantics (void)

{
   int			column;
   int			curr_il_idx;
   expr_arg_type	expr_desc;
   int 			ir_idx;
   int			line;
   int			nested_select_ir_idx;
   int			new_il_idx;
   opnd_type		opnd;
   int			select_ir_idx;
   
  
   TRACE (Func_Entry, "case_stmt_semantics", NULL);

   /* Upon entry to this procedure, the SELECT CASE statement header points   */
   /* at a Select IR that is used as a temporary place to hang the info about */
   /* the entire SELECT CASE.  The left operand of the Select IR points at    */
   /* actual Select IR.  						      */
   
   select_ir_idx        = SH_IR_IDX(SH_PARENT_BLK_IDX(curr_stmt_sh_idx));
   nested_select_ir_idx = IR_IDX_L(select_ir_idx);
   ir_idx               = SH_IR_IDX(curr_stmt_sh_idx);


   /* Get the type of the SELECT CASE expression and stuff it into the Case   */
   /* IR.  We'll still check later to make sure the user got the types right. */
   /* If the user got it wrong, putting the correct type into the IR won't    */
   /* matter 'cause we'll never get beyond the front-end.		      */

   if (! SH_ERR_FLG(SH_PARENT_BLK_IDX(curr_stmt_sh_idx))) {

      IR_TYPE_IDX(ir_idx) = IR_TYPE_IDX(nested_select_ir_idx);
   }


   COPY_OPND(opnd, IR_OPND_L(ir_idx));

   expr_mode      = Initialization_Expr;
   expr_desc.rank = 0;

   switch (OPND_FLD(opnd)) {

      case NO_Tbl_Idx:					/* CASE DEFAULT       */
         break;

      case CN_Tbl_Idx:
         expr_desc.type_idx	= CN_TYPE_IDX(OPND_IDX(opnd));
         expr_desc.type		= TYP_TYPE(expr_desc.type_idx);
         expr_desc.linear_type	= TYP_LINEAR(expr_desc.type_idx);
         break;

      case AT_Tbl_Idx:
         xref_state = CIF_Symbol_Reference;

         if (expr_semantics(&opnd, &expr_desc)) {
                  
            if (expr_desc.constant) {
               COPY_OPND(IR_OPND_L(ir_idx), opnd);
            }
            else {

               /* Did not resolve to a named constant.			      */

               find_opnd_line_and_column(&opnd, &line, &column);
               PRINTMSG(line, 811, Error, column);
               goto EXIT;
            }
         }
         else {
            goto EXIT;
         }

         break;

      case IR_Tbl_Idx:
         if (IR_OPR(OPND_IDX(opnd)) == Case_Range_Opr) {

            IR_TYPE_IDX(OPND_IDX(opnd)) = IR_TYPE_IDX(ir_idx);

            if (! SH_ERR_FLG(SH_PARENT_BLK_IDX(curr_stmt_sh_idx))  &&
                TYP_TYPE(IR_TYPE_IDX(nested_select_ir_idx)) == Logical) {
               find_opnd_line_and_column(&opnd, &line, &column);
               PRINTMSG(line, 764, Error, column);
            }
            else {
               NTR_IR_LIST_TBL(new_il_idx);
               COPY_OPND(IL_OPND(new_il_idx), opnd);
               case_value_range_semantics(OPND_IDX(opnd),
                                          new_il_idx,
                                          select_ir_idx);
            }

            goto EXIT;
         }
         else {
            xref_state = CIF_Symbol_Reference;

            if (expr_semantics(&opnd, &expr_desc)) {

               if (OPND_FLD(opnd) == CN_Tbl_Idx) {
                  COPY_OPND(IR_OPND_L(ir_idx), opnd);
               }
               else { /* Issue err if it did not resolve to a named constant. */
                  PRINTMSG(IR_LINE_NUM_L(ir_idx), 811, Error,
                           IR_COL_NUM_L(ir_idx));
                  goto EXIT;
               }
            }
            else {
               goto EXIT;
            }

         }

         break;

      default:
         PRINTMSG(IR_LINE_NUM_R(ir_idx), 179, Internal, 
                  IR_COL_NUM_R(ir_idx), "case_stmt_semantics");
   }  

   /* If this case-value is CASE DEFAULT, ignore it.			      */

   if (IR_FLD_L(ir_idx) == NO_Tbl_Idx) {
      goto EXIT;
   }

   /* The case-value expression must be scalar.				      */
   /* Note that if the current CASE is a case-value-range, it has already     */
   /* been completely processed by case_value_range_semantics.		      */

   if (expr_desc.rank != 0) {
      find_opnd_line_and_column(&opnd, &line, &column);
      PRINTMSG(line, 766, Error, column);
   }

   /* The case-value must be type integer, character, or logical.	      */

   if (expr_desc.type == Integer  ||  expr_desc.type == Character  ||
       expr_desc.type == Logical) {

      /* If the SELECT CASE stmt is OK, verify that the type of the           */
      /* case-value is the same as the SELECT CASE expression.		      */

      if (! SH_ERR_FLG(SH_PARENT_BLK_IDX(curr_stmt_sh_idx))  &&
          expr_desc.type != TYP_TYPE(IR_TYPE_IDX(nested_select_ir_idx))) {
         find_opnd_line_and_column(&opnd, &line, &column);
         PRINTMSG(line, 745, Error, column);
      }

   }
   else {

      /* Extension:  We'll also allow a BOZ constant (but NOT the X, trailing */
      /* B, Hollerith, or character used as Hollerith forms) to match an      */
      /* integer SELECT CASE expression.                                      */

      if (expr_desc.type == Typeless  &&  CN_BOZ_CONSTANT(OPND_IDX(opnd))) {

         if (! SH_ERR_FLG(SH_PARENT_BLK_IDX(curr_stmt_sh_idx))  &&
             TYP_TYPE(IR_TYPE_IDX(nested_select_ir_idx)) != Integer) {
            find_opnd_line_and_column(&opnd, &line, &column);
            PRINTMSG(line, 745, Error, column);
         }
         else if (expr_desc.linear_type == Short_Typeless_Const) {
            find_opnd_line_and_column(&opnd, &line, &column);
            OPND_IDX(opnd) = cast_typeless_constant(OPND_IDX(opnd),
                                                    INTEGER_DEFAULT_TYPE,
                                                    line,
                                                    column);
            
            COPY_OPND(IR_OPND_L(ir_idx), opnd);
            expr_desc.linear_type = INTEGER_DEFAULT_TYPE;
            expr_desc.type_idx    = INTEGER_DEFAULT_TYPE;
            expr_desc.type        = Integer;
         }
      }
      else {
         find_opnd_line_and_column(&opnd, &line, &column);
         PRINTMSG(line, 768, Error, column);
      }

   }

   if (SH_ERR_FLG(curr_stmt_sh_idx)) {
      goto EXIT;
   }

   /* Determine whether or not this case-value is OK (it might conflict with  */
   /* another case-value or might fall within the range of a case-value-      */
   /* range).  								      */

   NTR_IR_LIST_TBL(new_il_idx);
   COPY_OPND(IL_OPND(new_il_idx), IR_OPND_L(ir_idx));
   
   /* If this is the first CASE, just attach the new IL to the dummy Select   */
   /* IR's right operand.						      */

   if (IR_FLD_R(select_ir_idx) == NO_Tbl_Idx) {
      ++IR_LIST_CNT_R(select_ir_idx);
      IR_FLD_R(select_ir_idx) = IL_Tbl_Idx;
      IR_IDX_R(select_ir_idx) = new_il_idx;
      goto EXIT;
   }

   /* See where this case-value fits in with existing case-values.            */

   curr_il_idx = IR_IDX_R(select_ir_idx);

   while (curr_il_idx != NULL_IDX) {

      /* Is the current IL a single value?				      */

      if (IL_FLD(curr_il_idx) == CN_Tbl_Idx) {
         
         /* Yes.							      */
         /* Is the new value type logical?				      */
         /*   Y: Is the new value = current value?			      */
         /*        Y: Error; duplicate case-values.			      */
         /*           {Quit}						      */
         /*        N: --|						      */
         /*   N: Is the new value < current value?			      */
         /*        Y: Insert the new IL ahead of the current IL.	      */
         /*           {Done}						      */
         /*        N: Is the new value = current value?			      */
         /*             Y: Error; duplicate case-values.		      */
         /*                {Quit}					      */
         /*             N: --|						      */
         /* Is the current IL at the end of the list?			      */
         /*   Y: Append the new IL at the end of the list. 		      */
         /*      {Done}							      */
         /*   N: Advance to the next IL in the list.			      */

         if (expr_desc.type == Logical) {
             
            if (THIS_IS_TRUE(&CN_CONST(IL_IDX(new_il_idx)),
                             CN_TYPE_IDX(IL_IDX(new_il_idx))) ==
                THIS_IS_TRUE(&CN_CONST(IL_IDX(curr_il_idx)),
                             CN_TYPE_IDX(IL_IDX(curr_il_idx)))) {

               PRINTMSG(IL_LINE_NUM(new_il_idx), 746, Error, 
                        IL_COL_NUM(new_il_idx), IL_LINE_NUM(curr_il_idx));
               goto EXIT;
            }
        
         }
         else {
            if (fold_relationals(IL_IDX(new_il_idx),
                                 IL_IDX(curr_il_idx), Lt_Opr)) {
                  insert_on_left(new_il_idx, curr_il_idx, select_ir_idx);
                  goto EXIT;
            }
            else if (fold_relationals(IL_IDX(new_il_idx),
                                      IL_IDX(curr_il_idx), Eq_Opr)) {
               PRINTMSG(IL_LINE_NUM(new_il_idx), 746, Error, 
                        IL_COL_NUM(new_il_idx), IL_LINE_NUM(curr_il_idx));
               goto EXIT;
            }

         }

         if (IL_NEXT_LIST_IDX(curr_il_idx) == NULL_IDX) {
            IL_NEXT_LIST_IDX(curr_il_idx) = new_il_idx;
            IL_PREV_LIST_IDX(new_il_idx)  = curr_il_idx;
            ++IR_LIST_CNT_R(select_ir_idx);
            goto EXIT;
         }

      }
      else {

         /* Value in list is a case-value range.			      */
         /* Does the range in the list have a left value?	              */
         /*   Y: Is the new case value < the left value?		      */
         /*        Y: Insert the new IL ahead of the current IL.	      */
         /*           {Done}						      */
         /*        N: --|						      */
         /*   N: --|							      */

         if (IR_FLD_L(IL_IDX(curr_il_idx)) != NO_Tbl_Idx) {

            if (fold_relationals(IL_IDX(new_il_idx), 
                                 IR_IDX_L(IL_IDX(curr_il_idx)), Lt_Opr)) {
               insert_on_left(new_il_idx, curr_il_idx, select_ir_idx);
               goto EXIT;
            }

         }

         /* Does the case-value range in the list have a right value?         */
         /*   Y: Is the new case value > the right value?		      */
         /*        Y: Is the IL in the list at the tail of the list?          */
         /*             Y: Append the new IL to the end of the list.  	      */
         /*                {Done}					      */
         /*             N: Advance to the next IL in the list.		      */
         /*        N: --|						      */
         /*   N:  --|							      */
         /* Error - overlap.	  					      */
         /* {Quit}							      */

         if (IR_FLD_R(IL_IDX(curr_il_idx)) != NO_Tbl_Idx) {
            
            if (fold_relationals(IL_IDX(new_il_idx), 
                                 IR_IDX_R(IL_IDX(curr_il_idx)), Gt_Opr)) { 
               
               if (IL_NEXT_LIST_IDX(curr_il_idx) == NULL_IDX) {
                  IL_NEXT_LIST_IDX(curr_il_idx) = new_il_idx;
                  IL_PREV_LIST_IDX(new_il_idx)  = curr_il_idx;
                  ++IR_LIST_CNT_R(select_ir_idx);
                  goto EXIT;
               }
               else {
                  goto ADVANCE_TO_NEXT_IL;
               }

            }

         }
         
         PRINTMSG(IL_LINE_NUM(new_il_idx), 747, Error,
                  IL_COL_NUM(new_il_idx), IR_LINE_NUM(IL_IDX(curr_il_idx)));
         goto EXIT;
      }

ADVANCE_TO_NEXT_IL:

      curr_il_idx = IL_NEXT_LIST_IDX(curr_il_idx);
   }  /* while */
               
EXIT:

   expr_mode = Regular_Expr;

   TRACE (Func_Exit, "case_stmt_semantics", NULL);

   return;

}  /* case_stmt_semantics */


/******************************************************************************\
|*									      *|
|* Description:								      *|
|*	This routine handles both a user CONTINUE stmt and a compiler-        *|
|*      generated CONTINUE.  Nothing needs to be done for a user CONTINUE.    *|
|*      For a compiler-generated CONTINUE, if the line number has not yet     *|
|*      been filled in, the line and column info of the SH are filled in to   *|
|*      reflect those of the following SH.  Also complete the compiler-       *|
|*      generated label defined on the compiler-generated CONTINUE.	      *|
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
|*      The "next" SH that contains a nonzero line (and column) number must   *|
|*      be searched for, as opposed to just looking at the next SH, because   *|
|*      multiple SHs can exist with line numbers of 0.  For example:          *|
|*									      *|
|*            IF (condition) THEN					      *|
|*              IF (condition) action-stmt				      *|
|*            ELSE							      *|
|*									      *|
|*      produces the following SHs:					      *|
|*									      *|
|*            If_Cstrct_Stmt					              *|
|*            If_Then_Stmt						      *|
|*            If_Stmt							      *|
|*            (action-stmt)						      *|
|*            CG Continue_Stmt          ! Branch-around label for logical IF  *|
|*            CG Goto_Stmt              ! Branch around ELSE                  *|
|*            CG Continue_Stmt          ! Define label for ELSE               *|
|*            If_Else_Stmt						      *|
|*									      *|
\******************************************************************************/

void continue_stmt_semantics (void)

{
   int	col_num;
   int	line_num;
   int	sh_idx;


   TRACE (Func_Entry, "continue_stmt_semantics", NULL);

   if (SH_COMPILER_GEN(curr_stmt_sh_idx)   && 
       (SH_GLB_LINE(curr_stmt_sh_idx) == 0  ||
        IR_LINE_NUM_L(SH_IR_IDX(curr_stmt_sh_idx)) == 0)) { 
      sh_idx = SH_NEXT_IDX(curr_stmt_sh_idx);
    
# ifdef _DEBUG
      if (sh_idx == NULL_IDX) {
         PRINTMSG(SH_GLB_LINE(SH_PREV_IDX(curr_stmt_sh_idx)), 236,
                  Internal, 0);
      }
# endif

      while (SH_GLB_LINE(sh_idx) == 0  ||  SH_COMPILER_GEN(sh_idx)) {
         sh_idx = SH_NEXT_IDX(sh_idx);

# ifdef _DEBUG
         if (sh_idx == NULL_IDX) {
            PRINTMSG(SH_GLB_LINE(SH_PREV_IDX(curr_stmt_sh_idx)), 236,
                     Internal, 0);
         }
# endif
      }

      line_num = SH_GLB_LINE(sh_idx);
      col_num  = SH_COL_NUM(sh_idx);

      if (SH_GLB_LINE(curr_stmt_sh_idx) == 0) {
         SH_GLB_LINE(curr_stmt_sh_idx)            = line_num;
         SH_COL_NUM(curr_stmt_sh_idx)             = col_num;
         IR_LINE_NUM(SH_IR_IDX(curr_stmt_sh_idx)) = line_num;
         IR_COL_NUM(SH_IR_IDX(curr_stmt_sh_idx))  = col_num;
      }

      IR_LINE_NUM_L(SH_IR_IDX(curr_stmt_sh_idx))           = line_num;
      IR_COL_NUM_L(SH_IR_IDX(curr_stmt_sh_idx))            = col_num;
      AT_DEF_LINE(IR_IDX_L(SH_IR_IDX(curr_stmt_sh_idx)))   = line_num;
      AT_DEF_COLUMN(IR_IDX_L(SH_IR_IDX(curr_stmt_sh_idx))) = col_num;
   }

   TRACE (Func_Exit, "continue_stmt_semantics", NULL);

   return;

}  /* continue_stmt_semantics */


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

void deallocate_stmt_semantics (void)

{
   int		  attr_idx;
   int		  cn_idx;
   int		  col;
   opnd_type	  dope_opnd;
   expr_arg_type  exp_desc;
   boolean        has_pe_ref = FALSE;
   boolean        has_normal_ref = FALSE;
   int		  ir_idx;
   int		  line;
   int		  list_idx;
   int		  loc_idx;
   opnd_type      opnd;
   boolean        semantically_correct = TRUE;
   int		  stat_col;
   int            stat_line;
   int            stat_list_idx;
   opnd_type      stat_opnd;

# ifdef _SEPARATE_DEALLOCATES
   int		  list_idx2;
   int		  next_sh_idx;
   opnd_type	  stat_loc_opnd;
# endif


   TRACE (Func_Entry, "deallocate_stmt_semantics", NULL);

   ir_idx = SH_IR_IDX(curr_stmt_sh_idx);

   /* check stat var */

   NTR_IR_LIST_TBL(stat_list_idx);
   IL_FLD(stat_list_idx) = CN_Tbl_Idx;
   IL_IDX(stat_list_idx) = CN_INTEGER_ZERO_IDX;
   IL_LINE_NUM(stat_list_idx) = IR_LINE_NUM(ir_idx);
   IL_COL_NUM(stat_list_idx)  = IR_COL_NUM(ir_idx);

   if (IR_FLD_R(ir_idx) != NO_Tbl_Idx) {
      check_stat_variable(ir_idx, &stat_opnd, stat_list_idx);
      find_opnd_line_and_column(&stat_opnd, &stat_line, &stat_col);
   }
   else {
      stat_opnd = null_opnd;
   }

   list_idx = IR_IDX_L(ir_idx);

   while (list_idx != NULL_IDX) {

      COPY_OPND(opnd, IL_OPND(list_idx));
      exp_desc.rank = 0;
      xref_state    = CIF_Symbol_Modification;
      semantically_correct = expr_semantics(&opnd, &exp_desc)
                             && semantically_correct;
      COPY_OPND(IL_OPND(list_idx), opnd);

      if (exp_desc.rank != 0) {
         find_opnd_line_and_column((opnd_type *) &IL_OPND(list_idx), 
                                   &line, &col);
         PRINTMSG(line, 429, Error, col);
         semantically_correct = FALSE;
      }

      if (IR_FLD_R(ir_idx) != NO_Tbl_Idx                              &&
          OPND_FLD(stat_opnd) != NO_Tbl_Idx                           &&
          cmp_ref_trees(&stat_opnd,
                        (opnd_type *)&IR_OPND_L(IL_IDX(list_idx)))) {

         /* stat var can't alloc obj in same stmt */
         PRINTMSG(stat_line, 427, Error, stat_col);
         semantically_correct = FALSE;
      }

      if (ATP_PURE(SCP_ATTR_IDX(curr_scp_idx)) ||
          ATP_ELEMENTAL(SCP_ATTR_IDX(curr_scp_idx))) {
         attr_idx = find_left_attr(&opnd);

         if (AT_OBJ_CLASS(attr_idx) == Data_Obj && ATD_PURE(attr_idx)) {
            find_opnd_line_and_column(&opnd, &line, &col);
            semantically_correct = FALSE;
            PRINTMSG(line, 1270, Error, col,
                     AT_OBJ_NAME_PTR(attr_idx),
                     ATP_PURE(SCP_ATTR_IDX(curr_scp_idx)) ? "pure":"elemental");
         }
      }

      if (! semantically_correct) {
         goto EXIT;
      }

      attr_idx = find_left_attr(&opnd);

      if (ATD_ALLOCATABLE(attr_idx) &&
          ATD_PE_ARRAY_IDX(attr_idx) != NULL_IDX) {
         has_pe_ref = TRUE;
      }
      else {
         has_normal_ref = TRUE;
      }

      while (OPND_FLD(opnd) == IR_Tbl_Idx &&
             (IR_OPR(OPND_IDX(opnd)) == Whole_Substring_Opr ||
              IR_OPR(OPND_IDX(opnd)) == Whole_Subscript_Opr)) {
         COPY_OPND(opnd, IR_OPND_L(OPND_IDX(opnd)));
      }

      if (OPND_FLD(opnd)         == IR_Tbl_Idx     &&
          IR_OPR(OPND_IDX(opnd)) == Dealloc_Obj_Opr) {

         COPY_OPND(dope_opnd, IR_OPND_L(OPND_IDX(opnd)));

         if (OPND_FLD(dope_opnd) == IR_Tbl_Idx &&
             IR_OPR(OPND_IDX(dope_opnd)) == Dv_Deref_Opr) {

            COPY_OPND(dope_opnd, IR_OPND_L(OPND_IDX(dope_opnd)));
         }
         else {
            find_opnd_line_and_column(&opnd, &line, &col);
            PRINTMSG(line, 626, Internal, col,
                     "Dv_Deref_Opr", "deallocate_stmt_semantics");
         }
      }
      else {
         find_opnd_line_and_column(&opnd, &line, &col);
         PRINTMSG(line, 626, Internal, col,
                  "Dealloc_Obj_Opr", "deallocate_stmt_semantics");
      }

      find_opnd_line_and_column(&dope_opnd, &line, &col);

      /* replace deallocate obj with loc of dope vector */

      NTR_IR_TBL(loc_idx);
      IR_OPR(loc_idx) = Aloc_Opr;
      IR_TYPE_IDX(loc_idx) = CRI_Ptr_8;
      IR_LINE_NUM(loc_idx) = line;
      IR_COL_NUM(loc_idx)  = col;
      COPY_OPND(IR_OPND_L(loc_idx), dope_opnd);

      IL_FLD(list_idx) = IR_Tbl_Idx;
      IL_IDX(list_idx) = loc_idx;

      list_idx = IL_NEXT_LIST_IDX(list_idx);
   }

   if (glb_tbl_idx[Deallocate_Attr_Idx] == NULL_IDX) {
      glb_tbl_idx[Deallocate_Attr_Idx] = create_lib_entry_attr(
                                                    DEALLOCATE_LIB_ENTRY,
                                                    DEALLOCATE_NAME_LEN,
                                                    line,
                                                    col);
   }

   ADD_ATTR_TO_LOCAL_LIST(glb_tbl_idx[Deallocate_Attr_Idx]);

# ifdef _SEPARATE_DEALLOCATES

   list_idx = IR_IDX_L(ir_idx);

   if (list_idx) {
      /* reuse ir_idx. No IF check is needed */

      attr_idx = find_left_attr(&IL_OPND(list_idx));

      if (ATD_ALLOCATABLE(attr_idx) &&
          ATD_PE_ARRAY_IDX(attr_idx) != NULL_IDX) {
         has_pe_ref = TRUE;
      }
      else {
         has_pe_ref = FALSE;
      }

      list_idx2 = gen_il(3, FALSE, line, col,
                        AT_Tbl_Idx, glb_tbl_idx[Deallocate_Attr_Idx],
                        CN_Tbl_Idx, gen_alloc_header_const(Integer_8,
                                                           1, 
                                                           has_pe_ref,
                                                           &cn_idx),
                        IL_FLD(stat_list_idx), IL_IDX(stat_list_idx));

      IR_FLD_R(ir_idx) = IL_Tbl_Idx;
      IR_IDX_R(ir_idx) = list_idx2;
      IR_LIST_CNT_R(ir_idx) = 3;

      IR_IDX_L(ir_idx) = list_idx;
      IR_LIST_CNT_L(ir_idx) = 1;

      list_idx2 = IL_NEXT_LIST_IDX(list_idx);
      IL_PREV_LIST_IDX(list_idx) = NULL_IDX;
      IL_NEXT_LIST_IDX(list_idx) = NULL_IDX;

      list_idx = list_idx2;
   }

   COPY_OPND(stat_loc_opnd, IL_OPND(stat_list_idx));

   while (list_idx) {

      attr_idx = find_left_attr(&IL_OPND(list_idx));

      if (ATD_ALLOCATABLE(attr_idx) &&
          ATD_PE_ARRAY_IDX(attr_idx) != NULL_IDX) {
         has_pe_ref = TRUE;
      }
      else {
         has_pe_ref = FALSE;
      }

      copy_subtree(&stat_loc_opnd, &stat_loc_opnd);

      list_idx2 = IL_NEXT_LIST_IDX(list_idx);

      IL_PREV_LIST_IDX(list_idx) = NULL_IDX;
      IL_NEXT_LIST_IDX(list_idx) = NULL_IDX;

      ir_idx = gen_ir(IL_Tbl_Idx, list_idx,
                  Deallocate_Opr, TYPELESS_DEFAULT_TYPE, line, col,
                      IL_Tbl_Idx, gen_il(3, FALSE, line, col,
                            AT_Tbl_Idx, glb_tbl_idx[Deallocate_Attr_Idx],
                            CN_Tbl_Idx, gen_alloc_header_const(Integer_8,
                                                               1, 
                                                               has_pe_ref,
                                                               &cn_idx),
                            OPND_FLD(stat_loc_opnd), OPND_IDX(stat_loc_opnd)));

      gen_sh(After, Deallocate_Stmt, line, col, FALSE, FALSE, TRUE);
      SH_P2_SKIP_ME(curr_stmt_sh_idx)     = TRUE;
      SH_IR_IDX(curr_stmt_sh_idx)         = ir_idx;

      /* generate an IF (stat == 0) around this deallocate */

      next_sh_idx = SH_NEXT_IDX(curr_stmt_sh_idx);

      if (OPND_FLD(stat_opnd) != NO_Tbl_Idx) {
         copy_subtree(&stat_opnd, &stat_opnd);
         ir_idx = gen_ir(OPND_FLD(stat_opnd), OPND_IDX(stat_opnd),
                     Eq_Opr, LOGICAL_DEFAULT_TYPE, line, col,
                         CN_Tbl_Idx, CN_INTEGER_ZERO_IDX);

         gen_opnd(&opnd, ir_idx, IR_Tbl_Idx, line, col);
         gen_if_stmt(&opnd, 
                     curr_stmt_sh_idx, 
                     curr_stmt_sh_idx, 
                     NULL_IDX,
                     NULL_IDX, 
                     line, 
                     col);
      }

      curr_stmt_sh_idx = SH_PREV_IDX(next_sh_idx);
      list_idx = list_idx2;
   }

# else

   if (has_pe_ref && has_normal_ref) {
      /* must pull the normal refs off on their own call */
      gen_split_alloc(ir_idx,
                      glb_tbl_idx[Deallocate_Attr_Idx],
                      stat_list_idx);
   }

# ifdef _ALLOCATE_IS_CALL
   set_up_allocate_as_call(ir_idx,
                           glb_tbl_idx[Deallocate_Attr_Idx],
                           stat_list_idx,
                           has_pe_ref);
# else

   NTR_IR_LIST_TBL(list_idx);
   IR_FLD_R(ir_idx) = IL_Tbl_Idx;
   IR_IDX_R(ir_idx) = list_idx;
   IR_LIST_CNT_R(ir_idx) = 3;

   IL_FLD(list_idx) = AT_Tbl_Idx;
   IL_IDX(list_idx) = glb_tbl_idx[Deallocate_Attr_Idx];
   IL_LINE_NUM(list_idx) = line;
   IL_COL_NUM(list_idx)  = col;

   NTR_IR_LIST_TBL(IL_NEXT_LIST_IDX(list_idx));
   IL_PREV_LIST_IDX(IL_NEXT_LIST_IDX(list_idx)) = list_idx;
   list_idx = IL_NEXT_LIST_IDX(list_idx);

   IL_FLD(list_idx) = CN_Tbl_Idx;
   IL_IDX(list_idx) = gen_alloc_header_const(Integer_8,
                                             IR_LIST_CNT_L(ir_idx),
                                             has_pe_ref,
                                             &cn_idx);
   IL_LINE_NUM(list_idx) = line;
   IL_COL_NUM(list_idx)  = col;

   IL_NEXT_LIST_IDX(list_idx) = stat_list_idx;
   IL_PREV_LIST_IDX(stat_list_idx) = list_idx;

# endif
# endif

EXIT:

   TRACE (Func_Exit, "deallocate_stmt_semantics", NULL);

   return;

}  /* deallocate_stmt_semantics */


#ifdef KEY /* Bug 4889 */
/*
 * Given a clause of an OMP directive such as "private", look for a
 * variable within the clause which matches the do_var_idx. Return the
 * IL_IDX if found, or NULL_IDX if not.
 */
static int find_omp_clause_matching(int clause, int do_var_idx) {
  if (NULL_IDX == clause) {
    return NULL_IDX;
    }
  for (int vars = IL_IDX(clause); NULL_IDX != vars;
    vars = IL_NEXT_LIST_IDX(vars)) {
    if (AT_Tbl_Idx == IL_FLD(vars) && do_var_idx == IL_IDX(vars)) {
      return vars;
    }
  }
  return NULL_IDX;
}
/*
 * Set list_array to hold the IL_IDX for each clause in the Open MP directive
 * whose IR_IDX is omp_dir_idx; or fill the list_array with NULL_IDX if
 * omp_dir_idx is NULL_IDX.
 */
static void set_list_array(int list_array[], int omp_dir_idx) {
  if (NULL_IDX == omp_dir_idx) {
    for (int i = 0; i < OPEN_MP_LIST_CNT; i += 1) {
      list_array[i] = NULL_IDX;
      }
    return;
    }
  int clauses = IR_IDX_L(omp_dir_idx);
  for (int i = 0; i < OPEN_MP_LIST_CNT; i += 1) {
    list_array[i] = clauses;
    clauses = IL_NEXT_LIST_IDX(clauses);
    }
  }


#endif /* KEY Bug 4889 */
/******************************************************************************\
|*									      *|
|* Description:								      *|
|*	Perform semantic checks on all forms of the DO statement.             *|
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

void do_stmt_semantics (void)

{
   int			column;
   int			do_sh_idx;
   int			do_var_col;
   int                  do_var_idx;
   int			do_var_line;
   boolean		do_var_must_be_int = FALSE;
   opnd_type		do_var_opnd;
#ifdef KEY /* Bug 10177 */
   int			end_idx = 0;
#else /* KEY Bug 10177 */
   int			end_idx;
#endif /* KEY Bug 10177 */
   int			end_il_idx;
   expr_arg_type	exp_desc;
   int			il_idx;
   int			il_idx_2;
   int			inc_idx;
   int			inc_il_idx;
   int			ir_idx;
   int			label_attr;
   int			lc_il_idx;
   int			line;
   int			loop_control_il_idx;
   int			loop_info_idx;
   int			loop_labels_il_idx;
   boolean		semantics_ok;
   int			start_expr_sh_idx;
#ifdef KEY /* Bug 10177 */
   int			start_idx = 0;
#else /* KEY Bug 10177 */
   int			start_idx;
#endif /* KEY Bug 10177 */
   int			start_il_idx;
   opnd_type		temp_opnd;
   int			tmp_idx;

# if defined(_HIGH_LEVEL_DO_LOOP_FORM)
   int			label_idx;
   int			tmp_asg_ir_idx;
# else
   int			asg_idx;
   int			cg_do_var_idx;
   int			expr_ir_idx;
   int			idx;
   int			ir_idx_2;
   int			lbl_il_idx;
   int			loop_temps_il_idx;
   int                  opnd_column;
   int                  opnd_line;
   opnd_type		opnd;
   int			save_curr_stmt_sh_idx;
   int			trip_zero_sh_idx = NULL_IDX;
# endif


   TRACE (Func_Entry, "do_stmt_semantics", NULL);

   do_sh_idx           = curr_stmt_sh_idx;
   loop_info_idx       = SH_IR_IDX(curr_stmt_sh_idx);
   loop_control_il_idx = IR_IDX_R(loop_info_idx);
   loop_labels_il_idx  = IL_NEXT_LIST_IDX(loop_control_il_idx);


# ifndef _HIGH_LEVEL_DO_LOOP_FORM

   preamble_start_sh_idx = NULL_IDX;
   preamble_end_sh_idx   = NULL_IDX;

# endif


   switch (stmt_type)  {

      /* -------------------------------------------------------------------- */
      /*								      */
      /*                         Iterative DO statement			      */
      /*								      */
      /* -------------------------------------------------------------------- */

      case Do_Iterative_Stmt:

         if (IR_IDX_L(SH_IR_IDX(do_sh_idx)) == NULL_IDX) {

            /* If this was a DOALL loop make sure that the parallel region    */
            /* is terminated and cdir_switches.doall_sh_idx is cleared.       */
            /* Clear all the other cdir_switches that would have been         */
            /* cleared by this loop.                                          */

            clear_cdir_switches();
         }

         if (cdir_switches.doall_sh_idx ||
             cdir_switches.doacross_sh_idx ||
             cdir_switches.pdo_sh_idx ||
#ifndef KEY /* Bug 12687 */
	     /* OpenMP standard doesn't prohibit calls to internal procedures */
             cdir_switches.do_omp_sh_idx ||
             cdir_switches.paralleldo_omp_sh_idx ||
#endif /* KEY Bug 12687 */
             cdir_switches.paralleldo_sh_idx) {

            cdir_switches.parallel_region = TRUE;
            cdir_switches.no_internal_calls = TRUE;
            SH_DOALL_LOOP_END(IR_IDX_L(SH_IR_IDX(do_sh_idx))) = TRUE;
         }
#ifdef KEY /* Bug 12687 */
	 else if (cdir_switches.do_omp_sh_idx ||
             cdir_switches.paralleldo_omp_sh_idx) {
            cdir_switches.parallel_region = TRUE;
            SH_DOALL_LOOP_END(IR_IDX_L(SH_IR_IDX(do_sh_idx))) = TRUE;
	 }
#endif /* KEY Bug 12687 */

         if (cdir_switches.do_omp_sh_idx ||
             cdir_switches.paralleldo_omp_sh_idx) {

            do_var_must_be_int = TRUE;
         }

         /* Verify that the data type of the DO-variable is acceptable and    */
         /* make sure the DO-variable is a named scalar.		      */

         lc_il_idx = IL_IDX(loop_control_il_idx);

         do_var_idx = (IL_FLD(lc_il_idx) == AT_Tbl_Idx) ?
                         IL_IDX(lc_il_idx) : NULL_IDX;

#ifdef KEY /* Bug 4889 */
	 /*
	  * Inside an OMP "parallel do" or "do" directive, add a
	  * "private" clause for the do loop index if there isn't one already.
	  */

         if (NULL_IDX != inside_paralleldo && NULL_IDX != do_var_idx) {
	   int parallel_do_idx = SH_IR_IDX(inside_paralleldo);
	   int parallel_idx = SH_IR_IDX(inside_parallel);

	   /* Program need not provide an explicit "enddo" or "endparalleldo"
	     directive, but the directive applies to only the next loop */
	   inside_paralleldo = NULL_IDX;

	   int do_list_array[OPEN_MP_LIST_CNT];
	   int parallel_list_array[OPEN_MP_LIST_CNT];
	   set_list_array(do_list_array, parallel_do_idx);
	   set_list_array(parallel_list_array, parallel_idx);

	   /* Has the "do" variable already been declared "private",
	     "lastprivate", or "firstprivate" in either the "do" directive,
	     the "parallel do" directive, or an enclosing "parallel"
	     directive? */
	   if (NULL_IDX == find_omp_clause_matching(
	       do_list_array[OPEN_MP_PRIVATE_IDX], do_var_idx) &&
	     NULL_IDX == find_omp_clause_matching(
	       do_list_array[OPEN_MP_FIRSTPRIVATE_IDX], do_var_idx) &&
	     NULL_IDX == find_omp_clause_matching(
	       do_list_array[OPEN_MP_LASTPRIVATE_IDX], do_var_idx) &&
	     NULL_IDX == find_omp_clause_matching(
	       parallel_list_array[OPEN_MP_PRIVATE_IDX], do_var_idx) &&
	     NULL_IDX == find_omp_clause_matching(
	       parallel_list_array[OPEN_MP_FIRSTPRIVATE_IDX], do_var_idx) &&
	     NULL_IDX == find_omp_clause_matching(
	       parallel_list_array[OPEN_MP_LASTPRIVATE_IDX], do_var_idx)) {

	     /* No, so add it to the "private" list */
	     int new_var;
	     NTR_IR_LIST_TBL(new_var);
	     int private_list = do_list_array[OPEN_MP_PRIVATE_IDX];
	     int private_vars = IL_IDX(private_list);
	     if (NULL_IDX == private_vars) {
	       /* No list at all, so create first element */
	       IL_FLD(private_list) = IL_Tbl_Idx;
	       IL_NEXT_LIST_IDX(new_var) = NULL_IDX;
	     }
	     else {
	       /* Prepend to existing list */
	       IL_NEXT_LIST_IDX(new_var) = private_vars;
	     }
	     IL_IDX(private_list) = new_var;
	     IL_FLD(new_var) = AT_Tbl_Idx;
	     IL_IDX(new_var) = do_var_idx;
	     IL_LINE_NUM(new_var) = IR_COL_NUM(parallel_do_idx);
	     IL_COL_NUM(new_var) = IR_COL_NUM(parallel_do_idx);
	     IL_LIST_CNT(private_list) = IL_LIST_CNT(private_list) + 1;
	   }
	 }
#endif /* KEY Bug 4889 */

# if defined(_HIGH_LEVEL_DO_LOOP_FORM)
         if (cdir_switches.doall_sh_idx) {
            IR_FLD_R(SH_IR_IDX(cdir_switches.doall_sh_idx)) = AT_Tbl_Idx;
            IR_IDX_R(SH_IR_IDX(cdir_switches.doall_sh_idx)) = do_var_idx;

            IR_LINE_NUM_R(SH_IR_IDX(cdir_switches.doall_sh_idx)) =
                                                               stmt_start_line;
            IR_COL_NUM_R(SH_IR_IDX(cdir_switches.doall_sh_idx)) =
                                                               stmt_start_col;
            insert_sh_chain_before(cdir_switches.doall_sh_idx);

            if (do_var_idx != NULL_IDX &&
                ATD_TASK_SHARED(do_var_idx)) {

               PRINTMSG(IL_LINE_NUM(lc_il_idx), 961, Error, 
                        IL_COL_NUM(lc_il_idx));
            }

            cdir_switches.doall_sh_idx = NULL_IDX;
         }
         else if (cdir_switches.doacross_sh_idx) {
            IR_LINE_NUM_R(SH_IR_IDX(cdir_switches.doacross_sh_idx)) =
                                                               stmt_start_line;
            IR_COL_NUM_R(SH_IR_IDX(cdir_switches.doacross_sh_idx)) =
                                                               stmt_start_col;
            insert_sh_chain_before(cdir_switches.doacross_sh_idx);


            cdir_switches.doacross_sh_idx = NULL_IDX;
         }
         else if (cdir_switches.paralleldo_sh_idx) {
            IR_LINE_NUM_R(SH_IR_IDX(cdir_switches.paralleldo_sh_idx)) =
                                                               stmt_start_line;
            IR_COL_NUM_R(SH_IR_IDX(cdir_switches.paralleldo_sh_idx)) =
                                                               stmt_start_col;
            insert_sh_chain_before(cdir_switches.paralleldo_sh_idx);


            cdir_switches.paralleldo_sh_idx = NULL_IDX;
         }
         else if (cdir_switches.pdo_sh_idx) {
            IR_LINE_NUM_R(SH_IR_IDX(cdir_switches.pdo_sh_idx)) =
                                                               stmt_start_line;
            IR_COL_NUM_R(SH_IR_IDX(cdir_switches.pdo_sh_idx)) =
                                                               stmt_start_col;
            insert_sh_chain_before(cdir_switches.pdo_sh_idx);


            cdir_switches.pdo_sh_idx = NULL_IDX;
         }
         else if (cdir_switches.dopar_sh_idx) {

            IR_FLD_R(SH_IR_IDX(cdir_switches.dopar_sh_idx)) = AT_Tbl_Idx;
            IR_IDX_R(SH_IR_IDX(cdir_switches.dopar_sh_idx)) = do_var_idx;

            IR_LINE_NUM_R(SH_IR_IDX(cdir_switches.dopar_sh_idx)) =
                                                             stmt_start_line;
            IR_COL_NUM_R(SH_IR_IDX(cdir_switches.dopar_sh_idx)) =
                                                             stmt_start_col;
            insert_sh_chain_before(cdir_switches.dopar_sh_idx);
            cdir_switches.dopar_sh_idx = NULL_IDX;
         }
         else if (cdir_switches.do_omp_sh_idx) {

            IR_FLD_R(SH_IR_IDX(cdir_switches.do_omp_sh_idx)) = AT_Tbl_Idx;
            IR_IDX_R(SH_IR_IDX(cdir_switches.do_omp_sh_idx)) = do_var_idx;

            IR_LINE_NUM_R(SH_IR_IDX(cdir_switches.do_omp_sh_idx)) =
                                                             stmt_start_line;
            IR_COL_NUM_R(SH_IR_IDX(cdir_switches.do_omp_sh_idx)) =
                                                             stmt_start_col;
            insert_sh_chain_before(cdir_switches.do_omp_sh_idx);
            cdir_switches.do_omp_sh_idx = NULL_IDX;
         }
         else if (cdir_switches.paralleldo_omp_sh_idx) {

            IR_FLD_R(SH_IR_IDX(cdir_switches.paralleldo_omp_sh_idx)) = 
                                                             AT_Tbl_Idx;
            IR_IDX_R(SH_IR_IDX(cdir_switches.paralleldo_omp_sh_idx)) = 
                                                             do_var_idx;

            IR_LINE_NUM_R(SH_IR_IDX(cdir_switches.paralleldo_omp_sh_idx)) =
                                                             stmt_start_line;
            IR_COL_NUM_R(SH_IR_IDX(cdir_switches.paralleldo_omp_sh_idx)) =
                                                             stmt_start_col;
            insert_sh_chain_before(cdir_switches.paralleldo_omp_sh_idx);
            cdir_switches.paralleldo_omp_sh_idx = NULL_IDX;
         }


         label_idx = gen_internal_lbl(stmt_start_line);
         NTR_IR_TBL(ir_idx);
         IR_OPR(ir_idx)              = Label_Opr;
         IR_TYPE_IDX(ir_idx)         = TYPELESS_DEFAULT_TYPE;
         IR_LINE_NUM(ir_idx)         = stmt_start_line;
         IR_COL_NUM(ir_idx)          = stmt_start_col;
         IR_FLD_L(ir_idx)            = AT_Tbl_Idx;
         IR_IDX_L(ir_idx)            = label_idx;
         IR_COL_NUM_L(ir_idx)        = stmt_start_col;
         IR_LINE_NUM_L(ir_idx)       = stmt_start_line;

         AT_DEFINED(label_idx)       = TRUE;
         ATL_TOP_OF_LOOP(label_idx)  = TRUE;
         AT_REFERENCED(label_idx)    = Not_Referenced;

         gen_sh(Before, Continue_Stmt, stmt_start_line, 
                stmt_start_col, FALSE, FALSE, TRUE);
         SH_P2_SKIP_ME(SH_PREV_IDX(curr_stmt_sh_idx)) = TRUE;
         SH_IR_IDX(SH_PREV_IDX(curr_stmt_sh_idx)) = ir_idx;

         ATL_DEF_STMT_IDX(label_idx) = SH_PREV_IDX(curr_stmt_sh_idx);

         set_directives_on_label(label_idx);
# endif

         if (AT_DCL_ERR(do_var_idx)) {
            SH_ERR_FLG(do_sh_idx) = TRUE;
            goto EXIT;
         }

         COPY_OPND(do_var_opnd, IL_OPND(lc_il_idx));
         exp_desc.rank = 0;
         xref_state    = CIF_Symbol_Modification;
         processing_do_var = TRUE;

         semantics_ok = expr_semantics(&do_var_opnd, &exp_desc);

         processing_do_var = FALSE;

         if (semantics_ok) {

            COPY_OPND(IL_OPND(lc_il_idx), do_var_opnd);

            /* Is it a named constant?         				      */

            if (exp_desc.constant) {
               semantics_ok = FALSE;
               PRINTMSG(IL_LINE_NUM(lc_il_idx), 194, Error,
                        IL_COL_NUM(lc_il_idx));
            }

            if (do_var_must_be_int &&
                exp_desc.type != Integer) {

               PRINTMSG(IL_LINE_NUM(lc_il_idx), 1514, Error,
                        IL_COL_NUM(lc_il_idx));
            }

            /* Is it something of type integer?				      */

            if (exp_desc.type == Integer) {

               if (OPND_FLD(do_var_opnd) == AT_Tbl_Idx) {

                  /* Is it a function reference?			      */

                  if (ATD_CLASS(OPND_IDX(do_var_opnd)) == Compiler_Tmp) {
                     semantics_ok = FALSE;
                     PRINTMSG(IL_LINE_NUM(lc_il_idx), 194, Error,
                              IL_COL_NUM(lc_il_idx));
                  }
               }
               else {
              
                  if (do_var_idx == NULL_IDX) {
                     find_opnd_line_and_column(&do_var_opnd, &line, &column);
                     PRINTMSG(line, 199, Error, column);
                     semantics_ok = FALSE;
                  }
               }
            }

            /* Is it something of type default real or double precision?      */
             
            else if (exp_desc.type == Real  &&
                     (exp_desc.linear_type == REAL_DEFAULT_TYPE  ||
                      exp_desc.linear_type == DOUBLE_DEFAULT_TYPE)) { 

               if (OPND_FLD(do_var_opnd) == AT_Tbl_Idx) {

                  /* Is it a variable?					      */

                  if (ATD_CLASS(OPND_IDX(do_var_opnd)) != Compiler_Tmp) {
                     PRINTMSG(IL_LINE_NUM(lc_il_idx), 1569, Ansi,
                              IL_COL_NUM(lc_il_idx));
                  }
                  else { 
                     semantics_ok = FALSE;
                     PRINTMSG(IL_LINE_NUM(lc_il_idx), 194, Error,
                              IL_COL_NUM(lc_il_idx));
                  }
               }
               else {

                  if (do_var_idx == NULL_IDX) {
                     find_opnd_line_and_column(&do_var_opnd, &line, &column);
                     PRINTMSG(line, 199, Error, column);
                     semantics_ok = FALSE;
                  }
               }
            }
      
            /* Is is a CRI pointer?					      */

            else if (exp_desc.type == CRI_Ptr) {
               find_opnd_line_and_column(&do_var_opnd, &line, &column);
               PRINTMSG(line, 208, Ansi, column);
            }

            /* Nah, the DO variable is of an unapproved data type (like       */
            /* complex, character, or even derived type).		      */

            else {
               semantics_ok = FALSE;
               find_opnd_line_and_column(&do_var_opnd, &line, &column);
               PRINTMSG(line, 219, Error, column);
            }

            if (exp_desc.rank != 0) {
               semantics_ok = FALSE;
               find_opnd_line_and_column(&do_var_opnd, &line, &column);
               PRINTMSG(line, 223, Error, column);
            }
         }

         if (semantics_ok) {

            /* If the DO-variable is OK:				      */
            /*   * The DO-variable may have been host associated or it may be */
            /*     a pointer so grab its possibly updated Attr index.	      */
            /*   * Mark the DO variable's Attr as being a live DO-variable.   */
            /*     (Can only be done if the end-of-loop SH is also NOT marked */
            /*     in error because if it is, the driver will skip it and the */
            /*     "live DO-variable" flag will never get turned off).  So,   */
            /*     if the end-of-loop SH is in error, null the link back from */
            /*     the end-of-loop SH to the DO SH to signal to the driver    */
            /*     there is no DO-variable to turn off.			      */
            /*   * Make sure the DO-variable is not a dummy argument with     */
            /*     INTENT(IN).						      */

            if (OPND_FLD(do_var_opnd) == AT_Tbl_Idx) {
               do_var_idx = OPND_IDX(do_var_opnd);
            }
            else {
               do_var_idx = IR_IDX_L(OPND_IDX(do_var_opnd));
            }

            do_var_line = OPND_LINE_NUM(do_var_opnd);
            do_var_col  = OPND_COL_NUM(do_var_opnd);

            if ( ! check_for_legal_define(&do_var_opnd)) {
               semantics_ok = FALSE;
            }
            else {

               if (IR_FLD_L(SH_IR_IDX(do_sh_idx)) == SH_Tbl_Idx  &&
                   ! SH_ERR_FLG(IR_IDX_L(SH_IR_IDX(do_sh_idx)))) {

                     if (do_var_idx != NULL_IDX) {
                        ATD_LIVE_DO_VAR(do_var_idx) = TRUE;
                     }
               }
            }
         }

         if (! semantics_ok) {
            goto CLEAR_CDIR_SWITCHES;
         }

         /* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */
         /* Check the start expression.					      */
         /* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

         start_il_idx = IL_NEXT_LIST_IDX(lc_il_idx);
         semantics_ok = do_loop_expr_semantics( start_il_idx,
                    			        do_var_idx,
                                               &temp_opnd);

         start_expr_sh_idx = curr_stmt_sh_idx;

         if (semantics_ok) {

            if (OPND_FLD(temp_opnd) == CN_Tbl_Idx) {
               start_idx = OPND_IDX(temp_opnd);
            }
            else {
               start_idx = NULL_IDX;
            }
         }


         /* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */
         /* Check the end expression.					      */
         /* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

         end_il_idx   = IL_NEXT_LIST_IDX(start_il_idx);
         semantics_ok =
            do_loop_expr_semantics(end_il_idx, do_var_idx, &temp_opnd)  && 
            semantics_ok;

         if (semantics_ok) {

            if (start_idx != NULL_IDX  &&  OPND_FLD(temp_opnd) == CN_Tbl_Idx) {
               end_idx = OPND_IDX(temp_opnd);
            }
            else {
               start_idx = NULL_IDX;	       /* Yes, start_idx.  See below. */
            }
         }


         /* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */
         /* Check the increment expression.				      */
         /* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

         inc_idx      = NULL_IDX;
         inc_il_idx   = IL_NEXT_LIST_IDX(end_il_idx);
         semantics_ok =
            do_loop_expr_semantics(inc_il_idx, do_var_idx, &temp_opnd)  &&
            semantics_ok;

         if (semantics_ok) {

            if (OPND_FLD(temp_opnd) == CN_Tbl_Idx) {
               inc_idx = OPND_IDX(temp_opnd);

               if (fold_relationals(OPND_IDX(temp_opnd),
                                    CN_INTEGER_ZERO_IDX,
                                    Eq_Opr)) {
                  PRINTMSG(IL_LINE_NUM(inc_il_idx), 255, Error,
                           IL_COL_NUM(inc_il_idx));
                  semantics_ok = FALSE;
               }

            }
            else {
               start_idx = NULL_IDX;	     /* Yes, start_idx.  See below.   */
            }
         }

         if (! semantics_ok) {
            SH_ERR_FLG(do_sh_idx) = TRUE;

            goto CLEAR_CDIR_SWITCHES;

         }


         /* Generate an assignment statement to initialize the DO-variable.   */

# ifndef _HIGH_LEVEL_DO_LOOP_FORM

         gen_sh(After, Assignment_Stmt, stmt_start_line, stmt_start_col,
                FALSE, FALSE, TRUE);

         NTR_IR_TBL(ir_idx);
         SH_IR_IDX(curr_stmt_sh_idx) = ir_idx;
         IR_OPR(ir_idx)              = Asg_Opr;
         IR_TYPE_IDX(ir_idx)         = ATD_TYPE_IDX(do_var_idx);
         IR_LINE_NUM(ir_idx)         = stmt_start_line;
         IR_COL_NUM(ir_idx)          = stmt_start_col;
         COPY_OPND(IR_OPND_L(ir_idx), do_var_opnd);
         COPY_OPND(IR_OPND_R(ir_idx), IL_OPND(start_il_idx));

         if (cdir_switches.doall_sh_idx ||
             cdir_switches.paralleldo_omp_sh_idx) {

            if (preamble_end_sh_idx == NULL_IDX) {
               gen_opnd(&opnd, curr_stmt_sh_idx, SH_Tbl_Idx, 
                        stmt_start_line, stmt_start_col);
               copy_subtree(&opnd, &opnd);
               preamble_start_sh_idx = OPND_IDX(opnd);
               SH_COMPILER_GEN(preamble_start_sh_idx) = TRUE;
               SH_P2_SKIP_ME(preamble_start_sh_idx) = TRUE;
               preamble_end_sh_idx = preamble_start_sh_idx;
            }
            else {
               gen_opnd(&opnd, curr_stmt_sh_idx, SH_Tbl_Idx, 
                        stmt_start_line, stmt_start_col);
               copy_subtree(&opnd, &opnd);
               idx = OPND_IDX(opnd);
               SH_NEXT_IDX(preamble_end_sh_idx) = idx;

               if (SH_NEXT_IDX(preamble_end_sh_idx)) {
                  SH_PREV_IDX(SH_NEXT_IDX(preamble_end_sh_idx)) = 
                                                    preamble_end_sh_idx;
               }
               preamble_end_sh_idx = SH_NEXT_IDX(preamble_end_sh_idx);
               SH_COMPILER_GEN(preamble_end_sh_idx) = TRUE;
               SH_P2_SKIP_ME(preamble_end_sh_idx) = TRUE;
            }
         }

         /* Produce another IL node at the end of the IL list attached to the */
         /* Loop_Info IR.  The trip count will be saved in an IL attached to  */
         /* this "loop temps" IL node.  The trip count will either be a temp  */
         /* or a constant.						      */

         NTR_IR_LIST_TBL(loop_temps_il_idx);

         if (cif_flags & MISC_RECS) {
            il_idx = IL_NEXT_LIST_IDX(loop_labels_il_idx);
         }
         else {
            il_idx = loop_labels_il_idx;
         }

         IL_NEXT_LIST_IDX(il_idx)            = loop_temps_il_idx;
         IL_PREV_LIST_IDX(loop_temps_il_idx) = il_idx;
         ++IR_LIST_CNT_R(loop_info_idx); 

         NTR_IR_LIST_TBL(il_idx);
         IL_LIST_CNT(loop_temps_il_idx) = 1;
         IL_FLD(loop_temps_il_idx)      = IL_Tbl_Idx;
         IL_IDX(loop_temps_il_idx)      = il_idx;
         IL_LINE_NUM(il_idx)            = stmt_start_line;
         IL_COL_NUM(il_idx)             = stmt_start_col;

# endif 
       

         /* If all 3 loop control expressions are constant, we can check the  */
         /* values to see if the loop will actually be executed and can       */
         /* calculate the iteration count at compile time.		      */

         if (start_idx != NULL_IDX) {

            /* The iteration count is zero for both of the following cases:   */
            /*     start-expr < end-expr  and  inc-expr < 0                   */
            /*     start-expr > end-expr  and  inc-expr > 0		      */

            if ((fold_relationals(start_idx, end_idx, Lt_Opr)  &&
                 fold_relationals(inc_idx, CN_INTEGER_ZERO_IDX, Lt_Opr))  ||
                (fold_relationals(start_idx, end_idx, Gt_Opr)  &&
                 fold_relationals(inc_idx, CN_INTEGER_ZERO_IDX, Gt_Opr))  &&
                ! on_off_flags.exec_doloops_once) {  
               PRINTMSG(stmt_start_line, 254, Caution, stmt_start_col);
               tmp_idx = CN_INTEGER_ZERO_IDX;
            }

# ifndef _HIGH_LEVEL_DO_LOOP_FORM

            else {
               tmp_idx = calculate_iteration_count(do_sh_idx,
					           start_idx,
                                                   end_idx,
                                                   inc_idx,
                                                   do_var_idx);
            }

            IL_FLD(il_idx) = CN_Tbl_Idx;
            IL_IDX(il_idx) = tmp_idx;
            IL_LINE_NUM(il_idx) = stmt_start_line;
            IL_COL_NUM(il_idx) = stmt_start_col;

# endif

         }


# ifndef _HIGH_LEVEL_DO_LOOP_FORM

         /* Possibly generate the "skip the loop?" test.		      */
         /*								      */
         /* * If the increment is constant and is positive, generate:	      */
         /*								      */
         /*      IF ((end - start) < 0) branch around loop		      */
         /*								      */
         /* * If the increment is constant and is negative, generate: 	      */
         /*								      */
         /*      IF ((end - start) > 0) branch around loop		      */
         /*								      */
         /* * If the increment is unknown (a variable), generate:	      */
         /*								      */
         /*      IF (((end - start) .NE. 0)  .AND.			      */
         /*          (XOR(end - start, inc) .LT. 0)) branch around	      */
         /*								      */
         /* The test is generated only if:				      */
         /*   - "one-trip" DO were loops were NOT specified, and	      */
         /*   - at least one of the loop control expressions is		      */
         /*   - the iteration count was calculated and found to be            */
         /*     nonconstant or they're all constant but the interation        */
         /*     count is <= 0 (if the iteration count <= 0, too bad - they    */
         /*     get the IF test for stupidity).				      */

         if (! on_off_flags.exec_doloops_once  &&
             (start_idx == NULL_IDX  ||
              fold_relationals(tmp_idx, CN_INTEGER_ZERO_IDX, Le_Opr))) {

            NTR_IR_TBL(expr_ir_idx);
            IR_OPR(expr_ir_idx)        = Minus_Opr;
            IR_TYPE_IDX(expr_ir_idx)   = ATD_TYPE_IDX(do_var_idx);
            IR_LINE_NUM(expr_ir_idx)   = stmt_start_line;
            IR_COL_NUM(expr_ir_idx)    = stmt_start_col;
            COPY_OPND(IR_OPND_L(expr_ir_idx), IL_OPND(end_il_idx));
            COPY_OPND(IR_OPND_R(expr_ir_idx), IL_OPND(start_il_idx));

            NTR_IR_TBL(ir_idx);

            if (inc_idx != NULL_IDX) {

               if (fold_relationals(inc_idx,
                                    CN_INTEGER_ZERO_IDX,
                                    Ge_Opr)) {
                  IR_OPR(ir_idx) = Lt_Opr;
               }
               else {
                  IR_OPR(ir_idx) = Gt_Opr;
               }
            }
            else {
               IR_OPR(ir_idx) = Ne_Opr;
            }

            IR_TYPE_IDX(ir_idx)   = LOGICAL_DEFAULT_TYPE;
            IR_LINE_NUM(ir_idx)   = stmt_start_line;
            IR_COL_NUM(ir_idx)    = stmt_start_col;
            IR_LINE_NUM_L(ir_idx) = stmt_start_line;
            IR_COL_NUM_L(ir_idx)  = stmt_start_col;
            IR_FLD_L(ir_idx)      = IR_Tbl_Idx;
            IR_IDX_L(ir_idx)      = expr_ir_idx;
            IR_LINE_NUM_R(ir_idx) = stmt_start_line;
            IR_COL_NUM_R(ir_idx)  = stmt_start_col;
            IR_FLD_R(ir_idx)      = CN_Tbl_Idx;
            IR_IDX_R(ir_idx)      = CN_INTEGER_ZERO_IDX;

            if (inc_idx != NULL_IDX) {
               expr_ir_idx = ir_idx;
            }
            else {
               NTR_IR_TBL(expr_ir_idx);
               IR_OPR(expr_ir_idx)        = Minus_Opr;
               IR_TYPE_IDX(expr_ir_idx)   = ATD_TYPE_IDX(do_var_idx);
               IR_LINE_NUM(expr_ir_idx)   = stmt_start_line;
               IR_COL_NUM(expr_ir_idx)    = stmt_start_col;
               COPY_OPND(IR_OPND_L(expr_ir_idx), IL_OPND(end_il_idx));
               COPY_OPND(IR_OPND_R(expr_ir_idx), IL_OPND(start_il_idx));
   
               NTR_IR_TBL(ir_idx_2);
               IR_OPR(ir_idx_2)        = Bneqv_Opr;
               IR_TYPE_IDX(ir_idx_2)   = TYPELESS_DEFAULT_TYPE;
               IR_LINE_NUM(ir_idx_2)   = stmt_start_line;
               IR_COL_NUM(ir_idx_2)    = stmt_start_col;
               IR_LINE_NUM_L(ir_idx_2) = stmt_start_line;
               IR_COL_NUM_L(ir_idx_2)  = stmt_start_col;
               IR_FLD_L(ir_idx_2)      = IR_Tbl_Idx;
               IR_IDX_L(ir_idx_2)      = expr_ir_idx;
               COPY_OPND(IR_OPND_R(ir_idx_2), IL_OPND(inc_il_idx));

               NTR_IR_TBL(expr_ir_idx);
               IR_OPR(expr_ir_idx)        = Lt_Opr;
               IR_TYPE_IDX(expr_ir_idx)   = LOGICAL_DEFAULT_TYPE;
               IR_LINE_NUM(expr_ir_idx)   = stmt_start_line;
               IR_COL_NUM(expr_ir_idx)    = stmt_start_col;
               IR_LINE_NUM_L(expr_ir_idx) = stmt_start_line;
               IR_COL_NUM_L(expr_ir_idx)  = stmt_start_col;
               IR_FLD_L(expr_ir_idx)      = IR_Tbl_Idx;
               IR_IDX_L(expr_ir_idx)      = ir_idx_2;
               IR_LINE_NUM_R(expr_ir_idx) = stmt_start_line;
               IR_COL_NUM_R(expr_ir_idx)  = stmt_start_col;
               IR_FLD_R(expr_ir_idx)      = CN_Tbl_Idx;
               IR_IDX_R(expr_ir_idx)      = CN_INTEGER_ZERO_IDX;

               NTR_IR_TBL(ir_idx_2);
               IR_OPR(ir_idx_2)        = And_Opr;
               IR_TYPE_IDX(ir_idx_2)   = LOGICAL_DEFAULT_TYPE;
               IR_LINE_NUM(ir_idx_2)   = stmt_start_line;
               IR_COL_NUM(ir_idx_2)    = stmt_start_col;
               IR_LINE_NUM_L(ir_idx_2) = stmt_start_line;
               IR_COL_NUM_L(ir_idx_2)  = stmt_start_col;
               IR_FLD_L(ir_idx_2)      = IR_Tbl_Idx;
               IR_IDX_L(ir_idx_2)      = ir_idx;
               IR_LINE_NUM_R(ir_idx_2) = stmt_start_line;
               IR_COL_NUM_R(ir_idx_2)  = stmt_start_col;
               IR_FLD_R(ir_idx_2)      = IR_Tbl_Idx;
               IR_IDX_R(ir_idx_2)      = expr_ir_idx;

               expr_ir_idx = ir_idx_2;
            }  

            NTR_IR_TBL(ir_idx);
            IR_OPR(ir_idx)        = Br_True_Opr;
            IR_TYPE_IDX(ir_idx)   = LOGICAL_DEFAULT_TYPE;
            IR_LINE_NUM(ir_idx)   = stmt_start_line;
            IR_COL_NUM(ir_idx)    = stmt_start_col;
            IR_LINE_NUM_L(ir_idx) = stmt_start_line;
            IR_COL_NUM_L(ir_idx)  = stmt_start_col;
            IR_FLD_L(ir_idx)      = IR_Tbl_Idx;
            IR_IDX_L(ir_idx)      = expr_ir_idx;
            COPY_OPND(IR_OPND_R(ir_idx),
                      IL_OPND(IL_NEXT_LIST_IDX(IL_IDX(loop_labels_il_idx))));

            gen_sh(After, If_Stmt, stmt_start_line, stmt_start_col,
                   FALSE, FALSE, TRUE);

            SH_IR_IDX(curr_stmt_sh_idx) = ir_idx;
         }


         if (start_idx == NULL_IDX) {

            /* Generate the IR tree to calculate the trip count:              */
            /*								      */
            /*  trip-count-tmp = (end-tmp - start-tmp + inc-tmp) / inc-tmp    */
            /*								      */
            /* Even though the whole calculation is temps that have already   */
            /* been developed, the expression must be sent through            */
            /* expr_semantics so data types, etc. will be propagated.         */
            /* If the trip count expression result type is real (including    */
            /* double precision), on CRAYs it must be rounded so that the     */
            /* trip count will match the mathematical calculation.	      */

            NTR_IR_TBL(expr_ir_idx);
            IR_OPR(expr_ir_idx)        = Minus_Opr;
            IR_TYPE_IDX(expr_ir_idx)   = ATD_TYPE_IDX(do_var_idx);
            IR_LINE_NUM(expr_ir_idx)   = stmt_start_line;
            IR_COL_NUM(expr_ir_idx)    = stmt_start_col;
            COPY_OPND(IR_OPND_L(expr_ir_idx), IL_OPND(end_il_idx));
            COPY_OPND(IR_OPND_R(expr_ir_idx), IL_OPND(start_il_idx));

            NTR_IR_TBL(ir_idx);
   
            IR_OPR(ir_idx)        = Plus_Opr;
            IR_TYPE_IDX(ir_idx)   = ATD_TYPE_IDX(do_var_idx);
            IR_LINE_NUM(ir_idx)   = stmt_start_line;
            IR_COL_NUM(ir_idx)    = stmt_start_col;
            IR_LINE_NUM_L(ir_idx) = stmt_start_line;
            IR_COL_NUM_L(ir_idx)  = stmt_start_col;
            IR_FLD_L(ir_idx)      = IR_Tbl_Idx;
            IR_IDX_L(ir_idx)      = expr_ir_idx;
            COPY_OPND(IR_OPND_R(ir_idx), IL_OPND(inc_il_idx));

            expr_ir_idx = ir_idx;

            NTR_IR_TBL(ir_idx);
            IR_OPR(ir_idx)        = Div_Opr;
            IR_TYPE_IDX(ir_idx)   = ATD_TYPE_IDX(do_var_idx);
            IR_LINE_NUM(ir_idx)   = stmt_start_line;
            IR_COL_NUM(ir_idx)    = stmt_start_col;
            IR_LINE_NUM_L(ir_idx) = stmt_start_line;
            IR_COL_NUM_L(ir_idx)  = stmt_start_col;
            IR_FLD_L(ir_idx)      = IR_Tbl_Idx;
            IR_IDX_L(ir_idx)      = expr_ir_idx;
            COPY_OPND(IR_OPND_R(ir_idx), IL_OPND(inc_il_idx));

            expr_ir_idx = ir_idx;

            if (on_off_flags.exec_doloops_once) {
               NTR_IR_TBL(ir_idx);
               IR_OPR(ir_idx)        = Max_Opr;
               IR_TYPE_IDX(ir_idx)   = INTEGER_DEFAULT_TYPE;
               IR_LINE_NUM(ir_idx)   = stmt_start_line;
               IR_COL_NUM(ir_idx)    = stmt_start_col;
               IR_LINE_NUM_L(ir_idx) = stmt_start_line;
               IR_COL_NUM_L(ir_idx)  = stmt_start_col;
   
               NTR_IR_LIST_TBL(il_idx);
               IR_FLD_L(ir_idx)      = IL_Tbl_Idx;
               IR_IDX_L(ir_idx)      = il_idx;
               IL_LINE_NUM(il_idx)   = stmt_start_line;
               IL_COL_NUM(il_idx)    = stmt_start_col;
               IL_FLD(il_idx)        = IR_Tbl_Idx;
               IL_IDX(il_idx)        = expr_ir_idx;
   
               NTR_IR_LIST_TBL(il_idx_2);
               IL_NEXT_LIST_IDX(il_idx)   = il_idx_2;
               IL_PREV_LIST_IDX(il_idx_2) = il_idx;
               IL_LINE_NUM(il_idx_2)      = stmt_start_line;
               IL_COL_NUM(il_idx_2)       = stmt_start_col;
               IL_FLD(il_idx_2)           = CN_Tbl_Idx;
               IL_IDX(il_idx_2)           = CN_INTEGER_ONE_IDX;

               IR_LIST_CNT_L(ir_idx) = 2;

               expr_ir_idx = ir_idx;
            }

            gen_sh(After, Assignment_Stmt, stmt_start_line, stmt_start_col,
                   FALSE, FALSE, TRUE);

# ifdef _TARGET_OS_UNICOS

            GEN_COMPILER_TMP_ASG(ir_idx,
                                 tmp_idx,
                                 FALSE,                /* Do semantics on tmp */
                                 stmt_start_line,
                                 stmt_start_col,
			         (target_triton) ?
				    INTEGER_DEFAULT_TYPE :
                                    Integer_4,          /* At PDGCS' request. */
                                 Priv);

# else

            GEN_COMPILER_TMP_ASG(ir_idx,
                                 tmp_idx,
                                 FALSE,       /* Do semantics on tmp */
                                 stmt_start_line,
                                 stmt_start_col,
                                 INTEGER_DEFAULT_TYPE,
                                 Priv);

# endif

            SH_IR_IDX(curr_stmt_sh_idx) = ir_idx;
            IR_LINE_NUM_R(ir_idx)       = stmt_start_line;
            IR_COL_NUM_R(ir_idx)        = stmt_start_col;
            IR_FLD_R(ir_idx)            = IR_Tbl_Idx;
            IR_IDX_R(ir_idx)            = expr_ir_idx;


            /* Save the temp that represents the iteration count in the list  */
            /* of loop temps.						      */

            il_idx              = IL_IDX(loop_temps_il_idx); 
            IL_FLD(il_idx)      = AT_Tbl_Idx;
            IL_IDX(il_idx)      = tmp_idx;
            IL_LINE_NUM(il_idx) = stmt_start_line;
            IL_COL_NUM(il_idx)  = stmt_start_col;

     
            /* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -  */
            /* Now submit the trip count calculation to expr_semantics.	      */
            /* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -  */

            if (on_off_flags.exec_doloops_once) {           /* Get to Max IR. */
               ir_idx              = IR_IDX_R(ir_idx);
               IR_TYPE_IDX(ir_idx) = INTEGER_DEFAULT_TYPE; 
               COPY_OPND(temp_opnd, IL_OPND(IR_IDX_L(ir_idx)));
            }
            else {
               COPY_OPND(temp_opnd, IR_OPND_R(ir_idx));
            }

            exp_desc.rank  = 0;
            xref_state     = CIF_No_Usage_Rec;

            if (expr_semantics(&temp_opnd, &exp_desc)) {

# if defined(_TARGET_OS_UNICOS) 

               if (exp_desc.type == Real  && 
                   (exp_desc.linear_type == REAL_DEFAULT_TYPE  ||
                    exp_desc.linear_type == DOUBLE_DEFAULT_TYPE)) {
                  IR_OPR(OPND_IDX(temp_opnd)) = Real_Div_To_Int_Opr;
               }

#endif 

               if (on_off_flags.exec_doloops_once) {
                  COPY_OPND(IL_OPND(IR_IDX_L(ir_idx)), temp_opnd);
               }
               else {
                  COPY_OPND(IR_OPND_R(ir_idx), temp_opnd);
               }
            }
            else {
               PRINTMSG(SH_GLB_LINE(curr_stmt_sh_idx), 224, Internal, 0);
            }
         }
 

         /* Generate the assignment of 0 to the induction temp.               */

         gen_sh(After, Assignment_Stmt, stmt_start_line, stmt_start_col,
                FALSE, FALSE, TRUE);

# ifdef _TARGET_OS_UNICOS

         GEN_COMPILER_TMP_ASG(ir_idx,
                              tmp_idx,
                              FALSE,                   /* Do semantics on tmp */
                              stmt_start_line,
                              stmt_start_col,
                              (target_triton) ?
                                 INTEGER_DEFAULT_TYPE :
                                 Integer_4,          /* At PDGCS' request. */
                                 Priv);

# else

         GEN_COMPILER_TMP_ASG(ir_idx,
                              tmp_idx,
                              FALSE,       /* Do semantics on tmp */
                              stmt_start_line,
                              stmt_start_col,
                              INTEGER_DEFAULT_TYPE,
                              Priv);
# endif

# if defined(CDIR_INTERCHANGE)

         /* This is only necessary for pdgcs based platforms.  This     */
         /* sets up the level list to match the do list.  For example   */
         /* if the user specifies  interchange(k,i,j) and the do's are  */
         /* nested like  do i, do j, do k, then the level list should   */
         /* read 2, 3, 1 (as in i is 2nd in the list, j is 3rd in the   */
         /* list and k is 1st in the list).                             */
      
         setup_interchange_level_list(do_var_opnd);
# endif

         SH_IR_IDX(curr_stmt_sh_idx)       = ir_idx;
         IR_LINE_NUM_R(ir_idx)             = stmt_start_line;
         IR_COL_NUM_R(ir_idx)              = stmt_start_col;
         IR_FLD_R(ir_idx)                  = CN_Tbl_Idx;
         IR_IDX_R(ir_idx)                  = CN_INTEGER_ZERO_IDX;

         trip_zero_sh_idx = curr_stmt_sh_idx;

         /* Add another IL to the list attached to the "loop temps" IL node   */
         /* to save the induction temp.					      */

         NTR_IR_LIST_TBL(il_idx_2);
         ++IL_LIST_CNT(loop_temps_il_idx);
         il_idx                     = IL_IDX(loop_temps_il_idx);
         IL_NEXT_LIST_IDX(il_idx)   = il_idx_2;
         IL_PREV_LIST_IDX(il_idx_2) = il_idx;
         IL_LINE_NUM(il_idx_2)      = stmt_start_line;
         IL_COL_NUM(il_idx_2)       = stmt_start_col;
         IL_FLD(il_idx_2)           = AT_Tbl_Idx;
         IL_IDX(il_idx_2)           = tmp_idx;

 
         /* Save the induction temp for use with the DOALL or DOPARALLEL      */
         /* CMIC$.							      */
         /* If CMIC$ DOALL was specified:     				      */
         /*    (1) If the loop is being executed at least once (there is no   */
         /*        zero trip test), then the IR generated for the DOALL when  */
         /*        the CMIC DOALL was processed is inserted before the        */
         /*        compiler-generated assignment statement that freezes the   */
         /*        start expression in a temp, 				      */
         /* or (2) If the loop might not be executed, the IR generated for    */
         /*        the DOALL is inserted ahead of the top-of-loop label and   */
         /*        the loop preamble IR is duplicated and inserted after the  */
         /*        DOALL IR (within the parallel region).     		      */
         /*								      */
         /* If CMIC$ DOPARALLEL was specified the IR generated for the        */
         /* DOPARALLEL is inserted before the top-of-loop label.              */

         cg_do_var_idx  = tmp_idx;

         if (cdir_switches.doall_sh_idx) {

            IR_FLD_R(SH_IR_IDX(cdir_switches.doall_sh_idx)) = AT_Tbl_Idx;
            IR_IDX_R(SH_IR_IDX(cdir_switches.doall_sh_idx)) = cg_do_var_idx;
            IR_LINE_NUM_R(SH_IR_IDX(cdir_switches.doall_sh_idx)) = 
                                                               stmt_start_line;
            IR_COL_NUM_R(SH_IR_IDX(cdir_switches.doall_sh_idx)) = 
                                                               stmt_start_col;
            

            if (on_off_flags.exec_doloops_once) {

               save_curr_stmt_sh_idx = curr_stmt_sh_idx;
               curr_stmt_sh_idx      = start_expr_sh_idx;                  
               insert_sh_chain_before(cdir_switches.doall_sh_idx);
               curr_stmt_sh_idx      = save_curr_stmt_sh_idx;
            }
            else {

               save_curr_stmt_sh_idx = curr_stmt_sh_idx;
               curr_stmt_sh_idx      = trip_zero_sh_idx;

               insert_sh_chain_before(cdir_switches.doall_sh_idx);

               if (preamble_start_sh_idx != NULL_IDX) {
                  /* insert the preamble stmts before here */
                  insert_sh_chain(preamble_start_sh_idx,
                                  preamble_end_sh_idx,
                                  Before);
               }

               curr_stmt_sh_idx      = save_curr_stmt_sh_idx;
            }

            if (ATD_TASK_SHARED(do_var_idx)) {
               PRINTMSG(do_var_line, 961, Error, do_var_col);
            }

            cdir_switches.doall_sh_idx = NULL_IDX;
         }
         else if (cdir_switches.paralleldo_omp_sh_idx) {

            IR_FLD_R(SH_IR_IDX(cdir_switches.paralleldo_omp_sh_idx)) = 
                                                      AT_Tbl_Idx;
            IR_IDX_R(SH_IR_IDX(cdir_switches.paralleldo_omp_sh_idx)) = 
                                                      cg_do_var_idx;
            IR_LINE_NUM_R(SH_IR_IDX(cdir_switches.paralleldo_omp_sh_idx)) =
                                                             stmt_start_line;
            IR_COL_NUM_R(SH_IR_IDX(cdir_switches.paralleldo_omp_sh_idx)) =
                                                               stmt_start_col;
           

            if (on_off_flags.exec_doloops_once) {

               save_curr_stmt_sh_idx = curr_stmt_sh_idx;
               curr_stmt_sh_idx      = start_expr_sh_idx;
               insert_sh_chain_before(cdir_switches.paralleldo_omp_sh_idx);
               curr_stmt_sh_idx      = save_curr_stmt_sh_idx;
            }
            else {

               save_curr_stmt_sh_idx = curr_stmt_sh_idx;
               curr_stmt_sh_idx      = trip_zero_sh_idx;

               insert_sh_chain_before(cdir_switches.paralleldo_omp_sh_idx);

               if (preamble_start_sh_idx != NULL_IDX) {
                  /* insert the preamble stmts before here */
                  insert_sh_chain(preamble_start_sh_idx,
                                  preamble_end_sh_idx,
                                  Before);
               }

               curr_stmt_sh_idx      = save_curr_stmt_sh_idx;
            }

            if (ATD_TASK_SHARED(do_var_idx)) {
               PRINTMSG(do_var_line, 961, Error, do_var_col);
            }

            cdir_switches.paralleldo_omp_sh_idx = NULL_IDX;
         }
         else if (cdir_switches.dopar_sh_idx) {

            IR_FLD_R(SH_IR_IDX(cdir_switches.dopar_sh_idx)) = AT_Tbl_Idx;
            IR_IDX_R(SH_IR_IDX(cdir_switches.dopar_sh_idx)) = cg_do_var_idx;
            IR_LINE_NUM_R(SH_IR_IDX(cdir_switches.dopar_sh_idx)) = 
                                                             stmt_start_line;
            IR_COL_NUM_R(SH_IR_IDX(cdir_switches.dopar_sh_idx)) = 
                                                             stmt_start_col;
            insert_sh_chain_before(cdir_switches.dopar_sh_idx);
            cdir_switches.dopar_sh_idx = NULL_IDX;
         }
         else if (cdir_switches.do_omp_sh_idx) {

            IR_FLD_R(SH_IR_IDX(cdir_switches.do_omp_sh_idx)) = AT_Tbl_Idx;
            IR_IDX_R(SH_IR_IDX(cdir_switches.do_omp_sh_idx)) = cg_do_var_idx;
            IR_LINE_NUM_R(SH_IR_IDX(cdir_switches.do_omp_sh_idx)) =
                                                             stmt_start_line;
            IR_COL_NUM_R(SH_IR_IDX(cdir_switches.do_omp_sh_idx)) =
                                                             stmt_start_col;
            insert_sh_chain_before(cdir_switches.do_omp_sh_idx);
            cdir_switches.do_omp_sh_idx = NULL_IDX;
         }



         /* Generate a CONTINUE stmt to define the top-of-loop label.  The    */
         /* "referenced" flag for the label will be set when the end of the   */
         /* loop IR is generated to make sure the label is really referenced. */

         gen_sh(After, Continue_Stmt, stmt_start_line, stmt_start_col,
                FALSE, FALSE, TRUE);

         NTR_IR_TBL(ir_idx);
         SH_IR_IDX(curr_stmt_sh_idx) = ir_idx;
         IR_OPR(ir_idx)              = Label_Opr;
         IR_TYPE_IDX(ir_idx)         = TYPELESS_DEFAULT_TYPE;
         IR_LINE_NUM(ir_idx)         = stmt_start_line;
         IR_COL_NUM(ir_idx)          = stmt_start_col;
         COPY_OPND(IR_OPND_L(ir_idx), IL_OPND(IL_IDX(loop_labels_il_idx)));
         AT_DEFINED(IR_IDX_L(ir_idx))       = TRUE;
         AT_DEF_LINE(IR_IDX_L(ir_idx))      = SH_GLB_LINE(do_sh_idx);
         ATL_DEF_STMT_IDX(IR_IDX_L(ir_idx)) = curr_stmt_sh_idx;


         /* Set the loop info flags on the top-of-loop label.                 */

         label_attr = IL_IDX(IL_IDX(loop_labels_il_idx));

         set_directives_on_label(label_attr);

         /* Generate the assignment:                                          */
         /*   DO-variable = start_temp + induc_temp * inc_temp                */
         /* Like the trip count calculation, the DO-variable value            */
         /* calculation uses already-established temps.  The expression is    */
         /* sent through expression semantics to get the data types, etc.     */
         /* propagated.							      */

         NTR_IR_TBL(expr_ir_idx);
         IR_OPR(expr_ir_idx)        = Mult_Opr;
         IR_LINE_NUM(expr_ir_idx)   = stmt_start_line;
         IR_COL_NUM(expr_ir_idx)    = stmt_start_col;
         IR_LINE_NUM_L(expr_ir_idx) = stmt_start_line;
         IR_COL_NUM_L(expr_ir_idx)  = stmt_start_col;
         IR_FLD_L(expr_ir_idx)      = AT_Tbl_Idx;
         IR_IDX_L(expr_ir_idx)      = IL_IDX(il_idx_2);
         COPY_OPND(IR_OPND_R(expr_ir_idx), IL_OPND(inc_il_idx));

         NTR_IR_TBL(ir_idx);
         IR_OPR(ir_idx)        = Plus_Opr;
         IR_LINE_NUM(ir_idx)   = stmt_start_line;
         IR_COL_NUM(ir_idx)    = stmt_start_col;
         COPY_OPND(IR_OPND_L(ir_idx), IL_OPND(start_il_idx));
         IR_LINE_NUM_R(ir_idx) = stmt_start_line;
         IR_COL_NUM_R(ir_idx)  = stmt_start_col;
         IR_FLD_R(ir_idx)      = IR_Tbl_Idx;
         IR_IDX_R(ir_idx)      = expr_ir_idx;

         expr_ir_idx = ir_idx;

         NTR_IR_TBL(ir_idx);
         IR_OPR(ir_idx)        = Asg_Opr;
         IR_LINE_NUM(ir_idx)   = stmt_start_line;
         IR_COL_NUM(ir_idx)    = stmt_start_col;
         COPY_OPND(IR_OPND_L(ir_idx), do_var_opnd);
         IR_LINE_NUM_R(ir_idx) = stmt_start_line;
         IR_COL_NUM_R(ir_idx)  = stmt_start_col;
         IR_FLD_R(ir_idx)      = IR_Tbl_Idx;
         IR_IDX_R(ir_idx)      = expr_ir_idx;

         gen_sh(After, Assignment_Stmt, stmt_start_line, stmt_start_col,
                FALSE, TRUE, TRUE);

         SH_IR_IDX(curr_stmt_sh_idx)      = ir_idx;


         /* Now (finally) send the DO-variable value calculation through      */
         /* expr_semantics.						      */

         COPY_OPND(temp_opnd, IR_OPND_R(ir_idx));
         exp_desc.rank    = 0;
         xref_state       = CIF_No_Usage_Rec;

         if (expr_semantics(&temp_opnd, &exp_desc)) {
            IR_TYPE_IDX(ir_idx) = (OPND_FLD(do_var_opnd) == AT_Tbl_Idx) ?
                                     ATD_TYPE_IDX(OPND_IDX(do_var_opnd)) :
			             IR_TYPE_IDX(OPND_IDX(do_var_opnd));
            COPY_OPND(IR_OPND_R(ir_idx), temp_opnd);
         }
         else {
            PRINTMSG(SH_GLB_LINE(curr_stmt_sh_idx), 224, Internal, 0);
         }

         break;

# endif                         /* End long section that does not apply to    */
				/* high-level form of the iterative DO loop.  */

CLEAR_CDIR_SWITCHES:

         /* If this was a DOALL loop make sure that the parallel region    */
         /* is terminated and cdir_switches.doall_sh_idx is cleared.       */
         /* Clear all the other cdir_switches that would have been         */
         /* cleared by this loop.                                          */

         clear_cdir_switches();

         goto EXIT;


      /* -------------------------------------------------------------------- */
      /*                                                                      */
      /*                       DO WHILE statement			      */
      /*                                                                      */
      /* -------------------------------------------------------------------- */

      case Do_While_Stmt:

         if (cdir_switches.do_omp_sh_idx) {

            PRINTMSG(IR_LINE_NUM(SH_IR_IDX(cdir_switches.do_omp_sh_idx)),
                     1544, Error,
                     IR_COL_NUM(SH_IR_IDX(cdir_switches.do_omp_sh_idx)),
                     "!$OMP DO");

            cdir_switches.do_omp_sh_idx = NULL_IDX;
         }
         else if (cdir_switches.paralleldo_omp_sh_idx) {

           PRINTMSG(IR_LINE_NUM(SH_IR_IDX(cdir_switches.paralleldo_omp_sh_idx)),
                     1544, Error,
                     IR_COL_NUM(SH_IR_IDX(cdir_switches.paralleldo_omp_sh_idx)),
                     "!$OMP PARALLEL DO");

            cdir_switches.paralleldo_omp_sh_idx = NULL_IDX;
         }

         /* Check the scalar-logical-expr.				      */

         semantics_ok = TRUE;

# ifdef _HIGH_LEVEL_DO_LOOP_FORM

         label_idx = gen_internal_lbl(stmt_start_line);
         NTR_IR_TBL(ir_idx);
         IR_OPR(ir_idx)              = Label_Opr;
         IR_TYPE_IDX(ir_idx)         = TYPELESS_DEFAULT_TYPE;
         IR_LINE_NUM(ir_idx)         = stmt_start_line;
         IR_COL_NUM(ir_idx)          = stmt_start_col;
         IR_FLD_L(ir_idx)            = AT_Tbl_Idx;
         IR_IDX_L(ir_idx)            = label_idx;
         IR_COL_NUM_L(ir_idx)        = stmt_start_col;
         IR_LINE_NUM_L(ir_idx)       = stmt_start_line;

         AT_DEFINED(label_idx)       = TRUE;
         ATL_TOP_OF_LOOP(label_idx)  = TRUE;

         gen_sh(Before, Continue_Stmt, stmt_start_line, 
                stmt_start_col, FALSE, FALSE, TRUE);
         SH_P2_SKIP_ME(SH_PREV_IDX(curr_stmt_sh_idx)) = TRUE;
         SH_IR_IDX(SH_PREV_IDX(curr_stmt_sh_idx)) = ir_idx;

         ATL_DEF_STMT_IDX(label_idx) = SH_PREV_IDX(curr_stmt_sh_idx);

         set_directives_on_label(label_idx);

         il_idx = IL_IDX(loop_control_il_idx);
         COPY_OPND(temp_opnd, IL_OPND(il_idx));

         if (OPND_FLD(temp_opnd) == IR_Tbl_Idx) {
            copy_subtree(&temp_opnd, &temp_opnd);
         }

         /* Insert an assignment stmt ahead of the DO loop to capture the     */
         /* loop control expression in a temp.  We need to do this (and to    */
         /* repeat the assignment at the end of the loop) for the case where  */
         /* the expression contains a function reference.                     */

         curr_stmt_sh_idx = SH_PREV_IDX(do_sh_idx);

         gen_sh(After, 
                Assignment_Stmt,
                SH_GLB_LINE(do_sh_idx),
                SH_COL_NUM(do_sh_idx),
                FALSE,				/* Error flag.		      */
                FALSE,				/* Labeled.		      */
                TRUE);				/* Compiler-generated.        */

         GEN_COMPILER_TMP_ASG(ir_idx,
                              tmp_idx,
                              FALSE,		/* Value of AT_SEMANTICS_DONE */
        					/* for the temp.	      */
                              SH_GLB_LINE(do_sh_idx),
                              SH_COL_NUM(do_sh_idx),
                              LOGICAL_DEFAULT_TYPE,
                              Priv); 		/* ADD_TMP_TO_PRIVATE_LIST    */
						/* for the temp.	      */

         tmp_asg_ir_idx              = ir_idx;
         SH_IR_IDX(curr_stmt_sh_idx) = tmp_asg_ir_idx;

# else

         /* For the low-level form of the DO WHILE loop:		      */
         /*   (1) the IF SH is generated BEFORE the expression is evaluated   */
         /*       so that any IR generated to represent the expression is     */
         /*       inserted between the DO SH and the IF SH; and		      */
         /*   (2) the scalar-logical-expr is copied BEFORE calling	      */
         /*       expr_semantics because the tree could be expanded into a    */
         /*       bunch of statements.  The tree must be sent through         */
         /*       expr_semantics again when the end-of-loop IR is generated.  */

         gen_sh(After, If_Stmt, stmt_start_line, stmt_start_col,
                FALSE, FALSE, TRUE);

         il_idx = IL_IDX(loop_control_il_idx);
         COPY_OPND(temp_opnd, IL_OPND(il_idx));
         copy_subtree(&temp_opnd, &temp_opnd);

         defer_stmt_expansion = TRUE;
# endif

         exp_desc.rank  = 0;
         xref_state     = CIF_Symbol_Reference;

         if (expr_semantics(&temp_opnd, &exp_desc)) {

            if (exp_desc.rank != 0) {
               PRINTMSG(IL_LINE_NUM(il_idx), 222, Error, IL_COL_NUM(il_idx));
               semantics_ok = FALSE;
            }

            if (exp_desc.type == Logical) {

               if (semantics_ok) {

# ifdef _HIGH_LEVEL_DO_LOOP_FORM

                  COPY_OPND(IR_OPND_R(tmp_asg_ir_idx), temp_opnd);
                  curr_stmt_sh_idx = do_sh_idx;

                  /* Save the original expression index in an IL at the end   */
                  /* of the IL list attached to the Loop_Info IR.  Then plug  */
                  /* WHILE expression temp result into the IL where the       */
                  /* expression index originally appeared.		      */

                  NTR_IR_LIST_TBL(il_idx_2);
                  IL_NEXT_LIST_IDX(loop_labels_il_idx) = il_idx_2;
                  IL_PREV_LIST_IDX(il_idx_2)           = loop_labels_il_idx;
                  ++IR_LIST_CNT_R(loop_info_idx);
                  COPY_OPND(IL_OPND(il_idx_2), IL_OPND(il_idx));
                  IL_FLD(il_idx) = AT_Tbl_Idx;
                  IL_IDX(il_idx) = tmp_idx;
       
# else

                  defer_stmt_expansion = FALSE;

                  if (tree_produces_dealloc(&temp_opnd)) {
                     /* make logical tmp asg */
                     save_curr_stmt_sh_idx = curr_stmt_sh_idx;
                     find_opnd_line_and_column(&temp_opnd,
                                       &opnd_line, &opnd_column);

                     GEN_COMPILER_TMP_ASG(asg_idx,
                                          tmp_idx,
                                          TRUE,       /* Semantics done */
                                          opnd_line,
                                          opnd_column,
                                          exp_desc.type_idx,
                                          Priv);

                     gen_sh(Before, Assignment_Stmt, opnd_line,
                            opnd_column, FALSE, FALSE, TRUE);

                     curr_stmt_sh_idx = SH_PREV_IDX(curr_stmt_sh_idx);

                     SH_IR_IDX(curr_stmt_sh_idx)     = asg_idx;
                     SH_P2_SKIP_ME(curr_stmt_sh_idx) = TRUE;

                     process_deferred_functions(&temp_opnd);
                     COPY_OPND(IR_OPND_R(asg_idx), temp_opnd);

                     OPND_FLD(temp_opnd)        = AT_Tbl_Idx;
                     OPND_IDX(temp_opnd)        = tmp_idx;
                     OPND_LINE_NUM(temp_opnd)   = opnd_line;
                     OPND_COL_NUM(temp_opnd)    = opnd_column;
                     curr_stmt_sh_idx           = save_curr_stmt_sh_idx;
                  }
                  else {
                     process_deferred_functions(&temp_opnd);
                  }

                  /* Generate   IF (.NOT. scalar-logical-expr) GO TO skip-lbl */

                  NTR_IR_TBL(expr_ir_idx);
                  IR_OPR(expr_ir_idx)      = Not_Opr;
                  IR_TYPE_IDX(expr_ir_idx) = LOGICAL_DEFAULT_TYPE;
                  IR_LINE_NUM(expr_ir_idx) = stmt_start_line;
                  IR_COL_NUM(expr_ir_idx)  = stmt_start_col;
                  COPY_OPND(IR_OPND_L(expr_ir_idx), temp_opnd);

                  NTR_IR_TBL(ir_idx);
                  IR_OPR(ir_idx)        = Br_True_Opr;
                  IR_TYPE_IDX(ir_idx)   = LOGICAL_DEFAULT_TYPE;
                  IR_LINE_NUM(ir_idx)   = stmt_start_line;
                  IR_COL_NUM(ir_idx)    = stmt_start_col;
                  IR_LINE_NUM_L(ir_idx) = stmt_start_line;
                  IR_COL_NUM_L(ir_idx)  = stmt_start_col;
                  IR_FLD_L(ir_idx)      = IR_Tbl_Idx;
                  IR_IDX_L(ir_idx)      = expr_ir_idx;
                  IR_LINE_NUM_R(ir_idx) = stmt_start_line;
                  IR_COL_NUM_R(ir_idx)  = stmt_start_col;
                  IR_FLD_R(ir_idx)      = AT_Tbl_Idx;
                  lbl_il_idx            = 
                     IL_NEXT_LIST_IDX(IL_IDX(loop_labels_il_idx));
                  IR_IDX_R(ir_idx)      = IL_IDX(lbl_il_idx);

                  IR_TYPE_IDX(ir_idx) = exp_desc.type_idx;

                  SH_IR_IDX(curr_stmt_sh_idx) = ir_idx;

# endif

               }
            }
            else {
               PRINTMSG(IL_LINE_NUM(il_idx), 234, Error, IL_COL_NUM(il_idx));
               semantics_ok = FALSE;
            }
         }
         else {
            semantics_ok = FALSE;
         }

# ifdef _HIGH_LEVEL_DO_LOOP_FORM

         if (! semantics_ok) {
            SH_ERR_FLG(do_sh_idx) = TRUE;
            curr_stmt_sh_idx      = do_sh_idx;
         }

# else

         defer_stmt_expansion = FALSE;

         label_attr = IL_IDX(IL_IDX(loop_labels_il_idx));

         if (semantics_ok) {

            /* Generate a CONTINUE statement to define the top-of-loop label. */

            gen_sh(After, Continue_Stmt, stmt_start_line, stmt_start_col,
                   FALSE, FALSE, TRUE);

            NTR_IR_TBL(ir_idx);
            SH_IR_IDX(curr_stmt_sh_idx) = ir_idx;
            IR_OPR(ir_idx)              = Label_Opr;
            IR_TYPE_IDX(ir_idx)         = TYPELESS_DEFAULT_TYPE;
            IR_LINE_NUM(ir_idx)         = stmt_start_line;
            IR_COL_NUM(ir_idx)          = stmt_start_col;
            IR_LINE_NUM_L(ir_idx)       = stmt_start_line;
            IR_COL_NUM_L(ir_idx)        = stmt_start_col;
            IR_FLD_L(ir_idx)            = AT_Tbl_Idx;
            IR_IDX_L(ir_idx)            = label_attr;

            AT_DEF_LINE(label_attr) =
               SH_GLB_LINE(SH_NEXT_IDX(curr_stmt_sh_idx));
            ATL_DEF_STMT_IDX(label_attr) = curr_stmt_sh_idx;
         }
         else {
            SH_PARENT_BLK_IDX(IR_IDX_L(loop_info_idx)) = NULL_IDX;
         }


         /* Set the loop info flags on the label.                             */

         set_directives_on_label(label_attr);

# endif

         break;


      /* -------------------------------------------------------------------- */
      /*                                                                      */
      /*                      "Infinite" DO statement			      */
      /*                                                                      */
      /* -------------------------------------------------------------------- */

      case Do_Infinite_Stmt:

         if (cdir_switches.do_omp_sh_idx) {

            PRINTMSG(IR_LINE_NUM(SH_IR_IDX(cdir_switches.do_omp_sh_idx)),
                     1544, Error,
                     IR_COL_NUM(SH_IR_IDX(cdir_switches.do_omp_sh_idx)),
                     "!$OMP DO");

            cdir_switches.do_omp_sh_idx = NULL_IDX;
         }
         else if (cdir_switches.paralleldo_omp_sh_idx) {

           PRINTMSG(IR_LINE_NUM(SH_IR_IDX(cdir_switches.paralleldo_omp_sh_idx)),
                     1544, Error,
                     IR_COL_NUM(SH_IR_IDX(cdir_switches.paralleldo_omp_sh_idx)),
                     "!$OMP PARALLEL DO");

            cdir_switches.paralleldo_omp_sh_idx = NULL_IDX;
         }

         /* Generate a CONTINUE statement to define the top-of-loop label.    */

         gen_sh(After,
                Continue_Stmt,
                SH_GLB_LINE(SH_NEXT_IDX(curr_stmt_sh_idx)),
                SH_COL_NUM(SH_NEXT_IDX(curr_stmt_sh_idx)),
                FALSE,
                TRUE,
                TRUE);

         NTR_IR_TBL(ir_idx);
         SH_IR_IDX(curr_stmt_sh_idx)  = ir_idx;
         IR_OPR(ir_idx)               = Label_Opr;
         IR_TYPE_IDX(ir_idx)          = TYPELESS_DEFAULT_TYPE;
         IR_LINE_NUM(ir_idx)          = SH_GLB_LINE(curr_stmt_sh_idx);
         IR_COL_NUM(ir_idx)           = SH_COL_NUM(curr_stmt_sh_idx);
         IR_LINE_NUM_L(ir_idx)        = IR_LINE_NUM(ir_idx);
         IR_COL_NUM_L(ir_idx)         = IR_COL_NUM(ir_idx);
         IR_FLD_L(ir_idx)             = AT_Tbl_Idx;
         label_attr	              = IL_IDX(IL_IDX(loop_labels_il_idx));
         IR_IDX_L(ir_idx)             = label_attr;
         AT_DEFINED(label_attr)       = TRUE;
         AT_DEF_LINE(label_attr)      =
            SH_GLB_LINE(SH_NEXT_IDX(curr_stmt_sh_idx));
         ATL_DEF_STMT_IDX(label_attr) = curr_stmt_sh_idx;

         break;


      /* -------------------------------------------------------------------- */
      /*                                                                      */
      /*                      P R O B L E M S ! ! !			      */
      /*                                                                      */
      /* -------------------------------------------------------------------- */

      default:
         PRINTMSG(stmt_start_line, 179, Internal, stmt_start_col,
                  "do_stmt_semantics");
   }

EXIT:

   TRACE (Func_Exit, "do_stmt_semantics", NULL);

   return;

}  /* do_stmt_semantics */


/******************************************************************************\
|*									      *|
|* Description:								      *|
|*	This function handles the following syntax:			      *|
|*	   else-stmt       => ELSE [if-construct-name]			      *|
|*	   else-if-stmt    => ELSE IF ( sclr-lgcl-expr ) THEN [if-cnstrct-nme]*|
|*	   elsewhere-stmt  => ELSE WHERE				      *|
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

void else_stmt_semantics (void)

{
   int			and_idx;
   int			col;
   opnd_type		cond_expr;
   int			cond_expr_ir_idx;
   expr_arg_type        exp_desc;
   int			ir_idx;
   int			line;
   int			list_idx;
   opnd_type		mask_expr_opnd;
   int			mask_expr_tmp;
   boolean		ok = TRUE;
   opnd_type		opnd;
   opnd_type		pending_mask_opnd;
   int			sh_idx;
  
# if defined(_HIGH_LEVEL_IF_FORM)
   int			else_sh_idx;
   int			endif_sh_idx;
   int			save_curr_stmt_sh_idx;
# else
   int			cont_lbl_idx;
   int			if_ir_idx;
   int			prev_part_idx;
# endif


   TRACE (Func_Entry, "else_stmt_semantics", NULL);

   switch (stmt_type) {
      case Else_Stmt:

# if defined(_HIGH_LEVEL_IF_FORM)
         /* find Endif_Opr stmt. */

# if defined(_DEBUG)
         if (IR_OPR(SH_IR_IDX(SH_PARENT_BLK_IDX(curr_stmt_sh_idx))) != If_Opr) {
            PRINTMSG(stmt_start_line, 626, Internal, stmt_start_col,
                     "If_Opr", "else_stmt_semantics");
         }
# endif

         endif_sh_idx = IR_IDX_R(SH_IR_IDX(
                                SH_PARENT_BLK_IDX(curr_stmt_sh_idx)));

         SH_PARENT_BLK_IDX(endif_sh_idx) = curr_stmt_sh_idx;
         SH_PARENT_BLK_IDX(curr_stmt_sh_idx) = 
                                       IR_IDX_L(SH_IR_IDX(endif_sh_idx));

# else
         /* Generate a GO TO stmt ahead of the ELSE SH to branch to the end   */
         /* of the IF construct.  Get the END IF label from the second IL     */
         /* attached to the right operand of the If_Opr IR attached to the    */
         /* If_Cstrct SH.  (Walk back through the SH_PARENT_BLK_IDX chain to  */
         /* find the If_Cstrct SH.)					      */

         gen_sh(Before, Goto_Stmt,
                SH_GLB_LINE(SH_PREV_IDX(curr_stmt_sh_idx)),
                SH_COL_NUM(SH_PREV_IDX(curr_stmt_sh_idx)),
                FALSE, FALSE, TRUE);		 /* compiler-generated = TRUE */

         sh_idx                 = SH_PREV_IDX(curr_stmt_sh_idx);
         NTR_IR_TBL(ir_idx);
         SH_IR_IDX(sh_idx)      = ir_idx;
         IR_OPR(ir_idx)         = Br_Uncond_Opr;
         IR_TYPE_IDX(ir_idx)    = TYPELESS_DEFAULT_TYPE;
         IR_LINE_NUM(ir_idx)    = SH_GLB_LINE(SH_PREV_IDX(sh_idx));
         IR_COL_NUM(ir_idx)     = SH_COL_NUM(SH_PREV_IDX(sh_idx));

         IR_LINE_NUM_R(ir_idx)  = SH_GLB_LINE(SH_PREV_IDX(sh_idx));
         IR_COL_NUM_R(ir_idx)   = SH_COL_NUM(SH_PREV_IDX(sh_idx));
         IR_FLD_R(ir_idx)       = AT_Tbl_Idx;

         sh_idx = IR_IDX_L(SH_IR_IDX(curr_stmt_sh_idx));

         while (SH_STMT_TYPE(sh_idx) != If_Cstrct_Stmt) {
            sh_idx = IL_IDX(IL_NEXT_LIST_IDX(IR_IDX_R(SH_IR_IDX(sh_idx))));
         }

         if_ir_idx = SH_IR_IDX(sh_idx);

         IR_IDX_R(ir_idx) = IL_IDX(IL_NEXT_LIST_IDX(IR_IDX_R(if_ir_idx)));


         /* Generate a CONTINUE stmt to define the start of the ELSE.  If     */
         /* there are no ELSE IF stmts preceding this ELSE then get the       */
         /* branch-around label from the first IL attached to the If_Opr IR   */
         /* attached to the If_Cstrct SH.  If there was at least one ELSE IF  */
         /* stmt, then get the label from the right operand of the preceding  */
         /* ELSE IF (via SH_PARENT_BLK_IDX).				      */

         gen_sh(Before, Continue_Stmt, stmt_start_line, stmt_start_col,
                FALSE,
                TRUE, 					/* Labeled.           */
                TRUE);					/* Compiler-generated */

         sh_idx                = SH_PREV_IDX(curr_stmt_sh_idx);
         NTR_IR_TBL(ir_idx);
         SH_IR_IDX(sh_idx)     = ir_idx;
         IR_OPR(ir_idx)        = Label_Opr;
         IR_TYPE_IDX(ir_idx)   = TYPELESS_DEFAULT_TYPE;
         IR_LINE_NUM(ir_idx)   = stmt_start_line;
         IR_COL_NUM(ir_idx)    = stmt_start_col;
         IR_LINE_NUM_L(ir_idx) = stmt_start_line;
         IR_COL_NUM_L(ir_idx)  = stmt_start_col;
         IR_FLD_L(ir_idx)      = AT_Tbl_Idx;

         prev_part_idx = IR_IDX_L(SH_IR_IDX(curr_stmt_sh_idx));

         if (SH_STMT_TYPE(prev_part_idx) == If_Cstrct_Stmt) {
            cont_lbl_idx = IL_IDX(IR_IDX_R(if_ir_idx));
         }
         else {
            cont_lbl_idx = IL_IDX(IR_IDX_R(SH_IR_IDX(prev_part_idx)));
         }

         IR_IDX_L(ir_idx)               = cont_lbl_idx;
         AT_DEFINED(cont_lbl_idx)       = TRUE;
         AT_DEF_LINE(cont_lbl_idx)      = stmt_start_line;
         AT_DEF_COLUMN(cont_lbl_idx)    = stmt_start_col;
         AT_REFERENCED(cont_lbl_idx)    = Referenced;
         ATL_DEF_STMT_IDX(cont_lbl_idx) = sh_idx;
#endif

         break;


      case Else_If_Stmt:

         cond_expr_ir_idx = SH_IR_IDX(curr_stmt_sh_idx);

# if defined(_HIGH_LEVEL_IF_FORM)
         /* generate an Else_Opr stmt and change curr stmt to If_Opr */

         NTR_IR_TBL(ir_idx);
         IR_OPR(ir_idx) = Else_Opr;
         IR_TYPE_IDX(ir_idx) = TYPELESS_DEFAULT_TYPE;
         IR_LINE_NUM(ir_idx) = IR_LINE_NUM(cond_expr_ir_idx);
         IR_COL_NUM(ir_idx)  = IR_COL_NUM(cond_expr_ir_idx);
         COPY_OPND(IR_OPND_L(ir_idx), 
                   IL_OPND(IL_NEXT_LIST_IDX(IR_IDX_R(cond_expr_ir_idx))));

         gen_sh(Before, Else_Stmt, stmt_start_line, stmt_start_col,
                FALSE,
                FALSE,                                /* Not Labeled.       */
                TRUE);                                /* Compiler-generated */
         else_sh_idx = SH_PREV_IDX(curr_stmt_sh_idx);
         SH_IR_IDX(else_sh_idx) = ir_idx;

         /* change Else_If_Opr to If_Opr */

         IR_OPR(cond_expr_ir_idx) = If_Opr;
         SH_STMT_TYPE(curr_stmt_sh_idx) = If_Stmt;

         /* find Endif_Opr stmt. */

# if defined(_DEBUG)
         if (IR_OPR(SH_IR_IDX(SH_PARENT_BLK_IDX(curr_stmt_sh_idx))) != If_Opr) {
            PRINTMSG(stmt_start_line, 626, Internal, stmt_start_col,
                     "If_Opr", "else_stmt_semantics");
         }
# endif

         endif_sh_idx = IR_IDX_R(SH_IR_IDX(
                                SH_PARENT_BLK_IDX(curr_stmt_sh_idx)));

         SH_PARENT_BLK_IDX(endif_sh_idx) = else_sh_idx;

         save_curr_stmt_sh_idx = curr_stmt_sh_idx;
         curr_stmt_sh_idx = endif_sh_idx;

         SH_PARENT_BLK_IDX(else_sh_idx) = IR_IDX_L(SH_IR_IDX(endif_sh_idx));

         /* generate a new Endif_Opr stmt before endif_sh_idx */

         NTR_IR_TBL(ir_idx);
         IR_OPR(ir_idx) = Endif_Opr;
         IR_TYPE_IDX(ir_idx) = TYPELESS_DEFAULT_TYPE;
         IR_LINE_NUM(ir_idx) = IR_LINE_NUM(cond_expr_ir_idx);
         IR_COL_NUM(ir_idx)  = IR_COL_NUM(cond_expr_ir_idx);

         IR_FLD_L(ir_idx) = SH_Tbl_Idx;
         IR_IDX_L(ir_idx) = save_curr_stmt_sh_idx;

         gen_sh(Before, End_If_Stmt, stmt_start_line, stmt_start_col,
                FALSE,
                FALSE,                                /* Not Labeled.       */
                TRUE);                                /* Compiler-generated */
         SH_IR_IDX(SH_PREV_IDX(curr_stmt_sh_idx)) = ir_idx;
         SH_P2_SKIP_ME(SH_PREV_IDX(curr_stmt_sh_idx)) = TRUE;

         endif_sh_idx = SH_PREV_IDX(curr_stmt_sh_idx);

         /* set SH_PAREN_BLK_IDX to If_Opr stmt for now. */
         /* It may be overwritten if an Else or Else if clause follows */
         SH_PARENT_BLK_IDX(endif_sh_idx) = save_curr_stmt_sh_idx;

         curr_stmt_sh_idx = save_curr_stmt_sh_idx;

         IR_IDX_R(SH_IR_IDX(SH_PARENT_BLK_IDX(curr_stmt_sh_idx))) = 
                                                               endif_sh_idx;

         SH_PARENT_BLK_IDX(curr_stmt_sh_idx) = 0;
# endif
         /* The conditional expression must be scalar and type logical.       */
   

         in_branch_true       = TRUE;
         defer_stmt_expansion = TRUE;
         io_item_must_flatten = FALSE;
         number_of_functions  = 0;

         COPY_OPND(cond_expr, IR_OPND_L(cond_expr_ir_idx));
         exp_desc.rank = 0;
         xref_state    = CIF_Symbol_Reference;

         has_present_opr = FALSE;
         ok = expr_semantics(&cond_expr, &exp_desc);
         has_present_opr = FALSE;

         COPY_OPND(IR_OPND_L(cond_expr_ir_idx), cond_expr);

         defer_stmt_expansion = FALSE;
         in_branch_true       = FALSE;

         if (ok && exp_desc.rank != 0) {
            PRINTMSG(IR_LINE_NUM(cond_expr_ir_idx), 410, Error,
                     IR_COL_NUM(cond_expr_ir_idx));
         }

         if (ok && exp_desc.type != Logical) {
            PRINTMSG(IR_LINE_NUM(cond_expr_ir_idx), 416, Error,
                     IR_COL_NUM(cond_expr_ir_idx));
         } 

#ifndef _HIGH_LEVEL_IF_FORM

         /* Generate a GO TO stmt ahead of the ELSE IF SH to branch to  */
         /* the end of the IF construct.  Get the END IF label from the */
         /* second IL attached to the right operand of the If_Opr IR    */
         /* attached to the If_Cstrct SH.  (Walk back through the       */
         /* SH_PARENT_BLK_IDX chain to find the If_Cstrct SH.)          */

         gen_sh(Before, Goto_Stmt,
                SH_GLB_LINE(SH_PREV_IDX(curr_stmt_sh_idx)),
                SH_COL_NUM(SH_PREV_IDX(curr_stmt_sh_idx)),
                FALSE, FALSE, TRUE);       /* compiler-generated = TRUE */

         sh_idx                 = SH_PREV_IDX(curr_stmt_sh_idx);
         NTR_IR_TBL(ir_idx);
         SH_IR_IDX(sh_idx)      = ir_idx;
         IR_OPR(ir_idx)         = Br_Uncond_Opr;
         IR_TYPE_IDX(ir_idx)    = TYPELESS_DEFAULT_TYPE;
         IR_LINE_NUM(ir_idx)    = SH_GLB_LINE(SH_PREV_IDX(sh_idx));
         IR_COL_NUM(ir_idx)     = SH_COL_NUM(SH_PREV_IDX(sh_idx));

         IR_LINE_NUM_R(ir_idx)  = SH_GLB_LINE(SH_PREV_IDX(sh_idx));
         IR_COL_NUM_R(ir_idx)   = SH_COL_NUM(SH_PREV_IDX(sh_idx));
         IR_FLD_R(ir_idx)       = AT_Tbl_Idx;

         sh_idx =
            IL_IDX(IL_NEXT_LIST_IDX(IR_IDX_R(SH_IR_IDX(curr_stmt_sh_idx))));

         while (SH_STMT_TYPE(sh_idx) != If_Cstrct_Stmt) {
            sh_idx = IL_IDX(IL_NEXT_LIST_IDX(IR_IDX_R(SH_IR_IDX(sh_idx))));
         }

         if_ir_idx = SH_IR_IDX(sh_idx);

         IR_IDX_R(ir_idx) = IL_IDX(IL_NEXT_LIST_IDX(IR_IDX_R(if_ir_idx)));


         /* Generate a CONTINUE stmt to define the start of the ELSE    */
         /* IF.  If this is the first ELSE IF stmt, get the             */
         /* branch-around label from the first IL attached to the       */
         /* If_Opr IR attached to the If_Cstrct SH.  Otherwise, get it  */
         /* from the first IL attached to the right operand of the      */
         /* preceding ELSE IF.					      */

         gen_sh(Before, Continue_Stmt, stmt_start_line, stmt_start_col,
                FALSE, TRUE, TRUE);

         sh_idx                         = SH_PREV_IDX(curr_stmt_sh_idx);
         NTR_IR_TBL(ir_idx);
         SH_IR_IDX(sh_idx)              = ir_idx;
         IR_OPR(ir_idx)                 = Label_Opr;
         IR_TYPE_IDX(ir_idx)            = TYPELESS_DEFAULT_TYPE;
         IR_LINE_NUM(ir_idx)            = stmt_start_line;
         IR_COL_NUM(ir_idx)             = stmt_start_col;
         IR_LINE_NUM_L(ir_idx)          = stmt_start_line;
         IR_COL_NUM_L(ir_idx)           = stmt_start_col;
         IR_FLD_L(ir_idx)               = AT_Tbl_Idx;

         prev_part_idx =
            IL_IDX(IL_NEXT_LIST_IDX(IR_IDX_R(SH_IR_IDX(curr_stmt_sh_idx))));
         cont_lbl_idx = IL_IDX(IR_IDX_R(SH_IR_IDX(prev_part_idx)));

         IR_IDX_L(ir_idx)               = cont_lbl_idx;
         AT_DEFINED(cont_lbl_idx)       = TRUE;
         AT_DEF_LINE(cont_lbl_idx)      = stmt_start_line;
         AT_DEF_COLUMN(cont_lbl_idx)    = stmt_start_col;
         AT_REFERENCED(cont_lbl_idx)    = Referenced;
         ATL_DEF_STMT_IDX(cont_lbl_idx) = sh_idx;


         /* Generate the ".NOT. cond" IR under the Br_True IR.	      */

         IR_FLD_L(cond_expr_ir_idx) = IR_Tbl_Idx;
         NTR_IR_TBL(ir_idx);
         IR_IDX_L(cond_expr_ir_idx) = ir_idx;

         IR_OPR(ir_idx)        = Not_Opr;
         IR_TYPE_IDX(ir_idx)   = LOGICAL_DEFAULT_TYPE;
         IR_LINE_NUM(ir_idx)   = stmt_start_line;
         IR_COL_NUM(ir_idx)    = stmt_start_col;
         COPY_OPND(IR_OPND_L(ir_idx), cond_expr);

         
         /* Generate the branch-around label and save it in the first   */
         /* IL attached to the right operand of the Br_True IR.         */
         /* END IF processing will pull the label into the right operand*/
         /* of the Br_True IR.					      */

         IL_LINE_NUM(IR_IDX_R(cond_expr_ir_idx)) = stmt_start_line;
         IL_COL_NUM(IR_IDX_R(cond_expr_ir_idx))  = stmt_start_col;
         IL_FLD(IR_IDX_R(cond_expr_ir_idx))      = AT_Tbl_Idx;
         IL_IDX(IR_IDX_R(cond_expr_ir_idx)) = gen_internal_lbl(stmt_start_line);

#endif
 
               
         /* short_circuit_branch calls process_deferred_functions.      */


#ifdef _HIGH_LEVEL_IF_FORM

         if (ok) {
            short_circuit_high_level_if();
         }
#else

         if (ok) {
            short_circuit_branch();
         }

#endif


         in_branch_true       = FALSE;
         defer_stmt_expansion = FALSE;
         io_item_must_flatten = FALSE;
         arg_info_list_base   = NULL_IDX;
         arg_info_list_top    = NULL_IDX;
      
         break;

      case Else_Where_Stmt:

         ir_idx = SH_IR_IDX(curr_stmt_sh_idx);
         line = IR_LINE_NUM(ir_idx);
         col = IR_COL_NUM(ir_idx);

         sh_idx = SH_PARENT_BLK_IDX(curr_stmt_sh_idx);
# ifdef _DEBUG
         if (sh_idx == NULL_IDX) {
            PRINTMSG(stmt_start_line, 626, Internal, stmt_start_col,
                     "SH_PARENT_BLK_IDX(curr_stmt_sh_idx)",
                     "else_stmt_semantics");
         }
# endif

         if (IR_FLD_L(SH_IR_IDX(sh_idx)) == IL_Tbl_Idx &&
             IR_LIST_CNT_L(SH_IR_IDX(sh_idx)) == 2) {

            NTR_IR_LIST_TBL(list_idx);
            IR_FLD_L(ir_idx) = IL_Tbl_Idx;
            IR_LIST_CNT_L(ir_idx) = 1;
            IR_IDX_L(ir_idx) = list_idx;

            /* put the pending mask on as the control mask */
            COPY_OPND(IL_OPND(list_idx),
                      IL_OPND(IL_NEXT_LIST_IDX(IR_IDX_L(SH_IR_IDX(sh_idx)))));

            where_ir_idx = IL_IDX(list_idx);
         }
         break;

      case Else_Where_Mask_Stmt:
         ir_idx = SH_IR_IDX(curr_stmt_sh_idx);

         exp_desc.rank        = 0;
         xref_state           = CIF_Symbol_Reference;

         COPY_OPND(opnd, IR_OPND_L(ir_idx));

         ok = expr_semantics(&opnd, &exp_desc);

         find_opnd_line_and_column(&opnd, &line, &col);

         if (exp_desc.type != Logical) {
            PRINTMSG(line, 120, Error, col);
            ok = FALSE;
         }
         else if (exp_desc.rank == 0) {
            PRINTMSG(line, 181, Error, col);
            ok = FALSE;
         }

         if (where_ir_idx > 0) {
            /* check conformance */
      
            if (! check_where_conformance(&exp_desc)) {
               PRINTMSG(line, 1610, Error, col);
               ok = FALSE;
            }
         }

         sh_idx = SH_PARENT_BLK_IDX(curr_stmt_sh_idx);
# ifdef _DEBUG
         if (sh_idx == NULL_IDX) {
            PRINTMSG(stmt_start_line, 626, Internal, stmt_start_col,
                     "SH_PARENT_BLK_IDX(curr_stmt_sh_idx)",
                     "else_stmt_semantics");
         }
# endif

         if (IR_FLD_L(SH_IR_IDX(sh_idx)) == IL_Tbl_Idx &&
             IR_LIST_CNT_L(SH_IR_IDX(sh_idx)) == 2) {

            COPY_OPND(pending_mask_opnd,
                      IL_OPND(IL_NEXT_LIST_IDX(IR_IDX_L(SH_IR_IDX(sh_idx)))));
         }
         else {
            /* in error situation. */
            goto EXIT;
         }

         /* set up control mask */

         mask_expr_tmp = create_tmp_asg(&opnd, &exp_desc, &mask_expr_opnd,
                                        Intent_In, FALSE, TRUE);

         and_idx = gen_ir(OPND_FLD(pending_mask_opnd), 
                                       OPND_IDX(pending_mask_opnd),
                      And_Opr, exp_desc.type_idx, line, col,
                          OPND_FLD(mask_expr_opnd), OPND_IDX(mask_expr_opnd));
   
         gen_opnd(&opnd, and_idx, IR_Tbl_Idx, line, col);

         NTR_IR_LIST_TBL(list_idx);
         IR_FLD_L(ir_idx) = IL_Tbl_Idx;
         IR_IDX_L(ir_idx) = list_idx;
         IR_LIST_CNT_L(ir_idx) = 2;
   
         where_ir_idx  = OPND_IDX(opnd);
         COPY_OPND(IL_OPND(list_idx), opnd);

         /* set up new pending mask */

         and_idx = gen_ir(OPND_FLD(pending_mask_opnd), 
                                       OPND_IDX(pending_mask_opnd),
                      And_Opr, exp_desc.type_idx, line, col,
                          IR_Tbl_Idx, gen_ir(OPND_FLD(mask_expr_opnd), 
                                                       OPND_IDX(mask_expr_opnd),
                                         Not_Opr, exp_desc.type_idx, line, col,
                                             NO_Tbl_Idx, NULL_IDX));

         gen_opnd(&opnd, and_idx, IR_Tbl_Idx, line, col);

         NTR_IR_LIST_TBL(IL_NEXT_LIST_IDX(list_idx));
         IL_PREV_LIST_IDX(IL_NEXT_LIST_IDX(list_idx)) = list_idx;
         list_idx = IL_NEXT_LIST_IDX(list_idx);

         COPY_OPND(IL_OPND(list_idx), opnd);
         break;

   }

EXIT:

   TRACE (Func_Exit, "else_stmt_semantics", NULL);

   return;

}  /* else_stmt_semantics */

/******************************************************************************\
|*                                                                            *|
|* Description:                                                               *|
|*      This procedure completes the processing of the FORALL statement and   *|
|*      of the FORALL header portion of an FORALL construct.                  *|
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

void forall_semantics (void)

{
   int			asg_idx;
   int			body_end_sh_idx;
   int			body_start_sh_idx;
   opnd_type		br_around_opnd;
   int			col;
   expr_arg_type        exp_desc;
   int			index_idx;
   int                  ir_idx;
   int			line;
   int			list_idx;
   int			list_idx2;
   opnd_type		l_opnd;
   boolean		ok = TRUE;
   opnd_type            opnd;
   int			or_idx;
   int			save_next_sh_idx;
   int			tmp_idx;
   int			type_idx;


   TRACE (Func_Entry, "forall_semantics", NULL);

   ir_idx = SH_IR_IDX(curr_stmt_sh_idx);
   save_next_sh_idx = SH_NEXT_IDX(curr_stmt_sh_idx);

   br_around_opnd = null_opnd;

   if (active_forall_sh_idx) {
      gen_forall_loops(curr_stmt_sh_idx,
                       IR_IDX_L(ir_idx));
      gen_forall_if_mask(curr_stmt_sh_idx,
                         IR_IDX_L(ir_idx));
   }

   active_forall_sh_idx = curr_stmt_sh_idx;

   /* first, go through list of indexes to catch nested reuse */

   list_idx = IR_IDX_R(ir_idx);

   while (list_idx &&
          IL_FLD(list_idx) == IL_Tbl_Idx) {

# ifdef _DEBUG
      if (IL_FLD(IL_IDX(list_idx)) != AT_Tbl_Idx) {
         PRINTMSG(IR_LINE_NUM(ir_idx), 626, Internal, IR_COL_NUM(ir_idx),
                  "AT_Tbl_Idx", "forall_semantics");
      }
# endif

      find_opnd_line_and_column(&(IL_OPND(IL_IDX(list_idx))), &line, &col);

      COPY_OPND(opnd, IL_OPND(IL_IDX(list_idx)));
      exp_desc.rank = 0;
      xref_state    = CIF_Symbol_Modification;

      ok &= expr_semantics(&opnd, &exp_desc);

      if (OPND_FLD(opnd) == IR_Tbl_Idx &&
          IR_OPR(OPND_IDX(opnd)) == Dv_Deref_Opr) {
         COPY_OPND(opnd, IR_OPND_L(OPND_IDX(opnd)));
      }
      COPY_OPND(IL_OPND(IL_IDX(list_idx)), opnd);

      if (OPND_FLD(opnd) != AT_Tbl_Idx ||
          exp_desc.rank != 0 ||
          exp_desc.type != Integer ||
          ATD_CLASS(OPND_IDX(opnd)) == Constant) {

         PRINTMSG(line, 1598, Error, col);
         ok = FALSE;
      }
      else {
         index_idx = OPND_IDX(opnd);

         if (ATD_FORALL_INDEX(index_idx)) {

         /* BHJ - need to distinguish nested reuse from same forall reuse */

            PRINTMSG(line, 1599, Error, col, 
                     AT_OBJ_NAME_PTR(index_idx));
            ok = FALSE;
         }
         else {

            tmp_idx = gen_compiler_tmp(line, col, Priv, TRUE);

            AT_SEMANTICS_DONE(tmp_idx)= TRUE;
            ATD_TYPE_IDX(tmp_idx)     = ATD_TYPE_IDX(index_idx);
            ATD_STOR_BLK_IDX(tmp_idx) = SCP_SB_STACK_IDX(curr_scp_idx);
            ATD_FORALL_INDEX(tmp_idx) = TRUE;

            /* change name to original name */
            AT_NAME_IDX(tmp_idx) = AT_NAME_IDX(index_idx);
            AT_NAME_LEN(tmp_idx) = AT_NAME_LEN(index_idx);

            AT_ATTR_LINK(index_idx)         = tmp_idx;
            AT_IGNORE_ATTR_LINK(index_idx)  = TRUE;

            ATD_TMP_NEEDS_CIF(tmp_idx) = TRUE;

            /* issue a usage rec if needed */
            if ((cif_flags & XREF_RECS) != 0) {
               cif_usage_rec(tmp_idx, AT_Tbl_Idx, line, col,
                             CIF_Symbol_Modification);
            }
         }
      }

      list_idx = IL_NEXT_LIST_IDX(list_idx);
   }

   if (! ok ) {
      goto EXIT;
   }

   /* process subscripts and strides */

   list_idx = IR_IDX_R(ir_idx);

   while (list_idx &&
          IL_FLD(list_idx) == IL_Tbl_Idx) {

      type_idx = ATD_TYPE_IDX(IL_IDX(IL_IDX(list_idx)));

      list_idx2 = IL_NEXT_LIST_IDX(IL_IDX(list_idx));

      while (list_idx2) {
         find_opnd_line_and_column(&(IL_OPND(list_idx2)), &line, &col);

         COPY_OPND(opnd, IL_OPND(list_idx2));
         exp_desc.rank = 0;
         xref_state    = CIF_Symbol_Reference;
         ok &= expr_semantics(&opnd, &exp_desc);
         COPY_OPND(IL_OPND(list_idx2), opnd);

         /* check type and rank */

         if (exp_desc.type != Integer ||
             exp_desc.rank != 0) {

            PRINTMSG(line, 1604, Error, col);
            ok = FALSE;
         }

         ok &= check_forall_triplet_for_index(&opnd);

         /* cast to type_idx if appropriate */

         if (ok) {
            cast_to_type_idx(&opnd, 
                             &exp_desc,
                              type_idx);
            COPY_OPND(IL_OPND(list_idx2), opnd);
         }

         if (ok &&
             OPND_FLD(opnd) != CN_Tbl_Idx) {

            /* capture into tmp */

            tmp_idx = create_tmp_asg(&opnd,
                                     &exp_desc,
                                     &l_opnd,
                                     Intent_In,
                                     FALSE,
                                     FALSE);

            COPY_OPND(IL_OPND(list_idx2), l_opnd);
         }


         list_idx2 = IL_NEXT_LIST_IDX(list_idx2);
      }

      ok &= gen_forall_max_expr(IL_NEXT_LIST_IDX(IL_IDX(list_idx)),
                                &opnd);

      if (OPND_FLD(br_around_opnd) == NO_Tbl_Idx) {
         COPY_OPND(br_around_opnd, opnd);
      }
      else {
         or_idx = gen_ir(OPND_FLD(br_around_opnd), OPND_IDX(br_around_opnd),
                    Or_Opr, LOGICAL_DEFAULT_TYPE, line, col,
                         OPND_FLD(opnd), OPND_IDX(opnd));

         gen_opnd(&br_around_opnd, or_idx, IR_Tbl_Idx, line, col);
      }
  

      list_idx = IL_NEXT_LIST_IDX(list_idx);
   }

   if (ok) {
      gen_forall_branch_around(&br_around_opnd);
   }

   if (ok &&
       list_idx != NULL_IDX) {

      /* have mask */

      /* these capture the stmts around the loop body */
      /* they must be moved in after all body stmts are generated */

      body_start_sh_idx = curr_stmt_sh_idx;
      body_end_sh_idx = SH_NEXT_IDX(curr_stmt_sh_idx);

      find_opnd_line_and_column(&(IL_OPND(list_idx)), &line, &col);

      gen_sh(After, Assignment_Stmt, line, col, FALSE, FALSE, TRUE);

      /* curr_stmt_sh_idx is an empty assignment stmt right now. */

      within_forall_mask_expr = TRUE;
      COPY_OPND(opnd, IL_OPND(list_idx));
      exp_desc.rank = 0;
      xref_state    = CIF_Symbol_Reference;
      io_item_must_flatten = FALSE;

      if (expr_semantics(&opnd, &exp_desc)) {

         /* do not put the transformed opnd back on the forall stmt */

         if (exp_desc.type != Logical ||
             exp_desc.rank != 0) {

            PRINTMSG(line, 1607, Error, col);
            ok = FALSE;
         }
      }
      else {
         ok = FALSE;
      }

      within_forall_mask_expr = FALSE;

      if (SH_PREV_IDX(curr_stmt_sh_idx) != body_start_sh_idx ||
          SH_NEXT_IDX(curr_stmt_sh_idx) != body_end_sh_idx ||
          io_item_must_flatten ||
          forall_mask_needs_tmp(&opnd)) {

         NTR_IR_TBL(asg_idx);
         IR_OPR(asg_idx) = Asg_Opr;
         IR_TYPE_IDX(asg_idx) = exp_desc.type_idx;
         IR_LINE_NUM(asg_idx) = line;
         IR_COL_NUM(asg_idx) = col;

         COPY_OPND(IR_OPND_R(asg_idx), opnd);

         SH_IR_IDX(curr_stmt_sh_idx) = asg_idx;
         SH_P2_SKIP_ME(curr_stmt_sh_idx) = TRUE;

         gen_forall_tmp(&exp_desc, &opnd, line, col, FALSE);

         COPY_OPND(IR_OPND_L(asg_idx), opnd);

         /* save the mask temp as an additional list item */

         NTR_IR_LIST_TBL(IL_NEXT_LIST_IDX(list_idx));
         IL_PREV_LIST_IDX(IL_NEXT_LIST_IDX(list_idx)) = list_idx;
         list_idx = IL_NEXT_LIST_IDX(list_idx);
         IR_LIST_CNT_R(ir_idx) += 1;

         COPY_OPND(IL_OPND(list_idx), opnd);
   
         body_start_sh_idx = SH_NEXT_IDX(body_start_sh_idx);
         body_end_sh_idx = SH_PREV_IDX(body_end_sh_idx);
   
         gen_forall_loops(body_start_sh_idx, body_end_sh_idx);
      }
      else {
         curr_stmt_sh_idx = SH_PREV_IDX(curr_stmt_sh_idx);
         remove_sh(SH_NEXT_IDX(curr_stmt_sh_idx));

         NTR_IR_LIST_TBL(IL_NEXT_LIST_IDX(list_idx));
         IL_PREV_LIST_IDX(IL_NEXT_LIST_IDX(list_idx)) = list_idx;
         list_idx = IL_NEXT_LIST_IDX(list_idx);
         IR_LIST_CNT_R(ir_idx) += 1;

         COPY_OPND(IL_OPND(list_idx), opnd);
      }
   }

   within_forall_construct = TRUE;

EXIT:

   curr_stmt_sh_idx = SH_PREV_IDX(save_next_sh_idx);

   TRACE (Func_Exit, "forall_semantics", NULL);

   return;

}  /* forall_semantics */

/******************************************************************************\
|*									      *|
|* Description:								      *|
|*	Complete the processing for all user forms of the GO TO statement.    *|
|*      This procedure is also called to handle the compiler-generated GO TO  *|
|*      (no processing).						      *|
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
|*      The semantic checks made in this routine are very similar to those    *|
|*      made in assign_stmt_semantics for the ASSIGN stmt.  If you make a     *|
|*      change here, chances are the same (or similar) change will need to be *|
|*      made to the ASSIGN statement code.				      *|
|*                                                                            *|
\******************************************************************************/

void goto_stmt_semantics (void)

{
   int	 		attr_idx;
   int			column;
   expr_arg_type	expr_desc;
#ifdef KEY /* Bug 10177 */
   boolean		in_assign_stmt = TRUE;
#else /* KEY Bug 10177 */
   boolean		in_assign_stmt;
#endif /* KEY Bug 10177 */
   int 	        	ir_idx;
   int			lbl_idx;
   int			tmp_idx;
   int			line;
   opnd_type		opnd;
   opnd_type		l_opnd;
 

   TRACE (Func_Entry, "goto_stmt_semantics", NULL);

   if (SH_COMPILER_GEN(curr_stmt_sh_idx)) {
      goto EXIT;
   }

   ir_idx = SH_IR_IDX(curr_stmt_sh_idx);

   switch (IR_OPR(ir_idx)) {
   
      case Br_Uncond_Opr:

         /* If the GO TO is followed by a stmt that is not labeled, issue a   */
         /* warning message (at the following stmt) that the stmt can not be  */
         /* reached.						              */

         chk_for_unlabeled_stmt();
         break;

      case Br_Index_Opr:     /* Computed GO TO:  GO TO (lbl-list), expr       */
         COPY_OPND(opnd, IR_OPND_L(ir_idx));
         expr_desc.rank = 0;
         xref_state = CIF_Symbol_Reference;

         if (expr_semantics(&opnd, &expr_desc)) {
            find_opnd_line_and_column(&opnd, &line, &column);
            tmp_idx = create_tmp_asg(&opnd,
                                     &expr_desc,
                                     &l_opnd,
                                     Intent_In,
                                     TRUE,
                                     FALSE);

            if (expr_desc.type == Integer && expr_desc.rank == 0) {
               COPY_OPND(IR_OPND_L(ir_idx), l_opnd);
            }
            else {
               PRINTMSG(line, 369, Error, column);
            }
         } 

         break;

      case Br_Asg_Opr:       /* Assigned GO TO:  GO TO var [ [,] (lbl-list)]  */

         /* If the GO TO is followed by a stmt that is not labeled, issue a   */
         /* warning message (at the following stmt) that the stmt can not be  */
         /* reached.						              */

         chk_for_unlabeled_stmt();

         COPY_OPND(opnd, IR_OPND_L(ir_idx));

         /* The variable must have been assigned a label value SOMEWHERE in   */
         /* the CURRENT scoping unit (and that's why we have to grab the flag */
         /* before expr_semantics (possibly) resolves the reference to an     */
         /* Attr in the host).				                      */

         if (OPND_FLD(opnd) == AT_Tbl_Idx  &&
             AT_OBJ_CLASS(OPND_IDX(opnd)) == Data_Obj) {
            in_assign_stmt = ATD_IN_ASSIGN(OPND_IDX(opnd));
         }

         expr_desc.rank = 0;
         xref_state     = CIF_Symbol_Reference;

         if (expr_semantics(&opnd, &expr_desc)) {

            switch (OPND_FLD(opnd)) {

               case AT_Tbl_Idx:
                  COPY_OPND(IR_OPND_L(ir_idx), opnd);
                  attr_idx = IR_IDX_L(ir_idx);

                  /* If it's not a Data_Obj, don't do any more checking       */
                  /* because the variants are not valid (not allowed to       */
                  /* access ATD_IN_ASSIGN, for example, if AT_OBJ_CLASS is    */
                  /* not Data_Obj).					      */

                  if (AT_OBJ_CLASS(attr_idx) == Data_Obj) {

                     if (TYP_LINEAR(ATD_TYPE_IDX(attr_idx)) == 
                            INTEGER_DEFAULT_TYPE  &&
                         expr_desc.rank == 0) {

                        /* Verify that the variable was assigned a label      */
                        /* value SOMEWHERE in the current scoping unit.       */
                        /* Note that, like CFT77, CF90 does not verify that   */
                        /* the current value of the variable is a label nor   */
                        /* does it verify that the value is one of the labels */
                        /* in the list, if indeed the list exists.            */

                        if (! in_assign_stmt) {
                           PRINTMSG(IR_LINE_NUM_L(ir_idx), 340, Error,
                                    IR_COL_NUM_L(ir_idx),
                                    AT_OBJ_NAME_PTR(attr_idx));
                        }

                        break;
                     }
  
	          }

                  PRINTMSG(IR_LINE_NUM_L(ir_idx), 142, Error,
                           IR_COL_NUM_L(ir_idx), AT_OBJ_NAME_PTR(attr_idx));
                  break;

               case CN_Tbl_Idx:
                  find_opnd_line_and_column(&opnd, &line, &column);
                  PRINTMSG(line, 569, Error, column,
                           AT_OBJ_NAME_PTR(IR_IDX_L(ir_idx)));
                  break;
               
               case IR_Tbl_Idx:
                  /* Only case should be a Whole_Subscript IR.		      */
                  
                  PRINTMSG(IR_LINE_NUM_L(ir_idx), 142, Error,
                           IR_COL_NUM_L(ir_idx),
                           AT_OBJ_NAME_PTR(IR_IDX_L(ir_idx)));
                  break;
            
               default:
                  find_opnd_line_and_column(&opnd, &line, &column);
                  PRINTMSG(line, 179, Internal, column,
                           "goto_stmt_semantics");
            }
         }

         /* If the label list exists, check each label to verify that it      */
         /* appeared in an ASSIGN statement SOMEWHERE in the current scoping  */
         /* unit.  CFT77 doesn't make this check so to avoid possibly irate   */
         /* customers, CF90 issues a warning message rather than an error     */
         /* message like is issued above for the variable.                    */
       
         lbl_idx = IR_IDX_R(ir_idx);

         while (lbl_idx != NULL_IDX) {

            if ( ! ATL_IN_ASSIGN(IL_IDX(lbl_idx)) ) {
               PRINTMSG(IL_LINE_NUM(lbl_idx), 349, Warning, IL_COL_NUM(lbl_idx),
                        AT_OBJ_NAME_PTR(IL_IDX(lbl_idx)));
            }

            lbl_idx = IL_NEXT_LIST_IDX(lbl_idx);
         }

         if (ATP_HAS_TASK_DIRS(SCP_ATTR_IDX(curr_scp_idx))) {
            /* can't have assigned goto when subprogram has cmics */
            PRINTMSG(stmt_start_line, 1210, Error, stmt_start_col);
         }
   }         
      
EXIT:

   TRACE (Func_Exit, "goto_stmt_semantics", NULL);

   return;

}  /* goto_stmt_semantics */


/******************************************************************************\
|*									      *|
|* Description:								      *|
|*	This procedure completes the processing of the logical IF statement   *|
|*      and of the IF-THEN portion of an IF construct.			      *|
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

void if_stmt_semantics (void)

{
   opnd_type		cond_expr;
   int			cond_expr_ir_idx;
   expr_arg_type        exp_desc;
   boolean		ok = TRUE;
   int			sh_idx;

# ifndef _HIGH_LEVEL_IF_FORM
   int			il_idx_1;
   int			il_idx_2;
   int			ir_idx;
# endif


   TRACE (Func_Entry, "if_stmt_semantics", NULL);

   /* The conditional expression must be scalar and type logical.             */
   
   cond_expr_ir_idx = SH_IR_IDX(curr_stmt_sh_idx);
   COPY_OPND(cond_expr, IR_OPND_L(cond_expr_ir_idx));

   exp_desc.rank = 0;
   xref_state    = CIF_Symbol_Reference;

   if (! SH_COMPILER_GEN(curr_stmt_sh_idx)) {
      in_branch_true       = TRUE;
      defer_stmt_expansion = TRUE;
      io_item_must_flatten = FALSE;
      number_of_functions  = 0;
   }

   has_present_opr = FALSE;
   ok = expr_semantics(&cond_expr, &exp_desc);
   has_present_opr = FALSE;

   COPY_OPND(IR_OPND_L(cond_expr_ir_idx), cond_expr);

   defer_stmt_expansion = FALSE;
   in_branch_true       = FALSE;

   if (ok && exp_desc.rank != 0) {
      PRINTMSG(IR_LINE_NUM(cond_expr_ir_idx), 410, Error,
               IR_COL_NUM(cond_expr_ir_idx));
   }

   if (ok && exp_desc.type != Logical) {
      PRINTMSG(IR_LINE_NUM(cond_expr_ir_idx), 416, Error,
               IR_COL_NUM(cond_expr_ir_idx));
   } 

# ifdef _HIGH_LEVEL_IF_FORM

   /* Reset the operator and clear the right operand (where the index   */
   /* to the branch-around label had been stored by the Syntax Pass).   */
   /* It's not needed (but is used as a flag in that pass) so it's just */
   /* easier to generate then and delete now.			      */

   IR_OPR(cond_expr_ir_idx) = If_Opr;

   if (SH_STMT_TYPE(curr_stmt_sh_idx) == If_Stmt) {
      IR_OPND_R(cond_expr_ir_idx) = null_opnd;
   }

#endif

   IR_TYPE_IDX(cond_expr_ir_idx) = exp_desc.type_idx;

   if (SH_COMPILER_GEN(curr_stmt_sh_idx)) {
      COPY_OPND(IR_OPND_L(cond_expr_ir_idx), cond_expr);
   }
   else {

      /* If the IF is a logical IF of the form:		              */
      /*    IF (cond) GO TO <lbl>				      */
      /* PDGCS would like the usual form:                             */
      /*    If_Stmt          ->  Br_True			      */
      /*                           Not				      */
      /*                             cond			      */
      /*                           <cg-lbl>	                      */
      /*    Goto_Stmt        ->  Br_Uncond  Null, <user-lbl>          */
      /*    CG Continue_Stmt ->  Label      <cg-lbl>		      */
      /* simplified to:						      */
      /*    If_Stmt          ->  Br_True  cond, <user-lbl>	      */

      if ((SH_STMT_TYPE(SH_NEXT_IDX(curr_stmt_sh_idx)) == Goto_Stmt  &&
           IR_OPR(SH_IR_IDX(SH_NEXT_IDX(curr_stmt_sh_idx))) ==
             Br_Uncond_Opr)  ||
          SH_STMT_TYPE(SH_NEXT_IDX(curr_stmt_sh_idx)) == Cycle_Stmt  ||
          SH_STMT_TYPE(SH_NEXT_IDX(curr_stmt_sh_idx)) == Exit_Stmt) {
         COPY_OPND(IR_OPND_L(cond_expr_ir_idx), cond_expr);
         COPY_OPND(IR_OPND_R(cond_expr_ir_idx), 
                   IR_OPND_R(SH_IR_IDX(SH_NEXT_IDX(curr_stmt_sh_idx))));

#ifdef _HIGH_LEVEL_IF_FORM

         /* Restore the operator (changed to If_Opr not far above).     */

         IR_OPR(cond_expr_ir_idx) = Br_True_Opr;

#endif

         /* Link ahead to the CG End_If_Stmt SH (for high-level IF) or  */
         /* to the Continue_Stmt SH (for low-level IF).  Delete the     */
         /* SH's for the GO TO stmt and the CG End_If/Continue_Stmt.    */

         sh_idx = SH_NEXT_IDX(SH_NEXT_IDX(curr_stmt_sh_idx));
         SH_NEXT_IDX(curr_stmt_sh_idx) = SH_NEXT_IDX(sh_idx);
         if (SH_NEXT_IDX(curr_stmt_sh_idx)) {
            SH_PREV_IDX(SH_NEXT_IDX(curr_stmt_sh_idx)) = curr_stmt_sh_idx;
         }
      }

# ifndef _HIGH_LEVEL_IF_FORM

      else {
        
         if (SH_STMT_TYPE(curr_stmt_sh_idx) == If_Cstrct_Stmt) {

            /* Genererate the branch-around label and save it in the    */
            /* first IL attached to the right operand of the If_Opr IR. */
            /* Generate the END IF label and save it in the second IL   */
            /* attached to right operand of the If_Opr IR.  The latter  */
            /* label's Attr entry fields are completed as a part of     */
            /* END IF processing.  				      */

            NTR_IR_LIST_TBL(il_idx_1);
            IR_LIST_CNT_R(cond_expr_ir_idx) = 1;
            IR_FLD_R(cond_expr_ir_idx)      = IL_Tbl_Idx;
            IR_IDX_R(cond_expr_ir_idx)      = il_idx_1;

            IL_LINE_NUM(il_idx_1) = stmt_start_line;
            IL_COL_NUM(il_idx_1)  = stmt_start_col;
            IL_FLD(il_idx_1)      = AT_Tbl_Idx;
            IL_IDX(il_idx_1)      = gen_internal_lbl(stmt_start_line);

            NTR_IR_LIST_TBL(il_idx_2);
            IR_LIST_CNT_R(cond_expr_ir_idx) = 2;
            IL_NEXT_LIST_IDX(il_idx_1)      = il_idx_2;
            IL_PREV_LIST_IDX(il_idx_2)      = il_idx_1;

            IL_LINE_NUM(il_idx_2) = stmt_start_line;
            IL_COL_NUM(il_idx_2)  = stmt_start_col;
            IL_FLD(il_idx_2)      = AT_Tbl_Idx;
            IL_IDX(il_idx_2)      = gen_internal_lbl(stmt_start_line);
         }


         /* Generate the ".NOT. cond" IR under the Br_True IR.	      */

         NTR_IR_TBL(ir_idx);
         IR_FLD_L(cond_expr_ir_idx) = IR_Tbl_Idx;
         IR_IDX_L(cond_expr_ir_idx) = ir_idx;
         IR_OPR(ir_idx)             = Not_Opr;
         IR_TYPE_IDX(ir_idx)        = exp_desc.type_idx;
         IR_LINE_NUM(ir_idx)        = stmt_start_line;
         IR_COL_NUM(ir_idx)         = stmt_start_col;
         COPY_OPND(IR_OPND_L(ir_idx), cond_expr);
      }

#endif

      /* short_circuit_branch calls process_deferred_functions.         */


#ifdef _HIGH_LEVEL_IF_FORM

      if (ok) {
         short_circuit_high_level_if();
      }
#else

      if (ok) {
         short_circuit_branch();
      }

#endif

   }

   if (! ok) {
      SH_ERR_FLG(curr_stmt_sh_idx) = TRUE;    /* Make sure this gets set.     */
   }

   in_branch_true       = FALSE;
   defer_stmt_expansion = FALSE;
   io_item_must_flatten = FALSE;
   arg_info_list_base   = NULL_IDX;
   arg_info_list_top    = NULL_IDX;
   
   TRACE (Func_Exit, "if_stmt_semantics", NULL);

   return;

}  /* if_stmt_semantics */


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

void nullify_stmt_semantics (void)

{
   int		 attr_idx;
   int		 column;
   int		 dv_idx;
   expr_arg_type exp_desc;
   int		 ir_idx;
   int		 line;
   int		 list_idx;
   opnd_type     opnd;
   boolean       semantically_correct = TRUE;


   TRACE (Func_Entry, "nullify_stmt_semantics", NULL);

   ir_idx = SH_IR_IDX(curr_stmt_sh_idx);

   list_idx = IR_IDX_L(ir_idx);

   while (list_idx != NULL_IDX) {
   
      if (IL_FLD(list_idx) == IR_Tbl_Idx        &&
          IR_OPR(IL_IDX(list_idx)) == Call_Opr) {

         /* catch before expr_semantics to stop bad msgs */
         /* error .. must be pointer */

         PRINTMSG(IR_LINE_NUM(IL_IDX(list_idx)), 426, Error,
                  IR_COL_NUM(IL_IDX(list_idx)));
         semantically_correct = FALSE;
      }
      else {
         exp_desc.rank = 0;
         COPY_OPND(opnd, IL_OPND(list_idx));
         xref_state = CIF_Symbol_Modification;
         semantically_correct = expr_semantics(&opnd, &exp_desc);
         COPY_OPND(IL_OPND(list_idx), opnd);

         if (!exp_desc.pointer) {
            find_opnd_line_and_column(&opnd, &line, &column);
            PRINTMSG(line, 426, Error, column);
            semantically_correct = FALSE;
         }
#ifdef KEY /* Bug 572 */
	 /* An expression like "parameter_x%ptr_component_y" may be both a
	  * pointer and a constant, but a constant is not allowed here. */
	 else if (exp_desc.constant) {
            find_opnd_line_and_column(&opnd, &line, &column);
	    PRINTMSG(line, 1650, Error, column);
	    semantically_correct = FALSE;
	 }
#endif /* KEY Bug 572 */
         else {

            if (ATP_PURE(SCP_ATTR_IDX(curr_scp_idx)) ||
                ATP_ELEMENTAL(SCP_ATTR_IDX(curr_scp_idx))) {
               find_opnd_line_and_column(&opnd, &line, &column);
               attr_idx = find_left_attr(&opnd);

               if (AT_OBJ_CLASS(attr_idx) == Data_Obj && ATD_PURE(attr_idx)) {
                  semantically_correct = FALSE;
                  PRINTMSG(line, 1270, Error, column,
                           AT_OBJ_NAME_PTR(attr_idx),
                           ATP_PURE(SCP_ATTR_IDX(curr_scp_idx)) ? 
                                    "pure":"elemental");
               }
            }

            while (OPND_FLD(opnd) == IR_Tbl_Idx &&
                   (IR_OPR(OPND_IDX(opnd)) == Whole_Substring_Opr ||
                    IR_OPR(OPND_IDX(opnd)) == Whole_Subscript_Opr)) {
               COPY_OPND(opnd, IR_OPND_L(OPND_IDX(opnd)));
            }

            find_opnd_line_and_column(&opnd, &line, &column);

            if (OPND_FLD(opnd)         == IR_Tbl_Idx &&
                IR_OPR(OPND_IDX(opnd)) == Dv_Deref_Opr) {
 
               NTR_IR_TBL(dv_idx);
               IR_OPR(dv_idx)		= Dv_Set_Assoc;
               IR_TYPE_IDX(dv_idx)	= CG_INTEGER_DEFAULT_TYPE;
               IR_LINE_NUM(dv_idx)	= line;
               IR_COL_NUM(dv_idx)	= column;

               COPY_OPND(IR_OPND_L(dv_idx), IR_OPND_L(OPND_IDX(opnd)));

               IR_FLD_R(dv_idx)		= CN_Tbl_Idx;
               IR_IDX_R(dv_idx)		= CN_INTEGER_ZERO_IDX;
               IR_LINE_NUM_R(dv_idx)	= line;
               IR_COL_NUM_R(dv_idx)	= column;
 
               gen_sh(Before, Assignment_Stmt, line,
                      column, FALSE, FALSE, TRUE);

               SH_IR_IDX(SH_PREV_IDX(curr_stmt_sh_idx))		= dv_idx;
               SH_P2_SKIP_ME(SH_PREV_IDX(curr_stmt_sh_idx))	= TRUE;
#ifdef KEY /* Bug 9608 */
	      /*
	       * When we set assoc=0 for an array, we also set contig=1 so that
	       * copyinout doesn't blow up if user (illegally) passes the null
	       * pointer to a procedure lacking an explicit interface, in the
	       * (unjustified) expectation that the pointer won't be
	       * dereferenced if the procedure doesn't refer to the dummy
	       * argument. This seems cheaper than adding a test for null
	       * before and after every call.
	       */
	      if (exp_desc.rank) {
		gen_sh(Before, Assignment_Stmt, line, column, FALSE, FALSE,
		  TRUE);
		int contig_stmt_idx = SH_PREV_IDX(curr_stmt_sh_idx);
		SH_IR_IDX(contig_stmt_idx) = gen_ir(
		  IR_FLD_L(OPND_IDX(opnd)), IR_IDX_L(OPND_IDX(opnd)),
		  Dv_Set_A_Contig, CG_INTEGER_DEFAULT_TYPE, line, column,
		  CN_Tbl_Idx, CN_INTEGER_ONE_IDX);
		SH_P2_SKIP_ME(SH_PREV_IDX(curr_stmt_sh_idx)) = TRUE;
	      }
#endif /* KEY Bug 9608 */
            }
            else {
               PRINTMSG(line, 626, Internal, column,
                        "Dv_Deref_Opr", "nullify_stmt_semantics");
            }
         }
      }

      list_idx = IL_NEXT_LIST_IDX(list_idx);
   }

   if (semantically_correct) {
      /* remove nullify stmt */
      remove_sh(curr_stmt_sh_idx);
      curr_stmt_sh_idx = SH_PREV_IDX(curr_stmt_sh_idx);
   }

   TRACE (Func_Exit, "nullify_stmt_semantics", NULL);

   return;

}  /* nullify_stmt_semantics */


/******************************************************************************\
|*									      *|
|* Description:								      *|
|*	This procedure handles the semantic processing for the outmoded       *|
|*      indirect logical IF and the outmoded two-branch arithmetic IF stmts.  *|
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

void outmoded_if_stmt_semantics (void)

{

   int			br_ir_idx;
   int			col;
   opnd_type		cond_expr;
   expr_arg_type        exp_desc;
   int			il_idx;
   int			ir_idx;
   int			lbl_list_idx;
   int			line;


   TRACE (Func_Entry, "outmoded_if_stmt_semantics", NULL);

   /* If the outmoded IF is followed by a stmt that is not labeled, issue     */
   /* a warning message (at the following stmt) that the stmt can not be      */
   /* reached.							              */

   chk_for_unlabeled_stmt();

   /* The conditional expression must be scalar.			      */
   /* If the expression is a numeric type, the stmt is a two-branch	      */
   /* arithmetic IF; the numeric type must not be complex.		      */
   /* If the expression is type logical, the stmt is an indirect logical IF.  */
   /* Any other data type is an error.                 			      */
   
   br_ir_idx = SH_IR_IDX(curr_stmt_sh_idx);

   COPY_OPND(cond_expr, IR_OPND_L(br_ir_idx));
   exp_desc.rank = 0;
   xref_state    = CIF_Symbol_Reference;

   if (! expr_semantics(&cond_expr, &exp_desc)) {
      goto EXIT;
   }

   COPY_OPND(IR_OPND_L(br_ir_idx), cond_expr);

   if (exp_desc.type != Integer  &&  
       exp_desc.type != Real     &&  
       exp_desc.type != Logical  &&
       exp_desc.type != Typeless) {
      PRINTMSG(IR_LINE_NUM(br_ir_idx), 414, Error, IR_COL_NUM(br_ir_idx));
   } 

   if (exp_desc.rank != 0) {
      PRINTMSG(IR_LINE_NUM(br_ir_idx), 410, Error, IR_COL_NUM(br_ir_idx));
   }

   if (SH_ERR_FLG(curr_stmt_sh_idx)) {
      goto EXIT;
   }

   lbl_list_idx = IR_IDX_R(br_ir_idx);

   if (exp_desc.type == Logical) {

      if (cif_flags & MISC_RECS) {
         cif_stmt_type_rec(TRUE, 
                           CIF_If_Indirect_Logical_Stmt, 
                           statement_number);
      }

      /* Fill in the Br_True IR header fields and set the right operand to be */
      /* the first label.		      				      */

      IR_OPR(br_ir_idx)		= Br_True_Opr;
      IR_TYPE_IDX(br_ir_idx)	= LOGICAL_DEFAULT_TYPE;
      IR_LINE_NUM(br_ir_idx)	= stmt_start_line;
      IR_COL_NUM(br_ir_idx)	= stmt_start_col;

      COPY_OPND(IR_OPND_R(br_ir_idx), IL_OPND(IL_NEXT_LIST_IDX(lbl_list_idx)));
      FREE_IR_LIST_NODE(IL_NEXT_LIST_IDX(lbl_list_idx));

      /* Generate    Goto_Stmt ---> Br_Uncond                                 */
      /*                              Left:  null		     	      */
      /*                              Right: label 2			      */

      gen_sh(After, Goto_Stmt, stmt_start_line, stmt_start_col,
             FALSE, FALSE, TRUE);

      NTR_IR_TBL(ir_idx);
      SH_IR_IDX(curr_stmt_sh_idx) = ir_idx;
      IR_OPR(ir_idx)              = Br_Uncond_Opr;
      IR_TYPE_IDX(ir_idx)         = TYPELESS_DEFAULT_TYPE;
      IR_LINE_NUM(ir_idx)         = stmt_start_line;
      IR_COL_NUM(ir_idx)          = stmt_start_col;

      COPY_OPND(IR_OPND_R(ir_idx), IL_OPND(lbl_list_idx));
      FREE_IR_LIST_NODE(lbl_list_idx);
   }
   else {

      if (cif_flags & MISC_RECS) {
         cif_stmt_type_rec(TRUE, 
                           CIF_If_Two_Branch_Arithmetic_Stmt,
                           statement_number);
      }
 
      /* CRI extension:  The "type" of the expression may be typeless.     */
      /* PDGCS treats the expression (result) as an integer.               */
      /* If the expression is a typeless constant that is longer than a    */
      /* word, truncate it and reenter it as an integer.                   */

      if (exp_desc.linear_type == Long_Typeless) {
         IR_IDX_L(br_ir_idx) = ntr_const_tbl(CG_INTEGER_DEFAULT_TYPE,
                                             FALSE,
			                     &CN_CONST(IR_IDX_L(br_ir_idx)));
      }
      else if (exp_desc.linear_type == Short_Typeless_Const) {
         find_opnd_line_and_column(&(IR_OPND_L(br_ir_idx)), &line, &col);
         IR_IDX_L(br_ir_idx) = cast_typeless_constant(IR_IDX_L(br_ir_idx),
                                                      INTEGER_DEFAULT_TYPE,
                                                      line,
                                                      col);
         exp_desc.linear_type = INTEGER_DEFAULT_TYPE;
         exp_desc.type_idx    = INTEGER_DEFAULT_TYPE;
         exp_desc.type        = Integer;
      }

      /* Change the IR to look like a normal arithmetic IF.  Insert an IL     */
      /* between the two exiting ILs and copy the label operand from the     */
      /* second IL to the new IL.  This will make a nonzero condition jump to */
      /* label-1 and a zero condition jump to label-2.  		      */

      NTR_IR_LIST_TBL(il_idx);

      IL_NEXT_LIST_IDX(il_idx)		= IL_NEXT_LIST_IDX(lbl_list_idx);
      IL_NEXT_LIST_IDX(lbl_list_idx)	= il_idx;

      IL_PREV_LIST_IDX(IL_NEXT_LIST_IDX(il_idx)) = il_idx;
      IL_PREV_LIST_IDX(il_idx)                   = lbl_list_idx;

      COPY_OPND(IL_OPND(il_idx), IL_OPND(IL_NEXT_LIST_IDX(il_idx)));

      ++IR_LIST_CNT_R(br_ir_idx);
   }

EXIT:

   TRACE (Func_Exit, "outmoded_if_stmt_semantics", NULL);

   return;

}  /* outmoded_if_stmt_semantics */


/******************************************************************************\
|*									      *|
|* Description:								      *|
|*	Do the semantic processing for a RETURN statement.  		      *|
|*      Verify that the expression which follows a RETURN statement is        *|
|*      a scalar integer expression.                                          *|
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

void return_stmt_semantics (void)

{
   int			idx;
   int			ir_idx;
   expr_arg_type	exp_desc;
   int                  new_end_idx;
   size_offset_type     new_size;
   int                  new_start_idx;
   opnd_type		opnd;
   int			ptr;
   size_offset_type	result;
   int			rslt_idx;
   boolean		semantically_correct;
   size_offset_type	size;


   TRACE (Func_Entry, "return_stmt_semantics", NULL);

   if (cdir_switches.parallel_region) {

      /* a return stmt is illegal within a parallel region */

      PRINTMSG(stmt_start_line, 549, Error, stmt_start_col, "RETURN");
   }

   ir_idx = SH_IR_IDX(curr_stmt_sh_idx);

   /* If an alternate return specifier exits. */

   if (IR_FLD_L(ir_idx) != NO_Tbl_Idx) {
      COPY_OPND(opnd, IR_OPND_L(ir_idx));
      exp_desc.rank		= 0;
      xref_state		= CIF_Symbol_Reference;
      semantically_correct	= expr_semantics(&opnd,
                                                 &exp_desc);
      COPY_OPND(IR_OPND_L(ir_idx), opnd);

      if (semantically_correct &&
          (exp_desc.rank != 0 || exp_desc.type != Integer)) {
         PRINTMSG(IR_LINE_NUM(ir_idx), 369, Error, IR_COL_NUM(ir_idx));
         semantically_correct = FALSE;
      }

      /* check to see if the return specifier needs to be cast to cg default */
      if (semantically_correct) {
         COPY_OPND(opnd, IR_OPND_L(ir_idx));
         cast_to_cg_default(&opnd, &exp_desc);
         COPY_OPND(IR_OPND_L(ir_idx), opnd);
      }
   }

   if (ATP_PGM_UNIT(SCP_ATTR_IDX(curr_scp_idx)) == Function) {
      rslt_idx = ATP_RSLT_IDX(SCP_ATTR_IDX(curr_scp_idx));

      if (!ATD_IM_A_DOPE(rslt_idx) &&
          ATD_ARRAY_IDX(rslt_idx) == NULL_IDX &&
          TYP_TYPE(ATD_TYPE_IDX(rslt_idx)) != Structure &&
          TYP_TYPE(ATD_TYPE_IDX(rslt_idx)) != Character) {

# ifdef _SEPARATE_FUNCTION_RETURNS
         if (SCP_ALT_ENTRY_CNT(curr_scp_idx) != 0 &&
             SCP_RETURN_LABEL(curr_scp_idx) != NULL_IDX) {
            /* change return to goto to multiple return code block */
            IR_OPR(ir_idx)   = Br_Uncond_Opr;
            IR_FLD_R(ir_idx) = AT_Tbl_Idx;
            IR_IDX_R(ir_idx) = SCP_RETURN_LABEL(curr_scp_idx);
            IR_LINE_NUM_R(ir_idx)  = IR_LINE_NUM(ir_idx);
            IR_COL_NUM_R(ir_idx)   = IR_COL_NUM(ir_idx);
         }
         else {
            IR_FLD_R(ir_idx)       = AT_Tbl_Idx;
            IR_IDX_R(ir_idx)       = rslt_idx;
            IR_LINE_NUM_R(ir_idx)  = IR_LINE_NUM(ir_idx);
            IR_COL_NUM_R(ir_idx)   = IR_COL_NUM(ir_idx);
         }
# else

         IR_FLD_R(ir_idx)	= AT_Tbl_Idx;
         IR_LINE_NUM_R(ir_idx)	= IR_LINE_NUM(ir_idx);
         IR_COL_NUM_R(ir_idx)	= IR_COL_NUM(ir_idx);

         if (SCP_ENTRY_IDX(curr_scp_idx)) {
            idx	 = SCP_ENTRY_IDX(curr_scp_idx);
            size = stor_bit_size_of(rslt_idx, TRUE, FALSE);

            /* KAY - Disallowing n$pes in alternate entry function results */

            while (idx != NULL_IDX) {
               new_size	= stor_bit_size_of(ATP_RSLT_IDX(AL_ATTR_IDX(idx)),
                                           TRUE, 
                                           FALSE);
               
               size_offset_logical_calc(&new_size, &size, Gt_Opr, &result);

               if (THIS_IS_TRUE(result.constant, result.type_idx)) {
                  size		= new_size;
                  rslt_idx	= ATP_RSLT_IDX(AL_ATTR_IDX(idx));
               }
               idx		= AL_NEXT_IDX(idx);
            }
         }
         IR_IDX_R(ir_idx)	= rslt_idx;
# endif
      }
      else {

         /* Fill in the Return_Opr so that PDGCS can check  */
         /* to make sure the function result is defined.    */

         IR_FLD_R(ir_idx)       = AT_Tbl_Idx;
         IR_IDX_R(ir_idx)       = rslt_idx;
         IR_LINE_NUM_R(ir_idx)  = IR_LINE_NUM(ir_idx);
         IR_COL_NUM_R(ir_idx)   = IR_COL_NUM(ir_idx);
      }
   }
   else if (ATP_PGM_UNIT(SCP_ATTR_IDX(curr_scp_idx)) == Subroutine &&
            ATP_HAS_ALT_RETURN(SCP_ATTR_IDX(curr_scp_idx))         &&
            IR_FLD_L(ir_idx) == NO_Tbl_Idx) {
      /* if no alt return spec was specified, supply zero */
      IR_FLD_L(ir_idx) = CN_Tbl_Idx;
      IR_IDX_L(ir_idx) = CN_INTEGER_ZERO_IDX;
      IR_LINE_NUM_L(ir_idx)  = IR_LINE_NUM(ir_idx);
      IR_COL_NUM_L(ir_idx)   = IR_COL_NUM(ir_idx);
   }

   ptr = SCP_EXIT_IR_SH_IDX(curr_scp_idx);

   if (ptr) {
      while (SH_NEXT_IDX(ptr) != NULL_IDX) {
         ptr = SH_NEXT_IDX(ptr);
      }

      copy_entry_exit_sh_list(SCP_EXIT_IR_SH_IDX(curr_scp_idx), ptr,
                              &new_start_idx, &new_end_idx);

      insert_sh_chain_before(new_start_idx);
   }

   TRACE (Func_Exit, "return_stmt_semantics", NULL);

   return;

}  /* return_stmt_semantics */


/******************************************************************************\
|*									      *|
|* Description:								      *|
|*	Do the semantic processing for the SELECT CASE statement.             *|
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

void select_stmt_semantics (void)

{
   int			column;
   expr_arg_type	expr_desc;
   int			ir_idx;
   int			line;
   opnd_type		l_opnd;
   opnd_type		opnd;
   int			save_curr_stmt_sh_idx;
   int			tmp_idx;
   int			unused_curr_stmt_sh_idx;


   TRACE (Func_Entry, "select_stmt_semantics", NULL);

   ir_idx = IR_IDX_L(SH_IR_IDX(curr_stmt_sh_idx));
   COPY_OPND(opnd, IR_OPND_L(ir_idx));
   expr_desc.rank = 0;
   xref_state     = CIF_Symbol_Reference;

   defer_stmt_expansion = TRUE;
   number_of_functions = 0;

   if (expr_semantics(&opnd, &expr_desc)) {

      /* The case-expr must be type integer, character, or logical.	      */

      if (expr_desc.type != Integer  &&  expr_desc.type != Character  &&
          expr_desc.type != Logical) {
         find_opnd_line_and_column(&opnd, &line, &column);
         PRINTMSG(line, 767, Error, column);
      }
    
      /* The case-expr expression must be scalar.			      */

      if (expr_desc.rank != 0) {
         find_opnd_line_and_column(&opnd, &line, &column);
         PRINTMSG(line, 765, Error, column);
      }

      defer_stmt_expansion = FALSE;

      if (tree_produces_dealloc(&opnd)) {
         /* put expression into temp */

         find_opnd_line_and_column(&opnd, &line, &column);
         save_curr_stmt_sh_idx = curr_stmt_sh_idx;

         /* first, generate an unused sh to expand the function around */
         gen_sh(Before, Assignment_Stmt, line,
                column, FALSE, FALSE, TRUE);

         curr_stmt_sh_idx = SH_PREV_IDX(curr_stmt_sh_idx);
         unused_curr_stmt_sh_idx = curr_stmt_sh_idx;

         process_deferred_functions(&opnd);

         tmp_idx = create_tmp_asg(&opnd,
                                  &expr_desc,
                                  &l_opnd,
                                  Intent_In,
                                  FALSE,
                                  TRUE);

         COPY_OPND(opnd, l_opnd);

         /* remove the unused sh */
         remove_sh(unused_curr_stmt_sh_idx);
         FREE_SH_NODE(unused_curr_stmt_sh_idx);

         if (where_dealloc_stmt_idx != NULL_IDX) {
# ifdef _DEBUG
            if (IL_FLD(IL_NEXT_LIST_IDX(IR_IDX_R(ir_idx))) != AT_Tbl_Idx  ||
                AT_OBJ_CLASS(IL_IDX(IL_NEXT_LIST_IDX(IR_IDX_R(ir_idx)))) !=
                                                        Label) {

               PRINTMSG(line, 626, Internal, column,
                        "label", "select_stmt_semantics");
            }
# endif
             
            curr_stmt_sh_idx = ATL_DEF_STMT_IDX(IL_IDX(
                                    IL_NEXT_LIST_IDX(IR_IDX_R(ir_idx))));

            while (SH_STMT_TYPE(curr_stmt_sh_idx) != End_Select_Stmt) {
# ifdef _DEBUG
               if (curr_stmt_sh_idx == NULL_IDX) {
                  PRINTMSG(line, 626, Internal, column,
                           "End_Select_Stmt", "select_stmt_semantics");
               }
# endif
               curr_stmt_sh_idx = SH_NEXT_IDX(curr_stmt_sh_idx);
            }

            /* insert the where_dealloc_stmt_idx after End_Select_Stmt */
            insert_sh_chain(where_dealloc_stmt_idx,
                            where_dealloc_stmt_idx,
                            After);

            where_dealloc_stmt_idx = NULL_IDX;
         }

         curr_stmt_sh_idx           = save_curr_stmt_sh_idx;
      }
      else {

         process_deferred_functions(&opnd);

         if (expr_desc.type == Character) {
            validate_char_len(&opnd, &expr_desc);
         }
      }
   }
 
   if (! SH_ERR_FLG(curr_stmt_sh_idx)) {
      COPY_OPND(IR_OPND_L(ir_idx), opnd);
      IR_TYPE_IDX(ir_idx) = expr_desc.type_idx;
   }
         
   defer_stmt_expansion = FALSE;
   arg_info_list_base   = NULL_IDX;
   arg_info_list_top    = NULL_IDX;

   TRACE (Func_Exit, "select_stmt_semantics", NULL);

   return;

}  /* select_stmt_semantics */


/******************************************************************************\
|*									      *|
|* Description:								      *|
|*	Do all semantic processing for STOP/PAUSE.  A Return_Opr will be      *|
|*      generated following the call to $STOP.  Calls will be generated in    *|
|*      the IR to $STOP/$PAUSE.                                               *|
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

void stop_pause_stmt_semantics (void)

{
   int			attr_idx;
   expr_arg_type	exp_desc;
   int			ir_idx;
   boolean		is_call;
   int			list_idx;
   opnd_type		opnd;
   int			save_arg_info_list_base;
   boolean		semantically_correct		= TRUE; 
   char			str[16];
   int			type_idx;


   TRACE (Func_Entry, "stop_pause_stmt_semantics", NULL);

   /* do memory management stuff to make sure the call tables are big enough */

   if (max_call_list_size >= arg_list_size) {
      enlarge_call_list_tables();
   }

   save_arg_info_list_base = arg_info_list_base;
   arg_info_list_base      = arg_info_list_top;
   arg_info_list_top       = arg_info_list_base + 1;

   if (arg_info_list_top >= arg_info_list_size) {
      enlarge_info_list_table();
   }

   ir_idx = SH_IR_IDX(curr_stmt_sh_idx);

   if (IR_OPR(ir_idx) == Pause_Opr) {

      if (glb_tbl_idx[Pause_Attr_Idx] == NULL_IDX) {
         glb_tbl_idx[Pause_Attr_Idx] = create_lib_entry_attr(PAUSE_LIB_ENTRY,
                                                            PAUSE_NAME_LEN,
                                                            IR_LINE_NUM(ir_idx),
                                                            IR_COL_NUM(ir_idx));
      }

      attr_idx	= glb_tbl_idx[Pause_Attr_Idx];

      ADD_ATTR_TO_LOCAL_LIST(attr_idx);
      
      NTR_IR_LIST_TBL(list_idx);
      IL_ARG_DESC_VARIANT(list_idx)= TRUE;
      IL_FLD(list_idx)		= IR_FLD_L(ir_idx);
      IL_IDX(list_idx)		= IR_IDX_L(ir_idx);
      IL_COL_NUM(list_idx)	= IR_COL_NUM(ir_idx);
      IL_LINE_NUM(list_idx) 	= IR_LINE_NUM(ir_idx);	

      IR_FLD_R(ir_idx)		= IL_Tbl_Idx;
      IR_IDX_R(ir_idx)		= list_idx;
      IR_LIST_CNT_R(ir_idx)	= 1;

      is_call = TRUE;
   }
   else {

      if (glb_tbl_idx[Stop_Attr_Idx] == NULL_IDX) {
# ifdef _TARGET_OS_MAX
         if (cmd_line_flags.co_array_fortran) {
            glb_tbl_idx[Stop_Attr_Idx] = create_lib_entry_attr(
                                                           STOP_ALL_LIB_ENTRY,
                                                           STOP_ALL_NAME_LEN,
                                                           IR_LINE_NUM(ir_idx),
                                                           IR_COL_NUM(ir_idx));
         }
         else {
            glb_tbl_idx[Stop_Attr_Idx] = create_lib_entry_attr(STOP_LIB_ENTRY,
                                                               STOP_NAME_LEN,
                                                           IR_LINE_NUM(ir_idx),
                                                           IR_COL_NUM(ir_idx));
         }
# else
         glb_tbl_idx[Stop_Attr_Idx] = create_lib_entry_attr(STOP_LIB_ENTRY,
                                                            STOP_NAME_LEN,
                                                        IR_LINE_NUM(ir_idx),
                                                        IR_COL_NUM(ir_idx));
# endif
         ATP_NOSIDE_EFFECTS(glb_tbl_idx[Stop_Attr_Idx])     = TRUE;
         ATP_DOES_NOT_RETURN(glb_tbl_idx[Stop_Attr_Idx])     = TRUE;
      }

      attr_idx	= glb_tbl_idx[Stop_Attr_Idx];

# ifdef _STOP_IS_OPR
      is_call = FALSE;
# else
      ADD_ATTR_TO_LOCAL_LIST(attr_idx);
      is_call = TRUE;
# endif
      
      NTR_IR_LIST_TBL(list_idx);
      IL_ARG_DESC_VARIANT(list_idx)= TRUE;
      IL_FLD(list_idx)		= IR_FLD_L(ir_idx);
      IL_IDX(list_idx)		= IR_IDX_L(ir_idx);
      IL_COL_NUM(list_idx)	= IR_COL_NUM(ir_idx);
      IL_LINE_NUM(list_idx) 	= IR_LINE_NUM(ir_idx);	

      IR_FLD_R(ir_idx)		= IL_Tbl_Idx;
      IR_IDX_R(ir_idx)		= list_idx;
      IR_LIST_CNT_R(ir_idx)	= 1;

   }

   /* If stop_code exits. */

   if (IL_FLD(list_idx) != NO_Tbl_Idx) {

      switch (IL_FLD(list_idx)) {

      case AT_Tbl_Idx :                 /* we have a stand alone identifier */
         COPY_OPND(opnd, IL_OPND(list_idx));
         exp_desc.rank		= 0;
         xref_state		= CIF_Symbol_Reference;
         semantically_correct	= expr_semantics(&opnd,
                                                 &exp_desc);
         COPY_OPND(IL_OPND(list_idx), opnd);

         arg_info_list[arg_info_list_base + 1]			= init_arg_info;
         arg_info_list[arg_info_list_base + 1].ed		= exp_desc;
         arg_info_list[arg_info_list_base + 1].maybe_modified	= FALSE;

         if (!AT_DCL_ERR(IR_IDX_L(ir_idx))) {

            if (exp_desc.type != Character || exp_desc.rank != 0) {
               PRINTMSG(IR_LINE_NUM(ir_idx), 386, Error, IR_COL_NUM(ir_idx));
               semantically_correct = FALSE;
            }
            else if (! exp_desc.constant) {
               PRINTMSG(IR_LINE_NUM(ir_idx), 385, Ansi, IR_COL_NUM(ir_idx));
            }
         }
         break;


      case CN_Tbl_Idx :  /* we have a scalar constant */

         if (TYP_TYPE(CN_TYPE_IDX(IL_IDX(list_idx))) != Integer &&
             TYP_TYPE(CN_TYPE_IDX(IL_IDX(list_idx))) != Character) {
           PRINTMSG(IR_LINE_NUM(ir_idx), 386, Error, IR_COL_NUM(ir_idx));
           semantically_correct = FALSE;
         }

         if (TYP_TYPE(CN_TYPE_IDX(IL_IDX(list_idx))) == Integer) {

            if (compare_cn_and_value(IL_IDX(list_idx), 0, Lt_Opr) ||
                compare_cn_and_value(IL_IDX(list_idx), 99999, Gt_Opr)) {

               PRINTMSG(IR_LINE_NUM(ir_idx), 385, Ansi, IR_COL_NUM(ir_idx));
            }
       
            /* Convert the integer value to a character constant. */ 

            convert_to_string(&CN_CONST(IL_IDX(list_idx)),
                               CN_TYPE_IDX(IL_IDX(list_idx)),
                               str);

            CLEAR_TBL_NTRY(type_tbl, TYP_WORK_IDX);

            TYP_TYPE(TYP_WORK_IDX)	= Character;
            TYP_LINEAR(TYP_WORK_IDX)	= CHARACTER_DEFAULT_TYPE;
            TYP_CHAR_CLASS(TYP_WORK_IDX)= Const_Len_Char;
            TYP_FLD(TYP_WORK_IDX)	= CN_Tbl_Idx;
            TYP_IDX(TYP_WORK_IDX)	= C_INT_TO_CN(CG_INTEGER_DEFAULT_TYPE,
                                                      strlen(str));
            type_idx			= ntr_type_tbl();
            IL_IDX(list_idx)		= ntr_const_tbl(type_idx, 
                                                        TRUE,
                                                        (long_type *) str);
         }

         arg_info_list[arg_info_list_base + 1]			= init_arg_info;
         arg_info_list[arg_info_list_base + 1].ed.type_idx	= 
                                               CN_TYPE_IDX(IL_IDX(list_idx)); 
         arg_info_list[arg_info_list_base + 1].ed.type		= Character;
         arg_info_list[arg_info_list_base + 1].ed.linear_type	= Character_1;
         arg_info_list[arg_info_list_base + 1].ed.char_len.fld  = 
                    TYP_FLD(CN_TYPE_IDX(IL_IDX(list_idx)));
         arg_info_list[arg_info_list_base + 1].ed.char_len.idx  = 
                    TYP_IDX(CN_TYPE_IDX(IL_IDX(list_idx)));
         arg_info_list[arg_info_list_base + 1].ed.constant	= TRUE;
         arg_info_list[arg_info_list_base + 1].maybe_modified	= FALSE;
         break;


      case IR_Tbl_Idx :  /* we have an expression tree */
         COPY_OPND(opnd, IL_OPND(list_idx));
         exp_desc.rank		= 0;
         xref_state		= CIF_Symbol_Reference;
         semantically_correct	= expr_semantics(&opnd,
                                                 &exp_desc);
         COPY_OPND(IL_OPND(list_idx), opnd);

         if (semantically_correct) {

            if (exp_desc.rank != 0 || exp_desc.type != Character) {
               PRINTMSG(IR_LINE_NUM(ir_idx), 386, Error, IR_COL_NUM(ir_idx));
               semantically_correct = FALSE;
            }
            else if (exp_desc.type == Character) {
               PRINTMSG(IR_LINE_NUM(ir_idx), 385, Ansi, IR_COL_NUM(ir_idx));
            }
         }

         arg_info_list[arg_info_list_base + 1]			= init_arg_info;
         arg_info_list[arg_info_list_base + 1].ed		= exp_desc;
         arg_info_list[arg_info_list_base + 1].maybe_modified	= FALSE;
         break;


      default :
         PRINTMSG(IR_LINE_NUM(ir_idx), 386, Error, IR_COL_NUM(ir_idx));
         semantically_correct = FALSE;
         break;
      }
   }
   else {  /* no stop code exits - pass a blank */
      
# if (defined(_TARGET_OS_IRIX) || defined(_TARGET_OS_LINUX) || defined(_TARGET_OS_DARWIN))
      /* send a zero length string on irix */

      CLEAR_TBL_NTRY(type_tbl, TYP_WORK_IDX);
      TYP_TYPE(TYP_WORK_IDX)    = Character;
      TYP_LINEAR(TYP_WORK_IDX)  = CHARACTER_DEFAULT_TYPE;
      TYP_DESC(TYP_WORK_IDX)    = Default_Typed;
      TYP_CHAR_CLASS(TYP_WORK_IDX)= Const_Len_Char;
      TYP_FLD(TYP_WORK_IDX)     = CN_Tbl_Idx;
      TYP_IDX(TYP_WORK_IDX)     = CN_INTEGER_ZERO_IDX;
      type_idx                  = ntr_type_tbl();

      IL_FLD(list_idx)                  = CN_Tbl_Idx;
      IL_IDX(list_idx)                  = ntr_const_tbl(type_idx,
                                                        FALSE,
                                                        NULL);
      IL_LINE_NUM(list_idx) = IR_LINE_NUM(ir_idx);
      IL_COL_NUM(list_idx) = IR_COL_NUM(ir_idx);

      arg_info_list[arg_info_list_base + 1]                 = init_arg_info;
      arg_info_list[arg_info_list_base + 1].ed.type_idx     = type_idx;
      arg_info_list[arg_info_list_base + 1].ed.type         = Character;
      arg_info_list[arg_info_list_base + 1].ed.char_len.fld =
                    TYP_FLD(CN_TYPE_IDX(IL_IDX(list_idx)));
      arg_info_list[arg_info_list_base + 1].ed.char_len.idx =
                    TYP_IDX(CN_TYPE_IDX(IL_IDX(list_idx)));
      arg_info_list[arg_info_list_base + 1].ed.linear_type  = Character_1;
      arg_info_list[arg_info_list_base + 1].ed.constant     = TRUE;
      arg_info_list[arg_info_list_base + 1].maybe_modified  = FALSE;

# else
      str[0] = ' ';
      str[1] = '\0';

      IL_FLD(list_idx)			= CN_Tbl_Idx;
      IL_IDX(list_idx)			= ntr_const_tbl(CHARACTER_DEFAULT_TYPE,
                                                        FALSE,
                                                        (long_type *) str);
      IL_LINE_NUM(list_idx) = IR_LINE_NUM(ir_idx);
      IL_COL_NUM(list_idx) = IR_COL_NUM(ir_idx);

      arg_info_list[arg_info_list_base + 1]			= init_arg_info;
      arg_info_list[arg_info_list_base + 1].ed.type_idx= CHARACTER_DEFAULT_TYPE;
      arg_info_list[arg_info_list_base + 1].ed.type		= Character;
      arg_info_list[arg_info_list_base + 1].ed.char_len.fld  = 
                    TYP_FLD(CN_TYPE_IDX(IL_IDX(list_idx)));
      arg_info_list[arg_info_list_base + 1].ed.char_len.idx  = 
                    TYP_IDX(CN_TYPE_IDX(IL_IDX(list_idx)));
      arg_info_list[arg_info_list_base + 1].ed.linear_type	= Character_1;
      arg_info_list[arg_info_list_base + 1].ed.constant		= TRUE;
      arg_info_list[arg_info_list_base + 1].maybe_modified	= FALSE;
# endif
   }

   IR_TYPE_IDX(ir_idx) = TYPELESS_DEFAULT_TYPE;

   if (is_call) {
      IR_FLD_L(ir_idx) = AT_Tbl_Idx;
      IR_IDX_L(ir_idx) = attr_idx;
      IR_LINE_NUM_L(ir_idx) = IR_LINE_NUM(ir_idx);
      IR_COL_NUM_L(ir_idx) = IR_COL_NUM(ir_idx);
      IR_OPR(ir_idx)   = Call_Opr;
   }

   if (semantically_correct) {
      arg_list[1]		= list_idx;
      IL_ARG_DESC_IDX(list_idx) = arg_info_list_base + 1;

      COPY_OPND(opnd, IR_OPND_R(ir_idx));
      semantically_correct = final_arg_work(&opnd, attr_idx, 1, NULL) &&
                             semantically_correct;
      COPY_OPND(IR_OPND_R(ir_idx), opnd);
   }

   /* restore arg_info_list to previous "stack frame" */

   arg_info_list_top  = arg_info_list_base;
   arg_info_list_base = save_arg_info_list_base;

   TRACE (Func_Exit, "stop_pause_stmt_semantics", NULL);

   return;

}  /* stop_pause_stmt_semantics */


/******************************************************************************\
|*									      *|
|* Description:								      *|
|*	This function removes the Then_Stmt SH because it was only needed by  *|
|*      the Syntax Pass and the PDGCS interface doesn't want to see it.       *|
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

void then_stmt_semantics (void)

{
   int	then_idx;


   TRACE (Func_Entry, "then_stmt_semantics", NULL);

   then_idx                           = curr_stmt_sh_idx;
   curr_stmt_sh_idx                   = SH_PREV_IDX(then_idx);
   remove_sh(then_idx);
   FREE_SH_NODE(then_idx);

   TRACE (Func_Exit, "then_stmt_semantics", NULL);

   return;

}  /* then_stmt_semantics */


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

void where_stmt_semantics (void)

{
   int			and_idx;
   int			col;
   boolean		clear_alloc_block = FALSE;
   expr_arg_type	exp_desc;
   int			ir_idx;
   int			line;
   int			list_idx;
   opnd_type		mask_expr_opnd;
   int			mask_expr_tmp;
   boolean		ok		= TRUE;
   opnd_type		opnd;
   int			save_active_forall_sh_idx;
   int			save_where_ir_idx;
   int			sh_idx;


   TRACE (Func_Entry, "where_stmt_semantics", NULL);

   ir_idx		= SH_IR_IDX(curr_stmt_sh_idx);

   if (active_forall_sh_idx) {

      if (IR_OPR(ir_idx) == Where_Cnstrct_Opr) {
         gen_forall_loops(curr_stmt_sh_idx,
                          IR_IDX_R(ir_idx));
         gen_forall_if_mask(curr_stmt_sh_idx,
                            IR_IDX_R(ir_idx));

         SH_PARENT_BLK_IDX(curr_stmt_sh_idx) = active_forall_sh_idx;
         active_forall_sh_idx = NULL_IDX;
      }
      else {
         /* WHERE stmt. */
         gen_forall_loops(curr_stmt_sh_idx, curr_stmt_sh_idx);
         gen_forall_if_mask(curr_stmt_sh_idx, curr_stmt_sh_idx);
      }
   }

   exp_desc.rank	= 0;
   xref_state		= CIF_Symbol_Reference;

   COPY_OPND(opnd, IR_OPND_L(ir_idx));

   ok = expr_semantics(&opnd, &exp_desc);

   find_opnd_line_and_column(&opnd, &line, &col);

   if (exp_desc.type != Logical) {
      PRINTMSG(line, 120, Error, col);
      ok = FALSE;
   }
   else if (exp_desc.rank == 0) {
      PRINTMSG(line, 181, Error, col);
      ok = FALSE;
   }

   if (where_ir_idx > 0) {
      /* check conformance */

      if (! check_where_conformance(&exp_desc)) {
         PRINTMSG(line, 1610, Error, col);
         ok = FALSE;
      }
   }

   if (!ok) {
      if (stmt_type != Where_Stmt) {
         where_ir_idx = -1;
      }
      goto EXIT;
   }

   if (SH_PARENT_BLK_IDX(curr_stmt_sh_idx) == NULL_IDX ||
       (SH_STMT_TYPE(SH_PARENT_BLK_IDX(curr_stmt_sh_idx))!=Where_Cstrct_Stmt &&
        SH_STMT_TYPE(SH_PARENT_BLK_IDX(curr_stmt_sh_idx)) != Else_Where_Stmt &&
        SH_STMT_TYPE(SH_PARENT_BLK_IDX(curr_stmt_sh_idx)) != 
                                                  Else_Where_Mask_Stmt)) {

      /* this is the outer WHERE construct */
# ifdef _DEBUG
      if (alloc_block_start_idx != NULL_IDX ||
          alloc_block_end_idx != NULL_IDX) {
         PRINTMSG(line, 626, Internal, col,
                  "alloc_block_start_idx == NULL_IDX",
                  "where_stmt_semantics");
      }
# endif

      if (stmt_type != Where_Stmt)  {

         if (IR_FLD_R(ir_idx) == SH_Tbl_Idx &&
             ! SH_ERR_FLG(IR_IDX_R(ir_idx))) {

            alloc_block_start_idx = curr_stmt_sh_idx;
            alloc_block_end_idx = IR_IDX_R(ir_idx);
         }
      }
      else {
         alloc_block_start_idx = curr_stmt_sh_idx;
         alloc_block_end_idx = curr_stmt_sh_idx;
         clear_alloc_block = TRUE;
      }
   }

   if (stmt_type == Where_Stmt) {

      save_active_forall_sh_idx = active_forall_sh_idx;
      active_forall_sh_idx = NULL_IDX;

      save_where_ir_idx = where_ir_idx;

      mask_expr_tmp = create_tmp_asg(&opnd, &exp_desc, &mask_expr_opnd,
                                     Intent_In, FALSE, TRUE);

      if (where_ir_idx > 0) {
         and_idx = gen_ir(IR_Tbl_Idx, where_ir_idx,
                     And_Opr, exp_desc.type_idx, line, col,
                          OPND_FLD(mask_expr_opnd), OPND_IDX(mask_expr_opnd));

         gen_opnd(&opnd, and_idx, IR_Tbl_Idx, line, col);
      }
      else {
         COPY_OPND(opnd, mask_expr_opnd);
      }

      /* Check the next statement.  If it is a statement number statement */
      /* use it to set statement_number so that assignment statement gens */
      /* the correct statement number for CIF.  Remove the statement.     */

      if (SH_NEXT_IDX(curr_stmt_sh_idx) != NULL_IDX &&
          SH_STMT_TYPE(SH_NEXT_IDX(curr_stmt_sh_idx)) == Statement_Num_Stmt &&
          SH_IR_IDX(SH_NEXT_IDX(curr_stmt_sh_idx)) == NULL_IDX) {
         sh_idx					= SH_NEXT_IDX(curr_stmt_sh_idx);
         stmt_end_line				= SH_GLB_LINE(sh_idx);
         stmt_end_col				= SH_COL_NUM(sh_idx);
         statement_number			= SH_PARENT_BLK_IDX(sh_idx);
         SH_NEXT_IDX(curr_stmt_sh_idx)		= SH_NEXT_IDX(sh_idx);
         SH_PREV_IDX(SH_NEXT_IDX(sh_idx))	= curr_stmt_sh_idx;
         FREE_SH_NODE(sh_idx);
      }

      where_ir_idx  = OPND_IDX(opnd);

      /* need to remove the where operator and make this look */
      /* like assignment.                                     */

      SH_STMT_TYPE(curr_stmt_sh_idx) = Assignment_Stmt;
      stmt_type                      = Assignment_Stmt;

      find_opnd_line_and_column((opnd_type *) &IR_OPND_R(ir_idx), 
                                &stmt_start_line, 
                                &stmt_start_col);

      SH_IR_IDX(curr_stmt_sh_idx)    = IR_IDX_R(ir_idx);

      (*stmt_semantics[stmt_type])();

      if (clear_alloc_block) {
         alloc_block_start_idx = NULL_IDX;
         alloc_block_end_idx = NULL_IDX;
      }

      where_ir_idx = save_where_ir_idx;

      active_forall_sh_idx = save_active_forall_sh_idx;
   }
   else {

      /* set up control mask */

      mask_expr_tmp = create_tmp_asg(&opnd, &exp_desc, &mask_expr_opnd, 
                                     Intent_In, FALSE, TRUE);

      if (where_ir_idx > 0) {
         and_idx = gen_ir(IR_Tbl_Idx, where_ir_idx,
                      And_Opr, exp_desc.type_idx, line, col,
                          OPND_FLD(mask_expr_opnd), OPND_IDX(mask_expr_opnd));

         gen_opnd(&opnd, and_idx, IR_Tbl_Idx, line, col);
      }
      else {
         COPY_OPND(opnd, mask_expr_opnd);
      }

      NTR_IR_LIST_TBL(list_idx);
      IR_FLD_L(ir_idx) = IL_Tbl_Idx;
      IR_IDX_L(ir_idx) = list_idx;
      IR_LIST_CNT_L(ir_idx) = 2;

      COPY_OPND(IL_OPND(list_idx), opnd);

      /* set up pending mask */

      gen_opnd(&opnd, 
               gen_ir(OPND_FLD(mask_expr_opnd), OPND_IDX(mask_expr_opnd),
                  Not_Opr, exp_desc.type_idx, line, col,
                      NO_Tbl_Idx, NULL_IDX),
               IR_Tbl_Idx,
               line,
               col);

      if (where_ir_idx > 0) {
         and_idx = gen_ir(IR_Tbl_Idx, where_ir_idx,
                      And_Opr, exp_desc.type_idx, line, col,
                          OPND_FLD(opnd), OPND_IDX(opnd));

         gen_opnd(&opnd, and_idx, IR_Tbl_Idx, line, col);
      }

      /* do not change where_ir_idx until the pending mask tree is created */

      where_ir_idx  = IL_IDX(list_idx);

      NTR_IR_LIST_TBL(IL_NEXT_LIST_IDX(list_idx));
      IL_PREV_LIST_IDX(IL_NEXT_LIST_IDX(list_idx)) = list_idx;
      list_idx = IL_NEXT_LIST_IDX(list_idx);

      COPY_OPND(IL_OPND(list_idx), opnd);
   }

EXIT:
   
   TRACE (Func_Exit, "where_stmt_semantics", NULL);
   
   return;

}  /* where_stmt_semantics */


/******************************************************************************\
|*									      *|
|* Description:								      *|
|*	This procedure should be called by the semantics routine for any      *|
|*      statement that makes an unconditional branch (such as an              *|
|*      unconditional GO TO, an arithmetic IF, etc.) or that stops program    *|
|*      execution (such as the STOP statement).  It issues a warning message  *|
|*      (on the following statement) if the following statement is not        *|
|*      labeled (control can not reach it).				      *|
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

static void chk_for_unlabeled_stmt (void)

{
   int		sh_idx;
 

   TRACE (Func_Entry, "chk_for_unlabeled_stmt", NULL);

   /* Do not issue the message if the unconditional branching stmt is the     */
   /* action-stmt of a logical IF.                                            */


   if (! SH_ACTION_STMT(curr_stmt_sh_idx)) {
      sh_idx = SH_NEXT_IDX(curr_stmt_sh_idx);

      while (SH_COMPILER_GEN(sh_idx)) {
         sh_idx = SH_NEXT_IDX(sh_idx);
      }
             
      if (SH_STMT_TYPE(sh_idx) != Label_Def) {

         switch (SH_STMT_TYPE(sh_idx))
         {
            case Null_Stmt:
            case Contains_Stmt:
            case Data_Stmt:
            case Directive_Stmt:
	    case End_Do_Stmt:
            case End_Function_Stmt:
            case End_If_Stmt:
            case End_Program_Stmt:
            case End_Select_Stmt:
            case End_Stmt:
            case End_Subroutine_Stmt:
            case Case_Stmt:
            case Else_Stmt:
            case Else_If_Stmt:
            case Entry_Stmt:
	    case End_Parallel_Stmt:
	    case End_Do_Parallel_Stmt:
	    case End_Parallel_Case_Stmt:
            case Parallel_Case_Stmt:
            case End_Guard_Stmt:
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

               break;

            default:
               PRINTMSG(SH_GLB_LINE(sh_idx), 362, Warning, SH_COL_NUM(sh_idx));
         }
      }
   }

   TRACE (Func_Exit, "chk_for_unlabeled_stmt", NULL);

   return;

}  /* chk_for_unlabeled_stmt */


/******************************************************************************\
|*									      *|
|* Description:								      *|
|*	This procedure is called by case_stmt_semantics to perform semantic   *|
|*      analysis on a case-value-range.					      *|
|*									      *|
|* Input parameters:							      *|
|*	ir_idx        : the index of the Case_Range IR			      *|
|*      new_il_idx    : the index of the new IL to be added to the list;      *|
|*                      points at the Case_Range IR			      *|
|*      select_ir_idx : the index of the dummy Select IR		      *|
|*									      *|
|* Output parameters:							      *|
|*	NONE								      *|
|*									      *|
|* Returns:								      *|
|*	NONE								      *|
|*									      *|
\******************************************************************************/

static void case_value_range_semantics(int	ir_idx,
				       int	new_il_idx,
				       int	select_ir_idx)

{
   int			column;
   int			curr_il_idx;
   int			curr_range_ir_idx;
   expr_arg_type	expr_desc;
   opnd_type		opnd; 
   int			line;
 

   TRACE (Func_Entry, "case_value_range_semantics", NULL);

   COPY_OPND(opnd, IR_OPND_L(ir_idx));
   expr_desc.rank = 0;

   switch (IR_FLD_L(ir_idx)) {

      case NO_Tbl_Idx:
         break;

      case CN_Tbl_Idx:
         expr_desc.type_idx	= CN_TYPE_IDX(IR_IDX_L(ir_idx));
         expr_desc.type		= TYP_TYPE(expr_desc.type_idx);
         expr_desc.linear_type	= TYP_LINEAR(expr_desc.type_idx);
         break;

      case AT_Tbl_Idx:

      case IR_Tbl_Idx:
         xref_state = CIF_Symbol_Reference;

         if (expr_semantics(&opnd, &expr_desc)) {
                 
            if (expr_desc.constant) {
               COPY_OPND(IR_OPND_L(ir_idx), opnd);
            }
            else {

               /* Did not resolve to a named constant.			      */

               PRINTMSG(IR_LINE_NUM_L(ir_idx), 811, Error,
                        IR_COL_NUM_L(ir_idx));
               IR_OPND_L(ir_idx) = null_opnd;
            }
         }
         else {
            IR_OPND_L(ir_idx) = null_opnd;
         }

         break;

# ifdef _DEBUG
      default:
         PRINTMSG(SH_GLB_LINE(curr_stmt_sh_idx), 179, Internal, 
                  SH_COL_NUM(curr_stmt_sh_idx), "case_value_range_semantics");
# endif

   }  
                    
   if (IR_FLD_L(ir_idx) != NO_Tbl_Idx) {
      find_opnd_line_and_column(&opnd, &line, &column);
      
      /* The case-value expression must be scalar.			      */

      if (expr_desc.rank != 0) {
         PRINTMSG(line, 766, Error, column);
      }
         
      /* The case-value must be type integer or character.		      */

      if (expr_desc.type == Integer  ||  expr_desc.type == Character) {

         /* If the SELECT CASE stmt is OK, verify that the type of the        */
         /* case-value is the same as the SELECT CASE expression.             */

         if (! SH_ERR_FLG(SH_PARENT_BLK_IDX(curr_stmt_sh_idx))  &&
             expr_desc.type != TYP_TYPE(IR_TYPE_IDX(IR_IDX_L(select_ir_idx)))) {
            PRINTMSG(line, 745, Error, column);
         }

      }
      else if (expr_desc.type == Typeless  &&
               CN_BOZ_CONSTANT(OPND_IDX(opnd))) {

         /* Extension:  We'll also allow a BOZ constant (but NOT the X,       */
         /* trailing B, Hollerith, or character used as Hollerith forms) to   */
         /* match an integer SELECT CASE expression.                          */
   
         if (! SH_ERR_FLG(SH_PARENT_BLK_IDX(curr_stmt_sh_idx))  &&
             TYP_TYPE(IR_TYPE_IDX(IR_IDX_L(select_ir_idx))) != Integer) {
            PRINTMSG(line, 745, Error, column);
         }
         else if (expr_desc.linear_type == Short_Typeless_Const) {
            IR_IDX_L(ir_idx) = cast_typeless_constant(IR_IDX_L(ir_idx),
                                                      INTEGER_DEFAULT_TYPE,
                                                      line,
                                                      column);
            expr_desc.linear_type = INTEGER_DEFAULT_TYPE;
            expr_desc.type_idx    = INTEGER_DEFAULT_TYPE;
            expr_desc.type        = Integer;
         }
      }
      else {
         PRINTMSG(line, 768, Error, column);
      }
   }

   COPY_OPND(opnd, IR_OPND_R(ir_idx));
   expr_desc.rank = 0;

   switch (IR_FLD_R(ir_idx)) {

      case NO_Tbl_Idx:
         break;

      case CN_Tbl_Idx:
         expr_desc.type_idx	= CN_TYPE_IDX(IR_IDX_R(ir_idx));
         expr_desc.type		= TYP_TYPE(expr_desc.type_idx);
         expr_desc.linear_type	= TYP_LINEAR(expr_desc.type_idx);
         break;

      case AT_Tbl_Idx:

      case IR_Tbl_Idx:
         xref_state = CIF_Symbol_Reference;

         if (expr_semantics(&opnd, &expr_desc)) {
               
            if (expr_desc.constant) {
               COPY_OPND(IR_OPND_R(ir_idx), opnd);
            }
            else {

               /* Did not resolve to a named constant.			      */

               PRINTMSG(IR_LINE_NUM_R(ir_idx), 811, Error,
                        IR_COL_NUM_R(ir_idx));
               IR_OPND_R(ir_idx) = null_opnd;
            }
         }
         else {
            IR_OPND_R(ir_idx) = null_opnd;
         }

         break;

# ifdef _DEBUG
      default:
         PRINTMSG(SH_GLB_LINE(curr_stmt_sh_idx), 179, Internal, 
                  SH_COL_NUM(curr_stmt_sh_idx), "case_value_range_semantics");
# endif

   }  

   if (IR_FLD_R(ir_idx) != NO_Tbl_Idx) {
      find_opnd_line_and_column(&opnd, &line, &column);

      /* The case-value expression must be scalar.			      */

      if (expr_desc.rank != 0) {
         PRINTMSG(line, 766, Error, column);
      }
         
      /* The case-value must be type integer or character.		      */

      if (expr_desc.type == Integer  ||  expr_desc.type == Character) {

         /* If the SELECT CASE stmt is OK, verify that the type of the        */
         /* case-value is the same as the SELECT CASE expression.             */

         if (! SH_ERR_FLG(SH_PARENT_BLK_IDX(curr_stmt_sh_idx))  &&
             expr_desc.type != TYP_TYPE(IR_TYPE_IDX(IR_IDX_L(select_ir_idx)))) {
            PRINTMSG(line, 745, Error, column);
         }

      }
      else if (expr_desc.type == Typeless  &&
               CN_BOZ_CONSTANT(OPND_IDX(opnd))) {

         /* Extension:  We'll also allow a BOZ constant (but NOT the X,       */
         /* trailing B, Hollerith, or character used as Hollerith forms) to   */
         /* match an integer SELECT CASE expression.                          */
   
         if (! SH_ERR_FLG(SH_PARENT_BLK_IDX(curr_stmt_sh_idx))  &&
             TYP_TYPE(IR_TYPE_IDX(IR_IDX_L(select_ir_idx))) != Integer) {
            PRINTMSG(line, 745, Error, column);
         }
         else if (expr_desc.linear_type == Short_Typeless_Const) {
            IR_IDX_R(ir_idx) = cast_typeless_constant(IR_IDX_R(ir_idx),
                                                      INTEGER_DEFAULT_TYPE,
                                                      line,
                                                      column);
            expr_desc.linear_type = INTEGER_DEFAULT_TYPE;
            expr_desc.type_idx    = INTEGER_DEFAULT_TYPE;
            expr_desc.type        = Integer;
         }
      }
      else {
         PRINTMSG(line, 768, Error, column);
      }

      /* If the range has both a left and right value and the left value is   */
      /* greater than the right value, issue a warning and return.	      */

      if (! SH_ERR_FLG(curr_stmt_sh_idx)  &&
          IR_FLD_L(ir_idx) != NO_Tbl_Idx    &&
          fold_relationals(IR_IDX_L(ir_idx), IR_IDX_R(ir_idx), Gt_Opr)) {
#ifdef KEY /* Bug 2153 and Bug 4947 */
	 /* A "select" with n cases looks like this:
	  * block s: Select_Stmt: Select_Opr
	  *
	  *   Label_Opr <i=0>
	  *   Case_Stmt: Case_Opr, parent block=s
	  *   <various statements>
	  *   Br_Uncond_Opr n (omitted for last case)
	  *
	  *   <repeat this for cases i=1, 2,...,n-1>
	  *
	  *   Label_Opr <n>
	  * End_Select_Stmt, parent block=s
	  *
	  * To discard a particular case, we must omit every statment from
	  * the Label_Opr preceding the Case_Opr up to but not including
	  * the Label_Opr preceding the next CASE or END SELECT whose parent
	  * block matches the current SELECT.
	  */
	 int our_parent_idx = SH_PARENT_BLK_IDX(curr_stmt_sh_idx);
	 int last_live_sh_idx = SH_PREV_IDX(SH_PREV_IDX(curr_stmt_sh_idx));
	 for (int nextcase_sh_idx = SH_NEXT_IDX(curr_stmt_sh_idx);
	   ;
	   nextcase_sh_idx = SH_NEXT_IDX(nextcase_sh_idx)) {
	   if (((SH_STMT_TYPE(nextcase_sh_idx) == Case_Stmt ||
	     SH_STMT_TYPE(nextcase_sh_idx) == End_Select_Stmt)) &&
	     SH_PARENT_BLK_IDX(nextcase_sh_idx) == our_parent_idx) {
	     int next_live_sh_idx = SH_PREV_IDX(nextcase_sh_idx);
	     SH_NEXT_IDX(last_live_sh_idx) = next_live_sh_idx;
	     SH_PREV_IDX(next_live_sh_idx) = last_live_sh_idx;
	     break;
	   }
	 }

#endif /* KEY Bug 2153 and Bug 4947 */
         PRINTMSG(IR_LINE_NUM(ir_idx), 758, Warning, IR_COL_NUM(ir_idx));
         goto EXIT;
      }

   }

   if (SH_ERR_FLG(curr_stmt_sh_idx)) {
      goto EXIT;
   }

   /* If this is the first CASE, just attach the new IL to the dummy Select   */
   /* IR's right operand.						      */

   if (IR_FLD_R(select_ir_idx) == NO_Tbl_Idx) {
      ++IR_LIST_CNT_R(select_ir_idx);
      IR_FLD_R(select_ir_idx) = IL_Tbl_Idx;
      IR_IDX_R(select_ir_idx) = new_il_idx;
      goto EXIT;
   }

   /* See where this case-value range fits in with previous CASEs.	      */

   curr_il_idx = IR_IDX_R(select_ir_idx);

   while (curr_il_idx != NULL_IDX) {

      /* Is there a left value in this new case range?			      */

      if (IR_FLD_L(ir_idx) != NO_Tbl_Idx) {

         /* Yes.  Is there a right value in this new case range?	      */

         if (IR_FLD_R(ir_idx) != NO_Tbl_Idx) {

            /* Yes.  Does the current IL represent a single case-value?	      */

            if (IL_FLD(curr_il_idx) == CN_Tbl_Idx) {

               /* Yes.							      */
               /* Is the current value < new IL left value?		      */
               /*   Y: Is the current IL the last one in the chain?           */
               /*        Y: Append the new IL to the end of the chain.        */
               /*           {Done}					      */
               /*        N: Advance to the next IL in the chain.	      */
               /*   N: Is the current value > new IL right value?	      */
               /*        Y: Insert the new IL ahead of the current IL.	      */
               /*        N: Error; the (new) range contains a value already   */
               /*             specified by a previous single case-value.      */

               if (fold_relationals(IL_IDX(curr_il_idx), IR_IDX_L(ir_idx),
                                    Lt_Opr)) {

                  if (IL_NEXT_LIST_IDX(curr_il_idx) == NULL_IDX) {
                     IL_NEXT_LIST_IDX(curr_il_idx) = new_il_idx;
                     IL_PREV_LIST_IDX(new_il_idx)  = curr_il_idx;
                     ++IR_LIST_CNT_R(select_ir_idx);
                     goto EXIT;
                  }

               }
               else if (fold_relationals(IL_IDX(curr_il_idx), IR_IDX_R(ir_idx),
                                         Gt_Opr)) {
                  insert_on_left(new_il_idx, curr_il_idx, select_ir_idx);
                  goto EXIT;
               }
	       else {
		  PRINTMSG(IR_LINE_NUM(ir_idx), 748, Error,
			   IR_COL_NUM(ir_idx), IL_LINE_NUM(curr_il_idx));
		  goto EXIT;
	       }

            }
            else {

               /* No, the current IL represents a range.		      */
               /* Does the current range have a left value?		      */
               /*   Y: Does the current range have a right value?	      */
               /*        Y: Is the new left value > current left value?       */
               /*             Y: Is the new left value > current right value? */
               /*                  Y: Is the current IL at the end of the     */
               /*                     list?				      */
               /*                       Y: Append the new IL to the list.     */
               /*                          {Done}			      */
               /*                       N: Advance to the next IL in the list.*/
               /*                  N: Error; the ranges overlap.	      */
               /*		      {Quit}				      */
               /*	      N: --|					      */
       	       /*	 N: --|						      */
   	       /*      Is the new right value < current left value?	      */
               /*        Y: Insert the new IL to the left of the current IL.  */
	       /*           {Done}					      */
	       /*        N: Error; the ranges overlap.			      */
	       /*	    {Quit}					      */
               /*   N: Is the new left value > current right value?	      */
               /*        Y: Is the current IL the last one in the list?	      */
               /*             Y: Append the new IL to the end of the list.    */
               /*	      N: Advance to the next IL.		      */
               /*        N: Error; the ranges overlap.			      */
               /*	    {Quit}					      */

               curr_range_ir_idx = IL_IDX(curr_il_idx);

               if (IR_FLD_L(curr_range_ir_idx) != NO_Tbl_Idx) {

                  if (IR_FLD_R(curr_range_ir_idx) != NO_Tbl_Idx) {

                     if (fold_relationals(IR_IDX_L(ir_idx), 
				          IR_IDX_L(curr_range_ir_idx),
					  Gt_Opr)) {

                        if (fold_relationals(IR_IDX_L(ir_idx),
					     IR_IDX_R(curr_range_ir_idx),
					     Gt_Opr)) {

                           if (IL_NEXT_LIST_IDX(curr_il_idx) == NULL_IDX) {
		              IL_NEXT_LIST_IDX(curr_il_idx) = new_il_idx;
		              IL_PREV_LIST_IDX(new_il_idx)  = curr_il_idx;
                              ++IR_LIST_CNT_R(select_ir_idx);
		              goto EXIT;
		           }
                           else {
			      goto ADVANCE_TO_NEXT_IL;
                           }

			}
			else {
			   PRINTMSG(IR_LINE_NUM(ir_idx), 749, Error,
				    IR_COL_NUM(ir_idx),
				    IR_LINE_NUM(curr_range_ir_idx));
			   goto EXIT;
			}

                     }

                  }

                  if (fold_relationals(IR_IDX_R(ir_idx),
				       IR_IDX_L(curr_range_ir_idx),
				       Lt_Opr)) {
		     insert_on_left(new_il_idx, curr_il_idx, select_ir_idx);
		     goto EXIT;
    		  }
		  else {
		     PRINTMSG(IR_LINE_NUM(ir_idx), 749, Error,
                              IR_COL_NUM(ir_idx),
                              IR_LINE_NUM(curr_range_ir_idx));
		     goto EXIT;
		  }

               }
               else {

                  if (fold_relationals(IR_IDX_L(ir_idx),
			               IR_IDX_R(curr_range_ir_idx),
				       Gt_Opr)) {

		     if (IL_NEXT_LIST_IDX(curr_il_idx) == NULL_IDX) {
		        IL_NEXT_LIST_IDX(curr_il_idx) = new_il_idx;
		        IL_PREV_LIST_IDX(new_il_idx)  = curr_il_idx;
                        ++IR_LIST_CNT_R(select_ir_idx);
		        goto EXIT;
		     }
                     else {
                        goto ADVANCE_TO_NEXT_IL;
                     }

	          }
	          else {
		     PRINTMSG(IR_LINE_NUM(ir_idx), 749, Error,
			      IR_COL_NUM(ir_idx),
		              IR_LINE_NUM(curr_range_ir_idx));
		     goto EXIT;
                  }

               }

            }

         }
         else {

            /* The new case range does NOT have a right value.		      */

            /* Does the current IL represent a single case-value?	      */
            /*   Y: Is the new left value > current value?		      */
            /*        Y: Is the current IL at the end of the list?	      */
            /*             Y: Append the new IL to the end of the list.       */
            /*             N: Advance to the next IL.			      */
            /*        N: Error; this range contains a value that was already  */
            /*             specified by a single case-value.		      */
            /*           {Quit}					 	      */
            /*   N: Does the current range have a right value?		      */
            /*        Y: Is the new left value > current right value?	      */
            /*             Y: Is the current IL at the end of the list?       */
            /*                  Y: Append the new IL to the end of the list.  */
            /*                  N: Advance to the next IL.		      */
            /*             N: --|					      */
            /*        N: --|						      */
            /*      Error; the ranges overlap.				      */
            /*      {Quit}						      */

            if (IL_FLD(curr_il_idx) == CN_Tbl_Idx) {

               if (fold_relationals(IR_IDX_L(ir_idx),
				    IL_IDX(curr_il_idx), Gt_Opr)) {

                  if (IL_NEXT_LIST_IDX(curr_il_idx) == NULL_IDX) {
                     IL_NEXT_LIST_IDX(curr_il_idx) = new_il_idx;
                     IL_PREV_LIST_IDX(new_il_idx)  = curr_il_idx;
                     ++IR_LIST_CNT_R(select_ir_idx);
                     goto EXIT;
                  }

	       }
               else {
                  PRINTMSG(IR_LINE_NUM(ir_idx), 748, Error,
			   IR_COL_NUM(ir_idx), IL_LINE_NUM(curr_il_idx));
                  goto EXIT;
               }

	    }
            else {

               if (IR_FLD_R(IL_IDX(curr_il_idx)) != NO_Tbl_Idx) {

                  if (fold_relationals(IR_IDX_L(ir_idx),
		   		       IR_IDX_R(IL_IDX(curr_il_idx)), Gt_Opr)) {

                     if (IL_NEXT_LIST_IDX(curr_il_idx) == NULL_IDX) {
                        IL_NEXT_LIST_IDX(curr_il_idx) = new_il_idx;
                        IL_PREV_LIST_IDX(new_il_idx)  = curr_il_idx;
                        ++IR_LIST_CNT_R(select_ir_idx);
                        goto EXIT;
                     }
                     else {
			goto ADVANCE_TO_NEXT_IL;
		     }

	          }
     
               }

               PRINTMSG(IR_LINE_NUM(ir_idx), 749, Error,
			IR_COL_NUM(ir_idx), IR_LINE_NUM(IL_IDX(curr_il_idx)));
               goto EXIT;
            }

         }

      }
      else {

         /* The new case range does NOT have a left value.		      */

         /* Does the current IL represent a single case-value? 		      */
         /*   Y: Is the new right value < current value?		      */
         /*        Y: Insert the new IL at the head of the list.	      */
         /*           (The current IL must be at the head of the list or an   */
         /*            error would already have been detected.)		      */
         /*           {Done}						      */
         /*        N: Error; this range contains a value that was already     */
         /*             specified by a single case-value.		      */
         /*           {Quit}					 	      */
         /*   N: Does the current range have a left value?		      */
         /*        Y: Is the new right value < current left value?	      */
         /*             Y: Insert the new IL at the head of the list.	      */
         /*                (The current IL must be at the head of the list or */
         /*                 an error would already have been detected.)       */
         /*                {Done}					      */
         /*             N: --|						      */
         /*        N: --|						      */
         /*      Error; the ranges overlap.				      */
         /*      {Quit}						      	      */

         if (IL_FLD(curr_il_idx) == CN_Tbl_Idx) {

            if (fold_relationals(IR_IDX_R(ir_idx), IL_IDX(curr_il_idx),
			         Lt_Opr)) {
               insert_on_left(new_il_idx, curr_il_idx, select_ir_idx);
               goto EXIT;
	    }
            else {
               PRINTMSG(IR_LINE_NUM(ir_idx), 748, Error,
			IR_COL_NUM(ir_idx), IL_LINE_NUM(curr_il_idx));
               goto EXIT;
            }

	 }
         else {

            if (IR_FLD_L(IL_IDX(curr_il_idx)) != NO_Tbl_Idx) {

               if (fold_relationals(IR_IDX_R(ir_idx),
				    IR_IDX_L(IL_IDX(curr_il_idx)), Lt_Opr)) {
                  insert_on_left(new_il_idx, curr_il_idx, select_ir_idx);
                  goto EXIT;
	       }
     
            }

            PRINTMSG(IR_LINE_NUM(ir_idx), 749, Error, 
		     IR_COL_NUM(ir_idx), IR_LINE_NUM(IL_IDX(curr_il_idx)));
            goto EXIT;
         }

      }
         
ADVANCE_TO_NEXT_IL:

      curr_il_idx = IL_NEXT_LIST_IDX(curr_il_idx);
   }  /* while */

EXIT:

   TRACE (Func_Exit, "case_value_range_semantics", NULL);

   return;

}  /* case_value_range_semantics */


/******************************************************************************\
|*									      *|
|* Description:								      *|
|*	This procedure inserts the "new" IL to the left (or ahead of) the     *|
|*      current IL in the case-value IL list attached to the Select IR.       *|
|*									      *|
|* Input parameters:							      *|
|*	new_il_idx    : the index of the "new" IL to be inserted in the list  *|
|*      curr_il_idx   : the index of the current IL; the new IL is to be      *|
|*                      inserted ahead of this one			      *|
|*      select_ir_idx : the index of the dummy Select IR; the sorted          *|
|*                      case-value list is attached to the right operand      *|
|*									      *|
|* Output parameters:							      *|
|*	NONE								      *|
|*									      *|
|* Returns:								      *|
|*	NONE								      *|
|*									      *|
\******************************************************************************/

static void insert_on_left(int		new_il_idx,
			   int		curr_il_idx,
			   int		select_ir_idx)

{

   TRACE (Func_Entry, "insert_on_left", NULL);

   /* Is the current IL the first one in the list?			      */
   /*   Y: Insert the new IL at the head of the list.			      */
   /*   N: Insert the new IL between the current IL and the IL preceding the  */
   /*      current IL.							      */

   if (IR_IDX_R(select_ir_idx) == curr_il_idx) {
      IR_IDX_R(select_ir_idx) = new_il_idx;
   }
   else {
      IL_NEXT_LIST_IDX(IL_PREV_LIST_IDX(curr_il_idx)) = new_il_idx;
      IL_PREV_LIST_IDX(new_il_idx) = IL_PREV_LIST_IDX(curr_il_idx);
   }

   IL_NEXT_LIST_IDX(new_il_idx)  = curr_il_idx;
   IL_PREV_LIST_IDX(curr_il_idx) = new_il_idx;

   ++IR_LIST_CNT_R(select_ir_idx);

   TRACE (Func_Exit, "insert_on_left", NULL);

   return;

}  /* insert_on_left */


/******************************************************************************\
|*									      *|
|* Description:								      *|
|*	This procedure is called by the DO statement semantics routine to     *|
|*      check the semantics of the start, end, and increment expressions.     *|
|*      If the expression is OK, an assignment statement is generated to      *|
|*      freeze the expression in a temp if the expression is not a constant   *|
|*      value.								      *|
|*									      *|
|* Input parameters:							      *|
|*	expr_il_idx : The IL index of the expression to be evaluated.	      *|
|*      do_var_idx  : Attr index for the DO variable.			      *|
|*									      *|
|* Output parameters:							      *|
|*	expr_opnd   : Cray:  Points at the Asg IR generated to freeze the     *|
|*                           expression or it points at the result value.     *|
|*                    ACSET: Points at the expression result.		      *|
|*									      *|
|* Returns:								      *|
|*	True if the expression is acceptable.				      *|
|*									      *|
\******************************************************************************/

static boolean	 do_loop_expr_semantics (int	 	 expr_il_idx,
					 int	 	 do_var_idx,
				         opnd_type 	*expr_opnd)

{
   int			col;
   expr_arg_type	exp_desc;
   int			line;
   boolean		result		= TRUE;
   int			save_next_sh_idx;

# ifndef _HIGH_LEVEL_IF_FORM
   int			idx;
   int			ir_idx;
   opnd_type		opnd;
   int			tmp_idx;
# endif
 

   TRACE (Func_Entry, "do_loop_expr_semantics", NULL);

   save_next_sh_idx = SH_NEXT_IDX(curr_stmt_sh_idx);

   COPY_OPND(*expr_opnd, IL_OPND(expr_il_idx));
   find_opnd_line_and_column(expr_opnd, &line, &col);
   exp_desc.rank = 0;
   xref_state    = CIF_Symbol_Reference;
  
   if (expr_semantics(expr_opnd, &exp_desc)) {
      
      /* It is possible that expr_semantics generated statements that follow  */
      /* curr_stmt_sh_idx.  The following line moves curr_stmt_sh_idx to the  */
      /* end of the generated statements.			              */

      curr_stmt_sh_idx = SH_PREV_IDX(save_next_sh_idx);


      if (exp_desc.rank != 0) {
         PRINTMSG(IL_LINE_NUM(expr_il_idx), 222, Error,
                  IL_COL_NUM(expr_il_idx));
         result = FALSE;
      }

      /* The expression can be default or nondefault integer, default real or */
      /* double precision (both of these are obsolescent), or typeless.       */
      /* (It should be typeless only if the expression consists of a Boolean  */
      /* constant - a CRI extension.)					      */
      
      if (exp_desc.type == Integer) {

         /* Good.  Nothing to do.					      */
    
      } 
      else if (exp_desc.type == Real  && 
               (exp_desc.linear_type == REAL_DEFAULT_TYPE  ||
                exp_desc.linear_type == DOUBLE_DEFAULT_TYPE)) {
              PRINTMSG(IL_LINE_NUM(expr_il_idx), 1569, Ansi,
                       IL_COL_NUM(expr_il_idx));
      }
      else if (exp_desc.type == Typeless) {

         if ((exp_desc.linear_type == Typeless_4 ||
              exp_desc.linear_type == Typeless_8)  &&
             TYP_LINEAR(ATD_TYPE_IDX(do_var_idx)) == DOUBLE_DEFAULT_TYPE) {
            PRINTMSG(IL_LINE_NUM(expr_il_idx), 1047, Error,
                     IL_COL_NUM(expr_il_idx));
            result = FALSE;
         }
         else if (exp_desc.linear_type == Short_Typeless_Const) {
            OPND_IDX((*expr_opnd)) = 
                          cast_typeless_constant(OPND_IDX((*expr_opnd)),
                                                 ATD_TYPE_IDX(do_var_idx),
						 line,
						 col);
            exp_desc.type_idx    = ATD_TYPE_IDX(do_var_idx);
            exp_desc.type        = TYP_TYPE(exp_desc.type_idx);
            exp_desc.linear_type = TYP_LINEAR(exp_desc.type_idx);
         }
         else if (exp_desc.linear_type == Long_Typeless) {
            PRINTMSG(IL_LINE_NUM(expr_il_idx), 394, Error,
                     IL_COL_NUM(expr_il_idx));
            result = FALSE;
         }
      }
      else {
         PRINTMSG(IL_LINE_NUM(expr_il_idx),
                  (exp_desc.type == Typeless) ? 694 : 217,
                  Error,
                  IL_COL_NUM(expr_il_idx));
         result = FALSE;
      }


      /* If the expression is acceptable, then for the high-level iterative   */
      /* DO loop form, just replace the index to the IR tree in the loop      */
      /* control IL chain attached to the Loop_Info IR with the tree          */
      /* produced from expr_semantics.					      */
      /* For the low-level iterative DO loop form, do one of two things:      */
      /*   - If the expression resolves to a constant, convert it to the type */
      /*     of the DO variable.					      */
      /*   - Otherwise, generate an assignment statement to freeze the value  */
      /*     of the expression.						      */

      if (result) {

# ifdef _HIGH_LEVEL_DO_LOOP_FORM

         COPY_OPND(IL_OPND(expr_il_idx), *expr_opnd);

# else

         if (OPND_FLD((*expr_opnd)) == CN_Tbl_Idx) {
            IL_FLD(expr_il_idx) = CN_Tbl_Idx;
            IL_IDX(expr_il_idx) = OPND_IDX((*expr_opnd));

            if (CN_TYPE_IDX(OPND_IDX((*expr_opnd))) !=
                   ATD_TYPE_IDX(do_var_idx)) {
               IL_IDX(expr_il_idx) =
                  convert_to_do_var_type((TYP_TYPE(ATD_TYPE_IDX(do_var_idx)) ==
                                             CRI_Ptr) ?
                                             INTEGER_DEFAULT_TYPE :
                                             ATD_TYPE_IDX(do_var_idx),
                                         IL_IDX(expr_il_idx));
               OPND_IDX((*expr_opnd)) = IL_IDX(expr_il_idx);
            }
         }
         else {

            /* Generate an assignment statement to freeze the expression in   */
            /* a temp.  						      */

            gen_sh(After, Assignment_Stmt, stmt_start_line, stmt_start_col,
                   FALSE, FALSE, TRUE);

            GEN_COMPILER_TMP_ASG(ir_idx,
                                 tmp_idx,
                                 FALSE,         /* Do semantics on tmp */
                                 line,
                                 col,
                                 INTEGER_DEFAULT_TYPE,
                                 Priv);

            COPY_OPND(IR_OPND_R(ir_idx), *expr_opnd);

            SH_IR_IDX(curr_stmt_sh_idx) = ir_idx;

            /* Make the Asg IR result type is the same as the DO variable.    */
            /* Set the result temp to the type of the DO variable unless the  */
            /* DO variable is a CRI pointer in which case leave the temp as   */
            /* default integer (because the temps are used in the trip count  */
            /* calculation and the rules of CRI pointer arithmetic are a bit  */
            /* arcane).				         		      */

            IR_TYPE_IDX(ir_idx) = ATD_TYPE_IDX(do_var_idx);
   
            if (TYP_TYPE(ATD_TYPE_IDX(do_var_idx)) != CRI_Ptr) {
               ATD_TYPE_IDX(IR_IDX_L(ir_idx)) = ATD_TYPE_IDX(do_var_idx);
            }

            if (cdir_switches.doall_sh_idx ||
                cdir_switches.paralleldo_omp_sh_idx) {

               if (preamble_end_sh_idx == NULL_IDX) {
                  gen_opnd(&opnd, curr_stmt_sh_idx, SH_Tbl_Idx, 
                           stmt_start_line, stmt_start_col);
                  copy_subtree(&opnd, &opnd);
                  preamble_start_sh_idx = OPND_IDX(opnd);
                  SH_COMPILER_GEN(preamble_start_sh_idx) = TRUE;
                  SH_P2_SKIP_ME(preamble_start_sh_idx) = TRUE;
                  preamble_end_sh_idx = preamble_start_sh_idx;
               }
               else {
                  gen_opnd(&opnd, curr_stmt_sh_idx, SH_Tbl_Idx, 
                           stmt_start_line, stmt_start_col);
                  copy_subtree(&opnd, &opnd);
                  idx = OPND_IDX(opnd);
                  SH_NEXT_IDX(preamble_end_sh_idx) = idx;

                  if (SH_NEXT_IDX(preamble_end_sh_idx)) {
                     SH_PREV_IDX(SH_NEXT_IDX(preamble_end_sh_idx)) =
                                                       preamble_end_sh_idx;
                  }
                  preamble_end_sh_idx = SH_NEXT_IDX(preamble_end_sh_idx);
                  SH_COMPILER_GEN(preamble_end_sh_idx) = TRUE;
                  SH_P2_SKIP_ME(preamble_end_sh_idx) = TRUE;
               }
            }

            /* Save the target temp in the IL that originally pointed at the  */
            /* expression so that it can be used by end-of-loop processing.   */

            IL_FLD(expr_il_idx) = AT_Tbl_Idx;
            IL_IDX(expr_il_idx) = tmp_idx;
         } 

# endif 

      }
   }
   else {
      result = FALSE;
   }

   TRACE (Func_Exit, "do_loop_expr_semantics", NULL);

   return(result);

}  /* do_loop_expr_semantics */



# ifndef _HIGH_LEVEL_DO_LOOP_FORM

/******************************************************************************\
|*									      *|
|* Description:								      *|
|*	This procedure is called by the DO statement semantics routine when   *|
|*      all 3 loop expressions are constant values.  It calculates the        *|
|*      iteration count at compile time and, for Crays, checks to see if the  *|
|*      loop iteration count is too large.			              *|
|*									      *|
|* Input parameters:							      *|
|*      do_sh_idx   : SH index for the DO statement			      *|
|*	start_idx   : CN index for the start value			      *|
|*	end_idx     : CN index for the start value			      *|
|*	inc_idx     : CN index for the start value			      *|
|*      do_vari_idx : AT index for the DO variable			      *|
|*									      *|
|* Output parameters:							      *|
|*	NONE								      *|
|*									      *|
|* Returns:								      *|
|*	The CN index for the iteration count value if the calculation         *|
|*      succeeded.  Returns a 0 CN index otherwise.			      *|
|*									      *|
\******************************************************************************/

static  int calculate_iteration_count(int	do_sh_idx,
			   	      int	start_idx,
				      int	end_idx,
				      int	inc_idx,
				      int  	do_var_idx)
{
   long64		cri_loop_limit;
   int			cri_loop_limit_idx;
   basic_type_type	do_var_type;
   linear_type_type	do_var_lin_type;
   int			do_var_type_idx;
   expr_arg_type	expr_desc;
   opnd_type		expr_opnd;
   int			ir_idx;
   int			iter_count_idx;
   int			iter_count_ir_idx;
   int			result_type_idx;
   long_type            result_value[MAX_WORDS_FOR_NUMERIC];


# ifdef _DEBUG
   int			orig_iter_count_idx;
   long_type		debug_converted_value[MAX_WORDS_FOR_NUMERIC];
# endif


# ifdef _TARGET_OS_UNICOS

   /* Define the smallest numbers greater than 1.0 for CRAY PVP architecture. */
   /* (Needed for the multiplication below because on a CRAY, a division like */
   /* 42 / 6 can produce a value of 6.999... which when truncated produces    */
   /* 6.0 which is the wrong iteration count.)				      */

  long_type	fudge;
  int		fudge_idx;

  struct	{long_type	part_1;
		 long_type	part_2;
		} double_fudge;

# endif

               
   TRACE (Func_Entry, "calculate_iteration_count", NULL);

   /* Set comp_gen_expr to TRUE to force real constant expressions to be      */
   /* folded.  When -Oieeeconform is specified, the folding of real and       */
   /* complex expressions is disabled.					      */

   comp_gen_expr = TRUE;


   /* Get the type information for the DO variable.  Set up the IR            */
   /* representing the iteration count expression  (END - START + INC) / INC  */

   if (TYP_TYPE(ATD_TYPE_IDX(do_var_idx)) == CRI_Ptr) {
      do_var_type	= Integer;
      do_var_lin_type	= INTEGER_DEFAULT_TYPE;
      do_var_type_idx	= INTEGER_DEFAULT_TYPE;
   }
   else {
      do_var_type_idx	= ATD_TYPE_IDX(do_var_idx);
      do_var_type	= TYP_TYPE(do_var_type_idx);
      do_var_lin_type	= TYP_LINEAR(do_var_type_idx);
   }
   
   NTR_IR_TBL(iter_count_ir_idx);
   IR_OPR(iter_count_ir_idx)      = Minus_Opr;
   IR_TYPE_IDX(iter_count_ir_idx) = do_var_type_idx;
   IR_LINE_NUM(iter_count_ir_idx) = stmt_start_line;
   IR_COL_NUM(iter_count_ir_idx)  = stmt_start_line;
   IR_FLD_L(iter_count_ir_idx)    = CN_Tbl_Idx;
   IR_IDX_L(iter_count_ir_idx)    = end_idx;
   IR_LINE_NUM_L(iter_count_ir_idx) = stmt_start_line;
   IR_COL_NUM_L(iter_count_ir_idx)  = stmt_start_line;
   IR_FLD_R(iter_count_ir_idx)    = CN_Tbl_Idx;
   IR_IDX_R(iter_count_ir_idx)    = start_idx;
   IR_LINE_NUM_R(iter_count_ir_idx) = stmt_start_line;
   IR_COL_NUM_R(iter_count_ir_idx)  = stmt_start_line;

   NTR_IR_TBL(ir_idx);
   IR_OPR(ir_idx)      = Plus_Opr;
   IR_TYPE_IDX(ir_idx) = do_var_type_idx;
   IR_LINE_NUM(ir_idx) = stmt_start_line;
   IR_COL_NUM(ir_idx)  = stmt_start_line;
   IR_FLD_L(ir_idx)    = IR_Tbl_Idx;
   IR_IDX_L(ir_idx)    = iter_count_ir_idx;
   IR_FLD_R(ir_idx)    = CN_Tbl_Idx;
   IR_IDX_R(ir_idx)    = inc_idx;
   IR_LINE_NUM_R(ir_idx) = stmt_start_line;
   IR_COL_NUM_R(ir_idx)  = stmt_start_line;

   NTR_IR_TBL(iter_count_ir_idx);
   IR_OPR(iter_count_ir_idx)      = Div_Opr;
   IR_TYPE_IDX(iter_count_ir_idx) = do_var_type_idx;
   IR_LINE_NUM(iter_count_ir_idx) = stmt_start_line;
   IR_COL_NUM(iter_count_ir_idx)  = stmt_start_line;
   IR_FLD_L(iter_count_ir_idx)    = IR_Tbl_Idx;
   IR_IDX_L(iter_count_ir_idx)    = ir_idx;
   IR_FLD_R(iter_count_ir_idx)    = CN_Tbl_Idx;
   IR_IDX_R(iter_count_ir_idx)    = inc_idx;
   IR_LINE_NUM_R(iter_count_ir_idx) = stmt_start_line;
   IR_COL_NUM_R(iter_count_ir_idx)  = stmt_start_line;
  
   OPND_FLD(expr_opnd) = IR_Tbl_Idx;
   OPND_IDX(expr_opnd) = iter_count_ir_idx;


   /* If the host machine is a nonPVP machine or the host is a PVP and the    */
   /* loop control expressions are type INTEGER(8), we need to be careful in  */
   /* calculating the iteration count because it could overflow.  In order    */
   /* to prevent the overflow message from being output by the folder, turn   */
   /* it off, then upon return, check to see if overflow (including too small */
   /* of a negative integer value) occurred.				      */

   expr_desc.rank         = 0;
   issue_overflow_msg_719 = FALSE;

   if (expr_semantics(&expr_opnd, &expr_desc)) {
      iter_count_idx = OPND_IDX(expr_opnd);

      if (do_var_type != Integer) {

         /* Convert the iteration count to integer.                           */

# if !(defined(_HOST_OS_IRIX) || defined(_HOST_OS_LINUX) || defined(_HOST_OS_DARWIN))
# ifdef _TARGET_OS_UNICOS

# ifdef _DEBUG
         orig_iter_count_idx = OPND_IDX(expr_opnd);
# endif

         /* The DO-variable is type real or double precision, so we have to   */
         /* be careful to multiply the calculated value by the smallest       */
         /* number > 1 to round a division result like 6.99999... to 7.	      */
         /* Note that union variables can not be initialized so code exists   */
         /* below to get the value into the appropriate fudge factor.	      */

         /* IEEE machines (such as the IEEE T90) do division exactly so the   */
         /* fudging around and the multiply is not needed.		      */

         if (do_var_type == Real  &&  ! (target_triton  &&  target_ieee)) {
    
            if (do_var_lin_type == REAL_DEFAULT_TYPE) {


# if defined(_HOST_OS_UNICOS)

               fudge = 00400014000000000000001;

# elif defined(_HOST32)

               fudge = 00400014000000000000001ULL;   /* BRIANJ */

# endif

               fudge_idx = ntr_const_tbl( REAL_DEFAULT_TYPE,
                                          FALSE,
	       	                         &fudge);
            }
            else {

# if defined(_HOST_OS_UNICOS)

               double_fudge.part_1 = 00400014000000000000000;
               double_fudge.part_2 = 1;

# elif defined(_HOST32)

               double_fudge.part_1 = 00400014000000000000000ULL;
               double_fudge.part_2 = 1;

# endif

               fudge_idx = ntr_const_tbl(DOUBLE_DEFAULT_TYPE,
                                         FALSE,
                                         (long_type *) &double_fudge);
            }

            result_type_idx = do_var_type_idx;

            if (folder_driver( (char *) &CN_CONST(iter_count_idx),
                               do_var_type_idx,
                               (char *) &CN_CONST(fudge_idx),
                               do_var_type_idx,
                               result_value,
                              &result_type_idx,
                               stmt_start_line,
                               stmt_start_col,
                               2,
                               Mult_Opr)) {
               iter_count_idx = ntr_const_tbl(do_var_lin_type,
                                              FALSE,
                                              result_value);
            }
            else {
               PRINTMSG(stmt_start_line, 857, Internal, stmt_start_col);
               SH_ERR_FLG(do_sh_idx) = TRUE;
            }
         }

# endif                               /* End special Cray PVP considerations. */
# endif                               /* End special Cray PVP considerations. */


         result_type_idx = INTEGER_DEFAULT_TYPE;

         if (folder_driver((char *)&CN_CONST(iter_count_idx),
                           do_var_type_idx,
                           NULL,
                           NULL_IDX,
                           result_value,
                          &result_type_idx,
                           stmt_start_line,
                           stmt_start_col,
                           1,
                           Cvrt_Opr)) {

            iter_count_idx = ntr_const_tbl(INTEGER_DEFAULT_TYPE,
                                           FALSE,
                                           result_value);
         }


# if !(defined(_HOST_OS_IRIX) || defined(_HOST_OS_LINUX) || defined(_HOST_OS_DARWIN))
# ifdef _TARGET_OS_UNICOS
# ifdef _DEBUG
   
         /* Before comparing the calculated iteration count to the CRI limit, */
         /* make sure our fudge factor multiplication assumptions have worked */
         /* out.  That is, if we convert both the original iteration count    */
         /* and the fudged iteration count to integer, they should be equal   */
         /* or the fudged one should be 1 greater than the original value.    */
         /* If these relationships don't hold, we want to rethink this code.  */

         result_type_idx = INTEGER_DEFAULT_TYPE;

         if (folder_driver((char *)&CN_CONST(orig_iter_count_idx),
                           CN_TYPE_IDX(orig_iter_count_idx),
                           NULL,
                           NULL_IDX,
                           debug_converted_value,
                          &result_type_idx,
                           stmt_start_line,
                           stmt_start_col,
                           1,
                           Cvrt_Opr)) {

            orig_iter_count_idx = ntr_const_tbl(INTEGER_DEFAULT_TYPE,
                                                FALSE,
                                                debug_converted_value);
         }

         if (fold_relationals(orig_iter_count_idx, iter_count_idx, Ne_Opr)) {
            result_type_idx = INTEGER_DEFAULT_TYPE;

            if (folder_driver((char *) debug_converted_value,
                              INTEGER_DEFAULT_TYPE,
                              (char *) &CN_CONST(CN_INTEGER_ONE_IDX),
                              CN_TYPE_IDX(CN_INTEGER_ONE_IDX),
                              debug_converted_value,
                             &result_type_idx,
                              stmt_start_line,
                              stmt_start_col,
                              2,
                              Plus_Opr)) {
   
            }

            /* THe above call to folder_driver replaces this line */
            /* ++debug_converted_value[0];                        */
            /* BRIANJ JEFFL KAY                                   */
      
            orig_iter_count_idx = ntr_const_tbl(INTEGER_DEFAULT_TYPE,
                                                FALSE,
                                                debug_converted_value);

            if (! fold_relationals(orig_iter_count_idx, iter_count_idx,
                                   Eq_Opr)) {
               PRINTMSG(stmt_start_line, 857, Internal, stmt_start_col);
               SH_ERR_FLG(do_sh_idx) = TRUE;
            }
         } 

# endif  
# endif  
# endif                                       /* End Cray PVP considerations. */

      }


# if !(defined(_HOST_OS_IRIX) || defined(_HOST_OS_LINUX) || defined(_HOST_OS_DARWIN))
# ifdef _TARGET_OS_UNICOS

      /* Now that the iteration count has been converted to integer, if       */
      /* necessary, for Cray PVP machines verify that the iteration count     */
      /* will fit in a 32-bit A register.				      */

      if (! (target_triton  &&  target_ieee)) {

         if (target_triton) {
# ifdef _HOST64
            cri_loop_limit = 70368744177663L;                  /*  2**46 - 1  */
# else
            cri_loop_limit = 70368744177663LL;                 /*  2**46 - 1  */
# endif
         }
         else {
# ifdef _HOST64
            cri_loop_limit = 2147483647L;                      /*  2**31 - 1  */
# else
            cri_loop_limit = 2147483647LL;                     /*  2**31 - 1  */
# endif
         }

         cri_loop_limit_idx = C_INT_TO_CN(INTEGER_DEFAULT_TYPE,
				          cri_loop_limit);

         if (fold_relationals(iter_count_idx, cri_loop_limit_idx, Gt_Opr)) {
            PRINTMSG(stmt_start_line, 856, Error, stmt_start_col,
                     cri_loop_limit);
            SH_ERR_FLG(do_sh_idx) = TRUE;
         }
      }

# endif                                
# endif                                
                                      

   }
   else {

      /* Semantic analysis of the iteration count expression failed.          */

      iter_count_idx = 0;

      if (need_to_issue_719) {
         PRINTMSG(stmt_start_line, 1082, Error, stmt_start_col);
         need_to_issue_719     = FALSE;
         SH_ERR_FLG(do_sh_idx) = TRUE;
      }
      else {
         PRINTMSG(stmt_start_line, 857, Internal, stmt_start_col);
      }
   }

   issue_overflow_msg_719 = TRUE;


   /* Reset comp_gen_expr to FALSE because we're at the end of the compiler   */
   /* generated expression processing. 					      */

   comp_gen_expr = FALSE;

   TRACE (Func_Exit, "calculate_iteration_count", NULL);

   return(iter_count_idx);

}  /* calculate_iteration_count */



/******************************************************************************\
|*									      *|
|* Description:								      *|
|*	Convert the loop control expression value to the DO-variable type.    *|
|*									      *|
|* Input parameters:							      *|
|*      do_var_type_idx : DO-variable type_idx     	 		      *|
|*      cn_idx          : CN index of loop control expression       	      *|
|*									      *|
|* Output parameters:							      *|
|*	NONE								      *|
|*									      *|
|* Returns:								      *|
|*	converted_cn_idx : the index to the CN entry for the loop control     *|
|*                         expression converted to the DO-variable type or    *|
|*                         NULL_IDX if something went wrong		      *|
|*									      *|
\******************************************************************************/

static int convert_to_do_var_type(int		do_var_type_idx,
			   	  int		cn_idx)
{
   int			converted_cn_idx;
   long_type		converted_value[MAX_WORDS_FOR_NUMERIC];
   basic_type_type	do_var_type;
   linear_type_type	do_var_lin_type;
   int			type_idx;
  

   TRACE (Func_Entry, "convert_to_do_var_type", NULL);

   do_var_type		= TYP_TYPE(do_var_type_idx);
   do_var_lin_type	= TYP_LINEAR(do_var_type_idx);

   if (TYP_TYPE(CN_TYPE_IDX(cn_idx)) == do_var_type  &&
       TYP_LINEAR(CN_TYPE_IDX(cn_idx)) == do_var_lin_type) {
      converted_cn_idx = cn_idx;
   }
   else {

      if (TYP_TYPE(CN_TYPE_IDX(cn_idx)) == Typeless) {

         /* Hey, LRR, I need a better line and column here. */

         converted_cn_idx = cast_typeless_constant(cn_idx,
                                                   do_var_type_idx,
                                                   stmt_start_line,
                                                   stmt_start_col);
      }
      else {
             
         if (do_var_lin_type != TYP_LINEAR(CN_TYPE_IDX(cn_idx))) {

            type_idx = do_var_type_idx;

            if (folder_driver((char *)&CN_CONST(cn_idx),
                              CN_TYPE_IDX(cn_idx),
                              NULL,
                              NULL_IDX,
                              converted_value,
                             &type_idx,
                              stmt_start_line,
                              stmt_start_col,
                              1,
                              Cvrt_Opr)) {
            }
         }
         else {
            /* BRIANJ - This is probably wrong here. */
            converted_value[0] = CN_INT_TO_C(cn_idx);

            if (TYP_TYPE(CN_TYPE_IDX(cn_idx)) == Real  &&
                TYP_LINEAR(CN_TYPE_IDX(cn_idx)) == DOUBLE_DEFAULT_TYPE) {
               converted_value[1] = CP_CONSTANT(CN_POOL_IDX(cn_idx) + 1);
            }
         }

         converted_cn_idx = ntr_const_tbl(do_var_type_idx,
                                          FALSE,
				          converted_value);
      }
   }

   TRACE (Func_Exit, "convert_to_do_var_type", NULL);

   return(converted_cn_idx);

}  /* convert_to_do_var_type */

# endif



/******************************************************************************\
|*									      *|
|* Description:								      *|
|*	Generate the IR at the end of a loop.  This procedure is called by    *|
|*      semantics_pass_driver.						      *|
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

void gen_loop_end_ir()

{
   int			asg_ir_idx;
   int			attr_idx;
   int			do_sh_idx;
   expr_arg_type	expr_desc;
   int			il_idx;
   int			ir_idx;
   int			loop_control_il_idx;
   int			loop_labels_il_idx;
   int			loop_info_idx;
   opnd_type		temp_opnd;

# ifdef _HIGH_LEVEL_DO_LOOP_FORM
   int			loop_end_sh_idx;
# else
   int			asg_idx;
   int			do_var_il_idx;
   int			do_var_linear_type;
   int			expr_ir_idx;
   long_type		folded_const[MAX_WORDS_FOR_NUMERIC];
   int			il_idx_2;
   int			inc_il_idx;
   int			induc_tmp_il_idx;
   int			init_ir_idx;
   int			max_int_idx;
   int                  opnd_column;
   int                  opnd_line;
   int                  save_curr_stmt_sh_idx;
   int			start_il_idx;
   int			trip_cnt_il_idx;
   int			tmp_idx;
   int			tmp_idx2;
# endif


   TRACE (Func_Entry, "gen_loop_end_ir", NULL);

   /* The current SH is a compiler-generated CONTINUE SH that represents      */
   /* either the EXIT label or the loop bottom (skip) label.                  */

   do_sh_idx           = SH_PARENT_BLK_IDX(curr_stmt_sh_idx);
   loop_info_idx       = SH_IR_IDX(do_sh_idx);
   loop_control_il_idx = IR_IDX_R(loop_info_idx);
   loop_labels_il_idx  = IL_NEXT_LIST_IDX(loop_control_il_idx);


   /* If this is an iterative DO, clear its "live DO variable" flag.	      */

   if (SH_STMT_TYPE(do_sh_idx) == Do_Iterative_Stmt) {

      if (IL_FLD(loop_control_il_idx) == IL_Tbl_Idx) {
         il_idx = IL_IDX(loop_control_il_idx);

         attr_idx = find_left_attr(&IL_OPND(il_idx));

         if (attr_idx &&
             AT_OBJ_CLASS(attr_idx) == Data_Obj) {
            ATD_LIVE_DO_VAR(attr_idx) = FALSE;
         }
      }
   }


   /* If the DO statement is in error, don't bother trying to do anything     */
   /* more.								      */

   if (SH_ERR_FLG(SH_PARENT_BLK_IDX(curr_stmt_sh_idx))) {
      goto EXIT;
   }


   if (cif_flags & MISC_RECS) {
      cif_loop_def_rec();
   }


   /* Generate IR depending on the type of the loop.			      */

   switch (SH_STMT_TYPE(do_sh_idx)) {

      /* -------------------------------------------------------------------- */
      /*								      */
      /*               DO [label] [,] do-var = expr, expr [, expr]	      */
      /*								      */
      /* -------------------------------------------------------------------- */

      case Do_Iterative_Stmt:
 
# ifndef _HIGH_LEVEL_DO_LOOP_FORM

         start_il_idx = IL_NEXT_LIST_IDX(IL_IDX(loop_control_il_idx));
         inc_il_idx   = IL_NEXT_LIST_IDX(IL_NEXT_LIST_IDX(start_il_idx));

         if (cif_flags & MISC_RECS) {
            il_idx = IL_NEXT_LIST_IDX(IL_NEXT_LIST_IDX(loop_labels_il_idx));
         } 
         else {
            il_idx = IL_NEXT_LIST_IDX(loop_labels_il_idx);
         }

         trip_cnt_il_idx  = IL_IDX(il_idx);
         induc_tmp_il_idx = IL_NEXT_LIST_IDX(trip_cnt_il_idx);

         /* Generate the assignment statement:  induc_temp = induc_temp + 1   */

         NTR_IR_TBL(expr_ir_idx);
         IR_OPR(expr_ir_idx)           = Plus_Opr;
         IR_TYPE_IDX(expr_ir_idx)      = INTEGER_DEFAULT_TYPE;
         IR_LINE_NUM(expr_ir_idx)      = stmt_start_line;
         IR_COL_NUM(expr_ir_idx)       = stmt_start_col;
         COPY_OPND(IR_OPND_L(expr_ir_idx), IL_OPND(induc_tmp_il_idx));
         IR_LINE_NUM_R(expr_ir_idx)    = stmt_start_line;
         IR_COL_NUM_R(expr_ir_idx)     = stmt_start_col;
         IR_FLD_R(expr_ir_idx)         = CN_Tbl_Idx;
         IR_IDX_R(expr_ir_idx)         = CN_INTEGER_ONE_IDX;

         NTR_IR_TBL(ir_idx);
         IR_OPR(ir_idx)           = Asg_Opr;
         IR_TYPE_IDX(ir_idx)      = INTEGER_DEFAULT_TYPE;
         IR_LINE_NUM(ir_idx)      = stmt_start_line;
         IR_COL_NUM(ir_idx)       = stmt_start_col;
         COPY_OPND(IR_OPND_L(ir_idx), IL_OPND(induc_tmp_il_idx));
         IR_LINE_NUM_R(ir_idx)    = stmt_start_line;
         IR_COL_NUM_R(ir_idx)     = stmt_start_col;
         IR_FLD_R(ir_idx)         = IR_Tbl_Idx;
         IR_IDX_R(ir_idx)         = expr_ir_idx;

         gen_sh(Before, Assignment_Stmt, stmt_start_line, stmt_start_col,
                FALSE, FALSE, TRUE);
         
         SH_IR_IDX(SH_PREV_IDX(curr_stmt_sh_idx)) = ir_idx;


         /* Generate the test and branch to the top-of-loop label: 	      */
         /*   IF (induc_temp < trip_count_temp) GO TO top_lbl		      */

         NTR_IR_TBL(expr_ir_idx);
         IR_OPR(expr_ir_idx)           = Lt_Opr;
         IR_TYPE_IDX(expr_ir_idx)      = LOGICAL_DEFAULT_TYPE;
         IR_LINE_NUM(expr_ir_idx)      = stmt_start_line;
         IR_COL_NUM(expr_ir_idx)       = stmt_start_col;
         COPY_OPND(IR_OPND_L(expr_ir_idx), IL_OPND(induc_tmp_il_idx));
         COPY_OPND(IR_OPND_R(expr_ir_idx), IL_OPND(trip_cnt_il_idx));

         NTR_IR_TBL(ir_idx);
         IR_OPR(ir_idx)           = Br_True_Opr;
         IR_TYPE_IDX(ir_idx)      = LOGICAL_DEFAULT_TYPE;
         IR_LINE_NUM(ir_idx)      = stmt_start_line;
         IR_COL_NUM(ir_idx)       = stmt_start_col;
         IR_LINE_NUM_L(ir_idx)    = stmt_start_line;
         IR_COL_NUM_L(ir_idx)     = stmt_start_col;
         IR_FLD_L(ir_idx)         = IR_Tbl_Idx;
         IR_IDX_L(ir_idx)         = expr_ir_idx;
         IR_LINE_NUM_R(ir_idx)    = stmt_start_line;
         IR_COL_NUM_R(ir_idx)     = stmt_start_col;
         IR_FLD_R(ir_idx)         = AT_Tbl_Idx;
         IR_IDX_R(ir_idx)         = IL_IDX(IL_IDX(loop_labels_il_idx));

         AT_REFERENCED(IL_IDX(IL_IDX(loop_labels_il_idx))) = Referenced;

         gen_sh(Before, If_Stmt, stmt_start_line, stmt_start_col, 
                FALSE, FALSE, TRUE);

         SH_IR_IDX(SH_PREV_IDX(curr_stmt_sh_idx)) = ir_idx;


         /* Generate the assignment statement to set the terminal value of    */
         /* the DO-variable:						      */
         /*     DO-variable = start + trip_count * inc			      */

         NTR_IR_TBL(expr_ir_idx);
         IR_OPR(expr_ir_idx)        = Mult_Opr;
         IR_LINE_NUM(expr_ir_idx)   = stmt_start_line;
         IR_COL_NUM(expr_ir_idx)    = stmt_start_col;
         COPY_OPND(IR_OPND_L(expr_ir_idx), IL_OPND(trip_cnt_il_idx));
         COPY_OPND(IR_OPND_R(expr_ir_idx), IL_OPND(inc_il_idx));

         NTR_IR_TBL(ir_idx);
         IR_OPR(ir_idx)        = Plus_Opr;
         IR_LINE_NUM(ir_idx)   = stmt_start_line;
         IR_COL_NUM(ir_idx)    = stmt_start_col;
         COPY_OPND(IR_OPND_L(ir_idx), IL_OPND(start_il_idx));
         IR_LINE_NUM_R(ir_idx) = stmt_start_line;
         IR_COL_NUM_R(ir_idx)  = stmt_start_col;
         IR_FLD_R(ir_idx)      = IR_Tbl_Idx;
         IR_IDX_R(ir_idx)      = expr_ir_idx;

         NTR_IR_TBL(asg_ir_idx);
         IR_OPR(asg_ir_idx)           = Asg_Opr;
         IR_LINE_NUM(asg_ir_idx)      = stmt_start_line;
         IR_COL_NUM(asg_ir_idx)       = stmt_start_col;
         COPY_OPND(IR_OPND_L(asg_ir_idx),
                   IL_OPND(IL_IDX(IR_IDX_R(loop_info_idx))));
         IR_TYPE_IDX(asg_ir_idx)      = (IR_FLD_L(asg_ir_idx) == AT_Tbl_Idx) ?
                                           ATD_TYPE_IDX(IR_IDX_L(asg_ir_idx)) :
                                           IR_TYPE_IDX(IR_IDX_L(asg_ir_idx));
         IR_LINE_NUM_R(asg_ir_idx)    = stmt_start_line;
         IR_COL_NUM_R(asg_ir_idx)     = stmt_start_col;
         IR_FLD_R(asg_ir_idx)         = IR_Tbl_Idx;
         IR_IDX_R(asg_ir_idx)         = ir_idx;
    
         gen_sh(Before, Assignment_Stmt, stmt_start_line, stmt_start_col,
                FALSE, FALSE, TRUE);

         SH_IR_IDX(SH_PREV_IDX(curr_stmt_sh_idx)) = asg_ir_idx;


         /* Send the expression through expr_semantics to get types, etc.     */
         /* propagated.  However, we must be careful because the calculation  */
         /* could overflow.  In order to prevent the overflow message from    */
         /* being output by the folder, turn it off, then upon return, check  */
         /* to see if overflow (including too small of a negative integer     */
         /* value) occurred.						      */

         COPY_OPND(temp_opnd, IR_OPND_R(asg_ir_idx));
         expr_desc.rank         = 0;
         xref_state             = CIF_No_Usage_Rec;
         issue_overflow_msg_719 = FALSE;

         if (expr_semantics(&temp_opnd, &expr_desc)) {
            COPY_OPND(IR_OPND_R(asg_ir_idx), temp_opnd);

            if (OPND_FLD(temp_opnd) == CN_Tbl_Idx  &&
                TYP_TYPE(CN_TYPE_IDX(OPND_IDX(temp_opnd))) == Integer) {
            
               /* Get the DO variable's linear type.  If the DO variable is   */
               /* not represented by an Attr, the IL had better be pointing   */
               /* at something like a Dv_Deref IR (pointer).		      */

               do_var_il_idx = IL_IDX(loop_control_il_idx);

               if (IL_FLD(do_var_il_idx) == AT_Tbl_Idx) {
                  do_var_linear_type =
                  (TYP_TYPE(ATD_TYPE_IDX(IL_IDX(do_var_il_idx))) == CRI_Ptr) ?
                     INTEGER_DEFAULT_TYPE :
                     TYP_LINEAR(ATD_TYPE_IDX(IL_IDX(do_var_il_idx)));
               }
               else {
                  do_var_linear_type =
                     TYP_LINEAR(IR_TYPE_IDX(IL_IDX(do_var_il_idx)));
               }


               /* The final value might be bigger than the largest value that */
               /* can be held in an integer with the kind type parameter of   */
               /* the DO variable.					      */
 
               switch (do_var_linear_type) {

                  case Integer_1:
                     max_int_idx = cvrt_str_to_cn(HUGE_INT1_F90,
                                                  do_var_linear_type);
                     break;
  
                  case Integer_2:
                     max_int_idx = cvrt_str_to_cn(HUGE_INT2_F90,
                                                  do_var_linear_type);
                     break;
     
                  case Integer_4:
                     max_int_idx = cvrt_str_to_cn(HUGE_INT4_F90,
                                                  do_var_linear_type);
                     break;
     
                  case Integer_8:
                     max_int_idx = cvrt_str_to_cn(HUGE_INT8_F90,
                                                  do_var_linear_type);
               }
     
               if (compare_cn_and_value(IL_IDX(inc_il_idx), 0, Lt_Opr)) {

                  if (folder_driver( (char *) &CN_CONST(max_int_idx),
                                     do_var_linear_type,
                                     NULL,
                                     NULL_IDX,
                                     folded_const,
                                    &do_var_linear_type,
                                     IR_LINE_NUM(ir_idx),
                                     IR_COL_NUM(ir_idx),
                                     1,
                                     Uminus_Opr)) {
                     max_int_idx =  ntr_const_tbl(do_var_linear_type,
                                                  FALSE,
                                                  folded_const);
                  }
               }

               if ((compare_cn_and_value(IL_IDX(inc_il_idx), 0, Gt_Opr) &&
                    fold_relationals(OPND_IDX(temp_opnd),
                                     max_int_idx, Gt_Opr))  ||
                   (compare_cn_and_value(IL_IDX(inc_il_idx), 0, Lt_Opr) &&
                    fold_relationals(OPND_IDX(temp_opnd),
                                     max_int_idx, Lt_Opr))) {
                  PRINTMSG(SH_GLB_LINE(do_sh_idx), 1083, Warning,
                           SH_COL_NUM(do_sh_idx));
               }
            }
         }
         else {

            if (need_to_issue_719) {
               PRINTMSG(SH_GLB_LINE(do_sh_idx), 1083, Warning,
                        SH_COL_NUM(do_sh_idx));
               need_to_issue_719 = FALSE;


               /* The magnitude of the final value of the DO variable is too  */
               /* large for the target machine.  Hide the start value in a    */
               /* static temp so PDGCS won't attempt to fold the tree (and    */
               /* also produce a compile time overflow).                      */

               gen_sh(After, Data_Stmt, stmt_start_line, stmt_start_col,
                      FALSE, FALSE, TRUE);

               NTR_IR_TBL(init_ir_idx);
               SH_IR_IDX(curr_stmt_sh_idx) = init_ir_idx;
               IR_OPR(init_ir_idx)         = Init_Opr;
               IR_TYPE_IDX(init_ir_idx)    = TYPELESS_DEFAULT_TYPE;
               IR_LINE_NUM(init_ir_idx)    = stmt_start_line;
               IR_COL_NUM(init_ir_idx)     = stmt_start_col;

               tmp_idx = gen_compiler_tmp(stmt_start_line, stmt_start_col,
                                          Shared, TRUE);
               AT_SEMANTICS_DONE(tmp_idx) = TRUE;
               ATD_TYPE_IDX(tmp_idx)      = CN_TYPE_IDX(IL_IDX(start_il_idx));
               ATD_SAVED(tmp_idx)         = TRUE;
               ATD_DATA_INIT(tmp_idx)     = TRUE;
               ATD_STOR_BLK_IDX(tmp_idx) = SCP_SB_STATIC_INIT_IDX(curr_scp_idx);
               
               IR_LINE_NUM_L(init_ir_idx) = stmt_start_line;
               IR_COL_NUM_L(init_ir_idx)  = stmt_start_col;
               IR_FLD_L(init_ir_idx)      = AT_Tbl_Idx;
               IR_IDX_L(init_ir_idx)      = tmp_idx;

               NTR_IR_LIST_TBL(il_idx);
               COPY_OPND(IL_OPND(il_idx), IR_OPND_L(OPND_IDX(temp_opnd)));
               IR_LIST_CNT_R(init_ir_idx) = 1;
               IR_FLD_R(init_ir_idx)      = IL_Tbl_Idx;
               IR_IDX_R(init_ir_idx)      = il_idx;

               NTR_IR_LIST_TBL(il_idx_2);
               IL_NEXT_LIST_IDX(il_idx)   = il_idx_2;
               IL_PREV_LIST_IDX(il_idx_2) = il_idx;
               ++IR_LIST_CNT_R(init_ir_idx);
               IL_FLD(il_idx_2)           = CN_Tbl_Idx;
               IL_IDX(il_idx_2)           = CN_INTEGER_ONE_IDX;
               IL_LINE_NUM(il_idx_2)      = stmt_start_line;
               IL_COL_NUM(il_idx_2)       = stmt_start_col;
               il_idx                     = il_idx_2;

               NTR_IR_LIST_TBL(il_idx_2);
               IL_NEXT_LIST_IDX(il_idx)   = il_idx_2;
               IL_PREV_LIST_IDX(il_idx_2) = il_idx;
               ++IR_LIST_CNT_R(init_ir_idx);
               IL_FLD(il_idx_2)           = CN_Tbl_Idx;
               IL_IDX(il_idx_2)           = CN_INTEGER_ZERO_IDX;
               IL_LINE_NUM(il_idx_2)      = stmt_start_line;
               IL_COL_NUM(il_idx_2)       = stmt_start_col;

               IR_FLD_L(OPND_IDX(temp_opnd)) = AT_Tbl_Idx;
               IR_IDX_L(OPND_IDX(temp_opnd)) = tmp_idx;
               IR_LINE_NUM_L(OPND_IDX(temp_opnd)) = stmt_start_line;
               IR_COL_NUM_L(OPND_IDX(temp_opnd))  = stmt_start_col;
            }
            else {
               PRINTMSG(SH_GLB_LINE(curr_stmt_sh_idx), 224, Internal, 0);
            }
         }

         issue_overflow_msg_719 = TRUE;

# endif                              	/* End long section that is not done  */
					/* if the DO loop form is high-level. */

         break;


      /* -------------------------------------------------------------------- */
      /*								      */
      /*                    DO [label] [,] WHILE (expr)			      */
      /*								      */
      /* -------------------------------------------------------------------- */

      case Do_While_Stmt:

# ifdef _HIGH_LEVEL_DO_LOOP_FORM

         loop_end_sh_idx = curr_stmt_sh_idx;

         il_idx = IL_NEXT_LIST_IDX(loop_labels_il_idx);
         COPY_OPND(temp_opnd, IL_OPND(il_idx));

         /* Insert an assignment stmt ahead of the Loop_End (CG CONTINUE) stmt*/
         /* that ends the DO loop to capture the loop control expression in a */
         /* temp.  We need to do this (just as we did at the head of the loop)*/
         /* for the case where the expression contains a function reference.  */

         curr_stmt_sh_idx = SH_PREV_IDX(curr_stmt_sh_idx);

         gen_sh(After,
                Assignment_Stmt,
                SH_GLB_LINE(do_sh_idx),
                SH_COL_NUM(do_sh_idx),
                FALSE,                          /* Error flag.                */
                FALSE,                          /* Labeled.                   */
                TRUE);                          /* Compiler-generated.        */

         NTR_IR_TBL(asg_ir_idx);
         IR_OPR(asg_ir_idx)      = Asg_Opr;
         IR_TYPE_IDX(asg_ir_idx) = LOGICAL_DEFAULT_TYPE;
         COPY_OPND(IR_OPND_L(asg_ir_idx), IL_OPND(IL_IDX(loop_control_il_idx)));
         IR_LINE_NUM(asg_ir_idx) = IR_LINE_NUM_L(asg_ir_idx);
         IR_COL_NUM(asg_ir_idx)  = IR_COL_NUM_L(asg_ir_idx);

         SH_IR_IDX(curr_stmt_sh_idx) = asg_ir_idx;

         expr_desc.rank  = 0;
         xref_state     = CIF_No_Usage_Rec;

         if (! expr_semantics(&temp_opnd, &expr_desc)) {
            PRINTMSG(SH_GLB_LINE(loop_end_sh_idx), 224, Internal, 0);
         } 

         COPY_OPND(IR_OPND_R(asg_ir_idx), temp_opnd);
         curr_stmt_sh_idx = loop_end_sh_idx;

# else

         /* Generate    IF (scalar-logical-expr) GO TO top-lbl		      */

         gen_sh(Before, If_Stmt, stmt_start_line, stmt_start_col,
                FALSE, FALSE, TRUE);

         /* Send the expression through expr_semantics to get types, etc.     */
         /* propagated.  Temporarily reset curr_stmt_sh_idx to point at the   */
         /* IF SH so that any IR generated to represent the expression is     */
         /* inserted ahead of the IF SH.				      */

         tmp_idx          = curr_stmt_sh_idx;
         curr_stmt_sh_idx = SH_PREV_IDX(curr_stmt_sh_idx);

         COPY_OPND(temp_opnd, IL_OPND(IL_IDX(loop_control_il_idx)));
         expr_desc.rank  = 0;
         xref_state      = CIF_No_Usage_Rec;
         defer_stmt_expansion = TRUE;

         if (! expr_semantics(&temp_opnd, &expr_desc)) {
            PRINTMSG(SH_GLB_LINE(curr_stmt_sh_idx), 224, Internal, 0);
         }

         defer_stmt_expansion = FALSE;

         if (tree_produces_dealloc(&temp_opnd)) {
            /* make logical tmp asg */
            save_curr_stmt_sh_idx = curr_stmt_sh_idx;
            find_opnd_line_and_column(&temp_opnd,
                              &opnd_line, &opnd_column);

            GEN_COMPILER_TMP_ASG(asg_idx,
                                 tmp_idx2,
                                 TRUE,       /* Semantics done */
                                 opnd_line,
                                 opnd_column,
                                 expr_desc.type_idx,
                                 Priv);

            gen_sh(Before, Assignment_Stmt, opnd_line,
                   opnd_column, FALSE, FALSE, TRUE);

            curr_stmt_sh_idx = SH_PREV_IDX(curr_stmt_sh_idx);

            SH_IR_IDX(curr_stmt_sh_idx)     = asg_idx;
            SH_P2_SKIP_ME(curr_stmt_sh_idx) = TRUE;

            process_deferred_functions(&temp_opnd);
            COPY_OPND(IR_OPND_R(asg_idx), temp_opnd);

            OPND_FLD(temp_opnd)        = AT_Tbl_Idx;
            OPND_IDX(temp_opnd)        = tmp_idx2;
            OPND_LINE_NUM(temp_opnd)   = opnd_line;
            OPND_COL_NUM(temp_opnd)    = opnd_column;
            curr_stmt_sh_idx           = save_curr_stmt_sh_idx;
         }
         else {
            process_deferred_functions(&temp_opnd);
         }

         NTR_IR_TBL(ir_idx);
         IR_OPR(ir_idx)              = Br_True_Opr;
         IR_TYPE_IDX(ir_idx)         = LOGICAL_DEFAULT_TYPE;
         IR_LINE_NUM(ir_idx)         = stmt_start_line;
         IR_COL_NUM(ir_idx)          = stmt_start_col;
         COPY_OPND(IR_OPND_L(ir_idx), temp_opnd);
         IR_LINE_NUM_R(ir_idx)       = stmt_start_line;
         IR_COL_NUM_R(ir_idx)        = stmt_start_col;
         IR_FLD_R(ir_idx)            = AT_Tbl_Idx;
         IR_IDX_R(ir_idx)            = IL_IDX(IL_IDX(loop_labels_il_idx));    

         SH_IR_IDX(curr_stmt_sh_idx) = ir_idx;

         AT_REFERENCED(IL_IDX(IL_IDX(loop_labels_il_idx))) = Referenced;

         curr_stmt_sh_idx = tmp_idx;

# endif

         break;


      /* -------------------------------------------------------------------- */
      /*								      */
      /*                             DO [label]				      */
      /*								      */
      /* -------------------------------------------------------------------- */

      case Do_Infinite_Stmt:

         /* Generate a GO TO to branch back to the top-of-loop label.         */

         NTR_IR_TBL(ir_idx);
         IR_OPR(ir_idx)        = Br_Uncond_Opr;
         IR_TYPE_IDX(ir_idx)   = TYPELESS_DEFAULT_TYPE;
         IR_LINE_NUM(ir_idx)   = stmt_start_line;
         IR_COL_NUM(ir_idx)    = stmt_start_col;
         IR_LINE_NUM_R(ir_idx) = stmt_start_line;
         IR_COL_NUM_R(ir_idx)  = stmt_start_col;
         IR_FLD_R(ir_idx)      = AT_Tbl_Idx;
         IR_IDX_R(ir_idx)      = IL_IDX(IL_IDX(loop_labels_il_idx));

         AT_REFERENCED(IL_IDX(IL_IDX(loop_labels_il_idx))) = Referenced;

         gen_sh(Before, Goto_Stmt, stmt_start_line, stmt_start_col,
                FALSE, FALSE, TRUE);

         SH_IR_IDX(SH_PREV_IDX(curr_stmt_sh_idx)) = ir_idx;

   }  /* End switch on DO stmt type */

     
EXIT:

   TRACE (Func_Exit, "gen_loop_end_ir", NULL);

   return;

}  /* gen_loop_end_ir */


/******************************************************************************\
|*									      *|
|* Description:								      *|
|*	Create the tmp array for allocate/deallocate calls.                   *|
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

int	create_alloc_descriptor(int	count,
				int	line,
				int	col,
                                boolean	shared_heap)

{
   int		asg_idx;
   int		bd_idx;
   int		second_cn_idx;
   int		list_idx;
   int		subscript_idx;
   long_type	the_constant;
   long_type	version[2];
   int		tmp_idx;
   int		type_idx;


   TRACE (Func_Entry, "create_alloc_descriptor", NULL);

# if (defined(_TARGET_OS_IRIX) || defined(_TARGET_OS_LINUX) || defined(_TARGET_OS_DARWIN))
   type_idx = SA_INTEGER_DEFAULT_TYPE;
# else
   type_idx = CG_INTEGER_DEFAULT_TYPE;
# endif

   tmp_idx			= gen_compiler_tmp(line, col, Priv, TRUE);
   AT_SEMANTICS_DONE(tmp_idx)	= TRUE;
   ATD_TYPE_IDX(tmp_idx)	= type_idx;
   ATD_STOR_BLK_IDX(tmp_idx)	= SCP_SB_STACK_IDX(curr_scp_idx);

   bd_idx			= reserve_array_ntry(1);
   BD_RANK(bd_idx)		= 1;
   BD_ARRAY_CLASS(bd_idx)	= Explicit_Shape;
   BD_ARRAY_SIZE(bd_idx)	= Constant_Size;
   BD_LINE_NUM(bd_idx)		= line;
   BD_COLUMN_NUM(bd_idx)	= col;
   BD_RESOLVED(bd_idx)		= TRUE;

   the_constant			= 1 + count;

# if (defined(_TARGET_OS_IRIX) || defined(_TARGET_OS_LINUX) || defined(_TARGET_OS_DARWIN))
   /* the version/count item is always 64 bits */
   if (TYP_LINEAR(type_idx) == Integer_4) {
      the_constant++;
   }
# endif

   BD_LEN_FLD(bd_idx)		= CN_Tbl_Idx;
   BD_LEN_IDX(bd_idx)		= C_INT_TO_CN(CG_INTEGER_DEFAULT_TYPE,
                                              the_constant);

   BD_LB_FLD(bd_idx, 1)		= CN_Tbl_Idx;
   BD_LB_IDX(bd_idx, 1)		= CN_INTEGER_ONE_IDX;

   BD_UB_FLD(bd_idx, 1)		= CN_Tbl_Idx;
   BD_UB_IDX(bd_idx, 1)		= BD_LEN_IDX(bd_idx);

   BD_XT_FLD(bd_idx, 1)		= CN_Tbl_Idx;
   BD_XT_IDX(bd_idx, 1)		= BD_LEN_IDX(bd_idx);

   BD_SM_FLD(bd_idx, 1)		= CN_Tbl_Idx;
   BD_SM_IDX(bd_idx, 1)		= CN_INTEGER_ONE_IDX;

   ATD_ARRAY_IDX(tmp_idx)	= ntr_array_in_bd_tbl(bd_idx);

   /* fill in first word of tmp array */
   /* holds version and count */

   NTR_IR_TBL(asg_idx);
   IR_OPR(asg_idx)	= Asg_Opr;
   IR_TYPE_IDX(asg_idx) = type_idx;
   IR_LINE_NUM(asg_idx) = line;
   IR_COL_NUM(asg_idx)  = col;
   IR_FLD_R(asg_idx)    = CN_Tbl_Idx;
   IR_IDX_R(asg_idx)    = gen_alloc_header_const(type_idx,
                                                 count,
                                                 shared_heap,
                                                 &second_cn_idx);
   IR_LINE_NUM_R(asg_idx) = line;
   IR_COL_NUM_R(asg_idx)  = col;

   NTR_IR_TBL(subscript_idx);
   IR_OPR(subscript_idx) = Subscript_Opr;
   IR_TYPE_IDX(subscript_idx) = type_idx;
   IR_LINE_NUM(subscript_idx) = line;
   IR_COL_NUM(subscript_idx)  = col;
   IR_FLD_L(subscript_idx)    = AT_Tbl_Idx;
   IR_IDX_L(subscript_idx)    = tmp_idx;
   IR_LINE_NUM_L(subscript_idx) = line;
   IR_COL_NUM_L(subscript_idx)  = col;

   NTR_IR_LIST_TBL(list_idx);
   IR_FLD_R(subscript_idx)    = IL_Tbl_Idx;
   IR_LIST_CNT_R(subscript_idx) = 1;
   IR_IDX_R(subscript_idx)      = list_idx;
   IL_FLD(list_idx)             = CN_Tbl_Idx;
   IL_IDX(list_idx)             = CN_INTEGER_ONE_IDX;
   IL_LINE_NUM(list_idx)        = line;
   IL_COL_NUM(list_idx)         = col;

   IR_FLD_L(asg_idx)            = IR_Tbl_Idx;
   IR_IDX_L(asg_idx)            = subscript_idx;

   gen_sh(Before, Assignment_Stmt, line, col, FALSE, FALSE, TRUE);
   SH_IR_IDX(SH_PREV_IDX(curr_stmt_sh_idx)) = asg_idx;
   SH_P2_SKIP_ME(SH_PREV_IDX(curr_stmt_sh_idx)) = TRUE;

# if (defined(_TARGET_OS_IRIX) || defined(_TARGET_OS_LINUX) || defined(_TARGET_OS_DARWIN))
   if (TYP_LINEAR(type_idx) == Integer_4) {
      NTR_IR_TBL(asg_idx);
      IR_OPR(asg_idx) = Asg_Opr;
      IR_TYPE_IDX(asg_idx) = type_idx;
      IR_LINE_NUM(asg_idx) = line;
      IR_COL_NUM(asg_idx)  = col;
      NTR_IR_TBL(subscript_idx);
      IR_OPR(subscript_idx) = Subscript_Opr;
      IR_TYPE_IDX(subscript_idx) = type_idx;
      IR_LINE_NUM(subscript_idx) = line;
      IR_COL_NUM(subscript_idx)  = col;
      IR_FLD_L(subscript_idx)    = AT_Tbl_Idx;
      IR_IDX_L(subscript_idx)    = tmp_idx;
      IR_LINE_NUM_L(subscript_idx) = line;
      IR_COL_NUM_L(subscript_idx)  = col;

      NTR_IR_LIST_TBL(list_idx);
      IR_FLD_R(subscript_idx)      = IL_Tbl_Idx;
      IR_LIST_CNT_R(subscript_idx) = 1;
      IR_IDX_R(subscript_idx)      = list_idx;
      IL_FLD(list_idx)             = CN_Tbl_Idx;

      IL_IDX(list_idx)             = CN_INTEGER_TWO_IDX;
      IL_LINE_NUM(list_idx)        = line;
      IL_COL_NUM(list_idx)         = col;

      IR_FLD_L(asg_idx)            = IR_Tbl_Idx;
      IR_IDX_L(asg_idx)            = subscript_idx;

# ifdef _DEBUG
      if (second_cn_idx == NULL_IDX) {
         PRINTMSG(line, 626, Internal, col,
                  "second_cn_idx", "create_alloc_descriptor");
      }
# endif

      IR_FLD_R(asg_idx)            = CN_Tbl_Idx;
      IR_IDX_R(asg_idx)            = second_cn_idx;
      IR_LINE_NUM_R(asg_idx) = line;
      IR_COL_NUM_R(asg_idx)  = col;

      gen_sh(Before, Assignment_Stmt, line, col, FALSE, FALSE, TRUE);
      SH_IR_IDX(SH_PREV_IDX(curr_stmt_sh_idx)) = asg_idx;
      SH_P2_SKIP_ME(SH_PREV_IDX(curr_stmt_sh_idx)) = TRUE;
   }
# endif


   TRACE (Func_Exit, "create_alloc_descriptor", NULL);

   return(tmp_idx);

}  /* create_alloc_descriptor */

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

int gen_alloc_header_const(int		type_idx,
			   int          count,
                           boolean      shared_heap,
                           int		*second_cn_idx)

{
   int          cn_idx;
   long_type    version[2];


typedef struct AllocHead {
        unsigned int    version :8;     /* contains ALLOC_VERSION */
        unsigned int            :24;    /* unused */
        unsigned int            :15;     /* unused */
        unsigned int    imalloc :1;     /* call special malloc */
        unsigned int    icount  :16;    /* size of struct alloclist in */
                                        /* words. */
} AllocHeadType;

   AllocHeadType	*allochdr;

   TRACE (Func_Entry, "gen_alloc_header_const", NULL);

   /* make sure count is 16 bits */
   count = count & 0xFFFF;

   version[0] = 0;
   version[1] = 0;

   allochdr = (AllocHeadType *)version;
   
   allochdr->version = 1;
   allochdr->icount = count;

   if (shared_heap) {
      allochdr->imalloc = 1;
   }


   if (TYP_LINEAR(type_idx) == Integer_4) {
      /* OSP_467, #1, sizeof(long_type) == 64 on TARGET64 */
      int *p_version = (int *) version;
      cn_idx = ntr_const_tbl(type_idx,
                             FALSE,
                             (long_type *)p_version);

      *second_cn_idx = ntr_const_tbl(type_idx,
                                     FALSE,
                                     (long_type *)(p_version+1));
   }
   else {
      *second_cn_idx = NULL_IDX;
      cn_idx = ntr_const_tbl(type_idx,
                             FALSE,
                             version);
   }

   TRACE (Func_Exit, "gen_alloc_header_const", NULL);

   return(cn_idx);

}  /* gen_alloc_header_const */

/******************************************************************************\
|*									      *|
|* Description:								      *|
|*	Create the tmp array for allocate/deallocate calls.                   *|
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

void	set_directives_on_label(int	label_attr)

{
   int		idx;
   int		il_idx;
   int		il_idx2;
   int		new_idx;
   int		save_free_list;


   TRACE (Func_Entry, "set_directives_on_label", NULL);

   ATL_ALIGN(label_attr)	= cdir_switches.align;
   ATL_BL(label_attr)		= cdir_switches.bl;           /* Toggle */
   ATL_CNCALL(label_attr)	= cdir_switches.cncall;
   ATL_CONCURRENT(label_attr)	= cdir_switches.concurrent;
   ATL_IVDEP(label_attr)	= cdir_switches.ivdep;
   ATL_MAXCPUS(label_attr)	= cdir_switches.maxcpus;
   ATL_NEXTSCALAR(label_attr)	= cdir_switches.nextscalar;
   ATL_NOVSEARCH(label_attr)	= ! cdir_switches.vsearch;    /* Toggle */
   ATL_PERMUTATION(label_attr)	= cdir_switches.permutation;
   ATL_PREFERSTREAM(label_attr)	= cdir_switches.preferstream;
   ATL_PREFERSTREAM_NOCINV(label_attr)	= cdir_switches.preferstream_nocinv;
   ATL_PREFERTASK(label_attr)	= cdir_switches.prefertask;
   ATL_PREFERVECTOR(label_attr)	= cdir_switches.prefervector;
   ATL_NORECURRENCE(label_attr)	= ! cdir_switches.recurrence; /* Toggle */
   ATL_SHORTLOOP(label_attr)	= cdir_switches.shortloop;
   ATL_SHORTLOOP128(label_attr)	= cdir_switches.shortloop128;
   ATL_SPLIT(label_attr)	= cdir_switches.split;

   ATL_AGGRESSIVEINNERLOOPFISSION(label_attr)	= 
                                  cdir_switches.aggressiveinnerloopfission;
   ATL_FISSIONABLE(label_attr)	= cdir_switches.fissionable;
   ATL_FUSABLE(label_attr)	= cdir_switches.fusable;
   ATL_FUSION(label_attr)	= opt_flags.fusion;
   ATL_NOFISSION(label_attr)	= cdir_switches.nofission;
   ATL_NOFUSION(label_attr)	= cdir_switches.nofusion;
   ATL_NOINTERCHANGE(label_attr)= cdir_switches.nointerchange;
   ATL_NOBLOCKING(label_attr)	= cdir_switches.noblocking;

   if (! cdir_switches.vector) {
      ATL_NOVECTOR(label_attr)	= TRUE;
   }

   if (cdir_switches.stream) {
      ATL_STREAM(label_attr)	= TRUE;
   }

   if (cdir_switches.pattern) {
      ATL_PATTERN(label_attr)	= TRUE;
   }

# if (defined(_TARGET_OS_IRIX) || defined(_TARGET_OS_LINUX) || defined(_TARGET_OS_DARWIN))
   if (cdir_switches.notask_region) {
      ATL_NOTASK(label_attr)	= TRUE;
   }
# else
   if (! cdir_switches.task) {
      ATL_NOTASK(label_attr)	= TRUE;
   }
# endif

   /* Insure that these directive lists are consecutive. */

   /* ATL_DIRECTIVE_LIST is set as follows:              */
   /* ATL_DIRECTIVE_LIST holds an il_idx which describes */
   /* the subsequent dir list.  The list is accessed     */
   /* by the directive_label_type enum.   The first IL   */
   /* entry is waht holds the size of the list.          */

   save_free_list			= IL_NEXT_LIST_IDX(NULL_IDX);
   IL_NEXT_LIST_IDX(NULL_IDX)		= NULL_IDX;
   NTR_IR_LIST_TBL(il_idx);
   ATL_DIRECTIVE_LIST(label_attr)	= il_idx;
   IL_LIST_CNT(il_idx)			= Num_Dir_On_List;
   IL_FLD(il_idx)			= IL_Tbl_Idx;
   NTR_IR_LIST_TBL(new_idx);
   IL_IDX(il_idx)			= new_idx;  /* List start */
   IL_LINE_NUM(new_idx)			= AT_DEF_LINE(label_attr);
   IL_COL_NUM(new_idx)			= AT_DEF_COLUMN(label_attr);
   il_idx				= new_idx;

   for (idx = 1; idx < Num_Dir_On_List; idx++) {
      NTR_IR_LIST_TBL(new_idx);
      IL_NEXT_LIST_IDX(il_idx)  = new_idx;
      IL_PREV_LIST_IDX(new_idx) = il_idx;
      IL_LINE_NUM(new_idx)	= AT_DEF_LINE(label_attr);
      IL_COL_NUM(new_idx)	= AT_DEF_COLUMN(label_attr);
      il_idx                    = new_idx;
   }

   IL_NEXT_LIST_IDX(NULL_IDX)	= save_free_list;

   if (cdir_switches.safevl_idx != NULL_IDX) {
      il_idx		= IL_IDX(ATL_DIRECTIVE_LIST(label_attr))+Safevl_Dir_Idx;
      IL_FLD(il_idx)	= CN_Tbl_Idx;
      IL_IDX(il_idx)	= cdir_switches.safevl_idx;
   }

   if (cdir_switches.concurrent_idx != NULL_IDX) {
      il_idx		= IL_IDX(ATL_DIRECTIVE_LIST(label_attr)) +
                                 Concurrent_Dir_Idx;
      IL_FLD(il_idx)	= CN_Tbl_Idx;
      IL_IDX(il_idx)	= cdir_switches.concurrent_idx;
   }

   if (cdir_switches.maxcpus) {
      il_idx	= IL_IDX(ATL_DIRECTIVE_LIST(label_attr)) + Maxcpus_Dir_Idx;
      COPY_OPND(IL_OPND(il_idx), cdir_switches.maxcpus_opnd);
   }

   if (cdir_switches.mark) {
      il_idx		= IL_IDX(ATL_DIRECTIVE_LIST(label_attr)) + Mark_Dir_Idx;
      IL_FLD(il_idx)	= CN_Tbl_Idx;
      IL_IDX(il_idx)	= (cdir_switches.mark_dir_idx == NULL_IDX) ?
                                         cdir_switches.mark_cmdline_idx :
                                         cdir_switches.mark_dir_idx;
   }
      
   if (cdir_switches.cache_bypass_ir_idx != NULL_IDX) {
      il_idx	= IL_IDX(ATL_DIRECTIVE_LIST(label_attr)) + Cache_Bypass_Dir_Idx;
      IL_FLD(il_idx)	  = IR_FLD_L(cdir_switches.cache_bypass_ir_idx);
      IL_IDX(il_idx)	  = IR_IDX_L(cdir_switches.cache_bypass_ir_idx);
      IL_LIST_CNT(il_idx) = IR_LIST_CNT_L(cdir_switches.cache_bypass_ir_idx);
   }

   /* ATL_UNROLL_DIR is set TRUE if either a UNROLL directive or a */
   /* NOUNROLL directive is seen for this loop.                    */

   ATL_UNROLL_DIR(label_attr)	= cdir_switches.unroll_dir ||
                                  (opt_flags.unroll_lvl == Unroll_Lvl_2);
                                        

   if (cdir_switches.unroll_count_idx != NULL_IDX) {
      il_idx	= IL_IDX(ATL_DIRECTIVE_LIST(label_attr)) + Unroll_Dir_Idx;
      IL_FLD(il_idx)	= CN_Tbl_Idx;
      IL_IDX(il_idx)	= cdir_switches.unroll_count_idx;
   }

   /* 0 means optimizer sets unroll count.  1 means no unrolling.  If  */
   /* the default level is set to 2, then automatic unrolling happens. */
   /* If the default level is set to 1, we only unroll those loops     */
   /* for which the user specifies the UNROLL directive.               */

   cdir_switches.unroll_dir	  = FALSE;
   cdir_switches.unroll_count_idx = (opt_flags.unroll_lvl == Unroll_Lvl_2) ?
                                     CN_INTEGER_ZERO_IDX : CN_INTEGER_ONE_IDX;

   if (cdir_switches.interchange_count > 0) {
      il_idx	= IL_IDX(ATL_DIRECTIVE_LIST(label_attr)) + Interchange_Dir_Idx;
      IL_FLD(il_idx)	= CN_Tbl_Idx;
      IL_IDX(il_idx)	= C_INT_TO_CN(CG_INTEGER_DEFAULT_TYPE,
                                      cdir_switches.interchange_group);

      il_idx		= IL_IDX(ATL_DIRECTIVE_LIST(label_attr)) + 
                                               Interchange_Level_Dir_Idx;
      IL_FLD(il_idx)	= CN_Tbl_Idx;
      IL_IDX(il_idx)	= C_INT_TO_CN(CG_INTEGER_DEFAULT_TYPE,
                                      cdir_switches.interchange_level);
      --cdir_switches.interchange_count;
   }

   if (cdir_switches.blockable_count > 0) {
      il_idx	= IL_IDX(ATL_DIRECTIVE_LIST(label_attr)) + Blockable_Dir_Idx;
      IL_FLD(il_idx)	= CN_Tbl_Idx;
      IL_IDX(il_idx)	= C_INT_TO_CN(CG_INTEGER_DEFAULT_TYPE,
                                      cdir_switches.blockable_group);
      --cdir_switches.blockable_count;
   }
      
   /* reset cdir switches for one loop only directives. */

   clear_cdir_switches();

   TRACE (Func_Exit, "set_directives_on_label", NULL);

   return;

}  /* set_directives_on_label */

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

static void clear_cdir_switches(void)

{


   TRACE (Func_Entry, "clear_cdir_switches", NULL);

   /* reset cdir switches for one loop only directives. */

   cdir_switches.align                  = FALSE;
   cdir_switches.cache_bypass_ir_idx    = NULL_IDX;
   cdir_switches.concurrent             = FALSE;
   cdir_switches.concurrent_idx         = NULL_IDX;
   cdir_switches.cncall                 = FALSE;
   cdir_switches.ivdep                  = FALSE;
   cdir_switches.maxcpus                = FALSE;
   cdir_switches.nextscalar             = FALSE;
   cdir_switches.permutation            = FALSE;
   cdir_switches.preferstream           = FALSE;
   cdir_switches.preferstream_nocinv    = FALSE;
   cdir_switches.prefertask             = FALSE;
   cdir_switches.prefervector           = FALSE;
   cdir_switches.safevl_idx             = const_safevl_idx;
   cdir_switches.shortloop              = FALSE;
   cdir_switches.shortloop128           = FALSE;
   cdir_switches.split                  = (opt_flags.split_lvl == Split_Lvl_2);

   cdir_switches.aggressiveinnerloopfission     = FALSE;
   cdir_switches.fissionable                    = FALSE;
   cdir_switches.fusable                        = FALSE;
   cdir_switches.nofission                      = FALSE;
   cdir_switches.nofusion                       = FALSE;
   cdir_switches.nointerchange                  = opt_flags.nointerchange;
   cdir_switches.noblocking                     = FALSE;

   cdir_switches.doacross_sh_idx		= NULL_IDX;
   cdir_switches.paralleldo_sh_idx		= NULL_IDX;
   cdir_switches.pdo_sh_idx			= NULL_IDX;


   TRACE (Func_Exit, "clear_cdir_switches", NULL);

   return;

}  /* clear_cdir_switches */

/******************************************************************************\
|*                                                                            *|
|* Description:                                                               *|
|*      Short circuit high level if stmts if a Present_Opr is present.        *|
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

static void short_circuit_high_level_if(void)

{
   opnd_type    cn_opnd;
   int          col;
   int          cond_ir_idx;
   opnd_type    cond_opnd;
   opnd_type    first_opnd;
   int          if_idx;
   int          ir_idx;
   int          line;
   int          not_cnt;
   int          not_idx;
   opnd_type    opnd;
   int          save_curr_stmt_sh_idx;
   opnd_type    second_opnd;
   long_type    the_constant[MAX_WORDS_FOR_INTEGER];
   int          tmp_idx;


   TRACE (Func_Entry, "short_circuit_high_level_if", NULL);

# ifdef _DEBUG
   if (IR_OPR(SH_IR_IDX(curr_stmt_sh_idx)) != If_Opr &&
       IR_OPR(SH_IR_IDX(curr_stmt_sh_idx)) != Br_True_Opr) {
      PRINTMSG(SH_GLB_LINE(curr_stmt_sh_idx), 626, Internal,
               SH_COL_NUM(curr_stmt_sh_idx),
               "If_Opr", "short_circuit_high_level_if");
   }
# endif

   save_curr_stmt_sh_idx = curr_stmt_sh_idx;

   cond_ir_idx = SH_IR_IDX(curr_stmt_sh_idx);

   COPY_OPND(cond_opnd, IR_OPND_L(cond_ir_idx));
   COPY_OPND(opnd, IR_OPND_L(cond_ir_idx));

   find_opnd_line_and_column(&cond_opnd, &line, &col);

   not_cnt = 0;

   while (OPND_FLD(opnd) == IR_Tbl_Idx &&
          (IR_OPR(OPND_IDX(opnd)) == Not_Opr ||
           IR_OPR(OPND_IDX(opnd)) == Paren_Opr)) {

      if (IR_OPR(OPND_IDX(opnd)) == Not_Opr) {
         not_cnt++;
      }

      COPY_OPND(opnd, IR_OPND_L(OPND_IDX(opnd)));
   }

   if (not_cnt%2 == 0) {
      COPY_OPND(cond_opnd, opnd);
      COPY_OPND(IR_OPND_L(cond_ir_idx), cond_opnd);
   }
   else if (not_cnt > 1) {
      NTR_IR_TBL(not_idx);
      IR_OPR(not_idx) = Not_Opr;
      IR_TYPE_IDX(not_idx) = LOGICAL_DEFAULT_TYPE;
      IR_LINE_NUM(not_idx) = line;
      IR_COL_NUM(not_idx)  = col;
      COPY_OPND(IR_OPND_L(not_idx), opnd);
      OPND_FLD(cond_opnd) = IR_Tbl_Idx;
      OPND_IDX(cond_opnd) = not_idx;
      COPY_OPND(IR_OPND_L(cond_ir_idx), cond_opnd);
   }

   if (OPND_FLD(opnd) == IR_Tbl_Idx &&
       (IR_OPR(OPND_IDX(opnd)) == And_Opr ||
        IR_OPR(OPND_IDX(opnd)) == Or_Opr) &&
       (IR_SHORT_CIRCUIT_L(OPND_IDX(opnd)) ||
        IR_SHORT_CIRCUIT_R(OPND_IDX(opnd)) ||
        opt_flags.short_circuit_lvl == Short_Circuit_Left_Right)) {

      if (not_cnt%2 == 0) {
         /* nots cancel out */
         /* intentionally blank */
      }
      else {
         /* demorgan it */

         /* switch and/or */

         if (IR_OPR(OPND_IDX(opnd)) == And_Opr) {
            IR_OPR(OPND_IDX(opnd)) = Or_Opr;
         }
         else {
            IR_OPR(OPND_IDX(opnd)) = And_Opr;
         }

         /* negate the opnds */

         NTR_IR_TBL(not_idx);
         IR_OPR(not_idx) = Not_Opr;
         IR_TYPE_IDX(not_idx) = LOGICAL_DEFAULT_TYPE;
         IR_LINE_NUM(not_idx) = line;
         IR_COL_NUM(not_idx)  = col;
         COPY_OPND(IR_OPND_L(not_idx), IR_OPND_L(OPND_IDX(opnd)));
         IR_FLD_L(OPND_IDX(opnd)) = IR_Tbl_Idx;
         IR_IDX_L(OPND_IDX(opnd)) = not_idx;

         NTR_IR_TBL(not_idx);
         IR_OPR(not_idx) = Not_Opr;
         IR_TYPE_IDX(not_idx) = LOGICAL_DEFAULT_TYPE;
         IR_LINE_NUM(not_idx) = line;
         IR_COL_NUM(not_idx)  = col;
         COPY_OPND(IR_OPND_L(not_idx), IR_OPND_R(OPND_IDX(opnd)));
         IR_FLD_R(OPND_IDX(opnd)) = IR_Tbl_Idx;
         IR_IDX_R(OPND_IDX(opnd)) = not_idx;
      }

      /* now opnd holds the top of the conditional tree */

      GEN_COMPILER_TMP_ASG(ir_idx,
                           tmp_idx,
                           TRUE,       /* Semantics done */
                           line,
                           col,
                           LOGICAL_DEFAULT_TYPE,
                           Priv);

      gen_sh(Before, Assignment_Stmt, line, col,
             FALSE, FALSE, TRUE);
      SH_IR_IDX(SH_PREV_IDX(curr_stmt_sh_idx)) = ir_idx;
      SH_P2_SKIP_ME(SH_PREV_IDX(curr_stmt_sh_idx)) = TRUE;

      if (opt_flags.short_circuit_lvl == Short_Circuit_Functions &&
          IR_SHORT_CIRCUIT_L(OPND_IDX(opnd)) &&
          ! IR_SHORT_CIRCUIT_R(OPND_IDX(opnd))) {

         COPY_OPND(first_opnd, IR_OPND_R(OPND_IDX(opnd)));
         COPY_OPND(second_opnd, IR_OPND_L(OPND_IDX(opnd)));
      }
      else {
         COPY_OPND(first_opnd, IR_OPND_L(OPND_IDX(opnd)));
         COPY_OPND(second_opnd, IR_OPND_R(OPND_IDX(opnd)));
      }

      if (IR_OPR(OPND_IDX(opnd)) == And_Opr) {
         IR_FLD_R(ir_idx) = CN_Tbl_Idx;
         IR_IDX_R(ir_idx) = set_up_logical_constant(the_constant, 
                                                    CG_LOGICAL_DEFAULT_TYPE, 
                                                    TRUE_VALUE,
                                                    TRUE);
         IR_LINE_NUM_R(ir_idx) = line;
         IR_COL_NUM_R(ir_idx) = col;

         OPND_FLD(cn_opnd) = CN_Tbl_Idx;
         OPND_LINE_NUM(cn_opnd) = line;
         OPND_COL_NUM(cn_opnd) = col;
         OPND_IDX(cn_opnd) = set_up_logical_constant(the_constant, 
                                                     CG_LOGICAL_DEFAULT_TYPE, 
                                                     FALSE_VALUE,
                                                     TRUE);
         /* negate the opnds */

         NTR_IR_TBL(not_idx);
         IR_OPR(not_idx) = Not_Opr;
         IR_TYPE_IDX(not_idx) = LOGICAL_DEFAULT_TYPE;
         IR_LINE_NUM(not_idx) = line;
         IR_COL_NUM(not_idx)  = col;
         COPY_OPND(IR_OPND_L(not_idx), first_opnd);
         OPND_FLD(first_opnd) = IR_Tbl_Idx;
         OPND_IDX(first_opnd) = not_idx;

         NTR_IR_TBL(not_idx);
         IR_OPR(not_idx) = Not_Opr;
         IR_TYPE_IDX(not_idx) = LOGICAL_DEFAULT_TYPE;
         IR_LINE_NUM(not_idx) = line;
         IR_COL_NUM(not_idx)  = col;
         COPY_OPND(IR_OPND_L(not_idx), second_opnd);
         OPND_FLD(second_opnd) = IR_Tbl_Idx;
         OPND_IDX(second_opnd) = not_idx;

      }
      else {
         IR_FLD_R(ir_idx) = CN_Tbl_Idx;
         IR_IDX_R(ir_idx) = set_up_logical_constant(the_constant, 
                                                    CG_LOGICAL_DEFAULT_TYPE, 
                                                    FALSE_VALUE,
                                                    TRUE);
         IR_LINE_NUM_R(ir_idx) = line;
         IR_COL_NUM_R(ir_idx) = col;

         OPND_FLD(cn_opnd) = CN_Tbl_Idx;
         OPND_LINE_NUM(cn_opnd) = line;
         OPND_COL_NUM(cn_opnd) = col;
         OPND_IDX(cn_opnd) = set_up_logical_constant(the_constant, 
                                                     CG_LOGICAL_DEFAULT_TYPE, 
                                                     TRUE_VALUE,
                                                     TRUE);
      }

      /* gen IF (first_opnd) Before */

      NTR_IR_TBL(if_idx);
      IR_OPR(if_idx) = If_Opr;
      IR_TYPE_IDX(if_idx) = LOGICAL_DEFAULT_TYPE;
      IR_LINE_NUM(if_idx) = line;
      IR_COL_NUM(if_idx) = col;

      COPY_OPND(IR_OPND_L(if_idx), first_opnd);

      gen_sh(Before, If_Stmt, line, col,
             FALSE, FALSE, TRUE);
      SH_IR_IDX(SH_PREV_IDX(curr_stmt_sh_idx)) = if_idx;
      SH_P2_SKIP_ME(SH_PREV_IDX(curr_stmt_sh_idx)) = TRUE;

      /* short circuit IF Before */

      save_curr_stmt_sh_idx = curr_stmt_sh_idx;
      curr_stmt_sh_idx = SH_PREV_IDX(curr_stmt_sh_idx);

      short_circuit_high_level_if();

      curr_stmt_sh_idx = save_curr_stmt_sh_idx;

      /* gen temp = cn_opnd Before */

      NTR_IR_TBL(if_idx);
      IR_OPR(if_idx) = Asg_Opr;
      IR_TYPE_IDX(if_idx) = LOGICAL_DEFAULT_TYPE;
      IR_LINE_NUM(if_idx) = line;
      IR_COL_NUM(if_idx) = col;

      IR_FLD_L(if_idx) = AT_Tbl_Idx;
      IR_IDX_L(if_idx) = tmp_idx;
      IR_LINE_NUM_L(if_idx) = line;
      IR_COL_NUM_L(if_idx) = col;

      COPY_OPND(IR_OPND_R(if_idx), cn_opnd);

      gen_sh(Before, Assignment_Stmt, line, col,
             FALSE, FALSE, TRUE);
      SH_IR_IDX(SH_PREV_IDX(curr_stmt_sh_idx)) = if_idx;
      SH_P2_SKIP_ME(SH_PREV_IDX(curr_stmt_sh_idx)) = TRUE;

      /* gen ELSE stmt Before */

      NTR_IR_TBL(if_idx);
      IR_OPR(if_idx) = Else_Opr;
      IR_TYPE_IDX(if_idx) = LOGICAL_DEFAULT_TYPE;
      IR_LINE_NUM(if_idx) = line;
      IR_COL_NUM(if_idx) = col;

      gen_sh(Before, Else_Stmt, line, col,
             FALSE, FALSE, TRUE);
      SH_IR_IDX(SH_PREV_IDX(curr_stmt_sh_idx)) = if_idx;
      SH_P2_SKIP_ME(SH_PREV_IDX(curr_stmt_sh_idx)) = TRUE;

      /* gen IF (second_opnd) Before */

      NTR_IR_TBL(if_idx);
      IR_OPR(if_idx) = If_Opr;
      IR_TYPE_IDX(if_idx) = LOGICAL_DEFAULT_TYPE;
      IR_LINE_NUM(if_idx) = line;
      IR_COL_NUM(if_idx) = col;

      COPY_OPND(IR_OPND_L(if_idx), second_opnd);

      gen_sh(Before, If_Stmt, line, col,
             FALSE, FALSE, TRUE);
      SH_IR_IDX(SH_PREV_IDX(curr_stmt_sh_idx)) = if_idx;
      SH_P2_SKIP_ME(SH_PREV_IDX(curr_stmt_sh_idx)) = TRUE;

      /* short circuit IF Before */

      save_curr_stmt_sh_idx = curr_stmt_sh_idx;
      curr_stmt_sh_idx = SH_PREV_IDX(curr_stmt_sh_idx);

      short_circuit_high_level_if();

      curr_stmt_sh_idx = save_curr_stmt_sh_idx;

      /* gen temp = cn_opnd Before */

      NTR_IR_TBL(if_idx);
      IR_OPR(if_idx) = Asg_Opr;
      IR_TYPE_IDX(if_idx) = LOGICAL_DEFAULT_TYPE;
      IR_LINE_NUM(if_idx) = line;
      IR_COL_NUM(if_idx) = col;

      IR_FLD_L(if_idx) = AT_Tbl_Idx;
      IR_IDX_L(if_idx) = tmp_idx;
      IR_LINE_NUM_L(if_idx) = line;
      IR_COL_NUM_L(if_idx) = col;

      COPY_OPND(IR_OPND_R(if_idx), cn_opnd);

      gen_sh(Before, Assignment_Stmt, line, col,
             FALSE, FALSE, TRUE);
      SH_IR_IDX(SH_PREV_IDX(curr_stmt_sh_idx)) = if_idx;
      SH_P2_SKIP_ME(SH_PREV_IDX(curr_stmt_sh_idx)) = TRUE;

      /* gen ENDIF stmt Before */

      NTR_IR_TBL(if_idx);
      IR_OPR(if_idx) = Endif_Opr;
      IR_TYPE_IDX(if_idx) = LOGICAL_DEFAULT_TYPE;
      IR_LINE_NUM(if_idx) = line;
      IR_COL_NUM(if_idx) = col;

      gen_sh(Before, End_If_Stmt, line, col,
             FALSE, FALSE, TRUE);
      SH_IR_IDX(SH_PREV_IDX(curr_stmt_sh_idx)) = if_idx;
      SH_P2_SKIP_ME(SH_PREV_IDX(curr_stmt_sh_idx)) = TRUE;

      /* gen ENDIF stmt Before */

      NTR_IR_TBL(if_idx);
      IR_OPR(if_idx) = Endif_Opr;
      IR_TYPE_IDX(if_idx) = LOGICAL_DEFAULT_TYPE;
      IR_LINE_NUM(if_idx) = line;
      IR_COL_NUM(if_idx) = col;

      gen_sh(Before, End_If_Stmt, line, col,
             FALSE, FALSE, TRUE);
      SH_IR_IDX(SH_PREV_IDX(curr_stmt_sh_idx)) = if_idx;
      SH_P2_SKIP_ME(SH_PREV_IDX(curr_stmt_sh_idx)) = TRUE;

      /* replace original condition with temp */

      OPND_FLD(cond_opnd) = AT_Tbl_Idx;
      OPND_IDX(cond_opnd) = tmp_idx;
      OPND_LINE_NUM(cond_opnd) = line;
      OPND_COL_NUM(cond_opnd) = col;

      COPY_OPND(IR_OPND_L(cond_ir_idx), cond_opnd);
   }
   else {

      if (tree_produces_dealloc(&cond_opnd) ||
          io_item_must_flatten) {

         GEN_COMPILER_TMP_ASG(ir_idx,
                              tmp_idx,
                              TRUE,       /* Semantics done */
                              line,
                              col,
                              LOGICAL_DEFAULT_TYPE,
                              Priv);

         gen_sh(Before, Assignment_Stmt, line, col,
                FALSE, FALSE, TRUE);

         curr_stmt_sh_idx = SH_PREV_IDX(curr_stmt_sh_idx);

         SH_IR_IDX(curr_stmt_sh_idx)     = ir_idx;
         SH_P2_SKIP_ME(curr_stmt_sh_idx) = TRUE;

         process_deferred_functions(&cond_opnd);
         COPY_OPND(IR_OPND_R(ir_idx), cond_opnd);

         IR_FLD_L(cond_ir_idx)      = AT_Tbl_Idx;
         IR_IDX_L(cond_ir_idx)      = tmp_idx;
         IR_LINE_NUM_L(cond_ir_idx) = line;
         IR_COL_NUM_L(cond_ir_idx)  = col;
      }
      else {
         process_deferred_functions(&cond_opnd);
         COPY_OPND(IR_OPND_L(cond_ir_idx), cond_opnd);
      }
   }

   curr_stmt_sh_idx = save_curr_stmt_sh_idx;
   TRACE (Func_Exit, "short_circuit_high_level_if", NULL);

   return;

}  /* short_circuit_high_level_if */

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

static boolean check_stat_variable(int          ir_idx,
                                   opnd_type    *stat_opnd,
                                   int          stat_list_idx)

{
   int                  attr_idx;
   int                  col;
   expr_arg_type        exp_desc;
   int                  line;
   int                  loc_idx;
   boolean              ok = TRUE;
   opnd_type            opnd;
   int                  stat_col;
   int                  stat_line;

# if defined(_TARGET_OS_MAX) || defined(_TARGET_OS_SOLARIS) || \
     (defined(_TARGET_OS_IRIX) || defined(_TARGET_OS_LINUX) || defined(_TARGET_OS_DARWIN))
   int                  asg_idx;
   int                  tmp_idx;
# endif


   TRACE (Func_Entry, "check_stat_variable", NULL);

   /* check for call_opr before expr_semantics */
   if (IR_FLD_R(ir_idx) == IR_Tbl_Idx &&
       IR_OPR(IR_IDX_R(ir_idx)) == Call_Opr) {

      /* error .. must catch here to stop misleading messages */
      PRINTMSG(IR_LINE_NUM_L(IR_IDX_R(ir_idx)), 202, Error,
               IR_COL_NUM_L(IR_IDX_R(ir_idx)));
      ok = FALSE;
   }
   else {
      COPY_OPND(opnd, IR_OPND_R(ir_idx));
      exp_desc.rank = 0;
      xref_state    = CIF_Symbol_Modification;
      ok = expr_semantics(&opnd, &exp_desc);
      COPY_OPND(IR_OPND_R(ir_idx), opnd);

      attr_idx = find_base_attr(&opnd, &stat_line, &stat_col);

      if (attr_idx               == NULL_IDX   ||
          AT_OBJ_CLASS(attr_idx) != Data_Obj   ||
          exp_desc.constant                    ||
          exp_desc.type          != Integer    ||
          exp_desc.rank          != 0) {

         /* error 202  in stat variable */
         PRINTMSG(stat_line, 202, Error, stat_col);
         ok = FALSE;
      }

      if (! check_for_legal_define(&opnd)) {
         ok = FALSE;
      }

      *stat_opnd = null_opnd;

      if (ok) {

         if (OPND_FLD(opnd) == IR_Tbl_Idx             &&
             IR_OPR(OPND_IDX(opnd)) == Subscript_Opr) {
            COPY_OPND((*stat_opnd), IR_OPND_L(OPND_IDX(opnd)));
         }
         else {
            COPY_OPND((*stat_opnd), opnd);
         }

         find_opnd_line_and_column(&opnd, &line, &col);

# if defined(_TARGET_OS_MAX) || defined(_TARGET_OS_SOLARIS) || \
     (defined(_TARGET_OS_IRIX) || defined(_TARGET_OS_LINUX) || defined(_TARGET_OS_DARWIN))
# ifdef _TARGET_OS_MAX
         if (exp_desc.linear_type == Integer_1 ||
             exp_desc.linear_type == Integer_2 ||
             exp_desc.linear_type == Integer_4)
# else
         if (exp_desc.linear_type == Integer_8)
# endif
            {
            tmp_idx              = gen_compiler_tmp(line, col, Priv, TRUE);
            AT_SEMANTICS_DONE(tmp_idx)= TRUE;
            ATD_TYPE_IDX(tmp_idx)     = CG_INTEGER_DEFAULT_TYPE;
            ATD_STOR_BLK_IDX(tmp_idx) = SCP_SB_STACK_IDX(curr_scp_idx);

            NTR_IR_TBL(asg_idx);
            IR_OPR(asg_idx) = Asg_Opr;
            IR_TYPE_IDX(asg_idx) = exp_desc.type_idx;
            IR_LINE_NUM(asg_idx) = line;
            IR_COL_NUM(asg_idx)  = col;
            COPY_OPND(IR_OPND_L(asg_idx), opnd);
            IR_FLD_R(asg_idx) = AT_Tbl_Idx;
            IR_IDX_R(asg_idx) = tmp_idx;
            IR_LINE_NUM_R(asg_idx) = line;
            IR_COL_NUM_R(asg_idx)  = col;

            gen_sh(After, Assignment_Stmt, line, col, FALSE, FALSE, TRUE);
            SH_IR_IDX(curr_stmt_sh_idx) = asg_idx;
            SH_P2_SKIP_ME(curr_stmt_sh_idx) = TRUE;
            curr_stmt_sh_idx = SH_PREV_IDX(curr_stmt_sh_idx);

            OPND_FLD(opnd) = AT_Tbl_Idx;
            OPND_IDX(opnd) = tmp_idx;
            OPND_LINE_NUM(opnd) = line;
            OPND_COL_NUM(opnd)  = col;

         }
# endif


         NTR_IR_TBL(loc_idx);
         IR_OPR(loc_idx) = Aloc_Opr;
         IR_TYPE_IDX(loc_idx) = CRI_Ptr_8;
         IR_LINE_NUM(loc_idx) = line;
         IR_COL_NUM(loc_idx)  = col;
         IL_FLD(stat_list_idx) = IR_Tbl_Idx;
         IL_IDX(stat_list_idx) = loc_idx;
         IL_LINE_NUM(stat_list_idx) = line;
         IL_COL_NUM(stat_list_idx)  = col;

         COPY_OPND(IR_OPND_L(loc_idx), opnd);
      }
   }

   TRACE (Func_Exit, "check_stat_variable", NULL);

   return(ok);

}  /* check_stat_variable */

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

static void asg_opnd_to_tmp(int                 tmp_idx,
                            opnd_type           *opnd,
                            int                 line,
                            int                 col,
                            sh_position_type    position)

{
   int          asg_idx;

   TRACE (Func_Entry, "asg_opnd_to_tmp", NULL);

   NTR_IR_TBL(asg_idx);
   IR_OPR(asg_idx) = Asg_Opr;
   IR_TYPE_IDX(asg_idx) = ATD_TYPE_IDX(tmp_idx);
   IR_LINE_NUM(asg_idx) = line;
   IR_COL_NUM(asg_idx)  = col;
   IR_FLD_L(asg_idx) = AT_Tbl_Idx;
   IR_IDX_L(asg_idx) = tmp_idx;
   IR_LINE_NUM_L(asg_idx) = line;
   IR_COL_NUM_L(asg_idx)  = col;

   COPY_OPND(IR_OPND_R(asg_idx), (*opnd));

   gen_sh(position, Assignment_Stmt, line, col, FALSE, FALSE, TRUE);

   if (position == Before) {
      SH_IR_IDX(SH_PREV_IDX(curr_stmt_sh_idx)) = asg_idx;
      SH_P2_SKIP_ME(SH_PREV_IDX(curr_stmt_sh_idx)) = TRUE;
   }
   else {
      SH_IR_IDX(curr_stmt_sh_idx) = asg_idx;
      SH_P2_SKIP_ME(curr_stmt_sh_idx) = TRUE;
   }


   TRACE (Func_Exit, "asg_opnd_to_tmp", NULL);

   return;

}  /* asg_opnd_to_tmp */

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

static void gen_Dv_Set_stmt(opnd_type          *dope_opnd,
                            operator_type       opr,
                            int                 ir_dv_dim,
                            opnd_type           *opnd,
                            sh_position_type    position)

{
   int          col;
   int          dv_idx;
   int          line;


   TRACE (Func_Entry, "gen_Dv_Set_stmt", NULL);

   find_opnd_line_and_column(dope_opnd, &line, &col);

   NTR_IR_TBL(dv_idx);
   IR_OPR(dv_idx) = opr;
   IR_TYPE_IDX(dv_idx) = CG_INTEGER_DEFAULT_TYPE;
   IR_LINE_NUM(dv_idx) = line;
   IR_COL_NUM(dv_idx)  = col;
   COPY_OPND(IR_OPND_L(dv_idx), (*dope_opnd));
   COPY_OPND(IR_OPND_R(dv_idx), (*opnd));

   if (ir_dv_dim) {
      IR_DV_DIM(dv_idx) = ir_dv_dim;
   }

   gen_sh(position, Assignment_Stmt, line, col, FALSE, FALSE, TRUE);

   if (position == Before) {
      SH_IR_IDX(SH_PREV_IDX(curr_stmt_sh_idx)) = dv_idx;
      SH_P2_SKIP_ME(SH_PREV_IDX(curr_stmt_sh_idx)) = TRUE;
   }
   else {
      SH_IR_IDX(curr_stmt_sh_idx) = dv_idx;
      SH_P2_SKIP_ME(curr_stmt_sh_idx) = TRUE;
   }


   TRACE (Func_Exit, "gen_Dv_Set_stmt", NULL);

   return;

}  /* gen_Dv_Set_stmt */

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

void set_up_allocate_as_call(int                ir_idx,
                             int                attr_idx,
                             int                stat_list_idx,
                             boolean            shared_heap)


{
   int                  asg_idx;
   int                  call_idx;
   int                  col;
   int                  line;
   int                  list_idx;
   int                  list_idx2;
   int                  loc_idx;
   int                  subscript_idx;
   int                  tmp_array_idx;
   long_type            the_constant;


   TRACE (Func_Entry, "set_up_allocate_as_call", NULL);

   line = IR_LINE_NUM(ir_idx);
   col = IR_COL_NUM(ir_idx);
   tmp_array_idx = create_alloc_descriptor(IR_LIST_CNT_L(ir_idx),
                                           line,
                                           col,
                                           shared_heap);

   list_idx = IR_IDX_L(ir_idx);
   the_constant = 2;

# if (defined(_TARGET_OS_IRIX) || defined(_TARGET_OS_LINUX) || defined(_TARGET_OS_DARWIN))
   if (TYP_LINEAR(ATD_TYPE_IDX(tmp_array_idx)) == Integer_4) {
      the_constant++;
   }
# endif

   while (list_idx) {
      /* put loc of dope vector into tmp_array */

      NTR_IR_TBL(asg_idx);
      IR_OPR(asg_idx) = Asg_Opr;
      IR_TYPE_IDX(asg_idx) = ATD_TYPE_IDX(tmp_array_idx);
      IR_LINE_NUM(asg_idx) = line;
      IR_COL_NUM(asg_idx)  = col;

      COPY_OPND(IR_OPND_R(asg_idx), IL_OPND(list_idx));

      NTR_IR_TBL(subscript_idx);
      IR_OPR(subscript_idx) = Subscript_Opr;
      IR_TYPE_IDX(subscript_idx) = ATD_TYPE_IDX(tmp_array_idx);
      IR_LINE_NUM(subscript_idx) = line;
      IR_COL_NUM(subscript_idx)  = col;
      IR_FLD_L(subscript_idx) = AT_Tbl_Idx;
      IR_IDX_L(subscript_idx) = tmp_array_idx;
      IR_LINE_NUM_L(subscript_idx) = line;
      IR_COL_NUM_L(subscript_idx)  = col;

      IR_FLD_L(asg_idx) = IR_Tbl_Idx;
      IR_IDX_L(asg_idx) = subscript_idx;

      NTR_IR_LIST_TBL(list_idx2);
      IL_FLD(list_idx2) = CN_Tbl_Idx;
      IL_IDX(list_idx2) = C_INT_TO_CN(CG_INTEGER_DEFAULT_TYPE, the_constant);
      IL_LINE_NUM(list_idx2) = line;
      IL_COL_NUM(list_idx2)  = col;

      IR_FLD_R(subscript_idx) = IL_Tbl_Idx;
      IR_LIST_CNT_R(subscript_idx) = 1;
      IR_IDX_R(subscript_idx) = list_idx2;

      gen_sh(Before, Assignment_Stmt, line, col, FALSE, FALSE, TRUE);
      SH_IR_IDX(SH_PREV_IDX(curr_stmt_sh_idx)) = asg_idx;
      SH_P2_SKIP_ME(SH_PREV_IDX(curr_stmt_sh_idx)) = TRUE;

      list_idx = IL_NEXT_LIST_IDX(list_idx);
      the_constant++;
   }

   NTR_IR_TBL(call_idx);
   IR_OPR(call_idx)             = Call_Opr;
   IR_TYPE_IDX(call_idx)        = TYPELESS_DEFAULT_TYPE;
   IR_LINE_NUM(call_idx)        = line;
   IR_COL_NUM(call_idx)         = col;
   IR_FLD_L(call_idx)           = AT_Tbl_Idx;
   IR_LINE_NUM_L(call_idx)      = line;
   IR_COL_NUM_L(call_idx)       = col;
   IR_IDX_L(call_idx)           = attr_idx;
   IR_FLD_R(call_idx)           = IL_Tbl_Idx;
   IR_LIST_CNT_R(call_idx)      = 2;
   NTR_IR_LIST_TBL(list_idx);
   IR_IDX_R(call_idx)           = list_idx;

   NTR_IR_TBL(loc_idx);
   IR_OPR(loc_idx)              = Aloc_Opr;
   IR_TYPE_IDX(loc_idx)         = CRI_Ptr_8;
   IR_LINE_NUM(loc_idx)         = line;
   IR_COL_NUM(loc_idx)          = col;
   IR_FLD_L(loc_idx)            = AT_Tbl_Idx;
   IR_IDX_L(loc_idx)            = tmp_array_idx;
   IR_LINE_NUM_L(loc_idx)       = line;
   IR_COL_NUM_L(loc_idx)        = col;
   IL_FLD(list_idx)             = IR_Tbl_Idx;
   IL_IDX(list_idx)             = loc_idx;

   IL_NEXT_LIST_IDX(list_idx)   = stat_list_idx;

   SH_IR_IDX(curr_stmt_sh_idx)  = call_idx;


   TRACE (Func_Exit, "set_up_allocate_as_call", NULL);

   return;

}  /* set_up_allocate_as_call */

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

void gen_split_alloc(int                ir_idx,
                     int                lib_attr_idx,
                     int                stat_list_idx)

{
   int          attr_idx;
   int		cn_idx;
   int          col;
   int          line;
   int          list_idx;
   int          list_idx2 = NULL_IDX;
   int          new_ir_idx;

   TRACE (Func_Entry, "gen_split_alloc", NULL);

   NTR_IR_TBL(new_ir_idx);
   COPY_TBL_NTRY(ir_tbl, new_ir_idx, ir_idx);

   line = IR_LINE_NUM(ir_idx);
   col = IR_COL_NUM(ir_idx);

   IR_IDX_L(new_ir_idx) = NULL_IDX;
   IR_LIST_CNT_L(new_ir_idx) = 0;

   list_idx = IR_IDX_L(ir_idx);

   while (list_idx) {
      attr_idx = find_left_attr(&IL_OPND(list_idx));

      if (!ATD_ALLOCATABLE(attr_idx) ||
          ATD_PE_ARRAY_IDX(attr_idx) == NULL_IDX) {

         if (list_idx == IR_IDX_L(ir_idx)) {
            IR_IDX_L(ir_idx) = IL_NEXT_LIST_IDX(list_idx);
            IL_PREV_LIST_IDX(IL_NEXT_LIST_IDX(list_idx)) = NULL_IDX;
         }
         else {
            IL_NEXT_LIST_IDX(IL_PREV_LIST_IDX(list_idx)) =
                                          IL_NEXT_LIST_IDX(list_idx);
            IL_PREV_LIST_IDX(IL_NEXT_LIST_IDX(list_idx)) =
                                          IL_PREV_LIST_IDX(list_idx);
         }
         IR_LIST_CNT_L(ir_idx)--;

         if (list_idx2 == NULL_IDX) {
            IR_IDX_L(new_ir_idx) = list_idx;
            IL_PREV_LIST_IDX(list_idx) = NULL_IDX;
            IL_NEXT_LIST_IDX(list_idx) = NULL_IDX;
         }
         else {
            IL_NEXT_LIST_IDX(list_idx2) = list_idx;
            IL_PREV_LIST_IDX(list_idx) = list_idx2;
            IL_NEXT_LIST_IDX(list_idx) = NULL_IDX;
         }
         list_idx2 = list_idx;
         IR_LIST_CNT_L(new_ir_idx)++;

      }
      list_idx = IL_NEXT_LIST_IDX(list_idx);
   }

# ifdef _ALLOCATE_IS_CALL
   set_up_allocate_as_call(new_ir_idx,
                           lib_attr_idx,
                           stat_list_idx,
                           FALSE);
# else

   NTR_IR_LIST_TBL(list_idx);
   IR_FLD_R(new_ir_idx) = IL_Tbl_Idx;
   IR_IDX_R(new_ir_idx) = list_idx;
   IR_LIST_CNT_R(new_ir_idx) = 3;

   IL_FLD(list_idx) = AT_Tbl_Idx;
   IL_IDX(list_idx) = lib_attr_idx;
   IL_LINE_NUM(list_idx) = line;
   IL_COL_NUM(list_idx)  = col;

   NTR_IR_LIST_TBL(IL_NEXT_LIST_IDX(list_idx));
   IL_PREV_LIST_IDX(IL_NEXT_LIST_IDX(list_idx)) = list_idx;
   list_idx = IL_NEXT_LIST_IDX(list_idx);

   IL_FLD(list_idx) = CN_Tbl_Idx;
   IL_IDX(list_idx) = gen_alloc_header_const(Integer_8,
                                             IR_LIST_CNT_L(new_ir_idx),
                                             FALSE,
                                             &cn_idx);
   IL_LINE_NUM(list_idx) = line;
   IL_COL_NUM(list_idx)  = col;

   IL_NEXT_LIST_IDX(list_idx) = stat_list_idx;
   IL_PREV_LIST_IDX(stat_list_idx) = list_idx;

# endif


   gen_sh(Before, Allocate_Stmt, line, col, FALSE, FALSE, TRUE);

   SH_IR_IDX(SH_PREV_IDX(curr_stmt_sh_idx)) = new_ir_idx;
   SH_P2_SKIP_ME(SH_PREV_IDX(curr_stmt_sh_idx)) = TRUE;


   TRACE (Func_Exit, "gen_split_alloc", NULL);

   return;

}  /* gen_split_alloc */

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

boolean	is_local_forall_index(int	attr_idx)

{
   int		list_idx;
   boolean	result = FALSE;

   TRACE (Func_Entry, "is_local_forall_index", NULL);

   list_idx = IR_IDX_R(SH_IR_IDX(active_forall_sh_idx));

   while (list_idx &&
          IL_FLD(list_idx) == IL_Tbl_Idx) {

      if (ATD_FORALL_INDEX(attr_idx)) {
         if (attr_idx == AT_ATTR_LINK(IL_IDX(IL_IDX(list_idx)))) {
            result = TRUE;
            break;
         }
      }
      else if (attr_idx == IL_IDX(IL_IDX(list_idx))) {
         result = TRUE;
         break;
      }
          
      list_idx = IL_NEXT_LIST_IDX(list_idx);
   }
   

   TRACE (Func_Exit, "is_local_forall_index", NULL);

   return(result);

}  /* is_local_forall_index */

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

static boolean check_forall_triplet_for_index(opnd_type	*top_opnd)

{
   int		list_idx;
   boolean	ok = TRUE;


   TRACE (Func_Entry, "check_forall_triplet_for_index", NULL);

   switch (OPND_FLD((*top_opnd))) {
   case AT_Tbl_Idx:
      if (AT_OBJ_CLASS(OPND_IDX((*top_opnd))) == Data_Obj &&
          ATD_FORALL_INDEX(OPND_IDX((*top_opnd))) &&
          is_local_forall_index(OPND_IDX((*top_opnd)))) {

         PRINTMSG(OPND_LINE_NUM((*top_opnd)), 1605, Error,
                  OPND_COL_NUM((*top_opnd)));
         ok = FALSE;
      }
      break;

   case IR_Tbl_Idx:
      ok &= check_forall_triplet_for_index(&(IR_OPND_L(
                                        OPND_IDX((*top_opnd)))));
      ok &= check_forall_triplet_for_index(&(IR_OPND_R(
                                        OPND_IDX((*top_opnd)))));
      break;

   case IL_Tbl_Idx:
      list_idx = OPND_IDX((*top_opnd));

      while (list_idx) {
         ok &= check_forall_triplet_for_index(&(IL_OPND(list_idx)));
         list_idx = IL_NEXT_LIST_IDX(list_idx);
      }
      break;

   default:
       break;
   }


   TRACE (Func_Exit, "check_forall_triplet_for_index", NULL);

   return(ok);

}  /* check_forall_triplet_for_index */

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

static boolean gen_forall_max_expr(int		start_list_idx,
                                   opnd_type	*result_opnd)

{

   int			col;
   int			div_idx;
   int			end_list_idx;
   expr_arg_type	exp_desc;
   int			le_idx;
   int			line;
   int			minus_idx;
   boolean		ok = TRUE;
   int			plus_idx;
   int			stride_list_idx;
#ifdef KEY /* Bug 10177 */
   int			type_idx = 0;
#else /* KEY Bug 10177 */
   int			type_idx;
#endif /* KEY Bug 10177 */

   TRACE (Func_Entry, "gen_forall_max_expr", NULL);

   if (IL_FLD(start_list_idx) == CN_Tbl_Idx) {
      type_idx = CN_TYPE_IDX(IL_IDX(start_list_idx));
   }
   else if (IL_FLD(start_list_idx) == AT_Tbl_Idx) {
      type_idx = ATD_TYPE_IDX(IL_IDX(start_list_idx));
   }

   find_opnd_line_and_column(&(IL_OPND(start_list_idx)), &line, &col);

   end_list_idx = IL_NEXT_LIST_IDX(start_list_idx);
   stride_list_idx = IL_NEXT_LIST_IDX(end_list_idx);

   if (IL_FLD(stride_list_idx) == CN_Tbl_Idx &&
       compare_cn_and_value(IL_IDX(stride_list_idx), 0, Eq_Opr)) {

      PRINTMSG(IL_LINE_NUM(stride_list_idx), 1606, Error, 
               IL_COL_NUM(stride_list_idx));
      ok = FALSE;
   }

   minus_idx = gen_ir(IL_FLD(end_list_idx), IL_IDX(end_list_idx),
                 Minus_Opr, type_idx, line, col,
                      IL_FLD(start_list_idx), IL_IDX(start_list_idx));

   plus_idx = gen_ir(IR_Tbl_Idx, minus_idx,
                Plus_Opr, type_idx, line, col,
                     IL_FLD(stride_list_idx), IL_IDX(stride_list_idx));

   div_idx = gen_ir(IR_Tbl_Idx, plus_idx,
                Div_Opr, type_idx, line, col,
                    IL_FLD(stride_list_idx), IL_IDX(stride_list_idx));

   le_idx = gen_ir(IR_Tbl_Idx, div_idx,
               Le_Opr, LOGICAL_DEFAULT_TYPE, line, col,
                   CN_Tbl_Idx, CN_INTEGER_ZERO_IDX);

   OPND_FLD((*result_opnd)) = IR_Tbl_Idx;
   OPND_IDX((*result_opnd)) = le_idx;

   if (ok &&
       IL_FLD(start_list_idx)  == CN_Tbl_Idx &&
       IL_FLD(end_list_idx)    == CN_Tbl_Idx &&
       IL_FLD(stride_list_idx) == CN_Tbl_Idx) {

      exp_desc.rank = 0;
      xref_state    = CIF_No_Usage_Rec;
      ok &= expr_semantics(result_opnd, &exp_desc);
   }


   TRACE (Func_Exit, "gen_forall_max_expr", NULL);

   return(ok);

}  /* gen_forall_max_expr */

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

static void gen_forall_branch_around(opnd_type	*br_around_opnd)

{
   int		br_idx;
   int		col;
   int		label_idx;
   int		line;
   int		save_curr_stmt_sh_idx;

   TRACE (Func_Entry, "gen_forall_branch_around", NULL);

   save_curr_stmt_sh_idx = curr_stmt_sh_idx;

   find_opnd_line_and_column(br_around_opnd, &line, &col);

   label_idx = gen_internal_lbl(line);

   br_idx = gen_ir(OPND_FLD((*br_around_opnd)), OPND_IDX((*br_around_opnd)),
              Br_True_Opr, LOGICAL_DEFAULT_TYPE, line, col,
                   AT_Tbl_Idx, label_idx);

   curr_stmt_sh_idx = active_forall_sh_idx;

   gen_sh(Before, If_Stmt, line, col, FALSE, FALSE, TRUE);
   SH_IR_IDX(SH_PREV_IDX(curr_stmt_sh_idx)) = br_idx;
   SH_P2_SKIP_ME(SH_PREV_IDX(curr_stmt_sh_idx)) = TRUE;

   curr_stmt_sh_idx = IR_IDX_L(SH_IR_IDX(active_forall_sh_idx));

   br_idx = gen_ir(AT_Tbl_Idx, label_idx,
              Label_Opr, TYPELESS_DEFAULT_TYPE, line, col,
                   NO_Tbl_Idx, NULL_IDX);

   gen_sh(After, Continue_Stmt, line, col, FALSE, FALSE, TRUE);
   SH_IR_IDX(curr_stmt_sh_idx) = br_idx;
   SH_P2_SKIP_ME(curr_stmt_sh_idx) = TRUE;

   AT_DEFINED(label_idx) = TRUE;
   ATL_DEF_STMT_IDX(label_idx) = curr_stmt_sh_idx;


   curr_stmt_sh_idx = save_curr_stmt_sh_idx;

   TRACE (Func_Exit, "gen_forall_branch_around", NULL);

   return;

}  /* gen_forall_branch_around */

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

void gen_forall_loops(int	start_body_sh_idx,
                      int	end_body_sh_idx)

{
   opnd_type	end_opnd;
   int		lcv_idx;
   int		list_idx;
   opnd_type	start_opnd;
   opnd_type	stride_opnd;

   TRACE (Func_Entry, "gen_forall_loops", NULL);

   list_idx = IR_IDX_R(SH_IR_IDX(active_forall_sh_idx));

   while (list_idx &&
          IL_FLD(list_idx) == IL_Tbl_Idx) {

      lcv_idx = AT_ATTR_LINK(IL_IDX(IL_IDX(list_idx)));
      COPY_OPND(start_opnd, IL_OPND(IL_NEXT_LIST_IDX(IL_IDX(list_idx))));
      COPY_OPND(end_opnd,  
              IL_OPND(IL_NEXT_LIST_IDX(IL_NEXT_LIST_IDX(IL_IDX(list_idx)))));
      COPY_OPND(stride_opnd, 
              IL_OPND(IL_NEXT_LIST_IDX(IL_NEXT_LIST_IDX(
                               IL_NEXT_LIST_IDX(IL_IDX(list_idx))))));

      create_loop_stmts(lcv_idx, &start_opnd, &end_opnd, &stride_opnd,
                        start_body_sh_idx, 
                        end_body_sh_idx);

      list_idx = IL_NEXT_LIST_IDX(list_idx);
   }

   TRACE (Func_Exit, "gen_forall_loops", NULL);

   return;

}  /* gen_forall_loops */

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

void gen_forall_tmp(expr_arg_type	*exp_desc,
                    opnd_type		*result_opnd,
                    int			line,
                    int			col,
		    boolean		is_pointer)

{
   int			alloc_idx;
   int			base_asg_idx;
   int			base_tmp_idx;
   int			bd_idx;
   boolean		constant_shape;
   int			dealloc_idx;
   int			i;
   int			list_idx;
   int			list_idx2;
   int			list_idx3;
   expr_arg_type	loc_exp_desc;
   int			max_idx;
   int			save_curr_stmt_sh_idx;
   opnd_type		size_opnd;
   int			struct_idx;
   int			sub_idx;
   int			tmp_idx;
   int			triplet_idx;


   TRACE (Func_Entry, "gen_forall_tmp", NULL);

   save_curr_stmt_sh_idx = curr_stmt_sh_idx;
   curr_stmt_sh_idx = active_forall_sh_idx;

   tmp_idx = gen_compiler_tmp(line, col, Priv, TRUE);
   AT_SEMANTICS_DONE(tmp_idx) = TRUE;

   if (is_pointer) {
      ATD_TYPE_IDX(tmp_idx) = gen_forall_derived_type(exp_desc->type_idx,
                                                      exp_desc->rank,
                                                      line, 
                                                      col);
   }
   else {
      ATD_TYPE_IDX(tmp_idx) = exp_desc->type_idx;
   }

   ATD_STOR_BLK_IDX(tmp_idx) = SCP_SB_STACK_IDX(curr_scp_idx);

   if (is_pointer) {
      loc_exp_desc = init_exp_desc;
      loc_exp_desc.type_idx = ATD_TYPE_IDX(tmp_idx);
      loc_exp_desc.type = TYP_TYPE(loc_exp_desc.type_idx);
      loc_exp_desc.linear_type = TYP_LINEAR(loc_exp_desc.type_idx);
      constant_shape = gen_forall_tmp_bd_entry(&loc_exp_desc, 
                                               &bd_idx, line, col);
   }
   else {
      constant_shape = gen_forall_tmp_bd_entry(exp_desc, &bd_idx, line, col);
   }

   ATD_ARRAY_IDX(tmp_idx) = bd_idx;

   if (!constant_shape) {

      ATD_STOR_BLK_IDX(tmp_idx) =  SCP_SB_BASED_IDX(curr_scp_idx);

      /* initialize size_opnd to the number of elements for */
      /* determine_tmp_size.                                */

      gen_opnd(&size_opnd, BD_LEN_IDX(bd_idx), BD_LEN_FLD(bd_idx), line, col);

      /* now for the alloc and dealloc stmts */

      ATD_AUTOMATIC(tmp_idx) = TRUE;

      GEN_COMPILER_TMP_ASG(base_asg_idx,
                           base_tmp_idx,
                           TRUE,        /* Semantics done */
                           line,
                           col,
                           SA_INTEGER_DEFAULT_TYPE,
                           Priv);

      ATD_AUTO_BASE_IDX(tmp_idx)        = base_tmp_idx;

#ifdef KEY
      /* bug#488
	 <exp_desc->type_idx> gives the type that a pointer points to;
	 we need to find the type that holds these pointers.
       */
      determine_tmp_size(&size_opnd, ATD_TYPE_IDX(tmp_idx) );
#else
      determine_tmp_size(&size_opnd, exp_desc->type_idx);
#endif

      NTR_IR_TBL(max_idx);
      IR_OPR(max_idx) = Max_Opr;
      IR_TYPE_IDX(max_idx) = SA_INTEGER_DEFAULT_TYPE;
      IR_LINE_NUM(max_idx) = line;
      IR_COL_NUM(max_idx)  = col;
      IR_FLD_L(max_idx) = IL_Tbl_Idx;
      IR_LIST_CNT_L(max_idx) = 2;

      NTR_IR_LIST_TBL(list_idx);
      IR_IDX_L(max_idx) = list_idx;

      IL_FLD(list_idx) = CN_Tbl_Idx;
      IL_IDX(list_idx) = CN_INTEGER_ZERO_IDX;
      IL_LINE_NUM(list_idx) = line;
      IL_COL_NUM(list_idx)  = col;

      NTR_IR_LIST_TBL(IL_NEXT_LIST_IDX(list_idx));
      IL_PREV_LIST_IDX(IL_NEXT_LIST_IDX(list_idx)) = list_idx;
      list_idx = IL_NEXT_LIST_IDX(list_idx);

      COPY_OPND(IL_OPND(list_idx), size_opnd);

      OPND_FLD(size_opnd) = IR_Tbl_Idx;
      OPND_IDX(size_opnd) = max_idx;


      alloc_idx = gen_ir(OPND_FLD(size_opnd), OPND_IDX(size_opnd),
                     Alloc_Opr, TYPELESS_DEFAULT_TYPE, line, col,
                         NO_Tbl_Idx, NULL_IDX);

      IR_FLD_R(base_asg_idx) = IR_Tbl_Idx;
      IR_IDX_R(base_asg_idx) = alloc_idx;

      gen_sh(Before, Assignment_Stmt, line, col, FALSE, FALSE, TRUE);

      SH_IR_IDX(SH_PREV_IDX(curr_stmt_sh_idx)) = base_asg_idx;
      SH_P2_SKIP_ME(SH_PREV_IDX(curr_stmt_sh_idx)) = TRUE;

      /* The dealloc goes after the end_forall */

      curr_stmt_sh_idx = IR_IDX_L(SH_IR_IDX(active_forall_sh_idx));

      dealloc_idx = gen_ir(IR_FLD_L(base_asg_idx), IR_IDX_L(base_asg_idx),
                       Dealloc_Opr, TYPELESS_DEFAULT_TYPE, line, col,
                           NO_Tbl_Idx, NULL_IDX);

      gen_sh(After, Assignment_Stmt, line, col, FALSE, FALSE, TRUE);

      SH_IR_IDX(curr_stmt_sh_idx) = dealloc_idx;
      SH_P2_SKIP_ME(curr_stmt_sh_idx) = TRUE;

   }

   curr_stmt_sh_idx = save_curr_stmt_sh_idx;

   /* need to set the result_opnd with full array sections   */
   /* for the array syntax dims, and the index variables for */
   /* the remaining dims.                                    */

   NTR_IR_TBL(sub_idx);
   if (is_pointer) {
      IR_OPR(sub_idx) = Subscript_Opr;
   }
   else {
      IR_OPR(sub_idx) = (exp_desc->rank > 0 ? Section_Subscript_Opr :
                                               Subscript_Opr);
      IR_RANK(sub_idx) = exp_desc->rank;
   }

   IR_TYPE_IDX(sub_idx) = exp_desc->type_idx;
   IR_LINE_NUM(sub_idx) = line;
   IR_COL_NUM(sub_idx) = col;

   IR_FLD_L(sub_idx) = AT_Tbl_Idx;
   IR_IDX_L(sub_idx) = tmp_idx;
   IR_LINE_NUM_L(sub_idx) = line;
   IR_COL_NUM_L(sub_idx) = col;

   list_idx2 = NULL_IDX;
   list_idx = IR_IDX_R(SH_IR_IDX(active_forall_sh_idx));

   for (i = 1; i <= BD_RANK(bd_idx); i++) {

      if (list_idx2 == NULL_IDX) {
         NTR_IR_LIST_TBL(list_idx2);
         IR_FLD_R(sub_idx) = IL_Tbl_Idx;
         IR_IDX_R(sub_idx) = list_idx2;
         IR_LIST_CNT_R(sub_idx) = 1;
      }
      else {
         NTR_IR_LIST_TBL(IL_NEXT_LIST_IDX(list_idx2));
         IL_PREV_LIST_IDX(IL_NEXT_LIST_IDX(list_idx2)) = list_idx2;
         list_idx2 = IL_NEXT_LIST_IDX(list_idx2);
         IR_LIST_CNT_R(sub_idx) += 1;
      }

      if (! is_pointer &&
          i <= exp_desc->rank) {
         /* gen a whole section of this dim */

         NTR_IR_TBL(triplet_idx);
         IR_OPR(triplet_idx) = Triplet_Opr;
         IR_RANK(triplet_idx) = 1;
         IR_TYPE_IDX(triplet_idx) = CG_INTEGER_DEFAULT_TYPE;
         IR_LINE_NUM(triplet_idx) = line;
         IR_COL_NUM(triplet_idx) = col;
         IR_FLD_L(triplet_idx) = IL_Tbl_Idx;
         NTR_IR_LIST_TBL(list_idx3);
         IR_IDX_L(triplet_idx) = list_idx3;
         IR_LIST_CNT_L(triplet_idx) = 3;

         IL_FLD(list_idx3) = BD_LB_FLD(bd_idx,i);
         IL_IDX(list_idx3) = BD_LB_IDX(bd_idx,i);
         IL_LINE_NUM(list_idx3) = line;
         IL_COL_NUM(list_idx3) = col;

         NTR_IR_LIST_TBL(IL_NEXT_LIST_IDX(list_idx3));
         IL_PREV_LIST_IDX(IL_NEXT_LIST_IDX(list_idx3)) = list_idx3;
         list_idx3 = IL_NEXT_LIST_IDX(list_idx3);

         IL_FLD(list_idx3) = BD_UB_FLD(bd_idx,i);
         IL_IDX(list_idx3) = BD_UB_IDX(bd_idx,i);
         IL_LINE_NUM(list_idx3) = line;
         IL_COL_NUM(list_idx3) = col;

         NTR_IR_LIST_TBL(IL_NEXT_LIST_IDX(list_idx3));
         IL_PREV_LIST_IDX(IL_NEXT_LIST_IDX(list_idx3)) = list_idx3;
         list_idx3 = IL_NEXT_LIST_IDX(list_idx3);

         IL_FLD(list_idx3) = CN_Tbl_Idx;
         IL_IDX(list_idx3) = CN_INTEGER_ONE_IDX;
         IL_LINE_NUM(list_idx3) = line;
         IL_COL_NUM(list_idx3) = col;

         IL_FLD(list_idx2) = IR_Tbl_Idx;
         IL_IDX(list_idx2) = triplet_idx;
      }
      else {

         /* this is a forall index dim */

         IL_FLD(list_idx2) = AT_Tbl_Idx;
         IL_IDX(list_idx2) = AT_ATTR_LINK(IL_IDX(IL_IDX(list_idx)));
         IL_LINE_NUM(list_idx2) = line;
         IL_COL_NUM(list_idx2) = col;

         list_idx = IL_NEXT_LIST_IDX(list_idx);
      }
   }

   OPND_FLD((*result_opnd)) = IR_Tbl_Idx;
   OPND_IDX((*result_opnd)) = sub_idx;

   if (is_pointer) {
      NTR_IR_TBL(struct_idx);
      IR_OPR(struct_idx) = Struct_Opr;
      IR_TYPE_IDX(struct_idx) = exp_desc->type_idx;
      IR_LINE_NUM(struct_idx) = line;
      IR_COL_NUM(struct_idx) = col;
      COPY_OPND(IR_OPND_L(struct_idx), (*result_opnd));
      IR_FLD_R(struct_idx) = AT_Tbl_Idx;
      IR_IDX_R(struct_idx) = SN_ATTR_IDX(ATT_FIRST_CPNT_IDX(
                                        TYP_IDX(ATD_TYPE_IDX(tmp_idx))));
      IR_LINE_NUM_R(struct_idx) = line;
      IR_COL_NUM_R(struct_idx) = col;

      OPND_FLD((*result_opnd)) = IR_Tbl_Idx;
      OPND_IDX((*result_opnd)) = struct_idx;

      exp_desc->rank = 0;
      xref_state = CIF_No_Usage_Rec;
      expr_semantics(result_opnd, exp_desc);
   }
   else if (exp_desc->type == Character) {
      gen_whole_substring(result_opnd, exp_desc->rank);
   }
   

   TRACE (Func_Exit, "gen_forall_tmp", NULL);

   return;

}  /* gen_forall_tmp */

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

static boolean gen_forall_tmp_bd_entry(expr_arg_type	*exp_desc,
                                       int		*new_bd_idx,
                                       int		line,
                                       int		col)

{
   int			asg_idx;
   int			bd_idx;
   boolean		constant_shape = TRUE;
   expr_arg_type	loc_exp_desc;
   int			i;
   int			list_idx;
   int			list_idx2;
   int			mult_idx;
   opnd_type		num_el_opnd;
   int			plus_idx;
   int			rank;
   opnd_type		sm_opnd;
   size_offset_type     stride;
   int			tmp_idx;
   opnd_type		xt_opnd;


   TRACE (Func_Entry, "gen_forall_tmp_bd_entry", NULL);

   rank = 0;

   list_idx = IR_IDX_R(SH_IR_IDX(active_forall_sh_idx));

   while (list_idx &&
          IL_FLD(list_idx) == IL_Tbl_Idx) {

      rank++;
      list_idx = IL_NEXT_LIST_IDX(list_idx);
   }

   rank += exp_desc->rank;

# ifdef _DEBUG
   if (rank > 7) {
      PRINTMSG(line, 626, Internal, col,
               "rank <= 7", "gen_forall_tmp_bd_entry");
   }
# endif

   bd_idx = reserve_array_ntry(rank);
   BD_RANK(bd_idx)        = rank;
   BD_LINE_NUM(bd_idx)    = line;
   BD_COLUMN_NUM(bd_idx)  = col;
   BD_ARRAY_CLASS(bd_idx) = Explicit_Shape;
   BD_RESOLVED(bd_idx)    = TRUE;

   num_el_opnd = null_opnd;

   /* the first dimensions are from array syntax */

   for (i = 1; i <= exp_desc->rank; i++) {
      BD_LB_FLD(bd_idx,i) = CN_Tbl_Idx;
      BD_LB_IDX(bd_idx,i) = CN_INTEGER_ONE_IDX;

      if (OPND_FLD(exp_desc->shape[i-1]) == CN_Tbl_Idx) {
         BD_UB_FLD(bd_idx,i) = OPND_FLD(exp_desc->shape[i-1]);
         BD_UB_IDX(bd_idx,i) = OPND_IDX(exp_desc->shape[i-1]);
      }
      else {
         constant_shape = FALSE;

         if (OPND_FLD(exp_desc->shape[i-1]) == AT_Tbl_Idx &&
             ATD_CLASS(OPND_IDX(exp_desc->shape[i-1])) == Compiler_Tmp) {

            BD_UB_FLD(bd_idx,i) = OPND_FLD(exp_desc->shape[i-1]);
            BD_UB_IDX(bd_idx,i) = OPND_IDX(exp_desc->shape[i-1]);
         }
         else { /* must do tmp assignments */

            GEN_COMPILER_TMP_ASG(asg_idx,
                                 tmp_idx,
                                 TRUE,    /* Semantics done */
                                 line,
                                 col,
                                 SA_INTEGER_DEFAULT_TYPE,
                                 Priv);

            IR_FLD_R(asg_idx) = OPND_FLD(exp_desc->shape[i-1]);
            IR_IDX_R(asg_idx) = OPND_IDX(exp_desc->shape[i-1]);
            IR_LINE_NUM_R(asg_idx) = line;
            IR_COL_NUM_R(asg_idx)  = col;

            gen_sh(Before, Assignment_Stmt, line,
                            col, FALSE, FALSE, TRUE);

            SH_IR_IDX(SH_PREV_IDX(curr_stmt_sh_idx))      = asg_idx;
            SH_P2_SKIP_ME(SH_PREV_IDX(curr_stmt_sh_idx))  = TRUE;

            gen_copyin_bounds_stmt(tmp_idx);

            BD_UB_FLD(bd_idx, i)          = AT_Tbl_Idx;
            BD_UB_IDX(bd_idx, i)          = tmp_idx;
            OPND_FLD(exp_desc->shape[i-1])        = AT_Tbl_Idx;
            OPND_IDX(exp_desc->shape[i-1])        = tmp_idx;
            SHAPE_FOLDABLE(exp_desc->shape[i-1])  = FALSE;
            SHAPE_WILL_FOLD_LATER(exp_desc->shape[i-1]) = FALSE;
         }
      }

      /* might need max (extent, 0) here  */

      BD_XT_FLD(bd_idx,i) = BD_UB_FLD(bd_idx,i);
      BD_XT_IDX(bd_idx,i) = BD_UB_IDX(bd_idx,i);

      if (OPND_FLD(num_el_opnd) == NO_Tbl_Idx) {
         gen_opnd(&num_el_opnd, BD_XT_IDX(bd_idx,i), BD_XT_FLD(bd_idx,i),
                  line, col);
      }
      else {
         mult_idx = gen_ir(OPND_FLD(num_el_opnd), OPND_IDX(num_el_opnd),
                       Mult_Opr, SA_INTEGER_DEFAULT_TYPE, line, col,
                           BD_XT_FLD(bd_idx,i), BD_XT_IDX(bd_idx,i));

         OPND_IDX(num_el_opnd) = mult_idx;
         OPND_FLD(num_el_opnd) = IR_Tbl_Idx;
      }
   }

   /* the remaining dimensions are the forall indexes */


   list_idx = IR_IDX_R(SH_IR_IDX(active_forall_sh_idx));

   for ( ;i <= rank; i++) {


      if (IL_LIST_CNT(list_idx) == 7) {
         list_idx2 = IL_NEXT_LIST_IDX(IL_IDX(list_idx)); /* start opnd */
         list_idx2 = IL_NEXT_LIST_IDX(list_idx2); /* end opnd */
         list_idx2 = IL_NEXT_LIST_IDX(list_idx2);  /* stride opnd */

         list_idx2 = IL_NEXT_LIST_IDX(list_idx2);  /* LB tmp opnd */
         BD_LB_FLD(bd_idx,i) = IL_FLD(list_idx2);
         BD_LB_IDX(bd_idx,i) = IL_IDX(list_idx2);

         list_idx2 = IL_NEXT_LIST_IDX(list_idx2);  /* UB tmp opnd */
         BD_UB_FLD(bd_idx,i) = IL_FLD(list_idx2);
         BD_UB_IDX(bd_idx,i) = IL_IDX(list_idx2);
        
         list_idx2 = IL_NEXT_LIST_IDX(list_idx2);  /* XT tmp opnd */
         BD_XT_FLD(bd_idx,i) = IL_FLD(list_idx2);
         BD_XT_IDX(bd_idx,i) = IL_IDX(list_idx2);
      }
      else {

         list_idx2 = IL_NEXT_LIST_IDX(IL_IDX(list_idx)); /* start opnd */

         determine_lb_ub(list_idx2, 
                         bd_idx,
                         i);

         list_idx2 = IL_NEXT_LIST_IDX(list_idx2); /* end opnd */

         if (BD_LB_FLD(bd_idx,i) == CN_Tbl_Idx &&
             compare_cn_and_value(BD_LB_IDX(bd_idx,i),
                                  1,
                                  Eq_Opr)) {

            BD_XT_FLD(bd_idx, i) = BD_UB_FLD(bd_idx,i);
            BD_XT_IDX(bd_idx, i) = BD_UB_IDX(bd_idx,i);
         }
         else {
            /* make expression for extent */
            /* upper - lower + 1 */
            plus_idx = gen_ir(IR_Tbl_Idx,
                           gen_ir(BD_UB_FLD(bd_idx,i), BD_UB_IDX(bd_idx,i),
                               Minus_Opr, SA_INTEGER_DEFAULT_TYPE, line, col,
                                  BD_LB_FLD(bd_idx,i), BD_LB_IDX(bd_idx,i)),
                            Plus_Opr, SA_INTEGER_DEFAULT_TYPE, line, col,
                              CN_Tbl_Idx, CN_INTEGER_ONE_IDX);

            gen_opnd(&xt_opnd, plus_idx, IR_Tbl_Idx, line, col);

            if (BD_LB_FLD(bd_idx,i) == CN_Tbl_Idx &&
                BD_UB_FLD(bd_idx,i) == CN_Tbl_Idx) {
               loc_exp_desc.rank = 0;
               xref_state        = CIF_No_Usage_Rec;
               expr_semantics(&xt_opnd, &loc_exp_desc);
            }

            if (OPND_FLD(xt_opnd) != CN_Tbl_Idx &&
                (OPND_FLD(xt_opnd) != AT_Tbl_Idx ||
                 ATD_CLASS(OPND_IDX(xt_opnd)) != Compiler_Tmp)) {
   
               /* must do tmp assignments */

               GEN_COMPILER_TMP_ASG(asg_idx,
                                    tmp_idx,
                                    TRUE,    /* Semantics done */
                                    line,
                                    col,
                                    SA_INTEGER_DEFAULT_TYPE,
                                    Priv);

               IR_FLD_R(asg_idx) = OPND_FLD(xt_opnd);
               IR_IDX_R(asg_idx) = OPND_IDX(xt_opnd);
               IR_LINE_NUM_R(asg_idx) = line;
               IR_COL_NUM_R(asg_idx)  = col;
   
               gen_sh(Before, Assignment_Stmt, line,
                               col, FALSE, FALSE, TRUE);

               SH_IR_IDX(SH_PREV_IDX(curr_stmt_sh_idx))      = asg_idx;
               SH_P2_SKIP_ME(SH_PREV_IDX(curr_stmt_sh_idx))  = TRUE;

               gen_copyin_bounds_stmt(tmp_idx);

               OPND_FLD(xt_opnd) = AT_Tbl_Idx;
               OPND_IDX(xt_opnd) = tmp_idx;
            }

            BD_XT_FLD(bd_idx, i) = OPND_FLD(xt_opnd);
            BD_XT_IDX(bd_idx, i) = OPND_IDX(xt_opnd);
         }

         list_idx2 = IL_NEXT_LIST_IDX(list_idx2);  /* stride opnd */
         NTR_IR_LIST_TBL(IL_NEXT_LIST_IDX(list_idx2));
         IL_PREV_LIST_IDX(IL_NEXT_LIST_IDX(list_idx2)) = list_idx2;

         list_idx2 = IL_NEXT_LIST_IDX(list_idx2);  /* LB tmp opnd */

         gen_opnd(&IL_OPND(list_idx2), BD_LB_IDX(bd_idx,i), BD_LB_FLD(bd_idx,i),
                  line, col);


         NTR_IR_LIST_TBL(IL_NEXT_LIST_IDX(list_idx2));
         IL_PREV_LIST_IDX(IL_NEXT_LIST_IDX(list_idx2)) = list_idx2;

         list_idx2 = IL_NEXT_LIST_IDX(list_idx2);  /* UB tmp opnd */

         gen_opnd(&IL_OPND(list_idx2), BD_UB_IDX(bd_idx,i), BD_UB_FLD(bd_idx,i),
                  line, col);

         NTR_IR_LIST_TBL(IL_NEXT_LIST_IDX(list_idx2));
         IL_PREV_LIST_IDX(IL_NEXT_LIST_IDX(list_idx2)) = list_idx2;

         list_idx2 = IL_NEXT_LIST_IDX(list_idx2);  /* XT tmp opnd */
         gen_opnd(&IL_OPND(list_idx2), BD_XT_IDX(bd_idx,i), BD_XT_FLD(bd_idx,i),
                  line, col);

         IL_LIST_CNT(list_idx) = 7;
      }

      if (OPND_FLD(num_el_opnd) == NO_Tbl_Idx) {
         gen_opnd(&num_el_opnd, BD_XT_IDX(bd_idx,i), BD_XT_FLD(bd_idx,i),
                  line, col);
      }
      else {
         mult_idx = gen_ir(OPND_FLD(num_el_opnd), OPND_IDX(num_el_opnd),
                       Mult_Opr, SA_INTEGER_DEFAULT_TYPE, line, col,
                           BD_XT_FLD(bd_idx,i), BD_XT_IDX(bd_idx,i));

         OPND_IDX(num_el_opnd) = mult_idx;
         OPND_FLD(num_el_opnd) = IR_Tbl_Idx;
      }

      if (BD_LB_FLD(bd_idx,i) != CN_Tbl_Idx) {
         constant_shape = FALSE;
      }

      if (BD_UB_FLD(bd_idx,i) != CN_Tbl_Idx) {
         constant_shape = FALSE;
      }

      list_idx = IL_NEXT_LIST_IDX(list_idx);
   }
   
   /* someone needs to validate_char_len */

   if (exp_desc->type == Character &&
       TYP_FLD(exp_desc->type_idx) != CN_Tbl_Idx) {
      constant_shape = FALSE;
   }

   loc_exp_desc.rank = 0;
   xref_state        = CIF_No_Usage_Rec;

   expr_semantics(&num_el_opnd, &loc_exp_desc);

   if (OPND_FLD(num_el_opnd) == CN_Tbl_Idx) {
      BD_LEN_FLD(bd_idx)     = CN_Tbl_Idx;
      BD_LEN_IDX(bd_idx)     = OPND_IDX(num_el_opnd);
   }
   else if (OPND_FLD(num_el_opnd)            == AT_Tbl_Idx    &&
            ATD_CLASS(OPND_IDX(num_el_opnd)) == Compiler_Tmp) {
      BD_LEN_FLD(bd_idx)     = AT_Tbl_Idx;
      BD_LEN_IDX(bd_idx)     = OPND_IDX(num_el_opnd);
   }
   else { /* tmp assign the num_elements */

      GEN_COMPILER_TMP_ASG(asg_idx,
                           tmp_idx,
                           TRUE,     /* Semantics done */
                           line,
                           col,
                           loc_exp_desc.type_idx,
                           Priv);

      COPY_OPND(IR_OPND_R(asg_idx), num_el_opnd);
      gen_sh(Before, Assignment_Stmt, line,
                      col, FALSE, FALSE, TRUE);

      SH_IR_IDX(SH_PREV_IDX(curr_stmt_sh_idx))       = asg_idx;
      SH_P2_SKIP_ME(SH_PREV_IDX(curr_stmt_sh_idx))   = TRUE;

      BD_LEN_FLD(bd_idx) = AT_Tbl_Idx;
      BD_LEN_IDX(bd_idx) = tmp_idx;
   }

   if (constant_shape) {
      BD_ARRAY_SIZE(bd_idx)  = Constant_Size;
   }
   else {
      BD_ARRAY_SIZE(bd_idx)  = Var_Len_Array;
   }

   set_stride_for_first_dim(exp_desc->type_idx, &stride);

   BD_SM_FLD(bd_idx, 1) = stride.fld;
   BD_SM_IDX(bd_idx, 1) = stride.idx;

   for (i = 2; i <= BD_RANK(bd_idx); i++) {
      mult_idx = gen_ir(BD_SM_FLD(bd_idx, i - 1), BD_SM_IDX(bd_idx, i - 1),
                   Mult_Opr, SA_INTEGER_DEFAULT_TYPE, line, col,
                        BD_XT_FLD(bd_idx, i - 1), BD_XT_IDX(bd_idx, i - 1));

      OPND_FLD(sm_opnd)          = IR_Tbl_Idx;
      OPND_IDX(sm_opnd)          = mult_idx;

      loc_exp_desc.rank = 0;
      xref_state        = CIF_No_Usage_Rec;

      expr_semantics(&sm_opnd, &loc_exp_desc);

      if (loc_exp_desc.constant) {
         BD_SM_FLD(bd_idx, i) = CN_Tbl_Idx;
         BD_SM_IDX(bd_idx, i) = OPND_IDX(sm_opnd);
      }
      else if (OPND_FLD(sm_opnd)            == AT_Tbl_Idx    &&
               ATD_CLASS(OPND_IDX(sm_opnd)) == Compiler_Tmp) {
         BD_SM_FLD(bd_idx, i) = AT_Tbl_Idx;
         BD_SM_IDX(bd_idx, i) = OPND_IDX(sm_opnd);
      }
     else {

         GEN_COMPILER_TMP_ASG(asg_idx,
                              tmp_idx,
                              TRUE,  /* Semantics done */
                              line,
                              col,
                              loc_exp_desc.type_idx,
                              Priv);

         COPY_OPND(IR_OPND_R(asg_idx), sm_opnd);
         gen_sh(Before, Assignment_Stmt, line,
                         col, FALSE, FALSE, TRUE);

         SH_IR_IDX(SH_PREV_IDX(curr_stmt_sh_idx))     = asg_idx;
         SH_P2_SKIP_ME(SH_PREV_IDX(curr_stmt_sh_idx)) = TRUE;

         BD_SM_FLD(bd_idx, i) = AT_Tbl_Idx;
         BD_SM_IDX(bd_idx, i) = tmp_idx;
      }
   }

   BD_FLOW_DEPENDENT(bd_idx) = TRUE;

   *new_bd_idx = ntr_array_in_bd_tbl(bd_idx);

   TRACE (Func_Exit, "gen_forall_tmp_bd_entry", NULL);

   return(constant_shape);

}  /* gen_forall_tmp_bd_entry */

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

static void determine_lb_ub(int	start_list_idx,
			    int	bd_idx,
			    int	idx)

{
   int		asg_idx;
   int		col;
   int		else_idx;
   int		end_list_idx;
   int		gt_idx;
   int		if_idx;
   int		line;
   int		stride_list_idx;
   int		tmp_idx;
   int		type_idx;

# if defined(_HIGH_LEVEL_IF_FORM)
   int		else_sh_idx;
   int		endif_idx;
   int		if_sh_idx;
# else
   int		label1;
   int		label2;
# endif


   TRACE (Func_Entry, "determine_lb_ub", NULL);

   /* if start <= end => lb=start, ub=end */
   /* else if end < start => lb=end, ub=start */

   /* if not both constant, and stride is constant, then assume direction */

   line = BD_LINE_NUM(bd_idx);
   col = BD_COLUMN_NUM(bd_idx);

   end_list_idx = IL_NEXT_LIST_IDX(start_list_idx);
   stride_list_idx = IL_NEXT_LIST_IDX(end_list_idx);

   if (IL_FLD(start_list_idx) == CN_Tbl_Idx &&
       IL_FLD(end_list_idx) == CN_Tbl_Idx) {

      if (fold_relationals(IL_IDX(start_list_idx),
                           IL_IDX(end_list_idx),
                           Le_Opr)) {

         BD_LB_FLD(bd_idx,idx) = IL_FLD(start_list_idx);
         BD_LB_IDX(bd_idx,idx) = IL_IDX(start_list_idx);

         BD_UB_FLD(bd_idx,idx) = IL_FLD(end_list_idx);
         BD_UB_IDX(bd_idx,idx) = IL_IDX(end_list_idx);
      }
      else {
         BD_LB_FLD(bd_idx,idx) = IL_FLD(end_list_idx);
         BD_LB_IDX(bd_idx,idx) = IL_IDX(end_list_idx);

         BD_UB_FLD(bd_idx,idx) = IL_FLD(start_list_idx);
         BD_UB_IDX(bd_idx,idx) = IL_IDX(start_list_idx);
      }
   }
   else if (IL_FLD(stride_list_idx) == CN_Tbl_Idx) {

      if (compare_cn_and_value(IL_IDX(stride_list_idx),
                               0,
                               Gt_Opr)) {

         BD_LB_FLD(bd_idx,idx) = IL_FLD(start_list_idx);
         BD_LB_IDX(bd_idx,idx) = IL_IDX(start_list_idx);

         BD_UB_FLD(bd_idx,idx) = IL_FLD(end_list_idx);
         BD_UB_IDX(bd_idx,idx) = IL_IDX(end_list_idx);
      }
      else {
         BD_LB_FLD(bd_idx,idx) = IL_FLD(end_list_idx);
         BD_LB_IDX(bd_idx,idx) = IL_IDX(end_list_idx);

         BD_UB_FLD(bd_idx,idx) = IL_FLD(start_list_idx);
         BD_UB_IDX(bd_idx,idx) = IL_IDX(start_list_idx);
      }
   }
   else {
      tmp_idx = gen_compiler_tmp(line, col, Priv, TRUE);

      BD_LB_FLD(bd_idx,idx) = AT_Tbl_Idx;
      BD_LB_IDX(bd_idx,idx) = tmp_idx;

      type_idx = (IL_FLD(start_list_idx) == CN_Tbl_Idx ?
                     CN_TYPE_IDX(IL_IDX(start_list_idx)) : 
                     ATD_TYPE_IDX((IL_IDX(start_list_idx))));

      if (TYP_LINEAR(type_idx)<TYP_LINEAR((IL_FLD(end_list_idx) == CN_Tbl_Idx ?
                     CN_TYPE_IDX(IL_IDX(end_list_idx)) : 
                     ATD_TYPE_IDX((IL_IDX(end_list_idx)))))) {

         type_idx = (IL_FLD(end_list_idx) == CN_Tbl_Idx ?
                     CN_TYPE_IDX(IL_IDX(end_list_idx)) : 
                     ATD_TYPE_IDX((IL_IDX(end_list_idx))));
      }

      ATD_TYPE_IDX(tmp_idx) = type_idx;
      AT_SEMANTICS_DONE(tmp_idx)= TRUE;
      ATD_STOR_BLK_IDX(tmp_idx) = SCP_SB_STACK_IDX(curr_scp_idx);


      tmp_idx = gen_compiler_tmp(line, col, Priv, TRUE);

      BD_UB_FLD(bd_idx,idx) = AT_Tbl_Idx;
      BD_UB_IDX(bd_idx,idx) = tmp_idx;

      ATD_TYPE_IDX(tmp_idx) = type_idx;
      AT_SEMANTICS_DONE(tmp_idx)= TRUE;
      ATD_STOR_BLK_IDX(tmp_idx) = SCP_SB_STACK_IDX(curr_scp_idx);

# if defined(_HIGH_LEVEL_IF_FORM)

      gt_idx = gen_ir(IL_FLD(start_list_idx), IL_IDX(start_list_idx),
                      Gt_Opr, CG_LOGICAL_DEFAULT_TYPE, line, col,
                      IL_FLD(end_list_idx), IL_IDX(end_list_idx));


      if_idx = gen_ir(IR_Tbl_Idx, gt_idx,
                      If_Opr, CG_LOGICAL_DEFAULT_TYPE, line, col,
                      NO_Tbl_Idx, NULL_IDX);

      gen_sh(Before, If_Stmt, line, col, FALSE, FALSE, TRUE);
      SH_P2_SKIP_ME(SH_PREV_IDX(curr_stmt_sh_idx)) = TRUE;
      SH_IR_IDX(SH_PREV_IDX(curr_stmt_sh_idx)) = if_idx;

      if_sh_idx = SH_PREV_IDX(curr_stmt_sh_idx);
# else

      gt_idx = gen_ir(IL_FLD(start_list_idx), IL_IDX(start_list_idx),
                      Le_Opr, CG_LOGICAL_DEFAULT_TYPE, line, col,
                      IL_FLD(end_list_idx), IL_IDX(end_list_idx));


      label1 = gen_internal_lbl(line);
      label2 = gen_internal_lbl(line);

      if_idx = gen_ir(IR_Tbl_Idx, gt_idx,
                 Br_True_Opr, CG_LOGICAL_DEFAULT_TYPE, line, col,
                      AT_Tbl_Idx, label1);

      gen_sh(Before, If_Stmt, line, col, FALSE, FALSE, TRUE);
      SH_P2_SKIP_ME(SH_PREV_IDX(curr_stmt_sh_idx)) = TRUE;
      SH_IR_IDX(SH_PREV_IDX(curr_stmt_sh_idx)) = if_idx;

# endif

      /* if (start > end) => lb=end ub=start */

      asg_idx = gen_ir(BD_LB_FLD(bd_idx,idx), BD_LB_IDX(bd_idx,idx),
                   Asg_Opr, type_idx, line, col,
                       IL_FLD(end_list_idx), IL_IDX(end_list_idx));

      gen_sh(Before, Assignment_Stmt, line, col, FALSE, FALSE, TRUE);
      SH_P2_SKIP_ME(SH_PREV_IDX(curr_stmt_sh_idx)) = TRUE;
      SH_IR_IDX(SH_PREV_IDX(curr_stmt_sh_idx)) = asg_idx;

      asg_idx = gen_ir(BD_UB_FLD(bd_idx,idx), BD_UB_IDX(bd_idx,idx),
                   Asg_Opr, type_idx, line, col,
                       IL_FLD(start_list_idx), IL_IDX(start_list_idx));

      gen_sh(Before, Assignment_Stmt, line, col, FALSE, FALSE, TRUE);
      SH_P2_SKIP_ME(SH_PREV_IDX(curr_stmt_sh_idx)) = TRUE;
      SH_IR_IDX(SH_PREV_IDX(curr_stmt_sh_idx)) = asg_idx;


# if defined(_HIGH_LEVEL_IF_FORM)
      else_idx = gen_ir(SH_Tbl_Idx, if_sh_idx,
                   Else_Opr, CG_LOGICAL_DEFAULT_TYPE, line, col,
                        NO_Tbl_Idx, NULL_IDX);

      gen_sh(Before, Else_Stmt, line, col, FALSE, FALSE, TRUE);
      SH_P2_SKIP_ME(SH_PREV_IDX(curr_stmt_sh_idx)) = TRUE;
      SH_IR_IDX(SH_PREV_IDX(curr_stmt_sh_idx)) = else_idx;
      SH_PARENT_BLK_IDX(SH_PREV_IDX(curr_stmt_sh_idx)) = if_sh_idx;

      else_sh_idx = SH_PREV_IDX(curr_stmt_sh_idx);
# else
      else_idx = gen_ir(NO_Tbl_Idx, NULL_IDX,
                   Br_Uncond_Opr, TYPELESS_DEFAULT_TYPE, line, col,
                        AT_Tbl_Idx, label2);

      gen_sh(Before, Goto_Stmt, line, col, FALSE, FALSE, TRUE);
      SH_P2_SKIP_ME(SH_PREV_IDX(curr_stmt_sh_idx)) = TRUE;
      SH_IR_IDX(SH_PREV_IDX(curr_stmt_sh_idx)) = else_idx;

      else_idx = gen_ir(AT_Tbl_Idx, label1,
                   Label_Opr, TYPELESS_DEFAULT_TYPE, line, col,
                        NO_Tbl_Idx, NULL_IDX);

      gen_sh(Before, Continue_Stmt, line, col, FALSE, FALSE, TRUE);
      SH_P2_SKIP_ME(SH_PREV_IDX(curr_stmt_sh_idx)) = TRUE;
      SH_IR_IDX(SH_PREV_IDX(curr_stmt_sh_idx)) = else_idx;

      AT_DEFINED(label1) = TRUE;
      ATL_DEF_STMT_IDX(label1) = SH_PREV_IDX(curr_stmt_sh_idx);
# endif

      /* else  => lb=start ub=end */

      asg_idx = gen_ir(BD_LB_FLD(bd_idx,idx), BD_LB_IDX(bd_idx,idx),
                   Asg_Opr, type_idx, line, col,
                       IL_FLD(start_list_idx), IL_IDX(start_list_idx));

      gen_sh(Before, Assignment_Stmt, line, col, FALSE, FALSE, TRUE);
      SH_P2_SKIP_ME(SH_PREV_IDX(curr_stmt_sh_idx)) = TRUE;
      SH_IR_IDX(SH_PREV_IDX(curr_stmt_sh_idx)) = asg_idx;

      asg_idx = gen_ir(BD_UB_FLD(bd_idx,idx), BD_UB_IDX(bd_idx,idx),
                   Asg_Opr, type_idx, line, col,
                       IL_FLD(end_list_idx), IL_IDX(end_list_idx));

      gen_sh(Before, Assignment_Stmt, line, col, FALSE, FALSE, TRUE);
      SH_P2_SKIP_ME(SH_PREV_IDX(curr_stmt_sh_idx)) = TRUE;
      SH_IR_IDX(SH_PREV_IDX(curr_stmt_sh_idx)) = asg_idx;


# if defined(_HIGH_LEVEL_IF_FORM)
      endif_idx = gen_ir(SH_Tbl_Idx, if_sh_idx,
                    Endif_Opr, TYPELESS_DEFAULT_TYPE, line, col,
                         NO_Tbl_Idx, NULL_IDX);

      gen_sh(Before, End_If_Stmt, line, col, FALSE, FALSE, TRUE);
      SH_P2_SKIP_ME(SH_PREV_IDX(curr_stmt_sh_idx)) = TRUE;
      SH_IR_IDX(SH_PREV_IDX(curr_stmt_sh_idx)) = endif_idx;
      SH_PARENT_BLK_IDX(SH_PREV_IDX(curr_stmt_sh_idx)) = else_sh_idx;

      IR_FLD_R(if_idx) = SH_Tbl_Idx;
      IR_IDX_R(if_idx) = SH_PREV_IDX(curr_stmt_sh_idx);
      IR_LINE_NUM_R(if_idx) = line;
      IR_COL_NUM_R(if_idx) = col;
# else
      else_idx = gen_ir(AT_Tbl_Idx, label2,
                   Label_Opr, TYPELESS_DEFAULT_TYPE, line, col,
                        NO_Tbl_Idx, NULL_IDX);

      gen_sh(Before, Continue_Stmt, line, col, FALSE, FALSE, TRUE);
      SH_P2_SKIP_ME(SH_PREV_IDX(curr_stmt_sh_idx)) = TRUE;
      SH_IR_IDX(SH_PREV_IDX(curr_stmt_sh_idx)) = else_idx;

      AT_DEFINED(label2) = TRUE;
      ATL_DEF_STMT_IDX(label2) = SH_PREV_IDX(curr_stmt_sh_idx);
# endif

      
   }


   TRACE (Func_Exit, "determine_lb_ub", NULL);

   return;

}  /* determine_lb_ub */

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

void gen_forall_if_mask(int	start_sh_idx,
			int	end_sh_idx)

{
   int		col;
   opnd_type	forall_mask_opnd;
   int		line;
   int		list_idx;

   TRACE (Func_Entry, "gen_forall_if_mask", NULL);

   line = SH_GLB_LINE(start_sh_idx);
   col = SH_COL_NUM(start_sh_idx);

# ifdef _DEBUG
   if (active_forall_sh_idx == NULL_IDX) {
      PRINTMSG(line, 626, Internal, col,
               "active_forall_sh_idx", "gen_forall_if_mask");
   }
# endif

   list_idx = IR_IDX_R(SH_IR_IDX(active_forall_sh_idx));

   while (list_idx &&
          IL_FLD(list_idx) == IL_Tbl_Idx) {
      list_idx = IL_NEXT_LIST_IDX(list_idx);
   }

   if (list_idx &&
       IL_NEXT_LIST_IDX(list_idx) != NULL_IDX) {

      copy_subtree(&IL_OPND(IL_NEXT_LIST_IDX(list_idx)), &forall_mask_opnd);

   }
   else {
      goto EXIT;
   }


   gen_if_stmt(&forall_mask_opnd, 
               start_sh_idx,
               end_sh_idx,
               NULL_IDX,
               NULL_IDX,
               line,
               col);


EXIT:

   TRACE (Func_Exit, "gen_forall_if_mask", NULL);

   return;

}  /* gen_forall_if_mask */

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

static boolean forall_mask_needs_tmp(opnd_type	*top_opnd)

{
   boolean	needs_tmp = FALSE;
   opnd_type	lhs_opnd;
   opnd_type	mask_opnd;
   int		sh_idx;


   TRACE (Func_Entry, "forall_mask_needs_tmp", NULL);

   sh_idx = active_forall_sh_idx;

   COPY_OPND(mask_opnd, (*top_opnd));
   copy_subtree(&mask_opnd, &mask_opnd);
   process_attr_links(&mask_opnd);

   while (sh_idx != IR_IDX_L(SH_IR_IDX(active_forall_sh_idx))) {
      if (SH_STMT_TYPE(sh_idx) == Assignment_Stmt) {
         COPY_OPND(lhs_opnd, IR_OPND_L(SH_IR_IDX(sh_idx)));
         copy_subtree(&lhs_opnd, &lhs_opnd);
         process_attr_links(&lhs_opnd);

         check_dependence(&needs_tmp,
                          lhs_opnd,
                          mask_opnd);

         if (OPND_FLD(lhs_opnd) == IR_Tbl_Idx) {
            free_ir_stream(OPND_IDX(lhs_opnd));
         }

         if (needs_tmp) {
            break;
         }
      }
      sh_idx = SH_NEXT_IDX(sh_idx);
   }

   if (OPND_FLD(mask_opnd) == IR_Tbl_Idx) {
      free_ir_stream(OPND_IDX(mask_opnd));
   }

   TRACE (Func_Exit, "forall_mask_needs_tmp", NULL);

   return(needs_tmp);

}  /* forall_mask_needs_tmp */

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

static void process_attr_links(opnd_type	*opnd)

{
   int		attr_idx;
   int		ir_idx;
   int		list_idx;


   TRACE (Func_Entry, "process_attr_links", NULL);

   switch (OPND_FLD((*opnd))) {
   case AT_Tbl_Idx:
      attr_idx = OPND_IDX((*opnd));

      while (AT_ATTR_LINK(attr_idx)) {
         attr_idx = AT_ATTR_LINK(attr_idx);
      }

      OPND_IDX((*opnd)) = attr_idx;

      break;

   case CN_Tbl_Idx:
   case SH_Tbl_Idx:
   case NO_Tbl_Idx:
      break;

   case IR_Tbl_Idx:
      ir_idx = OPND_IDX((*opnd));
      process_attr_links(&IR_OPND_L(ir_idx));
      process_attr_links(&IR_OPND_R(ir_idx));
      break;

   case IL_Tbl_Idx:
      list_idx = OPND_IDX((*opnd));
      while (list_idx) {
         process_attr_links(&IL_OPND(list_idx));
         list_idx = IL_NEXT_LIST_IDX(list_idx);
      }
      break;

   }

   TRACE (Func_Exit, "process_attr_links", NULL);

   return;

}  /* process_attr_links */

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

static int gen_forall_derived_type(int	type_idx,
				   int	rank,
				   int	line,
				   int	col)

{
   int			attr_idx;
   int			dt_idx;
   int			length;
   id_str_type		name;
   int			np_idx;
   int			sn_idx;
   int			dt_type_idx;

   extern void set_up_fake_dt_blk(int);


   TRACE (Func_Entry, "gen_forall_derived_type", NULL);

   /****************************\
   |* create derived type attr *|
   \****************************/

   CREATE_ID(name, " ", 1);

   dt_counter++;

# if defined(_HOST_OS_UNICOS) || defined(_HOST_OS_MAX)
   length = sprintf(name.string, "dt$%d", dt_counter);
# else
   sprintf(name.string, "dt$%d", dt_counter);
   length = strlen(name.string);
# endif

   NTR_NAME_POOL(&(name.words[0]), length, np_idx);

   NTR_ATTR_TBL(dt_idx);
   AT_DEF_LINE(dt_idx)         = line;
   AT_DEF_COLUMN(dt_idx)       = col;
   AT_NAME_LEN(dt_idx)         = length;
   AT_NAME_IDX(dt_idx)         = np_idx;
   AT_DEFINED(dt_idx)          = TRUE;
   AT_LOCKED_IN(dt_idx)        = TRUE;
   AT_OBJ_CLASS(dt_idx)        = Derived_Type;
   ATT_SCP_IDX(dt_idx)         = curr_scp_idx;
   ATT_NUMERIC_CPNT(dt_idx)    = TRUE;
   ATT_DCL_NUMERIC_SEQ(dt_idx) = TRUE;
   ATT_SEQUENCE_SET(dt_idx)    = TRUE;
   AT_SEMANTICS_DONE(dt_idx)   = TRUE;
   ATT_POINTER_CPNT(dt_idx)    = TRUE;
   ATT_STRUCT_BIT_LEN_FLD(dt_idx) = CN_Tbl_Idx;
   ATT_STRUCT_BIT_LEN_IDX(dt_idx) = CN_INTEGER_ZERO_IDX;

   if (cmd_line_flags.s_pointer8) {
      ATT_ALIGNMENT(dt_idx) = Align_64;
   }
   else {
      ATT_ALIGNMENT(dt_idx) = WORD_ALIGN;
   }

   ATT_NUM_CPNTS(dt_idx) = 1;

   /*************************\
   |* now for the component *|
   \*************************/

   /* pointer component */

   CREATE_ID(TOKEN_ID(token), "PTR", 3);
   TOKEN_LEN(token)         = 3;
   TOKEN_VALUE(token)       = Tok_Id;
   TOKEN_LINE(token)        = line;
   TOKEN_COLUMN(token)      = col;

   NTR_SN_TBL(sn_idx);
   NTR_NAME_POOL(TOKEN_ID(token).words, TOKEN_LEN(token), np_idx);
   NTR_ATTR_TBL(attr_idx);
   AT_OBJ_CLASS(attr_idx)  = Data_Obj;
   AT_DEF_LINE(attr_idx)   = line;
   AT_DEF_COLUMN(attr_idx) = col;
   AT_NAME_LEN(attr_idx)   = TOKEN_LEN(token);
   AT_NAME_IDX(attr_idx)   = np_idx;
   SN_NAME_LEN(sn_idx)     = TOKEN_LEN(token);
   SN_NAME_IDX(sn_idx)     = np_idx;
   SN_ATTR_IDX(sn_idx)     = attr_idx;

   AT_SEMANTICS_DONE(attr_idx) = TRUE;
   ATD_CLASS(attr_idx)         = Struct_Component;
   ATD_DERIVED_TYPE_IDX(attr_idx)       = dt_idx;
   AT_TYPED(attr_idx)          = TRUE;

   ATD_TYPE_IDX(attr_idx)      = type_idx;
   ATD_IM_A_DOPE(attr_idx)     = TRUE;
   ATD_POINTER(attr_idx)       = TRUE;
   ATD_ARRAY_IDX(attr_idx)     = rank;

   ATD_OFFSET_ASSIGNED(attr_idx) = TRUE;
   ATD_OFFSET_FLD(attr_idx) = CN_Tbl_Idx;
   ATD_CPNT_OFFSET_IDX(attr_idx) = CN_INTEGER_ZERO_IDX;
   ATT_FIRST_CPNT_IDX(dt_idx) = sn_idx;

   set_up_fake_dt_blk(dt_idx);
   assign_offset(attr_idx);
   set_up_fake_dt_blk(NULL_IDX);

   CLEAR_TBL_NTRY(type_tbl, TYP_WORK_IDX);
   TYP_TYPE(TYP_WORK_IDX)   = Structure;
   TYP_LINEAR(TYP_WORK_IDX) = Structure_Type;
   TYP_IDX(TYP_WORK_IDX)    = dt_idx;
   dt_type_idx	            = ntr_type_tbl();

   TRACE (Func_Exit, "gen_forall_derived_type", NULL);

   return(dt_type_idx);

}  /* gen_forall_derived_type */

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

boolean check_where_conformance(expr_arg_type	*exp_desc)

{
   int			i;
   boolean		ok = TRUE;
   int			tmp_idx;

   TRACE (Func_Entry, "check_where_conformance", NULL);

   tmp_idx = find_left_attr(&IR_OPND_L(where_ir_idx));

# ifdef _DEBUG
   if (AT_OBJ_CLASS(tmp_idx) != Data_Obj ||
       ATD_CLASS(tmp_idx) != Compiler_Tmp) {
      PRINTMSG(IR_LINE_NUM(where_ir_idx), 626, Internal,
               IR_COL_NUM(where_ir_idx),
               "Compiler_Tmp", "check_where_conformance");
   }
# endif

   if (exp_desc->rank != BD_RANK(ATD_ARRAY_IDX(tmp_idx))) {
      ok = FALSE;
   }
   else {
      for (i = 0; i < exp_desc->rank; i++) {
         if (OPND_FLD(exp_desc->shape[i])           == CN_Tbl_Idx &&
             BD_XT_FLD(ATD_ARRAY_IDX(tmp_idx), i+1) == CN_Tbl_Idx &&
             fold_relationals(OPND_IDX(exp_desc->shape[i]),
                           BD_XT_IDX(ATD_ARRAY_IDX(tmp_idx), i+1),
                              Ne_Opr)) {

            /* non conforming array syntax */

            ok = FALSE;
            break;
         }
      }
   }

   TRACE (Func_Exit, "check_where_conformance", NULL);

   return(ok);

}  /* check_where_conformance */

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

static void setup_interchange_level_list(opnd_type	do_var_opnd)

{
   int		count;
   boolean	found_non_tmp;
   int		il_idx;
   int		ir_idx;


   TRACE (Func_Entry, "setup_interchange_level_list", NULL);

   /* This is only necessary for pdgcs based platforms.  This     */
   /* sets up the level list to match the do list.  For example   */
   /* if the user specifies  interchange(k,i,j) and the do's are  */
   /* nested like  do i, do j, do k, then the level list should   */
   /* read 2, 3, 1 (as in i is 2nd in the list, j is 3rd in the   */
   /* list and k is 1st in the list).                             */
      
   if (cdir_switches.interchange_sh_idx != NULL_IDX) {
      found_non_tmp	= FALSE;
      ir_idx		= SH_IR_IDX(cdir_switches.interchange_sh_idx);
      il_idx		= IR_IDX_L(ir_idx);
      count		= 1;

      while (il_idx != NULL_IDX) {

         if (IL_FLD(il_idx) == AT_Tbl_Idx &&
             OPND_IDX(do_var_opnd) == IL_IDX(il_idx)) {
            break;  /* This do var is #count in the list */
         }

         if (IL_FLD(il_idx) != AT_Tbl_Idx ||
            AT_OBJ_CLASS(IL_IDX(il_idx)) != Data_Obj ||
            ATD_CLASS(IL_IDX(il_idx)) != Compiler_Tmp) {
            found_non_tmp = TRUE;
         }
         il_idx		= IL_NEXT_LIST_IDX(il_idx);
         ++count;
      }

      cdir_switches.interchange_level = count;

      if (!found_non_tmp) {
         cdir_switches.interchange_sh_idx = NULL_IDX;
      }
   }


   TRACE (Func_Exit, "setup_interchange_level_list", NULL);

   return;

}  /* setup_interchange_level_list */
