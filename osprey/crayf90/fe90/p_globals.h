/*
 * Copyright (C) 2007, 2008. PathScale, LLC. All Rights Reserved.
 */
/*
 *  Copyright (C) 2007. QLogic Corporation. All Rights Reserved.
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



/* USMID:  "\n@(#)5.0_pl/headers/p_globals.h	5.3	07/19/99 12:03:43\n" */


/******************************************************************************/
/* DATA STRUCTURES DEPENDENT ON THE FOLLOWING ENUMERATION:                    */
/*      stmt_in_blk                     DEFINED IN      p_driver.h            */
/*      blk_err_msgs                    DEFINED IN      p_driver.h            */
/*      blk_struct_str                  DEFINED IN      debug.h               */
/*      blk_stk (entry size)            DEFINED IN      p_globals.h           */
/*      end_blocks                      DEFINED IN      p_end.h               */
/******************************************************************************/

enum    blk_cntxt_values       {Unknown_Blk,
                                Blockdata_Blk,
                                Module_Blk,
				Program_Blk,
                                Function_Blk,
                                Subroutine_Blk,
				Internal_Blk,
				Module_Proc_Blk,
				Interface_Body_Blk,
                                Do_Blk,
                                Forall_Blk,
                                If_Blk,
                                If_Then_Blk,
				If_Else_If_Blk,
				If_Else_Blk,
                                Select_Blk,
                                Case_Blk,
                                Where_Then_Blk,
                                Where_Else_Blk,
                                Where_Else_Mask_Blk,
				Parallel_Blk,
				Doall_Blk,
				Do_Parallel_Blk,
				Guard_Blk,
				Parallel_Case_Blk,
				Wait_Blk,
				SGI_Doacross_Blk,
				SGI_Psection_Blk,
				SGI_Section_Blk,
				SGI_Pdo_Blk,
				SGI_Parallel_Do_Blk,
				SGI_Parallel_Blk,
				SGI_Critical_Section_Blk,
				SGI_Single_Process_Blk,
				SGI_Region_Blk,
                                Open_Mp_Parallel_Blk,
                                Open_Mp_Do_Blk,
                                Open_Mp_Parallel_Sections_Blk,
                                Open_Mp_Sections_Blk,
                                Open_Mp_Section_Blk,
                                Open_Mp_Single_Blk,
                                Open_Mp_Parallel_Do_Blk,
                                Open_Mp_Master_Blk,
                                Open_Mp_Critical_Blk,
                                Open_Mp_Ordered_Blk,
                                Open_Mp_Workshare_Blk, /* by jhs, 02/7/18 */
                                Open_Mp_Parallel_Workshare_Blk, /* by jhs, 02/7/18 */
                                Contains_Blk,
                                Interface_Blk,
				Derived_Type_Blk,
#ifdef KEY /* Bug 10572 */
				Enum_Blk
#endif /* KEY Bug 10572 */
				};

enum	do_loop_values		{Unknown_Loop,
         			 Iterative_Loop,
				 While_Loop,
    				 Infinite_Loop };

enum expr_values		{Expr_Unknown,
				 Scalar_Int_Init_Expr,
				 Scalar_Int_Expr,	/* specification-expr */
				 Scalar_Init_Expr,
				 Scalar_Expr
				};
				

enum    directive_region_values {Parallel_Region,
                                 Doall_Region,
                                 Do_Parallel_Region,
                                 Guard_Region,
                                 Case_Region,
                                 Region_Region,
                                 Sgi_Parallel_Region,
                                 Doacross_Region,
                                 Parallel_Do_Region,
                                 Pdo_Region,
                                 Parallel_Section_Region,
                                 Critical_Section_Region,
                                 Single_Process_Region,
				 Open_Mp_Parallel_Region,
				 Open_Mp_Do_Region,
				 Open_Mp_Parallel_Sections_Region,
				 Open_Mp_Sections_Region,
				 Open_Mp_Section_Region,
				 Open_Mp_Single_Region,
				 Open_Mp_Parallel_Do_Region,
				 Open_Mp_Master_Region,
				 Open_Mp_Critical_Region,
				 Open_Mp_Ordered_Region,
				 Open_Mp_Workshare_Region, /* by jhs, 02/7/18 */
				 Open_Mp_Parallel_Workshare_Region, /* by jhs, 02/7/18 */
                                 Last_Region};



/* ************************************************************************** */
/*									      */
/*   The following enum definition is used to determine how to form the name  */
/*   of an internal loop label (see parse_do_stmt and end_do_blk).            */
/*									      */
/* ************************************************************************** */

enum lbl_pos_values             { Top_Lbl, Cycle_Lbl, Exit_Lbl, Skip_Lbl };


/* ************************************************************************** */
/*									      */
/*   The following enum definition is used to determine the type of Forward   */
/*   Reference entry to generate for a forward reference to a stmt label.     */
/*									      */
/* ************************************************************************** */

enum lbl_ref_values		{Illegal_Lbl_Ref_Value,
				 Branch_Context,
				 Format_Ref,
				 Assign_Ref,
                                 Do_Ref};


/* ************************************************************************** */
/*									      */
/*   NOTE:  All Comma searches must be at the end of the list and come after  */
/*          Find_Comma. 						      */
/*								              */
/* ************************************************************************** */

enum search_values 		{Find_None,
				 Find_EOS,
				 Find_Lparen,
				 Find_Rparen,
                                 Find_Matching_Rparen,
				 Find_Comma,
				 Find_Comma_Slash,
				 Find_Comma_Rparen,
				 Find_Expr_End,
                                 Find_Ref_End
				};


enum stmt_category_values	{Init_Stmt_Cat,
				 Sub_Func_Stmt_Cat,
				 Dir_Integer_Stmt_Cat,
				 Use_Stmt_Cat,
#ifdef KEY /* Bug 11741 */
				 Import_Stmt_Cat,
#endif /* KEY Bug 11741 */
				 Implicit_None_Stmt_Cat,
				 Implicit_Stmt_Cat,
				 Declaration_Stmt_Cat,
				 Executable_Stmt_Cat };

typedef	enum	blk_cntxt_values	blk_cntxt_type;
typedef union	blk_stk_entry		blk_stk_type;
typedef enum	do_loop_values		do_loop_type;
typedef	enum	expr_values		expr_type;
typedef enum    lbl_pos_values          lbl_pos_type;
typedef enum	lbl_ref_values		lbl_ref_type;
typedef enum	search_values		search_type;
typedef enum	stmt_category_values	stmt_category_type;


/******************************************************************************/
/*									      */
/*                        Block Stack Entry Definition                        */
/*									      */
/*  If you change the definition of this entry, be sure to also update the    */
/*  block stack entry size in sytb.m.					      */
/*									      */
/******************************************************************************/

union	blk_stk_entry  {
			struct {
				Uint			def_line	: 24;
				Uint			def_column	:  8;
				Uint			name_idx	: 20;
				boolean			parallel_region	:  1;
				boolean			blk_err		:  1;
				boolean			no_exec		:  1;
				boolean			fnd_default	:  1;
				blk_cntxt_type		blk_type	:  8;

				Uint			first_sh_idx	: 24;
				Uint			UNUSED2		:  8;
				Uint			UNUSED3		:  8;
				Uint			label_idx	: 24;

				opnd_type		multiuse_opnd	    ;

                        	Uint			top_lbl_idx	: 20;
				Uint			loop_num	: 12;
                        	boolean			has_cycle_stmt  :  1;
				boolean			has_exit_stmt 	:  1;
				boolean			has_nested_loop :  1;
				boolean			blkbl_nest_ok   :  1;
 		        	do_loop_type		do_type		:  4;
                        	Uint			skip_lbl_idx    : 24;

  				Uint			start_temp_idx	: 24;
				Uint			UNUSED8		:  8;
				Uint			UNUSED9		: 12;
				Uint			inc_temp_idx	: 20;

				Uint			induc_temp_idx	: 24;
				Uint			UNUSED10	:  8;
				Uint			UNUSED11	:  8;
				Uint			tc_temp_idx	: 24;

				Uint			blkbl_dir_sh_idx  : 24;
                                Uint                    blkbl_num_lcvs    :  8;
                                Uint                    intchg_num_lcvs   :  8;
				Uint			intchg_dir_sh_idx : 24;

				Uint			UNUSED12          : 24;
                                Uint                    UNUSED13          :  8;
                                Uint                    dir_nest_num_lcvs :  8;
				Uint			dir_nest_ck_sh_idx: 24;
				} fld;

			struct {
				Uint                   field32_1	: 32;
				Uint                   field32_2	: 32;

				Uint                   field32_3	: 32;
				Uint                   field32_4	: 32;

				Uint                   field32_5_1	: 16;
				Uint                   field32_5_2	: 16;
				Uint                   field32_6_1	: 16;
				Uint                   field32_6_2	: 16;

				Uint                   field32_7	: 32;
				Uint                   field32_8	: 32;

				Uint                   field32_9	: 32;
				Uint                   field32_10	: 32;

				Uint                   field32_11	: 32;
				Uint                   field32_12	: 32;

				Uint                   field32_13	: 32;
				Uint                   field32_14	: 32;

				Uint                   field32_15	: 32;
				Uint                   field32_16	: 32;
				} fld_long;

			long	wd[NUM_BLK_STK_WDS];
                        };


/******************************************************************************/

/*****************************************\
|* PARSER ACCESSIBLE FUNCTION PROTOTYPES *|
\*****************************************/

extern	int	blk_match_err(blk_cntxt_type, boolean, boolean);
extern  void    build_fwd_ref_entry (int, lbl_ref_type);
extern	char	ch_after_paren_grp (void);
extern	void	check_for_vestigial_task_blks(void);
extern  int	check_label_ref (void);
extern	void	complete_intrinsic_definition(int);
#ifdef KEY /* Bug 5089 */
extern  int	intrinsic_module_lookup(int);
#endif /* KEY Bug 5089 */
extern  void	determine_stmt_type (void);
extern  boolean digit_is_format_label(void);
extern  void    do_cmic_blk_checks(void);
extern	void	end_critical_section_blk (boolean);
extern	void	end_do_parallel_blk (boolean);
extern	void	end_doacross_blk (boolean);
extern	void	end_doall_blk (boolean);
extern	void	end_guard_blk (boolean);
extern  void    end_labeled_do (void);
extern	void	end_open_mp_critical_blk (boolean);
extern	void	end_open_mp_do_blk (boolean);
extern	void	end_open_mp_master_blk (boolean);
extern	void	end_open_mp_ordered_blk (boolean);
extern	void	end_open_mp_parallel_blk (boolean);
extern	void	end_open_mp_parallel_do_blk (boolean);
extern	void	end_open_mp_parallel_sections_blk (boolean);
extern	void	end_open_mp_section_blk (boolean);
extern	void	end_open_mp_sections_blk (boolean);
extern	void	end_open_mp_single_blk (boolean);
extern	void	end_open_mp_parallel_workshare_blk(boolean);
extern	void	end_open_mp_workshare_blk(boolean);
extern	void	end_parallel_blk (boolean);
extern	void	end_parallel_case_blk (boolean);
extern	void	end_pdo_blk (boolean);
extern	void	end_psection_blk (boolean);
extern	void	end_region_blk (boolean);
extern	void	end_SGI_parallel_blk (boolean);
extern	void	end_single_process_blk (boolean);
extern	void	end_wait_blk (boolean);

# if defined(_EXPRESSION_EVAL)
extern	void	expression_eval_end (void);
# endif

extern  void    flush_LA_to_EOS (void);
extern  void    flush_LA_to_symbol (void);
extern  void    format_line_n_col (int *, int *, int);
extern 	void	gen_attr_and_IR_for_lbl (boolean);
extern	int	gen_loop_lbl_name (int, lbl_pos_type);
extern	void	implicit_use_semantics(void);
extern	boolean	iss_blk_stk_err(void);
extern  boolean is_implied_do (void);
extern  boolean is_substring_ref (void);
extern  void	label_ref_semantics (int, lbl_ref_type, int, int, int);
extern	boolean	matched_specific_token (token_values_type, token_class_type);
extern	boolean	matched_token_class (token_class_type);
extern	boolean	merge_access (int, int, int, access_type);
extern	boolean	merge_allocatable (boolean, int, int, int);
extern	boolean	merge_automatic (boolean, int, int, int);
extern	boolean	merge_co_array (boolean, int, int, int, int);
extern	boolean	merge_data (boolean, int, int, int);
extern	boolean	merge_dimension (int, int, int, int);
extern	boolean	merge_external (boolean, int, int, int);
extern	boolean	merge_intent (boolean, int, int, int);
extern	boolean	merge_intrinsic (boolean, int, int, int);
extern	boolean	merge_optional (boolean, int, int, int);
extern	boolean	merge_pointer (boolean, int, int, int);
extern	boolean	merge_save (boolean, int, int, int);
extern	boolean	merge_target (boolean, int, int, int);
extern	boolean	merge_volatile (boolean, int, int, int);
#ifdef KEY /* Bug 14150 */
extern  boolean merge_bind(boolean, int, int, int);
extern  boolean merge_value(boolean, int, int, int);
#endif /* KEY Bug 14150 */
extern	int	move_blk_to_end(int);
extern  boolean next_arg_is_kwd_equal (void);
extern  boolean next_id_is_imp_control (void);
extern  boolean next_tok_is_paren_slash (void);
extern	int	ntr_io_string_constant(void);
extern  boolean parse_err_flush (search_type, char *);
extern  boolean parse_io_list (opnd_type *);
extern  boolean parse_operand (opnd_type *);
extern	boolean pop_and_err_blk_stk (int, boolean);
extern  void    prev_char_line_and_col (int *, int *);
extern  int     put_char_const_in_tbl (char, int *);
extern  int     put_format_in_tbl (void);
extern  boolean remove_do_parallel_blk(boolean, char *, int, int);
extern  boolean remove_pdo_blk(boolean, char *, int, int);
extern  void	resolve_fwd_lbl_refs (void);
extern	int	start_new_prog_unit (pgm_unit_type, blk_cntxt_type, 
				     boolean, boolean, int *);
extern  void    set_format_start_idx (int);
extern  boolean stmt_has_double_colon (void);
extern  boolean stmt_is_DATA_stmt (void);
extern  boolean stmt_is_DO_stmt (void);
extern  boolean stmt_is_save_stmt(int, int);
#ifdef KEY /* Bug 14110 */
extern void surprise_volatile(char *);
extern void revisit_volatile();
#endif /* KEY Bug 14110 */

/*********************\
|* statement parsers *|
\*********************/

extern	void parse_allocatable_stmt (void);
extern	void parse_allocate_stmt (void);
extern	void parse_access_stmt (void);
extern	void parse_assign_stmt (void);
extern	void parse_assignment_stmt (void);
extern	void parse_automatic_stmt (void);
extern	void parse_bad_stmt(void);
extern	void parse_backspace_stmt (void);
extern	void parse_block_stmt (void);
extern	void parse_buffer_stmt (void);
extern	void parse_call_stmt (void);
extern	void parse_case_stmt (void);
extern	void parse_close_stmt (void);
extern	void parse_common_stmt (void);
extern	void parse_contains_stmt (void);
extern	void parse_continue_stmt (void);
extern	void parse_cycle_stmt (void);
extern	void parse_data_stmt (void);
extern	void parse_deallocate_stmt (void);
extern	void parse_decode_stmt (void);
extern	void parse_dimension_stmt (void);
extern	void parse_directive_stmt (void);
extern	void parse_do_stmt (void);
extern	void parse_elemental_stmt (void);
extern	void parse_else_stmt (void);
extern	void parse_encode_stmt (void);
extern	void parse_end_stmt (void);
extern	void parse_endfile_stmt (void);
extern	void parse_entry_stmt (void);
extern	void parse_equivalence_stmt (void);
extern	void parse_exit_stmt (void);
extern	void parse_external_stmt (void);
extern	void parse_forall (void);
extern	void parse_format_stmt (void);
extern	void parse_function_stmt (void);
extern	void parse_goto_stmt (void);
extern	void parse_if_stmt (void);
extern	void parse_implicit_stmt (void);
#ifdef KEY /* Bug 11741 */
extern	void parse_import_stmt (void);
#endif /* KEY Bug 11741 */
#ifdef KEY /* Bug 10572 */
extern	void parse_enum_stmt (void);
extern	void parse_enumerator_stmt (void);
extern	boolean	parse_int_spec_expr(long *, fld_type *, boolean, boolean);
#endif /* KEY Bug 10572 */
#ifdef KEY /* Bug 14150 */
extern	void parse_bind_stmt (void);
extern	void parse_value_stmt (void);
#endif /* KEY Bug 14150 */
extern	void parse_inquire_stmt (void);
extern	void parse_intent_stmt (void);
extern	void parse_interface_stmt (void);
extern	void parse_intrinsic_stmt (void);
extern	void parse_module_stmt (void);
extern	void parse_namelist_stmt (void);
extern	void parse_nullify_stmt (void);
extern	void parse_open_stmt (void);
extern	void parse_optional_stmt (void);
extern	void parse_parameter_stmt (void);
extern	void parse_pointer_stmt (void);
extern	void parse_print_stmt (void);
extern	void parse_program_stmt (void);
extern	void parse_pure_stmt (void);
extern	void parse_read_stmt (void);
extern	void parse_recursive_stmt (void);
extern	void parse_return_stmt (void);
extern	void parse_rewind_stmt (void);
extern	void parse_save_stmt (void);
extern	void parse_select_stmt (void);
extern	void parse_sequence_stmt (void);
extern	void parse_stmt_func_stmt (int, int);
extern	void parse_stop_pause_stmt (void);
extern	void parse_subroutine_stmt (void);
extern	void parse_target_stmt (void);
extern  void parse_task_common_stmt (void);
extern	void parse_type_dcl_stmt (void);
extern	void parse_use_stmt (void);
extern	void parse_volatile_stmt (void);
extern	void parse_where_stmt (void);
extern	void parse_write_stmt (void);


/*************************\
|* statement sub-parsers *|
\*************************/

extern	int		generic_spec_semantics (void);
extern  boolean         paren_grp_is_cplx_const(void);
extern  boolean         parse_actual_arg_spec (opnd_type *, boolean, int);
extern	int		parse_array_spec (int);
extern	int		parse_pe_array_spec (int);
extern  boolean         parse_deref (opnd_type *, int);
extern	boolean 	parse_expr (opnd_type *);
extern	boolean		parse_generic_spec (void);
extern  boolean         parse_imp_do (opnd_type *);
extern  intent_type 	parse_intent_spec (void);
extern  void	    	parse_length_selector (int, boolean, boolean);
#ifdef KEY /* Bug 8422 */
extern  int		parse_non_char_kind_selector(boolean);
#endif /* KEY Bug 8422 */
extern	boolean		parse_type_spec (boolean);
extern	void		parse_typed_function_stmt (void);
#ifdef KEY /* Bug 14150 */
extern int		parse_language_binding_spec(token_type *result);
extern void		set_binding_label(fld_type, int, token_type *);
#endif /* KEY Bug 14150 */

/*******************************\
|* globally accessible objects *|
\*******************************/

extern	blk_stk_type		*blk_stk;		/* defed in p_driver.h*/
extern	int			 blk_stk_idx;		/* defed in p_driver.h*/
extern	int			 blk_stk_inc;		/* defed in p_driver.h*/
extern	int			 blk_stk_init_size;	/* defed in p_driver.h*/
extern	int			 blk_stk_limit;		/* defed in p_driver.h*/
extern	int			 blk_stk_num_wds;	/* defed in p_driver.h*/
extern	int			 blk_stk_size;		/* defed in p_driver.h*/
extern	int			 blk_stk_largest_idx;	/* defed in p_driver.h*/

extern	int			 cif_end_unit_column;	/* defed in fecif.h   */
extern	int			 cif_end_unit_line;	/* defed in fecif.h   */
extern	boolean			 clearing_blk_stk;      /* defed in main.h    */
extern	boolean			 colon_recovery;	/* defed in p_driver.h*/
extern	stmt_category_type	 curr_stmt_category;	/* defed in p_driver.h*/

extern  boolean			 EOPU_encountered;

extern	boolean			 label_ok;		/* defed in p_driver.h*/
extern	token_type		 label_token;		/* defed in p_driver.h*/

extern	token_type		 main_token;  		/* defed in p_driver.h*/

extern	intent_type		 new_intent;		/* defed in p_driver.h*/
#ifdef KEY /* Bug 14150 */
/* Overload token error field of new_binding_label to indicate whether "bind"
 * includes optional name= clause */
#  define BIND_SPECIFIES_NAME(nbl) (!TOKEN_ERR(nbl))
#  define SET_BIND_SPECIFIES_NAME(nbl,val) (TOKEN_ERR(nbl) = !(val))
extern token_type		 new_binding_label;	/* def'd in p_driver.h*/
#endif /* KEY Bug 14150 */

extern	char			*obj_str[];		/* defed in p_driver.h*/
extern	int			 stmt_construct_idx;	/* defed in p_driver.h*/
extern	int			 stmt_line_idx;		/* defed in src_input.*/
extern	long long		 stmt_in_blk[];		/* defed in p_driver.h*/
extern	stmt_category_type	 stmt_top_cat[];	/* defed in p_driver.h*/
extern	type_tbl_type		 type_init_tbl[];	/* defed in main.h    */

extern  void 			 (*stmt_parsers[])();	/* defed in p_driver.h*/

extern  int			 if_stmt_lbl_idx;       /* defed in           */
							/*   p_ctl_flow.h     */

extern	long    directive_state;

/******************************************************************************\
|*                                                                            *|
|*     typedefs used by src_input.c and cond_comp.c                           *|
|*                                                                            *|
\******************************************************************************/

enum    line_type_values       {Comment_Line,
                                Include_Line,
                                Cond_Comp_Line,
                                Dir_Line,
                                Dir_Continuation_Line,
                                Regular_Line,
                                Continuation_Line,
                                Pound_Include_Enter_Line,
                                Pound_Include_Exit_Line,
                                Pound_Src_Line,
                                EOF_Line };

typedef enum                    line_type_values        line_type;

struct  cc_stmt_buf_line_entry  {long           line;
                                 long           start_idx;
                                };

typedef struct cc_stmt_buf_line_entry   cc_stmt_buf_line_type;

