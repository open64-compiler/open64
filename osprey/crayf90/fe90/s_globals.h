/*
 * Copyright (C) 2008. PathScale, LLC. All Rights Reserved.
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



/* USMID:  "\n@(#)5.0_pl/headers/s_globals.h	5.5	10/19/99 17:14:30\n" */


/**********************************************\
|* Semantic routines for individual statements |
\**********************************************/

extern  void allocate_stmt_semantics (void);
extern  void arith_if_stmt_semantics (void);
extern  void assign_stmt_semantics (void);
extern  void assignment_stmt_semantics (void);
extern  void backspace_stmt_semantics (void);
extern  void blockdata_stmt_semantics (void);
extern  void buffer_stmt_semantics (void);
extern  void call_stmt_semantics (void);
extern  void case_stmt_semantics (void);
extern  void close_stmt_semantics (void);
extern  void continue_stmt_semantics (void);
extern  void data_stmt_semantics (void);
extern  void deallocate_stmt_semantics (void);
extern  void directive_stmt_semantics (void);
extern  void encode_decode_stmt_semantics (void);
extern  void do_stmt_semantics (void);
extern  void else_stmt_semantics (void);
extern  void end_forall_semantics (void);
extern  void end_function_semantics (void);

#ifndef _HIGH_LEVEL_IF_FORM
extern  void end_if_semantics (void);
#endif

extern  void end_select_semantics (void);
extern  void end_stmt_semantics (void);
extern  void end_subroutine_semantics (void);
extern  void end_where_semantics (void);
extern  void endfile_stmt_semantics (void);
extern  void entry_stmt_semantics (void);
extern  void forall_semantics (void);
extern  void function_stmt_semantics (void);
extern  void goto_stmt_semantics (void);
extern  void if_stmt_semantics (void);
extern  void inquire_stmt_semantics (void);
extern  void interface_stmt_semantics (void);
extern  void module_stmt_semantics (void);
extern  void label_def_stmt_semantics (void);
extern  void nullify_stmt_semantics (void);
extern  void open_stmt_semantics (void);
extern  void outmoded_if_stmt_semantics (void);
extern  void pause_stmt_semantics (void);
extern  void print_stmt_semantics (void);
extern  void program_stmt_semantics (void);
extern  void read_stmt_semantics (void);
extern  void return_stmt_semantics (void);
extern  void rewind_stmt_semantics (void);
extern  void select_stmt_semantics (void);
extern  void stop_pause_stmt_semantics (void);
extern  void subroutine_stmt_semantics (void);
extern  void then_stmt_semantics (void);
extern  void where_stmt_semantics (void);
extern  void write_stmt_semantics (void);
extern  void type_init_semantics (void);


/*************************************\
|* Other semantic processing routines |
\*************************************/

extern  void		add_substring_length(int);
extern  boolean         call_list_semantics(opnd_type *, expr_arg_type *, 
                                            boolean);
extern	void		cast_to_cg_default(opnd_type *, expr_arg_type *);
extern	void		cast_opnd_to_type_idx(opnd_type *, int);
extern	void		change_asg_to_where(int);
extern	boolean		check_for_legal_define(opnd_type *);
#ifdef KEY /* Bug 14150 */
extern  boolean		check_for_legal_assignment_define(opnd_type *, boolean);
#endif /* KEY Bug 14150 */
extern	void		change_section_to_this_element(opnd_type *, 
                                                       opnd_type *, int);
extern	boolean		check_where_conformance(expr_arg_type *);
extern  void		cif_object_rec_for_func_result(int);
extern  boolean         cmp_ref_trees(opnd_type *, opnd_type *);
extern  void		default_init_semantics(int);
extern	boolean		compare_dummy_arguments(int, int);
extern  int		create_alloc_descriptor(int, int, int, boolean);
extern	int		create_argchck_descriptor(opnd_type *);
extern  boolean		create_constructor_constant(opnd_type *,
                                                    expr_arg_type *);
extern	int		create_equiv_stor_blk(int, sb_type_type);
extern  void		create_namelist_descriptor(int);
extern  boolean         create_runtime_array_constructor(opnd_type *,
                                                         expr_arg_type *);
extern  boolean		create_runtime_struct_constructor(opnd_type *);
extern  void            create_loop_stmts(int, opnd_type *, opnd_type *,
                                          opnd_type *,int, int);
extern  int             create_tmp_asg(opnd_type *, expr_arg_type *, 
                                       opnd_type *, int, boolean, boolean);
extern  void		determine_tmp_size(opnd_type *, int);
extern	void		doall_end_semantics(void);
extern  void            enlarge_call_list_tables(void);
extern  void            enlarge_info_list_table(void);
extern  boolean         final_arg_work(opnd_type *, int, int, expr_arg_type *);
#ifdef KEY /* Bug 5089 */
extern int gen_ieee_save_and_restore(int curr_scp_idx, int line, int column);
#endif /* KEY Bug 5089 */
extern  int		find_base_attr(opnd_type *, int *, int *);
extern  int             find_left_attr(opnd_type *);
extern  void            flatten_function_call(opnd_type *);
extern  void		fold_clen_opr(opnd_type *, expr_arg_type *);
extern	int		gen_alloc_header_const(int, int, boolean, int *);
extern	boolean		gen_bd_entry(opnd_type *, expr_arg_type *,
                                     int *, int, int);
extern	void		gen_copyin_bounds_stmt(int);
extern	int		gen_directive_ir(operator_type);
extern	void		gen_dv_access_low_bound(opnd_type *, opnd_type *, int);
extern  void		gen_dv_whole_def(opnd_type *, opnd_type *,
                                         expr_arg_type *);
extern  void		gen_dv_whole_def_init(opnd_type *, int, 
                                              sh_position_type);
extern  void            gen_entry_dope_code(int);
extern	void		gen_forall_if_mask(int, int);
extern	void		gen_forall_loops(int, int);
extern	void		gen_forall_tmp(expr_arg_type *, opnd_type *, int, int,
                                       boolean);
extern  boolean		gen_internal_dope_vector(int_dope_type *, opnd_type *,
                                                 boolean, expr_arg_type *);
extern  void		gen_loop_end_ir (void);
extern	void		gen_runtime_conformance(opnd_type *, expr_arg_type *,
                                                opnd_type *, expr_arg_type *);
extern  void            gen_runtime_substring(int);
extern  void            gen_runtime_bounds(int);
extern  boolean         gen_whole_subscript (opnd_type *, expr_arg_type *);
extern	void		get_char_len(opnd_type *, opnd_type *);
extern  void		get_concat_len (int, opnd_type *);
extern  int             gen_sf_dv_whole_def(opnd_type *, int, int);
extern  void            gen_split_alloc(int, int, int);
extern	void		get_shape_from_attr(expr_arg_type *, int, int, int,int);
extern  int		get_stmt_tmp(int, boolean, int);
extern  void		illegal_stmt_type(void);
extern	boolean		is_local_forall_index(int);
extern  int 		gen_static_integer_array_tmp(int, int, int);
extern	void		gen_common_dv_init(opnd_type *, int, sh_position_type);
extern  void            gen_static_dv_whole_def(opnd_type *, int, 
                                                sh_position_type);
extern  void		look_for_real_div(opnd_type *);
extern  void		make_base_subtree(opnd_type *,opnd_type *,int *,int *);
extern  void		make_triplet_extent_tree(opnd_type *, int);
extern  void            process_cpnt_inits(opnd_type *, int, void (*)(), 
                                           int, sh_position_type);
extern	void		process_char_len(opnd_type *);
extern  void            process_deferred_functions(opnd_type *);
extern  void            dope_vector_setup(opnd_type *, expr_arg_type *,
                                               opnd_type *, boolean);
extern  void            ptr_assign_from_ptr(opnd_type *, opnd_type *);
extern  void		no_semantics_routine(void);	
extern  boolean         operation_is_intrinsic(operator_type,int,int,int,int);
extern	void		set_directives_on_label(int);
extern	void		set_sb_first_attr_idx(int);
extern  void		set_shape_for_deferred_funcs(expr_arg_type *, int);
extern  void            set_up_allocate_as_call(int, int, int, boolean);
extern  void            set_up_exp_desc(opnd_type *, expr_arg_type *);
extern  void		short_circuit_branch(void);
extern  boolean		stmt_func_semantics(int);
extern  void            transform_cri_ch_ptr(opnd_type *);
# if defined(_F_MINUS_MINUS)
extern	void		translate_distant_ref(opnd_type *, 
                                              expr_arg_type *, int);
extern	void		translate_dv_component(opnd_type *, expr_arg_type *);
# endif
extern  void		transform_char_sequence_ref(opnd_type *, int);
extern  boolean         tree_produces_dealloc(opnd_type *root);
extern  boolean         validate_char_len(opnd_type *, expr_arg_type *);
#ifdef KEY /* Bug 14293, 11986, 6845 */
extern int help_dealloc(int, int, fld_type, int, boolean, boolean, boolean);
extern int allocatable_structure_component(int);
#endif /* KEY Bug 14293, 11986, 6845 */
#ifdef KEY /* Bug 14150 */
extern boolean check_interoperable_type(int, boolean, boolean);
extern void check_interoperable_constraints(int);
extern boolean interoperable_variable(int);
extern boolean no_length_type_param(int);
extern boolean length_type_param_is_one(int);
#endif /* KEY Bug 14150 */


/*********************************************\
|* Semantic Pass Globally Accessible Objects *|
\*********************************************/

struct         arg_strct    {
                                expr_arg_type   ed;

                                int             kwd;
				int		line;
				int		col;
				int		association;
                                opnd_type	arg_opnd;
                                boolean         pgm_unit                : 1;
                                boolean         maybe_modified		: 1;
                                boolean         insert_place_holder	: 1;
                            };

typedef        struct arg_strct      arg_strct_type;


extern arg_strct_type           *arg_info_list;
extern int                      *arg_list;
extern int                       arg_list_size;
extern int                       arg_info_list_size;
extern int                       arg_info_list_base;
extern int                       arg_info_list_top;

extern arg_strct_type		 init_arg_info;

extern boolean			 in_call_list;
extern boolean			 in_io_list;
extern boolean                   in_branch_true;
extern boolean			 defer_stmt_expansion;
extern int			 number_of_functions;
extern boolean                   io_item_must_flatten;
extern boolean                   tree_has_constructor;
#ifdef KEY /* Bug 4889 */
/* Set to the sh_idx when we see an OpenMP "do" or "paralleldo" directive,
 * cleared when we reach the "do" statement itself.
 */
extern int			 inside_paralleldo;
/* Set to the sh_idx when we see an OpenMP "parallel" directive; cleared when
 * we reach the OpenMP "endparallel"
 */
extern int			 inside_parallel;
#endif /* KEY Bug 4889 */


/*********************************************************\
|* stmt_tmp_tbl points to lists of tmps that are reused. *|
\*********************************************************/

struct	stmt_tmp_entry	{
				int	scalar_tmps_head;
				int	scalar_tmps_tail;
				int	dope_vector_tmps_head[8];
				int	dope_vector_tmps_tail[8];
			};

typedef	struct	stmt_tmp_entry	stmt_tmp_tbl_type;

extern	stmt_tmp_tbl_type	stmt_tmp_tbl[Num_Linear_Types];

/********************************************************\
|* These are the final_arg_work tables that control the *|
|* behavior of argument association. (ie. COPY IN/OUT)  *|
\********************************************************/

enum            act_arg_values {
                                Scalar_Expression,
                                Scalar_Var,
                                Scalar_Tmp_Var,
                                Array_Elt,
                                Array_Tmp_Elt,
                                Scalar_Ptr,
                                Scalar_Tmp_Ptr,
                                Scalar_Constant,

                                Array_Expr,
                                Array_Ptr,
                                Array_Tmp_Ptr,
                                Whole_Allocatable,
                                Whole_Tmp_Allocatable,
                                Whole_Sequence,
                                Whole_Tmp_Sequence,
                                Whole_Ass_Shape,
                                Whole_Array_Constant,
                                Sequence_Array_Section,
                                Constant_Array_Section,
                                Dv_Array_Section,
                                Vector_Subscript_Section,
                                Contig_Section,
                                Dv_Contig_Section
                               };

typedef enum    act_arg_values  act_arg_type;

extern act_arg_type    get_act_arg_type(expr_arg_type *);

extern boolean	tree_has_ranf;

extern void    analyse_loops(opnd_type *, opnd_type *, size_level_type *);

extern	int	contig_test_ir_idx;

/*****************************************\
|* used for COPY_ASSUMED_SHAPE directive *|
\*****************************************/

extern int      shared_bd_idx;
extern boolean  reassign_XT_temps;


extern boolean label_allowed;

extern int	active_forall_sh_idx;
extern boolean	within_forall_construct;
extern boolean	within_forall_mask_expr;


extern	int     alloc_block_start_idx;
extern	int     alloc_block_end_idx;


extern	boolean	variable_size_func_expr;
