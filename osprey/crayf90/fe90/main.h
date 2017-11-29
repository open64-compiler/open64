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



/* USMID:  "\n@(#)5.0_pl/headers/main.h	5.10	09/01/99 12:24:50\n" */

/************************************************************/
/* setup release string and frontend version from release.c */
/************************************************************/

extern  char                    release_level[];
extern  char                    frontend_version[];
 
/****************************************\
|* Static data used within this module. *|
\****************************************/
 
				/* Ddd Mmm dd, 19yy  hh:mm:ss */
static	char	        	comp_date_time[DATE_TIME_STR_SIZE];

static  char			compiler_gen_time[]	= __TIME__;
static	long			max_field_len;

/**********************************\
|* enums used within this module. *|
\**********************************/

enum    language        {Pdgcs_Ansi_C = 1,      Pdgcs_Fortran_77,
                         Pdgcs_Fortran_90,      Pdgcs_Fortran_77_MPP,
                         Pdgcs_Fortran_90_MPP,  Pdgcs_Other_Lang
                        };


/*************************************************\
|* Globally accessible objects defined in main.h *|
\*************************************************/

	ac_cmd_line_flags_type	ac_cmd_line_flags;
	char		       *basic_type_str[]	= {
						       "INTEGER",
						       "LOGICAL",
						       "REAL",
						       "COMPLEX",
						       "Cray pointer",
						       "Cray parcel pointer",
						       "Cray character pointer",
						       "typeless",
						       "CHARACTER",
						       "derived-type"
						       };

	char			assembly_file[MAX_FILE_NAME_SIZE];
	char			assembly_listing_file[MAX_FILE_NAME_SIZE];
	boolean			assembly_output			= FALSE;
	char			bin_file[MAX_FILE_NAME_SIZE];
	boolean			binary_output			= FALSE;
	char			dot_i_file[MAX_FILE_NAME_SIZE];

	char		        debug_file_name[MAX_FILE_NAME_SIZE];

	long                    ccg_dump_flags;

	FILE		       *c_i_f;  /* File name used in all CIF accesses.*/

	FILE		       *cif_actual_file;

       /* CIF needs to know the actual -C options specified (not the OR'd     */
       /* final value representing the combination of options a single option */
       /* might imply) so the following variable is used to record these      */
       /* actual options.  						      */

	int			cif_C_opts;

	boolean			cif_first_pgm_unit;
        int			cif_flags;
	int			cif_internal_proc_start_line;
	int			cif_module_proc_start_line;
	boolean			cif_need_unit_rec;
	int			cif_pgm_unit_start_line;

        /* this is usually FALSE.  It only gets set TRUE, when    */
        /* expr_semantics is processing, the duplicate bounds     */
        /* tmps that are only kept for CIF.  We need to issue the */
        /* same number of messages, whether CIF is on or off.     */

	boolean			cif_tmp_so_no_msg	= FALSE;

        /* cif_tmp_file records output while parsing the first */
        /* stmt of a program unit are temporarily saved here.  */

	FILE		       *cif_tmp_file;

	boolean			clearing_blk_stk;
	cmd_line_flags_type	cmd_line_flags;
	int			code_size               = 0;
	int			comp_phase;
	char			compiler_gen_date[]	= __DATE__;
	int			contig_test_ir_idx	= NULL_IDX;
        convert_to_string_type	convert_to_string_fmt	= Dont_Care;
	int          		curr_debug_lbl;
	int			curr_glb_line		= 0;
	int          		curr_internal_lbl;
	int			curr_scp_idx;
	int			curr_stmt_sh_idx;
	int			curr_gl_stmt_sh_idx	= NULL_IDX;
	int			global_stmt_sh_idx	= NULL_IDX;
	int			const_safevl_idx;
	int			data_size               = 0;

	linear_type_type	default_linear_type[Num_Fortran_Types];
        long		       *dt_cmp_tbl		= NULL;
	char		       *dir_mic_str[Tok_Mic_End-Tok_Mic_Start] =
						       {" ",
                                			"CONTINUE",
                                			"CNCALL",
                                			"CASE",
                                			"DOPARALLEL",
                                			"DOALL",
                                			"ENDPARALLEL",
                                			"ENDGUARD",
                                			"ENDCASE",
                                			"ENDDO",
                                			"GUARD",
                                			"IF",
                                			"MAXCPUS",
                                			"NUMCPUS",
                                			"PERMUTATION",
                                			"PARALLEL",
                                			"POINT",
                                			"SEND",
                                			"SPAN",
                                			"TASKCOMMON",
                                			"WAIT"};

        char		       *directive_str[Tok_Dir_End-Tok_Dir_Start] =
						       {" ",
							"ALIGN",
							"ATOMIC UPDATE",
							"AUTOSCOPE",
							"AUXILIARY",
							"BARRIER",
							"BL",
							"BLOCK",
							"BOUNDS",
							"CACHE_ALIGN",
							"CACHE_BYPASS",
							"CHUNKSIZE",
							"CNCALL",
							"CODE",
							"COMMON",
							"CONCURRENT",
							"CONTROL",
							"COPY_ASSUMED_SHAPE",
							"CRITICAL",
							"DOSHARED",
							"DYNAMIC",
							"EJECT",
							"END CRITICAL",
							"END MASTER",
							"FIXED",
							"FLOW",
							"FREE",
							"GEOMETRY",
							"GETFIRST",
							"GUIDED",
							"ID",
							"IF",
							"IGNORE_TKR",
							"INLINE",
							"INLINEALWAYS",
							"INLINENEVER",
							"INTEGER",
							"IVDEP",
							"LIST",
							"MASTER",
							"MAXCPUS",
							"MODINLINE",
							"NAME",
							"NCPUS_CHUNKS",
							"NEXTSCALAR",
							"NO BARRIER",
							"NOBL",
							"NOBOUNDS",
							"NOCODE",
							"NOFLOW",
							"NOINLINE",
							"NOINTERCHANGE",
							"NOLIST",
							"NOMARK",
							"NOMODINLINE",
							"NOPATTERN",
							"NORECURRENCE",
							"NOSIDEEFFECTS",
							"NOSPLIT",
							"NOTASK",
							"NOUNROLL",
							"NOVECTOR",
							"NOVSEARCH",
							"NUMCHUNKS",
							"NUMCPUS",
							"PARALLEL_ONLY",
							"PATTERN",
							"PE_PRIVATE",
							"PE_RESIDENT",
							"PERMUTATION",
							"PREFERTASK",
							"PREFERVECTOR",
							"PRIVATE",
							"RECURRENCE",
							"REGFILE",
							"SAVELAST",
							"SEMEXTERN",
							"SERIAL_ONLY",
							"SHARED",
							"SHORTLOOP",
							"SHORTSEQUENCE",
							"SINGLE",
 							"SPLIT",
							"STACK",
							"STATIC",
							"SUPPRESS",
							"SYMMETRIC",
							"SYSTEM_MODULE",
							"TASK",
							"TASKCOMMON",
							"TASKHEAD",
							"UNKNOWN",
							"UNKNOWN_SHARED",
							"UNROLL",
							"USES_EREGS",
							"VECTOR",
							"VFUNCTION",
							"VSEARCH"};

	boolean			disregard_directive[Tok_Dir_End-Tok_Dir_Start];
	boolean			disregard_mics[Tok_Mic_End-Tok_Mic_Start];
	boolean			disregard_mips
                                          [Tok_SGI_Dir_End-Tok_SGI_Dir_Start];
	boolean			disregard_open_mp
                                   [Tok_Open_Mp_Dir_End-Tok_Open_Mp_Dir_Start];
	dump_flags_type		dump_flags;

	int			expanded_intrinsic_list = NULL_IDX;
        expr_mode_type		expr_mode		= Regular_Expr;
#ifdef KEY /* Bug 4232 */
        boolean			defining_stmt_func      = FALSE;
#endif /* KEY Bug 4232 */

	void			(*get_char) ();
        void                    (*get_char_literal) ();

	long			glb_tbl_idx[Num_Glb_Tbl_Idxs];

	boolean			have_unnamed_pgm_unit	= FALSE;  
	boolean			need_pure_function	= FALSE;  
	boolean			have_main_pgm_unit	= FALSE;

	boolean			in_action_stmt_of_if_where_or_forall	= FALSE;

	int			include_path_idx	= NULL_IDX;

	opnd_type		init_target_opnd	= INIT_OPND_TYPE;

	boolean			inline_global_sgi	= FALSE;
	boolean			noinline_global_sgi	= FALSE;

	int			inline_path_idx		= NULL_IDX;

	boolean			issue_overflow_msg_719	= TRUE;

        long			max_call_list_size 	= 0;
        long_type		max_character_length 	= 0;

	long			mc_tbl[128];
	char			mod_out_path[MAX_FILE_NAME_SIZE];

	int			module_path_idx		= NULL_IDX;
#ifdef KEY /* Bug 5089 */
	int			intrinsic_module_path_idx = NULL_IDX;
#endif /* KEY Bug 5089 */

	boolean			need_new_sh;
	boolean			need_to_issue_719	= FALSE;

	opnd_type		null_opnd		= INIT_OPND_TYPE;
	int			num_ansi                = 0;
	int			num_cautions            = 0;
	int			num_comments            = 0;
	int			num_errors              = 0;
	int			num_notes               = 0;
   	int			num_optz_msgs		= 0;
	int			num_warnings            = 0;

	int			statement_number	= 0;

	int			num_prog_unit_errors;
	int			num_of_derived_types;
 
	on_off_flags_type	on_off_flags;
	opt_flags_type		opt_flags;

        char			parse_operand_insert[40] = "operand";
        int                     pgm_unit_start_line	= 0;
	char			preinline_file[MAX_FILE_NAME_SIZE];
	int			prev_statement_number	= 0;
        char			program_unit_name[MAX_ID_LEN+1] = 
                                                      UNNAMED_PROGRAM_NAME;

        int			sb_len[End_Name_Blk] = {5,6,10,8,11,5,4,12,6,6};

# if defined(_NO_AT_SIGN_IN_NAMES)
        char		       *sb_name[End_Name_Blk] = {
				".WHAT",
				".BASED",
				".DATA_INIT",
				".POINTEE",
				".STACK_HOST",
				".DATA",
				".SYM",
				".DATA_UNINIT",
				".STACK",
				".DARGS"};
# else
        char		       *sb_name[End_Name_Blk] = {
				"@WHAT",
				"@BASED",
				"@DATA_INIT",
				"@POINTEE",
				"@STACK_HOST",
				"@DATA",
				"@SYM",
				"@DATA_UNINIT",
				"@STACK",
				"@DARGS"};
# endif
			
        src_form_type           source_form;
	char			src_file[MAX_FILE_NAME_SIZE];
        int                     stmt_end_col;
        int                     stmt_end_line;
	int			stmt_label_idx;
        int                     stmt_start_col;
        int                     stmt_start_line;
        stmt_type_type          stmt_type;

	char		       *stmt_type_str[]	= {"?????",
						   "ALLOCATABLE",
						   "AUTOMATIC",
						   "COMMON",
						   "CONTAINS",
 						   "component-def-stmt",
						   "DATA",
						   "TYPE type-name",
						   "DIMENSION",
						   "DIRECTIVE",
						   "EQUIVALENCE",
						   "EXTERNAL",
						   "FORMAT",
						   "IMPLICIT",
						   "IMPLICIT NONE",
						   "INTENT",
						   "INTERFACE",
						   "INTRINSIC",
						   "MODULE PROCEDURE",
						   "NAMELIST",
						   "OPTIONAL",
						   "PARAMETER",
						   "POINTER",
						   "PRIVATE",
						   "PUBLIC",
						   "SAVE",
						   "SEQUENCE",
						   "statement-function",
						   "TARGET",
						   "TASK COMMON",
						   "type-declaration",
						   "USE",
			
						   "BLOCKDATA",
						   "ELEMENTAL",
						   "FUNCTION",
						   "MODULE",
						   "PROGRAM",
						   "PURE",
						   "RECURSIVE",
						   "SUBROUTINE",
			
						   "END BLOCKDATA",
						   "END DO",
						   "END FUNCTION",
						   "END IF",
						   "END INTERFACE",
						   "END MODULE",
						   "END PROGRAM",
						   "END SELECT",
						   "END",
						   "END SUBROUTINE",
						   "END TYPE",
						   "END WHERE",
			
						   "ALLOCATE",
						   "arithmetic IF",
						   "ASSIGN",
						   "assignment",
						   "BACKSPACE",
						   "BUFFER",
						   "CALL",
						   "CASE",
						   "CLOSE",
						   "CONTINUE",
						   "CYCLE",
						   "DEALLOCATE",
						   "DECODE",
						   "DO",
						   "DO WHILE",
						   "DO",
						   "ELSE",
						   "ELSE IF",
						   "ELSE WHERE",
						   "ENCODE",
						   "ENDFILE",
						   "ENTRY",
						   "EXIT",
						   "GO TO",
						   "IF THEN",
						   "IF",
						   "INQUIRE",
						   "NULLIFY",
						   "OPEN",
						   "Outmoded IF",
						   "PAUSE",
						   "PRINT",
						   "READ",
						   "RETURN",
						   "REWIND",
						   "SELECT CASE",
						   "STOP",
						   "THEN",
						   "WHERE construct",
						   "WHERE",
						   "WRITE",
						   "Type Init",

               					   "Label Def",
						   "Construct Def",
						   "Automatic Base Calc",
						   "Automatic Base Size",

						   "END PARALLEL",
						   "END DO PARALLEL",
						   "END PARALLEL CASE",
						   "PARALLEL CASE",
						   "END GUARD",
						   "STATEMENT NUM",
						   "SECTION",
                                 		   "END PSECTION",
                                 		   "END PDO",
                                 		   "SGI END PARALLEL",
                                 		   "END CRITICAL SECTION",
                                 		   "END SINGLE PROCESS",
                                 		   "REGION END",
                                 		   "OPEN MP SECTION",
                                 		   "OPEN MP END PARALLEL",
                                 		   "OPEN MP END DO",
                                	  "OPEN MP END PARALLEL SECTIONS",
                                 		   "OPEN MP END SECTIONS",
                                 		   "OPEN MP END SECTION",
                                 		   "OPEN MP END SINGLE",
                                 		   "OPEN MP END PARALLEL DO",
                                 		   "OPEN MP END MASTER",
                                 		   "OPEN MP END CRITICAL",
                                 		   "OPEN MP END ORDERED",
						   "FORALL Construct",
						   "FORALL",
						   "END FORALL",
						   "ELSE WHERE MASK",
                                                   "VOLATILE",
                                                   "OPEN MP END PARALLEL WORKSHARE",
                                                   "OPEN MP END WORKSHARE",
#ifdef KEY /* Bug 11741 */
						   "IMPORT",
#endif /* KEY Bug 11741 */
#ifdef KEY /* Bug 10572 */
						   "ENUM",
						   "END ENUMERATOR",
						   "ENUMERATOR",
#endif /* KEY Bug 10572 */
#ifdef KEY /* Bug 14150 */
						   "BIND",
						   "VALUE"
#endif /* KEY Bug 10572 */
						  };

	target_machine_type	target_machine;
	boolean			target_ieee		= FALSE;
        int			target_safevl		= DEFAULT_SAFEVL;
	boolean			target_sv1		= FALSE;
	boolean			target_triton		= FALSE;
	boolean			target_t3e		= FALSE;
	int			target_os;
        boolean                 host_ieee               = FALSE;
	long			true_value		= -1;




/***************************************************\
|* globally accessible objects defined in tokens.h *|
\***************************************************/
      
	la_type			la_ch;
	token_type		token;



/******************************************************\
|* globally accessible objects defined in src_input.h *|
\******************************************************/

        boolean                 sig_blank		= FALSE;
        la_type                 stmt_EOS_la_ch		= {EOS,
                                               		   Ch_Class_EOS,
                                               		   0,
                                               		   0
                                               		  };

/**************************************************************************\
|* Variables global to s_io.c and p_io.c.                                 *|
\**************************************************************************/

boolean  is_namelist;
int      imp_do_var_list = NULL_IDX;
boolean  two_word_fcd;
boolean  char_len_in_bytes;

/**************************************************************************\
|* Variables global to s_asg_expr.c and s_utils.c                         *|
\**************************************************************************/

boolean   in_call_list         = FALSE;
boolean   in_branch_true       = FALSE;
boolean   defer_stmt_expansion = FALSE;
boolean   no_func_expansion    = FALSE;

int       number_of_functions  = 0;
boolean   io_item_must_flatten = FALSE;
boolean   tree_has_constructor = FALSE;

boolean   in_implied_do        = FALSE;

boolean   in_constructor       = FALSE;

boolean   in_io_list           = FALSE;

boolean   comp_gen_expr        = FALSE;

boolean   parsing_kind_selector		= FALSE;

long	message_error_tbl[MAX_MSG_SIZE];
long	message_suppress_tbl[MAX_MSG_SIZE];
long	message_warning_tbl[MAX_MSG_SIZE];

/*****************************************\
|* used for COPY_ASSUMED_SHAPE directive *|
\*****************************************/

int      shared_bd_idx = NULL_IDX;
boolean  reassign_XT_temps = FALSE;

/**************************************************************************\
|* global cdir switches for use in all passes.                            *|
\**************************************************************************/

cdir_switch_type	cdir_switches;

/**************************************************************************\
|* global idx for where mask temporary.                                   *|
\**************************************************************************/

int     where_ir_idx   		= NULL_IDX;
int     where_dealloc_stmt_idx	= NULL_IDX;

/**************************************************************************\
|* global variable for xref cif states.                                   *|
\**************************************************************************/

cif_usage_code_type	xref_state;

/**************************************************************************\
|* global variables for constant constructor type conversions             *|
\**************************************************************************/

boolean                 check_type_conversion = FALSE;
int			target_type_idx;
int   	                target_char_len_idx;
int			target_array_idx = NULL_IDX;

boolean			insert_subs_ok = TRUE;


/********************************************************************\
|* Get at catastrophic error routine in cif.c.			    *|
\********************************************************************/

extern void	cif_fake_a_unit (void);

/* LRR: Temporarily declare this here until we move to a single .T then e     */
/* remove this and the code that references it.				      */

extern FILE    *tmp_msg_file;



/**************************************************\
|* parameters used to define symbol and ir tables *|
\**************************************************/

/* NOTE:  ..._idx always points to the last used entry in the table. */

attr_list_tbl_type     * RESTRICT attr_list_tbl;
int                     attr_list_tbl_idx		= NULL_IDX;
int                     attr_list_tbl_inc		= 50;
int                     attr_list_tbl_init_size		= 100;
int			attr_list_tbl_limit		= (1 << 16) - 1;
int			attr_list_tbl_num_wds		= NUM_AL_WDS;
int                     attr_list_tbl_size 		= 0;
int                     attr_list_tbl_largest_idx	= NULL_IDX;

attr_tbl_type          * RESTRICT attr_tbl;
int                     attr_tbl_idx			= NULL_IDX;
int                     attr_tbl_inc			= 500;
int                     attr_tbl_init_size		= 1000;
int			attr_tbl_limit			= (1 << 20) - 1;
int			attr_tbl_num_wds		= NUM_AT_WDS;
int                     attr_tbl_size 			= 0;
int                     attr_tbl_largest_idx		= NULL_IDX;

attr_aux_tbl_type      * RESTRICT attr_aux_tbl;
int                     attr_aux_tbl_idx		= NULL_IDX;
int                     attr_aux_tbl_inc		= 500;
int                     attr_aux_tbl_init_size		= 1000;
int			attr_aux_tbl_limit		= (1 << 20) - 1;
int			attr_aux_tbl_num_wds		= NUM_AA_WDS;
int                     attr_aux_tbl_size 		= 0;
int                     attr_aux_tbl_largest_idx	= NULL_IDX;

bounds_tbl_type	       * RESTRICT bounds_tbl;
int                     bounds_tbl_idx			= BD_LAST_USED_IDX; 
int                     bounds_tbl_inc			= 100;
int                     bounds_tbl_init_size		= 100; 
int			bounds_tbl_limit		= (1 << 20) - 1;
int			bounds_tbl_num_wds		= NUM_BD_WDS;
int                     bounds_tbl_size 		= 0;
int                     bounds_tbl_largest_idx		= BD_LAST_USED_IDX; 

const_pool_type        * RESTRICT const_pool;
int                     const_pool_idx			= NULL_IDX; 
int                     const_pool_inc			= 500;
int                     const_pool_init_size		= 500; 
int			const_pool_limit		= (1 << 24) - 1;
int			const_pool_num_wds		= NUM_CP_WDS;
int                     const_pool_size			= 0;
int                     const_pool_largest_idx		= NULL_IDX; 


/* This is only used by ntr_abnormal_ieee_const but it needs to be here so    */
/* it can be initialized for each program unit.				      */

int                     ieee_const_tbl_idx[18];


const_tbl_type         * RESTRICT const_tbl;
int                     const_tbl_idx			= NULL_IDX; 
int                     const_tbl_inc			= 1000;
int                     const_tbl_init_size		= 1000; 
int			const_tbl_limit			= (1 << 24) - 1;
int			const_tbl_num_wds		= NUM_CN_WDS;
int                     const_tbl_size			= 0;
int                     const_tbl_largest_idx		= NULL_IDX; 

int			cn_root_idx[Num_Linear_Types];

equiv_tbl_type         * RESTRICT equiv_tbl;
int                     equiv_tbl_idx           	= NULL_IDX;
int                     equiv_tbl_inc           	= 100;
int                     equiv_tbl_init_size		= 100;
int                     equiv_tbl_limit         	= (1 << 16) - 1;
int                     equiv_tbl_num_wds       	= NUM_EQ_WDS;
int                     equiv_tbl_size          	= 0;
int                     equiv_tbl_largest_idx		= NULL_IDX;

        /* file_path_tbl_size is set to non-zero, because TBL_ALLOC is used   */
        /* to allocate this table, rather than CHECK_INITIAL_ALLOC which      */
        /* expects mod_file_tbl_size to be zero.                              */

file_path_tbl_type     * RESTRICT file_path_tbl; 
int			file_path_tbl_idx		= NULL_IDX;
int			file_path_tbl_inc		= 10;
int			file_path_tbl_init_size		= 30;
int			file_path_tbl_limit		= (1 << 16) - 1;
int			file_path_tbl_num_wds		= NUM_FP_WDS;
int			file_path_tbl_size		= 30;
int			file_path_tbl_largest_idx	= NULL_IDX;

global_attr_tbl_type   * RESTRICT global_attr_tbl;
int                     global_attr_tbl_idx		= NULL_IDX;
int                     global_attr_tbl_inc		= 10;
int                     global_attr_tbl_init_size	= 20;
int			global_attr_tbl_limit		= (1 << 20) - 1;
int			global_attr_tbl_num_wds		= NUM_GA_WDS;
int                     global_attr_tbl_size 		= 20;
int                     global_attr_tbl_largest_idx	= NULL_IDX;

global_bounds_tbl_type * RESTRICT global_bounds_tbl;
int                     global_bounds_tbl_idx		= NULL_IDX;
int                     global_bounds_tbl_inc		= 10;
int                     global_bounds_tbl_init_size	= 20;
int			global_bounds_tbl_limit		= (1 << 16) - 1;
int			global_bounds_tbl_num_wds	= NUM_GB_WDS;
int                     global_bounds_tbl_size 		= 20;
int                     global_bounds_tbl_largest_idx	= NULL_IDX;

global_line_tbl_type   * RESTRICT global_line_tbl;
long			global_line_tbl_idx		= 0; 
int			global_line_tbl_inc		= 5;
int			global_line_tbl_init_size	= 5;
long			global_line_tbl_limit		= OUR_LONG_MAX;
int			global_line_tbl_num_wds		= NUM_GL_WDS; 
long			global_line_tbl_size		= 5;
long			global_line_tbl_largest_idx	= 1; 

global_name_tbl_type   * RESTRICT global_name_tbl;
long			global_name_tbl_idx		= 0; 
int			global_name_tbl_inc		= 10;
int			global_name_tbl_init_size	= 50;
long			global_name_tbl_limit		= OUR_LONG_MAX;
int			global_name_tbl_num_wds		= NUM_GN_WDS; 
long			global_name_tbl_size		= 50;
long			global_name_tbl_largest_idx	= 1; 

global_type_tbl_type   * RESTRICT global_type_tbl;
int			global_type_tbl_idx		= 0;
int			global_type_tbl_inc		= 10;
int			global_type_tbl_init_size	= 10;
int			global_type_tbl_limit		= (1 << 16) - 1;
int			global_type_tbl_num_wds		= NUM_TYP_WDS;
int			global_type_tbl_size		= 10;
int			global_type_tbl_largest_idx	= 1;

global_ir_tbl_type     * RESTRICT global_ir_tbl			       = NULL;
int                     global_ir_tbl_idx                      = NULL_IDX;
int                     global_ir_tbl_inc                      = 100;
int                     global_ir_tbl_init_size                = 100;
int                     global_ir_tbl_limit                    = (1 << 24) - 1;
int                     global_ir_tbl_num_wds                  = NUM_IR_WDS;
int                     global_ir_tbl_size                     = 100;
int                     global_ir_tbl_largest_idx              = NULL_IDX;

global_ir_list_tbl_type * RESTRICT global_ir_list_tbl		       = NULL;
int                     global_ir_list_tbl_idx                 = NULL_IDX;
int                     global_ir_list_tbl_inc                 = 100;
int                     global_ir_list_tbl_init_size           = 100;
int                     global_ir_list_tbl_limit               = (1 << 24) - 1;
int                     global_ir_list_tbl_num_wds             = NUM_IL_WDS;
int                     global_ir_list_tbl_size                = 100;
int                     global_ir_list_tbl_largest_idx         = NULL_IDX;

global_sh_tbl_type     * RESTRICT global_sh_tbl			       = NULL;
int                     global_sh_tbl_idx                      = NULL_IDX;
int                     global_sh_tbl_inc                      = 100;
int                     global_sh_tbl_init_size                = 100;
int                     global_sh_tbl_limit                    = (1 << 24) - 1;
int                     global_sh_tbl_num_wds                  = NUM_SH_WDS;
int                     global_sh_tbl_size                     = 100;
int                     global_sh_tbl_largest_idx              = NULL_IDX;


loc_name_tbl_type      * RESTRICT hidden_name_tbl;
int                     hidden_name_tbl_idx		= NULL_IDX;
int                     hidden_name_tbl_inc		= 100;
int                     hidden_name_tbl_init_size	= 500;
int			hidden_name_tbl_limit		= (1 << 20) - 1;
int			hidden_name_tbl_num_wds		= NUM_HN_WDS;
int                     hidden_name_tbl_size 		= 0;
int                     hidden_name_tbl_largest_idx	= NULL_IDX;

ir_tbl_type	       * RESTRICT ir_tbl;
int			ir_tbl_idx			= NULL_IDX;
int			ir_tbl_inc			= 1000;
int			ir_tbl_init_size		= 1000;
int			ir_tbl_limit			= (1 << 24) - 1;
int			ir_tbl_num_wds			= NUM_IR_WDS;
int			ir_tbl_size			= 0;
int			ir_tbl_largest_idx		= NULL_IDX;
      
ir_list_tbl_type       * RESTRICT ir_list_tbl;
int			ir_list_tbl_idx			= NULL_IDX;
int			ir_list_tbl_inc			= 1000;
int			ir_list_tbl_init_size		= 1000;
int			ir_list_tbl_limit		= (1 << 24) - 1;
int			ir_list_tbl_num_wds		= NUM_IL_WDS;
int			ir_list_tbl_size		= 0;
int			ir_list_tbl_largest_idx		= NULL_IDX;

loc_name_tbl_type      * RESTRICT loc_name_tbl;
int                     loc_name_tbl_idx		= NULL_IDX;
int                     loc_name_tbl_inc		= 100;
int                     loc_name_tbl_init_size		= 500;
int			loc_name_tbl_limit		= (1 << 20) - 1;
int			loc_name_tbl_num_wds		= NUM_LN_WDS;
int                     loc_name_tbl_size 		= 0;
int                     loc_name_tbl_largest_idx	= NULL_IDX;

mod_link_tbl_type      * RESTRICT mod_link_tbl; 
long			mod_link_tbl_idx		= NULL_IDX;
int			mod_link_tbl_inc		= 10;
int			mod_link_tbl_init_size		= 0;
long			mod_link_tbl_limit		= OUR_LONG_MAX;
int			mod_link_tbl_num_wds		= NUM_ML_WDS;
long			mod_link_tbl_size		= 0;
long			mod_link_tbl_largest_idx	= NULL_IDX;

name_pool_type         * RESTRICT name_pool; 
int			name_pool_idx			= NP_LAST_USED_IDX;
int			name_pool_inc			= 500;
int			name_pool_init_size		= 500;
int			name_pool_limit			= (1 << 24) - 1;
int			name_pool_num_wds		= NUM_NP_WDS;
int			name_pool_size			= 0;
int			name_pool_largest_idx		= NP_LAST_USED_IDX;

pdg_link_tbl_type      * RESTRICT pdg_link_tbl; 
long			pdg_link_tbl_idx		= NULL_IDX;
int			pdg_link_tbl_inc		= 0;
int			pdg_link_tbl_init_size		= 0;
long			pdg_link_tbl_limit		= OUR_LONG_MAX;
int			pdg_link_tbl_num_wds		= NUM_PDG_WDS;
long			pdg_link_tbl_size		= 0;
long			pdg_link_tbl_largest_idx	= NULL_IDX;

rename_only_tbl_type   * RESTRICT rename_only_tbl; 
int			rename_only_tbl_idx		= NULL_IDX;
int			rename_only_tbl_inc		= 5;
int			rename_only_tbl_init_size	= 0;
int			rename_only_tbl_num_wds		= NUM_RO_WDS;
int			rename_only_tbl_limit		= (1 << 12) - 1;
int			rename_only_tbl_size		= 0;
int			rename_only_tbl_largest_idx	= NULL_IDX;

scp_tbl_type           * RESTRICT scp_tbl;
int                     scp_tbl_idx 			= NULL_IDX;
int                     scp_tbl_inc			= 2;
int                     scp_tbl_init_size		= 5;
int			scp_tbl_limit			= (1 << 16) - 1;
int			scp_tbl_num_wds			= NUM_SCP_WDS;
int                     scp_tbl_size			= 0;
int                     scp_tbl_largest_idx		= NULL_IDX;

sec_name_tbl_type      * RESTRICT sec_name_tbl;
int                     sec_name_tbl_idx		= NULL_IDX;
int                     sec_name_tbl_inc		= 500;
int                     sec_name_tbl_init_size		= 1000;
int			sec_name_tbl_limit		= (1 << 24) - 1;
int			sec_name_tbl_num_wds		= NUM_SN_WDS;
int                     sec_name_tbl_size 		= 0;
int                     sec_name_tbl_largest_idx	= NULL_IDX;

sh_tbl_type	       * RESTRICT sh_tbl;
int			sh_tbl_idx			= NULL_IDX;
int			sh_tbl_inc			= 1000;
int			sh_tbl_init_size		= 1000;
int			sh_tbl_limit			= (1 << 24) - 1;
int			sh_tbl_num_wds			= NUM_SH_WDS;
int			sh_tbl_size			= 0;
int			sh_tbl_largest_idx		= NULL_IDX;

stor_blk_tbl_type      * RESTRICT stor_blk_tbl;
int                     stor_blk_tbl_idx		= NULL_IDX;
int                     stor_blk_tbl_inc		= 10;
int			stor_blk_tbl_init_size		= 40;
int			stor_blk_tbl_limit      	= (1 << 16) - 1;
int			stor_blk_tbl_num_wds		= NUM_SB_WDS;
int                     stor_blk_tbl_size 		= 0;
int                     stor_blk_tbl_largest_idx	= NULL_IDX;

name_pool_type         * RESTRICT str_pool; 
int			str_pool_idx			= NULL_IDX;
int			str_pool_inc			= 50;
int			str_pool_init_size		= 100;
int			str_pool_limit			= (1 << 20) - 1;
int			str_pool_num_wds		= NUM_NP_WDS;
int			str_pool_size			= 100;
int			str_pool_largest_idx		= NULL_IDX;

type_tbl_type	       * RESTRICT type_tbl;
int                     type_tbl_idx			= TYP_LAST_USED_IDX;
int                     type_tbl_inc			= 20;
int			type_tbl_init_size		= 100;
int			type_tbl_limit			= (1 << 16) - 1;
int			type_tbl_num_wds		= NUM_TYP_WDS;
int                     type_tbl_size			= 0;
int                     type_tbl_largest_idx		= NULL_IDX;

/******************************************************************************\
|*      conversion table for fe90 linear type to arith.a types.               *|
\******************************************************************************/

# ifdef _ARITH_H

# if defined(_TARGET32) || defined(_WHIRL_HOST64_TARGET64)

long linear_to_arith[Num_Linear_Types] = {
        /* Err_Res          */          0,
        /* Short_Char_Const */          AR_Int_32_S,
        /* Short_Typeless_Const */      AR_Int_32_S,
        /* Typeless_1       */          AR_Int_32_S,
        /* Typeless_2       */          AR_Int_32_S,
        /* Typeless_4       */          AR_Int_32_S,
        /* Typeless_8       */          AR_Int_64_S,
        /* Long_Typeless    */          0,
        /* Integer_1        */          AR_Int_32_S,
        /* Integer_2        */          AR_Int_32_S,
        /* Integer_4        */          AR_Int_32_S,
        /* Integer_8        */          AR_Int_64_S,
        /* Real_4           */          AR_Float_IEEE_NR_32,
        /* Real_8           */          AR_Float_IEEE_NR_64,
        /* Real_16          */          AR_Float_IEEE_NR_128,
        /* Complex_4        */          AR_Complex_IEEE_NR_32,
        /* Complex_8        */          AR_Complex_IEEE_NR_64,
        /* Complex_16       */          AR_Complex_IEEE_NR_128,
        /* CRI_Ptr_8        */          0,
        /* Logical_1        */          AR_Logical,
        /* Logical_2        */          AR_Logical,
        /* Logical_4        */          AR_Logical,
        /* Logical_8        */          AR_Logical,
        /* Character_1      */          0,
        /* Character_2      */          0,
        /* Character_4      */          0,
        /* CRI_Ch_Ptr_8     */          0,
        /* Structure_Type   */          0,
        /* CRI_Parcel_Ptr_8   */                0
                };

# else

/* _TARGET64 */
long linear_to_arith[Num_Linear_Types] = {
        /* Err_Res          */          0,
        /* Short_Char_Const */          AR_Int_64_S,
        /* Short_Typeless_Const */      AR_Int_64_S,
        /* Typeless_1       */          AR_Int_32_S,
        /* Typeless_2       */          AR_Int_32_S,
        /* Typeless_4       */          AR_Int_32_S,
        /* Typeless_8       */          AR_Int_64_S,
        /* Long_Typeless    */          0,
# ifdef _TARGET_OS_MAX
        /* Integer_1        */          AR_Int_32_S,
        /* Integer_2        */          AR_Int_32_S,
        /* Integer_4        */          AR_Int_32_S,
# else
        /* Integer_1        */          AR_Int_64_S,
        /* Integer_2        */          AR_Int_64_S,
        /* Integer_4        */          AR_Int_64_S,
# endif
        /* Integer_8        */          AR_Int_64_S,
        /* Real_4           */          AR_Float_Cray1_64,
        /* Real_8           */          AR_Float_Cray1_64,
        /* Real_16          */          AR_Float_Cray1_128,
        /* Complex_4        */          AR_Complex_Cray1_64,
        /* Complex_8        */          AR_Complex_Cray1_64,
        /* Complex_16       */          AR_Complex_Cray1_128,
        /* CRI_Ptr_8        */          0,
        /* Logical_1        */          AR_Logical,
        /* Logical_2        */          AR_Logical,
        /* Logical_4        */          AR_Logical,
        /* Logical_8        */          AR_Logical,
        /* Character_1      */          0,
        /* Character_2      */          0,
        /* Character_4      */          0,
        /* CRI_Ch_Ptr_8     */          0,
        /* Structure_Type   */          0,
        /* CRI_Parcel_Ptr_8   */                0
                };
# endif

# if defined(_TARGET32) || defined(_WHIRL_HOST64_TARGET64)

long input_arith_type[Num_Linear_Types] = {
        /* Err_Res          */          0,
        /* Short_Char_Const */          0,
        /* Short_Typeless_Const */      0,
        /* Typeless_1       */          0,
        /* Typeless_2       */          0,
        /* Typeless_4       */          0,
        /* Typeless_8       */          0,
        /* Long_Typeless    */          0,
        /* Integer_1        */          AR_Int_32_U,
        /* Integer_2        */          AR_Int_32_U,
        /* Integer_4        */          AR_Int_32_U,
        /* Integer_8        */          AR_Int_64_U,
        /* Real_4           */          AR_Float_IEEE_NR_32,
        /* Real_8           */          AR_Float_IEEE_NR_64,
        /* Real_16          */          AR_Float_IEEE_NR_128,
        /* Complex_4        */          AR_Complex_IEEE_NR_32,
        /* Complex_8        */          AR_Complex_IEEE_NR_64,
        /* Complex_16       */          AR_Complex_IEEE_NR_128,
        /* CRI_Ptr_8        */          0,
        /* Logical_1        */          0,
        /* Logical_2        */          0,
        /* Logical_4        */          0,
        /* Logical_8        */          0,
        /* Character_1      */          0,
        /* Character_2      */          0,
        /* Character_4      */          0,
        /* CRI_Ch_Ptr_8     */          0,
        /* Structure_Type   */          0,
        /* CRI_Parcel_Ptr_8   */                0
                };

char arith_type_string[Num_Linear_Types][25] = {
        /* Err_Res          */          "",
        /* Short_Char_Const */          "",
        /* Short_Typeless_Const */      "",
        /* Typeless_1       */          "",
        /* Typeless_2       */          "",
        /* Typeless_4       */          "",
        /* Typeless_8       */          "",
        /* Long_Typeless    */          "",
        /* Integer_1        */          "AR_Int_32_U",
        /* Integer_2        */          "AR_Int_32_U",
        /* Integer_4        */          "AR_Int_32_U",
        /* Integer_8        */          "AR_Int_64_U",
        /* Real_4           */          "AR_Float_IEEE_NR_32",
        /* Real_8           */          "AR_Float_IEEE_NR_64",
        /* Real_16          */          "AR_Float_IEEE_NR_128",
        /* Complex_4        */          "AR_Complex_IEEE_NR_32",
        /* Complex_8        */          "AR_Complex_IEEE_NR_64",
        /* Complex_16       */          "AR_Complex_IEEE_NR_128",
        /* CRI_Ptr_8        */          "",
        /* Logical_1        */          "",
        /* Logical_2        */          "",
        /* Logical_4        */          "",
        /* Logical_8        */          "",
        /* Character_1      */          "",
        /* Character_2      */          "",
        /* Character_4      */          "",
        /* CRI_Ch_Ptr_8     */          "",
        /* Structure_Type   */          "",
        /* CRI_Parcel_Ptr_8   */        ""
                };

# else

/* _TARGET64 */
long input_arith_type[Num_Linear_Types] = {
        /* Err_Res          */          0,
        /* Short_Char_Const */          0,
        /* Short_Typeless_Const */      0,
        /* Typeless_1       */          0,
        /* Typeless_2       */          0,
        /* Typeless_4       */          0,
        /* Typeless_8       */          0,
        /* Long_Typeless    */          0,
# ifdef _TARGET_OS_MAX
        /* Integer_1        */          AR_Int_32_U,
        /* Integer_2        */          AR_Int_32_U,
        /* Integer_4        */          AR_Int_32_U,
# else
        /* Integer_1        */          AR_Int_64_U,
        /* Integer_2        */          AR_Int_64_U,
        /* Integer_4        */          AR_Int_64_U,
# endif
        /* Integer_8        */          AR_Int_64_U,
        /* Real_4           */          AR_Float_Cray1_64,
        /* Real_8           */          AR_Float_Cray1_64,
        /* Real_16          */          AR_Float_Cray1_128,
        /* Complex_4        */          AR_Complex_Cray1_64,
        /* Complex_8        */          AR_Complex_Cray1_64,
        /* Complex_16       */          AR_Complex_Cray1_128,
        /* CRI_Ptr_8        */          0,
        /* Logical_1        */          0,
        /* Logical_2        */          0,
        /* Logical_4        */          0,
        /* Logical_8        */          0,
        /* Character_1      */          0,
        /* Character_2      */          0,
        /* Character_4      */          0,
        /* CRI_Ch_Ptr_8     */          0,
        /* Structure_Type   */          0,
        /* CRI_Parcel_Ptr_8   */                0
                };

char arith_type_string[Num_Linear_Types][25] = {
        /* Err_Res          */          "",
        /* Short_Char_Const */          "",
        /* Short_Typeless_Const */      "",
        /* Typeless_1       */          "",
        /* Typeless_2       */          "",
        /* Typeless_4       */          "",
        /* Typeless_8       */          "",
        /* Long_Typeless    */          "",
# ifdef _TARGET_OS_MAX
        /* Integer_1        */          "AR_Int_32_U",
        /* Integer_2        */          "AR_Int_32_U",
        /* Integer_4        */          "AR_Int_32_U",
# else
        /* Integer_1        */          "AR_Int_64_U",
        /* Integer_2        */          "AR_Int_64_U",
        /* Integer_4        */          "AR_Int_64_U",
# endif
        /* Integer_8        */          "AR_Int_64_U",
        /* Real_4           */          "AR_Float_Cray1_64",
        /* Real_8           */          "AR_Float_Cray1_64",
        /* Real_16          */          "AR_Float_Cray1_128",
        /* Complex_4        */          "AR_Complex_Cray1_64",
        /* Complex_8        */          "AR_Complex_Cray1_64",
        /* Complex_16       */          "AR_Complex_Cray1_128",
        /* CRI_Ptr_8        */          "",
        /* Logical_1        */          "",
        /* Logical_2        */          "",
        /* Logical_4        */          "",
        /* Logical_8        */          "",
        /* Character_1      */          "",
        /* Character_2      */          "",
        /* Character_4      */          "",
        /* CRI_Ch_Ptr_8     */          "",
        /* Structure_Type   */          "",
        /* CRI_Parcel_Ptr_8   */        ""
                };

# endif

# endif



/***************************************\
|* These tables are from s_asg_expr.h. *|
\***************************************/

extern exp_tbl_type    eq_ne_tbl[Num_Linear_Types][Num_Linear_Types];
#ifdef KEY /* Bug 5710 */
extern exp_tbl_type    and_or_tbl[Num_Linear_Types][Num_Linear_Types];
extern exp_tbl_type eq_ne_on_logical_tbl[Num_Linear_Types][Num_Linear_Types];
#endif /* KEY Bug 5710 */
extern exp_tbl_type    lg_tbl[Num_Linear_Types][Num_Linear_Types];
extern exp_tbl_type    gt_lt_tbl[Num_Linear_Types][Num_Linear_Types];

extern void preprocess_only_driver(void);



/*******************************************\
|* Stuff that is needed globally in main.c *|
\*******************************************/

static int  some_scp_in_err;

boolean directives_are_global = TRUE;
boolean	insert_global_directives = FALSE;

boolean label_allowed = FALSE;

int curr_stmt_stk_il_idx = NULL_IDX;

int	active_forall_sh_idx = NULL_IDX;
boolean	within_forall_construct = FALSE;
boolean	within_forall_mask_expr = FALSE;

int	alloc_block_start_idx = NULL_IDX;
int	alloc_block_end_idx = NULL_IDX;
