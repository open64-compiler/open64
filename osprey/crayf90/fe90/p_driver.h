/*
 * Copyright (C) 2008. PathScale, LLC. All Rights Reserved.
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



/* USMID:  "\n@(#)5.0_pl/headers/p_driver.h	5.1	04/29/99 21:22:31\n" */

static	int			 blk_err_msgs[]		= {
				160,	/* Unknown_Blk        */
				15,	/* Blockdata_Blk      */
				19,	/* Module_Blk         */
				16,	/* Program_Blk	      */
				261,	/* Function_Blk       */
				272,	/* Subroutine_Blk     */
				164,	/* Internal_Blk       */
				165,	/* Module_Proc_Blk    */
				271,	/* Interface_Body_Blk */
				268,	/* Do_Blk             */
			       1597,	/* Forall_Blk         */
				160,	/* If_Blk             */
				267,	/* If_Then_Blk        */
				162,	/* If_Else_If_Blk     */
				163,	/* If_Else_Blk        */
				269,	/* Select_Blk         */
				183,	/* Case_Blk           */
				270,	/* Where_Then_Blk     */
				270,	/* Where_Else_Blk     */
				270,	/* Where_Else_Mask_Blk*/
                               1588,    /* Parallel_Blk	      */
                               1588,    /* Doall_Blk          */
                               1588,    /* Do_Parallel_Blk    */
                               1588,    /* Guard_Blk          */
                               1588,    /* Parallel_Case_Blk  */
                               1588,    /* Wait_Blk	      */
                               1588,	/* SGI_Doacross_Blk   */
                               1588,	/* SGI_Psection_Blk   */
                               1588,	/* SGI_Section_Blk    */
                               1588,	/* SGI_Pdo_Blk        */
                               1588,	/* SGI_Parallel_Do_Blk*/
                               1588,	/* SGI_Parallel_Blk   */
                               1588,	/* SGI_Critical_Section_Blk	*/
                               1588,	/* SGI_Single_Process_Blk	*/
                               1588,	/* SGI_Region_Blk		*/
                               1588,	/* Open_Mp_Parallel_Blk		*/
                               1588,	/* Open_Mp_Do_Blk		*/
                               1588,	/* Open_Mp_Parallel_Sections_Blk*/
                               1588,	/* Open_Mp_Sections_Blk		*/
                               1588,	/* Open_Mp_Section_Blk		*/
                               1588,	/* Open_Mp_Single_Blk		*/
                               1588,	/* Open_Mp_Parallel_Do_Blk	*/
                               1588,	/* Open_Mp_Master_Blk		*/
                               1588,	/* Open_Mp_Critical_Blk		*/
                               1588,	/* Open_Mp_Ordered_Blk		*/
                               1588,   /* Open_Mp_Workshare_Blk */ /* by jhs, 02/7/18 */
                               1588,   /* Open_Mp_Parallel_Workshare_Blk */ /* by jhs, 02/7/18 */
				264,	/* Contains_Blk       */
			 	26,	/* Interface_Blk      */
			 	25,	/* Derived_Type_Blk   */
#ifdef KEY /* Bug 10572 */
			       1686	/* Enum_Blk   */
#endif /* KEY Bug 10572 */
 				};

	blk_stk_type		*blk_stk;
	int			 blk_stk_idx		= NULL_IDX; 
	int			 blk_stk_inc		= 20;
	int			 blk_stk_init_size	= 20;
	int			 blk_stk_size		= 0;
	int			 blk_stk_limit		= (1 << 24) - 1;
	int			 blk_stk_num_wds	= NUM_BLK_STK_WDS;
	int			 blk_stk_largest_idx	= NULL_IDX; 

	boolean			 colon_recovery		= FALSE;
	stmt_category_type	 curr_stmt_category;

	boolean			 EOPU_encountered;

        /* The following entity is used in IF statement processing.           */
        /* Make the following variable static because only one copy is needed */
        /* by parse_if_stmt (even though it is called recursively to process  */
        /* a logical IF statement.)                                           */

	int		 	 if_stmt_lbl_idx;

	boolean			 label_ok		= TRUE;
	token_type		 label_token;

	token_type		 main_token;
	intent_type		 new_intent;
#ifdef KEY /* Bug 14150 */
	/* Pass "x" from parse of "bind(c, name=x)" to merge_bind(),
	 * imitating the bad example of "intent(x)" and "new_intent" by
	 * using a global variable. */
	token_type		 new_binding_label;
#endif /* KEY Bug 14150 */

	int			 stmt_construct_idx;



/* These strings are used to put out error messages about something that is   */
/* being added to the symbol table entry.                                     */

	char    *obj_str[Obj_Done]      =       {
                        "CHARACTER*(*)",           "explicit-shape DIMENSION",
                        "assumed-size DIMENSION",  "deferred-shape DIMENSION",
                        "assumed-shape DIMENSION", "co-array DIMENSION",
			"ALLOCATABLE",
#ifdef KEY /* Bug 14150 */
			"BIND",
			"VALUE",
#endif /* KEY Bug 14150 */
			"PARAMETER",
                        "INTENT",                  "OPTIONAL",
                        "PRIVATE",                 "PUBLIC",
                        "TARGET",                  "EQUIVALENCE",
                        "SAVE",                    "AUTOMATIC",
			"POINTER",		   "EXTERNAL",
	                "INTRINSIC",		   "DATA initialized",

                        /* NOTE:  type-spec needs to be long, because it is   */
                        /*        replaced by the type (ie: REAL) or each call*/

		        "type-spec                              ",
                        "VOLATILE",

                        /* directives */

			"COPY_ASSUMED_SHAPE",	   "AUXILIARY",
                        "VFUNCTION",               "NO SIDE EFFECTS",
			"SYMMETRIC",		   "INLINE",
			"IPA",
			"ALIGN_SYMBOL",		   "FILL_SYMBOL",
			"SECTION_GP",		   "SECTION_NON_GP",
			"IGNORE_TYPE_AND_KIND",	   "OPTIONAL",
			"NAME",

                        "Cray pointer",            "Cray pointee",
                        "Cray character pointee",  "result-name",
                        "dummy-argument",
                        "common-block-object",     "namelist-group-object",
                        "module-subprogram",       "type-name",
                        "generic-name",            "namelist-group-name",
                        "statement function",      "construct-name",
                        "function entry",          "subroutine entry",
                        "internal function",       "internal subroutine",
                        "module procedure function",
                        "module procedure subroutine",
                        "statement function dummy argument",
                        " ",
                        " ",
                        " ",
                        " ",
                        " ",
                        " ",
                        " ",
                        " ",
                        " "
			};


/******************************************************************************/
/*			token_to_stmt_type  TABLE			      */
/*									      */
/* THIS TABLE IS ORDER/ADDITION/DELETION DEPENDENT ON THE token_values	      */
/* enumeration defined in tokens.h					      */
/*									      */
/* NOTE: except for Tok_Id, an entry in this table of Assignment_Stmt implies */
/*	 that the token is not a valid beginning-of-stmt keyword	      */
/******************************************************************************/

static stmt_type_type		token_to_stmt_type [] = {
				Assignment_Stmt,      /* Tok_Label	      */
				Assignment_Stmt,      /* Tok_Id		      */
				Allocatable_Stmt,     /* Tok_Kwd_Allocatable  */
				Allocate_Stmt,	      /* Tok_Kwd_Allocate     */
				Assign_Stmt,	      /* Tok_Kwd_Assign	      */
				Assignment_Stmt,      /* Tok_Kwd_Assignment   */
				Automatic_Stmt,       /* Tok_Kwd_Automatic    */
				Backspace_Stmt,	      /* Tok_Kwd_Backspace    */
#ifdef KEY /* Bug 10572 */
				Bind_Stmt,	      /* Tok_Kwd_Bind         */
#endif /* KEY Bug 10572 */
				Blockdata_Stmt,       /* Tok_Kwd_Block	      */
				Buffer_Stmt,	      /* Tok_Kwd_Buffer	      */
				Type_Decl_Stmt,	      /* Tok_Kwd_Byte	      */
				Call_Stmt,	      /* Tok_Kwd_Call	      */
				Case_Stmt,	      /* Tok_Kwd_Case	      */
				Type_Decl_Stmt,	      /* Tok_Kwd_Character    */
				Close_Stmt,	      /* Tok_Kwd_Close	      */
				Common_Stmt,	      /* Tok_Kwd_Common	      */
				Type_Decl_Stmt,	      /* Tok_Kwd_Complex      */
				Contains_Stmt,	      /* Tok_Kwd_Contains     */
				Continue_Stmt,	      /* Tok_Kwd_Continue     */
				Cycle_Stmt,	      /* Tok_Kwd_Cycle	      */
				Data_Stmt,	      /* Tok_Kwd_Data	      */
				Deallocate_Stmt,      /* Tok_Kwd_Deallocate   */
				Decode_Stmt,	      /* Tok_Kwd_Decode	      */
				Assignment_Stmt,      /* Tok_Kwd_Default      */
				Dimension_Stmt,	      /* Tok_Kwd_Dimension    */
				Directive_Stmt,	      /* Tok_Kwd_Dir	      */
				Do_Iterative_Stmt,    /* Tok_Kwd_Do	      */
				Type_Decl_Stmt,	      /* Tok_Kwd_Double	      */
				Elemental_Stmt,	      /* Tok_Kwd_Elemental    */
				Else_Stmt,	      /* Tok_Kwd_Else	      */
				Encode_Stmt,	      /* Tok_Kwd_Encode	      */
				End_Stmt,	      /* Tok_Kwd_End	      */
				Entry_Stmt,	      /* Tok_Kwd_Entry	      */
#ifdef KEY /* Bug 10572 */
				Enum_Stmt,	      /* Tok_Kwd_Enum	      */
				Enumerator_Stmt,      /* Tok_Kwd_Enumerator   */
#endif /* KEY Bug 10572 */
				Equivalence_Stmt,     /* Tok_Kwd_Equivalence  */
				Exit_Stmt,	      /* Tok_Kwd_Exit	      */
				External_Stmt,	      /* Tok_Kwd_External     */
				Assignment_Stmt,      /* Tok_Kwd_File	      */
				Forall_Cstrct_Stmt,   /* Tok_Kwd_Forall	      */
				Format_Stmt,	      /* Tok_Kwd_Format	      */
				Function_Stmt,	      /* Tok_Kwd_Function     */
				Goto_Stmt,	      /* Tok_Kwd_Go	      */
				If_Cstrct_Stmt,	      /* Tok_Kwd_If	      */
				Implicit_Stmt,	      /* Tok_Kwd_Implicit     */
#ifdef KEY /* Bug 11741 */
				Import_Stmt,	      /* Tok_Kwd_Import       */
#endif /* KEY Bug 11741 */
				Assignment_Stmt,      /* Tok_Kwd_In	      */
				Inquire_Stmt,	      /* Tok_Kwd_Inquire      */
				Type_Decl_Stmt,	      /* Tok_Kwd_Integer      */
				Intent_Stmt,	      /* Tok_Kwd_Intent	      */
				Interface_Stmt,	      /* Tok_Kwd_Interface    */
				Intrinsic_Stmt,	      /* Tok_Kwd_Intrinsic    */
				Assignment_Stmt,      /* Tok_Kwd_Kind	      */
				Assignment_Stmt,      /* Tok_Kwd_Len	      */
				Type_Decl_Stmt,	      /* Tok_Kwd_Logical      */
				Module_Stmt,	      /* Tok_Kwd_Module	      */
#ifdef KEY /* Bug 10572 */
				Assignment_Stmt,      /* Tok_Kwd_Name         */
#endif /* KEY Bug 10572 */
				Namelist_Stmt,	      /* Tok_Kwd_Namelist     */
				Assignment_Stmt,      /* Tok_Kwd_None	      */
#ifdef KEY /* Bug 5089 */
				Assignment_Stmt,      /* Tok_Kwd_Nonintrinsic */
#endif /* KEY Bug 5089 */
				Nullify_Stmt,	      /* Tok_Kwd_Nullify      */
				Assignment_Stmt,      /* Tok_Kwd_Only	      */
				Open_Stmt,	      /* Tok_Kwd_Open	      */
				Assignment_Stmt,      /* Tok_Kwd_Operator     */
				Optional_Stmt,	      /* Tok_Kwd_Optional     */
				Assignment_Stmt,      /* Tok_Kwd_Out	      */
				Parameter_Stmt,	      /* Tok_Kwd_Parameter    */
				Pause_Stmt,	      /* Tok_Kwd_Pause	      */
				Pointer_Stmt,	      /* Tok_Kwd_Pointer      */
				Assignment_Stmt,      /* Tok_Kwd_Precision    */
				Print_Stmt,	      /* Tok_Kwd_Print	      */
				Private_Stmt,	      /* Tok_Kwd_Private      */
				Assignment_Stmt,      /* Tok_Kwd_Procedure    */
				Program_Stmt,	      /* Tok_Kwd_Program      */
				Public_Stmt,	      /* Tok_Kwd_Public	      */
				Pure_Stmt,	      /* Tok_Kwd_Pure	      */
				Read_Stmt,	      /* Tok_Kwd_Read	      */
				Type_Decl_Stmt,	      /* Tok_Kwd_Real	      */
				Recursive_Stmt,	      /* Tok_Kwd_Recursive    */
				Assignment_Stmt,      /* Tok_Kwd_Result	      */
				Return_Stmt,	      /* Tok_Kwd_Return	      */
				Rewind_Stmt,	      /* Tok_Kwd_Rewind	      */
				Save_Stmt,	      /* Tok_Kwd_Save	      */
				Select_Stmt,	      /* Tok_Kwd_Select	      */
				Sequence_Stmt,	      /* Tok_Kwd_Sequence     */
				Assignment_Stmt,      /* Tok_Kwd_Span         */
				Assignment_Stmt,      /* Tok_Kwd_Stat	      */
				Assignment_Stmt,      /* Tok_Kwd_Static	      */
				Stop_Stmt,	      /* Tok_Kwd_Stop	      */
				Subroutine_Stmt,      /* Tok_Kwd_Subroutine   */
				Target_Stmt,	      /* Tok_Kwd_Target	      */
                                Task_Common_Stmt,     /* Tok_Kwd_Task         */
				Assignment_Stmt,      /* Tok_Kwd_Then	      */
				Assignment_Stmt,      /* Tok_Kwd_To	      */
				Type_Decl_Stmt,	      /* Tok_Kwd_Type	      */
				Use_Stmt,	      /* Tok_Kwd_Use	      */
				Assignment_Stmt,      /* Tok_Kwd_Undefined    */
#ifdef KEY /* Bug 14150 */
				Value_Stmt,           /* Tok_Kwd_Value        */
#endif /* KEY Bug 14150 */
				Volatile_Stmt,        /* Tok_Kwd_Volatile     */
				Where_Cstrct_Stmt,    /* Tok_Kwd_Where	      */
				Assignment_Stmt,      /* Tok_Kwd_While	      */
				Write_Stmt };	      /* Tok_Kwd_Write	      */

/******************************************************************************/
/*			stmt_parsers  TABLE				      */
/*									      */
/* This table defines an array-of-pointers-to-functions-returning boolean     */
/* and is indexed by an object of type stmt_type_type which is the reason for */
/* the following dependency:						      */
/*									      */
/* THIS TABLE IS ORDER/ADDITION/DELETION DEPENDENT ON THE stmt_type_values    */
/* enumeration defined in globals.h					      */
/******************************************************************************/

void		(*stmt_parsers[]) () = {
				parse_bad_stmt,		/* Illegal stmt type  */

				parse_allocatable_stmt, /* Allocatable_Stmt   */
				parse_automatic_stmt,   /* Automatic_Stmt     */
				parse_common_stmt,	/* Common_Stmt	      */
				parse_contains_stmt,	/* Contains_Stmt      */
				parse_type_dcl_stmt,	/* Cpnt_Decl_Stmt     */
				parse_data_stmt,	/* Data_Stmt	      */
				parse_type_dcl_stmt,	/* Derived_Type_Stmt  */
				parse_dimension_stmt,	/* Dimension_Stmt     */
				parse_directive_stmt,	/* Directive_Stmt     */
				parse_equivalence_stmt, /* Equivalence_Stmt   */
				parse_external_stmt,	/* External_Stmt      */
				parse_format_stmt,	/* Format_Stmt	      */
				parse_implicit_stmt,	/* Implicit_Stmt      */
				parse_implicit_stmt,	/* Implicit_None_Stmt */
				parse_intent_stmt,	/* Intent_Stmt	      */
				parse_interface_stmt,	/* Interface_Stmt     */
				parse_intrinsic_stmt,	/* Intrinsic_Stmt     */
				parse_module_stmt,	/* Module_Proc_Stmt   */
				parse_namelist_stmt,	/* Namelist_Stmt      */
				parse_optional_stmt,	/* Optional_Stmt      */
				parse_parameter_stmt,	/* Parameter_Stmt     */
				parse_pointer_stmt,	/* Pointer_Stmt	      */
				parse_access_stmt,	/* Private_Stmt	      */
				parse_access_stmt,	/* Public_Stmt	      */
				parse_save_stmt,	/* Save_Stmt	      */
				parse_sequence_stmt,	/* Sequence_Stmt      */
				parse_stmt_func_stmt,	/* Stmt_Func_Stmt     */
				parse_target_stmt,	/* Target_Stmt	      */
                                parse_common_stmt, 	/* Task_Common_Stmt   */
				parse_type_dcl_stmt,	/* Type_Decl_Stmt     */
				parse_use_stmt,		/* Use_Stmt	      */

				parse_block_stmt,	/* Blockdata_Stmt     */
				parse_elemental_stmt,   /* Elemental_Stmt     */
				parse_function_stmt,	/* Function_Stmt      */
				parse_module_stmt,	/* Module_Stmt	      */
				parse_program_stmt,	/* Program_Stmt	      */
				parse_pure_stmt,	/* Pure_Stmt	      */
				parse_recursive_stmt,	/* Recursive_Stmt     */
				parse_subroutine_stmt,	/* Subroutine_Stmt    */

				parse_end_stmt,		/* End_Blockdata_Stmt */
				parse_end_stmt,		/* End_Do_Stmt        */
				parse_end_stmt,		/* End_Function_Stmt  */
				parse_end_stmt,		/* End_If_Stmt        */
				parse_end_stmt,		/* End_Interface_Stmt */
				parse_end_stmt,		/* End_Module_Stmt    */
				parse_end_stmt,		/* End_Program_Stmt   */
				parse_end_stmt,		/* End_Select_Stmt    */
				parse_end_stmt,		/* End_Stmt	      */
				parse_end_stmt,		/* End_Subroutine_Stmt*/
				parse_end_stmt,		/* End_Type_Stmt      */
				parse_end_stmt,		/* End_Where_Stmt     */

				parse_allocate_stmt,	/* Allocate_Stmt      */
				parse_if_stmt,		/* Arith_If_Stmt      */
				parse_assign_stmt,	/* Assign_Stmt	      */
				parse_assignment_stmt,	/* Assignment_Stmt    */
				parse_backspace_stmt,	/* Backspace_Stmt     */
				parse_buffer_stmt,	/* Buffer_Stmt	      */
				parse_call_stmt,	/* Call_Stmt	      */
				parse_case_stmt,	/* Case_Stmt	      */
				parse_close_stmt,	/* Close_Stmt	      */
				parse_continue_stmt,	/* Continue_Stmt      */
				parse_cycle_stmt,	/* Cycle_Stmt	      */
				parse_deallocate_stmt,	/* Deallocate_Stmt    */
				parse_decode_stmt,	/* Decode_Stmt	      */
				parse_do_stmt,		/* Do_Iterative_Stmt  */
				parse_do_stmt,		/* Do_While_Stmt      */
				parse_do_stmt,		/* Do_Infinite_Stmt   */
				parse_else_stmt,	/* Else_Stmt	      */
				parse_else_stmt,	/* Else_If_Stmt	      */
				parse_else_stmt,	/* Else_Where_Stmt    */
				parse_encode_stmt,	/* Encode_Stmt	      */
				parse_endfile_stmt,	/* Endfile_Stmt	      */
				parse_entry_stmt,	/* Entry_Stmt	      */
				parse_exit_stmt,	/* Exit_Stmt	      */
				parse_goto_stmt,	/* Goto_Stmt	      */
				parse_if_stmt,		/* If_Cstrct_Stmt     */
				parse_if_stmt,		/* If_Stmt	      */
				parse_inquire_stmt,	/* Inquire_Stmt	      */
				parse_nullify_stmt,	/* Nullify_Stmt	      */
				parse_open_stmt,	/* Open_Stmt	      */
				parse_if_stmt,		/* Outmoded_If_Stmt   */
				parse_stop_pause_stmt,	/* Pause_Stmt	      */
				parse_print_stmt,	/* Print_Stmt	      */
				parse_read_stmt,	/* Read_Stmt	      */
				parse_return_stmt,	/* Return_Stmt	      */
				parse_rewind_stmt,	/* Rewind_Stmt	      */
				parse_select_stmt,	/* Select_Stmt	      */
				parse_stop_pause_stmt,	/* Stop_Stmt	      */
				parse_if_stmt,		/* Then_Stmt	      */
				parse_where_stmt,	/* Where_Cstrct_Stmt  */
				parse_where_stmt,	/* Where_Stmt	      */
				parse_write_stmt,  	/* Write_Stmt	      */
				parse_bad_stmt,         /* Type_Init_Stmt     */

                                parse_bad_stmt,		/* Label_Def          */
                                parse_bad_stmt,  	/* Construct_Def      */

                                parse_bad_stmt,		/* Automatic_Base_Calc*/
                                parse_bad_stmt, 	/* Automatic_Base_Size*/
				parse_directive_stmt,	/* End_Parallel_Stmt  */
				parse_directive_stmt, /* End_Do_Parallel_Stmt */
				parse_directive_stmt, /*End_Parallel_Case_Stmt*/
				parse_directive_stmt,   /* Parallel_Case_Stmt */
				parse_directive_stmt,   /* End_Guard_Stmt     */
				parse_bad_stmt, 	/* Statement_Num_Stmt */
				parse_directive_stmt,   /* SGI_Section_Stmt   */
				parse_directive_stmt,/* SGI_End_Psection_Stmt */
				parse_directive_stmt,   /* SGI_End_Pdo_Stmt   */
				parse_directive_stmt,
                                              /* SGI_End_Parallel_Stmt     */
				parse_directive_stmt,
                                              /* SGI_End_Critical_Section_Stmt*/
				parse_directive_stmt,
                                                /*SGI_End_Single_Process_Stmt */
				parse_directive_stmt,   /* SGI_Region_End_Stmt*/
				parse_directive_stmt,
                                		/* Open_MP_Section_Stmt */
				parse_directive_stmt,
                                		/* Open_MP_End_Parallel_Stmt */
				parse_directive_stmt,
                                		/* Open_MP_End_Do_Stmt */
				parse_directive_stmt,
                               		/* Open_MP_End_Parallel_Sections_Stmt */
				parse_directive_stmt,
                                		/* Open_MP_End_Sections_Stmt */
				parse_directive_stmt,
                                		/* Open_MP_End_Section_Stmt */
				parse_directive_stmt,
                                		/* Open_MP_End_Single_Stmt */
				parse_directive_stmt,
                                	/* Open_MP_End_Parallel_Do_Stmt */
				parse_directive_stmt,
                                		/* Open_MP_End_Master_Stmt */
				parse_directive_stmt,
                                		/* Open_MP_End_Critical_Stmt */
				parse_directive_stmt,
                                		/* Open_MP_End_Ordered_Stmt */
				parse_forall,		/* Forall_Cstrct_Stmt */
				parse_forall,		/* Forall_Stmt        */
				parse_end_stmt,		/* End_Forall_Stmt    */
				parse_else_stmt,       /* Else_Where_Mask_Stmt*/
				parse_volatile_stmt,    /* Volatile_Stmt */
				parse_directive_stmt,
					/* Open_MP_End_Parallel_Workshare_Stmt */
				parse_directive_stmt,
					/* Open_MP_End_Workshare_Stmt */
#ifdef KEY /* Bug 11741 */
				parse_import_stmt,	/* Import_Stmt */
#endif /* KEY Bug 11741 */
#ifdef KEY /* Bug 10572 */
				parse_enum_stmt,	/* Enum_Stmt */
				parse_end_stmt,		/* End_Enum_Stmt */
				parse_enumerator_stmt,	/* Enumerator_Stmt */
#endif /* KEY Bug 10572 */
#ifdef KEY /* Bug 14150 */
				parse_bind_stmt,	/* Bind_Stmt */
				parse_value_stmt	/* Value_Stmt */
#endif /* KEY Bug 14150 */
				};


/* ************************************************************************** */
/*                           stmt_in_blk				      */
/* Dependent on stmt_type.  Blocks listed are blocks that the stmt can NOT be */
/* in.  Label_Def and Construct_Def do NOT exist in this table because they   */
/* are not user statement types.					      */
/* ************************************************************************** */

# ifdef _HOST32
# define ONE	1LL
# else
# define ONE	1L
# endif

long long     stmt_in_blk [] = {

			/*****  Null_Stmt  *****/

			       ((ONE << Unknown_Blk) |
				(ONE << Blockdata_Blk) |
				(ONE << Module_Blk) |
				(ONE << Program_Blk) |
				(ONE << Function_Blk) |
				(ONE << Subroutine_Blk) |
				(ONE << Internal_Blk) |
				(ONE << Module_Proc_Blk) |
				(ONE << Interface_Body_Blk) |
				(ONE << Forall_Blk) |
				(ONE << If_Blk) |
				(ONE << If_Then_Blk) |
				(ONE << If_Else_If_Blk) |
				(ONE << If_Else_Blk) |
				(ONE << Do_Blk) |
				(ONE << Select_Blk) |
				(ONE << Case_Blk) |
				(ONE << Where_Then_Blk) |
				(ONE << Where_Else_Blk) |
				(ONE << Where_Else_Mask_Blk) |
                                (ONE << Parallel_Blk) |
				(ONE << SGI_Parallel_Blk) |
                                (ONE << Doall_Blk) |
				(ONE << Wait_Blk) |
				(ONE << SGI_Doacross_Blk) |
				(ONE << SGI_Parallel_Do_Blk) |
                                (ONE << Do_Parallel_Blk) |
				(ONE << SGI_Pdo_Blk) |
                                (ONE << Guard_Blk) |
				(ONE << SGI_Critical_Section_Blk) |
                                (ONE << Parallel_Case_Blk) |
				(ONE << SGI_Psection_Blk) |
				(ONE << SGI_Section_Blk) |
				(ONE << SGI_Single_Process_Blk) |
				(ONE << SGI_Region_Blk) |
                                (ONE << Open_Mp_Parallel_Blk) |
                                (ONE << Open_Mp_Do_Blk) |
                                (ONE << Open_Mp_Parallel_Sections_Blk) |
                                (ONE << Open_Mp_Sections_Blk) |
                                (ONE << Open_Mp_Section_Blk) |
                                (ONE << Open_Mp_Single_Blk) |
                                (ONE << Open_Mp_Parallel_Do_Blk) |
                                (ONE << Open_Mp_Master_Blk) |
                                (ONE << Open_Mp_Critical_Blk) |
                                (ONE << Open_Mp_Ordered_Blk) |
                                (ONE << Open_Mp_Workshare_Blk) | /* by jhs, 02/7/18 */
                                (ONE << Open_Mp_Parallel_Workshare_Blk) | /* by jhs, 02/7/18 */
				(ONE << Contains_Blk) |
				(ONE << Interface_Blk) |
				(ONE << Derived_Type_Blk) | 
				(ONE << Enum_Blk)),

		/*****  Allocatable_Stmt  *****/

			       ((ONE << Unknown_Blk) |
				(ONE << Blockdata_Blk) |
				(ONE << Forall_Blk) |
				(ONE << If_Blk) |
				(ONE << If_Then_Blk) |
				(ONE << If_Else_If_Blk) |
				(ONE << If_Else_Blk) |
				(ONE << Do_Blk) |
				(ONE << Select_Blk) |
				(ONE << Case_Blk) |
				(ONE << Where_Then_Blk) |
				(ONE << Where_Else_Blk) |
				(ONE << Where_Else_Mask_Blk) |
                                (ONE << Parallel_Blk) |
				(ONE << SGI_Parallel_Blk) |
                                (ONE << Doall_Blk) |
				(ONE << Wait_Blk) |
				(ONE << SGI_Doacross_Blk) |
				(ONE << SGI_Parallel_Do_Blk) |
                                (ONE << Do_Parallel_Blk) |
				(ONE << SGI_Pdo_Blk) |
                                (ONE << Guard_Blk) |
				(ONE << SGI_Critical_Section_Blk) |
                                (ONE << Parallel_Case_Blk) |
				(ONE << SGI_Psection_Blk) |
				(ONE << SGI_Section_Blk) |
				(ONE << SGI_Single_Process_Blk) |
                                (ONE << Open_Mp_Parallel_Blk) |
                                (ONE << Open_Mp_Do_Blk) |
                                (ONE << Open_Mp_Parallel_Sections_Blk) |
                                (ONE << Open_Mp_Sections_Blk) |
                                (ONE << Open_Mp_Section_Blk) |
                                (ONE << Open_Mp_Single_Blk) |
                                (ONE << Open_Mp_Parallel_Do_Blk) |
                                (ONE << Open_Mp_Master_Blk) |
                                (ONE << Open_Mp_Critical_Blk) |
                                (ONE << Open_Mp_Ordered_Blk) |
                                (ONE << Open_Mp_Workshare_Blk) | /* by jhs, 02/7/18 */
                                (ONE << Open_Mp_Parallel_Workshare_Blk) | /* by jhs, 02/7/18 */
				(ONE << Contains_Blk) |
				(ONE << Interface_Blk) |
				(ONE << Derived_Type_Blk) | 
				(ONE << Enum_Blk)),

			/*****  Automatic_Stmt   *****/

			       ((ONE << Unknown_Blk) |
				(ONE << Module_Blk) |
				(ONE << Forall_Blk) |
				(ONE << If_Blk) |
				(ONE << If_Then_Blk) |
				(ONE << If_Else_If_Blk) |
				(ONE << If_Else_Blk) |
				(ONE << Do_Blk) |
				(ONE << Select_Blk) |
				(ONE << Case_Blk) |
				(ONE << Where_Then_Blk) |
				(ONE << Where_Else_Blk) |
				(ONE << Where_Else_Mask_Blk) |
                                (ONE << Parallel_Blk) |
				(ONE << SGI_Parallel_Blk) |
                                (ONE << Doall_Blk) |
				(ONE << Wait_Blk) |
				(ONE << SGI_Doacross_Blk) |
				(ONE << SGI_Parallel_Do_Blk) |
                                (ONE << Do_Parallel_Blk) |
				(ONE << SGI_Pdo_Blk) |
                                (ONE << Guard_Blk) |
				(ONE << SGI_Critical_Section_Blk) |
                                (ONE << Parallel_Case_Blk) |
				(ONE << SGI_Psection_Blk) |
				(ONE << SGI_Section_Blk) |
				(ONE << SGI_Single_Process_Blk) |
                                (ONE << Open_Mp_Parallel_Blk) |
                                (ONE << Open_Mp_Do_Blk) |
                                (ONE << Open_Mp_Parallel_Sections_Blk) |
                                (ONE << Open_Mp_Sections_Blk) |
                                (ONE << Open_Mp_Section_Blk) |
                                (ONE << Open_Mp_Single_Blk) |
                                (ONE << Open_Mp_Parallel_Do_Blk) |
                                (ONE << Open_Mp_Master_Blk) |
                                (ONE << Open_Mp_Critical_Blk) |
                                (ONE << Open_Mp_Ordered_Blk) |
                                (ONE << Open_Mp_Workshare_Blk) | /* by jhs, 02/7/18 */
                                (ONE << Open_Mp_Parallel_Workshare_Blk) | /* by jhs, 02/7/18 */
				(ONE << Contains_Blk) |
				(ONE << Interface_Blk) |
				(ONE << Derived_Type_Blk) | 
				(ONE << Enum_Blk)),

			/*****  Common_Stmt      *****/

			       ((ONE << Unknown_Blk) |
				(ONE << Forall_Blk) |
				(ONE << If_Blk) |
				(ONE << If_Then_Blk) |
				(ONE << If_Else_If_Blk) |
				(ONE << If_Else_Blk) |
				(ONE << Do_Blk) |
				(ONE << Select_Blk) |
				(ONE << Case_Blk) |
				(ONE << Where_Then_Blk) |
				(ONE << Where_Else_Blk) |
				(ONE << Where_Else_Mask_Blk) |
                                (ONE << Parallel_Blk) |
				(ONE << SGI_Parallel_Blk) |
                                (ONE << Doall_Blk) |
				(ONE << Wait_Blk) |
				(ONE << SGI_Doacross_Blk) |
				(ONE << SGI_Parallel_Do_Blk) |
                                (ONE << Do_Parallel_Blk) |
				(ONE << SGI_Pdo_Blk) |
                                (ONE << Guard_Blk) |
				(ONE << SGI_Critical_Section_Blk) |
                                (ONE << Parallel_Case_Blk) |
				(ONE << SGI_Psection_Blk) |
				(ONE << SGI_Section_Blk) |
				(ONE << SGI_Single_Process_Blk) |
                                (ONE << Open_Mp_Parallel_Blk) |
                                (ONE << Open_Mp_Do_Blk) |
                                (ONE << Open_Mp_Parallel_Sections_Blk) |
                                (ONE << Open_Mp_Sections_Blk) |
                                (ONE << Open_Mp_Section_Blk) |
                                (ONE << Open_Mp_Single_Blk) |
                                (ONE << Open_Mp_Parallel_Do_Blk) |
                                (ONE << Open_Mp_Master_Blk) |
                                (ONE << Open_Mp_Critical_Blk) |
                                (ONE << Open_Mp_Ordered_Blk) |
                                (ONE << Open_Mp_Workshare_Blk) | /* by jhs, 02/7/18 */
                                (ONE << Open_Mp_Parallel_Workshare_Blk) | /* by jhs, 02/7/18 */
				(ONE << Contains_Blk) |
				(ONE << Interface_Blk) |
				(ONE << Derived_Type_Blk) | 
				(ONE << Enum_Blk)),

			/*****  Contains_Stmt    *****/

			       ((ONE << Unknown_Blk) |
				(ONE << Blockdata_Blk) |
				(ONE << Interface_Body_Blk) |
				(ONE << Internal_Blk) |
				(ONE << Forall_Blk) |
				(ONE << If_Blk) |
				(ONE << If_Then_Blk) |
				(ONE << If_Else_If_Blk) |
				(ONE << If_Else_Blk) |
				(ONE << Do_Blk) |
				(ONE << Select_Blk) |
				(ONE << Case_Blk) |
				(ONE << Where_Then_Blk) |
				(ONE << Where_Else_Blk) |
				(ONE << Where_Else_Mask_Blk) |
                                (ONE << Parallel_Blk) |
				(ONE << SGI_Parallel_Blk) |
                                (ONE << Doall_Blk) |
				(ONE << Wait_Blk) |
				(ONE << SGI_Doacross_Blk) |
				(ONE << SGI_Parallel_Do_Blk) |
                                (ONE << Do_Parallel_Blk) |
				(ONE << SGI_Pdo_Blk) |
                                (ONE << Guard_Blk) |
				(ONE << SGI_Critical_Section_Blk) |
                                (ONE << Parallel_Case_Blk) |
				(ONE << SGI_Psection_Blk) |
				(ONE << SGI_Section_Blk) |
				(ONE << SGI_Single_Process_Blk) |
                                (ONE << Open_Mp_Parallel_Blk) |
                                (ONE << Open_Mp_Do_Blk) |
                                (ONE << Open_Mp_Parallel_Sections_Blk) |
                                (ONE << Open_Mp_Sections_Blk) |
                                (ONE << Open_Mp_Section_Blk) |
                                (ONE << Open_Mp_Single_Blk) |
                                (ONE << Open_Mp_Parallel_Do_Blk) |
                                (ONE << Open_Mp_Master_Blk) |
                                (ONE << Open_Mp_Critical_Blk) |
                                (ONE << Open_Mp_Ordered_Blk) |
                                (ONE << Open_Mp_Workshare_Blk) | /* by jhs, 02/7/18 */
                                (ONE << Open_Mp_Parallel_Workshare_Blk) | /* by jhs, 02/7/18 */
				(ONE << Contains_Blk) |
				(ONE << Interface_Blk) |
				(ONE << Derived_Type_Blk) | 
				(ONE << Enum_Blk)),

			/*****  Cpnt_Decl_Stmt   *****/

			       ((ONE << Unknown_Blk) |
				(ONE << Blockdata_Blk) |
				(ONE << Module_Blk) |
				(ONE << Program_Blk) |
				(ONE << Function_Blk) |
				(ONE << Subroutine_Blk) |
				(ONE << Internal_Blk) |
				(ONE << Module_Proc_Blk) |
				(ONE << Interface_Body_Blk) |
				(ONE << Do_Blk) |
				(ONE << Forall_Blk) |
				(ONE << If_Blk) |
				(ONE << If_Then_Blk) |
				(ONE << If_Else_If_Blk) |
				(ONE << If_Else_Blk) |
				(ONE << Select_Blk) |
				(ONE << Case_Blk) |
				(ONE << Where_Then_Blk) |
				(ONE << Where_Else_Blk) |
				(ONE << Where_Else_Mask_Blk) |
                                (ONE << Parallel_Blk) |
				(ONE << SGI_Parallel_Blk) |
                                (ONE << Doall_Blk) |
				(ONE << Wait_Blk) |
				(ONE << SGI_Doacross_Blk) |
				(ONE << SGI_Parallel_Do_Blk) |
                                (ONE << Do_Parallel_Blk) |
				(ONE << SGI_Pdo_Blk) |
                                (ONE << Guard_Blk) |
				(ONE << SGI_Critical_Section_Blk) |
                                (ONE << Parallel_Case_Blk) |
				(ONE << SGI_Psection_Blk) |
				(ONE << SGI_Section_Blk) |
				(ONE << SGI_Single_Process_Blk) |
                                (ONE << Open_Mp_Parallel_Blk) |
                                (ONE << Open_Mp_Do_Blk) |
                                (ONE << Open_Mp_Parallel_Sections_Blk) |
                                (ONE << Open_Mp_Sections_Blk) |
                                (ONE << Open_Mp_Section_Blk) |
                                (ONE << Open_Mp_Single_Blk) |
                                (ONE << Open_Mp_Parallel_Do_Blk) |
                                (ONE << Open_Mp_Master_Blk) |
                                (ONE << Open_Mp_Critical_Blk) |
                                (ONE << Open_Mp_Ordered_Blk) |
                                (ONE << Open_Mp_Workshare_Blk) | /* by jhs, 02/7/18 */
                                (ONE << Open_Mp_Parallel_Workshare_Blk) | /* by jhs, 02/7/18 */
				(ONE << Contains_Blk) |
				(ONE << Interface_Blk) |    
				(ONE << Enum_Blk)),

			/*****  Data_Stmt  *****/

			       ((ONE << Unknown_Blk) |
				(ONE << Interface_Body_Blk) |
				(ONE << Forall_Blk) |
				(ONE << If_Blk) |
				(ONE << Select_Blk) |
				(ONE << Where_Then_Blk) |
				(ONE << Where_Else_Blk) |
				(ONE << Where_Else_Mask_Blk) |
				(ONE << SGI_Psection_Blk) |
				(ONE << Contains_Blk) |
				(ONE << Interface_Blk) |
				(ONE << Derived_Type_Blk) | 
				(ONE << Enum_Blk)),

			/*****  Derived_Type_Stmt  *****/

			       ((ONE << Unknown_Blk) |
				(ONE << Forall_Blk) |
				(ONE << If_Blk) |
				(ONE << If_Then_Blk) |
				(ONE << If_Else_If_Blk) |
				(ONE << If_Else_Blk) |
				(ONE << Do_Blk) |
				(ONE << Select_Blk) |
				(ONE << Case_Blk) |
				(ONE << Where_Then_Blk) |
				(ONE << Where_Else_Blk) |
				(ONE << Where_Else_Mask_Blk) |
                                (ONE << Parallel_Blk) |
				(ONE << SGI_Parallel_Blk) |
                                (ONE << Doall_Blk) |
				(ONE << Wait_Blk) |
				(ONE << SGI_Doacross_Blk) |
				(ONE << SGI_Parallel_Do_Blk) |
                                (ONE << Do_Parallel_Blk) |
				(ONE << SGI_Pdo_Blk) |
                                (ONE << Guard_Blk) |
				(ONE << SGI_Critical_Section_Blk) |
                                (ONE << Parallel_Case_Blk) |
				(ONE << SGI_Psection_Blk) |
				(ONE << SGI_Section_Blk) |
				(ONE << SGI_Single_Process_Blk) |
                                (ONE << Open_Mp_Parallel_Blk) |
                                (ONE << Open_Mp_Do_Blk) |
                                (ONE << Open_Mp_Parallel_Sections_Blk) |
                                (ONE << Open_Mp_Sections_Blk) |
                                (ONE << Open_Mp_Section_Blk) |
                                (ONE << Open_Mp_Single_Blk) |
                                (ONE << Open_Mp_Parallel_Do_Blk) |
                                (ONE << Open_Mp_Master_Blk) |
                                (ONE << Open_Mp_Critical_Blk) |
                                (ONE << Open_Mp_Ordered_Blk) |
                                (ONE << Open_Mp_Workshare_Blk) | /* by jhs, 02/7/18 */
                                (ONE << Open_Mp_Parallel_Workshare_Blk) | /* by jhs, 02/7/18 */
				(ONE << Contains_Blk) |
				(ONE << Interface_Blk) |
				(ONE << Derived_Type_Blk) | 
				(ONE << Enum_Blk)),

			/*****  Dimension_Stmt  *****/

			       ((ONE << Unknown_Blk) |
				(ONE << Forall_Blk) |
				(ONE << If_Blk) |
				(ONE << If_Then_Blk) |
				(ONE << If_Else_If_Blk) |
				(ONE << If_Else_Blk) |
				(ONE << Do_Blk) |
				(ONE << Select_Blk) |
				(ONE << Case_Blk) |
				(ONE << Where_Then_Blk) |
				(ONE << Where_Else_Blk) |
				(ONE << Where_Else_Mask_Blk) |
                                (ONE << Parallel_Blk) |
				(ONE << SGI_Parallel_Blk) |
                                (ONE << Doall_Blk) |
				(ONE << Wait_Blk) |
				(ONE << SGI_Doacross_Blk) |
				(ONE << SGI_Parallel_Do_Blk) |
                                (ONE << Do_Parallel_Blk) |
				(ONE << SGI_Pdo_Blk) |
                                (ONE << Guard_Blk) |
				(ONE << SGI_Critical_Section_Blk) |
                                (ONE << Parallel_Case_Blk) |
				(ONE << SGI_Psection_Blk) |
				(ONE << SGI_Section_Blk) |
				(ONE << SGI_Single_Process_Blk) |
                                (ONE << Open_Mp_Parallel_Blk) |
                                (ONE << Open_Mp_Do_Blk) |
                                (ONE << Open_Mp_Parallel_Sections_Blk) |
                                (ONE << Open_Mp_Sections_Blk) |
                                (ONE << Open_Mp_Section_Blk) |
                                (ONE << Open_Mp_Single_Blk) |
                                (ONE << Open_Mp_Parallel_Do_Blk) |
                                (ONE << Open_Mp_Master_Blk) |
                                (ONE << Open_Mp_Critical_Blk) |
                                (ONE << Open_Mp_Ordered_Blk) |
                                (ONE << Open_Mp_Workshare_Blk) | /* by jhs, 02/7/18 */
                                (ONE << Open_Mp_Parallel_Workshare_Blk) | /* by jhs, 02/7/18 */
				(ONE << Contains_Blk) |
				(ONE << Interface_Blk) |
				(ONE << Derived_Type_Blk) | 
				(ONE << Enum_Blk)),

			/*****  Directive_Stmt  *****/

			       ((ONE << Unknown_Blk) |      
				(ONE << If_Blk) |
				(ONE << Enum_Blk)),

			/*****  Equivalence_Stmt  *****/

			       ((ONE << Unknown_Blk) |
				(ONE << Forall_Blk) |
				(ONE << If_Blk) |
				(ONE << If_Then_Blk) |
				(ONE << If_Else_If_Blk) |
				(ONE << If_Else_Blk) |
				(ONE << Do_Blk) |
				(ONE << Select_Blk) |
				(ONE << Case_Blk) |
				(ONE << Where_Then_Blk) |
				(ONE << Where_Else_Blk) |
				(ONE << Where_Else_Mask_Blk) |
                                (ONE << Parallel_Blk) |
				(ONE << SGI_Parallel_Blk) |
                                (ONE << Doall_Blk) |
				(ONE << Wait_Blk) |
				(ONE << SGI_Doacross_Blk) |
				(ONE << SGI_Parallel_Do_Blk) |
                                (ONE << Do_Parallel_Blk) |
				(ONE << SGI_Pdo_Blk) |
                                (ONE << Guard_Blk) |
				(ONE << SGI_Critical_Section_Blk) |
                                (ONE << Parallel_Case_Blk) |
				(ONE << SGI_Psection_Blk) |
				(ONE << SGI_Section_Blk) |
				(ONE << SGI_Single_Process_Blk) |
                                (ONE << Open_Mp_Parallel_Blk) |
                                (ONE << Open_Mp_Do_Blk) |
                                (ONE << Open_Mp_Parallel_Sections_Blk) |
                                (ONE << Open_Mp_Sections_Blk) |
                                (ONE << Open_Mp_Section_Blk) |
                                (ONE << Open_Mp_Single_Blk) |
                                (ONE << Open_Mp_Parallel_Do_Blk) |
                                (ONE << Open_Mp_Master_Blk) |
                                (ONE << Open_Mp_Critical_Blk) |
                                (ONE << Open_Mp_Ordered_Blk) |
                                (ONE << Open_Mp_Workshare_Blk) | /* by jhs, 02/7/18 */
                                (ONE << Open_Mp_Parallel_Workshare_Blk) | /* by jhs, 02/7/18 */
				(ONE << Contains_Blk) |
				(ONE << Interface_Blk) |
				(ONE << Derived_Type_Blk) | 
				(ONE << Enum_Blk)),

			/*****  External_Stmt  *****/

			       ((ONE << Unknown_Blk) |
				(ONE << Blockdata_Blk) |
				(ONE << Forall_Blk) |
				(ONE << If_Blk) |
				(ONE << If_Then_Blk) |
				(ONE << If_Else_If_Blk) |
				(ONE << If_Else_Blk) |
				(ONE << Do_Blk) |
				(ONE << Select_Blk) |
				(ONE << Case_Blk) |
				(ONE << Where_Then_Blk) |
				(ONE << Where_Else_Blk) |
				(ONE << Where_Else_Mask_Blk) |
                                (ONE << Parallel_Blk) |
				(ONE << SGI_Parallel_Blk) |
                                (ONE << Doall_Blk) |
				(ONE << Wait_Blk) |
				(ONE << SGI_Doacross_Blk) |
				(ONE << SGI_Parallel_Do_Blk) |
                                (ONE << Do_Parallel_Blk) |
				(ONE << SGI_Pdo_Blk) |
                                (ONE << Guard_Blk) |
				(ONE << SGI_Critical_Section_Blk) |
                                (ONE << Parallel_Case_Blk) |
				(ONE << SGI_Psection_Blk) |
				(ONE << SGI_Section_Blk) |
				(ONE << SGI_Single_Process_Blk) |
                                (ONE << Open_Mp_Parallel_Blk) |
                                (ONE << Open_Mp_Do_Blk) |
                                (ONE << Open_Mp_Parallel_Sections_Blk) |
                                (ONE << Open_Mp_Sections_Blk) |
                                (ONE << Open_Mp_Section_Blk) |
                                (ONE << Open_Mp_Single_Blk) |
                                (ONE << Open_Mp_Parallel_Do_Blk) |
                                (ONE << Open_Mp_Master_Blk) |
                                (ONE << Open_Mp_Critical_Blk) |
                                (ONE << Open_Mp_Ordered_Blk) |
                                (ONE << Open_Mp_Workshare_Blk) | /* by jhs, 02/7/18 */
                                (ONE << Open_Mp_Parallel_Workshare_Blk) | /* by jhs, 02/7/18 */
				(ONE << Contains_Blk) |
				(ONE << Interface_Blk) |
				(ONE << Derived_Type_Blk) | 
				(ONE << Enum_Blk)),

			/*****  Format_Stmt  *****/

			       ((ONE << Unknown_Blk) |
				(ONE << Blockdata_Blk) |
				(ONE << Module_Blk) |
				(ONE << Interface_Body_Blk) |
				(ONE << Forall_Blk) |
				(ONE << If_Blk) |
				(ONE << Select_Blk) |
				(ONE << Where_Then_Blk) |
				(ONE << Where_Else_Blk) |
				(ONE << Where_Else_Mask_Blk) |
				(ONE << SGI_Psection_Blk) |
				(ONE << Contains_Blk) |
				(ONE << Interface_Blk) |
				(ONE << Derived_Type_Blk) | 
				(ONE << Enum_Blk)),

			/*****  Implicit_Stmt  *****/

			       ((ONE << Unknown_Blk) |
				(ONE << Forall_Blk) |
				(ONE << If_Blk) |
				(ONE << If_Then_Blk) |
				(ONE << If_Else_If_Blk) |
				(ONE << If_Else_Blk) |
				(ONE << Do_Blk) |
				(ONE << Select_Blk) |
				(ONE << Case_Blk) |
				(ONE << Where_Then_Blk) |
				(ONE << Where_Else_Blk) |
				(ONE << Where_Else_Mask_Blk) |
                                (ONE << Parallel_Blk) |
				(ONE << SGI_Parallel_Blk) |
                                (ONE << Doall_Blk) |
				(ONE << Wait_Blk) |
				(ONE << SGI_Doacross_Blk) |
				(ONE << SGI_Parallel_Do_Blk) |
                                (ONE << Do_Parallel_Blk) |
				(ONE << SGI_Pdo_Blk) |
                                (ONE << Guard_Blk) |
				(ONE << SGI_Critical_Section_Blk) |
                                (ONE << Parallel_Case_Blk) |
				(ONE << SGI_Psection_Blk) |
				(ONE << SGI_Section_Blk) |
				(ONE << SGI_Single_Process_Blk) |
                                (ONE << Open_Mp_Parallel_Blk) |
                                (ONE << Open_Mp_Do_Blk) |
                                (ONE << Open_Mp_Parallel_Sections_Blk) |
                                (ONE << Open_Mp_Sections_Blk) |
                                (ONE << Open_Mp_Section_Blk) |
                                (ONE << Open_Mp_Single_Blk) |
                                (ONE << Open_Mp_Parallel_Do_Blk) |
                                (ONE << Open_Mp_Master_Blk) |
                                (ONE << Open_Mp_Critical_Blk) |
                                (ONE << Open_Mp_Ordered_Blk) |
                                (ONE << Open_Mp_Workshare_Blk) | /* by jhs, 02/7/18 */
                                (ONE << Open_Mp_Parallel_Workshare_Blk) | /* by jhs, 02/7/18 */
				(ONE << Contains_Blk) |
				(ONE << Interface_Blk) |
				(ONE << Derived_Type_Blk) | 
				(ONE << Enum_Blk)),

			/***** Implicit_None_Stmt  *****/

			       ((ONE << Unknown_Blk) |
				(ONE << Forall_Blk) |
				(ONE << If_Blk) |
				(ONE << If_Then_Blk) |
				(ONE << If_Else_If_Blk) |
				(ONE << If_Else_Blk) |
				(ONE << Do_Blk) |
				(ONE << Select_Blk) |
				(ONE << Case_Blk) |
				(ONE << Where_Then_Blk) |
				(ONE << Where_Else_Blk) |
				(ONE << Where_Else_Mask_Blk) |
                                (ONE << Parallel_Blk) |
				(ONE << SGI_Parallel_Blk) |
                                (ONE << Doall_Blk) |
				(ONE << Wait_Blk) |
				(ONE << SGI_Doacross_Blk) |
				(ONE << SGI_Parallel_Do_Blk) |
                                (ONE << Do_Parallel_Blk) |
				(ONE << SGI_Pdo_Blk) |
                                (ONE << Guard_Blk) |
				(ONE << SGI_Critical_Section_Blk) |
                                (ONE << Parallel_Case_Blk) |
				(ONE << SGI_Psection_Blk) |
				(ONE << SGI_Section_Blk) |
				(ONE << SGI_Single_Process_Blk) |
                                (ONE << Open_Mp_Parallel_Blk) |
                                (ONE << Open_Mp_Do_Blk) |
                                (ONE << Open_Mp_Parallel_Sections_Blk) |
                                (ONE << Open_Mp_Sections_Blk) |
                                (ONE << Open_Mp_Section_Blk) |
                                (ONE << Open_Mp_Single_Blk) |
                                (ONE << Open_Mp_Parallel_Do_Blk) |
                                (ONE << Open_Mp_Master_Blk) |
                                (ONE << Open_Mp_Critical_Blk) |
                                (ONE << Open_Mp_Ordered_Blk) |
                                (ONE << Open_Mp_Workshare_Blk) | /* by jhs, 02/7/18 */
                                (ONE << Open_Mp_Parallel_Workshare_Blk) | /* by jhs, 02/7/18 */
				(ONE << Contains_Blk) |
				(ONE << Interface_Blk) |
				(ONE << Derived_Type_Blk) | 
				(ONE << Enum_Blk)),

			/*****  Intent_Stmt  *****/

			       ((ONE << Unknown_Blk) |
				(ONE << Blockdata_Blk) |
				(ONE << Module_Blk) |
				(ONE << Program_Blk) |
				(ONE << Forall_Blk) |
				(ONE << If_Blk) |
				(ONE << If_Then_Blk) |
				(ONE << If_Else_If_Blk) |
				(ONE << If_Else_Blk) |
				(ONE << Do_Blk) |
				(ONE << Select_Blk) |
				(ONE << Case_Blk) |
				(ONE << Where_Then_Blk) |
				(ONE << Where_Else_Blk) |
				(ONE << Where_Else_Mask_Blk) |
                                (ONE << Parallel_Blk) |
				(ONE << SGI_Parallel_Blk) |
                                (ONE << Doall_Blk) |
				(ONE << Wait_Blk) |
				(ONE << SGI_Doacross_Blk) |
				(ONE << SGI_Parallel_Do_Blk) |
                                (ONE << Do_Parallel_Blk) |
				(ONE << SGI_Pdo_Blk) |
                                (ONE << Guard_Blk) |
				(ONE << SGI_Critical_Section_Blk) |
                                (ONE << Parallel_Case_Blk) |
				(ONE << SGI_Psection_Blk) |
				(ONE << SGI_Section_Blk) |
				(ONE << SGI_Single_Process_Blk) |
                                (ONE << Open_Mp_Parallel_Blk) |
                                (ONE << Open_Mp_Do_Blk) |
                                (ONE << Open_Mp_Parallel_Sections_Blk) |
                                (ONE << Open_Mp_Sections_Blk) |
                                (ONE << Open_Mp_Section_Blk) |
                                (ONE << Open_Mp_Single_Blk) |
                                (ONE << Open_Mp_Parallel_Do_Blk) |
                                (ONE << Open_Mp_Master_Blk) |
                                (ONE << Open_Mp_Critical_Blk) |
                                (ONE << Open_Mp_Ordered_Blk) |
                                (ONE << Open_Mp_Workshare_Blk) | /* by jhs, 02/7/18 */
                                (ONE << Open_Mp_Parallel_Workshare_Blk) | /* by jhs, 02/7/18 */
				(ONE << Contains_Blk) |
				(ONE << Interface_Blk) |
				(ONE << Derived_Type_Blk) | 
				(ONE << Enum_Blk)),

			/*****  Interface_Stmt  *****/

			       ((ONE << Unknown_Blk) |
				(ONE << Blockdata_Blk) |
				(0 << Interface_Body_Blk) |
				(ONE << Forall_Blk) |
				(ONE << If_Blk) |
				(ONE << If_Then_Blk) |
				(ONE << If_Else_If_Blk) |
				(ONE << If_Else_Blk) |
				(ONE << Do_Blk) |
				(ONE << Select_Blk) |
				(ONE << Case_Blk) |
				(ONE << Where_Then_Blk) |
				(ONE << Where_Else_Blk) |
				(ONE << Where_Else_Mask_Blk) |
                                (ONE << Parallel_Blk) |
				(ONE << SGI_Parallel_Blk) |
                                (ONE << Doall_Blk) |
				(ONE << Wait_Blk) |
				(ONE << SGI_Doacross_Blk) |
				(ONE << SGI_Parallel_Do_Blk) |
                                (ONE << Do_Parallel_Blk) |
				(ONE << SGI_Pdo_Blk) |
                                (ONE << Guard_Blk) |
				(ONE << SGI_Critical_Section_Blk) |
                                (ONE << Parallel_Case_Blk) |
				(ONE << SGI_Psection_Blk) |
				(ONE << SGI_Section_Blk) |
				(ONE << SGI_Single_Process_Blk) |
                                (ONE << Open_Mp_Parallel_Blk) |
                                (ONE << Open_Mp_Do_Blk) |
                                (ONE << Open_Mp_Parallel_Sections_Blk) |
                                (ONE << Open_Mp_Sections_Blk) |
                                (ONE << Open_Mp_Section_Blk) |
                                (ONE << Open_Mp_Single_Blk) |
                                (ONE << Open_Mp_Parallel_Do_Blk) |
                                (ONE << Open_Mp_Master_Blk) |
                                (ONE << Open_Mp_Critical_Blk) |
                                (ONE << Open_Mp_Ordered_Blk) |
                                (ONE << Open_Mp_Workshare_Blk) | /* by jhs, 02/7/18 */
                                (ONE << Open_Mp_Parallel_Workshare_Blk) | /* by jhs, 02/7/18 */
				(ONE << Contains_Blk) |
				(ONE << Interface_Blk) |
				(ONE << Derived_Type_Blk) | 
				(ONE << Enum_Blk)),

			/*****  Intrinsic_Stmt  *****/

			       ((ONE << Unknown_Blk) |
				(ONE << Forall_Blk) |
				(ONE << If_Blk) |
				(ONE << If_Then_Blk) |
				(ONE << If_Else_If_Blk) |
				(ONE << If_Else_Blk) |
				(ONE << Do_Blk) |
				(ONE << Select_Blk) |
				(ONE << Case_Blk) |
				(ONE << Where_Then_Blk) |
				(ONE << Where_Else_Blk) |
				(ONE << Where_Else_Mask_Blk) |
                                (ONE << Parallel_Blk) |
				(ONE << SGI_Parallel_Blk) |
                                (ONE << Doall_Blk) |
				(ONE << Wait_Blk) |
				(ONE << SGI_Doacross_Blk) |
				(ONE << SGI_Parallel_Do_Blk) |
                                (ONE << Do_Parallel_Blk) |
				(ONE << SGI_Pdo_Blk) |
                                (ONE << Guard_Blk) |
				(ONE << SGI_Critical_Section_Blk) |
                                (ONE << Parallel_Case_Blk) |
				(ONE << SGI_Psection_Blk) |
				(ONE << SGI_Section_Blk) |
				(ONE << SGI_Single_Process_Blk) |
                                (ONE << Open_Mp_Parallel_Blk) |
                                (ONE << Open_Mp_Do_Blk) |
                                (ONE << Open_Mp_Parallel_Sections_Blk) |
                                (ONE << Open_Mp_Sections_Blk) |
                                (ONE << Open_Mp_Section_Blk) |
                                (ONE << Open_Mp_Single_Blk) |
                                (ONE << Open_Mp_Parallel_Do_Blk) |
                                (ONE << Open_Mp_Master_Blk) |
                                (ONE << Open_Mp_Critical_Blk) |
                                (ONE << Open_Mp_Ordered_Blk) |
                                (ONE << Open_Mp_Workshare_Blk) | /* by jhs, 02/7/18 */
                                (ONE << Open_Mp_Parallel_Workshare_Blk) | /* by jhs, 02/7/18 */
				(ONE << Contains_Blk) |
				(ONE << Interface_Blk) |
				(ONE << Derived_Type_Blk) | 
				(ONE << Enum_Blk)),

			/*****  Module_Proc_Stmt  *****/

			       ((ONE << Unknown_Blk) |
				(ONE << Blockdata_Blk) |
				(ONE << Module_Blk) |
				(ONE << Program_Blk) |
				(ONE << Function_Blk) |
				(ONE << Subroutine_Blk) |
				(ONE << Internal_Blk) |
				(ONE << Module_Proc_Blk) |
				(ONE << Interface_Body_Blk) |
				(ONE << Forall_Blk) |
				(ONE << If_Blk) |
				(ONE << If_Then_Blk) |
				(ONE << If_Else_If_Blk) |
				(ONE << If_Else_Blk) |
				(ONE << Do_Blk) |
				(ONE << Select_Blk) |
				(ONE << Case_Blk) |
				(ONE << Where_Then_Blk) |
				(ONE << Where_Else_Blk) |
				(ONE << Where_Else_Mask_Blk) |
                                (ONE << Parallel_Blk) |
				(ONE << SGI_Parallel_Blk) |
                                (ONE << Doall_Blk) |
				(ONE << Wait_Blk) |
				(ONE << SGI_Doacross_Blk) |
				(ONE << SGI_Parallel_Do_Blk) |
                                (ONE << Do_Parallel_Blk) |
				(ONE << SGI_Pdo_Blk) |
                                (ONE << Guard_Blk) |
				(ONE << SGI_Critical_Section_Blk) |
                                (ONE << Parallel_Case_Blk) |
				(ONE << SGI_Psection_Blk) |
				(ONE << SGI_Section_Blk) |
				(ONE << SGI_Single_Process_Blk) |
                                (ONE << Open_Mp_Parallel_Blk) |
                                (ONE << Open_Mp_Do_Blk) |
                                (ONE << Open_Mp_Parallel_Sections_Blk) |
                                (ONE << Open_Mp_Sections_Blk) |
                                (ONE << Open_Mp_Section_Blk) |
                                (ONE << Open_Mp_Single_Blk) |
                                (ONE << Open_Mp_Parallel_Do_Blk) |
                                (ONE << Open_Mp_Master_Blk) |
                                (ONE << Open_Mp_Critical_Blk) |
                                (ONE << Open_Mp_Ordered_Blk) |
                                (ONE << Open_Mp_Workshare_Blk) | /* by jhs, 02/7/18 */
                                (ONE << Open_Mp_Parallel_Workshare_Blk) | /* by jhs, 02/7/18 */
				(ONE << Contains_Blk) |
				(ONE << Derived_Type_Blk) | 
				(ONE << Enum_Blk)),

			/*****  Namelist_Stmt  *****/

			       ((ONE << Unknown_Blk) |
				(ONE << Blockdata_Blk) |
				(ONE << Forall_Blk) |
				(ONE << If_Blk) |
				(ONE << Select_Blk) |
				(ONE << Where_Then_Blk) |
				(ONE << Where_Else_Blk) |
				(ONE << Where_Else_Mask_Blk) |
				(ONE << SGI_Psection_Blk) |
				(ONE << Contains_Blk) |
				(ONE << Interface_Blk) |
				(ONE << Derived_Type_Blk) | 
				(ONE << Enum_Blk)),

			/*****  Optional_Stmt  *****/

			       ((ONE << Unknown_Blk) |
				(ONE << Blockdata_Blk) |
				(ONE << Module_Blk) |
				(ONE << Program_Blk) |
				(ONE << Forall_Blk) |
				(ONE << If_Blk) |
				(ONE << If_Then_Blk) |
				(ONE << If_Else_If_Blk) |
				(ONE << If_Else_Blk) |
				(ONE << Do_Blk) |
				(ONE << Select_Blk) |
				(ONE << Case_Blk) |
				(ONE << Where_Then_Blk) |
				(ONE << Where_Else_Blk) |
				(ONE << Where_Else_Mask_Blk) |
                                (ONE << Parallel_Blk) |
				(ONE << SGI_Parallel_Blk) |
                                (ONE << Doall_Blk) |
				(ONE << Wait_Blk) |
				(ONE << SGI_Doacross_Blk) |
				(ONE << SGI_Parallel_Do_Blk) |
                                (ONE << Do_Parallel_Blk) |
				(ONE << SGI_Pdo_Blk) |
                                (ONE << Guard_Blk) |
				(ONE << SGI_Critical_Section_Blk) |
                                (ONE << Parallel_Case_Blk) |
				(ONE << SGI_Psection_Blk) |
				(ONE << SGI_Section_Blk) |
				(ONE << SGI_Single_Process_Blk) |
                                (ONE << Open_Mp_Parallel_Blk) |
                                (ONE << Open_Mp_Do_Blk) |
                                (ONE << Open_Mp_Parallel_Sections_Blk) |
                                (ONE << Open_Mp_Sections_Blk) |
                                (ONE << Open_Mp_Section_Blk) |
                                (ONE << Open_Mp_Single_Blk) |
                                (ONE << Open_Mp_Parallel_Do_Blk) |
                                (ONE << Open_Mp_Master_Blk) |
                                (ONE << Open_Mp_Critical_Blk) |
                                (ONE << Open_Mp_Ordered_Blk) |
                                (ONE << Open_Mp_Workshare_Blk) | /* by jhs, 02/7/18 */
                                (ONE << Open_Mp_Parallel_Workshare_Blk) | /* by jhs, 02/7/18 */
				(ONE << Contains_Blk) |
				(ONE << Interface_Blk) |
				(ONE << Derived_Type_Blk) | 
				(ONE << Enum_Blk)),

			/*****  Parameter_Stmt  *****/

			       ((ONE << Unknown_Blk) |
				(ONE << Forall_Blk) |
				(ONE << If_Blk) |
				(ONE << If_Then_Blk) |
				(ONE << If_Else_If_Blk) |
				(ONE << If_Else_Blk) |
				(ONE << Do_Blk) |
				(ONE << Select_Blk) |
				(ONE << Case_Blk) |
				(ONE << Where_Then_Blk) |
				(ONE << Where_Else_Blk) |
				(ONE << Where_Else_Mask_Blk) |
                                (ONE << Parallel_Blk) |
				(ONE << SGI_Parallel_Blk) |
                                (ONE << Doall_Blk) |
				(ONE << Wait_Blk) |
				(ONE << SGI_Doacross_Blk) |
				(ONE << SGI_Parallel_Do_Blk) |
                                (ONE << Do_Parallel_Blk) |
				(ONE << SGI_Pdo_Blk) |
                                (ONE << Guard_Blk) |
				(ONE << SGI_Critical_Section_Blk) |
                                (ONE << Parallel_Case_Blk) |
				(ONE << SGI_Psection_Blk) |
				(ONE << SGI_Section_Blk) |
				(ONE << SGI_Single_Process_Blk) |
                                (ONE << Open_Mp_Parallel_Blk) |
                                (ONE << Open_Mp_Do_Blk) |
                                (ONE << Open_Mp_Parallel_Sections_Blk) |
                                (ONE << Open_Mp_Sections_Blk) |
                                (ONE << Open_Mp_Section_Blk) |
                                (ONE << Open_Mp_Single_Blk) |
                                (ONE << Open_Mp_Parallel_Do_Blk) |
                                (ONE << Open_Mp_Master_Blk) |
                                (ONE << Open_Mp_Critical_Blk) |
                                (ONE << Open_Mp_Ordered_Blk) |
                                (ONE << Open_Mp_Workshare_Blk) | /* by jhs, 02/7/18 */
                                (ONE << Open_Mp_Parallel_Workshare_Blk) | /* by jhs, 02/7/18 */
				(ONE << Contains_Blk) |
				(ONE << Interface_Blk) |
				(ONE << Derived_Type_Blk) | 
				(ONE << Enum_Blk)),

			/*****  Pointer_Stmt  *****/

			       ((ONE << Unknown_Blk) |
				(ONE << Forall_Blk) |
				(ONE << If_Blk) |
				(ONE << If_Then_Blk) |
				(ONE << If_Else_If_Blk) |
				(ONE << If_Else_Blk) |
				(ONE << Do_Blk) |
				(ONE << Select_Blk) |
				(ONE << Case_Blk) |
				(ONE << Where_Then_Blk) |
				(ONE << Where_Else_Blk) |
				(ONE << Where_Else_Mask_Blk) |
                                (ONE << Parallel_Blk) |
				(ONE << SGI_Parallel_Blk) |
                                (ONE << Doall_Blk) |
				(ONE << Wait_Blk) |
				(ONE << SGI_Doacross_Blk) |
				(ONE << SGI_Parallel_Do_Blk) |
                                (ONE << Do_Parallel_Blk) |
				(ONE << SGI_Pdo_Blk) |
                                (ONE << Guard_Blk) |
				(ONE << SGI_Critical_Section_Blk) |
                                (ONE << Parallel_Case_Blk) |
				(ONE << SGI_Psection_Blk) |
				(ONE << SGI_Section_Blk) |
				(ONE << SGI_Single_Process_Blk) |
                                (ONE << Open_Mp_Parallel_Blk) |
                                (ONE << Open_Mp_Do_Blk) |
                                (ONE << Open_Mp_Parallel_Sections_Blk) |
                                (ONE << Open_Mp_Sections_Blk) |
                                (ONE << Open_Mp_Section_Blk) |
                                (ONE << Open_Mp_Single_Blk) |
                                (ONE << Open_Mp_Parallel_Do_Blk) |
                                (ONE << Open_Mp_Master_Blk) |
                                (ONE << Open_Mp_Critical_Blk) |
                                (ONE << Open_Mp_Ordered_Blk) |
                                (ONE << Open_Mp_Workshare_Blk) | /* by jhs, 02/7/18 */
                                (ONE << Open_Mp_Parallel_Workshare_Blk) | /* by jhs, 02/7/18 */
				(ONE << Contains_Blk) |
				(ONE << Interface_Blk) |
				(ONE << Derived_Type_Blk) | 
				(ONE << Enum_Blk)),

			/*****  Private_Stmt  *****/

			       ((ONE << Unknown_Blk) |
				(ONE << Blockdata_Blk) |
				(ONE << Program_Blk) |
				(ONE << Function_Blk) |
				(ONE << Subroutine_Blk) |
				(ONE << Internal_Blk) |
				(ONE << Module_Proc_Blk) |
				(ONE << Forall_Blk) |
				(ONE << If_Blk) |
				(ONE << If_Then_Blk) |
				(ONE << If_Else_If_Blk) |
				(ONE << If_Else_Blk) |
				(ONE << Do_Blk) |
				(ONE << Select_Blk) |
				(ONE << Case_Blk) |
				(ONE << Where_Then_Blk) |
				(ONE << Where_Else_Blk) |
				(ONE << Where_Else_Mask_Blk) |
                                (ONE << Parallel_Blk) |
				(ONE << SGI_Parallel_Blk) |
                                (ONE << Doall_Blk) |
				(ONE << Wait_Blk) |
				(ONE << SGI_Doacross_Blk) |
				(ONE << SGI_Parallel_Do_Blk) |
                                (ONE << Do_Parallel_Blk) |
				(ONE << SGI_Pdo_Blk) |
                                (ONE << Guard_Blk) |
				(ONE << SGI_Critical_Section_Blk) |
                                (ONE << Parallel_Case_Blk) |
				(ONE << SGI_Psection_Blk) |
				(ONE << SGI_Section_Blk) |
				(ONE << SGI_Single_Process_Blk) |
                                (ONE << Open_Mp_Parallel_Blk) |
                                (ONE << Open_Mp_Do_Blk) |
                                (ONE << Open_Mp_Parallel_Sections_Blk) |
                                (ONE << Open_Mp_Sections_Blk) |
                                (ONE << Open_Mp_Section_Blk) |
                                (ONE << Open_Mp_Single_Blk) |
                                (ONE << Open_Mp_Parallel_Do_Blk) |
                                (ONE << Open_Mp_Master_Blk) |
                                (ONE << Open_Mp_Critical_Blk) |
                                (ONE << Open_Mp_Ordered_Blk) |
                                (ONE << Open_Mp_Workshare_Blk) | /* by jhs, 02/7/18 */
                                (ONE << Open_Mp_Parallel_Workshare_Blk) | /* by jhs, 02/7/18 */
				(ONE << Contains_Blk) |
				(ONE << Interface_Blk) |    
				(ONE << Enum_Blk)),

			/*****  Public_Stmt  *****/

			       ((ONE << Unknown_Blk) |
				(ONE << Blockdata_Blk) |
				(ONE << Program_Blk) |
				(ONE << Function_Blk) |
				(ONE << Subroutine_Blk) |
				(ONE << Internal_Blk) |
				(ONE << Module_Proc_Blk) |
				(ONE << Forall_Blk) |
				(ONE << If_Blk) |
				(ONE << If_Then_Blk) |
				(ONE << If_Else_If_Blk) |
				(ONE << If_Else_Blk) |
				(ONE << Do_Blk) |
				(ONE << Select_Blk) |
				(ONE << Case_Blk) |
				(ONE << Where_Then_Blk) |
				(ONE << Where_Else_Blk) |
				(ONE << Where_Else_Mask_Blk) |
                                (ONE << Parallel_Blk) |
				(ONE << SGI_Parallel_Blk) |
                                (ONE << Doall_Blk) |
				(ONE << Wait_Blk) |
				(ONE << SGI_Doacross_Blk) |
				(ONE << SGI_Parallel_Do_Blk) |
                                (ONE << Do_Parallel_Blk) |
				(ONE << SGI_Pdo_Blk) |
                                (ONE << Guard_Blk) |
				(ONE << SGI_Critical_Section_Blk) |
                                (ONE << Parallel_Case_Blk) |
				(ONE << SGI_Psection_Blk) |
				(ONE << SGI_Section_Blk) |
				(ONE << SGI_Single_Process_Blk) |
                                (ONE << Open_Mp_Parallel_Blk) |
                                (ONE << Open_Mp_Do_Blk) |
                                (ONE << Open_Mp_Parallel_Sections_Blk) |
                                (ONE << Open_Mp_Sections_Blk) |
                                (ONE << Open_Mp_Section_Blk) |
                                (ONE << Open_Mp_Single_Blk) |
                                (ONE << Open_Mp_Parallel_Do_Blk) |
                                (ONE << Open_Mp_Master_Blk) |
                                (ONE << Open_Mp_Critical_Blk) |
                                (ONE << Open_Mp_Ordered_Blk) |
                                (ONE << Open_Mp_Workshare_Blk) | /* by jhs, 02/7/18 */
                                (ONE << Open_Mp_Parallel_Workshare_Blk) | /* by jhs, 02/7/18 */
				(ONE << Contains_Blk) |
				(ONE << Interface_Blk) |
				(ONE << Derived_Type_Blk) | 
				(ONE << Enum_Blk)),

			/*****  Save_Stmt  *****/

			       ((ONE << Unknown_Blk) |
				(ONE << Forall_Blk) |
				(ONE << If_Blk) |
				(ONE << If_Then_Blk) |
				(ONE << If_Else_If_Blk) |
				(ONE << If_Else_Blk) |
				(ONE << Do_Blk) |
				(ONE << Select_Blk) |
				(ONE << Case_Blk) |
				(ONE << Where_Then_Blk) |
				(ONE << Where_Else_Blk) |
				(ONE << Where_Else_Mask_Blk) |
                                (ONE << Parallel_Blk) |
				(ONE << SGI_Parallel_Blk) |
                                (ONE << Doall_Blk) |
				(ONE << Wait_Blk) |
				(ONE << SGI_Doacross_Blk) |
				(ONE << SGI_Parallel_Do_Blk) |
                                (ONE << Do_Parallel_Blk) |
				(ONE << SGI_Pdo_Blk) |
                                (ONE << Guard_Blk) |
				(ONE << SGI_Critical_Section_Blk) |
                                (ONE << Parallel_Case_Blk) |
				(ONE << SGI_Psection_Blk) |
				(ONE << SGI_Section_Blk) |
				(ONE << SGI_Single_Process_Blk) |
                                (ONE << Open_Mp_Parallel_Blk) |
                                (ONE << Open_Mp_Do_Blk) |
                                (ONE << Open_Mp_Parallel_Sections_Blk) |
                                (ONE << Open_Mp_Sections_Blk) |
                                (ONE << Open_Mp_Section_Blk) |
                                (ONE << Open_Mp_Single_Blk) |
                                (ONE << Open_Mp_Parallel_Do_Blk) |
                                (ONE << Open_Mp_Master_Blk) |
                                (ONE << Open_Mp_Critical_Blk) |
                                (ONE << Open_Mp_Ordered_Blk) |
                                (ONE << Open_Mp_Workshare_Blk) | /* by jhs, 02/7/18 */
                                (ONE << Open_Mp_Parallel_Workshare_Blk) | /* by jhs, 02/7/18 */
				(ONE << Contains_Blk) |
				(ONE << Interface_Blk) |
				(ONE << Derived_Type_Blk) | 
				(ONE << Enum_Blk)),

			/*****  Sequence_Stmt  *****/

			       ((ONE << Unknown_Blk) |
				(ONE << Blockdata_Blk) |
				(ONE << Module_Blk) |
				(ONE << Program_Blk) |
				(ONE << Function_Blk) |
				(ONE << Subroutine_Blk) |
				(ONE << Internal_Blk) |
				(ONE << Module_Proc_Blk) |
				(ONE << Interface_Body_Blk) |
				(ONE << Forall_Blk) |
				(ONE << If_Blk) |
				(ONE << If_Then_Blk) |
				(ONE << If_Else_If_Blk) |
				(ONE << If_Else_Blk) |
				(ONE << Do_Blk) |
				(ONE << Select_Blk) |
				(ONE << Case_Blk) |
				(ONE << Where_Then_Blk) |
				(ONE << Where_Else_Blk) |
				(ONE << Where_Else_Mask_Blk) |
                                (ONE << Parallel_Blk) |
				(ONE << SGI_Parallel_Blk) |
                                (ONE << Doall_Blk) |
				(ONE << Wait_Blk) |
				(ONE << SGI_Doacross_Blk) |
				(ONE << SGI_Parallel_Do_Blk) |
                                (ONE << Do_Parallel_Blk) |
				(ONE << SGI_Pdo_Blk) |
                                (ONE << Guard_Blk) |
				(ONE << SGI_Critical_Section_Blk) |
                                (ONE << Parallel_Case_Blk) |
				(ONE << SGI_Psection_Blk) |
				(ONE << SGI_Section_Blk) |
				(ONE << SGI_Single_Process_Blk) |
                                (ONE << Open_Mp_Parallel_Blk) |
                                (ONE << Open_Mp_Do_Blk) |
                                (ONE << Open_Mp_Parallel_Sections_Blk) |
                                (ONE << Open_Mp_Sections_Blk) |
                                (ONE << Open_Mp_Section_Blk) |
                                (ONE << Open_Mp_Single_Blk) |
                                (ONE << Open_Mp_Parallel_Do_Blk) |
                                (ONE << Open_Mp_Master_Blk) |
                                (ONE << Open_Mp_Critical_Blk) |
                                (ONE << Open_Mp_Ordered_Blk) |
                                (ONE << Open_Mp_Workshare_Blk) | /* by jhs, 02/7/18 */
                                (ONE << Open_Mp_Parallel_Workshare_Blk) | /* by jhs, 02/7/18 */
				(ONE << Contains_Blk) |
				(ONE << Interface_Blk) |    
				(ONE << Enum_Blk)),

			/*****  Stmt_Func_Stmt  *****/

			       ((ONE << Unknown_Blk) |
				(ONE << Blockdata_Blk) |
				(ONE << Module_Blk) |
				(ONE << Interface_Body_Blk) |
				(ONE << Forall_Blk) |
				(ONE << If_Blk) |
				(ONE << If_Then_Blk) |
				(ONE << If_Else_If_Blk) |
				(ONE << If_Else_Blk) |
				(ONE << Do_Blk) |
				(ONE << Select_Blk) |
				(ONE << Case_Blk) |
				(ONE << Where_Then_Blk) |
				(ONE << Where_Else_Blk) |
				(ONE << Where_Else_Mask_Blk) |
                                (ONE << Parallel_Blk) |
				(ONE << SGI_Parallel_Blk) |
                                (ONE << Doall_Blk) |
				(ONE << Wait_Blk) |
				(ONE << SGI_Doacross_Blk) |
				(ONE << SGI_Parallel_Do_Blk) |
                                (ONE << Do_Parallel_Blk) |
				(ONE << SGI_Pdo_Blk) |
                                (ONE << Guard_Blk) |
				(ONE << SGI_Critical_Section_Blk) |
                                (ONE << Parallel_Case_Blk) |
				(ONE << SGI_Psection_Blk) |
				(ONE << SGI_Section_Blk) |
				(ONE << SGI_Single_Process_Blk) |
                                (ONE << Open_Mp_Parallel_Blk) |
                                (ONE << Open_Mp_Do_Blk) |
                                (ONE << Open_Mp_Parallel_Sections_Blk) |
                                (ONE << Open_Mp_Sections_Blk) |
                                (ONE << Open_Mp_Section_Blk) |
                                (ONE << Open_Mp_Single_Blk) |
                                (ONE << Open_Mp_Parallel_Do_Blk) |
                                (ONE << Open_Mp_Master_Blk) |
                                (ONE << Open_Mp_Critical_Blk) |
                                (ONE << Open_Mp_Ordered_Blk) |
                                (ONE << Open_Mp_Workshare_Blk) | /* by jhs, 02/7/18 */
                                (ONE << Open_Mp_Parallel_Workshare_Blk) | /* by jhs, 02/7/18 */
				(ONE << Contains_Blk) |
				(ONE << Interface_Blk) |
				(ONE << Derived_Type_Blk) | 
				(ONE << Enum_Blk)),

			/*****  Target_Stmt  *****/

			       ((ONE << Unknown_Blk) |
				(ONE << Forall_Blk) |
				(ONE << If_Blk) |
				(ONE << If_Then_Blk) |
				(ONE << If_Else_If_Blk) |
				(ONE << If_Else_Blk) |
				(ONE << Do_Blk) |
				(ONE << Select_Blk) |
				(ONE << Case_Blk) |
				(ONE << Where_Then_Blk) |
				(ONE << Where_Else_Blk) |
				(ONE << Where_Else_Mask_Blk) |
                                (ONE << Parallel_Blk) |
				(ONE << SGI_Parallel_Blk) |
                                (ONE << Doall_Blk) |
				(ONE << Wait_Blk) |
				(ONE << SGI_Doacross_Blk) |
				(ONE << SGI_Parallel_Do_Blk) |
                                (ONE << Do_Parallel_Blk) |
				(ONE << SGI_Pdo_Blk) |
                                (ONE << Guard_Blk) |
				(ONE << SGI_Critical_Section_Blk) |
                                (ONE << Parallel_Case_Blk) |
				(ONE << SGI_Psection_Blk) |
				(ONE << SGI_Section_Blk) |
				(ONE << SGI_Single_Process_Blk) |
                                (ONE << Open_Mp_Parallel_Blk) |
                                (ONE << Open_Mp_Do_Blk) |
                                (ONE << Open_Mp_Parallel_Sections_Blk) |
                                (ONE << Open_Mp_Sections_Blk) |
                                (ONE << Open_Mp_Section_Blk) |
                                (ONE << Open_Mp_Single_Blk) |
                                (ONE << Open_Mp_Parallel_Do_Blk) |
                                (ONE << Open_Mp_Master_Blk) |
                                (ONE << Open_Mp_Critical_Blk) |
                                (ONE << Open_Mp_Ordered_Blk) |
                                (ONE << Open_Mp_Workshare_Blk) | /* by jhs, 02/7/18 */
                                (ONE << Open_Mp_Parallel_Workshare_Blk) | /* by jhs, 02/7/18 */
				(ONE << Contains_Blk) |
				(ONE << Interface_Blk) |
				(ONE << Derived_Type_Blk) | 
				(ONE << Enum_Blk)),

			/*****  Task_Common_Stmt  *****/

			       ((ONE << Unknown_Blk) |
				(ONE << Module_Blk) |
                                (ONE << Interface_Body_Blk) |
                                (ONE << Internal_Blk) |
                                (ONE << Module_Proc_Blk) |
				(ONE << Forall_Blk) |
				(ONE << If_Blk) |
				(ONE << If_Then_Blk) |
				(ONE << If_Else_If_Blk) |
				(ONE << If_Else_Blk) |
                                (ONE << Do_Blk) |
                                (ONE << Select_Blk) |
				(ONE << Case_Blk) |
                                (ONE << Where_Then_Blk) |
				(ONE << Where_Else_Blk) |
				(ONE << Where_Else_Mask_Blk) |
                                (ONE << Parallel_Blk) |
				(ONE << SGI_Parallel_Blk) |
                                (ONE << Doall_Blk) |
				(ONE << Wait_Blk) |
				(ONE << SGI_Doacross_Blk) |
				(ONE << SGI_Parallel_Do_Blk) |
                                (ONE << Do_Parallel_Blk) |
				(ONE << SGI_Pdo_Blk) |
                                (ONE << Guard_Blk) |
				(ONE << SGI_Critical_Section_Blk) |
                                (ONE << Parallel_Case_Blk) |
				(ONE << SGI_Psection_Blk) |
				(ONE << SGI_Section_Blk) |
				(ONE << SGI_Single_Process_Blk) |
                                (ONE << Open_Mp_Parallel_Blk) |
                                (ONE << Open_Mp_Do_Blk) |
                                (ONE << Open_Mp_Parallel_Sections_Blk) |
                                (ONE << Open_Mp_Sections_Blk) |
                                (ONE << Open_Mp_Section_Blk) |
                                (ONE << Open_Mp_Single_Blk) |
                                (ONE << Open_Mp_Parallel_Do_Blk) |
                                (ONE << Open_Mp_Master_Blk) |
                                (ONE << Open_Mp_Critical_Blk) |
                                (ONE << Open_Mp_Ordered_Blk) |
                                (ONE << Open_Mp_Workshare_Blk) | /* by jhs, 02/7/18 */
                                (ONE << Open_Mp_Parallel_Workshare_Blk) | /* by jhs, 02/7/18 */
                                (ONE << Contains_Blk) |
                                (ONE << Interface_Blk) |
                                (ONE << Derived_Type_Blk) | 
				(ONE << Enum_Blk)),

			/*****  Type_Decl_Stmt  *****/

			       ((ONE << Unknown_Blk) |
				(ONE << Forall_Blk) |
				(ONE << If_Blk) |
				(ONE << If_Then_Blk) |
				(ONE << If_Else_If_Blk) |
				(ONE << If_Else_Blk) |
				(ONE << Do_Blk) |
				(ONE << Select_Blk) |
				(ONE << Case_Blk) |
				(ONE << Where_Then_Blk) |
				(ONE << Where_Else_Blk) |
				(ONE << Where_Else_Mask_Blk) |
                                (ONE << Parallel_Blk) |
				(ONE << SGI_Parallel_Blk) |
                                (ONE << Doall_Blk) |
				(ONE << SGI_Doacross_Blk) |
				(ONE << SGI_Parallel_Do_Blk) |
                                (ONE << Do_Parallel_Blk) |
				(ONE << Wait_Blk) |
				(ONE << SGI_Pdo_Blk) |
                                (ONE << Guard_Blk) |
				(ONE << SGI_Critical_Section_Blk) |
                                (ONE << Parallel_Case_Blk) |
				(ONE << SGI_Psection_Blk) |
				(ONE << SGI_Section_Blk) |
				(ONE << SGI_Single_Process_Blk) |
                                (ONE << Open_Mp_Parallel_Blk) |
                                (ONE << Open_Mp_Do_Blk) |
                                (ONE << Open_Mp_Parallel_Sections_Blk) |
                                (ONE << Open_Mp_Sections_Blk) |
                                (ONE << Open_Mp_Section_Blk) |
                                (ONE << Open_Mp_Single_Blk) |
                                (ONE << Open_Mp_Parallel_Do_Blk) |
                                (ONE << Open_Mp_Master_Blk) |
                                (ONE << Open_Mp_Critical_Blk) |
                                (ONE << Open_Mp_Ordered_Blk) |
                                (ONE << Open_Mp_Workshare_Blk) | /* by jhs, 02/7/18 */
                                (ONE << Open_Mp_Parallel_Workshare_Blk) | /* by jhs, 02/7/18 */
				(ONE << Contains_Blk) |
				(ONE << Interface_Blk) |    
				(ONE << Enum_Blk)),

			/*****  Use_Stmt  *****/

			       ((ONE << Unknown_Blk) |
				(ONE << Forall_Blk) |
				(ONE << If_Blk) |
				(ONE << If_Then_Blk) |
				(ONE << If_Else_If_Blk) |
				(ONE << If_Else_Blk) |
				(ONE << Do_Blk) |
				(ONE << Select_Blk) |
				(ONE << Case_Blk) |
				(ONE << Where_Then_Blk) |
				(ONE << Where_Else_Blk) |
				(ONE << Where_Else_Mask_Blk) |
                                (ONE << Parallel_Blk) |
				(ONE << SGI_Parallel_Blk) |
                                (ONE << Doall_Blk) |
				(ONE << SGI_Doacross_Blk) |
				(ONE << SGI_Parallel_Do_Blk) |
                                (ONE << Do_Parallel_Blk) |
				(ONE << Wait_Blk) |
				(ONE << SGI_Pdo_Blk) |
                                (ONE << Guard_Blk) |
				(ONE << SGI_Critical_Section_Blk) |
                                (ONE << Parallel_Case_Blk) |
				(ONE << SGI_Psection_Blk) |
				(ONE << SGI_Section_Blk) |
				(ONE << SGI_Single_Process_Blk) |
                                (ONE << Open_Mp_Parallel_Blk) |
                                (ONE << Open_Mp_Do_Blk) |
                                (ONE << Open_Mp_Parallel_Sections_Blk) |
                                (ONE << Open_Mp_Sections_Blk) |
                                (ONE << Open_Mp_Section_Blk) |
                                (ONE << Open_Mp_Single_Blk) |
                                (ONE << Open_Mp_Parallel_Do_Blk) |
                                (ONE << Open_Mp_Master_Blk) |
                                (ONE << Open_Mp_Critical_Blk) |
                                (ONE << Open_Mp_Ordered_Blk) |
                                (ONE << Open_Mp_Workshare_Blk) | /* by jhs, 02/7/18 */
                                (ONE << Open_Mp_Parallel_Workshare_Blk) | /* by jhs, 02/7/18 */
				(ONE << Contains_Blk) |
				(ONE << Interface_Blk) |
				(ONE << Derived_Type_Blk) | 
				(ONE << Enum_Blk)),


			/*****  Blockdata_Stmt  *****/

			       ((ONE << Unknown_Blk) |
				(ONE << Blockdata_Blk) |
				(ONE << Module_Blk) |
				(ONE << Function_Blk) |
				(ONE << Subroutine_Blk) |
				(ONE << Internal_Blk) |
				(ONE << Module_Proc_Blk) |
				(ONE << Interface_Body_Blk) |
				(ONE << Forall_Blk) |
				(ONE << If_Blk) |
				(ONE << If_Then_Blk) |
				(ONE << If_Else_If_Blk) |
				(ONE << If_Else_Blk) |
				(ONE << Do_Blk) |
				(ONE << Select_Blk) |
				(ONE << Case_Blk) |
				(ONE << Where_Then_Blk) |
				(ONE << Where_Else_Blk) |
				(ONE << Where_Else_Mask_Blk) |
                                (ONE << Parallel_Blk) |
				(ONE << SGI_Parallel_Blk) |
                                (ONE << Doall_Blk) |
				(ONE << Wait_Blk) |
				(ONE << SGI_Doacross_Blk) |
				(ONE << SGI_Parallel_Do_Blk) |
                                (ONE << Do_Parallel_Blk) |
				(ONE << SGI_Pdo_Blk) |
                                (ONE << Guard_Blk) |
				(ONE << SGI_Critical_Section_Blk) |
                                (ONE << Parallel_Case_Blk) |
				(ONE << SGI_Psection_Blk) |
				(ONE << SGI_Section_Blk) |
				(ONE << SGI_Single_Process_Blk) |
                                (ONE << Open_Mp_Parallel_Blk) |
                                (ONE << Open_Mp_Do_Blk) |
                                (ONE << Open_Mp_Parallel_Sections_Blk) |
                                (ONE << Open_Mp_Sections_Blk) |
                                (ONE << Open_Mp_Section_Blk) |
                                (ONE << Open_Mp_Single_Blk) |
                                (ONE << Open_Mp_Parallel_Do_Blk) |
                                (ONE << Open_Mp_Master_Blk) |
                                (ONE << Open_Mp_Critical_Blk) |
                                (ONE << Open_Mp_Ordered_Blk) |
                                (ONE << Open_Mp_Workshare_Blk) | /* by jhs, 02/7/18 */
                                (ONE << Open_Mp_Parallel_Workshare_Blk) | /* by jhs, 02/7/18 */
				(ONE << Contains_Blk) |
				(ONE << Interface_Blk) |
				(ONE << Derived_Type_Blk) | 
				(ONE << Enum_Blk)),

			/*****  Elemental_Stmt  *****/

			       ((ONE << Unknown_Blk) |
				(ONE << Blockdata_Blk) |
				(ONE << Module_Blk) |
				(ONE << Function_Blk) |
				(ONE << Subroutine_Blk) |
				(ONE << Internal_Blk) |
				(ONE << Module_Proc_Blk) |
				(ONE << Interface_Body_Blk) |
				(ONE << Forall_Blk) |
				(ONE << If_Blk) |
				(ONE << If_Then_Blk) |
				(ONE << If_Else_If_Blk) |
				(ONE << If_Else_Blk) |
				(ONE << Do_Blk) |
				(ONE << Select_Blk) |
				(ONE << Case_Blk) |
				(ONE << Where_Then_Blk) |
				(ONE << Where_Else_Blk) |
				(ONE << Where_Else_Mask_Blk) |
                                (ONE << Parallel_Blk) |
				(ONE << SGI_Parallel_Blk) |
                                (ONE << Doall_Blk) |
				(ONE << Wait_Blk) |
				(ONE << SGI_Doacross_Blk) |
				(ONE << SGI_Parallel_Do_Blk) |
                                (ONE << Do_Parallel_Blk) |
				(ONE << SGI_Pdo_Blk) |
                                (ONE << Guard_Blk) |
				(ONE << SGI_Critical_Section_Blk) |
                                (ONE << Parallel_Case_Blk) |
				(ONE << SGI_Psection_Blk) |
				(ONE << SGI_Section_Blk) |
				(ONE << SGI_Single_Process_Blk) |
				(ONE << Derived_Type_Blk) | 
				(ONE << Enum_Blk)),

			/*****  Function_Stmt  *****/

			       ((ONE << Unknown_Blk) |
				(ONE << Blockdata_Blk) |
				(ONE << Module_Blk) |
				(ONE << Program_Blk) |
				(ONE << Function_Blk) |
				(ONE << Subroutine_Blk) |
				(ONE << Internal_Blk) |
				(ONE << Module_Proc_Blk) |
				(ONE << Interface_Body_Blk) |
				(ONE << Forall_Blk) |
				(ONE << If_Blk) |
				(ONE << If_Then_Blk) |
				(ONE << If_Else_If_Blk) |
				(ONE << If_Else_Blk) |
				(ONE << Do_Blk) |
				(ONE << Select_Blk) |
				(ONE << Case_Blk) |
				(ONE << Where_Then_Blk) |
				(ONE << Where_Else_Blk) |
				(ONE << Where_Else_Mask_Blk) |
                                (ONE << Parallel_Blk) |
				(ONE << SGI_Parallel_Blk) |
                                (ONE << Doall_Blk) |
				(ONE << Wait_Blk) |
				(ONE << SGI_Doacross_Blk) |
				(ONE << SGI_Parallel_Do_Blk) |
                                (ONE << Do_Parallel_Blk) |
				(ONE << SGI_Pdo_Blk) |
                                (ONE << Guard_Blk) |
				(ONE << SGI_Critical_Section_Blk) |
                                (ONE << Parallel_Case_Blk) |
				(ONE << SGI_Psection_Blk) |
				(ONE << SGI_Section_Blk) |
				(ONE << SGI_Single_Process_Blk) |
				(ONE << Derived_Type_Blk) | 
				(ONE << Enum_Blk)),

			/*****  Module_Stmt  *****/

			       ((ONE << Unknown_Blk) |
				(ONE << Blockdata_Blk) |
				(ONE << Module_Blk) |
				(ONE << Function_Blk) |
				(ONE << Subroutine_Blk) |
				(ONE << Internal_Blk) |
				(ONE << Module_Proc_Blk) |
				(ONE << Interface_Body_Blk) |
				(ONE << Forall_Blk) |
				(ONE << If_Blk) |
				(ONE << If_Then_Blk) |
				(ONE << If_Else_If_Blk) |
				(ONE << If_Else_Blk) |
				(ONE << Do_Blk) |
				(ONE << Select_Blk) |
				(ONE << Case_Blk) |
				(ONE << Where_Then_Blk) |
				(ONE << Where_Else_Blk) |
				(ONE << Where_Else_Mask_Blk) |
                                (ONE << Parallel_Blk) |
				(ONE << SGI_Parallel_Blk) |
                                (ONE << Doall_Blk) |
				(ONE << Wait_Blk) |
				(ONE << SGI_Doacross_Blk) |
				(ONE << SGI_Parallel_Do_Blk) |
                                (ONE << Do_Parallel_Blk) |
				(ONE << SGI_Pdo_Blk) |
                                (ONE << Guard_Blk) |
				(ONE << SGI_Critical_Section_Blk) |
                                (ONE << Parallel_Case_Blk) |
				(ONE << SGI_Psection_Blk) |
				(ONE << SGI_Section_Blk) |
				(ONE << SGI_Single_Process_Blk) |
                                (ONE << Open_Mp_Parallel_Blk) |
                                (ONE << Open_Mp_Do_Blk) |
                                (ONE << Open_Mp_Parallel_Sections_Blk) |
                                (ONE << Open_Mp_Sections_Blk) |
                                (ONE << Open_Mp_Section_Blk) |
                                (ONE << Open_Mp_Single_Blk) |
                                (ONE << Open_Mp_Parallel_Do_Blk) |
                                (ONE << Open_Mp_Master_Blk) |
                                (ONE << Open_Mp_Critical_Blk) |
                                (ONE << Open_Mp_Ordered_Blk) |
                                (ONE << Open_Mp_Workshare_Blk) | /* by jhs, 02/7/18 */
                                (ONE << Open_Mp_Parallel_Workshare_Blk) | /* by jhs, 02/7/18 */
				(ONE << Contains_Blk) |
				(ONE << Interface_Blk) |
				(ONE << Derived_Type_Blk) | 
				(ONE << Enum_Blk)),

			/*****  Program_Stmt  *****/

			       ((ONE << Unknown_Blk) |
				(ONE << Blockdata_Blk) |
				(ONE << Module_Blk) |
				(ONE << Function_Blk) |
				(ONE << Subroutine_Blk) |
				(ONE << Internal_Blk) |
				(ONE << Module_Proc_Blk) |
				(ONE << Interface_Body_Blk) |
				(ONE << Forall_Blk) |
				(ONE << If_Blk) |
				(ONE << If_Then_Blk) |
				(ONE << If_Else_If_Blk) |
				(ONE << If_Else_Blk) |
				(ONE << Do_Blk) |
				(ONE << Select_Blk) |
				(ONE << Case_Blk) |
				(ONE << Where_Then_Blk) |
				(ONE << Where_Else_Blk) |
				(ONE << Where_Else_Mask_Blk) |
                                (ONE << Parallel_Blk) |
				(ONE << SGI_Parallel_Blk) |
                                (ONE << Doall_Blk) |
				(ONE << Wait_Blk) |
				(ONE << SGI_Doacross_Blk) |
				(ONE << SGI_Parallel_Do_Blk) |
                                (ONE << Do_Parallel_Blk) |
				(ONE << SGI_Pdo_Blk) |
                                (ONE << Guard_Blk) |
				(ONE << SGI_Critical_Section_Blk) |
                                (ONE << Parallel_Case_Blk) |
				(ONE << SGI_Psection_Blk) |
				(ONE << SGI_Section_Blk) |
				(ONE << SGI_Single_Process_Blk) |
                                (ONE << Open_Mp_Parallel_Blk) |
                                (ONE << Open_Mp_Do_Blk) |
                                (ONE << Open_Mp_Parallel_Sections_Blk) |
                                (ONE << Open_Mp_Sections_Blk) |
                                (ONE << Open_Mp_Section_Blk) |
                                (ONE << Open_Mp_Single_Blk) |
                                (ONE << Open_Mp_Parallel_Do_Blk) |
                                (ONE << Open_Mp_Master_Blk) |
                                (ONE << Open_Mp_Critical_Blk) |
                                (ONE << Open_Mp_Ordered_Blk) |
                                (ONE << Open_Mp_Workshare_Blk) | /* by jhs, 02/7/18 */
                                (ONE << Open_Mp_Parallel_Workshare_Blk) | /* by jhs, 02/7/18 */
				(ONE << Contains_Blk) |
				(ONE << Interface_Blk) |
				(ONE << Derived_Type_Blk) | 
				(ONE << Enum_Blk)),

			/*****  Pure_Stmt  *****/

			       ((ONE << Unknown_Blk) |
				(ONE << Blockdata_Blk) |
				(ONE << Module_Blk) |
				(ONE << Function_Blk) |
				(ONE << Subroutine_Blk) |
				(ONE << Internal_Blk) |
				(ONE << Module_Proc_Blk) |
				(ONE << Interface_Body_Blk) |
				(ONE << Forall_Blk) |
				(ONE << If_Blk) |
				(ONE << If_Then_Blk) |
				(ONE << If_Else_If_Blk) |
				(ONE << If_Else_Blk) |
				(ONE << Do_Blk) |
				(ONE << Select_Blk) |
				(ONE << Case_Blk) |
				(ONE << Where_Then_Blk) |
				(ONE << Where_Else_Blk) |
				(ONE << Where_Else_Mask_Blk) |
                                (ONE << Parallel_Blk) |
				(ONE << SGI_Parallel_Blk) |
                                (ONE << Doall_Blk) |
				(ONE << Wait_Blk) |
				(ONE << SGI_Doacross_Blk) |
				(ONE << SGI_Parallel_Do_Blk) |
                                (ONE << Do_Parallel_Blk) |
				(ONE << SGI_Pdo_Blk) |
                                (ONE << Guard_Blk) |
				(ONE << SGI_Critical_Section_Blk) |
                                (ONE << Parallel_Case_Blk) |
				(ONE << SGI_Psection_Blk) |
				(ONE << SGI_Section_Blk) |
				(ONE << SGI_Single_Process_Blk) |
				(ONE << Derived_Type_Blk) | 
				(ONE << Enum_Blk)),

			/*****  Recursive_Stmt  *****/

			       ((ONE << Unknown_Blk) |
				(ONE << Blockdata_Blk) |
				(ONE << Module_Blk) |
				(ONE << Function_Blk) |
				(ONE << Subroutine_Blk) |
				(ONE << Internal_Blk) |
				(ONE << Module_Proc_Blk) |
				(ONE << Interface_Body_Blk) |
				(ONE << Forall_Blk) |
				(ONE << If_Blk) |
				(ONE << If_Then_Blk) |
				(ONE << If_Else_If_Blk) |
				(ONE << If_Else_Blk) |
				(ONE << Do_Blk) |
				(ONE << Select_Blk) |
				(ONE << Case_Blk) |
				(ONE << Where_Then_Blk) |
				(ONE << Where_Else_Blk) |
				(ONE << Where_Else_Mask_Blk) |
                                (ONE << Parallel_Blk) |
				(ONE << SGI_Parallel_Blk) |
                                (ONE << Doall_Blk) |
				(ONE << Wait_Blk) |
				(ONE << SGI_Doacross_Blk) |
				(ONE << SGI_Parallel_Do_Blk) |
                                (ONE << Do_Parallel_Blk) |
				(ONE << SGI_Pdo_Blk) |
                                (ONE << Guard_Blk) |
				(ONE << SGI_Critical_Section_Blk) |
                                (ONE << Parallel_Case_Blk) |
				(ONE << SGI_Psection_Blk) |
				(ONE << SGI_Section_Blk) |
				(ONE << SGI_Single_Process_Blk) |
				(ONE << Derived_Type_Blk) | 
				(ONE << Enum_Blk)),

			/*****  Subroutine_Stmt  *****/

			       ((ONE << Unknown_Blk) |
				(ONE << Blockdata_Blk) |
				(ONE << Module_Blk) |
				(ONE << Function_Blk) |
				(ONE << Subroutine_Blk) |
				(ONE << Internal_Blk) |
				(ONE << Module_Proc_Blk) |
				(ONE << Interface_Body_Blk) |
				(ONE << Forall_Blk) |
				(ONE << If_Blk) |
				(ONE << If_Then_Blk) |
				(ONE << If_Else_If_Blk) |
				(ONE << If_Else_Blk) |
				(ONE << Do_Blk) |
				(ONE << Select_Blk) |
				(ONE << Case_Blk) |
				(ONE << Where_Then_Blk) |
				(ONE << Where_Else_Blk) |
				(ONE << Where_Else_Mask_Blk) |
                                (ONE << Parallel_Blk) |
				(ONE << SGI_Parallel_Blk) |
                                (ONE << Doall_Blk) |
				(ONE << Wait_Blk) |
				(ONE << SGI_Doacross_Blk) |
				(ONE << SGI_Parallel_Do_Blk) |
                                (ONE << Do_Parallel_Blk) |
				(ONE << SGI_Pdo_Blk) |
                                (ONE << Guard_Blk) |
				(ONE << SGI_Critical_Section_Blk) |
                                (ONE << Parallel_Case_Blk) |
				(ONE << SGI_Psection_Blk) |
				(ONE << SGI_Section_Blk) |
				(ONE << SGI_Single_Process_Blk) |
				(ONE << Derived_Type_Blk) | 
				(ONE << Enum_Blk)),

			/*****  End_Blockdata_Stmt  *****/

			       ((ONE << Unknown_Blk) |
				(ONE << Module_Blk) |
				(ONE << Program_Blk) |
				(ONE << Function_Blk) |
				(ONE << Subroutine_Blk) |
				(ONE << Internal_Blk) |
				(ONE << Module_Proc_Blk) |
				(ONE << Interface_Body_Blk) |
				(ONE << Forall_Blk) |
				(ONE << If_Blk) |
				(ONE << If_Then_Blk) |
				(ONE << If_Else_If_Blk) |
				(ONE << If_Else_Blk) |
				(ONE << Do_Blk) |
				(ONE << Select_Blk) |
				(ONE << Case_Blk) |
				(ONE << Where_Then_Blk) |
				(ONE << Where_Else_Blk) |
				(ONE << Where_Else_Mask_Blk) |
                                (ONE << Parallel_Blk) |
				(ONE << SGI_Parallel_Blk) |
                                (ONE << Doall_Blk) |
				(ONE << Wait_Blk) |
				(ONE << SGI_Doacross_Blk) |
				(ONE << SGI_Parallel_Do_Blk) |
                                (ONE << Do_Parallel_Blk) |
				(ONE << SGI_Pdo_Blk) |
                                (ONE << Guard_Blk) |
				(ONE << SGI_Critical_Section_Blk) |
                                (ONE << Parallel_Case_Blk) |
				(ONE << SGI_Psection_Blk) |
				(ONE << SGI_Section_Blk) |
				(ONE << SGI_Single_Process_Blk) |
				(ONE << SGI_Region_Blk) |
                                (ONE << Open_Mp_Parallel_Blk) |
                                (ONE << Open_Mp_Do_Blk) |
                                (ONE << Open_Mp_Parallel_Sections_Blk) |
                                (ONE << Open_Mp_Sections_Blk) |
                                (ONE << Open_Mp_Section_Blk) |
                                (ONE << Open_Mp_Single_Blk) |
                                (ONE << Open_Mp_Parallel_Do_Blk) |
                                (ONE << Open_Mp_Master_Blk) |
                                (ONE << Open_Mp_Critical_Blk) |
                                (ONE << Open_Mp_Ordered_Blk) |
                                (ONE << Open_Mp_Workshare_Blk) | /* by jhs, 02/7/18 */
                                (ONE << Open_Mp_Parallel_Workshare_Blk) | /* by jhs, 02/7/18 */
				(ONE << Contains_Blk) |
				(ONE << Interface_Blk) |
				(ONE << Derived_Type_Blk) | 
				(ONE << Enum_Blk)),

			/*****  End_Do_Stmt  *****/

			       ((ONE << Unknown_Blk) |
				(ONE << Blockdata_Blk) |
				(ONE << Module_Blk) |
				(ONE << Program_Blk) |
				(ONE << Function_Blk) |
				(ONE << Subroutine_Blk) |
				(ONE << Internal_Blk) |
				(ONE << Module_Proc_Blk) |
				(ONE << Interface_Body_Blk) |
				(ONE << Forall_Blk) |
				(ONE << If_Blk) |
				(ONE << If_Then_Blk) |
				(ONE << If_Else_If_Blk) |
				(ONE << If_Else_Blk) |
				(ONE << Select_Blk) |
				(ONE << Case_Blk) |
				(ONE << Where_Then_Blk) |
				(ONE << Where_Else_Blk) |
				(ONE << Where_Else_Mask_Blk) |
                                (ONE << Parallel_Blk) |
				(ONE << SGI_Parallel_Blk) |
                                (ONE << Doall_Blk) |
				(ONE << Wait_Blk) |
				(ONE << SGI_Doacross_Blk) |
				(ONE << SGI_Parallel_Do_Blk) |
                                (ONE << Do_Parallel_Blk) |
				(ONE << SGI_Pdo_Blk) |
                                (ONE << Guard_Blk) |
				(ONE << SGI_Critical_Section_Blk) |
                                (ONE << Parallel_Case_Blk) |
				(ONE << SGI_Psection_Blk) |
				(ONE << SGI_Section_Blk) |
				(ONE << SGI_Single_Process_Blk) |
				(ONE << SGI_Region_Blk) |
                                (ONE << Open_Mp_Parallel_Blk) |
                                (ONE << Open_Mp_Do_Blk) |
                                (ONE << Open_Mp_Parallel_Sections_Blk) |
                                (ONE << Open_Mp_Sections_Blk) |
                                (ONE << Open_Mp_Section_Blk) |
                                (ONE << Open_Mp_Single_Blk) |
                                (ONE << Open_Mp_Parallel_Do_Blk) |
                                (ONE << Open_Mp_Master_Blk) |
                                (ONE << Open_Mp_Critical_Blk) |
                                (ONE << Open_Mp_Ordered_Blk) |
                                (ONE << Open_Mp_Workshare_Blk) | /* by jhs, 02/7/18 */
                                (ONE << Open_Mp_Parallel_Workshare_Blk) | /* by jhs, 02/7/18 */
				(ONE << Contains_Blk) |
				(ONE << Interface_Blk) |
				(ONE << Derived_Type_Blk) | 
				(ONE << Enum_Blk)),

			/*****  End_Function_Stmt  *****/

			       ((ONE << Unknown_Blk) |
				(ONE << Blockdata_Blk) |
				(ONE << Module_Blk) |
				(ONE << Program_Blk) |
				(ONE << Subroutine_Blk) |
				(ONE << Forall_Blk) |
				(ONE << If_Blk) |
				(ONE << If_Then_Blk) |
				(ONE << If_Else_If_Blk) |
				(ONE << If_Else_Blk) |
				(ONE << Do_Blk) |
				(ONE << Select_Blk) |
				(ONE << Case_Blk) |
				(ONE << Where_Then_Blk) |
				(ONE << Where_Else_Blk) |
				(ONE << Where_Else_Mask_Blk) |
                                (ONE << Parallel_Blk) |
				(ONE << SGI_Parallel_Blk) |
                                (ONE << Doall_Blk) |
				(ONE << Wait_Blk) |
				(ONE << SGI_Doacross_Blk) |
				(ONE << SGI_Parallel_Do_Blk) |
                                (ONE << Do_Parallel_Blk) |
				(ONE << SGI_Pdo_Blk) |
                                (ONE << Guard_Blk) |
				(ONE << SGI_Critical_Section_Blk) |
                                (ONE << Parallel_Case_Blk) |
				(ONE << SGI_Psection_Blk) |
				(ONE << SGI_Section_Blk) |
				(ONE << SGI_Single_Process_Blk) |
				(ONE << SGI_Region_Blk) |
                                (ONE << Open_Mp_Parallel_Blk) |
                                (ONE << Open_Mp_Do_Blk) |
                                (ONE << Open_Mp_Parallel_Sections_Blk) |
                                (ONE << Open_Mp_Sections_Blk) |
                                (ONE << Open_Mp_Section_Blk) |
                                (ONE << Open_Mp_Single_Blk) |
                                (ONE << Open_Mp_Parallel_Do_Blk) |
                                (ONE << Open_Mp_Master_Blk) |
                                (ONE << Open_Mp_Critical_Blk) |
                                (ONE << Open_Mp_Ordered_Blk) |
                                (ONE << Open_Mp_Workshare_Blk) | /* by jhs, 02/7/18 */
                                (ONE << Open_Mp_Parallel_Workshare_Blk) | /* by jhs, 02/7/18 */
				(ONE << Contains_Blk) |
				(ONE << Interface_Blk) |
				(ONE << Derived_Type_Blk) | 
				(ONE << Enum_Blk)),

			/*****  End_If_Stmt  *****/

			       ((ONE << Unknown_Blk) |
				(ONE << Blockdata_Blk) |
				(ONE << Module_Blk) |
				(ONE << Program_Blk) |
				(ONE << Function_Blk) |
				(ONE << Subroutine_Blk) |
				(ONE << Internal_Blk) |
				(ONE << Module_Proc_Blk) |
				(ONE << Interface_Body_Blk) |
				(ONE << Forall_Blk) |
				(ONE << If_Blk) |
				(ONE << Do_Blk) |
				(ONE << Select_Blk) |
				(ONE << Case_Blk) |
				(ONE << Where_Then_Blk) |
				(ONE << Where_Else_Blk) |
				(ONE << Where_Else_Mask_Blk) |
                                (ONE << Parallel_Blk) |
				(ONE << SGI_Parallel_Blk) |
                                (ONE << Doall_Blk) |
				(ONE << Wait_Blk) |
				(ONE << SGI_Doacross_Blk) |
				(ONE << SGI_Parallel_Do_Blk) |
                                (ONE << Do_Parallel_Blk) |
				(ONE << SGI_Pdo_Blk) |
                                (ONE << Guard_Blk) |
				(ONE << SGI_Critical_Section_Blk) |
                                (ONE << Parallel_Case_Blk) |
				(ONE << SGI_Psection_Blk) |
				(ONE << SGI_Section_Blk) |
				(ONE << SGI_Single_Process_Blk) |
				(ONE << SGI_Region_Blk) |
                                (ONE << Open_Mp_Parallel_Blk) |
                                (ONE << Open_Mp_Do_Blk) |
                                (ONE << Open_Mp_Parallel_Sections_Blk) |
                                (ONE << Open_Mp_Sections_Blk) |
                                (ONE << Open_Mp_Section_Blk) |
                                (ONE << Open_Mp_Single_Blk) |
                                (ONE << Open_Mp_Parallel_Do_Blk) |
                                (ONE << Open_Mp_Master_Blk) |
                                (ONE << Open_Mp_Critical_Blk) |
                                (ONE << Open_Mp_Ordered_Blk) |
                                (ONE << Open_Mp_Workshare_Blk) | /* by jhs, 02/7/18 */
                                (ONE << Open_Mp_Parallel_Workshare_Blk) | /* by jhs, 02/7/18 */
				(ONE << Contains_Blk) |
				(ONE << Interface_Blk) |
				(ONE << Derived_Type_Blk) | 
				(ONE << Enum_Blk)),

			/*****  End_Interface_Stmt  *****/

			       ((ONE << Unknown_Blk) |
				(ONE << Blockdata_Blk) |
				(ONE << Module_Blk) |
				(ONE << Program_Blk) |
				(ONE << Function_Blk) |
				(ONE << Subroutine_Blk) |
				(ONE << Internal_Blk) |
				(ONE << Module_Proc_Blk) |
				(ONE << Interface_Body_Blk) |
				(ONE << Forall_Blk) |
				(ONE << If_Blk) |
				(ONE << If_Then_Blk) |
				(ONE << If_Else_If_Blk) |
				(ONE << If_Else_Blk) |
				(ONE << Do_Blk) |
				(ONE << Select_Blk) |
				(ONE << Case_Blk) |
				(ONE << Where_Then_Blk) |
				(ONE << Where_Else_Blk) |
				(ONE << Where_Else_Mask_Blk) |
                                (ONE << Parallel_Blk) |
				(ONE << SGI_Parallel_Blk) |
                                (ONE << Doall_Blk) |
				(ONE << Wait_Blk) |
				(ONE << SGI_Doacross_Blk) |
				(ONE << SGI_Parallel_Do_Blk) |
                                (ONE << Do_Parallel_Blk) |
				(ONE << SGI_Pdo_Blk) |
                                (ONE << Guard_Blk) |
				(ONE << SGI_Critical_Section_Blk) |
                                (ONE << Parallel_Case_Blk) |
				(ONE << SGI_Psection_Blk) |
				(ONE << SGI_Section_Blk) |
				(ONE << SGI_Single_Process_Blk) |
				(ONE << SGI_Region_Blk) |
                                (ONE << Open_Mp_Parallel_Blk) |
                                (ONE << Open_Mp_Do_Blk) |
                                (ONE << Open_Mp_Parallel_Sections_Blk) |
                                (ONE << Open_Mp_Sections_Blk) |
                                (ONE << Open_Mp_Section_Blk) |
                                (ONE << Open_Mp_Single_Blk) |
                                (ONE << Open_Mp_Parallel_Do_Blk) |
                                (ONE << Open_Mp_Master_Blk) |
                                (ONE << Open_Mp_Critical_Blk) |
                                (ONE << Open_Mp_Ordered_Blk) |
                                (ONE << Open_Mp_Workshare_Blk) | /* by jhs, 02/7/18 */
                                (ONE << Open_Mp_Parallel_Workshare_Blk) | /* by jhs, 02/7/18 */
				(ONE << Contains_Blk) |
				(ONE << Derived_Type_Blk) | 
				(ONE << Enum_Blk)),

			/*****  End_Module_Stmt  *****/

			       ((ONE << Unknown_Blk) |
				(ONE << Blockdata_Blk) |
				(ONE << Program_Blk) |
				(ONE << Function_Blk) |
				(ONE << Subroutine_Blk) |
				(ONE << Internal_Blk) |
				(ONE << Module_Proc_Blk) |
				(ONE << Interface_Body_Blk) |
				(ONE << Forall_Blk) |
				(ONE << If_Blk) |
				(ONE << If_Then_Blk) |
				(ONE << If_Else_If_Blk) |
				(ONE << If_Else_Blk) |
				(ONE << Do_Blk) |
				(ONE << Select_Blk) |
				(ONE << Case_Blk) |
				(ONE << Where_Then_Blk) |
				(ONE << Where_Else_Blk) |
				(ONE << Where_Else_Mask_Blk) |
                                (ONE << Parallel_Blk) |
				(ONE << SGI_Parallel_Blk) |
                                (ONE << Doall_Blk) |
				(ONE << Wait_Blk) |
				(ONE << SGI_Doacross_Blk) |
				(ONE << SGI_Parallel_Do_Blk) |
                                (ONE << Do_Parallel_Blk) |
				(ONE << SGI_Pdo_Blk) |
                                (ONE << Guard_Blk) |
				(ONE << SGI_Critical_Section_Blk) |
                                (ONE << Parallel_Case_Blk) |
				(ONE << SGI_Psection_Blk) |
				(ONE << SGI_Section_Blk) |
				(ONE << SGI_Single_Process_Blk) |
				(ONE << SGI_Region_Blk) |
                                (ONE << Open_Mp_Parallel_Blk) |
                                (ONE << Open_Mp_Do_Blk) |
                                (ONE << Open_Mp_Parallel_Sections_Blk) |
                                (ONE << Open_Mp_Sections_Blk) |
                                (ONE << Open_Mp_Section_Blk) |
                                (ONE << Open_Mp_Single_Blk) |
                                (ONE << Open_Mp_Parallel_Do_Blk) |
                                (ONE << Open_Mp_Master_Blk) |
                                (ONE << Open_Mp_Critical_Blk) |
                                (ONE << Open_Mp_Ordered_Blk) |
                                (ONE << Open_Mp_Workshare_Blk) | /* by jhs, 02/7/18 */
                                (ONE << Open_Mp_Parallel_Workshare_Blk) | /* by jhs, 02/7/18 */
				(ONE << Contains_Blk) |
				(ONE << Interface_Blk) |
				(ONE << Derived_Type_Blk) | 
				(ONE << Enum_Blk)),

			/*****  End_Program_Stmt  *****/

			       ((ONE << Unknown_Blk) |
				(ONE << Blockdata_Blk) |
				(ONE << Module_Blk) |
				(ONE << Function_Blk) |
				(ONE << Subroutine_Blk) |
				(ONE << Internal_Blk) |
				(ONE << Module_Proc_Blk) |
				(ONE << Interface_Body_Blk) |
				(ONE << Forall_Blk) |
				(ONE << If_Blk) |
				(ONE << If_Then_Blk) |
				(ONE << If_Else_If_Blk) |
				(ONE << If_Else_Blk) |
				(ONE << Do_Blk) |
				(ONE << Select_Blk) |
				(ONE << Case_Blk) |
				(ONE << Where_Then_Blk) |
				(ONE << Where_Else_Blk) |
				(ONE << Where_Else_Mask_Blk) |
                                (ONE << Parallel_Blk) |
				(ONE << SGI_Parallel_Blk) |
                                (ONE << Doall_Blk) |
				(ONE << Wait_Blk) |
				(ONE << SGI_Doacross_Blk) |
				(ONE << SGI_Parallel_Do_Blk) |
                                (ONE << Do_Parallel_Blk) |
				(ONE << SGI_Pdo_Blk) |
                                (ONE << Guard_Blk) |
				(ONE << SGI_Critical_Section_Blk) |
                                (ONE << Parallel_Case_Blk) |
				(ONE << SGI_Psection_Blk) |
				(ONE << SGI_Section_Blk) |
				(ONE << SGI_Single_Process_Blk) |
				(ONE << SGI_Region_Blk) |
                                (ONE << Open_Mp_Parallel_Blk) |
                                (ONE << Open_Mp_Do_Blk) |
                                (ONE << Open_Mp_Parallel_Sections_Blk) |
                                (ONE << Open_Mp_Sections_Blk) |
                                (ONE << Open_Mp_Section_Blk) |
                                (ONE << Open_Mp_Single_Blk) |
                                (ONE << Open_Mp_Parallel_Do_Blk) |
                                (ONE << Open_Mp_Master_Blk) |
                                (ONE << Open_Mp_Critical_Blk) |
                                (ONE << Open_Mp_Ordered_Blk) |
                                (ONE << Open_Mp_Workshare_Blk) | /* by jhs, 02/7/18 */
                                (ONE << Open_Mp_Parallel_Workshare_Blk) | /* by jhs, 02/7/18 */
				(ONE << Contains_Blk) |
				(ONE << Interface_Blk) |
				(ONE << Derived_Type_Blk) | 
				(ONE << Enum_Blk)),

			/*****  End_Select_Stmt  *****/

			       ((ONE << Unknown_Blk) |
				(ONE << Blockdata_Blk) |
				(ONE << Module_Blk) |
				(ONE << Program_Blk) |
				(ONE << Function_Blk) |
				(ONE << Subroutine_Blk) |
				(ONE << Internal_Blk) |
				(ONE << Module_Proc_Blk) |
				(ONE << Interface_Body_Blk) |
				(ONE << Forall_Blk) |
				(ONE << If_Blk) |
				(ONE << If_Then_Blk) |
				(ONE << If_Else_If_Blk) |
				(ONE << If_Else_Blk) |
				(ONE << Do_Blk) |
				(ONE << Where_Then_Blk) |
				(ONE << Where_Else_Blk) |
				(ONE << Where_Else_Mask_Blk) |
                                (ONE << Parallel_Blk) |
				(ONE << SGI_Parallel_Blk) |
                                (ONE << Doall_Blk) |
				(ONE << Wait_Blk) |
				(ONE << SGI_Doacross_Blk) |
				(ONE << SGI_Parallel_Do_Blk) |
                                (ONE << Do_Parallel_Blk) |
				(ONE << SGI_Pdo_Blk) |
                                (ONE << Guard_Blk) |
				(ONE << SGI_Critical_Section_Blk) |
                                (ONE << Parallel_Case_Blk) |
				(ONE << SGI_Psection_Blk) |
				(ONE << SGI_Section_Blk) |
				(ONE << SGI_Single_Process_Blk) |
				(ONE << SGI_Region_Blk) |
                                (ONE << Open_Mp_Parallel_Blk) |
                                (ONE << Open_Mp_Do_Blk) |
                                (ONE << Open_Mp_Parallel_Sections_Blk) |
                                (ONE << Open_Mp_Sections_Blk) |
                                (ONE << Open_Mp_Section_Blk) |
                                (ONE << Open_Mp_Single_Blk) |
                                (ONE << Open_Mp_Parallel_Do_Blk) |
                                (ONE << Open_Mp_Master_Blk) |
                                (ONE << Open_Mp_Critical_Blk) |
                                (ONE << Open_Mp_Ordered_Blk) |
                                (ONE << Open_Mp_Workshare_Blk) | /* by jhs, 02/7/18 */
                                (ONE << Open_Mp_Parallel_Workshare_Blk) | /* by jhs, 02/7/18 */
				(ONE << Contains_Blk) |
				(ONE << Interface_Blk) |
				(ONE << Derived_Type_Blk) | 
				(ONE << Enum_Blk)),

			/*****  End_Stmt  *****/

			       ((ONE << Unknown_Blk) |
				(ONE << Internal_Blk) |
				(ONE << Module_Proc_Blk) |
				(ONE << Forall_Blk) |
				(ONE << If_Blk) |
				(ONE << If_Then_Blk) |
				(ONE << If_Else_If_Blk) |
				(ONE << If_Else_Blk) |
				(ONE << Do_Blk) |
				(ONE << Select_Blk) |
				(ONE << Case_Blk) |
				(ONE << Where_Then_Blk) |
				(ONE << Where_Else_Blk) |
				(ONE << Where_Else_Mask_Blk) |
                                (ONE << Parallel_Blk) |
				(ONE << SGI_Parallel_Blk) |
                                (ONE << Doall_Blk) |
				(ONE << Wait_Blk) |
				(ONE << SGI_Doacross_Blk) |
				(ONE << SGI_Parallel_Do_Blk) |
                                (ONE << Do_Parallel_Blk) |
				(ONE << SGI_Pdo_Blk) |
                                (ONE << Guard_Blk) |
				(ONE << SGI_Critical_Section_Blk) |
                                (ONE << Parallel_Case_Blk) |
				(ONE << SGI_Psection_Blk) |
				(ONE << SGI_Section_Blk) |
				(ONE << SGI_Single_Process_Blk) |
				(ONE << SGI_Region_Blk) |
                                (ONE << Open_Mp_Parallel_Blk) |
                                (ONE << Open_Mp_Do_Blk) |
                                (ONE << Open_Mp_Parallel_Sections_Blk) |
                                (ONE << Open_Mp_Sections_Blk) |
                                (ONE << Open_Mp_Section_Blk) |
                                (ONE << Open_Mp_Single_Blk) |
                                (ONE << Open_Mp_Parallel_Do_Blk) |
                                (ONE << Open_Mp_Master_Blk) |
                                (ONE << Open_Mp_Critical_Blk) |
                                (ONE << Open_Mp_Ordered_Blk) |
                                (ONE << Open_Mp_Workshare_Blk) | /* by jhs, 02/7/18 */
                                (ONE << Open_Mp_Parallel_Workshare_Blk) | /* by jhs, 02/7/18 */
				(ONE << Contains_Blk) |
				(ONE << Interface_Blk) |
				(ONE << Derived_Type_Blk) | 
				(ONE << Enum_Blk)),

			/*****  End_Subroutine_Stmt  *****/

			       ((ONE << Unknown_Blk) |
				(ONE << Blockdata_Blk) |
				(ONE << Module_Blk) |
				(ONE << Program_Blk) |
				(ONE << Function_Blk) |
				(ONE << Forall_Blk) |
				(ONE << If_Blk) |
				(ONE << If_Then_Blk) |
				(ONE << If_Else_If_Blk) |
				(ONE << If_Else_Blk) |
				(ONE << Do_Blk) |
				(ONE << Select_Blk) |
				(ONE << Case_Blk) |
				(ONE << Where_Then_Blk) |
				(ONE << Where_Else_Blk) |
				(ONE << Where_Else_Mask_Blk) |
                                (ONE << Parallel_Blk) |
				(ONE << SGI_Parallel_Blk) |
                                (ONE << Doall_Blk) |
				(ONE << Wait_Blk) |
				(ONE << SGI_Doacross_Blk) |
				(ONE << SGI_Parallel_Do_Blk) |
                                (ONE << Do_Parallel_Blk) |
				(ONE << SGI_Pdo_Blk) |
                                (ONE << Guard_Blk) |
				(ONE << SGI_Critical_Section_Blk) |
                                (ONE << Parallel_Case_Blk) |
				(ONE << SGI_Psection_Blk) |
				(ONE << SGI_Section_Blk) |
				(ONE << SGI_Single_Process_Blk) |
				(ONE << SGI_Region_Blk) |
                                (ONE << Open_Mp_Parallel_Blk) |
                                (ONE << Open_Mp_Do_Blk) |
                                (ONE << Open_Mp_Parallel_Sections_Blk) |
                                (ONE << Open_Mp_Sections_Blk) |
                                (ONE << Open_Mp_Section_Blk) |
                                (ONE << Open_Mp_Single_Blk) |
                                (ONE << Open_Mp_Parallel_Do_Blk) |
                                (ONE << Open_Mp_Master_Blk) |
                                (ONE << Open_Mp_Critical_Blk) |
                                (ONE << Open_Mp_Ordered_Blk) |
                                (ONE << Open_Mp_Workshare_Blk) | /* by jhs, 02/7/18 */
                                (ONE << Open_Mp_Parallel_Workshare_Blk) | /* by jhs, 02/7/18 */
				(ONE << Contains_Blk) |
				(ONE << Interface_Blk) |
				(ONE << Derived_Type_Blk) | 
				(ONE << Enum_Blk)),

			/*****  End_Type_Stmt  *****/

			       ((ONE << Unknown_Blk) |
				(ONE << Blockdata_Blk) |
				(ONE << Module_Blk) |
				(ONE << Program_Blk) |
				(ONE << Function_Blk) |
				(ONE << Subroutine_Blk) |
				(ONE << Internal_Blk) |
				(ONE << Module_Proc_Blk) |
				(ONE << Interface_Body_Blk) |
				(ONE << Forall_Blk) |
				(ONE << If_Blk) |
				(ONE << If_Then_Blk) |
				(ONE << If_Else_If_Blk) |
				(ONE << If_Else_Blk) |
				(ONE << Do_Blk) |
				(ONE << Select_Blk) |
				(ONE << Case_Blk) |
				(ONE << Where_Then_Blk) |
				(ONE << Where_Else_Blk) |
				(ONE << Where_Else_Mask_Blk) |
                                (ONE << Parallel_Blk) |
				(ONE << SGI_Parallel_Blk) |
                                (ONE << Doall_Blk) |
				(ONE << Wait_Blk) |
				(ONE << SGI_Doacross_Blk) |
				(ONE << SGI_Parallel_Do_Blk) |
                                (ONE << Do_Parallel_Blk) |
				(ONE << SGI_Pdo_Blk) |
                                (ONE << Guard_Blk) |
				(ONE << SGI_Critical_Section_Blk) |
                                (ONE << Parallel_Case_Blk) |
				(ONE << SGI_Psection_Blk) |
				(ONE << SGI_Section_Blk) |
				(ONE << SGI_Single_Process_Blk) |
				(ONE << SGI_Region_Blk) |
                                (ONE << Open_Mp_Parallel_Blk) |
                                (ONE << Open_Mp_Do_Blk) |
                                (ONE << Open_Mp_Parallel_Sections_Blk) |
                                (ONE << Open_Mp_Sections_Blk) |
                                (ONE << Open_Mp_Section_Blk) |
                                (ONE << Open_Mp_Single_Blk) |
                                (ONE << Open_Mp_Parallel_Do_Blk) |
                                (ONE << Open_Mp_Master_Blk) |
                                (ONE << Open_Mp_Critical_Blk) |
                                (ONE << Open_Mp_Ordered_Blk) |
                                (ONE << Open_Mp_Workshare_Blk) | /* by jhs, 02/7/18 */
                                (ONE << Open_Mp_Parallel_Workshare_Blk) | /* by jhs, 02/7/18 */
				(ONE << Contains_Blk) |
				(ONE << Interface_Blk) |   
				(ONE << Enum_Blk)),

			/*****  End_Where_Stmt  *****/

			       ((ONE << Unknown_Blk) |
				(ONE << Blockdata_Blk) |
				(ONE << Module_Blk) |
				(ONE << Program_Blk) |
				(ONE << Function_Blk) |
				(ONE << Subroutine_Blk) |
				(ONE << Internal_Blk) |
				(ONE << Module_Proc_Blk) |
				(ONE << Interface_Body_Blk) |
				(ONE << Forall_Blk) |
				(ONE << If_Blk) |
				(ONE << If_Then_Blk) |
				(ONE << If_Else_If_Blk) |
				(ONE << If_Else_Blk) |
				(ONE << Do_Blk) |
				(ONE << Select_Blk) |
				(ONE << Case_Blk) |
                                (ONE << Parallel_Blk) |
				(ONE << SGI_Parallel_Blk) |
                                (ONE << Doall_Blk) |
				(ONE << Wait_Blk) |
				(ONE << SGI_Doacross_Blk) |
				(ONE << SGI_Parallel_Do_Blk) |
                                (ONE << Do_Parallel_Blk) |
				(ONE << SGI_Pdo_Blk) |
                                (ONE << Guard_Blk) |
				(ONE << SGI_Critical_Section_Blk) |
                                (ONE << Parallel_Case_Blk) |
				(ONE << SGI_Psection_Blk) |
				(ONE << SGI_Section_Blk) |
				(ONE << SGI_Single_Process_Blk) |
				(ONE << SGI_Region_Blk) |
                                (ONE << Open_Mp_Parallel_Blk) |
                                (ONE << Open_Mp_Do_Blk) |
                                (ONE << Open_Mp_Parallel_Sections_Blk) |
                                (ONE << Open_Mp_Sections_Blk) |
                                (ONE << Open_Mp_Section_Blk) |
                                (ONE << Open_Mp_Single_Blk) |
                                (ONE << Open_Mp_Parallel_Do_Blk) |
                                (ONE << Open_Mp_Master_Blk) |
                                (ONE << Open_Mp_Critical_Blk) |
                                (ONE << Open_Mp_Ordered_Blk) |
                                (ONE << Open_Mp_Workshare_Blk) | /* by jhs, 02/7/18 */
                                (ONE << Open_Mp_Parallel_Workshare_Blk) | /* by jhs, 02/7/18 */
				(ONE << Contains_Blk) |
				(ONE << Interface_Blk) |
				(ONE << Derived_Type_Blk) | 
				(ONE << Enum_Blk)),

			/*****  Allocate_Stmt  *****/

			       ((ONE << Unknown_Blk) |
				(ONE << Blockdata_Blk) |
				(ONE << Module_Blk) |
				(ONE << Interface_Body_Blk) |
				(ONE << Forall_Blk) |
				(ONE << If_Blk) |
				(ONE << Where_Then_Blk) |
				(ONE << Where_Else_Blk) |
				(ONE << Where_Else_Mask_Blk) |
				(ONE << SGI_Psection_Blk) |
				(ONE << Select_Blk) |
				(ONE << Contains_Blk) |
				(ONE << Interface_Blk) |
				(ONE << Derived_Type_Blk) | 
				(ONE << Enum_Blk)),

			/*****  Arith_If_Stmt  *****/

			       ((ONE << Unknown_Blk) |
				(ONE << Blockdata_Blk) |
				(ONE << Module_Blk) |
				(ONE << Interface_Body_Blk) |
				(ONE << Forall_Blk) |
				(ONE << If_Blk) |
				(ONE << Where_Then_Blk) |
				(ONE << Where_Else_Blk) |
				(ONE << Where_Else_Mask_Blk) |
				(ONE << SGI_Psection_Blk) |
				(ONE << Select_Blk) |
				(ONE << Contains_Blk) |
				(ONE << Interface_Blk) |
				(ONE << Derived_Type_Blk) | 
				(ONE << Enum_Blk)),

			/*****  Assign_Stmt  *****/

			       ((ONE << Unknown_Blk) |
				(ONE << Blockdata_Blk) |
				(ONE << Module_Blk) |
				(ONE << Interface_Body_Blk) |
				(ONE << Forall_Blk) |
				(ONE << If_Blk) |
				(ONE << Where_Then_Blk) |
				(ONE << Where_Else_Blk) |
				(ONE << Where_Else_Mask_Blk) |
				(ONE << SGI_Psection_Blk) |
				(ONE << Select_Blk) |
				(ONE << Contains_Blk) |
				(ONE << Interface_Blk) |
				(ONE << Derived_Type_Blk) | 
				(ONE << Enum_Blk)),

			/*****  Assignment_Stmt  *****/

			       ((ONE << Unknown_Blk) |
				(ONE << Blockdata_Blk) |
				(ONE << Module_Blk) |
				(ONE << Interface_Body_Blk) |
				(ONE << If_Blk) |
				(ONE << SGI_Psection_Blk) |
				(ONE << Select_Blk) |
				(ONE << Contains_Blk) |
				(ONE << Interface_Blk) |
				(ONE << Derived_Type_Blk) | 
				(ONE << Enum_Blk)),

			/*****  Backspace_Stmt  *****/

			       ((ONE << Unknown_Blk) |
				(ONE << Blockdata_Blk) |
				(ONE << Module_Blk) |
				(ONE << Interface_Body_Blk) |
				(ONE << Forall_Blk) |
				(ONE << If_Blk) |
				(ONE << Where_Then_Blk) |
				(ONE << Where_Else_Blk) |
				(ONE << Where_Else_Mask_Blk) |
				(ONE << SGI_Psection_Blk) |
				(ONE << Select_Blk) |
				(ONE << Contains_Blk) |
				(ONE << Interface_Blk) |
				(ONE << Derived_Type_Blk) | 
				(ONE << Enum_Blk)),

			/*****  Buffer_Stmt  *****/

			       ((ONE << Unknown_Blk) |
				(ONE << Blockdata_Blk) |
				(ONE << Module_Blk) |
				(ONE << Interface_Body_Blk) |
				(ONE << Forall_Blk) |
				(ONE << If_Blk) |
				(ONE << Where_Then_Blk) |
				(ONE << Where_Else_Blk) |
				(ONE << Where_Else_Mask_Blk) |
				(ONE << SGI_Psection_Blk) |
				(ONE << Select_Blk) |
				(ONE << Contains_Blk) |
				(ONE << Interface_Blk) |
				(ONE << Derived_Type_Blk) | 
				(ONE << Enum_Blk)),

			/*****  Call_Stmt  *****/

			       ((ONE << Unknown_Blk) |
				(ONE << Blockdata_Blk) |
				(ONE << Module_Blk) |
				(ONE << Interface_Body_Blk) |
				(ONE << Forall_Blk) |
				(ONE << If_Blk) |
				(ONE << Where_Then_Blk) |
				(ONE << Where_Else_Blk) |
				(ONE << Where_Else_Mask_Blk) |
				(ONE << SGI_Psection_Blk) |
				(ONE << Select_Blk) |
				(ONE << Contains_Blk) |
				(ONE << Interface_Blk) |
				(ONE << Derived_Type_Blk) | 
				(ONE << Enum_Blk)),

			/*****  Case_Stmt  *****/

			       ((ONE << Unknown_Blk) |
				(ONE << Blockdata_Blk) |
				(ONE << Module_Blk) |
				(ONE << Program_Blk) |
				(ONE << Function_Blk) |
				(ONE << Subroutine_Blk) |
				(ONE << Internal_Blk) |
				(ONE << Module_Proc_Blk) |
				(ONE << Interface_Body_Blk) |
				(ONE << Forall_Blk) |
				(ONE << If_Blk) |
				(ONE << If_Then_Blk) |
				(ONE << If_Else_If_Blk) |
				(ONE << If_Else_Blk) |
				(ONE << Do_Blk) |
				(ONE << Where_Then_Blk) |
				(ONE << Where_Else_Blk) |
				(ONE << Where_Else_Mask_Blk) |
                                (ONE << Parallel_Blk) |
				(ONE << SGI_Parallel_Blk) |
                                (ONE << Doall_Blk) |
				(ONE << Wait_Blk) |
				(ONE << SGI_Doacross_Blk) |
				(ONE << SGI_Parallel_Do_Blk) |
                                (ONE << Do_Parallel_Blk) |
				(ONE << SGI_Pdo_Blk) |
                                (ONE << Guard_Blk) |
				(ONE << SGI_Critical_Section_Blk) |
                                (ONE << Parallel_Case_Blk) |
				(ONE << SGI_Psection_Blk) |
				(ONE << SGI_Section_Blk) |
				(ONE << SGI_Single_Process_Blk) |
				(ONE << SGI_Region_Blk) |
                                (ONE << Open_Mp_Parallel_Blk) |
                                (ONE << Open_Mp_Do_Blk) |
                                (ONE << Open_Mp_Parallel_Sections_Blk) |
                                (ONE << Open_Mp_Sections_Blk) |
                                (ONE << Open_Mp_Section_Blk) |
                                (ONE << Open_Mp_Single_Blk) |
                                (ONE << Open_Mp_Parallel_Do_Blk) |
                                (ONE << Open_Mp_Master_Blk) |
                                (ONE << Open_Mp_Critical_Blk) |
                                (ONE << Open_Mp_Ordered_Blk) |
                                (ONE << Open_Mp_Workshare_Blk) | /* by jhs, 02/7/18 */
                                (ONE << Open_Mp_Parallel_Workshare_Blk) | /* by jhs, 02/7/18 */
				(ONE << Contains_Blk) |
				(ONE << Interface_Blk) |
				(ONE << Derived_Type_Blk) | 
				(ONE << Enum_Blk)),

			/*****  Close_Stmt  *****/

			       ((ONE << Unknown_Blk) |
				(ONE << Blockdata_Blk) |
				(ONE << Module_Blk) |
				(ONE << Interface_Body_Blk) |
				(ONE << Forall_Blk) |
				(ONE << If_Blk) |
				(ONE << Where_Then_Blk) |
				(ONE << Where_Else_Blk) |
				(ONE << Where_Else_Mask_Blk) |
				(ONE << SGI_Psection_Blk) |
				(ONE << Select_Blk) |
				(ONE << Contains_Blk) |
				(ONE << Interface_Blk) |
				(ONE << Derived_Type_Blk) | 
				(ONE << Enum_Blk)),

			/*****  Continue_Stmt  *****/

			       ((ONE << Unknown_Blk) |
				(ONE << Blockdata_Blk) |
				(ONE << Module_Blk) |
				(ONE << Interface_Body_Blk) |
				(ONE << Forall_Blk) |
				(ONE << If_Blk) |
				(ONE << Where_Then_Blk) |
				(ONE << Where_Else_Blk) |
				(ONE << Where_Else_Mask_Blk) |
				(ONE << SGI_Psection_Blk) |
				(ONE << Select_Blk) |
				(ONE << Contains_Blk) |
				(ONE << Interface_Blk) |
				(ONE << Derived_Type_Blk) | 
				(ONE << Enum_Blk)),

			/*****  Cycle_Stmt  *****/

			       ((ONE << Unknown_Blk) |
				(ONE << Blockdata_Blk) |
				(ONE << Module_Blk) |
				(ONE << Program_Blk) |
				(ONE << Function_Blk) |
				(ONE << Subroutine_Blk) |
				(ONE << Internal_Blk) |
				(ONE << Module_Proc_Blk) |
				(ONE << Interface_Body_Blk) |
				(ONE << Forall_Blk) |
				(ONE << If_Blk) |
				(ONE << Where_Then_Blk) |
				(ONE << Where_Else_Blk) |
				(ONE << Where_Else_Mask_Blk) |
				(ONE << SGI_Psection_Blk) |
				(ONE << Select_Blk) |
				(ONE << Contains_Blk) |
				(ONE << Interface_Blk) |
				(ONE << Derived_Type_Blk) | 
				(ONE << Enum_Blk)),

			/*****  Deallocate_Stmt  *****/

			       ((ONE << Unknown_Blk) |
				(ONE << Blockdata_Blk) |
				(ONE << Module_Blk) |
				(ONE << Interface_Body_Blk) |
				(ONE << Forall_Blk) |
				(ONE << If_Blk) |
				(ONE << Where_Then_Blk) |
				(ONE << Where_Else_Blk) |
				(ONE << Where_Else_Mask_Blk) |
				(ONE << SGI_Psection_Blk) |
				(ONE << Select_Blk) |
				(ONE << Contains_Blk) |
				(ONE << Interface_Blk) |
				(ONE << Derived_Type_Blk) | 
				(ONE << Enum_Blk)),

			/*****  Decode_Stmt  *****/

			       ((ONE << Unknown_Blk) |
				(ONE << Blockdata_Blk) |
				(ONE << Module_Blk) |
				(ONE << Interface_Body_Blk) |
				(ONE << Forall_Blk) |
				(ONE << If_Blk) |
				(ONE << Where_Then_Blk) |
				(ONE << Where_Else_Blk) |
				(ONE << Where_Else_Mask_Blk) |
				(ONE << SGI_Psection_Blk) |
				(ONE << Select_Blk) |
				(ONE << Contains_Blk) |
				(ONE << Interface_Blk) |
				(ONE << Derived_Type_Blk) | 
				(ONE << Enum_Blk)),

			/*****  Do_Iterative_Stmt  *****/

			       ((ONE << Unknown_Blk) |
				(ONE << Blockdata_Blk) |
				(ONE << Module_Blk) |
				(ONE << Interface_Body_Blk) |
				(ONE << Forall_Blk) |
				(ONE << If_Blk) |
				(ONE << Where_Then_Blk) |
				(ONE << Where_Else_Blk) |
				(ONE << Where_Else_Mask_Blk) |
				(ONE << SGI_Psection_Blk) |
				(ONE << Select_Blk) |
				(ONE << Contains_Blk) |
				(ONE << Interface_Blk) |
				(ONE << Derived_Type_Blk) | 
				(ONE << Enum_Blk)),

			/*****  Do_While_Stmt  *****/

			       ((ONE << Unknown_Blk) |
				(ONE << Blockdata_Blk) |
				(ONE << Module_Blk) |
				(ONE << Interface_Body_Blk) |
				(ONE << Forall_Blk) |
				(ONE << If_Blk) |
				(ONE << Where_Then_Blk) |
				(ONE << Where_Else_Blk) |
				(ONE << Where_Else_Mask_Blk) |
				(ONE << SGI_Psection_Blk) |
				(ONE << Select_Blk) |
				(ONE << Contains_Blk) |
				(ONE << Interface_Blk) |
				(ONE << Derived_Type_Blk) | 
				(ONE << Enum_Blk)),

			/*****  Do_Infinite_Stmt  *****/

			       ((ONE << Unknown_Blk) |
				(ONE << Blockdata_Blk) |
				(ONE << Module_Blk) |
				(ONE << Interface_Body_Blk) |
				(ONE << Forall_Blk) |
				(ONE << If_Blk) |
				(ONE << Where_Then_Blk) |
				(ONE << Where_Else_Blk) |
				(ONE << Where_Else_Mask_Blk) |
				(ONE << SGI_Psection_Blk) |
				(ONE << Select_Blk) |
				(ONE << Contains_Blk) |
				(ONE << Interface_Blk) |
				(ONE << Derived_Type_Blk) | 
				(ONE << Enum_Blk)),

			/*****  Else_Stmt  *****/

			       ((ONE << Unknown_Blk) |
				(ONE << Blockdata_Blk) |
				(ONE << Module_Blk) |
				(ONE << Program_Blk) |
				(ONE << Function_Blk) |
				(ONE << Subroutine_Blk) |
				(ONE << Internal_Blk) |
				(ONE << Module_Proc_Blk) |
				(ONE << Interface_Body_Blk) |
				(ONE << Forall_Blk) |
				(ONE << If_Blk) |
				(ONE << If_Else_Blk) |
				(ONE << Do_Blk) |
				(ONE << Select_Blk) |
				(ONE << Case_Blk) |
				(ONE << Where_Then_Blk) |
				(ONE << Where_Else_Blk) |
				(ONE << Where_Else_Mask_Blk) |
                                (ONE << Parallel_Blk) |
				(ONE << SGI_Parallel_Blk) |
                                (ONE << Doall_Blk) |
				(ONE << Wait_Blk) |
				(ONE << SGI_Doacross_Blk) |
				(ONE << SGI_Parallel_Do_Blk) |
                                (ONE << Do_Parallel_Blk) |
				(ONE << SGI_Pdo_Blk) |
                                (ONE << Guard_Blk) |
				(ONE << SGI_Critical_Section_Blk) |
                                (ONE << Parallel_Case_Blk) |
				(ONE << SGI_Psection_Blk) |
				(ONE << SGI_Section_Blk) |
				(ONE << SGI_Single_Process_Blk) |
				(ONE << SGI_Region_Blk) |
                                (ONE << Open_Mp_Parallel_Blk) |
                                (ONE << Open_Mp_Do_Blk) |
                                (ONE << Open_Mp_Parallel_Sections_Blk) |
                                (ONE << Open_Mp_Sections_Blk) |
                                (ONE << Open_Mp_Section_Blk) |
                                (ONE << Open_Mp_Single_Blk) |
                                (ONE << Open_Mp_Parallel_Do_Blk) |
                                (ONE << Open_Mp_Master_Blk) |
                                (ONE << Open_Mp_Critical_Blk) |
                                (ONE << Open_Mp_Ordered_Blk) |
                                (ONE << Open_Mp_Workshare_Blk) | /* by jhs, 02/7/18 */
                                (ONE << Open_Mp_Parallel_Workshare_Blk) | /* by jhs, 02/7/18 */
				(ONE << Contains_Blk) |
				(ONE << Interface_Blk) |
				(ONE << Derived_Type_Blk) | 
				(ONE << Enum_Blk)),

			/*****  Else_If_Stmt  *****/

			       ((ONE << Unknown_Blk) |
				(ONE << Blockdata_Blk) |
				(ONE << Module_Blk) |
				(ONE << Program_Blk) |
				(ONE << Function_Blk) |
				(ONE << Subroutine_Blk) |
				(ONE << Internal_Blk) |
				(ONE << Module_Proc_Blk) |
				(ONE << Interface_Body_Blk) |
				(ONE << Forall_Blk) |
				(ONE << If_Blk) |
				(ONE << If_Else_Blk) |
				(ONE << Do_Blk) |
				(ONE << Select_Blk) |
				(ONE << Case_Blk) |
				(ONE << Where_Then_Blk) |
				(ONE << Where_Else_Blk) |
				(ONE << Where_Else_Mask_Blk) |
                                (ONE << Parallel_Blk) |
				(ONE << SGI_Parallel_Blk) |
                                (ONE << Doall_Blk) |
				(ONE << Wait_Blk) |
				(ONE << SGI_Doacross_Blk) |
				(ONE << SGI_Parallel_Do_Blk) |
                                (ONE << Do_Parallel_Blk) |
				(ONE << SGI_Pdo_Blk) |
                                (ONE << Guard_Blk) |
				(ONE << SGI_Critical_Section_Blk) |
                                (ONE << Parallel_Case_Blk) |
				(ONE << SGI_Psection_Blk) |
				(ONE << SGI_Section_Blk) |
				(ONE << SGI_Single_Process_Blk) |
				(ONE << SGI_Region_Blk) |
                                (ONE << Open_Mp_Parallel_Blk) |
                                (ONE << Open_Mp_Do_Blk) |
                                (ONE << Open_Mp_Parallel_Sections_Blk) |
                                (ONE << Open_Mp_Sections_Blk) |
                                (ONE << Open_Mp_Section_Blk) |
                                (ONE << Open_Mp_Single_Blk) |
                                (ONE << Open_Mp_Parallel_Do_Blk) |
                                (ONE << Open_Mp_Master_Blk) |
                                (ONE << Open_Mp_Critical_Blk) |
                                (ONE << Open_Mp_Ordered_Blk) |
                                (ONE << Open_Mp_Workshare_Blk) | /* by jhs, 02/7/18 */
                                (ONE << Open_Mp_Parallel_Workshare_Blk) | /* by jhs, 02/7/18 */
				(ONE << Contains_Blk) |
				(ONE << Interface_Blk) |
				(ONE << Derived_Type_Blk) | 
				(ONE << Enum_Blk)),

			/*****  Else_Where_Stmt  *****/

			       ((ONE << Unknown_Blk) |
				(ONE << Blockdata_Blk) |
				(ONE << Module_Blk) |
				(ONE << Program_Blk) |
				(ONE << Function_Blk) |
				(ONE << Subroutine_Blk) |
				(ONE << Internal_Blk) |
				(ONE << Module_Proc_Blk) |
				(ONE << Interface_Body_Blk) |
				(ONE << Forall_Blk) |
				(ONE << If_Blk) |
				(ONE << If_Then_Blk) |
				(ONE << If_Else_If_Blk) |
				(ONE << If_Else_Blk) |
				(ONE << Do_Blk) |
				(ONE << Select_Blk) |
				(ONE << Case_Blk) |
				(ONE << Where_Else_Blk) |
                                (ONE << Parallel_Blk) |
				(ONE << SGI_Parallel_Blk) |
                                (ONE << Doall_Blk) |
				(ONE << Wait_Blk) |
				(ONE << SGI_Doacross_Blk) |
				(ONE << SGI_Parallel_Do_Blk) |
                                (ONE << Do_Parallel_Blk) |
				(ONE << SGI_Pdo_Blk) |
                                (ONE << Guard_Blk) |
				(ONE << SGI_Critical_Section_Blk) |
                                (ONE << Parallel_Case_Blk) |
				(ONE << SGI_Psection_Blk) |
				(ONE << SGI_Section_Blk) |
				(ONE << SGI_Single_Process_Blk) |
				(ONE << SGI_Region_Blk) |
                                (ONE << Open_Mp_Parallel_Blk) |
                                (ONE << Open_Mp_Do_Blk) |
                                (ONE << Open_Mp_Parallel_Sections_Blk) |
                                (ONE << Open_Mp_Sections_Blk) |
                                (ONE << Open_Mp_Section_Blk) |
                                (ONE << Open_Mp_Single_Blk) |
                                (ONE << Open_Mp_Parallel_Do_Blk) |
                                (ONE << Open_Mp_Master_Blk) |
                                (ONE << Open_Mp_Critical_Blk) |
                                (ONE << Open_Mp_Ordered_Blk) |
                                (ONE << Open_Mp_Workshare_Blk) | /* by jhs, 02/7/18 */
                                (ONE << Open_Mp_Parallel_Workshare_Blk) | /* by jhs, 02/7/18 */
				(ONE << Contains_Blk) |
				(ONE << Interface_Blk) |
				(ONE << Derived_Type_Blk) | 
				(ONE << Enum_Blk)),

			/*****  Encode_Stmt  *****/

			       ((ONE << Unknown_Blk) |
				(ONE << Blockdata_Blk) |
				(ONE << Module_Blk) |
				(ONE << Interface_Body_Blk) |
				(ONE << Forall_Blk) |
				(ONE << If_Blk) |
				(ONE << Where_Then_Blk) |
				(ONE << Where_Else_Blk) |
				(ONE << Where_Else_Mask_Blk) |
				(ONE << SGI_Psection_Blk) |
				(ONE << Select_Blk) |
				(ONE << Contains_Blk) |
				(ONE << Interface_Blk) |
				(ONE << Derived_Type_Blk) | 
				(ONE << Enum_Blk)),

			/*****  Endfile_Stmt  *****/

			       ((ONE << Unknown_Blk) |
				(ONE << Blockdata_Blk) |
				(ONE << Module_Blk) |
				(ONE << Interface_Body_Blk) |
				(ONE << Forall_Blk) |
				(ONE << If_Blk) |
				(ONE << Where_Then_Blk) |
				(ONE << Where_Else_Blk) |
				(ONE << Where_Else_Mask_Blk) |
				(ONE << SGI_Psection_Blk) |
				(ONE << Select_Blk) |
				(ONE << Contains_Blk) |
				(ONE << Interface_Blk) |
				(ONE << Derived_Type_Blk) | 
				(ONE << Enum_Blk)),

			/*****  Entry_Stmt  *****/

			       ((ONE << Unknown_Blk) |
				(ONE << Blockdata_Blk) |
				(ONE << Module_Blk) |
				(ONE << Program_Blk) |
				(ONE << Interface_Body_Blk) |
				(ONE << Internal_Blk) |
				(ONE << Forall_Blk) |
				(ONE << If_Blk) |
				(ONE << If_Then_Blk) |
				(ONE << If_Else_If_Blk) |
				(ONE << If_Else_Blk) |
				(ONE << Do_Blk) |
				(ONE << Select_Blk) |
				(ONE << Case_Blk) |
				(ONE << Where_Then_Blk) |
				(ONE << Where_Else_Blk) |
				(ONE << Where_Else_Mask_Blk) |
                                (ONE << Parallel_Blk) |
				(ONE << SGI_Parallel_Blk) |
                                (ONE << Doall_Blk) |
				(ONE << Wait_Blk) |
				(ONE << SGI_Doacross_Blk) |
				(ONE << SGI_Parallel_Do_Blk) |
                                (ONE << Do_Parallel_Blk) |
				(ONE << SGI_Pdo_Blk) |
                                (ONE << Guard_Blk) |
				(ONE << SGI_Critical_Section_Blk) |
                                (ONE << Parallel_Case_Blk) |
				(ONE << SGI_Psection_Blk) |
				(ONE << SGI_Section_Blk) |
				(ONE << SGI_Single_Process_Blk) |
                                (ONE << Open_Mp_Parallel_Blk) |
                                (ONE << Open_Mp_Do_Blk) |
                                (ONE << Open_Mp_Parallel_Sections_Blk) |
                                (ONE << Open_Mp_Sections_Blk) |
                                (ONE << Open_Mp_Section_Blk) |
                                (ONE << Open_Mp_Single_Blk) |
                                (ONE << Open_Mp_Parallel_Do_Blk) |
                                (ONE << Open_Mp_Master_Blk) |
                                (ONE << Open_Mp_Critical_Blk) |
                                (ONE << Open_Mp_Ordered_Blk) |
                                (ONE << Open_Mp_Workshare_Blk) | /* by jhs, 02/7/18 */
                                (ONE << Open_Mp_Parallel_Workshare_Blk) | /* by jhs, 02/7/18 */
				(ONE << Contains_Blk) |
				(ONE << Interface_Blk) |
				(ONE << Derived_Type_Blk) | 
				(ONE << Enum_Blk)),

			/*****  Exit_Stmt  *****/

			       ((ONE << Unknown_Blk) |
				(ONE << Blockdata_Blk) |
				(ONE << Module_Blk) |
				(ONE << Program_Blk) |
				(ONE << Function_Blk) |
				(ONE << Subroutine_Blk) |
				(ONE << Internal_Blk) |
				(ONE << Module_Proc_Blk) |
				(ONE << Interface_Body_Blk) |
				(ONE << Forall_Blk) |
				(ONE << If_Blk) |
				(ONE << Where_Then_Blk) |
				(ONE << Where_Else_Blk) |
				(ONE << Where_Else_Mask_Blk) |
				(ONE << SGI_Psection_Blk) |
				(ONE << Select_Blk) |
				(ONE << Contains_Blk) |
				(ONE << Interface_Blk) |
				(ONE << Derived_Type_Blk) | 
				(ONE << Enum_Blk)),

			/*****  Go_To_Stmt  *****/

			       ((ONE << Unknown_Blk) |
				(ONE << Blockdata_Blk) |
				(ONE << Module_Blk) |
				(ONE << Interface_Body_Blk) |
				(ONE << Forall_Blk) |
				(ONE << If_Blk) |
				(ONE << Where_Then_Blk) |
				(ONE << Where_Else_Blk) |
				(ONE << Where_Else_Mask_Blk) |
				(ONE << SGI_Psection_Blk) |
				(ONE << Select_Blk) |
				(ONE << Contains_Blk) |
				(ONE << Interface_Blk) |
				(ONE << Derived_Type_Blk) | 
				(ONE << Enum_Blk)),

			/*****  If_Cstrct_Stmt  *****/

			       ((ONE << Unknown_Blk) |
				(ONE << Blockdata_Blk) |
				(ONE << Module_Blk) |
				(ONE << Interface_Body_Blk) |
				(ONE << Forall_Blk) |
				(ONE << If_Blk) |
				(ONE << Where_Then_Blk) |
				(ONE << Where_Else_Blk) |
				(ONE << Where_Else_Mask_Blk) |
				(ONE << SGI_Psection_Blk) |
				(ONE << Select_Blk) |
				(ONE << Contains_Blk) |
				(ONE << Interface_Blk) |
				(ONE << Derived_Type_Blk) | 
				(ONE << Enum_Blk)),

			/*****  If_Stmt	 *****/

			       ((ONE << Unknown_Blk) |
				(ONE << Blockdata_Blk) |
				(ONE << Module_Blk) |
				(ONE << Forall_Blk) |
				(ONE << If_Blk) |
				(ONE << Where_Then_Blk) |
				(ONE << Where_Else_Blk) |
				(ONE << Where_Else_Mask_Blk) |
				(ONE << SGI_Psection_Blk) |
				(ONE << Select_Blk) |
				(ONE << Interface_Body_Blk) |
				(ONE << Contains_Blk) |
				(ONE << Interface_Blk) |
				(ONE << Derived_Type_Blk) | 
				(ONE << Enum_Blk)),

			/*****  Inquire_Stmt  *****/

			       ((ONE << Unknown_Blk) |
				(ONE << Blockdata_Blk) |
				(ONE << Module_Blk) |
				(ONE << Interface_Body_Blk) |
				(ONE << Forall_Blk) |
				(ONE << If_Blk) |
				(ONE << Where_Then_Blk) |
				(ONE << Where_Else_Blk) |
				(ONE << Where_Else_Mask_Blk) |
				(ONE << SGI_Psection_Blk) |
				(ONE << Select_Blk) |
				(ONE << Contains_Blk) |
				(ONE << Interface_Blk) |
				(ONE << Derived_Type_Blk) | 
				(ONE << Enum_Blk)),

			/*****  Nullify_Stmt  *****/

			       ((ONE << Unknown_Blk) |
				(ONE << Blockdata_Blk) |
				(ONE << Module_Blk) |
				(ONE << Interface_Body_Blk) |
				(ONE << Forall_Blk) |
				(ONE << If_Blk) |
				(ONE << Where_Then_Blk) |
				(ONE << Where_Else_Blk) |
				(ONE << Where_Else_Mask_Blk) |
				(ONE << SGI_Psection_Blk) |
				(ONE << Select_Blk) |
				(ONE << Contains_Blk) |
				(ONE << Interface_Blk) |
				(ONE << Derived_Type_Blk) | 
				(ONE << Enum_Blk)),

			/*****  Open_Stmt  *****/

			       ((ONE << Unknown_Blk) |
				(ONE << Blockdata_Blk) |
				(ONE << Module_Blk) |
				(ONE << Interface_Body_Blk) |
				(ONE << Forall_Blk) |
				(ONE << If_Blk) |
				(ONE << Where_Then_Blk) |
				(ONE << Where_Else_Blk) |
				(ONE << Where_Else_Mask_Blk) |
				(ONE << SGI_Psection_Blk) |
				(ONE << Select_Blk) |
				(ONE << Contains_Blk) |
				(ONE << Interface_Blk) |
				(ONE << Derived_Type_Blk) | 
				(ONE << Enum_Blk)),

			/*****  Outmoded_If_Stmt  *****/

			       ((ONE << Unknown_Blk) |
				(ONE << Blockdata_Blk) |
				(ONE << Module_Blk) |
				(ONE << Interface_Body_Blk) |
				(ONE << Forall_Blk) |
				(ONE << If_Blk) |
				(ONE << Where_Then_Blk) |
				(ONE << Where_Else_Blk) |
				(ONE << Where_Else_Mask_Blk) |
				(ONE << SGI_Psection_Blk) |
				(ONE << Select_Blk) |
				(ONE << Contains_Blk) |
				(ONE << Interface_Blk) |
				(ONE << Derived_Type_Blk) | 
				(ONE << Enum_Blk)),

			/*****  Pause_Stmt  *****/

			       ((ONE << Unknown_Blk) |
				(ONE << Blockdata_Blk) |
				(ONE << Module_Blk) |
				(ONE << Interface_Body_Blk) |
				(ONE << Forall_Blk) |
				(ONE << If_Blk) |
				(ONE << Where_Then_Blk) |
				(ONE << Where_Else_Blk) |
				(ONE << Where_Else_Mask_Blk) |
				(ONE << SGI_Psection_Blk) |
				(ONE << Select_Blk) |
				(ONE << Contains_Blk) |
				(ONE << Interface_Blk) |
				(ONE << Derived_Type_Blk) | 
				(ONE << Enum_Blk)),

			/*****  Print_Stmt  *****/

			       ((ONE << Unknown_Blk) |
				(ONE << Blockdata_Blk) |
				(ONE << Module_Blk) |
				(ONE << Interface_Body_Blk) |
				(ONE << Forall_Blk) |
				(ONE << If_Blk) |
				(ONE << Where_Then_Blk) |
				(ONE << Where_Else_Blk) |
				(ONE << Where_Else_Mask_Blk) |
				(ONE << SGI_Psection_Blk) |
				(ONE << Select_Blk) |
				(ONE << Contains_Blk) |
				(ONE << Interface_Blk) |
				(ONE << Derived_Type_Blk) | 
				(ONE << Enum_Blk)),

			/*****  Read_Stmt  *****/

			       ((ONE << Unknown_Blk) |
				(ONE << Blockdata_Blk) |
				(ONE << Module_Blk) |
				(ONE << Interface_Body_Blk) |
				(ONE << Forall_Blk) |
				(ONE << If_Blk) |
				(ONE << Where_Then_Blk) |
				(ONE << Where_Else_Blk) |
				(ONE << Where_Else_Mask_Blk) |
				(ONE << SGI_Psection_Blk) |
				(ONE << Select_Blk) |
				(ONE << Contains_Blk) |
				(ONE << Interface_Blk) |
				(ONE << Derived_Type_Blk) | 
				(ONE << Enum_Blk)),

			/*****  Return_Stmt  *****/

			       ((ONE << Unknown_Blk) |
				(ONE << Blockdata_Blk) |
				(ONE << Module_Blk) |
				(ONE << Program_Blk) |
				(ONE << Interface_Body_Blk) |
				(ONE << Forall_Blk) |
				(ONE << If_Blk) |
				(ONE << Where_Then_Blk) |
				(ONE << Where_Else_Blk) |
				(ONE << Where_Else_Mask_Blk) |
				(ONE << SGI_Psection_Blk) |
				(ONE << Select_Blk) |
				(ONE << Contains_Blk) |
				(ONE << Interface_Blk) |
				(ONE << Derived_Type_Blk) | 
				(ONE << Enum_Blk)),

			/*****  Rewind_Stmt  *****/

			       ((ONE << Unknown_Blk) |
				(ONE << Blockdata_Blk) |
				(ONE << Module_Blk) |
				(ONE << Interface_Body_Blk) |
				(ONE << Forall_Blk) |
				(ONE << If_Blk) |
				(ONE << Where_Then_Blk) |
				(ONE << Where_Else_Blk) |
				(ONE << Where_Else_Mask_Blk) |
				(ONE << SGI_Psection_Blk) |
				(ONE << Select_Blk) |
				(ONE << Contains_Blk) |
				(ONE << Interface_Blk) |
				(ONE << Derived_Type_Blk) | 
				(ONE << Enum_Blk)),

			/*****  Select_Stmt  *****/

			       ((ONE << Unknown_Blk) |
				(ONE << Blockdata_Blk) |
				(ONE << Module_Blk) |
				(ONE << Interface_Body_Blk) |
				(ONE << Forall_Blk) |
				(ONE << If_Blk) |
				(ONE << Where_Then_Blk) |
				(ONE << Where_Else_Blk) |
				(ONE << Where_Else_Mask_Blk) |
				(ONE << SGI_Psection_Blk) |
				(ONE << Select_Blk) |
				(ONE << Contains_Blk) |
				(ONE << Interface_Blk) |
				(ONE << Derived_Type_Blk) | 
				(ONE << Enum_Blk)),

			/*****  Stop_Stmt  *****/

			       ((ONE << Unknown_Blk) |
				(ONE << Blockdata_Blk) |
				(ONE << Module_Blk) |
				(ONE << Interface_Body_Blk) |
				(ONE << Forall_Blk) |
				(ONE << If_Blk) |
				(ONE << Where_Then_Blk) |
				(ONE << Where_Else_Blk) |
				(ONE << Where_Else_Mask_Blk) |
				(ONE << SGI_Psection_Blk) |
				(ONE << Select_Blk) |
				(ONE << Contains_Blk) |
				(ONE << Interface_Blk) |
				(ONE << Derived_Type_Blk) | 
				(ONE << Enum_Blk)),

			/*****  Then_Stmt  *****/

			       ((ONE << Unknown_Blk) |
				(ONE << Blockdata_Blk) |
				(ONE << Module_Blk) |
				(ONE << Interface_Body_Blk) |
				(ONE << Forall_Blk) |
				(ONE << If_Else_Blk) |
				(ONE << If_Then_Blk) |
				(ONE << If_Else_If_Blk) |
				(ONE << Where_Then_Blk) |
				(ONE << Where_Else_Blk) |
				(ONE << Where_Else_Mask_Blk) |
				(ONE << SGI_Psection_Blk) |
				(ONE << Select_Blk) |
				(ONE << Contains_Blk) |
				(ONE << Interface_Blk) |
				(ONE << Derived_Type_Blk) | 
				(ONE << Enum_Blk)),

			/*****  Where_Cstrct_Stmt  *****/

			       ((ONE << Unknown_Blk) |
				(ONE << Blockdata_Blk) |
				(ONE << Module_Blk) |
				(ONE << Interface_Body_Blk) |
				(ONE << If_Blk) |
				(ONE << SGI_Psection_Blk) |
				(ONE << Select_Blk) |
				(ONE << Contains_Blk) |
				(ONE << Interface_Blk) |
				(ONE << Derived_Type_Blk) | 
				(ONE << Enum_Blk)),

			/*****  Where_Stmt  *****/

			       ((ONE << Unknown_Blk) |
				(ONE << Blockdata_Blk) |
				(ONE << Module_Blk) |
				(ONE << Interface_Body_Blk) |
				(ONE << If_Blk) |
				(ONE << SGI_Psection_Blk) |
				(ONE << Select_Blk) |
				(ONE << Contains_Blk) |
				(ONE << Interface_Blk) |
				(ONE << Derived_Type_Blk) | 
				(ONE << Enum_Blk)),

			/*****  Write_Stmt  *****/

			       ((ONE << Unknown_Blk) |
				(ONE << Blockdata_Blk) |
				(ONE << Module_Blk) |
				(ONE << Interface_Body_Blk) |
				(ONE << Forall_Blk) |
				(ONE << If_Blk) |
				(ONE << Where_Then_Blk) |
				(ONE << Where_Else_Blk) |
				(ONE << Where_Else_Mask_Blk) |
				(ONE << SGI_Psection_Blk) |
				(ONE << Select_Blk) |
				(ONE << Contains_Blk) |
				(ONE << Interface_Blk) |
				(ONE << Derived_Type_Blk) | 
				(ONE << Enum_Blk)),

			/*****  Type_Init_Stmt  *****/

			       (0), 

			/*****  Label_Def  *****/

                               (0), 

			/*****  Construct_Def  *****/

                               (0), 

			/*****  Automatic_Base_Calc_Stmt  *****/

                               (0), 

			/*****  Automatic_Base_Size_Stmt  *****/ 

                               (0), 
						
			/*****  End_Parallel_Stmt  *****/

                               ((ONE << Unknown_Blk) |
                                (ONE << Blockdata_Blk) |
                                (ONE << Module_Blk) |
                                (ONE << Program_Blk) |
                                (ONE << Function_Blk) |
                                (ONE << Subroutine_Blk) |
                                (ONE << Internal_Blk) |
                                (ONE << Module_Proc_Blk) |
                                (ONE << Interface_Body_Blk) |
				(ONE << Forall_Blk) |
                                (ONE << If_Blk) |
                                (ONE << If_Then_Blk) |
                                (ONE << If_Else_If_Blk) |
                                (ONE << If_Else_Blk) |
                                (ONE << Do_Blk) |
                                (ONE << Select_Blk) |
                                (ONE << Case_Blk) |
                                (ONE << Where_Then_Blk) |
                                (ONE << Where_Else_Blk) |
				(ONE << Where_Else_Mask_Blk) |
                                (ONE << Doall_Blk) |
				(ONE << Wait_Blk) |
                                (ONE << SGI_Parallel_Blk) |
				(ONE << SGI_Doacross_Blk) |
				(ONE << SGI_Parallel_Do_Blk) |
                                (ONE << Do_Parallel_Blk) |
				(ONE << SGI_Pdo_Blk) |
                                (ONE << Guard_Blk) |
				(ONE << SGI_Critical_Section_Blk) |
                                (ONE << Parallel_Case_Blk) |
				(ONE << SGI_Psection_Blk) |
				(ONE << SGI_Section_Blk) |
				(ONE << SGI_Single_Process_Blk) |
				(ONE << SGI_Region_Blk) |
                                (ONE << Open_Mp_Parallel_Blk) |
                                (ONE << Open_Mp_Do_Blk) |
                                (ONE << Open_Mp_Parallel_Sections_Blk) |
                                (ONE << Open_Mp_Sections_Blk) |
                                (ONE << Open_Mp_Section_Blk) |
                                (ONE << Open_Mp_Single_Blk) |
                                (ONE << Open_Mp_Parallel_Do_Blk) |
                                (ONE << Open_Mp_Master_Blk) |
                                (ONE << Open_Mp_Critical_Blk) |
                                (ONE << Open_Mp_Ordered_Blk) |
                                (ONE << Open_Mp_Workshare_Blk) | /* by jhs, 02/7/18 */
                                (ONE << Open_Mp_Parallel_Workshare_Blk) | /* by jhs, 02/7/18 */
                                (ONE << Contains_Blk) |
                                (ONE << Interface_Blk) |
				(ONE << Derived_Type_Blk) |
				(ONE << Enum_Blk)),

			/*****  End_Do_Parallel_Stmt  *****/

                               ((ONE << Unknown_Blk) |
                                (ONE << Blockdata_Blk) |
                                (ONE << Module_Blk) |
                                (ONE << Program_Blk) |
                                (ONE << Function_Blk) |
                                (ONE << Subroutine_Blk) |
                                (ONE << Internal_Blk) |
                                (ONE << Module_Proc_Blk) |
                                (ONE << Interface_Body_Blk) |
				(ONE << Forall_Blk) |
                                (ONE << If_Blk) |
                                (ONE << If_Then_Blk) |
                                (ONE << If_Else_If_Blk) |
                                (ONE << If_Else_Blk) |
                                (ONE << Do_Blk) |
                                (ONE << Select_Blk) |
                                (ONE << Case_Blk) |
                                (ONE << Where_Then_Blk) |
                                (ONE << Where_Else_Blk) |
				(ONE << Where_Else_Mask_Blk) |
                                (ONE << Parallel_Blk) |
				(ONE << SGI_Parallel_Blk) |
                                (ONE << Doall_Blk) |
				(ONE << Wait_Blk) |
				(ONE << SGI_Doacross_Blk) |
				(ONE << SGI_Parallel_Do_Blk) |
				(ONE << SGI_Pdo_Blk) |
                                (ONE << Guard_Blk) |
				(ONE << SGI_Critical_Section_Blk) |
                                (ONE << Parallel_Case_Blk) |
				(ONE << SGI_Psection_Blk) |
				(ONE << SGI_Section_Blk) |
				(ONE << SGI_Single_Process_Blk) |
				(ONE << SGI_Region_Blk) |
                                (ONE << Open_Mp_Parallel_Blk) |
                                (ONE << Open_Mp_Do_Blk) |
                                (ONE << Open_Mp_Parallel_Sections_Blk) |
                                (ONE << Open_Mp_Sections_Blk) |
                                (ONE << Open_Mp_Section_Blk) |
                                (ONE << Open_Mp_Single_Blk) |
                                (ONE << Open_Mp_Parallel_Do_Blk) |
                                (ONE << Open_Mp_Master_Blk) |
                                (ONE << Open_Mp_Critical_Blk) |
                                (ONE << Open_Mp_Ordered_Blk) |
                                (ONE << Open_Mp_Workshare_Blk) | /* by jhs, 02/7/18 */
                                (ONE << Open_Mp_Parallel_Workshare_Blk) | /* by jhs, 02/7/18 */
                                (ONE << Contains_Blk) |
                                (ONE << Interface_Blk) |
                                (ONE << Derived_Type_Blk) |
				(ONE << Enum_Blk)),

			/*****  End_Parallel_Case_Stmt	*****/

                               ((ONE << Unknown_Blk) |
                                (ONE << Blockdata_Blk) |
                                (ONE << Module_Blk) |
                                (ONE << Program_Blk) |
                                (ONE << Function_Blk) |
                                (ONE << Subroutine_Blk) |
                                (ONE << Internal_Blk) |
                                (ONE << Module_Proc_Blk) |
                                (ONE << Interface_Body_Blk) |
				(ONE << Forall_Blk) |
                                (ONE << If_Blk) |
                                (ONE << If_Then_Blk) |
                                (ONE << If_Else_If_Blk) |
                                (ONE << If_Else_Blk) |
                                (ONE << Do_Blk) |
                                (ONE << Select_Blk) |
                                (ONE << Case_Blk) |
                                (ONE << Where_Then_Blk) |
                                (ONE << Where_Else_Blk) |
				(ONE << Where_Else_Mask_Blk) |
                                (ONE << Parallel_Blk) |
				(ONE << SGI_Parallel_Blk) |
                                (ONE << Doall_Blk) |
				(ONE << Wait_Blk) |
				(ONE << SGI_Doacross_Blk) |
				(ONE << SGI_Parallel_Do_Blk) |
                                (ONE << Do_Parallel_Blk) |
				(ONE << SGI_Pdo_Blk) |
                                (ONE << Guard_Blk) |
				(ONE << SGI_Critical_Section_Blk) |
				(ONE << SGI_Psection_Blk) |
				(ONE << SGI_Section_Blk) |
				(ONE << SGI_Single_Process_Blk) |
				(ONE << SGI_Region_Blk) |
                                (ONE << Open_Mp_Parallel_Blk) |
                                (ONE << Open_Mp_Do_Blk) |
                                (ONE << Open_Mp_Parallel_Sections_Blk) |
                                (ONE << Open_Mp_Sections_Blk) |
                                (ONE << Open_Mp_Section_Blk) |
                                (ONE << Open_Mp_Single_Blk) |
                                (ONE << Open_Mp_Parallel_Do_Blk) |
                                (ONE << Open_Mp_Master_Blk) |
                                (ONE << Open_Mp_Critical_Blk) |
                                (ONE << Open_Mp_Ordered_Blk) |
                                (ONE << Open_Mp_Workshare_Blk) | /* by jhs, 02/7/18 */
                                (ONE << Open_Mp_Parallel_Workshare_Blk) | /* by jhs, 02/7/18 */
                                (ONE << Contains_Blk) |
                                (ONE << Interface_Blk) |
                                (ONE << Derived_Type_Blk) | 
				(ONE << Enum_Blk)),

			/*****  Parallel_Case_Stmt  *****/

                               ((ONE << Unknown_Blk) |
                                (ONE << Blockdata_Blk) |
                                (ONE << Module_Blk) |
                                (ONE << Program_Blk) |
                                (ONE << Function_Blk) |
                                (ONE << Subroutine_Blk) |
                                (ONE << Internal_Blk) |
                                (ONE << Module_Proc_Blk) |
                                (ONE << Interface_Body_Blk) |
				(ONE << Forall_Blk) |
                                (ONE << If_Blk) |
                                (ONE << If_Then_Blk) |
                                (ONE << If_Else_If_Blk) |
                                (ONE << If_Else_Blk) |
                                (ONE << Do_Blk) |
                                (ONE << Select_Blk) |
                                (ONE << Case_Blk) |
                                (ONE << Where_Then_Blk) |
                                (ONE << Where_Else_Blk) |
				(ONE << Where_Else_Mask_Blk) |
                                (ONE << Parallel_Blk) |
				(ONE << SGI_Parallel_Blk) |
                                (ONE << Doall_Blk) |
				(ONE << Wait_Blk) |
				(ONE << SGI_Doacross_Blk) |
				(ONE << SGI_Parallel_Do_Blk) |
                                (ONE << Do_Parallel_Blk) |
				(ONE << SGI_Pdo_Blk) |
                                (ONE << Guard_Blk) |
				(ONE << SGI_Critical_Section_Blk) |
				(ONE << SGI_Psection_Blk) |
				(ONE << SGI_Section_Blk) |
				(ONE << SGI_Single_Process_Blk) |
				(ONE << SGI_Region_Blk) |
                                (ONE << Open_Mp_Parallel_Blk) |
                                (ONE << Open_Mp_Do_Blk) |
                                (ONE << Open_Mp_Parallel_Sections_Blk) |
                                (ONE << Open_Mp_Sections_Blk) |
                                (ONE << Open_Mp_Section_Blk) |
                                (ONE << Open_Mp_Single_Blk) |
                                (ONE << Open_Mp_Parallel_Do_Blk) |
                                (ONE << Open_Mp_Master_Blk) |
                                (ONE << Open_Mp_Critical_Blk) |
                                (ONE << Open_Mp_Ordered_Blk) |
                                (ONE << Open_Mp_Workshare_Blk) | /* by jhs, 02/7/18 */
                                (ONE << Open_Mp_Parallel_Workshare_Blk) | /* by jhs, 02/7/18 */
                                (ONE << Contains_Blk) |
                                (ONE << Interface_Blk) |
                                (ONE << Derived_Type_Blk) |
				(ONE << Enum_Blk)),

			/*****  End_Guard_Stmt	*****/

                               ((ONE << Unknown_Blk) |
                                (ONE << Blockdata_Blk) |
                                (ONE << Module_Blk) |
                                (ONE << Program_Blk) |
                                (ONE << Function_Blk) |
                                (ONE << Subroutine_Blk) |
                                (ONE << Internal_Blk) |
                                (ONE << Module_Proc_Blk) |
                                (ONE << Interface_Body_Blk) |
				(ONE << Forall_Blk) |
                                (ONE << If_Blk) |
                                (ONE << If_Then_Blk) |
                                (ONE << If_Else_If_Blk) |
                                (ONE << If_Else_Blk) |
                                (ONE << Do_Blk) |
                                (ONE << Select_Blk) |
                                (ONE << Case_Blk) |
                                (ONE << Where_Then_Blk) |
                                (ONE << Where_Else_Blk) |
				(ONE << Where_Else_Mask_Blk) |
                                (ONE << Parallel_Blk) |
				(ONE << SGI_Parallel_Blk) |
                                (ONE << Doall_Blk) |
				(ONE << Wait_Blk) |
				(ONE << SGI_Doacross_Blk) |
				(ONE << SGI_Parallel_Do_Blk) |
                                (ONE << Do_Parallel_Blk) |
				(ONE << SGI_Pdo_Blk) |
                                (ONE << Parallel_Case_Blk) |
				(ONE << SGI_Critical_Section_Blk) |
				(ONE << SGI_Psection_Blk) |
				(ONE << SGI_Section_Blk) |
				(ONE << SGI_Single_Process_Blk) |
				(ONE << SGI_Region_Blk) |
                                (ONE << Open_Mp_Parallel_Blk) |
                                (ONE << Open_Mp_Do_Blk) |
                                (ONE << Open_Mp_Parallel_Sections_Blk) |
                                (ONE << Open_Mp_Sections_Blk) |
                                (ONE << Open_Mp_Section_Blk) |
                                (ONE << Open_Mp_Single_Blk) |
                                (ONE << Open_Mp_Parallel_Do_Blk) |
                                (ONE << Open_Mp_Master_Blk) |
                                (ONE << Open_Mp_Critical_Blk) |
                                (ONE << Open_Mp_Ordered_Blk) |
                                (ONE << Open_Mp_Workshare_Blk) | /* by jhs, 02/7/18 */
                                (ONE << Open_Mp_Parallel_Workshare_Blk) | /* by jhs, 02/7/18 */
                                (ONE << Contains_Blk) |
                                (ONE << Interface_Blk) |
                                (ONE << Derived_Type_Blk) |
				(ONE << Enum_Blk)),

			/*****  Statement_Num_Stmt  *****/ 

                               (0),

			/*****  SGI_Section_Stmt  *****/

			       ((ONE << Unknown_Blk) |
                                (ONE << Blockdata_Blk) |
                                (ONE << Module_Blk) |
                                (ONE << Program_Blk) |
                                (ONE << Function_Blk) |
                                (ONE << Subroutine_Blk) |
                                (ONE << Internal_Blk) |
                                (ONE << Module_Proc_Blk) |
                                (ONE << Interface_Body_Blk) |
				(ONE << Forall_Blk) |
                                (ONE << If_Blk) |
                                (ONE << If_Then_Blk) |
                                (ONE << If_Else_If_Blk) |
                                (ONE << If_Else_Blk) |
                                (ONE << Do_Blk) |
                                (ONE << Select_Blk) |
                                (ONE << Case_Blk) |
                                (ONE << Where_Then_Blk) |
                                (ONE << Where_Else_Blk) |
				(ONE << Where_Else_Mask_Blk) |
                                (ONE << Parallel_Blk) |
                                (ONE << SGI_Parallel_Blk) |
                                (ONE << Doall_Blk) |
				(ONE << Wait_Blk) |
                                (ONE << SGI_Doacross_Blk) |
                                (ONE << SGI_Parallel_Do_Blk) |
                                (ONE << Do_Parallel_Blk) |
                                (ONE << SGI_Pdo_Blk) |
                                (ONE << Guard_Blk) |
                                (ONE << SGI_Critical_Section_Blk) |
                                (ONE << Parallel_Case_Blk) |
                                (ONE << SGI_Single_Process_Blk) |
                                (ONE << SGI_Region_Blk) |
                                (ONE << Open_Mp_Parallel_Blk) |
                                (ONE << Open_Mp_Do_Blk) |
                                (ONE << Open_Mp_Parallel_Sections_Blk) |
                                (ONE << Open_Mp_Sections_Blk) |
                                (ONE << Open_Mp_Section_Blk) |
                                (ONE << Open_Mp_Single_Blk) |
                                (ONE << Open_Mp_Parallel_Do_Blk) |
                                (ONE << Open_Mp_Master_Blk) |
                                (ONE << Open_Mp_Critical_Blk) |
                                (ONE << Open_Mp_Ordered_Blk) |
                                (ONE << Open_Mp_Workshare_Blk) | /* by jhs, 02/7/18 */
                                (ONE << Open_Mp_Parallel_Workshare_Blk) | /* by jhs, 02/7/18 */
                                (ONE << Contains_Blk) |
                                (ONE << Interface_Blk) |
                                (ONE << Derived_Type_Blk) |
				(ONE << Enum_Blk)),

			/*****  SGI_End_Psection_Stmt  *****/

                               ((ONE << Unknown_Blk) |
                                (ONE << Blockdata_Blk) |
                                (ONE << Module_Blk) |
                                (ONE << Program_Blk) |
                                (ONE << Function_Blk) |
                                (ONE << Subroutine_Blk) |
                                (ONE << Internal_Blk) |
                                (ONE << Module_Proc_Blk) |
                                (ONE << Interface_Body_Blk) |
				(ONE << Forall_Blk) |
                                (ONE << If_Blk) |
                                (ONE << If_Then_Blk) |
                                (ONE << If_Else_If_Blk) |
                                (ONE << If_Else_Blk) |
                                (ONE << Do_Blk) |
                                (ONE << Select_Blk) |
                                (ONE << Case_Blk) |
                                (ONE << Where_Then_Blk) |
                                (ONE << Where_Else_Blk) |
				(ONE << Where_Else_Mask_Blk) |
                                (ONE << Parallel_Blk) |
                                (ONE << SGI_Parallel_Blk) |
                                (ONE << Doall_Blk) |
				(ONE << Wait_Blk) |
                                (ONE << SGI_Doacross_Blk) |
                                (ONE << SGI_Parallel_Do_Blk) |
                                (ONE << Do_Parallel_Blk) |
                                (ONE << SGI_Pdo_Blk) |
                                (ONE << Guard_Blk) |
                                (ONE << SGI_Critical_Section_Blk) |
                                (ONE << Parallel_Case_Blk) |
                                (ONE << SGI_Single_Process_Blk) |
                                (ONE << SGI_Region_Blk) |
                                (ONE << Open_Mp_Parallel_Blk) |
                                (ONE << Open_Mp_Do_Blk) |
                                (ONE << Open_Mp_Parallel_Sections_Blk) |
                                (ONE << Open_Mp_Sections_Blk) |
                                (ONE << Open_Mp_Section_Blk) |
                                (ONE << Open_Mp_Single_Blk) |
                                (ONE << Open_Mp_Parallel_Do_Blk) |
                                (ONE << Open_Mp_Master_Blk) |
                                (ONE << Open_Mp_Critical_Blk) |
                                (ONE << Open_Mp_Ordered_Blk) |
                                (ONE << Open_Mp_Workshare_Blk) | /* by jhs, 02/7/18 */
                                (ONE << Open_Mp_Parallel_Workshare_Blk) | /* by jhs, 02/7/18 */
                                (ONE << Contains_Blk) |
                                (ONE << Interface_Blk) |
                                (ONE << Derived_Type_Blk) |
				(ONE << Enum_Blk)),

			/*****  SGI_End_Pdo_Stmt   *****/

                               ((ONE << Unknown_Blk) |
                                (ONE << Blockdata_Blk) |
                                (ONE << Module_Blk) |
                                (ONE << Program_Blk) |
                                (ONE << Function_Blk) |
                                (ONE << Subroutine_Blk) |
                                (ONE << Internal_Blk) |
                                (ONE << Module_Proc_Blk) |
                                (ONE << Interface_Body_Blk) |
				(ONE << Forall_Blk) |
                                (ONE << If_Blk) |
                                (ONE << If_Then_Blk) |
                                (ONE << If_Else_If_Blk) |
                                (ONE << If_Else_Blk) |
                                (ONE << Do_Blk) |
                                (ONE << Select_Blk) |
                                (ONE << Case_Blk) |
                                (ONE << Where_Then_Blk) |
                                (ONE << Where_Else_Blk) |
				(ONE << Where_Else_Mask_Blk) |
                                (ONE << Parallel_Blk) |
                                (ONE << SGI_Parallel_Blk) |
                                (ONE << Doall_Blk) |
				(ONE << Wait_Blk) |
                                (ONE << SGI_Doacross_Blk) |
                                (ONE << SGI_Parallel_Do_Blk) |
                                (ONE << Do_Parallel_Blk) |
                                (ONE << Guard_Blk) |
                                (ONE << SGI_Critical_Section_Blk) |
                                (ONE << Parallel_Case_Blk) |
                                (ONE << SGI_Psection_Blk) |
                                (ONE << SGI_Section_Blk) |
                                (ONE << SGI_Single_Process_Blk) |
                                (ONE << SGI_Region_Blk) |
                                (ONE << Open_Mp_Parallel_Blk) |
                                (ONE << Open_Mp_Do_Blk) |
                                (ONE << Open_Mp_Parallel_Sections_Blk) |
                                (ONE << Open_Mp_Sections_Blk) |
                                (ONE << Open_Mp_Section_Blk) |
                                (ONE << Open_Mp_Single_Blk) |
                                (ONE << Open_Mp_Parallel_Do_Blk) |
                                (ONE << Open_Mp_Master_Blk) |
                                (ONE << Open_Mp_Critical_Blk) |
                                (ONE << Open_Mp_Ordered_Blk) |
                                (ONE << Open_Mp_Workshare_Blk) | /* by jhs, 02/7/18 */
                                (ONE << Open_Mp_Parallel_Workshare_Blk) | /* by jhs, 02/7/18 */
                                (ONE << Contains_Blk) |
                                (ONE << Interface_Blk) |
                                (ONE << Derived_Type_Blk) |
				(ONE << Enum_Blk)),

			/*****  SGI_End_Parallel_Stmt  *****/

                               ((ONE << Unknown_Blk) |
                                (ONE << Blockdata_Blk) |
                                (ONE << Module_Blk) |
                                (ONE << Program_Blk) |
                                (ONE << Function_Blk) |
                                (ONE << Subroutine_Blk) |
                                (ONE << Internal_Blk) |
                                (ONE << Module_Proc_Blk) |
                                (ONE << Interface_Body_Blk) |
				(ONE << Forall_Blk) |
                                (ONE << If_Blk) |
                                (ONE << If_Then_Blk) |
                                (ONE << If_Else_If_Blk) |
                                (ONE << If_Else_Blk) |
                                (ONE << Do_Blk) |
                                (ONE << Select_Blk) |
                                (ONE << Case_Blk) |
                                (ONE << Where_Then_Blk) |
                                (ONE << Where_Else_Blk) |
				(ONE << Where_Else_Mask_Blk) |
                                (ONE << Parallel_Blk) |
                                (ONE << Doall_Blk) |
				(ONE << Wait_Blk) |
                                (ONE << SGI_Doacross_Blk) |
                                (ONE << SGI_Parallel_Do_Blk) |
                                (ONE << Do_Parallel_Blk) |
                                (ONE << Guard_Blk) |
                                (ONE << Parallel_Case_Blk) |
                                (ONE << SGI_Region_Blk) |
                                (ONE << Open_Mp_Parallel_Blk) |
                                (ONE << Open_Mp_Do_Blk) |
                                (ONE << Open_Mp_Parallel_Sections_Blk) |
                                (ONE << Open_Mp_Sections_Blk) |
                                (ONE << Open_Mp_Section_Blk) |
                                (ONE << Open_Mp_Single_Blk) |
                                (ONE << Open_Mp_Parallel_Do_Blk) |
                                (ONE << Open_Mp_Master_Blk) |
                                (ONE << Open_Mp_Critical_Blk) |
                                (ONE << Open_Mp_Ordered_Blk) |
                                (ONE << Open_Mp_Workshare_Blk) | /* by jhs, 02/7/18 */
                                (ONE << Open_Mp_Parallel_Workshare_Blk) | /* by jhs, 02/7/18 */
                                (ONE << Contains_Blk) |
                                (ONE << Interface_Blk) |
                                (ONE << Derived_Type_Blk) |
				(ONE << Enum_Blk)),

			/*****  SGI_End_Critical_Section_Stmt  *****/

                               ((ONE << Unknown_Blk) |
                                (ONE << Blockdata_Blk) |
                                (ONE << Module_Blk) |
                                (ONE << Program_Blk) |
                                (ONE << Function_Blk) |
                                (ONE << Subroutine_Blk) |
                                (ONE << Internal_Blk) |
                                (ONE << Module_Proc_Blk) |
                                (ONE << Interface_Body_Blk) |
				(ONE << Forall_Blk) |
                                (ONE << If_Blk) |
                                (ONE << If_Then_Blk) |
                                (ONE << If_Else_If_Blk) |
                                (ONE << If_Else_Blk) |
                                (ONE << Do_Blk) |
                                (ONE << Select_Blk) |
                                (ONE << Case_Blk) |
                                (ONE << Where_Then_Blk) |
                                (ONE << Where_Else_Blk) |
				(ONE << Where_Else_Mask_Blk) |
                                (ONE << Parallel_Blk) |
                                (ONE << SGI_Parallel_Blk) |
                                (ONE << Doall_Blk) |
				(ONE << Wait_Blk) |
                                (ONE << SGI_Doacross_Blk) |
                                (ONE << SGI_Parallel_Do_Blk) |
                                (ONE << Do_Parallel_Blk) |
                                (ONE << SGI_Pdo_Blk) |
                                (ONE << Guard_Blk) |
                                (ONE << Parallel_Case_Blk) |
                                (ONE << SGI_Psection_Blk) |
                                (ONE << SGI_Section_Blk) |
                                (ONE << SGI_Single_Process_Blk) |
                                (ONE << SGI_Region_Blk) |
                                (ONE << Open_Mp_Parallel_Blk) |
                                (ONE << Open_Mp_Do_Blk) |
                                (ONE << Open_Mp_Parallel_Sections_Blk) |
                                (ONE << Open_Mp_Sections_Blk) |
                                (ONE << Open_Mp_Section_Blk) |
                                (ONE << Open_Mp_Single_Blk) |
                                (ONE << Open_Mp_Parallel_Do_Blk) |
                                (ONE << Open_Mp_Master_Blk) |
                                (ONE << Open_Mp_Critical_Blk) |
                                (ONE << Open_Mp_Ordered_Blk) |
                                (ONE << Open_Mp_Workshare_Blk) | /* by jhs, 02/7/18 */
                                (ONE << Open_Mp_Parallel_Workshare_Blk) | /* by jhs, 02/7/18 */
                                (ONE << Contains_Blk) |
                                (ONE << Interface_Blk) |
                                (ONE << Derived_Type_Blk) |
				(ONE << Enum_Blk)),

			/*****  SGI_End_Single_Process_Stmt  *****/

                               ((ONE << Unknown_Blk) |
                                (ONE << Blockdata_Blk) |
                                (ONE << Module_Blk) |
                                (ONE << Program_Blk) |
                                (ONE << Function_Blk) |
                                (ONE << Subroutine_Blk) |
                                (ONE << Internal_Blk) |
                                (ONE << Module_Proc_Blk) |
                                (ONE << Interface_Body_Blk) |
				(ONE << Forall_Blk) |
                                (ONE << If_Blk) |
                                (ONE << If_Then_Blk) |
                                (ONE << If_Else_If_Blk) |
                                (ONE << If_Else_Blk) |
                                (ONE << Do_Blk) |
                                (ONE << Select_Blk) |
                                (ONE << Case_Blk) |
                                (ONE << Where_Then_Blk) |
                                (ONE << Where_Else_Blk) |
				(ONE << Where_Else_Mask_Blk) |
                                (ONE << Parallel_Blk) |
                                (ONE << SGI_Parallel_Blk) |
                                (ONE << Doall_Blk) |
				(ONE << Wait_Blk) |
                                (ONE << SGI_Doacross_Blk) |
                                (ONE << SGI_Parallel_Do_Blk) |
                                (ONE << Do_Parallel_Blk) |
                                (ONE << SGI_Pdo_Blk) |
                                (ONE << Guard_Blk) |
                                (ONE << SGI_Critical_Section_Blk) |
                                (ONE << Parallel_Case_Blk) |
                                (ONE << SGI_Psection_Blk) |
                                (ONE << SGI_Section_Blk) |
                                (ONE << SGI_Region_Blk) |
                                (ONE << Open_Mp_Parallel_Blk) |
                                (ONE << Open_Mp_Do_Blk) |
                                (ONE << Open_Mp_Parallel_Sections_Blk) |
                                (ONE << Open_Mp_Sections_Blk) |
                                (ONE << Open_Mp_Section_Blk) |
                                (ONE << Open_Mp_Single_Blk) |
                                (ONE << Open_Mp_Parallel_Do_Blk) |
                                (ONE << Open_Mp_Master_Blk) |
                                (ONE << Open_Mp_Critical_Blk) |
                                (ONE << Open_Mp_Ordered_Blk) |
                                (ONE << Open_Mp_Workshare_Blk) | /* by jhs, 02/7/18 */
                                (ONE << Open_Mp_Parallel_Workshare_Blk) | /* by jhs, 02/7/18 */
                                (ONE << Contains_Blk) |
                                (ONE << Interface_Blk) |
                                (ONE << Derived_Type_Blk) |
				(ONE << Enum_Blk)),

			/*****  SGI_Region_End_Stmt  *****/

                               ((ONE << Unknown_Blk) |
                                (ONE << Blockdata_Blk) |
                                (ONE << Module_Blk) |
                                (ONE << Program_Blk) |
                                (ONE << Function_Blk) |
                                (ONE << Subroutine_Blk) |
                                (ONE << Internal_Blk) |
                                (ONE << Module_Proc_Blk) |
                                (ONE << Interface_Body_Blk) |
				(ONE << Forall_Blk) |
                                (ONE << If_Blk) |
                                (ONE << If_Then_Blk) |
                                (ONE << If_Else_If_Blk) |
                                (ONE << If_Else_Blk) |
                                (ONE << Do_Blk) |
                                (ONE << Select_Blk) |
                                (ONE << Case_Blk) |
                                (ONE << Where_Then_Blk) |
                                (ONE << Where_Else_Blk) |
				(ONE << Where_Else_Mask_Blk) |
                                (ONE << Parallel_Blk) |
                                (ONE << SGI_Parallel_Blk) |
                                (ONE << Doall_Blk) |
				(ONE << Wait_Blk) |
                                (ONE << SGI_Doacross_Blk) |
                                (ONE << SGI_Parallel_Do_Blk) |
                                (ONE << Do_Parallel_Blk) |
                                (ONE << SGI_Pdo_Blk) |
                                (ONE << Guard_Blk) |
                                (ONE << SGI_Critical_Section_Blk) |
                                (ONE << Parallel_Case_Blk) |
                                (ONE << SGI_Psection_Blk) |
                                (ONE << SGI_Section_Blk) |
                                (ONE << SGI_Single_Process_Blk) |
                                (ONE << Open_Mp_Parallel_Blk) |
                                (ONE << Open_Mp_Do_Blk) |
                                (ONE << Open_Mp_Parallel_Sections_Blk) |
                                (ONE << Open_Mp_Sections_Blk) |
                                (ONE << Open_Mp_Section_Blk) |
                                (ONE << Open_Mp_Single_Blk) |
                                (ONE << Open_Mp_Parallel_Do_Blk) |
                                (ONE << Open_Mp_Master_Blk) |
                                (ONE << Open_Mp_Critical_Blk) |
                                (ONE << Open_Mp_Ordered_Blk) |
                                (ONE << Open_Mp_Workshare_Blk) | /* by jhs, 02/7/18 */
                                (ONE << Open_Mp_Parallel_Workshare_Blk) | /* by jhs, 02/7/18 */
                                (ONE << Contains_Blk) |
                                (ONE << Interface_Blk) |
                                (ONE << Derived_Type_Blk) |
				(ONE << Enum_Blk)),

			/*****  Open_MP_Section_Stmt  *****/

                               ((ONE << Unknown_Blk) |
                                (ONE << Blockdata_Blk) |
                                (ONE << Module_Blk) |
                                (ONE << Program_Blk) |
                                (ONE << Function_Blk) |
                                (ONE << Subroutine_Blk) |
                                (ONE << Internal_Blk) |
                                (ONE << Module_Proc_Blk) |
                                (ONE << Interface_Body_Blk) |
				(ONE << Forall_Blk) |
                                (ONE << If_Blk) |
                                (ONE << If_Then_Blk) |
                                (ONE << If_Else_If_Blk) |
                                (ONE << If_Else_Blk) |
                                (ONE << Do_Blk) |
                                (ONE << Select_Blk) |
                                (ONE << Case_Blk) |
                                (ONE << Where_Then_Blk) |
                                (ONE << Where_Else_Blk) |
				(ONE << Where_Else_Mask_Blk) |
                                (ONE << Parallel_Blk) |
                                (ONE << SGI_Parallel_Blk) |
                                (ONE << Doall_Blk) |
				(ONE << Wait_Blk) |
                                (ONE << SGI_Doacross_Blk) |
                                (ONE << SGI_Parallel_Do_Blk) |
                                (ONE << Do_Parallel_Blk) |
                                (ONE << SGI_Pdo_Blk) |
                                (ONE << Guard_Blk) |
                                (ONE << SGI_Critical_Section_Blk) |
                                (ONE << Parallel_Case_Blk) |
                                (ONE << SGI_Psection_Blk) |
                                (ONE << SGI_Section_Blk) |
                                (ONE << SGI_Single_Process_Blk) |
                                (ONE << SGI_Region_Blk) |
                                (ONE << Open_Mp_Parallel_Blk) |
                                (ONE << Open_Mp_Do_Blk) |
                                (ONE << Open_Mp_Single_Blk) |
                                (ONE << Open_Mp_Parallel_Do_Blk) |
                                (ONE << Open_Mp_Master_Blk) |
                                (ONE << Open_Mp_Critical_Blk) |
                                (ONE << Open_Mp_Ordered_Blk) |
                                (ONE << Open_Mp_Workshare_Blk) | /* by jhs, 02/7/18 */
                                (ONE << Open_Mp_Parallel_Workshare_Blk) | /* by jhs, 02/7/18 */
                                (ONE << Contains_Blk) |
                                (ONE << Interface_Blk) |
                                (ONE << Derived_Type_Blk) |
				(ONE << Enum_Blk)),

			/*****  Open_MP_End_Parallel_Stmt  *****/

                               ((ONE << Unknown_Blk) |
                                (ONE << Blockdata_Blk) |
                                (ONE << Module_Blk) |
                                (ONE << Program_Blk) |
                                (ONE << Function_Blk) |
                                (ONE << Subroutine_Blk) |
                                (ONE << Internal_Blk) |
                                (ONE << Module_Proc_Blk) |
                                (ONE << Interface_Body_Blk) |
				(ONE << Forall_Blk) |
                                (ONE << If_Blk) |
                                (ONE << If_Then_Blk) |
                                (ONE << If_Else_If_Blk) |
                                (ONE << If_Else_Blk) |
                                (ONE << Do_Blk) |
                                (ONE << Select_Blk) |
                                (ONE << Case_Blk) |
                                (ONE << Where_Then_Blk) |
                                (ONE << Where_Else_Blk) |
				(ONE << Where_Else_Mask_Blk) |
                                (ONE << Parallel_Blk) |
                                (ONE << SGI_Parallel_Blk) |
                                (ONE << Doall_Blk) |
				(ONE << Wait_Blk) |
                                (ONE << SGI_Doacross_Blk) |
                                (ONE << SGI_Parallel_Do_Blk) |
                                (ONE << Do_Parallel_Blk) |
                                (ONE << SGI_Pdo_Blk) |
                                (ONE << Guard_Blk) |
                                (ONE << SGI_Critical_Section_Blk) |
                                (ONE << Parallel_Case_Blk) |
                                (ONE << SGI_Psection_Blk) |
                                (ONE << SGI_Section_Blk) |
                                (ONE << SGI_Single_Process_Blk) |
                                (ONE << SGI_Region_Blk) |
                                (ONE << Open_Mp_Do_Blk) |
                                (ONE << Open_Mp_Parallel_Sections_Blk) |
                                (ONE << Open_Mp_Sections_Blk) |
                                (ONE << Open_Mp_Section_Blk) |
                                (ONE << Open_Mp_Single_Blk) |
                                (ONE << Open_Mp_Parallel_Do_Blk) |
                                (ONE << Open_Mp_Master_Blk) |
                                (ONE << Open_Mp_Critical_Blk) |
                                (ONE << Open_Mp_Ordered_Blk) |
                                (ONE << Open_Mp_Workshare_Blk) | /* by jhs, 02/7/18 */
                                (ONE << Open_Mp_Parallel_Workshare_Blk) | /* by jhs, 02/7/18 */
                                (ONE << Contains_Blk) |
                                (ONE << Interface_Blk) |
                                (ONE << Derived_Type_Blk) |
				(ONE << Enum_Blk)),

			/*****  Open_MP_End_Do_Stmt  *****/

                               ((ONE << Unknown_Blk) |
                                (ONE << Blockdata_Blk) |
                                (ONE << Module_Blk) |
                                (ONE << Program_Blk) |
                                (ONE << Function_Blk) |
                                (ONE << Subroutine_Blk) |
                                (ONE << Internal_Blk) |
                                (ONE << Module_Proc_Blk) |
                                (ONE << Interface_Body_Blk) |
				(ONE << Forall_Blk) |
                                (ONE << If_Blk) |
                                (ONE << If_Then_Blk) |
                                (ONE << If_Else_If_Blk) |
                                (ONE << If_Else_Blk) |
                                (ONE << Do_Blk) |
                                (ONE << Select_Blk) |
                                (ONE << Case_Blk) |
                                (ONE << Where_Then_Blk) |
                                (ONE << Where_Else_Blk) |
				(ONE << Where_Else_Mask_Blk) |
                                (ONE << Parallel_Blk) |
                                (ONE << SGI_Parallel_Blk) |
                                (ONE << Doall_Blk) |
				(ONE << Wait_Blk) |
                                (ONE << SGI_Doacross_Blk) |
                                (ONE << SGI_Parallel_Do_Blk) |
                                (ONE << Do_Parallel_Blk) |
                                (ONE << SGI_Pdo_Blk) |
                                (ONE << Guard_Blk) |
                                (ONE << SGI_Critical_Section_Blk) |
                                (ONE << Parallel_Case_Blk) |
                                (ONE << SGI_Psection_Blk) |
                                (ONE << SGI_Section_Blk) |
                                (ONE << SGI_Single_Process_Blk) |
                                (ONE << SGI_Region_Blk) |
                                (ONE << Open_Mp_Parallel_Blk) |
                                (ONE << Open_Mp_Parallel_Sections_Blk) |
                                (ONE << Open_Mp_Sections_Blk) |
                                (ONE << Open_Mp_Section_Blk) |
                                (ONE << Open_Mp_Single_Blk) |
                                (ONE << Open_Mp_Parallel_Do_Blk) |
                                (ONE << Open_Mp_Master_Blk) |
                                (ONE << Open_Mp_Critical_Blk) |
                                (ONE << Open_Mp_Ordered_Blk) |
                                (ONE << Open_Mp_Workshare_Blk) | /* by jhs, 02/7/18 */
                                (ONE << Open_Mp_Parallel_Workshare_Blk) | /* by jhs, 02/7/18 */
                                (ONE << Contains_Blk) |
                                (ONE << Interface_Blk) |
                                (ONE << Derived_Type_Blk) |
				(ONE << Enum_Blk)),

			/*****  Open_MP_End_Parallel_Sections_Stmt  *****/

                               ((ONE << Unknown_Blk) |
                                (ONE << Blockdata_Blk) |
                                (ONE << Module_Blk) |
                                (ONE << Program_Blk) |
                                (ONE << Function_Blk) |
                                (ONE << Subroutine_Blk) |
                                (ONE << Internal_Blk) |
                                (ONE << Module_Proc_Blk) |
                                (ONE << Interface_Body_Blk) |
				(ONE << Forall_Blk) |
                                (ONE << If_Blk) |
                                (ONE << If_Then_Blk) |
                                (ONE << If_Else_If_Blk) |
                                (ONE << If_Else_Blk) |
                                (ONE << Do_Blk) |
                                (ONE << Select_Blk) |
                                (ONE << Case_Blk) |
                                (ONE << Where_Then_Blk) |
                                (ONE << Where_Else_Blk) |
				(ONE << Where_Else_Mask_Blk) |
                                (ONE << Parallel_Blk) |
                                (ONE << SGI_Parallel_Blk) |
                                (ONE << Doall_Blk) |
				(ONE << Wait_Blk) |
                                (ONE << SGI_Doacross_Blk) |
                                (ONE << SGI_Parallel_Do_Blk) |
                                (ONE << Do_Parallel_Blk) |
                                (ONE << SGI_Pdo_Blk) |
                                (ONE << Guard_Blk) |
                                (ONE << SGI_Critical_Section_Blk) |
                                (ONE << Parallel_Case_Blk) |
                                (ONE << SGI_Psection_Blk) |
                                (ONE << SGI_Section_Blk) |
                                (ONE << SGI_Single_Process_Blk) |
                                (ONE << SGI_Region_Blk) |
                                (ONE << Open_Mp_Parallel_Blk) |
                                (ONE << Open_Mp_Do_Blk) |
                                (ONE << Open_Mp_Sections_Blk) |
                                (ONE << Open_Mp_Section_Blk) |
                                (ONE << Open_Mp_Single_Blk) |
                                (ONE << Open_Mp_Parallel_Do_Blk) |
                                (ONE << Open_Mp_Master_Blk) |
                                (ONE << Open_Mp_Critical_Blk) |
                                (ONE << Open_Mp_Ordered_Blk) |
                                (ONE << Open_Mp_Workshare_Blk) | /* by jhs, 02/7/18 */
                                (ONE << Open_Mp_Parallel_Workshare_Blk) | /* by jhs, 02/7/18 */
                                (ONE << Contains_Blk) |
                                (ONE << Interface_Blk) |
                                (ONE << Derived_Type_Blk) |
				(ONE << Enum_Blk)),

			/*****  Open_MP_End_Sections_Stmt  *****/

                               ((ONE << Unknown_Blk) |
                                (ONE << Blockdata_Blk) |
                                (ONE << Module_Blk) |
                                (ONE << Program_Blk) |
                                (ONE << Function_Blk) |
                                (ONE << Subroutine_Blk) |
                                (ONE << Internal_Blk) |
                                (ONE << Module_Proc_Blk) |
                                (ONE << Interface_Body_Blk) |
				(ONE << Forall_Blk) |
                                (ONE << If_Blk) |
                                (ONE << If_Then_Blk) |
                                (ONE << If_Else_If_Blk) |
                                (ONE << If_Else_Blk) |
                                (ONE << Do_Blk) |
                                (ONE << Select_Blk) |
                                (ONE << Case_Blk) |
                                (ONE << Where_Then_Blk) |
                                (ONE << Where_Else_Blk) |
				(ONE << Where_Else_Mask_Blk) |
                                (ONE << Parallel_Blk) |
                                (ONE << SGI_Parallel_Blk) |
                                (ONE << Doall_Blk) |
				(ONE << Wait_Blk) |
                                (ONE << SGI_Doacross_Blk) |
                                (ONE << SGI_Parallel_Do_Blk) |
                                (ONE << Do_Parallel_Blk) |
                                (ONE << SGI_Pdo_Blk) |
                                (ONE << Guard_Blk) |
                                (ONE << SGI_Critical_Section_Blk) |
                                (ONE << Parallel_Case_Blk) |
                                (ONE << SGI_Psection_Blk) |
                                (ONE << SGI_Section_Blk) |
                                (ONE << SGI_Single_Process_Blk) |
                                (ONE << SGI_Region_Blk) |
                                (ONE << Open_Mp_Parallel_Blk) |
                                (ONE << Open_Mp_Do_Blk) |
                                (ONE << Open_Mp_Parallel_Sections_Blk) |
                                (ONE << Open_Mp_Section_Blk) |
                                (ONE << Open_Mp_Single_Blk) |
                                (ONE << Open_Mp_Parallel_Do_Blk) |
                                (ONE << Open_Mp_Master_Blk) |
                                (ONE << Open_Mp_Critical_Blk) |
                                (ONE << Open_Mp_Ordered_Blk) |
                                (ONE << Open_Mp_Workshare_Blk) | /* by jhs, 02/7/18 */
                                (ONE << Open_Mp_Parallel_Workshare_Blk) | /* by jhs, 02/7/18 */
                                (ONE << Contains_Blk) |
                                (ONE << Interface_Blk) |
                                (ONE << Derived_Type_Blk) |
				(ONE << Enum_Blk)),

			/*****  Open_MP_End_Section_Stmt  *****/

                               ((ONE << Unknown_Blk) |
                                (ONE << Blockdata_Blk) |
                                (ONE << Module_Blk) |
                                (ONE << Program_Blk) |
                                (ONE << Function_Blk) |
                                (ONE << Subroutine_Blk) |
                                (ONE << Internal_Blk) |
                                (ONE << Module_Proc_Blk) |
                                (ONE << Interface_Body_Blk) |
				(ONE << Forall_Blk) |
                                (ONE << If_Blk) |
                                (ONE << If_Then_Blk) |
                                (ONE << If_Else_If_Blk) |
                                (ONE << If_Else_Blk) |
                                (ONE << Do_Blk) |
                                (ONE << Select_Blk) |
                                (ONE << Case_Blk) |
                                (ONE << Where_Then_Blk) |
                                (ONE << Where_Else_Blk) |
				(ONE << Where_Else_Mask_Blk) |
                                (ONE << Parallel_Blk) |
                                (ONE << SGI_Parallel_Blk) |
                                (ONE << Doall_Blk) |
				(ONE << Wait_Blk) |
                                (ONE << SGI_Doacross_Blk) |
                                (ONE << SGI_Parallel_Do_Blk) |
                                (ONE << Do_Parallel_Blk) |
                                (ONE << SGI_Pdo_Blk) |
                                (ONE << Guard_Blk) |
                                (ONE << SGI_Critical_Section_Blk) |
                                (ONE << Parallel_Case_Blk) |
                                (ONE << SGI_Psection_Blk) |
                                (ONE << SGI_Section_Blk) |
                                (ONE << SGI_Single_Process_Blk) |
                                (ONE << SGI_Region_Blk) |
                                (ONE << Open_Mp_Parallel_Blk) |
                                (ONE << Open_Mp_Do_Blk) |
                                (ONE << Open_Mp_Parallel_Sections_Blk) |
                                (ONE << Open_Mp_Sections_Blk) |
                                (ONE << Open_Mp_Single_Blk) |
                                (ONE << Open_Mp_Parallel_Do_Blk) |
                                (ONE << Open_Mp_Master_Blk) |
                                (ONE << Open_Mp_Critical_Blk) |
                                (ONE << Open_Mp_Ordered_Blk) |
                                (ONE << Open_Mp_Workshare_Blk) | /* by jhs, 02/7/18 */
                                (ONE << Open_Mp_Parallel_Workshare_Blk) | /* by jhs, 02/7/18 */
                                (ONE << Contains_Blk) |
                                (ONE << Interface_Blk) |
                                (ONE << Derived_Type_Blk) |
				(ONE << Enum_Blk)),

			/*****  Open_MP_End_Single_Stmt  *****/

                               ((ONE << Unknown_Blk) |
                                (ONE << Blockdata_Blk) |
                                (ONE << Module_Blk) |
                                (ONE << Program_Blk) |
                                (ONE << Function_Blk) |
                                (ONE << Subroutine_Blk) |
                                (ONE << Internal_Blk) |
                                (ONE << Module_Proc_Blk) |
                                (ONE << Interface_Body_Blk) |
				(ONE << Forall_Blk) |
                                (ONE << If_Blk) |
                                (ONE << If_Then_Blk) |
                                (ONE << If_Else_If_Blk) |
                                (ONE << If_Else_Blk) |
                                (ONE << Do_Blk) |
                                (ONE << Select_Blk) |
                                (ONE << Case_Blk) |
                                (ONE << Where_Then_Blk) |
                                (ONE << Where_Else_Blk) |
				(ONE << Where_Else_Mask_Blk) |
                                (ONE << Parallel_Blk) |
                                (ONE << SGI_Parallel_Blk) |
                                (ONE << Doall_Blk) |
				(ONE << Wait_Blk) |
                                (ONE << SGI_Doacross_Blk) |
                                (ONE << SGI_Parallel_Do_Blk) |
                                (ONE << Do_Parallel_Blk) |
                                (ONE << SGI_Pdo_Blk) |
                                (ONE << Guard_Blk) |
                                (ONE << SGI_Critical_Section_Blk) |
                                (ONE << Parallel_Case_Blk) |
                                (ONE << SGI_Psection_Blk) |
                                (ONE << SGI_Section_Blk) |
                                (ONE << SGI_Single_Process_Blk) |
                                (ONE << SGI_Region_Blk) |
                                (ONE << Open_Mp_Parallel_Blk) |
                                (ONE << Open_Mp_Do_Blk) |
                                (ONE << Open_Mp_Parallel_Sections_Blk) |
                                (ONE << Open_Mp_Sections_Blk) |
                                (ONE << Open_Mp_Section_Blk) |
                                (ONE << Open_Mp_Parallel_Do_Blk) |
                                (ONE << Open_Mp_Master_Blk) |
                                (ONE << Open_Mp_Critical_Blk) |
                                (ONE << Open_Mp_Ordered_Blk) |
                                (ONE << Open_Mp_Workshare_Blk) | /* by jhs, 02/7/18 */
                                (ONE << Open_Mp_Parallel_Workshare_Blk) | /* by jhs, 02/7/18 */
                                (ONE << Contains_Blk) |
                                (ONE << Interface_Blk) |
                                (ONE << Derived_Type_Blk) |
				(ONE << Enum_Blk)),

			/*****  Open_MP_End_Parallel_Do_Stmt  *****/

                               ((ONE << Unknown_Blk) |
                                (ONE << Blockdata_Blk) |
                                (ONE << Module_Blk) |
                                (ONE << Program_Blk) |
                                (ONE << Function_Blk) |
                                (ONE << Subroutine_Blk) |
                                (ONE << Internal_Blk) |
                                (ONE << Module_Proc_Blk) |
                                (ONE << Interface_Body_Blk) |
				(ONE << Forall_Blk) |
                                (ONE << If_Blk) |
                                (ONE << If_Then_Blk) |
                                (ONE << If_Else_If_Blk) |
                                (ONE << If_Else_Blk) |
                                (ONE << Do_Blk) |
                                (ONE << Select_Blk) |
                                (ONE << Case_Blk) |
                                (ONE << Where_Then_Blk) |
                                (ONE << Where_Else_Blk) |
				(ONE << Where_Else_Mask_Blk) |
                                (ONE << Parallel_Blk) |
                                (ONE << SGI_Parallel_Blk) |
                                (ONE << Doall_Blk) |
				(ONE << Wait_Blk) |
                                (ONE << SGI_Doacross_Blk) |
                                (ONE << SGI_Parallel_Do_Blk) |
                                (ONE << Do_Parallel_Blk) |
                                (ONE << SGI_Pdo_Blk) |
                                (ONE << Guard_Blk) |
                                (ONE << SGI_Critical_Section_Blk) |
                                (ONE << Parallel_Case_Blk) |
                                (ONE << SGI_Psection_Blk) |
                                (ONE << SGI_Section_Blk) |
                                (ONE << SGI_Single_Process_Blk) |
                                (ONE << SGI_Region_Blk) |
                                (ONE << Open_Mp_Parallel_Blk) |
                                (ONE << Open_Mp_Do_Blk) |
                                (ONE << Open_Mp_Parallel_Sections_Blk) |
                                (ONE << Open_Mp_Sections_Blk) |
                                (ONE << Open_Mp_Section_Blk) |
                                (ONE << Open_Mp_Single_Blk) |
                                (ONE << Open_Mp_Master_Blk) |
                                (ONE << Open_Mp_Critical_Blk) |
                                (ONE << Open_Mp_Ordered_Blk) |
                                (ONE << Open_Mp_Workshare_Blk) | /* by jhs, 02/7/18 */
                                (ONE << Open_Mp_Parallel_Workshare_Blk) | /* by jhs, 02/7/18 */
                                (ONE << Contains_Blk) |
                                (ONE << Interface_Blk) |
                                (ONE << Derived_Type_Blk) |
				(ONE << Enum_Blk)),

			/*****  Open_MP_End_Master_Stmt  *****/

                               ((ONE << Unknown_Blk) |
                                (ONE << Blockdata_Blk) |
                                (ONE << Module_Blk) |
                                (ONE << Program_Blk) |
                                (ONE << Function_Blk) |
                                (ONE << Subroutine_Blk) |
                                (ONE << Internal_Blk) |
                                (ONE << Module_Proc_Blk) |
                                (ONE << Interface_Body_Blk) |
				(ONE << Forall_Blk) |
                                (ONE << If_Blk) |
                                (ONE << If_Then_Blk) |
                                (ONE << If_Else_If_Blk) |
                                (ONE << If_Else_Blk) |
                                (ONE << Do_Blk) |
                                (ONE << Select_Blk) |
                                (ONE << Case_Blk) |
                                (ONE << Where_Then_Blk) |
                                (ONE << Where_Else_Blk) |
				(ONE << Where_Else_Mask_Blk) |
                                (ONE << Parallel_Blk) |
                                (ONE << SGI_Parallel_Blk) |
                                (ONE << Doall_Blk) |
				(ONE << Wait_Blk) |
                                (ONE << SGI_Doacross_Blk) |
                                (ONE << SGI_Parallel_Do_Blk) |
                                (ONE << Do_Parallel_Blk) |
                                (ONE << SGI_Pdo_Blk) |
                                (ONE << Guard_Blk) |
                                (ONE << SGI_Critical_Section_Blk) |
                                (ONE << Parallel_Case_Blk) |
                                (ONE << SGI_Psection_Blk) |
                                (ONE << SGI_Section_Blk) |
                                (ONE << SGI_Single_Process_Blk) |
                                (ONE << SGI_Region_Blk) |
                                (ONE << Open_Mp_Parallel_Blk) |
                                (ONE << Open_Mp_Do_Blk) |
                                (ONE << Open_Mp_Parallel_Sections_Blk) |
                                (ONE << Open_Mp_Sections_Blk) |
                                (ONE << Open_Mp_Section_Blk) |
                                (ONE << Open_Mp_Single_Blk) |
                                (ONE << Open_Mp_Parallel_Do_Blk) |
                                (ONE << Open_Mp_Critical_Blk) |
                                (ONE << Open_Mp_Ordered_Blk) |
                                (ONE << Open_Mp_Workshare_Blk) | /* by jhs, 02/7/18 */
                                (ONE << Open_Mp_Parallel_Workshare_Blk) | /* by jhs, 02/7/18 */
                                (ONE << Contains_Blk) |
                                (ONE << Interface_Blk) |
                                (ONE << Derived_Type_Blk) |
				(ONE << Enum_Blk)),

			/*****  Open_MP_End_Critical_Stmt  *****/

                               ((ONE << Unknown_Blk) |
                                (ONE << Blockdata_Blk) |
                                (ONE << Module_Blk) |
                                (ONE << Program_Blk) |
                                (ONE << Function_Blk) |
                                (ONE << Subroutine_Blk) |
                                (ONE << Internal_Blk) |
                                (ONE << Module_Proc_Blk) |
                                (ONE << Interface_Body_Blk) |
				(ONE << Forall_Blk) |
                                (ONE << If_Blk) |
                                (ONE << If_Then_Blk) |
                                (ONE << If_Else_If_Blk) |
                                (ONE << If_Else_Blk) |
                                (ONE << Do_Blk) |
                                (ONE << Select_Blk) |
                                (ONE << Case_Blk) |
                                (ONE << Where_Then_Blk) |
                                (ONE << Where_Else_Blk) |
				(ONE << Where_Else_Mask_Blk) |
                                (ONE << Parallel_Blk) |
                                (ONE << SGI_Parallel_Blk) |
                                (ONE << Doall_Blk) |
				(ONE << Wait_Blk) |
                                (ONE << SGI_Doacross_Blk) |
                                (ONE << SGI_Parallel_Do_Blk) |
                                (ONE << Do_Parallel_Blk) |
                                (ONE << SGI_Pdo_Blk) |
                                (ONE << Guard_Blk) |
                                (ONE << SGI_Critical_Section_Blk) |
                                (ONE << Parallel_Case_Blk) |
                                (ONE << SGI_Psection_Blk) |
                                (ONE << SGI_Section_Blk) |
                                (ONE << SGI_Single_Process_Blk) |
                                (ONE << SGI_Region_Blk) |
                                (ONE << Open_Mp_Parallel_Blk) |
                                (ONE << Open_Mp_Do_Blk) |
                                (ONE << Open_Mp_Parallel_Sections_Blk) |
                                (ONE << Open_Mp_Sections_Blk) |
                                (ONE << Open_Mp_Section_Blk) |
                                (ONE << Open_Mp_Single_Blk) |
                                (ONE << Open_Mp_Parallel_Do_Blk) |
                                (ONE << Open_Mp_Master_Blk) |
                                (ONE << Open_Mp_Ordered_Blk) |
                                (ONE << Open_Mp_Workshare_Blk) | /* by jhs, 02/7/18 */
                                (ONE << Open_Mp_Parallel_Workshare_Blk) | /* by jhs, 02/7/18 */
                                (ONE << Contains_Blk) |
                                (ONE << Interface_Blk) |
                                (ONE << Derived_Type_Blk) |
				(ONE << Enum_Blk)),

			/*****  Open_MP_End_Ordered_Stmt  *****/

                               ((ONE << Unknown_Blk) |
                                (ONE << Blockdata_Blk) |
                                (ONE << Module_Blk) |
                                (ONE << Program_Blk) |
                                (ONE << Function_Blk) |
                                (ONE << Subroutine_Blk) |
                                (ONE << Internal_Blk) |
                                (ONE << Module_Proc_Blk) |
                                (ONE << Interface_Body_Blk) |
				(ONE << Forall_Blk) |
                                (ONE << If_Blk) |
                                (ONE << If_Then_Blk) |
                                (ONE << If_Else_If_Blk) |
                                (ONE << If_Else_Blk) |
                                (ONE << Do_Blk) |
                                (ONE << Select_Blk) |
                                (ONE << Case_Blk) |
                                (ONE << Where_Then_Blk) |
                                (ONE << Where_Else_Blk) |
				(ONE << Where_Else_Mask_Blk) |
                                (ONE << Parallel_Blk) |
                                (ONE << SGI_Parallel_Blk) |
                                (ONE << Doall_Blk) |
				(ONE << Wait_Blk) |
                                (ONE << SGI_Doacross_Blk) |
                                (ONE << SGI_Parallel_Do_Blk) |
                                (ONE << Do_Parallel_Blk) |
                                (ONE << SGI_Pdo_Blk) |
                                (ONE << Guard_Blk) |
                                (ONE << SGI_Critical_Section_Blk) |
                                (ONE << Parallel_Case_Blk) |
                                (ONE << SGI_Psection_Blk) |
                                (ONE << SGI_Section_Blk) |
                                (ONE << SGI_Single_Process_Blk) |
                                (ONE << SGI_Region_Blk) |
                                (ONE << Open_Mp_Parallel_Blk) |
                                (ONE << Open_Mp_Do_Blk) |
                                (ONE << Open_Mp_Parallel_Sections_Blk) |
                                (ONE << Open_Mp_Sections_Blk) |
                                (ONE << Open_Mp_Section_Blk) |
                                (ONE << Open_Mp_Single_Blk) |
                                (ONE << Open_Mp_Parallel_Do_Blk) |
                                (ONE << Open_Mp_Master_Blk) |
                                (ONE << Open_Mp_Critical_Blk) |
                                (ONE << Contains_Blk) |
                                (ONE << Interface_Blk) |
                                (ONE << Derived_Type_Blk) |
				(ONE << Enum_Blk)),

			/*****  Forall_Cstrct_Stmt  *****/

			       ((ONE << Unknown_Blk) |
				(ONE << Blockdata_Blk) |
				(ONE << Module_Blk) |
				(ONE << Interface_Body_Blk) |
				(ONE << If_Blk) |
				(ONE << Where_Then_Blk) |
				(ONE << Where_Else_Blk) |
				(ONE << Where_Else_Mask_Blk) |
				(ONE << SGI_Psection_Blk) |
				(ONE << Select_Blk) |
				(ONE << Contains_Blk) |
				(ONE << Interface_Blk) |
				(ONE << Derived_Type_Blk) | 
				(ONE << Enum_Blk)),

			/*****  Forall_Stmt  *****/

			       ((ONE << Unknown_Blk) |
				(ONE << Blockdata_Blk) |
				(ONE << Module_Blk) |
				(ONE << Interface_Body_Blk) |
				(ONE << If_Blk) |
				(ONE << Where_Then_Blk) |
				(ONE << Where_Else_Blk) |
				(ONE << Where_Else_Mask_Blk) |
				(ONE << SGI_Psection_Blk) |
				(ONE << Select_Blk) |
				(ONE << Contains_Blk) |
				(ONE << Interface_Blk) |
				(ONE << Derived_Type_Blk) | 
				(ONE << Enum_Blk)),


			/*****  End_Forall_Stmt  *****/

			       ((ONE << Unknown_Blk) |
				(ONE << Blockdata_Blk) |
				(ONE << Module_Blk) |
				(ONE << Program_Blk) |
				(ONE << Function_Blk) |
				(ONE << Subroutine_Blk) |
				(ONE << Internal_Blk) |
				(ONE << Module_Proc_Blk) |
				(ONE << Interface_Body_Blk) |
				(ONE << If_Blk) |
				(ONE << If_Then_Blk) |
				(ONE << If_Else_If_Blk) |
				(ONE << If_Else_Blk) |
				(ONE << Do_Blk) |
				(ONE << Select_Blk) |
				(ONE << Case_Blk) |
                                (ONE << Parallel_Blk) |
				(ONE << SGI_Parallel_Blk) |
                                (ONE << Doall_Blk) |
				(ONE << Wait_Blk) |
				(ONE << SGI_Doacross_Blk) |
				(ONE << SGI_Parallel_Do_Blk) |
                                (ONE << Do_Parallel_Blk) |
				(ONE << SGI_Pdo_Blk) |
                                (ONE << Guard_Blk) |
				(ONE << SGI_Critical_Section_Blk) |
                                (ONE << Parallel_Case_Blk) |
				(ONE << SGI_Psection_Blk) |
				(ONE << SGI_Section_Blk) |
				(ONE << SGI_Single_Process_Blk) |
				(ONE << SGI_Region_Blk) |
                                (ONE << Open_Mp_Parallel_Blk) |
                                (ONE << Open_Mp_Do_Blk) |
                                (ONE << Open_Mp_Parallel_Sections_Blk) |
                                (ONE << Open_Mp_Sections_Blk) |
                                (ONE << Open_Mp_Section_Blk) |
                                (ONE << Open_Mp_Single_Blk) |
                                (ONE << Open_Mp_Parallel_Do_Blk) |
                                (ONE << Open_Mp_Master_Blk) |
                                (ONE << Open_Mp_Critical_Blk) |
                                (ONE << Open_Mp_Ordered_Blk) |
                                (ONE << Open_Mp_Workshare_Blk) | /* by jhs, 02/7/18 */
                                (ONE << Open_Mp_Parallel_Workshare_Blk) | /* by jhs, 02/7/18 */
				(ONE << Contains_Blk) |
				(ONE << Interface_Blk) |
				(ONE << Derived_Type_Blk) | 
				(ONE << Enum_Blk)),

                        /*****  Else_Where_Mask_Stmt  *****/

                               ((ONE << Unknown_Blk) |
                                (ONE << Blockdata_Blk) |
                                (ONE << Module_Blk) |
                                (ONE << Program_Blk) |
                                (ONE << Function_Blk) |
                                (ONE << Subroutine_Blk) |
                                (ONE << Internal_Blk) |
                                (ONE << Module_Proc_Blk) |
                                (ONE << Interface_Body_Blk) |
                                (ONE << Forall_Blk) |
                                (ONE << If_Blk) |
                                (ONE << If_Then_Blk) |
                                (ONE << If_Else_If_Blk) |
                                (ONE << If_Else_Blk) |
                                (ONE << Do_Blk) |
                                (ONE << Select_Blk) |
                                (ONE << Case_Blk) |
                                (ONE << Where_Else_Blk) |
                                (ONE << Parallel_Blk) |
                                (ONE << SGI_Parallel_Blk) |
                                (ONE << Doall_Blk) |
                                (ONE << Wait_Blk) |
                                (ONE << SGI_Doacross_Blk) |
                                (ONE << SGI_Parallel_Do_Blk) |
                                (ONE << Do_Parallel_Blk) |
                                (ONE << SGI_Pdo_Blk) |
                                (ONE << Guard_Blk) |
                                (ONE << SGI_Critical_Section_Blk) |
                                (ONE << Parallel_Case_Blk) |
                                (ONE << SGI_Psection_Blk) |
                                (ONE << SGI_Section_Blk) |
                                (ONE << SGI_Single_Process_Blk) |
                                (ONE << SGI_Region_Blk) |
                                (ONE << Open_Mp_Parallel_Blk) |
                                (ONE << Open_Mp_Do_Blk) |
                                (ONE << Open_Mp_Parallel_Sections_Blk) |
                                (ONE << Open_Mp_Sections_Blk) |
                                (ONE << Open_Mp_Section_Blk) |
                                (ONE << Open_Mp_Single_Blk) |
                                (ONE << Open_Mp_Parallel_Do_Blk) |
                                (ONE << Open_Mp_Master_Blk) |
                                (ONE << Open_Mp_Critical_Blk) |
                                (ONE << Open_Mp_Ordered_Blk) |
                                (ONE << Open_Mp_Workshare_Blk) | /* by jhs, 02/7/18 */
                                (ONE << Open_Mp_Parallel_Workshare_Blk) | /* by jhs, 02/7/18 */
                                (ONE << Contains_Blk) |
                                (ONE << Interface_Blk) |
                                (ONE << Derived_Type_Blk) |
				(ONE << Enum_Blk)),

			/*****  Volatile_Stmt   *****/

			       ((ONE << Unknown_Blk) |
				(ONE << Forall_Blk) |
				(ONE << If_Blk) |
				(ONE << If_Then_Blk) |
				(ONE << If_Else_If_Blk) |
				(ONE << If_Else_Blk) |
				(ONE << Do_Blk) |
				(ONE << Select_Blk) |
				(ONE << Case_Blk) |
				(ONE << Where_Then_Blk) |
				(ONE << Where_Else_Blk) |
				(ONE << Where_Else_Mask_Blk) |
                                (ONE << Parallel_Blk) |
				(ONE << SGI_Parallel_Blk) |
                                (ONE << Doall_Blk) |
				(ONE << Wait_Blk) |
				(ONE << SGI_Doacross_Blk) |
				(ONE << SGI_Parallel_Do_Blk) |
                                (ONE << Do_Parallel_Blk) |
				(ONE << SGI_Pdo_Blk) |
                                (ONE << Guard_Blk) |
				(ONE << SGI_Critical_Section_Blk) |
                                (ONE << Parallel_Case_Blk) |
				(ONE << SGI_Psection_Blk) |
				(ONE << SGI_Section_Blk) |
				(ONE << SGI_Single_Process_Blk) |
                                (ONE << Open_Mp_Parallel_Blk) |
                                (ONE << Open_Mp_Do_Blk) |
                                (ONE << Open_Mp_Parallel_Sections_Blk) |
                                (ONE << Open_Mp_Sections_Blk) |
                                (ONE << Open_Mp_Section_Blk) |
                                (ONE << Open_Mp_Single_Blk) |
                                (ONE << Open_Mp_Parallel_Do_Blk) |
                                (ONE << Open_Mp_Master_Blk) |
                                (ONE << Open_Mp_Critical_Blk) |
                                (ONE << Open_Mp_Ordered_Blk) |
                                (ONE << Open_Mp_Workshare_Blk) | /* by jhs, 02/7/18 */
                                (ONE << Open_Mp_Parallel_Workshare_Blk) | /* by jhs, 02/7/18 */
				(ONE << Contains_Blk) |
				(ONE << Interface_Blk) |
				(ONE << Derived_Type_Blk) |
				(ONE << Enum_Blk)),

			/*****  Open_MP_End_Parallel_workshare_Stmt  *****/

                               ((ONE << Unknown_Blk) |
                                (ONE << Blockdata_Blk) |
                                (ONE << Module_Blk) |
                                (ONE << Program_Blk) |
                                (ONE << Function_Blk) |
                                (ONE << Subroutine_Blk) |
                                (ONE << Internal_Blk) |
                                (ONE << Module_Proc_Blk) |
                                (ONE << Interface_Body_Blk) |
                                (ONE << Do_Blk) |
				(ONE << Forall_Blk) |
                                (ONE << If_Blk) |
                                (ONE << If_Then_Blk) |
                                (ONE << If_Else_If_Blk) |
                                (ONE << If_Else_Blk) |
                                (ONE << Select_Blk) |
                                (ONE << Case_Blk) |
                                (ONE << Where_Then_Blk) |
                                (ONE << Where_Else_Blk) |
				(ONE << Where_Else_Mask_Blk) |
                                (ONE << Parallel_Blk) |
                                (ONE << Doall_Blk) |
                                (ONE << Do_Parallel_Blk) |
                                (ONE << Guard_Blk) |
                                (ONE << Parallel_Case_Blk) |
				(ONE << Wait_Blk) |
                                (ONE << SGI_Doacross_Blk) |
                                (ONE << SGI_Psection_Blk) |
                                (ONE << SGI_Section_Blk) |
                                (ONE << SGI_Pdo_Blk) |
                                (ONE << SGI_Parallel_Do_Blk) |
                                (ONE << SGI_Parallel_Blk) |
                                (ONE << SGI_Critical_Section_Blk) |
                                (ONE << SGI_Single_Process_Blk) |
                                (ONE << SGI_Region_Blk) |
                                (ONE << Open_Mp_Parallel_Blk) |
                                (ONE << Open_Mp_Do_Blk) |
				(ONE << Open_Mp_Parallel_Sections_Blk) |
                                (ONE << Open_Mp_Sections_Blk) |
                                (ONE << Open_Mp_Section_Blk) |
                                (ONE << Open_Mp_Single_Blk) |
                                (ONE << Open_Mp_Parallel_Do_Blk) |
                                (ONE << Open_Mp_Master_Blk) |
                                (ONE << Open_Mp_Critical_Blk) |
                                (ONE << Open_Mp_Ordered_Blk) |
                                (ONE << Open_Mp_Workshare_Blk) | /* by jhs, 02/7/18 */
                                (ONE << Contains_Blk) |
                                (ONE << Interface_Blk) |
                                (ONE << Derived_Type_Blk) |
				(ONE << Enum_Blk)),

			/*****  Open_MP_End_Workshare_Stmt  *****/

                               ((ONE << Unknown_Blk) |
                                (ONE << Blockdata_Blk) |
                                (ONE << Module_Blk) |
                                (ONE << Program_Blk) |
                                (ONE << Function_Blk) |
                                (ONE << Subroutine_Blk) |
                                (ONE << Internal_Blk) |
                                (ONE << Module_Proc_Blk) |
                                (ONE << Interface_Body_Blk) |
                                (ONE << Do_Blk) |
				(ONE << Forall_Blk) |
                                (ONE << If_Blk) |
                                (ONE << If_Then_Blk) |
                                (ONE << If_Else_If_Blk) |
                                (ONE << If_Else_Blk) |
                                (ONE << Select_Blk) |
                                (ONE << Case_Blk) |
                                (ONE << Where_Then_Blk) |
                                (ONE << Where_Else_Blk) |
				(ONE << Where_Else_Mask_Blk) |
                                (ONE << Parallel_Blk) |
                                (ONE << Doall_Blk) |
                                (ONE << Do_Parallel_Blk) |
                                (ONE << Guard_Blk) |
                                (ONE << Parallel_Case_Blk) |
				(ONE << Wait_Blk) |
                                (ONE << SGI_Doacross_Blk) |
                                (ONE << SGI_Psection_Blk) |
                                (ONE << SGI_Section_Blk) |
                                (ONE << SGI_Pdo_Blk) |
                                (ONE << SGI_Parallel_Do_Blk) |
                                (ONE << SGI_Parallel_Blk) |
                                (ONE << SGI_Critical_Section_Blk) |
                                (ONE << SGI_Single_Process_Blk) |
                                (ONE << SGI_Region_Blk) |
                                (ONE << Open_Mp_Parallel_Blk) |
                                (ONE << Open_Mp_Do_Blk) |
				(ONE << Open_Mp_Parallel_Sections_Blk) |
                                (ONE << Open_Mp_Sections_Blk) |
                                (ONE << Open_Mp_Section_Blk) |
                                (ONE << Open_Mp_Single_Blk) |
                                (ONE << Open_Mp_Parallel_Do_Blk) |
                                (ONE << Open_Mp_Master_Blk) |
                                (ONE << Open_Mp_Critical_Blk) |
                                (ONE << Open_Mp_Ordered_Blk) |
                                (ONE << Open_Mp_Parallel_Workshare_Blk) | /* by jhs, 02/7/18 */
                                (ONE << Contains_Blk) |
                                (ONE << Interface_Blk) |
                                (ONE << Derived_Type_Blk) |
				(ONE << Enum_Blk)),

#ifdef KEY /* Bug 11741 */
			/*****  Import_Stmt  *****/

				((ONE << Unknown_Blk) |
                                (ONE << Blockdata_Blk) |
                                (ONE << Module_Blk) |
				(ONE << Program_Blk) |
                                (ONE << Function_Blk) |
                                (ONE << Subroutine_Blk) |
				(ONE << Internal_Blk) |
				(ONE << Module_Proc_Blk) |
				(ONE << Interface_Blk) |
                                (ONE << Do_Blk) |
                                (ONE << Forall_Blk) |
                                (ONE << If_Blk) |
                                (ONE << If_Then_Blk) |
				(ONE << If_Else_If_Blk) |
				(ONE << If_Else_Blk) |
                                (ONE << Select_Blk) |
                                (ONE << Case_Blk) |
                                (ONE << Where_Then_Blk) |
                                (ONE << Where_Else_Blk) |
                                (ONE << Where_Else_Mask_Blk) |
				(ONE << Parallel_Blk) |
				(ONE << Doall_Blk) |
				(ONE << Do_Parallel_Blk) |
				(ONE << Guard_Blk) |
				(ONE << Parallel_Case_Blk) |
				(ONE << Wait_Blk) |
				(ONE << SGI_Doacross_Blk) |
				(ONE << SGI_Psection_Blk) |
				(ONE << SGI_Section_Blk) |
				(ONE << SGI_Pdo_Blk) |
				(ONE << SGI_Parallel_Do_Blk) |
				(ONE << SGI_Parallel_Blk) |
				(ONE << SGI_Critical_Section_Blk) |
				(ONE << SGI_Single_Process_Blk) |
				(ONE << SGI_Region_Blk) |
                                (ONE << Open_Mp_Parallel_Blk) |
                                (ONE << Open_Mp_Do_Blk) |
                                (ONE << Open_Mp_Parallel_Sections_Blk) |
                                (ONE << Open_Mp_Sections_Blk) |
                                (ONE << Open_Mp_Section_Blk) |
                                (ONE << Open_Mp_Single_Blk) |
                                (ONE << Open_Mp_Parallel_Do_Blk) |
                                (ONE << Open_Mp_Master_Blk) |
                                (ONE << Open_Mp_Critical_Blk) |
                                (ONE << Open_Mp_Ordered_Blk) |
                                (ONE << Open_Mp_Workshare_Blk) |
                                (ONE << Open_Mp_Parallel_Workshare_Blk) |
                                (ONE << Contains_Blk) |
				(ONE << Derived_Type_Blk) |
				(ONE << Enum_Blk)),
#endif /* KEY Bug 11741 */
#ifdef KEY /* Bug 10572 */
			/*****  Enum_Stmt  *****/

			       ((ONE << Unknown_Blk) |
				(ONE << Forall_Blk) |
				(ONE << If_Blk) |
				(ONE << If_Then_Blk) |
				(ONE << If_Else_If_Blk) |
				(ONE << If_Else_Blk) |
				(ONE << Do_Blk) |
				(ONE << Select_Blk) |
				(ONE << Case_Blk) |
				(ONE << Where_Then_Blk) |
				(ONE << Where_Else_Blk) |
				(ONE << Where_Else_Mask_Blk) |
                                (ONE << Parallel_Blk) |
				(ONE << SGI_Parallel_Blk) |
                                (ONE << Doall_Blk) |
				(ONE << Wait_Blk) |
				(ONE << SGI_Doacross_Blk) |
				(ONE << SGI_Parallel_Do_Blk) |
                                (ONE << Do_Parallel_Blk) |
				(ONE << SGI_Pdo_Blk) |
                                (ONE << Guard_Blk) |
				(ONE << SGI_Critical_Section_Blk) |
                                (ONE << Parallel_Case_Blk) |
				(ONE << SGI_Psection_Blk) |
				(ONE << SGI_Section_Blk) |
				(ONE << SGI_Single_Process_Blk) |
                                (ONE << Open_Mp_Parallel_Blk) |
                                (ONE << Open_Mp_Do_Blk) |
                                (ONE << Open_Mp_Parallel_Sections_Blk) |
                                (ONE << Open_Mp_Sections_Blk) |
                                (ONE << Open_Mp_Section_Blk) |
                                (ONE << Open_Mp_Single_Blk) |
                                (ONE << Open_Mp_Parallel_Do_Blk) |
                                (ONE << Open_Mp_Master_Blk) |
                                (ONE << Open_Mp_Critical_Blk) |
                                (ONE << Open_Mp_Ordered_Blk) |
                                (ONE << Open_Mp_Workshare_Blk) | /* by jhs, 02/7/18 */
                                (ONE << Open_Mp_Parallel_Workshare_Blk) | /* by jhs, 02/7/18 */
				(ONE << Contains_Blk) |
				(ONE << Interface_Blk) |
				(ONE << Derived_Type_Blk) | 
				(ONE << Enum_Blk)),

			/*****  End_Enum_Stmt  *****/

				((ONE << Unknown_Blk) |
                                (ONE << Blockdata_Blk) |
                                (ONE << Module_Blk) |
				(ONE << Program_Blk) |
                                (ONE << Function_Blk) |
                                (ONE << Subroutine_Blk) |
				(ONE << Internal_Blk) |
				(ONE << Module_Proc_Blk) |
				(ONE << Interface_Body_Blk) |
				(ONE << Interface_Blk) |
                                (ONE << Do_Blk) |
                                (ONE << Forall_Blk) |
                                (ONE << If_Blk) |
                                (ONE << If_Then_Blk) |
				(ONE << If_Else_If_Blk) |
				(ONE << If_Else_Blk) |
                                (ONE << Select_Blk) |
                                (ONE << Case_Blk) |
                                (ONE << Where_Then_Blk) |
                                (ONE << Where_Else_Blk) |
                                (ONE << Where_Else_Mask_Blk) |
				(ONE << Parallel_Blk) |
				(ONE << Doall_Blk) |
				(ONE << Do_Parallel_Blk) |
				(ONE << Guard_Blk) |
				(ONE << Parallel_Case_Blk) |
				(ONE << Wait_Blk) |
				(ONE << SGI_Doacross_Blk) |
				(ONE << SGI_Psection_Blk) |
				(ONE << SGI_Section_Blk) |
				(ONE << SGI_Pdo_Blk) |
				(ONE << SGI_Parallel_Do_Blk) |
				(ONE << SGI_Parallel_Blk) |
				(ONE << SGI_Critical_Section_Blk) |
				(ONE << SGI_Single_Process_Blk) |
				(ONE << SGI_Region_Blk) |
                                (ONE << Open_Mp_Parallel_Blk) |
                                (ONE << Open_Mp_Do_Blk) |
                                (ONE << Open_Mp_Parallel_Sections_Blk) |
                                (ONE << Open_Mp_Sections_Blk) |
                                (ONE << Open_Mp_Section_Blk) |
                                (ONE << Open_Mp_Single_Blk) |
                                (ONE << Open_Mp_Parallel_Do_Blk) |
                                (ONE << Open_Mp_Master_Blk) |
                                (ONE << Open_Mp_Critical_Blk) |
                                (ONE << Open_Mp_Ordered_Blk) |
                                (ONE << Open_Mp_Workshare_Blk) |
                                (ONE << Open_Mp_Parallel_Workshare_Blk) |
                                (ONE << Contains_Blk) |
				(ONE << Derived_Type_Blk)),

			/*****  Enumerator_Stmt  *****/

				((ONE << Unknown_Blk) |
                                (ONE << Blockdata_Blk) |
                                (ONE << Module_Blk) |
				(ONE << Program_Blk) |
                                (ONE << Function_Blk) |
                                (ONE << Subroutine_Blk) |
				(ONE << Internal_Blk) |
				(ONE << Module_Proc_Blk) |
				(ONE << Interface_Body_Blk) |
				(ONE << Interface_Blk) |
                                (ONE << Do_Blk) |
                                (ONE << Forall_Blk) |
                                (ONE << If_Blk) |
                                (ONE << If_Then_Blk) |
				(ONE << If_Else_If_Blk) |
				(ONE << If_Else_Blk) |
                                (ONE << Select_Blk) |
                                (ONE << Case_Blk) |
                                (ONE << Where_Then_Blk) |
                                (ONE << Where_Else_Blk) |
                                (ONE << Where_Else_Mask_Blk) |
				(ONE << Parallel_Blk) |
				(ONE << Doall_Blk) |
				(ONE << Do_Parallel_Blk) |
				(ONE << Guard_Blk) |
				(ONE << Parallel_Case_Blk) |
				(ONE << Wait_Blk) |
				(ONE << SGI_Doacross_Blk) |
				(ONE << SGI_Psection_Blk) |
				(ONE << SGI_Section_Blk) |
				(ONE << SGI_Pdo_Blk) |
				(ONE << SGI_Parallel_Do_Blk) |
				(ONE << SGI_Parallel_Blk) |
				(ONE << SGI_Critical_Section_Blk) |
				(ONE << SGI_Single_Process_Blk) |
				(ONE << SGI_Region_Blk) |
                                (ONE << Open_Mp_Parallel_Blk) |
                                (ONE << Open_Mp_Do_Blk) |
                                (ONE << Open_Mp_Parallel_Sections_Blk) |
                                (ONE << Open_Mp_Sections_Blk) |
                                (ONE << Open_Mp_Section_Blk) |
                                (ONE << Open_Mp_Single_Blk) |
                                (ONE << Open_Mp_Parallel_Do_Blk) |
                                (ONE << Open_Mp_Master_Blk) |
                                (ONE << Open_Mp_Critical_Blk) |
                                (ONE << Open_Mp_Ordered_Blk) |
                                (ONE << Open_Mp_Workshare_Blk) |
                                (ONE << Open_Mp_Parallel_Workshare_Blk) |
                                (ONE << Contains_Blk) |
				(ONE << Derived_Type_Blk)),
#endif /* KEY Bug 10572 */
#ifdef KEY /* Bug 14150 */

			/*****  Bind_Stmt  *****/

			       ((ONE << Unknown_Blk) |
				(ONE << Internal_Blk) |
				(ONE << Forall_Blk) |
				(ONE << If_Blk) |
				(ONE << If_Then_Blk) |
				(ONE << If_Else_If_Blk) |
				(ONE << If_Else_Blk) |
				(ONE << Do_Blk) |
				(ONE << Select_Blk) |
				(ONE << Case_Blk) |
				(ONE << Where_Then_Blk) |
				(ONE << Where_Else_Blk) |
				(ONE << Where_Else_Mask_Blk) |
                                (ONE << Parallel_Blk) |
				(ONE << SGI_Parallel_Blk) |
                                (ONE << Doall_Blk) |
				(ONE << Wait_Blk) |
				(ONE << SGI_Doacross_Blk) |
				(ONE << SGI_Parallel_Do_Blk) |
                                (ONE << Do_Parallel_Blk) |
				(ONE << SGI_Pdo_Blk) |
                                (ONE << Guard_Blk) |
				(ONE << SGI_Critical_Section_Blk) |
                                (ONE << Parallel_Case_Blk) |
				(ONE << SGI_Psection_Blk) |
				(ONE << SGI_Section_Blk) |
				(ONE << SGI_Single_Process_Blk) |
                                (ONE << Open_Mp_Parallel_Blk) |
                                (ONE << Open_Mp_Do_Blk) |
                                (ONE << Open_Mp_Parallel_Sections_Blk) |
                                (ONE << Open_Mp_Sections_Blk) |
                                (ONE << Open_Mp_Section_Blk) |
                                (ONE << Open_Mp_Single_Blk) |
                                (ONE << Open_Mp_Parallel_Do_Blk) |
                                (ONE << Open_Mp_Master_Blk) |
                                (ONE << Open_Mp_Critical_Blk) |
                                (ONE << Open_Mp_Ordered_Blk) |
                                (ONE << Open_Mp_Workshare_Blk) | /* by jhs, 02/7/18 */
                                (ONE << Open_Mp_Parallel_Workshare_Blk) | /* by jhs, 02/7/18 */
				(ONE << Contains_Blk) |
				(ONE << Interface_Blk) |    
				(ONE << Enum_Blk)),

			/*****  Value_Stmt  *****/
			       ((ONE << Unknown_Blk) |
				(ONE << Blockdata_Blk) |
				(ONE << Module_Blk) |
				(ONE << Program_Blk) |
				(ONE << Forall_Blk) |
				(ONE << If_Blk) |
				(ONE << If_Then_Blk) |
				(ONE << If_Else_If_Blk) |
				(ONE << If_Else_Blk) |
				(ONE << Do_Blk) |
				(ONE << Select_Blk) |
				(ONE << Case_Blk) |
				(ONE << Where_Then_Blk) |
				(ONE << Where_Else_Blk) |
				(ONE << Where_Else_Mask_Blk) |
                                (ONE << Parallel_Blk) |
				(ONE << SGI_Parallel_Blk) |
                                (ONE << Doall_Blk) |
				(ONE << Wait_Blk) |
				(ONE << SGI_Doacross_Blk) |
				(ONE << SGI_Parallel_Do_Blk) |
                                (ONE << Do_Parallel_Blk) |
				(ONE << SGI_Pdo_Blk) |
                                (ONE << Guard_Blk) |
				(ONE << SGI_Critical_Section_Blk) |
                                (ONE << Parallel_Case_Blk) |
				(ONE << SGI_Psection_Blk) |
				(ONE << SGI_Section_Blk) |
				(ONE << SGI_Single_Process_Blk) |
                                (ONE << Open_Mp_Parallel_Blk) |
                                (ONE << Open_Mp_Do_Blk) |
                                (ONE << Open_Mp_Parallel_Sections_Blk) |
                                (ONE << Open_Mp_Sections_Blk) |
                                (ONE << Open_Mp_Section_Blk) |
                                (ONE << Open_Mp_Single_Blk) |
                                (ONE << Open_Mp_Parallel_Do_Blk) |
                                (ONE << Open_Mp_Master_Blk) |
                                (ONE << Open_Mp_Critical_Blk) |
                                (ONE << Open_Mp_Ordered_Blk) |
                                (ONE << Open_Mp_Workshare_Blk) | /* by jhs, 02/7/18 */
                                (ONE << Open_Mp_Parallel_Workshare_Blk) | /* by jhs, 02/7/18 */
				(ONE << Contains_Blk) |
				(ONE << Interface_Blk) |
				(ONE << Derived_Type_Blk) | 
				(ONE << Enum_Blk)),

#endif /* KEY Bug 14150 */

				};
# undef ONE


/* ************************************************************************** */
/*                               stmt_top_cat                 		      */
/* Label_Def and Construct_Def (from stmt_type_values def) are NOT included   */
/* in this table because they are not user statement types.                   */
/* ************************************************************************** */

stmt_category_type	stmt_top_cat [] = {
				Init_Stmt_Cat,		/* Null_Stmt	      */

				Declaration_Stmt_Cat,	/* Allocatable_Stmt   */
				Declaration_Stmt_Cat,	/* Automatic_Stmt     */
				Declaration_Stmt_Cat,	/* Common_Stmt	      */
				Executable_Stmt_Cat,	/* Contains_Stmt      */
				Declaration_Stmt_Cat,	/* Cpnt_Decl_Stmt     */
				Executable_Stmt_Cat,	/* Data_Stmt	      */
				Declaration_Stmt_Cat,	/* Derived_Type_Stmt  */
				Declaration_Stmt_Cat,	/* Dimension_Stmt     */
				Executable_Stmt_Cat,	/* Directive_Stmt     */
				Declaration_Stmt_Cat,	/* Equivalence_Stmt   */
				Declaration_Stmt_Cat,	/* External_Stmt      */
				Executable_Stmt_Cat,	/* Format_Stmt	      */
				Implicit_Stmt_Cat,	/* Implicit_Stmt      */
				Implicit_None_Stmt_Cat,	/* Implicit_None_Stmt */
				Declaration_Stmt_Cat,	/* Intent_Stmt	      */
				Declaration_Stmt_Cat,	/* Interface_Stmt     */
				Declaration_Stmt_Cat,	/* Intrinsic_Stmt     */
				Sub_Func_Stmt_Cat,	/* Module_Proc_Stmt   */
				Executable_Stmt_Cat,	/* Namelist_Stmt      */
				Declaration_Stmt_Cat,	/* Optional_Stmt      */
				Declaration_Stmt_Cat,	/* Parameter_Stmt     */
				Declaration_Stmt_Cat,	/* Pointer_Stmt	      */
				Declaration_Stmt_Cat,	/* Private_Stmt	      */
				Declaration_Stmt_Cat,	/* Public_Stmt	      */
				Declaration_Stmt_Cat,	/* Save_Stmt	      */
				Declaration_Stmt_Cat,	/* Sequence_Stmt      */
				Declaration_Stmt_Cat,	/* Stmt_Func_Stmt     */
				Declaration_Stmt_Cat,	/* Target_Stmt	      */
                                Declaration_Stmt_Cat,   /* Task_Common_Stmt   */
				Declaration_Stmt_Cat,	/* Type_Decl_Stmt     */
				Use_Stmt_Cat,		/* Use_Stmt	      */

				Init_Stmt_Cat,		/* Blockdata_Stmt     */
				Sub_Func_Stmt_Cat,	/* Elemental_Stmt     */
				Sub_Func_Stmt_Cat,	/* Function_Stmt      */
				Init_Stmt_Cat,		/* Module_Stmt	      */
				Init_Stmt_Cat,		/* Program_Stmt	      */
				Sub_Func_Stmt_Cat,	/* Pure_Stmt          */
				Sub_Func_Stmt_Cat,	/* Recursive_Stmt     */
				Sub_Func_Stmt_Cat,	/* Subroutine_Stmt    */

				Declaration_Stmt_Cat,	/* End_Blockdata_Stmt */
				Executable_Stmt_Cat,	/* End_Do_Stmt	      */
				Executable_Stmt_Cat,	/* End_Function_Stmt  */
				Executable_Stmt_Cat,	/* End_If_Stmt	      */
				Declaration_Stmt_Cat,	/* End_Interface_Stmt */
				Declaration_Stmt_Cat,	/* End_Module_Stmt    */
				Executable_Stmt_Cat,	/* End_Program_Stmt   */
				Executable_Stmt_Cat,	/* End_Select_Stmt    */
				Executable_Stmt_Cat,	/* End_Stmt	      */
				Executable_Stmt_Cat,	/* End_Subroutine     */
				Declaration_Stmt_Cat,	/* End_Type_Stmt      */
				Executable_Stmt_Cat,	/* End_Where_Stmt     */

				Executable_Stmt_Cat,	/* Allocate_Stmt      */
				Executable_Stmt_Cat,	/* Arith_If_Stmt      */
				Executable_Stmt_Cat,	/* Assign_Stmt	      */
				Executable_Stmt_Cat,	/* Assignment_Stmt    */
				Executable_Stmt_Cat,	/* Backspace_Stmt     */
				Executable_Stmt_Cat,	/* Buffer_Stmt	      */
				Executable_Stmt_Cat,	/* Call_Stmt	      */
				Executable_Stmt_Cat,	/* Case_Stmt	      */
				Executable_Stmt_Cat,	/* Close_Stmt	      */
				Executable_Stmt_Cat,	/* Continue_Stmt      */
				Executable_Stmt_Cat,	/* Cycle_Stmt	      */
				Executable_Stmt_Cat,	/* Deallocate_Stmt    */
				Executable_Stmt_Cat,	/* Decode_Stmt	      */
				Executable_Stmt_Cat,	/* Do_Iterative_Stmt  */
				Executable_Stmt_Cat,	/* Do_While_Stmt      */
				Executable_Stmt_Cat,	/* Do_Infinite_Stmt   */
				Executable_Stmt_Cat,	/* Else_Stmt	      */
				Executable_Stmt_Cat,	/* Else_If_Stmt	      */
				Executable_Stmt_Cat,	/* Else_Where_Stmt    */
				Executable_Stmt_Cat,	/* Encode_Stmt	      */
				Executable_Stmt_Cat,	/* Endfile_Stmt	      */
				Executable_Stmt_Cat,	/* Entry_Stmt	      */
				Executable_Stmt_Cat,	/* Exit_Stmt	      */
				Executable_Stmt_Cat,	/* Go_To_Stmt	      */
				Executable_Stmt_Cat,	/* If_Cstrct_Stmt     */
				Executable_Stmt_Cat,	/* If_Stmt	      */
				Executable_Stmt_Cat,	/* Inquire_Stmt	      */
				Executable_Stmt_Cat,	/* Nullify_Stmt	      */
				Executable_Stmt_Cat,	/* Open_Stmt	      */
				Executable_Stmt_Cat,	/* Outmoded_If_Stmt   */
				Executable_Stmt_Cat,	/* Pause_Stmt	      */
				Executable_Stmt_Cat,	/* Print_Stmt	      */
				Executable_Stmt_Cat,	/* Read_Stmt	      */
				Executable_Stmt_Cat,	/* Return_Stmt	      */
				Executable_Stmt_Cat,	/* Rewind_Stmt	      */
				Executable_Stmt_Cat,	/* Select_Stmt	      */
				Executable_Stmt_Cat,	/* Stop_Stmt	      */
				Executable_Stmt_Cat,	/* Then_Stmt	      */
				Executable_Stmt_Cat,	/* Where_Cstrct_Stmt  */
				Executable_Stmt_Cat,	/* Where_Stmt	      */
				Executable_Stmt_Cat,	/* Write_Stmt	      */

                                Declaration_Stmt_Cat,	/* Type_Init_Stmt */

                                Init_Stmt_Cat,		/* Label_Def */
                                Executable_Stmt_Cat,	/* Construct_Def */
                                Init_Stmt_Cat,	  /* Automatic_Base_Calc_Stmt */
                                Init_Stmt_Cat,	  /* Automatic_Base_Size_Stmt */

                                Executable_Stmt_Cat,	/* End_Parallel_Stmt */
                                Executable_Stmt_Cat, /* End_Do_Parallel_Stmt */
                                Executable_Stmt_Cat, /* End_Parallel_Case_Stmt*/
                                Executable_Stmt_Cat, /* Parallel_Case_Stmt */
                                Executable_Stmt_Cat, /* End_Guard_Stmt */
                                Executable_Stmt_Cat, /* Statement_Num_Stmt */
                                Executable_Stmt_Cat, /* SGI_Section_Stmt */
                                Executable_Stmt_Cat, /* SGI_End_Psection_Stmt */
                                Executable_Stmt_Cat, /* SGI_End_Pdo_Stmt */
                                Executable_Stmt_Cat, /* SGI_End_Parallel_Stmt */
                                Executable_Stmt_Cat, 
					/* SGI_End_Critical_Section_Stmt */
                                Executable_Stmt_Cat,
						/* SGI_End_Single_Process_Stmt*/
                                Executable_Stmt_Cat, /* SGI_Region_End_Stmt */

                                Executable_Stmt_Cat, /* Open_MP_Section_Stmt */
                                Executable_Stmt_Cat,
						 /* Open_MP_End_Parallel_Stmt */
                                Executable_Stmt_Cat, /* Open_MP_End_Do_Stmt */
                                Executable_Stmt_Cat,
					 /* Open_MP_End_Parallel_Sections_Stmt*/
                                Executable_Stmt_Cat,
					 	/* Open_MP_End_Sections_Stmt */
                                Executable_Stmt_Cat,
					 	/* Open_MP_End_Section_Stmt */
                                Executable_Stmt_Cat,
					 	/* Open_MP_End_Single_Stmt */
                                Executable_Stmt_Cat,
					 	/*Open_MP_End_Parallel_Do_Stmt*/
                                Executable_Stmt_Cat,
					 	/* Open_MP_End_Master_Stmt */
                                Executable_Stmt_Cat,
					 	/* Open_MP_End_Critical_Stmt */
                                Executable_Stmt_Cat,
						 /* Open_MP_End_Ordered_Stmt */


				Executable_Stmt_Cat,	/* Forall_Cstrct_Stmt */
				Executable_Stmt_Cat,	/* Forall_Stmt	      */
				Executable_Stmt_Cat,    /* End_Forall_Stmt    */
				Executable_Stmt_Cat,   /* Else_Where_Mask_Stmt*/
				Declaration_Stmt_Cat,  /* Volatile_Stmt      */
				Executable_Stmt_Cat,
						/* Open_MP_End_Parallel_Workshare_Stmt */
				Executable_Stmt_Cat,
						/* Open_MP_End_Workshare_Stmt */
#ifdef KEY /* Bug 11741 */
				Import_Stmt_Cat,	/* Import_Stmt        */
#endif /* KEY Bug 11741 */
#ifdef KEY /* Bug 10572 */
				Declaration_Stmt_Cat,	/* Enum_Stmt          */
				Declaration_Stmt_Cat,	/* End_Enum_Stmt      */
				Declaration_Stmt_Cat,	/* Enumerator_Stmt    */
#endif /* KEY Bug 10572 */
#ifdef KEY /* Bug 14150 */
				Declaration_Stmt_Cat,	/* Bind_Stmt	      */
				Declaration_Stmt_Cat	/* Value_Stmt	      */
#endif /* KEY Bug 14150 */
				};


boolean		first_time_tbl_alloc = TRUE;
