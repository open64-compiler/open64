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



/* USMID:  "\n@(#)5.0_pl/headers/fecif.h	5.1	04/29/99 21:22:31\n" */


/******************************************************************************\
|**									     **|
|**			    Static Data Declarations			     **|
|**									     **|
\******************************************************************************/

/* cif_name is defined to have the length (MAX_FILE_NAME_SIZE + 2) to allow   */
/* for space for the ".T" in the event the source input file name uses all    */
/* character positions (so it kinda violates the policy of limiting file      */
/* names to MAX_FILE_NAME_SIZE characters).  Note that cft90, to be           */
/* compatible with cft77, does not force source input file names to have a    */
/* ".f" extension.							      */
/*									      */
/* cft77 uses a much larger value for the max path name size.  If we need to  */
/* use a larger one, we should use the values as explained below.             */
/* PATH_MAX is defined in the Cray sys/param.h file as 1023.                  */
/* PATHSIZE is defined in the Cray sys/param.h file as (PATH_MAX+1).          */
/* MAXPATHLEN is defined in the Sun sys/param.h file as 1024.                 */
/* The path name length allows for an expanded path name of 1023 characters   */
/* plus the null terminator.						      */
/*									      */

 	char	cif_name[MAX_FILE_NAME_SIZE + 2];

        char	cif_tmp_file_name[MAX_FILE_NAME_SIZE];

static	int	cif_derived_type_id;
static	int	cif_file_id;
static	int	cif_symbol_or_scope_id;


 
/******************************************************************************\
|*                              Other CIF data				      *|
\******************************************************************************/

int		 cif_end_unit_column;
int		 cif_end_unit_line;
int		 cif_number_of_struct_ids;
boolean		 cif_pgm_unit_error_recovery;
boolean          get_other_func_rslt_info	= FALSE;
int		 last_msg_file_rec;
char		*orig_cmd_line;
boolean		 skip_struct_base;


/* mapped_stmt_type is used to build the Statement Type record.  cif_stmt_type*/
/* is defined in globals.h because a number of procedures must be able to     */
/* call cif_stmt_type_rec when the exact type of statement has been	      */
/* determined.								      */

cif_stmt_type	mapped_stmt_type[] =
		       {CIF_Stmt_Type_Error,

                        CIF_Allocatable_Stmt,		/* Allocatable_Stmt   */
                       	CIF_Automatic_Stmt,		/* Automatic_Stmt     */
                       	CIF_Common_Stmt,		/* Common_Stmt        */
                       	CIF_Contains_Stmt,		/* Contains_Stmt      */
                       	CIF_Stmt_Type_Error,		/* Cpnt_Decl_Stmt     */
                       	CIF_Data_Stmt,			/* Data_Stmt          */
                       	CIF_Type_Stmt,			/* Derived_Type_Stmt  */
                       	CIF_Dimension_Stmt,		/* Dimension_Stmt     */
                        CIF_Not_Exact,			/* Directive_Stmt     */
                        CIF_Equivalence_Stmt,		/* Equivalence_Stmt   */
                        CIF_External_Stmt,		/* External_Stmt      */
                        CIF_Format_Stmt,		/* Format_Stmt	      */
                        CIF_Not_Exact,			/* Implicit_Stmt      */
                        CIF_Not_Exact,			/* Implicit_None_Stmt */
                        CIF_Not_Exact,			/* Intent_Stmt	      */
                        CIF_Not_Exact,			/* Interface_Stmt     */
                        CIF_Intrinsic_Stmt,		/* Intrinsic_Stmt     */
                        CIF_Not_Exact,			/* Module_Proc_Stmt   */
                        CIF_Namelist_Stmt,		/* Namelist_Stmt      */
                        CIF_Optional_Stmt,		/* Optional_Stmt      */
                        CIF_Parameter_Stmt,		/* Parameter_Stmt     */
                        CIF_Pointer_Stmt,		/* Pointer_Stmt       */
                        CIF_Private_Stmt,		/* Private_Stmt       */
                        CIF_Public_Stmt,		/* Public_Stmt        */
                        CIF_Save_Stmt, 			/* Save_Stmt	      */
                        CIF_Sequence_Stmt,		/* Sequence_Stmt      */
                        CIF_Statement_Function_Stmt,	/* Stmt_Func_Stmt     */
                        CIF_Target_Stmt,		/* Target_Stmt        */
                        CIF_Task_Common_Stmt,		/* Task_Common_Stmt   */
                        CIF_Not_Exact,		   	/* Type_Decl_Stmt     */
                        CIF_Use_Stmt,			/* Use_Stmt	      */

                        CIF_Block_Data_Stmt,		/* Blockdata_Stmt     */
                        CIF_Elemental_Stmt,		/* Elemental_Stmt     */
                        CIF_Function_Stmt,		/* Function_Stmt      */
                        CIF_Not_Exact,			/* Module_Stmt	      */
                        CIF_Program_Stmt,		/* Program_Stmt	      */
                        CIF_Pure_Stmt,			/* Pure_Stmt	      */
                        CIF_Not_Exact,			/* Recursive_Stmt     */
                        CIF_Subroutine_Stmt,		/* Subroutine_Stmt    */

                        CIF_End_Block_Data_Stmt,	/* End_Blockdata_Stmt */
                        CIF_End_Do_Stmt,		/* End_Do_Stmt	      */
                        CIF_End_Function_Stmt,		/* End_Function_Stmt  */
                        CIF_End_If_Stmt,		/* End_If_Stmt        */
                        CIF_End_Interface_Stmt,		/* End_Interface_Stmt */
                        CIF_End_Module_Stmt,		/* End_Module_Stmt    */
                        CIF_End_Program_Stmt,		/* End_Program_Stmt   */
                        CIF_End_Select_Stmt,		/* End_Select_Stmt    */
                        CIF_Not_Exact,			/* End_Stmt	      */
                        CIF_End_Subroutine_Stmt,	/* End_Subroutine_Stmt*/
                        CIF_End_Type_Stmt,		/* End_Type_Stmt      */
                        CIF_End_Where_Stmt,		/* End_Where_Stmt     */

                        CIF_Allocate_Stmt,		/* Allocate_Stmt      */
                        CIF_If_Arithmetic_Stmt,		/* Arith_If_Stmt      */
                        CIF_Assign_Stmt,		/* Assign_Stmt        */
                        CIF_Not_Exact,			/* Assignment_Stmt    */
                        CIF_Backspace_Stmt,		/* Backspace_Stmt     */
                        CIF_Not_Exact,			/* Buffer_Stmt        */
                        CIF_Call_Stmt,			/* Call_Stmt	      */
                        CIF_Not_Exact,			/* Case_Stmt	      */
                        CIF_Close_Stmt,			/* Close_Stmt	      */
                        CIF_Continue_Stmt,		/* Continue_Stmt      */
                        CIF_Cycle_Stmt,			/* Cycle_Stmt         */
                        CIF_Deallocate_Stmt,		/* Deallocate_Stmt    */
                        CIF_Decode_Stmt,		/* Decode_Stmt	      */
                        CIF_Not_Exact,			/* Do_Iterative_Stmt  */
                        CIF_Not_Exact,			/* Do_While_Stmt      */
   			CIF_Not_Exact,			/* Do_Infinite_Stmt   */
                        CIF_Not_Exact,			/* Else_Stmt	      */
                        CIF_Else_If_Stmt,		/* Else_If_Stmt	      */
                        CIF_Not_Exact,			/* Else_Where_Stmt    */
                        CIF_Encode_Stmt,		/* Encode_Stmt        */
                        CIF_Endfile_Stmt,		/* Endfile_Stmt       */
                        CIF_Entry_Stmt,			/* Entry_Stmt         */
                        CIF_Exit_Stmt,			/* Exit_Stmt          */
                        CIF_Not_Exact,			/* Goto_Stmt          */
                        CIF_Not_Exact,			/* If_Cstrct_Stmt     */
                        CIF_Not_Exact,			/* If_Stmt	      */
                        CIF_Inquire_Stmt,		/* Inquire_Stmt       */
                        CIF_Nullify_Stmt,		/* Nullify_Stmt       */
                        CIF_Open_Stmt,			/* Open_Stmt	      */
                        CIF_Not_Exact,			/* Outmoded_If_Stmt   */
                        CIF_Pause_Stmt,			/* Pause_Stmt         */
                        CIF_Print_Stmt,			/* Print_Stmt	      */
                        CIF_Read_Stmt,			/* Read_Stmt	      */
                        CIF_Return_Stmt,		/* Return_Stmt        */
                        CIF_Rewind_Stmt,		/* Rewind_Stmt        */
                        CIF_Select_Case_Stmt,		/* Select_Stmt        */
                        CIF_Stop_Stmt,			/* Stop_Stmt	      */
                        CIF_Stmt_Type_Error,		/* Then_Stmt	      */
                        CIF_Not_Exact,			/* Where_Cstrct_Stmt  */
                        CIF_Not_Exact,			/* Where_Stmt         */
                        CIF_Write_Stmt,			/* Write_Stmt         */
                        CIF_Stmt_Type_Error,		/* Type_Init_Stmt     */

                        CIF_Stmt_Type_Error,		/* Label_Def          */
                        CIF_Stmt_Type_Error,		/* Construct_Def      */

                        CIF_Stmt_Type_Error,      /* Automatic_Base_Calc_Stmt */
                        CIF_Stmt_Type_Error,      /* Automatic_Base_Size_Stmt */

                        CIF_Not_Exact,            /* End_Parallel_Stmt        */
                        CIF_Not_Exact,            /* End_Do_Parallel_Stmt     */
                        CIF_Not_Exact,            /* End_Parallel_Case_Stmt   */
                        CIF_Not_Exact,            /* Parallel_Case_Stmt       */
                        CIF_Not_Exact,            /* End_Guard_Stmt           */
                        CIF_Not_Exact,            /* Statement_Num_Stmt       */
                        CIF_Not_Exact,            /* SGI_Section_Stmt         */
                        CIF_Not_Exact,            /* SGI_End_Psection_Stmt    */
                        CIF_Not_Exact,            /* SGI_End_Pdo_Stmt         */
                        CIF_Not_Exact,            /* SGI_End_Parallel_Stmt    */
                        CIF_Not_Exact,       /* SGI_End_Critical_Section_Stmt */
                        CIF_Not_Exact,       /* SGI_End_Single_Process_Stmt   */
                        CIF_Not_Exact,            /* SGI_Region_End_Stmt      */
                        CIF_Not_Exact,       /* Open_MP_Section_Stmt */
                        CIF_Not_Exact,       /* Open_MP_End_Parallel_Stmt */
                        CIF_Not_Exact,       /* Open_MP_End_Do_Stmt */
                        CIF_Not_Exact,  /* Open_MP_End_Parallel_Sections_Stmt */
                        CIF_Not_Exact,       /* Open_MP_End_Sections_Stmt */
                        CIF_Not_Exact,       /* Open_MP_End_Section_Stmt */
                        CIF_Not_Exact,       /* Open_MP_End_Single_Stmt */
                        CIF_Not_Exact,       /* Open_MP_End_Parallel_Do_Stmt */
                        CIF_Not_Exact,       /* Open_MP_End_Master_Stmt */
                        CIF_Not_Exact,       /* Open_MP_End_Critical_Stmt */
                        CIF_Not_Exact,       /* Open_MP_End_Ordered_Stmt */

			CIF_Forall_Stmt,      /* Forall_Stmt */
			CIF_Forall_Construct, /* Forall_Construct_Stmt */
                        CIF_End_Forall_Stmt,  /* End_Forall_Stmt    */
                        CIF_Not_Exact,		/* Else_Where_Mask_Stmt    */

			CIF_Not_Exact,		/* Volatile_Stmt */
			CIF_Not_Exact,		/* Open_MP_End_Parallel_Workshare_Stmt */
			CIF_Not_Exact		/* Open_MP_End_Workshare_Stmt */
                       };


/******************************************************************************\
|*                      Globally accessible objects.                          *|
\******************************************************************************/

/* Get ahold of group_code and msg_sys from messages.h.  Don't make them      */
/* completely global (by putting them in globals.h because only messages.c    */
/* and cif.c need to know about them and msg_sys needs a system include file  */
/* that we don't need to drag in everywhere).				      */

extern  char		group_code[];
extern  nl_catd		msg_sys;


/* tmp_msg_file_error is in messages.c.					      */

extern void     tmp_msg_file_error (void);


/* Directive string is declared in main.h */

extern	char	*directive_str[];
extern	char	*dir_mic_str[];
