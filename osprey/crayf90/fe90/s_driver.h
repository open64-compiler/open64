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



/* USMID:  "\n@(#)5.0_pl/headers/s_driver.h	5.1	04/29/99 21:22:31\n" */


/*******************************************/
/* Globals used during the semantics phase */
/*******************************************/

		char	*pgm_unit_str[]		= {"EXTERNAL",
						   "ERROR",
						   "ERROR",
						   "FUNCTION",
						   "SUBROUTINE",
						   "PROGRAM",
						   "BLOCKDATA",
						   "MODULE"};


/******************************************************************************/
/**                        stmt_semantics  TABLE                              */
/**                                                           	              */
/** This table defines an array-of-pointers-to-functions and is indexed by an */
/** object of type stmt_type_type which is the reason for the following       */
/** dependency:		                                                      */
/**                                                                           */
/** THIS TABLE IS ORDER/ADDITION/DELETION DEPENDENT ON THE stmt_type_values   */
/** enumeration defined in globals.h                                          */
/******************************************************************************/
     
           void    (*stmt_semantics[]) () = {
                          illegal_stmt_type,            /* Illegal stmt type  */
           
                          no_semantics_routine,		/* Allocatable_Stmt   */
                          no_semantics_routine,		/* Automatic_Stmt     */
                          no_semantics_routine,         /* Common_Stmt        */
                          no_semantics_routine,         /* Contains_Stmt      */
                          no_semantics_routine,         /* Cpnt_Decl_Stmt     */
                          data_stmt_semantics,          /* Data_Stmt          */
                          no_semantics_routine,         /* Derived_Type_Stmt  */
                          no_semantics_routine,         /* Dimension_Stmt     */
                          directive_stmt_semantics,     /* Directive_Stmt     */
                          no_semantics_routine,         /* Equivalence_Stmt   */
                          no_semantics_routine,         /* External_Stmt      */
                          no_semantics_routine,         /* Format_Stmt        */
                          no_semantics_routine,         /* Implicit_Stmt      */
                          no_semantics_routine,         /* Implicit_None_Stmt */
                          no_semantics_routine,         /* Intent_Stmt        */
                          no_semantics_routine,         /* Interface_Stmt     */
                          no_semantics_routine,         /* Intrinsic_Stmt     */
                          no_semantics_routine,         /* Module_Proc_Stmt   */
                          no_semantics_routine,         /* Namelist_Stmt      */
                          no_semantics_routine,         /* Optional_Stmt      */
                          no_semantics_routine,         /* Parameter_Stmt     */
                          no_semantics_routine,         /* Pointer_Stmt       */
                          no_semantics_routine,         /* Private_Stmt       */
                          no_semantics_routine,         /* Public_Stmt        */
                          no_semantics_routine,         /* Save_Stmt          */
                          no_semantics_routine,         /* Sequence_Stmt      */
                          no_semantics_routine,         /* Stmt_Func_Stmt     */
                          no_semantics_routine,         /* Target_Stmt        */
                          no_semantics_routine,         /* Task_Common_Stmt   */
                          no_semantics_routine,         /* Type_Decl_Stmt     */

                          no_semantics_routine,         /* Use_Stmt           */

                          blockdata_stmt_semantics,     /* Blockdata_Stmt     */
                          no_semantics_routine,         /* Elemental_Stmt     */
                          function_stmt_semantics,      /* Function_Stmt      */
                          module_stmt_semantics,        /* Module_Stmt        */
                          program_stmt_semantics,       /* Program_Stmt       */
                          no_semantics_routine,         /* Pure_Stmt	      */
                          no_semantics_routine,         /* Recursive_Stmt     */
                          subroutine_stmt_semantics,    /* Subroutine_Stmt    */
           
                          no_semantics_routine,         /* End_Blockdata_Stmt */
                          no_semantics_routine,         /* End_Do_Stmt        */
                          end_function_semantics,       /* End_Function_Stmt  */

#ifdef _HIGH_LEVEL_IF_FORM
                          no_semantics_routine,         /* End_If_Stmt        */
#else
                          end_if_semantics,             /* End_If_Stmt        */
#endif
                          no_semantics_routine,		/* End_Interface_Stmt */
                          no_semantics_routine,         /* End_Module_Stmt    */
                          end_stmt_semantics,           /* End_Program_Stmt   */
                          end_select_semantics,         /* End_Select_Stmt    */
                          end_stmt_semantics,           /* End_Stmt           */
                          end_subroutine_semantics,     /* End_Subroutine_Stmt*/
                          no_semantics_routine,		/* End_Type_Stmt      */
                          end_where_semantics,          /* End_Where_Stmt     */
           
                          allocate_stmt_semantics,      /* Allocate_Stmt      */
			  arith_if_stmt_semantics,      /* Arith_If_Stmt      */
                          assign_stmt_semantics,        /* Assign_Stmt        */
                          assignment_stmt_semantics,    /* Assignment_Stmt    */
                          backspace_stmt_semantics,     /* Backspace_Stmt     */
                          buffer_stmt_semantics,        /* Buffer_Stmt        */
                          call_stmt_semantics,          /* Call_Stmt          */
                          case_stmt_semantics,          /* Case_Stmt          */
                          close_stmt_semantics,         /* Close_Stmt         */
                          continue_stmt_semantics,      /* Continue_Stmt      */
                          no_semantics_routine,         /* Cycle_Stmt         */
                          deallocate_stmt_semantics,    /* Deallocate_Stmt    */
                          encode_decode_stmt_semantics, /* Decode_Stmt        */
                          do_stmt_semantics,            /* Do_Iterative_Stmt  */
                          do_stmt_semantics,            /* Do_While_Stmt      */
                          do_stmt_semantics,            /* Do_Infinite_Stmt   */
                          else_stmt_semantics,          /* Else_Stmt          */
                          else_stmt_semantics,          /* Else_If_Stmt       */
                          else_stmt_semantics,          /* Else_Where_Stmt    */
                          encode_decode_stmt_semantics, /* Encode_Stmt        */
                          endfile_stmt_semantics,       /* Endfile_Stmt       */
                          entry_stmt_semantics,         /* Entry_Stmt         */
                          no_semantics_routine,         /* Exit_Stmt          */
                          goto_stmt_semantics,          /* Goto_Stmt          */
                          if_stmt_semantics,            /* If_Cstrct_Stmt     */
                          if_stmt_semantics,            /* If_Stmt            */
                          inquire_stmt_semantics,       /* Inquire_Stmt       */
                          nullify_stmt_semantics,       /* Nullify_Stmt       */
                          open_stmt_semantics,          /* Open_Stmt          */
			  outmoded_if_stmt_semantics,   /* Outmoded_If_Stmt   */
                          stop_pause_stmt_semantics,    /* Pause_Stmt         */
                          print_stmt_semantics,         /* Print_Stmt         */
                          read_stmt_semantics,          /* Read_Stmt          */
                          return_stmt_semantics,        /* Return_Stmt        */
                          rewind_stmt_semantics,        /* Rewind_Stmt        */
                          select_stmt_semantics,        /* Select_Stmt        */
                          stop_pause_stmt_semantics,    /* Stop_Stmt          */
                          then_stmt_semantics,          /* Then_Stmt          */
                          where_stmt_semantics,         /* Where_Cstrct_Stmt  */
                          where_stmt_semantics,         /* Where_Stmt         */
                          write_stmt_semantics,         /* Write_Stmt         */
			  type_init_semantics,          /* Type_Init_Stmt     */

   			  label_def_stmt_semantics,     /* Label_Def          */
			  no_semantics_routine,         /* Construct_Def      */
   			  no_semantics_routine,         /* Automatic_Base_Calc*/
   			  no_semantics_routine,         /* Automatic_Base_Size*/
                          directive_stmt_semantics,     /* End_Parallel_Stmt  */
                          directive_stmt_semantics, /* End_Do_Parallel_Stmt   */
                          directive_stmt_semantics, /* End_Parallel_Case_Stmt */
                          directive_stmt_semantics,     /* Parallel_Case_Stmt */
                          directive_stmt_semantics,     /* End_Guard_Stmt     */
			  no_semantics_routine, 	/* Statement_Num_Stmt */
                          directive_stmt_semantics, /* SGI_Section_Stmt */
                          directive_stmt_semantics, /* SGI_End_Psection_Stmt */
                          directive_stmt_semantics, /* SGI_End_Pdo_Stmt */
                          directive_stmt_semantics, /* SGI_End_Parallel_Stmt */
                          directive_stmt_semantics, 
                                         /* SGI_End_Critical_Section_Stmt */
                          directive_stmt_semantics, 
                                         /* SGI_End_Single_Process_Stmt */
                          directive_stmt_semantics, /* SGI_Region_End_Stmt */
                          directive_stmt_semantics,
                                 	/* Open_MP_Section_Stmt */
                          directive_stmt_semantics,
                                 	/* Open_MP_End_Parallel_Stmt */
                          directive_stmt_semantics,
                                 	/* Open_MP_End_Do_Stmt */
                          directive_stmt_semantics,
                                 	/* Open_MP_End_Parallel_Sections_Stmt */
                          directive_stmt_semantics,
                                 	/* Open_MP_End_Sections_Stmt */
                          directive_stmt_semantics,
                                 	/* Open_MP_End_Section_Stmt */
                          directive_stmt_semantics,
                                 	/* Open_MP_End_Single_Stmt */
                          directive_stmt_semantics,
                                 	/* Open_MP_End_Parallel_Do_Stmt */
                          directive_stmt_semantics,
                                 	/* Open_MP_End_Master_Stmt */
                          directive_stmt_semantics,
                                 	/* Open_MP_End_Critical_Stmt */
                          directive_stmt_semantics,
                                 	/* Open_MP_End_Ordered_Stmt */
                          forall_semantics,             /* Forall_Cstrct_Stmt */
                          forall_semantics,             /* Forall_Stmt        */
                          end_forall_semantics,         /* End_Forall_Stmt    */
                          else_stmt_semantics,     /* Else_Where_Mask_Stmt    */
                          no_semantics_routine,		/* Volatile_Stmt      */
			  directive_stmt_semantics,
			  		/* Open_MP_End_Parallel_Workshare_Stmt */
			  directive_stmt_semantics,
				  	/* Open_MP_End_Workshare_Stmt */
#ifdef KEY /* Bug 11741 */
                          no_semantics_routine,        /* Import_Stmt        */
#endif /* KEY Bug 11741 */
#ifdef KEY /* Bug 10572 */
                          no_semantics_routine,        /* Enum_Stmt           */
                          no_semantics_routine,        /* End_Enum_Stmt       */
                          no_semantics_routine,        /* Enumerator_Stmt     */
#endif /* KEY Bug 10572 */
#ifdef KEY /* Bug 14150 */
                          no_semantics_routine,         /* Bind_Stmt          */
                          no_semantics_routine          /* Value_Stmt         */
#endif /* KEY Bug 14150 */
			};

/*********************************************************\
|* stmt_tmp_tbl points to lists of tmps that are reused. *|
|* -1 => never reused.                                   *|
\*********************************************************/

stmt_tmp_tbl_type       stmt_tmp_tbl[Num_Linear_Types];

stmt_tmp_tbl_type       init_stmt_tmp_tbl[Num_Linear_Types] = 
				{
        /* Err_Res    */        	-1, -1,
					{-1, -1, -1, -1, -1, -1, -1, -1},
					{-1, -1, -1, -1, -1, -1, -1, -1},

        /* Short_Char_Const */  	-1, -1,
					{-1, -1, -1, -1, -1, -1, -1, -1},
					{-1, -1, -1, -1, -1, -1, -1, -1},

        /* Short_Typeless_Const */      -1, -1,
					{-1, -1, -1, -1, -1, -1, -1, -1},
					{-1, -1, -1, -1, -1, -1, -1, -1},

        /* Typeless_1       */    	-1, -1,
					{-1, -1, -1, -1, -1, -1, -1, -1},
					{-1, -1, -1, -1, -1, -1, -1, -1},

        /* Typeless_2       */    	-1, -1,
					{-1, -1, -1, -1, -1, -1, -1, -1},
					{-1, -1, -1, -1, -1, -1, -1, -1},

        /* Typeless_4       */    	-1, -1,
					{-1, -1, -1, -1, -1, -1, -1, -1},
					{-1, -1, -1, -1, -1, -1, -1, -1},

        /* Typeless_8  	    */    	-1, -1,
					{-1, -1, -1, -1, -1, -1, -1, -1},
					{-1, -1, -1, -1, -1, -1, -1, -1},

        /* Long_Typeless    */     	-1, -1,
					{-1, -1, -1, -1, -1, -1, -1, -1},
					{-1, -1, -1, -1, -1, -1, -1, -1},

        /* Integer_1        */    	0, 0,
					{0, 0, 0, 0, 0, 0, 0, 0},
					{0, 0, 0, 0, 0, 0, 0, 0},

        /* Integer_2        */    	0, 0,
					{0, 0, 0, 0, 0, 0, 0, 0},
					{0, 0, 0, 0, 0, 0, 0, 0},

        /* Integer_4        */    	0, 0,
					{0, 0, 0, 0, 0, 0, 0, 0},
					{0, 0, 0, 0, 0, 0, 0, 0},

        /* Integer_8        */    	0, 0,
					{0, 0, 0, 0, 0, 0, 0, 0},
					{0, 0, 0, 0, 0, 0, 0, 0},

        /* Real_4           */        	0, 0,
					{0, 0, 0, 0, 0, 0, 0, 0},
					{0, 0, 0, 0, 0, 0, 0, 0},

        /* Real_8           */        	0, 0,
					{0, 0, 0, 0, 0, 0, 0, 0},
					{0, 0, 0, 0, 0, 0, 0, 0},

        /* Real_16          */        	0, 0,
					{0, 0, 0, 0, 0, 0, 0, 0},
					{0, 0, 0, 0, 0, 0, 0, 0},

        /* Complex_4        */     	0, 0,
					{0, 0, 0, 0, 0, 0, 0, 0},
					{0, 0, 0, 0, 0, 0, 0, 0},

        /* Complex_8        */     	0, 0,
					{0, 0, 0, 0, 0, 0, 0, 0},
					{0, 0, 0, 0, 0, 0, 0, 0},

        /* Complex_16       */     	0, 0,
					{0, 0, 0, 0, 0, 0, 0, 0},
					{0, 0, 0, 0, 0, 0, 0, 0},

        /* CRI_Ptr_8        */    	-1, -1,
					{-1, -1, -1, -1, -1, -1, -1, -1},
					{-1, -1, -1, -1, -1, -1, -1, -1},

        /* Logical_1        */  	0, 0,
					{0, 0, 0, 0, 0, 0, 0, 0},
					{0, 0, 0, 0, 0, 0, 0, 0},

        /* Logical_2        */  	0, 0,
					{0, 0, 0, 0, 0, 0, 0, 0},
					{0, 0, 0, 0, 0, 0, 0, 0},

        /* Logical_4        */  	0, 0,
					{0, 0, 0, 0, 0, 0, 0, 0},
					{0, 0, 0, 0, 0, 0, 0, 0},

        /* Logical_8        */  	0, 0,
					{0, 0, 0, 0, 0, 0, 0, 0},
					{0, 0, 0, 0, 0, 0, 0, 0},

        /* Character_1      */ 		-1, -1,
					{-1, -1, -1, -1, -1, -1, -1, -1},
					{-1, -1, -1, -1, -1, -1, -1, -1},

        /* Character_2      */ 		-1, -1,
					{-1, -1, -1, -1, -1, -1, -1, -1},
					{-1, -1, -1, -1, -1, -1, -1, -1},

        /* Character_4      */ 		-1, -1,
					{-1, -1, -1, -1, -1, -1, -1, -1},
					{-1, -1, -1, -1, -1, -1, -1, -1},

        /* CRI_Ch_Ptr_8     */   	-1, -1,
					{-1, -1, -1, -1, -1, -1, -1, -1},
					{-1, -1, -1, -1, -1, -1, -1, -1},

        /* Structure_Type   */ 		-1, -1,
					{-1, -1, -1, -1, -1, -1, -1, -1},
					{-1, -1, -1, -1, -1, -1, -1, -1},

        /* CRI_Parcel_Ptr_8     */   	-1, -1,
					{-1, -1, -1, -1, -1, -1, -1, -1},
					{-1, -1, -1, -1, -1, -1, -1, -1}
				};
