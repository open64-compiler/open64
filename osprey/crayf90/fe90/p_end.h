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



/* USMID:  "\n@(#)5.0_pl/headers/p_end.h	5.1	04/29/99 21:22:31\n" */

boolean	end_of_contains;

/*****************************************************************\
|* function prototypes of static functions declared in this file *|
\*****************************************************************/

static void end_blockdata (boolean);
static void end_contains (boolean);
static void end_do_blk (boolean);
static void end_forall_blk (boolean);
static void end_function (boolean);
static void end_if_blk (boolean);
static void end_interface_blk (boolean);
#ifdef KEY /* Bug 10572 */
static void end_enum_blk (boolean);
#endif /* KEY Bug 10572 */
static void end_interface_body (boolean);
static void end_internal_err (boolean);
static void end_internal_proc (boolean);
static void end_module (boolean);
static void end_module_proc (boolean);
static void end_program_unit (boolean);
static void end_select_blk (boolean);
static void end_subroutine (boolean);
static void end_type_blk (boolean);
static void end_where_blk (boolean);


/******************************************************************************/
/*			end_blocks TABLE				      */
/*									      */
/* This table defines an array-of-pointers-to-functions-returning boolean     */
/* and is indexed by an object of type blk_cntxt_type which is the reason for */
/* the following dependency:						      */
/*									      */
/* THIS TABLE IS ORDER/ADDITION/DELETION DEPENDENT ON THE blk_cntxt_values    */
/* enumeration defined in p_globals.h					      */
/******************************************************************************/

void		(*end_blocks[]) () = {
		/* Unknown_Blk		*/	end_internal_err,
		/* Blockdata_Blk	*/	end_blockdata,
		/* Module_Blk		*/	end_module,
		/* Program_Blk		*/	end_program_unit,
		/* Function_Blk		*/	end_function,
		/* Subroutine_Blk	*/	end_subroutine,
		/* Internal_Blk		*/	end_internal_proc,
		/* Module_Proc_Blk	*/	end_module_proc,
		/* Interface_Body_Blk	*/	end_interface_body,
		/* Do_Blk		*/	end_do_blk,
		/* Forall_Blk		*/	end_forall_blk,
		/* If_Blk		*/	end_if_blk,
		/* If_Then_Blk		*/	end_if_blk,
		/* If_Else_If_Blk	*/	end_if_blk,
		/* If_Else_Blk		*/	end_if_blk,
		/* Select_Blk		*/	end_select_blk,
		/* Case_Blk		*/	end_select_blk,
		/* Where_Then_Blk	*/	end_where_blk,
		/* Where_Else_Blk	*/	end_where_blk,
		/* Where_Else_Mask_Blk	*/	end_where_blk,
		/* Parallel_Blk		*/	end_parallel_blk,
		/* Doall_Blk		*/	end_doall_blk,
		/* Do_Parallel_Blk	*/	end_do_parallel_blk,
		/* Guard_Blk		*/	end_guard_blk,
		/* Parallel_Case_Blk	*/	end_parallel_case_blk,
		/* Wait_Blk		*/	end_wait_blk,
		/* SGI_Doacross_Blk	*/      end_doacross_blk,
		/* SGI_Psection_Blk	*/      end_psection_blk,
		/* SGI_Section_Blk	*/      end_psection_blk,
		/* SGI_Pdo_Blk		*/      end_pdo_blk,
		/* SGI_Parallel_Do_Blk	*/      end_doacross_blk,
		/* SGI_Parallel_Blk	*/      end_SGI_parallel_blk,
		/* SGI_Critical_Section_Blk */  end_critical_section_blk,
		/* SGI_Single_Process_Blk */    end_single_process_blk,
		/* SGI_Region_Blk	*/      end_region_blk,
		/* Open_Mp_Parallel_Blk	*/	end_open_mp_parallel_blk,
		/* Open_Mp_Do_Blk	*/	end_open_mp_do_blk,
		/* Open_Mp_Parallel_Sections_Blk*/
                                          end_open_mp_parallel_sections_blk,
		/* Open_Mp_Sections_Blk	*/	end_open_mp_sections_blk,
		/* Open_Mp_Section_Blk	*/	end_open_mp_section_blk,
		/* Open_Mp_Single_Blk	*/	end_open_mp_single_blk,
		/* Open_Mp_Parallel_Do_Blk*/	end_open_mp_parallel_do_blk,
		/* Open_Mp_Master_Blk	*/	end_open_mp_master_blk,
		/* Open_Mp_Critical_Blk	*/	end_open_mp_critical_blk,
		/* Open_Mp_Ordered_Blk	*/	end_open_mp_ordered_blk,
		/* Open_Mp_Workshare_Blk */	end_open_mp_workshare_blk,
		/* Open_Mp_Parallel_Workshare_Blk */
						end_open_mp_parallel_workshare_blk,
		/* Contains_Blk		*/	end_contains,
		/* Interface_Blk	*/	end_interface_blk,
		/* Derived_Type_Blk	*/	end_type_blk,
#ifdef KEY /* Bug 10572 */
		/* Enum_Blk		*/	end_enum_blk
#endif /* KEY Bug 10572 */
		};
