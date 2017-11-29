/*
 * Copyright (C) 2012 Advanced Micro Devices, Inc.  All Rights Reserved.
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



/* USMID:  "\n@(#)5.0_pl/headers/debug.h	5.11	10/26/99 15:36:57\n" */


static FILE	*trace_file;
static char	 trace_file_name[]	= "cft90_trace";
static int	 trace_indent		= 0;
static int const trace_indent_len	= 2;	/* decled this way until      */
						/* debug.c includes debug.m   */
FILE		*debug_file;

token_type	 fake_token;

char		*access_str[]		= {
			"Public",
			"Private"
		};
char		*align_str[]		= {
			"No_Align",
			"Algn_Bit",
			"Align_8",
			"Align_16",
			"Align_32",
			"Align_64",
			"Double",
			"Algn_128"
		};

char		*atd_class_str[]	= {
			"Atd_Unknown",
			"Variable",
			"Dummy_Argument",
			"Function_Result",
			"Compiler_Tmp",
			"Cray_Pointee",
			"Struct_Component",
			"Constant"
		};

char		*atl_class_str[]	= {
			"Lbl_Unknown",
			"Lbl_User",
			"Lbl_Format",
			"Lbl_Internal",
			"Lbl_Debug",
			"Lbl_Construct"
		};

char		*atl_debug_class_str[]	= {
			"Ldbg_None",
			"Ldbg_Stmt_Lbl",
			"Ldbg_Exit_Point",
			"Ldbg_End_Prologue",
			"Ldbg_Start_Epilogue",
			"Ldbg_Construct_Name",
			"Ldbg_Loop_Lbl",
			"Ldbg_User_Lbl"
		};


char		*atp_pgm_unit_str[]	= {
			"Pgm_Unknown",
			"ERROR",
			"ERROR",
			"Function",
			"Subroutine",
			"Program",
			"Blockdata",
			"Module"
		};

char		*atp_proc_str[]	= {
			"Unknown_Proc",
			"Extern_Proc",
			"Intrin_Proc",
			"Dummy_Proc",
			"Intern_Proc",
			"Imported_Proc",
			"Module_Proc"
		};

char		*bd_array_class_str[]	= {
			"Unknown_Array",
			"Explicit_Shape",
			"Assumed_Size",
			"Deferred_Shape",
			"Assumed_Shape"
		};

char		*bd_array_size_str[]	= {
			"Unknown_Size",
			"Constant_Size",
			"Var_Len_Array",
			"Symbolic_Constant_Size"
		};

char		*blk_struct_str[]	= {

	"Unknown_Blk",			/* Unknown_Blk			*/
	"Blockdata_Blk",		/* Blockdata_Blk		*/
	"Module_Blk",	 		/* Module_Blk	   		*/
	"Main_Program_Blk",		/* Main_Program_Blk		*/
	"Function_Blk",	 		/* Function_Blk	   		*/
	"Subroutine_Blk",		/* Subroutine_Blk		*/
	"Internal_Blk",			/* Internal_Blk	   		*/
	"Module_Proc_Blk",		/* Module_Proc_Blk		*/
	"Interface_Body_Blk",		/* Interface_Body_Blk		*/
	"Do_Blk",			/* Do_Blk			*/
	"Forall_Blk",			/* Forall_Blk           	*/
	"If_Blk",	 		/* If_Blk	   		*/
	"If_Then_Blk",	 		/* If_Then_Blk	   		*/
	"If_Else_If_Blk", 		/* If_Else_If_Blk   		*/
	"If_Else_Blk",	 		/* If_Else_Blk	   		*/
	"Select_Blk",	 		/* Select_Blk	   		*/
	"Case_Blk",	 		/* Case_Blk	   		*/
	"Where_Then_Blk",		/* Where_Then_Blk 		*/
	"Where_Else_Blk",		/* Where_Else_Blk 		*/
	"Where_Else_Mask_Blk",		/* Where_Else_Mask_Blk 		*/
	"Parallel_Blk", 		/* Parallel_Blk			*/
	"Doall_Blk",			/* Doall_Blk			*/
	"Do_Parallel_Blk",		/* Do_Parallel_Blk		*/
	"Guard_Blk",			/* Guard_Blk			*/
	"Parallel_Case_Blk",		/* Parallel_Case_Blk		*/
	"Wait_Blk",			/* Wait_Blk			*/
	"SGI_Doacross_Blk",		/* SGI_Doacross_Blk		*/
	"SGI_Psection_Blk",		/* SGI_Psection_Blk		*/
	"SGI_Section_Blk",		/* SGI_Section_Blk		*/
	"SGI_Pdo_Blk",			/* SGI_Pdo_Blk			*/
	"SGI_Parallel_Do_Blk",		/* SGI_Parallel_Do_Blk		*/
	"SGI_Parallel_Blk",		/* SGI_Parallel_Blk		*/
	"SGI_Critical_Section_Blk",	/* SGI_Critical_Section_Blk	*/
	"SGI_Single_Process_Blk",	/* SGI_Single_Process_Blk	*/
	"SGI_Region_Blk",		/* SGI_Region_Blk		*/
	"Open_Mp_Parallel_Blk",		/* Open_Mp_Parallel_Blk		*/
	"Open_Mp_Do_Blk",		/* Open_Mp_Do_Blk		*/
	"Open_Mp_Parallel_Sections_Blk",/* Open_Mp_Parallel_Section_Blk	*/
	"Open_Mp_Sections_Blk",		/* Open_Mp_Sections_Blk		*/
	"Open_Mp_Section_Blk",		/* Open_Mp_Section_Blk		*/
	"Open_Mp_Single_Blk",		/* Open_Mp_Single_Blk		*/
	"Open_Mp_Parallel_Do_Blk",	/* Open_Mp_Parallel_Do_Blk	*/
	"Open_Mp_Master_Blk",		/* Open_Mp_Master_Blk		*/
	"Open_Mp_Critical_Blk",		/* Open_Mp_Critical_Blk		*/
	"Open_Mp_Ordered_Blk",		/* Open_Mp_Ordered_Blk		*/
	"Open_Mp_Workshare_Blk",	/* Open_Mp_Workshare_Blk	*/
	"Open_Mp_Parallel_Workshare_Blk", /* Open_Mp_Parallel_Workshare_Blk */
	"Contains_Blk",	 		/* Contains_Blk	   		*/
	"Interface_Blk",		/* Interface_Blk		*/
	"Derived_Type_Blk",		/* Derived_Type_Blk		*/
#ifdef KEY /* Bug 10572 */
	"Enum_Blk"			/* Enum_Blk			*/
#endif /* KEY Bug 10572 */
	};

char		*boolean_str[]		= {
			"F",
			"T"
		};

char		*context_str[]		= {
		"Init_Stmt_Cat",
		"Sub_Func_Stmt_Cat",
		"Dir_Integer_Stmt_Cat",
		"Use_Stmt_Cat",
#ifdef KEY /* Bug 11741 */
		"Import_Stmt_Cat",
#endif /* KEY Bug 11741 */
		"Implicit_None_Stmt_Cat",
		"Implicit_Stmt_Cat",
		"Declaration_Stmt_Cat",
		"Executable_Stmt_Cat",
		};

char		*cn_hollerith_str[]	= {
			"Not_Hollerith",
			"L_Hollerith",
			"H_Hollerith",
			"R_Hollerith"
		};

char		*debug_lvl_str[]	= {
			"Debug_Lvl_0",
			"Debug_Lvl_1",
			"Debug_Lvl_2",
			"Debug_Lvl_3",
			"No_Debugging"
		};

char		*distribution_str[]	= {
			"No_Distribution",
			"Block_Distribution",
			"Cyclic_Distribution",
			"Star_Distribution"
		};

char		*do_type_str[]		= {
  			"Unknown_Loop", 
			"Iterative_Loop",
			"While_Loop",
			"Infinite_Loop"
		};

char            *field_str[]         = {
                        "NO_Tbl_Idx",
                        "CN_Tbl_Idx",
                        "SB_Tbl_Idx",
                        "IL_Tbl_Idx",
                        "AT_Tbl_Idx",
                        "SH_Tbl_Idx",
                        "IR_Tbl_Idx"
                };

char            *file_path_str[]	= {
                        "Unknown_Fp",
                        "Directory_Fp",
                        "File_Fp",
                        "Mod_File_Fp",
                        "Archive_File_Fp",
                        "Elf_File_Fp",
                        "Module_Fp",
                        "Current_Compile_Fp",
                        "Include_Fp",
                        "Inline_Fp",
                        "Reshape_Array_Fp",
                        "Xcoff_Fp"
                };

char		*implicit_storage_str[]	= {
			"Impl_Default_Storage",
			"Impl_Automatic_Storage",
			"Impl_Static_Storage"
		};

char		*integer_size_str[]	= {
			"Integer_32",
			"Integer_64"
		};

char		*intent_str[]		= {
			"Unseen",
			"In",
			"Out",
			"Inout"
		};

char		*interface_str[]	= {
                        "Generic_Unknown_Interface",
                        "Generic_Function_Interface",
                        "Generic_Subroutine_Interface",
                        "Defined_Interface",
                        "Defined_Assign_Interface",
                        "Defined_Unary_Interface",
                        "Defined_Binary_Interface",
                        "Defined_Unary_Or_Binary_Interface"
		};

char		*intrin_str[]		= {
                                "Unknown_Intrinsic",
			        "Abs_Intrinsic",
			        "Achar_Intrinsic",
			        "Acos_Intrinsic",
				"Acosd_Intrinsic",
				"Add_And_Fetch_Intrinsic",
				"Adjustl_Intrinsic",
				"Adjustr_Intrinsic",
				"Aimag_Intrinsic",
				"Aint_Intrinsic",
				"All_Intrinsic",
				"Allocated_Intrinsic",
				"Alog_Intrinsic",
				"Alog10_Intrinsic",
				"Amax0_Intrinsic",
				"Amax1_Intrinsic",
				"Amin0_Intrinsic",
				"Amin1_Intrinsic",
				"Amod_Intrinsic",
				"And_Intrinsic",
				"And_And_Fetch_Intrinsic",
				"Anint_Intrinsic",
				"Any_Intrinsic",
				"Asin_Intrinsic",
				"Asind_Intrinsic",
				"Associated_Intrinsic",
				"Atan_Intrinsic",
				"Atan2_Intrinsic",
				"Atan2d_Intrinsic",
				"Atand_Intrinsic",
				"Bitest_Intrinsic",
				"Bit_Size_Intrinsic",
				"Bjtest_Intrinsic",
				"Bktest_Intrinsic",
				"Btest_Intrinsic",
				"Cabs_Intrinsic",
				"Ccos_Intrinsic",
				"Cdabs_Intrinsic",
				"Cdcos_Intrinsic",
				"Cdexp_Intrinsic",
				"Cdlog_Intrinsic",
				"Cdsin_Intrinsic",
				"Cdsqrt_Intrinsic",
				"Ceiling_Intrinsic",
				"Cexp_Intrinsic",
				"Char_Intrinsic",
				"Clear_Ieee_Exception_Intrinsic",
				"Cloc_Intrinsic",
				"Clock_Intrinsic",
				"Clog_Intrinsic",
				"Cmplx_Intrinsic",
				"Compare_And_Swap_Intrinsic",
				"Compl_Intrinsic",
				"Conjg_Intrinsic",
				"Cos_Intrinsic",
				"Cosd_Intrinsic",
				"Cosh_Intrinsic",
				"Cot_Intrinsic",
				"Count_Intrinsic",
				"Cputime_Intrinsic",
				"Cqabs_Intrinsic",
				"Cqcos_Intrinsic",
				"Cqexp_Intrinsic",
				"Cqlog_Intrinsic",
				"Cqsin_Intrinsic",
				"Cqsqrt_Intrinsic",
				"Cshift_Intrinsic",
				"Csin_Intrinsic",
				"Csmg_Intrinsic",
				"Csqrt_Intrinsic",
				"Cvmgm_Intrinsic",
				"Cvmgn_Intrinsic",
				"Cvmgp_Intrinsic",
				"Cvmgt_Intrinsic",
				"Cvmgz_Intrinsic",
#ifdef KEY /* Bug 14150 */
				"C_F_Pointer_Intrinsic",
				"C_F_Procpointer_Intrinsic",
				"C_Funloc_Intrinsic",
				"C_Loc_Iso_Intrinsic",
#endif /* KEY Bug 14150 */
				"C_Loc_Intrinsic",
				"Dabs_Intrinsic",
				"Dacos_Intrinsic",
				"Dacosd_Intrinsic",
				"Dasin_Intrinsic",
				"Dasind_Intrinsic",
				"Datan_Intrinsic",
				"Datan2_Intrinsic",
				"Datan2d_Intrinsic",
				"Datand_Intrinsic",
				"Date_Intrinsic",
				"Date_And_Time_Intrinsic",
				"Dble_Intrinsic",
				"Dbleq_Intrinsic",
				"Dcmplx_Intrinsic",
				"Dconjg_Intrinsic",
				"Dcos_Intrinsic",
				"Dcosd_Intrinsic",
				"Dcosh_Intrinsic",
				"Dcot_Intrinsic",
				"Ddim_Intrinsic",
#ifdef KEY /* Bug 1329 */
				"Derf_Intrinsic",
				"Derfc_Intrinsic",
#endif /* KEY Bug 1329 */
				"Dexp_Intrinsic",
				"Dfloat_Intrinsic",
				"Dfloati_Intrinsic",
				"Dfloatj_Intrinsic",
				"Dfloatk_Intrinsic",
				"Digits_Intrinsic",
				"Dim_Intrinsic",
				"Dimag_Intrinsic",
				"Dint_Intrinsic",
				"Disable_Ieee_Interrupt_Intrinsic",
				"Dlog_Intrinsic",
				"Dlog10_Intrinsic",
				"Dmax1_Intrinsic",
				"Dmin1_Intrinsic",
				"Dmod_Intrinsic",
				"Dnint_Intrinsic",
				"Dot_Product_Intrinsic",
				"Dprod_Intrinsic",
				"Dreal_Intrinsic",
				"Dshiftl_Intrinsic",
				"Dshiftr_Intrinsic",
				"Dsign_Intrinsic",
				"Dsin_Intrinsic",
				"Dsind_Intrinsic",
				"Dsinh_Intrinsic",
                                "DSM_Chunksize_Intrinsic",
                                "DSM_Distribution_Block_Intrinsic",
                                "DSM_Distribution_Cyclic_Intrinsic",
                                "DSM_Distribution_Star_Intrinsic",
                                "DSM_Isdistributed_Intrinsic",
                                "DSM_Isreshaped_Intrinsic",
                                "DSM_Numchunks_Intrinsic",
                                "DSM_Numthreads_Intrinsic",
                                "DSM_Rem_Chunksize_Intrinsic",
                                "DSM_This_Chunksize_Intrinsic",
                                "DSM_This_Startingindex_Intrinsic",
                                "DSM_This_Threadnum_Intrinsic",
				"Dsqrt_Intrinsic",
				"Dtan_Intrinsic",
				"Dtand_Intrinsic",
				"Dtanh_Intrinsic",
# ifdef KEY
				"Dtime_Intrinsic",
# endif
				"Enable_Ieee_Interrupt_Intrinsic",
				"Eoshift_Intrinsic",
				"Epsilon_Intrinsic",
				"Eqv_Intrinsic",
#ifdef KEY /* Bug 1324 */
				"Erf_Intrinsic",
				"Erfc_Intrinsic",
#endif /* KEY Bug 1324 */
#ifdef KEY /* Bug 3018 */
				"Etime_Intrinsic",
#endif /* KEY Bug 3018 */
				"Exit_Intrinsic",
				"Exp_Intrinsic",
				"Exponent_Intrinsic",
				"Fcd_Intrinsic",
# ifdef KEY
				"Fdate_Intrinsic",
# endif
				"Fetch_And_Add_Intrinsic",
				"Fetch_And_And_Intrinsic",
				"Fetch_And_Nand_Intrinsic",
				"Fetch_And_Or_Intrinsic",
				"Fetch_And_Sub_Intrinsic",
				"Fetch_And_Xor_Intrinsic",
				"Float_Intrinsic",
				"Floati_Intrinsic",
				"Floatj_Intrinsic",
				"Floatk_Intrinsic",
				"Floor_Intrinsic",
# ifdef KEY
				"Fnum_Intrinsic",
# endif
				"Fp_Class_Intrinsic",
				"Fraction_Intrinsic",
				"Free_Intrinsic",
#ifdef KEY
                                "Fstat_Intrinsic",
#endif
				"Getpos_Intrinsic",
				"Get_Ieee_Exceptions_Intrinsic",
				"Get_Ieee_Interrupts_Intrinsic",
				"Get_Ieee_Rounding_Mode_Intrinsic",
				"Get_Ieee_Status_Intrinsic",
				"Huge_Intrinsic",
				"Iabs_Intrinsic",
				"Iachar_Intrinsic",
				"Iand_Intrinsic",
				"Ibchng_Intrinsic",
				"Ibclr_Intrinsic",
				"Ibits_Intrinsic",
				"Ibset_Intrinsic",
				"Ichar_Intrinsic",
				"Idate_Intrinsic",
				"Idim_Intrinsic",
				"Idint_Intrinsic",
				"Idnint_Intrinsic",
				"Ieee_Binary_Scale_Intrinsic",
				"Ieee_Class_Intrinsic",
				"Ieee_Copy_Sign_Intrinsic",
				"Ieee_Exponent_Intrinsic",
				"Ieee_Finite_Intrinsic",
				"Ieee_Int_Intrinsic",
				"Ieee_Is_Nan_Intrinsic",
				"Ieee_Next_After_Intrinsic",
				"Ieee_Real_Intrinsic",
				"Ieee_Remainder_Intrinsic",
				"Ieee_Unordered_Intrinsic",
				"Ieor_Intrinsic",
				"Ifix_Intrinsic",
				"Iiabs_Intrinsic",
				"Iiand_Intrinsic",
#ifdef KEY /* Enum value itself predates KEY, but this string was missing */
				"Iibchng_Intrinsic",
#endif
				"Iibclr_Intrinsic",
				"Iibits_Intrinsic",
				"Iibset_Intrinsic",
				"Iidim_Intrinsic",
				"Iidint_Intrinsic",
				"Iieor_Intrinsic",
				"Iifix_Intrinsic",
				"Iint_Intrinsic",
				"Iior_Intrinsic",
				"Iiqint_Intrinsic",
				"Iisha_Intrinsic",
				"Iishc_Intrinsic",
				"Iishft_Intrinsic",
				"Iishftc_Intrinsic",
				"Iishl_Intrinsic",
				"Iisign_Intrinsic",
				"Ilen_Intrinsic",
				"Imag_Intrinsic",
				"Imod_Intrinsic",
				"Imvbits_Intrinsic",
				"Index_Intrinsic",
				"Inint_Intrinsic",
				"Inot_Intrinsic",
				"Int_Intrinsic",
				"Int1_Intrinsic",
				"Int2_Intrinsic",
				"Int4_Intrinsic",
				"Int8_Intrinsic",
				"Int_Mult_Upper_Intrinsic",
				"Ior_Intrinsic",
				"Iqint_Intrinsic",
				"Iqnint_Intrinsic",
				"Irtc_Intrinsic",
				"Isha_Intrinsic",
				"Ishc_Intrinsic",
				"Ishft_Intrinsic",
				"Ishftc_Intrinsic",
				"Ishl_Intrinsic",
				"Isign_Intrinsic",
				"Isnan_Intrinsic",
				"Jdate_Intrinsic",
				"Jiabs_Intrinsic",
				"Jiand_Intrinsic",
#ifdef KEY /* Enum value itself predates KEY, but this string was missing */
				"Jibchng_Intrinsic",
#endif
				"Jibclr_Intrinsic",
				"Jibits_Intrinsic",
				"Jibset_Intrinsic",
				"Jidim_Intrinsic",
				"Jidint_Intrinsic",
				"Jieor_Intrinsic",
				"Jifix_Intrinsic",
				"Jint_Intrinsic",
				"Jior_Intrinsic",
				"Jiqint_Intrinsic",
				"Jisha_Intrinsic",
				"Jishc_Intrinsic",
				"Jishft_Intrinsic",
				"Jishftc_Intrinsic",
				"Jishl_Intrinsic",
				"Jisign_Intrinsic",
				"Jmod_Intrinsic",
				"Jmvbits_Intrinsic",
				"Jnint_Intrinsic",
				"Jnot_Intrinsic",
				"Kiabs_Intrinsic",
				"Kiand_Intrinsic",
#ifdef KEY /* Enum value itself predates KEY, but this string was missing */
				"Kibchng_Intrinsic",
#endif
				"Kibclr_Intrinsic",
				"Kibits_Intrinsic",
				"Kibset_Intrinsic",
				"Kidim_Intrinsic",
				"Kidint_Intrinsic",
				"Kieor_Intrinsic",
				"Kifix_Intrinsic",
#ifdef KEY
				"Kill_Intrinsic",
#endif
				"Kind_Intrinsic",
				"Kint_Intrinsic",
				"Kior_Intrinsic",
				"Kiqint_Intrinsic",
				"Kisha_Intrinsic",
				"Kishc_Intrinsic",
				"Kishft_Intrinsic",
				"Kishftc_Intrinsic",
				"Kishl_Intrinsic",
				"Kisign_Intrinsic",
				"Kmod_Intrinsic",
				"Kmvbits_Intrinsic",
				"Knint_Intrinsic",
				"Knot_Intrinsic",
				"Lbound_Intrinsic",
				"Leadz_Intrinsic",
				"Len_Intrinsic",
				"Length_Intrinsic",
				"Len_Trim_Intrinsic",
#ifndef KEY	/* Remove duplicate string entry */
				"Length_Intrinsic",
#endif
				"Lge_Intrinsic",
				"Lgt_Intrinsic",
				"Lle_Intrinsic",
				"Llt_Intrinsic",
				"Loc_Intrinsic",
				"Lock_Release_Intrinsic",
				"Lock_Test_And_Set_Intrinsic",
				"Log_Intrinsic",
				"Log10_Intrinsic",
				"Log2_Images_Intrinsic",
				"Logical_Intrinsic",
				"Long_Intrinsic",
				"Lshift_Intrinsic",
				"M@clr_Intrinsic",
				"M@ld_Intrinsic",
				"M@ldmx_Intrinsic",
				"M@mx_Intrinsic",
				"M@ul_Intrinsic",
				"Malloc_Intrinsic",
				"Mask_Intrinsic",
				"Matmul_Intrinsic",
				"Max_Intrinsic",
				"Max0_Intrinsic",
				"Max1_Intrinsic",
				"Maxexponent_Intrinsic",
				"Maxloc_Intrinsic",
				"Maxval_Intrinsic",
				"Memory_Barrier_Intrinsic",
				"Merge_Intrinsic",
				"Min_Intrinsic",
				"Min0_Intrinsic",
				"Min1_Intrinsic",
				"Minexponent_Intrinsic",
				"Minloc_Intrinsic",
				"Minval_Intrinsic",
				"Mod_Intrinsic",
				"Modulo_Intrinsic",
				"Mvbits_Intrinsic",
                                "My_Pe_Intrinsic",
				"Nand_And_Fetch_Intrinsic",
				"Nearest_Intrinsic",
				"Neqv_Intrinsic",
				"Nint_Intrinsic",
				"Not_Intrinsic",
				"Null_Intrinsic",
				"Numarg_Intrinsic",
				"Num_Images_Intrinsic",
#ifdef KEY
                                "Omp_Destroy_Lock_Intrinsic",
                                "Omp_Destroy_Nest_Lock_Intrinsic",
#endif
				"Omp_Get_Dynamic_Intrinsic",
				"Omp_Get_Max_Threads_Intrinsic",
				"Omp_Get_Nested_Intrinsic",
				"Omp_Get_Num_Procs_Intrinsic",
				"Omp_Get_Num_Threads_Intrinsic",
				"Omp_Get_Thread_Num_Intrinsic",
#ifdef KEY
                                "Omp_Get_Wtick_Intrinsic",
                                "Omp_Get_Wtime_Intrinsic",
                                "Omp_Init_Lock_Intrinsic",
                                "Omp_Init_Nest_Lock_Intrinsic",
#endif
				"Omp_In_Parallel_Intrinsic",
				"Omp_Set_Lock_Intrinsic",
#ifdef KEY
				"Omp_Set_Nest_Lock_Intrinsic",
#endif
				"Omp_Test_Lock_Intrinsic",
#ifdef KEY
				"Omp_Test_Nest_Lock_Intrinsic",
#endif
				"Omp_Unset_Lock_Intrinsic",
#ifdef KEY
				"Omp_Unset_Nest_Lock_Intrinsic",
#endif
				"Or_Intrinsic",
				"Or_And_Fetch_Intrinsic",
				"Pack_Intrinsic",
				"Popcnt_Intrinsic",
				"Poppar_Intrinsic",
				"Precision_Intrinsic",
				"Present_Intrinsic",
				"Product_Intrinsic",
				"Qabs_Intrinsic",
				"Qacos_Intrinsic",
				"Qacosd_Intrinsic",
				"Qasin_Intrinsic",
				"Qasind_Intrinsic",
				"Qatan_Intrinsic",
				"Qatan2_Intrinsic",
				"Qatan2d_Intrinsic",
				"Qatand_Intrinsic",
				"Qcmplx_Intrinsic",
				"Qcos_Intrinsic",
				"Qcosd_Intrinsic",
				"Qcosh_Intrinsic",
				"Qcot_Intrinsic",
				"Qdim_Intrinsic",
				"Qexp_Intrinsic",
				"Qext_Intrinsic",
				"Qfloat_Intrinsic",
				"Qfloati_Intrinsic",
				"Qfloatj_Intrinsic",
				"Qfloatk_Intrinsic",
				"Qimag_Intrinsic",
				"Qint_Intrinsic",
				"Qlog_Intrinsic",
				"Qlog10_Intrinsic",
				"Qmod_Intrinsic",
				"Qnint_Intrinsic",
				"Qprod_Intrinsic",
				"Qreal_Intrinsic",
				"Qsign_Intrinsic",
				"Qsin_Intrinsic",
				"Qsind_Intrinsic",
				"Qsinh_Intrinsic",
				"Qsqrt_Intrinsic",
				"Qtan_Intrinsic",
				"Qtand_Intrinsic",
				"Qtanh_Intrinsic",
				"Radix_Intrinsic",
				"Ran_Intrinsic",
				"Random_Number_Intrinsic",
				"Random_Seed_Intrinsic",
				"Randu_Intrinsic",
				"Ranf_Intrinsic",
				"Range_Intrinsic",
				"Ranget_Intrinsic",
				"Ranset_Intrinsic",
				"Read@sm_Intrinsic",
				"Real_Intrinsic",
				"Remote_Write_Barrier_Intrinsic",
				"Rem_Images_Intrinsic",
				"Repeat_Intrinsic",
				"Reshape_Intrinsic",
				"Rrspacing_Intrinsic",
				"Rshift_Intrinsic",
				"Rtc_Intrinsic",
				"Scale_Intrinsic",
				"Scan_Intrinsic",
				"SIK_Intrinsic",
				"SRK_Intrinsic",
				"Set_Exponent_Intrinsic",
				"Set_Ieee_Exception_Intrinsic",
				"Set_Ieee_Exceptions_Intrinsic",
				"Set_Ieee_Interrupts_Intrinsic",
				"Set_Ieee_Rounding_Mode_Intrinsic",
				"Set_Ieee_Status_Intrinsic",
				"Shape_Intrinsic",
				"Shift_Intrinsic",
				"Shifta_Intrinsic",
				"Shiftl_Intrinsic",
				"Shiftr_Intrinsic",
				"Short_Intrinsic",
				"Sign_Intrinsic",
#ifdef KEY
				"Signal_Intrinsic",
#endif
				"Sin_Intrinsic",
				"Sind_Intrinsic",
				"Sinh_Intrinsic",
				"Size_Intrinsic",
				"Sizeof_Intrinsic",
				"Sngl_Intrinsic",
				"Snglq_Intrinsic",
				"Spacing_Intrinsic",
				"Spread_Intrinsic",
				"Sqrt_Intrinsic",
#ifdef KEY
                                "Stat_Intrinsic",
#endif
				"Sub_And_Fetch_Intrinsic",
				"Sum_Intrinsic",
				"Synchronize_Intrinsic",
				"Sync_Images_Intrinsic",
				"System_Clock_Intrinsic",
				"Tan_Intrinsic",
				"Tand_Intrinsic",
				"Tanh_Intrinsic",
                                "Test_Ieee_Exception_Intrinsic",
                                "Test_Ieee_Interrupt_Intrinsic",
				"This_Image_Intrinsic",
				"Time_Intrinsic",
#ifdef KEY
				"Time4_Intrinsic",
				"Time8_Intrinsic",
#endif
				"Tiny_Intrinsic",
				"Transfer_Intrinsic",
				"Transpose_Intrinsic",
				"Trim_Intrinsic",
				"Ubound_Intrinsic",
				"Unit_Intrinsic",
				"Unpack_Intrinsic",
				"Verify_Intrinsic",
                                "Write_Memory_Barrier_Intrinsic",
				"Xor_Intrinsic",
				"Xor_And_Fetch_Intrinsic", 
# ifdef KEY
				"Zabs_Intrinsic",
				"Zcos_Intrinsic",
				"Zexp_Intrinsic",
				"Zlog_Intrinsic",
				"Zsin_Intrinsic",
				"Zsqrt_Intrinsic",
# endif
#ifdef KEY /* Bug 1683 */
				"Pathf90_Intrinsic",
#endif /* KEY Bug 1683 */
#ifdef KEY /* Bug 5089 */
				"True_Intrinsic",
				"Support_Uflow_Intrinsic",
#endif /* KEY Bug 5089 */
#ifdef KEY /* F2003 */
				"Newline_Intrinsic"
#endif /* KEY F2003 */
		};

char		*msg_lvl_str[]		= {
			"Comment_Lvl",
			"Note_Lvl",
			"Caution_Lvl",
			"Warning_Lvl",
			"Error_Lvl"
		};

char		*obj_class_str[]	= {
			"Data_Obj",
			"Pgm_Unit",
			"Label",
			"Derived_Type",
			"Interface",
			"Namelist_Grp",
			"Stmt_Func"
		};

char		*operator_str[]		= {
                        "Null_Opr",
                        "Defined_Un_Opr",
                        "Alloc_Opr",
                        "SSD_Alloc_Opr",
                        "Cvrt_Opr",
			"Dealloc_Opr",
                        "Power_Opr",
                        "Mult_Opr",
                        "Div_Opr",
                        "Uplus_Opr",
                        "Uminus_Opr",
                        "Plus_Opr",
                        "Minus_Opr",
                        "Concat_Opr",
                        "Eq_Opr",
                        "Ne_Opr",
                        "Lt_Opr",
                        "Le_Opr",
                        "Gt_Opr",
                        "Ge_Opr",
                        "Not_Opr",
                        "Bnot_Opr",
                        "And_Opr",
                        "Or_Opr",
                        "Bor_Opr",
                        "Eqv_Opr",
                        "Beqv_Opr",
                        "Neqv_Opr",
                        "Bneqv_Opr",
			"Abs_Opr",
			"Cos_Opr",
			"Sin_Opr",
			"Log_E_Opr",
			"Log_10_Opr",
			"Tan_Opr",
			"Tanh_Opr",
			"Sinh_Opr",
			"Acos_Opr",
			"Asin_Opr",
			"Atan_Opr",
			"Cosh_Opr",
			"Atan2_Opr",
			"Aimag_Opr",
			"Sqrt_Opr",
			"Cot_Opr",
			"Exp_Opr",
			"Int_Opr",
			"Band_Opr",
			"Mod_Opr",
			"Anint_Opr",
			"Nint_Opr",
			"Sign_Opr",
			"Modulo_Opr",
			"Shift_Opr",
			"Shiftl_Opr",
			"Shiftr_Opr",
			"Leadz_Opr",
			"Popcnt_Opr",
			"Poppar_Opr",
			"Aint_Opr",
			"Dim_Opr",
			"Ranget_Opr",
			"Ranset_Opr",
			"Ranf_Opr",
			"Real_Opr",
			"Dble_Opr",
			"Mask_Opr",
                        "Conjg_Opr",
                        "Dprod_Opr",
                        "I24mult_Opr",
                        "Length_Opr",
                        "Getpos_Opr",
                        "Unit_Opr",
                        "Cmplx_Opr",
                        "Ichar_Opr",
                        "Char_Opr",
                        "Lint_Opr",
                        "Index_Opr",
                        "Lge_Opr",
                        "Lgt_Opr",
                        "Lle_Opr",
                        "Llt_Opr",
                        "Fcd_Opr",
                        "Numarg_Opr",
                        "Rtc_Opr",
                        "Cvmgp_Opr",
                        "Cvmgm_Opr",
                        "Cvmgz_Opr",
                        "Cvmgn_Opr",
                        "Cvmgt_Opr",
                        "Csmg_Opr",
                        "Adjustl_Opr",
                        "Adjustr_Opr",
                        "Ceiling_Opr",
                        "Exponent_Opr",
                        "Floor_Opr",
                        "Fraction_Opr",
                        "Spacing_Opr",
                        "Logical_Opr",
                        "Nearest_Opr",
                        "Rrspacing_Opr",
                        "Scale_Opr",
                        "Scan_Opr",
                        "Set_Exponent_Opr",
                        "Verify_Opr",
                        "Len_Trim_Opr",
                        "Dshiftl_Opr",
                        "Dshiftr_Opr",
                        "Mmx_Opr",
                        "Mldmx_Opr",
                        "Mld_Opr",
                        "Mul_Opr",
                        "Mcbl_Opr",
                        "Cshift_Opr",
                        "Dot_Product_Opr",
                        "Matmul_Opr",
                        "Spread_Opr",
                        "Transpose_Opr",
                        "All_Opr",
                        "Any_Opr",
                        "Count_Opr",
                        "Product_Opr",
                        "Sum_Opr",
                        "Eoshift_Opr",
                        "Maxval_Opr",
                        "Minval_Opr",
                        "Maxloc_Opr",
                        "Minloc_Opr",
                        "Reshape_Opr",
                        "SRK_Opr",
                        "SIK_Opr",
                        "Repeat_Opr",
                        "Trim_Opr",
                        "Transfer_Opr",
                        "Defined_Bin_Opr",
                        "Asg_Opr",
                        "Call_Opr",
			"Alt_Return_Opr",
                        "Case_Opr",
			"Allocate_Opr",
                        "Deallocate_Opr",
                        "End_Opr",
                        "Entry_Opr",
                        "Nullify_Opr",
                        "Pause_Opr",
                        "Ptr_Asg_Opr",
			"Flat_Array_Asg_Opr",
                        "Return_Opr",
                        "Select_Opr",
			"Stmt_Func_Call_Opr",
                        "Stop_Opr",
			"Max_Opr",
			"Min_Opr",
			"Read_Formatted_Opr",
			"Read_Unformatted_Opr",
			"Read_Namelist_Opr",
			"Write_Formatted_Opr",
			"Write_Unformatted_Opr",
			"Write_Namelist_Opr",
			"Inquire_Iolength_Opr",
                        "Dv_Whole_Copy_Opr",
			"Dv_Whole_Def_Opr",
			"Dv_Def_Asg_Opr",
			"Dv_Deref_Opr",
                        "Dv_Access_Base_Addr",
                        "Dv_Set_Base_Addr",
                        "Dv_Access_El_Len",
                        "Dv_Set_El_Len",
                        "Dv_Access_Assoc",
                        "Dv_Set_Assoc",
                        "Dv_Access_Ptr_Alloc",
                        "Dv_Set_Ptr_Alloc",
                        "Dv_Access_P_Or_A",
                        "Dv_Set_P_Or_A",
                        "Dv_Access_A_Contig",
                        "Dv_Set_A_Contig",
                        "Dv_Access_N_Dim",
                        "Dv_Set_N_Dim",
                        "Dv_Access_Typ_Code",
                        "Dv_Set_Typ_Code",
                        "Dv_Access_Orig_Base",
                        "Dv_Set_Orig_Base",
                        "Dv_Access_Orig_Size",
                        "Dv_Set_Orig_Size",
                        "Dv_Access_Low_Bound",
                        "Dv_Set_Low_Bound",
                        "Dv_Access_Extent",
                        "Dv_Set_Extent",
                        "Dv_Access_Stride_Mult",
                        "Dv_Set_Stride_Mult",

			"Br_Aif_Opr",
			"Br_Asg_Opr",
			"Br_Index_Opr",
			"Br_True_Opr",
			"Br_Uncond_Opr",
			"Case_Range_Opr",
			"Implied_Do_Opr",
			"Kwd_Opr",
			"Percent_Val_Opr",
			"Loc_Opr",
			"Aloc_Opr",
			"Const_Tmp_Loc_Opr",
			"Present_Opr",
			"Argchck_Present_Opr",
			"Argchck_Loc_Opr",
			"Len_Opr",
			"Clen_Opr",
			"Paren_Opr",
			"Struct_Opr",
			"Struct_Construct_Opr",
			"Array_Construct_Opr",
			"Constant_Struct_Construct_Opr",
			"Constant_Array_Construct_Opr",
			"Subscript_Opr",
			"Whole_Subscript_Opr",
			"Section_Subscript_Opr",
			"Alloc_Obj_Opr",
			"Dealloc_Obj_Opr",
			"Substring_Opr",
			"Whole_Substring_Opr",
			"Triplet_Opr",
			"Label_Opr",
			"Loop_Info_Opr",
			"Loop_End_Opr",
			"Init_Opr",
			"Init_Reloc_Opr",
			"Use_Opr",
			"Where_Opr",
			"Real_Div_To_Int_Opr",
			"Readsm_Opr",
			"Memory_Barrier_Opr",
			"Remote_Write_Barrier_Opr",
			"Write_Memory_Barrier_Opr",

			"Suppress_Opr",

			"Align_Cdir_Opr",
			"Bl_Cdir_Opr",
			"Bounds_Cdir_Opr",
			"Cachealign_Cdir_Opr",
			"Inline_Cdir_Opr",
			"Ivdep_Cdir_Opr",
			"Nextscalar_Cdir_Opr",
			"Nobl_Cdir_Opr",
			"Nobounds_Cdir_Opr",
			"Noinline_Cdir_Opr",
			"Norecurrence_Cdir_Opr",
			"Nosplit_Cdir_Opr",
			"Notask_Cdir_Opr",
			"Nounroll_Cdir_Opr",
			"Novector_Cdir_Opr",
			"Novsearch_Cdir_Opr",
			"Prefertask_Cdir_Opr",
			"Prefervector_Cdir_Opr",
			"Recurrence_Cdir_Opr",
			"Shortloop_Cdir_Opr",
			"Shortloop128_Cdir_Opr",
			"Split_Cdir_Opr",
			"Task_Cdir_Opr",
			"Unroll_Cdir_Opr",
			"Vector_Cdir_Opr",
			"Vsearch_Cdir_Opr",

			"Case_Cmic_Opr",
			"Endcase_Cmic_Opr",
			"Cncall_Cmic_Opr",
			"Continue_Cmic_Opr",
			"Doall_Cmic_Opr",
			"Doparallel_Cmic_Opr",
			"Enddo_Cmic_Opr",
			"Guard_Cmic_Opr",
			"Endguard_Cmic_Opr",
			"REMOVED_Opr",
			"Numcpus_Cmic_Opr",
			"Parallel_Cmic_Opr",
			"Endparallel_Cmic_Opr",
			"Permutation_Cmic_Opr",
			"Taskcommon_Cmic_Opr",
			"Wait_Cmic_Opr",
			"Send_Cmic_Opr",
			"My_Pe_Opr",
			"Ieee_Unordered_Opr",
			"Ieee_Next_After_Opr",
			"Ieee_Remainder_Opr",
			"Ieee_Exponent_Opr",
			"Ieee_Copy_Sign_Opr",
			"Ieee_Int_Opr",
			"Ieee_Real_Opr",
			"Ieee_Finite_Opr",
			"Ieee_Is_Nan_Opr",
			"Ieee_Class_Opr",
			"Ieee_Binary_Scale_Opr",
			"Int_Mult_Upper_Opr",
			"Get_Ieee_Status_Opr",
			"Set_Ieee_Status_Opr",
			"Get_Ieee_Exceptions_Opr",
			"Set_Ieee_Exceptions_Opr",
			"Get_Ieee_Interrupts_Opr",
			"Set_Ieee_Interrupts_Opr",
			"Get_Ieee_Rounding_Mode_Opr",
			"Set_Ieee_Rounding_Mode_Opr",
			"Test_Ieee_Interrupt_Opr",
			"Test_Ieee_Exception_Opr",
			"Set_Ieee_Exception_Opr",
			"Clear_Ieee_Exception_Opr",
			"Enable_Ieee_Interrupt_Opr",
			"Disable_Ieee_Interrupt_Opr",
			"Cvrt_Unsigned_Opr",
			"SSD_Dealloc_Opr",

			"Symbolic_Mult_Opr",
			"Symbolic_Div_Opr",
			"Symbolic_Uplus_Opr",
			"Symbolic_Uminus_Opr",
			"Symbolic_Plus_Opr",
			"Symbolic_Minus_Opr",
			"Symbolic_Max_Opr",

			"Rep_Count_Opr",
			"Lg_Opr",
			"Shifta_Opr",

			"Symbolic_Mod_Opr",
			"Symbolic_Shiftr_Opr",
			"Symbolic_Shiftl_Opr",
			"Symmetric_Alloc_Opr",
			"Symmetric_Dealloc_Opr",

			"Copy_In_Opr",
			"Copy_Out_Opr",

			"Pack_Opr",
			"Unpack_Opr",

			"Local_Pe_Dim_Opr",

                        "Start_Io_Opr",
                        "End_Io_Opr",

                        "Dot_Product_Logical_Opr",

			"Symbolic_Min_Opr",

			"Nopattern_Cdir_Opr",
			"Pattern_Cdir_Opr",
			"Mark_Cdir_Opr",
			"Nomark_Cdir_Opr",

			"Backspace_Opr",
                        "Buffer_In_Opr",
                        "Buffer_Out_Opr",
                        "Close_Opr",
                        "Endfile_Opr",
                        "Inquire_Opr",
                        "Open_Opr",
                        "Rewind_Opr",
                        "Mvbits_Opr",
                        "Ishftc_Opr",
                        "Ibits_Opr",

			"False_Parm_Opr",

			"Aggressiveinnerloopfission_Opr",
			"Blockable_Dir_Opr",
			"Blockingsize_Dir_Opr",
			"Fission_Star_Opr",
			"Fissionable_Star_Opr",
			"Fuse_Star_Opr",
			"Fusable_Star_Opr",
			"Interchange_Dir_Opr",
			"Nointerchange_Dir_Opr",
			"Nofission_Star_Opr",
			"Nofusion_Star_Opr",
			"Noblocking_Dir_Opr",
			"Opaque_Star_Opr",
			"Purpleconditional_Star_Opr",
			"Purpleunconditional_Star_Opr",

			"Redistribute_Dollar_Opr",
			"Doacross_Dollar_Opr",
                        "Pdo_Par_Opr",
                        "Parallel_Do_Par_Opr",
                        "Parallel_Par_Opr",
                        "Psection_Par_Opr",
                        "Singleprocess_Par_Opr",
                        "Section_Par_Opr",
                        "End_Pdo_Par_Opr",
                        "End_Parallel_Par_Opr",
                        "Barrier_Par_Opr",
                        "Critical_Section_Par_Opr",
                        "End_Critical_Section_Par_Opr",
                        "End_Psection_Par_Opr",
                        "End_Singleprocess_Par_Opr",

			"Unroll_Star_Opr",
			"Assert_Star_Opr",
			"Regionbegin_Star_Opr",
			"Regionend_Star_Opr",
			"Section_Gp_Star_Opr",
			"Section_Nongp_Star_Opr",

                        "Prefetch_Star_Opr",
                        "Prefetch_Manual_Star_Opr",
                        "Prefetch_Ref_Disable_Star_Opr",
                        "Prefetch_Ref_Star_Opr",

			"Align_Symbol_Star_Opr",
			"Fill_Symbol_Star_Opr",

			"Inline_Here_Star_Opr",
			"Noinline_Here_Star_Opr",
			"End_Inline_Here_Star_Opr",

                        "Dynamic_Dollar_Opr",
                        "Page_Place_Dollar_Opr",
			"Copyin_Dollar_Opr",

			"User_Code_Start_Opr",

                        "Fetch_And_Add_Opr",
                        "Fetch_And_Sub_Opr",
                        "Fetch_And_Or_Opr",
                        "Fetch_And_And_Opr",
                        "Fetch_And_Xor_Opr",
                        "Fetch_And_Nand_Opr",

                        "Add_And_Fetch_Opr",
                        "Sub_And_Fetch_Opr",
                        "Or_And_Fetch_Opr",
                        "And_And_Fetch_Opr",
                        "Xor_And_Fetch_Opr",
                        "Nand_And_Fetch_Opr",

                        "Synchronize_Opr",
                        "Lock_Release_Opr",
                        "Lock_Test_And_Set_Opr",
                        "Compare_And_Swap_Opr",

                        "Integer_Cdir_Opr",

                        "Malloc_Opr",
                        "Free_Opr",

                        "Concurrent_Cdir_Opr",

                        "Inline_Routine_Star_Opr",
                        "Noinline_Routine_Star_Opr",
                        "Inline_Global_Star_Opr",
                        "Noinline_Global_Star_Opr",

                        "Atomic_Open_Mp_Opr",
                        "Barrier_Open_Mp_Opr",
                        "Critical_Open_Mp_Opr",
                        "Do_Open_Mp_Opr",
                        "Endcritical_Open_Mp_Opr",
                        "Enddo_Open_Mp_Opr",
                        "Endparallel_Open_Mp_Opr",
                        "Endparalleldo_Open_Mp_Opr",
                        "Endparallelsections_Open_Mp_Opr",
                        "Endparallelworkshare_Open_Mp_Opr",
                        "Endmaster_Open_Mp_Opr",
                        "Endordered_Open_Mp_Opr",
                        "Endsections_Open_Mp_Opr",
                        "Endsingle_Open_Mp_Opr",
                        "Endworkshare_Open_Mp_Opr",
                        "Flush_Open_Mp_Opr",
                        "Master_Open_Mp_Opr",
                        "Ordered_Open_Mp_Opr",
                        "Parallel_Open_Mp_Opr",
                        "Paralleldo_Open_Mp_Opr",
                        "Parallelsections_Open_Mp_Opr",
                        "Parallelworkshare_Open_Mp_Opr",
                        "Section_Open_Mp_Opr",
                        "Sections_Open_Mp_Opr",
                        "Single_Open_Mp_Opr",
                        "Workshare_Open_Mp_Opr",

                        "Concurrentize_Star_Opr",
                        "Noconcurrentize_Star_Opr",

                        "Omp_Set_Lock_Opr",
                        "Omp_Unset_Lock_Opr",
                        "Omp_Test_Lock_Opr",

                        "Omp_Get_Num_Threads_Opr",
                        "Omp_Get_Max_Threads_Opr",
                        "Omp_Get_Thread_Num_Opr",
                        "Omp_Get_Num_Procs_Opr",
                        "Omp_In_Parallel_Opr",
                        "Omp_Get_Dynamic_Opr",
                        "Omp_Get_Nested_Opr",

			"Cache_Bypass_Cdir_Opr",

			"Forall_Opr",
			"If_Opr",
			"Else_Opr",
			"Endif_Opr",

			"Flush_Star_Opr",

			"Stmt_Expansion_Opr",

                 	"Cosd_Opr",   
                 	"Sind_Opr",   
                 	"Tand_Opr",   
                 	"Acosd_Opr",  
                 	"Asind_Opr",  
                 	"Atand_Opr",  
                 	"Atan2d_Opr", 

			"Stream_Dir_Opr",
			"UNUSED1_Opr",
			"UNUSED2_Opr",
			"UNUSED3_Opr",
			"UNUSED4_Opr",
			"UNUSED5_Opr",
			"Nostream_Dir_Opr",

			"Null_Intrinsic_Opr",

			"Io_Item_Type_Code_Opr",

			"Where_Cnstrct_Opr",
			"Else_Where_Mask_Opr",
			"Else_Where_Opr",

			"Preferstream_Opr",

			"Copyin_Bound_Opr",
			"Preferstream_Nocinv_Opr",

#ifdef KEY /* Bug 1324 */
			"Erf_Opr",
			"Erfc_Opr",
#endif /* KEY Bug 1324 */
#ifdef KEY /* Bug 2660 */
		 	"Options_Dir_Opr",
#endif /* KEY Bug 2660 */
#ifdef KEY /* Bug 10410 */
		 	"Cselect_Opr",
#endif /* KEY Bug 10410 */
                        "The_Last_Opr"
		};

char		*reference_str[]	= {
			"Not_Referenced",
		        "Char_Rslt_Bound_Ref",
			"Dcl_Bound_Ref",
			"Referenced"
		};

char		*sb_blk_type_str[]  	= {
			"Unknown",
			"Static",
			"Stack",
			"Formal",
			"Common",
			"Extern",
			"Exported",
			"Task_Common",
			"Soft_External",
			"Global_Breg",
			"Global_Treg",
			"Static_Named",
			"Based",
			"Equivalenced",
			"Restricted",
			"Distributed",
			"LM_Static",
			"LM_Common",
			"LM_Extern",
                        "Auxiliary",
			"Static_Local",
                        "Non_Local_Stack",
                        "Non_Local_Formal",
                        "Hosted_Stack",
			"Threadprivate",
			"Coment"
		};

char		*search_str[]		= {
			"Find_None",
			"Find_EOS",
			"Find_Lparen",
			"Find_Rparen",
                        "Find_Matching_Rparen",
			"Find_Comma",
			"Find_Comma_Slash",
			"Find_Comma_Rparen",
			"Find_Expr_End"
		};

char		*src_form_str[]		= {
			"Fixed",
			"Free"
		};

/* lex and statement type stuff */

char		*token_value_str[Tok_LAST+1]	= {
			"Tok_Label",		/* Tok_Label		*/
			"Tok_Id",		/* Tok_Id		*/
			"Tok_Kwd_Allocatable",	/* Tok_Kwd_Allocatable	*/
			"Tok_Kwd_Allocate",	/* Tok_Kwd_Allocate	*/
			"Tok_Kwd_Assign",	/* Tok_Kwd_Assign	*/
			"Tok_Kwd_Assignment",	/* Tok_Kwd_Assignment	*/
			"Tok_Kwd_Automatic",	/* Tok_Kwd_Automatic	*/
			"Tok_Kwd_Backspace",	/* Tok_Kwd_Backspace	*/
#ifdef KEY /* Bug 10572 */
			"Tok_Kwd_Bind",		/* Tok_Kwd_Bind    	*/
#endif /* KEY Bug 10572 */
			"Tok_Kwd_Block",	/* Tok_Kwd_Block	*/
			"Tok_Kwd_Buffer",	/* Tok_Kwd_Buffer	*/
			"Tok_Kwd_Byte",		/* Tok_Kwd_Byte		*/
			"Tok_Kwd_Call",		/* Tok_Kwd_Call		*/
			"Tok_Kwd_Case",		/* Tok_Kwd_Case		*/
			"Tok_Kwd_Character",	/* Tok_Kwd_Character	*/
			"Tok_Kwd_Close",	/* Tok_Kwd_Close	*/
			"Tok_Kwd_Common",	/* Tok_Kwd_Common	*/
			"Tok_Kwd_Complex",	/* Tok_Kwd_Complex	*/
			"Tok_Kwd_Contains",	/* Tok_Kwd_Contains	*/
			"Tok_Kwd_Continue",	/* Tok_Kwd_Continue	*/
			"Tok_Kwd_Cycle",	/* Tok_Kwd_Cycle	*/
			"Tok_Kwd_Data",		/* Tok_Kwd_Data		*/
			"Tok_Kwd_Deallocate",	/* Tok_Kwd_Deallocate	*/
			"Tok_Kwd_Decode",	/* Tok_Kwd_Decode	*/
			"Tok_Kwd_Default",	/* Tok_Kwd_Default	*/
			"Tok_Kwd_Dimension",	/* Tok_Kwd_Dimension	*/
			"Tok_Kwd_Dir",		/* Tok_Kwd_Dir		*/
			"Tok_Kwd_Do",		/* Tok_Kwd_Do		*/
			"Tok_Kwd_Double",	/* Tok_Kwd_Double	*/
			"Tok_Kwd_Elemental",	/* Tok_Kwd_Elemental	*/
			"Tok_Kwd_Else",		/* Tok_Kwd_Else		*/
			"Tok_Kwd_Encode",	/* Tok_Kwd_Encode	*/
			"Tok_Kwd_End",		/* Tok_Kwd_End		*/
			"Tok_Kwd_Entry",	/* Tok_Kwd_Entry	*/
#ifdef KEY /* Bug 10572 */
			"Tok_Kwd_Enum",		/* Tok_Kwd_Enum    	*/
			"Tok_Kwd_Enumerator",	/* Tok_Kwd_Enumerator  	*/
#endif /* KEY Bug 10572 */
			"Tok_Kwd_Equivalence",	/* Tok_Kwd_Equivalence	*/
			"Tok_Kwd_Exit",		/* Tok_Kwd_Exit		*/
			"Tok_Kwd_External",	/* Tok_Kwd_External	*/
			"Tok_Kwd_File",		/* Tok_Kwd_File		*/
			"Tok_Kwd_Forall",	/* Tok_Kwd_Forall       */
			"Tok_Kwd_Format",	/* Tok_Kwd_Format	*/
			"Tok_Kwd_Function",	/* Tok_Kwd_Function	*/
			"Tok_Kwd_Go",		/* Tok_Kwd_Go		*/
			"Tok_Kwd_If",		/* Tok_Kwd_If		*/
			"Tok_Kwd_Implicit",	/* Tok_Kwd_Implicit	*/
#ifdef KEY /* Bug 11741 */
			"Tok_Kwd_Import",	/* Tok_Kwd_Import	*/
#endif /* KEY Bug 11741 */
			"Tok_Kwd_In",		/* Tok_Kwd_In		*/
			"Tok_Kwd_Inquire",	/* Tok_Kwd_Inquire	*/
			"Tok_Kwd_Integer",	/* Tok_Kwd_Integer	*/
			"Tok_Kwd_Intent",	/* Tok_Kwd_Intent	*/
			"Tok_Kwd_Interface",	/* Tok_Kwd_Interface	*/
			"Tok_Kwd_Intrinsic",	/* Tok_Kwd_Intrinsic	*/
			"Tok_Kwd_Kind",		/* Tok_Kwd_Kind		*/
			"Tok_Kwd_Len",		/* Tok_Kwd_Len		*/
			"Tok_Kwd_Logical",	/* Tok_Kwd_Logical	*/
			"Tok_Kwd_Module",	/* Tok_Kwd_Module	*/
#ifdef KEY /* Bug 10572 */
			"Tok_Kwd_Name",		/* Tok_Kwd_Name    	*/
#endif /* KEY Bug 10572 */
			"Tok_Kwd_Namelist",	/* Tok_Kwd_Namelist	*/
			"Tok_Kwd_None",		/* Tok_Kwd_None		*/
#ifdef KEY /* Bug 5089 */
			"Tok_Kwd_Nonintrinsic",	/* Tok_Kwd_Nonintrinsic */
#endif /* KEY Bug 5089 */
			"Tok_Kwd_Nullify",	/* Tok_Kwd_Nullify	*/
			"Tok_Kwd_Only",		/* Tok_Kwd_Only		*/
			"Tok_Kwd_Open",		/* Tok_Kwd_Open		*/
			"Tok_Kwd_Operator",	/* Tok_Kwd_Operator	*/
			"Tok_Kwd_Optional",	/* Tok_Kwd_Optional	*/
			"Tok_Kwd_Out",		/* Tok_Kwd_Out		*/
			"Tok_Kwd_Parameter",	/* Tok_Kwd_Parameter	*/
			"Tok_Kwd_Pause",	/* Tok_Kwd_Pause	*/
			"Tok_Kwd_Pointer",	/* Tok_Kwd_Pointer	*/
			"Tok_Kwd_Precision",	/* Tok_Kwd_Precision	*/
			"Tok_Kwd_Print",	/* Tok_Kwd_Print	*/
			"Tok_Kwd_Private",	/* Tok_Kwd_Private	*/
			"Tok_Kwd_Procedure",	/* Tok_Kwd_Procedure	*/
			"Tok_Kwd_Program",	/* Tok_Kwd_Program	*/
			"Tok_Kwd_Public",	/* Tok_Kwd_Public	*/
			"Tok_Kwd_Pure",		/* Tok_Kwd_Pure		*/
			"Tok_Kwd_Read",		/* Tok_Kwd_Read		*/
			"Tok_Kwd_Real",		/* Tok_Kwd_Real		*/
			"Tok_Kwd_Recursive",	/* Tok_Kwd_Recursive	*/
			"Tok_Kwd_Result",	/* Tok_Kwd_Result	*/
			"Tok_Kwd_Return",	/* Tok_Kwd_Return	*/
			"Tok_Kwd_Rewind",	/* Tok_Kwd_Rewind	*/
			"Tok_Kwd_Save",		/* Tok_Kwd_Save		*/
			"Tok_Kwd_Select",	/* Tok_Kwd_Select	*/
			"Tok_Kwd_Sequence",	/* Tok_Kwd_Sequence	*/
			"Tok_Kwd_Span",		/* Tok_Kwd_Stat		*/
			"Tok_Kwd_Stat",		/* Tok_Kwd_Stat		*/
			"Tok_Kwd_Static",	/* Tok_Kwd_Static	*/
			"Tok_Kwd_Stop",		/* Tok_Kwd_Stop		*/
			"Tok_Kwd_Subroutine",	/* Tok_Kwd_Subroutine	*/
			"Tok_Kwd_Target",	/* Tok_Kwd_Target	*/
			"Tok_Kwd_Task",		/* Tok_Kwd_Then		*/
			"Tok_Kwd_Then",		/* Tok_Kwd_Then		*/
			"Tok_Kwd_To",		/* Tok_Kwd_To		*/
			"Tok_Kwd_Type",		/* Tok_Kwd_Type		*/
			"Tok_Kwd_Use",		/* Tok_Kwd_Use		*/
			"Tok_Kwd_Undefined",	/* Tok_Kwd_Undefined	*/
#ifdef KEY /* Bug 14150 */
			"Tok_Kwd_Value",	/* Tok_Kwd_Value	*/
#endif /* KEY Bug 14150 */
			"Tok_Kwd_Volatile",	/* Tok_Kwd_Volatile	*/
			"Tok_Kwd_Where",	/* Tok_Kwd_Where	*/
			"Tok_Kwd_While",	/* Tok_Kwd_While	*/
			"Tok_Kwd_Write",	/* Tok_Kwd_Write	*/
			"Tok_Dir_Start",	/* Tok_Dir_Start-marker	*/
			"Tok_Dir_Align",	/* Tok_Dir_Align	*/
			"Tok_Dir_Atomicupdate",	/* Tok_Dir_Atomicupdate	*/
			"Tok_Dir_Autoscope",    /* Tok_Dir_Autoscope    */
			"Tok_Dir_Auxiliary",	/* Tok_Dir_Auxiliary	*/
			"Tok_Dir_Barrier",	/* Tok_Dir_Barrier	*/
			"Tok_Dir_Bl",		/* Tok_Dir_Bl		*/
			"Tok_Dir_Block",	/* Tok_Dir_Block	*/
			"Tok_Dir_Blockable",	/* Tok_Dir_Blockable	*/
			"Tok_Dir_Blockingsize",	/* Tok_Dir_Blockingsize	*/
			"Tok_Dir_Bounds",	/* Tok_Dir_Bounds	*/
			"Tok_Dir_Cache_Align",	/* Tok_Dir_Cache_Align	*/
			"Tok_Dir_Cache_Bypass",	/* Tok_Dir_Cache_Bypass */
			"Tok_Dir_Cache_Noalloc",/* Tok_Dir_Cache_Noalloc*/
			"Tok_Dir_Chunksize",	/* Tok_Dir_Chunksize	*/
			"Tok_Dir_Cncall",	/* Tok_Dir_Cncall	*/ 
			"Tok_Dir_Code",		/* Tok_Dir_Code		*/
			"Tok_Dir_Common",	/* Tok_Dir_Common	*/
			"Tok_Dir_Concurrent",	/* Tok_Dir_Concurrent	*/
			"Tok_Dir_Control",	/* Tok_Dir_Control	*/
			"Tok_Dir_Copy_Assumed_Shape", /*Tok_Dir_Copy_Assumed_Shaped*/
			"Tok_Dir_Critical",	/* Tok_Dir_Critical	*/
			"Tok_Dir_Doshared",	/* Tok_Dir_Doshared	*/
			"Tok_Dir_Dynamic",	/* Tok_Dir_Dynamic	*/
			"Tok_Dir_Eject",	/* Tok_Dir_Eject	*/
			"Tok_Dir_Endcritical",	/* Tok_Dir_Endcritical	*/
			"Tok_Dir_Endmaster",	/* Tok_Dir_Endmaster	*/
			"Tok_Dir_Fixed",	/* Tok_Dir_Fixed	*/
			"Tok_Dir_Flow",		/* Tok_Dir_Flow		*/
			"Tok_Dir_Free",		/* Tok_Dir_Free		*/
			"Tok_Dir_Geometry",	/* Tok_Dir_Geometry	*/
			"Tok_Dir_Getfirst",	/* Tok_Dir_Getfirst	*/
			"Tok_Dir_Guided",	/* Tok_Dir_Guided	*/
			"Tok_Dir_Id",		/* Tok_Dir_Id		*/
			"Tok_Dir_If",		/* Tok_Dir_If		*/
			"Tok_Dir_Ignore_TKR",	/* Tok_Dir_Ignore_TKR	*/
			"Tok_Dir_Inline",	/* Tok_Dir_Inline	*/
			"Tok_Dir_Inline_Always",/* Tok_Dir_Inline_Always*/
			"Tok_Dir_Inline_Never",	/* Tok_Dir_Inline_Never	*/
			"Tok_Dir_Integer",	/* Tok_Dir_Integer	*/
			"Tok_Dir_Interchange",	/* Tok_Dir_Interchange	*/
			"Tok_Dir_Ivdep",	/* Tok_Dir_Ivdep	*/
			"Tok_Dir_List",		/* Tok_Dir_List		*/
			"Tok_Dir_Mark",		/* Tok_Dir_Mark		*/
			"Tok_Dir_Master",	/* Tok_Dir_Master	*/
			"Tok_Dir_Maxcpus",	/* Tok_Dir_Maxcpus	*/
			"Tok_Dir_Modinline",	/* Tok_Dir_Modinline	*/
			"Tok_Dir_Name",		/* Tok_Dir_Name		*/
			"Tok_Dir_Ncpus_Chunks",	/* Tok_Dir_Ncpus_Chunks	*/
			"Tok_Dir_Nextscalar",	/* Tok_Dir_Nextscalar	*/
			"Tok_Dir_Nobarrier",	/* Tok_Dir_Nobarrier	*/
			"Tok_Dir_Nobl",		/* Tok_Dir_Nobl		*/
			"Tok_Dir_Noblocking",	/* Tok_Dir_Noblocking	*/
			"Tok_Dir_Nobounds",	/* Tok_Dir_Nobounds	*/
			"Tok_Dir_Nocinv",	/* Tok_Dir_Nocinv	*/
			"Tok_Dir_Nocode",	/* Tok_Dir_Nocode	*/
			"Tok_Dir_Noflow",	/* Tok_Dir_Noflow	*/
			"Tok_Dir_Noinline",	/* Tok_Dir_Noinline	*/
			"Tok_Dir_Nointerchange",/* Tok_Dir_Nointerchange*/
			"Tok_Dir_Nolist",	/* Tok_Dir_Nolist	*/
			"Tok_Dir_Nomark",	/* Tok_Dir_Nomark	*/
			"Tok_Dir_Nomodinline",	/* Tok_Dir_Nomodinline	*/
			"Tok_Dir_Nopattern",	/* Tok_Dir_Nopattern	*/
			"Tok_Dir_Norecurrence",	/* Tok_Dir_Norecurrence */
			"Tok_Dir_Nosideeffects",/* Tok_Dir_Nosideeffects*/
			"Tok_Dir_Nosplit",	/* Tok_Dir_Nosplit	*/
			"Tok_Dir_Nostream",     /* Tok_Dir_Nostream     */
			"Tok_Dir_Notask",	/* Tok_Dir_Notask	*/
			"Tok_Dir_Nounroll",	/* Tok_Dir_Nounroll	*/
			"Tok_Dir_Novector",	/* Tok_Dir_Novector	*/
			"Tok_Dir_Novsearch",	/* Tok_Dir_Novsearch	*/
			"Tok_Dir_Numchunks",	/* Tok_Dir_Numchunks	*/
			"Tok_Dir_Numcpus",	/* Tok_Dir_Numcpus	*/
			"Tok_Dir_Parallel_Only",/* Tok_Dir_Parallel_Only*/
			"Tok_Dir_Pattern",	/* Tok_Dir_Pattern	*/
			"Tok_Dir_Pe_Private",	/* Tok_Dir_Pe_Private	*/
			"Tok_Dir_Pe_Resident",	/* Tok_Dir_Pe_Resident	*/
			"Tok_Dir_Permutation",	/* Tok_Dir_Permutation	*/
			"Tok_Dir_Preferstream",	/* Tok_Dir_Preferstream */
			"Tok_Dir_Prefertask",	/* Tok_Dir_Prefertask   */
			"Tok_Dir_Prefervector",	/* Tok_Dir_Prefervector */
			"Tok_Dir_Private",	/* Tok_Dir_Private	*/
			"Tok_Dir_Recurrence",	/* Tok_Dir_Recurrence	*/
			"Tok_Dir_Regfile",	/* Tok_Dir_Regfile	*/
			"Tok_Dir_Savelast",	/* Tok_Dir_Savelast	*/
			"Tok_Dir_Semextern",	/* Tok_Dir_Semextern	*/
			"Tok_Dir_Serial_Only",	/* Tok_Dir_Serial_Only	*/
			"Tok_Dir_Shared",	/* Tok_Dir_Shared	*/
			"Tok_Dir_Shortloop",	/* Tok_Dir_Shortloop	*/
			"Tok_Dir_Shortsequence",/* Tok_Dir_Shortsequence*/
			"Tok_Dir_Single",	/* Tok_Dir_Single	*/
			"Tok_Dir_Split",	/* Tok_Dir_Split	*/
			"Tok_Dir_Stack",	/* Tok_Dir_Stack	*/
			"Tok_Dir_Static",	/* Tok_Dir_Static	*/
			"Tok_Dir_Stream",	/* Tok_Dir_Stream       */
			"Tok_Dir_Suppress",	/* Tok_Dir_Suppress	*/
			"Tok_Dir_Symmetric",	/* Tok_Dir_Symmetric	*/
			"Tok_Dir_System_Module",/* Tok_Dir_System_Module*/
			"Tok_Dir_Task",		/* Tok_Dir_Task		*/
			"Tok_Dir_Taskcommon",	/* Tok_Dir_Taskcommon	*/
			"Tok_Dir_Taskhead",	/* Tok_Dir_Taskhead	*/
			"Tok_Dir_Unknown",	/* Tok_Dir_Unknown	*/
			"Tok_Dir_Unknown_Shared",/* Tok_Dir_Unknown_Shared*/
			"Tok_Dir_Unroll",	/* Tok_Dir_Unroll	*/
			"Tok_Dir_Uses_Eregs",	/* Tok_Dir_Uses_Eregs	*/
			"Tok_Dir_Vector",	/* Tok_Dir_Vector	*/
			"Tok_Dir_Vfunction",	/* Tok_Dir_Vfunction	*/
			"Tok_Dir_Vsearch",	/* Tok_Dir_Vsearch	*/
			"Tok_Dir_End",		/* Tok_Dir_End-marker 	*/
			"Tok_Mic_Start",	/* Tok_Mic_Start      	*/
			"Tok_Mic_Case",		/* Tok_Mic_Case		*/
			"Tok_Mic_End_Case",	/* Tok_Mic_End_Case	*/
			"Tok_Mic_Cncall",	/* Tok_Mic_Cncall	*/
			"Tok_Mic_Continue",	/* Tok_Mic_Continue	*/
			"Tok_Mic_Do_All",	/* Tok_Mic_Do_All	*/
			"Tok_Mic_Do_Parallel",	/* Tok_Mic_Do_Parallel	*/
			"Tok_Mic_End_Do",	/* Tok_Mic_End_Do	*/
			"Tok_Mic_Guard",	/* Tok_Mic_Guard	*/
			"Tok_Mic_End_Guard",	/* Tok_Mic_End_Guard	*/
			"Tok_Mic_If",		/* Tok_Mic_If		*/
			"Tok_Mic_Maxcpus",	/* Tok_Mic_Maxcpus	*/
			"Tok_Mic_Numcpus",	/* Tok_Mic_Numcpus	*/
			"Tok_Mic_Parallel",	/* Tok_Mic_Parallel	*/
			"Tok_Mic_End_Parallel",	/* Tok_Mic_End_Parallel	*/
			"Tok_Mic_Permutation",  /* Tok_Mic_Permutation  */
			"Tok_Mic_Point",	/* Tok_Mic_Point	*/
			"Tok_Mic_Send",		/* Tok_Mic_Send		*/
			"Tok_Mic_Span",		/* Tok_Mic_Span		*/
			"Tok_Mic_Taskcommon",	/* Tok_Mic_Taskcommon	*/
			"Tok_Mic_Wait",		/* Tok_Mic_Wait		*/
			"Tok_Mic_End",		/* Tok_Mic_End-marker 	*/

			/* the following is added by jhs, 2004/03/08 */
			"Tok_SGI_Dir_Start",		/* Start of SGI directives    */
			"Tok_SGI_Dir_Affinity",		/* AFFINITY		      */
			"Tok_SGI_Dir_Aggressiveinner",	/* AGGRESSIVEINNERLOOPFUSION  */
			"Tok_SGI_Dir_Align_Symbol",	/* ALIGN SYMBOL		      */
			"Tok_SGI_Dir_Argumentaliasing",	/* ARGUMENT ALIASING 	      */

			"Tok_SGI_Dir_Assert",		/* ASSERT      		      */
			"Tok_SGI_Dir_Barrier",		/* BARRIER		      */
			"Tok_SGI_Dir_Benign",		/* BENIGN                     */
			"Tok_SGI_Dir_Block",		/* BLOCK		      */
			"Tok_SGI_Dir_Blockable",	/* BLOCKABLE   		      */
			"Tok_SGI_Dir_Blocked",		/* BLOCKED		      */
			"Tok_SGI_Dir_Blockingsize",	/* BLOCKING SIZE	      */
			"Tok_SGI_Dir_Boundsviolations",	/* BOUNDS VIOLATIONS          */
			"Tok_SGI_Dir_Chunk",		/* CHUNK		      */
			"Tok_SGI_Dir_Concur",        	/* CONCUR                     */
			"Tok_SGI_Dir_Concurrent",    	/* CONCURRENT                 */
			"Tok_SGI_Dir_Concurrentcall",	/* CONCURRENT CALL            */
			"Tok_SGI_Dir_Concurrentize",   	/* CONCURRENTIZE              */
			"Tok_SGI_Dir_Copyin",		/* COPYIN		      */
			"Tok_SGI_Dir_Criticalsection",	/* CRITICAL SECTION	      */
			"Tok_SGI_Dir_Cyclic",		/* CYCLIC		      */
			"Tok_SGI_Dir_Data",		/* DATA			      */
			"Tok_SGI_Dir_Dependence",	/* DEPENDENCE                 */
			"Tok_SGI_Dir_Distribute",	/* DISTRIBUTE		      */
			"Tok_SGI_Dir_Distribute_Reshape",	/* DISTRIBUTE_RESHAPE	      */
			"Tok_SGI_Dir_Do",		/* DO                         */
			"Tok_SGI_Dir_Doacross",		/* DO ACROSS		      */
			"Tok_SGI_Dir_Doprefer",		/* DO PREFER                  */
			"Tok_SGI_Dir_Dynamic",		/* DYNAMIC		      */
			"Tok_SGI_Dir_Endcriticalsection",	/* END CRITICAL SECTION	      */
			"Tok_SGI_Dir_Endparallel",	/* END PARALLEL		      */
			"Tok_SGI_Dir_Endpdo",		/* END PDO		      */
			"Tok_SGI_Dir_Endpsection",	/* END PSECTION		      */
			"Tok_SGI_Dir_Endpsections",	/* END PSECTIONS	      */
			"Tok_SGI_Dir_Endsingleprocess",	/* END SINGLE PROCESS	      */
			"Tok_SGI_Dir_Equivalencehazard",	/* EQUIVALENCE HAZARD         */
			"Tok_SGI_Dir_Fill_Symbol",	/* FILL SYMBOL 		      */
			"Tok_SGI_Dir_Fission",		/* FISSION     		      */
			"Tok_SGI_Dir_Fissionable",	/* FISSIONABLE 		      */
			"Tok_SGI_Dir_Flush",      	/* FLUSH       		      */
			"Tok_SGI_Dir_Frequency",	/* FREQUENCY                  */
			"Tok_SGI_Dir_Fusable",		/* FUSABLE      	      */
			"Tok_SGI_Dir_Fuse",		/* FUSE        		      */
			"Tok_SGI_Dir_Global",		/* GLOBAL      		      */
			"Tok_SGI_Dir_Gss",		/* GSS			      */
			"Tok_SGI_Dir_Guided",		/* GUIDED		      */
			"Tok_SGI_Dir_Here",		/* HERE 	 	      */
			"Tok_SGI_Dir_If",		/* IF			      */
			"Tok_SGI_Dir_Ignoreanydependence",/* IGNORE ANY DEPENDENCE      */
			"Tok_SGI_Dir_Ignoreanydependences",/* IGNORE ANY DEPENDENCES    */
			"Tok_SGI_Dir_Ignoreassumeddependence",
                                                /* IGNORE ASSUMED DEPENDENCE  */
			"Tok_SGI_Dir_Ignoreassumeddependences",
                                                /* IGNORE ASSUMED DEPENDENCES */
			"Tok_SGI_Dir_Inline",		/* INLINE	       	      */
			"Tok_SGI_Dir_Interchange",	/* INTERCHANGE         	      */
			"Tok_SGI_Dir_Interleave",	/* INTERLEAVE		      */
			"Tok_SGI_Dir_Interleaved",	/* INTERLEAVED		      */
			"Tok_SGI_Dir_Ipa",		/* IPA	         	      */
			"Tok_SGI_Dir_Kind",		/* KIND       	 	      */
			"Tok_SGI_Dir_Lastlocal",	/* LASTLOCAL		      */
			"Tok_SGI_Dir_Lastthread",	/* LAST THREAD		      */
			"Tok_SGI_Dir_Lastvalueneeded",	/* LAST VALUE NEEDED          */
			"Tok_SGI_Dir_Lastvaluesneeded",	/* LAST VALUES NEEDED         */
			"Tok_SGI_Dir_Level",		/* LEVEL		      */
			"Tok_SGI_Dir_Limit",		/* LIMIT		      */
			"Tok_SGI_Dir_Local",		/* LOCAL		      */
			"Tok_SGI_Dir_L1cacheline",	/* L1CACHELINE                */
			"Tok_SGI_Dir_L2cacheline",	/* L2CACHELINE                */
			"Tok_SGI_Dir_Minconcurrent",	/* MINCONCURRENT	      */
			"Tok_SGI_Dir_Mp_Schedtype",	/* MP_SCHEDTYPE		      */
			"Tok_SGI_Dir_Ncpus_Chunk",	/* NCPUS_CHUNK		      */
			"Tok_SGI_Dir_Nest",		/* NEST			      */
			"Tok_SGI_Dir_Noargumentaliasing",	/* NOARGUMENTALIASING	      */
			"Tok_SGI_Dir_Noblocking",		/* NO BLOCKING  	      */
			"Tok_SGI_Dir_Noboundsviolations",	/* NO BOUNDS VIOLATIONS       */
			"Tok_SGI_Dir_Noconcurrentcall",	/* NO CONCURRENT CALL         */
			"Tok_SGI_Dir_Noconcurrentize",   	/* NO CONCURRENTIZE           */
			"Tok_SGI_Dir_Noequivalencehazard",/* NO EQUIVALENCE HAZARD      */
			"Tok_SGI_Dir_Nofission",		/* NO FISSION   	      */
			"Tok_SGI_Dir_Nofusion",		/* NO FUSION	   	      */
			"Tok_SGI_Dir_Noinline",		/* NO INLINE    	      */
			"Tok_SGI_Dir_Nointerchange",	/* NO INTERCHANGE	      */
			"Tok_SGI_Dir_Noipa",		/* NO IPA      		      */
			"Tok_SGI_Dir_Nolastvalueneeded",	/* NO LASTVALUE NEEDED        */
			"Tok_SGI_Dir_Nolastvaluesneeded",	/* NO LASTVALUES NEEDED       */
			"Tok_SGI_Dir_Norecurrence",	/* NO RECURRENCE              */
			"Tok_SGI_Dir_Nosync",		/* NO SYNC                    */
			"Tok_SGI_Dir_Notemporariesforconstantarguments",
                                     /* NO TEMPORARIES FOR CONSTANT ARGUMENTS */
			"Tok_SGI_Dir_Nowait",		/* NOWAIT		      */
			"Tok_SGI_Dir_Numchunks",	/* NUMCHUNKS		      */
			"Tok_SGI_Dir_Onto",		/* ONTO			      */
			"Tok_SGI_Dir_Opaque",		/* OPAQUE       	      */
			"Tok_SGI_Dir_Optional",		/* OPTIONAL     	      */
#ifdef KEY /* Bug 2660 */
			"Tok_SGI_Dir_Options",		/* OPTIONS     	      */
#endif /* KEY Bug 2660 */
			"Tok_SGI_Dir_Ordered",		/* ORDERED		      */
			"Tok_SGI_Dir_Page",      	/* PAGE      		      */
			"Tok_SGI_Dir_Page_Place",	/* PAGE_PLACE		      */
			"Tok_SGI_Dir_Parallel",		/* PARALLEL		      */
			"Tok_SGI_Dir_Paralleldo",	/* PARALLEL DO		      */
			"Tok_SGI_Dir_Pdo",		/* PDO			      */
			"Tok_SGI_Dir_Permutation",	/* PERMUTATION                */
			"Tok_SGI_Dir_Prefetch",		/* PREFETCH		      */
			"Tok_SGI_Dir_Prefetch_Manual",	/* PREFETCH_MANUAL	      */
			"Tok_SGI_Dir_Prefetch_Ref",	/* PREFETCH_REF		      */
			"Tok_SGI_Dir_Prefetch_Ref_Disable",/* PREFETCH_REF_DISABLE      */
			"Tok_SGI_Dir_Private",		/* PRIVATE		      */
			"Tok_SGI_Dir_Psection",		/* PSECTION		      */
			"Tok_SGI_Dir_Psections",		/* PSECTIONS		      */
			"Tok_SGI_Dir_Redistribute",	/* REDISTRIBUTE		      */
			"Tok_SGI_Dir_Reduction",	/* REDUCTION		      */
			"Tok_SGI_Dir_Regionbegin",	/* REGIONBEGIN		      */
			"Tok_SGI_Dir_Regionend",	/* REGIONEND		      */
			"Tok_SGI_Dir_Relation",		/* RELATION                   */
			"Tok_SGI_Dir_Routine",		/* ROUTINE		      */
			"Tok_SGI_Dir_Runtime",		/* RUNTIME		      */
			"Tok_SGI_Dir_Section",		/* SECTION		      */
			"Tok_SGI_Dir_Section_Gp",	/* SECTION_GP		      */
			"Tok_SGI_Dir_Section_Non_Gp",	/* SECTION_NON_GP	      */
			"Tok_SGI_Dir_Serial",		/* SERIAL		      */
			"Tok_SGI_Dir_Share",		/* SHARE		      */
			"Tok_SGI_Dir_Shared",		/* SHARED		      */
			"Tok_SGI_Dir_Simple",		/* SIMPLE		      */
			"Tok_SGI_Dir_Single",		/* SINGLE		      */
			"Tok_SGI_Dir_Singleprocess",	/* SINGLE PROCESS	      */
			"Tok_SGI_Dir_Size",		/* SIZE			      */
			"Tok_SGI_Dir_Static",		/* STATIC		      */
			"Tok_SGI_Dir_Stride",		/* STRIDE		      */
			"Tok_SGI_Dir_Unroll",		/* UNROLL		      */
			"Tok_SGI_Dir_Usecompress",	/* USE COMPRESS               */
			"Tok_SGI_Dir_Usecontrolledstore",	/* USE CONTROLLED STORE       */
			"Tok_SGI_Dir_Useexpand",	/* USE EXPAND                 */
			"Tok_SGI_Dir_Usegather",	/* USE GATHER                 */
			"Tok_SGI_Dir_Usescatter",	/* USE SCATTER                */
			"Tok_SGI_Dir_Temporariesforconstantarguments",
                                     /* TEMPORARIES FOR CONSTANT ARGUMENTS    */
			"Tok_SGI_Dir_Thread",		/* THREAD		      */
			"Tok_SGI_Dir_Tile",		/* TILE  		      */
			"Tok_SGI_Dir_Vector",		/* VECTOR		      */
			"Tok_SGI_Dir_End",		/* End of SGI directives      */

			"Tok_Open_Mp_Dir_Start",	/* Start of open mp directives*/
                	"Tok_Open_Mp_Dir_Affinity",	/* AFFINITY                   */
                	"Tok_Open_Mp_Dir_Atomic",	/* ATOMIC                     */
                	"Tok_Open_Mp_Dir_Barrier",	/* BARRIER                    */
                	"Tok_Open_Mp_Dir_Copyin",	/* COPYIN                     */
                	"Tok_Open_Mp_Dir_Copyprivate",	/* COPYPRIVATE          */ /* by jhs, 02/7/5 */
                	"Tok_Open_Mp_Dir_Critical",	/* CRITICAL                   */
                	"Tok_Open_Mp_Dir_Data",		/* DATA                       */
                	"Tok_Open_Mp_Dir_Default",	/* DEFAULT                    */
                	"Tok_Open_Mp_Dir_Distribute",	/* DISTRIBUTE                 */
                	"Tok_Open_Mp_Dir_Distribute_Reshape", /* DISTRIBUTE_RESHAPE     */
                	"Tok_Open_Mp_Dir_Do",           /* DO                         */
                	"Tok_Open_Mp_Dir_Dynamic",      /* DYNAMIC                    */
                	"Tok_Open_Mp_Dir_Endcritical",  /* END CRITICAL               */
                	"Tok_Open_Mp_Dir_Enddo",        /* END DO                     */
                	"Tok_Open_Mp_Dir_Endparallel",    /* END PARALLEL               */
                	"Tok_Open_Mp_Dir_Endparalleldo",  /* END PARALLEL DO            */
                	"Tok_Open_Mp_Dir_Endparallelsections", /* END PARALLEL SECTIONS */
                	"Tok_Open_Mp_Dir_Endparallelworkshare", /* END PARALLEL WORKSHARE */ 
                	"Tok_Open_Mp_Dir_Endmaster",      /* END MASTER                 */
                	"Tok_Open_Mp_Dir_Endordered",     /* END ORDERED                */
                	"Tok_Open_Mp_Dir_Endsections",    /* END SECTIONS               */
                	"Tok_Open_Mp_Dir_Endsingle",      /* END SINGLE                 */
                	"Tok_Open_Mp_Dir_Endworkshare", /* END WORKSHARE */ 
                	"Tok_Open_Mp_Dir_Firstprivate",   /* FIRSTPRIVATE               */
                	"Tok_Open_Mp_Dir_Flush",          /* FLUSH                      */
                	"Tok_Open_Mp_Dir_Guided",         /* GUIDED                     */
                	"Tok_Open_Mp_Dir_If",             /* IF                         */
                	"Tok_Open_Mp_Dir_Lastprivate",    /* LASTPRIVATE                */
                	"Tok_Open_Mp_Dir_Master",         /* MASTER                     */
                	"Tok_Open_Mp_Dir_Nest",           /* NEST                       */
                	"Tok_Open_Mp_Dir_None",           /* NONE                       */
                	"Tok_Open_Mp_Dir_Nowait",         /* NOWAIT                     */
                	"Tok_Open_Mp_Dir_Num_Threads", /* NUM_THREADS */ 
                	"Tok_Open_Mp_Dir_Onto",           /* ONTO                       */
                	"Tok_Open_Mp_Dir_Ordered",        /* ORDERED                    */
                	"Tok_Open_Mp_Dir_Page_Place",     /* PAGE PLACE                 */
                	"Tok_Open_Mp_Dir_Parallel",       /* PARALLEL                   */
                	"Tok_Open_Mp_Dir_Paralleldo",     /* PARALLEL DO                */
                	"Tok_Open_Mp_Dir_Parallelsections",  /* PARALLEL SECTIONS */
                	"Tok_Open_Mp_Dir_Parallelworkshare", /* PARALLEL WORKSHARE */ 
                	"Tok_Open_Mp_Dir_Private",        /* PRIVATE                    */
                	"Tok_Open_Mp_Dir_Redistribute",   /* REDISTRIBUTE               */
                	"Tok_Open_Mp_Dir_Reduction",      /* REDUCTION                  */
                	"Tok_Open_Mp_Dir_Runtime",        /* RUNTIME                    */
                	"Tok_Open_Mp_Dir_Schedule",       /* SCHEDULE                   */
                	"Tok_Open_Mp_Dir_Section",        /* SECTION                    */
                	"Tok_Open_Mp_Dir_Sections",       /* SECTIONS                   */
                	"Tok_Open_Mp_Dir_Shared",         /* SHARED                     */
                	"Tok_Open_Mp_Dir_Single",         /* SINGLE                     */
                	"Tok_Open_Mp_Dir_Static",         /* STATIC                     */
                	"Tok_Open_Mp_Dir_Thread",         /* THREAD                     */
                	"Tok_Open_Mp_Dir_Threadprivate",  /* THREADPRIVATE              */
                	"Tok_Open_Mp_Dir_Workshare", /* WORKSHARE */ 
                	"Tok_Open_Mp_Dir_End",            /* End of Open Mp directives  */
                	/* the above is added by jhs, 04/03/08 */
			
			"Tok_Dbg_Sytb",		/* Tok_Dbg_Sytb		*/
			"Tok_Dbg_Stmt",		/* Tok_Dbg_Stmt		*/
			
			"Tok_Punct_Colon",	/* Tok_Punct_Colon	*/
			"Tok_Punct_Colon_Colon",/* Tok_Punct_Colon_Colon*/
			"Tok_Punct_Comma",	/* Tok_Punct_Comma	*/
			"Tok_Punct_Dash",	/* Tok_Punct_Dash	*/
			"Tok_Punct_Eq",		/* Tok_Punct_Eq		*/
			"Tok_Punct_Lbrkt",	/* Tok_Punct_Lbrkt	*/
			"Tok_Punct_Lparen",	/* Tok_Punct_Lparen	*/
			"Tok_Punct_Rbrkt",	/* Tok_Punct_Rbrkt	*/
			"Tok_Punct_Rename",	/* Tok_Punct_Rename	*/
			"Tok_Punct_Rparen",	/* Tok_Punct_Rparen	*/
			"Tok_Punct_Slash",	/* Tok_Punct_Slash	*/
			"Tok_Punct_Star",	/* Tok_Punct_Star	*/
			"Tok_Const_False",	/* Tok_Const_False	*/
			"Tok_Const_True",	/* Tok_Const_True	*/
			"Tok_Const_Hollerith",	/* Tok_Const_Hollerith  */
			"Tok_Const_Boolean",	/* Tok_Const_Boolean	*/
			"Tok_Const_Boz",	/* Tok_Const_Boz	*/
			"Tok_Const_Char",	/* Tok_Const_Char	*/
			"Tok_Const_Int",	/* Tok_Const_Int	*/
			"Tok_Const_Real",	/* Tok_Const_Real	*/
			"Tok_Const_Dbl",	/* Tok_Const_Dbl	*/
			"Tok_Const_Quad",	/* Tok_Const_Quad	*/
			"Tok_Op_Add",		/* Tok_Op_Add		*/
			"Tok_Op_Div",		/* Tok_Op_Div		*/
			"Tok_Op_Mult",		/* Tok_Op_Mult		*/
			"Tok_Op_Power",		/* Tok_Op_Power		*/
			"Tok_Op_Sub",		/* Tok_Op_Sub		*/
			"Tok_Op_Concat",	/* Tok_Op_Concat	*/
			"Tok_Op_Eq",		/* Tok_Op_Eq		*/
			"Tok_Op_Ge",		/* Tok_Op_Ge		*/
			"Tok_Op_Gt",		/* Tok_Op_Gt		*/
			"Tok_Op_Le",		/* Tok_Op_Le		*/
			"Tok_Op_Lt",		/* Tok_Op_Lt		*/
			"Tok_Op_Ne",		/* Tok_Op_Ne		*/
			"Tok_Op_Lg",		/* Tok_Op_Lg		*/
			"Tok_Op_And",		/* Tok_Op_And		*/
			"Tok_Op_Eqv",		/* Tok_Op_Eqv		*/
			"Tok_Op_Neqv",		/* Tok_Op_Neqv		*/
			"Tok_Op_Not",		/* Tok_Op_Not		*/
			"Tok_Op_Or",		/* Tok_Op_Or		*/
			"Tok_Op_Assign",	/* Tok_Op_Assign	*/
			"Tok_Op_Defined",	/* Tok_Op_Defined	*/
			"Tok_Op_Deref",		/* Tok_Op_Deref		*/
			"Tok_Op_Ptr_Assign",	/* Tok_Op_Ptr_Assign	*/
			"Tok_Unknown",		/* Tok_Unknown		*/
			"Tok_EOS",		/* Tok_EOS		*/
			"Tok_EOF",		/* Tok_EOF		*/
			"Tok_LAST"		/* Tok_LAST		*/
		};

char		*type_char_class_str[]	= {
			"Unknown_Char",
			"Const_Len_Char",
			"Var_Len_Char",
			"Assumed_Size_Char"
		};


char		*type_desc_str[]	= {
			"Default_Typed",
			"Star_Typed",
			"Kind_Typed"
		};

char		*use_type_str[]		= {
			"Use_Not",
			"Use_All",
			"Use_Renamed",
			"Use_Only"
		};

char		*lin_type_str[]		= {
        /* Err_Res           */         "Err_Res",
        /* Short_Char_Const  */         "Short_Char_Const",
        /* Short_Typeless_Const */      "Short_Typeless_Const",
        /* Typeless_1        */         "Typeless_1",
        /* Typeless_2        */         "Typeless_2",
        /* Typeless_4        */         "Typeless_4",
        /* Typeless_8        */         "Typeless_8",
        /* Long_Typeless     */         "Long_Typeless",
        /* Integer_1         */         "Integer_1",
        /* Integer_2         */         "Integer_2",
        /* Integer_4         */         "Integer_4",
        /* Integer_8         */         "Integer_8",
        /* Real_4            */         "Real_4",
        /* Real_8            */         "Real_8",
        /* Real_16           */         "Real_16",
        /* Complex_4         */         "Complex_4",
        /* Complex_8         */         "Complex_8",
        /* Complex_16        */         "Complex_16",
        /* CRI_Ptr_8         */         "CRI_Ptr_8",
        /* Logical_1         */         "Logical_1",
        /* Logical_2         */         "Logical_2",
        /* Logical_4         */         "Logical_4",
        /* Logical_8         */         "Logical_8",
        /* Character_1       */         "Character_1",
        /* Character_2       */         "Character_2",
        /* Character_4       */         "Character_4",
        /* CRI_Ch_Ptr_8      */         "CRI_Ch_Ptr_8",
        /* Structure_Type    */         "Structure_Type",
        /* CRI_Parcel_Ptr_8  */		"CRI_Parcel_Ptr_8"
                                                };


char	*opr_str[]	= 	{
		"Null_Opr",
		"Defined_Un_Opr",
		"Readsm_Opr",
		"Alloc_Opr",
		"SSD_Alloc_Opr",
		"Cvrt_Opr",
		"Dealloc_Opr",
		"**",
		"*",
		"/",
		"+",
		"-",
		"+",
		"-",
		"||",
		".eq.",
		".ne.",
		".lt.",
		".le.",
		".gt.",
		".ge.",
		".not.",
		"not(",
		".and.",
		".or.",
		"or(",
		".eqv.",
		"eqv(",
		".neqv.",
		"neqv(",
		"abs(",
		"cos(",
		"sin(",
		"Log_E_Opr",
		"Log_10_Opr",
		"tan(",
		"tanh(",
		"sinh(",
		"acos(",
		"asin(",
		"atan(",
		"cosh(",
		"atan2(",
		"aimag(",
		"sqrt(",
		"cot(",
		"exp(",
		"int(",
		"and(",
		"mod(",
		"anint(",
		"nint(",
		"sign(",
		"module(",
		"shift(",
		"shiftl(",
		"shiftr(",
		"leadz(",
		"popcnt(",
		"poppar(",
		"aint(",
		"dim(",
		"ranget(",
		"ranset(",
		"ranf(",
		"real(",
		"double(",
		"mask(",
		"conjg(",
		"dprod(",
		"i24mult(",
		"length(",
		"getpos(",
		"unit(",
		"cmplx(",
		"ichar(",
		"char(",
		"lint(",
		"index(",
		"lge(",
		"lgt(",
		"lle(",
		"llt(",
		"Fcd_Opr",
		"Numarg_Opr",
		"Rtc_Opr",
		"cvmgp(",
		"cvmgm(",
		"cvmgz(",
		"cvmgn(",
		"cvmgt(",
		"csmg(",
		"adjustl(",
		"adjustr(",
		"ceiling(",
		"exponent(",
		"floor(",
		"fraction(",
		"spacing(",
		"logical(",
		"nearest(",
		"rrspacing(",
		"scale(",
		"scan(",
		"set_Exponent_Opr",
		"verify(",
		"len_Trim(",
		"dshiftl(",
		"dshiftr(",
		"mmx(",
		"mldmx(",
		"mld(",
		"mul(",
		"mcbl(",
		"cshift(",
		"dot_Product(",
		"matmul(",
		"spread(",
		"transpose(",
		"all(",
		"any(",
		"count(",
		"product(",
		"sum(",
		"eoshift(",
		"maxval(",
		"minval(",
		"maxloc(",
		"minloc(",
		"reshape(",
		"selected_real_kind(",
		"selected_int_kind(",
		"repeat(",
		"trim(",
		"transfer(",
		"Defined_Bin_Opr",
		"=",
		"CALL ",
		"RETURN ",
		"CASE ",
		"ALLOCATE ",
		"DEALLOCATE ",
		"END ",
		"ENTRY",
		"FORALL",
		"IF",
		"NULLIFY ",
		"PAUSE ",
		"=>",
		"Flat_Array_Asg_Opr",
		"RETURN ",
		"SELECT ",
		"Stmt_Func_Call_Opr",
		"STOP ",
		"MAX(",
		"MIN(",
		"READ(",
		"READ(",
		"READ(",
		"WRITE(",
		"WRITE(",
		"WRITE(",
		"Inquire_Iolength_Opr",
		"Dv_Whole_Copy_Opr",
		"Dv_Whole_Def_Opr",
		"Dv_Deref_Opr",
		"Dv_Access_Base_Addr",
		"Dv_Set_Base_Addr",
		"Dv_Access_El_Len",
		"Dv_Set_El_Len",
		"Dv_Access_Assoc",
		"Dv_Set_Assoc",
		"Dv_Access_Ptr_Alloc",
		"Dv_Set_Ptr_Alloc",
		"Dv_Access_P_Or_A",
		"Dv_Set_P_Or_A",
		"Dv_Access_Unused",
		"Dv_Set_Unused",
		"Dv_Access_N_Dim",
		"Dv_Set_N_Dim",
		"Dv_Access_Typ_Code",
		"Dv_Set_Typ_Code",
		"Dv_Access_Orig_Base",
		"Dv_Set_Orig_Base",
		"Dv_Access_Orig_Size",
		"Dv_Set_Orig_Size",
		"Dv_Access_Low_Bound",
		"Dv_Set_Low_Bound",
		"Dv_Access_Extent",
		"Dv_Set_Extent",
		"Dv_Access_Stride_Mult",
		"Dv_Set_Stride_Mult",
		"Br_Aif_Opr",
		"Br_Asg_Opr",
		"Br_Index_Opr",
		"Br_True_Opr",
		"Br_Uncond_Opr",
		"Case_Range_Opr",
		"Implied_Do_Opr",
		"Kwd_Opr",
		"Loc_Opr",
		"Aloc_Opr",
		"Const_Tmp_Loc_Opr",
		"Len_Opr",
		"Clen_Opr",
		"(",
		"Struct_Opr",
		"Struct_Construct_Opr",
		"Array_Construct_Opr",
		"Constant_Struct_Construct_Opr",
		"Constant_Array_Construct_Opr",
		"(",
		"Whole_Subscript_Opr",
		"Section_Subscript_Opr",
		"Alloc_Obj_Opr",
		"Dealloc_Obj_Opr",
		"Substring_Opr",
		"Whole_Substring_Opr",
		"Triplet_Opr",
		"Label_Opr",
		"Loop_Info_Opr",
		"Loop_End_Opr",
		"Init_Opr",
		"Init_Reloc_Opr",
		"USE",
		"WHERE (",
		"Real_Div_To_Int_Opr",
		"!DIR$ SUPPRESS",
		"!DIR$ VECTOR",
		"!DIR$ NOVECTOR",
		"!DIR$ TASK",
		"!DIR$ NOTASK",
		"!DIR$ BOUNDS",
		"!DIR$ NOBOUNDS",
		"!DIR$ RECURRENCE",
		"!DIR$ NORECURRENCE",
		"!DIR$ VSEARCH",
		"!DIR$ NOVSEARCH",
		"!DIR$ BL",
		"!DIR$ NOBL",
		"!DIR$ INLINE",
		"!DIR$ NOINLINE",
		"!DIR$ IVDEP",
		"!DIR$ NEXTSCALAR",
		"!DIR$ SHORTLOOP",
		"!DIR$ SHORTLOOP128",
		"!DIR$ ALIGN",
		"!MIC$ CASE",
		"!MIC$ ENDCASE",
		"!MIC$ CONTINUE",
		"!MIC$ DOALL",
		"!MIC$ DOPARALLEL",
		"!MIC$ ENDDO",
		"!MIC$ GUARD",
		"!MIC$ ENDGUARD",
		"!MIC$ PARALLEL",
		"!MIC$ ENDPARALLEL",
		"!MIC$ TASKCOMMON",
		"!MIC$ WAIT",
		"!MIC$ SEND",
                "My_Pe_Opr",
                "Ieee_Unordered_Opr",
                "Ieee_Next_After_Opr",
                "Ieee_Remainder_Opr",
                "Ieee_Exponent_Opr",
                "Ieee_Copy_Sign_Opr",
                "Ieee_Int_Opr",
                "Ieee_Real_Opr",
                "Ieee_Finite_Opr",
                "Ieee_Is_Nan_Opr",
                "Ieee_Class_Opr",
                "Ieee_Binary_Scale_Opr",
                "Int_Mult_Upper_Opr",
		"Get_Ieee_Status_Opr",
                "Set_Ieee_Status_Opr",
                "Get_Ieee_Exceptions_Opr",
                "Set_Ieee_Exceptions_Opr",
                "Get_Ieee_Interrupts_Opr",
                "Set_Ieee_Interrupts_Opr",
                "Get_Ieee_Rounding_Mode_Opr",
                "Set_Ieee_Rounding_Mode_Opr",
                "Test_Ieee_Interrupt_Opr",
                "Test_Ieee_Exception_Opr",
                "Set_Ieee_Exception_Opr",
                "Clear_Ieee_Exception_Opr",
                "Enable_Ieee_Interrupt_Opr",
                "Disable_Ieee_Interrupt_Opr",
		"Cvrt_Unsigned_Opr",
		"SSD_Dealloc_Opr",

		"The_Last_Opr"};

static char	*dv_whole_def_str[] = {
			"BASE ADDR",
			"EL_LEN",
			"ASSOC",
			"PTR_ALLOC",
			"P_OR_A",
			"A_CONTIG",
			"N_DIM",

# define DEBUG_STR_TYPE_CODE	7

			"TYPE_CODE",
			"ORIG_BASE",
			"ORIG_SIZE",
#ifdef KEY /* Bug 6845 */
			"ALLOC_CPN",
#else /* KEY Bug 6845 */
			"DIM %d LB",
			"DIM %d EX",
			"DIM %d SM",
                        "DIM %d LB",
                        "DIM %d EX",
                        "DIM %d SM",
                        "DIM %d LB",
                        "DIM %d EX",
                        "DIM %d SM",
                        "DIM %d LB",
                        "DIM %d EX",
                        "DIM %d SM",
                        "DIM %d LB",
                        "DIM %d EX",
                        "DIM %d SM",
                        "DIM %d LB",
                        "DIM %d EX",
                        "DIM %d SM",
                        "DIM %d LB",
                        "DIM %d EX",
                        "DIM %d SM"
#endif /* KEY Bug 6845 */
		};

#ifdef KEY /* Bug 6845 */
static char *dv_whole_def_bound_str[] = {
			"DIM %d LB",
			"DIM %d EX",
			"DIM %d SM"
		};
static char *dv_whole_def_alloc_cpnt_str = "ALLOC CPNT %d OFFSET";
#endif /* KEY Bug 6845 */

static char *open_mp_dir_opr_str[] = {
                         "IF clause",
			 "NUM_THREADS clause",
                         "PRIVATE clause",
                         "SHARED clause",
                         "FIRSTPRIVATE clause",
                         "DEFAULT clause",
                         "COPYIN clause",
                         "REDUCTION opr clause",
                         "REDUCTION var clause",
                         "LASTPRIVATE clause",
                         "ORDERED clause",
                         "SCHEDULE type",
                         "SCHEDULE list",
			 "AFFINITY clause",
			 "IS_THREAD clause",
			 "THREAD_DATA clause",
			 "ONTO clause",
			 "NEST clause",
                         "COPYPRIVATE clause"
		};

static char *mp_dir_opr_str[] = {
			"IF clause",
			"SHARE clause",
			"LASTLOCAL clause",
			"REDUCTION clause",
			"MP_SCHEDTYPE clause",
			"CHUNK clause",
			"AFFINITY clause",
			"IS_THREAD clause",
			"THREAD_DATA clause",
			"LOCAL clause",
			"ONTO clause",
			"NEST clause",
			"LASTTHREAD clause",
			"ORDERED clause",
		};
