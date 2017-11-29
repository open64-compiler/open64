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



/* USMID:  "\n@(#)5.0_pl/headers/globals.h	5.44	10/27/99 17:06:38\n" */

/***************************************\
|* globally accessible type specifiers *|
\***************************************/

enum	addr_mode_values	 {Addr_Full,		
				  Addr_Fast };

enum	comp_phase_values	 {Cmdline_Parsing,
				  Pass1_Parsing,
				  Lex_Parsing,
				  Decl_Semantics,
				  Pass2_Semantics,
                                  Inlining,
				  Pdg_Conversion 
				 };

enum	convert_to_string_values {Dont_Care,
  				  Binary_Fmt,
				  Octal_Fmt,
			  	  Hex_Fmt,
				  Character_Fmt 
				 };

enum	basic_type_values	 {Integer,
				  Logical,
				  Real,
				  Complex,		
				  CRI_Ptr,
				  CRI_Parcel_Ptr,
				  CRI_Ch_Ptr,
                                  Typeless,
                                  Character,
				  Structure,
				  Num_Basic_Types,
                                  Last_Linear_Type	= CRI_Ch_Ptr
				 };

enum	debug_lvl_values         {Debug_Lvl_0,		
				  Debug_Lvl_1,
				  Debug_Lvl_2,		
				  Debug_Lvl_3,
				  No_Debugging
           			 };

enum	expr_mode_values	 {Initialization_Expr,
				  Specification_Expr,
				  Stmt_Func_Expr,
				  Data_Stmt_Target,
				  Data_Stmt_Target_Expr,
				  Restricted_Imp_Do_Target,
				  Restricted_Imp_Do_Expr,
				  Regular_Expr
				 };	

enum	fld_values	         {NO_Tbl_Idx,		
				  CN_Tbl_Idx,
				  SB_Tbl_Idx,		
				  IL_Tbl_Idx,
			 	  AT_Tbl_Idx,		
				  SH_Tbl_Idx,
				  IR_Tbl_Idx
				 };

enum	fortran_type_values	{Fortran_Integer,
				 Fortran_Logical,
				 Fortran_Real,
				 Fortran_Double,
				 Fortran_Complex,		
				 Fortran_Double_Complex,		
                                 Fortran_Character,
                                 Fortran_Typeless,
				 Num_Fortran_Types
				};

enum    tasking_scope_values    {Priv,       Shared};
typedef enum tasking_scope_values       task_scope_type;

/* The following enum is used by label processing to determine the type of    */
/* forward reference an IL entry represents when it is being used as a        */
/* Forward Ref entry.  The values of these enums are stored in the "rank"     */
/* field of the IL_OPND.  See also IL_FORWARD_REF in sytb.m and its use in    */
/* p_utils.c.								      */

enum	forward_ref_values	{From_Assign_Stmt,
				 From_Do_Stmt,
				 To_Format_Stmt 
				};

/* These enums are used to access a table that holds all global indexes to    */
/* the symbol tables.  These are kept in a table, so they can be cleared out  */
/* easily.  If additions are made, the only other spot that needs to changed  */
/* is compress tables.  These have to be cleared or reset at that time.       */

enum	glb_tbl_idx_values	{Allocate_Attr_Idx,
				 Argchck_Attr_Idx,
				 Backspace_Attr_Idx,
				 Buffer_In_Attr_Idx,
				 Buffer_Out_Attr_Idx,
				 Close_Attr_Idx,
				 Dealloc_Attr_Idx,
				 Deallocate_Attr_Idx,
				 End_Attr_Idx,
				 Endfile_Attr_Idx,
				 Inquire_Attr_Idx,
				 Main_Attr_Idx,
				 Open_Attr_Idx,
				 Pause_Attr_Idx,
				 Realloc_Attr_Idx,
				 Rewind_Attr_Idx,
				 Stop_Attr_Idx,
				 Conform_Attr_Idx,
				 Bounds_Attr_Idx,
				 Rbounds_Attr_Idx,
				 Sbounds_Attr_Idx,
				 Ptr_Chk_Attr_Idx,
				 Pe_Offset_Attr_Idx,
				 Set_Numthreads_Attr_Idx,
#ifdef KEY /* Bug 8117 */
				 Copyin_Attr_Idx,
				 Copyout_Attr_Idx,
#endif /* KEY Bug 8117 */
#ifdef KEY /* Bug 5089 */
				 Ieee_Save_Attr_Idx,
				 Ieee_Restore_Attr_Idx,
#endif  /* KEY Bug 5089 */
#ifdef KEY /* Bug 6845 */
                                 Assign_Allocatable_Idx,
#endif /* KEY Bug 6845 */
				 Num_Glb_Tbl_Idxs
				};


enum	inline_lvl_values      {Inline_Lvl_0,
				Inline_Lvl_1,
				Inline_Lvl_2,
				Inline_Lvl_3,
				Inline_Lvl_4,
				Inline_Lvl_Err };

enum    short_circuit_lvl_values        {Short_Circuit_Off,
                                         Short_Circuit_Present,
                                         Short_Circuit_Left_Right,
                                         Short_Circuit_Functions};

typedef enum short_circuit_lvl_values short_circuit_lvl_type;

enum	sgi_inline_state_values	{Not_Specified_Sgi,
				 Inline_Sgi,
				 Noinline_Sgi };

typedef enum sgi_inline_state_values sgi_inline_state_type;
				 
#ifdef KEY /* Bug 4656 */
/* Bitmask values used for "family" member of intrin_tbl_entry; make sure
 * "families" member of intrin_tbl_entry is wide enough. */
enum    intrinsic_family       {
			        EVERY_FAMILY =	1,
			        ANSI_FAMILY =	2,
			        PGI_FAMILY =	4,
			        G77_FAMILY =	8,
				OMP_FAMILY =	16,
				TRADITIONAL_FAMILY = 32
			       };
/* Map name of intrinsic family onto mask used within "families" member of
 * intrin_tbl_entry. */
typedef struct {
  char *name;
  unsigned mask;
  } intrin_family_t;
extern intrin_family_t intrin_families[];
extern int SIZEOF_INTRIN_FAMILIES;
#endif /* KEY Bug 4656 */

enum    intrinsic_values       {Unknown_Intrinsic,
			        Abs_Intrinsic,
			        Achar_Intrinsic,
				Acos_Intrinsic,
				Acosd_Intrinsic,
				Add_And_Fetch_Intrinsic,
				Adjustl_Intrinsic,
				Adjustr_Intrinsic,
				Aimag_Intrinsic,
				Aint_Intrinsic,
				All_Intrinsic,
				Allocated_Intrinsic,
				Alog_Intrinsic,
				Alog10_Intrinsic,
				Amax0_Intrinsic,
				Amax1_Intrinsic,
				Amin0_Intrinsic,
				Amin1_Intrinsic,
				Amod_Intrinsic,
				And_Intrinsic,
				And_And_Fetch_Intrinsic,
				Anint_Intrinsic,
				Any_Intrinsic,
				Asin_Intrinsic,
				Asind_Intrinsic,
				Associated_Intrinsic,
				Atan_Intrinsic,
				Atan2_Intrinsic,
				Atan2d_Intrinsic,
				Atand_Intrinsic,
				Bitest_Intrinsic,
				Bit_Size_Intrinsic,
				Bjtest_Intrinsic,
				Bktest_Intrinsic,
				Btest_Intrinsic,
				Cabs_Intrinsic,
				Ccos_Intrinsic,
				Cdabs_Intrinsic,
				Cdcos_Intrinsic,
				Cdexp_Intrinsic,
				Cdlog_Intrinsic,
				Cdsin_Intrinsic,
				Cdsqrt_Intrinsic,
				Ceiling_Intrinsic,
				Cexp_Intrinsic,
				Char_Intrinsic,
				Clear_Ieee_Exception_Intrinsic,
				Cloc_Intrinsic,
				Clock_Intrinsic,
				Clog_Intrinsic,
				Cmplx_Intrinsic,
				Compare_And_Swap_Intrinsic,
				Compl_Intrinsic,
				Conjg_Intrinsic,
				Cos_Intrinsic,
				Cosd_Intrinsic,
				Cosh_Intrinsic,
				Cot_Intrinsic,
				Count_Intrinsic,
				Cputime_Intrinsic,
				Cqabs_Intrinsic,
				Cqcos_Intrinsic,
				Cqexp_Intrinsic,
				Cqlog_Intrinsic,
				Cqsin_Intrinsic,
				Cqsqrt_Intrinsic,
				Cshift_Intrinsic,
				Csin_Intrinsic,
				Csmg_Intrinsic,
				Csqrt_Intrinsic,
				Cvmgm_Intrinsic,
				Cvmgn_Intrinsic,
				Cvmgp_Intrinsic,
				Cvmgt_Intrinsic,
				Cvmgz_Intrinsic,
#ifdef KEY /* Bug 14150 */
				C_F_Pointer_Intrinsic,
				C_F_Procpointer_Intrinsic,
				C_Funloc_Intrinsic,
				C_Loc_Iso_Intrinsic,
#endif /* KEY Bug 14150 */
				C_Loc_Intrinsic,
				Dabs_Intrinsic,
				Dacos_Intrinsic,
				Dacosd_Intrinsic,
				Dasin_Intrinsic,
				Dasind_Intrinsic,
				Datan_Intrinsic,
				Datan2_Intrinsic,
				Datan2d_Intrinsic,
				Datand_Intrinsic,
				Date_Intrinsic,
				Date_And_Time_Intrinsic,
				Dble_Intrinsic,
				Dbleq_Intrinsic,
				Dcmplx_Intrinsic,
				Dconjg_Intrinsic,
				Dcos_Intrinsic,
				Dcosd_Intrinsic,
				Dcosh_Intrinsic,
				Dcot_Intrinsic,
				Ddim_Intrinsic,
#ifdef KEY /* Bug 1324 */
				Derf_Intrinsic,
				Derfc_Intrinsic,
#endif /* KEY Bug 1324 */
				Dexp_Intrinsic,
				Dfloat_Intrinsic,
				Dfloati_Intrinsic,
				Dfloatj_Intrinsic,
				Dfloatk_Intrinsic,
				Digits_Intrinsic,
				Dim_Intrinsic,
				Dimag_Intrinsic,
				Dint_Intrinsic,
				Disable_Ieee_Interrupt_Intrinsic,
				Dlog_Intrinsic,
				Dlog10_Intrinsic,
				Dmax1_Intrinsic,
				Dmin1_Intrinsic,
				Dmod_Intrinsic,
				Dnint_Intrinsic,
				Dot_Product_Intrinsic,
				Dprod_Intrinsic,
				Dreal_Intrinsic,
				Dshiftl_Intrinsic,
				Dshiftr_Intrinsic,
				Dsign_Intrinsic,
				Dsin_Intrinsic,
				Dsind_Intrinsic,
				Dsinh_Intrinsic,
                                DSM_Chunksize_Intrinsic,
                                DSM_Distribution_Block_Intrinsic,
                                DSM_Distribution_Cyclic_Intrinsic,
                                DSM_Distribution_Star_Intrinsic,
                                DSM_Isdistributed_Intrinsic,
                                DSM_Isreshaped_Intrinsic,
                                DSM_Numchunks_Intrinsic,
                                DSM_Numthreads_Intrinsic,
                                DSM_Rem_Chunksize_Intrinsic,
                                DSM_This_Chunksize_Intrinsic,
                                DSM_This_Startingindex_Intrinsic,
                                DSM_This_Threadnum_Intrinsic,
				Dsqrt_Intrinsic,
				Dtan_Intrinsic,
				Dtand_Intrinsic,
				Dtanh_Intrinsic,
# ifdef KEY
                                Dtime_Intrinsic,
# endif
				Enable_Ieee_Interrupt_Intrinsic,
				Eoshift_Intrinsic,
				Epsilon_Intrinsic,
				Eqv_Intrinsic,
#ifdef KEY /* Bug 1324 */
				Erf_Intrinsic,
				Erfc_Intrinsic,
#endif /* KEY Bug 1324 */
#ifdef KEY /* Bug 3018 */
				Etime_Intrinsic,
#endif /* KEY Bug 3018 */
				Exit_Intrinsic,
				Exp_Intrinsic,
				Exponent_Intrinsic,
				Fcd_Intrinsic,
# ifdef KEY
				Fdate_Intrinsic,
# endif
				Fetch_And_Add_Intrinsic,
				Fetch_And_And_Intrinsic,
				Fetch_And_Nand_Intrinsic,
				Fetch_And_Or_Intrinsic,
				Fetch_And_Sub_Intrinsic,
				Fetch_And_Xor_Intrinsic,
				Float_Intrinsic,
				Floati_Intrinsic,
				Floatj_Intrinsic,
				Floatk_Intrinsic,
				Floor_Intrinsic,
# ifdef KEY
				Fnum_Intrinsic,
# endif
				Fp_Class_Intrinsic,
				Fraction_Intrinsic,
				Free_Intrinsic,
#ifdef KEY
                                Fstat_Intrinsic,
#endif
				Getpos_Intrinsic,
				Get_Ieee_Exceptions_Intrinsic,
				Get_Ieee_Interrupts_Intrinsic,
				Get_Ieee_Rounding_Mode_Intrinsic,
				Get_Ieee_Status_Intrinsic,
				Huge_Intrinsic,
				Iabs_Intrinsic,
				Iachar_Intrinsic,
				Iand_Intrinsic,
				Ibchng_Intrinsic,
				Ibclr_Intrinsic,
				Ibits_Intrinsic,
				Ibset_Intrinsic,
				Ichar_Intrinsic,
				Idate_Intrinsic,
				Idim_Intrinsic,
				Idint_Intrinsic,
				Idnint_Intrinsic,
				Ieee_Binary_Scale_Intrinsic,
				Ieee_Class_Intrinsic,
				Ieee_Copy_Sign_Intrinsic,
				Ieee_Exponent_Intrinsic,
				Ieee_Finite_Intrinsic,
				Ieee_Int_Intrinsic,
				Ieee_Is_Nan_Intrinsic,
				Ieee_Next_After_Intrinsic,
				Ieee_Real_Intrinsic,
				Ieee_Remainder_Intrinsic,
				Ieee_Unordered_Intrinsic,
				Ieor_Intrinsic,
				Ifix_Intrinsic,
				Iiabs_Intrinsic,
				Iiand_Intrinsic,
				Iibchng_Intrinsic,
				Iibclr_Intrinsic,
				Iibits_Intrinsic,
				Iibset_Intrinsic,
				Iidim_Intrinsic,
				Iidint_Intrinsic,
				Iieor_Intrinsic,
				Iifix_Intrinsic,
				Iint_Intrinsic,
				Iior_Intrinsic,
				Iiqint_Intrinsic,
				Iisha_Intrinsic,
				Iishc_Intrinsic,
				Iishft_Intrinsic,
				Iishftc_Intrinsic,
				Iishl_Intrinsic,
				Iisign_Intrinsic,
				Ilen_Intrinsic,
				Imag_Intrinsic,
				Imod_Intrinsic,
				Imvbits_Intrinsic,
				Index_Intrinsic,
				Inint_Intrinsic,
				Inot_Intrinsic,
				Int_Intrinsic,
				Int1_Intrinsic,
				Int2_Intrinsic,
				Int4_Intrinsic,
				Int8_Intrinsic,
				Int_Mult_Upper_Intrinsic,
				Ior_Intrinsic,
				Iqint_Intrinsic,
				Iqnint_Intrinsic,
				Irtc_Intrinsic,
				Isha_Intrinsic,
				Ishc_Intrinsic,
				Ishft_Intrinsic,
				Ishftc_Intrinsic,
				Ishl_Intrinsic,
				Isign_Intrinsic,
				Isnan_Intrinsic,
				Jdate_Intrinsic,
				Jiabs_Intrinsic,
				Jiand_Intrinsic,
				Jibchng_Intrinsic,
				Jibclr_Intrinsic,
				Jibits_Intrinsic,
				Jibset_Intrinsic,
				Jidim_Intrinsic,
				Jidint_Intrinsic,
				Jieor_Intrinsic,
				Jifix_Intrinsic,
				Jint_Intrinsic,
				Jior_Intrinsic,
				Jiqint_Intrinsic,
				Jisha_Intrinsic,
				Jishc_Intrinsic,
				Jishft_Intrinsic,
				Jishftc_Intrinsic,
				Jishl_Intrinsic,
				Jisign_Intrinsic,
				Jmod_Intrinsic,
				Jmvbits_Intrinsic,
				Jnint_Intrinsic,
				Jnot_Intrinsic,
				Kiabs_Intrinsic,
				Kiand_Intrinsic,
				Kibchng_Intrinsic,
				Kibclr_Intrinsic,
				Kibits_Intrinsic,
				Kibset_Intrinsic,
				Kidim_Intrinsic,
				Kidint_Intrinsic,
				Kieor_Intrinsic,
				Kifix_Intrinsic,
#ifdef KEY
				Kill_Intrinsic,
#endif
				Kind_Intrinsic,
				Kint_Intrinsic,
				Kior_Intrinsic,
				Kiqint_Intrinsic,
				Kisha_Intrinsic,
				Kishc_Intrinsic,
				Kishft_Intrinsic,
				Kishftc_Intrinsic,
				Kishl_Intrinsic,
				Kisign_Intrinsic,
				Kmod_Intrinsic,
				Kmvbits_Intrinsic,
				Knint_Intrinsic,
				Knot_Intrinsic,
				Lbound_Intrinsic,
				Leadz_Intrinsic,
				Len_Intrinsic,
				Length_Intrinsic,
				Len_Trim_Intrinsic,
				Lge_Intrinsic,
				Lgt_Intrinsic,
				Lle_Intrinsic,
				Llt_Intrinsic,
				Loc_Intrinsic,
				Lock_Release_Intrinsic,
				Lock_Test_And_Set_Intrinsic,
				Log_Intrinsic,
				Log10_Intrinsic,
				Log2_Images_Intrinsic,
				Logical_Intrinsic,
				Long_Intrinsic,
				Lshift_Intrinsic,
				Mclr_Intrinsic,
				Mld_Intrinsic,
				Mldmx_Intrinsic,
				Mmx_Intrinsic,
				Mul_Intrinsic,
				Malloc_Intrinsic,
				Mask_Intrinsic,
				Matmul_Intrinsic,
				Max_Intrinsic,
				Max0_Intrinsic,
				Max1_Intrinsic,
				Maxexponent_Intrinsic,
				Maxloc_Intrinsic,
				Maxval_Intrinsic,
				Memory_Barrier_Intrinsic,
				Merge_Intrinsic,
				Min_Intrinsic,
				Min0_Intrinsic,
				Min1_Intrinsic,
				Minexponent_Intrinsic,
				Minloc_Intrinsic,
				Minval_Intrinsic,
				Mod_Intrinsic,
				Modulo_Intrinsic,
				Mvbits_Intrinsic,
                                My_Pe_Intrinsic,
				Nand_And_Fetch_Intrinsic,
				Nearest_Intrinsic,
				Neqv_Intrinsic,
				Nint_Intrinsic,
				Not_Intrinsic,
				Null_Intrinsic,
				Numarg_Intrinsic,
				Num_Images_Intrinsic,
#ifdef KEY
                                Omp_Destroy_Lock_Intrinsic,
                                Omp_Destroy_Nest_Lock_Intrinsic,
#endif
				Omp_Get_Dynamic_Intrinsic,
				Omp_Get_Max_Threads_Intrinsic,
				Omp_Get_Nested_Intrinsic,
				Omp_Get_Num_Procs_Intrinsic,
				Omp_Get_Num_Threads_Intrinsic,
				Omp_Get_Thread_Num_Intrinsic,
#ifdef KEY
                                Omp_Get_Wtick_Intrinsic,
                                Omp_Get_Wtime_Intrinsic,
                                Omp_Init_Lock_Intrinsic,
                                Omp_Init_Nest_Lock_Intrinsic,
#endif
				Omp_In_Parallel_Intrinsic,
				Omp_Set_Lock_Intrinsic,
#ifdef KEY
				Omp_Set_Nest_Lock_Intrinsic,
#endif
				Omp_Test_Lock_Intrinsic,
#ifdef KEY
				Omp_Test_Nest_Lock_Intrinsic,
#endif
				Omp_Unset_Lock_Intrinsic,
#ifdef KEY
				Omp_Unset_Nest_Lock_Intrinsic,
#endif
				Or_Intrinsic,
				Or_And_Fetch_Intrinsic,
				Pack_Intrinsic,
				Popcnt_Intrinsic,
				Poppar_Intrinsic,
				Precision_Intrinsic,
				Present_Intrinsic,
				Product_Intrinsic,
				Qabs_Intrinsic,
				Qacos_Intrinsic,
				Qacosd_Intrinsic,
				Qasin_Intrinsic,
				Qasind_Intrinsic,
				Qatan_Intrinsic,
				Qatan2_Intrinsic,
				Qatan2d_Intrinsic,
				Qatand_Intrinsic,
				Qcmplx_Intrinsic,
				Qcos_Intrinsic,
				Qcosd_Intrinsic,
				Qcosh_Intrinsic,
				Qcot_Intrinsic,
				Qdim_Intrinsic,
				Qexp_Intrinsic,
				Qext_Intrinsic,
				Qfloat_Intrinsic,
				Qfloati_Intrinsic,
				Qfloatj_Intrinsic,
				Qfloatk_Intrinsic,
				Qimag_Intrinsic,
				Qint_Intrinsic,
				Qlog_Intrinsic,
				Qlog10_Intrinsic,
				Qmod_Intrinsic,
				Qnint_Intrinsic,
				Qprod_Intrinsic,
				Qreal_Intrinsic,
				Qsign_Intrinsic,
				Qsin_Intrinsic,
				Qsind_Intrinsic,
				Qsinh_Intrinsic,
				Qsqrt_Intrinsic,
				Qtan_Intrinsic,
				Qtand_Intrinsic,
				Qtanh_Intrinsic,
				Radix_Intrinsic,
				Ran_Intrinsic,
				Random_Number_Intrinsic,
				Random_Seed_Intrinsic,
				Randu_Intrinsic,
				Ranf_Intrinsic,
				Range_Intrinsic,
				Ranget_Intrinsic,
				Ranset_Intrinsic,
				Readsm_Intrinsic,
				Real_Intrinsic,
				Remote_Write_Barrier_Intrinsic,
				Rem_Images_Intrinsic,
				Repeat_Intrinsic,
				Reshape_Intrinsic,
				Rrspacing_Intrinsic,
				Rshift_Intrinsic,
				Rtc_Intrinsic,
				Scale_Intrinsic,
				Scan_Intrinsic,
				SIK_Intrinsic,
				SRK_Intrinsic,
				Set_Exponent_Intrinsic,
				Set_Ieee_Exception_Intrinsic,
				Set_Ieee_Exceptions_Intrinsic,
				Set_Ieee_Interrupts_Intrinsic,
				Set_Ieee_Rounding_Mode_Intrinsic,
				Set_Ieee_Status_Intrinsic,
				Shape_Intrinsic,
				Shift_Intrinsic,
				Shifta_Intrinsic,
				Shiftl_Intrinsic,
				Shiftr_Intrinsic,
				Short_Intrinsic,
				Sign_Intrinsic,
#ifdef KEY
				Signal_Intrinsic,
#endif
				Sin_Intrinsic,
				Sind_Intrinsic,
				Sinh_Intrinsic,
				Size_Intrinsic,
				Sizeof_Intrinsic,
				Sngl_Intrinsic,
				Snglq_Intrinsic,
				Spacing_Intrinsic,
				Spread_Intrinsic,
				Sqrt_Intrinsic,
#ifdef KEY
                                Stat_Intrinsic,
#endif
				Sub_And_Fetch_Intrinsic,
				Sum_Intrinsic,
				Synchronize_Intrinsic,
				Sync_Images_Intrinsic,
				System_Clock_Intrinsic,
				Tan_Intrinsic,
				Tand_Intrinsic,
				Tanh_Intrinsic,
                                Test_Ieee_Exception_Intrinsic,
                                Test_Ieee_Interrupt_Intrinsic,
				This_Image_Intrinsic,
				Time_Intrinsic,
#ifdef KEY
				Time4_Intrinsic,
				Time8_Intrinsic,
#endif
				Tiny_Intrinsic,
				Transfer_Intrinsic,
				Transpose_Intrinsic,
				Trim_Intrinsic,
				Ubound_Intrinsic,
				Unit_Intrinsic,
				Unpack_Intrinsic,
				Verify_Intrinsic,
                                Write_Memory_Barrier_Intrinsic,
				Xor_Intrinsic,
				Xor_And_Fetch_Intrinsic,
# ifdef KEY
				Zabs_Intrinsic,
				Zcos_Intrinsic,
				Zexp_Intrinsic,
				Zlog_Intrinsic,
				Zsin_Intrinsic,
				Zsqrt_Intrinsic,
# endif
#ifdef KEY /* Bug 1683 */
			 	Pathf90_Intrinsic,
#endif /* KEY Bug 1683 */
#ifdef KEY /* Bug 5089 */
				True_Intrinsic,
				Support_Uflow_Intrinsic,
#endif /* KEY Bug 5089 */
#ifdef KEY /* F2003 */
				Newline_Intrinsic
#endif /* KEY F2003 */
				};


/* ************************************************************************** *\
|*                            YOUR ATTENTION PLEASE			      *|
|*  If you change the linear_type_values list (like adding or subtracting any *|
|*  values), you must also update procedure cif_data_type in fecif.c so that  *|
|*  it agrees with this list.  This means you may also have to negotiate a    *|
|*  change to the define constants that represent data types with the folks   *|
|*  in libcifland.							      *|
\* ************************************************************************** */

enum	linear_type_values     {Err_Res,
				Type_Void = Err_Res,
                                Short_Char_Const,
				Short_Typeless_Const,
                                Typeless_1,
                                Typeless_2,
                                Typeless_4,
                                Typeless_8,
                                Long_Typeless,
				Integer_1,
                                Integer_2,
                                Integer_4,
                                Integer_8,
                                Real_4,
                                Real_8,
                                Real_16,
                                Complex_4,
                                Complex_8,
                                Complex_16,
                                CRI_Ptr_8,
                                Logical_1,
                                Logical_2,
                                Logical_4,
                                Logical_8,
                                Character_1,
                                Character_2,
                                Character_4,
                                CRI_Ch_Ptr_8,
                                Structure_Type,
				CRI_Parcel_Ptr_8,
                                Num_Linear_Types
				};

enum	msg_lvl_values	       {Comment_Lvl,		
				Note_Lvl,
				Caution_Lvl,		
				Warning_Lvl,
				Error_Lvl };

/* MESSAGE LEVELS:  Do not change unless negotiated with PDGCS, C and backends*/
/* Note: many of these levels are not used in cft90			      */
/* Note: update 'msg_severity_names' in messages.h if this enum is changed    */
/* Note: all message levels are output to CIF, only Comment .. Internal,      */
/*	 Log_Error, Log_Warning and Ansi are output to stderr		      */

#ifdef KEY /* Bug 6121 */
/* Move PRINTMSG et al to printmsg.h so C++ can call it */
#include "printmsg.h"
#else /* KEY Bug 6121 */
enum	msg_severities	       {Comment,	Note,		Caution,
				Warning,	Error,		Internal,
				Vector,		Scalar,		Table,
				Ansi,		Log_Warning,	Inline,
				Info,		Tasking,	Limit,
				Log_Error,	Log_Summary,	F77_Ansi,
				Optimization,	Stream,		Unknown_Error };
#endif /* KEY Bug 6121 */

/* NOTE:  Tables in nameres.h are dependent on this enumeration.              */
/* The following enumeration is used to call fnd_semantic_err.  These are     */
/* enumerations of things being added to a symbol table entry.  The divisions */
/* are for getting messages out.  The first group are attributes, and use a   */
/* common message and then fill in the attribute.  The second group is names. */
/* These also have a common message that gets filled in.  The third group is  */
/* others, which needs individual messages, because they just didn't fit the  */
/* attribute style, or the name style.                                        */

/* NOTE:  p_driver.h needs to be changed as well!!!     (obj_str)             */

enum	obj_values	       {Obj_Assum_Type_Ch,	Obj_Expl_Shp_Arr,
				Obj_Assum_Size_Arr,	Obj_Defrd_Shp_Arr,
                                Obj_Assum_Shp_Arr,	Obj_Co_Array,
				Obj_Allocatable,
#ifdef KEY /* Bug 14150 */
				Obj_Bind,		Obj_Value,
#endif /* KEY Bug 14150 */
				Obj_Constant,
				Obj_Intent,		Obj_Optional,
				Obj_Private,		Obj_Public,
				Obj_Target,		Obj_Equiv,
				Obj_Saved,		Obj_Automatic,
				Obj_Pointer,		Obj_Dcl_Extern,
				Obj_Dcl_Intrin,		Obj_Data_Init,
				Obj_Typed,		Obj_Volatile,	

				Obj_Copy_Assumed_Shape, Obj_Auxiliary,
				Obj_Vfunction,		Obj_No_Side_Effects,
				Obj_Symmetric,		Obj_Inline,
				Obj_Ipa,		Obj_Align_Symbol,
				Obj_Fill_Symbol,	Obj_Section_Gp,
				Obj_Section_Non_Gp,	Obj_Ignore_TKR,
				Obj_Optional_Dir,	Obj_Name,		

				Obj_Cri_Ptr,		Obj_Cri_Pointee,
				Obj_Cri_Ch_Pointee,	
				Obj_Ntry_Func_Result,	Obj_Dummy_Arg,
				Obj_Common_Obj,		Obj_Namelist_Obj,
				Obj_Module_Proc,	Obj_Derived_Type,
				Obj_Generic_Interface,	Obj_Namelist_Grp,
				Obj_Stmt_Func,		Obj_Construct,
				Obj_Entry_Func,		Obj_Entry_Subr,
				Obj_Intern_Func,	Obj_Intern_Subr,
				Obj_Module_Func,	Obj_Module_Subr,
				Obj_Sf_Darg,		
				Obj_Name_Done = Obj_Sf_Darg,

				Obj_Sf_Actual_Arg,	Obj_Var_Len_Ch,
				Obj_Var_Len_Arr,	Obj_Sym_Constant_Arr,
				Obj_Interface_Func,	Obj_Interface_Subr,
				Obj_Use_Extern_Func,	Obj_Use_Extern_Subr,
				Obj_Use_In_Expr,	Obj_Use_Derived_Type,
				Obj_Use_Spec_Expr,	Obj_Use_Init_Expr,
				Obj_Done };

/*  Ordering rationale for the operator_values enum list:                     */
/*    - Null (no-op)  (Also positioned first to hopefully catch errors.)      */
/*    - Expression operators in highest to lowest precedence order.           */
/*    - Statement marker operators.                                           */
/*    - Individual statement operators in alphabetical order.                 */
/*    - Miscellaneous operators in alphabetical order.                        */
/*  If you change this list, don't forget to make the corresponding change to */
/*  the operator string array in debug.h.				      */

/* DO NOT CHANGE THE ORDER OF THESE OPERATORS!!!!                             */
/* DO NOT DELETE ANY OF THESE OPERATORS!!!!!!!!!!                             */
/* ADD NEW OPERATORS AT THE END OF THE LIST!!!!!!!!!!!!                       */

enum    operator_values      {  Null_Opr,

                                Defined_Un_Opr,

                                Alloc_Opr,
                                SSD_Alloc_Opr,
				Cvrt_Opr,
				Dealloc_Opr,
                                Power_Opr,
                                Mult_Opr,        
                                Div_Opr,
                                Uplus_Opr,       
                                Uminus_Opr,
                                Plus_Opr,        
                                Minus_Opr,
                                Concat_Opr,
                                Eq_Opr,          
                                Ne_Opr,
                                Lt_Opr,          
                                Le_Opr,
                                Gt_Opr,          
                                Ge_Opr,
                                Not_Opr,
                                Bnot_Opr,
                                And_Opr,
                                Or_Opr,
                                Bor_Opr,
                                Eqv_Opr,         

/* DO NOT CHANGE THE ORDER OF THESE OPERATORS!!!!                             */
/* DO NOT DELETE ANY OF THESE OPERATORS!!!!!!!!!!                             */
/* ADD NEW OPERATORS AT THE END OF THE LIST!!!!!!!!!!!!                       */

                                Beqv_Opr,         
                                Neqv_Opr,       
                                Bneqv_Opr,       
                                Abs_Opr,
                                Cos_Opr,
                                Sin_Opr,
                                Log_E_Opr,
                                Log_10_Opr,
                                Tan_Opr,
                                Tanh_Opr,
                                Sinh_Opr,
                                Acos_Opr,
                                Asin_Opr,
                                Atan_Opr,
                                Cosh_Opr,
                                Atan2_Opr,
                                Aimag_Opr,
                                Sqrt_Opr,
                                Cot_Opr,
                                Exp_Opr,
                                Int_Opr,
                                Band_Opr,
                                Mod_Opr,
                                Anint_Opr,
                                Nint_Opr,

/* DO NOT CHANGE THE ORDER OF THESE OPERATORS!!!!                             */
/* DO NOT DELETE ANY OF THESE OPERATORS!!!!!!!!!!                             */
/* ADD NEW OPERATORS AT THE END OF THE LIST!!!!!!!!!!!!                       */

                                Sign_Opr,
                                Modulo_Opr,
                                Shift_Opr,
                                Shiftl_Opr,
                                Shiftr_Opr,
                                Leadz_Opr,
                                Popcnt_Opr,
                                Poppar_Opr,
                                Aint_Opr,
                                Dim_Opr,
                                Ranget_Opr,
                                Ranset_Opr,
                                Ranf_Opr,
                                Real_Opr,
                                Dble_Opr,
                                Mask_Opr,
                                Conjg_Opr,
                                Dprod_Opr,
                                I24mult_Opr,
                                Length_Opr,
                                Getpos_Opr,
                                Unit_Opr,
                                Cmplx_Opr,
                                Ichar_Opr,
                                Char_Opr,

/* DO NOT CHANGE THE ORDER OF THESE OPERATORS!!!!                             */
/* DO NOT DELETE ANY OF THESE OPERATORS!!!!!!!!!!                             */
/* ADD NEW OPERATORS AT THE END OF THE LIST!!!!!!!!!!!!                       */

                                Lint_Opr,
                                Index_Opr,
                                Lge_Opr,
                                Lgt_Opr,
                                Lle_Opr,
                                Llt_Opr,
                                Fcd_Opr,
                                Numarg_Opr,
                                Rtc_Opr,
                                Cvmgp_Opr,
                                Cvmgm_Opr,
                                Cvmgz_Opr,
                                Cvmgn_Opr,
                                Cvmgt_Opr,
                                Csmg_Opr,
                                Adjustl_Opr,
                                Adjustr_Opr,
                                Ceiling_Opr,
                                Exponent_Opr,
                                Floor_Opr,
                                Fraction_Opr,
                                Spacing_Opr,
                                Logical_Opr,
                                Nearest_Opr,
                                Rrspacing_Opr,

/* DO NOT CHANGE THE ORDER OF THESE OPERATORS!!!!                             */
/* DO NOT DELETE ANY OF THESE OPERATORS!!!!!!!!!!                             */
/* ADD NEW OPERATORS AT THE END OF THE LIST!!!!!!!!!!!!                       */

                                Scale_Opr,
                                Scan_Opr,
                                Set_Exponent_Opr,
                                Verify_Opr,
                                Len_Trim_Opr,
                                Dshiftl_Opr,
                                Dshiftr_Opr,
                                Mmx_Opr,
                                Mldmx_Opr,
                                Mld_Opr,
                                Mul_Opr,
                                Mcbl_Opr,
                                Cshift_Opr,
                                Dot_Product_Opr,
                                Matmul_Opr,
                                Spread_Opr,
                                Transpose_Opr,
                                All_Opr,
                                Any_Opr,
                                Count_Opr,
                                Product_Opr,
                                Sum_Opr,
                                Eoshift_Opr,
                                Maxval_Opr,
                                Minval_Opr,

/* DO NOT CHANGE THE ORDER OF THESE OPERATORS!!!!                             */
/* DO NOT DELETE ANY OF THESE OPERATORS!!!!!!!!!!                             */
/* ADD NEW OPERATORS AT THE END OF THE LIST!!!!!!!!!!!!                       */

                                Maxloc_Opr,
                                Minloc_Opr,
                                Reshape_Opr,
                                SRK_Opr,
                                SIK_Opr,
                                Repeat_Opr,
                                Trim_Opr,
                                Transfer_Opr,

                                Defined_Bin_Opr,

                                Asg_Opr,
                                Call_Opr,
				Alt_Return_Opr,
                                Case_Opr,
				Allocate_Opr,
                                Deallocate_Opr,
                                End_Opr,
                                Entry_Opr,
                                Nullify_Opr,
                                Pause_Opr,
                                Ptr_Asg_Opr,
				Flat_Array_Asg_Opr,
                                Return_Opr,
                                Select_Opr,
				Stmt_Func_Call_Opr,
                                Stop_Opr,

/* DO NOT CHANGE THE ORDER OF THESE OPERATORS!!!!                             */
/* DO NOT DELETE ANY OF THESE OPERATORS!!!!!!!!!!                             */
/* ADD NEW OPERATORS AT THE END OF THE LIST!!!!!!!!!!!!                       */

                                Max_Opr,
                                Min_Opr,

                                Read_Formatted_Opr,
                                Read_Unformatted_Opr,
                                Read_Namelist_Opr,
                                Write_Formatted_Opr,
                                Write_Unformatted_Opr,
                                Write_Namelist_Opr,
				Inquire_Iolength_Opr,

/* Dope vector operators ..                                */
/*    Access operators are unary operators, return a value */
/*         operand goes on the left.                       */
/*    Set operators are like the assignment operator       */
/*         they are binary.                                */

                                Dv_Whole_Copy_Opr,
				Dv_Whole_Def_Opr,
				Dv_Def_Asg_Opr,
                                Dv_Deref_Opr,
 				Dv_Access_Base_Addr,
				Dv_Set_Base_Addr,
				Dv_Access_El_Len,
				Dv_Set_El_Len,
				Dv_Access_Assoc,
				Dv_Set_Assoc,
				Dv_Access_Ptr_Alloc,
				Dv_Set_Ptr_Alloc,
				Dv_Access_P_Or_A,
				Dv_Set_P_Or_A,
				Dv_Access_A_Contig,
				Dv_Set_A_Contig,

/* DO NOT CHANGE THE ORDER OF THESE OPERATORS!!!!                             */
/* DO NOT DELETE ANY OF THESE OPERATORS!!!!!!!!!!                             */
/* ADD NEW OPERATORS AT THE END OF THE LIST!!!!!!!!!!!!                       */

				Dv_Access_N_Dim,
				Dv_Set_N_Dim,
				Dv_Access_Typ_Code,
				Dv_Set_Typ_Code,
				Dv_Access_Orig_Base,
				Dv_Set_Orig_Base,
				Dv_Access_Orig_Size,
				Dv_Set_Orig_Size,

/* Per dimension stuff, dimension is in the IR_DV_DIM */

				Dv_Access_Low_Bound,
				Dv_Set_Low_Bound,
				Dv_Access_Extent,
				Dv_Set_Extent,
				Dv_Access_Stride_Mult,
				Dv_Set_Stride_Mult,

                                Br_Aif_Opr,
                                Br_Asg_Opr,
                                Br_Index_Opr,
                                Br_True_Opr,
                                Br_Uncond_Opr,

                                Case_Range_Opr,
                                Implied_Do_Opr,
                                Kwd_Opr,
				Percent_Val_Opr,
				Loc_Opr,
				Aloc_Opr,

/* DO NOT CHANGE THE ORDER OF THESE OPERATORS!!!!                             */
/* DO NOT DELETE ANY OF THESE OPERATORS!!!!!!!!!!                             */
/* ADD NEW OPERATORS AT THE END OF THE LIST!!!!!!!!!!!!                       */

				Const_Tmp_Loc_Opr,
				Present_Opr,
				Argchck_Present_Opr,
				Argchck_Loc_Opr,
				Len_Opr,
				Clen_Opr,
                                Paren_Opr,
                                Struct_Opr,
                                Struct_Construct_Opr,
				Array_Construct_Opr,
                                Constant_Struct_Construct_Opr,
				Constant_Array_Construct_Opr,
                                Subscript_Opr,
                                Whole_Subscript_Opr,
				Section_Subscript_Opr,
                                Alloc_Obj_Opr,
                                Dealloc_Obj_Opr,
                                Substring_Opr,
                                Whole_Substring_Opr,
                                Triplet_Opr,
                                Label_Opr,
				Loop_Info_Opr,
				Loop_End_Opr,
                                Init_Opr,
				Init_Reloc_Opr,

/* DO NOT CHANGE THE ORDER OF THESE OPERATORS!!!!                             */
/* DO NOT DELETE ANY OF THESE OPERATORS!!!!!!!!!!                             */
/* ADD NEW OPERATORS AT THE END OF THE LIST!!!!!!!!!!!!                       */

				Use_Opr,
				Where_Opr,
			        Real_Div_To_Int_Opr,
				Readsm_Opr,
                                Memory_Barrier_Opr,
                                Remote_Write_Barrier_Opr,
                                Write_Memory_Barrier_Opr,

				Suppress_Opr,

				Align_Cdir_Opr,
				Bl_Cdir_Opr,
				Bounds_Cdir_Opr,
				Cachealign_Cdir_Opr,
				Inline_Cdir_Opr,
				Ivdep_Cdir_Opr,
				Nextscalar_Cdir_Opr,
				Nobl_Cdir_Opr,
				Nobounds_Cdir_Opr,
				Noinline_Cdir_Opr,
				Norecurrence_Cdir_Opr,
				Nosplit_Cdir_Opr,
				Notask_Cdir_Opr,
				Nounroll_Cdir_Opr,
				Novector_Cdir_Opr,
				Novsearch_Cdir_Opr,
				Prefertask_Cdir_Opr,

/* DO NOT CHANGE THE ORDER OF THESE OPERATORS!!!!                             */
/* DO NOT DELETE ANY OF THESE OPERATORS!!!!!!!!!!                             */
/* ADD NEW OPERATORS AT THE END OF THE LIST!!!!!!!!!!!!                       */

				Prefervector_Cdir_Opr,
				Recurrence_Cdir_Opr,
				Shortloop_Cdir_Opr,
				Shortloop128_Cdir_Opr,
				Split_Cdir_Opr,
				Task_Cdir_Opr,
				Unroll_Cdir_Opr,
				Vector_Cdir_Opr,
				Vsearch_Cdir_Opr,

				Case_Cmic_Opr,
				Endcase_Cmic_Opr,
				Cncall_Cmic_Opr,
				Continue_Cmic_Opr,
				Doall_Cmic_Opr,
				Doparallel_Cmic_Opr,
				Enddo_Cmic_Opr,
				Guard_Cmic_Opr,
				Endguard_Cmic_Opr,
				REMOVED_Opr,    /* Available in 5.0 PL */
				Numcpus_Cmic_Opr,
				Parallel_Cmic_Opr,
				Endparallel_Cmic_Opr,
				Permutation_Cmic_Opr,
				Taskcommon_Cmic_Opr,
				Wait_Cmic_Opr,

/* DO NOT CHANGE THE ORDER OF THESE OPERATORS!!!!                             */
/* DO NOT DELETE ANY OF THESE OPERATORS!!!!!!!!!!                             */
/* ADD NEW OPERATORS AT THE END OF THE LIST!!!!!!!!!!!!                       */

				Send_Cmic_Opr,

                                My_Pe_Opr,

                                Ieee_Unordered_Opr,
                                Ieee_Next_After_Opr,
                                Ieee_Remainder_Opr,
                                Ieee_Exponent_Opr,
                                Ieee_Copy_Sign_Opr,
                                Ieee_Int_Opr,
                                Ieee_Real_Opr,
                                Ieee_Finite_Opr,
                                Ieee_Is_Nan_Opr,
                                Ieee_Class_Opr,
                                Ieee_Binary_Scale_Opr,
                                Int_Mult_Upper_Opr,
                                Get_Ieee_Status_Opr,
                                Set_Ieee_Status_Opr,
                                Get_Ieee_Exceptions_Opr,
                                Set_Ieee_Exceptions_Opr,
                                Get_Ieee_Interrupts_Opr,
                                Set_Ieee_Interrupts_Opr,
                                Get_Ieee_Rounding_Mode_Opr,
                                Set_Ieee_Rounding_Mode_Opr,
                                Test_Ieee_Interrupt_Opr,
                                Test_Ieee_Exception_Opr,
                                Set_Ieee_Exception_Opr,

/* DO NOT CHANGE THE ORDER OF THESE OPERATORS!!!!                             */
/* DO NOT DELETE ANY OF THESE OPERATORS!!!!!!!!!!                             */
/* ADD NEW OPERATORS AT THE END OF THE LIST!!!!!!!!!!!!                       */

                                Clear_Ieee_Exception_Opr,
                                Enable_Ieee_Interrupt_Opr,
                                Disable_Ieee_Interrupt_Opr,

                                Cvrt_Unsigned_Opr,
                                SSD_Dealloc_Opr,

                               /* These are used for symbolic expressions. */

				Symbolic_Mult_Opr,
				Symbolic_Div_Opr,
				Symbolic_Uplus_Opr,
				Symbolic_Uminus_Opr,
				Symbolic_Plus_Opr,
				Symbolic_Minus_Opr,
				Symbolic_Max_Opr,

				Rep_Count_Opr,
				Lg_Opr,
				Shifta_Opr,

				Symbolic_Mod_Opr,
				Symbolic_Shiftr_Opr,
				Symbolic_Shiftl_Opr,
				Symmetric_Alloc_Opr,
				Symmetric_Dealloc_Opr,

                                Copy_In_Opr,
                                Copy_Out_Opr,

                                Pack_Opr,
                                Unpack_Opr,

				Local_Pe_Dim_Opr,

/* DO NOT CHANGE THE ORDER OF THESE OPERATORS!!!!                             */
/* DO NOT DELETE ANY OF THESE OPERATORS!!!!!!!!!!                             */
/* ADD NEW OPERATORS AT THE END OF THE LIST!!!!!!!!!!!!                       */

				Start_Io_Opr,
				End_Io_Opr,

                                Dot_Product_Logical_Opr,

				Symbolic_Min_Opr,
				Nopattern_Cdir_Opr,
				Pattern_Cdir_Opr,
				Mark_Cdir_Opr,
				Nomark_Cdir_Opr,

				Backspace_Opr,
				Buffer_In_Opr,
				Buffer_Out_Opr,
				Close_Opr,
				Endfile_Opr,
				Inquire_Opr,
				Open_Opr,
				Rewind_Opr,
                                Mvbits_Opr,
                                Ishftc_Opr,
                                Ibits_Opr,

				False_Parm_Opr,

				Aggressiveinnerloopfission_Opr,
				Blockable_Dir_Opr,
				Blockingsize_Dir_Opr,
				Fission_Star_Opr,
				Fissionable_Star_Opr,

/* DO NOT CHANGE THE ORDER OF THESE OPERATORS!!!!                             */
/* DO NOT DELETE ANY OF THESE OPERATORS!!!!!!!!!!                             */
/* ADD NEW OPERATORS AT THE END OF THE LIST!!!!!!!!!!!!                       */

				Fuse_Star_Opr,
				Fusable_Star_Opr,
				Interchange_Dir_Opr,
				Nointerchange_Dir_Opr,
				Nofission_Star_Opr,
				Nofusion_Star_Opr,
				Noblocking_Dir_Opr,
				Opaque_Star_Opr,
				Redistribute_Dollar_Opr,
				Doacross_Dollar_Opr,
				Purpleconditional_Star_Opr,
				Purpleunconditional_Star_Opr,
				Pdo_Par_Opr,
				Parallel_Do_Par_Opr,
				Parallel_Par_Opr,
				Psection_Par_Opr,
				Singleprocess_Par_Opr,
				Section_Par_Opr,
				End_Pdo_Par_Opr,
				End_Parallel_Par_Opr,
				Barrier_Par_Opr,
				Critical_Section_Par_Opr,
				End_Critical_Section_Par_Opr,
				End_Psection_Par_Opr,
				End_Singleprocess_Par_Opr,
				
/* DO NOT CHANGE THE ORDER OF THESE OPERATORS!!!!                             */
/* DO NOT DELETE ANY OF THESE OPERATORS!!!!!!!!!!                             */
/* ADD NEW OPERATORS AT THE END OF THE LIST!!!!!!!!!!!!                       */

				Unroll_Star_Opr,
				Assert_Star_Opr,
				Regionbegin_Star_Opr,
				Regionend_Star_Opr,
				Section_Gp_Star_Opr,
				Section_Nongp_Star_Opr,

				Prefetch_Star_Opr,
				Prefetch_Manual_Star_Opr,
				Prefetch_Ref_Disable_Star_Opr,
				Prefetch_Ref_Star_Opr,

				Align_Symbol_Star_Opr,
				Fill_Symbol_Star_Opr,

				Inline_Here_Star_Opr,
				Noinline_Here_Star_Opr,
				End_Inline_Here_Star_Opr,

				Dynamic_Dollar_Opr,
				Page_Place_Dollar_Opr,
				Copyin_Dollar_Opr,

				User_Code_Start_Opr,

				Fetch_And_Add_Opr,
				Fetch_And_Sub_Opr,
				Fetch_And_Or_Opr,
				Fetch_And_And_Opr,
				Fetch_And_Xor_Opr,
				Fetch_And_Nand_Opr,

/* DO NOT CHANGE THE ORDER OF THESE OPERATORS!!!!                             */
/* DO NOT DELETE ANY OF THESE OPERATORS!!!!!!!!!!                             */
/* ADD NEW OPERATORS AT THE END OF THE LIST!!!!!!!!!!!!                       */

				Add_And_Fetch_Opr,
				Sub_And_Fetch_Opr,
				Or_And_Fetch_Opr,
				And_And_Fetch_Opr,
				Xor_And_Fetch_Opr,
				Nand_And_Fetch_Opr,

				Synchronize_Opr,
				Lock_Release_Opr,
				Lock_Test_And_Set_Opr,
				Compare_And_Swap_Opr,

				Integer_Cdir_Opr,

				Malloc_Opr,
				Free_Opr,

				Concurrent_Cdir_Opr,

                                Inline_Routine_Star_Opr,
                                Noinline_Routine_Star_Opr,
                                Inline_Global_Star_Opr,
                                Noinline_Global_Star_Opr,

				Atomic_Open_Mp_Opr,
				Barrier_Open_Mp_Opr,
				Critical_Open_Mp_Opr,
				Do_Open_Mp_Opr,
				Endcritical_Open_Mp_Opr,
				Enddo_Open_Mp_Opr,
				Endparallel_Open_Mp_Opr,

/* DO NOT CHANGE THE ORDER OF THESE OPERATORS!!!!                             */
/* DO NOT DELETE ANY OF THESE OPERATORS!!!!!!!!!!                             */
/* ADD NEW OPERATORS AT THE END OF THE LIST!!!!!!!!!!!!                       */

				Endparalleldo_Open_Mp_Opr,
				Endparallelsections_Open_Mp_Opr,
				Endparallelworkshare_Open_Mp_Opr, /* by jhs, 02/7/18 */
				Endmaster_Open_Mp_Opr,
				Endordered_Open_Mp_Opr,
				Endsections_Open_Mp_Opr,
				Endsingle_Open_Mp_Opr,
				Endworkshare_Open_Mp_Opr, /* by jhs, 02/7/18 */
				Flush_Open_Mp_Opr,
				Master_Open_Mp_Opr,
				Ordered_Open_Mp_Opr,
				Parallel_Open_Mp_Opr,
				Paralleldo_Open_Mp_Opr,
				Parallelsections_Open_Mp_Opr,
				Parallelworkshare_Open_Mp_Opr, /* by jhs, 02/7/18 */
				Section_Open_Mp_Opr,
				Sections_Open_Mp_Opr,
				Single_Open_Mp_Opr,
				Workshare_Open_Mp_Opr, /* by jhs, 02/7/18 */

                                Concurrentize_Star_Opr,
                                Noconcurrentize_Star_Opr,

                                Omp_Set_Lock_Opr,
                                Omp_Unset_Lock_Opr,
                                Omp_Test_Lock_Opr,

                                Omp_Get_Num_Threads_Opr,
                                Omp_Get_Max_Threads_Opr,
                                Omp_Get_Thread_Num_Opr,
                                Omp_Get_Num_Procs_Opr,
                                Omp_In_Parallel_Opr,

/* DO NOT CHANGE THE ORDER OF THESE OPERATORS!!!!                             */
/* DO NOT DELETE ANY OF THESE OPERATORS!!!!!!!!!!                             */
/* ADD NEW OPERATORS AT THE END OF THE LIST!!!!!!!!!!!!                       */

                                Omp_Get_Dynamic_Opr,
                                Omp_Get_Nested_Opr,

				Cache_Bypass_Cdir_Opr,

                                Forall_Opr,
				If_Opr,
				Else_Opr,
				Endif_Opr,

				Flush_Star_Opr,
				Stmt_Expansion_Opr,

				Cosd_Opr,
				Sind_Opr,
				Tand_Opr,
				Acosd_Opr,
				Asind_Opr,
				Atand_Opr,
				Atan2d_Opr,

				Stream_Dir_Opr,
				UNUSED1_Opr,
				UNUSED2_Opr,
				UNUSED3_Opr,
				UNUSED4_Opr,
				UNUSED5_Opr,
				Nostream_Dir_Opr,

				Null_Intrinsic_Opr,
				Io_Item_Type_Code_Opr,

/* DO NOT CHANGE THE ORDER OF THESE OPERATORS!!!!                             */
/* DO NOT DELETE ANY OF THESE OPERATORS!!!!!!!!!!                             */
/* ADD NEW OPERATORS AT THE END OF THE LIST!!!!!!!!!!!!                       */

				Where_Cnstrct_Opr,
				Else_Where_Mask_Opr,
				Else_Where_Opr,

				Preferstream_Dir_Opr,

				Copyin_Bound_Opr,

				Preferstream_Nocinv_Dir_Opr,
#ifdef KEY /* Bug 1324 */
				Erf_Opr,
				Erfc_Opr,
#endif /* KEY Bug 1324 */
#ifdef KEY /* Bug 2660 */
				Options_Dir_Opr,
#endif /* KEY Bug 2660 */
#ifdef KEY /* Bug 10410 */
				Cselect_Opr,
#endif /* KEY Bug 10410 */

                                /* PLACE NEW OPERATORS ABOVE THIS LINE. */
                                /* DO NOT PUT ANY OPRS AFTER THIS ONE */
				The_Last_Opr
                              };

enum	scalar_lvl_values      {Scalar_Lvl_0,
				Scalar_Lvl_1,
				Scalar_Lvl_2,
				Scalar_Lvl_3,
				Scalar_Lvl_Err };

enum    sh_position_values     {Before,			After};


enum	src_form_values	       {Fixed_Form,		Free_Form };


/******************************************************************************/
/* DATA STRUCTURES DEPENDENT ON THE FOLLOWING ENUMERATION:                    */
/*      token_to_stmt_type              DEFINED IN      p_driver.h            */
/*      stmt_parsers                    DEFINED IN      p_driver.h            */
/*      stmt_semantics			DEFINED IN	s_driver.h	      */
/*	stmt_in_blk			DEFINED IN      p_driver.h	      */
/*	stmt_top_cat			DEFINED IN      p_driver.h	      */
/*	stmt_type_str			DEFINED IN      main.h		      */
/*	mapped_stmt_type		DEFINED IN      fecif.h		      */
/*									      */
/* Notice:  If any new statement types are added, they must be added to the   */
/*          end of the enum.  You must also update table mapped_stmt_type     */
/*          in fecif.h and cif_stmt_values in this file (and contact the CIF  */
/*          folks to coordinate the new statement type numbers for the CIF    */
/*          Statement Type record.					      */
/******************************************************************************/

enum stmt_type_values           {Null_Stmt,

				 Allocatable_Stmt,
                                 Automatic_Stmt,
                                 Common_Stmt,
                                 Contains_Stmt,
                                 Cpnt_Decl_Stmt,
                                 Data_Stmt,
                                 Derived_Type_Stmt,     /* e.g. TYPE <name>   */
                                 Dimension_Stmt,
                                 Directive_Stmt,
                                 Equivalence_Stmt,
                                 External_Stmt,
                                 Format_Stmt,
                                 Implicit_Stmt,
                                 Implicit_None_Stmt,
                                 Intent_Stmt,
                                 Interface_Stmt,
                                 Intrinsic_Stmt,
                                 Module_Proc_Stmt,
                                 Namelist_Stmt,
                                 Optional_Stmt,
                                 Parameter_Stmt,
                                 Pointer_Stmt,
                                 Private_Stmt,
                                 Public_Stmt,
                                 Save_Stmt,
                                 Sequence_Stmt,
                                 Stmt_Func_Stmt,
                                 Target_Stmt,
                                 Task_Common_Stmt,
                                 Type_Decl_Stmt,        /* C L I R D CX TYPE( */
                                 Use_Stmt,

                                 Blockdata_Stmt,
                                 Elemental_Stmt,
                                 Function_Stmt,
                                 Module_Stmt,
                                 Program_Stmt,
                                 Pure_Stmt,
                                 Recursive_Stmt,
                                 Subroutine_Stmt,

				 End_Blockdata_Stmt,
				 End_Do_Stmt,
				 End_Function_Stmt,
				 End_If_Stmt,
				 End_Interface_Stmt,
				 End_Module_Stmt,
				 End_Program_Stmt,
				 End_Select_Stmt,
                                 End_Stmt,
				 End_Subroutine_Stmt,
				 End_Type_Stmt,
				 End_Where_Stmt,

                                 Allocate_Stmt,
                 		 Arith_If_Stmt,
                                 Assign_Stmt,
                                 Assignment_Stmt,
                                 Backspace_Stmt,
                                 Buffer_Stmt,
                                 Call_Stmt,
                                 Case_Stmt,
                                 Close_Stmt,
                                 Continue_Stmt,
                                 Cycle_Stmt,
                                 Deallocate_Stmt,
                                 Decode_Stmt,
                                 Do_Iterative_Stmt,
                                 Do_While_Stmt,
 				 Do_Infinite_Stmt,
                                 Else_Stmt,
                                 Else_If_Stmt,
                                 Else_Where_Stmt,
                                 Encode_Stmt,
                                 Endfile_Stmt,
                                 Entry_Stmt,
                                 Exit_Stmt,
                                 Goto_Stmt,
                                 If_Cstrct_Stmt,
                                 If_Stmt,
                                 Inquire_Stmt,
                                 Nullify_Stmt,
                                 Open_Stmt,
				 Outmoded_If_Stmt,
                                 Pause_Stmt,
                                 Print_Stmt,
                                 Read_Stmt,
                                 Return_Stmt,
                                 Rewind_Stmt,
                                 Select_Stmt,
                                 Stop_Stmt,
				 Then_Stmt,
                                 Where_Cstrct_Stmt,
                                 Where_Stmt,
                                 Write_Stmt,
				 Type_Init_Stmt,

				 Label_Def,
				 Construct_Def,

				 Automatic_Base_Calc_Stmt,
				 Automatic_Base_Size_Stmt,

				 End_Parallel_Stmt,
				 End_Do_Parallel_Stmt,
				 End_Parallel_Case_Stmt,
				 Parallel_Case_Stmt,
				 End_Guard_Stmt,

				 Statement_Num_Stmt,

				 SGI_Section_Stmt,
				 SGI_End_Psection_Stmt,
				 SGI_End_Pdo_Stmt,
				 SGI_End_Parallel_Stmt,
				 SGI_End_Critical_Section_Stmt,
				 SGI_End_Single_Process_Stmt,
				 SGI_Region_End_Stmt,

				 Open_MP_Section_Stmt,
				 Open_MP_End_Parallel_Stmt,
				 Open_MP_End_Do_Stmt,
				 Open_MP_End_Parallel_Sections_Stmt,
				 Open_MP_End_Sections_Stmt,
				 Open_MP_End_Section_Stmt,
				 Open_MP_End_Single_Stmt,
				 Open_MP_End_Parallel_Do_Stmt,
				 Open_MP_End_Master_Stmt,
				 Open_MP_End_Critical_Stmt,
				 Open_MP_End_Ordered_Stmt,

                                 Forall_Cstrct_Stmt,
                                 Forall_Stmt,
				 End_Forall_Stmt,

				 Else_Where_Mask_Stmt,

                                 Volatile_Stmt,

				 Open_MP_End_Parallel_Workshare_Stmt,
				 Open_MP_End_Workshare_Stmt,
#ifdef KEY /* Bug 11741 */
				 Import_Stmt,
#endif /* KEY Bug 11741 */
#ifdef KEY /* Bug 10572 */
				 Enum_Stmt,
				 End_Enum_Stmt,
				 Enumerator_Stmt,
#endif /* KEY Bug 10572 */
#ifdef KEY /* Bug 14150 */
				 Bind_Stmt,
				 Value_Stmt
#endif /* KEY Bug 10572 */

                                 /* When you add a stmt, make sure you change */
                                 /* stmt_type_str in main.h.                  */
                                 /* Also change mapped_stmt_type in fecif.h   */

				 /* To determine that a stmt is executable,   */
                                 /* see code that sets ATL_EXECUTABLE in      */
                                 /* p_driver.c .			      */

				};


enum	split_lvl_values       { Split_Lvl_0,
				 Split_Lvl_1,
				 Split_Lvl_2,
				 Split_Lvl_Err };

enum	stream_lvl_values      { Stream_Lvl_0,
				 Stream_Lvl_1,
				 Stream_Lvl_2,
				 Stream_Lvl_3,
				 Stream_Lvl_Err };

enum	task_lvl_values	       { Task_Lvl_0,
				 Task_Lvl_1,
				 Task_Lvl_2,
				 Task_Lvl_3,
				 Task_Lvl_Err };

enum	unroll_lvl_values      { Unroll_Lvl_0,
				 Unroll_Lvl_1,
				 Unroll_Lvl_2,
				 Unroll_Lvl_Err };

enum	vector_lvl_values      { Vector_Lvl_0,
				 Vector_Lvl_1,
				 Vector_Lvl_2,
				 Vector_Lvl_3,
				 Vector_Lvl_Err };

enum trace_values	       { Func_Entry,		
                                 Func_Exit,
				 Syntax_Pass,		
                                 Semantics_Pass,
			 	 PU_Start, 		
                                 Stmt_Start,
				 Mem_Alloc,		
                                 Mem_Realloc,
				 Mem_Free,		
                                 Mem_Compress };

enum    mp_directive_values     {
                                Doacross,
                                Pdo,
                                Parallel_Do,
                                Parallel,
                                Psection,
                                Singleprocess,
                                Num_Mp_Values   /* must be last */
                                };

typedef enum mp_directive_values        mp_directive_type;

enum    mp_clause_values        {
                                If_Clause,
                                Local_Clause,
                                Share_Clause,
                                Lastlocal_Clause,
                                Reduction_Clause,
                                Mp_Schedtype_Clause,
                                Chunk_Clause,
                                Blocked_Clause,
                                Affinity_Clause,
                                Mode_Clause,
                                Ordered_Clause,
                                Onto_Clause,
                                Nest_Clause,
                                Lastthread_Clause,
                                Last_Clause     /* must be last */
                                };
typedef enum mp_clause_values mp_clause_type;

extern char    *(mp_dir_str[Num_Mp_Values]);

extern boolean clause_allowed[Num_Mp_Values][Last_Clause];

enum    open_mp_directive_values     {
                                Parallel_Omp,
                                Do_Omp,
                                Sections_Omp,
                                Single_Omp,
                                Workshare_Omp, /* by jhs, 02/7/18 */
                                Parallel_Do_Omp,
                                Parallel_Sections_Omp,
                                Parallel_Workshare_Omp, /* by jhs, 02/7/18 */
                                Num_Omp_Values   /* must be last */
                                };

typedef enum open_mp_directive_values        open_mp_directive_type;

enum    open_mp_clause_values        {
                                If_Omp_Clause,
                                Num_Threads_Omp_Clause, /* by jhs, 02/7/18 */
                                Private_Omp_Clause,
                                Shared_Omp_Clause,
                                Firstprivate_Omp_Clause,
                                Default_Omp_Clause,
                                Copyin_Omp_Clause,
                                Reduction_Omp_Clause,
                                Lastprivate_Omp_Clause,
                                Ordered_Omp_Clause,
                                Schedule_Omp_Clause,
				Affinity_Omp_Clause,
				Nest_Omp_Clause,
				Onto_Omp_Clause,
                                Copyprivate_Omp_Clause,
                                Last_Omp_Clause     /* must be last */
                                };
typedef enum open_mp_clause_values open_mp_clause_type;

extern char    *(open_mp_dir_str[Num_Omp_Values]);

extern boolean open_mp_clause_allowed[Num_Omp_Values][Last_Omp_Clause];


typedef enum	addr_mode_values		addr_mode_type;
typedef	enum	basic_type_values		basic_type_type;
typedef	enum	convert_to_string_values 	convert_to_string_type;
typedef enum	debug_lvl_values		debug_lvl_type;
typedef	enum	expr_mode_values		expr_mode_type;
typedef	enum	fld_values			fld_type;
typedef	enum	fortran_type_values		fortran_type_type;
typedef enum	forward_ref_values		forward_ref_type;
typedef	enum	glb_tbl_idx_values		glb_tbl_idx_type;
typedef	enum	intrinsic_values		intrinsic_type;
typedef	enum	linear_type_values		linear_type_type;
typedef enum	msg_lvl_values			msg_lvl_type;
#ifdef KEY /* Bug 6121 */
/* Move PRINTMSG et al to printmsg.h so C++ can call it */
#else /* KEY Bug 6121 */
typedef enum	msg_severities			msg_severities_type;
#endif /* KEY Bug 6121 */
typedef enum	obj_values			obj_type;
typedef	enum	operator_values			operator_type;
typedef	enum	scalar_lvl_values		scalar_lvl_type;
typedef enum	sh_position_values		sh_position_type;
typedef enum	src_form_values			src_form_type;
typedef enum    stmt_type_values        	stmt_type_type;
typedef	enum	stream_lvl_values		stream_lvl_type;
typedef	enum	task_lvl_values			task_lvl_type;
typedef	enum	vector_lvl_values		vector_lvl_type;
typedef enum    trace_values			trace_type;
typedef struct  ac_cmd_line_flags_entry 	ac_cmd_line_flags_type;
typedef struct	cdir_switch_entry		cdir_switch_type;
typedef struct	cmd_line_flags_entry		cmd_line_flags_type;
typedef struct	dump_flags_entry		dump_flags_type;
typedef	struct	expr_semantics_args		expr_arg_type;
typedef struct	on_off_flags_entry		on_off_flags_type;
typedef	struct	opnd_entry			opnd_type;
typedef struct	opt_flags_entry			opt_flags_type;
typedef union	target_machine_entry		target_machine_type;
typedef union	id_str_entry			id_str_type;

union   id_str_entry		{char		string[MAX_ID_LEN+1];
                                 long           words[NUM_ID_WDS];
				};



struct	cmd_line_flags_entry {
	boolean		align8			: 1;		/* -align8    */
	boolean		align16			: 1;		/* -align16   */
	boolean		align32			: 1;		/* -align32   */
	boolean		align64			: 1;		/* -align64   */
	boolean		static_threadprivate	: 1;		/* -astatic_..*/
	boolean		dalign			: 1;		/* -a  dalign */
	boolean		pad			: 1;		/* -a  pad    */
	Uint		pad_amount		: 16;		/* -a  padn   */
	boolean		taskcommon		: 1;		/*-ataskcommon*/
	boolean		binary_output		: 1;		/* -b  TRUE   */
	boolean		reserved_c		: 1;		/*	      */
	boolean		off_options		: 1;		/* -d  flags  */
	boolean		on_options		: 1;		/* -e  flags  */
	src_form_type	src_form		: 2;		/* -f  fixed  */
	boolean		reserved_g		: 1;		/*            */
	boolean		reserved_h		: 1;		/*	      */
	boolean		integer_32		: 1;		/* -i  32     */
	boolean		reserved_j		: 1;		/*	      */
	boolean		solaris_profile		: 1;		/* -k 	      */
	boolean		reserved_l		: 1;		/*            */
	msg_lvl_type	msg_lvl_suppressed	: 4;		/* -m  warning*/
	boolean		reserved_n		: 1;		/*	      */
	boolean		reserved_o		: 1;		/*	      */
	boolean		module_paths		: 1;		/* -p path    */
	boolean		expression_eval_stmt	: 1;		/* -qs	      */
	boolean		expression_eval_expr	: 1;		/* -qe	      */
	boolean		reserved_q		: 1;		/*	      */
	boolean		reserved_r		: 1;		/*	      */
	boolean		s_float64		: 1;		/* -s float64 */
	boolean		s_default32		: 1;		/*-s default32*/
	boolean		s_default64		: 1;		/*-s default64*/
	boolean		s_cf77types		: 1;		/*-s cf77types*/
	boolean		s_integer8		: 1;		/* -sinteger8 */
	boolean		s_logical8		: 1;		/* -slogical8 */
	boolean		s_real8			: 1;		/* -sreal8    */
	boolean		s_complex8		: 1;		/* -scomplex8 */
	boolean		s_doublecomplex16	: 1;		/* -sdoublec  */
	boolean		s_doubleprecision16	: 1;		/* -sdoublepr */
	boolean		s_pointer8		: 1;		/* -spointer8 */
	Uint		truncate_bits		: 6;		/* -t 0       */
	boolean		dump_options		: 1;		/* -u flags   */
	boolean		pdgcs_debug_opts	: 1;		/* -v opts    */
	boolean		reserved_w		: 1;		/*	      */
	boolean		disregard_all_directives: 1;		/* -x  all    */
	boolean		disregard_all_dirs	: 1;		/* -x  dir    */
	boolean		disregard_all_mics	: 1;		/* -x  mic    */
	boolean		disregard_all_mips	: 1;		/* -x  mips   */
	boolean		disregard_all_mpp_cdirs	: 1;		/* -x  mpp    */
	boolean         disregard_all_omps      : 1;            /* -x  omp    */
	boolean         disregard_conditional_omp : 1;          /* No !$      */
	boolean		reserved_y		: 1;		/*	      */
	boolean		reserved_z		: 1;		/*	      */
	int		implicit_use_idx;
	boolean		reserved_B		: 1;		/*	      */
	boolean		cif_flags		: 1;		/* -C cif     */
	boolean		reserved_D		: 1;		/*	      */
	boolean		reserved_E		: 1;		/*	      */
	boolean		pp_macro_expansion	: 1;		/*            */
	debug_lvl_type	debug_lvl		: 4;		/* -G level   */
	boolean		dwarf_debug		: 1;		/* -Gd	      */
	boolean		reserved_H		: 1;		/*	      */
	boolean		reserved_I		: 1;		/* -I include */
	boolean		mod_out_path		: 1;		/* -J path    */
	boolean		reserved_K		: 1;		/*	      */
	boolean		reserved_L		: 1;		/*	      */
	Uint		num_msgs_suppressed	: 16;		/* # -M msg   */
	boolean		line_size_80		: 1;		/* -N  72     */
	boolean		line_size_132		: 1;		/* -N  72     */
	boolean		opt_options		: 1;		/* -O  flags  */
	boolean		small_pic_model		: 1;		/* -Ps        */
	boolean		large_pic_model		: 1;		/* -Pl        */
	boolean		reserved_Q		: 1;		/*	      */
	boolean		runtime_argument	: 1;		/* -R a	      */
	boolean		runtime_arg_call	: 1;		/* -R C	      */
	boolean		runtime_arg_entry	: 1;		/* -R E	      */
	boolean		runtime_arg_count_only	: 1;		/* -R n	      */
	boolean		runtime_bounds		: 1;		/* -R b	      */
	boolean		runtime_conformance	: 1;		/* -R c	      */
	boolean		runtime_intrinsics	: 1;		/* -R i	      */
	boolean		runtime_substring	: 1;		/* -R s	      */
	boolean		runtime_ptr_chk  	: 1;		/* -R p	      */
	boolean		assembly_output		: 1;		/* -S FALSE   */
	boolean		reserved_T		: 1;		/*            */
	boolean		reserved_U		: 1;		/*	      */
	boolean		verify_option		: 1;		/* -V  FALSE  */
	boolean		reserved_W		: 1;		/* -W  FALSE  */
	Uint		MPP_num_pes		:32;		/* -X  # pes  */
	boolean		malleable		: 1;		/* -Xm	      */
	boolean		ccg_dump_options	: 1;		/* -Y  flags  */
	boolean		co_array_fortran	: 1;		/* -Z	      */
	boolean		do_UDB_checks		: 1;		/* -G0&!scala0*/
	};

struct	ac_cmd_line_flags_entry {
	Uint		temp_fchar_len		: 3;		/*-YCLEN=1...6*/
	Uint		ghand_create_count	: 3;		/*-YMXGH=1...4*/
	};
 

struct	dump_flags_entry {

        /* The following options are allowed in a nondebug compiler.          */

        boolean		abort_on_ansi		: 1;
	boolean		no_dimension_padding	: 1;
	boolean		no_module_output	: 1;
	boolean		preinline		: 1;
	boolean		f_minus_minus		: 1;
	boolean		fmm1			: 1;
	boolean		fmm2			: 1;
	boolean		show_cmd_line		: 1;
        boolean		mod_version		: 1;
	boolean		mp			: 1;
	boolean		open_mp			: 1;
	boolean		dsm			: 1;
	boolean		cray_compatible		: 1;
	boolean		pack_half_word		: 1;
	int		pvp_test;

        /* The rest of the options are only allowed in a debug compiler.      */

	boolean		bd_tbl			: 1;
	boolean		blk_stk			: 1;
	boolean		cmd_line_tbls		: 1;
	boolean		cn_tbl			: 1;
	boolean		defines			: 1;
	boolean		fort_out		: 1;
	boolean		fortran_out		: 1;
        boolean         fp_tbl                  : 1;
	boolean		ftrace_info		: 1;
	boolean		gl_tbl			: 1;
	boolean		help_dbg		: 1;
	boolean		intrin_tbl		: 1;
	boolean		ir1_tbl			: 1;
	boolean		ir2_tbl			: 1;
	boolean		ir3_tbl			: 1;
	boolean		ir4_tbl			: 1;
        boolean         mem_report              : 1;
	boolean		msg_checking		: 1;
	boolean		mtrace_info		: 1;
	boolean		name_tbls		: 1;
	boolean		pdgcs			: 1;
	boolean		pdt_dump		: 1;
	boolean		sb_tbl			: 1;
	boolean		scp_tbl			: 1;
	boolean		src_dmp			: 1;
	boolean		std_err			: 1;
	boolean		stmt_dmp		: 1;
	boolean		sytb			: 1;
	boolean		typ_tbl			: 1;
	boolean		constant_bits		: 1;
#ifdef KEY /* Bug 8117 */
	boolean		arg_passing		: 1;
#endif /* KEY Bug 8117 */
	};


struct	on_off_flags_entry {
	boolean		abort_if_any_errors	: 1;		/* -ea	      */
	boolean		reserved_b		: 1;		/*	      */
	boolean		pad_char_literals	: 1;		/* -ec	      */
	boolean		reserved_d		: 1;		/*            */
	boolean		ieee			: 1;		/* -ee	      */
	boolean		flowtrace_option	: 1;		/* -ef        */
	boolean		assembly_listing_file	: 1;		/* -eg        */
	boolean		integer_1_and_2		: 1;		/* -eh	      */
	boolean		indef_init		: 1;		/* -ei        */
	boolean		exec_doloops_once	: 1;		/* -ej        */
	boolean		reserved_k		: 1;		/*	      */
	boolean		reserved_l		: 1;		/*    	      */
	boolean		module_to_mod		: 1;		/* -em	      */
	boolean		issue_ansi_messages	: 1;		/* -en        */
	boolean		reserved_o		: 1;		/*            */
	boolean		enable_double_precision	: 1;		/* -ep        */
	boolean		abort_on_100_errors	: 1;		/* -eq        */
	boolean		round_mult_operations	: 1;		/* -er        */
	boolean		reserved_s		: 1;		/*	      */
	boolean		alloc_autos_on_stack	: 1;		/* -et        */
	boolean		eu			: 1;		/* -eu        */
	boolean		round_integer_divide	: 1;		/* -eu        */
	boolean		reciprical_divide	: 1;		/* -eu        */
	boolean		save_all_vars		: 1;		/* -ev        */
	boolean		set_ev_option		: 1;		/* -ev        */
	boolean		reserved_w		: 1;		/*	      */
	boolean		reserved_x		: 1;		/*	      */
	boolean		reserved_y		: 1;		/*	      */
	boolean		recognize_minus_zero	: 1;		/* -ez        */
	boolean		MPP_apprentice		: 1;		/* -eA        */
	boolean		binary_output		: 1;		/* -eB        */
	boolean		shared_to_private_coer	: 1;		/* -eC	      */
	boolean		all_debug		: 1;		/* -eD 	      */
	boolean		reserved_E		: 1;		/*	      */
	boolean		reserved_F		: 1;		/*	      */
	boolean		reserved_G		: 1;		/*	      */
	boolean		reserved_H		: 1;		/*	      */
	boolean		implicit_none		: 1;		/* -eI	      */
	boolean		reserved_J		: 1;		/*	      */
	boolean		reserved_K		: 1;		/*	      */
	boolean		top_test_shortloops	: 1;		/* -eL	      */
	boolean		reserved_M		: 1;		/*	      */
	boolean		second_underscore	: 1;		/* -zN	      */
	boolean		underscoring    	: 1;		/* -zU	      */
	boolean		output_pound_lines	: 1;		/* -dP	      */
	boolean		preprocess_only		: 1;		/*            */
	boolean		preprocess     		: 1;		/*            */
	boolean		allow_leading_uscore    : 1;		/* -eQ        */
	boolean		recursive		: 1;		/* -eR	      */
	boolean		assembly_output		: 1;		/* -eS        */
	boolean		reserved_T		: 1;		/*	      */
	boolean		upper_case_names	: 1;		/* -eU        */
	boolean		reserved_V		: 1;		/*	      */
	boolean		reserved_W		: 1;		/*	      */
	boolean		atexpert		: 1;		/* -eX        */
	boolean		reserved_Y		: 1;		/*	      */
	boolean		save_dot_i		: 1;		/* -ek	      */
	boolean		zero_init		: 1;		/* -e0	      */
	boolean		d_lines			: 1;
#ifdef KEY /* Bug 5089 */
	/* For use only when building the library: mark a generated module as
	 * "intrinsic" in the F2003 sense, so that its linker name doesn't
	 * collide with that of any user-created module. */
	boolean		intrinsic_module_gen	: 1;		/* -intrinsic_module_gen */
#endif/* KEY Bug 5089 */
#ifdef KEY /* Bug 12482 */
	boolean		fortran2003		: 1;		/* -ffortran2003 */
#endif /* KEY Bug 12482 */
	};

/*************\
|* OPND TYPE *|
\*************/

/* IF THIS CHANGES, please change INIT_OPND_TYPE in globals.m */

struct	opnd_entry		{Uint			line_num        : 24;
                                 Uint 			col_num         :  8;

                                 Uint                   flag_1          :  1;
                                 Uint                   flag_2          :  1;
                                 Uint                   unused          :  2;
                                 fld_type               fld             :  4;
                                 Uint                   idx             : 24;
                                };


/***********************************************************\
|* enum used to describe array constructor size complexity *|
\***********************************************************/

enum    size_expr_level {
			Unknown_Expr_Size,
                        Simple_Expr_Size,
                        Interp_Loop_Size,
                        Guess_Size
                        };

typedef enum size_expr_level size_level_type;

/**************************************************\
|* expression descriptor type for expr semantics. *|
|* must be here because it needs opnd_type        *|
\**************************************************/

struct  expr_semantics_args    {basic_type_type		type		: 8;
				linear_type_type	linear_type	: 8;
				Uint			type_idx	: 16;

				boolean			kind0seen	: 1;
				boolean			kind0E0seen	: 1;
				boolean			kind0D0seen	: 1;
				boolean			kindnotconst	: 1;
				boolean			percent_val_arg : 1;
                                boolean			constant	: 1;
                                boolean			foldable	: 1;
                                boolean			will_fold_later	: 1;
                                boolean			pointer		: 1;
                                boolean			target		: 1;
                                boolean			vector_subscript: 1;
                                boolean			reference	: 1;
				boolean			constructor	: 1;
				boolean			component	: 1;
                                boolean			section		: 1;
                                boolean			label		: 1;
				boolean			array_elt	: 1;
				boolean			assumed_shape	: 1;
				boolean			assumed_size	: 1;
				boolean			allocatable	: 1;
				boolean			dope_vector	: 1;
                                boolean                 tmp_reference	: 1;
				boolean			has_constructor	: 1;
				boolean			optional_darg	: 1;
				boolean			pe_dim_ref	: 1;
				boolean			dist_reshape_ref: 1;
#ifdef KEY /* Bug 934 */
				/* Set to indicate that we are evaluating
				 * the RHS of an assignment of an entire
				 * derived type, so that we can enforce
				 * the constraint on such assigments
				 * within pure procedures */
				boolean                 derived_assign  : 1;
				Uint			UNUSED2		: 5;
#else /* KEY Bug 934 */
				Uint			UNUSED2		: 6;
#endif /* KEY Bug 934 */

                                Uint			rank   		: 32;

                                Uint			UNUSED3		: 2;
				Uint		   constructor_size_level : 2;
				boolean			has_symbolic	: 1;
                                boolean			tree_has_ranf	: 1;
                                boolean			contig_array	: 1;
				boolean			shape_known	: 1;
                                Uint			cif_id		: 24;


				opnd_type		char_len;
                                opnd_type		bias_opnd;
# ifdef _F_MINUS_MINUS
                                opnd_type		shape[14];
# else
                                opnd_type		shape[7];
# endif
                               };

struct	cdir_switch_entry	{
				 boolean	align			  : 1;
				 boolean	autoscope	          : 1;
				 boolean	bl			  : 1;
				 boolean	bounds			  : 1;
				 boolean	casedir        		  : 1;
				 boolean	cncall			  : 1;
				 boolean	code			  : 1;
				 boolean	concurrent		  : 1;
				 boolean	do_inline		  : 1;
				 boolean	do_parallel		  : 1;
				 boolean	doall_region	 	  : 1;
				 boolean	flow			  : 1;
				 boolean	guard  	       	          : 1;
				 boolean	guard_has_flag		  : 1;
				 boolean	guard_in_par_reg	  : 1;
				 boolean	ivdep			  : 1;
				 boolean	mark			  : 1;
				 boolean	maxcpus		          : 1;
				 boolean	nextscalar		  : 1;
				 boolean	no_internal_calls	  : 1;
				 boolean	noinline		  : 1;
				 boolean	parallel_region		  : 1;
				 boolean	pattern			  : 1;
				 boolean	permutation		  : 1;
				 boolean	preferstream		  : 1;
				 boolean	preferstream_nocinv	  : 1;
				 boolean	prefertask		  : 1;
				 boolean	prefervector		  : 1;
				 boolean	recurrence		  : 1;
				 boolean	shortloop		  : 1;
				 boolean	shortloop128		  : 1;
#ifdef KEY /* Bug 10441 */
				 boolean        single     	          : 1;
#endif /* KEY Bug 10441 */
				 boolean	split			  : 1;
				 boolean	stream			  : 1;
				 boolean	task			  : 1;
				 boolean	notask_region		  : 1;
				 boolean	unroll_dir		  : 1;
				 boolean	vector			  : 1;
				 boolean	vsearch			  : 1;

				 boolean	aggressiveinnerloopfission : 1;
				 boolean	fission			  : 1;
				 boolean	fissionable		  : 1;
				 boolean	fusable			  : 1;
				 boolean	fuse			  : 1;
				 boolean	noblocking		  : 1;
				 boolean	nofission		  : 1;
				 boolean	nofusion		  : 1;
				 boolean	nointerchange 		  : 1;
				 boolean	opaque			  : 1;
                                 boolean	inline_here_sgi		  : 1;
                                 boolean	noinline_here_sgi	  : 1;

				 int		blockable_count;
				 int		blockable_group;
				 int		blockable_sh_idx;
				 int		bounds_il_list;
				 int		cache_bypass_ir_idx;
				 int		concurrent_idx;
				 int		copyin_list_idx;
				 int		copyprivate_list_idx; /* by jhs, 02/7/22 */
				 int		default_scope_list_idx;
				 int		dir_nest_check_sh_idx;
                                 int		do_omp_sh_idx;
				 int            doacross_sh_idx;
				 int		doall_sh_idx;
                                 int		dopar_sh_idx;
				 int		firstprivate_list_idx;
				 int		getfirst_list_idx;
				 int		implicit_use_idx;
				 int		inline_here_list_idx;
				 int		interchange_count;
				 int		interchange_group;
				 int		interchange_level;
				 int		interchange_sh_idx;
				 int		lastlocal_list_idx;
				 int		lastprivate_list_idx;
				 int		lastthread_list_idx;
				 int		mark_cmdline_idx;
				 int		mark_dir_idx;
				 int		nobounds_il_list;
				 int		noinline_here_list_idx;
                                 int		paralleldo_omp_sh_idx;
				 int            paralleldo_sh_idx;
				 int            pdo_sh_idx;
				 int		private_list_idx;
				 int		reduction_list_idx;
				 int		safevl_idx;
				 int		send_list_idx;
				 int		shared_list_idx;
				 int		wait_list_idx;
				 int		unroll_count_idx;

                                 opnd_type	first_sh_blk_stk;

                                 opnd_type	chunk_opnd;
				 opnd_type	maxcpus_opnd;
                                 opnd_type	mp_schedtype_opnd;
				};


struct	opt_flags_entry {
	boolean		aggress			: 1;	/* -O aggress	      */
	boolean		bottom_load		: 1;	/* -O bl	      */
	boolean		set_fastint_option	: 1;	/* -O fastint         */
	boolean		set_nofastint_option	: 1;	/* -O nofastint       */
	boolean		set_allfastint_option	: 1;	/* -O allfastint      */
	boolean		fusion			: 1;	/* -O fusion	      */
	boolean		ieeeconform		: 1;	/* -O ieeeconform     */
	Uint		inline_lvl		: 3;	/* -O inlinen	      */
	Uint		short_circuit_lvl	: 2;	/* -O inlinen	      */
	boolean		jump			: 1;	/* -O jump	      */
	boolean		loopalign		: 1;	/* -O loopalign	      */
	boolean		mark			: 1;	/* -O mark	      */
	boolean		modinline		: 1;	/* -O modinline	      */
	boolean		extent_assert		: 1;	/* -O inlinene 	      */
	boolean		msgs			: 1;	/* -O msgs	      */
	boolean		neg_msgs		: 1;	/* -O negmsgs	      */
	boolean		nointerchange		: 1;	/* -O nointerchange   */
	boolean		opt_info		: 1;	/* -O opt_info	      */
	boolean		over_index		: 1;	/* -O over_index      */
        boolean		pattern			: 1;	/* -O pattern         */
        Uint		pipeline_lvl		: 2;	/* -O pipeline        */
	boolean		recurrence		: 1;	/* -O recurrence      */
	Uint		scalar_lvl		: 2;	/* -O scalarn	      */
	Uint		split_lvl		: 2;	/* -O scalarn	      */
	Uint		stream_lvl		: 2;	/* -O stream	      */
	Uint		support_lvl		: 2;    /* -O num             */
        Uint		task_lvl		: 2;    /* -O taskn           */
        boolean		taskinner		: 1;    /* -O taskinner       */
        boolean		threshold		: 1;    /* -O threshold       */
	Uint		unroll_lvl		: 2;	/* -O unroll	      */
	Uint		vector_lvl		: 2;	/* -O vectorn 	      */
	boolean		vsearch			: 1;	/* -O vsearch	      */
	boolean		zeroinc			: 1;	/* -O zeroinc	      */
	id_str_type	mark_name;			/* -O mark=name	      */
        boolean		matmul_inline		: 1;	/* -O matmul_inline   */
        boolean		mv_matmul_inline	: 1;	/* -O mv_matmul_inline*/

	Uint		reshape_idx		: 16;	/* -O reshape         */
        boolean		reshape			: 1;	/* -O reshape         */
        boolean		reshape_all_arrays	: 1;	/* -O reshape         */
	};
 

/* Use the following compiler-local definition of the target machine          */
/* structure until it's available from the library.			      */
/* 128 word machine characteristics table.				      */

union	target_machine_entry   {
		struct	{

                         /* Numeric machine characteristics.		      */

/* The SOLARIS version of "mcpmt" is 12 characters to account for an 8        */
/* character target machine name plus the null, rounded to the next word.     */
/* See also the ifdef's at the end of the table for the array overlay.	      */
 
# if defined(_GETPMC_AVAILABLE)
			 long		mcpmt;
# elif defined(_HOST_OS_SOLARIS) || (defined(_HOST_OS_IRIX) || defined(_HOST_OS_LINUX) || defined(_HOST_OS_DARWIN))
                         char		mcpmt[12];
# endif
			 long		mcbank;
			 long		mcncpu;
			 long		mcibsz;
			 long	 	mcmsz;
			 long		mcmspd;
			 long		mcclk;
			 long		mcncl;
			 long		mcbbsy;
        long    mc_serial;              /* System Serial Number         */
        long    mc_rls;                 /* Release level * 1000         */
        long    mc_c_option_rev;        /* hardware C REV number        */
        long    mc_i_option_rev;        /* hardware I REV number        */
        long    mc_r_option_rev;        /* hardware R REV number        */
        long    mc_m_option_rev_0;      /* hardware M_rev number 0      */
        long    mc_m_option_rev_1;      /* hardware M_rev number 1      */
        long    mc_m_option_rev_2;      /* hardware M_rev number 2      */
        long    mc_m_option_rev_3;      /* hardware M_rev number 3      */
        long    mc_subtype;             /* Machine sub-type name,  first word */
        long    mc_subtype1;            /* Machine sub-type name, second word */

			 long		numeric_unused[44];

                         /* Logical machine characteristics.		      */
                         Ulong		mcema;
			 Ulong		mccigs;
			 Ulong		mcvpop;
			 Ulong		mcpc;
			 Ulong		mcrdvl;
			 Ulong		mcvrcr;
			 Ulong		mcavl;
			 Ulong		mchpm;
			 Ulong		mcbdm;
			 Ulong		mcstr;
			 Ulong		mccori;
			 Ulong		mcaddr32;
			 Ulong		mcxea;
			 Ulong		mcbmm;
			 Ulong		mcavpop;
			 Ulong		mcfullsect;
			 Ulong		mcieee;
			 Ulong		mccmrreq;
			 Ulong		mccache;
			 Ulong		logical_unused[45];
                        } fld;

# if defined(_GETPMC_AVAILABLE)
		long	mc_tbl[128];
# elif defined(_HOST_OS_SOLARIS) || (defined(_HOST_OS_IRIX) || defined(_HOST_OS_LINUX) || defined(_HOST_OS_DARWIN))
                long    mc_tbl[130];                 
# endif
	};


/******************************************************************************\
|*   cif_usage_code_values and cif_usage_code_type must be defined here rather*|
|*   than in the more appropriate fecif.h because procedures outside of       *|
|*   fecif.c call cif_usage_rec to output a Usage record.		      *|
|*   CIF_No_Usage_Rec should always be last.                                  *|
\******************************************************************************/

enum	cif_usage_code_values	{CIF_Symbol_Declaration,
				 CIF_Symbol_Reference,
			         CIF_Symbol_Modification,
			         CIF_Symbol_Is_Actual_Arg,
				 CIF_Label_Referenced_In_ASSIGN,
				 CIF_Label_Referenced_As_Branch_Target,
				 CIF_Label_Referenced_As_Format,
				 CIF_Do_Loop_Label,
				 CIF_Derived_Type_Name_Definition,
				 CIF_Derived_Type_Name_Reference,
				 CIF_Construct_Name_Reference,
				 CIF_Symbol_Is_Dummy_Arg,
				 CIF_Symbol_Defined_Opr_Actual_Arg,
				 CIF_Symbol_Is_Hidden_Used_Module,
				 CIF_No_Usage_Rec
				};


enum	cif_directive_code_values {CIF_Master,
				   CIF_End_Master,
				   CIF_Barrier,
				   CIF_No_Barrier,
				   CIF_Critical,
				   CIF_End_Critical,
				   CIF_Shared_Io,
				   CIF_Atomic_Update,
				   CIF_List,
				   CIF_Nolist,
				   CIF_Eject
				  };


/******************************************************************************\
|*   cif_stmt_values and cif_stmt_type must be defined here rather than in    *|
|*   the more appropriate fecif.h because various parsing procedures must call*|
|*   cif_stmt_type_rec when the exact type of the statement is known (for     *|
|*   example, there are several types of DO statements so only the DO stmt    *|
|*   parsing code can accurately call cif_stmt_type_rec).		      *|
|*									      *|
|*   NOTE:  This enum corresponds to an enum in cif.h  Do not change this     *|
|*          without making sure that cif changes also.                        *|
|*									      *|
\******************************************************************************/

enum    cif_stmt_values
	       {CIF_Not_Exact			=  -2,  

		CIF_Stmt_Type_Error		=  -1,

                CIF_Allocatable_Stmt	/* CIF_F90_TP_ALLOCATABLE  */	=   0,
                CIF_Allocate_Stmt,	/* CIF_F90_TP_ALLOCATE     */
                CIF_Assign_Stmt,	/* CIF_F90_TP_ASSIGN       */
                CIF_Assignment_Stmt, 	/* CIF_F90_TP_ASSIGNMENT   */ 
                CIF_Backspace_Stmt,     /* CIF_F90_TP_BACKSPACE    */
                CIF_Block_Data_Stmt	/* CIF_F90_TP_BDATA        */ 	=   5,
 
                CIF_Call_Stmt		/* CIF_F90_TP_CALL         */	=   7,
                CIF_Case_Stmt, 		/* CIF_F90_TP_CASE	   */
                CIF_Close_Stmt,		/* CIF_F90_TP_CLOSE	   */  
                CIF_Common_Stmt 	/* CIF_F90_TP_COMMON	   */	=  10,
                CIF_Contains_Stmt,	/* CIF_F90_TP_CONTAINS	   */
                CIF_Continue_Stmt,	/* CIF_F90_TP_CONTINUE	   */
                CIF_Cycle_Stmt,		/* CIF_F90_TP_CYCLE	   */
                CIF_Data_Stmt,		/* CIF_F90_TP_DATA	   */
                CIF_Deallocate_Stmt 	/* CIF_F90_TP_DEALLOCATE   */	=  15,
		CIF_Decode_Stmt,	/* CIF_F90_TP_DECODE	   */
                CIF_Case_Default_Stmt,  /* CIF_F90_TP_CASE_DEFAULT */
                CIF_Type_Stmt,		/* CIF_F90_TP_TYPE	   */
                CIF_Dimension_Stmt,	/* CIF_F90_TP_DIMENSION	   */
                CIF_Directive_Stmt	/* CIF_F90_TP_DIRECTIVE	   */	=  20,

                CIF_Else_Stmt		/* CIF_F90_TP_ELSE	   */	=  23,
                CIF_Else_If_Stmt,	/* CIF_F90_TP_ELSEIF	   */
                CIF_Elsewhere_Stmt,	/* CIF_F90_TP_ELSEWHERE	   */
                CIF_Encode_Stmt		/* CIF_F90_TP_ENCODE	   */	=  26,

                CIF_Endfile_Stmt	/* CIF_F90_TP_ENDFILE	   */	=  28,
                CIF_Entry_Stmt,		/* CIF_F90_TP_ENTRY	   */
                CIF_Equivalence_Stmt	/* CIF_F90_TP_EQUIVALENCE  */	=  30,
                CIF_Exit_Stmt,		/* CIF_F90_TP_EXIT	   */
                CIF_External_Stmt,	/* CIF_F90_TP_EXTERNAL	   */
                CIF_Format_Stmt,	/* CIF_F90_TP_FORMAT	   */
		CIF_Function_Stmt	/* CIF_F90_TP_FUNCTION	   */	=  34,

                CIF_If_Construct	/* CIF_F90_TP_IF	   */	=  37,
                CIF_Implicit_Stmt,	/* CIF_F90_TP_IMPLICIT	   */
                CIF_Implicit_None_Stmt, /* CIF_F90_TP_IMPLICIT_NONE*/
                CIF_Inquire_Stmt	/* CIF_F90_TP_INQUIRE	   */	=  40,

                CIF_Intrinsic_Stmt	/* CIF_F90_TP_INTRINSIC	   */	=  43,
                CIF_Module_Stmt,	/* CIF_F90_TP_MODULE	   */
                CIF_Module_Procedure_Stmt  /* CIF_F90_TP_MODULE_PROC */ =  45,
                CIF_Namelist_Stmt,	/* CIF_F90_TP_NAMELIST	   */
                CIF_Nullify_Stmt,	/* CIF_F90_TP_NULLIFY	   */
                CIF_Open_Stmt,		/* CIF_F90_TP_OPEN	   */
                CIF_Optional_Stmt,	/* CIF_F90_TP_OPTIONAL	   */
                CIF_Parameter_Stmt 	/* CIF_F90_TP_PARAMETER	   */	=  50,
                CIF_Pause_Stmt,		/* CIF_F90_TP_PAUSE	   */
                CIF_Pointer_Stmt,	/* CIF_F90_TP_POINTER	   */
                CIF_Print_Stmt,		/* CIF_F90_TP_PRINT	   */
                CIF_Private_Stmt,	/* CIF_F90_TP_PRIVATE	   */
                CIF_Program_Stmt,	/* CIF_F90_TP_PROGRAM	   */
                CIF_Public_Stmt		/* CIF_F90_TP_PUBLIC	   */	=  56,

                CIF_Read_Stmt		/* CIF_F90_TP_READ	   */	=  58,
                CIF_Return_Stmt,	/* CIF_F90_TP_RETURN	   */
                CIF_Rewind_Stmt 	/* CIF_F90_TP_REWIND	   */	=  60,
                CIF_Save_Stmt,		/* CIF_F90_TP_SAVE	   */
                CIF_Select_Case_Stmt,	/* CIF_F90_TP_SELECT_CASE  */
                CIF_Sequence_Stmt,	/* CIF_F90_TP_SEQUENCE	   */
                CIF_Statement_Function_Stmt, /* CIF_F90_TP_STMTFUNC*/
                CIF_Stop_Stmt 		/* CIF_F90_TP_STOP	   */	=  65,
                CIF_Subroutine_Stmt,	/* CIF_F90_TP_SUBROUTINE   */
                CIF_Target_Stmt,	/* CIF_F90_TP_TARGET	   */
                CIF_Type_Declaration_Stmt,  /* CIF_F90_TP_TYPE_DECL*/
                CIF_Use_Stmt,		/* CIF_F90_TP_USE	   */
                CIF_Where_Stmt  	/* CIF_F90_TP_WHERE	   */	=  70,
                CIF_Where_Construct,	/* CIF_F90_TP_WHERE_CONST  */
                CIF_Write_Stmt,		/* CIF_F90_TP_WRITE	   */
                CIF_Buffer_In_Stmt,	/* CIF_F90_TP_BUFFER_IN    */
                CIF_Buffer_Out_Stmt	/* CIF_F90_TP_BUFFER_OUT   */	=  74,

                CIF_End_Do_Stmt		/*  CIF_F90_TP_END_DO	   */	=  77,
                CIF_End_If_Stmt,	/* CIF_F90_TP_END_IF	   */
                CIF_Include_Stmt,	/* CIF_F90_TP_INCLUDE	   */
		CIF_CDIR_Stmt		/* CIF_F90_TP_CDIR	   */	=  80,
       		CIF_Array_Assignment_Stmt, /* CIF_F90_TP_ARRAY_ASSIGNMENT */
		CIF_Pointer_Assigment_Stmt,/* CIF_F90_TP_POINTER_ASG      */

                CIF_Do_Labeled_Infinite_Stmt
				 /* CIF_F90_TP_LABELED_DO_INFINITE  */	= 100,
                CIF_Do_Unlabeled_Infinite_Stmt,
				 /* CIF_F90_TP_LABELED_DO_INFINITE  */
                CIF_Do_Labeled_Iterative_Stmt,    
				 /* CIF_F90_TP_LABELED_DO_ITERATIVE */
                CIF_Do_Unlabeled_Iterative_Stmt,
				 /* CIF_F90_TP_UNLABELED_DO_ITERATIVE */
                CIF_Do_Labeled_While_Stmt,
				 /* CIF_F90_TP_LABELED_DO_WHILE	    */
                CIF_Do_Unlabeled_While_Stmt 
				 /* CIF_F90_TP_UNLABELED_DO_WHILE   */	= 105,
                CIF_End_Block_Data_Stmt,/* CIF_F90_TP_END_BDATA    */
                CIF_End_Function_Stmt,	/* CIF_F90_TP_END_FUNCTION */
                CIF_End_Interface_Stmt, /* CIF_F90_TP_END_INTERFACE*/
                CIF_End_Module_Stmt,	/* CIF_F90_TP_END_MODULE   */
                CIF_End_Program_Stmt 	/* CIF_F90_TP_END_PROGRAM  */	= 110,
                CIF_End_Select_Stmt,	/* CIF_F90_TP_END_SELECT   */
                CIF_End_Subroutine_Stmt,/* CIF_F90_TP_END_SUBROUTINE */
                CIF_End_Type_Stmt,	/* CIF_F90_TP_END_TYPE	   */
                CIF_End_Where_Stmt,	/* CIF_F90_TP_END_WHERE    */
                CIF_Go_To_Unconditional_Stmt  /* CIF_F90_TP_GOTO   */	= 115,
                CIF_Go_To_Assigned_Stmt,/* CIF_F90_TP_GOTO_ASSIGNED*/
                CIF_Go_To_Computed_Stmt,/* CIF_F90_TP_GOTO_COMPUTED*/
                CIF_If_Logical_Stmt,	/* CIF_F90_TP_IF_LOGICAL   */
                CIF_If_Arithmetic_Stmt, /* CIF_F90_TP_IF_ARITHMETIC*/
                CIF_If_Indirect_Logical_Stmt
				 /* CIF_F90_TP_IF_IND_LOG */		= 120,
                CIF_If_Two_Branch_Arithmetic_Stmt,
				 /* CIF_F90_TP_IF_TWO_BRANCH_ARITHMETIC */
                CIF_Intent_In_Stmt,	/* CIF_F90_TP_INTENT_IN	   */
                CIF_Intent_Out_Stmt,	/* CIF_F90_TP_INTENT_OUT   */
                CIF_Intent_Inout_Stmt,	/* CIF_F90_TP_INTENT_INOUT */
                CIF_Interface_Explicit_Stmt  /* CIF_F90_TP_INTERFACE */ = 125,
                CIF_Interface_Generic_Stmt,
				 /* CIF_F90_TP_INTERFACE_GENERIC   */
                CIF_Interface_Operator_Stmt,
				 /* CIF_F90_TP_INTERFACE_OPERATOR  */
                CIF_Interface_Assignment_Stmt,
				 /* CIF_F90_TP_INTERFACE_ASSIGNMENT*/
                CIF_Task_Common_Stmt,	/* CIF_F90_TP_TASK_COMMON  */
                CIF_Automatic_Stmt 	/* CIF_F90_TP_AUTOMATIC    */	= 130,
                CIF_Elemental_Stmt,	/* CIF_F90_TP_ELEMENTAL	   */
                CIF_Pure_Stmt,		/* CIF_F90_TP_PURE	   */
		CIF_Forall_Stmt,	/* CIF_F90_TP_FORALL	   */
		CIF_Forall_Construct,	/* CIF_F90_TP_FORALL_CONSTRUCT */
		CIF_Max,		/* CIF_F90_TP_MAX */
		CIF_End_Forall_Stmt 	/* CIF_F90_TP_END_FORALL   */
               };

typedef enum	cif_directive_code_values	cif_directive_code_type;
typedef enum    cif_stmt_values			cif_stmt_type;
typedef	enum	cif_usage_code_values		cif_usage_code_type;


/*******************************************\
|* globally accessible function prototypes *|
\*******************************************/

extern  void  unknown_intrinsic (opnd_type *, expr_arg_type *, int *);
#ifdef KEY /* Bug 1683 */
extern  void  pathf90_intrinsic (opnd_type *, expr_arg_type *, int *);
#endif /* KEY Bug 1683 */
#ifdef KEY /* Bug 5089 */
extern  void  true_intrinsic (opnd_type *, expr_arg_type *, int *);
extern  void  support_uflow_intrinsic (opnd_type *, expr_arg_type *, int *);
#endif /* KEY Bug 5089 */
#ifdef KEY /* F2003 */
extern  void  newline_intrinsic (opnd_type *, expr_arg_type *, int *);
#endif /* KEY F2003 */
extern  void  abs_intrinsic     (opnd_type *, expr_arg_type *, int *);
extern  void  sin_intrinsic     (opnd_type *, expr_arg_type *, int *);
extern  void  erf_intrinsic     (opnd_type *, expr_arg_type *, int *);
extern  void  atan2_intrinsic   (opnd_type *, expr_arg_type *, int *);
extern  void  exit_intrinsic    (opnd_type *, expr_arg_type *, int *);
extern  void  aimag_intrinsic   (opnd_type *, expr_arg_type *, int *);
extern  void  int_intrinsic     (opnd_type *, expr_arg_type *, int *);
extern  void  ilen_intrinsic    (opnd_type *, expr_arg_type *, int *);
extern  void  iand_intrinsic    (opnd_type *, expr_arg_type *, int *);
extern  void  mod_intrinsic     (opnd_type *, expr_arg_type *, int *);
extern  void  anint_intrinsic   (opnd_type *, expr_arg_type *, int *);
extern  void  nint_intrinsic    (opnd_type *, expr_arg_type *, int *);
extern  void  sign_intrinsic    (opnd_type *, expr_arg_type *, int *);
extern  void  modulo_intrinsic  (opnd_type *, expr_arg_type *, int *);
extern  void  shift_intrinsic   (opnd_type *, expr_arg_type *, int *);
extern  void  leadz_intrinsic   (opnd_type *, expr_arg_type *, int *);
extern  void  not_intrinsic     (opnd_type *, expr_arg_type *, int *);
extern  void  aint_intrinsic    (opnd_type *, expr_arg_type *, int *);
extern  void  dim_intrinsic     (opnd_type *, expr_arg_type *, int *);
extern  void  max_intrinsic     (opnd_type *, expr_arg_type *, int *);
extern  void  ranget_intrinsic  (opnd_type *, expr_arg_type *, int *);
extern  void  ranf_intrinsic    (opnd_type *, expr_arg_type *, int *);
extern  void  real_intrinsic    (opnd_type *, expr_arg_type *, int *);
extern  void  mask_intrinsic    (opnd_type *, expr_arg_type *, int *);
extern  void  conjg_intrinsic   (opnd_type *, expr_arg_type *, int *);
extern  void  dprod_intrinsic   (opnd_type *, expr_arg_type *, int *);
extern  void  i24mult_intrinsic (opnd_type *, expr_arg_type *, int *);
extern  void  length_intrinsic  (opnd_type *, expr_arg_type *, int *);
extern  void  getpos_intrinsic  (opnd_type *, expr_arg_type *, int *);
extern  void  unit_intrinsic    (opnd_type *, expr_arg_type *, int *);
extern  void  cmplx_intrinsic   (opnd_type *, expr_arg_type *, int *);
extern  void  len_intrinsic     (opnd_type *, expr_arg_type *, int *);
extern  void  ichar_intrinsic   (opnd_type *, expr_arg_type *, int *);
extern  void  idate_intrinsic   (opnd_type *, expr_arg_type *, int *);
extern  void  char_intrinsic    (opnd_type *, expr_arg_type *, int *);
extern  void  lint_intrinsic    (opnd_type *, expr_arg_type *, int *);
extern  void  index_intrinsic   (opnd_type *, expr_arg_type *, int *);
extern  void  lge_intrinsic     (opnd_type *, expr_arg_type *, int *);
extern  void  numarg_intrinsic  (opnd_type *, expr_arg_type *, int *);
extern  void  fcd_intrinsic     (opnd_type *, expr_arg_type *, int *);
extern  void  loc_intrinsic     (opnd_type *, expr_arg_type *, int *);
extern  void  clock_intrinsic   (opnd_type *, expr_arg_type *, int *);
#ifdef KEY
extern  void  c_f_pointer_intrinsic(opnd_type *, expr_arg_type *, int *);
#endif /* KEY Bug 14150 */
#ifdef KEY
extern  void  time_intrinsic    (opnd_type *, expr_arg_type *, int *);
extern  void  dtime_intrinsic    (opnd_type *, expr_arg_type *, int *);
#endif
extern  void  rtc_intrinsic     (opnd_type *, expr_arg_type *, int *);
extern  void  my_pe_intrinsic   (opnd_type *, expr_arg_type *, int *);
extern  void  cvmgp_intrinsic   (opnd_type *, expr_arg_type *, int *);
extern  void  cvmgt_intrinsic   (opnd_type *, expr_arg_type *, int *);
extern  void  csmg_intrinsic    (opnd_type *, expr_arg_type *, int *);
extern  void  mergee_intrinsic  (opnd_type *, expr_arg_type *, int *);
extern  void  adjustl_intrinsic (opnd_type *, expr_arg_type *, int *);
extern  void  ceiling_intrinsic (opnd_type *, expr_arg_type *, int *);
extern  void  digits_intrinsic  (opnd_type *, expr_arg_type *, int *);
extern  void  epsilon_intrinsic (opnd_type *, expr_arg_type *, int *);
extern  void  exponent_intrinsic(opnd_type *, expr_arg_type *, int *);
extern  void  floor_intrinsic   (opnd_type *, expr_arg_type *, int *);
extern  void  fraction_intrinsic(opnd_type *, expr_arg_type *, int *);
extern  void  huge_intrinsic    (opnd_type *, expr_arg_type *, int *);
extern  void  ibits_intrinsic   (opnd_type *, expr_arg_type *, int *);
extern  void  ibset_intrinsic   (opnd_type *, expr_arg_type *, int *);
extern  void  btest_intrinsic   (opnd_type *, expr_arg_type *, int *);
extern  void  ishft_intrinsic   (opnd_type *, expr_arg_type *, int *);
extern  void  ishftc_intrinsic  (opnd_type *, expr_arg_type *, int *);
extern  void  mvbits_intrinsic  (opnd_type *, expr_arg_type *, int *);
extern  void  all_intrinsic     (opnd_type *, expr_arg_type *, int *);
extern  void  tiny_intrinsic    (opnd_type *, expr_arg_type *, int *);
extern  void  spacing_intrinsic (opnd_type *, expr_arg_type *, int *);
extern  void  cshift_intrinsic  (opnd_type *, expr_arg_type *, int *);
extern  void  eoshift_intrinsic (opnd_type *, expr_arg_type *, int *);
extern  void  minloc_intrinsic  (opnd_type *, expr_arg_type *, int *);
extern  void  minval_intrinsic  (opnd_type *, expr_arg_type *, int *);
extern  void  matmul_intrinsic  (opnd_type *, expr_arg_type *, int *);
extern  void  pack_intrinsic    (opnd_type *, expr_arg_type *, int *);
extern  void  unpack_intrinsic  (opnd_type *, expr_arg_type *, int *);
extern  void  trim_intrinsic    (opnd_type *, expr_arg_type *, int *);
extern  void  spread_intrinsic  (opnd_type *, expr_arg_type *, int *);
extern  void  repeat_intrinsic  (opnd_type *, expr_arg_type *, int *);
extern  void  size_intrinsic    (opnd_type *, expr_arg_type *, int *);
extern  void  sizeof_intrinsic  (opnd_type *, expr_arg_type *, int *);
extern  void  lbound_intrinsic  (opnd_type *, expr_arg_type *, int *);
extern  void  ubound_intrinsic  (opnd_type *, expr_arg_type *, int *);
extern  void  shape_intrinsic   (opnd_type *, expr_arg_type *, int *);
extern  void  reshape_intrinsic (opnd_type *, expr_arg_type *, int *);
extern  void  radix_intrinsic   (opnd_type *, expr_arg_type *, int *);
extern  void  range_intrinsic   (opnd_type *, expr_arg_type *, int *);
extern  void  kind_intrinsic    (opnd_type *, expr_arg_type *, int *);
extern  void  present_intrinsic (opnd_type *, expr_arg_type *, int *);
extern  void  logical_intrinsic (opnd_type *, expr_arg_type *, int *);
extern  void  nearest_intrinsic (opnd_type *, expr_arg_type *, int *);
extern  void  scale_intrinsic   (opnd_type *, expr_arg_type *, int *);
extern  void  dshiftl_intrinsic (opnd_type *, expr_arg_type *, int *);
extern  void  mmx_intrinsic     (opnd_type *, expr_arg_type *, int *);
extern  void  mldmx_intrinsic   (opnd_type *, expr_arg_type *, int *);
extern  void  mld_intrinsic     (opnd_type *, expr_arg_type *, int *);
extern  void  mul_intrinsic     (opnd_type *, expr_arg_type *, int *);
extern  void  mclr_intrinsic    (opnd_type *, expr_arg_type *, int *);
extern  void  readsm_intrinsic  (opnd_type *, expr_arg_type *, int *);
extern  void  ieee_finite_intrinsic  (opnd_type *, expr_arg_type *, int *);
extern  void  ieee_real_intrinsic    (opnd_type *, expr_arg_type *, int *);
extern  void  transfer_intrinsic     (opnd_type *, expr_arg_type *, int *);
extern  void  transpose_intrinsic    (opnd_type *, expr_arg_type *, int *);
extern  void  minexponent_intrinsic  (opnd_type *, expr_arg_type *, int *);
extern  void  maxexponent_intrinsic  (opnd_type *, expr_arg_type *, int *);
extern  void  precision_intrinsic    (opnd_type *, expr_arg_type *, int *);
extern  void  bit_size_intrinsic     (opnd_type *, expr_arg_type *, int *);
extern  void  rrspacing_intrinsic    (opnd_type *, expr_arg_type *, int *);
extern  void  set_exponent_intrinsic (opnd_type *, expr_arg_type *, int *);
extern  void  dot_product_intrinsic  (opnd_type *, expr_arg_type *, int *);
extern  void  allocated_intrinsic    (opnd_type *, expr_arg_type *, int *);
extern  void  associated_intrinsic   (opnd_type *, expr_arg_type *, int *);
extern  void  len_trim_intrinsic     (opnd_type *, expr_arg_type *, int *);
extern  void  random_number_intrinsic(opnd_type *, expr_arg_type *, int *);
extern  void  random_seed_intrinsic  (opnd_type *, expr_arg_type *, int *);
extern  void  fetch_and_add_intrinsic(opnd_type *, expr_arg_type *, int *);
extern  void  lock_release_intrinsic (opnd_type *, expr_arg_type *, int *);
extern  void  synchronize_intrinsic  (opnd_type *, expr_arg_type *, int *);
extern  void  free_intrinsic 	     (opnd_type *, expr_arg_type *, int *);
extern  void  null_intrinsic 	     (opnd_type *, expr_arg_type *, int *);
extern  void  malloc_intrinsic 	     (opnd_type *, expr_arg_type *, int *);
extern  void  num_images_intrinsic   (opnd_type *, expr_arg_type *, int *);
extern  void  system_clock_intrinsic (opnd_type *, expr_arg_type *, int *);
#ifdef KEY
extern  void  fnum_intrinsic         (opnd_type *, expr_arg_type *, int *);
extern  void  stat_intrinsic         (opnd_type *, expr_arg_type *, int *);
extern  void  fstat_intrinsic         (opnd_type *, expr_arg_type *, int *);
extern  void  signal_intrinsic       (opnd_type *, expr_arg_type *, int *);
extern  void  kill_intrinsic         (opnd_type *, expr_arg_type *, int *);
#endif
extern  void  dsm_numthreads_intrinsic      
			(opnd_type *, expr_arg_type *, int *);
extern  void  omp_get_max_threads_intrinsic 
			(opnd_type *, expr_arg_type *, int *);
extern  void  omp_set_lock_intrinsic 	    
			(opnd_type *, expr_arg_type *, int *);
extern  void  compare_and_swap_intrinsic    
			(opnd_type *, expr_arg_type *, int *);
extern  void  selected_int_kind_intrinsic   
			(opnd_type *, expr_arg_type *, int *);
extern  void  selected_real_kind_intrinsic  
			(opnd_type *, expr_arg_type *, int *);
extern  void  memory_barrier_intrinsic      
			(opnd_type *, expr_arg_type *, int *);
extern  void  get_ieee_status_intrinsic     
			(opnd_type *, expr_arg_type *, int *);
extern  void  set_ieee_exception_intrinsic  
			(opnd_type *, expr_arg_type *, int *);
extern  void  test_ieee_interrupt_intrinsic 
			(opnd_type *, expr_arg_type *, int *);
extern  void  remote_write_barrier_intrinsic
			(opnd_type *, expr_arg_type *, int *);
extern  void  write_memory_barrier_intrinsic
			(opnd_type *, expr_arg_type *, int *);

extern	void	 	check_dependence(boolean    *dependant,
                                         opnd_type  item, 
                                         opnd_type  exp);
extern  void            array_bounds_resolution(int, boolean *);
extern	void		array_dim_resolution(int, boolean);
extern	void		pe_array_dim_resolution(int);
extern	int		bound_semantics(int, boolean);
extern	void		bounds_cdir_handler(int);
extern  int		cast_typeless_constant(int, int, int, int);
extern  void    	cast_to_type_idx(opnd_type *, expr_arg_type *, int);
extern	void		char_len_resolution(int, boolean);
extern	void		char_bounds_resolution(int, boolean *);
extern  boolean		check_asg_semantics(int, int, int, int);
extern	boolean		check_substring_bounds(int);
extern	boolean		check_array_bounds(int);
extern	void		cif_begin_scope_rec(void);
extern	void		cif_call_site_rec(int, int);
extern	void		cif_cont_line_rec(int, int);
extern	void		cif_copy_temp_to_actual_CIF(void);
extern  void		cif_directive_rec(cif_directive_code_type, int, int);
extern	void		cif_enable_disable_rec(void);
extern	void		cif_end_scope_rec(void);
extern  void		cif_end_unit_rec(char *);
extern	void		cif_fake_a_unit (void);
extern  int		cif_file_name_rec(char *, char *);
extern	void		cif_include_rec(int, int, int);
extern	void		cif_interface_block_rec(void);
extern	void		cif_label_rec(int);
extern	void		cif_loop_def_rec(void);
extern	void		cif_machine_characteristics_rec(void);
extern	void		cif_message_rec(int, int, int, msg_severities_type,
                                        char*, long, long, long, long, char*, int);
extern	void		cif_misc_compiler_opts_rec(void);
extern	void		cif_named_constant_rec(int, int, int);
extern	void		cif_optimization_opts_rec(void);
extern	void		cif_prog_unit_init(void);
extern	int		cif_rename_rec(int, int, int, int);
extern	void		cif_sb_usage_rec(int, int, int, cif_usage_code_type);
extern	void		cif_scope_info_rec(void);
extern	void		cif_send_attr(int, int);
extern	void		cif_send_sytb(void);
extern	void		cif_source_file_rec(int, src_form_type);
extern	void		cif_stmt_type_rec(boolean, cif_stmt_type, int);
extern  void		cif_summary_rec(char*, char*, char*, float, long, long);
extern  void		cif_unit_rec(void);
extern	void		cif_usage_rec(int, fld_type, int, int, int);
extern	void		cif_use_module_rec(int, int, boolean);
extern  void		clean_up_module_files(void);
extern  void		close_cif(void);
extern  void		collapse_interface_blk(int);
# if defined(_HOST32) && defined(_TARGET64)
extern  boolean		compare_cn_and_value(int, long long, int);
#else
extern  boolean		compare_cn_and_value(int, long, int);
#endif
extern	boolean		compare_opnds(opnd_type *, opnd_type *);
extern	char   	       *convert_cval_to_string(long64 *, int, char *);
extern	char   	       *convert_to_string(long_type *, int, char *);
extern  void		copy_subtree(opnd_type *, opnd_type *);
extern  int		copy_to_gl_subtree(int, fld_type);
extern  int		copy_from_gl_subtree(int, fld_type);
extern  void            copy_entry_exit_sh_list(int, int, int *,  int *);
extern  int		create_bd_ntry_for_const(expr_arg_type *,int,int);
extern	void		create_mod_info_tbl(void);
#ifdef KEY /* Bug 3477 */
extern	boolean		create_mod_info_file(void);
#else
extern	void		create_mod_info_file(void);
#endif /* KEY Bug 3477 */
extern	int		cvrt_str_to_cn(char *, int);
extern	void		cvrt_to_pdg(char *);
extern	void		decl_semantics(void);
extern  void		exit_compiler(int);
extern  boolean		expr_is_symbolic_constant(opnd_type *);
extern  boolean		expr_semantics(opnd_type *, expr_arg_type *);
#ifdef KEY /* Bug 5710 */
extern  int		eq_ne_on_logical(expr_arg_type *exp_desc,
		         expr_arg_type *exp_desc_l, expr_arg_type *exp_desc_r);
#endif /* KEY Bug 5710 */
extern  boolean		expr_sem(opnd_type *, expr_arg_type *);
extern	boolean		find_attr_in_il(int, int, opnd_type *);
extern	boolean		find_attr_in_ir(int, int, opnd_type *);
extern	boolean		find_prog_unit_tbl(int);
extern	void		fixed_get_char(void);
extern	void		fixed_get_char_literal(void);
#ifdef KEY /* Bug 572 */
/* Set TRUE before calling expr_semantics() if a constant null pointer is
 * legal in this context. */
extern boolean          constant_ptr_ok;
#endif /* KEY Bug 572 */
extern  boolean		fold_aggragate_expression(opnd_type *, expr_arg_type *,
                                                  boolean);
extern  boolean		fold_relationals(int, int, operator_type);
extern  boolean         folder_driver(char *, int, char *, int, long_type *,
                                      int *, int, int, int, int, ...);
#ifdef KEY /* Bug 12482 */
extern void		copy_and_pad_boz(long_type *dst, Uint dst_words,
			  long_type *src, Uint src_words);
#endif /* KEY Bug 12482 */
extern	void		free_tables(void);
extern	void		free_get_char(void);
extern	void		free_get_char_literal(void);
extern	void		free_ir_list(int);
extern	void		free_ir_stream(int);
extern	void		free_stmt_expansion_opr(int);
extern	boolean         fnd_semantic_err(obj_type, int, int, int, boolean);
extern	void		gen_if_stmt(opnd_type *, int, int, int, int, int, int);
extern	int		gen_il(int, boolean, int, int, ...);
extern  int             gen_ir(fld_type, int, operator_type, int,
                               int, int, fld_type, int);

extern	void		gen_rbounds_condition(opnd_type *, opnd_type *,
                                              opnd_type *, opnd_type *,
                                              opnd_type *, opnd_type *, 
                                              int, int);
extern	void		gen_runtime_checks(opnd_type *);
extern	void		gen_runtime_ptr_chk(opnd_type *);
extern	void		gen_sh(sh_position_type, stmt_type_type, int,
                               int, boolean, boolean, boolean);
#ifdef KEY /* Bug 4955 */
extern	int	        gen_present_ir(int, int, int);
#endif /* KEY Bug 4955 */
#ifdef KEY /* Bug 4811 */
extern  int		gen_sh_at(sh_position_type, stmt_type_type, int,
                               int, boolean, boolean, boolean, int);
#endif /* KEY Bug 4811 */
#ifdef KEY /* Bug 6845 */
int do_count_allocatable_cpnt(int, int);
int do_make_struct_opr(int, int, int, fld_type, int);
int build_call(glb_tbl_idx_type, char *, int, int, int);
int pass_by_ref(fld_type, int, int, int);
int pre_gen_loops(int, int, int *);
void post_gen_loops(int, int);
void gen_loops(opnd_type *, opnd_type *, boolean);
#endif /* KEY Bug 6845 */
extern	void		gen_gl_sh(sh_position_type, stmt_type_type, int,
                               int, boolean, boolean, boolean);
extern	void		gen_internal_call_stmt(char *, opnd_type *, 
                                               sh_position_type);
extern	void		gen_lb_array_ref(opnd_type *, int);
extern	void		gen_opnd(opnd_type *, int, fld_type, int, int);
extern  int		get_next_array_expr_element(opnd_type *, long64 *);
extern	char	       *get_src_path_name(void);
extern	int		gen_stmt_expansion_opr(int, int);
extern  boolean         get_temp_file(char *, FILE **, char *);
extern	void		gen_temp_init(int, int);
extern	int		gen_initialized_tmp(int, int, int);
extern	boolean		gen_whole_substring (opnd_type *, int);
extern	char	       *global_to_local_file(int);
extern	int		global_to_local_line_number(int);
extern	char	       *global_to_local_path(int);
extern	int		global_to_file_line_number(int);
extern  void		init_cif(char *, char *);
extern  void		init_directive(int);
extern  void            inline_processing(int);
extern  void		insert_init_stmt_for_tmp(int);
extern  void		insert_sh_chain(int, int, sh_position_type);
extern  void		insert_sh_chain_after_entries(int, int);
extern  void            insert_sh_chain_before(int);
extern  void            interface_semantics_pass_driver(void);
extern  void    	issue_deferred_msgs (void);
extern  void    	issue_undefined_type_msg (int, int, int);
extern	int		main(int, char *[]);
extern  void		make_io_type_code(int, long_type *);
extern	int		make_in_parent_string(int, int, int, int *);
extern  void		mark_attr_defined(opnd_type *);
extern	boolean		needs_bounds_check(int);
extern	void		ntr_msg_queue(int, int, msg_severities_type, int,
				      char *, long, int);
extern	void		output_mod_info_file(void);
extern	void		parse_prog_unit(void);
extern	void		pdgcs_conversion(void);
#ifdef KEY /* Bug 6121 */
/* Move PRINTMSG et al to printmsg.h so C++ can call it */
#else
extern	void		PRINTMSG(int, int, msg_severities_type, int, ...);
#endif /* KEY Bug 6121 */
#ifdef KEY /* Bug 5040 */
extern  msg_severities_type ansi_or_warning(void);
#endif /* KEY Bug 5040 */
extern	void	        print_const_f(FILE *, int);
extern	void		print_err_line(int, int);
extern	void		print_scp_to_fortran(int, int, int, FILE *);
extern	char	       *print_type_f(int);
extern  int             put_const_in_tbl(long);
extern	void		remove_sh(int);
extern	void		reset_lex(int, int);
extern	void		reset_src_input(int, int);
extern	void		scan_for_ptr_chk(opnd_type *);
extern  char            scan_thru_close_paren(int, int, int);
extern  void            semantics_pass_driver(void);
extern	void		set_related_gl_source_lines(int);
extern 	int		set_up_logical_constant(long_type *, int, int, boolean);
extern  void            set_up_which_entry_tmp(void);
extern	boolean		set_stmt_type_known(void);
extern	long64		sm_unit_in_bits(int);
extern	void		stmt_expansion_control_start(void);
extern	void		stmt_expansion_control_end(opnd_type *);
extern	void		terminate_PDGCS(void);
extern  void		final_src_input(void);
extern  FILE 	       *init_debug_file(void);
extern	boolean		omp_extension_prefix(int);
extern  void		print_al(int);
extern  void		print_al_list(FILE *,int);
extern  void		print_at(int);
extern  void		print_at_all(int);
extern  void		print_bd(int);
extern  void		print_blk(int);
extern  void		print_cn(int);
extern  void		print_defines(void);
extern	void		print_expanded_stmt(void);
extern  void		print_eq(int);
extern  void		print_fp(int);
extern  void		print_ga(int);
extern  void		print_gb(int);
extern  void		print_gl(int);
extern  void		print_gn(int);
extern  void		print_gt(int);
extern  void		print_hn(int);
extern  void		print_il(int);
extern  void		print_ir(int);
extern  void		print_ln(int);
extern  void		print_lnr(int, int);
extern	void		print_mem_usage_report(char *, int, int);
extern  void		print_ml(int);
extern  void		print_mf(int);
extern  void		print_ro(int);
extern  void		print_sb(int);
extern  void		print_scp(int, boolean);
extern  void		print_sn(int);
extern  void		print_sn_list(int);
extern  void		print_src_stk_entry(int);  
extern	void		print_src_input_tbls(void);	
extern  void		print_typ(int);
extern  void		print_blk_tbl(void);
extern  void		print_bd_tbl(void);
extern  void		print_cmd_tbl(void);
extern  void		print_compressed_sytb(int, int);
extern  void		print_cn_tbl(void);
extern  void		print_eq_tbl(void);
extern  void		print_fp_includes(void);
extern  void		print_fp_tbl(void);
extern  void		print_ga_tbl(void);
extern  void		print_gb_tbl(void);
extern  void		print_gl_tbl(void);
extern  void		print_gn_tbl(void);
extern  void		print_gt_tbl(void);
extern  void		print_hn_tbl(void);
extern  void		print_ir_tbl(void);
extern  void		print_ln_tbl(void);
extern  void		print_ml_tbl(void);
extern  void		print_ro_tbl(int);
extern  void		print_sb_tbl(void);
extern  void		print_scp_tbl(void);
extern  void		print_sh_tbl(boolean);
extern  void		print_sn_tbl(void);
extern  void		print_typ_tbl(void);
extern  void		print_sytb(int, boolean, boolean);
#ifdef KEY /* Bug 8117 */
extern  void		print_arg_passing(FILE *);
#endif /* KEY Bug 8117 */
extern  void		print_attr_by_name(void);
extern  void		print_ln_by_name(void);
extern  void		print_sb_by_name(void);
extern  void		dump_func_trace_info(trace_type, char *, char *);
extern  void		dump_mem_trace_info(trace_type, char *, void *,
					     void *, long, int);

/*****************************************************\
|* globally accessible objects in ALPHABETICAL ORDER *|
\*****************************************************/

extern	ac_cmd_line_flags_type	ac_cmd_line_flags;
extern	char			assembly_file[];
extern	char			assembly_listing_file[];
extern	boolean			assembly_output;
extern	char		       *basic_type_str[];
extern	char			bin_file[];
extern	boolean			binary_output;
extern	long			ccg_dump_flags;
extern  char    		dot_i_file[];
extern	cdir_switch_type	cdir_switches;
extern	FILE		       *c_i_f;			/* cif output file */
extern	int			cif_C_opts;
extern	boolean			cif_first_pgm_unit;
extern  int			cif_flags;
extern	int			cif_internal_proc_start_line;
extern	int			cif_module_proc_start_line;
extern  char    		cif_name[];
extern	boolean			cif_need_unit_rec;
extern	FILE		       *cif_actual_file;	/* The real CIF file */
extern	boolean			cif_pgm_unit_error_recovery;
extern	int			cif_pgm_unit_start_line;
extern	FILE		       *cif_tmp_file;
extern	boolean			cif_tmp_so_no_msg;
extern	cmd_line_flags_type	cmd_line_flags;
extern  int			code_size;
extern	boolean			comp_gen_expr;
extern	int			comp_phase;
extern  int			const_safevl_idx;
extern	convert_to_string_type	convert_to_string_fmt;
extern	int			curr_debug_lbl;
extern	int			curr_glb_line;
extern	int			curr_internal_lbl;
extern	int			curr_scp_idx;
extern	int			curr_stmt_sh_idx;
extern	int			curr_gl_stmt_sh_idx;
extern  int			data_size;
extern	linear_type_type        double_linear_type[Num_Fortran_Types];
extern	boolean			disregard_mics[];
extern	boolean			disregard_directive[];
extern	boolean			disregard_mips[];
extern	boolean			disregard_open_mp[];
extern	FILE		       *debug_file;
extern	char			debug_file_name[];
extern	dump_flags_type		dump_flags;
extern	long		       *dt_cmp_tbl;  /* Assumes long is word length */
extern	int			expanded_intrinsic_list;
extern	expr_mode_type		expr_mode;
#ifdef KEY /* Bug 4232 */
/* We're processing a statement function definition (rather than a call) so
 * don't create temps to hold the arguments if they're by-reference. */
extern  boolean			defining_stmt_func;
#endif /* KEY Bug 4232 */
extern	void			(*get_char) ();
extern  void                    (*get_char_literal) ();
extern	long			glb_tbl_idx[Num_Glb_Tbl_Idxs];
extern	int			global_stmt_sh_idx;
extern	linear_type_type        half_linear_type[Num_Fortran_Types];
extern	boolean			have_unnamed_pgm_unit;
extern	boolean			have_main_pgm_unit;
extern	boolean			need_pure_function;
extern  boolean                 host_ieee;
extern	boolean			in_action_stmt_of_if_where_or_forall;
extern	int			include_path_idx;
extern	linear_type_type        init_default_linear_type[Num_Fortran_Types];
extern	linear_type_type        default_linear_type[Num_Fortran_Types];
extern	expr_arg_type		init_exp_desc;
extern	opnd_type		init_target_opnd;
extern	boolean			inline_global_sgi;
extern	int			inline_path_idx;
extern	boolean			issue_overflow_msg_719;
extern  long			max_call_list_size;
extern  long_type		max_character_length;
extern	char			mod_out_path[];
extern	int			module_path_idx;
#ifdef KEY /* Bug 5089 */
/* Search path for .mod files for F2003 intrinsic modules */
extern	int			intrinsic_module_path_idx;
#endif /* KEY Bug 5089 */
extern	boolean			need_new_sh;
extern	boolean			need_to_issue_719;
extern	boolean			no_func_expansion;
extern	boolean			noinline_global_sgi;
extern	opnd_type		null_opnd;
extern	int			num_ansi;
extern	int			num_cautions;
extern	int			num_comments;
extern	int			num_errors;
extern	int			num_notes;
extern	int			num_optz_msgs;
extern	int			num_warnings;
extern	int			num_of_derived_types;
extern	int			num_prog_unit_errors;
extern	on_off_flags_type	on_off_flags;
extern	opt_flags_type		opt_flags;
extern  char			parse_operand_insert[40];
extern  int                     pgm_unit_start_line;      
extern	char			preinline_file[];
extern	int			prev_statement_number;
extern  char			program_unit_name[MAX_ID_LEN+1];
extern  int                     register_bit_size_tbl[Num_Linear_Types];
extern	int			sb_len[];
extern	char		       *sb_name[];
extern	char		       *search_str[];
extern  src_form_type           source_form;
extern	char			src_file[];
extern	int			statement_number;
extern  int                     stmt_end_col;        
extern  int                     stmt_end_line;      
extern  int                     stmt_label_idx;
extern  int                     stmt_start_col;        
extern  int                     stmt_start_line;      
extern  stmt_type_type          stmt_type;  
extern	char		       *stmt_type_str[];
extern  int                     storage_bit_kind_tbl[Num_Linear_Types];
extern  int                     storage_bit_size_tbl[Num_Linear_Types];
extern  int                     bit_size_tbl[Num_Linear_Types];
extern  int                     storage_bit_prec_tbl[Num_Linear_Types];
extern  int                     stride_mult_unit_in_bits[Num_Linear_Types];
extern	target_machine_type	target_machine;
extern  int			target_safevl;
extern	boolean			target_ieee;
extern	int			target_os;
extern	boolean			target_sv1;
extern	boolean			target_triton;
extern	boolean			target_t3e;
extern	long			true_value;
extern	int			where_ir_idx;
extern  int			where_dealloc_stmt_idx;
extern  int                     type_alignment_tbl[Num_Linear_Types];


/*********************************\
|* global enums and types for io *|
\*********************************/

enum  io_stmt_entry   {Backspace,
                       Close,
                       Endfile,
                       Inquire,
                       Open,
                       Read,
                       Rewind,
                       Write,
                       Print,
                       Decode,
                       Encode
                      };


enum exp_form_entry   {Exp_Form,
                       Format_Form,
                       Label_Form,
                       Namelist_Form,
                       Var_Only_Form
                      };

typedef enum		exp_form_entry		exp_form_type;
typedef enum		io_stmt_entry		io_stmt_type;

typedef struct		ciitem_entry		ciitem_entry_type;
typedef struct		ciitem_tbl_entry	ciitem_tbl_type;
typedef struct		cilist1			cilist1_type;

typedef char		const_opts_type[MAX_CONST_OPT_LENGTH];
typedef char		ciitem_name[MAX_CIITEM_NAME_LENGTH];


struct	ciitem_entry	{ciitem_name      name;
                         int              name_length;
                         exp_form_type    allowed_form;
                         int              num_types;
                         basic_type_type  allowed_types[MAX_NUM_ALLOWED_TYPES];
                         int              arg_position;
                         boolean          has_const_opts;
			 boolean	  scalar;
                         int              num_const_opts;
                         const_opts_type  const_opts[MAX_NUM_CONST_OPTS];
                        };

typedef ciitem_entry_type	ciitem_list_type[MAX_NUM_CIITEM];

struct	ciitem_tbl_entry{int			num_ciitems;
                         int			num_diff_ciitems;
                         int			num_without_kwd;
                         ciitem_list_type	ciitem_list;
                        };

struct	cilist1		{Uint			version :  8;
                         Uint			uflag   :  8;
                         Uint			eeeflag :  8;
                         Uint			dsflag  :  8;
                         Uint			fmt     :  8;
                         Uint			stksize :  8;
                         Uint			unused  :  8;
                         Uint			icount  :  8;
                	};


extern cif_usage_code_type      xref_state;

extern long			message_error_tbl[MAX_MSG_SIZE];
extern long			message_suppress_tbl[MAX_MSG_SIZE];
extern long			message_warning_tbl[MAX_MSG_SIZE];

extern boolean                  check_type_conversion;
extern int			target_type_idx;
extern int                      target_char_len_idx;
extern int			target_array_idx;

extern boolean			insert_subs_ok;

extern boolean			two_word_fcd;
extern boolean			char_len_in_bytes;

typedef struct f90_type {

    unsigned int	unused : 32;

    enum typecodes {
        DVTYPE_UNUSED      = 0,
        DVTYPE_TYPELESS    = 1,
        DVTYPE_INTEGER     = 2,
        DVTYPE_REAL        = 3,
        DVTYPE_COMPLEX     = 4,
        DVTYPE_LOGICAL     = 5,
        DVTYPE_ASCII       = 6,
        DVTYPE_DERIVEDBYTE = 7,
        DVTYPE_DERIVEDWORD = 8
    }                   type    :8;     /* type code */
    unsigned int        dpflag  :1;     /* set if declared double precision
                                         * or double complex */
    enum dec_codes {
        DVD_DEFAULT     = 0,            /* KIND= and *n absent, or
                                         * KIND=expression which evaluates to
                                         * the default KIND, ie.:
                                         *      KIND(0) for integer
                                         *      KIND(0.0) for real
                                         *      KIND((0,0)) for complex
                                         *      KIND(.TRUE.) for logical
                                         *      KIND('A') for character
                                         * across on all ANSI-conformant
                                         *  implementations. */
        DVD_KIND        = 1,            /* KIND=expression which does not
                                         * qualify to be DVD_DEFAULT or
                                         * DVD_KIND_CONST or DVD_KIND_DOUBLE */
        DVD_STAR        = 2,            /* *n is specified (example: REAL*8 */
        DVD_KIND_CONST  = 3,            /* KIND=expression constant across
                                         * all implementations. */
        DVD_KIND_DOUBLE = 4             /* KIND=expression which evaluates to
                                         * KIND(1.0D0) for real across all
                                         * implementations.  This code may be
                                         * passed for real or complex type.  */
    } kind_or_star              :3;     /* Set if KIND= or *n appears in the
                                         * variable declaration.  Values
                                         * are from enum dec_codes */
    unsigned int        int_len :12;    /* internal length in bits of iolist
                                         * entity. 8 for character data to
                                         * indicate size of each character */
    unsigned int        dec_len :8;     /* declared length in bytes for *n
                                         * or KIND value. Ignored if
                                         * kind_or_star==DVD_DEFAULT */
} f90_type_t;


/***********************************************************************\
|* ext_dope_type is used for folding array intrinsics. It defines      *|
|* our dope vector in C so we can create dope vectors for the folders. *|
\***********************************************************************/

/* OSP_467, #4, dynamic selection of ptr32 or ptr64 for TARG_X8664 */
# if defined(_DOPE_VECTOR_32_OR_64) || defined(TARG_X8664)
union ext_dope_entry {
		struct {
                        int       	base_addr;
                        int       	el_len;
                        unsigned int    assoc     :  1;
                        unsigned int    ptr_alloc :  1;
                        unsigned int    p_or_a    :  2;
                        unsigned int    a_contig  :  1;
                        unsigned int    unused_1  : 27;

                        unsigned int    unused_2  : 29;
                        unsigned int    num_dims  :  3;

# ifdef _TYPE_CODE_64_BIT
                        f90_type_t      type_code;
# else
                        unsigned int    unused_3  : 32;
                        unsigned int    type_code : 32;
# endif

                        int       	orig_base;
                        int       	orig_size;

                        struct  {
                                int       low_bound;
                                int       extent;
                                int       stride_mult;
                                }       dim[7];

                        int	unused_fill[25];

           		} ptr32;
		struct  {
                        long long       base_addr;
                        long long       el_len;
                        unsigned int    assoc     :  1;
                        unsigned int    ptr_alloc :  1;
                        unsigned int    p_or_a    :  2;
                        unsigned int    a_contig  :  1;
                        unsigned int    unused_1  : 27;

                        unsigned int    unused_2  : 29;
                        unsigned int    num_dims  :  3;

# ifdef _TYPE_CODE_64_BIT
                        f90_type_t      type_code;
# else
                        unsigned int    unused_3  : 32;
                        unsigned int    type_code : 32;
# endif

                        long long       orig_base;
                        long long       orig_size;

                        struct  {
                                long long       low_bound;
                                long long       extent;
                                long long       stride_mult;
                                }       dim[7];
           		} ptr64;
		};

typedef union ext_dope_entry	ext_dope_type;

# else

struct	ext_dope_entry	{
                	long_type    	base_addr;
                	long_type    	el_len;
# if defined(_TARGET64)
                	unsigned int    assoc     :  1;
                	unsigned int    ptr_alloc :  1;
                	unsigned int    p_or_a    :  2;
			unsigned int	a_contig  :  1;
                        unsigned int    unused_1  : 27;
                	unsigned int    unused_2  : 29;

                	unsigned int    num_dims  :  3;

# ifdef _TYPE_CODE_64_BIT
                	f90_type_t      type_code;
# else
                	unsigned int    unused_3  : 32;
                	unsigned int    type_code : 32;
# endif
# else 
                        unsigned int    assoc     :  1;
                        unsigned int    ptr_alloc :  1;
                        unsigned int    p_or_a    :  2;
			unsigned int	a_contig  :  1;
                        unsigned int    unused_1  : 24;

                        unsigned int    num_dims  :  3;

                        unsigned int    type_code : 32;
# endif

                	long_type            orig_base;
                	long_type            orig_size;

                        struct	{
				long_type	low_bound;
				long_type	extent;
				long_type	stride_mult;
				}	dim[7];
           };

typedef struct ext_dope_entry		ext_dope_type;

# endif

# if defined(_DOPE_VECTOR_32_OR_64)
struct int_dope_entry {
                        int             base_addr;
                        int             el_len;
                        unsigned int    assoc     :  1;
                        unsigned int    ptr_alloc :  1;
                        unsigned int    p_or_a    :  2;
                        unsigned int    a_contig  :  1;
                        unsigned int    unused_1  : 27;

                        unsigned int    unused_2  : 29;
                        unsigned int    num_dims  :  3;

# ifdef _TYPE_CODE_64_BIT
                        f90_type_t      type_code;
# else
                        unsigned int    unused_3  : 32;
                        unsigned int    type_code : 32;
# endif

                        int             orig_base;
                        int             orig_size;

                        struct  {
                                int       low_bound;
                                int       extent;
                                int       stride_mult;
                                }       dim[7];

			};

# else

struct  int_dope_entry  {
                        long_type       base_addr;
                        long_type       el_len;
# if defined(_TARGET64)
                        unsigned int    assoc     :  1;
                        unsigned int    ptr_alloc :  1;
                        unsigned int    p_or_a    :  2;
                        unsigned int    a_contig  :  1;
                        unsigned int    unused_1  : 27;
                        unsigned int    unused_2  : 29;

                        unsigned int    num_dims  :  3;

# ifdef _TYPE_CODE_64_BIT
                        f90_type_t      type_code;
# else
                        unsigned int    unused_3  : 32;
                        unsigned int    type_code : 32;
# endif
# else
                        unsigned int    assoc     :  1;
                        unsigned int    ptr_alloc :  1;
                        unsigned int    p_or_a    :  2;
                        unsigned int    a_contig  :  1;
                        unsigned int    unused_1  : 24;

                        unsigned int    num_dims  :  3;

                        unsigned int    type_code : 32;
# endif

                        long_type            orig_base;
                        long_type            orig_size;

                        struct  {
                                long_type       low_bound;
                                long_type       extent;
                                long_type       stride_mult;
                                }       dim[7];
           };
# endif

typedef struct int_dope_entry           int_dope_type;

/*  linear_type_type is enum type, in ia64 system, this type is 32 bit length.
# ifdef _HOST64  
struct exp_tbl_entry            {
                                boolean			ext  :  1;
                                linear_type_type	type : 63;
                                };
# elif _HOST32
*/
struct exp_tbl_entry            {
                                boolean			ext  :  1;
                                linear_type_type	type : 31;
                                };
//# endif

typedef struct exp_tbl_entry    exp_tbl_type;

/***********************************************************************\
|* Definitions for namlist io structures. Copied from namelist.h.      *|
\***********************************************************************/

/*
 *      One nmlist_goli describes one namelist group_object_list_item.
 */
typedef struct nmlist_goli {
    unsigned int        valtype :8;     /* type of namelist entry */
    unsigned int                :24;    /* pad for first 32 bits        */
# ifndef _BITFIELD_RIGHT_TO_LEFT
    unsigned int                :32;    /* pad for second 32-bits       */
# endif
} nmlist_goli_t;

/*
 *      nmlist_group is the structure of a namelist.   One namelist group
 *      is passed with the single call to a compiler-library interface
 *      routine.
 */

typedef struct {
    unsigned int        version :3;     /* contains NAMELIST_VERSION */
#if (defined(_TARGET64) || (defined(_TARGET_OS_IRIX) || defined(_TARGET_OS_LINUX)) || defined(_TARGET_OS_DARWIN)) && \
    ! defined(_BITFIELD_RIGHT_TO_LEFT)
    unsigned int                :29;    /* unused */
    unsigned int                :16;    /* unused */
#else
    unsigned int                :13;    /* unused */
#endif
    unsigned int        icount  :16;    /* Number of group_object_list_items */
                                        /* in the namelist. */
} nmlist_group_hdr;

/*
 *      A nmlist_struclist describes a structure namelist group_object_list.
 *      If the structure is a scalar, then a null pointer is the second
 *      word in the structure table.  If the structure is an array, then
 *      the address of the dope vector is in the second word.
 */

typedef struct nmlist_struclist {

#if (defined(_TARGET64) || (defined(_TARGET_OS_IRIX) || defined(_TARGET_OS_LINUX)) || defined(_TARGET_OS_DARWIN)) && \
    ! defined(_BITFIELD_RIGHT_TO_LEFT)
    unsigned int                  :32;  /* unused */
    unsigned int                  :16;  /* unused */
#else
    unsigned int                  :16;  /* unused */
#endif
    unsigned int        structlen :16;  /* number of entries in structure. */
} nmlist_struclist_t;



extern void    print_dv (int_dope_type *, boolean);

extern long    linear_to_arith[Num_Linear_Types];
extern boolean in_constructor;
extern boolean in_implied_do;
extern boolean parsing_kind_selector;
extern int     num_host_wds[Num_Linear_Types];
extern boolean directives_are_global;
extern boolean insert_global_directives;
extern int     curr_stmt_stk_il_idx;

extern long    argchck_suppress_msg[40];
extern int     num_argchck_suppress_msg;

# ifdef _USE_FOLD_DOT_f
#ifdef KEY /* Bug 5554 */
extern boolean kludge_input_conversion (char *, int, boolean);
#else /* KEY Bug 5554 */
extern void kludge_input_conversion (char *, int);
#endif /* KEY Bug 5554 */
extern void kludge_output_conversion (long_type *, int, char *);
# endif
#ifdef KEY /* Bug 5089 */
extern  boolean		special_case_fcn_to_sub(int spec_idx);
extern char *init_msg_processing (char *[]);
extern void process_cmd_line (int, char *[], char *);
#endif /* KEY Bug 5089 */
#ifdef KEY /* Bug 14150 */
extern boolean c_ptr_abi_trouble(int);
extern boolean is_x8664_n32();
#endif /* KEY Bug 14150 */

#ifdef KEY /* Bug 3018 */
/* Suffix used in intrin_tbl to indicate G77 subroutine versions of intrinsics
 * which are normally functions. If you change this,
 * p_driver.c:enter_intrinsic may also need to change.
 */
#               define INTRIN_SUBR_SUFFIX ":Subroutine"
#endif /* KEY Bug 3018 */

