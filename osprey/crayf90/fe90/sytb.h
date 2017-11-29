/*
 * Copyright (C) 2010 Advanced Micro Devices, Inc.  All Rights Reserved.
 */
/*
 * Copyright (C) 2008. PathScale, LLC. All Rights Reserved.
 */
/*
 *  Copyright (C) 2006, 2007. QLogic Corporation. All Rights Reserved.
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



/* USMID:  "\n@(#)5.0_pl/headers/sytb.h	5.12	10/07/99 12:46:27\n" */

/***************************************\
|* globally accessible type specifiers *|
\***************************************/

enum	access_values		{Public,		Private };

enum	align_values		{No_Align,	
				 Align_Bit,
				 Align_8,
				 Align_16,
				 Align_32,
				 Align_64,
				 Align_Double,
				 Align_128 };

enum	atd_class_values	{Atd_Unknown,		Variable,
				 Dummy_Argument,	Function_Result,
				 Compiler_Tmp,		CRI__Pointee,
				 Struct_Component,	Constant };

enum	atl_class_values	{Lbl_Unknown,		Lbl_User,
				 Lbl_Format,		Lbl_Internal,
				 Lbl_Debug,		Lbl_Construct };

/*      This is set up to match the Interface calls */

enum	atl_debug_class_values {Ldbg_None,		Ldbg_Stmt_Lbl,
				Ldbg_Exit_Point,	Ldbg_End_Prologue,
				Ldbg_Start_Epilogue,	Ldbg_Construct_Name,
				Ldbg_Loop_Lbl,		Ldbg_User_Lbl };

/*      This is set up to match the Interface calls */

enum	atp_proc_values		{Unknown_Proc,		Extern_Proc,
				 Intrin_Proc,		Dummy_Proc,
				 Intern_Proc,		Imported_Proc,
                                 Module_Proc,		Intern_Proc_Refd=2};

enum	bd_array_size_values	{Unknown_Size,		Constant_Size,
				 Var_Len_Array,		Symbolic_Constant_Size};

enum	bd_array_values		{Unknown_Array,		Explicit_Shape,
				 Assumed_Size,		Deferred_Shape,
				 Assumed_Shape };

enum	directive_label_values	{Safevl_Dir_Idx,
				 Unroll_Dir_Idx,
				 Maxcpus_Dir_Idx,
				 Mark_Dir_Idx,
				 Cache_Bypass_Dir_Idx,
				 Concurrent_Dir_Idx,
				 Blockable_Dir_Idx,
				 Interchange_Dir_Idx,
				 Interchange_Level_Dir_Idx,
				 Num_Dir_On_List };

enum	distribution_values	{No_Distribution,
				 Block_Distribution,
				 Cyclic_Distribution,
				 Star_Distribution };

enum	file_path_values	{Unknown_Fp,
				 Directory_Fp,	
				 File_Fp,
				 Mod_File_Fp,
				 Archive_File_Fp,	
				 Elf_File_Fp,	
				 Module_Fp,	
				 Current_Compile_Fp,	
				 Include_Fp,
				 Inline_Fp,
				 Reshape_Array_Fp,
				 Xcoff_File_Fp };	

enum	implicit_storage_values	{Impl_Default_Storage,
				 Impl_Automatic_Storage,	
				 Impl_Static_Storage };

enum	intent_values		{Intent_Unseen,		Intent_In,
				 Intent_Out,		Intent_Inout };

enum	interface_values	{Generic_Unknown_Interface,
                                 Generic_Function_Interface,
                                 Generic_Subroutine_Interface,
                                 Defined_Interface,
                                 Defined_Assign_Interface,
                                 Defined_Unary_Interface,
                                 Defined_Binary_Interface,
                                 Defined_Unary_Or_Binary_Interface };


enum	obj_class_values	{Data_Obj,		Pgm_Unit,
				 Label,			Derived_Type,
				 Interface,		Namelist_Grp,
				 Stmt_Func,		Common_Block=Stmt_Func};

/*      This is set up to match the Interface calls.             */
/*      This can't go beyond 7, because it will blow field size. */

enum	pgm_unit_values		{Pgm_Unknown=0,		Function=3,
				 Subroutine=4,		Program=5,
				 Blockdata=6,		Module=7 };

enum	reference_values	{Not_Referenced,	Char_Rslt_Bound_Ref,
				 Dcl_Bound_Ref,		Referenced };

enum	sb_name_values		{What_Blk,
				 Based_Blk,
				 Data_Init_Blk,
				 Pointee_Blk,
				 Stack_Host_Blk,
				 Data_Blk,
				 Sym_Blk,
				 Data_Uninit_Blk,
				 Stack_Blk,
				 Dargs_Blk,
				 End_Name_Blk};


enum    sb_type_values		{Unknown_Seg,		Static,
                                 Stack,			Formal,
                                 Common,		Extern,
                                 Exported,		Task_Common,
                                 Soft_External,		Global_Breg,
                                 Global_Treg,		Static_Named,
                                 Based,			Equivalenced,
                                 Restricted,		Distributed,
                                 LM_Static,		LM_Common,
                                 LM_Extern,		Auxiliary,
                                 Static_Local,		Non_Local_Stack,
				 Non_Local_Formal,	Hosted_Stack,
				 Threadprivate,		Coment };

enum	type_char_values	{Unknown_Char,		Const_Len_Char,
				 Var_Len_Char,		Assumed_Size_Char };

enum	type_desc_values	{Default_Typed,		Star_Typed,
				 Kind_Typed };

enum	use_type_values		{Use_Not,		Use_All,
				 Use_Renamed,		Use_Only };

enum	holler_type_values	{Not_Hollerith,		H_Hollerith,
				 L_Hollerith,		R_Hollerith };


typedef	enum	access_values		access_type;
typedef	enum	align_values		align_type;
typedef	enum	atd_class_values	atd_class_type;
typedef enum	atl_class_values	atl_class_type;
typedef enum	atl_debug_class_values	atl_debug_class_type;
typedef	enum	atp_proc_values		atp_proc_type;
typedef enum	bd_array_values		bd_array_type;
typedef enum	bd_array_size_values	bd_array_size_type;
typedef enum	directive_label_values	directive_label_type;
typedef enum	distribution_values	distribution_type;
typedef enum	implicit_storage_values	implicit_storage_type;
typedef enum	intent_values		intent_type;
typedef	enum	interface_values	interface_type;
typedef	enum	obj_class_values	obj_class_type;
typedef enum	pgm_unit_values		pgm_unit_type;
typedef enum	reference_values	reference_type;
typedef enum	sb_type_values		sb_type_type;
typedef enum	type_char_values	type_char_type;
typedef	enum	type_desc_values	type_desc_type;
typedef	enum	use_type_values		use_type_type;
typedef enum    holler_type_values	holler_type;

typedef 	long_type		const_pool_type;

typedef union	attr_tbl_entry		attr_tbl_type;
typedef union	attr_aux_tbl_entry	attr_aux_tbl_type;
typedef union	bounds_tbl_entry	bounds_tbl_type;
typedef	union	global_attr_tbl_entry	global_attr_tbl_type;
typedef union	global_bounds_tbl_entry	global_bounds_tbl_type;
typedef	union	ir_list_tbl_entry	ir_list_tbl_type;
typedef	union	ir_list_tbl_entry	global_ir_list_tbl_type;
typedef	union	name_pool_entry		name_pool_type;
typedef union	scp_wd_entry		scp_wd_type;
typedef	union	global_type_tbl_entry	global_type_tbl_type;
typedef	union	type_tbl_entry		type_tbl_type;

typedef struct	attr_list_tbl_entry	attr_list_tbl_type;

typedef struct	const_search_tbl_entry	const_search_tbl_type;
typedef struct  cs_idx_tbl_entry        cs_idx_tbl_type;

typedef struct	const_tbl_entry		const_tbl_type;
typedef struct	new_const_tbl_entry	new_const_tbl_type;
typedef struct	old_const_tbl_entry	old_const_tbl_type;
typedef struct	equiv_tbl_entry		equiv_tbl_type;
typedef	struct	file_path_tbl_entry	file_path_tbl_type;
typedef	struct	global_line_tbl_entry	global_line_tbl_type;
typedef struct	intrin_tbl_entry	intrin_tbl_type;
typedef struct	intrin_map_entry	intrin_map_type;
typedef	struct	ir_list_link_entry	ir_list_link_type;
typedef	struct	ir_opr_entry		ir_opr_type;
typedef	struct	ir_tbl_entry		ir_tbl_type;
typedef	struct	old_ir_tbl_entry	old_ir_tbl_type;
typedef	struct	old_ir_opr_entry	old_ir_opr_type;
typedef	struct	ir_tbl_entry		global_ir_tbl_type;
typedef struct	name_tbl_entry		name_tbl_type;
typedef	struct	mod_link_tbl_entry	mod_link_tbl_type;
typedef	struct	pdg_link_tbl_entry	pdg_link_tbl_type;
typedef	struct	pdt_tbl_hdr_entry	pdt_tbl_hdr_type;
typedef	struct	rename_only_entry	rename_only_tbl_type;
typedef struct	scp_tbl_entry		scp_tbl_type;
typedef struct	sec_name_tbl_entry	sec_name_tbl_type;
typedef	struct	sh_tbl_entry		sh_tbl_type;
typedef	struct	size_offset_entry	size_offset_type;
typedef	struct	sh_tbl_entry		global_sh_tbl_type;
typedef union	stor_blk_tbl_entry	stor_blk_tbl_type;

/* Needed for table alloc macros */

typedef struct	name_tbl_entry		loc_name_tbl_type;
typedef struct	name_tbl_entry		global_name_tbl_type;
typedef struct	name_tbl_entry		hidden_name_tbl_type;
typedef	union	name_pool_entry		str_pool_type;

/***************************\
|*  Integer Constant entry *|
\***************************/

struct	size_offset_entry	{Uint		type_idx	: 16;
				 Uint		unused		: 12;
				 fld_type	fld		:  4;
				 Uint		idx		: 32;
				 long_type  constant[MAX_WORDS_FOR_INTEGER];
				};

/**************************************************\
|* PDT definition - must match PDT used by segldr *|
\**************************************************/

struct	pdt_tbl_hdr_entry	{Uint			hdr_type	:  7;
				 Uint			UNUSED		:  9;
				 Uint			hdr_bi		: 16;
				 Uint			hdr_len		: 32;
				};


/******************\
|* ATTR AUX TABLE *|
\******************/

union	attr_aux_tbl_entry {
			   struct {
			        Uint			def_line	: 24;
			        Uint			def_column	:  8;
				boolean			cif_use_in_bnd	:  1;
				boolean			cif_done	:  1;
				boolean			semantics_done	:  1;
				boolean			arg_to_kind	:  1;
				boolean			access_set	:  1;
				boolean			locked_in	:  1;
				boolean			cif_usage_rec	:  1;
				boolean			unused		:  1;
			        Uint			cif_sym_id	: 24;

				Uint			field1		: 16;
				Uint			field2		: 16;
				boolean			flag1		:  1;
				boolean			flag2		:  1;
				boolean			flag3		:  1;
				boolean			flag4		:  1;
				boolean			flag5		:  1;
				boolean			flag6		:  1;
				boolean			flag7		:  1;
				boolean			flag8		:  1;
				Uint			field3		: 24;

			        Uint			field4		: 24;
				boolean			flag9		:  1;
				boolean			flag10		:  1;
				boolean			flag11		:  1;
				boolean			flag12		:  1;
				boolean			flag13		:  1;
				boolean			flag14		:  1;
				boolean			flag15		:  1;
				boolean			flag16		:  1;
				boolean			flag17		:  1;
				boolean			flag18		:  1;
				boolean			flag19		:  1;
				boolean			flag20		:  1;
				boolean			flag21		:  1;
				boolean			flag22		:  1;
				boolean			flag23		:  1;
				boolean			flag24		:  1;
			        Uint			field5		: 24;

			       } fld;
			   long			wd[NUM_AA_WDS];
			};


/*******************\
|* ATTR LIST TABLE *|
\*******************/

struct	attr_list_tbl_entry    {Uint			next_idx	: 16;
				Uint			prev_idx	: 16;
				boolean			flag1		:  1;
				boolean			flag2		:  1;
				Uint			unused		:  6;
				Uint			attr_idx	: 24;
			       };


/*******************\
|* ATTRIBUTE TABLE *|
\*******************/

union	attr_tbl_entry	{
			 struct	{
				 Uint			object_class	:  4;
				 Uint			module_idx	: 20;
				 Uint		 	referenced	:  2;
				 boolean	 	defined		:  1;
				 boolean	 	passed_as_arg	:  1;
				 boolean		optional	:  1;
				 boolean	 	private_access	:  1;
				 boolean		use_associated	:  1;
				 boolean		host_associated	:  1;

				 boolean		namelist_obj	:  1;
				 boolean		dcl_err		:  1;
				 boolean		is_darg		:  1;
				 boolean		alt_darg	:  1;
				 boolean	 	elemental_intrin:  1;
				 boolean		is_intrin       :  1;
				 boolean		typed		:  1;
				 boolean		def_in_child	:  1;
				 boolean		ref_in_child	:  1;
				 boolean		compiler_gend	:  1;
				 boolean		not_visible	:  1;
				 boolean		ignore_attr_link:  1;
				 Uint			attr_link	: 20;

				 Uint			orig_name_idx	: 24;
				 Uint			orig_name_len	:  8;
				 Uint			length		:  8;
				 Uint			name_idx	: 24;

				 Uint			orig_module_idx	: 20;
				 boolean		module_object	:  1;
                                 boolean                flag57          :  1;
                                 boolean                flag58          :  1;
				 Uint			unused		:  9;
				 Uint			unused1		:  8;
				 Uint			field7		: 24;

				 Uint			secondary_info	:  3;
				 Uint		 	field2		:  3;
				 Uint			field3		:  2;
				 Uint			field4		: 24;

				 boolean	 	flag1		:  1;
				 boolean		flag2		:  1;
				 boolean		flag3		:  1;
				 boolean		flag4		:  1;
				 boolean		flag5		:  1;
				 boolean		flag6		:  1;
				 boolean		flag7		:  1;
				 boolean		flag8		:  1;
				 boolean		flag9		:  1;
				 boolean		flag10		:  1;
				 boolean		flag11		:  1;
				 boolean		flag12		:  1;
				 Uint			field1		: 20;

				 Uint		 	field5		: 16;
				 Uint		 	field6		: 16;

				 boolean	 	flag17 		:  1;
				 boolean	 	flag18	 	:  1;
				 boolean	 	flag19	 	:  1;
				 boolean	 	flag20	 	:  1;
				 boolean	 	flag21 		:  1;
				 boolean	 	flag22	 	:  1;
				 boolean		flag23		:  1;
				 boolean		flag24		:  1;
				 boolean	 	flag25 		:  1;
				 boolean	 	flag26	 	:  1;
				 boolean		flag27		:  1;
				 boolean	 	flag28		:  1;
				 Uint		 	field8		: 20;

				 boolean	 	flag29		:  1;
				 boolean	 	flag30		:  1;
				 boolean	 	flag31		:  1;
				 boolean	 	flag32		:  1;
				 boolean	 	flag33		:  1;
				 boolean	 	flag34		:  1;
				 boolean	 	flag35		:  1;
				 boolean	 	flag36		:  1;
				 Uint			field10		: 24;
				 Uint		 	field12		:  8;
				 Uint			field13		: 24;

				 Uint		 	field14		: 24;
				 boolean	 	flag49		:  1;
				 boolean	 	flag50		:  1;
				 boolean	 	flag51		:  1;
				 boolean	 	flag52		:  1;
				 boolean	 	flag53		:  1;
				 boolean	 	flag54		:  1;
				 boolean	 	flag55		:  1;
				 boolean	 	flag56		:  1;

				 boolean	 	flag37		:  1;
				 boolean	 	flag38		:  1;
				 boolean	 	flag39		:  1;
				 boolean	 	flag40		:  1;
				 boolean	 	flag41		:  1;
				 boolean	 	flag42		:  1;
				 boolean	 	flag43		:  1;
				 boolean	 	flag44		:  1;
				 boolean	 	flag45		:  1;
				 boolean	 	flag46		:  1;
				 boolean	 	flag47		:  1;
				 boolean	 	flag48		:  1;
				 Uint			field16		: 20;

				} fld;

                         /* This is used to specify a 32 bit alternative for */
                         /* one of the 64 bit words.                          */

			 struct	{Uint			field32_1 	: 32;
			         Uint			field32_2 	: 32;
			         Uint			field32_3 	: 32;
			         Uint			field32_4 	: 32;
			         Uint			field32_5 	: 32;
			         Uint			field32_6 	: 32;
			         Uint			field32_7 	: 32;
			         Uint			field32_8 	: 32;
			         Uint			field32_9 	: 32;
			         Uint			field32_10 	: 32;
			         Uint			field32_11 	: 32;
			         Uint			field32_12	: 32;
			         Uint			field32_13	: 32;
			         Uint			field32_14	: 32;
                                } fldd;

                         /* This is used to specify fld_type, so that we can */
                         /* get good error checking to prevent indexes being */
                         /* assigned to the enum field.                      */

			 struct	{Uint			field32_1 	: 32;
			         Uint			field32_2 	: 32;
			         Uint			field32_3 	: 32;
			         Uint			field32_4 	: 32;
			         Uint			field32_5 	: 32;
			         Uint			field32_6 	: 32;
			         Uint			field32_7 	: 32;
			         Uint			field32_8 	: 32;
			         Uint			field23 	: 24;
			         Uint			UNUSED	 	:  8;
			         Uint			field32_10 	: 32;
				 Uint			field32_11	: 32;
				 fld_type		field22		:  8;
			         Uint			field32_12	: 24;
			         Uint			distribution_idx: 20;
			         Uint			alignment	:  4;
			         Uint			field32_13_2	:  8;
			         Uint			field32_14	: 32;
                                } fld2;


                        /* This is what the attr table looked like in release */
                        /* 3.0.  It can be removed in release 5.0.            */

			struct	{
				 Uint					: 32;
				 boolean		not_visible	:  1;
				 boolean		use_associated	:  1;
				 boolean		host_associated	:  1;
				 boolean		cif_use_in_bnd	:  1;
				 boolean		cif_done	:  1;
				 boolean		module_object	:  1;
				 boolean		arg_to_kind	:  1;
				 Uint			unused		:  2;
				 Uint			module_idx	: 20;
				 Uint			object_class	:  3;

				 Uint			unused1		: 32;
				 Uint		 	unused2		: 32;

				 Uint			orig_name_idx	: 24;
				 Uint			orig_name_len	:  8;
				 Uint			length		:  8;
				 Uint			name_idx	: 24;

				 Uint			secondary_info	:  3;
				 Uint		 	field2		:  3;
				 Uint			field3		:  2;
				 Uint			field4		: 24;

				 boolean	 	flag1		:  1;
				 boolean		flag2		:  1;
				 boolean		flag3		:  1;
				 boolean		flag4		:  1;
				 boolean		flag5		:  1;
				 boolean		flag6		:  1;
				 boolean		flag7		:  1;
				 boolean		flag8		:  1;
				 boolean		flag9		:  1;
				 boolean		flag10		:  1;
				 boolean		flag11		:  1;
				 boolean		flag12		:  1;
				 boolean		flag13		:  1;
				 boolean	 	flag14 		:  1;
				 boolean	 	flag15	 	:  1;
				 boolean	 	flag16	 	:  1;
				 Uint			field1		: 16;

				 Uint		 	field5		: 16;
				 Uint		 	field6		: 16;

				 boolean	 	flag17 		:  1;
				 boolean	 	flag18	 	:  1;
				 boolean	 	flag19	 	:  1;
				 boolean	 	flag20	 	:  1;
				 boolean	 	flag21 		:  1;
				 boolean	 	flag22	 	:  1;
				 boolean		flag23		:  1;
				 boolean		flag24		:  1;
				 boolean	 	flag25 		:  1;
				 boolean	 	flag26	 	:  1;
				 boolean		flag27		:  1;
				 boolean	 	flag28		:  1;
				 Uint		 	field8		: 20;

				 boolean	 	flag29		:  1;
				 boolean	 	flag30		:  1;
				 boolean	 	flag31		:  1;
				 boolean	 	flag32		:  1;
				 boolean	 	flag33		:  1;
				 boolean	 	flag34		:  1;
				 boolean	 	flag35		:  1;
				 boolean	 	flag36		:  1;
				 Uint			field10		: 24;
				 Uint		 	field12		:  8;
				 Uint			field13		: 24;

				 Uint		 	field14		: 16;
				 Uint		 	field15		: 16;
				 boolean	 	flag37		:  1;
				 boolean	 	flag38		:  1;
				 boolean	 	flag39		:  1;
				 boolean	 	flag40		:  1;
				 boolean	 	flag41		:  1;
				 boolean	 	flag42		:  1;
				 boolean	 	flag43		:  1;
				 boolean	 	flag44		:  1;
				 boolean	 	flag45		:  1;
				 boolean	 	flag46		:  1;
				 boolean	 	flag47		:  1;
				 boolean	 	flag48		:  1;
				 Uint			field16		: 20;
				} old;

			 long			wd[NUM_AT_WDS];
			};


/***************\
|* ARRAY TABLE *|
\***************/


union	bounds_tbl_entry {			/* TOTAL 2 64 bit words */

			struct	{Uint		rank		:  8;
				 Uint		array_class	:  3;
			 	 boolean	error		:  1;
				 Uint		global_idx	: 20;

			 	 Uint		len_idx		: 24;
			 	 fld_type	len_fld		:  4;
			 	 boolean	resolved	:  1;
			 	 Uint		array_size	:  3;

				 Uint		line_num	: 24;
				 Uint		column_num	:  8;

			 	 Uint		next_free_ntry	: 20;
				 Uint		UNUSED2		:  2;
				 Uint		flow_dep	:  1;
			 	 Uint		used_ntry	:  1;
			 	 Uint		ntry_size	:  6;
				 boolean	dist_reshape	:  1;
				 boolean	dist_ntry	:  1;
				 } hdr;

			struct	{Uint		rank		:  4;
			 	 fld_type	len_fld		:  4;
			 	 Uint		len_idx		: 24;

			 	 Uint		next_free_ntry	: 16;
			 	 Uint		ntry_size	:  4;
			 	 Uint		used_ntry	:  1;
			 	 boolean	error		:  1;
			 	 boolean	resolved	:  1;
			 	 Uint		array_size	:  3;
				 Uint		array_class	:  3;
			 	 Uint		UNUSED2		:  3;

				 Uint		line_num	: 24;
				 Uint		column_num	:  8;
				 Uint		UNUSED3		: 30;
				 boolean	dist_reshape	:  1;
				 boolean	dist_ntry	:  1;
				 } old_hdr;

			struct	{fld_type	lb_fld		:  4;
				 Uint		UNUSED1		:  4;
				 Uint		lb_idx		: 24;
				 Uint		ub_idx		: 24;
				 Uint		UNUSED2		:  4;
				 fld_type	ub_fld		:  4;

				 fld_type	sm_fld		:  4;
				 Uint		UNUSED3		:  4;
				 Uint		sm_idx		: 24;
				 Uint		xt_idx		: 24;
				 Uint		UNUSED4		:  4;
				 fld_type	xt_fld		:  4;

				 } dim;

			struct	{fld_type	cyclic_fld	:  4;
				 distribution_type distribution	:  4;
				 Uint		cyclic_idx	: 24;
				 Uint		onto_idx	: 24;
				 Uint		UNUSED2		:  4;
				 fld_type	onto_fld	:  4;

				 Uint		UNUSED3		: 32;
				 Uint		UNUSED4		: 32;

				 } dist;

			struct	{
# if defined(_HOST32)
				 long long	lb;
				 long long	ub;
# else
				 long		lb;
				 long		ub;
# endif
				 } wd;
			long			len[NUM_BD_WDS];	
			};

/*******************\
|* CONSTANT TABLES *|
\*******************/

/* The const_tbl_idx only needs to be 24 bits, so the other 8 bits could be   */
/* used for flags.				                              */

struct	const_search_tbl_entry	{ Uint		const_tbl_idx	: 32;
				  Uint		UNUSED    	: 32;
				};

struct  cs_idx_tbl_entry {
                           Uint         bottom_idx     : 32;
                           Uint         top_idx        : 32;
                           Uint         last_entry_idx : 32;
                           Uint         curr_inc_size  : 32;
                         };


struct	const_tbl_entry		{Uint		const_pool_idx	: 32;
				 Uint		UNUSED1		:  7;
				 boolean	extra_zero_word :  1;
				 boolean	hollerith_endian:  1;
				 signed int	balance_factor 	:  2;
				 holler_type	hollerith_fld	:  3;
                                 boolean        boolean_constant:  1;
				 boolean	boz_constant	:  1;
				 Uint		type_idx	: 16;
				 Uint		left_child	: 32;
				 Uint		right_child	: 32;
				};

/* the following types are used for module translations. */

struct  old_const_tbl_entry     {Uint           const_pool_idx  : 32;
                                 Uint           UNUSED1         :  7;
                                 boolean        extra_zero_word :  1;
                                 boolean        hollerith_endian:  1;
                                 Uint           UNUSED2         :  2;
                                 holler_type    hollerith_fld   :  3;
                                 boolean        boolean_constant:  1;
                                 boolean        boz_constant    :  1;
                                 Uint           type_idx        : 16;
                                };

struct  new_const_tbl_entry     {Uint           const_pool_idx  : 32;
                                 Uint           UNUSED1         :  7;
                                 boolean        extra_zero_word :  1;
                                 boolean        hollerith_endian:  1;
                                 int            balance_factor  :  2;
                                 holler_type    hollerith_fld   :  3;
                                 boolean        boolean_constant:  1;
                                 boolean        boz_constant    :  1;
                                 Uint           type_idx        : 16;
                                 Uint           left_child      : 32;
                                 Uint           right_child     : 32;
                                };


/************************\
|* EQUIV TABLE 		*|
\************************/

struct  equiv_tbl_entry         {Uint           next_equiv_grp  : 16;
                                 Uint           next_equiv_obj  : 16;

                                 Uint           substring       :  1;
                                 Uint           dalign_me       :  1;
                                 Uint           dalign_shift    :  1;
                                 Uint           do_not_dalign   :  1;
                                 Uint           search_done     :  1;
                                 Uint           merged	        :  1;
                                 Uint           semantics_done  :  1;
                                 Uint           error	        :  1;
                                 Uint           list_idx	: 24;

                                 Uint           line_num        : 24;
                                 Uint           column_num      :  8;

                                 fld_type       opnd_fld        :  8;
                                 fld_type       fld             :  4;
                                 Uint           attr_idx        : 20;

                                 Uint           grp_end_idx	: 16;
                                 Uint           grp_idx		: 16;
                                 Uint           offset_idx	: 32;

				 Uint		opnd_idx	: 24;
				 Uint		unused2		:  8;
				 Uint		unused3		: 32;
                                };

/*******************\
|* FILE PATH TABLE *|
\*******************/

struct	file_path_tbl_entry	{Uint		name_idx	: 24;
				 Uint		file_class	:  4;
				 boolean	tmp_file   	:  1;
				 boolean	output_to_o   	:  1;
				 boolean	system_file	:  1;
				 boolean	srch_the_file	:  1;	

				 Uint		cif_id		: 16;
				 Uint		name_len	: 16;	

				 Uint		file_idx	: 16;
				 Uint		module_inline_idx: 16;

				 Uint		next_file_idx	: 16;
				 Uint		next_idx	: 16;

# if defined(_HOST32) && defined(_TARGET32)
				 Uint		unused3		: 32;
# endif
				 long_type	offset;
				};
 

/*********************\
|* GLOBAL ATTR TABLE *|
\*********************/

union	global_attr_tbl_entry	{

			 struct	{Uint			def_column	:  8;
				 Uint			def_line	: 24;

				 boolean		defined		:  1;
				 boolean		use_associated	:  1;
				 boolean		optional	:  1;
				 boolean		compiler_gend	:  1;
				 boolean	 	referenced	:  1;
				 boolean		flag01		:  1;
				 boolean		flag02		:  1;
				 boolean		flag03		:  1;
				 Uint			module_idx	: 20;
				 Uint			object_class	:  4;

				 Uint			orig_name_idx	: 24;
				 Uint			orig_name_len	:  8;
				 Uint			length		:  8;
				 Uint			name_idx	: 24;

				 Uint			field1		: 20;
				 Uint		 	field2		:  4;
				 boolean	 	flag1		:  1;
				 boolean		flag2		:  1;
				 boolean		flag3		:  1;
				 boolean		flag4		:  1;
				 boolean		flag5		:  1;
				 boolean		flag6		:  1;
				 boolean		flag7		:  1;
				 boolean		flag8		:  1;
				 boolean		flag9		:  1;
				 boolean		flag10		:  1;
				 boolean		flag11		:  1;
				 boolean		flag12		:  1;
				 Uint			field3		: 20;
				 Uint			field4		:  4;
				 Uint			field5		:  4;

				 Uint		 	field6		: 16;
				 Uint		 	field7		: 16;
				 Uint		 	field8		: 32;
#ifdef KEY /* Bug 14150 */
				 const char		*binding_label;
#endif /* KEY Bug 14150 */

				} fld;

			 struct	{Uint			field32_1 	: 32;
			         Uint			field32_2 	: 32;
			         Uint			field32_3 	: 32;
			         Uint			field32_4 	: 32;
			         Uint			field32_5 	: 32;
			         Uint			field32_5a	: 24;
			         Uint			field32_6 	:  8;

				 long_type	length[MAX_WORDS_FOR_INTEGER];
                                } wd;
			};


/**********************\
|* GLOBAL ARRAY TABLE *|
\**********************/

union	global_bounds_tbl_entry {

			struct	{Uint		rank		:  8;
			 	 Uint		array_size	:  8;
				 Uint		array_class	:  8;
			 	 Uint		unused1		:  8;
				 Uint		unused2		:  8;
			 	 Uint		unused3		: 24;
				 } hdr;

			struct	{Uint		lb_type		: 32;
			 	 Uint		ub_type		: 32;
				 } type;


			long_type		len[MAX_WORDS_FOR_INTEGER];
			};

/*********************\
|* GLOBAL LINE TABLE *|
\*********************/

struct  global_line_tbl_entry	{Uint           global_line     : 32;
                                 Uint           file_line       : 32;

                                 Uint           file_name_idx   : 32;
                                 Uint           unused		: 16;
                                 Uint           file_name_len   : 16;
				 Uint		incld_file_line : 32;
				 Uint		incld_file_col  : 32;

                                 Uint           path_name_idx   : 32;
                                 Uint           path_name_len   : 32;

				 Uint		source_lines	: 32;
                                 Uint           unused1		:  8;
				 Uint		cif_file_id	: 24;
                                };
 
 
/*********************\
|* GLOBAL NAME TABLE *|
\*********************/

struct	global_name_tbl_entry	{Uint		name_len	: 12;
                                 Uint		attr_idx	: 20;
                                 boolean	UNUSED		:  8;
				 Uint		name_idx	: 24;
                                };
 
/*****************\
|* IR LIST TABLE *|
\*****************/

struct ir_list_link_entry	{Uint                   prev_idx        : 24;
                                 boolean		flag_1		:  1;
                                 boolean		flag_2		:  1;
                                 boolean		flag_3		:  1;
                                 boolean		flag_4		:  1;
                                 boolean		flag_5		:  1;
				 Uint			for_ref		:  3;

                                 Uint                   unused          :  4;
                                 Uint                   flag_8          :  1;
				 boolean		flag_6          :  1;
				 boolean		flag_7          :  1;
				 boolean		arg_desc        :  1;
                                 Uint                   nxt_idx         : 24;
				};


union	ir_list_tbl_entry{struct 
				{ir_list_link_type	link;	/* word 1 */
                                 opnd_type 		op;	/* word 2 */
                                } il;
                          long		words[NUM_IL_WDS];
			  struct
				{long64			lwd1;
				 long64			lwd2;
			 	} il_long64; 
                         };


/************\
|* IR TABLE *|
\************/

struct	old_ir_opr_entry	{operator_type	the_operator	: 10;
                                 Uint		dim		:  3;
                                 Uint		rank		:  3;
                                 Uint		type_idx	: 16;
                                 Uint		line_num	: 24;
                                 Uint		col_num		:  8;
				};


struct	ir_opr_entry		{operator_type	the_operator	: 16;
                                 Uint		type_idx	: 16;
                                 Uint		line_num	: 24;
                                 Uint		col_num		:  8;
                                 Uint		rank		:  8;
                                 Uint		dim		:  8;
				 Uint		UNUSED1		: 16;
#ifdef KEY /* Bug 6845 */
				 Uint		n_alloc_cpnt	: 32;
#else /* KEY Bug 6845 */
				 Uint		UNUSED2		: 32;
#endif /* KEY Bug 6845 */
				};


struct	old_ir_tbl_entry	{old_ir_opr_type opr;		/* word 1 */
                                 opnd_type	op_l;		/* word 2 */
                                 opnd_type	op_r;		/* word 3 */
                                };

struct	ir_tbl_entry		{ir_opr_type	opr;		/* word 1 */
                                 opnd_type	op_l;		/* word 2 */
                                 opnd_type	op_r;		/* word 3 */
                                };
 
/********************\
|* LOCAL NAME TABLE *|
\********************/

struct	name_tbl_entry 		{Uint		attr_idx 	: 24;
				 Uint		name_len 	:  8;
				 boolean	in_only_list	:  1;
				 boolean	renamed		:  1;
				 boolean	def_locally 	:  1;
				 boolean	new_name	:  1;
				 Uint		UNUSED1		:  4;
				 Uint		name_idx 	: 24;
				};


/******************\
|* MOD LINK TABLE *|
\******************/

struct	mod_link_tbl_entry 	{Uint		at_idx 		: 32;

				 boolean	at_compressed	:  1;
				 boolean	at_ln_name	:  1;
				 boolean	at_searched	:  1;
				 boolean	at_search_me	:  1;
				 boolean	at_keep_me	:  1;
				 boolean	cp_keep_me	:  1;
				 boolean	cp_dalign_me	:  1;
				 boolean	ln_keep_me	:  1;
				 Uint		ln_idx	 	: 24;

				 Uint		cp_idx	 	: 32;
				 Uint		cp_len	 	: 32;

				 Uint		cn_idx 		: 32;
				 Uint		np_len 		:  8;
				 Uint		np_idx 		: 24;

				 Uint		il_idx	 	: 32;
				 boolean	il_keep_me	:  1;
				 boolean	cn_keep_me	:  1;
				 boolean	np_keep_me	:  1;
				 Uint		unused1 	:  4;
				 boolean	ir_keep_me	:  1;
				 Uint		ir_idx 		: 24;

				 Uint		sh_idx	 	: 32;
				 boolean	sh_keep_me	:  1;
				 Uint		unused2 	:  3;
				 boolean	bd_keep_me	:  1;
				 boolean	sb_keep_me	:  1;
				 boolean	typ_keep_me	:  1;
				 boolean	sn_keep_me	:  1;
				 Uint		sn_idx		: 24;

				 Uint		typ_idx	 	: 16;
				 Uint		sb_idx		: 16;
				 Uint		bd_idx	 	: 32;
				};


/*******************\
|* NAME POOL TABLE *|
\*******************/

union	name_pool_entry		{long		name_long;
				 char		name_char;
				};


/******************\
|* PDG LINK TABLE *|
\******************/

struct	pdg_link_tbl_entry 	{Ulong		at_idx;
				 Ulong		cn_idx;
				 Ulong		sb_idx;
				 Uint		at_typ_idx 	: 32;
				};


/*********************\
|* RENAME ONLY TABLE *|
\*********************/

struct	rename_only_entry	{Uint		line_num		: 24;
				 Uint		column_num		:  8;
				 Uint		name_len		:  8;
				 Uint		name_idx		: 24;

				 Uint		unused			: 23;	
				 boolean	duplicate_rename	:  1;	
				 Uint		unused1			:  7;	
				 boolean	rename_name		:  1;	
				 Uint		rename_idx		: 16;
				 Uint		next_idx		: 16;
				};


/***************\
|* SCOPE TABLE *|
\***************/

union	scp_wd_entry		{struct	{boolean	typed		:  1;
					 boolean	flag1		:  1;
					 boolean	flag2		:  1;
					 boolean	flag3		:  1;
					 Uint		storage		:  4;
					 Uint		field2		: 24;
					 Uint		field3		: 16;
					 Uint		type_idx	: 16;
					} fld1;

				 struct {Uint		UNUSED1		:  8;
					 Uint		field4		:  8;
					 Uint		field5		: 16;
					 Uint		UNUSED2		: 32;
					} fld2;

				};

struct	scp_tbl_entry		{scp_wd_type	wd[MAX_IMPL_CHS];
                                };


/************************\
|* SECONDARY NAME TABLE *|
\************************/

struct	sec_name_tbl_entry	{Uint		sibling_link	: 24;
                                 Uint           unused1         :  8;

/* WARNING - JLS has hard coded shifts and masks for these fields.  DO NOT */
/*           move them unless you want a bunch of wierd stuff to happen.   */

                                 Uint           length		:  8;
				 Uint		name_idx	: 24;

                                 Uint           line_num        : 24;
                                 Uint           column_num      :  8;

				 Uint		unused2		:  7;
				 boolean	matched		:  1;
				 Uint		attr_idx	: 24;
				};


/**************************\
|* STATEMENT HEADER TABLE *|
\**************************/

struct	sh_tbl_entry		{Uint		old_stmt_type      :  7;
                                 Uint		stmt_parse_err	   :  1;
                                 Uint		glb_line_num	   : 24;
				 Uint		col_num		   :  8;
                                 Uint		ir_idx		   : 24;

                                 /* word 2 */
                                 Uint		parent_blk_idx	   : 24;
                                 boolean	action_stmt	   :  1;
 				 boolean	inlining_attempted :  1;
 				 boolean	cif_skip_me	   :  1;
				 boolean	doall_loop_end	   :  1;
			  	 boolean	loop_end	   :  1;
                   		 boolean	labeled	    	   :  1;
			         boolean	compiler_gen	   :  1;
			         boolean	skip_pass_2	   :  1;
               
 				 Uint		unused		   : 16;
 				 stmt_type_type	stmt_type	   : 16;

                                 /* word 3 */
                                 Uint		prev_sh_idx        : 32;
                                 Uint		next_sh_idx        : 32;
                                };


/***********************\
|* STORAGE BLOCK TABLE *|
\***********************/

union	stor_blk_tbl_entry	{struct	{
				 Uint		name_idx	: 24;
				 Uint		name_len	:  8;
				 Uint		orig_scp_idx	: 16;
				 Uint		scp_idx		: 16;

				 Uint		first_attr_idx	: 20;
				 boolean	saved		:  1;
				 boolean	module		:  1;
				 boolean	blank_common	:  1;
				 boolean	auxiliary	:  1;
				 boolean	dcl_err		:  1;
				 boolean	hidden		:  1;
				 boolean	needs_offset	:  1;
				 boolean	hosted_stack	:  1;
				 boolean	def_mult_scps	:  1;
				 boolean	use_associated	:  1;
				 boolean	host_associated	:  1;
				 boolean	has_renames	:  1;
				 boolean	is_common	:  1;
				 boolean	cache_align	:  1;
				 boolean	hosted_static	:  1;
				 boolean	dcl_common_dir	:  1;
				 boolean	blk_has_npes	:  1;
				 boolean	symmetric	:  1;
				 boolean	pad_blk		:  1;
				 Uint		old_sb_type	:  5;
                                 boolean	pad_amount_set	:  1;
                                 boolean	equivalenced	:  1;
#ifdef KEY /* Bug 14150 */
                                 boolean	bind_attr   	:  1;
#else /* defined(BUILD_OS_DARWIN) */
                                 boolean	UNUSED   	:  1;
#endif /* KEY Bug 14150 */
                                 boolean	name_in_stone	:  1;
				 Uint		last_attr_list	: 16;

				 Uint		merged_blk_idx	: 16;
				 Uint		UNUSED1		: 16;
				 sb_type_type	sb_type		:  8;
				 boolean	align_symbol	:  1;
				 boolean	fill_symbol	:  1;
				 boolean	section_gp	:  1;
				 boolean	section_non_gp	:  1;
				 Uint		module_idx	: 20;

				 Uint		def_line	: 24;
				 Uint		def_column	:  8;
				 boolean	runtime_init	:  1;
				 boolean	duplicate_common:  1;
				 boolean	x_volatile	:  1;
				 boolean	flag31		:  1;
				 boolean	flag32		:  1;
				 boolean	flag33		:  1;
				 boolean	flag34		:  1;
				 boolean	flag35		:  1;
				 Uint		cif_idx		: 24;

#ifdef KEY /* Bug 14150 */
/* The -apad option has never been supported at Pathscale, so we can use
 * this space to implement F2003 "BIND(C, NAME=x)" for common blocks
 * without making old .mod files incompatible. */
				 Uint		ext_name_idx	: 24;
				 Uint		ext_name_len	:  8;
#else /* defined(BUILD_OS_DARWIN) */
				 Uint		pad_amount	: 16;
				 Uint		UNUSED3		: 16;
#endif /* KEY Bug 14150 */
				 fld_type	len_fld		:  8;
				 Uint		len_idx		: 24;
				} fld;

			struct {
				 Uint		Unused1		: 32;
				 Uint		Unused2		: 32;

				 Uint		Unused3		: 32;
				 Uint		Unused4		: 32;

				 Uint		Unused5		: 32;
				 Uint		Unused6		: 32;

				 Uint		Unused7		: 32;
				 Uint		Unused8		: 32;


# if defined(_HOST32) && defined(_TARGET32)
				 Uint		Unused9		: 32;
# endif
				 long_type	blk_len;
				}wd;
				};


/**************\
|* TYPE TABLE *|
\**************/

/* *** WARNING *** If the fields change here, type_init_tbl needs to change */
/* *** WARNING ***    in type.h   It is based on the type table definition. */

union	type_tbl_entry	{struct
				{basic_type_type  type		:  8;
				 Uint		  dcl_value	:  5;
				 Uint		  UNUSED0	:  3;
				 Uint		  char_class	:  2;
				 boolean	  kind_const    :  1;
				 boolean	  kind_double   :  1;
				 boolean	  dp_hit_me	:  1;
				 boolean	  resolved	:  1;
				 boolean	  type_is_byte	:  1;
				 boolean	  declared_dbl  :  1;
				 Uint		  UNUSED1	:  2;
				 Uint		  desc		:  2;
				 fld_type	  type_fld	:  4;

				 Uint		  type_idx	: 24;
				 linear_type_type linear_type	:  8;
				 long64		  bit_len;
				}fld;
			struct
				{Uint		  UNUSED3	: 32;
				 Uint		  UNUSED4	: 32;
				 Uint		  UNUSED5	: 32;
				 Uint		  index		: 32;
				}wd2;

			struct
				{Uint		  UNUSED6	: 32;
				 Uint		  UNUSED7	: 32;

# if defined(_HOST32) && defined(_TARGET32)
				 Uint		  UNUSED5	: 32;
# endif
				 long_type	  old_bit_len;
				}wd1;


			struct
				{Uint		  UNUSED8	: 32;
				 Uint		  UNUSED9	: 32;
				 long_type        length[MAX_WORDS_FOR_INTEGER];
				}wd;
			};


union	global_type_tbl_entry  {struct
				{basic_type_type  type		:  8;
				 Uint		  dcl_value	:  5;
				 Uint		  UNUSED0	:  3;
				 Uint		  char_class	:  2;
				 Uint		  len_lin_type	:  8;
				 Uint		  desc		:  2;
				 Uint		  UNUSED2	:  4;

				 Uint		  type_idx	: 24;
				 linear_type_type linear_type	:  8;
				 long64		  bit_len;
				}fld;

			struct
				{Uint		  UNUSED8	: 32;
				 Uint		  UNUSED9	: 32;
				 long_type        length[MAX_WORDS_FOR_INTEGER];
				}wd;
			};


/*******************************************\
|* intrinsic table declarations            *|
\*******************************************/

struct  intrin_tbl_entry       {
                                id_str_type     id_str;
                                Uint            name_len 		: 6;   
                                boolean         elemental               : 1;
                                boolean         function                : 1;
                                boolean         passable                : 1;
                                boolean         external                : 1;
                                boolean         optional                : 1;
                                boolean         dope                    : 1;
                                boolean         generic                 : 1;
#ifdef KEY /* Bug 4656 */
                                boolean         enabled                 : 1;
#else /* KEY Bug 4656 */
                                boolean         non_ansi                : 1;
#endif /* KEY Bug 4656 */
                                Uint            n_specifics             : 9;
                                Uint            intrin_enum		: 9;   
                                Uint            data_type		: 32;   
#ifdef KEY /* Bug 4656 */
                                Uint            families	        : 8;   
#endif /* KEY Bug 4656 */
        };

struct  intrin_map_entry       {
                                id_str_type     id_str;
                                id_str_type     mapped_4;
                                id_str_type     mapped_8;
        };




/*******************************************\
|* globally accessible function prototypes *|
\*******************************************/

extern	void		add_attr_to_local_list(int);
extern	void		align_bit_length(size_offset_type *, int);
extern	void		assign_offset(int);
#ifdef KEY /* Bug 14150 */
extern void assign_bind_c_offset(int, boolean);
#endif /* KEY Bug 14150 */
extern	void		assign_storage_blk(int);
extern	attr_aux_tbl_type  *attr_aux_var_error(char *, int);
extern	bounds_tbl_type *bd_var_error(char *, int);
extern	void		bits_and_bytes_to_words(size_offset_type *, int, int);
extern	void		global_name_semantics(int, int, int, int, int);
extern	void		check_for_duplicate_renames(int);
extern	int		check_global_pgm_unit(int);
extern	int		check_type_for_size_address(opnd_type *);
extern	void		chg_data_obj_to_pgm_unit(int, pgm_unit_type,
						      atp_proc_type);
extern	int		cval_to_cn_int(int, boolean, long64 *, boolean);
extern  int      	cval_to_f_int(long_type *, long64 *, int);
extern	boolean		compare_array_entries(int, int);
extern	boolean		compare_derived_types(int,int);
extern	int		compare_names(long *, int, long *, int);
extern	boolean		compare_target_consts(long_type *, int, long_type *,
                                              int, int);
extern	void		constant_value_semantics(opnd_type *, int);
extern	void		create_hidden_name_tbl(int);
extern	int		create_lib_entry_attr(char *, int, int, int);
extern	void		data_repeat_semantics(int);
extern  long64          f_int_to_cval(long_type *, int);
extern	void		fill_in_global_attr_ntry(int, int, int);
extern	void		find_opnd_line_and_column(opnd_type *, int *, int *);
extern	void		free_attr_list(int);
extern	global_attr_tbl_type  *ga_var_error(char *, int);
extern	int		gen_compiler_tmp(int, int, task_scope_type, boolean);
extern	int		gen_debug_lbl_stmt(int, atl_debug_class_type, int);
extern	void		gen_default_init_code(int);
extern	int		gen_internal_lbl(int);
extern	int		gen_tmp_equal_max_zero(opnd_type *, int, int, 
                                               boolean, boolean);
extern	char	       *get_basic_type_str(int);
extern	void		host_associated_attr_semantics(int, boolean);
extern	void		init_name_and_stor_tbls(int, boolean);
extern	void		init_sytb(void);
extern  ir_list_tbl_type *ir_list_var_error(char *, int);
extern	boolean		kind_to_linear_type(opnd_type *, int, boolean, boolean,
                                                              boolean, boolean);
extern	boolean		validate_kind(basic_type_type, int, int, long *, 
                                      linear_type_type *);
extern	void		make_external_name(int, int, int);
extern	int		make_ro_entry(int, int, boolean);
extern  long_type       mpp_cn_int_to_c(int);
extern	int		ntr_array_in_bd_tbl(int);
extern	int		ntr_attr_list_tbl(void);
extern  int		ntr_boz_const_tbl(int, long_type *);
extern  int		ntr_boolean_const_tbl(int, long_type *);
extern	int		ntr_common_in_global_attr_tbl(int, int);
extern	int		ntr_const_tbl(int, boolean, long_type *);
extern	int		ntr_cpnt_name(token_type *, int *, int);
extern	void 		ntr_global_name_tbl(int, int, int);
extern	int		ntr_global_attr_tbl(int, int);
extern	int		ntr_global_type_tbl(int);
extern	void		ntr_hidden_name_tbl(int, int, int);
extern	int		ntr_host_in_sym_tbl(token_type *,int,int,int,boolean);
extern	int		ntr_int_const_tbl(int, long64);
extern	int		ntr_ir_list_tbl(void);
extern	int		ntr_ir_tbl(void);
extern	int		ntr_gl_ir_list_tbl(void);
extern	int		ntr_gl_ir_tbl(void);
extern	int		ntr_gl_sh_tbl(void);
extern	int		ntr_local_attr_list(char *, int, int, int);
extern	int		ntr_sh_tbl(void);
extern	int		ntr_stor_blk_tbl(char *, int, int, int, int);
extern	int		ntr_sym_tbl(token_type *, int);
extern	int		ntr_type_tbl(void);
extern	int		ntr_unshared_const_tbl(int, boolean, long_type *);
extern	void		remove_hidden_name_ntry(int);
extern	void		remove_hidden_name_tbl(int);
extern	void		remove_ln_ntry(int);
extern	int		reserve_array_ntry(int);
extern	int		set_based_stor_blk(void);
extern	void		set_stride_for_first_dim(int, size_offset_type *);
extern  boolean         size_offset_binary_calc(size_offset_type *,
                                                size_offset_type *,
                                                operator_type,
                                                size_offset_type *);
extern  boolean         size_offset_logical_calc(size_offset_type *,
                                                 size_offset_type *,
                                                 operator_type,
                                                 size_offset_type *);
extern  boolean         size_offset_min_max_calc(size_offset_type *,
                                                 size_offset_type *,
					         operator_type,
                                                 size_offset_type *);
extern	boolean		srch_global_name_tbl(char *, int, int *);
extern	int 		srch_hidden_name_tbl(char *, int, int, int *, int *);
extern	int 		srch_host_stor_blk_tbl(token_type *);
extern	int 		srch_host_sym_tbl(char *, int, int *, boolean);
#ifdef KEY /* Bug 11741 */
extern int srch_host_sym_tbl_for_import(char *, int, int *);
extern int import_from_host(char *, int, int *, int);
#endif /* KEY Bug 11741 */
extern	int		srch_kwd_name(char *, int, int, int *);
extern	int		srch_linked_sn(char *, int, int *);
extern	int		srch_name_tbl(char *, int, int *, name_tbl_type *,
                                      name_pool_type *, int, int);
extern	int		srch_stor_blk_tbl(char *, int, int);
extern	int 		srch_sym_tbl(char *, int, int *);
extern	size_offset_type	stor_bit_size_of(int, boolean, boolean);
extern	attr_tbl_type  *sytb_var_error(char *, int);
extern	void		use_stmt_semantics(void);
#ifdef KEY /* Bug 6204 */
int decorate(char *, int, int);
#endif /* KEY Bug 6204 */


/*******************************\
|* globally accessible objects *|
\*******************************/

/* NOTE:  ..._idx always points to the last used entry in the table. */

extern	attr_list_tbl_type	*RESTRICT attr_list_tbl;
extern	int			 attr_list_tbl_idx;
extern	int			 attr_list_tbl_inc;
extern	int			 attr_list_tbl_size;
extern  int			 attr_list_tbl_limit;
extern	int			 attr_list_tbl_num_wds;
extern	int			 attr_list_tbl_init_size;
extern	int			 attr_list_tbl_largest_idx;

extern	attr_tbl_type		*RESTRICT attr_tbl;
extern	int			 attr_tbl_idx;
extern	int			 attr_tbl_inc;
extern	int			 attr_tbl_size;
extern  int			 attr_tbl_limit;
extern	int			 attr_tbl_num_wds;
extern	int			 attr_tbl_init_size;
extern	int			 attr_tbl_largest_idx;

extern	attr_aux_tbl_type	*RESTRICT attr_aux_tbl;
extern	int			 attr_aux_tbl_idx;
extern	int			 attr_aux_tbl_inc;
extern	int			 attr_aux_tbl_size;
extern  int			 attr_aux_tbl_limit;
extern	int			 attr_aux_tbl_num_wds;
extern	int			 attr_aux_tbl_init_size;
extern	int			 attr_aux_tbl_largest_idx;

extern	bounds_tbl_type		*RESTRICT bounds_tbl;
extern	int		 	 bounds_tbl_idx;
extern	int		 	 bounds_tbl_inc;
extern	int	 		 bounds_tbl_size;
extern  int			 bounds_tbl_limit;
extern	int			 bounds_tbl_num_wds;
extern	int			 bounds_tbl_init_size;
extern	int			 bounds_tbl_largest_idx;

extern  int			ieee_const_tbl_idx[];

extern	const_tbl_type		*RESTRICT const_tbl;
extern	int			 const_tbl_idx;
extern	int			 const_tbl_inc;
extern	int			 const_tbl_size;
extern  int			 const_tbl_limit;
extern	int			 const_tbl_num_wds;
extern	int			 const_tbl_init_size;
extern	int			 const_tbl_largest_idx;

extern	int			 cn_root_idx[Num_Linear_Types];

extern	const_pool_type		*RESTRICT const_pool;
extern	int			 const_pool_idx;
extern	int			 const_pool_inc;
extern	int			 const_pool_size;
extern  int			 const_pool_limit;
extern	int			 const_pool_num_wds;
extern	int			 const_pool_init_size;
extern	int			 const_pool_largest_idx;

extern	equiv_tbl_type		*RESTRICT equiv_tbl;
extern	int			 equiv_tbl_idx;
extern	int			 equiv_tbl_inc;
extern	int			 equiv_tbl_size;
extern  int			 equiv_tbl_limit;
extern	int			 equiv_tbl_num_wds;
extern	int			 equiv_tbl_init_size;
extern	int			 equiv_tbl_largest_idx;

extern	file_path_tbl_type	*RESTRICT file_path_tbl;
extern	int			 file_path_tbl_idx;
extern	int			 file_path_tbl_inc;
extern	int			 file_path_tbl_size;
extern  int			 file_path_tbl_limit;
extern	int			 file_path_tbl_num_wds;
extern	int			 file_path_tbl_init_size;
extern	int			 file_path_tbl_largest_idx;

extern	global_attr_tbl_type	*RESTRICT global_attr_tbl;
extern	int			 global_attr_tbl_idx;
extern	int			 global_attr_tbl_inc;
extern	int			 global_attr_tbl_size;
extern  int			 global_attr_tbl_limit;
extern	int			 global_attr_tbl_num_wds;
extern	int			 global_attr_tbl_init_size;
extern	int			 global_attr_tbl_largest_idx;

extern	global_bounds_tbl_type	*RESTRICT global_bounds_tbl;
extern	int			 global_bounds_tbl_idx;
extern	int			 global_bounds_tbl_inc;
extern	int			 global_bounds_tbl_size;
extern  int			 global_bounds_tbl_limit;
extern	int			 global_bounds_tbl_num_wds;
extern	int			 global_bounds_tbl_init_size;
extern	int			 global_bounds_tbl_largest_idx;

extern	global_line_tbl_type	*RESTRICT global_line_tbl;
extern	long			 global_line_tbl_idx;
extern	int			 global_line_tbl_inc;
extern	long			 global_line_tbl_size;
extern	long			 global_line_tbl_limit;
extern	int			 global_line_tbl_num_wds;
extern	int			 global_line_tbl_init_size;
extern	long			 global_line_tbl_largest_idx;

extern	name_tbl_type		*RESTRICT global_name_tbl;
extern	long			 global_name_tbl_idx;
extern	int			 global_name_tbl_inc;
extern	long			 global_name_tbl_size;
extern	long			 global_name_tbl_limit;
extern	int			 global_name_tbl_num_wds;
extern	int			 global_name_tbl_init_size;
extern	long			 global_name_tbl_largest_idx;
 
extern	global_type_tbl_type	*RESTRICT global_type_tbl;
extern	int			 global_type_tbl_idx;
extern	int			 global_type_tbl_inc;
extern	int			 global_type_tbl_size;
extern  int			 global_type_tbl_limit;
extern	int			 global_type_tbl_num_wds;
extern  int			 global_type_tbl_init_size;
extern  int			 global_type_tbl_largest_idx;

extern	global_ir_tbl_type      *RESTRICT global_ir_tbl;
extern	int                     global_ir_tbl_idx;
extern	int                     global_ir_tbl_inc;
extern	int                     global_ir_tbl_init_size;
extern	int                     global_ir_tbl_limit;
extern	int                     global_ir_tbl_num_wds;
extern	int                     global_ir_tbl_size;
extern	int                     global_ir_tbl_largest_idx;

extern	global_ir_list_tbl_type *RESTRICT global_ir_list_tbl;
extern	int                     global_ir_list_tbl_idx;
extern	int                     global_ir_list_tbl_inc;
extern	int                     global_ir_list_tbl_init_size;
extern	int                     global_ir_list_tbl_limit;
extern	int                     global_ir_list_tbl_num_wds;
extern	int                     global_ir_list_tbl_size;
extern	int                     global_ir_list_tbl_largest_idx;

extern	global_sh_tbl_type     *RESTRICT global_sh_tbl;
extern	int                     global_sh_tbl_idx;
extern	int                     global_sh_tbl_inc;
extern	int                     global_sh_tbl_init_size;
extern	int                     global_sh_tbl_limit;
extern	int                     global_sh_tbl_num_wds;
extern	int                     global_sh_tbl_size;
extern	int                     global_sh_tbl_largest_idx;

extern	name_tbl_type		*RESTRICT hidden_name_tbl;
extern	int			 hidden_name_tbl_idx;
extern	int			 hidden_name_tbl_inc;
extern	int			 hidden_name_tbl_size;
extern  int			 hidden_name_tbl_limit;
extern	int			 hidden_name_tbl_num_wds;
extern	int			 hidden_name_tbl_init_size;
extern	int			 hidden_name_tbl_largest_idx;

extern	ir_tbl_type		*RESTRICT ir_tbl;
extern	int			 ir_tbl_idx;
extern	int			 ir_tbl_inc;
extern	int			 ir_tbl_size;
extern	int			 ir_tbl_limit;
extern	int			 ir_tbl_num_wds;
extern	int			 ir_tbl_init_size;
extern	int			 ir_tbl_largest_idx;

extern  ir_list_tbl_type	*RESTRICT ir_list_tbl;
extern  int			 ir_list_tbl_idx;
extern  int			 ir_list_tbl_inc;
extern  int			 ir_list_tbl_size;
extern  int			 ir_list_tbl_limit;
extern	int			 ir_list_tbl_num_wds;
extern	int			 ir_list_tbl_init_size;
extern	int			 ir_list_tbl_largest_idx;
 
extern	name_tbl_type		*RESTRICT loc_name_tbl;
extern	int			 loc_name_tbl_idx;
extern	int			 loc_name_tbl_inc;
extern	int			 loc_name_tbl_size;
extern  int			 loc_name_tbl_limit;
extern	int			 loc_name_tbl_num_wds;
extern	int			 loc_name_tbl_init_size;
extern	int			 loc_name_tbl_largest_idx;

extern	mod_link_tbl_type	*RESTRICT mod_link_tbl;
extern	long			 mod_link_tbl_idx;
extern	int			 mod_link_tbl_inc;
extern	long			 mod_link_tbl_size;
extern  long			 mod_link_tbl_limit;
extern	int			 mod_link_tbl_num_wds;
extern	int			 mod_link_tbl_init_size;
extern	long			 mod_link_tbl_largest_idx;

extern	name_pool_type		*RESTRICT name_pool;
extern	int			 name_pool_idx;
extern	int			 name_pool_inc;
extern	int			 name_pool_size;
extern  int			 name_pool_limit;
extern	int			 name_pool_num_wds;
extern	int			 name_pool_init_size;
extern	int			 name_pool_largest_idx;

extern	pdg_link_tbl_type	*RESTRICT pdg_link_tbl;
extern	long			 pdg_link_tbl_idx;
extern	int			 pdg_link_tbl_inc;
extern	long			 pdg_link_tbl_size;
extern  long			 pdg_link_tbl_limit;
extern	int			 pdg_link_tbl_num_wds;
extern	int			 pdg_link_tbl_init_size;
extern	long			 pdg_link_tbl_largest_idx;

extern	rename_only_tbl_type	*RESTRICT rename_only_tbl;
extern	int			 rename_only_tbl_idx;
extern	int			 rename_only_tbl_inc;
extern	int			 rename_only_tbl_size;
extern  int			 rename_only_tbl_limit;
extern	int			 rename_only_tbl_num_wds;
extern	int			 rename_only_tbl_init_size;
extern	int			 rename_only_tbl_largest_idx;

extern	scp_tbl_type		*RESTRICT scp_tbl;
extern  int			 scp_tbl_idx;
extern	int			 scp_tbl_inc ;
extern  int			 scp_tbl_size;
extern  int			 scp_tbl_limit;
extern	int			 scp_tbl_num_wds;
extern	int			 scp_tbl_init_size;
extern	int			 scp_tbl_largest_idx;

extern	sec_name_tbl_type	*RESTRICT sec_name_tbl;
extern	int			 sec_name_tbl_idx;
extern	int			 sec_name_tbl_inc;
extern	int			 sec_name_tbl_size;
extern  int			 sec_name_tbl_limit;
extern	int			 sec_name_tbl_num_wds;
extern	int			 sec_name_tbl_init_size;
extern	int			 sec_name_tbl_largest_idx;

extern	sh_tbl_type		*RESTRICT sh_tbl;
extern	int			 sh_tbl_idx;
extern	int			 sh_tbl_inc;
extern	int			 sh_tbl_size;
extern	int			 sh_tbl_limit;
extern	int			 sh_tbl_num_wds;
extern	int			 sh_tbl_init_size;
extern	int			 sh_tbl_largest_idx;

extern	stor_blk_tbl_type	*RESTRICT stor_blk_tbl;
extern	int			 stor_blk_tbl_idx;
extern	int			 stor_blk_tbl_inc;
extern	int			 stor_blk_tbl_size;
extern  int			 stor_blk_tbl_limit;
extern	int			 stor_blk_tbl_num_wds;
extern	int			 stor_blk_tbl_init_size;
extern	int			 stor_blk_tbl_largest_idx;

extern	name_pool_type		*RESTRICT str_pool;
extern	int			 str_pool_idx;
extern	int			 str_pool_inc;
extern	int			 str_pool_size;
extern  int			 str_pool_limit;
extern	int			 str_pool_num_wds;
extern	int			 str_pool_init_size;
extern	int			 str_pool_largest_idx;

extern	type_tbl_type		*RESTRICT type_tbl;
extern	int			 type_tbl_idx;
extern	int			 type_tbl_inc;
extern	int			 type_tbl_size;
extern  int			 type_tbl_limit;
extern	int			 type_tbl_num_wds;
extern  int			 type_tbl_init_size;
extern  int			 type_tbl_largest_idx;

#ifdef KEY /* Bug 4656 */
extern  intrin_tbl_type    	 intrin_tbl[];
extern  unsigned		 MAX_INTRIN_TBL_SIZE;
#else /* KEY Bug 4656 */
extern  intrin_tbl_type    	 intrin_tbl[MAX_INTRIN_TBL_SIZE];
#endif /* KEY Bug 4656 */
extern  intrin_map_type    	 intrin_map[MAX_INTRIN_MAP_SIZE];
extern	void			 (*intrinsic_semantics[])();

extern	char		        *pgm_unit_str[];

#define COMPILER_TMP_PREFIX_LEN 2
extern char compiler_tmp_prefix[];

# ifdef _DEBUG
extern void print_so(size_offset_type);
# endif
#ifdef KEY /* Bug 14150 */
extern char *file_and_line(int);
#endif /* KEY Bug 14150 */
