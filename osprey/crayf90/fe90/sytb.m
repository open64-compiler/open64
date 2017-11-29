/*
 * Copyright (C) 2010 Advanced Micro Devices, Inc.  All Rights Reserved.
 */

/*
 *  Copyright (C) 2006. QLogic Corporation. All Rights Reserved.
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


/* USMID:  "\n@(#)5.0_pl/macros/sytb.m	5.16	09/30/99 15:47:54\n" */

/**************************\
|* RESERVED TABLE INDEXES *|
\**************************/

# define AT_WORK_IDX		NULL_IDX   /* Attr table work area            */
# define BD_FREE_LIST_IDX	NULL_IDX   /* Bounds table free list          */

/* WARNING - These must be 1-7.  Alot of code has made the asumption that     */
/*           rank matches index for these default deferred shape arrays.      */

# define BD_DEFERRED_1_IDX	1	   /* Deferred shape - rank 1         */
# define BD_DEFERRED_2_IDX	2	   /* Deferred shape - rank 2         */
# define BD_DEFERRED_3_IDX	3	   /* Deferred shape - rank 3         */
# define BD_DEFERRED_4_IDX	4	   /* Deferred shape - rank 4         */
# define BD_DEFERRED_5_IDX	5	   /* Deferred shape - rank 5         */
# define BD_DEFERRED_6_IDX	6	   /* Deferred shape - rank 6         */
# define BD_DEFERRED_7_IDX	7	   /* Deferred shape - rank 7         */

# define MAX_NUM_DIMS		7	   /* Max # of dimensions we allow.   */

/* If you add a constant macro here, you must change CN_LAST_USED_IDX.        */

# define CN_INTEGER_ZERO_IDX		1
# define CN_INTEGER_ONE_IDX		2
# define CN_INTEGER_TWO_IDX		3
# define CN_INTEGER_THREE_IDX		4
# define CN_INTEGER_NEG_ONE_IDX		5
# define CN_INTEGER_BITS_PER_WORD_IDX	6
# define CN_INTEGER_CHAR_BIT_IDX	7  /* CHAR_BIT from limits.h          */


# define INTRINSIC_SCP_IDX	NULL_IDX   /* Intrinsic scope.                */
# define MAIN_SCP_IDX		1	   /* Scope index of outer program    */

# define NAME_POOL_ZERO_IDX	1	   /* The all zero entry - for LN     */
# define NAME_POOL_ONES_IDX	2	   /* The all ones entry - for LN     */

# define TYP_WORK_IDX		0	   /* Enter type table work area      */

/* The enum Num_Linear_Types gets to the -dp entry in the type tbl. */
/* It follows all the default linear type entries.                  */

# define DOUBLE_PRECISION_TYPE_IDX                    Num_Linear_Types
# define DOUBLE_COMPLEX_TYPE_IDX                      Num_Linear_Types + 1

/********************\
|* SIZES AND LIMITS *|
\********************/

# ifdef _HOST64
#	define NUM_AA_WDS		3
#	define NUM_AL_WDS		1
#	define NUM_AT_WDS		7
#	define NUM_BD_WDS		2
#       define NUM_BLK_STK_WDS  	8
#	define NUM_CN_WDS		2
#	define OLD_NUM_CN_WDS		1
#	define NUM_CP_WDS		1
#	define NUM_CS_WDS		1
#	define NUM_EQ_WDS		4
#	define NUM_FP_WDS		3
#	define NUM_GA_WDS		4
#	define NUM_GB_WDS		1
#	define NUM_GL_WDS		5
#	define NUM_GN_WDS		1
#	define NUM_HN_WDS		1
#	define NUM_IL_WDS		2
#	define NUM_IR_WDS		4
#	define NUM_LN_WDS		1
#	define NUM_ML_WDS		6
#	define NUM_NP_WDS		1
# ifdef _WHIRL_HOST64_TARGET64
#	define NUM_PDG_WDS		4
# else
#	define NUM_PDG_WDS		2
# endif /* _WHIRL_HOST64_TARGET64 */
#	define NUM_RO_WDS		2
#	define NUM_SB_WDS		5
#	define NUM_SCP_WDS		26
#	define NUM_SH_WDS		3
#	define NUM_SN_WDS		2
#	define NUM_TYP_WDS		2
# else
#	define NUM_AA_WDS		6
#	define NUM_AL_WDS		2
#	define NUM_AT_WDS		14
#	define NUM_BD_WDS		4
#       define NUM_BLK_STK_WDS  	16
#	define NUM_CN_WDS		4
#	define OLD_NUM_CN_WDS		2
#	define NUM_CP_WDS		1
#	define NUM_CS_WDS		2
#	define NUM_EQ_WDS		8
#	define NUM_FP_WDS		6
#	define NUM_GA_WDS		8
#	define NUM_GB_WDS		2
#	define NUM_GL_WDS		10
#	define NUM_GN_WDS		2
#	define NUM_HN_WDS		2
#	define NUM_IL_WDS		4
#	define NUM_IR_WDS		8
#	define NUM_LN_WDS		2
#	define NUM_ML_WDS		12
#	define NUM_NP_WDS		1
#	define NUM_PDG_WDS		4
#	define NUM_RO_WDS		4
#	define NUM_SB_WDS		10
#	define NUM_SCP_WDS		52
#	define NUM_SH_WDS		6
#	define NUM_SN_WDS		4
#	define NUM_TYP_WDS		4
# endif

# define SET_POINTER_SIZE	(cmd_line_flags.s_pointer8)
/* OSP_456 */
/* To avoid compiler warnings don't include "config_targ.h", which
 * contains versions of Is_Target_32bit and Is_Target_64bit that are
 * BE specific.
 */
# define Is_Target_32bit()      (cmd_line_flags.s_pointer8 == 0)
# define Is_Target_64bit()      (cmd_line_flags.s_pointer8 == 1)

/* OSP_467, #4, select ptr32 or ptr64 for TARG_X8664 at runtime */
# if defined(_DOPE_VECTOR_32_OR_64) || defined(TARG_X8664)

# if defined(TARG_X8664) && defined(_HOST64)
#  define DV_ALLOC_CPNT_OFFSET_WORD_SIZE		1
#  define DV_DIM_WORD_SIZE               ((SET_POINTER_SIZE)? 3 : 3)
#  define DV_HD_WORD_SIZE                ((SET_POINTER_SIZE)? 6 : 8)
#  define DV_BITS_PER_WORD               ((SET_POINTER_SIZE)? 64 : 32)
#  define INTEGER_DEFAULT_BITS           ((SET_POINTER_SIZE)? 64 : 32)
# else
#  define DV_ALLOC_CPNT_OFFSET_WORD_SIZE ((SET_POINTER_SIZE)? 2 : 1)
#  define DV_DIM_WORD_SIZE               ((SET_POINTER_SIZE)? 6 : 3)
#  define DV_HD_WORD_SIZE                ((SET_POINTER_SIZE)? 12 : 8)
#  define DV_BITS_PER_WORD               TARGET_BITS_PER_WORD
#  define INTEGER_DEFAULT_BITS           TARGET_BITS_PER_WORD
# endif

# define DV_BASE_ADDR(DOPE)		((SET_POINTER_SIZE)?                   \
                     (DOPE).ptr64.base_addr : (DOPE).ptr32.base_addr)
# define DV_EL_LEN(DOPE)		((SET_POINTER_SIZE)?                   \
                     (DOPE).ptr64.el_len : (DOPE).ptr32.el_len)
# define DV_ASSOC(DOPE)			((SET_POINTER_SIZE)?                   \
                     (DOPE).ptr64.assoc : (DOPE).ptr32.assoc)
# define DV_PTR_ALLOC(DOPE)		((SET_POINTER_SIZE)?                   \
                     (DOPE).ptr64.ptr_alloc : (DOPE).ptr32.ptr_alloc)
# define DV_P_OR_A(DOPE)		((SET_POINTER_SIZE)?                   \
                     (DOPE).ptr64.p_or_a : (DOPE).ptr32.p_or_a)
# define DV_A_CONTIG(DOPE)		((SET_POINTER_SIZE)?                   \
                     (DOPE).ptr64.a_contig : (DOPE).ptr32.a_contig)
# define DV_NUM_DIMS(DOPE)		((SET_POINTER_SIZE)?                   \
                     (DOPE).ptr64.num_dims : (DOPE).ptr32.num_dims)
# define DV_TYPE_CODE(DOPE)		((SET_POINTER_SIZE)?                   \
                     (DOPE).ptr64.type_code : (DOPE).ptr32.type_code)
# define DV_UNUSED_1(DOPE)		((SET_POINTER_SIZE)?                   \
                     (DOPE).ptr64.unused_1 : (DOPE).ptr32.unused_1)
# define DV_UNUSED_2(DOPE)		((SET_POINTER_SIZE)?                   \
                     (DOPE).ptr64.unused_2 : (DOPE).ptr32.unused_2)
# define DV_UNUSED_3(DOPE)		((SET_POINTER_SIZE)?                   \
                     (DOPE).ptr64.unused_3 : (DOPE).ptr32.unused_3)
# define DV_ORIG_BASE(DOPE)		((SET_POINTER_SIZE)?                   \
                     (DOPE).ptr64.orig_base : (DOPE).ptr32.orig_base)
# define DV_ORIG_SIZE(DOPE)		((SET_POINTER_SIZE)?                   \
                     (DOPE).ptr64.orig_size : (DOPE).ptr32.orig_size)
# define DV_LOW_BOUND(DOPE,IDX)		((SET_POINTER_SIZE)?                   \
         (DOPE).ptr64.dim[IDX].low_bound : (DOPE).ptr32.dim[IDX].low_bound)
# define DV_EXTENT(DOPE,IDX)		((SET_POINTER_SIZE)?                   \
         (DOPE).ptr64.dim[IDX].extent : (DOPE).ptr32.dim[IDX].extent)
# define DV_STRIDE_MULT(DOPE,IDX)	((SET_POINTER_SIZE)?                   \
         (DOPE).ptr64.dim[IDX].stride_mult : (DOPE).ptr32.dim[IDX].stride_mult)

# define DV_SET_BASE_ADDR(DOPE,RHS)                                            \
                     { if (SET_POINTER_SIZE)(DOPE).ptr64.base_addr = (RHS);    \
                       else (DOPE).ptr32.base_addr = (RHS); }
# define DV_SET_EL_LEN(DOPE,RHS)                                               \
                     { if (SET_POINTER_SIZE)(DOPE).ptr64.el_len = (RHS);       \
                       else (DOPE).ptr32.el_len = (RHS); }
# define DV_SET_ASSOC(DOPE,RHS)                                                \
                     { if (SET_POINTER_SIZE)(DOPE).ptr64.assoc = (RHS);        \
                       else (DOPE).ptr32.assoc = (RHS); }
# define DV_SET_PTR_ALLOC(DOPE,RHS)                                            \
                     { if (SET_POINTER_SIZE)(DOPE).ptr64.ptr_alloc = (RHS);    \
                       else (DOPE).ptr32.ptr_alloc = (RHS); }
# define DV_SET_P_OR_A(DOPE,RHS)                                               \
                     { if (SET_POINTER_SIZE)(DOPE).ptr64.p_or_a = (RHS);       \
                       else (DOPE).ptr32.p_or_a = (RHS); }
# define DV_SET_A_CONTIG(DOPE,RHS)                                             \
                     { if (SET_POINTER_SIZE)(DOPE).ptr64.a_contig = (RHS);     \
                       else (DOPE).ptr32.a_contig = (RHS); }
# define DV_SET_NUM_DIMS(DOPE,RHS)                                             \
                     { if (SET_POINTER_SIZE)(DOPE).ptr64.num_dims = (RHS);     \
                       else (DOPE).ptr32.num_dims = (RHS); }
# define DV_SET_TYPE_CODE(DOPE,RHS)                                            \
                     { if (SET_POINTER_SIZE)(DOPE).ptr64.type_code = (RHS);    \
                       else (DOPE).ptr32.type_code = (RHS); }
# define DV_SET_UNUSED_1(DOPE,RHS)                                             \
                     { if (SET_POINTER_SIZE)(DOPE).ptr64.unused_1 = (RHS);     \
                       else (DOPE).ptr32.unused_1 = (RHS); }
# define DV_SET_UNUSED_2(DOPE,RHS)                                             \
                     { if (SET_POINTER_SIZE)(DOPE).ptr64.unused_2 = (RHS);     \
                       else (DOPE).ptr32.unused_2 = (RHS); }
# define DV_SET_UNUSED_3(DOPE,RHS)                                             \
                     { if (SET_POINTER_SIZE)(DOPE).ptr64.unused_3 = (RHS);     \
                       else (DOPE).ptr32.unused_3 = (RHS); }
# define DV_SET_ORIG_BASE(DOPE,RHS)                                            \
                     { if (SET_POINTER_SIZE)(DOPE).ptr64.orig_base = (RHS);    \
                       else (DOPE).ptr32.orig_base = (RHS); }
# define DV_SET_ORIG_SIZE(DOPE,RHS)                                            \
                     { if (SET_POINTER_SIZE)(DOPE).ptr64.orig_size = (RHS);    \
                       else (DOPE).ptr32.orig_size = (RHS); }
# define DV_SET_LOW_BOUND(DOPE,IDX,RHS)                                        \
            { if (SET_POINTER_SIZE)(DOPE).ptr64.dim[IDX].low_bound = (RHS);    \
                       else (DOPE).ptr32.dim[IDX].low_bound = (RHS); }
# define DV_SET_EXTENT(DOPE,IDX,RHS)                                           \
            { if (SET_POINTER_SIZE)(DOPE).ptr64.dim[IDX].extent = (RHS);       \
                       else (DOPE).ptr32.dim[IDX].extent = (RHS); }
# define DV_SET_STRIDE_MULT(DOPE,IDX,RHS)                                      \
            { if (SET_POINTER_SIZE)(DOPE).ptr64.dim[IDX].stride_mult = (RHS);  \
                       else (DOPE).ptr32.dim[IDX].stride_mult = (RHS); }

# else   /* _DOPE_VECTOR_32 and DOPE_VECTOR_64 */

#ifdef KEY /* Bug 6845 */
# define DV_ALLOC_CPNT_OFFSET_WORD_SIZE		1
#endif /* KEY Bug 6845 */

# define DV_DIM_WORD_SIZE		3       /* Size of dope vector dimen  */
# define DV_HD_WORD_SIZE		6       /* Size of dope vector header */

/* OSP_XXX */
# define DV_BITS_PER_WORD               TARGET_BITS_PER_WORD
# define INTEGER_DEFAULT_BITS           TARGET_BITS_PER_WORD

# define DV_BASE_ADDR(DOPE)             (DOPE).base_addr
# define DV_EL_LEN(DOPE)                (DOPE).el_len
# define DV_ASSOC(DOPE)                 (DOPE).assoc
# define DV_PTR_ALLOC(DOPE)             (DOPE).ptr_alloc
# define DV_P_OR_A(DOPE)                (DOPE).p_or_a
# define DV_A_CONTIG(DOPE)              (DOPE).a_contig
# define DV_NUM_DIMS(DOPE)              (DOPE).num_dims
# define DV_TYPE_CODE(DOPE)             (DOPE).type_code
# define DV_UNUSED_1(DOPE)              (DOPE).unused_1
# define DV_UNUSED_2(DOPE)              (DOPE).unused_2
# define DV_UNUSED_3(DOPE)              (DOPE).unused_3
# define DV_ORIG_BASE(DOPE)             (DOPE).orig_base
# define DV_ORIG_SIZE(DOPE)             (DOPE).orig_size
# define DV_LOW_BOUND(DOPE,IDX)         (DOPE).dim[IDX].low_bound
# define DV_EXTENT(DOPE,IDX)            (DOPE).dim[IDX].extent
# define DV_STRIDE_MULT(DOPE,IDX)       (DOPE).dim[IDX].stride_mult


# define DV_SET_BASE_ADDR(DOPE,RHS)	(DOPE).base_addr = (RHS)
# define DV_SET_EL_LEN(DOPE,RHS)	(DOPE).el_len = (RHS)
# define DV_SET_ASSOC(DOPE,RHS)		(DOPE).assoc = (RHS)
# define DV_SET_PTR_ALLOC(DOPE,RHS)	(DOPE).ptr_alloc = (RHS)
# define DV_SET_P_OR_A(DOPE,RHS)	(DOPE).p_or_a = (RHS)
# define DV_SET_A_CONTIG(DOPE,RHS)	(DOPE).a_contig = (RHS)
# define DV_SET_NUM_DIMS(DOPE,RHS)	(DOPE).num_dims = (RHS)
# define DV_SET_TYPE_CODE(DOPE,RHS)	(DOPE).type_code = (RHS)
# define DV_SET_UNUSED_1(DOPE,RHS)	(DOPE).unused_1 = (RHS)
# define DV_SET_UNUSED_2(DOPE,RHS)	(DOPE).unused_2 = (RHS)
# define DV_SET_UNUSED_3(DOPE,RHS)	(DOPE).unused_3 = (RHS)
# define DV_SET_ORIG_BASE(DOPE,RHS)	(DOPE).orig_base = (RHS)
# define DV_SET_ORIG_SIZE(DOPE,RHS)	(DOPE).orig_size = (RHS)
# define DV_SET_LOW_BOUND(DOPE,IDX,RHS)	(DOPE).dim[IDX].low_bound = (RHS)
# define DV_SET_EXTENT(DOPE,IDX,RHS)	(DOPE).dim[IDX].extent = (RHS)
# define DV_SET_STRIDE_MULT(DOPE,IDX,RHS) (DOPE).dim[IDX].stride_mult = (RHS)
# endif

# define MAX_GENERATED_LABELS		99999	/* Max num compiler gen'd lbls*/
# define MAX_IMPL_CHS 			26  	/* number of ASCII characters */
# define MAX_ALTERNATE_ENTRIES		65535  	/* Max num alt entries.       */


/********************************************************\
|* NOTE:  FIELD_BITS ARE BIT SIZES FOR THE VARIOUS IDX  *|
|* FIELDS FOR ALL TABLES.  THESE MUST BE KEPT UP TO     *|
|* DATE TO CHECK FOR OVERFLOW OF THESE IDX FIELDS. THEY *|
|* REPRESENT THE SMALLEST BIT LENGTH OF ALL THE IDX     *|
|* FIELDS FOR EACH TABLE TYPE.                          *|
\********************************************************/

# define BD_LAST_USED_IDX	7	/* last reserved index                */
# define CN_LAST_USED_IDX	7	/* last reserved index                */
# define CP_LAST_USED_IDX	7	/* last reserved index 		      */

# define CS_LAST_USED_IDX    1000       /* Must be the same as the initial    */
                                        /* size of the const_search_tbl.      */

# define NP_LAST_USED_IDX	2	/* last reserved index                */
# define TYP_LAST_USED_IDX	Num_Linear_Types + 1 /* 2 extra entries */

#ifdef KEY /* Bug 4656 */
 /* Let the C compiler figure it out */
#else
# define MAX_INTRIN_TBL_SIZE   13400    /* max entries in intrinsic table     */
#endif /* KEY Bug 4656 */
# ifdef KEY
# define MAX_INTRIN_MAP_SIZE   75       /* max entries in intrinsic map       */
# else
# define MAX_INTRIN_MAP_SIZE   59       /* max entries in intrinsic map       */
# endif

# define MAX_INLINE_ARGS       256      /* max actual args allowed on a       */
                                        /* call to be inlined                 */

# define MAX_INLINE_LABELS     1000     /* max number of labels allowed in    */
                                        /* a routine being inlined            */

# define MAX_INLINE_IR         100000   /* max number of IR entries allowed   */
                                        /* in a given routine                 */

# define MAX_INLINED_ROUTINES  1000     /* max number of distinct routines    */
                                        /* inlined with a given routine       */


/******************************\
|* OBJECT REPLACEMENT STRINGS *|
\******************************/

# define STORAGE_WORD_SIZE(BIT_SIZE)	(TARGET_BITS_TO_WORDS((BIT_SIZE)))



/*  ATTRIBUTE TABLE */

/* Common attribute definitions for all object classes */


# define AT_ACCESS_SET(IDX)		attr_aux_tbl[IDX].fld.access_set
# define AT_ACTUAL_ARG(IDX)		attr_tbl[IDX].fld.passed_as_arg
# define AT_ALT_DARG(IDX)		attr_tbl[IDX].fld.alt_darg
# define AT_ARG_TO_KIND(IDX)		attr_aux_tbl[IDX].fld.arg_to_kind
# define AT_ATTR_LINK(IDX)		attr_tbl[IDX].fld.attr_link
# define AT_CIF_DONE(IDX)		attr_aux_tbl[IDX].fld.cif_done
# define AT_CIF_IN_USAGE_REC(IDX)	attr_aux_tbl[IDX].fld.cif_usage_rec
# define AT_CIF_SYMBOL_ID(IDX)		attr_aux_tbl[IDX].fld.cif_sym_id
# define AT_CIF_USE_IN_BND(IDX)		attr_aux_tbl[IDX].fld.cif_use_in_bnd
# define AT_COMPILER_GEND(IDX)		attr_tbl[IDX].fld.compiler_gend
# define AT_DCL_ERR(IDX)		attr_tbl[IDX].fld.dcl_err
# define AT_DEF_LINE(IDX)		attr_aux_tbl[IDX].fld.def_line
# define AT_DEF_COLUMN(IDX)		attr_aux_tbl[IDX].fld.def_column
# define AT_DEF_IN_CHILD(IDX)		attr_tbl[IDX].fld.def_in_child
# define AT_DEFINED(IDX)		attr_tbl[IDX].fld.defined
# define AT_ELEMENTAL_INTRIN(IDX)	attr_tbl[IDX].fld.elemental_intrin
# define AT_HOST_ASSOCIATED(IDX)	attr_tbl[IDX].fld.host_associated
# define AT_IGNORE_ATTR_LINK(IDX)	attr_tbl[IDX].fld.ignore_attr_link
# define AT_IS_DARG(IDX)		attr_tbl[IDX].fld.is_darg
#ifdef KEY /* Bug 5089 */
/* F2003: For a module, this has special meanings: prior to semantics, it
 * means that at least one "use" for this module in the current scope has the
 * "intrinsic" module-nature keyword; after semantics, it means that we did
 * use-associate an intrinsic module. */
#endif /* KEY Bug 5089 */
# define AT_IS_INTRIN(IDX)		attr_tbl[IDX].fld.is_intrin
# define AT_LOCKED_IN(IDX)		attr_aux_tbl[IDX].fld.locked_in
# define AT_MODULE_IDX(IDX)		attr_tbl[IDX].fld.module_idx
# define AT_MODULE_OBJECT(IDX)		attr_tbl[IDX].fld.module_object
# define AT_NAME_IDX(IDX)		attr_tbl[IDX].fld.name_idx
# define AT_NAME_LEN(IDX)		attr_tbl[IDX].fld.length
# define AT_NAMELIST_OBJ(IDX)		attr_tbl[IDX].fld.namelist_obj
# define AT_NOT_VISIBLE(IDX)		attr_tbl[IDX].fld.not_visible
# define AT_OBJ_CLASS(IDX)		attr_tbl[IDX].fld.object_class
# define AT_OBJ_NAME(IDX)		name_pool[AT_NAME_IDX(IDX)].name_char
# define AT_OBJ_NAME_LONG(IDX)		((long *)&name_pool[AT_NAME_IDX(IDX)])
# define AT_OBJ_NAME_PTR(IDX)		((char *)&name_pool[AT_NAME_IDX(IDX)])
# define AT_OPTIONAL(IDX)		attr_tbl[IDX].fld.optional
# define AT_ORIG_MODULE_IDX(IDX)	attr_tbl[IDX].fld.orig_module_idx
# define AT_ORIG_NAME_IDX(IDX)		attr_tbl[IDX].fld.orig_name_idx
# define AT_ORIG_NAME_LEN(IDX)		attr_tbl[IDX].fld.orig_name_len
# define AT_ORIG_NAME_PTR(IDX)	    ((char *)&name_pool[AT_ORIG_NAME_IDX(IDX)])
# define AT_ORIG_NAME(IDX)	    name_pool[AT_ORIG_NAME_IDX(IDX)].name_char
# define AT_ORIG_NAME_LONG(IDX)     ((long *)&name_pool[AT_ORIG_NAME_IDX(IDX)])
# define AT_PRIVATE(IDX)		attr_tbl[IDX].fld.private_access
# define AT_REF_IN_CHILD(IDX)		attr_tbl[IDX].fld.ref_in_child
# define AT_REFERENCED(IDX)		attr_tbl[IDX].fld.referenced
# define AT_SEMANTICS_DONE(IDX)		attr_aux_tbl[IDX].fld.semantics_done
# define AT_TYPED(IDX)			attr_tbl[IDX].fld.typed
# define AT_USE_ASSOCIATED(IDX)		attr_tbl[IDX].fld.use_associated

/* Remove in 5.0 */

# define OLD_AT_HOST_ASSOCIATED(IDX)	attr_tbl[IDX].old.host_associated
# define OLD_AT_MODULE_IDX(IDX)		attr_tbl[IDX].old.module_idx
# define OLD_AT_MODULE_OBJECT(IDX)	attr_tbl[IDX].old.module_object
# define OLD_AT_NOT_VISIBLE(IDX)	attr_tbl[IDX].old.not_visible
# define OLD_AT_OBJ_CLASS(IDX)		attr_tbl[IDX].old.object_class
# define OLD_AT_USE_ASSOCIATED(IDX)	attr_tbl[IDX].old.use_associated



/* Remove in 5.0 */
/* Cannot variant check these, because AT_OBJ_CLASS has moved */

# define OLD_ATD_ALIGN_SYMBOL(IDX)	attr_tbl[IDX].old.flag13
# define OLD_ATD_ARRAY_IDX(IDX)		attr_tbl[IDX].old.field5
# define OLD_ATD_DISTRIBUTION_IDX(IDX)	attr_tbl[IDX].old.field14
# define OLD_ATD_FILL_SYMBOL(IDX)	attr_tbl[IDX].old.flag14
# define OLD_ATD_OFFSET_ASSIGNED(IDX)	attr_tbl[IDX].old.flag16
# define OLD_ATD_PE_ARRAY_IDX(IDX)      attr_tbl[IDX].old.field15
# define OLD_ATD_TYPE_IDX(IDX)		attr_tbl[IDX].old.field1
# define OLD_ATI_NUM_SPECIFICS(IDX)	attr_tbl[IDX].old.field5
# define OLD_ATL_CYCLE_LBL(IDX)		attr_tbl[IDX].old.flag15
# define OLD_ATL_DIRECTIVE_LIST(IDX)	attr_tbl[IDX].fld2.field23
# define OLD_ATL_NOTASK(IDX)		attr_tbl[IDX].old.flag14
# define OLD_ATL_PREFERVECTOR(IDX)	attr_tbl[IDX].old.flag16
# define OLD_ATL_TOP_OF_LOOP(IDX)	attr_tbl[IDX].old.flag13
# define OLD_ATP_EXPL_ITRFC(IDX)	attr_tbl[IDX].old.flag14
# define OLD_ATP_EXTERNAL_INTRIN(IDX)	attr_tbl[IDX].old.flag16
# define OLD_ATP_EXTRA_DARG(IDX)	attr_tbl[IDX].old.flag15
# define OLD_ATP_NUM_DARGS(IDX)		attr_tbl[IDX].old.field5
# define OLD_ATP_MOD_PATH_LEN(IDX)	attr_tbl[IDX].old.field6
# define OLD_ATP_SAVE_ALL(IDX)		attr_tbl[IDX].old.flag13

/* Definitions for data object class */

# ifdef _DEBUG
# define ATD_ALIGN_SYMBOL(IDX)						       \
	((AT_OBJ_CLASS(IDX) == Data_Obj) ?				       \
		attr_tbl : sytb_var_error("ATD_ALIGN_SYMBOL", IDX))	       \
		[IDX].fld.flag21
# else
# define ATD_ALIGN_SYMBOL(IDX)		attr_tbl[IDX].fld.flag21
# endif

# ifdef _DEBUG
# define ATD_ALIGNMENT(IDX)						       \
	((AT_OBJ_CLASS(IDX) == Data_Obj) ?				       \
		attr_tbl : sytb_var_error("ATD_ALIGNMENT", IDX))	       \
		[IDX].fld2.alignment
# else
# define ATD_ALIGNMENT(IDX)		attr_tbl[IDX].fld2.alignment
# endif

# ifdef _DEBUG
# define ATD_ALLOCATABLE(IDX)						       \
	((AT_OBJ_CLASS(IDX) == Data_Obj) ?				       \
		attr_tbl : sytb_var_error("ATD_ALLOCATABLE", IDX))	       \
		[IDX].fld.flag6
# else
# define ATD_ALLOCATABLE(IDX)		attr_tbl[IDX].fld.flag6
# endif

# ifdef _DEBUG
# define ATD_ARRAY_IDX(IDX)						       \
	((AT_OBJ_CLASS(IDX) == Data_Obj) ?				       \
		attr_tbl : sytb_var_error("ATD_ARRAY_IDX", IDX))	       \
		[IDX].fld.field1
# else
# define ATD_ARRAY_IDX(IDX)		attr_tbl[IDX].fld.field1
# endif

# ifdef _DEBUG
# define ATD_ASG_TMP(IDX)					 	       \
	((AT_OBJ_CLASS(IDX) == Data_Obj &&				       \
	 attr_tbl[IDX].fld.secondary_info == Compiler_Tmp) ?		       \
		attr_tbl : sytb_var_error("ATD_ASG_TMP", IDX))	    	       \
		[IDX].fld.flag49
# else
# define ATD_ASG_TMP(IDX)		   attr_tbl[IDX].fld.flag49
# endif

# ifdef _DEBUG
# define ATD_ASSIGN_TMP_IDX(IDX)					       \
	((AT_OBJ_CLASS(IDX) == Data_Obj &&				       \
	  attr_tbl[IDX].fld.secondary_info == Variable) ?		       \
		attr_tbl : sytb_var_error("ATD_ASSIGN_TMP_IDX", IDX))	       \
		[IDX].fld.field8
# else
# define ATD_ASSIGN_TMP_IDX(IDX)	attr_tbl[IDX].fld.field8
# endif

# ifdef _DEBUG
/* This checks ATD_AUTOMATIC */
# define ATD_AUTO_BASE_IDX(IDX)						       \
	((AT_OBJ_CLASS(IDX) == Data_Obj && attr_tbl[IDX].fld.flag12) ?	       \
		attr_tbl : sytb_var_error("ATD_AUTO_BASE_IDX", IDX))	       \
		[IDX].fld.field13
# else
# define ATD_AUTO_BASE_IDX(IDX)		attr_tbl[IDX].fld.field13
# endif

# ifdef _DEBUG
# define ATD_AUTOMATIC(IDX)						       \
	((AT_OBJ_CLASS(IDX) == Data_Obj) ?				       \
		attr_tbl : sytb_var_error("ATD_AUTOMATIC", IDX))	       \
		[IDX].fld.flag12
# else
# define ATD_AUTOMATIC(IDX)		attr_tbl[IDX].fld.flag12
# endif

# ifdef _DEBUG
# define ATD_AUXILIARY(IDX)						       \
	((AT_OBJ_CLASS(IDX) == Data_Obj) ?				       \
		attr_tbl : sytb_var_error("ATD_AUXILIARY", IDX))	       \
		[IDX].fld.flag9
# else
# define ATD_AUXILIARY(IDX)		attr_tbl[IDX].fld.flag9
# endif

# ifdef _DEBUG
# define ATD_BOUNDS_CHECK(IDX)                                                 \
        ((AT_OBJ_CLASS(IDX) == Data_Obj) ?                                     \
                attr_tbl : sytb_var_error("ATD_BOUNDS_CHECK", IDX))            \
                [IDX].fld.flag31
# else
# define ATD_BOUNDS_CHECK(IDX)          attr_tbl[IDX].fld.flag31
# endif

# ifdef _DEBUG
# define ATD_CACHE_ALIGN(IDX)                                                  \
        ((AT_OBJ_CLASS(IDX) == Data_Obj) ?                                     \
                attr_tbl : sytb_var_error("ATD_CACHE_ALIGN", IDX))             \
                [IDX].fld.flag26
# else
# define ATD_CACHE_ALIGN(IDX)              attr_tbl[IDX].fld.flag26
# endif

# ifdef _DEBUG
# define ATD_CACHE_BYPASS_ARRAY(IDX)				 	       \
	((AT_OBJ_CLASS(IDX) == Data_Obj) ?				       \
		attr_tbl : sytb_var_error("ATD_CACHE_BYPASS_ARRAY", IDX))      \
		[IDX].fld.flag39
# else
# define ATD_CACHE_BYPASS_ARRAY(IDX)	attr_tbl[IDX].fld.flag39
# endif


# ifdef _DEBUG
# define ATD_CACHE_NOALLOC(IDX)					 	       \
	((AT_OBJ_CLASS(IDX) == Data_Obj) ?				       \
		attr_tbl : sytb_var_error("ATD_CACHE_NOALLOC", IDX))	       \
		[IDX].fld.flag43
# else
# define ATD_CACHE_NOALLOC(IDX)		attr_tbl[IDX].fld.flag43
# endif


# ifdef _DEBUG
# define ATD_CHAR_LEN_IN_DV(IDX)					       \
	((AT_OBJ_CLASS(IDX) == Data_Obj) ?				       \
		attr_tbl : sytb_var_error("ATD_CHAR_LEN_IN_DV", IDX))	       \
		[IDX].fld.flag11
# else
# define ATD_CHAR_LEN_IN_DV(IDX)	attr_tbl[IDX].fld.flag11
# endif


# ifdef _DEBUG
# define ATD_CLASS(IDX)							       \
	((AT_OBJ_CLASS(IDX) == Data_Obj) ?				       \
		attr_tbl : sytb_var_error("ATD_CLASS", IDX))		       \
		[IDX].fld.secondary_info
# else
# define ATD_CLASS(IDX)			attr_tbl[IDX].fld.secondary_info
# endif

# ifdef _DEBUG
# define ATD_CONST_IDX(IDX)						       \
	((AT_OBJ_CLASS(IDX) == Data_Obj &&				       \
	  attr_tbl[IDX].fld.secondary_info == Constant) ?		       \
		attr_tbl : sytb_var_error("ATD_CONST_IDX", IDX))	       \
		[IDX].fld.field4
# else
# define ATD_CONST_IDX(IDX)		attr_tbl[IDX].fld.field4
# endif

# ifdef _DEBUG
# define ATD_COPY_ASSUMED_SHAPE(IDX)                                           \
        ((AT_OBJ_CLASS(IDX) == Data_Obj) ?                                     \
                attr_tbl : sytb_var_error("ATD_COPY_ASSUMED_SHAPE", IDX))      \
                [IDX].fld.flag36
# else
# define ATD_COPY_ASSUMED_SHAPE(IDX)     attr_tbl[IDX].fld.flag36
# endif

# ifdef _DEBUG
# define ATD_CPNT_INIT_IDX(IDX)					 	       \
	((AT_OBJ_CLASS(IDX) == Data_Obj	&&				       \
	  attr_tbl[IDX].fld.secondary_info == Struct_Component) ?	       \
		attr_tbl : sytb_var_error("ATD_CPNT_INIT_IDX", IDX))	       \
		[IDX].fld.field4
# else
# define ATD_CPNT_INIT_IDX(IDX)		attr_tbl[IDX].fld.field4
# endif

# ifdef _DEBUG
# define ATD_CPNT_OFFSET_IDX(IDX)					       \
	((AT_OBJ_CLASS(IDX) == Data_Obj &&				       \
	  attr_tbl[IDX].fld.secondary_info == Struct_Component) ?	       \
		attr_tbl : sytb_var_error("ATD_CPNT_OFFSET_IDX", IDX))	       \
		[IDX].fld.field13
# else
# define ATD_CPNT_OFFSET_IDX(IDX)	attr_tbl[IDX].fld.field13
# endif

# ifdef _DEBUG
# define ATD_DATA_INIT(IDX)						       \
	((AT_OBJ_CLASS(IDX) == Data_Obj) ?				       \
		attr_tbl : sytb_var_error("ATD_DATA_INIT", IDX))	       \
		[IDX].fld.flag2
# else
# define ATD_DATA_INIT(IDX)		attr_tbl[IDX].fld.flag2
# endif

# ifdef _DEBUG
# define ATD_DCL_EQUIV(IDX)						       \
	((AT_OBJ_CLASS(IDX) == Data_Obj) ?				       \
		attr_tbl : sytb_var_error("ATD_DCL_EQUIV", IDX))	       \
		[IDX].fld.flag10
# else
# define ATD_DCL_EQUIV(IDX)		attr_tbl[IDX].fld.flag10
# endif

# ifdef _DEBUG
# define ATD_DEFINING_ATTR_IDX(IDX)					       \
	((AT_OBJ_CLASS(IDX) == Data_Obj &&				       \
	  attr_tbl[IDX].fld.secondary_info == Compiler_Tmp) ?		       \
		attr_tbl : sytb_var_error("ATD_DEFINING_ATTR_IDX", IDX))       \
		[IDX].fld.field8
# else
# define ATD_DEFINING_ATTR_IDX(IDX)	attr_tbl[IDX].fld.field8
# endif

# ifdef _DEBUG
# define ATD_DERIVED_TYPE_IDX(IDX)					       \
	((AT_OBJ_CLASS(IDX) == Data_Obj && 				       \
	  attr_tbl[IDX].fld.secondary_info == Struct_Component) ?	       \
		attr_tbl : sytb_var_error("ATD_DERIVED_TYPE_IDX", IDX))	       \
		[IDX].fld.field8
# else
# define ATD_DERIVED_TYPE_IDX(IDX)	attr_tbl[IDX].fld.field8
# endif

# ifdef _DEBUG
# define ATD_DISTRIBUTION_IDX(IDX)				 	       \
	((AT_OBJ_CLASS(IDX) == Data_Obj) ?				       \
		attr_tbl : sytb_var_error("ATD_DISTRIBUTION_IDX", IDX))	       \
		[IDX].fld2.distribution_idx
# else
# define ATD_DISTRIBUTION_IDX(IDX)	attr_tbl[IDX].fld2.distribution_idx
# endif

# ifdef _DEBUG
# define ATD_WAS_SCOPED(IDX)                                                   \
		((AT_OBJ_CLASS(IDX) == Data_Obj) ?                             \
		attr_tbl : sytb_var_error("ATD_WAS_SCOPED", IDX))              \
		[IDX].fld.flag45
# else
# define ATD_WAS_SCOPED(IDX)		attr_tbl[IDX].fld.flag45
# endif

#ifdef KEY /* Bug 14150 */
# ifdef _DEBUG
# define AT_BIND_ATTR(IDX)						       \
		((!(AT_OBJ_CLASS(IDX) == Data_Obj &&                           \
		   ATD_CLASS(IDX) == Dummy_Argument)) ?                        \
		   attr_tbl : sytb_var_error("AT_BIND_ATTR", IDX))             \
		   [IDX].fld.flag46
# else
# define AT_BIND_ATTR(IDX)		attr_tbl[IDX].fld.flag46
# endif

# ifdef _DEBUG
# define ATD_VALUE_ATTR(IDX)                                                   \
		((AT_OBJ_CLASS(IDX) == Data_Obj &&                             \
		   ATD_CLASS(IDX) == Dummy_Argument) ?                         \
		attr_tbl : sytb_var_error("AT_VALUE_ATTR", IDX))               \
		[IDX].fld.flag46
# else
# define ATD_VALUE_ATTR(IDX)		attr_tbl[IDX].fld.flag46
# endif

#endif /* KEY Bug 14150 */

# ifdef _DEBUG
# define ATD_DV_ALIAS(IDX)  						       \
        (((AT_OBJ_CLASS(IDX) == Data_Obj) && 				       \
           ((comp_phase == Inlining) || (comp_phase == Pdg_Conversion))) ?     \
                attr_aux_tbl : attr_aux_var_error("ATD_DV_ALIAS", IDX))        \
                [IDX].fld.field3
# else
# define ATD_DV_ALIAS(IDX)  	        attr_aux_tbl[IDX].fld.field3
# endif

# ifdef _DEBUG
# define ATD_DYNAMIC(IDX)    	                                               \
        ((AT_OBJ_CLASS(IDX) == Data_Obj) ?                                     \
                attr_tbl : sytb_var_error("ATD_DYNAMIC", IDX))  	       \
                [IDX].fld.flag41
# else
# define ATD_DYNAMIC(IDX)         	attr_tbl[IDX].fld.flag41
# endif

# ifdef _DEBUG
# define ATD_ELEMENTAL_CALL_TMP(IDX)					       \
	((AT_OBJ_CLASS(IDX) == Data_Obj &&				       \
	 attr_tbl[IDX].fld.secondary_info == Compiler_Tmp) ?		       \
		attr_tbl : sytb_var_error("ATD_ELEMENTAL_CALL_TMP", IDX))      \
		[IDX].fld.flag50
# else
# define ATD_ELEMENTAL_CALL_TMP(IDX)	   attr_tbl[IDX].fld.flag50
# endif


# ifdef _DEBUG
# define ATD_EQUIV(IDX)							       \
	((AT_OBJ_CLASS(IDX) == Data_Obj) ?				       \
		attr_tbl : sytb_var_error("ATD_EQUIV", IDX))		       \
		[IDX].fld.flag8
# else
# define ATD_EQUIV(IDX)			attr_tbl[IDX].fld.flag8
# endif

# ifdef _DEBUG
# define ATD_EQUIV_IN_BNDS_EXPR(IDX)				 	       \
	((AT_OBJ_CLASS(IDX) == Data_Obj) ?				       \
		attr_aux_tbl:attr_aux_var_error("ATD_EQUIV_IN_BNDS_EXPR", IDX))\
		[IDX].fld.flag5
# else
# define ATD_EQUIV_IN_BNDS_EXPR(IDX)	attr_aux_tbl[IDX].fld.flag5
# endif

# ifdef _DEBUG
# define ATD_EQUIV_LIST(IDX)						       \
	((AT_OBJ_CLASS(IDX) == Data_Obj) ?				       \
		attr_aux_tbl : attr_aux_var_error("ATD_EQUIV_LIST", IDX))      \
		[IDX].fld.field2
# else
# define ATD_EQUIV_LIST(IDX)		attr_aux_tbl[IDX].fld.field2
# endif

# ifdef _DEBUG
# define ATD_EXPR_EVAL_TMP(IDX)					 	       \
	((AT_OBJ_CLASS(IDX) == Data_Obj) ?				       \
		attr_aux_tbl:attr_aux_var_error("ATD_EXPR_EVAL_TMP", IDX))     \
		[IDX].fld.flag2
# else
# define ATD_EXPR_EVAL_TMP(IDX)		attr_aux_tbl[IDX].fld.flag2
# endif

# ifdef _DEBUG
# define ATD_FILL_SYMBOL(IDX)						       \
	((AT_OBJ_CLASS(IDX) == Data_Obj) ?				       \
		attr_tbl : sytb_var_error("ATD_FILL_SYMBOL", IDX))	       \
		[IDX].fld.flag22
# else
# define ATD_FILL_SYMBOL(IDX)		attr_tbl[IDX].fld.flag22
# endif

# ifdef _DEBUG
# define ATD_FIRST_SEEN_IL_IDX(IDX)					       \
	((AT_OBJ_CLASS(IDX) == Data_Obj) ?				       \
		attr_aux_tbl : attr_aux_var_error("ATD_FIRST_SEEN_IL_IDX",IDX))\
		[IDX].fld.field3
# else
# define ATD_FIRST_SEEN_IL_IDX(IDX)        attr_aux_tbl[IDX].fld.field3
# endif

# ifdef _DEBUG
# define ATD_FLD(IDX)                                                          \
        ((AT_OBJ_CLASS(IDX) == Data_Obj) ?                                     \
                attr_tbl : sytb_var_error("ATD_FLD", IDX))                     \
                [IDX].fld.field2
# else
# define ATD_FLD(IDX)               attr_tbl[IDX].fld.field2
# endif

# ifdef _DEBUG
# define ATD_FORALL_INDEX(IDX) 						       \
        ((AT_OBJ_CLASS(IDX) == Data_Obj) ? 				       \
                attr_aux_tbl : attr_aux_var_error("ATD_FORALL_INDEX", IDX))    \
                [IDX].fld.flag8
# else
# define ATD_FORALL_INDEX(IDX) 	        attr_aux_tbl[IDX].fld.flag8
# endif

# ifdef _DEBUG
# define ATD_FUNC_IDX(IDX)						       \
	((AT_OBJ_CLASS(IDX) == Data_Obj &&				       \
	  attr_tbl[IDX].fld.secondary_info == Function_Result) ?	       \
		attr_tbl : sytb_var_error("ATD_FUNC_IDX", IDX))		       \
		[IDX].fld.field4
# else
# define ATD_FUNC_IDX(IDX)		attr_tbl[IDX].fld.field4
# endif

# ifdef _DEBUG
#ifdef KEY /* Bug 14150 */
# define ATD_IGNORE_TKR(IDX)						       \
        ((AT_OBJ_CLASS(IDX) == Data_Obj || AT_OBJ_CLASS(IDX) == Pgm_Unit) ?    \
                attr_tbl : sytb_var_error("ATD_IGNORE_TKR", IDX))	       \
                [IDX].fld.flag48
#else /* KEY */
# define ATD_IGNORE_TKR(IDX)						       \
        ((AT_OBJ_CLASS(IDX) == Data_Obj) ?                                     \
                attr_tbl : sytb_var_error("ATD_IGNORE_TKR", IDX))	       \
                [IDX].fld.flag48
#endif /* KEY Bug 14150 */
# else
# define ATD_IGNORE_TKR(IDX)		attr_tbl[IDX].fld.flag48
# endif

# ifdef _DEBUG
# define ATD_IM_A_DOPE(IDX)                                                    \
        ((AT_OBJ_CLASS(IDX) == Data_Obj) ?                                     \
                attr_tbl : sytb_var_error("ATD_IM_A_DOPE", IDX))               \
                [IDX].fld.flag3
# else
# define ATD_IM_A_DOPE(IDX)		attr_tbl[IDX].fld.flag3
# endif

# ifdef _DEBUG
# define ATD_IMP_DO_LCV(IDX)                                                   \
        ((AT_OBJ_CLASS(IDX) == Data_Obj) ?                                     \
                attr_aux_tbl : attr_aux_var_error("ATD_IMP_DO_LCV", IDX))      \
                [IDX].fld.flag10
# else
# define ATD_IMP_DO_LCV(IDX)           attr_aux_tbl[IDX].fld.flag10
# endif

# ifdef _DEBUG
# define ATD_IN_ASSIGN(IDX)						       \
	((AT_OBJ_CLASS(IDX) == Data_Obj) ?				       \
                attr_aux_tbl : attr_aux_var_error("ATD_IN_ASSIGN", IDX))       \
		[IDX].fld.flag9
# else
# define ATD_IN_ASSIGN(IDX)		attr_aux_tbl[IDX].fld.flag9
# endif

# ifdef _DEBUG
# define ATD_IN_COMMON(IDX)						       \
	((AT_OBJ_CLASS(IDX) == Data_Obj) ?				       \
		attr_tbl : sytb_var_error("ATD_IN_COMMON", IDX))	       \
		[IDX].fld.flag7
# else
# define ATD_IN_COMMON(IDX)		attr_tbl[IDX].fld.flag7
# endif

# ifdef _DEBUG
# define ATD_INTENT(IDX)						       \
	((AT_OBJ_CLASS(IDX) == Data_Obj) ?				       \
		attr_tbl : sytb_var_error("ATD_INTENT", IDX))		       \
		[IDX].fld.field3
# else
# define ATD_INTENT(IDX)		attr_tbl[IDX].fld.field3
# endif

# ifdef _DEBUG
# define ATD_INTRIN_DARG(IDX)						       \
	((AT_OBJ_CLASS(IDX) == Data_Obj &&				       \
	  attr_tbl[IDX].fld.secondary_info == Dummy_Argument) ?		       \
		attr_tbl : sytb_var_error("ATD_INTRIN_DARG", IDX))	       \
		[IDX].fld.flag27
# else
# define ATD_INTRIN_DARG(IDX)		attr_tbl[IDX].fld.flag27
# endif

# ifdef _DEBUG
# define ATD_INTRIN_DARG_TYPE(IDX)					       \
	((AT_OBJ_CLASS(IDX) == Data_Obj &&				       \
	  attr_tbl[IDX].fld.secondary_info == Dummy_Argument) ?		       \
		attr_tbl : sytb_var_error("ATD_INTRIN_DARG_TYPE", IDX))	       \
		[IDX].fldd.field32_12
# else
# define ATD_INTRIN_DARG_TYPE(IDX)	attr_tbl[IDX].fldd.field32_12
# endif

# ifdef _DEBUG
# define ATD_LCV_IS_CONST(IDX)                                                 \
        ((AT_OBJ_CLASS(IDX) == Data_Obj) ?                                     \
                attr_tbl : sytb_var_error("ATD_LCV_IS_CONST", IDX))            \
                [IDX].fld.flag18
# else
# define ATD_LCV_IS_CONST(IDX)         attr_tbl[IDX].fld.flag18
# endif

# ifdef _DEBUG
# define ATD_LIVE_DO_VAR(IDX)						       \
        ((AT_OBJ_CLASS(IDX) == Data_Obj) ?                                     \
                attr_aux_tbl : attr_aux_var_error("ATD_LIVE_DO_VAR", IDX))     \
                [IDX].fld.flag1
# else
# define ATD_LIVE_DO_VAR(IDX)		attr_aux_tbl[IDX].fld.flag1
# endif

# ifdef _DEBUG
# define ATD_NEXT_MEMBER_IDX(IDX)				 	       \
	((AT_OBJ_CLASS(IDX) == Data_Obj	&&				       \
	  (attr_tbl[IDX].fld.secondary_info == Variable ||		       \
	   attr_tbl[IDX].fld.secondary_info == Compiler_Tmp)) ?		       \
		attr_tbl : sytb_var_error("ATD_NEXT_MEMBER_IDX", IDX))	       \
		[IDX].fld.field16
# else
# define ATD_NEXT_MEMBER_IDX(IDX)	attr_tbl[IDX].fld.field16
# endif

# ifdef _DEBUG
# define ATD_NO_ENTRY_LIST(IDX)						       \
        ((AT_OBJ_CLASS(IDX) == Data_Obj) ?                                     \
                attr_aux_tbl : attr_aux_var_error("ATD_NO_ENTRY_LIST", IDX))   \
                [IDX].fld.field1
# else
# define ATD_NO_ENTRY_LIST(IDX)		attr_aux_tbl[IDX].fld.field1
# endif

# ifdef _DEBUG
# define ATD_NOBOUNDS_CHECK(IDX)                                               \
        ((AT_OBJ_CLASS(IDX) == Data_Obj) ?                                     \
                attr_tbl : sytb_var_error("ATD_NOBOUNDS_CHECK", IDX))          \
                [IDX].fld.flag32
# else
# define ATD_NOBOUNDS_CHECK(IDX)        attr_tbl[IDX].fld.flag32
# endif

# ifdef _DEBUG
# define ATD_NOT_PT_UNIQUE_MEM(IDX)                                            \
	((AT_OBJ_CLASS(IDX) == Data_Obj) ?				       \
                attr_tbl : sytb_var_error("ATD_NOT_PT_UNIQUE_MEM", IDX))       \
                [IDX].fld.flag51
# else
# define ATD_NOT_PT_UNIQUE_MEM(IDX)       attr_tbl[IDX].fld.flag51
# endif

# ifdef _DEBUG
# define ATD_OFFSET_ASSIGNED(IDX)                                              \
        ((AT_OBJ_CLASS(IDX) == Data_Obj) ?				       \
                attr_tbl : sytb_var_error("ATD_OFFSET_ASSIGNED", IDX))         \
                [IDX].fld.flag40
# else
# define ATD_OFFSET_ASSIGNED(IDX)	attr_tbl[IDX].fld.flag40
# endif

# ifdef _DEBUG
# define ATD_OFFSET_FLD(IDX)						       \
	((AT_OBJ_CLASS(IDX) == Data_Obj &&				       \
	  (attr_tbl[IDX].fld.secondary_info == Variable ||                     \
	   attr_tbl[IDX].fld.secondary_info == Function_Result ||	       \
           attr_tbl[IDX].fld.secondary_info == Struct_Component ||             \
	   attr_tbl[IDX].fld.secondary_info == Compiler_Tmp)) ?		       \
		attr_tbl : sytb_var_error("ATD_OFFSET_FLD", IDX))	       \
		[IDX].fld2.field22
# else
# define ATD_OFFSET_FLD(IDX)		attr_tbl[IDX].fld2.field22
# endif

# ifdef _DEBUG
# define ATD_OFFSET_IDX(IDX)						       \
	((AT_OBJ_CLASS(IDX) == Data_Obj &&				       \
	  (attr_tbl[IDX].fld.secondary_info == Variable ||                     \
	   attr_tbl[IDX].fld.secondary_info == Function_Result ||	       \
	   attr_tbl[IDX].fld.secondary_info == Compiler_Tmp)) ?		       \
		attr_tbl : sytb_var_error("ATD_OFFSET_IDX", IDX))	       \
		[IDX].fld.field13
# else
# define ATD_OFFSET_IDX(IDX)		attr_tbl[IDX].fld.field13
# endif

# ifdef _DEBUG
# define ATD_PARENT_OBJECT(IDX)						       \
        ((AT_OBJ_CLASS(IDX) == Data_Obj) ? 				       \
                attr_aux_tbl : attr_aux_var_error("ATD_PARENT_OBJECT", IDX))   \
                [IDX].fld.flag7
# else
# define ATD_PARENT_OBJECT(IDX)	        attr_aux_tbl[IDX].fld.flag7
# endif

# ifdef _DEBUG
# define ATD_PE_ARRAY_IDX(IDX)                                                 \
	((AT_OBJ_CLASS(IDX) == Data_Obj) ?				       \
                attr_tbl : sytb_var_error("ATD_PE_ARRAY_IDX", IDX))            \
                [IDX].fld.field10
# else
# define ATD_PE_ARRAY_IDX(IDX)        attr_tbl[IDX].fld.field10
# endif

# ifdef _DEBUG
# define ATD_PERMUTATION(IDX)						       \
	((AT_OBJ_CLASS(IDX) == Data_Obj) ?				       \
		attr_tbl : sytb_var_error("ATD_PERMUTATION", IDX))	       \
		[IDX].fld.flag30
# else
# define ATD_PERMUTATION(IDX)		attr_tbl[IDX].fld.flag30
# endif

# ifdef _DEBUG
# define ATD_POINTER(IDX)						       \
	((AT_OBJ_CLASS(IDX) == Data_Obj) ?				       \
		attr_tbl : sytb_var_error("ATD_POINTER", IDX))		       \
		[IDX].fld.flag5
# else
# define ATD_POINTER(IDX)		attr_tbl[IDX].fld.flag5
# endif

# ifdef _DEBUG
# define ATD_PTR_ASSIGNED(IDX)	     	 	   			       \
	((AT_OBJ_CLASS(IDX) == Data_Obj) ?				       \
		attr_tbl : sytb_var_error("ATD_PTR_ASSIGNED", IDX))	       \
		[IDX].fld.flag20
# else
# define ATD_PTR_ASSIGNED(IDX)		attr_tbl[IDX].fld.flag20
# endif

# ifdef _DEBUG
# define ATD_PTR_HALF_WORD(IDX)						       \
        ((AT_OBJ_CLASS(IDX) == Data_Obj) ?                                     \
                attr_aux_tbl : attr_aux_var_error("ATD_PTR_HALF_WORD", IDX))   \
                [IDX].fld.flag3
# else
# define ATD_PTR_HALF_WORD(IDX)		attr_aux_tbl[IDX].fld.flag3
# endif

# ifdef _DEBUG
# define ATD_PTR_IDX(IDX)						       \
	((AT_OBJ_CLASS(IDX) == Data_Obj &&				       \
	  attr_tbl[IDX].fld.secondary_info == CRI__Pointee) ?		       \
		attr_tbl : sytb_var_error("ATD_PTR_IDX", IDX))		       \
		[IDX].fld.field8
# else
# define ATD_PTR_IDX(IDX)		attr_tbl[IDX].fld.field8
# endif

# ifdef _DEBUG
# define ATD_PTR_TYPE_SET(IDX)					 	       \
	((AT_OBJ_CLASS(IDX) == Data_Obj) ?				       \
		attr_aux_tbl : attr_aux_var_error("ATD_PTR_TYPE_SET", IDX))    \
		[IDX].fld.flag4
# else
# define ATD_PTR_TYPE_SET(IDX)		attr_aux_tbl[IDX].fld.flag4
# endif

# ifdef _DEBUG
# define ATD_PURE(IDX)						 	       \
	((AT_OBJ_CLASS(IDX) == Data_Obj) ?				       \
		attr_tbl : sytb_var_error("ATD_PURE", IDX))		       \
		[IDX].fld.flag38
# else
# define ATD_PURE(IDX)		attr_tbl[IDX].fld.flag38
# endif

# ifdef _DEBUG
# define ATD_READ_ONLY_VAR(IDX)                                                \
	((AT_OBJ_CLASS(IDX) == Data_Obj) ?				       \
                attr_tbl : sytb_var_error("ATD_READ_ONLY_VAR", IDX))           \
                [IDX].fld.flag52
# else
# define ATD_READ_ONLY_VAR(IDX)           attr_tbl[IDX].fld.flag52
# endif

# ifdef KEY
# ifdef _DEBUG
# define ATD_F2C_ABI_VAR(IDX)                                                \
	((AT_OBJ_CLASS(IDX) == Data_Obj || AT_OBJ_CLASS(IDX) == Pgm_Unit) ?				       \
                attr_tbl : sytb_var_error("ATD_F2C_ABI_VAR", IDX))           \
                [IDX].fld.flag57
# else
# define ATD_F2C_ABI_VAR(IDX)           attr_tbl[IDX].fld.flag57
# endif
# endif

# ifdef _DEBUG
# define ATD_SAVED(IDX)						 	       \
	((AT_OBJ_CLASS(IDX) == Data_Obj) ?				       \
		attr_tbl : sytb_var_error("ATD_SAVED", IDX))		       \
		[IDX].fld.flag4
# else
# define ATD_SAVED(IDX)			attr_tbl[IDX].fld.flag4
# endif

# ifdef _DEBUG
# define ATD_SECTION_GP(IDX)					 	       \
	((AT_OBJ_CLASS(IDX) == Data_Obj) ?				       \
		attr_tbl : sytb_var_error("ATD_SECTION_GP", IDX))	       \
		[IDX].fld.flag19
# else
# define ATD_SECTION_GP(IDX)		attr_tbl[IDX].fld.flag19
# endif

# ifdef _DEBUG
# define ATD_SECTION_NON_GP(IDX)				 	       \
	((AT_OBJ_CLASS(IDX) == Data_Obj) ?				       \
		attr_tbl : sytb_var_error("ATD_SECTION_NON_GP", IDX))	       \
		[IDX].fld.flag29
# else
# define ATD_SECTION_NON_GP(IDX)	attr_tbl[IDX].fld.flag29
# endif


# ifdef _DEBUG
# define ATD_SF_ARG_IDX(IDX)					 	       \
	((AT_OBJ_CLASS(IDX) == Data_Obj &&				       \
	  attr_tbl[IDX].fld.secondary_info == Dummy_Argument) ?		       \
		attr_tbl : sytb_var_error("ATD_SF_ARG_IDX", IDX))	       \
		[IDX].fld.field4
# else
# define ATD_SF_ARG_IDX(IDX)		attr_tbl[IDX].fld.field4
# endif

# ifdef _DEBUG
# define ATD_SF_DARG(IDX)						       \
	((AT_OBJ_CLASS(IDX) == Data_Obj &&				       \
	  attr_tbl[IDX].fld.secondary_info == Dummy_Argument) ?		       \
		attr_tbl : sytb_var_error("ATD_SF_DARG", IDX))		       \
		[IDX].fld.flag28
# else
# define ATD_SF_DARG(IDX)		attr_tbl[IDX].fld.flag28
# endif

# ifdef _DEBUG
# define ATD_SF_LINK(IDX)						       \
	((AT_OBJ_CLASS(IDX) == Data_Obj &&				       \
	  attr_tbl[IDX].fld.secondary_info == Dummy_Argument) ?		       \
		attr_tbl : sytb_var_error("ATD_SF_LINK", IDX))		       \
		[IDX].fld.field13
# else
# define ATD_SF_LINK(IDX)		attr_tbl[IDX].fld.field13
# endif

# ifdef _DEBUG
# define ATD_STACK(IDX)						 	       \
	((AT_OBJ_CLASS(IDX) == Data_Obj) ?				       \
		attr_tbl : sytb_var_error("ATD_STACK", IDX))		       \
		[IDX].fld.flag37
# else
# define ATD_STACK(IDX)			attr_tbl[IDX].fld.flag37
# endif

# ifdef _DEBUG
# define ATD_STOR_BLK_IDX(IDX)                                                 \
        ((AT_OBJ_CLASS(IDX) == Data_Obj) ?                                     \
                attr_tbl : sytb_var_error("ATD_STOR_BLK_IDX", IDX))            \
                [IDX].fld.field6
# else
# define ATD_STOR_BLK_IDX(IDX)          attr_tbl[IDX].fld.field6
# endif

# ifdef _DEBUG
# define ATD_RESHAPE_ARRAY_IDX(IDX)				 	       \
	((AT_OBJ_CLASS(IDX) == Data_Obj) ?				       \
		attr_tbl : sytb_var_error("ATD_RESHAPE_ARRAY_IDX", IDX))       \
		[IDX].fld.field7
# else
# define ATD_RESHAPE_ARRAY_IDX(IDX)	attr_tbl[IDX].fld.field7
# endif

# ifdef _DEBUG
# define ATD_SYMMETRIC(IDX)					 	       \
	((AT_OBJ_CLASS(IDX) == Data_Obj) ?				       \
		attr_tbl : sytb_var_error("ATD_SYMMETRIC", IDX))	       \
		[IDX].fld.flag33
# else
# define ATD_SYMMETRIC(IDX)		attr_tbl[IDX].fld.flag33
# endif

# ifdef _DEBUG
# define ATD_RESHAPE_ARRAY_OPT(IDX)				 	       \
	((AT_OBJ_CLASS(IDX) == Data_Obj) ?				       \
		attr_tbl : sytb_var_error("ATD_RESHAPE_ARRAY_OPT", IDX))      \
		[IDX].fld.flag17
# else
# define ATD_RESHAPE_ARRAY_OPT(IDX)	attr_tbl[IDX].fld.flag17
# endif

/* This flag is overlayed with ATD_TMP_NEEDS_CIF.  This flag is only          */
/* pertinent to variables but we don't need to check for that because it can  */
/* never be set for a temp because temps are created by the compiler.         */

# ifdef _DEBUG
# define ATD_SEEN_AS_IO_LCV(IDX)                                               \
        ((AT_OBJ_CLASS(IDX) == Data_Obj) ?                                     \
                attr_tbl : sytb_var_error("ATD_SEEN_AS_IO_LCV", IDX))          \
                [IDX].fld.flag34
# else
# define ATD_SEEN_AS_IO_LCV(IDX)         attr_tbl[IDX].fld.flag34
# endif

# ifdef _DEBUG
# define ATD_SEEN_AS_LCV(IDX)						       \
        ((AT_OBJ_CLASS(IDX) == Data_Obj) ?                                     \
		attr_tbl : sytb_var_error("ATD_SEEN_AS_LCV", IDX))	       \
		[IDX].fld.flag25
# else
# define ATD_SEEN_AS_LCV(IDX)             attr_tbl[IDX].fld.flag25
# endif

# ifdef _DEBUG
# define ATD_SEEN_IN_IMP_DO(IDX) 					       \
        ((AT_OBJ_CLASS(IDX) == Data_Obj) ?                                     \
		attr_tbl : sytb_var_error("ATD_SEEN_IN_IMP_DO", IDX))	       \
		[IDX].fld.flag23
# else
# define ATD_SEEN_IN_IMP_DO(IDX)              attr_tbl[IDX].fld.flag23
# endif

# ifdef _DEBUG
# define ATD_SEEN_OUTSIDE_IMP_DO(IDX)   				       \
        ((AT_OBJ_CLASS(IDX) == Data_Obj) ?                                     \
		attr_tbl : sytb_var_error("ATD_SEEN_OUTSIDE_IMP_DO",IDX))      \
		[IDX].fld.flag24
# else
# define ATD_SEEN_OUTSIDE_IMP_DO(IDX)         attr_tbl[IDX].fld.flag24
# endif

# ifdef _DEBUG
# define ATD_SYMBOLIC_CONSTANT(IDX)				 	       \
        ((AT_OBJ_CLASS(IDX) == Data_Obj) ?                                     \
		attr_tbl : sytb_var_error("ATD_SYMBOLIC_CONSTANT", IDX))       \
		[IDX].fld.flag35
# else
# define ATD_SYMBOLIC_CONSTANT(IDX)	attr_tbl[IDX].fld.flag35
# endif

# ifdef _DEBUG
# define ATD_TARGET(IDX)					 	       \
	((AT_OBJ_CLASS(IDX) == Data_Obj) ?				       \
		attr_tbl : sytb_var_error("ATD_TARGET", IDX))		       \
		[IDX].fld.flag1
# else
# define ATD_TARGET(IDX)		attr_tbl[IDX].fld.flag1
# endif

# ifdef _DEBUG
# define ATD_TASK_PRIVATE(IDX)                                                 \
        ((AT_OBJ_CLASS(IDX) == Data_Obj) ?                                     \
		attr_tbl:sytb_var_error("ATD_TASK_PRIVATE", IDX))      \
                [IDX].fld.flag58
# else
# define ATD_TASK_PRIVATE(IDX)           attr_tbl[IDX].fld.flag58
# endif

# ifdef _DEBUG
# define ATD_TASK_COPYIN(IDX)                                                  \
        ((AT_OBJ_CLASS(IDX) == Data_Obj) ?                                     \
		attr_aux_tbl:attr_aux_var_error("ATD_TASK_COPYIN", IDX))       \
                [IDX].fld.flag18
# else
# define ATD_TASK_COPYIN(IDX)             attr_aux_tbl[IDX].fld.flag18
# endif

# ifdef _DEBUG
# define ATD_TASK_GETFIRST(IDX)                                                \
        ((AT_OBJ_CLASS(IDX) == Data_Obj) ?                                     \
		attr_aux_tbl:attr_aux_var_error("ATD_TASK_GETFIRST", IDX))     \
                [IDX].fld.flag13
# else
# define ATD_TASK_GETFIRST(IDX)         attr_aux_tbl[IDX].fld.flag13
# endif

# ifdef _DEBUG
# define ATD_TASK_FIRSTPRIVATE(IDX)                                            \
        ((AT_OBJ_CLASS(IDX) == Data_Obj) ?                                     \
		attr_aux_tbl:attr_aux_var_error("ATD_TASK_FIRSTPRIVATE", IDX)) \
                [IDX].fld.flag17
# else
# define ATD_TASK_FIRSTPRIVATE(IDX)       attr_aux_tbl[IDX].fld.flag17
# endif

# ifdef _DEBUG
# define ATD_TASK_LASTLOCAL(IDX)                                               \
        ((AT_OBJ_CLASS(IDX) == Data_Obj) ?                                     \
		attr_aux_tbl:attr_aux_var_error("ATD_TASK_LASTLOCAL", IDX))    \
                [IDX].fld.flag14
# else
# define ATD_TASK_LASTLOCAL(IDX)         attr_aux_tbl[IDX].fld.flag14
# endif

# ifdef _DEBUG
# define ATD_TASK_LASTPRIVATE(IDX)                                             \
        ((AT_OBJ_CLASS(IDX) == Data_Obj) ?                                     \
		attr_aux_tbl:attr_aux_var_error("ATD_TASK_LASTPRIVATE", IDX))  \
                [IDX].fld.flag19
# else
# define ATD_TASK_LASTPRIVATE(IDX)        attr_aux_tbl[IDX].fld.flag19
# endif

/* the following is added by jhs, 02/7/22*/
# ifdef _DEBUG
# define ATD_TASK_COPYPRIVATE(IDX)                                             \
        ((AT_OBJ_CLASS(IDX) == Data_Obj) ?                                     \
		attr_aux_tbl:attr_aux_var_error("ATD_TASK_COPYPRIVATE", IDX))  \
                [IDX].fld.flag20
# else
# define ATD_TASK_COPYPRIVATE(IDX)        attr_aux_tbl[IDX].fld.flag20
# endif
/* the above is added by jhs, 02/7/22*/
# ifdef _DEBUG
# define ATD_TASK_LASTTHREAD(IDX)                                              \
        ((AT_OBJ_CLASS(IDX) == Data_Obj) ?                                     \
		attr_aux_tbl:attr_aux_var_error("ATD_TASK_LASTTHREAD", IDX))   \
                [IDX].fld.flag16
# else
# define ATD_TASK_LASTTHREAD(IDX)         attr_aux_tbl[IDX].fld.flag16
# endif

/* Module elements can be private or shared, see bug 686.
 * ATD_TASK_PRIVATE is now a field of an element of attr_tbl (not
 * attr_aux_tbl) since attr_aux_tbl is not exported in modules.
 */

# ifdef _DEBUG
# define ATD_TASK_REDUCTION(IDX)                                               \
        ((AT_OBJ_CLASS(IDX) == Data_Obj) ?                                     \
		attr_aux_tbl:attr_aux_var_error("ATD_TASK_REDUCTION", IDX))    \
                [IDX].fld.flag15
# else
# define ATD_TASK_REDUCTION(IDX)         attr_aux_tbl[IDX].fld.flag15
# endif

# ifdef _DEBUG
# define ATD_TASK_SHARED(IDX)                                                  \
        ((AT_OBJ_CLASS(IDX) == Data_Obj) ?                                     \
		attr_aux_tbl:attr_aux_var_error("ATD_TASK_SHARED", IDX))       \
                [IDX].fld.flag11
# else
# define ATD_TASK_SHARED(IDX)                attr_aux_tbl[IDX].fld.flag11
# endif

# ifdef _DEBUG
# define ATD_TMP_GEN_ZERO(IDX)					 	       \
	((AT_OBJ_CLASS(IDX) == Data_Obj &&				       \
	 attr_tbl[IDX].fld.secondary_info == Compiler_Tmp) ?		       \
		attr_tbl : sytb_var_error("ATD_TMP_GEN_ZERO", IDX))	       \
		[IDX].fld.flag28
# else
# define ATD_TMP_GEN_ZERO(IDX)		   attr_tbl[IDX].fld.flag28
# endif

# ifdef _DEBUG
# define ATD_TMP_HAS_CVRT_OPR(IDX)				 	       \
	((AT_OBJ_CLASS(IDX) == Data_Obj) ?				       \
		attr_aux_tbl:attr_aux_var_error("ATD_TMP_HAS_CVRT_OPR", IDX))  \
		[IDX].fld.flag6
# else
# define ATD_TMP_HAS_CVRT_OPR(IDX)	attr_aux_tbl[IDX].fld.flag6
# endif

# ifdef _DEBUG
# define ATD_TMP_IDX(IDX)						       \
	((AT_OBJ_CLASS(IDX) == Data_Obj &&				       \
	  attr_tbl[IDX].fld.secondary_info == Compiler_Tmp) ?		       \
		attr_tbl : sytb_var_error("ATD_TMP_IDX", IDX))		       \
		[IDX].fld.field4
# else
# define ATD_TMP_IDX(IDX)		attr_tbl[IDX].fld.field4
# endif

# ifdef _DEBUG
# define ATD_TMP_INIT_NOT_DONE(IDX)                                            \
        ((AT_OBJ_CLASS(IDX) == Data_Obj &&                                     \
          attr_tbl[IDX].fld.secondary_info == Compiler_Tmp) ?                  \
                attr_tbl : sytb_var_error("ATD_TMP_INIT_NOT_DONE", IDX))       \
                [IDX].fld.flag26
# else
# define ATD_TMP_INIT_NOT_DONE(IDX)     attr_tbl[IDX].fld.flag26
# endif

# ifdef _DEBUG
# define ATD_TMP_NEEDS_CIF(IDX)					 	       \
        ((AT_OBJ_CLASS(IDX) == Data_Obj &&                                     \
          attr_tbl[IDX].fld.secondary_info == Compiler_Tmp) ?                  \
		attr_tbl : sytb_var_error("ATD_TMP_NEEDS_CIF", IDX))	       \
		[IDX].fld.flag34
# else
# define ATD_TMP_NEEDS_CIF(IDX)		attr_tbl[IDX].fld.flag34
# endif

# ifdef _DEBUG
# define ATD_TMP_SEMANTICS_DONE(IDX)					       \
	((AT_OBJ_CLASS(IDX) == Data_Obj &&				       \
	  attr_tbl[IDX].fld.secondary_info == Compiler_Tmp) ?		       \
		attr_tbl : sytb_var_error("ATD_TMP_SEMANTICS_DONE", IDX))      \
		[IDX].fld.flag27
# else
# define ATD_TMP_SEMANTICS_DONE(IDX)	attr_tbl[IDX].fld.flag27
# endif

# ifdef _DEBUG
# define ATD_TOO_BIG_FOR_DV(IDX)                                               \
		((AT_OBJ_CLASS(IDX) == Data_Obj) ?                             \
		attr_tbl : sytb_var_error("ATD_TOO_BIG_FOR_DV", IDX))          \
		[IDX].fld.flag44
# else
# define ATD_TOO_BIG_FOR_DV(IDX)	attr_tbl[IDX].fld.flag44
# endif


# ifdef _DEBUG
# define ATD_TYPE_IDX(IDX)					 	       \
	((AT_OBJ_CLASS(IDX) == Data_Obj ||				       \
	  AT_OBJ_CLASS(IDX) == Interface ||				       \
	  AT_OBJ_CLASS(IDX) == Stmt_Func) ?				       \
		attr_tbl : sytb_var_error("ATD_TYPE_IDX", IDX))		       \
		[IDX].fld.field5
# else
# define ATD_TYPE_IDX(IDX)		attr_tbl[IDX].fld.field5
# endif

# ifdef _DEBUG
# define ATD_VARIABLE_TMP_IDX(IDX)				 	       \
	((AT_OBJ_CLASS(IDX) == Data_Obj	&&				       \
	  attr_tbl[IDX].fld.secondary_info == Variable) ?		       \
		attr_tbl : sytb_var_error("ATD_VARIABLE_TMP_IDX", IDX))	       \
		[IDX].fld.field4
# else
# define ATD_VARIABLE_TMP_IDX(IDX)	attr_tbl[IDX].fld.field4
# endif

# ifdef _DEBUG
# define ATD_VOLATILE(IDX)					 	       \
	((AT_OBJ_CLASS(IDX) == Data_Obj) ?				       \
		attr_tbl : sytb_var_error("ATD_VOLATILE", IDX))		       \
		[IDX].fld.flag42
# else
# define ATD_VOLATILE(IDX)		attr_tbl[IDX].fld.flag42
# endif


/* Definitions for interface class */

# ifdef _DEBUG
# define ATI_CIF_SCOPE_ID(IDX) 						       \
	((AT_OBJ_CLASS(IDX) == Interface) ?				       \
		attr_aux_tbl : attr_aux_var_error("ATI_CIF_SCOPE_ID", IDX))    \
		[IDX].fld.field3
# else
# define ATI_CIF_SCOPE_ID(IDX)	attr_aux_tbl[IDX].fld.field3
# endif

# ifdef _DEBUG
# define ATI_CIF_SEEN_IN_CALL(IDX)                                             \
        ((AT_OBJ_CLASS(IDX) == Interface) ?                                    \
                attr_tbl : sytb_var_error("ATI_CIF_SEEN_IN_CALL", IDX))        \
                [IDX].fld.flag5
# else
# define ATI_CIF_SEEN_IN_CALL(IDX)        attr_tbl[IDX].fld.flag5
# endif

# ifdef _DEBUG
# define ATI_DCL_INTRINSIC(IDX)						       \
	((AT_OBJ_CLASS(IDX) == Interface) ?				       \
		attr_tbl : sytb_var_error("ATI_DCL_INTRINSIC", IDX))	       \
		[IDX].fld.flag3
# else
# define ATI_DCL_INTRINSIC(IDX)		attr_tbl[IDX].fld.flag3
# endif

# ifdef _DEBUG
# define ATI_DEFINED_OPR(IDX)                                                  \
        ((AT_OBJ_CLASS(IDX) == Interface) ?                                    \
                attr_tbl : sytb_var_error("ATI_DEFINED_OPR", IDX))             \
		[IDX].fld.field6
# else
# define ATI_DEFINED_OPR(IDX)           attr_tbl[IDX].fld.field6
# endif

# ifdef _DEBUG
# define ATI_FIRST_SPECIFIC_IDX(IDX)					       \
	((AT_OBJ_CLASS(IDX) == Interface) ?				       \
		attr_tbl : sytb_var_error("ATI_FIRST_SPECIFIC_IDX", IDX))      \
		[IDX].fld.field10
# else
# define ATI_FIRST_SPECIFIC_IDX(IDX)	attr_tbl[IDX].fld.field10
# endif

# ifdef _DEBUG
# define ATI_GENERIC_INTRINSIC(IDX)	         			       \
	((AT_OBJ_CLASS(IDX) == Interface) ?				       \
		attr_tbl : sytb_var_error("ATI_GENERIC_INTRINSIC", IDX))      \
		[IDX].fld.flag7
# else
# define ATI_GENERIC_INTRINSIC(IDX)	attr_tbl[IDX].fld.flag7
# endif

# ifdef _DEBUG
# define ATI_HAS_NON_MOD_PROC(IDX)	         			       \
	((AT_OBJ_CLASS(IDX) == Interface) ?				       \
		attr_tbl : sytb_var_error("ATI_HAS_NON_MOD_PROC", IDX))	       \
		[IDX].fld.flag6
# else
# define ATI_HAS_NON_MOD_PROC(IDX)	attr_tbl[IDX].fld.flag6
# endif

# ifdef _DEBUG
# define ATI_INLINE_ALWAYS(IDX)						       \
	((AT_OBJ_CLASS(IDX) == Interface) ?				       \
		attr_tbl : sytb_var_error("ATI_INLINE_ALWAYS", IDX))	       \
		[IDX].fld.flag8
# else
# define ATI_INLINE_ALWAYS(IDX)		attr_tbl[IDX].fld.flag8
# endif

# ifdef _DEBUG
# define ATI_INLINE_NEVER(IDX)						       \
	((AT_OBJ_CLASS(IDX) == Interface) ?				       \
		attr_tbl : sytb_var_error("ATI_INLINE_NEVER", IDX))	       \
		[IDX].fld.flag9
# else
# define ATI_INLINE_NEVER(IDX)		attr_tbl[IDX].fld.flag9
# endif

# ifdef _DEBUG
# define ATI_IPA_DIR_SPECIFIED(IDX)					       \
	((AT_OBJ_CLASS(IDX) == Interface) ?				       \
		attr_tbl : sytb_var_error("ATI_IPA_DIR_SPECIFIED", IDX))       \
		[IDX].fld.flag12
# else
# define ATI_IPA_DIR_SPECIFIED(IDX)	attr_tbl[IDX].fld.flag12
# endif

# ifdef _DEBUG
# define ATI_INTERFACE_CLASS(IDX)					       \
	((AT_OBJ_CLASS(IDX) == Interface) ?				       \
		attr_tbl : sytb_var_error("ATI_INTERFACE_CLASS", IDX))	       \
		[IDX].fld.secondary_info
# else
# define ATI_INTERFACE_CLASS(IDX)	attr_tbl[IDX].fld.secondary_info
# endif

# ifdef _DEBUG
# define ATI_INTRIN_PASSABLE(IDX)	         			       \
	((AT_OBJ_CLASS(IDX) == Interface) ?				       \
		attr_tbl : sytb_var_error("ATI_INTRIN_PASSABLE", IDX))	       \
		[IDX].fld.flag1
# else
# define ATI_INTRIN_PASSABLE(IDX)	attr_tbl[IDX].fld.flag1
# endif

# ifdef _DEBUG
# define ATI_INTRIN_TBL_IDX(IDX)					       \
	((AT_OBJ_CLASS(IDX) == Interface) ?				       \
		attr_tbl : sytb_var_error("ATI_INTRIN_TBL_IDX", IDX))	       \
		[IDX].fld.field13
# else
# define ATI_INTRIN_TBL_IDX(IDX)		attr_tbl[IDX].fld.field13
# endif

# ifdef _DEBUG
# define ATI_NUM_SPECIFICS(IDX)						       \
        ((AT_OBJ_CLASS(IDX) == Interface) ?                                    \
		attr_tbl : sytb_var_error("ATI_NUM_SPECIFICS", IDX))	       \
		[IDX].fld.field14
# else
# define ATI_NUM_SPECIFICS(IDX)		attr_tbl[IDX].fld.field14
# endif

# ifdef _DEBUG
# define ATI_PROC_IDX(IDX)						       \
        ((AT_OBJ_CLASS(IDX) == Interface) ?				       \
                attr_tbl : sytb_var_error("ATI_PROC_IDX", IDX))		       \
                [IDX].fld.field8
# else
# define ATI_PROC_IDX(IDX)           attr_tbl[IDX].fld.field8
# endif

# ifdef _DEBUG
# define ATI_SGI_ROUTINE_INLINE(IDX)					       \
	((AT_OBJ_CLASS(IDX) == Interface) ?				       \
		attr_tbl : sytb_var_error("ATI_SGI_ROUTINE_INLINE", IDX))      \
		[IDX].fld.flag10
# else
# define ATI_SGI_ROUTINE_INLINE(IDX)	attr_tbl[IDX].fld.flag10
# endif

# ifdef _DEBUG
# define ATI_SGI_ROUTINE_NOINLINE(IDX)					       \
	((AT_OBJ_CLASS(IDX) == Interface) ?				       \
		attr_tbl : sytb_var_error("ATI_SGI_ROUTINE_NOINLINE", IDX))    \
		[IDX].fld.flag11
# else
# define ATI_SGI_ROUTINE_NOINLINE(IDX)	attr_tbl[IDX].fld.flag11
# endif

# ifdef _DEBUG
# define ATI_UNNAMED_INTERFACE(IDX)	         			       \
	((AT_OBJ_CLASS(IDX) == Interface) ?				       \
		attr_tbl : sytb_var_error("ATI_UNNAMED_INTERFACE", IDX))       \
		[IDX].fld.flag2
# else
# define ATI_UNNAMED_INTERFACE(IDX)	attr_tbl[IDX].fld.flag2
# endif


# ifdef _DEBUG
# define ATI_USER_SPECIFIED(IDX)	         			       \
	((AT_OBJ_CLASS(IDX) == Interface) ?				       \
		attr_tbl : sytb_var_error("ATI_USER_SPECIFIED", IDX))          \
		[IDX].fld.flag4
# else
# define ATI_USER_SPECIFIED(IDX)	attr_tbl[IDX].fld.flag4
# endif


/* Definitions for label class */

# ifdef _DEBUG
# define ATL_AGGRESSIVEINNERLOOPFISSION(IDX)			 	       \
	((AT_OBJ_CLASS(IDX) == Label) ?					       \
		attr_tbl:sytb_var_error("ATL_AGGRESSIVEINNERLOOPFISSION",IDX)) \
		[IDX].fld.flag31
# else
# define ATL_AGGRESSIVEINNERLOOPFISSION(IDX)	attr_tbl[IDX].fld.flag31
# endif

# ifdef _DEBUG
# define ATL_ALIGN(IDX)					 	               \
	((AT_OBJ_CLASS(IDX) == Label) ?					       \
		attr_tbl : sytb_var_error("ATL_ALIGN", IDX))	               \
		[IDX].fld.flag12
# else
# define ATL_ALIGN(IDX)	 		attr_tbl[IDX].fld.flag12
# endif

# ifdef _DEBUG
# define ATL_ASG_LBL_CHAIN_START(IDX)			 	               \
	((AT_OBJ_CLASS(IDX) == Label) ?					       \
		attr_tbl : sytb_var_error("ATL_ASG_LBL_CHAIN_START", IDX))     \
		[IDX].fld.flag21
# else
# define ATL_ASG_LBL_CHAIN_START(IDX)	attr_tbl[IDX].fld.flag21
# endif

# ifdef _DEBUG
# define ATL_BL(IDX)					 	               \
	((AT_OBJ_CLASS(IDX) == Label) ?					       \
		attr_tbl : sytb_var_error("ATL_BL", IDX))	               \
		[IDX].fld.flag11
# else
# define ATL_BL(IDX)	 		attr_tbl[IDX].fld.flag11
# endif

# ifdef _DEBUG
# define ATL_BLK_STMT_IDX(IDX)					 	       \
	((AT_OBJ_CLASS(IDX) == Label &&					       \
	  attr_tbl[IDX].fld.secondary_info <= Lbl_User) ?		       \
		attr_tbl : sytb_var_error("ATL_BLK_STMT_IDX", IDX))	       \
		[IDX].fld.field10
# else
# define ATL_BLK_STMT_IDX(IDX)		attr_tbl[IDX].fld.field10
# endif

# ifdef _DEBUG
# define ATL_CLASS(IDX)					 		       \
	((AT_OBJ_CLASS(IDX) == Label) ?					       \
		attr_tbl : sytb_var_error("ATL_CLASS", IDX))		       \
		[IDX].fld.secondary_info
# else
# define ATL_CLASS(IDX)			attr_tbl[IDX].fld.secondary_info
# endif

# ifdef _DEBUG
# define ATL_CMIC_BLK_STMT_IDX(IDX)                                            \
        ((AT_OBJ_CLASS(IDX) == Label &&                                        \
	  attr_tbl[IDX].fld.secondary_info != Lbl_Format) ?		       \
                attr_tbl : sytb_var_error("ATL_CMIC_BLK_STMT_IDX", IDX))       \
                [IDX].fld.field4
# else
# define ATL_CMIC_BLK_STMT_IDX(IDX)        attr_tbl[IDX].fld.field4
# endif

# ifdef _DEBUG
# define ATL_CONCURRENT(IDX)					 	       \
	((AT_OBJ_CLASS(IDX) == Label) ?					       \
		attr_tbl : sytb_var_error("ATL_CONCURRENT", IDX))	       \
		[IDX].fld.flag33
# else
# define ATL_CONCURRENT(IDX)		attr_tbl[IDX].fld.flag33
# endif

# ifdef _DEBUG
# define ATL_CONSTRUCTOR_LOOP(IDX)				 	       \
	((AT_OBJ_CLASS(IDX) == Label) ?					       \
		attr_tbl : sytb_var_error("ATL_CONSTRUCTOR_LOOP", IDX))	       \
		[IDX].fld.flag34
# else
# define ATL_CONSTRUCTOR_LOOP(IDX)	attr_tbl[IDX].fld.flag34
# endif

# ifdef _DEBUG
# define ATL_CNCALL(IDX)					 	       \
	((AT_OBJ_CLASS(IDX) == Label) ?					       \
		attr_tbl : sytb_var_error("ATL_CNCALL", IDX))		       \
		[IDX].fld.flag18
# else
# define ATL_CNCALL(IDX)		attr_tbl[IDX].fld.flag18
# endif

# ifdef _DEBUG
# define ATL_CYCLE_LBL(IDX)					 	       \
	((AT_OBJ_CLASS(IDX) == Label) ?					       \
		attr_tbl : sytb_var_error("ATL_CYCLE_LBL", IDX))               \
		[IDX].fld.flag38
# else
# define ATL_CYCLE_LBL(IDX)		attr_tbl[IDX].fld.flag38
# endif

# ifdef _DEBUG
# define ATL_DEBUG_CLASS(IDX)					 	       \
	((AT_OBJ_CLASS(IDX) == Label) ?					       \
		attr_tbl : sytb_var_error("ATL_DEBUG_CLASS", IDX))	       \
		[IDX].fld.field2
# else
# define ATL_DEBUG_CLASS(IDX)			attr_tbl[IDX].fld.field2
# endif

# ifdef _DEBUG
# define ATL_DEF_STMT_IDX(IDX)					 	       \
	((AT_OBJ_CLASS(IDX) == Label &&	AT_DEFINED(IDX)) ?		       \
		attr_tbl : sytb_var_error("ATL_DEF_STMT_IDX", IDX))	       \
		[IDX].fld.field13
# else
# define ATL_DEF_STMT_IDX(IDX)		attr_tbl[IDX].fld.field13
# endif

# ifdef _DEBUG
# define ATL_CASE_LABEL(IDX)					 	       \
	((AT_OBJ_CLASS(IDX) == Label) ?					       \
		attr_tbl : sytb_var_error("ATL_CASE_LABEL", IDX))	       \
		[IDX].fld.flag25
# else
# define ATL_CASE_LABEL(IDX)		attr_tbl[IDX].fld.flag25
# endif

# ifdef _DEBUG
# define ATL_DIRECTIVE_LIST(IDX)			 	               \
	((AT_OBJ_CLASS(IDX) == Label) ?					       \
		attr_tbl : sytb_var_error("ATL_DIRECTIVE_LIST", IDX))          \
		[IDX].fld.field14
# else
# define ATL_DIRECTIVE_LIST(IDX)	attr_tbl[IDX].fld.field14
# endif

# ifdef _DEBUG
# define ATL_EXECUTABLE(IDX)					 	       \
	((AT_OBJ_CLASS(IDX) == Label) ?					       \
		attr_tbl : sytb_var_error("ATL_EXECUTABLE", IDX))	       \
		[IDX].fld.flag2
# else
# define ATL_EXECUTABLE(IDX)		attr_tbl[IDX].fld.flag2
# endif

# ifdef _DEBUG
# define ATL_FISSIONABLE(IDX)					 	       \
	((AT_OBJ_CLASS(IDX) == Label) ?					       \
		attr_tbl : sytb_var_error("ATL_FISSIONABLE", IDX))	       \
		[IDX].fld.flag26
# else
# define ATL_FISSIONABLE(IDX)		attr_tbl[IDX].fld.flag26
# endif

# ifdef _DEBUG
# define ATL_FORMAT_TMP(IDX)					 	       \
	((AT_OBJ_CLASS(IDX) == Label &&					       \
	  attr_tbl[IDX].fld.secondary_info == Lbl_Format) ?		       \
		attr_tbl : sytb_var_error("ATL_FORMAT_TMP", IDX))	       \
		[IDX].fld.field10
# else
# define ATL_FORMAT_TMP(IDX)		attr_tbl[IDX].fld.field10
# endif

# ifdef _DEBUG
# define ATL_FUSABLE(IDX)					 	       \
	((AT_OBJ_CLASS(IDX) == Label) ?					       \
		attr_tbl : sytb_var_error("ATL_FUSABLE", IDX))		       \
		[IDX].fld.flag28
# else
# define ATL_FUSABLE(IDX)		attr_tbl[IDX].fld.flag28
# endif

# ifdef _DEBUG
# define ATL_FUSION(IDX)					 	       \
	((AT_OBJ_CLASS(IDX) == Label) ?					       \
		attr_tbl : sytb_var_error("ATL_FUSION", IDX))		       \
		[IDX].fld.flag35
# else
# define ATL_FUSION(IDX)		attr_tbl[IDX].fld.flag35
# endif

# ifdef _DEBUG
# define ATL_FWD_REF_IDX(IDX)					 	       \
	((AT_OBJ_CLASS(IDX) == Label &&	!(AT_DEFINED(IDX))) ?		       \
		attr_tbl : sytb_var_error("ATL_FWD_REF_IDX", IDX))	       \
		[IDX].fld.field13
# else
# define ATL_FWD_REF_IDX(IDX)		attr_tbl[IDX].fld.field13
# endif

# ifdef _DEBUG
# define ATL_IN_ASSIGN(IDX)                                                    \
        ((AT_OBJ_CLASS(IDX) == Label) ?                                        \
                attr_tbl : sytb_var_error("ATL_IN_ASSIGN", IDX))               \
                [IDX].fld.flag1
# else
# define ATL_IN_ASSIGN(IDX)             attr_tbl[IDX].fld.flag1
# endif

# ifdef _DEBUG
# define ATL_IN_ASSIGN_LBL_CHAIN(IDX)                                          \
        ((AT_OBJ_CLASS(IDX) == Label) ?                                        \
                attr_tbl : sytb_var_error("ATL_IN_ASSIGN_LBL_CHAIN", IDX))     \
                [IDX].fld.flag3
# else
# define ATL_IN_ASSIGN_LBL_CHAIN(IDX)            attr_tbl[IDX].fld.flag3
# endif

# ifdef _DEBUG
# define ATL_IVDEP(IDX)					 	               \
	((AT_OBJ_CLASS(IDX) == Label) ?					       \
		attr_tbl : sytb_var_error("ATL_IVDEP", IDX))	               \
		[IDX].fld.flag4
# else
# define ATL_IVDEP(IDX)		attr_tbl[IDX].fld.flag4
# endif

# ifdef _DEBUG
# define ATL_MAXCPUS(IDX)				 	               \
	((AT_OBJ_CLASS(IDX) == Label) ?					       \
		attr_tbl : sytb_var_error("ATL_MAXCPUS", IDX))	               \
		[IDX].fld.flag23
# else
# define ATL_MAXCPUS(IDX)	attr_tbl[IDX].fld.flag23
# endif

# ifdef _DEBUG
# define ATL_NEW_LBL_IDX(IDX)                                                  \
        ((AT_OBJ_CLASS(IDX) == Label &&                                        \
          attr_tbl[IDX].fld.secondary_info == Lbl_Internal) ?                  \
                attr_tbl : sytb_var_error("ATL_NEW_LBL_IDX", IDX))             \
                [IDX].fld.field10
# else
# define ATL_NEW_LBL_IDX(IDX)          attr_tbl[IDX].fld.field10
# endif

# ifdef _DEBUG
# define ATL_NEXT_ASG_LBL_IDX(IDX)				 	       \
	((AT_OBJ_CLASS(IDX) == Label) ?					       \
		attr_tbl : sytb_var_error("ATL_NEXT_ASG_LBL_IDX", IDX))	       \
		[IDX].fld.field8
# else
# define ATL_NEXT_ASG_LBL_IDX(IDX)		attr_tbl[IDX].fld.field8
# endif

# ifdef _DEBUG
# define ATL_NEXTSCALAR(IDX)					 	       \
	((AT_OBJ_CLASS(IDX) == Label) ?					       \
		attr_tbl : sytb_var_error("ATL_NEXTSCALAR", IDX))	       \
		[IDX].fld.flag10
# else
# define ATL_NEXTSCALAR(IDX)		attr_tbl[IDX].fld.flag10
# endif

# ifdef _DEBUG
# define ATL_NOBLOCKING(IDX)					 	       \
	((AT_OBJ_CLASS(IDX) == Label) ?					       \
		attr_tbl : sytb_var_error("ATL_NOBLOCKING", IDX))	       \
		[IDX].fld.flag32
# else
# define ATL_NOBLOCKING(IDX)		attr_tbl[IDX].fld.flag32
# endif

# ifdef _DEBUG
# define ATL_NOFISSION(IDX)					 	       \
	((AT_OBJ_CLASS(IDX) == Label) ?					       \
		attr_tbl : sytb_var_error("ATL_NOFISSION", IDX))	       \
		[IDX].fld.flag27
# else
# define ATL_NOFISSION(IDX)		attr_tbl[IDX].fld.flag27
# endif

# ifdef _DEBUG
# define ATL_NOFUSION(IDX)					 	       \
	((AT_OBJ_CLASS(IDX) == Label) ?					       \
		attr_tbl : sytb_var_error("ATL_NOFUSION", IDX))		       \
		[IDX].fld.flag29
# else
# define ATL_NOFUSION(IDX)		attr_tbl[IDX].fld.flag29
# endif

# ifdef _DEBUG
# define ATL_NOINTERCHANGE(IDX)					 	       \
	((AT_OBJ_CLASS(IDX) == Label) ?					       \
		attr_tbl : sytb_var_error("ATL_NOINTERCHANGE", IDX))	       \
		[IDX].fld.flag30
# else
# define ATL_NOINTERCHANGE(IDX)		attr_tbl[IDX].fld.flag30
# endif

# ifdef _DEBUG
# define ATL_NORECURRENCE(IDX)					 	       \
	((AT_OBJ_CLASS(IDX) == Label) ?					       \
		attr_tbl : sytb_var_error("ATL_NORECURRENCE", IDX))	       \
		[IDX].fld.flag5
# else
# define ATL_NORECURRENCE(IDX)		attr_tbl[IDX].fld.flag5
# endif

# ifdef _DEBUG
# define ATL_NOTASK(IDX)					 	       \
	((AT_OBJ_CLASS(IDX) == Label) ?					       \
		attr_tbl : sytb_var_error("ATL_NOTASK", IDX))	               \
		[IDX].fld.flag37
# else
# define ATL_NOTASK(IDX)		attr_tbl[IDX].fld.flag37
# endif

# ifdef _DEBUG
# define ATL_NOVECTOR(IDX)					 	       \
	((AT_OBJ_CLASS(IDX) == Label) ?					       \
		attr_tbl : sytb_var_error("ATL_NOVECTOR", IDX))	               \
		[IDX].fld.flag6
# else
# define ATL_NOVECTOR(IDX)		attr_tbl[IDX].fld.flag6
# endif

# ifdef _DEBUG
# define ATL_NOVSEARCH(IDX)					 	       \
	((AT_OBJ_CLASS(IDX) == Label) ?					       \
		attr_tbl : sytb_var_error("ATL_NOVSEARCH", IDX))	       \
		[IDX].fld.flag9
# else
# define ATL_NOVSEARCH(IDX)		attr_tbl[IDX].fld.flag9
# endif

# ifdef _DEBUG
# define ATL_PATTERN(IDX)					 	       \
	((AT_OBJ_CLASS(IDX) == Label) ?					       \
		attr_tbl : sytb_var_error("ATL_PATTERN", IDX))		       \
		[IDX].fld.flag24
# else
# define ATL_PATTERN(IDX)		attr_tbl[IDX].fld.flag24
# endif

# ifdef _DEBUG
# define ATL_PERMUTATION(IDX)					 	       \
	((AT_OBJ_CLASS(IDX) == Label) ?					       \
		attr_tbl : sytb_var_error("ATL_PERMUTATION", IDX))	       \
		[IDX].fld.flag19
# else
# define ATL_PERMUTATION(IDX)		attr_tbl[IDX].fld.flag19
# endif

# ifdef _DEBUG
# define ATL_PP_FORMAT_TMP(IDX)                                                \
        ((AT_OBJ_CLASS(IDX) == Label &&                                        \
	  attr_tbl[IDX].fld.secondary_info == Lbl_Format) ?		       \
                attr_tbl : sytb_var_error("ATL_PP_FORMAT_TMP", IDX))           \
                [IDX].fld.field4
# else
# define ATL_PP_FORMAT_TMP(IDX)       attr_tbl[IDX].fld.field4
# endif

# ifdef _DEBUG
# define ATL_PREFERSTREAM(IDX)					 	       \
	((AT_OBJ_CLASS(IDX) == Label) ?					       \
		attr_tbl : sytb_var_error("ATL_PREFERSTREAM", IDX))	       \
		[IDX].fld.flag41
# else
# define ATL_PREFERSTREAM(IDX)		attr_tbl[IDX].fld.flag41
# endif

# ifdef _DEBUG
# define ATL_PREFERSTREAM_NOCINV(IDX)					       \
	((AT_OBJ_CLASS(IDX) == Label) ?					       \
		attr_tbl : sytb_var_error("ATL_PREFERSTREAM_NOCINV", IDX))     \
		[IDX].fld.flag43
# else
# define ATL_PREFERSTREAM_NOCINV(IDX)	attr_tbl[IDX].fld.flag43
# endif

# ifdef _DEBUG
# define ATL_PREFERTASK(IDX)					 	       \
	((AT_OBJ_CLASS(IDX) == Label) ?					       \
		attr_tbl : sytb_var_error("ATL_PREFERTASK", IDX))	       \
		[IDX].fld.flag20
# else
# define ATL_PREFERTASK(IDX)		attr_tbl[IDX].fld.flag20
# endif

# ifdef _DEBUG
# define ATL_PREFERVECTOR(IDX)					 	       \
	((AT_OBJ_CLASS(IDX) == Label) ?					       \
		attr_tbl : sytb_var_error("ATL_PREFERVECTOR", IDX))	       \
		[IDX].fld.flag39
# else
# define ATL_PREFERVECTOR(IDX)		attr_tbl[IDX].fld.flag39
# endif

# ifdef _DEBUG
# define ATL_SHORTLOOP(IDX)					 	       \
	((AT_OBJ_CLASS(IDX) == Label) ?					       \
		attr_tbl : sytb_var_error("ATL_SHORTLOOP", IDX))	       \
		[IDX].fld.flag7
# else
# define ATL_SHORTLOOP(IDX)		attr_tbl[IDX].fld.flag7
# endif

# ifdef _DEBUG
# define ATL_SHORTLOOP128(IDX)					 	       \
	((AT_OBJ_CLASS(IDX) == Label) ?					       \
		attr_tbl : sytb_var_error("ATL_SHORTLOOP128", IDX))	       \
		[IDX].fld.flag8
# else
# define ATL_SHORTLOOP128(IDX)		attr_tbl[IDX].fld.flag8
# endif

# ifdef _DEBUG
# define ATL_SPLIT(IDX)						 	       \
	((AT_OBJ_CLASS(IDX) == Label) ?					       \
		attr_tbl : sytb_var_error("ATL_SPLIT", IDX))		       \
		[IDX].fld.flag22
# else
# define ATL_SPLIT(IDX)		attr_tbl[IDX].fld.flag22
# endif

# ifdef _DEBUG
# define ATL_STREAM(IDX)			 	 	               \
	((AT_OBJ_CLASS(IDX) == Label) ?					       \
		attr_tbl : sytb_var_error("ATL_STREAM", IDX))	               \
		[IDX].fld.flag40
# else
# define ATL_STREAM(IDX)	attr_tbl[IDX].fld.flag40
# endif

# ifdef _DEBUG
# define ATL_INFORM_ONLY(IDX)			 	 	               \
	((AT_OBJ_CLASS(IDX) == Label) ?					       \
		attr_tbl : sytb_var_error("ATL_INFORM_ONLY", IDX))             \
		[IDX].fld.flag42
# else
# define ATL_INFORM_ONLY(IDX)	attr_tbl[IDX].fld.flag42
# endif

# ifdef _DEBUG
# define ATL_TOP_OF_LOOP(IDX)					 	       \
	((AT_OBJ_CLASS(IDX) == Label) ?					       \
		attr_tbl : sytb_var_error("ATL_TOP_OF_LOOP", IDX))	       \
		[IDX].fld.flag36
# else
# define ATL_TOP_OF_LOOP(IDX)		attr_tbl[IDX].fld.flag36
# endif

# ifdef _DEBUG
# define ATL_UNROLL_DIR(IDX)					 	       \
	((AT_OBJ_CLASS(IDX) == Label) ?					       \
		attr_tbl : sytb_var_error("ATL_UNROLL_DIR", IDX))	       \
		[IDX].fld.flag17
# else
# define ATL_UNROLL_DIR(IDX)		attr_tbl[IDX].fld.flag17
# endif



/* Definitions for namelist class */

# ifdef _DEBUG
# define ATN_FIRST_NAMELIST_IDX(IDX)					       \
	((AT_OBJ_CLASS(IDX) == Namelist_Grp) ?				       \
		attr_tbl : sytb_var_error("ATN_FIRST_NAMELIST_IDX", IDX))      \
		[IDX].fld.field13
# else
# define ATN_FIRST_NAMELIST_IDX(IDX)	attr_tbl[IDX].fld.field13
# endif

# ifdef _DEBUG
# define ATN_LAST_NAMELIST_IDX(IDX)					       \
	((AT_OBJ_CLASS(IDX) == Namelist_Grp) ?				       \
		attr_tbl : sytb_var_error("ATN_LAST_NAMELIST_IDX", IDX))       \
		[IDX].fld.field10
# else
# define ATN_LAST_NAMELIST_IDX(IDX)	attr_tbl[IDX].fld.field10
# endif

# ifdef _DEBUG
# define ATN_NAMELIST_DESC(IDX)						       \
	((AT_OBJ_CLASS(IDX) == Namelist_Grp) ?				       \
		attr_tbl : sytb_var_error("ATN_NAMELIST_DESC", IDX))	       \
		[IDX].fld.field4
# else
# define ATN_NAMELIST_DESC(IDX)		attr_tbl[IDX].fld.field4
# endif

# ifdef _DEBUG
# define ATN_NUM_NAMELIST(IDX)						       \
	((AT_OBJ_CLASS(IDX) == Namelist_Grp) ?				       \
		attr_tbl : sytb_var_error("ATN_NUM_NAMELIST", IDX))	       \
		[IDX].fld.field5
# else
# define ATN_NUM_NAMELIST(IDX)		attr_tbl[IDX].fld.field5
# endif


/* Definitions for program unit class */

# ifdef _DEBUG
# define ATP_ALIGN(IDX)							       \
	((AT_OBJ_CLASS(IDX) == Pgm_Unit) ?				       \
		attr_tbl : sytb_var_error("ATP_ALIGN", IDX))		       \
		[IDX].fld.flag32
# else
# define ATP_ALIGN(IDX)			attr_tbl[IDX].fld.flag32
# endif

# ifdef _DEBUG
# define ATP_ALL_INTENT_IN(IDX)						       \
	((AT_OBJ_CLASS(IDX) == Pgm_Unit) ?				       \
		attr_tbl : sytb_var_error("ATP_ALL_INTENT_IN", IDX))	       \
		[IDX].fld.flag3
# else
# define ATP_ALL_INTENT_IN(IDX)		attr_tbl[IDX].fld.flag3
# endif

# ifdef _DEBUG
# define ATP_ALT_ENTRY(IDX)						       \
	((AT_OBJ_CLASS(IDX) == Pgm_Unit) ?				       \
		attr_tbl : sytb_var_error("ATP_ALT_ENTRY", IDX))	       \
		[IDX].fld.flag2
# else
# define ATP_ALT_ENTRY(IDX)		attr_tbl[IDX].fld.flag2
# endif

# ifdef _DEBUG
# define ATP_ARGCHCK_CALL(IDX)						       \
	((AT_OBJ_CLASS(IDX) == Pgm_Unit) ?				       \
		attr_tbl : sytb_var_error("ATP_ARGCHCK_CALL", IDX))	       \
		[IDX].fld.flag24
# else
# define ATP_ARGCHCK_CALL(IDX)		attr_tbl[IDX].fld.flag24
# endif

# ifdef _DEBUG
# define ATP_ARGCHCK_ENTRY(IDX)						       \
	((AT_OBJ_CLASS(IDX) == Pgm_Unit) ?				       \
		attr_tbl : sytb_var_error("ATP_ARGCHCK_ENTRY", IDX))	       \
		[IDX].fld.flag25
# else
# define ATP_ARGCHCK_ENTRY(IDX)		attr_tbl[IDX].fld.flag25
# endif

# ifdef _DEBUG
# define ATP_CIF_DARG_PROC(IDX)						       \
	((AT_OBJ_CLASS(IDX) == Pgm_Unit) ?				       \
		attr_aux_tbl : attr_aux_var_error("ATP_CIF_DARG_PROC", IDX))   \
		[IDX].fld.flag1
# else
# define ATP_CIF_DARG_PROC(IDX)		attr_aux_tbl[IDX].fld.flag1
# endif

# ifdef _DEBUG
# define ATP_DCL_EXTERNAL(IDX)						       \
	((AT_OBJ_CLASS(IDX) == Pgm_Unit) ?				       \
		attr_tbl : sytb_var_error("ATP_DCL_EXTERNAL", IDX))	       \
		[IDX].fld.flag10
# else
# define ATP_DCL_EXTERNAL(IDX)		attr_tbl[IDX].fld.flag10
# endif

# ifdef _DEBUG
# define ATP_DOES_NOT_RETURN(IDX)                                              \
        ((AT_OBJ_CLASS(IDX) == Pgm_Unit) ?                                     \
                attr_tbl : sytb_var_error("ATP_DOES_NOT_RETURN", IDX))         \
                [IDX].fld.flag49
# else
# define ATP_DOES_NOT_RETURN(IDX)   attr_tbl[IDX].fld.flag49
# endif

# ifdef _DEBUG
# define ATP_DUMMY_PROC_LINK(IDX)                                              \
        ((AT_OBJ_CLASS(IDX) == Pgm_Unit &&                                     \
          attr_tbl[IDX].fld.field2 == Dummy_Proc) ?                            \
                attr_tbl : sytb_var_error("ATP_DUMMY_PROC_LINK", IDX))         \
                [IDX].fld.field4
# else
# define ATP_DUMMY_PROC_LINK(IDX)      attr_tbl[IDX].fld.field4
# endif

# ifdef _DEBUG
# define ATP_DUPLICATE_INTERFACE_IDX(IDX)				       \
	((AT_OBJ_CLASS(IDX) == Pgm_Unit) ?				       \
	  attr_aux_tbl:attr_aux_var_error("ATP_DUPLICATE_INTERFACE_IDX",IDX))  \
		[IDX].fld.field4
# else
# define ATP_DUPLICATE_INTERFACE_IDX(IDX)	attr_aux_tbl[IDX].fld.field4
# endif

# ifdef _DEBUG
# define ATP_ELEMENTAL(IDX)						       \
	((AT_OBJ_CLASS(IDX) == Pgm_Unit) ?				       \
		attr_tbl : sytb_var_error("ATP_ELEMENTAL", IDX))	       \
		[IDX].fld.flag31
# else
# define ATP_ELEMENTAL(IDX)		attr_tbl[IDX].fld.flag31
# endif

# ifdef _DEBUG
# define ATP_ENTRY_LABEL_SH_IDX(IDX) 					       \
	((AT_OBJ_CLASS(IDX) == Pgm_Unit) ?				       \
		attr_aux_tbl:attr_aux_var_error("ATP_ENTRY_LABEL_SH_IDX",IDX)) \
		[IDX].fld.field3
# else
# define ATP_ENTRY_LABEL_SH_IDX(IDX)	attr_aux_tbl[IDX].fld.field3
# endif

# ifdef _DEBUG
# define ATP_EXT_NAME_IDX(IDX)						       \
	(AT_OBJ_CLASS(IDX) == Pgm_Unit ?				       \
		attr_tbl : sytb_var_error("ATP_EXT_NAME_IDX", IDX))	       \
		[IDX].fld.field13
# else
# define ATP_EXT_NAME_IDX(IDX)		attr_tbl[IDX].fld.field13
# endif

# define ATP_EXT_NAME(IDX)	      name_pool[ATP_EXT_NAME_IDX(IDX)].name_char
# define ATP_EXT_NAME_LONG(IDX)	     ((long *)&name_pool[ATP_EXT_NAME_IDX(IDX)])
# define ATP_EXT_NAME_PTR(IDX)	     ((char *)&name_pool[ATP_EXT_NAME_IDX(IDX)])

# ifdef _DEBUG
# define ATP_EXT_NAME_LEN(IDX)						       \
        (AT_OBJ_CLASS(IDX) == Pgm_Unit ?				       \
		attr_tbl : sytb_var_error("ATP_EXT_NAME_LEN", IDX))	       \
		[IDX].fld.field12
# else
# define ATP_EXT_NAME_LEN(IDX)		attr_tbl[IDX].fld.field12
# endif

# ifdef _DEBUG
# define ATP_EXPL_ITRFC(IDX)						       \
	((AT_OBJ_CLASS(IDX) == Pgm_Unit) ?				       \
		attr_tbl : sytb_var_error("ATP_EXPL_ITRFC", IDX))	       \
		[IDX].fld.flag41
# else
# define ATP_EXPL_ITRFC(IDX)		attr_tbl[IDX].fld.flag41
# endif

# ifdef _DEBUG
# define ATP_EXTERNAL_INTRIN(IDX)					       \
	((AT_OBJ_CLASS(IDX) == Pgm_Unit) ?				       \
		attr_tbl : sytb_var_error("ATP_EXTERNAL_INTRIN", IDX))	       \
		[IDX].fld.flag43
# else
# define ATP_EXTERNAL_INTRIN(IDX)	attr_tbl[IDX].fld.flag43
# endif

# ifdef _DEBUG
# define ATP_EXTRA_DARG(IDX)					 	       \
	((AT_OBJ_CLASS(IDX) == Pgm_Unit) ? 				       \
		attr_tbl : sytb_var_error("ATP_EXTRA_DARG", IDX))	       \
		[IDX].fld.flag42
# else
# define ATP_EXTRA_DARG(IDX)		attr_tbl[IDX].fld.flag42
# endif

# ifdef _DEBUG
# define ATP_FIRST_IDX(IDX)						       \
	(((AT_OBJ_CLASS(IDX) == Pgm_Unit) &&				       \
	  (attr_tbl[IDX].fld.secondary_info != Module)) ||		       \
	 (AT_OBJ_CLASS(IDX) == Stmt_Func) ?				       \
		attr_tbl : sytb_var_error("ATP_FIRST_IDX", IDX))	       \
		[IDX].fld.field14
# else
# define ATP_FIRST_IDX(IDX)		attr_tbl[IDX].fld.field14
# endif

# ifdef _DEBUG
# define ATP_FIRST_SH_IDX(IDX)                                                 \
        ((AT_OBJ_CLASS(IDX) == Pgm_Unit &&				       \
	  attr_tbl[IDX].fld.secondary_info != Module &&			       \
	  attr_tbl[IDX].fld.field2 != Intrin_Proc) ?			       \
                attr_tbl : sytb_var_error("ATP_FIRST_SH_IDX", IDX))            \
                [IDX].fld.field4
# else
# define ATP_FIRST_SH_IDX(IDX)         attr_tbl[IDX].fld.field4
# endif

# ifdef _DEBUG
# define ATP_GLOBAL_ATTR_IDX(IDX)					       \
	((AT_OBJ_CLASS(IDX) == Pgm_Unit) ?				       \
		attr_tbl : sytb_var_error("ATP_GLOBAL_ATTR_IDX", IDX))	       \
		[IDX].fld.field16
# else
# define ATP_GLOBAL_ATTR_IDX(IDX)	attr_tbl[IDX].fld.field16
# endif

# ifdef _DEBUG
# define ATP_HAS_ALT_RETURN(IDX)					       \
	((AT_OBJ_CLASS(IDX) == Pgm_Unit) ?				       \
		attr_tbl : sytb_var_error("ATP_HAS_ALT_RETURN", IDX))	       \
		[IDX].fld.flag9
# else
# define ATP_HAS_ALT_RETURN(IDX)	attr_tbl[IDX].fld.flag9
# endif

# ifdef _DEBUG
# define ATP_HAS_OVER_INDEXING(IDX)					       \
	((AT_OBJ_CLASS(IDX) == Pgm_Unit) ?				       \
		attr_tbl : sytb_var_error("ATP_HAS_OVER_INDEXING", IDX))       \
		[IDX].fld.flag26
# else
# define ATP_HAS_OVER_INDEXING(IDX)	attr_tbl[IDX].fld.flag26
# endif

# ifdef _DEBUG
# define ATP_HAS_TASK_DIRS(IDX)						       \
	((AT_OBJ_CLASS(IDX) == Pgm_Unit) ?				       \
		attr_tbl : sytb_var_error("ATP_HAS_TASK_DIRS", IDX))	       \
		[IDX].fld.flag27
# else
# define ATP_HAS_TASK_DIRS(IDX)		attr_tbl[IDX].fld.flag27
# endif

# ifdef _DEBUG
# define ATP_IMPLICIT_USE_MODULE(IDX)					       \
	((AT_OBJ_CLASS(IDX) == Pgm_Unit) ?				       \
	     attr_aux_tbl: attr_aux_var_error("ATP_IMPLICIT_USE_MODULE",IDX))  \
		[IDX].fld.flag5
# else
# define ATP_IMPLICIT_USE_MODULE(IDX)	attr_aux_tbl[IDX].fld.flag5
# endif

# ifdef _DEBUG
# define ATP_IN_INTERFACE_BLK(IDX)					       \
	((AT_OBJ_CLASS(IDX) == Pgm_Unit) ?				       \
		attr_tbl : sytb_var_error("ATP_IN_INTERFACE_BLK", IDX))	       \
		[IDX].fld.flag11
# else
# define ATP_IN_INTERFACE_BLK(IDX)	attr_tbl[IDX].fld.flag11
# endif

# ifdef _DEBUG
# define ATP_IN_UNNAMED_INTERFACE(IDX)					       \
	((AT_OBJ_CLASS(IDX) == Pgm_Unit) ?				       \
		attr_tbl : sytb_var_error("ATP_IN_UNNAMED_INTERFACE", IDX))    \
		[IDX].fld.flag18
# else
# define ATP_IN_UNNAMED_INTERFACE(IDX)	attr_tbl[IDX].fld.flag18
# endif

# ifdef _DEBUG
# define ATP_INDIRECT_MODULE(IDX)					       \
	((AT_OBJ_CLASS(IDX) == Pgm_Unit) ?				       \
		attr_aux_tbl : attr_aux_var_error("ATP_INDIRECT_MODULE", IDX)) \
		[IDX].fld.flag2
# else
# define ATP_INDIRECT_MODULE(IDX)	attr_aux_tbl[IDX].fld.flag2
# endif

# ifdef _DEBUG
# define ATP_INLINE_ALWAYS(IDX)						       \
	((AT_OBJ_CLASS(IDX) == Pgm_Unit) ?				       \
		attr_tbl : sytb_var_error("ATP_INLINE_ALWAYS", IDX))	       \
		[IDX].fld.flag22
# else
# define ATP_INLINE_ALWAYS(IDX)		attr_tbl[IDX].fld.flag22
# endif

# ifdef _DEBUG
# define ATP_INLINE_NEVER(IDX)						       \
	((AT_OBJ_CLASS(IDX) == Pgm_Unit) ?				       \
		attr_tbl : sytb_var_error("ATP_INLINE_NEVER", IDX))	       \
		[IDX].fld.flag23
# else
# define ATP_INLINE_NEVER(IDX)		attr_tbl[IDX].fld.flag23
# endif

# ifdef _DEBUG
# define ATP_INTERFACE_IDX(IDX)                                                \
        ((AT_OBJ_CLASS(IDX) == Pgm_Unit &&				       \
	  attr_tbl[IDX].fld.field2 == Intrin_Proc) ?			       \
                attr_tbl : sytb_var_error("ATP_INTERFACE_IDX", IDX))           \
                [IDX].fld.field4
# else
# define ATP_INTERFACE_IDX(IDX)		attr_tbl[IDX].fld.field4
# endif

# ifdef _DEBUG
# define ATP_INTRIN_ENUM(IDX)						       \
	((AT_OBJ_CLASS(IDX) == Pgm_Unit &&				       \
	  attr_tbl[IDX].fld.field2 == Intrin_Proc) ?			       \
		attr_tbl : sytb_var_error("ATP_INTRIN_ENUM", IDX))	       \
		[IDX].fld.field10
# else
# define ATP_INTRIN_ENUM(IDX)		attr_tbl[IDX].fld.field10
# endif

# ifdef _DEBUG
# define ATP_IN_CURRENT_COMPILE(IDX)					       \
	((AT_OBJ_CLASS(IDX) == Pgm_Unit) ?				       \
		attr_aux_tbl: attr_aux_var_error("ATP_IN_CURRENT_COMPILE",IDX))\
		[IDX].fld.flag4
# else
# define ATP_IN_CURRENT_COMPILE(IDX)	attr_aux_tbl[IDX].fld.flag4
# endif

# ifdef _DEBUG
# define ATP_MAY_INLINE(IDX)						       \
	((AT_OBJ_CLASS(IDX) == Pgm_Unit) ?				       \
		attr_tbl : sytb_var_error("ATP_MAY_INLINE", IDX))	       \
		[IDX].fld.flag21
# else
# define ATP_MAY_INLINE(IDX)		attr_tbl[IDX].fld.flag21
# endif

# ifdef _DEBUG
# define ATP_MOD_PATH_IDX(IDX)						       \
	((AT_OBJ_CLASS(IDX) == Pgm_Unit && 				       \
	  attr_tbl[IDX].fld.secondary_info == Module) ?			       \
		attr_tbl : sytb_var_error("ATP_MOD_PATH_IDX", IDX))	       \
		[IDX].fld.field4
# else
# define ATP_MOD_PATH_IDX(IDX)		attr_tbl[IDX].fld.field4
# endif

# define ATP_MOD_PATH_NAME_PTR(IDX)  ((char *)&name_pool[ATP_MOD_PATH_IDX(IDX)])

# ifdef _DEBUG
# define ATP_MOD_PATH_LEN(IDX)						       \
	((AT_OBJ_CLASS(IDX) == Pgm_Unit &&				       \
	  attr_tbl[IDX].fld.secondary_info == Module) ?			       \
		attr_tbl : sytb_var_error("ATP_MOD_PATH_LEN", IDX))	       \
		[IDX].fld.field6
# else
# define ATP_MOD_PATH_LEN(IDX)		attr_tbl[IDX].fld.field6
# endif

# ifdef _DEBUG
# define ATP_MODULE_STR_IDX(IDX)                                               \
      ((AT_OBJ_CLASS(IDX) == Pgm_Unit &&                                       \
        attr_tbl[IDX].fld.secondary_info == Module) ?                          \
              attr_tbl : sytb_var_error("ATP_MODULE_STR_IDX", IDX))            \
              [IDX].fld.field10
# else
# define ATP_MODULE_STR_IDX(IDX)	attr_tbl[IDX].fld.field10
# endif

# ifdef _DEBUG
# define ATP_NAME_IN_STONE(IDX)						       \
	((AT_OBJ_CLASS(IDX) == Pgm_Unit) ?				       \
		attr_tbl : sytb_var_error("ATP_NAME_IN_STONE", IDX))	       \
		[IDX].fld.flag20
# else
# define ATP_NAME_IN_STONE(IDX)		attr_tbl[IDX].fld.flag20
# endif

# ifdef _DEBUG
# define ATP_NO_ENTRY_LIST(IDX)						       \
	((AT_OBJ_CLASS(IDX) == Pgm_Unit) ?				       \
		attr_aux_tbl : attr_aux_var_error("ATP_NO_ENTRY_LIST", IDX))   \
		[IDX].fld.field1
# else
# define ATP_NO_ENTRY_LIST(IDX)		attr_aux_tbl[IDX].fld.field1
# endif

# ifdef _DEBUG
# define ATP_NON_ANSI_INTRIN(IDX)					       \
	((AT_OBJ_CLASS(IDX) == Pgm_Unit) ?				       \
		attr_tbl : sytb_var_error("ATP_NON_ANSI_INTRIN", IDX))	       \
		[IDX].fld.flag6
# else
# define ATP_NON_ANSI_INTRIN(IDX)	attr_tbl[IDX].fld.flag6
# endif

# ifdef _DEBUG
# define ATP_NOSIDE_EFFECTS(IDX)					       \
	((AT_OBJ_CLASS(IDX) == Pgm_Unit) ?				       \
		attr_tbl : sytb_var_error("ATP_NOSIDE_EFFECTS", IDX))	       \
		[IDX].fld.flag5
# else
# define ATP_NOSIDE_EFFECTS(IDX)	attr_tbl[IDX].fld.flag5
# endif

# ifdef _DEBUG
# define ATP_NUM_DARGS(IDX)						       \
	(((AT_OBJ_CLASS(IDX) == Pgm_Unit &&				       \
	   attr_tbl[IDX].fld.secondary_info != Module) ||		       \
	  AT_OBJ_CLASS(IDX) == Stmt_Func) ?				       \
		attr_tbl : sytb_var_error("ATP_NUM_DARGS", IDX))	       \
		[IDX].fld.field6
# else
# define ATP_NUM_DARGS(IDX)		attr_tbl[IDX].fld.field6
# endif

# ifdef _DEBUG
# define ATP_PARENT_IDX(IDX)						       \
	((AT_OBJ_CLASS(IDX) == Pgm_Unit && 				       \
	  attr_tbl[IDX].fld.secondary_info != Module) ?			       \
		attr_tbl : sytb_var_error("ATP_PARENT_IDX", IDX))	       \
		[IDX].fld.field10
# else
# define ATP_PARENT_IDX(IDX)		attr_tbl[IDX].fld.field10
# endif

# ifdef _DEBUG
# define ATP_PGM_UNIT(IDX)						       \
	((AT_OBJ_CLASS(IDX) == Pgm_Unit) ?				       \
		attr_tbl : sytb_var_error("ATP_PGM_UNIT", IDX))		       \
		[IDX].fld.secondary_info
# else
# define ATP_PGM_UNIT(IDX)		attr_tbl[IDX].fld.secondary_info
# endif

# ifdef _DEBUG
# define ATP_OPTIONAL_DIR(IDX)   					       \
	((AT_OBJ_CLASS(IDX) == Pgm_Unit) ?				       \
		attr_tbl : sytb_var_error("ATP_OPTIONAL_DIR", IDX))            \
		[IDX].fld.flag44
# else
# define ATP_OPTIONAL_DIR(IDX)	       attr_tbl[IDX].fld.flag44
# endif

# ifdef _DEBUG
# define ATP_PROC(IDX)							       \
	((AT_OBJ_CLASS(IDX) == Pgm_Unit) ?				       \
		attr_tbl : sytb_var_error("ATP_PROC", IDX))		       \
		[IDX].fld.field2
# else
# define ATP_PROC(IDX)			attr_tbl[IDX].fld.field2
# endif

# ifdef _DEBUG
# define ATP_PURE(IDX)							       \
	((AT_OBJ_CLASS(IDX) == Pgm_Unit) ?				       \
		attr_tbl : sytb_var_error("ATP_PURE", IDX))		       \
		[IDX].fld.flag30
# else
# define ATP_PURE(IDX)			attr_tbl[IDX].fld.flag30
# endif

# ifdef _DEBUG
# define ATP_RECURSIVE(IDX)						       \
	((AT_OBJ_CLASS(IDX) == Pgm_Unit) ?				       \
		attr_tbl : sytb_var_error("ATP_RECURSIVE", IDX))	       \
		[IDX].fld.flag1
# else
# define ATP_RECURSIVE(IDX)		attr_tbl[IDX].fld.flag1
# endif

# ifdef _DEBUG
# define ATP_RSLT_IDX(IDX)						       \
	((AT_OBJ_CLASS(IDX) == Pgm_Unit && 				       \
	  ATP_PGM_UNIT(IDX) != Module) ?  				       \
		attr_tbl : sytb_var_error("ATP_RSLT_IDX", IDX))		       \
		[IDX].fld.field8
# else
# define ATP_RSLT_IDX(IDX)		attr_tbl[IDX].fld.field8
# endif

# ifdef _DEBUG
# define ATP_RSLT_NAME(IDX)						       \
	((AT_OBJ_CLASS(IDX) == Pgm_Unit) ?				       \
		attr_tbl : sytb_var_error("ATP_RSLT_NAME", IDX))	       \
		[IDX].fld.flag12
# else
# define ATP_RSLT_NAME(IDX)		attr_tbl[IDX].fld.flag12
# endif

# ifdef _DEBUG
# define ATP_SAVE_ALL(IDX)						       \
	((AT_OBJ_CLASS(IDX) == Pgm_Unit) ?				       \
		attr_tbl : sytb_var_error("ATP_SAVE_ALL", IDX))		       \
		[IDX].fld.flag34
# else
# define ATP_SAVE_ALL(IDX)		attr_tbl[IDX].fld.flag34
# endif

# ifdef _DEBUG
# define ATP_SCP_ALIVE(IDX)						       \
	((AT_OBJ_CLASS(IDX) == Pgm_Unit) ?				       \
		attr_tbl : sytb_var_error("ATP_SCP_ALIVE", IDX))	       \
		[IDX].fld.flag8
# else
# define ATP_SCP_ALIVE(IDX)		attr_tbl[IDX].fld.flag8
# endif

# ifdef _DEBUG
# define ATP_SCP_IDX(IDX)                                                      \
        ((AT_OBJ_CLASS(IDX) == Pgm_Unit) ?                                     \
                attr_aux_tbl : attr_aux_var_error("ATP_SCP_IDX", IDX))	       \
                [IDX].fld.field5
# else
# define ATP_SCP_IDX(IDX)               attr_aux_tbl[IDX].fld.field5
# endif

# ifdef _DEBUG
# define ATP_SGI_LOCAL_INLINE(IDX)					       \
	((AT_OBJ_CLASS(IDX) == Pgm_Unit) ?				       \
		attr_aux_tbl : attr_aux_var_error("ATP_SGI_LOCAL_INLINE",IDX)) \
		[IDX].fld.flag6
# else
# define ATP_SGI_LOCAL_INLINE(IDX)	attr_aux_tbl[IDX].fld.flag6
# endif

# ifdef _DEBUG
# define ATP_SGI_LOCAL_NOINLINE(IDX)					       \
	((AT_OBJ_CLASS(IDX) == Pgm_Unit) ?				       \
    	     attr_aux_tbl : attr_aux_var_error("ATP_SGI_LOCAL_NOINLINE",IDX))  \
		[IDX].fld.flag7
# else
# define ATP_SGI_LOCAL_NOINLINE(IDX)	attr_aux_tbl[IDX].fld.flag7
# endif

# ifdef _DEBUG
# define ATP_SGI_GLOBAL_INLINE(IDX)					       \
	((AT_OBJ_CLASS(IDX) == Pgm_Unit) ?				       \
		attr_tbl : sytb_var_error("ATP_SGI_GLOBAL_INLINE", IDX))       \
		[IDX].fld.flag35
# else
# define ATP_SGI_GLOBAL_INLINE(IDX)	attr_tbl[IDX].fld.flag35
# endif

# ifdef _DEBUG
# define ATP_SGI_GLOBAL_NOINLINE(IDX)					       \
	((AT_OBJ_CLASS(IDX) == Pgm_Unit) ?				       \
		attr_tbl : sytb_var_error("ATP_SGI_GLOBAL_NOINLINE", IDX))     \
		[IDX].fld.flag36
# else
# define ATP_SGI_GLOBAL_NOINLINE(IDX)	attr_tbl[IDX].fld.flag36
# endif

# ifdef _DEBUG
# define ATP_SGI_ROUTINE_INLINE(IDX)					       \
	((AT_OBJ_CLASS(IDX) == Pgm_Unit) ?				       \
		attr_tbl : sytb_var_error("ATP_SGI_ROUTINE_INLINE", IDX))      \
		[IDX].fld.flag38
# else
# define ATP_SGI_ROUTINE_INLINE(IDX)	attr_tbl[IDX].fld.flag38
# endif

# ifdef _DEBUG
# define ATP_SGI_ROUTINE_NOINLINE(IDX)					       \
	((AT_OBJ_CLASS(IDX) == Pgm_Unit) ?				       \
		attr_tbl : sytb_var_error("ATP_SGI_ROUTINE_NOINLINE", IDX))    \
		[IDX].fld.flag37
# else
# define ATP_SGI_ROUTINE_NOINLINE(IDX)	attr_tbl[IDX].fld.flag37
# endif

# ifdef _DEBUG
# define ATP_STACK_DIR(IDX)						       \
	((AT_OBJ_CLASS(IDX) == Pgm_Unit) ?				       \
		attr_tbl : sytb_var_error("ATP_STACK_DIR", IDX))	       \
		[IDX].fld.flag7
# else
# define ATP_STACK_DIR(IDX)		attr_tbl[IDX].fld.flag7
# endif

# ifdef _DEBUG
# define ATP_SYMMETRIC(IDX)						       \
	((AT_OBJ_CLASS(IDX) == Pgm_Unit) ?				       \
		attr_tbl : sytb_var_error("ATP_SYMMETRIC", IDX))	       \
		[IDX].fld.flag28
# else
# define ATP_SYMMETRIC(IDX)		attr_tbl[IDX].fld.flag28
# endif

# ifdef _DEBUG
# define ATP_SYSTEM_MODULE(IDX)						       \
      ((AT_OBJ_CLASS(IDX) == Pgm_Unit) ?				       \
         attr_aux_tbl : attr_aux_var_error("ATP_SYSTEM_MODULE", IDX))	       \
              [IDX].fld.flag3
# else
# define ATP_SYSTEM_MODULE(IDX)		attr_aux_tbl[IDX].fld.flag3
# endif

# ifdef _DEBUG
# define ATP_TASK_SHARED(IDX)						       \
	((AT_OBJ_CLASS(IDX) == Pgm_Unit) ?				       \
		attr_tbl : sytb_var_error("ATP_TASK_SHARED", IDX))	       \
		[IDX].fld.flag19
# else
# define ATP_TASK_SHARED(IDX)	attr_tbl[IDX].fld.flag19
# endif

# ifdef _DEBUG
# define ATP_USE_LIST(IDX)     		                                       \
	((AT_OBJ_CLASS(IDX) == Pgm_Unit && 				       \
	  attr_tbl[IDX].fld.secondary_info == Module) ?			       \
                attr_aux_tbl : attr_aux_var_error("ATP_USE_LIST", IDX))	       \
                [IDX].fld.field2
# else
# define ATP_USE_LIST(IDX)		attr_aux_tbl[IDX].fld.field2
# endif

# ifdef _DEBUG
# define ATP_USE_TYPE(IDX)						       \
	(AT_OBJ_CLASS(IDX) == Pgm_Unit ?				       \
		attr_tbl : sytb_var_error("ATP_USE_TYPE", IDX))		       \
		[IDX].fld.field3
# else
# define ATP_USE_TYPE(IDX)			attr_tbl[IDX].fld.field3
# endif

# ifdef _DEBUG
# define ATP_USES_EREGS(IDX)	     	 	   			       \
	((AT_OBJ_CLASS(IDX) == Pgm_Unit) ?				       \
		attr_tbl : sytb_var_error("ATP_USES_EREGS", IDX))	       \
		[IDX].fld.flag29
# else
# define ATP_USES_EREGS(IDX)		attr_tbl[IDX].fld.flag29
# endif

# ifdef _DEBUG
# define ATP_VFUNCTION(IDX)						       \
	((AT_OBJ_CLASS(IDX) == Pgm_Unit) ?				       \
		attr_tbl : sytb_var_error("ATP_VFUNCTION", IDX))	       \
		[IDX].fld.flag4
# else
# define ATP_VFUNCTION(IDX)		attr_tbl[IDX].fld.flag4
# endif


/* Symbol table definitions for statement functions. */

# ifdef _DEBUG
# define ATS_SF_ACTIVE(IDX)					 	       \
	((AT_OBJ_CLASS(IDX) == Stmt_Func) ?				       \
		attr_tbl : sytb_var_error("ATS_SF_ACTIVE", IDX))	       \
		[IDX].fld.flag1
# else
# define ATS_SF_ACTIVE(IDX)		attr_tbl[IDX].fld.flag1
# endif

# ifdef _DEBUG
# define ATS_SF_FLD(IDX)						       \
	((AT_OBJ_CLASS(IDX) == Stmt_Func) ?				       \
		attr_tbl : sytb_var_error("ATS_SF_FLD", IDX))		       \
		[IDX].fld.secondary_info
# else
# define ATS_SF_FLD(IDX)		attr_tbl[IDX].fld.secondary_info
# endif

# ifdef _DEBUG
# define ATS_SF_IDX(IDX)						       \
	((AT_OBJ_CLASS(IDX) == Stmt_Func) ?				       \
		attr_tbl : sytb_var_error("ATS_SF_IDX", IDX))		       \
		[IDX].fld.field4
# else
# define ATS_SF_IDX(IDX)		attr_tbl[IDX].fld.field4
# endif

# ifdef _DEBUG
# define ATS_SF_SEMANTICS_DONE(IDX)					       \
	((AT_OBJ_CLASS(IDX) == Stmt_Func) ?				       \
		attr_tbl : sytb_var_error("ATS_SF_SEMANTICS_DONE", IDX))       \
		[IDX].fld.flag2
# else
# define ATS_SF_SEMANTICS_DONE(IDX)	attr_tbl[IDX].fld.flag2
# endif


/* definitions for derived types - ATT */

/* Remove in 5.0 */

# ifdef _DEBUG
# define OLD_ATT_16_BIT_ALIGN(IDX)					       \
	((AT_OBJ_CLASS(IDX) == Derived_Type) ?				       \
		attr_tbl : sytb_var_error("OLD_ATT_16_BIT_ALIGN", IDX))	       \
		[IDX].fld.flag27
# else
# define OLD_ATT_16_BIT_ALIGN(IDX)	attr_tbl[IDX].fld.flag27
# endif

# ifdef _DEBUG
# define OLD_ATT_8_BIT_ALIGN(IDX)					       \
	((AT_OBJ_CLASS(IDX) == Derived_Type) ?				       \
		attr_tbl : sytb_var_error("OLD_ATT_8_BIT_ALIGN", IDX))	       \
		[IDX].fld.flag28
# else
# define OLD_ATT_8_BIT_ALIGN(IDX)	attr_tbl[IDX].fld.flag28
# endif

# ifdef _DEBUG
# define OLD_ATT_HALF_WORD_ALIGN(IDX)					       \
	((AT_OBJ_CLASS(IDX) == Derived_Type) ?				       \
		attr_tbl : sytb_var_error("OLD_ATT_HALF_WORD_ALIGN", IDX))     \
		[IDX].fld.flag26
# else
# define OLD_ATT_HALF_WORD_ALIGN(IDX)	attr_tbl[IDX].fld.flag26
# endif

# ifdef _DEBUG
# define ATT_ALIGNMENT(IDX)						       \
	((AT_OBJ_CLASS(IDX) == Derived_Type) ?				       \
		attr_tbl : sytb_var_error("ATT_ALIGNMENT", IDX))	       \
		[IDX].fld.field8
# else
# define ATT_ALIGNMENT(IDX)		attr_tbl[IDX].fld.field8
# endif

# ifdef _DEBUG
# define ATT_CHAR_CPNT(IDX)						       \
	((AT_OBJ_CLASS(IDX) == Derived_Type) ?				       \
		attr_tbl : sytb_var_error("ATT_CHAR_CPNT", IDX))	       \
		[IDX].fld.flag22
# else
# define ATT_CHAR_CPNT(IDX)		attr_tbl[IDX].fld.flag22
# endif

# ifdef _DEBUG
# define ATT_CHAR_SEQ(IDX)						       \
	((AT_OBJ_CLASS(IDX) == Derived_Type) ?				       \
		attr_tbl : sytb_var_error("ATT_CHAR_SEQ", IDX))		       \
		[IDX].fld.flag20
# else
# define ATT_CHAR_SEQ(IDX)		attr_tbl[IDX].fld.flag20
# endif

# ifdef _DEBUG
# define ATT_CIF_DT_ID(IDX)    						       \
	((AT_OBJ_CLASS(IDX) == Derived_Type) ?				       \
		attr_aux_tbl : attr_aux_var_error("ATT_CIF_DT_ID", IDX))       \
		[IDX].fld.field4
# else
# define ATT_CIF_DT_ID(IDX)		attr_aux_tbl[IDX].fld.field4
# endif

# ifdef _DEBUG
# define ATT_DALIGN_ME(IDX)						       \
	((AT_OBJ_CLASS(IDX) == Derived_Type) ?				       \
		attr_tbl : sytb_var_error("ATT_DALIGN_ME", IDX))	       \
		[IDX].fld.flag24
# else
# define ATT_DALIGN_ME(IDX)		attr_tbl[IDX].fld.flag24
# endif

# ifdef _DEBUG
# define ATT_DCL_NUMERIC_SEQ(IDX)					       \
	((AT_OBJ_CLASS(IDX) == Derived_Type) ?				       \
		attr_tbl : sytb_var_error("ATT_DCL_NUMERIC_SEQ", IDX))	       \
		[IDX].fld.flag25
# else
# define ATT_DCL_NUMERIC_SEQ(IDX)	attr_tbl[IDX].fld.flag25
# endif

# ifdef _DEBUG
# define ATT_DEFAULT_INITIALIZED(IDX)					       \
	((AT_OBJ_CLASS(IDX) == Derived_Type) ?				       \
		attr_tbl : sytb_var_error("ATT_DEFAULT_INITIALIZED", IDX))     \
		[IDX].fld.flag29
# else
# define ATT_DEFAULT_INITIALIZED(IDX)	attr_tbl[IDX].fld.flag29
# endif

# ifdef _DEBUG
# define ATT_FIRST_CPNT_IDX(IDX)					       \
	((AT_OBJ_CLASS(IDX) == Derived_Type) ?				       \
		attr_tbl : sytb_var_error("ATT_FIRST_CPNT_IDX", IDX))	       \
		[IDX].fld.field10
# else
# define ATT_FIRST_CPNT_IDX(IDX)	attr_tbl[IDX].fld.field10
# endif

# ifdef _DEBUG
# define ATT_GLOBAL_TYPE_IDX(IDX) 	                                       \
        ((AT_OBJ_CLASS(IDX) == Derived_Type) ?                                 \
                attr_tbl : sytb_var_error("ATT_GLOBAL_TYPE_IDX", IDX))	       \
                [IDX].fld.field6
# else
# define ATT_GLOBAL_TYPE_IDX(IDX)	attr_tbl[IDX].fld.field6
# endif

# ifdef _DEBUG
# define ATT_LABEL_LIST_IDX(IDX)					       \
        ((AT_OBJ_CLASS(IDX) == Derived_Type) ?				       \
                attr_aux_tbl : attr_aux_var_error("ATT_LABEL_LIST_IDX", IDX))  \
                [IDX].fld.field2
# else
# define ATT_LABEL_LIST_IDX(IDX)	attr_aux_tbl[IDX].fld.field2
# endif

# ifdef _DEBUG
# define ATT_NON_DEFAULT_CPNT(IDX)					       \
	((AT_OBJ_CLASS(IDX) == Derived_Type) ?				       \
		attr_tbl : sytb_var_error("ATT_NON_DEFAULT_CPNT", IDX))	       \
		[IDX].fld.flag23
# else
# define ATT_NON_DEFAULT_CPNT(IDX)		attr_tbl[IDX].fld.flag23
# endif

# ifdef _DEBUG
# define ATT_NUM_CPNTS(IDX)						       \
	((AT_OBJ_CLASS(IDX) == Derived_Type) ?				       \
		attr_tbl : sytb_var_error("ATT_NUM_CPNTS", IDX))	       \
		[IDX].fld.field5
# else
# define ATT_NUM_CPNTS(IDX)		attr_tbl[IDX].fld.field5
# endif

# ifdef _DEBUG
# define ATT_NUMERIC_CPNT(IDX)						       \
	((AT_OBJ_CLASS(IDX) == Derived_Type) ?				       \
		attr_tbl : sytb_var_error("ATT_NUMERIC_CPNT", IDX))	       \
		[IDX].fld.flag21
# else
# define ATT_NUMERIC_CPNT(IDX)		attr_tbl[IDX].fld.flag21
# endif

# ifdef _DEBUG
# define ATT_POINTER_CPNT(IDX)						       \
	((AT_OBJ_CLASS(IDX) == Derived_Type) ?				       \
		attr_tbl : sytb_var_error("ATT_POINTER_CPNT", IDX))	       \
		[IDX].fld.flag19
# else
# define ATT_POINTER_CPNT(IDX)		attr_tbl[IDX].fld.flag19
# endif

#ifdef KEY /* Bug 6845 */
# ifdef _DEBUG
# define ATT_ALLOCATABLE_CPNT(IDX)					       \
	((AT_OBJ_CLASS(IDX) == Derived_Type) ?				       \
		attr_tbl : sytb_var_error("ATT_ALLOCATABLE_CPNT", IDX))	       \
		[IDX].fld.flag30
# else
# define ATT_ALLOCATABLE_CPNT(IDX)		attr_tbl[IDX].fld.flag30
# endif
#endif /* KEY Bug 6845 */

# ifdef _DEBUG
# define ATT_PRIVATE_CPNT(IDX)						       \
	((AT_OBJ_CLASS(IDX) == Derived_Type) ?				       \
		attr_tbl : sytb_var_error("ATT_PRIVATE_CPNT", IDX))	       \
		[IDX].fld.flag17
# else
# define ATT_PRIVATE_CPNT(IDX)		attr_tbl[IDX].fld.flag17
# endif

# ifdef _DEBUG
# define ATT_SCP_IDX(IDX)                                                      \
        ((AT_OBJ_CLASS(IDX) == Derived_Type) ?                                 \
		attr_aux_tbl : attr_aux_var_error("ATT_SCP_IDX", IDX))	       \
                [IDX].fld.field1
# else
# define ATT_SCP_IDX(IDX)               attr_aux_tbl[IDX].fld.field1
# endif

# ifdef _DEBUG
# define ATT_SEQUENCE_SET(IDX)						       \
	((AT_OBJ_CLASS(IDX) == Derived_Type) ?				       \
		attr_tbl : sytb_var_error("ATT_SEQUENCE_SET", IDX))	       \
		[IDX].fld.flag18
# else
# define ATT_SEQUENCE_SET(IDX)		attr_tbl[IDX].fld.flag18
# endif

# ifdef _DEBUG
# define ATT_STRUCT_BIT_LEN_FLD(IDX)					       \
	((AT_OBJ_CLASS(IDX) == Derived_Type) ? 				       \
		attr_tbl : sytb_var_error("ATT_STRUCT_BIT_LEN_FLD", IDX))      \
		[IDX].fld2.field22
# else
# define ATT_STRUCT_BIT_LEN_FLD(IDX)		attr_tbl[IDX].fld2.field22
# endif

# ifdef _DEBUG
# define ATT_STRUCT_BIT_LEN_IDX(IDX)					       \
	((AT_OBJ_CLASS(IDX) == Derived_Type) ? 				       \
		attr_tbl : sytb_var_error("ATT_STRUCT_BIT_LEN_IDX", IDX))      \
		[IDX].fld.field13
# else
# define ATT_STRUCT_BIT_LEN_IDX(IDX)		attr_tbl[IDX].fld.field13
# endif

#ifdef KEY /* Bug 5089 */
/* F2003: at least one "use" for this module in the current scope has the
 * "non_intrinsic" module-nature keyword */
# ifdef _DEBUG
# define ATT_NON_INTRIN(IDX)					               \
	((AT_OBJ_CLASS(IDX) == Pgm_Unit && ATP_PGM_UNIT(IDX) == Module) ?      \
		attr_tbl : sytb_var_error("ATT_NON_INTRIN", IDX))              \
		[IDX].fld.flag17
# else
# define ATT_NON_INTRIN(IDX)		attr_tbl[IDX].fld.flag17
# endif

/* F2003: at least one "use" for this module in the current scope has neither
 * the "intrinsic" nor the "non_intrinsic" module-nature keyword */
# ifdef _DEBUG
# define ATT_NO_MODULE_NATURE(IDX)					       \
	((AT_OBJ_CLASS(IDX) == Pgm_Unit && ATP_PGM_UNIT(IDX) == Module) ?      \
		attr_tbl : sytb_var_error("ATT_NO_MODULE_NATURE", IDX))        \
		[IDX].fld.flag18
# else
# define ATT_NO_MODULE_NATURE(IDX)	attr_tbl[IDX].fld.flag18
# endif
#endif /* KEY Bug 5089 */

# ifdef _DEBUG
# define ATT_UNIQUE_ID(IDX)						       \
	((AT_OBJ_CLASS(IDX) == Derived_Type) ?				       \
		attr_aux_tbl : attr_aux_var_error("ATT_UNIQUE_ID", IDX))       \
		[IDX].fld.field3
# else
# define ATT_UNIQUE_ID(IDX)		attr_aux_tbl[IDX].fld.field3
# endif



/*  ATTR LIST TABLE */

# define AL_ATTR_IDX(IDX)		attr_list_tbl[IDX].attr_idx
# define AL_EQ_IDX(IDX)			attr_list_tbl[IDX].attr_idx
# define AL_NEXT_IDX(IDX)		attr_list_tbl[IDX].next_idx
# define AL_IDX_IS_EQ(IDX)		attr_list_tbl[IDX].flag1
# define AL_FREE(IDX)			attr_list_tbl[IDX].flag2

/* prev_idx field is a special field available for specific */
/* uses and it is overloaded.  If you use it, give it a     */
/* specific name for its use and be careful.                */

# define AL_ENTRY_COUNT(IDX)		attr_list_tbl[IDX].prev_idx
# define AL_PREV_MODULE_IDX(IDX)	attr_list_tbl[IDX].prev_idx


/*  BOUNDS TABLE - Common */

# define BD_ARRAY_CLASS(IDX)		bounds_tbl[IDX].hdr.array_class
# define BD_ARRAY_SIZE(IDX)		bounds_tbl[IDX].hdr.array_size
# define BD_COLUMN_NUM(IDX)		bounds_tbl[IDX].hdr.column_num
# define BD_DCL_ERR(IDX)		bounds_tbl[IDX].hdr.error
# define BD_DIST_NTRY(IDX)		bounds_tbl[IDX].hdr.dist_ntry
# define BD_DISTRIBUTE_RESHAPE(IDX)	bounds_tbl[IDX].hdr.dist_reshape
# define BD_GLOBAL_IDX(IDX)		bounds_tbl[IDX].hdr.global_idx
# define BD_LEN_FLD(IDX)		bounds_tbl[IDX].hdr.len_fld
# define BD_LEN_IDX(IDX)		bounds_tbl[IDX].hdr.len_idx
# define BD_LINE_NUM(IDX)		bounds_tbl[IDX].hdr.line_num
# define BD_NEXT_FREE_NTRY(IDX)		bounds_tbl[IDX].hdr.next_free_ntry
# define BD_NTRY_SIZE(IDX)		bounds_tbl[IDX].hdr.ntry_size
# define BD_RANK(IDX)			bounds_tbl[IDX].hdr.rank
# define BD_RESOLVED(IDX)		bounds_tbl[IDX].hdr.resolved
# define BD_USED_NTRY(IDX)		bounds_tbl[IDX].hdr.used_ntry
# define BD_FLOW_DEPENDENT(IDX)		bounds_tbl[IDX].hdr.flow_dep


/*  BOUNDS TABLE - Dimensions */

# ifdef _DEBUG
# define BD_LB_FLD(IDX,DIM)						       \
	((!BD_DIST_NTRY(IDX)) ?		 				       \
		bounds_tbl : bd_var_error("BD_LB_FLD", IDX))		       \
                [(IDX)+(DIM)].dim.lb_fld
# else
# define BD_LB_FLD(IDX,DIM)		bounds_tbl[(IDX)+(DIM)].dim.lb_fld
# endif

# ifdef _DEBUG
# define BD_LB_IDX(IDX,DIM)						       \
	((!BD_DIST_NTRY(IDX)) ?		 				       \
		bounds_tbl : bd_var_error("BD_LB_IDX", IDX))		       \
                [(IDX)+(DIM)].dim.lb_idx
# else
# define BD_LB_IDX(IDX,DIM)		bounds_tbl[(IDX)+(DIM)].dim.lb_idx
# endif

# ifdef _DEBUG
# define BD_SM_IDX(IDX,DIM)						       \
	((!BD_DIST_NTRY(IDX)) ?		 				       \
		bounds_tbl : bd_var_error("BD_SM_IDX", IDX))		       \
                [(IDX)+(DIM)].dim.sm_idx
# else
# define BD_SM_IDX(IDX,DIM)		bounds_tbl[(IDX)+(DIM)].dim.sm_idx
# endif

# ifdef _DEBUG
# define BD_SM_FLD(IDX,DIM)						       \
	((!BD_DIST_NTRY(IDX)) ?		 				       \
		bounds_tbl : bd_var_error("BD_SM_FLD", IDX))		       \
                [(IDX)+(DIM)].dim.sm_fld
# else
# define BD_SM_FLD(IDX,DIM)		bounds_tbl[(IDX)+(DIM)].dim.sm_fld
# endif

# ifdef _DEBUG
# define BD_UB_FLD(IDX,DIM)						       \
	((!BD_DIST_NTRY(IDX)) ?		 				       \
		bounds_tbl : bd_var_error("BD_UB_FLD", IDX))		       \
                [(IDX)+(DIM)].dim.ub_fld
# else
# define BD_UB_FLD(IDX,DIM)		bounds_tbl[(IDX)+(DIM)].dim.ub_fld
# endif

# ifdef _DEBUG
# define BD_UB_IDX(IDX,DIM)						       \
	((!BD_DIST_NTRY(IDX)) ?		 				       \
		bounds_tbl : bd_var_error("BD_UB_IDX", IDX))		       \
                [(IDX)+(DIM)].dim.ub_idx
# else
# define BD_UB_IDX(IDX,DIM)		bounds_tbl[(IDX)+(DIM)].dim.ub_idx
# endif

# ifdef _DEBUG
# define BD_XT_FLD(IDX,DIM)						       \
	((!BD_DIST_NTRY(IDX)) ?		 				       \
		bounds_tbl : bd_var_error("BD_XT_FLD", IDX))		       \
                [(IDX)+(DIM)].dim.xt_fld
# else
# define BD_XT_FLD(IDX,DIM)		bounds_tbl[(IDX)+(DIM)].dim.xt_fld
# endif

# ifdef _DEBUG
# define BD_XT_IDX(IDX,DIM)						       \
	((!BD_DIST_NTRY(IDX)) ?		 				       \
		bounds_tbl : bd_var_error("BD_XT_IDX", IDX))		       \
                [(IDX)+(DIM)].dim.xt_idx
# else
# define BD_XT_IDX(IDX,DIM)		bounds_tbl[(IDX)+(DIM)].dim.xt_idx
# endif

/*  BOUNDS TABLE - Distribution Info */

# ifdef _DEBUG
# define BD_CYCLIC_FLD(IDX,DIM)						       \
	((BD_DIST_NTRY(IDX)) ?		 				       \
		bounds_tbl : bd_var_error("BD_CYCLIC_FLD", IDX))	       \
                [(IDX)+(DIM)].dist.cyclic_fld
# else
# define BD_CYCLIC_FLD(IDX,DIM)		bounds_tbl[(IDX)+(DIM)].dist.cyclic_fld
# endif

# ifdef _DEBUG
# define BD_CYCLIC_IDX(IDX,DIM)						       \
	((BD_DIST_NTRY(IDX)) ?		 				       \
		bounds_tbl : bd_var_error("BD_CYCLIC_IDX", IDX))	       \
                [(IDX)+(DIM)].dist.cyclic_idx
# else
# define BD_CYCLIC_IDX(IDX,DIM)		bounds_tbl[(IDX)+(DIM)].dist.cyclic_idx
# endif

# ifdef _DEBUG
# define BD_ONTO_FLD(IDX,DIM)						       \
	((BD_DIST_NTRY(IDX)) ?		 				       \
		bounds_tbl : bd_var_error("BD_ONTO_FLD", IDX))		       \
                [(IDX)+(DIM)].dist.onto_fld
# else
# define BD_ONTO_FLD(IDX,DIM)		bounds_tbl[(IDX)+(DIM)].dist.onto_fld
# endif

# ifdef _DEBUG
# define BD_ONTO_IDX(IDX,DIM)						       \
	((BD_DIST_NTRY(IDX)) ?		 				       \
		bounds_tbl : bd_var_error("BD_ONTO_IDX", IDX))		       \
                [(IDX)+(DIM)].dist.onto_idx
# else
# define BD_ONTO_IDX(IDX,DIM)		bounds_tbl[(IDX)+(DIM)].dist.onto_idx
# endif

# ifdef _DEBUG
# define BD_DISTRIBUTION(IDX,DIM)					       \
	((BD_DIST_NTRY(IDX)) ?		 				       \
		bounds_tbl : bd_var_error("BD_DISTRIBUTION", IDX))	       \
                [(IDX)+(DIM)].dist.distribution
# else
# define BD_DISTRIBUTION(IDX,DIM)    bounds_tbl[(IDX)+(DIM)].dist.distribution
# endif


/* CONSTANT TABLE, CONSTANT SEARCH TABLE and CONSTANT POOL definitions */

# define CS_CN_IDX(IDX)			const_search_tbl[IDX].const_tbl_idx

# define CN_BOZ_CONSTANT(IDX)		const_tbl[IDX].boz_constant
# define CN_BOOLEAN_CONSTANT(IDX)	const_tbl[IDX].boolean_constant
# define CN_HOLLERITH_ENDIAN(IDX)	const_tbl[IDX].hollerith_endian
# define CN_HOLLERITH_TYPE(IDX)		const_tbl[IDX].hollerith_fld
# define CN_EXTRA_ZERO_WORD(IDX)	const_tbl[IDX].extra_zero_word
# define CN_POOL_IDX(IDX)		const_tbl[IDX].const_pool_idx
# define CN_CONST(IDX)			const_pool[CN_POOL_IDX(IDX)]
# define CN_TYPE_IDX(IDX)		const_tbl[IDX].type_idx
# define CN_LEFT_CHILD(IDX)		const_tbl[IDX].left_child
# define CN_RIGHT_CHILD(IDX)		const_tbl[IDX].right_child
# define CN_BALANCE_FACTOR(IDX)		const_tbl[IDX].balance_factor

# define CP_CONSTANT(IDX)		const_pool[IDX]


/* EQUIV TABLE */

# define EQ_ATTR_IDX(IDX)               equiv_tbl[IDX].attr_idx
# define EQ_COLUMN_NUM(IDX)		equiv_tbl[IDX].column_num
# define EQ_DALIGN_ME(IDX)              equiv_tbl[IDX].dalign_me
# define EQ_DALIGN_SHIFT(IDX)		equiv_tbl[IDX].dalign_shift
# define EQ_DO_NOT_DALIGN(IDX)          equiv_tbl[IDX].do_not_dalign
# define EQ_ERROR(IDX)		        equiv_tbl[IDX].error
# define EQ_GRP_END_IDX(IDX)		equiv_tbl[IDX].grp_end_idx
# define EQ_GRP_IDX(IDX)		equiv_tbl[IDX].grp_idx
# define EQ_LINE_NUM(IDX)               equiv_tbl[IDX].line_num
# define EQ_LIST_IDX(IDX)               equiv_tbl[IDX].list_idx
# define EQ_MERGED(IDX)			equiv_tbl[IDX].merged
# define EQ_NEXT_EQUIV_GRP(IDX)         equiv_tbl[IDX].next_equiv_grp
# define EQ_NEXT_EQUIV_OBJ(IDX)         equiv_tbl[IDX].next_equiv_obj
# define EQ_OFFSET_FLD(IDX)		equiv_tbl[IDX].fld
# define EQ_OFFSET_IDX(IDX)		equiv_tbl[IDX].offset_idx
# define EQ_OPND_FLD(IDX)		equiv_tbl[IDX].opnd_fld
# define EQ_OPND_IDX(IDX)		equiv_tbl[IDX].opnd_idx
# define EQ_SEARCH_DONE(IDX)		equiv_tbl[IDX].search_done
# define EQ_SEMANTICS_DONE(IDX)		equiv_tbl[IDX].semantics_done
# define EQ_SUBSTRINGED(IDX)            equiv_tbl[IDX].substring


/* FILE PATH TABLE */

# define FP_CIF_ID(IDX)			file_path_tbl[IDX].cif_id
# define FP_CLASS(IDX)			file_path_tbl[IDX].file_class
# define FP_TMP_FILE(IDX)		file_path_tbl[IDX].tmp_file
# define FP_FILE_IDX(IDX)		file_path_tbl[IDX].file_idx
# define FP_MODULE_IDX(IDX)		file_path_tbl[IDX].next_idx
# define FP_MODULE_INLINE_IDX(IDX)	file_path_tbl[IDX].module_inline_idx
# define FP_NAME_IDX(IDX)		file_path_tbl[IDX].name_idx
# define FP_NAME_LEN(IDX)		file_path_tbl[IDX].name_len
# define FP_NAME(IDX)		        (str_pool[FP_NAME_IDX(IDX)].name_char)
# define FP_NAME_LONG(IDX)		((long *)&str_pool[FP_NAME_IDX(IDX)])
# define FP_NAME_PTR(IDX)		((char *)&str_pool[FP_NAME_IDX(IDX)])
# define FP_NEXT_FILE_IDX(IDX)		file_path_tbl[IDX].next_file_idx
# define FP_OFFSET(IDX)			file_path_tbl[IDX].offset
# define FP_OUTPUT_TO_O(IDX)		file_path_tbl[IDX].output_to_o
# define FP_SRCH_THE_FILE(IDX)		file_path_tbl[IDX].srch_the_file
# define FP_SYSTEM_FILE(IDX)		file_path_tbl[IDX].system_file

/*  GLOBAL ATTRIBUTE TABLE */

/* Common attribute definitions for all object classes */

# define GA_COMPILER_GEND(IDX)		global_attr_tbl[IDX].fld.compiler_gend
# define GA_DEF_LINE(IDX)		global_attr_tbl[IDX].fld.def_line
# define GA_DEF_COLUMN(IDX)		global_attr_tbl[IDX].fld.def_column
# define GA_DEFINED(IDX)		global_attr_tbl[IDX].fld.defined
# define GA_MODULE_IDX(IDX)		global_attr_tbl[IDX].fld.module_idx
# define GA_NAME_IDX(IDX)		global_attr_tbl[IDX].fld.name_idx
# define GA_NAME_LEN(IDX)		global_attr_tbl[IDX].fld.length
# define GA_OBJ_CLASS(IDX)		global_attr_tbl[IDX].fld.object_class
# define GA_OBJ_NAME_LONG(IDX)		((long *)&str_pool[GA_NAME_IDX(IDX)])
# define GA_OBJ_NAME_PTR(IDX)		((char *)&str_pool[GA_NAME_IDX(IDX)])
# define GA_OPTIONAL(IDX)		global_attr_tbl[IDX].fld.optional
# define GA_ORIG_NAME_LEN(IDX)		global_attr_tbl[IDX].fld.orig_name_len
# define GA_ORIG_NAME_IDX(IDX)		global_attr_tbl[IDX].fld.orig_name_idx
# define GA_ORIG_NAME_PTR(IDX)	      ((char *)&str_pool[GA_ORIG_NAME_IDX(IDX)])
# define GA_ORIG_NAME_LONG(IDX)	      ((long *)&str_pool[GA_ORIG_NAME_IDX(IDX)])
# define GA_REFERENCED(IDX)		global_attr_tbl[IDX].fld.referenced
# define GA_USE_ASSOCIATED(IDX)		global_attr_tbl[IDX].fld.use_associated
#ifdef KEY /* Bug 14150 */
# define GA_BIND_ATTR(IDX)		global_attr_tbl[IDX].fld.flag10
# define GA_BINDING_LABEL(IDX)		global_attr_tbl[IDX].fld.binding_label
#endif /* KEY Bug 14150 */

/* Definitions for data object class */

#ifdef KEY /* Bug 14110 */
# ifdef _DEBUG
# define GAD_VOLATILE(IDX)                                            \
       ((GA_OBJ_CLASS(IDX) == Data_Obj) ?                                     \
               global_attr_tbl : ga_var_error("GAD_ARRAY_ELEMENT_REF", IDX))  \
               [IDX].fld.flag7
# else
# define GAD_VOLATILE(IDX)     global_attr_tbl[IDX].fld.flag7
# endif
#endif /* KEY Bug 14110 */

# ifdef _DEBUG
# define GAD_ARRAY_ELEMENT_REF(IDX)					       \
	((GA_OBJ_CLASS(IDX) == Data_Obj) ?				       \
		global_attr_tbl : ga_var_error("GAD_ARRAY_ELEMENT_REF", IDX))  \
		[IDX].fld.flag5
# else
# define GAD_ARRAY_ELEMENT_REF(IDX)	global_attr_tbl[IDX].fld.flag5
# endif

# ifdef _DEBUG
# define GAD_ARRAY_IDX(IDX)						       \
	((GA_OBJ_CLASS(IDX) == Data_Obj) ?				       \
		global_attr_tbl : ga_var_error("GAD_ARRAY_IDX", IDX))	       \
		[IDX].fld.field3
# else
# define GAD_ARRAY_IDX(IDX)		global_attr_tbl[IDX].fld.field3
# endif

# ifdef _DEBUG
# define GAD_ASSUMED_SHAPE_ARRAY(IDX)					       \
	((GA_OBJ_CLASS(IDX) == Data_Obj) ?				       \
		global_attr_tbl : ga_var_error("GAD_ASSUMED_SHAPE_ARRAY", IDX))\
		[IDX].fld.flag2
# else
# define GAD_ASSUMED_SHAPE_ARRAY(IDX)	global_attr_tbl[IDX].fld.flag2
# endif

# ifdef _DEBUG
# define GAD_CLASS(IDX)							       \
	((GA_OBJ_CLASS(IDX) == Data_Obj) ?				       \
		global_attr_tbl : ga_var_error("GAD_CLASS", IDX))	       \
		[IDX].fld.field5
# else
# define GAD_CLASS(IDX)			global_attr_tbl[IDX].fld.field5
# endif

# ifdef _DEBUG
# define GAD_HOLLERITH(IDX)						       \
	(((GA_OBJ_CLASS(IDX) == Data_Obj) &&				       \
	  (GAD_CLASS(IDX) == Constant)) ?				       \
		global_attr_tbl : ga_var_error("GAD_HOLLERITH", IDX))	       \
		[IDX].fld.field4
# else
# define GAD_HOLLERITH(IDX)		global_attr_tbl[IDX].fld.field4
# endif

# ifdef _DEBUG
# define GAD_IGNORE_TKR(IDX)						       \
	((GA_OBJ_CLASS(IDX) == Data_Obj) ?				       \
		global_attr_tbl : ga_var_error("GAD_IGNORE_TKR", IDX))	       \
		[IDX].fld.flag1
# else
# define GAD_IGNORE_TKR(IDX)		global_attr_tbl[IDX].fld.flag1
# endif

# ifdef _DEBUG
# define GAD_INTENT(IDX)						       \
	(((GA_OBJ_CLASS(IDX) == Data_Obj) &&				       \
	  (GAD_CLASS(IDX) == Dummy_Argument)) ?				       \
		global_attr_tbl : ga_var_error("GAD_INTENT", IDX))	       \
		[IDX].fld.field4
# else
# define GAD_INTENT(IDX)		global_attr_tbl[IDX].fld.field4
# endif

# ifdef _DEBUG
# define GAD_NEXT_IDX(IDX)					 	       \
	((GA_OBJ_CLASS(IDX) == Data_Obj) ?				       \
		global_attr_tbl : ga_var_error("GAD_NEXT_IDX", IDX))	       \
		[IDX].fld.field8
# else
# define GAD_NEXT_IDX(IDX)		global_attr_tbl[IDX].fld.field8
# endif

# ifdef _DEBUG
# define GAD_POINTER(IDX)						       \
	((GA_OBJ_CLASS(IDX) == Data_Obj) ?				       \
		global_attr_tbl : ga_var_error("GAD_POINTER", IDX))	       \
		[IDX].fld.flag3
# else
# define GAD_POINTER(IDX)		global_attr_tbl[IDX].fld.flag3
# endif

# ifdef _DEBUG
# define GAD_RANK(IDX)							       \
	((GA_OBJ_CLASS(IDX) == Data_Obj) ?				       \
		global_attr_tbl : ga_var_error("GAD_RANK", IDX))	       \
		[IDX].fld.field2
# else
# define GAD_RANK(IDX)	global_attr_tbl[IDX].fld.field2
# endif

# ifdef _DEBUG
# define GAD_TARGET(IDX)					 	       \
	((GA_OBJ_CLASS(IDX) == Data_Obj) ?				       \
		global_attr_tbl : ga_var_error("GAD_TARGET", IDX))	       \
		[IDX].fld.flag4
# else
# define GAD_TARGET(IDX)		global_attr_tbl[IDX].fld.flag4
# endif

# ifdef _DEBUG
# define GAD_TYPE_IDX(IDX)					 	       \
	((GA_OBJ_CLASS(IDX) == Data_Obj) ?				       \
		global_attr_tbl : ga_var_error("GAD_TYPE_IDX", IDX))	       \
		[IDX].fld.field1
# else
# define GAD_TYPE_IDX(IDX)		global_attr_tbl[IDX].fld.field1
# endif

/* Definitions for common block class */

# ifdef _DEBUG
# define GAC_ALIGN_SYMBOL(IDX)						       \
	((GA_OBJ_CLASS(IDX) == Common_Block) ?				       \
		global_attr_tbl : ga_var_error("GAC_ALIGN_SYMBOL", IDX))       \
		[IDX].fld.flag5
# else
# define GAC_ALIGN_SYMBOL(IDX)		global_attr_tbl[IDX].fld.flag5
# endif

# ifdef _DEBUG
# define GAC_AUXILIARY(IDX)						       \
	((GA_OBJ_CLASS(IDX) == Common_Block) ?				       \
		global_attr_tbl : ga_var_error("GAC_AUXILIARY", IDX))	       \
		[IDX].fld.flag1
# else
# define GAC_AUXILIARY(IDX)		global_attr_tbl[IDX].fld.flag1
# endif

# ifdef _DEBUG
# define GAC_CACHE_ALIGN(IDX)						       \
	((GA_OBJ_CLASS(IDX) == Common_Block) ?				       \
		global_attr_tbl : ga_var_error("GAC_CACHE_ALIGN", IDX))        \
		[IDX].fld.flag2
# else
# define GAC_CACHE_ALIGN(IDX)		global_attr_tbl[IDX].fld.flag2
# endif

# ifdef _DEBUG
# define GAC_EQUIVALENCED(IDX)						       \
	((GA_OBJ_CLASS(IDX) == Common_Block) ?				       \
		global_attr_tbl : ga_var_error("GAC_EQUIVALENCED", IDX))       \
		[IDX].fld.flag4
# else
# define GAC_EQUIVALENCED(IDX)		global_attr_tbl[IDX].fld.flag4
# endif

# ifdef _DEBUG
# define GAC_FILL_SYMBOL(IDX)						       \
	((GA_OBJ_CLASS(IDX) == Common_Block) ?				       \
		global_attr_tbl : ga_var_error("GAC_FILL_SYMBOL", IDX))        \
		[IDX].fld.flag6
# else
# define GAC_FILL_SYMBOL(IDX)		global_attr_tbl[IDX].fld.flag6
# endif

# ifdef _DEBUG
# define GAC_FIRST_MEMBER_IDX(IDX)					       \
	((GA_OBJ_CLASS(IDX) == Common_Block) ?				       \
		global_attr_tbl : ga_var_error("GAC_FIRST_MEMBER_IDX", IDX))   \
		[IDX].fld.field3
# else
# define GAC_FIRST_MEMBER_IDX(IDX)	global_attr_tbl[IDX].fld.field3
# endif

# ifdef _DEBUG
# define GAC_FOUND_DIFFS(IDX)						       \
	((GA_OBJ_CLASS(IDX) == Common_Block) ?				       \
		global_attr_tbl : ga_var_error("GAC_FOUND_DIFFS", IDX))        \
		[IDX].fld.flag7
# else
# define GAC_FOUND_DIFFS(IDX)		global_attr_tbl[IDX].fld.flag7
# endif

# ifdef _DEBUG
# define GAC_PGM_UNIT_IDX(IDX)					 	       \
	((GA_OBJ_CLASS(IDX) == Common_Block) ?				       \
		global_attr_tbl : ga_var_error("GAC_PGM_UNIT_IDX", IDX))       \
		[IDX].fld.field1
# else
# define GAC_PGM_UNIT_IDX(IDX)		global_attr_tbl[IDX].fld.field1
# endif

# ifdef _DEBUG
# define GAC_SECTION_GP(IDX)						       \
	((GA_OBJ_CLASS(IDX) == Common_Block) ?				       \
		global_attr_tbl : ga_var_error("GAC_SECTION_GP", IDX))	       \
		[IDX].fld.flag8
# else
# define GAC_SECTION_GP(IDX)		global_attr_tbl[IDX].fld.flag8
# endif

# ifdef _DEBUG
# define GAC_SECTION_NON_GP(IDX)					       \
	((GA_OBJ_CLASS(IDX) == Common_Block) ?				       \
		global_attr_tbl : ga_var_error("GAC_SECTION_NON_GP", IDX))     \
		[IDX].fld.flag9
# else
# define GAC_SECTION_NON_GP(IDX)	global_attr_tbl[IDX].fld.flag9
# endif

# ifdef _DEBUG
# define GAC_TASK_COMMON(IDX)						       \
	((GA_OBJ_CLASS(IDX) == Common_Block) ?				       \
		global_attr_tbl : ga_var_error("GAC_TASK_COMMON", IDX))	       \
		[IDX].fld.flag3
# else
# define GAC_TASK_COMMON(IDX)		global_attr_tbl[IDX].fld.flag3
# endif


/* Definitions for program unit class */

# ifdef _DEBUG
# define GAP_ELEMENTAL(IDX)						       \
	((GA_OBJ_CLASS(IDX) == Pgm_Unit) ?				       \
		global_attr_tbl : ga_var_error("GAP_ELEMENTAL", IDX))	       \
		[IDX].fld.flag3
# else
# define GAP_ELEMENTAL(IDX)		global_attr_tbl[IDX].fld.flag3
# endif

# ifdef _DEBUG
# define GAP_FIRST_IDX(IDX)						       \
	((GA_OBJ_CLASS(IDX) == Pgm_Unit) ? 				       \
		global_attr_tbl : ga_var_error("GAP_FIRST_IDX", IDX))	       \
		[IDX].fld.field1
# else
# define GAP_FIRST_IDX(IDX)		global_attr_tbl[IDX].fld.field1
# endif

# ifdef _DEBUG
# define GAP_FP_IDX(IDX)						       \
	((GA_OBJ_CLASS(IDX) == Pgm_Unit) ? 				       \
		global_attr_tbl : ga_var_error("GAP_FP_IDX", IDX))	       \
		[IDX].fld.field6
# else
# define GAP_FP_IDX(IDX)		global_attr_tbl[IDX].fld.field6
# endif

# ifdef _DEBUG
# define GAP_GLOBAL_DIR(IDX)						       \
	((GA_OBJ_CLASS(IDX) == Pgm_Unit) ? 				       \
		global_attr_tbl : ga_var_error("GAP_GLOBAL_DIR", IDX))	       \
		[IDX].fld.flag9
# else
# define GAP_GLOBAL_DIR(IDX)		global_attr_tbl[IDX].fld.flag9
# endif

# ifdef _DEBUG
# define GAP_IN_INTERFACE_BLK(IDX)					       \
	((GA_OBJ_CLASS(IDX) == Pgm_Unit) ?				       \
		global_attr_tbl : ga_var_error("GAP_IN_INTERFACE_BLK", IDX))   \
		[IDX].fld.flag2
# else
# define GAP_IN_INTERFACE_BLK(IDX)	global_attr_tbl[IDX].fld.flag2
# endif

# ifdef _DEBUG
# define GAP_NEEDS_EXPL_ITRFC(IDX)					       \
	((GA_OBJ_CLASS(IDX) == Pgm_Unit) ?				       \
		global_attr_tbl : ga_var_error("GAP_NEEDS_EXPL_ITRFC", IDX))   \
		[IDX].fld.flag1
# else
# define GAP_NEEDS_EXPL_ITRFC(IDX)	global_attr_tbl[IDX].fld.flag1
# endif

# ifdef _DEBUG
# define GAP_NEXT_PGM_UNIT_IDX(IDX)					       \
	((GA_OBJ_CLASS(IDX) == Pgm_Unit) ? 				       \
		global_attr_tbl : ga_var_error("GAP_NEXT_PGM_UNIT_IDX", IDX))  \
		[IDX].fld.field8
# else
# define GAP_NEXT_PGM_UNIT_IDX(IDX)	global_attr_tbl[IDX].fld.field8
# endif

# ifdef _DEBUG
# define GAP_NOSIDE_EFFECTS(IDX)					       \
	((GA_OBJ_CLASS(IDX) == Pgm_Unit) ?				       \
		global_attr_tbl : ga_var_error("GAP_NOSIDE_EFFECTS", IDX))     \
		[IDX].fld.flag5
# else
# define GAP_NOSIDE_EFFECTS(IDX)	global_attr_tbl[IDX].fld.flag5
# endif

# ifdef _DEBUG
# define GAP_NUM_DARGS(IDX)						       \
	((GA_OBJ_CLASS(IDX) == Pgm_Unit) ?				       \
		global_attr_tbl : ga_var_error("GAP_NUM_DARGS", IDX))	       \
		[IDX].fld.field7
# else
# define GAP_NUM_DARGS(IDX)		global_attr_tbl[IDX].fld.field7
# endif

# ifdef _DEBUG
# define GAP_PGM_UNIT(IDX)						       \
	((GA_OBJ_CLASS(IDX) == Pgm_Unit) ?				       \
		global_attr_tbl : ga_var_error("GAP_PGM_UNIT", IDX))	       \
		[IDX].fld.field5
# else
# define GAP_PGM_UNIT(IDX)		global_attr_tbl[IDX].fld.field5
# endif

# ifdef _DEBUG
# define GAP_PGM_UNIT_DEFINED(IDX)					       \
	((GA_OBJ_CLASS(IDX) == Pgm_Unit) ?				       \
		global_attr_tbl : ga_var_error("GAP_PGM_UNIT_DEFINED", IDX))   \
		[IDX].fld.flag4
# else
# define GAP_PGM_UNIT_DEFINED(IDX)	global_attr_tbl[IDX].fld.flag4
# endif

# ifdef _DEBUG
# define GAP_PURE(IDX)							       \
	((GA_OBJ_CLASS(IDX) == Pgm_Unit) ?				       \
		global_attr_tbl : ga_var_error("GAP_PURE", IDX))	       \
		[IDX].fld.flag6
# else
# define GAP_PURE(IDX)			global_attr_tbl[IDX].fld.flag6
# endif

# ifdef _DEBUG
# define GAP_RECURSIVE(IDX)						       \
	((GA_OBJ_CLASS(IDX) == Pgm_Unit) ?				       \
		global_attr_tbl : ga_var_error("GAP_RECURSIVE", IDX))	       \
		[IDX].fld.flag7
# else
# define GAP_RECURSIVE(IDX)		global_attr_tbl[IDX].fld.flag7
# endif

# ifdef _DEBUG
# define GAP_RSLT_IDX(IDX)						       \
	((GA_OBJ_CLASS(IDX) == Pgm_Unit) ? 				       \
		global_attr_tbl : ga_var_error("GAP_RSLT_IDX", IDX))	       \
		[IDX].fld.field3
# else
# define GAP_RSLT_IDX(IDX)		global_attr_tbl[IDX].fld.field3
# endif

# ifdef _DEBUG
# define GAP_INLINE_STATE(IDX)						       \
	((GA_OBJ_CLASS(IDX) == Pgm_Unit) ?				       \
		global_attr_tbl : ga_var_error("GAP_INLINE_STATE", IDX))       \
		[IDX].fld.field2
# else
# define GAP_INLINE_STATE(IDX)		global_attr_tbl[IDX].fld.field2
# endif

# ifdef _DEBUG
# define GAP_VFUNCTION(IDX)						       \
	((GA_OBJ_CLASS(IDX) == Pgm_Unit) ?				       \
		global_attr_tbl : ga_var_error("GAP_VFUNCTION", IDX))	       \
		[IDX].fld.flag8
# else
# define GAP_VFUNCTION(IDX)		global_attr_tbl[IDX].fld.flag8
# endif


/* definitions for derived types - GAT */

# ifdef _DEBUG
# define GAT_FIRST_CPNT_IDX(IDX)					       \
	((GA_OBJ_CLASS(IDX) == Derived_Type) ?				       \
		global_attr_tbl : ga_var_error("GAT_FIRST_CPNT_IDX", IDX))     \
		[IDX].fld.field1
# else
# define GAT_FIRST_CPNT_IDX(IDX)	global_attr_tbl[IDX].fld.field1
# endif

# ifdef _DEBUG
# define GAT_NUM_CPNTS(IDX)						       \
	((GA_OBJ_CLASS(IDX) == Derived_Type) ?				       \
		global_attr_tbl : ga_var_error("GAT_NUM_CPNTS", IDX))          \
		[IDX].fld.field3
# else
# define GAT_NUM_CPNTS(IDX)		global_attr_tbl[IDX].fld.field3
# endif

# ifdef _DEBUG
# define GAT_PRIVATE_CPNT(IDX)						       \
	((GA_OBJ_CLASS(IDX) == Derived_Type) ?				       \
		global_attr_tbl : ga_var_error("GAT_PRIVATE_CPNT", IDX))       \
		[IDX].fld.flag1
# else
# define GAT_PRIVATE_CPNT(IDX)		global_attr_tbl[IDX].fld.flag1
# endif

# ifdef _DEBUG
# define GAT_SEQUENCE_SET(IDX)						       \
	((GA_OBJ_CLASS(IDX) == Derived_Type) ?				       \
		global_attr_tbl : ga_var_error("GAT_SEQUENCE_SET", IDX))       \
		[IDX].fld.flag2
# else
# define GAT_SEQUENCE_SET(IDX)		global_attr_tbl[IDX].fld.flag2
# endif

# ifdef _DEBUG
# define GAT_STRUCT_LIN_TYPE(IDX)					       \
	((GA_OBJ_CLASS(IDX) == Derived_Type) ?				       \
		global_attr_tbl : ga_var_error("GAT_STRUCT_LIN_TYPE", IDX))    \
		[IDX].wd.field32_6
# else
# define GAT_STRUCT_LIN_TYPE(IDX)	global_attr_tbl[IDX].wd.field32_6
# endif

# ifdef _DEBUG
# define GAT_STRUCT_BIT_LEN(IDX)					       \
	((GA_OBJ_CLASS(IDX) == Derived_Type) ? 				       \
		global_attr_tbl : ga_var_error("GAT_STRUCT_BIT_LEN", IDX))     \
		[IDX].wd.length
# else
# define GAT_STRUCT_BIT_LEN(IDX)	global_attr_tbl[IDX].wd.length
# endif

/*  GLOBAL BOUNDS TABLE - Common */

# define GB_ARRAY_CLASS(IDX)		global_bounds_tbl[IDX].hdr.array_class
# define GB_ARRAY_SIZE(IDX)		global_bounds_tbl[IDX].hdr.array_size
# define GB_RANK(IDX)			global_bounds_tbl[IDX].hdr.rank

/*  GLOBAL BOUNDS TABLE - Dimensions */

# define GB_LOWER_BOUND(IDX,DIM)	global_bounds_tbl[(IDX)+(DIM*3)-2].len
# define GB_UPPER_BOUND(IDX,DIM)	global_bounds_tbl[(IDX)+(DIM*3)-1].len
# define GB_LB_TYPE(IDX,DIM)	global_bounds_tbl[(IDX)+(DIM*3)].type.lb_type
# define GB_UB_TYPE(IDX,DIM)	global_bounds_tbl[(IDX)+(DIM*3)].type.ub_type

/* GLOBAL LINE TABLE */

# define GL_CIF_FILE_ID(IDX)		global_line_tbl[IDX].cif_file_id
# define GL_FILE_LINE(IDX)		global_line_tbl[IDX].file_line
# define GL_FILE_NAME_IDX(IDX)		global_line_tbl[IDX].file_name_idx
# define GL_FILE_NAME_LEN(IDX)		global_line_tbl[IDX].file_name_len
# define GL_GLOBAL_LINE(IDX)		global_line_tbl[IDX].global_line
# define GL_INCLUDE_FILE_COL(IDX)	global_line_tbl[IDX].incld_file_col
# define GL_INCLUDE_FILE_LINE(IDX)	global_line_tbl[IDX].incld_file_line
# define GL_PATH_NAME_IDX(IDX)		global_line_tbl[IDX].path_name_idx
# define GL_PATH_NAME_LEN(IDX)		global_line_tbl[IDX].path_name_len
# define GL_SOURCE_LINES(IDX)		global_line_tbl[IDX].source_lines

# define GL_FILE_NAME_LONG(IDX)	      ((long *)&str_pool[GL_FILE_NAME_IDX(IDX)])
# define GL_FILE_NAME_PTR(IDX)	      ((char *)&str_pool[GL_FILE_NAME_IDX(IDX)])

# define GL_PATH_NAME_LONG(IDX)	      ((long *)&str_pool[GL_PATH_NAME_IDX(IDX)])
# define GL_PATH_NAME_PTR(IDX)	      ((char *)&str_pool[GL_PATH_NAME_IDX(IDX)])


/* GLOBAL NAME TABLE */

# define GN_ATTR_IDX(IDX)		global_name_tbl[IDX].attr_idx
# define GN_NAME_IDX(IDX)		global_name_tbl[IDX].name_idx
# define GN_NAME_LEN(IDX)		global_name_tbl[IDX].name_len
# define GN_NAME_LONG(IDX)		((long *)&str_pool[GN_NAME_IDX(IDX)])
# define GN_NAME_PTR(IDX)		((char *)&str_pool[GN_NAME_IDX(IDX)])

/* GLOBAL TYPE TABLE */

# define GT_BIT_LEN(IDX)		global_type_tbl[IDX].fld.bit_len
# define GT_CHAR_CLASS(IDX)		global_type_tbl[IDX].fld.char_class
# define GT_DCL_VALUE(IDX)		global_type_tbl[IDX].fld.dcl_value
# define GT_DESC(IDX)			global_type_tbl[IDX].fld.desc
# define GT_STRUCT_IDX(IDX)		global_type_tbl[IDX].fld.type_idx
# define GT_LENGTH(IDX)			global_type_tbl[IDX].wd.length
# define GT_LENGTH_LIN_TYPE(IDX)	global_type_tbl[IDX].fld.len_lin_type
# define GT_LINEAR_TYPE(IDX)		global_type_tbl[IDX].fld.linear_type
# define GT_PTR_INCREMENT(IDX) \
                          global_type_tbl[IDX].wd.length[MAX_WORDS_FOR_INTEGER]
# define GT_TYPE(IDX)			global_type_tbl[IDX].fld.type

/* HIDDEN NAME TABLE */

# define HN_ATTR_IDX(IDX)		hidden_name_tbl[IDX].attr_idx
# define HN_NAME_IDX(IDX)		hidden_name_tbl[IDX].name_idx
# define HN_NAME_LEN(IDX)		hidden_name_tbl[IDX].name_len
# define HN_NAME_PTR(IDX)		((char *)&name_pool[HN_NAME_IDX(IDX)])


/* SCOPE TABLE definitions */

# define SCP_ATTR_IDX(IDX)		scp_tbl[IDX].wd[0].fld1.field2
# define SCP_LN_FW_IDX(IDX)		scp_tbl[IDX].wd[1].fld1.field2
# define SCP_LN_LW_IDX(IDX)		scp_tbl[IDX].wd[2].fld1.field2
# define SCP_FIRST_SH_IDX(IDX)		scp_tbl[IDX].wd[3].fld1.field2
# define SCP_LAST_SH_IDX(IDX)		scp_tbl[IDX].wd[4].fld1.field2
# define SCP_EXIT_IR_SH_IDX(IDX)	scp_tbl[IDX].wd[5].fld1.field2
# define SCP_ASSIGN_LBL_CHAIN(IDX)	scp_tbl[IDX].wd[6].fld1.field2
# define SCP_CIF_ID(IDX)		scp_tbl[IDX].wd[7].fld1.field2
# define SCP_HN_FW_IDX(IDX)		scp_tbl[IDX].wd[8].fld1.field2
# define SCP_HN_LW_IDX(IDX)		scp_tbl[IDX].wd[9].fld1.field2
# define SCP_OPTIONAL_CHAR_TMP(IDX)	scp_tbl[IDX].wd[10].fld1.field2
# define SCP_RETURN_LABEL(IDX)		scp_tbl[IDX].wd[11].fld1.field2
# define SCP_COPY_ASSUMED_LIST(IDX)	scp_tbl[IDX].wd[12].fld1.field2
# define SCP_WHICH_ENTRY_TMP(IDX)	scp_tbl[IDX].wd[13].fld1.field2
# define SCP_RESHAPE_ARRAY_LIST(IDX)	scp_tbl[IDX].wd[14].fld1.field2

# define SCP_DEFAULT_STORAGE(IDX)	scp_tbl[IDX].wd[24].fld2.field4
# define SCP_LEVEL(IDX)			scp_tbl[IDX].wd[23].fld2.field4
# define SCP_CIF_ERR_LIST(IDX)		scp_tbl[IDX].wd[23].fld2.field5

# define SCP_IS_INTERFACE(IDX)		scp_tbl[IDX].wd[0].fld1.flag1
# define SCP_IN_ERR(IDX)		scp_tbl[IDX].wd[0].fld1.flag2
# define SCP_IS_USED_PROC(IDX)		scp_tbl[IDX].wd[0].fld1.flag3
# define SCP_IMPL_NONE(IDX)		scp_tbl[IDX].wd[1].fld1.flag1
# define SCP_PARENT_NONE(IDX)		scp_tbl[IDX].wd[1].fld1.flag2
# define SCP_COPY_ASSUMED_SHAPE(IDX)    scp_tbl[IDX].wd[1].fld1.flag3
# define SCP_IGNORE_TKR(IDX)		scp_tbl[IDX].wd[2].fld1.flag1
# define SCP_HAS_CALLS(IDX)		scp_tbl[IDX].wd[2].fld1.flag2
# define SCP_DOES_IO(IDX)		scp_tbl[IDX].wd[2].fld1.flag3
#ifdef KEY /* Bug 5089 */
/* Uses intrinsic module ieee_features, ieee_arithmetic, or ieee_exceptions
 * and therefore needs to save/restore FPU state on entry/exit */
# define SCP_USES_IEEE(IDX)		scp_tbl[IDX].wd[3].fld1.flag1
#endif /* KEY Bug 5089 */
#ifdef KEY /* Bug 11741 */
/* Scope is interface body having "import" sans explicit list of identifiers */
# define SCP_IMPORT(IDX)		scp_tbl[IDX].wd[4].fld1.flag1
#endif /* KEY Bug 11741 */
# define SCP_INLINE_SGI(IDX)		scp_tbl[IDX].wd[3].fld1.flag2
# define SCP_NOINLINE_SGI(IDX)		scp_tbl[IDX].wd[3].fld1.flag3
# define SCP_DBG_PRINT_SYTB(IDX)	scp_tbl[IDX].wd[25].fld1.flag1
# define SCP_DBG_PRINT_STMT(IDX)	scp_tbl[IDX].wd[25].fld1.flag2

# define SCP_SIBLING_IDX(IDX)		scp_tbl[IDX].wd[0].fld1.field3
# define SCP_LAST_CHILD_IDX(IDX)	scp_tbl[IDX].wd[1].fld1.field3
# define SCP_TMP_FW_IDX2(IDX)		scp_tbl[IDX].wd[2].fld1.field3
# define SCP_PARENT_IDX(IDX)		scp_tbl[IDX].wd[3].fld1.field3
# define SCP_FIRST_CHILD_IDX(IDX)	scp_tbl[IDX].wd[4].fld1.field3
# define SCP_NUM_CHILDREN(IDX)		scp_tbl[IDX].wd[5].fld1.field3
# define SCP_DARG_LIST(IDX)		scp_tbl[IDX].wd[6].fld1.field3
# define SCP_ENTRY_IDX(IDX)		scp_tbl[IDX].wd[7].fld1.field3
# define SCP_USED_MODULE_LIST(IDX)	scp_tbl[IDX].wd[8].fld1.field3
# define SCP_FIRST_EQUIV_GRP(IDX)	scp_tbl[IDX].wd[9].fld1.field3
# define SCP_TMP_LIST(IDX)		scp_tbl[IDX].wd[10].fld1.field3
# define SCP_TMP_FW_IDX(IDX)		scp_tbl[IDX].wd[11].fld1.field3
# define SCP_SB_HOSTED_STACK_IDX(IDX)	scp_tbl[IDX].wd[12].fld1.field3
# define SCP_SB_STACK_IDX(IDX)		scp_tbl[IDX].wd[13].fld1.field3
# define SCP_SB_BASED_IDX(IDX)		scp_tbl[IDX].wd[14].fld1.field3
# define SCP_SB_DARG_IDX(IDX)		scp_tbl[IDX].wd[15].fld1.field3
# define SCP_SB_STATIC_IDX(IDX)		scp_tbl[IDX].wd[16].fld1.field3
# define SCP_SB_SYMMETRIC_IDX(IDX)	scp_tbl[IDX].wd[17].fld1.field3
# define SCP_ATTR_LIST_END(IDX)		scp_tbl[IDX].wd[18].fld1.field3
# define SCP_SB_HOSTED_STATIC_IDX(IDX)	scp_tbl[IDX].wd[19].fld1.field3
# define SCP_SB_STATIC_INIT_IDX(IDX)	scp_tbl[IDX].wd[20].fld1.field3
# define SCP_ATTR_LIST(IDX)		scp_tbl[IDX].wd[21].fld1.field3
# define SCP_ALT_ENTRY_CNT(IDX)		scp_tbl[IDX].wd[22].fld1.field3
# define SCP_SB_STATIC_UNINIT_IDX(IDX)	scp_tbl[IDX].wd[23].fld1.field3
# define SCP_SB_HOSTED_DATA_IDX(IDX)	scp_tbl[IDX].wd[24].fld1.field3
# define SCP_FILE_PATH_IDX(IDX)		scp_tbl[IDX].wd[25].fld1.field3

/* IMPLICIT TABLE definitions */

# define IMPL_IDX(CH)			CH - 'A'
# define IM_SET(SIDX,IDX)		scp_tbl[SIDX].wd[IDX].fld1.typed
# define IM_STORAGE(SIDX,IDX)		scp_tbl[SIDX].wd[IDX].fld1.storage
# define IM_TYPE_IDX(SIDX,IDX)		scp_tbl[SIDX].wd[IDX].fld1.type_idx

/* IR TABLE */
# define OLD_IR_OPR(IDX)                old_ir_tbl[IDX].opr.the_operator
# define OLD_IR_RANK(IDX)               old_ir_tbl[IDX].opr.rank
# define OLD_IR_DV_DIM(IDX)		old_ir_tbl[IDX].opr.dim

# define IR_COL_NUM(IDX)                ir_tbl[IDX].opr.col_num
# define IR_LINE_NUM(IDX)               ir_tbl[IDX].opr.line_num
# define IR_NEXT_IDX(IDX)               ir_tbl[IDX].opr.line_num
# define IR_OPR(IDX)                    ir_tbl[IDX].opr.the_operator
# define IR_TYPE_IDX(IDX)		ir_tbl[IDX].opr.type_idx

# define IR_RANK(IDX)                   ir_tbl[IDX].opr.rank
# define IR_DV_DIM(IDX)			ir_tbl[IDX].opr.dim
#ifdef KEY /* Bug6845 */
/* When dope vector represents allocatable array whose element type is a
 * derived type having component(s) which are themselves allocatable, this
 * counts the allocatable compnents */
# define IR_DV_N_ALLOC_CPNT(IDX)	ir_tbl[IDX].opr.n_alloc_cpnt
#endif /* KEY Bug6845 */
# define IR_CONTIG_ARRAY(IDX)		ir_tbl[IDX].opr.dim
# define IR_WHOLE_ARRAY(IDX)		ir_tbl[IDX].opr.dim
# define IR_INLINE_STATE(IDX)		ir_tbl[IDX].opr.dim

# define IR_COL_NUM_L(IDX)              ir_tbl[IDX].op_l.col_num
# define IR_FLD_L(IDX)                  ir_tbl[IDX].op_l.fld
# define IR_IDX_L(IDX)                  ir_tbl[IDX].op_l.idx
# define IR_LINE_NUM_L(IDX)             ir_tbl[IDX].op_l.line_num
# define IR_LIST_CNT_L(IDX)             ir_tbl[IDX].op_l.line_num
# define IR_SHORT_CIRCUIT_L(IDX)	ir_tbl[IDX].op_l.flag_1
# define IR_ARRAY_SYNTAX(IDX)		ir_tbl[IDX].op_l.flag_2

# define IR_COL_NUM_R(IDX)              ir_tbl[IDX].op_r.col_num
# define IR_FLD_R(IDX)                  ir_tbl[IDX].op_r.fld
# define IR_IDX_R(IDX)                  ir_tbl[IDX].op_r.idx
# define IR_LINE_NUM_R(IDX)             ir_tbl[IDX].op_r.line_num
# define IR_LIST_CNT_R(IDX)             ir_tbl[IDX].op_r.line_num
# define IR_SHORT_CIRCUIT_R(IDX)	ir_tbl[IDX].op_r.flag_1
# define IR_BOUNDS_DONE(IDX)		ir_tbl[IDX].op_r.flag_2

# define IR_OPND_L(IDX)			ir_tbl[IDX].op_l
# define IR_OPND_R(IDX)			ir_tbl[IDX].op_r


/* IR LIST TABLE */

# define IL_NEXT_LIST_IDX(IDX)          ir_list_tbl[IDX].il.link.nxt_idx
# define IL_IDX(IDX)                    ir_list_tbl[IDX].il.op.idx
# define IL_LINE_NUM(IDX)               ir_list_tbl[IDX].il.op.line_num
# define IL_COL_NUM(IDX)                ir_list_tbl[IDX].il.op.col_num
# define IL_LIST_CNT(IDX)               ir_list_tbl[IDX].il.op.line_num
# define IL_FLD(IDX)                    ir_list_tbl[IDX].il.op.fld
# define IL_ARG_DESC_VARIANT(IDX)	ir_list_tbl[IDX].il.link.arg_desc

# ifdef _DEBUG
# define IL_PREV_LIST_IDX(IDX)                                                \
         (! IL_ARG_DESC_VARIANT(IDX) ?                                        \
                ir_list_tbl : ir_list_var_error("IL_PREV_LIST_IDX", IDX))     \
                [IDX].il.link.prev_idx
# else 
# define IL_PREV_LIST_IDX(IDX)          ir_list_tbl[IDX].il.link.prev_idx
# endif

# ifdef _DEBUG
# define IL_ARG_DESC_IDX(IDX)                                                 \
         (IL_ARG_DESC_VARIANT(IDX) ?                                          \
                ir_list_tbl : ir_list_var_error("IL_ARG_DESC_IDX", IDX))      \
                [IDX].il.link.prev_idx
# else 
# define IL_ARG_DESC_IDX(IDX)           ir_list_tbl[IDX].il.link.prev_idx
# endif

/* flags for array subscript lists */
# define IL_VECTOR_SUBSCRIPT(IDX)       ir_list_tbl[IDX].il.link.flag_1
# define IL_CONSTANT_SUBSCRIPT(IDX)     ir_list_tbl[IDX].il.link.flag_2
# define IL_PE_SUBSCRIPT(IDX)		ir_list_tbl[IDX].il.link.flag_3

/* flags for io control lists */
# define IL_NAMELIST_EXPECTED(IDX)      ir_list_tbl[IDX].il.link.flag_1
# define IL_FORMAT_EXPECTED(IDX)        ir_list_tbl[IDX].il.link.flag_2

/* flags for io list */
# define IL_HAS_FUNCTIONS(IDX)          ir_list_tbl[IDX].il.link.flag_1
# define IL_MUST_BE_LOOP(IDX)           ir_list_tbl[IDX].il.link.flag_2
# define IL_MUST_FLATTEN(IDX)           ir_list_tbl[IDX].il.link.flag_3
# define IL_HAS_CONSTRUCTOR(IDX)        ir_list_tbl[IDX].il.link.flag_4
# define IL_STRUCT_REF(IDX)             ir_list_tbl[IDX].il.link.flag_5
# define IL_INTRIN_PLACE_HOLDER(IDX)    ir_list_tbl[IDX].il.link.flag_6
# define IL_NONDEFAULT_IMP_DO_LCV(IDX)	ir_list_tbl[IDX].il.link.flag_7
# define IL_DISTRIBUTION_VARIANT(IDX)	ir_list_tbl[IDX].il.link.flag_8

/* Flags for label forward ref lists.  See MISC above for values. */
# define IL_FORWARD_REF(IDX)		ir_list_tbl[IDX].il.link.for_ref
# define IL_DISTRIBUTION(IDX)		ir_list_tbl[IDX].il.link.for_ref

# define IL_WORD(IDX,WD)                ir_list_tbl[IDX].words[WD]
# define IL_ELEMENT(IDX)                ir_list_tbl[IDX].il_long64.lwd2

# define IL_OPND(IDX)			ir_list_tbl[IDX].il.op


/* STMT HEADER TABLE */

# define SH_CIF_SKIP_ME(IDX)		sh_tbl[IDX].cif_skip_me
# define SH_COL_NUM(IDX)		sh_tbl[IDX].col_num
# define SH_COMPILER_GEN(IDX)		sh_tbl[IDX].compiler_gen
# define SH_DOALL_LOOP_END(IDX)		sh_tbl[IDX].doall_loop_end
# define SH_ERR_FLG(IDX)	        sh_tbl[IDX].stmt_parse_err
# define SH_GLB_LINE(IDX)               sh_tbl[IDX].glb_line_num
# define SH_ACTION_STMT(IDX)            sh_tbl[IDX].action_stmt
# define SH_INLINING_ATTEMPTED(IDX)     sh_tbl[IDX].inlining_attempted
# define SH_IR_IDX(IDX)                 sh_tbl[IDX].ir_idx
# define SH_LABELED(IDX)		sh_tbl[IDX].labeled
# define SH_LOOP_END(IDX)		sh_tbl[IDX].loop_end
# define SH_NEXT_IDX(IDX)               sh_tbl[IDX].next_sh_idx
# define SH_P2_SKIP_ME(IDX)		sh_tbl[IDX].skip_pass_2
# define SH_PARENT_BLK_IDX(IDX)         sh_tbl[IDX].parent_blk_idx
# define SH_PREV_IDX(IDX)               sh_tbl[IDX].prev_sh_idx
# define OLD_SH_STMT_TYPE(IDX)          sh_tbl[IDX].old_stmt_type
# define SH_STMT_TYPE(IDX)              sh_tbl[IDX].stmt_type


/* Global ir,il, and sh tables */

/* GLOBAL IR TABLE */

# define GL_IR_COL_NUM(IDX)                global_ir_tbl[IDX].opr.col_num
# define GL_IR_LINE_NUM(IDX)               global_ir_tbl[IDX].opr.line_num
# define GL_IR_NEXT_IDX(IDX)               global_ir_tbl[IDX].opr.line_num
# define GL_IR_OPR(IDX)                    global_ir_tbl[IDX].opr.the_operator
# define GL_IR_TYPE_IDX(IDX)               global_ir_tbl[IDX].opr.type_idx

# define GL_IR_RANK(IDX)                   global_ir_tbl[IDX].opr.rank
# define GL_IR_DV_DIM(IDX)                 global_ir_tbl[IDX].opr.dim
# define GL_IR_CONTIG_ARRAY(IDX)           global_ir_tbl[IDX].opr.dim
# define GL_IR_WHOLE_ARRAY(IDX)            global_ir_tbl[IDX].opr.dim
# define GL_IR_INLINE_STATE(IDX)           global_ir_tbl[IDX].opr.dim

# define GL_IR_COL_NUM_L(IDX)              global_ir_tbl[IDX].op_l.col_num
# define GL_IR_FLD_L(IDX)                  global_ir_tbl[IDX].op_l.fld
# define GL_IR_IDX_L(IDX)                  global_ir_tbl[IDX].op_l.idx
# define GL_IR_LINE_NUM_L(IDX)             global_ir_tbl[IDX].op_l.line_num
# define GL_IR_LIST_CNT_L(IDX)             global_ir_tbl[IDX].op_l.line_num
# define GL_IR_SHORT_CIRCUIT_L(IDX)        global_ir_tbl[IDX].op_l.flag_1
# define GL_IR_ARRAY_SYNTAX(IDX)           global_ir_tbl[IDX].op_l.flag_2

# define GL_IR_COL_NUM_R(IDX)              global_ir_tbl[IDX].op_r.col_num
# define GL_IR_FLD_R(IDX)                  global_ir_tbl[IDX].op_r.fld
# define GL_IR_IDX_R(IDX)                  global_ir_tbl[IDX].op_r.idx
# define GL_IR_LINE_NUM_R(IDX)             global_ir_tbl[IDX].op_r.line_num
# define GL_IR_LIST_CNT_R(IDX)             global_ir_tbl[IDX].op_r.line_num
# define GL_IR_SHORT_CIRCUIT_R(IDX)        global_ir_tbl[IDX].op_r.flag_1

# define GL_IR_OPND_L(IDX)                 global_ir_tbl[IDX].op_l
# define GL_IR_OPND_R(IDX)                 global_ir_tbl[IDX].op_r


/* GLOBAL IR LIST TABLE */

# define GL_IL_NEXT_LIST_IDX(IDX)      global_ir_list_tbl[IDX].il.link.nxt_idx
# define GL_IL_IDX(IDX)                global_ir_list_tbl[IDX].il.op.idx
# define GL_IL_LINE_NUM(IDX)           global_ir_list_tbl[IDX].il.op.line_num
# define GL_IL_COL_NUM(IDX)            global_ir_list_tbl[IDX].il.op.col_num
# define GL_IL_LIST_CNT(IDX)           global_ir_list_tbl[IDX].il.op.line_num
# define GL_IL_FLD(IDX)                global_ir_list_tbl[IDX].il.op.fld
# define GL_IL_ARG_DESC_VARIANT(IDX)   global_ir_list_tbl[IDX].il.link.arg_desc

# define GL_IL_PREV_LIST_IDX(IDX)      global_ir_list_tbl[IDX].il.link.prev_idx
# define GL_IL_ARG_DESC_IDX(IDX)       global_ir_list_tbl[IDX].il.link.prev_idx

/* flags for array subscript lists */
# define GL_IL_VECTOR_SUBSCRIPT(IDX)   global_ir_list_tbl[IDX].il.link.flag_1
# define GL_IL_CONSTANT_SUBSCRIPT(IDX) global_ir_list_tbl[IDX].il.link.flag_2
# define GL_IL_PE_SUBSCRIPT(IDX)       global_ir_list_tbl[IDX].il.link.flag_3

/* flags for io control lists */
# define GL_IL_NAMELIST_EXPECTED(IDX)  global_ir_list_tbl[IDX].il.link.flag_1
# define GL_IL_FORMAT_EXPECTED(IDX)    global_ir_list_tbl[IDX].il.link.flag_2

/* flags for io list */
# define GL_IL_HAS_FUNCTIONS(IDX)      global_ir_list_tbl[IDX].il.link.flag_1
# define GL_IL_MUST_BE_LOOP(IDX)       global_ir_list_tbl[IDX].il.link.flag_2
# define GL_IL_MUST_FLATTEN(IDX)       global_ir_list_tbl[IDX].il.link.flag_3
# define GL_IL_HAS_CONSTRUCTOR(IDX)    global_ir_list_tbl[IDX].il.link.flag_4
# define GL_IL_STRUCT_REF(IDX)         global_ir_list_tbl[IDX].il.link.flag_5
# define GL_IL_INTRIN_PLACE_HOLDER(IDX) global_ir_list_tbl[IDX].il.link.flag_6
# define GL_IL_NONDEFAULT_IMP_DO_LCV(IDX) global_ir_list_tbl[IDX].il.link.flag_7
# define GL_IL_DISTRIBUTION_VARIANT(IDX)  global_ir_list_tbl[IDX].il.link.flag_8

/* Flags for label forward ref lists.  See MISC above for values. */
# define GL_IL_FORWARD_REF(IDX)        global_ir_list_tbl[IDX].il.link.for_ref
# define GL_IL_DISTRIBUTION(IDX)       global_ir_list_tbl[IDX].il.link.for_ref

# define GL_IL_WORD(IDX,WD)            global_ir_list_tbl[IDX].words[WD]

# define GL_IL_OPND(IDX)               global_ir_list_tbl[IDX].il.op


/* STMT HEADER TABLE */

# define GL_SH_CIF_SKIP_ME(IDX)            global_sh_tbl[IDX].cif_skip_me
# define GL_SH_COL_NUM(IDX)                global_sh_tbl[IDX].col_num
# define GL_SH_COMPILER_GEN(IDX)           global_sh_tbl[IDX].compiler_gen
# define GL_SH_DOALL_LOOP_END(IDX)         global_sh_tbl[IDX].doall_loop_end
# define GL_SH_ERR_FLG(IDX)                global_sh_tbl[IDX].stmt_parse_err
# define GL_SH_GLB_LINE(IDX)               global_sh_tbl[IDX].glb_line_num
# define GL_SH_INLINING_ATTEMPTED(IDX)     global_sh_tbl[IDX].inlining_attempted
# define GL_SH_IR_IDX(IDX)                 global_sh_tbl[IDX].ir_idx
# define GL_SH_LABELED(IDX)                global_sh_tbl[IDX].labeled
# define GL_SH_LOOP_END(IDX)               global_sh_tbl[IDX].loop_end
# define GL_SH_NEXT_IDX(IDX)               global_sh_tbl[IDX].next_sh_idx
# define GL_SH_P2_SKIP_ME(IDX)             global_sh_tbl[IDX].skip_pass_2
# define GL_SH_PARENT_BLK_IDX(IDX)         global_sh_tbl[IDX].parent_blk_idx
# define GL_SH_PREV_IDX(IDX)               global_sh_tbl[IDX].prev_sh_idx
# define GL_SH_STMT_TYPE(IDX)              global_sh_tbl[IDX].stmt_type

/* MOD LINK TABLE */

# define ML_AT_IDX(IDX)			mod_link_tbl[IDX].at_idx
# define ML_AT_COMPRESSED_IDX(IDX)	mod_link_tbl[IDX].at_compressed
# define ML_AT_KEEP_ME(IDX)		mod_link_tbl[IDX].at_keep_me
# define ML_AT_LN_NAME(IDX)		mod_link_tbl[IDX].at_ln_name
# define ML_AT_SEARCHED(IDX)		mod_link_tbl[IDX].at_searched
# define ML_AT_SEARCH_ME(IDX)		mod_link_tbl[IDX].at_search_me
# define ML_BD_IDX(IDX)			mod_link_tbl[IDX].bd_idx
# define ML_BD_KEEP_ME(IDX)		mod_link_tbl[IDX].bd_keep_me
# define ML_CN_IDX(IDX)			mod_link_tbl[IDX].cn_idx
# define ML_CN_KEEP_ME(IDX)		mod_link_tbl[IDX].cn_keep_me
# define ML_CP_DALIGN_ME(IDX)		mod_link_tbl[IDX].cp_dalign_me
# define ML_CP_IDX(IDX)			mod_link_tbl[IDX].cp_idx
# define ML_CP_KEEP_ME(IDX)		mod_link_tbl[IDX].cp_keep_me
# define ML_CP_LEN(IDX)			mod_link_tbl[IDX].cp_len
# define ML_IL_IDX(IDX)			mod_link_tbl[IDX].il_idx
# define ML_IL_KEEP_ME(IDX)		mod_link_tbl[IDX].il_keep_me
# define ML_IR_IDX(IDX)			mod_link_tbl[IDX].ir_idx
# define ML_IR_KEEP_ME(IDX)		mod_link_tbl[IDX].ir_keep_me
# define ML_LN_IDX(IDX)			mod_link_tbl[IDX].ln_idx
# define ML_LN_KEEP_ME(IDX)		mod_link_tbl[IDX].ln_keep_me
# define ML_NP_IDX(IDX)			mod_link_tbl[IDX].np_idx
# define ML_NP_KEEP_ME(IDX)		mod_link_tbl[IDX].np_keep_me
# define ML_NP_LEN(IDX)			mod_link_tbl[IDX].np_len
# define ML_SB_IDX(IDX)			mod_link_tbl[IDX].sb_idx
# define ML_SB_KEEP_ME(IDX)		mod_link_tbl[IDX].sb_keep_me
# define ML_SH_IDX(IDX)			mod_link_tbl[IDX].sh_idx
# define ML_SH_KEEP_ME(IDX)		mod_link_tbl[IDX].sh_keep_me
# define ML_SN_IDX(IDX)			mod_link_tbl[IDX].sn_idx
# define ML_SN_KEEP_ME(IDX)		mod_link_tbl[IDX].sn_keep_me
# define ML_TYP_IDX(IDX)		mod_link_tbl[IDX].typ_idx
# define ML_TYP_KEEP_ME(IDX)		mod_link_tbl[IDX].typ_keep_me


/* LOCAL NAME TABLE */

# define LN_ATTR_IDX(IDX)		loc_name_tbl[IDX].attr_idx
# define LN_DEF_LOC(IDX)		loc_name_tbl[IDX].def_locally
# define LN_IN_ONLY_LIST(IDX)		loc_name_tbl[IDX].in_only_list
# define LN_NAME_IDX(IDX)		loc_name_tbl[IDX].name_idx
# define LN_NAME_LEN(IDX)		loc_name_tbl[IDX].name_len
# define LN_NAME_LONG(IDX)		((long *)&name_pool[LN_NAME_IDX(IDX)])
# define LN_NAME_PTR(IDX)		((char *)&name_pool[LN_NAME_IDX(IDX)])
# define LN_NEW_NAME(IDX)		loc_name_tbl[IDX].new_name
# define LN_RENAMED(IDX)		loc_name_tbl[IDX].renamed


/* MOD LINK TABLE */

# define PDG_AT_IDX(IDX)		pdg_link_tbl[IDX].at_idx
# define PDG_AT_TYP_IDX(IDX)		pdg_link_tbl[IDX].at_typ_idx
# define PDG_CN_IDX(IDX)		pdg_link_tbl[IDX].cn_idx
# define PDG_SB_IDX(IDX)		pdg_link_tbl[IDX].sb_idx


/* RENAME ONLY TABLE */

# define RO_COLUMN_NUM(IDX)		rename_only_tbl[IDX].column_num
# define RO_DUPLICATE_RENAME(IDX)	rename_only_tbl[IDX].duplicate_rename
# define RO_LINE_NUM(IDX)		rename_only_tbl[IDX].line_num
# define RO_NAME_IDX(IDX)		rename_only_tbl[IDX].name_idx
# define RO_NAME_LEN(IDX)		rename_only_tbl[IDX].name_len
# define RO_NAME_LONG(IDX)		((long *)&name_pool[RO_NAME_IDX(IDX)])
# define RO_NAME_PTR(IDX)		((char *)&name_pool[RO_NAME_IDX(IDX)])
# define RO_NEXT_IDX(IDX)		rename_only_tbl[IDX].next_idx
# define RO_RENAME_IDX(IDX)		rename_only_tbl[IDX].rename_idx
# define RO_RENAME_NAME(IDX)		rename_only_tbl[IDX].rename_name


/* SECONDARY NAME TABLE */

# define SN_ATTR_IDX(IDX)		sec_name_tbl[IDX].attr_idx
# define SN_COLUMN_NUM(IDX)		sec_name_tbl[IDX].column_num
# define SN_LINE_NUM(IDX)		sec_name_tbl[IDX].line_num
# define SN_MATCHED_DARG(IDX)		sec_name_tbl[IDX].matched
# define SN_NAME_IDX(IDX)		sec_name_tbl[IDX].name_idx
# define SN_NAME_LEN(IDX)		sec_name_tbl[IDX].length
# define SN_NAME_PTR(IDX)		((char *)&name_pool[SN_NAME_IDX(IDX)])
# define SN_SIBLING_LINK(IDX)		sec_name_tbl[IDX].sibling_link


/* STORAGE BLOCK TABLE */

# define SB_ALIGN_SYMBOL(IDX)		stor_blk_tbl[IDX].fld.align_symbol
# define SB_AUXILIARY(IDX)		stor_blk_tbl[IDX].fld.auxiliary
# define SB_BLANK_COMMON(IDX)		stor_blk_tbl[IDX].fld.blank_common
# define SB_BLK_HAS_NPES(IDX)		stor_blk_tbl[IDX].fld.blk_has_npes
# define SB_BLK_TYPE(IDX)		stor_blk_tbl[IDX].fld.sb_type
# define OLD_SB_BLK_TYPE(IDX)		stor_blk_tbl[IDX].fld.old_sb_type
# define SB_CACHE_ALIGN(IDX)		stor_blk_tbl[IDX].fld.cache_align
# define SB_CIF_SYMBOL_ID(IDX)		stor_blk_tbl[IDX].fld.cif_idx
# define SB_COMMON_NEEDS_OFFSET(IDX)	stor_blk_tbl[IDX].fld.needs_offset
# define SB_DCL_ERR(IDX)		stor_blk_tbl[IDX].fld.dcl_err
# define SB_DCL_COMMON_DIR(IDX)		stor_blk_tbl[IDX].fld.dcl_common_dir
# define SB_DEF_COLUMN(IDX)		stor_blk_tbl[IDX].fld.def_column
# define SB_DEF_MULT_SCPS(IDX)		stor_blk_tbl[IDX].fld.def_mult_scps
# define SB_DEF_LINE(IDX)		stor_blk_tbl[IDX].fld.def_line
# define SB_DUPLICATE_COMMON(IDX)	stor_blk_tbl[IDX].fld.duplicate_common
# define SB_EQUIVALENCED(IDX)		stor_blk_tbl[IDX].fld.equivalenced
#ifdef KEY /* Bug 14150 */
# define SB_BIND_ATTR(IDX)		stor_blk_tbl[IDX].fld.bind_attr
# define SB_EXT_NAME_IDX(IDX)		stor_blk_tbl[IDX].fld.ext_name_idx
# define SB_EXT_NAME_LEN(IDX)		stor_blk_tbl[IDX].fld.ext_name_len
# define SB_EXT_NAME(IDX)	      name_pool[SB_EXT_NAME_IDX(IDX)].name_char
# define SB_EXT_NAME_PTR(IDX)	      ((char *)&name_pool[SB_EXT_NAME_IDX(IDX)])
#endif /* KEY Bug 14150 */
# define SB_FILL_SYMBOL(IDX)		stor_blk_tbl[IDX].fld.fill_symbol
# define SB_FIRST_ATTR_IDX(IDX)		stor_blk_tbl[IDX].fld.first_attr_idx
# define SB_HAS_RENAMES(IDX)		stor_blk_tbl[IDX].fld.has_renames
# define SB_HIDDEN(IDX)			stor_blk_tbl[IDX].fld.hidden
# define SB_HOST_ASSOCIATED(IDX)	stor_blk_tbl[IDX].fld.host_associated
# define SB_HOSTED_STACK(IDX)		stor_blk_tbl[IDX].fld.hosted_stack
# define SB_HOSTED_STATIC(IDX)		stor_blk_tbl[IDX].fld.hosted_static
# define SB_IS_COMMON(IDX)		stor_blk_tbl[IDX].fld.is_common
# define SB_LAST_ATTR_LIST(IDX)         stor_blk_tbl[IDX].fld.last_attr_list
# define SB_LEN_FLD(IDX)		stor_blk_tbl[IDX].fld.len_fld
# define SB_LEN_IDX(IDX)		stor_blk_tbl[IDX].fld.len_idx
# define SB_MERGED_BLK_IDX(IDX)		stor_blk_tbl[IDX].fld.merged_blk_idx
# define SB_MODULE_IDX(IDX)		stor_blk_tbl[IDX].fld.module_idx
# define SB_MODULE(IDX)			stor_blk_tbl[IDX].fld.module
# define SB_NAME_IDX(IDX)		stor_blk_tbl[IDX].fld.name_idx
# define SB_NAME_PTR(IDX)		((char *)&name_pool[SB_NAME_IDX(IDX)])
# define SB_NAME_LONG(IDX)		((long *)&name_pool[SB_NAME_IDX(IDX)])
# define SB_NAME(IDX)			name_pool[SB_NAME_IDX(IDX)].name_char
# define SB_NAME_IN_STONE(IDX)		stor_blk_tbl[IDX].fld.name_in_stone
# define SB_NAME_LEN(IDX)		stor_blk_tbl[IDX].fld.name_len
# define SB_ORIG_SCP_IDX(IDX)		stor_blk_tbl[IDX].fld.orig_scp_idx
#ifdef KEY /* Bug 14150 */
/* Assignment "SB_PAD_AMOUNT(IDX) = cmd_line_flags.pad_amount" will be a nop,
 * and value of SB_PAD_AMOUNT(IDX) will always be zero because we don't support
 * any way to set cmd_line_flags.pad_amount. */
# define SB_PAD_AMOUNT(IDX)		cmd_line_flags.pad_amount
#else /* KEY Bug 14150 */
# define SB_PAD_AMOUNT(IDX)		stor_blk_tbl[IDX].fld.pad_amount
#endif /* KEY Bug 14150 */
# define SB_PAD_AMOUNT_SET(IDX)		stor_blk_tbl[IDX].fld.pad_amount_set
# define SB_PAD_BLK(IDX)		stor_blk_tbl[IDX].fld.pad_blk
# define SB_RUNTIME_INIT(IDX)		stor_blk_tbl[IDX].fld.runtime_init
# define SB_SAVED(IDX)			stor_blk_tbl[IDX].fld.saved
# define SB_SCP_IDX(IDX)		stor_blk_tbl[IDX].fld.scp_idx
# define SB_SECTION_GP(IDX)		stor_blk_tbl[IDX].fld.section_gp
# define SB_SECTION_NON_GP(IDX)		stor_blk_tbl[IDX].fld.section_non_gp
# define SB_SYMMETRIC(IDX)		stor_blk_tbl[IDX].fld.symmetric
# define SB_USE_ASSOCIATED(IDX)		stor_blk_tbl[IDX].fld.use_associated
# define SB_VOLATILE(IDX)		stor_blk_tbl[IDX].fld.x_volatile


/* TYPE TABLE */

# define TYP_BIT_LEN(IDX)		type_tbl[IDX].fld.bit_len
# define TYP_BIT_LEN_F(IDX)		type_tbl[IDX].wd.length
# define TYP_CHAR_CLASS(IDX)		type_tbl[IDX].fld.char_class
# define TYP_DCL_VALUE(IDX)		type_tbl[IDX].fld.dcl_value
# define TYP_DECLARED_DBL(IDX)		type_tbl[IDX].fld.declared_dbl
# define TYP_DESC(IDX)			type_tbl[IDX].fld.desc
# define TYP_DP_HIT_ME(IDX)		type_tbl[IDX].fld.dp_hit_me
# define TYP_FLD(IDX)			type_tbl[IDX].fld.type_fld
# define TYP_IDX(IDX)			type_tbl[IDX].fld.type_idx
# define TYP_IS_BYTE(IDX)		type_tbl[IDX].fld.type_is_byte
# define TYP_LINEAR(IDX)		type_tbl[IDX].fld.linear_type
# define TYP_ORIG_LEN_IDX(IDX)		type_tbl[IDX].fld.bit_len
# define TYP_PTR_INCREMENT(IDX)		type_tbl[IDX].fld.bit_len
# define TYP_RESOLVED(IDX)		type_tbl[IDX].fld.resolved
# define TYP_TYPE(IDX)			type_tbl[IDX].fld.type
# define TYP_KIND_CONST(IDX)		type_tbl[IDX].fld.kind_const
# define TYP_KIND_DOUBLE(IDX)		type_tbl[IDX].fld.kind_double

# define OLD_TYP_DP_HIT_ME(IDX)		type_tbl[IDX].wd.old_dp_hit_me
# define OLD_TYP_FLD(IDX)		type_tbl[IDX].fld.old_type_fld
# define OLD_TYP_RESOLVED(IDX)		type_tbl[IDX].wd.old_resolved
# define OLD_TYP_TYPE(IDX)		type_tbl[IDX].wd.old_type


 
/* SYMBOL TABLE SEARCH AND ENTER MACROS */

/*  Used for secondary name table pool index and length. */

# define MSK_NAME_IDX                    0000000000000077777777
# define MSK_NAME_LEN			 0000000000000000000377	

# if defined(_HOST64) && !defined(_WHIRL_HOST64_TARGET64)
# define SN_NP_IDX(IDX)			(sn_tbl_base[(IDX)<<1] & MSK_NAME_IDX)
# else
# define SN_NP_IDX(IDX)                 SN_NAME_IDX(IDX)
# endif

# if defined(_HOST64) && !defined(_WHIRL_HOST64_TARGET64)
# define SN_LEN(IDX)                  ((sn_tbl_base[(IDX)<<1]>>24)&MSK_NAME_LEN)
# else
# define SN_LEN(IDX)             	SN_NAME_LEN(IDX)
# endif

/*  Cray shift is logical, SUN's and Alpha is arithmetic and  */
/*  preserves the sign bit                                    */

# ifdef _HOST_OS_UNICOS
# define RIGHT_JUSTIFY_SIGN_BIT(I)      (I) >> (8 * sizeof(long) - 1)
# else
# define RIGHT_JUSTIFY_SIGN_BIT(I) ((unsigned long) (I) >> (8 * sizeof(long)-1))
# endif

# define STRIDE_CALC(SEG_LEN)		(((SEG_LEN) + _MAXVL - 1) / _MAXVL)
# define WORD_LEN(ID_LEN)               ((ID_LEN) + sizeof(long)) / sizeof(long)


/***********************************************\
|* STATEMENT/FUNCTION-LIKE REPLACEMENT STRINGS *|
\***********************************************/

# define CLEAR_ATTR_NTRY(IDX)						       \
	CLEAR_TBL_NTRY(attr_tbl, IDX);					       \
	CLEAR_TBL_NTRY(attr_aux_tbl, IDX);

# define CLEAR_TBL_NTRY(TBL,IDX)					       \
	{								       \
	 long	*_tbl_idx;						       \
	 int	 _tbl_i;						       \
	 int	 _size;							       \
	 _tbl_idx	= ((long *) (&TBL[IDX]));			       \
	 _size		= TBL##_num_wds;				       \
	 Pragma("_CRI shortloop")					       \
	 for (_tbl_i = 0; _tbl_i < _size; _tbl_i++) {			       \
	    *(_tbl_idx) = 0;						       \
	    _tbl_idx++;							       \
	 }								       \
	}

# define COPY_ATTR_NTRY(TO, FROM)					       \
	attr_tbl[TO]		= attr_tbl[FROM];			       \
	attr_aux_tbl[TO]	= attr_aux_tbl[FROM];


# define COPY_TBL_NTRY(TBL, TO, FROM)					       \
	{								       \
	 long	*_to_idx, *_from_idx;					       \
	 int	 _idx;							       \
	 int	 _size;							       \
	 _from_idx	= ((long *) (&TBL[FROM]));			       \
	 _to_idx	= ((long *) (&TBL[TO]));			       \
	 _size		= TBL##_num_wds;				       \
	 Pragma("_CRI shortloop")					       \
	 for (_idx = 0; _idx < _size; _idx++) {				       \
	    *(_to_idx)	= *(_from_idx);					       \
            _to_idx++;							       \
            _from_idx++;						       \
	 }								       \
	}

# define COPY_GL_TBL_NTRY(TBL1, TBL2, TO, FROM)				       \
	{								       \
	 long	*_to_idx, *_from_idx;					       \
	 int	 _idx;							       \
	 _from_idx	= ((long *) (&TBL2[FROM]));			       \
	 _to_idx	= ((long *) (&TBL1[TO]));			       \
	 Pragma("_CRI shortloop")					       \
	 for (_idx = 0; _idx < TBL2##_num_wds; _idx++) {		       \
	    *(_to_idx)	= *(_from_idx);					       \
            _to_idx++;							       \
            _from_idx++;						       \
	 }								       \
	}

	
# define COPY_BD_NTRY(TO, FROM)						       \
	{								       \
	int	_idx, _bd_num;						       \
	int	_size;							       \
	long	*_to_idx, *_from_idx;					       \
	_from_idx	= ((long *) (&bounds_tbl[FROM]));		       \
	_to_idx		= ((long *) (&bounds_tbl[TO]));			       \
	_bd_num		= BD_NTRY_SIZE(FROM);				       \
	_size		= NUM_BD_WDS * _bd_num;				       \
	Pragma("_CRI shortloop")					       \
	for (_idx = 0; _idx < _size; _idx++) {				       \
	    *(_to_idx)	= *(_from_idx);					       \
            _to_idx++;							       \
            _from_idx++;						       \
	 }								       \
	}


# define CREATE_ERR_ATTR(IDX, LINE, COL, CLASS)				       \
	{								       \
	 int	_al_idx;						       \
	 int	_err_idx;						       \
	 NTR_ATTR_TBL(_err_idx);					       \
	 AT_DCL_ERR(IDX)		= TRUE;				       \
	 COPY_COMMON_ATTR_INFO(IDX, _err_idx, CLASS);			       \
	 AT_ATTR_LINK(_err_idx)		= NULL_IDX;			       \
	 AT_DEF_LINE(_err_idx)		= LINE;				       \
	 AT_DEF_COLUMN(_err_idx)	= COL;				       \
	 IDX				= _err_idx;			       \
	 NTR_ATTR_LIST_TBL(_al_idx);					       \
	 AL_ATTR_IDX(_al_idx)		= _err_idx;			       \
	 AL_NEXT_IDX(_al_idx)		= SCP_CIF_ERR_LIST(curr_scp_idx);      \
	 SCP_CIF_ERR_LIST(curr_scp_idx)	= _al_idx;			       \
	}

/* This macro copies common fields from on attr entry to another.  Class is   */
/* what AT_OBJ_CLASS should end up being after the copy, in the TO attr.      */
/* CLASS is passed in to prevent variant problems, because AT_OBJ_CLASS is    */
/* used to decide which variant is active.                                    */

# ifdef _HOST64
# define COPY_COMMON_ATTR_INFO(FROM, TO, CLASS)				       \
	{attr_tbl[(TO)].wd[0]		= attr_tbl[(FROM)].wd[0];	       \
	 attr_tbl[(TO)].wd[1]		= attr_tbl[(FROM)].wd[1];	       \
	 attr_tbl[(TO)].fldd.field32_5	= attr_tbl[(FROM)].fldd.field32_5;     \
	 attr_aux_tbl[(TO)].wd[0]	= attr_aux_tbl[(FROM)].wd[0];	       \
         AT_OBJ_CLASS(TO)		= CLASS;			       \
	 AT_CIF_SYMBOL_ID(TO)		= 0;				       \
	 AT_CIF_DONE(TO)		= FALSE;			       \
	}
# else
# define COPY_COMMON_ATTR_INFO(FROM, TO, CLASS)				       \
	{attr_tbl[(TO)].wd[0]		= attr_tbl[(FROM)].wd[0];	       \
	 attr_tbl[(TO)].wd[1]		= attr_tbl[(FROM)].wd[1];	       \
	 attr_tbl[(TO)].wd[2]		= attr_tbl[(FROM)].wd[2];	       \
	 attr_tbl[(TO)].wd[3]		= attr_tbl[(FROM)].wd[3];	       \
	 attr_tbl[(TO)].wd[4]		= attr_tbl[(FROM)].wd[4];	       \
	 attr_aux_tbl[(TO)].wd[0]	= attr_aux_tbl[(FROM)].wd[0];	       \
	 attr_aux_tbl[(TO)].wd[1]	= attr_aux_tbl[(FROM)].wd[1];	       \
         AT_OBJ_CLASS(TO)		= CLASS;			       \
	 AT_CIF_SYMBOL_ID(TO)		= 0;				       \
	 AT_CIF_DONE(TO)		= FALSE;			       \
	}
# endif

# ifdef _HOST64
# define COPY_VARIANT_ATTR_INFO(FROM, TO, CLASS)			       \
	{attr_tbl[(TO)].fldd.field32_6	= attr_tbl[(FROM)].fldd.field32_6;     \
	 attr_tbl[(TO)].wd[3]		= attr_tbl[(FROM)].wd[3];	       \
	 attr_tbl[(TO)].wd[4]		= attr_tbl[(FROM)].wd[4];	       \
	 attr_tbl[(TO)].wd[5]		= attr_tbl[(FROM)].wd[5];	       \
	 attr_tbl[(TO)].wd[6]		= attr_tbl[(FROM)].wd[6];	       \
	 attr_aux_tbl[(TO)].wd[1]	= attr_aux_tbl[(FROM)].wd[1];	       \
	 attr_aux_tbl[(TO)].wd[2]	= attr_aux_tbl[(FROM)].wd[2];	       \
         AT_OBJ_CLASS(TO)		= CLASS;			       \
	}
# else
# define COPY_VARIANT_ATTR_INFO(FROM, TO, CLASS)			       \
	{attr_tbl[(TO)].wd[5]		= attr_tbl[(FROM)].wd[5];	       \
	 attr_tbl[(TO)].wd[6]		= attr_tbl[(FROM)].wd[6];	       \
	 attr_tbl[(TO)].wd[7]		= attr_tbl[(FROM)].wd[7];	       \
	 attr_tbl[(TO)].wd[8]		= attr_tbl[(FROM)].wd[8];	       \
	 attr_tbl[(TO)].wd[9]		= attr_tbl[(FROM)].wd[9];	       \
	 attr_tbl[(TO)].wd[10]		= attr_tbl[(FROM)].wd[10];	       \
	 attr_tbl[(TO)].wd[11]		= attr_tbl[(FROM)].wd[11];	       \
	 attr_tbl[(TO)].wd[12]		= attr_tbl[(FROM)].wd[12];	       \
	 attr_tbl[(TO)].wd[13]		= attr_tbl[(FROM)].wd[13];	       \
	 attr_aux_tbl[(TO)].wd[2]	= attr_aux_tbl[(FROM)].wd[2];	       \
	 attr_aux_tbl[(TO)].wd[3]	= attr_aux_tbl[(FROM)].wd[3];	       \
	 attr_aux_tbl[(TO)].wd[4]	= attr_aux_tbl[(FROM)].wd[4];	       \
	 attr_aux_tbl[(TO)].wd[5]	= attr_aux_tbl[(FROM)].wd[5];	       \
         AT_OBJ_CLASS(TO)		= CLASS;			       \
	}
# endif


/* This macro clears variant fields in the attr entry.  AT_OBJ_CLASS = CLASS  */
/* CLASS is passed in to prevent variant problems, because AT_OBJ_CLASS is    */
/* used to decide which variant is active.                                    */

# ifdef _HOST64
# define CLEAR_VARIANT_ATTR_INFO(IDX, CLASS)				       \
	{attr_tbl[(IDX)].fldd.field32_6	= 0;				       \
	 attr_tbl[(IDX)].wd[3]		= 0;				       \
	 attr_tbl[(IDX)].wd[4]		= 0;				       \
	 attr_tbl[(IDX)].wd[5]		= 0;				       \
	 attr_tbl[(IDX)].wd[6]		= 0;				       \
         AT_OBJ_CLASS(IDX)		= CLASS;			       \
	 attr_aux_tbl[(IDX)].wd[1]	= 0;				       \
	 attr_aux_tbl[(IDX)].wd[2]	= 0;				       \
	}
# else
# define CLEAR_VARIANT_ATTR_INFO(IDX, CLASS)				       \
	{attr_tbl[(IDX)].wd[5]		= 0;				       \
	 attr_tbl[(IDX)].wd[6]		= 0;				       \
	 attr_tbl[(IDX)].wd[7]		= 0;				       \
	 attr_tbl[(IDX)].wd[8]		= 0;				       \
	 attr_tbl[(IDX)].wd[9]		= 0;				       \
	 attr_tbl[(IDX)].wd[10]		= 0;				       \
	 attr_tbl[(IDX)].wd[11]		= 0;				       \
	 attr_tbl[(IDX)].wd[12]		= 0;				       \
	 attr_tbl[(IDX)].wd[13]		= 0;				       \
         AT_OBJ_CLASS(IDX)		= CLASS;			       \
	 attr_aux_tbl[(IDX)].wd[2]	= 0;				       \
	 attr_aux_tbl[(IDX)].wd[3]	= 0;				       \
	 attr_aux_tbl[(IDX)].wd[4]	= 0;				       \
	 attr_aux_tbl[(IDX)].wd[5]	= 0;				       \
	}
# endif

/* Use to make a Function result for an attribute.  The attribute must be a   */
/*     Pgm_Unit.  Adding any of the following to a Pgm_Unit attr with         */
/*     Pgm_Unknown will force it to a Function:  TYPE, DIMENSION, POINTER,    */
/*     TARGET, VFUNCTION, or declaring it to be a NAMELIST object.  Also,     */
/*     this is needed by call_list_semantics when something is being called   */
/*     as a Function, but is still a Pgm_Unknown.  This only creates the      */
/*     result attr and links it up.  Typing and anything else must be done    */
/*     by the caller.                                                         */


# define CREATE_FUNC_RSLT(AT_IDX, RSLT_IDX)				       \
	 NTR_ATTR_TBL(RSLT_IDX);					       \
	 COPY_COMMON_ATTR_INFO(AT_IDX, RSLT_IDX, Data_Obj);		       \
	 ATD_CLASS(RSLT_IDX)	= Function_Result;			       \
	 ATD_FUNC_IDX(RSLT_IDX)	= AT_IDX;				       \
	 ATP_RSLT_IDX(AT_IDX)	= RSLT_IDX;


# define FREE_IR_LIST_NODE(IDX)                                                \
	IL_NEXT_LIST_IDX(IDX)		= IL_NEXT_LIST_IDX(NULL_IDX);          \
	IL_NEXT_LIST_IDX(NULL_IDX)	= IDX;


# define FREE_IR_NODE(IDX)                                                     \
	IR_NEXT_IDX(IDX)		= IR_NEXT_IDX(NULL_IDX);	       \
	IR_NEXT_IDX(NULL_IDX)		= IDX;


# define FREE_SH_NODE(IDX)                                                     \
	SH_NEXT_IDX(IDX)		= SH_NEXT_IDX(NULL_IDX);	       \
	SH_NEXT_IDX(NULL_IDX)		= IDX;

# define FREE_GL_IR_LIST_NODE(IDX)                                             \
        GL_IL_NEXT_LIST_IDX(IDX)           = GL_IL_NEXT_LIST_IDX(NULL_IDX);    \
        GL_IL_NEXT_LIST_IDX(NULL_IDX)      = IDX;


# define FREE_GL_IR_NODE(IDX)                                                  \
        GL_IR_NEXT_IDX(IDX)                = GL_IR_NEXT_IDX(NULL_IDX);         \
        GL_IR_NEXT_IDX(NULL_IDX)           = IDX;


# define FREE_GL_SH_NODE(IDX)                                                  \
        GL_SH_NEXT_IDX(IDX)                = GL_SH_NEXT_IDX(NULL_IDX);         \
        GL_SH_NEXT_IDX(NULL_IDX)           = IDX;



# define GEN_COMPILER_TMP_ASG(IR_IDX, ATTR_IDX, SEM_DONE, LINE, COL, TYPE_IDX, \
                              SCOPE)                                           \
        {NTR_IR_TBL(IR_IDX);                                                   \
         ATTR_IDX                     = gen_compiler_tmp(LINE,COL,SCOPE,TRUE); \
         AT_SEMANTICS_DONE(ATTR_IDX)    = SEM_DONE;                            \
         ATD_TYPE_IDX(ATTR_IDX)         = TYPE_IDX;                            \
         ATD_TMP_IDX(ATTR_IDX)          = IR_IDX;                              \
         ATD_FLD(ATTR_IDX)              = IR_Tbl_Idx;                          \
         ATD_STOR_BLK_IDX(ATTR_IDX)     = SCP_SB_STACK_IDX(curr_scp_idx);      \
         AT_DEFINED(ATTR_IDX)           = TRUE;                                \
         IR_OPR(IR_IDX)                 = Asg_Opr;                             \
         IR_TYPE_IDX(IR_IDX)            = ATD_TYPE_IDX(ATTR_IDX);              \
         IR_FLD_L(IR_IDX)               = AT_Tbl_Idx;                          \
         IR_IDX_L(IR_IDX)               = ATTR_IDX;                            \
         IR_LINE_NUM_L(IR_IDX)          = LINE;                                \
         IR_LINE_NUM(IR_IDX)            = LINE;                                \
         IR_COL_NUM_L(IR_IDX)           = COL;                                 \
         IR_COL_NUM(IR_IDX)             = COL;                                 \
        }

# define IS_STMT_ENTITY(ATTR_IDX)					       \
	(AT_OBJ_CLASS(ATTR_IDX) == Data_Obj &&				       \
	 ATD_SEEN_AS_LCV(ATTR_IDX) &&					       \
	 ! ATD_SEEN_IN_IMP_DO(ATTR_IDX) &&				       \
	 ! ATD_SEEN_OUTSIDE_IMP_DO(ATTR_IDX))

# define NTR_ATTR_TBL(ATTR_IDX)		                                       \
         TBL_REALLOC_CK(attr_tbl, 1);                                          \
         CLEAR_TBL_NTRY(attr_tbl, attr_tbl_idx);                               \
         TBL_REALLOC_CK(attr_aux_tbl, 1);                                      \
         CLEAR_TBL_NTRY(attr_aux_tbl, attr_aux_tbl_idx);                       \
         ATTR_IDX = attr_tbl_idx;

# define NTR_ATTR_LIST_TBL(ATTR_LIST_IDX)                                      \
         {int		_idx;                                                  \
          _idx		= ntr_attr_list_tbl();                                 \
         ATTR_LIST_IDX	= _idx;}

# define ADD_ATTR_TO_LOCAL_LIST(AT_IDX)		add_attr_to_local_list(AT_IDX);


# define NTR_EQ_TBL(EQ_IDX)		                                       \
         TBL_REALLOC_CK(equiv_tbl, 1);                                         \
         CLEAR_TBL_NTRY(equiv_tbl, equiv_tbl_idx);		  	       \
	 EQ_OFFSET_FLD(equiv_tbl_idx)	= CN_Tbl_Idx;			       \
	 EQ_OFFSET_IDX(equiv_tbl_idx)	= CN_INTEGER_ZERO_IDX;		       \
         EQ_IDX = equiv_tbl_idx;

/* If a module is to be read on a 64 bit system, it requires a full 64 bit    */
/* word following the name pool entry.  If the name pool entry was originally */
/* created on a 32 bit system, it would only have a 32 bit word following it. */
/* This macro is used to add an extra 32 bit word when this is a cross        */
/* compiler from a 32 bit system to a 64 bit system.                          */

# if defined(_TARGET64) && defined(_HOST32)
# define EXTRA_WORD			1
# else
# define EXTRA_WORD			0
# endif

# define NTR_NAME_POOL(NAME, LEN, NP_IDX)                                      \
	{register long	*_name_id;					       \
	 register int	 _np_idx;					       \
	 register int	 _start_idx;					       \
	 register int	 _wd_len;					       \
	 _name_id	= NAME;						       \
	 _wd_len	= WORD_LEN(LEN) + EXTRA_WORD;			       \
	 _start_idx	= name_pool_idx + 1;				       \
	 NP_IDX		= _start_idx;					       \
         TBL_REALLOC_CK(name_pool,_wd_len);				       \
	 for (_np_idx = 0; _np_idx < _wd_len; _np_idx++) {		       \
	     name_pool[_start_idx+_np_idx].name_long = _name_id[_np_idx];      \
	 }								       \
	}


# define NTR_IR_LIST_TBL(IR_LIST_IDX)		                               \
            {int	_idx;                                                  \
            _idx = ntr_ir_list_tbl();                                          \
            IR_LIST_IDX = _idx;}
            


# define NTR_IR_TBL(IR_IDX)			                               \
            {int	_idx;                                                  \
            _idx = ntr_ir_tbl();                                               \
            IR_IDX = _idx;}

# define NTR_GL_IR_LIST_TBL(IR_LIST_IDX)                                       \
            {int        _idx;                                                  \
            _idx = ntr_gl_ir_list_tbl();                                       \
            IR_LIST_IDX = _idx;}



# define NTR_GL_IR_TBL(IR_IDX)                                                 \
            {int        _idx;                                                  \
            _idx = ntr_gl_ir_tbl();                                            \
            IR_IDX = _idx;}



# define NTR_SCP_TBL(SCP_IDX)						       \
      	TBL_REALLOC_CK(scp_tbl,1);					       \
        CLEAR_TBL_NTRY(scp_tbl, scp_tbl_idx);				       \
	SCP_IDX	= scp_tbl_idx;


# define NTR_SN_TBL(SN_IDX)		                                       \
         TBL_REALLOC_CK(sec_name_tbl,1);                                       \
         CLEAR_TBL_NTRY(sec_name_tbl, sec_name_tbl_idx);		       \
         SN_IDX = sec_name_tbl_idx;


# define NTR_INTERFACE_IN_SN_TBL(SN_IDX, AT_IDX, PARENT_IDX, LINE, COLUMN)     \
	NTR_SN_TBL(SN_IDX);						       \
	SN_ATTR_IDX(SN_IDX)		  = AT_IDX;			       \
	SN_NAME_LEN(SN_IDX)		  = AT_NAME_LEN(AT_IDX);	       \
	SN_NAME_IDX(SN_IDX)		  = AT_NAME_IDX(AT_IDX);	       \
	SN_LINE_NUM(SN_IDX)		  = LINE;			       \
	SN_COLUMN_NUM(SN_IDX)		  = COLUMN;			       \
	SN_SIBLING_LINK(SN_IDX)		  = ATI_FIRST_SPECIFIC_IDX(PARENT_IDX);\
	ATI_FIRST_SPECIFIC_IDX(PARENT_IDX)= SN_IDX;			       \
	ATI_NUM_SPECIFICS(PARENT_IDX)	 += 1;


#if defined(_HOST64) && !defined(_WHIRL_HOST64_TARGET64)
# define NTR_NAME_IN_LN_TBL(ENTER_NAME_IDX, NEW_NAME_IDX)		       \
	{long	*_name_tbl_base;					       \
	 int	 _idx;							       \
	 _name_tbl_base = (long *) loc_name_tbl;			       \
	 Pragma("_CRI ivdep")						       \
	 for (_idx = SCP_LN_LW_IDX(curr_scp_idx);			       \
	      _idx>= ENTER_NAME_IDX; _idx--) {				       \
	   _name_tbl_base[_idx] = _name_tbl_base[_idx-1];		       \
	 }								       \
	 loc_name_tbl[ENTER_NAME_IDX]  = loc_name_tbl[NEW_NAME_IDX];	       \
	}
#else
# define NTR_NAME_IN_LN_TBL(ENTER_NAME_IDX, NEW_NAME_IDX)		       \
	{int	 _idx;							       \
	 Pragma("_CRI ivdep")						       \
	 for (_idx = SCP_LN_LW_IDX(curr_scp_idx);			       \
	      _idx>= ENTER_NAME_IDX; _idx--) {				       \
	    loc_name_tbl[_idx]  = loc_name_tbl[_idx-1];			       \
	 }								       \
	 loc_name_tbl[ENTER_NAME_IDX]  = loc_name_tbl[NEW_NAME_IDX];	       \
	}
# endif


# define SET_IMPL_TYPE(AT_IDX)                              		       \
	{int	_i_idx;							       \
	 _i_idx			= IMPL_IDX(AT_OBJ_NAME(AT_IDX));	       \
	 ATD_TYPE_IDX(AT_IDX)	= IM_TYPE_IDX(curr_scp_idx, _i_idx);	       \
	}


# define SET_IMPL_TYPE_IN_SCP(AT_IDX, SCP_IDX)                 		       \
	{int	_i_idx;							       \
	 _i_idx			= IMPL_IDX(AT_OBJ_NAME(AT_IDX));	       \
	 ATD_TYPE_IDX(AT_IDX)	= IM_TYPE_IDX(SCP_IDX, _i_idx);		       \
	}

# define BITS_TO_WORDS(BIT_SIZE, ALIGN_TO)				       \
	bits_and_bytes_to_words(&(BIT_SIZE), (ALIGN_TO == 64) ? 63:31,	       \
				(ALIGN_TO == 64) ? 6 : 5);

# define BYTES_TO_WORDS(BIT_SIZE, ALIGN_TO)				       \
	bits_and_bytes_to_words(&(BIT_SIZE), (ALIGN_TO == 64) ? 7:3,	       \
				(ALIGN_TO == 64) ? 3 : 2);

/* OSP_467, #2 */
# define BITS_TO_INTEGER_DEFAULT_WORDS(BIT_SIZE, INT_SIZE)                     \
	((INT_SIZE) == 64) ? (((BIT_SIZE)+63) >> 6) : (((BIT_SIZE)+31) >> 5)

/*      This routine will test the value held in an integer array (c) against */
/*      the value held in TRUE_VALUE to see if the they match. This is to     */
/*      the cases when a logical result from a fold is two machine words.     */

# if defined(_TARGET32) && !defined(_TARGET_LITTLE_ENDIAN)
# define THIS_IS_TRUE(THE_CONSTANT, TYPE_IDX) 				       \
       ((THE_CONSTANT)[(TYP_LINEAR(TYPE_IDX) == Logical_8) ? 1:0] == TRUE_VALUE)
# else  /* TARGET64 */
# define THIS_IS_TRUE(THE_CONSTANT, TYPE_IDX) 				       \
	((THE_CONSTANT)[0] == TRUE_VALUE)
# endif


# if defined(_HOST64) && defined(_TARGET64)
# define CONVERT_CVAL_TO_STR(CONSTANT, TYPE_IDX, RESULT)		       \
	convert_to_string(CONSTANT, TYPE_IDX, RESULT)
# else
# define CONVERT_CVAL_TO_STR(CONSTANT, TYPE_IDX, RESULT)		       \
	convert_cval_to_string(CONSTANT, TYPE_IDX, RESULT)
# endif


/******************************************************************************\
|*                                                                            *|
|* The following macros are used to convert host and target integer constant. *|
|* be used in 'C' arithmetic and comparison.                                  *|
|*                                                                            *|
|* C_TO_F_INT  : C host integer const to target const in an array of long_type*|
|* C_INT_TO_CN : C host integer const to the constant table.                  *|
|* CN_INT_TO_C : Constant table entry to C host integer const                 *|
|* F_INT_TO_C  : Target integer constant (array of long_type) to C host int   *|
|*                                                                            *|
|* target integer constant is declared  long_type   con[MAX_WORDS_FOR_INTEGER]*|
|* A host integer constant is declared  long64      con                       *|
|*                                                                            *|
|* Arithmetic on target integer constants must be done with calls to folder.  *|
|* Arithmetic on host integer constants can be done with 'C' code.            *|
|*                                                                            *|
|* The underlying defs for long_type and long64 are in target.h               *|
|*                                                                            *|
\******************************************************************************/


# if defined(_TARGET64) && defined(_HOST64)
# define F_INT_TO_C(ARRAY,LIN_TYPE)	*(ARRAY)
# elif (defined(_TARGET_OS_IRIX) || defined(_TARGET_OS_LINUX) || defined(_TARGET_OS_DARWIN))
# define F_INT_TO_C(ARRAY,LIN_TYPE)					       \
                        ((LIN_TYPE == Integer_8 || LIN_TYPE == Typeless_8)     \
                                   ? *((long64 *) (ARRAY)):(long64) *(ARRAY))
# else
# define F_INT_TO_C(ARRAY,LIN_TYPE)	f_int_to_cval(ARRAY, LIN_TYPE)
# endif

# if defined(_TARGET_OS_MAX)
# define CN_INT_TO_C(IDX)       mpp_cn_int_to_c(IDX)
# else
# define CN_INT_TO_C(IDX) 						       \
	 F_INT_TO_C(&CN_CONST(IDX), TYP_LINEAR(CN_TYPE_IDX(IDX)))
# endif

# define C_INT_TO_CN(TYPE_IDX, CONST)					       \
	ntr_int_const_tbl(TYPE_IDX, (long64) (CONST))

/* Extra care is taken here, because CONST might be a literal */
/* constant, so an address cannot be taken of CONST.          */

/* 09Feb01[sos] was: # if defined(_TARGET64) && defined(_HOST64) */
# if defined(_TARGET64)
# define C_TO_F_INT(ARRAY, CONST, LIN_TYPE)	ARRAY[0] = CONST
# elif (defined(_TARGET_OS_IRIX) || defined(_TARGET_OS_LINUX) || defined(_TARGET_OS_DARWIN))
# define C_TO_F_INT(ARRAY, CONST, LIN_TYPE)				       \
	{								       \
	long   *_cn_ptr;						       \
	long64  _big_cn;						       \
	if (LIN_TYPE == Integer_8 || LIN_TYPE == Typeless_8) {		       \
	   _big_cn = (long64) CONST;					       \
	   _cn_ptr = (long *) &_big_cn;					       \
	   ARRAY[0] = *_cn_ptr;						       \
	   ARRAY[1] = *(++_cn_ptr);					       \
	} else {							       \
	   ARRAY[0] = (long) CONST;					       \
	   ARRAY[1] = 0;						       \
	} }
# else
# define C_TO_F_INT(ARRAY, CONST, LIN_TYPE)				       \
	{								       \
	long64   _con;							       \
	_con = (long64) CONST;						       \
	cval_to_f_int(ARRAY, &_con, LIN_TYPE);				       \
	}
# endif

