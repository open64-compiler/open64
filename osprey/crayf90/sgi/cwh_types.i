/*
 *  Copyright (C) 2006. QLogic Corporation. All Rights Reserved.
 */

/*

  Copyright 2003, 2004, 2005, 2006 PathScale, Inc.  All Rights Reserved.

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

/* Mapping of types - basic type and alignment used as index */

#define ALIGNS 5
#define align_index(x) (al_off[x>>3])
#define basic_index(x) (x)
#define alignment_to_align(x) (al_off[x])


static const INT16 al_off[17]= {0,  /* align 0 */
				0,  /* align 8 bits */
				1,  /* align 16 bits */
				0,
				2,  /* align 32 bits */
				0,0,0,
				3,  /* align 64 bits */
	                        0,0,0,0,0,0,0,
			        4   /* align 128 bits */
			       };

static const char * alstr[ALIGNS-1]= {
				    ".align1",  /* align 8 bits */
  			            ".align2",  /* align 16 bits */
			            ".align4",  /* align 32 bits */
			            ".align8",  /* align 64 bits */
                                    }; 

static const char * logstr[ALIGNS-1]= { ".log.1",".log.2",".log.4",".log.8" } ;

static TY_IDX  unaligned_type [MTYPE_LAST+1][ALIGNS];
static TY_IDX basic_logical_ty[NUM_LOG_KINDS][ALIGNS];

static const TYPE_ID Mtypes[ALIGNS][Vector_Mask+1] = {
{ 
MTYPE_V,     
MTYPE_I1,	/* Logical         */
MTYPE_U1,	/* Typeless        */		
MTYPE_V,	/* Void		   */ 
MTYPE_I1,	/* Char_Fortran	   */
MTYPE_I1,	/* Char_C	   */
MTYPE_I1,	/* Structure	   */ 
MTYPE_I1,	/* Union	   */ 
MTYPE_I1,	/* Integral	   */
MTYPE_F4,	/* Floating_Pt	   */ 
MTYPE_C4,	/* Complex	   */ 
MTYPE_I1,	/* CRI_Pointer	   */ 
MTYPE_I1,	/* CRI_Pointer_Char*/ 
MTYPE_I1,	/* BT_func_ptr     */ 
MTYPE_V		/* Vector_Mask     */ 
},{ 
MTYPE_V,     
MTYPE_I2,	/* Logical         */
MTYPE_U2,	/* Typeless        */		
MTYPE_V,	/* Void		   */ 
MTYPE_I2,	/* Char_Fortran	   */
MTYPE_I2,	/* Char_C	   */
MTYPE_I2,	/* Structure	   */ 
MTYPE_I2,	/* Union	   */ 
MTYPE_I2,	/* Integral	   */
MTYPE_F4,	/* Floating_Pt	   */ 
MTYPE_C4,	/* Complex	   */ 
MTYPE_I2,	/* CRI_Pointer	   */ 
MTYPE_I2,	/* CRI_Pointer_Char*/ 
MTYPE_I2,	/* BT_func_ptr     */ 
MTYPE_V		/* Vector_Mask     */ 

},{ 
MTYPE_V,     
MTYPE_I4,	/* Logical         */
MTYPE_U4,	/* Typeless        */		
MTYPE_V,	/* Void		   */ 
MTYPE_I4,	/* Char_Fortran	   */
MTYPE_I4,	/* Char_C	   */
MTYPE_I4,	/* Structure	   */ 
MTYPE_I4,	/* Union	   */ 
MTYPE_I4,	/* Integral	   */
MTYPE_F4,	/* Floating_Pt	   */ 
MTYPE_C4,	/* Complex	   */ 
MTYPE_I4,	/* CRI_Pointer	   */ 
MTYPE_I4,	/* CRI_Pointer_Char*/ 
MTYPE_I4,	/* BT_func_ptr     */ 
MTYPE_V		/* Vector_Mask     */ 
},{ 
MTYPE_V,     
MTYPE_I8,	/* Logical       */
MTYPE_U8,	/* Typeless      */		
MTYPE_V,	/* Void		 */ 
MTYPE_I8,	/* Char_Fortran	 */
MTYPE_I8,	/* Char_C	 */
MTYPE_I8,	/* Structure	 */ 
MTYPE_I8,	/* Union	 */ 
MTYPE_I8,	/* Integral	 */
MTYPE_F8,	/* Floating_Pt	 */ 
MTYPE_C8,	/* Complex	 */ 
MTYPE_I8,	/* CRI_Pointer	 */ 
MTYPE_I8,	/* CRI_Pointer_Char*/ 
MTYPE_I8,	/* BT_func_ptr   */ 
MTYPE_V		/* Vector_Mask   */ 
},{ 
MTYPE_V,     
MTYPE_I8,	/* Logical         */
MTYPE_U8,	/* Typeless        */		
MTYPE_V,	/* Void		   */ 
MTYPE_I8,	/* Char_Fortran	   */
MTYPE_I8,	/* Char_C          */
MTYPE_I8,	/* Structure	   */ 
MTYPE_I8,	/* Union           */ 
MTYPE_I8,	/* Integral	   */
MTYPE_FQ,	/* Floating_Pt	   */ 
MTYPE_CQ,	/* Complex	   */ 
MTYPE_I8,	/* CRI_Pointer	   */ 
MTYPE_I8,	/* CRI_Pointer_Char*/ 
MTYPE_I8,	/* BT_func_ptr     */ 
MTYPE_V		/* Vector_Mask     */ 
}} ;


/* local definitions */

#define MAX_ALIGN 16


/*
 * variables used to build array types
 * fei_array_dimen adds a bound on each
 * pass. 
 * decl_bounds - the details of the bounds
 * top_of_decl_bounds - # of axes.
 * ty_dim1      - base type of array
 * last_bitsize - size of last axis.
 *
 */

static ARB_HANDLE    decl_bounds;  
static INT32  top_of_decl_bounds = ANULL ;
static TY_IDX ty_dim1 ;
static INT64  last_bitsize; 

/* structure used to hold distribution info for DISTRIBUTED arrays */
/*   decl_distribution	- the distribution kind (BLOCK, STAR, etc...)
     decl_cyclic_val	- args for CYCLIC
     decl_onto		- the corresponding ONTO arg (0=star)

   top_of_decl_bounds is used as a bound for this array
   decl_is_distributed - is set depending on which directive is used
   distribute_onto - set to TRUE if this distribute has an ONTO clause

   decl_distribute_pragmas - set to a list of distribute pragmas generated

*/     

static DISTRIBUTE_TYPE	decl_distribution[MAX_ARY_DIMS];
static union {
	WN *wn;
	INT val;
	} decl_cyclic_val[MAX_ARY_DIMS];
static WN	*decl_onto[MAX_ARY_DIMS];
static WN_PRAGMA_ID decl_distributed_pragma_id;
static BOOL	distribute_onto;
WN	*decl_distribute_pragmas;

/*
 * definitions of the dope vector entries 
 * names, offsets,sizes, type-ids. n32/64
 * versions, hence two tables for each.
 *
 * bounds are defined similarly below.
 *
 */
#ifdef KEY /* Bug 6845 */
/* Should now use typedef enum dv_idx_type (see cwh_types.h) to index into the
 * following dope_xxx arrays because we've added an entry, and the original
 * scheme was a mess anyway. */
#endif /* KEY Bug 6845 */

static const char * dope_name [DOPE_NM] = { 
	"base",	
	"el_len",
	"assoc",
        "ptr_alloc",
        "p_or_a",
        "a_contig",
#ifdef KEY /* Bug 6845 */
        "alloccpnt",
#endif /* KEY Bug 6845 */
        "unused_1",
        "num_dims",
        "type_code",
        "orig_base",
        "orig_size",
};

static const int dope_bofst[DOPE_NM] = { 
#ifdef KEY /* Bug 6845 */
 0,0,0,1,2,4,5,6,29,0,0,0
#else /* KEY Bug 6845 */
 0,0,0,1,2,4,5,29,0,0,0
#endif /* KEY Bug 6845 */
};

static const int dope_bsize[DOPE_NM] = { 
#ifdef KEY /* Bug 6845 */
 0,0,1,1,2,1,1,55,3,64,0,0
#else /* KEY Bug 6845 */
 0,0,1,1,2,1,56,3,64,0,0
#endif /* KEY Bug 6845 */
};

static TYPE_ID *dope_btype;
static INT *dope_offset;


static TYPE_ID dope_btype_64[DOPE_NM] = {
MTYPE_U8,MTYPE_I8,MTYPE_U4,
MTYPE_U4,MTYPE_U4,MTYPE_U4,
#ifdef KEY /* Bug 6845 */
MTYPE_U4,
#endif /* KEY Bug 6845 */
MTYPE_U8,MTYPE_U4,
MTYPE_U8,MTYPE_U8,
MTYPE_I8
};

static int dope_offset_64 [DOPE_NM] = { 
#ifdef KEY /* Bug 6845 */
ADDR_OFFSET,8,16,16,16,16,16,16,20,24,32,40
#else /* KEY Bug 6845 */
ADDR_OFFSET,8,16,16,16,16,16,20,24,32,40
#endif /* KEY Bug 6845 */
};


static TYPE_ID dope_btype_32[DOPE_NM] = {
MTYPE_U4,MTYPE_I4,MTYPE_U4,
MTYPE_U4,MTYPE_U4,MTYPE_U4,
#ifdef KEY /* Bug 6845 */
MTYPE_U4,
#endif /* KEY Bug 6845 */
MTYPE_U8,MTYPE_U4,
MTYPE_U8,MTYPE_U4,
MTYPE_I4
};

static int dope_offset_32 [DOPE_NM] = { 
#ifdef KEY /* Bug 6845 */
ADDR_OFFSET,4,8,8,8,8,8,8,12,16,24,28
#else /* KEY Bug 6845 */
ADDR_OFFSET,4,8,8,8,8,8,12,16,24,28
#endif /* KEY Bug 6845 */
};

static const char * bound_name [BOUND_NM] = { 
 "lb",
 "ext",
 "str_m",				
} ;


/* 
 * descriptor type name - used to distinguish dope    
 * structs from other structs. see cwh_types_is_dope  
 *
 */

static const char * const dope_str = ".dope." ;
static const char * const dope_invariant_str = ".flds." ;
#define DOPENM_LEN 6

/* is this a base type that a global, shared descriptor   */
/* TY could be created for? Logicals can't be shared with */
/* integers, so they need another 'table' which is tacked */
/* onto the entries for the other mtypes                  */                        

#define IS_SHARED_DOPE_BASE(ty) (TY_kind(ty) == KIND_SCALAR && !TY_is_character(ty))
#define NUM_LOGICAL_DOPE_TYPES 4
#define NUM_DOPE_TYPES MTYPE_LAST + 1 + NUM_LOGICAL_DOPE_TYPES 
#define LOGICAL_OFFSET(bt) (MTYPE_LAST + (bt - MTYPE_I1 + 1))


/*
 * These are used for sizing dope vectors
 * They are set in cwh_addr.c, in cwh_addr_init_target
 */

TY_IDX  DOPE_bound_ty;
INT32 DOPE_bound_sz;
INT32 DOPE_dim_offset;
INT32 DOPE_sz;



/* logical 4 byte ty, for general use */

TY_IDX logical4_ty;


/* 
 *  structure and stack to deal with nested derived types 
 *    dty      - TY of enclosing KIND_STRUCT
 *    dty_last - last FLD processed by fei_member
 *    ncompos  - number of components
 *    seq      - is sequence derived type
 */

typedef struct {
	TY_IDX dty ;
	FLD_IDX dty_last ;
	INT32 ncompos  ;
	BOOL  seq      ;
	BOOL  hosted   ;
} dtype_t ;

static INT32 dtype_stk_size = 0;
static dtype_t *dtype_stk=NULL;
static INT32 dtype_top = -1 ;

#define STK_SIZE_CHANGE 100;

/* 
	Namelist definitions 
*/

/* Length of fields for holding namelist/variable names. */
#define NL_Name_Length			36

/* How much space does an mtype take in memory? */
#define MTYPE_MemorySize(x)	(MTYPE_size_min(x) >> 3)


/*
 * Sizes/offsets for our structures for 32/64-bit compiles.  The
 * NL_Table_Index variable chooses between the two.
 *
 */

#define ALIGN_Dims			0
#define OFFSET_Dims_ndims		1
#define OFFSET_Dims_nels		2
#define OFFSET_Dims_baseoff		3
#define OFFSET_Dims_span		4
#define ALIGN_Nlentry			5
#define OFFSET_Nlentry_varname		6
#define OFFSET_Nlentry_varaddr		7
#define OFFSET_Nlentry_type		8
#define OFFSET_Nlentry_dimp		9
#define SIZE_Nlentry			10
#define ALIGN_Namelist			11
#define OFFSET_Namelist_nlname		12
#define OFFSET_Namelist_nlvnames	13

static INT NL_Table_Index;

static WN_OFFSET NL_Tables[][2] = {
    /* ALIGN_Dims */			4,	8,
    /* OFFSET_Dims_ndims */		0,	0,
    /* OFFSET_Dims_nels */		4,	8,
    /* OFFSET_Dims_baseoff */		8,	16,
    /* OFFSET_Dims_span */		12,	24,
    /* ALIGN_Nlentry */			4,	8,
    /* OFFSET_Nlentry_varname */	0,	0,
    /* OFFSET_Nlentry_varaddr */	36,	40,
    /* OFFSET_Nlentry_type */		40,	48,
    /* OFFSET_Nlentry_dimp */		44,	56,
    /* SIZE_Nlentry */			48,	64,
    /* ALIGN_Namelist */		4,	8,
    /* OFFSET_Namelist_nlname */	0,	0,
    /* OFFSET_Namelist_nlvnames */	36,	40
};


#define GET_HOST_SYMTAB ((SYMTAB_level(Current_Symtab) == 1) ? Current_Symtab : SYMTAB_parent(Current_Symtab))


/* ty_idx of last TY created. So it can be deleted */

static TY_IDX  Last_TY_Created = 0 ;

/* the FE cannot guarantee components of hosted derived type */
/* are marked hosted, hence we retain a context */

static BOOL in_hosted_dtype = FALSE ;



/* forward references */

static FLD_HANDLE cwh_types_fld_util(const char* name_string, TY_IDX  fld_ty,  OFFSET_64 offset, BOOL global) ;
static void   cwh_types_fill_type(INT32 flag_bits, TYPE *t, TY_IDX  ty) ;
static TY_IDX cwh_types_dim_struct_TY(void);
static TY_IDX cwh_types_dim_TY(INT32 num_dims) ;
static void   cwh_types_push_dtype(dtype_t d) ;
static dtype_t cwh_types_pop_dtype(void) ;
static ST *   cwh_types_formal_util(TY_IDX  ty) ;
static BOOL   cwh_types_in_dtype(void) ;
static TY_IDX    cwh_types_mk_namelist_item_TY(void)  ;
static TY_IDX    cwh_types_mk_unique_pointer_TY(TY_IDX  ty, BOOL host) ;
static TY_IDX    cwh_types_mk_misaligned_TY(TY_IDX ty, mUINT16 alignment) ;

static TY_IDX cwh_types_mk_array_TY(ARB_HANDLE bounds,INT16 n,TY_IDX base, INT64 size);
static TY_IDX cwh_types_mk_basic_TY (BASIC_TYPE, INTPTR size, mUINT16 alignment) ;
static TY_IDX cwh_types_mk_struct(INT64 size, INT32 align, FLD_HANDLE list, const char *name) ;
static TY_IDX cwh_types_shared_dope(FLD_HANDLE  list,int ndims, BOOL is_ptr,
#ifdef KEY /* Bug 6845 */
  int n_allocatable_cpnt
#endif /* KEY Bug 6845 */
  );
static TY_IDX cwh_types_mk_dope_invariant_TY(void);
static TY_IDX cwh_types_new_TY(BOOL global,INT32 align) ;
static ST *   cwh_types_make_bounds_ST(void) ;
