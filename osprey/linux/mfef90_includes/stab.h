#ifndef stab_INCLUDED
#define stab_INCLUDED

#ifdef _NEW_SYMTAB
#ifdef __cplusplus
#include "symtab.h"
#endif /* __cplusplus */

/* Enter_ST has no effect in new symtab */

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


#define Enter_ST(s)
#define ST_can_use_reg_align(s)	FALSE
#define Enter_TY(s)

#else /* _NEW_SYMTAB */

#ifdef __cplusplus
extern "C" {
#endif

/* ====================================================================
 * ====================================================================
 *
 * Module: stab.h
 * $Source: /proj/osprey/CVS/open64/osprey1.0/linux/mfef90_includes/stab.h,v $
 *
 * Revision history:
 *  11-Feb-89 - Original Version
 *  12-Jun-91 - Integrated from Josie
 *  07-Jun-92 - Removed call utilities to callutil.h
 *  30-Apr-93 - Removed constant table utilities to const.h
 *     Oct-94 - Redo for Mongoose
 *
 * Description:
 *
 * Definitions of the symbol table data structures, access functions,
 * and associated constants, including type identifiers.
 *
 * ====================================================================
 * ====================================================================
 */

/* ====================================================================
 *
 * Symbol Table Structure
 *
 * The Symbol Table consists of a list of SYMTAB structures, 
 * one for each PU (procedure), plus one for the global symbols.
 * Under each SYMTAB there is a list of symbols (ST), labels (ST),
 * and types (TY) that are defined in that PU.
 *
 * The constant symbols (both literals and symbolic constants
 * like relocations) are kept in a separate constant table because
 * they are hashed to avoid duplicate copies.  See const.{h,c} for
 * information on the constant table.
 *
 * ====================================================================
 */

/* Dummy declarations of structs for prototypes: */
struct wn;
struct tcon;
struct einfo;
struct symexpr;		/* Symbolic expression node from symconst.h */
struct inito;

#include "mtypes.h"	/* for supported MTYPEs (machine types) */

/* Forward definitions of the various structure typedefs: */
typedef struct st    ST;	/* Symbol table entry */
typedef ST* ST_IDX;
typedef UINT32 LABEL_IDX;
typedef struct stch  STCH;	/* Symbol table entry list (of STs) */
typedef struct sblk  SBLK;	/* Symbol table block info */
typedef struct ty    TY;	/* Type descriptor */
typedef TY         * TY_IDX;    /* Type descriptor index */
typedef struct tylist TYLIST;	/* Type descriptor list */
typedef TYLIST*	TYLIST_IDX;	/* Type descriptor list */
typedef struct fld   FLD;	/* Structured type field descriptor */
typedef struct fld * FLD_IDX;	/* Structured type field descriptor */
typedef struct ari   ARI;	/* Array information */
typedef struct arb   ARB;	/* Array bounds */
typedef struct arb * ARB_IDX;
typedef struct symtab SYMTAB;	/* Symbol table */
typedef SYMTAB* SYMTAB_IDX;
typedef struct enum_const ENUM_CONST;	/* Enumerated type constant */
typedef struct fti FTI;		/* Function Type Information */
typedef struct tcon *TCON_IDX;  /* TCON index */

/* for compatibilty with new symtab */
/*REFERENCED*/
inline TY_IDX
Get_TY (TY_IDX x)	{ return x; }    
#define Get_ST(x)	(x)    
#define ST_st_idx(x)	(x)
#define Get_ARB(x)	(x)
#define Get_FLD(x)	(x)

#include "aux_stab.h"

/* ====================================================================
 *
 * Checking macros
 *
 * The macros below may be ignored for purposes of understanding the
 * structure definitions in this file.  They are separated out here
 * to make the rest of the file easier to read.  They follow the basic
 * rule that they all take the struct pointer to be checked as one (or
 * the only) operand, check it, and return it as the result, so they
 * may be viewed as noops.
 *
 * ====================================================================
 */

/* --------------------------------------------------------------------
 * These macros are meant for miscellaneous pointer checking.  If
 * needed, they must be defined in aux_stab.h. The definitions below
 * are the default definitions if there are no previous definitions.
 *
 * Non-default versions will be used in Cfe for checking that things in
 * local scope are not used afer that scope is closed and deallocated.
 *
 * Theoretically, with these macros, one can have extremely complicated
 * checking mechanisms in which each pointer is checked for validity
 * before it is used.
 * --------------------------------------------------------------------
 */

#ifndef CAN_USE_ARB
#define CAN_USE_ARB(a) (a)
#endif

#ifndef  CAN_USE_STCH
#define CAN_USE_STCH(a) (a)
#endif

#ifndef CAN_USE_TY
#define CAN_USE_TY(a) (a)
#endif

#ifndef  CAN_USE_TYLIST
#define CAN_USE_TYLIST(a) (a)
#endif

#ifndef CAN_USE_FLD
#define CAN_USE_FLD(a) (a)
#endif

#ifndef CAN_USE_MBR
#define CAN_USE_MBR(a) (a)
#endif

#ifndef CAN_USE_ENUM_CONST
#define CAN_USE_ENUM_CONST(a) (a)
#endif

#ifndef CAN_USE_ST
#define CAN_USE_ST(a) (a)
#endif

/* --------------------------------------------------------------------
 * TY checking
 * --------------------------------------------------------------------
 */

/* Validate the dimension number of an array: */
#if defined(Is_True_On) && !(defined(FRONT_END_C) || defined(FRONT_END_CPLUSPLUS))
extern INT16 Is_Valid_Dim ( const TY *ty, INT16 i);
#define IS_VALID_DIM(ty, i) Is_Valid_Dim(ty,i)
#else
#define IS_VALID_DIM(ty, i) (i)
#endif

/* Validate the type kind for a kind-specific access: */
#ifdef Is_True_On
  extern void CHECK_KIND_func(const TY *,UINT16);
  extern void CHECK_KIND2_func(const TY *,UINT16,UINT16);
# define CHECK_KIND(s,k) 	(CHECK_KIND_func(s,k),s)
# define CHECK_KIND2(s,k,kk) 	(CHECK_KIND2_func(s,k,kk),s)
#else
# define CHECK_KIND(s,k) 	(s)
# define CHECK_KIND2(s,k,kk) 	(s)
#endif

#define KIND_IS_SCALAR(s)	CHECK_KIND(s,KIND_SCALAR)
#define KIND_IS_ARRAY(s)	CHECK_KIND(s,KIND_ARRAY)
#define KIND_IS_STRUCT(s)	CHECK_KIND(s,KIND_STRUCT)
#define KIND_IS_ENUM(s)		CHECK_KIND(s,KIND_ENUM)
#define KIND_IS_POINTER(s)	CHECK_KIND(s,KIND_POINTER)
#define KIND_IS_FUNCTION(s)	CHECK_KIND(s,KIND_FUNCTION)
#define KIND_IS_CLASS(s)	CHECK_KIND(s,KIND_CLASS)
#define KIND_IS_STRUCT_CLASS(s) CHECK_KIND2(s,KIND_STRUCT,KIND_CLASS)
#define KIND_IS_SCALAR_OR_POINTER(s) CHECK_KIND2(s,KIND_SCALAR,KIND_POINTER)

/* ---------------------------------------------------------------------
 * ST checking:  Validate the variant-specific accesses.
 * ---------------------------------------------------------------------
 */

#ifdef Is_True_On

  extern ST *CHECK_SCLASS_func (ST *, INT16 );
  extern ST *CHECK_CLASS2_func (ST *, INT16, INT16 );
  extern ST *CHECK_CLASS3_func (ST *, INT16, INT16, INT16 );
  extern ST *CHECK_CLASS_func ( const ST *, INT16 );

# define CHECK_SCLASS(s,c) 	CHECK_SCLASS_func(s,c)
# define CHECK_CLASS2(s,c1,c2)	CHECK_CLASS2_func(s,c1,c2)
# define CHECK_CLASS3(s,c1,c2,c3)	CHECK_CLASS3_func(s,c1,c2,c3)
# define CHECK_CLASS(s,c) 	CHECK_CLASS_func(s,c)

#else

# define CHECK_SCLASS(s,c) 	(s)
# define CHECK_CLASS2(s,c1,c2)	(s)
# define CHECK_CLASS3(s,c1,c2,c3)	(s)
# define CHECK_CLASS(s,c) 	(s)

#endif

#define SCLASS_IS_LOCAL(s)	CHECK_SCLASS(s,SCLASS_LOCAL)
#define SCLASS_IS_SAVED(s)	CHECK_SCLASS(s,SCLASS_SAVED)
#define SCLASS_IS_COMMON(s)	CHECK_SCLASS(s,SCLASS_COMMON)
#define SCLASS_IS_EXTERN(s)	CHECK_SCLASS(s,SCLASS_EXTERN)
#define SCLASS_IS_STATIC (s)	CHECK_SCLASS(s,SCLASS_STATIC)
#define SCLASS_IS_DEFINED(s)	CHECK_SCLASS(s,SCLASS_DEFINED)

#define CLASS_IS_VAR(s)		CHECK_CLASS(s,CLASS_VAR)
#define CLASS_IS_FUNC(s)	CHECK_CLASS(s,CLASS_FUNC)
#define CLASS_IS_CONST(s)	CHECK_CLASS(s,CLASS_CONST)
#define CLASS_IS_LABEL(s)	CHECK_CLASS(s,CLASS_LABEL)
#define CLASS_IS_SYM_CONST(s)	CHECK_CLASS(s,CLASS_SYM_CONST)
#define CLASS_IS_INTR(s)	CHECK_CLASS(s,CLASS_INTR)
#define CLASS_IS_PREG(s)	CHECK_CLASS(s,CLASS_PREG)
#define CLASS_IS_BLOCK(s)	CHECK_CLASS(s,CLASS_BLOCK)
#define CLASS_IS_VAR_UNK(s)	CHECK_CLASS2(s,CLASS_VAR,CLASS_UNK)
#define CLASS_IS_FUNC_UNK(s)	CHECK_CLASS2(s,CLASS_FUNC,CLASS_UNK)
#define CLASS_IS_VAR_FUNC(s)	CHECK_CLASS2(s,CLASS_VAR,CLASS_FUNC)
#define CLASS_IS_VAR_CONST(s)	\
	CHECK_CLASS3(s,CLASS_VAR,CLASS_CONST,CLASS_SYM_CONST)
#define CLASS_IS_VAR_BLOCK(s)	CHECK_CLASS2(s,CLASS_VAR,CLASS_BLOCK)
#define CLASS_IS_CONST_SYM(s)	CHECK_CLASS2(s,CLASS_CONST,CLASS_SYM_CONST)


/* ====================================================================
 *
 * FLD: structure/union fields
 *
 * Structure field information is represented by a list of FLD elements
 * attached to the type descriptor for the structure type.  There is
 * one such list for each structure type, pointed to from any TY
 * record that has TY_kind==KIND_STRUCT.
 *
 * The fields represented by a FLD list may or may not overlap each 
 * other in memory, determined strictly by their given byte addresses
 * and sizes.  Both Fortran equivalence and C union are mapped into
 * structures with overlapping fields.
 *
 * ====================================================================
 */

struct fld {
  char 	  	*name;  /* pointer to field name */
  struct ty   	*type;  /* pointer to field type */
  mINT64  	ofst;   /* byte offset of first data byte wrt parent */
  mUINT8	bsize;	/* bit field: size in bits */
  mUINT8	bofst;	/* bit field: bit offset in byte specified by ofst */
  mUINT16	flags;	/* Attribute flags */
  FLD		*next; 	/* next FLD in list */
};

#define FLD_name(fld)	((CAN_USE_FLD(fld))->name)
#define FLD_type(fld)	((CAN_USE_FLD(fld))->type) 
#define FLD_ofst(fld)	((CAN_USE_FLD(fld))->ofst)
#define FLD_bsize(fld)	((CAN_USE_FLD(fld))->bsize)
#define FLD_bofst(fld)	((CAN_USE_FLD(fld))->bofst)
#define FLD_flags(fld)	((CAN_USE_FLD(fld))->flags)
#define FLD_next(fld)	((CAN_USE_FLD(fld))->next)

#define FLD_SET_ONCE	0x0010		/* is set only once */
#define FLD_EQUIVALENCE 0x0020		/* is equivalence */
				/* in C we have nested structs for unions,
				 * but in fortran we can flatten it out
				 * so mark that a union is starting. */
#define FLD_BEGIN_UNION	0x0040		/* begin a union */
#define FLD_END_UNION	0x0080		/* end a union */
#define FLD_BEGIN_MAP	0x0100		/* begin a map (fortran) */
#define FLD_END_MAP	0x0200		/* end a map */
#define FLD_BIT_FIELD   0x0400		/* is bit field */

#define       FLD_is_set_once(m)	((FLD_flags(m)) & FLD_SET_ONCE)
#define   Set_FLD_is_set_once(m)	((FLD_flags(m)) |= FLD_SET_ONCE)
#define Reset_FLD_is_set_once(m)	((FLD_flags(m)) &= ~FLD_SET_ONCE)
#define       FLD_is_equivalence(m)	((FLD_flags(m)) & FLD_EQUIVALENCE)
#define   Set_FLD_is_equivalence(m)	((FLD_flags(m)) |= FLD_EQUIVALENCE)
#define Reset_FLD_is_equivalence(m)	((FLD_flags(m)) &= ~FLD_EQUIVALENCE)
#define       FLD_is_begin_union(m)	((FLD_flags(m)) & FLD_BEGIN_UNION)
#define   Set_FLD_is_begin_union(m)	((FLD_flags(m)) |= FLD_BEGIN_UNION)
#define Reset_FLD_is_begin_union(m)	((FLD_flags(m)) &= ~FLD_BEGIN_UNION)
#define       FLD_is_end_union(m)	((FLD_flags(m)) & FLD_END_UNION)
#define   Set_FLD_is_end_union(m)	((FLD_flags(m)) |= FLD_END_UNION)
#define Reset_FLD_is_end_union(m)	((FLD_flags(m)) &= ~FLD_END_UNION)
#define       FLD_is_begin_map(m)	((FLD_flags(m)) & FLD_BEGIN_MAP)
#define   Set_FLD_is_begin_map(m)	((FLD_flags(m)) |= FLD_BEGIN_MAP)
#define Reset_FLD_is_begin_map(m)	((FLD_flags(m)) &= ~FLD_BEGIN_MAP)
#define       FLD_is_end_map(m)	((FLD_flags(m)) & FLD_END_MAP)
#define   Set_FLD_is_end_map(m)	((FLD_flags(m)) |= FLD_END_MAP)
#define Reset_FLD_is_end_map(m)	((FLD_flags(m)) &= ~FLD_END_MAP)
#define       FLD_is_bit_field(m)	((FLD_flags(m)) & FLD_BIT_FIELD)
#define   Set_FLD_is_bit_field(m)	((FLD_flags(m)) |= FLD_BIT_FIELD)
#define Reset_FLD_is_bit_field(m)	((FLD_flags(m)) &= ~FLD_BIT_FIELD)

/* ====================================================================
 *
 * MBR: C++ class members
 *
 * Class member information is represented by a list of MBR elements
 * attached to the type descriptor for the class type.  There is one
 * such list for each class type, pointed to from any TY record that
 * has TY_kind==KIND_CLASS.
 *
 * The members represented by a MBR list may or may not overlap each 
 * other in memory, determined strictly by their given byte addresses
 * and sizes.
 *
 * NOTE:  MBRs now have the same info as struct FLDs, so just use the
 * same structure.  This simplifies some things.
 * NOTE:  someday we should just remove MBR and use FLD.
 *
 * ====================================================================
 */

typedef FLD	MBR;	/* Class member descriptor */

/* Descriptor field access: */
#define MBR_name(mbr)	FLD_name(mbr)
#define MBR_ofst(mbr)	FLD_ofst(mbr)
#define MBR_type(mbr)	FLD_type(mbr)
#define MBR_next(mbr)	FLD_next(mbr)
#define MBR_flags(mbr)	FLD_flags(mbr)

/* Attribute flags: */
#define MBR_STATIC	0x0001		/* Static member */
#define MBR_VIRTUAL	0x0002		/* Virtual member (function) */
#define MBR_FUNCTION	0x0004		/* Function member */
#define MBR_BASE	0x0008		/* Base class */
					/* Missing bits are used for FLD */
#define MBR_BIT_FIELD	0x0400		/* Bit field */

/* Flag field access: */
#define       MBR_static(m)	((MBR_flags(m)) & MBR_STATIC)
#define   Set_MBR_static(m)	((MBR_flags(m)) |= MBR_STATIC)
#define Reset_MBR_static(m)	((MBR_flags(m)) &= ~MBR_STATIC)
#define       MBR_virtual(m)	((MBR_flags(m)) & MBR_VIRTUAL)
#define   Set_MBR_virtual(m)	((MBR_flags(m)) |= MBR_VIRTUAL)
#define Reset_MBR_virtual(m)	((MBR_flags(m)) &= ~MBR_VIRTUAL)
#define       MBR_function(m)	((MBR_flags(m)) & MBR_FUNCTION)
#define   Set_MBR_function(m)	((MBR_flags(m)) |= MBR_FUNCTION)
#define Reset_MBR_function(m)	((MBR_flags(m)) &= ~MBR_FUNCTION)
#define       MBR_base(m)	((MBR_flags(m)) & MBR_BASE)
#define   Set_MBR_base(m)	((MBR_flags(m)) |= MBR_BASE)
#define Reset_MBR_base(m)	((MBR_flags(m)) &= ~MBR_BASE)
#define       MBR_is_bit_field(m)	((MBR_flags(m)) & MBR_BIT_FIELD)
#define   Set_MBR_is_bit_field(m)	((MBR_flags(m)) |= MBR_BIT_FIELD)
#define Reset_MBR_is_bit_field(m)	((MBR_flags(m)) &= ~MBR_BIT_FIELD)

/* For Delta C++, an offset may be dynamic, i.e. unknown at compile
 * time.  We represent this with the minimum negative integer, so that
 * it can be loaded with a single lui.  We don't use -1 to allow for
 * representing fields at negative offsets.  We also have an offset
 * value to represent the case where the offset is undefined (e.g.
 * for static members).  MBR_VALID_OFST is the minimum true offset.
 */
#define MBR_DYNAMIC_OFST	INT32_MIN
#define MBR_NO_OFST		INT32_MIN+1
#define MBR_VALID_OFST		INT32_MIN+2

/* ====================================================================
 *
 * COMP: component (struct field/class member) pointer
 *
 * A comp is a unioned pointer to allow common treatment of struct
 * field and C++ class member lists.
 *
 * NOTE:  because FLD and MBR are now the same, COMP is just a typedef.
 * NOTE:  someday we should just remove COMP and use FLD.
 * ====================================================================
 */

typedef FLD	COMP;	/* Component: field or class pointer */

#define COMP_fld(c)	((FLD) c)
#define COMP_mbr(c)	((MBR) c)

/* ====================================================================
 *
 * ENUM_CONST: enumerated type constants
 *
 * For enumerated types, there is one TY record for the basic 
 * enumerated type, and this is of KIND_SCALAR (KIND_ENUM is not 
 * used!).  If the scalar type is not an enumerated type, then the 
 * TY_enum_const_list(ty) is NULL.  If that field is non-NULL, then it 
 * contains a pointer to the head of a linked list of ENUM_CONST 
 * structures, each of which contains information about one of the 
 * members of the enumerated type (i.e., its symbolic name and integer 
 * value).  Variables declared to be enumerated types have ST_type 
 * fields which point to the main TY record.
 *
 * ====================================================================
 */

struct enum_const {
  char			*name;	/* Symbolic name of constant  */
  mINT64		val;	/* Value of constant */
  struct enum_const	*next;	/* Next constant in the list or NULL */
};

#define ENUM_CONST_name(f)	((CAN_USE_ENUM_CONST(f))->name)
#define ENUM_CONST_val(f)	((CAN_USE_ENUM_CONST(f))->val)
#define ENUM_CONST_next(f)	((CAN_USE_ENUM_CONST(f))->next)

/* ====================================================================
 *
 * ARI: Array information
 * ARB: Array bounds
 *
 * Array dimension information is represented by a structure which
 * consists of an ARI, whose tail end contains as many instances of
 * ARB as there are declared dimensions.
 *
 * The ARI front part holds info for the whole array, while each
 * contained ARB holds info for a single dimension.
 *
 * There is one such structure for each declared array type, pointed to
 * from each TY type record that has TY_kind==KIND_ARRAY.
 *
 * NOTE:  for unknown historical reasons, the names "fbnd" and "tbnd"
 * were chosen.  I've changed them to "lbnd" and "ubnd", but kept the
 * old names for compatibility.
 * ====================================================================
 */

struct arb {
	mBOOL const_lbnd;	/* has constant lower bound */
	mBOOL const_ubnd;	/* has constant upper bound */
	mBOOL const_stride;	/* has constant stride */
	union {
		mINT64 lbnd_val;
		struct wn *lbnd_tree;
	} lbnd;			/* lower bound, as written */
	union {
		mINT64 ubnd_val;
		struct wn *ubnd_tree;
	} ubnd;			/* upper bound, as written */
	union {
		mINT64 stride_val;
		struct wn *stride_tree;
	} stride;		/* stride, as written */
};

#define ARB_const_lbnd(a)	((CAN_USE_ARB(a)).const_lbnd)
#define ARB_lbnd_val(a)		((CAN_USE_ARB(a)).lbnd.lbnd_val)
#define ARB_lbnd_tree(a)	((CAN_USE_ARB(a)).lbnd.lbnd_tree)
#define ARB_const_ubnd(a)	((CAN_USE_ARB(a)).const_ubnd)
#define ARB_ubnd_val(a)		((CAN_USE_ARB(a)).ubnd.ubnd_val)
#define ARB_ubnd_tree(a)	((CAN_USE_ARB(a)).ubnd.ubnd_tree)
#define ARB_const_stride(a)	((CAN_USE_ARB(a)).const_stride)
#define ARB_stride_val(a)	((CAN_USE_ARB(a)).stride.stride_val)
#define ARB_stride_tree(a)	((CAN_USE_ARB(a)).stride.stride_tree)

struct ari {
  struct ty	*etype;		/* Pointer to element type */
  mINT16 	ndims;		/* Number of dimensions */
  mBOOL const_zofst;		/* has constant zero offset */
  union {
	mINT64 zofst_val;
	struct wn *zofst_tree;
  } zofst;			/* zero offset, as written */
  ARB		bnds[1];
};

#define ARI_etype(a)		((a)->etype)
#define ARI_ndims(a)		((a)->ndims)
#define ARI_const_zofst(a)	((a)->const_zofst)
#define ARI_zofst_val(a)	((a)->zofst.zofst_val)
#define ARI_zofst_tree(a)	((a)->zofst.zofst_tree)
#define ARI_bnds(a)		((a)->bnds)
#define ARI_bnd(a,i)		((a)->bnds[i])

struct tylist {
  TY			*item;
  struct tylist		*next;
};

#define TYLIST_item(s)	((CAN_USE_TYLIST(s))->item)
#define TYLIST_next(s)	((CAN_USE_TYLIST(s))->next)

/* ====================================================================
 * FTI:  Function Type Info
 * KIND_FUNCTION types point to the FTI structure for more information.
 * ====================================================================
 */
struct fti { 
      struct tylist 	*parms; 	/* parameter type list */
      struct ty 	*ret_type;	/* returned type */
      mINT32		arg_area_size;	/* size of area for args;
					 * used to avoid recomputing the size,
					 * this will be set in back-end. */
};
#define FTI_parms(s)	((s)->parms)
#define FTI_ret_type(s)	((s)->ret_type)
#define FTI_arg_area_size(s)	((s)->arg_area_size)

/* ====================================================================
 *
 * TY: Type descriptor
 *
 * Type information is represented by a tree structure with nodes that
 * are TY records, with the TY_kind field specifying the nature of
 * each level.
 *
 * If TY_kind==KIND_STRUCT, there is a list of FLD records, with each
 * FLD containing field info and a TY pointer giving the field type.
 *
 * For TY_kind==KIND_CLASS, there is a similar list of MBR records
 * with information about class members.  For a Delta C++ dynamic class,
 * the size may be TY_DYNAMIC_SIZE, indicating unknown, and the
 * alignment may be TY_DYNAMIC_ALIGN, also indicating unknown.
 *
 * If TY_kind==KIND_ARRAY, there is a pointer to an (ARI,n*ARB)
 * structure which gives dimension info, and a pointer to a TY which
 * gives the element type. 
 *
 * If TY_kind==KIND_SCALAR, there are two possibilities:
 *   1) if this is a non-enumerated type, then there are no children.
 *   2) if this is an enumerated type, then there is a list of 
 *      ENUM_CONST structures which give a mapping between the 
 *      symbolic names and numeric values.
 * The value of the TY_enum_const_list(ty) field distinguishes 
 * between the two:  if that field is NULL, then we are in case (1), 
 * else we are in case (2).  Either way, TY_btype gives the 
 * elementary type (TYPE_ID from mtypes.h).
 *
 * NOTE: Char strings are represented as arrays of char.
 *
 * NOTE: const and volatile qualifiers are represented by
 * bits in TY records. The TY record for 'volatile <ty>' and '<ty>'
 * differ by only one that one bit (and type_tag). In particular if
 * <ty> is a struct type, the TY_flist of these two TY records will
 * be same pointer; similarly if it is array, the arinfo will be same
 * pointer etc. The equality of TY_flist's of two struct typed TYs
 * can be used to test test the equality of types ignoring volatile 
 * and const bits 
 *
 * ====================================================================
 */

/* Kinds of types: */
typedef enum {
  KIND_INVALID,	/*	  Invalid type */
  KIND_SCALAR,	/* 0x0001 Scalar type (integer/floating point) */
  KIND_ARRAY,	/* 0x0002 array type */
  KIND_STRUCT,	/* 0x0003 struct/union type */
  KIND_ENUM,	/* 0x0004 enumerated type */
  KIND_POINTER,	/* 0x0005 pointer type */
  KIND_FUNCTION,/* 0x0006 function/procedure type */
  KIND_VOID,	/* 0x0007 C void type */
  KIND_CLASS,	/* 0x0008 C++ class */
  KIND_LAST
} TY_KIND;

/* Kind queries: */
extern BOOL Is_Simple_Type ( TY_IDX );	/* SCALAR, ENUM, PTR, VOID */
extern BOOL Is_Structure_Type ( TY_IDX );	/* STRUCT, CLASS */
extern BOOL Is_Composite_Type ( TY_IDX );	/* STRUCT, CLASS, ARRAY */

/* Qualified type "kind" for use only by mtob/btom: */
#define KIND_QUALIFIED	0x0080

/* The type descriptor:  Note that references to "all objects of the
 * type" refer to both ST objects and FLDs.
 */
struct ty {
  mINT64  size;		/* Size, in bytes, incl for structures or
			 * arrays all children and all padding.
			 * If kind==KIND_POINTER, the size is that of
			 * the pointer only, not that of the pointee */
  mUINT32 flags;	/* Attributes of the type */
  mUINT32 id;		/* A unique ID. */
  mUINT8 kind;		/* changed from type  TY_KIND to mUINT16 : PV 357942 */
  mUINT8 btype;		/* One of the possible back-end types for TY_kind */
  mUINT16 align;	/* Required alignment for this type. 1, 2, 4 or 8 */
  char  	*name;	/* Name of type (needed for debugger output) */
  union {
    ENUM_CONST 	*ec_list; /* KIND_SCALAR: maybe enum const list */
    FLD  	*flist;	  /* KIND_STRUCT: field list */
    MBR  	*mlist;	  /* KIND_CLASS: member list */
    COMP  	*clist;	  /* field/member pointer union */
    ARI 	*arinfo;  /* KIND_ARRAY:  array bounds info */
    struct ty   *pointed; /* KIND_POINTER: type of pointee */
    struct fti  *ftinfo;  /* KIND_FUNCTION:  parameter and return info */
  } spec_type_info; 
  /* The next two fields are used to speed up type queries that were
   * consuming large portions of the compiler's running time. */
  struct ty     *pointer;/* type which points to this type */
  struct ty     *next;   /* Linked list of TY's for debugging */
};

#define TY_GLOBAL	   0x0001   /* Type is global */
#define TY_CHARACTER	   0x0002   /* type is a character (fortran) */
#define TY_DYNAMIC	   0x0004   /* type info is dynamic (delta-C++) */
#define TY_RETURN_TO_PARAM 0x0008   /* Return value through first parameter */
#define TY_VARARGS	   0x0010   /* Functions of type are varargs */
#define TY_PROTOTYPED	   0x0020   /* Function type has prototype */
#define TY_REFERENCED	   0x0040   /* BE: Type is referenced */
#define TY_THUNK	   0x0080   /* Function is a thunk */
#define TY_VOLATILE	   0x0100   /* Objects of type are volatile */
#define TY_CONST	   0x0200   /* Objects of this type never
				         * change in value (C semantics) */
#define TY_RESTRICT	   0x1000
#define TY_QUALIFIED	   0x1300   /* Qualified type (const or volatile) */
#define TY_LOGICAL         0x0400   /* Type is logical (fortran) */
#define TY_UNION	   0x0800   /* Struct or class type is union */
#define TY_LONG		   0x1000   /* Type is "long" not int or longlong (C) */
#define TY_TRANSLATED_TO_C 0x2000   /* Type has been defined in whirl2c */
#define TY_PACKED          0x4000   /* Struct or class type is packed */
#define TY_PTR_AS_ARRAY    0x8000   /* Treat pointer as array */
#define TY_ANONYMOUS      0x10000   /* Anonymous structs/classes/unions */
#define TY_SPLIT          0x20000   /* Split from a larger common block
                                     * equivalence (block_split) */
#define TY_LAST_IN_SPLIT  0x40000   /* Last in the list of items split 
                                     * in common/equivalence (block_split) */
#define TY_ENTERED        0x80000   /* TY entered */
#define TY_F90_POINTER   0x100000   /* If the type is an F90 pointer */
#define TY_F90_TARGET    0x200000   /* If the type has the F90 target attribute */
#define TY_NOT_IN_UNION  0x40000    /* If the type cannot be part of a union */

#define TY_NO_ANSI_ALIAS  TY_VARARGS

#define TY_kind(s)	(CAN_USE_TY(s)->kind)
#define TY_size(s) 	(CAN_USE_TY(s)->size)
#define TY_align(s) 	(CAN_USE_TY(s)->align)
#define TY_flags(s)	(CAN_USE_TY(s)->flags)
#define TY_id(s)	(CAN_USE_TY(s)->id)
#define TY_next(s)	(CAN_USE_TY(s)->next)
#define TY_name(s)	(CAN_USE_TY(s)->name)
#define TY_btype(s)	(CAN_USE_TY(s)->btype)
#define TY_mtype(s)	TY_btype(s)

#define Set_TY_kind(t,v)	(TY_kind(t) = v)
#define Set_TY_size(t,v)	(TY_size(t) = v)
#define Set_TY_align(t,v)	(TY_align(t) = v)
#define Set_TY_name(t,v)	(TY_name(t) = v)

#define       TY_is_global(s)	(TY_flags(s) & TY_GLOBAL)
#define   Set_TY_is_global(s)	(TY_flags(s) |= TY_GLOBAL)
#define Reset_TY_is_global(s)	(TY_flags(s) &= ~TY_GLOBAL)
#define       TY_is_character(s)	(TY_flags(s) & TY_CHARACTER)
#define   Set_TY_is_character(s)	(TY_flags(s) |= TY_CHARACTER)
#define Reset_TY_is_character(s)	(TY_flags(s) &= ~TY_CHARACTER)
#define       TY_is_dynamic(s)	(TY_flags(s) & TY_DYNAMIC)
#define   Set_TY_is_dynamic(s)	(TY_flags(s) |= TY_DYNAMIC)
#define Reset_TY_is_dynamic(s)	(TY_flags(s) &= ~TY_DYNAMIC)
#define       TY_return_to_param(s)  (TY_flags(s) & TY_RETURN_TO_PARAM)
#define   Set_TY_return_to_param(s)  (TY_flags(s) |= TY_RETURN_TO_PARAM)
#define Reset_TY_return_to_param(s)  (TY_flags(s) &= ~TY_RETURN_TO_PARAM)
#define       TY_is_varargs(s)	(TY_flags(s) & TY_VARARGS)
#define   Set_TY_is_varargs(s)	(TY_flags(s) |= TY_VARARGS)
#define Reset_TY_is_varargs(s)	(TY_flags(s) &= ~TY_VARARGS)
#define       TY_has_prototype(s) (TY_flags(s) & TY_PROTOTYPED)
#define   Set_TY_has_prototype(s) (TY_flags(s) |= TY_PROTOTYPED)
#define Reset_TY_has_prototype(s) (TY_flags(s) &= ~TY_PROTOTYPED)
#define       TY_is_thunk(s)	(TY_flags(s) & TY_THUNK)
#define   Set_TY_is_thunk(s)	(TY_flags(s) |= TY_THUNK)
#define Reset_TY_is_thunk(s)	(TY_flags(s) &= ~TY_THUNK)
#define       TY_is_referenced(s) (TY_flags(s) & TY_REFERENCED)
#define   Set_TY_is_referenced(s) (TY_flags(s) |= TY_REFERENCED)
#define Reset_TY_is_referenced(s) (TY_flags(s) &= ~TY_REFERENCED)
#define       TY_is_volatile(s)	(TY_flags(s) & TY_VOLATILE)
#define   Set_TY_is_volatile(s)	(TY_flags(s) |= TY_VOLATILE)
#define Reset_TY_is_volatile(s)	(TY_flags(s) &= ~TY_VOLATILE)
#define       TY_is_const(s)	(TY_flags(s) & TY_CONST)
#define   Set_TY_is_const(s)	(TY_flags(s) |= TY_CONST)
#define Reset_TY_is_const(s)	(TY_flags(s) &= ~TY_CONST)
#define       TY_is_restrict(s)	(TY_flags(s) & TY_RESTRICT)
#define   Set_TY_is_restrict(s)	(TY_flags(s) |= TY_RESTRICT)
#define Reset_TY_is_restrict(s)	(TY_flags(s) &= ~TY_RESTRICT)
#define       TY_is_qualified(s) (TY_flags(s) & TY_QUALIFIED)
#define       TY_is_logical(s)	(TY_flags(s) & TY_LOGICAL)
#define   Set_TY_is_logical(s)	(TY_flags(s) |= TY_LOGICAL)
#define Reset_TY_is_logical(s)	(TY_flags(s) &= ~TY_LOGICAL)
#define       TY_is_union(s)	(TY_flags(s) & TY_UNION)
#define   Set_TY_is_union(s)	(TY_flags(s) |= TY_UNION)
#define Reset_TY_is_union(s)	(TY_flags(s) &= ~TY_UNION)
#define       TY_is_long(s)	(TY_flags(s) & TY_LONG)
#define   Set_TY_is_long(s)	(TY_flags(s) |= TY_LONG)
#define Reset_TY_is_long(s)	(TY_flags(s) &= ~TY_LONG)
#define       TY_is_translated_to_c(s)	(TY_flags(s) & TY_TRANSLATED_TO_C)
#define   Set_TY_is_translated_to_c(s)	(TY_flags(s) |= TY_TRANSLATED_TO_C)
#define Reset_TY_is_translated_to_c(s)	(TY_flags(s) &= ~TY_TRANSLATED_TO_C)
#define       TY_is_packed(s)	(TY_flags(s) & TY_PACKED)
#define   Set_TY_is_packed(s)	(TY_flags(s) |= TY_PACKED)
#define Reset_TY_is_packed(s)	(TY_flags(s) &= ~TY_PACKED)
#define       TY_ptr_as_array(s)	(TY_flags(s) & TY_PTR_AS_ARRAY)
#define   Set_TY_ptr_as_array(s)	(TY_flags(s) |= TY_PTR_AS_ARRAY)
#define Reset_TY_ptr_as_array(s)	(TY_flags(s) &= ~TY_PTR_AS_ARRAY)
#define       TY_anonymous(s)	(TY_flags(s) & TY_ANONYMOUS)
#define   Set_TY_anonymous(s)	(TY_flags(s) |= TY_ANONYMOUS)
#define Reset_TY_anonymous(s)	(TY_flags(s) &= ~TY_ANONYMOUS)
#define       TY_split(s)	(TY_flags(s) & TY_SPLIT)
#define   Set_TY_split(s)	(TY_flags(s) |= TY_SPLIT)
#define Reset_TY_split(s)	(TY_flags(s) &= ~TY_SPLIT)
#define       TY_last_in_split(s)	(TY_flags(s) & TY_LAST_IN_SPLIT)
#define   Set_TY_last_in_split(s)	(TY_flags(s) |= TY_LAST_IN_SPLIT)
#define Reset_TY_last_in_split(s)	(TY_flags(s) &= ~TY_LAST_IN_SPLIT)
#define       TY_entered(s)	(TY_flags(s) & TY_ENTERED)
#define   Set_TY_entered(s)	(TY_flags(s) |= TY_ENTERED)
#define Reset_TY_entered(s)	(TY_flags(s) &= ~TY_ENTERED)
#define       TY_no_ansi_alias(s)	(TY_flags(s) & TY_NO_ANSI_ALIAS)
#define   Set_TY_no_ansi_alias(s)	(TY_flags(s) |= TY_NO_ANSI_ALIAS)
#define Reset_TY_no_ansi_alias(s)	(TY_flags(s) &= ~TY_NO_ANSI_ALIAS)
#define       TY_is_f90_pointer(s)	(TY_flags(s) & TY_F90_POINTER)
#define   Set_TY_is_f90_pointer(s)	(TY_flags(s) |= TY_F90_POINTER)
#define Reset_TY_is_f90_pointer(s)	(TY_flags(s) &= ~TY_F90_POINTER)
#define       TY_is_f90_target(s)	(TY_flags(s) & TY_F90_TARGET)
#define   Set_TY_is_f90_target(s)	(TY_flags(s) |= TY_F90_TARGET)
#define Reset_TY_is_f90_target(s)	(TY_flags(s) &= ~TY_F90_TARGET)
#define	      TY_not_in_union(s)	(TY_flags(s) & TY_NOT_IN_UNION)
#define   Set_TY_not_in_union(s)	(TY_flags(s) |= TY_NOT_IN_UNION)
#define Reset_TY_not_in_union(s)	(TY_flags(s) &= ~TY_NOT_IN_UNION)

#define TY_enum_const_list(s) (KIND_IS_SCALAR(s)->spec_type_info.ec_list)
#define TY_flist(s)	(KIND_IS_STRUCT(s)->spec_type_info.flist)
#define TY_fld(s)	(KIND_IS_STRUCT(s)->spec_type_info.flist)
#define TY_mlist(s)	(KIND_IS_CLASS(s)->spec_type_info.mlist)
#define TY_clist(s)	(KIND_IS_STRUCT_CLASS(s)->spec_type_info.clist)
#define TY_arinfo(s)	(KIND_IS_ARRAY(s)->spec_type_info.arinfo)
#define TY_pointed(s)	(KIND_IS_POINTER(s)->spec_type_info.pointed)
#define TY_ftinfo(s)	(KIND_IS_FUNCTION(s)->spec_type_info.ftinfo)
#define TY_parms(s)	(FTI_parms(TY_ftinfo(s)))
#define TY_ret_type(s)  (FTI_ret_type(TY_ftinfo(s)))
#define TY_arg_area_size(s)	(FTI_arg_area_size(TY_ftinfo(s)))

#define TY_AR_etype(s)		(ARI_etype(TY_arinfo(s)))
#define TY_etype(t)		TY_AR_etype(t)
#define TY_AR_ndims(s)		(ARI_ndims(TY_arinfo(s)))
#define TY_AR_const_zofst(s)	(ARI_const_zofst(TY_arinfo(s)))
#define TY_AR_zofst_val(s)	(ARI_zofst_val(TY_arinfo(s)))
#define TY_AR_zofst_tree(s)	(ARI_zofst_tree(TY_arinfo(s)))
#define TY_AR_bnds(s,i)		(ARI_bnd(TY_arinfo(s),IS_VALID_DIM((s),(i))))
#define TY_AR_const_stride(s,i)	(ARB_const_stride(TY_AR_bnds(s,i)))
#define TY_AR_stride_val(s,i)	(ARB_stride_val(TY_AR_bnds(s,i)))
#define TY_AR_stride_tree(s,i)	(ARB_stride_tree(TY_AR_bnds(s,i)))
#define TY_AR_const_lbnd(s,i)	(ARB_const_lbnd(TY_AR_bnds(s,i)))
#define TY_AR_lbnd_val(s,i)	(ARB_lbnd_val(TY_AR_bnds(s,i)))
#define TY_AR_lbnd_tree(s,i)	(ARB_lbnd_tree(TY_AR_bnds(s,i)))
#define TY_AR_const_ubnd(s,i)	(ARB_const_ubnd(TY_AR_bnds(s,i)))
#define TY_AR_ubnd_val(s,i)	(ARB_ubnd_val(TY_AR_bnds(s,i)))
#define TY_AR_ubnd_tree(s,i)	(ARB_ubnd_tree(TY_AR_bnds(s,i)))
/* for compatibility: */
#define TY_AR_const_fbnd(s,i)	TY_AR_const_lbnd(s,i)
#define TY_AR_fbnd_val(s,i)	TY_AR_lbnd_val(s,i)
#define TY_AR_fbnd_tree(s,i)	TY_AR_lbnd_tree(s,i)
#define TY_AR_const_tbnd(s,i)	TY_AR_const_ubnd(s,i)
#define TY_AR_tbnd_val(s,i)	TY_AR_ubnd_val(s,i)
#define TY_AR_tbnd_tree(s,i)	TY_AR_ubnd_tree(s,i)

#define TY_pointer(s)	(CAN_USE_TY(s)->pointer)

/* For Delta C++, a class size may be dynamic, i.e. unknown at compile
 * time.  
 */
#define  TY_DYNAMIC_SIZE	-1
#define  TY_DYNAMIC_ALIGN	0

/* ====================================================================
 *
 * ST: Symbol table element
 *
 * There is one ST record for each declared "variable", plus one for
 * each formal parm.  "Variable" in this context means a chunk of
 * contiguous bytes the compiler allocates, either statically or on
 * the stack.  Usually this is just the ordinary (top-level)
 * variables. For Fortran however, each common block and each
 * equivalence class is a single "variable".
 *
 * The ST records carry info about the nature of the variable and its
 * base address, and point to a TY tree to give the type.
 *
 * ====================================================================
 */

/* --------------------------------------------------------------------
 * Symbol class:
 *
 * The following enumerate the various classes of symbol which are
 * represented by symbol table entries.
 * --------------------------------------------------------------------
 */

typedef enum {
  CLASS_BAD,	/* 0x00: A mistake */
  CLASS_NEW,	/* 0x01: A new name, not yet resolved */
  CLASS_UNK,	/* 0x02: Unknown class, e.g. Fortran symbol declared
		 * EXTERNAL but never used in context identifying its
		 * class. */
  CLASS_VAR,	/* 0x03: A variable, i.e. data */
  CLASS_FUNC,	/* 0x04: A function, i.e. address of a function */
  CLASS_CONST,	/* 0x05: A constant value, possibly in memory */
  CLASS_LABEL,	/* 0x06: A label, i.e. address within a function */
  CLASS_SYM_CONST, /* 0x07: A symbolic constant, i.e. linker resolved */
  CLASS_PREG,	/* 0x08: A pseudo-register */
  CLASS_BLOCK,	/* 0x09: A base to a block of data */
  CLASS_COUNT	/* For checking purposes */
} ST_CLASS;

/* The following must satisfy CLASS_MASK+1 >= CLASS_COUNT: */
#define CLASS_MASK  	0x0f

/* --------------------------------------------------------------------
 * Symbol Export Class:
 *
 * The following enumerate the various possible scopes relevant to
 * symbols exported from a file (i.e. linker globals).  Except for the
 * first two, they are only relevant to DSO-related compilation.
 * --------------------------------------------------------------------
 */

typedef enum {
  EXPORT_LOCAL,		/* Not exported, e.g. C static */
  EXPORT_INTERNAL,	/* Exported, only visible and used within the
			 * containing DSO/executable, i.e. not even
			 * passed outside using a pointer. */
  EXPORT_HIDDEN,	/* Exported, but name is hidden within the
			 * containing DSO/executable.  However, the
			 * address may be exported from the DSO via a
			 * pointer. */
  EXPORT_PROTECTED,	/* Exported from DSO, but non-preemptible. */
  EXPORT_PREEMPTIBLE,	/* Exported and preemptible. */
  EXPORT_OPTIONAL,   	/* STO_OPTIONAL case in "sys/elf.h" */
  EXPORT_COUNT		/* Must be last for consistency checking */
} ST_EXPORT;

/* NOTE: Only an EXPORT_LOCAL symbol must be defined in the current
 * file.  All but EXPORT_PREEMPTIBLE must be defined in the current
 * DSO/executable.
 */

/* The following must satisfy EXPORT_MASK+1 >= EXPORT_COUNT: */
#define EXPORT_MASK	0x07

/* optional symbols are also preemptible */
#define ST_is_preemptible(s)	\
  (ST_export(s) == EXPORT_PREEMPTIBLE || ST_export(s) == EXPORT_OPTIONAL)

/* --------------------------------------------------------------------
 * Storage Class:
 *
 * The following enumerate the various possible storage classes in
 * which front ends may place objects.  The front end should be able
 * to choose one of these classes based strictly on source semantics,
 * and memmodel.c's Allocate_Object should be able to allocate the
 * object to a target segment based on that choice and current target
 * and compilation options.  (WARNING: We are not there yet.)
 *
 * Some of the less obvious classes mean the following:
 *
 *  SCLASS_EXTERN:
 *	Any C or Fortran symbol declared external.  Note that the
 *	export classes above may refine how external it is.  Such
 *	symbols MUST be defined by another module (unless they're
 *	weak, in which case they could remain undefined).  Therefore,
 *	size is irrelevant for them.
 *
 *  SCLASS_FSTATIC:		C static data
 *  SCLASS_UGLOBAL:
 *  SCLASS_DGLOBAL:
 *	C static (i.e. non-stack), non-extern data falls in one of
 *	these classes.  If it is declared "static," either at file
 *	scope or within a function, it is SCLASS_FSTATIC, and its
 *	export class is EXPORT_LOCAL.  Initialization is irrelevant.
 *	If it is exported (not declared "static") but is definitely to
 *	be allocated in this module, it is SCLASS_DGLOBAL.  This will
 *	occur if an initial value is provided, or without an initial
 *	value if it is not declared "extern" and the strict ref-def
 *	linkage model is being used.  (See the ANSI C Rationale,
 *	3.1.2.2.)  If it is not extern and not initialized, and the
 *	relaxed ref-def model is being used (-common), it is
 *	SCLASS_UGLOBAL.
 *
 *	Note that for all of these, the object size is required for
 *	allocation -- it is obtained from the type.  If the type's
 *	size is TY_DYNAMIC_SIZE, however, it is a Delta C++ object of
 *	unknown size and special treatment will be required.
 *
 *  SCLASS_FSTATIC:		FORTRAN static data
 *  SCLASS_COMMON:
 *	FORTRAN static data has either PU scope (SCLASS_FSTATIC) or is
 *	in a common block (SCLASS_COMMON).  Note that any external
 *	symbols (SCLASS_EXTERN) in FORTRAN will be subprograms.
 *
 *  SCLASS_THREAD:		Multiprocessing support
 *	Multiprocessing sometimes requires that each thread have its
 *	own copy of a data block.  Such data is SCLASS_THREAD.  It is
 *	allocated in the current module and not exported.
 *
 *  SCLASS_BASED:		Aggregating symbol
 *	A symbol that associates a group of symbols, to provide data
 *      layout mechanism in Mongoose.  Hence, the ST_base of the
 *      ST* cannot be a program variable.
 *
 *  SCLASS_FORMAL_REF:          Fortran formal reference parameter
 *      In Fortran parameters are usually passed by reference.
 *      Marking an ST as having storage class SCLASS_FORMAL_REF
 *      permits the compiler to avoid explicitly generating a dereference
 *      operator while accessing this data except during the final
 *      lowering phase.  This is currently supported for scalars and
 *      pointers.
 *
 * WARNING: These values share a field with ST_AFORM, so they must not
 * have overlapping values with the address form.
 * --------------------------------------------------------------------
 */

/* Define storage classes: */
typedef enum {
  SCLASS_UNKNOWN,	/* Unknown, not yet defined */
  SCLASS_AUTO,		/* Local stack user variable */
  SCLASS_TEMP_OBSOLETE,	/* Obsolete */
  SCLASS_ACTUAL,	/* Obsolete */
  SCLASS_FORMAL,	/* Formal parameter */
  SCLASS_PSTATIC,	/* Statically-allocated data with PU scope */
  SCLASS_FSTATIC,	/* Statically-allocated data with file scope,
			 * i.e. Fortran SAVE, C static */
  SCLASS_THREAD,	/* Fortran per-thread local data block */
  SCLASS_COMMON,	/* Fortran common block */
  SCLASS_EXTERN,	/* Unallocated C external data or text */
  SCLASS_UGLOBAL,	/* Uninitialized C global data: provisionally
			 * allocated but allocation may be preempted
			 * by another module -- equivalent to Fortran
			 * uninitialized COMMON */
  SCLASS_DGLOBAL,	/* Defined (initialized) C global data:
			 * allocated in this module */
  SCLASS_TEXT,		/* Executable code */
  SCLASS_BASED,		/* Data pointed to by another datum */
  SCLASS_REG,		/* Register variable (PREG) */
  SCLASS_FORMAL_REF,	/* Fortran formal reference parameter */
  SCLASS_LAST		/* Marker -- must be last */
} ST_SCLASS;

#define SCLASS_MASK	0x0f
#define SCLASS_LOCAL	SCLASS_AUTO	/* Obsolete */
#define SCLASS_SAVED	SCLASS_FSTATIC	/* Obsolete */
#define SCLASS_STATIC	SCLASS_UGLOBAL	/* Obsolete */
#define SCLASS_DEFINED	SCLASS_DGLOBAL	/* Obsolete */
#define SCLASS_TEMP 	SCLASS_AUTO	/* Obsolete */

/* --------------------------------------------------------------------
 *
 * Addressing formats:
 *
 * The following enumerate the options for treating the addressing of
 * symbols.
 *
 * AFORM_MEM:  This is the normal memory addressing case.  It indicates
 * an object allocated to memory, where the memory address is the sum
 * of a base and an offset.  The base is represented as a pointer to
 * another ST, where the value of the base for this purpose is the
 * value of the object described, NOT ITS ADDRESS.  In general terms,
 * the address of an object is obtained at runtime by loading its base,
 * and adding the offset.  Note that there are special symbols created
 * as base symbols for static sections, which are considered to have
 * values equal to their addresses -- these are indicated by the flag
 * ST_is_base_sym.
 *
 * AFORM_REG:  This is a formal parameter passed in a register.
 *
 * AFORM_RADICAL:  This is an object allocated to a register, either a
 * user variable or a compiler temporary.  It has an associated "home"
 * TN given by its STV_copy_TN.  The model used for assignments to such
 * objects (called radicals) is that the assigned value is copied to
 * the home TN.
 *
 * WARNING: These values share a field with ST_SCLASS, so they must not
 * have overlapping values with the storage class.
 *
 * --------------------------------------------------------------------
 */

typedef enum {
  AFORM_MEM	= 0x00,	/* Memory: ST_base+ST_ofst */
  AFORM_UNUSED	= 0x02,	/* */
  AFORM_REG	= 0x04,	/* Formal in reg: dedicated reg ST_reg_tnnum */
  AFORM_RADICAL	= 0x06,	/* Radical: STV_copy_TN contains value */
  AFORM_MASK	= 0x06	/* Must mask all values in field */
} ST_AFORM;

/* Fortran multiple entrypoints require copying formals on entry from
 * the location where they are passed to their "home" locations, which
 * are consistent for all entries.  In this case, a clone of the formal
 * (or of its base address symbol for a reference parameter) is made to
 * represent what is passed, and the formal symbol itself represents
 * the homed value.  The copied formal may have either address format
 * appropriate for formals (i.e. AFORM_MEM or AFORM_REG).  Its
 * STV_cform field points to the formal copied (or the base for
 * reference parameters), and its STV_copy_TN field points to a TN
 * used for copying it on entry.  Note that the copied formal ST should
 * never be referenced except in the entry code.
 */
#define STFL_CFORM	0x10

/* --------------------------------------------------------------------
 * SBLK:
 *
 * This structure exists for CLASS_BLOCK ST's.  It contains extra information
 * used for the data layout.
 * --------------------------------------------------------------------
 */

struct sblk {
	mINT64 size;		/* size of the block */
	mUINT16 align;		/* alignment of the block: 1,2,4,8 */
	mINT16 temp;		/* temp field */
	mUINT8 gp_group;	/* gp_group of block */
	mUINT64 section_idx;	/* idx to section table */
};
struct stch {
  ST			*item;
  struct stch		*next;
};
#define STCH_item(s)	((CAN_USE_STCH(s))->item)
#define STCH_next(s)	((CAN_USE_STCH(s))->next)

/* --------------------------------------------------------------------
 * Symbol table element:
 * --------------------------------------------------------------------
 */

/* Define a symbol table element: */
struct st {
  mINT64   	ofst;		/* Offset from base (bytes) */
  union {
    struct tcon	*tcon;		/* CLASS_CONST: value */
    char	*name;		/* Other: Name */
  } id;
  ST_CLASS	symclass :8;	/* Class of symbol */
  ST_SCLASS	sclass :8;	/* Storage class */
  mUINT8	level;		/* Display level (global = 0; max 7 bits) */
  mUINT8	scope_id;	/* Scope in which declared (non-{SYM_}CONST) */
  union {
    mINT32	hashval;	/* CLASS_CONST, SYM_CONST */
    mUINT32	index;		/* non-{SYM_}CONST (only 24 bits used) */
  } uindex;
  mUINT32	symtab_id;	/* Symtab in which declared */
  mUINT32	flags;		/* Miscellaneous attributes */
  TY		*type;		/* Declared type of symbol */
  ST		*base;		/* Base of address, e.g. .DATA, $sp */
  void*		temp;		/* dummy field for use within a phase */
  union {
    ST		*common;	/* ST for full common */
    ST		*strong;	/* ST for strong alias */
    ST		*formalBase;	/* ST for SCLASS_FORMAL_REF base */
    mPREG_NUM	preg;		/* Preg no for formal */
  } full;
  union {		/* per class: */
    struct {		    /* CLASS_VAR: */
      mUINT32	flags2;		/* extra flag space */
      void      *address_taken_field_bv;	/* structures: address of a field taken (bitvector) */
		/* the above void* to be replaced by BV* when the
		   implementation is done */
    } vs;
    struct {                /* CLASS_FUNC: */
      mUINT32	flags2;		/* extra flag space */
      mUINT16   int_reg_used;   /* bit mask for mips_registers_used pragma */
      mUINT16   float_reg_used; /* bit mask for mips_registers_used pragma */
    } fs;
    struct {		    /* CLASS_LABEL: */
      mUINT32	label_number;	/* whirl number of LABEL */
      void*	ltemp;		/* temp field */
    } ls;
    mINT32	ct_idx;	    /* CLASS_CONST: const table index */
    struct {		    /* CLASS_SYM_CONST: */
      INTSC	disp;		/* Displacement */
      union {			/* Form depends on ST_sym_simple */
	ST*	refsym;		/* Simple: Referenced symbol */
	struct symexpr *expr;	/* General: Value tree */
      } scu;
    } scs;
    SBLK	*block_info;  /* CLASS_BLOCK: extra info */
  } uc;			/* end per-class union */
  ST 		*next;
};

/* --------------------------------------------------------------------
 * Symbol table base field access:
 * --------------------------------------------------------------------
 */

/* Define the access functions for the base fields: */
#define ST_name(s) 	(CAN_USE_ST(s)->id.name)
#define Set_ST_name(s,n)	(ST_name(s) = n)
#define STC_tcon(s)	(CAN_USE_ST(s)->id.tcon)
#define ST_tcon(s)	(STC_tcon(s)) /* for compatibility */
#define STC_val(s)	(*STC_tcon(s))

#define ST_class(s)	(CAN_USE_ST(s)->symclass)    /* for compatibility */
#define ST_symclass(s)	(CAN_USE_ST(s)->symclass)
#define ST_sclass(s)	(CAN_USE_ST(s)->sclass)
#define Set_ST_sclass(s,c)	(ST_sclass(s) = c)
#define Set_ST_classes(s,c,sc)	(ST_class(s) = c, ST_sclass(s) = sc)
#define ST_flags(s)	(CAN_USE_ST(s)->flags)
#define ST_scope_id(s)	(CAN_USE_ST(s)->scope_id)
#define ST_level(s)	(CAN_USE_ST(s)->level)
#define ST_index(s)	(CAN_USE_ST(s)->uindex.index)
#define ST_hashval(s)	(CLASS_IS_CONST_SYM(s)->uindex.hashval)
#define ST_symtab_id(s)	(CAN_USE_ST(s)->symtab_id)
#define ST_type(s)	(CAN_USE_ST(s)->type)
#define Set_ST_type(s,t)	(ST_type(s) = t)
#define ST_btype(s)	(TY_btype(ST_type(s)))
#define ST_base(s)	(CAN_USE_ST(s)->base)
/* by default, ST_base points to self, will point to block if allocated */
#define Has_Base_Block(st)	(ST_base(st) != st)
#define Set_ST_base(s,b)	(ST_base(s) = b)
#define ST_ofst(s)	(CAN_USE_ST(s)->ofst)
#define Set_ST_ofst(s,o)	(ST_ofst(s) = o)
#define ST_size(s)	(TY_size(ST_type(s)))
#define ST_full(s)	(CAN_USE_ST(s)->full.common)
#define ST_strong(s)	(CAN_USE_ST(s)->full.strong)	/* ST_strong is alias to ST_full */
#define ST_formal_ref_base(s)	(CAN_USE_ST(s)->full.formalBase) /* Formal Ref points to base */
#define ST_formal_preg_no(s)	((s)->full.preg) /* Formal may contain preg number */
#define ST_temp(s)	(CAN_USE_ST(s)->temp)
#define Set_ST_temp(s,v)	(ST_temp(s) = (void*)v)
/* ST_flags2 is for when it can be either VAR or FUNC */
#define ST_flags2(s)	(CLASS_IS_VAR_FUNC(s)->uc.vs.flags2)
#define STV_flags2(s)	(CLASS_IS_VAR(s)->uc.vs.flags2)
#define STV_address_taken_field_bv(s)	(CLASS_IS_VAR(s)->uc.vs.address_taken_field_bv)
#define STF_flags2(s)	(CLASS_IS_FUNC(s)->uc.fs.flags2)
#define STF_int_reg_used(s)	(CLASS_IS_FUNC(s)->uc.fs.int_reg_used)
#define STF_float_reg_used(s)	(CLASS_IS_FUNC(s)->uc.fs.float_reg_used)
#define STL_label_number(s)	(CLASS_IS_LABEL(s)->uc.ls.label_number)
#define STL_ltemp(s)	(CLASS_IS_LABEL(s)->uc.ls.ltemp)
#define Set_STL_ltemp(s,v)	(STL_ltemp(s) = (void*)v)
#define STC_ct_idx(s)	(CLASS_IS_CONST(s)->uc.ct_idx)
#define STSC_refsym(s)	(CLASS_IS_SYM_CONST(s)->uc.scs.scu.refsym)
#define STSC_expr(s)	(CLASS_IS_SYM_CONST(s)->uc.scs.scu.expr)
#define STSC_disp(s)	(CLASS_IS_SYM_CONST(s)->uc.scs.disp)
#define STB_block_info(s)	(CLASS_IS_BLOCK(s)->uc.block_info)
#define STB_size(s)	(STB_block_info(s)->size)
#define Set_STB_size(s,v)	(STB_block_info(s)->size = v)
#define STB_align(s)	(STB_block_info(s)->align)
#define Set_STB_align(s,v)	(STB_block_info(s)->align = v)
#define STB_temp(s)	(STB_block_info(s)->temp)
#define STB_gp_group(s)	(STB_block_info(s)->gp_group)
#define STB_section_idx(s)	(STB_block_info(s)->section_idx)
#define Set_STB_section_idx(s,v)	(STB_block_info(s)->section_idx = v)
#define ST_next(s)  	(CAN_USE_ST(s)->next)

/* Return a unique identifier for an ST.  The most significant bit of
 * the result must always be zero (negative values are reserved for
 * constant symbols).  The next 7 bits specify the nesting level of the
 * SYMTAB where the ST is defined.  This assumes that STs are never
 * referenced outside the scopes where they are visible, and it requires
 * the current SYMTAB to identify the ST.  The final 24 bits are the
 * ST_index field. */

#define ST_id(s) \
	((ST_index(s) & 0xffffff) | ((ST_level(s) & 0x7f) << 24))

/* is full-split COMMON symbol */
#define Is_Full_Split_Common(st) \
        (ST_sclass(st) == SCLASS_COMMON && ST_full(st) != NULL)
#define ST_is_split_common(st)	Is_Full_Split_Common(st)

#define Is_Formal_Preg(st) \
        (ST_sclass(st) == SCLASS_FORMAL && ST_formal_preg_no(st) != 0)

/* --------------------------------------------------------------------
 * Symbol table flag/subfield access -- ST_flags masks:
 * --------------------------------------------------------------------
 */

/* Low-order is EXPORT_MASK, defined above as 0x07. */
#define STFL_WEAK_SYMBOL	0x00000008 /* Weak external name */
#define STFL_WEAK_ALIAS		0x00000010 /* Weak external is aliased
					    * to its ST_base */
#define STFL_REFERENCED		0x00000020 
            /* Symbol is referenced.  Any setting of this flag only
             * applies locally to a particular algorithm in a 
             * particular compiler-phase, and should not be relied 
             * upon across such algorithms/phases. */
#define STFL_NOT_USED		0x00000040 /* Symbol is not referenced; 
					this flag is preserved across phases. */

#define STFL_GLOBAL		0x00000080 /* Symbol in global symtab */
#define STFL_INITIALIZED	0x00000100 /* Symbol is initialized	*/

#define STFL_ADDR_TAKEN_SAVED	0x00000200 /* Addr taken and saved */
#define STFL_ADDR_USED_LOCALLY	0x00000400 /* Addr taken but not saved */
	/* CLASS_VAR or CLASS_FUNC: address of object is taken */
#define STFL_STATIC_MBR		0x00000800	
	/* CLASS_VAR or CLASS_FUNC: static class member */

#define STFL_IS_CONST_VAR	0x00001000 /* CLASS_VAR, CBLK: const */
#define STFL_USE_REG_ALIGN	0x00002000 /* use register alignment */
				   /* for temps that don't want ty alignment */
#define STFL_IS_PADDED	       	0x00004000
	/* CLASS_VAR or CLASS_CONST: Object padded at end */
#define STFL_IS_RETURN_VAR	0x00008000 /* CLASS_VAR: Return value */
#define STFL_IS_VALUE_PARM	0x00010000 /* CLASS_VAR: Value parm */
#define STFL_PROMOTE_PARM	0x00020000 /* CLASS_VAR: Promote C formal */
#define STFL_IS_NOT_ALIASED	0x00040000 /* CLASS_VAR: not aliased */
	/* This flag implies that the marked object is not aliased to
	 * any other object accessible in the current scope, and if it
	 * is a pointer (including C arrays) the same applies to the
	 * pointee.  It should be set by default for FORTRAN formals.
	 */
#define STFL_GPREL		0x00080000 /* CLASS_VAR/BLOCK: data is GP-relative */
#define STFL_FORMAL_REF		0x00100000 /* CLASS_VAR: Reference to formal */
#define STFL_TEMP_VAR		0x00200000 /* CLASS_VAR: compiler temp */
	/* This flag means the value of the object is its address */
#define STFL_USE_CPLINIT	0x00400000 /* CLASS_VAR: alloc in cplinit */
#define STFL_IS_RESTRICT	0x00800000 /* CLASS_VAR, CBLK: */
					   /* restrict */
/* The following flag means that every reference to memory pointed to
 * by the symbol in question must use the symbol in question as a
 * pointer. In particular, if this flag is set on the ST for x, it is
 * not allowed to write:
 *   y = x;
 *   *y     <---- illegal access to memory pointed to by x, because
 *                the access does not dereference x.
 */
#define STFL_PT_TO_UNIQUE_MEM	0x01000000 /* CLASS_VAR: every */
					   /* reference to memory */
					   /* pointed to by this */
					   /* symbol must be a */
					   /* dereference of this */
					   /* symbol */
#define STFL_ADDR_TAKEN_PASSED  0x02000000
#define STFL_KEEP_NAME_W2F	0x04000000 /* CLASS_VAR: don't mangle name */
#define STFL_IS_DATAPOOL	0x08000000 /* CLASS_VAR: represents datapool */
#define STFL_USE_EH_REGION	0x10000000 /* CLASS_VAR: allocate in eh_region */
#define STFL_USE_EH_REGION_SUPP 0x20000000 /* CLASS_VAR: allocate in eh_region_supp */
#define STFL_USE_DISTR_ARRAY	0x40000000 /* CLASS_VAR: alloc in distr_array */
#define STFL_MAY_NOT_BE_ADDR_TAKEN	0x80000000 /* CLASS_VAR: may be possible to remove addr_taken bits after inlining */

	/* CLASS_VAR or CLASS_FUNC: address of object is taken and passed */

/* Flags for CLASS_CONST objects: */
#define STFL_ADD_NULL           0x00008000 /* F77 add null to this char literal */

/* Flags for CLASS_SYM_CONST objects: */
#define STFL_SYM_SIMPLE		0x00008000 /* Simple address form */
#define STFL_SYM_UNIQUE		0x00010000 /* Unique -- no reuse */

/* Flags for CLASS_BLOCK objects: */
#define STFL_DECREMENT		0x00000200 /* grow block by decrementing */
#define STFL_WRITE		0x00000400 /* (ELF) writable section */
#define STFL_EXEC		0x00000800 /* (ELF) executable instructions */
#define STFL_NOBITS		0x00001000 /* (ELF) occupies no space in file */
#define STFL_MERGE		0x00002000 /* (ELF) merge duplicates in ld */
#define STFL_ADDR		0x00004000 /* (ELF) addresses only */
#define STFL_NAMES		0x00008000 /* (ELF) section with SHF_MIPS_NAMES
						attribute (cplinit section) */
#define STFL_STATIC		0x00010000 /* Segment contains data allocated
						statically (not on stack) */
#define STFL_SECTION		0x00020000 /* block symbol for elf section */
#define STFL_ROOT_BASE		0x00040000 /* block symbol should not be merged */
/* STFL_GPREL is also for blocks */
#define STFL_IS_BASEREG		0x00100000 /* block symbol that maps into reg*/
#define STFL_NOSTRIP		0x00200000 /* (ELF) nostrip section */

/* Flags for CLASS_LABEL objects: */
#define STFL_LABEL_ASSIGNED	0x00000200 /* In ASSIGN statement */
#define STFL_IN_COMPGOTO_LST 	0x00000400 /* In computed GOTO list */
#define STFL_LKIND		0x00003000 /* Kind of label: */
#define	    LKIND_UNK		0x00000000 /*   Unknown kind */ 
#define	    LKIND_FMT		0x00001000 /*   FORTRAN format */ 
#define	    LKIND_EXEC		0x00002000 /*   Executable */ 
#define	    LKIND_SPEC		0x00003000 /*   Label on spec statement */ 
#define	    LKIND_SHIFT		12	   /* To shift into place (f90) */ 
#define STFL_BEGIN_EH_RANGE     0x00010000
#define STFL_END_EH_RANGE       0x00020000
#define STFL_BEGIN_HANDLER	0x00040000
#define STFL_END_HANDLER	0x00080000

/* Flags for CLASS_FUNC objects: */
/*	STFL_ADDR_USED_LOCALLY	0x00000400	above	*/
/*	STFL_ADDR_TAKEN_PASSED	0x02000000	above	*/
/*	STFL_STATIC_MBR		0x00000800	above	*/
#define STFL_PU_IS_LEAF		0x00001000 /* PU: is a leaf PU */
	/* is_leaf is never used? */
#define STFL_PU_IS_MAINPU	0x00002000 /* PU: is the MAIN__ PU */
#define STFL_PU_NO_SIDE_EFFECTS	0x00004000 /* PU: has no side effects */
	/* no_se is never used? */
#define STFL_PU_IS_PURE		0x00008000 /* PU: is is pure */
#define STFL_PU_RECURSIVE       0x00010000 /* PU: f90/f77 PU is recursive */
#define STFL_PU_IS_INLINE_FUNCTION 0x00020000 /* PU: inline keyword specified */
#define STFL_PU_NEEDS_T9        0x00040000 /* PU: needs T9 */
#define STFL_PU_IS_BLOCKDATA	0x00080000 /* PU: is blockdata */
#define STFL_PU_NO_INLINE	0x00100000 /* PU: noinline pragma specified */
#define STFL_PU_NO_DELETE	0x00200000 /* PU: nodelete pragma specified */
#define STFL_PU_HAS_EXC_SCOPES	0x00400000 /* PU: PU has eh regions, or would
					    * have if exceptions were enabled */
#define STFL_PU_NO_THROWS	0x00800000 /* PU: PU should not throw an
					    * exception.  Set by the frontend
					    * if function has an empty
					    * exception specification, and
					    * possibly propagated by ipa or
					    * inlining */
#define STFL_PU_THROWS		0x01000000 /* PU: PU should be assumed to throw
					    * exceptions.  Set by the frontend
					    * if the function contains a throw
					    * expression, and possibly
					    * propagated by ipa or inlining */
#define STFL_PU_IS_NESTED_FUNC	0x04000000 /* PU: PU is a nested function */
#define STFL_PU_HAS_NON_MANGLED_CALL 0x08000000 /* PU has a call in which *
                                                 * no reshaped arrays are *
                                                 * passed                 */
#define STFL_PU_IS_CONSTRUCTOR  0x10000000 /* PU: is a constructor */
#define STFL_PU_IS_DESTRUCTOR   0x20000000 /* PU: is a destructor */
#define STFL_PU_ARGS_ALIASED    0x40000000 /* PU: f77 arguments are aliased */
#define STFL_PU_IN_ELF_SECTION  0x80000000 /* This function should go into 
					    * it own text section. */

/*
 *  The following flags must be variable flags that go into flags2.
 */
#define STFL_IS_RESHAPED	0x00000001 /* lno may reshape the ST */
#define STFL_EMIT_SYMBOL	0x00000002 /* emit the empty dummy symbol */
#define STFL_HAS_NESTED_REF	0x00000004 /* has ref in nested pu */
#define STFL_INIT_VALUE_ZERO	0x00000008 /* has initial value of zero */
#define STFL_FORCE_GPREL	0x00000010 /* force object to be gp-relative */
#define STFL_FORCE_NOT_GPREL	0x00000020 /* force object to not be gp-relative */
#define STFL_IS_NAMELIST	0x00000040 /* namelist table */
#define STFL_IS_F90_POINTER	0x00000080 /* F90 pointer */
#define STFL_IS_F90_TARGET	0x00000100 /* F90 target */
#define STFL_DECLARED_STATIC	0x00000200 /* VMS formals declared static */
#define STFL_IS_EQUIVALENCED	0x00000400 /* is part of an equivalence */
#define STFL_IS_THIS_POINTER    0x00000800 /* C++ this pointer */
#define STFL_IS_AUTO_OR_CPOINTER  0x00001000 /* F90 Automatic or Cray Pointer */
#define STFL_IS_NON_CONTIGUOUS    0x00002000 /* F90 non-contiguous array */
#define STFL_IS_FILL_ALIGN        0x00004000 /* This symbol came with a
                                              * fill/align pragma.
                                              */
#define STFL_IS_OPTIONAL_ARGUMENT 0x00008000 /* F90 OPTIONAL arguments */
#define STFL_IS_THREAD_PRIVATE	0x00010000 /* LCOMMON */

/* WARNING WARNING WARNING WARNING WARNING WARNING WARNING WARNING WARNING
 *
 * Cannot use bits 0xff000000 as they are reserved for gp_group.
 *
 * WARNING WARNING WARNING WARNING WARNING WARNING WARNING WARNING WARNING
 */

#define STFL_PU_NEEDS_F90_LOWERING 0x00000001 /* PU: needs F90 Lowering */
#define STFL_PU_HAS_SINGLE_RETURN  0x00000002 /* PU: has a single return */
#define STFL_PU_NO_GP_PROLOG       0x00000004 /* PU: doesn't need GP prolog */
#define STFL_PU_MUST_INLINE        0x00000008 /* PU: must inline */
#define STFL_PU_CALLS_SETJMP       0x00000010 /* PU: has a call to setjmp */
#define STFL_PU_CALLS_LONGJMP      0x00000020 /* PU: has a call to longjmp */
#define STFL_PU_NEEDS_FILL_ALIGN_LOWERING   0x00004000 /* This PU needs
                                                        * fill/align lowering
                                                        */

/*
 *  The following flags must be function flags that go into flags2.
 */

/* gp_group is 8 bits at msb of flags2, for both vars and funcs */
#define DEFAULT_GP_GROUP 0
#define MAX_GP_GROUP	0xff
#define GPGROUP_MASK	0xff000000
#define GPGROUP_SHIFT	24

/* Definitions:
 *
 * A leaf PU does not call any other routines.
 *
 * A PU has no side effects if it does no I/O, does not modify
 * global data, and does not affect global machine state.  Therefore,
 * a call to such a PU which does not use its function result may be
 * optimized away.  Such a PU may not reference volatile data.  It may,
 * however, reference global data, which is what distinguishes it from
 * a pure function.
 *
 * A function is pure if it has no side effects and its result depends
 * only on its operand values.  If the operands of such a function are
 * loop-invariant, its result will be too, and the call may be moved
 * out of the loop.
 */

/* --------------------------------------------------------------------
 * Symbol table flag/subfield access -- ST_flags access macros:
 * --------------------------------------------------------------------
 */

/* Export class: */
#define     ST_export(s)	(ST_flags(s) & EXPORT_MASK)
#define Set_ST_export(s,c)	(ST_flags(s)=(ST_flags(s)&(~EXPORT_MASK))|(c))

/* Flags for CLASS_VAR objects: */
#define ST_CVF_flags(s)		ST_flags(CLASS_IS_VAR_FUNC(s))
#define ST_CVC_flags(s)		ST_flags(CLASS_IS_VAR_CONST(s))
#define ST_CVB_flags(s)		ST_flags(CLASS_IS_VAR_BLOCK(s))
#define ST_CV_flags(s)		ST_flags(CLASS_IS_VAR(s))
#define ST_CV_flags2(s)		STV_flags2(CLASS_IS_VAR(s))
#define ST_CVF_flags2(s)	ST_flags2(CLASS_IS_VAR_FUNC(s))
#define ST_SC_flags(s)		ST_flags(CLASS_IS_SYM_CONST(s))

/* Flags for all symbols: */
#define     ST_is_weak_symbol(s) (ST_flags(s) & STFL_WEAK_SYMBOL)
#define Set_ST_is_weak_symbol(s) (ST_flags(s) |= STFL_WEAK_SYMBOL)
#define     ST_is_weak_alias(s)  (ST_flags(s) & STFL_WEAK_ALIAS)
#define Set_ST_is_weak_alias(s)  (ST_flags(s) |= STFL_WEAK_ALIAS)
#define       ST_is_referenced(s)	 (ST_flags(s) & STFL_REFERENCED)
#define   Set_ST_is_referenced(s)	 (ST_flags(s) |= STFL_REFERENCED)
#define Reset_ST_is_referenced(s)	 (ST_flags(s) &= ~STFL_REFERENCED)
#define       ST_is_not_used(s)	 (ST_flags(s) & STFL_NOT_USED)
#define   Set_ST_is_not_used(s)	 (ST_flags(s) |= STFL_NOT_USED)
#define Reset_ST_is_not_used(s)	 (ST_flags(s) &= ~STFL_NOT_USED)
#define     ST_is_global(s)	 (ST_flags(s) & STFL_GLOBAL)
#define Set_ST_is_global(s)	 (ST_flags(s) |= STFL_GLOBAL)
#define Reset_ST_is_global(s)	 (ST_flags(s) &= ~STFL_GLOBAL)

#define       ST_is_initialized(s)	(ST_flags(s) &  STFL_INITIALIZED)
#define   Set_ST_is_initialized(s)	(ST_flags(s) |= STFL_INITIALIZED)
#define Reset_ST_is_initialized(s)	(ST_flags(s) &= ~STFL_INITIALIZED)

/* ST_is_formal_ref   "replaces" ST_seg_formal_ref */
#define       ST_is_formal_ref(s)	(ST_flags(s) &  STFL_FORMAL_REF)
#define   Set_ST_is_formal_ref(s)	(ST_flags(s) |= STFL_FORMAL_REF)
#define Reset_ST_is_formal_ref(s)	(ST_flags(s) &= ~STFL_FORMAL_REF)

/* ST_is_temp_var	"replaces" ST_seg_temp_var */
#define       ST_is_temp_var(s)		(ST_flags(s) &  STFL_TEMP_VAR)
#define   Set_ST_is_temp_var(s)		(ST_flags(s) |= STFL_TEMP_VAR)
#define Reset_ST_is_temp_var(s)		(ST_flags(s) &= ~STFL_TEMP_VAR)

#define       ST_addr_taken_saved(s) (ST_CVF_flags(s) & STFL_ADDR_TAKEN_SAVED)
#define   Set_ST_addr_taken_saved(s) (ST_CVF_flags(s) |= STFL_ADDR_TAKEN_SAVED)
#define Reset_ST_addr_taken_saved(s) (ST_CVF_flags(s) &= ~STFL_ADDR_TAKEN_SAVED)
#define       ST_addr_used_locally(s) (ST_CVF_flags(s) & STFL_ADDR_USED_LOCALLY)
#define   Set_ST_addr_used_locally(s) (ST_CVF_flags(s) |= STFL_ADDR_USED_LOCALLY)
#define Reset_ST_addr_used_locally(s) (ST_CVF_flags(s) &= ~STFL_ADDR_USED_LOCALLY)
#define       ST_addr_taken_passed(s) \
				(ST_CVF_flags(s) & STFL_ADDR_TAKEN_PASSED)
#define   Set_ST_addr_taken_passed(s) \
				(ST_CVF_flags(s) |= STFL_ADDR_TAKEN_PASSED)
#define Reset_ST_addr_taken_passed(s) \
				(ST_CVF_flags(s) &= (~STFL_ADDR_TAKEN_PASSED))
#define ST_addr_taken(s) \
 (ST_addr_taken_saved(s) || ST_addr_taken_passed(s) || ST_addr_used_locally(s))

/* CG needs to know if a symbol is visible outside the DSO. This is used to
 *  a) check if we need to convert OPR_CALL to OPR_PICCALL.
 *  b) check if we need to setup a new GP on entry to a procedure.
 *
 * "bogus_" added because ST_pu_no_gp_prolog() has nothing to do with
 * whether the symbol is visible outside the DSO, and people might
 * otherwise assume this macro does what it claims to do. Let's name
 * things right, please, rather than assume existing uses will be the
 * only ones ever.   -- RK 970929
 * change name to back to ST_visible_outside_dso(),
 * because no_gp_prolog replaced in new symtab with export-local-internal,
 * and thus no longer pu-dependent.	-- mpm
 */
#define ST_visible_outside_dso(s) \
 ((ST_export(s) > EXPORT_INTERNAL) || \
  ((ST_export(s) == EXPORT_LOCAL) && \
   (ST_addr_taken_saved(s) || ST_addr_taken_passed(s)) && \
   (!ST_pu_no_gp_prolog(s))))
#define bogus_ST_visible_outside_dso(s)	ST_visible_outside_dso(s)

#define       ST_may_not_be_addr_taken(s) \
				(ST_CVF_flags(s) & STFL_MAY_NOT_BE_ADDR_TAKEN)
#define   Set_ST_may_not_be_addr_taken(s) \
				(ST_CVF_flags(s) |= STFL_MAY_NOT_BE_ADDR_TAKEN)
#define Reset_ST_may_not_be_addr_taken(s) \
				(ST_CVF_flags(s) &= (~STFL_MAY_NOT_BE_ADDR_TAKEN))
#define       ST_static_mbr(s)	(ST_CVF_flags(s) & STFL_STATIC_MBR)
#define   Set_ST_static_mbr(s)	(ST_CVF_flags(s) |= STFL_STATIC_MBR)
#define Reset_ST_static_mbr(s)	(ST_CVF_flags(s) &= ~STFL_STATIC_MBR)

#define     ST_is_const_var(s)	(ST_CV_flags(s) & STFL_IS_CONST_VAR)
#define Set_ST_is_const_var(s)	(ST_CV_flags(s) |= STFL_IS_CONST_VAR)
#define     ST_use_reg_align(s)	(ST_CV_flags(s) & STFL_USE_REG_ALIGN)
#define Set_ST_use_reg_align(s)	(ST_CV_flags(s) |= STFL_USE_REG_ALIGN)
#define     ST_is_restrict(s)	(ST_CV_flags(s) & STFL_IS_RESTRICT)
#define Set_ST_is_restrict(s)	(ST_CV_flags(s) |= STFL_IS_RESTRICT)
#define     ST_is_padded(s)	(ST_CVC_flags(s) & STFL_IS_PADDED)
#define Set_ST_is_padded(s)	(ST_CVC_flags(s) |= STFL_IS_PADDED)
#define     ST_is_return_var(s)	(ST_CV_flags(s) & STFL_IS_RETURN_VAR)
#define Set_ST_is_return_var(s)	(ST_CV_flags(s) |= STFL_IS_RETURN_VAR)
#define Reset_ST_is_return_var(s) (ST_CV_flags(s) &= (~STFL_IS_RETURN_VAR))
#define     ST_is_value_parm(s)	(ST_CV_flags(s) & STFL_IS_VALUE_PARM)
#define Set_ST_is_value_parm(s)	(ST_CV_flags(s) |= STFL_IS_VALUE_PARM)
#define Reset_ST_is_value_parm(s) (ST_CV_flags(s) &= (~STFL_IS_VALUE_PARM))

#define     ST_promote_parm(s)	(ST_CV_flags(s) & STFL_PROMOTE_PARM)
#define Set_ST_promote_parm(s)	(ST_CV_flags(s) |= STFL_PROMOTE_PARM)
#define Clear_ST_promote_parm(s)  (ST_CV_flags(s) &= (~STFL_PROMOTE_PARM))

#define     ST_pt_to_unique_mem(s) (ST_CV_flags(s) & STFL_PT_TO_UNIQUE_MEM)
#define Set_ST_pt_to_unique_mem(s) (ST_CV_flags(s) |= STFL_PT_TO_UNIQUE_MEM)
#define     ST_use_cplinit(s) (ST_CV_flags(s) & STFL_USE_CPLINIT)
#define Set_ST_use_cplinit(s) (ST_CV_flags(s) |= STFL_USE_CPLINIT)
#define     ST_use_eh_region(s) (ST_CV_flags(s) & STFL_USE_EH_REGION)
#define Set_ST_use_eh_region(s) (ST_CV_flags(s) |= STFL_USE_EH_REGION)
#define     ST_use_eh_region_supp(s) (ST_CV_flags(s) & STFL_USE_EH_REGION_SUPP)
#define Set_ST_use_eh_region_supp(s) (ST_CV_flags(s) |= STFL_USE_EH_REGION_SUPP)
#define     ST_use_distr_array(s) (ST_CV_flags(s) & STFL_USE_DISTR_ARRAY)
#define Set_ST_use_distr_array(s) (ST_CV_flags(s) |= STFL_USE_DISTR_ARRAY)
#define     ST_is_not_aliased(s) (ST_CV_flags(s) & STFL_IS_NOT_ALIASED)
#define Set_ST_is_not_aliased(s) (ST_CV_flags(s) |= STFL_IS_NOT_ALIASED)
#define       ST_gprel(s)	(ST_CVB_flags(s) & STFL_GPREL)
#define   Set_ST_gprel(s)	(ST_CVB_flags(s) |= STFL_GPREL)
#define Reset_ST_gprel(s)	(ST_CVB_flags(s) &= (~STFL_GPREL))
#define       ST_keep_name_w2f(s)  (ST_CV_flags(s) & STFL_KEEP_NAME_W2F)
#define   Set_ST_keep_name_w2f(s)  (ST_CV_flags(s) |= STFL_KEEP_NAME_W2F)
#define Reset_ST_keep_name_w2f(s)  (ST_CV_flags(s) &= (~STFL_KEEP_NAME_W2F))
#define       ST_is_datapool(s)  (ST_CV_flags(s) & STFL_IS_DATAPOOL)
#define   Set_ST_is_datapool(s)  (ST_CV_flags(s) |= STFL_IS_DATAPOOL)

/* for compatibility: */
#define STB_gprel(s)		ST_gprel(s)
#define Set_STB_gprel(s)	Set_ST_gprel(s)

/* Flags for CLASS_CONST objects: */
#define ST_CC_flags(s)		ST_flags(CLASS_IS_CONST(s))
#define	    STC_add_null(s)   (ST_CC_flags(s) & STFL_ADD_NULL)
#define Set_STC_add_null(s)   (ST_CC_flags(s) |= STFL_ADD_NULL)

/* Flags for CLASS_SYM_CONST objects: */
#define       STSC_sym_simple(s) (ST_SC_flags(s) & STFL_SYM_SIMPLE)
#define   Set_STSC_sym_simple(s) (ST_SC_flags(s) |= STFL_SYM_SIMPLE)
#define Reset_STSC_sym_simple(s) (ST_SC_flags(s) &= (~STFL_SYM_SIMPLE))
#define       STSC_sym_unique(s) (ST_SC_flags(s) & STFL_SYM_UNIQUE)
#define   Set_STSC_sym_unique(s) (ST_SC_flags(s) |= STFL_SYM_UNIQUE)
#define Reset_STSC_sym_unique(s) (ST_SC_flags(s) &= (~STFL_SYM_UNIQUE))

/* Flags for CLASS_BLOCK objects: */
#define ST_BL_flags(s)		ST_flags(CLASS_IS_BLOCK(s))
#define       STB_decrement(s)	(ST_BL_flags(s) & STFL_DECREMENT)
#define   Set_STB_decrement(s)	(ST_BL_flags(s) |= STFL_DECREMENT)
#define Reset_STB_decrement(s)	(ST_BL_flags(s) &= (~STFL_DECREMENT))
#define       STB_exec(s)	(ST_BL_flags(s) & STFL_EXEC)
#define   Set_STB_exec(s)	(ST_BL_flags(s) |= STFL_EXEC)
#define Reset_STB_exec(s)	(ST_BL_flags(s) &= (~STFL_EXEC))
#define       STB_nobits(s)	(ST_BL_flags(s) & STFL_NOBITS)
#define   Set_STB_nobits(s)	(ST_BL_flags(s) |= STFL_NOBITS)
#define Reset_STB_nobits(s)	(ST_BL_flags(s) &= (~STFL_NOBITS))
#define       STB_merge(s)	(ST_BL_flags(s) & STFL_MERGE)
#define   Set_STB_merge(s)	(ST_BL_flags(s) |= STFL_MERGE)
#define Reset_STB_merge(s)	(ST_BL_flags(s) &= (~STFL_MERGE))
#define       STB_section(s)    (ST_BL_flags(s) & STFL_SECTION)
#define   Set_STB_section(s)    (ST_BL_flags(s) |= STFL_SECTION)
#define Reset_STB_section(s)    (ST_BL_flags(s) &= (~STFL_SECTION))
#define       STB_root_base(s)  (ST_BL_flags(s) & STFL_ROOT_BASE)
#define   Set_STB_root_base(s)  (ST_BL_flags(s) |= STFL_ROOT_BASE)
#define Reset_STB_root_base(s)  (ST_BL_flags(s) &= (~STFL_ROOT_BASE))
#define       STB_is_basereg(s) (ST_BL_flags(s) & STFL_IS_BASEREG)
#define   Set_STB_is_basereg(s) (ST_BL_flags(s) |= STFL_IS_BASEREG)
#define Reset_STB_is_basereg(s) (ST_BL_flags(s) &= (~STFL_IS_BASEREG))

/* Flags for CLASS_LABEL objects: */
#define ST_CL_flags(s)		ST_flags(CLASS_IS_LABEL(s))
#define     STL_is_assigned(s)	(ST_CL_flags(s) & STFL_LABEL_ASSIGNED)
#define Set_STL_is_assigned(s)	(ST_CL_flags(s) |= STFL_LABEL_ASSIGNED)
#define     STL_in_compgoto_lst(s)    (ST_CL_flags(s) & STFL_IN_COMPGOTO_LST)
#define Set_STL_in_compgoto_lst(s)    (ST_CL_flags(s) |= STFL_IN_COMPGOTO_LST)
#define     STL_lkind(s)        (ST_CL_flags(s) & STFL_LKIND) 
#define Set_STL_lkind(s,k)      (ST_flags(s)=(ST_CL_flags(s)&(~STFL_LKIND))|(k))
#define     STL_begin_eh_range(s)     (ST_CL_flags(s) & STFL_BEGIN_EH_RANGE)
#define Set_STL_begin_eh_range(s)     (ST_CL_flags(s) |= STFL_BEGIN_EH_RANGE)
#define Reset_STL_begin_eh_range(s)   (ST_CL_flags(s) &= ~STFL_BEGIN_EH_RANGE)
#define     STL_end_eh_range(s)     (ST_CL_flags(s) & STFL_END_EH_RANGE)
#define Set_STL_end_eh_range(s)     (ST_CL_flags(s) |= STFL_END_EH_RANGE)
#define Reset_STL_end_eh_range(s)   (ST_CL_flags(s) &= ~STFL_END_EH_RANGE)
#define     STL_begin_handler(s)       (ST_CL_flags(s) & STFL_BEGIN_HANDLER)
#define Set_STL_begin_handler(s)       (ST_CL_flags(s) |= STFL_BEGIN_HANDLER)
#define     STL_end_handler(s)       (ST_CL_flags(s) & STFL_END_HANDLER)
#define Set_STL_end_handler(s)       (ST_CL_flags(s) |= STFL_END_HANDLER)
#define     STL_switch_fallthru(s)    (ST_CL_flags(s) & STFL_SWITCH_FALLTHRU)
#define Set_STL_switch_fallthru(s)    (ST_CL_flags(s) |= STFL_SWITCH_FALLTHRU)

/* Flags for CLASS_FUNC objects: */
#define ST_CF_flags(s)		ST_flags(CLASS_IS_FUNC(s))
#define     ST_pu_is_leaf(s)	(ST_CF_flags(s) & STFL_PU_IS_LEAF)
#define Set_ST_pu_is_leaf(s)	(ST_CF_flags(s) |= STFL_PU_IS_LEAF)
#define Reset_ST_pu_is_leaf(s)	(ST_CF_flags(s) &= (~STFL_PU_IS_LEAF))
#define     ST_is_mainPU(s)	(ST_CF_flags(s) & STFL_PU_IS_MAINPU)
#define Set_ST_is_mainPU(s)	(ST_CF_flags(s) |= STFL_PU_IS_MAINPU)
#define     ST_pu_no_se(s)	(ST_CF_flags(s) & STFL_PU_NO_SIDE_EFFECTS)
#define Set_ST_pu_no_se(s)	(ST_CF_flags(s) |= STFL_PU_NO_SIDE_EFFECTS)
#define     ST_pu_is_pure(s)	(ST_CF_flags(s) & STFL_PU_IS_PURE)
#define Set_ST_pu_is_pure(s)	(ST_CF_flags(s) |= STFL_PU_IS_PURE)
#define     ST_pu_is_inline_function(s)	(ST_CF_flags(s) & STFL_PU_IS_INLINE_FUNCTION)
#define Set_ST_pu_is_inline_function(s)	(ST_CF_flags(s) |= STFL_PU_IS_INLINE_FUNCTION)
#define     ST_pu_needs_t9(s)	(ST_CF_flags(s) & STFL_PU_NEEDS_T9)
#define Set_ST_pu_needs_t9(s)	(ST_CF_flags(s) |= STFL_PU_NEEDS_T9)
#define     ST_pu_is_blockdata(s) (ST_CF_flags(s) & STFL_PU_IS_BLOCKDATA)
#define Set_ST_pu_is_blockdata(s) (ST_CF_flags(s) |= STFL_PU_IS_BLOCKDATA)
#define     ST_pu_no_inline(s)	(ST_CF_flags(s) & STFL_PU_NO_INLINE)
#define Set_ST_pu_no_inline(s)	(ST_CF_flags(s) |= STFL_PU_NO_INLINE)
#define     ST_pu_no_delete(s)	(ST_CF_flags(s) & STFL_PU_NO_DELETE)
#define Set_ST_pu_no_delete(s)	(ST_CF_flags(s) |= STFL_PU_NO_DELETE)
#define     ST_pu_has_exc_scopes(s) (ST_CF_flags(s) & STFL_PU_HAS_EXC_SCOPES)
#define Set_ST_pu_has_exc_scopes(s) (ST_CF_flags(s) |= STFL_PU_HAS_EXC_SCOPES)
#define     ST_pu_no_throws(s)	(ST_CF_flags(s) & STFL_PU_NO_THROWS)
#define Set_ST_pu_no_throws(s)	(ST_CF_flags(s) |= STFL_PU_NO_THROWS)
#define     ST_pu_throws(s)	(ST_CF_flags(s) & STFL_PU_THROWS)
#define Set_ST_pu_throws(s)	(ST_CF_flags(s) |= STFL_PU_THROWS)
#define     ST_pu_is_nested_func(s)  (ST_CF_flags(s) & STFL_PU_IS_NESTED_FUNC)
#define Set_ST_pu_is_nested_func(s)  (ST_CF_flags(s) |= STFL_PU_IS_NESTED_FUNC)
#define       ST_pu_has_non_mangled_call(s) (ST_CF_flags(s) & STFL_PU_HAS_NON_MANGLED_CALL)
#define   Set_ST_pu_has_non_mangled_call(s) (ST_CF_flags(s) |= STFL_PU_HAS_NON_MANGLED_CALL)
#define     ST_pu_is_constructor(s)	(ST_CF_flags(s) & STFL_PU_IS_CONSTRUCTOR)
#define Set_ST_pu_is_constructor(s)	(ST_CF_flags(s) |= STFL_PU_IS_CONSTRUCTOR)
#define     ST_pu_is_destructor(s)	(ST_CF_flags(s) & STFL_PU_IS_DESTRUCTOR)
#define Set_ST_pu_is_destructor(s)	(ST_CF_flags(s) |= STFL_PU_IS_DESTRUCTOR)
#define     ST_pu_args_aliased(s)	(ST_CF_flags(s) & STFL_PU_ARGS_ALIASED)
#define Set_ST_pu_args_aliased(s)	(ST_CF_flags(s) |= STFL_PU_ARGS_ALIASED)
#define Reset_ST_pu_args_aliased(s)	(ST_CF_flags(s) &= (~STFL_PU_ARGS_ALIASED))
#define     ST_pu_is_recursive(s)       (ST_CF_flags(s) & STFL_PU_RECURSIVE)
#define Set_ST_pu_is_recursive(s)       (ST_CF_flags(s) |= STFL_PU_RECURSIVE)

#define     ST_pu_in_elf_section(s)	(ST_CF_flags(s) & STFL_PU_IN_ELF_SECTION)
#define Set_ST_pu_in_elf_section(s)	(ST_CF_flags(s) |= STFL_PU_IN_ELF_SECTION)
#define Reset_ST_pu_in_elf_section(s)	(ST_CF_flags(s) &= (~STFL_PU_IN_ELF_SECTION))

/* flags2 CLASS_VAR flags */
#define	    ST_is_reshaped(s)	(ST_CV_flags2(s) & STFL_IS_RESHAPED)
#define Set_ST_is_reshaped(s)	(ST_CV_flags2(s) |= STFL_IS_RESHAPED)
#define	    ST_emit_symbol(s)	(ST_CV_flags2(s) & STFL_EMIT_SYMBOL)
#define Set_ST_emit_symbol(s)	(ST_CV_flags2(s) |= STFL_EMIT_SYMBOL)
#define	    ST_has_nested_ref(s) (ST_CV_flags2(s) & STFL_HAS_NESTED_REF)
#define Set_ST_has_nested_ref(s) (ST_CV_flags2(s) |= STFL_HAS_NESTED_REF)
#define	    ST_init_value_zero(s) (ST_CV_flags2(s) & STFL_INIT_VALUE_ZERO)
#define Set_ST_init_value_zero(s) (ST_CV_flags2(s) |= STFL_INIT_VALUE_ZERO)
#define	    ST_force_gprel(s) (ST_CV_flags2(s) & STFL_FORCE_GPREL)
#define Set_ST_force_gprel(s) (ST_CV_flags2(s) |= STFL_FORCE_GPREL)
#define	    ST_force_not_gprel(s) (ST_CV_flags2(s) & STFL_FORCE_NOT_GPREL)
#define Set_ST_force_not_gprel(s) (ST_CV_flags2(s) |= STFL_FORCE_NOT_GPREL)
#define	    ST_is_namelist(s)	(ST_CV_flags2(s) & STFL_IS_NAMELIST)
#define Set_ST_is_namelist(s)	(ST_CV_flags2(s) |= STFL_IS_NAMELIST)
#define	    ST_is_f90_pointer(s)	(ST_CV_flags2(s) & STFL_IS_F90_POINTER)
#define Set_ST_is_f90_pointer(s)	(ST_CV_flags2(s) |= STFL_IS_F90_POINTER)
#define	    ST_is_f90_target(s)	(ST_CV_flags2(s) & STFL_IS_F90_TARGET)
#define Set_ST_is_f90_target(s)	(ST_CV_flags2(s) |= STFL_IS_F90_TARGET)
#define	    ST_declared_static(s)	(ST_CV_flags2(s) & STFL_DECLARED_STATIC)
#define Set_ST_declared_static(s)	(ST_CV_flags2(s) |= STFL_DECLARED_STATIC)
#define	    ST_is_equivalenced(s)	(ST_CV_flags2(s) & STFL_IS_EQUIVALENCED)
#define Set_ST_is_equivalenced(s)	(ST_CV_flags2(s) |= STFL_IS_EQUIVALENCED)
#define	    ST_is_this_pointer(s)	(ST_CV_flags2(s) & STFL_IS_THIS_POINTER)
#define Set_ST_is_this_pointer(s)	(ST_CV_flags2(s) |= STFL_IS_THIS_POINTER)
#define	    ST_is_auto_or_cpointer(s)	(ST_CV_flags2(s) & STFL_IS_AUTO_OR_CPOINTER)
#define Set_ST_is_auto_or_cpointer(s)	(ST_CV_flags2(s) |= STFL_IS_AUTO_OR_CPOINTER)
#define Reset_ST_is_auto_or_cpointer(s)	(ST_CV_flags2(s) &= (~STFL_IS_AUTO_OR_CPOINTER))
#define	    ST_is_non_contiguous(s)	(ST_CV_flags2(s) & STFL_IS_NON_CONTIGUOUS)
#define Set_ST_is_non_contiguous(s)	(ST_CV_flags2(s) |= STFL_IS_NON_CONTIGUOUS)
#define Reset_ST_is_non_contiguous(s)	(ST_CV_flags2(s) &= (~STFL_IS_NON_CONTIGUOUS))
#define	    ST_is_optional_argument(s)	(ST_CV_flags2(s) & STFL_IS_OPTIONAL_ARGUMENT)
#define Set_ST_is_optional_argument(s)	(ST_CV_flags2(s) |= STFL_IS_OPTIONAL_ARGUMENT)
#define Reset_ST_is_optional_argument(s)	(ST_CV_flags2(s) &= (~STFL_IS_OPTIONAL_ARGUMENT))
#define	    ST_is_thread_private(s)	(ST_CV_flags2(s) & STFL_IS_THREAD_PRIVATE)
#define Set_ST_is_thread_private(s)	(ST_CV_flags2(s) |= STFL_IS_THREAD_PRIVATE)
#define Reset_ST_is_thread_private(s)	(ST_CV_flags2(s) &= (~STFL_IS_THREAD_PRIVATE))
#define	    ST_is_fill_align(s)	(ST_CV_flags2(s) & STFL_IS_FILL_ALIGN)
#define Set_ST_is_fill_align(s)	(ST_CV_flags2(s) |= STFL_IS_FILL_ALIGN)


#define     ST_pu_needs_f90_lowering(s)	(ST_CVF_flags2(s) & STFL_PU_NEEDS_F90_LOWERING)
#define Set_ST_pu_needs_f90_lowering(s)	(ST_CVF_flags2(s) |= STFL_PU_NEEDS_F90_LOWERING)
#define	    ST_pu_has_single_return(s)	(ST_CVF_flags2(s) & STFL_PU_HAS_SINGLE_RETURN)
#define Set_ST_pu_has_single_return(s)	(ST_CVF_flags2(s) |= STFL_PU_HAS_SINGLE_RETURN)
#define Reset_ST_pu_has_single_return(s)	(ST_CVF_flags2(s) &= (~STFL_PU_HAS_SINGLE_RETURN))
#define     ST_pu_no_gp_prolog(s)	(ST_CVF_flags2(s) & STFL_PU_NO_GP_PROLOG)
#define Set_ST_pu_no_gp_prolog(s)	(ST_CVF_flags2(s) |= STFL_PU_NO_GP_PROLOG)
#define Reset_ST_pu_no_gp_prolog(s)	(ST_CVF_flags2(s) &= (~STFL_PU_NO_GP_PROLOG))
#define	    ST_pu_must_inline(s)	(ST_CVF_flags2(s) & STFL_PU_MUST_INLINE)
#define Set_ST_pu_must_inline(s)	(ST_CVF_flags2(s) |= STFL_PU_MUST_INLINE)
#define Reset_ST_pu_must_inline(s)	(ST_CVF_flags2(s) &= (~STFL_PU_MUST_INLINE))
#define	    ST_pu_calls_setjmp(s)	(ST_CVF_flags2(s) & STFL_PU_CALLS_SETJMP)
#define Set_ST_pu_calls_setjmp(s)	(ST_CVF_flags2(s) |= STFL_PU_CALLS_SETJMP)
#define Reset_ST_pu_calls_setjmp(s)	(ST_CVF_flags2(s) &= (~STFL_PU_CALLS_SETJMP))
#define	    ST_pu_calls_longjmp(s)	(ST_CVF_flags2(s) & STFL_PU_CALLS_LONGJMP)
#define Set_ST_pu_calls_longjmp(s)	(ST_CVF_flags2(s) |= STFL_PU_CALLS_LONGJMP)
#define Reset_ST_pu_calls_longjmp(s)	(ST_CVF_flags2(s) &= (~STFL_PU_CALLS_LONGJMP))
#define     ST_pu_needs_fill_align_lowering(s)	(ST_CVF_flags2(s) & STFL_PU_NEEDS_FILL_ALIGN_LOWERING)
#define Set_ST_pu_needs_fill_align_lowering(s)	(ST_CVF_flags2(s) |= STFL_PU_NEEDS_FILL_ALIGN_LOWERING)

#define     ST_gp_group(s)	((ST_CVF_flags2(s) & GPGROUP_MASK)>>GPGROUP_SHIFT)
#define Set_ST_gp_group(s,v)	(ST_CVF_flags2(s)=((ST_CVF_flags2(s)&(~GPGROUP_MASK))|(v<<GPGROUP_SHIFT)))


/* 
 * PREG_DEBUG_INFO contains the name and variable ST associated with a preg.
 * We store indexes rather than the pointer itself to make read/write
 * quicker, at the cost of making debugging/dumps slower. 
 */
typedef struct {
	INT32 name_index;	/* strtab index */
	struct wn *home;	/* home wn associated with this preg */
} PREG_DEBUG_INFO;

/* ====================================================================
 *
 * SYMTAB: Symbol Table Descriptor
 *
 * A program will, in general, have a global and a local symbol table
 * visible at any time.  Programs with nested subroutines may have 
 * multiple (nested) symbol tables live at once.  The SYMTAB structure 
 * contains the essential information about the various symbol tables 
 * which have been constructed.
 *
 * The full set of symbol table descriptors will be kept in a linked
 * list starting at Global_Symtab, which must be the unique global
 * symbol table.  The Current_Symtab variable points at the SYMTAB
 * for the current PU.
 *
 * SYMTAB_parent() points to the enclosing SYMTAB (usually the Global_Symtab).
 * SYMTAB_stack_model() gives the stack model of the PU (see memmodel.h)
 * SYMTAB_symbols() is the list of ST symbols.
 * SYMTAB_labels() is the list of ST labels.
 * SYMTAB_types() is the list of TY types.
 * SYMTAB_last_preg() is the number of the last preg in the PU.
 * SYMTAB_last_label() is the number of the last label in the PU.
 * ====================================================================
 */

struct symtab {
  struct symtab *parent;	/* Enclosing symbol table */
  mUINT32	id;		/* Unique id */
  mUINT8	level;		/* Display level (0=>global; max 7 bits) */
  mUINT8	stack_model;	/* Stack model */
  mUINT16	flags;		/* Symbol table flags */
  ST		*symbols;	/* List of symbols */
  ST		*last_symbol;	/* Point to last symbol for fast appending */
  ST		*labels;	/* Labels (FE: local only) */
  TY		*types;		/* List of types */
  TY		*last_type;	/* Point to last type for fast appending */
  ST		*slink_sym;	/* Static link symbol */
  ST		*gp_sym;	/* Global pointer (.sdata and/or .got) */
  ST		*ra_sym;	/* Return address symbol */
  mINT64	actual_size;	/* Actual parameter area size */
  PREG_DEBUG_INFO *preg_dinfo;	/* array of preg debug_info, indexed by offset */
  mUINT32	last_st_id;	/* Last ST index in PU (only 24 bits used) */
  mUINT32	last_symtab_id;	/* Last SYMTAB_id in file (global only) */
  mUINT8	last_scope_id;	/* Last scope id */
  mUINT8	gp_group;	/* default gp_group of file (global only) */
  mUINT16	last_preg;	/* Number of last pseudo-reg in PU */
  union {
    /* last_label applies to local symtabs, last_type_id to the global symtab */
    mUINT16	last_label;	/* Number of last label in PU (local) */
    mUINT16	last_type_id;	/* Number of last type_id in file (global) */
  } ulast;
  ST		**st_map;	/* Map from indices to STs */
  SYMTAB	**symtab_map;	/* Map from IDs (global) or levels (local) */
  TY		**ty_map;	/* Map from IDs to TYs (global only) */
  struct inito *initdata;	/* initialized data */
};

/* Access macros: */
#define SYMTAB_parent(s)	((s)->parent)
#define SYMTAB_id(s)		((s)->id)
#define SYMTAB_flags(s)		((s)->flags)
#define SYMTAB_level(s)		((s)->level)
#define SYMTAB_stack_model(s)	((s)->stack_model)
#define SYMTAB_last_scope_id(s)	((s)->last_scope_id)
#define SYMTAB_gp_group(s)	((s)->gp_group)
#define SYMTAB_symbols(s)	((s)->symbols)
#define SYMTAB_last_symbol(s)	((s)->last_symbol)
#define SYMTAB_labels(s)	((s)->labels)
#define SYMTAB_types(s)		((s)->types)
#define SYMTAB_last_type(s)	((s)->last_type)
#define SYMTAB_slink_sym(s)	((s)->slink_sym)
#define SYMTAB_gp_sym(s)	((s)->gp_sym)
#define SYMTAB_ra_sym(s)	((s)->ra_sym)
#define SYMTAB_actual_size(s)	((s)->actual_size)
#define SYMTAB_preg_dinfo(s)	((s)->preg_dinfo)
/* define that 0 offset has size of preg_dinfo array */
#define SYMTAB_preg_dinfo_size(s)	SYMTAB_preg_dinfo(s)[0].name_index
#define SYMTAB_last_preg(s)	((s)->last_preg)
#define SYMTAB_last_st_id(s)	((s)->last_st_id)
#define SYMTAB_last_symtab_id(s) ((s)->last_symtab_id)
#define SYMTAB_last_label(s)	((s)->ulast.last_label)
#define SYMTAB_last_type_id(s)	((s)->ulast.last_type_id)
#define SYMTAB_st_map(s)	((s)->st_map)
#define SYMTAB_symtab_map(s)	((s)->symtab_map)
#define SYMTAB_level_map(s)	((s)->symtab_map)
#define SYMTAB_ty_map(s)	((s)->ty_map)
#define SYMTAB_initdata(s)	((s)->initdata)

/* Flags access: */
#define SYMTAB_UPLEVEL	0x0001	/* Other symtab nested in this one */
#define SYMTAB_MP_NEEDS_LNO  0x0002  /* PU needs LNO processing */
#define SYMTAB_ALLOCA	0x0004	/* Symtab has alloca in it */
#define SYMTAB_IPA	0x0008  /* IPA has been run on program, global symtab*/
#define SYMTAB_HASMP	0x0010	/* Symtab has MP region/do within it */
#define SYMTAB_MP	0x0020	/* Symtab is an MP region/do */
#define SYMTAB_HAS_NAMELIST	0x0040	/* PU has namelist */
#define SYMTAB_ADDR_TAKEN 0x0080 /* addr_saved and addr_passed bits should
				    be handled separately, global symtab */
#define SYMTAB_HAS_RGN	0x0100  /* PU has regions (of any type) in it */
#define SYMTAB_HAS_INL	0x0200  /* PU has inlined code in it */
#define SYMTAB_HAS_VERY_HIGH_WHIRL 0x0400 /* PU has very high whirl in it */
#define SYMTAB_HAS_ALTENTRY	0x0800	/* PU has alternate entries */

/* source language flags: */
#define SYMTAB_SRC_LANG	0xF000	/* Symtab source language mask */
#define SYMTAB_C_LANG	0x1000	/* Symtab source language is C */
#define SYMTAB_CXX_LANG	0x2000	/* Symtab source language is C++ */
#define SYMTAB_F77_LANG	0x4000	/* Symtab source language is F77 */
#define SYMTAB_F90_LANG	0x8000	/* Symtab source language is F90 */

#define       SYMTAB_uplevel(s)	(SYMTAB_flags(s) & SYMTAB_UPLEVEL)
#define   Set_SYMTAB_uplevel(s)	(SYMTAB_flags(s) |= SYMTAB_UPLEVEL)
#define Reset_SYMTAB_uplevel(s)	(SYMTAB_flags(s) &= ~SYMTAB_UPLEVEL)
#define       SYMTAB_has_alloca(s)	(SYMTAB_flags(s) & SYMTAB_ALLOCA)
#define   Set_SYMTAB_has_alloca(s)	(SYMTAB_flags(s) |= SYMTAB_ALLOCA)
#define Reset_SYMTAB_has_alloca(s)	(SYMTAB_flags(s) &= ~SYMTAB_ALLOCA)
#define       SYMTAB_IPA_on(s)	(SYMTAB_flags(s) & SYMTAB_IPA)
#define   Set_SYMTAB_IPA_on(s)	(SYMTAB_flags(s) |= SYMTAB_IPA)
#define Reset_SYMTAB_IPA_on(s)	(SYMTAB_flags(s) &= ~SYMTAB_IPA)
#define       SYMTAB_has_mp(s)	(SYMTAB_flags(s) & SYMTAB_HASMP)
#define   Set_SYMTAB_has_mp(s)	(SYMTAB_flags(s) |= SYMTAB_HASMP)
#define Reset_SYMTAB_has_mp(s)	(SYMTAB_flags(s) &= ~SYMTAB_HASMP)
#define       SYMTAB_mp(s)	(SYMTAB_flags(s) & SYMTAB_MP)
#define   Set_SYMTAB_mp(s)	(SYMTAB_flags(s) |= SYMTAB_MP)
#define Reset_SYMTAB_mp(s)	(SYMTAB_flags(s) &= ~SYMTAB_MP)
#define	      SYMTAB_has_nested(s)	(SYMTAB_uplevel(s) || SYMTAB_has_mp(s))
#define       SYMTAB_has_namelist(s)	(SYMTAB_flags(s) & SYMTAB_HAS_NAMELIST)
#define   Set_SYMTAB_has_namelist(s)	(SYMTAB_flags(s) |= SYMTAB_HAS_NAMELIST)
#define Reset_SYMTAB_has_namelist(s)	(SYMTAB_flags(s) &= ~SYMTAB_HAS_NAMELIST
#define       SYMTAB_addr_taken(s)	(SYMTAB_flags(s) & SYMTAB_ADDR_TAKEN)
#define   Set_SYMTAB_addr_taken(s)	(SYMTAB_flags(s) |= SYMTAB_ADDR_TAKEN)
#define Reset_SYMTAB_addr_taken(s)	(SYMTAB_flags(s) &= ~SYMTAB_ADDR_TAKEN)
#define       SYMTAB_has_rgn(s)	(SYMTAB_flags(s) & SYMTAB_HAS_RGN)
#define   Set_SYMTAB_has_rgn(s)	(SYMTAB_flags(s) |= SYMTAB_HAS_RGN)
#define Reset_SYMTAB_has_rgn(s)	(SYMTAB_flags(s) &= ~SYMTAB_HAS_RGN)
#define       SYMTAB_has_inlines(s)	(SYMTAB_flags(s) & SYMTAB_HAS_INL)
#define   Set_SYMTAB_has_inlines(s)	(SYMTAB_flags(s) |= SYMTAB_HAS_INL)
#define Reset_SYMTAB_has_inlines(s)	(SYMTAB_flags(s) &= ~SYMTAB_HAS_INL)
#define       SYMTAB_has_very_high_whirl(s)	\
			(SYMTAB_flags(s) & SYMTAB_HAS_VERY_HIGH_WHIRL)
#define   Set_SYMTAB_has_very_high_whirl(s)	\
			(SYMTAB_flags(s) |= SYMTAB_HAS_VERY_HIGH_WHIRL)
#define Reset_SYMTAB_has_very_high_whirl(s)	\
			(SYMTAB_flags(s) &= ~SYMTAB_HAS_VERY_HIGH_WHIRL)
#define       SYMTAB_has_altentry(s)	(SYMTAB_flags(s) & SYMTAB_HAS_ALTENTRY)
#define   Set_SYMTAB_has_altentry(s)	(SYMTAB_flags(s) |= SYMTAB_HAS_ALTENTRY)
#define Reset_SYMTAB_has_altentry(s)	(SYMTAB_flags(s) &= ~SYMTAB_HAS_ALTENTRY)
#define     SYMTAB_mp_needs_lno(s)	(SYMTAB_flags(s)&SYMTAB_MP_NEEDS_LNO)
#define Set_SYMTAB_mp_needs_lno(s)	(SYMTAB_flags(s)|=SYMTAB_MP_NEEDS_LNO)
#define Reset_SYMTAB_mp_needs_lno(s)	(SYMTAB_flags(s)&=~SYMTAB_MP_NEEDS_LNO)

#define       SYMTAB_src_lang(s)	(SYMTAB_flags(s) & SYMTAB_SRC_LANG)
#define   Set_SYMTAB_src_lang(s,v)	(SYMTAB_flags(s) |= v)

/* Special symbol tables: */
extern SYMTAB_IDX Global_Symtab;	/* The global symbol table */
extern SYMTAB_IDX Current_Symtab;	/* The last symbol table defined */
extern SYMTAB_IDX *Display_Symtab;	/* Static display of symbol tables */
#define Display(n)	Display_Symtab[n]

/* References to SYMTABs, STs, and TYs are written to the binary WHIRL
 * files using ID numbers.  The following macros convert from ID
 * numbers to pointers using the mappings stored in the symbol tables.
 * Note: ir_bread.c defines functions to replace these macros when
 * debugging; they must be kept in sync with these macros. */

#define Convert_Symtab_Id(id) \
	SYMTAB_symtab_map(Global_Symtab)[(id)]
#define Convert_Symtab_Level(id, current) \
	SYMTAB_level_map(current)[(id)]
#define Convert_TY_Id(id) \
	SYMTAB_ty_map(Global_Symtab)[(id)]
#define Convert_ST_Id(id) \
	SYMTAB_st_map(Convert_Symtab_Level((id) >> 24, Current_Symtab)) \
	[(id) & 0xffffff]

/* symbol is in global symbol table? */
#define Is_Global_Symbol(s)	(ST_symtab_id(s) == SYMTAB_id(Global_Symtab))
/* note that Local does not mean local uplevel */
#define Is_Local_Symbol(s)	(ST_symtab_id(s) == SYMTAB_id(Current_Symtab))


/* ====================================================================
 *
 * Symbol table data
 *
 * ====================================================================
 */
 
/* Map MTYPES to TY records: */
/* We use a malloc'ed pointer rather than a fixed array 
 * so IPA can easily have multiple tables available. */
extern TY_IDX *MTYPE_To_TY_array;

#ifdef STAB_DOT_C
  char *BTT_Message = "Bad MTYPE index %1d to TY array";
#endif

#ifdef Is_True_On
  extern char *BTT_Message;
# define MTYPE_To_TY(k) (MTYPE_To_TY_array[Is_True((k)<=MTYPE_LAST, \
						(BTT_Message,(k))),(k)])
#else /* ! Is_True_On */

# define MTYPE_To_TY(k) (MTYPE_To_TY_array[k])

#endif /* Is_True_On */
#define Be_Type_Tbl(k)	MTYPE_To_TY(k)

/* These must be passed to the back end to handle function results: */
extern TY_IDX Quad_Type;
extern TY *Complex_Type, *Double_Complex_Type, *Quad_Complex_Type;
extern TY *Complex_Type_Split, *Complex_Type_Inverted;

/* Keep track of the PU currently being processed: */
extern ST_IDX Current_PU;
 
#define GLOBAL_SCOPE_ID	0	/* scope_id of globals */

/* --------------------------------------------------------------------
 * The following are iterators for the various kinds of symbol table entities.
 * FOR_ALL_*(stab,x)	iterates over the given symtab.
 * FOR_ALL_GLOBAL_*(x)	iterates over the Global_Symtab.
 * FOR_ALL_LOCAL_*(x)	iterates over the Current_Symtab.
 * FOR_ALL_VISIBLE_*(x)	iterates over the Current_Symtab and its parents.
 * --------------------------------------------------------------------
 */

#ifndef FOR_ALL_TYPES
/* t is a TY pointer, and may not be changed during the iteration */
#define FOR_ALL_TYPES(stab,t)	\
	for ( t = SYMTAB_types(stab); t != NULL; t = TY_next(t) )
#define FOR_ALL_GLOBAL_TYPES(t)		FOR_ALL_TYPES(Global_Symtab,t)
#define FOR_ALL_LOCAL_TYPES(t)		FOR_ALL_TYPES(Current_Symtab,t)
/* FOR_ALL_VISIBLE_TYPES walks all visible types in current and parent symtabs */
#define FOR_ALL_VISIBLE_TYPES(t)	\
	for ( t = Get_Next_Type(NULL); t != NULL; t = Get_Next_Type(t) )
extern TY_IDX Get_Next_Type ( TY_IDX ty );	/* internal routine for FOR_ALL_TYPES */
#endif /* !defined FOR_ALL_TYPES */

#ifndef FOR_ALL_SYMBOLS
/* s is a ST pointer, and may not be changed during the iteration */
#define FOR_ALL_SYMBOLS(stab,s) \
        for (s = SYMTAB_symbols(stab); s != NULL; s = ST_next(s))
#define FOR_ALL_GLOBAL_SYMBOLS(s)	FOR_ALL_SYMBOLS(Global_Symtab,s)
#define FOR_ALL_LOCAL_SYMBOLS(s)	FOR_ALL_SYMBOLS(Current_Symtab,s)
/* FOR_ALL_VISIBLE_SYMBOLS walks all visible symbols in current and parent symtabs */
#define FOR_ALL_VISIBLE_SYMBOLS(s)	\
	for ( s = Get_Next_Symbol(NULL); s != NULL; s = Get_Next_Symbol(s) )
extern ST_IDX Get_Next_Symbol ( ST_IDX st );	/* internal routine for FOR_ALL_SYMBOLS */
#endif /* !defined FOR_ALL_SYMBOLS */

#ifndef FOR_ALL_LABELS
/* l is a ST pointer, and may not be changed during the iteration */
#define FOR_ALL_LABELS(stab,l)	\
        for (l = SYMTAB_labels(stab); l != NULL; l = ST_next(l))
#define FOR_ALL_LOCAL_LABELS(l)		FOR_ALL_LABELS(Current_Symtab,l)
#endif /* !defined FOR_ALL_LABELS */

#define FOR_ALL_SYMTABS_IN_LIST(init,stab)	\
        for (stab = init; stab != NULL; stab = SYMTAB_next(stab))
#define FOR_ALL_SYMTABS(stab)	\
	for (stab = Global_Symtab; stab != NULL; stab = ((stab == Global_Symtab) ? SYMTAB_child(stab) : SYMTAB_next(stab)) )

/* The TY record which represents the type of comparisons (OP_JLES
 * etc.).  This has MTYPE determined in config_targ.h.
 */
extern TY_IDX Comparison_Result_Type;

extern TY	*Void_Type;
extern TY	*FE_int_Type;	/* C FE's "int" type or NULL */
extern TY	*FE_double_Type;/* C FE's "double" type or NULL */
extern TY	*Spill_Int_Type, *Spill_Float_Type;
extern TY	*LL_Struct_Type,	/* long long structure */
		*LLV_Struct_Type,	/* volatile long long */
		*ULL_Struct_Type,	/* unsigned long long */
		*ULLV_Struct_Type,	/* volatile unsigned long long*/
		*LD_Struct_Type,	/* long double */
		*LDV_Struct_Type;	/* volatile long double */
extern INT64	Actual_Arg_Size, Formal_Size,
		Stack_Size, Frame_Len;

/*
 * PREGs:
 * Temporaries are allocated as pseudo-registers (PREGs).
 * In order to save space, we don't use a separate ST for PREGS,
 * but instead we have a unique offset (PREG_NUM) and then there
 * are common PREG STs for each base type.  The PREG_NUM becomes
 * the offset in WHIRL ld/stid.  The PREG_NUMs are unique across a PU.
 */
/*
 * predefined PREG symbols, one for each mtype
 * (actually only have pregs for the register-size mtypes and simulated mtypes;
 * in particular, the I1/I2/U1/U2 mtypes point to the 4-byte PREG.
 */
extern ST** MTYPE_To_PREG_array;
#define MTYPE_To_PREG(t)	MTYPE_To_PREG_array[t]
#define Int32_Preg	MTYPE_To_PREG (MTYPE_I4)
#define Int64_Preg	MTYPE_To_PREG (MTYPE_I8)
#define Float32_Preg	MTYPE_To_PREG (MTYPE_F4)
#define Float64_Preg	MTYPE_To_PREG (MTYPE_F8)
/* preferred preg symbols for physical registers 
 * (point to one of above pregs, depending on ABI). */
extern ST	*Int_Preg, *Float_Preg;	/* for pseudo-registers */

/* some definitions for the dedicated hardware pregs: */
#define Int_Preg_Min_Offset		1
#define Int_Preg_Max_Offset		31
#define Float_Preg_Min_Offset		32
#define Float_Preg_Max_Offset		63
#define Fcc_Preg_Min_Offset		64
#define Fcc_Preg_Max_Offset		71
#define Last_Dedicated_Preg_Offset	Fcc_Preg_Max_Offset
#define Preg_Offset_Is_Int(n) \
	((n) >= Int_Preg_Min_Offset && (n) <= Int_Preg_Max_Offset)
#define Preg_Offset_Is_Float(n) \
	((n) >= Float_Preg_Min_Offset && (n) <= Float_Preg_Max_Offset)
#define Preg_Offset_Is_Fcc(n) \
	((n) >= Fcc_Preg_Min_Offset && (n) <= Fcc_Preg_Max_Offset)
#define Preg_Is_Dedicated(n) (n <= Last_Dedicated_Preg_Offset)

/*
 * Create_Preg creates a new preg (new PREG_NUM).  
 * We associate a name and home ST with the preg for debugging purposes.
 * Don't bother with names for physical registers.
 * For pregs of simulated types, that will later get lowered into
 * multiple pregs, reserve space for the multiple pregs.
 */
/* create a preg of the given type with the associated name */
extern INT32 Preg_Increment(TYPE_ID mtype);
extern PREG_NUM Create_Preg (TYPE_ID mtype, char *name, struct wn *home);
extern void Reset_Last_Preg (PREG_NUM i);	/* cut-back the preg array */
extern char* Preg_Name (PREG_NUM i);		/* get name of preg */
extern void Set_Preg_Name (PREG_NUM i, char *name);  /* set name of preg */
extern struct wn* Preg_Home (PREG_NUM i);	/* get home of preg */


/* ====================================================================
 *
 * Utility function prototypes.
 *
 * ====================================================================
 */

/* --------------------------------------------------------------------
 * Basic symbol table facilities.
 * --------------------------------------------------------------------
 */

/* Initialize the symbol table for a new compilation.
 * If stab == NULL, then create a global symtab, 
 * else use the parameter as the global symtab. */
extern void Stab_Initialize ( SYMTAB *global);
/* Create a new symtab for a new PU, underneath its parent
 * (put under Global_Symtab for top-level PUs). */
extern void Stab_Begin_PU ( SYMTAB *parent );
/* Setup the symbol table to use the "current" symtab. */
extern void Stab_Set_Current_PU (SYMTAB *current);

extern void Stab_Begin_File(void);	/* when a new file begins */

/* Construct a new scope descriptor: */
extern void New_Scope ( BOOL new_pu );

/* Construct ASCII names for tracing: */
extern char *Aux_Class_Name ( INT16 );
extern char *Class_Name ( INT16 );
extern char *Sclass_Name ( INT32 );
extern char *Kind_Name ( INT16 );

/* Given a formal parameter, find the type to which it is promoted for
 * passing.  For most formals, this is simply ST_type(st).  However, if
 * ST_promote_parm(st) is set, we must check the promotion rules for C:
 */
extern TY_IDX Promoted_Parm_Type ( ST_IDX formal_parm );

/* Determine if the ST represents a constant or read-only memory location. */
extern BOOL ST_is_constant (ST_IDX st);

/* Determine if the ST represents a constant. */
extern BOOL ST_is_const_initialized( const ST *st );

/* Determine if the ST represents a constant scalar variable that has
 * a known initialized value.  If true, stores the value in 'tc'.
 */
extern BOOL ST_is_const_initialized_scalar( const ST *st, struct tcon *tc );

/* Determine if the ST represents a constant scalar variable that has
 * a known initialized value.  If true, returns its initv.
 */
extern struct initv *ST_is_const_and_has_initv( const ST *st );

/* --------------------------------------------------------------------
 * Type and symbol creation utilities.
 *
 * The is_global parameter in the following routines specifies whether to 
 * use a file-scope memory pool or a pu-scope memory pool.
 * --------------------------------------------------------------------
 */
/* Allocate symbol table space: */
extern void *Symtab_Alloc ( size_t bytes, BOOL is_global );

extern ST_IDX New_ST ( BOOL is_global );	/* allocate a new ST record */
extern SBLK *New_SBLK (BOOL is_global);	/* allocate a new SBLK record */

/* Generate new type structures: */
extern TY_IDX New_TY ( BOOL is_global );
/* The following routines take a "num" parameter which says how many
 * records to allocate in a linked list.  These routines will set the
 * next field for the records in the linked list. */
extern FLD *New_FLD (INT nfields, BOOL is_global );
extern MBR *New_MBR (INT nfields, BOOL is_global );
extern ARI *New_ARI (INT ndims, BOOL is_global);
extern FTI *New_FTI (INT nparms, BOOL is_global);
extern ENUM_CONST *New_ENUM_CONST (INT num, BOOL is_global);
extern TYLIST *New_TYLIST (INT num, BOOL is_global);

/* The copy routines will make complete copies of the record and all
 * other records that it points to, with the following exceptions:
 * If the TY is a predefined type (and always_copy=FALSE), then it 
 * will re-use the ty.
 * If the old and new ST are in the same file, then we reuse things
 * like the name string. */
extern ST_IDX Copy_ST (ST_IDX, BOOL same_file);
extern TY_IDX Copy_TY (TY_IDX, BOOL always_copy);

/* Add st to the stab chain stch. */
extern STCH *Append_To_STCH ( STCH *stch, ST *st, BOOL is_global );

extern TY_IDX Make_Pointer_Type ( TY_IDX pointee, BOOL is_global );
extern TY_IDX Make_Global_Pointer_As_Array_Type ( TY_IDX ty);

extern TY_IDX Make_Function_Type ( TY_IDX rtype, BOOL is_global );
extern TY_IDX Make_Array_Type (TYPE_ID element, INT32 ndim, INT64 len, BOOL is_global);


/* find (if one exists) or create a TY that matches the given one in
 * all respects, except the new one will be "volatile."
 * NOTE: "is_global" simply refers to where we will put the type when
 * creating a new one.  If it may be used simply for the current PU,
 * should say "is_global=FALSE" which is more efficient memory-wise.
 */
extern TY *Make_Volatile_Type ( TY *base_type, BOOL is_global );
extern TY *Make_Align_Type ( TY *rtype, INT32 align, BOOL is_global );

/* Routines to create a new temporary: */
extern struct st *Gen_Read_Only_Symbol ( TY * ty, char *rootname );
extern struct st *Gen_Temp_Symbol ( TY *ty, char *rootname );
extern struct st *Gen_Intrinsic_Function( TY *, char *);

/* Generate a new "user" label (internal if name == NULL): */
extern ST *Gen_Label ( char *name );
/* Generate a label given a number */
extern ST *Gen_Number_Label ( INT32 num );
extern ST *Gen_Local_Number_Label ( INT32 num );

extern ST *Gen_GP_Sym (void);

/* Get the base symbol and the offset from a ST */
extern void  Base_Symbol_And_Offset (
  ST *st,                       /* Symbol to analyze */
  ST **base_symbol,             /* Result: root base of st */
  INT64 *offset_from_base       /* Result: offset from primary base */
);

/* fill the display level in a SYMTAB after creation */
extern void Fill_Symtab(SYMTAB *stab, INT16 level);

/* --------------------------------------------------------------------
 * Symbol table listing and tracing utilities.
 * --------------------------------------------------------------------
 */

extern void Enter_Label_In_Symtab ( ST *, SYMTAB *, INT32 );
extern void Enter_ST ( ST_IDX);
extern void Enter_ST_In_Symtab ( ST_IDX, SYMTAB *);
extern void Enter_TY ( TY_IDX);
extern void Enter_TY_In_Symtab ( TY_IDX, SYMTAB *);
extern void Print_ST ( FILE *f, ST_IDX st, BOOL verbose );
#pragma mips_frequency_hint NEVER Print_ST
extern void Print_TY ( FILE *f, const TY *ty, BOOL verbose );
#pragma mips_frequency_hint NEVER Print_TY
extern void Print_Symbol_Table ( FILE *f, SYMTAB *stab, BOOL verbose );
#pragma mips_frequency_hint NEVER Print_Symbol_Table
extern void Trace_SYMTAB ( FILE *f, SYMTAB *stab, BOOL verbose );
#pragma mips_frequency_hint NEVER Trace_SYMTAB
extern void Print_Symtab_Stats ( FILE *f);
#pragma mips_frequency_hint NEVER Print_Symtab_Stats

/* for debugging:  doesn't use trace file */
extern void dump_st (ST_IDX);
extern void dump_ty (TY_IDX);
extern void dump_symtab (SYMTAB *);
extern void dump_constants (void);

extern SYMTAB * Get_Symtab_At_Id (INT32 id);

/* get unique id of ST, return kind as 'S|L|C' for symbol/label/const */
extern char Get_ST_Id (ST_IDX st, INT *level, INT *index);
/* Given a symtab level and the symbol index, return the ST */
extern ST_IDX Get_ST_At_Id (INT32 level, INT32 index, char kind);
extern ST_IDX Get_Symbol_At_Id (INT32 level, INT32 index);
extern ST * Get_Label_At_Id (INT32 level, INT32 index);
extern TY_IDX Get_TY_At_Id (INT32 id);

extern BOOL ST_is_private_local(const ST *st);

#ifdef __cplusplus
}

class SYMBOL_TABLE {
public:
  ST &operator[](ST *idx) {
    return *idx;
  }
};

class TYPE_TABLE {
 public:
  TY *operator[](TY *idx) {
    return idx;
  }
};

class FIELD_TABLE {
 public:
  FLD *operator[](FLD *idx) {
    return idx;
  }
};

class ARB_TABLE {
 public:
  ARB *operator[](ARB *idx) {
    return idx;
  }
};

extern "C" SYMBOL_TABLE St_Table;
extern "C" TYPE_TABLE   Ty_Table;
extern "C" FIELD_TABLE  Fld_Table;
extern "C" ARB_TABLE    Arb_Table;
#endif

/* The following are macros to make the new symtab code compileable
 * as old symtab code too (avoids unnecessary use of #ifdef NEW_SYMTAB)
 */
#define PU_has_mp(p)		SYMTAB_has_mp(p)
#define PU_has_altentry(p)	SYMTAB_has_altentry(p)
#define PU_has_nested(p)	SYMTAB_has_nested(p)
#define PU_has_alloca(p)	SYMTAB_has_alloca(p)
#define PU_uplevel(p)		SYMTAB_uplevel(p)
#define PU_has_region(p)	SYMTAB_has_rgn(p)
#define Set_PU_has_region(p)	Set_SYMTAB_has_rgn(p)
#define PU_has_namelist(p)	SYMTAB_has_namelist(p)
#define PU_IPA_on(p)		SYMTAB_IPA_on(p)
#define PU_has_return_address(p) SYMTAB_ra_sym(p)
#define Get_Current_PU()	Current_Symtab
#define Get_Current_PU_ST()	Current_PU
#define PU_lexical_level(p)	ST_scope_id(p)
#define Clear_ST_is_not_used(s)	Reset_ST_is_not_used(s)
#define LABEL_name(s)		ST_name(s)
#define FILE_INFO_has_inlines(s)	SYMTAB_has_inlines(s)
#define PU_has_inlines(s)		SYMTAB_has_inlines(s)
#define LABEL_begin_eh_range(s)	STL_begin_eh_range(s)
#define Set_LABEL_begin_eh_range(s)	Set_STL_begin_eh_range(s)
#define LABEL_end_eh_range(s)	STL_end_eh_range(s)
#define Set_LABEL_end_eh_range(s)	Set_STL_end_eh_range(s)
#define ST_pu_type(s)		ST_type(s)

#define PU_C_LANG		SYMTAB_C_LANG
#define PU_CXX_LANG		SYMTAB_CXX_LANG
#define PU_F77_LANG		SYMTAB_F77_LANG
#define PU_F90_LANG		SYMTAB_F90_LANG
#define PU_src_lang(p)		SYMTAB_src_lang(p)
#define FOREACH_SYMBOL(t,s,i)	FOR_ALL_SYMBOLS(t,s)
#define FOREACH_LABEL(t,s,i)	FOR_ALL_LABELS(t,s)
#define FOREACH_TYPE(t,s,i)	FOR_ALL_TYPES(t,s)
#define CURRENT_SYMTAB		Current_Symtab
#define GLOBAL_SYMTAB		Global_Symtab
#define File_info		Global_Symtab

#endif /* _NEW_SYMTAB */

#endif /* stab_INCLUDED */
