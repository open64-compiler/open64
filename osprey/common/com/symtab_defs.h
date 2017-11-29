/*
 * Copyright (C) 2009-2010 Advanced Micro Devices, Inc.  All Rights Reserved.
 */

/*
 * Copyright (C) 2007. QLogic Corporation. All Rights Reserved.
 */

/*
 * Copyright 2003, 2004, 2005, 2006 PathScale, Inc.  All Rights Reserved.
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


/* -*-Mode: c++;-*- (Tell emacs to use c++ mode) */

#ifndef symtab_defs_INCLUDED
#define symtab_defs_INCLUDED

// Note:
//
// The comments in this file are intentionally brief.  For detail description,
// please refer to the "WHIRL Symbol Table Specification", which is the only
// document actively maintained.
//
// KEY: Any change in this file (and friends) about change in struct size
// or ordering of struct fields may need to be reflected elsewhere in the
// compiler, at least one known place of which is ipa/common/ipc_ty_hash.cxx
// and friends.
// Last notable change: changing STR_IDX from UINT32 to UINT64.


enum ST_CLASS
{
    CLASS_UNK	= 0,
    CLASS_VAR	= 1,			// data variable
    CLASS_FUNC	= 2,			// addrress of a function.
    CLASS_CONST	= 3,			// constant value
    CLASS_PREG	= 4,			// pseudo register
    CLASS_BLOCK	= 5,			// base to a block of data
    CLASS_NAME  = 6,			// just hold an ST name
    CLASS_COUNT = 7			// total number of classes
}; // ST_CLASS


enum ST_SCLASS
{
    // the values should not overlap with those in ST_CLASS for error checking
    SCLASS_UNKNOWN	= 0,
    SCLASS_AUTO		= 1,		// stack variable
    SCLASS_FORMAL	= 2,		// formal parameter
    SCLASS_FORMAL_REF	= 3,		// reference parameter
    SCLASS_PSTATIC	= 4,		// PU scope static data
    SCLASS_FSTATIC	= 5,		// file scope static data
    SCLASS_COMMON	= 6,		// common block (linker allocated)
    SCLASS_EXTERN	= 7,		// unallocated external data or text
    SCLASS_UGLOBAL	= 8,		// uninitialized global data
    SCLASS_DGLOBAL	= 9,		// initialized global data
    SCLASS_TEXT		= 10,		// executable code
    SCLASS_REG		= 11,		// register variable
    SCLASS_CPLINIT	= 12,		// cplinit
    SCLASS_EH_REGION	= 13,		// eh region
    SCLASS_EH_REGION_SUPP = 14,		// eh region supp
    SCLASS_DISTR_ARRAY	= 15,		// distributed array
    SCLASS_COMMENT	= 16,		// comment section
    SCLASS_THREAD_PRIVATE_FUNCS	= 17, // thread-private constr/destr funcs
    SCLASS_COUNT	= 18		// total number of classes

}; // ST_SCLASS


enum ST_EXPORT
{
    EXPORT_LOCAL	= 0,		// Not exported, e.g. C static
    EXPORT_LOCAL_INTERNAL = 1,		// Statics that do not have address
					// passed outside of a.out/DSO. 
    EXPORT_INTERNAL	= 2,		// Exported, only visible and used
					// within the containing
					// DSO/executable, i.e. not even
					// passed outside using a pointer.  
    EXPORT_HIDDEN	= 3,		// Exported, but name is hidden
					// within the containing
					// DSO/executable.  However, the
					// address may be exported from the
					// DSO via a pointer.  
    EXPORT_PROTECTED	= 4,		// Exported from DSO, but
					// non-preemptible. 
    EXPORT_PREEMPTIBLE	= 5,		// Exported and preemptible.
    EXPORT_OPTIONAL	= 6,		// STO_OPTIONAL case in "sys/elf.h"
    EXPORT_COUNT	= 7		// Must be last for consistency
					// checking 
}; // ST_EXPORT

#ifdef TARG_NVISA
enum ST_MEMORY
{
    MEMORY_UNKNOWN = 0,
    MEMORY_GLOBAL = 1,
    MEMORY_LOCAL = 2,
    MEMORY_SHARED = 3,
    MEMORY_CONSTANT = 4,
    MEMORY_TEXTURE = 5,
    MEMORY_PARAM = 6,
    MEMORY_COUNT = 7
}; // ST_MEMORY
#else
enum ST_TLS_MODEL {
    TLS_NONE,
    TLS_EMULATED, /* for emulated tls */
    TLS_REAL,
    TLS_GLOBAL_DYNAMIC = TLS_REAL,
    TLS_LOCAL_DYNAMIC,
    TLS_INITIAL_EXEC,
    TLS_LOCAL_EXEC,
}; // TLS_MODEL
#endif /* TARG_NVISA */

enum ST_FLAGS
{
    ST_IS_WEAK_SYMBOL	= 0x00000001,	// Weak external name
    ST_IS_SPLIT_COMMON	= 0x00000002,	// part of a splitted common
    ST_IS_NOT_USED	= 0x00000004,	// Symbol is not referenced
    ST_IS_INITIALIZED	= 0x00000008,	// Symbol is initialized
    ST_IS_RETURN_VAR	= 0x00000010,	// CLASS_VAR: Return value
    ST_IS_VALUE_PARM	= 0x00000020,	// CLASS_VAR: Value parm
    ST_PROMOTE_PARM	= 0x00000040,	// CLASS_VAR: Promote C formal
    ST_KEEP_NAME_W2F	= 0x00000080,	// CLASS_VAR: don't mangle name
    ST_IS_DATAPOOL	= 0x00000100,	// CLASS_VAR: represents datapool
    ST_IS_RESHAPED	= 0x00000200,	// lno may reshape the ST
    ST_EMIT_SYMBOL	= 0x00000400,	// emit the empty dummy symbol
    ST_HAS_NESTED_REF	= 0x00000800,	// has ref in nested pu
    ST_INIT_VALUE_ZERO	= 0x00001000,	// has initial value of zero
    ST_GPREL		= 0x00002000,	// force object to be gp-rel
    ST_NOT_GPREL	= 0x00004000,	// force object to not be gp-rel
    ST_IS_NAMELIST	= 0x00008000,	// namelist table
    ST_IS_F90_TARGET	= 0x00010000,	// F90 target
    ST_DECLARED_STATIC	= 0x00020000,	// VMS formals declared static
    ST_IS_EQUIVALENCED	= 0x00040000,	// is part of an equivalence
    ST_IS_FILL_ALIGN	= 0x00080000,	// has fill/align pragma.
    ST_IS_OPTIONAL_ARGUMENT = 0x00100000, // F90 OPTIONAL arguments
    ST_PT_TO_UNIQUE_MEM	= 0x00200000,	// a pointer that is not aliased
    ST_IS_TEMP_VAR	= 0x00400000,	// compiler generated temp. variable
    ST_IS_CONST_VAR	= 0x00800000,	// read-only variable
    ST_ADDR_SAVED	= 0x01000000,	// address saved
    ST_ADDR_PASSED	= 0x02000000,	// address passed
    ST_IS_THREAD_PRIVATE = 0x04000000,	// Symbol is allocated in XLOCAL
					// data segment
    ST_PT_TO_COMPILER_GENERATED_MEM = 0x08000000,   // a pointer to compiler-
                                        // allocated memory space
    ST_IS_SHARED_AUTO = 0x10000000,     // SCLASS_AUTO and accessed with
                                        // SHARED scope in an MP region
    ST_ASSIGNED_TO_DEDICATED_PREG = 0x20000000,	// assigned to dedicated preg
						// specified in offset
    ST_ASM_FUNCTION_ST  = 0x40000000,	// ST is CLASS_NAME asm string with
                                        // file scope.
    ST_HAS_NAMED_SECTION = 0x80000000	// has named section attribute
    // new flags must go into flags_ext field.
}; // ST_FLAGS
    
#ifdef KEY
enum ST_FLAGS_EXT
{
    ST_ONE_PER_PU = 0x01,		// Only 1 instance per pu
    ST_COPY_CONSTRUCTOR_ST =   0x02,	// ST is copy constructor function
    ST_INITV_IN_OTHER_ST   =   0x04,    // ST is being used as an initianliation offset by other symbol
    ST_IS_INITIALIZED_IN_F90 = 0x8, 
    ST_IS_METHOD_FUNC	= 0x10,         // ST is c++ method function (make sense only 
                                        // when st-class is CLASS_FUNC	
    ST_IS_THIS_PTR      = 0x20, 	// ST is "this"-pointer
    ST_IS_PURE_VFUNC    = 0x40,         // ST is pure virtual function
    ST_IS_THREAD_LOCAL  = 0x80,         // ST is Thread-Local_Storage, __thread
    ST_IS_ARRAY_REMAPPING_CANDIDATE = 0x100,
    ST_IS_ARRAY_REMAPPING_CANDIDATE_MALLOC = 0x200, // storage for the remapped
                                        // array is from malloc()
#if defined(TARG_SL)
    ST_IN_V1BUF = 0x400,                // ST is vector1 variable
    ST_IN_V2BUF = 0x800,                // ST is vector2 variable 
    ST_IN_V4BUF = 0x1000,               // ST is vector4 variable 
    ST_IN_SDRAM = 0x2000,               // ST is sdram variable 
    ST_IN_SBUF =  0x4000,               // ST is explcitly declared sbuf so 
    ST_IS_VBUF_OFFSET = 0x8000,  // represent this symbol means offset instead of a absolute address
    ST_IS_SBUF_OFFSET = 0x10000, // same as above and will be deleted for we don't have sbuf in the future.
#endif     
    ST_IS_GLOBAL_AS_LOCAL = 0x20000, // Is a global variable that can be treated as a local variable.
    ST_IS_VTABLE = 0x40000,        //st is a vtalbe
}; // ST_FLAGS_EXT
#endif

// symbol table element
class ST
{
public:
    // after add new member, Make sure to update function eq_const_st::operator() 
    // in file ipc_symtab_merge.cxx 
    union {
	STR_IDX name_idx;		// index to the name string
	TCON_IDX tcon;			// constant value
    } u1;

    mUINT32 flags;			// misc. attributes

    mUINT32 flags_ext;			// more attributes

    ST_CLASS sym_class : 8;		// class info
    ST_SCLASS storage_class : 8;	// storage info
    ST_EXPORT export_class : 4;		// export class of the symbol
#ifdef TARG_NVISA
    ST_MEMORY memory_space: 4;		// memory space of the symbol
#else
    ST_TLS_MODEL tls_model: 4;		// Thread-Local-Storage(TLS) model
#endif
    union {
	TY_IDX type;			// idx to high-level type
	PU_IDX pu;			// idx to program unit table
	BLK_IDX blk;			// idx to block table
    } u2;

#ifdef KEY
    // bug 14141: we need to ensure the pad bytes are zero so that
    // byte-comparison in IPA does not fail.
    mUINT32 pad;			// 4 pad bytes (initialize to zero)
#endif
    mUINT64 offset;			// offset from base

    ST_IDX base_idx;			// base of the allocated block

    ST_IDX st_idx;			// my own st_idx

    TY_IDX vtable_ty_idx;

    mUINT32 line;           // The line num where define the sym in the source file.

    // operations
    
    ST ()  {Fail_FmtAssertion("ST default constructor must not be called.");}

    void Verify(UINT level) const;
    
    void Print(FILE *f, BOOL verbose = TRUE) const;
    
    BOOL operator==(ST &st) const;

    friend std::ostream& operator<<(std::ostream &os, const ST& st);

}; // ST



// Give information about a field in a struct.  The TY of the struct type
// points to the FLD entry for the first field.  The remaining fields
// follow in consecutive FLD entries, until a flag indicates it is the last
// field.

enum FLD_FLAGS
{
    FLD_LAST_FIELD	= 0x0001,	// last field in a struct
    FLD_EQUIVALENCE	= 0x0002,	// fortran equivalence
    FLD_BEGIN_UNION	= 0x0004,      	// begin a union
    FLD_END_UNION	= 0x0008,	// end a union
    FLD_BEGIN_MAP	= 0x0010,	// begin a map (fortran)
    FLD_END_MAP		= 0x0020,	// end a map
    FLD_IS_BIT_FIELD	= 0x0040,	// is bit field
    FLD_IS_ANONYMOUS    = 0x0080,       // is anonymous field
    FLD_IS_BASE_CLASS   = 0x0100,       // is a field of base class type
    FLD_IS_VIRTUAL      = 0x0200,       // is virutal (base class) field
};

struct FLD
{
    STR_IDX name_idx;

    mUINT64 ofst;			// offset within the struct in bytes

    mUINT8 bsize;			// bit field size in bits
    mUINT8 bofst;			// bit field offset starting at
					// byte specified by ofst
    mUINT16 flags;			// misc. attributes

    ST_IDX st;				// used if an st exists for this fld

    TY_IDX type;

    // operations

    FLD ();

    void Verify (UINT64 record_size) const;

    void Print (FILE *f) const;

}; // FLD


typedef TY_IDX TYLIST;			// for now, it's just a list of TY_IDX


// Give information about a dimension of an array.  The TY of the array type
// points to the ARB entry for the first dimension.  The remaining dimensions
// follow in consecutive ARB entries until a flag indicates it is the last
// dimension.
enum ARB_FLAGS
{
    ARB_CONST_LBND	= 0x0001,	// constant lower bound
    ARB_CONST_UBND	= 0x0002,	// constant upper bound
    ARB_CONST_STRIDE	= 0x0004,	// constant stride
    ARB_FIRST_DIMEN	= 0x0008,	// first dimension
    ARB_LAST_DIMEN	= 0x0010	// last dimension
};

struct ARB
{
    mUINT16 flags;			// misc. attributes
    mUINT16 dimension;			// number of dimensions

    mUINT32 unused;			// must be zero'ed

    union {
	mINT64 lbnd_val;		// constant lower bound value
	struct {
	    ST_IDX lbnd_var;		// variable that stores the
					// non-constant lower bound
	    mINT32 unused;		// filler, must be zero'ed
	} var;
    } u1;

    union {
	mINT64 ubnd_val;		// constant upper bound value
	struct {
	    ST_IDX ubnd_var;		// variable that stores the
					// non-constant upper bound
	    mINT32 unused;		// filler, must be zero'ed
	} var;
    } u2;

    union {
	mINT64 stride_val;		// constant stride
	struct {
	    ST_IDX stride_var;		// variable that stores the
					// non-constant stride
	    mINT32 unused;		// filler, must be zero'ed
	} var;
    } u3;

    // access functions
    INT64 Lbnd_val () const		{ return u1.lbnd_val; }
    void Set_lbnd_val (INT64 val)	{ u1.lbnd_val = val; }

    ST_IDX Lbnd_var () const		{ return u1.var.lbnd_var; }
    void Set_lbnd_var (ST_IDX st) {
	u1.var.lbnd_var = st;
	u1.var.unused = 0;
    }
    
    INT64 Ubnd_val () const		{ return u2.ubnd_val; }
    void Set_ubnd_val (INT64 val)	{ u2.ubnd_val = val; }

    ST_IDX Ubnd_var () const		{ return u2.var.ubnd_var; }
    void Set_ubnd_var (ST_IDX st) {
	u2.var.ubnd_var = st;
	u2.var.unused = 0;
    }
    
    INT64 Stride_val () const		{ return u3.stride_val; }
    void Set_stride_val (INT64 val)	{ u3.stride_val = val; }

    ST_IDX Stride_var () const		{ return u3.var.stride_var; }
    void Set_stride_var (ST_IDX st) {
	u3.var.stride_var = st;
	u3.var.unused = 0;
    }
    
	
    // operations

#ifdef really_call_bzero	// don't call bzero if it is poisoned
    ARB () { really_call_bzero (this, sizeof(ARB)); }
#else
    ARB () { BZERO (this, sizeof(ARB)); }
#endif

    void Verify (mUINT16 dim) const;
    
    void Print (FILE *f) const;

}; // ARB


enum LABEL_KIND
{
    LKIND_DEFAULT		= 0,
    LKIND_ASSIGNED		= 1,	// in ASSIGNED statement
    LKIND_BEGIN_EH_RANGE	= 2,
    LKIND_END_EH_RANGE		= 3,
    LKIND_BEGIN_HANDLER		= 4,
    LKIND_END_HANDLER		= 5,
    LKIND_TAG			= 6	// symbolic address, never branched to
};

enum LABEL_FLAGS
{
    LABEL_TARGET_OF_GOTO_OUTER_BLOCK	= 1,
    LABEL_ADDR_SAVED			= 2,
    LABEL_ADDR_PASSED			= 4
};

struct LABEL
{
    STR_IDX    name_idx;
    mUINT32    flags:24;
    LABEL_KIND kind:8;

    // operations
    
    LABEL () {Fail_FmtAssertion("LABEL default constructor must not be called.");}

    LABEL (STR_IDX idx, LABEL_KIND k) : name_idx (idx), kind (k) {}

    void Verify(UINT level) const;

    void Print (FILE *f) const;
}; // LABEL


struct PREG
{
    STR_IDX  name_idx;

    // operations
    PREG(void)
      {
	Fail_FmtAssertion("PREG default constructor must not be called.");
      }

    PREG (STR_IDX idx) : name_idx (idx) { }

    void Verify(UINT level) const;

    void Print (FILE *f) const;

}; // PREG


// misc. ST attributes
enum ST_ATTR_KIND
{
    ST_ATTR_UNKNOWN		= 0,
    ST_ATTR_DEDICATED_REGISTER	= 1,	// physical register number
    ST_ATTR_SECTION_NAME	= 2	// name of sections where defined.
};

class ST_ATTR
{
    public:
    ST_IDX st_idx;
    ST_ATTR_KIND kind;
    private:
    union {
	mUINT32 value;			// generic 32-bit value
	mPREG_NUM reg_id;
	STR_IDX section_name;
    } u;

    public:
    // operations
    ST_ATTR () {
	Fail_FmtAssertion("ST_ATTR default constructor must not be called.");
    }

    ST_ATTR (ST_IDX idx, ST_ATTR_KIND akind, UINT64 val) :
	st_idx (idx), kind (akind) {
	u.section_name = val;
    }

    // Access the union value
    void Set_u (UINT64 val) {
        u.section_name = val;
    }

    // Access reg_id
    void Set_reg_id (mPREG_NUM r) {
        u.reg_id = r;
    }

    mPREG_NUM Get_reg_id (void) const {
        return u.reg_id;
    }

    // Access section_name
    void Set_section_name (STR_IDX i) {
        u.section_name = i;
    }

    STR_IDX Get_section_name (void) const {
        return u.section_name;
    }
	    

    void Verify (UINT level) const;

    void Print (FILE* f) const;
}; // ST_ATTR



/* Kinds of types: */
enum TY_KIND
{
    KIND_INVALID	= 0,		// Invalid
    KIND_SCALAR		= 1,		// integer/floating point
    KIND_ARRAY		= 2,		// array
    KIND_STRUCT		= 3,		// struct/union
    KIND_POINTER	= 4,		// pointer
    KIND_FUNCTION	= 5,		// function/procedure
    KIND_VOID		= 6,		// C void type
    KIND_LAST		= 8
};


enum TY_FLAGS
{
    TY_IS_CHARACTER	= 0x0001,	/* type is a character (fortran) */
    TY_IS_LOGICAL	= 0x0002,	/* Type is logical (fortran) */
    TY_IS_UNION		= 0x0004,	/* Struct or class type is union */
    TY_IS_PACKED	= 0x0008,	/* Struct or class type is packed */
    TY_PTR_AS_ARRAY	= 0x0010,	/* Treat pointer as array */
    TY_ANONYMOUS	= 0x0020,	/* Anonymous structs/classes/unions */
    TY_SPLIT		= 0x0040,	/* Split from a larger common block
					 * equivalence (block_split) */
    TY_IS_F90_POINTER	= 0x0080,	/* If the type is an F90 pointer */
    TY_NOT_IN_UNION	= 0x0100,	/* If the type cannot be part of a union */
    TY_NO_ANSI_ALIAS	= 0x0200,	// ANSI alias rules don't applied
    TY_IS_NON_POD	= 0x0400,	// type is non pod (for C++ classes)
#ifdef KEY
    TY_RETURN_IN_MEM	= 0x0800,	// Functions must return objects of this
					// type in memory.
    TY_CONTENT_SEEN	= 0x1000,	// for differentiating between a 0-sized
    					// struct and its forward declaration;
					// used by wgen
    TY_IS_INCOMPLETE    = 0x2000,       // Type is incomplete, used by wgen to
                                        // mark VLA types when used as function
                                        // arguments.
    TY_NO_SPLIT         = 0x4000,       // May be used by any phase after
                                        // front-end for TY that should not
                                        // be split.
#endif
#ifdef TARG_NVISA
    TY_CAN_BE_VECTOR	= 0x8000,	// vector type like int4
#endif
    TY_COMPLETE_STRUCT_RELAYOUT_CANDIDATE = 0x0001, // it's OK to share this
      // with TY_IS_CHARACTER above for now, since this has to be a struct
};


// TY flags that are valid only for KIND_FUNCTION
enum TY_PU_FLAGS
{
    TY_RETURN_TO_PARAM	= 0x00000001,	// return value through first param
    TY_IS_VARARGS	= 0x00000002,	// variable number of arguments
    TY_HAS_PROTOTYPE	= 0x00000004,	// has ansi-style prototype
#ifdef TARG_X8664
    TY_HAS_SSEREG_PARM	= 0x00000008,	// SSE register parameters under i386
    TY_HAS_1_REG_PARM	= 0x00000010,	// 1 register parameter under i386
    TY_HAS_2_REG_PARM	= 0x00000020,	// 2 register parameters under i386
    TY_HAS_3_REG_PARM	= 0x00000030,	// 3 register parameters under i386
    TY_HAS_STDCALL      = 0x00000040,   // stdcall calling convention under i386
    TY_HAS_FASTCALL     = 0x00000080    // fastcall calling convention under i386
#endif
};

class TY
{
public:
    mUINT64 size;			// size of the type in bytes

    TY_KIND kind : 8;			// kind of type
    mTYPE_ID mtype : 8;			// WHIRL data type
    mUINT16 flags;			// misc. attributes

    union {
	FLD_IDX fld;
	TYLIST_IDX tylist;
	ARB_IDX arb;
    } u1;				// idx to FLD_TAB, TYLIST_TAB, etc.

    STR_IDX name_idx;			// name 

    union {
	TY_IDX etype;			// type of array element (array only)
	TY_IDX pointed;			// pointed-to type (pointers only)
	mUINT32 pu_flags;		// attributes for KIND_FUNCTION
#ifdef KEY
	ST_IDX copy_constructor;	// copy constructor X(X&) (record only)
#endif
    } u2;
	
    ST_IDX vtable;

    // access function for unions

    FLD_IDX Fld () const		{ return u1.fld; }
    void Set_fld (FLD_IDX idx)		{ u1.fld = idx; }
    
    TYLIST_IDX Tylist () const		{ return u1.tylist; }
    void Set_tylist (TYLIST_IDX idx)	{ u1.tylist = idx; }
    
    ARB_IDX Arb () const		{ return u1.arb; }
    void Set_arb (ARB_IDX idx)		{ u1.arb = idx; }

    TY_IDX Etype () const
      {
	Is_True(kind == KIND_ARRAY,
		("non-KIND_ARRAY type has no element type"));
	return u2.etype;
      }
    void Set_etype (TY_IDX idx)		{ u2.etype = idx; }
    
    TY_IDX Pointed () const
      {
	Is_True(kind == KIND_POINTER,
		("non-KIND_POINTER type doesn't point"));
	return u2.pointed;
      }
    void Set_pointed (TY_IDX idx)	{ u2.pointed = idx; }

#ifdef KEY
    ST_IDX Copy_constructor () const
      {
	Is_True(kind == KIND_STRUCT,
		("non-KIND_STRUCT type has no copy constructor"));
	return u2.copy_constructor;
      }
    void Set_copy_constructor (ST_IDX idx)	{ u2.copy_constructor = idx; }
#endif	// KEY

    ST_IDX Vtable () const
      {
	Is_True(kind == KIND_STRUCT,
		("non-KIND_STRUCT type has no vtable"));
	return vtable;
      }
    void Set_vtable (ST_IDX idx)	{ vtable = idx; }


    PU_IDX Pu_flags () const		{ return u2.pu_flags; }
    void Set_pu_flag (TY_PU_FLAGS f)	{ u2.pu_flags |= f; }
    void Clear_pu_flag (TY_PU_FLAGS f)	{ u2.pu_flags &= ~f; }

    // operations

    TY ();

    void Verify(UINT level) const;

    void Print (FILE *f) const;

}; // TY


// PU_FLAGS: cannot use enum as some of the the values are > max unsigned int

#define	PU_IS_PURE		0x00000001	// pure function
#define	PU_NO_SIDE_EFFECTS	0x00000002	// no side effect
#define	PU_IS_INLINE_FUNCTION	0x00000004	// inline keyword specified
#define	PU_NO_INLINE		0x00000008	// noinline pragma specified
#define	PU_MUST_INLINE		0x00000010	// must inline
#define	PU_NO_DELETE		0x00000020	// nodelete pragma specified
#define	PU_HAS_EXC_SCOPES	0x00000040	// has eh regions, or would have
						// if exceptions were enabled
#define	PU_IS_NESTED_FUNC	0x00000080	// is a nested function
#define	PU_HAS_NON_MANGLED_CALL	0x00000100	// PU has a call in which no
					 	// reshaped arrays are passed
#define	PU_ARGS_ALIASED		0x00000200	// f77 arguments are aliased
#define	PU_NEEDS_FILL_ALIGN_LOWERING 0x00000400	// needs fill/align lowering
#define	PU_NEEDS_T9		0x00000800	// needs T9
#define	PU_HAS_VERY_HIGH_WHIRL	0x00001000	// PU has very high whirl in it
#define	PU_HAS_ALTENTRY		0x00002000	// PU has alternate entries
#define	PU_RECURSIVE		0x00004000	// in recursive path
#define	PU_IS_MAINPU		0x00008000	// is entry point of program
#define	PU_UPLEVEL		0x00010000	// Other PU nested in this one
#define	PU_MP_NEEDS_LNO		0x00020000	// PU needs LNO processing
#define	PU_HAS_ALLOCA		0x00040000	// PU has alloca in it
#define	PU_IN_ELF_SECTION	0x00080000	// PU is in its own Elf section
#define	PU_HAS_MP		0x00100000	// Symtab has MP region/do within it
#define	PU_MP			0x00200000	// PU is an MP region/do
#define	PU_HAS_NAMELIST		0x00400000	// PU has namelist
#define	PU_HAS_RETURN_ADDRESS	0x00800000	// __return_address was used
#define	PU_HAS_REGION		0x01000000	// PU has regions in it
#define	PU_HAS_INLINES		0x02000000	// PU has inlined code in it
#define	PU_CALLS_SETJMP		0x04000000	// PU has calls to setjmp(2)
#define	PU_CALLS_LONGJMP	0x08000000	// PU has calls to longjmp(2)
#define PU_IPA_ADDR_ANALYSIS    0x10000000      // IPA has done address analysis
#define PU_SMART_ADDR_ANALYSIS  0x20000000      // Unnecessary addr flags are reset

#define	PU_HAS_SYSCALL_LINKAGE	0x40000000	// preserve input regs
#define PU_HAS_GLOBAL_PRAGMAS	0x80000000	// PU is a dummy pu with global
						//   pragmas
#define	PU_HAS_USER_ALLOCA	0x0000000100000000LL
						// PU has user alloca in it
#define	PU_HAS_UNKNOWN_CONTROL_FLOW \
                          	0x0000000200000000LL
						// PU has unknown control flow
						//  which disables tail call 
						// optimization
#define PU_IS_THUNK		0x0000000400000000LL // pu is a C++ thunk

#ifdef KEY
#define PU_NEEDS_MANUAL_UNWINDING \
                                0x0000000800000000LL // PU has cleanups in outermost scope and hence needs to call _Unwind_Resume itself
#endif

#ifdef TARG_X8664
#define PU_FF2C_ABI		0x0000001000000000LL // PU use g77 linkage convention for returns of complex and float
#endif

#ifdef KEY
#define PU_IS_EXTERN_INLINE	0x0000002000000000LL // PU is marked extern _inline_ in C
#define PU_MP_LOWER_GENERATED   0x0000004000000000LL // PU generated by mp-lowerer
#define PU_IS_MARKED_INLINE     0x0000008000000000LL
                                                   // C/C++: inline keyword used. Note
                                                   // PU_IS_INLINE_FUNCTION just
                                                   // indicates GNU inlining decision.
#define PU_NO_INSTRUMENT        0x0000010000000000LL // -finstrument-functions will skip PU
#endif

#define PU_HAS_ATTR_MALLOC      0x0000020000000000LL // __attribute__((malloc)) semantic 
#define PU_HAS_ATTR_PURE        0x0000040000000000LL // __attribute__((pure)) semantic 
#define PU_HAS_ATTR_NORETURN    0x0000080000000000LL // __attribute__((noreturn)) semantic
#define PU_IS_CONSTRUCTOR       0x0000100000000000LL  // PU is a constructor of a class
#define PU_IS_OPERATOR          0x0000200000000000LL // PU is overload of operator

#define PU_NEED_TRAMPOLINE      0x0000400000000000LL // a nested function whose address is taken
#define PU_HAS_NONLOCAL_GOTO_LABEL	\
                                0x0000800000000000LL // has a label jumped to directly from a nested function
#define PU_HAS_GOTO_OUTER_BLOCK	0x0001000000000000LL // has GOTO_OUTER_BLOCK stmt
#define PU_IS_CDECL             0x0002000000000000LL // __attribute__((cdecl)) semantic
#define PU_NOTHROW              0x0004000000000000LL // doesn't throw, e.g. decl as "void foo() throw()".
#define PU_HAS_APPLY_ARGS       0x0008000000000000LL // __builtin_apply_args
#define PU_SIMPLE_EH_RANGE      0x0010000000000000LL // there is a single eh range in PU, no clean-up or catch

enum PU_SRC_LANG_FLAGS
{
    PU_UNKNOWN_LANG	= 0x00,	// UNKNOWN 
    PU_MIXED_LANG	= 0x01,	// MIXED
    PU_C_LANG		= 0x02,	// C
    PU_CXX_LANG		= 0x04,	// C++
    PU_F77_LANG		= 0x08,	// F77
    PU_F90_LANG		= 0x10,	// F90
    PU_JAVA_LANG	= 0x20	// JAVA
};

struct PU
{
    TARGET_INFO_IDX target_idx;		// idx to table for target-specific 
					// information

    TY_IDX prototype;			// function prototype
    TY_IDX base_class;                  // the class type which this PU belongs to if this PU is a member function
    SYMTAB_IDX lexical_level;		// lexical level (of nested proc). 8-bits
    INITO_IDX misc;		        // store misc such EH related TYPE/TYPE_SPEC info. 32bits
#ifdef TARG_NVISA
    mUINT16 thread_limit;
    mUINT16 block_limit;
#endif
    mUINT8 gp_group;			// gp_group id
    mUINT8 src_lang;			// source language
    mUINT8 unused : 8;		        // for alignment 
    mUINT64 flags;			// misc. attributes about this func.

    // operations

    PU ();

    void Verify(UINT level) const;

    void Print (FILE *f) const;

}; // PU


// BLK only exists for CLASS_BLOCK.
// It is used for data layout in be.
// Only reason it is not local to be is that
// the IPA global symbol table will need the info.
class BLK
{
private:

	mUINT64 size;		// size of the block
	mUINT16 align;		// alignment of the block: 1,2,4,8
	mUINT16 flags;		// block flags
	mUINT16 section_idx;	// section index (0 if not a section)
	mUINT16 scninfo_idx;	// scninfo_idx (0 if not a section)

public:

	BLK () : size (0), section_idx(0), scninfo_idx(0), flags (0) {}

#ifdef really_call_bzero	// don't call bzero if it is poisoned
	void Init (void)	{ really_call_bzero (this, sizeof(BLK)); }
#else
	void Init (void)	{ BZERO (this, sizeof(BLK)); }
#endif

public:

    // access functions

    UINT64 Size () const	{ return size; }
    void Set_size (UINT64 s)	{ size = s; }

    UINT16 Align () const	{ return align; }
    void Set_align (UINT16 s)	{ align = s; }

    UINT16 Section_idx () const	{ return section_idx; }
    void Set_section_idx (UINT16 s)	{ section_idx = s; }
    UINT16 Scninfo_idx () const	{ return scninfo_idx; }
    void Set_scninfo_idx (UINT16 s)	{ scninfo_idx = s; }

    UINT16 Flags () const		{ return flags; }
    BOOL Is_set (UINT16 f) const	{ return flags & f; }
    void Set_flags (UINT16 f)		{ flags |= f; }
    void Clear_flags (UINT16 f)		{ flags &= ~f; }
    void Clear_all_flags ()		{ flags = 0; }

    void Print (FILE *f) const;
}; // BLK

// BLK flags
#define BLK_SECTION		0x0001 // block for elf section
#define BLK_ROOT_BASE		0x0002 // block should not be merged
#define BLK_IS_BASEREG		0x0004 // block that maps into reg
#define BLK_DECREMENT		0x0008 // grow block by decrementing
#define BLK_EXEC		0x0010 // (ELF) executable instructions
#define BLK_NOBITS		0x0020 // (ELF) occupies no space in file
#define BLK_MERGE		0x0040 // (ELF) merge duplicates in ld
#define BLK_COMPILER_LAYOUT	0x0080 // children symbols are not connected


// place holder for misc. file-level info

enum FILE_INFO_FLAGS
{
    FI_IPA		= 0x1,		// IPA generated file
    FI_NEEDS_LNO	= 0x2,		// needs to run LNO
    FI_HAS_INLINES	= 0x4,		// some PUs have PU_HAS_INLINES set
    FI_HAS_MP   	= 0x8,		// need to process MP constructs
    FI_HAS_GLOBAL_ASM	= 0x10		// contains global asm, do not emit .org
};

struct FILE_INFO
{
    mUINT32 flags;			// misc. attributes
    mUINT8 gp_group;			// gp group id
    mUINT32 unused : 24;		// filler, must be zero

    void Verify() const;
    void Print (FILE *f) const;
    FILE_INFO () : flags (0), gp_group (0), unused (0) {}

};


// Type definitions of the symbol tables

typedef SEGMENTED_ARRAY<PU>	PU_TAB;
typedef RELATED_SEGMENTED_ARRAY<ST>	ST_TAB;
typedef SEGMENTED_ARRAY<TY>	TY_TAB;
typedef SEGMENTED_ARRAY<FLD>	FLD_TAB;
typedef SEGMENTED_ARRAY<TYLIST>	TYLIST_TAB;
typedef SEGMENTED_ARRAY<ARB>	ARB_TAB;
typedef RELATED_SEGMENTED_ARRAY<LABEL>	LABEL_TAB;
typedef RELATED_SEGMENTED_ARRAY<PREG>	PREG_TAB;
typedef SEGMENTED_ARRAY<ST_ATTR>	ST_ATTR_TAB;
typedef SEGMENTED_ARRAY<BLK>	BLK_TAB;
typedef SEGMENTED_ARRAY<TCON,64> TCON_TAB;
typedef SEGMENTED_ARRAY<INITO>	INITO_TAB;
typedef SEGMENTED_ARRAY<INITV>	INITV_TAB;

typedef PU_TAB::iterator	PU_ITER;
typedef ST_TAB::iterator	ST_ITER;
typedef TY_TAB::iterator	TY_ITER;
typedef FLD_TAB::iterator	FLD_ITER;
typedef TYLIST_TAB::iterator	TYLIST_ITER;
typedef ARB_TAB::iterator	ARB_ITER;
typedef LABEL_TAB::iterator	LABEL_ITER;
typedef PREG_TAB::iterator	PREG_ITER;
typedef ST_ATTR_TAB::iterator	ST_ATTR_ITER;
typedef BLK_TAB::iterator	BLK_ITER;
typedef TCON_TAB::iterator	TCON_ITER;
typedef INITO_TAB::iterator	INITO_ITER;
typedef INITV_TAB::iterator	INITV_ITER;


struct SCOPE
{
    MEM_POOL *pool;			// mem pool for local tables
    ST* st;				// ST * for the current pu
    ST_TAB *st_tab;
    LABEL_TAB *label_tab;
    PREG_TAB *preg_tab;
    INITO_TAB *inito_tab;
    ST_ATTR_TAB *st_attr_tab;

    void Init (ST_TAB *s) {
	// for global symtab
	pool = NULL;
	st_tab = s;
	st = NULL;
	label_tab = NULL;
	preg_tab = NULL;
	inito_tab = NULL;
	st_attr_tab = NULL;
    }

    void Init (ST_TAB *s, LABEL_TAB *l, PREG_TAB *p, INITO_TAB *io,
	       ST_ATTR_TAB* d, MEM_POOL *mp = Malloc_Mem_Pool) {
	pool = mp;
	st = NULL;
	st_tab = s;
	label_tab = l;
	preg_tab = p;
	inito_tab = io;
	st_attr_tab = d;
	
    }
    

}; // SCOPE

// predefined Scope indices
#define GLOBAL_SYMTAB		(1)	// file scope global symtab
#define CURRENT_SYMTAB		(Current_scope)

struct SCOPE_TAB_SYMTAB_ACCESS {
  SCOPE_TAB_SYMTAB_ACCESS(void) { }

  ST_TAB *operator()(SCOPE **scope_tab, SYMTAB_IDX level)
    { return (*scope_tab)[level].st_tab; }
};

struct SCOPE_TAB_INITO_ACCESS {
  SCOPE_TAB_INITO_ACCESS(void) { }

  INITO_TAB *operator()(SCOPE **scope_tab, SYMTAB_IDX level)
    { return (*scope_tab)[level].inito_tab; }
};

struct SCOPE_TAB_LABEL_ACCESS {
  SCOPE_TAB_LABEL_ACCESS(void) { }

  LABEL_TAB *operator()(SCOPE **scope_tab, SYMTAB_IDX level)
    { return (*scope_tab)[level].label_tab; }
};

// The global scope table
extern SCOPE		*Scope_tab;

// One instance of the following class per compilation.
typedef TABLE_INDEXED_BY_LEVEL8_AND_INDEX24<ST, ST_IDX, SYMTAB_IDX,
                                            SCOPE *, &Scope_tab,
					    SCOPE_TAB_SYMTAB_ACCESS>
        SYMBOL_TABLE;

typedef TABLE_INDEXED_BY_LEVEL8_AND_INDEX24<INITO, INITO_IDX, SYMTAB_IDX,
                                            SCOPE *, &Scope_tab,
					    SCOPE_TAB_INITO_ACCESS>
        INITO_TABLE;

typedef TABLE_INDEXED_BY_LEVEL8_AND_INDEX24<LABEL, LABEL_IDX, SYMTAB_IDX,
                                            SCOPE *, &Scope_tab,
					    SCOPE_TAB_LABEL_ACCESS>
        LABEL_TABLE;

struct PREG_TABLE
{
  inline PREG& operator[] (PREG_IDX idx);
  inline PREG& operator() (SYMTAB_IDX level, PREG_IDX idx);
};

struct ST_ATTR_TABLE
{
  inline ST_ATTR& operator[] (ST_ATTR_IDX idx);
  inline ST_ATTR& operator() (SYMTAB_IDX level, ST_ATTR_IDX idx);
};

struct TYPE_TABLE
{
  inline TY& operator[] (TY_IDX idx);
  inline TY_TAB* operator& ();
};

// declaration of global tables
extern FILE_INFO	File_info;
extern PU_TAB		Pu_Table;
extern SYMBOL_TABLE	St_Table;
extern TY_TAB		Ty_tab;
extern TYPE_TABLE	Ty_Table;
extern FLD_TAB		Fld_Table;
extern TYLIST_TAB	Tylist_Table;
extern ARB_TAB		Arb_Table;
extern STRING_TABLE     Str_Table;
extern TCON_TAB		Tcon_Table;
extern INITV_TAB	Initv_Table;
extern INITO_TABLE	Inito_Table;
extern PREG_TABLE	Preg_Table;
extern ST_ATTR_TABLE	St_Attr_Table;
extern LABEL_TABLE	Label_Table;
// some BLK_TAB items are really local, but make global to ease management
extern BLK_TAB		Blk_Table;

// global variables

extern SYMTAB_IDX Current_scope;
extern PU* Current_pu;




// headers that describe the layout of the symtab structures in files
// changing any of these requires incrementing the WHIRL revision number

// symbol table header types
enum SHDR_TYPE
{
    SHDR_UNK	= 0,			// uninitialized
    SHDR_FILE	= 1,
    SHDR_ST	= 2,
    SHDR_TY	= 3,
    SHDR_PU	= 4,
    SHDR_FLD	= 5,
    SHDR_ARB	= 6,
    SHDR_TYLIST	= 7,
    SHDR_TCON	= 8,
    SHDR_STR	= 9,
    SHDR_LABEL	= 10,
    SHDR_PREG	= 11,
    SHDR_INITO	= 12,
    SHDR_INITV	= 13,
    SHDR_BLK 	= 14,
    SHDR_ST_ATTR= 15
}; // SHDR_TYPE


struct SYMTAB_HEADER
{
    mUINT64 offset;			// offset from beginning of section
    mUINT64 size;			// size in bytes
    mUINT32 entsize;			// size of each entry if fixed
					// zero if entry size variable
    mUINT16 align;			// alignment
    mUINT16 type;			// type of table

    void Init (UINT64 _offset, UINT64 _size, UINT32 _entsize,
	       UINT16 _align, SHDR_TYPE _type) {
	offset = _offset;
	size = _size;
	entsize = _entsize;
	align = _align;
	type = _type;
    }
    
}; // SYMTAB_HEADER


#define GLOBAL_SYMTAB_TABLES	(13)	// # of tables in global symtab:
					// FILE_INFO, ST, TY, PU, FLD, ARB,
					// TYLIST, TCON, STR, INITO, INITV,
					// BLK, and ST_ATTR
#define LOCAL_SYMTAB_TABLES	(5)	// # of tables in local symtab:
					// ST, LABEL, PREG, INITO, and ST_ATTR

template <UINT table_size>
struct SYMTAB_HEADER_TABLE
{
    mUINT32 size;			// size of myself in bytes
    mUINT32 entries;			// number of SYMTAB_HEADER entries

    SYMTAB_HEADER header[table_size];

    typedef SYMTAB_HEADER_TABLE<table_size> self;

    SYMTAB_HEADER_TABLE () {
	size = sizeof(self);
	entries = table_size;
#ifdef really_call_bzero	// don't call bzero if it is poisoned
	really_call_bzero (header, sizeof(header));
#else
	BZERO (header, sizeof(header));
#endif
    }

}; // SYMTAB_HEADER_TABLE

typedef SYMTAB_HEADER_TABLE<GLOBAL_SYMTAB_TABLES> GLOBAL_SYMTAB_HEADER_TABLE;
typedef SYMTAB_HEADER_TABLE<LOCAL_SYMTAB_TABLES> LOCAL_SYMTAB_HEADER_TABLE;

#endif /* symtab_defs_INCLUDED */

