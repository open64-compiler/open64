/*
 * Copyright (C) 2009 Advanced Micro Devices, Inc.  All Rights Reserved.
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


#ifndef opt_points_to_INCLUDED
#define opt_points_to_INCLUDED	"opt_points_to.h"

#include "defs.h"
#include "config.h"
#include "opt_defs.h"
#include "cxx_base.h"
#include "cxx_memory.h"		// for CXX_NEW
#include "opcode.h"
#include "wn.h"
#include "config_wopt.h"	// for WOPT_Alias_Class_Limit,
				//     WOPT_Ip_Alias_Class_Limit
#include "be_memop_annot.h"
#include "alias_analyzer.h"
#if defined(TARG_SL)
#include "intrn_info.h"
#endif

typedef struct bs BS;

// interface to Mongoose symbol table
struct STAB_ADAPTER {
    ST *St_ptr (WN *wn) const { return WN_st (wn); }
};

class OPT_STAB;

//  ALIAS ANALYSIS and POINTS TO ANALYSIS
//
//  The Mongoose optimizer distinguishes points-to analysis from alias
//  analysis. The former analyzes the lvalue of an expression and
//  summarizes the information into a POINTS_TO instance.
//
//  The latter uses the POINTS_TO information to perform alias analysis.
//
//  The POINTS_TO information is gathered in two methods:
//   1) Flow free
//        such as determining the symbol, the type, the qualifiers, ...
//   2) Flow sensitive
//        such as determining the value of a pointer, ...
// 
//  The alias analysis is performed in 3 domains:
//   1)  symbol to symbol
//         This happens in FORTRAN common blocks, C unions ...
//   2)  memop to symbol
//         This is used by SSA
//   3)  memop to memop
//         This is not used by SSA, but required by the OPT/CG ...
//   4)  call to symbol
//         This is used by SSA.
//   5)  call to memop
//         This is used by SSA.
//


//  EXPR_KIND keeps track of the kind of the pointer expression
//  It is used only during FSA.
//
//     INVALID         -- the compiler should assert when this value is used.
//     BEING_PROCESSED -- this pointer expression is being processed.
//                        assume the most optimistic value in FSA.
//     INT             -- the expression contains an integer expression.
//     ADDR            -- the expression is based on some variable.
//     UNKNOWN         -- the most conservative assumption should be made.
//     ANY             -- the most optimistic assumption should be made.
//
enum EXPR_KIND {
  EXPR_IS_INVALID,
  EXPR_IS_BEING_PROCESSED,
  EXPR_IS_INT,
  EXPR_IS_ADDR, 
  EXPR_IS_UNKNOWN,   // expr is unknown, take it conservatively
  EXPR_IS_ANY        // expr is unknown, but can be combined with anything
  };

//  BASE_KIND qualifies what type of information *base contains.
//
//    INVALID        -- should cause assertion.
//    FIXED          -- the memop is based on a global or local variable.
//    DYNAMIC        -- the base is determined at runtime.
//    UNKNOWN        -- no information  
//
//  The base field in the POINTS_TO data structure can be
//     1) a static address (represented by a ST *)
//     2) a dynamic computed value
//        2a) a WHIRL expression  (represented by a WN *)
//        2b) a CHI node          (represented by a CHI_NODE *)
//        2c) a PHI node          (represented by a PHI_NODE *)
//        2d) it is not defined in this PU (i.e, parameter or global 
//             or even undefined locals).
//             (Represented by the aux_id.)
//
enum BASE_KIND {
  BASE_IS_INVALID,     
  BASE_IS_FIXED,	    //  base() is ST*
  BASE_IS_DYNAMIC,          //  base() is some definition
  BASE_IS_UNKNOWN,          //  base() is NULL
  // *** Do not add new base_kind after this line ***
  MAX_BASE_KIND             //  For creating array indexed by BASE_KIND
  };


//  OFTS_KIND qualifies what type of information the ofst and size field contains
//
//   INVALID     -- should assert
//   FIXED       -- the ofst field denotes the lower range of access
//                  and the ofst+size denotes the upper range of access
//   UNKNOWN     -- the ofst field contains no useful information
//
enum OFST_KIND {
  OFST_IS_INVALID,        // invalid ofst kind 
  OFST_IS_FIXED,          // offset is fixed (available in the ofst field)
  OFST_IS_UNKNOWN         // offset is unknown
  };


//  Contains single-bit attributes.
//    
//    Notice the 0 is the default (conservative) values for every attribute.
//    Example: the default attribute for a variable is address taken.
//
//    If a variable is not address taken and we assume it is,
//    alias analysis is not accurate but still produces a correct answer.
//    However, it the variable is address taken and we assume it is not,
//    then alias analysis will produce an incorrect answer.
//
//    NOT_ADDR_SAVED  --  The address of this variable is not saved into a variable.
//    NOT_ADDR_PASSED --  The address of this variable is not passed to some routine.
//    LOCAL           --  The variable determined to be local variable.
//                        This bit does not necessarily set for all locals.
//    NAMED           --  This memory operation accesses a named variable.
//    CONST           --  This memory operation accesses read-only
//                        data (i.e., data that we can assume is not
//                        written by anything in the program [except
//                        possibly runtime initializer/constructor?]).
//    RESTRICTED      --  This memory operation is based on a restricted pointer.
//    UNIQUE_PT       --  This memory operation is based on a unique pointer.
//    F_PARAM         --  This memory operation is based on a Fortran parameter.
//    NO_ALIAS        --  This memory operation is accessed a variable
//                        that has no alias except itself.
//    WEAK            --  This memory operation writes to a weak symbol.
//    WEAK_BASE       --  This symbol is the base(target) of a weak symbol.
//    IS_POINTER      --  Represents a range of address of a pointer.
//    SAFE_TO_SPECULATE -- safe to speculate.
//    NOT_AUTO        --  It is known that if this variable has an
//                        initializer, it is initialized at compile/link
//                        time (not run time). C/C++ "automatic"
//                        locals do not have this bit set. C/C++
//                        globals and "static" do. Currently, this is
//                        used only when the CONST attribute is also
//                        set; it indicates that we can safely say
//                        this item does not alias. With CONST and
//                        without NOT_AUTO, we must at least say the
//                        item's initialization aliases with its uses,
//                        so the uses don't get scheduled first.
//    FORMAL          --  it is a formal parameter passed to this routine.
//                        Needs this bit because after data layout, formal parameters
//                        and local variables are layout w.r.t. to $SP.
//
enum PT_ATTR {
  PT_ATTR_NONE = 0,
  PT_ATTR_NOT_ADDR_SAVED  = 0x1,      // address is not saved
  PT_ATTR_NOT_ADDR_PASSED = 0x2,      // address is not passed
  PT_ATTR_LOCAL           = 0x4,      // local variable
  PT_ATTR_GLOBAL          = 0x8,      // global variable
  PT_ATTR_NAMED           = 0x10,     // reference a named variable
  PT_ATTR_CONST           = 0x20,     // the variable is read-only
  PT_ATTR_RESTRICTED      = 0x40,     // pointer gives restricted access
  PT_ATTR_UNIQUE_PT       = 0x80,     // pointer gives exclusive access
  PT_ATTR_F_PARAM         = 0x100,    // fortran call-by-ref parameter
  PT_ATTR_DEDICATED       = 0x200,    // dedicated register or memory
				      // location
  PT_ATTR_NO_ALIAS        = 0x400,    // this object has no alias

  //  Weak is different from the other attr.
  //  When two POINTS_TO are combined, it either one is weak, then
  //  the combined one is weak
  PT_ATTR_WEAK            = 0x800,    // weak symbol
  PT_ATTR_WEAK_BASE       = 0x1000,   // base of a weak symbol
  PT_ATTR_IS_POINTER      = 0x2000,   // is a pointer content (not memory ref)
  PT_ATTR_SAFE_TO_SPECULATE = 0x4000, // safe to speculate
  PT_ATTR_NOT_AUTO        = 0x8000,   // static init is not at run time
  PT_ATTR_FORMAL          = 0x10000,  // is a formal parameter
  PT_ATTR_DEFAULT_VSYM    = 0x20000,  // is the default vsym, alias to
				      // everything
  PT_ATTR_F90_POINTER     = 0x40000,  // access is known to be through
				      // an f90 pointer
  PT_ATTR_NOT_F90_POINTER = 0x80000,  // access is known not to be
				      // through an f90 pointer
  PT_ATTR_NOT_F90_TARGET  = 0x100000, // access is to memory that may
				      // be pointed to by f90 pointer
  PT_ATTR_NOT_ALLOCA_MEM  = 0x200000, // this access can safely be
                                      // moved past stack pointer updates
  PT_ATTR_EXTENDED        = 0x400000, // used by alias manager to represent
                                      // a consecutive array of POINTS_TO
  PT_ATTR_THIS_PTR        = 0x800000,  // indirect access of "this" pointer
#ifdef KEY
  PT_ATTR_FIELD          = 0x1000000,  // is a field in a struct
#endif
  PT_ATTR_ARRAY          = 0x2000000,  // array reference

  // 24 of 32 bits used
};


//  ALIAS_INFO is the subset of POINTS_TO that will be passed in .B file.
//
// _ptr, _ptr_ver and _iofst_kind are devoted to indrect access. 
// Their meaning are self-descriptive. These three fileds, together 
// with _{bit|byte}_{ofst|size},  are used to descibe a indirect access.
// e.g. assume <p> is pointer to integer array. the ALIAS_INFO for 
// access "p[13]" has: 
//     o. _base = NULL, however, _ptr="p" and _ptr_ver=current_version_of_p
//     o. _ofst_kind=OFST_UNKNONW however _iofst_kind=OFST_FIXED
//     o. _bit_{ofst|size} = 0
//     o. _byte_ofst = sizeof(int)*13
//     o. _byte_size = sizeof(int)
// 
class ALIAS_INFO {

friend class POINTS_TO;
  mUINT32       _expr_kind :3;     // one of EXPR_KIND
  mUINT32       _base_kind :3;     // one of BASE_KIND
  mUINT32       _ofst_kind :2;     // one of OFST_KIND
  mUINT32       _based_sym_depth:3;// used for disjoint
  mUINT32	_unused : 5;	   // unused bits for future expansion
  mUINT32	_bit_ofst : 8;     // bit offset (for bit fields)
  mUINT32	_bit_size : 8;	   // bit size (for bit fields)
				   // _bit_size should be zero when this is 
				   // not a bit field.  When it is
				   // non-zero, _byte_ofst/_byte_size
				   // gives the ofst/size of the container
				   // of the bit field, and _bit_ofst/_bit_size
				   // gives the bit ofst/size within the
				   // container 
  mUINT32	_iofst_kind :2;    // one of OFST_KIND. the kind of "offset" from _ptr. 
  // the selector of the following union <u>
  mUINT32       _ptr_is_pointer:1;
  mUINT32       _ptr_is_aux_id :1;
  mUINT32       _ptr_is_coderep_id:1;

  PT_ATTR       _attr;		   // PT_ATTR attributes
  INT64         _byte_ofst;        // offset from base    -- controlled by _{i}ofst_kind  
  UINT64        _byte_size;        // size of access      -- controlled by _{i}ofst_kind
  ST           *_base;             // base symbol         -- controlled by _base_kind
  ST           *_based_sym;        // for restricted pointer, Fortran ref-param ...
  union {
    ST         *_ptr;              // pointer for indirect access
    AUX_ID      _aux_id;           // AUX_ID of the pointer
    INT32       _coderep_id;       // CODEREP of the pointer
  } u;  // the name "u" is actually not necessary
  VER_ID        _ptr_ver;          // the version of _ptr
  IDTYPE        _alias_class;      // which equivalence class this
				   // memop is in, according to per-PU
				   // analysis
  IDTYPE        _ip_alias_class;   // which equivalence class this
				   // memop is in, according to
				   // whole-program analysis
  AliasTag     _alias_tag;  // tag used to query AliasAnalyzer results
};

// for alias classification
const IDTYPE OPTIMISTIC_AC_ID  = 0;
const IDTYPE PESSIMISTIC_AC_ID = 1;

typedef enum {
  AR_INVALID,            
  AR_NOT_ALIAS,
  AR_POSSIBLE_ALIAS,
  AR_DEFINITE_ALIAS,
  // memops access same bytes, not necessarily same bits
  // aliased and with identical bytes can be dce'd
  AR_IDENTICAL_BYTES, 
  AR_IDENTICAL,    // access exactly the same bits.
} ALIAS_KIND_CODE ;

class ALIAS_KIND {
private:
  ALIAS_KIND_CODE _kind;

public:
  ALIAS_KIND (void) { _kind = AR_INVALID ; }
  ALIAS_KIND (const ALIAS_KIND & ak) { _kind = ak._kind; }
  ALIAS_KIND (ALIAS_KIND_CODE kind) { _kind = kind; }

  ALIAS_KIND_CODE Alias_kind (void) const { return _kind; }

  // converting to boolean
  operator BOOL () const { return _kind != AR_NOT_ALIAS; }

  BOOL Definite (void) const { 
     return _kind == AR_DEFINITE_ALIAS ||
            _kind == AR_IDENTICAL_BYTES ||
            _kind == AR_IDENTICAL ||
            _kind == AR_NOT_ALIAS;
  }
};

//  POINTS_TO is a structure to contain Points-to analysis results.
//   
//     Points-to analysis determines for the memop
//         1. base pointer 
//         2. types of memory access (e.g. to local variable, global variable, ...)
//         3. access range (ofst, size)
// 
//  !!! the fields make sense only if EXPR_IS_ADDR !!!
//
class POINTS_TO {
private:
  ALIAS_INFO    ai;
  TY_IDX        _ty;              // user-declared type for this memop
  TY_IDX        _hl_ty;
  UINT32        _field_id;
  INT32         _id;              // only used by the emitter.
  PT_MEM_ANNOT  _mem_annot; 

  // Force everyone to use Copy_non_sticky_info or Copy_fully by
  // declaring an private assignment operator and an undefined copy
  // constructor.
  POINTS_TO &operator= (const POINTS_TO &p)
    { ai = p.ai; _ty = p._ty; _id = p._id; 
      _hl_ty = p._hl_ty; _field_id = p._field_id; 
      _mem_annot = p._mem_annot; return *this; }

  POINTS_TO(const POINTS_TO &);

  void Clear_Selector (void) 
         {ai._ptr_is_pointer = ai._ptr_is_aux_id = ai._ptr_is_coderep_id = 0;}
  void Set_ptr_is_pointer (void) {
          Is_True (((BASE_KIND)ai._base_kind) != BASE_IS_FIXED, 
                   ("It is expected to be indirect load"));
          Clear_Selector (); ai._ptr_is_pointer = 1;
       }
  void Set_ptr_is_aux_id (void) { 
          Is_True (((BASE_KIND)ai._base_kind) != BASE_IS_FIXED, 
                   ("It is expected to be indirect load"));
          Clear_Selector ();ai._ptr_is_aux_id = 1; 
       }
  void Set_ptr_is_coderep_id (void) {
          Is_True (((BASE_KIND)ai._base_kind) != BASE_IS_FIXED, 
                   ("It is expected to be indirect load"));
          Clear_Selector ();ai._ptr_is_coderep_id=1;
       }
  void Reset_ptr_is_aux_id (void) { ai._ptr_is_aux_id = 0; }
  void Reset_ptr_is_pointer (void){  ai._ptr_is_pointer = 0;}
  void Reset_ptr_is_coderep_id (void) {ai._ptr_is_coderep_id=0;}

public:
  //  Member access functions
  //
  EXPR_KIND   Expr_kind(void)        const { return (EXPR_KIND) ai._expr_kind; }
  BASE_KIND   Base_kind(void)        const { return (BASE_KIND) ai._base_kind; }
  OFST_KIND   Ofst_kind(void)        const { return (OFST_KIND) ai._ofst_kind; }
  OFST_KIND   Iofst_kind(void)       const { return (OFST_KIND) ai._iofst_kind; }
  ST          *Base(void)            const { return ai._base; }

  BOOL Pointer_is_named_symbol (void) const{ return ai._ptr_is_pointer != 0; }
  BOOL Pointer_is_aux_id (void) const      { return ai._ptr_is_aux_id != 0; }
  BOOL Pointer_is_coderep_id (void) const  { return ai._ptr_is_coderep_id != 0;}
  ST   *Pointer(void) const
          { return Pointer_is_named_symbol () ? ai.u._ptr : NULL; }
  AUX_ID Pointer_aux_id(void) const
          { return Pointer_is_aux_id () ?  ai.u._aux_id : 0; }
  INT32  Pointer_coderep_id (void) const
          { return Pointer_is_coderep_id() ?  ai.u._coderep_id : 0; }
  VER_ID Pointer_ver (void)  const { 
          return Pointer_is_named_symbol () || Pointer_is_aux_id() ? ai._ptr_ver : 0; }

  // Regarding annotation 
  BOOL Has_annotation (void) const { return _mem_annot.Has_annotation (); }
  void Replace_or_add_annot (const MEMOP_ANNOT_ITEM& i) 
         { _mem_annot.Replace_or_add_annot (i);}
  void Replace_or_add_annots (MEMOP_ANNOT* annots) 
         { _mem_annot.Replace_or_add_annots (annots); }
  void Remove_annot (MEM_ANNOT_KIND kind) { _mem_annot.Remove_annot (kind); }
  PT_MEM_ANNOT& Mem_annot (void) { return _mem_annot; } 
  UINT64 Malloc_id (void) const  { return _mem_annot.Malloc_id (); }
  void Set_malloc_id (UINT64 id) { _mem_annot.Set_malloc_id (id); }
  LMV_ALIAS_GROUP LMV_alias_group (void) const 
         { return _mem_annot.LMV_alias_group (); }
  void Set_LMV_alias_group (LMV_ALIAS_GROUP grp_id) 
         { _mem_annot.Set_LMV_alias_group(grp_id);}
  // End of annotation stuff 

#if _NO_BIT_FIELDS
  mINT64      Ofst(void)             const { return ai._ofst; }
  mINT64      Size(void)             const { return ai._size; }
#else
  mINT64      Byte_Ofst(void)	     const { return ai._byte_ofst; }
  mINT64      Byte_Size(void)	     const { return ai._byte_size; }
  mUINT8      Bit_Ofst(void)	     const { return ai._bit_ofst; }
  mUINT8      Bit_Size(void)	     const { return ai._bit_size; }
#endif
  ST          *Based_sym(void)       const { return ai._based_sym; }
  UINT32      Based_sym_depth(void)  const { return ai._based_sym_depth; }
  TY_IDX      Ty(void)		     const { return _ty; }
  TY_IDX      Highlevel_Ty (void)    const { return _hl_ty; }
  UINT32      Field_id (void)        const { return _field_id; }
  INT32       Id(void)               const { return _id; }
  PT_ATTR     Attr(void)             const { return ai._attr; }
  BOOL        Not_addr_saved(void)   const { return ai._attr & PT_ATTR_NOT_ADDR_SAVED; }
  BOOL        Not_addr_passed(void)  const { return ai._attr & PT_ATTR_NOT_ADDR_PASSED; }
  BOOL        Local(void)            const { return ai._attr & PT_ATTR_LOCAL; }
  BOOL        Global(void)           const { return ai._attr & PT_ATTR_GLOBAL; }
  BOOL        Named(void)            const { return ai._attr & PT_ATTR_NAMED; }
  BOOL        Unnamed(void)          const { return !Named(); }
  BOOL        Const(void)            const { return ai._attr & PT_ATTR_CONST; }
  BOOL        Restricted(void)       const { return ai._attr & PT_ATTR_RESTRICTED; }
  BOOL        This_ptr(void)         const { return ai._attr & PT_ATTR_THIS_PTR; }
  BOOL        Unique_pt(void)        const { return ai._attr & PT_ATTR_UNIQUE_PT; }
  BOOL        F_param(void)          const { return ai._attr & PT_ATTR_F_PARAM; }
  BOOL        Dedicated(void)        const { return ai._attr & PT_ATTR_DEDICATED; }
  BOOL        No_alias(void)         const { return ai._attr & PT_ATTR_NO_ALIAS; }
  BOOL        Weak(void)             const { return ai._attr & PT_ATTR_WEAK; }
  BOOL        Weak_base(void)        const { return ai._attr & PT_ATTR_WEAK_BASE; }
  BOOL        Is_pointer(void)       const { return ai._attr & PT_ATTR_IS_POINTER; }
  BOOL        Safe_to_speculate(void)        const { return ai._attr & PT_ATTR_SAFE_TO_SPECULATE; }
  BOOL        Not_auto(void)         const { return ai._attr & PT_ATTR_NOT_AUTO; }
  BOOL        Formal(void)           const { return ai._attr & PT_ATTR_FORMAL; }
  BOOL        Default_vsym(void)     const { return ai._attr & PT_ATTR_DEFAULT_VSYM; }
  BOOL        Known_f90_pointer(void) const
    { return ai._attr & PT_ATTR_F90_POINTER; }
  BOOL        Known_not_f90_pointer(void) const
    { return ai._attr & PT_ATTR_NOT_F90_POINTER; }
  BOOL        Not_f90_target(void)   const { return ai._attr & PT_ATTR_NOT_F90_TARGET; }
  BOOL        Not_alloca_mem(void)   const { return ai._attr & PT_ATTR_NOT_ALLOCA_MEM; }
  BOOL        Extended(void)         const { return ai._attr & PT_ATTR_EXTENDED; }
#ifdef KEY
  BOOL        Is_field(void)         const { return ai._attr & PT_ATTR_FIELD; }
#endif
  BOOL        Is_array(void)         const { return ai._attr & PT_ATTR_ARRAY; }


  //  Set members
  //
  void Set_expr_kind(EXPR_KIND expr_kind) { ai._expr_kind = expr_kind; }
  void Set_base_kind(BASE_KIND base_kind) { ai._base_kind = base_kind; }
  void Set_ofst_kind(OFST_KIND ofst_kind) { ai._ofst_kind = ofst_kind; }
  void Set_iofst_kind(OFST_KIND iofst_kind) { ai._iofst_kind = iofst_kind; }
  void Set_unused()			  { ai._unused = 0; }
  void Set_base(ST *base)                 { ai._base = base; }
  void Set_byte_ofst(mINT64 ofst)         { ai._byte_ofst = ofst; }
  void Set_byte_size(mINT64 size)         { ai._byte_size = size; }
  void Set_bit_ofst_size(mUINT8 ofst, mUINT8 size) {
      ai._bit_ofst = ofst;
      ai._bit_size = size;
  }

  void Set_pointer (ST* ptr) {Set_ptr_is_pointer(); ai.u._ptr = ptr; }
  void Set_pointer_as_aux_id (AUX_ID id) {Set_ptr_is_aux_id(); ai.u._aux_id = id;}
  void Set_pointer_as_coderep_id (INT32 id) {
         Set_ptr_is_coderep_id (); ai.u._coderep_id = id;
       }
  void Set_pointer_ver (VER_ID ver)  {
         Is_True (!Pointer_is_coderep_id (),
                  ("The version does not make sense for coderep"));
         ai._ptr_ver = ver;
       }

  void Set_based_sym(ST *sym)             { ai._based_sym = sym; }
  void Set_based_sym_depth(UINT32 d)      { ai._based_sym_depth = (d > 7) ? 7 : d; }
  void Set_alias_class(const IDTYPE alias_class)
    {
      if (alias_class <= WOPT_Alias_Class_Limit) {
	ai._alias_class = alias_class;
      }
      else {
	ai._alias_class = PESSIMISTIC_AC_ID;
      }
    }
  void Set_ip_alias_class(const IDTYPE iac)
    {
      if (iac <= WOPT_Ip_Alias_Class_Limit) {
	ai._ip_alias_class = iac;
      }
      else {
	ai._ip_alias_class = PESSIMISTIC_AC_ID;
      }
    }
  void Set_alias_tag(const AliasTag tag) { ai._alias_tag = tag; }

  void Set_ty(TY_IDX ty)                  { _ty = ty; }
  void Set_hl_ty(TY_IDX hlty)             { _hl_ty = hlty; }
  void Set_field_id (UINT32 fldid)        { _field_id = fldid; }
  void Set_id(INT32 id)                   { _id = id; }
  void Set_attr(PT_ATTR attr)             { ai._attr = attr; }
  void Set_not_addr_saved(void)           { ai._attr = (PT_ATTR) (ai._attr | PT_ATTR_NOT_ADDR_SAVED); }
  void Set_not_addr_passed(void)          { ai._attr = (PT_ATTR) (ai._attr | PT_ATTR_NOT_ADDR_PASSED); }
  void Set_local(void)                    { ai._attr = (PT_ATTR) (ai._attr | PT_ATTR_LOCAL); }
  void Set_global(void)                   { ai._attr = (PT_ATTR) (ai._attr | PT_ATTR_GLOBAL); }
  void Set_named(void)                    { ai._attr = (PT_ATTR) (ai._attr | PT_ATTR_NAMED); }
  void Set_const(void)                    { ai._attr = (PT_ATTR) (ai._attr | PT_ATTR_CONST); }
  void Set_restricted(void)               { ai._attr = (PT_ATTR) (ai._attr | PT_ATTR_RESTRICTED); }
  void Set_unique_pt(void)                { ai._attr = (PT_ATTR) (ai._attr | PT_ATTR_UNIQUE_PT); }
  void Set_F_param(void)                  { ai._attr = (PT_ATTR) (ai._attr | PT_ATTR_F_PARAM); }
  void Set_dedicated(void)                { ai._attr = (PT_ATTR) (ai._attr | PT_ATTR_DEDICATED); }
  void Set_no_alias(void)                 { ai._attr = (PT_ATTR) (ai._attr | PT_ATTR_NO_ALIAS); }
  void Set_weak(void)                     { ai._attr = (PT_ATTR) (ai._attr | PT_ATTR_WEAK); }
  void Set_weak_base(void)                { ai._attr = (PT_ATTR) (ai._attr | PT_ATTR_WEAK_BASE); }
  void Set_is_pointer(void)               { ai._attr = (PT_ATTR) (ai._attr | PT_ATTR_IS_POINTER); }
  void Set_safe_to_speculate(void)                { ai._attr = (PT_ATTR) (ai._attr | PT_ATTR_SAFE_TO_SPECULATE); }
  void Set_not_auto(void)                 { ai._attr = (PT_ATTR) (ai._attr | PT_ATTR_NOT_AUTO); }
  void Set_formal(void)                   { ai._attr = (PT_ATTR) (ai._attr | PT_ATTR_FORMAL); }
  void Set_default_vsym(void)             { ai._attr = (PT_ATTR) (ai._attr | PT_ATTR_DEFAULT_VSYM); }
  void Set_known_f90_pointer(void)
    {
      Is_True(!Known_not_f90_pointer(),
	      ("POINTS_TO: f90_pointer attributes inconsistent"));
      ai._attr = (PT_ATTR) (ai._attr | PT_ATTR_F90_POINTER);
    }
  void Set_known_not_f90_pointer(void)
    {
      Is_True(!Known_f90_pointer(),
	      ("POINTS_TO: f90_pointer attributes inconsistent"));
      ai._attr = (PT_ATTR) (ai._attr | PT_ATTR_NOT_F90_POINTER);
    }
  void Set_not_f90_target(void)           { ai._attr = (PT_ATTR) (ai._attr | PT_ATTR_NOT_F90_TARGET); }
  void Set_not_alloca_mem(void)           { ai._attr = (PT_ATTR) (ai._attr | PT_ATTR_NOT_ALLOCA_MEM); }
  void Set_extended(void)                 { ai._attr = (PT_ATTR) (ai._attr | PT_ATTR_EXTENDED); }
#ifdef KEY
  void Set_is_field(void)                 { ai._attr = (PT_ATTR) (ai._attr | PT_ATTR_FIELD); }
#endif
  void Set_is_array(void)                 { ai._attr = (PT_ATTR) (ai._attr | PT_ATTR_ARRAY); }

  void Reset_attr(void)                   { ai._attr = PT_ATTR_NONE; }
  void Reset_not_addr_saved(void)         { ai._attr = (PT_ATTR) (ai._attr & ~PT_ATTR_NOT_ADDR_SAVED); }
  void Reset_not_addr_passed(void)        { ai._attr = (PT_ATTR) (ai._attr & ~PT_ATTR_NOT_ADDR_PASSED); }
  void Reset_local(void)                  { ai._attr = (PT_ATTR) (ai._attr & ~PT_ATTR_LOCAL); }
  void Reset_global(void)                 { ai._attr = (PT_ATTR) (ai._attr & ~PT_ATTR_GLOBAL); }
  void Reset_named(void)                  { ai._attr = (PT_ATTR) (ai._attr & ~PT_ATTR_NAMED); }
  void Reset_const(void)                  { ai._attr = (PT_ATTR) (ai._attr & ~PT_ATTR_CONST); }
  void Reset_restricted(void)             { ai._attr = (PT_ATTR) (ai._attr & ~PT_ATTR_RESTRICTED); }
  void Reset_unique_pt(void)              { ai._attr = (PT_ATTR) (ai._attr & ~PT_ATTR_UNIQUE_PT); }
  void Reset_F_param(void)                { ai._attr = (PT_ATTR) (ai._attr & ~PT_ATTR_F_PARAM); }
  void Reset_dedicated(void)              { ai._attr = (PT_ATTR) (ai._attr & ~PT_ATTR_DEDICATED); }
  void Reset_no_alias(void)               { ai._attr = (PT_ATTR) (ai._attr & ~PT_ATTR_NO_ALIAS); }
  void Reset_weak(void)                   { ai._attr = (PT_ATTR) (ai._attr & ~PT_ATTR_WEAK); }
  void Reset_weak_base(void)              { ai._attr = (PT_ATTR) (ai._attr & ~PT_ATTR_WEAK_BASE); }
  void Reset_is_pointer(void)             { ai._attr = (PT_ATTR) (ai._attr & ~PT_ATTR_IS_POINTER); }
  void Reset_safe_to_speculate(void)              { ai._attr = (PT_ATTR) (ai._attr & ~PT_ATTR_SAFE_TO_SPECULATE); }
  void Reset_not_auto(void)               { ai._attr = (PT_ATTR) (ai._attr & ~PT_ATTR_NOT_AUTO); }
  void Reset_formal(void)                 { ai._attr = (PT_ATTR) (ai._attr & ~PT_ATTR_FORMAL); }
  void Reset_default_vsym(void)           { ai._attr = (PT_ATTR) (ai._attr & ~PT_ATTR_DEFAULT_VSYM); }
  void Reset_known_f90_pointer(void)      { ai._attr = (PT_ATTR) (ai._attr & ~PT_ATTR_F90_POINTER); }
  void Reset_known_not_f90_pointer(void)  { ai._attr = (PT_ATTR) (ai._attr & ~PT_ATTR_NOT_F90_POINTER); }
  void Reset_not_f90_target(void)         { ai._attr = (PT_ATTR) (ai._attr & ~PT_ATTR_NOT_F90_TARGET); }
  void Reset_not_alloca_mem(void)         { ai._attr = (PT_ATTR) (ai._attr & ~PT_ATTR_NOT_ALLOCA_MEM); }
  void Reset_extended(void)               { ai._attr = (PT_ATTR) (ai._attr & ~PT_ATTR_EXTENDED); }

  void Invalidate_ptr_info (void) {
         Clear_Selector ();
         ai.u._ptr = NULL; ai._iofst_kind = OFST_IS_INVALID; ai._ptr_ver = (VER_ID)0; 
       }
#ifdef KEY
  void Reset_is_field(void) { ai._attr = (PT_ATTR) (ai._attr & ~PT_ATTR_FIELD); }
#endif
  void Reset_is_array(void) { ai._attr = (PT_ATTR) (ai._attr & ~PT_ATTR_ARRAY); }

  void Init(void) {
    //  Set fields in POINTS_TO to invalid for error detection.
    Set_expr_kind(EXPR_IS_INVALID);
    Set_base_kind(BASE_IS_INVALID);
    Set_ofst_kind(OFST_IS_INVALID);
    Set_iofst_kind(OFST_IS_INVALID);
    Set_unused();
    Set_based_sym_depth(0);
    Set_base((ST*)NULL);
    Invalidate_ptr_info ();
    Set_byte_ofst(0);
    Set_byte_size(0);
    Set_bit_ofst_size(0,0);
    Set_based_sym((ST*)NULL);
    Set_ty(0);
    Set_hl_ty(0);
    Set_field_id(0);
    Set_id(0);
    Set_alias_class(OPTIMISTIC_AC_ID);
    Set_ip_alias_class(OPTIMISTIC_AC_ID);
    Set_alias_tag(EmptyAliasTag);
    _mem_annot.Init();
    // The default attributes: 
    Set_attr(PT_ATTR_NONE);
  }

#if defined(TARG_SL)
  ST* Get_ST_base(ST* st) const;
#endif
  //  Return TRUE if bases are the same.   Return FALSE if don't know.
  BOOL Same_base(const POINTS_TO *) const;
  
  // Return TRUE if the two POINTS_TO share the same indirect base.
  BOOL Same_pointer (const POINTS_TO*) const;

  // Return TRUE if indirect base information helps in disambiguation 
  BOOL Pointer_info_does_help (void) const;

  //  Return TRUE if bases are different.  Return FALSE if don't know.
  BOOL Different_base(const POINTS_TO *) const;

  //  Check whether offset overlaps.  Their bases must be the same.
  ALIAS_KIND Overlap(const POINTS_TO *) const;

  // Check if PTs are similar, used in creating region bounds and rejecting
  // duplicates
  BOOL Similar(const POINTS_TO *pt) const {
    if (Expr_kind() == pt->Expr_kind() &&
	Base_kind() == pt->Base_kind() &&
	Ofst_kind() == pt->Ofst_kind() &&
	Base_kind() == BASE_IS_FIXED && 
	Ofst_kind() == OFST_IS_FIXED &&
	Base() == pt->Base() &&
	Byte_Ofst() == pt->Byte_Ofst() &&
	Byte_Size() == pt->Byte_Size())
      return TRUE;

    if ((F_param() && pt->F_param()) &&
	(Based_sym() == pt->Based_sym()))
      return TRUE;
  
    return FALSE;
  }

  IDTYPE Alias_class(void) const { return ai._alias_class; }

  IDTYPE Ip_alias_class(void) const { return ai._ip_alias_class; }

  AliasTag Alias_tag(void) const { return ai._alias_tag; }

  void Shift_ofst(mINT64 shift)   // shift offset by that amount
    { Set_byte_ofst( Byte_Ofst() + shift ); }

  //  Return TRUE if base is fixed.
  BOOL Base_is_fixed(void) const 
    { return Base_kind() == BASE_IS_FIXED; }

  BOOL Int_is_constant(void) const 
    { return (Ofst_kind() == OFST_IS_FIXED); }  // overloaded ofst_kind
  
  mINT64 Int_const_val(void) const
    { return Byte_Ofst(); }

  void Set_const_val(mINT64 val) 
    { Set_byte_ofst(val); Set_ofst_kind(OFST_IS_FIXED); }

  // Merge the information from alias classification for two POINTS_TO's
  void Meet_info_from_alias_class(const POINTS_TO *);

  // Merge the information from the alias tags for two POINTS_TO's
  void Meet_alias_tag(const POINTS_TO *, AliasAnalyzer *);

  // Merge two points to information
  void Meet(const POINTS_TO *, ST *);

  // Copy a POINTS_TO structure
  void Copy_fully(const POINTS_TO *p)
    { *this = *p; }

  void Copy_fully(const POINTS_TO &p)
    { *this = p; }

  // Copy a POINTS_TO structure except for its sticky parts (i.e., the
  // parts related to unique_pt and restrict.
  void Copy_non_sticky_info(const POINTS_TO *p)  
    {
      BOOL  is_unique_pt  = Unique_pt();
      BOOL  is_restricted = Restricted();
      ST   *based_sym     = Based_sym();
      PT_MEM_ANNOT mem_annot = Mem_annot();

      *this = *p;
      if (is_unique_pt) {
	Set_unique_pt();
	Set_based_sym(based_sym);
      }
      if (is_restricted) {
	Set_restricted();
	Set_based_sym(based_sym);
      }
      _mem_annot = mem_annot;
    }

  void Copy_pointer_info (const POINTS_TO* p) 
    {
      Clear_Selector ();
      if (p->Pointer_is_named_symbol()) {
        Set_pointer (p->Pointer ());
        Set_pointer_ver (p->Pointer_ver());
      } else if (p->Pointer_is_aux_id()) {
        Set_pointer_as_aux_id (p->Pointer_aux_id ());
        Set_pointer_ver (p->Pointer_ver());
      } else if (p->Pointer_is_coderep_id ()) {
        Set_pointer_as_coderep_id (p->Pointer_coderep_id());
      }
    }


  void Copy_indirect_access_info (const POINTS_TO* p) 
    {
      Copy_pointer_info (p);
      Set_iofst_kind (p->Iofst_kind ());
      Set_byte_size (p->Byte_Size ());
      Set_bit_ofst_size (p->Bit_Ofst (), p->Bit_Size ());
    }

  // Copy a POINTS_TO structure except for its sticky parts (i.e., the
  // parts related to unique_pt and restrict.
  void Copy_non_sticky_info(const POINTS_TO &p)
    {
      BOOL  is_unique_pt  = Unique_pt();
      BOOL  is_restricted = Restricted();
      ST   *based_sym     = Based_sym();
      *this = p;
      if (is_unique_pt) {
	Set_unique_pt();
	Set_based_sym(based_sym);
      }
      if (is_restricted) {
	Set_restricted();
	Set_based_sym(based_sym);
      }
    }

  // Compute the POINTS_TO info for an ST
  void Analyze_ST(ST *st, INT64 byte_ofst, INT64 byte_size, UINT8 bit_ofst, 
		  UINT8 bit_size, TY_IDX ty, BOOL has_equiv);

  void Analyze_ST_as_base(ST *, INT64, TY_IDX);

  void Analyze_WN_expr(WN *);
  
#ifdef __cplusplus
  template <class SYMTAB> void Analyze_Ldid_Base(WN *wn, const SYMTAB &symtab);
  template <class SYMTAB> void Analyze_Parameter_Base(WN *wn, const SYMTAB &symtab);
  template <class SYMTAB> void Analyze_WN_expr(WN *wn, const SYMTAB &symtab);
#endif

  void Analyze_Lda_Base(WN *, const OPT_STAB &);
  void Analyze_Lda_Base(WN *, const STAB_ADAPTER &);
  void Lower_to_base(WN *);

  POINTS_TO() {}
  POINTS_TO(ST *st, BOOL indirect = FALSE) { 
    Init(); 
    if (indirect) 
      Analyze_ST_as_base(st, 0, 0);
    else
      Analyze_ST(st, 0, ST_size(st), 0, 0, 0,  TRUE); 
    if (Byte_Size() == 0)
      Set_ofst_kind(OFST_IS_UNKNOWN);
  }
  POINTS_TO(ST *st, INT64 ofst, INT64 size, BOOL indirect = FALSE) {
    Init(); 
    if (indirect) 
      Analyze_ST_as_base(st, ofst, 0);
    else
      Analyze_ST(st, ofst, size, 0, 0, 0,  TRUE); 
    if (Byte_Size() == 0)
      Set_ofst_kind(OFST_IS_UNKNOWN);
  }
  POINTS_TO(ST* st, INT64 byte_ofst, INT64 byte_size, UINT8 bit_ofst,
	    UINT8 bit_size, BOOL indirect = FALSE) {
    Init();
    if (indirect) 
      Analyze_ST_as_base(st, byte_ofst, 0);
    else
      Analyze_ST(st, byte_ofst, byte_size, bit_ofst, bit_size, 0,  TRUE); 
    if (Byte_Size() == 0)
      Set_ofst_kind(OFST_IS_UNKNOWN);
  }

  // Print (for debugging)
  void Print(FILE *fp=stderr) const;
};

class PT_MEMOP_ANNOT_STIKER {
private:
  BOOL _has_annot; 
  POINTS_TO* _pt;
  PT_MEM_ANNOT _mem_annot;

public:
  PT_MEMOP_ANNOT_STIKER (POINTS_TO* pt) { 
    if (_has_annot = pt->Has_annotation()) {
      _mem_annot = pt->Mem_annot () ;
      _pt = pt;
    }
  }

  ~PT_MEMOP_ANNOT_STIKER (void) {
    if (_has_annot) {
      if (_mem_annot.Item_is_inlined ())
        _pt->Replace_or_add_annot (_mem_annot.Get_inlined_item ());
      else 
        _pt->Replace_or_add_annots (_mem_annot.Get_annots_ptr ());
    }
  }
};

//  POINTS_TO_NODE:  contains the points_to item
//
class POINTS_TO_NODE : public SLIST_NODE {
  DECLARE_SLIST_NODE_CLASS(POINTS_TO_NODE)
private:
  POINTS_TO *_pt;
  POINTS_TO_NODE(void);
  POINTS_TO_NODE(const POINTS_TO_NODE&);
  POINTS_TO_NODE& operator = (const POINTS_TO_NODE&);
public:
  POINTS_TO_NODE(POINTS_TO *pt)		{ _pt = pt; }
  ~POINTS_TO_NODE(void)			{}
  POINTS_TO *Pt(void) const		{ return _pt; }
};


//  POINTS_TO_LIST is a internal linked list
//
class POINTS_TO_LIST : public SLIST {
  DECLARE_SLIST_CLASS (POINTS_TO_LIST, POINTS_TO_NODE)
private:
  POINTS_TO_LIST(const POINTS_TO_LIST&);
  POINTS_TO_LIST& operator = (const POINTS_TO_LIST&);
public:
  void Prepend( POINTS_TO *pt, MEM_POOL *pool );
};


class POINTS_TO_ITER : public SLIST_ITER {
private:
  DECLARE_SLIST_ITER_CLASS (POINTS_TO_ITER, POINTS_TO_NODE, POINTS_TO_LIST)
  POINTS_TO_ITER(const POINTS_TO_ITER&);
  POINTS_TO_ITER& operator = (const POINTS_TO_ITER&);
public:
  ~POINTS_TO_ITER(void)		{}
  POINTS_TO_NODE *First_elem(void)	{ return First(); }
  POINTS_TO_NODE *Next_elem(void)	{ return Next();  }
};


#ifdef Is_True_On
extern void CHECK_POINTS_TO(POINTS_TO *);
#else
#define CHECK_POINTS_TO(pt)
#endif

//  Lower ST into the <base,ofst> form
void Expand_ST_into_base_and_ofst(ST *st, INT64 st_ofst, ST ** base, INT64 *ofst);

template <class SYMTAB> WN *Find_addr_recur(WN *, const SYMTAB &);

inline static BOOL Is_FORTRAN()
{
    const PU& pu = Get_Current_PU ();
    return (PU_f77_lang (pu) || PU_f90_lang (pu));
}

template <class SYMTAB>
ST *Is_nested_call(const WN *wn, const SYMTAB &symtab);

extern ST *Is_nested_call(const WN *wn);

// Implementation stuff originally from opt_points_to.cxx, since g++ (rightly)
// doesn't do implicit inclusion of .cxx files.


template <class SYMTAB>
void POINTS_TO::Analyze_Ldid_Base(WN *wn_ldid, const SYMTAB &stab)
{
  ST *st = stab.St_ptr(wn_ldid);
  INT64 ofst = WN_offset(wn_ldid);
  TY_IDX ty = WN_ty(wn_ldid);
  Analyze_ST_as_base(st, ofst, ty);
  Set_ofst_kind(OFST_IS_UNKNOWN);
}

// Look for a LDA/LDID node recursively 
template <class SYMTAB>
WN *Find_addr_recur(WN *wn, const SYMTAB &stab)
{
  if (wn == NULL) return NULL;
  switch (WN_operator(wn)) {
#if defined(TARG_SL) 
  case OPR_INTRINSIC_OP:
    if(INTRN_copy_addr(WN_intrinsic(wn)))
      return Find_addr_recur(WN_kid0(WN_kid0(wn)),stab);
    return NULL;
#endif

  case OPR_PARM:
    if ((WN_Parm_By_Reference(wn) || WN_Parm_Dereference(wn)) && WN_kid_count(wn))
      return Find_addr_recur(WN_kid0(wn), stab);		
    // otherwise, there is no address expression
    return NULL;

  case OPR_LDA:
    return wn;
  case OPR_LDID:
    { 
      ST *st = stab.St_ptr(wn);
      if (Is_FORTRAN() && ST_sclass(st) == SCLASS_FORMAL &&
	  ! ST_is_value_parm(st))
	return wn;
      if (ST_pt_to_unique_mem(st)) {
	TY_IDX ty = WN_ty(wn);
	if (TY_kind(Ty_Table[ty]) != KIND_POINTER) {
	  TY_IDX new_ty = Make_Pointer_Type(ty);
	  WN_set_ty(wn, new_ty);
	  // Detect problem shown up in PV 652586.
	  DevWarn("Fixing TY %s of LDID <%s> to a pointer type because the ST has PT_TO_UNIQUE_MEM set",  
		  TY_name(ty) ? TY_name(ty) : "noname",
		  ST_name(st) ? ST_name(st) : "noname");
	}
	return wn;
      }
      TY_IDX ty = WN_ty(wn);
      if (TY_kind(Ty_Table[ty]) == KIND_POINTER)
	return wn;
      Is_True(!TY_is_restrict(ty),
	      ("__restrict object must be KIND_POINTER"));
      return NULL;
    }
  case OPR_ARRAY:
    return Find_addr_recur(WN_kid0(wn), stab);
  case OPR_ADD:
    { 
      WN *ret_wn;
      for (INT32 i = 0; i < WN_kid_count(wn); i++)
	if ((ret_wn = Find_addr_recur(WN_kid(wn,i), stab)) != NULL)
	  return ret_wn;
      return NULL;
    }
  case OPR_SUB:
    return Find_addr_recur(WN_kid(wn,0), stab);
  default:
    return NULL;
  }
}


template <class SYMTAB>
void POINTS_TO::Analyze_Parameter_Base(WN *wn, const SYMTAB &stab)
{
  Set_expr_kind(EXPR_IS_ADDR);
  Set_base_kind(BASE_IS_UNKNOWN);
  Set_ofst_kind(OFST_IS_UNKNOWN);
  WN *wn_lda;
  switch (WN_operator(wn)) {
  case OPR_LDA:
    Analyze_Lda_Base(wn, stab);
    Lower_to_base(NULL);
    break;
  case OPR_LDID:
    Analyze_Ldid_Base(wn, stab);
    Set_ofst_kind(OFST_IS_UNKNOWN);
    break;
    
  case OPR_ARRAY:
  default:  // no information
    wn_lda = Find_addr_recur(wn, stab);
    if (wn_lda != NULL) {
      if (WN_operator(wn_lda) == OPR_LDA)  {
	Analyze_Lda_Base(wn_lda, stab);
	Lower_to_base(NULL);
      } else if (WN_operator(wn_lda) == OPR_LDID) {
	Analyze_Ldid_Base(wn_lda, stab);
	Lower_to_base(NULL);
	// ignore the range.  Passing A[i] does not mean only passing one element.
      }
    }
    break;
  } // switch
}

//  Templatize symbol table
template <class SYMTAB>
void POINTS_TO::Analyze_WN_expr(WN *wn, const SYMTAB &stab)
{
  OPERATOR opr = WN_operator(wn);
  switch ( opr ) {
  case OPR_LDID:
  case OPR_STID:
  case OPR_LDBITS:
  case OPR_STBITS:
    {
      ST *st = stab.St_ptr(wn);
      if (ST_sclass(st) != SCLASS_UNKNOWN) {
	TY_IDX ty = ST_type(st);
	INT64 ofst = WN_offset(wn);
	INT64 size = MTYPE_size_min(WN_desc(wn)) >> 3;
	if (opr == OPR_LDBITS || opr == OPR_STBITS)
	    Analyze_ST (st, ofst, size, WN_bit_offset (wn), WN_bit_size
			(wn), ty, TRUE /* has equiv */);
	else
	    Analyze_ST(st, ofst, size, 0, 0, ty, TRUE /* assume has equiv */);
	return;
      }
    }
    break;
  case OPR_ILDBITS:
  case OPR_ISTBITS:
  case OPR_ILOAD:
  case OPR_MLOAD:
  case OPR_PARM:
  case OPR_ISTORE:
  case OPR_MSTORE:
    { 
      WN *wn_lda = Find_addr_recur(
	OPERATOR_is_store(opr) ? WN_kid1(wn) : WN_kid0(wn), stab);
      if (wn_lda != NULL) {
	if (WN_operator(wn_lda) == OPR_LDA)  {
	  ST *st = stab.St_ptr(wn_lda);
	  if (ST_sclass(st) != SCLASS_UNKNOWN) {
	    Analyze_Lda_Base(wn_lda, stab);
	    Lower_to_base(NULL);
	    return;
	  }
	} else if (WN_operator(wn_lda) == OPR_LDID) {
	  Analyze_Ldid_Base(wn_lda, stab);
	  Lower_to_base(NULL);
	  return;
	}
      }
    }
    break;
  default:
    break;
  } // switch
  Init();   
}

// returns NULL if not a nested call
// global function, can be called with symtab or aux_stab
// used by OPT_STAB::Add_nested_call_mu_chi and REGION_BOUNDS:grb
template <class SYMTAB>
ST *Is_nested_call(const WN *wn, const SYMTAB &symtab)
{
  ST *call_st = NULL;
  INT32 num_parms = WN_kid_count(wn);
  OPERATOR opr = WN_operator(wn);
  if (opr == OPR_CALL) {
    call_st = WN_st(wn);
  } else if (opr == OPR_ICALL)
    num_parms--;

  // nested procedure?
  // SPECIAL CASE:  Check if any of the parameters is an LDA of
  // a nested function.  If so, we consider this to also be a call
  // to that function.  Only do this for fortran because that's the
  // only language so far that supports nested functions.
  ST *nested_func = NULL;

  for ( INT parmkid = 0; parmkid < num_parms; parmkid++ ) {
    WN *parm = WN_kid(wn, parmkid);
    if ( WN_operator(parm) == OPR_PARM ) {
      WN *actual_parm = WN_kid0(parm);
      if ( WN_operator(actual_parm) == OPR_LDA ) {
	// tricky:
	// for stab == OPT_STAB it is
	//	OPT_STAB::St_idx --> aux_stab[WN_aux(wn)].St()
	// for stab == STAB_ADAPTER, it is
	//	STAB_ADAPTER::St_idx --> WN_st(wn)
	ST *lda_st = symtab.St_ptr(actual_parm);
	if ( ST_class(lda_st) == CLASS_FUNC &&
	    PU_is_nested_func (Pu_Table[ST_pu (lda_st)])
	    )
	  nested_func = lda_st;
      }
    }
  }

  if ( nested_func == NULL && call_st != NULL &&
      PU_is_nested_func (Pu_Table[ST_pu (call_st)])
      )
    nested_func = call_st;

  return nested_func;
}

#endif // opt_points_to_INCLUDED
