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


#ifndef _LEGO_PRAGMA_INCLUDED_
#define _LEGO_PRAGMA_INCLUDED_

/***********************************************************************
 *
 * Exported Types
 * ==============
 *
 *  class DISTR_DIM
 *      Store information about a single dimension of a distributed array.
 *
 *
 * Exported Methods
 * ----------------
 * 
 *  DISTR_DIM ()
 *      constructor
 *
 *  ~DISTR_DIM ()
 *      destructor
 *
 *  BOOL operator ==(const DISTR_DIM& test_dim) const;
 *      Compare two distributions, return true if they are the same
 *      (including chunksize in cyclic(k)).
 *
 *  BOOL CyclicOne ()
 *      Return TRUE if cyclic(1), FALSE otherwise.
 *
 *  DISTRIBUTE_TYPE   Distr_Type()
 *      Return the type of distribution.
 *
 *  INT64 Chunk_Const_Val()
 *      Return the chunksize (must be constant)
 *
 *  WN *Chunk_WN()
 *      Return the original WHIRL expression for the chunksize.
 *
 *  WN *Chunksize()
 *      Return a new copy of the WHIRL expression for the chunksize.
 *
 *  void Evaluate_Chunk_Into_Symbol (SYMBOL* s, WN* stid_wn)
 *      Given an STID that evaluates the chunksize expr into the 
 *      symbol pointer, store the symbols and stid_wn for subsequent DU-info.
 *
 *  void Init_Block_Star (DISTRIBUTE_TYPE dt);
 *      Initialize routine for block/star distributions.
 *
 *  void Init_Cyclic_Const (DISTRIBUTE_TYPE dt, INT64 const_val, WN* pwn)
 *      Initialize routines for cyclic_const
 *
 *  void Init_Cyclic_Expr (DISTRIBUTE_TYPE dt, WN* val_wn)
 *      Initialize routine for cyclic_expr
 *
 *  void Print (FILE* fp)
 *      Standard print routine.
 *
 *
 * Exported Type
 * =============
 *
 *  class DISTR_ARRAY
 *      Store all the information for a single data distribution pragma.
 *
 * Exported Methods
 * ----------------
 *
 *  DISTR_ARRAY ()
 *  DISTR_ARRAY (DISTR_DIM* dims, WN *first_pragma, WN *last_pragma, 
 *               WN** bounds_wns)
 *      Constructors.
 *
 *  ~DISTR_ARRAY ()
 *      Destructor.
 *
 *  DISTR_DIM *Dims()
 *      Return a pointer to this arrays distributed dimensions info.
 *
 *  DISTR_DIM *Get_Dim (INT i)
 *      Return a pointer this arrays distributed dimension, "i"
 *
 *  INT Num_Distr_Dim ()
 *      Return the number of distributed dimensions in this array.
 *
 *  WN* First_Pragma_WN ()
 *      Return a pointer to the first pragma node.
 *
 *  WN* Last_Pragma_WN  ()
 *      Return a pointer to the last pragma node.
 *
 *  mBOOL Is_Compiler_Generated ()
 *      Return TRUE if these pragmas were compiler-generated,
 *      FALSE otherwise.
 *
 *  WN* void Set_Last_Pragma_WN (WN* lwn)
 *      Store the pointer to the last pragma node.
 *
 *  WN* Array_Size_WN  (INT dim)
 *      Return (a cloned tree of) the expression 
 *      for the size of dimension dim of the array.
 *
 *  BOOL Has_Onto ()
 *      Return TRUE if this distribution had an ONTO clause, FALSE otherwise.
 *
 *  INT64 Onto(INT32 i)
 *      Return the size of the onto value in dimension "i"
 *
 *  DISTR_INFO* Dinfo()
 *  void Set_Dinfo(DISTR_INFO *dinfo)
 *      Routines to get and set the dinfo.
 *
 *  mBOOL DACT_Equiv(DISTR_ARRAY *test_dact, INT32 my_dim, INT32 test_dim)
 *      Compare "this" and the given dact in the specified dimensions.
 *      Return TRUE if the distributions are the same, and the array
 *      bounds are equivalent.
 *
 *  mBOOL DACT_Equiv(DISTR_ARRAY *test_dact)
 *      Return TRUE if "this" and "test_dact" are equiv in all dimensions.
 *
 *  void Convert_Expr_To_Symbol ()
 *      For each distribute_cyclic_expr, evaluate the expr into a
 *      local symbol, and replace the WHIRL tree in DISTR_DIM with the sym.
 *
 *  WN *Chunksize (INT i)
 *      Return a usable whirl expression for the chunksize in dimension "i".
 *
 *  void Print (FILE* fp) const
 *      Standard Print method.
 *
 * 
 *
 * Exported Type
 * =============
 *
 *  class DISTR_INFO
 *      Store all the data distribution pragmas for an array.
 *
 * Exported Methods
 * ----------------
 *
 *  DISTR_INFO (mBOOL isreshaped, INT numdim, SYMBOL* array)
 *      Constructor
 *
 *  ~DISTR_INFO ()
 *      Destructor
 *
 *  mBOOL IsReshaped()
 *      Return TRUE if array is reshaped.
 *
 *  mBOOL IsDynamic()
 *      Return TRUE if dynamic.
 *
 *  void Set_Dynamic()
 *      Set dynamic.
 *
 *  INT Num_Dim()
 *      Return number of dimensions in the array.
 *
 *  SYMBOL* Array()
 *      Return SYMBOL* for the distributed array.
 *
 *  ST* Array_ST()
 *      Return ST* of the array.
 *
 *  ST* Dart_ST()
 *      Return ST* of the dart for the array.
 *
 *  void Find_Alloca ()
 *      For a regular distributed local adjustable-sized array,
 *      locate the alloca in the function preamble and setup
 *      DU-chains and alias info.
 *
 *  WN* Load_Distr_Array ()
 *      Load the address of a distributed array, 
 *      and update alias/DU info as required.
 *      Return an ldid or an lda of the array.
 *
 *  WN* Load_New_Distr_Array ()
 *      Load the (new) address of a reshaped distributed array.
 *      Return an ldid or an lda of the array.
 *      Called only for reshaped arrays in a COMMON block, after
 *      array lowering.
 *      
 *  SYMBOL* Array_Common_Symbol()
 *      If a common reshaped array, return the symbol for the new array
 *      ST.
 *
 *  void Set_Array_Alias_WN (WN* wn)
 *  WN* Get_Array_Alias_WN ()
 *      Store/Fetch WHIRL node that references the array. 
 *      Use to copy-alias-info.
 *
 *  void Set_Array_Ptr_Alias_WN (WN* wn)  { _array_ptr_alias_wn = wn; }
 *  WN* Get_Array_Ptr_Alias_WN ()         { return _array_ptr_alias_wn; }
 *      Store/Fetch WHIRL node that dereferences the array.
 *      Use to copy-alias-info.
 *
 *  void Set_Array_Def_WN (WN* wn)
 *  void Add_Array_Use_WN (WN* use_wn)
 *      Store the def WN* of the array. Add uses and update DU-chains.
 *
 *  WN* Get_Array_Def_WN ()
 *      Used just for debugging
 *
 *  WN* DART_Ldid ();
 *      Return an LDID for the dart, with updated alias info and DU-chains.
 *  void DART_Stid (WN* stid_wn);
 *      Given an STID for the dart, update alias and DU info.
 *  void DART_Ptr_Ref(WN* wn)
 *      Given an ISTORE/ILOAD for the dart, update alias info.
 *      
 *  SYMBOL* Get_Dimsize (INT i)
 *  SYMBOL* Get_Numprocs (INT i)
 *      Return SYMBOLS* for dimsize/numprocs in specified dimension.
 *
 *  WN* Numprocs(INT dim);
 *  WN* Dimsize(INT dim);
 *  WN* Chunksize(INT dim); 
 *      Return an ldid of the numprocs/dimsize/chunksize
 *
 *  void Numprocs_Ldid (INT dim, WN* ldid_wn);
 *  void Numprocs_Stid (INT dim, WN* stid_wn);
 *  void Dimsize_Ldid (INT dim, WN* ldid_wn);
 *  void Dimsize_Stid (INT dim, WN* stid_wn);
 *      Routines to handle alias info and DU-chains for dimsize/numprocs.
 *
 *  WN* Get_Array_Dim_Size (INT dim);
 *      Return a WHIRL expr for the upper bound of the given dim.
 *
 *  void Add_Dact (DISTR_ARRAY* dact)
 *      Insert a dact into this dinfo.
 *
 *  INT Num_Dact ()
 *      Return the number of dacts in this dinfo.
 *
 *  DISTR_ARRAY* Get_Dact (INT i)
 *      Return dact# i.
 *
 *  void Add_Redistr (DISTR_ARRAY* redistr)
 *      Add a redistribute dact.
 *
 *  INT Num_Redistr ()
 *      Return the number of redistribute dacts in this dinfo.
 *
 *  DISTR_ARRAY* Get_Redistr (INT i)
 *      Return redistributed dact# i
 *
 *  void Add_Gen_Redistr (DISTR_ARRAY* redistr)
 *      Add a redistribute dact to those collected after LNO, during lego-gen.
 *
 *  INT Num_Redistr ()
 *      Return the number of redistribute dacts (after LNO, during lego-gen)
 *      in this dinfo.
 *
 *  DISTR_ARRAY* Get_Redistr (INT i)
 *      Return redistributed dact# i from the list after LNO, during lego-gen.
 *
 *  void Set_Orig_TY(TY_IDX ty)
 *  TY_IDX Orig_TY()
 *      Store/fetch the original TY. Useful since the original
 *      TY gets reshaped.
 *
 *  void Print (FILE* fp)
 *      Standard Print method.
 *
 *
 * Exported Type
 * =============
 *
 * class DISTR_GLOBAL_INFO
 *  Store information about global arrays
 *
 * Exported Methods
 * ----------------
 *
 *  DISTR_GLOBAL_INFO(TY_IDX ty)
 *      Constructor
 *
 *  TY_IDX Get_TY ()
 *      Return the original TY of the array.
 *
 *
 * Exported Types
 * ==============
 *
 *  typedef HASH_TABLE<ST*, DISTR_INFO*> DA_HASH_TABLE;
 *      Hash-table of ST entries and the corresponding DISTR_INFO 
 *      for the array. 
 *
 *  typedef STACK<DISTR_INFO*> DA_STACK;
 *      Stack of DISTR_INFOs in the PU. Duplicate of hash-table
 *      (i.e. same entries) but easier to walk since it's a stack.
 *
 *  typedef HASH_TABLE<ST*, DISTR_GLOBAL_INFO*> DA_GLOBAL_HASH_TABLE;
 *      Hash-table of ST entries and the corresponding DISTR_INFO 
 *      for the array, for global arrays
 *
 * Exported Functions
 * ==================
 *
 *  extern void Read_Distr_Pragmas (WN* func_nd);
 *
 *  extern WN *Read_Pragma_Distribute(WN *pwn);
 *
 *  extern WN* Read_Pragma_Redistribute (WN* pwn, BOOL gen_phase = FALSE)
 *
 *  extern WN* Load_Distr_Array (ST* array_st)
 *      Load the start address of the given array.
 *
 *  extern WN* Numprocs (ST* array_st, INT dim, BOOL inside_loop, WN** block_wn)
 *      Return a WHIRL tree for numprocs in dimension "dim" of given
 *      array, based on a dynamic runtime lookup. block_wn contains
 *      generated statement code. inside_loop tells whether the code will
 * 	be inserted within a loop. 
 *
 * Exported Variables
 * ==================
 *
 *  extern DA_HASH_TABLE *da_hash;
 *      For each ST, store all data distribution pragmas.
 *
 *  extern DA_HASH_TABLE *da_global;
 *      Store information for global distributed arrays across PU processing
 *
 ***********************************************************************/

#include "defs.h"
#include "wn.h"
#include "stab.h"
#include "cxx_hash.h"
#include "access_vector.h"
#include "wn_pragmas.h"
#include "lego.h"
#include "opt_du.h"
#include "opt_alias_interface.h"
#include "lego_util.h"
#include "lwn_util.h"

/***********************************************************************
 *
 * External declarations
 *
 ***********************************************************************/

extern DU_MANAGER *Du_Mgr;                  // PU DU manager
extern class ALIAS_MANAGER *Alias_Mgr;      // Alias manager
extern WN *Current_Func_Node;               // whirl node for current function

class DISTR_DIM {
  DISTRIBUTE_TYPE _distr_type;  /* DISTRIBUTE_STAR,
                                 * DISTRIBUTE_BLOCK,
                                 * DISTRIBUTE_CYCLIC_EXPR, or
                                 * DISTRIBUTE_CYCLIC_CONST.
                                 * (definition from wn_pragmas.h)
                                 */
  union {                       /* If cyclic then expr for chunksize. */
    INT64   const_val;          /* if int-const then store value */
    SYMBOL* sym;                /* Later replace by symbol containing value. */
  } _chunksize;
  WN* _chunk_val_wn;            /* if cyclic_expr (non-const) then expr-WN */
  WN* _chunk_stid_wn;           /* if cyclic_expr, then this stores the
                                 * stid that evaluates the expr into
                                 * the symbol. Useful for alias-info/du-chains.
                                 */

public:
  DISTR_DIM () { 
    _chunksize.const_val = 0;
    _chunk_val_wn = NULL;
    _chunk_stid_wn = NULL;
  }
  ~DISTR_DIM () {
    if (_distr_type == DISTRIBUTE_CYCLIC_EXPR)
      CXX_DELETE(_chunksize.sym, LEGO_pool);
  }
  BOOL operator ==(const DISTR_DIM& test_dim) const;

  BOOL CyclicOne () {
    if ((_distr_type == DISTRIBUTE_CYCLIC_CONST) &&
        (_chunksize.const_val == 1)) return TRUE;
    return FALSE;
  }
  DISTRIBUTE_TYPE   Distr_Type()    const { return _distr_type; }
  INT64             Chunk_Const_Val() const { return _chunksize.const_val; }
  WN                *Chunk_WN()     const { return _chunk_val_wn; }
  WN                *Chunksize();
  /*
   * s is the symbol into which the chunksize is evaluated.
   * stid_wn is the stid, which we store for alias and du-info.
   */
  void              Evaluate_Chunk_Into_Symbol (SYMBOL* s, WN* stid_wn) {
    Is_True (_distr_type == DISTRIBUTE_CYCLIC_EXPR,
             ("Replacing Val with symbol for non CYCLIC_EXPR"));
    _chunksize.sym = s;
    _chunk_stid_wn = stid_wn;
  }
  // for block/star
  void Init_Block_Star (DISTRIBUTE_TYPE dt);            
  // for cyclic_const
  void Init_Cyclic_Const (DISTRIBUTE_TYPE dt, INT64 const_val, WN* pwn);
  // for cyclic_expr
  void Init_Cyclic_Expr (DISTRIBUTE_TYPE dt, WN* val_wn);
  // for cyclic_expr, but with runtime lookup of chunksize
  void Init_Cyclic_Expr_Runtime (DISTRIBUTE_TYPE dt, INT32 dimnum);
  void Print (FILE* fp) const;
};

class DISTR_INFO;

class DISTR_ARRAY {
  DISTR_DIM*_dims;            /* distr of each dimension */
  WN*       _first_pragma_wn; /* pointer to first pragma node for this
                               * particular distribute pragma.
                               */
  WN*       _last_pragma_wn;  /* pointer to last pragma node for this
                               * particular distribute pragma.
                               */
  WN**      _bounds_wns;      /* array of pointer to bound xpragma nodes,
                               * one for each dimension.
                               */
  INT64*    _onto;              /* array of onto values, if any */
  DISTR_INFO* _dinfo;

public:
  DISTR_ARRAY () {}
  DISTR_ARRAY (DISTR_DIM* dims,
               WN *first_pragma,
               WN* last_pragma,
               WN** bounds_wns,
               INT64* onto) :
    _dims(dims),
    _first_pragma_wn(first_pragma),
    _last_pragma_wn(last_pragma),
    _dinfo(NULL),
    _bounds_wns(bounds_wns),
    _onto(onto)
  { }

  ~DISTR_ARRAY () {
    CXX_DELETE_ARRAY (_dims, LEGO_pool);
    CXX_DELETE_ARRAY (_bounds_wns, LEGO_pool);
    if (_onto) CXX_DELETE_ARRAY (_onto, LEGO_pool);
  }

  DISTR_DIM *Dims() const { return _dims; }
  DISTR_DIM *Get_Dim (INT i) const { return &(_dims[i]); }
  INT Num_Distr_Dim ();

  WN* First_Pragma_WN () const { return _first_pragma_wn; }
  WN* Last_Pragma_WN  () const { return _last_pragma_wn; }
  mBOOL Is_Compiler_Generated () const {
    return WN_pragma_compiler_generated(_first_pragma_wn);
  }

  /* Caution: This routine returns a cloned tree: if the caller only needs 
   * to look at it, then the caller should explicitly free it when done.
   */
  WN* Array_Size_WN  (INT i) const {
    WN* copy_wn = LWN_Copy_Tree(WN_kid0(_bounds_wns[i]));
    LWN_Copy_Def_Use (WN_kid0(_bounds_wns[i]), copy_wn, Du_Mgr);
    return copy_wn;
  }

  BOOL Has_Onto () const { return (_onto != NULL); }
  INT64 Onto(INT32 i) const {
    Is_True (_onto,("Onto: asking for onto-value, but no onto specified\n"));
    return _onto[i];
  }
  DISTR_INFO* Dinfo() const { return _dinfo; }
  void Set_Dinfo(DISTR_INFO *dinfo) { _dinfo = dinfo; }
  mBOOL DACT_Equiv(DISTR_ARRAY *test_dact, INT32 my_dim, INT32 test_dim);
  mBOOL DACT_Equiv(DISTR_ARRAY *test_dact);
  void Convert_Expr_To_Symbol ();
  WN *Chunksize (INT i) { return _dims[i].Chunksize(); }
  void Print (FILE* fp) const;
};

typedef DYN_ARRAY<DISTR_ARRAY*> DISTR_ARRAY_DA;

class DISTR_INFO {
  mBOOL     _isreshaped;    /* whether array is reshaped or not */
  mBOOL     _isdynamic;     /* whether we saw a c$dynamic or not */
  INT       _num_dim;       /* total number of dimensions in array */
  SYMBOL*   _array;         /* Symbol for ST entry for original array */
  ST*    _dart_st;       /* ST entry for runtime distr-tag for array */

  WN*       _dart_ptr_alias_wn; /* store alias-wn for derefs of the dart */
  DYN_ARRAY<WN*>  _dart_wn;     /* store all wns that define the dart */
  
  WN*       _ec_dart_def_wn;/* WN that defines dart used in error checking */
  WN*       _ec_dart_ptr_wn;/* WN of pointer to dart used in error checking */


  WN*       _array_alias_wn;/* store alias-wn for references to array */
  WN*       _array_ptr_alias_wn; /* store alias-wn for derefs of the array */
  WN*       _array_def_wn;  /* stid that stores value returned by
                             * __dsm_Alloc_Reshaped_Array for this array
                             * (for locals) or ldid (for globals), if reshaped.
                             */

  /* The following variables, dimsize/numprocs/chunksize for each dimension,
   * by rights belong within the DISTR_DIM class, but are declared here
   * since we want just one instance of each of those variables for a
   * distributed array, rather than one for each dact.
   */
  SYMBOL** _dimsize;        /* pointer to symbol containing block dim size*/
  SYMBOL** _numprocs;       /* pointer to symbol that contains
                             * the number of procs in this dimension
                             */
  SYMBOL* _array_common;    /* symbol for a reshaped array in a common
                             * (the new location).
                             */
  WN* _array_common_alias_wn; /* WN storing alias for reshaped common array
                               * (the new location).
                               */
  DYN_ARRAY<WN*>* _dimsize_wn;  /* stmt wns that define _dimsize (for locals)
                                 * or an ldid of _dimsize (for globals)
                                 */
  DYN_ARRAY<WN*>* _numprocs_wn; /* stmt wns that define _numprocs (for locals)
                                 * or an ldid of _numprocs (for globals)
                                 */
  TY_IDX _orig_ty;                 /* the original TY entry for the array */

  DISTR_ARRAY_DA _dact_da;          /* distribute DACTs */
  DISTR_ARRAY_DA _redistr_da;       /* redistribute DACTs */
  DISTR_ARRAY_DA _gen_redistr_da;   /* redistribute DACTs collected
                                     * after rest of LNO, during lego-gen
                                     * phase.
                                     */
  BOOL  _small_index;       /* TRUE if all indices fit in 32-bits,else FALSE */
  ST* _hoist_proc_array;    /* local array */
  INT _hoist_proc_index;    /* last value already used */
  WN *_hoist_proc_alias;    /* store aliasing WN for hoist_proc */
  DISTR_INFO* _buddy_dinfo; /* a buddy conforming dinfo, if any */
public:
  DISTR_INFO (mBOOL isreshaped, INT numdim, SYMBOL* array);
  ~DISTR_INFO ();

  BOOL Small_Index () const { return _small_index; }
  mBOOL IsReshaped() const { return _isreshaped; }
  mBOOL IsDynamic() const { return _isdynamic; }
  void Set_Dynamic() { _isdynamic = TRUE; }
  INT Num_Dim() const { return _num_dim; }
  SYMBOL* Array() const { return _array; }
  ST* Array_ST() const { return _array->St(); }
  ST* Dart_ST() const { return _dart_st; }

  void Find_Alloca ();
  WN* Load_Distr_Array ();
  WN* Load_New_Distr_Array ();
  SYMBOL* Array_Common_Symbol() const { return _array_common; }
  void Set_Array_Alias_WN (WN* wn)  { _array_alias_wn = wn; }
  WN* Get_Array_Alias_WN ()         { return _array_alias_wn; }
  void Set_Array_Ptr_Alias_WN (WN* wn)  { _array_ptr_alias_wn = wn; }
  WN* Get_Array_Ptr_Alias_WN ()         { return _array_ptr_alias_wn; }
  void Set_Array_Def_WN (WN* wn)    { _array_def_wn = wn; }
  void Add_Array_Use_WN (WN* use_wn) {
    if (_array_def_wn == NULL) _array_def_wn = Current_Func_Node;
    Du_Mgr->Add_Def_Use (_array_def_wn, use_wn);
  }
  // the following is used just for debugging
  WN* Get_Array_Def_WN ()           { return _array_def_wn; }

  WN* DART_Ldid (ST* st = NULL);
  void DART_Stid (WN* stid_wn, ST* st = NULL);
  void DART_Ptr_Ref(WN* wn, ST* st = NULL);

  SYMBOL* Get_Dimsize (INT i) const { return _dimsize[i]; }
  SYMBOL* Get_Numprocs (INT i) const { return _numprocs[i]; }
  WN* Numprocs(INT dim);
  WN* Dimsize(INT dim);
  WN* Chunksize(INT dim); 

  void Numprocs_Ldid (INT dim, WN* ldid_wn);
  void Numprocs_Stid (INT dim, WN* stid_wn);
  void Dimsize_Ldid (INT dim, WN* ldid_wn);
  void Dimsize_Stid (INT dim, WN* stid_wn);

  /* Return a WHIRL array node, a sample array reference to this array */
  WN* Get_Array_Dim_Size (INT dim);

  void Add_Dact (DISTR_ARRAY* dact) {
    _dact_da[_dact_da.Newidx()] = dact;
    dact->Set_Dinfo(this);
    FmtAssert (!_isreshaped || (_dact_da.Elements() == 1),
               ("Multiple reshapes (%s) not allowed", ST_name(_array->St())));
  }
  INT Num_Dact () const { return _dact_da.Elements(); }
  DISTR_ARRAY* Get_Dact (INT i) const { return _dact_da[i]; }

  void Add_Redistr (DISTR_ARRAY* redistr) {
    _redistr_da[_redistr_da.Newidx()] = redistr;
    redistr->Set_Dinfo(this);
    FmtAssert (!_isreshaped, ("Cannot redistribute a reshaped array (%s)",
                              ST_name(_array->St())));
  }
  INT Num_Redistr () const { return _redistr_da.Elements(); }
  DISTR_ARRAY* Get_Redistr (INT i) const { return _redistr_da[i]; }

  void Add_Gen_Redistr (DISTR_ARRAY* redistr) {
    _gen_redistr_da[_gen_redistr_da.Newidx()] = redistr;
    redistr->Set_Dinfo(this);
    FmtAssert (!_isreshaped, ("Cannot redistribute a reshaped array (%s)",
                              ST_name(_array->St())));
  }
  INT Num_Gen_Redistr () const { return _gen_redistr_da.Elements(); }
  DISTR_ARRAY* Get_Gen_Redistr (INT i) const { return _gen_redistr_da[i]; }

  void Set_Orig_TY(TY_IDX ty) { _orig_ty = ty; }
  TY_IDX Orig_TY() const { return _orig_ty; }

  ST* Hoist_Proc_Array() const { return _hoist_proc_array; }
  INT Hoist_Proc_Next_Offset() {
    Is_True (_hoist_proc_array, ("Hoist-Next-Offset, but not initialized\n"));
    ++_hoist_proc_index;
    return (_hoist_proc_index*
            TY_size(TY_AR_etype(ST_type(_hoist_proc_array))));
  }
  void Hoist_Proc_Init (TYPE_ID type);
  void Hoist_Proc_Alias (WN* wn) {
    if (_hoist_proc_alias == NULL) {
      _hoist_proc_alias = wn;
      Create_unique_pointer_alias (Alias_Mgr, 
		_hoist_proc_array, NULL, wn);
    }
    else {
      Copy_alias_info (Alias_Mgr, _hoist_proc_alias, wn);
    }
  }
  void Set_Buddy (DISTR_INFO* dinfo) {
    Is_True (dinfo != this,
             ("Set_Buddy: trying to set self to buddy"));
    _buddy_dinfo = dinfo;
  }

  void Print (FILE* fp) const {
    INT i;
    fprintf (fp, "Distributed array: "); _array->Print (fp);
    fprintf (fp, " %d dimensions, %s\n", _num_dim,
             (_isreshaped ? "reshaped" : "not reshaped"));
    fprintf (fp, "Compile-time distributions (%d)\n", _dact_da.Elements());
    for (i=0; i<_dact_da.Elements (); i++) {
      fprintf (fp, "Distribution number %d\n", i);
      _dact_da[i]->Print (fp);
    }
    fprintf (fp, "Redistributions (%d)\n", _redistr_da.Elements());
    for (i=0; i<_redistr_da.Elements (); i++) {
      fprintf (fp, "Redistribution number %d\n", i);
      _redistr_da[i]->Print (fp);
    }
  }
};

class DISTR_GLOBAL_INFO {
  TY_IDX _orig_ty;
  DISTR_GLOBAL_INFO (void);
  DISTR_GLOBAL_INFO (const DISTR_GLOBAL_INFO&);
  DISTR_GLOBAL_INFO* operator= (const DISTR_GLOBAL_INFO&);
public:
  DISTR_GLOBAL_INFO(TY_IDX ty) { _orig_ty = ty; }
  TY_IDX Get_TY () { return _orig_ty; }
};

typedef HASH_TABLE<ST*, DISTR_INFO*> DA_HASH_TABLE;
extern DA_HASH_TABLE *da_hash;
typedef STACK<DISTR_INFO*> DA_STACK;
extern DA_STACK *da_stack;
typedef HASH_TABLE<ST*, DISTR_GLOBAL_INFO*> DA_GLOBAL_HASH_TABLE;
extern DA_GLOBAL_HASH_TABLE* da_global;

extern void Read_Distr_Pragmas (WN* func_nd);
extern WN *Read_Pragma_Distribute(WN *pwn);
extern WN* Read_Pragma_Redistribute (WN* pwn, BOOL gen_phase = FALSE);
extern WN* Load_Distr_Array (ST* array_st);
extern WN* Numprocs (ST* array_st, INT dim, BOOL inside_loop, WN** block_wn);

#endif // _LEGO_PRAGMA_INCLUDED_
