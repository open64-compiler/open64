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


#ifndef dra_internal_INCLUDED
#define dra_internal_INCLUDED

// ====================================================================
// ====================================================================
//
// Module: dra_internal.h
//
// Revision history:
//  16-Jul-96: Original Version
//
// Description: Internal data structures and interface specifications
//              used to support cloning and name mangling in the 
//              presence of distribute_reshaped array arguments.
//
//              -----------------------------------------------------
//              DO NOT include this file from outside dra_* world !!!
//              -----------------------------------------------------
//
// ====================================================================
// ====================================================================

// ====================================================================
//
// class DRA_INFO
//       Store basic information for a reshaped array.
//
// Public Methods
// --------------
//
//  DRA_INFO (INT16 ndims, INT64 esize, MEM_POOL *pool)
//      Constructor.
//
//  DISTRIBUTE_TYPE Distr_Type(INT16 dim)
//      Return the type of distribution.
//
//  INT64 Chunk_Const_Val(INT16 dim)
//      Return the chunksize (must be constant)
// 
//  void Init (INT16 dim, DISTRIBUTE_TYPE dt);
//      Initialize routine for block/star/cyclic_expr distributions.
//
//  void Init (INT16 dim, DISTRIBUTE_TYPE dt, INT64 const_val)
//      Initialize routines for cyclic_const
//
//  INT64 Element_Size()
//      Return the size in bytes of a single array element.
//
//  void Print (FILE* fp)
//      Standard print routine
//
// ====================================================================
//
// class DRA_GLOBAL_INFO
//      Store information about global arrays
//
// Public Methods
// --------------
//
//  DRA_GLOBAL_INFO(TY_IDX ty)
//      Constructor
//
//  TY_IDX Get_TY ()
//      Return the original TY of the array.
//
// ====================================================================
//
// class STRING_NODE
//       Derived from the base class SLIST_NODE
//
// class STRING_LIST
//       Derived from the base class SLIST
//
// class STRING_ITER
//       Derived from the base clase SLIST_ITER
//
// ====================================================================
// 
// Exported Types
// --------------
//
//  DRA_HASH_TABLE
//      Hash table of ST entries and the corresponding DRA_INFO's
//
//  STRING_LIST_TABLE
//      Hash table of linked lists of strings
// 
//  NAME_ST_TABLE
//      Hash table of mangled names and their ST entries
//
// ====================================================================
//
// Exported variables
// ------------------
//
// extern NAME_ST_TABLE *DRA_func_table;
//      Global table used to resolve cloned and mangled names
//
// extern DRA_HASH_TABLE *DRA_array_table;
//      Global table used to store information about DRA's
//
// extern MEM_POOL DRA_name_pool, *DRA_name_pool_ptr;
//      Memory pool used for DRA_array_table
//
// extern DRA_GLOBAL_HASH_TABLE *dra_global;
//      Global hash-table that stores information about global arrays.
//
// ====================================================================
//
// Exported Functions
// ------------------
//
// Reading distribute_reshape pragmas and mangling function names:
// extern void DRA_Read_Pragmas (WN* func_nd, DRA_HASH_TABLE *dra_table);
// extern void DRA_Mangle_All(WN *func_wn, 
//                            DRA_HASH_TABLE *dra_table,
//                            PU_Info *pu_info);
// extern WN* Get_Preamble_End(WN *pu_wn);
//  
// Utility functions for processing .rii files:
// extern void Make_rii_File_Name();
// extern char* Mem_Map_Prelinker_File();
// extern void Mem_Unmap_Prelinker_File(char *rii_file);
// extern FILE* Open_Prelinker_File_For_Append();
//
// ====================================================================
// 
// Exported Constants
// ------------------
//
// DRA_MANGLE_SIG, DRA_MANGLE_SIG_LEN
//      Signature string used in name mangling and its length
// DRA_STAR_CODE, DRA_BLOCK_CODE, DRA_CYCLIC_CODE 
//      Encodings for different distribution types
// DRA_NDIMS_END, DRA_ESIZE_END, DRA_ARG_SEPARATOR
//      Encoding separators
// 
// PRELINKER_FILE_SEPARATOR
//      Section separator in .rii files
//
// ====================================================================


#ifndef defs_INCLUDED
#include "defs.h"               // standard definitions
#endif

#ifndef pragmas_INCLUDED
#include "wn_pragmas.h"         // DISTRIBUTE_TYPE
#endif

#ifndef CXX_MEMORY_INCLUDED
#include "cxx_memory.h"         // CXX_NEW
#endif

#ifndef cxx_hash_INCLUDED
#include "cxx_hash.h"           // HASH_TABLE
#endif


#ifndef cxx_base_INCLUDED
#include "cxx_base.h"           // SLIST, SLIST_NODE
#endif

#ifndef mtypes_INCLUDED
#include "mtypes.h"             // TYPE_ID
#endif

#include "symtab.h"


typedef struct {
  DISTRIBUTE_TYPE _distr_type;  // DISTRIBUTE_STAR,
                                // DISTRIBUTE_BLOCK,
                                // DISTRIBUTE_CYCLIC_EXPR, or
                                // DISTRIBUTE_CYCLIC_CONST.
                                // (definitions in wn_pragmas.h)

  INT64 _chunksize;             // chunk size for CYCLIC_CONST

} RESHAPED_DIM;



class DRA_INFO {

private:

  INT16 _ndims;                 // number of dimensions

  INT64 _esize;                 // element size

  struct mem_pool *_pool;       // memory pool used for allocation

  RESHAPED_DIM *_dims;          // distr of each dimension

public:

  DRA_INFO (INT16 ndims, INT64 esize, struct mem_pool *pool) {
    _ndims = ndims;
    _esize = esize;
    _pool = pool;
    _dims = CXX_NEW_ARRAY(RESHAPED_DIM, ndims, pool);
    for (INT16 i = 0; i < ndims; i++) {
      _dims[i]._chunksize = 0;
    }
  }

  INT16 Num_Dims() const { return _ndims; }

  DISTRIBUTE_TYPE Distr_Type (INT16 dim) const {
    return _dims[dim]._distr_type;
  }

  INT64 Chunk_Const_Val (INT16 dim) const {
    return _dims[dim]._chunksize;
  }

  void Init (INT16 dim, DISTRIBUTE_TYPE dt) {  
    Is_True ((dt == DISTRIBUTE_BLOCK) || 
             (dt == DISTRIBUTE_STAR)  ||
             (dt == DISTRIBUTE_CYCLIC_EXPR),
             ("Distribute type must be BLOCK, STAR, or CYCLIC_EXPR"));
    _dims[dim]._distr_type = dt;
  }

  void Init (INT16 dim, DISTRIBUTE_TYPE dt, INT64 chunksize) { 
    Is_True (dt == DISTRIBUTE_CYCLIC_CONST,
             ("Distribute type must be CYCLIC_CONST"));
    _dims[dim]._distr_type = dt;
    _dims[dim]._chunksize = chunksize;
  }

  INT64 Element_Size () const { return _esize; }

  void Print (FILE* fp) const {
    fprintf (fp, "Distribution: \n");
    for (INT16 i = 0; i < _ndims; i++) {
      DISTRIBUTE_TYPE dt = _dims[i]._distr_type;
      fprintf (fp, "%s", 
               (dt == DISTRIBUTE_STAR) ? "STAR" : 
               (dt == DISTRIBUTE_BLOCK) ? "BLOCK" : 
               (dt == DISTRIBUTE_CYCLIC_EXPR) ? "CYCLIC_EXPR" : 
               (dt == DISTRIBUTE_CYCLIC_CONST) ? "CYCLIC_CONST" : 
               "unknown");
      if (dt == DISTRIBUTE_CYCLIC_CONST) {
        fprintf (fp, " (%lld)", _dims[i]._chunksize);
      }
    }
    fprintf (fp, "\n");
  }
    
};



class STRING_NODE : public SLIST_NODE {
  DECLARE_SLIST_NODE_CLASS(STRING_NODE);

  private:
    STR_IDX _string;
  
  public:
    STRING_NODE(STR_IDX string) { Set_Next(NULL); _string = string; }
    STR_IDX String() { return _string; }
};


class STRING_LIST : public SLIST {
  DECLARE_SLIST_CLASS(STRING_LIST, STRING_NODE);
};


class STRING_ITER : public SLIST_ITER {
  DECLARE_SLIST_ITER_CLASS(STRING_ITER, STRING_NODE, STRING_LIST);
};


// HASH_TABLE of DRA_INFO's
//
typedef HASH_TABLE<ST*, DRA_INFO*> DRA_HASH_TABLE;


// HASH_TABLE of STRING_LIST's 
//
typedef HASH_TABLE<STR_IDX, STRING_LIST*> STRING_LIST_TABLE;


typedef struct {
  ST *st;
  BOOL is_clone;
  BOOL is_called;
} MANGLED_FUNC;

// HASH_TABLE of mangled names and their ST entries with flags
//
typedef HASH_TABLE<STR_IDX, MANGLED_FUNC*> NAME_ST_TABLE;

typedef HASH_TABLE_ITER <STR_IDX, MANGLED_FUNC*> NAME_ST_TABLE_ITER;


class DRA_GLOBAL_INFO {
  TY_IDX _orig_ty;
  DRA_GLOBAL_INFO (void);
  DRA_GLOBAL_INFO (const DRA_GLOBAL_INFO&);
  DRA_GLOBAL_INFO* operator= (const DRA_GLOBAL_INFO&);
public:
  DRA_GLOBAL_INFO(TY_IDX ty) { _orig_ty = ty; }
  TY_IDX Get_TY () { return _orig_ty; }
};

typedef HASH_TABLE<ST*, DRA_GLOBAL_INFO*> DRA_GLOBAL_HASH_TABLE;
extern DRA_GLOBAL_HASH_TABLE* dra_global;

typedef HASH_TABLE<ST_IDX, BOOL> DRA_COMMON_HASH_TABLE;


// Global table used to resolve cloned and mangled names
//
extern NAME_ST_TABLE *DRA_func_table;


// Global table used to store information about DRA's
//
extern DRA_HASH_TABLE *DRA_array_table;


// Memory pool used for DRA_array_table
//
extern struct mem_pool DRA_name_pool, *DRA_name_pool_ptr;


// File descriptor and pointer to mapped memory for .rii file
//
extern INT DRA_file_desc;

extern char *DRA_file_mmap;

extern char DRA_file_name[];

class WN;

// Reading distribute_reshape pragmas and mangling function names
//
extern void DRA_Read_Pragmas (WN *func_nd,
                              DRA_HASH_TABLE *dra_table);

extern void DRA_Mangle_All (WN *func_wn,
                            DRA_HASH_TABLE *dra_table,
                            struct pu_info *pu_info);

extern void DRA_EC_Declare_Types ();

extern void DRA_EC_Array_Portion_Parms (WN *func_nd,
                                        WN *entry_nd);

extern WN *Get_Preamble_End (WN *pu_wn);

extern ST *Find_Return_Registers (TYPE_ID type, 
                                        PREG_NUM *rreg1, 
                                        PREG_NUM *rreg2);


// Utility functions for processing of .rii files
//
extern void DRA_Open_And_Map_File();

extern void DRA_Set_Write_Location();

extern void DRA_Mem_Unmap_File();

extern void DRA_Close_File();



// Constants used in name mangling
// 
// NOTE: All these constants have their #define counterparts
//       in common/com/dra_demangle.c
//       Any change in this file should also be reflected there.      

static const char* DRA_MANGLE_SIG     = "__nn__";
static const INT   DRA_MANGLE_SIG_LEN = strlen(DRA_MANGLE_SIG);

static const char  DRA_STAR_CODE      = 'S';
static const char  DRA_BLOCK_CODE     = 'B';
static const char  DRA_CYCLIC_CODE    = 'C';
static const char  DRA_NDIMS_END      = 'D';
static const char  DRA_ESIZE_END      = 'E';
static const char  DRA_ARG_SEPARATOR  = '_';


// Section separator in the prelinker (.rii) file
//
static const char* DRA_FILE_SEPARATOR = "----\n";

// Get the array type of a distributed array given the st

extern TY_IDX Get_Array_Type (ST* st);

// Get the original type of a st

extern TY_IDX Get_Original_Type (ST* st);

#endif /* dra_internal_INCLUDED */
