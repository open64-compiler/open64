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


// ====================================================================
// ====================================================================
//
// Module: dra_mangle.cxx
//
// Revision history:
//  16-Jul-96: Original Version
//
// Description:
//  Routines used for name mangling needed to support cloning
//  of routines that have distribute-reshaped array parameters.
// 
// ====================================================================
// ====================================================================

#include <alloca.h>             // alloca
#if ! defined(BUILD_OS_DARWIN)
#include <elf.h>
#endif /* ! defined(BUILD_OS_DARWIN) */

#include "defs.h"               // standard definitions
#include "wn.h"                 // WN
#include "stab.h"               // ST, TY
#include "pu_info.h"            // PU_Info

#include "opcode.h"             // OPCODE_make_op
#include "mempool.h"             // MEM_POOL
#include "cxx_memory.h"         // CXX_NEW, CXX_DELETE
#include "wn_pragmas.h"         // WN_pragma
#include "config_targ.h"        // Pointer_Size
#include "targ_sim.h"           // Get_Return_Mtypes
#include "erbe.h"               // ErrMsgSrcpos
#include "strtab.h"             // Save_Str
#include "wn_util.h"            // WN_DELETE
#include "dwarf_DST.h"          // DST_IDX
#include "config.h"             // Run_ipl, Run_cg
#include "cg.h"                 // CG_Change_Elf_Symbol_To_Undefined

#include "dra_internal.h"       // Internal DRA interface

#if defined(__linux__) || defined(BUILD_OS_DARWIN) || !defined(SHARED_BUILD)
extern void (*CG_Change_Elf_Symbol_To_Undefined_p) (ST*);
#define CG_Change_Elf_Symbol_To_Undefined (*CG_Change_Elf_Symbol_To_Undefined_p)
#else
#pragma weak CG_Change_Elf_Symbol_To_Undefined
#endif // __linux__

// =====================================================================
//
//                      Local function prototypes
//
// =====================================================================

static DRA_INFO* New_DRA (WN** pwn_addr,
                          ST *array_st,
                          TY_IDX array_ty,
                          WN *block, 
                          WN *preamble);

static BOOL Array_TY_OK (TY_IDX array_ty);

static void Insert_Alloca(ST *array_st, 
                          INT16 ndims,
                          INT64 esize,
                          WN *dim_size_wn[], 
                          WN *block, 
                          WN *preamble);

static WN* Delete_Dist_Reshape_Pragmas (WN* pwn, 
                                        WN* block);

static void DRA_Mangle_Entry(WN *entry_wn, 
                             INT16 num_formals, 
                             DRA_HASH_TABLE *dra_table, 
                             DST_ASSOC_INFO *assoc_info);

static void DRA_Mangle_Call_Site(WN *call_wn, 
                                 OPERATOR call_oper, 
                                 DRA_HASH_TABLE *dra_table);

static UINT DRA_Preprocess_Entry(WN *entry_wn, 
                                 INT16 num_formals, 
                                 DRA_HASH_TABLE *dra_table, 
                                 BOOL *has_reshaped_formals);

static UINT DRA_Preprocess_Call_Site(WN *call_wn, 
                                     INT16 num_args, 
                                     DRA_HASH_TABLE *dra_table,
                                     BOOL *has_reshaped_args);

static char* DRA_Insert_Mangling_Sigs(char *orig_name, 
                                      char mangled_buf[]);

static char* DRA_Encode_Parameter(ST *st, 
                                  TY_IDX ty, 
                                  char *buf, 
                                  DRA_HASH_TABLE *dra_table);


// =====================================================================
//
//                      Exported function definitions
//
// =====================================================================

// =====================================================================
//
// Function Name: DRA_Read_Pragmas
//
// Description:  Given the function tree, read all DISTRIBUTE_RESHAPE 
//               pragmas and build up internal representations.
//
// =====================================================================


// Read pragmas in given WN block.  Can be called recursively.
static void
DRA_Read_Pragmas_In_Block (WN *first, WN* block, WN *preamble, 
                           DRA_HASH_TABLE *dra_table)
{
  WN *pwn = first;

  // Walk the tree, processing pragma nodes
  // 
  while (pwn) {

    if (WN_opcode(pwn) == OPC_REGION) {
      // look inside regions
      DRA_Read_Pragmas_In_Block (WN_first(WN_region_body(pwn)), WN_region_body(pwn), preamble, dra_table);
    }

    if (WN_opcode(pwn) == OPC_PRAGMA &&
        WN_pragma(pwn) == WN_PRAGMA_DISTRIBUTE_RESHAPE) {

      ST *array_st = WN_st(pwn);
      
      if (array_st == NULL || ST_is_not_used(array_st)) {
        // Front end may discard ST entries for arrays that are never used
        //
        pwn = Delete_Dist_Reshape_Pragmas(pwn, block);
      }

      else {

        TY_IDX array_ty = Get_Array_Type(array_st);

        if (TY_kind(array_ty) != KIND_ARRAY) {
          // This error will most likely be caught by FE
          //
          ErrMsgSrcpos(EC_DRA_unsupported_type, 
                       WN_Get_Linenum(pwn),
                       "DISTRIBUTE_RESHAPE",
                       ST_name(array_st),
                       "Cannot reshape non-arrays");
          
          pwn = Delete_Dist_Reshape_Pragmas(pwn, block);
        }
        
        else if (ST_is_equivalenced(array_st)) {
          // We don't support reshaping of equivalenced arrays
          //
          ErrMsgSrcpos(EC_DRA_unsupported_type, 
                       WN_Get_Linenum(pwn),
                       "DISTRIBUTE_RESHAPE",
                       ST_name(array_st),
                       "Cannot reshape equivalenced arrays");
          
          pwn = Delete_Dist_Reshape_Pragmas(pwn, block);
        }

        else if (ST_sclass(array_st) == SCLASS_DGLOBAL ||
                 ST_is_initialized(array_st)) {
          // Defined (initialized) C global data, allocated in this 
          // module and initialized ST are for now not handled
          //
          ErrMsgSrcpos(EC_DRA_unsupported_type, 
                       WN_Get_Linenum(pwn),
                       "DISTRIBUTE_RESHAPE",
                       ST_name(array_st),
                       "Cannot reshape initialized data");
          
          pwn = Delete_Dist_Reshape_Pragmas(pwn, block);
        }

        else if (!Array_TY_OK (array_ty)) {
          ErrMsgSrcpos(EC_DRA_unsupported_type, 
                       WN_Get_Linenum(pwn),
                       "DISTRIBUTE_RESHAPE",
                       ST_name(array_st),
                       "Cannot reshape assumed size arrays");
          
          pwn = Delete_Dist_Reshape_Pragmas(pwn, block);
        }

        else {

          FmtAssert (ST_class(array_st) == CLASS_VAR,
                   ("Distributed array is not a variable"));

          DRA_INFO* dra = New_DRA(&pwn, array_st, array_ty, block, preamble);

          if (dra != NULL) {
            // Add dra to the hash table
            //
            dra_table->Enter(array_st, dra);
          }
        }
      }
    } 

    else {
      pwn = WN_next(pwn);
    }
  }
}

void
DRA_Read_Pragmas (WN* func_nd,
                  DRA_HASH_TABLE *dra_table)
{
  Set_Error_Phase("Processing DISTRIBUTE_RESHAPE pragmas");
  
  WN *block = WN_func_body(func_nd);
  WN *preamble = Get_Preamble_End(func_nd);

  DRA_Read_Pragmas_In_Block (preamble, block, preamble, dra_table);
} 




// =====================================================================
// 
// Function Name: DRA_Mangle_All
// 
// Description: Mangle the name of the given PU if at least one of 
//              its formal parameters is a distribute-reshaped array.
//              
//              For each call site in the PU, the name of the callee
//              is mangled if at least one of the actual arguments is
//              a distribute-reshaped array.
//
// =====================================================================

void 
DRA_Mangle_All(WN *func_wn,
               DRA_HASH_TABLE *dra_table,
               PU_Info *pu_info)
{   
  // Allocate the global name/ST hash table used for resolving names
  //
  if (DRA_func_table == NULL) {
    DRA_func_table = CXX_NEW(NAME_ST_TABLE(31, &MEM_src_pool), &MEM_src_pool);
  }

  DST_ASSOC_INFO* assoc_info = NULL;
  DST_IDX pu_idx = PU_Info_pu_dst(pu_info);
  if (!DST_IS_NULL(pu_idx)) {
    DST_INFO* info = DST_INFO_IDX_TO_PTR(pu_idx);
    DST_SUBPROGRAM* pu_attr = 
      DST_ATTR_IDX_TO_PTR(DST_INFO_attributes(info), DST_SUBPROGRAM);
    assoc_info = &(DST_SUBPROGRAM_def_st(pu_attr));
  }

  // Process the function entry
  //
  DRA_Mangle_Entry(func_wn, WN_num_formals(func_wn), dra_table, assoc_info);
  
  // Walk the tree and process alternate entry points and call sites
  //
  WN_ITER *wni;
  for (wni = WN_WALK_TreeIter(func_wn); wni; wni = WN_WALK_TreeNext(wni)) {

    WN *stmt_wn = WN_ITER_wn(wni);

    if (WN_opcode(stmt_wn) == OPC_ALTENTRY) {
      DST_ASSOC_INFO* assoc_info = NULL;
      DST_IDX dst_idx = DST_INFO_sibling(DST_INFO_IDX_TO_PTR(pu_idx));
      while (!DST_IS_NULL(dst_idx)) {
		DST_INFO* info = DST_INFO_IDX_TO_PTR(dst_idx);
		if (DST_INFO_tag(info) == DW_TAG_entry_point) {
          DST_ENTRY_POINT* attr = 
            DST_ATTR_IDX_TO_PTR(DST_INFO_attributes(info), DST_ENTRY_POINT);
          if (DST_ASSOC_INFO_st_index(DST_ENTRY_POINT_st(attr)) == 
              ST_index(WN_st(stmt_wn))) {
            assoc_info = &(DST_ENTRY_POINT_st(attr));
            break;
          }
        }
        dst_idx = DST_INFO_sibling(info);
      }

      DRA_Mangle_Entry(stmt_wn, WN_kid_count(stmt_wn), dra_table, assoc_info);
    }

    else {
      OPERATOR stmt_oper = WN_operator(stmt_wn);
      if (stmt_oper == OPR_CALL || stmt_oper == OPR_ICALL) {
        DRA_Mangle_Call_Site(stmt_wn, stmt_oper, dra_table);
      }
    } 

  } 
} 




// =====================================================================
//
// Function Name: Get_Preamble_End
//
// Description: For a given entry, find the PREAMBLE_END pragma.
//
// =====================================================================

WN*
Get_Preamble_End(WN* entry_wn)
{
  Is_True (WN_opcode(entry_wn) == OPC_FUNC_ENTRY ||
           WN_opcode(entry_wn) == OPC_ALTENTRY,
           ("Get_Preamble_End: Expected FUNC_ENTRY or ALTENTRY node"));
  
  WN* wn;
  if (WN_opcode(entry_wn) == OPC_FUNC_ENTRY) {
    wn = WN_entry_first(entry_wn); 
  }
  else {
    wn = WN_next(entry_wn);
  }

  while (wn != NULL && (WN_opcode(wn) != OPC_PRAGMA ||
                        WN_pragma(wn) != WN_PRAGMA_PREAMBLE_END)) {
    wn = WN_next(wn);
  }
  
  Is_True (wn != NULL, ("Could not find the PREAMBLE_END pragma"));
  
  return wn;
}




// =====================================================================
//
// Function Name: Find_Return_Registers
//
// Description: Figure out which return registers are to be used
//              for returning an object of the given type.
//
// =====================================================================

ST* 
Find_Return_Registers(TYPE_ID type,
                      PREG_NUM *rreg1,
                      PREG_NUM *rreg2)
{
  TYPE_ID       mtype1;
  TYPE_ID       mtype2;

  if (WHIRL_Return_Info_On) {

    RETURN_INFO return_info = Get_Return_Info (Be_Type_Tbl(type),
                                               Use_Simulated);

    if (RETURN_INFO_count(return_info) <= 2) {

      *rreg1 = RETURN_INFO_preg (return_info, 0);
      *rreg2 = RETURN_INFO_preg (return_info, 1);
    }

    else {

      Fail_FmtAssertion ("Find_Return_Registers: more than 2 return registers");
      return NULL;
    }
  }

  else {

    Get_Return_Mtypes(Be_Type_Tbl(type), Use_Simulated, &mtype1, &mtype2);
    Get_Return_Pregs(mtype1, mtype2, rreg1, rreg2);
  }

  if (Preg_Offset_Is_Int(*rreg1)) {
    if (MTYPE_bit_size(type) == 32) {
      return Int32_Preg;
    } else {
      return Int64_Preg;
    }
  } else {
    if (MTYPE_bit_size(type) == 32) {
      return Float32_Preg;
    } else {
      return Float64_Preg;
    }
  }
}





// =====================================================================
//
//                      Local function definitions
//
// =====================================================================


// =====================================================================
//
// Function Name: New_DRA
//
// Description: Given a pointer to the first pragma node, consume all 
//              pragma nodes belonging to this DISTRIBUTE_RESHAPE pragma
//              and return a DRA_INFO. As a side effect, the pointer to 
//              the pragma node in the caller is bumped up to point to 
//              the node immediately following the last pragma node.
//
// =====================================================================

static DRA_INFO* 
New_DRA (WN** pwn_addr,  // address of the pragma node pointer 
         ST *array_st,   // ST entry for the array
         TY_IDX array_ty, // TY entry for the array
         WN *block,      // block to which pragmas belong
         WN *preamble)   // end of preamble code
{
  TY& ty = Ty_Table[array_ty];
  INT16 ndims = TY_AR_ndims(ty);
  INT64 esize = TY_size(TY_AR_etype(ty));
  DRA_INFO *dra = CXX_NEW(DRA_INFO(ndims, esize, DRA_name_pool_ptr),
                          DRA_name_pool_ptr);

  WN *pwn = *pwn_addr;
  WN **dim_size_wn = (WN**) alloca(ndims*sizeof(WN*));
  INT16 distr_dims = 0;

  for (INT16 i = 0; i < ndims; i++) {

    // Process ith dimension. pwn points to node for ith dimension
    //
    FmtAssert (WN_opcode(pwn) == OPC_PRAGMA,
               ("Distribute_Reshape_Pragma: expected a PRAGMA node\n"));

    FmtAssert (WN_pragma(pwn) == WN_PRAGMA_DISTRIBUTE_RESHAPE,
               ("Distribute_Reshape_Pragma: unexpected PRAGMA type\n"));

    switch (WN_pragma_distr_type(pwn)) {

      case DISTRIBUTE_STAR:
        dra->Init (i, (DISTRIBUTE_TYPE) WN_pragma_distr_type(pwn));
        break;

      case DISTRIBUTE_BLOCK:
        distr_dims++;
        dra->Init (i, (DISTRIBUTE_TYPE) WN_pragma_distr_type(pwn));
        break;

      case DISTRIBUTE_CYCLIC_CONST:
        distr_dims++;
        dra->Init (i, (DISTRIBUTE_TYPE) WN_pragma_distr_type(pwn),
                   (INT64) WN_pragma_arg2(pwn));
        break;

      case DISTRIBUTE_CYCLIC_EXPR:
        distr_dims++;
        dra->Init (i, (DISTRIBUTE_TYPE) WN_pragma_distr_type(pwn));
        
        pwn = WN_next(pwn);

        FmtAssert (WN_opcode(pwn) == OPC_XPRAGMA,
                   ("Distribute_Reshape_Pragma: expected an XPRAGMA node\n"));

        FmtAssert (WN_pragma(pwn) == WN_PRAGMA_DISTRIBUTE_RESHAPE,
                   ("Distribute_Reshape_Pragma: unexpected XPRAGMA type\n"));

        break;
      
      default:
        FmtAssert (FALSE, 
                   ("Distribute_Reshape_Pragma: strange distribute type\n"));
        break;  
    }

    pwn = WN_next(pwn);
    dim_size_wn[i] = WN_kid(pwn, 0);
    
    FmtAssert (WN_opcode(pwn) == OPC_XPRAGMA,
               ("Distribute_Reshape_Pragma: expected an XPRAGMA node\n"));

    FmtAssert (WN_pragma(pwn) == WN_PRAGMA_DISTRIBUTE_RESHAPE,
               ("Distribute_Reshape_Pragma: unexpected XPRAGMA type\n"));

    pwn = WN_next(pwn);
  } 


  if (distr_dims == 0) {

    ErrMsgSrcpos(EC_DRA_all_stars, 
                 WN_Get_Linenum(*pwn_addr),
                 ST_name(array_st));
                 
    // Since LNO will not see this DRA pragma, we need 
    // to generate ALLOCA for allocatable local arrays
    //
    if (ST_sclass(array_st) == SCLASS_AUTO &&
        TY_kind(ST_type(array_st)) == KIND_POINTER) {

      Insert_Alloca(array_st, ndims, esize, dim_size_wn, block, preamble);
    }

    dra = NULL;
    pwn = Delete_Dist_Reshape_Pragmas (*pwn_addr, block);

  }

  *pwn_addr = pwn;

  return dra;
} 




// =====================================================================
//
// Function Name: Array_TY_OK
//
// Description: Check that the array TY is kosher for data distribution.
//
// =====================================================================
static BOOL 
Array_TY_OK (TY_IDX array_ty) 
{
  const TY& ty = Ty_Table[array_ty];

  Is_True (TY_kind(ty) == KIND_ARRAY,
           ("Array_TY_OK called on a non-array"));

  INT16 ndims = TY_AR_ndims(ty);

  Is_True (ndims > 0, ("Array_TY_OK: array has 0 dimensions?"));

  for (INT16 i = 0; i < ndims; i++) {
    // Each thing must be a constant or a WHIRL <expr> tree
    //
    if ((!TY_AR_const_lbnd(ty, i)) &&
        (TY_AR_lbnd_val(ty, i) == 0)) {
      return FALSE;
    }

    if ((!TY_AR_const_ubnd(ty, i)) &&
        (TY_AR_ubnd_val(ty, i) == 0)) {
      return FALSE;
    }
  }

  return TRUE;
}




// =====================================================================
//
// Function Name: Insert_Alloca
//
// Description: Generate a call to ALLOCA for an allocatable local array
//
// =====================================================================

static void
Insert_Alloca(ST *array_st, 
              INT16 ndims,
              INT64 esize,
              WN *dim_size_wn[], 
              WN *block, 
              WN *preamble)
{
  WN* size_wn = WN_Intconst(MTYPE_I8, esize);
  for (INT16 i = 0; i < ndims; i++) {
    size_wn = WN_Mpy(MTYPE_I8, size_wn, WN_CopyNode(dim_size_wn[i]));
  }

  OPCODE call_op = OPCODE_make_op(OPR_INTRINSIC_CALL, 
                                  Pointer_type, 
                                  MTYPE_V);
  WN *call_wn = WN_Create(call_op, 1);
  WN_intrinsic(call_wn) =
    (Pointer_Size == 8) ? INTRN_U8I8ALLOCA : INTRN_U4I4ALLOCA;

  WN* parm_wn = WN_CreateParm (Pointer_type, 
                               size_wn,
                               Be_Type_Tbl(Pointer_type),
                               WN_PARM_BY_REFERENCE);
  WN_kid0(call_wn) = parm_wn;

  // WN_Copy_Linenumber(dim_size_wn[0], call_wn);

  // Insert call node
  //
  WN_INSERT_BlockBefore(block, preamble, call_wn);

      
  // Generate code to store the return values into the array
  //
  PREG_NUM rreg1, rreg2;
  ST* rst = Find_Return_Registers (Pointer_type, &rreg1, &rreg2);
  FmtAssert(rreg1 != 0 && rreg2 == 0, ("Bad pointer type ret regs"));
      
  WN *ldid_wn = 
    WN_CreateLdid (OPCODE_make_op(OPR_LDID, Pointer_type, Pointer_type),
                   rreg1, 
                   rst, 
                   Be_Type_Tbl(Pointer_type));

  WN *stid_wn = 
    WN_CreateStid (OPCODE_make_op(OPR_STID, MTYPE_V, Pointer_type),
                   0,
                   array_st,
                   ST_type(array_st),
                   ldid_wn);

  // WN_Copy_Linenumber (dim_size_wn[0], stid_wn);
      
  // Insert store node
  //
  WN_INSERT_BlockBefore(block, preamble, stid_wn);

  Set_ST_pt_to_unique_mem(array_st);
  Set_PU_has_alloca (Get_Current_PU ());
}




// =====================================================================
//
// Function Name: Delete_Dist_Reshape_Pragmas
//
// Description: Delete all DISTRIBUTE_RESHAPE pragmas that are in the
//              same set as the given pragma node and return the 
//              statement node following the last deleted pragma.
//
// =====================================================================

static WN*
Delete_Dist_Reshape_Pragmas (WN* pwn, 
                             WN *block) 
{
  FmtAssert (WN_opcode(pwn) == OPC_PRAGMA &&
             WN_pragma(pwn) == WN_PRAGMA_DISTRIBUTE_RESHAPE,
             ("Delete_Dist_Reshape_Pragma: Wrong opcode and/or pragma\n"));
  
  WN *current = pwn;
  WN *next;
  ST *st = WN_st(current);

  while (current) {

    // remember next statement
    //
    next = WN_next(current);

    // delete current statement
    //
    WN_DELETE_FromBlock (block, current);
    // WN_DELETE_Tree(current);

    // advance current statement pointer
    //
    current = next;

    // Delete all DISTRIBUTE_RESHAPE [x]pragmas with the same ST pointer
    //
    if ((WN_opcode(current) != OPC_PRAGMA &&
         WN_opcode(current) != OPC_XPRAGMA) ||
        WN_pragma(current) != WN_PRAGMA_DISTRIBUTE_RESHAPE ||
        WN_st(current) != st) {
      return current;
    }
  }

  return NULL; // It should never get here
}


// =====================================================================
// 
// Function Name: Change_ST_Of_Current_PU
// 
// Description: Change some globals to reflect mangling of ST for an entry
//
// =====================================================================

static void
Change_ST_Of_Current_PU(ST *new_entry_st)
{
    // The idea is to change all the ST_IDX's in globals from the non-
    // mangled version of the PU to the mangled version, new_entry_st.
    // Hopefully we are catching all the global instances here.
  Current_PU_Info->proc_sym = ST_st_idx(new_entry_st);
  Scope_tab[CURRENT_SYMTAB].st = new_entry_st;
}


// =====================================================================
// 
// Function Name: DRA_Mangle_Entry
// 
// Description: Mangle the name of a function entry point
//
// =====================================================================

static void
DRA_Mangle_Entry(WN* entry_wn, 
                 INT16 num_formals, 
                 DRA_HASH_TABLE* dra_table, 
                 DST_ASSOC_INFO* assoc_info)
{
  BOOL has_reshaped_formals = FALSE;
  
  UINT bufsize = DRA_Preprocess_Entry(entry_wn, 
                                      num_formals, 
                                      dra_table, 
                                      &has_reshaped_formals);
 
  if (has_reshaped_formals) {

    // Allocate the buffer and insert mangling signature
    //
    char *mangled_buf = (char *) alloca(bufsize);
    char *entry_name = ST_name(WN_st(entry_wn));
    char *mangled_ptr = DRA_Insert_Mangling_Sigs(entry_name, mangled_buf);

    // Write the encoding of all formal parameters
    //
    for (INT16 formal = 0; formal < num_formals; ++formal) {

      ST *formal_st = WN_st(WN_formal(entry_wn, formal));
      TY_IDX formal_ty = Get_Array_Type(formal_st);

      mangled_ptr = DRA_Encode_Parameter(formal_st, 
                                         formal_ty, 
                                         mangled_ptr, 
                                         dra_table);
      *mangled_ptr++ = DRA_ARG_SEPARATOR;
    }
  
    *mangled_ptr = '\0';


    // Remember the old ST entry in order to make it undefined
    //
    ST *old_st = WN_st(entry_wn);
    ST *entry_st = old_st;

    // Lookup the mangled name in the hash table
    //
    MANGLED_FUNC *entry_desc = DRA_func_table->Find(Save_Str(mangled_buf));

    if (entry_desc == NULL) { // name not found

      if (strcmp(entry_name, mangled_buf) != 0) {

        // For a new mangled name, create an ST entry
        //
        entry_st = Copy_ST(entry_st, TRUE);
        WN_st_idx(entry_wn) = ST_st_idx(entry_st);
        Set_ST_name(entry_st, Save_Str(mangled_buf));
        Clear_ST_is_not_used(entry_st);
      }

      // Enter the (name, ST-entry) pair in the hash table
      //
      entry_desc = CXX_NEW(MANGLED_FUNC, &MEM_src_pool);
      entry_desc->st = entry_st;
      entry_desc->is_clone = FALSE;
      entry_desc->is_called = FALSE;
      DRA_func_table->Enter(ST_name_idx(entry_st), entry_desc);
    }

    else { // name found
      entry_st = entry_desc->st;
      WN_st_idx (entry_wn) = ST_st_idx (entry_st);
      Set_ST_sclass (entry_st, SCLASS_TEXT);
      // nenad, 05/06/98:
      // Must set the base index properly
      if (Run_cg) {
        Set_ST_base (entry_st, ST_base(old_st));
      }
    }

    // Update ST index in DST
    //
    if (assoc_info != NULL) {
      pDST_ASSOC_INFO_st_idx(assoc_info) = ST_st_idx(entry_st);
    }

    // Make old ST entry undefined
    //
    if (entry_st != old_st) {
      Change_ST_Of_Current_PU(entry_st);
      Set_ST_sclass (old_st, SCLASS_EXTERN);
      if (!PU_has_non_mangled_call (Pu_Table[ST_pu (old_st)]))
        Set_ST_is_not_used (old_st);
      else if (Run_cg) {
        CG_Change_Elf_Symbol_To_Undefined(old_st);
      }
    }
  }
  else {
    Set_PU_has_non_mangled_call(Pu_Table[ST_pu(WN_st(entry_wn))]);
    Clear_ST_is_not_used(WN_st(entry_wn));
  }
}




// =====================================================================
// 
// Function Name: DRA_Mangle_Call_Site
// 
// Description: Mangle the name of a fucntion/subroutine at a call site
//
// =====================================================================

static void
DRA_Mangle_Call_Site(WN *call_wn, 
                     OPERATOR call_oper, 
                     DRA_HASH_TABLE *dra_table)
{
  // Don't mangle calls to DSM intrinsics
  //
  if (call_oper == OPR_CALL &&
      strncmp(ST_name(WN_st(call_wn)), "dsm_", 4) == 0) {
    return;
  }

  // Don't mangle calls to C IO routines
  //
  if (call_oper == OPR_CALL &&
      (strcmp (ST_name(WN_st(call_wn)), "printf") == 0 ||
       strcmp (ST_name(WN_st(call_wn)), "fprintf") == 0 ||
       strcmp (ST_name(WN_st(call_wn)), "sprintf") == 0 ||
       strcmp (ST_name(WN_st(call_wn)), "vprintf") == 0 ||
       strcmp (ST_name(WN_st(call_wn)), "vfprintf") == 0 ||
       strcmp (ST_name(WN_st(call_wn)), "vsprintf") == 0 ||
       strcmp (ST_name(WN_st(call_wn)), "scanf") == 0 ||
       strcmp (ST_name(WN_st(call_wn)), "sscanf") == 0 ||
       strcmp (ST_name(WN_st(call_wn)), "fscanf") == 0)) {
    return;
  }

  BOOL has_reshaped_args = FALSE;
  INT16 num_args = WN_num_actuals(call_wn);

  UINT bufsize = DRA_Preprocess_Call_Site(call_wn,
                                          num_args,
                                          dra_table,
                                          &has_reshaped_args);
  
  if (has_reshaped_args) {

    // Indirect procedure calls are not allowed to 
    // have distribute-reshaped array arguments
    //
    if (call_oper == OPR_ICALL) {
      ErrMsgSrcpos(EC_DRA_indirect_call, WN_Get_Linenum(call_wn));
      return;
    }
    
    // Allocate the buffer and insert mangling signature
    //
    char *mangled_buf = (char *) alloca(bufsize);
    char *call_name = ST_name(WN_st(call_wn));
    char *mangled_ptr = DRA_Insert_Mangling_Sigs(call_name, mangled_buf);

    // Write the encoding of the arguments
    //
    for (INT16 arg = 0; arg < num_args; ++arg) {

      // Kid 0 of OPC_PARM is the actual argument
      //
      WN *arg_wn = WN_kid(WN_actual(call_wn,arg), 0);

      if (WN_operator(arg_wn) == OPR_LDA ||
          WN_operator(arg_wn) == OPR_LDID) {
      
        ST *arg_st = WN_st(arg_wn);
        TY_IDX arg_ty = Get_Array_Type(arg_st);

        mangled_ptr = DRA_Encode_Parameter(arg_st, 
                                           arg_ty, 
                                           mangled_ptr,
                                           dra_table);
      }
      else {
        // non-LDA arguments are expressions -- treat them as scalars
        //
        *mangled_ptr++ = '0'; 
      }
      *mangled_ptr++ = DRA_ARG_SEPARATOR;
    }
    *mangled_ptr = '\0';

    
    // Remember the old ST entry in order to mark it unused
    //
    ST *old_st = WN_st(call_wn);

    // Lookup the mangled name in the hash table
    //
    MANGLED_FUNC *call_desc = DRA_func_table->Find(Save_Str(mangled_buf));

    if (call_desc == NULL) { // name not found

      // If the mangled name is new, create new PU/ST entries
      //
      PU_IDX new_pu_idx;
      PU& new_pu = New_PU (new_pu_idx);
      Pu_Table[new_pu_idx] = Pu_Table[ST_pu(old_st)];
      Set_PU_no_inline (new_pu);
    
      ST *new_st = Copy_ST(old_st);
      WN_st_idx (call_wn) = ST_st_idx (new_st);
      Clear_ST_is_not_used (new_st);

      // Set the PU index for the new ST
      //
      Set_ST_pu (new_st, new_pu_idx);
      
      // Store the new (now mangled) name
      //
      Set_ST_name (new_st, Save_Str(mangled_buf));

      // Set the storage class to EXTERN (no definition yet)
      //
      Set_ST_sclass (new_st, SCLASS_EXTERN);

      // Copy_ST may set base_idx to .text block, and
      // that's not allowed for EXTERN storage class
      Set_ST_base (new_st, new_st);
      Set_ST_ofst (new_st, 0); 

      // Remember the name in the local hash table
      //
      call_desc = CXX_NEW(MANGLED_FUNC, &MEM_src_pool);
      call_desc->st = new_st;
      call_desc->is_called = TRUE;
      DRA_func_table->Enter(ST_name_idx(new_st), call_desc);
    } else {
      // If the name is found, use its ST pointer
      //
      WN_st_idx (call_wn) = ST_st_idx (call_desc->st);
      call_desc->is_called = TRUE;
    }

    // Mark old ST entry as unused if there were no other calls to it
    //
    if (!PU_has_non_mangled_call (Pu_Table[ST_pu (old_st)]))
      Set_ST_is_not_used (old_st);
  }
  
  else {
    // No reshaped arrays are passed at this call
    //
    if (call_oper == OPR_CALL) {
      ST* st = WN_st (call_wn);
      Set_PU_has_non_mangled_call (Pu_Table[ST_pu (st)]);
      Clear_ST_is_not_used (st);
    }
  }
}




// =====================================================================
// 
// Function Name: DRA_Preprocess_Entry
// 
// Description: Compute the mangling buffer size for a function entry 
//              point and determine if it has reshaped formals
//
// =====================================================================

static UINT
DRA_Preprocess_Entry(WN *entry_wn, 
                     INT16 num_formals, 
                     DRA_HASH_TABLE *dra_table, 
                     BOOL *has_reshaped_formals)
{
  UINT bufsize = strlen(ST_name(WN_st(entry_wn))) + 2*DRA_MANGLE_SIG_LEN + 1;
  
  for (INT16 formal = 0; formal < num_formals; ++formal) {

    ST *formal_st = WN_st(WN_formal(entry_wn, formal));
    TY_IDX formal_ty = Get_Array_Type(formal_st);

    if (TY_kind(formal_ty) != KIND_ARRAY) { 
      bufsize += 2;
    }
    else {
      DRA_INFO *dra = dra_table->Find(formal_st);
      if (dra == NULL) { 
        bufsize += 2;
      }
      else {
        *has_reshaped_formals = TRUE;
        
        // For encoding a reshaped array argument, we need:
        // <ndims(5)>D<esize(21)>E<distributions(ndims x 22)> chars
        //
        bufsize += 28 + 22*TY_AR_ndims(Ty_Table[formal_ty]);
      }
    }
  }

  return bufsize;
}




// =====================================================================
// 
// Function Name: DRA_Preprocess_Call_site
// 
// Description: Compute the mangling buffer size for a call site
//              point and determine if it has reshaped arguments
//
// =====================================================================

static UINT
DRA_Preprocess_Call_Site(WN *call_wn, 
                         INT16 num_args, 
                         DRA_HASH_TABLE *dra_table,
                         BOOL *has_reshaped_args)
{
  UINT bufsize = 0;
  if (WN_operator(call_wn) == OPR_CALL) {
    bufsize = strlen(ST_name(WN_st(call_wn))) + 2*DRA_MANGLE_SIG_LEN + 1;
  }
  
  for (INT16 arg = 0; arg < num_args; ++arg) {

    // Kid 0 of OPC_PARM is the actual argument
    //
    WN *parm_wn = WN_actual(call_wn, arg);
    WN *arg_wn = WN_kid(parm_wn, 0);

    if (WN_operator(arg_wn) == OPR_LDA ||
        WN_operator(arg_wn) == OPR_LDID) {
      
      ST *arg_st = WN_st(arg_wn);
      TY_IDX arg_ty = Get_Array_Type(arg_st);

      if (TY_kind(arg_ty) != KIND_ARRAY) { 
        bufsize += 2;
      }
      else {
        DRA_INFO *dra = dra_table->Find(arg_st);
        if (dra == NULL) { 
          bufsize += 2;
        }
        else {
          *has_reshaped_args = TRUE;
        
          // For encoding a reshaped array argument, we need:
          // <ndims(5)>D<esize(21)>E<distributions(ndims x 22)> chars
          //
          bufsize += 28 + 22*TY_AR_ndims(Ty_Table[arg_ty]);
        }
      }
    }
    else {
      bufsize += 2;
    }
  }
  
  return bufsize;
}




// =====================================================================
// 
// Function Name: DRA_Insert_Mangling_Sigs
// 
// Description: Insert DRA_MANGLE_SIG before and after the original 
//              function name if that had not already been done
//
// =====================================================================

static char*
DRA_Insert_Mangling_Sigs(char *orig_name,
                         char mangled_buf[])
{
  
  if (strncmp(orig_name, DRA_MANGLE_SIG, DRA_MANGLE_SIG_LEN)) {
    // If the function name has not been mangled, do it now
    //
    (void) strcpy(mangled_buf, DRA_MANGLE_SIG);
    (void) strcpy(mangled_buf + DRA_MANGLE_SIG_LEN, orig_name);
    (void) strcpy(mangled_buf + DRA_MANGLE_SIG_LEN + strlen(orig_name),
                  DRA_MANGLE_SIG);
  }
  else {
    // Otherwise, extract the sigs and the original function name
    //
    INT name_len = strstr(orig_name+1, DRA_MANGLE_SIG)
                   + DRA_MANGLE_SIG_LEN - orig_name;
    (void) strncpy(mangled_buf, orig_name, name_len);
    mangled_buf[name_len] = '\0';
  }

  return (mangled_buf + strlen(mangled_buf));
}




// =====================================================================
// 
// Function Name: DRA_Encode_Parameter
// 
// Description: Encode a function parameter into mangled name
//              Scalars and non-reshaped arrays: 0
//              Distribute-reshaped arrays: <ndims>D<esize>E{S|B|C[chunk]}*
//
// =====================================================================

static char*
DRA_Encode_Parameter(ST *st,                // ST pointer for the parameter
                     TY_IDX ty_idx,         // TY pointer for the parameter
                     char *buf,             // encoding buffer 
                     DRA_HASH_TABLE *dra_table)
{
  TY& ty = Ty_Table[ty_idx];
  // Encoding for all non-arrays is '0'
  //
  if (TY_kind(ty) != KIND_ARRAY) { 
    *buf++ = '0';                  
    return buf;
  }

  DRA_INFO *dra = dra_table->Find(st);

  // Encoding for non-reshaped arrays is '0'
  //
  if (dra == NULL) {              
    *buf++ = '0';                  
    return buf;
  }

  // Now encode reshaped array
  //
  INT16 ndims = ARB_dimension (TY_arb(ty));
  UINT64 size = TY_size (TY_etype(ty));
  buf += sprintf(buf, "%d%c", ndims, DRA_NDIMS_END);
  buf += sprintf(buf, "%lld%c", size, DRA_ESIZE_END);

  for (INT16 dim = 0; dim < ndims; ++dim) {

    switch (dra->Distr_Type(dim)) {

      case DISTRIBUTE_STAR:  
        *buf++ = DRA_STAR_CODE;
        break;

      case DISTRIBUTE_BLOCK:
        *buf++ = DRA_BLOCK_CODE;
        break;

      case DISTRIBUTE_CYCLIC_CONST:
        *buf++ = DRA_CYCLIC_CODE;
        buf += sprintf(buf, "%lld", dra->Chunk_Const_Val(dim));
        break;

      case DISTRIBUTE_CYCLIC_EXPR:
        *buf++ = DRA_CYCLIC_CODE;
        break;
    }
  }
  
  return buf;
}
