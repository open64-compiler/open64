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


// ====================================================================
// ====================================================================
//
// Module: dra_clone.cxx
//
// Revision history:
//  16-Jul-96: Original Version
//
// Description:
//  Routines used in cloning of subroutines based on
//  the distribution of reshaped array arguments.
//
// ====================================================================
// ====================================================================

#define ONST(x, y) (y)

#include <alloca.h>             // alloca
#include <unistd.h>             // write
#include <stdint.h>

#include "pu_info.h"            // PU_Info

#include "defs.h"               // Standard definitions
#include "config_asm.h"         // Temp_Symbol_Prefix
#include "wn.h"                 // WN
#include "wn_map.h"             // Current_Map_Tab
#include "wn_util.h"            // WN_INSERT_BlockAfter
#include "symtab.h"
#include "strtab.h"             // Save_Str
#include "mempool.h"             // MEM_POOL
#include "cxx_memory.h"         // CXX_NEW
#include "erbe.h"               // EC_*
#include "errors.h"             // ErrMsg, ErrMsgSrcpos
#include "dwarf_DST_mem.h"      // DST_IDX
#include "clone.h"              // IPO_CLONE
#include "clone_DST_utils.h"    // DST_enter_cloned_subroutine
#include "dra_demangle.h"       // DRA_Demangle

#include "dra_internal.h"       // Internal DRA interface



// =====================================================================
// 
//                      Local function prototypes
//
// =====================================================================

static BOOL DRA_Clone_Initialize(void);

static BOOL DRA_Process_Requests(char *tir_names);

static BOOL DRA_Parse_Clone_Name(char *clone_name);

static void DRA_Clone_Instantiate(PU_Info *orig_pu,
                                  BOOL pu_has_feedback,
                                  STRING_LIST *tir_list,
                                  DRA_HASH_TABLE *dra_table);

static char* DRA_New_Clone_Sig(WN *pu_wn,
                               char *clone_name,
                               DRA_HASH_TABLE *dra_table);

static void DRA_Add_Clone (PU_Info *orig_pu, 
                           MEM_POOL *mem_pool,
                           STR_IDX clone_name,
                           char *arg_sig,
                           BOOL pu_has_feedback);

static void DRA_Insert_Pragmas(WN *pu_wn,
                               char *arg_sig);

static void DRA_Process_Commons(DRA_HASH_TABLE *dra_table,
                                DRA_COMMON_HASH_TABLE*);

static void DRA_Collect_Commons(WN *pu,
                                DRA_COMMON_HASH_TABLE *dra_common_ht);

static void DRA_Process_Globals(DRA_HASH_TABLE *dra_table);

static BOOL DRA_Info_Matches_Encoding(DRA_INFO *dra, 
                                      char *arg_sig);

// ===================================================================== 
// 
//                          Exported variables
//
// =====================================================================

BOOL Run_Dsm_Cloner = FALSE;

BOOL Run_Dsm_Common_Check = FALSE;

BOOL Run_Dsm_Check = FALSE;

NAME_ST_TABLE *DRA_func_table = NULL;

MEM_POOL DRA_name_pool, *DRA_name_pool_ptr = NULL;

DRA_GLOBAL_HASH_TABLE* dra_global = NULL;

// ===================================================================== 
// 
//                      File static variables
//
// =====================================================================

static STRING_LIST_TABLE *DRA_clone_table;

static MEM_POOL DRA_clone_pool, *DRA_clone_pool_ptr = NULL;

static MEM_POOL DRA_check_pool, *DRA_check_pool_ptr = NULL;


// =====================================================================
// 
//                      Exported function definitions
//
// =====================================================================


// =====================================================================
//
// Function Name: Get_Orig_Type
//
// Description: Given an ST return the ST_type it originally had.
//      Same as ST_type, except when called on reshaped globals 
//      whose type has been mangled.
//
// =====================================================================

extern TY_IDX Get_Original_Type (ST* st) {

  TY_IDX ty;

  if (ST_class(st) != CLASS_VAR) return ST_type(st);

  if (ONST(ST_is_global(st),ST_level(st) == GLOBAL_SYMTAB) &&
      ST_is_reshaped(st)) {

    DRA_GLOBAL_INFO* dgi = dra_global->Find(st);

    if (dgi) {
      // has been seen before
      ty = dgi->Get_TY();
    }
    else {
      // seeing it for the first time
      ty = ST_type(st);
      DRA_GLOBAL_INFO* dgi = CXX_NEW (DRA_GLOBAL_INFO(ty), Malloc_Mem_Pool);
      dra_global->Enter (st, dgi);
    }
  }
  else {
    ty = ST_type(st);
  }
  return ty;
}

// =====================================================================
//
// Function Name: Get_Array_Type
//
// Description: Given the ST for a distributed array, return the array TY.
//
// =====================================================================

extern TY_IDX Get_Array_Type (ST* st) {

  TY_IDX ty;

  ty = Get_Original_Type (st);

  if (TY_kind(ty) == KIND_POINTER &&
      (ST_sclass(st) == SCLASS_FORMAL ||
       ST_sclass(st) == SCLASS_AUTO ||
       (ONST(ST_sclass(st)==SCLASS_BASED, ST_base_idx(st)==ST_st_idx(st)) &&
        ST_sclass(ST_base(st)) == SCLASS_AUTO))) {
    ty = TY_pointed(ty);
  }

  return ty;
}


extern "C" void
DRA_Initialize(void)
{
  DRA_Open_And_Map_File();

  if (Run_Dsm_Cloner && DRA_Clone_Initialize()) {
    Set_FILE_INFO_needs_lno (File_info);
  }

  if (Run_Dsm_Common_Check) {
    MEM_POOL_Initialize (&DRA_check_pool, "DRA Common Check", TRUE);
    DRA_check_pool_ptr = &DRA_check_pool;
    DRA_Set_Write_Location();
  }

  if (Run_Dsm_Check) {
    DRA_EC_Declare_Types();
  }

  // information about globals must survive PUs
  dra_global = CXX_NEW (DRA_GLOBAL_HASH_TABLE(20, Malloc_Mem_Pool),
                        Malloc_Mem_Pool);
}




// =====================================================================
//
// Function Name: DRA_Finalize
//
// Description: Pop and Delete DRA_clone_pool if necessary
//
// =====================================================================

extern "C" void
DRA_Finalize (void)
{
  ST *st;

  // delete info about distributed globals
  //
  {
    HASH_TABLE_ITER<ST*, DRA_GLOBAL_INFO*> iter (dra_global);
    ST* st;
    DRA_GLOBAL_INFO* dgi;
    while (iter.Step (&st, &dgi)) {
      CXX_DELETE (dgi, Malloc_Mem_Pool);
    }
    CXX_DELETE (dra_global, Malloc_Mem_Pool);
    dra_global = NULL;
  }

  // Make the symbols that are not used invisible
  //
  INT i;
  FOREACH_SYMBOL (GLOBAL_SYMTAB, st, i) {
    if (ST_is_not_used(st) &&
        ST_class(st) == CLASS_FUNC &&
        ST_sclass(st) == SCLASS_EXTERN &&
        !PU_has_non_mangled_call(Pu_Table[ST_pu(*st)])) {
      Set_ST_export(st, EXPORT_LOCAL);
      Set_ST_sclass(st, SCLASS_TEXT);
    }
  }

  // Emit type 'N' symbols for all cloned functions 
  // that are referenced in the same file
  //
  if (DRA_func_table != NULL) {

    NAME_ST_TABLE_ITER iter(DRA_func_table);
    STR_IDX func_name;
    MANGLED_FUNC *func_desc;

    while (iter.Step(&func_name, &func_desc)) {

      if (func_desc->is_clone && func_desc->is_called) {
        ST* aux_st = New_ST (GLOBAL_SYMTAB);
        ST_Init (aux_st,
                 func_name,
                 CLASS_NAME,
                 SCLASS_UNKNOWN,
                 EXPORT_LOCAL,
                 (TY_IDX) NULL);
        Set_ST_is_not_used(aux_st);
        Set_ST_emit_symbol(aux_st);
      }
    }
  }

  if (DRA_clone_pool_ptr != NULL) {
    MEM_POOL_Pop (DRA_clone_pool_ptr);
    MEM_POOL_Delete (DRA_clone_pool_ptr);
    DRA_clone_pool_ptr = NULL;
  }

  if (DRA_check_pool_ptr != NULL) {
    MEM_POOL_Delete (DRA_check_pool_ptr);
    DRA_check_pool_ptr = NULL;
  }
  
  DRA_Close_File();
}




// =====================================================================
//
// Function Name: DRA_Processing
//
// Description: Main driver for DRA related tasks - reading pragmas,
//              cloning, name mangling, and common block processing
//
// =====================================================================

extern "C" void
DRA_Processing(PU_Info *pu_info,
               WN* pu,
               BOOL pu_has_feedback)
{
  STRING_LIST *clone_requests = NULL;
  DRA_HASH_TABLE *dra_table = NULL;

  Set_Error_Phase("DRA Processing");

  if (Run_Dsm_Cloner) {
    clone_requests = DRA_clone_table->Find(ST_name_idx(WN_st(pu)));
  }

  if (clone_requests != NULL ||                 // we need to clone
      Run_Dsm_Common_Check   ||                 // we need to process commons
      ONST(SYMTAB_mp_needs_lno(Current_Symtab), // we need to mangle names
           PU_mp_needs_lno (Get_Current_PU()))) {

    // Initialize and push DRA_name_pool to be used for DRA_HASH_TABLE
    //
    DRA_name_pool_ptr = &DRA_name_pool;
    MEM_POOL_Initialize (DRA_name_pool_ptr, "DRA Names", FALSE);
    MEM_POOL_Push (DRA_name_pool_ptr);

    // Create dra_table that stores the info about all DRA's
    //
    dra_table = CXX_NEW(DRA_HASH_TABLE(31, DRA_name_pool_ptr), 
                        DRA_name_pool_ptr);

    DRA_Read_Pragmas(pu, dra_table);
  }
  
  if (clone_requests != NULL) {
    DRA_Clone_Instantiate(pu_info, pu_has_feedback, clone_requests, dra_table);
  }
      
  if (Run_Dsm_Common_Check) {
    MEM_POOL_Push (DRA_check_pool_ptr);
    DRA_COMMON_HASH_TABLE
      *dra_common_ht = CXX_NEW (DRA_COMMON_HASH_TABLE(20, DRA_check_pool_ptr),
                                DRA_check_pool_ptr);

    DRA_Collect_Commons(pu, dra_common_ht);
    DRA_Process_Commons(dra_table, dra_common_ht);

    CXX_DELETE (dra_common_ht, DRA_check_pool_ptr);
    MEM_POOL_Pop (DRA_check_pool_ptr);

    // Also write out information about globals (C, C++) into rii_file
    //
    DRA_Process_Globals(dra_table);
  }

  if (dra_table->Num_Entries() > 0) {
    DRA_Mangle_All(pu, dra_table, pu_info);   
    Set_PU_no_inline(Pu_Table[ST_pu(WN_st(pu))]);
  }
  else {
    Set_PU_has_non_mangled_call(Pu_Table[ST_pu(WN_st(pu))]);
    Clear_ST_is_not_used(WN_st(pu));
  }

  if (Run_Dsm_Check) {
    DRA_EC_Array_Portion_Parms(pu, pu);
    
    if (ONST(SYMTAB_has_altentry(Current_Symtab),
             PU_has_altentry (Get_Current_PU()))) {
      // Walk the tree and process alternate entry points
      //
      WN_ITER *wni;
      for (wni = WN_WALK_TreeIter(pu); wni; wni = WN_WALK_TreeNext(wni)) {
        if (WN_opcode(WN_ITER_wn(wni)) == OPC_ALTENTRY) {
          DRA_EC_Array_Portion_Parms(pu, WN_ITER_wn(wni));
        }
      } 
    }
  }
  
  // Pop and Delete DRA_name_pool
  //
  if (DRA_name_pool_ptr != NULL) {
    MEM_POOL_Pop (DRA_name_pool_ptr);
    MEM_POOL_Delete (DRA_name_pool_ptr);
    DRA_name_pool_ptr = NULL;
  }
}



  
// =====================================================================
// 
//                      Local function definitions
//
// =====================================================================


// =====================================================================
//
// Function Name: DRA_Clone_Initialize
//
// Description: Process .rii file and return TRUE if Template
//              Instatiation Requests (TIR's) have been found.
//
// =====================================================================

static BOOL 
DRA_Clone_Initialize(void)
{
  // Initialize DRA_clone_pool to be used for cloning
  // This MEM_POOL lives throughout the compilation of the file
  //
  MEM_POOL_Initialize (&DRA_clone_pool, "DRA Cloning", TRUE);
  DRA_clone_pool_ptr = &DRA_clone_pool;
  MEM_POOL_Push (DRA_clone_pool_ptr);
  
  // From now on use DRA_file_mmap as a normal memory pointer
  //
  char *tir_names = strstr(DRA_file_mmap, DRA_FILE_SEPARATOR)
                  + strlen(DRA_FILE_SEPARATOR);

  // Allocate the TIR name table
  // Use DRA_clone_pool because the table must live across all PU's
  //
  DRA_clone_table = CXX_NEW(STRING_LIST_TABLE(31, DRA_clone_pool_ptr), 
                            DRA_clone_pool_ptr);

  // and store all TIR names in it
  //
  BOOL needs_cloning = DRA_Process_Requests(tir_names);

  // Allocate the global name/ST hash table used for resolving names
  //
  if (DRA_func_table == NULL) {
    DRA_func_table = CXX_NEW(NAME_ST_TABLE(31, &MEM_src_pool), 
                             &MEM_src_pool);
  }

  DRA_Mem_Unmap_File();

  return needs_cloning;
}


  

// =====================================================================
//
// Function Name: DRA_Process_Requests
//
// Description: Read the TIR names from .rii file and store them
//              into a hash table. Keys are the original names of
//              functions, while the entries represent linked lists
//              of the names that need to be instantiated.
//
// =====================================================================

static BOOL
DRA_Process_Requests(char *tir_name)
{
  BOOL needs_cloning = FALSE;
  
  // Replace "----" with the string terminator '\0'
  //
  char *end_tir_names = strstr(tir_name, DRA_FILE_SEPARATOR);
  if (end_tir_names != NULL) {
    *end_tir_names = '\0';
  }


  char *end_of_line;
  for ( ; *tir_name; *end_of_line = '\n', tir_name = end_of_line+1) {
    
    // find the of the line
    //
    if ((end_of_line = strchr(tir_name, '\n')) == NULL) {
      break;
    }

    // replace eol with the string terminator
    //
    *end_of_line = '\0';

    // Parse tir_name for correctness
    //
    if (!DRA_Parse_Clone_Name(tir_name)) {
      (void) unlink(DRA_file_name);
      ErrMsg(EC_DRA_rii_file_format, DRA_file_name);
      return FALSE;
    }

    char *orig_name = tir_name + DRA_MANGLE_SIG_LEN;
    char *postfix_sig = strstr(orig_name, DRA_MANGLE_SIG);

    STR_IDX save_tir_name = Save_Str(tir_name);

    *postfix_sig = '\0';

    STR_IDX save_orig_name = Save_Str(orig_name);

    // restore original contents of overwritten location
    //
    *postfix_sig = DRA_MANGLE_SIG[0];


    // Get the list of TIR's corresponding to the original function
    //
    STRING_LIST *tir_list = DRA_clone_table->Find(save_orig_name);

    // If it has been created, do it now
    //
    if (tir_list == NULL) { 
      tir_list = CXX_NEW(STRING_LIST(), DRA_clone_pool_ptr);
      DRA_clone_table->Enter(save_orig_name, tir_list);
    }

    // Add the tir name to the list
    //
    STRING_NODE *tir_node = 
      CXX_NEW(STRING_NODE(save_tir_name), DRA_clone_pool_ptr);
    tir_list->Append(tir_node);

    needs_cloning = TRUE;
  }


  // restore original contents of overwritten locations
  //
  if (end_tir_names != NULL) {
    *end_tir_names = DRA_FILE_SEPARATOR[0];
  }
  
  return needs_cloning;
}




// =====================================================================
//
// Function Name: DRA_Parse_Clone_Name
//
// Description: Parse the name read from .rii file to make sure
//              it can be used to generate meaningful pragmas,
//
// =====================================================================

static BOOL
DRA_Parse_Clone_Name(char *clone_name)
{
  // Check for DRA mangling prefix
  //
  if (strncmp(clone_name, DRA_MANGLE_SIG, DRA_MANGLE_SIG_LEN) != 0)
    return FALSE;
    
  char *arg_sig = strstr(clone_name + DRA_MANGLE_SIG_LEN, DRA_MANGLE_SIG);
  
  // Check for DRA mangling suffix
  //
  if (arg_sig == NULL || *(arg_sig += DRA_MANGLE_SIG_LEN) == 0)
    return FALSE;


  // Check the parameter list
  //
  for ( ; *arg_sig; ) {
    
    char *current;

    // Check number of dimensions:
    // INT16; non-negative; if 0, it must be followed by _
    //
    INT64 num_dims = (INT64) strtol(arg_sig, &current, 10);

    if (current == arg_sig) 
      return FALSE;

    if (num_dims == 0) {
      if (*current++ != DRA_ARG_SEPARATOR)
        return FALSE;
      else {
        arg_sig = current;
        continue;
      }
    }
      
    if (num_dims < 0 || num_dims > INT16_MAX)
      return FALSE;

    // Check array element size:
    // INT64; positive; must be surrounded by D and E
    //
    if (*current++ != DRA_NDIMS_END)
      return FALSE;

    arg_sig = current;
    
    INT64 esize = (INT64) strtol(arg_sig, &current, 10);
    
    if (current == arg_sig || esize <= 0 || *current++ != DRA_ESIZE_END)
      return FALSE;
    
    arg_sig = current;
    
    // Check distributions in all dimensions:
    // B, C, or S; C may be followed by a positive INT64
    //
    for (INT16 dim = 0; dim < num_dims; dim++) {

      if (*arg_sig == DRA_BLOCK_CODE || *arg_sig == DRA_STAR_CODE) {
        arg_sig++;
        continue;
      }
      
      else if (*arg_sig == DRA_CYCLIC_CODE) {
        
        if (arg_sig[1] == DRA_BLOCK_CODE || 
            arg_sig[1] == DRA_STAR_CODE || 
            arg_sig[1] == DRA_CYCLIC_CODE ||
            (arg_sig[1] == DRA_ARG_SEPARATOR && dim == num_dims-1)) {
          arg_sig++;
          continue;
        }
        
        arg_sig++;

        INT64 chunk = (INT64) strtol(arg_sig, &current, 10);
        
        if (current == arg_sig || chunk <= 0)
          return FALSE;
    
        arg_sig = current;
      }
      
      else 
        return FALSE;
    }

    if (*arg_sig++ != DRA_ARG_SEPARATOR)
      return FALSE;
  }

  return TRUE;
}




// =====================================================================
//
// Function Name: DRA_Clone_Instantiate
//
// Description: Instantiate all the clones found in the TIR table
//              that correspond to the passed PU.
//
// =====================================================================

static void 
DRA_Clone_Instantiate(PU_Info *orig_pu,
                      BOOL pu_has_feedback,
                      STRING_LIST *tir_list,
                      DRA_HASH_TABLE *dra_table)
{
  // The cloner cannot handle routines with alternate entry points
  // 
  if (ONST(SYMTAB_has_altentry(Current_Symtab),
           PU_has_altentry(Get_Current_PU()))) {
    ErrMsgSrcpos(EC_DRA_clone_altentry, 
                 WN_Get_Linenum(PU_Info_tree_ptr(orig_pu)));
    return;
  }
  
  // Iterate over the string list
  //
  STRING_ITER tir_iter(tir_list);
  STRING_NODE *n;

  for (n = tir_iter.First(); !tir_iter.Is_Empty(); n = tir_iter.Next()) {

    STR_IDX clone_name = n->String();

    // Get clone argument signature that ignores formal parameters
    // that already have DISTRIBUTE_RESHAPE specification.
    //
    char *arg_sig = DRA_New_Clone_Sig(PU_Info_tree_ptr(orig_pu), 
                                      Index_To_Str(clone_name), 
                                      dra_table);

    // NULL signature is used to flag inconsistent cloning requests
    //
    if (arg_sig != NULL) {
      DRA_Add_Clone(orig_pu, 
                    DRA_clone_pool_ptr, 
                    clone_name, 
                    arg_sig,
                    pu_has_feedback);
    }
  }
}




// =====================================================================
//
// Function Name: DRA_New_Clone_Sig
//
// Description: Given a PU and an instantiation request, return the 
//              clone argument signature that ignores formal parameters 
//              that already have DISTRIBUTE_RESHAPE directive. In case
//              of errors, return NULL.
//
// =====================================================================

static char* 
DRA_New_Clone_Sig(WN *pu_wn,
                  char *clone_name,
                  DRA_HASH_TABLE *dra_table)
{
  Set_Error_Phase("Instantiating DRA Clones");

  FmtAssert(strncmp(clone_name, DRA_MANGLE_SIG, DRA_MANGLE_SIG_LEN) == 0,
            ("The name of a DRA clone does not have DRA_MANGLE_SIG prefix"));
    
  char *arg_sig = strstr(clone_name + DRA_MANGLE_SIG_LEN, DRA_MANGLE_SIG);
  
  FmtAssert(arg_sig != NULL,
            ("The name of a DRA clone does not have DRA_MANGLE_SIG postfix"));

  arg_sig += DRA_MANGLE_SIG_LEN;

  char *buf = CXX_NEW_ARRAY(char, strlen(arg_sig)+1, DRA_name_pool_ptr);
  char *bufptr = buf;

  char *dim_sig;


  INT16 arg_pos;
  for ( arg_pos = 0; *arg_sig; arg_sig++, arg_pos++ ) {

    ST *arg_st = WN_st(WN_kid(pu_wn, arg_pos));

    if (arg_st == NULL) {
      // This warning should be deleted once the testing is finished
      //
      ErrMsgSrcpos(EC_DRA_bad_clone_request,
                   WN_Get_Linenum(pu_wn),
                   DRA_Demangle(clone_name, DRA_DIMS_COLUMNWISE),
         "cannot be satisfied -- too many arguments passed or types mismatch");
      // Do not clone in the presence of errors!
      // 
      return NULL;
    }

    // Extract the number of dimensions
    //
    TY_IDX arg_ty = Get_Array_Type(arg_st);
    DRA_INFO *dra = dra_table->Find(arg_st);
    INT16 num_dims = (INT16) strtol (arg_sig, &dim_sig, 10);

    // Do some consistency checking
    //
    if (num_dims == 0) {
      if (dra != NULL) {
        // This warning should be deleted once the testing is finished
        //
        ErrMsgSrcpos(EC_DRA_bad_clone_request,
                     WN_Get_Linenum(pu_wn),
                     DRA_Demangle(clone_name, DRA_DIMS_COLUMNWISE),
                     "cannot be satisfied -- non-reshaped argument passed to reshaped formal parameter");
        // Do not clone in the presence of errors!
        // 
        return NULL;
      } 
      else {
        arg_sig = strchr(arg_sig, DRA_ARG_SEPARATOR);
        *bufptr++ = '0'; 
        *bufptr++ = DRA_ARG_SEPARATOR; 
        continue;
      }
    }
    
    // From now on num_dims must be > 0
    //

    if (TY_kind(arg_ty) != KIND_ARRAY) {
      // This warning should be deleted once the testing is finished
      //
      ErrMsgSrcpos(EC_DRA_bad_clone_request,
                   WN_Get_Linenum(pu_wn),
                   DRA_Demangle(clone_name, DRA_DIMS_COLUMNWISE),
                   "cannot be satisfied -- reshaped argument passed to a non-array formal parameter");
      // Do not clone in the presence of errors!
      // 
      return NULL;
    }
    
    if (num_dims != TY_AR_ndims(arg_ty) ||
        (dra != NULL && num_dims != dra->Num_Dims())) {
      // This warning should be deleted once the testing is finished
      //
      ErrMsgSrcpos(EC_DRA_bad_clone_request,
                   WN_Get_Linenum(pu_wn),
                   DRA_Demangle(clone_name, DRA_DIMS_COLUMNWISE),
                   "cannot be satisfied -- reshaped argument and matching formal parameter have different ranks");
      // Do not clone in the presence of errors!
      // 
      return NULL;
    }


    INT64 elem_size = (INT64) strtol(dim_sig+1, &dim_sig, 10);
    
    if (elem_size != TY_size(TY_AR_etype(arg_ty)) ||
        (dra != NULL && elem_size != dra->Element_Size())) {
      // This warning should be deleted once the testing is finished
      //
      ErrMsgSrcpos(EC_DRA_bad_clone_request,
                   WN_Get_Linenum(pu_wn),
                   DRA_Demangle(clone_name, DRA_DIMS_COLUMNWISE),
                   "cannot be satisfied -- reshaped argument and matching formal parameter have different element sizes");
      // Do not clone in the presence of errors!
      // 
      return NULL;
    }
      
    
    if (dra != NULL) {
      // dim_sig points to 'D'; skip it to process element size first
      //
      if (!DRA_Info_Matches_Encoding(dra, dim_sig+1)) {
        // This warning should be deleted once the testing is finished
        //
        ErrMsgSrcpos(EC_DRA_bad_clone_request,
                     WN_Get_Linenum(pu_wn),
                     DRA_Demangle(clone_name, DRA_DIMS_COLUMNWISE),
                     "cannot be satisfied -- reshaping distributions of arguments and formal parameters do not match");
        // Do not clone in the presence of errors!
        // 
        return NULL;
      }
      else {
        // Ignore this DRA because it's already specified
        //
        arg_sig = strchr(arg_sig, DRA_ARG_SEPARATOR);
        *bufptr++ = '0'; 
        *bufptr++ = DRA_ARG_SEPARATOR; 
        continue;
      }
    }
        
    // dra is NULL, and we need to insert pragma for this parameter

    // Copy ndims (digits before 'D')
    //
    while (*arg_sig != DRA_NDIMS_END) {
      *bufptr++ = *arg_sig++;
    }
    
    // Skip D<esize>E 
    //
    arg_sig++;
    while (*arg_sig++ != DRA_ESIZE_END);

    // Copy distribution encodings
    //
    while (*arg_sig != DRA_ARG_SEPARATOR) {
      *bufptr++ = *arg_sig++;
    }
    *bufptr++ = DRA_ARG_SEPARATOR;
  }

  // Do not clone if no new pragmas are needed
  //
  if (bufptr == buf) {
    return NULL;
  }
  
  *bufptr = '\0'; 


  // If the number of actual arguments is less than the number of 
  // formal parameters, we still clone but also warn the user
  //
  if (arg_pos < WN_num_formals(pu_wn)) {
    ErrMsgSrcpos(EC_DRA_bad_clone_request, 
                 WN_Get_Linenum(pu_wn),
                 DRA_Demangle(clone_name, DRA_DIMS_COLUMNWISE),
                 "has incomplete argument list");
  }
  
  return buf;
}




// =====================================================================
//
// Function Name: DRA_Add_Clone
//
// Description: Clone the PU whose PU_Info structure is passed in
//              and add it to the PU list.
//
//              The assumption is that WT_SYMTAB, WT_TREE, and
//              WT_PROC_SYM sections of the orig_pu are in state 
//              Subsect_InMem. 
// 
//              Returned PU_Info structure is allocated in the
//              Malloc_Mem_Pool. 
//
//              Local objects that disappear after the cloning
//              is finished are allocated from MEM_local_pool.
//
//              Everything else (tree, map tables, symtabs (?),
//              DSTs) is allocated from the mem_pool that is
//              passed in, and the client has control over its
//              life-time.
//
// =====================================================================

static void 
DRA_Add_Clone (PU_Info *orig_pu, 
               MEM_POOL *mem_pool,
               STR_IDX clone_name,
               char *arg_sig,
               BOOL pu_has_feedback)
{
  // Save current pointers to standard memory pools and scope table
  //
  MEM_POOL *save_pu_pool_ptr = MEM_pu_pool_ptr;
  MEM_POOL *save_wn_pool_ptr = WN_mem_pool_ptr;
  
  // Save local symbol table of the original PU, since
  // cloning will overwrite its Scope_tab entry
  //
  Set_PU_Info_symtab_ptr(orig_pu, NULL);
  Save_Local_Symtab(CURRENT_SYMTAB, orig_pu);

  // Use the given mem_pool for WN, ST, etc.
  //
  MEM_pu_pool_ptr = mem_pool;    
  WN_mem_pool_ptr = mem_pool;

  // Define a new IPO_CLONE object
  //
  IPO_CLONE clone(PU_Info_tree_ptr(orig_pu),
                  Scope_tab,
                  CURRENT_SYMTAB,
                  PU_Info_maptab(orig_pu),
                  mem_pool,
                  mem_pool);

  ST* orig_st = ST_ptr(PU_Info_proc_sym(orig_pu));

  // Lookup the clone name in the table of mangled names
  //
  MANGLED_FUNC *clone_desc = DRA_func_table->Find(clone_name);

  // If not found, the new ST entry should be created
  //
  if (clone_desc == NULL) {
    clone_desc = CXX_NEW(MANGLED_FUNC, &MEM_src_pool);

    // Create a PU
    PU_IDX pu_idx;
    PU& pu = New_PU (pu_idx);
    Pu_Table[pu_idx] = Pu_Table[ST_pu(orig_st)];
    
    // Make an ST: add function to global symbol table
    clone_desc->st = New_ST (ST_level(orig_st));
    ST_Init (clone_desc->st, 
             clone_name, 
             CLASS_FUNC, 
             SCLASS_TEXT,
             ST_export(orig_st),
             pu_idx);

    clone_desc->is_called = FALSE;
    DRA_func_table->Enter(clone_name, clone_desc);
  } 
  
  clone_desc->is_clone = TRUE;
  Set_ST_sclass (clone_desc->st, SCLASS_TEXT);
  if (Run_cg) {
    Set_ST_base (clone_desc->st, ST_base(orig_st));
  }

  // This performs actual cloning
  //
  clone.New_Clone(clone_desc->st);

  // Set frequencies to be the same as in the original PU
  //
  if (pu_has_feedback) {
    DevWarn("Need to fix up feedback in DRA_Add_Clone\n");
  }
#if TODO
    FEEDBACK cloned_fb(clone.Get_Cloned_PU(), mem_pool);
    FB_IPA_Clone(Cur_PU_Feedback, clone_node()->Feedback,
                    WN_func_body(Callee_Wn ()), clone.Get_Cloned_PU(),
                    1.0f);
#endif

  // Set the current scope entry to point to the clone
  //
  Scope_tab[CURRENT_SYMTAB] = 
    clone.Get_sym()->Get_cloned_scope_tab()[CURRENT_SYMTAB];
  Scope_tab[CURRENT_SYMTAB].st = clone_desc->st;
  
  // Insert DISTRIBUTE_RESHAPE pragmas based on the argument signature
  //
  DRA_Insert_Pragmas(clone.Get_Cloned_PU(), arg_sig);

  // Generate DST information for the clone
  //
  Set_FILE_INFO_has_inlines (File_info);
  DST_IDX new_pu_dst = 
    DST_enter_cloned_subroutine(DST_get_compile_unit(),
                                PU_Info_pu_dst(orig_pu), 
                                clone.Get_Func_ST(), 
                                Current_DST,
                                clone.Get_sym());
  
  // Alocate and initialize PU_Info structure for the clone
  //
  PU_Info *new_pu = CXX_NEW(PU_Info, Malloc_Mem_Pool);
  PU_Info_init(new_pu);

  // Add new pu right after the original pu
  //
  PU_Info_next(new_pu) = PU_Info_next(orig_pu);
  PU_Info_next(orig_pu) = new_pu;

  // Update the PU pointers and state information
  //
  Set_PU_Info_flags(new_pu, PU_IS_COMPILER_GENERATED);
  Set_PU_Info_flags(new_pu, PU_IS_DRA_CLONE);  
  Set_PU_Info_pu_dst(new_pu, new_pu_dst);

  Set_PU_Info_tree_ptr(new_pu, clone.Get_Cloned_PU());
  PU_Info_proc_sym(new_pu) = ST_st_idx(clone.Get_Func_ST());  
  PU_Info_maptab(new_pu) = clone.Get_Cloned_maptab();

  Set_PU_Info_state(new_pu, WT_TREE,     Subsect_InMem);
  Set_PU_Info_state(new_pu, WT_SYMTAB,   Subsect_InMem);
  Set_PU_Info_state(new_pu, WT_PROC_SYM, Subsect_InMem);

  if (pu_has_feedback) {
    Set_PU_Info_state(new_pu, WT_FREQ, Subsect_InMem);
  }

  // Mark that clone requires LNO processing
  //
  Set_PU_mp_needs_lno(PU_Info_pu(new_pu));

  // Restore Curent_Map_Tab and Current_Symtab to those of the original PU
  //
  Current_Map_Tab = PU_Info_maptab(orig_pu);

  // Restore pointers to standard memory pools
  //
  MEM_pu_pool_ptr = save_pu_pool_ptr;
  WN_mem_pool_ptr = save_wn_pool_ptr;

  // Save local symbol table of the clone
  //
  Set_PU_Info_symtab_ptr(new_pu, NULL);  
  Save_Local_Symtab(CURRENT_SYMTAB, new_pu);

  // Restore local symbol table of the original PU
  //
  Restore_Local_Symtab(orig_pu);
}  



// =====================================================================
//
// Function Name: Find_Insertion_Point
//
// Description: Find the place in the PU where we should start
//              inserting distribute_reshape pragmas. Ordinarily
//              it would be after the preamble, but in C/C++ 
//              there are assignments to __vla_bound variables
//              that can occur after the PREAMBLE. In which case the
//              insertion point must be after the STIDs to those variables.
//
// =====================================================================

static WN* 
Find_Insertion_Point (WN *pu_wn, 
                      char *arg_sig) 
{
  WN *preamble_wn = Get_Preamble_End(pu_wn); 

  if (ONST (SYMTAB_src_lang(Current_Symtab) != SYMTAB_C_LANG &&
            SYMTAB_src_lang(Current_Symtab) != SYMTAB_CXX_LANG,
            PU_src_lang(Get_Current_PU()) != PU_C_LANG &&
            PU_src_lang(Get_Current_PU()) != PU_CXX_LANG)) {
    return preamble_wn;
  }

  WN *current_wn = preamble_wn;

  for (INT16 arg_pos = 0; *arg_sig; arg_sig++, arg_pos++ ) {

    // Extract the number of dimensions
    //
    INT16 num_dims = (INT16) strtol (arg_sig, &arg_sig, 10);
    ST *arg_st = WN_st(WN_kid(pu_wn, arg_pos));
    TY_IDX arg_ty = Get_Array_Type(arg_st);

    for (INT16 dim = 0; dim < num_dims; dim++) {

      if (*arg_sig++ == DRA_CYCLIC_CODE) {
        INT64 chunk = (INT64) strtol (arg_sig, &arg_sig, 10);
      }
      // For each dimension see if the bound is __vla_bound
      //
      if (!TY_AR_const_ubnd(arg_ty, num_dims-1-dim) &&
          TY_AR_ubnd_val(arg_ty, num_dims-1-dim) &&
          strncmp(ST_name(TY_AR_ubnd_var(arg_ty, num_dims-1-dim)),
                  Temp_Symbol_Prefix "__vla_bound",
		  sizeof (Temp_Symbol_Prefix "__vla_bound") - 1 ) == 0) {

        ST* vlabound_st = ONST(WN_st(TY_AR_ubnd_tree(arg_ty, dim)),
                               &(St_Table[TY_AR_ubnd_var(arg_ty, num_dims-1-dim)]));
        
        // simple LDID for upper bound of __vla_bound
        // Find the STID in the tree
        BOOL saw_preamble = FALSE;
        BOOL saw_current = FALSE;
        WN *wn = WN_first(WN_func_body(pu_wn));

        while (wn) {

          if (WN_operator(wn) == OPR_PRAGMA &&
              WN_pragma(wn) == WN_PRAGMA_PREAMBLE_END) {
            saw_preamble = TRUE;
          }
          if (wn == current_wn) saw_current = TRUE;

          if (WN_operator(wn) == OPR_STID &&
              WN_st(wn) == vlabound_st) {

            if (saw_preamble && saw_current) {
              // we must move current_wn
              //
              current_wn = wn;

              // see if we're followed by an XPRAGMA-COPYIN
              if (WN_next(wn) &&
                  WN_operator(WN_next(wn)) == OPR_XPRAGMA &&
                  WN_operator(WN_kid0(WN_next(wn)))==OPR_LDID &&
                  WN_st(WN_kid0(WN_next(wn))) == vlabound_st) {

                current_wn = WN_next(wn);
              }
            }
            else {
              // don't need to do anything
            }
            break;
          }
          wn = WN_next(wn);
        }

        FmtAssert (wn,
                   ("Find_Insertion_Point: No STID vla_bound for %s\n",
                    ST_name(arg_st)));
      }
    } 
  }
  return current_wn;
}


// =====================================================================
//
// Function Name: DRA_Insert_Pragmas
//
// Description: Insert DISTRIBUTE_RESHAPE pragmas into the passed tree
//              based on the argument signature given by arg_sig.
//
// =====================================================================

static void 
DRA_Insert_Pragmas(WN *pu_wn,
                   char *arg_sig)
{
  // strtol (char *str, char *ptr, INT base) returns as a long integer 
  // the value represented by the character string pointed to by str. 
  // The string is scanned up to  the first character inconsistent with 
  // the base. If the value of ptr is not (char **)NULL, a pointer to 
  // the character terminating the scan is returned in the location 
  // pointed to by ptr. If no integer can be formed, that location is 
  // set to str, and zero is returned.

  WN *block = WN_func_body(pu_wn);
  // WN *current = Get_Preamble_End(pu_wn); 
  WN *current = Find_Insertion_Point(pu_wn, arg_sig); 

  for (INT16 arg_pos = 0; *arg_sig; arg_sig++, arg_pos++ ) {

    // Extract the number of dimensions
    //
    INT16 num_dims = (INT16) strtol (arg_sig, &arg_sig, 10);
    ST *arg_st = WN_st(WN_kid(pu_wn, arg_pos));
    TY_IDX arg_ty = Get_Array_Type(arg_st);

    for (INT16 dim = 0; dim < num_dims; dim++) {

      // For each dimension create a pragma node
      //
      WN *pwn = WN_CreatePragma(WN_PRAGMA_DISTRIBUTE_RESHAPE, arg_st, 0, 0);
      WN_pragma_index(pwn) = dim;

      WN_set_pragma_compiler_generated(pwn);
      WN_INSERT_BlockAfter(block, current, pwn); // Need to fix this
      current = pwn;

      switch (*arg_sig++) {

        case DRA_BLOCK_CODE:
          WN_pragma_distr_type(pwn) = DISTRIBUTE_BLOCK;
          break;

        case DRA_STAR_CODE:
          WN_pragma_distr_type(pwn) = DISTRIBUTE_STAR;
          break;

        case DRA_CYCLIC_CODE: 
          {
            INT64 chunk = (INT64) strtol (arg_sig, &arg_sig, 10);
            if (chunk != 0) {
              WN_pragma_distr_type(pwn) = DISTRIBUTE_CYCLIC_CONST;
              WN_pragma_arg2(pwn) = chunk;
            }
            else {
              // For CYCLIC_EXPR create an additional XPRAGMA node
              //
              WN_pragma_distr_type(pwn) = DISTRIBUTE_CYCLIC_EXPR;
              WN *xpwn = WN_CreateXpragma(WN_PRAGMA_DISTRIBUTE_RESHAPE,
                                          arg_st, 1);
              WN_kid(xpwn, 0) = WN_Intconst(MTYPE_I8, 0);

              WN_set_pragma_compiler_generated(xpwn);
              WN_INSERT_BlockAfter(block, current, xpwn);
              current = xpwn;
            }
          }
          break;

        default:
          FmtAssert(FALSE, 
                    ("Unrecognized distribution in the mangled name"));
      }

      // Finally, create an XPRAGMA node for array size
      //
      WN *xpwn = WN_CreateXpragma(WN_PRAGMA_DISTRIBUTE_RESHAPE, arg_st, 1);
      
      INT16 st_dim = dim;

      WN *lb;
      if (TY_AR_const_lbnd(arg_ty, st_dim)) {
        lb = WN_Intconst(MTYPE_I8, TY_AR_lbnd_val(arg_ty, st_dim));
      }
      else {
        ST_IDX lb_st = TY_AR_lbnd_var(arg_ty, st_dim);
        TY_IDX lb_ty = ST_type(lb_st);
        lb = WN_CreateLdid(OPCODE_make_op(OPR_LDID, 
                                          TY_mtype(lb_ty),
                                          TY_mtype(lb_ty)),
                           0, 
                           lb_st, 
                           lb_ty);
      }

      WN *ub;
      if (TY_AR_const_ubnd(arg_ty, st_dim)) {
        ub = WN_Intconst(MTYPE_I8, TY_AR_ubnd_val(arg_ty, st_dim));
      }
      else {
        ST_IDX ub_st = TY_AR_ubnd_var(arg_ty, st_dim);
        TY_IDX ub_ty = ST_type(ub_st);
        ub = WN_CreateLdid(OPCODE_make_op(OPR_LDID,
                                          TY_mtype(ub_ty),
                                          TY_mtype(ub_ty)),
                           0, 
                           ub_st, 
                           ub_ty);
      }

      WN_kid(xpwn, 0) = WN_Add(MTYPE_I8, 
                               WN_Sub(MTYPE_I8, ub, lb), 
                               WN_Intconst(MTYPE_I8, 1));
      
      WN_set_pragma_compiler_generated(xpwn);
      WN_INSERT_BlockAfter(block, current, xpwn);
      current = xpwn;
    } 
  }
}



// =====================================================================
// 
// Function Name: DRA_Collect_Commons
// 
// Description: Given a WHIRL tree and a hash-table, (recursively) collect all
//              the base COMMON STs referenced in the tree into the hash-table.
//
// =====================================================================

static void 
DRA_Collect_Commons(WN *wn, DRA_COMMON_HASH_TABLE *dra_common_ht)
{
  if (wn == NULL) return;

  OPCODE opc = WN_opcode(wn);

  ST *st = OPCODE_has_sym(opc) ? WN_st(wn) : NULL;

  if (st &&
      (ST_base(st) != st) &&
      (ST_sclass(st) == SCLASS_COMMON || ST_sclass(st) == SCLASS_DGLOBAL) &&
      (ST_class(ST_base(st)) == CLASS_VAR &&
       TY_kind(ST_type(ST_base(st))) == KIND_STRUCT)) {
    // smells like a common
    dra_common_ht->Enter_If_Unique (ST_st_idx(ST_base(st)), TRUE);
  }

  // recurse
  //
  if (opc == OPC_BLOCK) {
    WN *kid = WN_first(wn);
    while (kid) {
      DRA_Collect_Commons (kid, dra_common_ht);
      kid = WN_next(kid);
    }
  }
  else {
    for (INT i=0; i<WN_kid_count(wn); i++) {
      DRA_Collect_Commons (WN_kid(wn,i), dra_common_ht);
    }
  }
}


// =====================================================================
// 
// Function Name: DRA_Process_Commons
// 
// Description: Write the information related to distribute-reshaped
//              arrays appearing in common blocks into .rii file that
//              will be consumed by the prelinker in order to do 
//              consistency checks.
//
// =====================================================================

static void 
DRA_Process_Commons(DRA_HASH_TABLE *dra_table, 
                    DRA_COMMON_HASH_TABLE *dra_common_ht)
{       
  BOOL seen_common = FALSE;
  BOOL new_common = FALSE;
  UINT bufsize = 1024;
  char *buf = (char *) alloca(bufsize);
  char *bufptr = buf;
  char *common_name = NULL;
  INT64 common_offset;
  INT64 non_dra_start = 0;
  INT64 non_dra_end = 0;
  ST *st;
  INT i;

  /* COMMON blocks are now in global symtab */
  FOREACH_SYMBOL (GLOBAL_SYMTAB, st, i) {

    // Common blocks and their fields are listed consecutively in ST
    //
    ST_SCLASS st_sclass = ST_sclass(st);

    if (st_sclass == SCLASS_COMMON &&
        ST_st_idx(st) == ST_base_idx(st) &&
        dra_common_ht->Find(ST_st_idx(st))) {

      // COMMON and not based, so must be the base of the COMMON block

      char *st_name = ST_name(st);

      // Names of split commons: BaseName.BaseOffset (Try to find '.')
      //
      char *dot = strchr(st_name, '.');

      // Full common name
      //
      if (dot == NULL) {
        if (common_name == NULL || 
            strcmp(st_name, common_name) != 0) {
          common_name = strcpy((char *) alloca(strlen(st_name)+1), 
                               st_name);
          new_common = TRUE;
        }
        common_offset = 0;
      }

      // Split common name
      //
      else {
        if (common_name == NULL || 
            strncmp(st_name, common_name, dot-st_name) != 0) {
          common_name = strncpy((char *) alloca(dot-st_name+1), 
                                st_name, dot-st_name);
          common_name[dot-st_name] = '\0';
          new_common = TRUE;
        }
        common_offset = strtol(dot+1, NULL, 10);
      }

      if (new_common) {

        // Write the last chunk of the previous common (if it existed)
        //
        if (non_dra_end - non_dra_start > 0) {
          bufptr += sprintf(bufptr, " %lld\n", non_dra_end - non_dra_start);
        }
        else if (seen_common) {
          *bufptr++ = '\n';
        }
        
        // Write the name of the new common
        //
        INT name_len = strlen(common_name);

        if (bufptr - buf + name_len + 21 >= bufsize) {
          bufsize *= 2;
          char *newbuf = (char *) alloca(bufsize);
          buf = strcpy(newbuf, buf);
          bufptr = buf + strlen(buf);
        }

        (void) strcpy(bufptr, common_name);
        bufptr += name_len;

        non_dra_end = non_dra_start = 0;
        new_common = FALSE;
      }

      seen_common = TRUE;
    }

    else if (ST_st_idx(st) != ST_base_idx(st) &&
             ST_sclass(ST_base(st)) == SCLASS_COMMON &&
             dra_common_ht->Find(ST_st_idx(ST_base(st)))) {

      TY_IDX ty = ST_type(st);

      DRA_INFO *dra = (dra_table ? dra_table->Find(st) : NULL);

      if (dra != NULL) {              // reshaped array

        INT16 ndims = TY_AR_ndims(ty);

        // Reallocate if necessary (double the buffer size)
        // We need space to write this reshaped array and
        // possibly a non-reshaped chunk size that follows it
        // 31 chars prefix: DRA_ndims(5)_esize(21)
        // 69 chars per dimension: _lb(22):ub(22):distr(1)chunk(21)
        // 21 chars for the next non-reshaped chunk
        //
        if (bufptr - buf + 31 + ndims*69 + 21 >= bufsize) {
          bufsize *= 2;
          char *newbuf = (char *) alloca(bufsize);
          buf = strcpy(newbuf, buf);
          bufptr = buf + strlen(buf);
        }

        if (non_dra_end - non_dra_start > 0) {
          bufptr += sprintf(bufptr, " %lld", non_dra_end - non_dra_start);
        }
        non_dra_start = common_offset + ST_ofst(st) + TY_size(ty);
        non_dra_end = non_dra_start;

        bufptr += 
          sprintf(bufptr, " DRA_%lld_%d", TY_size(TY_AR_etype(ty)), ndims);

        for (INT16 dim = 0; dim < ndims; ++dim) {
          
          bufptr += sprintf(bufptr, 
                            "_%lld:%lld:", 
                            TY_AR_lbnd_val(ty, ndims-1-dim), 
                            TY_AR_ubnd_val(ty, ndims-1-dim));

          switch (dra->Distr_Type(dim)) {
            case DISTRIBUTE_STAR: 
              *bufptr++ = DRA_STAR_CODE; 
              break;	
            case DISTRIBUTE_BLOCK: 
              *bufptr++ = DRA_BLOCK_CODE; 
              break;
            case DISTRIBUTE_CYCLIC_CONST:
              *bufptr++ = DRA_CYCLIC_CODE; 
              bufptr += sprintf(bufptr, "%lld", dra->Chunk_Const_Val(dim));
              break;
            case DISTRIBUTE_CYCLIC_EXPR:
              *bufptr++ = DRA_CYCLIC_CODE; 
              break;
          }	
        }
      }

      else if (common_offset + ST_ofst(st) + TY_size(ty) > non_dra_end) {
        non_dra_end = common_offset + ST_ofst(st) + TY_size(ty);
      }
    }
  }

  if (bufptr != buf) {
    if (non_dra_end - non_dra_start > 0) {
      bufptr += sprintf(bufptr, " %lld\n", non_dra_end - non_dra_start);
    }
    else {
      *bufptr++ = '\n';
    }
    write(DRA_file_desc, (void*)buf, bufptr-buf);
  }
}



// =====================================================================
// 
// Function Name: DRA_Process_Globals
// 
// Description: Write the information related to distribute-reshaped
//              global arrays into .rii file that
//              will be consumed by the prelinker in order to do 
//              consistency checks.
//
// =====================================================================

static void 
DRA_Process_Globals(DRA_HASH_TABLE *dra_table)
{       
  UINT bufsize = 1024;
  char *buf = (char *) alloca(bufsize);
  char *bufptr = buf;
  ST *st;
  INT i;

  {
    // process globals just once per file, not once per PU
    static BOOL done_globals = FALSE;
    if (done_globals) return;
    done_globals = TRUE;
  }

  FOREACH_SYMBOL (GLOBAL_SYMTAB, st, i) {

    if (ST_class(st) != CLASS_VAR) continue;

    // skip commons.
    //
    if ((ST_sclass(st) == SCLASS_COMMON) ||         // common
        (ST_sclass(st) == SCLASS_DGLOBAL &&         // might be common
                                                    // if this st or
                                                    // the base is kind struct
         (TY_kind(ST_type(st)) == KIND_STRUCT ||
          (ST_class(ST_base(st)) == CLASS_VAR &&
           TY_kind(ST_type(ST_base(st))) == KIND_STRUCT)))) {
      continue;
    }

    bufptr = buf;

    // is it a global array? if so, write it out
    //
    TY_IDX ty = Get_Original_Type(st);
    if (ty && TY_kind(ty) == KIND_ARRAY) {

      char* st_name = ST_name(st);
      INT name_len = strlen(st_name);

      if (bufptr - buf + name_len + 21 >= bufsize) {
        bufsize *= 2;
        char *newbuf = (char *) alloca(bufsize);
        buf = strcpy(newbuf, buf);
        bufptr = buf + strlen(buf);
      }

      strcpy (bufptr, st_name);
      bufptr += name_len;

      DRA_INFO* dra = (dra_table ? dra_table->Find(st) : NULL);

      if (dra != NULL) {
        // reshaped
        //

        INT16 ndims = TY_AR_ndims(ty);

        // Reallocate if necessary (double the buffer size)
        // We need space to write this reshaped array 
        // 31 chars prefix: DRA_ndims(5)_esize(21)
        // 69 chars per dimension: _lb(22):ub(22):distr(1)chunk(21)
        //
        if (bufptr - buf + 31 + ndims*69 >= bufsize) {
          bufsize *= 2;
          char *newbuf = (char *) alloca(bufsize);
          buf = strcpy(newbuf, buf);
          bufptr = buf + strlen(buf);
        }

        bufptr += 
          sprintf(bufptr, " DRA_%lld_%d", TY_size(TY_AR_etype(ty)), ndims);

        // emit dimensions consistently: i.e. stride-one dimension first
        // 
        for (INT16 dim = 0; dim < ndims; ++dim) {
          
          bufptr += sprintf(bufptr, 
                            "_%lld:%lld:", 
                            TY_AR_lbnd_val(ty, ndims-1-dim), 
                            TY_AR_ubnd_val(ty, ndims-1-dim));

          switch (dra->Distr_Type(dim)) {
            case DISTRIBUTE_STAR: 
              *bufptr++ = DRA_STAR_CODE; 
              break;	
            case DISTRIBUTE_BLOCK: 
              *bufptr++ = DRA_BLOCK_CODE; 
              break;
            case DISTRIBUTE_CYCLIC_CONST:
              *bufptr++ = DRA_CYCLIC_CODE; 
              bufptr += sprintf(bufptr, "%lld", dra->Chunk_Const_Val(dim));
              break;
            case DISTRIBUTE_CYCLIC_EXPR:
              *bufptr++ = DRA_CYCLIC_CODE; 
              break;
          }	
        }
        *bufptr++ = '\n';
      }
      else {
        // not reshaped
        //
        bufptr += sprintf (bufptr, " %lld", TY_size(ty));
        *bufptr++ = '\n';
      }
      write(DRA_file_desc, (void*)buf, bufptr-buf);
    }
  }
}



// =====================================================================
//
// Function Name: DRA_Info_Matches_Encoding
//
// Description: Check if the DISTRIBUTE_RESHAPE information from
//              DRA_INFO matches that of the encoded argument.
//
// =====================================================================

static BOOL
DRA_Info_Matches_Encoding(DRA_INFO *dra, 
                          char *arg_sig)
{
  INT16 num_dims = dra->Num_Dims();
  for (INT16 dim = 0; dim < num_dims; dim++) {

    switch (*arg_sig++) {

      case DRA_BLOCK_CODE:
        if (dra->Distr_Type(dim) != DISTRIBUTE_BLOCK) {
          return FALSE;
        }
        break;

      case DRA_STAR_CODE:
        if (dra->Distr_Type(dim) != DISTRIBUTE_STAR) {
          return FALSE;
        }
        break;

      case DRA_CYCLIC_CODE: 
        {
          INT64 chunk = (INT64) strtol (arg_sig, &arg_sig, 10);
          if (chunk != 0) {
            if (dra->Distr_Type(dim) != DISTRIBUTE_CYCLIC_CONST ||
                dra->Chunk_Const_Val(dim) != chunk) {
              return FALSE;
            }
          }
          else {
            if (dra->Distr_Type(dim) != DISTRIBUTE_CYCLIC_EXPR) {
              return FALSE;
            }
          }
        }
        break;

      default:
        FmtAssert(FALSE, 
                  ("Uncrecognized distribution in the mangled name"));
    }
  }
  return TRUE;
}
