/*
 * Copyright 2003, 2004, 2005, 2006 PathScale, Inc.  All Rights Reserved.
 *
 * This program is free software; you can redistribute it and/or modify it
 * under the terms of version 2 of the GNU General Public License as
 * published by the Free Software Foundation.
 *
 * This program is distributed in the hope that it would be useful, but
 * WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  
 *
 * Further, this software is distributed without any warranty that it is
 * free of the rightful claim of any third person regarding infringement 
 * or the like.  Any license provided herein, whether implied or 
 * otherwise, applies only to this software file.  Patent licenses, if 
 * any, provided herein do not apply to combinations of this program with 
 * other software, or any other product whatsoever.  
 *
 * You should have received a copy of the GNU General Public License along
 * with this program; if not, write the Free Software Foundation, Inc., 59
 * Temple Place - Suite 330, Boston MA 02111-1307, USA.
 */


/* ====================================================================
 * ====================================================================
 *
 * Module: ipa_builtins.cxx
 *
 * Description:
 *	Add built-in functions at the IPA level.
 *
 * ====================================================================
 * ====================================================================
 */

#include <stdint.h>
#include <alloca.h>
#include "wn.h"	                        // WN
#include "cxx_memory.h"		        // CXX_NEW
#include "pu_info.h"                    // PU_Info
#include "ipc_symtab_merge.h"           // AUX_PU
#include "ipa_option.h"
#include "ipa_cg.h"		        // IPA_NODE
#include "ipo_defs.h"                   // IPA_NODE_CONTEXT
#include "region_util.h"		// for WN_Fake_Call_EH_Region
#include "wn_util.h"
#include "ipa_builtins.h"

// List of all the IPA builtins.
std::vector<IPA_BUILTIN*> IPA_builtins_list;

// Char map arrays.
ST *IPA_builtin_ctype_b = NULL;
ST *IPA_builtin_ctype_tolower = NULL;
ST *IPA_builtin_ctype_toupper = NULL;

static MEM_POOL IPA_Builtins_Mem_Pool;


// Do the initial steps in creating a function.
static PU_Info*
IPA_Start_Function (TY_IDX func_ty_idx, char *func_name, int num_args)
{
  // Based on common/com:Gen_Intrinsic_Function.  Similar code also in
  // kg++fe/tree_symtab.cxx:Create_ST_For_Tree.
  ST *func_st = New_ST(GLOBAL_SYMTAB);

  PU_IDX pu_idx;
  PU& pu = New_PU (pu_idx);
  PU_Init (pu, func_ty_idx, GLOBAL_SYMTAB+1);	// 2nd arg is prototype
  ST_Init (func_st, Save_Str(func_name), CLASS_FUNC, SCLASS_TEXT,
	   EXPORT_PREEMPTIBLE, TY_IDX(pu_idx));
  Set_ST_pu (func_st, pu_idx);
  Set_PU_c_lang(pu);
  Clear_PU_uplevel(pu);

  WN *entry_wn, *body, *wn;
  body = WN_CreateBlock();
  entry_wn = WN_CreateEntry ( num_args, func_st, body, NULL, NULL );

  PU_Info *pu_info;
  /* allocate a new PU_Info and add it to the list */
  pu_info = TYPE_MEM_POOL_ALLOC(PU_Info, Malloc_Mem_Pool);
  PU_Info_init(pu_info);

  Set_PU_Info_tree_ptr (pu_info, entry_wn);
  // Is MEM_pu_nz_pool correct?
  PU_Info_maptab (pu_info) = WN_MAP_TAB_Create(MEM_pu_nz_pool_ptr);
  PU_Info_proc_sym (pu_info) = ST_st_idx(func_st);
  // This is how kg++fe/wfe_decl.cxx:WFE_Start_Function sets the PU_Info_pu_dst
  // and PU_Info_cu_dst.  For now, let's see if we can get away using
  // DST_INVALID_IDX.
  // PU_Info_pu_dst (pu_info) = DST_Create_Subprogram (func_st,fndecl);
  // PU_Info_cu_dst (pu_info) = DST_Get_Comp_Unit ();
  PU_Info_pu_dst (pu_info) = DST_INVALID_IDX;
  PU_Info_cu_dst (pu_info) = DST_INVALID_IDX;

  Set_PU_Info_state(pu_info, WT_SYMTAB, Subsect_InMem);
  Set_PU_Info_state(pu_info, WT_TREE, Subsect_InMem);
  Set_PU_Info_state(pu_info, WT_PROC_SYM, Subsect_InMem);

  Set_PU_Info_flags(pu_info, PU_IS_COMPILER_GENERATED);

  return pu_info;
}


// Create a IPA Node for a IPA-created builtin.
static void
IPA_Add_Builtin_IPA_NODE (PU_Info *pu_info, INTRINSIC intrinsic)
{
  ST *func_st = &St_Table[PU_Info_proc_sym (pu_info)];

  // Create IPA Node.  The IPA Node for a builtin doesn't have all the info
  // that a regular IPA Node has.
  IPA_NODE *ipa_node = IPA_Call_Graph->Add_New_Node (func_st, -1, -1, -1);
  NODE_INDEX cg_node = ipa_node->Node_Index ();
  ipa_node->Set_Scope(Scope_tab);
  ipa_node->Set_Builtin();
  ipa_node->Set_Builtin_PU_Info(pu_info);

  // Add the pu to Aux_Pu_Table.
  UINT32 aux_idx;
  AUX_PU& aux_pu = Aux_Pu_Table.New_entry (aux_idx);
  Set_AUX_PU_node (aux_pu, cg_node);
  // Builtins don't have file headers either.  Leave the file hdr alone for now.
  // Set_AUX_PU_file_hdr (aux_pu, &file_hdr);

  // Add the builtin to the builtins list.
  IPA_BUILTIN *ipa_builtin = CXX_NEW(IPA_BUILTIN(pu_info, intrinsic),
				     &IPA_Builtins_Mem_Pool);
  IPA_builtins_list.push_back(ipa_builtin);
}


// Create the prototype for a function with 3 args.  Return the prototype
// type index.
static TY_IDX
IPA_Create_Func_Type_3 (TY_IDX ret_ty_idx, TY_IDX arg0_ty_idx,
			TY_IDX arg1_ty_idx, TY_IDX arg2_ty_idx)
{
  // Based on kg++fe/tree_symtab.cxx:Create_TY_For_Tree.

  TY_IDX func_ty_idx;	// type of the function
  INT32 num_args;
  TY &func_ty = New_TY (func_ty_idx);
  TY_Init (func_ty, 0, KIND_FUNCTION, MTYPE_UNKNOWN, STR_IDX_ZERO);
  Set_TY_align (func_ty_idx, 1);
  TYLIST tylist_idx;

  // allocate TYs for return as well as parameters
  // this is needed to avoid mixing TYLISTs if one
  // of the parameters is a pointer to a function

  Set_TYLIST_type (New_TYLIST (tylist_idx), ret_ty_idx);
  Set_TY_tylist (func_ty, tylist_idx);

  Set_TYLIST_type (New_TYLIST (tylist_idx), arg0_ty_idx);
  Set_TYLIST_type (New_TYLIST (tylist_idx), arg1_ty_idx);
  Set_TYLIST_type (New_TYLIST (tylist_idx), arg2_ty_idx);

  Set_TY_has_prototype(func_ty_idx);
  Set_TYLIST_type (Tylist_Table [tylist_idx], 0);

  return func_ty_idx;
}


/*
#include "wn.h"
#include "wn_util.h"
#include "symtab.h"
*/

unsigned short int path__ctype_b[] = {
 2, 2, 2, 2, 2, 2, 2, 2, 2, 8195, 8194, 8194, 8194, 8194, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 24577, 49156, 49156, 49156, 49156, 49156, 49156, 49156, 49156, 49156, 49156, 49156, 49156, 49156, 49156, 49156, 55304, 55304, 55304, 55304, 55304, 55304, 55304, 55304, 55304, 55304, 49156, 49156, 49156, 49156, 49156, 49156, 49156, 54536, 54536, 54536, 54536, 54536, 54536, 50440, 50440, 50440, 50440, 50440, 50440, 50440, 50440, 50440, 50440, 50440, 50440, 50440, 50440, 50440, 50440, 50440, 50440, 50440, 50440, 49156, 49156, 49156, 49156, 49156, 49156, 54792, 54792, 54792, 54792, 54792, 54792, 50696, 50696, 50696, 50696, 50696, 50696, 50696, 50696, 50696, 50696, 50696, 50696, 50696, 50696, 50696, 50696, 50696, 50696, 50696, 50696, 49156, 49156, 49156, 49156, 2, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 2, 0, 2, 0, 2, 0, 2, 0, 2, 0, 2, 0, 2, 0, 2, 0, 2, 0, 8195, 0, 8194, 0, 8194, 0, 8194, 0, 8194, 0, 2, 0, 2, 0, 2, 0, 2, 0, 2, 0, 2, 0, 2, 0, 2, 0, 2, 0, 2, 0, 2, 0, 2, 0, 2, 0, 2, 0, 2, 0, 2, 0, 2, 0, 2, 0, 24577, 0, 49156, 0, 49156, 0, 49156, 0, 49156, 0, 49156, 0, 49156, 0, 49156, 0, 49156, 0, 49156, 0, 49156, 0, 49156, 0, 49156, 0, 49156, 0, 49156, 0, 49156, 0, 55304, 0, 55304, 0, 55304, 0, 55304, 0, 55304, 0, 55304, 0, 55304, 0, 55304, 0, 55304, 0, 55304, 0, 49156, 0, 49156, 0, 49156, 0, 49156, 0, 49156, 0, 49156};

int path__ctype_tolower[] = {
 0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30, 31, 32, 33, 34, 35, 36, 37, 38, 39, 40, 41, 42, 43, 44, 45, 46, 47, 48, 49, 50, 51, 52, 53, 54, 55, 56, 57, 58, 59, 60, 61, 62, 63, 64, 97, 98, 99, 100, 101, 102, 103, 104, 105, 106, 107, 108, 109, 110, 111, 112, 113, 114, 115, 116, 117, 118, 119, 120, 121, 122, 91, 92, 93, 94, 95, 96, 97, 98, 99, 100, 101, 102, 103, 104, 105, 106, 107, 108, 109, 110, 111, 112, 113, 114, 115, 116, 117, 118, 119, 120, 121, 122, 123, 124, 125, 126, 127, 128, 129, 130, 131, 132, 133, 134, 135, 136, 137, 138, 139, 140, 141, 142, 143, 144, 145, 146, 147, 148, 149, 150, 151, 152, 153, 154, 155, 156, 157, 158, 159, 160, 161, 162, 163, 164, 165, 166, 167, 168, 169, 170, 171, 172, 173, 174, 175, 176, 177, 178, 179, 180, 181, 182, 183, 184, 185, 186, 187, 188, 189, 190, 191, 192, 193, 194, 195, 196, 197, 198, 199, 200, 201, 202, 203, 204, 205, 206, 207, 208, 209, 210, 211, 212, 213, 214, 215, 216, 217, 218, 219, 220, 221, 222, 223, 224, 225, 226, 227, 228, 229, 230, 231, 232, 233, 234, 235, 236, 237, 238, 239, 240, 241, 242, 243, 244, 245, 246, 247, 248, 249, 250, 251, 252, 253, 254, 255, 0, 0, 134217726, 0, 0, 0, 0, 0, 7, 1, 6, 1, 1, 24, 0, 32, 134217726, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 134217726, 0, 0, 0, 0, 7, 1, 6, 1, 1, 24, 0, 32, 0, 134217726, 0, 0, 0, 0, 0, 0, 0, 0, 134217726, 134217726, 0, 0, 0, 0, 7, 1, 6, 1, 1, 24, 0, 32, 134217726, 134217726, 0, 0, 0, 0, 0, 0, 0, 67043328, 0, 0, 0, 0, 0, 0, 6, 1, 6, 0, 1, 24, 28, 0, 67043328, 0, 0, 0, 0, 0, 0, 0, 0, 67043328, 126, 126, 0, 0, 0, 0, 7, 1, 7, 0, 3, 24, 28, 0, 67043328, 126, 126, 0, 0, 0, 0, 0, 15872, 1, 0, 0, 0, 0, 0, 0};

int path__ctype_toupper[] = {
 0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30, 31, 32, 33, 34, 35, 36, 37, 38, 39, 40, 41, 42, 43, 44, 45, 46, 47, 48, 49, 50, 51, 52, 53, 54, 55, 56, 57, 58, 59, 60, 61, 62, 63, 64, 65, 66, 67, 68, 69, 70, 71, 72, 73, 74, 75, 76, 77, 78, 79, 80, 81, 82, 83, 84, 85, 86, 87, 88, 89, 90, 91, 92, 93, 94, 95, 96, 65, 66, 67, 68, 69, 70, 71, 72, 73, 74, 75, 76, 77, 78, 79, 80, 81, 82, 83, 84, 85, 86, 87, 88, 89, 90, 123, 124, 125, 126, 127, 128, 129, 130, 131, 132, 133, 134, 135, 136, 137, 138, 139, 140, 141, 142, 143, 144, 145, 146, 147, 148, 149, 150, 151, 152, 153, 154, 155, 156, 157, 158, 159, 160, 161, 162, 163, 164, 165, 166, 167, 168, 169, 170, 171, 172, 173, 174, 175, 176, 177, 178, 179, 180, 181, 182, 183, 184, 185, 186, 187, 188, 189, 190, 191, 192, 193, 194, 195, 196, 197, 198, 199, 200, 201, 202, 203, 204, 205, 206, 207, 208, 209, 210, 211, 212, 213, 214, 215, 216, 217, 218, 219, 220, 221, 222, 223, 224, 225, 226, 227, 228, 229, 230, 231, 232, 233, 234, 235, 236, 237, 238, 239, 240, 241, 242, 243, 244, 245, 246, 247, 248, 249, 250, 251, 252, 253, 254, 255, 128, 129, 130, 131, 132, 133, 134, 135, 136, 137, 138, 139, 140, 141, 142, 143, 144, 145, 146, 147, 148, 149, 150, 151, 152, 153, 154, 155, 156, 157, 158, 159, 160, 161, 162, 163, 164, 165, 166, 167, 168, 169, 170, 171, 172, 173, 174, 175, 176, 177, 178, 179, 180, 181, 182, 183, 184, 185, 186, 187, 188, 189, 190, 191, 192, 193, 194, 195, 196, 197, 198, 199, 200, 201, 202, 203, 204, 205, 206, 207, 208, 209, 210, 211, 212, 213, 214, 215, 216, 217, 218, 219, 220, 221, 222, 223, 224, 225, 226, 227, 228, 229, 230, 231, 232, 233, 234, 235, 236, 237, 238, 239, 240, 241, 242, 243, 244, 245, 246, 247, 248, 249, 250, 251, 252, 253, 254, -1};


static ST *
Insert_ctype_b(void) {
  // create the array
  INT n = sizeof(path__ctype_b)/sizeof(*path__ctype_b);
  TY_IDX ty_idx;
  TY &ty = New_TY(ty_idx);
  TY_Init(ty, n*sizeof(*path__ctype_b), KIND_ARRAY, MTYPE_M, 
  	  Save_Str("path__ctype_barray"));
  Set_TY_etype(ty, MTYPE_To_TY(MTYPE_U2));
  Set_TY_align (ty_idx, TY_align(TY_etype(ty)));
  ARB_HANDLE arb = New_ARB ();
  ARB_Init (arb, 0, 0, 0);
  Set_TY_arb (ty, arb);
  Set_ARB_first_dimen (arb);
  Set_ARB_last_dimen (arb);
  Set_ARB_dimension (arb, 1);
  Set_ARB_const_stride(arb);
  Set_ARB_stride_val(arb, 1);
  Set_ARB_const_lbnd (arb);
  Set_ARB_lbnd_val (arb, 0);
  Set_ARB_const_ubnd (arb);
  Set_ARB_ubnd_val (arb, n);
  ST *st = New_ST(1);
  ST_Init(st, Save_Str("path__ctype_b"), CLASS_VAR, SCLASS_FSTATIC, EXPORT_LOCAL, ty_idx);
  Set_ST_is_initialized(st);
  INITO_IDX inito = New_INITO(st);
  INITV_IDX inv = New_INITV();
  INITV_Init_Integer(inv, MTYPE_U2, path__ctype_b[0]);
  Set_INITO_val(inito, inv);
  INITV_IDX last_inv = inv;
  for (INT i = 1; i < n; i++) {
    inv = New_INITV();
    INITV_Init_Integer(inv, MTYPE_U2, path__ctype_b[i]);
    Set_INITV_next(last_inv, inv);
    last_inv = inv;
  }

  // create the pointer initialized to point to the array
  TY_IDX ty_idx2 = Make_Pointer_Type(ty_idx);
  ST *st2 = New_ST(1);
  ST_Init(st2, Save_Str("path__ctype_b_ptr"), CLASS_VAR, SCLASS_FSTATIC, EXPORT_LOCAL, ty_idx2);
  Set_ST_is_initialized(st2);
  inito = New_INITO(st2);
  inv = New_INITV();
  INITV_Init_Symoff(inv, st, 0);
  Set_INITO_val(inito, inv);
  return st2;
}

static ST *
Insert_ctype_tolower(void) {
  // create the array
  INT n = sizeof(path__ctype_tolower)/sizeof(*path__ctype_tolower);
  TY_IDX ty_idx;
  TY &ty = New_TY(ty_idx);
  TY_Init(ty, n*sizeof(*path__ctype_tolower), KIND_ARRAY, MTYPE_M, 
	  Save_Str("path__ctype_tolowerarray"));
  Set_TY_etype(ty, MTYPE_To_TY(MTYPE_I4));
  Set_TY_align (ty_idx, TY_align(TY_etype(ty)));
  ARB_HANDLE arb = New_ARB ();
  ARB_Init (arb, 0, 0, 0);
  Set_TY_arb (ty, arb);
  Set_ARB_first_dimen (arb);
  Set_ARB_last_dimen (arb);
  Set_ARB_dimension (arb, 1);
  Set_ARB_const_stride(arb);
  Set_ARB_stride_val(arb, 1);
  Set_ARB_const_lbnd (arb);
  Set_ARB_lbnd_val (arb, 0);
  Set_ARB_const_ubnd (arb);
  Set_ARB_ubnd_val (arb, n);
  ST *st = New_ST(1);
  ST_Init(st, Save_Str("path__ctype_tolower"), CLASS_VAR, SCLASS_FSTATIC, EXPORT_LOCAL, ty_idx);
  Set_ST_is_initialized(st);
  INITO_IDX inito = New_INITO(st);
  INITV_IDX inv = New_INITV();
  INITV_Init_Integer(inv, MTYPE_I4, path__ctype_tolower[0]);
  Set_INITO_val(inito, inv);
  INITV_IDX last_inv = inv;
  for (INT i = 1; i < n; i++) {
    inv = New_INITV();
    INITV_Init_Integer(inv, MTYPE_I4, path__ctype_tolower[i]);
    Set_INITV_next(last_inv, inv);
    last_inv = inv;
  }

  // create the pointer initialized to point to the array
  TY_IDX ty_idx2 = Make_Pointer_Type(ty_idx);
  ST *st2 = New_ST(1);
  ST_Init(st2, Save_Str("path__ctype_tolower_ptr"), CLASS_VAR, SCLASS_FSTATIC, EXPORT_LOCAL, ty_idx2);
  Set_ST_is_initialized(st2);
  inito = New_INITO(st2);
  inv = New_INITV();
  INITV_Init_Symoff(inv, st, 0);
  Set_INITO_val(inito, inv);
  return st2;
}

static ST *
Insert_ctype_toupper(void) {
  // create the array
  INT n = sizeof(path__ctype_toupper)/sizeof(*path__ctype_toupper);
  TY_IDX ty_idx;
  TY &ty = New_TY(ty_idx);
  TY_Init(ty, n*sizeof(*path__ctype_toupper), KIND_ARRAY, MTYPE_M, 
	  Save_Str("path__ctype_toupperarray"));
  Set_TY_etype(ty, MTYPE_To_TY(MTYPE_I4));
  Set_TY_align (ty_idx, TY_align(TY_etype(ty)));
  ARB_HANDLE arb = New_ARB ();
  ARB_Init (arb, 0, 0, 0);
  Set_TY_arb (ty, arb);
  Set_ARB_first_dimen (arb);
  Set_ARB_last_dimen (arb);
  Set_ARB_dimension (arb, 1);
  Set_ARB_const_stride(arb);
  Set_ARB_stride_val(arb, 1);
  Set_ARB_const_lbnd (arb);
  Set_ARB_lbnd_val (arb, 0);
  Set_ARB_const_ubnd (arb);
  Set_ARB_ubnd_val (arb, n);
  ST *st = New_ST(1);
  ST_Init(st, Save_Str("path__ctype_toupper"), CLASS_VAR, SCLASS_FSTATIC, EXPORT_LOCAL, ty_idx);
  Set_ST_is_initialized(st);
  INITO_IDX inito = New_INITO(st);
  INITV_IDX inv = New_INITV();
  INITV_Init_Integer(inv, MTYPE_I4, path__ctype_toupper[0]);
  Set_INITO_val(inito, inv);
  INITV_IDX last_inv = inv;
  for (INT i = 1; i < n; i++) {
    inv = New_INITV();
    INITV_Init_Integer(inv, MTYPE_I4, path__ctype_toupper[i]);
    Set_INITV_next(last_inv, inv);
    last_inv = inv;
  }

  // create the pointer initialized to point to the array
  TY_IDX ty_idx2 = Make_Pointer_Type(ty_idx);
  ST *st2 = New_ST(1);
  ST_Init(st2, Save_Str("path__ctype_toupper_ptr"), CLASS_VAR, SCLASS_FSTATIC, EXPORT_LOCAL, ty_idx2);
  Set_ST_is_initialized(st2);
  inito = New_INITO(st2);
  inv = New_INITV();
  INITV_Init_Symoff(inv, st, 0);
  Set_INITO_val(inito, inv);
  return st2;
}

// Create our own arrays for use by ctype.h.
static void
IPA_Create_Ctype_Arrays()
{
  IPA_builtin_ctype_b = Insert_ctype_b();
  IPA_builtin_ctype_tolower = Insert_ctype_tolower();
  IPA_builtin_ctype_toupper = Insert_ctype_toupper();
}


void
IPA_Create_Builtins ()
{
  // save current pointers to standard memory pools
  MEM_POOL* save_pu_pool_ptr = MEM_pu_pool_ptr;
  MEM_POOL* save_wn_pool_ptr = WN_mem_pool_ptr;

  // set standard memory pools
  MEM_POOL_Initialize(&IPA_Builtins_Mem_Pool, "IPA builtins pool", 0);
  MEM_pu_pool_ptr = &IPA_Builtins_Mem_Pool;
  WN_mem_pool_ptr = &IPA_Builtins_Mem_Pool;

  // create the builtins (currently none)

  // Create our own arrays for used in ctype.h.
  if(IPA_Enable_Ctype)
    IPA_Create_Ctype_Arrays();

  // Restore pointers to standard memory pools
  MEM_pu_pool_ptr = save_pu_pool_ptr;
  WN_mem_pool_ptr = save_wn_pool_ptr;
}


// Change all calls in NODE's PU to use the IPA builtins whenever possible.
void
IPA_Rename_Builtins (IPA_NODE *node)
{
  if (!node || !node->PU_Info())  // PU_Info() is null for Fortran ALTENTRY
    return;

  // Use the node's mempool for wn creation.
  IPA_NODE_CONTEXT context(node);

  for (WN_ITER* wni = WN_WALK_SCFIter(node->Whirl_Tree(FALSE)); 
       wni != NULL;
       wni = WN_WALK_SCFNext(wni)) {

    if (WN_operator(WN_ITER_wn(wni)) == OPR_BLOCK) {
      WN *wn;
      WN *block = WN_ITER_wn(wni);
      for (wn = WN_first(block); wn != NULL; wn = WN_next(wn)) {
	// See if WN is a call to a C library function that returns the address
	// of arrays used in ctype.h.  Change the call to a LDA of our own
	// array.
	if (IPA_Enable_Ctype &&
	    WN_operator(wn) == OPR_CALL) {
	  WN *new_wn = NULL;
	  char *name = &Str_Table[ST_name_idx(WN_st(wn))];
	  if (strcmp(name,"__ctype_b_loc") == 0) {
	    new_wn = WN_Lda(Pointer_Mtype, 0, IPA_builtin_ctype_b, 0);
	  } else if (strcmp(name,"__ctype_tolower_loc") == 0) {
	    new_wn = WN_Lda(Pointer_Mtype, 0, IPA_builtin_ctype_tolower, 0);
	  } else if (strcmp(name,"__ctype_toupper_loc") == 0) {
	    new_wn = WN_Lda(Pointer_Mtype, 0, IPA_builtin_ctype_toupper, 0);
	  }
	  // Replace call with LDA.
	  if (new_wn) {
	    WN *old_wn = wn;
	    wn = WN_next(wn);
	    FmtAssert(WN_operator(wn) == OPR_STID ||
		      WN_operator(wn) == OPR_ISTORE,
	    	      ("IPA_Rename_Builtins: STID for return val not found"));
	    WN *ret_val = WN_kid0(wn);
	    FmtAssert(WN_operator(ret_val) == OPR_LDID &&
	    	      WN_offset(ret_val) == -1,
	    	      ("IPA_Rename_Builtins: LDID for return val not found"));
	    WN_kid0(wn) = new_wn;
	    WN_EXTRACT_FromBlock(block, old_wn);
	  }
	}
      }
    }
  }
}
