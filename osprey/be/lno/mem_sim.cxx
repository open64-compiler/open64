/*
 * Copyright (C) 2010 Advanced Micro Devices, Inc.  All Rights Reserved.
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


//                     Memory Simulation
//                     -----------------
//
// Description:
//
//	Instrument the code with calls to the memory simulator run-time.
//	Register every local and global array used by the subroutine.
//	Instrument every indirect memory reference
//
//
/* ====================================================================
 * ====================================================================
 *
 * Module: mem_sim.cxx
 * $Revision$
 * $Date$
 * $Author$
 * $Source$
 *
 * Revision history:
 *  02-07-96 - Original Version
 *
 * Description: Instrument code for memory simulator
 *
 * ====================================================================
 * ====================================================================
 */

#ifdef USE_PCH
#include "lno_pch.h"
#endif // USE_PCH
#pragma hdrstop

const static char *source_file = __FILE__;
const static char *rcs_id = "$Source$ $Revision$";

#ifndef _NEW_SYMTAB

#include <sys/types.h>
#include "lnopt_main.h"
#include "config_targ.h"
#include "stab.h"
#include "strtab.h"
#include "stblock.h"
#include "lwn_util.h"
#include "dep.h"
#include "lnoutils.h"
#include "targ_const.h"
#include "const.h"
#include "glob.h"

typedef HASH_TABLE<ST*, BOOL> STBOOL_HASH_TABLE;
typedef STACK<WN*> STACK_OF_WN;

static void  Instrument_Array_Ref(WN *, BOOL,INT, STACK_OF_WN *, STBOOL_HASH_TABLE *);
static void Instrument_Address(WN *address, BOOL is_load, INT num_bytes);
static ST *Create_MemTools_Store();
static ST *Create_MemTools_Load();
static WN *Find_Statement(WN *wn);
static INT Size(OPCODE opcode);
static void Instrument_Rec(WN *, STACK_OF_WN *, STACK_OF_WN *, STBOOL_HASH_TABLE *);
static void Remove_Locals(STACK_OF_WN *, STACK_OF_WN *);
static ST *Create_Local_Array_ST (TY* ty, INT num);
static void  Process_Base(WN *, INT , STACK_OF_WN *);
static ST *dim_sizes;

#define MAX_ARRAY_DIMS 8

void Instrument_Mem_Sim(WN *func_nd)
{
  dim_sizes = NULL;
  MEM_POOL_Push(&LNO_local_pool);
  // the STs of all the local arrays in the routine
  STACK_OF_WN *local_arrays = CXX_NEW(STACK_OF_WN(&LNO_local_pool),
					&LNO_local_pool);

  // the WNs of all the return points in the function
  STACK_OF_WN *return_points = CXX_NEW(STACK_OF_WN(&LNO_local_pool),
					&LNO_local_pool);
  // a hash table to map whether or not we have seen an array before
  STBOOL_HASH_TABLE *array_hash = 
    CXX_NEW(STBOOL_HASH_TABLE(200,&LNO_local_pool),&LNO_local_pool);

  Instrument_Rec(func_nd,local_arrays,return_points,array_hash);

  WN *last_statement = WN_last(WN_func_body(func_nd));
  if (WN_opcode(last_statement) != OPC_RETURN) return_points->Push(last_statement);
  Remove_Locals(local_arrays,return_points);
  CXX_DELETE(local_arrays,&LNO_local_pool);
  CXX_DELETE(return_points,&LNO_local_pool);
  CXX_DELETE(array_hash,&LNO_local_pool); 
  MEM_POOL_Pop(&LNO_local_pool);
}

static void Instrument_Rec(WN *wn, STACK_OF_WN *local_arrays, 
		STACK_OF_WN *return_points, STBOOL_HASH_TABLE *array_hash)
{
  OPCODE opcode = WN_opcode(wn);
  if (opcode == OPC_BLOCK) {
    WN *kid = WN_first (wn);
    while (kid) {
      Instrument_Rec(kid,local_arrays,return_points,array_hash);
      kid = WN_next(kid);
    }
  } else {
    for (INT kidno=0; kidno<WN_kid_count(wn); kidno++) {
      Instrument_Rec(WN_kid(wn,kidno),local_arrays,return_points,array_hash);
    }
    if (opcode == OPC_RETURN) {
      return_points->Push(wn);
    } else if (OPCODE_is_load(opcode) && (OPCODE_operator(opcode) != OPR_LDID)) {
      if (WN_operator(WN_kid0(wn)) == OPR_ARRAY) {
	Instrument_Array_Ref(WN_kid0(wn),TRUE,Size(opcode),
						local_arrays,array_hash);
      } else {
	Instrument_Address(WN_kid0(wn),TRUE,Size(opcode));
      }
    } else if (OPCODE_is_store(opcode) && (OPCODE_operator(opcode) != OPR_STID)) {
      if (WN_operator(WN_kid1(wn)) == OPR_ARRAY) {
	Instrument_Array_Ref(WN_kid1(wn),FALSE,Size(opcode),
						local_arrays,array_hash);
      } else {
	Instrument_Address(WN_kid1(wn),FALSE,Size(opcode));
      }
    }
  }
}

// How many bytes are we loading or storing
static INT Size(OPCODE opcode)
{
  switch(OPCODE_desc(opcode)) {
    case MTYPE_I1: case MTYPE_U1: return 1;
    case MTYPE_I2: case MTYPE_U2: return 2;
    case MTYPE_I4: case MTYPE_U4: case MTYPE_F4: return 4;
    case MTYPE_I8: case MTYPE_U8: case MTYPE_F8: case MTYPE_C4: return 8;
#if defined(TARG_IA64) || define(TARG_X8664)
    case MTYPE_F10:
#endif								
    case MTYPE_C8: case MTYPE_FQ: return 16;
    case MTYPE_C10:
    case MTYPE_CQ: return 32;
  }
  return 0;
}



// Find the statement surrounding wn
static WN *Find_Statement(WN *wn)
{
  while (OPCODE_is_expression(WN_opcode(wn))) wn = LWN_Get_Parent(wn);
  return wn;
}

// Create an st for MemoryTools_Load
static ST *Create_MemTools_Load()
{
  static st *result=NULL;
  if (result) return result;

  TY *func_ty = New_TY(TRUE);
  TY_kind(func_ty) = KIND_FUNCTION;
  TY_btype(func_ty) = MTYPE_UNKNOWN;
  Set_TY_has_prototype(func_ty);
  TY_ftinfo(func_ty) = New_FTI (2, TRUE /* global/local */ );
  TYLIST *parms = TY_parms(func_ty);
  TYLIST_item(&parms[0]) = Be_Type_Tbl(Pointer_type);
  TYLIST_item(&parms[1]) = Be_Type_Tbl(Pointer_type);
  TY_name(func_ty) = Save_Str(".MemoryTools_Load");
  TY *voidpty = Make_Pointer_Type (Be_Type_Tbl(MTYPE_V));
  TY_size(func_ty) = TY_size(voidpty);
  TY_align(func_ty) = TY_align(voidpty);
  TY_ret_type(func_ty) = Be_Type_Tbl(MTYPE_V);
  Enter_TY (func_ty);

  /* Make a ST: add function to global symbol table */
  result = New_ST ( TRUE );
  ST_name(result) = Save_Str("MemoryTools_Load");
  ST_class(result) = CLASS_FUNC;
  Set_ST_sclass(result,SCLASS_EXTERN);
  Set_ST_export(result, EXPORT_PREEMPTIBLE);
  ST_type(result) = func_ty;
  Enter_ST ( result);
  return result;
}

// Create an st for MemoryTools_Store
static ST *Create_MemTools_Store()
{
  static st *result=NULL;
  if (result) return result;

  TY *func_ty = New_TY(TRUE);
  TY_kind(func_ty) = KIND_FUNCTION;
  TY_btype(func_ty) = MTYPE_UNKNOWN;
  Set_TY_has_prototype(func_ty);
  TY_ftinfo(func_ty) = New_FTI (2, TRUE /* global/local */ );
  TYLIST *parms = TY_parms(func_ty);
  TYLIST_item(&parms[0]) = Be_Type_Tbl(Pointer_type);
  TYLIST_item(&parms[1]) = Be_Type_Tbl(Pointer_type);
  TY_name(func_ty) = Save_Str(".MemoryTools_Store");
  TY *voidpty = Make_Pointer_Type (Be_Type_Tbl(MTYPE_V));
  TY_size(func_ty) = TY_size(voidpty);
  TY_align(func_ty) = TY_align(voidpty);
  TY_ret_type(func_ty) = Be_Type_Tbl(MTYPE_V);
  Enter_TY (func_ty);

  /* Make a ST: add function to global symbol table */
  result = New_ST ( TRUE );
  ST_name(result) = Save_Str("MemoryTools_Store");
  ST_class(result) = CLASS_FUNC;
  Set_ST_sclass(result,SCLASS_EXTERN);
  Set_ST_export(result, EXPORT_PREEMPTIBLE);
  ST_type(result) = func_ty;
  Enter_ST ( result);
  return result;
}

// Create an st for MemoryTools_MakeGlobal
static ST *Create_MemTools_MakeGlobal()
{
  static st *result=NULL;
  if (result) return result;

  TY *func_ty = New_TY(TRUE);
  TY_kind(func_ty) = KIND_FUNCTION;
  TY_btype(func_ty) = MTYPE_UNKNOWN;
  Set_TY_has_prototype(func_ty);
  TY_ftinfo(func_ty) = New_FTI (6, TRUE /* global/local */ );
  TYLIST *parms = TY_parms(func_ty);
  TYLIST_item(&parms[0]) = Make_Pointer_Type(Be_Type_Tbl(MTYPE_U1));
  TYLIST_item(&parms[1]) = Be_Type_Tbl(Pointer_type);
  TYLIST_item(&parms[2]) = Be_Type_Tbl(Pointer_type);
  TYLIST_item(&parms[3]) = Be_Type_Tbl(Pointer_type);
  TYLIST_item(&parms[4]) = Make_Pointer_Type(Be_Type_Tbl(Pointer_type));
  TYLIST_item(&parms[5]) = Be_Type_Tbl(MTYPE_I4);
  TY_name(func_ty) = Save_Str(".MemoryTools_MakeGlobal");
  TY *voidpty = Make_Pointer_Type (Be_Type_Tbl(MTYPE_V));
  TY_size(func_ty) = TY_size(voidpty);
  TY_align(func_ty) = TY_align(voidpty);
  TY_ret_type(func_ty) = Be_Type_Tbl(MTYPE_V);
  Enter_TY (func_ty);

  /* Make a ST: add function to global symbol table */
  result = New_ST ( TRUE );
  ST_name(result) = Save_Str("MemoryTools_MakeGlobal");
  ST_class(result) = CLASS_FUNC;
  Set_ST_sclass(result,SCLASS_EXTERN);
  Set_ST_export(result, EXPORT_PREEMPTIBLE);
  ST_type(result) = func_ty;
  Enter_ST ( result);
  return result;
}

// Create an st for MemoryTools_MakeLocal
static ST *Create_MemTools_MakeLocal()
{
  static st *result=NULL;
  if (result) return result;

  TY *func_ty = New_TY(TRUE);
  TY_kind(func_ty) = KIND_FUNCTION;
  TY_btype(func_ty) = MTYPE_UNKNOWN;
  Set_TY_has_prototype(func_ty);
  TY_ftinfo(func_ty) = New_FTI (7, TRUE /* global/local */ );
  TYLIST *parms = TY_parms(func_ty);
  TYLIST_item(&parms[0]) = Make_Pointer_Type(Be_Type_Tbl(MTYPE_U1));
  TYLIST_item(&parms[1]) = Make_Pointer_Type(Be_Type_Tbl(MTYPE_U1));
  TYLIST_item(&parms[2]) = Be_Type_Tbl(Pointer_type);
  TYLIST_item(&parms[3]) = Be_Type_Tbl(Pointer_type);
  TYLIST_item(&parms[4]) = Be_Type_Tbl(Pointer_type);
  TYLIST_item(&parms[5]) = Make_Pointer_Type(Be_Type_Tbl(Pointer_type));
  TYLIST_item(&parms[6]) = Be_Type_Tbl(MTYPE_I4);
  TY_name(func_ty) = Save_Str(".MemoryTools_MakeLocal");
  TY *voidpty = Make_Pointer_Type (Be_Type_Tbl(MTYPE_V));
  TY_size(func_ty) = TY_size(voidpty);
  TY_align(func_ty) = TY_align(voidpty);
  TY_ret_type(func_ty) = Be_Type_Tbl(MTYPE_V);
  Enter_TY (func_ty);

  /* Make a ST: add function to global symbol table */
  result = New_ST ( TRUE );
  ST_name(result) = Save_Str("MemoryTools_MakeLocal");
  ST_class(result) = CLASS_FUNC;
  Set_ST_sclass(result,SCLASS_EXTERN);
  Set_ST_export(result, EXPORT_PREEMPTIBLE);
  ST_type(result) = func_ty;
  Enter_ST ( result);
  return result;
}

// Create an st for MemoryTools_RemoveLocal
static ST *Create_MemTools_RemoveLocal()
{
  static st *result=NULL;
  if (result) return result;

  TY *func_ty = New_TY(TRUE);
  TY_kind(func_ty) = KIND_FUNCTION;
  TY_btype(func_ty) = MTYPE_UNKNOWN;
  Set_TY_has_prototype(func_ty);
  TY_ftinfo(func_ty) = New_FTI (1, TRUE /* global/local */ );
  TYLIST *parms = TY_parms(func_ty);
  TYLIST_item(&parms[0]) = Make_Pointer_Type(Be_Type_Tbl(Pointer_type));
  TY_name(func_ty) = Save_Str(".MemoryTools_RemoveLocal");
  TY *voidpty = Make_Pointer_Type (Be_Type_Tbl(MTYPE_V));
  TY_size(func_ty) = TY_size(voidpty);
  TY_align(func_ty) = TY_align(voidpty);
  TY_ret_type(func_ty) = Be_Type_Tbl(MTYPE_V);
  Enter_TY (func_ty);

  /* Make a ST: add function to global symbol table */
  result = New_ST ( TRUE );
  ST_name(result) = Save_Str("MemoryTools_RemoveLocal");
  ST_class(result) = CLASS_FUNC;
  Set_ST_sclass(result,SCLASS_EXTERN);
  Set_ST_export(result, EXPORT_PREEMPTIBLE);
  ST_type(result) = func_ty;
  Enter_ST ( result);
  return result;
}


// Given an arbitrary load or store to address, instrument it
static void Instrument_Address(WN *address, BOOL is_load, INT num_bytes)
{
  WN *insertion_point = Find_Statement(address);
  WN *new_address = LWN_Copy_Tree(address);
  LWN_Copy_Def_Use(address,new_address,Du_Mgr);
  ST *func;
  if (is_load) {
    func = Create_MemTools_Load();
  } else {
    func = Create_MemTools_Store();
  }
  WN *call = WN_Create(OPC_VCALL,2);
  WN_st(call) = func;
  WN_linenum(call) = WN_linenum(insertion_point);
  WN_kid0(call) = LWN_CreateParm(Pointer_type,new_address,Be_Type_Tbl(Pointer_type),
			WN_PARM_BY_VALUE);
  LWN_Set_Parent(WN_kid0(call),call);
  WN_kid1(call) = LWN_CreateParm(Pointer_type,LWN_Make_Icon(Pointer_type,num_bytes),
			Be_Type_Tbl(Pointer_type),WN_PARM_BY_VALUE);
  LWN_Set_Parent(WN_kid1(call),call);
  LWN_Copy_Frequency(call,insertion_point);
  LWN_Insert_Block_Before(LWN_Get_Parent(insertion_point),insertion_point,
	call);
}

// Instrument array references
// For "clean" stride one references, generate an instrumentation call
// outside of the inner loop, otherwise use Instrument_Address
static void  Instrument_Array_Ref(WN *address, BOOL is_load, INT num_bytes,
		STACK_OF_WN *local_arrays, STBOOL_HASH_TABLE *array_hash)
{
  WN *base = WN_array_base(address);
  if (WN_operator(base) == OPR_LDA) {
    if (!array_hash->Find(WN_st(base))) {
      array_hash->Enter(WN_st(base),TRUE);
      Process_Base(base,num_bytes,local_arrays);
    }
  }
  WN *enclosing_loop = Enclosing_Do_Loop(address);
  INT depth;
  if (!enclosing_loop) {
    depth = -1;
  } else {
    depth = Do_Loop_Depth(enclosing_loop);
  }
  ACCESS_ARRAY *array = (ACCESS_ARRAY *) WN_MAP_Get(LNO_Info_Map,address);
  if (!array) {
    Instrument_Address(address,is_load,num_bytes);
    return;
  }
  BOOL stride1 = TRUE;
  if (array->Too_Messy || (array->Non_Const_Loops() > depth) ) {
    stride1 = FALSE;
  }
  if (array->Num_Vec() > MAX_ARRAY_DIMS) {
    Instrument_Address(address,is_load,num_bytes);
    return;
  }
  for (INT i=0; i<array->Num_Vec()-1; i++) {
    ACCESS_VECTOR *av = array->Dim(i);
    if (av->Loop_Coeff(av->Nest_Depth()-1)) {
      stride1 = FALSE;
    }
  }
  ACCESS_VECTOR *av = array->Dim(i);
  if (av->Loop_Coeff(av->Nest_Depth()-1) != 1) {
    stride1 = FALSE;
  }
  if (!stride1) {
    Instrument_Address(address,is_load,num_bytes);
    return;
  }

  WN *loop = Enclosing_Do_Loop(address);
  if (!loop) {
    Instrument_Address(address,is_load,num_bytes);
    return;
  }
  DO_LOOP_INFO *dli = Get_Do_Loop_Info(loop);
  ACCESS_VECTOR *Step = dli->Step;
  if (!Step->Is_Const() || (Step->Const_Offset != 1)) {
    Instrument_Address(address,is_load,num_bytes);
    return;
  }
  ACCESS_ARRAY *low_array = dli->LB;
  if (low_array->Too_Messy) {
    Instrument_Address(address,is_load,num_bytes);
    return;
  }
  for (i=0; i<low_array->Num_Vec(); i++) {
    ACCESS_VECTOR *av = low_array->Dim(i);
    if (av->Too_Messy) {
      Instrument_Address(address,is_load,num_bytes);
      return;
    }
  }

  WN *trip_count = LWN_Loop_Trip_Count(loop);
  if (!trip_count) {
    Instrument_Address(address,is_load,num_bytes);
    return;
  }


  WN *size = LWN_CreateExp2(
    OPCODE_make_op(OPR_MPY, WN_rtype(trip_count),MTYPE_V),
    trip_count, LWN_Make_Icon(WN_rtype(trip_count),num_bytes));


  // set new_address to point to the first array element
  WN *new_address = LWN_Copy_Tree(address);
  LWN_Copy_Def_Use(address,new_address,Du_Mgr);

  Replace_Ldid_With_Exp_Copy(SYMBOL(WN_start(loop)),new_address,
	WN_kid0(WN_start(loop)),Du_Mgr);

  ST *func;
  if (is_load) {
    func = Create_MemTools_Load();
  } else {
    func = Create_MemTools_Store();
  }
  WN *call = WN_Create(OPC_VCALL,2);
  WN_st(call) = func;
  WN_linenum(call) = WN_linenum(loop);
  WN_kid0(call) = LWN_CreateParm(Pointer_type,new_address,
		Be_Type_Tbl(Pointer_type), WN_PARM_BY_VALUE);
  LWN_Set_Parent(WN_kid0(call),call);
  WN_kid1(call) = LWN_CreateParm(Pointer_type,size,
			Be_Type_Tbl(Pointer_type),WN_PARM_BY_VALUE);
  LWN_Set_Parent(WN_kid1(call),call);
  LWN_Copy_Frequency(call,loop);
  LWN_Insert_Block_Before(LWN_Get_Parent(loop),loop,
	call);


}


// unregister all the local arrays
static void Remove_Locals(STACK_OF_WN *local_arrays,
		STACK_OF_WN *return_points) 
{
  ST *func_st = Create_MemTools_RemoveLocal();
  for (INT i=0; i<return_points->Elements(); i++) {
    WN *return_point = return_points->Bottom_nth(i);
    for (INT j=0; j<local_arrays->Elements(); j++) {
      WN *array = local_arrays->Bottom_nth(j);
      WN *call = WN_Create(OPC_VCALL,1);
      WN_st(call) = func_st;
      WN_kid0(call) = LWN_CreateParm(Pointer_type,LWN_Copy_Tree(array),
	Make_Pointer_Type(Be_Type_Tbl(Pointer_type)),WN_PARM_BY_REFERENCE);
      LWN_Set_Parent(WN_kid0(call),call);
      LWN_Insert_Block_Before(LWN_Get_Parent(return_point),return_point, call);
    }
  }
}

// register a new array, base is the base of the array statement
static void  Process_Base(WN *base, INT num_bytes, STACK_OF_WN *local_arrays)
{
  BOOL is_fortran;
  switch (SYMTAB_src_lang(Current_Symtab)) {
    case SYMTAB_F90_LANG:
    case SYMTAB_F77_LANG:
      is_fortran=TRUE;
      break;
    default: is_fortran=FALSE;
  }

  WN *array = LWN_Get_Parent(base);
  ST *st = WN_st(base);
  BOOL is_local = FALSE;
  if (ST_symtab_id(st) == SYMTAB_id(Current_Symtab)) {
    if (ST_sclass(st) == SCLASS_AUTO || ST_sclass(st) == SCLASS_TEMP) {
      is_local = TRUE;
      local_arrays->Push(base);
    }
  }

  WN *func_nd = base;
  while (LWN_Get_Parent(func_nd)) func_nd = LWN_Get_Parent(func_nd);
  WN *first_statement = WN_first(WN_func_body(func_nd));

  if (!dim_sizes) { // create a local variable that will hold dim sizes
     dim_sizes = 
	Create_Local_Array_ST(Be_Type_Tbl(Pointer_type),MAX_ARRAY_DIMS); 
  }

  // initialize the local variable
  for (INT i=0; i<WN_num_dim(array); i++) {
    WN *dim = WN_array_dim(array,i);
    Is_True(WN_operator(dim) == OPR_INTCONST,
	("Non const dimension for a local or global array "));
    OPCODE op_array = OPCODE_make_op(OPR_ARRAY,Pointer_type,MTYPE_V);
    OPCODE lda_op = OPCODE_make_op(OPR_LDA,Pointer_type,MTYPE_V);
    WN *dim_array = WN_Create(op_array,3);
    WN_element_size(dim_array) = 4;
    WN_array_base(dim_array) = WN_CreateLda(lda_op,0,
	Make_Pointer_Type(ST_type(dim_sizes)) ,dim_sizes);
    LWN_Set_Parent(WN_array_base(dim_array),dim_array);
    if (is_fortran) {
      WN_array_index(dim_array,0) = 
	LWN_Make_Icon(Pointer_type,WN_num_dim(array)-i-1);
    } else {
      WN_array_index(dim_array,0) = LWN_Make_Icon(Pointer_type,i);
    }
    LWN_Set_Parent(WN_array_index(dim_array,0),dim_array);
    WN_array_dim(dim_array,0) = LWN_Make_Icon(Pointer_type,MAX_ARRAY_DIMS);
    LWN_Set_Parent(WN_array_dim(dim_array,0),dim_array);
    WN *store = LWN_CreateIstore(
	OPCODE_make_op(OPR_ISTORE, MTYPE_V, Pointer_type),
	0,Make_Pointer_Type(Be_Type_Tbl(Pointer_type)),
	LWN_Make_Icon(Pointer_type,WN_const_val(dim)),
	dim_array);
    LWN_Insert_Block_Before(LWN_Get_Parent(first_statement),first_statement,
	store);
  }
  ST *func;
  INT func_name_kid = 0;
  if (is_local) {
    func = Create_MemTools_MakeLocal();
    func_name_kid++;
  } else {
    func = Create_MemTools_MakeGlobal();
  }
  WN *call = WN_Create(OPC_VCALL,6+func_name_kid);
  WN_st(call) = func;

  TCON tc = Host_To_Targ_String ( MTYPE_STRING, ST_name(WN_st(base)), strlen(ST_name(WN_st(base)))+1);
  ST *string = Gen_String_Sym ( &tc, MTYPE_To_TY(MTYPE_STRING), FALSE );
  OPCODE lda_op = OPCODE_make_op(OPR_LDA,Pointer_type,MTYPE_V);
  WN *lda = WN_CreateLda(lda_op,0,Make_Pointer_Type(Be_Type_Tbl(MTYPE_U1)),string);

  WN_kid0(call) = LWN_CreateParm(Pointer_type,lda,
	Make_Pointer_Type(Be_Type_Tbl(MTYPE_U1)),WN_PARM_BY_REFERENCE);
  LWN_Set_Parent(WN_kid0(call),call);

  if (func_name_kid) {
    TCON tc = Host_To_Targ_String ( MTYPE_STRING,Cur_PU_Name,
	strlen(Cur_PU_Name)+1);
    ST *string = Gen_String_Sym ( &tc, MTYPE_To_TY(MTYPE_STRING), FALSE );
    OPCODE lda_op = OPCODE_make_op(OPR_LDA,Pointer_type,MTYPE_V);
    WN *lda = WN_CreateLda(lda_op,0,
		Make_Pointer_Type(Be_Type_Tbl(MTYPE_U1)),string);

    WN_kid1(call) = LWN_CreateParm(Pointer_type,lda,
	Make_Pointer_Type(Be_Type_Tbl(MTYPE_U1)),WN_PARM_BY_REFERENCE);
    LWN_Set_Parent(WN_kid1(call),call);
  }

  WN *address = LWN_Copy_Tree(base);
  WN_kid(call,1+func_name_kid) =
    LWN_CreateParm(Pointer_type,address,Be_Type_Tbl(Pointer_type),
			WN_PARM_BY_VALUE);
  LWN_Set_Parent(WN_kid(call,1+func_name_kid),call);

  WN_kid(call,2+func_name_kid) =LWN_CreateParm(MTYPE_U4,
			LWN_Make_Icon(MTYPE_I4,WN_num_dim(array)),
			Be_Type_Tbl(MTYPE_I4), WN_PARM_BY_VALUE);
  LWN_Set_Parent(WN_kid(call,2+func_name_kid),call);

  WN_kid(call,3+func_name_kid) =LWN_CreateParm(Pointer_type,
			LWN_Make_Icon(Pointer_type,num_bytes),
			Be_Type_Tbl(Pointer_type), WN_PARM_BY_VALUE);
  LWN_Set_Parent(WN_kid(call,3+func_name_kid),call);

  lda_op = OPCODE_make_op(OPR_LDA,Pointer_type,MTYPE_V);
  lda = WN_CreateLda(lda_op,0,Make_Pointer_Type(ST_type(dim_sizes)),dim_sizes);
  WN_kid(call,4+func_name_kid) = LWN_CreateParm(Pointer_type,lda,
    Make_Pointer_Type(Be_Type_Tbl(Pointer_type)),WN_PARM_BY_REFERENCE);
  LWN_Set_Parent(WN_kid(call,4+func_name_kid),call);

  WN_kid(call,5+func_name_kid) = 
	LWN_CreateParm(MTYPE_I4,LWN_Make_Icon(MTYPE_I4,!is_fortran),
	Be_Type_Tbl(MTYPE_I4),WN_PARM_BY_VALUE);
  LWN_Set_Parent(WN_kid(call,5+func_name_kid),call);

  LWN_Insert_Block_Before(LWN_Get_Parent(first_statement),first_statement,
	call);

}


static ST* Create_Local_Array_ST (TY* ty, INT num) {
  char name[64];
  ST *st;
  TY *arr_ty;
  ARI *ari;
  
  ari = New_ARI (1, FALSE);
  ARI_etype(ari) = ty;
  ARB_const_lbnd(ARI_bnd(ari, 0)) = TRUE;
  ARB_const_ubnd(ARI_bnd(ari, 0)) = TRUE;
  ARB_const_stride(ARI_bnd(ari, 0)) = TRUE;
  ARB_lbnd_val(ARI_bnd(ari, 0)) = 0;
  ARB_ubnd_val(ARI_bnd(ari, 0)) = num-1;
  ARB_stride_val(ARI_bnd(ari, 0)) = 1;
  

  arr_ty = New_TY(FALSE);
  TY_kind(arr_ty) = KIND_ARRAY;
  TY_btype(arr_ty) = MTYPE_M;
  TY_arinfo(arr_ty) = ari;
  sprintf (name, "tmp_num_dims");
  TY_name(arr_ty) = Save_Str(name);
  TY_size(arr_ty) = TY_size(ty)*num;
  TY_align(arr_ty) = 8;
  Enter_TY(arr_ty);
  

  st = New_ST(FALSE);
  ST_name(st) = Save_Str(name);
  ST_class(st) = CLASS_VAR;
  Set_ST_sclass(st, SCLASS_AUTO);
  Set_ST_is_temp_var(st);
  ST_type(st) = arr_ty;
  Enter_ST (st);
  Set_ST_pt_to_unique_mem(st);
  return st;
}

#endif
