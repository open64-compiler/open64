/*
 * Copyright (C) 2021 Xcalibyte (Shenzhen) Limited.
 */

// ==================================================================
// wgen_core.h
// core API definition for WHIRL Foreign Function Interface
// ==================================================================

#ifndef _wgen_core_INCLUDED
#define _wgen_core_INCLUDED

#include <stdint.h>

// ==================================================================
// types definition
// We won't export internal WHIRL types, wrapper types defined below
// ==================================================================

typedef uint32_t    id_t;   // id type for TY_IDX, ST_IDX, PU_IDX, etc
typedef uintptr_t   ptr_t;  // pointer type for WN*
typedef uintptr_t   ref_t;  // reference type
typedef uint64_t    spos_t; // srcpos
typedef const char* cstr_t; // C string type

#ifdef __cplusplus
extern "C" {
#endif

// ==================================================================
// Initialize & Finalize
// ==================================================================

// initialize whirl generator
void Wgen_Initialize(cstr_t src_fname, cstr_t whirl_fname, cstr_t abi);

// finalize whirl generator
void Wgen_Finalize();

// ==================================================================
// String table related
// ==================================================================

// add a string to whirl string table
id_t Wgen_EnterString(cstr_t str);

// ==================================================================
// Type table related
// ==================================================================

// get primitive type according to size and types
id_t Wgen_GetPrimitiveType(int size, bool is_unsigned, bool is_fp);

// get numeric const type
id_t Wgen_GetNumericType();

// get vector type for primitive type
id_t Wgen_GetVectorType(id_t prim, int size);

// get array type for primitive type
id_t Wgen_GetArrayType(id_t elem_ty, cstr_t name);

// get string type
id_t Wgen_GetStringType();

// generate a void ptr for AnyKeyword for the moment
id_t Wgen_GetAnyType();

// get void type
id_t Wgen_GetVoidType();

// get default pointer ty
id_t Wgen_GetDefaultPointerTy();

// get pointer type of ty
id_t Wgen_GetPointerTy(id_t ty);

// get pointed type of ty
id_t Wgen_GetPointedTy(id_t ty);

// set method func list for record ty
void Wgen_SetFuncSTForRecordTy(id_t ty_idx, id_t length, id_t func_st[]);

// create incomplete record ty idx
id_t Wgen_CreateIncompleteRecordType(cstr_t name, bool is_union);

// get record type for primitive type
id_t Wgen_GetRecordType(cstr_t name, id_t field_types[], int field_num, bool is_union, id_t vtable_size = 0, bool need_vtable = false, id_t ty_idx = 0);

// set vtable for derived ty
void Wgen_SetVtableForDerivedTy(id_t ty_idx, id_t derived_ty);

// get derived type of ty_idx
id_t Wgen_GetDerivedTypeFromBase(cstr_t name, id_t ty_idx);

// set st flat for optional arg
void Wgen_STSetOptionalArgs(id_t param_st);

// set ty flag for optional args
void Wgen_TYSetOptionalArgs(id_t pu_ty);

// set ty flag for varargs
void Wgen_TYSetVarArgs(id_t pu_ty);

// get function type
id_t Wgen_FunctionType(id_t ret_ty, id_t parm_types[], int length);

// get return ty from function type
id_t Wgen_GetReturnTy(id_t func_ty);

// get parm ty from func ty and index
id_t Wgen_GetParmTyFromFuncTy(id_t func_ty, id_t index);

// get type from st
id_t Wgen_GetTyFromST(id_t st_idx);

// get type from WN
id_t Wgen_GetTyFromWN(ptr_t wn);

// get ty size
id_t Wgen_GetTYSize(id_t ty_idx);

// get elem ty
id_t Wgen_GetElemTy(id_t ty_idx);

// get fld count
id_t Wgen_GetFldCount(id_t ty_idx);

// get fld ty
id_t Wgen_GetFldTy(id_t ty_idx, id_t field_id);

// get fld name by field_id
cstr_t Wgen_GetFldName(id_t ty_idx, id_t field_id);

// get field id by fld name
id_t Wgen_GetFieldIdByFldName(id_t ty_idx, cstr_t fld_name);

// return TRUE if ty_idx is array ty
bool Wgen_IsArrayType(id_t ty_idx);

// return TRUE if ty_idx is string ty
bool Wgen_IsStringType(id_t ty_idx);

// return TRUE if ty_idx is record ty
bool Wgen_IsRecordType(id_t ty_idx);

// return TRUE if ty_idx is union ty
bool Wgen_IsUnionType(id_t ty_idx);

// return TRUE if ty_idx is Any ty
bool Wgen_IsAnyType(id_t ty_idx);

// return TRUE if ty_idx is function ty
bool Wgen_IsFunctionTy(id_t ty_idx);

// return TRUE if ty_idx has vtable
bool Wgen_TyHasVtable(id_t ty_idx);

// ==================================================================
// Symbol table related
// ==================================================================

// create symbol
id_t Wgen_CreateSymbol(cstr_t str_name, id_t ty_idx);

// create function symbol
id_t Wgen_CreateFunctionSymbol(cstr_t str_name, id_t pu_ty_idx);

// ==================================================================
// PU table related
// ==================================================================

// create function
void Wgen_CreateFunction(id_t st_idx, int num_params, id_t parms[], id_t record_ty);

// create PU Info
ptr_t Wgen_CreatePUInfo (id_t st_idx);

// get this st from current entry wn
id_t Wgen_GetThisST();

// ==================================================================
// Initv & Inito table related
// ==================================================================


// ==================================================================
// Whirl node related
// ==================================================================

// append wn into body block of current function
void Wgen_StmtAppend(ptr_t wn);

// start of create function
void Wgen_StartFunction(id_t st_idx);

// end of create function
void Wgen_FinishFunction();

// get current pu
ptr_t Wgen_GetCurrentPUInfo();

// generate null const whirl node
ptr_t Wgen_GenNullConst(id_t ty_idx);

// create const wn
ptr_t Wgen_GetConst(id_t ty, id_t init);

// get rvalue
ptr_t Wgen_GetRValue(id_t st_idx, id_t ofst, int field);

// get lvalue
ptr_t Wgen_GetLValue(ptr_t base, id_t ty_idx, int field);

// create a new block
ptr_t Wgen_CreateBlock();

// insert wn after the last node in blk
ptr_t Wgen_InsertBlockLast(ptr_t blk, ptr_t wn);

// return TRUE if blk is a null block
bool Wgen_IsNullBlock(ptr_t blk);

// return TRUE if opcode occurs at top of an expression tree
bool Wgen_OpcodeIsExpression(ptr_t wn);

// create eval
ptr_t Wgen_CreateEval(ptr_t wn);

// create string lda wn
ptr_t Wgen_CreateStringWN(cstr_t str, int length, cstr_t name);

// emit String initialization
ptr_t Wgen_EmitStringInit(ptr_t len, ptr_t str, ptr_t dest, id_t ty_idx);

// emit record initialization
ptr_t Wgen_EmitRecordInit(id_t st_idx, id_t length, ptr_t inits[]);

// create STID
ptr_t Wgen_Stid(id_t st_idx, ptr_t init, id_t field_id);

// create ISTROE
ptr_t Wgen_Istore(id_t ty_idx, ptr_t addr, ptr_t init, id_t field_id);

// ==================================================================
// handle expression related functions
// ==================================================================

// handle binary operator expression
ptr_t Wgen_ConvertBinaryExpr(id_t kind, id_t ty_idx, ptr_t lhs, ptr_t rhs);

// handle compound assign operator
ptr_t Wgen_ConvertCompoundAssignOperator(id_t kind, id_t ty_idx, id_t st_idx, ptr_t lhs, ptr_t rhs);

// handle unary operator expression
ptr_t Wgen_ConvertUnaryExpr(id_t kind, ptr_t sub);

// generate intrinsic call
ptr_t Wgen_GenerateIntrinsicCall(id_t intrinsic, id_t ret_ty, id_t args_num, ptr_t args_wn[], bool retv);

// convert call expression
ptr_t Wgen_ConvertCallExpr(id_t st_idx, id_t length, id_t args_ty[], ptr_t args_wn[], bool retv);

// convert icall expression
ptr_t Wgen_ConvertICallExpr(id_t func_ty, id_t length, id_t args_ty[], ptr_t args_wn[], id_t offset, bool retv);

// convert conditional expression
ptr_t Wgen_ConvertConditionalExpr(id_t ty_idx, ptr_t cond, ptr_t true_wn, ptr_t false_wn, bool retv);

// convert member expression
ptr_t Wgen_ConvertMemberExpr(ptr_t base_wn, ptr_t index);

// convert array access expression
ptr_t Wgen_ConvertArrayExpr(ptr_t base_wn, ptr_t index);

// get length from array obj
ptr_t Wgen_GetLengthFromArray(ptr_t st_idx, id_t ty_idx);

// convert PreInc/PreDec/PostInc/PostDec expression
ptr_t Wgen_ConvertPrePostIncDec(bool is_prefix, bool is_add, ptr_t base, bool retv);

// convert New expression
ptr_t Wgen_GetOperatorNewExpr(ptr_t args[], id_t arg_length, id_t ty_idx, ptr_t blk);

// get rest value from Array
ptr_t Wgen_GetRestValueFromArray(id_t array_ty, ptr_t wn);

// convert Array to Set
ptr_t Wgen_ConvertArrayToSet(id_t ty, ptr_t array);

// convert Set to Array
ptr_t Wgen_ConvertSetToArray(id_t ty, ptr_t set);

// ==================================================================
// handle stmt related functions
// ==================================================================

// convert Return stmt
ptr_t Wgen_ConvertReturnStmt(ptr_t ret);

// convert If stmt
ptr_t Wgen_ConvertIfStmt(ptr_t cond, ptr_t then_blk, ptr_t else_blk);

// generate a new label idx
id_t Wgen_GetLabelIdx();

// generate label wn
ptr_t Wgen_GenLabelWN(id_t label_idx);

// record loop switch index
void Wgen_RecordLoopSwitch(id_t kind, id_t break_label_idx, id_t continue_label_idx);

// handle cond wn in switch stmt
ptr_t Wgen_ConvertCondInSwitchStmt(ptr_t cond);

// convert Switch stmt
ptr_t Wgen_ConvertSwitchStmt(ptr_t body_blk, id_t body_exit_label_idx);

// convert Loop stmt
ptr_t Wgen_ConvertLoopStmt(id_t kind, ptr_t cond, ptr_t body_blk, ptr_t inc_blk, id_t break_label, id_t continue_label);

// convert case stmt
ptr_t Wgen_ConvertCaseStmt(id_t val);

// convert break stmt
ptr_t Wgen_ConvertBreakStmt(id_t label_idx);

// convert continue stmt
ptr_t Wgen_ConvertContinueStmt(id_t label_idx);

// convert default stmt
ptr_t Wgen_ConvertDefaultStmt();

#ifdef __cplusplus
}
#endif
#endif
