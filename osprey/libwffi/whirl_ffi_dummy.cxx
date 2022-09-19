/*
 * Copyright (C) 2021 Xcalibyte (Shenzhen) Limited.
 */

// ==================================================================
// whirl_ffi_dummy.cxx
//
// only call whirl core function with dummy parameters and return values
// so that the function prototype can be kept in WHIRL .B file and
// handled by whirl_ffi_gen to generate ffi binding for different foreign
// languages
//
// ==================================================================

// ==================================================================
// Usage:
// $ xvsa -sw -kp -xfa -c whirl_ffi_dummy.cxx -o whirl_ffi_dummy.o
// $ whirl_ffi_gen -x ts whirl_ffi_dummy.o
// $ whirl_ffi_gen -x python whirl_ffi_dummy.o
// ==================================================================

#include "wgen_core.h"

cstr_t cstr;   // for parameter or return value in C string type
ptr_t  ptr;    // for parameter or return value in pointer
ref_t  ref;    // for parameter or return value in reference
id_t   itr;
id_t   id_array[1];
ptr_t  ptr_array[1];

// dummy function, each call stmt will be handled by whirl_ffi_gen to
// generate wffi in foreign languages
void foo() {
  // Initialize & Finalize
  Wgen_Initialize(cstr, cstr, cstr);
  Wgen_Finalize();

  // String table related
  Wgen_EnterString(cstr);

  // Type table related
  Wgen_GetPrimitiveType(itr, itr, itr);
  Wgen_GetNumericType();
  Wgen_GetArrayType(itr, cstr);
  Wgen_GetStringType();
  Wgen_GetAnyType();
  Wgen_GetVoidType();
  Wgen_GetDefaultPointerTy();
  Wgen_GetPointerTy(itr);
  Wgen_GetPointedTy(itr);
  Wgen_SetFuncSTForRecordTy(itr, itr, id_array);
  Wgen_CreateIncompleteRecordType(cstr, itr);
  Wgen_GetRecordType(cstr, id_array, itr, itr, itr, itr);
  Wgen_SetVtableForDerivedTy(itr, itr);
  Wgen_GetDerivedTypeFromBase(cstr, itr);
  Wgen_STSetOptionalArgs(itr);
  Wgen_TYSetOptionalArgs(itr);
  Wgen_TYSetVarArgs(itr);
  Wgen_FunctionType(itr, id_array, itr);
  Wgen_GetReturnTy(itr);
  Wgen_GetParmTyFromFuncTy(itr, itr);
  Wgen_GetTyFromST(itr);
  Wgen_GetTyFromWN(ptr);
  Wgen_GetTYSize(itr);
  Wgen_GetElemTy(itr);
  Wgen_GetFldCount(itr);
  Wgen_GetFldTy(itr, itr);
  Wgen_GetFldName(itr, itr);
  Wgen_GetFieldIdByFldName(itr, cstr);
  Wgen_IsArrayType(itr);
  Wgen_IsStringType(itr);
  Wgen_IsRecordType(itr);
  Wgen_IsUnionType(itr);
  Wgen_IsAnyType(itr);
  Wgen_IsFunctionTy(itr);
  Wgen_TyHasVtable(itr);

  // Symbol table related
  Wgen_CreateSymbol(cstr, itr);
  Wgen_CreateFunctionSymbol(cstr, itr);

  // PU table related
  Wgen_CreateFunction(itr, itr, id_array, itr);
  Wgen_FinishFunction();
  Wgen_GetThisST();

  // Whirl node related
  Wgen_StmtAppend(ptr);
  Wgen_StartFunction(itr);
  Wgen_GetCurrentPUInfo();
  Wgen_GenNullConst(itr);
  Wgen_GetConst(itr, itr);
  Wgen_GetRValue(itr, itr, itr);
  Wgen_GetLValue(ptr, itr, itr);
  Wgen_CreateBlock();
  Wgen_CreateEval(ptr);
  Wgen_InsertBlockLast(ptr, ptr);
  Wgen_IsNullBlock(ptr);
  Wgen_OpcodeIsExpression(ptr);
  Wgen_CreateStringWN(cstr, itr, cstr);
  Wgen_EmitStringInit(ptr, ptr, ptr, itr);
  Wgen_EmitRecordInit(itr, itr, ptr_array);
  Wgen_Stid(itr, ptr, itr);
  Wgen_Istore(itr, ptr, ptr, itr);

  // expression related
  Wgen_ConvertBinaryExpr(itr, itr, ptr, ptr);
  Wgen_ConvertCompoundAssignOperator(itr, itr, itr, ptr, ptr);
  Wgen_ConvertUnaryExpr(itr, ptr);
  Wgen_GenerateIntrinsicCall(itr, itr, itr, ptr_array, itr);
  Wgen_ConvertCallExpr(itr, itr, id_array, ptr_array, itr);
  Wgen_ConvertICallExpr(itr, itr, id_array, ptr_array, itr, itr);
  Wgen_ConvertConditionalExpr(itr, ptr, ptr, ptr, itr);
  Wgen_ConvertMemberExpr(ptr, ptr);
  Wgen_ConvertArrayExpr(ptr, ptr);
  Wgen_GetLengthFromArray(ptr, itr);
  Wgen_ConvertPrePostIncDec(itr, itr, ptr, itr);
  Wgen_GetOperatorNewExpr(ptr_array, itr, itr, ptr);
  Wgen_GetRestValueFromArray(itr, ptr);
  Wgen_ConvertArrayToSet(itr, ptr);
  Wgen_ConvertSetToArray(itr, ptr);

  // stmt related
  Wgen_ConvertReturnStmt(ptr);
  Wgen_ConvertIfStmt(ptr, ptr, ptr);
  Wgen_GetLabelIdx();
  Wgen_GenLabelWN(itr);
  Wgen_ConvertCondInSwitchStmt(ptr);
  Wgen_RecordLoopSwitch(itr, itr, itr);
  Wgen_ConvertSwitchStmt(ptr, itr);
  Wgen_ConvertLoopStmt(itr, ptr, ptr, ptr, itr, itr);
  Wgen_ConvertCaseStmt(itr);
  Wgen_ConvertBreakStmt(itr);
  Wgen_ConvertContinueStmt(itr);
  Wgen_ConvertDefaultStmt();
}
