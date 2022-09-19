/*

   Copyright (C) 2021 Xcalibyte (Shenzhen) Limited.

   Licensed under the Apache License, Version 2.0 (the "License");
   you may not use this file except in compliance with the License.
   You may obtain a copy of the License at

     http://www.apache.org/licenses/LICENSE-2.0

   Unless required by applicable law or agreed to in writing, software
   distributed under the License is distributed on an "AS IS" BASIS,
   WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
   See the License for the specific language governing permissions and
   limitations under the License.

 */

#ifndef mpl_type_infer_INCLUDED
#define mpl_type_infer_INCLUDED

/* this file should be included after all MAPLE
 * header files are included.
 */

class MPL2whirl;

enum TypeClass {
  TC_none      = 0x0,        // no hint
  TC_i8        = 0x1,        // int8 type
  TC_i16       = 0x2,        // int16 type
  TC_i32       = 0x4,        // int32 type
  TC_i64       = 0x8,        // int64 type
  TC_integer   = 0xf,        // any integer type
  TC_f32       = 0x10,       // float32 type
  TC_f64       = 0x20,       // float64 type
  TC_float     = 0x30,       // any float type
  TC_primitive = 0xff,       // any primitive type
  TC_prim_ptr  = 0x100,      // primitive pointer type
  TC_obj_ptr   = 0x200,      // object pointer type
  TC_func_ptr  = 0x400,      // function pointer type
  TC_label_ptr = 0x800,      // label pointer type
  TC_pointer   = 0xf00,      // any pointer type
  TC_object    = 0x1000,     // object type
  TC_array     = 0x2000,     // array type
  TC_string    = 0x4000,     // string type, should be 0x40?

  TC_dynamic   = 0x80000000, // still a dynamic type
};

class TyHint {
 private:
  TypeClass tyClass;
  TyIdx  tyIdx;

 public:
  TyHint() : tyClass(TC_dynamic), tyIdx() {}
  TyHint(TypeClass tc, TyIdx ty) : tyClass(tc), tyIdx(ty) {}
  TyHint(TypeClass tc, PrimType pty) : tyClass(tc), tyIdx(pty) {}

  void SetHint(TypeClass tc)       { tyClass = tc;     }
  void TrimHint(TypeClass tc)      { tyClass = (TypeClass)(tyClass & tc);    }
  void AddHint(TypeClass tc)       { tyClass = (TypeClass)(tyClass | tc);    }
  void RemoveHint(TypeClass tc)    { tyClass = (TypeClass)(tyClass & ~tc); }
  bool HasHint(TypeClass tc) const { return (tyClass & tc) == tc; }
  bool IsEmpty() const             { return tyClass == TC_dynamic && tyIdx == TyIdx(0); }

  TyHint GetPointerType() const;
  TyHint MergeHint(TyHint hint) const;

  bool IsDynamic() const;
  TyIdx GetTyIdx() const;
  PrimType GetPrimType() const;
};

struct StRefInfo
{
  StmtNode *stmt;
  TyHint hint;
  StRefInfo(StmtNode *s, TyHint t) : stmt(s), hint(t) {}
};

struct RetStmtInfo
{
  MIRFunction *func;
  StmtNode *stmt;
  TyHint hint;
  RetStmtInfo(MIRFunction *f, StmtNode *s, TyHint t) : func(f), stmt(s), hint(t) {}
};

typedef std::vector<StRefInfo> StRefsArray;
typedef std::vector<RetStmtInfo> FuncRetsArray;

class StTypeInfo
{
 private:
  StIdx stIdx;
  TyHint tyHint;
  StRefsArray refs;

 public:
  StTypeInfo() {}

  TyHint Hint() const { return tyHint; }

  const StRefsArray& Refs() const { return refs; }

  void  Clear() { refs.clear(); }

  TyHint AppendHint(TyHint hint);

  TyHint AppendHint(StmtNode *stmt, TyHint hint);
};

class FuncRetInfo
{
 private:
  PUIdx puIdx;
  TyHint tyHint;
  FuncRetsArray rets;

 public:
  FuncRetInfo() {}

  TyHint Hint() const { return tyHint; }

  const FuncRetsArray& Rets() const { return rets; }

  TyHint AppendHint(TyHint hint);

  TyHint AppendHint(MIRFunction *f, StmtNode *s, TyHint hint);
};

class MPLTypeInfer {
 protected:
  MIRModule *mod;
  MIRFunction *func;
  typedef std::vector<StTypeInfo> TypeInfoArray;
  typedef std::vector<FuncRetInfo> RetInfoArray;
  TypeInfoArray globalStTypeInfo;
  TypeInfoArray localStTypeInfo;
  RetInfoArray funcRetInfo;
  TyHint retTy;

 protected:
  // predef types for JS
  TyIdx anyTy;   // u64 handle
  TyIdx objTy;   // ptr->object
  TyIdx refTy;   // ref->something
  TyIdx strTy;   // ptr->string
  TyIdx arrTy;   // ptr->array
  // create predef types
  TyIdx CreateAnyTy();
  TyIdx CreateObjTy();
  TyIdx CreateRefTy();
  TyIdx CreateStrTy();
  TyIdx CreateArrTy();
  // get predef types
  TyIdx GetAnyTy() { return anyTy != 0 ? anyTy : CreateAnyTy(); }
  TyIdx GetObjTy() { return objTy != 0 ? objTy : CreateObjTy(); }
  TyIdx GetRefTy() { return refTy != 0 ? refTy : CreateRefTy(); }
  TyIdx GetStrTy() { return strTy != 0 ? strTy : CreateStrTy(); }
  TyIdx GetArrTy() { return arrTy != 0 ? arrTy : CreateArrTy(); }

 public:
  explicit MPLTypeInfer(MIRModule *module) : mod(module), func(NULL) {}

 private:
  TyHint CheckBuiltinObjectType(IntrinsiccallNode *call, TyHint *hint, uint32 opnd, bool result);
  TyHint GetIntrinsicType(MIRIntrinsicID intrn, TyHint *hint, uint32 opnd, bool result);
  TyHint CheckTypesExpr(StmtNode *stmt, BaseNode *expr, TyHint hint);
  void CheckTypesDeref(IassignNode *iass, TyHint hint);
  void CheckTypesStmt(StmtNode *stmt);
  void CheckTypesBlock(BlockNode *block);

  TyHint ResolveDynamicType(PrimType pty, TyHint res, TyHint opnd);
  TyHint FindType(StmtNode *stmt, StIdx stIdx, TyHint hint);
  TyHint FindType(StmtNode *stmt, PregIdx pregIdx, TyHint hint);
  void AnnotateType(StmtNode *stmt, StIdx stIdx, TyHint hint);
  void AnnotateType(StmtNode *stmt, PregIdx pregIdx, TyHint hint);
  void AddPendingCallStmt(StmtNode *stmt, PUIdx puIdx, TyHint hint);

  uint32 FixFieldType(MIRStructType *structTy, uint32 fieldID, uint32 curId, TyIdx ty);
  void FixPendingCallStmt();
  void FixFunctionProtoType();
  void FixTypeStmt(StmtNode *stmt, MIRSymbol *st);
  void FixType(TypeInfoArray& info, int scope);

 public:
  void CalibrateTypes();
};

#endif /* mpl_type_infer_INCLUDED */

