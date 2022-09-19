/*
 * Copyright (C) 2021 Xcalibyte (Shenzhen) Limited.
 */

package io.xcalibyte;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import soot.*;
import soot.JastAddJ.Modifiers;
import soot.jimple.*;
import soot.tagkit.*;
import soot.toolkits.scalar.Pair;
import static org.assertj.core.api.Assertions.*;

import java.nio.charset.Charset;
import java.util.*;

import static io.xcalibyte.InternalConstants.*;
import static io.xcalibyte.WhirlConstants.*;

class SymbolHandler {

  private static Logger logger = LogManager.getLogger(SymbolHandler.class);
  private static Map<Object, Long> cache = new HashMap<>();
  private static Map<Integer, Long> methodLocalCache = new HashMap<>();
  // cache for runtime symbols
  private static Map<JvMethod, Long> rtCache = new HashMap<>();
  private static Map<Integer, Long> localPregCache = new HashMap<>();
  private static Map<RefType, Long> ehTypeSymbolCache = new HashMap<>();
  private static Map<Long, Long> tempLocalCache = new HashMap<>();
  private static Map<JvMethod, Long> throwsCatch = new HashMap<>();
  private static long tempLocalCount = 0L;


  /*************************************************************************************
   * Get Symbol of a Static Field
   * @param fd STATIC field to get symbol of
   * @return the symbol_idx of the static field
   *************************************************************************************/
  static long getSymbol(SootField fd) {
    assertThat(fd.isStatic()).as( "Non static field, should use member field to handle.").isTrue();
    if (cache.containsKey(fd)) {
      return cache.get(fd);
    }
    String mangledName = MangleTool.mangle(fd);
    long typeIdx = TypeHandler.getType(fd);
    long symbolIdx = BGenDriver.createVarSymbol(
      mangledName, typeIdx,
      SClass.SCLASS_EXTERN.toInt(), SExport.EXPORT_PREEMPTIBLE.toInt(), 1
    );
    cache.put(fd, symbolIdx);
    return symbolIdx;
  }

  static long getSymbol(StaticFieldRef fr) {
    SootField fd = fr.getField();
    return getSymbol(fd);
  }


  static long getSymbol(SootMethodRef methodRef) {
    String mangledName = MangleTool.mangle(methodRef);
    if(mangledName.startsWith("_ZN2io3xc510RBC_ENGINE")) {
      BGenDriver.setCurrPuRbc();
    }
    if (cache.containsKey(mangledName)) {
      return cache.get(mangledName);
    }
    long typeIdx = TypeHandler.getType(methodRef);
    SootMethod method = methodRef.tryResolve();
    if (method != null && (method.getModifiers() & Modifiers.ACC_VARARGS) != 0) {
      BGenDriver.setTypePuAttribute(typeIdx, TyPuFlag.TY_IS_VARARGS.toInt());
    }
    long symbolIdx = BGenDriver.createFunctionSymbol(
            mangledName, typeIdx,
            SClass.SCLASS_EXTERN.toInt(), SExport.EXPORT_PREEMPTIBLE.toInt(), 1
    );
    cache.put(mangledName, symbolIdx);
    BGenDriver.setPuLangJava(symbolIdx);
    if(methodRef.name().equals(SootMethod.constructorName)) {
      long class_ty = TypeHandler.getType(methodRef.declaringClass());
      BGenDriver.setPuConstructor(symbolIdx, class_ty);
      if(methodRef.parameterTypes().size() == 1 &&
         methodRef.parameterType(0) == methodRef.declaringClass().getType()) {
        BGenDriver.setClassCopyCtor(class_ty, symbolIdx);
      }
    }
    return symbolIdx;
  }


  static long getSymbol(SootMethod method) {
    String mangledName = MangleTool.mangle(method);
    if (cache.containsKey(mangledName)) {
      return cache.get(mangledName);
    }
    if(B2WFrontEnd.i().isGcj()) {
      assertThat(!(method.getDeclaringClass().isAbstract() && method.isAbstract())).as("Can't get abstract method.").isTrue();
    }
    long typeIdx = TypeHandler.getType(method);
    long symbolIdx = BGenDriver.createFunctionSymbol(
      mangledName, typeIdx,
      SClass.SCLASS_EXTERN.toInt(), SExport.EXPORT_PREEMPTIBLE.toInt(), 1
    );
    cache.put(mangledName, symbolIdx);
    if (method.getDeclaringClass().isAbstract()) {
      BGenDriver.setMiscFlags(symbolIdx, WhirlConstants.FlagKind.FLAG_SET_STFLAG_EXT.toInt(),
              WhirlConstants.STFlagsExt.ST_JAVA_ABSTRACT.toInt());
    }
    BGenDriver.setPuLangJava(symbolIdx);
    if (method.isConstructor()) {
      long class_ty = TypeHandler.getType(method.getDeclaringClass());
      BGenDriver.setPuConstructor(symbolIdx, class_ty);
      if(method.getParameterCount() == 1 &&
         method.getParameterType(0) == method.getDeclaringClass().getType()) {
        BGenDriver.setClassCopyCtor(class_ty, symbolIdx);
      }
    }
    return symbolIdx;
  }

  static long getNativeSymbol(String mangledName, Type rtype, List<Type> paramType) {
    if (cache.containsKey(mangledName)) {
      return cache.get(mangledName);
    }
    long typeIdx = TypeHandler.getMethodType(rtype, paramType);
    long symbolIdx = BGenDriver.createFunctionSymbol(
      mangledName, typeIdx,
      SClass.SCLASS_EXTERN.toInt(), SExport.EXPORT_PREEMPTIBLE.toInt(), 1
    );
    cache.put(mangledName, symbolIdx);
    BGenDriver.setPuLangCXX(symbolIdx);
    return symbolIdx;
  }
 
  static long getSymbol(Local local) {
    if (methodLocalCache.containsKey(local.getNumber())) {
      return methodLocalCache.get(local.getNumber());
    }
    long symbolIdx = -1;
    long typeIdx;
    if(local.getType() instanceof NullType && local.getType().getArrayType() instanceof ArrayType)
      typeIdx = TypeHandler.getType(local.getType().getArrayType());
    else
      typeIdx = TypeHandler.getType(local);
    symbolIdx = BGenDriver.createVarSymbol(
      local.getName(), typeIdx,
      SClass.SCLASS_AUTO.toInt(),
      SExport.EXPORT_LOCAL.toInt(), 2
    );
    methodLocalCache.put(local.getNumber(), symbolIdx);
    return symbolIdx;
  }

  static long getEHTypeSymbol(RefType type) {
    if(ehTypeSymbolCache.containsKey(type)) {
      return ehTypeSymbolCache.get(type);
    }
    long typeIdx = TypeHandler.getType(type);
    long symbolIdx = BGenDriver.createVarSymbol(type.getClassName() + "_ref", typeIdx,  // FIXME: hard-coded name
                                            SClass.SCLASS_FSTATIC.toInt(),
                                            SExport.EXPORT_PREEMPTIBLE.toInt(), 1);
    ehTypeSymbolCache.put(type, symbolIdx);
    long initSymbolIdx = getInternal(type.getSootClass(), SymbolCategory.CLASS_SYMBOL);
    SymbolInitializer.setFstatic(symbolIdx);
    SymbolInitializer.initSymbol(symbolIdx, initSymbolIdx);
    return symbolIdx;
  }

  static void finishMethodLocals() {
    methodLocalCache.clear();
  }

  // below are help function for runtime symbol
  // _Jv_XXXX
  static long getJvMethodSym(JvMethod method) {
    if (rtCache.containsKey(method)) {
      return rtCache.get(method);
    }
    long methodSymIdx;
    // javaprims.h
    // void *_Jv_AllocObjectNoFinializer(java.lang.Class *)
    if(method == JvMethod._Jv_AllocObjectNoFinalizer) {
      // return void*
      long retTypeIdx = TypeHandler.getInternal(GCJType.V_PTR);
      long[] argTypes = { TypeHandler.getInternal(GCJType.JAVA_LANG_CLASS_PTR) };
      long funcTypeIdx = BGenDriver.createFunctionType(retTypeIdx, argTypes, BCRConfig.DEFAULT_PU_ALIGN);
      methodSymIdx = BGenDriver.createExternFunctionSymbol("_Jv_AllocObjectNoFinalizer", funcTypeIdx);
    }
    // jvm.h
    // void *_Jv_LookupInterfaceMethodIdx(java.lang.Class *, java.clang.Class *, int)
    else if(method == JvMethod._Jv_LookupInterfaceMethodIdx) {
      long retTypeIdx = TypeHandler.getInternal(GCJType.V_PTR);
      long[] argTypes = new long[3];
      argTypes[0] = argTypes[1] = TypeHandler.getInternal(GCJType.JAVA_LANG_CLASS_PTR);
      argTypes[2] = BGenDriver.getPrimitiveType(Mtype.MTYPE_I4.toInt());
      long funcTypeIdx = BGenDriver.createFunctionType(retTypeIdx, argTypes, BCRConfig.DEFAULT_PU_ALIGN);
      methodSymIdx = BGenDriver.createExternFunctionSymbol("_Jv_LookupInterfaceMethodIdx", funcTypeIdx);
    }
    // Class.h
    // void _Jv_InitClass(java.lang.Class *)
    // Jv_InitClass for static usage of outer class or any static method
    else if(method == JvMethod._Jv_InitClass) {
      // return void*
      long retTypeIdx = TypeHandler.getType(VoidType.v());
      long[] argTypes = { TypeHandler.getInternal(GCJType.JAVA_LANG_CLASS_PTR) }; // Java_Lang_Class_PTR
      long funcTypeIdx = BGenDriver.createFunctionType(retTypeIdx, argTypes, BCRConfig.DEFAULT_PU_ALIGN);
      methodSymIdx = BGenDriver.createExternFunctionSymbol("_Jv_InitClass", funcTypeIdx);
    }
    // void *_Jv_NewObjectArray(java.lang.Class *, void)
    else if(method == JvMethod._Jv_NewObjectArray) {
      // return void*
      long retTypeIdx = TypeHandler.getInternal(GCJType.V_PTR);
      long[] argTypes = {
              BGenDriver.getPrimitiveType(Mtype.MTYPE_I4.toInt()),
              TypeHandler.getInternal(GCJType.JAVA_LANG_CLASS_PTR),
              TypeHandler.getInternal(GCJType.JAVA_LANG_OBJECT_PTR)
      };
      long funcType = BGenDriver.createFunctionType(retTypeIdx, argTypes, BCRConfig.DEFAULT_PU_ALIGN);
      methodSymIdx = BGenDriver.createExternFunctionSymbol("_Jv_NewObjectArray", funcType);
    }
    // void *_Jv_NewPrimArray(java.lang.Class *, void)
    else if(method == JvMethod._Jv_NewPrimArray) {
      // return void*
      long retTypeIdx = TypeHandler.getInternal(GCJType.JAVA_LANG_OBJECT_PTR);
      long[] argTypes = {
        TypeHandler.getInternal(GCJType.JAVA_LANG_CLASS_PTR),
        BGenDriver.getPrimitiveType(Mtype.MTYPE_I4.toInt())
      };
      long funcType = BGenDriver.createFunctionType(retTypeIdx, argTypes, BCRConfig.DEFAULT_PU_ALIGN);
      methodSymIdx = BGenDriver.createExternFunctionSymbol("_Jv_NewPrimArray", funcType);
    }
    // void *_Jv_NewMultiArray(java.lang.Class *, int, int *)
    else if(method == JvMethod._Jv_NewMultiArray) {
      long retTypeIdx = TypeHandler.getInternal(GCJType.V_PTR);
      long i4TyIdx = BGenDriver.getPrimitiveType(Mtype.MTYPE_I4.toInt());
      long[] argTypes = {
        TypeHandler.getInternal(GCJType.JAVA_LANG_CLASS_PTR),
        i4TyIdx, BGenDriver.jniMakePointerType(i4TyIdx)
      };
      long funcType = BGenDriver.createFunctionType(retTypeIdx, argTypes, BCRConfig.DEFAULT_PU_ALIGN);
      BGenDriver.setTypePuAttribute(funcType, TyPuFlag.TY_IS_VARARGS.toInt());
      methodSymIdx = BGenDriver.createExternFunctionSymbol("_Jv_NewMultiArray", funcType);
    }
    // void *_Jv_CheckCast(java.lang.Class *, java.lang.Object *)
    else if (method == JvMethod._Jv_CheckCast) {
      long retTypeIdx = TypeHandler.getInternal(GCJType.V_PTR);
      long[] argTypes = {
        TypeHandler.getInternal(GCJType.JAVA_LANG_CLASS_PTR),
        TypeHandler.getInternal(GCJType.JAVA_LANG_OBJECT_PTR)
      };
      long funcType = BGenDriver.createFunctionType(retTypeIdx, argTypes, BCRConfig.DEFAULT_PU_ALIGN);
      methodSymIdx = BGenDriver.createExternFunctionSymbol("_Jv_CheckCast", funcType);
    }
    else if (method == JvMethod._Jv_Throw) {
      long retTypeIdx = BGenDriver.getPrimitiveType(Mtype.MTYPE_V.toInt());
      long[] argTypes = {TypeHandler.getInternal(GCJType.JAVA_LANG_THROWABLE_PTR)};
      long funcType = BGenDriver.createFunctionType(retTypeIdx, argTypes, BCRConfig.DEFAULT_PU_ALIGN);
      methodSymIdx = BGenDriver.createExternFunctionSymbol("_Jv_Throw", funcType);
      if (B2WFrontEnd.i().isThrowNoReturn())
        BGenDriver.setPuNoReturn(methodSymIdx);
    }
    else if (method == JvMethod._Jv_MonitorEnter) {
      long retTypeIdx = BGenDriver.getPrimitiveType(Mtype.MTYPE_V.toInt());
      long[] argTypes = {TypeHandler.getInternal(GCJType.JAVA_LANG_OBJECT_PTR)};
      long funcType = BGenDriver.createFunctionType(retTypeIdx, argTypes, BCRConfig.DEFAULT_PU_ALIGN);
      methodSymIdx = BGenDriver.createExternFunctionSymbol("_Jv_MonitorEnter", funcType);
    }
    else if (method == JvMethod._Jv_MonitorExit) {
      long retTypeIdx = BGenDriver.getPrimitiveType(Mtype.MTYPE_V.toInt());
      long[] argTypes = {TypeHandler.getInternal(GCJType.JAVA_LANG_OBJECT_PTR)};
      long funcType = BGenDriver.createFunctionType(retTypeIdx, argTypes, BCRConfig.DEFAULT_PU_ALIGN);
      methodSymIdx = BGenDriver.createExternFunctionSymbol("_Jv_MonitorExit", funcType);
    } else if (method == JvMethod._Jv_IsInstanceOf) {
      long retTypeIdx =  BGenDriver.getPrimitiveType(Mtype.MTYPE_I4.toInt());
      long[] argTypes = {TypeHandler.getInternal(GCJType.JAVA_LANG_OBJECT_PTR), TypeHandler.getInternal(GCJType.JAVA_LANG_CLASS_PTR)};
      long funcType = BGenDriver.createFunctionType(retTypeIdx, argTypes, BCRConfig.DEFAULT_PU_ALIGN);
      methodSymIdx = BGenDriver.createExternFunctionSymbol("_Jv_IsInstanceOf", funcType);
    }
    // jvm.h
    // void _Jv_ThrowAbstractMethodError()
    else if (method == JvMethod._Jv_ThrowAbstractMethodError) {
      long retTypeIdx = TypeHandler.getType(VoidType.v());
      long[] argTypes = {};
      long funcType = BGenDriver.createFunctionType(retTypeIdx, argTypes, BCRConfig.DEFAULT_PU_ALIGN);
      methodSymIdx = BGenDriver.createExternFunctionSymbol("_Jv_ThrowAbstractMethodError", funcType);
    }
    else {
      assertThat(false).as( "Not support jv method.").isTrue();
      methodSymIdx = 0;
    }
    rtCache.put(method, methodSymIdx);
    return methodSymIdx;
  }

  // ========================================= Class Symbol ================================================
  private static Map<SootClass, ClassInfo> classInfoCache = new HashMap<>();
  private static Map<Constant, Long> clazzConstSymCache = new HashMap<>();
  private static Map<ArrayType, Long>  clazzArraySymCache = new HashMap<>();
  private static Map<String, Long> stringConstCache = new HashMap<>();
  private static Map<Pair<SootClass, SymbolCategory>, Long> internalSymbolCache = new HashMap<>();
  private static Map<String, Long> methodNCodeSymCache = new HashMap<>();

  /*********************************************************************************
   * Get Internal Symbol,
   * Default = SCLASS_EXTERN (to be linked from GCJ runtime)
   * If this symbol should be initialized then SCLASS should be UGLOBAL here.
   *   (and change to DGLOBAL once finishing the initialization)
   * @param clazz Class to be dealt with
   * @param cate  Category of the desired symbol
   * @return return symbol idx
   **********************************************************************************/
  static long getInternal(SootClass clazz, SymbolCategory cate) {
    Pair<SootClass, SymbolCategory> key = new Pair<>(clazz, cate);
    if(internalSymbolCache.containsKey(key)) {
      return internalSymbolCache.get(key);
    }
    long symIdx = 0;
    ClassInfo info = getClassInfo(clazz);
    switch(cate) {
      case CLASS_SYMBOL: {
        long typeIdx = TypeHandler.getInternal(GCJType.JAVA_LANG_CLASS);
        symIdx = BGenDriver.createVarSymbol(
          MangleTool.mangleForClassInternalSymbol(clazz.getName(), SymbolCategory.CLASS_SYMBOL),
          typeIdx, SClass.SCLASS_EXTERN.toInt(),
          SExport.EXPORT_PREEMPTIBLE.toInt(), DefaultValue.LEVEL
        );
        BGenDriver.setClassSymbol(symIdx, TypeHandler.getType(clazz));
        break;
      }
      case VTABLE: {
        assertThat(!clazz.isInterface()).as( "Interface don't need to add vtable, interface : " + clazz).isTrue();
        long elementType = TypeHandler.getInternal(GCJType.V_PTR);
        long vtableType = TypeHandler.getSubTypeArray(elementType, info.getVtable().size() + 4, BCRConfig.ALIGN);
        symIdx = BGenDriver.createVarSymbol(
          MangleTool.mangleForClassInternalSymbol(clazz.getName(), SymbolCategory.VTABLE), vtableType,
          SClass.SCLASS_EXTERN.toInt(), SExport.EXPORT_PREEMPTIBLE.toInt(), 1
        );
        BGenDriver.setVTable(symIdx, TypeHandler.getType(clazz));
        break;
      }
      case METHOD_TABLE: {
        long eltType =  TypeHandler.getInternal(GCJType.GCJ_METHOD);
        long arrType = TypeHandler.getSubTypeArray(eltType, info.getMethodTableSize(), BCRConfig.ALIGN);
        symIdx = BGenDriver.createVarSymbol(
          MangleTool.mangleForClassInternalSymbol(clazz.getName(), SymbolCategory.METHOD_TABLE),
          arrType, SClass.SCLASS_EXTERN.toInt(),
          SExport.EXPORT_PREEMPTIBLE.toInt(), DefaultValue.LEVEL
        );
        break;
      }
      case FIELD_TABLE: {
        long eltType =  TypeHandler.getInternal(GCJType.GCJ_FIELD);
        long arrType = TypeHandler.getSubTypeArray(eltType, info.getFieldCount(), BCRConfig.ALIGN);
        symIdx = BGenDriver.createVarSymbol(
          MangleTool.mangleForClassInternalSymbol(clazz.getName(), SymbolCategory.FIELD_TABLE),
          arrType, SClass.SCLASS_EXTERN.toInt(),
          SExport.EXPORT_PREEMPTIBLE.toInt(), DefaultValue.LEVEL
        );
        break;
      }
      case CATCHES_TABLE: {
        long eltType =  TypeHandler.getInternal(GCJType.GCJ_CATCH);
        long arrType = TypeHandler.getSubTypeArray(eltType, getClassInfo(clazz).getCatchTableSize(), BCRConfig.ALIGN);
        symIdx = BGenDriver.createVarSymbol(
          MangleTool.mangleForClassInternalSymbol(clazz.getName(), SymbolCategory.CATCHES_TABLE),
          arrType, SClass.SCLASS_EXTERN.toInt(),
          SExport.EXPORT_PREEMPTIBLE.toInt(), DefaultValue.LEVEL
        );
        break;
      }
      case GCJ_CONSTANTS_DATA: {
        ClassInfo cls = getClassInfo(clazz);
        long eltType =  TypeHandler.getInternal(GCJType.V_PTR);
        long arrType = TypeHandler.getSubTypeArray(eltType, cls.getConstantSize(), BCRConfig.ALIGN);
        symIdx = getInternal(clazz, SymbolCategory.GCJ_CONSTANTS_DATA_BASE);
        BGenDriver.setMiscFlags(symIdx, FlagKind.FLAG_ST_TYPE.toInt(),
                arrType);
        BGenDriver.setMiscFlags(symIdx, WhirlConstants.FlagKind.FLAG_SET_STFLAG_EXT.toInt(),
          WhirlConstants.STFlagsExt.ST_IS_CLASS_CONST_DATA.toInt());
        BGenDriver.setMiscFlags(symIdx, WhirlConstants.FlagKind.FLAG_SET_STFLAG.toInt(),
          WhirlConstants.STFlags.ST_IS_CONST_VAR.toInt());
        break;
      }
      case GCJ_CONSTANTS_DATA_BASE: {
        long vptr = TypeHandler.getInternal(GCJType.V_PTR);
        symIdx = BGenDriver.createVarSymbol(
                MangleTool.mangleForClassInternalSymbol(clazz.getName(), SymbolCategory.GCJ_CONSTANTS_DATA),
                vptr, SClass.SCLASS_EXTERN.toInt(),
                SExport.EXPORT_PREEMPTIBLE.toInt(), DefaultValue.LEVEL
        );
        BGenDriver.setMiscFlags(symIdx, WhirlConstants.FlagKind.FLAG_SET_STFLAG.toInt(),
          WhirlConstants.STFlags.ST_IS_CONST_VAR.toInt());
        break;
      }
      case GCJ_CONSTANTS_TAGS: {
        long eltType = BGenDriver.getPrimitiveType(Mtype.MTYPE_U1.toInt());
        long arrType = TypeHandler.getSubTypeArray(eltType, getClassInfo(clazz).getConstantSize(), (int) BGenDriver.getTySize(eltType));
        symIdx = getInternal(clazz, SymbolCategory.GCJ_CONSTANTS_TAGS_BASE);
        BGenDriver.setMiscFlags(symIdx, FlagKind.FLAG_ST_TYPE.toInt(),
                arrType);
        break;
      }
      case GCJ_CONSTANTS_TAGS_BASE: {
        long vptr = TypeHandler.getInternal(GCJType.V_PTR);
        symIdx = BGenDriver.createVarSymbol(
                MangleTool.mangleForClassInternalSymbol(clazz.getName(), SymbolCategory.GCJ_CONSTANTS_TAGS),
                vptr, SClass.SCLASS_EXTERN.toInt(),
                SExport.EXPORT_PREEMPTIBLE.toInt(), DefaultValue.LEVEL
        );
        break;
      }
      case INTERFACE_TABLE: {
        long eltType =  TypeHandler.getInternal(GCJType.JAVA_LANG_CLASS_PTR);
        long arrType = TypeHandler.getSubTypeArray(eltType, clazz.getInterfaceCount(), BCRConfig.ALIGN);
        symIdx = BGenDriver.createVarSymbol(
          MangleTool.mangleForClassInternalSymbol(clazz.getName(), SymbolCategory.INTERFACE_TABLE),
          arrType, SClass.SCLASS_EXTERN.toInt(),
          SExport.EXPORT_PREEMPTIBLE.toInt(), DefaultValue.LEVEL
        );
        break;
      }
      case CLASS_SYMBOL_PTR: {
        long eltType =  TypeHandler.getInternal(GCJType.JAVA_LANG_CLASS_PTR);
        long arrType = TypeHandler.getSubTypeArray(eltType, 1, BCRConfig.ALIGN); //TODO: Verify if only one is possible
        symIdx = BGenDriver.createVarSymbol(
                MangleTool.mangleForClassInternalSymbol(clazz.getName(), SymbolCategory.CLASS_SYMBOL_PTR),
                arrType, SClass.SCLASS_EXTERN.toInt(),
                SExport.EXPORT_PREEMPTIBLE.toInt(), DefaultValue.LEVEL
        );
        break;
      }
      case GCJ_JCR_CLASS_PTR: {
        long eltType =  TypeHandler.getInternal(GCJType.JAVA_LANG_CLASS_PTR);
        long arrType = TypeHandler.getSubTypeArray(eltType, 1, BCRConfig.ALIGN); //TODO: Verify if only one is possible
        symIdx = BGenDriver.createVarSymbol(
                MangleTool.mangleForClassInternalSymbol(clazz.getName(), SymbolCategory.GCJ_JCR_CLASS_PTR),
                arrType, SClass.SCLASS_EXTERN.toInt(),
                SExport.EXPORT_PREEMPTIBLE.toInt(), DefaultValue.LEVEL
        );
        break;
      }
      default: {
        assertThat(false).as( "Not support symbol category : " + cate).isTrue();
        break;
      }
    }
    assertThat(symIdx).as("getInternal must not return zero").isNotZero();
    internalSymbolCache.put(key, symIdx);
    return symIdx;
  }

  static long getMethodThrowsTable(SootMethod method) {
    if (throwsCatch.containsKey(method)) {
      return throwsCatch.get(method);
    }
    int throwsCount = method.getExceptions().size();
    long tyIdx = TypeHandler.getMethodThrowsTable(throwsCount + 1); // added one was for GCJ
    long symIdx = BGenDriver.createVarSymbol(MangleTool.mangleForMethodThrowTable(method),
            tyIdx, SClass.SCLASS_EXTERN.toInt(),
            SExport.EXPORT_PREEMPTIBLE.toInt(), DefaultValue.LEVEL
    );
    return symIdx;
  }

  static int getUtf8StringSize(String val) {
    if(BCRConfig.USE_JAVA_CHARSET_UTF8){
      return val.getBytes(Charset.forName("UTF-8")).length + 1;
    }
    return BGenDriver.getStringUTF8Length(val) + 1;
  }

  static long getStringConstSymbol(String str) {
    // string can be empty
    // assertThat(str.length() > 0 ).as( "getStringConstSymbol [size of string-to-initialize is 0]").isTrue();
    if(stringConstCache.containsKey(str)) {
      return stringConstCache.get(str);
    }
    long typeIdx = TypeHandler.getUtfStringType(getUtf8StringSize(str));
    long symIdx = BGenDriver.createVarSymbol(
      MangleTool.mangleForUTF(str), typeIdx, SClass.SCLASS_EXTERN.toInt(),
      SExport.EXPORT_PREEMPTIBLE.toInt(), DefaultValue.LEVEL
    );
    stringConstCache.put(str, symIdx);
    return symIdx;
  }


  public static long getByteArray(int length) {
    long typeIdx = TypeHandler.getSubTypeArray(BGenDriver.getPrimitiveType(Mtype.MTYPE_U1.toInt()), length, 1);
    long symIdx = BGenDriver.createVarSymbol(
            MangleTool.mangleForUTF(UTF_DUMMY_INDENTIFIER), typeIdx, SClass.SCLASS_EXTERN.toInt(),
            SExport.EXPORT_PREEMPTIBLE.toInt(), DefaultValue.LEVEL
    );
    return symIdx;
  }

  static long getArrayInstanceSymbol(ArrayType type) {

    if(clazzArraySymCache.containsKey(type)) {
      return clazzArraySymCache.get(type);
    } else {
      String name = MangleTool.mangleForClassInternalSymbol(type.toString(), SymbolCategory.CLASS_SYMBOL);
      long typeIdx = TypeHandler.getType(type);
      long ArrayClassSymbol = BGenDriver.createVarSymbol(
        name,
        typeIdx,
        SClass.SCLASS_EXTERN.toInt(),
        SExport.EXPORT_PREEMPTIBLE.toInt(),
        DefaultValue.LEVEL);
      clazzArraySymCache.put(type, ArrayClassSymbol);
      return ArrayClassSymbol;
    }
  }

  static long getEHHandlerSymbol(int exceptionNum){
      long typeIdx = TypeHandler.getEHRegionSupp(exceptionNum);
      long symIdx = BGenDriver.createVarSymbol("dummy.eh.supp." + (tempLocalCount++), typeIdx,
              SClass.SCLASS_EH_REGION_SUPP.toInt(), SExport.EXPORT_LOCAL.toInt(), 2);
      return symIdx;
  }

  static long getTempSymbol(long typeIdx) {
    long symbolIdx = -1;
    symbolIdx = BGenDriver.createVarSymbol(
            ".saved_temp."+tempLocalCount, typeIdx,
            SClass.SCLASS_AUTO.toInt(),
            SExport.EXPORT_LOCAL.toInt(), 2
    );
    tempLocalCache.put(tempLocalCount, symbolIdx);
    tempLocalCount++;
    return symbolIdx;
  }

  static long getClassInstanceSymbol(SootClass clazz) {
    return getInternal(clazz, SymbolCategory.CLASS_SYMBOL);
  }

  static long getVTableSymbol(SootClass clazz) {
    return getInternal(clazz, SymbolCategory.VTABLE);
  }

  static long getMethodNCodeSymbol(SootMethod method) {
    return getSymbol(method);
  }

  static ClassInfo getClassInfo(SootClass clazz) {
    if(classInfoCache.containsKey(clazz)) {
      return classInfoCache.get(clazz);
    }
    ClassInfo info = new ClassInfo(clazz);
    classInfoCache.put(clazz, info);
    return info;
  }

  static Map<String, Long> primitiveClassSymCache = new HashMap<>();

  public static long getPrimitiveClassSymbol(Type ty) {
    assertThat(ty.toString()).isNotNull();
    if(primitiveClassSymCache.containsKey(ty.toString())) {
      return primitiveClassSymCache.get(ty.toString());
    }
    long clazzTy = TypeHandler.getInternal(GCJType.JAVA_LANG_CLASS);
    String symName = MangleTool.mangleForPrimitiveGCJType(ty.toString());
    long sym = BGenDriver.createVarSymbol(symName, clazzTy,
            SClass.SCLASS_EXTERN.toInt(),
            SExport.EXPORT_PREEMPTIBLE.toInt(), 1);

    BGenDriver.setMiscFlags(sym, WhirlConstants.FlagKind.FLAG_SET_STFLAG.toInt(), WhirlConstants.STFlags.ST_ADDR_PASSED.toInt());

    primitiveClassSymCache.put(ty.toString(), sym);
    return sym;
  }

  /***
   * This mimic the abstract data structure stored in the class symbol,
   * i.e. Method table (MT), Field table (FL),
   * or internal data structure like V-Table (_ZTV)
   * @param <T> the element item type to be stored
   */
  abstract static class TableInfo<T> {
    List<T> table;
    Map<String, Integer> nameToTableIndex;

    TableInfo() {
      table = new ArrayList<>();
      nameToTableIndex = new HashMap<>();
    }

    void add(TableInfo<T> info) {
      table.addAll(info.table);
      nameToTableIndex.putAll(info.nameToTableIndex);
    }

    // if override, find same method will update table
    void addToTable(T item, boolean override) {
      String mangleName = transformToString(item);
      if(nameToTableIndex.containsKey(mangleName)) {
        if(override) {
          int index = nameToTableIndex.get(mangleName);
          table.set(index, item);
        }
      } else {
        table.add(item);
        nameToTableIndex.put(mangleName, table.size() - 1);
      }
    }

    abstract String transformToString(T item);

    abstract int getOffset(T item);

    public String toString() {
      String[] funStrArr = new String[table.size()];
      for(int i = 0; i < table.size(); i++) {
        funStrArr[i] = table.get(i).toString();
      }
      return Arrays.toString(funStrArr);
    }

    int size() {
      return table.size();
    }

    T get(int i) {
      return table.get(i);
    }

    int getListIndex(T item) {
      String mangleName = transformToString(item);
      return nameToTableIndex.get(mangleName);
    }
  }

  static abstract class SootMethodTableInfo extends TableInfo<SootMethod> {
    @Override
    String transformToString(SootMethod method) {
      return MangleTool.mangleForMethodOverride(method.getName(), method.getParameterTypes(), method.getReturnType());
    }
  }

  /***
   * Used for storing the V-Table info
   */
  static class VTableInfo extends SootMethodTableInfo {
    @Override
    int getOffset(SootMethod method) {
      String mangleName = transformToString(method);
      if(nameToTableIndex.containsKey(mangleName)) {
        return nameToTableIndex.get(mangleName) + 2;
      }
      // if can't find in vtable, return -1
      // method table need to get every method that this class defined
      // some of them don't in vtable, like constructor, static method, private method etc
      return -1;
    }
  }

  /***
   * Used for storing the Method Table info (MT), in class symbols (_ZNXXXX6class$)
   */
  static class MTableInfo extends SootMethodTableInfo {
    @Override
    int getOffset(SootMethod method) {
      String mangleName = transformToString(method);
      assertThat(nameToTableIndex.containsKey(mangleName)).as("Can't find method in table, method : " + method).isTrue();
      return nameToTableIndex.get(mangleName) + 1;
    }
  }


  /**
   * Used for storing the field info
   */
  static class FieldInfo extends TableInfo<SootField> {

    @Override
    String transformToString(SootField field) {
      return field.getSignature();
    }

    @Override
    int getOffset(SootField field) {
      String mangleName = transformToString(field);
      assertThat(nameToTableIndex.containsKey(mangleName)).as("Can't find field in table, field : " + field).isTrue();
      return nameToTableIndex.get(mangleName) + 1;
    }
  }

  /**
   * Annotation table used for storing and preparing the annotation info
   */
  static class AnnotationTable {
    public static Logger logger = LogManager.getLogger(AnnotationTable.class);
    List<Byte> byteAllBuf = new ArrayList<>();
    ClassInfo classInfo;
    public AnnotationTable(ClassInfo classInfo) {
      this.classInfo = classInfo;
    }

    /**
     * Adding a annotation to the annitation table
     * @param a
     * @param clazz
     * @param level
     * @param memberIndex
     */
    public void add(Tag a, SootClass clazz, Level level, int memberIndex) {
      // Visit the element value pairs
      List<Byte> annotationDetailList = new ArrayList<>();
      // Add attribute level info
      if (a instanceof VisibilityParameterAnnotationTag) {
        VisibilityParameterAnnotationTag vpat = (VisibilityParameterAnnotationTag) a;
        addUint8(annotationDetailList, (byte) JvAttrKind.JV_PARAMETER_ANNOTATIONS_KIND.val); // annotation kind
        if (level != Level.CLAZZ) {
          addUint16(annotationDetailList, memberIndex); // member index
        }
        int numParameters = classInfo.getMethodTable().get(memberIndex).getParameterCount();
        assertThat(vpat.getVisibilityAnnotations().size() == numParameters)
          .as("The parm annotation should have the parameter_annotations of same length to the param count").isTrue();
        addUint8(annotationDetailList, (byte) classInfo.getMethodTable().get(memberIndex).getParameterCount()); // num_parameters
        visitVisibilityParameterAnnotationTag(vpat, numParameters, annotationDetailList);
      } else if (a instanceof VisibilityAnnotationTag) {
        addUint8(annotationDetailList, (byte) JvAttrKind.JV_ANNOTATIONS_KIND.val); // annotation kind
        if (level != Level.CLAZZ) {
          addUint16(annotationDetailList, memberIndex); // member index
        }
        visitVisibilityAnnotationTag(((VisibilityAnnotationTag) a), annotationDetailList);
      } else {
        assertThat(false).as("Not considered tag met: " + a.toString()).isTrue();
      }

      // add JV_ATTR_TYPE
      if (level == Level.CLAZZ) {
        addUint8(byteAllBuf, (byte) JvAttrType.JV_CLASS_ATTR.val);
      } else if (level == Level.METHOD) {
        addUint8(byteAllBuf, (byte) JvAttrType.JV_METHOD_ATTR.val);
      } else if (level == Level.FIELD) {
        addUint8(byteAllBuf, (byte) JvAttrType.JV_FIELD_ATTR.val);
      } else if (level == Level.CODE) {
        addUint8(byteAllBuf, (byte) JvAttrType.JV_CODE_ATTR.val);
      }
      addUint32(byteAllBuf, annotationDetailList.size()); // data length
      logger.debug("Appending to Level: {}, ATTR values list : (len = {}) {}", level, annotationDetailList.size(), annotationDetailList);
      byteAllBuf.addAll(annotationDetailList);
    }

    private void visitVisibilityParameterAnnotationTag(VisibilityParameterAnnotationTag vpat,
                                                       int numParameters, List<Byte> resultList) {
      // Adding the array
      ArrayList<VisibilityAnnotationTag> annos = vpat.getVisibilityAnnotations();
      if (annos != null) {
        logger.debug("Visiting a param annotation, annos size: {}", annos.size());
        for (int i = 0; i < annos.size(); i++) {
          VisibilityAnnotationTag va = annos.get(i);
          List<Byte> oneParamAnnos = new ArrayList<>();
          logger.debug("Visiting a param {}", i);
          visitVisibilityAnnotationTag(va, oneParamAnnos);
          logger.debug("Visiting param {}, result (len = {}) = {}", i, oneParamAnnos.size(), oneParamAnnos);
          resultList.addAll(oneParamAnnos);
        }
      } else {
        // if RuntimeVisiableParameterAnnotations is null, add 0 for each parameter
        for (int i = 0; i < numParameters; i++) {
          addUint16(resultList, 0);
        }
      }
    }

    /**
     * Visiting an element value pair
     * @param vat
     * @param resultList
     */
    private void visitVisibilityAnnotationTag(VisibilityAnnotationTag vat, List<Byte> resultList) {
      if (vat == null) {
        addUint16(resultList, 0);
        return;
      }
      ArrayList<AnnotationTag>  annotations = vat.getAnnotations();
      int countOfAnnotations = annotations != null ? annotations.size() : 0;
      logger.debug("Annotations size : " + countOfAnnotations);
      addUint16(resultList, countOfAnnotations);
      // If there are no annotations (i.e. size == 0, we still would need this unit16 for back-end to parse)
      for (AnnotationTag anno: annotations) {
        logger.debug("Visiting annotation : " + anno.toString());
        visitAnnotationTag(anno, resultList);
      }
    }

    /**
     * Visit the annotation data
     * @param anno
     * @param resultList
     */
    private void visitAnnotationTag(AnnotationTag anno, List<Byte> resultList) {
      addUint16(resultList, getConstantFromPool(anno.getType())); // type_idx
      Collection<AnnotationElem> elems = anno.getElems();
      int num_elem_value_pairs = elems.size();
      logger.debug("Visiting annotation type = "+ anno.getType() +", name = "+ anno.getName() +" with element value pairs, count = " + num_elem_value_pairs);
      addUint16(resultList, elems.size()); // num_element_value_pairs
      for (AnnotationElem elemValuePair: elems) {
        visitElementValuePair(elemValuePair, resultList);
      }
    }

    /**
     * String put into the constant table
     * @param val
     * @return
     */
    private int getConstantFromPool(String val) {
      long symIdx = SymbolInitializer.initStringConst(val);
      int constantId = classInfo.addConstant(JvConstant.getConstVal(JvConstantType.JV_CONSTANT_Utf8, symIdx));
      logger.debug("Put into constant : ["+ val +"] : Constant Index: " + constantId + ", symIdx: " + symIdx);
      return constantId;
    }

    /***
     * Int constant put into the constant table
     * @param value
     * @return
     */
    private int getConstantFromPool(JvConstantType type, long value) {
      int constantId = classInfo.addConstant(JvConstant.getConstVal(type, value));
      logger.debug("Put into constant, type: " + type + ", value: " + value + ", index: " + constantId);
      return constantId;
    }

    /**
     * Visiting a element value (this may be invoked recursively)
     * @param elemValue
     * @param resultList
     */
    private void visitElementValue(AnnotationElem elemValue, List<Byte> resultList) {
      addUint8(resultList, (byte) elemValue.getKind()); // tag
      if (elemValue instanceof AnnotationBooleanElem) {
        int constantId = getConstantFromPool(JvConstantType.JV_CONSTANT_Utf8,
          ((AnnotationBooleanElem) elemValue).getValue() ? 1l : 0l);
        addUint16(resultList, constantId);
      } else if (elemValue instanceof AnnotationDoubleElem) {
        int constantId = getConstantFromPool(JvConstantType.JV_CONSTANT_Double,
          Double.doubleToRawLongBits(((AnnotationDoubleElem) elemValue).getValue())); // double_const_index
        addUint16(resultList, constantId);
      } else if (elemValue instanceof AnnotationFloatElem) {
        int constantId = getConstantFromPool(JvConstantType.JV_CONSTANT_Float,
          Float.floatToRawIntBits(((AnnotationFloatElem) elemValue).getValue())); // float_const_index
        addUint16(resultList, constantId);
      } else if (elemValue instanceof AnnotationIntElem) {
        int constantId = getConstantFromPool(JvConstantType.JV_CONSTANT_Integer,
          ((AnnotationIntElem) elemValue).getValue()); // int_const_index
        addUint16(resultList, constantId);
      } else if (elemValue instanceof AnnotationLongElem) {
        int constantId = getConstantFromPool(JvConstantType.JV_CONSTANT_Long,
          ((AnnotationLongElem) elemValue).getValue()); // long_const_index
        addUint16(resultList, constantId);
      } else if (elemValue instanceof AnnotationStringElem) {
        int constantId = getConstantFromPool(((AnnotationStringElem) elemValue).getValue()); // string_const_index
        addUint16(resultList, constantId);
      } else if (elemValue instanceof AnnotationAnnotationElem) {
        visitAnnotationTag(((AnnotationAnnotationElem) elemValue).getValue(), resultList);
      } else if (elemValue instanceof AnnotationEnumElem) {
        addUint16(resultList, getConstantFromPool(((AnnotationEnumElem) elemValue).getTypeName()));
        addUint16(resultList, getConstantFromPool(((AnnotationEnumElem) elemValue).getConstantName()));
      } else if (elemValue instanceof AnnotationClassElem) {
        addUint16(resultList, getConstantFromPool(((AnnotationClassElem) elemValue).getDesc()));
      } else if (elemValue instanceof AnnotationArrayElem) {
        ArrayList<AnnotationElem> values = ((AnnotationArrayElem) elemValue).getValues();
        int elementLength = values.size();
        addUint16(resultList, elementLength);
        logger.debug("Visiting sub-elements, count = " + elementLength);
        for (AnnotationElem subElem: values) {
          visitElementValue(subElem, resultList);
        }
      } else {
        assertThat(false).as("Element value kind not implemented kind:" + elemValue.getKind() + ", type: " + elemValue.toString()).isTrue();
      }
    }

    /**
     * Visiting a element value pair
     * @param elemValuePair (AnnotationElem) from soot data structure
     * @param resultList
     */
    private void visitElementValuePair(AnnotationElem elemValuePair, List<Byte> resultList) {
      logger.debug(String.format("Visiting element-value pair tag:\"%c\", name: %s", elemValuePair.getKind(), elemValuePair.getName()));
      addUint16(resultList, getConstantFromPool(elemValuePair.getName())); // element_name_index
      visitElementValue(elemValuePair, resultList);
    }

    /***
     * Return the total space needed the store the reflect_data structure (i.e. a byte array)
     * @return
     */
    public int getAnnotationSize() {
      return byteAllBuf.size();
    }

    /**
     * Adding a uint number of length = 1 byte.1
     * @param bytebuf the buffer to add to
     * @param content
     * @return
     */
    public int addUint8(List<Byte> bytebuf, byte content) {
      int size = bytebuf.size();
      bytebuf.add(content);
      return size;
    }

    /**
     * Adding a uint number of length = 2 bytes, or 16 bits.
     * @param bytebuf the buffer to add to
     * @param content
     * @return
     */
    public int addUint16(List<Byte> bytebuf, int content) {
      int size = bytebuf.size();
      bytebuf.add((byte)((content >> 8) & 0xFF));
      bytebuf.add((byte)((content) & 0xFF));
      return size;
    }

    /**
     * Adding a uint number of length = 4 bytes, or 32 bits.
     * @param bytebuf the buffer to add to
     * @param content
     * @return
     */
    public int addUint32(List<Byte> bytebuf, long content) {
      int size = bytebuf.size();
      bytebuf.add((byte)((content >> 24) & 0xFF));
      bytebuf.add((byte)((content >> 16) & 0xFF));
      bytebuf.add((byte)((content >> 8) & 0xFF));
      bytebuf.add((byte)((content) & 0xFF));
      return size;
    }

    /**
     * This should be invoked after all related info has been added to the table,
     * and at the time to dump the info into the WHIRL file.
     * @return
     */
    public List<Byte> getAsByteArray() {
      assertThat(byteAllBuf).as("Buffer not should be none, this should be initialized during constructor").isNotNull();
      return byteAllBuf;
    }

    /**
     * Writing a finishing marker in the end of the buffer (JV_DONE_ATTR)
     */
    public void finishUp() {
      addUint8(getAsByteArray(), (byte) JvAttrType.JV_DONE_ATTR.val);
      addUint8(getAsByteArray(), (byte) 0);
    }

    /**
     * The level indicating which type of JV_ATTR_TYPE to use.
     */
    public enum Level {
      CLAZZ,
      FIELD,
      METHOD,
      CODE
    }
  }

  public static class ClassInfo {
    SootClass clazz;
    VTableInfo vTable;
    MTableInfo methodTable;
    AnnotationTable annotationTable;
    FieldInfo fieldTable;
    long size;
    private List<JvConstant> constants;
    private Map<Pair<JvConstantType, Long>, Integer> constantMap;

    public long getSize() {
      return size;
    }

    ClassInfo(SootClass clazz) {
      this.clazz = clazz;
      vTable = new VTableInfo();
      constants = new ArrayList<>();
      constantMap = new HashMap<>();
      if(clazz.hasSuperclass() && !clazz.getSuperclass().isInterface()) {
        vTable.add(getClassInfo(clazz.getSuperclass()).getVtable());
      }
      // if class is not interface, all methods that override interface must be implemented
      // already in vtable, needn't add to mtable
      if(!clazz.isInterface()) {
        addToVTable(clazz);
        methodTable = new MTableInfo();
        addToMethodTable(clazz);
      }
      // if class is interface, add it's method to mtable, then add it's super interface's method to mtable
      else {
        methodTable = new MTableInfo();
        addToMethodTable(clazz);
        for(SootClass c: clazz.getInterfaces()) {
          addToMethodTable(c);
        }
      }
      fieldTable = new FieldInfo();
      for (SootField f : clazz.getFields()) {
        fieldTable.addToTable(f, false);
      }
      BGenDriver.info("[ClassInfo] Starting to parse fields in " + clazz.getName() + "\n");
      size = BGenDriver.getTySize(TypeHandler.getType(clazz));
      // Annotation Size
      constants.add(JvConstant.getConstVal(JvConstantType.JV_CONSTANT_Undefined, 0));
      /**
       * Generating the annotation table for normal classes
       * */
      annotationTable = new AnnotationTable(this);
      if (!clazz.getName().equals("java.lang.Class")) {
        for (SootField field: clazz.getFields()) {
          for (Tag t: field.getTags()) {
            if (t instanceof VisibilityParameterAnnotationTag || t instanceof VisibilityAnnotationTag) {
              annotationTable.add(t, clazz, AnnotationTable.Level.FIELD, fieldTable.getListIndex(field));
            }
          }
        }
        for (SootMethod method: clazz.getMethods()) {
          for (Tag t: method.getTags()) {
            if (t instanceof VisibilityParameterAnnotationTag || t instanceof VisibilityAnnotationTag) {
              annotationTable.add(t, clazz, AnnotationTable.Level.METHOD, methodTable.getListIndex(method));
            }
          }
        }
        for (Tag a: clazz.getTags()) {
          if (a instanceof VisibilityAnnotationTag) {
            annotationTable.add(a, clazz, AnnotationTable.Level.CLAZZ, 0);
          }
          // We're not handling direct AnnotationTag here
          assertThat(a instanceof AnnotationTag).isFalse();
        }
        annotationTable.finishUp();
      }
      logger.info("[ClassInfo] finishing : " + clazz.getName() + "'s size is " + size + "\n");
    }

    TableInfo<SootMethod> getVtable() {
      assertThat(!clazz.isInterface()).as( "Interface don't have vtable, Interface : " + clazz).isTrue();
      return vTable;
    }

    TableInfo<SootMethod> getMethodTable() {
      return methodTable;
    }


    private void addToVTable(SootClass clazz) {
      List<SootMethod> methods = clazz.getMethods();
      boolean ignoreFinal = clazz.getName().equals(JvName.LANG_OBJECT);
      for (SootMethod method: methods) {
        // private method is not in vtable
        if (method.isStatic() || method.isConstructor() || method.isPrivate()) {
          continue;
        }
        if (ignoreFinal && method.isFinal()) {
          continue;
        }
        vTable.addToTable(method, true);
      }
      // abstract class need to append not implemented method to vtable
      // abstract method should occupy position in vtable, symbol init will ignore symbol, fill up 0
      if (clazz.isAbstract()) {
        for (SootClass i : clazz.getInterfaces()) {
          for (SootMethod m : i.getMethods()) {
            vTable.addToTable(m, false);
          }
        }
      }
    }

    private void addToMethodTable(SootClass clazz) {
      for(SootMethod method: clazz.getMethods()) {
        methodTable.addToTable(method, false);
      }
    }

    int getVTableOffset(SootMethod method) {
      return vTable.getOffset(method);
    }

    int getMethodTableOffset(SootMethod method) {
      return methodTable.getOffset(method);
    }

    public String toString() {
      return "VTable : " + vTable.toString() + "\nMethodTable : " + methodTable.toString();
    }

    int getFieldCount() {
      return clazz.getFieldCount();
    }

    int getStaticFieldCount() {
      int count = 0;
      for(SootField field: clazz.getFields()) {
        if(field.isStatic()) {
          count++;
        }
      }
      return count;
    }

    private int getMethodTableSize() {
      return methodTable.size();
    }

    int getConstantSize() {
      return constants.size();
    }

    int getCatchTableSize() {
      return 3; //**TODO: Get Catch classes count !
    }

    List<JvConstant> getConstants() {
      return constants;
    }

    int addConstant(JvConstant consts) {
      int pos = constants.size();
      // If there is a constant in already in the list, if so, skip the generation.
      if (constantMap.containsKey(new Pair<>(consts.getType(), consts.getValue()))) {
        return constantMap.get(new Pair<>(consts.getType(), consts.getValue()));
      }
      constants.add(consts);
      constantMap.put(new Pair<>(consts.getType(), consts.getValue()), pos);
      return pos;
    }

    public int getAnnotationSize() {
      return annotationTable.getAnnotationSize();
    }

    public byte[] getAnnotationByteArray() {
      byte[] btValues = new byte[annotationTable.getAsByteArray().size()];
      int cursor = 0;
      for (Byte b:annotationTable.getAsByteArray()) {
        btValues[cursor++] = b;
      }
      return btValues;
    }
  }

  static class JvConstant {
    private JvConstantType type;
    private long value;

    private JvConstant(JvConstantType type, long value) {
      this.type = type;
      this.value = value;
    }

    JvConstantType getType() {
      return type;
    }

    long getValue() {
      return value;
    }

    static JvConstant getConstVal(JvConstantType type, long value) {
      return new JvConstant(type, value);
    }
  }
}
