/*
 * Copyright (C) 2021 Xcalibyte (Shenzhen) Limited.
 */

package org.jetbrains.java.decompiler.modules.b2w.handler;

import org.jetbrains.java.decompiler.main.ClassesProcessor;
import org.jetbrains.java.decompiler.main.DecompilerContext;
import org.jetbrains.java.decompiler.modules.bgen.BGenDriver;
import org.jetbrains.java.decompiler.modules.decompiler.exps.FieldExprent;
import org.jetbrains.java.decompiler.modules.decompiler.exps.VarExprent;
import org.jetbrains.java.decompiler.modules.decompiler.vars.VarProcessor;
import org.jetbrains.java.decompiler.struct.StructClass;
import org.jetbrains.java.decompiler.struct.StructField;
import org.jetbrains.java.decompiler.struct.StructMethod;

import java.util.HashMap;
import java.util.Map;

public class SymbolHandler {
  private static Map<String, Long> cache = new HashMap<>();

  private SymbolHandler() {}

  public static long getSymbol(StructMethod sm) {
    String mangledName = mangling(sm);
    if(cache.containsKey(mangledName)) {
      return cache.get(mangledName);
    }
    long symbolIdx = -1;
    long typeIdx = TypeHandler.getType(sm);
    symbolIdx = BGenDriver.createFunctionSymbol(
      sm.getName(), typeIdx,
      WhirlConstants.SClass.SCLASS_EXTERN.toInt(),
      WhirlConstants.SExport.EXPORT_PREEMPTIBLE.toInt(), 1
    );
    cache.put(mangledName, symbolIdx);
    return symbolIdx;
  }

  public static long getSymbol(StructField struct) {
    String mangledName = mangling(struct);
    if(cache.containsKey(mangledName)) {
      return cache.get(mangledName);
    }
    long symbolIdx = 0;
    long typeIdx = TypeHandler.getType(struct);
    WhirlConstants.SClass sClass = WhirlConstants.SClass.SCLASS_UGLOBAL;
    symbolIdx = BGenDriver.createVarSymbol(
      struct.getName(), typeIdx, sClass.toInt(), WhirlConstants.SExport.EXPORT_PREEMPTIBLE.toInt(), 1
    );
    cache.put(mangledName, symbolIdx);
    return symbolIdx;
  }

  public static long getSymbol(VarExprent ve) {
    String mangledName = mangling(ve);
    if(!ve.isDefinition()) {
      assert cache.containsKey(mangledName) :
        "VarExprent not definition, but not in symbol table, symbol name : " + getVarExprentName(ve);
      return cache.get(mangledName);
    }
    assert !cache.containsKey(mangledName) :
      "VarExprent is definition, but in symbol table, symbol name : " + getVarExprentName(ve);
    long typeIdx = TypeHandler.getType(ve);
    long symbolIdx = -1;
    symbolIdx = BGenDriver.createVarSymbol(
      getVarExprentName(ve), typeIdx,
      WhirlConstants.SClass.SCLASS_AUTO.toInt(),
      WhirlConstants.SExport.EXPORT_LOCAL.toInt(), 2
    );
    cache.put(mangledName, symbolIdx);
    return symbolIdx;
  }

  public static long getSymbol(FieldExprent fe) {
    String mangledName = mangling(fe);
    if(cache.containsKey(mangledName)) {
      return cache.get(mangledName);
    }
    assert false: "Other class member field, class : " + fe.getClassname() + ", name : " + fe.getName();
    return -1;
  }

  protected static String mangling(StructClass ss) {
    return ss.qualifiedName;
  }

  // 'M' + mangled class name length + mangled class name + method
  protected static String mangling(StructMethod sm) {
    StringBuilder mangledName = new StringBuilder();
    mangledName.append("M");
    String mangledClassName = mangling(sm.getClassStruct());
    mangledName.append(mangledClassName.length());
    mangledName.append(mangledClassName);
    String name = sm.getDescriptor() + sm.getName();
    mangledName.append(name.length());
    mangledName.append(name);
    return mangledName.toString();
  }

  // 'F' + mangled class name length + mangled class name + field name length + field name
  protected static String mangling(StructField struct) {
    StringBuilder mangledName = new StringBuilder();
    mangledName.append("F");
    ClassesProcessor.ClassNode node = (ClassesProcessor.ClassNode)DecompilerContext.getProperty(DecompilerContext.CURRENT_CLASS_NODE);
    if(node != null) {
      String mangledClassName = mangling(node.classStruct);
      mangledName.append(mangledClassName.length());
      mangledName.append(mangledClassName);
    }
    mangledName.append(struct.getName().length());
    mangledName.append(struct.getName());
    return mangledName.toString();
  }

  // 'V' + mangled method name length + mangled method name + name length + name
  protected static String mangling(VarExprent ve) {
    StringBuilder mangledName = new StringBuilder();
    mangledName.append("V");
    VarProcessor vp = ve.getProcessor();
    StructMethod method = vp.getStructMethod();
    String mangledMethodName = mangling(method);
    mangledName.append(mangledMethodName.length());
    mangledName.append(mangledMethodName);
    String name = getVarExprentName(ve);
    mangledName.append(name.length());
    mangledName.append(name);
    return mangledName.toString();
  }

  protected static String mangling(FieldExprent fe) {
    StringBuilder mangledName = new StringBuilder();
    mangledName.append("F");
    String clazzName = fe.getClassname();
    mangledName.append(clazzName.length());
    mangledName.append(clazzName);
    String name = fe.getName();
    mangledName.append(name.length());
    mangledName.append(name);
    return mangledName.toString();
  }

  private static String getVarExprentName(VarExprent ve) {
    String name = ve.getProcessor().getVarName(ve.getVarVersionPair());
    if(name == null) {
      name = "var" + ve.getIndex() + (ve.getVersion() == 0 ? "" : "_" + ve.getVersion());
    }
    return name;
  }

  public static String print() {
    return cache.toString();
  }

}
