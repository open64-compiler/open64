/*
 * Copyright (C) 2021 Xcalibyte (Shenzhen) Limited.
 */

package org.jetbrains.java.decompiler.modules.b2w.handler;

import org.jetbrains.java.decompiler.code.CodeConstants;
import org.jetbrains.java.decompiler.modules.bgen.BGenDriver;
import org.jetbrains.java.decompiler.modules.decompiler.exps.FieldExprent;
import org.jetbrains.java.decompiler.modules.decompiler.exps.VarExprent;
import org.jetbrains.java.decompiler.struct.StructField;
import org.jetbrains.java.decompiler.struct.StructMethod;
import org.jetbrains.java.decompiler.struct.gen.MethodDescriptor;
import org.jetbrains.java.decompiler.struct.gen.VarType;

import java.util.Map;
import java.util.HashMap;

public class TypeHandler {
  private static Map<String, Long> cache = new HashMap<>();

  private TypeHandler() {}

  public static WhirlConstants.TyFlag getTypeAttribute(StructField struct) {
    int accessFlag = struct.getAccessFlags();
    if((accessFlag & CodeConstants.ACC_VOLATILE) != 0) {
      return JavaToWhirlConst.getTyFlag(CodeConstants.ACC_VOLATILE);
    } else if((accessFlag & CodeConstants.ACC_FINAL) != 0) {
      return JavaToWhirlConst.getTyFlag(CodeConstants.ACC_FINAL);
    }
    return WhirlConstants.TyFlag.TY_ATTR_NULL;
  }

  public static long getType(StructMethod sm) {
    if(cache.containsKey(sm.getDescriptor())) {
      return cache.get(sm.getDescriptor());
    }
    MethodDescriptor md = MethodDescriptor.parseDescriptor(sm.getDescriptor());
    VarType[] varTypeArray = md.params;
    VarType retType = md.ret;
    long[] varTypeIdxArray = new long[varTypeArray.length];
    for(int i = 0; i < varTypeArray.length; i++) {
      // TODO: figure out the type attribute
      varTypeIdxArray[i] = getType(varTypeArray[i], WhirlConstants.TyFlag.TY_ATTR_NULL);
    }
    // TODO: figure out the type attribute
    long retTypeIdx = getType(retType, WhirlConstants.TyFlag.TY_ATTR_NULL);
    long typeIdx = BGenDriver.createFunctionType(retTypeIdx, varTypeIdxArray, 8);
    cache.put(sm.getDescriptor(), typeIdx);
    return typeIdx;
  }

  public static long getType(StructField struct) {
    VarType type = new VarType(struct.getDescriptor());
    WhirlConstants.TyFlag jAttr = getTypeAttribute(struct);
    return getType(type, jAttr);
  }

  public static long getType(VarExprent ve) {
    VarType type = ve.getVarType();
    // TODO: figure out the type attribute
    return getType(type, WhirlConstants.TyFlag.TY_ATTR_NULL);
  }

  public static long getType(FieldExprent fe) {
    // TODO: figure out the type attribute
    return getType(fe.getDescriptor().type, WhirlConstants.TyFlag.TY_ATTR_NULL);
  }


  public static long getType(VarType type, WhirlConstants.TyFlag Attribute) {
    long id = -1;
    String typeCacheKey = type.toString() + "+" + Attribute;
    if(cache.containsKey(typeCacheKey)) {
      return cache.get(typeCacheKey);
    }
    if(type.type <= CodeConstants.TYPE_BOOLEAN || type.type == CodeConstants.TYPE_VOID) {
      id = BGenDriver.getPrimitiveType(type.type, Attribute.toInt());
    } else {
      assert false: "not support type, descriptor : " + type.toString()  + " id : " + type.type;
    }
    cache.put(typeCacheKey, id);
    return id;
  }

  protected static String mangleVarType(VarType type) {
    if(type.type <= CodeConstants.TYPE_BOOLEAN || type.type == CodeConstants.TYPE_VOID) {
      return manglePrimitiveType(type);
    }
    assert false: "not support yet.";
    return null;
  }

  protected static String manglePrimitiveType(VarType type) {
    switch(type.type) {
      case CodeConstants.TYPE_BYTE:
        return "c";
      case CodeConstants.TYPE_CHAR:
        return "w";
      case CodeConstants.TYPE_DOUBLE:
        return "d";
      case CodeConstants.TYPE_FLOAT:
        return "f";
      case CodeConstants.TYPE_INT:
        return "i";
      case CodeConstants.TYPE_LONG:
        return "x";
      case CodeConstants.TYPE_SHORT:
        return "s";
      case CodeConstants.TYPE_BOOLEAN:
        return "b";
      case CodeConstants.TYPE_VOID:
        return "v";
      default:
        break;
    }
    assert false: "Not primitive type, type : " + type.getClass();
    return null;
  }

}
