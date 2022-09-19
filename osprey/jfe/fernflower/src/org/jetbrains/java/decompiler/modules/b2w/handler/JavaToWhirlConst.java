/*
 * Copyright (C) 2021 Xcalibyte (Shenzhen) Limited.
 */

package org.jetbrains.java.decompiler.modules.b2w.handler;

import org.jetbrains.java.decompiler.code.CodeConstants;

import java.util.Map;
import java.util.HashMap;

public class JavaToWhirlConst
{
  private final static Map<Integer, WhirlConstants.TyFlag> JTypeAccFlagToTYAttr = new HashMap<>();
  static {
    JTypeAccFlagToTYAttr.put(CodeConstants.ACC_FINAL, WhirlConstants.TyFlag.TY_CONST);
    JTypeAccFlagToTYAttr.put(CodeConstants.ACC_VOLATILE, WhirlConstants.TyFlag.TY_VOLATILE);
  }

  public static WhirlConstants.TyFlag getTyFlag(int flag) {
    assert JTypeAccFlagToTYAttr.containsKey(flag): "Not support operator : " + flag;
    return JTypeAccFlagToTYAttr.get(flag);
  }

  private final static Map<Integer, WhirlConstants.Mtype> JTypeToMType = new HashMap<>();
  static {
    JTypeToMType.put(CodeConstants.TYPE_BYTE, WhirlConstants.Mtype.MTYPE_I1);
    JTypeToMType.put(CodeConstants.TYPE_CHAR, WhirlConstants.Mtype.MTYPE_U2);
    JTypeToMType.put(CodeConstants.TYPE_DOUBLE, WhirlConstants.Mtype.MTYPE_F8);
    JTypeToMType.put(CodeConstants.TYPE_FLOAT, WhirlConstants.Mtype.MTYPE_F4);
    JTypeToMType.put(CodeConstants.TYPE_INT, WhirlConstants.Mtype.MTYPE_I4);
    JTypeToMType.put(CodeConstants.TYPE_LONG, WhirlConstants.Mtype.MTYPE_I8);
    JTypeToMType.put(CodeConstants.TYPE_SHORT, WhirlConstants.Mtype.MTYPE_I2);
    JTypeToMType.put(CodeConstants.TYPE_BOOLEAN, WhirlConstants.Mtype.MTYPE_B);
//    JTypeToMType.put(CodeConstants.TYPE_OBJECT, WhirlConstants.MTYPE_I8);
    JTypeToMType.put(CodeConstants.TYPE_BYTECHAR, WhirlConstants.Mtype.MTYPE_I1);
    JTypeToMType.put(CodeConstants.TYPE_SHORTCHAR, WhirlConstants.Mtype.MTYPE_I2);
  }

  public static int getMType(int type) {
    assert JTypeToMType.containsKey(type): "Not support type : " + type;
    return JTypeToMType.get(type).toInt();
  }

  private final static Map<Integer, WhirlConstants.Operator> JOperatorToOperator = new HashMap<>();
  static {
    JOperatorToOperator.put(0, WhirlConstants.Operator.OPR_ADD);
    JOperatorToOperator.put(1, WhirlConstants.Operator.OPR_SUB);
  }

  public static int getOperator(int operator) {
    assert JOperatorToOperator.containsKey(operator): "Not support operator : " + operator;
    return JOperatorToOperator.get(operator).toInt();
  }

}