/*
 * Copyright (C) 2021 Xcalibyte (Shenzhen) Limited.
 */

package io.xcalibyte;

import java.util.Map;
import java.util.HashMap;
import static org.assertj.core.api.Assertions.*;

import soot.*;
import soot.jimple.toolkits.typing.fast.BottomType;

class JavaToWhirlConst {
  private final static Map<Integer, WhirlConstants.TyAttr> JTypeAccFlagToTYAttr = new HashMap<>();

  static {
    JTypeAccFlagToTYAttr.put(Modifier.FINAL, WhirlConstants.TyAttr.TY_CONST);
    JTypeAccFlagToTYAttr.put(Modifier.VOLATILE, WhirlConstants.TyAttr.TY_VOLATILE);
  }

  static WhirlConstants.TyAttr getTyAttr(int flag) {
    assertThat(JTypeAccFlagToTYAttr.containsKey(flag) ).as( "Not support operator : " + flag).isTrue();
    return JTypeAccFlagToTYAttr.get(flag);
  }

  private final static Map<String, WhirlConstants.Mtype> JTypeToMType = new HashMap<>();

  static {
    JTypeToMType.put(SootConstants.TYPE_BYTE, WhirlConstants.Mtype.MTYPE_I1);
    JTypeToMType.put(SootConstants.TYPE_CHAR, WhirlConstants.Mtype.MTYPE_U2);
    JTypeToMType.put(SootConstants.TYPE_DOUBLE, WhirlConstants.Mtype.MTYPE_F8);
    JTypeToMType.put(SootConstants.TYPE_FLOAT, WhirlConstants.Mtype.MTYPE_F4);
    JTypeToMType.put(SootConstants.TYPE_INT, WhirlConstants.Mtype.MTYPE_I4);
    JTypeToMType.put(SootConstants.TYPE_LONG, WhirlConstants.Mtype.MTYPE_I8);
    JTypeToMType.put(SootConstants.TYPE_SHORT, WhirlConstants.Mtype.MTYPE_I2);
    // ATTENTION: boolean type in java should link to MTYPE_I4 to WHIRL
    // because backend limit, we should deal with boolean type carefully
    JTypeToMType.put(SootConstants.TYPE_BOOLEAN, WhirlConstants.Mtype.MTYPE_I1);
    JTypeToMType.put(SootConstants.TYPE_BYTECHAR, WhirlConstants.Mtype.MTYPE_I1);
    JTypeToMType.put(SootConstants.TYPE_SHORTCHAR, WhirlConstants.Mtype.MTYPE_I2);
    JTypeToMType.put(SootConstants.TYPE_VOID, WhirlConstants.Mtype.MTYPE_V);
  }

  static int getMType(Type type) {
    String typeString = type.toString();
    // if type is RefType, use default U8 for pointer type
    if (type instanceof RefType || type instanceof ArrayType || type instanceof NullType || type instanceof BottomType) {
      return BCRConfig.getPtrType();
    }
    assertThat(JTypeToMType.containsKey(typeString) ).as( "Not support type : " + type).isTrue();
    return JTypeToMType.get(typeString).toInt();
  }

  private final static Map<String, WhirlConstants.Operator> JOperatorToOperator = new HashMap<>();

  static {
    // WN_Binary
    JOperatorToOperator.put(" + ", WhirlConstants.Operator.OPR_ADD);        // JAddExpr
    JOperatorToOperator.put(" - ", WhirlConstants.Operator.OPR_SUB);        // JSubExpr
    JOperatorToOperator.put(" * ", WhirlConstants.Operator.OPR_MPY);        // JMulExpr
    JOperatorToOperator.put(" / ", WhirlConstants.Operator.OPR_DIV);        // JDivExpr
    JOperatorToOperator.put(" & ", WhirlConstants.Operator.OPR_BAND);       // JAndExpr
    JOperatorToOperator.put(" | ", WhirlConstants.Operator.OPR_BIOR);       // JOrExpr
    JOperatorToOperator.put(" >> ", WhirlConstants.Operator.OPR_LSHR);      // JShrExpr
    JOperatorToOperator.put(" >>> ", WhirlConstants.Operator.OPR_ASHR);     // JUShrExpr
    JOperatorToOperator.put(" << ", WhirlConstants.Operator.OPR_SHL);       // JShlExpr
    JOperatorToOperator.put(" ^ ", WhirlConstants.Operator.OPR_BXOR);       // JXorExpr
    JOperatorToOperator.put(" % ", WhirlConstants.Operator.OPR_REM);        // JRemExpr
    // WN_Relational
    JOperatorToOperator.put(" == ", WhirlConstants.Operator.OPR_EQ);        // JEqExpr
    JOperatorToOperator.put(" >= ", WhirlConstants.Operator.OPR_GE);        // JGeExpr
    JOperatorToOperator.put(" > ", WhirlConstants.Operator.OPR_GT);         // JGtExpr
    JOperatorToOperator.put(" <= ", WhirlConstants.Operator.OPR_LE);        // JLeExpr
    JOperatorToOperator.put(" < ", WhirlConstants.Operator.OPR_LT);         // JLtExpr
    JOperatorToOperator.put(" != ", WhirlConstants.Operator.OPR_NE);        // JNeExpr
    // WN_Unary
    JOperatorToOperator.put("neg", WhirlConstants.Operator.OPR_NEG);      // JNegExpr
  }

  static int getOperator(String operator) {
    assertThat(JOperatorToOperator.containsKey(operator)).as( "Not support operator : " + operator).isTrue();
    return JOperatorToOperator.get(operator).toInt();
  }

}