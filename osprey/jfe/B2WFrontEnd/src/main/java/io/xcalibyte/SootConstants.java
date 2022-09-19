/*
 * Copyright (C) 2021 Xcalibyte (Shenzhen) Limited.
 */

package io.xcalibyte;

import soot.Type;
import static org.assertj.core.api.Assertions.*;

class SootConstants {

  final static String TYPE_BYTE = "byte";
  final static String TYPE_CHAR = "char";
  final static String TYPE_DOUBLE = "double";
  final static String TYPE_FLOAT = "float";
  final static String TYPE_INT = "int";
  final static String TYPE_LONG = "long";
  final static String TYPE_SHORT = "short";
  final static String TYPE_BOOLEAN = "boolean";
  final static String TYPE_BYTECHAR = "bytechar";
  final static String TYPE_SHORTCHAR = "shortchar";
  final static String TYPE_VOID = "void";

  final static String CLASS_OBJECT = "java.lang.Object";
  final static String CLASS_CLASS = "java.lang.Class";
  final static String CLASS_THROWABLE = "java.lang.Throwable";


  static long getJTypeSize(Type t, long align) {
    if (JavaToWhirlConst.getMType(t) == WhirlConstants.Mtype.MTYPE_I1.toInt()
   || JavaToWhirlConst.getMType(t) == WhirlConstants.Mtype.MTYPE_U1.toInt()) {
      return 1 <= align ? align : 1;
    }else if (JavaToWhirlConst.getMType(t) == WhirlConstants.Mtype.MTYPE_I2.toInt()
        || JavaToWhirlConst.getMType(t) == WhirlConstants.Mtype.MTYPE_U2.toInt()) {
      return 2 <= align ? align : 2;
    }else if (JavaToWhirlConst.getMType(t) == WhirlConstants.Mtype.MTYPE_I4.toInt()
        || JavaToWhirlConst.getMType(t) == WhirlConstants.Mtype.MTYPE_U4.toInt()
        || JavaToWhirlConst.getMType(t) == WhirlConstants.Mtype.MTYPE_F4.toInt()){
      return 4 <= align ? align : 4;
    }else if (JavaToWhirlConst.getMType(t) == WhirlConstants.Mtype.MTYPE_I8.toInt()
        || JavaToWhirlConst.getMType(t) == WhirlConstants.Mtype.MTYPE_U8.toInt()
        || JavaToWhirlConst.getMType(t) == WhirlConstants.Mtype.MTYPE_F8.toInt()
        || JavaToWhirlConst.getMType(t) == WhirlConstants.Mtype.MTYPE_C4.toInt()) {
      return 8 <= align ? align : 8;
    }else if (JavaToWhirlConst.getMType(t) == WhirlConstants.Mtype.MTYPE_C8.toInt()
        || JavaToWhirlConst.getMType(t) == WhirlConstants.Mtype.MTYPE_F16.toInt()) {
      return 16 <= align ? align : 16;
    }else if (JavaToWhirlConst.getMType(t) == WhirlConstants.Mtype.MTYPE_FQ.toInt()) {
      return 32 <= align ? align : 32;
    }else if (JavaToWhirlConst.getMType(t) == WhirlConstants.Mtype.MTYPE_CQ.toInt()) {
      return 64 <= align ? align : 64;
    }else if (JavaToWhirlConst.getMType(t) == WhirlConstants.Mtype.MTYPE_B.toInt()) {
      return 1 <= align ? align : 1;
    }else if (JavaToWhirlConst.getMType(t) == WhirlConstants.Mtype.MTYPE_STR.toInt()) {
      assertThat(false ).as( ("[SootConstants::getJTypeSize] cannot determine size(as string) for " + t.toString())).isTrue();
      return 8;
    }else{
      assertThat(false ).as( ("[SootConstants::getJTypeSize] cannot determine size for " + t.toString())).isTrue();
      return 8 <= align ? align : 8;
    }
  }

}
