/*
 * Copyright (C) 2021 Xcalibyte (Shenzhen) Limited.
 */

package org.jetbrains.java.decompiler.modules.b2w.handler;

public interface WhirlConstants
{
  public static enum SClass {
    SCLASS_UNKNOWN(0),
    SCLASS_AUTO(1),
    SCLASS_FORMAL(2),
    SCLASS_FORMAL_REF(3),
    SCLASS_EXTERN(7),
    SCLASS_UGLOBAL(8),
    SCLASS_DGLOBAL(9);

    private final int sclassId;

    private SClass(int id) {
      sclassId = id;
    }

    public int toInt() {
      return sclassId;
    }

    public String toString() {
      return String.valueOf(sclassId);
    }
  }

  public static enum TyFlag {
    TY_ATTR_NULL (0x0),
    TY_CONST (0x20),
    TY_VOLATILE (0x40);

    private final int typeFlag;

    TyFlag(int flag){
      typeFlag = flag;
    }

    public int toInt() {
      return typeFlag;
    }

    public String toString() {
      return String.valueOf(typeFlag);
    }
  }

  public static enum SExport {
    EXPORT_LOCAL(0),
    EXPORT_PREEMPTIBLE(5);

    private final int exportId;

    private SExport(int id) {
      exportId = id;
    }

    public int toInt() {
      return exportId;
    }

  }

  public static enum Mtype {
    // ----------------------------------------------------------------------
    // MTYPE
    // ----------------------------------------------------------------------
    MTYPE_B(1),
    MTYPE_I1(2),
    MTYPE_I2(3),
    MTYPE_I4(4),
    MTYPE_I8(5),
    MTYPE_U1(6),	/*   8-bit unsigned integer */
    MTYPE_U2(7),		/*  16-bit unsigned integer */
    MTYPE_U4(8),		/*  32-bit unsigned integer */
    MTYPE_U8(9),		/*  64-bit unsigned integer */

    MTYPE_F4(10),
    MTYPE_F8(11),

    MTYPE_F10(12),		/*  80-bit IEEE floating point */
    MTYPE_F16(13),		/* 128-bit IEEE floating point */

    MTYPE_STR(14)	,	/* char strings - TCONs only */
    MTYPE_STRING(14),
    MTYPE_FQ(15),		/* for SGI long double */
    MTYPE_M(16)	,		/* memory chunk, for structures */
    MTYPE_C4(17)	,	/* for 32-bit complex */
    MTYPE_C8(18)	,	/* for 64-bit complex */
    MTYPE_CQ(19)	,	/* for quad complex */
    MTYPE_V(20);   /* for */

    private final int mtypeId;

    Mtype(int id){
      mtypeId = id;
    }

    public int toInt() {
      return mtypeId;
    }

    public String toString() {
      return String.valueOf(mtypeId);
    }
  }

  public static enum Operator {
    // ----------------------------------------------------------------------
    // OPERATOR
    // ----------------------------------------------------------------------
    OPR_ADD (2),
    OPR_SUB (114);

    private final int operatorId;

    Operator(int id){
      operatorId = id;
    }

    public int toInt() {
      return operatorId;
    }

    public String toString() {
      return String.valueOf(operatorId);
    }
  }
}
