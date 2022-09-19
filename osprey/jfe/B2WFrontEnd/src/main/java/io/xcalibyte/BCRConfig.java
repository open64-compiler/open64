/*
 * Copyright (C) 2021 Xcalibyte (Shenzhen) Limited.
 */

package io.xcalibyte;

public class BCRConfig {

  private static boolean isInit = false;
  private static int abiBitLength;

  static final boolean USE_JAVA_CHARSET_UTF8 = false;
  static int ALIGN = 8;
  static final int DEFAULT_PU_ALIGN = 1;
  static final int DEFAULT_CLASS_SIZE = 1;

  static boolean USE_GCJ = true;

  static void init(int abiBitLength, boolean useGCJ) {
    isInit = true;
    BCRConfig.abiBitLength = abiBitLength;
    ALIGN = BCRConfig.abiBitLength / 8;
    USE_GCJ = useGCJ;
  }

  static int getPtrType() {
    if (abiBitLength == 64) {
      return WhirlConstants.Mtype.MTYPE_U8.toInt();
    } else {
      return WhirlConstants.Mtype.MTYPE_U4.toInt();
    }
  }

  static int getConvertableIntegerType() {
    if (abiBitLength == 64) {
      return WhirlConstants.Mtype.MTYPE_I8.toInt();
    } else {
      return WhirlConstants.Mtype.MTYPE_I4.toInt();
    }
  }

  static int getWordBytes() {
    return abiBitLength / 8;
  }

  static boolean is64Abi() {
    return abiBitLength == 64;
  }

  static boolean is32Abi() {
    return abiBitLength == 32;
  }

  public static int getAbiWidth() {
    return abiBitLength;
  }
}
