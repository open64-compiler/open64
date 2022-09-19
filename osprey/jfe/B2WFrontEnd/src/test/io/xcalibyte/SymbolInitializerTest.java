/*
 * Copyright (C) 2021 Xcalibyte (Shenzhen) Limited.
 */

package io.xcalibyte;

import io.xcalibyte.BGenDriver;
import org.apache.commons.lang3.ObjectUtils;
import org.junit.AfterClass;
import org.junit.BeforeClass;
import org.junit.Rule;
import org.junit.Test;
import org.junit.rules.ExpectedException;
import static io.xcalibyte.WhirlConstants.*;
import static io.xcalibyte.SymbolInitializer.*;
import static org.junit.Assert.*;

public class SymbolInitializerTest {

  public static boolean setUpCompleted = false;

  @BeforeClass
  public static synchronized void setUp() {
    if (setUpCompleted) return;
    setUpCompleted = true;
    System.loadLibrary("macbcb");
    BGenDriver.bgenInit( true,0xffffffff, 64);
    BGenDriver.bgenInitOpenIrFile("MyJavaTest.B");
    BGenDriver.bgenInitSetDST("MyJavaTest.class");
  }

  @AfterClass
  public static synchronized void tearDown() {
    BGenDriver.bgenFileClose();
    BGenDriver.bgenFileFinish();
  }

  @Test
  public void initClassSymbol() {
  }

  @Test
  public void getHashCode() {
  }

  @Test
  public void getUtf8StringSize() {
    int loc = getHash16("abc\u0000");
    assertNotEquals(4, loc);
  }

  @Test
  public void symBolInitializer(){
    long u2type = BGenDriver.getPrimitiveType(Mtype.MTYPE_U2.toInt());
    long simpleIndex = BGenDriver.createVarSymbol("simpleVar", u2type,
            SClass.SCLASS_UGLOBAL.toInt(),
            SExport.EXPORT_PREEMPTIBLE.toInt(), DefaultValue.LEVEL);
    InitValAppender initval = new InitValAppender(simpleIndex);
    initval.add(Initial.I2_MASK, 10);
    initval.finish();
  }
}