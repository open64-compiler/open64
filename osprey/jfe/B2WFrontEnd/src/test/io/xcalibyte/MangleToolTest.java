/*
 * Copyright (C) 2021 Xcalibyte (Shenzhen) Limited.
 */

package io.xcalibyte;

import com.github.rvesse.airline.SingleCommand;
import com.github.rvesse.airline.builder.ParserBuilder;
import com.github.rvesse.airline.model.ParserMetadata;
import com.github.rvesse.airline.parser.options.ClassicGetOptParser;
import io.xcalibyte.MangleTool;
import io.xcalibyte.test.classtest.B;
import org.junit.After;
import org.junit.Before;
import org.junit.Test;
import soot.RefType;
import soot.Scene;
import soot.SootClass;

import java.time.Instant;
import java.time.ZoneId;
import java.util.*;

import static org.junit.Assert.*;

public class MangleToolTest {

  @Before
  public void setUp() {
    //String[] args = {"-fB,a.B"};
    //B2WFrontEnd.parse(args);
    //B2WFrontEnd.i().run();
  }

  @After
  public void tearDown() {
  }

  @Test
  public void mangle() {
    //MangleTool.class.getMethods();
//    String v = MangleTool("abcd");
//    assertTrue(v.startsWith("_Utf"));
//    v = MangleTool.mangleForUTF("I[]cV()");
//    assertTrue(v.startsWith("_Utf"));
//    v = MangleTool.mangleForUTF("L/com/java/my/poing");
//    assertTrue(v.startsWith("_Utf"));
  }

  @Test
  public void mangle1() {
    Scene.v();
  }

  @Test
  public void mangle2() {
    String ev = String.valueOf(new char[]{'A','Z',' '});
    System.out.println(ev);
    String em = Arrays.toString(new int[]{10,2,300,40});
    System.out.println(em);
    String eq = Arrays.toString(new Exception[]{new Exception()});
    System.out.println(eq);
  }

  @Test(expected = Throwable.class)
  public void mangle3() {
    Map<String, String> map = new HashMap<>();
    Comparator m = HashMap.Entry.comparingByValue();
    java.util.Base64.getMimeEncoder(0, new byte[]{3,4,5});
    java.util.UUID.fromString("qcs");
    java.util.TimeZone.setDefault(java.util.TimeZone.getTimeZone(ZoneId.of("q", map)));
    java.util.Date.from(Instant.MAX);
    Object[] obj1 = new Object[1];
    java.util.Arrays.deepEquals(obj1, obj1);
    java.util.Arrays.hashCode(new double[1]);
    java.util.Arrays.hashCode(new float[1]);
    java.util.Arrays.hashCode(new long[1]);
    java.util.Arrays.hashCode(new short[1]);
    java.util.Arrays.hashCode(new char[1]);
  }

  @Test
  public void mangleForClassInternalSymbol() {
  }
}