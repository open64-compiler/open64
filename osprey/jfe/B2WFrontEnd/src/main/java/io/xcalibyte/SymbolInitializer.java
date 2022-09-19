/*
 * Copyright (C) 2021 Xcalibyte (Shenzhen) Limited.
 */

package io.xcalibyte;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import soot.*;
import soot.util.Chain;

import static org.assertj.core.api.Assertions.*;

import java.util.*;

class SymbolInitializer {
  public static Logger logger = LogManager.getLogger(SymbolInitializer.class);
  private static Set<Long> initializedSymCache = new HashSet<>();
  private static Map<String, Long> initializedStringConst = new HashMap<>();
  private static Map<SootField, Long> initializedStaticField = new HashMap<>();
  private static boolean initializedClassSymbol = false; // mark for whether

  private static WhirlConstants.Initial getPtrMask() {
    if (BCRConfig.is32Abi()) {
      return WhirlConstants.Initial.U4_MASK;
    } else {
      return WhirlConstants.Initial.U8_MASK;
    }
  }

  static void initClassSymbol(SootClass clazz) {
    long symIdx = SymbolHandler.getClassInstanceSymbol(clazz);
    if(initializedSymCache.contains(symIdx)) {
      return;
    }
    initializedSymCache.add(symIdx);
    // FIXME: Check if lambda is present, then initialization might be acceptable then.
    // assertThat(initializedClassSymbol).as("[initClassSymbol] should only init one class's symbol, now met : " + clazz.getName()).isFalse();
    initializedClassSymbol = true;
    initPtrSymbol(clazz, InternalConstants.SymbolCategory.CLASS_SYMBOL_PTR);
    //initPtrSymbol(clazz, InternalConstants.SymbolCategory.GCJ_JCR_CLASS_PTR);
    SymbolHandler.ClassInfo classInfo = SymbolHandler.getClassInfo(clazz);
    InitValAppender initValueList = new InitValAppender(symIdx);
    initValueList.add(WhirlConstants.Initial.MASK, WhirlConstants.Initial.START_BLOCK);

    //*   0  vtable  	anon_ptr. (#49) align 4
    //*   0  .anonymous.5	java.lang.Object (#44) align 4
    initValueList.add(WhirlConstants.Initial.OFST_MASK, getVTableOffset());
    initValueList.add(WhirlConstants.Initial.SYM_MASK, getJavaLangClassVTable());
    //*   4  next_or_version	anon_ptr. (#52) align 4
    initValueList.add(getPtrMask(), InternalConstants.CLASS_VERSION);
    //*   8  name    	anon_ptr. (#54) align
    initValueList.add(WhirlConstants.Initial.SYM_MASK,initStringConst(clazz.getName()));
    //*  12  accflags	.predef_U2 (#7) align
    initValueList.add(WhirlConstants.Initial.I2_MASK, clazz.getModifiers());
    //*  16  superclass	anon_ptr. (#52) align
    if(clazz.hasSuperclass()) {
      initValueList.add(WhirlConstants.Initial.SYM_MASK, SymbolHandler.getClassInstanceSymbol(clazz.getSuperclass()));
    } else {
      initValueList.add(getPtrMask(), 0L);
    }

    //*  20  constants	.anonymous.4 (#51) align
    initValueList.add(WhirlConstants.Initial.MASK, WhirlConstants.Initial.START_BLOCK);
    //    0  size  CT_SIZE       .predef_U4 (#8) align 4
    initValueList.add(WhirlConstants.Initial.I4_MASK, SymbolHandler.getClassInfo(clazz).getConstantSize());
    //    4  tags _CT_MyClass     anon_ptr. (#48) align 4
    initValueList.add(WhirlConstants.Initial.SYM_MASK, initConstantTags(clazz));
    //    8  data _CD_MyClass     anon_ptr. (#48) align 4
    initValueList.add(WhirlConstants.Initial.SYM_MASK, initConstantData(clazz));
    initValueList.add(WhirlConstants.Initial.MASK, WhirlConstants.Initial.END_BLOCK);

    //*  32  methods _MT_a        anon_ptr. (#56) align
    if (classInfo.getMethodTable().size() > 0) {
      initValueList.add(WhirlConstants.Initial.SYM_MASK, initMethodTable(clazz, classInfo));
    } else {
      initValueList.add(getPtrMask(), 0L);
    }
    //*  36  method_count	.predef_I2 (#3) align
    initValueList.add(WhirlConstants.Initial.I2_MASK, (long) clazz.getMethodCount());
    //*  38  vtable_method_count .predef_I2 (#3) align
    if (clazz.isInterface()) {
      initValueList.add(WhirlConstants.Initial.I2_MASK, 0L);
    } else {
      initValueList.add(WhirlConstants.Initial.I2_MASK, (long) classInfo.getVtable().size());
    }
    //*  40  fields  	anon_ptr. (#59) align
    if (classInfo.getFieldCount() > 0) {
      initValueList.add(WhirlConstants.Initial.SYM_MASK, initFieldsTable(clazz, classInfo));
    } else {
      initValueList.add(getPtrMask(), 0L);
    }
    //*  44  size_in_bytes	.predef_I4 (#4) align
    initValueList.add(WhirlConstants.Initial.I4_MASK, classInfo.getSize()); //FIXME: should add 8?
    //*  48  field_count	.predef_I2 (#3) align
    initValueList.add(WhirlConstants.Initial.I2_MASK, (long) classInfo.getFieldCount());
    //*  50  static_field_count .predef_I2 (#3) align
    initValueList.add(WhirlConstants.Initial.I2_MASK, (long) classInfo.getStaticFieldCount());
    //*  52  vtable  	anon_ptr. (#49) align
    if (clazz.isInterface()) {
      initValueList.add(getPtrMask(), 0L);
    } else {
      initValueList.add(WhirlConstants.Initial.OFST_MASK, getVTableOffset());
      initValueList.add(WhirlConstants.Initial.SYM_MASK, initVTable(clazz));
    }
    //*  56  otable  	anon_ptr. (#61) align
    initValueList.add(getPtrMask(), 0L);
    //*  60  otable_syms	anon_ptr. (#64) align
    initValueList.add(getPtrMask(), 0L);
    //*  64  atable  	anon_ptr. (#65) align
    initValueList.add(getPtrMask(), 0L);
    //*  68  atable_syms	anon_ptr. (#64) align
    initValueList.add(getPtrMask(), 0L);
    //*  72  itable  	anon_ptr. (#65) align
    initValueList.add(getPtrMask(), 0L);
    //*  76  itable_syms	anon_ptr. (#64) align
    initValueList.add(getPtrMask(), 0L);
    //*  80  catch_classes	anon_ptr. (#48) align
    initValueList.add(WhirlConstants.Initial.SYM_MASK, initCatchesTable(clazz, classInfo));
    //*  84  interfaces	anon_ptr. (#66) align
    if (clazz.getInterfaceCount() > 0) {
      initValueList.add(WhirlConstants.Initial.SYM_MASK, initInterfacesTable(clazz, classInfo));
    } else {
      initValueList.add(getPtrMask(), 0);
    }
    //*  88  loader  	anon_ptr. (#48) align
    initValueList.add(getPtrMask(), 0L);
    //*  92  interface_count	.predef_I2 (#3) align
    initValueList.add(WhirlConstants.Initial.I2_MASK,(long) clazz.getInterfaceCount());
    //*  94  state   	.predef_I1 (#2) align
    initValueList.add(WhirlConstants.Initial.I1_MASK,1L);
    //*  96  thread  	anon_ptr. (#48) align
    initValueList.add(getPtrMask(), 0L);
    //* 100  depth   	.predef_I2 (#3) align
    initValueList.add(WhirlConstants.Initial.I2_MASK,0L);
    //* 104  ancestors	anon_ptr. (#48) align
    initValueList.add(getPtrMask(), 0L);
    //* 108  idt     	anon_ptr. (#48) align
    initValueList.add(getPtrMask(), 0L);
    //* 112  arrayclass	anon_ptr. (#48) align
    initValueList.add(getPtrMask(), 0L);
    //* 116  protectionDomain	anon_ptr. (#48) align
    initValueList.add(getPtrMask(), 0L);
    //* 120  assertion_table	anon_ptr. (#48) align
    initValueList.add(getPtrMask(), 0L);
    //* 124  hack_signers	anon_ptr. (#48) align
    initValueList.add(getPtrMask(), 0L);
    //* 128  chain   	anon_ptr. (#48) align
    initValueList.add(getPtrMask(), 0L);
    //* 132  aux_info	anon_ptr. (#48) align
    initValueList.add(getPtrMask(), 0L);
    //* 136  engine  	anon_ptr. (#48) align
    initValueList.add(getPtrMask(), 0L);
    //* +++  reflection_data  anon_ptr. (#48) align
    if (!B2WFrontEnd.i().isEnableAnnotationData() || classInfo.getAnnotationSize() <= 0) {
      initValueList.add(getPtrMask(), 0L);
    } else {
      initValueList.add(WhirlConstants.Initial.SYM_MASK, initReflectionData(clazz, classInfo));
    }
    initValueList.add(WhirlConstants.Initial.MASK, WhirlConstants.Initial.END_BLOCK);
    initValueList.finish();
    initSymbolFlag(symIdx, WhirlConstants.STFlags.ST_ADDR_PASSED);
  }

  private static void initSymbolFlag(long idx, WhirlConstants.STFlags flags) {
    BGenDriver.setMiscFlags(idx, WhirlConstants.FlagKind.FLAG_SET_STFLAG.toInt(), flags.toInt());
  }

  private static long getVTableOffset() {
    return BCRConfig.ALIGN * 2;
  }

  /**
   * Convert a hex string to a byte array
   * @param inHex
   * @return
   */
  public static byte[] hexToByteArray(String inHex) {
    byte[] result;
    assertThat(inHex.length() % 2).as("Hexadecimal string should be with a length that's a multiple of two").isZero();
    result = new byte[(inHex.length() / 2)];
    int i = 0, j = 0;
    for (; i < inHex.length(); i+=2) {
      result[j++] = (byte) Integer.parseInt((inHex.substring(i, i+2)), 16);
    }
    return result;
  }

  /**
   * initReflectionData, either inject the user-appointed or gather the tags from the byte-code.
   * @param clazz
   * @param classInfo
   * @return
   */
  private static long initReflectionData(SootClass clazz, SymbolHandler.ClassInfo classInfo) {
    int length = classInfo.getAnnotationSize();
    assertThat(length).as("The annotation data must has a size larger than zero").isGreaterThan(0);
    long symIdx = SymbolHandler.getByteArray(length);
    if(initializedSymCache.contains(symIdx)) {
      return symIdx;
    }
    logger.debug("Initilializing byte array = {}", Arrays.toString(classInfo.getAnnotationByteArray()));
    if (B2WFrontEnd.i().getReflectionSubsideData() != null) {
      String subside = B2WFrontEnd.i().getReflectionSubsideData();
      initByteArray(hexToByteArray(subside), symIdx);
    } else {
      initByteArray(classInfo.getAnnotationByteArray(), symIdx);
    }
    return symIdx;
  }

  private static void initByteArray(byte[] annotationByteArray, long symIdx) {
    assertThat(annotationByteArray != null).as("Annotation byte array should not be null").isTrue();
    InitValAppender initValueList = new InitValAppender(symIdx);
    initValueList.add(WhirlConstants.Initial.STR_MASK, annotationByteArray);
    initValueList.finish();
  }

  private static long initConstantTags(SootClass clazz) {
    long symIdx = SymbolHandler.getInternal(clazz, InternalConstants.SymbolCategory.GCJ_CONSTANTS_TAGS);
    if(initializedSymCache.contains(symIdx)) {
      return symIdx;
    }
    initializedSymCache.add(symIdx);
    List<SymbolHandler.JvConstant> constants = SymbolHandler.getClassInfo(clazz).getConstants();
    InitValAppender initval = new InitValAppender(symIdx);
    initval.add(WhirlConstants.Initial.MASK, WhirlConstants.Initial.START_BLOCK);
    for (SymbolHandler.JvConstant obj : constants){
      initval.add(WhirlConstants.Initial.U1_MASK, obj.getType().val);
    }
    initval.add(WhirlConstants.Initial.MASK, WhirlConstants.Initial.END_BLOCK);
    initval.finish();
    setFstatic(symIdx);
    return symIdx;
  }

  private static long initConstantData(SootClass clazz) {
    long symIdx = SymbolHandler.getInternal(clazz, InternalConstants.SymbolCategory.GCJ_CONSTANTS_DATA);
    if(initializedSymCache.contains(symIdx)) {
      return symIdx;
    }
    initializedSymCache.add(symIdx);
    List<SymbolHandler.JvConstant> constants = SymbolHandler.getClassInfo(clazz).getConstants();
    InitValAppender initval = new InitValAppender(symIdx);
    initval.add(WhirlConstants.Initial.MASK, WhirlConstants.Initial.START_BLOCK);
    for (SymbolHandler.JvConstant obj : constants) {
      switch (obj.getType()) {
        case JV_CONSTANT_Unicode:
          assertThat(false).as("JvConstantType is JV_CONSTANT_Undefined.").isTrue();
          break;
        case JV_CONSTANT_Undefined:
        case JV_CONSTANT_Integer:
        case JV_CONSTANT_Float: // float value already converted to row int value
          initval.add(WhirlConstants.Initial.U8_MASK, obj.getValue());
          break;
        case JV_CONSTANT_Long:
        case JV_CONSTANT_Double: // double value already converted to row long value
          initval.add(WhirlConstants.Initial.U8_MASK, obj.getValue());
          break;
        case JV_CONSTANT_Utf8:
        case JV_CONSTANT_Class:
        case JV_CONSTANT_String:
        case JV_CONSTANT_Fieldref:
        case JV_CONSTANT_Methodref:
        case JV_CONSTANT_InterfaceMethodref:
        case JV_CONSTANT_NameAndType:
          initval.add(WhirlConstants.Initial.SYM_MASK, obj.getValue());
          break;
        default:
          assertThat(false).as("Not supported type for now, JvConstantType is " + obj.getType() + ".").isTrue();
          break;
      }
    }
    initval.add(WhirlConstants.Initial.MASK, WhirlConstants.Initial.END_BLOCK);
    initval.finish();
    setFstatic(symIdx);
    return symIdx;
  }

  private static long initPtrSymbol(SootClass clazz, InternalConstants.SymbolCategory category) {
    long symIdx = SymbolHandler.getInternal(clazz, category);
    if(initializedSymCache.contains(symIdx)) {
      return symIdx;
    }
    assertThat(SymbolHandler.getClassInstanceSymbol(clazz))
            .as("[initPtrSymbol] class symbol should be visited first, then the initptr symbol.")
            .isIn(initializedSymCache);
    InitValAppender initValueList = new InitValAppender(symIdx);
    initValueList.add(WhirlConstants.Initial.MASK, WhirlConstants.Initial.START_BLOCK);
    initValueList.add(WhirlConstants.Initial.SYM_MASK, SymbolHandler.getClassInstanceSymbol(clazz));
    initValueList.add(WhirlConstants.Initial.MASK, WhirlConstants.Initial.END_BLOCK);
    initValueList.finish();
    initializedSymCache.add(symIdx);
    BGenDriver.setMiscFlags(symIdx,
            WhirlConstants.FlagKind.FLAG_SCLASS.toInt(),
            WhirlConstants.SClass.SCLASS_DGLOBAL.toInt());
    return symIdx;
  }

  private static Long getJavaLangClassVTable() {
    SootClass clazz = Scene.v().getSootClass(InternalConstants.JvName.LANG_CLASS);
    assertThat(SymbolHandler.getVTableSymbol(clazz) != 0 ).as( ("cannot get vtable for class : java.lang.Class")).isTrue();
    return SymbolHandler.getVTableSymbol(clazz);
  }

  static private long initVTable(SootClass clazz) {
    assertThat(!clazz.isInterface()).as( "Interface don't have vtable symbol, interface : " + clazz).isTrue();
    long symIdx = SymbolHandler.getVTableSymbol(clazz);
    if(initializedSymCache.contains(symIdx)) {
      return symIdx;
    }
    initializedSymCache.add(symIdx);
    SymbolHandler.ClassInfo info = SymbolHandler.getClassInfo(clazz);
    InitValAppender initValueList = new InitValAppender(symIdx);
    initValueList.add(WhirlConstants.Initial.MASK, WhirlConstants.Initial.START_BLOCK);
    // top offset
    initValueList.add(getPtrMask(),0L);
    // type info
    initValueList.add(getPtrMask(),0L);
    // class symbol
    initValueList.add(WhirlConstants.Initial.SYM_MASK, SymbolHandler.getClassInstanceSymbol(clazz));
    // gc_descr
    initValueList.add(getPtrMask(), 8L);
    // methods
    for (int i = 0; i < info.getVtable().size(); i++) {
      SootMethod m = info.getVtable().get(i);
      if (B2WFrontEnd.i().isGcj() && m.isAbstract()) {
        initValueList.add(getPtrMask(), 0);
      } else {
        initValueList.add(WhirlConstants.Initial.SYM_MASK, SymbolHandler.getSymbol(info.getVtable().get(i)));
      }
    }
    initValueList.add(WhirlConstants.Initial.MASK, WhirlConstants.Initial.END_BLOCK);
    initValueList.finish();
    BGenDriver.setMiscFlags(symIdx, WhirlConstants.FlagKind.FLAG_SET_STFLAG_EXT.toInt(),
            WhirlConstants.STFlagsExt.ST_IS_VTABLE.toInt());
    BGenDriver.setMiscFlags(symIdx, WhirlConstants.FlagKind.FLAG_SET_STFLAG_EXT.toInt(),
            WhirlConstants.STFlagsExt.ST_INITV_IN_OTHER_ST.toInt());
    return symIdx;
  }

  static private long initMethodTable(SootClass clazz, SymbolHandler.ClassInfo classInfo) {
    long symIdx = SymbolHandler.getInternal(clazz, InternalConstants.SymbolCategory.METHOD_TABLE);
    if(initializedSymCache.contains(symIdx)) {
      return symIdx;
    }
    initializedSymCache.add(symIdx);
    InitValAppender initValueList = new InitValAppender(symIdx);
    initValueList.add(WhirlConstants.Initial.MASK, WhirlConstants.Initial.START_BLOCK);
    SymbolHandler.TableInfo<SootMethod> methodTable = classInfo.getMethodTable();
    for(int i = 0; i < methodTable.size(); i++) {
      SootMethod method = methodTable.get(i);
      initValueList.add(WhirlConstants.Initial.MASK, WhirlConstants.Initial.START_BLOCK);
      // name
      initValueList.add(WhirlConstants.Initial.SYM_MASK, initStringConst(method.getName()));
      // signature
      initValueList.add(WhirlConstants.Initial.SYM_MASK, initStringConst(MangleTool.mangleForMethodSignature(method)));
      // accflags
      long accflag = method.getModifiers() & 0xffff;
      accflag |= 0x4000;
      initValueList.add(WhirlConstants.Initial.U2_MASK, accflag);
      // FIXME: offset maybe not correct -- compromise to I2, should be U2 (declared, yet gcj didn't follow)

      if(clazz.isInterface()) {
        initValueList.add(WhirlConstants.Initial.I2_MASK, -1);
      } else {
        initValueList.add(WhirlConstants.Initial.I2_MASK, (long) classInfo.getVtable().getOffset(method));
      }
      // ncode
      // abstract class's abstract method can't call getSymbol, can't be symbol table
      // will cause link error, abstract method will not have definition
      if (clazz.isAbstract() && method.isAbstract() && B2WFrontEnd.i().isGcj()) {
        initValueList.add(WhirlConstants.Initial.SYM_MASK, SymbolHandler.getJvMethodSym(InternalConstants.JvMethod._Jv_ThrowAbstractMethodError));
      } else {
        initValueList.add(WhirlConstants.Initial.SYM_MASK, SymbolHandler.getMethodNCodeSymbol(method));
      }

      // throws
      if (method.getExceptionsUnsafe() == null || method.getExceptions().size() <= 0) {
        initValueList.add(getPtrMask(), 0);
      } else {
        initValueList.add(WhirlConstants.Initial.SYM_MASK, initMethodThrows(method));
      }
      initValueList.add(WhirlConstants.Initial.MASK, WhirlConstants.Initial.END_BLOCK);
    }
    initValueList.add(WhirlConstants.Initial.MASK, WhirlConstants.Initial.END_BLOCK);
    initValueList.finish();
    setFstatic(symIdx);
    return symIdx;
  }

  private static long initMethodThrows(SootMethod method) {
    long symIdx = SymbolHandler.getMethodThrowsTable(method);
    if(initializedSymCache.contains(symIdx)) {
      return symIdx;
    }
    InitValAppender initValueList = new InitValAppender(symIdx);
    initValueList.add(WhirlConstants.Initial.MASK, WhirlConstants.Initial.START_BLOCK);
    assertThat(method.getExceptionsUnsafe() != null && method.getExceptions().size() > 0).isTrue();
    List<SootClass> throwsTable = method.getExceptions();
    for(int i = 0; i < throwsTable.size(); i++) {
      String throwStr = MangleTool.mangleForClassInternalSymbol(
                                     throwsTable.get(i).getName(),
                                     InternalConstants.SymbolCategory.CLASS_SYMBOL);
      initValueList.add(WhirlConstants.Initial.SYM_MASK, initStringConst(throwStr));
    }
    initValueList.add(getPtrMask(), 0);
    initValueList.add(WhirlConstants.Initial.MASK, WhirlConstants.Initial.END_BLOCK);
    initValueList.finish();
    setFstatic(symIdx);
    return symIdx;
  }

  static private long initFieldsTable(SootClass clazz, SymbolHandler.ClassInfo classInfo) {
    long symIdx = SymbolHandler.getInternal(clazz, InternalConstants.SymbolCategory.FIELD_TABLE);
    if(initializedSymCache.contains(symIdx)) {
      return symIdx;
    }
    // TODO: Fields Table
    /**
     * struct _Jv_Field
     * {
     *   struct _Jv_Utf8Const*	name;
     *
     *   /* The type of the field, if isResolved().
     *      If !isResolved():  The fields's signature as a (Utf8Const*).
     *  jclass type;
     *
     *_Jv_ushort flags;
     *
     *_Jv_ushort bsize;  /* not really needed ...
     *
     *union {
     *jint boffset;  /* offset in bytes for instance field
     *char*addr;  /* address of static field
     *
     *jobject * object_addr;  /* address of static object field...
     *jbyte * byte_addr;
     *jshort * short_addr;
     *jchar * char_addr;
     *jint * int_addr;
     *jlong * long_addr;
     *jfloat * float_addr;
     *jdouble * double_addr;
     *} u;
     */
    InitValAppender initValueList = new InitValAppender(symIdx);
    initValueList.add(WhirlConstants.Initial.MASK, WhirlConstants.Initial.START_BLOCK);
    for(SootField sf : clazz.getFields()) {
      initValueList.add(WhirlConstants.Initial.MASK, WhirlConstants.Initial.START_BLOCK);
      initValueList.add(WhirlConstants.Initial.SYM_MASK, initStringConst(sf.getName()));
      if(TypeHandler.getBoxedClass(sf.getType()) == null){
        initValueList.add(getPtrMask(),0);
      }else {
        initValueList.add(WhirlConstants.Initial.SYM_MASK,
                SymbolHandler.getClassInstanceSymbol(TypeHandler.getBoxedClass(sf.getType())));
      }
      initValueList.add(WhirlConstants.Initial.U2_MASK, InternalConstants.AccFlags.getAccFlags(sf));
      initValueList.add(WhirlConstants.Initial.U2_MASK, 0); //TODO: evaluated whether needed.
      if (sf.isStatic()) {
        initValueList.add(WhirlConstants.Initial.SYM_MASK, SymbolHandler.getSymbol(sf));
      } else {
        initValueList.add(getPtrMask(), 0);
      }
      initValueList.add(WhirlConstants.Initial.MASK, WhirlConstants.Initial.END_BLOCK);
    }
    initValueList.add(WhirlConstants.Initial.MASK, WhirlConstants.Initial.END_BLOCK);
    initValueList.finish();
    setFstatic(symIdx);
    initializedSymCache.add(symIdx);
    return symIdx;
  }

  private static long initInterfacesTable(SootClass clazz, SymbolHandler.ClassInfo classInfo) { //FIXME: need to be done with classInfo
    long symIdx = SymbolHandler.getInternal(clazz, InternalConstants.SymbolCategory.INTERFACE_TABLE);
    if(initializedSymCache.contains(symIdx)) {
      return symIdx;
    }
    Chain<SootClass> classes = clazz.getInterfaces();
    InitValAppender initValueList = new InitValAppender(symIdx);
    initValueList.add(WhirlConstants.Initial.MASK, WhirlConstants.Initial.START_BLOCK);
    for (SootClass sc : classes) {
      initValueList.add(WhirlConstants.Initial.SYM_MASK, SymbolHandler.getClassInstanceSymbol(sc));
    }
    initValueList.add(WhirlConstants.Initial.MASK, WhirlConstants.Initial.END_BLOCK);
    initValueList.finish();
    setFstatic(symIdx);
    initializedSymCache.add(symIdx);
    return symIdx;
  }

  private static long initCatchesTable(SootClass clazz, SymbolHandler.ClassInfo classInfo) { //FIXME: need to be done with classInfo
    long symIdx = SymbolHandler.getInternal(clazz, InternalConstants.SymbolCategory.CATCHES_TABLE);
    if(initializedSymCache.contains(symIdx)) {
      return symIdx;
    }
    //FIXME: add catch table, temporarily set all field to zero
    // (did not found what to do in Open64Java)
    // (yet we should refer to GCJ is this is important)
    int size = 3;
    InitValAppender initValueList = new InitValAppender(symIdx);
    initValueList.add(WhirlConstants.Initial.MASK, WhirlConstants.Initial.START_BLOCK);
    for(int i = 0; i < size; i++) {
      initValueList.add(WhirlConstants.Initial.MASK, WhirlConstants.Initial.START_BLOCK);
      initValueList.add(getPtrMask(), 0);
      initValueList.add(getPtrMask(), 0);
      initValueList.add(WhirlConstants.Initial.MASK, WhirlConstants.Initial.END_BLOCK);
    }
    initValueList.add(WhirlConstants.Initial.MASK, WhirlConstants.Initial.END_BLOCK);
    initValueList.finish();
    setFstatic(symIdx);
    initializedSymCache.add(symIdx);
    return symIdx;
  }

  static long initStringConst(String str) {
    if(initializedStringConst.containsKey(str)) {
      return initializedStringConst.get(str);
    }
    long symIdx = SymbolHandler.getStringConstSymbol(str);
    InitValAppender initValueList = new InitValAppender(symIdx);
    initValueList.add(WhirlConstants.Initial.MASK,WhirlConstants.Initial.START_BLOCK);
    // hash
    int hash16 = getHash16(str);
    assertThat(hash16).as("[InitValAppender hash16 function wrong] hash:" + hash16).isGreaterThanOrEqualTo(0);
    initValueList.add(WhirlConstants.Initial.U2_MASK, hash16 );
    // length
    initValueList.add(WhirlConstants.Initial.U2_MASK,(long) SymbolHandler.getUtf8StringSize(str) - 1);
    // data
    initValueList.add(WhirlConstants.Initial.STR_MASK, str);
    initValueList.add(WhirlConstants.Initial.MASK, WhirlConstants.Initial.END_BLOCK);
    initValueList.finish();
    initializedSymCache.add(symIdx);
    initializedStringConst.put(str, symIdx);
    setFstatic(symIdx);
    BGenDriver.setMiscFlags(symIdx, WhirlConstants.FlagKind.FLAG_SET_STFLAG.toInt(),
            WhirlConstants.STFlags.ST_IS_CONST_VAR.toInt());
    return symIdx;
  }

  static void initSymbol(long symIdx, long initSymIdx) {
    InitValAppender initValList = new InitValAppender(symIdx);
    initValList.add(WhirlConstants.Initial.SYM_MASK, initSymIdx);
    initValList.finish();
    BGenDriver.setMiscFlags(symIdx, WhirlConstants.FlagKind.FLAG_SET_STFLAG.toInt(),
      WhirlConstants.STFlags.ST_IS_CONST_VAR.toInt());
    BGenDriver.setMiscFlags(symIdx, WhirlConstants.FlagKind.FLAG_SET_STFLAG.toInt(),
      WhirlConstants.STFlags.ST_IS_INITIALIZED.toInt());
  }

  static void setFstatic(long symIdx) {
    BGenDriver.setMiscFlags(symIdx, WhirlConstants.FlagKind.FLAG_SCLASS.toInt(),
            WhirlConstants.SClass.SCLASS_FSTATIC.toInt());
    BGenDriver.setMiscFlags(symIdx, WhirlConstants.FlagKind.FLAG_EXPORT.toInt(),
            WhirlConstants.SExport.EXPORT_LOCAL.toInt());
  }

  /**
   * Computes the hashcode for this String. This is done with int arithmetic,
   * where ** represents exponentiation, by this formula:<br>
   * <code>s[0]*31**(n-1) + s[1]*31**(n-2) + ... + s[n-1]</code>.
   *
   * @return hashcode value of this String
   */
  static int getHash16(String str)
  {
    return str.hashCode() & 0xFFFF;
  }

  static long initStaticSymbol(SootField fd) {
    if (initializedStaticField.containsKey(fd)) {
      return initializedStringConst.get(fd);
    }
    long symIdx = SymbolHandler.getSymbol(fd);
    // TODO: finish Initialization of Static Fields (AnyClass::__U3c_clinit__U3e_) function
    if (fd.getType() instanceof RefType) {
      // null
      // not-null -> clinit
      //assertThat(fd.getType() instanceof RefType).as("havn't supported ref static field initialization right now.").isFalse();
    } else if (fd.getType() instanceof ArrayType) {
      // null
      // not-null -> clinit
      //assertThat(fd.getType() instanceof ArrayType).as("havn't supported array static field initialization right now.").isFalse();
    } else {
      // Primitive
      //assertThat(fd.getType() instanceof ArrayType).as("havn't supported non-array static field initialization right now.").isTrue();
    }
    BGenDriver.setMiscFlags(symIdx,
            WhirlConstants.FlagKind.FLAG_SCLASS.toInt(),
            WhirlConstants.SClass.SCLASS_UGLOBAL.toInt()); //Same as Open64Java
    return symIdx;
  }

  static class InitValAppender {

    static Map<Long, Object> tempValueMap = new HashMap<>();
    private int typeAlign;
    private long symbolIdx = 0;
    private long typeIdx = 0;
    private long requiredSize = 0;
    private List<WhirlConstants.Initial> mask = new LinkedList<>();
    private List<Long> data = new LinkedList<>();
    private long savedINITO = -1;

    InitValAppender(long stId) {
      super();
      size = 0;
      symbolIdx = stId;
      assertThat(stId > 0 && ((stId >> 8) > 0)).as("InitValAppender symbolIdx not valid : " + stId).isTrue();
      typeIdx = BGenDriver.getTyFromST(stId);
      assertThat(typeIdx > 0 && ((typeIdx >> 8) > 0)).as("InitValAppender typeIdx not retrievable : " + stId + ",ty:"+typeIdx).isTrue();
      requiredSize = BGenDriver.getTySize(typeIdx);
      assertThat(requiredSize > 0).as("InitValAppender, symbol has size <= 0 , symbol:"+symbolIdx + ", type:"+typeIdx).isTrue();
      typeAlign = BGenDriver.getTyAlign(typeIdx);
      if(typeAlign == 0){
        assertThat(typeAlign).as("[InitValAppender should not met align == 0]" + symbolIdx).isNotZero();
        typeAlign = BCRConfig.ALIGN;
      }
      assertThat(typeAlign).as("GCJType align should be greater than zero.").isGreaterThan(0);
    }

    public long getSavedINITO() {
      assertThat(savedINITO != -1).as("savedINITO should be initialized").isTrue();
      return savedINITO;
    }

    int size = 0;
    int remain = 0;

    InitValAppender add(WhirlConstants.Initial mask, String val) {
      // padding?
      if(mask == WhirlConstants.Initial.STR_MASK){
        long valTempIdx = BGenDriver.saveAsTempString(val);
        tempValueMap.put(valTempIdx, val);
        alignAdd(SymbolHandler.getUtf8StringSize(val));
        this.mask.add(mask);
        this.data.add(valTempIdx);
      }else{
        assertThat(false).as( "[InitValAppender::add(MASK,String)] MASK cannot be " + mask).isTrue();
      }
      return this;
    }

    InitValAppender add(WhirlConstants.Initial mask, byte[] val) {
      // padding?
      if(mask == WhirlConstants.Initial.STR_MASK){
        long valTempIdx = BGenDriver.saveAsTempByteArray(val);
        tempValueMap.put(valTempIdx, val);
        alignAdd(val.length);
        this.mask.add(mask);
        this.data.add(valTempIdx);
      }else{
        assertThat(false).as( "[InitValAppender::add(MASK,String)] MASK cannot be " + mask).isTrue();
      }
      return this;
    }

    InitValAppender add(WhirlConstants.Initial mask, WhirlConstants.Initial val) {
      switch (mask){
        case MASK:
          switch (val){
            case START_BLOCK:
            case VAL_NOP:
              break;
            case END_BLOCK:
              alignWithPad();
              break;
            default:
              assertThat(false).as( "[InitValAppender::add(MASK_1,MASK_2)] MASK_2 cannot be " + mask).isTrue();
          }
          this.mask.add(mask);
          this.data.add(val.toInt());
          break;
        default:
          assertThat(false).as( "[InitValAppender::add(MASK_1,MASK_2)] MASK_1 cannot be " + mask).isTrue();
      }
      return this;
    }

    InitValAppender add(WhirlConstants.Initial mask, long val) {
      // padding?
      int fieldSize = 0;
      switch (mask) {
        case I1_MASK:
          fieldSize = 1;
          break;
        case I2_MASK:
          fieldSize = 2;
          break;
        case I4_MASK:
          fieldSize = 4;
          break;
        case I8_MASK:
          fieldSize = 8;
          break;
        case SYM_MASK:
          fieldSize = BCRConfig.getWordBytes();
          break;
        case LABEL_MASK:
          fieldSize = 4;
          break;
        case F4_MASK:
          fieldSize = 4;
          break;
        case F8_MASK:
          fieldSize = 8;
          break;
        case U1_MASK:
          fieldSize = 1;
          break;
        case U2_MASK:
          fieldSize = 2;
          break;
        case U4_MASK:
          fieldSize = 4;
          break;
        case U8_MASK:
          fieldSize = 8;
          break;
        case OFST_MASK:
          fieldSize = 0;
          break;
        case STR_MASK:
        case MASK:
        case PAD_MASK:
        case NO_MASK:
        default:
          assertThat(false).as( "[InitValAppender::add] cannot accept (mask, long) as MASK = " + mask).isTrue();
      }
      switch (mask) {
        case U1_MASK:
        case U2_MASK:
        case U4_MASK:
        case U8_MASK:
//          assertThat(val).as("[InitValAppender::add] Unsigned type cannot accept value < 0 , given : " + val)
//                  .isGreaterThanOrEqualTo(0);
          break;
        default:
          break;
      }
      alignAdd(fieldSize);
      this.mask.add(mask);
      this.data.add(val);
      return this;
    }

    List<Long> toList(){
      List<Long> initvList = new ArrayList<>();
      for (int i = 0; i < mask.size(); i++) {
        initvList.add(mask.get(i).toInt());
        initvList.add(data.get(i));
      }
      return initvList;
    }

    void finish() {
      alignWithPad();
      if(size != requiredSize){
        BGenDriver.printSymbolInfo(symbolIdx);
        BGenDriver.printTypeInfo(typeIdx);
        print();
        assertThat(size == requiredSize).as("[InitValAppender::finish] incorrect " +
                "length to initialize required:"+requiredSize+", given:"+size + "\n\n Please also refer to trace file to see the type's info.!").isTrue();
      }
      savedINITO = BGenDriver.initializeWithList(toList(), symbolIdx);
    }

    private void print() {
      System.err.println("[InitValAppender] size:"+size+", remain:"+remain);
      for (int i = 0; i < mask.size(); i++) {
        if(mask.get(i) == WhirlConstants.Initial.MASK){
          System.err.println("(" + mask.get(i) + "," + WhirlConstants.Initial.fromInt(data.get(i)) + ")");
        }else {
          System.err.println("(" + mask.get(i) + "," + data.get(i) + ")");
        }
      }
    }

    private void alignWithPad() {
      if(remain > 0){
        addPad(typeAlign - remain);
      }
    }

    private void addPad(int len) {
      mask.add(WhirlConstants.Initial.PAD_MASK);
      data.add((long) len);
      alignAdd(len);
    }

    private void alignAdd(int len) {
      if (remain + len < typeAlign) {
        remain += len;
      } else if(remain + len == typeAlign) {
        remain = 0;
        size += typeAlign;
      } else{
        alignWithPad();
        remain = len % typeAlign;
        if (remain == 0){
          size += len;
        } else {
          size += len - remain;
        }
      }
    }
  }
}
