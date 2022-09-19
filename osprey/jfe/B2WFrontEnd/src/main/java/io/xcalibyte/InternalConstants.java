/*
 * Copyright (C) 2021 Xcalibyte (Shenzhen) Limited.
 */

package io.xcalibyte;

import soot.Modifier;
import soot.SootClass;
import soot.SootField;
import soot.SootMethod;

class InternalConstants {

  final static String UTF_DUMMY_INDENTIFIER = "DUMMY";
  final static int FILE_NAME_MAX_LIMIT = 256;
  final static long CLASS_VERSION = 600000;

  static class JvName {
    final static String _JV_METHOD = "_Jv_Method";
    final static String _JV_UTF8CONST = "_Jv_Utf8Const";
    final static String _JV_VTABLE = "_JV_VTable";
    final static String LANG_OBJECT = "java.lang.Object";
    final static String LANG_CLASS = "java.lang.Class";
  }


  public enum FileSystemFlag {
    FILE_SYSTEM_UNIX(1),
    FILE_SYSTEM_WINDOWS(2);
    int val;
    FileSystemFlag(int val) {
      this.val = val;
    }
  }

  enum GCJType {
    TYPE_INFO,
    CLASS_TYPE_INFO,
    VMI_CLASS_TYPE_INFO,
    BASE_CLASS_TYPE_INFO,

    JAVA_LANG_OBJECT,
    JAVA_LANG_OBJECT_PTR,
    JAVA_LANG_THROWABLE,
    JAVA_LANG_THROWABLE_PTR,
    JAVA_LANG_CLASS,
    JAVA_LANG_CLASS_PTR,
    JAVA_LANG_CLASS_PTR_PTR,

    V_PTR,
    V_PTR_PTR,
    V_PTR_PTR_PTR,

    U1_PTR,
    U2_PTR,
    U4_PTR,
    U8_PTR,

    I1_PTR,
    I2_PTR,
    I4_PTR,
    I8_PTR,

    GCJ_METHOD_SYMBOL,
    GCJ_METHOD_SYMBOL_PTR,
    GCJ_ATABLE,
    GCJ_ATABLE_PTR,
    GCJ_OTABLE,
    GCJ_OTABLE_PTR,

    GCJ_VTABLE,
    GCJ_VTABLE_PTR,
    GCJ_METHOD,
    GCJ_METHOD_PTR,
    GCJ_FIELD,
    GCJ_FIELD_PTR,
    GCJ_FIELD_OFFSET,
    GCJ_CATCH,
    GCJ_CATCH_PTR,
    GCJ_UTF8CONST,
    GCJ_UTF8CONST_PTR,
    GCJ_CONSTANTS_TABLE,
    EH_REGION_TABLE,
    GCJ_UTF8CONST_PTR_PTR
  }

  public enum SymbolCategory {
    CLASS_SYMBOL,
    VTABLE,
    METHOD_TABLE,
    FIELD_TABLE,
    CATCHES_TABLE,
    INTERFACE_TABLE,
    GCJ_JCR_CLASS_PTR,
    CLASS_SYMBOL_PTR,
    GCJ_CONSTANTS_DATA,
    GCJ_CONSTANTS_TAGS,
    GCJ_CONSTANTS_DATA_BASE,
    GCJ_CONSTANTS_TAGS_BASE,
    GCJ_METHOD_THROW_TABLE,
    GCJ_REFLECTION_DATA;
  }

  public enum JvMethod {
    _Jv_AllocObjectNoFinalizer,
    _Jv_LookupInterfaceMethodIdx,
    _Jv_NewPrimArray,
    _Jv_NewObjectArray,
    _Jv_InitClass,
    _Jv_NewMultiArray,
    _Jv_Throw,
    _Jv_MonitorEnter,
    _Jv_MonitorExit,
    _Jv_IsInstanceOf,
    _Jv_CheckCast,
    _Jv_ThrowAbstractMethodError,
    _Jv_GetJNIEnvNewFrame,
    _Jv_LookupJNIMethod,
  }

  public enum JvAttrType {
    JV_CLASS_ATTR(0),
    JV_METHOD_ATTR(1),
    JV_FIELD_ATTR(2),
    JV_DONE_ATTR(3),
    JV_CODE_ATTR(4);

    int val;
    JvAttrType(int val) {
      this.val = val;
    }
  } ;

  public enum JvAttrKind {
    JV_INNER_CLASSES_KIND(0),
    JV_ENCLOSING_METHOD_KIND(1),
    JV_SIGNATURE_KIND(2),
    JV_ANNOTATIONS_KIND(3),
    JV_PARAMETER_ANNOTATIONS_KIND(4),
    JV_ANNOTATION_DEFAULT_KIND(5),
    JV_TYPE_ANNOTATIONS_KIND(6);

    int val;
    JvAttrKind(int val) {
      this.val = val;
    }
  };

  public enum JvConstantType {
    JV_CONSTANT_Undefined(0),
    JV_CONSTANT_Utf8(1),
    JV_CONSTANT_Unicode(2),
    JV_CONSTANT_Integer(3),
    JV_CONSTANT_Float(4),
    JV_CONSTANT_Long(5),
    JV_CONSTANT_Double(6),
    JV_CONSTANT_Class(7),
    JV_CONSTANT_String(8),
    JV_CONSTANT_Fieldref(9),
    JV_CONSTANT_Methodref(10),
    JV_CONSTANT_InterfaceMethodref(11),
    JV_CONSTANT_NameAndType(12),
    JV_CONSTANT_ResolvedFlag(16),
    JV_CONSTANT_LazyFlag(32),
    JV_CONSTANT_ResolvedString(16 | 8),
    JV_CONSTANT_ResolvedClass(16 | 7);

    int val;
    JvConstantType(int val) {
      this.val = val;
    }
  }

  //Please consult or visit the following places for verification
  //Java SE latest(as to JDK11) http://www.javassist.org/html/constant-values.html#javassist.bytecode.AccessFlag.BRIDGE
  //(as to JDK6) GCC-6.4.0 : libjava/java/lang/reflect/Modifier.h
  static class AccFlags {
    final static int ACC_PUBLIC = 0x0001;
    final static int ACC_PRIVATE = 0x0002;
    final static int ACC_PROTECTED = 0x0004;
    final static int ACC_STATIC = 0x0008;
    final static int ACC_FINAL = 0x0010;
    // Both 0x0020
    final static int ACC_SYNCHRONIZED = 0x0020; //32 ACC_and_Modifier
    final static int ACC_SUPER = 0x0020; //32 ACC_Only
    // Both 0x0040
    final static int ACC_VOLATILE = 0x0040; //64 ACC_and_Modifier
    final static int ACC_BRIDGE = 0x0040;   //64 ACC_Only
    // Both 0x0080
    final static int ACC_TRANSIENT = 0x0080; //128
    final static int ACC_VARARGS = 0x0080; //128
    final static int ACC_NATIVE = 0x0100; //256
    final static int ACC_INTERFACE = 0x0200; //512
    final static int ACC_ABSTRACT = 0x0400; //1024
    final static int ACC_STRICT = 0x0800; //2048
    final static int ACC_SYNTHETIC = 0x1000; //4096
    final static int ACC_ANNOTATION = 0x2000; //8192
    final static int ACC_ENUM = 0x4000; //16384
    final static int ACC_MANDATED = 0x8000; //32768 (JDK7+)
    final static int ACC_MODULE = 0x8000; //32768 (JDK9+)

    static long getModifiers(int m){
      long accFlags = 0;
      if(Modifier.isPublic(m)) {
        accFlags |= ACC_PUBLIC;
      }
      if(Modifier.isPrivate(m)) {
        accFlags |= ACC_PRIVATE;
      }
      if(Modifier.isProtected(m)) {
        accFlags |= ACC_PROTECTED;
      }
      if(Modifier.isStatic(m)) {
        accFlags |= ACC_STATIC; // 8
      }
      if(Modifier.isFinal(m)) {
        accFlags |= ACC_FINAL; // 16
      }
      if(Modifier.isSynchronized(m)) {
        accFlags |= ACC_SYNCHRONIZED; //32
      }
      if(Modifier.isVolatile(m)){
        accFlags |= ACC_VOLATILE; //64
      }
      if(Modifier.isTransient(m)){
        accFlags |= ACC_TRANSIENT; //128
      }
      if(Modifier.isNative(m)) {
        accFlags |= ACC_NATIVE; // 256
      }
      if(Modifier.isInterface(m)){
        accFlags |= ACC_INTERFACE; // 512
      }
      if(Modifier.isAbstract(m)) {
        accFlags |= ACC_ABSTRACT; // 1024
      }
      if(Modifier.isStrictFP(m)) {
        accFlags |= ACC_STRICT; // 2048
      }
      // 4096  = SYNTHETIC = not available in Modifiers
      if(Modifier.isAnnotation(m)){
        accFlags |= ACC_ANNOTATION; // 8192
      }
      if(Modifier.isEnum(m)){
        accFlags |= ACC_ENUM; // 16384
      }
      return accFlags;
    }

    static long getAccFlags(SootMethod method) {
      return method.getModifiers();
    }

    static long getAccFlags(SootClass clazz) {
      long accFlags = 0;
      int m = clazz.getModifiers();
      accFlags |= getModifiers(m);
      // FIXME: VARARGS Needed ?
      accFlags |= ACC_SYNCHRONIZED;  // FIXME: cancel hardcode for GCJ
      if(false){ // FIXME : How to determine if it's synthetic
        accFlags |= ACC_SYNTHETIC; //4096
      }
      if(false){ // FIXME : How to determine if it's MANDATED
        accFlags |= ACC_MANDATED; //32768
      }
      if(false){ // FIXME : How to determine if it's MODULE
        accFlags |= ACC_MODULE; //32768
      }
      return accFlags;
    }

    static long getAccFlags(SootField field) {
      long accFlags = 0;
      int m = field.getModifiers();
      accFlags |= getModifiers(m);
      return accFlags;
    }
  }
}
