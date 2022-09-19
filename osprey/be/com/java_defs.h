/*
 * Copyright (C) 2021 Xcalibyte (Shenzhen) Limited.
 */

#ifndef JAVA_DEFS_INCLUDED
#define JAVA_DEFS_INCLUDED

// Enums
enum Class_Symbol_Entry_Kind {
  JAVA_LANG_CLASS_VTBL_SYM = 0,
  CLASS_VERSION,
  CLASS_NAME_SYM,
  CLASS_ACC_FLAG,
  SUPER_CLASS_SYM,
  CONSTANT_SIZE,
  CONSTANT_TAGS,
  CONSTANT_DATA,
  METH_TABLE_SYM,
  METH_CNT,
  VTABLE_METH_CNT,
  FIELDS_TABLE_SYM,
  CLASS_SIZE,
  FIELD_CNT,
  STATIC_FIELD_CNT,
  VTABLE_SYM,
  OTABLE,
  OTABLE_SYMS,
  ATABLE,
  ATABLE_SYMS,
  ITABLE,
  ITABLE_SYMS,
  CATCH_CLASSES_SYM,
  INTERFACE_TABLE_SYM,
  LOADER,
  INTERFACE_CNT,
  STATE,
  THREAD,
  DEPTH,
  ANCESTORS,
  IDT,
  ARRAY_CLASS,
  PROTECTION_DOMAIN,
  ASSERTION_TABLE,
  HACK_SIGNERS,
  CHAIN,
  AUX_INFO,
  ENGINE,
  REFLECTION_DATA,
  ENTRY_KIND_MAX
};


enum Vtable_Entry_Kind {
  TOP_OFFSET = 0,
  TYPE_INFO,
  CLASS_SYM,
  GC_DESC,
  VTABLE_FIRST_METH
};

enum Interface_Entry_Kind {
};

enum MTable_Entry_Kind {
  METH_NAME = 0,
  METH_SIGNATURE,
  METH_FLAG,
  METH_OFF,
  METH_SYM,
  THROWS,
  METH_END
};

enum StrTable_Entry_Kind {
  STRING_HASH = 0,
  STRING_LEN,
  STRING_IDX,
  STRING_END
};

enum Annotation_Entry_Kind {
  ANNOT_DATA = 0,
  ANNOT_END,
};

enum CLASS_FLAGS {
  IS_INTERFACE    = 0x00000001,
  HAS_INTERFACE   = 0x00000002,
  HAS_ANNOT       = 0x00000004,
};

// Copied from JFE InternalConstants.java
enum ACC_FLAG {
  ACC_INVALID      = 0x0000,
  ACC_PUBLIC       = 0x0001,
  ACC_PRIVATE      = 0x0002,
  ACC_PROTECTED    = 0x0004,
  ACC_STATIC       = 0x0008,
  ACC_FINAL        = 0x0010,
  ACC_SYNCHRONIZED = 0x0020, // ACC_and_Modifier
  ACC_SUPER        = 0x0020, // ACC_Only
  ACC_VOLATILE     = 0x0040, // ACC_and_Modifier
  ACC_BRIDGE       = 0x0040, // ACC_Only
  ACC_TRANSIENT    = 0x0080,
  ACC_VARARGS      = 0x0080,
  ACC_NATIVE       = 0x0100,
  ACC_INTERFACE    = 0x0200,
  ACC_ABSTRACT     = 0x0400,
  ACC_STRICT       = 0x0800,
  ACC_SYNTHETIC    = 0x1000,
  ACC_ANNOTATION   = 0x2000,
  ACC_ENUM         = 0x4000,
  ACC_MANDATED     = 0x8000, // JDK7+
  ACC_MODULE       = 0x8000  // JDK9+
};

#define JAVA_PRODUCER "XCALIBYTE_JFE"
#define RT_VERSION "XCALIBYTE_JFE_RT_1.1"
#define CLINIT "clinit"
#define JAVA_ARR_LEN_FLDID 4

#endif
