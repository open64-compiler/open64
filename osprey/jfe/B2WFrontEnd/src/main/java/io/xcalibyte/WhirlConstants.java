/*
 * Copyright (C) 2021 Xcalibyte (Shenzhen) Limited.
 */

package io.xcalibyte;

import static org.assertj.core.api.Assertions.*;

public class WhirlConstants {

  enum Initial {

    NO_MASK(0),
    MASK(1),
    SYM_MASK(2),
    I1_MASK(3),
    I2_MASK(4),
    I4_MASK(5),
    I8_MASK(6),
    F4_MASK(7),
    F8_MASK(8),
    STR_MASK(9),
    PAD_MASK(10),
    U1_MASK(21),
    U2_MASK(22),
    U4_MASK(23),
    U8_MASK(24),
    OFST_MASK(31),
    LABEL_MASK(35),

    START_BLOCK(1000),
    END_BLOCK(2000),
    VAL_NOP(3000);

    public final int internalId;

    Initial(int id) {
      internalId = id;
    }

    public static Initial fromInt(Long aLong) {
      for (Initial i: values()) {
        if(i.internalId == aLong){
          return i;
        }
      }
      assertThat(false).as("[WhirlConst::fromInt] cannot find counterpart for " + aLong).isTrue();
      return fromInt(0L);
    }

    public long toInt() {
      return internalId;
    }
  }

  enum STFlags {
    ST_IS_WEAK_SYMBOL(0x00000001),                // Weak external name
    ST_IS_SPLIT_COMMON(0x00000002),                // part of a splitted common
    ST_IS_NOT_USED(0x00000004),                // Symbol is not referenced
    ST_IS_INITIALIZED(0x00000008),                // Symbol is initialized
    ST_IS_RETURN_VAR(0x00000010),                // CLASS_VAR: Return value
    ST_IS_VALUE_PARM(0x00000020),                // CLASS_VAR: Value parm
    ST_PROMOTE_PARM(0x00000040),                // CLASS_VAR: Promote C formal
    ST_KEEP_NAME_W2F(0x00000080),                // CLASS_VAR: don't mangle name
    ST_IS_DATAPOOL(0x00000100),                // CLASS_VAR: represents datapool
    ST_IS_RESHAPED(0x00000200),                // lno may reshape the ST
    ST_EMIT_SYMBOL(0x00000400),                // emit the empty dummy symbol
    ST_HAS_NESTED_REF(0x00000800),                // has ref in nested pu
    ST_INIT_VALUE_ZERO(0x00001000),                // has initial value of zero
    ST_GPREL(0x00002000),                // force object to be gp-rel
    ST_NOT_GPREL(0x00004000),                // force object to not be gp-rel
    ST_IS_NAMELIST(0x00008000),                // namelist table
    ST_IS_F90_TARGET(0x00010000),                // F90 target
    ST_DECLARED_STATIC(0x00020000),                // VMS formals declared static
    ST_IS_EQUIVALENCED(0x00040000),                // is part of an equivalence
    ST_IS_FILL_ALIGN(0x00080000),                // has fill/align pragma.
    ST_IS_OPTIONAL_ARGUMENT(0x00100000), // F90 OPTIONAL arguments
    ST_PT_TO_UNIQUE_MEM(0x00200000),                // a pointer that is not aliased
    ST_IS_TEMP_VAR(0x00400000),                // compiler generated temp. variable
    ST_IS_CONST_VAR(0x00800000),                // read-only variable
    ST_ADDR_SAVED(0x01000000),                // address saved
    ST_ADDR_PASSED(0x02000000),                // address passed
    ST_IS_THREAD_PRIVATE(0x04000000),                // Symbol is allocated in XLOCAL
    // data segment
    ST_PT_TO_COMPILER_GENERATED_MEM(0x08000000),   // a pointer to compiler-
    // allocated memory space
    ST_IS_SHARED_AUTO(0x10000000),     // SCLASS_AUTO and accessed with
    // SHARED scope in an MP region
    ST_ASSIGNED_TO_DEDICATED_PREG(0x20000000),                // assigned to dedicated preg
    // specified in offset
    ST_ASM_FUNCTION_ST(0x40000000),                // ST is CLASS_NAME asm string with
    // file scope.
    ST_HAS_NAMED_SECTION(0x80000000);                // has named section attribute
    // new flags must go into flags_ext field.

    public final int internalId;

    STFlags(int id) {
      internalId = id;
    }

    public static STFlags fromInt(Long aLong) {
      for (STFlags i : values()) {
        if (i.internalId == aLong) {
          return i;
        }
      }
      assertThat(false).as("[WhirlConst::fromInt] cannot find counterpart for " + aLong).isTrue();
      return fromInt(0L);
    }

    public long toInt() {
      return internalId;
    }
  }

  enum STFlagsExt {
    ST_ONE_PER_PU(0x01),              // Only 1 instance per pu
    ST_COPY_CONSTRUCTOR_ST(0x02),     // ST is copy constructor function
    ST_INITV_IN_OTHER_ST(0x04),       // ST is being used as an initianliation offset by other symbol
    ST_IS_INITIALIZED_IN_F90(0x8),
    ST_IS_METHOD_FUNC(0x10),          // ST is c++ method function (make sense only when st-class is CLASS_FUNC
    ST_IS_THIS_PTR(0x20),             // ST is "this"-pointer
    ST_IS_PURE_VFUNC(0x40),           // ST is pure virtual function
    ST_IS_THREAD_LOCAL(0x80),         // ST is Thread-Local_Storage, __thread
    ST_IS_ARRAY_REMAPPING_CANDIDATE(0x100),
    ST_IS_ARRAY_REMAPPING_CANDIDATE_MALLOC(0x200), // storage for the remapped
    // array is from malloc()
    ST_JAVA_ABSTRACT(0x400),
    // ST_IN_V1BUF(0x400),                // ST is vector1 variable
    ST_IN_V2BUF(0x800),                // ST is vector2 variable
    ST_IN_V4BUF(0x1000),               // ST is vector4 variable
    ST_IN_SDRAM(0x2000),               // ST is sdram variable
    ST_IN_SBUF(0x4000),               // ST is explcitly declared sbuf so
    ST_IS_VBUF_OFFSET(0x8000),  // represent this symbol means offset instead of a absolute address
    ST_IS_SBUF_OFFSET(0x10000), // same as above and will be deleted for we don't have sbuf in the future.
    ST_IS_GLOBAL_AS_LOCAL(0x20000), // Is a global variable that can be treated as a local variable.
    ST_IS_VTABLE(0x40000),           //st is a vtalbe
    ST_IS_CLASS_SYMBOL(0x80000),       // st is class symbol
    ST_IS_CLASS_CONST_DATA(0x100000);  // st is class constant data symbol

    public final int internalId;

    STFlagsExt(int id) {
      internalId = id;
    }

    public static STFlagsExt fromInt(Long aLong) {
      for (STFlagsExt i : values()) {
        if (i.internalId == aLong) {
          return i;
        }
      }
      assertThat(false).as("[WhirlConst::fromInt] cannot find counterpart for " + aLong).isTrue();
      return fromInt(0L);
    }

    public long toInt() {
      return internalId;
    }
  }; // ST_FLAGS_EXT

  enum FlagKind {

    FLAG_EXPORT(1),
    FLAG_SCLASS(2),
    FLAG_TYFLAG(3),
    FLAG_STB(4),
    FLAG_SET_STFLAG(5),
    FLAG_CLEAR_STFLAG(6),
    FLAG_SET_STFLAG_EXT(7),
    FLAG_CLEAR_STFLAG_EXT(8),
    FLAG_ST_TYPE(9),
    FLAG_ST_SRCPOS(10);

    public final int internalId;

    FlagKind(int id) {
      internalId = id;
    }

    public static FlagKind fromInt(Long aLong) {
      for (FlagKind i: values()) {
        if(i.internalId == aLong){
          return i;
        }
      }
      assertThat(false).as("[WhirlConst::fromInt] cannot find counterpart for " + aLong).isTrue();
      return fromInt(0L);
    }

    public long toInt() {
      return internalId;
    }
  }

  public static enum SClass {
    SCLASS_UNKNOWN(0),
    SCLASS_AUTO(1),
    SCLASS_FORMAL(2),
    SCLASS_FORMAL_REF(3),
    SCLASS_PSTATIC(4), //preg
    SCLASS_FSTATIC(5),
    SCLASS_COMMON(6),
    SCLASS_EXTERN(7),
    SCLASS_UGLOBAL(8),
    SCLASS_DGLOBAL(9),
    SCLASS_TEXT(10),
    SCLASS_REG(11),
    SCLASS_CPLINIT(12),
    SCLASS_EH_REGION(13),
    SCLASS_EH_REGION_SUPP(14),
    SCLASS_DISTR_ARRAY(15),
    SCLASS_COMMENT(16),
    SCLASS_THREAD_PRIVATE_FUNCS(17),
    SCLASS_COUNT(18);

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

  public static enum TyAttr {
    TY_ATTR_NULL(0x0),
    TY_CONST(0x20),
    TY_VOLATILE(0x40);

    private final int typeAttr;

    TyAttr(int attr) {
      typeAttr = attr;
    }

    public int toInt() {
      return typeAttr;
    }

    public String toString() {
      return String.valueOf(typeAttr);
    }
  }

  public static enum TyFlag {
    TY_IS_CHARACTER(0x0001),
    TY_IS_LOGICAL(0x0002),
    TY_IS_UNION(0x0004),
    TY_IS_PACKED(0x0008),
    TY_PTR_AS_ARRAY(0x0010),
    TY_ANONYMOUS(0x0020),
    TY_SPLIT(0x0040),
    TY_IS_F90_POINTER(0x0080),
    TY_NOT_IN_UNION(0x0100),
    TY_NO_ANSI_ALIAS(0x0200),
    TY_IS_NON_POD(0x0400),
    TY_RETURN_IN_MEM(0x0800),
    TY_CONTENT_SEEN(0x1000),
    TY_IS_INCOMPLETE(0x2000),
    TY_NO_SPLIT(0x4000),
    TY_IS_ARRAY_CLASS(0x8000);

    private final int typeFlag;

    TyFlag(int flag) {
      typeFlag = flag;
    }

    public int toInt() {
      return typeFlag;
    }

    public String toString() {
      return String.valueOf(typeFlag);
    }
  }

  enum TyPuFlag {
    TY_RETURN_TO_PARAM(0x1),
    TY_IS_VARARGS(0x2),
    TY_HAS_PROTOTYPE(0x4);

    private final int typeFlag;

    TyPuFlag(int flag) {
      typeFlag = flag;
    }

    public int toInt() {
      return typeFlag;
    }

    public String toString() {
      return String.valueOf(typeFlag);
    }
  }

  enum SExport {
    EXPORT_LOCAL(0),        // Not exported, e.g. C static
    EXPORT_LOCAL_INTERNAL(1),  // Statics that do not have address // passed outside of a.out/DSO.
    EXPORT_INTERNAL(2),  // Exported, only visible and used within the containing DSO/executable, i.e. not even passed outside using a pointer.
    EXPORT_HIDDEN(3),  //Exported, but name is hidden within the containing DSO/executable.  However, the address may be exported from the DSO via a pointer.
    EXPORT_PROTECTED(4),  // Exported from DSO, but non preemptible
    EXPORT_PREEMPTIBLE(5), // Exported and preemptible. (GLOBAL, or INITOed)
    EXPORT_OPTIONAL(6),  //  STO_OPTIONAL case in "sys/elf.h"
    EXPORT_COUNT(7); // Must be last for consistency for checking

    private final int exportId;

    private SExport(int id) {
      exportId = id;
    }

    public int toInt() {
      return exportId;
    }

  }

  enum Mtype {
    // ----------------------------------------------------------------------
    // MTYPE
    // ----------------------------------------------------------------------
    MTYPE_B(1),
    MTYPE_I1(2),
    MTYPE_I2(3),
    MTYPE_I4(4),
    MTYPE_I8(5),
    MTYPE_U1(6),    /*   8-bit unsigned integer */
    MTYPE_U2(7),        /*  16-bit unsigned integer */
    MTYPE_U4(8),        /*  32-bit unsigned integer */
    MTYPE_U8(9),        /*  64-bit unsigned integer */

    MTYPE_F4(10),
    MTYPE_F8(11),

    MTYPE_F10(12),        /*  80-bit IEEE floating point */
    MTYPE_F16(13),        /* 128-bit IEEE floating point */

    MTYPE_STR(14),    /* char strings - TCONs only */
    MTYPE_STRING(14),
    MTYPE_FQ(15),        /* for SGI long double */
    MTYPE_M(16),        /* memory chunk, for structures */
    MTYPE_C4(17),    /* for 32-bit complex */
    MTYPE_C8(18),    /* for 64-bit complex */
    MTYPE_CQ(19),    /* for quad complex */
    MTYPE_V(20);   /* for void */

    private final int mtypeId;

    Mtype(int id) {
      mtypeId = id;
    }

    public static Mtype valueOf(int mType) {
      for(Mtype one : values()){
        if(one.mtypeId == mType){
          return one;
        }
      }
      assertThat(false).as("mtype cannot understand MTYPE_ID : " + mType).isTrue();
      return null;
    }

    public int toInt() {
      return mtypeId;
    }

    public String toString() {
      return String.valueOf(mtypeId);
    }
  }

  enum Operator {
    // ----------------------------------------------------------------------
    // OPERATOR
    // ----------------------------------------------------------------------
    OPR_ADD(2),     // add
    OPR_ASHR(8),    // arithmetic shift right
    OPR_BAND(11),   // binary and
    OPR_BIOR(12),   // binary or
    OPR_BXOR(16),   // binary xor
    OPR_CSELECT(27), // C_select (ternary)
    OPR_DIV(30),    // divide
    OPR_EQ(34),     // equal
    OPR_GE(42),     // great equal
    OPR_GT(44),     // great than
    OPR_INTRINSIC_CALL(56),
    OPR_INTRINSIC_OP(57),
    OPR_LE(68),     // less equal
    OPR_LSHR(73),   // logic shift right
    OPR_LT(74),     // less than
    OPR_MOD(82),    // mode
    OPR_MPY(83),    // multiply
    OPR_NE(86),     // not equal
    OPR_NEG(87),    // neg
    OPR_REM(104),   // remainder
    OPR_SELECT(109),// select (ternary)
    OPR_SHL(110),   // shift left
    OPR_SUB(114);   // sub

    private final int operatorId;

    Operator(int id) {
      operatorId = id;
    }

    public int toInt() {
      return operatorId;
    }

    public String toString() {
      return String.valueOf(operatorId);
    }
  }

  // TODO: can get real id through jni
  enum Intrinsic {
    INTRN_LCMP(1646),
    INTRN_FCMPL(1647),
    INTRN_FCMPG(1648),
    INTRN_DCMPL(1649),
    INTRN_DCMPG(1650),
    INTRN_FMOD(1651),
    INTRN_ALLOC_OBJ(1652),
    INTRN_LOOKUP_IF(1653),
    INTRN_NEW_PRIM_ARR(1654),
    INTRN_NEW_OBJ_ARR(1655),
    INTRN_INIT_CLASS(1656),
    INTRN_NEW_MULTI_ARR(1657),
    INTRN_THROW(1658),
    INTRN_ENTER_MONITOR(1659),
    INTRN_EXIT_MONITOR(1660),
    INTRN_INSTANCEOF(1661),
    INTRN_CHECK_CAST(1662),
    INTRN_THROW_ABS_ERR(1663),
    INTRN_GET_JNIENV(1664),
    INTRN_VA_ARG_PACK(1665),
    INTRN_VA_ARG_PACK_LEN(1666),
    INTRN_LOOKUP_VIRT_FUNC(1667);

    private final int id;

    Intrinsic(int id) {
      this.id = id;
    }

    public int toInt() {
      return id;
    }

    public String toString() {
      return String.valueOf(id);
    }
  }

  enum Flag {
    WN_PARM_BY_REFERENCE(0x01),
    WN_PARM_BY_VALUE(0x02);

    private final int flag;

    Flag(int flag) {
      this.flag = flag;
    }

    public int toInt() {
      return flag;
    }

    public String toString() {
      return String.valueOf(flag);
    }
  }

  enum CallFlags {
    WN_CALL_DOES_MEM_ALLOC(0x200),
    WN_CALL_IS_VIRTUAL(0x2000),
    WN_CALL_IS_INTERFACE(0x4000);

    private final int flag;

    CallFlags(int flag) {
      this.flag = flag;
    }

    public int toInt() {
      return flag;
    }

    public String toString() {
      return String.valueOf(flag);
    }
  }


static String ANON_PTR = "anon_ptr.";
  static class DefaultValue {
    static final int LEVEL = 1;
    static final int OFFSET = 0;
  }
}
