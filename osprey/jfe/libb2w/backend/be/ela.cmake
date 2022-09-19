
if(BUILD_LINUX)
    set(LINUX_BINARY_GMAKE_DIR /home/xc5/xc5/build)

endif()

set(LOCAL_INC_DIR
  ${OSPREY_SRC_DIR}/common/com/${BUILD_TARGET_PREFIX}
  ${OSPREY_SRC_DIR}/common/util/${BUILD_TARGET_PREFIX}
  ${OSPREY_SRC_DIR}/common/util
  ${OSPREY_SRC_DIR}/common/com
  ${OSPREY_SRC_DIR}/common/targ_info/access
  ${OSPREY_SRC_DIR}/be/be
  ${OSPREY_SRC_DIR}/be/vho
    ${OSPREY_SRC_DIR}/be/cg
  ${OSPREY_SRC_DIR}/ir_tools
  ${OSPREY_SRC_DIR}/be/com
  ${OSPREY_SRC_DIR}/be/opt
  ${OSPREY_SRC_DIR}/be/lno
    ${OSPREY_SRC_DIR}/be/whirl2c
    ${OSPREY_SRC_DIR}/be/whirl2f
  ${OSPREY_SRC_DIR}/be/region
  ${OSPREY_SRC_DIR}/include
  ${OSPREY_SRC_DIR}/ipa/analyze
  ${OSPREY_SRC_DIR}/ipa/local
  ${OSPREY_SRC_DIR}/ipa/optimize
  ${OSPREY_SRC_DIR}/ipa/common
  ${BUILD_PLATFORM_INC_DIR}
  ${OSPREY_SRC_DIR}/ipa/main/analyze
  ${OSPREY_SRC_DIR}/libdwarf/libdwarf
#  ${OSPREY_SRC_DIR}/gnu/include
  ${LINUX_BINARY_GMAKE_DIR}/osprey/targdir/targ_info
  ${LINUX_BINARY_GMAKE_DIR}/osprey/targdir/be
  ${BUILD_DIR}
)

set(LOCAL_DEF
   #  BE Specific
   -DNO_VALGRIND
    -D_LEGO_CLONER
    -DBACK_END
    -Dlonglong
    -DSTD_MONGOOSE_LOC='HEREISMONGOOSE_LOC'
    -DMONGOOSE_BE
    # need to define _LONGLONG and _SVR4_SOURCE to allow compilation with -ansi.
    -D_LONGLONG -D_SVR4_SOURCE
    -D_NEW_SYMTAB
    -D__MIPS_AND_IA64_ELF_H
    -DIs_True_On
    -D_LANGUAGE_C_PLUS_PLUS
    -DUSE_PCH
 #   -DDEBUG=1
    -DCHECKING=1
    -DGPLUSPLUS_FE
    -DHANDLE_PRAGMA_WEAK
    -DNEW_INITIALIZER
)

set(ELA_BE_BE_OPT -DHAVE_CONFIG_H
        -DFRONT_END
        -DUSE_DECL_SRCPOS
        -DFE_GNU_4_2_0
        -DFRONT_END_C
        -DCFE
        -DCIL
        -DDO_IL_LOWERING=0
        -DNO_USR_INCLUDE=TRUE
        -DAUTOMATIC_TEMPLATE_INSTANTIATION=0
        -DINSTANTIATION_BY_IMPLICIT_INCLUSION=0
        -DBACK_END_IS_C_GEN_BE=0
        -DMONGOOSE_CIF
        #  -DBACK_END
        -DSGI_RAG_BACKEND
        -DSGI_MONGOOSE
        )

set(LOCAL_COMPILER_OPT )
set(LOCAL_C_OPT )
set(LOCAL_CPP_OPT )
