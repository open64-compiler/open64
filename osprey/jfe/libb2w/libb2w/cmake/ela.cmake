set(LOCAL_INC_DIR "")

if(BUILD_MACOS)
    set(LOCAL_INC_DIR ${OSPREY_FOR_PLATFORM_SRC_DIR}/macos/include
        ${OSPREY_FOR_PLATFORM_SRC_DIR}/common/com)
endif()

set(LOCAL_INC_DIR
  ${LOCAL_INC_DIR}
  ${OPENCC_CMAKE_DIR}/libb2w
  ${OSPREY_SRC_DIR}/../libspin
  ${OSPREY_SRC_DIR}/common/com/${BUILD_TARGET_PREFIX}
)

if(BUILD_MACOS)
    set(LOCAL_INC_DIR
        ${LOCAL_INC_DIR}
        $ENV{JAVA_HOME}/include/darwin
        $ENV{JAVA_HOME}/include
    )
endif()

if(BUILD_LINUX)
    set(LOCAL_INC_DIR
        ${LOCAL_INC_DIR}
        $ENV{JAVA_HOME}/include/linux
        $ENV{JAVA_HOME}/include
    )
endif()

set(LOCAL_INC_DIR 
  ${LOCAL_INC_DIR}
  ${OSPREY_SRC_DIR}/common/util/${BUILD_TARGET_PREFIX}
  ${OSPREY_SRC_DIR}/common/util
  ${OSPREY_SRC_DIR}/common/com
  ${OSPREY_SRC_DIR}/common/targ_info/access
  ${OSPREY_SRC_DIR}/wgen
  ${OSPREY_SRC_DIR}/ir_tools
  ${OSPREY_SRC_DIR}/be/com
  ${OSPREY_SRC_DIR}/be/opt
  ${OSPREY_SRC_DIR}/be/region
  ${OSPREY_SRC_DIR}/include
  ${BUILD_PLATFORM_INC_DIR}
#  ${OSPREY_SRC_DIR}/macos/include
  ${OSPREY_SRC_DIR}/ipa/main/analyze
  ${OSPREY_SRC_DIR}/libdwarf/libdwarf
#  ${OSPREY_SRC_DIR}/gnu/include
  ${BUILD_DIR}
)

message(STATUS " B2w Inc Dir : " ${LOCAL_INC_DIR})

set(LOCAL_DEF
#  -DIN_GCC
#  -DHAVE_CONFIG_H
#-D_SGI_SOURCE
#-D_LANGUAGE_C
  -DHAVE_CONFIG_H
  -DFRONT_END
#  -DMONGOOSE_BE
#  -DIR_TOOLS
  -DBUILD_NO_SIGNAL
  -DUSE_DECL_SRCPOS
  -DFE_GNU_4_2_0
  -DFRONT_END_C
  -DCFE
  -DIs_True_On
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
   -DHANDLE_PRAGMA_WEAK
   -DNEW_INITIALIZER
   -DDEBUG=1
   -DCHECKING=1
   -DGPLUSPLUS_FE
)

set(LOCAL_COMPILER_OPT -Wall -Wextra)
set(LOCAL_C_OPT )
set(LOCAL_CPP_OPT )
