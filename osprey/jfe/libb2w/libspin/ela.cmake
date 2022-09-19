set(LOCAL_INC_DIR
  ${OSPREY_SRC_DIR}/common/com
  ${OSPREY_SRC_DIR}/common/com/${GLOBAL_TARGET_PREFIX}
  ${OSPREY_SRC_DIR}/ir_tools
  ${OSPREY_SRC_DIR}/common/util
  ${OSPREY_SRC_DIR}/be/com
  ${OSPREY_SRC_DIR}/be/opt
  ${OSPREY_SRC_DIR}/include
#  ${OSPREY_SRC_DIR}/macos/include
  ${BUILD_PLATFORM_INC_DIR}
  ${OSPREY_SRC_DIR}/../libspin
  ${OSPREY_SRC_DIR}/libdwarf/libdwarf
  ${OSPREY_SRC_DIR}/include/cmplrs
  ${BUILD_DIR}
  )
set(LOCAL_DEF
-DFE_GNU_4_2_0
 )
set(LOCAL_COMPILER_OPT -Wno-pointer-sign -Wno-switch)
set(LOCAL_C_OPT )
set(LOCAL_CPP_OPT )
