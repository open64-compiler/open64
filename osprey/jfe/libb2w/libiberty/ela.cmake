set(LOCAL_INC_DIR
  ${OSPREY_SRC_DIR}/common/com
  ${OSPREY_SRC_DIR}/common/com/${GLOBAL_TARGET_PREFIX}
  ${OSPREY_SRC_DIR}/ir_tools
  ${OSPREY_SRC_DIR}/common/util
  ${OSPREY_SRC_DIR}/be/com
  ${OSPREY_SRC_DIR}/be/opt
  ${OSPREY_SRC_DIR}/include
  #${OSPREY_SRC_DIR}/macos/include
  ${BUILD_PLATFORM_INC_DIR}
  ${OSPREY_SRC_DIR}/../libspin
  ${OSPREY_SRC_DIR}/include/gnu
  )
set(LOCAL_DEF -DHAVE_SYS_STAT_H=1
  -DHAVE_STDLIB_H=1
  -DHAVE_STRING_H=1
#  -DHAVE_CONFIG_H=1
  )
set(LOCAL_COMPILER_OPT
  -Wno-implicit-function-declaration)
set(LOCAL_C_OPT )
set(LOCAL_CPP_OPT )
