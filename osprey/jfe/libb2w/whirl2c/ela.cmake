set(LOCAL_INC_DIR
  ${OSPREY_SRC_DIR}/common/com/${BUILD_TARGET_PREFIX}
  ${OSPREY_SRC_DIR}/common/util/${BUILD_TARGET_PREFIX}
  ${OSPREY_SRC_DIR}/common/util
  ${OSPREY_SRC_DIR}/common/com
  ${OSPREY_SRC_DIR}/common/targ_info
  ${OSPREY_SRC_DIR}/common/targ_info/access
  ${OSPREY_SRC_DIR}/be/whirl2c
  ${OSPREY_SRC_DIR}/be
  ${OSPREY_SRC_DIR}/be/be
  ${OSPREY_SRC_DIR}/be/com
  ${OSPREY_SRC_DIR}/be/cg
  ${OSPREY_SRC_DIR}/be/opt
  ${OSPREY_SRC_DIR}/be/lno
  ${OSPREY_SRC_DIR}/be/region
  ${OSPREY_SRC_DIR}/include
#  ${OSPREY_SRC_DIR}/macos/include
  ${BUILD_PLATFORM_INC_DIR}
  ${OSPREY_SRC_DIR}/ipa/main/analyze
  ${OSPREY_SRC_DIR}/ipa/common
  ${BUILD_DIR}
)

set(LOCAL_DEF
  -Dsgi
  -Dlonglong
  -D_LONGLONG -D_SVR4_SOURCE  #for ansi
  -DMONGOOSE_BE -DBUILD_WHIRL2C
  -DIs_True_On
  -DInsist_On
)

list(REMOVE_ITEM HOST_DEF -DTARG_64_DEF)

set(LOCAL_COMPILER_OPT )
set(LOCAL_C_OPT )
set(LOCAL_CPP_OPT )
