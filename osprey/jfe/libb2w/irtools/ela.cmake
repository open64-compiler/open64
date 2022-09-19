set(LOCAL_INC_DIR
  ${BUILD_PLATFORM_INC_DIR}
  ${OSPREY_SRC_DIR}/common/com/${GLOBAL_TARGET_PREFIX}
 # ${OSPREY_SRC_DIR}/macos/include
  ${OSPREY_SRC_DIR}/libdwarf/libdwarf
#  ${OSPREY_SRC_DIR}/libelf/lib
  ${OSPREY_SRC_DIR}/common/com
  ${OSPREY_SRC_DIR}/ir_tools
  ${OSPREY_SRC_DIR}/common/util
  ${OSPREY_SRC_DIR}/be/com
  ${OSPREY_SRC_DIR}/be/opt
  ${OSPREY_SRC_DIR}/include
  ${OSPREY_SRC_DIR}/libjson/include
  ${TARGET_DIR}
  ${TARGET_DIR}/include
)
set(LOCAL_DEF
  -DMONGOOSE_BE
  -DIR_TOOLS
  )
set(LOCAL_COMPILER_OPT -Wno-c++11-narrowing
  -Wno-parentheses)
set(LOCAL_C_OPT )
set(LOCAL_CPP_OPT )

set(IR_TOOLS_DEPENDENCY jsoncpp_lib_static)
set(IR_TOOL_JSONCPP jsoncpp_lib_static)
#find_package(PkgConfig REQUIRED)
#pkg_check_modules(JSONCPP jsoncpp)