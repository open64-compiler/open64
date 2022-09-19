set(GLOBAL_TARGET_PREFIX x8664)

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
  ${TARGET_DIR}
  ${TARGET_DIR}/include
)

set(LOCAL_DEF )

if(DEBUG)
  set(LOCAL_DEF ${LOCAL_DEF} -DIs_True_On -DInsist_On)
endif()

set(LOCAL_COMPILER_OPT
  -Wno-logical-op-parentheses
  -Wno-macro-redefined
  -Wno-switch
  -Wno-deprecated-declarations
  -Wno-shift-op-parentheses
  -Wno-empty-body)
set(LOCAL_C_OPT )
set(LOCAL_CPP_OPT )
