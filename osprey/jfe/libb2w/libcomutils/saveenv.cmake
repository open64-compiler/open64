add_library(comutils
  ${SHARED_BUILD}
  ${COM_UTILS_SRCS}
)
target_compile_options(comutils PUBLIC ${TEMP_C_OPT} ${HOST_COMPILER_OPT} ${LOCAL_COMPLER_OPT} ${HOST_C_OPT} ${LOCAL_C_OPT})
target_include_directories(comutils PUBLIC ${HOST_INC_DIR} ${LOCAL_INC_DIR})
target_compile_definitions(comutils PUBLIC ${HOST_DEF} ${LOCAL_DEF})
