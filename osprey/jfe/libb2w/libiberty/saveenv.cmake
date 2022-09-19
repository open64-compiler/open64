add_library(iberty ${IBERTY_SRC})
target_compile_options(iberty PUBLIC ${TEMP_C_OPT} ${HOST_COMPILER_OPT} ${LOCAL_COMPLER_OPT} ${HOST_C_OPT} ${LOCAL_C_OPT})
target_include_directories(iberty PUBLIC ${HOST_INC_DIR} ${LOCAL_INC_DIR})
target_compile_definitions(iberty PUBLIC ${HOST_DEF} ${LOCAL_DEF})
