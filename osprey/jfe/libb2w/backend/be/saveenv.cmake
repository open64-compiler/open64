add_executable(bebe ${BE_BE_OUT_SRCS})
target_compile_options(bebe PUBLIC ${TEMP_C_OPT} ${HOST_COMPILER_OPT} ${LOCAL_COMPLER_OPT} ${HOST_C_OPT} ${LOCAL_C_OPT})
target_include_directories(bebe PUBLIC ${LOCAL_INC_DIR} ${HOST_INC_DIR})
target_compile_definitions(bebe PRIVATE ${HOST_DEF} ${LOCAL_DEF} -DMONGOOSE_BE)

add_library(ldplugin ${LD_PLUGIN_SRCS})
target_compile_options(ldplugin PUBLIC ${TEMP_C_OPT} ${HOST_COMPILER_OPT} ${LOCAL_COMPLER_OPT} ${HOST_C_OPT} ${LOCAL_C_OPT})
target_include_directories(ldplugin PUBLIC ${LOCAL_INC_DIR} ${HOST_INC_DIR})
target_compile_definitions(ldplugin PUBLIC ${HOST_DEF} ${LOCAL_DEF})
target_link_libraries(ldplugin comutils spin cmplrs iberty)

