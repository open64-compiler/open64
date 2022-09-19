set(IR_TOOL_MIDDLEWARE_NAME irtoolcore)

add_library(${IR_TOOL_MIDDLEWARE_NAME} ${IR_TOOLS_SRC})
target_link_libraries(${IR_TOOL_MIDDLEWARE_NAME} comutils)
target_compile_options(${IR_TOOL_MIDDLEWARE_NAME} PUBLIC ${TEMP_C_OPT} ${HOST_COMPILER_OPT} ${LOCAL_COMPLER_OPT} ${HOST_C_OPT} ${LOCAL_C_OPT})
target_include_directories(${IR_TOOL_MIDDLEWARE_NAME} PUBLIC ${HOST_INC_DIR} ${LOCAL_INC_DIR} ${JSONCPP_INCLUDE_DIRS})
target_compile_definitions(${IR_TOOL_MIDDLEWARE_NAME} PUBLIC ${IR_TOOLS_DEF} ${HOST_DEF} ${LOCAL_DEF})



add_executable(ir_size ${IR_SIZE_SRC})
add_executable(ir_b2a ${IR_A2B_SRC})
# add_executable(ir_walker ${IR_WALKER_SRC})

set(IR_TARGETS
  ir_size
  ir_b2a
  #  ir_walker
)

#
#ExternalProject_Add(libjson_ex
#    URL ../../../libjson
#    PREFIX ${CMAKE_CURRENT_BINARY_DIR}/libjson
#    CMAKE_ARGS -DCMAKE_INSTALL_PREFIX:PATH=<INSTALL_DIR>)
#
##include(${CMAKE_CURRENT_BINARY_DIR}/lib/project_a/project_a-targets.cmake)
#
#ExternalProject_Get_Property(libjson_ex install_dir)
#include_directories(${install_dir}/include)

foreach(ONE_TARG IN LISTS IR_TARGETS)
  target_link_libraries(${ONE_TARG} ${IR_TOOL_MIDDLEWARE_NAME} ${JSONCPP_LIBRARIES} ${IR_TOOL_JSONCPP})
  target_compile_options(${ONE_TARG} PUBLIC ${TEMP_C_OPT} ${HOST_COMPILER_OPT} ${LOCAL_COMPLER_OPT} ${HOST_C_OPT} ${LOCAL_C_OPT})
  target_include_directories(${ONE_TARG} PUBLIC ${HOST_INC_DIR} ${LOCAL_INC_DIR})
  target_compile_definitions(${ONE_TARG} PUBLIC ${IR_TOOLS_DEF} ${HOST_DEF} ${LOCAL_DEF})
  add_dependencies(${ONE_TARG} ${IR_TOOLS_DEPENDENCY})
  #target_link_libraries(${ONE_TARG} ${install_dir}/libjson.)
endforeach()

# -- backup --
#add_headers_and_sources(ir_b2a $)
