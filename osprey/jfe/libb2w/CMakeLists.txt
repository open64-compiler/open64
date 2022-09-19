cmake_minimum_required(VERSION 3.4)
project(osprey)

message(STATUS "=================================")
message(STATUS "   Start to build OPENCC CMake   ")
message(STATUS "=================================")

set(CMAKE_CXX_STANDARD 14)
set(CMAKE_C_STANDARD 11)

set(OPENCC_CMAKE_DIR ${CMAKE_CURRENT_SOURCE_DIR})

set(GLOBAL_OSPREY_LIB
  libcomutils
  libcmplrs
  libiberty
  libspin
  jsoncpp
)

set(GLOBAL_OSPREY_TARG
  irtools
  wgen
#  whirl2c
  jgen
  libb2w  
  wgen-java
)



foreach(ONE_TARG IN LISTS GLOBAL_OSPREY_LIB)
  add_subdirectory(${ONE_TARG})
endforeach()

foreach(ONE_TARG IN LISTS GLOBAL_OSPREY_TARG)
  add_subdirectory(${ONE_TARG})
endforeach()
