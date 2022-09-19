cmake_minimum_required(VERSION 3.4)
project(osprey)

message(STATUS "=================================")
message(STATUS "   Start to build LIBB2W   ")
message(STATUS "=================================")

set(CMAKE_CXX_STANDARD 14)
set(CMAKE_C_STANDARD 11)

#Global DIR
include(../global/dir.cmake)

#Global VAR
include(../global/global.cmake)

#Global SRC
include(../global/sharedsf.cmake)


#Local VAR
include(cmake/ela.cmake)

#Local DIR
include(cmake/local.cmake)

#Local Source
include(cmake/localsf.cmake)

#Making Progess
include(cmake/saveenv.cmake)
