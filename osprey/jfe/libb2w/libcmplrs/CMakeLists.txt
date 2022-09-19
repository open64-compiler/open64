cmake_minimum_required(VERSION 3.4)
project(osprey)

set(CMAKE_CXX_STANDARD 11)
set(CMAKE_C_STANDARD 11)

#Global DIR
include(../global/dir.cmake)

#Global VAR
include(../global/global.cmake)

#Global SRC
include(../global/sharedsf.cmake)

#Local VAR
include(ela.cmake)

#Local DIR
include(local.cmake)

#Local Source
include(localsf.cmake)

#Making Progess
include(saveenv.cmake)
