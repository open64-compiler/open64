option(BUILD_TARGET_X8664 "Is Target X8664 " ON)
if(BUILD_TARGET_X8664)
  message(STATUS "Target Platform: Building for x8664 ...")
  set(TARGET_DEF ${TARGET_DEF}
    -D__MIPS_AND_IA64_ELF_H
    -DTARG_64_DEF
    -DTARG_X8664)
  set(BUILD_TARGET_PREFIX x8664)
  set(TARGET_INC_DIR ${OPENCC_CMAKE_DIR}/x8664)
endif()

set(COMMON_COM_TARG_DIR ${OSPREY_SRC_DIR}/common/com/${BUILD_TARGET_PREFIX})
set(COMMON_BUILD_SRC ${TARGET_DIR})

if(CMAKE_COMPILER_IS_GNUCXX)
    #add_compile_options(-std=c++11)
    message(STATUS "GNU CXX: OK, using GNU CXX ...")   
endif(CMAKE_COMPILER_IS_GNUCXX)

set(HOST_INC_DIR
  #        /usr/local/Cellar/libelf/0.8.13_1/include/libelf
${TARGET_INC_DIR}
${CMAKE_BINARY_DIR}/include
/usr/include
/include
)

# Host Platform

set(HOST_C_FLAG -D_LANGUAGE_C)
set(HOST_CXX_FLAG -D_LANGUAGE_C_PLUS_PLUS -fpermissive)
#set(HOST_COMPILER_OPT -m64 -mtune=generic -march=x86-64
 # -funsigned-char -fPIC -fstack-protector-strong -Wformat -Wformat-security -Wno-constant-conversion
 # -Wno-c++11-compat-deprecated-writable-strings -Wno-dangling-else -Wno-array-bounds -Wno-return-type
 # -Wno-c++11-narrowing)

set(HOST_DEF
	-D_GNU_SOURCE
        -D_SGI_SOURCE
        -D__GNU_BUG_WORKAROUND
        -D_NOTHREADS
        -DVENDOR_OSP
	-DNO_VALGRIND
	-DInsist_On
        -DDEBUG_IR
        -DPSC_TO_OPEN64
        -DSHARED_BUILD
        -D_GNU_SOURCE
      #  -DIR_TOOLS
        -D_NEW_SYMTAB
        -D__STDC_LIMIT_MACROS
	-DDYNAMICLANG
        -DKEY
        -DOSP_OPT
        -DPATHSCALE_MERGE
        -DPSC_TO_OPEN64
        -DSHARED_BUILD
	${TARGET_DEF}
        -D_MIPSEL
        -D_LONGLONG
        -D_MIPS_SZINT=32
        -D_MIPS_SZPTR=64
        -D_MIPS_SZLONG=64
        -D_LP64)

option(DEBUG "is debug on?")
if(DEBUG)
  set(HOST_COMPILER_OPT ${HOST_COMPILER_OPT} -g -O0)
endif()


# Host(Building) Platform
MESSAGE(STATUS "###################################")
MESSAGE(STATUS "operation system is ${CMAKE_SYSTEM}")
IF (CMAKE_SYSTEM_NAME MATCHES "Linux")
  MESSAGE(STATUS "current platform: Linux ")
  set(BUILD_LINUX 1)
  set(BUILD_MACOS FALSE)
ELSEIF (CMAKE_SYSTEM_NAME MATCHES "Windows")
  MESSAGE(STATUS "current platform: Windows")
    set(BUILD_WINDOWS 1)
ELSEIF (CMAKE_SYSTEM_NAME MATCHES "FreeBSD")
  MESSAGE(STATUS "current platform: FreeBSD")
ELSE ()
  MESSAGE(STATUS "other platform: ${CMAKE_SYSTEM_NAME}")
  set(BUILD_MACOS 1)
  set(BUILD_LINUX FALSE)
ENDIF (CMAKE_SYSTEM_NAME MATCHES "Linux")

MESSAGE(STATUS "###################################")

if(BUILD_MACOS)

  set(HOST_DEF
    ${HOST_DEF}
    -DBUILD_OS_DARWIN=1
    -DBUILD_CXX11=1)

  message(STATUS "checking for buld os ...  DARWIN (macOS) ")

  set(OSPREY_FOR_PLATFORM_SRC_DIR ${OPENCC_CMAKE_DIR}/osprey-macos)
  set(BUILD_PLATFORM_INC_DIR
          ${OSPREY_FOR_PLATFORM_SRC_DIR}/macos/include
          ${OSPREY_FOR_PLATFORM_SRC_DIR}/common/com
          ${OSPREY_SRC_DIR}/macos/include
          ${OSPREY_SRC_DIR}/linux/include)
  set(HOST_C_FLAG -D_LANGUAGE_C)
  set(HOST_CXX_FLAG -D_LANGUAGE_C_PLUS_PLUS -fpermissive)
  set(HOST_COMPILER_OPT -m64 -mtune=generic -march=x86-64
  -funsigned-char -fPIC -fstack-protector-strong -Wformat -Wformat-security -Wno-constant-conversion
  -Wno-c++11-compat-deprecated-writable-strings -Wno-dangling-else -Wno-array-bounds -Wno-return-type
  -Wno-c++11-narrowing)

endif()

if(BUILD_LINUX)

  set(HOST_DEF
    ${HOST_DEF}
    -DBUILD_OS_LINUX
    -DBUILD_LINUX)
  set(HOST_C_FLAG -D_LANGUAGE_C)
  set(HOST_CXX_FLAG -D_LANGUAGE_C_PLUS_PLUS -fpermissive -Wno-conversion-null)
  set(HOST_COMPILER_OPT -m64 -mtune=generic -march=x86-64
  -funsigned-char -fPIC -fstack-protector-strong -Wformat -Wformat-security
  -Wno-array-bounds -Wno-return-type -Wno-deprecated
  -Wno-narrowing)

  set(BUILD_PLATFORM_INC_DIR ${OSPREY_SRC_DIR}/linux/include)
endif()


# Generation of bstring.h / pathscale_defs.h
file(STRINGS ${OSPREY_SRC_DIR}/../defs.mk OPEN64_DEFS)
set(OPEN64_MK_OUTPUTVARS ${OPEN64_DEFS})
#string(REGEX MATCHALL OPEN64_[a-zA-Z=0-9]* OPEN64_MK_OUTPUTVARS ${OPEN64_DEFS})
#message(STATUS "OPEN64_MK_OUTPUTVARS:" ${OPEN64_MK_OUTPUTVARS})
foreach(one_var IN LISTS OPEN64_MK_OUTPUTVARS)
    #message(STATUS "one_var:" ${one_var} "---")
    string(FIND ${one_var} = OPEN64_CFG_EQPOS)
    if(${OPEN64_CFG_EQPOS} GREATER 0)
      string(SUBSTRING ${one_var} 0 ${OPEN64_CFG_EQPOS} OPEN64_CFG_LHS)
      string(SUBSTRING ${one_var} ${OPEN64_CFG_EQPOS} -1 OPEN64_CFG_RHS)
      string(SUBSTRING ${OPEN64_CFG_RHS} 1 -1 OPEN64_CFG_RHS)
      # message(STATUS "Setting : [" ${OPEN64_CFG_LHS}  "] = [" ${OPEN64_CFG_RHS} "]")
      set(OPEN64_IN_${OPEN64_CFG_LHS} ${OPEN64_CFG_RHS})
    endif()
endforeach()

set(OPEN64_IN_OPEN64_FULL_VERSION "\"" ${OPEN64_IN_OPEN64_MAJOR_VERSION} . ${OPEN64_IN_OPEN64_MINOR_VERSION} "\"")

set(PATHSCALE_DEFSH
        " #ifndef __pathscale_defs_h "
        "\n #define __pathscale_defs_h "
        "\n #define OPEN64_NAME_PREFIX \"" ${OPEN64_IN_OPEN64_NAME_PREFIX} "\""
        "\n #define OPEN64_MAJOR_VERSION_NUM " ${OPEN64_IN_OPEN64_MAJOR_VERSION}
        "\n #define OPEN64_MINOR_VERSION_NUM " ${OPEN64_IN_OPEN64_MINOR_VERSION}
        "\n #define OPEN64_MAJOR_VERSION " ${OPEN64_IN_OPEN64_MAJOR_VERSION}
        "\n #define OPEN64_MINOR_VERSION " ${OPEN64_IN_OPEN64_MINOR_VERSION}
        "\n #define OPEN64_FULL_VERSION " ${OPEN64_IN_OPEN64_FULL_VERSION}
        "\n #define OPEN64_GCC_VERSION " ${OPEN64_IN_OPEN64_GCC_VERSION}
        "\n #define OPEN64_GCC40_VERSION " ${OPEN64_IN_OPEN64_GCC40_VERSION}
        "\n #define OPEN64_GCC42_VERSION " ${OPEN64_IN_OPEN64_GCC42_VERSION}
        "\n #define OPEN64_INSTALL_PREFIX " ${OPEN64_IN_OPEN64_INSTALL_PREFIX}
        "\n #define OPEN64_TARGET " ${OPEN64_IN_OPEN64_TARGET}
        "\n #define OPEN64_PATCH_LEVEL " ${OPEN64_IN_OPEN64_PATCH_LEVEL}
        "\n #endif" )
file(WRITE ${CMAKE_BINARY_DIR}/include/pathscale_defs.h ${PATHSCALE_DEFSH})

macro(get_osprey_file_path one_file_path dir_path output_var_list)
  set(TEMP_NOTFOUND_ANY TRUE)
  if(EXISTS ${OPENCC_CMAKE_DIR}/${dir_path}/${one_file_path})
    list(APPEND ${output_var_list} ${OPENCC_CMAKE_DIR}/${dir_path}/${one_file_path})
    set(TEMP_NOTFOUND_ANY FALSE)
    if(DEBUG_OSPREY_GETFILE)
      message(STATUS "1->" ${output_var_list} "++" ${OPENCC_CMAKE_DIR}/${dir_path}/${one_file_path})
    endif()
  endif()
  if(BUILD_MACOS)
    if(EXISTS ${OSPREY_FOR_PLATFORM_SRC_DIR}/${dir_path}/${one_file_path})
      list(APPEND ${output_var_list} ${OSPREY_FOR_PLATFORM_SRC_DIR}/${dir_path}/${one_file_path})
      set(TEMP_NOTFOUND_ANY FALSE)
      if(DEBUG_OSPREY_GETFILE)
      message(STATUS "2->" ${output_var_list} "++" ${OSPREY_FOR_PLATFORM_SRC_DIR}/${dir_path}/${one_file_path})
      endif()
    endif()
  endif()
  if(${TEMP_NOTFOUND_ANY})
    list(APPEND ${output_var_list} ${OSPREY_SRC_DIR}/${dir_path}/${one_file_path})
  else()
    if(DEBUG_OSPREY_GETFILE)
    message(STATUS "FOUNDED!")
    endif()
  endif()
endmacro(get_osprey_file_path)
