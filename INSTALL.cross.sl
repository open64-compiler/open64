#!/bin/bash
#
#  The installation script for open64-SL compiler.
#
#  Copyright (C) 2006-2011 Simplight Nanoelectronics, Inc. All Rights Reserved.
#
#  Copyright (C) 2000 Silicon Graphics, Inc.  All Rights Reserved.
#
#  Open64 is free software; you can redistribute it and/or modify it 
#  under the terms of the GNU General Public License as published by 
#  the Free Software Foundation; either version 2 of the License, 
#  or (at your option) any later version.

#  Open64 is distributed in the hope that it will be useful, but 
#  WITHOUT ANY WARRANTY; without even the implied warranty of
#  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
#  GNU General Public License for more details.

#  You should have received a copy of the GNU General Public License along 
#  with this program; if not, write to the Free SoftwareFoundation, Inc., 
#  51 Franklin Street, Fifth Floor, Boston, MA  02110-1301, USA.
#
echo $TOOLROOT
abs_top_builddir=$1


# everything we will install is under $ROOT
ROOT=${TOOLROOT}/


AREA="$abs_top_builddir/osprey/targdir/"
AREA_LIB="$abs_top_builddir/osprey/targdir_lib/"
AREA_CYGNUS="$abs_top_builddir/osprey/cygnus/"
GNUFE42_AREA="$abs_top_builddir/osprey-gcc-4.2.0/"

PHASEPATH=${ROOT}/usr/bin
INSTR_LIB_DIR=${COMP_TARGET_ROOT}/usr/lib
BIN_DIR=${ROOT}/usr/bin
ALT_BIN_DIR=${ROOT}/usr/altbin

INSTALL="/usr/bin/install -D"
INSTALL_DATA="/usr/bin/install -D -m 644"

INSTALL_EXEC_SUB () {

    [ $# -ne 2 ] && echo "Usage: INSTALL_EXEC_SUB SRC_file DEST_file" && return 1
    
    [ ! -e "$1" ] && echo "$1 does not exist" && return 1

    echo -e "$2 : $1 \n\t${INSTALL} $1 $2\n" | make -f - |\
    grep -v "Entering directory\|Leaving directory\|up to date"

    return 0;
}

INSTALL_DATA_SUB () {

    [ $# -ne 2 ] && echo "Usage: INSTALL_DATA_SUB SRC_file DEST_file" && exit 1

    [ ! -e "$1" ] && echo "$1 does not exist" && return 1

    echo -e "$2 : $1 \n\t${INSTALL_DATA} $1 $2\n" | make -f - |\
    grep -v "Entering directory\|Leaving directory\|up to date"

    return 0
}


INSTALL_DRIVER () {
    INSTALL_EXEC_SUB ${AREA}/driver/driver  ${PHASEPATH}/driver

    [ ! -d ${BIN_DIR}       ] && mkdir -p ${BIN_DIR}
    [ ! -e ${BIN_DIR}/mipsel-elf-linux-g++  ] && ln -sf driver ${BIN_DIR}/mipsel-elf-linux-g++
    [ ! -e ${BIN_DIR}/slCC ] && ln -sf driver ${BIN_DIR}/slCC
    [ ! -e ${BIN_DIR}/mipsel-elf-linux-gcc  ] && ln -sf driver ${BIN_DIR}/mipsel-elf-linux-gcc
    [ ! -e ${BIN_DIR}/slcc ] && ln -sf driver ${BIN_DIR}/slcc
    return 0
}

   # Install front-end components
INSTALL_FE () {
    [ ! -e ${ALT_BIN_DIR} ] && mkdir -p ${ALT_BIN_DIR}
    INSTALL_EXEC_SUB ${AREA}/gccfe/gfec  ${PHASEPATH}/gfec
    INSTALL_EXEC_SUB ${AREA}/g++fe/gfecc ${PHASEPATH}/gfecc

    # GNU 4.2.0 based FE
    INSTALL_EXEC_SUB ${AREA}/wgen/wgen42 ${PHASEPATH}/wgen42
    INSTALL_EXEC_SUB ${GNUFE42_AREA}/gcc/cc1 ${PHASEPATH}/cc142
    INSTALL_EXEC_SUB ${GNUFE42_AREA}/gcc/cc1 ${ALT_BIN_DIR}/cc1
    INSTALL_EXEC_SUB ${GNUFE42_AREA}/gcc/cc1plus ${PHASEPATH}/cc1plus42
    INSTALL_EXEC_SUB ${GNUFE42_AREA}/gcc/cc1plus ${ALT_BIN_DIR}/cc1plus
    INSTALL_EXEC_SUB ${GNUFE42_AREA}/gcc/xgcc ${ALT_BIN_DIR}/gcc
    INSTALL_EXEC_SUB ${GNUFE42_AREA}/gcc/g++ ${ALT_BIN_DIR}/g++
    INSTALL_EXEC_SUB ${GNUFE42_AREA}/gcc/cpp ${ALT_BIN_DIR}/cpp
    return 0
}

  # Install back-end components 
INSTALL_BE () {
    INSTALL_EXEC_SUB ${AREA}/be/be  ${PHASEPATH}/be
    INSTALL_EXEC_SUB ${AREA}/be/be.so ${PHASEPATH}/be.so

    return 0
}

  # Install IPA-related components
INSTALL_IPA () {

    INSTALL_EXEC_SUB ${AREA}/ipa/ipa.so ${PHASEPATH}/ipa.so
    INSTALL_EXEC_SUB ${AREA}/ipl/ipl.so ${PHASEPATH}/ipl.so

    INSTALL_EXEC_SUB ${AREA_CYGNUS}/ld/ld-new  ${PHASEPATH}/ipa_link
    ln -sf be ${PHASEPATH}/ipl 

    return 0
}

INSTALL_CG () {
    INSTALL_EXEC_SUB ${AREA}/cg/cg.so                ${PHASEPATH}/cg.so
    return 0
}

INSTALL_WHIRL_STUFF () {

    INSTALL_EXEC_SUB  ${AREA}/whirl2c/whirl2c   ${PHASEPATH}/whirl2c
    INSTALL_EXEC_SUB  ${AREA}/whirl2c/whirl2c.so ${PHASEPATH}/whirl2c.so

    (cd ${PHASEPATH}; ln -sf be whirl2c_be) 

    INSTALL_EXEC_SUB  ${AREA}/ir_tools/ir_b2a    ${BIN_DIR}/ir_b2a

    return 0
}

INSTALL_NATIVE_HEADER () {

    INSTALL_DATA_SUB osprey/include/nue/stdarg.h  ${PHASEPATH}/include/stdarg.h
    INSTALL_DATA_SUB osprey/include/nue/va-ia64.h  ${PHASEPATH}/include/va-ia64.h 

    cp -f -a osprey/include ${PHASEPATH}/ 

    return 0
}


INSTALL_MISC () {

    ### INSTALL .so 
    INSTALL_EXEC_SUB ${AREA}/wopt/wopt.so         ${PHASEPATH}/wopt.so
    INSTALL_EXEC_SUB ${AREA}/lw_inline/lw_inline  ${PHASEPATH}/inline
    INSTALL_EXEC_SUB ${AREA}/lno/lno.so           ${PHASEPATH}/lno.so

    return 0
}

cd `dirname $0`

INSTALL_DRIVER 
INSTALL_FE 
INSTALL_BE 
INSTALL_IPA 
INSTALL_CG 
INSTALL_WHIRL_STUFF 
INSTALL_NATIVE_HEADER
INSTALL_MISC

exit 0
