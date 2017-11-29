#!/bin/bash
#
#
#  Copyright (C) 2008-2010 Advanced Micro Devices, Inc.  All Rights Reserved.
#
#  Copyright (C) 2000 Silicon Graphics, Inc.  All Rights Reserved.
#
#  This program is free software; you can redistribute it and/or modify it
#  under the terms of version 2 of the GNU General Public License as
#  published by the Free Software Foundation.
#
#  This program is distributed in the hope that it would be useful, but
#  WITHOUT ANY WARRANTY; without even the implied warranty of
#  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  
#
#  Further, this software is distributed without any warranty that it is
#  free of the rightful claim of any third person regarding infringement 
#  or the like.  Any license provided herein, whether implied or 
#  otherwise, applies only to this software file.  Patent licenses, if 
#  any, provided herein do not apply to combinations of this program with 
#  other software, or any other product whatsoever.  
#
#  You should have received a copy of the GNU General Public License along
#  with this program; if not, write the Free Software Foundation, Inc., 59
#  Temple Place - Suite 330, Boston MA 02111-1307, USA.
#
#  Contact information:  Silicon Graphics, Inc., 1600 Amphitheatre Pky,
#  Mountain View, CA 94043, or:
#
#  http://www.sgi.com
#
#  For further information regarding this notice, see:
#
#  http://oss.sgi.com/projects/GenInfo/NoticeExplan
#
#

VER_MAJOR="5"
VER_MINOR="0"
#PATCH_LEVEL=""
VERSION="${OPEN64_FULL_VERSION:-${VER_MAJOR}.${VER_MINOR}}"

PREBUILT_LIB="./lib"
PREBUILT_BIN="./bin"

# get the machine type
if [ -z "$1" ]; then
    ARCH=`uname -m | sed -e s/i.86/i386/`
else
    ARCH=$1
fi

if [ -z "$2" ]; then
    INSTALL_FORTRAN="YES"
else
    INSTALL_FORTRAN=$2
fi

if [ -z "$3" ]; then
	CROSS_TARGET=""
	if [ $1 = "PPC32" ]; then
		ARCH="ppc"
	fi
else
    CROSS_TARGET=$3
fi

# set the build host
case $ARCH in 
ia64 )
    BUILD_HOST="ia64"
    TARG_HOST="ia64"
    AREA="osprey/targia64_ia64_nodebug"
    PHASE_DIR_PREFIX="ia64"
    PREBUILD_INTERPOS="ia64-linux"
    INSTALL_TYPE="ia64-native"
    ;;
i386 )
    BUILD_HOST="ia32"
    TARG_HOST="x8664"
    PHASE_DIR_PREFIX="x86_64"
    PREBUILD_INTERPOS="x8664-linux"
    AREA="osprey/targia32_x8664"
    INSTALL_TYPE="x8664-native"
    ;;
x86_64 )
    BUILD_HOST="x8664"
    TARG_HOST="x8664"
    PHASE_DIR_PREFIX="x86_64"
    PREBUILD_INTERPOS="x8664-linux"
    AREA="osprey/targx8664_x8664"
    INSTALL_TYPE="x8664-native"
    ;;
ppc )
    BUILD_HOST="ppc32"
    TARG_HOST="ppc32"
    INSTALL_FORTRAN="NO"
    AREA="osprey/targppc32_ppc32"
    PHASE_DIR_PREFIX="ppc32"
    PREBUILD_INTERPOS="ppc32-linux"
    INSTALL_TYPE="ppc32-native"
    ;;
PPC32 )			
    BUILD_HOST="ia32"
    TARG_HOST="ppc32"
    INSTALL_FORTRAN="NO"
    AREA="osprey/targia32_ppc32"
    PHASE_DIR_PREFIX="ppc32"
    PREBUILD_INTERPOS="ppc32-linux"
    INSTALL_TYPE="ppc32-cross"
    ;;
cross )
    BUILD_HOST="ia32"
    TARG_HOST="ia64"
    PHASE_DIR_PREFIX="ia64"
    PREBUILD_INTERPOS="ia32-linux"
    AREA="osprey/targia32_ia64_nodebug"
    INSTALL_TYPE="ia64-cross"
    ;;
*)
    echo "Error: Unsupport platform: $ARCH"
    exit 1
    ;;
esac

AREA="osprey/targdir"

# get the TOOLROOT
if [ -z ${TOOLROOT} ] ; then
    echo "NOTE: \$TOOLROOT is not set! You can either set \$TOOLROOT or specify an install directory."
    echo "INSTALL DIRECTORY:"
    read	# in $REPLY
    [ ! -d $REPLY ] && echo "$REPLY does not exist. Will create." && mkdir -p $REPLY
    [ ! -d $REPLY ] && echo "Can not create directory: $REPLY, exit." && exit 1
    ORIGIN_DIR=`pwd`
    cd $REPLY
    TOOLROOT=`pwd`
    cd $ORIGIN_DIR
    echo "INSTALL to $TOOLROOT"
fi 

# everything we will install is under $ROOT
ROOT=${TOOLROOT}/

# both targ_os and build_os are 'linux' so far
TARG_OS="linux"
BUILD_OS="linux"

# prepare the source dir
GNUFE_AREA="osprey-gcc"
GNUFE42_AREA="osprey-gcc-4.2.0"
LD_NEW_DIR="osprey/cygnus/ld"

# prepare the distination dir
INTERPOSE=
[ "$BUILD_HOST" = "$TARG_HOST" ] &&  INTERPOSE="" ; 
PHASEPATH=${ROOT}/${INTERPOSE}/lib/gcc-lib/${PHASE_DIR_PREFIX}-open64-linux/${VERSION}/
NATIVE_LIB_DIR=${PHASEPATH}
BIN_DIR=${ROOT}/${INTERPOSE}/bin
ALT_BIN_DIR=${ROOT}/${INTERPOSE}/altbin

# install commands
INSTALL="/usr/bin/install -D"
INSTALL_DATA="/usr/bin/install -D -m 644"

INSTALL_EXEC_SUB () {

    [ $# -ne 2 ] && echo "!!!Component is missing, you probably need to install prebuilt binaries/archives" && return 1
    
    [ ! -e "$1" ] && echo "$1 does not exist" && return 1

    echo -e "$2 : $1 \n\t${INSTALL} $1 $2\n" | make -f - |\
    grep -v "Entering directory\|Leaving directory\|up to date"

    return 0;
}

INSTALL_DATA_SUB () {

    [ $# -ne 2 ] && echo "!!!Component is missing, you probably need to install prebuilt binaries/archives" && return 1

    [ ! -e "$1" ] && echo "$1 does not exist" && return 1

    echo -e "$2 : $1 \n\t${INSTALL_DATA} $1 $2\n" | make -f - |\
    grep -v "Entering directory\|Leaving directory\|up to date"

    return 0
}

# install the driver
INSTALL_DRIVER () {
    INSTALL_EXEC_SUB ${AREA}/driver/driver  ${PHASEPATH}/driver
    if [ "$TARG_HOST" = "ia64" ] || [ "$TARG_HOST" = "x8664" ]; then
      INSTALL_EXEC_SUB ${TOP_SRCDIR}/osprey/targdir/driver/kdriver  ${PHASEPATH}/kdriver
    fi
    if [ "$TARG_HOST" = "ppc32" ]; then
      INSTALL_EXEC_SUB ${TOP_SRCDIR}/osprey/targdir/driver/kdriver  ${PHASEPATH}/kdriver
    fi

    [ ! -d ${BIN_DIR}       ] && mkdir -p ${BIN_DIR}
    if [ "$ARCH" = "PPC32" ]; then
    INSTALL_EXEC_SUB ${AREA}/driver/driver  ${BIN_DIR}/powercc
    INSTALL_EXEC_SUB ${AREA}/driver/driver  ${BIN_DIR}/powercc-${VERSION}
    else
    INSTALL_EXEC_SUB ${AREA}/driver/driver  ${BIN_DIR}/opencc
    INSTALL_EXEC_SUB ${AREA}/driver/driver  ${BIN_DIR}/openCC
    [ "$INSTALL_FORTRAN" = "YES" ] && INSTALL_EXEC_SUB ${AREA}/driver/driver  ${BIN_DIR}/openf90
    [ "$INSTALL_FORTRAN" = "YES" ] && INSTALL_EXEC_SUB ${AREA}/driver/driver  ${BIN_DIR}/openf95
    INSTALL_EXEC_SUB ${AREA}/driver/driver  ${BIN_DIR}/opencc-${VERSION}
    INSTALL_EXEC_SUB ${AREA}/driver/driver  ${BIN_DIR}/openCC-${VERSION}
    [ "$INSTALL_FORTRAN" = "YES" ] && INSTALL_EXEC_SUB ${AREA}/driver/driver  ${BIN_DIR}/openf90-${VERSION}
    [ "$INSTALL_FORTRAN" = "YES" ] && INSTALL_EXEC_SUB ${AREA}/driver/driver  ${BIN_DIR}/openf95-${VERSION}

    if [ "$TARG_HOST" = "ia64" ] || [ "$TARG_HOST" = "x8664" ]; then
      INSTALL_EXEC_SUB ${TOP_SRCDIR}/osprey/targdir/driver/kdriver ${BIN_DIR}/kopencc
    fi
    if [ "$TARG_HOST" = "ppc32" ]; then
      INSTALL_EXEC_SUB ${TOP_SRCDIR}/osprey/targdir/driver/kdriver ${BIN_DIR}/kopencc
    fi
	fi

    return 0
}

# Install internal gcc distribution
INSTALL_GCC () {
    pushd ${GNUFE42_AREA}

    for j in '*/*' '*/*/*' '*/*/*/*' '*/*/*/*/*' '*/*/*/*/*/*' '*/*/*/*/*/*/*' '*/*/*/*/*/*/*/*'; do
        for i in open64-gcc-4.2.0/$j; do
            if [ -e $i ] && [ ! -d $i ]; then
                INSTALL_EXEC_SUB $i ${ROOT}/$i
            fi
        done
    done

    # Make links to gcc runtime libraries
    cd ${ROOT}
    test -d $PHASEPATH/64 || mkdir $PHASEPATH/64
    if [ "$TARG_HOST" = "x8664" ]
    then
	for i in open64-gcc-4.2.0/lib64/lib*.so*; do
	    if [ -e "$i" ]
	    then
		(cd $PHASEPATH/64; ln -sf ../../../../../$i `basename $i`)
	    fi
	done
	for i in open64-gcc-4.2.0/lib/lib*.so*; do
	    if [ -e "$i" ]
	    then
		(cd $PHASEPATH/32; ln -sf ../../../../../$i `basename $i`)
	    fi
	done
    elif [ "$TARG_HOST" = "ia64" ]
    then
	for i in open64-gcc-4.2.0/lib/lib*.so*; do
	    if [ -e "$i" ]
	    then
		(cd $PHASEPATH/64; ln -sf ../../../../../$i `basename $i`)
	    fi
	done
    fi

    popd

    return 0
}

# Install front-end components
INSTALL_FE () {

    # optional GNU 3.3 based FE
    if [ -f ${AREA}/gccfe/gfec ] ; then 
      INSTALL_EXEC_SUB ${AREA}/gccfe/gfec  ${PHASEPATH}/gfec
      INSTALL_EXEC_SUB ${AREA}/g++fe/gfecc ${PHASEPATH}/gfecc
    fi

    # GNU 4.2.0 based FE
    INSTALL_EXEC_SUB ${AREA}/wgen/wgen42 ${PHASEPATH}/wgen42
    if [ "$TARG_HOST" = "ppc32" ]; then
    LIBEXEC=libexec/gcc/powerpc-redhat-linux/4.2.0
    else
    LIBEXEC=libexec/gcc/${PHASE_DIR_PREFIX}-redhat-linux/4.2.0
    fi
    (cd $PHASEPATH; ln -sf ../../../../open64-gcc-4.2.0/${LIBEXEC}/cc1 cc142)
    (cd $PHASEPATH; ln -sf ../../../../open64-gcc-4.2.0/${LIBEXEC}/cc1plus cc1plus42)

    if [ -f ${AREA}/crayf90/sgi/mfef95 ] ; then 
      INSTALL_EXEC_SUB ${AREA}/crayf90/sgi/mfef95   ${PHASEPATH}/mfef95
      INSTALL_EXEC_SUB ${AREA}/crayf90/sgi/cf95.cat ${PHASEPATH}/cf95.cat
    fi
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

    INSTALL_EXEC_SUB ${LD_NEW_DIR}/ld-new  ${PHASEPATH}/ipa_link

    (cd ${PHASEPATH}; ln -sf be ipl)

    return 0
}

# Install CG-related components
INSTALL_CG () {
    INSTALL_EXEC_SUB ${AREA}/cg/cg.so                ${PHASEPATH}/cg.so
    if [ "$TARG_HOST" = "ia64" ]; then
        # orc_ict.so and orc_intel.so is only valid on ia64
        INSTALL_EXEC_SUB ${AREA}/orc_ict/orc_ict.so      ${PHASEPATH}/orc_ict.so
        INSTALL_EXEC_SUB ${AREA}/orc_intel/orc_intel.so  ${PHASEPATH}/orc_intel.so
    fi
    return 0
}

INSTALL_WHIRL_STUFF () {

    INSTALL_EXEC_SUB  ${AREA}/whirl2c/whirl2c    ${PHASEPATH}/whirl2c
    [ "$INSTALL_FORTRAN" = "YES" ] && INSTALL_EXEC_SUB  ${AREA}/whirl2f/whirl2f    ${PHASEPATH}/whirl2f
    INSTALL_EXEC_SUB  ${AREA}/whirl2c/whirl2c.so ${PHASEPATH}/whirl2c.so
    [ "$INSTALL_FORTRAN" = "YES" ] && INSTALL_EXEC_SUB  ${AREA}/whirl2f/whirl2f.so ${PHASEPATH}/whirl2f.so

    (cd ${PHASEPATH}; ln -sf be whirl2c_be) 
    [ "$INSTALL_FORTRAN" = "YES" ] && (cd ${PHASEPATH}; ln -sf be whirl2f_be) 

    INSTALL_EXEC_SUB  ${AREA}/ir_tools/ir_b2a    ${BIN_DIR}/ir_b2a
    INSTALL_EXEC_SUB  ${AREA}/libspin_4_2_0/gspin42 ${BIN_DIR}/gspin42
    (cd ${BIN_DIR}; ln -sf gspin42 gspin)

    return 0
}



# Install those archieves that are deemed as part of compiler, so 
# we put them where the orcc-phases reside.
INSTALL_PHASE_SPECIFIC_ARCHIVES () {

    if [ "$TARG_HOST" = "ia64" ] ; then
        # These stuffs are only valid on ia64
	LIBAREA="osprey/targdir_lib"

        # f90 related archieves 
        [ "$INSTALL_FORTRAN" = "YES" ] && INSTALL_DATA_SUB ${AREA}/temp_f90libs/lib.cat  ${PHASEPATH}/lib.cat
        [ "$INSTALL_FORTRAN" = "YES" ] && INSTALL_DATA_SUB ${AREA}/temp_f90libs/lib.exp  ${PHASEPATH}/lib.exp

        # instrument archieves.
        INSTALL_DATA_SUB ${LIBAREA}/libcginstr/libcginstr.a  ${PHASEPATH}/libcginstr.a  
        INSTALL_DATA_SUB ${LIBAREA}/libinstr/libinstr.a      ${PHASEPATH}/libinstr.a 

        #  SGI implementation for turning on FLUSH to ZERO
        INSTALL_DATA_SUB ${LIBAREA}/init/ftz.o     ${PHASEPATH}/ftz.o
    elif [ "$TARG_HOST" = "ppc32" ] ;  then
	LIBAREA="osprey/targdir_lib"
	LIB32AREA="osprey/targdir_lib2"
        # 64bit libraries
        [ "$INSTALL_FORTRAN" = "YES" ] && INSTALL_DATA_SUB ${LIBAREA}/libfortran/libfortran.a ${PHASEPATH}/libfortran.a
        [ "$INSTALL_FORTRAN" = "YES" ] && INSTALL_DATA_SUB ${LIBAREA}/libu/libffio.a          ${PHASEPATH}/libffio.a
        INSTALL_DATA_SUB ${LIBAREA}/libm/libmsgi.a       ${PHASEPATH}/libmsgi.a
        INSTALL_DATA_SUB ${LIBAREA}/libmv/libmv.a           ${PHASEPATH}/libmv.a
	    INSTALL_DATA_SUB ${LIBAREA}/libopenmp/libopenmp.a      ${PHASEPATH}/libopenmp.a
        # 32bit libraries
        [ "$INSTALL_FORTRAN" = "YES" ] && INSTALL_DATA_SUB ${LIB32AREA}/libfortran/libfortran.a ${PHASEPATH}/32/libfortran.a
        [ "$INSTALL_FORTRAN" = "YES" ] && INSTALL_DATA_SUB ${LIB32AREA}/libu/libffio.a          ${PHASEPATH}/32/libffio.a
        INSTALL_DATA_SUB ${LIB32AREA}/libm/libmsgi.a       ${PHASEPATH}/32/libmsgi.a
        INSTALL_DATA_SUB ${LIB32AREA}/libmv/libmv.a           ${PHASEPATH}/32/libmv.a
    else
        # IA32 and x86_64
	LIBAREA="osprey/targdir_lib2"
	LIB32AREA="osprey/targdir_lib"
        HUGETLB=${TOP_SRCDIR}/osprey/libhugetlbfs

        INSTALL_DATA_SUB ${LIBAREA}/libinstr2/libinstr.a      ${PHASEPATH}/libinstr.a
        INSTALL_DATA_SUB ${LIB32AREA}/libinstr2/libinstr.a      ${PHASEPATH}/32/libinstr.a

        INSTALL_DATA_SUB ${LIBAREA}/libopen64rt/libopen64rt.a      ${PHASEPATH}/libopen64rt.a
        INSTALL_DATA_SUB ${LIB32AREA}/libopen64rt/libopen64rt.a      ${PHASEPATH}/32/libopen64rt.a

        INSTALL_DATA_SUB ${LIBAREA}/libopen64rt/libopen64rt_shared.a      ${PHASEPATH}/libopen64rt_shared.a
        INSTALL_DATA_SUB ${LIB32AREA}/libopen64rt/libopen64rt_shared.a      ${PHASEPATH}/32/libopen64rt_shared.a

        INSTALL_DATA_SUB ${LIBAREA}/libhugetlbfs/obj64/libhugetlbfs_open64.a     ${PHASEPATH}/libhugetlbfs_open64.a
        INSTALL_DATA_SUB ${LIB32AREA}/libhugetlbfs/obj32/libhugetlbfs_open64.a   ${PHASEPATH}/32/libhugetlbfs_open64.a

        INSTALL_DATA_SUB ${LIBAREA}/libhugetlbfs/obj64/libhugetlbfs_open64.so    ${PHASEPATH}/libhugetlbfs_open64.so.1
        INSTALL_DATA_SUB ${LIB32AREA}/libhugetlbfs/obj32/libhugetlbfs_open64.so  ${PHASEPATH}/32/libhugetlbfs_open64.so.1

        (cd ${PHASEPATH}; ln -sf libhugetlbfs_open64.so.1 libhugetlbfs_open64.so)
        (cd ${PHASEPATH}/32; ln -sf libhugetlbfs_open64.so.1 libhugetlbfs_open64.so)

        INSTALL_DATA_SUB ${HUGETLB}/ldscripts/elf_x86_64.xB      ${PHASEPATH}/64/elf.xB
        INSTALL_DATA_SUB ${HUGETLB}/ldscripts/elf_i386.xB        ${PHASEPATH}/32/elf.xB

        INSTALL_DATA_SUB ${HUGETLB}/ldscripts/elf_x86_64.xBDT    ${PHASEPATH}/64/elf.xBDT
        INSTALL_DATA_SUB ${HUGETLB}/ldscripts/elf_i386.xBDT      ${PHASEPATH}/32/elf.xBDT

        INSTALL_DATA_SUB ${HUGETLB}/ldscripts/elf_x86_64_1G.xBDT    ${PHASEPATH}/64/elf_1G.xBDT
        INSTALL_DATA_SUB ${HUGETLB}/ldscripts/elf_i386_1G.xBDT      ${PHASEPATH}/32/elf_1G.xBDT

        INSTALL_DATA_SUB ${LIBAREA}/libhugetlbfs/elf.xBD    ${PHASEPATH}/64/elf.xBD
        INSTALL_DATA_SUB ${LIB32AREA}/libhugetlbfs/elf.xBD      ${PHASEPATH}/32/elf.xBD

        INSTALL_DATA_SUB ${LIBAREA}/libhugetlbfs/elf_1G.xBD    ${PHASEPATH}/64/elf_1G.xBD
        INSTALL_DATA_SUB ${LIB32AREA}/libhugetlbfs/elf_1G.xBD      ${PHASEPATH}/32/elf_1G.xBD

    fi

    # libgcc.a, libstdc++.a and libstdc++.so are deemed as "GNU link" specific archives
    if [ "$ARCH" = "ia64" ] ; then
        for i in libgcc.a libgcc_s.so libstdc++.a libstdc++.so; do 
            F=`gcc --print-file-name $i`
            if [ ! -z "$F" ] && [ -e "$F" ]; then
              INSTALL_DATA_SUB $F ${PHASEPATH}/$i
            fi
        done
    fi
    return 0
}

# Install the general propose libraries, libfortran.a, libffio.a, libmsgi.a, libmv.a, libm.a, libopenmp.a
INSTALL_GENERAL_PURPOSE_NATIVE_ARCHIVES () {

    if [ "$TARG_HOST" = "ia64" ] ; then
	LIBAREA="osprey/targdir_lib"
        [ "$INSTALL_FORTRAN" = "YES" ] && INSTALL_DATA_SUB ${LIBAREA}/libfortran/libfortran.a ${PHASEPATH}/libfortran.a 
        [ "$INSTALL_FORTRAN" = "YES" ] && INSTALL_DATA_SUB ${LIBAREA}/libu/libffio.a          ${PHASEPATH}/libffio.a
        # libmsgi.a is no longer needed
        #INSTALL_DATA_SUB ${LIBAREA}/libmsgi/libmsgi.a       ${PHASEPATH}/libmsgi.a
        INSTALL_DATA_SUB ${LIBAREA}/libmv/libmv.a           ${PHASEPATH}/libmv.a
        INSTALL_DATA_SUB ${PREBUILT_LIB}/${TARG_HOST}-${TARG_OS}/gnu/libm.a ${PHASEPATH}/libm.a
	INSTALL_DATA_SUB ${LIBAREA}/libopenmp/libopenmp.a      ${PHASEPATH}/libopenmp.a
    elif [ "$TARG_HOST" = "ppc32" ] ; then
	LIBAREA="osprey/targdir_lib"
	LIB32AREA="osprey/targdir_lib2"
    else
	LIBAREA="osprey/targdir_lib2"
        LIB32AREA="osprey/targdir_lib"
        # 64bit libraries
        [ "$INSTALL_FORTRAN" = "YES" ] && INSTALL_DATA_SUB ${LIBAREA}/libfortran/libfortran.a ${PHASEPATH}/libfortran.a
        [ "$INSTALL_FORTRAN" = "YES" ] && INSTALL_DATA_SUB ${LIBAREA}/libfortran/libfortran.so ${PHASEPATH}/libfortran.so
        # FMODS="IEEE_ARITHMETIC.mod IEEE_EXCEPTIONS.mod IEEE_FEATURES.mod ISO_C_BINDING.mod ISO_FORTRAN_ENV.mod"
	FMODS="ISO_C_BINDING.mod"
        for i in $FMODS ; do
            [ "$INSTALL_FORTRAN" = "YES" ] && INSTALL_DATA_SUB ${LIBAREA}/libfortran/$i ${PHASEPATH}/$i
        done
        [ "$INSTALL_FORTRAN" = "YES" ] && INSTALL_DATA_SUB ${LIBAREA}/libu/libffio.a          ${PHASEPATH}/libffio.a
        [ "$INSTALL_FORTRAN" = "YES" ] && INSTALL_DATA_SUB ${LIBAREA}/libu/libffio.so          ${PHASEPATH}/libffio.so
        #INSTALL_DATA_SUB ${LIBAREA}/libm/libmsgi.a       ${PHASEPATH}/libmsgi.a
        INSTALL_DATA_SUB ${LIBAREA}/libmv/libmv.a           ${PHASEPATH}/libmv.a
        INSTALL_DATA_SUB ${LIBAREA}/libmv/libmv.so.1           ${PHASEPATH}/libmv.so.1
        INSTALL_DATA_SUB ${LIBAREA}/libopenmp/libopenmp.a      ${PHASEPATH}/libopenmp.a
        INSTALL_DATA_SUB ${LIBAREA}/libopenmp/libopenmp.so.1      ${PHASEPATH}/libopenmp.so.1
        INSTALL_DATA_SUB ${LIBAREA}/libacml_mv/libacml_mv.a ${PHASEPATH}/libacml_mv.a
        INSTALL_DATA_SUB ${LIBAREA}/libacml_mv/libacml_mv.so.1 ${PHASEPATH}/libacml_mv.so.1
        # 32bit libraries
        [ "$INSTALL_FORTRAN" = "YES" ] && INSTALL_DATA_SUB ${LIB32AREA}/libfortran/libfortran.a ${PHASEPATH}/32/libfortran.a
        [ "$INSTALL_FORTRAN" = "YES" ] && INSTALL_DATA_SUB ${LIB32AREA}/libfortran/libfortran.so ${PHASEPATH}/32/libfortran.so
        for i in $FMODS ; do
            [ "$INSTALL_FORTRAN" = "YES" ] && INSTALL_DATA_SUB ${LIB32AREA}/libfortran/$i ${PHASEPATH}/32/$i
        done
        [ "$INSTALL_FORTRAN" = "YES" ] && INSTALL_DATA_SUB ${LIB32AREA}/libu/libffio.a          ${PHASEPATH}/32/libffio.a
        [ "$INSTALL_FORTRAN" = "YES" ] && INSTALL_DATA_SUB ${LIB32AREA}/libu/libffio.so          ${PHASEPATH}/32/libffio.so
        #INSTALL_DATA_SUB ${LIB32AREA}/libm/libmsgi.a       ${PHASEPATH}/32/libmsgi.a
        INSTALL_DATA_SUB ${LIB32AREA}/libmv/libmv.a           ${PHASEPATH}/32/libmv.a
        INSTALL_DATA_SUB ${LIB32AREA}/libmv/libmv.so.1           ${PHASEPATH}/32/libmv.so.1
        INSTALL_DATA_SUB ${LIB32AREA}/libopenmp/libopenmp.a      ${PHASEPATH}/32/libopenmp.a
        INSTALL_DATA_SUB ${LIB32AREA}/libopenmp/libopenmp.so.1      ${PHASEPATH}/32/libopenmp.so.1
        INSTALL_DATA_SUB ${LIB32AREA}/libacml_mv/libacml_mv.a ${PHASEPATH}/32/libacml_mv.a
        INSTALL_DATA_SUB ${LIB32AREA}/libacml_mv/libacml_mv.so.1 ${PHASEPATH}/32/libacml_mv.so.1

        (cd ${PHASEPATH}; ln -sf libmv.so.1 libmv.so; ln -sf libopenmp.so.1 libopenmp.so)
        (cd ${PHASEPATH}; ln -sf libacml_mv.so.1 libacml_mv.so)
        (cd ${PHASEPATH}/32; ln -sf libmv.so.1 libmv.so; ln -sf libopenmp.so.1 libopenmp.so)
        (cd ${PHASEPATH}/32; ln -sf libacml_mv.so.1 libacml_mv.so)
    fi 
    return 0
}

INSTALL_PREBUILD_GNU_NATIVE_CRT_STARTUP () {

    if [ "$ARCH" = "ia64" ] ; then
        for i in crtbegin.o crtend.o crtbeginS.o crtendS.o crtbeginT.o crtendT.o; do 
            F=`gcc --print-file-name=$i`
            if [ ! -z "F" ] && [ -e "$F" ]; then
              INSTALL_DATA_SUB $F ${PHASEPATH}/$i
            fi
        done
    fi
    return 0
}


INSTALL_PREBUILD_OPEN64_NATIVE_LIB () {

    [ ! -d ${PREBUILT_LIB}/${PREBUILD_INTERPOS}/open64 ] && return 0

    for i in ${PREBUILT_LIB}/${PREBUILD_INTERPOS}/open64/* ; do 

        x=`basename $i`
        [ "$x" = "CVS" ] && continue;
        [ "$x" = ".svn" ] && continue;

        [ "$x" = "libinstr.a" ] &&
            INSTALL_DATA_SUB $i ${PHASEPATH}/$x && continue;
        [ "$x" = "libcginstr.a" ] &&
            INSTALL_DATA_SUB $i ${PHASEPATH}/$x && continue;

        INSTALL_DATA_SUB $i ${NATIVE_LIB_DIR}/`basename $i`
    done

    # install the 32bit prebuild libraries for x8664
    [ "$TARG_HOST" != "x8664" ] && return 0
    [ ! -d ${PREBUILT_LIB}/${PREBUILD_INTERPOS}/open64/32 ] && return 0
    for i in ${PREBUILT_LIB}/${PREBUILD_INTERPOS}/open64/32/* ; do

        x=`basename $i`
        [ "$x" = "CVS" ] && continue;
        [ "$x" = ".svn" ] && continue;

        INSTALL_DATA_SUB $i ${NATIVE_LIB_DIR}/32/$x
    done

    return 0
}

   # Install GNU glic-devel package. this is perform only for cross compilation. 
   # On native environment, we requires the end user install glibc-devel before 
   # hand.
INSTALL_PREBUILD_GLIBC_NATIVE_LIB () {

    [ ! -d ${PREBUILT_LIB}/${PREBUILD_INTERPOS}/gnu ] && return 0

    for i in ${PREBUILT_LIB}/${PREBUILD_INTERPOS}/gnu/* ; do 
        x=`basename $i`
        [ "$x" = "CVS" ] && continue;
        [ "$x" = ".svn" ] && continue;
        [ "$x" = "libstdc++.a" ] && continue; 
        [ "$x" = "libgcc.a"    ] && continue;
        INSTALL_EXEC_SUB $i ${NATIVE_LIB_DIR}/`basename $i`
    done  
    
    return 0
}

INSTALL_PREBUILD_PHASE () {

    # Some prebuild
    for i in ${PREBUILT_BIN}/${PREBUILD_INTERPOS}/phase/* ; do 
	[ ! -e $i ] && continue;
        [ "`basename $i`" = "CVS" ] && continue
        [ "`basename $i`" = ".svn" ] && continue
        INSTALL_EXEC_SUB $i ${PHASEPATH}/`basename $i`
    done

    return 0
}

INSTALL_CROSS_UTIL () {

    [ ! -d ${PREBUILT_BIN}/${PREBUILD_INTERPOS}/util ] && return 0

    for i in ${PREBUILT_BIN}/${PREBUILD_INTERPOS}/util/* ; do 
	[ "`basename $i`" = "CVS" ] && continue
        [ "`basename $i`" = ".svn" ] && continue
    	INSTALL_EXEC_SUB $i ${BIN_DIR}/`basename $i`
    done

    return 0
}

INSTALL_NATIVE_HEADER () {

    #INSTALL_DATA_SUB osprey/include/nue/stdarg.h  ${PHASEPATH}/include/stdarg.h
    #INSTALL_DATA_SUB osprey/include/nue/va-ia64.h  ${PHASEPATH}/include/va-ia64.h 
    #cp -f -a osprey/include ${PHASEPATH}/ 
    INSTALL_DATA_SUB ${TOP_SRCDIR}/osprey/include/whirl2c.h  ${ROOT}/include/${VERSION}/whirl2c.h
    [ "$INSTALL_FORTRAN" = "YES" ] && INSTALL_DATA_SUB ${TOP_SRCDIR}/osprey/include/whirl2f.h  ${ROOT}/include/${VERSION}/whirl2f.h

    INSTALL_DATA_SUB ${AREA}/include/dwarf.h  ${ROOT}/include/${VERSION}/dwarf.h
    INSTALL_DATA_SUB ${AREA}/include/libdwarf.h  ${ROOT}/include/${VERSION}/libdwarf.h

    INSTALL_DATA_SUB ${AREA}/include/libelf/libelf.h  ${ROOT}/include/${VERSION}/libelf/libelf.h
    INSTALL_DATA_SUB ${AREA}/include/libelf/sys_elf.h  ${ROOT}/include/${VERSION}/libelf/sys_elf.h

    INSTALL_DATA_SUB ${TOP_SRCDIR}/osprey/include/omp/omp.h  ${ROOT}/include/${VERSION}/omp.h
    INSTALL_DATA_SUB ${TOP_SRCDIR}/osprey/include/omp/omp_lib.h  ${ROOT}/include/${VERSION}/omp_lib.h
    [ "$INSTALL_FORTRAN" = "YES" ] && INSTALL_DATA_SUB ${TOP_SRCDIR}/osprey/include/omp/omp_lib.f  ${ROOT}/include/${VERSION}/omp_lib.f

    return 0
}

INSTALL_MAN_PAGE () {

    d1=osprey/man/linux/man1
    d2=$ROOT/usr/man/man1

    INSTALL_DATA_SUB $d1/sgicc.1 $d2 
    [ "$INSTALL_FORTRAN" = "YES" ] && INSTALL_DATA_SUB $d1/sgif90.1 $d2

    (cd $d2; ln -sf sgicc.1 sgiCC.1)

    return 0
}

INSTALL_MISC () {
    INSTALL_EXEC_SUB ${AREA}/wopt/wopt.so         ${PHASEPATH}/wopt.so
    INSTALL_EXEC_SUB ${AREA}/lw_inline/lw_inline  ${PHASEPATH}/inline
    INSTALL_EXEC_SUB ${AREA}/lno/lno.so           ${PHASEPATH}/lno.so

    if [ "$TARG_HOST" = "ia64" ]; then
        INSTALL_EXEC_SUB ${AREA}/targ_info/itanium.so ${PHASEPATH}/itanium.so
        INSTALL_EXEC_SUB ${AREA}/targ_info/itanium2.so ${PHASEPATH}/itanium2.so
    fi

    # install some scripts
    [ ! -d ${PREBUILT_BIN}/misc ] && return 0
    for i in ${PREBUILT_BIN}/misc/* ; do 
        [ -f "$i" ] && INSTALL_EXEC_SUB ${i} ${BIN_DIR}/`basename $i`
    done

    return 0
}

# Create the Fortran module files for the OpenMP interface
INSTALL_MODULES () {
    if [ ! -e ${ROOT}/include/${VERSION}/OMP_LIB.mod ] ; then
        (cd ${ROOT}/include/${VERSION}; ${ROOT}/bin/openf90 -c omp_lib.f)
    fi

    return 0
}

# cd `dirname $0`

[ ! -d ${BIN_DIR} ] && mkdir -p ${BIN_DIR}
[ ! -d ${NATIVE_LIB_DIR} ] && mkdir -p ${NATIVE_LIB_DIR}
if [ "$TARG_HOST" = "x8664" -a ! -d "${NATIVE_LIB_DIR}/32" ]; then
    mkdir -p ${NATIVE_LIB_DIR}/32
fi

INSTALL_DRIVER 
if [ "$TARG_HOST" != "ppc32"  ]; then
INSTALL_GCC
fi
INSTALL_FE 
INSTALL_BE 
INSTALL_IPA 
INSTALL_CG 
INSTALL_WHIRL_STUFF 
INSTALL_MISC
INSTALL_NATIVE_HEADER

#cat << _EOF_
# ------------------------------------------------------------------------
# NOTE: Following archives may not present. these archives are built on 
#   Native or NUE platform (by 'make library'), but do not
#   worry, prebuild verion of them are provided.
#  
#   {libcginstr.a libinstr.a ftz.o libfortran.a libffio.a
#                 libmsgi.a libmv.a}
#  
#   Normally, you need not to build these archives.
# ------------------------------------------------------------------------
#_EOF_

# Install archieves 
INSTALL_PHASE_SPECIFIC_ARCHIVES 
[ "$INSTALL_TYPE" = "ia64-cross" ] && INSTALL_PREBUILD_GLIBC_NATIVE_LIB 
[ "$INSTALL_TYPE" = "ia64-cross" ] && INSTALL_NATIVE_HEADER 
INSTALL_GENERAL_PURPOSE_NATIVE_ARCHIVES
INSTALL_PREBUILD_OPEN64_NATIVE_LIB 
INSTALL_PREBUILD_GNU_NATIVE_CRT_STARTUP 
[ "$INSTALL_TYPE" = "ia64-cross" ] && INSTALL_CROSS_UTIL
INSTALL_PREBUILD_PHASE 
[ "$INSTALL_FORTRAN" = "YES" ] && INSTALL_MODULES

exit 0

