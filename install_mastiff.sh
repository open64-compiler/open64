#!/bin/bash

TARGET=$1
VERSION=$2
DIST=0
if [ ! -z "$3" ]; then
  DIST=1
fi

AREA="osprey/targdir"
FE_AREA="osprey-gcc-4.2.0/host-unknown"
LD_AREA="binutils"
CYGNUS_AREA="osprey/cygnus"
LIB_AREA="osprey/targdir_lib"
LIB_AREA2="osprey/targdir_lib2"

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
ROOT_BIN=${TOOLROOT}/bin
ROOT_LIB=${TOOLROOT}/lib/${VERSION}
ROOT_LIB32=${TOOLROOT}/lib/${VERSION}/32
ROOT_INC=${TOOLROOT}/include
ROOT_GCC=${TOOLROOT}/compat/gcc-4
ROOT_UDR=${TOOLROOT}/udr

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
    INSTALL_EXEC_SUB ${AREA}/driver/driver   ${ROOT_BIN}/xvsa
    INSTALL_EXEC_SUB ${TOP_SRCDIR}/osprey/be/rbc/xcalibyte-meta.py  ${ROOT_BIN}/xcalmeta
    INSTALL_EXEC_SUB ${TOP_SRCDIR}/osprey/linux/tools/simpv ${ROOT_BIN}/simpv
    INSTALL_EXEC_SUB ${TOP_SRCDIR}/osprey/linux/tools/json_to_ascii.py ${ROOT_BIN}/json_to_ascii.py
    INSTALL_EXEC_SUB ${TOP_SRCDIR}/osprey/linux/tools/convert.sh ${ROOT_BIN}/convert.sh
    if [ $DIST -eq 0 ]; then
      INSTALL_EXEC_SUB ${AREA}/ir_tools/ir_b2a ${ROOT_BIN}/ir_b2a
      INSTALL_EXEC_SUB ${LD_AREA}/binutils/cxxfilt ${ROOT_BIN}/cxxfilt
    fi
    return 0
}

# Install front-end components
INSTALL_FE () {
    INSTALL_EXEC_SUB ${AREA}/wgen/wgen42    ${ROOT_LIB}/mapirg
    INSTALL_EXEC_SUB ${FE_AREA}/gcc/cc1     ${ROOT_LIB}/mapfec
    INSTALL_EXEC_SUB ${FE_AREA}/gcc/cc1plus ${ROOT_LIB}/mapfex

    # install jfe
    # INSTALL_EXEC_SUB ${AREA}/jfe/libb2w/libb2w/libb2w.so ${ROOT_LIB}/libjfe/libb2w.so
    # INSTALL_EXEC_SUB ${AREA}/jfe/fernflower/libs/fernflower.jar ${ROOT_LIB}/libjfe/fernflower.jar
    INSTALL_EXEC_SUB ${AREA}/jfe/libb2w/libb2w/libmacbcb.so ${ROOT_LIB}/libmacbcb.so
    INSTALL_EXEC_SUB ${AREA}/jfe/mapfej ${ROOT_LIB}/mapfej
    INSTALL_EXEC_SUB ${AREA}/jfe/B2WFrontEnd/libs/macbcr.jar ${ROOT_LIB}/macbcr.jar

    INSTALL_EXEC_SUB ${AREA}/clang2whirl/mapclang ${ROOT_LIB}/mapclang
    INSTALL_EXEC_SUB ${AREA}/js2mplref/js2mpl ${ROOT_LIB}/js2mpl
    INSTALL_EXEC_SUB ${AREA}/ir_tools/mpl2whirl ${ROOT_LIB}/mpl2whirl

    # install clang header files
    for file in $(cd ${TOP_SRCDIR}/osprey-clang/clang && find . -type f);
    do
      ${INSTALL_DATA} ${TOP_SRCDIR}/osprey-clang/clang/${file} ${ROOT_INC}/clang/${file};
    done

    return 0
}

# Install back-end components 
INSTALL_BE () {
    INSTALL_EXEC_SUB ${AREA}/lw_inline/lw_inline  ${ROOT_LIB}/mapinl
    INSTALL_EXEC_SUB ${AREA}/be/be                ${ROOT_LIB}/mapcbe
    INSTALL_EXEC_SUB ${LD_AREA}/ld/ld-new         ${ROOT_LIB}/mapxfa
    INSTALL_EXEC_SUB ${AREA}/be/maccom.so         ${ROOT_LIB}/maccom.so
    INSTALL_EXEC_SUB ${AREA}/be/macldp.so         ${ROOT_LIB}/macldp.so
    INSTALL_EXEC_SUB ${AREA}/wopt/macdfa.so       ${ROOT_LIB}/macdfa.so
    INSTALL_EXEC_SUB ${AREA}/vsa/macvsa.so        ${ROOT_LIB}/macvsa.so
    INSTALL_EXEC_SUB ${AREA}/vsa/macrbc.so        ${ROOT_LIB}/macrbc.so
    INSTALL_EXEC_SUB ${AREA}/cg/macdcg.so         ${ROOT_LIB}/macdcg.so
    INSTALL_EXEC_SUB ${AREA}/lno/maclpa.so        ${ROOT_LIB}/maclpa.so

    INSTALL_EXEC_SUB ${AREA}/vsa/meta_mgr                         ${ROOT_LIB}/metamgr
    INSTALL_EXEC_SUB ${AREA}/include/rbc_base.h                   ${ROOT_INC}/rbc_base.h
    INSTALL_EXEC_SUB ${AREA}/include/RBC_ENGINE.java              ${ROOT_INC}/RBC_ENGINE.java
    INSTALL_EXEC_SUB ${LIB_AREA}/librbc/libcertc32.inlskip        ${ROOT_LIB}/libcertc32.inlskip
    INSTALL_EXEC_SUB ${LIB_AREA}/librbc/libcertc64.inlskip        ${ROOT_LIB}/libcertc64.inlskip
    INSTALL_EXEC_SUB ${LIB_AREA}/librbc/libcertc32.a              ${ROOT_LIB}/libcertc32.a
    INSTALL_EXEC_SUB ${LIB_AREA}/librbc/libcertc64.a              ${ROOT_LIB}/libcertc64.a
    INSTALL_EXEC_SUB ${LIB_AREA}/librbc/libcxx_intrn32.a          ${ROOT_LIB}/libcxx_intrn32.a
    INSTALL_EXEC_SUB ${LIB_AREA}/librbc/libcxx_intrn64.a          ${ROOT_LIB}/libcxx_intrn64.a
    INSTALL_EXEC_SUB ${LIB_AREA}/librbc/libjni.a                  ${ROOT_LIB}/libjni.a
    INSTALL_EXEC_SUB ${LIB_AREA}/librbc/libcertj.a                ${ROOT_LIB}/libcertj.a
    INSTALL_EXEC_SUB ${LIB_AREA}/librbc/librulex.a                ${ROOT_LIB}/librulex.a
    INSTALL_EXEC_SUB ${LIB_AREA}/librbc/libxcbr32.a               ${ROOT_LIB}/libxcbr32.a
    INSTALL_EXEC_SUB ${LIB_AREA}/librbc/libxcbr64.a               ${ROOT_LIB}/libxcbr64.a
    INSTALL_EXEC_SUB ${LIB_AREA}/librbc/libmtr32.a                ${ROOT_LIB}/libmtr32.a
    INSTALL_EXEC_SUB ${LIB_AREA}/librbc/libmtr64.a                ${ROOT_LIB}/libmtr64.a

    if [ $DIST -eq 0 ]; then
      INSTALL_EXEC_SUB ${LIB_AREA}/libwffi/wffi.a     ${ROOT_LIB}/wffi.a
      INSTALL_EXEC_SUB ${LIB_AREA}/libwffi/libwffi.so ${ROOT_LIB}/libwffi.so
      INSTALL_DATA_SUB ${LIB_AREA}/libwffi/wffi.js    ${ROOT_LIB}/wffi.js
    fi

    return 0
}

INSTALL_LIB() {
  if [ $DIST -eq 0 ]; then
    # only open64rt build for 64bit for now
    INSTALL_EXEC_SUB ${LIB_AREA}/libopen64rt/libopen64rt.a           ${ROOT_LIB}/libopen64rt.a
    INSTALL_EXEC_SUB ${LIB_AREA}/libopen64rt/libopen64rt_shared.a    ${ROOT_LIB}/libopen64rt_shared.a
    # install 32 bit library if exists
    if [ -e "${LIB_AREA2}" ]; then
      INSTALL_EXEC_SUB ${LIB_AREA2}/libopen64rt/libopen64rt.a        ${ROOT_LIB32}/libopen64rt.a
      INSTALL_EXEC_SUB ${LIB_AREA2}/libopen64rt/libopen64rt_shared.a ${ROOT_LIB32}/libopen64rt_shared.a
      INSTALL_EXEC_SUB ${LIB_AREA2}/libmv/libmv.a                    ${ROOT_LIB32}/libmv.a
      INSTALL_EXEC_SUB ${LIB_AREA2}/libmv/libmv.so.1                 ${ROOT_LIB32}/libmv.so.1
      INSTALL_EXEC_SUB ${LIB_AREA2}/libopenmp/libopenmp.a            ${ROOT_LIB32}/libopenmp.a
      INSTALL_EXEC_SUB ${LIB_AREA2}/libopenmp/libopenmp.so.1         ${ROOT_LIB32}/libopenmp.so.1
      INSTALL_EXEC_SUB ${LIB_AREA2}/libacml_mv/libacml_mv.a          ${ROOT_LIB32}/libacml_mv.a
      INSTALL_EXEC_SUB ${LIB_AREA2}/libacml_mv/libacml_mv.so.1       ${ROOT_LIB32}/libacml_mv.so.1
      if [ -e "${ROOT_LIB32}" ]; then
        (cd ${ROOT_LIB32}; ln -sf libmv.so.1 libmv.so; ln -sf libopenmp.so.1 libopenmp.so)
        (cd ${ROOT_LIB32}; ln -sf libacml_mv.so.1 libacml_mv.so)
      fi
    fi
  fi
}

INSTALL_IPA() {
    INSTALL_EXEC_SUB ${AREA}/ipa/macdip.so        ${ROOT_LIB}/macdip.so
    INSTALL_EXEC_SUB ${AREA}/ipl/macdls.so        ${ROOT_LIB}/macdls.so
    INSTALL_EXEC_SUB ${CYGNUS_AREA}/ld/ld-new     ${ROOT_LIB}/maplink

    (cd ${ROOT_LIB}; ln -sf mapcbe mapdls)
    return 0
}

INSTALL_INTENT_LIB () {
  INTENT_TABLES=`find ${LIB_AREA}/librbc/udr/java -name "*.udr"`
  for INTENT_TABLE in ${INTENT_TABLES}
  do
    BASE_NAME=`basename ${INTENT_TABLE}`
    INSTALL_EXEC_SUB ${INTENT_TABLE} ${ROOT_UDR}/java/${BASE_NAME}
  done
}

# Remove debug info and unused files for distribution
MAKE_DIST () {
    # rm extra gcc files
    rm ${ROOT_GCC}/bin/c++ ${ROOT_GCC}/bin/cpp ${ROOT_GCC}/bin/gccbug \
       ${ROOT_GCC}/bin/gcov ${ROOT_GCC}/bin/x86_64*
    rm -rf ${ROOT_GCC}/include/c++/4.2.0/x86_64-linux-gnu/bits/stdc++.h.gch
    rm -rf ${ROOT_GCC}/include/c++/4.2.0/x86_64-linux-gnu/bits/stdtr1c++.h.gch
    rm ${ROOT_GCC}/libexec/gcc/x86_64-linux-gnu/4.2.0/cc1 \
       ${ROOT_GCC}/libexec/gcc/x86_64-linux-gnu/4.2.0/cc1plus \
       ${ROOT_GCC}/libexec/gcc/x86_64-linux-gnu/4.2.0/collect2
    rm ${ROOT_GCC}/lib/gcc/x86_64-linux-gnu/4.2.0/*.*
    rm ${ROOT_GCC}/lib/x86_64-linux-gnu/*.*
    rm -rf ${ROOT_GCC}/lib64/*.*
    rm -rf ${ROOT_GCC}/info ${ROOT_GCC}/man

    # strip debug symtab
    for i in ${ROOT_BIN}/xvsa ${ROOT_LIB}/metamgr \
             ${ROOT_LIB}/mapirg ${ROOT_LIB}/mapfec ${ROOT_LIB}/mapfex ${ROOT_LIB}/mapclang \
             ${ROOT_LIB}/libmacbcb.so \
             ${ROOT_LIB}/mapinl ${ROOT_LIB}/mapcbe ${ROOT_LIB}/mapxfa \
             ${ROOT_LIB}/maccom.so ${ROOT_LIB}/macldp.so ${ROOT_LIB}/macdfa.so ${ROOT_LIB}/macvsa.so ${ROOT_LIB}/macrbc.so \
             ${ROOT_LIB}/js2mpl ${ROOT_LIB}/mpl2whirl \
             ${ROOT_LIB}/macdcg.so ${ROOT_LIB}/maclpa.so ${ROOT_LIB}/macdls.so ${ROOT_LIB}/macdip.so ${ROOT_LIB}/maplink \
        ; do
        strip $i
    done

    # create symbol link for gcc
    workdir=`pwd`
    cd ${ROOT_GCC}/bin
    ln -sf gcc cc
    ln -sf g++ c++
    cd ${ROOT_GCC}/libexec/gcc/x86_64-linux-gnu/4.2.0
    ln -sf ../../../../../../lib/${VERSION}/mapfec cc1
    ln -sf ../../../../../../lib/${VERSION}/mapfex cc1plus
    cd ${workdir}

    # cp libstdc++ & libgcc_s
    cp `gcc --print-file-name=libstdc++.so.6` ${ROOT_LIB}/libstdc++.so.6
    cp `gcc --print-file-name=libgcc_s.so.1`  ${ROOT_LIB}/libgcc_s.so.1
}

INSTALL_DRIVER 
INSTALL_FE 
INSTALL_BE
INSTALL_LIB
INSTALL_IPA
if [ $TARGET != "UWASM" ]
then
  INSTALL_INTENT_LIB
fi

if [ $DIST -eq 1 ]; then
  MAKE_DIST
fi

exit 0