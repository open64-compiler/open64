Copyright 2010 Hewlett Packard Company
Copyright (C) 2012 Advanced Micro Devices, Inc.  All Rights Reserved.

This file is free software: you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation, either version 2 of the License, or
(at your option) any later version.

This file is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this file; ; see the file osprey/COPYING.  If not, see
<http://www.gnu.org/licenses/>.


# When modifying this file, configure.ac, the person making the changes
# should use autoconf 2.59 to regenerate the configure script.  Both
# configure.ac and configure should be checked in to subversion.  This is
# so that developers or users can build open64 without needing autoconf on
# their systems.

# It may be necessary from time to time to also update some of the files
# used by autoconf.  These are config.guess, config.sub, install-sh.
# The official versions of these three files are actually part of automake
# (which open64 does not use).  If any of these files need updating, the
# required change should be made to the latest automake sources first
# (if they are not already there) and then the files should be copied
# from the automake sources to our source tree.  Our versions of these
# files should always match some version of the automake sources and
# not diverge by having local changes in them.


AC_PREREQ([2.59])
AC_INIT([opencc],[4.2])
AC_CANONICAL_BUILD
AC_CANONICAL_HOST
AC_CANONICAL_TARGET
#AC_PROG_CC
#AC_PROG_CXX
#AC_PROG_RANLIB

# There are known bugs when building the compiler in 64 bit mode on x86 so,
# unless the user specifies otherwise, we default to 32 bit mode as the
# default on x86 64 bit platforms.

if test "$build_alias" = "" && test "$host_alias" = "" && test "$host_cpu" = "x86_64"; then
  host_cpu=i686
  host=$host_cpu-$host_vendor-$host_os
fi

if test "$build_alias" = "" && test "$target_alias" = "" && test "$target_cpu" = "x86_64"; then
  target_cpu=i686
  target=$target_cpu-$target_vendor-$target_os
fi

AC_ARG_WITH(build-compiler,
[  --with-build-compiler=GNU|OSP	Use GNU or OSP compilers in build],
BUILD_COMPILER="$with_build_compiler",
BUILD_COMPILER=GNU)

case "$BUILD_COMPILER" in
  GNU|gnu|gcc)
    BUILD_COMPILER=GNU
    GCC_CONFIGURE_COMPILER=gcc
    CYGNUS_CONFIGURE_COMPILER=gcc
    ;;
  OSP|osp|opencc|open64)
    BUILD_COMPILER=OSP
    GCC_CONFIGURE_COMPILER=opencc
    CYGNUS_CONFIGURE_COMPILER=opencc
    ;;
  *)
    AC_MSG_ERROR(["BUILD_COMPILER=$BUILD_COMPILER is not supported"])
    ;;
esac

AC_ARG_WITH(build-optimize,
[  --with-build-optimize=DEFAULT|DEBUG	Use DEFAULT or DEBUG flags in compiler build],
BUILD_OPTIMIZE="$with_build_optimize",
BUILD_OPTIMIZE=DEFAULT)

case "$BUILD_OPTIMIZE" in
  DEFAULT|default)
    BUILD_OPTIMIZE=DEFAULT
    GCC_CONFIGURE_CFLAGS="-O2"
    ;;
  DEBUG|debug)
    BUILD_OPTIMIZE=DEBUG
    GCC_CONFIGURE_CFLAGS="-O0 -g -DIs_True_On"
    ;;
  *)
    AC_MSG_ERROR(["BUILD_OPTIMIZE=$BUILD_OPTIMIZE is not supported"])
    ;;
esac

# TODO cleanup Fortran Front-End (FFE) so that extra debugging checks
# can be enabled when BUILD_OPTIMIZE is set to DEBUG.
AC_ARG_WITH(build-ffe-optimize,
[  --with-build-ffe-optimize=DEFAULT|DEBUG Use DEFAULT or DEBUG flags in FFE build],
BUILD_FFE_OPTIMIZE="$with_build_ffe_optimize",
BUILD_FFE_OPTIMIZE=DEFAULT)

AC_ARG_WITH(build-lib-optimize,
[  --with-build-lib-optimize=DEFAULT|DEBUG Use DEFAULT or DEBUG flags in library build.
    Note that --with-build-lib-optimize=DEBUG is currently not well tested.],
BUILD_LIB_OPTIMIZE="$with_build_lib_optimize",
BUILD_LIB_OPTIMIZE=DEFAULT)

case "$BUILD_LIB_OPTIMIZE" in
  DEFAULT|default)
    BUILD_LIB_OPTIMIZE=DEFAULT
    ;;
  DEBUG|debug)
    BUILD_LIB_OPTIMIZE=DEBUG
    ;;
  *)
    AC_MSG_ERROR(["BUILD_LIB_OPTIMIZE=$BUILD_LIB_OPTIMIZE is not supported"])
    ;;
esac

# BUILD_MULTILIB supports building libraries twice, this is used on x86
# targets to have 32 and 64 bit libraries available.
BUILD_MULTILIB=NO

# BUILD_BOTH is (only) used in osprey/Makefile.gsetup.in for setting -fPIC.
BUILD_BOTH=

# Set whether to build the GCC 3.3 C/C++ front-end.
AC_ARG_ENABLE([gnu3],
  AS_HELP_STRING([--enable-gnu3], [Enable GNU3-based C/C++ frontend]))

AS_IF([test "x$enable_gnu3" = "xyes"], [
  BUILD_GNU3=YES
])

# Set whether Fortran driver and libraries will be built.
AC_ARG_ENABLE([fortran],
  AS_HELP_STRING([--disable-fortran], [Disable Fortran language support]))

AS_IF([test "x$enable_fortran" != "xno"], [
  BUILD_FORTRAN=YES
])

# Set whether WHIRL a node contains a debugging ID
AC_ARG_ENABLE([whirl_id],
  AS_HELP_STRING([--enable-whirl-id], [Enable debugging ID in WHIRL node]))

AS_IF([test "x$enable_whirl_id" = "xyes"], [
  WHIRL_HAS_ID_FIELD=YES
], [
  WHIRL_HAS_ID_FIELD=NO
])

# Set whether to skip IPA support (default NO).
AC_ARG_ENABLE([ipa],
  AS_HELP_STRING([--disable-ipa], [Disable IPA compilation support (EXPERIMENTAL)]))

AS_IF([test "x$enable_ipa" = "xno"], [
  BUILD_SKIP_IPA=YES
])

# Set whether shared libraries will be built.  Implies --disable-ipa.
BUILD_TYPE=NONSHARED
AC_ARG_ENABLE([shared],
  AS_HELP_STRING([--disable-shared], [Disable shared libraries (EXPERIMENTAL)]))

AS_IF([test "x$enable_shared" != "xno"], [
  BUILD_TYPE=SHARED
])
 
if test "${BUILD_TYPE}" = "NONSHARED"; then
  BUILD_SKIP_IPA=YES
fi

# Specify the targ_info directory under osprey/common to use.  IA64 uses
# ipfec_targ_info instead of targ_info.
TARG_INFO_NAME=targ_info

# Specify the GCC src directory.
GCC_DIR=osprey-gcc-4.2.0

# LIB_* and LIB2_* variables will default to the BUILD_* variables if not
# overridden in one of the case statements below so most platforms will
# not have to set these.
LIB_MACHINE_TYPE=
LIB_BUILD_TARGET=
LIB_BUILD_ABI=
LIB_BUILD_ARCH=
LIB_BUILD_HOST=
LIB2_MACHINE_TYPE=
LIB2_BUILD_TARGET=
LIB2_BUILD_ABI=
LIB2_BUILD_ARCH=
LIB2_BUILD_HOST=

CYGNUS_CONFIGURE_CFLAGS=

# Set variables based on the host machine that the compiler will run on.
case "$host" in
	x86_64*-*-linux*)
		BUILD_ABI=I64BIT
		BUILD_ARCH=X8664
		BUILD_HOST=X8664
		CYGNUS_CONFIGURE_HOST=i686-open64-linux
		GCC_CONFIGURE_HOST=x86_64-redhat-linux
		GCC_CONFIGURE_COMPILER+=" -m64"
		;;
	i*86*-*-linux*)
		BUILD_ABI=I32BIT
		BUILD_ARCH=IA32
		BUILD_HOST=IA32
		CYGNUS_CONFIGURE_HOST=i686-open64-linux
		GCC_CONFIGURE_HOST=x86_64-redhat-linux
		GCC_CONFIGURE_COMPILER+=" -m32"
		CYGNUS_CONFIGURE_CFLAGS="-m32"
		;;
	ia64*-*-linux*)
		BUILD_ABI=I64BIT
		BUILD_ARCH=IA64
		BUILD_HOST=IA64
		CYGNUS_CONFIGURE_HOST=ia64-linux-gnu
		GCC_CONFIGURE_HOST=ia64-redhat-linux
		;;
	powerpc-*-linux*)
		;;
	*)
		AC_MSG_ERROR(["open64 is not supported on $target"])
		;;
esac

BUILD_VENDOR=

# Set variables based on the target machine that the compiler will generate
# code for.
case "$target" in
	x86_64*-*-linux*)
		MACHINE_TYPE=x86_64
		BUILD_TARGET=X8664
		LIB_MACHINE_TYPE=IA32
		LIB_BUILD_TARGET=IA32
		LIB_BUILD_ABI=I32BIT
		LIB_BUILD_ARCH=IA32
		LIB2_MACHINE_TYPE=X86_64
		LIB2_BUILD_TARGET=X8664
		LIB2_BUILD_ABI=I64BIT
		LIB2_BUILD_ARCH=X8664
		CYGNUS_CONFIGURE_TARG=x86_64-open64-linux
		GCC_CONFIGURE_TARG=x86_64-redhat-linux
		GCC_CONFIGURE_CFLAGS+=" -DTARG_X8664"
		;;
	i*86*-*-linux*)
		MACHINE_TYPE=i386
		BUILD_TARGET=X8664
		LIB_MACHINE_TYPE=IA32
		LIB_BUILD_TARGET=IA32
		LIB_BUILD_ABI=I32BIT
		LIB_BUILD_ARCH=IA32
		LIB2_MACHINE_TYPE=X86_64
		LIB2_BUILD_TARGET=X8664
		LIB2_BUILD_ABI=I64BIT
		LIB2_BUILD_ARCH=X8664
		CYGNUS_CONFIGURE_TARG=x86_64-open64-linux
		GCC_CONFIGURE_TARG=x86_64-redhat-linux
		GCC_CONFIGURE_CFLAGS+=" -DTARG_X8664"
		;;
	ia64*-*-linux*)
		MACHINE_TYPE=ia64
		BUILD_TARGET=IA64
		CYGNUS_CONFIGURE_TARG=ia64-linux-gnu
		GCC_CONFIGURE_TARG=ia64-redhat-linux
		GCC_CONFIGURE_CFLAGS+=" -DTARG_IA64"
		TARG_INFO_NAME=ipfec_targ_info
		;;
	powerpc-*-linux*)
		MACHINE_TYPE=PPC32
		BUILD_ABI=I32BIT
		BUILD_TARGET=PPC32
		BUILD_MULTILIB=NO
		BUILD_VENDOR=OSP
		GCC_CONFIGURE_TARG=powerpc-redhat-linux
		GCC_CONFIGURE_HOST=powerpc-redhat-linux
		GCC_CONFIGURE_BUILD=powerpc-redhat-linux
		CYGNUS_CONFIGURE_TARG=mipsel-elf-linux
		GCC_CONFIGURE_COMPILER+=" -m32"
		GCC_CONFIGURE_CFLAGS+=" -DTARG_PPC32"
		case "$host" in
			powerpc-*-linux*)
				BUILD_ARCH=PPC32
				BUILD_HOST=PPC32
				BUILD_SKIP_IPA=YES
				GCC_CONFIGURE_CFLAGS+=" -DTARG_PPC"
			;;
			*)
				BUILD_SKIP_IPA=NO
				BUILD_ARCH=IA32
				BUILD_HOST=IA32
				CROSS_TARGET="$target"
			;;
		esac ;;
	sl*-*-linux*)
		MACHINE_TYPE=SL
		BUILD_TARGET=SL
		BUILD_ABI=N32BIT
		BUILD_ARCH=MIPS4
		BUILD_VENDOR=SL
		CYGNUS_CONFIGURE_HOST=i686-gnu-linux
		GCC_CONFIGURE_HOST=mips-redhat-linux
		CYGNUS_CONFIGURE_TARG=mipsel-elf-linux
		GCC_CONFIGURE_TARG=mips-redhat-linux
		GCC_CONFIGURE_CFLAGS+=" -m32 -DTARG_SL"
		CYGNUS_CONFIGURE_CFLAGS=" -m32"
		BUILD_MULTILIB=NO
		;;
	*)
		AC_MSG_ERROR(["open64 is not supported on $target"])
		;;
esac

if test "${BUILD_TARGET}" = "SL"; then
   BUILD_GNU3="YES"
   BUILD_FORTRAN="NO"
fi

if test "${BUILD_TARGET}" = "PPC32"; then
   BUILD_GNU3="NO"
   BUILD_FORTRAN="NO"
fi

UNAME_MACHINE=`uname -m`
if test "$UNAME_MACHINE" = "x86_64"; then
   BUILD_MULTILIB=YES
fi
# Set default values for LIB_* and LIB2_* variables that were not set above.
if test "$LIB_MACHINE_TYPE" = ""; then
  LIB_MACHINE_TYPE=$MACHINE_TYPE
fi
if test "$LIB_BUILD_TARGET" = ""; then
  LIB_BUILD_TARGET=$BUILD_TARGET
fi
if test "$LIB_BUILD_ABI" = ""; then
  LIB_BUILD_ABI=$BUILD_ABI
fi
if test "$LIB_BUILD_ARCH" = ""; then
  LIB_BUILD_ARCH=$BUILD_ARCH
fi
if test "$LIB_BUILD_HOST" = ""; then
  LIB_BUILD_HOST=$BUILD_HOST
fi

if test "$LIB2_MACHINE_TYPE" = ""; then
  LIB2_MACHINE_TYPE=$MACHINE_TYPE
fi
if test "$LIB2_BUILD_TARGET" = ""; then
  LIB2_BUILD_TARGET=$BUILD_TARGET
fi
if test "$LIB2_BUILD_ABI" = ""; then
  LIB2_BUILD_ABI=$BUILD_ABI
fi
if test "$LIB2_BUILD_ARCH" = ""; then
  LIB2_BUILD_ARCH=$BUILD_ARCH
fi
if test "$LIB2_BUILD_HOST" = ""; then
  LIB2_BUILD_HOST=$BUILD_HOST
fi

# Export these variables so they can be used in the 'subconfigures run in
# osprey/cygnus and osprey-gcc-4.2.0.
export CYGNUS_CONFIGURE_COMPILER
export CYGNUS_CONFIGURE_HOST
export CYGNUS_CONFIGURE_TARG
export CYGNUS_CONFIGURE_CFLAGS
export GCC_CONFIGURE_COMPILER
export GCC_CONFIGURE_HOST
export GCC_CONFIGURE_TARG
export GCC_CONFIGURE_CFLAGS

# Variables to replace in the configurable files.
AC_SUBST([MACHINE_TYPE])
AC_SUBST([CROSS_TARGET])
AC_SUBST([BUILD_ABI])
AC_SUBST([BUILD_ARCH])
AC_SUBST([BUILD_HOST])
AC_SUBST([BUILD_TARGET])
AC_SUBST([BUILD_VENDOR])
AC_SUBST([BUILD_OPTIMIZE])
AC_SUBST([BUILD_FFE_OPTIMIZE])
AC_SUBST([BUILD_LIB_OPTIMIZE])
AC_SUBST([BUILD_COMPILER])
AC_SUBST([BUILD_BOTH])
AC_SUBST([LIB_MACHINE_TYPE])
AC_SUBST([LIB_BUILD_TARGET])
AC_SUBST([LIB_BUILD_ABI])
AC_SUBST([LIB_BUILD_ARCH])
AC_SUBST([LIB_BUILD_HOST])
AC_SUBST([LIB2_MACHINE_TYPE])
AC_SUBST([LIB2_BUILD_TARGET])
AC_SUBST([LIB2_BUILD_ABI])
AC_SUBST([LIB2_BUILD_ARCH])
AC_SUBST([LIB2_BUILD_HOST])
AC_SUBST([BUILD_MULTILIB])
AC_SUBST([COMPILER_TARG_DIR])
AC_SUBST([TARG_INFO_NAME])
AC_SUBST([BUILD_GNU3])
AC_SUBST([BUILD_FORTRAN])
AC_SUBST([WHIRL_HAS_ID_FIELD])
AC_SUBST([BUILD_SKIP_IPA])
AC_SUBST([BUILD_TYPE])
AC_SUBST([GCC_CONFIGURE_TARG])
AC_SUBST([GCC_DIR])

# List of configurable files to create in object directory.
AC_CONFIG_FILES([
  Makefile
  osprey/Makefile.gsetup
  osprey/targdir/libspin/Makefile
  osprey/targdir/libspin_4_2_0/Makefile
  osprey/targdir/include/Makefile
  osprey/targdir/driver/Makefile
  osprey/targdir/libiberty/Makefile
  osprey/targdir/targ_info/Makefile
  osprey/targdir/be/Makefile
  osprey/targdir/libelf/Makefile
  osprey/targdir/libelfutil/Makefile
  osprey/targdir/libdwarf/Makefile
  osprey/targdir/libunwindP/Makefile
  osprey/targdir/cg/Makefile
  osprey/targdir/wopt/Makefile
  osprey/targdir/ipl/Makefile
  osprey/targdir/lno/Makefile
  osprey/targdir/lw_inline/Makefile
  osprey/targdir/ipa/Makefile
  osprey/targdir/whirl2c/Makefile
  osprey/targdir/libcomutil/Makefile
  osprey/targdir/ir_tools/Makefile
  osprey/targdir/libcmplrs/Makefile
  osprey/targdir/wgen/Makefile
  osprey/targdir/libcif/Makefile
  osprey/targdir/arith/Makefile
  osprey/targdir/orc_ict/Makefile
  osprey/targdir/orc_intel/Makefile
  osprey/targdir/libkapi/Makefile
])

if test "${BUILD_FORTRAN}" = "YES"; then
 AC_CONFIG_FILES([
  osprey/targdir/whirl2f/Makefile      
  osprey/targdir/crayf90/Makefile
  osprey/targdir/crayf90/fe90/Makefile
  osprey/targdir/crayf90/libf90sgi/Makefile
  osprey/targdir/crayf90/sgi/Makefile
 ])
fi

if test "${BUILD_GNU3}" = "YES"; then
  AC_CONFIG_FILES([
    osprey/targdir/gccfe/Makefile
    osprey/targdir/gccfe/gnu/Makefile
    osprey/targdir/g++fe/Makefile
    osprey/targdir/g++fe/gnu/Makefile
  ])
fi

# The directories created under targdir_lib (and targdir_lib2) also
# need to be listed in LIBLIST.  This list is used in targdir_lib/Makefile.in
# to control the order of the library builds.  When adding a directory
# under targdir_lib you need to add it to AC_CONFIG_FILES and add it to
# LIBLIST.

if test "${BUILD_TARGET}" = "SL"; then
  LIBLIST="include"
elif test "${BUILD_TARGET}" = "IA64"; then
  LIBLIST="include libopenmp libmv"
  if test "${BUILD_FORTRAN}" = "YES"; then
     LIBLIST="$LIBLIST libfortran libu"
  fi
else
  LIBLIST="include libopenmp libhugetlbfs libmv"
  if test "${BUILD_FORTRAN}" = "YES"; then
     LIBLIST="$LIBLIST libfortran libu"
  fi
fi

AC_CONFIG_FILES([
  osprey/targdir_lib/Makefile
  osprey/targdir_lib/include/Makefile
])

if test "${BUILD_TARGET}" = "SL"; then
AC_CONFIG_FILES([
  osprey/targdir_lib/libinstr/Makefile:osprey/targdir_lib/libinstr2/Makefile.in
])
else
AC_CONFIG_FILES([
  osprey/targdir_lib/libopenmp/Makefile
  osprey/targdir_lib/libhugetlbfs/Makefile
  osprey/targdir_lib/libmv/Makefile
])
fi

if test "${BUILD_FORTRAN}" = "YES"; then
   AC_CONFIG_FILES([
   osprey/targdir_lib/libfortran/Makefile
   osprey/targdir_lib/libu/Makefile
])
fi

if test "${BUILD_TARGET}" = "X8664" || test "${BUILD_TARGET}" = "IA32"; then
  LIBLIST="libopen64rt libacml_mv $LIBLIST libinstr2"
  AC_CONFIG_FILES([
    osprey/targdir_lib/libopen64rt/Makefile
    osprey/targdir_lib/libinstr2/Makefile
    osprey/targdir_lib/libacml_mv/Makefile
  ])
fi
if test "${BUILD_TARGET}" = "IA64" ; then
  LIBLIST="$LIBLIST init"
  AC_CONFIG_FILES([
    osprey/targdir_lib/init/Makefile:osprey/targdir_lib/init/Makefile.in
  ])
fi   

if test $BUILD_MULTILIB = "YES"; then
  AC_CONFIG_FILES([
    osprey/targdir_lib2/Makefile:osprey/targdir_lib/Makefile.in
    osprey/targdir_lib2/include/Makefile:osprey/targdir_lib/include/Makefile.in
    osprey/targdir_lib2/libopenmp/Makefile:osprey/targdir_lib/libopenmp/Makefile.in
    osprey/targdir_lib2/libhugetlbfs/Makefile:osprey/targdir_lib/libhugetlbfs/Makefile.in
    osprey/targdir_lib2/libmv/Makefile:osprey/targdir_lib/libmv/Makefile.in
  ])
  if test "${BUILD_FORTRAN}" = "YES"; then
    AC_CONFIG_FILES([
    osprey/targdir_lib2/libfortran/Makefile:osprey/targdir_lib/libfortran/Makefile.in
    osprey/targdir_lib2/libu/Makefile:osprey/targdir_lib/libu/Makefile.in
  ])
  fi
  if test "${BUILD_TARGET}" = "X8664" || test "${BUILD_TARGET}" = "IA32"; then
    AC_CONFIG_FILES([
      osprey/targdir_lib2/libopen64rt/Makefile:osprey/targdir_lib/libopen64rt/Makefile.in
      osprey/targdir_lib2/libinstr2/Makefile:osprey/targdir_lib/libinstr2/Makefile.in
      osprey/targdir_lib2/libacml_mv/Makefile:osprey/targdir_lib/libacml_mv/Makefile.in
    ])
  fi
fi

AC_SUBST([LIBLIST])

# Configure cygnus and GCC subdirectories.
AC_CONFIG_SUBDIRS(osprey/cygnus)
# We do not use $GCC_DIR because using AC_CONFIG_SUBDIRS with variables
# breaks "configure --help=recursive".  See autoconf documentation.
AC_CONFIG_SUBDIRS(osprey-gcc-4.2.0)

AC_OUTPUT
