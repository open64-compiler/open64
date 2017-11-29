Summary: SGI's IA-64 compiler system (ALPHA_NUMBER).
Name: pro64
Version: 0.01.0
Release: 13
Copyright: Copyright 2000, 2001 Silicon Graphics, Inc.  All rights reserved.
Group: Development/Languages
Prereq: /sbin/install-info

%description
This package contains SGI's IA-64 compiler system which includes a modified
EGCS C front-end, the SGI optimizing back-end and all appropriate object
tools updated to support 64-bit LSB code generation. It also contains
the SGI Fortran90 front-end and fortran runtime libraries and the SGI
version of libm. It is meant to be installed under the NUE environment.

%package pro64
Summary: SGI's IA64 compiler system.
Group: Development/Languages

%description pro64
This package contains SGI's IA-64 compiler system which includes a modified
EGCS C front-end, the SGI optimizing back-end and all appropriate object
tools updated to support 64-bit LSB code generation. It also contains
the SGI Fortran90 front-end and fortran runtime libraries and the SGI
version of libm. It is meant to be installed under the NUE environment
or on IA-64 hardware.


%prep

%build

%install
chmod -R g-w /usr
chown -R root.root /usr

%clean

%post

%preun

%files
%dir /usr
%dir /usr/bin
%dir /usr/local
%dir /usr/local/bin
%dir /usr/lib
%dir /usr/lib/gcc-lib
%dir /usr/lib/gcc-lib/ia64-sgi-linux
%dir /usr/lib/gcc-lib/ia64-sgi-linux/sgicc-1.0
%dir /usr/lib/gcc-lib/ia64-sgi-linux/sgicc-1.0/include
%dir /usr/man
%dir /usr/man/man1
/usr/bin/sgicc
/usr/bin/sgiCC
/usr/bin/sgi++
/usr/bin/sgif90
/usr/local/bin/ir_b2a
/usr/lib/libfortran.a
/usr/lib/libffio.a
/usr/lib/libmsgi.a
/usr/lib/libmv.a
/usr/man/man1/sgicc.1
/usr/man/man1/sgiCC.1
/usr/man/man1/sgif90.1
/usr/lib/gcc-lib/ia64-sgi-linux/sgicc-1.0/be
/usr/lib/gcc-lib/ia64-sgi-linux/sgicc-1.0/be.so
/usr/lib/gcc-lib/ia64-sgi-linux/sgicc-1.0/cf90.cat
/usr/lib/gcc-lib/ia64-sgi-linux/sgicc-1.0/cg.so
/usr/lib/gcc-lib/ia64-sgi-linux/sgicc-1.0/driver
/usr/lib/gcc-lib/ia64-sgi-linux/sgicc-1.0/gfec
/usr/lib/gcc-lib/ia64-sgi-linux/sgicc-1.0/gfecc
/usr/lib/gcc-lib/ia64-sgi-linux/sgicc-1.0/inline
/usr/lib/gcc-lib/ia64-sgi-linux/sgicc-1.0/ipa.so
/usr/lib/gcc-lib/ia64-sgi-linux/sgicc-1.0/ipl.so
/usr/lib/gcc-lib/ia64-sgi-linux/sgicc-1.0/ipl
/usr/lib/gcc-lib/ia64-sgi-linux/sgicc-1.0/itanium.so
/usr/lib/gcc-lib/ia64-sgi-linux/sgicc-1.0/lib.cat
/usr/lib/gcc-lib/ia64-sgi-linux/sgicc-1.0/lib.exp
/usr/lib/gcc-lib/ia64-sgi-linux/sgicc-1.0/lno.so
/usr/lib/gcc-lib/ia64-sgi-linux/sgicc-1.0/mfef90
/usr/lib/gcc-lib/ia64-sgi-linux/sgicc-1.0/whirl2c
/usr/lib/gcc-lib/ia64-sgi-linux/sgicc-1.0/whirl2c.so
/usr/lib/gcc-lib/ia64-sgi-linux/sgicc-1.0/whirl2f
/usr/lib/gcc-lib/ia64-sgi-linux/sgicc-1.0/whirl2f.so
/usr/lib/gcc-lib/ia64-sgi-linux/sgicc-1.0/wopt.so
/usr/lib/gcc-lib/ia64-sgi-linux/sgicc-1.0/libgcc.a
/usr/lib/gcc-lib/ia64-sgi-linux/sgicc-1.0/libstdc++.a
/usr/lib/gcc-lib/ia64-sgi-linux/sgicc-1.0/libinstr.a
/usr/lib/gcc-lib/ia64-sgi-linux/sgicc-1.0/ftz.o
/usr/lib/gcc-lib/ia64-sgi-linux/sgicc-1.0/libstdc++.a.2.10.0
/usr/lib/gcc-lib/ia64-sgi-linux/sgicc-1.0/include/cxxabi.h
/usr/lib/gcc-lib/ia64-sgi-linux/sgicc-1.0/include/exception
/usr/lib/gcc-lib/ia64-sgi-linux/sgicc-1.0/include/float.h
/usr/lib/gcc-lib/ia64-sgi-linux/sgicc-1.0/include/ia64intrin.h
/usr/lib/gcc-lib/ia64-sgi-linux/sgicc-1.0/include/iso646.h
/usr/lib/gcc-lib/ia64-sgi-linux/sgicc-1.0/include/limits.h
/usr/lib/gcc-lib/ia64-sgi-linux/sgicc-1.0/include/new
/usr/lib/gcc-lib/ia64-sgi-linux/sgicc-1.0/include/new.h
/usr/lib/gcc-lib/ia64-sgi-linux/sgicc-1.0/include/proto.h
/usr/lib/gcc-lib/ia64-sgi-linux/sgicc-1.0/include/stdarg.h
/usr/lib/gcc-lib/ia64-sgi-linux/sgicc-1.0/include/stdbool.h
/usr/lib/gcc-lib/ia64-sgi-linux/sgicc-1.0/include/stddef.h
/usr/lib/gcc-lib/ia64-sgi-linux/sgicc-1.0/include/syslimits.h
/usr/lib/gcc-lib/ia64-sgi-linux/sgicc-1.0/include/typeinfo
/usr/lib/gcc-lib/ia64-sgi-linux/sgicc-1.0/include/varargs.h
/usr/lib/gcc-lib/ia64-sgi-linux/sgicc-1.0/ipa_link
