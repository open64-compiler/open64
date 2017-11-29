%define open64_name    open64
%define open64_version 4.2
%define open64_release 0
%define open64_prefix  /opt/open64
%define open64_bin_dir %{open64_prefix}/bin
%ifarch %{ix86}
%define open64_arch x86_64
%else
%define open64_arch %{_arch}
%endif
%define open64_lib_dir %{open64_prefix}/lib/gcc-lib/%{open64_arch}-open64-linux
%define __spec_install_post /usr/lib/rpm/brp-compress /usr/lib/rpm/brp-strip-comment-note %{nil}

Summary: The Open64 Compiler Suite v%{open64_version}
Name: %{open64_name}
Version: %{open64_version}
Release: %{open64_release}
License: GPL
Group: Development/Languages
Source: %{open64_name}-%{open64_version}-%{open64_release}.tar.gz
URL: http://www.open64.net
Prereq: /sbin/install-info
Requires: gcc >= 3.2
Prefix: %{open64_prefix}

%description
Open64 is the final result of research contributions from a number of compiler 
groups around the world. Formerly known as Pro64, Open64 was initially created 
by SGI and licensed under the GNU Public License (GPL). It was derived from 
SGI's MIPSPro compiler. 

Open64 also derives from work done by Intel Corp, in conjunction with the 
Chinese Academy of Sciences. They created the Open Research Compiler (ORC), 
a specially modified version of Open64 with custom modifications for 
researchers. These changes were later folded back into the main Open64 source 
tree in 2005. 

Open64 has been retargetted to a number of architectures. Pathscale modified 
Open64 to create EkoPath, a compiler for the AMD64 architecture. The University 
of Delaware's Computer Architecture and Parallel Systems Laboratory (CAPSL) 
modified Open64 to create the Kylin Compiler, a compiler for Intel's X-Scale 
architecture. CAPSL and Hewlet-Packard are currently working on Osprey, 
a project to replace Open64's front end (derived from GCC 2.95's frontend) 
with the current GCC front end. 

The Open64 compiler suite currently includes compilers for C, C++, and 
Fortran90/95 compilers for the IA-64 Linux ABI and API standards. This project 
is led by Shinming Liu at HP Inc. It is the result of partnership between 
Tsinghua University, Institute of Computing Technology at the Chinese Academy 
of Science, CAPSL research laboratory at the University of Delaware, 
Hewlett-Packard, Inc. and Google Inc.. We'd also like to acknowledge the 
contributions of PathScale Inc. and Sun Chan at SimpLight Nanoelectronics.


%files
%defattr(-,root,root)
%dir %{open64_prefix}
%dir %{open64_bin_dir}
%dir %{open64_prefix}/lib
%dir %{open64_prefix}/lib/gcc-lib
%dir %{open64_prefix}/lib/gcc-lib/%{open64_arch}-open64-linux
%dir %{open64_prefix}/lib/gcc-lib/%{open64_arch}-open64-linux/%{open64_version}

%{open64_bin_dir}/opencc
%{open64_bin_dir}/openCC
%{open64_bin_dir}/openf90
%{open64_bin_dir}/openf95
%{open64_bin_dir}/opencc-%{open64_version}
%{open64_bin_dir}/openCC-%{open64_version}
%{open64_bin_dir}/openf90-%{open64_version}
%{open64_bin_dir}/openf95-%{open64_version}
%{open64_bin_dir}/ir_b2a
%{open64_bin_dir}/gspin
%{open64_bin_dir}/gspin42
%{open64_bin_dir}/kopencc

%{open64_lib_dir}/%{open64_version}/kdriver
%{open64_lib_dir}/%{open64_version}/whirl2f.so
%{open64_lib_dir}/%{open64_version}/mfef95
%{open64_lib_dir}/%{open64_version}/ipl.so
%{open64_lib_dir}/%{open64_version}/gfecc
%{open64_lib_dir}/%{open64_version}/wgen
%{open64_lib_dir}/%{open64_version}/wgen42
%{open64_lib_dir}/%{open64_version}/ipa_link
%{open64_lib_dir}/%{open64_version}/cf95.cat
%{open64_lib_dir}/%{open64_version}/lno.so
%{open64_lib_dir}/%{open64_version}/whirl2f
%{open64_lib_dir}/%{open64_version}/wopt.so
%{open64_lib_dir}/%{open64_version}/cc1
%{open64_lib_dir}/%{open64_version}/cc142
%{open64_lib_dir}/%{open64_version}/cg.so
%{open64_lib_dir}/%{open64_version}/be
%{open64_lib_dir}/%{open64_version}/cc1plus
%{open64_lib_dir}/%{open64_version}/cc1plus42
%{open64_lib_dir}/%{open64_version}/ipa.so
%{open64_lib_dir}/%{open64_version}/gfec
%{open64_lib_dir}/%{open64_version}/whirl2c.so
%{open64_lib_dir}/%{open64_version}/be.so
%{open64_lib_dir}/%{open64_version}/whirl2c
%{open64_lib_dir}/%{open64_version}/inline
%{open64_lib_dir}/%{open64_version}/driver

%ifnarch %{ix86}
#%{open64_lib_dir}/%{open64_version}/libF77.a
%{open64_lib_dir}/%{open64_version}/libffio.a
%{open64_lib_dir}/%{open64_version}/libfortran.a
%{open64_lib_dir}/%{open64_version}/libinstr.a
#%{open64_lib_dir}/%{open64_version}/libmsgi.a
%{open64_lib_dir}/%{open64_version}/libmv.a
%{open64_lib_dir}/%{open64_version}/libopenmp.a
%endif

%{open64_bin_dir}/hpe.pl
%{open64_bin_dir}/cycount.pl

# ia64 specified files
%ifarch ia64
#%{open64_lib_dir}/%{open64_version}/libm.a
%{open64_lib_dir}/%{open64_version}/libcginstr.a
%{open64_lib_dir}/%{open64_version}/ftz.o
%{open64_lib_dir}/%{open64_version}/lib.exp
%{open64_lib_dir}/%{open64_version}/lib.cat
%{open64_lib_dir}/%{open64_version}/itanium.so
%{open64_lib_dir}/%{open64_version}/itanium2.so
%{open64_lib_dir}/%{open64_version}/orc_intel.so
%{open64_lib_dir}/%{open64_version}/orc_ict.so
%endif

# ia32/x8664 specified files
%ifarch %{ix86} x86_64
%dir %{open64_lib_dir}/%{open64_version}/32
%{open64_lib_dir}/%{open64_version}/opteron.so
%{open64_lib_dir}/%{open64_version}/em64t.so
%{open64_lib_dir}/%{open64_version}/barcelona.so
%{open64_lib_dir}/%{open64_version}/core.so
#%{open64_lib_dir}/%{open64_version}/32/libF77.a
%{open64_lib_dir}/%{open64_version}/32/libffio.a
%{open64_lib_dir}/%{open64_version}/32/libfortran.a
%{open64_lib_dir}/%{open64_version}/32/libinstr.a
#%{open64_lib_dir}/%{open64_version}/32/libmsgi.a
%{open64_lib_dir}/%{open64_version}/32/libmv.a
%{open64_lib_dir}/%{open64_version}/32/libopenmp.a
%endif

%prep
#%setup

%build


%install


%clean


%pre


%post
PHASEPATH=${RPM_INSTALL_PREFIX}/lib/gcc-lib/%{open64_arch}-open64-linux/%{open64_version}
INSTALL_DATA="/usr/bin/install -D -m 644"

# create symbol links
ln -s -f ${PHASEPATH}/be ${PHASEPATH}/ipl
ln -s -f ${PHASEPATH}/be ${PHASEPATH}/whirl2c_be
ln -s -f ${PHASEPATH}/be ${PHASEPATH}/whirl2f_be

# install gcc libraries
for i in libgcc.a libgcc_s.so libstdc++.a libstdc++.so; do
%ifarch ia64 x86_64
  F=`gcc --print-file-name $i`
  [ ! -z "$F" ] && [ -e $F ] && ${INSTALL_DATA} $F ${PHASEPATH}/$i
%endif

%ifarch %{ix86}
  F=`gcc --print-file-name $i`
  [ ! -z "$F" ] && [ -e $F ] && ${INSTALL_DATA} $F ${PHASEPATH}/32/$i
%endif

%ifarch x86_64
  F=`gcc -m32 --print-file-name $i`
  [ ! -z "$F" ] && [ -e $F ] && ${INSTALL_DATA} $F ${PHASEPATH}/32/$i
%endif

done

# install gcc startup files
for i in crtbegin.o crtend.o crtbeginS.o crtendS.o crtbeginT.o crtendT.o ; do

%ifarch ia64 x86_64
  F=`gcc --print-file-name $i`
  [ ! -z "$F" ] && [ -e $F ] && ${INSTALL_DATA} $F ${PHASEPATH}/$i
%endif

%ifarch %{ix86}
  F=`gcc --print-file-name $i`
  [ ! -z "$F" ] && [ -e $F ] && ${INSTALL_DATA} $F ${PHASEPATH}/32/$i
%endif

%ifarch x86_64
  F=`gcc -m32 --print-file-name $i`
  [ ! -z "$F" ] && [ -e $F ] && ${INSTALL_DATA} $F ${PHASEPATH}/32/$i
%endif

done

echo "Install Open64 Compiler Suite v%{open64_version} to ${RPM_INSTALL_PREFIX} successfully."

true

%preun
PHASEPATH=${RPM_INSTALL_PREFIX}/lib/gcc-lib/%{open64_arch}-open64-linux/%{open64_version}

#if [ "${1}" = "0" ]; then
#We do not support upgrade
rm -f ${PHASEPATH}/ipl
rm -f ${PHASEPATH}/whirl2c_be
rm -f ${PHASEPATH}/whirl2f_be

for i in libgcc.a libgcc_s.so libstdc++.a libstdc++.so; do
  rm -f ${PHASEPATH}/$i

%ifarch %{ix86} x86_64
  rm -f ${PHASEPATH}/32/$i
%endif

done

for i in crtbegin.o crtend.o crtbeginS.o crtendS.o crtbeginT.o crtendT.o ; do
  rm -f ${PHASEPATH}/$i

%ifarch %{ix86} x86_64
  rm -f ${PHASEPATH}/32/$i
%endif

done
#fi

true 

%postun

%changelog
* Wed Apr 16 2008 laijx jianxin.lai@hp.com
-Add barcelona.so, core.so for IA32/x86_64
* Thu Mar 20 2008 laijx jianxin.lai@hp.com
-Open64 4.2 release 
-Add libgcc_s.so, GNU 4.2 FE stuffs: cc142, cc1plus42, wgen42, gspin42
* Fri Nov 30 2007 laijx jianxin.lai@hp.com
-Add ftz.o for IA64 
* Wed Nov 28 2007 laijx jianxin.lai@hp.com
-Remove libF77.a, libm.a, libmsgi.a, Append libopenmp.a
* Tue Nov 27 2007 laijx jianxin.lai@hp.com
-Update installation for gcc libraries
* Tue Jun 5 2007 laijx jianxin.lai@hp.com
-initial version
