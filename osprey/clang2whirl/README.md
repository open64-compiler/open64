Clang based Open64 Front End
================
Copyright (C) 2019-2022 Xcalibyte (Shenzhen) Limited.

1. Prepare LLVM & Clang
----------------
1.1 Build from source
 $ tar xf llvm-x.y.z.src.tar.xz
 $ tar xf cfe-x.y.z.src.tar.xz
 $ ln -sf `pwd`/cfe-x.y.x.src llvm-x.y.z.src/tools/clang
 $ mkdir build && cd build
 $ cmake -DCMAKE_INSTALL_PREFIX=../x.y.z/debug \
         -DCMAKE_BUILD_TYPE=Debug  -DLLVM_TARGETS_TO_BUILD=host  \
         -DLLVM_USE_LINKER=gold  -DLLVM_ENABLE_LIBEDIT=OFF  \
         -DLLVM_ENABLE_ZLIB=OFF  -DLLVM_ENABLE_LIBPFM=OFF  \
         -DLLVM_ENABLE_LIBXML2=OFF  -DCLANG_ENABLE_STATIC_ANALYZER=OFF  \
         -DCLANG_ENABLE_ARCMT=OFF -DLLVM_ENABLE_TERMINFO=OFF  \
         -DLLVM_ENABLE_CRASH_OVERRIDES=OFF -DLLVM_ENABLE_PIC=OFF  \
         -DLLVM_ENABLE_BINDINGS=OFF -DLLVM_ENABLE_OCAMLDOC=OFF  \
         ../llvm-x.y.z.src
 $ make -j && make install

 For release build, change CMAKE_BUILD_TYPE from Debug to Release and CMAKE_INSTALL_PREFIX to ../x.y.z/release

 If debug version is used, set CLANG_HOME to path to debug version binaries
 $ export CLANG_HOME=`pwd`/../x.y.z/debug
 Or to use release version:
 $ export CLANG_HOME=`pwd`/../x.y.z/release

1.2 Copy prebuilt LLVM & Clang binaries
  $ mkdir clang-prebuilt && cd clang-prebuilt
  $ scp -r xman@10.10.3.12:/mnt/cephfs/clang-prebuilt/x.y.z x.y.z

  Set CLANG_HOME to build clangfe in Step 3
  $ export CLANG_HOME=`pwd`/x.y.z/debug
  Or:
  $ export CLANG_HOME=`pwd`/x.y.z/release

2. Clone and configure mastiff (Open64)
2.1 Clone the branch
  $ git clone -b clangfe http://git.xc5.io/git/xc5-sz/mastiff.git clangfe
  $ mkdir build && cd build
  $ ../clangfe/configure --prefix=/home/open64/clangfe --disable-fortran --build=x86_64-linux-gnu --target=x86_64-linux-gnu --with-build-optimize=DEBUG --with-build-product=OPEN64 --disable-multilib

  For release build, change --with-build-optimize from DEBUG to DEFAULT

3. Build clangfe
  $ make V=1 CLANG_HOME=<PATH_TO_CLANG_BINARIES>/x.y.z/release clangfe
  Or,
  $ export CLANG_HOME=<<PATH_TO_CLANG_BINARIES>/x.y.z/release
  $ make V=1 clangfe

  The final executable 'mapclang' is about 500MB+ if linked with debug Clang/LLVM libraries and about 50MB+ if linked with release Clang/LLVM.

4. Invoke clangfe directly
  $ ./mapclang -cc1 -emit-llvm a.c
  The command above will compile a.c and generate a.B. '-cc1' and '-emit-llvm' is necessary to generate the .B file.

  $ ./mapclang -cc1 -ast-dump a.c
  The command above will print the Clang AST on screen.

5. Invoke with open64 driver
  $ opencc -clang -show -keep hello.c
  This command will invoke clang front end, inline, backend, as and linker to generate the executable

6. Internal
  This front end does Clang AST to WHIRL, NOT LLVM IR to WHIRL. The implementation is we keep the interface for Clang CodeGen (which converts Clang AST to LLVM IR) unchanged but reimplement it to do Clang AST to WHIRL. Refer dummy_gen.cxx for the implementation. dummy_gen.cxx implements the Clang
CodeGen interface and invoke WhirlGenConsumer to do the real conversion.
  The driver.cxx and cc1_main.cxx is copied from Clang/tools/driver/driver.cpp and Clang/tools/driver/cc1_main.cpp respectively with a few modification to remove unused code and header files. The goal to include the two files is to keep the command line compatibility with clang at minimal efforts.

Appendix 1. Build in the original open64 source tree
  $ git clone -b clangfe http://git.xc5.io/git/xc5-sz/mastiff.git clangfe
  $ git clone https://github.com/open64-compiler/open64.git open64
  $ cd open64/osprey
  $ ln -sf ../../clangfe/osprey/clang2whirl .
  $ cd ../..
  $ mkdir build && cd build
  $ ../open64/configure --prefix=/home/open64/clangfe --disable-fortran --build=x86_64-linux-gnu --target=x86_64-linux-gnu --with-build-optimize=DEBUG --disable-multilib
  $ cd osprey/targdir
  $ mkdir clang2whirl && cd clang2whirl
  $ cat > Makefile
BUILD_BASE     = ../../../../open64/osprey/clang2whirl
BUILD_VARIANT  =
include ../../../osprey/Makefile.gsetup
  $ make


That's it :-)

