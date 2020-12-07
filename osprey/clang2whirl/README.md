Clang based Open64 Front End
================
Copyright (C) 2019, 2020 Xcalibyte Limited, Inc.  All Rights Reserved.

1. Prepare LLVM & Clang and build from source
 So far CLANG 7.0.1 and 11.0.0 are supported. Replace x.y.z with 7.0.1 or 11.0.0 below.

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

2. Build clangfe (the C/C++ front-end generating whirl) in the open64 source tree:

2.1. Start with public version of open64
  $ git clone https://github.com/open64-compiler/open64.git open64
  $ mkdir obj && cd obj
  $ ../open64/configure --disable-fortran --build=x86_64-linux-gnu --target=x86_64-linux-gnu --with-build-optimize=DEBUG --disable-multilib
  $ make
  $ make install (to install everything in /usr/local/bin)

  With either builds, the final executable 'clangfe' is about 500MB+ if linked with debug Clang/LLVM libraries and about 50MB+ if linked with release Clang/LLVM.

3. Invoke clangfe directly
  $ ./clangfe -cc1 -emit-llvm a.c
  The command above will compile a.c and generate a.B. '-cc1' and '-emit-llvm' is necessary to generate the .B file.

  $ ./clangfe -cc1 -ast-dump a.c
  The command above will print the Clang AST on screen.

4. Invoke with open64 driver (only if built using 2.1)
  $ opencc -clang -show -keep hello.c
  This command will invoke clang front end, inline, backend, as and linker to generate the executable.
  If opencc is built from open64ark, this will generate wrong x86-64 code because there were hacks to tweak the generated whirl to adhere to aarch64 ABI.

5. Internal
  This front end does Clang AST to WHIRL, NOT LLVM IR to WHIRL. The implementation is we keep the interface for Clang CodeGen (which converts Clang AST to LLVM IR) unchanged but reimplement it to do Clang AST to WHIRL. Refer whirl_gen.cxx for the implementation. whirl_gen.cxx implements the Clang
CodeGen interface and invoke WhirlGenConsumer to do the real conversion.
  The driver.cxx and cc1_main.cxx is copied from Clang/tools/driver/driver.cpp and Clang/tools/driver/cc1_main.cpp respectively with a few modification to remove unused code and header files. The goal to include the two files is to keep the command line compatibility with clang at minimal efforts.
