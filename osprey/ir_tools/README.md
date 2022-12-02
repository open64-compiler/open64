# IR Tools
---
Open64 provides several handy tools for analyzing or transforming IR.

## Building
---
Build open64 by running the following commands:
```shell
# fetch the source from github
git clone https://github.com/open64-compiler/open64.git
git checkout develop

# create a temporary build directory
mkdir build && cd build

# set $CLANG_HOME to be the path to the LLVM library
# for example: export CLANG_HOME=/usr/lib/llvm-15/
export CLANG_HOME=/PATH/TO/YOUR/LLVM/DIRECTORY
export PATH=$CLANG_HOME/bin/:$PATH

# make sure that you have llvm-config in the PATH
llvm-config --version

../open64/configure --host=x86_64-linux-gnu --target=x86_64-linux-gnu --disable-fortran --with-build-product=OPEN64 --with-build-optimize=DEBUG --disable-multilib --prefix=/usr/local/open64
make && make install

# you can add binarys of open64 in the PATH
export PATH=/usr/local/open64/bin/:$PATH
```

Then you can find the following useful IR tools in `osprey/targdir/ir_tools`:
```shell
ls -l -I "*.o" -I "*.d" osprey/targdir/ir_tools

lrwxr-xr-x 1 root root         6 Nov  4 15:19 ir_all -> ir_b2a
-rwxr-xr-x 1 root root   9643232 Nov  4 15:18 ir_b2a
-rwxr-xr-x 1 root root   4097696 Nov  4 15:18 irbuild
lrwxr-xr-x 1 root root         6 Nov  4 15:19 ir_sel -> ir_b2a
-rwxr-xr-x 1 root root   9479584 Nov  4 15:19 ir_size
-rwxr-xr-x 1 root root  15408768 Nov  4 15:19 mpl2whirl
-rwxr-xr-x 1 root root   9656696 Nov  4 15:18 uwasm_nwrap
-rwxr-xr-x 1 root root 343480504 Nov  4 15:30 w2ll
-rwxr-xr-x 1 root root  14931120 Nov  4 15:19 whirl2mpl
```

## WHIRL2LLVM
WHIRL2LLVM(w2ll) converts the WHIRL IR file(.B/.I/O) to the LLVM IR(.ll/.bc). This tool is still under developing and more modern language features of C++ will be supported soon!

Usage:

Use the following C source code as input:
```c
// hello.c
#include <stdio.h>
int main() {
    printf("hello world\n");
    return 0;
}
```

First, use open64 to generate .B/.I/.O files:
```shell
/usr/local/open64/bin/openCC -clang -emit-llvm -fno-exceptions -O3 -OPT:malloc_alg=off -PHASE:c=off -c -sw -kp hello.c
```

Next, use `w2ll` to generate the LLVM IR file.
```shell
w2ll hello.O -all
```
Then you can get the following output:

```llvm
; ModuleID = 'a.O'
source_filename = "a.O"

@str = private unnamed_addr constant [12 x i8] c"hello world\00", align 1

; Function Attrs: nounwind
declare signext i32 @printf(ptr, ...) #0

; Function Attrs: nounwind
define signext i32 @main() #0 {
entry:
  %puts = call i32 @puts(ptr nonnull @str)
  ret i32 0
}

; Function Attrs: nofree nounwind
declare noundef i32 @puts(ptr nocapture noundef readonly) #1

attributes #0 = { nounwind "target-features"="+a,+c,+d,+f,+m,+relax" }
attributes #1 = { nofree nounwind }
```

## IR_B2A
ir_b2a can convert the WHIRL IR from the bytecode form to the ASCII form.

Usage:

Still use the `hello.O` as input:
```shell
ir_b2a a.O
```

Then we can get the following dump:
```
 LOC 0 0 source files:	1	"/media/psf/Home/code/xc5/open64_build/a.c"
 LOC 0 0 source files:	2	"/usr/include/stdio.h"
FUNC_ENTRY <1,50,main> {line: 1/2}
BODY
 BLOCK {line: 0/0}
 PRAGMA 0 72 <null-st> 0 (0x0) # WOPT_FINISHED_OPTIMIZATION {line: 0/0}
 END_BLOCK
 BLOCK {line: 0/0}
 END_BLOCK
 BLOCK {line: 1/2}
 PRAGMA 0 119 <null-st> 0 (0x0) # PREAMBLE_END {line: 1/2}
   U8LDA 0 <1,52,(13_bytes)_"hello_world\n\000"> T<57,anon_ptr.,8>
  U8PARM 2 T<55,anon_ptr.,8> #  by_value
 VCALL 126 <1,51,printf> # flags 0x7e {line: 1/3}
 RETURN {line: 1/4}
 END_BLOCK
```

We can also dump the `symbol table` of the WHIRL IR:
```shell
ir_b2a -st2 a.O
```

## TODO
---
The document of the following tools is coming.
- ir_a2b
- whirl2mpl
- ir_size
- ...


