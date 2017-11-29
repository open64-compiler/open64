//CFLAGS:-O0 -c -m32 -mmmx
//This is the testcase of bug #514
//This testcase is from gcc regression testsuit
#define static
#define __inline
#include <mmintrin.h>
