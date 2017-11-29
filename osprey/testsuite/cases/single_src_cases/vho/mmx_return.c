//PLATFORM: x86_64
//OBJ
//#define __m64 long long
#ifndef __m64
typedef int __m64 __attribute__ ((__vector_size__ (8)));
#endif
extern __m64 mm;
__m64 foo()
{
        return mm;
}

int main()
{
        __m64 m = foo();
        return 0;
}


