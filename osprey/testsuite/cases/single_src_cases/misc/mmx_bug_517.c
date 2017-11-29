//CFLAGS:-O0 -c -m32 -mmmx
//This is the testcase of bug #514

typedef int __m64 __attribute__ ((__vector_size__ (8)));
typedef short __v4hi __attribute__ ((__vector_size__ (8)));
__m64
    _mm_sll_pi16 (__m64 __m, __m64 __count)
{
          return (__m64) __builtin_ia32_psllw ((__v4hi)__m, (long long)__count);
}


