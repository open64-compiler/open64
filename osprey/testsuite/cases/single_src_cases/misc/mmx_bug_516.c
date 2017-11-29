//CFLAGS:-O0 -c -m32 -mmmx
//This is the testcase of bug #516

typedef float __v2f __attribute__ ((__vector_size__ (8)));
__v2f
foo3 (__v2f a0,__v2f a1,__v2f a2,__v2f a3)
{
        return a0;
}

