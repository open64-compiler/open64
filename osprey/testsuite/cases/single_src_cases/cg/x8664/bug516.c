//OBJ
//CFLAGS: -O0 -m32 -mmmx 


typedef float __v2f __attribute__ ((__vector_size__ (8)));
__v2f
foo3 (__v2f a0 ,__v2f a1,__v2f a2,__v2f a3)
{
  return a0;
}
