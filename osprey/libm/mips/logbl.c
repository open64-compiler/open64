
extern	double	__logb(double);

#if defined(BUILD_OS_DARWIN) /* Mach-O doesn't support aliases */
extern long double __logbl(long double);
#pragma weak logbl
long double logbl(long double x) {
  return ( (long double)__logb((double)x) );
}
#elif defined(__GNUC__)
extern  long double  __logbl(long double);

long double    logbl() __attribute__ ((weak, alias ("__logbl")));

#endif

long double
__logbl(long double x)
{
	return ( (long double)__logb((double)x) );
}

