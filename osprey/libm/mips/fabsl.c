
extern	double	__fabs(double);

#if defined(BUILD_OS_DARWIN) /* Mach-O doesn't support aliases */
extern long double __fabsl(long double);
#pragma weak fabsl
long double fabsl(long double x) {
  return ( (long double)__fabs((double)x) );
}
#elif defined(__GNUC__)
extern  long double  __fabsl(long double);

long double    fabsl() __attribute__ ((weak, alias ("__fabsl")));

#endif

long double
__fabsl(long double x)
{
	return ( (long double)__fabs((double)x) );
}

