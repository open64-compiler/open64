
extern	double	__sqrt(double);

#if defined(BUILD_OS_DARWIN) /* Mach-O doesn't support aliases */
extern  long double  __sqrtl(long double);
#pragma weak sqrtl
long double sqrtl(long double x) {
  return ( (long double)__sqrt((double)x) );
}
#elif defined(__GNUC__)
extern  long double  __sqrtl(long double);

long double    sqrtl() __attribute__ ((weak, alias ("__sqrtl")));

#endif

long double
__sqrtl(long double x)
{
	return ( (long double)__sqrt((double)x) );
}

