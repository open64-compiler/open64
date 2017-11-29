
extern	double	__trunc(double);

#if defined(BUILD_OS_DARWIN) /* Mach-O doesn't support aliases */
extern long double __truncl(long double);
#pragma weak truncl
long double truncl(long double x) {
  return ( (long double)__trunc((double)x) );
}
#elif defined(__GNUC__)
extern  long double  __truncl(long double);

long double    truncl() __attribute__ ((weak, alias ("__truncl")));

#endif

long double
__truncl(long double x)
{
	return ( (long double)__trunc((double)x) );
}

