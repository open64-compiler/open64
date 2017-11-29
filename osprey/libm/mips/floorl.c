
extern	double	__floor(double);

#if defined(BUILD_OS_DARWIN) /* Mach-O doesn't support aliases */
extern long double __floorl(long double);
#pragma weak floorl
long double floorl(long double x) {
  return ( (long double)__floor((double)x) );
}
#elif defined(__GNUC__)
extern  long double  __floorl(long double);

long double    floorl() __attribute__ ((weak, alias ("__floorl")));

#endif

long double
__floorl(long double x)
{
	return ( (long double)__floor((double)x) );
}

