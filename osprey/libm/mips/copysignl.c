
extern	double	__copysign(double, double);

#if defined(BUILD_OS_DARWIN) /* Mach-O doesn't support aliases */
extern long double __copysignl(long double, long double);
#pragma weak copysignl
long double copysignl(long double x, long double y) {
  return ( (long double)__copysign((double)x, (double)y) );
}
#elif defined(__GNUC__)
extern  long double  __copysignl(long double, long double);

long double    copysignl() __attribute__ ((weak, alias ("__copysignl")));

#endif

long double
__copysignl(long double x, long double y)
{
	return ( (long double)__copysign((double)x, (double)y) );
}

