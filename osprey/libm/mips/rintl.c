
extern	double	__rint(double);

#if defined(BUILD_OS_DARWIN) /* Mach-O doesn't support aliases */
extern long double __rintl(long double);
#pragma weak rintl
long double rintl(long double x) {
  return ( (long double)__rint((double)x) );
}
#elif defined(__GNUC__)
extern  long double  __rintl(long double);

long double    rintl() __attribute__ ((weak, alias ("__rintl")));

#endif

long double
__rintl(long double x)
{
	return ( (long double)__rint((double)x) );
}

