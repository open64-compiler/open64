
extern	double	__ceil(double);

#if defined(BUILD_OS_DARWIN) /* Mach-O doesn't support aliases */
extern long double __ceill(long double);
#pragma weak ceill
long double ceill(long double x) {
  return ( (long double)__ceil((double)x) );
}
#elif defined(__GNUC__)
extern  long double  __ceill(long double);

long double    ceill() __attribute__ ((weak, alias ("__ceill")));

#endif

long double
__ceill(long double x)
{
	return ( (long double)__ceil((double)x) );
}

