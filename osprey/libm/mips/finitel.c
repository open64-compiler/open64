
extern	int	__finite(double);

#if defined(BUILD_OS_DARWIN) /* Mach-O doesn't support aliases */
extern int __finitel(long double);
#pragma weak finitel
int finitel(long double x) {
  return ( __finite((double)x) );
}
#elif defined(__GNUC__)
extern  int  __finitel(long double);

int    finitel() __attribute__ ((weak, alias ("__finitel")));

#endif

int
__finitel(long double x)
{
	return ( __finite((double)x) );
}

