//OBJ
//This is the testcase of bug #443
//This testcase is extracted from P26056@CCVS

inline long double
pow(long double __x, int __n) {
  return __builtin_powil(__x, __n);
}


long double      x0 = -10.0L;
int      z0 = -4.0;
long double      x1 = -10.0L;
int      z1 = 3.0;
long double      val;
long double      low;

int
main( void )
{
  val = pow(x0, z0);
  if (val < low) {
    return 1;
  }
  val = pow(x1, z1);
}
