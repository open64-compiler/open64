//CXXFLAGS: -gnu3
//This is the testcase of a merge error, which is fixed in Rev 1717
//This testcase is extracted from P70048@CCVS

#include <stdio.h>
#include <math.h>
double x0 = 0.5;

int main() {
  printf("%.6lf\n", x0 + 0.5);
  return 0;
}
