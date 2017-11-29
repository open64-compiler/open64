#include <stdio.h>
static unsigned int next_rand1 = 1;
static int seedi=1403745862;
int main() {
       int i;
       double tmp=0.0;
       for (i = 0; i < 10; ++i) { 
         tmp += POV_RAND();
       }
       if (tmp != 197643.0) {//just simply compare the result ignoring precision
         printf("expect:197643.000000, but got:%f\n",tmp);//errMSG,return 1
         return 1;
       }
       return 0;
}
double spec_rand(void)
{
int lo; 
int hi;
int test;

hi = seedi / 127773L;
lo = seedi % 127773L;
test = 16807L * lo - 2836L * hi;
if (test > 0) {
   seedi = test;
} else {
   seedi = test + 2147483647L;
}
return ( (double) seedi / 2147483647L);
}
int POV_RAND() {
next_rand1 = spec_rand() * 2147483647L;
return((int)(next_rand1 >> 16) & 0x7FFF);
}
/*
opencc -O2 -ipa -show -keep -o test bug_336_case.c 
suppose POV_RAND return values to double:
7650.000000
26299.000000
11024.000000
25994.000000
30359.000000
24737.000000
11156.000000
5298.000000
25054.000000
30072.000000
wrong POV_RAND return values to double(revision 1124 built by gcc 3.4.3 O0) :
7650.000000
-6469.000000
11024.000000
-6774.000000
-2409.000000
-8031.000000
11156.000000
5298.000000
-7714.000000
-2696.000000
*/
