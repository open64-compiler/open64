//This is the testcase of a merge error, which is fixed in Rev 1810
//This testcase is extracted from 254.gap@SPEC2000

#include <stdio.h>

int i32 = -1;

int main() {
  unsigned long u64 = (unsigned long)(i32 >> 1);
  int i32_2 = ((long)u64) >> 1;
  printf("%d\n", i32_2);
  return 0;
}
