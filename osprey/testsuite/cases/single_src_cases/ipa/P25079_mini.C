//This is the testcase of a merge error, which is fixed in Rev 1797
//This testcase is extracted from P25079@CCVS

#include <algorithm>

int myRand(int n) {
  return ((int) (std::rand()%n));
}

int main(void)
{
  char s01[] = "abc";
  for (int i=0; i<1000; i++) {
    std::random_shuffle(s01, s01+3, myRand);
  }
  return 0;
}
