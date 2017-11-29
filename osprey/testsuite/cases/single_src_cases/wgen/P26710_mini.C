//This is the testcase of a bug emerged by merge, which is walk arounded by Rev 1770
//This testcase is extracted from P26710@CCVS
#include <iostream>
#include <complex>
using namespace std;

long double absreal();

int main() {
  cout << absreal() << endl;
  return 0;
}

long double absreal() {
  std::complex<long double> x(1, 0);
  return std::exp(x).real();
}
