//OBJ
//This is the testcase of a merge error, which is fixed in Rev 1671
//This testcase is extracted from P26099@CCVS

#include <complex>
typedef std::complex<float> Fcx;

float iif00(const Fcx& arg00 ){
  return std::abs<float>(arg00);
}
