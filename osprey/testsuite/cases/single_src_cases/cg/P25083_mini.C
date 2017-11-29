//OBJ
//This is the testcase of a merge error, which is fixed in Rev 1692
//This testcase is extracted from P25083@CCVS

unsigned gcd(unsigned m, unsigned n) {
  while (n != 0) {
    unsigned t = m % n;
    m = n;
    n = t;
  }
  return m;
}
