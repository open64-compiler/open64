//OBJ
//This is the testcase of a merge error, which is fixed in Rev 1690
//This testcase is extracted from P08124@CCVS

struct X {
  X() { }
};

void f() {
  static X VAR03;
}
