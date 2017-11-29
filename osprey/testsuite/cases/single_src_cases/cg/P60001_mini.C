//OBJ
//This is the testcase of a merge error, which is fixed in Rev 1692
//This testcase is extracted from P60001@CCVS

float c = 1;

void g();

void f() {
  if (c > 10.0)
    g();
}
