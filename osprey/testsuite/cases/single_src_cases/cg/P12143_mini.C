//OBJ
//This is the testcase of a merge error, which is fixed in Rev 1658
//This testcase is extracted from P12143@CCVS

int counter = 0;
class C {
public:
  int member;
  C() { member = counter++; }
  ~C() { counter--; }
};


void g(int);
int f(void) {
  C c[5];
  g(c[0].member);
}
