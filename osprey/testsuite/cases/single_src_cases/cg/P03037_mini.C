//OBJ
//This is the testcase of a merge error, which is fixed in Rev 1657
//This testcase is extracted from P03037@CCVS

int flag = 0;

struct S {
  S() {}
  ~S() { flag++; }
};

int main(void)
{

  S s1;

  if (flag == 0)
    return 1;

  return 0;

}
