//OBJ
//This is the testcase of a merge error, which is fixed in Rev 1567
//This testcase is extracted from P22398@CCVS

void bar(long double, long double, long double)
{
}

void foo()
{
  bar(0, 0, 0);
}
