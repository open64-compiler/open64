//OBJ
//This is the testcase of a merge error, which is fixed in Rev 1580
//This testcase is extracted from G04224@CCVS

int f()
{
  long double e = 1.0L;
  int x = 1;
  return ((x ? e : 2.0L) != e);
}
