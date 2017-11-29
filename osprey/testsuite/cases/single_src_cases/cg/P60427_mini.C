//OBJ
//This is the testcase of a merge error, which is fixed in Rev 1676
//This testcase is extracted from P60427@CCVS

void f()
{
  int i, j;
  for (i = 3, j = 19; i < j; i++, j--);
  if ((i != 11) || (j != 11))
    return;
}
