//OBJ
//This is the testcase of a merge error, which is fixed in Rev 1806
//This testcase is extracted from P25054@CCVS
bool func();
extern char* s01;

inline void find_if(char* __first, char* __last) {
  if (__last == __first) {
    if (func()) 
      return;
  }
}

int main(void)
{
  find_if(s01, s01);
  return 0;
}
