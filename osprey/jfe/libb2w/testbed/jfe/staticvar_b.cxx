static int MyVar1 = 98765432;
static int MyVar2 = 2929292929;

void foo(){
  int a = 0;
  a += MyVar1;
}

#ifdef TESTCLASS
class MyClassA {
  int myclass_fieldA;
};

MyClassA myinstance1; 

#endif
