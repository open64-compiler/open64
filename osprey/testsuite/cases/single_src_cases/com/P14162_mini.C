//OBJ
//This is the testcase of a merge error, which is fixed in Rev 1580
//This testcase is extracted from P14162@CCVS

struct Class123 {
  Class123() {}
};
 
template <class T> struct Class456 {
  Class456() {
    static T var789;
  }
};

void func123()
{
  Class456<Class123> var123;
}
