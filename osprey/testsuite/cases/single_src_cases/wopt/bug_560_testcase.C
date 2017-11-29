//CFLAGS: -O2 
//openCC -O3 bug_560_testcase.C
//get wrong result


#include<stdio.h>
void bar()
{
}
struct OBJECT2
{
  int Type;
};
typedef struct OBJECT2 OBJ2;

int foo(OBJ2 *Object)
{
  if (Object->Type & 1)
  {
    bar();
    printf("Error!!Object->Type=%d\n", Object->Type);
    return 1;
  }
  return 0;
}

int main() {
  OBJ2 obj;
  obj.Type=4;
  return foo(&obj);
}
