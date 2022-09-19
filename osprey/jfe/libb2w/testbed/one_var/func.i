# 1 "func.c"
# 1 "<built-in>"
# 1 "<command-line>"
# 1 "func.c"
int foo(int oneparm){
  return oneparm + oneparm;
}

int main(){
  int myvar = 2;
  myvar = foo(3);
  return myvar;
}
