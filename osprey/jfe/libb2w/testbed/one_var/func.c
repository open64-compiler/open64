int foo(int oneparm){
  return oneparm + oneparm;
}

int main(){
  int myvar = 2;
  myvar = foo(3);
  return myvar;
}
