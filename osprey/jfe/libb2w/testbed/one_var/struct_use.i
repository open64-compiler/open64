# 1 "struct_use.c"
# 1 "<built-in>"
# 1 "<command-line>"
# 1 "struct_use.c"
struct colson {
  int a;
  int b;
  int c;
};

int main(){
  struct colson one_struct;
  one_struct.a = 0;
  return one_struct.a;
}
