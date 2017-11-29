//CFLAGS: -gnu3
//opencc -c -gnu3 test.c
//gfec: internal compiler error: Segmentation fault
unsigned int arr[3];
void foo(unsigned int *arr, unsigned int i, unsigned int j)
{
  printf("arr[index]=%u\n", arr[3-i-j]);
  return;
}
int main()
{
  foo(arr, 0, 1);
  foo(arr, 0, 2);
  return 0;
}

