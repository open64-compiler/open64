int foo(int random_val)
{
  int *p;
  for (int i=0; i<100; i++) {
    if (random_val != 0) {
      random_val++;
    }
  }
  return *p;
}
