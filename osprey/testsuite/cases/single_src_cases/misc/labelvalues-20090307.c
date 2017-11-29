extern long printf(const char *, ...);
int count = 0;
void inc_count() {
  count ++;
}

int f() {
  static void* sptr = &&a;
  static void* sptr_array[] = { &&a, &&b, &&c, &&d };
  static long  sptrdiff1 = &&a - &&b;
#ifdef __OPEN64__
  static long  sptrdiff2 = &&a - &&b + (&&c - &&d);
#endif
  static long  sptrdiff_array[] = { &&a - &&a, &&b - &&a, &&c - &&a, &&d - &&a };
  void* aptr = &&a;
  void* aptr_array[] = { &&a, &&b, &&c, &&d };
  long  aptrdiff1 = &&a - &&b;
#ifdef __OPEN64__
  long  aptrdiff2 = &&a - &&b + (&&c - &&d);
#endif
  long  aptrdiff_array[] = { &&a - &&a, &&b - &&a, &&c - &&a, &&d - &&a };

  int i;
  char label;

  if ( sptr == 0 || sptr != aptr)
    goto err;
  inc_count();

  for (i = 0; i<sizeof(sptr_array)/sizeof(void*); i++) {
    if ( sptr_array[i] == 0 || sptr_array[i] != aptr_array[i] )
      goto err;
  }
  inc_count();

  if ( sptrdiff1 == 0 || sptrdiff1 != aptrdiff1 )
      goto err;
  inc_count();

#ifdef __OPEN64__
  if ( sptrdiff2 == 0 || sptrdiff2 != aptrdiff2 )
      goto err;
  inc_count();
#endif

  if ( sptrdiff_array[0] != 0 || aptrdiff_array[0] != 0 )
    goto err;
  for (i=1; i<sizeof(sptrdiff_array)/sizeof(long); i++) {
    if ( sptrdiff_array[i] == 0 || sptrdiff_array[i] != aptrdiff_array[i] )
      goto err;
  }
  inc_count();

  i = 0;
  goto *sptr;

start:
  if ( label != 'a' + i )
    goto err;
  i ++;
  if ( i == sizeof(sptrdiff_array)/sizeof(long) ) {
    inc_count();
    goto end;
  }

  aptr = sptr + aptrdiff_array[i];
  goto *aptr; // goto a

a:
  label = 'a';
  goto start;
b:
  label = 'b';
  goto start;
c:
  label = 'c';
  goto start;
d:
  label = 'd';
  goto start;

end:
  printf("Tested cases: %d\n", count);
  printf("PASS\n");
  return 0;
err:
  printf("Tested cases: %d\n", count);
  printf("FAIL\n");
  return 1;
}

int main() {
  return f();
}

