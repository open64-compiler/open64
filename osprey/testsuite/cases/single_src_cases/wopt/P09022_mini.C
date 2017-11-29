//This is the testcase about a bug of DSE, which is fixed in Rev 1682
//This testcase is extracted from P09022@CCVS

extern "C" int printf(const char*, ...);

struct bits {
  unsigned a : 1;
  unsigned b : 2;
  void set(int va, int vb) {
    a = va;
    b = vb;
  }
};

bits abits;

int main() {
  abits.set(1, 2);

  printf("%x %x\n", abits.a ,abits.b);
  return 0;      
}
