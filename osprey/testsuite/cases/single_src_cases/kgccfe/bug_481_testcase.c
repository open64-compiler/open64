//CFLAGS: -gnu3
//Open64 4.2 (32 bit build, 64 bit target) incorrectly defines the type long to be 4 bytes 
//rather than 8 when compiling -gnu3
int main() {
      printf("sizeof int=%d\n",sizeof(int));
      printf("sizeof long int=%d\n",sizeof(long int));
      printf("sizeof ptr=%d\n",sizeof(int*));
      return 0;
}
