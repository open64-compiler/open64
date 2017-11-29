//CFLAGS:-O2 -IPA
//This is the testcase of bug #455
//This testcase is extracted from 445.gobmk@spec2006
#include<stdio.h>
unsigned char board[421];

int
is_self_atari(int pos)
{

  
  if ((((unsigned) (pos) < 421 ) && board[pos] != 3)) 
	 printf("ok\n");
  return 1;
}

static int deltatt[8]  = { 20, -1, -20, 1, 19, -21, -19, 21};

main()
{
  int i;
  int pos=4;
  int type=1;

  board[4] = 1;

  int apos = pos +  deltatt[type];
  if (board[pos] == 3)
      return 0;
  is_self_atari(apos);
}
