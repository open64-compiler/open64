//CFLAGS: ALL
//This is the testcase of bug #491
//This testcase is extracted from firefox
#include <stdarg.h>

struct ttype
{
      int a;
        va_list v;
};

void foo1 ( int amount, ...)
{
        struct ttype tmp;
            int i;

                va_start(tmp.v,amount);
                    i=foo2(&tmp,amount);
                        va_end(tmp.v);
}

int foo2(struct ttype *tmp, int amount)
{
        return va_arg(tmp->v,int);
}

int main ()
{
      foo1(7,702);
}
