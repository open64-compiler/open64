#include <stdio.h>

#ifndef ALIGN
#error Missing ALIGN
#endif

#ifndef TYPE
#error Missing TYPE
#endif

#define	SZ	sizeof(TYPE)/ALIGN

#ifdef __hpux
typedef __float80 longdouble;
#else
typedef long double longdouble;
#endif

#pragma pack 1
struct foo {
	TYPE f;
	char x[ALIGN];
} __attribute__((packed));

const TYPE val = 1.5;

static void
fail(const char *op, int t, void *p)
{
	fprintf(stderr, "FAIL: %s: %d, %p\n", op, t, p);
	fflush(stderr);
}

static void
swap(char *p)
{
	/*
	 * Perform byte swapping in order to test sequences for non-native
	 * byte ordering.  This is only applicable when the compiler is
	 * rigged to emit those foreign sequences.  Hence, this function
	 * is empty because rigging the compiler is not viable for normal
	 * regression testing.
	 */
}

static TYPE
collect(const char *src)
{
	TYPE f;

	memcpy(&f, src, sizeof(TYPE));
	return (f);
}

static void
supply(char *src)
{
	memcpy(src, &val, sizeof(TYPE));
}

static TYPE
load(struct foo *f)
{
	swap(&f->f);
	return f->f;
}

static void
store(struct foo *f, TYPE v)
{
	f->f = v;
	swap(&f->f);
}

int
main()
{
	static struct foo src[SZ];
	TYPE f;
	int i;

	for (i = 0; i < SZ; i++) {
		/* Test unaligned store */
		store(src + i, val);
		f = collect((const char *)&src[i].f);
		if (f != val) {
			fail("store", i, src + i);
			supply((char *)&src[i].f);
		}
		/* Test unaligned load */
		f = load(src + i);
		if (f != val)
			fail("load", i, src + i);
	}
	return (0);
}
