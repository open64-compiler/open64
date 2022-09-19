/*
   Copyright (C) 2019-2022 Xcalibyte (Shenzhen) Limited.

   Licensed under the Apache License, Version 2.0 (the "License");
   you may not use this file except in compliance with the License.
   You may obtain a copy of the License at

     http://www.apache.org/licenses/LICENSE-2.0

   Unless required by applicable law or agreed to in writing, software
   distributed under the License is distributed on an "AS IS" BASIS,
   WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
   See the License for the specific language governing permissions and
   limitations under the License.
 */

//==============================================================================
//
// unit test for printf format string parser
//
// give a printf format string and parameters, call the parser implemented in mastiff
// and printf(), compare their length to make sure the parser works
//
// $ g++ -O0 -g opt_vsa_printf_unit.cxx
//
//==============================================================================
#include <stdio.h>
#include <string.h>
#include <ctype.h>
#include <stdlib.h>
#include <stddef.h>
#include <stdarg.h>

// implementation for types and macros used in Vsa_parse_printf_format
typedef int INT;
typedef struct
{
  virtual int get_arg_length(int arg, int& short_cnt, int& long_cnt) const = 0;
} FORMAT_PARSER_CONTEXT;

template<typename T>
struct type_traits : public FORMAT_PARSER_CONTEXT
{
  int get_arg_length(int arg, int& short_cnt, int& long_cnt) const {
    return 0;
  }
};

template<>
struct type_traits<unsigned char> : public FORMAT_PARSER_CONTEXT
{
  int get_arg_length(int arg, int& short_cnt, int& long_cnt) const {
    short_cnt = 2;
    long_cnt = 0;
    return 1;
  }
};

template<>
struct type_traits<unsigned short> : public FORMAT_PARSER_CONTEXT
{
  int get_arg_length(int arg, int& short_cnt, int& long_cnt) const {
    short_cnt = 1;
    long_cnt = 0;
    return 1;
  }
};

static void
Json_copy(char* dst, const char* src, int len)
{
  memcpy(dst, src, len);
}

#define FALSE 0
#define Is_True(x, y)  do { if(!(x)) { printf("LINE %d ", __LINE__); printf y; } } while (0)
#define Is_Target_32bit() (sizeof(void*) == 4)
#define Is_Target_64bit() (sizeof(void*) == 8)

#define INT_PARM_TYPE        int
#define INT_PARM_INITIALIZER (-1)
#define INTP_PARM_TYPE       int *
#define INTP_PARM_INITIALIZER NULL
#define FLT_PARM_TYPE        double
#define FLT_PARM_INITIALIZER (0.0)
#define STR_PARM_TYPE        const char *
#define STR_PARM_INITIALIZER NULL
#define STR_LEN_TYPE         int

#define INT_PARM_CMP(arg, cmp, val) \
  (arg cmp val)

#define INT_PARM_SET(arg, val) \
  (arg = val)

#define INT_PARM_ADD(ctx, a, b) \
  (a + b)

#define PARSER_ARGS  , ...

#define PARSER_INIT(ctx, fmt) \
  va_list ap; va_start(ap, fmt)

#define PARSER_FINI(ctx, arg, len) \
  va_end(ap)

#define PARSER_FOR_SCANF(ctx) \
  (0)

#define PARSER_GET_ARG(ctx, arg, type) \
  va_arg(ap, type)

#define PARSER_CHECK_TYPE(ctx, arg, s_cnt, l_cnt, fmt)

#define PARSER_CHECK_NPD(ctx, arg)

#define PARSER_CHECK_UAF(ctx, arg)

#define PARSER_CHECK_OBJ_SIZE(ctx, arg, sz)

#define PARSER_CHECK_UNLIMIT_STRING(ctx, n_arg, arg)

#define PARSER_STR_LENGTH(ctx, obj) \
  strlen(obj)

#define PARSER_RETURN(ctx, arg, len) \
  ((arg << 16) | len)

#define PARSER_GET_ARG_LENGTH(ctx, arg, short_cnt, long_cnt, fmt, sharp, length) \
  ctx->get_arg_length(arg, short_cnt, long_cnt)

#define PARSER_REPORT_ERROR(ctx, msg1, msg2) \
  printf("Error: %s, %s\n", msg1, msg2)

// include the parser implementation for unit test
#include "opt_vsa_printf.h"

// define values used in unit test
#define U8_MAX     0xff
#define U16_MAX    0xffff
#define U32_MAX    0xffffffff
#define U64_MAX    0xffffffffffffffffL
#define UL64_MAX   0xffffffffffffffffLL
#define S8_MAX     0x80
#define S16_MAX    0x8000
#define S32_MAX    0x80000000
#define S64_MAX    0x8000000000000000L
#define SL64_MAX   0x8000000000000000LL
#define FLOAT_MAX  0xff7fffffL
#define DOUBLE_MAX 0xffefffffffffffffLL

// TEST0, call the parser only with the format string, no extra va_args
#define TEST0(fmt) do { \
    int lhs, tmp; \
    ++ test_count; \
    lhs = Vsa_parse_printf_format(NULL, fmt); \
    tmp = printf(fmt); \
    printf(": %d\n", tmp); \
    if (lhs != tmp) { \
      printf("\033[31m\033[1m>>> FAIL LINE %d %s\033[0m: %d != %d\n", __LINE__, fmt, lhs, tmp); \
      ++ err_count; \
    } \
  } while (0)

// TEST, call the parser with printf format string and extra va_args
#define TEST(fmt, arg, ...) do { \
    int lhs, tmp; \
    ++ test_count; \
    lhs = Vsa_parse_printf_format(NULL, fmt, __VA_ARGS__); \
    tmp = printf(fmt, __VA_ARGS__); \
    printf(": %d\n", tmp); \
    if (lhs != PARSER_RETURN(NULL, arg, tmp)) { \
      printf("\033[31m\033[1m>>> FAIL LINE %d %s\033[0m: (%d,%d) != (%d,%d)\n", \
             __LINE__, fmt, lhs >> 16, lhs & 0xffff, arg, tmp); \
      ++ err_count; \
    } \
  } while (0)

// TEST_1, call the parser with printf format string and extra va_args
#define TEST_TYPE_1(fmt, arg, type, value) do { \
    int lhs, tmp; \
    ++ test_count; \
    type_traits<type> tt; \
    lhs = Vsa_parse_printf_format(&tt, fmt, value); \
    tmp = printf(fmt, value); \
    printf(": %d\n", tmp); \
    if (lhs != PARSER_RETURN(NULL, arg, tmp)) { \
      printf("\033[31m\033[1m>>> FAIL LINE %d %s\033[0m: (%d,%d) != (%d,%d)\n", \
             __LINE__, fmt, lhs >> 16, lhs & 0xffff, arg, tmp); \
      ++ err_count; \
    } \
  } while (0)

// number of total tests and errors
int test_count = 0;
int err_count = 0;

// test_0, test the parser without extra va_args
static void
test_0()
{
  TEST0("a");
  TEST0("%%");
  TEST0("a%%a");
}

// test_1_int, test the parser for integer format %d, %u, %o, %x, hh/h/l/ll, etc
static void
test_1_int()
{
  TEST("a%hhda", 1, S8_MAX);
  TEST("a%hda", 1,  S16_MAX);
  TEST("a%da", 1,   S32_MAX);
  TEST("a%lda", 1,  S64_MAX);
  TEST("a%llda", 1, SL64_MAX);
  TEST("a%Lda", 1,  SL64_MAX);
  TEST("a%hhua", 1, U8_MAX);
  TEST("a%hua", 1,  U16_MAX);
  TEST("a%ua", 1,   U32_MAX);
  TEST("a%lua", 1,  U64_MAX);
  TEST("a%llua", 1, UL64_MAX);
  TEST("a%Lua", 1,  UL64_MAX);
  TEST("a%hhoa", 1, U8_MAX);
  TEST("a%hoa", 1,  U16_MAX);
  TEST("a%oa", 1,   U32_MAX);
  TEST("a%loa", 1,  U64_MAX);
  TEST("a%lloa", 1, UL64_MAX);
  TEST("a%Loa", 1,  UL64_MAX);
  TEST("a%hhxa", 1, U8_MAX);
  TEST("a%hxa", 1,  U16_MAX);
  TEST("a%xa", 1,   U32_MAX);
  TEST("a%lxa", 1,  U64_MAX);
  TEST("a%llxa", 1, UL64_MAX);
  TEST("a%Lxa", 1,  UL64_MAX);
  // width
  TEST("a%2hhda", 1, S8_MAX);
  TEST("a%12hda", 1, S16_MAX);
  TEST("a%22da", 1,  S32_MAX);
  TEST("a%32lda", 1, S64_MAX);
  TEST("a%42llda", 1,SL64_MAX);
  TEST("a%52Lda", 1, SL64_MAX);
  TEST("a%2hhua", 1, U8_MAX);
  TEST("a%12hua", 1, U16_MAX);
  TEST("a%22ua", 1,  U32_MAX);
  TEST("a%22lxa", 1, U64_MAX);
  TEST("a%22Loa", 1, UL64_MAX);
  TEST("a%22Lxa", 1, UL64_MAX);
  // star
  TEST("a%*hhda", 2, 2, S8_MAX);
  TEST("a%*hda", 2, 12, S16_MAX);
  TEST("a%*da", 2,  22, S32_MAX);
  TEST("a%*lda", 2, 32, S64_MAX);
  TEST("a%*llda", 2,42, SL64_MAX);
  TEST("a%*Lda", 2, 52, SL64_MAX);
  TEST("a%*hhua", 2, 2, U8_MAX);
  TEST("a%*hua", 2, 12, U16_MAX);
  TEST("a%*ua", 2,  22, U32_MAX);
  TEST("a%*lxa", 2, 22, U64_MAX);
  TEST("a%*Loa", 2, 22, UL64_MAX);
  TEST("a%*Lxa", 2, 22, UL64_MAX);
  // sharp
  TEST("a%#oa\n", 1, S32_MAX);
  TEST("a%#0oa\n", 1, S32_MAX);
  TEST("a%#1oa\n", 1, S32_MAX);
  TEST("a%#10oa\n", 1, S32_MAX);
  TEST("a%#40oa\n", 1, S32_MAX);
  TEST("a%#*oa\n", 2, 0, S32_MAX);
  TEST("a%#*oa\n", 2, 1, S32_MAX);
  TEST("a%#*oa\n", 2, 10, S32_MAX);
  TEST("a%#*oa\n", 2, 40, S32_MAX);
  TEST("a%#xa\n", 1, S32_MAX);
  TEST("a%#0xa\n", 1, S32_MAX);
  TEST("a%#1xa\n", 1, S32_MAX);
  TEST("a%#10xa\n", 1, S32_MAX);
  TEST("a%#40xa\n", 1, S32_MAX);
  TEST("a%#*xa\n", 2, 0, S32_MAX);
  TEST("a%#*xa\n", 2, 1, S32_MAX);
  TEST("a%#*xa\n", 2, 10, S32_MAX);
  TEST("a%#*xa\n", 2, 40, S32_MAX);
}

// test_1_int
void test_1_int_type_func(unsigned char a, unsigned short b, unsigned long c)
{
  TEST_TYPE_1("a%da\n", 1, unsigned char, a);
  TEST_TYPE_1("a%da\n", 1, unsigned short, b);
  a = (c >> 16) & 0xff;
  TEST_TYPE_1("a%da\n", 1, unsigned char, a);
  b = (c >> 16) & 0xffff;
  TEST_TYPE_1("a%da\n", 1, unsigned short, b);
}

void test_1_int_type()
{
  TEST_TYPE_1("a%da\n", 1, unsigned char, U8_MAX);
  TEST_TYPE_1("a%da\n", 1, unsigned short, U16_MAX);
  test_1_int_type_func(U8_MAX, U16_MAX, U32_MAX);
}

// test_1_float, test the parser for float format %a, %e, %f, %g, l/L etc
static void
test_1_float()
{
  union {
    int ival;
    float fval;
  } u1;
  u1.ival = FLOAT_MAX;
  union {
    long long lval;
    double dval;
    long double ldval;
  } u2;
  u2.lval = DOUBLE_MAX;
  long double ldval = (long double)u2.dval;
  TEST("a%fa", 1,         u1.fval);
  TEST("a%lfa", 1,        u2.dval);
  TEST("a%Lfa", 1,        ldval);
  TEST("a%2.0Fa", 1,      u1.fval);
  TEST("a%20.10lFa", 1,   u2.dval);
  TEST("a%40.20LFa", 1,   ldval);
  TEST("a%ga", 1,         u1.fval);
  TEST("a%lga", 1,        u2.dval);
  TEST("a%Lga", 1,        ldval);
  TEST("a%2.0ga", 1,      u1.fval);
  TEST("a%10.10lga", 1,   u2.dval);
  TEST("a%80.20Lga", 1,   ldval);
  TEST("a%ea", 1,         u1.fval);
  TEST("a%lea", 1,        u2.dval);
  TEST("a%Lea", 1,        ldval);
  TEST("a%2.0Ea", 1,      u1.fval);
  TEST("a%30.10lEa", 1,   u2.dval);
  TEST("a%100.20LEa", 1,  ldval);
  TEST("a%aa", 1,         u1.fval);
  TEST("a%laa", 1,        u2.dval);
  TEST("a%Laa", 1,        ldval);
  TEST("a%2.0aa", 1,      u1.fval);
  TEST("a%30.10lAa", 1,   u2.dval);
  TEST("a%100.20LAa", 1,  ldval);
  // star
  TEST("a%*fa", 2, 0,          u1.fval);
  TEST("a%*fa", 2, 1,          u1.fval);
  TEST("a%*fa", 2, 22,         u1.fval);
  TEST("a%*lfa", 2, 22,        u2.dval);
  TEST("a%*.*Fa", 3, 0, 0,     u1.fval);
  TEST("a%*.*Fa", 3, 0, 1,     u1.fval);
  TEST("a%*.*Fa", 3, 2, 0,     u1.fval);
  TEST("a%*.*Fa", 3, 2, 1,     u1.fval);
  TEST("a%*.*lFa", 3, 20, 10,  u2.dval);
  TEST("a%*.*LFa", 3, 40, 20,  ldval);
  TEST("a%*.*ga", 3, 0, 0,     u1.fval);
  TEST("a%*.*ga", 3, 0, 1,     u1.fval);
  TEST("a%*.*ga", 3, 2, 0,     u1.fval);
  TEST("a%*.*ga", 3, 2, 1,     u1.fval);
  TEST("a%*.*lga", 3, 10, 10,  u2.dval);
  TEST("a%*.*Lga", 3, 80, 20,  ldval);
  TEST("a%*.*Ea", 3, 0, 0,     u1.fval);
  TEST("a%*.*Ea", 3, 0, 1,     u1.fval);
  TEST("a%*.*Ea", 3, 2, 0,     u1.fval);
  TEST("a%*.*Ea", 3, 2, 1,     u1.fval);
  TEST("a%*.*lEa", 3, 30, 10,  u2.dval);
  TEST("a%*.*LEa", 3, 100, 20, ldval);
  TEST("a%*.*aa", 3, 0, 0,     u1.fval);
  TEST("a%*.*aa", 3, 0, 1,     u1.fval);
  TEST("a%*.*aa", 3, 2, 0,     u1.fval);
  TEST("a%*.*aa", 3, 2, 1,     u1.fval);
  TEST("a%*.*laa", 3, 30, 10,  u2.dval);
  TEST("a%*.*Laa", 3, 100, 20, ldval);
  // sharp
  TEST("a%#aa", 1,         u1.fval);
  TEST("a%#laa", 1,        u2.dval);
  TEST("a%#Laa", 1,        ldval);
  TEST("a%#2.0aa", 1,      u1.fval);
  TEST("a%#30.10lAa", 1,   u2.dval);
  TEST("a%#100.20LAa", 1,  ldval);
  TEST("a%#*.*aa",3, 0, 0, u1.fval);
  TEST("a%#*.*aa",3, 0, 1, u1.fval);
  TEST("a%#*.*aa",3, 2, 0, u1.fval);
  TEST("a%#*.*aa",3, 2, 1, u1.fval);
  TEST("a%#*.*aa",3,50,10, u1.fval);
  TEST("a%#*.*laa",3, 0, 0, u2.dval);
  TEST("a%#*.*Laa",3, 0, 1, ldval);
  TEST("a%#*.*laa",3, 2, 0, u2.dval);
  TEST("a%#*.*Laa",3, 2, 1, ldval);
  TEST("a%#*.*laa",3,50,10, u2.dval);
  TEST("a%#*.*Laa",3,50,10, ldval);
  TEST("a%#ea", 1,         u1.fval);
  TEST("a%#lea", 1,        u2.dval);
  TEST("a%#Lea", 1,        ldval);
  TEST("a%#2.0ea", 1,      u1.fval);
  TEST("a%#30.10lEa", 1,   u2.dval);
  TEST("a%#100.20LEa", 1,  ldval);
  TEST("a%#*.*ea",3, 0, 0, u1.fval);
  TEST("a%#*.*ea",3, 0, 1, u1.fval);
  TEST("a%#*.*ea",3, 2, 0, u1.fval);
  TEST("a%#*.*ea",3, 2, 1, u1.fval);
  TEST("a%#*.*ea",3,50,10, u1.fval);
  TEST("a%#*.*lea",3, 0, 0, u2.dval);
  TEST("a%#*.*Lea",3, 0, 1, ldval);
  TEST("a%#*.*lea",3, 2, 0, u2.dval);
  TEST("a%#*.*Lea",3, 2, 1, ldval);
  TEST("a%#*.*lea",3,50,10, u2.dval);
  TEST("a%#*.*Lea",3,50,10, ldval);
  TEST("a%#fa", 1,         u1.fval);
  TEST("a%#lfa", 1,        u2.dval);
  TEST("a%#Lfa", 1,        ldval);
  TEST("a%#2.0fa", 1,      u1.fval);
  TEST("a%#30.10lFa", 1,   u2.dval);
  TEST("a%#100.20LFa", 1,  ldval);
  TEST("a%#*.*fa",3, 0, 0, u1.fval);
  TEST("a%#*.*fa",3, 0, 1, u1.fval);
  TEST("a%#*.*fa",3, 2, 0, u1.fval);
  TEST("a%#*.*fa",3, 2, 1, u1.fval);
  TEST("a%#*.*fa",3,50,10, u1.fval);
  TEST("a%#*.*lfa",3, 0, 0, u2.dval);
  TEST("a%#*.*Lfa",3, 0, 1, ldval);
  TEST("a%#*.*lfa",3, 2, 0, u2.dval);
  TEST("a%#*.*Lfa",3, 2, 1, ldval);
  TEST("a%#*.*lfa",3,50,10, u2.dval);
  TEST("a%#*.*Lfa",3,50,10, ldval);
  TEST("a%#ga", 1,         u1.fval);
  TEST("a%#lg", 1,        u2.dval);
  TEST("a%#Lga", 1,        ldval);
  TEST("a%#2.0ga", 1,      u1.fval);
  TEST("a%#30.10lGa", 1,   u2.dval);
  TEST("a%#100.20LGa", 1,  ldval);
  TEST("a%#*.*ga",3, 0, 0, u1.fval);
  TEST("a%#*.*ga",3, 0, 1, u1.fval);
  TEST("a%#*.*ga",3, 2, 0, u1.fval);
  TEST("a%#*.*Ga",3, 2, 1, u1.fval);
  TEST("a%#*.*Ga",3,50,10, u1.fval);
  TEST("a%#*.*lga",3, 0, 0, u2.dval);
  TEST("a%#*.*Lga",3, 0, 1, ldval);
  TEST("a%#*.*lga",3, 2, 0, u2.dval);
  TEST("a%#*.*Lga",3, 2, 1, ldval);
  TEST("a%#*.*lga",3,50,10, u2.dval);
  TEST("a%#*.*Lga",3,50,10, ldval);
}

// test_1_char, test the parser for char format %c
static void
test_1_char()
{
  TEST("a%ca", 1, 'a');
  TEST("a%lca", 1, 'a');
  TEST("a%1lca", 1, 'a');
  TEST("a%2ca", 1, 'a');
  TEST("a%5lca", 1, 'a');
  // star
  TEST("a%*ca", 2, 0, 'a');
  TEST("a%*lca", 2, 0, 'a');
  TEST("a%*lca", 2, 1, 'a');
  TEST("a%*ca", 2, 2, 'a');
  TEST("a%*lca", 2, 5, 'a');
}

// test_1_ptr, test the parser for pointer format %p
static void
test_1_ptr()
{
  void *v = (void*)(unsigned long long)(UL64_MAX);
  TEST("a%pa", 1, v);
  TEST("a%15pa", 1, v);
  TEST("a%30pa", 1, v);
  // star
  TEST("a%*pa", 2, 0, v);
  TEST("a%*pa", 2, 1, v);
  TEST("a%*pa", 2, 15, v);
  TEST("a%*pa", 2, 30, v);
} 

// test_1_str, test the parser for string format %s
static void
test_1_str()
{
  const char* str1 = "abcd";
  const char* str2 = "abcdedfhijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ";
  TEST("x%sx\n", 1, str1);
  TEST("x%2sx\n", 1, str1);
  TEST("x%20sx\n", 1, str1);
  TEST("x%-2sx\n", 1, str1);
  TEST("x%-20sx\n", 1, str1);
  // star
  TEST("x%*sx\n", 2, 0, str1);
  TEST("x%*sx\n", 2, 2, str1);
  TEST("x%*sx\n", 2, 20, str1);
  TEST("x%-*sx\n", 2, 2, str1);
  TEST("x%-*sx\n", 2, 20, str1);
}

// test_2, test the parser with 2 format argements
static void
test_2()
{
  const char* str1 = "abcd";
  const char* str2 = "abcdedfhijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ";
  union {
    int ival;
    float fval;
  } u1;
  u1.ival = FLOAT_MAX;
  union {
    long long lval;
    double dval;
    long double ldval;
  } u2;
  u2.lval = DOUBLE_MAX;
  long double ldval = (long double)u2.dval;
  // string - integer
  TEST("x%sx y%dy\n", 2, str1, S32_MAX);
  TEST("x%2sx y%20uy\n", 2, str1, U32_MAX);
  TEST("x%20sx y%20luy\n", 2, str1, U64_MAX);
  TEST("x%-2sx y%-30Loy\n", 2, str1, UL64_MAX);
  TEST("x%-20sx y%-Lxy\n", 2, str1, UL64_MAX);
  // star
  TEST("x%*sx y%*dy\n", 4, 0, str1, 0, S32_MAX);
  TEST("x%*sx y%*luy\n", 4, 2, str1, 2, U64_MAX);
  TEST("x%*sx y%*Lxy\n", 4, 20, str1, 20, UL64_MAX);
  TEST("x%-*sx y%-*oy\n", 4, 2, str1, 2, U32_MAX);
  TEST("x%-*sx y%-*hd\n", 4, 20, str1, 20, S16_MAX);
  // string - floating point
  TEST("x%sx y%fy\n", 2, str1, u1.fval);
  TEST("x%2sx y%20lfy\n", 2, str1, u2.dval);
  TEST("x%20sx y%20ay\n", 2, str1, u1.fval);
  TEST("x%-2sx y%-30Lay\n", 2, str1, ldval);
  TEST("x%-20sx y%-Ley\n", 2, str1, ldval);
  TEST("x%-20sx y%ey\n", 2, str1, u1.fval);
  TEST("x%20sx y%20gy\n", 2, str1, u1.fval);
  TEST("x%20sx y%-20Lgy\n", 2, str1, ldval);
  // star
  TEST("x%*sx y%*fy\n", 4, 0, str1, 0, u1.fval);
  TEST("x%*sx y%*lfy\n", 4, 2, str1, 2, u2.dval);
  TEST("x%*sx y%*Lay\n", 4, 20, str1, 20, ldval);
  TEST("x%-*sx y%-*a\n", 4, 20, str1, 20, u1.fval);
  TEST("x%-*sx y%-*ey\n", 4, 2, str1, 2, u1.fval);
  TEST("x%-*sx y%-*le\n", 4, 20, str1, 20, u2.dval);
  TEST("x%-*sx y%*gy\n", 4, 2, str1, 2, u1.fval);
  TEST("x%-*sx y%-*Lg\n", 4, 20, str1, 20, ldval);
  
  // string - string
  TEST("x%sx y%sy\n", 2, str1, str2);
  TEST("x%20sx y%20sy\n", 2, str1, str2);
  TEST("x%*sx y%*sy\n", 4, 0, str1, 0, str2);
  TEST("x%*sx y%*sy\n", 4, 1, str1, 1, str2);
  TEST("x%*sx y%*sy\n", 4, 30, str1, 30, str2);
  TEST("x%*sx y%*sy\n", 4, 60, str1, 60, str2);
}

// main
int main()
{
  test_0();
  test_1_int();
  test_1_int_type();
  test_1_float();
  test_1_char();
  test_1_ptr();
  test_1_str();
  test_2();
  printf("====================================\n");
  printf("Total %d tests, %d failed\n", test_count, err_count);
}

