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

#ifndef OPT_VSA_PRINTF_INCLUDED
#define OPT_VSA_PRINTF_INCLUDED

//=============================================================================
// implementation of printf format string parser.
//
// this implementation is in a header file so that it can be
// included in both mastiff and unit test.
//
// when this file is modified, please compile the opt_vsa_printf_test.cxx
// and make sure all tests pass.
//=============================================================================

static INT
Vsa_parse_printf_format(FORMAT_PARSER_CONTEXT* ctx, const char* fmt_str PARSER_ARGS)
{
  enum {
    fmt_sdec = 0, /* %d for integer */
    fmt_udec,     /* %u for integer */
    fmt_oct,      /* %o for integer */
    fmt_hex,      /* %x for integer */
    fmt_ff,       /* %f for float */
    fmt_feg,      /* %e/g for float */
    fmt_fa,       /* %a for float */
    fmt_max
  };

  static const INT digit_max_len[fmt_max][4] = {
    /* sdec '%d': %hd, %hhd, %d, %ld/%lld */
    { sizeof("-32768") - 1, sizeof("-128") - 1,
      sizeof("-2147483648") - 1, sizeof("-9223372036854775808") - 1 },
    /* udec '%u': %hu, %hhu, %u, %lu/%llu */
    { sizeof("65535") - 1, sizeof("255") - 1,
      sizeof("4294967295") - 1, sizeof("18446744073709551615") - 1 },
    /* oct  '%o': %ho, %hhu, %o, %lo/%llo */
    { sizeof("177777") - 1, sizeof("377") - 1,
      sizeof("37777777777") - 1, sizeof("1777777777777777777777") - 1 },
    /* hex  '%x': %hx, %hhx, %x, %lx/%llx */
    { sizeof("ffff") - 1, sizeof("ff") - 1,
      sizeof("ffffffff") - 1, sizeof("ffffffffffffffff") - 1 },
    /* float '%f': %f, %lf/%Lf, before the decimal point */
    { sizeof("-340282346638528859811704183484516925440") - 1,
      sizeof("-179769313486231570814527423731704356798070567525844996598917476803157260780028538760589558632766878171540458953514382464234321326889464182768467546703537516986049910576551282076245490090389328944075868508455133942304583236903222948165808559332123348274797826204144723168738177180919299881250404026184124858368") - 1,
      -1, -1 },
    /* float '%e(%g)': %e(%g), %le/%Le(%lg/%Lg), before the decimal point */
    { sizeof("-1e+38") - 1,
      sizeof("-1e+308") - 1,
      -1, -1 },
    /* float '%a': %a, %la/%La, before the decimal point */
    { sizeof("-0x1p+127") - 1,
      sizeof("-0x1p+1023") - 1,
      -1, -1 }
  };

  INT n_arg = 0;
  INT n_len = 0;
  char sspec_1[64];
  char sspec_2[64];  // %m.nf fp only
  char wrong_fmt[32];

  // initialize parser
  PARSER_INIT(ctx, fmt_str);

  const char* ptr = fmt_str;
  const char* last_percent = NULL;
  while (*ptr != '\0') {
    if (*ptr != '%') {
      ++ ptr;
      ++ n_len;
      continue;
    }
    INT_PARM_TYPE fw_1 = INT_PARM_INITIALIZER; // field width %*d
    INT_PARM_TYPE fw_2 = INT_PARM_INITIALIZER; // field width %*.*f fp only
    INT_PARM_TYPE int_val = INT_PARM_INITIALIZER;  // %d
    INTP_PARM_TYPE intp_val = INTP_PARM_INITIALIZER;  // %n
    FLT_PARM_TYPE flt_val = FLT_PARM_INITIALIZER;  // %f
    STR_PARM_TYPE str_val = STR_PARM_INITIALIZER;  // %s
    char* sptr_1 = sspec_1;
    char* sptr_2 = sspec_2;
    int flag_sharp = 0;
    last_percent = ptr ++;  // skip '%'

    // flag
    while (strchr("-0 +\'I#", *ptr)) {
      switch (*ptr) {
      case '-':  // left aligned
      case '0':  // padding with 0
      case ' ':  // space before positive number
      case '+':  // '+' for positive number
        // doesn't impact the width
        break;
      case '\'': // TODO: no idea, assume no impact on width
      case 'I':  // TODO: no idea, assume no impact on width
        break;
      case '#':
        flag_sharp = 1; // with '#' for alternate form:
                        // o: +1 for '0'
                        // x: +2 for '0x'
                        // a/e/f: +1 for '.' when nspec_2/fw_2 is 0
                        // g: doesn't impact width
        break;
      }
      ++ ptr;  // skip "-+#0"
    }

    // width
    if (*ptr == '*') {
      // fw_1 = ctx->callsite->Get_arg(n_arg);
      // fw_1 = va_arg(ap, int);
      fw_1 = PARSER_GET_ARG(ctx, n_arg, int);
      // printf("arg %d: * %d\n", n_arg, fw_1);
      ++ n_arg;
      ++ ptr;  // skip '*'
    }
    else {
      while (isdigit(*ptr))
        *sptr_1++ = *ptr++;
    }

    // precision
    if (*ptr == '.') {
      ++ ptr; // skip "."
      if (*ptr == '*') {
        // fw_2 = ctx->callsite->Get_arg(n_arg);
        // fw_2 = va_arg(ap, int);
        fw_2 = PARSER_GET_ARG(ctx, n_arg, int);
        // printf("arg %d: * %d\n", n_arg, fw_2);
        ++ n_arg;
        ++ ptr;  // skip '*'
      }
      else {
        while (isdigit(*ptr))
          *sptr_2++ = *ptr++;
      }
    }
    *sptr_1 = '\0';
    *sptr_2 = '\0';
    INT nspec_1 = sspec_1[0] == '\0' ? -1 : atoi(sspec_1);
    INT nspec_2 = sspec_2[0] == '\0' ? -1 : atoi(sspec_2);

    // length
    INT short_cnt = 0;
    INT long_cnt = 0;
    while (strchr("hlLjzt", *ptr)) {
      switch (*ptr) {
      case 'h':  // h: short INT, hh: char
        ++ short_cnt;
        break;
      case 'l':  // l: long, ll: long long
        ++ long_cnt;
        break;
      case 'L':  // long double
        long_cnt = 2;
      case 'j':  // intmax_t: long int
      case 'z':  // size_t: unsigned long int
      case 't':  // ptrdiff_t: long int
        long_cnt = 1;
        break;
      default:
        Is_True(FALSE, ("TODO: handle %c sspec_1\n", *ptr));
      }
      ++ ptr;  // skip "hlL"
    }

    INT fmt = -1;
    INT arg_len = 0;
    switch (*ptr) {
    case 'd':
    case 'i':
      if (fmt == -1)
        fmt = fmt_sdec;
      // fall through
    case 'o':
      if (fmt == -1)
        fmt = fmt_oct;
      // fall through
    case 'u':
      if (fmt == -1)
        fmt = fmt_udec;
      // fall through
    case 'x':
    case 'X':
      if (fmt == -1)
        fmt = fmt_hex;
      // fall through
      // printf("arg %d: %c\n", n_arg, *ptr);
      arg_len = 0;
      if (ctx &&
          PARSER_GET_ARG_LENGTH(ctx, n_arg, short_cnt, long_cnt, *ptr, flag_sharp, arg_len)) {
        // unsigned value type and short_cnt/long cnt is override
        if (fmt == fmt_sdec)
          fmt = fmt_udec;
      }
      if (arg_len == 0) {
        if (short_cnt == 1 || short_cnt == 2)
          arg_len = digit_max_len[fmt][short_cnt - 1];
        else if (long_cnt == 1 && Is_Target_32bit())
          arg_len = digit_max_len[fmt][2];
        else if (long_cnt == 1 || long_cnt == 2)
          arg_len = digit_max_len[fmt][3];
        else
          arg_len = digit_max_len[fmt][2];

        if (flag_sharp) {
          if (*ptr == 'o')
            arg_len += 1;   // '0'
          else if (*ptr == 'x' || *ptr == 'X')
            arg_len += 2;   // '0x'
        }
      }
      int_val = PARSER_GET_ARG(ctx, n_arg, int);
      PARSER_CHECK_TYPE(ctx, n_arg, int_val, short_cnt, long_cnt, 'd');

      ++ n_arg;
      break;

    case 'C':
      // synonym for 'lc'. TODO: issue a warning about `don't use'.
      long_cnt = 1;
      // fall through
    case 'c':
      ++ n_arg;
      if (nspec_1 > 0)
        arg_len = nspec_1 * sizeof(char);
      else
        arg_len = sizeof(char);
      break;

    case 'f':
    case 'F':
      if (fmt == -1)
        fmt = fmt_ff;
      // fall through
    case 'a':
    case 'A':
      if (fmt == -1)
        fmt = fmt_fa;
      // fall through
    case 'e':
    case 'E':
      if (fmt == -1)
        fmt = fmt_feg;
      // fall through
    case 'g':
    case 'G':
      if (fmt == -1) {
        fmt = fmt_feg;
        if (nspec_2 == 1)
          nspec_2 = 0;
        if (fw_2 != INT_PARM_INITIALIZER &&
            INT_PARM_CMP(fw_2, ==, 1))  // fw_2 == 1
          INT_PARM_SET(fw_2, 0);      // fw_2 = 0
      }
      flt_val = PARSER_GET_ARG(ctx, n_arg, double);
      PARSER_CHECK_TYPE(ctx, n_arg, flt_val, short_cnt, long_cnt, 'f');

      // process f/a/e/g/F/A/E/G
      ++ n_arg;
      if (long_cnt > 0)
        arg_len = digit_max_len[fmt][1];
      else
        arg_len = digit_max_len[fmt][0];

      if (nspec_2 != -1) {
        if (*ptr == 'g' || *ptr == 'G' || nspec_2 == 0)
          arg_len += nspec_2;      // total nspec_2 digits
        else
          arg_len += nspec_2 + 1;  // digits + '.'
      }
      else if (fw_2 != INT_PARM_INITIALIZER) {
        if (*ptr == 'g' || *ptr == 'G' ||
            INT_PARM_CMP(fw_2, ==, 0))  // fw_2 == 0
          arg_len = INT_PARM_ADD(ctx, arg_len, fw_2);  // total fw_2 digits, arg_len + fw_2
        else
          arg_len = INT_PARM_ADD(ctx, arg_len + 1, fw_2); // digits + '.', arg_len + fw_2 + 1
      }
      else {
        if (*ptr == 'g' || *ptr == 'G')
          arg_len += 6;           // total 6 digits
        else if (*ptr == 'a' || *ptr == 'A')
          arg_len += (long_cnt > 0) ? 13 + 1 : 6 + 1;  // 1 for '.'
        else
          arg_len += 6 + 1;       // 6 digits + '.'
      }

      if (flag_sharp == 1 &&
          (nspec_2 == 0 ||
           (fw_2 != INT_PARM_INITIALIZER && INT_PARM_CMP(fw_2, ==, 0)))) {
        arg_len += 1;            // '.'
      }
      break;

    case 'S':
      // synonym for 'ls'. TODO: issue a warning about `don't use'.
      long_cnt = 1;
      // fall through
    case 's':
      // str_val = ctx->callsite->Get_arg(n_arg);
      // str_val = va_arg(ap, char *);
      str_val = PARSER_GET_ARG(ctx, n_arg, char *);
      // printf("arg %d: s %s\n", n_arg, str_val);
      // arg_len = ctx->caller->vsa->Get_object_length(str_val);
      // arg_len = strlen(str_val);
      PARSER_CHECK_TYPE(ctx, n_arg, str_val, 0, long_cnt, 's');
      PARSER_CHECK_NPD(ctx, str_val);
      PARSER_CHECK_UAF(ctx, str_val);
      arg_len = PARSER_STR_LENGTH(ctx, str_val);
      ++ n_arg;
      break;

    case 'p':
      intp_val = PARSER_GET_ARG(ctx, n_arg, int *);
      PARSER_CHECK_TYPE(ctx, n_arg, intp_val, 0, 0, 'p');
      ++ n_arg;
      long_cnt = Is_Target_64bit() ? 2 : 1;
      arg_len = digit_max_len[fmt_hex][long_cnt + 1] + 2;  // prefix '0x'
      break;

    case 'n':
      // NPD, length of the object >= sizeof(int)
      intp_val = PARSER_GET_ARG(ctx, n_arg, int *);
      PARSER_CHECK_TYPE(ctx, n_arg, intp_val, 0, 0, 'n');
      PARSER_CHECK_NPD(ctx, intp_val);
      PARSER_CHECK_UAF(ctx, intp_val);
      PARSER_CHECK_OBJ_SIZE(ctx, intp_val, sizeof(int));
      ++ n_arg;
      arg_len = 0;
      break;

    case 'm':
      // no argument for '%m'
      PARSER_CHECK_UNLIMIT_STRING(ctx, -1, INT_PARM_INITIALIZER);
      arg_len = 0;  // already checked above
      break;

    case '%':
      arg_len = 1;
      break;

    default:
      {
        ptrdiff_t len = ptr - last_percent + 1;
        if (len > sizeof(wrong_fmt))
          len = sizeof(wrong_fmt);
        Json_copy(wrong_fmt, last_percent, len);
        PARSER_REPORT_ERROR(ctx, wrong_fmt, "Unknown format character");
      }
      break;
    }
    if (nspec_1 != -1 && nspec_1 > arg_len)
      n_len += nspec_1;
    else if (fw_1 != INT_PARM_INITIALIZER && INT_PARM_CMP(fw_1, >, arg_len))
      n_len = INT_PARM_ADD(ctx, n_len, fw_1);
    else
      n_len += arg_len;
    ++ ptr;
  }
  // printf("Parse \"%s\", total arg: %d, length: %d\n", fmt_str, n_arg, n_len);
  // ctx->parser_fini();
  // va_end(ap);
  PARSER_FINI(ctx, n_arg, n_len);

  return PARSER_RETURN(ctx, n_arg, n_len);
}

#endif /* OPT_VSA_PRINTF_INCLUDED */

