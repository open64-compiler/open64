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

#ifndef OPT_VSA_SCANF_INCLUDED
#define OPT_VSA_SCANF_INCLUDED

//=============================================================================
// implementation of format string parser.
//
// this implementation is in a header file so that it can be
// included in both mastiff and unit test.
//
// when this file is modified, please compile the opt_vsa_scanf_unit.cxx
// and make sure all tests pass.
//=============================================================================

static INT
Vsa_parse_scanf_format(FORMAT_PARSER_CONTEXT* ctx, const char* fmt_str PARSER_ARGS)
{
  INT n_arg = 0;
  INT n_len = 0;
  char sspec_1[64];
  char wrong_fmt[32];

  // initialize parser
  PARSER_INIT(ctx, fmt_str);

  const char* ptr = fmt_str;
  const char* last_square = NULL;
  while (*ptr != '\0') {
    if (*ptr != '%') {
      ++ ptr;
      ++ n_len;
      continue;
    }

    char* sptr_1 = sspec_1;
    ++ ptr; // skip '%'

    BOOL discard_input = FALSE;  // scanf for %*
    BOOL malloc_buffer = FALSE;  // scanf for %m
    if (*ptr == '*') {
      discard_input = TRUE;
      ++ ptr;  // skip '*'
    }
    else if (*ptr == 'm') {
      malloc_buffer = TRUE;
      ++ ptr;
    }
    else {
      while (isdigit(*ptr))
        *sptr_1++ = *ptr++;
    }
    *sptr_1 = '\0';
    INT nspec_1 = sspec_1[0] == '\0' ? -1 : atoi(sspec_1);

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

    PTR_PARM_TYPE act_arg = PTR_PARM_INITIALIZER;
    INT fmt = -1;
    INT arg_len = 0;
    switch (*ptr) {
    case 'd':
    case 'i':
    case 'o':
    case 'u':
    case 'x':
    case 'X':
      if (discard_input)
        break;
      act_arg = PARSER_GET_ARG(ctx, n_arg, PTR_PARM_TYPE);
      PARSER_CHECK_NPD(ctx, act_arg);
      PARSER_CHECK_UAF(ctx, act_arg);
      if (short_cnt == 1)
        arg_len = sizeof(short);
      else if (short_cnt == 2)
        arg_len = sizeof(char);
      else if (long_cnt == 1 && Is_Target_32bit())
        arg_len = sizeof(int);
      else if (long_cnt == 1 || long_cnt == 2)
        arg_len = sizeof(long long);
      else
        arg_len = sizeof(int);
      PARSER_CHECK_OBJ_SIZE(ctx, act_arg, arg_len);
      ++ n_arg;
      break;

    case 'f':
    case 'a':
    case 'e':
    case 'g':
    case 'E':
      if (discard_input)
        break;
      act_arg = PARSER_GET_ARG(ctx, n_arg, PTR_PARM_TYPE);
      PARSER_CHECK_NPD(ctx, act_arg);
      PARSER_CHECK_UAF(ctx, act_arg);
      if (long_cnt > 0)
        arg_len = sizeof(double);
      else
        arg_len = sizeof(float);
      PARSER_CHECK_OBJ_SIZE(ctx, act_arg, arg_len);
      ++ n_arg;
      break;

    case '[':
      // search the closing ']'
      last_square = ptr ++;
      if (*ptr == '^')
        ++ ptr;  // skip first ^
      if (*ptr == ']')
        ++ ptr;  // skip ']' right after '[' or '^'
      while (*ptr != ']') {
        if (*ptr == '\0') {
          ptrdiff_t len = ptr - last_square + 1;
          if (len > sizeof(wrong_fmt))
            len = sizeof(wrong_fmt);
          Json_copy(wrong_fmt, last_square, len);
          PARSER_REPORT_ERROR(ctx, wrong_fmt, "not find close backet \']\'.");
          return PARSER_RETURN(ctx, -1, -1);
        }
        ++ ptr;  // skip all characters before ']'
      }
      // fall through 
    case 'c':
    case 's':
      if (discard_input)
        break;
      act_arg = PARSER_GET_ARG(ctx, n_arg, PTR_PARM_TYPE);
      PARSER_CHECK_NPD(ctx, act_arg);
      PARSER_CHECK_UAF(ctx, act_arg);
      if (malloc_buffer == TRUE) {
        PARSER_ANNOTATE_MALLOC(ctx, n_arg);
      }
      else if (nspec_1 == -1) {
        PARSER_REPORT_VSA_ERROR(ctx, act_arg, AOB);
      }
      else {
        // %s has an extra '\0' added automatically
        arg_len = (*ptr == 's') ? nspec_1 + 1 : nspec_1;
        PARSER_CHECK_OBJ_SIZE(ctx, act_arg, arg_len);
      }
      ++ n_arg;
      break;

    case 'p':
      if (discard_input)
        break;
      act_arg = PARSER_GET_ARG(ctx, n_arg, PTR_PARM_TYPE);
      PARSER_CHECK_NPD(ctx, act_arg);
      PARSER_CHECK_UAF(ctx, act_arg);
      arg_len = Is_Target_64bit() ? sizeof(long long) : sizeof(int);
      PARSER_CHECK_OBJ_SIZE(ctx, act_arg, arg_len);
      ++ n_arg;
      break;

    case 'n':
      if (discard_input)
        break;
      act_arg = PARSER_GET_ARG(ctx, n_arg, PTR_PARM_TYPE);
      PARSER_CHECK_NPD(ctx, act_arg);
      PARSER_CHECK_UAF(ctx, act_arg);
      arg_len = sizeof(int);
      PARSER_CHECK_OBJ_SIZE(ctx, act_arg, arg_len);
      ++ n_arg;
      break;

    case '%':
      arg_len = 1;
      break;

    default:
      Is_True(FALSE, ("TODO: handle %c sspec_1\n", *ptr));
      break;
    }
    n_len += arg_len;
    ++ ptr;
  }
  // printf("Parse \"%s\", total arg: %d, length: %d\n", fmt_str, n_arg, n_len);
  // ctx->parser_fini();
  // va_end(ap);
  PARSER_FINI(ctx, n_arg, n_len);

  return PARSER_RETURN(ctx, n_arg, n_arg);
}

#endif /* OPT_VSA_SCANF_INCLUDED */

