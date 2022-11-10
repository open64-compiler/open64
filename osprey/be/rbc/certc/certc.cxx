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

#include "sys_base.h"
#include "rbc_base.h"

#ifdef __cplusplus
extern "C" {
#endif

RBC_ENGINE rbc;

int execl(const char *path, const char *arg, ...)
{
  rbc.Model_decl(rbc.Set_parm_deref(rbc.Get_arg(1)));
  rbc.Model_decl(rbc.Set_parm_deref(rbc.Get_arg(2)));
  rbc.Model_decl(rbc.Set_parm_deref(rbc.Get_arg(3)));
  rbc.Model_decl(rbc.Set_parm_deref(rbc.Get_arg(4)));

  rbc.Rbc_assert(rbc.Not(rbc.Is_tag_set(rbc.Get_arg(1), "tainted")), "STR02-C");
  rbc.Rbc_assert(rbc.Not(rbc.Is_tag_set(rbc.Get_arg(2), "tainted")), "STR02-C");
  rbc.Rbc_assert(rbc.Not(rbc.Is_tag_set(rbc.Get_arg(3), "tainted")), "STR02-C");
  rbc.Rbc_assert(rbc.Not(rbc.Is_tag_set(rbc.Get_arg(4), "tainted")), "STR02-C");
  rbc.Model_decl(rbc.Set_tag_input_defval("tainted", 1));
  return 0;
}

size_t strlen(const char *s)
{
  rbc.Model_decl(rbc.Set_parm_deref(rbc.Get_arg(1)));

  rbc.Rbc_assert(rbc.Is_compatible_parm_type(1), "STR38-C");
  rbc.Rbc_assert(rbc.Is_memory_big_enough(rbc.Get_arg(1), sizeof(char), rbc.Get_strlen(rbc.Get_arg(1)) + 1), "MSR_21_17");
  return 0;
}

int execlp(const char *file, const char *arg, ...)
{
  rbc.Model_decl(rbc.Set_parm_deref(rbc.Get_arg(1)));
  rbc.Model_decl(rbc.Set_parm_deref(rbc.Get_arg(2)));
  rbc.Model_decl(rbc.Set_parm_deref(rbc.Get_arg(3)));
  rbc.Model_decl(rbc.Set_parm_deref(rbc.Get_arg(4)));

  rbc.Rbc_assert(rbc.Not(rbc.Is_tag_set(rbc.Get_arg(1), "tainted")), "STR02-C");
  rbc.Rbc_assert(rbc.Not(rbc.Is_tag_set(rbc.Get_arg(2), "tainted")), "STR02-C");
  rbc.Rbc_assert(rbc.Not(rbc.Is_tag_set(rbc.Get_arg(3), "tainted")), "STR02-C");
  rbc.Rbc_assert(rbc.Not(rbc.Is_tag_set(rbc.Get_arg(4), "tainted")), "STR02-C");
  return 0;
}

FILE *popen(const char *command, const char *type)
{
  rbc.Model_decl(rbc.Set_parm_deref(rbc.Get_arg(1)));
  rbc.Model_decl(rbc.Set_parm_deref(rbc.Get_arg(2)));

  rbc.Rbc_assert(rbc.Not(rbc.Is_tag_set(rbc.Get_arg(1), "tainted")), "STR02-C");
  return 0;
}

void *memchr(const void *ptr, int value, size_t num)
{
  rbc.Rbc_assert(rbc.Is_memory_big_enough(rbc.Get_arg(1), sizeof(char), rbc.Get_value(rbc.Get_arg(3))), "ARR38-C");
  rbc.Rbc_assert(rbc.Is_memory_big_enough(rbc.Get_arg(1), sizeof(char), rbc.Get_value(rbc.Get_arg(3))), "MSR_21_18");
  return NULL;
}

void *memcpy(void *dest, const void *src, size_t n)
{
  rbc.Model_decl(rbc.Set_parm_mod(rbc.Get_arg(1)));
  rbc.Model_decl(rbc.Set_parm_deref(rbc.Get_arg(2)));
  rbc.Model_decl(rbc.Set_implicit_assign(rbc.Get_arg(1), rbc.Get_arg(2)));
  rbc.Model_decl(rbc.Copy_tag(rbc.Get_arg(1), rbc.Get_arg(2)));

  rbc.Rbc_assert(rbc.Is_memory_big_enough(rbc.Get_arg(1), sizeof(char), rbc.Get_value(rbc.Get_arg(3))), "ARR38-C");
  rbc.Rbc_assert(rbc.Is_memory_big_enough(rbc.Get_arg(1), sizeof(char), rbc.Get_value(rbc.Get_arg(3))), "MSR_21_18");
  rbc.Rbc_assert(rbc.Not(rbc.Is_memory_overlap(rbc.Get_arg(1), rbc.Get_value(rbc.Get_arg(3)), rbc.Get_arg(2))), "MSR_19_1");
  return NULL;
}

wchar_t *wmemcpy(wchar_t *destination, const wchar_t *source, size_t num)
{
  rbc.Model_decl(rbc.Set_parm_mod(rbc.Get_arg(1)));
  rbc.Model_decl(rbc.Set_parm_deref(rbc.Get_arg(2)));
  rbc.Model_decl(rbc.Set_implicit_assign(rbc.Get_arg(1), rbc.Get_arg(2)));
  rbc.Model_decl(rbc.Copy_tag(rbc.Get_arg(1), rbc.Get_arg(2)));

  rbc.Rbc_assert(rbc.Is_memory_big_enough(rbc.Get_arg(1), sizeof(wchar_t), rbc.Get_value(rbc.Get_arg(3))), "ARR38-C");
  rbc.Rbc_assert(rbc.Is_compatible_parm_type(1), "STR38-C");
  rbc.Rbc_assert(rbc.Is_compatible_parm_type(2), "STR38-C");
  return NULL;
}

wchar_t *wmemset(wchar_t *ptr, wchar_t wc, size_t num)
{
  rbc.Model_decl(rbc.Set_parm_mod(rbc.Get_arg(1)));
  rbc.Model_decl(rbc.Set_implicit_assign(rbc.Get_arg(1), rbc.Get_arg(2)));
  rbc.Model_decl(rbc.Copy_tag(rbc.Get_arg(1), rbc.Get_arg(2)));

  rbc.Rbc_assert(rbc.Is_memory_big_enough(rbc.Get_arg(1), sizeof(wchar_t), rbc.Get_value(rbc.Get_arg(3))), "ARR38-C");
  rbc.Rbc_assert(rbc.Is_compatible_parm_type(1), "STR38-C");
  rbc.Rbc_assert(rbc.Is_compatible_parm_type(2), "STR38-C");
  return NULL;
}

size_t wcslen(const wchar_t *wcs)
{
  rbc.Model_decl(rbc.Set_parm_deref(rbc.Get_arg(1)));

  rbc.Rbc_assert(rbc.Is_compatible_parm_type(1), "STR38-C");
  return 0;
}

intmax_t wcstoimax(const wchar_t *nptr, wchar_t **endptr, int base)
{
  rbc.Model_decl(rbc.Set_func_errno_setting());

  rbc.Rbc_assert(rbc.Is_errno_cleared_before(), "MSR_22_8");
  rbc.Rbc_assert(rbc.Is_errno_checked_after(), "MSR_22_9");
  return 0;
}

uintmax_t wcstoumax(const wchar_t *nptr, wchar_t **endptr, int base)
{
  rbc.Model_decl(rbc.Set_func_errno_setting());

  rbc.Rbc_assert(rbc.Is_errno_cleared_before(), "MSR_22_8");
  rbc.Rbc_assert(rbc.Is_errno_checked_after(), "MSR_22_9");
  return 0;
}

long wcstol(const wchar_t *str, wchar_t **endptr, int base)
{
  rbc.Model_decl(rbc.Set_func_errno_setting());

  rbc.Rbc_assert(rbc.Is_errno_cleared_before(), "MSR_22_8");
  rbc.Rbc_assert(rbc.Is_errno_checked_after(), "MSR_22_9");
  return 0;
}

long long wcstoll(const wchar_t *str, wchar_t **str_end, int base)
{
  rbc.Model_decl(rbc.Set_func_errno_setting());

  rbc.Rbc_assert(rbc.Is_errno_cleared_before(), "MSR_22_8");
  rbc.Rbc_assert(rbc.Is_errno_checked_after(), "MSR_22_9");
  return 0;
}

unsigned long wcstoul(const wchar_t *str, wchar_t **str_end, int base)
{
  rbc.Model_decl(rbc.Set_func_errno_setting());

  rbc.Rbc_assert(rbc.Is_errno_cleared_before(), "MSR_22_8");
  rbc.Rbc_assert(rbc.Is_errno_checked_after(), "MSR_22_9");
  return 0;
}

unsigned long long wcstoull(const wchar_t *str, wchar_t **str_end, int base)
{
  rbc.Model_decl(rbc.Set_func_errno_setting());

  rbc.Rbc_assert(rbc.Is_errno_cleared_before(), "MSR_22_8");
  rbc.Rbc_assert(rbc.Is_errno_checked_after(), "MSR_22_9");
  return 0;
}

float wcstof(const wchar_t *str, wchar_t **str_end)
{
  rbc.Model_decl(rbc.Set_func_errno_setting());

  rbc.Rbc_assert(rbc.Is_errno_cleared_before(), "MSR_22_8");
  rbc.Rbc_assert(rbc.Is_errno_checked_after(), "MSR_22_9");
  return 0;
}

double wcstod(const wchar_t *str, wchar_t **str_end)
{
  rbc.Model_decl(rbc.Set_func_errno_setting());

  rbc.Rbc_assert(rbc.Is_errno_cleared_before(), "MSR_22_8");
  rbc.Rbc_assert(rbc.Is_errno_checked_after(), "MSR_22_9");
  return 0;
}

long double wcstold(const wchar_t *str, wchar_t **str_end)
{
  rbc.Model_decl(rbc.Set_func_errno_setting());

  rbc.Rbc_assert(rbc.Is_errno_cleared_before(), "MSR_22_8");
  rbc.Rbc_assert(rbc.Is_errno_checked_after(), "MSR_22_9");
  return 0;
}

size_t wcrtomb(char *s, wchar_t wc, mbstate_t *ps)
{
  rbc.Model_decl(rbc.Set_func_errno_setting());

  rbc.Rbc_assert(rbc.Is_errno_cleared_before(), "MSR_22_8");
  rbc.Rbc_assert(rbc.Is_errno_checked_after(), "MSR_22_9");
  return 0;
}

size_t wcstombs(char *dst, const wchar_t *src, size_t len)
{
  rbc.Model_decl(rbc.Set_func_errno_setting());

  rbc.Rbc_assert(rbc.Is_errno_cleared_before(), "MSR_22_8");
  rbc.Rbc_assert(rbc.Is_errno_checked_after(), "MSR_22_9");
  return 0;
}

size_t mbrtowc(wchar_t *pwc, const char *s, size_t n, mbstate_t *ps)
{
  rbc.Model_decl(rbc.Set_func_errno_setting());

  rbc.Rbc_assert(rbc.Is_errno_cleared_before(), "MSR_22_8");
  rbc.Rbc_assert(rbc.Is_errno_checked_after(), "MSR_22_9");
  return 0;
}

void *memmove(void *dest, const void *src, size_t n)
{
  rbc.Model_decl(rbc.Set_parm_mod(rbc.Get_arg(1)));
  rbc.Model_decl(rbc.Set_parm_deref(rbc.Get_arg(2)));
  rbc.Model_decl(rbc.Set_implicit_assign(rbc.Get_arg(1), rbc.Get_arg(2)));
  rbc.Model_decl(rbc.Copy_tag(rbc.Get_arg(1), rbc.Get_arg(2)));

  rbc.Rbc_assert(rbc.Is_memory_big_enough(rbc.Get_arg(1), sizeof(char), rbc.Get_value(rbc.Get_arg(3))), "ARR38-C");
  rbc.Rbc_assert(rbc.Is_memory_big_enough(rbc.Get_arg(1), sizeof(char), rbc.Get_value(rbc.Get_arg(3))), "MSR_21_18");
  return NULL;
}

int memcmp(const void *s1, const void *s2, size_t n)
{
  rbc.Model_decl(rbc.Set_parm_deref(rbc.Get_arg(1)));
  rbc.Model_decl(rbc.Set_parm_deref(rbc.Get_arg(2)));

  rbc.Rbc_assert(rbc.Is_memory_big_enough(rbc.Get_arg(1), sizeof(char), rbc.Get_value(rbc.Get_arg(3))), "ARR38-C");
  rbc.Rbc_assert(rbc.Is_memory_big_enough(rbc.Get_arg(1), sizeof(char), rbc.Get_value(rbc.Get_arg(3))), "MSR_21_18");
  rbc.Rbc_assert(rbc.Not(rbc.Or(rbc.Is_null_term_str(rbc.Get_arg(1), rbc.Get_value(rbc.Get_arg(3))),
                                rbc.Is_null_term_str(rbc.Get_arg(2), rbc.Get_value(rbc.Get_arg(3))))),
                 "MSR_21_14");
  // TODO: EXP42-C
  return 0;
}

void *memset(void *ptr, int value, size_t num)
{
  rbc.Model_decl(rbc.Set_parm_mod(rbc.Get_arg(1)));
  rbc.Model_decl(rbc.Set_implicit_assign(rbc.Get_arg(1), rbc.Get_arg(2)));
  rbc.Model_decl(rbc.Copy_tag(rbc.Get_arg(1), rbc.Get_arg(2)));

  rbc.Rbc_assert(rbc.Is_memory_big_enough(rbc.Get_arg(1), sizeof(char), rbc.Get_value(rbc.Get_arg(3))), "ARR38-C");
  rbc.Rbc_assert(rbc.Is_memory_big_enough(rbc.Get_arg(1), sizeof(char), rbc.Get_value(rbc.Get_arg(3))), "MSR_21_18");
  return NULL;
}

int atexit(void (*func)(void))
{
  // rule #1 : all registered functions should terminate by returning only.
  rbc.Rbc_assert(!rbc.Func_may_not_return(rbc.Get_arg(1)), "ENV32-C");
  // rule #2 : all registered functions should not be in recursive calls
  rbc.Rbc_assert(!rbc.Func_may_enter_recursion(rbc.Get_arg(1)), "ENV32-C");
  return 0;
}

FILE *fopen(const char *filename, const char *mode)
{
  rbc.Model_decl(rbc.Set_parm_deref(rbc.Get_arg(1)));
  rbc.Model_decl(rbc.Set_parm_deref(rbc.Get_arg(2)));
  rbc.Model_decl(rbc.Fsm_use("fio45c"));

  rbc.Rbc_assert(rbc.Not(rbc.Is_tag_set(rbc.Get_arg(1), "tainted")), "FIO02-C");
  return NULL;
}

FILE *_wfopen(const wchar_t *filename, const wchar_t *mode)
{
  rbc.Model_decl(rbc.Set_parm_deref(rbc.Get_arg(1)));
  rbc.Model_decl(rbc.Set_parm_deref(rbc.Get_arg(2)));

  rbc.Rbc_assert(rbc.Post_check_var_value(rbc.Get_ret(), "ne", NULL), "ERR33-C");
  rbc.Rbc_assert(rbc.Post_check_var_value(rbc.Get_ret(), "ne", NULL), "MSR_D_4_7");
  return NULL;
}

size_t fread(void *ptr, size_t size, size_t nmemb, FILE *stream)
{
  rbc.Model_decl(rbc.Set_parm_mod(rbc.Get_arg(1)));
  rbc.Model_decl(rbc.Set_parm_deref(rbc.Get_arg(4)));
  rbc.Model_decl(rbc.Fsm_use("fio45c"));

  rbc.Rbc_assert(rbc.Post_check_var_value(rbc.Get_ret(), "ne", rbc.Get_arg(3)), "ERR33-C");
  rbc.Rbc_assert(rbc.Post_check_var_value(rbc.Get_ret(), "ne", rbc.Get_arg(3)), "MSR_D_4_7");
  rbc.Rbc_assert(rbc.Is_memory_big_enough(rbc.Get_arg(1), rbc.Get_value(rbc.Get_arg(2)), rbc.Get_value(rbc.Get_arg(3))), "ARR38-C");
  return 0;
}

struct tm *gmtime(const time_t *timep)
{
  rbc.Model_decl(rbc.Set_parm_deref(rbc.Get_arg(1)));

  rbc.Rbc_assert(rbc.Not(rbc.Is_var_invalid_and_used_after(rbc.Get_ret())), "MSR_21_20");
  return NULL;
}

struct tm *gmtime_r(const time_t *timep, struct tm *result)
{
  rbc.Model_decl(rbc.Set_parm_deref(rbc.Get_arg(1)));
  rbc.Model_decl(rbc.Set_parm_mod(rbc.Get_arg(2)));
  return NULL;
}

char *ctime(const time_t *timep)
{
  rbc.Rbc_same_as_func("gmtime");
  return NULL;
}

char *ctime_r(const time_t *timep, char *buf)
{
  rbc.Rbc_same_as_func("gmtime_r");
  return NULL;
}
struct tm *localtime_r(const time_t *timep, struct tm *result)
{
  rbc.Rbc_same_as_func("gimtime_r");
  return NULL;
}

size_t strftime(char *s, size_t max, const char *format, const struct tm *tm)
{
  rbc.Model_decl(rbc.Set_parm_mod(rbc.Get_arg(1)));
  rbc.Model_decl(rbc.Set_parm_deref(rbc.Get_arg(3)));
  rbc.Model_decl(rbc.Set_parm_deref(rbc.Get_arg(4)));
  return 0;
}

size_t fwrite(const void *ptr, size_t size, size_t count, FILE *stream)
{
  rbc.Model_decl(rbc.Set_parm_deref(rbc.Get_arg(1)));
  rbc.Model_decl(rbc.Set_parm_deref(rbc.Get_arg(4)));
  rbc.Model_decl(rbc.Fsm_use("fio45c"));

  rbc.Rbc_assert(rbc.Post_check_var_value(rbc.Get_ret(), "ne", rbc.Get_arg(3)), "ERR33-C");
  rbc.Rbc_assert(rbc.Post_check_var_value(rbc.Get_ret(), "ne", rbc.Get_arg(3)), "MSR_D_4_7");
  rbc.Rbc_assert(rbc.Is_memory_big_enough(rbc.Get_arg(1), rbc.Get_value(rbc.Get_arg(2)), rbc.Get_value(rbc.Get_arg(3))), "ARR38-C");
  return 0;
}

int fseek(FILE *stream, long offset, int whence)
{
  rbc.Model_decl(rbc.Set_parm_deref(rbc.Get_arg(1)));

  rbc.Rbc_assert(rbc.Post_check_var_value(rbc.Get_ret(), "ne", 0), "ERR33-C");
  rbc.Rbc_assert(rbc.Post_check_var_value(rbc.Get_ret(), "ne", 0), "MSR_D_4_7");
  return 0;
}

long ftell(FILE *stream)
{
  rbc.Model_decl(rbc.Set_parm_deref(rbc.Get_arg(1)));
  rbc.Model_decl(rbc.Set_func_errno_setting());

  rbc.Rbc_assert(rbc.Is_errno_cleared_before(), "MSR_22_8");
  rbc.Rbc_assert(rbc.Is_errno_checked_after(), "MSR_22_9");
  return 0;
}

void rewind(FILE *stream)
{
  rbc.Rbc_same_as_func("ftell");
  return;
}

int fclose(FILE *stream)
{
  rbc.Model_decl(rbc.Set_parm_deref(rbc.Get_arg(1)));
  rbc.Model_decl(rbc.Fsm_use("fio45c"));

  rbc.Rbc_assert(rbc.Not(rbc.Is_var_used_after(rbc.Get_arg(1))), "MSR_22_6");
  return 0;
}

int rand(void)
{
  rbc.Rbc_assert(rbc.Do_not_get_called(), "MSC30-C");
  return 0;
}

int setvbuf(FILE *stream, char *buf, int mode, size_t size)
{
  rbc.Model_decl(rbc.Set_parm_deref(rbc.Get_arg(1)));
  return 0;
}

void setbuf(FILE *stream, char *buf)
{
  rbc.Rbc_same_as_func("setvbuf");
  return;
}

void setbuffer(FILE *stream, char *buf, size_t size)
{
  rbc.Rbc_same_as_func("setvbuf");
  return;
}

void setlinebuf(FILE *stream)
{
  rbc.Rbc_same_as_func("setvbuf");
  return;
}

long int random(void)
{
  rbc.Rbc_assert(rbc.Pre_call("srandom"), "MSC32-C");
  return 0;
}

char *setlocale(int category, const char *locale)
{
  rbc.Rbc_assert(rbc.Post_check_var_value(rbc.Get_ret(), "ne", NULL), "ERR33-C");
  rbc.Rbc_assert(rbc.Post_check_var_value(rbc.Get_ret(), "ne", NULL), "MSR_D_4_7");
  rbc.Rbc_assert(rbc.Not(rbc.Is_var_defined_after(rbc.Get_ret())), "MSR_21_19");
  rbc.Rbc_assert(rbc.Not(rbc.Is_var_invalid_and_used_after(rbc.Get_ret())), "MSR_21_20");
  return 0;
}

FILE *fmemopen(void *buf, size_t size, const char *mode)
{
  rbc.Model_decl(rbc.Set_parm_mod(rbc.Get_arg(1)));  // mode[0] == 'w' || mode[0] == '+'
  rbc.Model_decl(rbc.Set_parm_deref(rbc.Get_arg(3)));

  rbc.Rbc_assert(rbc.Post_check_var_value(rbc.Get_ret(), "ne", NULL), "POS54-C");
  return 0;
}

FILE *open_memstream(char **ptr, size_t *sizeloc)
{
  rbc.Rbc_assert(rbc.Post_check_var_value(rbc.Get_ret(), "ne", NULL), "POS54-C");
  return 0;
}

void *mmap(void *addr, size_t length, int prot, int flags, int fd, off_t offset)
{
  rbc.Rbc_assert(rbc.Post_check_var_value(rbc.Get_ret(), "ne", MAP_FAILED), "POS54-C");
  return 0;
}

int munmap(void *addr, size_t length)
{
  rbc.Model_decl(rbc.Set_parm_deref(rbc.Get_arg(1)));
  return 0;
}

void *mremap(void *old_address, size_t old_size, size_t new_size, int flags, ...)
{
  rbc.Model_decl(rbc.Set_parm_deref(rbc.Get_arg(1)));
  rbc.Model_decl(rbc.Set_parm_deref(rbc.Get_arg(5)));
  return NULL;
}

int dlclose(void *handle)
{
  rbc.Model_decl(rbc.Set_parm_deref(rbc.Get_arg(1)));
  rbc.Model_decl(rbc.Set_parm_mod(rbc.Get_arg(1)));
  return 0;
}

void *malloc(size_t size)
{
  rbc.Model_decl(rbc.Set_tag(rbc.Get_ret(), (char *)"malloc"));

  rbc.Rbc_assert(rbc.Get_value(rbc.Get_arg(1)) >= rbc.Get_mem_size(rbc.Get_ret()), "MEM35-C");
  rbc.Rbc_assert(rbc.Post_check_var_value(rbc.Get_ret(), "ne", NULL), "ERR33-C");
  rbc.Rbc_assert(rbc.Post_check_var_value(rbc.Get_ret(), "ne", NULL), "MSR_D_4_7");
  return NULL;
}

void *realloc(void *ptr, size_t size)
{
  rbc.Model_decl(rbc.Set_parm_mod(rbc.Get_arg(1)));
  rbc.Model_decl(rbc.Set_tag(rbc.Get_ret(), (char *)"malloc"));

  rbc.Rbc_assert(rbc.Post_check_var_value(rbc.Get_ret(), "ne", NULL), "ERR33-C");
  rbc.Rbc_assert(rbc.Post_check_var_value(rbc.Get_ret(), "ne", NULL), "MSR_D_4_7");
  // TODO: STR32-C
  return NULL;
}

void *calloc(size_t nmemb, size_t size)
{
  rbc.Model_decl(rbc.Set_tag(rbc.Get_ret(), (char *)"malloc"));

  rbc.Rbc_assert(rbc.Post_check_var_value(rbc.Get_ret(), "ne", NULL), "ERR33-C");
  rbc.Rbc_assert(rbc.Post_check_var_value(rbc.Get_ret(), "ne", NULL), "MSR_D_4_7");
  return NULL;
}

void free(void *ptr)
{
  //rbc.Rbc_assert(rbc.Is_tag_set(rbc.Get_arg(1), "malloc"), "MEM51-CPP");
}

int system(const char *command)
{
  rbc.Model_decl(rbc.Set_parm_deref(rbc.Get_arg(1)));

  rbc.Rbc_assert(rbc.Do_not_get_called(), "ENV33-C");
  rbc.Rbc_assert(rbc.Not(rbc.Is_tag_set(rbc.Get_arg(1), "tainted")), "STR02-C");
  return 0;
}

ssize_t readlink(const char *pathname, char *buf, size_t bufsiz)
{
  rbc.Model_decl(rbc.Set_parm_deref(rbc.Get_arg(1)));
  rbc.Model_decl(rbc.Set_parm_mod(rbc.Get_arg(2)));

  rbc.Rbc_assert(rbc.Get_mem_size(rbc.Get_arg(2)) > rbc.Get_value(rbc.Get_arg(3)), "POS30-C");
  return 0;
}

char *asctime(const struct tm *timeptr)
{
  rbc.Model_decl(rbc.Set_parm_deref(rbc.Get_arg(1)));

  rbc.Rbc_assert(rbc.Pre_sanitized(rbc.Get_arg(1)), "MSC33-C");
  rbc.Rbc_assert(rbc.Not(rbc.Is_var_invalid_and_used_after(rbc.Get_ret())), "MSR_21_20");
  return NULL;
}

struct tm *localtime(const time_t *timep)
{
  rbc.Model_decl(rbc.Set_parm_deref(rbc.Get_arg(1)));
  rbc.Model_decl(rbc.Func_performs_sanitize());

  rbc.Rbc_assert(rbc.Not(rbc.Is_var_invalid_and_used_after(rbc.Get_ret())), "MSR_21_20");
  return NULL;
}

struct lconv *localeconv (void)
{
  rbc.Rbc_assert(rbc.Not(rbc.Is_var_defined_after(rbc.Get_ret())), "MSR_21_19");
  rbc.Rbc_assert(rbc.Not(rbc.Is_var_invalid_and_used_after(rbc.Get_ret())), "MSR_21_20");
  return NULL;
}

int putenv(char *string)
{
  rbc.Model_decl(rbc.Set_parm_deref(rbc.Get_arg(1)));

  rbc.Rbc_assert(!rbc.Is_automatic_variable(rbc.Get_arg(1)), "POS34-C");
  return NULL;
}

sighandler_t signal(int signum, sighandler_t handler)
{
  rbc.Rbc_assert(rbc.Func_is_asynchronous_safe(rbc.Get_arg(2)), "SIG30-C");
  rbc.Rbc_assert(rbc.Do_not_access_shared_obj(rbc.Get_arg(2)), "SIG31-C");
  return NULL;
}

char *getenv(const char *name)
{
  rbc.Model_decl(rbc.Set_parm_deref(rbc.Get_arg(1)));
  rbc.Model_decl(rbc.Set_tag(rbc.Get_ret(), (char *)"tainted"));

  rbc.Rbc_assert(rbc.Is_dynamically_allocated_if_copied(rbc.Get_ret()), "STR31-C");
  rbc.Rbc_assert(rbc.Not(rbc.Is_var_defined_after(rbc.Get_ret())), "MSR_21_19");
  rbc.Rbc_assert(rbc.Not(rbc.Is_var_invalid_and_used_after(rbc.Get_ret())), "MSR_21_20");
  return NULL;
}

char *strerror(int errnum)
{
  rbc.Rbc_assert(rbc.Not(rbc.Is_var_defined_after(rbc.Get_ret())), "MSR_21_19");
  rbc.Rbc_assert(rbc.Not(rbc.Is_var_invalid_and_used_after(rbc.Get_ret())), "MSR_21_20");
  return NULL;
}

void qsort(void *base, size_t nmemb, size_t size,
            int (*compar)(const void *, const void *))
{
  rbc.Model_decl(rbc.Set_parm_deref(rbc.Get_arg(1)));
  rbc.Model_decl(rbc.Set_parm_deref(rbc.Get_arg(4)));
  rbc.Model_decl(rbc.Set_parm_mod(rbc.Get_arg(1)));
  return;
}

double sqrt(double __x)
{
  rbc.Rbc_assert(rbc.Pre_check_var_value(rbc.Get_arg(1), "gt", 0), "MSR_D_4_11");
  return 0;
}

double log(double __x)
{
  rbc.Rbc_assert(rbc.Pre_check_var_value(rbc.Get_arg(1), "gt", 0), "MSR_D_4_11");
  return 0;
}

double fmod(double __x, double __y)
{
  rbc.Rbc_assert(rbc.Pre_check_var_value(rbc.Get_arg(2), "ne", 0), "MSR_D_4_11");
  return 0;
}

char *gets(char *s)
{
  rbc.Model_decl(rbc.Set_parm_mod(rbc.Get_arg(1)));

  rbc.Rbc_assert(rbc.Do_not_get_called(), "STR31-C");
  return NULL;
}

char *strcat(char *dest, const char *src)
{
  rbc.Model_decl(rbc.Set_parm_mod(rbc.Get_arg(1)));
  rbc.Model_decl(rbc.Set_parm_deref(rbc.Get_arg(1)));
  rbc.Model_decl(rbc.Set_parm_deref(rbc.Get_arg(2)));
  rbc.Model_decl(rbc.Set_implicit_assign(rbc.Get_arg(1), rbc.Get_arg(2)));
  rbc.Model_decl(rbc.Merge_tag(rbc.Get_arg(1), rbc.Get_arg(1), rbc.Get_arg(2)));

  rbc.Rbc_assert(rbc.Is_memory_big_enough(rbc.Get_arg(1), sizeof(char),
                                          rbc.Get_strlen(rbc.Get_arg(1)) + rbc.Get_strlen(rbc.Get_arg(2)) + 1),
                 "MSR_21_17");
  return NULL;
}

char *strncat(char *dest, const char *src, size_t n)
{
  rbc.Model_decl(rbc.Set_parm_mod(rbc.Get_arg(1)));
  rbc.Model_decl(rbc.Set_parm_deref(rbc.Get_arg(2)));
  rbc.Model_decl(rbc.Set_implicit_assign(rbc.Get_arg(1), rbc.Get_arg(2)));
  rbc.Model_decl(rbc.Merge_tag(rbc.Get_arg(1), rbc.Get_arg(1), rbc.Get_arg(2)));

  rbc.Rbc_assert(rbc.Is_memory_big_enough(rbc.Get_arg(1), sizeof(char), rbc.Get_value(rbc.Get_arg(3))), "ARR38-C");
  rbc.Rbc_assert(rbc.Is_memory_big_enough(rbc.Get_arg(1), sizeof(char), rbc.Get_value(rbc.Get_arg(3))), "MSR_21_18");
  return NULL;
}

char *strncpy(char *destination, const char *source, size_t num)
{
  rbc.Model_decl(rbc.Set_parm_mod(rbc.Get_arg(1)));
  rbc.Model_decl(rbc.Set_parm_deref(rbc.Get_arg(2)));
  rbc.Model_decl(rbc.Set_implicit_assign(rbc.Get_arg(1), rbc.Get_arg(2)));
  rbc.Model_decl(rbc.Copy_tag(rbc.Get_arg(1), rbc.Get_arg(2)));

  rbc.Rbc_assert(rbc.Is_memory_big_enough(rbc.Get_arg(1), sizeof(char), rbc.Get_value(rbc.Get_arg(3))), "ARR38-C");
  rbc.Rbc_assert(rbc.Is_memory_big_enough(rbc.Get_arg(1), sizeof(char), rbc.Get_value(rbc.Get_arg(3))), "MSR_21_18");
  rbc.Rbc_assert(rbc.Is_compatible_parm_type(1), "STR38-C");
  rbc.Rbc_assert(rbc.Is_compatible_parm_type(2), "STR38-C");
  return NULL;
}

size_t strxfrm(char *destination, const char *source, size_t num)
{
  rbc.Rbc_assert(rbc.Is_memory_big_enough(rbc.Get_arg(1), sizeof(char), rbc.Get_value(rbc.Get_arg(3))), "ARR38-C");
  rbc.Rbc_assert(rbc.Is_memory_big_enough(rbc.Get_arg(1), sizeof(char), rbc.Get_value(rbc.Get_arg(3))), "MSR_21_18");
  return 0;
}

char *strcpy(char *destination, const char *source)
{
  rbc.Model_decl(rbc.Set_parm_mod(rbc.Get_arg(1)));
  rbc.Model_decl(rbc.Set_parm_deref(rbc.Get_arg(2)));
  rbc.Model_decl(rbc.Set_implicit_assign(rbc.Get_arg(1), rbc.Get_arg(2)));
  rbc.Model_decl(rbc.Copy_tag(rbc.Get_arg(1), rbc.Get_arg(2)));

  rbc.Rbc_assert(rbc.Get_mem_size(rbc.Get_arg(1)) >= rbc.Get_mem_size(rbc.Get_arg(2)), "STR31-C");
  rbc.Rbc_assert(!rbc.Hard_coded_password(rbc.Get_arg(1), rbc.Get_arg(2)), "MSC41-C");
  rbc.Rbc_assert(rbc.Is_memory_big_enough(rbc.Get_arg(1), sizeof(char), rbc.Get_strlen(rbc.Get_arg(2)) + 1), "MSR_21_17");
  return NULL;
}

wchar_t *wcscpy(wchar_t *destination, const wchar_t *source)
{
  rbc.Model_decl(rbc.Set_parm_mod(rbc.Get_arg(1)));
  rbc.Model_decl(rbc.Set_parm_deref(rbc.Get_arg(2)));
  rbc.Model_decl(rbc.Set_implicit_assign(rbc.Get_arg(1), rbc.Get_arg(2)));
  rbc.Model_decl(rbc.Copy_tag(rbc.Get_arg(1), rbc.Get_arg(2)));

  rbc.Rbc_assert(rbc.Get_elem_count(rbc.Get_arg(1)) > rbc.Get_elem_count(rbc.Get_arg(2)), "ARR38-C");
  rbc.Rbc_assert(rbc.Is_compatible_parm_type(1), "STR38-C");
  rbc.Rbc_assert(rbc.Is_compatible_parm_type(2), "STR38-C");
  rbc.Rbc_assert(!rbc.Hard_coded_password(rbc.Get_arg(1), rbc.Get_arg(2)), "MSC41-C");
  return NULL;
}

wchar_t *wcsncpy(wchar_t *destination, const wchar_t *source, size_t num)
{
  rbc.Model_decl(rbc.Set_parm_mod(rbc.Get_arg(1)));
  rbc.Model_decl(rbc.Set_parm_deref(rbc.Get_arg(2)));
  rbc.Model_decl(rbc.Set_implicit_assign(rbc.Get_arg(1), rbc.Get_arg(2)));
  rbc.Model_decl(rbc.Copy_tag(rbc.Get_arg(1), rbc.Get_arg(2)));

  rbc.Rbc_assert(rbc.Is_compatible_parm_type(1), "STR38-C");
  rbc.Rbc_assert(rbc.Is_compatible_parm_type(2), "STR38-C");
  rbc.Rbc_assert(rbc.Is_memory_big_enough(rbc.Get_arg(1), sizeof(wchar_t), rbc.Get_value(rbc.Get_arg(3))), "ARR38-C");
  return NULL;
}

wchar_t *wcsncat(wchar_t *dest, const wchar_t *src, size_t n)
{
  rbc.Model_decl(rbc.Set_parm_mod(rbc.Get_arg(1)));
  rbc.Model_decl(rbc.Set_parm_deref(rbc.Get_arg(2)));
  rbc.Model_decl(rbc.Set_implicit_assign(rbc.Get_arg(1), rbc.Get_arg(2)));
  rbc.Model_decl(rbc.Merge_tag(rbc.Get_arg(1), rbc.Get_arg(1), rbc.Get_arg(2)));

  rbc.Rbc_assert(rbc.Is_memory_big_enough(rbc.Get_arg(1), sizeof(wchar_t), rbc.Get_value(rbc.Get_arg(3))), "ARR38-C");
  return NULL;
}

int strcmp(const char *s1, const char *s2)
{
  rbc.Model_decl(rbc.Set_parm_deref(rbc.Get_arg(1)));
  rbc.Model_decl(rbc.Set_parm_deref(rbc.Get_arg(2)));

  rbc.Rbc_assert(rbc.Is_memory_big_enough(rbc.Get_arg(1), sizeof(char), rbc.Get_strlen(rbc.Get_arg(1)) + 1), "MSR_21_17");
  rbc.Rbc_assert(rbc.Is_memory_big_enough(rbc.Get_arg(2), sizeof(char), rbc.Get_strlen(rbc.Get_arg(2)) + 1), "MSR_21_17");
  return 0;
}

int strncmp(const char *s1, const char *s2, size_t n)
{
  rbc.Model_decl(rbc.Set_parm_deref(rbc.Get_arg(1)));
  rbc.Model_decl(rbc.Set_parm_deref(rbc.Get_arg(2)));

  rbc.Rbc_assert(rbc.Is_memory_big_enough(rbc.Get_arg(1), sizeof(char), rbc.Get_value(rbc.Get_arg(3))), "ARR38-C");
  rbc.Rbc_assert(rbc.Is_memory_big_enough(rbc.Get_arg(1), sizeof(char), rbc.Get_value(rbc.Get_arg(3))), "MSR_21_18");
  return 0;
}

int strcoll(const char *str1, const char *str2)
{
  rbc.Rbc_assert(rbc.Is_memory_big_enough(rbc.Get_arg(1), sizeof(char), rbc.Get_strlen(rbc.Get_arg(1)) + 1), "MSR_21_17");
  rbc.Rbc_assert(rbc.Is_memory_big_enough(rbc.Get_arg(2), sizeof(char), rbc.Get_strlen(rbc.Get_arg(2)) + 1), "MSR_21_17");
  return 0;
}

char *strchr(const char *s, int c)
{
  rbc.Model_decl(rbc.Set_parm_deref(rbc.Get_arg(1)));

  rbc.Rbc_assert(rbc.Is_memory_big_enough(rbc.Get_arg(1), sizeof(char), rbc.Get_strlen(rbc.Get_arg(1)) + 1), "MSR_21_17");
  return NULL;
}

size_t strcspn(const char *s, const char *reject)
{
  rbc.Model_decl(rbc.Set_parm_deref(rbc.Get_arg(1)));
  rbc.Model_decl(rbc.Set_parm_deref(rbc.Get_arg(2)));

  rbc.Rbc_assert(rbc.Is_memory_big_enough(rbc.Get_arg(1), sizeof(char), rbc.Get_strlen(rbc.Get_arg(1)) + 1), "MSR_21_17");
  rbc.Rbc_assert(rbc.Is_memory_big_enough(rbc.Get_arg(2), sizeof(char), rbc.Get_strlen(rbc.Get_arg(2)) + 1), "MSR_21_17");
  return 0; 
}

size_t strspn(const char *s, const char *accept)
{
  rbc.Rbc_same_as_func("strcspn");
  return 0;
}

char *strrchr(const char *s, int c)
{
  rbc.Model_decl(rbc.Set_parm_deref(rbc.Get_arg(1)));

  rbc.Rbc_assert(rbc.Is_memory_big_enough(rbc.Get_arg(1), sizeof(char), rbc.Get_strlen(rbc.Get_arg(1)) + 1), "MSR_21_17");
  return NULL;
}

char *strpbrk(const char *s, const char *accept)
{
  rbc.Rbc_assert(rbc.Is_memory_big_enough(rbc.Get_arg(1), sizeof(char), rbc.Get_strlen(rbc.Get_arg(1)) + 1), "MSR_21_17");
  rbc.Rbc_assert(rbc.Is_memory_big_enough(rbc.Get_arg(2), sizeof(char), rbc.Get_strlen(rbc.Get_arg(2)) + 1), "MSR_21_17");
  return NULL;
}

char *strstr(const char *haystack, const char *needle)
{
  rbc.Model_decl(rbc.Set_parm_deref(rbc.Get_arg(1)));
  rbc.Model_decl(rbc.Set_parm_deref(rbc.Get_arg(2)));

  rbc.Rbc_assert(rbc.Is_memory_big_enough(rbc.Get_arg(1), sizeof(char), rbc.Get_strlen(rbc.Get_arg(1)) + 1), "MSR_21_17");
  rbc.Rbc_assert(rbc.Is_memory_big_enough(rbc.Get_arg(2), sizeof(char), rbc.Get_strlen(rbc.Get_arg(2)) + 1), "MSR_21_17");
  return NULL;
}

intmax_t strtoimax(const char *nptr, char **endptr, int base)
{
  rbc.Model_decl(rbc.Set_func_errno_setting());

  rbc.Rbc_assert(rbc.Is_errno_cleared_before(), "MSR_22_8");
  rbc.Rbc_assert(rbc.Is_errno_checked_after(), "MSR_22_9");
  return 0;
}

uintmax_t strtoumax(const char *nptr, char **endptr, int base)
{
  rbc.Model_decl(rbc.Set_func_errno_setting());

  rbc.Rbc_assert(rbc.Is_errno_cleared_before(), "MSR_22_8");
  rbc.Rbc_assert(rbc.Is_errno_checked_after(), "MSR_22_9");
  return 0;
}

char *strtok(char *str, const char *delimiters)
{
  rbc.Rbc_assert(rbc.Is_memory_big_enough(rbc.Get_arg(1), sizeof(char), rbc.Get_strlen(rbc.Get_arg(1)) + 1), "MSR_21_17");
  rbc.Rbc_assert(rbc.Is_memory_big_enough(rbc.Get_arg(2), sizeof(char), rbc.Get_strlen(rbc.Get_arg(2)) + 1), "MSR_21_17");
  return NULL;
}

char *strtok_r(char *str, const char *delim, char **saveptr)
{
  rbc.Rbc_same_as_func("strtok");
  return NULL;
}

int getchar(void)
{
  rbc.Rbc_assert(rbc.Post_call("feof"), "FIO34-C");
  return 0;
}

int fgetc(FILE *stream)
{
  rbc.Model_decl(rbc.Set_parm_deref(rbc.Get_arg(1)));

  rbc.Rbc_assert(rbc.Post_check_var_value(rbc.Get_ret(), "eq", (void*)EOF), "ERR33-C");
  rbc.Rbc_assert(rbc.Post_check_var_value(rbc.Get_ret(), "eq", (void*)EOF), "MSR_D_4_7");
  return 0;
}

int getc(FILE *stream)
{
  rbc.Model_decl(rbc.Set_parm_deref(rbc.Get_arg(1)));

  rbc.Rbc_assert(rbc.Post_call("feof"), "FIO34-C");
  rbc.Rbc_rule_exception("FIO34-C", rbc.Is_called_by("getchar"));
  return 0;
}

int _IO_getc(FILE *stream)
{
  rbc.Model_decl(rbc.Set_parm_deref(rbc.Get_arg(1)));

  rbc.Rbc_assert(rbc.Post_call("feof"), "FIO34-C");
  rbc.Rbc_rule_exception("FIO34-C", rbc.Is_called_by("getchar"));
  return 0;
}

wint_t getwc (FILE *stream)
{
  rbc.Model_decl(rbc.Set_parm_deref(rbc.Get_arg(1)));

  rbc.Rbc_assert(rbc.Post_call("feof"), "FIO34-C");// && rbc.Post_call("ferror"),
  return 0;
}

wint_t fgetwc(FILE *stream)
{
  rbc.Model_decl(rbc.Set_func_errno_setting());

  rbc.Rbc_assert(rbc.Is_errno_cleared_before(), "MSR_22_8");
  rbc.Rbc_assert(rbc.Is_errno_checked_after(), "MSR_22_9");
  return 0;
}

char *fgets(char *str, int num, FILE *stream)
{
  rbc.Model_decl(rbc.Set_parm_mod(rbc.Get_arg(1)));
  rbc.Model_decl(rbc.Set_tag(rbc.Get_arg(1), (char *)"tainted"));
  rbc.Model_decl(rbc.Set_parm_deref(rbc.Get_arg(3)));

  rbc.Rbc_assert(rbc.Post_check_var_value(rbc.Get_ret(), "eq", NULL), "ERR33-C");
  rbc.Rbc_assert(rbc.Post_check_var_value(rbc.Get_ret(), "eq", NULL), "MSR_D_4_7");
  rbc.Rbc_assert(rbc.Post_check_var_func(rbc.Get_arg(1), "strchr", "\n"), "FIO37-C");
  rbc.Rbc_assert(rbc.Is_memory_big_enough(rbc.Get_arg(1), sizeof(char), rbc.Get_value(rbc.Get_arg(2))), "STR31-C");
  // TODO: FIO40-C
  return NULL;
}

wchar_t *fgetws(wchar_t *ws, int n, FILE *stream)
{
  rbc.Model_decl(rbc.Set_parm_mod(rbc.Get_arg(1)));
  rbc.Model_decl(rbc.Set_tag(rbc.Get_arg(1), (char *)"tainted"));
  rbc.Model_decl(rbc.Set_parm_deref(rbc.Get_arg(3)));

  rbc.Rbc_assert(rbc.Post_check_var_value(rbc.Get_ret(), "eq", NULL), "ERR33-C");
  rbc.Rbc_assert(rbc.Post_check_var_value(rbc.Get_ret(), "eq", NULL), "MSR_D_4_7");
  rbc.Rbc_assert(rbc.Post_check_var_func(rbc.Get_arg(1), "strchr", "\n"), "FIO37-C");
  rbc.Rbc_assert(rbc.Get_elem_count(rbc.Get_arg(1)) >= rbc.Get_value(rbc.Get_arg(2)), "STR31-C");
  return NULL;
}

int fputc(int c, FILE *stream)
{
  rbc.Model_decl(rbc.Set_parm_mod(rbc.Get_arg(2)));
  rbc.Model_decl(rbc.Fsm_use("fio45c"));

  rbc.Rbc_assert(rbc.Post_check_var_value(rbc.Get_ret(), "eq", (void*)EOF), "ERR33-C");
  rbc.Rbc_assert(rbc.Post_check_var_value(rbc.Get_ret(), "eq", (void*)EOF), "MSR_D_4_7");
  // rbc.Rbc_rule_exception("ERR33-C", rbc.Is_std_output(rbc.Get_arg(2)));
  return 0;
}

int fputs(const char *s, FILE *stream)
{
  rbc.Model_decl(rbc.Set_parm_deref(rbc.Get_arg(1)));
  rbc.Model_decl(rbc.Set_parm_mod(rbc.Get_arg(2)));
  rbc.Model_decl(rbc.Fsm_use("fio45c"));

  rbc.Rbc_assert(rbc.Post_check_var_value(rbc.Get_ret(), "eq", (void*)EOF), "ERR33-C");
  rbc.Rbc_assert(rbc.Post_check_var_value(rbc.Get_ret(), "eq", (void*)EOF), "MSR_D_4_7");
  // rbc.Rbc_rule_exception("ERR33-C", rbc.Is_std_output(rbc.Get_arg(2)));
  return 0;
}

wint_t fputwc(wchar_t wc, FILE *stream)
{
  rbc.Model_decl(rbc.Set_func_errno_setting());

  rbc.Rbc_assert(rbc.Is_errno_cleared_before(), "MSR_22_8");
  rbc.Rbc_assert(rbc.Is_errno_checked_after(), "MSR_22_9");
  return 0;
}

int putc(int c, FILE *stream)
{
  rbc.Model_decl(rbc.Set_parm_mod(rbc.Get_arg(2)));

  rbc.Rbc_assert(rbc.Post_check_var_value(rbc.Get_ret(), "eq", (void*)EOF), "ERR33-C");
  rbc.Rbc_assert(rbc.Post_check_var_value(rbc.Get_ret(), "eq", (void*)EOF), "MSR_D_4_7");
  rbc.Rbc_rule_exception("ERR33-C", rbc.Is_called_by("putchar"));
  // rbc.Rbc_rule_exception("ERR33-C", rbc.Is_std_output(rbc.Get_arg(2)));
  return 0;
}

int putchar(int c)
{
  rbc.Rbc_assert(rbc.Post_check_var_value(rbc.Get_ret(), "eq", (void*)EOF), "ERR33-C");
  rbc.Rbc_assert(rbc.Post_check_var_value(rbc.Get_ret(), "eq", (void*)EOF), "MSR_D_4_7");
  rbc.Rbc_rule_exception("ERR33-C", 1);
  return 0;
}

int puts(const char *s)
{
  rbc.Model_decl(rbc.Set_parm_deref(rbc.Get_arg(1)));

  rbc.Rbc_assert(rbc.Post_check_var_value(rbc.Get_ret(), "eq", (void*)EOF), "ERR33-C");
  rbc.Rbc_assert(rbc.Post_check_var_value(rbc.Get_ret(), "eq", (void*)EOF), "MSR_D_4_7");
  rbc.Rbc_rule_exception("ERR33-C", 1);
  return 0;
}

int fgetpos(FILE *stream, fpos_t *pos)
{
  rbc.Model_decl(rbc.Set_func_errno_setting());

  rbc.Rbc_assert(rbc.Is_errno_cleared_before(), "MSR_22_8");
  rbc.Rbc_assert(rbc.Is_errno_checked_after(), "MSR_22_9");
  return 0;
}

int fsetpos(FILE *stream, const fpos_t *pos)
{
  rbc.Model_decl(rbc.Set_func_errno_setting());

  rbc.Rbc_assert(rbc.Is_errno_cleared_before(), "MSR_22_8");
  rbc.Rbc_assert(rbc.Is_errno_checked_after(), "MSR_22_9");
  return 0;
}

int remove(const char *pathname)
{
  rbc.Model_decl(rbc.Set_parm_deref(rbc.Get_arg(1)));

  rbc.Rbc_assert(rbc.Post_check_var_value(rbc.Get_ret(), "ne", 0), "ERR33-C");
  rbc.Rbc_assert(rbc.Post_check_var_value(rbc.Get_ret(), "ne", 0), "MSR_D_4_7");
  return 0;
}

int rename(const char *oldname, const char *newname)
{
  rbc.Model_decl(rbc.Set_parm_deref(rbc.Get_arg(1)));
  rbc.Model_decl(rbc.Set_parm_deref(rbc.Get_arg(2)));

  rbc.Rbc_assert(rbc.Post_check_var_value(rbc.Get_ret(), "ne", 0), "ERR33-C");
  rbc.Rbc_assert(rbc.Post_check_var_value(rbc.Get_ret(), "ne", 0), "MSR_D_4_7");
  return 0;
}

int printf(const char *format, ...)
{
  rbc.Model_decl(rbc.Set_parm_deref(rbc.Get_arg(1)));

  rbc.Rbc_assert(rbc.Not(rbc.Is_tag_set(rbc.Get_arg(1), "tainted")), "FIO30-C");
  // STR32-C check is done in VSA
  return 0;
}

int dprintf(int fd, const char *format, ...)
{
  rbc.Model_decl(rbc.Set_parm_deref(rbc.Get_arg(2)));

  rbc.Rbc_assert(rbc.Not(rbc.Is_tag_set(rbc.Get_arg(2), "tainted")), "FIO30-C");
  rbc.Rbc_assert(rbc.Post_check_var_value(rbc.Get_ret(), "lt", 0), "ERR33-C");
  rbc.Rbc_assert(rbc.Post_check_var_value(rbc.Get_ret(), "lt", 0), "MSR_D_4_7");
  return 0;
}

int sprintf(char *str, const char *format, ...)
{
  rbc.Model_decl(rbc.Set_parm_mod(rbc.Get_arg(1)));
  rbc.Model_decl(rbc.Set_parm_deref(rbc.Get_arg(2)));

  rbc.Rbc_assert(rbc.Not(rbc.Is_tag_set(rbc.Get_arg(2), "tainted")), "FIO30-C");
  // STR31-C check is done in VSA
  return 0;
}

int snprintf(char *s, size_t n, const char *format, ...)
{
  rbc.Model_decl(rbc.Set_parm_mod(rbc.Get_arg(1)));
  rbc.Model_decl(rbc.Set_parm_deref(rbc.Get_arg(3)));
  rbc.Model_decl(rbc.Set_implicit_assign(rbc.Get_arg(1), rbc.Get_arg(4)));
  rbc.Model_decl(rbc.Copy_tag(rbc.Get_arg(1), rbc.Get_arg(4)));

  rbc.Rbc_assert(rbc.Post_check_var_value(rbc.Get_ret(), "lt", 0), "ERR33-C");
  rbc.Rbc_assert(rbc.Post_check_var_value(rbc.Get_ret(), "lt", 0), "MSR_D_4_7");
  rbc.Rbc_assert(rbc.Not(rbc.Is_tag_set(rbc.Get_arg(3), "tainted")), "FIO30-C");
  rbc.Rbc_assert(rbc.Is_memory_big_enough(rbc.Get_arg(1), sizeof(char), rbc.Get_value(rbc.Get_arg(2))), "ARR38-C");
  return 0;
}

int fprintf(FILE *stream, const char *format, ...)
{
  rbc.Model_decl(rbc.Set_parm_deref(rbc.Get_arg(2)));
  rbc.Model_decl(rbc.Fsm_use("fio45c"));

  rbc.Rbc_assert(rbc.Not(rbc.Is_tag_set(rbc.Get_arg(2), "tainted")), "FIO30-C");
  rbc.Rbc_assert(rbc.Post_check_var_value(rbc.Get_ret(), "lt", 0), "ERR33-C");
  rbc.Rbc_assert(rbc.Post_check_var_value(rbc.Get_ret(), "lt", 0), "MSR_D_4_7");
  // rbc.Rbc_rule_exception("ERR33-C", rbc.Is_std_output(rbc.Get_arg(1)));
  return 0;
}

int vprintf(const char *format, va_list ap)
{
  rbc.Model_decl(rbc.Set_parm_deref(rbc.Get_arg(1)));

  rbc.Rbc_assert(rbc.Not(rbc.Is_tag_set(rbc.Get_arg(1), "tainted")), "FIO30-C");
  return 0;
}

int vfprintf(FILE *stream, const char *format, va_list ap)
{
  rbc.Model_decl(rbc.Set_parm_deref(rbc.Get_arg(2)));
  rbc.Model_decl(rbc.Fsm_use("fio45c"));

  rbc.Rbc_assert(rbc.Not(rbc.Is_tag_set(rbc.Get_arg(2), "tainted")), "FIO30-C");
  rbc.Rbc_rule_exception("FIO30-C", rbc.Is_called_by("vprintf"));
  return 0;
}

int vdprintf(int fd, const char *format, va_list ap)
{
  rbc.Model_decl(rbc.Set_parm_deref(rbc.Get_arg(2)));

  rbc.Rbc_assert(rbc.Not(rbc.Is_tag_set(rbc.Get_arg(2), "tainted")), "FIO30-C");
  rbc.Rbc_assert(rbc.Post_check_var_value(rbc.Get_ret(), "lt", 0), "ERR33-C");
  rbc.Rbc_assert(rbc.Post_check_var_value(rbc.Get_ret(), "lt", 0), "MSR_D_4_7");
  return 0;
}

int vsprintf(char *str, const char *format, va_list ap)
{
  rbc.Model_decl(rbc.Set_parm_mod(rbc.Get_arg(1)));
  rbc.Model_decl(rbc.Set_parm_deref(rbc.Get_arg(2)));

  rbc.Rbc_assert(rbc.Not(rbc.Is_tag_set(rbc.Get_arg(2), "tainted")), "FIO30-C");
  return 0;
}

int vsnprintf(char *str, size_t size, const char *format, va_list ap)
{
  rbc.Model_decl(rbc.Set_parm_mod(rbc.Get_arg(1)));
  rbc.Model_decl(rbc.Set_parm_deref(rbc.Get_arg(3)));

  rbc.Rbc_assert(rbc.Not(rbc.Is_tag_set(rbc.Get_arg(3), "tainted")), "FIO30-C");
  return 0;
}

int wprintf(const wchar_t *format, ...)
{
  rbc.Model_decl(rbc.Set_parm_deref(rbc.Get_arg(1)));

  rbc.Rbc_assert(rbc.Not(rbc.Is_tag_set(rbc.Get_arg(1), "tainted")), "FIO30-C");
  return 0;
}

int fwprintf(FILE *stream, const wchar_t *format, ...)
{
  rbc.Model_decl(rbc.Set_parm_deref(rbc.Get_arg(2)));

  rbc.Rbc_assert(rbc.Not(rbc.Is_tag_set(rbc.Get_arg(2), "tainted")), "FIO30-C");
  rbc.Rbc_assert(rbc.Post_check_var_value(rbc.Get_ret(), "lt", 0), "ERR33-C");
  rbc.Rbc_assert(rbc.Post_check_var_value(rbc.Get_ret(), "lt", 0), "MSR_D_4_7");
  return 0;
}

int swprintf(wchar_t *wcs, size_t maxlen, const wchar_t *format, ...)
{
  rbc.Model_decl(rbc.Set_parm_mod(rbc.Get_arg(1)));

  rbc.Model_decl(rbc.Set_parm_deref(rbc.Get_arg(3)));
  rbc.Rbc_assert(rbc.Not(rbc.Is_tag_set(rbc.Get_arg(3), "tainted")), "FIO30-C");
  return 0;
}

int vwprintf(const wchar_t *format, va_list args)
{
  rbc.Model_decl(rbc.Set_parm_deref(rbc.Get_arg(1)));

  rbc.Rbc_assert(rbc.Not(rbc.Is_tag_set(rbc.Get_arg(1), "tainted")), "FIO30-C");
  return 0;
}

int vfwprintf(FILE *stream, const wchar_t *format, va_list args)
{
  rbc.Model_decl(rbc.Set_parm_deref(rbc.Get_arg(2)));

  rbc.Rbc_assert(rbc.Not(rbc.Is_tag_set(rbc.Get_arg(2), "tainted")), "FIO30-C");
  rbc.Rbc_assert(rbc.Post_check_var_value(rbc.Get_ret(), "lt", 0), "ERR33-C");
  rbc.Rbc_assert(rbc.Post_check_var_value(rbc.Get_ret(), "lt", 0), "MSR_D_4_7");
  return 0;
}

int vswprintf(wchar_t *wcs, size_t maxlen, const wchar_t *format, va_list args)
{
  rbc.Model_decl(rbc.Set_parm_mod(rbc.Get_arg(1)));

  rbc.Model_decl(rbc.Set_parm_deref(rbc.Get_arg(3)));
  rbc.Rbc_assert(rbc.Not(rbc.Is_tag_set(rbc.Get_arg(3), "tainted")), "FIO30-C");
  return 0;
}

int scanf(const char *format, ...)
{
  rbc.Model_decl(rbc.Set_parm_deref(rbc.Get_arg(1)));

  rbc.Rbc_assert(rbc.Post_check_var_value(rbc.Get_ret(), "eq", (void*)EOF), "ERR33-C");
  rbc.Rbc_assert(rbc.Post_check_var_value(rbc.Get_ret(), "eq", (void*)EOF), "MSR_D_4_7");
  return 0;
}

int __isoc99_scanf(const char *format, ...)
{
  rbc.Rbc_same_as_func("scanf");
  return 0;
}

int fscanf(FILE *stream, const char *format, ...)
{
  rbc.Model_decl(rbc.Set_parm_deref(rbc.Get_arg(2)));

  rbc.Rbc_assert(rbc.Post_check_var_value(rbc.Get_ret(), "eq", (void*)EOF), "ERR33-C");
  rbc.Rbc_assert(rbc.Post_check_var_value(rbc.Get_ret(), "eq", (void*)EOF), "MSR_D_4_7");
  rbc.Model_decl(rbc.Set_tag(rbc.Get_arg(3), (char *)"tainted"));
  // TODO: STR31-C
  return 0;
}

int __isoc99_fscanf(FILE *stream, const char *format, ...)
{
  rbc.Rbc_same_as_func("fscanf");
  return 0;
}

int sscanf(const char *str, const char *format, ...)
{
  rbc.Model_decl(rbc.Set_parm_deref(rbc.Get_arg(1)));
  rbc.Model_decl(rbc.Set_parm_deref(rbc.Get_arg(2)));

  rbc.Rbc_assert(rbc.Post_check_var_value(rbc.Get_ret(), "eq", (void*)EOF), "ERR33-C");
  rbc.Rbc_assert(rbc.Post_check_var_value(rbc.Get_ret(), "eq", (void*)EOF), "MSR_D_4_7");
  return 0;
}

int __isoc99_sscanf(const char *str, const char *format, ...)
{
  rbc.Rbc_same_as_func("sscanf");
  return 0;
}

void syslog(int priority, const char *format, ...)
{
  rbc.Model_decl(rbc.Set_parm_deref(rbc.Get_arg(2)));

  rbc.Rbc_assert(rbc.Not(rbc.Is_tag_set(rbc.Get_arg(2), "tainted")), "FIO30-C");
  return;
}

int setuid(int i)
{
  rbc.Model_decl(rbc.Fsm_use("pos37c"));
  return 0;
}

int seteuid(int i)
{
  rbc.Model_decl(rbc.Fsm_use("pos37c"));
  return 0;
}

int pos37_c_fsm(void)
{
  rbc.Model_decl(rbc.Fsm_build_begin("pos37c"));
  rbc.Model_decl(rbc.Fsm_new_start_state("start_up"));
  rbc.Model_decl(rbc.Fsm_new_final_state("perm_drop_succ"));
  // "start_up" :       (0, U, 0)
  // "temp_drop_succ" : (U, U, 0)
  // "perm_drop_succ" : (U, U, U)
  // "start_up" : temp drop actions:
  rbc.Model_decl(rbc.Fsm_add_transition("start_up", "seteuid", NULL,
                                        rbc.Parm_is_def_by_func(rbc.Get_arg(1), "getuid"),
                                        "temp_drop", 0, 77));
  rbc.Model_decl(rbc.Fsm_add_transition("temp_drop", "if", NULL,
                                        rbc.Is_func_exec_successful("seteuid", "eq", 0),
                                        "temp_drop_succ", 0, 148));
  rbc.Model_decl(rbc.Fsm_add_transition("temp_drop", "if", NULL,
                                        !rbc.Is_func_exec_successful("seteuid", "eq", 0),
                                        "start_up", 0, 149));
  // "temp_drop_succ" : restore actions:
  rbc.Model_decl(rbc.Fsm_add_transition("temp_drop_succ", "seteuid", NULL,
                                        rbc.Get_value(rbc.Get_arg(1)) == 0,
                                        "restore", 0, 78));
  rbc.Model_decl(rbc.Fsm_add_transition("restore", "if", NULL,
                                        rbc.Is_func_exec_successful("seteuid", "eq", 0),
                                        "start_up", 0, 148));
  rbc.Model_decl(rbc.Fsm_add_transition("restore", "if", NULL,
                                        !rbc.Is_func_exec_successful("seteuid", "eq", 0),
                                        "temp_drop_succ", 0, 149));
  // "temp_drop_succ" : perm drop actions:
  rbc.Model_decl(rbc.Fsm_add_transition("temp_drop_succ", "setuid", NULL,
                                        rbc.Parm_is_def_by_func(rbc.Get_arg(1), "getuid"),
                                        "temp_drop_succ", 0, 79));
  // "start_up" : perm drop actions:
  rbc.Model_decl(rbc.Fsm_add_transition("start_up", "setuid", NULL,
                                        rbc.Parm_is_def_by_func(rbc.Get_arg(1), "getuid"),
                                        "perm_drop", 0, 79));
  rbc.Model_decl(rbc.Fsm_add_transition("perm_drop", "if", NULL,
                                        rbc.Is_func_exec_successful("setuid", "eq", 0),
                                        "perm_drop_succ", 0, 148));
  rbc.Model_decl(rbc.Fsm_add_transition("perm_drop", "if", NULL,
                                        !rbc.Is_func_exec_successful("setuid", "eq", 0),
                                        "start_up", 0, 149));
  // set error transition
  rbc.Model_decl(rbc.Fsm_set_default_action("temp_drop_succ", "POS37-C", 70));
  rbc.Model_decl(rbc.Fsm_build_end("pos37c"));
}

int fstat(int fd, struct stat *statbuf)
{
  rbc.Model_decl(rbc.Set_parm_mod(rbc.Get_arg(2)));
  rbc.Model_decl(rbc.Fsm_use("pos35c"));
  return 0;
}

int open(const char *pathname, int flags, ...)
{
  rbc.Model_decl(rbc.Set_parm_deref(rbc.Get_arg(1)));
  rbc.Model_decl(rbc.Fsm_use("pos35c"));
  return 0;
}

int close(int fd)
{
  rbc.Model_decl(rbc.Fsm_use("pos35c"));
  return 0;
}

int access(const char *pathname, int mode)
{
  rbc.Model_decl(rbc.Set_parm_deref(rbc.Get_arg(1)));
  return 0;
}

char *getcwd(char *buf, size_t size)
{
  rbc.Model_decl(rbc.Set_parm_mod(rbc.Get_arg(1)));
  return NULL;
}

char *getwd(char *buf)
{
  rbc.Rbc_same_as_func("getcwd");
  return NULL;
}

int stat(const char *pathname, struct stat *statbuf)
{
  rbc.Model_decl(rbc.Set_parm_deref(rbc.Get_arg(1)));
  rbc.Model_decl(rbc.Set_parm_mod(rbc.Get_arg(2)));
  return 0;
}

int lstat(const char *pathname, struct stat *statbuf)
{
  rbc.Rbc_same_as_func("stat");
  return 0;
}

ssize_t pread(int fd, void *buf, size_t count, off_t offset)
{
  rbc.Model_decl(rbc.Set_parm_mod(rbc.Get_arg(2)));
  return 0;
}

ssize_t pread64(int fd, void *buf, size_t count, off_t offset)
{
  rbc.Rbc_same_as_func("pread");
  return 0;
}

ssize_t write(int fd, const void *buf, size_t count)
{
  rbc.Model_decl(rbc.Set_parm_deref(rbc.Get_arg(2)));
  rbc.Model_decl(rbc.Fsm_use("pos35c"));
  return 0;
}

int chmod(const char *pathname, mode_t mode)
{
  rbc.Model_decl(rbc.Set_parm_deref(rbc.Get_arg(1)));
  return 0;
}

int unlink(const char *fname)
{
  rbc.Model_decl(rbc.Set_parm_deref(rbc.Get_arg(1)));
  return 0;
}

int unlinkat(int dirfd, const char *pathname, int flags)
{
  rbc.Model_decl(rbc.Set_parm_deref(rbc.Get_arg(2)));
  return 0;
}

int mkdir(const char *pathname, mode_t mode)
{
  rbc.Model_decl(rbc.Set_parm_deref(rbc.Get_arg(1)));
  return 0;
}

int mkdirat(int dirfd, const char *pathname, mode_t mode)
{
  rbc.Model_decl(rbc.Set_parm_deref(rbc.Get_arg(2)));
  return 0;
}

int rmdir(const char *pathname)
{
  rbc.Model_decl(rbc.Set_parm_deref(rbc.Get_arg(1)));
  return 0;
}

int chdir(const char *path)
{
  rbc.Model_decl(rbc.Set_parm_deref(rbc.Get_arg(1)));
  return 0;
}

int chown(const char *pathname, uid_t owner, gid_t group)
{
  rbc.Model_decl(rbc.Set_parm_deref(rbc.Get_arg(1)));
  return 0;
}

int lchown(const char *pathname, uid_t owner, gid_t group)
{
  rbc.Model_decl(rbc.Set_parm_deref(rbc.Get_arg(1)));
  return 0;
}

size_t confstr(int name, char *buf, size_t len)
{
  rbc.Model_decl(rbc.Set_parm_mod(rbc.Get_arg(2)));
  return 0;
}

int symlink(const char *target, const char *linkpath)
{
  rbc.Model_decl(rbc.Set_parm_deref(rbc.Get_arg(1)));
  rbc.Model_decl(rbc.Set_parm_deref(rbc.Get_arg(2)));
  return 0;
}

int symlinkat(const char *target, int newdirfd, const char *linkpath)
{
  rbc.Model_decl(rbc.Set_parm_deref(rbc.Get_arg(1)));
  rbc.Model_decl(rbc.Set_parm_deref(rbc.Get_arg(3)));
  return 0;
}

DIR *opendir(const char *name)
{
  rbc.Model_decl(rbc.Set_parm_deref(rbc.Get_arg(1)));
  return NULL;
}

struct dirent *readdir(DIR *dirp)
{
  rbc.Model_decl(rbc.Set_parm_deref(rbc.Get_arg(1)));
  return NULL;
}

int closedir(DIR *dirp)
{
  rbc.Model_decl(rbc.Set_parm_deref(rbc.Get_arg(1)));
  rbc.Model_decl(rbc.Set_parm_mod(rbc.Get_arg(1)));
  return 0;
}

ssize_t read(int fd, void *buf, size_t count)
{
  rbc.Model_decl(rbc.Set_parm_mod(rbc.Get_arg(2)));
  rbc.Model_decl(rbc.Fsm_use("pos35c"));
  rbc.Model_decl(rbc.Set_parm_mod(rbc.Get_arg(2)));
  return 0;
}

int pos35_c_fsm(void)
{
  rbc.Model_decl(rbc.Fsm_build_begin("pos35c"));
  rbc.Model_decl(rbc.Fsm_new_start_state("start"));
  rbc.Model_decl(rbc.Fsm_new_final_state("finish"));

  // write transitions
  // #1-1 "start" => "openw" : open(*, O_WRONLY)
  rbc.Model_decl(rbc.Fsm_add_transition("start", "open", rbc.Get_arg(1),
                                        (rbc.Get_value(rbc.Get_arg(2)) != O_RDONLY) &
                                        ((rbc.Get_value(rbc.Get_arg(2)) & O_EXCL) == 0),
                                        "openw", NULL, 74));
  // #3-1 "openw" => "openw_s" : read(*, _, _)
  rbc.Model_decl(rbc.Fsm_add_transition("openw", "read", rbc.Get_arg(1), 1,
                                        "openw_s", "ERR33-C MSR_D_4_7 POS35-C", 67));
  // #3-2 "openw" => "openw_s" : write(*, _, _)
  rbc.Model_decl(rbc.Fsm_add_transition("openw", "write", rbc.Get_arg(1), 1,
                                        "openw_s", "ERR33-C MSR_D_4_7 POS35-C", 68));
  // #3-3 "openw" => "finish" : close(*)
  rbc.Model_decl(rbc.Fsm_add_transition("openw", "close", rbc.Get_arg(1), 1,
                                        "finish", "ERR33-C MSR_D_4_7", 69));
  // #3-4 "openw" => "finish" : default
  rbc.Model_decl(rbc.Fsm_set_default_action("openw", "FIO42-C MSR_22_1", 70));
  // #3-5 "openw" => "openw_c" : check if (fd != -1)
  rbc.Model_decl(rbc.Fsm_add_transition("openw", "if:test", NULL,
                                        rbc.Is_return_checked_properly("open", "ne", -1),
                                        "openw_c", NULL, 66));
  // #3-6 "openw" => "openw" : not check if (fd != -1), ignored due to no state changes
  // rbc.Model_decl(rbc.Fsm_add_transition("openw", "if:test", NULL,
  //                                       !rbc.Is_return_checked_properly("open", "ne", -1),
  //                                       "openw", NULL, 149));
  // #3-7 "openw" => "openw_f" : fstat(*, _)
  rbc.Model_decl(rbc.Fsm_add_transition("openw", "fstat", rbc.Get_arg(1), 1,
                                        "openw_f", NULL, 150));
  // #4-1 "openw_c" => "openw_s" : if (fd != -1) True path
  rbc.Model_decl(rbc.Fsm_add_transition("openw_c", "if", NULL,
                                        rbc.Is_func_exec_successful("open", "ne", -1),
                                        "openw_s", NULL, 66));
  // #4-2 "openw_c" => "open_f" : if (fd != -1) False path
  rbc.Model_decl(rbc.Fsm_add_transition("openw_c", "if", NULL,
                                        !rbc.Is_func_exec_successful("open", "ne", -1),
                                        "open_f", NULL, 149));
  // #5-1 "openw_s" => "openw_s" : read(*, _, _)
  rbc.Model_decl(rbc.Fsm_add_transition("openw_s", "read", rbc.Get_arg(1), 1,
                                        "openw_s", "POS35-C", 67));
  // #5-2 "openw_s" => "openw_s" : write(*, _, _)
  rbc.Model_decl(rbc.Fsm_add_transition("openw_s", "write", rbc.Get_arg(1), 1,
                                        "openw_s", "POS35-C", 68));
  // #5-3 "openw_s" => "finish" : close(*)
  rbc.Model_decl(rbc.Fsm_add_transition("openw_s", "close", rbc.Get_arg(1), 1,
                                        "finish", NULL, 69));
  // #5-4 "openw_s" => "finish" : default
  rbc.Model_decl(rbc.Fsm_set_default_action("openw_s", "FIO42-C MSR_22_1", 70));
  // #5-5 "openw_s" => "openw_sc" : check if (fd != -1)
  rbc.Model_decl(rbc.Fsm_add_transition("openw_s", "if:test", NULL,
                                        rbc.Is_return_checked_properly("open", "ne", -1),
                                        "openw_sc", NULL, 66));
  // #5-6 "openw_s" => "openw_s" : not check if (fd != -1), ignored due to no state changes
  // rbc.Model_decl(rbc.Fsm_add_transition("openw_s", "if:test", NULL,
  //                                       !rbc.Is_return_checked_properly("open", "ne", -1),
  //                                       "openw_s", NULL, 149));
  // #5-7 "openw_s" => "openw_sf" : fstat(*, _)
  rbc.Model_decl(rbc.Fsm_add_transition("openw_s", "fstat", rbc.Get_arg(1), 1,
                                        "openw_sf", NULL, 150));
  // #6-1 "open_f" => "finish" : read(*, _, _)
  rbc.Model_decl(rbc.Fsm_add_transition("open_f", "read", rbc.Get_arg(1), 1,
                                        "finish", "ERR33-C MSR_D_4_7", 67));
  // #6-2 "open_f" => "finish" : write(*, _, _)
  rbc.Model_decl(rbc.Fsm_add_transition("open_f", "write", rbc.Get_arg(1), 1,
                                        "finish", "ERR33-C MSR_D_4_7", 68));
  // #6-3 "open_f" => "finish" : close(*)
  rbc.Model_decl(rbc.Fsm_add_transition("open_f", "close", rbc.Get_arg(1), 1,
                                        "finish", "ERR33-C MSR_D_4_7", 69));
  // #6-4 "open_f" => "open_fc" : check if (fd != -1)
  rbc.Model_decl(rbc.Fsm_add_transition("open_f", "if:test", NULL,
                                        rbc.Is_return_checked_properly("open", "ne", -1),
                                        "open_fc", NULL, 66));
  // #6-5 "open_f" => "open_f" : not check if (fd != -1), ignored due to no state changes
  // rbc.Model_decl(rbc.Fsm_add_transition("open_f", "if:test", NULL,
  //                                       !rbc.Is_return_checked_properly("open", "ne", -1),
  //                                       "open_f", NULL, 149));
  // #6-6 "open_f" => "finish" : default, ignored as good finish
  // rbc.Model_decl(rbc.Fsm_set_default_action("open_f", NULL, 70));
  // #6-7 "open_f" => "open_ff" : fstat(*, _)
  rbc.Model_decl(rbc.Fsm_add_transition("open_f", "fstat", rbc.Get_arg(1), 1,
                                        "open_ff", NULL, 150));
  // #7-1 "openw_sc" => "openw_s" : if (fd != -1) True path
  rbc.Model_decl(rbc.Fsm_add_transition("openw_sc", "if", NULL,
                                        rbc.Is_func_exec_successful("open", "ne", -1),
                                        "openw_s", NULL, 66));
  // #7-2 "openw_sc" => "finish" : if (fd != -1) False path
  rbc.Model_decl(rbc.Fsm_add_transition("openw_sc", "if", NULL,
                                        !rbc.Is_func_exec_successful("open", "ne", -1),
                                        "finish", "DDC", 149));
  // #8-1 "open_fc" => "finish" : if (fd != -1) True path
  rbc.Model_decl(rbc.Fsm_add_transition("open_fc", "if", NULL,
                                        rbc.Is_func_exec_successful("open", "ne", -1),
                                        "finish", "DDC", 66));
  // #8-2 "open_fc" => "open_f" : if (fd != -1) False path
  rbc.Model_decl(rbc.Fsm_add_transition("open_fc", "if", NULL,
                                        !rbc.Is_func_exec_successful("open", "ne", -1),
                                        "open_f", NULL, 149));
  // #9-1 "openw_f" => "openw_fc" : check if (fstat() != -1)
  rbc.Model_decl(rbc.Fsm_add_transition("openw_f", "if:test", NULL,
                                        rbc.Is_return_checked_properly("fstat", "ne", -1),
                                        "openw_fc", NULL, 66));
  // #9-2 "openw_f" => "openw" : not check if (fstat() != -1)
  rbc.Model_decl(rbc.Fsm_add_transition("openw_f", "if:test", NULL,
                                        !rbc.Is_return_checked_properly("fstat", "ne", -1),
                                        "openw", NULL, 149));
  // #10-1 "openw_sf" => "openw_sfc" : check if (fstat() != -1)
  rbc.Model_decl(rbc.Fsm_add_transition("openw_sf", "if:test", NULL,
                                        rbc.Is_return_checked_properly("fstat", "ne", -1),
                                        "openw_sfc", NULL, 66));
  // #10-2 "openw_sf" => "openw_s" : not check if (fstat() != -1)
  rbc.Model_decl(rbc.Fsm_add_transition("openw_sf", "if:test", NULL,
                                        !rbc.Is_return_checked_properly("fstat", "ne", -1),
                                        "openw_s", NULL, 149));
  // #11-1 "open_ff" => "open_ffc" : check if (fstat() != -1)
  rbc.Model_decl(rbc.Fsm_add_transition("open_ff", "if:test", NULL,
                                        rbc.Is_return_checked_properly("fstat", "ne", -1),
                                        "open_ffc", NULL, 66));
  // #11-2 "open_ff" => "open_f" : not check if (fstat() != -1)
  rbc.Model_decl(rbc.Fsm_add_transition("open_ff", "if:test", NULL,
                                        !rbc.Is_return_checked_properly("fstat", "ne", -1),
                                        "open_f", NULL, 149));
  // #12-1 "openw_fc" => "openx_s" : if (fstat() != -1) True path
  rbc.Model_decl(rbc.Fsm_add_transition("openw_fc", "if", NULL,
                                        rbc.Is_func_exec_successful("fstat", "ne", -1),
                                        "openx_s", NULL, 66));
  // #12-2 "openw_fc" => "open_f" : if (fstat() != -1) False path
  rbc.Model_decl(rbc.Fsm_add_transition("openw_fc", "if", NULL,
                                        !rbc.Is_func_exec_successful("fstat", "ne", -1),
                                        "open_f", NULL, 149));
  // #13-1 "openw_sfc" => "openx_s" : if (fstat() != -1) True path
  rbc.Model_decl(rbc.Fsm_add_transition("openw_sfc", "if", NULL,
                                        rbc.Is_func_exec_successful("fstat", "ne", -1),
                                        "openx_s", NULL, 66));
  // #13-2 "openw_sfc" => "open_f" : if (fstat() != -1) False path
  rbc.Model_decl(rbc.Fsm_add_transition("openw_sfc", "if", NULL,
                                        !rbc.Is_func_exec_successful("fstat", "ne", -1),
                                        "open_f", NULL, 149));
  // #14-1 "open_ffc" => "finish" : if (fstat() != -1) True path
  rbc.Model_decl(rbc.Fsm_add_transition("open_ffc", "if", NULL,
                                        rbc.Is_func_exec_successful("fstat", "ne", -1),
                                        "finish", "DDC", 66));
  // #14-1 "open_ffc" => "open_f" : if (fstat() != -1) False path
  rbc.Model_decl(rbc.Fsm_add_transition("open_ffc", "if", NULL,
                                        !rbc.Is_func_exec_successful("fstat", "ne", -1),
                                        "open_f", NULL, 149));

  // exclusive write transitions
  // #1-2 "start" => "openx" : open(*, O_WRONLY | O_EXCL)
  rbc.Model_decl(rbc.Fsm_add_transition("start", "open", rbc.Get_arg(1),
                                        (rbc.Get_value(rbc.Get_arg(2)) != 0) &
                                        ((rbc.Get_value(rbc.Get_arg(2)) & O_EXCL) != 0),
                                        "openx", NULL, 76));
  // #16-1 "openx" => "openx_s" : read(*, _, _)
  rbc.Model_decl(rbc.Fsm_add_transition("openx", "read", rbc.Get_arg(1), 1,
                                        "openx_s", "ERR33-C MSR_D_4_7", 67));
  // #16-2 "openx" => "openx_s" : write(*, _, _)
  rbc.Model_decl(rbc.Fsm_add_transition("openx", "write", rbc.Get_arg(1), 1,
                                        "openx_s", "ERR33-C MSR_D_4_7", 68));
  // #16-3 "openx" => "finish" : close(*)
  rbc.Model_decl(rbc.Fsm_add_transition("openx", "close", rbc.Get_arg(1), 1,
                                        "finish", "ERR33-C MSR_D_4_7", 69));
  // #16-4 "openx" => "finish" : default
  rbc.Model_decl(rbc.Fsm_set_default_action("openx", "FIO42-C MSR_22_1", 70));
  // #16-5 "openx" => "openx_c" : check if (fd != -1)
  rbc.Model_decl(rbc.Fsm_add_transition("openx", "if:test", NULL,
                                        rbc.Is_return_checked_properly("open", "ne", -1),
                                        "openx_c", NULL, 66));
  // #16-6 "openx" => "openx" : not check if (fd != -1), ignored due to no state changes
  // rbc.Model_decl(rbc.Fsm_add_transition("openx", "if:test", NULL,
  //                                       !rbc.Is_return_checked_properly("open", "ne", -1),
  //                                       "openx", NULL, 149));
  // #16-7 "openx" => "openx_f" : fstat(*, _)
  rbc.Model_decl(rbc.Fsm_add_transition("openx", "fstat", rbc.Get_arg(1), 1,
                                        "openx_f", NULL, 150));
  // #17-1 "openx_c" => "openx_s" : if (fd != -1) True path
  rbc.Model_decl(rbc.Fsm_add_transition("openx_c", "if", NULL,
                                        rbc.Is_func_exec_successful("open", "ne", -1),
                                        "openx_s", NULL, 66));
  // #17-2 "openx_c" => "open_f" : if (fd != -1) False path
  rbc.Model_decl(rbc.Fsm_add_transition("openx_c", "if", NULL,
                                        !rbc.Is_func_exec_successful("open", "ne", -1),
                                        "open_f", NULL, 149));
  // #15-1 "openx_s" => "openx_s" : read(*, _, _), ignored due to no state changes
  // rbc.Model_decl(rbc.Fsm_add_transition("openx_s", "read", rbc.Get_arg(1), 1,
  //                                       "openx_s", NULL, 67));
  // #15-2 "openx_s" => "openx_s" : write(*, _, _), ignored due to no state changes
  // rbc.Model_decl(rbc.Fsm_add_transition("openx_s", "write", rbc.Get_arg(1), 1,
  //                                       "openx_s", NULL, 68));
  // #15-3 "openx_s" => "finish" : close(*)
  rbc.Model_decl(rbc.Fsm_add_transition("openx_s", "close", rbc.Get_arg(1), 1,
                                        "finish", NULL, 69));
  // #15-4 "openx_s" => "finish" : default
  rbc.Model_decl(rbc.Fsm_set_default_action("openx_s", "FIO42-C MSR_22_1", 70));
  // #15-5 "openx_s" => "openx_sc" : check if (fd != -1)
  rbc.Model_decl(rbc.Fsm_add_transition("openx_s", "if:test", NULL,
                                        rbc.Is_return_checked_properly("open", "ne", -1),
                                        "openx_sc", NULL, 66));
  // #15-6 "openx_s" => "openx_s" : not check if (fd != -1), ignored due to no state changes
  // rbc.Model_decl(rbc.Fsm_add_transition("openx_s", "if:test", NULL,
  //                                       !rbc.Is_return_checked_properly("open", "ne", -1),
  //                                       "openx_s", NULL, 149));
  // #15-7 "openx_s" => "openx_sf" : fstat(*, _)
  rbc.Model_decl(rbc.Fsm_add_transition("openx_s", "fstat", rbc.Get_arg(1), 1,
                                        "openx_sf", NULL, 150));
  // #18-1 "openx_sc" => "openx_s" : if (fd != -1) True path
  rbc.Model_decl(rbc.Fsm_add_transition("openx_sc", "if", NULL,
                                        rbc.Is_func_exec_successful("open", "ne", -1),
                                        "openx_s", NULL, 66));
  // #18-2 "openx_sc" => "finish" : if (fd != 1) False path
  rbc.Model_decl(rbc.Fsm_add_transition("openx_sc", "if", NULL,
                                        !rbc.Is_func_exec_successful("open", "ne", -1),
                                        "finish", "DDC", 149));
  // #19-1 "openx_f" => "openw_fc" : check if (fstat() != -1)
  rbc.Model_decl(rbc.Fsm_add_transition("openx_f", "if:test", NULL,
                                        rbc.Is_return_checked_properly("fstat", "ne", -1),
                                        "openw_fc", NULL, 66));
  // #19-2 "openx_f" => "openx" : not check if (fstat() != -1)
  rbc.Model_decl(rbc.Fsm_add_transition("openx_f", "if:test", NULL,
                                        !rbc.Is_return_checked_properly("fstat", "ne", -1),
                                        "openx", NULL, 149));
  // #20-1 "openx_sf" => "openx_sfc" : check if (fstat() != -1)
  rbc.Model_decl(rbc.Fsm_add_transition("openx_sf", "if:test", NULL,
                                        rbc.Is_return_checked_properly("fstat", "ne", -1),
                                        "openx_sfc", NULL, 66));
  // #20-2 "openx_sf" => "openx_s" : not check if (fstat() != -1)
  rbc.Model_decl(rbc.Fsm_add_transition("openx_sf", "if:test", NULL,
                                        !rbc.Is_return_checked_properly("fstat", "ne", -1),
                                        "openx_s", NULL, 149));
  // #21-1 "openx_sfc" => "openx_s" : if (fstat() != -1) True path
  rbc.Model_decl(rbc.Fsm_add_transition("openx_sfc", "if", NULL,
                                        rbc.Is_func_exec_successful("fstat", "ne", -1),
                                        "openx_s", NULL, 66));
  // #21-2 "openx_sfc" => "finish" : if (fstat() != -1) False path
  rbc.Model_decl(rbc.Fsm_add_transition("openx_sfc", "if", NULL,
                                        !rbc.Is_func_exec_successful("fstat", "ne", -1),
                                        "finish", "DDC", 149));

  // read transitions
  // #1-3 "start" => "openr" : open(*, O_RDONLY)
  rbc.Model_decl(rbc.Fsm_add_transition("start", "open", rbc.Get_arg(1),
                                        rbc.Get_value(rbc.Get_arg(2)) == O_RDONLY,
                                        "openr", NULL, 73));
  // #22-1 "openr" => "openr_s" : read(*, _, _)
  rbc.Model_decl(rbc.Fsm_add_transition("openr", "read", rbc.Get_arg(1), 1,
                                        "openr_s", "ERR33-C MSR_D_4_7", 67));
  // #22-2 "openr" => "openr_s" : write(*, _, _)
  rbc.Model_decl(rbc.Fsm_add_transition("openr", "write", rbc.Get_arg(1), 1,
                                        "openr_s", "ERR33-C MSR_D_4_7 WRF MSR_22_4", 68));
  // #22-3 "openr" => "finish" : close(*)
  rbc.Model_decl(rbc.Fsm_add_transition("openr", "close", rbc.Get_arg(1), 1,
                                        "finish", "ERR33-C MSR_D_4_7", 69));
  // #22-4 "openr" => "finish" : default
  rbc.Model_decl(rbc.Fsm_set_default_action("openr", "FIO42-C MSR_22_1", 70));
  // #22-5 "openr" => "openr_c" : check if (fd != -1)
  rbc.Model_decl(rbc.Fsm_add_transition("openr", "if:test", NULL,
                                        rbc.Is_return_checked_properly("open", "ne", -1),
                                        "openr_c", NULL, 66));
  // #22-6 "openr" => "openr" : not check if (fd != -1), ignored due to no state changes
  // rbc.Model_decl(rbc.Fsm_add_transition("openr", "if:test", NULL,
  //                                       !rbc.Is_return_checked_properly("open", "ne", -1),
  //                                       "openr", NULL, 149));
  // #22-7 "openr" => "openr_f" : fstat(*, _)
  rbc.Model_decl(rbc.Fsm_add_transition("openr", "fstat", rbc.Get_arg(1), 1,
                                        "openr_f", NULL, 150));
  // #23-1 "openr_c" => "openr_s" : if (fd != -1) True path
  rbc.Model_decl(rbc.Fsm_add_transition("openr_c", "if", NULL,
                                        rbc.Is_func_exec_successful("open", "ne", -1),
                                        "openr_s", NULL, 66));
  // #23-2 "openr_c" => "open_f" : if (fd != -1) False path
  rbc.Model_decl(rbc.Fsm_add_transition("openr_c", "if", NULL,
                                        !rbc.Is_func_exec_successful("open", "ne", -1),
                                        "open_f", NULL, 149));
  // #24-1 "openr_s" => "openr_s" : read(*, _, _), ignored due to no state changes
  // rbc.Model_decl(rbc.Fsm_add_transition("openr_s", "read", rbc.Get_arg(1), 1,
  //                                       "openr_s", NULL, 67));
  // #24-2 "openr_s" => "openr_s" : write(*, _, _)
  rbc.Model_decl(rbc.Fsm_add_transition("openr_s", "write", rbc.Get_arg(1), 1,
                                        "openr_s", "WRF MSR_22_4", 68));
  // #24-3 "openr_s" => "finish" : close(*)
  rbc.Model_decl(rbc.Fsm_add_transition("openr_s", "close", rbc.Get_arg(1), 1,
                                        "finish", NULL, 69));
  // #24-4 "openr_s" => "finish" : default
  rbc.Model_decl(rbc.Fsm_set_default_action("openr_s", "FIO42-C MSR_22_1", 70));
  // #24-5 "openr_s" => "openr_sc" : check if (fd != -1)
  rbc.Model_decl(rbc.Fsm_add_transition("openr_s", "if:test", NULL,
                                        rbc.Is_return_checked_properly("open", "ne", -1),
                                        "openr_sc", NULL, 66));
  // #24-6 "openr_s" => "openr_s" : not check if (fd != -1), ignored due to no state changes
  // rbc.Model_decl(rbc.Fsm_add_transition("openr_s", "if:test", NULL,
  //                                       !rbc.Is_return_checked_properly("open", "ne", -1),
  //                                       "openr_s", NULL, 149));
  // #24-7 "openr_s" => "openr_sf" : fstat(*, _)
  rbc.Model_decl(rbc.Fsm_add_transition("openr_s", "fstat", rbc.Get_arg(1), 1,
                                        "openr_sf", NULL, 150));
  // #25-1 "openr_sc" => "openr_s" : if (fd != -1) True path
  rbc.Model_decl(rbc.Fsm_add_transition("openr_sc", "if", NULL,
                                        rbc.Is_func_exec_successful("open", "ne", -1),
                                        "openr_s", NULL, 66));
  // #25-2 "openr_sc" => "finish" : if (fd != -1) False path
  rbc.Model_decl(rbc.Fsm_add_transition("openr_sc", "if", NULL,
                                        !rbc.Is_func_exec_successful("open", "ne", -1),
                                        "finish", "DDC", 149));
  // #26-1 "openr_f" => "openr_fc" : check if (fstat() != -1)
  rbc.Model_decl(rbc.Fsm_add_transition("openr_f", "if:test", NULL,
                                        rbc.Is_return_checked_properly("fstat", "ne", -1),
                                        "openr_fc", NULL, 66));
  // #26-2 "openr_f" => "openr" : not check if (fstat() != -1)
  rbc.Model_decl(rbc.Fsm_add_transition("openr_f", "if:test", NULL,
                                        !rbc.Is_return_checked_properly("fstat", "ne", -1),
                                        "openr", NULL, 149));
  // #27-1 "openr_sf" => "openr_sfc" : check if (fstat() != -1)
  rbc.Model_decl(rbc.Fsm_add_transition("openr_sf", "if:test", NULL,
                                        rbc.Is_return_checked_properly("fstat", "ne", -1),
                                        "openr_sfc", NULL, 66));
  // #27-2 "openr_sf" => "openr_s" : not check if (fstat() != -1)
  rbc.Model_decl(rbc.Fsm_add_transition("openr_sf", "if:test", NULL,
                                        !rbc.Is_return_checked_properly("fstat", "ne", -1),
                                        "openr_s", NULL, 149));
  // #28-1 "openr_sfc" => "openr_s" : if (fstat() != -1) True path
  rbc.Model_decl(rbc.Fsm_add_transition("openr_sfc", "if", NULL,
                                        rbc.Is_func_exec_successful("fstat", "ne", -1),
                                        "openr_s", NULL, 66));
  // #28-2 "openr_sfc" => "finish" : if (fstat() != -1) False path
  rbc.Model_decl(rbc.Fsm_add_transition("openr_sfc", "if", NULL,
                                        !rbc.Is_func_exec_successful("fstat", "ne", -1),
                                        "finish", "DDC", 149));
  // #29-1 "openr_fc" => "openr_s" : if (fstat() != -1) True path
  rbc.Model_decl(rbc.Fsm_add_transition("openr_fc", "if", NULL,
                                        rbc.Is_func_exec_successful("fstat", "ne", -1),
                                        "openr_s", NULL, 66));
  // #29-2 "openr_fc" => "open_f" : if (fstat() != -1) False path
  rbc.Model_decl(rbc.Fsm_add_transition("openr_fc", "if", NULL,
                                        !rbc.Is_func_exec_successful("fstat", "ne", -1),
                                        "open_f", NULL, 149));

  rbc.Model_decl(rbc.Fsm_build_end("pos35c"));
}

int fio45_c_fsm(void)
{
  rbc.Model_decl(rbc.Fsm_build_begin("fio45c"));
  rbc.Model_decl(rbc.Fsm_new_start_state("start_up"));
  rbc.Model_decl(rbc.Fsm_new_final_state("finish"));

  // read transitions
  // #1 "start_up" => "open_r" : fopen(*, "r")
  rbc.Model_decl(rbc.Fsm_add_transition("start_up", "fopen", rbc.Get_arg(1),
                                        rbc.Is_str_eq(rbc.Get_arg(2), "r") | rbc.Is_str_eq(rbc.Get_arg(2), "rb"),
                                        "open_r", NULL, 65));
  rbc.Model_decl(rbc.Fsm_add_transition("open_r", "fopen", rbc.Get_arg(1),
                                        !(rbc.Is_str_eq(rbc.Get_arg(2), "r") | rbc.Is_str_eq(rbc.Get_arg(2), "rb")),
                                        "open_r", "MSR_22_3", 71));
  // #2 "open_r" => "open_rv" : if (f != NULL)
  rbc.Model_decl(rbc.Fsm_add_transition("open_r", "if", NULL,
                                        rbc.Is_func_exec_successful("fopen", "ne", NULL),
                                        "open_rv", NULL, 66));
  rbc.Model_decl(rbc.Fsm_add_transition("open_r", "if", NULL,
                                        !rbc.Is_func_exec_successful("fopen", "ne", NULL),
                                        "finish", NULL, 149));
  // #3 "open_r" => "open_rv" : fread(_, _, _, *)
  rbc.Model_decl(rbc.Fsm_add_transition("open_r", "fread", rbc.Get_arg(4),
                                        1, "open_rv", "ERR33-C MSR_D_4_7", 67));
  // #4 "open_r" => "open_rv" : fwrite(_, _, _, *)
  rbc.Model_decl(rbc.Fsm_add_transition("open_r", "fwrite", rbc.Get_arg(4),
                                        1, "open_rv", "ERR33-C MSR_D_4_7 WRF MSR_22_4", 68));
  // #4.1 "open_r" => "open_rv" : fprintf(*, _, ...)
  rbc.Model_decl(rbc.Fsm_add_transition("open_r", "fprintf", rbc.Get_arg(1),
                                        1, "open_rv", "ERR33-C MSR_D_4_7 MSR_22_4", 68));
  // #4.2 "open_r" => "open_rv" : fputc(_, *)
  rbc.Model_decl(rbc.Fsm_add_transition("open_r", "fputc", rbc.Get_arg(2),
                                        1, "open_rv", "ERR33-C MSR_D_4_7 MSR_22_4", 68));
  // #4.3 "open_r" => "open_rv" : fputs(_, *)
  rbc.Model_decl(rbc.Fsm_add_transition("open_r", "fputs", rbc.Get_arg(2),
                                        1, "open_rv", "ERR33-C MSR_D_4_7 MSR_22_4", 68));
  // #4.4 "open_r" => "open_rv" : vfprintf(*, _, ...)
  rbc.Model_decl(rbc.Fsm_add_transition("open_r", "vfprintf", rbc.Get_arg(1),
                                        1, "open_rv", "ERR33-C MSR_D_4_7 MSR_22_4", 68));
  // #5 "open_r" => "finish" : fclose(*)
  rbc.Model_decl(rbc.Fsm_add_transition("open_r", "fclose", rbc.Get_arg(1),
                                        1, "finish", "ERR33-C MSR_D_4_7", 69));
  // #6 "open_rv" => "open_rv" : fread(_, _, _, *)
  rbc.Model_decl(rbc.Fsm_add_transition("open_rv", "fread", rbc.Get_arg(4),
                                        1, "open_rv", NULL, 67));
  // #7 "open_rv" => "open_rv" : fwrite(_, _, _, *)
  rbc.Model_decl(rbc.Fsm_add_transition("open_rv", "fwrite", rbc.Get_arg(4),
                                        1, "open_rv", "WRF MSR_22_4", 68));
  // #7.1 "open_rv" => "open_rv" : fprintf(*, _, ...)
  rbc.Model_decl(rbc.Fsm_add_transition("open_rv", "fprintf", rbc.Get_arg(1),
                                        1, "open_rv", "MSR_22_4", 68));
  // #7.2 "open_rv" => "open_rv" : fputc(_, *)
  rbc.Model_decl(rbc.Fsm_add_transition("open_rv", "fputc", rbc.Get_arg(2),
                                        1, "open_rv", "MSR_22_4", 68));
  // #7.3 "open_rv" => "open_rv" : fputs(_, *)
  rbc.Model_decl(rbc.Fsm_add_transition("open_rv", "fputs", rbc.Get_arg(2),
                                        1, "open_rv", "MSR_22_4", 68));
  // #7.4 "open_rv" => "open_rv" : vfprintf(*, _, ...)
  rbc.Model_decl(rbc.Fsm_add_transition("open_rv", "vfprintf", rbc.Get_arg(1),
                                        1, "open_rv", "MSR_22_4", 68));
  // #8 "open_rv" => "finish" : fclose(*)
  rbc.Model_decl(rbc.Fsm_add_transition("open_rv", "fclose", rbc.Get_arg(1),
                                        1, "finish", NULL, 69));
  // #9 "open_r" => "finish" : default
  rbc.Model_decl(rbc.Fsm_set_default_action("open_r", "FIO42-C MSR_22_1", 70));
  // #10 "open_rv" => "finish" : default turn off before if_opnd API is ready
  rbc.Model_decl(rbc.Fsm_set_default_action("open_rv", "FIO42-C MSR_22_1", 70));

  // write transitions
  // #1 "start_up" => "open_w" : fopen(*, "w")
  rbc.Model_decl(rbc.Fsm_add_transition("start_up", "fopen", rbc.Get_arg(1),
                                        !(rbc.Is_str_eq(rbc.Get_arg(2), "r") | rbc.Is_str_eq(rbc.Get_arg(2), "rb")) &
                                        !rbc.Is_str_sub(rbc.Get_arg(2), "x"),
                                        "open_w", NULL, 71));
  rbc.Model_decl(rbc.Fsm_add_transition("open_w", "fopen", rbc.Get_arg(1),
                                        rbc.Is_str_eq(rbc.Get_arg(2), "r") | rbc.Is_str_eq(rbc.Get_arg(2), "rb"),
                                        "open_w", "MSR_22_3", 65));
  // #2 "open_w" => "open_wv" : if (f != NULL)
  rbc.Model_decl(rbc.Fsm_add_transition("open_w", "if", NULL,
                                        rbc.Is_func_exec_successful("fopen", "ne", NULL),
                                        "open_wv", NULL, 66));
  rbc.Model_decl(rbc.Fsm_add_transition("open_w", "if", NULL,
                                        !rbc.Is_func_exec_successful("fopen", "ne", NULL),
                                        "finish", NULL, 149));
  // #3 "open_w" => "open_wv" : fread(_, _, _, *)
  rbc.Model_decl(rbc.Fsm_add_transition("open_w", "fread", rbc.Get_arg(4),
                                        1, "open_wv", "ERR33-C MSR_D_4_7 FIO45-C", 67));
  // #4 "open_w" => "open_wv" : fwrite(_, _, _, *)
  rbc.Model_decl(rbc.Fsm_add_transition("open_w", "fwrite", rbc.Get_arg(4),
                                        1, "open_wv", "ERR33-C MSR_D_4_7 FIO45-C", 68));
  // #5 "open_w" => "finish" : fclose(*)
  rbc.Model_decl(rbc.Fsm_add_transition("open_w", "fclose", rbc.Get_arg(1),
                                        1, "finish", "ERR33-C MSR_D_4_7", 69));
  // #6 "open_wv" => "open_wv" : fread(_, _, _, *)
  rbc.Model_decl(rbc.Fsm_add_transition("open_wv", "fread", rbc.Get_arg(4),
                                        1, "open_wv", "FIO45-C", 67));
  // #7 "open_wv" => "open_wv" : fwrite(_, _, _, *)
  rbc.Model_decl(rbc.Fsm_add_transition("open_wv", "fwrite", rbc.Get_arg(4),
                                        1, "open_wv", "FIO45-C", 68));
  // #8 "open_wv" => "finish" : fclose(*)
  rbc.Model_decl(rbc.Fsm_add_transition("open_wv", "fclose", rbc.Get_arg(1),
                                        1, "finish", NULL, 69));
  // #9 "open_w" => "finish" : default
  rbc.Model_decl(rbc.Fsm_set_default_action("open_w", "FIO42-C MSR_22_1", 70));
  // #10 "open_wv" => "finish" : default turn off before if_opnd API is ready
  rbc.Model_decl(rbc.Fsm_set_default_action("open_wv", "FIO42-C MSR_22_1", 70));

  // exclusive write transitions
  // #1 "start_up" => "open_x" : fopen(*, "wx")
  rbc.Model_decl(rbc.Fsm_add_transition("start_up", "fopen", rbc.Get_arg(1),
                                        !(rbc.Is_str_eq(rbc.Get_arg(2), "r") | rbc.Is_str_eq(rbc.Get_arg(2), "rb")) &
                                        rbc.Is_str_sub(rbc.Get_arg(2), "x"),
                                        "open_x", NULL, 72));
  rbc.Model_decl(rbc.Fsm_add_transition("open_x", "fopen", rbc.Get_arg(1),
                                        rbc.Is_str_eq(rbc.Get_arg(2), "r") | rbc.Is_str_eq(rbc.Get_arg(2), "rb"),
                                        "open_x", "MSR_22_3", 65));
  // #2 "open_x" => "open_xv" : if (f != NULL)
  rbc.Model_decl(rbc.Fsm_add_transition("open_x", "if", NULL,
                                        rbc.Is_func_exec_successful("fopen", "ne", NULL),
                                        "open_xv", NULL, 66));
  rbc.Model_decl(rbc.Fsm_add_transition("open_x", "if", NULL,
                                        !rbc.Is_func_exec_successful("fopen", "ne", NULL),
                                        "finish", NULL, 149));
  // #3 "open_x" => "open_xv" : fread(_, _, _, *)
  rbc.Model_decl(rbc.Fsm_add_transition("open_x", "fread", rbc.Get_arg(4),
                                        1, "open_xv", "ERR33-C MSR_D_4_7", 67));
  // #4 "open_x" => "open_xv" : fwrite(_, _, _, *)
  rbc.Model_decl(rbc.Fsm_add_transition("open_x", "fwrite", rbc.Get_arg(4),
                                        1, "open_xv", "ERR33-C MSR_D_4_7", 68));
  // #5 "open_x" => "finish" : fclose(*)
  rbc.Model_decl(rbc.Fsm_add_transition("open_x", "fclose", rbc.Get_arg(1),
                                        1, "finish", "ERR33-C MSR_D_4_7", 69));
  // #6 "open_xv" => "open_xv" : fread(_, _, _, *)
  rbc.Model_decl(rbc.Fsm_add_transition("open_xv", "fread", rbc.Get_arg(4),
                                        1, "open_xv", NULL, 67));
  // #7 "open_xv" => "open_xv" : fwrite(_, _, _, *)
  rbc.Model_decl(rbc.Fsm_add_transition("open_xv", "fwrite", rbc.Get_arg(4),
                                        1, "open_xv", NULL, 68));
  // #8 "open_xv" => "finish" : fclose(*)
  rbc.Model_decl(rbc.Fsm_add_transition("open_xv", "fclose", rbc.Get_arg(1),
                                        1, "finish", NULL, 69));
  // #9 "open_x" => "finish" : default
  rbc.Model_decl(rbc.Fsm_set_default_action("open_x", "FIO42-C MSR_22_1", 70));
  // #10 "open_xv" => "finish" : default turn off before if_opnd API is ready
  rbc.Model_decl(rbc.Fsm_set_default_action("open_xv", "FIO42-C MSR_22_1", 70));

  rbc.Model_decl(rbc.Fsm_build_end("fio45c"));
}

int socket(int domain, int type, int protocol)
{
  rbc.Model_decl(rbc.Fsm_use("CWE134"));
  return 0;
}

int bind(int sockfd, const struct sockaddr *addr, socklen_t addrlen)
{
  rbc.Model_decl(rbc.Fsm_use("CWE134"));
  return 0;
}

int listen(int sockfd, int backlog)
{
  rbc.Model_decl(rbc.Fsm_use("CWE134"));
  return 0;
}

int accept(int sockfd, struct sockaddr *addr, socklen_t *addrlen)
{
  rbc.Model_decl(rbc.Fsm_use("CWE134"));
  return 0;
}

int connect(int sockfd, const struct sockaddr *addr, socklen_t addrlen)
{
  rbc.Model_decl(rbc.Fsm_use("CWE134"));
  return 0;
}

ssize_t recv(int sockfd, void *buf, size_t len, int flags)
{
  rbc.Model_decl(rbc.Set_parm_mod(rbc.Get_arg(2)));
  rbc.Model_decl(rbc.Fsm_use("CWE134"));
  rbc.Model_decl(rbc.Set_tag(rbc.Get_arg(2), (char *)"tainted"));

  rbc.Rbc_assert(rbc.Get_mem_size(rbc.Get_arg(2)) >= rbc.Get_value(rbc.Get_arg(3)), "MEM35-C");
  return 0;
}

ssize_t recvfrom(int sockfd, void *buf, size_t len, int flags,
                 struct sockaddr *src_addr, socklen_t *addrlen)
{
  rbc.Rbc_same_as_func("recv");
}

int atoi(const char *str)
{
  rbc.Model_decl(rbc.Set_parm_deref(rbc.Get_arg(1)));
  rbc.Model_decl(rbc.Copy_tag(rbc.Get_ret(), rbc.Get_arg(1)));
  return 0;
}

double atof(const char *str)
{
  rbc.Model_decl(rbc.Copy_tag(rbc.Get_ret(), rbc.Get_arg(1)));
  return 0.0;
}

long strtol(const char *str, char **endptr, int base)
{
  rbc.Rbc_same_as_func("atoi");
  rbc.Model_decl(rbc.Set_func_errno_setting());

  rbc.Rbc_assert(rbc.Is_errno_cleared_before(), "MSR_22_8");
  rbc.Rbc_assert(rbc.Is_errno_checked_after(), "MSR_22_9");
  return 0;
}

long long int strtoll(const char *nptr, char **endptr, int base)
{
  rbc.Rbc_same_as_func("atoi");
  rbc.Model_decl(rbc.Set_func_errno_setting());

  rbc.Rbc_assert(rbc.Is_errno_cleared_before(), "MSR_22_8");
  rbc.Rbc_assert(rbc.Is_errno_checked_after(), "MSR_22_9");
  return 0;
}

unsigned long int strtoul(const char *nptr, char **endptr, int base)
{
  rbc.Rbc_same_as_func("atoi");
  rbc.Model_decl(rbc.Set_func_errno_setting());

  rbc.Rbc_assert(rbc.Is_errno_cleared_before(), "MSR_22_8");
  rbc.Rbc_assert(rbc.Is_errno_checked_after(), "MSR_22_9");
  return 0;
}

unsigned long long int strtoull(const char *nptr, char **endptr, int base)
{
  rbc.Rbc_same_as_func("atoi");
  rbc.Model_decl(rbc.Set_func_errno_setting());

  rbc.Rbc_assert(rbc.Is_errno_cleared_before(), "MSR_22_8");
  rbc.Rbc_assert(rbc.Is_errno_checked_after(), "MSR_22_9");
  return 0;
}

double strtod(const char *nptr, char **endptr)
{
  rbc.Rbc_same_as_func("atoi");
  rbc.Model_decl(rbc.Set_func_errno_setting());

  rbc.Rbc_assert(rbc.Is_errno_cleared_before(), "MSR_22_8");
  rbc.Rbc_assert(rbc.Is_errno_checked_after(), "MSR_22_9");
  return 0.0;
}

float strtof(const char *nptr, char **endptr)
{
  rbc.Rbc_same_as_func("atoi");
  rbc.Model_decl(rbc.Set_func_errno_setting());

  rbc.Rbc_assert(rbc.Is_errno_cleared_before(), "MSR_22_8");
  rbc.Rbc_assert(rbc.Is_errno_checked_after(), "MSR_22_9");
  return 0.0;
}

long double strtold(const char *nptr, char **endptr)
{
  rbc.Rbc_same_as_func("atoi");
  rbc.Model_decl(rbc.Set_func_errno_setting());

  rbc.Rbc_assert(rbc.Is_errno_cleared_before(), "MSR_22_8");
  rbc.Rbc_assert(rbc.Is_errno_checked_after(), "MSR_22_9");
  return 0;
}

int kstrtol(const char *s, unsigned int base, long *res)
{
  rbc.Model_decl(rbc.Set_parm_deref(rbc.Get_arg(1)));
  rbc.Model_decl(rbc.Set_parm_mod(rbc.Get_arg(3)));

  rbc.Rbc_assert(rbc.Post_check_var_value(rbc.Get_ret(), "ne", 0), "ERR33-C");
  rbc.Rbc_assert(rbc.Post_check_var_value(rbc.Get_ret(), "ne", 0), "MSR_D_4_7");
  return 0;
}

int _kstrtol(const char *s, unsigned int base, long *res)
{
  rbc.Rbc_same_as_func("kstrtol");
  return 0;
}

int kstrtoul(const char *s, unsigned int base, unsigned long *res)
{
  rbc.Rbc_same_as_func("kstrtol");
  return 0;
}

int _kstrtoul(const char *s, unsigned int base, unsigned long *res)
{
  rbc.Rbc_same_as_func("kstrtoul");
  return 0;
}

int kstrtoull(const char *s, unsigned int base, unsigned long long *res)
{
  rbc.Rbc_same_as_func("kstrtol");
  return 0;
}

int kstrtoll(const char *s, unsigned int base, long long *res)
{
  rbc.Rbc_same_as_func("kstrtol");
  return 0;
}

int kstrtouint(const char *s, unsigned int base, unsigned int *res)
{
  rbc.Rbc_same_as_func("kstrtol");
  return 0;
}

int kstrtoint(const char *s, unsigned int base, int *res)
{
  rbc.Rbc_same_as_func("kstrtol");
  return 0;
}

int kstrtobool(const char *s, bool *res)
{
  rbc.Model_decl(rbc.Set_parm_deref(rbc.Get_arg(1)));
  rbc.Model_decl(rbc.Set_parm_mod(rbc.Get_arg(2)));

  rbc.Rbc_assert(rbc.Post_check_var_value(rbc.Get_ret(), "ne", 0), "ERR33-C");
  rbc.Rbc_assert(rbc.Post_check_var_value(rbc.Get_ret(), "ne", 0), "MSR_D_4_7");
  return 0;
}

int kstrtou16(const char *s, unsigned int base, unsigned short *res)
{
  rbc.Rbc_same_as_func("kstrtol");
  return 0;
}

int kstrtos16(const char *s, unsigned int base, short *res)
{
  rbc.Rbc_same_as_func("kstrtol");
  return 0;
}

int kstrtou8(const char *s, unsigned int base, unsigned char *res)
{
  rbc.Rbc_same_as_func("kstrtol");
  return 0;
}

int kstrtos8(const char *s, unsigned int base, char *res)
{
  rbc.Rbc_same_as_func("kstrtol");
  return 0;
}

int kstrtol_from_user(const char *s, size_t count, unsigned int base, long *res)
{
  rbc.Model_decl(rbc.Set_parm_deref(rbc.Get_arg(1)));
  rbc.Model_decl(rbc.Set_parm_mod(rbc.Get_arg(4)));

  rbc.Rbc_assert(rbc.Post_check_var_value(rbc.Get_ret(), "ne", 0), "ERR33-C");
  rbc.Rbc_assert(rbc.Post_check_var_value(rbc.Get_ret(), "ne", 0), "MSR_D_4_7");
  return 0;
}

int kstrtoll_from_user(const char *s, size_t count, unsigned int base, long long *res)
{
  rbc.Rbc_same_as_func("kstrtol_from_user");
  return 0;
}

int kstrtoul_from_user(const char *s, size_t count, unsigned int base, unsigned long *res)
{
  rbc.Rbc_same_as_func("kstrtol_from_user");
  return 0;
}

int kstrtoull_from_user(const char *s, size_t count, unsigned int base, unsigned long long *res)
{
  rbc.Rbc_same_as_func("kstrtol_from_user");
  return 0;
}

int kstrtouint_from_user(const char *s, size_t count, unsigned int base, unsigned int *res)
{
  rbc.Rbc_same_as_func("kstrtol_from_user");
  return 0;
}

int kstrtoint_from_user(const char *s, size_t count, unsigned int base, int *res)
{
  rbc.Rbc_same_as_func("kstrtol_from_user");
  return 0;
}

int kstrtou16_from_user(const char *s, size_t count, unsigned int base, unsigned short *res)
{
  rbc.Rbc_same_as_func("kstrtol_from_user");
  return 0;
}

int kstrtos16_from_user(const char *s, size_t count, unsigned int base, short *res)
{
  rbc.Rbc_same_as_func("kstrtol_from_user");
  return 0;
}

int kstrtou8_from_user(const char *s, size_t count, unsigned int base, unsigned char *res)
{
  rbc.Rbc_same_as_func("kstrtol_from_user");
  return 0;
}

int kstrtos8_from_user(const char *s, size_t count, unsigned int base, char *res)
{
  rbc.Rbc_same_as_func("kstrtol_from_user");
  return 0;
}

int kstrtobool_from_user(const char *s, size_t count, bool *res)
{
  rbc.Model_decl(rbc.Set_parm_deref(rbc.Get_arg(1)));
  rbc.Model_decl(rbc.Set_parm_mod(rbc.Get_arg(3)));

  rbc.Rbc_assert(rbc.Post_check_var_value(rbc.Get_ret(), "ne", 0), "ERR33-C");
  rbc.Rbc_assert(rbc.Post_check_var_value(rbc.Get_ret(), "ne", 0), "MSR_D_4_7");
  return 0;
}

int setsockopt(int sockfd, int level, int optname, const void *optval, socklen_t optlen)
{
  rbc.Model_decl(rbc.Fsm_use("CWE134"));
  return 0;
}

int cwe134_c_fsm(void)
{
  rbc.Model_decl(rbc.Fsm_build_begin("CWE134"));
  rbc.Model_decl(rbc.Fsm_new_start_state("start_up"));
  rbc.Model_decl(rbc.Fsm_new_final_state("finish"));

  // connect socket
  // #1 "start_up" => "opened" : socket(_, _, _)
  rbc.Model_decl(rbc.Fsm_add_transition("start_up", "socket", rbc.Get_ret(),
                                        1, "opened", NULL, 80));
  // #2 "opened" => "connected" : connect(*, _, _)
  rbc.Model_decl(rbc.Fsm_add_transition("opened", "connect", rbc.Get_arg(1),
                                        1, "connected", NULL, 81));
  // #3 "connected" => "finish" : recv(*, _, _)
  rbc.Model_decl(rbc.Fsm_add_transition("connected", "recv", rbc.Get_arg(1),
                                        1, "finish", NULL, 82));

  // listen socket
  // #4 "opened" => "binded" : bind(*, _, _)
  rbc.Model_decl(rbc.Fsm_add_transition("opened", "bind", rbc.Get_arg(1),
                                        1, "binded", NULL, 83));
  // #5 "binded" => "listened" : listen(*, _)
  rbc.Model_decl(rbc.Fsm_add_transition("binded", "listen", rbc.Get_arg(1),
                                        1, "listened", NULL, 84));
  // #6 "listened" => "accepted" : accept(*, _, _)
  rbc.Model_decl(rbc.Fsm_add_transition("listened", "accept", rbc.Get_arg(1),
                                        1, "accepted", NULL, 85));
  // #7 "accepted" => "finish" : recv(*, _, _)
  rbc.Model_decl(rbc.Fsm_add_transition("accepted", "recv", rbc.Get_arg(1),
                                        1, "finish", NULL, 82));
  // wrong orders:
  // #8 "opened" => "finish" : listen(*, _)
   rbc.Model_decl(rbc.Fsm_add_transition("opened", "listen", rbc.Get_arg(1),
                                         1, "finish", "LSO", 134));
  // // #9 "opened" => "finish" : accept(*, _, _)
   rbc.Model_decl(rbc.Fsm_add_transition("opened", "accept", rbc.Get_arg(1),
                                         1, "finish", "LSO", 134));
  // // #10 "opened" => "finish" : recv(*, _, _)
   rbc.Model_decl(rbc.Fsm_add_transition("opened", "recv", rbc.Get_arg(1),
                                         1, "finish", "LSO", 134));
  // // #11 "binded" => "finish" : accept(*, _, _)
   rbc.Model_decl(rbc.Fsm_add_transition("binded", "accept", rbc.Get_arg(1),
                                         1, "finish", "LSO", 134));
  // // #12 "binded" => "finish" : recv(*, _, _)
   rbc.Model_decl(rbc.Fsm_add_transition("binded", "recv", rbc.Get_arg(1),
                                         1, "finish", "LSO", 134));
  // // #13 "listened" => "finish" : recv(*, _, _)
   rbc.Model_decl(rbc.Fsm_add_transition("listened", "recv", rbc.Get_arg(1),
                                         1, "finish", "LSO", 134));
  // // #14 "opened" => "opened" : setsockopt(*, _, _, _, _)
   rbc.Model_decl(rbc.Fsm_add_transition("opened", "setsockopt", rbc.Get_arg(1),
                                         rbc.Get_value(rbc.Get_arg(3)) == SO_REUSEADDR, "opened", "MBSP", 135));

  rbc.Model_decl(rbc.Fsm_build_end("CWE134"));
}

#ifdef __cplusplus
} // extern C
#endif
