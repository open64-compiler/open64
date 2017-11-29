/*
 Copyright (C) 2010, Hewlett-Packard Development Company, L.P.
 All Rights Reserved.

 Open64 is free software; you can redistribute it and/or
 modify it under the terms of the GNU General Public License
 as published by the Free Software Foundation; either version 2
 of the License, or (at your option) any later version.

 Open64 is distributed in the hope that it will be useful,
 but WITHOUT ANY WARRANTY; without even the implied warranty of
 MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 GNU General Public License for more details.

 You should have received a copy of the GNU General Public License
 along with this program; if not, write to the Free Software
 Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston,
 MA  02110-1301, USA.
*/
#include "cse_table.h"


CallSideEffectInfoBase RawLibcallSideEffectTable[] =
  {
    {
      "abort",
      CFAT_libc_default_attr &
      ~(CFAT_exposes_argument_address_to_return |CFAT_exposes_argument_address_to_globals)
      | CFAT_argument_indirectly_read | CFAT_argument_indirectly_write
      | CFAT_argument_one_level_deref,
      0,
      { 0, }
    },
    {
      "acos",
      CFAT_default_attr & (~CFAT_pure_call_attr_mask),
      0,
      { 0, }
    },
    {
      "acosd",
      CFAT_default_attr & (~CFAT_pure_call_attr_mask),
      0,
      { 0, }
    },
    {
      "acosdf",
      CFAT_default_attr & (~CFAT_pure_call_attr_mask),
      0,
      { 0, }
    },
    {
      "acosdl",
      CFAT_default_attr & (~CFAT_pure_call_attr_mask),
      0,
      { 0, }
    },
    {
      "acosf",
      CFAT_default_attr & (~CFAT_pure_call_attr_mask),
      0,
      { 0, }
    },
    {
      "acosh",
      CFAT_default_attr & (~CFAT_pure_call_attr_mask),
      0,
      { 0, }
    },
    {
      "acoshf",
      CFAT_default_attr & (~CFAT_pure_call_attr_mask),
      0,
      { 0, }
    },
    {
      "acoshl",
      CFAT_default_attr & (~CFAT_pure_call_attr_mask),
      0,
      { 0, }
    },
    {
      "acosl",
      CFAT_default_attr & (~CFAT_pure_call_attr_mask),
      0,
      { 0, }
    },
    {
      "asin",
      CFAT_default_attr & (~CFAT_pure_call_attr_mask),
      0,
      { 0, }
    },
    {
      "asind",
      CFAT_default_attr & (~CFAT_pure_call_attr_mask),
      0,
      { 0, }
    },
    {
      "asindf",
      CFAT_default_attr & (~CFAT_pure_call_attr_mask),
      0,
      { 0, }
    },
    {
      "asindl",
      CFAT_default_attr & (~CFAT_pure_call_attr_mask),
      0,
      { 0, }
    },
    {
      "asinf",
      CFAT_default_attr & (~CFAT_pure_call_attr_mask),
      0,
      { 0, }
    },
    {
      "asinh",
      CFAT_default_attr & (~CFAT_pure_call_attr_mask),
      0,
      { 0, }
    },
    {
      "asinhf",
      CFAT_default_attr & (~CFAT_pure_call_attr_mask),
      0,
      { 0, }
    },
    {
      "asinhl",
      CFAT_default_attr & (~CFAT_pure_call_attr_mask),
      0,
      { 0, }
    },
    {
      "asinl",
      CFAT_default_attr & (~CFAT_pure_call_attr_mask),
      0,
      { 0, }
    },
    {
      "assert",
      CFAT_libc_default_attr &
      ~(CFAT_exposes_argument_address_to_return |CFAT_exposes_argument_address_to_globals)
      //		| CFAT_argument_indirectly_write
      | CFAT_argument_indirectly_read | CFAT_argument_one_level_deref,
      0,
      { 0, }
    },
    {
      "__assert",
      CFAT_libc_default_attr &
      ~(CFAT_exposes_argument_address_to_return |CFAT_exposes_argument_address_to_globals)
      //  assert is not indirectly write its arguments
      //		| CFAT_argument_indirectly_write
      | CFAT_argument_indirectly_read | CFAT_argument_one_level_deref,
      0,
      { 0, }
    },
    {
      "atan",
      CFAT_default_attr & (~CFAT_pure_call_attr_mask),
      0,
      { 0, }
    },
    {
      "atan2",
      CFAT_default_attr & (~CFAT_pure_call_attr_mask),
      0,
      { 0, }
    },
    {
      "atan2d",
      CFAT_default_attr & (~CFAT_pure_call_attr_mask),
      0,
      { 0, }
    },
    {
      "atan2df",
      CFAT_default_attr & (~CFAT_pure_call_attr_mask),
      0,
      { 0, }
    },
    {
      "atan2dl",
      CFAT_default_attr & (~CFAT_pure_call_attr_mask),
      0,
      { 0, }
    },
    {
      "atan2f",
      CFAT_default_attr & (~CFAT_pure_call_attr_mask),
      0,
      { 0, }
    },
    {
      "atan2l",
      CFAT_default_attr & (~CFAT_pure_call_attr_mask),
      0,
      { 0, }
    },
    {
      "atand",
      CFAT_default_attr & (~CFAT_pure_call_attr_mask),
      0,
      { 0, }
    },
    {
      "atandf",
      CFAT_default_attr & (~CFAT_pure_call_attr_mask),
      0,
      { 0, }
    },
    {
      "atandl",
      CFAT_default_attr & (~CFAT_pure_call_attr_mask),
      0,
      { 0, }
    },
    {
      "atanf",
      CFAT_default_attr & (~CFAT_pure_call_attr_mask),
      0,
      { 0, }
    },
    {
      "atanh",
      CFAT_default_attr & (~CFAT_pure_call_attr_mask),
      0,
      { 0, }
    },
    {
      "atanhf",
      CFAT_default_attr & (~CFAT_pure_call_attr_mask),
      0,
      { 0, }
    },
    {
      "atanhl",
      CFAT_default_attr & (~CFAT_pure_call_attr_mask),
      0,
      { 0, }
    },
    {
      "atanl",
      CFAT_default_attr & (~CFAT_pure_call_attr_mask),
      0,
      { 0, }
    },
    {
      "atexit",
      CFAT_libc_default_attr &
      ~(CFAT_exposes_argument_address_to_return |CFAT_exposes_argument_address_to_globals)
      | CFAT_argument_indirectly_read | CFAT_argument_indirectly_write
      | CFAT_argument_one_level_deref,
      0,
      { 0, }
    },
    {
      "atof",
      CFAT_libc_default_attr &
      ~(CFAT_exposes_argument_address_to_return |CFAT_exposes_argument_address_to_globals)
      | CFAT_argument_indirectly_read 
      | CFAT_argument_one_level_deref,
      1,
      { CPA_one_level_read, }
    },
    {
      "atoi",
      CFAT_libc_default_attr &
      ~(CFAT_exposes_argument_address_to_return |CFAT_exposes_argument_address_to_globals)
      | CFAT_argument_indirectly_read 
      | CFAT_argument_one_level_deref,
      1,
      { CPA_one_level_read, }
    },
    {
      "atol",
      CFAT_libc_default_attr &
      ~(CFAT_exposes_argument_address_to_return |CFAT_exposes_argument_address_to_globals)
      | CFAT_argument_indirectly_read 
      | CFAT_argument_one_level_deref,
      1,
      { CPA_one_level_read, }
    },
    {
      "bcmp",
      CFAT_default_attr & ~CFAT_pure_call_attr_mask | CFAT_is_libc_entry 
      |CFAT_argument_indirectly_read |CFAT_argument_one_level_deref,
      3,
      {
        CPA_one_level_read , 
        CPA_one_level_read , 
        CPA_no_ptr_deref_and_expose
      }
    },
    {
      "bcopy",
      CFAT_default_attr & ~CFAT_pure_call_attr_mask | CFAT_is_libc_entry 
      |CFAT_argument_indirectly_read | CFAT_argument_indirectly_write 
      |CFAT_argument_one_level_deref,
      3,
      {
        CPA_one_level_read ,
        CPA_one_level_write , 
        CPA_no_ptr_deref_and_expose
      }
    },
    {
      "bsearch",
      CFAT_default_attr & ~(CFAT_exposes_argument_address_to_return),
      0,
      { 0, }
    },
    {
      "bzero",
      CFAT_default_attr & ~CFAT_pure_call_attr_mask | CFAT_is_libc_entry 
      | CFAT_argument_indirectly_write |CFAT_exposes_argument_address_to_return 
      |CFAT_argument_one_level_deref,
      2,
      {
        CPA_one_level_write | CPA_exposed_to_return,
        CPA_no_ptr_deref_and_expose
      }
    },
    {
      "cacos",
      CFAT_default_attr & (~CFAT_pure_call_attr_mask),
      0,
      { 0, }
    },
    {
      "cacosf",
      CFAT_default_attr & (~CFAT_pure_call_attr_mask),
      0,
      { 0, }
    },
    {
      "cacosh",
      CFAT_default_attr & (~CFAT_pure_call_attr_mask),
      0,
      { 0, }
    },
    {
      "cacoshf",
      CFAT_default_attr & (~CFAT_pure_call_attr_mask),
      0,
      { 0, }
    },
    {
      "calloc",
      CFAT_libc_default_attr &
      ~(CFAT_exposes_argument_address_to_return |
        CFAT_exposes_argument_address_to_globals)
      | CFAT_allocates_heap_memory | CFAT_returns_heap_memory ,
      2,
      { CPA_no_ptr_deref_and_expose, CPA_no_ptr_deref_and_expose },
    },
    {
      "ccos",
      CFAT_default_attr & (~CFAT_pure_call_attr_mask),
      0,
      { 0, }
    },
    {
      "ccosf",
      CFAT_default_attr & (~CFAT_pure_call_attr_mask),
      0,
      { 0, }
    },
    {
      "ceil",
      CFAT_default_attr & (~CFAT_pure_call_attr_mask),
      0,
      { 0, }
    },
    {
      "cexp",
      CFAT_default_attr & (~CFAT_pure_call_attr_mask),
      0,
      { 0, }
    },
    {
      "cexpf",
      CFAT_default_attr & (~CFAT_pure_call_attr_mask),
      0,
      { 0, }
    },
    {
      "chdir",
      CFAT_libc_default_attr &
      ~(CFAT_exposes_argument_address_to_return |CFAT_exposes_argument_address_to_globals)
      | CFAT_argument_indirectly_read | CFAT_argument_indirectly_write
      | CFAT_argument_one_level_deref,
      0,
      { 0, }
    },
    {
      "chmod",
      CFAT_libc_default_attr &
      ~(CFAT_exposes_argument_address_to_return |CFAT_exposes_argument_address_to_globals)
      | CFAT_argument_indirectly_read | CFAT_argument_indirectly_write
      | CFAT_argument_one_level_deref,
      0,
      { 0, }
    },
    {
      "clock",
      CFAT_libc_default_attr &
      ~(CFAT_exposes_argument_address_to_return |CFAT_exposes_argument_address_to_globals)
      | CFAT_argument_indirectly_read | CFAT_argument_indirectly_write
      | CFAT_argument_one_level_deref,
      0,
      { 0, }
    },
    {
      "clogf",
      CFAT_default_attr & (~CFAT_pure_call_attr_mask),
      0,
      { 0, }
    },
    {
      "clearerr",
      CFAT_libc_default_attr &
      ~(CFAT_exposes_argument_address_to_return |CFAT_exposes_argument_address_to_globals)
      | CFAT_argument_indirectly_read | CFAT_argument_indirectly_write
      | CFAT_argument_one_level_deref,
      1,
      { CPA_one_level_read |CPA_two_level_read |CPA_one_level_write |CPA_two_level_write }
    },
    {
      "close",
      CFAT_libc_default_attr &
      ~(CFAT_exposes_argument_address_to_return |CFAT_exposes_argument_address_to_globals)
      | CFAT_argument_indirectly_read | CFAT_argument_indirectly_write
      | CFAT_argument_one_level_deref,
      0,
      { 0, }
    },
    {
      "closedir",
      CFAT_libc_default_attr &
      ~(CFAT_exposes_argument_address_to_return |CFAT_exposes_argument_address_to_globals)
      | CFAT_argument_indirectly_read | CFAT_argument_indirectly_write
      | CFAT_argument_one_level_deref
      | CFAT_deallocates_heap_memory, 
      0,
      { 0, }
    },
    {
      "copysign",
      CFAT_default_attr & (~CFAT_pure_call_attr_mask),
      0,
      { 0, }
    },
    {
      "copysignf",
      CFAT_default_attr & (~CFAT_pure_call_attr_mask),
      0,
      { 0, }
    },
    {
      "copysignl",
      CFAT_default_attr & (~CFAT_pure_call_attr_mask),
      0,
      { 0, }
    },
    {
      "cos",
      CFAT_default_attr & (~CFAT_pure_call_attr_mask),
      0,
      { 0, }
    },
    {
      "cosd",
      CFAT_default_attr & (~CFAT_pure_call_attr_mask),
      0,
      { 0, }
    },
    {
      "cosdf",
      CFAT_default_attr & (~CFAT_pure_call_attr_mask),
      0,
      { 0, }
    },
    {
      "cosdl",
      CFAT_default_attr & (~CFAT_pure_call_attr_mask),
      0,
      { 0, }
    },
    {
      "cosf",
      CFAT_default_attr & (~CFAT_pure_call_attr_mask),
      0,
      { 0, }
    },
    {
      "cosh",
      CFAT_default_attr & (~CFAT_pure_call_attr_mask),
      0,
      { 0, }
    },
    {
      "coshf",
      CFAT_default_attr & (~CFAT_pure_call_attr_mask),
      0,
      { 0, }
    },
    {
      "coshl",
      CFAT_default_attr & (~CFAT_pure_call_attr_mask),
      0,
      { 0, }
    },
    {
      "cosl",
      CFAT_default_attr & (~CFAT_pure_call_attr_mask),
      0,
      { 0, }
    },
    {
      "cpow",
      CFAT_default_attr & (~CFAT_pure_call_attr_mask),
      0,
      { 0, }
    },
    {
      "cpowf",
      CFAT_default_attr & (~CFAT_pure_call_attr_mask),
      0,
      { 0, }
    },
    {
      "cpoww",
      CFAT_default_attr & (~CFAT_pure_call_attr_mask),
      0,
      { 0, }
    },
    {
      "csin",
      CFAT_default_attr & (~CFAT_pure_call_attr_mask),
      0,
      { 0, }
    },
    {
      "csinf",
      CFAT_default_attr & (~CFAT_pure_call_attr_mask),
      0,
      { 0, }
    },
    {
      "csqrt",
      CFAT_default_attr & (~CFAT_pure_call_attr_mask),
      0,
      { 0, }
    },
    {
      "csqrtf",
      CFAT_default_attr & (~CFAT_pure_call_attr_mask),
      0,
      { 0, }
    },
    {
      "ctan",
      CFAT_default_attr & (~CFAT_pure_call_attr_mask),
      0,
      { 0, }
    },
    {
      "ctanf",
      CFAT_default_attr & (~CFAT_pure_call_attr_mask),
      0,
      { 0, }
    },
    {
      "ctime",
      CFAT_libc_default_attr &
      ~(CFAT_exposes_argument_address_to_return |CFAT_exposes_argument_address_to_globals)
      | CFAT_argument_indirectly_read | CFAT_argument_indirectly_write
      | CFAT_argument_one_level_deref | CFAT_return_is_not_escaping,
      0,
      { 0, }
    },
    {
      "difftime",
      CFAT_libc_default_attr &
      ~(CFAT_exposes_argument_address_to_return |CFAT_exposes_argument_address_to_globals)
      | CFAT_argument_indirectly_read | CFAT_argument_indirectly_write
      | CFAT_argument_one_level_deref,
      0,
      { 0, }
    },
    {
      "div",
      CFAT_default_attr & (~CFAT_pure_call_attr_mask),
      0,
      { 0, }
    },
    {
      "drand48",
      CFAT_libc_default_attr &
      ~(CFAT_exposes_argument_address_to_return |CFAT_exposes_argument_address_to_globals),
      0,
      { 0, }
    },
    {
      "dup",
      CFAT_libc_default_attr &
      ~(CFAT_exposes_argument_address_to_return |CFAT_exposes_argument_address_to_globals)
      | CFAT_argument_indirectly_read | CFAT_argument_indirectly_write
      | CFAT_argument_one_level_deref,
      0,
      { 0, }
    },
    {
      "dup2",
      CFAT_libc_default_attr &
      ~(CFAT_exposes_argument_address_to_return |CFAT_exposes_argument_address_to_globals)
      | CFAT_argument_indirectly_read | CFAT_argument_indirectly_write
      | CFAT_argument_one_level_deref,
      0,
      { 0, }
    },
    {
      "ecvt",
      CFAT_libc_default_attr &
      ~(CFAT_exposes_argument_address_to_return |CFAT_exposes_argument_address_to_globals)
      | CFAT_argument_indirectly_write
      | CFAT_argument_one_level_deref
      | CFAT_allocates_heap_memory | CFAT_returns_heap_memory | CFAT_returns_exposed_memory,
      4,
      { CPA_no_ptr_deref_and_expose, CPA_no_ptr_deref_and_expose, CPA_one_level_write,CPA_one_level_write }
    },
    {
      "__errno",
      CFAT_libc_default_attr &
      ~(CFAT_exposes_argument_address_to_return |CFAT_exposes_argument_address_to_globals)
      | CFAT_argument_indirectly_read | CFAT_argument_indirectly_write
      | CFAT_argument_one_level_deref,
      0,
      { 0, }
    },
    {
      "execl",
      CFAT_libc_default_attr &
      ~(CFAT_exposes_argument_address_to_return |CFAT_exposes_argument_address_to_globals)
      | CFAT_argument_indirectly_read | CFAT_argument_indirectly_write
      | CFAT_argument_one_level_deref,
      0,
      { 0, }
    },
    {
      "execv",
      CFAT_libc_default_attr &
      ~(CFAT_exposes_argument_address_to_return |CFAT_exposes_argument_address_to_globals)
      | CFAT_argument_indirectly_read | CFAT_argument_indirectly_write
      | CFAT_argument_one_level_deref,
      0,
      { 0, }
    },
    {
      "execve",
      CFAT_libc_default_attr &
      ~(CFAT_exposes_argument_address_to_return |CFAT_exposes_argument_address_to_globals)
      | CFAT_argument_indirectly_read | CFAT_argument_indirectly_write
      | CFAT_argument_one_level_deref,
      0,
      { 0, }
    },
    {
      "execvp",
      CFAT_libc_default_attr &
      ~(CFAT_exposes_argument_address_to_return |CFAT_exposes_argument_address_to_globals)
      | CFAT_argument_indirectly_read | CFAT_argument_indirectly_write
      | CFAT_argument_one_level_deref,
      0,
      { 0, }
    },
    {
      "exit",
      CFAT_libc_default_attr &
      ~(CFAT_exposes_argument_address_to_return |CFAT_exposes_argument_address_to_globals),
      1,
      { CPA_no_ptr_deref_and_expose, }
    },
    {
      "_exit",
      CFAT_libc_default_attr &
      ~(CFAT_exposes_argument_address_to_return |CFAT_exposes_argument_address_to_globals),
      0,
      { 0, }
    },
    {
      "exp",
      CFAT_default_attr & (~CFAT_pure_call_attr_mask),
      0,
      { 0, }
    },
    {
      "expf",
      CFAT_default_attr & (~CFAT_pure_call_attr_mask),
      0,
      { 0, }
    },
    {
      "expl",
      CFAT_default_attr & (~CFAT_pure_call_attr_mask),
      0,
      { 0, }
    },
    {
      "_F90_ALLOCATE",
      CFAT_argument_indirectly_read | CFAT_argument_indirectly_write |
      CFAT_allocates_heap_memory |CFAT_is_lib_f90,
      3,
      { CPA_one_level_read |CPA_one_level_write |CPA_two_level_write | 
        CPA_is_pointer_to_heap_addr_loc, CPA_one_level_read | 
        CPA_one_level_write, 
        CPA_no_ptr_deref_and_expose
      }
    },
    {
      "fabs",
      CFAT_default_attr & (~CFAT_pure_call_attr_mask),
      0,
      { 0, }
    },
    {
      "fclose",
      CFAT_libc_default_attr &
      ~(CFAT_exposes_argument_address_to_return |CFAT_exposes_argument_address_to_globals)
      | CFAT_argument_indirectly_read | CFAT_argument_indirectly_write
      | CFAT_argument_one_level_deref,
      1,
      { CPA_one_level_read |CPA_two_level_read |CPA_one_level_write |CPA_two_level_write
        |CPA_is_not_escaping }
    },
    {
      "fcntl",
      CFAT_libc_default_attr &
      ~(CFAT_exposes_argument_address_to_return |CFAT_exposes_argument_address_to_globals)
      | CFAT_argument_indirectly_read | CFAT_argument_indirectly_write
      | CFAT_argument_one_level_deref,
      0,
      { 0, }
    },
    {
      "fcvt",
      CFAT_libc_default_attr &
      ~(CFAT_exposes_argument_address_to_return |CFAT_exposes_argument_address_to_globals)
      | CFAT_argument_indirectly_write
      | CFAT_argument_one_level_deref
      | CFAT_allocates_heap_memory | CFAT_returns_heap_memory | CFAT_returns_exposed_memory,
      4,
      { CPA_no_ptr_deref_and_expose, CPA_no_ptr_deref_and_expose, CPA_one_level_write,CPA_one_level_write }
    },
    {
      "fdopen",
      CFAT_libc_default_attr &
      ~(CFAT_exposes_argument_address_to_return |CFAT_exposes_argument_address_to_globals)
      | CFAT_argument_indirectly_read | CFAT_argument_indirectly_write
      | CFAT_argument_one_level_deref,
      0,
      { 0, }
    },
    {
      "feof",
      CFAT_libc_default_attr &
      ~(CFAT_exposes_argument_address_to_return |CFAT_exposes_argument_address_to_globals)
      | CFAT_argument_indirectly_read | CFAT_argument_indirectly_write
      | CFAT_argument_one_level_deref,
      1,
      { CPA_one_level_read |CPA_two_level_read |CPA_one_level_write |CPA_two_level_write
        |CPA_is_not_escaping }
    },
    {
      "ferror",
      CFAT_libc_default_attr &
      ~(CFAT_exposes_argument_address_to_return |CFAT_exposes_argument_address_to_globals)
      | CFAT_argument_indirectly_read | CFAT_argument_indirectly_write
      | CFAT_argument_one_level_deref,
      1,
      { CPA_one_level_read |CPA_two_level_read |CPA_one_level_write |CPA_two_level_write }
    },
    {
      "fflush",
      CFAT_libc_default_attr &
      ~(CFAT_exposes_argument_address_to_return |CFAT_exposes_argument_address_to_globals)
      | CFAT_argument_indirectly_read | CFAT_argument_indirectly_write
      | CFAT_argument_one_level_deref,
      1,
      { CPA_one_level_read |CPA_two_level_read |CPA_one_level_write |CPA_two_level_write
        |CPA_is_not_escaping}
    },
    {
      "ffs",
      CFAT_default_attr & ~CFAT_pure_call_attr_mask | CFAT_is_libc_entry ,
      1,
      {
        CPA_no_ptr_deref_and_expose
      }
    },
    {
      "fgetc",
      CFAT_libc_default_attr &
      ~(CFAT_exposes_argument_address_to_return |CFAT_exposes_argument_address_to_globals)
      | CFAT_argument_indirectly_read | CFAT_argument_indirectly_write
      | CFAT_argument_one_level_deref,
      1,
      { CPA_one_level_read |CPA_two_level_read |CPA_one_level_write |CPA_two_level_write }
    },
    {
      "fgets",
      CFAT_libc_default_attr &
      ~(CFAT_exposes_argument_address_to_return |CFAT_exposes_argument_address_to_globals)
      | CFAT_argument_indirectly_read | CFAT_argument_indirectly_write
      | CFAT_argument_one_level_deref  |CFAT_exposes_argument_address_to_return,
      3,
      { 
        CPA_one_level_write | CPA_one_level_read | CPA_exposed_to_return |CPA_is_not_escaping,
        0,
        CPA_one_level_read |CPA_two_level_read |CPA_one_level_write |CPA_two_level_write
        |CPA_is_not_escaping
      }
    },
    {
      "__filbuf",
      CFAT_libc_default_attr &
      ~(CFAT_exposes_argument_address_to_return |CFAT_exposes_argument_address_to_globals)
      | CFAT_argument_indirectly_read | CFAT_argument_indirectly_write
      | CFAT_argument_one_level_deref,
      1,
      { CPA_one_level_read |CPA_two_level_read |CPA_one_level_write |CPA_two_level_write }
    },
    {
      "fileno",
      CFAT_libc_default_attr &
      ~(CFAT_exposes_argument_address_to_return |CFAT_exposes_argument_address_to_globals)
      | CFAT_argument_indirectly_read | CFAT_argument_indirectly_write
      | CFAT_argument_one_level_deref,
      0,
      { 0, }
    },
    {
      "floor",
      CFAT_default_attr & (~CFAT_pure_call_attr_mask),
      0,
      { 0, }
    },
    {
      "__flsbuf",
      CFAT_libc_default_attr &
      ~(CFAT_exposes_argument_address_to_return |CFAT_exposes_argument_address_to_globals)
      | CFAT_argument_indirectly_read | CFAT_argument_indirectly_write
      | CFAT_argument_one_level_deref,
      2,
      { 0, CPA_one_level_read |CPA_two_level_read |CPA_one_level_write |CPA_two_level_write}
    },
    {
      "fmod",
      CFAT_default_attr & (~CFAT_pure_call_attr_mask),
      0,
      { 0, }
    },
    {
      "fmodf",
      CFAT_default_attr & (~CFAT_pure_call_attr_mask),
      0,
      { 0, }
    },
    {
      "fmodl",
      CFAT_default_attr & (~CFAT_pure_call_attr_mask),
      0,
      { 0, }
    },
    {
      "fopen",
      CFAT_libc_default_attr &
      ~(CFAT_exposes_argument_address_to_return |CFAT_exposes_argument_address_to_globals)
      | CFAT_argument_indirectly_read | CFAT_argument_indirectly_write
      | CFAT_argument_one_level_deref,
      0,
      { 0, }
    },
    {
      "fork",
      CFAT_libc_default_attr &
      ~(CFAT_exposes_argument_address_to_return |CFAT_exposes_argument_address_to_globals)
      | CFAT_argument_indirectly_read | CFAT_argument_indirectly_write
      | CFAT_argument_one_level_deref,
      0,
      { 0, }
    },
    {
      "fprintf",
      CFAT_libc_default_attr &
      ~(CFAT_exposes_argument_address_to_return |CFAT_exposes_argument_address_to_globals)
      | CFAT_argument_indirectly_read /* default for var args */
      | CFAT_argument_one_level_deref |CFAT_is_printf_like | CFAT_has_format_string,
      2,
      { CPA_one_level_read |CPA_two_level_read |CPA_one_level_write|CPA_two_level_write
        |CPA_is_not_escaping,
        CPA_one_level_read |CPA_is_format_string, }
    },
    {
      "fputc",
      CFAT_libc_default_attr &
      ~(CFAT_exposes_argument_address_to_return |CFAT_exposes_argument_address_to_globals)
      | CFAT_argument_indirectly_read | CFAT_argument_indirectly_write
      | CFAT_argument_one_level_deref,
      2,
      { 
        0,
        CPA_one_level_read |CPA_two_level_read |CPA_one_level_write |CPA_two_level_write,
      }
    },
    {
      "fputs",
      CFAT_libc_default_attr &
      ~(CFAT_exposes_argument_address_to_return |CFAT_exposes_argument_address_to_globals)
      | CFAT_argument_indirectly_read | CFAT_argument_indirectly_write
      | CFAT_argument_one_level_deref,
      2,
      { 
        CPA_one_level_read ,
        CPA_one_level_read |CPA_one_level_write |CPA_two_level_read | CPA_two_level_write
        |CPA_is_not_escaping,
      }
    },
    {
      "fread",
      CFAT_libc_default_attr &
      ~(CFAT_exposes_argument_address_to_return |CFAT_exposes_argument_address_to_globals)
      | CFAT_argument_indirectly_read | CFAT_argument_indirectly_write
      | CFAT_argument_one_level_deref,
      4,
      { CPA_one_level_write |CPA_is_not_escaping, 0, 0,
        CPA_one_level_read |CPA_two_level_read |CPA_one_level_write |CPA_two_level_write
        |CPA_is_not_escaping}
    },
    {
      "free",
      CFAT_libc_default_attr &
      ~(CFAT_exposes_argument_address_to_return |
        CFAT_exposes_argument_address_to_globals)
      | CFAT_argument_indirectly_write
      | CFAT_argument_one_level_deref | CFAT_deallocates_heap_memory,
      1,
      { CPA_one_level_write }
    },
    {
      "freopen",
      CFAT_libc_default_attr &
      ~(CFAT_exposes_argument_address_to_return |CFAT_exposes_argument_address_to_globals)
      | CFAT_argument_indirectly_read | CFAT_argument_indirectly_write
      | CFAT_argument_one_level_deref,
      0,
      { 0, }
    },
    {
      "frexp",
      CFAT_default_attr & (~CFAT_pure_call_attr_mask)
      | CFAT_argument_indirectly_write | CFAT_argument_one_level_deref,
      2,
      { CPA_no_ptr_deref_and_expose, CPA_one_level_write }
    },
    {
      "_frexp",
      CFAT_default_attr & (~CFAT_pure_call_attr_mask)
      | CFAT_argument_indirectly_write | CFAT_argument_one_level_deref,
      2,
      { CPA_no_ptr_deref_and_expose, CPA_one_level_write }
    },
    {
      "frexpf",
      CFAT_default_attr & (~CFAT_pure_call_attr_mask)
      | CFAT_argument_indirectly_write | CFAT_argument_one_level_deref,
      2,
      { CPA_no_ptr_deref_and_expose, CPA_one_level_write }
    },
    {
      "_frexpf",
      CFAT_default_attr & (~CFAT_pure_call_attr_mask)
      | CFAT_argument_indirectly_write | CFAT_argument_one_level_deref,
      2,
      { CPA_no_ptr_deref_and_expose, CPA_one_level_write }
    },
    {
      "frexpl",
      CFAT_default_attr & (~CFAT_pure_call_attr_mask)
      | CFAT_argument_indirectly_write | CFAT_argument_one_level_deref,
      2,
      { CPA_no_ptr_deref_and_expose, CPA_one_level_write }
    },
    {
      "_frexpl",
      CFAT_default_attr & (~CFAT_pure_call_attr_mask)
      | CFAT_argument_indirectly_write | CFAT_argument_one_level_deref,
      2,
      { CPA_no_ptr_deref_and_expose, CPA_one_level_write }
    },
    {
      "frexpw",
      CFAT_default_attr & (~CFAT_pure_call_attr_mask)
      | CFAT_argument_indirectly_write | CFAT_argument_one_level_deref,
      2,
      { CPA_no_ptr_deref_and_expose, CPA_one_level_write }
    },
    {
      "frexpq",
      CFAT_default_attr & (~CFAT_pure_call_attr_mask)
      | CFAT_argument_indirectly_write | CFAT_argument_one_level_deref,
      2,
      { CPA_no_ptr_deref_and_expose, CPA_one_level_write }
    },
    {
      "fscanf",
      CFAT_libc_default_attr &
      ~(CFAT_exposes_argument_address_to_return |CFAT_exposes_argument_address_to_globals)
      | CFAT_argument_indirectly_read | CFAT_argument_indirectly_write
      | CFAT_argument_one_level_deref,
      2,
      { 
        CPA_one_level_read |CPA_two_level_read |CPA_one_level_write|CPA_two_level_write, 
        CPA_one_level_read |CPA_is_format_string
      },
    },
    {
      "fseek",
      CFAT_libc_default_attr &
      ~(CFAT_exposes_argument_address_to_return |CFAT_exposes_argument_address_to_globals)
      | CFAT_argument_indirectly_read | CFAT_argument_indirectly_write
      | CFAT_argument_one_level_deref,
      1,
      { CPA_one_level_read |CPA_two_level_read |CPA_one_level_write |CPA_two_level_write
        |CPA_is_not_escaping}
    },
    {
      "fseeko",
      CFAT_libc_default_attr &
      ~(CFAT_exposes_argument_address_to_return |CFAT_exposes_argument_address_to_globals)
      | CFAT_argument_indirectly_read | CFAT_argument_indirectly_write
      | CFAT_argument_one_level_deref,
      1,
      { CPA_one_level_read |CPA_two_level_read |CPA_one_level_write |CPA_two_level_write, }
    },
    {
      "fstat",
      CFAT_libc_default_attr &
      ~(CFAT_exposes_argument_address_to_return |CFAT_exposes_argument_address_to_globals)
      | CFAT_argument_indirectly_read | CFAT_argument_indirectly_write
      | CFAT_argument_one_level_deref,
      0,
      { 0, }
    },
    {
      "__fstat64",
      CFAT_libc_default_attr &
      ~(CFAT_exposes_argument_address_to_return |CFAT_exposes_argument_address_to_globals)
      | CFAT_argument_indirectly_read | CFAT_argument_indirectly_write
      | CFAT_argument_one_level_deref,
      0,
      { 0, }
    },
    {
      "ftell",
      CFAT_libc_default_attr &
      ~(CFAT_exposes_argument_address_to_return 
        |CFAT_exposes_argument_address_to_globals)
      | CFAT_argument_indirectly_read | CFAT_argument_indirectly_write
      | CFAT_argument_one_level_deref,
      1,
      { CPA_one_level_read |CPA_two_level_read |CPA_one_level_write 
        |CPA_two_level_write |CPA_is_not_escaping, }
    },
    {
      "ftime",
      CFAT_libc_default_attr &
      ~(CFAT_exposes_argument_address_to_return |CFAT_exposes_argument_address_to_globals)
      | CFAT_argument_indirectly_read | CFAT_argument_indirectly_write
      | CFAT_argument_one_level_deref,
      0,
      { 0, }
    },
    {
      "ftruncate",
      CFAT_libc_default_attr &
      ~(CFAT_exposes_argument_address_to_return |CFAT_exposes_argument_address_to_globals)
      | CFAT_argument_indirectly_read | CFAT_argument_indirectly_write
      | CFAT_argument_one_level_deref,
      0,
      { 0, }
    },
    {
      "fwrite",
      CFAT_libc_default_attr &
      ~(CFAT_exposes_argument_address_to_return |CFAT_exposes_argument_address_to_globals)
      | CFAT_argument_indirectly_read | CFAT_argument_indirectly_write
      | CFAT_argument_one_level_deref,
      4,
      { CPA_one_level_read, 0, 0, 
        CPA_one_level_read |CPA_two_level_read |CPA_one_level_write |CPA_two_level_write
        |CPA_is_not_escaping}
    },
    {
      "gcvt",
      CFAT_libc_default_attr &
      ~(CFAT_exposes_argument_address_to_globals)
      | CFAT_exposes_argument_address_to_return
      | CFAT_argument_indirectly_write
      | CFAT_argument_one_level_deref,
      3,
      { CPA_no_ptr_deref_and_expose, CPA_no_ptr_deref_and_expose, 
        CPA_one_level_write|CPA_exposed_to_return 
      }
    },
    {
      "getc",
      CFAT_libc_default_attr &
      ~(CFAT_exposes_argument_address_to_return |CFAT_exposes_argument_address_to_globals)
      | CFAT_argument_indirectly_read | CFAT_argument_indirectly_write
      | CFAT_argument_one_level_deref,
      1,
      { CPA_one_level_read |CPA_two_level_read |CPA_one_level_write |CPA_two_level_write, }
    },
    {
      "getchar",
      CFAT_libc_default_attr &
      ~(CFAT_exposes_argument_address_to_return |CFAT_exposes_argument_address_to_globals)
      | CFAT_argument_indirectly_read | CFAT_argument_indirectly_write
      | CFAT_argument_one_level_deref,
      0,
      { 0, }
    },
    {
      "getcwd",
      CFAT_libc_default_attr &
      ~(CFAT_exposes_argument_address_to_globals)
      | CFAT_exposes_argument_address_to_return
      | CFAT_argument_indirectly_read | CFAT_argument_indirectly_write
      | CFAT_argument_one_level_deref
      | CFAT_allocates_heap_memory | CFAT_returns_heap_memory,
      1,
      { CPA_one_level_write | CPA_one_level_read | CPA_exposed_to_return, }
    },
    {
      "getegid",
      CFAT_libc_default_attr &
      ~(CFAT_exposes_argument_address_to_return |CFAT_exposes_argument_address_to_globals)
      | CFAT_argument_indirectly_read | CFAT_argument_indirectly_write
      | CFAT_argument_one_level_deref,
      0,
      { 0, }
    },
    {
      "getenv",
      CFAT_libc_default_attr &
      ~(CFAT_exposes_argument_address_to_return |CFAT_exposes_argument_address_to_globals)
      | CFAT_argument_indirectly_read | CFAT_argument_indirectly_write
      | CFAT_argument_one_level_deref | CFAT_return_is_not_escaping,
      0,
      { 0, }
    },
    {
      "geteuid",
      CFAT_libc_default_attr &
      ~(CFAT_exposes_argument_address_to_return |CFAT_exposes_argument_address_to_globals)
      | CFAT_argument_indirectly_read | CFAT_argument_indirectly_write
      | CFAT_argument_one_level_deref,
      0,
      { 0, }
    },
    {
      "getgid",
      CFAT_libc_default_attr &
      ~(CFAT_exposes_argument_address_to_return |CFAT_exposes_argument_address_to_globals)
      | CFAT_argument_indirectly_read | CFAT_argument_indirectly_write
      | CFAT_argument_one_level_deref,
      0,
      { 0, }
    },
    {
      "gethostname",
      CFAT_libc_default_attr &
      ~(CFAT_exposes_argument_address_to_return |CFAT_exposes_argument_address_to_globals)
      | CFAT_argument_indirectly_read | CFAT_argument_indirectly_write
      | CFAT_argument_one_level_deref,
      2,
      { CPA_one_level_read | CPA_one_level_write, 0 }
    },
    {
      "getopt",
      CFAT_libc_default_attr &
      ~(CFAT_exposes_argument_address_to_return |CFAT_exposes_argument_address_to_globals)
      | CFAT_argument_indirectly_read ,
      3,
      { CPA_no_ptr_deref_and_expose, CPA_one_level_read | CPA_two_level_read, CPA_one_level_read }
    },
    {
      "getpagesize",
      CFAT_libc_default_attr &
      ~(CFAT_exposes_argument_address_to_return |CFAT_exposes_argument_address_to_globals)
      | CFAT_argument_indirectly_read | CFAT_argument_indirectly_write
      | CFAT_argument_one_level_deref,
      0,
      { 0, }
    },
    {
      "getpgid",
      CFAT_libc_default_attr &
      ~(CFAT_exposes_argument_address_to_return |CFAT_exposes_argument_address_to_globals)
      | CFAT_argument_indirectly_read | CFAT_argument_indirectly_write
      | CFAT_argument_one_level_deref,
      0,
      { 0, }
    },
    {
      "getpgrp",
      CFAT_libc_default_attr &
      ~(CFAT_exposes_argument_address_to_return |CFAT_exposes_argument_address_to_globals)
      | CFAT_argument_indirectly_read | CFAT_argument_indirectly_write
      | CFAT_argument_one_level_deref,
      0,
      { 0, }
    },
    {
      "getpid",
      CFAT_libc_default_attr &
      ~(CFAT_exposes_argument_address_to_return |CFAT_exposes_argument_address_to_globals)
      | CFAT_argument_indirectly_read | CFAT_argument_indirectly_write
      | CFAT_argument_one_level_deref,
      0,
      { 0, }
    },
    {
      "getppid",
      CFAT_libc_default_attr &
      ~(CFAT_exposes_argument_address_to_return |CFAT_exposes_argument_address_to_globals)
      | CFAT_argument_indirectly_read | CFAT_argument_indirectly_write
      | CFAT_argument_one_level_deref,
      0,
      { 0, }
    },
    {
      "getrlimit",
      CFAT_libc_default_attr &
      ~(CFAT_exposes_argument_address_to_return |CFAT_exposes_argument_address_to_globals)
      | CFAT_argument_indirectly_read | CFAT_argument_indirectly_write
      | CFAT_argument_one_level_deref,
      0,
      { 0, }
    },
    {
      "getrusage",
      CFAT_libc_default_attr &
      ~(CFAT_exposes_argument_address_to_return |CFAT_exposes_argument_address_to_globals)
      | CFAT_argument_indirectly_read | CFAT_argument_indirectly_write
      | CFAT_argument_one_level_deref,
      0,
      { 0, }
    },
    {
      "gets",
      CFAT_libc_default_attr &
      ~(CFAT_exposes_argument_address_to_return |CFAT_exposes_argument_address_to_globals)
      | CFAT_argument_indirectly_read | CFAT_argument_indirectly_write
      | CFAT_argument_one_level_deref  |CFAT_exposes_argument_address_to_return,
      1,
      { 
        CPA_one_level_write | CPA_one_level_read | CPA_exposed_to_return,
      }
    },
    {
      "gettimeofday",
      CFAT_libc_default_attr &
      ~(CFAT_exposes_argument_address_to_return |CFAT_exposes_argument_address_to_globals)
      | CFAT_argument_indirectly_read | CFAT_argument_indirectly_write
      | CFAT_argument_one_level_deref,
      0,
      { 0, }
    },
    {
      "getuid",
      CFAT_libc_default_attr &
      ~(CFAT_exposes_argument_address_to_return |CFAT_exposes_argument_address_to_globals)
      | CFAT_argument_indirectly_read | CFAT_argument_indirectly_write
      | CFAT_argument_one_level_deref,
      0,
      { 0, }
    },
    {
      "gmtime",
      CFAT_libc_default_attr &
      ~(CFAT_exposes_argument_address_to_return |CFAT_exposes_argument_address_to_globals)
      | CFAT_argument_indirectly_read | CFAT_argument_indirectly_write
      | CFAT_argument_one_level_deref,
      0,
      { 0, }
    },
    {
      "_IO_getc",
      CFAT_libc_default_attr &
      ~(CFAT_exposes_argument_address_to_return |CFAT_exposes_argument_address_to_globals)
      | CFAT_argument_indirectly_read | CFAT_argument_indirectly_write
      | CFAT_argument_one_level_deref,
      1,
      { CPA_one_level_read |CPA_two_level_read |CPA_one_level_write |CPA_two_level_write, }
    },
    {
      "_IO_putc",
      CFAT_libc_default_attr &
      ~(CFAT_exposes_argument_address_to_return |CFAT_exposes_argument_address_to_globals)
      | CFAT_argument_indirectly_read | CFAT_argument_indirectly_write
      | CFAT_argument_one_level_deref,
      2,
      { 
        0,
        CPA_one_level_read |CPA_two_level_read |CPA_one_level_write |CPA_two_level_write,
      }
    },
    {
      "ioctl",
      CFAT_libc_default_attr &
      ~(CFAT_exposes_argument_address_to_return |CFAT_exposes_argument_address_to_globals)
      | CFAT_argument_indirectly_read | CFAT_argument_indirectly_write
      | CFAT_argument_one_level_deref,
      0,
      { 0, }
    },
    {
      "_isalnum",
      CFAT_default_attr & (~CFAT_pure_call_attr_mask) | CFAT_hidden_data_read
      | CFAT_libc_globals_read | CFAT_is_libc_entry,
      0,
      {
        0, 
      }
    },
    {
      "isalnum",
      CFAT_default_attr & (~CFAT_pure_call_attr_mask) | CFAT_hidden_data_read
      | CFAT_libc_globals_read | CFAT_is_libc_entry,
      0,
      {
        0,
      }
    },
    {
      "_isalpha",
      CFAT_default_attr & (~CFAT_pure_call_attr_mask) | CFAT_hidden_data_read
      | CFAT_libc_globals_read | CFAT_is_libc_entry,
      0,
      {
        0,
      }
    },
    {
      "isalpha",
      CFAT_default_attr & (~CFAT_pure_call_attr_mask) | CFAT_hidden_data_read
      | CFAT_libc_globals_read | CFAT_is_libc_entry,
      0,
      {
        0,
      }
    },
    {
      "isascii",
      CFAT_default_attr & (~CFAT_pure_call_attr_mask) | CFAT_hidden_data_read
      | CFAT_libc_globals_read | CFAT_is_libc_entry,
      0,
      {
        0,
      }
    },
    {
      "isatty",
      CFAT_default_attr & (~CFAT_pure_call_attr_mask) | CFAT_hidden_data_read
      | CFAT_libc_globals_read | CFAT_is_libc_entry,
      0,
      {
        0,
      }
    },
    {
      "_isblank",
      CFAT_default_attr & (~CFAT_pure_call_attr_mask) | CFAT_hidden_data_read
      | CFAT_libc_globals_read | CFAT_is_libc_entry,
      0,
      {
        0,
      }
    },
    {
      "isblank",
      CFAT_default_attr & (~CFAT_pure_call_attr_mask) | CFAT_hidden_data_read
      | CFAT_libc_globals_read | CFAT_is_libc_entry,
      0,
      {
        0,
      }
    },
    {
      "_iscntrl",
      CFAT_default_attr & (~CFAT_pure_call_attr_mask) | CFAT_hidden_data_read
      | CFAT_libc_globals_read | CFAT_is_libc_entry,
      0,
      {
        0,
      }
    },
    {
      "_isdigit",
      CFAT_default_attr & (~CFAT_pure_call_attr_mask) | CFAT_hidden_data_read
      | CFAT_libc_globals_read | CFAT_is_libc_entry,
      0,
      {
        0,
      }
    },
    {
      "isdigit",
      CFAT_default_attr & (~CFAT_pure_call_attr_mask) | CFAT_hidden_data_read
      | CFAT_libc_globals_read | CFAT_is_libc_entry,
      0,
      {
        0,
      }
    },
    {
      "_isgraph",
      CFAT_default_attr & (~CFAT_pure_call_attr_mask) | CFAT_hidden_data_read
      | CFAT_libc_globals_read | CFAT_is_libc_entry,
      0,
      {
        0,
      }
    },
    {
      "_islower",
      CFAT_default_attr & (~CFAT_pure_call_attr_mask) | CFAT_hidden_data_read
      | CFAT_libc_globals_read | CFAT_is_libc_entry,
      0,
      {
        0,
      }
    },
    {
      "islower",
      CFAT_default_attr & (~CFAT_pure_call_attr_mask) | CFAT_hidden_data_read
      | CFAT_libc_globals_read | CFAT_is_libc_entry,
      0,
      {
        0,
      }
    },
    {
      "_isprint",
      CFAT_default_attr & (~CFAT_pure_call_attr_mask) | CFAT_hidden_data_read
      | CFAT_libc_globals_read | CFAT_is_libc_entry,
      0,
      {
        0,
      }
    },
    {
      "_ispunct",
      CFAT_default_attr & (~CFAT_pure_call_attr_mask) | CFAT_hidden_data_read
      | CFAT_libc_globals_read | CFAT_is_libc_entry,
      0,
      {
        0,
      }
    },
    {
      "_isspace",
      CFAT_default_attr & (~CFAT_pure_call_attr_mask) | CFAT_hidden_data_read
      | CFAT_libc_globals_read | CFAT_is_libc_entry,
      0,
      {
        0,
      }
    },
    {
      "isspace",
      CFAT_default_attr & (~CFAT_pure_call_attr_mask) | CFAT_hidden_data_read
      | CFAT_libc_globals_read | CFAT_is_libc_entry,
      0,
      {
        0,
      }
    },
    {
      "_isupper",
      CFAT_default_attr & (~CFAT_pure_call_attr_mask) | CFAT_hidden_data_read
      | CFAT_libc_globals_read | CFAT_is_libc_entry,
      0,
      {
        0,
      }
    },
    {
      "_isxdigit",
      CFAT_default_attr & (~CFAT_pure_call_attr_mask) | CFAT_hidden_data_read
      | CFAT_libc_globals_read | CFAT_is_libc_entry,
      0,
      {
        0,
      }
    },
    {
      "isxdigit",
      CFAT_default_attr & (~CFAT_pure_call_attr_mask) | CFAT_hidden_data_read
      | CFAT_libc_globals_read | CFAT_is_libc_entry,
      0,
      {
        0,
      }
    },
    {
      "_iswalnum",  /* Equivalent in side effect behavior to _isalnum */
      CFAT_default_attr & (~CFAT_pure_call_attr_mask) | CFAT_hidden_data_read
      | CFAT_libc_globals_read | CFAT_is_libc_entry,
      0,
      {
        0, 
      }
    },
    {
      "iswalnum",  /* Equivalent in side effect behavior to isalnum */
      CFAT_default_attr & (~CFAT_pure_call_attr_mask) | CFAT_hidden_data_read
      | CFAT_libc_globals_read | CFAT_is_libc_entry,
      0,
      {
        0,
      }
    },
    {
      "_iswalpha",  /* Equivalent in side effect behavior to _isalpha */
      CFAT_default_attr & (~CFAT_pure_call_attr_mask) | CFAT_hidden_data_read
      | CFAT_libc_globals_read | CFAT_is_libc_entry,
      0,
      {
        0,
      }
    },
    {
      "iswalpha",  /* Equivalent in side effect behavior to isalpha */
      CFAT_default_attr & (~CFAT_pure_call_attr_mask) | CFAT_hidden_data_read
      | CFAT_libc_globals_read | CFAT_is_libc_entry,
      0,
      {
        0,
      }
    },
    {
      "_iswblank", /* Equivalent in side effect behavior to isblank */
      CFAT_default_attr & (~CFAT_pure_call_attr_mask) | CFAT_hidden_data_read
      | CFAT_libc_globals_read | CFAT_is_libc_entry,
      0,
      {
        0,
      }
    },
    {
      "iswblank", /* Equivalent in side effect behavior to isblank */
      CFAT_default_attr & (~CFAT_pure_call_attr_mask) | CFAT_hidden_data_read
      | CFAT_libc_globals_read | CFAT_is_libc_entry,
      0,
      {
        0,
      }
    },
    {
      "_iswdigit",  /* Equivalent in side effect behavior to _isdigit */
      CFAT_default_attr & (~CFAT_pure_call_attr_mask) | CFAT_hidden_data_read
      | CFAT_libc_globals_read | CFAT_is_libc_entry,
      0,
      {
        0,
      }
    },
    {
      "iswdigit",  /* Equivalent in side effect behavior to isdigit */
      CFAT_default_attr & (~CFAT_pure_call_attr_mask) | CFAT_hidden_data_read
      | CFAT_libc_globals_read | CFAT_is_libc_entry,
      0,
      {
        0,
      }
    },
    {
      "iswgraph",  /* Equivalent in side effect behavior to _isgraph */
      CFAT_default_attr & (~CFAT_pure_call_attr_mask) | CFAT_hidden_data_read
      | CFAT_libc_globals_read | CFAT_is_libc_entry,
      0,
      {
        0,
      }
    },
    {
      "_iswgraph",  /* Equivalent in side effect behavior to _isgrapha */
      CFAT_default_attr & (~CFAT_pure_call_attr_mask) | CFAT_hidden_data_read
      | CFAT_libc_globals_read | CFAT_is_libc_entry,
      0,
      {
        0,
      }
    },
    {
      "_iswlower",  /* Equivalent in side effect behavior to _islower */
      CFAT_default_attr & (~CFAT_pure_call_attr_mask) | CFAT_hidden_data_read
      | CFAT_libc_globals_read | CFAT_is_libc_entry,
      0,
      {
        0,
      }
    },
    {
      "iswlower",  /* Equivalent in side effect behavior to islower */
      CFAT_default_attr & (~CFAT_pure_call_attr_mask) | CFAT_hidden_data_read
      | CFAT_libc_globals_read | CFAT_is_libc_entry,
      0,
      {
        0,
      }
    },
    {
      "iswprint",  /* Equivalent in side effect behavior to _isprint */
      CFAT_default_attr & (~CFAT_pure_call_attr_mask) | CFAT_hidden_data_read
      | CFAT_libc_globals_read | CFAT_is_libc_entry,
      0,
      {
        0,
      }
    },
    {
      "_iswprint",  /* Equivalent in side effect behavior to _isprint */
      CFAT_default_attr & (~CFAT_pure_call_attr_mask) | CFAT_hidden_data_read
      | CFAT_libc_globals_read | CFAT_is_libc_entry,
      0,
      {
        0,
      }
    },
    {
      "iswpunct",  /* Equivalent in side effect behavior to _ispunct */
      CFAT_default_attr & (~CFAT_pure_call_attr_mask) | CFAT_hidden_data_read
      | CFAT_libc_globals_read | CFAT_is_libc_entry,
      0,
      {
        0,
      }
    },
    {
      "_iswpunct",  /* Equivalent in side effect behavior to _ispunct */
      CFAT_default_attr & (~CFAT_pure_call_attr_mask) | CFAT_hidden_data_read
      | CFAT_libc_globals_read | CFAT_is_libc_entry,
      0,
      {
        0,
      }
    },
    {
      "iswspace",
      CFAT_default_attr & (~CFAT_pure_call_attr_mask) | CFAT_hidden_data_read
      | CFAT_libc_globals_read | CFAT_is_libc_entry,
      0,
      {
        0,
      }
    },
    {
      "iswupper",  /* Equivalent in side effect behavior to _isupper */
      CFAT_default_attr & (~CFAT_pure_call_attr_mask) | CFAT_hidden_data_read
      | CFAT_libc_globals_read | CFAT_is_libc_entry,
      0,
      {
        0,
      }
    },
    {
      "_iswupper",  /* Equivalent in side effect behavior to _isupper */
      CFAT_default_attr & (~CFAT_pure_call_attr_mask) | CFAT_hidden_data_read
      | CFAT_libc_globals_read | CFAT_is_libc_entry,
      0,
      {
        0,
      }
    },
    {
      "_iswxdigit",  /* Equivalent in side effect behavior to _isxdigit */
      CFAT_default_attr & (~CFAT_pure_call_attr_mask) | CFAT_hidden_data_read
      | CFAT_libc_globals_read | CFAT_is_libc_entry,
      0,
      {
        0,
      }
    },
    {
      "iswxdigit",  /* Equivalent in side effect behavior to isxdigit */
      CFAT_default_attr & (~CFAT_pure_call_attr_mask) | CFAT_hidden_data_read
      | CFAT_libc_globals_read | CFAT_is_libc_entry,
      0,
      {
        0,
      }
    },
    {
      "kill",
      CFAT_libc_default_attr &
      ~(CFAT_exposes_argument_address_to_return |CFAT_exposes_argument_address_to_globals)
      | CFAT_argument_indirectly_read | CFAT_argument_indirectly_write
      | CFAT_argument_one_level_deref,
      0,
      { 0, }
    },
    {
      "ldexp",
      CFAT_default_attr & (~CFAT_pure_call_attr_mask),
      0,
      { 0, }
    },
    {
      "ldexpf",
      CFAT_default_attr & (~CFAT_pure_call_attr_mask),
      0,
      { 0, }
    },
    {
      "ldexpl",
      CFAT_default_attr & (~CFAT_pure_call_attr_mask),
      0,
      { 0, }
    },
    {
      "ldexpw",
      CFAT_default_attr & (~CFAT_pure_call_attr_mask),
      0,
      { 0, }
    },
    {
      "ldexpq",
      CFAT_default_attr & (~CFAT_pure_call_attr_mask),
      0,
      { 0, }
    },
    {
      "ldiv",
      CFAT_default_attr & (~CFAT_pure_call_attr_mask),
      0,
      { 0, }
    },
    {
      "link",
      CFAT_libc_default_attr &
      ~(CFAT_exposes_argument_address_to_return |CFAT_exposes_argument_address_to_globals)
      | CFAT_argument_indirectly_read | CFAT_argument_indirectly_write
      | CFAT_argument_one_level_deref,
      0,
      { 0, }
    },
    {
      "localeconv",
      CFAT_libc_default_attr &
      ~(CFAT_exposes_argument_address_to_return |CFAT_exposes_argument_address_to_globals)
      | CFAT_argument_indirectly_read | CFAT_argument_indirectly_write
      | CFAT_argument_one_level_deref,
      0,
      { 0, }
    },
    {
      "localtime",
      CFAT_libc_default_attr &
      ~(CFAT_exposes_argument_address_to_return |CFAT_exposes_argument_address_to_globals)
      | CFAT_argument_indirectly_read | CFAT_argument_indirectly_write
      | CFAT_argument_one_level_deref,
      0,
      { 0, }
    },
    {
      "log",
      CFAT_default_attr & (~CFAT_pure_call_attr_mask),
      0,
      { 0, }
    },
    {
      "log10",
      CFAT_default_attr & (~CFAT_pure_call_attr_mask),
      0,
      { 0, }
    },
    {
      "log10f",
      CFAT_default_attr & (~CFAT_pure_call_attr_mask),
      0,
      { 0, }
    },
    {
      "log10l",
      CFAT_default_attr & (~CFAT_pure_call_attr_mask),
      0,
      { 0, }
    },
    {
      "logf",
      CFAT_default_attr & (~CFAT_pure_call_attr_mask),
      0,
      { 0, }
    },
    {
      "logl",
      CFAT_default_attr & (~CFAT_pure_call_attr_mask),
      0,
      { 0, }
    },
    {
      "longjmp",
      CFAT_libc_default_attr &
      ~(CFAT_exposes_argument_address_to_return |CFAT_exposes_argument_address_to_globals)
      | CFAT_argument_indirectly_read | CFAT_argument_indirectly_write
      | CFAT_argument_one_level_deref,
      0,
      { 0, }
    },
    {
      "lseek",
      CFAT_libc_default_attr &
      ~(CFAT_exposes_argument_address_to_return |CFAT_exposes_argument_address_to_globals)
      | CFAT_argument_indirectly_read | CFAT_argument_indirectly_write
      | CFAT_argument_one_level_deref,
      0,
      { 0, }
    },
    {
      "__lseek64",
      CFAT_libc_default_attr &
      ~(CFAT_exposes_argument_address_to_return |CFAT_exposes_argument_address_to_globals)
      | CFAT_argument_indirectly_read | CFAT_argument_indirectly_write
      | CFAT_argument_one_level_deref,
      0,
      { 0, }
    },
    {
      "malloc",
      CFAT_libc_default_attr &
      ~(CFAT_exposes_argument_address_to_return |
        CFAT_exposes_argument_address_to_globals)
      | CFAT_allocates_heap_memory | CFAT_returns_heap_memory ,
      1,
      { CPA_no_ptr_deref_and_expose },
    },
    {
      "mblen",
      CFAT_libc_default_attr &
      ~(CFAT_exposes_argument_address_to_return |CFAT_exposes_argument_address_to_globals)
      | CFAT_argument_indirectly_read | CFAT_argument_one_level_deref,
      2,
      {CPA_one_level_read, CPA_no_ptr_deref_and_expose }
    },
    {
      "__mbrlen",
      CFAT_libc_default_attr &
      ~(CFAT_exposes_argument_address_to_return |CFAT_exposes_argument_address_to_globals)
      | CFAT_argument_indirectly_read | CFAT_argument_one_level_deref,
      3,
      {CPA_one_level_read, CPA_no_ptr_deref_and_expose, 
       CPA_one_level_read | CPA_one_level_write }  
    },
    {
      "mbrlen",
      CFAT_libc_default_attr &
      ~(CFAT_exposes_argument_address_to_return |CFAT_exposes_argument_address_to_globals)
      | CFAT_argument_indirectly_read | CFAT_argument_one_level_deref,
      3,
      {CPA_one_level_read, CPA_no_ptr_deref_and_expose, 
       CPA_one_level_read | CPA_one_level_write }  
    },
    {
      "__mbrtowc",
      CFAT_libc_default_attr &
      ~(CFAT_exposes_argument_address_to_return |CFAT_exposes_argument_address_to_globals)
      | CFAT_argument_indirectly_read | CFAT_argument_indirectly_write
      | CFAT_argument_one_level_deref,
      4,
      { CPA_one_level_write, CPA_one_level_read, CPA_no_ptr_deref_and_expose, 
        CPA_one_level_read|CPA_one_level_write } 
    },
    {
      "mbrtowc",
      CFAT_libc_default_attr &
      ~(CFAT_exposes_argument_address_to_return |CFAT_exposes_argument_address_to_globals)
      | CFAT_argument_indirectly_read | CFAT_argument_indirectly_write
      | CFAT_argument_one_level_deref,
      4,
      { CPA_one_level_write, CPA_one_level_read, CPA_no_ptr_deref_and_expose, 
        CPA_one_level_read|CPA_one_level_write } 
    },
    {
      "__mbsrtowcs",
      CFAT_libc_default_attr &
      ~(CFAT_exposes_argument_address_to_return |CFAT_exposes_argument_address_to_globals)
      | CFAT_argument_indirectly_read | CFAT_argument_indirectly_write
      | CFAT_argument_one_level_deref,
      4,
      { CPA_one_level_write, CPA_one_level_read, CPA_no_ptr_deref_and_expose,
        CPA_one_level_read | CPA_one_level_write }  
    },
    {
      "mbsrtowcs",
      CFAT_libc_default_attr &
      ~(CFAT_exposes_argument_address_to_return |CFAT_exposes_argument_address_to_globals)
      | CFAT_argument_indirectly_read | CFAT_argument_indirectly_write
      | CFAT_argument_one_level_deref,
      4,
      { CPA_one_level_write, CPA_one_level_read, CPA_no_ptr_deref_and_expose,
        CPA_one_level_read | CPA_one_level_write }  
    },
    {
      "mbstowcs",
      CFAT_libc_default_attr &
      ~(CFAT_exposes_argument_address_to_return |CFAT_exposes_argument_address_to_globals)
      | CFAT_argument_indirectly_read | CFAT_argument_indirectly_write
      | CFAT_argument_one_level_deref,
      3,
      { CPA_one_level_write, CPA_one_level_read, CPA_no_ptr_deref_and_expose }
    },
    {
      "mbtowc",
      CFAT_libc_default_attr &
      ~(CFAT_exposes_argument_address_to_return |CFAT_exposes_argument_address_to_globals)
      | CFAT_argument_indirectly_read | CFAT_argument_indirectly_write
      | CFAT_argument_one_level_deref,
      3,
      { CPA_one_level_write, CPA_one_level_read, CPA_no_ptr_deref_and_expose }
    },
    {
      "memccpy",
      CFAT_default_attr & ~CFAT_pure_call_attr_mask | CFAT_is_libc_entry 
      |CFAT_argument_indirectly_read | CFAT_argument_indirectly_write 
      |CFAT_argument_one_level_deref |CFAT_exposes_argument_address_to_return,
      4,
      {
        CPA_one_level_write | CPA_exposed_to_return,
        CPA_one_level_read, 
        CPA_no_ptr_deref_and_expose,
        CPA_no_ptr_deref_and_expose
      }
    },
    {
      "memchr",
      CFAT_default_attr & ~CFAT_pure_call_attr_mask | CFAT_is_libc_entry 
      |CFAT_argument_indirectly_read 
      |CFAT_argument_one_level_deref |CFAT_exposes_argument_address_to_return,
      3,
      {
        CPA_one_level_read |CPA_exposed_to_return, 
        CPA_no_ptr_deref_and_expose,
        CPA_no_ptr_deref_and_expose
      }
    },
    {
      "memcmp",
      CFAT_default_attr & ~CFAT_pure_call_attr_mask | CFAT_is_libc_entry 
      |CFAT_argument_indirectly_read |CFAT_argument_one_level_deref,
      3,
      {
        CPA_one_level_read , 
        CPA_one_level_read , 
        CPA_no_ptr_deref_and_expose
      }
    },
    {
      "memcpy",
      CFAT_default_attr & ~CFAT_pure_call_attr_mask | CFAT_is_libc_entry 
      |CFAT_argument_indirectly_read | CFAT_argument_indirectly_write 
      |CFAT_argument_one_level_deref |CFAT_exposes_argument_address_to_return,
      3,
      {
        CPA_one_level_write | CPA_exposed_to_return,
        CPA_one_level_read , 
        CPA_no_ptr_deref_and_expose
      }
    },
    {
      "memmove",
      CFAT_default_attr & ~CFAT_pure_call_attr_mask | CFAT_is_libc_entry 
      |CFAT_argument_indirectly_read | CFAT_argument_indirectly_write 
      |CFAT_argument_one_level_deref |CFAT_exposes_argument_address_to_return,
      3,
      {
        CPA_one_level_write | CPA_exposed_to_return,
        CPA_one_level_read , 
        CPA_no_ptr_deref_and_expose
      }
    },
    {
      "memset",
      CFAT_default_attr & ~CFAT_pure_call_attr_mask | CFAT_is_libc_entry 
      | CFAT_argument_indirectly_write 
      | CFAT_exposes_argument_address_to_return 
      | CFAT_argument_one_level_deref,
      2,
      {
        CPA_one_level_write | CPA_exposed_to_return ,
        CPA_no_ptr_deref_and_expose
      }
    },
    {
      "mkdir",
      CFAT_libc_default_attr &
      ~(CFAT_exposes_argument_address_to_return |CFAT_exposes_argument_address_to_globals)
      | CFAT_argument_indirectly_read | CFAT_argument_indirectly_write
      | CFAT_argument_one_level_deref,
      0,
      { 0, }
    },
    {
      "mkstemp",
      CFAT_libc_default_attr & 
      ~(CFAT_exposes_argument_address_to_globals | CFAT_exposes_argument_address_to_return)
      | CFAT_argument_indirectly_read | CFAT_argument_indirectly_write
      | CFAT_argument_one_level_deref ,
      1,
      {  CPA_one_level_read|CPA_one_level_write |CPA_one_level_read, }
    },
    {
      "mktemp",
      CFAT_libc_default_attr & ~(CFAT_exposes_argument_address_to_globals)
      | CFAT_argument_indirectly_read | CFAT_argument_indirectly_write
      | CFAT_argument_one_level_deref |CFAT_exposes_argument_address_to_return,
      1,
      {  CPA_one_level_read|CPA_one_level_write |CPA_one_level_read |CPA_exposed_to_return, }
    },
    {
      "modf",
      CFAT_default_attr & (~CFAT_pure_call_attr_mask)
      | CFAT_argument_indirectly_write | CFAT_argument_one_level_deref,
      2,
      { CPA_no_ptr_deref_and_expose, CPA_one_level_write }
    },
    {
      "_modf",
      CFAT_default_attr & (~CFAT_pure_call_attr_mask)
      | CFAT_argument_indirectly_write | CFAT_argument_one_level_deref,
      2,
      { CPA_no_ptr_deref_and_expose, CPA_one_level_write }
    },
    {
      "nice",
      CFAT_libc_default_attr &
      ~(CFAT_exposes_argument_address_to_return |CFAT_exposes_argument_address_to_globals)
      | CFAT_argument_indirectly_read | CFAT_argument_indirectly_write
      | CFAT_argument_one_level_deref,
      0,
      { 0, }
    },
    {
      "open",
      CFAT_libc_default_attr &
      ~(CFAT_exposes_argument_address_to_return |CFAT_exposes_argument_address_to_globals)
      | CFAT_argument_indirectly_read | CFAT_argument_indirectly_write
      | CFAT_argument_one_level_deref,
      0,
      { 0, }
    },
    {
      "opendir",
      CFAT_libc_default_attr &
      ~(CFAT_exposes_argument_address_to_return |CFAT_exposes_argument_address_to_globals)
      | CFAT_argument_indirectly_read | CFAT_argument_indirectly_write
      | CFAT_argument_one_level_deref
      | CFAT_allocates_heap_memory | CFAT_returns_heap_memory,
      0,
      { 0, }
    },
    {
      "__open64",
      CFAT_libc_default_attr &
      ~(CFAT_exposes_argument_address_to_return |CFAT_exposes_argument_address_to_globals)
      | CFAT_argument_indirectly_read | CFAT_argument_indirectly_write
      | CFAT_argument_one_level_deref,
      0,
      { 0, }
    },
    {
      "pclose",
      CFAT_libc_default_attr &
      ~(CFAT_exposes_argument_address_to_return |CFAT_exposes_argument_address_to_globals)
      | CFAT_argument_indirectly_read | CFAT_argument_indirectly_write
      | CFAT_argument_one_level_deref,
      0,
      { 0, }
    },
    {
      "perror",
      CFAT_libc_default_attr &
      ~(CFAT_exposes_argument_address_to_return |CFAT_exposes_argument_address_to_globals)
      | CFAT_argument_indirectly_read | CFAT_argument_indirectly_write
      | CFAT_argument_one_level_deref,
      0,
      { 0, }
    },
    {
      "pipe",
      CFAT_libc_default_attr &
      ~(CFAT_exposes_argument_address_to_return |CFAT_exposes_argument_address_to_globals)
      | CFAT_argument_indirectly_read | CFAT_argument_indirectly_write
      | CFAT_argument_one_level_deref,
      0,
      { 0, }
    },
    {
      "popen",
      CFAT_libc_default_attr &
      ~(CFAT_exposes_argument_address_to_return |CFAT_exposes_argument_address_to_globals)
      | CFAT_argument_indirectly_read | CFAT_argument_indirectly_write
      | CFAT_argument_one_level_deref,
      0,
      { 0, }
    },
    {
      "pow",
      CFAT_default_attr & (~CFAT_pure_call_attr_mask),
      0,
      { 0, }
    },
    {
      "powf",
      CFAT_default_attr & (~CFAT_pure_call_attr_mask),
      0,
      { 0, }
    },
    {
      "powl",
      CFAT_default_attr & (~CFAT_pure_call_attr_mask),
      0,
      { 0, }
    },
    {
      "poww",
      CFAT_default_attr & (~CFAT_pure_call_attr_mask),
      0,
      { 0, }
    },
    {
      "pread",
      CFAT_libc_default_attr &
      ~(CFAT_exposes_argument_address_to_return |CFAT_exposes_argument_address_to_globals)
      | CFAT_argument_indirectly_read | CFAT_argument_indirectly_write
      | CFAT_argument_one_level_deref,
      0,
      { 0, }
    },
    {
      "printf",
      CFAT_libc_default_attr &
      ~(CFAT_exposes_argument_address_to_return |CFAT_exposes_argument_address_to_globals)
      | CFAT_argument_indirectly_read 
      | CFAT_argument_one_level_deref | CFAT_is_printf_like | CFAT_has_format_string,
      1,
      { CPA_one_level_read | CPA_is_format_string }
    },
    {
      "pstat",
      CFAT_libc_default_attr &
      ~(CFAT_exposes_argument_address_to_return |CFAT_exposes_argument_address_to_globals)
      | CFAT_argument_indirectly_read | CFAT_argument_indirectly_write
      | CFAT_argument_one_level_deref,
      0,
      { 0, }
    },
    {
      "putc",
      CFAT_libc_default_attr &
      ~(CFAT_exposes_argument_address_to_return |CFAT_exposes_argument_address_to_globals)
      | CFAT_argument_indirectly_read | CFAT_argument_indirectly_write
      | CFAT_argument_one_level_deref,
      2,
      { 
        0,
        CPA_one_level_read |CPA_two_level_read |CPA_one_level_write |CPA_two_level_write,
      }
    },
    {
      "putchar",
      CFAT_libc_default_attr &
      ~(CFAT_exposes_argument_address_to_return |CFAT_exposes_argument_address_to_globals)
      | CFAT_argument_indirectly_read | CFAT_argument_indirectly_write
      | CFAT_argument_one_level_deref,
      0,
      { 0, }
    },
    {
      "putenv",
      CFAT_libc_default_attr &
      ~(CFAT_exposes_argument_address_to_return |CFAT_exposes_argument_address_to_globals)
      | CFAT_argument_indirectly_read | CFAT_argument_indirectly_write
      | CFAT_argument_one_level_deref,
      0,
      { 0, }
    },
    {
      "puts",
      CFAT_libc_default_attr &
      ~(CFAT_exposes_argument_address_to_return |CFAT_exposes_argument_address_to_globals)
      | CFAT_argument_indirectly_read | CFAT_argument_one_level_deref,
      1,
      { 
        CPA_one_level_read ,
      }
    },
    {
      "pwrite",
      CFAT_libc_default_attr &
      ~(CFAT_exposes_argument_address_to_return |CFAT_exposes_argument_address_to_globals)
      | CFAT_argument_indirectly_read | CFAT_argument_indirectly_write
      | CFAT_argument_one_level_deref,
      0,
      { 0, }
    },
    {
      "qsort",
      CFAT_default_attr & ~CFAT_exposes_argument_address_to_return ,
      0,
      { 0, }
    },
    {
      "raise",
      CFAT_libc_default_attr &
      ~(CFAT_exposes_argument_address_to_return |CFAT_exposes_argument_address_to_globals)
      | CFAT_argument_indirectly_read | CFAT_argument_indirectly_write
      | CFAT_argument_one_level_deref,
      0,
      { 0, }
    },
    {
      "rand",
      CFAT_libc_default_attr &
      ~(CFAT_exposes_argument_address_to_return |CFAT_exposes_argument_address_to_globals)
      | CFAT_argument_indirectly_read | CFAT_argument_indirectly_write
      | CFAT_argument_one_level_deref,
      0,
      { 0, }
    },
    {
      "__rawmemchr",
      CFAT_default_attr & ~CFAT_pure_call_attr_mask | CFAT_is_libc_entry 
      |CFAT_argument_indirectly_read 
      |CFAT_argument_one_level_deref |CFAT_exposes_argument_address_to_return,
      2,
      {
        CPA_one_level_read |CPA_exposed_to_return, 
        CPA_no_ptr_deref_and_expose
      }
    },
    {
      "read",
      CFAT_libc_default_attr &
      ~(CFAT_exposes_argument_address_to_return |CFAT_exposes_argument_address_to_globals)
      | CFAT_argument_indirectly_read | CFAT_argument_indirectly_write
      | CFAT_argument_one_level_deref,
      0,
      { 0, }
    },
    {
      "readdir",
      CFAT_libc_default_attr &
      ~(CFAT_exposes_argument_address_to_return |CFAT_exposes_argument_address_to_globals)
      | CFAT_argument_indirectly_read | CFAT_argument_indirectly_write
      | CFAT_argument_one_level_deref,
      0,
      { 0, }
    },
    {
      "readv",
      CFAT_libc_default_attr &
      ~(CFAT_exposes_argument_address_to_return |CFAT_exposes_argument_address_to_globals)
      | CFAT_argument_indirectly_read | CFAT_argument_indirectly_write
      | CFAT_argument_one_level_deref,
      0,
      { 0, }
    },
    {
      "realloc",
      CFAT_libc_default_attr &
      ~(CFAT_exposes_argument_address_to_return |
        CFAT_exposes_argument_address_to_globals)
      | CFAT_argument_indirectly_read | CFAT_argument_indirectly_write 
      | CFAT_argument_one_level_deref |CFAT_allocates_heap_memory | 
      CFAT_returns_heap_memory,
      2,
      { CPA_one_level_read|CPA_one_level_write, CPA_no_ptr_deref_and_expose },
    },
    {
      "realpath",
      CFAT_libc_default_attr &
      ~(CFAT_exposes_argument_address_to_globals)
      | CFAT_exposes_argument_address_to_return
      | CFAT_argument_indirectly_read | CFAT_argument_indirectly_write
      | CFAT_argument_one_level_deref,
      2,
      { CPA_one_level_read,
        CPA_one_level_write | CPA_exposed_to_return,  }
    },
    {
      "remove",
      CFAT_libc_default_attr &
      ~(CFAT_exposes_argument_address_to_return |CFAT_exposes_argument_address_to_globals)
      | CFAT_argument_indirectly_read | CFAT_argument_indirectly_write
      | CFAT_argument_one_level_deref,
      0,
      { 0, }
    },
    {
      "rename",
      CFAT_libc_default_attr &
      ~(CFAT_exposes_argument_address_to_return |CFAT_exposes_argument_address_to_globals)
      | CFAT_argument_indirectly_read | CFAT_argument_indirectly_write
      | CFAT_argument_one_level_deref,
      0,
      { 0, }
    },
    {
      "rewind",
      CFAT_libc_default_attr &
      ~(CFAT_exposes_argument_address_to_return |CFAT_exposes_argument_address_to_globals)
      | CFAT_argument_indirectly_read | CFAT_argument_indirectly_write
      | CFAT_argument_one_level_deref,
      0,
      { 0, }
    },
    {
      "rmdir",
      CFAT_libc_default_attr &
      ~(CFAT_exposes_argument_address_to_return |CFAT_exposes_argument_address_to_globals)
      | CFAT_argument_indirectly_read | CFAT_argument_indirectly_write
      | CFAT_argument_one_level_deref,
      0,
      { 0, }
    },
    {
      "rsqrt",
      CFAT_default_attr & (~CFAT_pure_call_attr_mask),
      0,
      { 0, }
    },
    {
      "rsqrtf",
      CFAT_default_attr & (~CFAT_pure_call_attr_mask),
      0,
      { 0, }
    },
    {
      "rsqrtl",
      CFAT_default_attr & (~CFAT_pure_call_attr_mask),
      0,
      { 0, }
    },
    {
      "sbrk",
      CFAT_libc_default_attr &
      ~(CFAT_exposes_argument_address_to_return |CFAT_exposes_argument_address_to_globals)
      | CFAT_argument_indirectly_read | CFAT_argument_indirectly_write
      | CFAT_argument_one_level_deref,
      0,
      { 0, }
    },
    {
      "scanf",
      CFAT_libc_default_attr &
      ~(CFAT_exposes_argument_address_to_return |CFAT_exposes_argument_address_to_globals)
      | CFAT_argument_indirectly_read | CFAT_argument_indirectly_write
      | CFAT_argument_one_level_deref,
      1,
      { CPA_one_level_read |CPA_is_format_string, }
    },
    {
      "seekdir",
      CFAT_libc_default_attr &
      ~(CFAT_exposes_argument_address_to_return |CFAT_exposes_argument_address_to_globals)
      | CFAT_argument_indirectly_read | CFAT_argument_indirectly_write
      | CFAT_argument_one_level_deref,
      0,
      { 0, }
    },
    {
      "select",
      CFAT_libc_default_attr &
      ~(CFAT_exposes_argument_address_to_return |CFAT_exposes_argument_address_to_globals)
      | CFAT_argument_indirectly_read | CFAT_argument_indirectly_write
      | CFAT_argument_one_level_deref,
      0,
      { 0, }
    },
    {
      "setbuf",
      CFAT_libc_default_attr &
      ~(CFAT_exposes_argument_address_to_return |CFAT_exposes_argument_address_to_globals)
      | CFAT_argument_indirectly_read | CFAT_argument_indirectly_write
      | CFAT_argument_one_level_deref,
      0,
      { 0, }
    },
    {
      "setgid",
      CFAT_libc_default_attr &
      ~(CFAT_exposes_argument_address_to_return |CFAT_exposes_argument_address_to_globals)
      | CFAT_argument_indirectly_read | CFAT_argument_indirectly_write
      | CFAT_argument_one_level_deref,
      0,
      { 0, }
    },
    {
      "setjmp",
      CFAT_libc_default_attr &
      ~(CFAT_exposes_argument_address_to_return |CFAT_exposes_argument_address_to_globals)
      | CFAT_argument_indirectly_read | CFAT_argument_indirectly_write
      | CFAT_argument_one_level_deref,
      0,
      { 0, }
    },
    {
      "setrlimit",
      CFAT_libc_default_attr &
      ~(CFAT_exposes_argument_address_to_return |CFAT_exposes_argument_address_to_globals)
      | CFAT_argument_indirectly_read | CFAT_argument_indirectly_write
      | CFAT_argument_one_level_deref,
      0,
      { 0, }
    },
    {
      "setuid",
      CFAT_libc_default_attr &
      ~(CFAT_exposes_argument_address_to_return |CFAT_exposes_argument_address_to_globals)
      | CFAT_argument_indirectly_read | CFAT_argument_indirectly_write
      | CFAT_argument_one_level_deref,
      0,
      { 0, }
    },
    {
      "setvbuf",
      CFAT_libc_default_attr &
      ~(CFAT_exposes_argument_address_to_return |CFAT_exposes_argument_address_to_globals)
      | CFAT_argument_indirectly_read | CFAT_argument_indirectly_write
      | CFAT_argument_one_level_deref,
      0,
      { 0, }
    },
    {
      "siglongjmp",
      CFAT_libc_default_attr &
      ~(CFAT_exposes_argument_address_to_return |CFAT_exposes_argument_address_to_globals)
      | CFAT_argument_indirectly_read | CFAT_argument_indirectly_write
      | CFAT_argument_one_level_deref,
      0,
      { 0, }
    },
    {
      "signal",
      CFAT_libc_default_attr &
      ~(CFAT_exposes_argument_address_to_return |CFAT_exposes_argument_address_to_globals)
      | CFAT_argument_indirectly_read | CFAT_argument_indirectly_write
      | CFAT_argument_one_level_deref 
      | CFAT_reads_all_attr | CFAT_writes_all_attr,
      0,
      { 0, }
    },
    {
      "sigsetjmp",
      CFAT_libc_default_attr &
      ~(CFAT_exposes_argument_address_to_return |CFAT_exposes_argument_address_to_globals)
      | CFAT_argument_indirectly_read | CFAT_argument_indirectly_write
      | CFAT_argument_one_level_deref,
      0,
      { 0, }
    },
    {
      "sin",
      CFAT_default_attr & (~CFAT_pure_call_attr_mask),
      0,
      { 0, }
    },
    {
      "sind",
      CFAT_default_attr & (~CFAT_pure_call_attr_mask),
      0,
      { 0, }
    },
    {
      "sindf",
      CFAT_default_attr & (~CFAT_pure_call_attr_mask),
      0,
      { 0, }
    },
    {
      "sindl",
      CFAT_default_attr & (~CFAT_pure_call_attr_mask),
      0,
      { 0, }
    },
    {
      "sindq",
      CFAT_default_attr & (~CFAT_pure_call_attr_mask),
      0,
      { 0, }
    },
    {
      "sindw",
      CFAT_default_attr & (~CFAT_pure_call_attr_mask),
      0,
      { 0, }
    },
    {
      "sinf",
      CFAT_default_attr & (~CFAT_pure_call_attr_mask),
      0,
      { 0, }
    },
    {
      "sinh",
      CFAT_default_attr & (~CFAT_pure_call_attr_mask),
      0,
      { 0, }
    },
    {
      "sinhf",
      CFAT_default_attr & (~CFAT_pure_call_attr_mask),
      0,
      { 0, }
    },
    {
      "sinhl",
      CFAT_default_attr & (~CFAT_pure_call_attr_mask),
      0,
      { 0, }
    },
    {
      "sinl",
      CFAT_default_attr & (~CFAT_pure_call_attr_mask),
      0,
      { 0, }
    },
    {
      "sleep",
      CFAT_libc_default_attr &
      ~(CFAT_exposes_argument_address_to_return |CFAT_exposes_argument_address_to_globals)
      | CFAT_argument_indirectly_read | CFAT_argument_indirectly_write
      | CFAT_argument_one_level_deref,
      0,
      { 0, }
    },
    {
      "snprintf",
      CFAT_libc_default_attr &
      ~(CFAT_exposes_argument_address_to_return |CFAT_exposes_argument_address_to_globals)
      | CFAT_argument_indirectly_read | CFAT_argument_indirectly_write
      | CFAT_argument_one_level_deref,
      3,
      {CPA_one_level_write, 0, CPA_one_level_read}
    },
    {
      "sprintf",
      CFAT_libc_default_attr &
      ~(CFAT_exposes_argument_address_to_return |CFAT_exposes_argument_address_to_globals)
      | CFAT_argument_indirectly_read | CFAT_argument_indirectly_write
      | CFAT_argument_one_level_deref |CFAT_is_printf_like |CFAT_has_format_string,
      2,
      { CPA_one_level_write, CPA_one_level_read |CPA_is_format_string |CPA_is_not_escaping }
    },
    {
      "sqrt",
      CFAT_default_attr & (~CFAT_pure_call_attr_mask),
      0,
      { 0, }
    },
    {
      "sqrtf",
      CFAT_default_attr & (~CFAT_pure_call_attr_mask),
      0,
      { 0, }
    },
    {
      "sqrtl",
      CFAT_default_attr & (~CFAT_pure_call_attr_mask),
      0,
      { 0, }
    },
    {
      "srand",
      CFAT_libc_default_attr &
      ~(CFAT_exposes_argument_address_to_return |CFAT_exposes_argument_address_to_globals)
      | CFAT_argument_indirectly_read | CFAT_argument_indirectly_write
      | CFAT_argument_one_level_deref,
      0,
      { 0, }
    },
    {
      "srand48",
      CFAT_libc_default_attr &
      ~(CFAT_exposes_argument_address_to_return |CFAT_exposes_argument_address_to_globals),
      0,
      { 0, }
    },
    {
      "sscanf",
      CFAT_libc_default_attr &
      ~(CFAT_exposes_argument_address_to_return |CFAT_exposes_argument_address_to_globals)
      | CFAT_argument_indirectly_read | CFAT_argument_indirectly_write
      | CFAT_argument_one_level_deref,
      2,
      { 
        CPA_one_level_read,
        CPA_one_level_read | CPA_is_format_string 
      },
    },
    {
      "stat",
      CFAT_libc_default_attr &
      ~(CFAT_exposes_argument_address_to_return |CFAT_exposes_argument_address_to_globals)
      | CFAT_argument_indirectly_read | CFAT_argument_indirectly_write
      | CFAT_argument_one_level_deref,
      0,
      { 0, }
    },
    {
      "strcasecmp",
      CFAT_default_attr & ~CFAT_pure_call_attr_mask | CFAT_is_libc_entry 
      |CFAT_argument_indirectly_read | CFAT_argument_one_level_deref,
      2,
      {
        CPA_one_level_read,
        CPA_one_level_read
      }
    },
    {
      "strcat",
      CFAT_default_attr & ~CFAT_pure_call_attr_mask | CFAT_is_libc_entry 
      |CFAT_argument_indirectly_read | CFAT_argument_indirectly_write 
      |CFAT_argument_one_level_deref |CFAT_exposes_argument_address_to_return,
      2,
      {
        CPA_one_level_read | CPA_one_level_write | CPA_exposed_to_return,
        CPA_one_level_read 
      }
    },
    {
      "strchr",
      CFAT_default_attr & ~CFAT_pure_call_attr_mask | CFAT_is_libc_entry 
      |CFAT_argument_indirectly_read |CFAT_argument_one_level_deref |
      CFAT_exposes_argument_address_to_return,
      2,
      {
        CPA_one_level_read | CPA_exposed_to_return,
        CPA_no_ptr_deref_and_expose
      }
    },
    {
      "strcmp",
      CFAT_default_attr & ~CFAT_pure_call_attr_mask | CFAT_is_libc_entry 
      |CFAT_argument_indirectly_read |CFAT_argument_one_level_deref,
      2,
      {
        CPA_one_level_read,
        CPA_one_level_read
      }
    },
    {
      "strcpy",
      CFAT_default_attr & ~CFAT_pure_call_attr_mask | CFAT_is_libc_entry 
      |CFAT_argument_indirectly_read | CFAT_argument_indirectly_write 
      |CFAT_argument_one_level_deref |CFAT_exposes_argument_address_to_return,
      2,
      {
        CPA_one_level_write |CPA_exposed_to_return,
        CPA_one_level_read,
      }
    },
    {
      "strcspn",
      CFAT_default_attr & ~CFAT_pure_call_attr_mask | CFAT_is_libc_entry 
      |CFAT_argument_indirectly_read |CFAT_argument_one_level_deref |
      CFAT_returns_non_pointer,
      2,
      {
        CPA_one_level_read , 
        CPA_one_level_read 
      }
    },	
    {
      "strdup",
      CFAT_default_attr & ~CFAT_pure_call_attr_mask | CFAT_is_libc_entry 
      |CFAT_argument_indirectly_read |CFAT_argument_one_level_deref |
      CFAT_allocates_heap_memory | CFAT_returns_heap_memory,
      1,
      {
        CPA_one_level_read 
      }
    },
    {
      "strncat",
      CFAT_default_attr & ~CFAT_pure_call_attr_mask | CFAT_is_libc_entry 
      |CFAT_argument_indirectly_read | CFAT_argument_indirectly_write 
      |CFAT_argument_one_level_deref |CFAT_exposes_argument_address_to_return,
      3,
      {
        CPA_one_level_read | CPA_one_level_write | CPA_exposed_to_return,
        CPA_one_level_read,
        CPA_no_ptr_deref_and_expose
      }
    },
    {
      "strerror",
      CFAT_libc_default_attr &
      ~(CFAT_exposes_argument_address_to_return |CFAT_exposes_argument_address_to_globals),
      1,
      { CPA_no_ptr_deref_and_expose }
    },
    {
      "strftime",
      CFAT_libc_default_attr &
      ~(CFAT_exposes_argument_address_to_return |CFAT_exposes_argument_address_to_globals)
      | CFAT_argument_indirectly_read | CFAT_argument_indirectly_write
      | CFAT_argument_one_level_deref,
      4,
      { CPA_one_level_write, CPA_no_ptr_deref_and_expose, CPA_one_level_read, CPA_one_level_read }
    },
    {
      "strlen",
      CFAT_default_attr & ~CFAT_pure_call_attr_mask | CFAT_is_libc_entry 
      |CFAT_argument_indirectly_read |CFAT_argument_one_level_deref |
      CFAT_returns_non_pointer,
      1,
      {
        CPA_one_level_read 
      }
    },
    {
      "strncasecmp",
      CFAT_default_attr & ~CFAT_pure_call_attr_mask | CFAT_is_libc_entry 
      |CFAT_argument_indirectly_read |CFAT_argument_one_level_deref,
      3,
      {
        CPA_one_level_read ,
        CPA_one_level_read ,
        CPA_no_ptr_deref_and_expose
      }
    },
    {
      "strncat",
      CFAT_default_attr & ~CFAT_pure_call_attr_mask | CFAT_is_libc_entry 
      |CFAT_argument_indirectly_read | CFAT_argument_indirectly_write 
      |CFAT_argument_one_level_deref |CFAT_exposes_argument_address_to_return,
      3,
      {
        CPA_one_level_read | CPA_one_level_write | CPA_exposed_to_return,
        CPA_one_level_read,
        CPA_no_ptr_deref_and_expose
      }
    },
    {
      "strncmp",
      CFAT_default_attr & ~CFAT_pure_call_attr_mask | CFAT_is_libc_entry 
      |CFAT_argument_indirectly_read |CFAT_argument_one_level_deref,
      3,
      {
        CPA_one_level_read ,
        CPA_one_level_read ,
        CPA_no_ptr_deref_and_expose
      }
    },
    {
      "strncpy",
      CFAT_default_attr & ~CFAT_pure_call_attr_mask | CFAT_is_libc_entry 
      |CFAT_argument_indirectly_read | CFAT_argument_indirectly_write 
      |CFAT_argument_one_level_deref |CFAT_exposes_argument_address_to_return,
      3,
      {
        CPA_one_level_write |CPA_exposed_to_return,
        CPA_one_level_read,
        CPA_no_ptr_deref_and_expose
      }
    },
    {
      "strpbrk",
      CFAT_default_attr & ~CFAT_pure_call_attr_mask | CFAT_is_libc_entry 
      |CFAT_argument_indirectly_read |CFAT_argument_one_level_deref |
      CFAT_exposes_argument_address_to_return,
      2,
      {
        CPA_one_level_read | CPA_exposed_to_return,
        CPA_one_level_read 
      }
    },
    {
      "strrchr",
      CFAT_default_attr & ~CFAT_pure_call_attr_mask | CFAT_is_libc_entry 
      |CFAT_argument_indirectly_read |CFAT_argument_one_level_deref |
      CFAT_exposes_argument_address_to_return,
      2,
      {
        CPA_one_level_read | CPA_exposed_to_return,
        CPA_no_ptr_deref_and_expose
      }
    },
    {
      "strrstr",
      CFAT_default_attr & ~CFAT_pure_call_attr_mask | CFAT_is_libc_entry 
      |CFAT_argument_indirectly_read |CFAT_argument_one_level_deref |
      CFAT_exposes_argument_address_to_return,
      2,
      {
        CPA_one_level_read | CPA_exposed_to_return,
        CPA_one_level_read 
      }
    },
    {
      "strspn",
      CFAT_default_attr & ~CFAT_pure_call_attr_mask | CFAT_is_libc_entry 
      |CFAT_argument_indirectly_read |CFAT_argument_one_level_deref |
      CFAT_returns_non_pointer,
      2,
      {
        CPA_one_level_read ,
        CPA_one_level_read 
      }
    },
    {
      "strstr",
      CFAT_default_attr & ~CFAT_pure_call_attr_mask | CFAT_is_libc_entry 
      |CFAT_argument_indirectly_read |CFAT_argument_one_level_deref |
      CFAT_exposes_argument_address_to_return,
      2,
      {
        CPA_one_level_read | CPA_exposed_to_return,
        CPA_one_level_read 
      }
    },
    {
      "strtod",
      CFAT_libc_default_attr &
      ~(CFAT_exposes_argument_address_to_return |CFAT_exposes_argument_address_to_globals)
      | CFAT_argument_indirectly_read | CFAT_argument_indirectly_write
      | CFAT_argument_one_level_deref,
      2,
      { CPA_one_level_read, CPA_one_level_write}
    },
    {
      "__strtod_internal",
      CFAT_libc_default_attr &
      ~(CFAT_exposes_argument_address_to_return |CFAT_exposes_argument_address_to_globals)
      | CFAT_argument_indirectly_read | CFAT_argument_indirectly_write
      | CFAT_argument_one_level_deref,
      2,
      { CPA_one_level_read, CPA_one_level_write}
    },
    {
      "strtof",
      CFAT_libc_default_attr &
      ~(CFAT_exposes_argument_address_to_return |CFAT_exposes_argument_address_to_globals)
      | CFAT_argument_indirectly_read | CFAT_argument_indirectly_write
      | CFAT_argument_one_level_deref,
      2,
      { CPA_one_level_read, CPA_one_level_write}
    },
    {
      "strtok",
      CFAT_default_attr & ~CFAT_pure_call_attr_mask | CFAT_is_libc_entry 
      |CFAT_argument_indirectly_read | CFAT_argument_indirectly_write 
      |CFAT_argument_one_level_deref | CFAT_returns_exposed_memory |
      CFAT_exposes_argument_address_to_return | CFAT_exposes_argument_address_to_globals,
      2,
      {
        CPA_one_level_read | CPA_one_level_write |CPA_exposed_to_return |CPA_exposed_to_globals
        |CPA_is_not_escaping,
        CPA_one_level_read
      }
    },
    {
      "strtol",
      CFAT_libc_default_attr &
      ~(CFAT_exposes_argument_address_to_return |CFAT_exposes_argument_address_to_globals)
      | CFAT_argument_indirectly_read | CFAT_argument_indirectly_write
      | CFAT_argument_one_level_deref,
      3,
      { CPA_one_level_read, CPA_one_level_write,CPA_no_ptr_deref_and_expose }
    },
    {
      "__strtol_internal",
      CFAT_libc_default_attr &
      ~(CFAT_exposes_argument_address_to_return |CFAT_exposes_argument_address_to_globals)
      | CFAT_argument_indirectly_read | CFAT_argument_indirectly_write
      | CFAT_argument_one_level_deref,
      3,
      { CPA_one_level_read, CPA_one_level_write, CPA_no_ptr_deref_and_expose }
    },
    {
      "strtold",
      CFAT_libc_default_attr &
      ~(CFAT_exposes_argument_address_to_return |CFAT_exposes_argument_address_to_globals)
      | CFAT_argument_indirectly_read | CFAT_argument_indirectly_write
      | CFAT_argument_one_level_deref,
      2,
      { CPA_one_level_read, CPA_one_level_write}
    },
    {
      "__strtoll_internal",
      CFAT_libc_default_attr &
      ~(CFAT_exposes_argument_address_to_return |CFAT_exposes_argument_address_to_globals)
      | CFAT_argument_indirectly_read | CFAT_argument_indirectly_write
      | CFAT_argument_one_level_deref,
      3,
      { CPA_one_level_read, CPA_one_level_write, CPA_no_ptr_deref_and_expose }
    },
    {
      "strtoul",
      CFAT_libc_default_attr &
      ~(CFAT_exposes_argument_address_to_return |CFAT_exposes_argument_address_to_globals)
      | CFAT_argument_indirectly_read | CFAT_argument_indirectly_write
      | CFAT_argument_one_level_deref,
      3,
      { CPA_one_level_read, CPA_one_level_write,CPA_no_ptr_deref_and_expose }
    },
    {
      "__strtoul_internal",
      CFAT_libc_default_attr &
      ~(CFAT_exposes_argument_address_to_return |CFAT_exposes_argument_address_to_globals)
      | CFAT_argument_indirectly_read | CFAT_argument_indirectly_write
      | CFAT_argument_one_level_deref,
      3,
      { CPA_one_level_read, CPA_one_level_write, CPA_no_ptr_deref_and_expose }
    },
    {
      "strtoull",
      CFAT_libc_default_attr &
      ~(CFAT_exposes_argument_address_to_return |CFAT_exposes_argument_address_to_globals)
      | CFAT_argument_indirectly_read | CFAT_argument_indirectly_write
      | CFAT_argument_one_level_deref,
      3,
      { CPA_one_level_read, CPA_one_level_write,CPA_no_ptr_deref_and_expose }
    },
    {
      "__strtoull_internal",
      CFAT_libc_default_attr &
      ~(CFAT_exposes_argument_address_to_return |CFAT_exposes_argument_address_to_globals)
      | CFAT_argument_indirectly_read | CFAT_argument_indirectly_write
      | CFAT_argument_one_level_deref,
      3,
      { CPA_one_level_read, CPA_one_level_write, CPA_no_ptr_deref_and_expose }
    },
    {
      "sysconf",
      CFAT_libc_default_attr &
      ~(CFAT_exposes_argument_address_to_return |CFAT_exposes_argument_address_to_globals)
      | CFAT_argument_indirectly_read | CFAT_argument_indirectly_write
      | CFAT_argument_one_level_deref,
      0,
      { 0, }
    },
    {
      "system",
      CFAT_libc_default_attr &
      ~(CFAT_exposes_argument_address_to_return |CFAT_exposes_argument_address_to_globals)
      | CFAT_argument_indirectly_read | CFAT_argument_indirectly_write
      | CFAT_argument_one_level_deref,
      0,
      { 0, }
    },
    {
      "tan",
      CFAT_default_attr & (~CFAT_pure_call_attr_mask),
      0,
      { 0, }
    },
    {
      "tand",
      CFAT_default_attr & (~CFAT_pure_call_attr_mask),
      0,
      { 0, }
    },
    {
      "tandf",
      CFAT_default_attr & (~CFAT_pure_call_attr_mask),
      0,
      { 0, }
    },
    {
      "tandl",
      CFAT_default_attr & (~CFAT_pure_call_attr_mask),
      0,
      { 0, }
    },
    {
      "tanf",
      CFAT_default_attr & (~CFAT_pure_call_attr_mask),
      0,
      { 0, }
    },
    {
      "tanh",
      CFAT_default_attr & (~CFAT_pure_call_attr_mask),
      0,
      { 0, }
    },
    {
      "tanhf",
      CFAT_default_attr & (~CFAT_pure_call_attr_mask),
      0,
      { 0, }
    },
    {
      "tanhl",
      CFAT_default_attr & (~CFAT_pure_call_attr_mask),
      0,
      { 0, }
    },
    {
      "tanl",
      CFAT_default_attr & (~CFAT_pure_call_attr_mask),
      0,
      { 0, }
    },
    {
      "telldir",
      CFAT_libc_default_attr &
      ~(CFAT_exposes_argument_address_to_return |CFAT_exposes_argument_address_to_globals)
      | CFAT_argument_indirectly_read | CFAT_argument_indirectly_write
      | CFAT_argument_one_level_deref,
      0,
      { 0, }
    },
    {
      "tempnam",
      CFAT_libc_default_attr &
      ~(CFAT_exposes_argument_address_to_return |CFAT_exposes_argument_address_to_globals)
      | CFAT_argument_indirectly_read | CFAT_argument_indirectly_write
      | CFAT_argument_one_level_deref
      | CFAT_allocates_heap_memory | CFAT_returns_heap_memory,
      0,
      { 0, }
    },
    {
      "time",
      CFAT_libc_default_attr &
      ~(CFAT_exposes_argument_address_to_return |CFAT_exposes_argument_address_to_globals)
      | CFAT_argument_indirectly_read | CFAT_argument_indirectly_write
      | CFAT_argument_one_level_deref,
      0,
      { 0, }
    },
    {
      "times",
      CFAT_libc_default_attr &
      ~(CFAT_exposes_argument_address_to_return |CFAT_exposes_argument_address_to_globals)
      | CFAT_argument_indirectly_read | CFAT_argument_indirectly_write
      | CFAT_argument_one_level_deref,
      0,
      { 0, }
    },
    {
      "tmpfile",
      CFAT_libc_default_attr &
      ~(CFAT_exposes_argument_address_to_return |CFAT_exposes_argument_address_to_globals)
      | CFAT_argument_indirectly_read | CFAT_argument_indirectly_write
      | CFAT_argument_one_level_deref,
      0,
      { 0, }
    },
    {
      "tmpnam",
      CFAT_libc_default_attr &
      ~(CFAT_exposes_argument_address_to_globals)
      | CFAT_exposes_argument_address_to_return
      | CFAT_argument_indirectly_read | CFAT_argument_indirectly_write
      | CFAT_argument_one_level_deref,
      1,
      { CPA_one_level_read|CPA_one_level_write |CPA_exposed_to_return,
      }
    },
    {
      "__tolower",
      CFAT_default_attr & (~CFAT_pure_call_attr_mask) | CFAT_hidden_data_read
      | CFAT_libc_globals_read | CFAT_is_libc_entry,
      0,
      {
        0,
      }
    },
    {
      "tolower",
      CFAT_libc_default_attr &
      ~(CFAT_exposes_argument_address_to_return |CFAT_exposes_argument_address_to_globals),
      0,
      { 0, }
    },
    {
      "toupper",
      CFAT_libc_default_attr &
      ~(CFAT_exposes_argument_address_to_return |CFAT_exposes_argument_address_to_globals),
      0,
      { 0, }
    },
    {
      "towlower",
      CFAT_libc_default_attr &
      ~(CFAT_exposes_argument_address_to_return |CFAT_exposes_argument_address_to_globals),
      0,
      { 0, }
    },
    {
      "towupper",
      CFAT_libc_default_attr &
      ~(CFAT_exposes_argument_address_to_return |CFAT_exposes_argument_address_to_globals),
      0,
      { 0, }
    },
    {
      "truncate",
      CFAT_libc_default_attr &
      ~(CFAT_exposes_argument_address_to_return |CFAT_exposes_argument_address_to_globals)
      | CFAT_argument_indirectly_read | CFAT_argument_indirectly_write
      | CFAT_argument_one_level_deref,
      0,
      { 0, }
    },
    {
      "ttyname",
      CFAT_libc_default_attr &
      ~(CFAT_exposes_argument_address_to_return |CFAT_exposes_argument_address_to_globals)
      | CFAT_argument_indirectly_read | CFAT_argument_indirectly_write
      | CFAT_argument_one_level_deref,
      0,
      { 0, }
    },
    {
      "umask",
      CFAT_libc_default_attr &
      ~(CFAT_exposes_argument_address_to_return |CFAT_exposes_argument_address_to_globals)
      | CFAT_argument_indirectly_read | CFAT_argument_indirectly_write
      | CFAT_argument_one_level_deref,
      0,
      { 0, }
    },
    {
      "ungetc",
      CFAT_libc_default_attr &
      ~(CFAT_exposes_argument_address_to_return |CFAT_exposes_argument_address_to_globals)
      | CFAT_argument_indirectly_read | CFAT_argument_indirectly_write
      | CFAT_argument_one_level_deref,
      2,
      { 0, 
        CPA_one_level_read |CPA_two_level_read |CPA_one_level_write |CPA_two_level_write}
    },
    {
      "unlink",
      CFAT_libc_default_attr &
      ~(CFAT_exposes_argument_address_to_return |CFAT_exposes_argument_address_to_globals)
      | CFAT_argument_indirectly_read | CFAT_argument_indirectly_write
      | CFAT_argument_one_level_deref,
      0,
      { 0, }
    },
    {
      "utime",
      CFAT_libc_default_attr &
      ~(CFAT_exposes_argument_address_to_return |CFAT_exposes_argument_address_to_globals)
      | CFAT_argument_indirectly_read | CFAT_argument_indirectly_write
      | CFAT_argument_one_level_deref,
      0,
      { 0, }
    },
    {
      "vfprintf",
      CFAT_libc_default_attr &
      ~(CFAT_exposes_argument_address_to_return |CFAT_exposes_argument_address_to_globals)
      | CFAT_argument_indirectly_read | CFAT_argument_indirectly_write
      | CFAT_argument_one_level_deref,
      2,
      { CPA_one_level_read |CPA_two_level_read |CPA_one_level_write |CPA_two_level_write
        |CPA_is_not_escaping,
        CPA_one_level_read }
    },
    {
      "vsnprintf",
      CFAT_libc_default_attr &
      ~(CFAT_exposes_argument_address_to_return |CFAT_exposes_argument_address_to_globals)
      | CFAT_argument_indirectly_read | CFAT_argument_indirectly_write
      | CFAT_argument_one_level_deref,
      3,
      {CPA_one_level_write, 0, CPA_one_level_read }
    },
    {
      "vsprintf",
      CFAT_libc_default_attr &
      ~(CFAT_exposes_argument_address_to_return |CFAT_exposes_argument_address_to_globals)
      | CFAT_argument_indirectly_read | CFAT_argument_indirectly_write
      | CFAT_argument_one_level_deref,
      2,
      { CPA_one_level_write, CPA_one_level_read}
    },
    {
      "wait",
      CFAT_libc_default_attr &
      ~(CFAT_exposes_argument_address_to_return |CFAT_exposes_argument_address_to_globals)
      | CFAT_argument_indirectly_read | CFAT_argument_indirectly_write
      | CFAT_argument_one_level_deref,
      0,
      { 0, }
    },
    {
      "waitpid",
      CFAT_libc_default_attr &
      ~(CFAT_exposes_argument_address_to_return |CFAT_exposes_argument_address_to_globals)
      | CFAT_argument_indirectly_read | CFAT_argument_indirectly_write
      | CFAT_argument_one_level_deref,
      0,
      { 0, }
    },
    {
      "__wcrtomb",
      CFAT_libc_default_attr &
      ~(CFAT_exposes_argument_address_to_return |CFAT_exposes_argument_address_to_globals)
      | CFAT_argument_indirectly_read | CFAT_argument_indirectly_write
      | CFAT_argument_one_level_deref,
      3,
      { CPA_one_level_write, CPA_no_ptr_deref_and_expose, 
        CPA_one_level_read | CPA_one_level_write }  
    },
    {
      "wcrtomb",
      CFAT_libc_default_attr &
      ~(CFAT_exposes_argument_address_to_return |CFAT_exposes_argument_address_to_globals)
      | CFAT_argument_indirectly_read | CFAT_argument_indirectly_write
      | CFAT_argument_one_level_deref,
      3,
      { CPA_one_level_write, CPA_no_ptr_deref_and_expose, 
        CPA_one_level_read | CPA_one_level_write }  
    },
    {
      "wcscat",  /* Equivalent in side effect behavior to strcat */
      CFAT_default_attr & ~CFAT_pure_call_attr_mask | CFAT_is_libc_entry 
      |CFAT_argument_indirectly_read | CFAT_argument_indirectly_write 
      |CFAT_argument_one_level_deref |CFAT_exposes_argument_address_to_return,
      2,
      {
        CPA_one_level_read | CPA_one_level_write | CPA_exposed_to_return,
        CPA_one_level_read 
      }
    },
    {
      "wcschr", /* Equivalent in side effect behavior to strchr */
      CFAT_default_attr & ~CFAT_pure_call_attr_mask | CFAT_is_libc_entry 
      |CFAT_argument_indirectly_read |CFAT_argument_one_level_deref |
      CFAT_exposes_argument_address_to_return,
      2,
      {
        CPA_one_level_read | CPA_exposed_to_return,
        CPA_no_ptr_deref_and_expose
      }
    },
    {
      "wcscmp", /* Equivalent in side effect behavior to strcmp */
      CFAT_default_attr & ~CFAT_pure_call_attr_mask | CFAT_is_libc_entry 
      |CFAT_argument_indirectly_read |CFAT_argument_one_level_deref,
      2,
      {
        CPA_one_level_read,
        CPA_one_level_read
      }
    },
    {
      "wcscpy", /* Equivalent in side effect behavior to strcpy */
      CFAT_default_attr & ~CFAT_pure_call_attr_mask | CFAT_is_libc_entry 
      |CFAT_argument_indirectly_read | CFAT_argument_indirectly_write 
      |CFAT_argument_one_level_deref |CFAT_exposes_argument_address_to_return,
      2,
      {
        CPA_one_level_write |CPA_exposed_to_return,
        CPA_one_level_read,
      }
    },
    {
      "wcscspn",  /* Equivalent in side effect behavior to strcspn */
      CFAT_default_attr & ~CFAT_pure_call_attr_mask | CFAT_is_libc_entry 
      |CFAT_argument_indirectly_read |CFAT_argument_one_level_deref |
      CFAT_returns_non_pointer,
      2,
      {
        CPA_one_level_read , 
        CPA_one_level_read 
      }
    },
    {
      "wcslen", /* Equivalent in side effect behavior to strlen */
      CFAT_default_attr & ~CFAT_pure_call_attr_mask | CFAT_is_libc_entry 
      |CFAT_argument_indirectly_read |CFAT_argument_one_level_deref |
      CFAT_returns_non_pointer,
      1,
      {
        CPA_one_level_read 
      }
    },
    {
      "wcsncat", /* Equivalent in side effect behavior to strncat */
      CFAT_default_attr & ~CFAT_pure_call_attr_mask | CFAT_is_libc_entry 
      |CFAT_argument_indirectly_read | CFAT_argument_indirectly_write 
      |CFAT_argument_one_level_deref |CFAT_exposes_argument_address_to_return,
      3,
      {
        CPA_one_level_read | CPA_one_level_write | CPA_exposed_to_return,
        CPA_one_level_read,
        CPA_no_ptr_deref_and_expose
      }
    },
    {
      "wcsncmp",  /* Equivalent in side effect behavior to strncmp */
      CFAT_default_attr & ~CFAT_pure_call_attr_mask | CFAT_is_libc_entry 
      |CFAT_argument_indirectly_read |CFAT_argument_one_level_deref,
      3,
      {
        CPA_one_level_read ,
        CPA_one_level_read ,
        CPA_no_ptr_deref_and_expose
      }
    },
    {
      "wcsncpy",  /* Equivalent in side effect behavior to strncpy */
      CFAT_default_attr & ~CFAT_pure_call_attr_mask | CFAT_is_libc_entry 
      |CFAT_argument_indirectly_read | CFAT_argument_indirectly_write 
      |CFAT_argument_one_level_deref |CFAT_exposes_argument_address_to_return,
      3,
      {
        CPA_one_level_write |CPA_exposed_to_return,
        CPA_one_level_read,
        CPA_no_ptr_deref_and_expose
      }
    },
    {
      "wcspbrk",  /* Equivalent in side effect behavior to strpbrk */
      CFAT_default_attr & ~CFAT_pure_call_attr_mask | CFAT_is_libc_entry 
      |CFAT_argument_indirectly_read |CFAT_argument_one_level_deref |
      CFAT_exposes_argument_address_to_return,
      2,
      {
        CPA_one_level_read | CPA_exposed_to_return,
        CPA_one_level_read 
      }
    },
    {
      "wcsrchr",  /* Equivalent in side effect behavior to strrchr */
      CFAT_default_attr & ~CFAT_pure_call_attr_mask | CFAT_is_libc_entry 
      |CFAT_argument_indirectly_read |CFAT_argument_one_level_deref |
      CFAT_exposes_argument_address_to_return,
      2,
      {
        CPA_one_level_read | CPA_exposed_to_return,
        CPA_no_ptr_deref_and_expose
      }
    },
    {
      "__wcsrtombs",
      CFAT_libc_default_attr &
      ~(CFAT_exposes_argument_address_to_return |CFAT_exposes_argument_address_to_globals)
      | CFAT_argument_indirectly_read | CFAT_argument_indirectly_write
      | CFAT_argument_one_level_deref,
      4,
      { CPA_one_level_write, CPA_one_level_read, CPA_no_ptr_deref_and_expose,
        CPA_one_level_read | CPA_one_level_write }  
    },
    {
      "wcsrtombs",
      CFAT_libc_default_attr &
      ~(CFAT_exposes_argument_address_to_return |CFAT_exposes_argument_address_to_globals)
      | CFAT_argument_indirectly_read | CFAT_argument_indirectly_write
      | CFAT_argument_one_level_deref,
      4,
      { CPA_one_level_write, CPA_one_level_read, CPA_no_ptr_deref_and_expose,
        CPA_one_level_read | CPA_one_level_write }  
    },
    {
      "wcsspn",  /* Equivalent in side effect behavior to strspn */
      CFAT_default_attr & ~CFAT_pure_call_attr_mask | CFAT_is_libc_entry 
      |CFAT_argument_indirectly_read |CFAT_argument_one_level_deref |
      CFAT_returns_non_pointer,
      2,
      {
        CPA_one_level_read ,
        CPA_one_level_read 
      }
    },
    {
      "wcsstr",  /* Equivalent in side effect behavior to strstr */
      CFAT_default_attr & ~CFAT_pure_call_attr_mask | CFAT_is_libc_entry 
      |CFAT_argument_indirectly_read |CFAT_argument_one_level_deref |
      CFAT_exposes_argument_address_to_return,
      2,
      {
        CPA_one_level_read | CPA_exposed_to_return,
        CPA_one_level_read 
      }
    },
    {
      "wcstombs",
      CFAT_libc_default_attr &
      ~(CFAT_exposes_argument_address_to_return |CFAT_exposes_argument_address_to_globals)
      | CFAT_argument_indirectly_read | CFAT_argument_indirectly_write
      | CFAT_argument_one_level_deref,
      3,
      { CPA_one_level_write, CPA_one_level_read, CPA_no_ptr_deref_and_expose}
    },
    {
      "wctomb",
      CFAT_libc_default_attr &
      ~(CFAT_exposes_argument_address_to_return |CFAT_exposes_argument_address_to_globals)
      | CFAT_argument_indirectly_read | CFAT_argument_indirectly_write
      | CFAT_argument_one_level_deref,
      2,
      { CPA_one_level_write, CPA_no_ptr_deref_and_expose}
    },
    {
      "_wcwidth",
      CFAT_default_attr & (~CFAT_pure_call_attr_mask) | CFAT_hidden_data_read
      | CFAT_libc_globals_read | CFAT_is_libc_entry,
      0,
      {
        0,
      }
    },
    {
      "wcwidth",
      CFAT_default_attr & (~CFAT_pure_call_attr_mask) | CFAT_hidden_data_read
      | CFAT_libc_globals_read | CFAT_is_libc_entry,
      0,
      {
        0,
      }
    },
    {
      "wmemchr",  /* Equivalent in side effect behavior to memchr */
      CFAT_default_attr & ~CFAT_pure_call_attr_mask | CFAT_is_libc_entry 
      |CFAT_argument_indirectly_read 
      |CFAT_argument_one_level_deref |CFAT_exposes_argument_address_to_return,
      3,
      {
        CPA_one_level_read |CPA_exposed_to_return, 
        CPA_no_ptr_deref_and_expose,
        CPA_no_ptr_deref_and_expose
      }
    },
    {
      "wmemcmp",  /* Equivalent in side effect behavior to memcmp */
      CFAT_default_attr & ~CFAT_pure_call_attr_mask | CFAT_is_libc_entry 
      |CFAT_argument_indirectly_read |CFAT_argument_one_level_deref,
      3,
      {
        CPA_one_level_read , 
        CPA_one_level_read , 
        CPA_no_ptr_deref_and_expose
      }
    },
    {
      "wmemcpy",  /* Equivalent in side effect behavior to memcpy */
      CFAT_default_attr & ~CFAT_pure_call_attr_mask | CFAT_is_libc_entry 
      |CFAT_argument_indirectly_read | CFAT_argument_indirectly_write 
      |CFAT_argument_one_level_deref |CFAT_exposes_argument_address_to_return,
      3,
      {
        CPA_one_level_write | CPA_exposed_to_return,
        CPA_one_level_read , 
        CPA_no_ptr_deref_and_expose
      }
    },
    {
      "wmemmove",  /* Equivalent in side effect behavior to memmove */
      CFAT_default_attr & ~CFAT_pure_call_attr_mask | CFAT_is_libc_entry 
      |CFAT_argument_indirectly_read | CFAT_argument_indirectly_write 
      |CFAT_argument_one_level_deref |CFAT_exposes_argument_address_to_return,
      3,
      {
        CPA_one_level_write | CPA_exposed_to_return,
        CPA_one_level_read , 
        CPA_no_ptr_deref_and_expose
      }
    },
    {
      "wmemset", /* Equivalent in side effect behavior to memset */
      CFAT_default_attr & ~CFAT_pure_call_attr_mask | CFAT_is_libc_entry 
      | CFAT_argument_indirectly_write |CFAT_exposes_argument_address_to_return 
      |CFAT_argument_one_level_deref,
      2,
      {
        CPA_one_level_write | CPA_exposed_to_return,
        CPA_no_ptr_deref_and_expose
      }
    },
    {
      "write",
      CFAT_libc_default_attr &
      ~(CFAT_exposes_argument_address_to_return |CFAT_exposes_argument_address_to_globals)
      | CFAT_argument_indirectly_read | CFAT_argument_indirectly_write
      | CFAT_argument_one_level_deref,
      0,
      { 0, }
    },
    {
      "xdr_double",
      CFAT_libc_default_attr &
      ~(CFAT_exposes_argument_address_to_return |CFAT_exposes_argument_address_to_globals)
      | CFAT_argument_indirectly_read  | CFAT_argument_indirectly_write,
      2,
      { CPA_one_level_read | CPA_two_level_read | CPA_multi_level_read |
        CPA_one_level_write |CPA_two_level_write | CPA_multi_level_write,
        CPA_one_level_read | CPA_one_level_write 
      }
    },
    {
      "xdr_float",
      CFAT_libc_default_attr &
      ~(CFAT_exposes_argument_address_to_return |CFAT_exposes_argument_address_to_globals)
      | CFAT_argument_indirectly_read  | CFAT_argument_indirectly_write,
      2,
      { CPA_one_level_read | CPA_two_level_read | CPA_multi_level_read |
        CPA_one_level_write |CPA_two_level_write | CPA_multi_level_write,
        CPA_one_level_read | CPA_one_level_write 
      }
    },
    {
      "xdr_int",
      CFAT_libc_default_attr &
      ~(CFAT_exposes_argument_address_to_return |CFAT_exposes_argument_address_to_globals)
      | CFAT_argument_indirectly_read  | CFAT_argument_indirectly_write,
      2,
      { CPA_one_level_read | CPA_two_level_read | CPA_multi_level_read |
        CPA_one_level_write |CPA_two_level_write | CPA_multi_level_write,
        CPA_one_level_read | CPA_one_level_write 
      }
    },
    {
      "_ZdaPv",
      CFAT_libcpp_default_attr &
      ~(CFAT_exposes_argument_address_to_return 
        | CFAT_exposes_argument_address_to_globals)
      | CFAT_globals_read | CFAT_globals_write | CFAT_is_marked_libcall
      | CFAT_argument_indirectly_read | CFAT_argument_indirectly_write 
      | CFAT_argument_one_level_deref | CFAT_deallocates_heap_memory,
      0,
      { 0, }
    },
    {
      "_ZdlPv",
      CFAT_libcpp_default_attr &
      ~(CFAT_exposes_argument_address_to_return 
        | CFAT_exposes_argument_address_to_globals)
      | CFAT_globals_read | CFAT_globals_write  | CFAT_is_marked_libcall
      | CFAT_argument_indirectly_read | CFAT_argument_indirectly_write 
      | CFAT_argument_one_level_deref | CFAT_deallocates_heap_memory,
      0,
      { 0, }
    },
    {
      "_Znam",
      CFAT_libcpp_default_attr &
      ~(CFAT_exposes_argument_address_to_return 
        | CFAT_exposes_argument_address_to_globals)
      | CFAT_globals_read | CFAT_globals_write 
      | CFAT_argument_indirectly_read | CFAT_argument_indirectly_write 
      | CFAT_is_marked_libcall 
      | CFAT_argument_one_level_deref | CFAT_allocates_heap_memory 
      | CFAT_returns_heap_memory,
      0,
      { 0, }
    },
    {
      "_Znaj",
      CFAT_libcpp_default_attr &
      ~(CFAT_exposes_argument_address_to_return 
        | CFAT_exposes_argument_address_to_globals)
      | CFAT_globals_read | CFAT_globals_write 
      | CFAT_argument_indirectly_read | CFAT_argument_indirectly_write 
      | CFAT_is_marked_libcall 
      | CFAT_argument_one_level_deref | CFAT_allocates_heap_memory 
      | CFAT_returns_heap_memory,
      0,
      { 0, }
    },
    {
      "_Znwm",
      CFAT_libcpp_default_attr &
      ~(CFAT_exposes_argument_address_to_return 
        | CFAT_exposes_argument_address_to_globals)
      | CFAT_globals_read | CFAT_globals_write 
      | CFAT_argument_indirectly_read | CFAT_argument_indirectly_write  
      | CFAT_is_marked_libcall
      | CFAT_argument_one_level_deref | CFAT_allocates_heap_memory 
      | CFAT_returns_heap_memory,
      0,
      { 0, }
    },
    {
      "_Znwj",
      CFAT_libcpp_default_attr &
      ~(CFAT_exposes_argument_address_to_return 
        | CFAT_exposes_argument_address_to_globals)
      | CFAT_globals_read | CFAT_globals_write 
      | CFAT_argument_indirectly_read | CFAT_argument_indirectly_write  
      | CFAT_is_marked_libcall
      | CFAT_argument_one_level_deref | CFAT_allocates_heap_memory 
      | CFAT_returns_heap_memory,
      0,
      { 0, }
    },
    { 0 }
  };



inline void cpa_set_one_level_read(UINT32& p) { p |= CPA_one_level_read;}
inline void cpa_set_one_level_write(UINT32& p) { p |= CPA_one_level_write;}
inline void cpa_set_two_level_read(UINT32& p) {  p |= CPA_two_level_read;}
inline void cpa_set_two_level_write(UINT32& p) {  p |= CPA_two_level_write;}
inline void cpa_set_multi_level_read(UINT32& p) { p |= CPA_multi_level_read;}
inline void cpa_set_multi_level_write(UINT32& p) {  p |= CPA_multi_level_write;}
inline void cpa_set_exposed_to_globals(UINT32& p) {  p |= CPA_exposed_to_globals; }
inline void cpa_set_exposed_to_return(UINT32& p) { p |= CPA_exposed_to_return; }
inline void cpa_set_is_pointer_to_heap_addr_loc(UINT32& p) { p |= CPA_is_pointer_to_heap_addr_loc;}
inline void cpa_set_is_format_string(UINT32& p ) { p |= CPA_is_format_string; }

inline void cpa_clear_one_level_read(UINT32& p) { p &= ~CPA_one_level_read;}
inline void cpa_clear_one_level_write(UINT32& p) { p &= ~CPA_one_level_write;}
inline void cpa_clear_two_level_read(UINT32& p) {  p &= ~CPA_two_level_read;}
inline void cpa_clear_two_level_write(UINT32& p) {  p &= ~CPA_two_level_write;}
inline void cpa_clear_multi_level_read(UINT32& p) { p &= ~CPA_multi_level_read;}
inline void cpa_clear_multi_level_write(UINT32& p) {  p &= ~CPA_multi_level_write;}
inline void cpa_clear_exposed_to_globals(UINT32& p) {  p &= ~CPA_exposed_to_globals; }
inline void cpa_clear_exposed_to_return(UINT32& p) { p &= ~CPA_exposed_to_return; }
inline void cpa_clear_is_pointer_to_heap_addr_loc(UINT32& p) { p &= ~CPA_is_pointer_to_heap_addr_loc;}
inline void cpa_clear_is_format_string(UINT32& p ) { p &= ~CPA_is_format_string; }

static CallSideEffectInfoTable CallSideEffectTable;

CallSideEffectInfoTable::CallSideEffectInfoTable()
{
  // create side effect hash table:
  INT32 i = 0;
  const char* func = RawLibcallSideEffectTable[0].FuncName;
  while (func)
  {
    CallSideEffectInfo* new_info = 
      new CallSideEffectInfo(RawLibcallSideEffectTable[i]);
    SideEffectTable_[func] = new_info;
    i++;
    func = RawLibcallSideEffectTable[i].FuncName;
  }
}

// check %n in the format string:
extern bool
doesFormatStringContainPercN(WN* call_node, UINT32 format_arg_pos)
{
  // get format string argument 
  WN* format_node = WN_kid0(WN_kid(call_node, format_arg_pos));
  if ((WN_operator(format_node) == OPR_LDA) &&
      (ST_sym_class(WN_st(format_node)) == CLASS_CONST) &&
      (TCON_ty(STC_val(WN_st(format_node))) == MTYPE_STRING))
  {
    char* format_string = Targ_Print(NULL, STC_val(WN_st(format_node)));
    // Simple scan for %n
    const char* cur_c = format_string;
    while (*cur_c != '\0')
    {
      if (*cur_c++ == '%')
      {
        // skip modifiers
        while ((*cur_c == 'h') || (*cur_c == 'l') || (*cur_c == 'L') ||
               isdigit(*cur_c) || (*cur_c == '$'))
          ++cur_c;
        
        // %%n --> produce can false positive -- should be ok.
        if (*cur_c == 'n') return true;
      }
    }
    return false;
  }
  
  return true; // may contain:
}


UINT32
CallSideEffectInfo::GetArgumentAttr(UINT32 arg_pos, 
                                    WN* call_node,
                                    bool ignore_format_string) const
{
  if (arg_pos < NumOfKnownPars)
  {
    return ParAttrs[arg_pos];
  }

  UINT32 arg_attr;
  // Special case for printf/sprintf/fprintf family functions --
  // by default, arguments in vararg list should have attributes '0', but
  // when %n is detected in the format string, it should be CPA_one_level_write 
  // (conservatively for all trailing arguments).
  //  
  if (SideEffects & CFAT_is_printf_like)
  {
    arg_attr = GetArgumentAttr_();

    if (call_node) {
      // Now find the format string:
      INT32 format_pos = -1;
      for (INT32 i = 0; i < NumOfKnownPars; i ++)
      {
        if (ParAttrs[i] & CPA_is_format_string)
        {
          format_pos = i;
          break;
        }
      }
      if (doesFormatStringContainPercN(call_node,format_pos))
        arg_attr =  arg_attr | CPA_one_level_write;
    }
    else  // call_node is not available, we have to be conservative
      arg_attr = arg_attr | CPA_one_level_write;

    if (ignore_format_string)
      arg_attr &= ~CPA_one_level_write;
  }
  else arg_attr = GetArgumentAttr_();

  return arg_attr;
}

UINT32
CallSideEffectInfo::GetArgumentAttr_() const
{
  UINT32 default_attr = CPA_default_attr;
  if ((SideEffects & CFAT_argument_indirectly_read) == 0)
  {
    cpa_clear_one_level_read(default_attr);
    cpa_clear_two_level_read(default_attr);
    cpa_clear_multi_level_read(default_attr);
    cpa_clear_two_level_write(default_attr);
    cpa_clear_multi_level_write(default_attr);
  }
  // only guarantees one level:
  if ((SideEffects & CFAT_argument_indirectly_write) == 0)
  {
    cpa_clear_one_level_write(default_attr);
  }

  if (SideEffects & CFAT_argument_one_level_deref)
  {
    cpa_clear_two_level_read(default_attr);
    cpa_clear_multi_level_read(default_attr);
    cpa_clear_two_level_write(default_attr);
    cpa_clear_multi_level_write(default_attr);
  }

  if ((SideEffects & CFAT_exposes_argument_address_to_return) == 0)
    cpa_clear_exposed_to_return(default_attr);
  if ((SideEffects & CFAT_exposes_argument_address_to_globals) == 0)
    cpa_clear_exposed_to_globals(default_attr);

  return default_attr;
}

CallSideEffectInfo::CallSideEffectInfo(const WN* call_node)
{
  FuncName = 0;
  NumOfKnownPars = 0;
  UINT32 side_effects = CFAT_default_attr;

  if (call_node)
  {
#if 0
    // Pure call case:
    // if (WN_Call_Pure(call_node))
    if(ci->GetLibCall() == 2)
    {
      // Set Pure attribute:
      side_effects &= ~CFAT_pure_call_attr_mask;
      
      // Now special case for some math lib calls:
      UINT32 num_args = call_node->GetNumArguments();
      INT32 i;
      for (i = 0; i < num_args; i++)
      {
        SyzNodePtr arg = call_node->GetArgument(i);
        if (arg->GetResultType()->isPointer()) 
        {
          side_effects |= CFAT_argument_indirectly_read;
          break;
        }
      }
    
      SideEffects = side_effects;
      return;
    }
#endif

    if (WN_Call_Pure(call_node))
      side_effects &= ~CFAT_pure_call_attr_mask;
    
    if (WN_Call_Does_Mem_Alloc(call_node) ||
        (WN_has_sym(call_node) &&
         ST_class(WN_st(call_node)) == CLASS_FUNC &&
         PU_has_attr_malloc(Pu_Table[ST_pu(WN_st(call_node))])) )
    {
      side_effects |= CFAT_allocates_heap_memory;
      side_effects |= CFAT_returns_heap_memory;
    }
  }
  // TODO -- a few more side_effects may be retrieved from the WN
  // (specifically for Fortran intrinsic calls)

  SideEffects  = side_effects;
  return;
}

CallSideEffectInfo::CallSideEffectInfo(CallSideEffectInfoBase& csb)
  : CallSideEffectInfoBase(csb)
{}


CallSideEffectInfo
CallSideEffectInfo::GetDefaultCallSideEffectInfo(const WN* call_node )
{
  CallSideEffectInfo csi(call_node);
  return csi;
}

CallSideEffectInfo
CallSideEffectInfo::GetCallSideEffectInfo(const WN* call_node, 
                                          bool* from_table)
{
  const char* name_str = 0;
  // Firstly, find the name key to lookup the side effect table:
  OPCODE opc = WN_opcode(call_node);
  OPERATOR opr = OPCODE_operator(opc);
  if (opr == OPR_INTRINSIC_CALL)
    name_str = INTRN_c_name(WN_intrinsic(call_node));
  else
    name_str = ST_name(WN_st(call_node));

  if (!name_str) 
  {
    if (from_table)
      *from_table = false;
    return GetDefaultCallSideEffectInfo(call_node);
  }

  return GetCallSideEffectInfo_(name_str, call_node, from_table);
}

CallSideEffectInfo
CallSideEffectInfo::GetCallSideEffectInfo(const INTRINSIC intr_id,
                                          bool *from_table)
{
  const char *name_str = INTRN_c_name(intr_id);
  if (!name_str)
  { 
    if (from_table)
      *from_table = false;
    return GetDefaultCallSideEffectInfo(NULL);
  }
  return GetCallSideEffectInfo_(name_str, NULL, from_table);
}

CallSideEffectInfo
CallSideEffectInfo::GetCallSideEffectInfo(const ST* call_sym,
                                          bool* from_table)
{
  const char* name_str = 0;
  // Firstly, find the name key to lookup the side effect table:
  name_str = ST_name(call_sym);

  if (!name_str)
  { 
    if (from_table)
      *from_table = false;
    return GetDefaultCallSideEffectInfo(NULL);
  }
  if (ST_class(call_sym) == CLASS_FUNC &&
      PU_has_attr_malloc(Pu_Table[ST_pu(call_sym)])) {
    CallSideEffectInfo ret_info = GetDefaultCallSideEffectInfo(NULL);
    ret_info.SideEffects |= CFAT_allocates_heap_memory;
    ret_info.SideEffects |= CFAT_returns_heap_memory;
    return ret_info;
  }
  else
    return GetCallSideEffectInfo_(name_str, NULL, from_table);
}

CallSideEffectInfo
CallSideEffectInfo::GetCallSideEffectInfo_(const char* func_name,
                                           const WN* call_node,
                                           bool* from_table)
{
  CallSideEffectInfo* csip = CallSideEffectTable.Find(func_name);
  // No entry found in the side effect table:
  if (csip == 0)
  {
    if (from_table)
      *from_table = false;
    if (call_node &&
        WN_has_sym(call_node) &&
        ST_class(WN_st(call_node)) == CLASS_FUNC && 
        PU_has_attr_malloc(Pu_Table[ST_pu(WN_st(call_node))])) {
      CallSideEffectInfo ret_info = GetDefaultCallSideEffectInfo(NULL);
      ret_info.SideEffects |= CFAT_allocates_heap_memory;
      ret_info.SideEffects |= CFAT_returns_heap_memory;
      return ret_info;
    }
    else
      return GetDefaultCallSideEffectInfo(NULL);
  }

  // a copy is made:
  CallSideEffectInfo csi = *csip;

  if (from_table)
    *from_table = true;

  return csi;
}


static bool  
printAttr_(UINT32 side_effects, UINT32 attr, const char* encode,
           bool suppress_hippen, bool& is_first, UINT32 mask = 0)
{
  if (mask == 0) mask = attr;
  bool printed = false;

  if ((side_effects & mask) == attr)
  {
    if (!is_first && !suppress_hippen)
      fprintf(stderr,"-");
    else if (is_first)
      fprintf(stderr,"\t");

    fprintf(stderr,"%s", encode);

    printed = true;
    is_first = false;
  }

  return printed;
}

void 
CallSideEffectInfo::Print(WN* callnode) const
{
#if 0
  // Print string encoding (lower case for read, upper case for write -- 
  // for single letter case)
  // 
  // a/A -- argument indirectly read/write
  // g/G -- globals read/write
  // c/C -- libc globals read/write
  // p/P -- libcpp globals read/write
  // h/H -- hidden data read/write
  // s/S -- statics read/write
  // e/E -- exposed memory read/write
  // -------------------
  // rh  -- returns heap address
  // re  -- returns exposed locatiino
  // eg  -- exposing to global
  // er  -- exposing to return
  // ol  -- one level deref
  // mc  -- marked libcall
  // fm  -- has format string
  // ---------------------
  // PR  -- printf like
  // STC  -- libc entry
  // CPP  -- libcpp entry
  // FOR  -- f90 entry
  // ASM  -- assem intrin
  // DCTR -- default ctor
  // CCTR -- default copy ctor
  // ALLC -- allocator
  // DLLC -- deallocator 


  // Per-argument attribute encoding:
  // ord -- one level read
  // trd -- two level read
  // mrd -- mult level read
  // owr -- one level write
  // twr -- two level write
  // mwr -- mult level write
  // ------------------------
  // eg  -- exposed to global
  // er  -- exposed to return
  // php -- pointer to heap pointer
  // fmt -- is format string


  fprintf(stderr,"[CSI]: node_id=[%05x] ",callnode->GetId().GetVal());
  SYZ_OPCODE op = callnode->GetOpcode();
  if (op == OP_icup)
    fprintf(stderr,"\n");
  else
  {
    if (op == OP_cup)
      fprintf(stderr, "callee=%s\n",
              callnode->GetEntry()->GetName()->GetCharString());
    else
    {
      SYZ_IntrinsicInfo iif = callnode->GetIntrinsicInfo();
      if (iif.isHPIntelIntrinsic())
        fprintf(stderr,"asmop=%s\n", iif.GetName());
      else
        fprintf(stderr,"f90cip=%s\n", iif.GetName());
    }
  }

  bool is_first = true;
  bool p = false;
  p =   printAttr_(SideEffects, CFAT_globals_read, "g", false, is_first);
  p =   printAttr_(SideEffects, CFAT_globals_write, "G", p, is_first);
  p =   printAttr_(SideEffects, CFAT_statics_read, "s", false, is_first);
  p =   printAttr_(SideEffects, CFAT_statics_write, "S", p, is_first);
  p =   printAttr_(SideEffects, CFAT_libc_globals_read, "c", false, is_first);
  p =   printAttr_(SideEffects, CFAT_libc_globals_write, "C", p, is_first);
  p =   printAttr_(SideEffects, CFAT_libcpp_globals_read, "p", false, is_first);
  p =   printAttr_(SideEffects, CFAT_libcpp_globals_write, "P", p, is_first);
  p =   printAttr_(SideEffects, CFAT_exposed_memory_read, "e", false, is_first);
  p =   printAttr_(SideEffects, CFAT_exposed_memory_write, "E", p, is_first);
  p =   printAttr_(SideEffects, CFAT_hidden_data_read, "h", false, is_first);
  p =   printAttr_(SideEffects, CFAT_hidden_data_write, "H", p, is_first);
  p =   printAttr_(SideEffects, CFAT_argument_indirectly_read, "a", false, is_first);
  p =   printAttr_(SideEffects, CFAT_argument_indirectly_write, "A", p, is_first);
  p =   printAttr_(SideEffects, CFAT_argument_one_level_deref, "ol", false, is_first);
  p =   printAttr_(SideEffects, CFAT_is_marked_libcall, "mc", false, is_first);
  p =   printAttr_(SideEffects, CFAT_has_format_string, "fm", false, is_first);
  p =   printAttr_(SideEffects, CFAT_returns_heap_memory, "rh", false, is_first);
  p =   printAttr_(SideEffects, CFAT_returns_exposed_memory, "re", false, is_first);
  p =   printAttr_(SideEffects, CFAT_exposes_argument_address_to_return, "er", false, is_first);
  p =   printAttr_(SideEffects, CFAT_exposes_argument_address_to_globals, "eg", false, is_first);
  p =   printAttr_(SideEffects, CFAT_allocates_heap_memory, "ALLC", false, is_first);
  p =   printAttr_(SideEffects, CFAT_deallocates_heap_memory, "DLLC", false, is_first);
  p =   printAttr_(SideEffects, CFAT_is_printf_like, "PR", false, is_first);
  fprintf(stderr,"-[");
  p =   printAttr_(SideEffects, CFAT_is_libc_entry, "STC", true, is_first, CFAT_callee_kind_mask);
  p =   printAttr_(SideEffects, CFAT_is_libcpp_entry, "CPP", true, is_first, CFAT_callee_kind_mask);
  p =   printAttr_(SideEffects, CFAT_is_lib_f90, "FOR", true, is_first, CFAT_callee_kind_mask);
  p =   printAttr_(SideEffects, CFAT_is_asm_intrin, "ASM", true, is_first, CFAT_callee_kind_mask);
  p =   printAttr_(SideEffects, CFAT_is_dflt_ctor, "DCTR", true, is_first, CFAT_callee_kind_mask);
  p =   printAttr_(SideEffects, CFAT_is_dflt_cpy_ctor, "CCTR", true, is_first, CFAT_callee_kind_mask);

  fprintf(stderr,":%d]\n",NumOfKnownPars);
    
  // Now print per-argument attribute:
  for (INT32 i = 0 ; i < callnode->GetNumArguments(); i++)
  {
    UINT32 arg_attr = GetArgumentAttr(callnode,i);
    bool is_first = true;
    p = f<alse;    
    fprintf(stderr,"\t[%d]",i);
    p = printAttr_(arg_attr, CPA_one_level_read,"ord", false, is_first);
    p = (printAttr_(arg_attr, CPA_one_level_write,"owr", false, is_first) || p);
    p = (printAttr_(arg_attr, CPA_two_level_read,"trd", false, is_first) || p);
    p = (printAttr_(arg_attr, CPA_two_level_write,"twr", false, is_first) || p);
    p = (printAttr_(arg_attr, CPA_multi_level_read,"mrd", false, is_first) || p);
    p = (printAttr_(arg_attr, CPA_multi_level_write,"mwr", false, is_first) || p);
    p = (printAttr_(arg_attr, CPA_exposed_to_globals,"eg", false, is_first) || p);
    p = (printAttr_(arg_attr, CPA_exposed_to_return,"er", false, is_first) || p);
    p = (printAttr_(arg_attr, CPA_is_pointer_to_heap_addr_loc,"php", false, is_first) || p);
    p = (printAttr_(arg_attr, CPA_is_format_string,"fmt", false, is_first) || p);
    if (p) fprintf(stderr,"\n");
    else  fprintf(stderr,"\t---\n");
  }
  fprintf(stderr,"\n");
  fflush(stderr);
#endif
}

