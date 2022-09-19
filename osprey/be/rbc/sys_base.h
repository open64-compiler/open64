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

#ifndef __RBC_SYS_BASE__
#define __RBC_SYS_BASE__

/*
 * For any types or declarations, if it's used in internal rule
 * implementation like certc/certcpp, append it into this file.
 * so that certc/certcpp won't include system header files.
 * otherwise there might be compilation error in different system
 * header files.
 *
 * Notice:
 * There will be eigher 32-bit or 64-bit rule, make sure these
 * definitions in this file works for both 32-bit or 64-bit.
 */

#ifdef LANG_JAVA
# error "This file can't be compiled for JAVA"
#endif

/*
 * scalar type definition
 */
#ifdef __LP64__
typedef unsigned long int size_t;
typedef long int ssize_t;
typedef long int __intmax_t;
typedef unsigned long int __uintmax_t;
#else
typedef unsigned int size_t;
typedef int ssize_t;
__extension__ typedef long long int __intmax_t;
__extension__ typedef unsigned long long int __uintmax_t;
#endif
typedef long int off_t;
typedef long int time_t;
typedef unsigned int wint_t;
typedef unsigned int socklen_t;
typedef unsigned int mode_t;
typedef unsigned int uid_t;
typedef unsigned int gid_t;
typedef __intmax_t  intmax_t;
typedef __uintmax_t uintmax_t;

/*
 * struct type definition
 */
typedef __builtin_va_list va_list;
typedef struct _IO_FILE FILE;
typedef void (*__sighandler_t)(int);
typedef __sighandler_t sighandler_t;
typedef struct __dirstream DIR;
typedef struct _G_fpos_t fpos_t;
struct mbstate_t;

/*
 * constant macro definition
 */
#define NULL            (0)
#define O_RDONLY        00000000
#define O_EXCL	        00000200
#define MAP_FAILED      ((void *) -1)
#define EOF             (-1)
#define SO_REUSEADDR    2

#endif /* __RBC_SYS_BASE__ */
