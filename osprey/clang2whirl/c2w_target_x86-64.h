/*
  Copyright (C) 2019-2020 XC5 Limited, Inc.  All Rights Reserved.

  This program is free software; you can redistribute it and/or modify it
  under the terms of version 2 of the GNU General Public License as
  published by the Free Software Foundation.

  This program is distributed in the hope that it would be useful, but
  WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  

  Further, this software is distributed without any warranty that it is
  free of the rightful claim of any third person regarding infringement 
  or the like.  Any license provided herein, whether implied or 
  otherwise, applies only to this software file.  Patent licenses, if 
  any, provided herein do not apply to combinations of this program with 
  other software, or any other product whatsoever.  

  You should have received a copy of the GNU General Public License along
  with this program; if not, write the Free Software Foundation, Inc., 59
  Temple Place - Suite 330, Boston MA 02111-1307, USA.

  http://www.xcalibyte.com

  For more information, see:
  http://github.com/open64-compiler/open64
  http://gitee.com/open64-compiler/open64

*/

#ifdef CLANG2WHIRL_TARGET_X86_64_INC
#error CLANG2WHIRL_TARGET_X86_64_INC already defined. This file should only be included once
#endif
#define CLANG2WHIRL_TARGET_X86_64_INC

enum {
 FIRST_PSEUDO_REGISTER = 53,
};

#define REGISTER_NAMES                                                  \
{"ax","dx","cx","bx","si","di","bp","sp",                               \
 "st","st(1)","st(2)","st(3)","st(4)","st(5)","st(6)","st(7)",          \
 "argp", "flags", "fpsr", "dirflag", "frame",                           \
 "xmm0","xmm1","xmm2","xmm3","xmm4","xmm5","xmm6","xmm7",               \
 "mm0", "mm1", "mm2", "mm3", "mm4", "mm5", "mm6", "mm7" ,               \
 "r8", "r9", "r10", "r11", "r12", "r13", "r14", "r15",                  \
 "xmm8", "xmm9", "xmm10", "xmm11", "xmm12", "xmm13", "xmm14", "xmm15"}

#define ADDITIONAL_REGISTER_NAMES \
{ { "eax", 0 }, { "edx", 1 }, { "ecx", 2 }, { "ebx", 3 },       \
  { "esi", 4 }, { "edi", 5 }, { "ebp", 6 }, { "esp", 7 },       \
  { "rax", 0 }, { "rdx", 1 }, { "rcx", 2 }, { "rbx", 3 },       \
  { "rsi", 4 }, { "rdi", 5 }, { "rbp", 6 }, { "rsp", 7 },       \
  { "al", 0 }, { "dl", 1 }, { "cl", 2 }, { "bl", 3 },           \
  { "ah", 0 }, { "dh", 1 }, { "ch", 2 }, { "bh", 3 } }

