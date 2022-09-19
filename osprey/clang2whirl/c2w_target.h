/*
  Copyright (C) 2019-2022 Xcalibyte (Shenzhen) Limited.

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

#ifndef CLANG2WHIRL_TARGET_H
#define CLANG2WHIRL_TARGET_H

#include "defs.h"
#include <string>
extern PREG_NUM Map_Reg_To_Preg[];

namespace wgen {
// max asm operands supported
enum {
  MAX_ASM_OPERANDS = 32,
};

enum {
  ASM_CLOBBER_MEMORY  = -4,   // 'memory'
  ASM_CLOBBER_CC      = -3,   // 'cc'
  ASM_CLOBBER_REG     = -2,   // any register
  ASM_CLOBBER_UNKNOWN = -1,   // unknown
};

INT  AsmDecodeRegisterName(const char *s);
BOOL AsmConstraintsByAddress(const char *s, const std::string *constraint_array);
void AsmIUpdateOperand(INT *opnd_map, char *constraint_string);

} // namespace wgen

#endif /* CLANG2WHIRL_TARGET_H */
