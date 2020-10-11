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

#include "c2w_target.h"
#include "c2w_target_x86-64.h"
#include "errors.h"
#include <ctype.h>

namespace wgen {

static const char *
StripRegisterName(const char *s) {
  if (*s == '%' || *s == '#')
    return s + 1;  // skip '%' or '#'
  return s;
}

INT
AsmDecodeRegisterName(const char *s) {
  if (s == NULL || *s == '\0')
    return ASM_CLOBBER_UNKNOWN;
  s = StripRegisterName(s);
  INT i;
  for (i = strlen(s) - 1; i >= 0; --i) {
    if (!isdigit(s[i]))
      break;
  }

  if (s[0] != '\0' && i < 0) {
    INT reg = atoi(s);
    if (reg >= 0 && reg < FIRST_PSEUDO_REGISTER)
      return reg;
    else
      return ASM_CLOBBER_REG;
  }

  if (strcmp(s, "memory") == 0)
    return ASM_CLOBBER_MEMORY;

  if (strcmp(s, "cc") == 0)
    return ASM_CLOBBER_CC;

  {
    static const char* reg_names[] = REGISTER_NAMES;
    for (i = 0; i < FIRST_PSEUDO_REGISTER; ++i) {
      if (reg_names[i][0] &&
          strcmp(s, StripRegisterName(reg_names[i])) == 0)
        return i;
    }
  }

#ifdef ADDITIONAL_REGISTER_NAMES
  {
    static const struct {
      const char *const name;
      const int number;
    } table[] = ADDITIONAL_REGISTER_NAMES;

    for (i = 0; i < sizeof(table)/sizeof(table[0]); ++i) {
      if (table[i].name &&
          strcmp(s, StripRegisterName(table[i].name)) == 0)
        return table[i].number;
    }
  }
#endif /* ADDITIONAL_REGISTER_NAMES */

  return ASM_CLOBBER_REG;
}

BOOL
AsmConstraintsByAddress(const char *s, const std::string *constraint_array) {
  if (strchr(s, 'm') || strchr(s, 'g'))
    return TRUE;
  if (isdigit(*s)) {
    Is_True(*s - '0' < MAX_ASM_OPERANDS, ("asm operands oob"));
    const std::string ns = constraint_array[*s - '0'];
    return AsmConstraintsByAddress(ns.c_str(), constraint_array);
  }
  return FALSE;
}

void
AsmUpdateOperand(INT *opnd_map, char *constraint_string) {
  char *p = constraint_string;
  for (; *p != '\0'; ++p) {
    if (*p >= '0' && *p <= '9') {
      INT old_index = *p - '0';
      INT new_index = opnd_map[old_index];
      Is_True(new_index >= 0 && new_index <= old_index,
              ("wrong index in opnd map"));
      *p = new_index + '0';
    }
  }
}

} // namespace wgen
