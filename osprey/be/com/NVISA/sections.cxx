/*
 * Copyright 2005-2008 NVIDIA Corporation.  All rights reserved.
 */

/*
 * Copyright 2003, 2004, 2005 PathScale, Inc.  All Rights Reserved.
 */

/*

  Copyright (C) 2000, 2001 Silicon Graphics, Inc.  All Rights Reserved.

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

  Contact information:  Silicon Graphics, Inc., 1600 Amphitheatre Pky,
  Mountain View, CA 94043, or:

  http://www.sgi.com

  For further information regarding this notice, see:

  http://oss.sgi.com/projects/GenInfo/NoticeExplan

*/


#include <stdint.h>
#include "elf_stuff.h"
#include "sections.h"

#define INST_BYTES 16

SECTION Sections[_SEC_INDEX_MAX] = {
  {_SEC_UNKNOWN,NULL,
     0,
	0, 0, 
     0, ".unknown", 0},
  {_SEC_TEXT,	NULL,
     0|SHF_EXECINSTR|SHF_ALLOC,
	SHT_PROGBITS, INST_BYTES, 
     INT64_MAX, ELF_TEXT, 0},
  {_SEC_DATA,	NULL,
     0|SHF_WRITE|SHF_ALLOC, 
	SHT_PROGBITS, 0, 
     INT64_MAX, ELF_DATA, 0},
  {_SEC_SDATA,	NULL,
     0|SHF_WRITE|SHF_IA_64_SHORT|SHF_ALLOC,
	SHT_PROGBITS, 0, 
     INT32_MAX, MIPS_SDATA, 0},
  {_SEC_LDATA,	NULL,
     0|SHF_WRITE|SHF_ALLOC|SHF_MIPS_LOCAL,
	SHT_PROGBITS, 0, 
     INT64_MAX, ".MIPS.ldata", 0},
  {_SEC_RDATA,	NULL,
     0|SHF_ALLOC,
	SHT_PROGBITS, 0, 
     INT64_MAX, ELF_RODATA, 0},
  {_SEC_SRDATA,	NULL,
     0|SHF_IA_64_SHORT|SHF_ALLOC,
	SHT_PROGBITS, 0, 
     INT32_MAX, MIPS_SRDATA, 0},
  {_SEC_LIT4,	NULL,
     0|SHF_IA_64_SHORT|SHF_ALLOC|SHF_MIPS_MERGE,
	SHT_PROGBITS, 4, 
     INT32_MAX, MIPS_LIT4, 0},
  {_SEC_LIT8,	NULL,
     0|SHF_IA_64_SHORT|SHF_ALLOC|SHF_MIPS_MERGE,
	SHT_PROGBITS, 8, 
     INT32_MAX, MIPS_LIT8, 0},
  {_SEC_LIT16,	NULL,
     0|SHF_IA_64_SHORT|SHF_ALLOC|SHF_MIPS_MERGE,
	SHT_PROGBITS, 16, 
     INT32_MAX, MIPS_LIT16, 0},
  {_SEC_BSS,	NULL,
     0|SHF_WRITE|SHF_ALLOC,
	SHT_NOBITS, 0, 
     INT64_MAX, ELF_BSS, 0},
  {_SEC_SBSS,	NULL,
     0|SHF_WRITE|SHF_IA_64_SHORT|SHF_ALLOC,
	SHT_NOBITS, 0, 
     INT64_MAX, MIPS_SBSS, 0},
#ifdef TARG_MIPS
  {_SEC_LBSS,	NULL,
     0|SHF_WRITE|SHF_ALLOC|SHF_MIPS_LOCAL,
	SHT_NOBITS, 0, 
     INT64_MAX, MIPS_LBSS, 0},
#else
  {_SEC_LBSS,   NULL,
     0|SHF_WRITE|SHF_ALLOC|SHF_TLS,
        SHT_NOBITS, 0,
     INT64_MAX, ELF_TBSS, 0},
#endif
  {_SEC_GOT,	NULL,
     0|SHF_IA_64_SHORT|SHF_ALLOC,
	SHT_PROGBITS, 0, 
     INT32_MAX, ELF_GOT, 0},
  {_SEC_CPLINIT,	NULL,
     0|SHF_WRITE|SHF_ALLOC|SHF_MIPS_NAMES,
	SHT_PROGBITS, 0, 
     INT64_MAX, "__cplinit", 0},
#ifdef TARG_MIPS
  {_SEC_EH_REGION,	NULL,
     0|SHF_WRITE|SHF_ALLOC|SHF_MIPS_NAMES,
	SHT_PROGBITS, 0, 
     INT64_MAX, MIPS_EH_REGION, 0},
  {_SEC_EH_REGION_SUPP,	NULL,
     0|SHF_WRITE|SHF_ALLOC|SHF_MIPS_NAMES,
	SHT_PROGBITS, 0, 
     INT64_MAX, MIPS_EH_REGION_SUPP, 0},
#else
#ifdef KEY
  {_SEC_EH_REGION,      NULL,
     0|SHF_WRITE|SHF_ALLOC|SHF_MIPS_NAMES,
        SHT_PROGBITS, 0,
     INT64_MAX, ".except_table", 0},
  {_SEC_EH_REGION_SUPP, NULL,
     0|SHF_WRITE|SHF_ALLOC|SHF_MIPS_NAMES,
        SHT_PROGBITS, 0,
     INT64_MAX, ".except_table_supp", 0},
#else
  // It's not yet clear what to do about the EH_REGION sections on Linux
  {_SEC_EH_REGION,      NULL,
     0,
        0, 0,
     0, ".unknown", 0},
  {_SEC_EH_REGION_SUPP, NULL,
     0,
        0, 0,
     0, ".unknown", 0},
#endif // KEY
#endif
  {_SEC_DISTR_ARRAY,  NULL,
     0|SHF_WRITE|SHF_ALLOC|SHF_MIPS_NAMES,
	SHT_PROGBITS, 0,
     INT64_MAX, "_MIPS_distr_array", 0},
#ifdef TARG_X8664
  {_SEC_DATA_REL_RO,  NULL,
     0|SHF_WRITE|SHF_ALLOC,
	SHT_PROGBITS, 0,
     INT64_MAX, ".data.rel.ro.local", 0},
#endif
};

extern SECTION_IDX
Corresponding_Short_Section (SECTION_IDX sec)
{
   switch ( sec ) {
   case _SEC_DATA:      return _SEC_SDATA;
   case _SEC_RDATA:     return _SEC_SRDATA;
   case _SEC_BSS:       return _SEC_SBSS;
   default:		return sec;
   }
}

extern BOOL
SEC_is_gprel (SECTION_IDX sec)
{
  //return (SEC_flags(sec) & SHF_IA_64_SHORT);
  return FALSE;
}

extern BOOL
SEC_is_merge (SECTION_IDX sec)
{
	return (SEC_flags(sec) & SHF_MIPS_MERGE);
}

extern BOOL
SEC_is_exec (SECTION_IDX sec)
{
	return (SEC_flags(sec) & SHF_EXECINSTR);
}

extern BOOL
SEC_is_nobits (SECTION_IDX sec)
{
	return (SEC_type(sec) & SHT_NOBITS);
}

extern BOOL
SEC_is_tls (SECTION_IDX sec)
{
        return (SEC_flags(sec) & SHF_TLS);
}
