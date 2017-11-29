/********************************************************************\
|*                                                                  *|   
|*  Copyright (c) 2006 by SimpLight Nanoelectronics.                *|
|*  All rights reserved                                             *|
|*                                                                  *|
|*  This program is free software; you can redistribute it and/or   *|
|*  modify it under the terms of the GNU General Public License as  *|
|*  published by the Free Software Foundation; either version 2,    *|
|*  or (at your option) any later version.                          *|
|*                                                                  *|
\********************************************************************/

#include <stdint.h>
#include "elf_stuff.h"
#include "defs.h"
#include "sections.h"

#ifdef SHF_IA_64_SHORT
#undef SHF_IA_64_SHORT
#define SHF_IA_64_SHORT	SHF_MIPS_GPREL
#endif
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
     0|SHF_WRITE|SHF_MIPS_GPREL|SHF_ALLOC,
	SHT_PROGBITS, 0, 
     INT32_MAX, MIPS_SDATA, 0},
  {_SEC_LDATA,	NULL,
     0|SHF_WRITE|SHF_ALLOC|SHF_MIPS_LOCAL,
	SHT_PROGBITS, 0, 
     INT64_MAX, ".MIPS.ldata", 0},
  {_SEC_RDATA,	NULL,
     0|SHF_ALLOC,
	SHT_PROGBITS, 0, 
//  INT64_MAX, ELF_RODATA, 0},
//	Temporary move rdata to small data
    INT64_MAX, MIPS_RDATA, 0},
  {_SEC_SRDATA,	NULL,
     0|SHF_MIPS_GPREL|SHF_ALLOC,
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
     INT32_MAX, MIPS_SBSS, 0},
#ifndef linux
  {_SEC_LBSS,	NULL,
     0|SHF_WRITE|SHF_ALLOC|SHF_MIPS_LOCAL,
	SHT_NOBITS, 0, 
     INT64_MAX, MIPS_LBSS, 0},
#else
  // There is no MIPS_LBSS section on Linux, but we need a space holder
  {_SEC_LBSS,   NULL,
     0,
        0, 0,
     0, ".unknown", 0},
#endif
  {_SEC_GOT,	NULL,
     0|SHF_IA_64_SHORT|SHF_ALLOC,
	SHT_PROGBITS, 0, 
     INT32_MAX, ELF_GOT, 0},
  {_SEC_CPLINIT,	NULL,
     0|SHF_WRITE|SHF_ALLOC|SHF_MIPS_NAMES,
	SHT_PROGBITS, 0, 
     INT64_MAX, "__cplinit", 0},
#ifndef linux
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
#endif
#endif
  {_SEC_DISTR_ARRAY,  NULL,
     0|SHF_WRITE|SHF_ALLOC|SHF_MIPS_NAMES,
	SHT_PROGBITS, 0,
     INT64_MAX, "_MIPS_distr_array", 0},
#ifdef TARG_SL
    {_SEC_SSDATA,	NULL,
     0|SHF_WRITE|SHF_MIPS_GPREL|SHF_ALLOC,
	SHT_PROGBITS, 0, 
     INT32_MAX, MIPS_SSDATA, 0},

     {_SEC_VSDATA,	NULL,
     0|SHF_WRITE|SHF_MIPS_GPREL|SHF_ALLOC,
	SHT_PROGBITS, 0, 
     INT32_MAX, MIPS_VSDATA, 0},


     {_SEC_VS1DATA,	NULL,
     0|SHF_WRITE|SHF_MIPS_GPREL|SHF_ALLOC,
	SHT_PROGBITS, 0, 
     INT32_MAX, MIPS_VS1DATA, 0},
     
     {_SEC_VS2DATA,	NULL,
     0|SHF_WRITE|SHF_MIPS_GPREL|SHF_ALLOC,
	SHT_PROGBITS, 0, 
     INT32_MAX, MIPS_VS2DATA, 0},
     
     {_SEC_VS4DATA,	NULL,
     0|SHF_WRITE|SHF_MIPS_GPREL|SHF_ALLOC,
	SHT_PROGBITS, 0, 
     INT32_MAX, MIPS_VS4DATA, 0},     
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
	return (SEC_flags(sec) & SHF_IA_64_SHORT);
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
        INT t = (SEC_flags(sec) & SHF_TLS);
	Is_True(t == 0, ("SL cannot has TLS sections"));
	return FALSE;
}
