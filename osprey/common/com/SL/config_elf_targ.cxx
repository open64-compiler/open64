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

/* ====================================================================
 * ====================================================================
 *
 * Module: config_elf_targ.cxx
 * $Revision: 1.1 $
 * $Date: 2005/07/27 02:18:02 $
 * $Author: kevinlo $
 * $Source: /depot/CVSROOT/javi/src/sw/cmplr/common/com/MIPS/config_elf_targ.cxx,v $
 *
 * Description:
 *
 * ELF configuration specific to the target machine/system.
 *
 * ====================================================================
 * ====================================================================
 */

#include "elf_stuff.h"
#include <sys/elf_whirl.h>
#include "defs.h"
#include "erglob.h"
#include "tracing.h"
#include "config_elf_targ.h"

/* ====================================================================
 *
 * Config_Target_From_ELF
 *
 * Based on the flags in the ELF header <ehdr>,
 * return whether is 64bit and the isa level.
 *
 * ====================================================================
 */
void Config_Target_From_ELF (Elf64_Word e_flags, BOOL *is_64bit, INT *isa)
{
  *is_64bit = (e_flags & EF_IA64_ABI64);

  *isa = 1;
}


/* ====================================================================
 *
 * Config_ELF_From_Target
 *
 * Return the ELF specific flags for the current target ABI and ISA subset.
 *
 * ====================================================================
 */
Elf32_Word Config_ELF_From_Target(BOOL is_64bit, BOOL old_abi, INT isa)
{
  Elf32_Word e_flags = 0;

  if (is_64bit) e_flags |= EF_IA64_ABI64;

  return e_flags;
}

Elf32_Half Get_Elf_Target_Machine (void)
{
// From /usr/include/elf.h
#define EM_MIPS_RS3_LE 10 /* MIPS R3000 little-endian */
  return EM_MIPS_RS3_LE;
}

