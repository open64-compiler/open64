/* Output variables, constants and external declarations, for GNU compiler.
   Copyright (C) 1987, 1988, 1989, 1992, 1993, 1994, 1995, 1996, 1997,
   1998, 1999, 2000, 2001, 2002, 2003, 2004, 2005
   Free Software Foundation, Inc.

This file is part of GCC.

GCC is free software; you can redistribute it and/or modify it under
the terms of the GNU General Public License as published by the Free
Software Foundation; either version 2, or (at your option) any later
version.

GCC is distributed in the hope that it will be useful, but WITHOUT ANY
WARRANTY; without even the implied warranty of MERCHANTABILITY or
FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
for more details.

You should have received a copy of the GNU General Public License
along with GCC; see the file COPYING.  If not, write to the Free
Software Foundation, 59 Temple Place - Suite 330, Boston, MA
02111-1307, USA.  */

#include <ctype.h>
#include <stdlib.h>
#include <string.h>
#ifdef TARG_MIPS
#include "mips.h"
#else
#include "i386.h"
#endif

#define ARRAY_SIZE(x) (sizeof(x) / sizeof((x)[0]))

static const char *reg_names[] = REGISTER_NAMES;

/* Given NAME, a putative register name, discard any customary prefixes.  */

static const char *
strip_reg_name (const char *name)
{
  if (name[0] == '%' || name[0] == '#')
    name++;
  return name;
}

/* Decode an `asm' spec for a declaration as a register name.
   Return the register number, or -1 if nothing specified,
   or -2 if the ASMSPEC is not `cc' or `memory' and is not recognized,
   or -3 if ASMSPEC is `cc' and is not recognized,
   or -4 if ASMSPEC is `memory' and is not recognized.
   Accept an exact spelling or a decimal number.
   Prefixes such as % are optional.  */

int
gs_decode_reg_name (const char *asmspec)
{
  if (asmspec != 0)
    {
      int i;

      /* Get rid of confusing prefixes.  */
      asmspec = strip_reg_name (asmspec);

      /* Allow a decimal number as a "register name".  */
      for (i = strlen (asmspec) - 1; i >= 0; i--)
	if (! isdigit (asmspec[i]))
	  break;
      if (asmspec[0] != 0 && i < 0)
	{
	  i = atoi (asmspec);
	  if (i < FIRST_PSEUDO_REGISTER && i >= 0)
	    return i;
	  else
	    return -2;
	}

      for (i = 0; i < FIRST_PSEUDO_REGISTER; i++)
	if (reg_names[i][0]
	    && ! strcmp (asmspec, strip_reg_name (reg_names[i])))
	  return i;

#ifdef ADDITIONAL_REGISTER_NAMES
      {
	static const struct { const char *const name; const int number; } table[]
	  = ADDITIONAL_REGISTER_NAMES;

	for (i = 0; i < (int) ARRAY_SIZE (table); i++)
	  if (! strcmp (asmspec, table[i].name))
	    return table[i].number;
      }
#endif /* ADDITIONAL_REGISTER_NAMES */

      if (!strcmp (asmspec, "memory"))
	return -4;

      if (!strcmp (asmspec, "cc"))
	return -3;

      return -2;
    }

  return -1;
}
