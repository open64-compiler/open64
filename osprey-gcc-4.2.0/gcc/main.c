/*
 * Copyright (C) 2007. QLogic Corporation. All Rights Reserved.
 */
/* main.c: defines main() for cc1, cc1plus, etc.

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
Software Foundation, 51 Franklin Street, Fifth Floor, Boston, MA
02110-1301, USA.  */

#include "config.h"
#include "system.h"
#include "coretypes.h"
#include "tm.h"
#include "toplev.h"

int main (int argc, char **argv);

/* We define main() to call toplev_main(), which is defined in toplev.c.
   We do this in a separate file in order to allow the language front-end
   to define a different main(), if it so desires.  */

#ifdef KEY
/* Defined in tree.c */
extern int    gs_argc;
extern char **gs_argv;
#endif

int
main (int argc, char **argv)
{
#ifdef KEY
  int i;
  gs_argc = argc;
  gs_argv = (char **) xmalloc (argc * sizeof (char *));
  for (i = 0; i < argc; i++)
    gs_argv[i] = xstrdup (argv[i]);
#endif

  return toplev_main (argc, (const char **) argv);
}
