/* 
   Copyright 2003, 2004, 2005, 2006 PathScale, Inc.  All Rights Reserved.
   File modified October 3, 2003 by PathScale, Inc. to update Open64 C/C++ 
   front-ends to GNU 3.3.1 release.
 */

/* Various diagnostic subroutines for the GNU C language.
   Copyright (C) 2000, 2001 Free Software Foundation, Inc.
   Contributed by Gabriel Dos Reis <gdr@codesourcery.com>

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

#include "config.h"
#include "system.h"
#ifdef SGI_MONGOOSE
// To get typdef tree
#include "rtl.h"
#endif /* SGI_MONGOOSE */
#include "tree.h"
#include "c-tree.h"
#include "tm_p.h"
#include "flags.h"
#include "diagnostic.h"

/* Issue an ISO C99 pedantic warning MSGID.  */

void
pedwarn_c99 VPARAMS ((const char *msgid, ...))
{
  diagnostic_info diagnostic;
#ifndef SGI_MONGOOSE
  VA_OPEN (ap, msgid);
  VA_FIXEDARG (ap, const char *, msgid);
#else
  va_list ap;

  VA_START (ap, msgid);
#endif /* SGI_MONGOOSE */

  diagnostic_set_info (&diagnostic, msgid, &ap, input_filename, lineno,
                       flag_isoc99 ? pedantic_error_kind () : DK_WARNING);
  report_diagnostic (&diagnostic);
#ifndef SGI_MONGOOSE
  VA_CLOSE (ap);
#else
  va_end (ap);
#endif /* SGI_MONGOOSE */
}

/* Issue an ISO C90 pedantic warning MSGID.  This function is supposed to
   be used for matters that are allowed in ISO C99 but not supported in
   ISO C90, thus we explicitly don't pedwarn when C99 is specified.
   (There is no flag_c90.)  */

void
pedwarn_c90 (const char *msgid, ...)
{
  diagnostic_info diagnostic;
  va_list ap;

  va_start (ap, msgid);
  diagnostic_set_info (&diagnostic, msgid, &ap, input_filename, lineno,
                       flag_isoc99 ? DK_WARNING : pedantic_error_kind ());
  report_diagnostic (&diagnostic);
  va_end (ap);
}
