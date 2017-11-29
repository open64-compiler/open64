/*
Copyright 2004, 2005 PathScale, Inc.  All Rights Reserved.

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
*/
#ifdef KEY /* Bug 1683 */

#include <sys/types.h>
#include <unistd.h>
#include <sys/wait.h>
#if defined(BUILD_OS_DARWIN)
#include <stdlib.h>
#else /* defined(BUILD_OS_DARWIN) */
#include <malloc.h>
#endif /* defined(BUILD_OS_DARWIN) */
#include "cmplrs/f_errno.h"
#include <sys/param.h>
#include "externals.h"

#include "pathf90_libU_intrin.h"

/* Provide this for backward compatibility, and in case somebody declares
 * it "external" in Fortran but expects to get it from the library instead
 * of defining it themselves.
 */
extern __int32_t
chmod_ (char *name, char *mode, __int32_t namlen, __int32_t modlen) {
  return pathf90_chmod(name, mode, 0, namlen, modlen);
  }

#endif /* KEY Bug 1683 */
