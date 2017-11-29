#pragma ident "@(#)92/msgnew/catgetmsg.c	92.2	06/03/99 09:59:18"

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


#ident  "$Header: /proj/osprey/CVS/open64/osprey1.0/libcsup/msgnew/catgetmsg.c,v 1.1.1.1 2005/10/21 19:00:00 marcel Exp $"

#if ! defined(BUILD_OS_DARWIN)
/* Mach-O doesn't implement aliases; Linux version ignores these anyway
 * because there's no C declaration of the alias id */
#if defined(__STDC__) && defined(_LITTLE_ENDIAN)
	#pragma weak _catgetmsg = catgetmsg
#else				/* else STDC and _LITTLE_ENDIAN */
#if defined(__STDC__) && !defined(_LIBU)
        #pragma weak catgetmsg = _catgetmsg
        #pragma weak __catgetmsg_error_code = ___catgetmsg_error_code
#endif
#endif
#endif				/* end STDC and _LITTLE_ENDIAN */

/*
 * IMPORTANT:
 * This section is needed since this file also resides in the compilers'
 * libcsup/msg (v7.2 and higher). Once the compilers drop support for
 * pre-IRIX 6.5 releases this can be removed. Please build a libu before
 * checking in any changes to this file.
 *
 */

#ifdef	_LITTLE_ENDIAN
#include <nl_types.h>
#include <cray/nlcatmsg.h>
#include <string.h>
#else				/* Else _LITTLE_ENDIAN */
#ifndef _LIBU
#include "synonyms.h"
#endif

#define __NLS_INTERNALS 1
#include <nl_types.h>
#undef __NLS_INTERNALS
#include <string.h>
#endif				/* End _LITTLE_ENDIAN */

/*
 * catgetmsg -- retrieves a message to a user supplied buffer from a catalog
 *
 */
char *
catgetmsg(
	  nl_catd catd,
	  int set_num,
	  int msg_num,
	  char *buf,
	  int buflen
	  )
{
  size_t len;
  char *str;
  
  if ((str = catgets(catd, set_num, msg_num, NULL)) != NULL) {
    
    /* find the proper string length to copy */
    len = strlen(str); 
    if (len >= buflen)
      len = buflen - 1;
    
    (void) strncpy(buf, str, len);
    buf[len] = '\0';
    
    return buf;
  } else {
    return "\0";
  }
}


#ifndef	_LITTLE_ENDIAN
/*
 *      __catgetmsg_error_code - returns the error status from the last
 *              failed catgetmsg() call.
 *
 *              returns  < 0    Internal error code
 *                       > 0    System error code
 */
int
__catgetmsg_error_code(void)
{
        return(__catgets_error_code());
}
#endif				/* End NOT _LITTLE_ENDIAN */
