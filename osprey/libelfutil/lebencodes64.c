/*

  Copyright (C) 2000, 2001 Silicon Graphics, Inc.  All Rights Reserved.

  This program is free software; you can redistribute it and/or modify it
  under the terms of version 2.1 of the GNU Lesser General Public License 
  as published by the Free Software Foundation.

  This program is distributed in the hope that it would be useful, but
  WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  

  Further, this software is distributed without any warranty that it is
  free of the rightful claim of any third person regarding infringement 
  or the like.  Any license provided herein, whether implied or 
  otherwise, applies only to this software file.  Patent licenses, if
  any, provided herein do not apply to combinations of this program with 
  other software, or any other product whatsoever.  

  You should have received a copy of the GNU Lesser General Public 
  License along with this program; if not, write the Free Software 
  Foundation, Inc., 59 Temple Place - Suite 330, Boston MA 02111-1307, 
  USA.

  Contact information:  Silicon Graphics, Inc., 1600 Amphitheatre Pky,
  Mountain View, CA 94043, or:

  http://www.sgi.com

  For further information regarding this notice, see:

  http://oss.sgi.com/projects/GenInfo/NoticeExplan

*/


/*
    lebencodes64.c

    $Revision: 1.1.1.1 $  
    $Date: 2005/10/21 19:00:00 $

*/

#ifndef _LP64
#include <sgidefs.h>
#endif /* _LP64 */
#include <cmplrs/leb128.h>


/* 
    This routine encodes the given signed number in leb128 mode,
    and stores the bytes at address buffer.  It returns the number 
    of bytes in the encoding.  It is the user's responsibility to
    make sure there is enough storage at buffer.  The details of
    leb128 encoding are in the Dwarf document.

    This relies on right shifting of negative values preserving
    the sign.  Implementation defined according to ISO C.

    10 bytes in buffer are all that is needed.
*/
int
_leb128_signed_encode64(__int64_t number, char *buffer)
{
    int		sign;
    char	*bufPtr;
    char	byte;

    sign = (number < 0) ? -1 : 0;
    bufPtr = buffer;

    for(;;) {
	byte = (number & 0x7f);
	number >>= 7;

	if (number == sign && (byte & 0x40) == ((char)sign & 0x40)) {
	    *bufPtr = byte;
	    bufPtr++;
    	    return bufPtr - buffer;
	}
	byte |= 0x80;
	*bufPtr = byte;
	bufPtr++;
    }

}
