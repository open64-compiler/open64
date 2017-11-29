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
    
   lebdecodes64.c

    $Revision: 1.1.1.1 $  
    $Date: 2005/10/21 19:00:00 $
*/

#ifndef _LP64
#include <sgidefs.h>
#endif /* _LP64 */
#include <cmplrs/leb128.h>

#define TRUE 1
#define FALSE 0


/*
    This routine takes a pointer to an encoded signed leb128
    number, and returns the number of bytes in the encoding.
    In other words, it returns the length of the leb128 number.
    It also returns the number itself in *value.  The details
    of the leb128 encoding are in the Dwarf document.
*/
int
_leb128_signed_decode64(char *data, __int64_t	*value)
{
    unsigned char 	byte = *data;
    unsigned char    	sign = 0;
    __int64_t   lvalue;

    byte = *(data);
    if ((byte & 0x80) == 0) {
	lvalue = byte;
	sign = byte & 0x40;
    	if (sign ) {
	   lvalue |= - (1 << 7);
    	}
	*value = lvalue;
	return 1;
    }
    else if ((*(data + 1) & 0x80) == 0) {
	lvalue = *data & 0x7f;
	byte = *(data + 1);
	lvalue |= (byte & 0x7f) << 7;
	sign = byte & 0x40;
	if(sign) {
	   lvalue |= - (1 << 14);
	}
	*value = lvalue;
	return 2;
    }
    else if ((*(data + 2) & 0x80) == 0) {
	lvalue = *data & 0x7f;
	lvalue |= (*(data + 1) & 0x7f) << 7;
	byte = *(data +2);
	lvalue |= (byte & 0x7f) << 14;
	sign = byte & 0x40;
	if(sign) {
	   lvalue |= - (1 << 21);
	}
	*value = lvalue;
	return 3;
    }
    else if ((*(data + 3) & 0x80) == 0) {
	lvalue = *data & 0x7f;
	lvalue |= (*(data + 1) & 0x7f) << 7;
	lvalue |= (*(data + 2) & 0x7f) << 14;
	byte = *(data +3);
	lvalue |= (byte & 0x7f) << 21;
	sign = byte & 0x40;
	if(sign) {
	   lvalue |= - (1 << 28);
	}
	*value = lvalue;
	return 4;
    }
    else if ((*(data + 4) & 0x80) == 0) {
	lvalue = *data & 0x7f;
	lvalue |= (*(data + 1) & 0x7f) << 7;
	lvalue |= (*(data + 2) & 0x7f) << 14;
	lvalue |= (*(data + 3) & 0x7f) << 21;
	byte = *(data +4);
	lvalue |= ((__uint64_t)(byte & 0x7f)) << 28;
	sign = byte & 0x40;
	if(sign) {
	   lvalue |= - (1ULL << 35);
	}
	*value = lvalue;
	return 5;
    }
    else if ((*(data + 5) & 0x80) == 0) {
	lvalue = *data & 0x7f;
	lvalue |= (*(data + 1) & 0x7f) << 7;
	lvalue |= (*(data + 2) & 0x7f) << 14;
	lvalue |= (*(data + 3) & 0x7f) << 21;
	lvalue |= ((__uint64_t)(*(data + 4) & 0x7f)) << 28;
	byte = *(data +5);
	lvalue |= ((__uint64_t)(byte & 0x7f)) << 35;
	sign = byte & 0x40;
	if(sign) {
	   lvalue |= - (1ULL << 42);
	}
	*value = lvalue;
	return 6;
    }
    else if ((*(data + 6) & 0x80) == 0) {
	lvalue = *data & 0x7f;
	lvalue |= (*(data + 1) & 0x7f) << 7;
	lvalue |= (*(data + 2) & 0x7f) << 14;
	lvalue |= (*(data + 3) & 0x7f) << 21;
	lvalue |= ((__uint64_t)(*(data + 4) & 0x7f)) << 28;
	lvalue |= ((__uint64_t)(*(data + 5) & 0x7f)) << 35;
	byte = *(data +6);
	lvalue |= ((__uint64_t)(byte & 0x7f)) << 42;
	sign = byte & 0x40;
	if(sign) {
	   lvalue |= - (1ULL << 49);
	}
	*value = lvalue;
	return 7;
    }
    else if ((*(data + 7) & 0x80) == 0) {
	lvalue = *data & 0x7f;
	lvalue |= (*(data + 1) & 0x7f) << 7;
	lvalue |= (*(data + 2) & 0x7f) << 14;
	lvalue |= (*(data + 3) & 0x7f) << 21;
	lvalue |= ((__uint64_t)(*(data + 4) & 0x7f)) << 28;
	lvalue |= ((__uint64_t)(*(data + 5) & 0x7f)) << 35;
	lvalue |= ((__uint64_t)(*(data + 6) & 0x7f)) << 42;
	byte = *(data +7);
	lvalue |= ((__uint64_t)(byte & 0x7f)) << 49;
	sign = byte & 0x40;
	if(sign) {
	   lvalue |= - (1ULL << 56);
	}
	*value = lvalue;
	return 8;
    }
    else if ((*(data + 8) & 0x80) == 0) {
	lvalue = *data & 0x7f;
	lvalue |= (*(data + 1) & 0x7f) << 7;
	lvalue |= (*(data + 2) & 0x7f) << 14;
	lvalue |= (*(data + 3) & 0x7f) << 21;
	lvalue |= ((__uint64_t)(*(data + 4) & 0x7f)) << 28;
	lvalue |= ((__uint64_t)(*(data + 5) & 0x7f)) << 35;
	lvalue |= ((__uint64_t)(*(data + 6) & 0x7f)) << 42;
	lvalue |= ((__uint64_t)(*(data + 7) & 0x7f)) << 49;
	byte = *(data +8);
	lvalue |= ((__uint64_t)(byte & 0x7f)) << 56;
	sign = byte & 0x40;
	if(sign) {
	   /* here just apply the sign to the remaining bit */
	   lvalue |=  (1ULL << 63);
	}
	*value = lvalue;
	return 9;
    }
    {
	lvalue = *data & 0x7f;
	lvalue |= (*(data + 1) & 0x7f) << 7;
	lvalue |= (*(data + 2) & 0x7f) << 14;
	lvalue |= (*(data + 3) & 0x7f) << 21;
	lvalue |= ((__uint64_t)(*(data + 4) & 0x7f)) << 28;
	lvalue |= ((__uint64_t)(*(data + 5) & 0x7f)) << 35;
	lvalue |= ((__uint64_t)(*(data + 6) & 0x7f)) << 42;
	lvalue |= ((__uint64_t)(*(data + 7) & 0x7f)) << 49;
	lvalue |= ((__uint64_t)(*(data + 8) & 0x7f)) << 56;
	byte = *(data +9);
	/* this just applies one bit */
	lvalue |= ((__uint64_t)(byte & 0x7f)) << 63;
	/* there are no bits left over, so no need to
	** sign extend
	*/
	*value = lvalue;
	return 10;
    }
}
