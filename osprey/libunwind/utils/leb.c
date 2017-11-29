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



#include <stdlib.h>
#include <stdio.h>
#include <sys/unwind.h>



/* little-endian base 128 variable-length decoding */
__uint64_t __leb128_decode(char *ptr, __uint64_t size, __uint64_t *val) {
        __uint64_t num = 0L;

	if (num == size)
		return 0;
	*val = 0L;
        while (*ptr & 0x80) {
                *val += ((__uint64_t)(*ptr++ & 0x7f) << (7*num++));
		if (num == size)
			return 0;
	}
        *val += ((__uint64_t)(*ptr & 0x7f) << (7*num++));
        return num;
}



/* little-endian base 128 variable-length encoding */
__uint64_t __leb128_encode(char *ptr, __uint64_t size, __uint64_t val) {
        __uint64_t num = 0L;

        do {
                if (num == size)
                        return 0;
                *ptr = (char)(val & 0x000000000000007fL);
                *ptr |= 0x80;
                val >>= 7;
                num++;
                ptr++;
        } while (val);
        *--ptr &= 0x7f;
        return num;
}
