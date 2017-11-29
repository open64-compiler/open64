/*
   Copyright (C) 2009 Advanced Micro Devices, Inc.  All Rights Reserved.

   The Open64 Runtime Library is free software; you can redistribute it
   and/or modify it under the terms of the GNU Lesser General Public
   License as published by the Free Software Foundation; either
   version 2.1 of the License, or (at your option) any later version.

   The Open64 Runtime Library is distributed in the hope that it will be
   useful, but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
   Lesser General Public License for more details.

   You should have received a copy of the GNU Lesser General Public
   License along with the Open64 Runtime Library; if not, write to the
   Free Software Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA
   02111-1307 USA.
*/

#include <malloc.h>
#include <stdlib.h>

void __pathscale_malloc_alg(int mode)
{
    if (getenv("OPEN64_NO_MALLOC_ALG") != NULL)
	return;
	
    if (mode < 1)
        mode = 1;
    else if (mode > 4) {
        mode = 4;
    }
    switch (mode) {
    case 1:
        mallopt(M_MMAP_MAX, 0x2);
        mallopt(M_TRIM_THRESHOLD, 0x10000000);
        break;
    case 2:
        mallopt(M_MMAP_MAX, 0x2);
        mallopt(M_TRIM_THRESHOLD, 0x40000000);
        break;
    case 3:
        mallopt(M_MMAP_MAX, 0x0);
        mallopt(M_TRIM_THRESHOLD, 0xffffffff);
        break;
    case 4:
        mallopt(M_MMAP_MAX, 0x2);
        mallopt(M_TRIM_THRESHOLD, 0x10000000);
        break;
    }
}
