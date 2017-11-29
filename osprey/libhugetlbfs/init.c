/*
 * Copyright (C) 2008 Advanced Micro Devices, Inc.  All Rights Reserved.
 */

/*
 * libhugetlbfs - Easy use of Linux hugepages
 * Copyright (C) 2008 Nishanth Aravamudan, IBM Corporation
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public License
 * as published by the Free Software Foundation; either version 2.1 of
 * the License, or (at your option) any later version.
 *
 * This library is distributed in the hope that it will be useful, but
 * WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public
 * License along with this library; if not, write to the Free Software
 * Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA 02110-1301 USA
 */

#include "libhugetlbfs_internal.h"

#ifdef OPEN64_MOD

void __attribute__ ((constructor)) setup_libhugetlbfs(void)
#else
static void __attribute__ ((constructor)) setup_libhugetlbfs(void)
#endif
{
	__hugetlbfs_setup_debug();

#ifdef OPEN64_MOD
        hugepages_seg_total = 0;
        heapbase = 0;
        heaptop = 0;
        mapsize = 0;
#endif
        
#ifndef NO_ELFLINK
	__hugetlbfs_setup_elflink();
#endif
#ifndef OPEN64_MOD
	__hugetlbfs_setup_morecore();
#endif
}
