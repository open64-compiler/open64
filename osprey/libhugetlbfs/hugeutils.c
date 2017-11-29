/*
 * Copyright (C) 2008-2009 Advanced Micro Devices, Inc.  All Rights Reserved.
 */

/*
 * libhugetlbfs - Easy use of Linux hugepages
 * Copyright (C) 2005-2006 David Gibson & Adam Litke, IBM Corporation.
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

#define _LARGEFILE64_SOURCE /* Need this for statfs64 */
#define _GNU_SOURCE
#include <dlfcn.h>
#include <features.h>

#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include <errno.h>
#include <limits.h>
#include <string.h>
#include <ctype.h>
#include <signal.h>

#include <unistd.h>
#include <fcntl.h>
#include <sys/vfs.h>
#include <sys/statfs.h>
#include <sys/types.h>
#include <sys/mman.h>
#include <sys/file.h>

#include "libhugetlbfs_internal.h"
#include "hugetlbfs.h"

static long hpage_size; /* = 0 */
static char htlb_mount[PATH_MAX+1]; /* = 0 */
static int hugepagesize_errno; /* = 0 */


/********************************************************************/
/* Internal functions                                               */
/********************************************************************/

#define BUF_SZ 256
#define MEMINFO_SIZE	2048

static long read_meminfo(const char *tag)
{
	int fd;
	char buf[MEMINFO_SIZE];
	int len, readerr;
	char *p, *q;
	long val;

	fd = open("/proc/meminfo", O_RDONLY);

	if (fd < 0) {
		ERROR("Couldn't open /proc/meminfo (%s)\n", strerror(errno));
		return -1;
	}

	len = read(fd, buf, sizeof(buf));
	readerr = errno;
	close(fd);
	if (len < 0) {
		ERROR("Error reading /proc/meminfo (%s)\n", strerror(readerr));
		return -1;
	}
	if (len == sizeof(buf)) {
		ERROR("/proc/meminfo is too large\n");
		return -1;
	}
	buf[len] = '\0';

	p = strstr(buf, tag);
	if (!p)
		return -1; /* looks like the line we want isn't there */

	p += strlen(tag);
	errno = 0;
	val = strtol(p, &q, 0);
	if (errno != 0) {
		if (errno == ERANGE && val == LONG_MAX)
			ERROR("Value of %s in /proc/meminfo overflows long\n", tag);
		else
			ERROR("strtol() failed (%s)\n", strerror(errno));
		return -1;
	}
	if (! isspace(*q)) {
		ERROR("Couldn't parse /proc/meminfo value\n");
		return -1;
	}

	return val;
}

#ifdef OPEN64_MOD
static long read_sysctrl(const char * tag)
{
    int fd;
    char buf[MEMINFO_SIZE];
    int len, readerr;
    long val;

    fd = open(tag, O_RDONLY);

    if (fd < 0)
        return 0;

    len = read(fd, buf, sizeof(buf));
    readerr = errno;
    close(fd);

    if (len < 0) 
        return 0;

    if (len == sizeof(buf)) {
        ERROR("%s is too large\n", tag);
        return -1;
    }

    buf[len] = '\0';

    val = strtol(buf, 0, 0);

    return val;
}
#endif

/********************************************************************/
/* Library user visible functions                                   */
/********************************************************************/

/*
 * returns:
 *   on success, size of a huge page in number of bytes
 *   on failure, -1
 *	errno set to ENOSYS if huge pages are not supported
 *	errno set to EOVERFLOW if huge page size would overflow return type
 */
long gethugepagesize(void)
{
	long hpage_kb;
	long max_hpage_kb = LONG_MAX / 1024;

#if defined(OPEN64_MOD) && defined(M_PAGE)
        switch (hugepage_m_stype) {
        case SIZE_2M:
            return BLOCKSIZE_2M;
        case SIZE_1G:
            return BLOCKSIZE_1G;
        default:
            ERROR("Illegal huge page size");
        }
#endif
	if (hpage_size) {
		errno = hugepagesize_errno;
		return hpage_size;
	}
	errno = 0;

	hpage_kb = read_meminfo("Hugepagesize:");
	if (hpage_kb < 0) {
		hpage_size = -1;
		errno = hugepagesize_errno = ENOSYS;
	} else {
		if (hpage_kb > max_hpage_kb) {
			/* would overflow if converted to bytes */
			hpage_size = -1;
			errno = hugepagesize_errno = EOVERFLOW;
		}
		else
			/* convert from kb to bytes */
			hpage_size = 1024 * hpage_kb;
	}

	return hpage_size;
}

int hugetlbfs_test_path(const char *mount)
{
	struct statfs64 sb;
	int err;

	/* Bugs in the 32<->64 translation code in pre-2.6.15 kernels
	 * mean that plain statfs() returns bogus errors on hugetlbfs
	 * filesystems.  Use statfs64() to work around. */
	err = statfs64(mount, &sb);
	if (err)
		return -1;

#if !defined(OPEN64_MOD) || !defined(M_PAGE)
	return (sb.f_type == HUGETLBFS_MAGIC);
#else
        switch (hugepage_m_stype) {
        case SIZE_1G:
            return ((sb.f_type == HUGETLBFS_MAGIC) && (sb.f_bsize == BLOCKSIZE_1G));
        case SIZE_2M:
            return ((sb.f_type == HUGETLBFS_MAGIC) && (sb.f_bsize == BLOCKSIZE_2M));
        default:
            ERROR("Unknow type of huge page to match\n");
            return 0;
        }
#endif
}

#define LINE_MAXLEN	2048

const char *hugetlbfs_find_path(void)
{
	int fd;
	char line[LINE_MAXLEN + 1];
	char *eol;
	int bytes, err, dummy;
	off_t offset;

	/* Have we already located a mount? */
	if (*htlb_mount)
		return htlb_mount;

	fd = open("/proc/mounts", O_RDONLY);
	if (fd < 0) {
		fd = open("/etc/mtab", O_RDONLY);
		if (fd < 0) {
			ERROR("Couldn't open /proc/mounts or /etc/mtab (%s)\n",
				strerror(errno));
			return NULL;
		}
	}

	while ((bytes = read(fd, line, LINE_MAXLEN)) > 0) {
		line[LINE_MAXLEN] = '\0';
		eol = strchr(line, '\n');
		if (!eol) {
			ERROR("Line too long when parsing mounts\n");
			break;
		}

		/*
		 * Truncate the string to just one line and reset the file
		 * to begin reading at the start of the next line.
		 */
		*eol = '\0';
		offset = bytes - (eol + 1 - line);
		lseek(fd, -offset, SEEK_CUR);

		/*
		 * Match only hugetlbfs filesystems.
		 * Subtle: sscanf returns the number of input items matched
		 * and assigned.  To force sscanf to match the literal
		 * "hugetlbfs" string we include a 'dummy' input item
		 * following that string.
		 */
		err = sscanf(line, "%*s %" stringify(PATH_MAX) "s hugetlbfs "
			"%*s %d", htlb_mount, &dummy);
		if ((err == 2) && (hugetlbfs_test_path(htlb_mount) == 1)) {
			close(fd);
			return htlb_mount;
		}

		memset(htlb_mount, 0, sizeof(htlb_mount));
	}
	close(fd);

#if defined(OPEN64_MOD) && defined(M_PAGE)
        if (hugepage_m_stype != SIZE_1G)
#endif
	WARNING("Could not find hugetlbfs mount point in /proc/mounts. "
			"Is it mounted?\n");

	return NULL;
}

int hugetlbfs_unlinked_fd(void)
{
	const char *path;
	char name[PATH_MAX+1];
	int fd;

	path = hugetlbfs_find_path();
	if (!path)
		return -1;

	name[sizeof(name)-1] = '\0';

	strcpy(name, path);
	strncat(name, "/libhugetlbfs.tmp.XXXXXX", sizeof(name)-1);
	/* FIXME: deal with overflows */

	fd = mkstemp64(name);

	if (fd < 0) {
		ERROR("mkstemp() failed: %s\n", strerror(errno));
		return -1;
	}

	unlink(name);

	return fd;
}

/********************************************************************/
/* Library user visible DIAGNOSES/DEBUGGING ONLY functions          */
/********************************************************************/

long hugetlbfs_num_free_pages(void)
{
	return read_meminfo("HugePages_Free:");
}

long hugetlbfs_num_pages(void)
{
#ifdef OPEN64_MOD

#ifdef M_PAGE
    if (hugepage_m_stype == SIZE_1G) {
        /* TODO: make change when 1G dynamic is supported */
        return read_sysctrl("/sys/kernel/mm/hugepages/hugepages-1048576kB/nr_hugepages");
    }
#endif

    if (hugepage_m_stype == SIZE_2M) {

        /* Interface for the future release
         * TODO: verify this on a release kernel.
         */
        long o_val = read_sysctrl("/sys/kernel/mm/hugepages/hugepages-2048kB/nr_overcommit_hugepages");

        if (o_val > 0) {
            long t_val = read_sysctrl("/sys/kernel/mm/hugepages/hugepages-2048kB/nr_hugepages");
            long s_val = read_sysctrl("/sys/kernel/mm/hugepages/hugepages-2048kB/surplus_hugepages");
            
            /* dynamic pool exists */
            DEBUG("2M dynamic pool = %ld\n", o_val);
            return (o_val + t_val - s_val);
        }

        /* Interface for the current release 
         * TODO: verify this on a release kernel.
         */
        o_val = read_sysctrl("/proc/sys/vm/nr_overcommit_hugepages");
        
        if (o_val > 0) {
            long t_val = read_meminfo("HugePages_Total:");
            long s_val = read_meminfo("HugePages_Surp:");

            /* dynamic pool exists */
            DEBUG("2M dynamic pool = %ld\n", o_val);
            return (o_val + t_val - s_val);
        }
    }
#endif
    
    return (read_meminfo("HugePages_Total:"));
}

#define MAPS_BUF_SZ 4096
long dump_proc_pid_maps()
{
	FILE *f;
	char line[MAPS_BUF_SZ];
	size_t ret;

	f = fopen("/proc/self/maps", "r");
	if (!f) {
		ERROR("Failed to open /proc/self/maps\n");
		return -1;
	}

	while (1) {
		ret = fread(line, sizeof(char), MAPS_BUF_SZ, f);
		if (ret < 0) {
			ERROR("Failed to read /proc/self/maps\n");
			return -1;
		}
		if (ret == 0)
			break;
		ret = fwrite(line, sizeof(char), ret, stderr);
		if (ret < 0) {
			ERROR("Failed to write /proc/self/maps to stderr\n");
			return -1;
		}
	}

	fclose(f);
	return 0;
}
