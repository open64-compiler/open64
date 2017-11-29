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


#include <sys/types.h>
#include <sys/stat.h>
#ifndef __MINGW32__
#include <sys/mman.h>
#endif
#if defined(BUILD_OS_DARWIN)
#include <darwin_elf.h>
#else /* defined(BUILD_OS_DARWIN) */
#include <elf.h>
#endif /* defined(BUILD_OS_DARWIN) */
#include <fcntl.h>
#include <stdlib.h>
#include <unistd.h>
#include <ctype.h>
#include <ar.h>
#include <strings.h>
#include "defs.h"
#include "mempool.h" 

struct archive_hash {
    struct archive_hash *next;
    char* name;
    INT offset;
};

struct archive_hash_table {
    struct ar_hdr *str_table;
    struct archive_hash **hash_table;
    struct archive_hash *buckets;
    INT size;

    /* used only for abi searching algorithm */
    INT current_idx;		    /* index to the next symbol in symtab */
    INT last_idx;		    /* index to the last symbol used */
    INT n_entries;		    /* number of buckets */
};

#define ELF_AR_STRING_NAME      "//              "

static UINT32
elfhash(char *pname)
{
    register UINT32 h = 0;
    register char ch;

    while (ch = *pname++) {
        register UINT32 g;
        h = (h << 4) + ch;
        if (g = (h & 0xF0000000)) {
            h ^= g >> 24;
            h &= ~g;
        }
    }
    return h;
}

static UINT64
round (UINT64 addr, UINT64 align)
{
    if ((INT64) align-- <= 1)
        return addr;

    return ((addr + align) & ~align);
}


#ifndef __GNUC__
#pragma pack(1)
struct unalign_word {
    UINT32 w;
};
struct unalign_longlong {
    UINT64 x;
};
#pragma pack(0)
#else
struct unalign_word {
    UINT32 w;
} __attribute__ ((aligned (1), packed));
struct unalign_longlong {
    UINT64 x;
} __attribute__ ((aligned (1), packed));
#endif

#ifndef _LIGHTWEIGHT_INLINER
void *
Digest_Archive (void* handle, MEM_POOL* m, INT64 file_size)
{
    struct ar_hdr *symtab, *strtab;
    struct archive_hash_table *p;
    struct archive_hash *buckets;
    char *end_of_symtab, *end_of_strtab;
    BOOL ar64 = FALSE;

    symtab = (struct ar_hdr *) ((char *)handle + SARMAG);
    
    if (strncmp (symtab->ar_name, ELF_AR64_SYMTAB_NAME,
		 ELF_AR64_SYMTAB_NAME_LEN) == 0) {
	ar64 = TRUE;
    }
    else if (strncmp (symtab->ar_name, ELF_AR_SYMTAB_NAME,
		 ELF_AR_SYMTAB_NAME_LEN) ) {
	fprintf(stderr, "Expecting WHIRL archives...\n");
	return NULL;
    }

    end_of_symtab = (char *)handle + SARMAG + sizeof(struct ar_hdr) +
	atol (symtab->ar_size);

    p = (struct archive_hash_table *) MEM_POOL_Alloc (m, sizeof(struct archive_hash_table));

    // set up hash table 
    INT* file_offset;
    file_offset = (INT*)((char *)handle + SARMAG + sizeof(struct ar_hdr));

    /* 
     * address that holds the number of entries in the 64bit
     * archives is mis-aligned 
     */
    INT n_entries;
    char *name;
    if (ar64) {
        n_entries = (INT) ((struct unalign_longlong *) file_offset)->x;
        file_offset = (INT*)((char *)file_offset + sizeof(unalign_longlong));
        name = (char *) ((char*)file_offset + n_entries*sizeof(unalign_longlong));
    }
    else  {
        n_entries = *(INT*)file_offset;
        ++file_offset;
        name = (char *) (file_offset + n_entries);
    }


    // p->size must be powers of 2.  
    // See calculation of hash_value below.
    for (p->size = 1; p->size < n_entries; p->size *= 2);

    p->hash_table = (struct archive_hash **)
	MEM_POOL_Alloc (m, sizeof(struct archive_hash *) * p->size);

    BZERO ((char *)p->hash_table, sizeof(struct archive_hash *) * p->size);

    p->buckets = buckets = (struct archive_hash *)
	MEM_POOL_Alloc (m, sizeof(struct archive_hash) * n_entries);

    p->n_entries = n_entries;

    while (n_entries > 0) {
	UINT hash_value;
	
	if (ar64)
	    buckets->offset = ((struct unalign_longlong *) file_offset)->x;
	else
	    buckets->offset = *(INT *)file_offset;

	buckets->name = name;
	hash_value = elfhash (name) & (p->size - 1);

	buckets->next = p->hash_table[hash_value];
	p->hash_table[hash_value] = buckets;
	
	while (*name++ != 0);
	buckets++;
        if (ar64) {
            file_offset = (INT*)((char *)file_offset + sizeof(unalign_longlong));
        }
        else  {
            ++file_offset;
        }

	n_entries--;
    }
    
    p->current_idx = 0;
    p->last_idx = -1;

    /* check if string table exist */
    p->str_table = 0;
    if (end_of_symtab >= ((char *)handle + file_size))
	return p;
    
    strtab = (struct ar_hdr *) end_of_symtab;
    if (strncmp (strtab->ar_name, ELF_AR_STRING_NAME, sizeof(strtab->ar_name)) == 0) 
	p->str_table = strtab;

    return p;

} /* Digest_Archive */

void
Cleanup_Archive_Handle (void *handle, MEM_POOL* m)
{
    struct archive_hash_table *p;

    p = (struct archive_hash_table *) handle;

    MEM_POOL_FREE (m, p->hash_table);
    MEM_POOL_FREE (m, p->buckets);
    MEM_POOL_FREE (m, p);
    
} /* Cleanup_Archive_Handle */


	/*******************************************************
		Function: defined_by_archive

		

	 *******************************************************/
off_t
Defined_By_Archive (char *name, void* handle)
{
    struct archive_hash_table *p;
    struct archive_hash *bucket;

    p = (struct archive_hash_table *) handle;
    bucket = p->hash_table[elfhash (name) % p->size];

    while (bucket) {
	if (strcmp (bucket->name, name) == 0)
	    return bucket->offset;
	bucket = bucket->next;
    }

    return 0;
    
} /* Defined_By_Archive */
#endif
char *
Read_Member_Name (struct ar_hdr *header, void* handle, MEM_POOL* m)
{
    struct archive_hash_table *p;
    char *name;
    INT len;
    char *buf;

    p = (struct archive_hash_table *) handle;

    if (header->ar_name[0] == '/') {
	int str_offset = atoi (header->ar_name + 1);
	if (p->str_table == 0)
	    return const_cast<char *>("");

	name = (char *) p->str_table + sizeof(struct ar_hdr) + str_offset;
    } else {
	name = header->ar_name;
    }

    for (len = 1; name[len] != '/'; len++);

    buf = (char *) MEM_POOL_Alloc (m, len+1);  /* free by process_archive() */
    strncpy (buf, name, len);
    buf[len] = 0;
    return buf;
} /* Read_Member_Name */

off_t
Next_Archive_Member (char* base, off_t offset, INT64 size)
{
    struct ar_hdr *ar;
	
    if (offset == 0) {
	offset = SARMAG;
	ar = (struct ar_hdr *) (base + offset);
	while (ar->ar_name[0] == '/' && !isdigit(ar->ar_name[1])) {
	    offset += atol(ar->ar_size) + sizeof(struct ar_hdr);
	    offset = (off_t)round (offset, 2);
	    if (offset >= size)
		return 0;
	    ar = (struct ar_hdr *) (base + offset);
	}
    } else {
	ar = (struct ar_hdr *) (base + offset);
	offset += atol(ar->ar_size) + sizeof(struct ar_hdr);
	offset = (off_t)round (offset, 2);
	if (offset >= size)
	    return 0;
    }
    return offset;
} /* Next_Archive_Member */


