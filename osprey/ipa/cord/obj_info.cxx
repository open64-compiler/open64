/*
 * Copyright 2003, 2004, 2005, 2006 PathScale, Inc.  All Rights Reserved.
 */

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


#include <stdio.h>
#if defined(BUILD_OS_DARWIN)
#include <darwin_elf.h>
#else /* defined(BUILD_OS_DARWIN) */
#include <elf.h>
#endif /* defined(BUILD_OS_DARWIN) */
#include <sys/types.h>
#include <sys/stat.h>
#include <sys/mman.h>
#include <fcntl.h>

#include "obj_info.h"

#include <stdlib.h>
void
error () {
    perror ("Error");
    exit (1);
}

File_Info::File_Info (char *name)
{
    int fd = open (name, O_RDONLY);
    struct stat stat_buf;

    if (fstat (fd, &stat_buf) != 0)
	error ();

    if (stat_buf.st_size == 0)
	error ();

    file_size = stat_buf.st_size;

    mmap_handle = mmap (0, file_size, PROT_READ, MAP_SHARED, fd, 0);

    if (mmap_handle == MAP_FAILED)
	error ();

    process_file ();
} // File_Info::File_Info


template <class HEADER, class SHDR>
void
File_Info::fill_file_info (HEADER *ehdr, SHDR *shdr)
{
    shdr = (SHDR *) ((char *)mmap_handle + ehdr->e_shoff);

    strtab = (char *) mmap_handle + shdr[ehdr->e_shstrndx].sh_offset;
    
    shdr_end = shdr + ehdr->e_shnum;

    while (shdr < shdr_end && (shdr->sh_type != SHT_PROGBITS ||
			       (shdr->sh_flags & SHF_EXECINSTR) == 0))
	++shdr;

    shdr_begin = shdr;

} // File_Info::fill_file_info


inline void
File_Info::process_file ()
{
    unsigned char *ident = (unsigned char *) mmap_handle;
    obj_class = ident[EI_CLASS];
    switch (obj_class) {
    case ELFCLASS32:
	fill_file_info ((Elf32_Ehdr *) mmap_handle, Shdr32_tag ());
	return;
    case ELFCLASS64:
	fill_file_info ((Elf64_Ehdr *) mmap_handle, Shdr64_tag ());
	return;
    default:
	error ();
    }
} // File_Info::process_file

