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


#ifndef __OBJLIST_H
#define __OBJLIST_H
/* $Header$ */

typedef struct {
        Elf32_Word      oi_magic; /* to distinguish from old obj struct */
	Elf32_Word	oi_size; /* size in bytes of this struct */
	Elf32_Addr	oi_next; /* next record in doubly-linked list */
	Elf32_Addr	oi_prev; /* previous record */
	Elf32_Addr	oi_ehdr; /* address of ELF header in memory */
	Elf32_Addr	oi_orig_ehdr;/* QS address of ELF header */
	Elf32_Addr	oi_pathname; /* pathname as mapped by rld */
	Elf32_Word	oi_pathname_len; /* strlen(pathname) */
} Elf32_Obj_Info;

typedef struct {
        Elf64_Word      oi_magic; /* to distinguish from old obj struct */
	Elf64_Word	oi_size; /* size in bytes of this struct */
	Elf64_Addr	oi_next; /* next record in doubly-linked list */
	Elf64_Addr	oi_prev; /* previous record */
	Elf64_Addr	oi_ehdr; /* address of ELF header in memory */
	Elf64_Addr	oi_orig_ehdr;/* QS address of ELF header */
	Elf64_Addr	oi_pathname; /* pathname as mapped by rld */
	Elf64_Word	oi_pathname_len; /* strlen(pathname) */
} Elf64_Obj_Info;

/* We want dbx, pcs and libexc to be able to tell the new
 * obj struct from the old.  Therefore, since the old one
 * has a pointer in the first 4 bytes, we ensure that the
 * first 4 bytes of the new one cannot be an address.
 * First and foremost, this is accomplished by setting
 * the sign bit, since that indicates kernel space.  I believe
 * that any application will have to know that it is in
 * 64 bit mode before it gets this far.
 */
#define NEW_OBJ_INFO_MAGIC 0xffffffff
#endif

