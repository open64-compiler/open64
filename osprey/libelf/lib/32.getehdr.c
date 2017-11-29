/*
 * Copyright 2005 PathScale, Inc.  All Rights Reserved.
 */

/*
32.getehdr.c - implementation of the elf{32,64}_getehdr(3) functions.
Copyright (C) 1995 - 1998 Michael Riepe <michael@stud.uni-hannover.de>

This library is free software; you can redistribute it and/or
modify it under the terms of the GNU Library General Public
License as published by the Free Software Foundation; either
version 2 of the License, or (at your option) any later version.

This library is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
Library General Public License for more details.

You should have received a copy of the GNU Library General Public
License along with this library; if not, write to the Free Software
Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
*/

#include <private.h>

#ifndef lint
static const char rcsid[] = "@(#) $Id: 32.getehdr.c,v 1.1.1.1 2005/10/21 19:00:00 marcel Exp $";
#endif /* lint */

char*
_elf_getehdr(Elf *elf, unsigned cls) {
    if (!elf) {
	return NULL;
    }
    elf_assert(elf->e_magic == ELF_MAGIC);
    if (elf->e_kind != ELF_K_ELF) {
	seterr(ERROR_NOTELF);
    }
    else if (elf->e_class != cls) {
	seterr(ERROR_CLASSMISMATCH);
    }
    else if (elf->e_ehdr || _elf_cook(elf)) {
	return elf->e_ehdr;
    }
    return NULL;
}

Elf32_Ehdr*
elf32_getehdr(Elf *elf) {
    return (Elf32_Ehdr*)_elf_getehdr(elf, ELFCLASS32);
}

#if __LIBELF64

Elf64_Ehdr*
elf64_getehdr(Elf *elf) {
    return (Elf64_Ehdr*)_elf_getehdr(elf, ELFCLASS64);
}

#endif /* __LIBELF64 */
