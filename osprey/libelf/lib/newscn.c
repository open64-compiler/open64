/*
 * Copyright 2005 PathScale, Inc.  All Rights Reserved.
 */

/*
newscn.c - implementation of the elf_newscn(3) function.
Copyright (C) 1995 - 1998, 2003 Michael Riepe <michael@stud.uni-hannover.de>

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
static const char rcsid[] = "@(#) $Id: newscn.c,v 1.1.1.1 2005/10/21 19:00:00 marcel Exp $";
#endif /* lint */

int
_elf_update_shnum(Elf *elf, size_t shnum) {
    Elf_Scn *scn;

    if (elf->e_class == ELFCLASS32) {
	if (shnum >= SHN_LORESERVE) {
	    scn = elf->e_scn_1;
	    elf_assert(scn->s_index == 0);
	    scn->s_shdr_flags |= ELF_F_DIRTY;
	    scn->s_shdr32.sh_size = shnum;
	    shnum = 0;
	}
	elf->e_ehdr_flags |= ELF_F_DIRTY;
	((Elf32_Ehdr*)elf->e_ehdr)->e_shnum = shnum;
	return 0;
    }
#if __LIBELF64
    else if (elf->e_class == ELFCLASS64) {
	if (shnum >= SHN_LORESERVE) {
	    scn = elf->e_scn_1;
	    elf_assert(scn->s_index == 0);
	    scn->s_shdr_flags |= ELF_F_DIRTY;
	    scn->s_shdr64.sh_size = shnum;
	    shnum = 0;
	}
	elf->e_ehdr_flags |= ELF_F_DIRTY;
	((Elf64_Ehdr*)elf->e_ehdr)->e_shnum = shnum;
	return 0;
    }
#endif /* __LIBELF64 */
    else if (valid_class(elf->e_class)) {
	seterr(ERROR_UNIMPLEMENTED);
    }
    else {
	seterr(ERROR_UNKNOWN_CLASS);
    }
    return -1;
}

static Elf_Scn*
_buildscn(Elf *elf) {
    Elf_Scn *scn;

    elf_assert(elf);
    elf_assert(elf->e_ehdr);
    elf_assert(_elf_scn_init.s_magic == SCN_MAGIC);
    while ((scn = (Elf_Scn*)malloc(sizeof(*scn)))) {
	*scn = _elf_scn_init;
	scn->s_elf = elf;
	scn->s_scn_flags = ELF_F_DIRTY;
	scn->s_shdr_flags = ELF_F_DIRTY;
	scn->s_freeme = 1;
	if (elf->e_scn_n) {
	    scn->s_index = elf->e_scn_n->s_index + 1;
	    if (_elf_update_shnum(elf, scn->s_index + 1)) {
		free(scn);
		return NULL;
	    }
	    elf->e_scn_n->s_link = scn;
	    elf->e_scn_n = scn;
	    return scn;
	}
	scn->s_index = 0;
	if (_elf_update_shnum(elf, 1)) {
	    free(scn);
	    return NULL;
	}
	elf->e_scn_1 = elf->e_scn_n = scn;
    }
    seterr(ERROR_MEM_SCN);
    return NULL;
}

Elf_Scn*
elf_newscn(Elf *elf) {
    Elf_Scn *scn;

    if (!elf) {
	return NULL;
    }
    elf_assert(elf->e_magic == ELF_MAGIC);
    if (!elf->e_readable && !elf->e_ehdr) {
	seterr(ERROR_NOEHDR);
    }
    else if (elf->e_kind != ELF_K_ELF) {
	seterr(ERROR_NOTELF);
    }
    else if (!elf->e_ehdr && !_elf_cook(elf)) {
	return NULL;
    }
    else if ((scn = _buildscn(elf))) {
	return scn;
    }
    return NULL;
}
