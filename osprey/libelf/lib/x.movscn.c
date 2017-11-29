/*
 * Copyright 2005 PathScale, Inc.  All Rights Reserved.
 */

/*
x.movscn.c - implementation of the elfx_movscn(3) function.
Copyright (C) 1995 - 2001, 2003 Michael Riepe <michael@stud.uni-hannover.de>

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
static const char rcsid[] = "@(#) $Id: x.movscn.c,v 1.1.1.1 2005/10/21 19:00:00 marcel Exp $";
#endif /* lint */

size_t
elfx_movscn(Elf *elf, Elf_Scn *scn, Elf_Scn *after) {
    Elf_Scn *prev;
    Elf_Scn *tmp;
    int off;

    if (!elf || !scn || !after) {
	return SHN_UNDEF;
    }
    elf_assert(elf->e_magic == ELF_MAGIC);
    if (elf->e_kind != ELF_K_ELF) {
	seterr(ERROR_NOTELF);
	return SHN_UNDEF;
    }
    elf_assert(scn->s_magic == SCN_MAGIC);
    elf_assert(after->s_magic == SCN_MAGIC);
    if (scn->s_elf != elf || after->s_elf != elf) {
	seterr(ERROR_ELFSCNMISMATCH);
	return SHN_UNDEF;
    }
    elf_assert(elf->e_scn_1);
    if (scn == elf->e_scn_1) {
	seterr(ERROR_NULLSCN);
	return SHN_UNDEF;
    }
    if (scn == after || scn == after->s_link) {
	/* nothing to do */
	return scn->s_index;
    }

    /*
     * Find previous section.
     */
    prev = NULL;
    for (tmp = elf->e_scn_1; tmp->s_link; tmp = tmp->s_link) {
	if (tmp->s_link == scn) {
	    prev = tmp;
	    break;
	}
    }
    elf_assert(prev != NULL);

    /*
     * Update section indices
     */
    off = 0;
    for (tmp = elf->e_scn_1; tmp; tmp = tmp->s_link) {
	if (off) {
	    tmp->s_index += off;
	}
	if (tmp == after) {
	    off++;
	}
	else if (tmp == scn) {
	    off--;
	}
    }
    elf_assert(off == 0);

    /*
     * Move section.
     */
    prev->s_link = scn->s_link;
    scn->s_link = after->s_link;
    after->s_link = scn;
    scn->s_index = after->s_index + 1;
    if (elf->e_scn_n == scn) {
	elf->e_scn_n = prev;
    }
    else if (elf->e_scn_n == after) {
	elf->e_scn_n = scn;
    }

#if ENABLE_DEBUG
    /*
     * Check section indices
     */
    tmp = elf->e_scn_1;
    elf_assert(tmp->s_index == 0);
    while (tmp->s_link) {
	elf_assert(tmp->s_link->s_index == tmp->s_index + 1);
	tmp = tmp->s_link;
    }
#endif /* ENABLE_DEBUG */

    return scn->s_index;
}
