/*
 * Copyright 2004 PathScale, Inc.  All Rights Reserved.
 */

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


#pragma ident "@(#) libu/ffio/txtopen.c	92.1	06/29/99 13:16:47"

#include <stdio.h>
#include <malloc.h>
#include <ffio.h>
#include "txtio.h"
#include "fxlist.h"

DECLARE(TXT_XLIST);
struct xtr_s TXT_XLIST_P = { TXT_XLIST };

/*
 * Initialize the state of a foreign dataset.  Allocate buffers
 * and initialize pointers.
 */

_ffopen_t
_txt_open(
const char            *name,
int             flags,
mode_t             mode,
struct fdinfo   *fio,
union spec_u    *spec,
struct ffsw     *stat,
long            cbits,
int             cblks,
struct gl_o_inf *oinf)
	{
	char *ptr;
	union spec_u *nspec;
	int blksize;
	_ffopen_t nextfio;
	struct text_f *text_info;
/*
 *	Internally, blksize is in bits! So convert 8-bit bytes to bits.
 */
	blksize = spec->fld.mbs << 3;
/*
 *	If blksize is 0, set it to 4096 bytes so maxblksize and _ffbufsiz are 
 *      set correctly.
 */
	if (blksize == 0) blksize =  8 * 4096 * 8;	/* 8 sectors of bits */
	if (blksize < (16*8)) blksize = 16 * 8;		/* min of 16 bytes */
  	fio->maxblksize = blksize;
	fio->_ffbufsiz = blksize + 64; /* bit size of buffer */
	ptr = malloc((blksize >> 3) + 16 + 8); /* + 1 word for EOF */
	if (ptr == NULL) goto nomem;
	SET_BPTR(fio->_base, CPTR2BP(ptr));
/*
 *	Allocate private storage for text class handling.
 */
	text_info = (struct text_f *)calloc(sizeof(struct text_f), 1);
	if (text_info == NULL) goto nomem;
	fio->lyr_info = (char *)text_info;
/*
 *	Set up the EOR and EOF marks, as appropriate
 */
	text_info->eof_mark = 0;
	text_info->eof_len = 0;
	text_info->eor_len = 8;
	if (fio->rtype == 0) fio->rtype = TEXT_NL;
	switch(fio->rtype)
		{
		case TEXT_NL_WEOF:
			text_info->eof_mark = TEXT_MAGIC_MARKER << TEXTNL_JUSTIFY_SHIFT;
			text_info->eof_len = 24;
		case TEXT_NL:
			text_info->eor_char = (unsigned long)'\n' << CHAR_JUSTIFY;
			break;
		case TEXT_205:
		case TEXT_CTSS:
			text_info->eor_char = (long)0x1f << CHAR_JUSTIFY; /* ASCII US */
			text_info->eof_mark = (long)0x1c << CHAR_JUSTIFY; /* ASCII FS */
			text_info->eof_len = 8;
			break;
		}
/*
 *	If recsize != 0, 
 *	Load recsize as the requested EOR characater.
 */
	if (spec->fld.recsize != 0)
		text_info->eor_char = ((long)spec->fld.recsize) << CHAR_JUSTIFY;

	fio->rwflag = POSITIN;
	fio->segbits = 0;
	fio->_cnt = 0;
	fio->_ptr = fio->_base;
	fio->scc = SCCFULL;
	fio->lastscc = SCCFULL;
/*
 *	Now, open the lower layers
 */
	nspec = spec;
	NEXT_SPEC(nspec);
	nextfio = _ffopen(name, flags, mode, nspec, stat, cbits, cblks, NULL,
			oinf);
	if (nextfio == _FFOPEN_ERR) goto badret;

	DUMP_IOB(fio); /* debugging only */
	return(nextfio);

nomem:
	_SETERROR(stat, FDC_ERR_NOMEM, 0);
badret:
	if (BPTR2CP(fio->_base) != NULL) free(BPTR2CP(fio->_base));
	if (fio->lyr_info != NULL) free(fio->lyr_info);
	return(_FFOPEN_ERR);
	}
