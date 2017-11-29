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


static char USMID[] = "@(#) libcif/cifspace.c	30.2	07/26/96 07:19:13";


/* --------------------------------------------------------------------------
 * CIF memory management routines
 * Tabs are set up to be read with tab spacing = 3
 * --------------------------------------------------------------------------
 */

#define CIF_VERSION 3

#ifdef _ABSOFT
#include "cif.h"
#else
#include <cif.h>
#endif

#if defined(BUILD_OS_DARWIN)
#include <stdlib.h>
#else /* defined(BUILD_OS_DARWIN) */
#include <malloc.h>
#endif /* defined(BUILD_OS_DARWIN) */
#include <memory.h>
#include <stdio.h>
#include <stdlib.h>

#include "cif_int.h"

#define MEMSIZE(X)	((X<CIF_BUFSIZE?CIF_BUFSIZE:X))
#define ROUND(X)		(((X+sizeof(long)-1)/sizeof(long)*sizeof(long)))

/* -------------------------------------------------------------------------
 * _Cif_memtbl constructs the _Cif_memarea table.
 * -------------------------------------------------------------------------
 */
int _Cif_memtbl ()
{
	int me, size;

	size = CIF_MEM_BUMP * sizeof(struct _Cif_Mem_Area);
	if ((_Cif_memarea = (struct _Cif_Mem_Area *) malloc (size)) == NULL)
		return (CIF_NOMEM);
	_Cif_memasize = CIF_MEM_BUMP;
	for (me = 1; me < CIF_MEM_BUMP; me++) {
		_Cif_memarea[me].used = NO;
		_Cif_memarea[me].mbp = NULL;
	}
	_Cif_memarea[0].used = YES;
	return (0);

}

/* -------------------------------------------------------------------------
 * "_Cif_mementry" retrieves an open entry in _Cif_Memarea.  It returns either
 * a negative status or a positive index into the table.
 * -------------------------------------------------------------------------
 */

int _Cif_mementry(bsize)
unsigned bsize;				/* size of buffer required */
{

	int me, oldsize;

	/* Look for unused entry with buffer attached.  If none available, look
	 * for unused entry without a buffer and get one.  Otherwise expand the
	 * table, get an entry and a buffer.
	 */

	for (me = 1; me < _Cif_memasize; me++) {
		if (_Cif_memarea[me].used == NO && _Cif_memarea[me].mbp != NULL &&
			_Cif_memarea[me].msize >= bsize) break;
	}
	if (me >= _Cif_memasize) {
		for (me = 1; me < _Cif_memasize; me++) {
			if (_Cif_memarea[me].used == NO && _Cif_memarea[me].mbp == NULL)
				break;
		}
	}
	if (me >= _Cif_memasize) {
		oldsize = _Cif_memasize;
		_Cif_memasize += CIF_MEM_BUMP;
		if ((_Cif_memarea = (struct _Cif_Mem_Area *)realloc(_Cif_memarea,
				_Cif_memasize * sizeof(struct _Cif_Mem_Area))) == NULL)
			return (CIF_NOMEM);
		for (me = oldsize; me < _Cif_memasize; me++) {
			_Cif_memarea[me].used = NO;
			_Cif_memarea[me].mbp = NULL;
		}
		me = oldsize;
	}
	if (_Cif_memarea[me].mbp == NULL) {
		if ((_Cif_memarea[me].mbp = malloc (bsize)) == NULL)
			return (CIF_NOMEM);
		_Cif_memarea[me].msize = bsize;
	}
	_Cif_memarea[me].used = YES;
	_Cif_memarea[me].nme = 0;
	_Cif_memarea[me].mused = 0;
	return (me);

}

/* --------------------------------------------------------------------------
 * "_Cif_managed_space" acquires space to contain record structures when the
 * file memory management mode is CIF_MEM_MANAGED.
 * --------------------------------------------------------------------------
 */

char *_Cif_managed_space
#ifdef __STDC__
(unsigned int size, int cifd)
#else
(size, cifd)
unsigned int size;
int cifd;
#endif
{

	int me, newme;	/* _Cif_memarea index */
	char *bp;		/* pointer to memory area returned */

	/*
	 * Round size to a "long" boundary.  If enough space in current buffer,
	 * use it, otherwise get a new buffer.
	 */

	size = ROUND(size);
	me = _Cif_filetbl[cifd].lme;
	if (_Cif_memarea[me].mused + size > _Cif_memarea[me].msize) {
		newme = _Cif_mementry(MEMSIZE(size));
		if (newme < 0)
			return ((char *)NULL);
		_Cif_memarea[me].nme = _Cif_filetbl[cifd].lme = newme;
		me = newme;
	}
	bp = _Cif_memarea[me].mbp + _Cif_memarea[me].mused;
	_Cif_memarea[me].mused += size;
	return (bp);
}

/* --------------------------------------------------------------------------
 * "_Cif_fixed_space" acquires space to contain record structures when the
 * file memory management mode is CIF_MEM_FIXED.
 * --------------------------------------------------------------------------
 */

char *_Cif_fixed_space
#ifdef __STDC__
(unsigned int size, int cifd)
#else
(size, cifd)
unsigned int size;
int cifd;
#endif
{

	int me;
	char *bp;

	/*
	 * Round size to a "long" boundary.  If enough space in current buffer,
	 * use it, otherwise make the buffer bigger.
	 */

	size = ROUND(size);
	me = _Cif_filetbl[cifd].fme;
	if (_Cif_memarea[me].mused + size > _Cif_memarea[me].msize) {
		_Cif_memarea[me].msize += size;
		_Cif_memarea[me].mbp = realloc (_Cif_memarea[me].mbp,
		                                _Cif_memarea[me].msize);
	}
	bp = _Cif_memarea[me].mbp + _Cif_memarea[me].mused;
	_Cif_memarea[me].mused += size;
	return (bp);
}
