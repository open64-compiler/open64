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


/* $Header$ */

#ifndef __OBJ_TYPE_H__
#define __OBJ_TYPE_H__

#ifdef __cplusplus
extern "C" {
#endif

#include "obj.h"

typedef struct obj_type {
	struct obj	*obj;
	unsigned long	init;		/* whether to init with this type */
	long		type;		/* index to aux we are processing */

	/* the rest are fields we fill in */
	TIR		ti;
	unsigned long	skip;		/* how many tq's & bt to ignore */
	long		bt_symbol;
	long		bt_type;
	unsigned long	bt_low;		/* for range */
	unsigned long	bt_high;	/* for range */
	unsigned long	tq;
	unsigned long	tq_index;	/* ongoing index to next aux */
	unsigned long   tq_save;
	long		symbol;
	unsigned long	low;
	unsigned long	high;
	unsigned long	stride;
	unsigned long	width;
	unsigned long	dimension;
	unsigned long	dimcount;
	unsigned long	offset;
	unsigned long	precision;
	unsigned long	isproc;
	long 		nodesym;
	unsigned int	language;
} OBJ_TYPE, *pOBJ_TYPE;
#define cbOBJ_TYPE (sizeof(OBJ_TYPE))

extern AUXU usertype[];
extern int tinx;

/*

basic type represented in a size other than the def size:
	tq		tqNil
	bt		bt whatever
	width		number of bits to represent the type

basic type:
	tq		tqNil
	bt		bt whatever
	width		0

Pointer:
	tq		tqPtr

Function returning:
	tq		tqFunc

Array:
	tq		tqArray
	type		rindex to subscript's type TIR
	low		low index
	high		high index
	stride		stride in bits

Set:
Indirect
	bt		btSet or btIndirect
	type		type index to tir of set or indirect type.

Enumerated type:
	bt		btEnum
	symbol		symbol index to enum block

Range:
	bt		btRange
	type		type index to TIR to type we are subranging
	low		low range value
	high		high range value

C union:
Structures:
	bt		btUnion or btStruct
	symbol		symbol index to block defining union or struct

basic type is a Typedef:
	bt		btTypedef
	symbol		symbol index to typedef symbol

FORTRAN_common:
FORTRAN_equivalence:
	bt		btStruct
	symbol		symbol index to block defining struct

*/

#ifdef __cplusplus
}
#endif

#endif /* __OBJ_TYPE_H__ */
