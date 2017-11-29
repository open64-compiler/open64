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


#ifndef srcpos_INCLUDED
#define srcpos_INCLUDED

#include "config_host.h"	/* for HOST_IS_LITTLE_ENDIAN */

#ifdef __cplusplus
extern "C" {
#endif

/* ====================================================================
 * ====================================================================
 *
 * Module: srcpos.h
 * $Revision: 1.1.1.1 $
 * $Date: 2005/10/21 19:00:00 $
 * $Author: marcel $
 * $Source: /proj/osprey/CVS/open64/osprey1.0/common/com/srcpos.h,v $
 *
 * Description:
 *
 *   Description of the various fields in the source position structure.
 *
 * ====================================================================
 */


/* SRCPOS is defined as a 64bit unsigned integer. This is the declaration 
 * visible to most files. Defining it this way allows existing calls that 
 * pass a INT32 line number to still compile fine. The prototype will 
 * automatically do the conversion from INT32 to the SRCPOS type. The 
 * value of the linenum field is still valid, since it is the lower 
 * 32bits of the source_position structure.
 */

 typedef mUINT64 SRCPOS;


/* The following struct, the union and with SRCPOS and the macros are 
 * for use by routines that need to setup or access the individual 
 * fields of the source position descriptor.
 */
struct srcpos_struct {
  mUINT16	filenum;
  mUINT16	column : 12;
  mUINT16	stmt_begin : 1;
  mUINT16	bb_begin : 1;
  mUINT16	unused : 2;
  mINT32	linenum;
};
#define SRC_POS_SIZE  2       /* 2 * sizeof(mINT32) */

typedef union source_position {
  SRCPOS srcpos;
  struct srcpos_struct t;
  mINT32 fillers[SRC_POS_SIZE];
} USRCPOS;

#define CHECK_SIZE_CONSISTENCY(s) check_assertion(sizeof(s) == (SRC_POS_SIZE*sizeof(INT32)))
#define USRCPOS_clear(s)          ((s).fillers[0] = 0,(s).fillers[1] = 0)

#define USRCPOS_srcpos(s)	((s).srcpos)
#define USRCPOS_filenum(s) 	((s).t.filenum)
#define USRCPOS_column(s) 	((s).t.column)
#define USRCPOS_stmt_begin(s) 	((s).t.stmt_begin)
#define USRCPOS_bb_begin(s) 	((s).t.bb_begin)
#define USRCPOS_linenum(s) 	((s).t.linenum)

#define SRCPOS_clear(s)		((s) = 0)
#define SRCPOS_filenum(s) 	(((USRCPOS *)&(s))->t.filenum)
#define SRCPOS_column(s) 	(((USRCPOS *)&(s))->t.column)
#define SRCPOS_stmt_begin(s) 	(((USRCPOS *)&(s))->t.stmt_begin)
#define SRCPOS_bb_begin(s) 	(((USRCPOS *)&(s))->t.bb_begin)
#define SRCPOS_linenum(s) 	(((USRCPOS *)&(s))->t.linenum)

#if HOST_IS_LITTLE_ENDIAN
#define Srcpos_To_Line(s)	((mINT32)((s)>>32))
#else
#define Srcpos_To_Line(s)	((mINT32)(s))
#endif /* HOST_IS_LITTLE_ENDIAN */

#ifdef __cplusplus
}
#endif
#endif /* srcpos_INCLUDED */
