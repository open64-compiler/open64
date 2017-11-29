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


/* USMID @(#) libu/ffio/cmp_parse.h	92.1	06/10/99 15:02:15 */

/*  CMP LAYER ASSIGN PARSING DEFINITION */

#define NUM_CMP_OPTS     0
#define NUM_CMP_ALIAS    0

struct LAYER_NUMERICS _cmp_numerics[]  = {
   0, 0, 'n' , 0x200  , 0x800000 , 0x2000 ,  "segSz" ,
   0, 0, 'n' , 1 , 50 , 10 ,  "cacheSz",
   0, 0, 'n' , 0 , 4  , 0 ,  "debugLvl"
} ;

#define NUM_CMP_NUMERICS sizeof(_cmp_numerics)/sizeof(struct LAYER_NUMERICS)

struct LAYER_DATA _cmp_data =
    {
	CLASS_CMP,
	FLD_EXT_TYPE,
	"cmp",
	"" ,
	0,
	0,
	NUM_CMP_OPTS,
	1,
	NUM_CMP_NUMERICS,
	NUM_CMP_ALIAS,
	NULL,
	_cmp_numerics,
	NULL,
	NULL
    };
