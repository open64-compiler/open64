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


/* USMID @(#) libu/ffio/cch_parse.h	92.1	10/07/99 15:40:23 */

/*  CACHE LAYER ASSIGN PARSING DEFINITION */

#include "cchio.h"
#define NUM_CACHE_ALIAS    0

struct LAYER_OPTS _cch_opts[] = {
   CLASS_CACHE, _STR1M, _INFO1_STR1(TR_CCH_MEM), 0, 0, _STR1M, 0, 0, "mem" ,
#if	!defined(__mips) && !defined(_LITTLE_ENDIAN)
   CLASS_CACHE, _STR1M, _INFO1_STR1(TR_CCH_SDS), 0, 0, _STR1M, 0, 0, "sds",
   CLASS_CACHE, _STR1M, _INFO1_STR1(TR_CCH_SDS), 0, 0, _STR1M, 0, 0, "ssd"
#endif
} ;

struct LAYER_NUMERICS _cch_numerics[]  = {
   1, 0, 'b', 1 , 0xfffff,  0 ,  "page_size", /*  number of blocks for buffer */   
   1, 0, 'n', 1 , 0xfffff,  0 ,  "num_pages", /*  number of blocks for buffer */
   /* number of blocks at which we bypass buffering */
   1, 1, 'b', 1 , 0xffffffffffffLL,  0 ,  "bypass_size"
} ;

#define NUM_CACHE_OPTS     (sizeof(_cch_opts)/sizeof(struct LAYER_OPTS))
#define NUM_CACHE_NUMERICS (sizeof(_cch_numerics)/sizeof(struct LAYER_NUMERICS))

struct LAYER_DATA _cch_data =
    {
         CLASS_CACHE ,
         FLD_EXT_TYPE,
         "cache",
         "" ,
         0,
         0,
         NUM_CACHE_OPTS,
         1 ,
         NUM_CACHE_NUMERICS,
         NUM_CACHE_ALIAS ,
         _cch_opts ,
         _cch_numerics ,
         NULL,
         NULL
    } ;

