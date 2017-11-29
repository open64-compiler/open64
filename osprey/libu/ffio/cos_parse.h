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


/* USMID @(#) libu/ffio/cos_parse.h	92.0	10/08/98 14:57:41 */

/*  COS LAYER ASSIGN PARSING DEFINITION */

#include "cosio.h"
#define NUM_COS_ALIAS    0

struct LAYER_OPTS _cos_opts[] = {
 CLASS_COS, _STR1M, _INFO1_STR1(TR_AUTO),  0, 0, _STR1M, 0, 0, "auto" ,
 CLASS_COS, _STR1M, _INFO1_STR1(TR_SYNC),  0, 0, _STR1M, 0, 0, "sync" ,
 CLASS_COS, _STR1M, _INFO1_STR1(TR_ASYNC), 0, 0, _STR1M, 0, 0, "async"
} ;

struct LAYER_NUMERICS _cos_numerics[]  = {
   1, 0, 'b' , 1 , 0xfffff  , 0 ,  "bufsize" /*  number of blocks for buffer */
} ;

#define NUM_COS_NUMERICS (sizeof(_cos_numerics)/sizeof(struct LAYER_NUMERICS))
#define NUM_COS_OPTS     (sizeof(_cos_opts)/sizeof(struct LAYER_OPTS))

struct LAYER_DATA _cos_data =
    {
         CLASS_COS ,
         FLD_TYPE,
         "cos",
         "" ,   /* ".auto" */
         0,
         0,
         NUM_COS_OPTS,
         1 ,
         NUM_COS_NUMERICS,
         NUM_COS_ALIAS ,
         _cos_opts ,
         _cos_numerics ,
         NULL,
         NULL
    } ;

struct LAYER_DATA _blocked_data =
    {
         CLASS_COS ,
         FLD_TYPE,
         "blocked",
         "" ,   /* ".auto" */
         0,
         0,
         NUM_COS_OPTS,
         1 ,
         NUM_COS_NUMERICS,
         NUM_COS_ALIAS ,
         _cos_opts ,
         _cos_numerics ,
         NULL,
         NULL
    } ;
