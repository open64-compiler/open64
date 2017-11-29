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


/* USMID @(#) libu/ffio/global_parse.h	92.0	10/08/98 14:57:41 */

/*  GLOBAL LAYER ASSIGN PARSING DEFINITION */

#include "globio.h"
#define NUM_GLOBAL_ALIAS    0

struct LAYER_OPTS _global_opts[] = {
 CLASS_GLOBAL, _STR1M, _INFO1_STR1(TR_GLOB_PRIVPOS),   0, 0, _STR1M, 0, 0, "privpos" ,
 CLASS_GLOBAL, _STR1M, _INFO1_STR1(TR_GLOB_GLOBPOS),   0, 0, _STR1M, 0, 0, "globpos" ,
} ;

struct LAYER_NUMERICS _global_numerics[]  = {

   /*  number of blocks for cache page */
   { 1, 0, 'b' , 1 , 0xffffffff, 0 ,  "page_size" }, 

   /*  number of pages per PE */
   { 1, 0, 'n' , 1 , 0xffffffff, 0 ,  "num_pages" },
} ;

#define NUM_GLOBAL_NUMERICS (sizeof(_global_numerics)/sizeof(struct LAYER_NUMERICS))

#define NUM_GLOBAL_OPTS     (sizeof(_global_opts)/sizeof(struct LAYER_OPTS))

struct LAYER_DATA _global_data =
    {
         CLASS_GLOBAL ,
         FLD_TYPE,
         "global",
         "" ,
         0,
         0,
         NUM_GLOBAL_OPTS,
         1 ,
         NUM_GLOBAL_NUMERICS,
         NUM_GLOBAL_ALIAS ,
         _global_opts ,
	 _global_numerics,
         NULL,
         NULL
    } ;

