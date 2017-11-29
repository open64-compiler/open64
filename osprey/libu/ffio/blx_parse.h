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


/* USMID @(#) libu/ffio/blx_parse.h	92.0	10/08/98 14:57:41 */

/*  BLX LAYER ASSIGN PARSING DEFINITION */

#include "blxio.h"
#define NUM_BLX_ALIAS    0

struct LAYER_OPTS _blx_opts[] = {
	CLASS_BLX, _STR1M, _INFO1_STR1(BLX_COS) , 0, 0, _STR1M, 0, 0, "cos" ,
	CLASS_BLX, _STR1M, _INFO1_STR1(BLX_CTSS), 0, 0, _STR1M, 0, 0, "ctss" ,
	CLASS_BLX, _STR1M, _INFO1_STR1(BLX_CTSS), 0, 0, _STR1M, 0, 0, "c205" ,
} ;

struct LAYER_NUMERICS _blx_numerics[]  = {
   1, 1, 'n' , 0 , 0xff  , 0 ,  "blxchr" ,
   1, 1, 'n' , 0 , 0xff  , 0 ,  "blnk"
                     } ;
#define NUM_BLX_OPTS     sizeof(_blx_opts)/sizeof(struct LAYER_OPTS)
#define NUM_BLX_NUMERICS sizeof(_blx_numerics)/sizeof(struct LAYER_NUMERICS)

struct LAYER_DATA _blx_data =
    {
	CLASS_BLX	,
         FLD_EXT_TYPE     ,
         "blx",
         ".cos" ,
         0,
         0,
         NUM_BLX_OPTS,
         1 ,
         NUM_BLX_NUMERICS,
         NUM_BLX_ALIAS ,
         _blx_opts ,
         _blx_numerics ,
         NULL,
         NULL
    } ;
struct LAYER_DATA _blankx_data =
    {
	CLASS_BLX	,
         FLD_EXT_TYPE     ,
         "blankx",
         ".cos" ,
         0,
         0,
         NUM_BLX_OPTS,
         1 ,
         NUM_BLX_NUMERICS,
         NUM_BLX_ALIAS ,
         _blx_opts ,
         _blx_numerics ,
         NULL,
         NULL
	};

