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


/* USMID @(#) libu/ffio/vms_parse.h	92.0	10/08/98 14:57:41 */
#ifndef _FF_VMS_PARSE
#define _FF_VMS_PARSE
/*  VMS LAYER ASSIGN PARSING DEFINITION */

#include "spec_parse.h"		/* for declaration of _class_vms_check */
#include "common_parse.h"
#include "gfio.h"
#include "gvio.h"
#include "gxio.h"
#include "vms_def.h"
#define NUM_VMS_ALIAS    0

/* VMS is different than other layers. The value that goes in the info */
/* and class fields are determined in _class_vms_check, and depend on both */
/* the type and subtype. So, temporarily we store type info in info1, and */
/* subtype info in info2 */
 
struct LAYER_OPTS _vms_opts[] = {
 CLASS_F, _STR1M, VMS_F,      0, 0,       _STR1M,      0, 0,  "f" ,
 CLASS_V, _STR1M, VMS_V,      0, 0,       _STR1M,      0, 0,  "v" ,
 CLASS_V, _STR1M, VMS_S,      0, 0,       _STR1M,      0, 0,  "s" ,
 CLASS_V,      0,     0, _STR1M, VMS_BB,       0, _STR1M, 0,  "bb" ,
 CLASS_V,      0,     0, _STR1M, VMS_TAPE,     0, _STR1M, 0,  "tape" ,
 CLASS_V,      0,     0, _STR1M, VMS_TR,       0, _STR1M, 0,  "tr" ,
 CLASS_V,      0,     0, _STR1M, VMS_DSK,      0, _STR1M, 0,  "disk" ,
 CLASS_V,      0,     0, _STR1M, VMS_NONL,     0, _STR1M, 0,  "nonl" 
} ;

#define NUM_VMS_OPTS     (sizeof(_vms_opts)/sizeof(struct LAYER_OPTS))

struct LAYER_DATA _vms_data =
    {
         CLASS_V ,
         FLD_TYPE,
         "vms",
         "" ,
         0x0007,
	 0,
         NUM_VMS_OPTS,
         1 ,
         NUM_REC_BUF_NUMERICS,
         NUM_VMS_ALIAS ,
         _vms_opts ,
         _rec_buf_numerics ,
         NULL,
         _class_vms_check,
    } ;
#endif
