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


/* USMID @(#) libu/ffio/nosve_parse.h	92.0	10/08/98 14:57:41 */

/*  NOSVE LAYER ASSIGN PARSING DEFINITION */

#include "gxio.h"
#include "gfio.h"
#include "gvio.h"
#include "common_parse.h"
#include "spec_parse.h"		/* for declaration of _class_vf_check */

#define NUM_NOSVE_ALIAS    0

struct LAYER_OPTS _nosve_opts[] = {
 CLASS_X, _STR1M,  _INFO1_STR1(TR_NVE_V),    0, 0, _STR1M, 0, 0,  "v" ,
 CLASS_V, _STR1M,  _INFO1_STR1(TR_NVE_S),    0, 0, _STR1M, 0, 0,  "s" ,
 CLASS_V, _STR1M,  _INFO1_STR1(TR_NVE_D),    0, 0, _STR1M, 0, 0,  "d" ,
 CLASS_F, _STR1M,  _INFO1_STR1(TR_VMS_F_TP), 0, 0, _STR1M, 0, 0,  "f" ,
 CLASS_V, _STR1M,  _INFO1_STR1(TR_IBM_U),    0, 0, _STR1M, 0, 0,  "u" 
} ;

#define NUM_NOSVE_OPTS     (sizeof(_nosve_opts)/sizeof(struct LAYER_OPTS))


struct LAYER_DATA _nosve_data =
    {
         CLASS_X ,
         FLD_TYPE     ,
         "nosve" ,
         "" ,
         _STR1M,
         0,
         NUM_NOSVE_OPTS,
         1 ,
         NUM_REC_BUF_NUMERICS,
         NUM_NOSVE_ALIAS ,
         _nosve_opts ,
         _rec_buf_numerics ,
         NULL,
         _class_vf_check
    } ;

