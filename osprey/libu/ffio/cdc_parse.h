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


/* USMID @(#) libu/ffio/cdc_parse.h	92.0	10/08/98 14:57:41 */

/*  CDC LAYER ASSIGN PARSING DEFINITION */

#define NUM_CDC_NUMERICS 0
#define NUM_CDC_ALIAS    0

#include "cdcio.h"
struct LAYER_OPTS _cdc_opts[] = {
   CLASS_CDC, _STR1M,  _INFO1_STR1(TR_CDC_CZ),      0, 0, _STR1M, 0, 0, "cz" ,
   CLASS_CDC, _STR1M,  _INFO1_STR1(TR_CDC_IW),      0, 0, _STR1M, 0, 0, "iw" ,
   CLASS_CDC, _STR1M,  _INFO1_STR1(TR_CDC_CW),      0, 0, _STR1M, 0, 0, "cw" ,
   CLASS_CDC, _STR1M,  _INFO1_STR1(TR_CDC_CS),      0, 0, _STR1M, 0, 0, "cs" ,
   CLASS_CDC, _STR2M,  _INFO1_STR2(TR_CDC_BT_DISK), 0, 0, _STR2M, 0, 0, "disk" ,
   CLASS_CDC, _STR2M,  _INFO1_STR2(TR_CDC_BT_I),    0, 0, _STR2M, 0, 0, "i" ,
   CLASS_CDC, _STR2M,  _INFO1_STR2(TR_CDC_BT_SI),   0, 0, _STR2M, 0, 0, "si"
} ;

#define NUM_CDC_OPTS     (sizeof(_cdc_opts)/sizeof(struct LAYER_OPTS))

struct LAYER_DATA _cdc_data =
    {
         CLASS_CDC ,
         NO_TYPE,
         "cdc",
         ".disk" ,
         _STR1M,
         0,
         NUM_CDC_OPTS,
         1,
         NUM_CDC_NUMERICS,
         NUM_CDC_ALIAS,
         _cdc_opts,
         NULL,
         NULL,
         NULL
    } ;

