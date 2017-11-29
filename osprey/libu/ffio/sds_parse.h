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


/* USMID @(#) libu/ffio/sds_parse.h	92.0	10/08/98 14:57:41 */
/*  SDS LAYER ASSIGN PARSING DEFINITION */
#include "common_parse.h"
#include "fssio.h"
#define NUM_SDS_ALIAS    0 

struct LAYER_OPTS _sds_opts[] = {
 CLASS_SDS, _STR1M, _INFO1_STR1(TR_FSS_SCR),    0, 0, _STR1M, 0, 0, "scr"   ,
 CLASS_SDS, _STR1M, _INFO1_STR1(TR_FSS_SAVE),   0, 0, _STR1M, 0, 0, "save"   ,
 CLASS_SDS, _STR2M, _INFO1_STR2(FSS_OPT_OVFL) , 0, 0, _STR2M, 0, 0, "ovfl"   ,
 CLASS_SDS, _STR2M, _INFO1_STR2(FSS_OPT_NOVFL), 0, 0, _STR2M, 0, 0, "novfl"
} ;
#define NUM_SDS_OPTS (sizeof(_sds_opts)/sizeof(struct LAYER_OPTS))
extern int _mr_sds_check(union spec_u *specs, struct LAYER_DATA *table, int num,
    int warnmode, int errmode);

struct LAYER_DATA _sds_data = 
    {
         CLASS_SDS ,
         FSS_TYPE,
         "sds" ,
         ".save.ovfl" ,
         0,
         0,
         NUM_SDS_OPTS,
         1 ,
         NUM_MR_SDS_NUMERICS ,
         NUM_SDS_ALIAS ,
         _sds_opts ,
         _mr_sds_numerics ,
         NULL,
         _mr_sds_check
    } ;
