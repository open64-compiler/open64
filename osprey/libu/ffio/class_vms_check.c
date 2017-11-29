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


#pragma ident "@(#) libu/ffio/class_vms_check.c	92.1	06/29/99 13:16:47"
#include <ffio.h>
#include <stdio.h>
#include <liberrno.h>
#include "layer_def.h"
#include "spec_parse.h"
#include "gfio.h"
#include "gvio.h"
#include "vms_def.h"

/*
 * This function sets the str1 and str2 fields of the spec_u
 * structure  and then calls _class_vf_check to verify that the
 * options given with assign -F vms are correct.
 * Some verification of options is done before this is called.
 */
_class_vms_check(
union spec_u *specs,	/* the options */
struct LAYER_DATA *table,	/* the cdc table */
int num,		/* number of valid specs */
int warnmode,
int errmode)
{
int subtype;
int type;
    type = specs->class_info.info1;
    subtype = specs->class_info.info2;
    specs->class_info.info1 = 0;
    if (type == VMS_F) {
        specs->class_info.class = CLASS_F;
        switch(subtype){
            case VMS_TAPE:
                specs->hdr.str1 = TR_VMS_F_TP;
                break;
            case VMS_DSK:
            case VMS_BB:
                specs->hdr.str1 = TR_VMS_F_DSK;
                break;
            case VMS_TR:
                specs->hdr.str1 = TR_VMS_F_TR;
                break;
            default:
                goto badrf;
        }
    }
    else if (type == VMS_S) {
        specs->class_info.class = CLASS_V;
        switch(subtype){
            case VMS_TAPE:
                specs->hdr.str1 = TR_VMS_S_TP;
                break;
            case VMS_DSK:
            case VMS_BB:
            case VMS_NONL:
                specs->hdr.str1 = TR_VMS_S_DSK;
                break;
            case VMS_TR:
                specs->hdr.str1 = TR_VMS_S_TR;
                break;
            default:
                goto badrf;
        }
    }
    else if (type == VMS_V){
        specs->class_info.class = CLASS_V;
        switch(subtype){
            case VMS_TAPE:
                specs->hdr.str1 = TR_VMS_V_TP;
                break;
            case VMS_DSK:
            case VMS_BB:
                specs->hdr.str1 = TR_VMS_V_DSK;
                break;
            case VMS_TR:
                specs->hdr.str1 = TR_VMS_V_TR;
                break;
            default:
                goto badrf;
        }
    }
    else {
         return(ERR);
    }
    specs->class_info.info2 = 0;
    return(_class_vf_check(specs,table,num,warnmode,errmode));
badrf:
    if (errmode)
        _lerror(_LELVL_MSG, ERAS_BADRFMT);
    return(ERR);
}
