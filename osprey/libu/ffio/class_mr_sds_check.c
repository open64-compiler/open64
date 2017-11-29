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


#pragma ident "@(#) libu/ffio/class_mr_sds_check.c	92.1	06/29/99 13:16:47"
#include <ffio.h>
#include <stdio.h>
#include <liberrno.h>
#include "layer_def.h"
#include "spec_parse.h"

/*
 * This function verifies that the options given with assign -F [mr,sds]
 * are correct. Some verification of options is done before this is called.
 * For example, we know that no bogus options have been selected. But 
 * other options must be looked at all together, and that's what this
 * function does.
 */
_mr_sds_check(
union spec_u *specs,	/* the options */
struct LAYER_DATA *table,	/* the cdc table */
int num,		/* number of valid specs */
int warnmode,
int errmode)
{

union spec_u *spec2;
    if (specs->class_info.class == CLASS_MR ||
        specs->class_info.class == CLASS_SDS) {
        if (num >= 2) {
            specs++;
            spec2 = specs+1;
            if ((specs->info.quan != 0) && (spec2->info.quan != 0)){
                if (specs->info.quan > spec2->info.quan) {
                    if (errmode)
                        _lerror(_LELVL_MSG, ERAS_INMAX, 
                            specs->info.quan,spec2->info.quan);

                    return(ERR);
                }
            }
        }
    }
    return(0);
}
