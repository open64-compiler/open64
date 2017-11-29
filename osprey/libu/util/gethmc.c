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


#pragma ident "@(#) libu/util/gethmc.c	92.2	11/24/99 14:11:55"

#ifdef _UNICOS
#include <sys/target.h>
#endif
#include "targlib.h"

#ifdef _UNICOS
#pragma _CRI duplicate GETHMC@ as GETHMC
#pragma _CRI duplicate GETHMC2@ as GETHMC2


extern struct target __target;

GETHMC@(pmctable)
long pmctable[128];
{

	memwcpy(pmctable,&__target,128);
	_setsubtype(PM_HOST, (struct mctable *)pmctable);
	return(0);
}
_gethmc(pmctable)
long pmctable[128];
{

	memwcpy(pmctable,&__target,128);
	return(0);
}
#endif


/*
 * Sets the machine subtype field in the 128-word machine characteristics
 * table. Here's the algorithm:
 *   If the subtype field is set, return
 *   if primary machine == C90, T3D, T3E, SPARC, set subtype = primary	
 *   else if primary machine == TS, set subtype according to ieee charac.
 *   else if (primary machine == YMP){ 
 *	If ipm == PM_HOST or PM_TARGET {
 *	   then the TARGET environment
 *	   variable was not set, and we got the
 *         machine characteristics table from the target() system call. In that
 *         case,
 *	A:
 * 		if clock == 30000, subtype = EL
 *		else if clock == 10000, subtype = J90
 *		else subtype = YMP
 *	}
 *      else (ipm must be PM_CRAY_YMP, PM_CRAY_EL, PM_CRAY_J90, PM_CRAY_JSE, or
 *		PM_CRAY_SV1) {
 *		if (ipm = PM_CRAY_EL) subtype = EL
 *		else if (ipm = PM_CRAY_J90) subtype = J90
 *		else if (ipm = PM_CRAY_JSE) subtype = JSE
 *		else if (ipm = PM_CRAY_SV1) subtype = SV1
 *		else if (ipm = PM_CRAY_YMP) goto A
 *	}
 *   }	
 *		
 */
void
_setsubtype(int ipm, struct mctable *mctable)
{
	if (mctable->mcsubt[0] != 0 || mctable->mcsubt[1] != 0) {
		return;
	}
	switch(ipm) {
		default:
		case PM_CRAY_C90:
		case PM_CRAY_T3D:
		case PM_CRAY_T3E:
#ifndef _UNICOS
		case PM_SPARC:
#endif
		case PM_IOP:
			mctable->mcsubt[0] = mctable->mcpmt;	/* subtype = primary */
			break;
		case PM_HOST:
		case PM_TARGET:
			/* we must have gotten this info from target() system call */
			if (mctable->mcpmt == CRAY_TS_NAME) {
				if (mctable->mctlog.mctblxy.mcieee == 0)
					mctable->mcsubt[0] = mctable->mcpmt;
				else {
					mctable->mcsubt[0] = CRAY_TSIEEE_SUBTYPE0;
					mctable->mcsubt[1] = CRAY_TSIEEE_SUBTYPE1;
				}
				break;
			}
			if (mctable->mcpmt != CRAY_YMP_NAME) {
				mctable->mcsubt[0] = mctable->mcpmt;	
				break;
			}	
			/* fall through */
		case PM_CRAY_YMP:
			if (mctable->mcclk == 30000) 
				mctable->mcsubt[0] = CRAY_EL_SUBTYPE;	
			else if (mctable->mcclk == 10000) 
				mctable->mcsubt[0] = CRAY_J90_SUBTYPE;	
			else
				mctable->mcsubt[0] = CRAY_YMP_NAME;
			break;
		case PM_CRAY_J90:
			mctable->mcsubt[0] = CRAY_J90_SUBTYPE;
			break;
		case PM_CRAY_JSE:
			mctable->mcsubt[0] = CRAY_JSE_SUBTYPE;
			break;
		case PM_CRAY_SV1:
			mctable->mcsubt[0] = CRAY_SV1_SUBTYPE;
			break;
		case PM_CRAY_EL:
			mctable->mcsubt[0] = CRAY_EL_SUBTYPE;
			break;
		case PM_CRAY_TS:
			if (mctable->mctlog.mctblxy.mcieee == 0)
				mctable->mcsubt[0] = mctable->mcpmt;	/* subtype = primary */
			else {
				mctable->mcsubt[0] = CRAY_TSIEEE_SUBTYPE0;
				mctable->mcsubt[1] = CRAY_TSIEEE_SUBTYPE1;
			}
			break;
			
	}
}

/*
 *      When SV1 was released it was a subtype of YMP.  Later
 *      SV1 became a primary machine type.
 *
 *      GETHMC2 is a variant of GETHMC for migration from
 *      primary type CRAY-YMP, subtype SV1 to type CRAY-SV1
 *      subtype SV1.  GETHMC2 is called by products which need
 *      the SV1 primary type, while GETHMC and GETHMC@ still
 *      consider SV1 a subtype of CRAY-YMP.  The subtype is
 *      SV1 in both cases.
 *
 *      PM_CRAY_SV1, PM_CRAY_J90, etc. result from the name look
 *      up in table pmt[]. _setsubtype then determines the subtype. 
 *      If subtype is SV1 __setsv1type forces the type to SV1.
 */

#ifdef _UNICOS

/*
 * __setsv1type() tests the machine subtype field in the machine
 * characteristics table.  If the subtype is 'CRAY-SV1',
 * set the primary type to 'CRAY-SV1'.
 */
void
__setsv1type(struct mctable *mctable)
{
	if (mctable->mcsubt[0] == CRAY_SV1_SUBTYPE && mctable->mcsubt[1] == 0) {
		mctable->mcpmt = CRAY_SV1_NAME;
	}
}
GETHMC2@(pmctable)
long pmctable[128];
{
	memwcpy(pmctable,&__target,128);
	_setsubtype(PM_HOST, (struct mctable *)pmctable);
	__setsv1type((struct mctable *)pmctable);
	return(0);
}
#endif
