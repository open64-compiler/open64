
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

/* USMID @(#) libu/ffio/ff_all.cpp	92.0	10/08/98 14:57:41 */


#include <fdcconfig.h>
#include "fxlist.h"

*
* IBM, VMS, NOSVE and other F and V class
* record blocking routines
*
LOADIT(F_XLIST_P)
LOADIT(V_XLIST_P)
#ifndef _CRAYIEEE
*
* IBM numerics
*
HARDREF=CRAY2IBM,IBM2CRAY
#endif

#ifndef _CRAYIEEE
*
* VAX/VMS numerics
*
HARDREF=CRAY2VAX,VAX2CRAY
#endif

*
* NOS/VE, f77 and ETA blocking routines
*
LOADIT(X_XLIST_P)

#ifndef _CRAYIEEE
*
* NOS/VE numerics
*
HARDREF=CRAY2NVE,NVE2CRAY
#endif

#ifndef _CRAYIEEE
*
* ETA numerics
*
HARDREF=CRAY2ETA,ETA2CRAY
#endif

#if defined(_CRAYMPP) || !defined(_CRAYIEEE)
*
* IEEE numerics
*
HARDREF=CRAY2IEG,IEG2CRAY
#endif

*
* COS blocking routines
*
LOADIT(COS_XLIST_P)

#ifndef _CRAYMPP
*
* BMX/TAPE routines
*
LOADIT(TAPE_XLIST_P)
#endif

#ifndef _CRAYIEEE
*
* CDC routines
*
LOADIT(CDC_XLIST_P)
HARDREF=CRAY2CDC,CDC2CRAY
#endif

#if defined(_CRAY1) || (defined(_CRAYMPP) && defined(_UNICOS_MAX))
*
* SDS routines
*
LOADIT(SDS_XLIST_P)
#endif

#ifdef  _CRAY1
*
* ER90 byte stream routines
*
LOADIT(ER90B_XLIST_P)
#endif

*
* MR routines
*
LOADIT(MR_XLIST_P)

*
* TRACE routines
*
LOADIT(TRC_XLIST_P)

*
* TEXT routines
*
LOADIT(TXT_XLIST_P)

*
* USER routines are *always* soft
*
****LOADIT(USR_XLIST_P)

*
* SITE routines
*
#if ENABLE_SITE_LYR
LOADIT(SITE_XLIST_P)
#else
* layer not enabled
#endif
#if ENABLE_SITE_DAT
HARDREF=CRAY2STE,STE2CRAY
#else
* data conversion not enabled
#endif

*
* FD routines
*
LOADIT(FD_XLIST_P)

*
* BLX routines
*
LOADIT(BLX_XLIST_P)

*
* CACHE routines
*
LOADIT(CCH_XLIST_P)

*
* CACHEA routines
*
LOADIT(CCA_XLIST_P)

*
* EVENT routines
*
LOADIT(EVNT_XLIST_P)

#if defined(_CRAYMPP) 
*
* GLOBAL routines
*
LOADIT(GLOBAL_XLIST_P)
#endif
