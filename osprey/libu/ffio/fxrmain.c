/*
 * Copyright 2004 PathScale, Inc.  All Rights Reserved.
 */

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


#pragma ident "@(#) libu/ffio/fxrmain.c	92.4	10/14/99 17:05:06"

#include <ffio.h>
#include <fdcconfig.h>
#include <stddef.h>
#include "fxlist.h"

/*
 * This is a table that defines the routines that are used
 * to do Flexible File I/O.  For each FFIO layer there is a set of
 * routines that perform the basic functions in that
 * layer.
 *
 * For each entry point, all of the routines for that entry point
 * have the same protocol and parameters.  For example:
 *
 * Parameters: (ffread and ffwrite)
 *      fd      - file descriptor (dummy) returned by ffopen.  This
 *			is really a pointer to the FDINFO block.
 *      bufptr  - bit pointer to data to be transferred.
 *      nbytes  - Number of bytes to be transferred
 *      stat    - pointer to status return word
 *      fulp    - full or partial write mode flag
 *      ubc     - pointer to unused bit count.  This represents a count of
 *			unused bits in the last byte to be transferred.
 *
 * Return:
 *	if (return 0)
 *		Call was successful.
 *		*stat contains information about hitting EOR, or other
 *		informational status
 *	if (return == 0)
 *		call was OK.
 *		*stat is information about status.  This usually
 *		indicates an EOF/EOD condition.
 *	if (return < 0)
 *		call failed
 *		some data may have been transferred.
 *		*stat points to error code.
 */

/*
 * SYSTEM class routines
 */
DECLARE_P(END_XLIST_P)
DECLARE_P(ERR_XLIST_P)
DECLARE_P(SYSTEM_XLIST_P)
DECLARE_P(SYSCALL_XLIST_P)
DECLARE_P(NULL_XLIST_P)

/*
 * COS blocking routines
 */
DECLARE_P(COS_XLIST_P)

/*
 * FD routines (there is only one...)
 */
DECLARE_P(FD_XLIST_P)

/*
 * CACHE routines
 */
DECLARE_P(CCH_XLIST_P)

/*
 * F class routines
 */
#if ! (ENABLE_IBM_LYR || ENABLE_VMS_LYR || ENABLE_NVE_LYR)
SOFTIZE_P(F_XLIST_P)
#endif
DECLARE_P(F_XLIST_P)

/*
 * V class routines
 */
#if ! (ENABLE_IBM_LYR || ENABLE_VMS_LYR || ENABLE_NVE_LYR)
SOFTIZE_P(V_XLIST_P)
#endif
DECLARE_P(V_XLIST_P)

/*
 * X class routines
 */
#if ! (ENABLE_NVE_LYR || ENABLE_CRAY_LYR || ENABLE_UX_LYR || ENABLE_205_LYR)
SOFTIZE_P(X_XLIST_P)
#endif
DECLARE_P(X_XLIST_P)

/*
 * BMX glue routines
 */
#if ! (ENABLE_BMX_LYR)
SOFTIZE_P(TAPE_XLIST_P)
#endif
DECLARE_P(TAPE_XLIST_P)

/*
 * CDC routines
 */
#if ! (ENABLE_CDC_LYR)
SOFTIZE_P(CDC_XLIST_P)
#endif
DECLARE_P(CDC_XLIST_P)

/*
 * SDS routines
 */
#if ! (ENABLE_SDS_LYR)
SOFTIZE_P(SDS_XLIST_P)
#endif
DECLARE_P(SDS_XLIST_P)

/*
 * MR routines
 */
#if ! (ENABLE_MR_LYR)
SOFTIZE_P(MR_XLIST_P)
#endif
DECLARE_P(MR_XLIST_P)

/*
 * TRACE routines
 */
#if ! (ENABLE_TRC_LYR)
SOFTIZE_P(TRC_XLIST_P)
#endif
DECLARE_P(TRC_XLIST_P)

/*
 * TEXT routines
 */
#if ! (ENABLE_TEXT_LYR)
SOFTIZE_P(TXT_XLIST_P)
#endif
DECLARE_P(TXT_XLIST_P)

/*
 * USER routines
 */
#if defined(_SOLARIS) || defined(__mips)
#pragma weak	_usr_ffvect	/* always weak/soft */
#elif	!defined(_LITTLE_ENDIAN)
SOFTIZE_P(USR_XLIST_P)		/* always weak/soft */
#endif
DECLARE_P(USR_XLIST_P)

/*
 * SITE routines
 */
#if ! (ENABLE_SITE_LYR)
#ifdef __mips
#pragma weak _site_ffvect
#else
SOFTIZE_P(SITE_XLIST_P)
#endif
#endif
DECLARE_P(SITE_XLIST_P)

/*
 * BLX routines
 */
#if ! (ENABLE_BLX_LYR)
SOFTIZE_P(BLX_XLIST_P)
#endif
DECLARE_P(BLX_XLIST_P)

/*
 * ER90B routines
 */
#if ! (ENABLE_ER90B_LYR)
SOFTIZE_P(ER90B_XLIST_P)
#endif
DECLARE_P(ER90B_XLIST_P)

/*
 * BUFA routines
 */
#if ! (ENABLE_BUFA_LYR)
SOFTIZE_P(BUFA_XLIST_P)
#endif
DECLARE_P(BUFA_XLIST_P)

/*
 * CACHEA routines
 */
#if ! (ENABLE_CACHEA_LYR)
SOFTIZE_P(CACHEA_XLIST_P)
#endif
DECLARE_P(CACHEA_XLIST_P)

/*
 * EVENT routines
 */
#if ! (ENABLE_EVENT_LYR)
SOFTIZE_P(EVENT_XLIST_P)
#endif
DECLARE_P(EVENT_XLIST_P)

/*
 * LOCK routines
 */
#if ! (ENABLE_LOCK_LYR)
SOFTIZE_P(LOCK_XLIST_P)
#endif
DECLARE_P(LOCK_XLIST_P)

/*
 * GLOBAL routines
 */
#if ! (ENABLE_GLOBAL_LYR)
SOFTIZE_P(GLOBAL_XLIST_P)
#endif
DECLARE_P(GLOBAL_XLIST_P)

/*
 * F77 routines
 */
#if !(ENABLE_F77_LYR)
SOFTIZE_P(F77_XLIST_P)
#endif
DECLARE_P(F77_XLIST_P)

/*
 * TMF routines
 */
#if !(ENABLE_TMF_LYR)
SOFTIZE_P(TMF_XLIST_P)
#endif
DECLARE_P(TMF_XLIST_P)

/*
 * CMP routines
 */
#if !(ENABLE_CMP_LYR)
SOFTIZE_P(CMP_XLIST_P)
#endif
DECLARE_P(CMP_XLIST_P)

/*
 * USERx layer routines.  These are always soft externals.
 */
#ifdef __mips
#pragma weak _usr0_ffvect
DECLARE_P(USR0_XLIST_P)

#pragma weak _usr1_ffvect
DECLARE_P(USR1_XLIST_P)

#pragma weak _usr2_ffvect
DECLARE_P(USR2_XLIST_P)

#pragma weak _usr3_ffvect
DECLARE_P(USR3_XLIST_P)

#pragma weak _usr4_ffvect
DECLARE_P(USR4_XLIST_P)

#pragma weak _usr5_ffvect
DECLARE_P(USR5_XLIST_P)

#pragma weak _usr6_ffvect
DECLARE_P(USR6_XLIST_P)

#pragma weak _usr7_ffvect
DECLARE_P(USR7_XLIST_P)

#pragma weak _usr8_ffvect
DECLARE_P(USR8_XLIST_P)

#pragma weak _usr9_ffvect
DECLARE_P(USR9_XLIST_P)
#else
SOFTIZE_P(USR0_XLIST_P)
DECLARE_P(USR0_XLIST_P)

SOFTIZE_P(USR1_XLIST_P)
DECLARE_P(USR1_XLIST_P)

SOFTIZE_P(USR2_XLIST_P)
DECLARE_P(USR2_XLIST_P)

SOFTIZE_P(USR3_XLIST_P)
DECLARE_P(USR3_XLIST_P)

SOFTIZE_P(USR4_XLIST_P)
DECLARE_P(USR4_XLIST_P)

SOFTIZE_P(USR5_XLIST_P)
DECLARE_P(USR5_XLIST_P)

SOFTIZE_P(USR6_XLIST_P)
DECLARE_P(USR6_XLIST_P)

SOFTIZE_P(USR7_XLIST_P)
DECLARE_P(USR7_XLIST_P)

SOFTIZE_P(USR8_XLIST_P)
DECLARE_P(USR8_XLIST_P)

SOFTIZE_P(USR9_XLIST_P)
DECLARE_P(USR9_XLIST_P)
#endif
/*
 *	This is the main table of function pointers.
 */
#ifdef	_UNICOS
struct xtr_s *_recfm_tab[NUM_CLASSES] =
{
	&END_XLIST_P,
	&SYSCALL_XLIST_P,
	&NULL_XLIST_P,
	&SYSTEM_XLIST_P,
	&COS_XLIST_P,
	&TAPE_XLIST_P,
	&F_XLIST_P,
	&V_XLIST_P,
	&TXT_XLIST_P,
	&X_XLIST_P,
	&CDC_XLIST_P,
	&SDS_XLIST_P,
	&MR_XLIST_P,
	&TRC_XLIST_P,
	&USR_XLIST_P,
	&SITE_XLIST_P,
	&ERR_XLIST_P,	/* error (generate errors) */
	&FD_XLIST_P,	/* fd (user specified) */
	&BLX_XLIST_P,	/* blx */
	&CCH_XLIST_P,	/* cache */
	&ER90B_XLIST_P, /* er90 byte-stream */
	&BUFA_XLIST_P,  /* bufa */
	&CACHEA_XLIST_P, /* cachea */
	&EVENT_XLIST_P,	/* event */
	&LOCK_XLIST_P,	/* lock */
	&GLOBAL_XLIST_P,/* global */
	&F77_XLIST_P,	/* f77 */
	&TMF_XLIST_P,	/* tmf (IRIX only) */
	&CMP_XLIST_P,	/* compression */
	NULL,		/* 29 */
	&USR0_XLIST_P,	/* user0 */
	&USR1_XLIST_P,	/* user1 */
	&USR2_XLIST_P,	/* user2 */
	&USR3_XLIST_P,	/* user3 */
	&USR4_XLIST_P,	/* user4 */
	&USR5_XLIST_P,	/* user5 */
	&USR6_XLIST_P,	/* user6 */
	&USR7_XLIST_P,	/* user7 */
	&USR8_XLIST_P,	/* user8 */
	&USR9_XLIST_P,	/* user9 */
};
#elif   defined(KEY)
struct xtr_s *_recfm_tab[NUM_CLASSES] =
{
	&END_XLIST_P,
	&SYSCALL_XLIST_P,
	&NULL_XLIST_P,
	&SYSTEM_XLIST_P,
	&COS_XLIST_P,
	NULL,
	&F_XLIST_P,
	&V_XLIST_P,
	&TXT_XLIST_P,
	NULL,
	NULL,
	NULL,
	NULL,
	NULL,
	NULL,
	NULL,
	&ERR_XLIST_P,	/* error (generate errors) */
	&FD_XLIST_P,	/* fd (user specified) */
	NULL,           /* blx */
	&CCH_XLIST_P,	/* cache */
	NULL,           /* er90 byte-stream */
	&BUFA_XLIST_P,  /* bufa */
	&CACHEA_XLIST_P, /* cachea */
	&EVENT_XLIST_P,	/* event */
	&LOCK_XLIST_P,	/* lock */
	NULL,           /* global */
	&F77_XLIST_P,	/* f77 */
	NULL,           /* tmf (IRIX only) */
	NULL,           /* compression */
	NULL,		/* 29 */
	NULL,   	/* user0 */
	NULL,	        /* user1 */
	NULL,           /* user2 */
	NULL,           /* user3 */
	NULL,           /* user4 */
	NULL,           /* user5 */
	NULL,           /* user6 */
	NULL,           /* user7 */
	NULL,           /* user8 */
	NULL,           /* user9 */
};
#elif	defined(__mips)
struct xtr_s *_recfm_tab[NUM_CLASSES] =
{
	&END_XLIST_P,
	&SYSCALL_XLIST_P,
	&NULL_XLIST_P,
	&SYSTEM_XLIST_P,
	&COS_XLIST_P,
	NULL,
	&F_XLIST_P,
	&V_XLIST_P,
	&TXT_XLIST_P,
	NULL,
	NULL,
	NULL,
	NULL,
	NULL,
	&USR_XLIST_P,
	&SITE_XLIST_P,
	&ERR_XLIST_P,	/* error (generate errors) */
	&FD_XLIST_P,	/* fd (user specified) */
	NULL,
	&CCH_XLIST_P,	/* cache */
	NULL,
	&BUFA_XLIST_P,	/* bufa */
	&CACHEA_XLIST_P, /* cachea */
	&EVENT_XLIST_P,	/* event */
	&LOCK_XLIST_P,	/* lock */
	&GLOBAL_XLIST_P,/* global */
	&F77_XLIST_P,	/* f77 */
	&TMF_XLIST_P,	/* tmf */
	NULL,		/* 28 */
	NULL,		/* 29 */
	&USR0_XLIST_P,	/* user0 */
	&USR1_XLIST_P,	/* user1 */
	&USR2_XLIST_P,	/* user2 */
	&USR3_XLIST_P,	/* user3 */
	&USR4_XLIST_P,	/* user4 */
	&USR5_XLIST_P,	/* user5 */
	&USR6_XLIST_P,	/* user6 */
	&USR7_XLIST_P,	/* user7 */
	&USR8_XLIST_P,	/* user8 */
	&USR9_XLIST_P,	/* user9 */
};
#elif	defined(_LITTLE_ENDIAN)
struct xtr_s *_recfm_tab[NUM_CLASSES] =
{
	&END_XLIST_P,
	&SYSCALL_XLIST_P,
	&NULL_XLIST_P,	/* null */
	&SYSTEM_XLIST_P,
	&COS_XLIST_P,	/* cos */
	NULL,
	NULL,
	NULL,
	&TXT_XLIST_P,	/* txt */
	NULL,
	NULL,
	NULL,
	NULL,
	NULL,
	NULL,		/* usr - no way to do weak now. */
	NULL,		/* site */
	&ERR_XLIST_P,	/* error (generate errors) */
	NULL,		/* fd */
	NULL,		/* blx - unicos only */
	&CCH_XLIST_P,	/* cache */
	NULL,		/* er90 byte-stream */
	NULL,		/* bufa */
	NULL,		/* cachea */
	NULL,		/* event */
	NULL,		/* lock */
	NULL,		/* global */
	&F77_XLIST_P,	/* f77 */
	NULL,		/* tmf */
	NULL,		/* 28 */
	NULL,		/* 29 */
	NULL,		/* 30, usr0 */
	NULL,		/* 31 */
	NULL,		/* 32 */
	NULL,		/* 33 */
	NULL,		/* 34 */
	NULL,		/* 35 */
	NULL,		/* 36 */
	NULL,		/* 37 */
	NULL,		/* 38 */
	NULL,		/* 39, usr9 */
};
#else
struct xtr_s *_recfm_tab[NUM_CLASSES] =
{
	&END_XLIST_P,
	&SYSCALL_XLIST_P,
	&NULL_XLIST_P,
	&SYSTEM_XLIST_P,
	NULL,
	NULL,
	NULL,
	NULL,
	NULL,
	NULL,
	NULL,
	NULL,
	NULL,
	NULL,
	&USR_XLIST_P,
	NULL,
	&ERR_XLIST_P,	/* error (generate errors) */
	NULL,		/* fd (user specified) */
	NULL,		/* blx */
	NULL,		/* cache */
	NULL,		/* er90 byte-stream */
	NULL,		/* bufa */
	NULL,		/* cachea */
	NULL,		/* event */
	NULL,
	NULL,
	&F77_XLIST_P,
	NULL,		/* tmf */
	NULL,		/* 28 */
	NULL,		/* 29 */
	NULL,		/* 30 */
	NULL,		/* 31 */
	NULL,		/* 32 */
	NULL,		/* 33 */
	NULL,		/* 34 */
	NULL,		/* 35 */
	NULL,		/* 36 */
	NULL,		/* 37 */
	NULL,		/* 38 */
	NULL,		/* 39 */
};
#endif
