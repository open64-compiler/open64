/*

  Copyright (C) 2000, 2001 Silicon Graphics, Inc.  All Rights Reserved.

  This program is free software; you can redistribute it and/or modify it
  under the terms of version 2 of the GNU General Public License as
  published by the Free Software Foundation.

  This program is distributed in the hope that it would be useful, but
  WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  

  Further, this software is distributed without any warranty that it is
  free of the rightful claim of any third person regarding infringement 
  or the like.  Any license provided herein, whether implied or 
  otherwise, applies only to this software file.  Patent licenses, if 
  any, provided herein do not apply to combinations of this program with 
  other software, or any other product whatsoever.  

  You should have received a copy of the GNU General Public License along
  with this program; if not, write the Free Software Foundation, Inc., 59
  Temple Place - Suite 330, Boston MA 02111-1307, USA.

  Contact information:  Silicon Graphics, Inc., 1600 Amphitheatre Pky,
  Mountain View, CA 94043, or:

  http://www.sgi.com

  For further information regarding this notice, see:

  http://oss.sgi.com/projects/GenInfo/NoticeExplan

*/

/* USMID @(#) clibinc/fdcconfig.h	92.6	11/16/99 11:55:31 */


#ifndef _FDCCONFIG_H
#define _FDCCONFIG_H

/*
 **************************************************************************
 *
 *			S W I T C H E S
 *	Each of these defines enables or disables one feature of the
 *	data conversion complex.  The first few ending in '_LYR'
 *	enable the layer/class indicated.  For example, ENABLE_IBM_LYR
 *	controls access to the IBM blocking routines.  (-F ibm.xxx on
 *	the assign command)  If the YES on
 *	on this #define is changed to NO, then the references to the
 *	IBM blocking routines will be made 'soft'.   The result of this is
 *	that the routines will still be in the library, but will not be
 *	loaded unless specifically requested.  The command(s) that
 *	allow implicit (asgcmd(1) and assign(1))
 *	assignment of IBM blocking will put out a warning if this change
 *	is made, if the user ignores the warning, and tries to perform
 *	implicit IBM blocking, an error will result.  Similarly for the
 *	numeric/data conversion facilities (-C and -N options on assign)
 *	the ENABLE_IBM_DAT #define controls loading of these routines.
 *
 *	The primary reason to turn these facilities off would be to reduce
 *	the size of user programs.  Fortran loads all of the layers
 *	and numeric conversion routines that are enabled here.
 *
 *	If switched off here, the user can still explicitly force
 *	the loading of the disabled routines, but currently he
 *	needs the names of the disabled routines to do this.
 *	The numeric conversion routines are much larger than the
 *	blocking routines, therefore they are likely candidates to
 *	remove first.
 *
 *	Note that COS, BMX, and other 'non-foreign' layers are also
 *	switchable.  However, COS is currently the default blocking type
 *	for unformatted Fortran files on CX/CEA systems, so it is not
 *	recommended that it be disabled.  The switches are, however
 * 	honored by the FDC routines.
 *
 *	Note also that in libc/fdcio/fxrmisc.c a mask is built of these
 *	switches for run-time checking.
 *
 **************************************************************************
 */

#ifndef YES
#define YES		1
#endif
#ifndef NO
#define NO		0
#endif

#ifdef	_CRAY1
#  define _C1	1 		/* CRAY PVP systems */
#  ifdef  _CRAYIEEE
#    define _CR	0
#  else
#    define _CR	1 		/* CRAY PVP systems with Cray floating point */
#  endif
#else
#  define _C1	0
#endif 

#ifdef	_CRAYMPP
#  define _CM	1 		/* CRAY T3D/T3E MPP systems */
#else
#  define _CM	0
#endif 

#ifdef _CRAYT3E
#  define _CE	1		/* CRAY T3E systems */
#else
#  define _CE	0
#endif

#if defined(_CRAYT3D) && !defined(_UNICOS_MAX)
#  define _CH	1		/* CRAY T3D self-hosted systems */
#else
#  define _CH	0		
#endif

#if defined(_CRAYT3D) && defined(_UNICOS_MAX)
#  define _CD	1		/* CRAY T3D Unicos/MAX systems */
#else
#  define _CD	0		
#endif

#ifdef	_SOLARIS
#  define _S4	1 		/* Solaris with 4 byte words */
#else
#  define _S4	0
#endif 

#ifdef __mips
#  define _IR	1		/* Irix systems */
#else
#  define _IR	0
#endif

#if defined(_LITTLE_ENDIAN)
#  define _IA	1		/* Little Endian systems */
#else
#  define _IA	0
#endif

/*
 *	Enable/disable FFIO layers (-F option on assign).
 */

#define ENABLE_TEXT_LYR		(_C1 || _CM || _IR)	/* text */
#define ENABLE_COS_LYR		(_C1 || _CM || _IR)	/* cos */
#define ENABLE_BMX_LYR		(_C1 || _CH || _CE)	/* tape */
#define ENABLE_IBM_LYR		(_C1 || _IR)		/* ibm */
#define ENABLE_VMS_LYR		(_C1 || _IR)		/* vms */
#define ENABLE_CDC_LYR		( NO)			/* cdc */
#define ENABLE_NVE_LYR		( NO)			/* nosve */
#define ENABLE_UX_LYR		(_C1 || _CM )		/* X class */
#define ENABLE_SDS_LYR		(_C1 || _CD)		/* sds */
#define ENABLE_MR_LYR		(_C1 || _CM)		/* mr */
#define ENABLE_TRC_LYR		( NO)			/* trace */
#define ENABLE_BLX_LYR		(_C1 || _CM)		/* blx */
#define ENABLE_FD_LYR		(_C1 || _CM || _IR)	/* fd */
#define ENABLE_205_LYR		( NO)			/* c205 */
#define ENABLE_CACHE_LYR	(_C1 || _CM || _IR || _IA) /* cache */
/* USER record layer is always disabled/soft */
#define ENABLE_SITE_LYR		( NO)			/* site */
#define ENABLE_ER90B_LYR	( NO)			/* er90 */
#define ENABLE_BUFA_LYR		(_C1 || _CM || _IR)	/* bufa */
#define ENABLE_CACHEA_LYR	(_C1 || _CM || _IR)	/* cachea */
#define ENABLE_EVENT_LYR	(_C1 || _CE || _IR)	/* event */
#define ENABLE_LOCK_LYR		(_C1 || _IR)		/* lock */
#define ENABLE_GLOBAL_LYR	(       _CM || _IR)	/* global */
#define ENABLE_F77_LYR		(_C1 || _CM || _S4 || _IR || _IA) /* f77 class */
#define ENABLE_TMF_LYR		(_IR)			/* tmf */
#define ENABLE_CMP_LYR		(_C1 || _CE)		/* compression */

/*
 *	Enable/disable implicit data conversion types (-N option on assign).
 */

#define ENABLE_CRY_DAT		(_C1 || _CM || _IR)	/* cray y-mp */
#define ENABLE_MIPS_DAT		(_C1 || _IR || _IA)	/* mips */
#define ENABLE_IBM_DAT		(_C1 || _IR)		/* ibm */
#define ENABLE_VMS_DAT		(_CR || _IR)		/* vms */
#define ENABLE_IEG_DAT		(_C1 || _CM || _IR)	/* ieee_32 */
#define ENABLE_IEL_DAT		(_C1 || _IR)		/* ieee_64 */
#define ENABLE_IEU_DAT		(_CR || _IR)		/* ultrix */
#define ENABLE_T3D_DAT		(_CR || _CM)		/* cray mpp */
#define ENABLE_IA_DAT		(_IR || _IA)		/* intel */
#define ENABLE_CDC_DAT		( NO)			/* cdc */
#define ENABLE_NVE_DAT		( NO)			/* nosve */
#define ENABLE_205_DAT		( NO)			/* c205 */
/* USER data is always disabled/soft */
#define ENABLE_SITE_DAT		( NO)			/* site */

#endif	/* !_FDCCONFIG_H */
