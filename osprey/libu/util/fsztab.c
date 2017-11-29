/*
 * Copyright 2003, 2004 PathScale, Inc.  All Rights Reserved.
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


#pragma ident "@(#) libu/util/fsztab.c	92.7	11/16/99 15:10:31"


#include <fdcconfig.h>
#include <ffio.h>
#include <cray/fndc.h>

/*
 *	The _dsz_s tables tell the size of the data items according to
 *	the mapping of implicit types for implicit conversion.  These
 *	structures allow the data item sizes to be defined in the
 *	routines that do the conversion, and does not require that any
 *	of them be included in the load.  Each _dsz_s table is also a
 *	'common' block.
 */

#ifdef	_CRAY
#define	CRY_SZ	G@CRY@SZ	/* NCV_CRAY */
#define	IBM_SZ	G@IBM@SZ	/* NCV_IBM */
#define	CDC_SZ	G@CDC@SZ	/* NCV_CDC */
#define	VAX_SZ	G@VAX@SZ	/* NCV_VMS */
#define	IEG_SZ	G@IEG@SZ	/* NCV_IEG */
#define	NVE_SZ	G@NVE@SZ	/* NCV_NVE */
#define	ETA_SZ	G@ETA@SZ	/* NCV_205 */
#define	IEU_SZ	G@IEU@SZ	/* NCV_IEU */
#define	IED_SZ	G@IED@SZ	/* NCV_IED */
#define	USR_SZ	G@USR@SZ	/* NCV_USER */
#define	STE_SZ	G@STE@SZ	/* NCV_SITE */
#define	IBD_SZ	G@IBD@SZ	/* NCV_IBD */
#define	VAD_SZ	G@VAD@SZ	/* NCV_VAD */
#define	IUD_SZ	G@IUD@SZ	/* NCV_IUD */
#define	MPP_SZ	G@CRI@SZ	/* NCV_T3D */
#define	IEL_SZ	G@CRY@SZ	/* NCV_IEL */
#define	MIP_SZ	G@MIP@SZ	/* NCV_MIPS */
#define	INA_SZ	G@INA@SZ	/* NCV_IA */

#define CRY_AL	G@64B@AL	/* NCV_CRAY Y-MP, C90, T90 */
#define IBM_AL	G@IBM@AL	/* NCV_IBM */
#define CDC_AL	G@CDC@AL	/* NCV_CDC */
#define VAX_AL	G@VAX@AL	/* NCV_VMS */
#define IEG_AL	G@IEG@AL	/* NCV_IEG */
#define NVE_AL	G@NVE@AL	/* NCV_NVE */
#define ETA_AL	G@ETA@AL	/* NCV_205 */
#define IEU_AL	G@IEG@AL	/* NCV_IEU */
#define IED_AL	G@IED@AL	/* NCV_IED */
#define USR_AL	G@USR@AL	/* NCV_USER */
#define STE_AL	G@STE@AL	/* NCV_SITE */
#define IBD_AL	G@IBD@AL	/* NCV_IBD */
#define VAD_AL	G@VAD@AL	/* NCV_VAD */
#define IUD_AL	G@IUD@AL	/* NCV_IUD */
#define MPP_AL	G@64B@AL	/* NCV_T3D */
#define IEL_AL	G@64B@AL	/* NCV_IEL */
#define MIP_AL	G@MIP@AL	/* NCV_MIPS */
#define INA_AL	G@INA@AL	/* NCV_IA */

#else

#define	CRY_SZ	g$cry$sz_	/* NCV_CRAY */
#define	IBM_SZ	g$ibm$sz_	/* NCV_IBM */
#define	CDC_SZ	g$cdc$sz_	/* NCV_CDC */
#define	VAX_SZ	g$vax$sz_	/* NCV_VMS */
#define	IEG_SZ	g$ieg$sz_	/* NCV_IEG */
#define	NVE_SZ	g$nve$sz_	/* NCV_NVE */
#define	ETA_SZ	g$eta$sz_	/* NCV_205 */
#define	IEU_SZ	g$ieu$sz_	/* NCV_IEU */
#define	IED_SZ	g$ied$sz_	/* NCV_IED */
#define	USR_SZ	g$usr$sz_	/* NCV_USER */
#define	STE_SZ	g$ste$sz_	/* NCV_SITE */
#define	IBD_SZ	g$ibd$sz_	/* NCV_IBD */
#define	VAD_SZ	g$vad$sz_	/* NCV_VAD */
#define	IUD_SZ	g$iud$sz_	/* NCV_IUD */
#define	MPP_SZ	g$cri$sz_	/* NCV_T3D */
#define	IEL_SZ	g$cry$sz_	/* NCV_IEL */
#define	MIP_SZ	g$mip$sz_	/* NCV_MIPS */
#define	INA_SZ	g$ina$sz_	/* NCV_IA */

#define CRY_AL	g$64b$al_	/* NCV_CRAY Y-MP, C90, T90 */
#define IBM_AL	g$ibm$al_	/* NCV_IBM */
#define CDC_AL	g$cdc$al_	/* NCV_CDC */
#define VAX_AL	g$vax$al_	/* NCV_VMS */
#define IEG_AL	g$ieg$al_	/* NCV_IEG */
#define NVE_AL	g$nve$al_	/* NCV_NVE */
#define ETA_AL	g$eta$al_	/* NCV_205 */
#define IEU_AL	g$ieg$al_	/* NCV_IEU */
#define IED_AL	g$ied$al_	/* NCV_IED */
#define USR_AL	g$usr$al_	/* NCV_USER */
#define STE_AL	g$ste$al_	/* NCV_SITE */
#define IBD_AL	g$ibd$al_	/* NCV_IBD */
#define VAD_AL	g$vad$al_	/* NCV_VAD */
#define IUD_AL	g$iud$al_	/* NCV_IUD */
#define MPP_AL	g$64b$al_	/* NCV_T3D */
#define IEL_AL	g$64b$al_	/* NCV_IEL */
#define MIP_AL	g$mip$al_	/* NCV_MIPS */
#define INA_AL	g$ina$al_	/* NCV_IA */

#endif

#ifdef	_CRAYMPP
struct _dsz_s	CRY_SZ;		/* Defined elsewhere */
#else
struct _dsz_s	CRY_SZ = {
	/* number of types */
		8,
/*	none	int	real	dble	cmplx	log	char	sint    */
	64,	64,	64,	128,	128,	64,	8,	64 };
#endif

#if	!defined(_CRAYIEEE) && defined(_CRAY)
static struct _dsz_s	CRI_SZ;
#else
static struct _dsz_s	CRI_SZ = {
	/* number of types */
		8,
/*	none	int	real	dble	cmplx	log	char	sint    */
	64,	64,	64,	64,	128,	64,	8,	64 };
#endif

#ifdef	_CRAYIEEE		/* CRAY T3D/T3E/T90 IEEE */
struct _dsz_s	IBM_SZ = {
	/* number of types */
		9,
/*	none	int	real	dble	cmplx	log	char	sint	spec */
	64,	32,	32,	64,	64,	32,	8,	16,	64 };
#else
struct _dsz_s	IBM_SZ;
#endif

#ifdef	_CRAYMPP
struct _dsz_s	MIP_SZ;
#else
struct _dsz_s	MIP_SZ = {	/* MIPS Systems */
	/* number of types */
		9,
/*	none	int	real	dble	cmplx	log	char	sint    spec*/
	64,	32,	32,	64,	64,	32,	8,	16,	64 };
#endif

#ifndef	_CRAY
struct _dsz_s	IEG_SZ = {	/* IEEE generic */
	/* number of types */
		9,
/*	none	int	real	dble	cmplx	log	char	sint    spec*/
	64,	32,	32,	64,	64,	32,	8,	16,	64 };

struct _dsz_s	VAX_SZ = {	/* VAX */
	/* number of types */
		9,
/*	none	int	real	dble	cmplx	log	char	sint    spec*/
	64,	32,	32,	64,	64,	32,	8,	16,	64 };

struct _dsz_s	IBM_SZ = {	/* IBM */
	/* number of types */
		9,
/*	none	int	real	dble	cmplx	log	char	sint	spec */
	64,	32,	32,	64,	64,	32,	8,	16,	64 };
#endif

struct _dsz_s	INA_SZ = {	/* Intel */
	/* number of types */
		8,
/*	none	int	real	dble	cmplx	log	char	sint	spec */
	64,	32,	32,	64,	64,	32,	8,	16 };

struct _dsz_s	CDC_SZ;
struct _dsz_s	VAX_SZ;
struct _dsz_s	IEG_SZ;
struct _dsz_s	NVE_SZ;
struct _dsz_s	ETA_SZ;
struct _dsz_s	IEU_SZ;
struct _dsz_s	IED_SZ;
struct _dsz_s	USR_SZ;
struct _dsz_s	STE_SZ;
struct _dsz_s	IBD_SZ;
struct _dsz_s	VAD_SZ;
struct _dsz_s	IUD_SZ;
struct _dsz_s	MIP_SZ;

#define	SZNULL	(struct _dsz_s	*)0

struct _dsz_s	*__fndc_f77sz[NCV_MAX] = {
	SZNULL,		/* NCV_NONE */
	&CRY_SZ,	/* NCV_CRAY */
	&IBM_SZ,	/* NCV_IBM */
	&CDC_SZ,	/* NCV_CDC */
	&VAX_SZ,	/* NCV_VMS */
	&IEG_SZ,	/* NCV_IEG */
	&NVE_SZ,	/* NCV_NVE */
	&ETA_SZ,	/* NCV_205 */
	&IEU_SZ,	/* NCV_IEU */
	&IED_SZ,	/* NCV_IED */
	&USR_SZ,	/* NCV_USER */
	&STE_SZ,	/* NCV_SITE */
	&IBD_SZ,	/* NCV_IBD */
	&VAD_SZ,	/* NCV_VAD */
	&IUD_SZ,	/* NCV_IUD */
	&CRI_SZ,	/* NCV_T3D */
	&CRY_SZ,	/* NCV_IEL */
	&MIP_SZ,	/* NCV_MIPS */
	&INA_SZ,	/* NCV_IA */
};

/*
 *	The __fndc_charsz table defines the length of character data,
 *	in bits, for implicit conversion.
 */

int	__fndc_charsz[CS_MAX] = {
	0,		/* CS_NONE */
	8,		/* CS_ASCII */
	8,		/* CS_EBCDIC */
	6,		/* CS_CDC */
	8,		/* CS_USER */
	8		/* CS_SITE */
};

/*
 *	The _dal_s table sets up the alignment of the data items
 *	according to the conversion types for implicit conversion.
 *	These structures allow the alignment to be defined in the
 *	routines that do the conversion, and does not require that
 *	any of them be included in the load.  Each _dal_s table is
 *	also a 'common' block.
 */

/*
 * Define a _dal_s table entry for all CRAY 64-bit systems.
 */

#ifdef	_CRAY
struct _dal_s	G@64B@AL = {
#else
struct _dal_s	g$64b$al_ = {
#endif
	YES,			/* YES, please align */
	0,			/* (unused) */
	64,			/* On 64-bit boundaries */
#ifdef	_CRAY
	0x2020202020202020	/* With ASCII blanks */
#else
	0x2020202020202020LL	/* With ASCII blanks */
#endif
};

#ifdef	_CRAYIEEE		/* CRAY T3D/T3E/T90 IEEE */
struct _dal_s	IBM_AL = {
	NO,			/* NO, don't align */
	0,			/* (unused) */
	0,			/* (unused) */
	0			/* (unused) */
};
#else
struct _dal_s	IBM_AL;
#endif

#ifndef	__mips
struct _dal_s	MIP_AL = {
	NO,			/* NO, don't align */
	0,			/* (unused) */
	0,			/* (unused) */
	0			/* (unused) */
};
#endif

#ifndef	_CRAY
struct _dal_s	IEG_AL = {	/* IEG */
	NO,			/* NO, don't align */
	0,			/* (unused) */
	0,			/* (unused) */
	0			/* (unused) */
};

struct _dal_s	VAX_AL = {	/* VAX */
	NO,			/* NO, don't align */
	0,			/* (unused) */
	0,			/* (unused) */
	0			/* (unused) */
};

struct _dal_s	IBM_AL = {	/* IBM */
	NO,			/* NO, don't align */
	0,			/* (unused) */
	0,			/* (unused) */
	0			/* (unused) */
};
#endif

struct _dal_s	INA_AL = {	/* Intel */
	NO,			/* NO, don't align */
	0,			/* (unused) */
	0,			/* (unused) */
	0			/* (unused) */
};

struct _dal_s	CDC_AL;
struct _dal_s	VAX_AL;
struct _dal_s	IEG_AL;
struct _dal_s	NVE_AL;
struct _dal_s	ETA_AL;
struct _dal_s	IEU_AL;
struct _dal_s	IED_AL;
struct _dal_s	USR_AL;
struct _dal_s	STE_AL;
struct _dal_s	IBD_AL;
struct _dal_s	VAD_AL;
struct _dal_s	IUD_AL;
struct _dal_s	MIP_AL;

#define	ALNULL	(struct _dal_s	*)0

struct _dal_s	*__fndc_align[NCV_MAX] = {
#ifdef	_CRAY
	&G@64B@AL,	/* NCV_NONE (default alignment--no data conversion) */
#elif	defined(__mips)
	&g$mip$al_,	/* NCV_NONE (default alignment--no data conversion) */
#elif	defined(_LITTLE_ENDIAN)
	&g$ina$al_,	/* NCV_NONE (default alignment--no data conversion) */
#else
	ALNULL,		/* NCV_NONE */
#endif
	&CRY_AL,	/* NCV_CRAY Y-MP, C90, T90 */
	&IBM_AL,	/* NCV_IBM */
	&CDC_AL,	/* NCV_CDC */
	&VAX_AL,	/* NCV_VMS */
	&IEG_AL,	/* NCV_IEG */
	&NVE_AL,	/* NCV_NVE */
	&ETA_AL,	/* NCV_205 */
	&IEU_AL,	/* NCV_IEU */
	&IED_AL,	/* NCV_IED */
	&USR_AL,	/* NCV_USER */
	&STE_AL,	/* NCV_SITE */
	&IBD_AL,	/* NCV_IBD */
	&VAD_AL,	/* NCV_VAD */
	&IUD_AL,	/* NCV_IUD */
	&MPP_AL,	/* NCV_T3D */
	&IEL_AL,	/* NCV_IEL */
	&MIP_AL,	/* NCV_MIPS */
	&INA_AL		/* NCV_IA */
};

#if	defined(_CRAY1) && !defined(_CRAYIEEE)
#if	(ENABLE_T3D_DAT == NO) && !ALL_F
#pragma soft CRAY2CRI
#pragma soft CRI2CRAY
#endif
extern int	CRAY2CRI(), CRI2CRAY();		/* NCV_T3D */
#endif	/* _CRAY1 && !_CRAYIEEE */

#if	defined(_CRAYMPP)
#if	(ENABLE_CRY_DAT == NO) && !ALL_F
#pragma soft CRAY2CRI
#pragma soft CRI2CRAY
#endif
extern int	CRAY2CRI(), CRI2CRAY();		/* NCV_CRAY */
#endif	/* _CRAYMPP */

#if	(ENABLE_MIPS_DAT == NO) && !ALL_F
#ifdef	__mips
#pragma weak mips2cry_
#pragma weak cry2mips_
#else
#pragma soft MIPS2CRY
#pragma soft CRY2MIPS
#endif
#endif
#ifdef	__mips
extern int	mips2cry_(), cry2mips_();	/* NCV_MIPS */
#else
extern int	MIPS2CRY(), CRY2MIPS();		/* NCV_MIPS */
#endif

#ifdef	_CRAY
#if	(ENABLE_IBM_DAT == NO) && !ALL_F
#pragma soft IBM2CRAY
#pragma soft CRAY2IBM
#pragma soft IBM2CRI
#pragma soft CRI2IBM
#pragma soft IBD2CRAY@
#pragma soft CRAY2IBD@
#endif
extern int	IBM2CRAY(),  CRAY2IBM();	/* NCV_IBM */
extern int	IBM2CRI(),   CRI2IBM();		/* NCV_IBM */
extern int	IBD2CRAY@(), CRAY2IBD@();	/* NCV_IBD (ibm_dp) */
#elif	defined(__mips)
#if	(ENABLE_IBM_DAT == NO) && !ALL_F
#pragma weak mips2ibm_
#pragma weak ibm2mips_
#endif
extern int	mips2ibm_(), ibm2mips_();	/* NCV_IBM */
#endif	/* ! __mips */

#ifdef	_CRAY
#if	(ENABLE_CDC_DAT == NO) && !ALL_F
#pragma soft CDC2CRAY
#pragma soft CRAY2CDC
#endif
extern int	CDC2CRAY(), CRAY2CDC();		/* NCV_CDC */
#endif	/* _CRAY */

#ifdef	_CRAY
#if	(ENABLE_VMS_DAT == NO) && !ALL_F
#pragma soft VAX2CRAY
#pragma soft CRAY2VAX
#pragma soft VAD2CRAY@
#pragma soft CRAY2VAD@
#endif
extern int	VAX2CRAY(),  CRAY2VAX();	/* NCV_VMS (vms) */
extern int	VAD2CRAY@(), CRAY2VAD@();	/* NCV_VAD (vms_dp) */
#elif	defined(__mips)
#if	(ENABLE_VMS_DAT == NO) && !ALL_F
#pragma weak mips2vax_
#pragma weak vax2mips_
#endif
extern int	mips2vax_(), vax2mips_();	/* NCV_VMS */
#endif	/* ! __mips */

#ifdef	_CRAY1
#if	(ENABLE_IEG_DAT == NO) && !ALL_F
# ifdef	_CRAYIEEE
#pragma soft IEG2CRAY@
#pragma soft CRAY2IEG@
# else
#pragma soft IEG2CRAY
#pragma soft CRAY2IEG
# endif
#pragma soft IED2CRAY@
#pragma soft CRAY2IED@
#endif
#if	(ENABLE_IEL_DAT == NO) && !ALL_F
#pragma soft CRI2CRY
#pragma soft CRY2CRI
#endif
extern int	IEG2CRAY@(), CRAY2IEG@();	/* NCV_IEG (ieee) */
extern int	IEG2CRAY(),  CRAY2IEG();	/* NCV_IEG (ieee) */
extern int	IED2CRAY@(), CRAY2IED@();	/* NCV_IED (ieee_dp) */
extern int	CRI2CRY(),   CRY2CRI();		/* NCV_IEL (ieee_64) */
#elif	defined(_CRAYMPP)
#if	(ENABLE_IEG_DAT == NO) && !ALL_F
#pragma soft IEG2CRI_77
#pragma soft CRI2IEG_77
#endif
extern int	IEG2CRI_77(), CRI2IEG_77();	/* NCV_IEG */
#elif	defined(__mips)
#if	(ENABLE_IEG_DAT == NO) && !ALL_F
#pragma weak mips2ieg_
#pragma weak ieg2mips_
#endif
extern int	mips2ieg_(), ieg2mips_();	/* NCV_IEG */
#endif	/* ! __mips */

#ifdef	_CRAY
#if	(ENABLE_NVE_DAT == NO) && !ALL_F
#pragma soft NVE2CRAY
#pragma soft CRAY2NVE
#endif
extern int	NVE2CRAY(), CRAY2NVE(); /* NCV_NVE (nosve) */
#endif	/* _CRAY */

#ifdef	_CRAY
#if	(ENABLE_205_DAT == NO) && !ALL_F
#pragma soft ETA2CRAY
#pragma soft CRAY2ETA
#endif
extern int	ETA2CRAY(), CRAY2ETA(); /* C205/ETA (c205/eta) */
#endif	/* _CRAY */

#ifdef	_CRAY
#if	(ENABLE_IEU_DAT == NO) && !ALL_F
#pragma soft IEU2CRAY
#pragma soft CRAY2IEU
#pragma soft IUD2CRAY@
#pragma soft CRAY2IUD@
#endif
extern int	IEU2CRAY(),  CRAY2IEU();	/* NCV_IEU (ultrix) */
extern int	IUD2CRAY@(), CRAY2IUD@();	/* NCV_IEU (ultrix_dp) */
#elif	defined(__mips)
#if	(ENABLE_IEU_DAT == NO) && !ALL_F
#pragma weak mips2ieu$_
#pragma weak ieu2mips$_
#endif
extern int	mips2ieu$_(), ieu2mips$_();	/* NCV_IEU (ieee_le) */
#endif	/* _CRAY */

#ifdef	__mips
#if	(ENABLE_IA_DAT == NO) && !ALL_F
#pragma weak ia2mips_
#pragma weak mips2ia_
#endif
extern int	ia2mips_(), mips2ia_();		/* NCV_IA (Intel) */
#endif

#ifdef _LITTLE_ENDIAN
#if	(ENABLE_MIPS_DAT == NO) && !ALL_F
#pragma weak ia2mips_
#pragma weak mips2ia_
#endif
extern int	ia2mips_(), mips2ia_();		/* NCV_MIPS (mips) */
#endif

#ifdef	__mips
#pragma weak usr2mips_
#pragma weak mips2usr_
extern int	usr2mips_(), mips2usr_(); /* USER conversion (always weak!) */

#if	(ENABLE_SITE_DAT == NO)
#pragma weak ste2mips_
#pragma weak mips2ste_
#endif
extern int	ste2mips_(), mips2ste_(); /* SITE conversion */
#elif defined(_LITTLE_ENDIAN) /* __mips */
#else	/* __mips */
#pragma soft USR2CRAY
#pragma soft CRAY2USR
extern int	USR2CRAY(), CRAY2USR(); /* USER conversion (always weak!) */

#if	(ENABLE_SITE_DAT == NO)
#pragma soft STE2CRAY
#pragma soft CRAY2STE
#endif
extern int	STE2CRAY(), CRAY2STE(); /* SITE conversion */
#endif	/* __mips */

/*
 * Define a dummy numeric data conversion function that just returns
 * an error code.
 */

int
_conv_err()
{
	return(-FELDDCNV);
}

struct c_funs_s __fndc_ncfunc[NCV_MAX] = {
	{ _conv_err,	_conv_err,	0, 0, 0 },	/* NCV_NONE (no conversion) */

#if	defined(_CRAY1) && defined(_CRAYIEEE)	/* cray-ts,ieee */
	{ CRY2CRI,	CRI2CRY,	1, 1, 0 },	/* NCV_CRAY */
	{ IBM2CRI,	CRI2IBM,	1, 1, 0 },	/* NCV_IBM */
	{ _conv_err,	_conv_err,	0, 0, 0 },	/* NCV_CDC */
	{ _conv_err,	_conv_err,	0, 0, 0 },	/* NCV_VMS */
	{ IEG2CRAY@,	CRAY2IEG@,	0, 0, 0 },	/* NCV_IEG */
	{ _conv_err,	_conv_err,	0, 0, 0 },	/* NCV_NVE */
	{ _conv_err,	_conv_err,	0, 0, 0 },	/* NCV_205 */
	{ _conv_err,	_conv_err,	0, 0, 0 },	/* NCV_IEU */
	{ IED2CRAY@,	CRAY2IED@,	0, 0, 0 },	/* NCV_IED */
	{ USR2CRAY,	CRAY2USR,	0, 0, 0 },	/* NCV_USER */
	{ STE2CRAY,	CRAY2STE,	0, 0, 0 },	/* NCV_SITE */
	{ IBD2CRAY@,	CRAY2IBD@,	0, 0, 0 },	/* NCV_IBD */
	{ _conv_err,	_conv_err,	0, 0, 0 },	/* NCV_VAD */
	{ _conv_err,	_conv_err,	0, 0, 0 },	/* NCV_IUD */
	{ _conv_err,	_conv_err,	0, 0, 0 },	/* NCV_T3D */
	{ _conv_err,	_conv_err,	0, 0, 0 },	/* NCV_IEL (not used) */
	{ _conv_err,	_conv_err,	0, 0, 0 },	/* NCV_MIPS */

#elif	defined(_CRAY1) && !defined(_CRAYIEEE)	/* cray-c90, cray-ymp, etc. */
	{ _conv_err,	_conv_err,	0, 0, 0 },	/* NCV_CRAY (not used) */
	{ IBM2CRAY,	CRAY2IBM,	0, 1, 0 },	/* NCV_IBM */
	{ CDC2CRAY,	CRAY2CDC,	0, 1, 0 },	/* NCV_CDC */
	{ VAX2CRAY,	CRAY2VAX,	0, 0, 0 },	/* NCV_VMS */
	{ IEG2CRAY,	CRAY2IEG,	0, 1, 0 },	/* NCV_IEG */
	{ NVE2CRAY,	CRAY2NVE,	0, 1, 0 },	/* NCV_NVE */
	{ ETA2CRAY,	CRAY2ETA,	0, 0, 0 },	/* NCV_205 */
	{ IEU2CRAY,	CRAY2IEU,	0, 0, 0 },	/* NCV_IEU */
	{ IED2CRAY@,	CRAY2IED@,	0, 1, 0 },	/* NCV_IED */
	{ USR2CRAY,	CRAY2USR,	0, 0, 0 },	/* NCV_USER */
	{ STE2CRAY,	CRAY2STE,	0, 0, 0 },	/* NCV_SITE */
	{ IBD2CRAY@,	CRAY2IBD@,	0, 1, 0 },	/* NCV_IBD */
	{ VAD2CRAY@,	CRAY2VAD@,	0, 0, 0 },	/* NCV_VAD */
	{ IUD2CRAY@,	CRAY2IUD@,	0, 0, 0 },	/* NCV_IUD */
	{ CRI2CRAY,	CRAY2CRI,	0, 1, 1 },	/* NCV_T3D */
	{ CRI2CRY,	CRY2CRI,	1, 1, 0 },	/* NCV_IEL */
	{ MIPS2CRY,	CRY2MIPS,	1, 1, 0 },	/* NCV_MIPS */

#elif	defined(_CRAYMPP)			/* cray-t3d, cray-t3e, etc. */
	{ CRAY2CRI,	CRI2CRAY,	0, 1, 0 },	/* NCV_CRAY (CRAY Y-MP) */
	{ _conv_err,	_conv_err,	0, 0, 0 },	/* NCV_IBM */
	{ _conv_err,	_conv_err,	0, 0, 0 },	/* NCV_CDC */
	{ _conv_err,	_conv_err,	0, 0, 0 },	/* NCV_VMS */
	{ IEG2CRI_77,	CRI2IEG_77,	0, 1, 0 },	/* NCV_IEG */
	{ _conv_err,	_conv_err,	0, 0, 0 },	/* NCV_NVE */
	{ _conv_err,	_conv_err,	0, 0, 0 },	/* NCV_205 */
	{ _conv_err,	_conv_err,	0, 0, 0 },	/* NCV_IEU */
	{ _conv_err,	_conv_err,	0, 0, 0 },	/* NCV_IED */
	{ USR2CRAY,	CRAY2USR,	0, 0, 0 },	/* NCV_USER */
	{ STE2CRAY,	CRAY2STE,	0, 0, 0 },	/* NCV_SITE */
	{ _conv_err,	_conv_err,	0, 0, 0 },	/* NCV_IBD */
	{ _conv_err,	_conv_err,	0, 0, 0 },	/* NCV_VAD */
	{ _conv_err,	_conv_err,	0, 0, 0 },	/* NCV_IUD */
	{ _conv_err,	_conv_err,	0, 0, 0 },	/* NCV_T3D (not used) */
	{ _conv_err,	_conv_err,	0, 0, 0 },	/* NCV_IEL */
	{ _conv_err,	_conv_err,	0, 0, 0 },	/* NCV_MIPS */

#elif	__mips					/* All MIPS systems */
	{ cry2mips_,	mips2cry_,	1, 1, 0 },	/* NCV_CRAY */
	{ ibm2mips_,	mips2ibm_,	1, 1, 0 },	/* NCV_IBM */
	{ _conv_err,	_conv_err,	0, 0, 0 },	/* NCV_CDC */
	{ vax2mips_,	mips2vax_,	1, 0, 0 },	/* NCV_VMS */
	{ ieg2mips_,	mips2ieg_,	1, 1, 0 },	/* NCV_IEG */
	{ _conv_err,	_conv_err,	0, 0, 0 },	/* NCV_NVE */
	{ _conv_err,	_conv_err,	0, 0, 0 },	/* NCV_205 */
	{ ieu2mips$_,	mips2ieu$_,	1, 0, 0 },	/* NCV_IEU */
	{ _conv_err,	_conv_err,	0, 0, 0 },	/* NCV_IED */
	{ usr2mips_,	mips2usr_,	0, 0, 0 },	/* NCV_USER */
	{ ste2mips_,	mips2ste_,	0, 0, 0 },	/* NCV_SITE */
	{ _conv_err,	_conv_err,	0, 0, 0 },	/* NCV_IBD */
	{ _conv_err,	_conv_err,	0, 0, 0 },	/* NCV_VAD */
	{ _conv_err,	_conv_err,	0, 0, 0 },	/* NCV_IUD */
	{ _conv_err,	_conv_err,	0, 0, 0 },	/* NCV_T3D */
	{ ieg2mips_,	mips2ieg_,	1, 1, 0 },	/* NCV_IEL */
	{ _conv_err,	_conv_err,	0, 0, 0 },	/* NCV_MIPS (not used)*/

#elif	defined(_LITTLE_ENDIAN)			/* IA systems */
	{ _conv_err,	_conv_err,	0, 0, 0 },	/* NCV_CRAY */
	{ _conv_err,	_conv_err,	0, 0, 0 },	/* NCV_IBM */
	{ _conv_err,	_conv_err,	0, 0, 0 },	/* NCV_CDC */
	{ _conv_err,	_conv_err,	0, 0, 0 },	/* NCV_VMS */
	{ _conv_err,	_conv_err,	0, 0, 0 },	/* NCV_IEG */
	{ _conv_err,	_conv_err,	0, 0, 0 },	/* NCV_NVE */
	{ _conv_err,	_conv_err,	0, 0, 0 },	/* NCV_205 */
	{ _conv_err,	_conv_err,	0, 0, 0 },	/* NCV_IEU */
	{ _conv_err,	_conv_err,	0, 0, 0 },	/* NCV_IED */
	{ _conv_err,	_conv_err,	0, 0, 0 },	/* NCV_USER */
	{ _conv_err,	_conv_err,	0, 0, 0 },	/* NCV_SITE */
	{ _conv_err,	_conv_err,	0, 0, 0 },	/* NCV_IBD */
	{ _conv_err,	_conv_err,	0, 0, 0 },	/* NCV_VAD */
	{ _conv_err,	_conv_err,	0, 0, 0 },	/* NCV_IUD */
	{ _conv_err,	_conv_err,	0, 0, 0 },	/* NCV_T3D */
	{ _conv_err,	_conv_err,	0, 0, 0 },	/* NCV_IEL */
	{ mips2ia_,	ia2mips_,	1, 0, 0 },	/* NCV_MIPS */

#else						/* SPARC, et al */
	{ _conv_err,	_conv_err,	0, 0, 0 },	/* NCV_CRAY */
	{ _conv_err,	_conv_err,	0, 0, 0 },	/* NCV_IBM */
	{ _conv_err,	_conv_err,	0, 0, 0 },	/* NCV_CDC */
	{ _conv_err,	_conv_err,	0, 0, 0 },	/* NCV_VMS */
	{ _conv_err,	_conv_err,	0, 0, 0 },	/* NCV_IEG */
	{ _conv_err,	_conv_err,	0, 0, 0 },	/* NCV_NVE */
	{ _conv_err,	_conv_err,	0, 0, 0 },	/* NCV_205 */
	{ _conv_err,	_conv_err,	0, 0, 0 },	/* NCV_IEU */
	{ _conv_err,	_conv_err,	0, 0, 0 },	/* NCV_IED */
	{ USR2CRAY,	CRAY2USR,	0, 0, 0 },	/* NCV_USER */
	{ STE2CRAY,	CRAY2STE,	0, 0, 0 },	/* NCV_SITE */
	{ _conv_err,	_conv_err,	0, 0, 0 },	/* NCV_IBD */
	{ _conv_err,	_conv_err,	0, 0, 0 },	/* NCV_VAD */
	{ _conv_err,	_conv_err,	0, 0, 0 },	/* NCV_IUD */
	{ _conv_err,	_conv_err,	0, 0, 0 },	/* NCV_T3D */
	{ _conv_err,	_conv_err,	0, 0, 0 },	/* NCV_IEL */
	{ _conv_err,	_conv_err,	0, 0, 0 },	/* NCV_MIPS */
#endif

};
