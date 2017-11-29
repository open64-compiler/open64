/* USMID @(#)10/cmd/tmf/include/tmf/festbls.h	10.0	02/18/98 14:05:12 */


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


#ifndef __FESTBLS_H_
#define __FESTBLS_H_

#include <tmf/fesdefs.h>

/*
 *   Dataset Enquiry Auxiliary information table.
 */
typedef	struct	dex {
	U_LONG	ast	: 8;	/* word  0, bits  0 - 7		AST */
	U_LONG	est	: 8;	/* word  0, bits  8 - 15	EST */
	U_LONG	xxx1	: 16;	/* word  0, bits 16 - 31	not used */
	U_LONG	arg	: 1;	/* word  0, bit  32		ARG */
	U_LONG	conc	: 1;	/* word  0, bit  33		CONC */
	U_LONG	xxx2	: 30;	/* word  0, bits 34 - 63	not used */
	U_LONG	prb	: 32;	/* word  1, bits  0 - 31	PRB */
	U_LONG	apb	: 32;	/* word  1, bits 32 - 63	APB */
} dex_t;

/*
 *   Dataset Update Auxiliary table.
 */
typedef	struct	dux {
	U_LONG	fc	: 8;	/* word  0, bits  0 - 7		FC */
	U_LONG	xxx1	: 1;	/* word  0, bit   8		not used */
	U_LONG	conc	: 1;	/* word  0, bit   9		CONC */
	U_LONG	xxx2	: 6;	/* word  0, bits 10 - 15	not used */
	U_LONG	jes	: 16;	/* word  0, bits 16 - 31	JES */
	U_LONG	st	: 8;	/* word  0, bits 32 - 39	ST */
	U_LONG	des	: 24;	/* word  0, bits 40 - 63	DES */
	char	dvn[8];		/* word  1, bits  0 - 63	DVN */
	char	gdn[8];		/* word  2, bits  0 - 63	GDN */
} dux_t;

/*
 *   Front-end service message header.
 */
typedef	struct	fsh {
	char	jn[7];		/* word  0, bits  0 - 55	JN */
	U_LONG	xxx1	: 8;	/* word  0, bits 56 - 63	not used */
	char	dn[7];		/* word  1, bits  0 - 55	DN */
	U_LONG	xxx2	: 8;	/* word  1, bits 56 - 63	not used */
	U_LONG	jsq	: 16;	/* word  2, bits  0 - 15	JSQ */
	char	omf[2];		/* word  2, bits 16 - 31	OMF */
	U_LONG	ssc	: 16;	/* word  2, bits 32 - 47	SSC */
	U_LONG	mhl	: 16;	/* word  2, bits 48 - 63	MHL */
	char	otid[16];	/* word  3, bits  0 - 63	OTID */
				/* word  4, bits  0 - 63	OTID */
	char	usr[15];	/* word  5, bits  0 - 63	USR */
				/* word  6, bits  0 - 55	USR */
	U_LONG	xxx3	: 8;	/* word  6, bits 56 - 63	not used */
	char	acn[15];	/* word  7, bits  0 - 63	ACN */
				/* word  8, bits  0 - 55	ACN */
	U_LONG	xxx4	: 8;	/* word  8, bits 56 - 63	not used */
	U_LONG	axo	: 16;	/* word  9, bits  0 - 15	AXO */
	U_LONG	lbo	: 16;	/* word  9, bits 16 - 31	LBO or TXO */
	U_LONG	ldo	: 16;	/* word  9, bits 32 - 47	LDO */
	U_LONG	sso	: 16;	/* word  9, bits 48 - 63	SSO */
} fsh_t;

/*
 *   Label group table.
 */
typedef	struct	lbl {
	char	tn[3];		/* word  0, bits  0 - 23	TN */
	U_LONG	xxx1	: 24;	/* word  0, bits 24 - 47	not used */
	U_LONG	tl	: 16;	/* word  0, bits 48 - 63	TL */
	long	xxx2;		/* word  1, bits  0 - 63	not used */
	U_LONG	xxx3	: 16;	/* word  2, bits  0 - 15	not used */
	U_LONG	v1b	: 24;	/* word  2, bits 16 - 39	V1B */
	U_LONG	h1b	: 24;	/* word  2, bits 40 - 63	H1B */
	U_LONG	xxx4	: 16;	/* word  3, bits  0 - 15	not used */
	U_LONG	h2b	: 24;	/* word  3, bits 16 - 39	H2B */
	U_LONG	xxx5	: 24;	/* word  3, bits 40 - 63	not used */
	char	vol1[80];	/* words  4-13			vol1 label */
	char	hdr1[80];	/* words 14-23			hdr1 label */
	char	hdr2[80];	/* words 24-33			hdr2 label */
} lbl_t;

/*
 *   Label Definition table.
 */
typedef	struct	ldt {		/*  label definition table  */
	char	tn[3];		/* word  0, bits  0 - 23	TN */
	U_LONG	xxx1	: 24;	/* word  0, bits 24 - 47	not used */
	U_LONG	tl	: 16;	/* word  0, bits 48 - 63	TL */
        U_LONG	ct	: 4;	/* word  1, bits  0 - 3		CT */
	U_LONG	lt	: 4;	/* word  1, bits  4 - 7		LT */
	U_LONG	prot	: 1;	/* word  1, bit   8		PROT */
	U_LONG	cat	: 1;	/* word  1, bit   9		CAT */
	U_LONG	xxx2	: 18;	/* word  1, bits 10 - 27	not used */
	U_LONG	idc	: 4;	/* word  1, bits 28 - 31	IDC */
	U_LONG	xxx3	: 8;	/* word  1, bits 32 - 39	not used */
	U_LONG	dnt	: 24;	/* word  1, bits 40 - 63	DNT */
        U_LONG	xxx4	: 16;	/* word  2, bits  0 - 15	not used */
	U_LONG	v1b	: 24;	/* word  2, bits 16 - 39	V1B */
	U_LONG	h1b	: 24;	/* word  2, bits 40 - 63	H1B */
        U_LONG	xxx5	: 16;	/* word  3, bits  0 - 15	not used */
	U_LONG	h2b	: 24;	/* word  3, bits 16 - 39	H2B */
	U_LONG	xxx6	: 24;	/* word  3, bits 40 - 63	not used */
} ldt_t;

typedef	struct	ldtvsn {
	char	vsn[6];		/* word  0, bits  0 - 47	VSN */
	U_LONG	vrg	: 1;	/* word  0, bit  48		VRG */
	U_LONG	xxx1	: 3;	/* word  0, bits 49 - 51	not used */
	U_LONG	vdc	: 4;	/* word  0, bits 52 - 55	VDC */
	char	vac;		/* word  0, bits 56 - 63	VAC */
} ldtvsn_t;

typedef	struct	ldtvol1 {
	char	vol1[4];	/* word  0, bits  0 - 31	VOL1 */
	U_LONG	vsb	: 16;	/* word  0, bits 32 - 47	VSB */
	U_LONG	vl1l	: 16;	/* word  0, bits 48 - 63	VL1L */
        U_LONG	vsnl	: 16;	/* word  1, bits  0 - 15	VSNL */
	U_LONG	cvn	: 16;	/* word  1, bits 16 - 31	CVN */
	U_LONG	dt	: 8;	/* word  1, bits 32 - 39	DT */
	U_LONG	xxx1	: 8;	/* word  1, bits 40 - 47	not used */
	U_LONG	fvn	: 16;	/* word  1, bits 48 - 63	FVN */
        char	oid[14];	/* word  2, bits  0 - 63	OID */
        			/* word  3, bits  0 - 47	OID */
	U_LONG	xxx2	: 16;	/* word  3, bits 48 - 63	not used */
        char	gdn[8];		/* word  4, bits  0 - 63	GDN */
	struct	ldtvsn	vsns[LDTVSN_MAXVSN];
} ldtvol1_t;

typedef	struct	ldthdr1 {
	char	hdr1[4];	/* word  0, bits  0 - 31	HDR1 */
	U_LONG	xxx1	: 16;	/* word  0, bits 32 - 47	not used */
	U_LONG	hr1l	: 16;	/* word  0, bits 48 - 63	HR1L */
        char	fid[44];	/* word  1, bits  0 - 63	FID */
        			/* word  2, bits  0 - 63	FID */
        			/* word  3, bits  0 - 63	FID */
        			/* word  4, bits  0 - 63	FID */
        			/* word  5, bits  0 - 63	FID */
        			/* word  6, bits  0 - 31	FID */
	U_LONG	cvsq	: 16;	/* word  6, bits 32 - 47	CVSQ */
	U_LONG	fvsq	: 16;	/* word  6, bits 48 - 63	FVSQ */
        char	fsec[4];	/* word  7, bits  0 - 31	FSEC */
	char	csec[4];	/* word  7, bits 32 - 63	CSEC */
        char	fseq[4];	/* word  8, bits  0 - 31	FSEQ */
	char	dac;		/* word  8, bits 32 - 39	DAC */
	U_LONG	vn	: 8;	/* word  8, bits 40 - 47	VN */
	U_LONG	fsq	: 16;	/* word  8, bits 48 - 63	FSQ */
        char	gen[4];		/* word  9, bits  0 - 31	GEN */
	U_LONG	gn	: 16;	/* word  9, bits 32 - 47	GN */
	char	gvn[2];		/* word  9, bits 48 - 63	GVN */
        char	cdt[6];		/* word 10, bits  0 - 47	CDT */
	U_LONG	xxx2	: 16;	/* word 10, bits 48 - 63	not used */
        char	xdt[6];		/* word 11, bits  0 - 47	XDT */
	U_LONG	uxd	: 1;	/* word 11, bit  48		UXD */
	U_LONG	rt	: 15;	/* word 11, bits 49 - 63	RT */
        char	blk[6];		/* word 12, bits  0 - 47	BLK */
	U_LONG	xxx3	: 16;	/* word 12, bits 48 - 63	not used */
        char	set[6];		/* word 13, bits  0 - 47	SET */
	U_LONG	xxx4	: 16;	/* word 13, bits 48 - 63	not used */
        U_LONG	fbc	: 32;	/* word 14, bits  0 - 31	FBC */
	U_LONG	vbc	: 32;	/* word 14, bits 32 - 63	VBC */
        char	scod[13];	/* word 15, bits  0 - 63	SCOD */
        			/* word 15, bits  0 - 39	SCOD */
	U_LONG	xxx5	: 24;	/* word 16, bits 40 - 63	not used */
} ldthdr1_t;

typedef	struct	ldthdr2 {
	char	hdr2[4];	/* word  0, bits  0 - 31	HDR2 */
	U_LONG	xxx1	: 16;	/* word  0, bits 32 - 47	not used */
	U_LONG	hr2l	: 16;	/* word  0, bits 48 - 63	HR2L */
        char	fmt;		/* word  1, bits  0 - 7		FMT */
	char	ba;		/* word  1, bits  8 - 15	BA */
	U_LONG	xxx2	: 16;	/* word  1, bits 16 - 31	not used */
	U_LONG	mbs	: 32;	/* word  1, bits 32 - 63	MBS */
        U_LONG	bfo	: 16;	/* word  2, bits  0 - 15	BFO */
	U_LONG	xxx3	: 16;	/* word  2, bits 16 - 31	not used */
	U_LONG	mrs	: 32;	/* word  2, bits 32 - 63	MRS */
        char	bl[5];		/* word  3, bits  0 - 39	BL */
	U_LONG	xxx4	: 24;	/* word  3, bits 40 - 63	not used */
        char	rl[5];		/* word  4, bits  0 - 39	RL */
	U_LONG	xxx5	: 24;	/* word  4, bits 40 - 63	not used */
} ldthdr2_t;

/*
 *   (Re)mount Auxiliary Information Table.
 */
typedef	struct	rmx {
	char	dvn[8];		/* word  0, bits  0 - 63	DVN */
	char	gdn[8];		/* word  1, bits  0 - 63	GDN */
	U_LONG	dt	: 8;	/* word  2, bits  0 - 7		DT */
	U_LONG	xxx1	: 8;	/* word  2, bits  8 - 15	not used */
	U_LONG	txl	: 16;	/* word  2, bits 16 - 31	TXL */
	U_LONG	txo	: 16;	/* word  2, bits 32 - 47	TXO */
	U_LONG	vsq	: 16;	/* word  2, bits 48 - 63	VSQ */
	char	vsn[6];		/* word  3, bits  0 - 47	VSN */
	U_LONG	xxx2	: 7;	/* word  3, bits 48 - 54	not used */
	U_LONG	rg	: 1;	/* word  3, bit  55		RG */
	U_LONG	dc	: 4;	/* word  3, bits 56 - 59	DC */
	U_LONG	lt	: 4;	/* word  3, bits 60 - 63	LT */
	U_LONG	rcl	: 8;	/* word  4, bits  0 - 7		RCL */
	U_LONG	xxx3	: 8;	/* word  4, bits  8 - 15	not used */
	U_LONG	rjl	: 16;	/* word  4, bits 16 - 31	RJL */
	U_LONG	rjo	: 16;	/* word  4, bits 32 - 47	RJO */
	U_LONG	xxx4	: 16;	/* word  4, bits 48 - 63	not used */
} rmx_t;

/*
 *   Supplementary logfile message.
 */
typedef	struct	tmsg {
	U_LONG	s	: 1;	/* word  0, bit   0		S */
	U_LONG	u	: 1;	/* word  0, bit   1		U */
	U_LONG	xxx1	: 46;	/* word  0, bits  2 - 47	not used */
	U_LONG	txc	: 16;	/* word  0, bits 48 - 63	TXC */
} tmsg_t;

/*
 *   Volume access auxiliary information table.
 */
typedef	struct	vax {
	U_LONG	ast	: 8;	/* word  0, bits  0 - 7		AST */
	U_LONG	est	: 8;	/* word  0, bits  8 - 15	EST */
	U_LONG	xst	: 8;	/* word  0, bits 16 - 23	XST */
	U_LONG	xxx1	: 8;	/* word  0, bits 24 - 31	not used */
	U_LONG	arg	: 1;	/* word  0, bit  32		ARG */
	U_LONG	xxx2	: 7;	/* word  0, bits 33 - 39	not used */
	U_LONG	co	: 4;	/* word  0, bits 40 - 43	CO */
	U_LONG	dds	: 4;	/* word  0, bits 44 - 47	DDS */
	U_LONG	fat	: 1;	/* word  0, bit  48		FAT */
	U_LONG	xxx3	: 9;	/* word  0, bits 49 - 57	not used */
	U_LONG	nsm	: 1;	/* word  0, bit  58		NSM */
	U_LONG	conc	: 1;	/* word  0, bit  59		CONC */
	U_LONG	bof	: 1;	/* word  0, bit  60		BOF */
	U_LONG	bov	: 1;	/* word  0, bit  61		BOV */
	U_LONG	xxx4	: 2;	/* word  0, bits 62 - 63	not used */
	char	dvn[8];		/* word  1, bits  0 - 63	DVN */
	char	gdn[8];		/* word  2, bits  0 - 63	GDN */
} vax_t;

/*
 *   Volume update auxiliary information table.
 */
typedef	struct	vux {
	U_LONG	bof	: 1;	/* word  0, bit   0		BOF */
	U_LONG	bov	: 1;	/* word  0, bit   1		BOV */
	U_LONG	xxx1	: 2;	/* word  0, bits  2 - 3		not used */
	U_LONG	conc	: 1;	/* word  0, bit   4		CONC */
	U_LONG	nsm	: 1;	/* word  0, bit   5		NSM */
	U_LONG	xxx2	: 2;	/* word  0, bits  6 - 7		not used */
	U_LONG	jes	: 24;	/* word  0, bits  8 - 31	JES */
	U_LONG	st	: 8;	/* word  0, bits 32 - 39	ST */
	U_LONG	des	: 24;	/* word  0, bits 40 - 63	DES */
	char	dvn[8];		/* word  1, bits  0 - 63	DVN */
	char	gdn[8];		/* word  2, bits  0 - 63	GDN */
	U_LONG	lo	: 4;	/* word  3, bits  0 - 3		LO */
	U_LONG	dds	: 4;	/* word  3, bits  4 - 7		DDS */
	U_LONG	alt	: 4;	/* word  3, bits  8 - 11	ALT */
	U_LONG	vst	: 4;	/* word  3, bits 12 - 15	VST */
	U_LONG	evs	: 16;	/* word  3, bits 16 - 31	EVS */
	U_LONG	pde	: 32;	/* word  3, bits 32 - 63	PDE */
} vux_t;


typedef	struct	dal {
	char	dvn[8]	;	/* Device Name		*/
} dal_t;

/*
 *   Volume Inquiry Auxiliary Information Table
 */
typedef	struct	lix {
	U_LONG	fc	: 8;	/* word 0, bits 0 - 7	FC */
	U_LONG	type	: 8;	/* word 0, bits 8 - 15	TYPE */
	U_LONG	lb	: 8;	/* word 0, bits 16 - 23	LB */
	U_LONG	den	: 8;	/* word 0, bits 24 -31	DEN */
	U_LONG	ndvn	: 8;	/* word 0, bits 32 - 39	NUM OF DEVICE NAMES */
	U_LONG	st	: 8;	/* word 0, bits 40 - 47	COMPLETION STATUS */
	U_LONG	lst	: 16;	/* word 0, bits 48 - 63	LDR STATUS	*/
	char	host[8]	;	/* word 1		HOST (CRAY) NAME */
	char	ldr[8]	;	/* word 2		LOADER NAME */
	U_LONG	scr	: 1;	/* word 3 bit 0		SCRATCH REQUEST */
	U_LONG	fill_1	: 7;	/* word 3 bits  1 -  7	unused		*/
	U_LONG	dt	: 8;	/* word 3 bits 8 - 15 	DEVICE TYPE */
	char	vsn[6]	;	/* word 3 bits 16 - 63	VOLUME SERIAL NUMBER */
	char	sp[8]	;	/* word 4		SCRATCH POOL NAME */
	char	gdn[8]	;	/* word 5		DEVICE GROUP NAME */
	U_LONG	domain	:  8;	/* word 6 bits  0 -  7	acs number	*/
	U_LONG	fill_2	: 56;	/* word 6 bits  8 - 63	unused		*/
	char	loc[8]  ;	/* word 7 		ASCII LOCATION */
	struct	dal dl[128];	/* word 8 - 135		DEV AVAILABILITY LIST*/
} lix_t;

/*
 *   Loader Function Auxialiary Information Table
 */
typedef	struct	lfx {	
	U_LONG	fc	: 8;	/* word 0, bits  0 -  7	FC		*/
	U_LONG	type	: 8;	/* word 0, bits  8 - 15	TYPE		*/
	U_LONG	lb	: 8;	/* word 0, bits 16 - 23	LB		*/
	U_LONG	den	: 8;	/* word 0, bits 24 - 31	DEN		*/
	U_LONG	fill_1	: 8;	/* word 0, bits 32 - 39 Unused		*/
	U_LONG	st	: 8;	/* word 0, bits 40 - 47	STATUS */
	U_LONG	err	: 16;	/* word 0, bits 48 - 63	DEVICE ERROR CODE */
	char	host[8]	;	/* word 1		HOST (CRAY) NAME */
	char	ldr[8]	;	/* word 2		LOADER NAME */
	U_LONG	scr	: 1;	/* word 3, bit   0	SCRATCH REQUEST	*/
	U_LONG	ring	: 1;	/* word 3, bit   1	WRITE ENABLE	*/
	U_LONG	unattd	: 1;	/* word 3, bit   2	UNATTTENDED MODE*/
	U_LONG	lfx_itm	: 1;	/* word 3, bit   3	INTERMEDIATE REPLY*/
	U_LONG	fill_2	: 4;	/* word 3, bits  4 -  7	UNUSED		*/
	U_LONG	dt	: 8;	/* word 3, bits  8 - 15	DEVICE TYPE	*/
	char	vsn[6]	;	/* word 3, bits 16 - 63	VOLUME SERIAL NUMBER*/
	char	sp[8]	;	/* word 4		SCRATCH POOL NAME */
	char	dvn[8]	;	/* word 5 		DEVICE NAME */
	char	pdv[8]	;	/* word 6		PREVIOUS DEVICE NAME */
	char	gdn[8]	;	/* word 7		DEVICE GROUP NAME */
} lfx_t;

typedef	struct	lfx_req		{
	int		lf_bytes;
	int		lf_cfc;		/* function code for loader daemon*/
	char		*lf_data;
	struct	fs	*lf_datafsp;
	char		*lf_dsp;
	int		lf_lfc;		/* subfunction code for loader daemon*/
	struct	fs	*lf_repfsp;
	int		lf_size;
	char		*lf_srpp;
	int		lf_try;
	int		lf_wait;
	int		lf_wc;
} lfx_req_t;

#endif /* __FESTBLS_H_ */
