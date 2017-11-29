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


#pragma ident "@(#) libu/util/target.c	92.4	11/29/99 15:41:28"



#include <malloc.h>
#include <stdio.h>
#include <string.h>
#include <sys/param.h>

#ifdef _UNICOS
#include <fortran.h>
#include <sys/target.h>
#endif

#include "targlib.h"

#ifdef _UNICOS
#pragma _CRI duplicate _GETTMC as GETTMC,GETTMC@
#pragma _CRI duplicate _GETPMC as GETPMC,GETPMC@
#pragma _CRI duplicate _GETTMC2 as GETTMC2
#pragma _CRI duplicate _GETPMC2 as GETPMC2
#pragma _CRI duplicate _CHECKMC as CHECKMC,CHECKMC@
#pragma _CRI duplicate _MCNUMPP as MCNUMPP,MCNUMPP@
#pragma _CRI duplicate _MCTOPDT as MCTOPDT,MCTOPDT@

#else
#define _GETTMC GETTMC
#define _GETPMC GETPMC
#define _CHECKMC CHECKMC
#define _MCNUMPP MCNUMPP
#define _MCTOPDT MCTOPDT
#endif

static int getmach(char *pmtname);
static int checkpmt(char *pmtname);
static int getmcindx(char *pmtname);
static void makelower(char *pmtname);
static long _getpmc(struct mctable *, char *);


static int target_gotten = 0; /* non-zero when target_mct is valid */
static int subtype_gotten = 0; /* non-zero when target_mct has valid subtype*/
static int subtype_pmt = 0; /* When target_gotten != 0, this will contain pmt */
static ULL_int target_mct[128]; /* holds copy of target characteristics */


/* should match PM_HOST, PM_TARGET, etc */
static
struct pmttab pmt[] = {
	{"host",    HOSTTYPE},
	{"target",  HOSTTYPE},
	{"iop",     IOPTYPE},
	{"cray-ymp",YMPTYPE},
	{"cray-c90",C90TYPE},
	{"cray-t3d",MPPTYPE},
	{"cray-el", YMPTYPE},
	{"cray-ts", TSTYPE},
	{"cray-t3e",MPPTYPE},
	{"cray-j90",YMPTYPE},
	{"cray-jse",YMPTYPE},
	{"cray-sv1",YMPTYPE},
	{"cray-sv2",YMPTYPE},
#ifndef _UNICOS
	{"sparc",   SPARCTYPE},
#endif
	{"*host",   HOSTTYPE},
	{"*target", HOSTTYPE},
};
/* The order of strings in _cxymchar should match the logical characteristics */
/* section of struct mctable. This is used by the target command, and */
/* by nmake */
char *_cxymchar[] = {"ema", "cigs", "vpop", "pc", "readvl", "vrecur",
		     "avl", "hpm", "bdm", "statrg", "cori", "addr32",
		     "xea", "bmm", "avpop", "", "ieee", NULL };
static
struct mcoffs mcoffs[NUMMCHARS] = {
        /*primary   */  { 1, 0, 64, 0, 0, 0, ALLTYPE, ALLTYPE, "primary"},
        /*banks     */  { -1, 0, 0, 1, 0, 0, C1TYPE , C1TYPE, "banks"},
        /*numcpus   */  { -1, 0, 0, 2, 0, 0, C1TYPE , C1TYPE, "numcpus"},
        /*ibufsize  */  { -1, 0, 0, 3, 0, 0, C1TYPE , C1TYPE, "ibufsize" },
        /*memsize   */  { -1, 0, 0, 4, 0, 0, C1TYPE | MPPTYPE,
			 C1TYPE  | MPPTYPE, "memsize"},
        /*memspeed  */  { -1, 0, 0, 5, 0, 0, C1TYPE , C1TYPE, "memspeed" },
        /*clocktim  */  { -1, 0, 0, 6, 0, 0, C1TYPE , C1TYPE, "clocktim" },
        /*numclstr  */  { 2, 52, 1, 7, 0, 0, C1TYPE , C1TYPE, "numclstr" },
        /*bankbusy  */  { -1, 0, 0, 8, 0, 0, C1TYPE , C1TYPE, "bankbusy" },
        /*subtype   */  { -1, 0, 0, 19, 0, 0, ALLTYPE ,     0, "subtype" },
        /*ema       */  { 2, 62, 1, 64, 1, -1, C1TYPE, C90_XY_TYPE, "ema" },
        /*noema     */  { -1, 0, 0, 64, 1, 0, C1TYPE , C1TYPE, "noema"},
        /*cigs      */  { 2, 61, 1, 65, 1, -1, C1TYPE, C1TYPE, "cigs" },
        /*nocigs    */  { -1, 0, 0, 65, 1, 0, C1TYPE, C1TYPE, "nocigs" },
        /*vpop      */  { 2, 63, 1, 66, 1, -1, C1TYPE, C1TYPE, "vpop"},
        /*novpop    */  { -1, 0, 0, 66, 1, 0, C1TYPE, C90_XY_TYPE, "novpop"},
        /*pc        */  { 2, 60, 1, 67, 1, -1, C1TYPE, C1TYPE, "pc"},
        /*nopc      */  { -1, 0, 0, 67, 1, 0, C1TYPE, C90_XY_TYPE, "nopc"},
        /*readvl    */  { 2, 59, 1, 68, 1, -1, C1TYPE, C1TYPE, "readvl"},
        /*noreadvl  */  { -1, 0, 0, 68, 1, 0, C1TYPE, C90_XY_TYPE, "noreadvl" },
        /*vrecur    */  { 2, 58, 1, 69, 1, -1, C1TYPE, C90_XY_TYPE, "vrecur" },
        /*novrecur  */  { 2, 57, 1, 69, 1, 0, C1TYPE, C1TYPE, "novrecur"},
        /*avl       */  { 2, 56, 1, 70, 1, -1, C1TYPE, C1TYPE, "avl"},
        /*noavl     */  { -1, 0, 0, 70, 1, 0, C1TYPE, C90_XY_TYPE, "noavl"},
        /*hpm       */  { 2, 55, 1, 71, 1, -1, C1TYPE, C1TYPE, "hpm"},
        /*nohpm     */  { -1, 0, 0, 71, 1, 0, C1TYPE, C90_XY_TYPE, "nohpm"},
        /*bdm       */  { 2, 54, 1, 72, 1, -1, C1TYPE, C1TYPE, "bdm"},
        /*nobdm     */  { -1, 0, 0, 72, 1, 0, C1TYPE, C90_XY_TYPE, "nobdm"},
        /*statrg    */  { 2, 53, 1, 73, 1, -1, C1TYPE, C1TYPE, "statrg"},
        /*nostatrg  */  { -1, 0, 0, 73, 1, 0, C1TYPE, C90_XY_TYPE, "nostatrg"},
        /*cori      */  { 2, 51, 1, 74, 1, -1, C1TYPE, C1TYPE, "cori"},
        /*nocori    */  { -1, 0, 0, 74, 1, 0, C1TYPE, C90_XY_TYPE, "nocori"},
        /*addr32    */  { 2, 50, 1, 75, 1, -1, C1TYPE, C90_XY_TYPE, "addr32"},
        /*noaddr32  */  {-1,  0, 0, 75, 1, 0, C1TYPE, C1TYPE, "noaddr32"},
        /*xea       */  {-1,  0, 0, 76, 1, -1, C1TYPE, C90_XY_TYPE, "xea"},
        /*noxea     */  {-1,  0, 0, 76, 1, 0, C1TYPE, C1TYPE, "noxea"},
        /*bmm       */  { 2, 49, 1, 77, 1, -1, C1TYPE, C1TYPE, "bmm"},
        /*nobmm     */  {-1,  0, 0, 77, 1, 0, C1TYPE, C90_XY_TYPE, "nobmm"},
	/*avpop	    */  { 2, 48, 1, 78, 1, -1, TSTYPE | YMPTYPE | C90TYPE,
			TSTYPE | YMPTYPE | C90TYPE, "avpop"},
	/*noavpop   */  {-1,  0, 0, 78, 1, 0, C1TYPE, C1TYPE, "noavpop"},
	/* fullsect - deprecated */ { 2, 45, 1, 79, 1, -1, 0, 0, "fullsect"},
	/* nofullsect - deprecated */ { -1, 0, 0, 79, 1, 0, 0, 0, "nofulsct"},
	/*ieee      */  { 2, 46, 1, 80, 1, -1, TSTYPE | YMPTYPE | C90TYPE, 
			TSTYPE, "ieee"},
	/*noieee    */  {-1,  0, 0, 80, 1, 0, C1TYPE, C1TYPE, "noieee" },
};



/*
 * Checks machine characteristic name, returning pointers to
 * machine characteristics table and PDT
 * Input:	
 *	mcname - The name of a machine characteristic
 *	pmtname - The ASCII name of primary machine type (left-justified,
 *		  zero filled). This should be the name of a general
 *		  machine type.
 * Output:
 *	pdtword - The word in the PDT that contains the field for the
 *		  requested machine characteristic. Word 0 is assumed
 *		  to be the first word of the machine-type checking
 *		  portion of the PDT.
 *	pdtstrt - The starting bit position of the field in the PDT
 *		  that contains the requested machine characteristic.
 *		  (The leftmost or most significant bit of the word is
 *		  assumed to be 0).
 *	pdtlen  - The length in bits of the field in the PDT that contains
 *		  the requested machine characteristic.
 *	mctindx - The offset to the word in the machine characteristics
 *		  table that contains the requested machine characteristic.
 *		  The first word of this table has an offset value of 0.
 *	mctype  - The type of the requested machine characteristic. This
 *		  value is 0 if the name of a numeric characteristic
 *		  was supplied; the value is 1 if a logical machine 
 *		  characteristic name was supplied.
 *	mcdef   - This argument is significant only for logical characteristics.
 *		  The value is set to the default value for a logical char.
 *	flag	- This is an optional parameter. If passed, and equal to 1,
 *		  return 0 if this characteristic may not be set.
 *		  This is present because the compiler (which doesn't use this
 *		  parameter currently) wants to call CHECKMC with a 
 *		  characteristic that really cannot exist on a machine, and
 *		  have it return the index of that characteristic (e.g.,
 *		  EMA on CRAY_TS). But, we want to disallow people from using
 *		  that as an actual target.
 * Return value:
 *	If the characteristic name was found, a value of -1 is returned.
 *	If the characteristic name was not found, a value of 0 is returned.
 */
long _CHECKMC(mcname,pdtwrd,pdtstrt,pdtlen,mcindx,mctype,mcdef,pmtname,flag)

char *mcname;
LL_int *pdtwrd;
LL_int *pdtstrt;
LL_int *pdtlen;
LL_int *mcindx;
LL_int *mctype;
LL_int *mcdef;
char *pmtname;
LL_int *flag;
{
	register int i;
	int mach;

	*pdtwrd = *pdtstrt = *pdtlen = *mcindx = -1;
	*mctype = *mcdef = 0;
	mach = getmach(pmtname);
	if (mach == -1)	/* if invalid machine name */
		return(0);
	if((i = getmcindx(mcname)) == -1)
		return(0);
	if ((pmt[mach].mtype & mcoffs[i].mach) == 0)
		return(0);	/* not a valid char. for this machine */
#ifdef _UNICOS
	if (_numargs() > 8 && *flag == 1) {
		if ((pmt[mach].mtype & mcoffs[i].canset) == 0)
			return(0);	/* not a valid char. for this machine */
	}
#endif
	*pdtwrd = (LL_int)mcoffs[i].wrd;
	*pdtstrt = (LL_int)mcoffs[i].strt;
	*pdtlen = (LL_int)mcoffs[i].len;
	*mcindx = (LL_int)mcoffs[i].indx;
	*mctype = (LL_int)mcoffs[i].type;
	*mcdef = (LL_int)mcoffs[i].def;
	return(-1);
}

/*
 *
 * getmach checks the primary machine type. If it is "host" or
 * "target", GETHMC or GETTMC are called to process.
 * Input: 
 *	pmtname - primary machine type name
 * Returns:
 *	an index into the pmt structure which can be used by 
 *	the calling routine.
 */
static int
getmach(pmtname)
char *pmtname;
{
long mach;
ULL_int machnam[2];
struct mctable pmctable;
	mach = checkpmt((char *)pmtname);
	if ((mach != PM_HOST) && (mach != PM_TARGET))
		return(mach);
	if (mach == PM_HOST)
#ifdef _UNICOS
		_gethmc(&pmctable);
#else /* on Sun, get target info even for PM_HOST */
		_gettmc(&pmctable);
#endif
	else if (mach == PM_TARGET)
		_gettmc(&pmctable);

	machnam[1] = 0;
	machnam[0] = pmctable.mcpmt;
	mach = checkpmt((char *)machnam);/* get host machine type */
	return(mach);
}

/*
 *	checkpmt checks the primary machine type
 * Input: 
 *	pmtname - primary machine type name
 * Returns:
 *	an index into the pmt structure which can be used by 
 *	the calling routine.
 */
static
checkpmt(pmtname)
char *pmtname;  
{
	register int i;
	char cptr[8];
	(void)strncpy(cptr,pmtname,8);
	if (cptr[7] != '\0' && pmtname[8] != '\0')
	{
		/*  A machine name longer than 8 characters was passed in  */
		return(-1);
	}
	makelower(cptr);
	for (i=0; i<PMTYPES+2; i++){
		if ((strncmp(cptr,pmt[i].name,8)) == 0){
			if (i == PM_STARHOST || i == PM_STARTARGET)
				return(i - PM_STARHOST);
			else
				return(i);
		}
	}
	return(-1);
}
/*
 * Given a machine characteristics name, finds the corresponding
 * index into the machine characteristics table (mcoffs[]). 
 * Returns -1 if characteristic not found.
 */
static
getmcindx(mcname)
char *mcname;
{
	register int i;
	char cptr[8];
	(void)strncpy(cptr,mcname,8);
	makelower(cptr);
	for (i=0; i<NUMMCHARS; i++)
		if ((strncmp(cptr,mcoffs[i].mcname,8)) == 0){
			return(i);
		}
	return(-1);
}
static void
makelower(name)
char *name;
{
	int i = 8;
	while (i != 0 && *name != '\0'){
		if (*name >= 'A' && *name <= 'Z')
			*name += 'a' - 'A';
		name++;
		i--;
	}
}
/* Order should match pmt[] */
static
struct mctable default_tables[PMTYPES] = {

/*      { mcpmt,mcbank,mcncpu,mcibsz,mcmsz,mcmspd,mcclk,mcncl,mcbbsy          */
/*			    subtype, numeric_unused - 53      		      */
/*   For CX/CEA machines:						      */
/*   mcema,mccigs,mcvpop,mcpc,mcrdvl,mcvrcr,mcavl,mchpm,mcbdm,mcstr,mccori    */
/*   mcaddr32,mcyxp,mcbmm,mcavpop,mcfullsect,mcieee,mccmrreq,mccache          */
/*			   logical_unused - 45 },                             */
/*   Note: mcfullsect, mccmrreq, mccache are deprecated, but remain so that */
/*   we do not reuse those bits */

/*   We leave the subtype field blank in the defaults table. It is filled in */
/*   by routine _setsubtype() 							*/
/*unused*/{HOST_NAME,    0,  0,  0,  0,  0,   0,  0,  0, 	
	 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
	 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
	 F, F, F, F, F, F, F, F, F, F, F, F, F, F, F, F, F, F, F, 
	 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
	 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},
/*unused*/{TARGET_NAME,  0,  0,  0,  0,  0,   0,  0,  0, 	
	 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
	 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
	 F, F, F, F, F, F, F, F, F, F, F, F, F, F, F, F, F, F, F,
	 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
	 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0 },
/*iop*/	{IOP_NAME,       0,  0,  0,  0,  0,   0,  0,  0, 	
	 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
	 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
	 F, F, F, F, F, F, F, F, F, F, F, F, F, F, F, F, F, F, F,
	 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
	 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0 },
/*ymp*/	{CRAY_YMP_NAME,256,  8,  32, MC32MEG,  17,   6000,  9,  5, 	
	 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
	 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
	 F, T, T, T, T, F, T, T, T, T, T, T, F, F, F, F, F, F, F,
	 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
	 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0 },

/*c90*/	{CRAY_C90_NAME,1024,  16,  32, MC64MEG,  23,   4000,  18,  6, 	
	 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
	 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
	 F, T, T, T, T, F, T, T, T, T, T, T, F, F, F, F, F, F, F,
	 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
	 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0 },

/*t3d*/ {CRAY_T3D_NAME,    0,  0,  0,  MC8MEG,  0,   0,  0,  0, 	
	 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
	 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
	 F, F, F, F, F, F, F, F, F, F, F, F, F, F, F, F, F, F, F,
	 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
	 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0 },

/*el*/	{CRAY_YMP_NAME,64,  4,  32, MC64MEG,  16,  30000,  5,  5, 	
	 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
	 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
	 F, T, T, T, T, F, T, F, T, T, T, T, F, F, F, F, F, F, F,
	 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
	 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0 },

/*ts*/  {CRAY_TS_NAME,512,  32,  32, MC1024MEG,  52,  2000,  33,  2, 	
	 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
	 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
	 F, T, T, T, T, F, T, T, T, T, T, F, F, T, T, F, F, F, F,
	 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
	 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0 },

/*t3e*/ {CRAY_T3E_NAME,    0,  0,  0,  MC16MEG,  0,   0,  0,  0, 	
	 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
	 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
	 F, F, F, F, F, F, F, F, F, F, F, F, F, F, F, F, F, F, F,
	 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
	 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0 },

/*j90*/	{CRAY_YMP_NAME, 1024,  32,  32, MC32MEG,  33,   10000,  33,  14, 	
	 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
	 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
	 F, T, T, T, T, F, T, T, T, T, T, T, F, F, F, F, F, F, F,
	 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
	 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0 },

/*jse*/ {CRAY_YMP_NAME, 1024,  32,  32, MC32MEG,  33,   10000,  33,  14, 	
	 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
	 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
	 F, T, T, T, T, F, T, T, T, T, T, T, F, F, F, F, F, F, F,
	 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
	 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0 },

/*sv1*/ {CRAY_SV1_NAME, 1024,  32,  32, MC32MEG,  33,   10000,  33,  14, 
	 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
	 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
	 F, T, T, T, T, F, T, T, T, T, T, T, F, T, F, F, F, F, F,
	 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
	 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0 },

/* These are meaningless placeholder values for sv2 */
/*sv2*/ {CRAY_SV2_NAME, 1024,  32,  32, MC32MEG,  33,   10000,  33,  14, 
	 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
	 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
	 F, T, T, T, T, F, T, T, T, T, T, T, F, T, F, F, F, F, F,
	 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
	 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0 },

#ifndef _UNICOS
/*sparc*/ {SPARC_NAME,    0,  0,  0,  0,  0,   0,  0,  0, 	
	 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
	 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
	 F, F, F, F, F, F, F, F, F, F, F, F, F, F, F, F, F, F, F,
	 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
	 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0 },
#endif
};

/*
 * _GETPMC returns primary machine characteristics.
 * Input:
 *	pmtname - the name of a primary machine type (e.g., CRAY-C90)
 * Output:
 *	pmctable - a 128 word table filled with the
 *	characteristics of the specified primary machine type.
 *	Each word is 64 bits.
 * Returns:
 *	-1 if the machine name was found.
 *	0 if the machine name was not found (invalid name)
 */
long _GETPMC(pmctable, pmtname)
struct mctable *pmctable;
char *pmtname;
{
	register int i;
	i = _getpmc(pmctable,pmtname);
	if (i == -1)	/* if pmtname was invalid, return error */
		return(0);
	_setsubtype(i, pmctable); /* set subtype in pmctable */
	return(-1);
}

/* _getpmc returns -1 on error, otherwise pmt index */
static
long _getpmc(pmctable, pmtname)
struct mctable *pmctable;
char *pmtname;
{
	register int i;
	i = checkpmt(pmtname);
	switch(i) {
		case -1:	/* if pmtname was invalid, return error */
			return(-1);
		case PM_TARGET:
			i = _gettmc(pmctable);
			break;
		case PM_HOST:
#ifdef _UNICOS
			if (_gethmc(pmctable) < 0)
				return(0);
#else
			/* for Sun, make host same as target */
			i = _gettmc(pmctable);
#endif
			break;
		default :
			(void) memcpy((void *)pmctable,(void *)&default_tables[i],1024);
	} /* end switch */
	return(i);
}

#define MCFATAL(msg,gettmc){		\
	if (gettmc){			\
		fprintf(stderr,"GETTMC: %s on TARGET statement\n",msg);	\
		exit(1);		\
	}				\
	return(msg);			\
}

#define MCWARN(msg, gettmc, mc) {	\
	msgbuff = malloc(80);		\
	if (msgbuff == NULL) {		\
		if (gettmc)		\
			fprintf(stderr,"cannot allocate space :%s",msg);\
		else			\
			return("cannot allocate space"); \
	}				\
	sprintf(msgbuff,msg,mc);	\
	if (gettmc) {			\
		fprintf(stderr,"GETTMC: %s\n",msgbuff);	\
		free(msgbuff);		\
	} else {			\
		return(msgbuff);	\
	}				\
}

/* "crackmc" is called by GETTMC to process the TARGET environment variable
 * with a non-zero value of "gettmc".  All others should call it with a
 * zero "gettmc" value.
 * Input:
 *	cpu:	pointer to machine characteristic specification
 *	gettmc:	error processing indicator. If gettmc =0, then
 *		crackmc will return a pointer to a string describing
 *		the error, if one occurs. Otherwise,
 *		crackmc will print a message and exit() if an error
 *		occurs. 
 *	targtbl:pointer to 128-word machine characteristics table
 * Output:
 * Returns:
 *	NULL if successful
 *      pointer to a string describing the error if one occurred and
 *		if it was called with zero "gettmc" value
 */
static char *crackmc(cpu,targtbl,gettmc,ipm)
char *cpu;			/* ptr to mach char specification */
ULL_int *targtbl;			/* ptr to mach char table */
int gettmc;			/* error processing indicator: */
				/*   0 = return a string describing error */
				/*   1 = print a message */
int *ipm;			/* returns machine type */
{
	long c;
	int i = 0;
	int equals = 0;
	int nextmc = 1;
	char *cpuname, *localcpu, *localptr;
	char *mcname, *mcstring;
	char *msgbuff;
	LL_int mcindx, mctype, mcdef;
	LL_int mcval = 0;
	long one = 1;

	if ((localcpu = malloc(strlen(cpu)+1))==NULL) {
		if (gettmc){
			fprintf(stderr,"GETTMC: cannot allocate space\n");
			exit(1);
		} 
		else 
			return("cannot allocate space");
	}
	localptr = localcpu;
	(void)strcpy(localcpu,cpu);
	if (*localcpu == ',') {
		cpuname = "host";
		*localcpu = '\0';
		localcpu++;
	} else {
		cpuname = (char *)localcpu;  /* pick off cpuname */
		while ((*localcpu != ',')&&(*localcpu != '\0'))
			localcpu++;
		if (*localcpu == ',') {
			*localcpu = '\0';
			localcpu++;
		}
	}
	if (*localcpu == '\0') nextmc = 0;
	i = checkpmt(cpuname);
	*ipm = i;
#ifdef _UNICOS
	if (i == PM_TARGET)
		cpuname = "host";
#else
	if (i==PM_HOST || i==PM_TARGET) {
		cpuname = "sparc";
		*ipm = PM_SPARC;
	}
#endif
	if (_getpmc((struct mctable *)targtbl,cpuname) == -1) {
		MCFATAL("invalid primary machine name",gettmc);
	}
	while (nextmc) {	/* pick off machine characteristics which may
			     have been specified on command line */
		mcname = (char *)localcpu;
		equals = 0;
		while ((*localcpu != ',')&&(*localcpu != '\0') && (*localcpu != '='))
			localcpu++;
		if (*localcpu == ',') {
			*localcpu = '\0';
			localcpu++;
		} else if (*localcpu == '=') {
			equals = 1;
			*localcpu = '\0';
			localcpu++;
		} else
			nextmc = 0;
		/* check machine characteristic for validity */
		if (_CHECKMC(mcname,&c,&c,&c,&mcindx,&mctype,&mcdef,cpuname,&one)==0) {
			MCWARN("%s is an invalid machine characteristic",gettmc,mcname);
		}
		else {
		   if (mctype == 0) { /* if numeric characteristic */
			if (equals) {  /* check for equals sign */
			   /* pick off value */
			   mcstring = (char *)localcpu;
         		   while ((*localcpu != ',') && (*localcpu != '\0'))
	         		localcpu++;
         	 	   if (*localcpu == '\0') nextmc = 0;
         	 		*localcpu++ = '\0';
			   /* check value for validity */
	         	   if (_MCNUMPP(mcname,mcstring,&mcval)==0) {
				MCWARN("value specified for %s is invalid",
					gettmc, mcname);
			   }
		 	   else {
		 		targtbl[mcindx] = mcval;
		 	   }
	     		} else {
				MCWARN("value must be given for %s",gettmc,mcname);
			}
	   	   } else {	/* logical characteristic */
			if (equals) {
				MCWARN("logical characteristic %s cannot be given a numeric value",
					gettmc,mcname);
			}
			else {
	 			targtbl[mcindx] = mcdef;
			}
		   } /* if (mctype == 0) */
		} /* if (0 == CHECKMC .. */
	} /* while (nextmc)*/
	free(localptr);
	return (NULL);
}

#ifdef _UNICOS
/* CRACKMC is a Fortran interface to crackmc.  It copies the output status
 * string from crackmc to a Fortran array.  It returns zero if all is well,
 * non-zero if errors were encountered.
 */
_f_int
CRACKMC (mcstr, targtbl, statbuf, sblen)
_fcd mcstr;			/* machine characteristics string to process */
long *targtbl;			/* 128 targeting information table */
long statbuf;			/* buffer to return error string (may be _fcd)*/
long *sblen;			/* size of statbuf in words */
				/* This parameter is required. It is unused */
				/* if statbuf is an _fcd */
{
	char	*cp, *mcbuf, *retnstr;
	int	i, slen, status;
	long	*iptr;
	int 	ipm;

	if (_numargs() < 3)
		return( (_f_int) 1);

#ifndef	_ADDR64			/* assume _fcd on _ADDR64 systems */
	if (!_isfcd(mcstr)) {
		mcbuf	= (char *) mcstr;
	}
	else
#endif
	{
		if ((mcbuf = _f2ccpy(mcstr)) == NULL)
			return( (_f_int) 1);
	}

	if ((retnstr = crackmc(mcbuf, targtbl, 0, &ipm)) == NULL)
		status	= 0;
	else {

#ifdef	_ADDR64
		/* Detect an _fcd by the number of words in the argument list */
		if (_numargs() * sizeof(long) ==
		    sizeof(_fcd) + 2*sizeof(long*) + sizeof(_fcd) ) {
#else
		if (_isfcd(statbuf)) {
#endif
			int	rlen;
			_fcd	fcdstat;

			rlen	= strlen(retnstr);
			fcdstat	= *(_fcd *) &statbuf;
			cp	= _fcdtocp(fcdstat);
			slen	= _fcdlen (fcdstat);

			(void) strncpy(cp, retnstr, slen);

			if (rlen < slen)
				(void) memset((void *)(cp + rlen), BLANK, slen - rlen);
		}
		else {
			if (_numargs() * sizeof(long) <
			    sizeof(_fcd) + sizeof(long*) + sizeof(long) +
			    sizeof(long*))
				return( (_f_int) 1);

			cp	= (char *) statbuf;
			iptr	= (long *) statbuf;
			slen	= *sblen * sizeof(long);
			iptr[*sblen-1] = 0L;
			(void) strncpy(cp, retnstr, slen - 1);
		}
		status	= 1;
	}

#ifndef _ADDR64                 /* assume _fcd on _ADDR64 systems */
	if (_isfcd(mcstr))
#endif
		free(mcbuf);

	_setsubtype(ipm, (struct mctable *)targtbl);
	return (status);
}
#endif /* _UNICOS */

/*
 * GETTMC returns target machine characteristics in the 128 word array
 * 	  mctabinit. Each word is 64 bits.
 * If the TARGET environment variable is set, it defines the target.
 * Otherwise, on a Cray the host machine is the target.
 * On the Sun, the target defaults to SPARC.
 */
void
_GETTMC(mctabint)

ULL_int mctabint[128];  /* view the mctable as a 128 word array so that we can
			change certain values in the mctable structure but
			using an index instead of a structure members name */
{
	int i = 0;
	char *cpu;
	extern char *getenv();
	int ipm;

	if (target_gotten == 0) {
		_gettmc(mctabint);
	}
	if (subtype_gotten == 0) {
		_setsubtype(subtype_pmt,(struct mctable *)target_mct); /* set subtype in target_mct */
		subtype_gotten = 1;
	}
#pragma ivdep
		for ( i = 0 ; i < 128 ; i++ )
			mctabint[i] = target_mct[i];
}
_gettmc(mctabint)

ULL_int mctabint[128];  /* view the mctable as a 128 word array so that we can
			change certain values in the mctable structure but
			using an index instead of a structure members name */
{
	int i = 0;
	char *cpu;
	extern char *getenv();
	struct mctable *targptr;

	if (target_gotten == 0) {
       		cpu = getenv("TARGET");/* get pointer to target value in current
					environment */
#ifdef _UNICOS
		if (cpu) {
			(void) crackmc (cpu, target_mct, 1, &subtype_pmt);
		}else{
			if (target(MC_GET_TARGET,(struct target *)target_mct) < 0) {
				fprintf(stderr,"target system call failed\n");
				(void)exit(1);
			}
			subtype_pmt = PM_HOST;
		}
		/* Considerations for SV1 as a primary type:
		 * If $TARGET set:
		 *	 crackmc() above returns type CRAY-SV1, subtype 0;
		 * else
		 *	target() returns subtype CRAY-SV1;
		 *	U10.0.0.6 target() returns type CRAY-YMP.
		 *	U10.0.0.7 target() returns type CRAY-SV1.
		 * For migration, reset type to CRAY-YMP if type CRAY-SV1
		 * and subtype 0 or CRAY-SV1.
		 */
		targptr = (struct mctable *)target_mct;
		if (targptr->mcpmt == CRAY_SV1_NAME &&
			( targptr->mcsubt[0] == CRAY_SV1_SUBTYPE || targptr->mcsubt[0] == 0) &&
			targptr->mcsubt[1] == 0) {
				targptr->mcpmt = CRAY_YMP_NAME;
		}
#else
		if (!cpu) {
			cpu = "SPARC";
		}
		(void) crackmc (cpu, target_mct, 1, &subtype_pmt);
#endif
	}
#pragma ivdep
	for ( i = 0 ; i < 128 ; i++ )
		mctabint[i] = target_mct[i];
	target_gotten = 1;
	return(subtype_pmt);
}
static
char *numeric_char[] = {
	"banks",
	"numcpus",
	"ibufsize",
	"memsize",
	"memspeed",
	"clocktim",
	"numclstr",
	"bankbusy",
};
#define NUMNUMCH sizeof(numeric_char)/sizeof (char *)
/* MEMNUM = index into numeric_char[] for memsize */
#define MEMNUM   3	
/*
 * MCNUMPP parses numeric machine characteristic parameters
 * Input:
 *		mcname -   the name of a machine characteristic
 *		mcstring - the string representing the value to
 *			   be assigned to the machine characteristic
 * Output:
 *		mcval -    the binary value corresponding to mcstring if
 *			   the conversion was successful
 * Returns:
 *		-1 if successful, otherwise 0
 */
_MCNUMPP(mcname,mcstring,mcval)
char *mcname;
char *mcstring;
LL_int *mcval;
{
	register int i;
	long c;


	/*  Set default value to 0  */
	*mcval = 0;

	makelower(mcname);
	for (i=0; i < NUMNUMCH; i++)
	{
		/*  Search the numeric table for the corresponding characteristic */
		if ((strcmp(mcname,numeric_char[i])) == 0)
		{
			break;
		}
	}
	if (i == NUMNUMCH)
	{
		/*  Invalid machine characteristic  */
		*mcval = -1;
		return(0);
	}

	/*  Search the mcstring to get the numeric value  */
	c = *mcstring++;
	while (c != '\0')
	{
		if ((c >= '0') && (c <= '9'))
		{
			/*  Increase mcval  */
			*mcval = (*mcval * 10) + (c - '0');
		}
		else if (i == MEMNUM)
		{
			/*  Check if c is a "K" or a "k"  */
			if ((c == 'K') || (c == 'k'))
			{
				/*  User specified ##K words  */
				*mcval = (*mcval * 1024);
			}
			else if ((c == 'M') || (c == 'm'))
			{
				/*  User specified ##M words  */
				*mcval = (*mcval * 1048576);
			}
			else
			{
				/*  Invalid character in string  */
				*mcval = -1;
				return(0);
			}
			return(-1);
		}
		else
		{
			/*  Invalid character in string  */
			*mcval = -1;
			return(0);
		}
		c = *mcstring++;
	}
	return(-1);
}

/*
 * MCTOPDT converts 128-word machine characteristics table to
 *	   the format that is stored in PDT.
 * Input:  mctable -  128-word machine characteristics table.
 *		      Same format as the table received from GETHMC,
 *		      GETTMC, and GETPMC.
 *	   pdttable - The starting address of the first word of the
 *		      machine type checking portion of the pdt.
 * Output: pdttable - fields in the pdttable are filled in from
 *		      the data supplied in mctable. The PDT fields
 *		      do not correspond completely to the fields in
 *		      machine characteristics table; in general, the
 *		      numeric characteristics in the machine characteristics
 *		      table are not duplicated in the PDT.
 *		      The machine type field is set in the pdt, and
 *		      the length field is set.
 *		
 */
void _MCTOPDT(mctable,pdttable)

struct mctable *mctable;
struct pdttable *pdttable;
{
	register int mach;
	LL_int name[2];

	name[0] = mctable->mcpmt;
	name[1]=0;		/* make sure name is null terminated */
				/* in case it is exactly 8 chars */
	mach = getmach((char*)name);/* check for valid primary machine type */
	if (mach == -1) {
		return;
	}
	pdttable->length = 3; /* hard code -- currently 3 words for machine
				 type checking                              */
	pdttable->machine = mctable->mcpmt; /* set machine type in pdt */
	/* If the machine type requires bits in the pdt, set it up */
	if ((pmt[mach].mtype & C1TYPE ) != 0) {
		pdttable->word2.pdtxy.cori = mctable->mctlog.mctblxy.mccori;
		if (mctable->mcncl)
			pdttable->word2.pdtxy.clsreq = 1;
		else
			pdttable->word2.pdtxy.clsreq = 0;
		pdttable->word2.pdtxy.str = mctable->mctlog.mctblxy.mcstr;
		if (mctable->mctlog.mctblxy.mcbdm)
			pdttable->word2.pdtxy.bdm = 0;
		else
			pdttable->word2.pdtxy.bdm = 1;
		pdttable->word2.pdtxy.hpm = mctable->mctlog.mctblxy.mchpm;
		pdttable->word2.pdtxy.avl = mctable->mctlog.mctblxy.mcavl;
		pdttable->word2.pdtxy.vrcr = mctable->mctlog.mctblxy.mcvrcr;
		if (mctable->mctlog.mctblxy.mcvrcr)
			pdttable->word2.pdtxy.nvrcr = 0;
		else
			pdttable->word2.pdtxy.nvrcr = 1;
		pdttable->word2.pdtxy.rdvl = mctable->mctlog.mctblxy.mcrdvl;
		pdttable->word2.pdtxy.pc = mctable->mctlog.mctblxy.mcpc;
		pdttable->word2.pdtxy.cigs = mctable->mctlog.mctblxy.mccigs;
		pdttable->word2.pdtxy.ema = mctable->mctlog.mctblxy.mcema;
		pdttable->word2.pdtxy.vpop = mctable->mctlog.mctblxy.mcvpop;
		pdttable->word2.pdtxy.addr32 = mctable->mctlog.mctblxy.mcaddr32;
		pdttable->word2.pdtxy.bmm = mctable->mctlog.mctblxy.mcbmm;
		pdttable->word2.pdtxy.avpop = mctable->mctlog.mctblxy.mcavpop;
		pdttable->word2.pdtxy.fullsect = mctable->mctlog.mctblxy.mcfullsect;
		pdttable->word2.pdtxy.ieee = mctable->mctlog.mctblxy.mcieee;
		pdttable->word2.pdtxy.cmrreq = mctable->mctlog.mctblxy.mccmrreq;
		pdttable->word2.pdtxy.cache = mctable->mctlog.mctblxy.mccache;
	}
}

/*
 * VALTMC(2) validates machine characteristics from PDT
 * Input: 	
 *		curconfg-the current machine characteristics
 *			 required, based on PDTs previously
 *			 analyzed. This table is updated from
 *			 newconfg if the characteristics contained
 *			 in the 2 tables are not conflicting but
 *			 the characteristics specified by newconfg
 *			 are more restrictive than those currently
 *			 specified by curconfg.
 *
 *		newconfg-The new machine characteristics required.
 *
 *		sv1_is_type- (VALTMC2 only) 1 if SV1 is a primary
 *		         machine type; 0 if SV1 is a YMP subtype.
 * Output :
 *		curconfg-See above.
 *		mcname - the name of a machine characteristic
 *			 that conflicts between the current and
 *			 new configurations.
 *		pdtword -the word in the PDT that contains the
 *			 field for conflicting machine characteristics.
 *			 Word 0 is assumed to be the first word of 
 *			 the machine-type checking portion of the PDT.
 *		pdtstrt -the starting bit position of the field in the 
 *			 PDT that contains the conflicting characteristic
 *		pdtlen  -the length in bits of the field in the PDT that
 *			 contains the conflicting machine characteristics
 * Returns:	-1 if the characteristics are not in conflict
 *		 0 if the characteristics are in conflict
 * 
 * Because more than one machine requirement may be in conflict, the
 * characteristic in the newconfg table should be set to the characteristic
 * in the curconfg table, and the routine should be called again until no
 * more conflicting requirements are found.
 *
 * VALTMC recognizes SV1 as a subtype of primary machine type YMP.
 * VALTMC2 lets YMP and SV1 primary machine types mix.  Result is SV1.
 */
long
VALTMC2(curconfg,newconfg,mcname,pdtword,pdtstrt,pdtlen,sv1_is_type)
struct pdttable *curconfg;
struct pdttable *newconfg;
LL_int *mcname;
LL_int *pdtword;
LL_int *pdtstrt;
LL_int *pdtlen;
LL_int sv1_is_type;
{

#define PMTNONE 0

	struct mctable mctable;
	int curmcode = PMTNONE;
	int newmcode = PMTNONE;
	static int firstime = 0;
	ULL_int newmname;
	LL_int *ptr;
	LL_int machn[2];
	int error;
	LL_int curmask, newmask;

	/* get the primary machine type name for the current pdt and
	 * convert it to a numeric code for internal use
	 */
	machn[1]=0;		/* make sure name is null terminated */
				/* in case it is exactly 8 chars */
	if (curconfg->machine == 0)
		firstime = 0;
	else {
		machn[0] = curconfg->machine;
		if ( _getpmc(&mctable,(char *)machn) == -1){
			error = PRIMARY;
			goto mcerror;
		}
		if (sv1_is_type){	/* update primary type if SV1 */
			__setsv1type(&mctable);
		}
		machn[0] = mctable.mcpmt;
		curmcode = checkpmt((char *)machn);
		if (curmcode == -1){
			error = PRIMARY;
			goto mcerror;
		}
	}
	/* get the primary machine type name for the new pdt and
	 * convert it to a numeric code for internal use
	 */
	if (newconfg->length > 3) {
		/* make adustment for flawed CAL-2 formats*/
		LL_int cray_name;
		ptr = (LL_int *) newconfg;
		/*if( *ptr > ('A'<<56) ){*/
		cray_name = CRAY_NAME;
		if( *ptr > cray_name ){
			newconfg = (struct pdttable*)( (LL_int)newconfg -1);
		}
	}
	if (newconfg->machine != 0){
		machn[0]  = newconfg->machine;
		if ( _getpmc(&mctable,(char *)machn) == -1){
			error = PRIMARY;
			goto mcerror;
		}
		if (sv1_is_type){	/* update SV1 primary type */
			__setsv1type(&mctable);
		}
		machn[0]  = mctable.mcpmt;
		newmcode = checkpmt((char *)machn);
		if (newmcode == -1) {
			error = PRIMARY;
			goto mcerror;
		}
		newmname = mctable.mcpmt;
	}
	if (newmcode == PM_IOP  || newmcode == PM_CRAY_T3D || 
		(newmcode == PM_CRAY_T3E)
#ifndef _UNICOS
		|| (newmcode == PM_SPARC)
#endif
		) {
		if (curmcode == newmcode)
			return (-1);
		else if (curmcode == PMTNONE){
			curconfg->machine = newmname;
			return (-1);
		}else{
			error = PRIMARY;
			goto mcerror;
		}
	}
	else if ( (newmcode == PM_CRAY_YMP) || (newmcode == PM_CRAY_SV1)
	   ||(newmcode == PM_CRAY_C90) || (newmcode == PM_CRAY_TS)){

		if (curmcode == PMTNONE){
			curmcode = newmcode;
			curconfg->machine = newmname;
		}else if (curmcode != newmcode) {
			if(newmcode == PM_CRAY_SV1 && curmcode == PM_CRAY_YMP){
				curmcode = newmcode;
				curconfg->machine = newmname;
			}
			if(newmcode != PM_CRAY_YMP && curmcode != PM_CRAY_SV1){
				/* Error if types differ, unless YMP/SV1 */
				error = PRIMARY;
				goto mcerror;
			}
		}
		/* If vector recursion is set -- error */
		if (curconfg->word2.pdtxy.vrcr){
			curconfg->word2.pdtxy.vrcr = 0;
			error = VRECUR;
			goto mcerror;
		}

		/* check if addr32/noaddr32 values conflict */
		/* or if IEEE values conflict */
		if (firstime == 0){
			firstime = 1;
			curconfg->word2.pdtxy.addr32 = newconfg->word2.pdtxy.addr32;
			curconfg->word2.pdtxy.ieee = newconfg->word2.pdtxy.ieee;
		}
		if (curconfg->word2.pdtxy.addr32 != newconfg->word2.pdtxy.addr32){
			error = ADDR32;
			goto mcerror;
		}
		if (curconfg->word2.pdtxy.ieee != newconfg->word2.pdtxy.ieee){
			error = IEEE;
			goto mcerror;
		}

		/*
		 * Bits in the andmask are set only if set in both PDTs
		 */
		curconfg->word2.pdtmask.andmask &= newconfg->word2.pdtmask.andmask;
		/* Check if new pdt hardware requirements are a strict
		 * subset of the current pdt hardware requirements
		 */
		/* get mask for current configuration */
		curmask = curconfg->word2.pdtmask.mask;
		/* get mask for new configuration */
		newmask = newconfg->word2.pdtmask.mask;
		/* if the two masks are not the same and the differences appear
           	   as bits set in newmask, then continue processing; else 
           	   return a true status to calling process */
		if ((curmask ^ newmask) && newmask);
		else return(-1);
		/* check if vector recursion required/not allowed fields 
   		   differ from current primary machine type. */
		if (newconfg->word2.pdtxy.vrcr)
		{
			if (curconfg->word2.pdtxy.nvrcr)
			{
				newconfg->word2.pdtxy.vrcr = 0;
				error = VRECUR;
				goto mcerror;
			}
			curconfg->word2.pdtxy.vrcr = 1;
		}
		if (newconfg->word2.pdtxy.nvrcr)
		{
			if (curconfg->word2.pdtxy.vrcr)
			{
				newconfg->word2.pdtxy.nvrcr = 0;
				error = VRECUR;
				goto mcerror;
			}
			curconfg->word2.pdtxy.nvrcr = 1;
		}
		/* extract fields that can simply be added to current pdt flags
   		   without conflicting with current settings */
		curconfg->word2.pdtmask.mask |= (newconfg->word2.pdtmask.mask &
		  PDTORBITS);
	}
	else {
		/* Cray-1  & Cray-Xmp are no longer supported */
		error = PRIMARY;
		goto mcerror;
	}

	return(-1);

/* if an error occured, set return values and return false (0) */
mcerror:
	ptr = (LL_int *)mcoffs[error].mcname;
	*mcname = *ptr;
	*pdtword = mcoffs[error].wrd;
	*pdtstrt = mcoffs[error].strt;
	*pdtlen = mcoffs[error].len;
	return(0);	
}

/* VALTMC recognizes sv1 as a YMP subtype */
long
VALTMC(curconfg,newconfg,mcname,pdtword,pdtstrt,pdtlen)
struct pdttable *curconfg;
struct pdttable *newconfg;
LL_int *mcname;
LL_int *pdtword;
LL_int *pdtstrt;
LL_int *pdtlen;
{
	int sv1_is_type = 0;
	return VALTMC2(curconfg,newconfg,mcname,pdtword,pdtstrt,pdtlen,sv1_is_type);
}

#ifdef _UNICOS
/*
 * VALABS validates the absolute binary machine requirements from a
 *	  PDT against the host machine hardware configuration.
 * Input:
 *	abspdt - the PDT header record from the absolute binary image.
 *		 The first word should correspond to the first word of
 *		 the machine-type checking portion of the PDT. This word
 *		 is considered to have an offset value of 0. This table
 *		 is updated from the host machine configuration, one
 *		 hardware requirement field at a time, if the absolute
 *		 binary requires hardware features that are unavailable
 *		 on the host machine configuration. Every time a call
 *		 is made, VALABS reports an unavailable hardware feature
 *		 and removes the requirement for the feature from the
 *		 absconfg table in preparation for the next call.
 * Output:
 *	mcname - the name of a machine characteristic which is unavailable
 *		 on the host machine
 *	pdtword -the word in the PDT (absconfg) that requires a hardware feature
 *		 unavailable on the host machine. Word 0 is assumed to
 *		 be the first word of the machine-type checking portion
 *		 of the PDT.
 *	pdtstrt -The starting bit position of the field in the PDT that
 *		 contains the hardware feature that is unavailable on
 *		 the host machine. (leftmost bit in the word is bit 0).
 *	pdtlen - The length in bits of the field in the PDT that contains
 *		 the hardware feature that is unavailable on the host 
 *		 machine.
 *	bgnmask- always set to zero
 *	
 * Returns:
 *	 0 if the absolute binary requires hardware that is unavailable
 *	   on the host machine.
 *      -1 if the absolute binary is capable of being run correctly on
 *	   the host hardware configuration
 */
long
VALABS(abspdt,mcname,pdtword,pdtstrt,pdtlen,bgnmask)
struct pdttable *abspdt;
long *mcname;
long *pdtword;
long *pdtstrt;
long *pdtlen;
long *bgnmask;
{
	struct mctable mctable;
	struct pdttable hostpdt;
	int abs_pmtindx;  /* pmt index for absconfg  */
	int host_pmtindx;  /* pmt index for host  */
	int error, i;
	long *ptr;
	long  machn[2];
	long diff;
	unsigned long msk;
	struct charreq {
		int indx;
		int required;	/* Must hardware support match? */
	}; 
	/* The order of this table matches the order in pdt */
	struct charreq elemnt[] = {
		{VPOP,1},
		{EMA,1},
		{CIGS,1},
		{PC,1},
		{READVL,1},
		{VRECUR,1},
		{NOVRECUR,1},
		{AVL,1},
		{HPM,1},
		{BDM,1},
		{STATRG,1},
		{NUMCLSTR,1},
		{CORI,1},
		{ADDR32,1},
		{BMM,1},
		{AVPOP,1},
		{FULLSECT,0},
		{IEEE,1},
/*		{CMRREQ,1},	 deprecated */
/*		{CACHE,1},	 deprecated */
	};
	int numchar = sizeof(elemnt) / sizeof(struct charreq) - 1;

	*bgnmask = 0;  /* under UNICOS, the bgnmask is not used */

	/* get absolute machine type -- if not valid, return error */
	machn[0] = abspdt->machine;
	machn[1]=0;	/* make sure name is null terminated in case */
			/* it is exactly 8 chars long */
	if (abs_pmtindx = checkpmt((char *)machn) == -1){
		error = PRIMARY;
		goto mcerror;
	}
	if (_gethmc(&mctable) < 0) {
		error = PRIMARY;
		goto mcerror;
	}
	_MCTOPDT(&mctable,&hostpdt);
	machn[0] = hostpdt.machine;
	host_pmtindx = checkpmt((char *)machn);/* get host machine type */
	if ((host_pmtindx == PM_IOP) || (host_pmtindx == PM_CRAY_T3D)
		|| (host_pmtindx == PM_CRAY_T3E) 
#ifndef _UNICOS 
		|| (host_pmtindx == PM_SPARC) 
#endif
		){
		if (abs_pmtindx != host_pmtindx) {
			error = PRIMARY;
			goto mcerror;
		} else
			return(-1);
	}
	if ((abs_pmtindx == PM_IOP) || (abs_pmtindx == PM_CRAY_T3D)
		|| (abs_pmtindx == PM_CRAY_T3E) 
#ifndef _UNICOS 
		|| (abs_pmtindx == PM_SPARC) 
#endif
		){
		error = PRIMARY;
		goto mcerror;
	}
/* ******************* */
	/* Check the required machine characteristics found in abspdt
	   against the hardware features available on the host (hostpdt).
	   If host is deficient, then determine which characteristic holds
           the deficiency and return the error code  */
	diff = abspdt->word2.pdtmask.mask & ~(hostpdt.word2.pdtmask.mask);
	if (diff != 0) {
		msk = 1 << numchar;
		for (i = numchar; i >= 0; i--) {
			if ((msk & diff) && elemnt[i].required ) {
				abspdt->word2.pdtmask.mask = abspdt->word2.pdtmask.mask & ~ msk;
				error = elemnt[i].indx;
				goto mcerror;
			}
			msk = msk >> 1;
		}
	}
	return(-1);
	
/* if an error occured, set return values and return false (0) */
mcerror:
	ptr = (long *)mcoffs[error].mcname;
        *mcname = *ptr;
        *pdtword = mcoffs[error].wrd;
        *pdtstrt = mcoffs[error].strt;
        *pdtlen = mcoffs[error].len;
        return(0);
}

/*
 *	PDTCHAR(mcname,pdt)
 *	Parameters:
 *	   mcname = name of a logical machine characteristic (e.g., "cigs")
 *	   pdt    = pdt table
 *	Returns:
 *	   -1     if error (invalid characteristic name, etc.)
 *	    0     Characteristic not true
 *	    1	  Characteristic is true
 */
PDTCHAR(mcname,pdt)
char *mcname;
long pdt[];
{
	int indx;
	if ((indx = getmcindx(mcname)) == -1){
		return(-1);
	}
	if (mcoffs[indx].type != 1){
		/* not logical */
		return(-1);
	}
	if (mcoffs[indx].wrd < 0){
		/* bad characteristic */
		return(-1);
	}
	if (*(pdt+mcoffs[indx].wrd) & (1<<(63-mcoffs[indx].strt))){
		return(1);
	}
	return(0);
}
#endif /* _UNICOS */

/*
 * Returns primary machine characteristics.
 * Input:
 *      pmtname - the name of a primary machine type (e.g., CRAY-C90)
 * Output:
 *      pmctable - a 128 word table filled with the
 *      characteristics of the specified primary machine type.
 *      Each word is 64 bits.
 * Returns:
 *      -1 if the machine name was found.
 *      0 if the machine name was not found (invalid name)
 */
long _GETPMC2(pmctable, pmtname)
struct mctable *pmctable;
char *pmtname;
{
	register int i;
	i = _GETPMC(pmctable, pmtname);
	if (i == -1){	/* if pmtname was valid, */
		__setsv1type(pmctable);	/* update type if SV1 */
	}
	return(i);
}

/*
 * Returns target machine characteristics in the 128 word array
 * 	  mctabinit. Each word is 64 bits.
 * If the TARGET environment variable is set, it defines the target.
 * Otherwise, on a Cray the host machine is the target.
 * On the Sun, the target defaults to SPARC.
 */
void
_GETTMC2(mctabint)

ULL_int mctabint[128];  /* view the mctable as a 128 word array so that we can
			change certain values in the mctable structure but
			using an index instead of a structure members name */
{
	_GETTMC(mctabint);
	__setsv1type((struct mctable *)mctabint); /* Set type if subtype SV1 */
}
