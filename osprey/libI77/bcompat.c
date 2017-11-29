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


/* $Header: /proj/osprey/CVS/open64/osprey1.0/libI77/bcompat.c,v 1.1.1.1 2005/10/21 19:00:00 marcel Exp $ */

/*  
**  Provide backward compatibility with object files compiled with old 
**  releases (4D1-3.3 or earlier) of the compiler.
**
**  Also some support routines for the 32 bit stubs to 64bit IO code
*/

#include <cmplrs/fio.h>
#include "vmsflags.h"


f_open(a) olist *a;
{
   int alllong=0;

   return(f_open1(a,&alllong));
}


#ifdef sgi
f_open_unix(a) olist *a;
{
    f77vms_flag_[OLD_RL] = 1;
    f_open(a);
}

f_open_vms(a) olist *a;
{
    f77vms_flag_[OLD_RL] = 0;
    f_open(a);
}

f_inqu_vms(a) inlist *a;
{
    f77vms_flag_[OLD_RL] = 0;
    f_inqu(a);
}

f_inqu_unix(a) inlist *a;
{
    f77vms_flag_[OLD_RL] = 1;
    f_inqu(a);
}
#endif

f_inqu(a) inlist *a;
{
   int alllong=0;

   return(f_inqu0(a,&alllong));
}

void
get_cilist64( cilist64 *a64, cilist *a )
{
    a64->cierr = a->cierr;
    a64->ciunit = a->ciunit;
    a64->ciend = a->ciend;
    a64->cifmt = a->cifmt;
    a64->cirec = a->cirec;
    a64->cimatch = a->cimatch;
    a64->cikeytype = a->cikeytype;
    a64->cikeyval = a->cikeyval;
    a64->cikeyid = a->cikeyid;
    a64->cinml = a->cinml;
    a64->cikeyvallen = a->cikeyvallen;
#if defined(_COMPILER_VERSION) && (_COMPILER_VERSION>=400) /* MIPSpro */
    a64->ciadvance = a->ciadvance; 
    a64->ciadvancelen = a->ciadvancelen;
    a64->cieor = a->cieor;          
    a64->cisize = a->cisize;         
#endif
}

void get_inlist64(inlist64 *dst, inlist *src)
 {
dst->inerr = src->inerr;
dst->inunit = src->inunit;
dst->infile = src->infile;
dst->infilen = src->infilen;
dst->inex = src->inex;		/* parameters in standard's order */
dst->inopen = src->inopen;
dst->innum = src->innum;
dst->innamed = src->innamed;
dst->inname = src->inname;
dst->innamlen = src->innamlen;
dst->inacc = src->inacc;
dst->inacclen = src->inacclen;
dst->inseq = src->inseq;
dst->inseqlen = src->inseqlen;
dst->indir = src->indir;
dst->indirlen = src->indirlen;
dst->infmt = src->infmt;
dst->infmtlen = src->infmtlen;
dst->inform = src->inform;
dst->informlen = src->informlen;
dst->inunf = src->inunf;
dst->inunflen = src->inunflen;
dst->inrecl = src->inrecl;
dst->innrec = src->innrec;
dst->inblank = src->inblank;
dst->inblanklen = src->inblanklen;
dst->indefaultfile = src->indefaultfile;
dst->indefaultfilelen = src->indefaultfilelen;
dst->incc = src->incc;
dst->incclen = src->incclen;
dst->inkeyed = src->inkeyed;
dst->inkeyedlen = src->inkeyedlen;
dst->inorg = src->inorg;
dst->inorglen = src->inorglen;
dst->inrecordtype = src->inrecordtype;
dst->inrecordtypelen = src->inrecordtypelen;
#if defined(_COMPILER_VERSION) && (_COMPILER_VERSION>=400) /* MIPSpro */
					/* Fortran-90 extension */
dst->inaction = src->inaction;		/* ACTION=		*/
dst->inactionlen = src->inactionlen;		
dst->indelim = src->indelim;		/* DELIM=		*/
dst->indelimlen = src->indelimlen;
dst->inpad = src->inpad;			/* PAD=			*/
dst->inpadlen = src->inpadlen;
dst->inposition = src->inposition;		/* POSITION=		*/
dst->inpositionlen = src->inpositionlen;
dst->inread = src->inread;		/* READ=		*/
dst->inreadlen = src->inreadlen;
dst->inreadwrite = src->inreadwrite;		/* READWRITE=		*/
dst->inreadwritelen = src->inreadwritelen;
dst->inwrite = src->inwrite;		/* WRITE=		*/
dst->inwritelen = src->inwritelen;
/* next 3 fields are new  - dont copy */
dst->inconv = NULL;
dst->inconvlen = 0;
dst->inbuffsize = NULL;

#endif /* MIPSpro */
}

void get_icilist64(icilist64 *dst, icilist *src)
 {
 dst->icierr = src->icierr;
 dst->iciunit = src->iciunit;
 dst->iciend = src->iciend;
 dst->icifmt = src->icifmt;
 dst->icirlen = src->icirlen;
 dst->icirnum = src->icirnum;
 }

void get_olist64(olist64 *dst, olist *src)
 {
 dst->oerr = src->oerr;
 dst->ounit = src->ounit;
 dst->ofnm = src->ofnm;
 dst->ofnmlen = src->ofnmlen;
 dst->osta = src->osta;
 dst->oacc = src->oacc;
 dst->ofm = src->ofm;
 dst->orl = src->orl;
 dst->oblnk = src->oblnk;
 dst->occ = src->occ;
 dst->oorg = src->oorg;
 dst->oshared = src->oshared;
 dst->oreadonly = src->oreadonly;
 dst->onkeys = src->onkeys;
 dst->okeys = src->okeys;
 dst->oassocv = src->oassocv;
 dst->odfnm = src->odfnm;
 dst->odfnmlen = src->odfnmlen;
 dst->odisp = src->odisp;
 dst->omaxrec = src->omaxrec;
 dst->orectype = src->orectype;

#if defined(_COMPILER_VERSION) && (_COMPILER_VERSION>=400) /* MIPSpro */
				/* Fortran-90 extension */
 dst->oaction = src->oaction;		/* ACTION=		*/
 dst->oactionlen = src->oactionlen;
 dst->odelim = src->odelim;		/* DELIM=		*/
 dst->odelimlen = src->odelimlen;
 dst->opad = src->opad;		/* PAD=			*/
 dst->opadlen = src->opadlen;
 dst->oposition = src->oposition;	/* POSITION=		*/
 dst->opositionlen = src->opositionlen;
	/* next 4 fields are new - no need to copy */
		/* Extensions for portability and performance */
 dst->oconv = NULL;	/* File conversion parameter.  Valid values */
 dst->oconvlen = 0;	/*are: "LITTLE_ENDIAN", "BIG_ENDIAN",
				   "CRAY", "IBM", "VAXD", "VAXD", "NATIVE" */
 dst->obuffsize = 0;	/*User-selected buffer size */
 dst->odirect = 0;	/*applying direct I/O to this direct
			   unformatted file */

#endif /* MIPSpro */
 }


/* we include the following temporarily because xxx64 calls are
   not available in 5.2/6.0 yet in the 32 bit libc */
#ifdef NO_XXX64_IN_LIBC

long long lseek64(int a, long long b, int c)
 {
 return (long long)  lseek(a, (long) b, c);
 }

int fseek64(FILE * a, long long b, int c)
 {
 return fseek(a, (long) b, c);
 }

long long ftell64(FILE *a)
 {
 return (long long) ftell(a);
 }

int ftruncate64(int a, long long b)
 {
 return ftruncate(a, (long) b);
 }

int truncate64(const char *a, long long b)
 {
 return truncate(a, (long) b);
 }

int fstat64(int a , struct STAT_TAG *b)
 {
 return fstat(a,b);
 }

int stat64(const char *a, struct STAT_TAG *b)
 {
 return stat(a,b);
 }

#endif
