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


static char USMID[] = "@(#) libf/auxarr/c1/auxinit.c	92.0	10/08/98 14:30:10";


/* Auxiliary array routines */
#include <stdio.h>
#include <malloc.h>
#include <errno.h>
#include "aux.h"
extern int sys_nerr;
extern char *sys_errlist[];

/* Semaphore used to lock auxiliary array routines - must match */
/* what is used in %REVVA%, etc. */
#define AUXSEM  1

extern info _infoblk;	/* Initialized by SEGLDR */

extern int $pgshift;		/* Shift value for converting an address */
				/* to a page. Used by %REVVA%, etc. */
extern int *$adartable;		/* Address table. One entry per SDS "page" */
				/* Used by %REVVA%, etc. */

/* Stride of 2 */
int $stride2buf[]={0,2,4,6,8,10,12,14,16,18,20,22,24,26,28,30,32,
	34,36,38,40,42,44,46,48,50,52,54,56,58,60,62,64,66,68,70,
	72,74,76,78,80,82,84,86,88,90,92,94,96,98,100,102,104,
	106,108,110,112,114,116,118,120,122,124,126,128,130,132,134,
	136,138,140,142,144,146,148,150,152,154,156,158,160,162,164,
	166,168,170,172,174,176,178,180,182,184,186,188,190,192,194,
	196,198,200,202,204,206,208,210,212,214,216,218,220,222,224,
	226,228,230,232,234,236,238,240,242,244,246,248,250,252,254};

int $tmparea[2];	/* temporary area for reading and writing */
			/* used by %REVVA%, etc. */

static int *adar;	/* describes pages that are in memory */
static int pgsize;	/* size of a page. In units of 512 words. */
static int nbuf;	/* number of buffers */
static int *bufptr;	/* Points to beginning of pages in memory. */
static int vmsize = 0;	/* Total size of $adartable */
static int adarsize = 0;	/* High water mark in $adartable */
static int maxtransize;	/* Number of entries in transformation table */
static int numtrans = 0;/* Number of used entries in transformation table */
static trans *transptr;	/* points to transformation table */

static int __vainit = 0;/* Indicates whether we've done initialization */

static int replace = 0;	/* Next entry in $adartable to be replaced */
static int pgblks;	/* 512 * (2**pgblks) = size of a page */

static int _sdswrite = 0;	/* Number of times we have written to SDS */
static int _sdsread = 0;	/* Number of times we have read from SDS */

/*
 * Prototypes
 */

static void _fmtnum(char *bufptr, unsigned num);
static void _auxerr(char *msg, int err);

/*
 * Initialization for auxiliary arrays 
 */

$auxinit(start)
int start;		/* Indicates whether this routine is called */
			/* from $START */
{
unsigned vmlen;
char *envptr;
char *getenv();
info *infoblk;
int sds_static_ptr;	/* for static SDS allocation */
int err;		/* Error returned from sdsalloc */

	/* See if user has specified number of buffers */
	if ((envptr = getenv("AUXBUF")) != NULL){
		nbuf = atoi(envptr);
	}
	else
		nbuf = 64;

	/* See if the user has specified the size of each buffer. */
	/* The size of each buffer is specified in units of 512 words, */
	/* and must be a power of 2, to make conversion from addresses to */
	/* pages easy. */
	if ((envptr = getenv("AUXPAGE")) != NULL){
		pgsize = atoi(envptr);
         	pgblks = 64-_leadz(pgsize)-1;
		pgsize = 1<<pgblks;
		$pgshift = pgblks+9;
	}
	else{
		pgblks = 1;
		$pgshift = 10;	/* default is 1024 */
		pgsize = 2;	/* pgsize is in units of 512 */
	}

	maxtransize = 10;	/* Default size of translation table */
	if((transptr = 
	(trans *)malloc(sizeof(trans)*maxtransize)) == (trans *)0){
		_auxerr(MSG1,0);
	}

	/* If this routine is called from $START, determine how much */
	/* SDS space we need, and allocate it */
	if (start == 0){
		infoblk = &_infoblk;
		vmlen = infoblk->vmlen;	/* Segldr tells us how many */
					/* words of SDS space we need. */

		/* Allocate space on the ssd */
		/* The allocation unit is 512-word blocks. */
		/* But, to make life easier, we allocate multiples of pages */
	
		vmsize = (((vmlen+511)/512)+(pgsize-1))/pgsize * pgsize;
		adarsize = vmsize;
		if (vmlen != 0){
			if ((sds_static_ptr = sdsalloc(vmsize,&err)) == -1){
				_auxerr(MSG2,err);
			}

			/* Allocate space for the address table and initialize it */
			/* Need 1 word per page */
			if (($adartable = (int *)malloc(vmsize/pgsize *8))==NULL){
				_auxerr(MSG1,0);
			}
			/* Indicate that no pages are in memory */
			(void) _memwset($adartable,-1,vmsize/pgsize);

			/* Initialize the first entry in the translation table */
			/* to correspond to the space just allocated */
			transptr->begcompadr = 0;
			transptr->endcompadr = vmsize*512 -1;
			transptr->ssdadr = sds_static_ptr;
			numtrans=1;
		}
	}

	/* Allocate space for the pages that will be held in memory. */
	if ((bufptr = (int *)calloc(nbuf * 512 * 8 * pgsize+nbuf*8,1)) == NULL){
		_auxerr(MSG1,0);
	}
	
	/* This array will contain an element that describes the SDS address */
	/* for each page in memory. */
	if ((adar = (int *)malloc(nbuf*8)) == NULL){
		_auxerr(MSG1,0);
	}
	/* Initialize it to show that nothing is in memory. */
	(void) _memwset(adar,-1,nbuf);
	__vainit = 1;
} 


/*
 * Read a page in from SDS
 */
$auxread(adr)
int adr;	/* page to be read in */
{
int offset;
int i;
int newpage;
int sdsadr;
int sdsword;
int found;
trans *tptr;
int adarrep;

	sdsadr = adr<<(pgblks);

	/* replace indicates which page currently in memory is to be */
	/* swapped out. */
	adarrep = *(adar+replace);

	sdsword = sdsadr*512;

	/* Indicate that the page to be replaced is no longer in memory */
	if (adarrep != -1) {
	 	*($adartable+(adarrep>>pgblks)) = -1;
	}

	/* Determine whether the page to be replaced needs to be */
	/* written to the SDS */
	/* "bufptr" contains the address of all the pages that are in */
	/* memory. "replace" tells which page we are replacing. The first */
	/* word of each page indicates whether that page is dirty. */
	/* Look at the "dirty" word for the replacement page. */
	if (*((int *)bufptr + (replace<<$pgshift) + replace)!=0){
		*((int *)bufptr + (replace<<$pgshift) + replace)=0;
		/* Determine the offset into sds. Look through*/
		/* the translation table to find this address */
		tptr = transptr;
		found = FALSE;
		for (i = 0; i < numtrans; i++){
			if ((adarrep>=tptr->begcompadr/512) &&
			 (adarrep<= tptr->endcompadr/512)){
				offset = tptr->ssdadr+adarrep-(tptr->begcompadr)/512;
				found = TRUE;
				break;
			}
			tptr++;
		}
		if (found){
			/* Write it */
			if(sswrite((int *)bufptr + (replace<<($pgshift))+(replace+1),
			  offset,pgsize) == -1) {
				_auxerr(MSG4,errno);
			}
			_sdswrite++;
		}	
	}
	/* Now read in the requested page. */
	tptr = transptr;
	found = FALSE;

	/* Look through the translation table to find the SDS offset */
	for (i = 0; i < numtrans; i++){
		if (((sdsword)>=tptr->begcompadr) &&
		   ((sdsword)<= tptr->endcompadr)){
			offset = tptr->ssdadr+sdsadr - tptr->begcompadr/512;
			found = TRUE;
			break;
		}
		tptr++;
	}
	if (!found){
		_auxerr(MSG9,0);
	}

	/* Read in the requested page */
	newpage = replace*512*pgsize;
	if(ssread((int *)bufptr + newpage+(replace+1),offset,pgsize) == -1)
	{
		_auxerr(MSG5,errno);
	}
	_sdsread++;

	/* 
	 * Save address
	 */
	*(adar+replace) = sdsadr;

	/* Update $adartable to indicate that this page is in memory*/
	newpage += replace;
 	*($adartable+adr)=(int)(bufptr+newpage);

	/* Replace pages in a round-robin fashion */
	replace++;
	if (replace>=nbuf)
		replace=0;
	return((int)(bufptr+newpage));
}

AUXSTAT(){
char buf[100];
 	
	(void) strcpy(buf,"Sds written to ");
        _fmtnum(&buf[strlen(buf)],_sdswrite);
	strcat(buf," times \n");
	write(2,buf,strlen(buf));	
	(void) strcpy(buf,"Sds read from ");
        _fmtnum(&buf[strlen(buf)],_sdsread);
	strcat(buf," times \n");
	write(2,buf,strlen(buf));	
}
static void
_fmtnum(char *bufptr, unsigned num)
{
	int i, pr, d;

	pr = 0;
	for (i = 10000000000000; i != 1; i /=10)
	{
		if ((pr |= (d = num/i)))
			*bufptr++ = d + '0';
		num %= i;
	}
	*bufptr++ = num + '0';
	*bufptr++ = ' ';
	*bufptr = '\0';
}
static void
_auxerr(char *msg, int err)
{
char buf[50];
	write(2,msg,strlen(msg));
	if (err != 0){
        	_fmtnum(buf,err);
		write(2,buf,strlen(buf));
		if (err < sys_nerr && sys_errlist[err] != 0){
			write(2,sys_errlist[err],strlen(sys_errlist[err]));
		}
		write(2,"\n",1);
       	} 
	_semclr(AUXSEM);
	abort();
}

/* Allocate ssd space */

#define TINC 1000
#define BIGNUM 10000

$ssdallc(nwords,abflag,status)
int nwords;	/* number of words to allocate */
int abflag;	/* Abort on error? */
long *status;
{
int nblk;
int sdsptr;
int i,j,x;
trans *tptr;
trans *xptr;
int err;
char *msg;
#define ERREX(x,y)   msg = x; err = y; goto errexit;
		    
	if (!__vainit){
		$auxinit(1);
	}
	/* Round nwords to nearest page */
	nblk = ((((nwords+511)/512)+(pgsize-1))/pgsize) * pgsize;

	nwords = nblk*512;
	*status = 0;

	/* Can  we allocate this much space? */
	if((sdsptr = sdsalloc(nblk,&err)) == -1){
		ERREX(MSG2,err);
	}
	/* We need to transform this address to one the */
	/* compiler will use. The reason we do this is */
	/* so that $adartable will not have large unused areas. */
	/* Since we may not be the only one in this process*/
	/* using SDS space, our actual addresses may not be contiguous.*/
	/* Is there a free area in the transformation table */
	/* where we can put this address? */
	tptr = transptr;
	for (i = 0; i < numtrans-1; i++){
		x = tptr->endcompadr;
		tptr++;
		if ((tptr->begcompadr-x)>= nwords){
			/* Have enough space */
			/* Move the following entries down so that */
			/* we can insert this one. */
			/* Is transptr big enough? */
			if (numtrans < maxtransize){
				/* It will go between i and i+1 */
				/* In this case, there is guaranteed to */
				/* be initialized space $adartable */
			}
			else {
				/* We have adequate room for this address*/
				/* between i and i+1, but the transformation */
				/* table is not big enough. */
				if((transptr = (trans *)realloc(transptr,
				  sizeof(trans)*(maxtransize+TINC))) == (trans *)0){
					ERREX(MSG6,0);
				}
				maxtransize+=TINC;
			}

			tptr = transptr+numtrans;
			for (j = numtrans; j > i; j--){
				xptr = tptr-1;
				tptr->begcompadr = xptr->begcompadr;			
				tptr->endcompadr = xptr->endcompadr;			
				tptr->ssdadr = xptr->ssdadr;			
				tptr = xptr;
			}
			/* Always make this a multiple of pages. */
			tptr->begcompadr = x+1;
			tptr->endcompadr = x+nwords;
			tptr->ssdadr = sdsptr;	
			numtrans++;
			return(tptr->begcompadr);
		}
	}
	/* Do not have space in the middle of the table. */
	/* Is there room to add it on the end? */
	if (numtrans < maxtransize){
		tptr = transptr+numtrans;
		if (numtrans != 0){
			xptr = tptr-1;
			x = xptr->endcompadr;
			tptr->begcompadr = x+1;
			tptr->endcompadr = x+nwords;
		}
		else {
			tptr->begcompadr = 0;
			tptr->endcompadr = nwords;
			
		}
		tptr->ssdadr = sdsptr;	
		numtrans++;
	}
	else{
		if((transptr = (trans *)realloc(transptr,
		  sizeof(trans)*(maxtransize+TINC))) == (trans *)0){
			ERREX(MSG6,0);
		}
		maxtransize+=TINC;
		tptr = transptr+numtrans;
		xptr = tptr-1;
		x = xptr->endcompadr;
		tptr->begcompadr = x+1;
		tptr->endcompadr = x+nwords;
		tptr->ssdadr = sdsptr;
		numtrans++;
	}
	/* Is $adartable big enough? */
	if (adarsize+nblk > vmsize){
		/* No */
		if (adarsize == 0){
			if (($adartable = (int *)malloc(nblk/pgsize *8))==NULL){
				ERREX(MSG6,0);
			}
		}
		else if(($adartable = (int *)realloc($adartable,
		  (adarsize+nblk)/pgsize *8))==(int *)0){
			ERREX(MSG6,0);
		}	
		/* Adartable space is initialized to -1 */
		(void) _memwset($adartable+adarsize/pgsize,-1,nblk/pgsize);
		adarsize+=nblk;
		vmsize=adarsize;
	}
	else
		adarsize+=nblk;	
	return(tptr->begcompadr);
errexit:
	if (abflag!= 0)
		_auxerr(msg,err);
	else
		*status = err;
	return((long)-1);
}

$ssdfree(adr,abflag)
long adr;	/* Address to free */
long abflag;	/* 0 - return an error code if an error occurs. */
		/* Nonzero - abort on error */
{
trans *tptr;
trans *xptr;
int i,j;
int *aptr;
int *ptr;
int err;
char *msg;
#define ERREX(x,y)   msg = x; err = y; goto errexit;

	/* Search for adr in the transformation table */
	tptr = transptr;
	for (i=0; i < numtrans; i++){
		if (tptr->begcompadr == adr){
			/* Found it. */
			if (sdsfree(tptr->ssdadr,&err)!=0){
				ERREX(MSG7,err);
			}
			ptr = adar;
			for (j = 0; j < nbuf; j++){
				/* These pages are no longer in memory */
				if (*ptr >= tptr->begcompadr/512 && *ptr <=
				tptr->endcompadr/512){
					*ptr = -1;
				}
			ptr++;
			}
			/* Is this the last entry in the transformation */
			/* table? */
			if (i+1 == numtrans){
				/* Yes */
				/* See if this falls below a threshold that */
				/* indicates it's time to reallocate the */
				/* transformation table. */
				if ((vmsize - (adarsize-
				  (tptr->endcompadr - tptr->begcompadr+1)/512)) > BIGNUM){
					if (adarsize - (tptr->endcompadr - tptr->begcompadr + 1)/512 == 0)
						free($adartable);
					else if(($adartable = (int *)realloc($adartable,
					  (adarsize-(tptr->endcompadr - tptr->begcompadr
					  +1)/512)/pgsize * 8)) == (int *)0){
						ERREX(MSG6,0);
					}
					vmsize = (adarsize - (tptr->endcompadr -
					tptr->begcompadr + 1)/512);
					adarsize = vmsize;
				}
				else {
					aptr= $adartable+tptr->begcompadr/(512*pgsize);
					(void) _memwset(aptr ,-1,
					  (tptr->endcompadr - tptr->begcompadr+1)/(512*pgsize));
					adarsize = (adarsize - (tptr->endcompadr -
					tptr->begcompadr + 1)/512);
				}
				if (maxtransize - numtrans > TINC){
					if ((transptr = (trans *)realloc(transptr,
					  sizeof(trans)*(maxtransize-TINC))) == (trans *)0){
						ERREX(MSG6,0);
					}
					maxtransize-=TINC;
				}
			}
			else{
				/* Not the last entry */
				/* Move entries up in the transformation */
				/* table. */	
				aptr= $adartable+tptr->begcompadr/(512*pgsize);
				(void) _memwset(aptr ,-1,
				  (tptr->endcompadr - tptr->begcompadr+1)/(512*pgsize));
				for (j = i+1; j < numtrans; j++){
					xptr = tptr+1;
					tptr->begcompadr = xptr->begcompadr;
					tptr->endcompadr = xptr->endcompadr;
					tptr->ssdadr = xptr->ssdadr;
					tptr++;
				}
			}
			numtrans--;
			replace = 0;
			return(0);
		}
		tptr++;
	}
	ERREX(MSG8,0);

errexit:
	if (abflag!= 0)
		_auxerr(msg,err);
	else
		return((long)err);
}
