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



/* $Header: /proj/osprey/CVS/open64/osprey1.0/libI77/nio.c,v 1.1.1.1 2005/10/21 19:00:00 marcel Exp $ */

#include <stdio.h>
#include <mutex.h>
#include <math.h>
#include <ctype.h>
#include <string.h>
#include <cmplrs/fio.h>
#include "fmt.h"
#include "iomode.h"
#include "lio.h"
#include "lread.h"
#include "err.h"
#include "open.h"
#include "nio.h"
#include "util.h"
#include "bcompat.h"

/*
 *	namelist io
 *
 *	see f77's proc.c at namelist() for description
 */

#define BSIZ    (unsigned) 50
static char     nlrs[] = "namelist read";
static char    *getword(unit *, char *, unsigned int, int, int);
static int c_nle (cilist64 *pcl, unit **fu);
static int s_wsne64_mp (cilist64 *pnlarg, unit **fu);


#define XINT_TYPE	int
#define NAMEDims	Dims
#define NAMENlentry	Nlentry
#define NAMENamelist	Namelist

#include "nio_decl.h"	/* define Dims, Nlentry, Namelist */

#undef XINT_TYPE
#undef NAMEDims
#undef NAMENlentry
#undef NAMENamelist

#define XINT_TYPE	XINT
#define NAMEDims	Dims64
#define NAMENlentry	Nlentry64
#define NAMENamelist	Namelist64

#include "nio_decl.h"	/* define Dims64, Nlentry64, Namelist64 */
#undef XINT_TYPE
#undef NAMEDims
#undef NAMENlentry
#undef NAMENamelist


/*
   we build 2 sets of routines for -64 -- a "compatibility" set using the
   32 bit interface, and a "full 64bit" set for 64bit I/O.  The 32-bit
   interface will generally be stubs to the 64bit routines.  For -r32/-n32
   we are extending the interface to "partial 64bit", there are no
   backwards compatibility issues here.
*/

#define NAMEs_rsNe_work	s_rsNe_work
#define NAMEs_wsNe_work	s_wsNe_work
#define TYPEcilist	cilist64	/* we convert all cilists to cilist64 */
#define TYPENamelist	Namelist
#define TYPENlentry	Nlentry
#define NAMEfindit	findit
#define NAMEgetvar	getvar

#include "nio_work.h"	/* define s_rsNe_work, findit, getvar */
#undef NAMEs_rsNe_work
#undef NAMEs_wsNe_work
#undef TYPEcilist
#undef TYPENamelist
#undef TYPENlentry
#undef NAMEfindit
#undef NAMEgetvar

/* define the full 64 set */
#define NAMEs_rsNe_work	s_rsNe64_work
#define NAMEs_wsNe_work	s_wsNe64_work
#define TYPEcilist	cilist64
#define TYPENamelist	Namelist64
#define TYPENlentry	Nlentry64
#define NAMEfindit	findit64
#define NAMEgetvar	getvar64

#include "nio_work.h"	/* define s_rsNe64_work, findit64, getvar64 */
#undef NAMEs_rsNe_work
#undef NAMEs_wsNe_work
#undef TYPEcilist
#undef TYPENamelist
#undef TYPENlentry
#undef NAMEfindit
#undef NAMEgetvar



/*
 * s_rsne - start read namelist from external file
 *
 * s_rsNe and s_rsne are the same but s_wsne calls getvar_old which changes
 * from the old type enumeration using the match_type array in lio.h (I90)
 *
 * s_rsne
 *	if (file not initialized)
 *		intialize it
 *	if (file doesn't jive with namelist io)
 *		return error
 *	initialize some global variables
 *	if (not currently reading on file OR not capable of doing so)
 *		return error
 *	read namelist name
 *	if (not correct namelist name)
 *		error
 *	determine number of variables in namelist
 *	read variable name
 *	while (variable name is not "$end")
 *		if (variable is in namelist)
 *			read value(s) for it
 *		else
 *			error
 *	if (not correct number of variables read)
 *		error
 * end s_rsne
 */


int
s_rsne (cilist *pnlarg)
{
   Namelist       *pnl;
   Nlentry        *pnlent;

/* to maintain backwards compatibility and reasonable performance, we only
   convert the cilist structure to cilist64 */
 cilist64 dst;
 get_cilist64(&dst, pnlarg);
#define WORK_ARG        &dst

#ifdef I90OLD
				/* Fortran 90 wants to call wsne from rsne */
				/* also wants to use match_type in getvar */
 return s_rsNe_work(WORK_ARG, &f77curunit, 1, 1);

#else	/* ! I90OLD */

				/* fortran 77 does a bunch of pre-processing */
   pnl = (Namelist *) pnlarg->cinml;
   pnlent = pnl->nlvnames;
   do {
      if (pnlent->type >= 0)
	 pnlent->type = match_type[pnlent->type];
      pnlent++;
   } while (strlen (pnlent->varname));
   return s_rsNe_work (WORK_ARG, &f77curunit,1,0); /* F77 wants to call s_rsne from rsne */
#endif	/* I90OLD */

#undef WORK_ARG
}

int
s_rsNe (cilist *pnlarg)
{
/* to maintain backwards compatibility and reasonable performance, we only
   convert the cilist structure to cilist64 */
 cilist64 dst;
 get_cilist64(&dst, pnlarg);
#define WORK_ARG	&dst
#ifdef I90OLD
	return s_rsNe_work(WORK_ARG, &f77curunit, 0, 0);  /* but Fortran 90 wants to call wsNe
					 from rsNe, also dont use match_type */
#else
	return s_rsNe_work(WORK_ARG, &f77curunit, 1, 0);
				/* Fortran 77 wants to call wsne from
				   rsNe */
#endif
#undef WORK_ARG
}


int
s_rsNe_mp (cilist *pnlarg, unit **fu)
{
/* to maintain backwards compatibility and reasonable performance, we only
   convert the cilist structure to cilist64 */
 cilist64 dst;
 get_cilist64(&dst, pnlarg);
#define WORK_ARG	&dst
#ifdef I90OLD
	return s_rsNe_work(WORK_ARG, fu, 0, 0);  /* but Fortran 90 wants to call wsNe
					 from rsNe, also dont use match_type */
#else
	return s_rsNe_work(WORK_ARG, fu, 1, 0);
				/* Fortran 77 wants to call wsne from
				   rsNe */
#endif
#undef WORK_ARG
}



/* miscellaneous utility functions for namelist read */

/* getword - get a "word" text string from current file */

static char    *getword(unit *ftnunit, char *s, unsigned int n, int skip_newline, int space_is_a_delimiter)
{
   int             i;
   char           *p = s;

 /* If skip_newline is FALSE then we don't read past this line and
    return an empty string if this line contains just white spaces
    and a newline. PV 272510 */

   if (!skip_newline) {
      while (isspace(GETC(i))) {
	if (i == '\n') {
           *p = '\0';
           return s;
        }
      }
      UNGETC (i);
   }

/* LHL 5/22/89
 * fix bug 4573
 * change it so that any char in the 1st column are considered as comments
 * till the rest of the line.
 */

   while ((GETC (i) == '\n') && (GETC (i) && (i == '*' || i == 'C' || i == 'c'))) {
      for (UNGETC (i); GETC (i) != '\n' && i != EOF;);
      if (i == '\n')
	 UNGETC (i);
      if (i == EOF)
	 break;
   }
   UNGETC (i);
/* end of change */
   for (GETC (i);
   isspace (i) || (ispunct (i) && i != '?' && i != '$' && i != '&');
	GETC (i)) {
/* LHL 5/22/89
 * fix bug 4573
 * change it so that comments are allowed anywhere within the namelist
 * file.
 */
      if (i == '!') {
	 for (UNGETC (i); GETC (i) != '\n' && i != EOF;);
	 if (i == '\n')
	    UNGETC (i);
	 while ((GETC (i) == '\n') && (GETC (i) && (i == '*' || i == 'C' || i == 'c'))) {
	    for (UNGETC (i); GETC (i) != '\n' && i != EOF;);
	    if (i == '\n')
	       UNGETC (i);
	    if (i == EOF)
	       break;
	 }
	 UNGETC (i);
      }
/* end of change */
   }
   if (space_is_a_delimiter) {
     while (n--) {
        if (i != EOF && i != '=' && !isspace (i) )
           *p++ = (isupper (i) ? i + 'a' - 'A' : i);
        else
           break;
        GETC (i);
     } /* while loop */
   } else {
     while (n--) {
        if (i != EOF && i != '=' && (!isspace (i) || i == ' ' || i == '\t'))
           *p++ = (isupper (i) ? i + 'a' - 'A' : i);
        else
           break;
        GETC (i);
     } /* while loop */
   }    /* if */

/* LHL 5/22/89
 * fix bug 4573
 * make sure that '\n' gets back to the input stream.
 */
   if (i == '\n')
      UNGETC (i);
/* end of change */
   if (feof (ftnunit->ufd) && p == s)
      return NULL;
   *p = '\0';
   return s;
}


/*
 * s_wsne - start write namelist 
 *
 * s_wsne
 *	if (file not initialized)
 *		initialize it
 *	if (file doesn't jive with namelist io)
 *		return error
 *	initialize some global variables
 *	if (not currently writing on file OR not capable of doing so)
 *		return error
 *	set up namelist and entry pointers
 *	output namelist name in proper format
 *	do
 *		output variable name
 *		output value based on type
 *		point to next entry
 *	while (there are more to do AND sneakily output a comma separator)
 *	output end line
 * end s_wsne
 */

/* compatibility bug fix */
int
s_wsne (cilist *pnlarg)
{
/* to maintain backwards compatibility and reasonable performance, we only
   convert the cilist structure to cilist64 */
 cilist64 dst;
 get_cilist64(&dst, pnlarg);
#ifdef I90OLD
 return s_wsNe_work(&dst, &f77curunit, 1);
#else
 return s_wsNe_work(&dst, &f77curunit, 0);
#endif

}

int
s_wsne_mp (cilist *pnlarg, unit **fu)
{
   Namelist       *pnl;
   Nlentry        *pnlent;
/* to maintain backwards compatibility and reasonable performance, we only
   convert the cilist structure to cilist64 */
 cilist64 dst;
 get_cilist64(&dst, pnlarg);
#define WORK_ARG        &dst

#ifdef I90OLD
	/* Fortran 90: set use_match_type to use match_type, but doesn't
		set it in the list - dont know if it makes any difference
		but thats the way the code goes */
 return s_wsNe_work(WORK_ARG, fu, 1);

#else	/* ! I90 */

	/* Fortran 77: does match_type up-front, sets it in the list */
   pnl = (Namelist *) pnlarg->cinml;
   pnlent = pnl->nlvnames;
   do {
      if (pnlent->type >= 0)
	 pnlent->type = match_type[pnlent->type];
      pnlent++;
   } while (strlen (pnlent->varname));
   return s_wsNe_work (WORK_ARG, fu, 0);
#endif

#undef WORK_ARG
}

static int
s_wsne64_mp (cilist64 *pnlarg, unit **fu)
{
   Namelist       *pnl;
   Nlentry        *pnlent;

#ifdef I90OLD
	/* Fortran 90: set use_match_type to use match_type, but doesn't
		set it in the list - dont know if it makes any difference
		but thats the way the code goes */
 return s_wsNe_work(pnlarg, fu, 1);

#else	/* ! I90 */

	/* Fortran 77: does match_type up-front, sets it in the list */
   pnl = (Namelist *) pnlarg->cinml;
   pnlent = pnl->nlvnames;
   do {
      if (pnlent->type >= 0)
	 pnlent->type = match_type[pnlent->type];
      pnlent++;
   } while (strlen (pnlent->varname));
   return s_wsNe_work (pnlarg, fu, 0);
#endif

}


int
s_wsNe (cilist *pnlarg)
{
/* to maintain backwards compatibility and reasonable performance, we only
   convert the cilist structure to cilist64 */
 cilist64 dst;
 get_cilist64(&dst, pnlarg);
#define WORK_ARG	&dst
    return( s_wsNe_work( WORK_ARG, &f77curunit, 0 ) );
#undef WORK_ARG
}


int
s_wsNe_mp (cilist *pnlarg, unit **fu)
{
/* to maintain backwards compatibility and reasonable performance, we only
   convert the cilist structure to cilist64 */
 cilist64 dst;
 get_cilist64(&dst, pnlarg);
#define WORK_ARG	&dst
   return s_wsNe_work(WORK_ARG, fu, 0);
	/* dont set use_match_type */
#undef WORK_ARG
}


/*
 * c_nle - check namelist 
 *
 * c_nle
 *	set up global variables
 *	if (bogus unit)
 *		f77fatal error
 *	if (unit is unitialized AND can't be)
 *		f77fatal error
 *	if (can't do unformatted io on unit)
 *		f77fatal error
 * end c_nle
 */

static
int
c_nle (cilist64 *pcl, unit **fu)
{
   unit		*ftnunit;

   ftnunit = *fu = map_luno (pcl->ciunit);
   while (fu != &f77curunit && test_and_set( &ftnunit->lock_unit, 1L ))
       ;
   if (ftnunit->uconn <= 0 && fk_open (SEQ, FMT, pcl->ciunit)) {
      ftnunit->uconn = 0;
      errret(pcl->cierr, 102, "namelist io");
   }
   ftnunit->f77fmtbuf = "namelist io";
   ftnunit->f77scale = 0;
   ftnunit->f77recpos = 0;
   ftnunit->ufd = ftnunit->ufd;

   if (!ftnunit->ufmt)
      errret(pcl->cierr, 103, "namelist io");
   return (0);
}

#pragma weak __kai_s_rsne = s_rsNe
#pragma weak __kai_s_wsne = s_wsNe
#pragma weak __kai_s_rsne_mp = s_rsNe_mp
#pragma weak __kai_s_wsne_mp = s_wsNe_mp

/* now declare all the 64 bit versions */

int
s_rsNe64(cilist64 *pnlarg)
	{
	return s_rsNe64_work(pnlarg, &f77curunit, 1, 0);
	}

int
s_rsNe64_mp(cilist64 *pnlarg, unit **fu)
	{
	return s_rsNe64_work(pnlarg, fu, 1, 0);
	}

int
s_wsNe64(cilist64 *pnlarg)
	{
	return( s_wsNe64_work(pnlarg, &f77curunit, 0 ));
	}

int
s_wsNe64_mp(cilist64 *pnlarg, unit **fu)
	{
	return( s_wsNe64_work(pnlarg, fu, 0));
	}

#pragma weak __kai_s_rsne64 = s_rsNe64
#pragma weak __kai_s_wsne64 = s_wsNe64
#pragma weak __kai_s_rsne64_mp = s_rsNe64_mp
#pragma weak __kai_s_wsne64_mp = s_wsNe64_mp

