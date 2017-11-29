/*

  Copyright (C) 2000, 2001, Silicon Graphics, Inc.  All Rights Reserved.

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



#pragma ident "@(#) libf/fio/f77wrappers.c	92.3	11/16/99 15:43:33"

 
#if	!defined(_LITTLE_ENDIAN)
/*
 * This file contains the necessary interfaces to either the Cray or old F77 
 * routines which need such new interfaces
 *
 */
#include <stdio.h>
#include <foreign.h>
#include <errno.h>
#include <liberrno.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <stdlib.h>
#include "fio.h"


/* Return 1 if open by F90, 0 otherwise */
static int f90_or_f77 (int *u)
{
   unit	*cup;
   cup	= _fort_unit[UHASH(*u)].ulist;
   if (cup != NULL) {
      if (cup->private || cup->uid != *u)
	 cup	= _search_unit_list(cup, *u);
   }
   return (cup != NULL);
}

#define SELECT(unit, funcname, args) \
if (f90_or_f77(unit)) return __##funcname##_f90##args; else  return __##funcname##_f77##args


/* all the external routines */

extern int __fgetc_f90 (int *u, char *c, int clen);
extern int __fgetc_f77 (int *u, char *c, int clen);
extern int __fputc_f90 (int *u, char *c, int clen);
extern int __fputc_f77 (int *u, char *c, int clen);
extern int __fseek_f90 (int *u, int *off, int *from);
extern int __fseek_f77 (int *u, int *off, int *from);
extern int __fseek64_f90 (int *u, long long *off, int *from);
extern int __fseek64_f77 (int *u, long long *off, int *from);
extern int __fstat_f90 (int *u, int *stbuf);
extern int __fstat_f77 (int *u, int *stbuf);
extern int __fstat64_f90 (int *u, long long *stbuf);
extern int __fstat64_f77 (int *u, long long *stbuf);
extern int __isatty_f90 (int *u);
extern int __isatty_f77 (int *u);
extern long long __ftell64_f90 (int *u);
extern long long __ftell64_f77 (int *u);
extern void __ttynam_f90 (char *name, int strlen, int *u);
extern void __ttynam_f77 (char *name, int strlen, int *u);
extern int __usdumplock_f90 (void *l, int *u, char *str, int len);
extern int __usdumplock_f77 (void *l, int *u, char *str, int len);
extern int __usdumpsema_f90 (void *s, int *u, char *str, int len);
extern int __usdumpsema_f77 (void *s, int *u, char *str, int len);
extern void __flush_f90 (int *u, int *istat);
extern int __flush_f77 (int *u);


/* Here are the wrappers */
					 
extern int
fgetc_(int *u, char *c, int clen)
{
   SELECT(u,fgetc,(u,c,clen));
}

extern int 
fputc_(int *u, char *c, int clen)
{
   SELECT(u,fputc,(u,c,clen));
}

extern int
fseek_(int *u, int *off, int *from)
{
   SELECT(u,fseek,(u,off,from));
}

extern int
fseek64_(int *u, long long *off, int *from)
{
   SELECT(u,fseek64,(u,off,from));
}

extern int 
fstat_(int *u, int *stbuf)
{
   SELECT(u,fstat,(u,stbuf));
}

extern int 
fstat64_(int *u, long long *stbuf)
{
   SELECT(u,fstat64,(u,stbuf));
}


extern int 
isatty_ (int *u)
{
   SELECT(u,isatty,(u));
}

extern long long
ftell64_(int *u)
{
   SELECT(u,ftell64,(u));
}

extern int ftell_ (int *u)
{
   int r;
   r = ftell64_(u);
   return (r);
}

extern void 
ttynam_ (char *name, int strlen, int *u)
{
   if (f90_or_f77(u)) __ttynam_f90(name,strlen,u);
   else __ttynam_f77(name,strlen,u);
}


extern int
usdumplock_(void *l, int *u, char *str, int len)
{
   SELECT(u,usdumplock,(l,u,str,len));
}

extern int
usdumpsema_(void *s, int *u, char *str, int len) {
   SELECT(u,usdumpsema,(s,u,str,len));
}

extern int
flush_ (int *u)
{
   int istat;
   if (f90_or_f77(u)) {
      __flush_f90(u,&istat);
      return (istat);
   } else {
      return (__flush_f77(u));
   }
}
#endif
