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


#ifndef __GLOBALS_H__
#define __GLOBALS_H__


#ident "$Revision: 1.1.1.1 $"

#include <stdio.h>
#include <cmplrs/fio.h>
#include "fmt.h"
#include "lio.h"

#if defined(_LANGUAGE_C) || defined(_LANGUAGE_C_PLUS_PLUS)
/* PROTOTYPES */
extern int (*f77ungetn)(int);	/* for f77formatted io */
extern int (*f77doed)(unit *, struct f77syl *, char *, ftnlen, ftnint);
extern int (*f77doned)(struct f77syl *);
extern int (*dowrite)(void);
extern int (*f77do_unf)();
extern int (*f77doend)(void);
extern int (*f77donewrec)(void);
extern int (*f77dorevert)(void);
extern int (*f77lioproc)(unit *, ftnint *, flex *, ftnlen, ftnint);
extern int (*_libisam_iscleanup)();
extern int (*_libisam_idxrd)();
extern int (*_libisam_idxcls)();
extern int (*_libisam_idxwrt)(void);


extern unit *f77units;	/* unit table *//* sjc #1963 11Dec87 */
extern int  mxunit;	/* size of unit table *//* sjc #1963
				 * 11Dec87 */
extern flag f77init;	/* 0 on entry, 1 after
			 * initializations */
extern cilist  *f77elist;	/* active f77external io list */
extern icilist *f77svic;	/* active internal io list */
extern flag     f77reading;	/* 1 if f77reading, 0 if writing */
extern flag     f77cplus, f77cblank;
extern char    *f77fmtbuf;
extern flag     f77external;/* 1 if f77external io, 0 if internal */
extern flag     f77formatted;	/* 1 if f77formatted io, 0 if
					 * unformatted */
extern FILE    *f77cf;	/* current file */
extern unit    *f77curunit;	/* current unit */
/* extern int      f77reclen;	/* record length */
extern int      f77recpos;	/* place in current record */
extern int      f77recend;	/* end of current record */
extern int      f77fio_size;/* size of record buffer */
extern char    *f77fio_buf;	/* record buffer */
extern int      f77cursor, f77scale;
extern ftnint   errluno;	/* logical unit of last error */
extern flag     f77workdone, f77nonl;
extern int      icnum, icpos;
extern char    *icptr, *icend;
extern flag     lquit;
extern int      lcount;
extern int      overflowed;
extern int      space_assigned;

#ifdef I90
extern int f90sw;
extern int f90eor;
extern int f90npad;
extern int f90nadv;
#endif

#endif /* C || C++ */

#endif /* !__GLOBALS_H__ */
