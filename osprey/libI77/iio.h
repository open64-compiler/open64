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


#ifndef __IIO_H__
#define __IIO_H__


#ident "$Revision: 1.1.1.1 $"

#include <stdio.h>
#include <cmplrs/fio.h>

#if defined(_LANGUAGE_C) || defined(_LANGUAGE_C_PLUS_PLUS)
/* PROTOTYPES */
extern int z_ungetc(unit *, int);
extern int z_getc(unit *);
extern int z_gets(unit *, char *, int, char);
extern int z_putc(unit *, XINT, char, char *);
extern int z_wnew(unit *);
extern int z_rnew(unit *);
extern int s_wsfi(icilist *);
extern int s_wsfi_mp(icilist *, unit**);
extern int z_rSL(unit *);
extern int z_wSL(unit *);
extern int y_ierr(unit *);
extern int e_rsfi();
extern int e_wsfi();
extern int e_rsfi_mp(unit **);
extern int e_wsfi_mp(unit **);

extern int s_wsfi64(icilist64 *);
extern int s_wsfi64_mp(icilist64 *, unit**);
extern int e_rsfi64();
extern int e_wsfi64();
extern int e_rsfi64_mp(unit **);
extern int e_wsfi64_mp(unit **);
extern int c_si(icilist64 *, unit *);

extern XINT icnum, icpos;
extern char *icptr, *icend;
#endif /* C || C++ */

#endif /* !__IIO_H__ */
