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


#ifndef __RDFMT_H__
#define __RDFMT_H__


#ident "$Revision: 1.1.1.1 $"

#include <stdio.h>
#include <cmplrs/fio.h>

#if defined(_LANGUAGE_C) || defined(_LANGUAGE_C_PLUS_PLUS)
/* PROTOTYPES */
extern int s_rsfi(icilist *);
extern int s_rsfi_mp(icilist *, unit **);
extern int __s_rsfi_com (icilist64 *a, unit **fu, int f90sw);
extern int rd_ed(unit *, struct f77syl *, char *, ftnlen, ftnint);
extern int rd_ned(unit *, struct f77syl *);
extern int rd_I(unit *, uinteger *, long, ftnlen);
extern int rd_OZ(unit *, unsigned char *, long, ftnlen, int);
extern int rd_Q(unit *, uinteger *, ftnlen);
extern int rd_L(unit *, uinteger *, long, ftnlen);
extern int rd_F(unit *, ufloat *, long, long, ftnlen);
extern int rd_A(unit *, char *, ftnlen);
extern int rd_AW(unit *, char *, long, ftnlen);
extern int rd_H(unit *, long);
extern int rd_POS(unit *, char *);
/* extern int c_si(icilist *, unit *); declared in iio.h */

extern int rd_B (unit *ftnunit, unsigned char *n, long w, ftnlen len);


extern int s_rsfi64 (icilist64 *a);
extern int s_rsfi64_mp (icilist64 *a, unit **fu);


#endif /* C || C++ */

#endif /* !__RDFMT_H__ */





