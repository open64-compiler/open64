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


#ifndef __OPEN_H__
#define __OPEN_H__


#ident "$Revision: 1.1.1.1 $"

#include <stdio.h>
#include <cmplrs/fio.h>

#if defined(_LANGUAGE_C) || defined(_LANGUAGE_C_PLUS_PLUS)
/* PROTOTYPES */
 
extern int f_open1(olist *, int *);
extern int f_open0(olist *, int *);
extern int fk_open(int, int, ftnint);
extern int absent(char *, int);
extern int inc_var(ftnintu *, ftnint, int);
extern int set_var(ftnintu *, ftnint, int, long long);
extern int f_dfnf(struct dfnf_struct *);
extern int f_dfnf1(struct dfnf_struct *, ftnint *);
extern int f_duped (olist64 *, unit *, unit **);

extern int f_open064 (olist64 *a, int *mask);
extern int f_open064x (olist64 *a, XINT xmask);
extern int f_open064x_mp (olist64 *a, XINT xmask);


#endif /* C || C++ */

#endif /* !__OPEN_H__ */
