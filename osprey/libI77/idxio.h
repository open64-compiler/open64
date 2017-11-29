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


#ifndef __IDXIO_H__
#define __IDXIO_H__


#ident "$Revision: 1.1.1.1 $"

#include <stdio.h>
#include <cmplrs/fio.h>

#if defined(_LANGUAGE_C) || defined(_LANGUAGE_C_PLUS_PLUS)
/* PROTOTYPES */
extern int idxopen(unit *, char *, int, flag);
extern int idxclose(unit *, flag);
extern int idxread(unit *);
extern int iscleanup(void);
extern void s_idxwrite(unit *);
extern int idxwrite(unit *);
extern int idxrewrite(unit *);
extern int dokey(int, int);
extern int f_del(alist *);
extern int f_del_mp (alist *a);
extern int f_unl(alist *);
extern int f_unl_mp (alist *a);
extern int s_xsue(cilist *);
extern int s_xsue_mp(cilist *, unit **);
extern int s_xsle(cilist *);
extern int s_xsle_mp(cilist *, unit **);
extern int s_xsfe(cilist *);
extern int s_xsfe_mp(cilist *, unit **);
extern int e_xsue(void);
extern int e_xsue_mp(unit **);
extern int f_del64(alist *);
extern int f_del64_mp (alist *a);
extern int f_unl64(alist *);
extern int f_unl64_mp (alist *a);
extern int s_xsue64(cilist64 *);
extern int s_xsue64_mp(cilist64 *, unit **);
extern int s_xsle64(cilist64 *);
extern int s_xsle64_mp(cilist64 *, unit **);
extern int s_xsfe64(cilist64 *);
extern int s_xsfe64_mp(cilist64 *, unit **);
extern int e_xsue64(void);
extern int e_xsue64_mp(unit **);

#endif /* C || C++ */

#endif /* !__IDXIO_H__ */





