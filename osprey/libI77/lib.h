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


#ifndef __LIB_H__
#define __LIB_H__


#ident "$Revision: 1.1.1.1 $"

#include <stdio.h>
#include <cmplrs/fio.h>

#if defined(_LANGUAGE_C) || defined(_LANGUAGE_C_PLUS_PLUS)
/* PROTOTYPES */
extern int setcilist(cilist *, int, char *, int, int, int);
extern int setolist(olist *, int, char *, char *, char *, int, char *, int);
extern int stcllist(cllist *, int, char *, int);
extern int setalist(alist *, int, int);

extern int setcilist64(cilist64 *, ftnint, char *, XINT64, int, int);
extern int setolist64(olist64 *, ftnint, char *, char *, char *, int, char *, int);
/* extern int setcllist64(cllist64 *, ftnint, char *, int); */
/* extern int setalist64(alist64 *, ftnint, int); */

#endif /* C || C++ */

#endif /* !__LIB_H__ */





