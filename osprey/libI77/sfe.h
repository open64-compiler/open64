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


#ifndef __SFE_H__
#define __SFE_H__


#ident "$Revision: 1.1.1.1 $"

#include <stdio.h>
#include <cmplrs/fio.h>

#if defined(_LANGUAGE_C) || defined(_LANGUAGE_C_PLUS_PLUS)
/* PROTOTYPES */
extern int e_rsfe(void);
extern int e_rsfe_mp(unit **);
extern int c_sfe (cilist64 *a, unit **fu);
extern int e_wsfe(void);
extern int e_wsfe_mp(unit **);
extern int e_xsfe(void);
extern int e_xsfe_mp(unit **);
extern int e_wsue(void);
extern int e_wsue_mp(unit **);

extern int e_rsfe64(void);
extern int e_rsfe64_mp(unit **);
extern int e_wsfe64(void);
extern int e_wsfe64_mp(unit **);
extern int e_xsfe64(void);
extern int e_xsfe64_mp(unit **);
extern int e_wsue64(void);
extern int e_wsue64_mp(unit **);

#endif /* C || C++ */

#endif /* !__SFE_H__ */
