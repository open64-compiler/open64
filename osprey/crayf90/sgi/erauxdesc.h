/*

  Copyright (C) 2000, 2001 Silicon Graphics, Inc.  All Rights Reserved.

  This program is free software; you can redistribute it and/or modify it
  under the terms of version 2 of the GNU General Public License as
  published by the Free Software Foundation.

  This program is distributed in the hope that it would be useful, but
  WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  

  Further, this software is distributed without any warranty that it is
  free of the rightful claim of any third person regarding infringement 
  or the like.  Any license provided herein, whether implied or 
  otherwise, applies only to this software file.  Patent licenses, if 
  any, provided herein do not apply to combinations of this program with 
  other software, or any other product whatsoever.  

  You should have received a copy of the GNU General Public License along
  with this program; if not, write the Free Software Foundation, Inc., 59
  Temple Place - Suite 330, Boston MA 02111-1307, USA.

  Contact information:  Silicon Graphics, Inc., 1600 Amphitheatre Pky,
  Mountain View, CA 94043, or:

  http://www.sgi.com

  For further information regarding this notice, see:

  http://oss.sgi.com/projects/GenInfo/NoticeExplan

*/



#ifndef erauxdesc_INCLUDED
#define erauxdesc_INCLUDED

#include "erfe90.h"

ERROR_DESC EDESC_FE[] = {
  { EC_Multiple_Initialization,	EM_User | ES_WARNING,	RAG_EN_NONE,
    "Multiple DATA initialization of storage for %s. Some initializations ignored.",
    1, ET_STRING, 0,0,0,0,0 },
  { EC_Makedepend_Error,	EM_User | ES_ERRABORT,	RAG_EN_NONE,
    "Error making make dependency file: %s",
    1, ET_STRING, 0,0,0,0,0 },
  { EC_Unknown_Mpsched,	        EM_User | ES_WARNING,	RAG_EN_NONE,
    "Unknown value for -mp_schedtype: %s",
    1, ET_STRING, 0,0,0,0,0 },
  { EC_IEEE_Intrinsic_Warning,	EM_User | ES_WARNING,	RAG_EN_NONE,
    "Optimization may move floating-point computations relative\nto the %s intrinsic, possibly resulting in different behavior.",
    1, ET_STRING, 0,0,0,0,0 },

  /* All error descriptor lists must end with a -1 error code: */
  { -1,	0, RAG_EN_NONE, "", 0, 0,0,0,0,0,0 }

};


#endif
