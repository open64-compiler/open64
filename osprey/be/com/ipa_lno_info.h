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



#ifndef _IPA_LNO_INFO
#define _IPA_LNO_INFO

// Values of sh_info field for section type SHT_MIPS_IPALNO

#define IPA_NULL             0x0
#define IPA_PROCEDURE        0x1
#define IPA_PROJECTED_REGION 0x2
#define IPA_PROJECTED_ARRAY  0x3
#define IPA_TERM_ARRAY       0x4
#define IPA_IVAR             0x5
#define IPA_FORMAL           0x6
#define IPA_GLOBAL           0x7
#define IPA_VALUE	     0x8
#define IPA_EXPR	     0x9
#define IPA_STRINGS          0xa
#define IPA_REVISION 	     0xb 

// Names of these sections

#define IPA_PROCEDURE_NAME          "IPA.procedures"
#define IPA_PROJECTED_REGION_NAME   "IPA.regions"
#define IPA_PROJECTED_ARRAY_NAME    "IPA.projected_arrays"
#define IPA_TERM_ARRAY_NAME         "IPA.terms"
#define IPA_IVAR_NAME               "IPA.ivars"
#define IPA_FORMAL_NAME             "IPA.formals"
#define IPA_GLOBAL_NAME             "IPA.commons"
#define IPA_VALUE_NAME              "IPA.values"
#define IPA_EXPR_NAME               "IPA.exprs"
#define IPA_STRINGS_NAME            "IPA.strings"
#define IPA_REVISION_NAME 	    "IPA.revision"

#define IPA_LNO_SECTION_COUNT 	12

#endif /* IPA_LNO_INFO */

