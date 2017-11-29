/*
 * Copyright 2003, 2004, 2005, 2006 PathScale, Inc.  All Rights Reserved.
 */

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


#include <stdint.h>
#include <sys/types.h>
#if defined(BUILD_OS_DARWIN)
#include <darwin_elf.h>
#else /* defined(BUILD_OS_DARWIN) */
#include <elf.h>
#endif /* defined(BUILD_OS_DARWIN) */
#include <ctype.h>
#include "wn.h"
#include "wn_map.h"
#include "wn_util.h"
#include <stdio.h>
#include "opt_du.h"
#include "opt_alias_mgr.h"
#include "dep_graph.h"
#include "ir_reader.h"
#include "wb_util.h"
#include "wb_buffer.h"
#include "wb_carray.h"
#include "wb_browser.h"
#include "wb.h"
#include "wb_ipl.h"

WB_BROWSER wb_ipa; 

extern void WB_IPA_Initialize(WN* wn_global,
			      PU* pu)
{ 
  WB_Initialize(&wb_ipa, wn_global, pu, NULL, NULL);
} 

extern void WB_IPA_Terminate()
{ 
  WB_Terminate(&wb_ipa); 
} 

extern void s_ipa_debug(const char init_buffer[])
{ 
  wb_ipa.Sdebug(init_buffer); 
} 

