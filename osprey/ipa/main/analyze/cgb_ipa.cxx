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
#include <stdio.h>
#if defined(BUILD_OS_DARWIN)
#include <darwin_elf.h>
#else /* defined(BUILD_OS_DARWIN) */
#include <elf.h>
#endif /* defined(BUILD_OS_DARWIN) */
#include <sys/elf_whirl.h>
#include <sys/types.h>
#include <ctype.h>
#include <limits.h>
#include "wn.h"
#include "wn_map.h"
#include "wn_util.h"
#include "dwarf_DST.h"
#include "ipc_file.h"
#include "wb_util.h"
#include "cg_browser.h"
#include "cgb.h"
#include "cgb_ipa.h"
#include "opt_du.h"
#include "opt_alias_interface.h"
#include "dep_graph.h"
#include "wb.h"

CG_BROWSER cgb_ipa; 

extern void CGB_IPA_Initialize(IPA_CALL_GRAPH* ipa_cg)
{ 
  CGB_Set_Phase(CGBP_IPA); 
  WB_Set_Phase(WBP_IPA);
  CGB_Initialize(&cgb_ipa, ipa_cg); 
} 

extern void CGB_IPA_Terminate()
{ 
  CGB_Set_Phase(CGBP_NONE); 
  WB_Set_Phase(WBP_NONE);
  CGB_Terminate(&cgb_ipa); 
} 

extern void s_cg_ipa_debug(const char init_buffer[])
{ 
  cgb_ipa.Sdebug(init_buffer); 
} 

