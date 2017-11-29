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


#ifndef cgb_INCLUDED
#define cgb_INCLUDED

#ifndef cgb_browser_INCLUDED
#include "cg_browser.h"
#endif

enum CGB_PHASE {
  CGBP_NONE, 
  CGBP_IPA 
}; 

extern void CGB_Set_Phase(CGB_PHASE WB_Phase); 

extern void CGB_Initialize(CG_BROWSER* cgb, IPA_CALL_GRAPH* ipa_cg);

extern void CGB_Terminate(CG_BROWSER* cgb);

extern "C" void cg_sdebug(const char init_buffer[]);

extern void scgb(const char init_buffer[]);

extern void cg_debug();

extern void cgb();

#endif /* cgb_INCLUDED */

