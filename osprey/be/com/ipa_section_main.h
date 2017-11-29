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


//* -*-Mode: c++;-*- (Tell emacs to use c++ mode) */

#ifndef ipa_section_main_INCLUDED
#define ipa_section_main_INCLUDED

#ifndef ipa_section_INCLUDED
#include "ipa_section.h"
#endif

extern INT 
Locate_symbol(LOOP_SYMBOL_ARRAY* syms, 
              SYSTEM_OF_EQUATIONS* soe, 
              const LOOP_SYMBOL& symbol);


extern void
Add_to_SOE(PROJECTED_REGION* a, 
           const INT pos, 
           SYSTEM_OF_EQUATIONS* soe,
	   const BOOL convert_equation, 
	   LOOP_SYMBOL_ARRAY* sym, 
           INT depth, 
           BOOL trace);

#ifndef IPA_SUMMARY
extern void Init_IPA_Print_Arrays (IPA_NODE* node);
extern INT IPA_Ivar_Count;
#endif

#endif 
