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


#ifndef ipl_main_INCLUDED
#define ipl_main_INCLUDED

#ifdef __cplusplus
extern "C" {
#endif

/* General progress trace: */
extern BOOL Trace_IPA;
extern BOOL Trace_Perf;
extern BOOL Debug_On;
extern BOOL Do_Const;
extern BOOL Do_Par;
extern BOOL Do_Split_Commons;
extern BOOL Do_Split_Commons_Set;
extern BOOL IPL_Enable_Unknown_Frequency;
extern BOOL IPL_Generate_Elf_Symtab;    
#ifdef KEY
extern UINT32 IPL_Ignore_Small_Loops;
#endif
extern struct DU_MANAGER *Ipl_Du_Mgr;
extern struct ALIAS_MANAGER *Ipl_Al_Mgr;

#ifdef __cplusplus
}
#endif

extern BOOL DoPreopt;

extern WN_MAP Summary_Map;
extern WN_MAP Stmt_Map;

#endif // ipl_main_INCLUDED
