/*
 * Copyright 2004, 2005, 2006 PathScale, Inc.  All Rights Reserved.
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


#ifndef pf_common_INCLUDED
#define pf_common_INCLUDED

extern MEM_POOL *PF_mpool;
extern BOOL Debug_Prefetch;
extern FILE* TFile;
extern BOOL Verbose_Prefetch;

#ifdef  PF_PrintDebugging
#define PF_PRINT(x) if (Debug_Prefetch) { x; }
#else
#define PF_PRINT(x)
#endif

#ifdef Is_True_On
#define VB_PRINT(x) if (Verbose_Prefetch) { x; }
#else
#define VB_PRINT(x)
#endif

/* in verbose mode, the pretty printing indent */
extern UINT vb_num_indent;
extern BOOL vb_print_split;
#define vb_print_indent { for (INT i=0;i<vb_num_indent; i++) printf (" "); }

/* in list mode, the pretty printing indent */
extern UINT ls_num_indent;
#define ls_print_indent { for (INT i=0;i<ls_num_indent; i++) fprintf (LNO_Analysis, " "); }
extern void dump_tree (WN *wn);
extern void dump_wn (WN *wn);

#define PF_MAX_STRIDE 33

#endif // pf_common_INCLUDED
