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


/* -*-Mode: c++;-*- (Tell emacs to use c++ mode) */
/* ====================================================================
 * ====================================================================
 *
 * Module: ipc_daVinci.h
 * $Revision: 1.1.1.1 $
 * $Date: 2005/10/21 19:00:00 $
 * $Author: marcel $
 * $Source: /proj/osprey/CVS/open64/osprey1.0/ipa/common/ipc_daVinci.h,v $
 *
 * Description:
 *	Class definition for the interface to daVinci.
 *
 * ====================================================================
 * ====================================================================
 */

#ifndef ipc_daVinci_INCLUDED
#define ipc_daVinci_INCLUDED

class daVinci {

private:
    
    MEM_POOL *m;
    GRAPH *g;
    BOOL display_ok;
    pid_t pid;

    FILE *to_display, *from_display;

    void Graph_To_Term (NODE_INDEX, mUINT8 *);

    void wait_for (const char *str = "ok\n");

    void cleanup ();
    
public:

    daVinci (GRAPH *, MEM_POOL *);
    
    void Translate_Call_Graph (void);

    void Mark_Used (NODE_INDEX);
    void Mark_Deleted (NODE_INDEX);
    void Mark_Inlined_Caller (NODE_INDEX);
    void Mark_Inlined_Callee (NODE_INDEX);
    void Mark_Inlined_Deleted (NODE_INDEX);

}; // daVinci

extern daVinci *cg_display;

#endif /* ipc_daVinci_INCLUDED */
