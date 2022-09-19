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


/*-*-c++-*-*/
// ====================================================================
//
// Module: options_stack.h
// $Revision: 1.1.1.1 $
// $Date: 2005/10/21 19:00:00 $
// $Author: marcel $
// $Source: /proj/osprey/CVS/open64/osprey1.0/common/util/options_stack.h,v $
//
// Revision history:
//  23-SEP-97 dahl - Original Version
//
// Description:
//	
// ====================================================================

#ifndef	options_stack_INCLUDED
#define	options_stack_INCLUDED

#include "cxx_template.h"	// STACK template

// number of bytes needed to store all options
// as we save them, we count them, but if we are over we need to recompile
#define OPTIONS_SIZE	2720

class OPTIONS_STACK {
 private:
  BOOL _trace;
  STACK<char *> _options_stack;

  BOOL Trace(void) const { return _trace; }
  INT32 str2argv(char *, char ***, MEM_POOL *);

 public:
  OPTIONS_STACK(MEM_POOL *pool) : _options_stack(pool) {
    _options_stack.Clear();
    _trace = Get_Trace(TP_MISC, 0x80); /* full command line tracing */
    Is_Trace(Trace(), (TFile, "OPTIONS_STACK::OPTIONS_STACK\n"));
  }
  ~OPTIONS_STACK(void) { }

  // take this out once bug 530832 is fixed!!
  void Init(void) {
    printf("This is a bug because it is repeated for every DSO"
	   "even when compiled with -INLINE:=0\n");
  }

  // push and pop with copying of options
  void Push_Current_Options(void);
  void Pop_Current_Options(void);

  // Process pragma options
  void Process_Pragma_Options(char *);
};

#endif
