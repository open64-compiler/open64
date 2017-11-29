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

#ifndef ipo_clone_INCLUDED
#define ipo_clone_INCLUDED

// to avoid extra includes; 
struct ip_file_hdr;

#ifndef cxx_base_INCLUDED
#include "cxx_base.h"
#endif


//-------------------------------------------------------------------
// use an array of such lists to store st entries that have already
// been copied during the cloning process
//-------------------------------------------------------------------
class CLONED_NODE: public SLIST_NODE {
private:
  ST* _st, *_copy_st;
public:
  DECLARE_SLIST_NODE_CLASS(CLONED_NODE);
  void Set_orig_st(ST* s) { _st = s;};
  void Set_copy_st(ST* s) { _copy_st = s;};
  ST* Get_orig_st() { return _st;};
  ST* Get_copy_st() { return _copy_st;};

  CLONED_NODE(ST* orig, ST* copy);
};

class CLONED_LIST: public SLIST {
public:
  DECLARE_SLIST_CLASS(CLONED_LIST, CLONED_NODE);
  void Append(ST*,ST*, MEM_POOL *m);
  ST* Lookup(ST* orig_st); 
};


class CLONED_LIST_ITER: public SLIST_ITER {

public:
  DECLARE_SLIST_ITER_CLASS(CLONED_LIST_ITER,CLONED_NODE,CLONED_LIST);
};

// Main entry points to cloning

extern void IPO_Clone (IPA_NODE* orig_node, IPA_NODE* clone_node);

extern WN* IPO_Copy_Tree (WN *);
 
#if (!defined(_STANDALONE_INLINER) && !defined(_LIGHTWEIGHT_INLINER))

/* this routine is responsible for updating the st entry in the call
   to reflect the call to the clone. */
extern void Clone_update_st(WN *, ST_IDX, SYMTAB_IDX, MEM_POOL*,
                            IPA_NODE *);
#endif
#endif /* ipo_clone_INCLUDED */
