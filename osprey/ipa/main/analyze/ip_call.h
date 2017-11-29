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

#ifndef cxx_ip_call_INCLUDED
#define cxx_ip_call_INCLUDED

#ifndef clone_INCLUDED
#include "clone.h"	    // for IPO_ADDR_HASH
#endif // clone_INCLUDED

/*---------------------------------------------------------------------*/
/* caller specific state information                                   */
/*---------------------------------------------------------------------*/
class CALLEE_STATE {

private:
    
    IPO_ADDR_HASH *addr_map;	    // maps for originals/copies of TY and ST

    INT _count;

public:

    CALLEE_STATE (IPO_ADDR_HASH *hash, INT c) {
	addr_map = hash;
	_count = c;
    };

    INT Get_count() { return _count;};
    IPO_ADDR_HASH *Get_addr_map () { return addr_map; };
    void Set_count(INT c) { _count = c;};
    void Set_addr_map (IPO_ADDR_HASH *hash) { addr_map = hash; };
}; // CALLEE_STATE

#endif // cxx_ip_call_INCLUDED
