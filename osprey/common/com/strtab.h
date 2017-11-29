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
#ifndef strtab_INCLUDED
#define strtab_INCLUDED
/* ====================================================================
 * ====================================================================
 *
 * Module: strtab.h
 * $Revision: 1.1.1.1 $
 * $Date: 2005/10/21 19:00:00 $
 * $Author: marcel $
 * $Source: /proj/osprey/CVS/open64/osprey1.0/common/com/strtab.h,v $
 *
 * Description:
 *
 * String table external interface.
 *
 * ====================================================================
 * ====================================================================
 */



#ifndef symtab_idx_INCLUDED
#include "symtab_idx.h"
#endif

// New hash function to accommodate unsigned long long, STL hash does
// not have one.
namespace __new_hash
{
template <class _Key> struct hash { };

  template<> struct hash<unsigned long long> {
    size_t operator()(unsigned long long __x) const { return __x; }
  }; 
}

// From a user point of view, the string table is a collection of unique
// strings, each of which can be indentified by STR_IDX.  The actual
// implementation is transparent to the user.

// init an empty table; use by producer (e.g., front end)
// size is string buffer size in bytes
extern void
Initialize_Strtab (UINT32 size);

// initialized the string table with the strtab from an input file
extern void
Initialize_Strtab (const char *buf, UINT32 size);

// add string to string table
extern STR_IDX
Save_Str (const char *str);

// concat two strings and then add to string table
extern STR_IDX
Save_Str2 (const char *s1, const char *s2);

// concat two strings and an int and add to string table
extern STR_IDX
Save_Str2i (const char *s1, const char *s2, UINT i);

extern char *
Index_To_Str (STR_IDX idx);

struct STRING_TABLE
{
    char &operator[] (STR_IDX idx) {
	return *Index_To_Str (idx);
    }
};

// New code should use Str_Table[idx] to get the string from an index
extern STRING_TABLE Str_Table;

extern STR_IDX
STR_Table_Size ();

// for compatibility
inline char *
Index_To_Str (STR_IDX idx, void *)	{ return Index_To_Str (idx); }

#define Str_To_Index(s,t)	(s)

// for character array table
extern UINT32
TCON_strtab_size ();

extern char *
TCON_strtab_buffer ();

extern void
Initialize_TCON_strtab (UINT32 size);

extern void
Initialize_TCON_strtab (const char *buf, UINT32 size);

// (similar to strncpy) s1 might not be null-termianted
extern UINT32
Save_StrN (const char *s1, UINT32 len);

extern char *
Index_to_char_array (UINT32 idx);

#ifdef MONGOOSE_BE

#include <ext/hash_map>
using __gnu_cxx::hash_map;

#ifndef mempool_allocator_INCLUDED
#include "mempool_allocator.h"
#endif

// merge in a strtab read from an input file with the current string table;
// returns a mapping from the string indices of the "buf" to the 
// corresponding string indices in the merged table.

class STR_IDX_MAP {
private:
  typedef hash_map<STR_IDX, STR_IDX, __new_hash::hash<STR_IDX>, std::equal_to<STR_IDX>,
      mempool_allocator<std::pair<const STR_IDX, STR_IDX> > > rep_type;
  rep_type rep;  

public:

  typedef rep_type::allocator_type allocator_type;

  STR_IDX_MAP(rep_type::size_type n = 0,
              rep_type::hasher hf = rep_type::hasher(),
              rep_type::key_equal eq  = rep_type::key_equal(),
              rep_type::allocator_type a = rep_type::allocator_type())
    : rep(n, hf, eq, a)
    {}

  STR_IDX& operator[](STR_IDX idx) { return rep[idx]; }
  STR_IDX operator[](STR_IDX idx) const {
    rep_type::const_iterator i = rep.find(idx);
    return i != rep.end() ? i->second : STR_IDX(0);
  }
};

extern void
Merge_Strtab (const char *buf, UINT32 size, STR_IDX_MAP& map);

extern void
Merge_TCON_Strtab (const char *buf, UINT32 size, STR_IDX_MAP& map);

#endif // MONGOOSE_BE


#endif /* strtab_INCLUDED */
