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
#ifndef cxx_sparse_bv_INCLUDED
#define cxx_sparse_bv_INCLUDED

#include <vector>

using std::vector;

#define mUINT64_BIT	64		// number of bits in an mUINT64

class SPARSE_BV
{
private:
    // the sparse bit vector is represented as a vector of pointers to
    // small buffers defined as array of mUINT64.
    // The number of bits in each small buffer is specified by buffer_size, 
    // which must be a multiple of 64 (number of bits in mUINT64).
    typedef vector<UINT32> SPARSE_BV_BUFFER;
    typedef SPARSE_BV_BUFFER::iterator BUFFER_ITER;

    typedef vector<mUINT64> SPARSE_BV_DATA;

private:
    SPARSE_BV_BUFFER* buffer;		// the bit vector map
    SPARSE_BV_DATA* data;		// actual data buffer
private:
    
    // set the bit corresponding to the given index
    void Set_Bit (UINT32 index);

    // core loop in union of two vectors (error checking done by caller)
    void Union_2_Core (const SPARSE_BV& vec, BUFFER_ITER vec_iter,
		       BUFFER_ITER iter); 

    // core loop for Union_2_diff (see below)
    BOOL Union_2_Diff_Core (const SPARSE_BV& vec);

public:
    
    SPARSE_BV () : buffer (NULL) {}

    SPARSE_BV (const SPARSE_BV& bv) {
	if (bv.buffer) {
	    buffer = CXX_NEW (SPARSE_BV_BUFFER (*bv.buffer), Malloc_Mem_Pool);
	    data = CXX_NEW (SPARSE_BV_DATA (*bv.data), Malloc_Mem_Pool);
	} else {
	    buffer = NULL;
	    data = NULL;
	}
    }

    ~SPARSE_BV () {
	if (buffer != 0) {
	    CXX_DELETE (data, Malloc_Mem_Pool);
	    CXX_DELETE (buffer, Malloc_Mem_Pool);
	}
    }

    // inlined version for fast path of Set_Bit
    void Set (UINT32 index) {
	if (buffer && index < buffer->size () * mUINT64_BIT) {
	    UINT32 idx = (*buffer)[index / mUINT64_BIT];
	    if (idx != 0) {
		(*data)[idx] |= 1LL << (index % mUINT64_BIT);
		return;
	    }
	}

	// do heavy weight insertion
	Set_Bit (index);
    }

    // return if the specified bit is set
    BOOL Is_Set (UINT32 index) const {
	if (buffer == NULL || index >= buffer->size () * mUINT64_BIT)
	    return FALSE;
	UINT32 idx = (*buffer)[index / mUINT64_BIT];
	if (idx == 0)
	    return FALSE;
	return ((*data)[idx] & (1LL << (index % mUINT64_BIT))) != 0LL;
    }
	    

    // destructive union of the specified bit vector with this one
    void Union_2 (const SPARSE_BV& vec);

    // similar to Union_2, but return TRUE if "this" bit vector is modified
    BOOL Union_2_diff (const SPARSE_BV& vec) {
	if (vec.buffer == NULL)
	    return FALSE;
	else if (buffer != NULL) {
	    return Union_2_Diff_Core (vec);
	} else {
	    Union_2 (vec);
	    return TRUE;
	}
    }

#ifdef Is_True_On
    // debugging and tracing

    // verify consistency of the bit vector
    void Verify () const;

    // return the number of bytes used to store the bit vector.  Overhead
    // in memory allocation is not counted.
    UINT32 Memory_Size () const {
	if (buffer == NULL)
	    return 0;
	return buffer->size() * sizeof(UINT32) + data->size() * sizeof(mUINT64);
    }

    void Print (FILE* f) const {
	if (buffer == NULL)
	    return;
	fprintf (f, "Size = %d\n", Memory_Size ());
	UINT32 index = 0;
	for (BUFFER_ITER first (buffer->begin ()); first != buffer->end ();
	     ++first) {
	    if (*first != 0) {
		fprintf (f, "0x%x: %016llx\n", index, (*data)[*first]);
	    }
	    index += mUINT64_BIT;
	}
    }
#endif
};
    

#endif // cxx_sparse_bv_INCLUDED
