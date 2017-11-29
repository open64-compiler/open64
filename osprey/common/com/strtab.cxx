/*
 * Copyright 2003, 2004, 2005 PathScale, Inc.  All Rights Reserved.
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



#ifdef USE_PCH
#include "common_com_pch.h"
#endif /* USE_PCH */
#pragma hdrstop

#include <alloca.h>
#include <strings.h>

#include <ext/hash_map>			// stl hash table

#include "defs.h"
#include "errors.h"
#include "cxx_memory.h"			// for CXX_NEW

#include "strtab.h"

// The string table is implmeneted as a single character buffer, re-allocated
// if necessary.  Byte 0 of this buffer is alwasy NULL.  Each string is
// copied to the buffer once.  Duplicates are not entered.  A simple hash
// table is used to for quick lookup.  The hash table is a fixed size
// array of STR_BUCKET, each of which a dynamic array of <idx,size> pair.
// The size field is truncated to 8 bits.  It is used for 
// quick comparision to minimize the need to call strcmp.

// we support two types of strings: C-style null-terminated string and
// character array with a specified size (may contain null character within
// the array.

typedef STR_IDX STR_INDEX;

#if defined(linux) || defined(BUILD_OS_DARWIN)

#define STR_INDEX_size(xxx) (xxx & 0xff)
#define STR_INDEX_index(xxx) (xxx >> 8)

#else

static inline mUINT8
STR_INDEX_size (STR_INDEX idx)		{ return idx & 0xff; }
static inline STR_INDEX
STR_INDEX_index (STR_INDEX idx)		{ return idx >> 8; }

#endif /* linux */

static inline STR_INDEX
make_STR_INDEX (UINT32 size, STR_INDEX idx)
{
    // if string length larger than 0xff, just use 0xff
    if (size > 0xff)
	size = 0xff;
    return (STR_INDEX) ((idx << 8) | size);
}

struct NULL_TERMINATED_STRING
{
    static char *get_str (char *buffer) {
	return buffer;
    }

    static const char *get_str (const char *buffer) {
	return buffer;
    }

    static UINT32 get_length (const char *buffer) {
	return strlen (buffer);
    }

    static UINT32 get_buffer_length (UINT32 length) {
	return length + 1;		// length + null character
    }


    static void copy (const char *str, UINT32 length, char *buffer) {
	memcpy (buffer, str, length+1);
    }
};


union UNALIGN_INT32
{
    char ch[sizeof(UINT32)];
    UINT32 n;

    UNALIGN_INT32 (UINT32 x) : n (x) {}

    UNALIGN_INT32 (const char *s) {
	for (INT32 i = 0; i < sizeof(UINT32); ++i)
	    ch[i] = *s++;
    }
};


struct CHARACTER_ARRAY
{
    // If the character array is less then 0xff bytes, we store the size in
    // The First Byte Followed By The Array.  Otherwise, The First Byte Is Set
    // To 0xff And The Next 4 Bytes Hold The Size Of The Array As An Unsigned
    // Integer, And The Array Follows.

    static const char *get_str (const char *buffer) {
	if (*buffer != 0xff)
	    return buffer + 1;
	else
	    return buffer + sizeof(UINT32) + 1;
    }

    static char *get_str (char *buffer) {
	if (*buffer != 0xff)
	    return buffer + 1;
	else
	    return buffer + sizeof(UINT32) + 1;
    }

    static UINT32 get_length (const char *buffer) {
	if (*buffer != 0xff)
	    return *buffer;
	else {
	    UNALIGN_INT32 unalign (buffer + 1);
	    return unalign.n;
	}
    }

    static UINT32 get_buffer_length (UINT32 length) {
	return length < 0xff ? length + 1 : length + 1 + sizeof(UINT32);
    }

    static void copy (const char *str, UINT32 length, char *buffer) {
	if (length < 0xff) {
	    *buffer++ = length;
	} else {
	    *buffer++ = 0xff;
	    UNALIGN_INT32 unalign (length);
	    for (INT32 i = 0; i < sizeof(UINT32); ++i)
		*buffer++ = unalign.ch[i];
	}
	memcpy (buffer, str, length);
    }

}; // CHARACTER_ARRAY


template <class STR>
struct STR_TAB
{
    char *buffer;
    STR_IDX last_idx;
    STR_INDEX buffer_size;

    // the following 4 function objects are required for setting up the
    // hash table declared after them.

    // hash key, which is a pair of string pointer and string length
    struct hash_key {
	const char *str;
	mUINT32 size;

	hash_key () {}
	
	hash_key (const char *s, UINT32 l) : str (s), size (l) {}
	
	hash_key (const hash_key& p) : str (p.str), size (p.size) {}
    };


    // hash function
    struct hash {
	UINT32 operator() (const hash_key &key) const {
	    UINT32 h = 0;
	    const UINT32 length = key.size;
	    const char *str = key.str;
	    for (UINT32 i = 0; i < length; ++i)
		h = 5 * h + str[i];
	    return h;
	}
    };
	    

    // how the hashtable convert a STR_INDEX into a hash_key
    struct extract_key {
	const STR_TAB<STR>& strtab;

	extract_key (const STR_TAB<STR>& tab) : strtab (tab) {}
	
	hash_key operator() (STR_INDEX str_index) const {
	    const char *str = strtab.buffer + STR_INDEX_index (str_index);
	    if (STR_INDEX_size (str_index) < 0xff) {
		return hash_key (STR::get_str (str),
				      STR_INDEX_size (str_index));
	    } else {
		return hash_key (STR::get_str (str), STR::get_length (str));
	    }
	}
    };


    // equality of two hash_keys
    struct equal {
	BOOL operator() (const hash_key& key1, const hash_key& key2) const {
	    return (key1.size == key2.size &&
		    key1.str[0] == key2.str[0] &&
		    memcmp (key1.str, key2.str, key1.size) == 0);
	}
    };

    
    typedef __gnu_cxx::hashtable<STR_INDEX, hash_key, hash, extract_key, equal> HASHTABLE;

    HASHTABLE hash_table;

    // constructor

    STR_TAB (UINT bucket_size) : hash_table (bucket_size, hash (), equal (),
					     extract_key (*this)),
	buffer (NULL), last_idx (0), buffer_size (0) {}


    // operations
    
    void reserve (UINT32 size) {
	if (size > buffer_size - last_idx) {
	    // realloc
	    while (size > buffer_size - last_idx) {
		if (buffer_size < 0x100000)
		    buffer_size *= 2;
		else
		    buffer_size += 0x80000;
	    }
	    buffer = (char *) MEM_POOL_Realloc (Malloc_Mem_Pool, buffer, 0,
						buffer_size); 
	}
    }
    
    void init_hash ();

    STR_INDEX insert (const char *str, UINT32 size);

    STR_INDEX insert (const char *str) {
	return insert (str, strlen (str));
    }
    
    void copy_str (const char *str, UINT32 size);

}; // STR_TAB


template <class STR>
STR_INDEX
STR_TAB<STR>::insert (const char *str, UINT32 size)
{
    STR_INDEX index = last_idx;

    copy_str (str, size);

    STR_INDEX new_index = make_STR_INDEX (size, index);
    STR_INDEX old_index = hash_table.find_or_insert (new_index);

    if (new_index != old_index) {
	// already inserted, reset buffer and return
	last_idx = index;
	return STR_INDEX_index (old_index);
    } else
	return index;

} // STR_TAB<STR>::insert


template <class STR>
void
STR_TAB<STR>::copy_str (const char *str, UINT32 size)
{
    UINT32 buffer_size = STR::get_buffer_length (size);
#ifdef KEY // str may be pointing to buffer, which could be freed in reserve (bug 625)
    char *new_str = (char *) alloca(size+1);
    memcpy (new_str, str, size);
    new_str[size] = 0;
#endif
    reserve (buffer_size);
#ifdef KEY
    STR::copy (new_str, size, buffer + last_idx);
#else
    STR::copy (str, size, buffer + last_idx);
#endif
    last_idx += buffer_size;
} // STR_TAB::copy_str


template <class STR>
void
STR_TAB<STR>::init_hash ()
{
    STR_INDEX idx = 1;			// first entry always null
    while (idx < last_idx) {

	UINT32 length = STR::get_length (buffer + idx);
	hash_table.insert_unique (make_STR_INDEX (length, idx));
	idx += STR::get_buffer_length (length);
    }
} // STR_TAB<STR>::init_hash


template <class STRTAB>
static inline void
initialize_strtab (STRTAB& strtab, UINT32 size)
{
    strtab.buffer_size = size;
    strtab.buffer = (char *) MEM_POOL_Alloc (Malloc_Mem_Pool, size);
    strtab.buffer[0] = 0;
    strtab.last_idx = 1;
}


template <class STRTAB>
static inline void
initialize_strtab (STRTAB& strtab, const char *buf, UINT32 size)
{
    if (strtab.hash_table.size() != 0)
        strtab.hash_table.erase(strtab.hash_table.begin(),
                                strtab.hash_table.end());
    strtab.buffer_size = size;
    strtab.buffer = (char *) MEM_POOL_Alloc (Malloc_Mem_Pool, size);
    BCOPY (buf, strtab.buffer, size);
    strtab.last_idx = size;
    strtab.init_hash ();
    
} // Initialize_Strtab
    

// Global String table
static STR_TAB<NULL_TERMINATED_STRING> Strtab (1000);			       

STRING_TABLE Str_Table;

STR_IDX
STR_Table_Size ()
{
    return Strtab.last_idx;
}

// init an empty table; use by producer (e.g., front end)

void
Initialize_Strtab (UINT32 size)
{
    initialize_strtab (Strtab, size);
} 

// initialized the string table with the strtab from an input file
void
Initialize_Strtab (const char *buf, UINT32 size)
{
    initialize_strtab (Strtab, buf, size);
} // Initialize_Strtab


STR_IDX
Save_Str (const char *str)
{
    if (str == NULL)
	return 0;

    return Strtab.insert (str);

} // Save_Str


STR_IDX
Save_Str2 (const char *s1, const char *s2)
{
    UINT len = strlen (s1) + strlen(s2) + 1;
    char *new_str = (char *) alloca (len);
    strcpy (new_str, s1);
    strcat (new_str, s2);
    return Save_Str (new_str);
} // Save_Str2

STR_IDX
Save_Str2i (const char *s1, const char *s2, UINT i)
{
    UINT len = strlen (s1) + strlen(s2) + 17;
    char *new_str = (char *) alloca (len);
    sprintf(new_str, "%s%s%d", s1, s2, i);
    return Save_Str (new_str);
} // Save_Str2


char *
Index_To_Str (STR_IDX idx)
{
    Is_True (idx < Strtab.last_idx, ("Invalid STR_IDX"));
    return NULL_TERMINATED_STRING::get_str (Strtab.buffer + idx);
}

// character array table
const UINT32 TCON_STRTAB_HASH_SIZE = 512;
static STR_TAB<CHARACTER_ARRAY> TCON_strtab (TCON_STRTAB_HASH_SIZE);

UINT32
TCON_strtab_size ()
{
    return TCON_strtab.last_idx;
}

char *
TCON_strtab_buffer ()
{
    return TCON_strtab.buffer;
}

// init an empty table; use by producer
void
Initialize_TCON_strtab (UINT32 size)
{
    initialize_strtab (TCON_strtab, size);
}

// init the TCON strtab from an input file
void
Initialize_TCON_strtab (const char *buf, UINT32 size)
{
    initialize_strtab (TCON_strtab, buf, size);
} 



// add string of length "len"; string might not be null-terminated
UINT32
Save_StrN (const char *s1, UINT32 len)
{
    if (len == 0)
	return 0;

    return TCON_strtab.insert (s1, len);
} // Save_StrN

char *
Index_to_char_array (UINT32 idx)
{
    Is_True (idx < TCON_strtab.last_idx, ("Invalid TCON str index"));
    return CHARACTER_ARRAY::get_str (TCON_strtab.buffer + idx);
}


#ifdef MONGOOSE_BE

template <class STR, class MAP>
void
merge_strtab (STR_TAB<STR>& strtab, const char *buf, UINT32 size, MAP& map)
{
    map[0] = 0;
    UINT32 idx = 1;
    while (idx < size) {
	const char *str = STR::get_str (buf + idx);
	UINT32 size = STR::get_length (buf + idx);
	UINT32 new_idx = strtab.insert (str, size);
	map[idx] = new_idx;
	idx += STR::get_buffer_length (size);
    }
} // merge_strtab


// merge in a string table buffer from an input file, return a map from
// the old STR_IDX to the new (merged) STR_IDX

void
Merge_Strtab (const char *buf, UINT32 size, STR_IDX_MAP& map)
{
    merge_strtab (Strtab, buf, size, map);
}

// merge in a string table buffer from an input file, return a map from
// the old STR_IDX to the new (merged) STR_IDX

void
Merge_TCON_Strtab (const char *buf, UINT32 size, STR_IDX_MAP& map)
{
    merge_strtab (TCON_strtab, buf, size, map);
}

#endif // MONGOOSE_BE

