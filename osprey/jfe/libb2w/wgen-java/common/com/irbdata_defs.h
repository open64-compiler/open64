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

#ifndef irbdata_defs_INCLUDED
#define irbdata_defs_INCLUDED

/* See the "WHIRL Symbol Table Specification" for description of the INITO
   and INITV data structures */

#ifndef symtab_idx_INCLUDED
#include "symtab_idx.h"
#endif /* symtab_idx_INCLUDED */

// initialized objects
struct INITO {
    ST_IDX st_idx;			// item being initialized
    INITV_IDX val;			// initial value

    void Verify (UINT level) const;
    void Print  (FILE* f)    const;
};


// initial value

enum INITVKIND {
    INITVKIND_UNK	= 0,
    INITVKIND_SYMOFF	= 1,
    INITVKIND_ZERO	= 2,
    INITVKIND_ONE	= 3,
    INITVKIND_VAL	= 4,
    INITVKIND_BLOCK	= 5,
    INITVKIND_PAD	= 6,
    INITVKIND_SYMDIFF	= 7,
    INITVKIND_SYMDIFF16 = 8,
#ifdef TARG_IA64
    INITVKIND_LABEL	= 9,
    INITVKIND_SYMIPLT	= 10	//just for function descriptor which equals to @fptr + gp
#else 
    INITVKIND_LABEL     = 9
#endif
};

#ifdef KEY
enum INITVFLAGS {	// currently only for EH
    INITVFLAGS_UNDEFINED	= 0,
    INITVFLAGS_ACTION_REC,
    INITVFLAGS_TYPEINFO,
    INITVFLAGS_EH_SPEC,
    INITVFLAGS_SUMMARIZED, // initv has been summarized in ipl, don't redo
    INITVFLAGS_UPDATED     // summary info in initv has been updated in ipa
};
#endif // KEY

struct INITV
{
    INITV_IDX next;			// next value for non-scalar member
    INITVKIND kind : 16;		// kind of value
    mUINT16 repeat1;			// repeat factor (repeat2 used for
					// INITVKIND_VAL
    union {

	struct {			// this field for SYMOFF and SYMIPLT
	    ST_IDX st;			
	    mINT32 ofst;
	} sto;				// address + offset 
	 				

	struct {
	    LABEL_IDX lab;		// for INITVKIND_LABEL
	    mINT32 unused;		// filler, must be zero
	} lab;
	
	struct {
	    LABEL_IDX lab1;
	    ST_IDX st2;
	} stdiff;			// lab1 - st2
	
	struct {
	    union {
		TCON_IDX tc;		// value
		mTYPE_ID mtype;		// machine type for INITVKIND_ZERO
					// and INITVKIND_ONE
	    } u;
	    mUINT32 repeat2;		// 32-bits for repeat factor 
	} tcval;

	struct {
	    INITV_IDX blk;		// useful for aggregate values
#ifdef KEY
	    mINT32 flags;		// flags
#else
	    mINT32 unused;		// filler, must be zero
#endif // KEY
	} blk;
	
	struct {
	    mINT32 pad;			// amount of padding in bytes
	    mINT32 unused;		// filler, must be zero
	} pad;
    } u;

    ST_IDX St () const			{ return u.sto.st; }
    INT32 Ofst () const			{ return u.sto.ofst; }

    LABEL_IDX Lab () const		{ return u.lab.lab; }

    LABEL_IDX Lab1 () const		{ return u.stdiff.lab1; }
    ST_IDX St2 () const			{ return u.stdiff.st2; }

    TCON_IDX Tc () const		{ return u.tcval.u.tc; }
    TYPE_ID Mtype () const		{ return u.tcval.u.mtype; }
    UINT32 Repeat2 () const		{ return u.tcval.repeat2; }

    INITV_IDX Blk () const		{ return u.blk.blk; }
    INT32 Pad () const			{ return u.pad.pad; }

    void Verify (UINT level) const;
}; // INITV


#endif /* irbdata_defs_INCLUDED */
