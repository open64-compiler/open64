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
#ifndef cxx_ipo_inline_util_INCLUDED
#define cxx_ipo_inline_util_INCLUDED

#include "defs.h"
#include "wn.h"
#include "cxx_template.h"
// ======================================================================
// Auxiliary classes and definitions used by IPO_INLINE
// ======================================================================

class IPO_MP_PRAGMA_CODE {

private:
  WN* _pragma;             // keeps the mp pragma node
  WN* _do_loop;            // keeps the code (often loop) for body of pragma

public:
  IPO_MP_PRAGMA_CODE() : _pragma(NULL), _do_loop(NULL) {}

  WN* Get_pragma()  const { return _pragma;}
  WN* Get_do_loop() const { return _do_loop;}

  void Set_pragma_do_loop(WN* pragma, WN* do_loop)
    {
      _pragma = pragma;
      _do_loop = do_loop;
    };

};

typedef DYN_ARRAY<IPO_MP_PRAGMA_CODE> IPO_MP_PRAGMA_CODE_ARRAY;

extern void
Record_Parallel_Loop_Pragma(WN* , WN* _loop, IPO_MP_PRAGMA_CODE_ARRAY *);

extern WN* 
Get_Parallel_Pragma_Node(WN*, IPO_MP_PRAGMA_CODE_ARRAY *);

extern void
Mark_MP_Loops(WN* , IPO_MP_PRAGMA_CODE_ARRAY *);


extern INT
Do_Loop_Is_MP(WN* ,IPO_MP_PRAGMA_CODE_ARRAY * );

extern WN*
Find_MP_Loop_Or_Region(WN* ,IPO_MP_PRAGMA_CODE_ARRAY *);

extern PREG_NUM 
Process_Alloca_Preamble(WN *);

extern void 
Process_Alloca_Postamble(PREG_NUM , WN*);

extern mUINT32
Compute_max_region_id(WN *caller_wn);

// preg related code
struct DEDICATED_RETURN_PREGS
{
    struct {
	PREG_IDX first;			// original preg
	PREG_IDX second;		// new preg
    } pregs[2];
    UINT32 num_pregs;
};

struct NEGATIVE_RETURN_PREGS
{
    PREG_IDX preg_idx;			// preg for the return value
    ST* st;				// for struct, use tmp variable
					// for preg, this is the preg ST
    BOOL use_tmp_st;			// true if using tmp variable
};

class RETURN_PREG
{
    // By 7.3 MR, there will always be 1 return preg, and the
    // DEDICATED_RETURN_PREGS part should be removed.
private:
    union {
	DEDICATED_RETURN_PREGS old_style; // no OPR_RETURN_VAL
	NEGATIVE_RETURN_PREGS new_style; // use OPR_RETURN_VAL
    } _u;
    BOOL use_return_val;

public:

    RETURN_PREG () : use_return_val (TRUE) {
	_u.old_style.num_pregs = 0;
    }

    void insert (PREG_IDX _orig, PREG_IDX _new) {
	use_return_val = FALSE;
	if (_u.old_style.num_pregs == 0) {
	    _u.old_style.num_pregs = 1;
	    _u.old_style.pregs[0].first = _orig;
	    _u.old_style.pregs[0].second = _new;
	} else {
	    _u.old_style.num_pregs = 2;
	    _u.old_style.pregs[1].first = _orig;
	    _u.old_style.pregs[1].second = _new;
	}
    }

    void insert (PREG_IDX _new, ST* preg) {
	use_return_val = TRUE;
	_u.new_style.use_tmp_st = FALSE;
	_u.new_style.preg_idx = _new;
	_u.new_style.st = preg;
    }

    void insert (ST* st) {
	use_return_val = TRUE;
	_u.new_style.use_tmp_st = TRUE;
	_u.new_style.st = st;
    }

    PREG_IDX find (PREG_IDX old) const {
	if (old == (PREG_IDX) -1) {
	    Is_True (_u.new_style.use_tmp_st == FALSE,
		     ("expecting the return value to be a struct"));
	    return _u.new_style.preg_idx;
	} else {
	    if (_u.old_style.pregs[0].first == old)
		return _u.old_style.pregs[0].second;
	    if (_u.old_style.num_pregs == 2 &&
		_u.old_style.pregs[1].first == old)
		return _u.old_style.pregs[1].second;
	    Fail_FmtAssertion ("Illegal dedicated return register %d \n", old);
	    return 0;
	}
    }

    ST* find_st () const {
	Is_True (use_return_val, ("expecting PREG -1 for return values"));
	return _u.new_style.st;
    }

    UINT size () const {
	return use_return_val ? 1 : _u.old_style.num_pregs;
    }
}; // RETURN_PREG	

extern void      
Compute_Return_Preg_Offset(WN *, RETURN_PREG& rp, BOOL use_lowered_return_preg,
			   SCOPE *, SYMTAB_IDX);

// Fix dedicated pregs in caller
extern void      
Fix_Return_Pregs(WN *, const RETURN_PREG& rp);

#ifdef KEY
#include "ipa_cg.h"
extern void
Get_enclosing_region (IPA_NODE *, IPA_EDGE *);
#endif

#endif // cxx_ipo_inline_util_INCLUDED
