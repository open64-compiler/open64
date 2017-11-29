//-*-c++-*-
// ====================================================================
// ====================================================================
//
// Module: opt_rvi_emit.h
// $Revision: 1.1.1.1 $
// $Date: 2005/10/21 19:00:00 $
// $Author: marcel $
// $Source: /proj/osprey/CVS/open64/osprey1.0/be/opt/opt_rvi_emit.h,v $
//
// ====================================================================
//
// Copyright (C) 2000, 2001 Silicon Graphics, Inc.  All Rights Reserved.
//
// This program is free software; you can redistribute it and/or modify
// it under the terms of version 2 of the GNU General Public License as
// published by the Free Software Foundation.
//
// This program is distributed in the hope that it would be useful, but
// WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
//
// Further, this software is distributed without any warranty that it
// is free of the rightful claim of any third person regarding
// infringement  or the like.  Any license provided herein, whether
// implied or otherwise, applies only to this software file.  Patent
// licenses, if any, provided herein do not apply to combinations of
// this program with other software, or any other product whatsoever.
//
// You should have received a copy of the GNU General Public License
// along with this program; if not, write the Free Software Foundation,
// Inc., 59 Temple Place - Suite 330, Boston MA 02111-1307, USA.
//
// Contact information:  Silicon Graphics, Inc., 1600 Amphitheatre Pky,
// Mountain View, CA 94043, or:
//
// http://www.sgi.com
//
// For further information regarding this notice, see:
//
// http://oss.sgi.com/projects/GenInfo/NoticeExplan
//
// ====================================================================
//
// Description:
//
// Emitter for (RVI) Register-Variable Identification
//
// ====================================================================
// ====================================================================


#ifndef opt_rvi_emit_INCLUDED
#define opt_rvi_emit_INCLUDED "opt_rvi_emit.h"
#ifdef _KEEP_RCS_ID
static char *opt_rvi_emitrcs_id = opt_rvi_emit_INCLUDED"$ $Revision$";
#endif /* _KEEP_RCS_ID */

// forward declarations
class RVI;
class ALIAS_MANAGER;

// class used only by rvi emitter to manage building up regions
//
class RVIE_REGION {
  friend class RVI_EMIT;

  BB_NODE *_region_start;	// the start of the region
  BB_NODE *_region_end;		// last block in the region
  WN      *_prev_wn;		// previous WN that we must follow

  // private constructor so it cannot be used
  RVIE_REGION(void);
  RVIE_REGION(const RVIE_REGION&);
  RVIE_REGION& operator = (const RVIE_REGION&);

public:
  RVIE_REGION(BB_NODE *region_start, BB_NODE *region_end, WN *prev_wn) :
    _region_start(region_start),
    _region_end(region_end),
    _prev_wn(prev_wn)
  {
  }

  BB_NODE *Region_start(void) const { return _region_start; }
  BB_NODE *Region_end(void) const { return _region_end; }
  WN      *Prev_wn(void) const { return _prev_wn; }
}; // end rvie_region class


// class for emitting a program.  Note: constructor does all the
// work.
//
class RVI_EMIT {
private:
  const RVI	*_rvi;
  WN		*_entry_wn;
  WN		*_first_wn;
  WN		*_last_wn;
  BOOL		 _lda_only;	// only replace lda's while emitting
  ALIAS_MANAGER *_alias_mgr;
  REGION_LEVEL  _region_level;	// the rvi emitter is called for Phase 1
  				// and Phase 2

  // deal with the creation of regions using a stack of them
  STACK<RVIE_REGION *> _region_stack;
  void     Push_region( BB_NODE *start_region );
  void     Pop_region( void );

  ALIAS_MANAGER *Alias_Mgr(void) const 
           { return _alias_mgr; }

  // emit the code
  void Emit_bb( BB_NODE *bb );
  // update the block with the annotations
  void Emit_bb_annotations( BB_NODE *bb );
  // handle annotations for a given statement or expression
  void Emit_wn_annotations( BB_NODE *bb, WN *wn, WN **new_wn ) const;
  // handle annotations for a given lda expression
  void Emit_lda_wn_annotations( BB_NODE *bb, WN *wn, WN **new_wn )const;

  RVI_EMIT( void );		// not used
  RVI_EMIT(const RVI_EMIT&);	// not used
  RVI_EMIT& operator = (const RVI_EMIT&);
public:
  // constructor does everything (to build tree, etc.)
  RVI_EMIT(const RVI *rvi, BOOL lda_only, ALIAS_MANAGER *alias_mgr,
	   REGION_LEVEL region_level);
  ~RVI_EMIT( void ) {}

  // access methods
  const RVI *Rvi( void ) const
		{ return _rvi; }
  WN *Entry_wn( void )
		{ return _entry_wn; }
  void Set_entry_wn( WN *entry_wn )
		{ _entry_wn = entry_wn; }
  WN *First_wn( void )
		{ return _first_wn; }
  void Set_first_wn( WN *first_wn )
		{ _first_wn = first_wn; }
  WN *Last_wn( void )
		{ return _last_wn; }
  void Set_last_wn( WN *last_wn )
		{ _last_wn = last_wn; }
  BOOL Lda_only( void ) const
		{ return _lda_only; }
};



#endif  // opt_rvi_emit_INCLUDED
