//-*-c++-*-
// ====================================================================
// ====================================================================
//
// Module: opt_rvilr.h
// $Revision: 1.1.1.1 $
// $Date: 2005/10/21 19:00:00 $
// $Author: marcel $
// $Source: /proj/osprey/CVS/open64/osprey1.0/be/opt/opt_rvilr.h,v $
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
// Live-ranges for (RVI) Register-Variable Identification
//
// ====================================================================
// ====================================================================


#ifndef opt_rvilr_INCLUDED
#define opt_rvilr_INCLUDED "opt_rvilr.h"
#ifdef _KEEP_RCS_ID
static char *opt_rvilrrcs_id = opt_rvilr_INCLUDED"$ $Revision$";
#endif /* _KEEP_RCS_ID */

// forward declarations
class BB_NODE;
class BB_NODE_SET;
class CFG;
class RVI;
class RVI_LR_INFO;

// class to hold live-range information for a given basic block
//
class RVI_LRBB : public SLIST_NODE {
  DECLARE_SLIST_NODE_CLASS(RVI_LRBB);
private:
  enum { MAX_RVI_REFS = 255 };	// max number our counters can track
  enum RVI_LRBB_FLAG {
    LRBBF_NONE        = 0x00,	// initial value
    LRBBF_STR         = 0x01,	// first ref to this is a store to it
    LRBBF_ND_STR_BOT  = 0x02,	// need a store at exit of bb
    LRBBF_ND_STR_SUCC = 0x04,	// need a store at entry of succs
    LRBBF_ND_STR_IREF = 0x08,	// need a store at exit before iref
    LRBBF_ND_STR      = (LRBBF_ND_STR_BOT|
			 LRBBF_ND_STR_SUCC|
			 LRBBF_ND_STR_IREF),
    LRBBF_ND_LOD_HERE = 0x10,	// need load at entry of bb
    LRBBF_ND_LOD_PRED = 0x20,	// need load at exit of preds
    LRBBF_ND_LOD_CHI  = 0x40,	// need load at exit of preds,after chi
    LRBBF_ND_LOD      = (LRBBF_ND_LOD_HERE|
			 LRBBF_ND_LOD_PRED|
			 LRBBF_ND_LOD_CHI),
    // next should be 0x80
  };

  BB_NODE	*_bb;		// the block
  mUINT8	 _flags;	// actually of type RVI_LRBB_FLAG
  mUINT8	 _load_cnt;	// number of direct loads of this
  mUINT8	 _store_cnt;	// number of direct stores to this

  RVI_LRBB( void );		// not used
  RVI_LRBB(const RVI_LRBB&);	// not used
  RVI_LRBB& operator = (const RVI_LRBB&);
public:
  RVI_LRBB( BB_NODE *bb ) :
		_bb(bb)
		{ _flags = LRBBF_NONE;
		  _load_cnt = 0;
		  _store_cnt = 0;
		}
  ~RVI_LRBB( void ) {}

  // access methods
  BB_NODE *Bb( void ) const
		{ return _bb; }

  // flag access methods
  //
  BOOL First_is_store( void ) const
		{ return (_flags & LRBBF_STR) != 0; }
  void Set_first_is_store( void )
		{ _flags |= LRBBF_STR; }
  void Reset_first_is_store( void )
		{ _flags &= ~LRBBF_STR; }
  BOOL Need_store( void ) const
		{ return (_flags & LRBBF_ND_STR) != 0; }
  BOOL Need_store_bot( void ) const
		{ return (_flags & LRBBF_ND_STR_BOT) != 0; }
  void Set_need_store_bot( void )
		{ _flags |= LRBBF_ND_STR_BOT; }
  void Reset_need_store_bot( void )
		{ _flags &= ~LRBBF_ND_STR_BOT; }
  BOOL Need_store_succ( void ) const
		{ return (_flags & LRBBF_ND_STR_SUCC) != 0; }
  void Set_need_store_succ( void )
		{ _flags |= LRBBF_ND_STR_SUCC; }
  void Reset_need_store_succ( void )
		{ _flags &= ~LRBBF_ND_STR_SUCC; }
  BOOL Need_store_iref( void ) const
		{ return (_flags & LRBBF_ND_STR_IREF) != 0; }
  void Set_need_store_iref( void )
		{ _flags |= LRBBF_ND_STR_IREF; }
  void Reset_need_store_iref( void )
		{ _flags &= ~LRBBF_ND_STR_IREF; }
  BOOL Need_load( void ) const
		{ return (_flags & LRBBF_ND_LOD) != 0; }
  BOOL Need_load_here( void ) const
		{ return (_flags & LRBBF_ND_LOD_HERE) != 0; }
  void Set_need_load_here( void )
		{ _flags |= LRBBF_ND_LOD_HERE; }
  void Reset_need_load_here( void )
		{ _flags &= ~LRBBF_ND_LOD_HERE; }
  BOOL Need_load_pred( void ) const
		{ return (_flags & LRBBF_ND_LOD_PRED) != 0; }
  void Set_need_load_pred( void )
		{ _flags |= LRBBF_ND_LOD_PRED; }
  void Reset_need_load_pred( void )
		{ _flags &= ~LRBBF_ND_LOD_PRED; }
  BOOL Need_load_chi( void ) const
		{ return (_flags & LRBBF_ND_LOD_CHI) != 0; }
  void Set_need_load_chi( void )
		{ _flags |= LRBBF_ND_LOD_CHI; }
  void Reset_need_load_chi( void )
		{ _flags &= ~LRBBF_ND_LOD_CHI; }

  // counter access methods
  //
  UINT8 Load_cnt( void ) const
		{ return _load_cnt; }
  void Set_load_cnt( UINT load_cnt )
		{ _load_cnt = MIN(load_cnt,MAX_RVI_REFS); }
  UINT8 Store_cnt( void ) const
		{ return _store_cnt; }
  void Set_store_cnt( UINT store_cnt )
		{ _store_cnt = MIN(store_cnt,MAX_RVI_REFS); }

  // print out a live-range
  void Print( FILE *fp = stderr ) const;
};


// singly linked list of RVI_LRBBs
//
class RVI_LRBB_LIST : public SLIST {
  DECLARE_SLIST_CLASS(RVI_LRBB_LIST,RVI_LRBB)
private:
  RVI_LRBB_LIST(const RVI_LRBB_LIST&);
  RVI_LRBB_LIST& operator = (const RVI_LRBB_LIST&);
public:
  ~RVI_LRBB_LIST(void) {}	// destructor, use mempool

  // locate a block in this list
  RVI_LRBB *Find( const BB_NODE *bb );

}; // end of class RVI_LRBB_LIST;


// class for iterating through lists of RVI_LRBBs
//
class RVI_LRBB_ITER : public SLIST_ITER {
  DECLARE_SLIST_ITER_CLASS(RVI_LRBB_ITER, RVI_LRBB, RVI_LRBB_LIST)
public:
  ~RVI_LRBB_ITER(void)	{}
};



// class for each live-range
//
class RVI_LR : public SLIST_NODE {
private:
  enum { MAX_RVI_REFS = 255 };	// max number our counters can track
  enum RVI_LR_FLAG {
    LRF_NONE        = 0x00,	// initial value
    LRF_REPLACE     = 0x01,	// replace something (do rvi for lr)
    LRF_NEED_HOME   = 0x02,	// references the home (or value)
    LRF_PREDOUT_NOST= 0x04,	// pred out of LR, doesn't start w/store
    // next should be 0x08
  };
  IDX_32		_bitpos;	// bit position this lr is for
  RVI_LRBB_LIST		_blocks;	// list of blocks in live-range
  BB_NODE_SET	       *_block_set;	// set of blocks in live-range
  mINT32		_preg;		// preg used for this live-range
  mUINT8		_load_cnt;	// total number of loads
  mUINT8		_store_cnt;	// total number of stores
  mUINT8		_flags;		// various flags

  RVI_LR( void );		// not used
  RVI_LR(const RVI_LR&);
  RVI_LR& operator = (const RVI_LR&);
public:
  RVI_LR( IDX_32 bitpos, const CFG *cfg, MEM_POOL *pool );
  ~RVI_LR( void ) {}

  IDX_32 Bitpos( void ) const
		{ return _bitpos; }

  RVI_LRBB_LIST *Blocks( void )
		{ return &_blocks; }
  const RVI_LRBB_LIST *Blocks( void ) const
		{ return (const RVI_LRBB_LIST *)&_blocks; }

  const BB_NODE_SET *Block_set( void ) const
		{ return _block_set; }
  BB_NODE_SET *Block_set( void )
		{ return _block_set; }
  void Set_block_set( BB_NODE_SET *block_set )
		{ _block_set = block_set; }

  INT32 Preg( void ) const
		{ return _preg; }
  void Set_preg( INT32 preg )
		{ _preg = preg; }
  UINT8 Load_cnt( void ) const
		{ return _load_cnt; }
  void Set_load_cnt( UINT load_cnt )
		{ _load_cnt = MIN(load_cnt,MAX_RVI_REFS); }
  UINT8 Store_cnt( void ) const
		{ return _store_cnt; }
  void Set_store_cnt( UINT store_cnt )
		{ _store_cnt = MIN(store_cnt,MAX_RVI_REFS); }

  // various flags
  BOOL Replace_anything( void ) const
		{ return (_flags & LRF_REPLACE) != 0; }
  void Set_replace_anything( void )
		{ _flags |= LRF_REPLACE; }
  BOOL Need_home( void ) const
		{ return (_flags & LRF_NEED_HOME) != 0; }
  void Set_need_home( void )
		{ _flags |= LRF_NEED_HOME; }
  BOOL Predout_nostore( void ) const
		{ return (_flags & LRF_PREDOUT_NOST) != 0; }
  void Set_predout_nostore( void )
		{ _flags |= LRF_PREDOUT_NOST; }

  // count up number of preds/succs of bb in/not-in the live-range
  void Analyze_preds( const BB_NODE *bb, const RVI *rvi, 
		      RVI_LR_INFO *lr_info );
  void Analyze_succs( const BB_NODE *bb, const RVI *rvi,
		      RVI_LR_INFO *lr_info );

  // determine if we should bother doing anything with this lr
  BOOL Do_anything( void );

  // print out a live-range
  void Print( FILE *fp = stderr );
};


// singly linked list of RVI_LRs
//
class RVI_LR_LIST : public SLIST {
private:
  RVI_LR_LIST(const RVI_LR_LIST&);
  RVI_LR_LIST& operator = (const RVI_LR_LIST&);

  DECLARE_SLIST_CLASS(RVI_LR_LIST,RVI_LR)

public:
  ~RVI_LR_LIST(void) {}	// destructor, use mempool
}; // end of class RVI_LR_LIST;


// class for iterating through lists of live-ranges
//
class RVI_LR_ITER : public SLIST_ITER {
  DECLARE_SLIST_ITER_CLASS(RVI_LR_ITER, RVI_LR, RVI_LR_LIST)
public:
  void Init( void ) const {}
};


#endif  // opt_rvilr_INCLUDED
