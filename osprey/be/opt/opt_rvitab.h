//-*-c++-*-
// ====================================================================
// ====================================================================
//
// Module: opt_rvitab.h
// $Revision: 1.1.1.1 $
// $Date: 2005/10/21 19:00:00 $
// $Author: marcel $
// $Source: /proj/osprey/CVS/open64/osprey1.0/be/opt/opt_rvitab.h,v $
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
// Tables for (RVI) Register-Variable Identification
//
// ====================================================================
// ====================================================================


#ifndef opt_rvitab_INCLUDED
#define opt_rvitab_INCLUDED "opt_rvitab.h"
#ifdef _KEEP_RCS_ID
static char *opt_rvitabrcs_id = opt_rvitab_INCLUDED"$ $Revision$";
#endif /* _KEEP_RCS_ID */

#ifndef wn_INCLUDED
#include "wn.h"
#endif // wn_INCLUDED
#ifndef mempool_INCLUDED
#include "mempool.h"
#endif // mempool_INCLUDED
#ifndef stab_INCLUDED
#include "stab.h"
#endif // stab_INCLUDED
#ifndef opt_array_INCLUDED
#include "opt_array.h"
#endif // opt_array_INCLUDED

// forward declarations
//
class ALIAS_MANAGER;
class RVI_LR;
class RVI_LR_LIST;
class RVI_LRBB;


// information related to a particular constant or variable
//
class RVI_NODE : public SLIST_NODE {
private:
  IDX_32	 _bitpos;	// bit position assigned to variable
  WN		*_loadwn;	// typical load
  WN		*_storewn;	// typical store
  RVI_LRBB_LIST	*_appearances;	// live-range blocks with appearances
  RVI_LR_LIST	*_live_ranges;	// live-ranges for this variable

  RVI_NODE( void );		// not used
  RVI_NODE& operator = (const RVI_NODE&);
public:
  RVI_NODE( IDX_32 bp, WN *loadwn, WN *storewn ) :
		_bitpos(bp), _loadwn(loadwn), _storewn(storewn)
		{ _live_ranges = NULL;
		  _appearances = NULL;
		}
  ~RVI_NODE( void )
		{}

  // access methods
  IDX_32 Bitpos( void ) const
		{ return _bitpos; }
  WN *Loadwn( void ) const
		{ return _loadwn; }
  void Set_loadwn( WN *loadwn )
		{ _loadwn = loadwn; }
  WN *Storewn( void ) const
		{ return _storewn; }
  void Set_storewn( WN *storewn )
		{ _storewn = storewn; }
  RVI_LRBB_LIST *Appearances( void )
		{ return _appearances; }
  void Set_appearances( RVI_LRBB_LIST *appearances )
		{ _appearances = appearances; }
  RVI_LR_LIST *Live_ranges( void ) const
		{ return _live_ranges; }
  void Set_live_ranges( RVI_LR_LIST *live_ranges )
		{ _live_ranges = live_ranges; }

  // the machine type of this value
  MTYPE Mtype( void ) const;
  // the ST of this variable
  ST   *St( void ) const;
  // the name of this variable
  const char *Name( void ) const;
  // a new WN representing the home of this variable
  WN *New_home_wn( ALIAS_MANAGER * ) const;

  // determine if this constant node and the given whirl node match
  BOOL Match_constant( const WN *wn ) const;

  // Keep track of blocks with references to this variable
  void Add_reference( BB_NODE *bb, BOOL is_load, MEM_POOL *pool );

  // determine if a variable is always volatile and can't be removed
  // WARNING: individual references may be cast as volatile and can't
  // be removed.  This only tells if the variable itself is volatile
  BOOL Is_volatile( void ) const;

  // create a load of the variable into the given pseudo reg
  WN *Create_load( INT32 preg, ALIAS_MANAGER *alias_mgr ) const;
  // create a store of the preg into variable
  WN *Create_store( INT32 preg, ALIAS_MANAGER *alias_mgr ) const;

  // print out a node
  void Print( FILE *fp = stderr ) const;
};

// singly linked list of RVI_NODEs
//
class RVI_NODE_LIST : public SLIST {
private:
  RVI_NODE_LIST(const RVI_NODE_LIST&);
  RVI_NODE_LIST& operator = (const RVI_NODE_LIST&);

  DECLARE_SLIST_CLASS(RVI_NODE_LIST,RVI_NODE)

public:
  ~RVI_NODE_LIST(void) {}	// destructor, use mempool
}; // end of class RVI_NODE_LIST;


// class for iterating through nodes
//
class RVI_NODE_ITER : public SLIST_ITER {
  DECLARE_SLIST_ITER_CLASS(RVI_NODE_ITER, RVI_NODE, RVI_NODE_LIST)
public:
  void Init( void ) const {}
};


// class to hold hash table of constants
//
class RVI_CTAB {
friend class RVI_CTAB_ITER;
private:
  ARRAY<RVI_NODE_LIST *> _ctab;
  MEM_POOL	*_mem_pool;

  RVI_CTAB( void );		// not used
  RVI_CTAB& operator = (const RVI_CTAB&);

public:

  RVI_CTAB( MEM_POOL *mem_pool );	// constructor to build ctab
  ~RVI_CTAB( void );		// destructor

  // determine if a constant node is already in the hash table
  // (returns the rvi_cnode if found, or else returns null)
  RVI_NODE *Find( const WN *wn, IDX_32 hash_val ) const;

  // add a new unique node to the ctab (assumed not to be found)
  RVI_NODE *Add_unique( WN *wn, IDX_32 bitpos, IDX_32 hash_val ) const;

  // come up with a hash value of the constant node
  IDX_32 Hash( const WN *wn ) const;

  // print the entire table
  void Print( FILE *fp = stderr ) const;
};


// iterator for the constant table
//
class RVI_CTAB_ITER {
private:
  const RVI_CTAB*_ctab;			// the constant table
  INT		 _ctab_index;		// index into _ctab array
  RVI_NODE_ITER _cnode_iter;		// last one we looked at
  RVI_NODE	*_cnode;		// last one we returned
  BOOL		 _did_first;		// initialized with first

  RVI_CTAB_ITER& operator = (const RVI_CTAB_ITER&);

public:
  void Init( const RVI_CTAB *ctab )
		{ _ctab = ctab;
		  _ctab_index = 0;
		  _cnode = NULL;
		  _did_first = FALSE;
		}

  RVI_CTAB_ITER( void )
		{ Init( NULL ); }
  RVI_CTAB_ITER( const RVI_CTAB *ctab )
		{ Init( ctab ); }
  ~RVI_CTAB_ITER( void ) {}

  RVI_NODE *First( void );
  RVI_NODE *Next( void );
  BOOL Is_Empty( void );
};


// class to hold table of variables
//
class RVI_VTAB {
private:
  ARRAY<RVI_NODE *> _vtab;	// array indexed by variables' bitpos
  INT32		 _size;		// declared array size
  MEM_POOL	*_mem_pool;

  RVI_VTAB( void );		// not used
  RVI_VTAB& operator = (const RVI_VTAB&);

public:

  // constructor to build vtab
  RVI_VTAB( INT num_vars, MEM_POOL *mem_pool );
  ~RVI_VTAB( void );			// destructor

  INT32 Size( void ) const
		{ return _size; }

  // determine if a bitpos is already assigned in the variable table
  // (returns the rvi_vnode if found, or else returns null)
  RVI_NODE *Find( IDX_32 bitpos ) const
		{ return _vtab[bitpos]; }

  // determines if there is a vnode in our table that matches the
  // memory reference (st, offset, mtype all match)
  RVI_NODE *Find_match( const WN *wn ) const;
		
  // add a new unique node to the vtab (assumed not to be found)
  RVI_NODE *Add_store( WN *storewn, IDX_32 bitpos ) const;
  RVI_NODE *Add_load( WN *loadwn, IDX_32 bitpos ) const;

  // print the entire table
  void Print( FILE *fp = stderr ) const;
};


// iterator for the variable table
//
class RVI_VTAB_ITER {
private:
  const RVI_VTAB*_vtab;			// the variable table
  INT		 _vtab_index;		// index into _vtab array
  RVI_NODE	*_vnode;		// last one we returned
  BOOL		 _did_first;		// initialized with first

  RVI_VTAB_ITER& operator = (const RVI_VTAB_ITER&);

public:
  void Init( const RVI_VTAB *vtab )
		{ _vtab = vtab;
		  _vtab_index = 0;
		  _vnode = NULL;
		  _did_first = FALSE;
		}

  RVI_VTAB_ITER( void )
		{ Init( NULL ); }
  RVI_VTAB_ITER( const RVI_VTAB *vtab )
		{ Init( vtab ); }
  ~RVI_VTAB_ITER( void ) {}

  RVI_NODE *First( void );
  RVI_NODE *Next( void );
  BOOL Is_Empty( void );
};

#endif  // opt_rvitab_INCLUDED
