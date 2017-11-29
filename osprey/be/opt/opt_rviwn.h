//-*-c++-*-
// ====================================================================
// ====================================================================
//
// Module: opt_rviwn.h
// $Revision: 1.1.1.1 $
// $Date: 2005/10/21 19:00:00 $
// $Author: marcel $
// $Source: /proj/osprey/CVS/open64/osprey1.0/be/opt/opt_rviwn.h,v $
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
// WN interface for (RVI) Register-Variable Identification
//
// ====================================================================
// ====================================================================


#ifndef opt_rviwn_INCLUDED
#define opt_rviwn_INCLUDED "opt_rviwn.h"
#ifdef _KEEP_RCS_ID
static char *opt_rviwnrcs_id = opt_rviwn_INCLUDED"$ $Revision$";
#endif /* _KEEP_RCS_ID */

#ifndef stab_INCLUDED
#include "stab.h"
#endif /* stab_INCLUDED */
#ifndef opt_rvitab_INCLUDED
#include "opt_rvitab.h"
#endif  // opt_rvitab_INCLUDED

// class to hold annotation information
//
class RVI_ANN : public SLIST_NODE {
  DECLARE_SLIST_NODE_CLASS(RVI_ANN);
private:
  ST		*_preg_st;	// the preg's st
  mINT32	 _preg;		// the preg to use
  RVI_NODE	*_rvi_node;	// the node this is for

  RVI_ANN( void );		// not used
  RVI_ANN(const RVI_ANN&);	// not used
  RVI_ANN& operator = (const RVI_ANN&);
public:
  RVI_ANN( ST *st, INT32 preg, RVI_NODE *rvi_node ) :
		_preg_st(st), _preg(preg), _rvi_node(rvi_node)
		{}
  ~RVI_ANN( void ) {}

  // access methods
  ST *Preg_st( void ) const
		{ return _preg_st; }
  INT32 Preg( void ) const
		{ return _preg; }
  RVI_NODE *Rvi_node( void ) const
		{ return _rvi_node; }
  IDX_32 Bitpos( void ) const
		{ return _rvi_node->Bitpos(); }

  // Get a TY_IDX associated with this preg
  TY_IDX Preg_ty( void ) const
    { return MTYPE_To_TY(TY_mtype(ST_type(Preg_st()))); }
  // create a new LDID of this preg
  WN *New_ldid( ALIAS_MANAGER *alias_mgr ) const;

  // print out an annotation
  void Print( FILE *fp = stderr ) const;
};


// singly linked list of RVI_ANNs
//
class RVI_ANN_LIST : public SLIST {
  DECLARE_SLIST_CLASS(RVI_ANN_LIST,RVI_ANN)
private:
  RVI_ANN_LIST(const RVI_ANN_LIST&);
  RVI_ANN_LIST& operator = (const RVI_ANN_LIST&);
public:
  ~RVI_ANN_LIST(void) {}	// destructor, use mempool

  // locate a bitpos in this list
  RVI_ANN *Find( const IDX_32 bitpos );

  // print out the annotation list
  void Print( FILE *fp = stderr );

}; // end of class RVI_ANN_LIST;


// class for iterating through lists of RVI_ANNs
//
class RVI_ANN_ITER : public SLIST_ITER {
  DECLARE_SLIST_ITER_CLASS(RVI_ANN_ITER, RVI_ANN, RVI_ANN_LIST)
public:
  ~RVI_ANN_ITER(void)	{}
};



#endif  // opt_rviwn_INCLUDED
