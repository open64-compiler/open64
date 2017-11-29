//-*-c++-*-
// ====================================================================
// ====================================================================
//
// Module: opt_rvitab.cxx
// $Revision: 1.1.1.1 $
// $Date: 2005/10/21 19:00:00 $
// $Author: marcel $
// $Source: /proj/osprey/CVS/open64/osprey1.0/be/opt/opt_rvitab.cxx,v $
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
// Tables used for Register-Variable Identification.
//
// ====================================================================
// ====================================================================


#ifdef USE_PCH
#include "opt_pch.h"
#endif // USE_PCH
#pragma hdrstop


#ifdef _KEEP_RCS_ID
#define opt_rvitab_CXX	"opt_rvitab.cxx"
static char *rcs_id = 	opt_rvitab_CXX"$Revision$";
#endif /* _KEEP_RCS_ID */

#include "defs.h"
#include "errors.h"
#include "erglob.h"
#include "tracing.h"
#include "stab.h"
#include "wn.h"
#include "wn_util.h"
#include "cxx_memory.h"
#include "data_layout.h"

#include "opt_defs.h"
#include "opt_config.h"
#include "opt_base.h"
#include "opt_rvilr.h"
#include "opt_rvitab.h"
#include "opt_util.h"
#include "opt_wn.h"
#include "opt_alias_interface.h"

// ====================================================================
// ====================================================================
// Constant Table
// ====================================================================
// ====================================================================

RVI_CTAB::RVI_CTAB( MEM_POOL *mem_pool ) :
  _mem_pool(mem_pool),
  _ctab(RVI_CTAB_SIZE, mem_pool)
{
  // clear out the array
  _ctab.Bzero_array();
}

RVI_CTAB::~RVI_CTAB( void )
{
}

// ====================================================================
// Find a node in our constant table that matches the given node
// ====================================================================

RVI_NODE *
RVI_CTAB::Find( const WN *wn, IDX_32 hash_val ) const
{
  RVI_NODE_ITER node_iter;
  RVI_NODE *cnode;

  FOR_ALL_NODE( cnode, node_iter, Init(_ctab[hash_val]) ) {
    if ( cnode->Match_constant( wn ) ) {
      return cnode;
    }
  }

  return NULL;
}

// ====================================================================
// Add a node to our constant table (without checking for duplicate)
// ====================================================================

RVI_NODE *
RVI_CTAB::Add_unique( WN *wn, IDX_32 bitpos, IDX_32 hash_val ) const
{
  RVI_NODE *cnode = CXX_NEW(RVI_NODE(bitpos,wn,NULL), _mem_pool);

  if ( _ctab[hash_val] == NULL ) {
    _ctab[hash_val] = CXX_NEW(RVI_NODE_LIST(), _mem_pool);
  }

  _ctab[hash_val]->Prepend( cnode );

  return cnode;
}

// ====================================================================
// Come up with a hash value for the given node
// ====================================================================

IDX_32
RVI_CTAB::Hash( const WN *wn ) const
{
  const OPCODE   opc = WN_opcode(wn);
  const OPERATOR opr = OPCODE_operator(opc);

  if ( opr == OPR_INTCONST ) {
    // hash the opcode and the value
    return ( ((UINT32)opc+(UINT64)WN_const_val(wn)) % RVI_CTAB_SIZE );
  }
  else if (opr == OPR_CONST || opr == OPR_LDA) {
    UINT32 val;

    switch (ST_class(WN_st(wn))) {
    case CLASS_CONST:
      // Maybe a bad choice of initial values for compile-time
      // performance. Revisit this.
      Warn_todo("RVI_CTAB::Hash: use better initial value for CLASS_CONST");
      val = WN_st_idx(wn);
      break;
    default:
      Warn_todo("RVI_CTAB::Hash: use better initial value for non-CONST classes");
      val = WN_st_idx(wn);
      break;
    }

    // hash the opcode and the st's val
    if ( opr == OPR_CONST ) {
      return (((UINT32)opc+val) % RVI_CTAB_SIZE);
    }
    else {
      // lda case
      // hash the opcode, the st's val, and the offset
#ifdef TARG_SL
      return (((UINT32)opc+val+(UINT32)WN_lda_offset(wn) + (UINT32)WN_is_internal_mem_ofst(wn)) % RVI_CTAB_SIZE);
#else 
      return (((UINT32)opc+val+(UINT32)WN_lda_offset(wn)) % RVI_CTAB_SIZE);
#endif
    }
  }
  else {
    Warn_todo( "RVI_CTAB::Hash: unknown operator" );
    return (((UINT32)opc) % RVI_CTAB_SIZE);
  }
}

// ====================================================================
// The machine type of this variable
// ====================================================================

MTYPE
RVI_NODE::Mtype( void ) const
{
  MTYPE mtype;
  if ( Storewn() != NULL ) {
    const OPCODE opc = WN_opcode(Storewn());
    mtype = OPCODE_desc(opc);
  }
  else if ( Loadwn() != NULL ) {
    const OPCODE opc = WN_opcode(Loadwn());
    const OPERATOR opr = OPCODE_operator(opc);

    if ( opr == OPR_LDID )
      mtype = OPCODE_desc(opc);
    else
      mtype = OPCODE_rtype(opc);
  }
  else {
    FmtAssert( FALSE, ("RVI_NODE::Mtype: No way to determine type") );
    return MTYPE_V;
  }

  // don't allow sub-integer types
  if ( MTYPE_size_best(mtype) >= 32/*bits*/ )
    return mtype;
  else
    return Mtype_from_class_size( mtype, MTYPE_I4 );
}

// ====================================================================
// Determine if the given node is identical to 'this'
// ====================================================================

BOOL
RVI_NODE::Match_constant( const WN *wn ) const
{
  const OPCODE opc = WN_opcode(wn);

  if ( WN_opcode(Loadwn()) != opc )
    return FALSE;

  // ok, same opcode, what about particulars
  switch ( OPCODE_operator(opc) ) {
    case OPR_CONST:
      return WN_st(Loadwn()) == WN_st(wn);

    case OPR_INTCONST:
      return WN_const_val(Loadwn()) == WN_const_val(wn);

    case OPR_LDA:
      return (WN_st(Loadwn()) == WN_st(wn)) &&
	     (WN_lda_offset(Loadwn()) == WN_lda_offset(wn));
  }

  return FALSE;
}

// ====================================================================
// Print the entire table
// ====================================================================

void
RVI_CTAB::Print( FILE *fp ) const
{
  fprintf( fp, "%sRVI_CTAB::Print\n%s", SBar, SBar );
  RVI_CTAB_ITER ctab_iter;
  RVI_NODE *cnode;
  FOR_ALL_NODE( cnode, ctab_iter, Init( this ) ) {
    cnode->Print( fp );
  }
}

// ====================================================================
// Iterator class for RVI_CTAB
// ====================================================================

RVI_NODE *
RVI_CTAB_ITER::First( void )
{
  _did_first = TRUE;
  _cnode = NULL;
  for ( _ctab_index = 0; _ctab_index < RVI_CTAB_SIZE; _ctab_index++ ) {
    RVI_NODE_LIST *clist = _ctab->_ctab[_ctab_index];
    if ( clist != NULL ) {
      FOR_ALL_NODE( _cnode, _cnode_iter, Init(clist) ) {
	return _cnode;
      }
    }
  }

  return NULL;
}

RVI_NODE *
RVI_CTAB_ITER::Next( void )
{
  if ( ! _did_first ) {
    return First();
  }

  _cnode = _cnode_iter.Next();
  if ( _cnode != NULL ) {
    return _cnode;
  }

  // pick up where we left off
  for ( _ctab_index += 1; _ctab_index < RVI_CTAB_SIZE; _ctab_index++ ) {
    RVI_NODE_LIST *clist = _ctab->_ctab[_ctab_index];
    if ( clist != NULL ) {
      FOR_ALL_NODE( _cnode, _cnode_iter, Init(clist) ) {
	return _cnode;
      }
    }
  }

  return NULL;
}

BOOL
RVI_CTAB_ITER::Is_Empty( void )
{
  if ( ! _did_first ) {
    (void)First();
  }

  return _cnode == NULL;
}

// ====================================================================
// ====================================================================
// Variable Table
// ====================================================================
// ====================================================================

RVI_VTAB::RVI_VTAB( INT num_vars, MEM_POOL *mem_pool ) :
  _size(num_vars+1),
  _mem_pool(mem_pool),
  _vtab(num_vars+1, mem_pool)
{
  // clear out the array
  _vtab.Bzero_array();
}

RVI_VTAB::~RVI_VTAB( void )
{
}

// ====================================================================
// Iterator class for RVI_VTAB
// ====================================================================

RVI_NODE *
RVI_VTAB_ITER::First( void )
{
  _did_first = TRUE;
  _vnode = NULL;
  for ( _vtab_index = 0; _vtab_index < _vtab->Size(); _vtab_index++ ) {
    _vnode = _vtab->Find(_vtab_index);
    if ( _vnode != NULL ) {
      return _vnode;
    }
  }

  return NULL;
}

RVI_NODE *
RVI_VTAB_ITER::Next( void )
{
  if ( ! _did_first ) {
    return First();
  }

  // pick up where we left off
  _vnode = NULL;
  for ( _vtab_index += 1; _vtab_index < _vtab->Size(); _vtab_index++ ) {
    _vnode = _vtab->Find(_vtab_index);
    if ( _vnode != NULL ) {
      return _vnode;
    }
  }

  return NULL;
}

BOOL
RVI_VTAB_ITER::Is_Empty( void )
{
  if ( ! _did_first ) {
    (void)First();
  }

  return _vnode == NULL;
}

// ====================================================================
// Add a load node to our variable table
// ====================================================================

RVI_NODE *
RVI_VTAB::Add_load( WN *loadwn, IDX_32 bitpos ) const
{
  RVI_NODE *vnode = _vtab[bitpos];

  if ( vnode == NULL ) {
    vnode = CXX_NEW(RVI_NODE(bitpos,loadwn,NULL),_mem_pool);
    _vtab[bitpos] = vnode;
  }
  else if ( vnode->Loadwn() == NULL ) {
    vnode->Set_loadwn(loadwn);
  }
  else {
    // we already had a load, so check to see if the result of this
    // load is larger than the load we've already seen.  Pick the
    // larger result size.
    const MTYPE cur_rtype = WN_rtype(vnode->Loadwn());
    const MTYPE new_rtype = WN_rtype(loadwn);

    if ( MTYPE_size_min(new_rtype) > MTYPE_size_min(cur_rtype) ) {
      vnode->Set_loadwn(loadwn);
    }
  }

  return vnode;
}

// ====================================================================
// Add a store node to our variable table
// ====================================================================

RVI_NODE *
RVI_VTAB::Add_store( WN *storewn, IDX_32 bitpos ) const
{
  RVI_NODE *vnode = _vtab[bitpos];

  if ( vnode == NULL ) {
    vnode = CXX_NEW(RVI_NODE(bitpos,NULL,storewn),_mem_pool);
    _vtab[bitpos] = vnode;
  }
  else if ( vnode->Storewn() == NULL ) {
    vnode->Set_storewn(storewn);
  }

  return vnode;
}

// ====================================================================
// Determine if there is a vnode in our table that matches the
// memory reference (st, offset, mtype all match)
// ====================================================================

RVI_NODE *
RVI_VTAB::Find_match( const WN *wn ) const
{
  const OPCODE   wnopc = WN_opcode(wn);
  const OPERATOR wnopr = OPCODE_operator(wnopc);
  Is_True( wnopr == OPR_STID || wnopr == OPR_LDID,
    ("RVI_VTAB::Find_match: neither stid nor ldid") );
  const ST *wnst = WN_st(wn);
  const WN_OFFSET wnoff = (wnopr == OPR_STID ? WN_store_offset(wn) :
					       WN_load_offset(wn));
  const MTYPE wnmtype = TY_mtype(ST_type(wnst));

  RVI_VTAB_ITER vtab_iter;
  RVI_NODE *vnode;
  FOR_ALL_NODE( vnode, vtab_iter, Init(this) ) {
    ST *vst;
    WN_OFFSET voff;
    MTYPE vmtype;

    if ( vnode->Loadwn() != NULL ) {
      vst = WN_st(vnode->Loadwn());
      voff = WN_load_offset(vnode->Loadwn());
      vmtype = TY_mtype(ST_type(vst));
    }
    else if ( vnode->Storewn() != NULL ) {
      vst = WN_st(vnode->Storewn());
      voff = WN_store_offset(vnode->Storewn());
      vmtype = TY_mtype(ST_type(vst));
    }
    else {
      FmtAssert( FALSE, 
		("RVI_VTAB::Find_match: bitpos %d", vnode->Bitpos()) );
    }

    if ( wnst == vst && wnoff == voff && wnmtype == vmtype ) {
      return vnode;
    }
  }

  return NULL;
}
		

// ====================================================================
// The ST of this variable
// ====================================================================

ST *
RVI_NODE::St( void ) const
{
  if ( Loadwn() != NULL ) {
    const OPCODE opc = WN_opcode(Loadwn());
    const OPERATOR opr = OPCODE_operator(opc);

    if ( opr == OPR_LDID )
      return WN_st(Loadwn());
    else {
      // constants don't have a name (yet)
      return NULL;
    }
  }
  else if ( Storewn() != NULL ) {
    return WN_st(Storewn());
  }
  else {
    FmtAssert( FALSE, ("RVI_NODE::Name: No way to determine ST") );
    return NULL;
  }
}

// ====================================================================
// The name of this variable
// ====================================================================

const char *
RVI_NODE::Name( void ) const
{
  ST *st = NULL;

  if ( Loadwn() != NULL ) {
    const OPCODE opc = WN_opcode(Loadwn());
    const OPERATOR opr = OPCODE_operator(opc);

    if ( opr == OPR_LDID )
      st = WN_st(Loadwn());
    else {
      // constants don't have a name (yet)
      return NULL;
    }
  }
  else if ( Storewn() != NULL ) {
    st = WN_st(Storewn());
  }
  else {
    FmtAssert( FALSE, ("RVI_NODE::Name: No way to determine name") );
    return "RVI-unknown";
  }

  return ST_name(st);
}

// ====================================================================
// Create a new WN to represent the "home" of this node for use in
// passing information to CG about this preg.
// ====================================================================

WN *
RVI_NODE::New_home_wn(ALIAS_MANAGER *alias_mgr) const
{
  WN *home_wn = NULL;

  if ( Loadwn() != NULL )
  {
    WN  *wn = Loadwn();

    // A formal ref of a non preg LDA is homeable (ie. it hase an addressible base)
    if ( WN_has_sym(wn) && WN_sclass(wn) == SCLASS_FORMAL_REF )
    {
      if ( WN_operator_is(wn, OPR_LDA))
      {
	ST *base = Get_ST_formal_ref_base(WN_st(wn));

	if (!ST_has_formal_preg_num(base))
	{
	  // Partial fix 653956.
          // home_wn = WN_CreateIdname(WN_lda_offset(wn),  base);
	  TY_IDX ty = WN_ty(wn);
	  home_wn = WN_CreateLdid(OPR_LDID, Pointer_type, Pointer_type,
				  WN_lda_offset(wn), base,
				  WN_ty(wn));
        }
      }
    }
    else if ( WN_operator_is(wn, OPR_LDID ))
    {
      home_wn = WN_CreateIdname( WN_load_offset(wn), WN_st(wn) );
      Copy_alias_info(alias_mgr, wn, home_wn);
    }
    else
    {
      // constants don't have a name (yet)
      home_wn = WN_COPY_Tree( wn );
    }
  }
  else if ( Storewn() != NULL )
  {
    WN  *wn = Storewn();

    if (WN_sclass(wn) != SCLASS_FORMAL_REF)
    {
      home_wn = WN_CreateIdname(WN_store_offset(wn), WN_st(wn) );
      Copy_alias_info(alias_mgr, wn, home_wn);
    }
  }
  else 
  {
    FmtAssert( FALSE, 
      ("RVI_NODE::New_home_wn: No way to determine home") );
  }

  return home_wn;
}

// ====================================================================
// Track references to this variable or constant
// NOTE: If this is the first reference for this variable in this
// block, we assume source-order, so if is_load is false, we assume
// the first reference to this variable in this block is a store.
// ====================================================================

void
RVI_NODE::Add_reference( BB_NODE *bb, BOOL is_load, MEM_POOL *pool )
{
  if ( Appearances() == NULL ) {
    Set_appearances( CXX_NEW(RVI_LRBB_LIST(), pool) );
  }

  RVI_LRBB *lrbb = Appearances()->Find( bb );
  if ( lrbb == NULL ) {
    lrbb = CXX_NEW(RVI_LRBB(bb),pool);
    Appearances()->Prepend( lrbb );

    // first reference
    if ( !is_load ) {
      lrbb->Set_first_is_store();
    }
  }

  if ( is_load ) {
    lrbb->Set_load_cnt( lrbb->Load_cnt() + 1 );
  }
  else {
    lrbb->Set_store_cnt( lrbb->Store_cnt() + 1 );
  }
}

// ====================================================================
// Is this variable a "volatile" one?  
// WARNING: returning FALSE does not imply that every reference is not
// volatile because some languages allow casting for a particular
// reference.
// ====================================================================

BOOL
RVI_NODE::Is_volatile( void ) const
{
  TY_IDX  ty = (TY_IDX) 0;
  ST     *st = NULL;
  if ( Loadwn() != NULL ) {
    ty = WN_ty(Loadwn());
    st = WN_st(Loadwn());
  }
  else if ( Storewn() != NULL ) {
    ty = WN_ty(Storewn());
    st = WN_st(Storewn());
  }

  Is_True( ty != 0, ("RVI_NODE::Is_volatile: no ty [%d]", Bitpos()) );

  if (TY_is_volatile(ty))
    return TRUE;

  // the variable may be declared volatile, but cast as non-vol,
  // in which case, we still treat as volatile (user probably is
  // clueless)
  Is_True(st != NULL,
	  ("RVI_NODE::Is_volatile: no st [%d]", Bitpos()));
  TY_IDX st_ty = ST_type(st);
  if (TY_is_volatile(st_ty))
    return TRUE;

  return FALSE;
}

// ====================================================================
// Print an individual node
// ====================================================================

void
RVI_NODE::Print( FILE *fp ) const
{
  fprintf( fp, "Bitpos: %d", Bitpos() );
  if ( Loadwn() != NULL ) {
    const OPCODE opc = WN_opcode(Loadwn());
    const OPERATOR opr = OPCODE_operator(opc);

    if ( opr == OPR_LDID ) {
      fprintf( fp, ", Loadwn: %s %s %d",
	OPCODE_name(WN_opcode(Loadwn())), ST_name(WN_st(Loadwn())),
	WN_load_offset(Loadwn()) );
    }
    else {
      fprintf( fp, ", Loadwn: " );
      fdump_tree( fp, Loadwn() );
    }
  }
  if ( Storewn() != NULL ) {
    fprintf( fp, ", Storewn: %s %s %d",
      OPCODE_name(WN_opcode(Storewn())), ST_name(WN_st(Storewn())),
      WN_store_offset(Storewn()) );
  }

  fprintf( fp, "\n" );
}

// ====================================================================
// Print the entire table
// ====================================================================

void
RVI_VTAB::Print( FILE *fp ) const
{
  fprintf( fp, "%sRVI_VTAB::Print\n%s", SBar, SBar );
  RVI_VTAB_ITER vtab_iter;
  RVI_NODE *vnode;
  FOR_ALL_NODE( vnode, vtab_iter, Init(this) ) {
    vnode->Print( fp );
  }

}

