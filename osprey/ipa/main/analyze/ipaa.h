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
/* ====================================================================
 * ====================================================================
 *
 * Module: ipaa.h
 *
 * Revision history:
 *  13-Jun-95 - Original Version
 *
 * Description:
 *
 * Define the external interfaces of the interprocedural alias analysis
 * module.
 *
 * ====================================================================
 * ====================================================================
 */

#ifndef ipaa_INCLUDED
#define ipaa_INCLUDED

#ifndef cxx_sparse_bv_INCLUDED
#include "sparse_bv.h"
#endif

#include "ipo_tlog_utils.h"
#include "ipa_option.h"

class IPAA_SUMMARY;
class IPAA;

// ====================================================================
//
// FBV: Formal bit vector
//
// This class represents a set of formals, represented either as a
// single 32-bit word bit vector, or as a pointer to a bit vector of
// 64-bit words.  It is intended for use only within a context where
// the user knows the size of the set, e.g. an IPAA_OBJECT_REF_SET.
//
// NOTE:  Elements of these sets are always indexed from zero.
//
// ====================================================================

class FBV
{
private:
    union {
	mINT32 _fbits;	// Formal bit vector
	mINT64 *_fdata;	// Pointer to formal bit vector
    } _fbv;

public:

    // Constructor:
    FBV ( INT32 fcount, MEM_POOL *pool ) {
	if ( fcount > 32 ) {
	    INT32 dw = (fcount+63)/64;
	    _fbv._fdata = (INT64 *) MEM_POOL_Alloc ( pool, dw*sizeof(INT64) );
	    BZERO ( _fbv._fdata, dw*sizeof(INT64) );
	} else
	    _fbv._fbits = 0;
    }

    FBV (const FBV& fbv, INT32 fcount, MEM_POOL *pool) {
	if (fcount > 32) {
	    INT32 dw = (fcount+63)/64;
	    _fbv._fdata = (INT64 *) MEM_POOL_Alloc ( pool, dw*sizeof(INT64) );
	    BCOPY (fbv._fbv._fdata, _fbv._fdata, dw*sizeof(INT64));
	} else
	    _fbv._fbits = fbv._fbv._fbits;
    }

    // "Destructor":
    void FBV_free ( INT32 fcount, MEM_POOL *pool ) {
	if ( fcount > 32 ) {
	    MEM_POOL_FREE ( pool, _fbv._fdata );
	}
    }

    // Tracing:
    void Print ( FILE *f, INT16 fcount ) const {
	fprintf ( f, "FBV:" );
	if ( fcount > 32 ) {
	    INT32 i;
	    for ( i = 0; i <= (fcount+63)/64; i++ ) {
		fprintf ( f, ":%016llx", _fbv._fdata[i] );
		if ( IPA_Trace_Mod_Ref ) {
			Ipa_tlog("MOD_REF", 0, ":%016llx", _fbv._fdata[i] );
		}
	    }
	} else {
	    fprintf ( f, ":%08x", _fbv._fbits );
		if ( IPA_Trace_Mod_Ref ) {
			Ipa_tlog("MOD_REF", 0, ":%08x", _fbv._fbits );
		}
	}
    }

    // Add an element to a set:
    void Add_element ( INT32 fcount, INT32 elmt ) {
	if ( fcount > 32 ) {
	    *(_fbv._fdata+elmt/64) |= (mINT64)1 << (elmt&63);
	} else {
	    _fbv._fbits |= 1 << elmt;
	}
    }

    // Check for an element in a set:
    BOOL Is_element ( INT32 fcount, INT32 elmt ) {
	if ( fcount > 32 ) {
	    return ( *(_fbv._fdata+elmt/64) & ( (mINT64)1 << (elmt&63) ) ) != 0;
	} else {
	    return ( _fbv._fbits & ( 1 << elmt ) ) != 0;
	}
    }
};

// ====================================================================
//
// IPAA_OBJECT_REF_SET: Set of object references
//
// This class represents a set of object references.  It contains a set
// represented as a sparse bit vector which contains the globals
// indexed by the global number.  It also contains two sets of the
// formals of the routine to which it is attached, each represented
// either as a single 32-bit word bit vector, or as a pointer to a bit
// vector of 64-bit words.  One set of formals represents direct
// references of the relevant type, one indirect references.
//
// When an IPAA_OBJECT_REF_SET is created, it must be given the
// relevant set sizes.
//
// ====================================================================

class IPAA_OBJECT_REF_SET
{
    friend class IPAA;			// So emission can see _data

private:
    SPARSE_BV _data;			// Sparse bit vector of globals
    FBV	_dformals;			// Direct reference formal bit vector
    FBV	_iformals;			// Indirect reference formal bit vector
    mINT32 _fcount;			// Number of formals

public:

    // Constructor:
    IPAA_OBJECT_REF_SET (INT32 fcount) :
	_fcount (fcount),
	_dformals (fcount, Malloc_Mem_Pool),
	_iformals (fcount, Malloc_Mem_Pool),
	_data () {}

    // copy constructor:
    IPAA_OBJECT_REF_SET (const IPAA_OBJECT_REF_SET& set) :
	_fcount (set._fcount),
	_data (set._data),
	_dformals (set._dformals, set._fcount, Malloc_Mem_Pool),
	_iformals (set._iformals, set._fcount, Malloc_Mem_Pool) {}

    // Destructor:
    ~IPAA_OBJECT_REF_SET () {
	_dformals.FBV_free (_fcount, Malloc_Mem_Pool);
	_iformals.FBV_free (_fcount, Malloc_Mem_Pool);
    }

    // Tracing:
    void Print ( FILE * ) const;
    void Trace ( void ) const;

    // NOTE:  Elements of these sets are always indexed from zero.

    // Add an element to a reference set:
    void Add_elmt ( INT32 elmt )
	{ _data.Set ( elmt ); }
    void Add_direct_formal_elmt ( INT32 elmt )
	{ _dformals.Add_element ( _fcount, elmt ); }
    void Add_indirect_formal_elmt ( INT32 elmt )
	{ _iformals.Add_element ( _fcount, elmt ); }

    // Check for an element in a reference set:
    BOOL Is_elmt ( INT32 elmt ) const
	{ return _data.Is_Set ( elmt ); }
    BOOL Is_direct_formal_elmt ( INT32 elmt )
	{ return _dformals.Is_element ( _fcount, elmt ); }
    BOOL Is_indirect_formal_elmt ( INT32 elmt )
	{ return _iformals.Is_element ( _fcount, elmt ); }

    // NOTE:  The set operations on these objects do not operate on
    // the formal reference sets, since those are incomparable, i.e.
    // they refer to a different set of formals for each node.

    // Destructive union (2-address) with another reference set:
    void Union_2 ( const IPAA_OBJECT_REF_SET *set ) {
	_data.Union_2 (set->_data);
    }
      
    // Destructive (2-address) union of another set into this one,
    // returning TRUE iff this set changes as a result:
    BOOL Union_2_diff ( const IPAA_OBJECT_REF_SET *sbv ) {
	return _data.Union_2_diff (sbv->_data);
    }
};

// ====================================================================
//
// IPAA_NODE_INFO: Node information for IP alias analysis.
//
// ====================================================================

// We will represent the formal map as an array of pointers to what is,
// here, an opaque class:
class IPAA_FORMAL_MAP;

class IPAA_NODE_INFO
{
private:
    IPAA_OBJECT_REF_SET *_eref;	// Set of exposed uses of this node
    IPAA_OBJECT_REF_SET *_def;	// Set of maybe definitions of this node
    IPAA_OBJECT_REF_SET *_kill;	// Set of definite kills of this node
    IPAA_FORMAL_MAP **_fmap;	// Formal map
    mINT32 _pu_info;		// Summary file IPAA_PU_INFO index
    mINT32 _seq_id;		// Traversal-order sequence ID
    mINT32 _fcount;		// Formal count
    mINT16 _changed_iter;	// Last iteration where changed
    mINT16 _flags;		// Attributes of this node

    // Masks for _flags:
    typedef enum {
	INITIALIZED		= 0x01,	// Have merged local summary info
	BACK_EDGE		= 0x02,	// Contains recursive call
	DEPENDS_ON_BACK_EDGE	= 0x04,	// Indirect path to recursive call
	INDIRECT_EDGE		= 0x08,	// Contains an indirect call
	PASS_FORMALS_BYVAL	= 0x10,	// Passes some formals as val actuals
	PASS_GLOBALS_BYREF	= 0x20,	// Passes some globals as ref actuals
	UNKNOWN_CALL_EFFECTS	= 0x40,	// Has calls with unknown effects
	// => mod/ref must include all addr_taken global vars
	VISITED			= 0x80,	// Have visited on first graph walk
	LOCAL_ADDR_TAKEN_MOD	= 0x100, // Modified local addr_taken flag(s)
	FULL_MASK		= 0x1ff,
    } FLAG_MASK;

public:
    // Construct an empty set of IPAA node information:
    IPAA_NODE_INFO ( INT32 formal_count ) {
	BZERO ( this, sizeof(IPAA_NODE_INFO) );
	_fcount = formal_count;
    }

    // copy constructor (used by cloning)
    // ***NODE*** we only copy those fields that are used by consumers of
    // the mod/ref info.  Fields used only during the mod/ref analysis
    // phases are cleared.
    IPAA_NODE_INFO (const IPAA_NODE_INFO& node_info) :
	_fmap (NULL),
	_pu_info (-1),
	_seq_id (-1),
	_fcount (node_info._fcount),
	_changed_iter (-1),
	_flags (node_info._flags) {

	if (node_info._eref) {
	    _eref = CXX_NEW (IPAA_OBJECT_REF_SET (*node_info._eref),
			     Malloc_Mem_Pool);
	}
	if (node_info._def) {
	    _def = CXX_NEW (IPAA_OBJECT_REF_SET (*node_info._def),
			     Malloc_Mem_Pool);
	}
	if (node_info._kill) {
	    _kill = CXX_NEW (IPAA_OBJECT_REF_SET (*node_info._kill),
			     Malloc_Mem_Pool);
	}
    }

    void Free_Ref_Sets () {
	if (_eref) CXX_DELETE (_eref, Malloc_Mem_Pool);
	if (_def) CXX_DELETE (_def, Malloc_Mem_Pool);
	if (_kill) CXX_DELETE (_kill, Malloc_Mem_Pool);
    }

    // Tracing:
    void Print ( FILE *, const char *pfx = NULL ) const;
    void Trace ( const char *pfx = NULL ) const;

    // Simple field access:
    IPAA_FORMAL_MAP **Get_fmap() const		{ return _fmap; }
    void Set_fmap ( IPAA_FORMAL_MAP **f )	{ _fmap = f; }
    IPAA_FORMAL_MAP *Get_fmap ( INT32 i ) const {
	return (_fmap == NULL) ? NULL : _fmap[i];
    }
    void Set_fmap ( INT32 i, IPAA_FORMAL_MAP *f ) { _fmap[i] = f; }
    INT32 Get_pu_info() const			{ return _pu_info; }
    void Set_pu_info ( INT32 cnt )		{ _pu_info = cnt; }
    INT32 Get_seq_id() const			{ return _seq_id; }
    void Set_seq_id ( INT32 seq )		{ _seq_id = seq; }
    INT32 Get_fcount() const			{ return _fcount; }
    void Set_fcount ( INT32 cnt )		{ _fcount = cnt; }
    INT32 Get_changed_iter() const		{ return _changed_iter; }
    void Set_changed_iter ( INT32 i )		{ _changed_iter = i; }

    // Flag field access:
    BOOL Is_initialized() const		{ return _flags & INITIALIZED; }
    void Set_initialized ()		{ _flags |= INITIALIZED; }
    BOOL Has_back_edge() const		{ return _flags & BACK_EDGE; }
    void Set_has_back_edge ()		{ _flags |= BACK_EDGE; }
    BOOL Depends_on_back_edge() const	{
	return _flags & DEPENDS_ON_BACK_EDGE;
    }
    void Set_depends_on_back_edge ()	{ _flags |= DEPENDS_ON_BACK_EDGE; }
    BOOL Has_indirect_edge() const	{ return _flags & INDIRECT_EDGE; }
    void Set_has_indirect_edge ()	{ _flags |= INDIRECT_EDGE; }
    BOOL Passes_formals_by_value() const {
	return _flags & PASS_FORMALS_BYVAL;
    }
    void Set_passes_formals_by_value () { _flags |= PASS_FORMALS_BYVAL; }
    BOOL Passes_globals_by_reference() const {
	return _flags & PASS_GLOBALS_BYREF;
    }
    void Set_passes_globals_by_reference () { _flags |= PASS_GLOBALS_BYREF; }
    BOOL Has_unknown_call_effects() const {
	return _flags & UNKNOWN_CALL_EFFECTS;
    }
    void Set_has_unknown_call_effects () { _flags |= UNKNOWN_CALL_EFFECTS; }
    BOOL Is_visited() const		{ return _flags & VISITED; }
    void Set_visited ()			{ _flags |= VISITED; }
    BOOL Local_addr_taken_mods() const {
	return _flags & LOCAL_ADDR_TAKEN_MOD;
    }
    void Set_local_addr_taken_mods ()	 { _flags |= LOCAL_ADDR_TAKEN_MOD; }

    // Compress a dense reference set:
    void Compress ( MEM_POOL *pool ) const;

    // Create the sets of exposed uses, definitions, or kills:
    void New_eref_set ( INT32 fsize );
    void New_def_set  ( INT32 fsize );
    void New_kill_set ( INT32 fsize );

    // Get the sets of exposed uses, definitions, or kills:
    IPAA_OBJECT_REF_SET * Get_eref_set() const { return _eref;};
    IPAA_OBJECT_REF_SET * Get_def_set()  const { return _def;};
    IPAA_OBJECT_REF_SET * Get_kill_set() const { return _kill;};

    // Add an exposed use, definition, or kill:
    void Add_eref_elmt( INT32 g ) { Get_eref_set()->Add_elmt(g); };
    void Add_def_elmt( INT32 g )  { Get_def_set ()->Add_elmt(g); };
    void Add_kill_elmt( INT32 g ) { Get_kill_set()->Add_elmt(g); };

    // Add a formal exposed use, definition, or kill:
    void Add_formal_dref_elmt( INT32 f )
	{ Get_eref_set()->Add_direct_formal_elmt(f); };
    void Add_formal_iref_elmt( INT32 f )
	{ Get_eref_set()->Add_indirect_formal_elmt(f); };
    void Add_formal_dmod_elmt ( INT32 f )
	{ Get_def_set ()->Add_direct_formal_elmt(f); };
    void Add_formal_imod_elmt ( INT32 f )
	{ Get_def_set ()->Add_indirect_formal_elmt(f); };
    void Add_formal_dkill_elmt( INT32 f )
	{ Get_kill_set()->Add_direct_formal_elmt(f); };
    void Add_formal_ikill_elmt( INT32 f )
	{ Get_kill_set()->Add_indirect_formal_elmt(f); };

    // Check for an exposed use, definition, or kill:
    BOOL Is_eref_elmt( INT32 g ) const
	{ return Get_eref_set()->Is_elmt(g); };
    BOOL Is_def_elmt( INT32 g ) const
	{ return Get_def_set ()->Is_elmt(g); };
    BOOL Is_kill_elmt( INT32 g ) const
	{ return Get_kill_set()->Is_elmt(g); };

    // Check for a formal exposed use, definition, or kill:
    BOOL Is_formal_dref_elmt ( INT32 f ) const
	{ return Get_eref_set()->Is_direct_formal_elmt(f); };
    BOOL Is_formal_iref_elmt ( INT32 f ) const
	{ return Get_eref_set()->Is_indirect_formal_elmt(f); };
    BOOL Is_formal_dmod_elmt ( INT32 f ) const
	{ return Get_def_set ()->Is_direct_formal_elmt(f); };
    BOOL Is_formal_imod_elmt ( INT32 f ) const
	{ return Get_def_set ()->Is_indirect_formal_elmt(f); };
    BOOL Is_formal_dkill_elmt( INT32 f ) const
	{ return Get_kill_set()->Is_direct_formal_elmt(f); };
    BOOL Is_formal_ikill_elmt( INT32 f ) const
	{ return Get_kill_set()->Is_indirect_formal_elmt(f); };

    // Is an actual in the formal map?
    BOOL Is_actual_in_map ( INT32 f, UINT32 glob ) const;
}; // IPAA_NODE_INFO

// ====================================================================
//
// IPAA: Master IP alias analysis class
//
// ====================================================================

class IPAA
{
private:
  MEM_POOL *_m;		// Memory pool to use
  IPA_CALL_GRAPH *_cg;	// Callgraph, not valid between public calls
  IPAA_SUMMARY *_summary;	// Summary for emission

  // Emit various names to the summary string table:
  INT32 Emit_name ( const char *name );
  INT32 Emit_global_name ( INT32 merged_id );
  INT32 Emit_pu_name ( const char *name );

  // Construct an IPAA_SYMBOL_REF for a global symbol:
  INT32 Emit_global ( INT32 merged_id, BOOL reuse_ok );

  // Emit a single PU node's information to the output files:
  void Emit_IPAA_node ( const IPA_NODE &node );

  // Emit a PU's callsite mapping information to the output files:
  void Emit_IPAA_callsites ( IPA_NODE &node );

public:
  IPAA ( MEM_POOL *m );

  void Set_IPAA_Mem_Pool ( MEM_POOL *pool )	{ _m = pool; };
  MEM_POOL * Get_IPAA_Mem_Pool ( void ) const	{ return _m; };
  IPA_CALL_GRAPH * Get_cg ( void ) const	{ return _cg; }

  void Do_Simple_IPAA ( IPA_CALL_GRAPH &cg );

  void Emit_Simple_IPAA ( IPA_CALL_GRAPH &cg );
};


extern IPAA_OBJECT_REF_SET* icall_eref;
extern IPAA_OBJECT_REF_SET *icall_def;
extern IPAA_OBJECT_REF_SET *icall_kill;

#endif /* ipaa_INCLUDED */
