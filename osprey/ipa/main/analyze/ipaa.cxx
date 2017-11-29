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


// ====================================================================
// ====================================================================
//
// Module: ipaa.cxx
// $Source: /proj/osprey/CVS/open64/osprey1.0/ipa/main/analyze/ipaa.cxx,v $
//
// Revision history:
//  14-Jun-95 - Original Version
//
// Description:
//
// Interprocedural alias analysis.
//
// TODO:  (* is required for correctness, + for completeness)
//
// * 3)	Need to handle varargs parms in the variable section.
//
//   4) We are currently not considering parts of objects, most
//	seriously COMMON blocks.
//
//   5) Should speed up iterations after the first, probably via a
//	worklist or equivalent.
//
//   6) Should avoid replicating work for multiple calls to the same
//	routine from the same caller.
//
// Testing:
//
// * 1) Test treatment of indirect calls.
//
// * 2) Test recursion treatment.
//
// * 3) Test implementation of formal points-to mapping stage.
//
// ====================================================================
// ====================================================================


#include <stdint.h>
#if defined(BUILD_OS_DARWIN)
#include <darwin_elf.h>
#else /* defined(BUILD_OS_DARWIN) */
#include <elf.h>
#endif /* defined(BUILD_OS_DARWIN) */
#include <cmplrs/host.h>

#include "assert.h"
#define USE_STANDARD_TYPES		// override unwanted defines in defs.h
#include "defs.h"
#include "cxx_memory.h"
#include "cxx_hash.h"
#include "erglob.h"
#include "glob.h"
#include "mempool.h"
#include "tracing.h"

#include "strtab.h"
#include "stab.h"
#include "wn.h"
#include "const.h"
#include "pu_info.h"

#include "irbdata.h"			// for INITO
#include "dwarf_DST_mem.h"		// for DST_TYPE

#include "ipc_defs.h"
#include "ipc_weak.h"			// for tmpdir
#include "ipc_file.h"

#include "ipl_summary.h"		// for summary info data structures
#include "ipa_cg.h"
#include "ipa_option.h"
#include "ipa_solver.h"
#include "ipa_summary.h"
#include "ipaa.h"
#include "opt_ipaa_io.h"
#include "opt_ipaa_summary.h"
#include "ipc_symtab_merge.h"

// In lieu of non-conforming ld header with uncooperative types:
extern "C" void add_to_tmp_file_list ( char *pathname );

// Memory pool to be used in this module:
MEM_POOL *IPAA_Pool = NULL;

// Merged external symbol table size:
INT Mext_Size = 0;

// Are we tracing?  These are globals rather than class members because
// we want access to them throughout this module.  Perhaps they should
// be static members of class IPAA.
BOOL Trace_IPAA     = FALSE;	// General IPAA trace
BOOL Trace_Detail   = FALSE;	// Detail IPAA trace
BOOL Trace_Stats    = FALSE;	// IPAA statistics trace
BOOL Trace_Iterator = FALSE;	// IPAA solver's iterator trace
BOOL Log_IPAA       = FALSE;	// Trace log for IPAA


// global mod/ref list for addr_taken functions.
IPAA_OBJECT_REF_SET *icall_eref;
IPAA_OBJECT_REF_SET *icall_def;
IPAA_OBJECT_REF_SET *icall_kill;


// ====================================================================
//
// Get_Merged_Sym_ID: Get a global index in the merged symbol list.
//
// ====================================================================

static inline UINT32
Get_Merged_Sym_ID ( SUMMARY_SYMBOL *sym )
{
  return ST_IDX_index(sym->St_idx());
}

// ====================================================================
// ====================================================================
//
// IPAA_OBJECT_REF_SET
//
// This class contains a set of object references.  
//
// ====================================================================
// ====================================================================

void
IPAA_OBJECT_REF_SET::Print ( FILE *f ) const
{
  fprintf ( f, "ORS:f%3d: d", _fcount );
  _dformals.Print ( f, _fcount );
  fprintf ( f, ": i" );
  _iformals.Print ( f, _fcount );
  fprintf ( f, "\n\t\t" );
}

// ====================================================================

void
IPAA_OBJECT_REF_SET::Trace ( void ) const
{
  Print ( TFile );
}


// ====================================================================
// ====================================================================
//
// IPAA_FORMAL_MAP
//
// This class contains a set of globals to which a particular formal
// may be mapped (i.e. the formal may point to the global).  Because
// we expect a large number of formals to map to no globals, the data
// structure is built only on demand.  Therefore, queries against a
// NULL map are valid and should return the default answer.
//
// ====================================================================
// ====================================================================

class IPAA_FORMAL_MAP
{
 public:
  
  // An element of a map is a global, represented as a merged symbol
  // table index:
  typedef INT32 MAP_ELMT;
  static const MAP_ELMT MAP_NONE = -1;

 private:

  mBOOL _junk;		// The formal is mapped to unknown objects
  mINT16 _count;	// Number of mapped objects
  union {
    MAP_ELMT _single;	// A single map element (if _count == 1)
    MAP_ELMT *_mvec;	// Vector of map elements (if _count > 1)
  };

  void Expand ( INT32 min_size = 0 );

 public:

  // Constructor / destructor:
  IPAA_FORMAL_MAP ( void );
  ~IPAA_FORMAL_MAP ( void ) {}

  // Tracing:
  void Print ( FILE * ) const;
  void Trace ( void ) const;

  // Is this formal mapped to anything?
  BOOL Is_empty () const	{ return _junk && _count == 0; }
  INT32 Get_size () const	{ return _count + (_junk == TRUE); }

  // Is this formal mapped to objects unknown?
  BOOL Get_junk () const	{ return _junk; }
  void Set_junk ()		{ _junk = TRUE; }

  // Does this map contain a given symbol?
  BOOL Is_elmt ( INT32 elmt ) const;
  BOOL Add_elmt ( INT32 elmt );

  // Iterate through the elements of the map:
  MAP_ELMT Get_elmt ( INT32 index ) const {
    return ( _count == 1 ) ? _single : _mvec[index];
  }

  // Destructive union (2-address) with another formal map:
  BOOL Union_2 ( const IPAA_FORMAL_MAP *set );
};

// ====================================================================
//
// IPAA_FORMAL_MAP::Expand
//
// Expand the vector to at least the given size.  If zero, double the
// existing map to at least two.  The map always at least doubles so
// that gradual expansion to big sets won't be overly expensive.
//
// ====================================================================

void
IPAA_FORMAL_MAP::Expand ( INT32 min_size )
{
  MAP_ELMT *data;
  INT32 i;

  // Find a power of two to use:
  if ( min_size == 0 ) {
    if ( _count == 0 ) {
      min_size = 2;
    } else {
      min_size = _count*2;
    }
  } else {
    if ( min_size <= _count ) {
      return;
    }
    if ( (min_size & (min_size-1)) != 0 ) {
      while ( (min_size & (min_size-1)) != 0 ) {
	min_size &= (min_size-1);
      }
      min_size += min_size;
    }
  }

  // Go allocate the space:
  if ( _count <= 1 ) {
    data = (MAP_ELMT *) MEM_POOL_Alloc
		( IPAA_Pool, sizeof(MAP_ELMT) * min_size );
    if ( data == NULL ) {
      ErrMsg ( EC_No_Mem, "Expand (new)" );
    }
    if ( _count == 1 ) {
      data[0] = _single;
    }

  } else {
    data = (MAP_ELMT *) MEM_POOL_Realloc
		( IPAA_Pool, _mvec,
		  sizeof(MAP_ELMT) * _count,
		  sizeof(MAP_ELMT) * min_size );
    if ( data == NULL ) {
      ErrMsg ( EC_No_Mem, "Expand (enlarge)" );
    }
  }

  // Clear new data and update class members:
  for ( i = _count; i < min_size; i++ ) {
    data[i] = MAP_NONE;
  }
  _mvec = data;
  _count = min_size;
}

// ====================================================================
//
// IPAA_FORMAL_MAP::IPAA_FORMAL_MAP -- Constructor
//
// Just clear everything.
//
// ====================================================================

IPAA_FORMAL_MAP::IPAA_FORMAL_MAP ()
{
  // Initialize the fields:
  _junk = FALSE;
  _count = 0;
  _mvec = NULL;
}


// ====================================================================
//
// IPAA_FORMAL_MAP::Print  /  IPAA_FORMAL_MAP::Trace
//
// Trace to an arbitrary / the trace file
//
// ====================================================================

void
IPAA_FORMAL_MAP::Print ( FILE *f ) const
{
  INT16 i;

  if ( this == NULL ) {
    fprintf ( f, "<NULL>\n" );
    return;
  }
  fprintf ( f, "%s[%d] ", _junk ? "arbitrary + " : "", _count );
  if ( _count == 0 ) {
    fprintf ( f, "EMPTY\n" );
  } else if ( _count == 1 ) {
    fprintf ( f, "%d\n", _single );
  } else {
    for ( i = 0; i < _count; i++ ) {
      if ( _mvec[i] != MAP_NONE ) {
	fprintf ( f, "%d ", _mvec[i] );
      }
    }
    fprintf ( f, "\n" );
  }
}

// ====================================================================

void
IPAA_FORMAL_MAP::Trace ( void ) const
{
  Print ( TFile );
}

// ====================================================================
//
// IPAA_FORMAL_MAP::Is_elmt
//
// Is the given object (given by its merged symbol index) in the map?
//
// ====================================================================

BOOL
IPAA_FORMAL_MAP::Is_elmt ( INT32 elmt ) const
{
  INT16 i;

  if ( _count == 0 ) return FALSE;
  if ( _count == 1 ) return ( _single == elmt );
  for ( i = 0; i < _count; i++ ) {
    if ( _mvec[i] == elmt )	return TRUE;	// Found it
    if ( _mvec[i] == MAP_NONE ) return FALSE;	// Off the end
    if ( _mvec[i] > elmt )	return FALSE;	// Past the element
  }
  return FALSE;
}

// ====================================================================
//
// IPAA_FORMAL_MAP::Add_elmt
//
// Add the given object (given by its merged symbol index) to the map,
// unless it's already there.  Return TRUE if the element was new.
//
// ====================================================================

BOOL
IPAA_FORMAL_MAP::Add_elmt ( INT32 elmt )
{
  INT16 i;
  INT32 old;
  BOOL added = FALSE;

  if ( _count == 0 ) {
    _count = 1;
    _single = elmt;
    added = TRUE;
  } else if ( _count == 1 ) {
    if ( _single != elmt ) {
      old = _single;
      Expand ( 4 );
      if ( old < elmt ) {
	_mvec[0] = old;
	_mvec[1] = elmt;
      } else {
	_mvec[0] = elmt;
	_mvec[1] = old;
      }
      added = TRUE;
    }
  } else {
    for ( i = 0; i < _count; i++ ) {
      if ( _mvec[i] == elmt )	return FALSE;	// Found it
      if ( _mvec[i] == MAP_NONE ) {	// Off the end
	_mvec[i] = elmt;
	added = TRUE;
	goto trace_and_exit;
      }
      if ( _mvec[i] > elmt ) {		// Past the element -- insert
	INT16 k = _count;
	if ( _mvec[_count-1] != MAP_NONE ) {
	  Expand ();
	  while ( _mvec[--k] == MAP_NONE ) {};
	  while ( k >= i ) {
	    _mvec[k+1] = _mvec[k];
	    k--;
	  }
	  _mvec[i] = elmt;
	  added = TRUE;
	  goto trace_and_exit;
	}
      }
    }

    // We've fallen off the end -- expand and insert it at i:
    Expand ();
    _mvec[i] = elmt;
    added = TRUE;
  }

 trace_and_exit:
  if ( Trace_Detail ) {
    fprintf ( TFile, "<ipaa> Add element %d to formals:", elmt );
    Trace ();
  }

  return added;
}

// ====================================================================
//
// IPAA_FORMAL_MAP::Union_2
//
// Destructive union of the given map into this.  Return whether the
// union added anything.
//
// ====================================================================

BOOL
IPAA_FORMAL_MAP::Union_2 ( const IPAA_FORMAL_MAP *map )
{
  INT16 i, t, m, cnt;
  BOOL changed;
  MAP_ELMT valt, valm;

  // Deal with the trivial cases for map:
  if ( map->_count == 0 ) {
    return FALSE;
  } else if ( map->_count == 1 ) {
    return Add_elmt ( map->_single );
  } else if ( map->_mvec[1] == MAP_NONE ) {
    // There's only one real element:
    return Add_elmt ( map->_mvec[0] );
  }

  // There are at least two elements in map -- look at this:
  if ( this->_count == 0 ) {
    Expand ( map->_count );
    bcopy ( _mvec, map->_mvec, map->_count * sizeof(MAP_ELMT) );
    return TRUE;
  } else if ( this->_count <= 1 || this->_mvec[1] == MAP_NONE ) {
    valt = ( this->_count <= 1 ) ? this->_single : this->_mvec[0];
    Expand ( map->_count );
    bcopy ( _mvec, map->_mvec, map->_count * sizeof(MAP_ELMT) );
    (void) Add_elmt ( valt );
    return TRUE;
  }

  // Each has at least two elements -- merge more carefully:
  // How much empty space in this?
  for ( cnt = _count; cnt > 0; cnt-- ) {
    if ( this->_mvec[cnt-1] != MAP_NONE ) break;
  }
  if ( cnt + map->_count > this->_count ) {
    // Not enough for map's elements:
    Expand ( cnt + map->_count );
  }

  // Now merge to the end of the list:
  changed = FALSE;
  t = cnt-1;
  valt = this->_mvec[t];
  for ( m = map->_count-1; m>=0; m-- ) {
    if ( (valm = map->_mvec[m]) != MAP_NONE ) break;
  }
  for ( i = this->_count-1; i>=0; i-- ) {
    if ( valt >= valm ) {
      this->_mvec[i] = valt;
      if ( valt == valm ) {
	m--;
	valm = (m>=0) ? map->_mvec[m] : -1;
      }
      t--;
      valt = (t>=0) ? this->_mvec[t] : -1;
    } else {
      this->_mvec[i] = valm;
      changed = TRUE;
      m--;
      valm = (m>=0) ? map->_mvec[m] : -1;
    }
    if ( m < 0 && t < 0 ) break;
  }

  // Finally, shift back to the beginning of the list:
  if ( i > 0 ) {
    for ( t = 0; i < this->_count; t++, i++ ) {
      this->_mvec[t] = this->_mvec[i];
    }
    for ( ; t < this->_count; t++ ) {
      this->_mvec[t] = MAP_NONE;
    }
  }

  return changed;
}

// ====================================================================
// ====================================================================
//
// IPAA_NODE_INFO
//
// This class contains the information associated with a callgraph node
// which is retained throughout IP alias analysis.
//
// ====================================================================
// ====================================================================

void
IPAA_NODE_INFO::Print ( FILE *f, const char *pfx ) const
{
  INT16 fid;

  if ( pfx != NULL ) {
    sprintf ( Modref_Buf,
	      "<ipaa> NODE_INFO: %s (iter%d : pu%d) -- "
	      "changed iter %d%s%s%s%s%s%s\n",
	      pfx, _fcount, _changed_iter, _pu_info,
	      Has_back_edge() ? ", BE" : "",
	      Depends_on_back_edge() ? ", DBE" : "",
	      Has_indirect_edge() ? ", IE" : "",
	      Passes_formals_by_value() ? ", FVAL" : "",
	      Passes_globals_by_reference() ? ", GREF" : "",
	      Has_unknown_call_effects() ? ", UNK" : "" );
    fprintf (f, "%s", Modref_Buf);
	if ( IPA_Trace_Mod_Ref ) {
		Ipa_tlog("MOD_REF", 0, "%s", Modref_Buf);
	}
  }
  fprintf ( f, "<ipaa>\tEREF #\t" );
  if ( IPA_Trace_Mod_Ref )
    Ipa_tlog("MOD_REF", 0, "%s", "<ipaa>  EREF #");
  _eref->Print ( f );
  fprintf ( f, "<ipaa>\tDEF  #\t" );
  if ( IPA_Trace_Mod_Ref )
    Ipa_tlog("MOD_REF", 0, "%s", "<ipaa>  DEF #");
  _def->Print ( f );
  fprintf ( f, "<ipaa>\tKILL #\t" );
  if ( IPA_Trace_Mod_Ref )
    Ipa_tlog("MOD_REF", 0, "%s", "<ipaa>  KILL #");
  _kill->Print ( f );
  if ( _fmap != NULL ) {
    for ( fid = 0; fid < _fcount; fid++ ) {
      fprintf ( f, "<ipaa>\tFMAP %d #\t", fid );
      if ( IPA_Trace_Mod_Ref )
          Ipa_tlog("MOD_REF", 0, "<ipaa>  FMAP %d #  ", fid);
      Get_fmap(fid)->Print ( f );
    }
  }
}

// ====================================================================

void
IPAA_NODE_INFO::Trace ( const char *pfx ) const
{
  Print ( TFile, pfx );
}

// ====================================================================

void
IPAA_NODE_INFO::New_eref_set ( INT32 fsize )
{
    _eref = CXX_NEW ( IPAA_OBJECT_REF_SET ( fsize ), Malloc_Mem_Pool );
}

// ====================================================================

void
IPAA_NODE_INFO::New_def_set ( INT32 fsize )
{
  _def = CXX_NEW ( IPAA_OBJECT_REF_SET ( fsize ), Malloc_Mem_Pool );
}

// ====================================================================

void
IPAA_NODE_INFO::New_kill_set ( INT32 fsize )
{
  _kill = CXX_NEW ( IPAA_OBJECT_REF_SET ( fsize ), Malloc_Mem_Pool );
}

// ====================================================================
//
// IPAA_NODE_INFO::Is_actual_in_map
//
// Is an actual in the formal map?  The formal of interest (index from
// 0) is the first parameter, the global being queried (merged global
// index) is the second.
//
// ====================================================================

BOOL
IPAA_NODE_INFO::Is_actual_in_map ( INT16 f, UINT32 glob ) const
{
  // If the map isn't there, the actual isn't either:
  if ( Get_fmap() == NULL
    || f >= Get_fcount()
    || Get_fmap(f) == NULL )
  {
    return FALSE;
  }

  // Otherwise, check for the global:
  return Get_fmap(f)->Is_elmt ( glob );
}

// ====================================================================
//
// Force_Map
//
// Force a formal map for the given node and formal (unless the formal
// number is too large).  The formal of interest (index from 0) is the
// second parameter.  The first parameter is an IPAA_NODE_INFO object.
// This is not a class member function because we don't want to expose
// it outside.
//
// Return the (new or old) formal map.
//
// ====================================================================

IPAA_FORMAL_MAP *
Force_Map ( IPAA_NODE_INFO &info, INT16 f )
{
  IPAA_FORMAL_MAP **mapvec;
  IPAA_FORMAL_MAP *map;

  // Make sure the formal number is within range for the function.  It
  // is legal for it to be too large (for a varargs routine), so we
  // don't generate an error -- just return NULL:
  if ( f >= info.Get_fcount() ) return NULL;

  // If the map isn't there, make one:
  if ( info.Get_fmap() == NULL ) {
    mapvec = (IPAA_FORMAL_MAP **) MEM_POOL_Alloc
	  ( IPAA_Pool, sizeof(IPAA_FORMAL_MAP *) * info.Get_fcount() );
    if ( mapvec == NULL ) {
      ErrMsg ( EC_No_Mem, "Force_Map: map vector" );
    }
    bzero ( mapvec, sizeof(IPAA_FORMAL_MAP *) * info.Get_fcount() );
    info.Set_fmap ( mapvec );
  }
  if ( info.Get_fmap(f) == NULL ) {
    map = (IPAA_FORMAL_MAP *)
		CXX_NEW ( IPAA_FORMAL_MAP (), IPAA_Pool );
    if ( map == NULL ) {
      ErrMsg ( EC_No_Mem, "Force_Map: map" );
    }
    bzero ( map, sizeof(IPAA_FORMAL_MAP) );
    info.Set_fmap ( f, map );
  }

  // Return the map, new or old:
  return info.Get_fmap(f);
}

// ====================================================================
//
// Add_Actual_To_Map
//
// Add an actual to the formal map.  The formal of interest (index from
// 0) is the second parameter, the global being queried (merged global
// index) is the third.  The first parameter is an IPAA_NODE_INFO
// object -- this is not a class member function because we don't want
// to expose it outside.
//
// Return TRUE iff the actual is added, i.e. if it was inserted in a
// map which did not already contain it.
//
// ====================================================================

static BOOL
Add_Actual_To_Map ( IPAA_NODE_INFO &info, INT16 f, UINT32 glob )
{
  IPAA_FORMAL_MAP *tmap;

  // Check that we can actually get a map:
  if ( ( tmap = Force_Map ( info, f ) ) != NULL ) {
    // Add the global:
    return tmap->Add_elmt ( glob );
  }

  // Otherwise, we didn't add anything:
  return FALSE;
}

// ====================================================================
//
// Add_Map_To_Map
//
// Merge a formal map into another formal map.  The formal target map
// is given by its IPAA_NODE_INFO and formal number.  The source formal
// map is the last parameter.
//
// Return TRUE iff the merge changes the target formal map.
//
// ====================================================================

static BOOL
Add_Map_To_Map (
  IPAA_NODE_INFO &info,		// The node of the target map
  INT16 f,			// Formal number of the target map
  IPAA_FORMAL_MAP &smap )	// Source formal map
{
  IPAA_FORMAL_MAP *tmap;

  // Check that we can actually get a map:
  if ( ( tmap = Force_Map ( info, f ) ) != NULL
    && &smap != NULL 
    && tmap != &smap )
  {
    // Merge the source map:
    return tmap->Union_2 ( &smap );
  }

  // Otherwise, we didn't add anything:
  return FALSE;
}

// ====================================================================
//
// Force_IPAA_Info
//
// This routine, given a callgraph node, makes sure it has an IPAA
// node information struct attached, and initilizes it if necessary.
//
// ====================================================================

IPAA_NODE_INFO *
Force_IPAA_Info ( IPA_NODE &pu )
{
  IPAA_NODE_INFO *pu_info = pu.Mod_Ref_Info();

  if ( pu_info == NULL ) {
    INT16 fcnt = pu.Summary_Proc()->Get_formal_count();
    pu.Set_Mod_Ref_Info( CXX_NEW ( IPAA_NODE_INFO(fcnt), IPAA_Pool ) );
    pu_info = pu.Mod_Ref_Info();
  }
  return pu_info;
}

// ====================================================================
// ====================================================================
//
// IPAA_STEP
//
// This class carries the information associated with processing a
// single node in the IP alias analysis, and defines the fundamental
// functions involved in IP alias analysis.
//
// ====================================================================
// ====================================================================

// Type to return sort of actual symbol:
typedef enum {
  ACTUAL_KIND_NONE =	0,	// None of the possibilities below
  ACTUAL_KIND_GLOBAL,		// Global (externally visible)
  ACTUAL_KIND_FORMAL,		// Formal parameter
  ACTUAL_KIND_LOCAL,		// Local
  ACTUAL_KIND_PSTATIC,		// pu-level static
  ACTUAL_KIND_FSTATIC,		// file-level static
} ACTUAL_KIND;

class IPAA_STEP
{
private:

  IPA_CALL_GRAPH *_cg;		// The call graph
  IPA_NODE *_nd;		// The node being processed
  IPAA_NODE_INFO *_info;	// CG node information for IPAA
  mINT32 _fix;			// File index for summary information
  mINT16 _fsize;		// Number of formals
  SUMMARY_SYMBOL* symbols;	// Symbol reference summary
  SUMMARY_GLOBAL* globals;// Global variable reference summary
  SUMMARY_ACTUAL *actuals;	// Actual parameter summary
  SUMMARY_FORMAL* formals;	// Formal Parameter summary

public:

  // Constructor / destructor:
  IPAA_STEP ( IPA_CALL_GRAPH &graph, IPA_NODE &cg_node );
  ~IPAA_STEP ( void ) {}

  // Field access:
  SUMMARY_ACTUAL* Get_actuals (void) const	{ return actuals; }

  // Initialize global pointers to summary information for tracing:
  void Init_Summary_Pointers ( void );

  // Tracing:
  void Print ( FILE * ) const;
  void Trace ( void ) const;

  // Get a global symbol ID in the merged symbol list:
  UINT32 Get_merged_global_id ( SUMMARY_GLOBAL *gs );

  // Initialize reference info for a node:
  BOOL Initialize_Ref_Info ();

  // Get information about actual parameter:
  ACTUAL_KIND Get_actual_info (
    const SUMMARY_ACTUAL &actual,	// Actual parameter
    SUMMARY_SYMBOL * &sym,	// Return if a symbol
    ST_IDX &merged_id,			// Return if global
    INT32 &formal_num ) const;		// Return if formal

  // Finalize reference info for a node:
  void Finalize_Ref_Info ();
};

// ====================================================================
//
// IPAA_STEP::Init_Summary_Pointers
//
// Initialize global pointers to summary information for tracing.
//
// ====================================================================

void
IPAA_STEP::Init_Summary_Pointers ( void )
{
  Ipl_Summary_Symbol = symbols;
}

// ====================================================================
//
// IPAA_STEP constructor
//
// This routine initializes an IPAA_STEP structure with the node
// pointer and the IPAA_NODE_INFO struct, creating it if needed.
//
// ====================================================================

IPAA_STEP::IPAA_STEP ( IPA_CALL_GRAPH &callgraph, IPA_NODE &cg_node )
{
  bzero ( this, sizeof(IPAA_STEP) );
  this->_cg = &callgraph;
  this->_nd = &cg_node;
  _fsize = cg_node.Summary_Proc()->Get_formal_count();

  // Initialize the IPAA node information structure if needed:
  this->_info = Force_IPAA_Info ( *_nd );

  // Initialize the ref/def sets if needed:
  if ( this->_info->Get_eref_set() == NULL ) {
    this->_info->New_eref_set ( _fsize );
  }
  if ( this->_info->Get_def_set() == NULL ) {
    this->_info->New_def_set  ( _fsize );
  }
  if ( this->_info->Get_kill_set() == NULL ) {
    this->_info->New_kill_set ( _fsize );
  }

  // Initialize the file pointers:
  actuals = IPA_get_actual_array(this->_nd);
  symbols = IPA_get_symbol_array(this->_nd);
  globals = IPA_get_global_array(this->_nd);
  formals = IPA_get_formal_array(this->_nd);

  // Initialize the global pointers to summary information for traces:
  Init_Summary_Pointers ();
}

// ====================================================================
// ====================================================================
//
// IPAA_STEP tracing
//
// ====================================================================
// ====================================================================

void
IPAA_STEP::Print ( FILE *f ) const
{
  _info->Print ( f, _nd->Name() );
}

// ====================================================================

void
IPAA_STEP::Trace ( void ) const
{
  Print ( TFile );
}

// ====================================================================
//
// Get_merged_global_id
//
// Get a global symbol ID in the merged symbol list.
//
// ====================================================================

UINT32
IPAA_STEP::Get_merged_global_id ( SUMMARY_GLOBAL *gs )
{
  INT32 sym_id = gs->Get_symbol_index();
  return ( sym_id < 0 ) ? 0 : Get_Merged_Sym_ID ( &(this->symbols)[sym_id] );
}

// ====================================================================
//
// Initialize_Ref_Info: Initialize reference info for a node.
//
// This routine initializes the reference information for a callgraph
// node.  This involves creating the reference sets if they haven't
// been and initializing them to the direct references of this node.
//
// ====================================================================

BOOL
IPAA_STEP::Initialize_Ref_Info ()
{
    SUMMARY_PROCEDURE *p = _nd->Summary_Proc();
    INT gix = p->Get_global_index();
    INT gcount = p->Get_global_count();
    INT fix = p->Get_formal_index();
    INT j;
    BOOL global_change = FALSE;

    // Have we already done this?
    if ( _info->Is_initialized() ) return global_change;

    // Add the current node's refs and defs:
    for ( j=0; j<gcount; j++, gix++ ) {
	SUMMARY_GLOBAL* global = &(this->globals)[gix];
	UINT32 gid = 0;

	if ( Trace_Detail ) {
	    fprintf ( TFile, "<ipaa>\tiri: " );
	    global->Trace();
	    fprintf ( TFile, "<ipaa>\t\t" );
	    Ipl_Summary_Symbol[global->Get_symbol_index()].Trace ();
	}

	if ( global->Is_modref() ) {
	    // There's something to note here:
	    gid = this->Get_merged_global_id ( global );
	    if ( gid <= 0 ) continue;
	}

	if ( global->Is_ref() ) {
	    (_info->Get_eref_set())->Add_elmt ( gid );
	}
	if ( global->Is_dmod() ) {
	    (_info->Get_def_set())->Add_elmt ( gid );
	}
	if ( global->Is_dkill() ) {
	    (_info->Get_kill_set())->Add_elmt ( gid );
	}
    }


    const SUMMARY_SYMBOL* func_sym = _nd->Summary_Symbol ();
    if (func_sym->Is_addr_saved() || func_sym->Is_addr_passed ()) {
	icall_eref->Union_2 (_info->Get_eref_set ());
	icall_def->Union_2 (_info->Get_def_set ());
	icall_kill->Union_2 (_info->Get_kill_set ());
	global_change = TRUE;
    }
    
    // Initialize the formals' def/ref information:
    for ( j = 0; j<_fsize; j++ ) {
	INT16 position = formals[fix+j].Get_position();
	SUMMARY_SYMBOL *sym;
	sym = &Ipl_Summary_Symbol[formals[fix+j].Get_symbol_index()];

	if ( Trace_Detail ) {
	    fprintf ( TFile, "<ipaa>\tiri: " );
	    formals[fix+j].Trace();
	}
        if ( IPA_Trace_Mod_Ref ) {
            fprintf ( TFile, "<ipaa>\tiri: " );
            formals[fix+j].Trace();
        }

	if ( sym->Is_imod() ) {
	    (_info->Get_def_set())->Add_indirect_formal_elmt ( position );
	}
	if ( sym->Is_dmod() ) {
	    (_info->Get_def_set())->Add_direct_formal_elmt ( position );
	}
	if ( sym->Is_iref() ) {
	    (_info->Get_eref_set())->Add_indirect_formal_elmt ( position );
	}
	if ( sym->Is_dref() ) {
	    (_info->Get_eref_set())->Add_direct_formal_elmt ( position );
	}
	if ( sym->Is_ikill() ) {
	    (_info->Get_kill_set())->Add_indirect_formal_elmt ( position );
	}
	if ( sym->Is_dkill() ) {
	    (_info->Get_kill_set())->Add_direct_formal_elmt ( position );
	}
    }

    // Note that we've done this:
    _info->Set_initialized();

    // Trace if required:
    if ( Trace_IPAA ) {
	fprintf ( TFile, "<ipaa> Initialize_Ref_Info:\n" );
	this->Trace();
	fflush ( TFile );
    }
    if ( IPA_Trace_Mod_Ref ) {
        Ipa_tlog("MOD_REF", 0, "%s", "<ipaa> Initialize_Ref_Info:");
        this->Trace();
    }

    return global_change;
}

// ====================================================================
//
// IPAA_STEP::Get_actual_info
//
// Get the interesting information about an actual parameter.
// Check whether it is a global, and if so return the merged index.
// Check whether it is a formal, and if so return the formal number.
// For any valid symbol, return the SUMMARY_SYMBOL.
//
// ====================================================================

ACTUAL_KIND
IPAA_STEP::Get_actual_info (const SUMMARY_ACTUAL &actual,
			    SUMMARY_SYMBOL * &sym,
			    UINT32 &merged_idx,
			    INT32  &formal_num ) const	 
{
    INT32 formid;

    if (actual.Get_symbol_index () == -1)
	return ACTUAL_KIND_NONE;
    
    sym = symbols + actual.Get_symbol_index ();

    if (sym->Is_global ()) {
	merged_idx = Get_Merged_Sym_ID ( sym );
	if ( Trace_Detail ) {
	    fprintf ( TFile, "<ipaa>\tgai: Global %d\n", merged_idx );
	}
	return ACTUAL_KIND_GLOBAL;
    }
    else if (sym->Is_formal()) {
	formid = sym->Get_findex ();
	SUMMARY_FORMAL *formal = formals + formid;
	formal_num = formal->Get_position ();
	const SUMMARY_PROCEDURE *proc = _nd->Summary_Proc();
	if (proc->Is_alt_entry () || proc->Has_alt_entry ()) {
	    // for alt. entry points, we can't tell if it is a formal of
	    // the PU we are looking at, or one of the entry point.
	    if (proc->Get_formal_index () + formal_num != formid)
		return ACTUAL_KIND_LOCAL;
	}
	if ( Trace_Detail ) {
	    fprintf ( TFile, "<ipaa>\tgai: Formal %d\n", formal_num );
	}
	return ACTUAL_KIND_FORMAL;
    } else if (sym->Is_static ())
	// until we fix the summary info, force it to fstatic (PV 477092)
	return ACTUAL_KIND_FSTATIC;

    // return the merged_id for the common symbol
    else if (sym->Is_common()) {
	// common block symbol 
	merged_idx = ST_IDX_index(ST_base_idx(St_Table[sym->St_idx()]));
	Is_True (merged_idx,
		 ("Invalid global symbol idx in IPAA_STEP::Get_actual_info"));
	if ( Trace_Detail ) {
	    fprintf ( TFile, "<ipaa>\tgai: Global %d\n", merged_idx );
	}
	return ACTUAL_KIND_GLOBAL;
    } else if (sym->Is_local())
	return ACTUAL_KIND_LOCAL;
    // if we reach here return unknown
    return ACTUAL_KIND_NONE;
}

// ====================================================================
//
// IPAA_STEP::Finalize_Ref_Info
//
// This routine finalizes the reference information for a callgraph
// node.  We use a full bit vector to accumulate the reference info.
// This routine strips out the special sets and then compresses it to
// a sparse BV.
//
// TODO:  To deal with recursion, we need to do some revisiting once
// we process the head of a back arc.
//
// ====================================================================

void
IPAA_STEP::Finalize_Ref_Info ()
{
  if ( Trace_IPAA ) {
    fprintf ( TFile, "<ipaa> Finalize_Ref_Info:\n" );
    this->Trace();
  }
  if ( IPA_Trace_Mod_Ref ) {
    Ipa_tlog("MOD_REF", 0, "%s", "<ipaa> Finalize_Ref_Info:");
    this->Trace();
  }
}

// ====================================================================
// ====================================================================
//
// IPAA_DF_SOLVER
//
// This class is based on the DFBASE generic solver defined in
// ipa_solver.h.  It will actually contain two instantiations of the
// solver, a backward solver for collecting the direct global mod/ref
// information, and a forward solver for propagating actuals to
// formals.
//
// ====================================================================
// ====================================================================

class IPAA_DF_SOLVER : public DFBASE
{
public:
    // We will use the IPAA_NODE_INFO class as the input annotations,
    // pointing it at the member in the IPA_NODE class.  We will not use
    // a real output annotation, since there is nothing for the Trans
    // function to do.
    typedef IPAA_NODE_INFO IN_T;
    typedef void OUT_T;

    typedef enum {
	PHASE_INIT,			// Initialization (pre-solver)
	PHASE_DIRECT,			// Backward direct reference solver
	PHASE_FORMALS,			// Forward formal mapping solver
    } PHASE;

private:
    // The following are required by the solver templates:
    IN_T **_in;				// Input annotations -- mod/ref sets
    OUT_T **_out;			// Output annotations -- not used

    // We need to keep track of where we are in the solver process:
    mINT16 _iteration;			// Which iteration?
    PHASE _phase;			// Which phase?
    IPAA_STEP *_step;			// Graph walk current step

    // Statistics on the graph walking process:
    mINT32 _next_seq_id;		// Next node sequence ID
    mINT16 _direct_iters;		// Direct phase iterations
    mINT16 _formal_iters;		// Formal mapping phase iterations
    mINT32 _direct_nodes;		// Direct phase nodes processed
    mINT32 _direct_edges;		// Direct phase edges processed
    mINT32 _direct_recur;		// Direct phase direct recurrence edges processed
    mINT32 _direct_changed;		// Direct phase changed callee edges processed
    mINT32 _formal_nodes;		// Formal mapping phase nodes processed
    mINT32 _formal_edges;		// Formal mapping phase edges processed

public:

    // Constructor -- DFBASE's parameters plus phase:
    IPAA_DF_SOLVER (IPA_CALL_GRAPH &cg, DF_DIRECTION df, MEM_POOL *m, PHASE
		    phase ); 

    // Destructor -- free extra data:
    ~IPAA_DF_SOLVER (void);

    // ====== Field access =====
    // The following accessors are required by the solver templates;
    // they can generally be identical to these given IN_T and OUT_T.
    IN_T  **Get_in (void)  const	{ return _in; }
    OUT_T **Get_out (void) const	{ return _out; }
    void Set_in  ( IN_T **in )		{ _in = in; }
    void Set_out ( OUT_T **out )	{ _out = out; }
    IN_T  *Get_in_elmt(NODE_INDEX i)  const	{ return _in[i]; }
    OUT_T *Get_out_elmt(NODE_INDEX i) const	{ return _out[i]; }
    void Set_in_elmt  ( NODE_INDEX i, IN_T *in ) { _in[i] = in; };
    void Set_out_elmt ( NODE_INDEX i, OUT_T *out ) { _out[i] = out; };

    INT16 Get_iteration (void) const	{ return _iteration; }
    void Set_iteration ( INT16 i )	{ _iteration = i; }
    void Inc_iteration (void)		{ ++_iteration; }
    PHASE Get_phase (void) const	{ return _phase; }
    void Set_phase ( PHASE phase )	{ _phase = phase; }

    // Access to the IPAA_STEP struct and its fields:
    IPAA_STEP *Get_step (void) const	{ return _step; }
    void Set_step ( IPAA_STEP *step )	{ _step = step; }
    SUMMARY_ACTUAL *Get_actuals (void) const { return _step->Get_actuals(); }

    // Graph walk statistical fields:
    INT32 Get_next_seq_id (void) const	{ return _next_seq_id; }
    INT32 Inc_next_seq_id (void)	{ return ++_next_seq_id; }
    INT16 Get_direct_iters (void) const	{ return _direct_iters; }
    INT16 Inc_direct_iters (void)	{ return ++_direct_iters; }
    void  Set_direct_iters ( INT16 cnt ){ _direct_iters = cnt; }
    INT16 Get_formal_iters (void) const	{ return _formal_iters; }
    INT16 Inc_formal_iters (void)	{ return ++_formal_iters; }
    void Set_formal_iters ( INT16 cnt )	{ _formal_iters = cnt; }
    INT32 Get_direct_nodes (void) const	{ return _direct_nodes; }
    INT32 Inc_direct_nodes (void)	{ return ++_direct_nodes; }
    INT32 Get_formal_nodes (void) const	{ return _formal_nodes; }
    INT32 Inc_formal_nodes (void)	{ return ++_formal_nodes; }
    INT32 Get_direct_edges (void) const	{ return _direct_edges; }
    INT32 Inc_direct_edges (void)	{ return ++_direct_edges; }
    INT32 Get_formal_edges (void) const	{ return _formal_edges; }
    INT32 Inc_formal_edges (void)	{ return ++_formal_edges; }
    INT32 Get_direct_changed (void) const{ return _direct_changed; }
    INT32 Inc_direct_changed (void)	{ return ++_direct_changed; }

    // ====== Processing =====

    // Report statistics after an iteration of the given phase:
    void Report_graph_statistics ( PHASE phase ) const;

    // Initialize the annotation of a callgraph node.  We don't need
    // any initialization since we will set up the IPAA_NODE_INFO
    // instance on demand, but the function is required by the
    // instantiation that follows.
    void Initialize_node ( void * ) const	{};

    // This function, to initialize the callgraph vertex and in/out
    // annotations, is required by the solver.  It can normally be just
    // an instantiation of the template, identical to this one, given
    // Initialize_node above.
    void Initialize_annotations (void) {
	Initialize_Annotation ( this, this->_in, this->_out );
    };

    // Map worst-case attributes to actual parameter:
    void Map_worst_case_actual_attributes (const IPA_NODE &caller,
					   IPAA_NODE_INFO &caller_info,	
					   const SUMMARY_ACTUAL &actual );

    // Map attributes of formal to corresponding actual:
    BOOL Map_formal_attributes (const IPA_NODE &caller,		
				IPAA_NODE_INFO &caller_info,
				IPAA_NODE_INFO &callee_info,
				INT16 formal_num,
				const SUMMARY_ACTUAL &actual);

    // Accumulate reference info from a call:
    void Accumulate_ref_info (const IPA_NODE &caller, IPA_NODE &callee,
			      IPA_EDGE &edge );

    // Accumulate reference info from an indirect call:
    void Accumulate_indirect_actual_refs (const IPA_NODE &caller,
					  SUMMARY_CALLSITE &call );

    BOOL Check_and_accumulate_ref_info (const IPA_NODE &caller,
					const IPA_NODE &callee,
					IPA_EDGE &edge );

    // Map a single actual to callee's formal mapping list:
    BOOL Map_actual (const IPA_NODE &caller, IPAA_NODE_INFO &caller_info,
		     IPA_EDGE &edge,
		     const IPA_NODE &callee, IPAA_NODE_INFO &callee_info,
		     INT16 formal_num, const SUMMARY_ACTUAL &actual);

    // Map caller's actuals to callee's formal mapping list:
    void Map_actuals (const IPA_NODE &caller, const IPA_NODE &callee,
		      IPA_EDGE &edge );

    // The Meet function merges data from the input edges and the current
    // input annotation (given by the in parameter), producing a new
    // input annotation, which it returns.  It must be able to cope with
    // a NULL input annotation the first time.
    void Meet_direct_iter_1 ( IPA_NODE &caller ); // Direct phase, iter 1
    void Meet_direct_iter_n ( IPA_NODE &caller ); // Direct phase, iter N
    void Meet_formals_iter_1 ( IPA_NODE &caller ); // Formal phase, iter 1
    void Meet_formals_iter_n ( IPA_NODE &caller ); // Formal phase, iter N
    IN_T *Meet ( IN_T *in, void *vertex_user );	// Driver

    // The Trans function transfers data from the input annotation (the
    // in parameter) using the node information (vertex_user) and the old
    // output annotation (out), to produce a new output annotation.  In
    // this solver, we don't do anything interesting for this step.
    OUT_T *Trans ( IN_T *, OUT_T *, void * ) const { return NULL; }

    // One or both of Meet and Trans must set _changed to TRUE if either
    // makes a change which may affect the solution for successor nodes.
    // In our example, Meet will set it -- see its header for conditions.

    // A single-iteration callgraph walk occurs in the solver core, which
    // should be an instantiation of Iterative_Solver_Core:
    void Solver_core (void) {
	Inc_iteration ();
	Iterative_Solver_Core ( this );
    }

    // The master solver needs to call the solver core above, as well
    // as the various setup/cleanup routines.  As an iterative solver,
    // it should be an instantiation of Iterative_Dataflow_Solver:
    void Solver (void) {
	Iterative_Dataflow_Solver ( this, Depth_First_Ordering );
    }

    // place holder
    void Init_IO () {}
    void Post_process_IO ( const void * ) {}

    // Both output steps are called from this extraction routine,
    // which winds up by reporting the SBV statistics:
    void Extract_solution (void) {
	if ( Trace_Stats ) {
	    Extract_Solution ( this );
	}
    }
}; // class IPAA_DF_SOLVER : public DFBASE

// ====================================================================
//
// IPAA_DF_SOLVER::IPAA_DF_SOLVER
//
// Constructor for the IPAA solver class.  The graph iterator will
// be NULL, so the solver must provide it later.
//
// ====================================================================

IPAA_DF_SOLVER::IPAA_DF_SOLVER (
  IPA_CALL_GRAPH &cg,		// The underlying call graph to use
  DF_DIRECTION direction,	// The dataflow problem direction
  MEM_POOL *mpool,		// Where to put new junk
  PHASE phase )			// Which phase?
: DFBASE ( cg, direction, mpool, NULL )
{
  // Nothing much to do:
  _in = NULL;
  _out = NULL;
  _iteration = 0;
  _phase = phase;
  _next_seq_id = 0;
  _direct_iters = 0;
  _formal_iters = 0;
  _direct_nodes = 0;
  _formal_nodes = 0;
  _direct_edges = 0;
  _formal_edges = 0;
  _direct_changed = 0;
}


// ====================================================================
//
// IPAA_DF_SOLVER::~IPAA_DF_SOLVER
//
// Destructor for the IPAA solver class.  We must remove the input and
// output annotations; the DFBASE destructor will take care
// restoring the graph and freeing the iterator memory.
//
// ====================================================================

IPAA_DF_SOLVER::~IPAA_DF_SOLVER ()
{
  if ( _in != NULL ) {
    MEM_POOL_FREE ( Get_mempool(), _in );
    _in = NULL;
  }
  if ( _out != NULL ) {
    MEM_POOL_FREE ( Get_mempool(), _out );
    _out = NULL;
  }
}

// ====================================================================
//
// IPAA_DF_SOLVER::Report_graph_statistics
//
// Report statistics on iteration: nodes and edges processed.
//
// ====================================================================

void
IPAA_DF_SOLVER::Report_graph_statistics ( PHASE phase ) const
{
  if ( ! Trace_Stats ) return;

  if ( phase == PHASE_DIRECT ) {
    fprintf ( TFile,
	      "<ipaa> DIRECT phase: %2d iterations, "
	      "%3d nodes, %3d edges (%3d changed)\n",
	      Get_direct_iters(), Get_direct_nodes(),
	      Get_direct_edges(), Get_direct_changed()
	    );
  } else {
    fprintf ( TFile,
	      "<ipaa> FORMAL phase: %2d iterations, "
	      "%3d nodes, %3d edges\n",
	      Get_formal_iters(), Get_formal_nodes(),
	      Get_formal_edges()
	    );
  }
}

// ====================================================================
//
// IPAA_DF_SOLVER::Map_worst_case_actual_attributes:
//
// Map worst-case attributes to an actual parameter.
//
// What we do will depend on how the object is passed.  If passed by
// value, it is DREF, IREF, and IMOD.  If passed by reference, it is
// directly and indirectly referenced and modified.
//
// ====================================================================

void
IPAA_DF_SOLVER::Map_worst_case_actual_attributes (
  const IPA_NODE	&caller,	// The caller's node
  IPAA_NODE_INFO	&caller_info,	// Caller's IPAA information
  const SUMMARY_ACTUAL	&actual )	// Actual parameter
{
    IPA_PASS_TYPE  pass  = actual.Get_pass_type();
    SUMMARY_SYMBOL * sym;
    UINT32 merged_idx = 0;
    INT32 caller_formal = 0;

    switch ( pass ) {

	// Pass by value (Parameter is p):
    case PASS_LDID:
	// Referenced; not defined or killed


	switch (Get_step()->Get_actual_info (actual, sym, merged_idx,
					     caller_formal)) {

	case ACTUAL_KIND_GLOBAL:
	    // Passing a global -- mark it referenced:
	    caller_info.Add_eref_elmt ( merged_idx );
	    break;
	    
	case ACTUAL_KIND_FORMAL:
	    // Passing a formal by value:
	    caller_info.Add_formal_iref_elmt ( caller_formal );
	    caller_info.Add_formal_dref_elmt ( caller_formal );
	    caller_info.Add_formal_imod_elmt ( caller_formal );
	    break;
	    
	}
	break;

    case PASS_LDA:	// Parameter is &p
	// May be both referenced and defined, directly and indirectly:
	switch (Get_step()->Get_actual_info (actual, sym, merged_idx,
					     caller_formal)) {
	case ACTUAL_KIND_GLOBAL: {
	      ST_IDX merged_st_idx = make_ST_IDX( merged_idx, GLOBAL_SYMTAB );
	      caller_info.Add_eref_elmt ( merged_idx );
	      caller_info.Add_def_elmt  ( merged_idx );
	      if (IPA_Enable_Addressing &&
		  ! (ST_addr_saved  (&St_Table[merged_st_idx]) ||
		 ST_addr_passed (&St_Table[merged_st_idx]))  ) {
		  Set_ST_addr_saved (&St_Table[merged_st_idx]);

	      }
            } 
	    break;
	    
	case ACTUAL_KIND_FORMAL:
	    caller_info.Add_formal_dref_elmt ( caller_formal );
	    caller_info.Add_formal_dmod_elmt ( caller_formal );
	    caller_info.Add_formal_iref_elmt ( caller_formal );
	    caller_info.Add_formal_imod_elmt ( caller_formal );
	    if (IPA_Enable_Addressing && ! sym->Is_addr_saved() ) {
		sym -> Set_addr_saved ();
		caller_info . Set_local_addr_taken_mods ();

		// we only need to read in the local symbol table and set
		// flags if there is a change 
		SUMMARY_PROCEDURE *summary_proc = caller.Summary_Proc();
		if ( summary_proc != NULL ) {
		    summary_proc->Set_has_addr_taken_reset();
		}
	    }
	    break;
	    
	case ACTUAL_KIND_LOCAL:
	    if (IPA_Enable_Addressing && ! sym->Is_addr_saved() ) {
		sym -> Set_addr_saved ();
		caller_info . Set_local_addr_taken_mods ();

		// we only need to read in the local symbol table and set
		// flags if there is a change 
		SUMMARY_PROCEDURE *summary_proc = caller.Summary_Proc();
		if ( summary_proc != NULL ) {
		    summary_proc->Set_has_addr_taken_reset();
		}
	    }
	    break;
	}
	
	break;
    }
}

// ====================================================================
//
// IPAA_DF_SOLVER::Map_formal_attributes:
//
// Map the attributes of the callee's formal to the actual, given the
// callee info, the formal number, and the actual summary node.  The
// returned value indicates whether anything changed.
//
// What we do will depend on how the object is passed, and on
// what the caller does with it.  The possibilities are:
//
//		Used:		Defined:
// passed by:	iref dref aref	imod dmod ikill dkill
// ---------	---- ---- ----	---- ---- ----- -----
// val (LDID)	iref dref dref	imod none imod  none
// ref (LDA)	dref aref aref  dmod none dmod  none
//		(aref means effectively addr-taken)
//
// ====================================================================

BOOL
IPAA_DF_SOLVER::Map_formal_attributes (const IPA_NODE &caller,
				       IPAA_NODE_INFO &caller_info,
				       IPAA_NODE_INFO &callee_info,
				       INT16 formal_num,
				       const SUMMARY_ACTUAL &actual )	  
{
    SUMMARY_SYMBOL * sym;
    UINT32 merged_idx = 0;
    INT32 caller_formal = 0;
    BOOL ref, mod;
    BOOL changed = FALSE;

    if ( formal_num >= callee_info.Get_fcount() && Get_iteration() == 1 ) {
	// This parameter is beyond the end of the callee's formals,
	// presumably a varargs parameter.  Just treat it as worst-case:
	Map_worst_case_actual_attributes ( caller, caller_info, actual );

	// Since it's worst-case, it won't get worse on later iterations.
	// On the first iteration, noone depends on "changed" information.
	// So we just return no change:
	return FALSE;
    }

    switch ( actual.Get_pass_type() ) {

	// Pass by value (Parameter is p):
    case PASS_LDID:
	// Referenced; not defined or killed

	switch ( Get_step()->Get_actual_info (actual, sym, merged_idx,
					      caller_formal)) {

	case ACTUAL_KIND_GLOBAL:
	    // Passing a global -- mark it referenced:
	    if (! caller_info.Is_eref_elmt ( merged_idx ) &&
		(callee_info.Is_formal_iref_elmt ( formal_num ) ||
		 callee_info.Is_formal_dref_elmt ( formal_num ) ||
		 callee_info.Is_formal_imod_elmt ( formal_num ) ) ) {
		caller_info.Add_eref_elmt ( merged_idx );
		changed = TRUE;
	    }
	    break;

	case ACTUAL_KIND_FORMAL:
	    // Passing a formal by value.  Mark the caller so we know
	    // to try mapping on subsequent iterations:
	    caller_info.Set_passes_formals_by_value();
	    
	    // Passing a formal -- copy callee's info:
	    if (!caller_info.Is_formal_iref_elmt ( caller_formal ) &&
		callee_info.Is_formal_iref_elmt ( formal_num ) ) {
		caller_info.Add_formal_iref_elmt ( caller_formal );
		changed = TRUE;
	    }

	    if (!caller_info.Is_formal_dref_elmt ( caller_formal ) &&
		callee_info.Is_formal_dref_elmt ( formal_num ) ) {
		caller_info.Add_formal_dref_elmt ( caller_formal );
		changed = TRUE;
	    }
	    
	    if (!caller_info.Is_formal_imod_elmt ( caller_formal ) &&
		callee_info.Is_formal_imod_elmt ( formal_num ) ) {
		caller_info.Add_formal_imod_elmt ( caller_formal );
		changed = TRUE;
	    }

	    break;
	    
	}
	break;

    case PASS_LDA:			// Parameter is &p
	// May be referenced or defined, based on the callee
	// information:
	ACTUAL_KIND akind = Get_step()->Get_actual_info (actual, sym,
							 merged_idx,
							 caller_formal ); 

	if ( akind == ACTUAL_KIND_NONE )
	    break;

	// Get information from the corresponding formal in the callee:
	ref = callee_info.Is_formal_iref_elmt ( formal_num )
	    | callee_info.Is_formal_dref_elmt ( formal_num );
#ifdef KEY // bug 8316
	// Don't miss the DMOD of the callee while propagating it to caller
	mod = callee_info.Is_formal_imod_elmt ( formal_num )
	    | callee_info.Is_formal_dmod_elmt ( formal_num );
#else
	mod = callee_info.Is_formal_imod_elmt ( formal_num );
#endif

	// ... and attach it:
	switch (akind) {
	case ACTUAL_KIND_GLOBAL:
	    // Passing a global by reference.  Mark the caller so we know
	    // to try mapping on subsequent iterations:
	    caller_info.Set_passes_globals_by_reference();

	    // update the callee information also
#if _RECORD_GLOBAL_REF_VIA_FORMAL_
	    if ( ref && ! callee_info.Is_eref_elmt ( merged_idx ) ) {
	      callee_info.Add_eref_elmt ( merged_idx );
	      changed = TRUE;
            }
#endif
	    
	    if ( mod && ! callee_info.Is_def_elmt ( merged_idx ) ) {
              callee_info.Add_def_elmt ( merged_idx );
              changed = TRUE;
            }

	    ref |= callee_info.Is_eref_elmt (merged_idx);
	    mod |= callee_info.Is_def_elmt (merged_idx);

	    if ( ref  && ! caller_info.Is_eref_elmt ( merged_idx ) ) {
		caller_info.Add_eref_elmt ( merged_idx );
		changed = TRUE;
	    }
	    if ( mod  && ! caller_info.Is_def_elmt ( merged_idx ) ) {
		caller_info.Add_def_elmt ( merged_idx );
		changed = TRUE;
	    }

	    break;

	case ACTUAL_KIND_FORMAL:
	    if ( ref  && ! caller_info.Is_formal_dref_elmt ( caller_formal ) ) {
		caller_info.Add_formal_dref_elmt ( caller_formal );
		changed = TRUE;
	    }
	    if ( mod  && ! caller_info.Is_formal_dmod_elmt ( caller_formal ) ) {
		caller_info.Add_formal_dmod_elmt ( caller_formal );
		changed = TRUE;
	    }
	    break;

	case ACTUAL_KIND_LOCAL:
	    if (ref && !sym->Is_dref()) {
		sym->Set_dref ();
		changed = TRUE;
	    }
	    if (mod && !sym->Is_dmod()) {
		sym->Set_dmod ();
		changed = TRUE;
	    }
	    break;

	case ACTUAL_KIND_PSTATIC:
	case ACTUAL_KIND_FSTATIC:
	    // until we have enough summary info, force worst case
	    // see PV 477092
	    mod = ref = TRUE;
	    break;
	}

	break;
    }

    

    return changed;
} // IPAA_DF_SOLVER::Map_formal_attributes

// ====================================================================
//
// IPAA_DF_SOLVER::Accumulate_ref_info
//
// This routine accumulates the reference information from a callee
// into the caller's node, given the call edge.  There are three
// aspects of particular interest:
//
//  1)	Global references in the callee are accumulated into the
//	caller.
//
//  2)	When an actual of the call is one of the caller's formals, the
//	reference information for the corresponding callee's formal is
//	attributed to the caller's formal.  In this way, as we walk up
//	the tree, we end up with full information about references to
//	formals, even indirectly.
//
//	We do the same thing for global objects as formals.  Note that
//	this results in having full information in the caller for
//	global references that result from explicitly passing them to
//	its callees, but no information for references which result
//	from globals that happen to be mapped to the caller's formals
//	as a result of calls from further up the call graph.  But
//	this distinction is probably what we want, since what we have
//	is precise information (common to all calls to this PU, except
//	for issues of flow-sensitivity), whereas global references via
//	the PU's formals are specific to the callsites which passed the
//	global, and we don't want them propagated up the tree to other
//	callsites.  We will catch them later on the way back down the
//	tree processing formal parameter mappings.
//
//  3)	Our reference sets may contain certain summary items which
//	represent references to some unknown element of a special set
//	of symbols, e.g. all of the external addr-taken symbols.  Once
//	a DREF/EREF to one of these is seen, the individual elements of
//	the set become uninteresting for DREF/EREF purposes, so we do
//	two things:  (a) we look for these first, and don't process
//	the element symbols if present, and (b) when we first see one
//	of them, we clear the individual element symbols out of the
//	set to limit space and processing time in callers.  DMOD are
//	the same (because they are maybe information anyway), but KILLs
//	are not, in the sense that once we've seen a DMOD for one of
//	the special sets, we are still interested in KILLs for its
//	elements.  (A KILL cannot apply to one of the sets, because it
//	needs to be definite.)
//
//	TODO:  (3) is not yet done.
//
// ====================================================================

void
IPAA_DF_SOLVER::Accumulate_ref_info (const IPA_NODE &caller,
				     IPA_NODE &callee, IPA_EDGE &edge ) 
{
    IPAA_NODE_INFO *callee_info = callee.Mod_Ref_Info();
    IPAA_NODE_INFO *caller_info = caller.Mod_Ref_Info();
    SUMMARY_CALLSITE *call;
    INT32 acount;

    // Trace if required:
    if ( Trace_IPAA ) {
	fprintf ( TFile, "<ipaa> ari: edge " );
	edge.Trace ( Get_callgraph(), TRUE );
	fflush ( TFile );
    }

    // If the callee_info is NULL, this means we are dealing with a
    // back-edge.  We want to initialize it, so that its direct
    // references are merged in during the first iteration.  That will
    // prevent forcing us to check for formal mapping related changes
    // on subsequent iterations, and sometimes eliminate changes in the
    // globals, so we don't need as many iterations:
    if ( callee_info == NULL || ! callee_info->Is_initialized() ) {
	IPAA_STEP istep ( *Get_callgraph(), callee );

	// Initialize the bit vectors in the nodes's IPAA_NODE_INFO object:
	if (istep.Initialize_Ref_Info ())
	    Set_changed ();
    }

    // (a) Merge global information (but not for self-calls):
    if ( &caller != &callee ) {
	(caller_info->Get_eref_set())->Union_2 ( callee_info->Get_eref_set() );
	(caller_info->Get_def_set() )->Union_2 ( callee_info->Get_def_set() );

	const SUMMARY_SYMBOL* func_sym = caller.Summary_Symbol ();
	
	if (func_sym->Is_addr_saved() || func_sym->Is_addr_passed ()) {
	    if (Get_changed ())
		icall_eref->Union_2 (caller_info->Get_eref_set ());
	    else if (icall_eref->Union_2_diff (caller_info->Get_eref_set ()))
		Set_changed ();

	    if (Get_changed ())
		icall_def->Union_2 (caller_info->Get_def_set ());
	    else if (icall_def->Union_2_diff (caller_info->Get_def_set ()))
		Set_changed ();
	}
    } else if ( Trace_IPAA | Trace_Detail ) {
	fprintf ( TFile, "<ipaa> ari: self-call edge\n" );
	fflush ( TFile );
    }

    // (b) Deal with simple formals/globals passed as actuals:
    call = edge.Summary_Callsite();
    acount = call->Get_param_count();
    if ( acount > 0 ) {
	// We've got some actuals to check:
	INT32 aix;
	INT32 j;

	// Now walk through the actuals looking for simple variables:
	for ( j=0, aix = call->Get_actual_index(); j < acount; ++j, ++aix) {
	    const SUMMARY_ACTUAL &actual = Get_actuals()[aix];

	    // Trace if required:
	    if ( Trace_Detail ) {
		fprintf ( TFile, "<ipaa> ari: " );
		actual.Trace ( aix );
	    }
        if ( IPA_Trace_Mod_Ref ) {
        Ipa_tlog("MOD_REF", 0, "%s <ipaa> ari: ", caller.Name());
        actual.Trace ( aix );
        }

	    (void) Map_formal_attributes ( caller, *caller_info, 
					  *callee_info, j, actual);
	}
    }
} // IPAA_DF_SOLVER::Accumulate_ref_info

// ====================================================================
//
// IPAA_DF_SOLVER::Accumulate_indirect_actual_refs
//
// This routine accumulates the reference information for the actuals
// of an indirect call, i.e. it makes the worst-case assumptions.
//
// ====================================================================

void
IPAA_DF_SOLVER::Accumulate_indirect_actual_refs (const IPA_NODE &caller,
						 SUMMARY_CALLSITE &call)
{
    IPAA_NODE_INFO *caller_info = caller.Mod_Ref_Info();
    INT32 acount;

    // Trace if required:
    if ( Trace_IPAA ) {
	fprintf ( TFile, "<ipaa> aiar: edge " );
	call.Trace ();
	fflush ( TFile );
    }

    // Deal with simple formals/globals passed as actuals:
    acount = call.Get_param_count();
    if ( acount > 0 ) {
	// We've got some actuals to check:
	INT32 aix;
	INT32 j;

	// Now walk through the actuals looking for simple variables:
	for ( j=0, aix = call.Get_actual_index(); j < acount; ++j, ++aix) {
	    SUMMARY_ACTUAL* actual = &(Get_actuals())[aix];
	    
	    // Trace if required:
	    if ( Trace_Detail ) {
		fprintf ( TFile, "<ipaa> aiar: " );
		actual->Trace ( aix );
	    }

	    Map_worst_case_actual_attributes ( caller, *caller_info, *actual );
	}
    }
}

// ====================================================================
//
// IPAA_DF_SOLVER::Check_and_accumulate_ref_info
//
// This routine accumulates the reference information from a callee
// into the caller's node, given the call edge, checking for changes
// as it goes.  See the description of Accumulate_ref_info above.
//
// ====================================================================

BOOL
IPAA_DF_SOLVER::Check_and_accumulate_ref_info (const IPA_NODE &caller,
					       const IPA_NODE &callee,
					       IPA_EDGE &edge)	 
{
    IPAA_NODE_INFO *callee_info = callee.Mod_Ref_Info();
    IPAA_NODE_INFO *caller_info = caller.Mod_Ref_Info();
    SUMMARY_CALLSITE *call;
    INT32 acount;
    BOOL changed = FALSE;

    // Trace if required:
    if ( Trace_IPAA ) {
	fprintf ( TFile, "<ipaa> cari: edge " );
	edge.Trace ( Get_callgraph(), TRUE );
	fflush ( TFile );
    }

    // The callee_info will never be NULL after the first iteration.

    // (a) Merge global information:
    changed |= (caller_info->Get_eref_set())->Union_2_diff
	( callee_info->Get_eref_set() );
    changed |= (caller_info->Get_def_set() )->Union_2_diff
	( callee_info->Get_def_set() );

    if (changed) {
	const SUMMARY_SYMBOL* func_sym = caller.Summary_Symbol ();
	if (func_sym->Is_addr_saved () || func_sym->Is_addr_passed ()) {
	    if (Get_changed ())
		icall_eref->Union_2 (caller_info->Get_eref_set ());
	    else if (icall_eref->Union_2_diff (caller_info->Get_eref_set ()))
		Set_changed ();

	    if (Get_changed ())
		icall_def->Union_2 (caller_info->Get_def_set ());
	    else if (icall_def->Union_2_diff (caller_info->Get_def_set ()))
		Set_changed ();
	}
    }

    // (b) Deal with simple formals/globals passed as actuals:
    call = edge.Summary_Callsite();
    acount = call->Get_param_count();
    for (INT j = 0, aix = call->Get_actual_index (); j < acount; ++j, ++aix) {

	// Now walk through the actuals looking for simple variables:
	const SUMMARY_ACTUAL &actual = Get_actuals()[aix];

	// Trace if required:
	if ( Trace_Detail ) {
	    fprintf ( TFile, "<ipaa> ari: " );
	    actual.Trace ( aix );
	}

	changed |= Map_formal_attributes (caller, *caller_info, 
					  *callee_info, j, actual); 
    }

    if ( Trace_Detail ) {
	fprintf ( TFile, "<ipaa> cari: edge changed " );
	edge.Trace ( Get_callgraph(), TRUE );
    }
    return changed;
}

// ====================================================================
//
// IPAA_DF_SOLVER::Meet_direct_iter_1
//
// This routine handles Meet for the DIRECT phase, iteration 1.  See
// the description of Meet below.
//
// Merge data from the callees of the current node into the current
// node.  This must be able to cope with a NULL IPAA_NODE_INFO from
// back edges in the callgraph.
//
// In phase PHASE_DIRECT, we are doing a post-order callgraph walk to
// identify data objects which are directly referenced by a routine or
// its callees.  In the subsequent phase PHASE_FORMALS, we do a
// pre-order walk which maps reference parameters and identifies their
// effects on the reference functions.
//
// We assume that IPL has identified the direct references within each
// routine.  Therefore, our purpose is to propagate them up through the
// callgraph so that each routine has identified both its and its
// callees' direct references.  On the first iteration of PHASE_DIRECT,
// we must initialize the IPAA_NODE_INFO reference sets for the current
// routine's direct references, and then merge in those of its callees.
//
// A single iteration suffices in the absence of recursion.  Therefore,
// we update _changed on the first iteration only if we encounter a
// back edge, and we special-case self-recursion to deal with it
// immediately and avoid causing a new iteration for its benefit.  In
// subsequent iterations, we must identify changes upon updating the
// reference sets.
//
// ====================================================================

void
IPAA_DF_SOLVER::Meet_direct_iter_1 ( IPA_NODE &caller )	
{
    INT16 self_calls = 0;

    // Get the callgraph and set up an IPAA_STEP object, which also
    // attaches an IPAA_NODE_INFO to the IPA_NODE if needed:
    IPA_CALL_GRAPH *cg = Get_callgraph();
    IPAA_STEP istep ( *cg, caller );
    Set_step ( &istep );

    // Initialize the bit vectors in the nodes's IPAA_NODE_INFO object,
    // based on the local summary information:
    if (istep.Initialize_Ref_Info ())
	Set_changed ();

    // Mark the node visited, to distinguish it from other nodes which
    // have IPAA_NODE_INFO objects because processing back edges required
    // them.  This allows us to distinguish back edges below:
    IPAA_NODE_INFO *info = caller.Mod_Ref_Info();
    info->Set_visited();
    if ( info->Get_seq_id() == 0 ) {
	info->Set_seq_id ( Inc_next_seq_id() );
    }
    (void) Inc_direct_nodes();

    // We assume that we've changed this node this round:
    info->Set_changed_iter(1);

    // Follow the successor edges:  Note that since the solver has
    // inverted the graph, these look like predecessor edges now.
    //
    // TODO: For now, we follow no particular order.  It may be
    // better to sort them first, at least to put calls to the same
    // callee together, and maybe to order them by some
    // characteristic of the reference lists, e.g. to process the
    // biggest ones first.

    IPA_PRED_ITER edge_iter ( cg, &caller );
    for ( edge_iter.First(); !edge_iter.Is_Empty(); edge_iter.Next() ) {
	IPA_EDGE* edge = edge_iter.Current_Edge();

	if (edge == NULL)
	    continue;
	// TODO:  We want a check at this point whether we've already
	// processed an edge between these two PUs.  If so, the merging
	// of the callee's direct references into this one has already
	// occurred, and we only need to look at globals passed as actual
	// parameters.
	(void) Inc_direct_edges();

	// Identify the callee, and force an IPAA_NODE_INFO for it:
	IPA_NODE *callee = Get_callee(edge);
	IPAA_NODE_INFO *callee_info = Force_IPAA_Info ( *callee );

	// Is this an (indirect) back edge?
	if ( ! callee_info->Is_visited() ) {
	    // This is a back edge, and we haven't visited the callee yet.
	    // Mark it in the callgraph edge for later use, and mark this
	    // node as having a back edge.
	    edge->Set_Recursive ();
	    info->Set_has_back_edge ();
            callee->Set_Incoming_Back_Edge ();

	    // For an indirectly recursive call (i.e. to different node),
	    // we just have wait until the next iteration:
	    Set_changed ();

	    if ( Trace_IPAA ) {
		fprintf ( TFile, "<ipaa> Meet_direct_iter_1: unvisited edge " );
		edge->Trace ( Get_callgraph(), TRUE );
		fflush ( TFile );
	    }

	    // Or, is it a direct back edge (to self)?
	} else if ( &caller == callee ) {
	    // This is a call to self.  It is still a back edge, but it
	    // is simpler -- we will reach closure immediately (below), so
	    // we don't set changed in this case:
	    edge->Set_Recursive ();
	    info->Set_has_back_edge ();
	    info->Set_depends_on_back_edge ();
            callee->Set_Incoming_Back_Edge ();
	    Accumulate_ref_info ( caller, *callee, *edge );

	    // The only information that propagates after the initial
	    // call to Accumulate_ref_info comes through formal parameters,
	    // and it can only take as many steps to propagate it as there
	    // are distinct calls to pass through:
	    if ( info->Passes_formals_by_value() ) {
		++self_calls;
	    }

	    // Otherwise, we're calling a routine we've already processed:
	} else {
	    // Accumulate the reference information into the caller's
	    // node:
	    if ( callee_info->Depends_on_back_edge() ) {
		info->Set_depends_on_back_edge();
	    }
	    Accumulate_ref_info ( caller, *callee, *edge );
	}

	// If the callee has unknown call effects, so does this one:
	if ( callee_info->Has_unknown_call_effects() ) {
	    info->Set_has_unknown_call_effects();
	}

    }

    // Look at the indirect call list and set the appropriate flags.
    // An indirect call implies that any potentially addr-taken object
    // may have anything done to it or with it, and similarly that any
    // actual parameter may be arbitrarily modified or referenced.
    // TODO:  Someday, we may be able to propagate specific callees to
    // indirect calls, at which point they can be treated like direct
    // calls.
    if (! caller.Icall_List().empty ()) {
	IPA_ICALL_LIST& icall_list = caller.Icall_List();
	for (IPA_ICALL_LIST::iterator icall_iter = icall_list.begin ();
	     icall_iter != icall_list.end (); ++icall_iter) {
	    SUMMARY_CALLSITE *c = (*icall_iter)->Callsite();
	    (void) Inc_direct_edges();
	    
	    if ( c != NULL ) {
		Accumulate_indirect_actual_refs ( caller, *c );
	    }

	    info->Set_has_indirect_edge();
	    info->Set_has_unknown_call_effects();
	}

	info->Get_eref_set ()->Union_2 (icall_eref);
	info->Get_def_set ()->Union_2 (icall_def);
	info->Get_kill_set ()->Union_2 (icall_kill);
    }

    // Look at the opaque call list and set the appropriate flags.
    // This case is exactly analogous to the indirect call list above,
    // but propagation of callees might someday make the indirect call
    // case more tractable.
    if (! caller.Ocall_List ().empty () ) {
	IPA_ICALL_LIST& ocall_list = caller.Ocall_List ();
	for (IPA_ICALL_LIST::iterator ocall_iter = ocall_list.begin ();
	     ocall_iter != ocall_list.end (); ++ocall_iter) {

	    SUMMARY_CALLSITE *c = (*ocall_iter)->Callsite();
	    (void) Inc_direct_edges();
	    
	    if ( c != NULL ) {
		Accumulate_indirect_actual_refs ( caller, *c );
	    }
	    
	    info->Set_has_unknown_call_effects();
	}

	info->Get_eref_set ()->Union_2 (icall_eref);
	info->Get_def_set ()->Union_2 (icall_def);
	info->Get_kill_set ()->Union_2 (icall_kill);
    }

    // Now deal with self-calls.  To make sure that we propagate all
    // formal references through a chain of actual->formal mappings, we
    // need to go through them once per such call.
    BOOL changed = TRUE;
    while ( self_calls-- > 0 && changed ) {
	changed = FALSE;
	IPA_PRED_ITER edge_iter ( cg, &caller );

	for ( edge_iter.First(); !edge_iter.Is_Empty(); edge_iter.Next() ) {
	    IPA_EDGE* edge = edge_iter.Current_Edge();

	    if ( edge != NULL ) {
		IPA_NODE *callee = Get_callee(edge);
		
		if ( &caller == callee ) {
		    // This is a call to self:
		    (void) Inc_direct_edges();
		    (void) Inc_direct_changed();
		    changed |= Check_and_accumulate_ref_info (caller,
							      *callee,
							      *edge ); 
		}
	    }
	}
    }

    // NOTE:  edge_iter is freed automatically when it is exhausted.

    // Go finalize the reference sets for this node, which includes
    // compressing them:
    istep.Finalize_Ref_Info ();

    Set_step ( NULL );
}

// ====================================================================
//
// IPAA_DF_SOLVER::Meet_direct_iter_n
//
// This routine handles Meet for the DIRECT phase, iteration n>1.  See
// the description of Meet_direct_iter_1 above and of Meet below.
//
// On iterations after the first, we are more careful.  We only process
// nodes marked as depending on back edges.  For those, we check
// whether a forward-edge successor has changed in this iteration, or a
// back-edge successor in the previous iteration before trying to
// actually merge information.  If we do merge, we check first whether
// the information has changed.
//
// ====================================================================

void
IPAA_DF_SOLVER::Meet_direct_iter_n ( IPA_NODE &caller )
{
  IPAA_NODE_INFO *info = caller.Mod_Ref_Info();
  if (! caller.Mod_Ref_Info ()->Depends_on_back_edge () &&
      caller.Icall_List().empty () &&
      caller.Ocall_List ().empty ())
      return;
  (void) Inc_direct_nodes ();

  // Get the callgraph and set up an IPAA_STEP object:
  IPA_CALL_GRAPH *cg = Get_callgraph();
  IPAA_STEP istep ( *cg, caller );
  Set_step ( &istep );

  // Follow the successor edges:  Note that since the solver has
  // inverted the graph, these look like predecessor edges now.
  //
  // TODO: See the comment on ordering in Meet_direct_iter_1 above.

  IPA_PRED_ITER edge_iter ( cg, &caller );
  INT16 iteration = Get_iteration();

  for ( edge_iter.First();
	!edge_iter.Is_Empty();
	edge_iter.Next() )
  {
    IPA_EDGE* edge = edge_iter.Current_Edge();

    if ( edge != NULL ) {
      IPA_NODE *callee = Get_callee(edge);
      IPAA_NODE_INFO *callee_info = callee -> Mod_Ref_Info();
      (void) Inc_direct_edges ();

      // Has the callee's information changed?
      INT16 changed_iteration = callee_info->Get_changed_iter()
		  + ( edge->Is_Recursive() ? 1 : 0 );

      // If so, go accumulate:
      if ( changed_iteration == iteration ) {
	// Accumulate the reference information into the caller's
	// node:
	if ( Check_and_accumulate_ref_info ( caller, *callee, *edge ) )
	{
	  callee_info->Set_changed_iter ( iteration );
	  Set_changed ();
	  (void) Inc_direct_changed ();
	}
      }
    }
  }

  // now do the same for icall and ocall
  
  if (! caller.Icall_List().empty ()) {
      info->Get_eref_set ()->Union_2 (icall_eref);
      info->Get_def_set ()->Union_2 (icall_def);
      info->Get_kill_set ()->Union_2 (icall_kill);
  }

  if (! caller.Ocall_List ().empty () ) {
      info->Get_eref_set ()->Union_2 (icall_eref);
      info->Get_def_set ()->Union_2 (icall_def);
      info->Get_kill_set ()->Union_2 (icall_kill);
  }

  // NOTE:  edge_iter is freed automatically when it is exhausted.

  Set_step ( NULL );
}

// ====================================================================
//
// Check_Passed_Global / Check_Passed_Local
//
// We have a global (local/formal) being passed by reference as an
// actual parameter.  Determine whether the corresponding
// formal parameter's usage requires setting the
// addr_taken_saved attribute, i.e. if it has a
// direct or address reference.
//
// ====================================================================

static void
Check_Passed_Global (const IPAA_NODE_INFO &callee_info,
		     INT16 formal_num,
		     UINT32 merged_idx )	
{
    if (callee_info.Is_formal_dref_elmt ( formal_num ) ||
	callee_info.Get_fcount() <= formal_num ||
	callee_info.Is_eref_elmt (merged_idx) ||
	callee_info.Is_def_elmt (merged_idx) ||
	callee_info.Is_kill_elmt (merged_idx)) {
	    
	// Formal is referenced directly, or is a varargs parameter with
	// unknown references, so global's address could be saved.
	// Also, if the callee references this global directly in any form,
	// we need to  tell the backend that it is aliased.
	// Set addr_taken_saved:
	ST_IDX merged_st_idx = make_ST_IDX( merged_idx, GLOBAL_SYMTAB );
	Set_ST_addr_saved (&St_Table[merged_st_idx]);
    }
}

// ====================================================================

static void
Check_Passed_Local (
  const IPA_NODE	&caller,	// The caller's node
  IPAA_NODE_INFO	&callee_info,	// Callee's IPAA information
  INT16			formal_num,	// Formal number
  SUMMARY_SYMBOL *sym )	// Local/formal symbol
{
  if ( callee_info . Get_fcount() <= formal_num
    || callee_info . Is_formal_dref_elmt ( formal_num ) )
  {
    // Formal is referenced directly, or is a varargs parameter with
    // unknown references, so local's address could be saved.  Set
    // Is_addr_saved:
    sym -> Set_addr_saved ();
    callee_info . Set_local_addr_taken_mods ();

    // we only need to read in the local symbol table and set
    // flags if there is a change 
    SUMMARY_PROCEDURE *summary_proc = caller.Summary_Proc();
    if ( summary_proc != NULL ) {
      summary_proc->Set_has_addr_taken_reset();
    }
  }
}

// ====================================================================
//
// IPAA_DF_SOLVER::Map_actual
//
// Map a single actual to the callee's formal mapping list, given the
// caller/callee info, the formal number, and the actual summary node.
// The returned value indicates whether anything changed.
//
// What we do will depend on how the object is passed, and on
// what the caller does with it.  The possibilities are:
//
// passed by:	actual is formal:	actual is global:
// ---------	----------------	----------------
// val (LDID)	xfer to callee formal	none
// ref (LDA)	none			add to callee formal map
//
// Note that after the first iteration, all globals will have been
// added to the maps -- only formal propagation remains required.
//
// We also finish addr_taken processing here: for any global or local
// symbols passed by reference, we check whether the corresponding
// formal parameter in the callee is directly referenced.  If so, and
// if addr_taken_and_passed is set, we reset it, set
// addr_taken_and_saved, and also set Local_addr_taken_mods so that
// we know to fix up the symbol table later.
//
// ====================================================================

BOOL
IPAA_DF_SOLVER::Map_actual (const IPA_NODE &caller,
			    IPAA_NODE_INFO &caller_info,
			    IPA_EDGE &edge,
			    const IPA_NODE &callee,
			    IPAA_NODE_INFO &callee_info,
			    INT16 formal_num,
			    const SUMMARY_ACTUAL &actual)
{
    IPA_PASS_TYPE  pass  = actual.Get_pass_type();
    SUMMARY_SYMBOL * sym = 0;
    INT32 caller_formal = 0;
    BOOL changed = FALSE;
    ACTUAL_KIND akind;
    BOOL mod;
    UINT32 merged_idx;

    switch ( pass ) {

	// Pass by value (Parameter is p):
    case PASS_LDID:

	akind = Get_step()->Get_actual_info
	    ( actual, sym, merged_idx, caller_formal );

	if ( akind == ACTUAL_KIND_FORMAL ) {
	    // Passing a formal by value -- merge pointees into callee's:
	    changed |=
		Add_Map_To_Map ( callee_info, formal_num,
				*caller_info.Get_fmap(caller_formal) );
	}
	break;

	// Pass by reference (parameter is &p):
    case PASS_LDA:
	// After the first iteration, we don't care any more:
	if ( Get_iteration() > 1 ) break;

	akind = Get_step()->Get_actual_info
	    ( actual, sym, merged_idx, caller_formal );

	mod = callee_info.Is_formal_imod_elmt (formal_num);

	switch ( akind ) {

	case ACTUAL_KIND_GLOBAL:
	    // Passing a global by reference -- add to callee's map:
	    if ( Add_Actual_To_Map ( callee_info, formal_num, merged_idx ) ) {
		ST_IDX merged_st_idx = make_ST_IDX( merged_idx, GLOBAL_SYMTAB );
		changed = TRUE;
		if (IPA_Enable_Addressing &&
		    ! (ST_addr_saved  (&St_Table[merged_st_idx]) || 
               ST_addr_passed (&St_Table[merged_st_idx])     )    )
		    Check_Passed_Global (callee_info, formal_num, merged_idx );
	    }

	    mod |= callee_info.Is_def_elmt (merged_idx);

	    break;

	case ACTUAL_KIND_FORMAL:
	case ACTUAL_KIND_LOCAL:
	    // Passing a local or formal by reference -- check addr-taken:
	    if (IPA_Enable_Addressing && ! sym -> Is_addr_saved() ) {
		Check_Passed_Local (caller, callee_info, formal_num, sym );
	    }
	    break;

	case ACTUAL_KIND_PSTATIC:
	case ACTUAL_KIND_FSTATIC:
	    // until we have enough summary info, force worst case
	    // see PV 477092
	    mod = TRUE;
	    break;
	}

	// record readonly parameter
	if (!IPA_Enable_Readonly_Ref || caller.Is_Lang_F77() || 
	    caller.Is_Lang_F90())
	    break;
	
	if (sym && sym->Is_addr_saved ()) {
	    edge.Clear_Param_Readonly(formal_num);
	    edge.Clear_Param_Pass_Not_Saved(formal_num);
	    break;
	}

	if (mod)
	    edge.Clear_Param_Readonly(formal_num);
	else
	    edge.Set_Param_Readonly(formal_num);
	if (callee_info.Is_formal_dref_elmt (formal_num))
	    edge.Clear_Param_Pass_Not_Saved(formal_num);
	else
	    edge.Set_Param_Pass_Not_Saved (formal_num);
	
	break;
    }

    return changed;
}

/* If the address of the same variable is passed multiple times in the same
   call, we have to reset the readonly/addr_not_saved bit as long as one of
   them is not.
  */
static void
Verify_duplicated_actuals (IPA_EDGE &edge, const SUMMARY_ACTUAL *actual,
                           INT actual_count)
{
    for (INT i = 0; i < actual_count; ++i) {
        if (actual[i].Get_pass_type () != PASS_LDA)
            continue;
        for (INT j = i + 1; j < actual_count; ++j) {
            if (actual[j].Get_pass_type () != PASS_LDA)
                continue;
            if (actual[j].Get_symbol_index () != actual[i].Get_symbol_index ())
                continue;
            if (edge.Is_Param_Readonly(i) != edge.Is_Param_Readonly(j)) {
                edge.Clear_Param_Readonly(i);
                edge.Clear_Param_Readonly(j);
            }
            if (edge.Is_Param_Pass_Not_Saved(i) !=
                edge.Is_Param_Pass_Not_Saved(j)) {
                edge.Clear_Param_Pass_Not_Saved(i);
                edge.Clear_Param_Pass_Not_Saved(j);
            }
        }
    }
    
} // Verify_duplicated_actuals


// ====================================================================
//
// IPAA_DF_SOLVER::Map_actuals
//
// This routine maps the actuals from a caller to the formal mapping
// list of the callee's node, given the call edge.  If a change occurs
// on a recursive arc, the callee is normally marked as changed in the
// current iteration plus 1 so that it will be investigated on the next
// iteration.  If the change occurs on a non-recursive arc, the callee
// is normally marked with the current iteration.  In both cases,
// however, the change is only noted if the callee passes formals as
// value actuals, since that is the only case in which the change might
// propagate further.
//
// ====================================================================

void
IPAA_DF_SOLVER::Map_actuals (const IPA_NODE &caller,
			     const IPA_NODE &callee,
			     IPA_EDGE &edge )	 
{
    SUMMARY_CALLSITE *call = edge.Summary_Callsite();
    INT32 acount;

    // Trace if required:
    if ( Trace_IPAA ) {
	fprintf ( TFile, "<ipaa> mapa: edge " );
	edge.Trace ( Get_callgraph() );
	fflush ( TFile );
    }

    // Deal with simple formals/globals passed as actuals:
    acount = call->Get_param_count();
    SUMMARY_ACTUAL *actual = Get_actuals() + call->Get_actual_index();

    // Now walk through the actuals looking for simple variables:
    for (INT j=0; j < acount; ++j) {
	IPAA_NODE_INFO *callee_info = callee.Mod_Ref_Info();
	IPAA_NODE_INFO *caller_info = caller.Mod_Ref_Info();

	// Trace if required:
	if ( Trace_Detail ) {
	    fprintf ( TFile, "<ipaa> mapa: " );
	    actual[j].Trace (call->Get_actual_index() + j);
	}

	if ( Map_actual (caller, *caller_info, edge, callee, *callee_info,
			 j, actual[j])) {

	    // There has been a change.  Mark it if the callee passes
	    // formals by value so that the callee will be processed the
	    // next time it is encountered for propagation:

	    if ( callee_info->Passes_formals_by_value() ) {
		INT16 iteration = Get_iteration();

		// For recursive edges, processing of the callee must occur
		// on the next iteration:
		if ( edge.Is_Recursive() ) {
		    callee_info->Set_changed_iter ( iteration+1 );
		    Set_changed();
		    
		    // Otherwise we only care about successors on this iteration:
		} else {
		    callee_info->Set_changed_iter ( iteration );
		}
	    }
	}
    }

    Verify_duplicated_actuals (edge, actual, acount);
}

// ====================================================================
//
// IPAA_DF_SOLVER::Meet_formals_iter_1
//
// This routine handles Meet for the FORMALS phase, iteration 1.  See
// the description of Meet below.
//
// A single iteration suffices in the absence of recursion.  Therefore,
// we update _changed on the first iteration only if we encounter a
// back edge.  In subsequent iterations, we must identify changes upon
// updating the reference sets.
//
// ====================================================================

void
IPAA_DF_SOLVER::Meet_formals_iter_1 ( IPA_NODE &caller )	
{
  // Get the callgraph and set up an IPAA_STEP object:
  IPA_CALL_GRAPH *cg = Get_callgraph();
  IPAA_NODE_INFO *info = caller.Mod_Ref_Info();
  IPAA_STEP istep ( *cg, caller );
  Set_step ( &istep );
  (void) Inc_formal_nodes ();

  // Initialize the changed indicator to 0:
  info->Set_changed_iter ( 0 );

  // Follow the successor edges:

  IPA_SUCC_ITER edge_iter ( cg, &caller );
  for ( edge_iter.First();
	!edge_iter.Is_Empty();
	edge_iter.Next() )
  {
    IPA_EDGE* edge = edge_iter.Current_Edge();

    // Go check for actuals to map:
    if ( edge != NULL ) {
      Map_actuals ( caller, *Get_callee(edge), *edge );
      (void) Inc_formal_edges ();
    }
  }

  // NOTE:  edge_iter is freed automatically when it is exhausted.

  // Trace if required:
  if ( Trace_IPAA ) {
    fprintf ( TFile, "<ipaa> Meet_formals_iter_1:\n" );
    istep.Trace();
    fflush ( TFile );
  }

  Set_step ( NULL );
}

// ====================================================================
//
// IPAA_DF_SOLVER::Meet_formals_iter_n
//
// This routine handles Meet for the FORMALS phase, iteration n>1.  See
// the description of Meet_formals_iter_1 above and of Meet below.
//
// ====================================================================

void
IPAA_DF_SOLVER::Meet_formals_iter_n ( IPA_NODE &caller )
{
  INT16 iteration = Get_iteration();
  IPAA_NODE_INFO *caller_info = caller.Mod_Ref_Info();

  // If none of our inputs have changed on this iteration (or last
  // iteration via a back-edge), we've nothing to do:
  if ( caller_info->Get_changed_iter() < iteration ) return;

  // If this routine passes no formals as value actuals, we've nothing
  // to propagate.  (This is not actually required, since we check
  // before setting the changed flags.):
  // if ( ! caller_info->Passes_formals_by_value() ) return;

  // Get the callgraph and set up an IPAA_STEP object:
  (void) Inc_formal_nodes ();
  IPA_CALL_GRAPH *cg = Get_callgraph();
  IPAA_STEP istep ( *cg, caller );
  Set_step ( &istep );

  // Follow the successor edges:

  IPA_SUCC_ITER edge_iter ( cg, &caller );

  for ( edge_iter.First();
	!edge_iter.Is_Empty();
	edge_iter.Next() )
  {
    IPA_EDGE* edge = edge_iter.Current_Edge();

    if ( edge != NULL ) {
      // Go check for actuals to map:
      Map_actuals ( caller, *Get_callee(edge), *edge );
      (void) Inc_formal_edges ();
    }
  }

  // NOTE:  edge_iter is freed automatically when it is exhausted.

  // Trace if required:
  if ( Trace_IPAA ) {
    fprintf ( TFile, "<ipaa> Meet_formals_iter_%d:\n", iteration );
    istep.Trace();
    fflush ( TFile );
  }

  Set_step ( NULL );
}

// ====================================================================
//
// IPAA_DF_SOLVER::Meet
//
// The Meet function merges data from the input edges and the current
// input annotation (given by the in parameter), producing a new
// input annotation, which it returns.  It must be able to cope with
// a NULL input annotation the first time.
//
// ====================================================================
//
// In phase PHASE_DIRECT, we are doing a post-order callgraph walk to
// identify data objects which are directly referenced by a routine or
// its callees.  In the subsequent phase PHASE_FORMALS, we do a
// pre-order walk which maps reference parameters and identifies their
// effects on the reference functions.
//
// We assume that IPL has identified the direct references within each
// routine.  Therefore, our purpose is to propagate them up through the
// callgraph so that each routine has identified both its and its
// callees' direct references.  On the first iteration of PHASE_DIRECT,
// we must initialize the IPAA_NODE_INFO reference sets for the current
// routine's direct references, and then merge in those of its callees.
// On subsequent iterations, the initialization is not necessary.
//
// A single iteration suffices in the absence of recursion.  Therefore,
// we update _changed on the first iteration only if we encounter a
// back edge.  In subsequent iterations, we must identify changes upon
// updating the reference sets.
//
// ====================================================================
//
// In phase PHASE_FORMALS, we are doing a pre-order callgraph walk to
// identify formal parameter mappings, i.e. which formals are mapped to
// the addresses of which global variables.
//
// Given this information, the references attributed to a call are its
// direct references as determined in PHASE_DIRECT, plus the direct
// references to its formals mapped to the actual globals passed to it.
// In addition, the call must be assumed to reference any addr_taken
// objects in the worst way -- we don't include those in the analysis
// here.
//
// ====================================================================

IPAA_DF_SOLVER::IN_T *
IPAA_DF_SOLVER::Meet ( IN_T *, void *vertex_user )
{
  IPA_NODE *caller = (IPA_NODE *)vertex_user;

  // The first time, trace the node iterator order:
  if ( Trace_Iterator ) {
    DFN *iter = this->Get_iter();
    INT32 i;

    fprintf ( TFile, "\n%sAnalyze_Direct_Refs: tracing iterator\n%s",
	      DBar, DBar );

    for ( i=DFN_first(iter); i< DFN_end(iter); ++i ) {
      INT vindex = DFN_v_list_i(iter,i);
      IPA_NODE *vertex = (IPA_NODE *)
	 NODE_user ( &GRAPH_v_i (Get_callgraph()->Graph(), vindex) );

      fprintf ( TFile, "Node %5d: ", vindex );
      if ( vertex != NULL ) {
	vertex->Trace ();
      } else {
	fprintf ( TFile, "NULL\n" );
      }
    }

    fprintf ( TFile, "%s\n", DBar );
    fflush ( TFile );

    Trace_Iterator = FALSE;
  }

  // Trace this node if required:
  if ( Trace_IPAA ) {
    if ( caller == NULL ) {
      fprintf ( TFile, "<ipaa> Meet: NULL vertex\n" );
    } else {
      fprintf ( TFile, "<ipaa> Meet: " );
      caller->Print ( TFile );
    }
    fflush ( TFile );
  }

  // Check for a real node:
  if ( caller == NULL ) {
    return NULL;
  }

  if ( Get_phase() == PHASE_DIRECT ) {

    if ( Get_iteration() == 1 ) {

      // In the first iteration, we do a straightforward walk, only
      // noting 'changed' for nodes with back edges, since they are the
      // only situation which will force another iteration.

      Meet_direct_iter_1 ( *caller );

    } else {

      // Report statistics for the previous iteration before first node:
      if ( Get_iteration() != Get_direct_iters() ) {
	Report_graph_statistics ( PHASE_DIRECT );
	Set_direct_iters ( Get_iteration() );
      }

      // On subsequent iterations, be more careful.  We only process
      // nodes marked as depending on back edges.  For those, we check
      // whether a forward-edge successor has changed in this
      // iteration, or a back-edge successor in the previous iteration,
      // before trying to actually merge information.  If we do merge,
      // we check first whether the information has changed.

      Meet_direct_iter_n ( *caller );
    }

  } else if ( Get_phase() == PHASE_FORMALS ) {

    if ( Get_iteration() == 1 ) {

      // In the first iteration, we do a straightforward walk, only
      // noting 'changed' for nodes with back edges, since they are the
      // only situation which will force another iteration.

      Meet_formals_iter_1 ( *caller );

    } else {

      // Report statistics for the previous iteration before first node:
      if ( Get_iteration() != Get_formal_iters() ) {
	Report_graph_statistics ( PHASE_FORMALS );
	Set_formal_iters ( Get_iteration() );
      }

      // On subsequent iterations, be more careful.  We only process
      // nodes marked as changed.
      // whether a forward-edge successor has changed in this
      // iteration, or a back-edge successor in the previous iteration,
      // before trying to actually merge information.  If we do merge,
      // we check first whether the information has changed.

      Meet_formals_iter_n ( *caller );
    }

  } else {
    ErrMsg ( EC_Unimplemented, "Meet -- unexpected phase" );
  }

  // We might have updated this -- get the new version:
  return caller->Mod_Ref_Info();
}

// ====================================================================
//
// Analyze_Direct_Refs: direct reference analysis
//
// This routine does a post-order callgraph walk to identify data
// objects which are directly referenced by a routine or its callees.
// It will be followed by pre-order walk which maps reference
// parameters and identifies their effects on the reference functions.
//
// We assume that IPL has identified the direct references within each
// routine.  Therefore, our purpose is to propagate them up through the
// callgraph so that each routine has identified both its and its
// callees' direct references.
//
// ====================================================================

static void
Analyze_Direct_Refs ( IPA_CALL_GRAPH &cg )
{
  IPAA_DF_SOLVER solver ( cg, BACKWARD, IPAA_Pool,
			  IPAA_DF_SOLVER::PHASE_DIRECT );

  // Do the analysis:
  solver.Solver ();

  // Report statistics for the final iteration:
  solver.Report_graph_statistics ( IPAA_DF_SOLVER::PHASE_DIRECT );

  // The solver is destroyed here, which restores the graph.
}

// ====================================================================
//
// Analyze_Indirect_Refs: parameter mapping and indirect ref analysis
//
// This routine does a pre-order walk to push reference parameter
// aliasing effects down to the leaves.
//
// ====================================================================

static void
Analyze_Indirect_Refs ( IPA_CALL_GRAPH &cg )
{
  IPAA_DF_SOLVER solver ( cg, FORWARD, IPAA_Pool,
			  IPAA_DF_SOLVER::PHASE_FORMALS );

  solver.Solver ();

  // Report statistics for the final iteration:
  solver.Report_graph_statistics ( IPAA_DF_SOLVER::PHASE_FORMALS );

  // Extract final statistical information from the solver:
  solver.Extract_solution ();

  // The solver is destroyed here, which restores the graph.
}

// ====================================================================
//
// IPAA::IPAA: Simple alias analysis constructor
//
// ====================================================================

IPAA::IPAA ( MEM_POOL *mem )
{
  _m = mem;
}

// ====================================================================
//
// IPAA::Do_Simple_IPAA: Simple alias analysis
//
// This module implements simple alias analysis suitable for Fortran
// without pointers.  It is a post-order walk of the call graph to
// calculate the recursive closure of the direct reference vectors,
// followed by a pre-order walk to push reference parameter aliasing
// effects down to the leaves.
//
// ====================================================================

void
IPAA::Do_Simple_IPAA ( IPA_CALL_GRAPH &cg )
{
    // Tracing?
    Trace_IPAA	= Get_Trace ( TP_IPA, IPA_TRACE_IPAA );
    Trace_Detail	= Get_Trace ( TP_IPA, IPA_TRACE_DETAIL );
    Trace_Stats	= Get_Trace ( TP_IPA, IPA_TRACE_STATS );
    Trace_Iterator = Get_Trace ( TP_IPA, IPA_TRACE_ITERATOR );
	IPA_Trace_Mod_Ref  = Get_Trace ( TP_IPA, IPA_TRACE_MODREF );
    if ( Trace_IPAA ) {
	Trace_Callgraph ( cg, FORWARD, "before IPAA" );
    }

    // Set up the memory pool: use the call graph's for now:
    IPAA_Pool = this->Get_IPAA_Mem_Pool();
    if ( IPAA_Pool == NULL ) {
	IPAA_Pool = Malloc_Mem_Pool;
	this->Set_IPAA_Mem_Pool ( IPAA_Pool );
    }
    MEM_POOL_Push ( MEM_local_pool_ptr );

    // Determine size to use for bit vectors:
    Mext_Size = ST_Table_Size(GLOBAL_SYMTAB);

    // global mod/ref list for functions with addr_taken
    icall_eref = CXX_NEW (IPAA_OBJECT_REF_SET (0), Malloc_Mem_Pool);
    icall_def = CXX_NEW (IPAA_OBJECT_REF_SET (0), Malloc_Mem_Pool);
    icall_kill = CXX_NEW (IPAA_OBJECT_REF_SET (0), Malloc_Mem_Pool);


    /* Analyze direct references in a post-order callgraph walk: */
    if ( Trace_IPAA ) {
	fprintf ( TFile, "\n%s%sIPAA: Analyze Direct References\n%s%s\n",
		 DBar, DBar, DBar, DBar );
	fflush ( TFile );
    }
    Analyze_Direct_Refs ( cg );

    /* Analyze parameter mapping and indirect references in a pre-order
     * callgraph walk:
     */
    if ( Trace_IPAA ) {
	fprintf ( TFile,
		 "\n%s%sIPAA: Analyze Formal Points-To Mapping\n%s%s\n",
		 DBar, DBar, DBar, DBar );
	fflush ( TFile );
    }
    Analyze_Indirect_Refs ( cg );

    // Forget the memory pool:
    IPAA_Pool = NULL;
    MEM_POOL_Pop ( MEM_local_pool_ptr );
}

#ifdef _OBSOLETE
// ====================================================================
//
// IPAA -- Support for table emission to the IPAA summary files.
//
// ====================================================================

// We use a separate memory pool for summary emission.  During the
// emission phase, IPAA_Pool is set to this as well:
MEM_POOL IPAA_Emit_Pool;

// We also use a separate memory pool for callsite and local symbol
// mapping emission.  It must survive until after the .I files are
// written, so we allocate the control block with malloc:
MEM_POOL *IPAA_Callsite_Pool;

// Hash table for strings in the summary string table.
// We avoid all duplicates:
typedef class USER_HASH_TABLE < char*, INT32, String_Hash, String_Equal >
	STRING_HASH_TABLE;
STRING_HASH_TABLE *String_Hash_Table;
String_Hash Hash_Func;

// To avoid repeated lookup of global names, we build a table indexed
// by the merged symbol table index and look up names there before
// inserting them:
static INT32 *Name_Table = NULL;

// To avoid repeated lookup, we build a table indexed by the merged
// symbol table index, pointing to symrefs of the corresponding
// global symbols in the summary symref table.
static SYMREF_IX *Symref_Table = NULL;


// ====================================================================
//
// IPAA::Emit_name
//
// Emit a name to the IPAA summary string table and return its index.
// A hash table (above) is used to avoid any duplication.
//
// WARNING:  The hash table implementation used stores the string
// pointer as the key rather than the string itself.  So the name
// entered must not be a string which will be overwritten before we
// are done with the hash table.
//
// ====================================================================

INT32
IPAA::Emit_name ( const char *name )
{
  INT32 index = String_Hash_Table->Find ( (char *)name );

  if ( index == 0 ) {
    if ( Trace_IPAA_Summary ) {
      fprintf ( TFile,
		"<ipaa> Emit_Name 0x%08lx hash 0x%04x (%2d) %s\n",
		name, Hash_Func(name), Hash_Func(name)%75,
		(char *)name );
      fflush ( TFile );
    }
    index = _summary->Add_string ( (char *)name );
    String_Hash_Table->Enter ( (char *)name, index );
    if ( Trace_IPAA_Summary ) {
      String_Hash_Table->Print ( TFile );
    }
  }

  return index;
}

// ====================================================================
//
// IPAA::Emit_global_name
//
// Emit a global name to the summary string table, returning its index.
//
// To avoid repeated lookup, we build a table indexed by the merged
// symbol table index, pointing to the names of the corresponding
// global symbols in the summary string table.
//
// WARNING:  Name_Table must be cleared to NULL if the memory pool is
// released.
//
// ====================================================================

INT32
IPAA::Emit_global_name ( INT32 merged_id )
{
  if ( Name_Table == NULL ) {
    Name_Table = TYPE_MEM_POOL_ALLOC_N ( INT32, &IPAA_Emit_Pool,
					 ST_Table_Size(GLOBAL_SYMTAB) );
  }

  if ( Name_Table[merged_id] == NULL ) {
	ST_IDX st_idx = make_ST_IDX( merged_id, GLOBAL_SYMTAB );
    Name_Table[merged_id] = Emit_name ( ST_name(St_Table[st_idx]) );
  }

  return Name_Table[merged_id];
}


// ====================================================================
//
// IPAA::Emit_pu_name
//
// Construct an IPAA_SYMBOL_REF for a PU name.
//
// TODO:  Currently always builds a simple name SYMREF -- should
// identify those which are global/local symbols and build the
// appropriate kind.
//
// ====================================================================

INT32
IPAA::Emit_pu_name ( const char *name )
{
  // Build the symbol reference:
  IPAA_SYMBOL_REF sref ( SREF_NAME, 0, Emit_name ( name ) );

  // Add it to the tables and return its index:
  return _summary->Add_symref ( sref );
}

// ====================================================================
//
// IPAA::Emit_global
//
// Construct an IPAA_SYMBOL_REF for a global symbol.  The parameter
// reuse_ok indicates whether it is OK to reference an existing symref
// instead of creating a new one.
//
// NOTE:  We always remember the most recent reference created to a
// given global SYMREF.
//
// ====================================================================

SYMREF_IX
IPAA::Emit_global ( INT32 merged_id, BOOL reuse_ok )
{
  IPAA_SYMBOL_REF sref ( SREF_GLOBAL, 0,
			 Emit_global_name ( merged_id ) );

  // Create the lookup table if needed:
  if ( Symref_Table == NULL ) {
    Symref_Table = TYPE_MEM_POOL_ALLOC_N ( SYMREF_IX,
					   &IPAA_Emit_Pool,
					   ST_Table_Size(GLOBAL_SYMTAB) );
  }

  if ( reuse_ok && Symref_Table[merged_id] != 0 ) {
    return Symref_Table[merged_id];
  } else {
    return Symref_Table[merged_id] = _summary->Add_symref ( sref );
  }
}

// ====================================================================
//
// IPAA::Emit_IPAA_node
//
// This method collects the IPAA information for a single callgraph
// node into the format required by emission to the output files.
//
// ====================================================================

void
IPAA::Emit_IPAA_node ( const IPA_NODE &node )
{
  IPAA_NODE_INFO *info = node.Mod_Ref_Info();
  INT32 i, count;

  // Trace entry:
  if ( Trace_IPAA_Summary ) {
    fprintf ( TFile, "<ipaa> ### Emit_IPAA_node ( %s in %s ) ###\n",
	      node.Name(), node.Input_File_Name() );
    if ( info == NULL ) {
      fprintf ( TFile,
		"<ipaa> ### No IPAA information available ###\n" );
    }
  }

  // We may not have information in which case we do nothing and let
  // WOPT assume the worst case (PV 332138):
  if ( info == NULL ) return;

  INT32 fcount = info->Get_fcount();

  // Generate the IPAA summary string table entry for the filename:
  INT32 filename_ix = Emit_name ( node . Input_File_Name() );

  // Generate the IPAA_SYMBOL_REF for the PU itself:
  SYMREF_IX pu = Emit_pu_name ( node . Name() );

  // Generate the points-to sets for the formals.  This becomes a set
  // (with cardinality the number of formals) of sets, each being the
  // set of references for one of the formals.
  SET_IX formal_mapping = 0;
  SET_IX formal_modref = 0;

  if ( fcount > 0 ) {
    // Generate the points-to sets for the formals.  This becomes a set
    // (with cardinality the number of formals) of sets, each being the
    // set of references for one of the formals.
    IPAA_SET *formal_sets = (IPAA_SET *)
	CXX_NEW_ARRAY ( IPAA_SET, fcount, MEM_local_pool_ptr );
    IPAA_FORMAL_MAP **maps = info->Get_fmap();
    
    for ( i = 0; i < fcount; i++ ) {
      IPAA_FORMAL_MAP *map = maps ? maps[i] : NULL;
      IPAA_SET *set = formal_sets + i;

      set->Set_kind ( SET_SYMREF );
      set->Set_index ( 0 );
      set->Set_size ( 0 );
      if ( (map != NULL) && ( count = map->Get_size() ) > 0 ) {
	SYMREF_IX first = 0, last;
	INT32 j = 0;

	if ( map->Get_junk() ) {
	  // We have an unknown reference, so we start the set with
	  // an unknown reference and then add the others:
	  // NOTE:  We decrement count so the j loop below is OK.
	  if ( count-- == 0 ) {
	    // We can just use the dedicated unknown SYMREF.
	    // TODO:  Optimize this by eliminating any explicit
	    // references to globals which would be implied by the
	    // unknown reference.
	    first = SYMREF_IX_UNKNOWN;
	  } else {
	    IPAA_SYMBOL_REF sref ( SREF_UNKNOWN, 0, 0 );
	    first = _summary->Add_symref ( sref );
	  }
	}

	for ( j = 0; j < count; j++ ) {
	  IPAA_FORMAL_MAP::MAP_ELMT melmt = map->Get_elmt ( j );

	  if ( melmt != IPAA_FORMAL_MAP::MAP_NONE ) {
	    last = Emit_global ( (INT32) melmt, FALSE );
	    if ( first == 0 ) first = last;
	  }
	}

	set->Set_index ( first );
	set->Set_size  ( map->Get_size() );
      }
    }

    // Now we have an array of formal mapping sets -- emit it:
    IPAA_SET formal_maps ( formal_sets, fcount );
    formal_mapping = _summary->Add_set ( formal_maps );

    // Generate the formal mod/ref masks:
    REFBITS *masks = TYPE_MEM_POOL_ALLOC_N ( REFBITS,
					     MEM_local_pool_ptr,
					     fcount );
    for ( i = 0; i < fcount; i++ ) {
      if ( info-> Is_formal_dref_elmt ( i ) ) masks[i] |= MODREF_DREF;
      if ( info-> Is_formal_iref_elmt ( i ) ) masks[i] |= MODREF_IREF;
      if ( info-> Is_formal_dmod_elmt ( i ) ) masks[i] |= MODREF_DMOD;
      if ( info-> Is_formal_imod_elmt ( i ) ) masks[i] |= MODREF_IMOD;
      if ( info-> Is_formal_dkill_elmt( i ) ) masks[i] |= MODREF_DKILL;
      if ( info-> Is_formal_ikill_elmt( i ) ) masks[i] |= MODREF_IKILL;
    }
    IPAA_SET formal_modrefs ( masks, fcount );
    formal_modref = _summary->Add_set ( formal_modrefs );
  }

  // Generate the global/local mod/ref data:
  // WARNING:  we depend on kill being a subset of def!
  SPARSE_ITER eref ( info->Get_eref_set() -> _data );
  SPARSE_ITER  def ( info-> Get_def_set() -> _data );
  SPARSE_ITER kill ( info->Get_kill_set() -> _data );
  REFBITS refbits;
  INT32 elmt;
  SBV_ELEMENT eref_elmt = eref.Element();
  SBV_ELEMENT  def_elmt =  def.Element();
  BOOL eref_valid = (eref_elmt != SBV_Invalid );
  BOOL  def_valid = ( def_elmt != SBV_Invalid );
  MODREF_IX first_global = 0;
  INT32 num_globals = 0;

  while ( eref_valid || def_valid ) {
    refbits = 0;
    if ( eref_valid
      && ( !def_valid || eref_elmt <= def_elmt ) )
    {
      // Earliest element is an EREF, and maybe DEF, KILL:
      elmt = eref_elmt;

      refbits = MODREF_DREF;
      if ( eref_valid = eref.Advance() ) {
	eref_elmt = eref.Element();
      }
      if ( def_elmt == elmt ) {
	refbits |= MODREF_DMOD;
	if ( def_valid = def.Advance() ) {
	  def_elmt = def.Element();
	}
	if ( kill.Element() == elmt ) {
	  refbits |= MODREF_DKILL;
	  (void) kill.Advance();
	}
      }
    } else {
      // Earliest element is DEF, maybe KILL (not EREF):
      elmt = def_elmt;

      refbits = MODREF_DMOD;
      if ( def_valid = def.Advance() ) {
	def_elmt = def.Element();
      }
      if ( kill.Element() == elmt ) {
	refbits |= MODREF_DKILL;
	(void) kill.Advance();
      }
    }

    // Write it:
    IPAA_MODREF modref ( refbits, Emit_global ( elmt, TRUE ) );
    MODREF_IX mix = _summary->Add_modref ( modref );
    if ( first_global == 0 ) first_global = mix;
    ++ num_globals;
  }

  // Now write the set of globals:
  SET_IX globals = 0;
  if ( num_globals > 0 ) {
    IPAA_SET global_set ( SET_MODREF, first_global, num_globals );
    globals = _summary->Add_set ( global_set );
  }

  // Now we have all of the information for the PU's info record:
  IPAA_PU_INFO pu_info ( pu, formal_mapping, formal_modref, globals );
  info -> Set_pu_info ( _summary->Add_pu_info ( pu_info ) );
}

// ====================================================================
//
// IPAA::Emit_IPAA_callsites
//
// This method collects the IPAA information for a single callgraph
// node into the format required by emission to the output files.
//
// ====================================================================

void
IPAA::Emit_IPAA_callsites ( IPA_NODE &node )
{
  IPAA_NODE_INFO *info = node.Mod_Ref_Info();

  // Trace entry:
  if ( Trace_IPAA_Summary ) {
    fprintf ( TFile, "<ipaa> ### Emit_IPAA_callsites ( %s in %s ) ###\n",
	      node.Name(), node.Input_File_Name() );
    if ( info == NULL ) {
      fprintf ( TFile,
		"<ipaa> ### No IPAA information available ###\n" );
    }
  }

  // We may not have information in which case we do nothing and let
  // WOPT assume the worst case (PV 332138):
  if ( info == NULL ) return;

  // Build an IPAA_CALLSITES class with enough space for successors:
  IPAA_CALLSITES *callsites =
	CXX_NEW ( IPAA_CALLSITES ( info->Get_pu_info(),
				   node . Total_Succ(),
				   IPAA_Callsite_Pool ),
		  IPAA_Callsite_Pool );

  // Walk the successor edges and add callsite mappings:
  IPA_SUCC_ITER edge_iter ( Get_cg(), &node );
  for ( edge_iter.First();
	! edge_iter.Is_Empty();
        edge_iter.Next() )
  {
    IPA_EDGE *e = edge_iter.Current_Edge();

    if ( e != NULL ) {
      if ( Trace_IPAA_Summary ) {
	fprintf ( TFile, "<ipaa>\tedge: " );
	e->Trace ( Get_cg() );
      }

      INT32 map_id = e -> Summary_Callsite() -> Get_map_id();
      IPAA_NODE_INFO *callee_info = Get_cg() -> Callee ( e )
					     -> Mod_Ref_Info ();

      // If we have info for the callee, emit the callsite.  We may
      // not have information in which case we do nothing and let
      // WOPT assume the worst case (PV 332138):
      if ( callee_info != NULL ) {
	INT32 callee = callee_info -> Get_pu_info();
	(void ) callsites -> Add_callsite ( map_id, callee );
      }
    }
  }

  // Write the map to the PU_INFO struct:
  PU_Info *pu = node . PU_Info();
  Set_PU_Info_callsites_ptr ( pu, callsites );
  Set_PU_Info_state ( pu, WT_CALLSITES, Subsect_InMem );

  if ( Trace_IPAA_Summary ) {
    callsites -> Trace ( node.Name() );
  }

  return;
}

// ====================================================================
//
// IPAA::Emit_Simple_IPAA
//
// This method collects the IPAA information into the format required
// by emission to the output files.
//
// ====================================================================

void
IPAA::Emit_Simple_IPAA ( IPA_CALL_GRAPH &cg )
{
  NODE_INDEX i;

  // Tracing?
  Trace_IPAA = Get_Trace ( TP_IPA, IPA_TRACE_IPAA );
  Trace_IPAA_Summary = Get_Trace ( TP_IPA, IPA_TRACE_IPAA_SUMMARY );
  if ( Trace_IPAA || Trace_IPAA_Summary ) {
    Trace_Callgraph ( cg, FORWARD, "emitting IPAA summary" );
  }
  _cg = &cg;

  // Set up a temporary memory pool for emission use:
  MEM_POOL_Initialize ( &IPAA_Emit_Pool, "IPAA summary pool", TRUE );
  MEM_POOL_Push ( &IPAA_Emit_Pool );
  IPAA_Pool = &IPAA_Emit_Pool;

  // Push the local memory pool:
  MEM_POOL_Push ( MEM_local_pool_ptr );

  // Allocate and initialize the IPAA_SUMMARY record:
  _summary = CXX_NEW ( IPAA_SUMMARY ( &IPAA_Emit_Pool ),
			&IPAA_Emit_Pool );
  if ( _summary == NULL ) {
    ErrMsg ( EC_No_Mem, "IPAA::Emit_Simple_IPAA : summary" );
  }

  // Initialize the string hash table:
  String_Hash_Table = CXX_NEW ( STRING_HASH_TABLE ( ST_Table_Size(GLOBAL_SYMTAB),
						    &IPAA_Emit_Pool ),
				&IPAA_Emit_Pool );

  // Build the summary record.  This requires building an iterator
  // over the callgraph, and then processing each node.
  DFN *dfn = Depth_First_Ordering ( cg.Graph(), &IPAA_Emit_Pool );
  for ( i = DFN_first(dfn); i < DFN_end(dfn); ++i ) {
    NODE_INDEX ix = DFN_v_list_i(dfn,i);
    IPA_NODE *node = cg.Graph()->Node_User(ix);
    if ( node != NULL ) {
      Emit_IPAA_node ( *node );
    }
  }

  // Set up a temporary memory pool for callsite/local symbol use:
  IPAA_Callsite_Pool = (MEM_POOL *) malloc ( sizeof(MEM_POOL) );
  if ( IPAA_Callsite_Pool == NULL ) {
    ErrMsg ( EC_No_Mem, "IPAA::Emit_Simple_IPAA : Callsites" );
  }
  MEM_POOL_Initialize ( IPAA_Callsite_Pool, "IPAA callsite pool", FALSE );
  MEM_POOL_Push ( IPAA_Callsite_Pool );

  // Build the callsites records, one for each PU.  This requires
  // a second iteration over the callgraph, since we must have the
  // IPAA_PU_INFO indices assigned first by the previous step, but
  // the previous depth first numbering will do just fine:
  for ( i = DFN_first(dfn); i < DFN_end(dfn); ++i ) {
    NODE_INDEX ix = DFN_v_list_i(dfn,i);
    IPA_NODE *node = cg.Graph()->Node_User(ix);
    if ( node != NULL ) {
      Emit_IPAA_callsites ( *node );
    }
  }

  // Build an IPAA_FILE_DESCRIPTOR, and write the summary file:
  extern char *tmpdir __attribute__((weak));
  char *Ipa_Path_Name = (char *) malloc
	( strlen(tmpdir) + strlen(Ipa_File_Name) + 2 );
  (void) sprintf ( Ipa_Path_Name, "%s/%s", tmpdir, Ipa_File_Name );
  _summary->Write ( Ipa_Path_Name );
  if ( Trace_IPAA || Trace_IPAA_Summary ) {
    fprintf ( TFile,
	      "<ipaa> Writing IPAA summary to %s\n", Ipa_Path_Name );
  }

  // Add the pathname to ld's temporary file list to be removed after
  // compilation.  WARNING:  this retains a pointer to the pathname,
  // so we can't free it here.
  add_to_tmp_file_list ( Ipa_Path_Name );

  // Destroy the IPAA_SUMMARY record -- we don't use CXX_DELETE
  // because we're about to remove the pool anyway:
  _summary -> ~IPAA_SUMMARY ();
  _summary = NULL;

  // Similarly, just forget the lookup tables:
  String_Hash_Table = NULL;
  Symref_Table = NULL;
  Name_Table = NULL;
 
  // Forget the memory pools:
  MEM_POOL_Pop ( &IPAA_Emit_Pool );
  IPAA_Pool = NULL;
  MEM_POOL_Pop ( MEM_local_pool_ptr );
}
#endif // _OBSOLETE
