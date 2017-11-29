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
// ====================================================================
// ====================================================================
//
// Module: opt_ipaa_summary.h
// $Revision: 1.2 $
// $Date: 02/11/07 23:41:38-00:00 $
// $Author: fchow@keyresearch.com $
// $Source: /scratch/mee/2.4-65/kpro64-pending/be/com/SCCS/s.opt_ipaa_summary.h $
//
// Revision history:
//  2-Nov-95 - Original Version
//
// Description:
//
// Definition of the summary information interface between IPAA and
// the optimizer.  We observe the following principles:
//
//  1)	The information consists of several tables, with all
//	inter-table references being indices.  The reason is (4)
//	below.
//
//  2)	There is a file header table which contains information
//	identifying the layout of the file, i.e. which tables are
//	present, how big they are, and where they are in the file.
//	It also contains a version identifier for verification.  Note,
//	however, that unlike the local summary information, the
//	compilation model currently implies consuming this information
//	immediately, so there is no need to cope with multiple versions
//	in the consumer (WOPT).
//
//  3)	The information is almost independent of the WHIRL symbol
//	table.  For local variables, that is not easy, so we don't try,
//	but for most purposes, reference to the symbol table won't be
//	necessary.
//
//  4)	The IPAA summary file will be read read-only by the back end,
//	so that it may be memory-mapped concurrently by multiple back
//	end compilations, and we will limit the data structures which
//	must be converted on input to the file descriptor.
//
// In addition, we require the following:
//
//  5)	Index zero is not a normal entry for any of the tables.
//	Therefore, it always represents an uninitialized, invalid, or
//	unknown entry.
//
// Most IPAA summary information will be written to a single file,
// named "<outputname>.ipaa" if -keep is specified.  However, the
// following must be put in the WHIRL .o, in section .WHIRL.ipaa:
//
//   -	IPAA_LOCALS:  These contain the mappings from per-file IDs to
//	the local symbol tables.
//
//   -	IPAA_CALLSITES:  These contain the mappings from callsites to
//	the callees' information in the ipaa file.
//
// See also opt_ipaa_io.(h|cxx) for I/O routines which must reside in
// be.so because they are called before wopt.so is loaded.
//
// ====================================================================
// ====================================================================

#ifndef cxx_opt_ipaa_summary_INCLUDED
#define cxx_opt_ipaa_summary_INCLUDED

#ifdef _KEEP_RCS_ID
static char *opt_ipaa_summary_rcs_id = "$Source: /scratch/mee/2.4-65/kpro64-pending/be/com/SCCS/s.opt_ipaa_summary.h $ $Revision: 1.2 $";
#endif /* _KEEP_RCS_ID */

// Trace flag for IPAA summary file build/output:
extern BOOL Trace_IPAA_Summary;

// Forward and incomplete definitions:
typedef mINT32 SECTION_IX;
class IPAA_SUMMARY;
struct st;

// Global pointer to the summary information.  This pointer MUST be
// either NULL or point to the current summary class instance; the
// latter is required for many of the methods in this module.  The
// IPAA_SUMMARY constructors/destructor deal with this automatically;
// anyone trying to manage multiple instances at once must be careful.
extern IPAA_SUMMARY *IPAA_Summary;

// ====================================================================
// ====================================================================
//
// IPAA_SYMBOL_REF
//
// This is the basic symbol descriptor -- all symbol references in the
// IPAA information point to one of these.  They should be distinct,
// that is, two references to different non-SECTION entries in this
// table should be to different data.  SECTION entries ultimately
// point to a non-SECTION entry here (see IPAA_SECTION below).  This
// table therefore forms the universe for all sets of symbols from
// IPAA.
//
// The kinds of symbols (SYMBOL_REF_KIND) currently represented are:
//
//   SREF_NAME:		This is a name, with no other semantic
//			implications.  It may be used for such
//			purposes as referencing file names or symbol
//			names (e.g. PUs) without knowing their
//			locality characteristics.
//
//   SREF_GLOBAL:	This is a global symbol, i.e. visible to the
//			linker.  The only identification required is
//			its external name (which may be mangled).
//
//   SREF_LOCAL:	This is a symbol local to one of the component
//			files.  It is identified by the name of the
//			source file where it originated, and an ID
//			within that file.  Except while compiling that
//			file, more information than this ID should not
//			be required for identification.
//
//   SREF_SECTION:	This is a section of another symbol.  Its ID
//			fields point to the IPAA_SYMBOL_REF for the
//			base symbol, and to a section table entry which
//			contains the start offset and size of the
//			section.  Eventually, this will likely be
//			extended to contain stride information as well.
//
//   SREF_UNKNOWN:	The referenced symbol is unknown.  Therefore
//			the information applies to any potentially
//			addr_taken object.
//
// The index type of this table is SYMREF_IX.  It currently uses the
// following indices for dedicated purposes:
//
//   SYMREF_IX_INVALID (0): An invalid entry.
//
//   SYMREF_IX_UNKNOWN (1): An SREF_UNKNOWN entry.
//
//   SYMREF_IX_FIRST (2): The first normal (non-dedicated) entry.
//
// TODO:  Probably add flags (in _dummy) for address-taken and for
// automatic variables (SREF_LOCAL only).
//
// ====================================================================
// ====================================================================

// The valid kinds of symbol reference:
typedef enum {
  SREF_INVALID,		// Invalid kind
  SREF_NAME,		// Simple name
  SREF_GLOBAL,		// Global symbol
  SREF_LOCAL,		// Local symbol
  SREF_SECTION,		// Section of another symbol
  SREF_UNKNOWN,		// Unknown symbol
  SREF_MASK	= 7	// Make sure this is a valid mask for others
} SYMBOL_REF_KIND;

// Print symbolic name of a kind to a string:
extern char * sPrint_Symref_Kind ( SYMBOL_REF_KIND kind );

// The type of a symbol reference table index:
typedef mINT32 SYMREF_IX;
const SYMREF_IX SYMREF_IX_INVALID = 0;
const SYMREF_IX SYMREF_IX_UNKNOWN = 1;
const SYMREF_IX SYMREF_IX_FIRST = 2;

// The symbol reference class -- members of the table:
class IPAA_SYMBOL_REF
{
  mINT32 _id1	:24;	// NAME/GLOBAL: unused
			// LOCAL:  per-file identifier
			//	-- See IPAA_LOCAL_MAP below
			// SECTION: section table index
			// UNKNOWN: unused
  mINT32 _dummy	:4;
  mINT32 _kind	:4;	// SYMBOL_REF_KIND of the symbol
  mINT32 _id2;		// NAME/GLOBAL: string table index (name)
			// LOCAL:  string table index (file name)
			// SECTION: base symbol ref index
			// UNKNOWN: unused

 public:

  // Constructor/destructor:
  IPAA_SYMBOL_REF ( SYMBOL_REF_KIND kind,
		    INT32 id1, INT32 id2 )
	{ _kind = kind; _id1 = id1; _id2 = id2; _dummy = 0; }
  ~IPAA_SYMBOL_REF () {}

  // Tracing:
  void Print ( FILE *f, const char *pfx ) const;
  void Trace ( const char *pfx ) const;

  // Member access:
  SYMBOL_REF_KIND Kind (void) const	{ return (SYMBOL_REF_KIND)_kind; }

  INT32 Name_id (void) const		{ return _id2; }
  inline char *Name (void) const;

  INT32 Global_id (void) const		{ return _id2; }
  inline char *Global_name (void) const;

  INT32 Local_id (void) const		{ return _id1; }
  INT32 Local_file_id (void) const	{ return _id2; }
  inline char *Local_file_name (void) const;

  SECTION_IX Section_id (void) const	{ return (SECTION_IX)_id1; }
  SYMREF_IX Section_base (void) const	{ return (SYMREF_IX)_id2; }
  inline INT64 Section_disp (void) const;
  inline INT64 Section_size (void) const;
};

// ====================================================================
// ====================================================================
//
// IPAA_SECTION
//
// This class provides the information which describes the part of a
// base symbol referenced by a section, i.e. its displacement and size.
// Eventually, this will likely be extended to contain stride
// information as well.
//
// The index type of this table is SECTION_IX.  Index 0 is invalid.
//
// ====================================================================
// ====================================================================

class IPAA_SECTION
{
  INT64 _disp;		// Byte displacement from start of the base
  INT64 _size;		// Byte size of the section

 public:

  // Constructor/destructor:
  IPAA_SECTION ( INT64 disp, INT64 size )
	{ _disp = disp; _size = size; }
  ~IPAA_SECTION () {}

  // Tracing:
  void Print ( FILE *f ) const;
  void Trace ( void ) const;

  // Member access:
  INT64 Disp (void) const	{ return _disp; }
  INT64 Size (void) const	{ return _size; }
};

// ====================================================================
// ====================================================================
//
// IPAA_MODREF
//
// This class provides mod/ref information for a single symbol
// reference.  If the symbol reference is 0, it implies the given
// mod/ref attributes for an unknown symbol or symbols (e.g. via an
// unanalyzable pointer expression).
//
// The MODREF_KIND type matches the definitions of the IPL mod/ref bits
// in ipl_cg.h -- these should be merged.  Note that AREF is not used
// in the IPAA output, and the indirect reference kinds are used only
// for formals.
//
// The index type of this table is MODREF_IX.
//
// ====================================================================
// ====================================================================

// Define the types which we use to represent mod/ref information:
typedef enum {
  MODREF_NONE	= 0,	// No mod/ref/kill
  MODREF_IMOD	= 1,	// Indirect modification (maybe) -- only formals
  MODREF_DMOD	= 2,	// Direct modification (maybe)
  MODREF_IREF	= 4,	// Indirect reference (maybe)	-- only formals
  MODREF_DREF	= 8,	// Direct reference (maybe)
  MODREF_AREF	= 16,	// Address reference (&a)	-- unused
  MODREF_IKILL	= 32,	// Indirect kill (always)	-- only formals
  MODREF_DKILL	= 64,	// Direct kill (always)
  MODREF_ANY	= 127	// Anything (full mask)
} MODREF_KIND;

typedef mINT8 REFBITS;

// Print MOD/REF bits symbolically to a string:
extern char *sPrint_Modref ( REFBITS bits );

typedef mINT32 MODREF_IX;

// Conservative mod/ref information:
const REFBITS MODREF_CONSERVATIVE =
	      MODREF_DMOD|MODREF_DREF|MODREF_IMOD|MODREF_IREF;

class IPAA_MODREF
{
  SYMREF_IX _symref :24; // IPAA_SYMBOL_REF referenced: 0 => unknown
  mINT32 _modref :8;	// Mod/ref flags

 public:

  // Constructor/destructor:
  IPAA_MODREF ( REFBITS modref, INT32 symref )
	{ _modref = modref; _symref = symref; }
  ~IPAA_MODREF () {}

  // Tracing:
  void Print ( FILE *f ) const;
  void Trace ( void ) const;

  // Member access:
  REFBITS Modref (void) const		{ return _modref; }
  SYMREF_IX Symref (void) const		{ return _symref; }
  // More to come, for setting, and for modref bit access...
};

// ====================================================================
// ====================================================================
//
// IPAA_SET
//
// This class provides information about a set of something.  The
// current kinds of set are:
//
//   SET_MODREF:	A set of IPAA_MODREF objects, with the index
//			of the first one.  This is used to represent
//			the global mod/ref information for a PU.
//
//   SET_REFBITS:	A set of REFBITS objects, with the index
//			of the first one.  This is used to represent
//			the formal mod/ref information for a PU.
//
//   SET_SYMREF:	A set of IPAA_SYMBOL_REF objects, with the
//			index of the first one.  This is used to
//			represent the points-to set of a formal.
//
//   SET_SETS:		A set of IPAA_SET objects, with the index of
//			the first one.  This is used to represent the
//			set of formal points-to sets for a PU.
//
// In addition, it seems likely that we will add other kinds, in
// particular to represent sets by bit vectors when that is smaller.
// Those would be represented by indices into a new table.
//
// The index type of this table is SET_IX.
//
// ====================================================================
// ====================================================================

// Declare the possible kinds of sets (based on their elements):
typedef enum {
  SET_INVALID,	// Invalid kind
  SET_MODREF,	// Simple set of IPAA_MODREF objects
  SET_REFBITS,	// Simple set of REFBITS objects
  SET_SYMREF,	// Simple set of IPAA_SYMBOL_REF objects
  SET_SETS,	// Simple set of IPAA_SET objects
} IPAA_SET_KIND;

// Print a set kind to a string:
extern char * sPrint_Set_Kind ( IPAA_SET_KIND kind );

// The set table is indexed by this type:
typedef mINT32 SET_IX;

// Declare the actual set class:
class IPAA_SET
{
  mINT32 _size	:24;		// Size of set
  IPAA_SET_KIND _kind	:8;	// Kind of set -- see above
  mINT32 _index;		// Index of first element of set

 public:

  // Constructors/destructor:
  IPAA_SET ( void ) { _kind = SET_INVALID; _index = 0; _size = 0; }
  IPAA_SET ( IPAA_SET_KIND kind, INT32 index, INT32 size )
	{ _kind = kind; _index = index; _size = size; }
  IPAA_SET ( const REFBITS *refbits, INT32 size );
  IPAA_SET ( const IPAA_MODREF *modref, INT32 size );
  IPAA_SET ( const IPAA_SYMBOL_REF *symref, INT32 size );
  IPAA_SET ( const IPAA_SET *set, INT32 size );
  ~IPAA_SET () {}

  // Tracing:
  void Print ( FILE *f, BOOL print_members = TRUE ) const;
  void Trace ( BOOL print_members = TRUE ) const;

  // Member access:
  IPAA_SET_KIND Kind (void) const	{ return (IPAA_SET_KIND)_kind; }
  void	Set_kind ( IPAA_SET_KIND kind )	{ _kind = kind; }
  INT32 Index (void) const		{ return _index; }
  void	Set_index ( INT32 ix )		{ _index = ix; }
  INT32 Size (void) const		{ return _size; }
  void	Set_size ( INT32 sz )		{ _size = sz; }
};

// ====================================================================
// ====================================================================
//
// IPAA_PU_INFO
//
// This class provides mod/ref information for a program unit.
//
// The index type of this table is PU_INFO_IX.
//
// ====================================================================
// ====================================================================

typedef mINT32 PU_INFO_IX;

class IPAA_PU_INFO
{
  SYMREF_IX _pu	:24;		// IPAA_SYMBOL_REF for the PU itself
  mINT32 _flags	:8;		// Attribute flags
  SET_IX _points_to;		// Set of formal points-to sets
  SET_IX _formal_modref;	// Set of formal mod/ref masks
  SET_IX _global_modref;	// Set of global mod/ref data

 public:

  // Constructor/destructor:
  IPAA_PU_INFO ( SYMREF_IX pu, SET_IX pt, SET_IX formals, SET_IX globals )
	{
	  _flags = 0;
	  _pu = pu;
	  _points_to = pt;
	  _formal_modref = formals;
	  _global_modref = globals;
	}
  ~IPAA_PU_INFO () {}

  // Tracing:
  void Print ( FILE *f ) const;
  void Trace ( void ) const;

  // Member access:
  SYMREF_IX Pu_ix (void) const		{ return _pu; }
  SET_IX Points_to_ix (void) const	{ return _points_to; }
  SET_IX Formal_modref_ix (void) const	{ return _formal_modref; }
  SET_IX Global_modref_ix (void) const	{ return _global_modref; }

  // Go get the name of the PU, via Pu_ix and IPAA_Summary:
  inline char * Name (void) const;

  // More to come, for access for individual formals, etc.
};

// ====================================================================
// ====================================================================
//
// IPAA_FILE_DESCRIPTOR / IPAA_SUMMARY
//
// These classes describe the contents of an IPAA file.  They comes in
// two forms:  IPAA_FILE_DESCRIPTOR is the in-file form, while
// IPAA_SUMMARY is the form used in IPAA and the back end (WOPT).
//
// ====================================================================
// ====================================================================

class IPAA_FILE_DESCRIPTOR;
class IPAA_SUMMARY;

#define IPAA_MAJOR	0
#define IPAA_MINOR	0
#define IPAA_MAGIC	0x49504141

// Attributes stored in the IPAA_FILE_DESCRIPTOR / IPAA_SUMMARY flags:
typedef enum {
  IPAA_ADDR_ANAL	= 1	// Addressing (addr_taken) analyzed
} ATTR_MASK;

class IPAA_TABLE_DESCRIPTOR
{
  friend class IPAA_FILE_DESCRIPTOR;
  friend class IPAA_SUMMARY;

  mINT32 _table_size	:24;	// Number of elements in the table
  mINT32 _element_size	:8;	// Byte size of an element
  mUINT32 _file_offset;		// Byte offset of table in the file

  IPAA_TABLE_DESCRIPTOR ( INT32 elmt, INT32 size, UINT32 disp )
  {
    _element_size = elmt;
    _table_size = size,
    _file_offset = disp;
  }
  ~IPAA_TABLE_DESCRIPTOR () {}
};

class IPAA_FILE_DESCRIPTOR
{
  friend class IPAA_SUMMARY;

  mINT32 _magic;			// Magic number
  mINT8 _major;				// Major revision
  mINT8 _minor;				// Minor revision
  mINT8 _size;				// Byte size of file descriptor
  mINT8 _flags;				// Attributes
  IPAA_TABLE_DESCRIPTOR _strings;	// string (name) table
  IPAA_TABLE_DESCRIPTOR _symrefs;	// IPAA_SYMBOL_REF table
  IPAA_TABLE_DESCRIPTOR _sections;	// IPAA_SECTION table
  IPAA_TABLE_DESCRIPTOR _modrefs;	// IPAA_MODREF table
  IPAA_TABLE_DESCRIPTOR _refbits;	// REFBITS table
  IPAA_TABLE_DESCRIPTOR _sets;		// IPAA_SET table
  IPAA_TABLE_DESCRIPTOR _pu_info;	// IPAA_PU_INFO table

  // Constructor for use in IPAA (from IPAA_SUMMARY::Write):
  IPAA_FILE_DESCRIPTOR ( INT32 major, INT32 minor )
    :	_magic		( IPAA_MAGIC ),
	_major		( major ),
	_minor		( minor ),
	_size		( sizeof(IPAA_FILE_DESCRIPTOR) ),
	_flags		( 0 ),
	_strings	( 1,			0, 0 ),
	_symrefs	( sizeof(IPAA_SYMBOL_REF), 0, 0 ),
	_sections	( sizeof(IPAA_SECTION),	0, 0 ),
	_modrefs	( sizeof(IPAA_MODREF),	0, 0 ),
	_refbits	( sizeof(REFBITS),	0, 0 ),
	_sets		( sizeof(IPAA_SET),	0, 0 ),
	_pu_info	( sizeof(IPAA_PU_INFO), 0, 0 )
    {
    }

  // Trivial destructor:
  ~IPAA_FILE_DESCRIPTOR () {}

 public:

  // Tracing:
  void Print ( FILE *f ) const;
  void Trace ( void ) const;

  // Member attribute access:
  BOOL	Get_addressing	( void ) const
	{ return ( _flags & IPAA_ADDR_ANAL ); }
  void	Set_addressing	( void ) { _flags |= IPAA_ADDR_ANAL; }
  void	Reset_addressing( void ) { _flags &= ~IPAA_ADDR_ANAL; }

};

// Read (mmap) summary file and return pointer to its file descriptor:
IPAA_FILE_DESCRIPTOR * IPAA_Summary_Read ( char *file_name );


// ====================================================================
//
// IPAA_SUMMARY
//
// This class describes the in-memory contents of an IPAA file, while
// it is being constructed in IPAA and while it is being used in the
// back end -- see above.  Each constituent table has a current (i.e.
// actual data) size and a max (i.e. allocated space) size.  All of the
// sizes are in terms of element counts, except the string table, which
// is in bytes since the elements are variable-length.  In IPAA, the
// memory pool must be provided, and will be used for expanding the
// constituent tables.  In WOPT, it must be NULL, and adding elements
// will not work.
//
// ====================================================================

// We want to avoid including this in be.so, so exclude it if we're
// compiling opt_ipaa_io.cxx:
#ifndef OPT_IPAA_IO

class IPAA_SUMMARY
{
  class TABLE {
    friend class IPAA_SUMMARY;
    mINT32 _max;	// Maximum (allocated) size
    mINT32 _size;	// Current (used) size
    void *_p;		// Data vector

    void Print ( FILE *f, char *hdr ) const
      { fprintf ( f, "%-16s -- size %d (max %d)\n", hdr, _size, _max ); }
  };

  mINT8 _major;			// Major revision
  mINT8 _minor;			// Minor revision
  mINT8 _flags;			// Minor revision
  IPAA_FILE_DESCRIPTOR *_desc;	// IPAA_FILE_DESCRIPTOR
  MEM_POOL *_mpool;		// Memory pool to use
  TABLE _hash;			// string hash table (internal)
  TABLE _strings;		// string (name) table
  TABLE _symrefs;		// IPAA_SYMBOL_REF table
  TABLE _sections;		// IPAA_SECTION table
  TABLE _modrefs;		// IPAA_MODREF table
  TABLE _refbits;		// REFBITS table
  TABLE _sets;			// IPAA_SET table
  TABLE _pu_info;		// IPAA_PU_INFO table

  // Initialize a TABLE from an IPAA_TABLE_DESCRIPTOR:
  void Make_table ( TABLE &t, IPAA_TABLE_DESCRIPTOR &tdesc )
  {
    t._p = (void *) ( ((char *)_desc) + tdesc._file_offset );
    t._size = t._max = tdesc._table_size;
  }

  // Expand one of the tables, containing objects of the given size:
  void Expand_table ( TABLE &tab, INT32 size, INT32 count = 1 );

 public:

  // Constructor for use in IPAA -- build a new summary:
  IPAA_SUMMARY ( MEM_POOL *pool )
	{
	  bzero ( this, sizeof (IPAA_SUMMARY) );
	  _major = IPAA_MAJOR;
	  _minor = IPAA_MINOR;
	  IPAA_Summary = this;
	  _mpool = pool;
	  _flags = 0;
	}

  // Constructor for use in WOPT -- read summary file.  This should
  // read (mmap) the summary file, verify that the version matches
  // the current ones, and build this class from the file descriptor.
  IPAA_SUMMARY ( char *file_name );

  // Destructor -- clear the global pointer:
  ~IPAA_SUMMARY () { IPAA_Summary = NULL; }

  // Tracing:
  void Print ( FILE *f, char *msg ) const;
  void Trace ( char *msg ) const;

  // Member access:
  BOOL	Get_addressing	( void ) const
	{ return ( _flags & IPAA_ADDR_ANAL ); }
  void	Set_addressing	( void ) { _flags |= IPAA_ADDR_ANAL; }
  void	Reset_addressing( void ) { _flags &= ~IPAA_ADDR_ANAL; }

  IPAA_FILE_DESCRIPTOR *Get_desc( void ) const	{ return _desc; }
  INT32 Get_string_size	( void ) const { return _strings._size; }
  INT32 Get_symref_size	( void ) const { return _symrefs._size; }
  INT32 Get_section_size( void ) const { return _sections._size; }
  INT32 Get_modref_size	( void ) const { return _modrefs._size; }
  INT32 Get_refbit_size	( void ) const { return _refbits._size; }
  INT32 Get_set_size	( void ) const { return _sets._size; }
  INT32 Get_pu_info_size( void ) const { return _pu_info._size; }

  char * Get_strings		( void ) const
		{ return (char *) _strings._p; }
  IPAA_SYMBOL_REF * Get_symrefs	( void ) const
		{ return (IPAA_SYMBOL_REF *) _symrefs._p; }
  IPAA_SECTION * Get_sections	( void ) const
		{ return (IPAA_SECTION *) _sections._p; }
  IPAA_MODREF * Get_modrefs	( void ) const
		{ return (IPAA_MODREF *) _modrefs._p; }
  REFBITS * Get_refbits		( void ) const
		{ return (REFBITS *) _refbits._p; }
  IPAA_SET * Get_sets		( void ) const
		{ return (IPAA_SET *) _sets._p; }
  IPAA_PU_INFO * Get_pu_info	( void ) const
		{ return (IPAA_PU_INFO *) _pu_info._p; }

  char * Get_string		( INT32 i ) const
		{ return ((char *)_strings._p) + i; }
  IPAA_SYMBOL_REF * Get_symref	( SYMREF_IX i ) const
		{ return ((IPAA_SYMBOL_REF *)_symrefs._p) + i; }
  IPAA_SECTION * Get_section	( SECTION_IX i ) const
		{ return ((IPAA_SECTION *)_sections._p) + i; }
  IPAA_MODREF * Get_modref	( MODREF_IX i ) const
		{ return ((IPAA_MODREF *) _modrefs._p) + i; }
  REFBITS Get_refbits		( INT32 i ) const
		{ return ((REFBITS *) _refbits._p)[i]; }
  IPAA_SET * Get_set		( SET_IX i ) const
		{ return ((IPAA_SET *)_sets._p) + i; }
  IPAA_PU_INFO * Get_pu_info	( PU_INFO_IX i ) const
		{ return ((IPAA_PU_INFO *)_pu_info._p) + i; }

  // Add objects to the tables (only valid if _mpool != NULL):
  INT32 Add_string	( const char * );
  SYMREF_IX Add_symref	( const IPAA_SYMBOL_REF & );
  SYMREF_IX Add_symref	( INT32 cnt, const IPAA_SYMBOL_REF * );
  SECTION_IX Add_section (const IPAA_SECTION & );
  SECTION_IX Add_section (INT32 cnt, const IPAA_SECTION * );
  MODREF_IX Add_modref	( const IPAA_MODREF & );
  MODREF_IX Add_modref	( INT32 cnt, const IPAA_MODREF * );
  INT32 Add_refbits	( const REFBITS & );
  INT32 Add_refbits	( INT32 cnt, const REFBITS * );
  SET_IX Add_set	( const IPAA_SET & );
  SET_IX Add_set	( INT32 cnt, const IPAA_SET * );
  PU_INFO_IX Add_pu_info (const IPAA_PU_INFO & );
  PU_INFO_IX Add_pu_info (INT32 cnt, const IPAA_PU_INFO * );

  // Build an IPAA_FILE_DESCRIPTOR, and write the summary file:
  void Write ( const char *file_name );
};

// ====================================================================
// ====================================================================
//
// The following are inlinable methods from the classes above which
// depend on the global IPAA_Summary variable.
//
// ====================================================================
// ====================================================================

char *
IPAA_SYMBOL_REF::Name ( void ) const
{
  return IPAA_Summary->Get_string ( Name_id() );
}

// ====================================================================

char *
IPAA_SYMBOL_REF::Global_name ( void ) const
{
  return IPAA_Summary->Get_string ( Global_id() );
}

// ====================================================================

char *
IPAA_SYMBOL_REF::Local_file_name ( void ) const
{
  return IPAA_Summary->Get_string ( Local_file_id() );
}

// ====================================================================

INT64
IPAA_SYMBOL_REF::Section_disp ( void ) const
{
  return IPAA_Summary -> Get_section ( Section_id() ) -> Disp();
}

// ====================================================================

INT64
IPAA_SYMBOL_REF::Section_size ( void ) const
{
  return IPAA_Summary -> Get_section ( Section_id() ) -> Size();
}

// ====================================================================

char *
IPAA_PU_INFO::Name ( void ) const
{
  SYMREF_IX six = Pu_ix(); 
  if ( six == 0 ) return NULL;
  IPAA_SYMBOL_REF *symref = IPAA_Summary -> Get_symref ( six );
  if ( symref->Kind() == SREF_GLOBAL ) return symref->Global_name();
  return NULL;
}
#endif /* not OPT_IPAA_IO */

// ====================================================================
// ====================================================================
//
// The following information is passed in the individual WHIRL files,
// in the .WHIRL.ipaa section.
//
// ====================================================================
// ====================================================================
//
// IPAA_CALLSITE
//
// This class describes a callsite; its purpose is to map a
// callsite in WHIRL to the callee's information in the IPAA summary
// file.  The mechanism for doing so is a map consisting of a pair
// < callsite_map_id, PU_INFO_INDEX >.  The first element is the
// ID of a mapping created in IPL for the callsites; the second is the
// index of the callee's IPAA_PU_INFO record in the IPAA summary file.
//
// IPAA_CALLSITES
//
// This class describes a the callsite mapping, i.e. the set of
// IPAA_CALLSITE records.  They should be in _map_id order to allow
// fast binary search lookup in WOPT.
//
// There is one such mapping per PU, with the set of them in their own
// PU-Info subsection in the .B file.
//
// ====================================================================
// ====================================================================

class IPAA_CALLSITES;

class IPAA_CALLSITE
{
  friend class IPAA_CALLSITES;

  mINT32 _map_id;	// The mapping ID of the callsite
  PU_INFO_IX _pu_idx;	// The IPAA_PU_INFO index of the callee

 public:

  // Constructor:
  IPAA_CALLSITE ( INT32 id, INT32 pu )	{ _map_id = id; _pu_idx = pu; }

  // Destructor:
  ~IPAA_CALLSITE ( void ) {}
};


extern "C" {
  // Read table from .B file
  void *IPAA_CALLSITES_Read ( char *base, UINT32 size );

  // Write the table to the .B file
  void IPAA_CALLSITES_Write ( void *callsites, struct output_file *fl );
}


class IPAA_CALLSITES
{
  PU_INFO_IX _caller;		// IPAA_PU_INFO index of the caller
  mINT32 _max;			// Allocated number of IPAA_CALLSITE pairs
  mINT32 _size;			// Actual number of IPAA_CALLSITE pairs
				// (includes invalid dummy at index 0)
  IPAA_CALLSITE *_callsites;	// IPAA_CALLSITE vector
  MEM_POOL *_mpool;		// Memory pool to use for _pu_idx

  friend void *IPAA_CALLSITES_Read(char *base, UINT32 size);
  friend void IPAA_CALLSITES_Write(void *callsites, struct output_file *fl);

  // Access to callsite vector:
  INT32 Get_map_id ( INT32 ix ) const
	{ return _callsites[ix]._map_id; }
  PU_INFO_IX Get_pu_idx ( INT32 ix ) const
	{ return _callsites[ix]._pu_idx; }

  // Expand the callsite vector:
  void Expand_vector ( INT32 count = 2 );

 public:

  // Constructor for IPAA use -- parameter is initial size:
  IPAA_CALLSITES ( PU_INFO_IX caller, INT32 size, MEM_POOL *mpool );

  // Destructor -- de-allocate _pu_idx:
  ~IPAA_CALLSITES ( void );

  // Tracing:
  void Print ( FILE *f, char *msg ) const;
  void Trace ( char *msg ) const;

  // Given a map ID, where is the callee's PU information?
  PU_INFO_IX Map_to_PU_id ( INT32 map_id );
  IPAA_PU_INFO *Map_to_PU ( INT32 map_id );

  // Add a new callsite mapping to the list, resizing if necessary:
  INT32 Add_callsite ( INT32 map_id, PU_INFO_IX pu_idx );
};

// ====================================================================
// ====================================================================
//
// IPAA_LOCAL_MAP
//
// This class describes the mapping from the per-file local IDs used in
// IPAA summary file to the symbol table IDs used in the WHIRL file.
// There will be one such mapping written per .B file, as its own
// ELF section.
//
// The expected usage of this mapping involves being given a symbol,
// and determining whether a reference exists.  This requires
// efficiently looking up the local ID given the SYMTAB_id and
// ST_id for the symbol, and then efficiently checking whether that
// symbol is referenced.  We expect to do this as follows:
//
// Before using this table in the back end, we will build a mapping
// indexed by ST_id for each SYMTAB we care about.  This will allow
// rapid lookup from symbol to local ID, with no IPA arrangement
// (other than breaking these into files, which is required anyway),
// and with only a simple single-pass initialization in the back end.
//
// We can determine whether a particular symbol is referenced as
// follows, if we typically process a callsite completely and then move
// on.  We can do a pass over the reference sets for the callsite,
// noting the reference information in the relevant mapping structures,
// so that it is immediately referenceable.  It must then be removed
// before processing the next callsite.
//
// ====================================================================
// ====================================================================

class IPAA_LOCAL_MAP
{
  INT32 _max;		// Number of elements allocated in the maps
  INT32 _size;		// Number of elements used in the maps
  mINT16 *_symtab_id;	// The local-id -> SYMTAB_id value map
  mINT32 *_st_id;	// The local-id -> ST_id value map
  MEM_POOL *_mpool;	// Where to put _symtab_id and _st_id?

  friend void *IPAA_LOCAL_MAP_Read(char *base, UINT32 size);
  friend void IPAA_LOCAL_MAP_Write(void *localmap, struct output_file *fl);

 public:

  // Constructor for IPAA use -- parameter is initial size:
  IPAA_LOCAL_MAP ( INT32 size, MEM_POOL *mpool );

  // Destructor -- de-allocate _st_id:
  ~IPAA_LOCAL_MAP ( void );

  // Tracing:
  void Print ( FILE *f ) const;
  void Trace ( void ) const;

  // Add an ST to the map for the given local index:
  void Map_local_to_st ( INT32 local, struct st *st );

  // Given a local ID, return its identifiers:
  void Local_to_ids ( INT32 local, INT16 *symtab_id, INT32 *st_id )
	{ *symtab_id = _symtab_id[local]; *st_id = _st_id[local]; }
};

#endif
