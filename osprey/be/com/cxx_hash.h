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


//-*-c++-*-
// ====================================================================
// ====================================================================
//
// Module: cxx_hash.h
// $Revision: 1.2 $
// $Date: 02/11/07 23:41:35-00:00 $
// $Author: fchow@keyresearch.com $
// $Source: /scratch/mee/2.4-65/kpro64-pending/be/com/SCCS/s.cxx_hash.h $
//
// Revision history:
//  07-Dec-95 - Merged USER_HASH_TABLE support.
//
// Description:
//
// This file contains templates for open hash tables.
//
// ====================================================================
//
// (template...) class HASH_TABLE
//
// The first implementation maps SIG_TYPE keys into DATA_TYPE *data.
// A typical instantiation would be INT for SIG_TYPE and void * for
// DATA_TYPE.  This implementation has the following key attributes:
//
//  1)	The size of the hash table is determined at constructor time.
//
//  2)	The hash table elements are lists of objects.
//
//  3)	The objects in the table's lists are pairs consisting of a
//	signature (key) and a data element to which the key is mapped.
//
//  4)	The hash function is a built in function of the signature.
//	Therefore, for example, it will be the pointer value for
//	a string key, and will produce different entries for distinct
//	pointers to the same character string.
//
// template <class SIG_TYPE, class DATA_TYPE> class HASH_TABLE
// 
//		An open hash table.  This table maps SIG_TYPE signatures
//		into DATA_TYPE data.
//
//   	    HASH_TABLE(const UINT num_elements, MEM_POOL *pool)
//
//		Create a new hash table with num_elements elements.
//		Store the table in pool.
//
//	    void Enter(SIG_TYPE signature, DATA_TYPE data)
//
//		Enter into the table the pair (signature, data)
//		This routine does not check for duplicates.
//
//	    void Enter_If_Unique(SIG_TYPE signature, DATA_TYPE data)
//
//		Enter the pair into the table if signature is not
//		already in the table
//
//	    DATA_TYPE Find(SIG_TYPE signature) const
//
//		Return the data if it is in the table, NULL otherwise.
//
//          void Find_And_Set(SIG_TYPE signature, DATA_TYPE data)
//              If the element is in the table, reset its data. 
//              Otherwise, enter into the table.
//
//	     void Remove(SIG_TYPE signature)
//
//		If it's in the table, remove it
//
//      UINT Num_Entries() const
//          Return the number of entries in the table.
//          This is different from Num_Elements.
//
//	    ~HASH_TABLE() 
//
//  template <class SIG_TYPE, class DATA_TYPE> HASH_TABLE_ITER
//
//	    HASH_TABLE_ITER(const HASH_TABLE*)
//	    BOOL Step(SIG_TYPE*, DATA_TYPE*)
//
//		Usage:
//		    HASH_TABLE_ITER it(hashtable);
//		    while (it.Step(&sig, &data))
//			...
//
// ====================================================================
//
// (template...) class USER_HASH_TABLE
//
// The second implementation maps KEY_TYPE keys into DATA_TYPE *data.
// It is very similar to HASH_TABLE, but allows the user to supply a
// hash function and a key equality function.
//
// A typical instantiation would be char * for KEY_TYPE and void * for
// DATA_TYPE.  This implementation has the following key attributes:
//
//  1)	The size of the hash table is determined at constructor time.
//
//  2)	The hash table elements are lists of objects.
//
//  3)	The objects in the table's lists are pairs consisting of a
//	signature (key) and a data element to which the key is mapped.
//
//  4)	The hash function is provided by the user as a function object
//	HASH_FUNC on the KEY_TYPE, and equivalence between keys is
//	also provided by a user function object KEY_EQ.  We supply
//	reasonable hash and equality functions for character strings
//	below.
//
// typedef ... HASH;
//	This type must be the result type of the user hash function.
//
// template < class KEY_TYPE, class DATA_TYPE,
//	      class HASH_FUNC, class KEY_EQ >
// class USER_HASH_TABLE
// 
//		An open hash table.  This table maps KEY_TYPE
//		keys into DATA_TYPE data.  HASH_FUNC must map
//		KEY_TYPE objects into an HASH hash value, and KEY_EQ
//		must compare two KEY_TYPE objects for equality and
//		return a BOOL result.
//
//   	    USER_HASH_TABLE ( const UINT32 num_elements, MEM_POOL *pool )
//
//		Create a new hash table with num_elements elements.
//		Store the table in pool.  It may later be resized.
//
//	    void Enter ( KEY_TYPE key, DATA_TYPE data )
//
//		Enter into the table the pair (key, data)
//		This routine does not check for duplicates.
//
//	    void Enter_If_Unique ( KEY_TYPE key, DATA_TYPE data )
//
//		Enter the pair into the table if key is not
//		already in the table
//
//	    DATA_TYPE Find ( KEY_TYPE key ) const
//
//		Return the data if it is in the table, NULL otherwise.
//
//      UINT Num_Entries() const
//          Return the number of entries in the table.
//          This is different from Num_Elements.
//
//	    ~HASH_TABLE() 
//
// template < class KEY_TYPE, class DATA_TYPE,
//	      class HASH_FUNC, class KEY_EQ >
// USER_HASH_TABLE_ITER
//
//	    USER_HASH_TABLE_ITER ( const USER_HASH_TABLE* )
//	    BOOL Step ( KEY_TYPE*, DATA_TYPE* )
//
//		Usage:
//		    USER_HASH_TABLE_ITER it(hashtable);
//		    while (it.Step(&key, &data))
//			...
//
// ====================================================================
//
// HASH String_Hash ( const char *k );
//	This function object may be used as the HASH_FUNC parameter
//	for USER_HASH_TABLE instances with string keys.
//
// BOOL String_Compare ( const char *s, const char *t );
//	This function object may be used as the KEY_EQ parameter for
//	USER_HASH_TABLE instances with string keys.
//
// ====================================================================
//
// NOTE:  The bodies for the definitions in this file are split between
// cxx_hash.cxx (template bodies) and cxx_hash_util.cxx (non-template
// bodies).
//
// ====================================================================
// ====================================================================

#ifndef cxx_hash_INCLUDED
#define cxx_hash_INCLUDED "cxx_hash.h"

#ifdef _KEEP_RCS_ID
static char *cxx_hash_rcs_id  = cxx_hash_INCLUDED "$Revision: 1.2 $";
#endif /* _KEEP_RCS_ID */

#ifndef CXX_MEMORY_INCLUDED
#include "cxx_memory.h"
#endif
#include "erglob.h"

// ====================================================================
// ====================================================================
//
// HASH_ELEMENT
//
// For both hash map implementations, the main hash table contains
// lists of these objects.
//
// This is public, but no one should touch this except HASH_TABLE,
// HASH_TABLE_ITER, USER_HASH_TABLE, and USER_HASH_TABLE_ITER.
// I would use friend functions, but they don't seem to work with
// templates.
//
// ====================================================================
// ====================================================================

template <class SIG_TYPE, class DATA_TYPE>
class HASH_ELEMENT
{
public:
  DATA_TYPE     _data;
  SIG_TYPE	_signature;
  HASH_ELEMENT  *_next;

  // create a new element
  HASH_ELEMENT(const SIG_TYPE& signature, const DATA_TYPE& data) :
    _signature(signature), _data(data), _next(NULL) {}
  void Add_To_List(HASH_ELEMENT *e) { e->_next = _next; _next = e; }
};

// ====================================================================
// ====================================================================
//
// HASH_TABLE
//
// This is a basic hash map, with a built-in hash function.
//
// ====================================================================
// ====================================================================

template <class SIG_TYPE, class DATA_TYPE>
class HASH_TABLE {
private:
  MEM_POOL *_pool;
  HASH_ELEMENT<SIG_TYPE,DATA_TYPE> *  *_data;  // an array of HASH_ELEMENT *
  UINT _num_elements;
  UINT _num_entries;
public:
  HASH_TABLE<SIG_TYPE,DATA_TYPE>(UINT num_elements, MEM_POOL *pool);
  UINT Num_Elements() const { return _num_elements; };
  UINT Num_Entries() const { return _num_entries; };
  HASH_ELEMENT<SIG_TYPE,DATA_TYPE> *Data(UINT i) const { return _data[i]; };

  void Enter(SIG_TYPE signature,DATA_TYPE data) {
    typedef HASH_ELEMENT<SIG_TYPE,DATA_TYPE> THIS_HASH_ELEMENT;
    typedef HASH_ELEMENT<SIG_TYPE,DATA_TYPE> *HASH_ELEMENTP;
    HASH_ELEMENTP element = 
      CXX_NEW(THIS_HASH_ELEMENT(signature,data),_pool);
    UINT location = abs((INT)(INTPS)signature) % _num_elements;
    if (_data[location]) { // some thing is there
      _data[location]->Add_To_List(element);
    } else {
      _data[location] = element;
    }
    _num_entries++;
  }

  void Enter_If_Unique(SIG_TYPE signature,DATA_TYPE data); 
  DATA_TYPE Find(SIG_TYPE signature) const;
  void Find_And_Set(SIG_TYPE signature,DATA_TYPE data); 
  void Remove(SIG_TYPE signature); 

  // Destructor -- remove all entries and the pointer array:
  ~HASH_TABLE() {  
    typedef HASH_ELEMENT<SIG_TYPE,DATA_TYPE> *HASH_ELEMENTP;
    for (UINT loc = 0; loc < _num_elements; loc++) {
      HASH_ELEMENTP element = _data[loc];
      HASH_ELEMENTP next_element;
      while (element) {
        next_element = element->_next;
        CXX_DELETE(element, _pool);
        element = next_element;
      }
    }
    CXX_DELETE_ARRAY(_data, _pool); 
  };

};

template <class SIG_TYPE, class DATA_TYPE>
class HASH_TABLE_ITER {
 private:
  INT					_loc;
  HASH_ELEMENT<SIG_TYPE,DATA_TYPE> 	*_he;
  const HASH_TABLE<SIG_TYPE,DATA_TYPE>	*_hash;
 public:
  BOOL			Step(SIG_TYPE* sig, DATA_TYPE* data);	// TRUE of ok
  HASH_TABLE_ITER(const HASH_TABLE<SIG_TYPE,DATA_TYPE> * h) : 
			_hash(h), _loc(-1), _he(NULL) {}
  ~HASH_TABLE_ITER() {}
};

// ====================================================================
// ====================================================================
//
// Support for USER_HASH_TABLE
//
// Define the required result type of the hash function, and provide
// basic string hash and equality function objects.
//
// ====================================================================
// ====================================================================

// Hash value typedef:
typedef UINT32 HASH;

// String hash function:
struct String_Hash
{
  HASH operator() ( const char * k ) const;
};

// String equality function:
struct String_Equal
{
  BOOL operator() ( const char *a, const char *b ) const
	{ return ! strcmp ( a, b ); }
};

// ====================================================================
// ====================================================================
//
// USER_HASH_TABLE
//
// This is a hash map, very similar to HASH_TABLE, but allowing the
// user to specify the hash function and key equality as function
// objects.
//
// ====================================================================
// ====================================================================

// The template parameter list is long and unwieldy:
#define TEMPLATE_USER_HASH_TABLE template \
    < class KEY_TYPE, class DATA_TYPE, class HASH_FUNC, class KEY_EQ >
#define CLASS_USER_HASH_TABLE \
    USER_HASH_TABLE < KEY_TYPE, DATA_TYPE, HASH_FUNC, KEY_EQ >

TEMPLATE_USER_HASH_TABLE
class USER_HASH_TABLE {
 private:
  MEM_POOL *_pool;
  HASH_ELEMENT<KEY_TYPE,DATA_TYPE> *  *_data;  // array of HASH_ELEMENT*
  UINT32 _num_elements;
  UINT _num_entries;
  HASH_FUNC _hash;
  KEY_EQ _equal;

 public:
  // Constructor -- Build a map in 'pool' with 'num_elements' slots:
  USER_HASH_TABLE ( UINT32 num_elements, MEM_POOL *pool );

  // Tracing -- only valid if KEY_TYPE is char*:
  void Print ( FILE *f );

  // Field access:
  UINT32 Num_Elements ( void ) const { return _num_elements; };
  UINT Num_Entries( void ) const { return _num_entries; };
  HASH_ELEMENT < KEY_TYPE, DATA_TYPE > *Data ( UINT32 i) const
	{ return _data[i]; }

  // Enter a new element without checking for duplication:
  void Enter ( KEY_TYPE key, DATA_TYPE data ) {
    typedef HASH_ELEMENT<KEY_TYPE,DATA_TYPE> THIS_HASH_ELEMENT;
    typedef HASH_ELEMENT<KEY_TYPE,DATA_TYPE> *HASH_ELEMENTP;
    HASH_ELEMENTP element = 
      CXX_NEW ( THIS_HASH_ELEMENT(key,data), _pool );
    UINT32 location = _hash(key) % _num_elements;

    element->_next = _data[location];
    _data[location] = element;
    _num_entries++;
  }

  // Enter a new element only if the given key is not already mapped:
  void Enter_If_Unique ( KEY_TYPE key, DATA_TYPE data ); 

  // Find the given key in the map:
  DATA_TYPE Find(KEY_TYPE key) const;

  // Destructor -- remove all entries and the pointer array:
  ~USER_HASH_TABLE() {  
    typedef HASH_ELEMENT<KEY_TYPE,DATA_TYPE> *HASH_ELEMENTP;
    for (UINT loc = 0; loc < _num_elements; loc++) {
      HASH_ELEMENTP element = _data[loc];
      HASH_ELEMENTP next_element;
      while (element) {
        next_element = element->_next;
        CXX_DELETE(element, _pool);
        element = next_element;
      }
    }
    CXX_DELETE_ARRAY(_data, _pool); 
  };
};

TEMPLATE_USER_HASH_TABLE
class USER_HASH_TABLE_ITER {
 private:
  INT					_loc;
  HASH_ELEMENT<KEY_TYPE,DATA_TYPE> 	*_he;
  const USER_HASH_TABLE < KEY_TYPE, DATA_TYPE, HASH_FUNC, KEY_EQ > *_hash;

 public:
  BOOL	Step ( KEY_TYPE* key, DATA_TYPE* data );	// TRUE of ok
  USER_HASH_TABLE_ITER (
    const USER_HASH_TABLE < KEY_TYPE, DATA_TYPE, HASH_FUNC, KEY_EQ > * h ) : 
    _hash(h), _loc(-1), _he(NULL) {}
  ~USER_HASH_TABLE_ITER ( void ) {}
};

// Implementation stuff follows. This was taken from cxx_hash.cxx,
// since g++ (rightly) doesn't do the "implicit .cxx file inclusion"
// thing.

// ====================================================================
// ====================================================================
//
// HASH_TABLE
//
// This is a simple hash map, with the following attributes:
//
//  1)	The size of the hash table is determined at constructor time.
//
//  2)	The hash table elements are lists of objects.
//
//  3)	The objects in the table's lists are pairs consisting of a
//	signature (key) and a data element to which the key is mapped.
//
//  4)	The hash function is built in as the signature modulo the table
//	size.  Therefore, for example, it will be the pointer value for
//	a string key, and will produce different entries for distinct
//	pointers to the same character string.
//
// ====================================================================
// ====================================================================

template <class SIG_TYPE, class DATA_TYPE>
HASH_TABLE<SIG_TYPE,DATA_TYPE> :: HASH_TABLE (
  UINT num_elements,
  MEM_POOL *pool )
{
  typedef HASH_ELEMENT<SIG_TYPE,DATA_TYPE> *HASH_ELEMENTP;
  _pool = pool;
  _num_elements = num_elements;
  _num_entries = 0;
  _data = CXX_NEW_ARRAY(HASH_ELEMENTP ,num_elements,pool);
  for (INT i=0; i<num_elements; i++) {
    _data[i] = (HASH_ELEMENTP) 0;
  }
}

template <class SIG_TYPE, class DATA_TYPE>
void
HASH_TABLE<SIG_TYPE,DATA_TYPE> :: Enter_If_Unique(SIG_TYPE signature, 
						  DATA_TYPE data)
{ 
  typedef HASH_ELEMENT<SIG_TYPE,DATA_TYPE> THIS_HASH_ELEMENT;
  typedef HASH_ELEMENT<SIG_TYPE,DATA_TYPE> *HASH_ELEMENTP;
  HASH_ELEMENTP element = 
    CXX_NEW(THIS_HASH_ELEMENT(signature,data),_pool);
  UINT location = abs((INT)(INTPS)signature) % _num_elements;

  if (_data[location]) { // something is there
    HASH_ELEMENTP iter = _data[location];
    for (; iter != NULL ; iter = iter->_next) {
      if (iter->_signature == signature) {
         return; // not unique
      }
    }
    _data[location]->Add_To_List(element);
  } else {
    _data[location] = element;
  }
  _num_entries++;
}

template <class SIG_TYPE, class DATA_TYPE>
DATA_TYPE
HASH_TABLE<SIG_TYPE,DATA_TYPE> :: Find (
  SIG_TYPE signature ) const
{
  typedef HASH_ELEMENT<SIG_TYPE,DATA_TYPE> *HASH_ELEMENTP;
  HASH_ELEMENTP hash_element = _data[abs((INT)(INTPS)signature) % _num_elements];

  for (; hash_element != NULL; hash_element = hash_element->_next) {
    if (hash_element->_signature == signature) {
      return(hash_element->_data);
    }
  }
  return((DATA_TYPE)0);
}

template <class SIG_TYPE, class DATA_TYPE>
void
HASH_TABLE<SIG_TYPE,DATA_TYPE> :: Find_And_Set (
  SIG_TYPE signature, DATA_TYPE data ) 
{
  typedef HASH_ELEMENT<SIG_TYPE,DATA_TYPE> *HASH_ELEMENTP;
  HASH_ELEMENTP hash_element = _data[abs((INT)(INTPS)signature) % _num_elements];

  for (; hash_element != NULL; hash_element = hash_element->_next) {
    if (hash_element->_signature == signature) {
        hash_element->_data = data;
        return;
    }
  }
  Enter(signature, data);
}

template <class SIG_TYPE, class DATA_TYPE>
void HASH_TABLE<SIG_TYPE,DATA_TYPE> :: Remove (
  SIG_TYPE signature ) 
{
  typedef HASH_ELEMENT<SIG_TYPE,DATA_TYPE> *HASH_ELEMENTP;
  HASH_ELEMENTP hash_element = _data[abs((INT)(INTPTR)signature) % _num_elements];

  if (hash_element->_signature == signature) {
    _data[abs((INT)(INTPS)signature) % _num_elements] = hash_element->_next;
    CXX_DELETE(hash_element,_pool);
    _num_entries--;
    return;
  }

  HASH_ELEMENTP prev = hash_element;
  for (hash_element = hash_element->_next; hash_element; 
				hash_element = hash_element->_next) {
    if (hash_element->_signature == signature) {
      prev->_next = hash_element->_next;
      CXX_DELETE(hash_element,_pool);
      _num_entries--;
      return;
    }
    prev = hash_element;
  }
}

template <class SIG_TYPE, class DATA_TYPE>
BOOL
HASH_TABLE_ITER<SIG_TYPE,DATA_TYPE> :: Step (
  SIG_TYPE* sig,
  DATA_TYPE* data )
{
  if (_he && _he->_next) {
    _he = _he->_next;
    *sig = _he->_signature;
    *data = _he->_data;
    return TRUE;
  }
    
  for (_loc++; _loc < _hash->Num_Elements(); _loc++) {
    if (_hash->Data(_loc)) {
      _he = _hash->Data(_loc);
      *sig = _he->_signature;
      *data = _he->_data;
      return TRUE;
    }
  }

  return FALSE;
}

// ====================================================================
// ====================================================================
//
// USER_HASH_TABLE
//
// This is a hash map, very similar to HASH_TABLE, with the following
// attributes (only #4 differs from HASH_TABLE):
//
//  1)	The size of the hash table is determined at constructor time.
//
//  2)	The hash table elements are lists of objects.
//
//  3)	The objects in the table's lists are pairs consisting of a
//	signature (key) and a data element to which the key is mapped.
//
//  4)	The hash function is provided by the user as a function object
//	HASH_FUNC on the KEY_TYPE, and equivalence between keys is
//	also provided by a user function object KEY_EQ.
//
// ====================================================================
// ====================================================================

TEMPLATE_USER_HASH_TABLE
CLASS_USER_HASH_TABLE :: USER_HASH_TABLE
  ( UINT32 num_elements, MEM_POOL *pool )
{
  typedef HASH_ELEMENT < KEY_TYPE, DATA_TYPE > *pHASH_ELEMENT;

  _pool = pool;
  _num_elements = num_elements;
  _num_entries = 0;
  _data = CXX_NEW_ARRAY ( pHASH_ELEMENT, num_elements, pool );
  if ( _data == NULL ) {
    ErrMsg ( EC_No_Mem, "USER_HASH_TABLE::USER_HASH_TABLE" );
  }
  for ( INT i=0; i<num_elements; i++ ) {
    _data[i] = (pHASH_ELEMENT) NULL;
  }
}

TEMPLATE_USER_HASH_TABLE
void
CLASS_USER_HASH_TABLE :: Print ( FILE *f )
{
  HASH_ELEMENT < KEY_TYPE, DATA_TYPE > *elt;

  for ( INT32 i = 0; i < _num_elements; i++ ) {
    if ( _data[i] != NULL ) {
      fprintf ( f, "%2d:", i );
      elt = _data[i];
      while  ( elt != NULL ) {
	fprintf ( f, " k%2d:%s:d%d:n0x%06lx",
		  _hash(elt->_signature) % _num_elements,
		  elt->_signature, elt->_data, elt->_next );
	elt = elt->_next;
      }
      fprintf ( f, "\n" );
    }
  }
}

TEMPLATE_USER_HASH_TABLE
void
CLASS_USER_HASH_TABLE :: Enter_If_Unique(KEY_TYPE key, DATA_TYPE data)
{ 
  typedef HASH_ELEMENT<KEY_TYPE,DATA_TYPE> THIS_HASH_ELEMENT;
  typedef HASH_ELEMENT<KEY_TYPE,DATA_TYPE> *pHASH_ELEMENT;
  pHASH_ELEMENT element = 
	CXX_NEW ( THIS_HASH_ELEMENT(key,data), _pool );
  UINT32 location = _hash ( key ) % _num_elements;

  if ( element == NULL ) {
    ErrMsg ( EC_No_Mem, "USER_HASH_TABLE::Enter_If_Unique" );
  }

  if ( _data[location] ) { // something is there
    pHASH_ELEMENT iter = _data[location];

    for ( ; iter != NULL ; iter = iter->_next ) {
      if ( _equal ( iter->_signature, key ) ) {
        return; // not unique
      }
    }
    element->_next = _data[location];
  }
  _data[location] = element;
  _num_entries++;
}

TEMPLATE_USER_HASH_TABLE
DATA_TYPE
CLASS_USER_HASH_TABLE :: Find ( KEY_TYPE key ) const
{
  typedef HASH_ELEMENT < KEY_TYPE, DATA_TYPE > *pHASH_ELEMENT;
  HASH hash = _hash(key) % _num_elements;
  pHASH_ELEMENT hash_element = _data[hash];

  // fprintf ( TFile, "Find: 0x%08lx %2d 0x%08lx %s\n",
  //	      key, hash, hash_element, key );
  // fflush ( TFile );

  for ( ; hash_element != NULL; hash_element = hash_element->_next ) {
    if ( _equal ( hash_element->_signature, key ) ) {
      return ( hash_element->_data );
    }
  }

  return((DATA_TYPE)0);
}


TEMPLATE_USER_HASH_TABLE
BOOL
USER_HASH_TABLE_ITER < KEY_TYPE, DATA_TYPE, HASH_FUNC, KEY_EQ > :: Step
  ( KEY_TYPE* key, DATA_TYPE* data )
{
  if ( _he && _he->_next ) {
    _he = _he->_next;
    *key = _he->_signature;
    *data = _he->_data;
    return TRUE;
  }
    
  for ( _loc++; _loc < _hash->Num_Elements(); _loc++ ) {
    if ( _hash->Data(_loc) ) {
      _he = _hash->Data(_loc);
      *key = _he->_signature;
      *data = _he->_data;
      return TRUE;
    }
  }

  return FALSE;
}
#endif /* cxx_hash_INCLUDED */

