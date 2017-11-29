/*
 * Copyright (C) 2009 Advanced Micro Devices, Inc.  All Rights Reserved.
 */

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


//-*-c++-*-
// ====================================================================
// ====================================================================
//
// Module: fb_freq.h
// $Revision: 1.8 $
// $Date: 05/12/05 08:59:36-08:00 $
// $Author: bos@eng-24.pathscale.com $
// $Source: /scratch/mee/2.4-65/kpro64-pending/common/com/SCCS/s.fb_freq.h $
//
// Description:
//
// During instrumentation, certain events associated with program
// control flow (such as the number of times a particular branch is
// taken, or not taken) are counted.  During subsequent compilations,
// this data may be retrieved and used to guide optimization decisions.
//
// Frequencies are stored as float in order to permit scaling.
//
// A frequency value is stored as a FB_FREQ data type, which can have
// one of the following values:
// - an EXACT nonnegative float value
// - a  GUESS nonnegative float value
// - UNKNOWN - an unknown frequency; its value may be computable from
//             other frequency data
// - UNINIT  - an uninitialized frequency
// - ERROR   - the result of dividing by zero, or subtracting a larger
//             from a smaller frequency value.
//
// EXACT values give an exactly correct count of the number of times
// that a particular edge was traveled during the program's
// instrumentated execution.  GUESS values are educated estimates of
// the count.  Whenever a piece of code is cloned (eg: IPA; also the
// test condition of a WHILE_DO loop is cloned during lowering), the
// code's frequencies are scaled, and EXACT values are converted to
// GUESSes.
//
// Sometimes dead branch elimination and frequency propagation will
// enable GUESSes to be replaced later by EXACT counts, and UNKNOWN
// values to be replaced by EXACT and GUESS values.
//
// Normally, ERROR and UNINIT frequencies should only be encountered
// locally.
//
// Operations ( +, - , * , /, +=, -=, *=, /=, ==, !=, <, >, <=, >= )
// are provided to combine, compare, and display frequencies.  * is
// assumed to be scaling, so EXACT values are replaced by GUESSes.
// Comparison operations do not distinguish between GUESS and EXACT
// values.
//
// ====================================================================
// ====================================================================

#ifndef fb_freq_INCLUDED
#define fb_freq_INCLUDED

#include <math.h>
#include "defs.h"
#ifndef ERRORS_INCLUDED
#include "errors.h"
#endif

// ====================================================================
// FB_FREQ_TYPE - frequency types
// ====================================================================

// The FB_FREQ_TYPE integer values are chosen to satisfy two rules:
// (1) When two FB_FREQs are combined, the resulting FB_FREQ has normally
//     FB_FREQ_TYPE equal to the minimum of those of the two FB_FREQ
//     addends; and
// (2) EXACTness and KNOWNness can be ascertained by comparing the
//     FB_FREQ_TYPE to zero.

enum FB_FREQ_TYPE {
  FB_FREQ_TYPE_EXACT   =  1,
  FB_FREQ_TYPE_GUESS   =  0,
  FB_FREQ_TYPE_UNKNOWN = -1,
  FB_FREQ_TYPE_UNINIT  = -2,
  FB_FREQ_TYPE_ERROR   = -3
};

#define FB_FREQ_TYPE_COMBINE( type1, type2 ) \
        ( (type1) > (type2) ? (type2) : (type1) )
#define FB_FREQ_TYPE_BETTER( type1, type2 ) ( (type1) > (type2) )

#define FB_FREQ_TYPE_IS_EXACT( type )        ( (type) >  0 )
#define FB_FREQ_TYPE_IS_GUESS( type )        ( (type) == 0 )
#define FB_FREQ_TYPE_IS_KNOWN( type )        ( (type) >= 0 )
#define FB_FREQ_TYPE_NOT_KNOWN( type )       ( (type) <  0 )
#define FB_FREQ_TYPE_INITIALIZED( type )     ( (type) >= FB_FREQ_TYPE_UNKNOWN )

#define FB_FREQ_TYPE_IS_VALID( type ) \
        ( (type) >= FB_FREQ_TYPE_ERROR && (type) <= FB_FREQ_TYPE_EXACT )

// ====================================================================
// FB_FREQ - frequency values
// ====================================================================

// To account for round-off error, x == y  iff  x > y (1 - epsilon)
//                                         and  y > x (1 - epsilon)

#if defined(TARG_SL) && defined(__SL__) 
#define  FREQ_OFFSET 3
#define  FREQ_SET_OFFSET(value)    (value + FREQ_OFFSET)
#define  FREQ_RESET_OFFSET(value)  (value - FREQ_OFFSET)
typedef UINT32 FREQ_VALUE_TYPE;
#else
const float FB_FREQ_EPSILON = 0.0001;
typedef float FREQ_VALUE_TYPE;
#endif

class FB_FREQ {
public:

  FB_FREQ_TYPE  _type;
  
  FREQ_VALUE_TYPE    _value;

  // In order to simplify FB_FREQ operations, two internal restrictions
  // are imposed:
  // (1) If _type >= 0, then _value >= 0.0; and
  // (2) If _type <  0, then _value == (float) _type.

  // Private constructor, for binary operations
  
#if defined(TARG_SL) && defined(__SL__) 
  FB_FREQ( FB_FREQ_TYPE type, FREQ_VALUE_TYPE value )
    : _type( type ), _value( FREQ_SET_OFFSET(value) ) { }
#else
  FB_FREQ( FB_FREQ_TYPE type, FREQ_VALUE_TYPE value )
    : _type( type ), _value( value ) { }
#endif  


public:

  // Constructor methods
#if defined(TARG_SL) && defined(__SL__) 
  FB_FREQ()
    : _type( FB_FREQ_TYPE_UNINIT ),
      _value( FREQ_SET_OFFSET((FREQ_VALUE_TYPE) FB_FREQ_TYPE_UNINIT) ) {}

  FB_FREQ( FREQ_VALUE_TYPE value, bool exact )
    : _type( exact ? FB_FREQ_TYPE_EXACT : FB_FREQ_TYPE_GUESS ),
      _value( FREQ_SET_OFFSET(value))
    { Is_True( value >= 0, ( "FB_FREQ: negative value %ld", value ) ); }
  FB_FREQ( INT32 value )
    : _type( FB_FREQ_TYPE_EXACT ),
      _value( FREQ_SET_OFFSET((FREQ_VALUE_TYPE) value))
    { Is_True( value >= 0, ( "FB_FREQ: negative value %ld", value ) ); }
  FB_FREQ( FREQ_VALUE_TYPE value )
    : _type( FB_FREQ_TYPE_EXACT ),
      _value(FREQ_SET_OFFSET(value))
    { Is_True( value >= 0, ( "FB_FREQ: negative value %ld", value ) ); }

  FB_FREQ( FB_FREQ_TYPE type )
    : _type( type ),
      _value( type >= 0 ? FREQ_SET_OFFSET(0) : FREQ_SET_OFFSET((FREQ_VALUE_TYPE) type) ) {
    Is_True( FB_FREQ_TYPE_IS_VALID( type ),
	     ( "FB_FREQ: invalid type %d", type ) );
  }
#else

  FB_FREQ()
    : _type( FB_FREQ_TYPE_UNINIT ),
      _value( (FREQ_VALUE_TYPE) FB_FREQ_TYPE_UNINIT ) {}

  FB_FREQ( float value, bool exact )
    : _type( exact ? FB_FREQ_TYPE_EXACT : FB_FREQ_TYPE_GUESS ),
      _value( value )
    { Is_True( value >= 0.0, ( "FB_FREQ: negative value %f", value ) ); }

  FB_FREQ( INT32 value )
    : _type( FB_FREQ_TYPE_EXACT ),
      _value( (FREQ_VALUE_TYPE) value )
    { Is_True( value >= 0, ( "FB_FREQ: negative value %lld", value ) ); }

#ifdef KEY
  FB_FREQ( double value )
    : _type( FB_FREQ_TYPE_EXACT ),
      _value( (FREQ_VALUE_TYPE) value )
    { Is_True( value >= 0.0, ( "FB_FREQ: negative value %lld", value ) ); }

  FB_FREQ( float value )
    : _type( FB_FREQ_TYPE_EXACT ),
      _value( value )
    { Is_True( value >= 0.0, ( "FB_FREQ: negative value %lld", value ) ); }
#endif

  FB_FREQ( INT64 value )
    : _type( FB_FREQ_TYPE_EXACT ),
      _value( (FREQ_VALUE_TYPE) value )
    { Is_True( value >= 0, ( "FB_FREQ: negative value %lld", value ) ); }

  FB_FREQ( FB_FREQ_TYPE type )
    : _type( type ),
      _value( type >= 0 ? 0.0 : (FREQ_VALUE_TYPE) type ) {
    Is_True( FB_FREQ_TYPE_IS_VALID( type ),
	     ( "FB_FREQ: invalid type %d", type ) );
  }
#endif

  // Member access methods
  FREQ_VALUE_TYPE Value() const { return _value; }
  FB_FREQ_TYPE Type() const { return _type; }
  FREQ_VALUE_TYPE& Get_Value() { return _value; }
  void   Set_Value(FREQ_VALUE_TYPE val) { _value=val; }

#if defined(TARG_SL) && defined(__SL__) 

  friend FB_FREQ operator+ ( const FB_FREQ freq1, const FB_FREQ freq2 ) {
    FB_FREQ_TYPE type = FB_FREQ_TYPE_COMBINE( freq1._type, freq2._type );
    if ( FB_FREQ_TYPE_NOT_KNOWN( type ) )
      return FB_FREQ( type );
    return FB_FREQ( type, FREQ_RESET_OFFSET(freq1._value) +FREQ_RESET_OFFSET( freq2._value));
  }

  FB_FREQ operator+= ( const FB_FREQ freq ) {
    _type = FB_FREQ_TYPE_COMBINE( _type, freq._type );
    if ( FB_FREQ_TYPE_NOT_KNOWN( _type ) )
      _value = FREQ_SET_OFFSET((FREQ_VALUE_TYPE) _type);
    else
      _value += FREQ_RESET_OFFSET(freq._value) ;
    return *this;
  }

  void Print( FILE *fp ) const {
    switch ( _type ) {
    case FB_FREQ_TYPE_EXACT:
      fprintf( fp, "%d!", _value );
      break;
    case FB_FREQ_TYPE_GUESS:
      fprintf( fp, "%d?", _value );
      break;
    case FB_FREQ_TYPE_UNKNOWN:
      fprintf( fp, "unknown" );
      break;
    case FB_FREQ_TYPE_UNINIT:
      fprintf( fp, "uninitialized" );
      break;
    case FB_FREQ_TYPE_ERROR:
      fprintf( fp, "error" );
      break;
    default:
      Is_True( FALSE, ("FB_FREQ: Unexpected type %d", _type ));
      break;
    }
  }

  void Print_simple (FILE * fp) const {
    fprintf(fp, "_type = %d   |  _value = %f \n", _type, _value);
  }

#else

  // Boolean tests

  bool Known()         const { return FB_FREQ_TYPE_IS_KNOWN( _type ); }
  bool Guess()         const { return FB_FREQ_TYPE_IS_GUESS( _type ); }
  bool Exact()         const { return FB_FREQ_TYPE_IS_EXACT( _type ); }
  bool Uninitialized() const { return ( _type == FB_FREQ_TYPE_UNINIT ); }
  bool Initialized()   const { return FB_FREQ_TYPE_INITIALIZED( _type ); }
  bool Error()         const { return ( _type == FB_FREQ_TYPE_ERROR ); }
  bool Zero()          const { return ( _value < FB_FREQ_EPSILON &&
					_value > - FB_FREQ_EPSILON ); }

  bool Better( const FB_FREQ freq ) const {
    return FB_FREQ_TYPE_BETTER( _type, freq._type );
  }

  FB_FREQ sqrt() {
    if ( Zero() && Exact() )
      return FB_FREQ( FB_FREQ_TYPE_EXACT, 0.0 );
    FB_FREQ_TYPE type = FB_FREQ_TYPE_COMBINE( _type, FB_FREQ_TYPE_GUESS );
    if ( FB_FREQ_TYPE_NOT_KNOWN( type ) )
      return FB_FREQ( type );
    Is_True( _value >= 0, ( "FB_FREQ: negative value %ld", _value ) );
    return FB_FREQ( type, ::sqrt((double)_value) );
    return *this;
  }
  
  // Operators

  FB_FREQ operator+= ( const FB_FREQ freq ) {
    _type = FB_FREQ_TYPE_COMBINE( _type, freq._type );
    if ( FB_FREQ_TYPE_NOT_KNOWN( _type ) )
      _value = (FREQ_VALUE_TYPE) _type;
    else
      _value += freq._value;
    return *this;
  }

  FB_FREQ operator-= ( const FB_FREQ freq ) {
    _type = FB_FREQ_TYPE_COMBINE( _type, freq._type );
    if ( FB_FREQ_TYPE_NOT_KNOWN( _type ) )
      _value = (FREQ_VALUE_TYPE) _type;
    else {
      _value -= freq._value;
      if ( _value < 0 )
	if ( - _value < FB_FREQ_EPSILON
	     || - _value < FB_FREQ_EPSILON * freq._value )
	  _value = 0;
	else {
	  DevWarn( "FB_FREQ: subtraction of larger from smaller value" );
	  _type  = FB_FREQ_TYPE_ERROR;
	  _value = (FREQ_VALUE_TYPE) _type;
	}
    }
    return *this;
  }

  FB_FREQ operator*= ( const FB_FREQ freq ) {
    if ( Zero() && Exact() )
      ; // Nothing to do
    else if ( freq.Zero() && freq.Exact() ) {
      _type  = FB_FREQ_TYPE_EXACT;
      _value = 0.0;
    } else {
      _type = FB_FREQ_TYPE_COMBINE( _type, freq._type );
      if ( FB_FREQ_TYPE_NOT_KNOWN( _type ) )
	_value = (FREQ_VALUE_TYPE) _type;
      else
	_value *= freq._value;
    }
    return *this;
  }

  // All scale factors are assumed to be guesses
  FB_FREQ operator*= ( const FREQ_VALUE_TYPE scale ) {
    Is_True ( scale >= 0.0, ( "FB_FREQ: negative scale %f", scale ) );
    return ( *this *= FB_FREQ( FB_FREQ_TYPE_GUESS, scale ) );
  }

  // Division used to determine scale or probability.
  // Results other than exact 0s and 1s are assumed to be guesses.
  FB_FREQ operator/= ( const FB_FREQ freq ) {
    if ( Zero() && Exact() )
      ; // Nothing to do
    else if ( freq.Zero() ) {
      DevWarn("FB_FREQ: division by zero");
      _type  = FB_FREQ_TYPE_ERROR;
      _value = (FREQ_VALUE_TYPE) _type;
    } else {
      _type = FB_FREQ_TYPE_COMBINE( _type, freq._type );
      if ( FB_FREQ_TYPE_NOT_KNOWN( _type ) )
	_value = (FREQ_VALUE_TYPE) _type;
      else {
	if ( _value != freq._value )
	  _type = FB_FREQ_TYPE_COMBINE( _type, FB_FREQ_TYPE_GUESS );
	_value /= freq._value;
      }
    }
    return *this;
  }

  // All scale factors are assumed to be guesses
  FB_FREQ operator/= ( const FREQ_VALUE_TYPE scale ) {
    Is_True ( scale >= 0.0, ( "FB_FREQ: negative scale %f", scale ) );
    return ( *this /= FB_FREQ( FB_FREQ_TYPE_GUESS, scale ) );
  }

  // Binary operations

  friend FB_FREQ operator+ ( const FB_FREQ freq1, const FB_FREQ freq2 ) {
    FB_FREQ_TYPE type = FB_FREQ_TYPE_COMBINE( freq1._type, freq2._type );
    if ( FB_FREQ_TYPE_NOT_KNOWN( type ) )
      return FB_FREQ( type );
    return FB_FREQ( type, freq1._value + freq2._value );
  }

  friend FB_FREQ operator- ( const FB_FREQ freq1, const FB_FREQ freq2 ) {
    FB_FREQ_TYPE type = FB_FREQ_TYPE_COMBINE( freq1._type, freq2._type );
    if ( FB_FREQ_TYPE_NOT_KNOWN( type ) )
      return FB_FREQ( type );
    FREQ_VALUE_TYPE value = freq1._value - freq2._value;
    if ( value >= 0 )
      return FB_FREQ( type, value );
    if ( - value < FB_FREQ_EPSILON
	 || - value < freq2._value * FB_FREQ_EPSILON )
      return FB_FREQ( type, 0 );
    DevWarn( "FB_FREQ: subtraction of larger from smaller value" );
    return FB_FREQ( FB_FREQ_TYPE_ERROR );
  }

  friend FB_FREQ operator* ( const FB_FREQ freq1, const FB_FREQ freq2 ) {
    if ( ( freq1.Zero() && freq1.Exact() ) ||
	 ( freq2.Zero() && freq2.Exact() ) )
      return FB_FREQ( FB_FREQ_TYPE_EXACT, 0.0 );
    FB_FREQ_TYPE type = FB_FREQ_TYPE_COMBINE( freq1._type, freq2._type );
    if ( FB_FREQ_TYPE_NOT_KNOWN( type ) )
      return FB_FREQ( type );
    return FB_FREQ( type, freq1._value * freq2._value );
  }

  // All scale factors are assumed to be guesses
  friend FB_FREQ operator* ( const FB_FREQ freq, const FREQ_VALUE_TYPE scale ) {
    Is_True ( scale >= 0.0, ( "FB_FREQ: negative scale %f", scale ) );
    return ( freq * FB_FREQ( FB_FREQ_TYPE_GUESS, scale ) );
  }

  // Division used to determine scale or probability.
  // Results other than exact 0s and 1s are assumed to be guesses.
  friend FB_FREQ operator/ ( const FB_FREQ freq1, const FB_FREQ freq2 ) {
    if ( freq1.Zero() && freq1.Exact() )
      return FB_FREQ( FB_FREQ_TYPE_EXACT, 0.0 );
    if ( freq2.Zero() ) {
      DevWarn("FB_FREQ: division by zero");
      return FB_FREQ( FB_FREQ_TYPE_ERROR );
    }
    FB_FREQ_TYPE type = FB_FREQ_TYPE_COMBINE( freq1._type, freq2._type );
    if ( FB_FREQ_TYPE_NOT_KNOWN( type ) )
      return FB_FREQ( type );
    if ( freq1._value != freq2._value )
      type = FB_FREQ_TYPE_COMBINE( type, FB_FREQ_TYPE_GUESS );
    FREQ_VALUE_TYPE value = freq1._value / freq2._value;
    return FB_FREQ( type, value );
  }

  // All scale factors are assumed to be guesses
  friend FB_FREQ operator/ ( const FB_FREQ freq, FREQ_VALUE_TYPE scale ) {
    Is_True ( scale >= 0.0, ( "FB_FREQ: negative scale %f", scale ) );
    return ( freq / FB_FREQ( FB_FREQ_TYPE_GUESS, scale ) );
  }

  friend bool operator== ( const FB_FREQ freq1, const FB_FREQ freq2 ) {
    if ( freq1._value > freq2._value )
      return ( freq1._value < FB_FREQ_EPSILON
	       || freq1._value * ( 1 - FB_FREQ_EPSILON ) < freq2._value );
    else if ( freq1._value < freq2._value )
      return ( freq2._value < FB_FREQ_EPSILON
	       || freq2._value * ( 1 - FB_FREQ_EPSILON ) < freq1._value );
    else return true;
  }

  friend bool operator!= ( const FB_FREQ freq1, const FB_FREQ freq2 ) {
    if ( freq1._value > freq2._value )
      return ( freq1._value >= FB_FREQ_EPSILON
	       && freq1._value * ( 1 - FB_FREQ_EPSILON ) >= freq2._value );
    else if ( freq1._value < freq2._value )
      return ( freq2._value >= FB_FREQ_EPSILON
	       && freq2._value * ( 1 - FB_FREQ_EPSILON ) >= freq1._value );
    else return false;
  }

  friend bool operator> ( const FB_FREQ freq1, const FB_FREQ freq2 ) {
    return ( freq1._value > freq2._value );
  }

  friend bool operator< ( const FB_FREQ freq1, const FB_FREQ freq2 ) {
    return ( freq1._value < freq2._value );
  }

#ifdef KEY // fix problem with g++ 3.2
  friend bool operator>=( const FB_FREQ freq1, const FB_FREQ freq2 ) {
    return ( freq1._value >= freq2._value );
  }

  friend bool operator<=( const FB_FREQ freq1, const FB_FREQ freq2 ) {
    return ( freq1._value <= freq2._value );
  }
#endif

  // Printing methods

  void Print( FILE *fp ) const {
    switch ( _type ) {
    case FB_FREQ_TYPE_EXACT:
      fprintf( fp, "%g!", _value );
      break;
    case FB_FREQ_TYPE_GUESS:
      fprintf( fp, "%g?", _value );
      break;
    case FB_FREQ_TYPE_UNKNOWN:
      fprintf( fp, "unknown" );
      break;
    case FB_FREQ_TYPE_UNINIT:
      fprintf( fp, "uninitialized" );
      break;
    case FB_FREQ_TYPE_ERROR:
      fprintf( fp, "error" );
      break;
    default:
      Is_True( FALSE, ("FB_FREQ: Unexpected type %d", _type ));
      break;
    }
  }

  void Print_simple (FILE * fp) const {
    fprintf(fp, "_type = %d   |  _value = %f \n", _type, _value);
  }

  INT Sprintf( char *buffer ) const {
    INT length = 0;
    switch ( _type ) {
    case FB_FREQ_TYPE_EXACT:
      length = sprintf( buffer, "%g!", _value );
      break;
    case FB_FREQ_TYPE_GUESS:
      length = sprintf( buffer, "%g?", _value );
      break;
    case FB_FREQ_TYPE_UNKNOWN:
      length = sprintf( buffer, "unknown" );
      break;
    case FB_FREQ_TYPE_UNINIT:
      length = sprintf( buffer, "uninitialized" );
      break;
    case FB_FREQ_TYPE_ERROR:
      length = sprintf( buffer, "error" );
      break;
    default:
      Is_True( FALSE, ("FB_FREQ: Unexpected type %d", _type ));
      break;
    }
    return length;
  }
#endif  
};


// Some FB_FREQ constants.  For unknown and uninitialized frequencies,
// use these instead of invoking an FB_FREQ constructor.
const FB_FREQ FB_FREQ_ZERO(    0, true /*EXACT*/  );
const FB_FREQ FB_FREQ_UNKNOWN( FB_FREQ_TYPE_UNKNOWN );
const FB_FREQ FB_FREQ_UNINIT(  FB_FREQ_TYPE_UNINIT  );
const FB_FREQ FB_FREQ_ERROR(   FB_FREQ_TYPE_ERROR   );

// ====================================================================

#endif //#ifndef fb_freq_INCLUDED
