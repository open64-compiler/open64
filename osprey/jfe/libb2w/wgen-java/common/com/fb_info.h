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
// Module: fb_info.h
// $Revision: 1.1.1.1 $
// $Date: 2005/10/21 19:00:00 $
// $Author: marcel $
// $Source: /proj/osprey/CVS/open64/osprey1.0/common/com/fb_info.h,v $
//
// Description:
//
// During instrumentation, certain events associated with program
// control flow (such as the number of times a particular branch is
// taken, or not taken) are counted.  During subsequent compilations,
// this data may be retrieved and used to guide optimization decisions.
//
// fb_info.h defines data structs used to store this feedback data for
// various types of whirl nodes.  These structures are used by the
// FEEDBACK class (defined in fb_whirl.h) and by the instrumentation runtime
//
// ====================================================================
// ====================================================================

#ifndef fb_info_INCLUDED
#define fb_info_INCLUDED

#include "fb_freq.h"
#include "fb_tnv.h"

#ifdef _INCLUDED_FROM_LIBINSTR_
#include "vector.h"

using Instr::vector;
#else
#include <vector>             // STL vector.
#endif

#ifdef MONGOOSE_BE

#ifndef mempool_allocator_INCLUDED
#include "mempool_allocator.h"
#endif

#ifndef wn_INCLUDED
#include "wn.h"
#endif

#endif // MONGOOSE_BE

#define FB_TNV_FLAG_UNINIT  -1

// data structures shared by both the instrumentation runtime and the back
// end. 

struct FB_Info_Invoke {

  FB_FREQ freq_invoke;    // number of times statement invoked

  FB_Info_Invoke( FB_FREQ invoke)
    : freq_invoke( invoke ) {}

  FB_Info_Invoke() :
    freq_invoke( FB_FREQ_UNINIT ) {}

  void Print( FILE *fp ) const {
    fprintf( fp, "FB---> invoke = " );
    freq_invoke.Print( fp );
  }

  void Print_simple (FILE *fp) const {
    fprintf( fp, "FB---> invoke = " );
    freq_invoke.Print_simple( fp );
  	}
};

#ifdef KEY
// use vector later, like FB_Info_Switch.
#define TNV 10
struct FB_Info_Value {
  INT64   num_values;   // how many valid entries in the value array
  FB_FREQ exe_counter;  // how many times this inst is executed
  INT64   value[TNV];   // the top TNV profiled values
  FB_FREQ freq[TNV];    // the corresponding freq for each value

  FB_Info_Value() : num_values(0), exe_counter(0.0) {}

  FB_Info_Value( const INT64 num, const INT64 e, const INT64* v, const INT64* f ) {
    num_values = MIN( TNV, num );
    exe_counter = e;

    for( int i = 0; i < num_values; i++ ){
      value[i] = v[i];
      freq[i] = f[i];
      Is_True( exe_counter >= freq[i], ("Execution counter overflows.") );
    }
  }

  FB_FREQ Total() const {    return exe_counter;   }

  void Print( FILE* fp ) const {
    fprintf( fp, "execution counter: %d\n", (int)exe_counter.Value() );
    for( int i = 0; i < num_values; i++ ){
      fprintf( fp, "value %lld\t freq %f\n", value[i], freq[i].Value() );
    }
  }
};

struct FB_Info_Value_FP_Bin {
  FB_FREQ exe_counter;  // how many times this inst is executed
  FB_FREQ zopnd0;       // how many times was operand 0 == 0.0
  FB_FREQ zopnd1;       // how many times was operand 1 == 0.0
  FB_FREQ uopnd0;       // how many times was operand 0 == 1.0
  FB_FREQ uopnd1;       // how many times was operand 1 == 1.0

  FB_Info_Value_FP_Bin() : exe_counter(0.0), zopnd0(0.0), zopnd1(0.0),
                           uopnd0(0.0), uopnd1(0.0) {}

  FB_Info_Value_FP_Bin( const INT64 e, const INT64 z0, const INT64 z1,
  			const INT64 u0, const INT64 u1 ) {
    exe_counter = e;
    zopnd0 = z0;
    zopnd1 = z1;
    uopnd0 = u0;
    uopnd1 = u1;
  }

  FB_FREQ Total() const {    return exe_counter;   }

  void Print( FILE* fp ) const {
    fprintf( fp, "execution counter: %d\n", (int)exe_counter.Value() );
    fprintf( fp, "operand 0 zero counter: %d\n", (int)zopnd0.Value() );
    fprintf( fp, "operand 1 zero counter: %d\n", (int)zopnd1.Value() );
    fprintf( fp, "operand 0 one counter: %d\n", (int)uopnd0.Value() );
    fprintf( fp, "operand 1 one counter: %d\n", (int)uopnd1.Value() );
  }
};
#endif

struct FB_Info_Branch {

  FB_FREQ freq_taken;     // number of times branch taken
                          // (then clause for IF, Kid0 for CSELECT)
  FB_FREQ freq_not_taken; // number of times branch not taken
                          // (else clause for IF, Kid1 for CSELECT)

  FB_Info_Branch( FB_FREQ taken, FB_FREQ not_taken )
    : freq_taken(     taken ),
      freq_not_taken( not_taken ) {}

#ifdef MONGOOSE_BE
  FB_Info_Branch( FB_FREQ taken, FB_FREQ not_taken, OPERATOR opr )
    : freq_taken(     opr != OPR_FALSEBR ? taken : not_taken ),
      freq_not_taken( opr != OPR_FALSEBR ? not_taken : taken ) {
    // if opr == OPR_FALSEBR, then reverse taken and not_taken
    Is_True( opr == OPR_TRUEBR || opr == OPR_FALSEBR
	     || opr == OPR_IF || opr == OPR_CSELECT,
	     ( "FB_Info_Branch found unexpected operator" ) );
  }
#endif // MONGOOSE_BE
    
  FB_Info_Branch()
    : freq_taken(     FB_FREQ_UNINIT ),
      freq_not_taken( FB_FREQ_UNINIT ) {}

  void Print( FILE *fp ) const {
    fprintf( fp, "FB---> taken = " );
    freq_taken.Print( fp );
    fprintf( fp, ", not_taken = " );
    freq_not_taken.Print( fp );
  }

  void Print_simple( FILE *fp ) const {
    fprintf( fp, "FB---> taken = " );
    freq_taken.Print_simple( fp );
    fprintf( fp, ", not_taken = " );
    freq_not_taken.Print_simple( fp );
  }
  FB_FREQ Total() const {
    return ( freq_taken + freq_not_taken );
  }
};

// ====================================================================

// Feedback info for a loop node
//         ___|__
//        | test |
//        |______|
//          /  \
//     zero/N  Y\positive
//        /      \    ____
//        |      _\__/_   \
//        |     | body |   |
//        |     |______|   |        exit    = zero     + out
//        |     | test |   |        iterate = positive + back
//        |     |______|   |
//        |       /  \    /         In a DO_WHILE loop, zero = 0.
//        |   out/N  Y\__/
//         \    /     back
//         _\__/_
//        |      |


struct FB_Info_Loop {

  FB_FREQ freq_zero;
  FB_FREQ freq_positive;
  FB_FREQ freq_out;
  FB_FREQ freq_back;
  FB_FREQ freq_exit;
  FB_FREQ freq_iterate;

  FB_Info_Loop( FB_FREQ zero, FB_FREQ positive, FB_FREQ out, FB_FREQ back,
		FB_FREQ exit, FB_FREQ iterate )
    : freq_zero(     zero ),
      freq_positive( positive ),
      freq_out(      out ),
      freq_back(     back ),
      freq_exit(     exit ),
      freq_iterate(  iterate ) {}

  FB_Info_Loop( FB_FREQ zero, FB_FREQ positive, FB_FREQ out, FB_FREQ back )
    : freq_zero(     zero ),
      freq_positive( positive ),
      freq_out(      out ),
      freq_back(     back ),
      freq_exit(     zero + out ),
      freq_iterate(  positive + back ) {}

  FB_Info_Loop( FB_FREQ exit, FB_FREQ iterate )
    : freq_zero(     FB_FREQ_UNKNOWN ),
      freq_positive( FB_FREQ_UNKNOWN ),
      freq_out(      FB_FREQ_UNKNOWN ),
      freq_back(     FB_FREQ_UNKNOWN ),
      freq_exit(     exit ),
      freq_iterate(  iterate ) {}

  FB_Info_Loop()
    : freq_zero(     FB_FREQ_UNINIT ),
      freq_positive( FB_FREQ_UNINIT ),
      freq_out(      FB_FREQ_UNINIT ),
      freq_back(     FB_FREQ_UNINIT ),
      freq_exit(     FB_FREQ_UNINIT ),
      freq_iterate(  FB_FREQ_UNINIT ) {}

  void Print( FILE *fp ) const {
    fprintf( fp, "FB---> zero = " );
    freq_zero.Print( fp );
    fprintf( fp, ", positive = " );
    freq_positive.Print( fp );
    fprintf( fp, ", out = " );
    freq_out.Print( fp );
    fprintf( fp, ", back = " );
    freq_back.Print( fp );
    fprintf( fp, "\n       exit = " );
    freq_exit.Print( fp );
    fprintf( fp, ", iterate = " );
    freq_iterate.Print( fp );
  }

  void Print_simple( FILE *fp ) const {
    fprintf( fp, "FB---> zero = " );
    freq_zero.Print_simple( fp );
    fprintf( fp, ", positive = " );
    freq_positive.Print_simple( fp );
    fprintf( fp, ", out = " );
    freq_out.Print( fp );
    fprintf( fp, ", back = " );
    freq_back.Print_simple( fp );
    fprintf( fp, "\n       exit = " );
    freq_exit.Print_simple( fp );
    fprintf( fp, ", iterate = " );
    freq_iterate.Print_simple( fp );
  }

  FB_FREQ Total() const {
    return ( freq_exit + freq_iterate );
  }

  float Loop_lower_scale() const {
    FB_FREQ freq_scale = ( freq_zero + freq_positive ) / Total();
    if ( freq_scale.Known() )
      return freq_scale.Value();
    else
      return 0.1;  // This is a wild guess!!
  }
};

struct FB_Info_Circuit {

  FB_FREQ freq_left;   // "taken" means false for CAND, true for CIOR
  FB_FREQ freq_right;
  FB_FREQ freq_neither;

  FB_Info_Circuit( FB_FREQ left, FB_FREQ right, FB_FREQ neither )
    : freq_left(    left    ),
      freq_right(   right   ),
      freq_neither( neither ) {}

  FB_Info_Circuit()
    : freq_left(    FB_FREQ_UNINIT ),
    freq_right(   FB_FREQ_UNINIT ),
    freq_neither( FB_FREQ_UNINIT ) {}

  void Print( FILE *fp ) const {
    fprintf( fp, "FB---> left = " );
    freq_left.Print( fp );
    fprintf( fp, ", right = " );
    freq_right.Print( fp );
    fprintf( fp, ", neither = " );
    freq_neither.Print( fp );
  }

  void Print_simple( FILE *fp ) const {
    fprintf( fp, "FB---> left = " );
    freq_left.Print_simple( fp );
    fprintf( fp, ", right = " );
    freq_right.Print_simple( fp );
    fprintf( fp, ", neither = " );
    freq_neither.Print_simple( fp );
  }

  FB_FREQ Total() const {
    return ( freq_left + freq_right + freq_neither );
  }
};


struct FB_Info_Call {
  FB_FREQ freq_entry;
  FB_FREQ freq_exit;
  BOOL    in_out_same;  // TRUE iff freq_entry and freq_exit must be ==
  BOOL    dummy_buffer; // FB_Info_Call size must be 0 mod 64bits for
			//   consistent feedback file alignment in IA32/64

  FB_Info_Call( FB_FREQ entry, FB_FREQ exit, BOOL same = FALSE )
    : freq_entry(  entry ),
      freq_exit(   exit  ),
      in_out_same( same ) {
    Is_True( freq_entry == freq_exit || ! in_out_same,
	     ( "FEEDBACK::Annot_call: in_out_same failure" ) );
  }

  FB_Info_Call( FB_FREQ entry_and_exit )
    : freq_entry(  entry_and_exit ),
      freq_exit(   entry_and_exit ),
      in_out_same( TRUE  ) {}


  FB_Info_Call() :
    freq_entry(FB_FREQ_UNINIT),
    freq_exit(FB_FREQ_UNINIT),
    in_out_same(FALSE) {}

  void Print( FILE *fp ) const {
    fprintf( fp, "FB---> entry = " );
    freq_entry.Print( fp );
    fprintf( fp, ", exit = " );
    freq_exit.Print( fp );
    fprintf( fp, ", in_out_same = %c", in_out_same ? 'Y' : 'N' );
  }

  void Print_simple( FILE *fp ) const {
    fprintf( fp, "FB---> entry = " );
    freq_entry.Print_simple( fp );
    fprintf( fp, ", exit = " );
    freq_exit.Print_simple( fp );
    fprintf( fp, ", in_out_same = %c", in_out_same ? 'Y' : 'N' );
  }

};

struct FB_Info_Icall{
      FB_TNV tnv;
	  FB_Info_Icall()
	  {
		  tnv._flag = FB_TNV_FLAG_UNINIT;
	  }
	  BOOL Is_uninit() const
	  {
		  return (tnv._flag == FB_TNV_FLAG_UNINIT);
	  }
      void Print( FILE *fp) const {
      	fprintf(fp, "FB--->Icall = ");
      	tnv.Print( fp );
      }
};

struct FB_Info_Switch {

  vector<FB_FREQ> freq_targets;

  FB_Info_Switch() {}

  FB_Info_Switch( const vector<FB_FREQ>::size_type n )
    : freq_targets( vector<FB_FREQ>( n, FB_FREQ_UNINIT ) ) {}

  FB_FREQ& operator[] ( const vector<FB_FREQ>::size_type n ) {
    if ( n >= freq_targets.size() ) {
      // KEY
      vector<FB_FREQ>::size_type size = freq_targets.size();
      for (int i = 0; i < n - size + 1; i++)
        freq_targets.push_back (FB_FREQ_UNINIT);
    }
    return freq_targets[n];
  }

  const FB_FREQ& operator[] ( const vector<FB_FREQ>::size_type n ) const {
    return ( n >= freq_targets.size() ? FB_FREQ_UNINIT : freq_targets[n] );
  }
    
  vector<FB_FREQ>::size_type size() const {
    return freq_targets.size();
  }

  vector<FB_FREQ>::const_reference back() const {
    return freq_targets.back();
  }

  void pop_back() {
    freq_targets.pop_back();
  }

  void Print( FILE *fp ) const {
    fprintf( fp, "FB---> targets = %d", (INT) freq_targets.size() );
    for ( INT t = 0; t < freq_targets.size(); t++ ) {
      fprintf( fp, ", %d: ", t );
      freq_targets[t].Print( fp );
    }
  }

  void Print_simple( FILE *fp ) const {
    fprintf( fp, "FB---> targets = %d", (INT) freq_targets.size() );
    for ( INT t = 0; t < freq_targets.size(); t++ ) {
      fprintf( fp, ", %d: ", t );
      freq_targets[t].Print_simple( fp );
    }
  }

  FB_FREQ Total() const {
    vector<FB_FREQ>::const_iterator iter;
    FB_FREQ freq = FB_FREQ_ZERO;
    for ( iter = freq_targets.begin(); iter != freq_targets.end(); iter++ ) {
      freq += *iter;
    }
    return freq;
  }
};

struct FB_Info_Edge {

  FB_FREQ freq_edge;    // number of times edge

  FB_Info_Edge( FB_FREQ freq)
    : freq_edge( freq ) {}

  FB_Info_Edge() :
    freq_edge( FB_FREQ_UNINIT ) {}

  void Print( FILE *fp ) const {
    fprintf( fp, "FB---> Edge = " );
    freq_edge.Print( fp );
  }
};

#ifndef KEY
struct FB_Info_Value {
      FB_TNV tnv;
      void Print( FILE *fp) const {
      	fprintf(fp, "FB--->Value = ");
      	tnv.Print( fp );
      }
};
#endif

struct FB_Info_Stride{
      FB_TNV tnv;
      void Print( FILE *fp) const {
      	fprintf(fp, "FB--->Stride = ");
      	tnv.Print( fp );
      }
};

// Print the entire FB_Info vector
template <class T>
void
FB_Info_Print (const T& info, const char* name, FILE *fp)
{
    size_t size = info.size ();

    if (size != 0)
	fprintf (fp, "%s Profile:\n", name);
    for (size_t i = 0; i < size; i++) {
	fprintf(fp, "\t%s id = %ld\t", name, i);
	info[i].Print (fp);
	fputc ('\n', fp);
    }
}


// utilities and definitions only used by the back end (and not by the
// instrumentation runtime
#ifdef MONGOOSE_BE

typedef vector<FB_Info_Invoke, mempool_allocator<FB_Info_Invoke> >
					FB_Invoke_Vector;
typedef vector<FB_Info_Branch, mempool_allocator<FB_Info_Branch> >
					FB_Branch_Vector;
typedef vector<FB_Info_Loop, mempool_allocator<FB_Info_Loop> >
					FB_Loop_Vector;
typedef vector<FB_Info_Circuit, mempool_allocator<FB_Info_Circuit> >
					FB_Circuit_Vector;
typedef vector<FB_Info_Call, mempool_allocator<FB_Info_Call> >
					FB_Call_Vector;
typedef vector<FB_Info_Icall, mempool_allocator<FB_Info_Icall> >
					FB_Icall_Vector;
typedef vector<FB_Info_Switch, mempool_allocator<FB_Info_Switch> >
					FB_Switch_Vector;
typedef vector<FB_Info_Edge, mempool_allocator<FB_Info_Edge> >
					FB_Edge_Vector;
typedef vector<FB_Info_Value, mempool_allocator<FB_Info_Value> >
                                       FB_Value_Vector;
#ifdef KEY
typedef vector<FB_Info_Value_FP_Bin, mempool_allocator<FB_Info_Value_FP_Bin> >
                                       FB_Value_FP_Bin_Vector;
#endif

// ====================================================================
// FB_EDGE_TYPE -- Indicates the source of a node's frequency data
//
// Frequency values are assigned to edges in the control flow graph.
// ====================================================================

// FB_IO_ESCAPE_EDGES_MAX is the maximum number of labels that can appear
//   within the ITEMs pf an IO as branch targets.

#define FB_IO_ESCAPE_EDGES_MAX 3

enum FB_EDGE_TYPE {
  FB_EDGE_UNINIT           =  0,  // NOTE: If any of these are changed, then
  FB_EDGE_INCOMING         =  1,  // the array FB_EDGE_NAMES in fb_info.cxx
  FB_EDGE_OUTGOING         =  2,  // must also be updated!
  FB_EDGE_ENTRY_OUTGOING   =  3,
  FB_EDGE_BRANCH_TAKEN     =  4,
  FB_EDGE_BRANCH_NOT_TAKEN =  5,
  FB_EDGE_LOOP_ZERO        =  6,
  FB_EDGE_LOOP_POSITIVE    =  7,
  FB_EDGE_LOOP_OUT         =  8,
  FB_EDGE_LOOP_BACK        =  9,
  FB_EDGE_LOOP_EXIT        = 10,  // EXIT    == ZERO     + OUT
  FB_EDGE_LOOP_ITERATE     = 11,  // ITERATE == POSITIVE + BACK
  FB_EDGE_CIRCUIT_LEFT     = 12,
  FB_EDGE_CIRCUIT_RIGHT    = 13,
  FB_EDGE_CIRCUIT_NEITHER  = 14,
  FB_EDGE_CALL_INCOMING    = 15,
  FB_EDGE_CALL_OUTGOING    = 16,
  FB_EDGE_CALL_INOUTSAME   = 17,
  FB_EDGE_IO_OUTGOING      = 18,
  FB_EDGE_IO_ESCAPE_BASE   = 19,
  FB_EDGE_SWITCH_DEFAULT   = 22,  // 19 + FB_IO_ESCAPE_EDGES_MAX
  FB_EDGE_SWITCH_BASE      = 23   // must be last
};

// IO     branch br is BASE + br; 0 <= br < FB_IO_ESCAPE_EDGES_MAX
// SWITCH branch br is BASE + br; 0 <= br

#define FB_EDGE_IO_ESCAPE(br) ( FB_EDGE_TYPE( FB_EDGE_IO_ESCAPE_BASE + (br) ) )

#define FB_EDGE_SWITCH(br)    ( FB_EDGE_TYPE( FB_EDGE_SWITCH_BASE    + (br) ) )
#define FB_EDGE_SWITCH_INDEX(typ)  ( INT32( (typ) - FB_EDGE_SWITCH_BASE + 1 ) )

#define FB_EDGE_TYPE_NAME_LENGTH 20  // buffer length required for
				     //   FB_EDGE_TYPE_sprintf

extern const char *FB_EDGE_NAMES[];

void FB_EDGE_TYPE_fprintf( FILE *fp,     const FB_EDGE_TYPE fb_type );
INT  FB_EDGE_TYPE_sprintf( char *buffer, const FB_EDGE_TYPE fb_type );


// ====================================================================
// The following structures hold feedback info for the incoming and
// outgoing edges of various kinds of whirl nodes.
// ====================================================================

// Minimal feedback info for label, goto, entry and return nodes, and
// the WN_PRAGMA_PREAMBLE_END PRAGMA node

#define fb_opr_cases_invoke \
  case OPR_LABEL:           \
  case OPR_GOTO:            \
  case OPR_MSTORE:          \
  case OPR_FUNC_ENTRY:      \
  case OPR_ALTENTRY:        \
  case OPR_RETURN:          \
  case OPR_RETURN_VAL
// Note that OPR_PRAGMA is not included in fb_opr_cases_invoke

inline bool FB_valid_opr_invoke(const WN *wn) {
  OPERATOR opr = WN_operator( wn );
  switch ( opr ) {
  fb_opr_cases_invoke:
    return true;
  case OPR_PRAGMA:
    return ( WN_pragma( wn ) == WN_PRAGMA_PREAMBLE_END );
  default:
    return false;
  }
}


// ====================================================================

// Feedback info for a branch node
// IDEA: Within a loop test expression, distinguish for CSELECT between
//       first and later trips

#define fb_opr_cases_branch \
  case OPR_TRUEBR:          \
  case OPR_FALSEBR:         \
  case OPR_IF:              \
  case OPR_CSELECT

inline bool FB_valid_opr_branch(const WN *wn) {
  OPERATOR opr = WN_operator( wn );
  switch ( opr ) {
  fb_opr_cases_branch:
    return true;
  default:
    return false;
  }
}


// ====================================================================

#define fb_opr_cases_loop \
  case OPR_DO_LOOP:       \
  case OPR_WHILE_DO:      \
  case OPR_DO_WHILE

inline bool FB_valid_opr_loop(const WN *wn) {
  OPERATOR opr = WN_operator( wn );
  switch ( opr ) {
  fb_opr_cases_loop:
    return true;
  default:
    return false;
  }
}

#ifdef KEY
// ====================================================================

#define fb_opr_cases_value \
  case OPR_DIV:            \
  case OPR_REM:            \
  case OPR_MOD
#define fb_opr_cases_value_fp_bin \
  case OPR_MPY

inline bool FB_valid_opr_value(const WN *wn) {
  OPERATOR opr = WN_operator( wn );
  switch ( opr ) {
  fb_opr_cases_value:
    return true;
  fb_opr_cases_value_fp_bin:
    return true;
  default:
    return false;
  }
}
#endif

// ====================================================================

// Feedback info for OPR_CAND and/or OPR_CIOR
// IDEA: Within a loop test expression, distinguish between first and later
//       trips

#define fb_opr_cases_circuit \
  case OPR_CAND:             \
  case OPR_CIOR

inline bool FB_valid_opr_circuit(const WN *wn) {
  OPERATOR opr = WN_operator( wn );
  switch ( opr ) {
  fb_opr_cases_circuit:
    return true;
  default:
    return false;
  }
}


// ====================================================================
// Feedback info for a call

#define fb_opr_cases_call  \
  case OPR_PICCALL:        \
  case OPR_CALL:           \
  case OPR_ICALL:          \
  case OPR_INTRINSIC_CALL: \
  case OPR_IO
// case OPR_REGION:

inline bool FB_valid_opr_call(const WN *wn) {
  OPERATOR opr = WN_operator( wn );
  switch ( opr ) {
  fb_opr_cases_call:
    return true;
  default:
    return false;
  }
}


// ====================================================================

// Feedback info for a OPR_SWITCH or OPR_XGOTO or OPR_SWITCH
// A frequency count is given for each possible jump target

#define fb_opr_cases_switch \
  case OPR_SWITCH:          \
  case OPR_COMPGOTO:        \
  case OPR_XGOTO

inline bool FB_valid_opr_switch (const WN *wn) {
  OPERATOR opr = WN_operator( wn );
  switch ( opr ) {
  fb_opr_cases_switch:
    return true;
  default:
    return false;
  }
}

#endif // MONGOOSE_BE
// ====================================================================

#endif
