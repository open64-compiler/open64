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


// isa_decode_gen.h
/////////////////////////////////////
//
//  Interface for decoding instructions.
//
//  The decoding interface is specification is in the form of a state
//  machine. Each intermediate (non-final) state examines a bit field
//  of the instruction word and uses that to determine the next state.
//  When a final state is reached, that state will specify the topcode
//  for the instruction.
//
//  void ISA_Decode_Begin(const char* archname)
//      Initialize to generate instruction decoding information for the 
//      architecture with the given <archname>.  The information will be 
//	written to the files targ_isa_decode.[ch].  
//
//  void ISA_Decode_End(void)
//      Complete processing of decoding information.
//
//  TYPE STATE
//	This type holds the information about a state. It's content
//	are hidden from the client.
//
//  STATE Create_Unit_State(const char *tag, int pos, int width)
//  STATE Create_Inst_State(const char *tag, int idx, int pos, int width)
//	Create a intermediate (non-final) state. <tag> is a short string
//	to uniquely identify the state. The string is used as part of
//	an identifier, so it should only contain characters legal in an
//	identifier. Create_Unit_State identifies that this state
//	transitions based on bits in the execution unit. Create_Inst_State
//	identifies that this state transitions based on bits in the
//	instruction. <pos> and <width> specify a bit field in the 
//	instruction word or the execution unit that will determine the 
//	next state. <idx> determines what instruction word contains
//	the specified bit field.
//
//  STATE Final(TOP topcode)
//	Creates a final state. <topcode> specifies the resulting
//	topcode for the decode operation.
//
//  const int END_TRANSITIONS
//	Marks the end of a transition list. See the description for
//	Transitions() for details.
//
//  void Transitions(STATE state, ...)
//	Specifies the transitions for <state>. Each transition is
//	specified by the pair: int num, STATE newstate. <num> specifies
//	the transition number (corresponds to the bitfield specified
//	in Create_State). <newstate> is the state to move to when
//	the value of the bitfield matches the transition number.
//	The list of transitions is terminated by passing the constant
//	END_TRANSITIONS for <num>.
//
//  void Initial_State(STATE state)
//	This sets the initial state for the instruction decoder.
//
/////////////////////////////////////

//  $Revision: 1.1.1.1 $
//  $Date: 2005/10/21 19:00:00 $
//  $Author: marcel $
//  $Source: /proj/osprey/CVS/open64/osprey1.0/common/targ_info/generate/isa_decode_gen.h,v $

#ifndef isa_decode_gen_INCLUDED
#define isa_decode_gen_INCLUDED

#ifdef _KEEP_RCS_ID
static const char isa_decode_gen_rcs_id[] = "$Source: /proj/osprey/CVS/open64/osprey1.0/common/targ_info/generate/isa_decode_gen.h,v $ $Revision: 1.1.1.1 $";
#endif /* _KEEP_RCS_ID */

#ifdef __cplusplus
extern "C" {
#endif

extern void ISA_Decode_Begin(const char* archname);
extern void ISA_Decode_End(void);

typedef struct state *STATE;
enum { END_TRANSITIONS = -1 };
extern STATE Create_Unit_State(const char *tag, int pos, int width);
extern STATE Create_Inst_State(const char *tag, int idx, int pos, int width);
extern void Transitions(STATE state, ...);
extern void Initial_State(STATE state);
extern STATE Final(TOP topcode);

#ifdef __cplusplus
}
#endif
#endif /* isa_decode_gen_INCLUDED */
