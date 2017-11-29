/*
 * Copyright (C) 2010 Advanced Micro Devices, Inc.  All Rights Reserved.
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


// si_gen.h
/////////////////////////////////////
//
//  Interface to hardware scheduling information generation library.
//  This library is used to describe of a processor's various scheduling
//  properties, primarily latencies and resources.  Once a machine description
//  had been written, it can be used to generate a machine readable machine
//  description in one of a number of different formats, including one
//  readable by the mongoose compiler and one readable by mixie. (Others?)
//
//  In order to describe your machine you write a C++ program (could be C, but
//  then I couldn't use end of line comments in this header) that does the
//  following:
//
//  1. Initialize
//  =============
//
//      This is done by including this header file and by calling:
//
//          void Targ_SI( );
//              Perform one-time initializtion.
//
//          void Machine( const char* name, ISA_SUBSET isa )
//              <name> is a name you wish to give to the processor being
//              described. <isa> is the ISA for the machine. <slot> is 
//              is the index in the global si_machines array.
//
//  2. Describe the fundamental resources
//  =====================================
//
//      Resources are represented by the abstract type RES, which has no
//      client visible fields.  These are created by the function:
//
//          RESOURCE RESOURCE_Create( const char* name, int count )
//              Creates a resource with <count> instances available per
//              machine cycle.  <name> is given to the resource for debugging
//              and documentation purposes.
//
//  3. Describe the skew rules, if any
//  ==================================
//
//      Beast has a "skewed pipe" which allows some instructions to be issued
//      one cycle earlier than they execute.  If your machine does not have
//      such a feature, you can ignore this section.  To accomadate this
//      feature, we support a concept of multiple issue slots in an instrution
//      word.  The issue slots are ordered and the rule is that all the
//      instructions assigned to earlier issue slots must appear before those
//      assigned to later issue slots.  For beast, the slots are A-stage and
//      E-stage, but we actually order E-stage before A-stage to force the
//      E-stage instructions to be grouped with the A-stage instruction from
//      the previous instruction.  (Earl, this is a very scheduler-centric
//      description.  Do you want to offer some more simulator friendly
//      language?)
//
//      An abstract type, ISSUE_SLOT, is used to force instructions to be
//      grouped together:
//
//          ISSUE_SLOT ISSUE_SLOT_Create( const char* name,
//                                        int skew, int count )
//              Creates an ISSUE_SLOT giving it the given <name> for
//              documentation and debugging purposes.  ISSUE_SLOTs must be
//              created in the order in which the operations assigned to them
//              should be emitted.  <skew> gives a number of cycles to add to
//              the access time of operand, available time of result.  <count>
//              gives the maximum number of instructions that may occupy this
//              issue slot.  A count of 0 means that an unlimited number of
//              instructions may be issued in the slot.
//
//  4. Describe the instruction groups
//  ==================================
//
//      An instruction group is a set of opcodes with identical scheduling
//      properties.  The description of each instruction group begins with a
//      call to:
//
//          void Instruction_Group( const char* name,
//                                  TOP top,...,TOP_UNDEFINED)
//              Start a description of a new instruction group that includes
//              all the given <topi>, each of which should be the symbolic
//              name for an opcode.  Notice the TOP_UNDEFINED, which
//              terminates the list and must be the last argument of this
//              function.  This is called the "current instruction group" and
//              remains current until the next call to Instruction_Group (or
//              the call to Machine_Done).  <name> is used only for
//              documentation and debugging.
//
//      Latencies are described by giving an access time for the operand(s)
//      and an available time for the for the result(s) of the members of an
//      instruction group.  If I1 defines a register which I2 uses, I2 will
//      execute at least:
//
//          Max(Available_Time(I2) - Access_Time(I1),0)
//
//      after I1.  (But I2 must occur after I1 in the instruction stream in
//      any case.)
//
//      Both operand access time and result available times can be defined in
//      one of two styles.  A single access time can apply to any operand or
//      specific operands can be given specific access times (similarly for
//      results).  This allows instructions with different numbers of operands
//      to be described in the same instruction group so long as any operands
//      all have the same access time.  Here are the access and available time
//      functions:
//
//          void Any_Operand_Access_Time( int time )
//              All the operands of the instructions in the current
//              instruction group are be accessed at the given <time>
//              relative to execution.
//
//          void Operand_Access_Time( int operand_index, int time )
//              The <operand_index>'th operand of the current instruction
//              group access their operands at the given <time> relative to
//              execution.  This does not imply that all the instructions in
//              the current group actually have this many operands, only that
//              those with enough operands have this access time.
//
//          void Any_Result_Available_Time( int time )
//              Similar to Any_Operand_Access_Time, but specifies time the
//              results are available.
//
//          void Result_Available_Time( int result_index, int time )
//              Similar to Operand_Access_Time, but specifies time the
//              results are available.
//
//          void Store_Available_Time( int time )
//              For memory stores, gives the time the stored value is
//              available in memory.
//
//          void Load_Access_Time( int time )
//              For memory loads, gives the time the loaded value is accessed
//              from memory.
//              
//	    void Last_Issue_Cycle( int time )
//		For simulated instructions, the cycle that the last
//		instruction of the sequence is issued. Non-simulated
//		instructions always have a last-issue-cycle of 0,
//		which is the default.
//
//      Resource requirements are described by calls to:
//
//          void Resource_Requirement( RESOURCE resource, int time )
//              The members of the current instruction group use one instance
//              of <resource> at the given <time> relative to execution.
//              Write loops and functions to decribe complex resource use
//              patterns (you are writing in C++ after all).
//
//      For machines with exposed issue slots (or skewed pipes), the following
//      exist to control assignment of instructions to issue slots:
//
//          void Valid_Issue_Slot( ISSUE_SLOT slot )
//              The members of the current instruction group may be assigned
//              to the given <slot>.  More than one valid issue slot may be
//              specified for an instruction group, or if no valid issue slot
//              is specified, the instructions can issue without a skew if
//              their resource restrictions are satisfied.  In other words,
//              don't have to specify a valid issue slot if it really isn't an
//              problem for this group of instructions.
//
//      Some miscellany exists:
//
//          void Write_Write_Interlock()
//              The members of the current instruction group incur a stall
//              when trying to write to a register which has already been
//              written to but not yet defined.  (Earl, is this right?)
//
//          (More to come for the compiler)
//          
//  5. Finalize the machine description
//  ===================================
//
//      After all the instruction groups have been described, call:
//
//          void Machine_Done( )
//              Finalizes the current machine description.  This call may be
//              followed by any number of Machine/Machine_Done pairs to specify
//              descriptions for more than one machine.
//
//          void Targ_SI_Done( const char* basename )
//              Write out all machine descriptions to a C source file and
//              header file derived from <basename>.  The source filename is
//              constructed by appending ".c", and the header, ".h".
//
/////////////////////////////////////


//  $Revision: 1.1.1.1 $
//  $Date: 2005/10/21 19:00:00 $
//  $Author: marcel $
//  $Source: /proj/osprey/CVS/open64/osprey1.0/common/targ_info/generate/si_gen.h,v $


#ifndef SI_GEN_INCLUDED
#define SI_GEN_INCLUDED

#include "targ_isa_subset.h"

typedef class RES* RESOURCE;
typedef class ISLOT* ISSUE_SLOT;

extern void Targ_SI( void );
extern void Machine( const char* name, ISA_SUBSET isa );
extern RESOURCE RESOURCE_Create( const char* name, int count );
extern ISSUE_SLOT ISSUE_SLOT_Create( const char* name, int skew, int count );
extern void Instruction_Group( const char* name, ... );
extern void Any_Operand_Access_Time( int time );
extern void Operand_Access_Time( int operand_index, int time );
extern void Any_Result_Available_Time( int time );
extern void Result_Available_Time( int result_index, int time );
extern void Store_Available_Time( int time );
extern void Load_Access_Time( int time );
extern void Last_Issue_Cycle( int time );
extern void Alternative_Resource_Requirement( RESOURCE resource, int time );
extern void Resource_Requirement( RESOURCE resource, int time );
extern void Valid_Issue_Slot( ISSUE_SLOT slot );
extern void Write_Write_Interlock( void );
extern void Machine_Done( void );
extern void Targ_SI_Done( const char* basename );

#endif
