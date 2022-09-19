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


// isa_pseudo_gen.h
/////////////////////////////////////
//
//  Interface for pseudo instructions.
//
//  void ISA_Pseudo_Begin(const char* archname)
//      Initialize to generate pseudo instruction translation information 
//	for the architecture with the given <archname>.  The information 
//	will be written to the files targ_isa_pseudo.[ch].  
//
//  void ISA_Pseudo_End(void)
//      Complete processing of pseudo translation information.
//
//  void Machine_To_Pseudo(TOP pseudo, TOP machine)
//      Starts the definition of a machine-instruction to pseudo-
//	instruction translation. These definitions are used
//	during disassembly.
//
//	<pseudo> and <machine> specify the topcodes for the pseudo
//	and machine instructions.
//
//	Following the Machine_To_Pseudo() call may be a number of Require() 
//	calls to constrain the operands of this particular translation 
//	(there can be multiple Machine_To_Pseudo() calls with the same 
//	<machine> operand provided the operand constraints are unique).
//
//	Also following the Machine_To_Pseudo() call may be a number of 
//	Map_Arg() calls are used to specify how the instruction operands
//	are handled when a translation occurs.
//
//  void Pseudo_To_Machine(TOP machine, TOP pseudo)
//	Starts the definition of a pseudo-instruction to machine-
//	instruction translation. These definitions are used during
//	lowering before final code emission.
//
//	<pseudo> and <machine> specify the topcodes for the pseudo
//	and machine instructions.
//
//	Following the Machine_To_Pseudo() call may be a number of 
//	Map_Arg() calls are used to specify how the instruction operands
//	are handled when a translation occurs.
//
//  void Require(const char *bool_expr)
//	Adds a requirement on the operands/results of the machine instruction
//	in a machine-to-pseudo translation.
//
//	<bool_expr> is a C expression, specified as a string, that 
//	results in a boolean value. This expression will be evaluated 
//	at run-time and must be true for the machine instruction to be
//	translated to the current pseudo instruction. Multiple requirements 
//	for a given machine instruction can be specified by making multiple 
//	Require() calls. The tests will be performed in the order they appear
//	in the specification.
//
//	As noted, the C expression must result in a boolean value.
//	In addition to using numeric constants and builtin operators,
//	the macros OPND(int n) and RESULT(int n) may be used to
//	refer to the actual operand and result values at run time.
//
//  void Map_Arg(const char *lvalue, const  char *expr)
//	If a machine-to-pseudo or pseudo-to-machine translation will
//	occur, then the information supplied by this function
//	is used to map the results/operands of the input instruction to
//	the output instruction. The effect of Map_Arg is to perform
//	the assignment: <lvalue> = <expr>.
//
//	<lvalue> is a C l-value, specified as a string, that identifies
//	the result/operand of the output instruction. The OPND(int n)
//	and RESULT(int n) macro in this context identify operands and
//	results of the input OP.
//
//	<expr> is a C expression, specified as a string, that will become 
//	the value of the result/operand specified by <lvalue>. The 
//	OPND(int n) and RESULT(int n) macro in this context identify 
//	operands and results of the input instruction.
//
/////////////////////////////////////

//  $Revision: 1.1.1.1 $
//  $Date: 2005/10/21 19:00:00 $
//  $Author: marcel $
//  $Source: /proj/osprey/CVS/open64/osprey1.0/common/targ_info/generate/isa_pseudo_gen.h,v $

#ifndef isa_pseudo_gen_INCLUDED
#define isa_pseudo_gen_INCLUDED

#ifdef _KEEP_RCS_ID
static const char isa_pseudo_gen_rcs_id[] = "$Source: /proj/osprey/CVS/open64/osprey1.0/common/targ_info/generate/isa_pseudo_gen.h,v $ $Revision: 1.1.1.1 $";
#endif /* _KEEP_RCS_ID */

#ifdef __cplusplus
extern "C" {
#endif

extern void ISA_Pseudo_Begin(const char* archname);
extern void Machine_To_Pseudo(TOP pseudo, TOP machine);
extern void Pseudo_To_Machine(TOP machine, TOP pseudo);
extern void Require(const char *bool_expr);
extern void Map_Arg(const char *lvalue, const  char *expr);
extern void ISA_Pseudo_End(void);

#ifdef __cplusplus
}
#endif
#endif /* isa_pseudo_gen_INCLUDED */
