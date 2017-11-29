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


//  
//  Generate ABI information
///////////////////////////////////////


//  $Revision: 1.2 $
//  $Date: 2006/10/14 02:42:05 $
//  $Author: ylduan $
//  $Source: /home/cvsroot/godson/godson2_n32/src/osprey1.0/common/targ_info/abi/godson2/abi_properties.cxx,v $

#include <stddef.h>
#include "abi_properties_gen.h"
#include "targ_isa_registers.h"

static ABI_PROPERTY
	emptyprop,
	entry_ptr,
	allocatable,	
	callee,
	caller,
	stacked,
	func_arg,
	func_val,
	static_link,
	global_ptr,
	frame_ptr,
	stack_ptr,
	assembler_temporary,
	zero,
	fzero,
	fone,
	true_predicate,
	ret_addr,
	prev_funcstate,
	return_address,
	loop_count,
	epilog_count,
	hi,
	lo,
	fsr;

static void n32_abi(void)
{
  static const char *branch_names[8] = {
    "rp",   NULL,   NULL,   NULL,   NULL,   NULL,   NULL,   NULL,
  };

  Reg_Property(entry_ptr, ISA_REGISTER_CLASS_integer,  
  		25, -1);

  Reg_Property(allocatable, ISA_REGISTER_CLASS_integer,
			   1,  2,   3,   4,   5,   6,   7,  
		 8,   9,  10,  11,  12,  13,  14,  15,
		16,  17,  18,  19,  20,  21,  22,  23, 
		24,  25,  30,  31,
	       -1);
  Reg_Property(callee, ISA_REGISTER_CLASS_integer,
		16,   17,   18,   19,  20,  21,  22,  
		23,  28,  29,  30,
	       -1);
  Reg_Property(caller, ISA_REGISTER_CLASS_integer,
		 1,  2,   3,   4,   5,   6,   7,   8,   9,  
		 10,  11,  12,  13,  14,  15,  24,  25,  31,
	       -1);
   Reg_Property(stacked, ISA_REGISTER_CLASS_integer, 
	       -1);  
   
  Reg_Property(func_arg, ISA_REGISTER_CLASS_integer,
		4,  5,  6,  7,  8,  9,  10,  11,
	       -1);
  Reg_Property(func_val, ISA_REGISTER_CLASS_integer,
		 2,  3,
	       -1);
  Reg_Property(static_link, ISA_REGISTER_CLASS_integer, 
		12,
	       -1);
  Reg_Property(global_ptr, ISA_REGISTER_CLASS_integer, 
		28,
	       -1);
  Reg_Property(frame_ptr, ISA_REGISTER_CLASS_integer, 
		30,
	       -1);
  Reg_Property(stack_ptr, ISA_REGISTER_CLASS_integer, 
		29,
	       -1);
  Reg_Property(assembler_temporary, ISA_REGISTER_CLASS_integer, 
   		1,
	       -1);
  Reg_Property(zero, ISA_REGISTER_CLASS_integer, 
		0,
	       -1);
  Reg_Property(fzero, ISA_REGISTER_CLASS_integer,
   		-1);
  Reg_Property(fone, ISA_REGISTER_CLASS_integer,
   		-1);
  Reg_Property(true_predicate, ISA_REGISTER_CLASS_integer,
   		-1);
   
  Reg_Property(ret_addr, ISA_REGISTER_CLASS_integer, 
   		31, 
   		-1);
  Reg_Property(prev_funcstate, ISA_REGISTER_CLASS_integer,
   		-1);
  Reg_Property(return_address, ISA_REGISTER_CLASS_integer,
   		-1);
  Reg_Property(loop_count, ISA_REGISTER_CLASS_integer,
   		-1);

  Reg_Property(epilog_count, ISA_REGISTER_CLASS_integer,
   		-1);
   
  // float register class:

  Reg_Property(allocatable, ISA_REGISTER_CLASS_float,
		 0,  1,   2,   3,   4,   5,   6,   7,
		 8,   9,  10,  11,  12,  13,  14,  15,
		16,  17,  18,  19,  20,  21,  22,  23,
		24,  25,  26,  27,  28,  29,  30,  31,
	       -1);
  Reg_Property(callee, ISA_REGISTER_CLASS_float,
		20,  22,   24,  26,  28,  30, 
	       -1);
  Reg_Property(caller, ISA_REGISTER_CLASS_float,
		  0,  1,   2,   3,   4,   5,   6,   7,
		 8,   9,  10,  11,  12,  13,  14,  15,
		 16,  17,  18,  19,  21,  23,  25,  27,  29,  31,
	       -1);
  Reg_Property(func_arg, ISA_REGISTER_CLASS_float,
		 12,  13,  14,  15,  16,  17,  18,  19,
	       -1);
  Reg_Property(func_val, ISA_REGISTER_CLASS_float,
		 0,  2,
	       -1);
  Reg_Property(fzero, ISA_REGISTER_CLASS_float, 

	       -1);
  Reg_Property(fone, ISA_REGISTER_CLASS_float, 
	       -1);

  // predicate register class:
  Reg_Property(true_predicate, ISA_REGISTER_CLASS_predicate, 
	       0,
	       -1);

  // branch register class:


  // application register class:

  Reg_Property(prev_funcstate, ISA_REGISTER_CLASS_application, 
	       -1);
  Reg_Property(loop_count, ISA_REGISTER_CLASS_application, 
	       -1);
  Reg_Property(epilog_count, ISA_REGISTER_CLASS_application, 
	       -1);

  // control register class:
  // hilo register class:
  Reg_Property(hi, ISA_REGISTER_CLASS_hilo,  0,  -1);
  Reg_Property(lo, ISA_REGISTER_CLASS_hilo,  1,  -1);

  Reg_Property(fsr, ISA_REGISTER_CLASS_fpsr,  0,  -1);
  /* nothing! */
}

static void n64_abi(void)
{
  static const char *branch_names[8] = {
    "rp",   NULL,   NULL,   NULL,   NULL,   NULL,   NULL,   NULL,
  };

  Reg_Property(entry_ptr, ISA_REGISTER_CLASS_integer,  
  		25, -1);

  Reg_Property(allocatable, ISA_REGISTER_CLASS_integer,
			   1,  2,   3,   4,   5,   6,   7,  
		 8,   9,  10,  11,  12,  13,  14,  15,
		16,  17,  18,  19,  20,  21,  22,  23, 
		24,  25,  30,  31,
	       -1);
  Reg_Property(callee, ISA_REGISTER_CLASS_integer,
		16,   17,   18,   19,  20,  21,  22,  
		23,  28,  29,  30,
	       -1);
  Reg_Property(caller, ISA_REGISTER_CLASS_integer,
		 1,  2,   3,   4,   5,   6,   7,   8,   9,  
		 10,  11,  12,  13,  14,  15,  24,  25,  31,
	       -1);
  Reg_Property(stacked, ISA_REGISTER_CLASS_integer, 
	       -1);  
   
  Reg_Property(func_arg, ISA_REGISTER_CLASS_integer,
		4,  5,  6,  7,  8,  9,  10,  11,
	       -1);
  Reg_Property(func_val, ISA_REGISTER_CLASS_integer,
		 2,  3,
	       -1);
  Reg_Property(static_link, ISA_REGISTER_CLASS_integer, 
		12,
	       -1);
  Reg_Property(global_ptr, ISA_REGISTER_CLASS_integer, 
		28,
	       -1);
  Reg_Property(frame_ptr, ISA_REGISTER_CLASS_integer, 
		30,
	       -1);
  Reg_Property(stack_ptr, ISA_REGISTER_CLASS_integer, 
		29,
	       -1);
  Reg_Property(assembler_temporary, ISA_REGISTER_CLASS_integer, 
   		1,
	       -1);
  Reg_Property(zero, ISA_REGISTER_CLASS_integer, 
		0,
	       -1);
  Reg_Property(fzero, ISA_REGISTER_CLASS_integer,
	       -1);
  Reg_Property(fone, ISA_REGISTER_CLASS_integer,
	       -1);
  Reg_Property(true_predicate, ISA_REGISTER_CLASS_integer,
	       -1);
   
  Reg_Property(ret_addr, ISA_REGISTER_CLASS_integer, 
   		31, 
   		-1);
  Reg_Property(prev_funcstate, ISA_REGISTER_CLASS_integer,
   		-1);
  Reg_Property(return_address, ISA_REGISTER_CLASS_integer,
   		-1);
  Reg_Property(loop_count, ISA_REGISTER_CLASS_integer,
   		-1);

  Reg_Property(epilog_count, ISA_REGISTER_CLASS_integer,
   		-1);

  // float register class:

  Reg_Property(allocatable, ISA_REGISTER_CLASS_float,
		 0,  1,   2,   3,   4,   5,   6,   7,
		 8,   9,  10,  11,  12,  13,  14,  15,
		16,  17,  18,  19,  20,  21,  22,  23,
		24,  25,  26,  27,  28,  29,  30,  31,
	       -1);
  Reg_Property(callee, ISA_REGISTER_CLASS_float,
		24,  25,  26,  27,  28,  29,  30,  31, 
	       -1);
  Reg_Property(caller, ISA_REGISTER_CLASS_float,
		  0,  1,   2,   3,   4,   5,   6,   7,
		 8,   9,  10,  11,  12,  13,  14,  15,
		 16,  17,  18,  19,  20, 21,  22,  23, 
	       -1);
  Reg_Property(func_arg, ISA_REGISTER_CLASS_float,
		 12,  13,  14,  15,  16,  17,  18,  19,
	       -1);
  Reg_Property(func_val, ISA_REGISTER_CLASS_float,
		 0,  2,
	       -1);
  Reg_Property(fzero, ISA_REGISTER_CLASS_float, 

	       -1);
  Reg_Property(fone, ISA_REGISTER_CLASS_float, 
	       -1);

  // predicate register class:
  Reg_Property(true_predicate, ISA_REGISTER_CLASS_predicate, 
	       0,
	       -1);

  // branch register class:


  // application register class:

  Reg_Property(prev_funcstate, ISA_REGISTER_CLASS_application, 
	       -1);
  Reg_Property(loop_count, ISA_REGISTER_CLASS_application, 
	       -1);
  Reg_Property(epilog_count, ISA_REGISTER_CLASS_application, 
	       -1);

  // control register class:
  // hilo register class:
  Reg_Property(hi, ISA_REGISTER_CLASS_hilo,  0,  -1);
  Reg_Property(lo, ISA_REGISTER_CLASS_hilo,  1,  -1);

  Reg_Property(fsr, ISA_REGISTER_CLASS_fpsr,  0,  -1);
  /* nothing! */
}

main()
{
  ABI_Properties_Begin("loongson");

  emptyprop = Create_Reg_Property("emptyprop");
  entry_ptr = Create_Reg_Property("entry_ptr");
  allocatable = Create_Reg_Property("allocatable");
  callee = Create_Reg_Property("callee");
  caller = Create_Reg_Property("caller");
  stacked = Create_Reg_Property("stacked");
  func_arg = Create_Reg_Property("func_arg");
  func_val = Create_Reg_Property("func_val");
  static_link = Create_Reg_Property("static_link");
  global_ptr = Create_Reg_Property("global_ptr");
  frame_ptr = Create_Reg_Property("frame_ptr");
  stack_ptr = Create_Reg_Property("stack_ptr");
  assembler_temporary = Create_Reg_Property("assembler_temporary");
  zero = Create_Reg_Property("zero");
   fzero = Create_Reg_Property("fzero");
  fone = Create_Reg_Property("fone");
  true_predicate = Create_Reg_Property("true_predicate");
  ret_addr = Create_Reg_Property("ret_addr");
  prev_funcstate = Create_Reg_Property("prev_funcstate");
  return_address = Create_Reg_Property("return_address");
  loop_count = Create_Reg_Property("loop_count");
  epilog_count = Create_Reg_Property("epilog_count");
  hi = Create_Reg_Property("hi");
  lo = Create_Reg_Property("lo");
  fsr = Create_Reg_Property("fsr");
  

  ///////////////////////////////////////
  Begin_ABI("o32");
  n32_abi();

  ///////////////////////////////////////
  Begin_ABI("n32");
  n32_abi();

  ///////////////////////////////////////
  Begin_ABI("n64");
  n64_abi();

  ABI_Properties_End();
}
