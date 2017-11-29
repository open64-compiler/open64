/*
 * Copyright 2003, 2004 PathScale, Inc.  All Rights Reserved.
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


//  
//  Generate ABI information
///////////////////////////////////////


//  $Revision: 1.3 $
//  $Date: 2006/06/12 08:36:37 $
//  $Author: elaineli $
//  $Source: /depot/CVSROOT/javi/src/sw/cmplr/common/targ_info/abi/MIPS/abi_properties.cxx,v $

#include <stddef.h>
#include "abi_properties_gen.h"
#include "targ_isa_registers.h"

static ABI_PROPERTY
  allocatable,
  callee,
  caller,
  func_arg,
  func_val,
  stack_ptr,
  frame_ptr,
  static_link,
  global_ptr,
  entry_ptr,
  zero,
  tmp1,
  tmp2,
#if defined(TARG_SL2)
  c2acc,
  c2cond,
  c2mvsel,
  c2vlcs,
  c2movpat,
#endif
#if defined(TARG_SL)  
  jump_addr,
  loop_count,
  accum,
  addr_reg,
  loopcount0,
  loopcount1,
  loopcount2,
  loopcount3,
  #endif
  hi_reg,
  lo_reg,
  #if defined(TARG_SL)
  acc0,
  acc1,
  acc2,
  acc3,
  add0,
  add1,
  add2,
  add3,
  add4,
  add5,
  add6,
  add7,
  addrsize0,
  addrsize1,
  addrsize2,
  addrsize3,
  addrsize4,
  addrsize5,
  addrsize6,
  addrsize7,
#endif
  ret_addr;


static void n64_abi(void)
{
  static const char *integer_names[32] = {
    NULL,   NULL,   NULL,   NULL,   NULL,   NULL,   NULL,   NULL,
    NULL,   NULL,   NULL,   NULL,   NULL,   NULL,   NULL,   NULL,
    NULL,   NULL,   NULL,   NULL,   NULL,   NULL,   NULL,   NULL,
    NULL,   "$t9",  NULL,   NULL,   "$gp",  "$sp",  "$fp",  NULL,
  };

  Reg_Names(ISA_REGISTER_CLASS_integer, 0, 31, integer_names);

  Reg_Property(allocatable, ISA_REGISTER_CLASS_integer,
		      1,   2,   3,   4,   5,   6,   7,  
		 8,   9,  10,  11,  12,  13,  14,  15,
                16,  17,  18,  19,  20,  21,  22,  23,                
                24,  25,                            30,						//26, 27 for kernel, 28 for gp, 29 for sp, 31 for ra
	       -1);
  Reg_Property(callee, ISA_REGISTER_CLASS_integer,
                16,  17,  18,  19,  20,  21,  22,  23,
	                                   28, 29,   30,
	       -1);
  Reg_Property(caller, ISA_REGISTER_CLASS_integer,
		      1,   2,   3,   4,   5,   6,   7,  
		 8,   9,  10,  11,  12,  13,  14,  15,

  		 24, 25,26, 27,                        31,
	       -1);
  Reg_Property(func_arg, ISA_REGISTER_CLASS_integer,
			             4,   5,   6,   7,  
		 8,   9,  10,  11, 
	       -1);
  Reg_Property(func_val, ISA_REGISTER_CLASS_integer,
			   2,   3, 
	       -1);
  Reg_Property(static_link, ISA_REGISTER_CLASS_integer,
			   2,   
	       -1);
  Reg_Property(global_ptr, ISA_REGISTER_CLASS_integer, 
		                    28,
	       -1);
  Reg_Property(entry_ptr, ISA_REGISTER_CLASS_integer, 
		                    25,
	       -1);
  Reg_Property(stack_ptr, ISA_REGISTER_CLASS_integer, 
		                        29,
	       -1);
  Reg_Property(frame_ptr, ISA_REGISTER_CLASS_integer, 
		                            30,
	       -1);
  Reg_Property(ret_addr, ISA_REGISTER_CLASS_integer, 
		                                31,
	       -1);


  Reg_Property(zero, ISA_REGISTER_CLASS_integer, 
		0,
	       -1);

  Reg_Property(allocatable, ISA_REGISTER_CLASS_float,
		 0,   1,   2,   3,   4,   5,   6,   7,  
		 8,   9,  10,  11,  12,  13,  14,  15,
                16,  17,  18,  19,  20,  21,  22,  23,
		24,  25,  26,  27,  28,  29,  30,  31,
	       -1);

  Reg_Property(callee, ISA_REGISTER_CLASS_float,
                                    20,       22, 
		24,       26,       28,       30,
	       -1);
  Reg_Property(caller, ISA_REGISTER_CLASS_float,
		 0,   1,   2,   3,   4,   5,   6,   7,  
		 8,   9,  10,  11,  12,  13,  14,  15,
                16,  17,  18,  19,       21,       23,
		     25,       27,       29,       31,
	       -1);
  Reg_Property(func_arg, ISA_REGISTER_CLASS_float,
		                    12,  13,  14,  15,
                16,  17,  18,  19, 
	       -1);
  Reg_Property(func_val, ISA_REGISTER_CLASS_float,
		 0,        2, 
	       -1);
  Reg_Property(allocatable, ISA_REGISTER_CLASS_fcc,
		 0,   1,   2,   3,   4,   5,   6,   7,  
	       -1);
  Reg_Property(callee, ISA_REGISTER_CLASS_fcc,
	       -1);
  Reg_Property(caller, ISA_REGISTER_CLASS_fcc,
		 0,   1,   2,   3,   4,   5,   6,   7,  
	       -1);

  Reg_Property(allocatable, ISA_REGISTER_CLASS_hilo,
	       -1);
  Reg_Property(callee, ISA_REGISTER_CLASS_hilo,
	       -1);
  Reg_Property(caller, ISA_REGISTER_CLASS_hilo,
	       -1);
 }

main()
{
  ABI_Properties_Begin("MIPS");

  allocatable = Create_Reg_Property("allocatable");
  callee = Create_Reg_Property("callee");
  caller = Create_Reg_Property("caller");
  func_arg = Create_Reg_Property("func_arg");
  func_val = Create_Reg_Property("func_val");
  stack_ptr = Create_Reg_Property("stack_ptr");
  frame_ptr = Create_Reg_Property("frame_ptr");
  static_link = Create_Reg_Property("static_link");
  global_ptr = Create_Reg_Property("global_ptr");
  entry_ptr = Create_Reg_Property("entry_ptr");
  zero = Create_Reg_Property("zero");
  tmp1 = Create_Reg_Property("tmp1");
  tmp2 = Create_Reg_Property("tmp2");
  ret_addr = Create_Reg_Property("ret_addr");
  #ifdef TARG_SL
  jump_addr = Create_Reg_Property("jump_addr"); 
  loop_count = Create_Reg_Property("loop_count");
  accum = Create_Reg_Property("accum");
  addr_reg = Create_Reg_Property("addr_reg");
  loopcount0 = Create_Reg_Property("loopcount0");
  loopcount1 = Create_Reg_Property("loopcount1");
  loopcount2 = Create_Reg_Property("loopcount2");
  loopcount3 = Create_Reg_Property("loopcount3");
#ifdef TARG_SL2
  c2acc = Create_Reg_Property("c2acc");
  c2cond = Create_Reg_Property("c2cond");
  c2mvsel = Create_Reg_Property("c2mvsel");
  c2vlcs = Create_Reg_Property("c2vlcs");
  c2movpat = Create_Reg_Property("c2movpat");
#endif
#endif
  hi_reg = Create_Reg_Property("hi_reg");
  lo_reg= Create_Reg_Property("lo_reg");
#ifdef TARG_SL
  acc0 = Create_Reg_Property("acc0");
  acc1 = Create_Reg_Property("acc1");
  acc2 = Create_Reg_Property("acc2");
  acc3 = Create_Reg_Property("acc3");
  add0 = Create_Reg_Property("add0");
  add1 = Create_Reg_Property("add1");
  add2 = Create_Reg_Property("add2");
  add3 = Create_Reg_Property("add3");
  add4 = Create_Reg_Property("add4");
  add5 = Create_Reg_Property("add5");
  add6 = Create_Reg_Property("add6");
  add7 = Create_Reg_Property("add7");
  addrsize0 = Create_Reg_Property("addrsize0");
  addrsize1 = Create_Reg_Property("addrsize1");
  addrsize2 = Create_Reg_Property("addrsize2");
  addrsize3 = Create_Reg_Property("addrsize3");
  addrsize4 = Create_Reg_Property("addrsize4");
  addrsize5 = Create_Reg_Property("addrsize5");
  addrsize6 = Create_Reg_Property("addrsize6");
  addrsize7 = Create_Reg_Property("addrsize7");
  #endif
  Begin_ABI("n32");
  n64_abi();
  
  Begin_ABI("n64");
  n64_abi();

  ABI_Properties_End();
}

