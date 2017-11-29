/********************************************************************\
|*                                                                  *|   
|*  Copyright (c) 2006 by SimpLight Nanoelectronics.                *|
|*  All rights reserved                                             *|
|*                                                                  *|
|*  This program is free software; you can redistribute it and/or   *|
|*  modify it under the terms of the GNU General Public License as  *|
|*  published by the Free Software Foundation; either version 2,    *|
|*  or (at your option) any later version.                          *|
|*                                                                  *|
\********************************************************************/

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
  ret_addr,
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
  hi_reg,
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
  addrsize7;
#endif

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
#if defined(TARG_SL)
	       24,  25,                     30,  31,// 27 is used temporary for arm debugging aid
#else
		24,  25,                      30,  31,
#endif
	       -1);
  Reg_Property(callee, ISA_REGISTER_CLASS_integer,
                16,  17,  18,  19,  20,  21,  22,  23,
	                                      30,
	       -1);
  Reg_Property(caller, ISA_REGISTER_CLASS_integer,
		      1,   2,   3,   4,   5,   6,   7,  
		 8,   9,  10,  11,  12,  13,  14,  15,
#if defined(TARG_SL)                
	       24,  25,            		31,// 27 is used temporary for arm debugging aid
#else
	       24,  25,         		31,
#endif	   
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
#if defined(TARG_SL)
  Reg_Property(tmp1, ISA_REGISTER_CLASS_integer, 
		                     26,
	       -1);
  Reg_Property(tmp2, ISA_REGISTER_CLASS_integer, 
		                     27,
	       -1);
#endif
  Reg_Property(stack_ptr, ISA_REGISTER_CLASS_integer, 
		                        29,
	       -1);
  Reg_Property(frame_ptr, ISA_REGISTER_CLASS_integer, 
		                            30,
	       -1);
#if defined(TARG_SL)
  Reg_Property(jump_addr, ISA_REGISTER_CLASS_control,
                                            0,
               -1);
  Reg_Property(ret_addr, ISA_REGISTER_CLASS_control,
                                            1,
               -1);

  Reg_Property(loop_count, ISA_REGISTER_CLASS_loop,
	       0, 1, 2, 3,
	       -1);

  Reg_Property(loop_count, ISA_REGISTER_CLASS_accum,
	       0, 1, 2, 3,
	       -1);

  Reg_Property(loop_count, ISA_REGISTER_CLASS_addr,
	       0, 1, 2, 3, 4, 5, 6, 7,
	       -1);
  Reg_Property(loopcount0, ISA_REGISTER_CLASS_control,
               2,
               -1);
  Reg_Property(loopcount1, ISA_REGISTER_CLASS_control,
               3,
               -1);
  Reg_Property(loopcount2, ISA_REGISTER_CLASS_control,
               4,
               -1);
  Reg_Property(loopcount3, ISA_REGISTER_CLASS_control,
               5,
               -1);
  Reg_Property(hi_reg, ISA_REGISTER_CLASS_special,
               20,
               -1);
#else
  Reg_Property(ret_addr, ISA_REGISTER_CLASS_integer, 
		                                31,
	       -1);
#endif

#ifdef TARG_SL2
  Reg_Property(c2acc, ISA_REGISTER_CLASS_c2accum,
               1,
               -1);
  Reg_Property(c2cond, ISA_REGISTER_CLASS_c2cond,
               1,
               -1);
  Reg_Property(c2mvsel, ISA_REGISTER_CLASS_c2mvsel,
               1,
               -1);
  Reg_Property(c2vlcs, ISA_REGISTER_CLASS_c2vlcs,
               1,
               -1);
  Reg_Property(c2movpat, ISA_REGISTER_CLASS_c2movpat,
               1,
               -1);
#endif

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
#if defined(TARG_SL)  

  Reg_Property(acc0, ISA_REGISTER_CLASS_accum,
                      0,
                      -1);
  Reg_Property(acc1, ISA_REGISTER_CLASS_accum,
                      1,
                      -1);
  Reg_Property(acc2, ISA_REGISTER_CLASS_accum,
                      2,
                      -1);
  Reg_Property(acc3, ISA_REGISTER_CLASS_accum,
                      3,
                      -1);
  Reg_Property(add0, ISA_REGISTER_CLASS_addr,
                      0,
                      -1);
  Reg_Property(add1, ISA_REGISTER_CLASS_addr,
                      1,
                      -1);
  Reg_Property(add2, ISA_REGISTER_CLASS_addr,
                      2,
                      -1);
  Reg_Property(add3, ISA_REGISTER_CLASS_addr,
                      3,
                      -1);
  Reg_Property(add4, ISA_REGISTER_CLASS_addr,
                      4,
                      -1);
  Reg_Property(add5, ISA_REGISTER_CLASS_addr,
                      5,
                      -1);
  Reg_Property(add6, ISA_REGISTER_CLASS_addr,
                      6,
                      -1);
  Reg_Property(add7, ISA_REGISTER_CLASS_addr,
                      7,
                      -1);
  Reg_Property(addrsize0, ISA_REGISTER_CLASS_addr_size,
                      0,
                      -1);
  Reg_Property(addrsize1, ISA_REGISTER_CLASS_addr_size,
                      1,
                      -1);
  Reg_Property(addrsize2, ISA_REGISTER_CLASS_addr_size,
                      2,
                      -1);
  Reg_Property(addrsize3, ISA_REGISTER_CLASS_addr_size,
                      3,
                      -1);
  Reg_Property(addrsize4, ISA_REGISTER_CLASS_addr_size,
                      4,
                      -1);
  Reg_Property(addrsize5, ISA_REGISTER_CLASS_addr_size,
                      5,
                      -1);
  Reg_Property(addrsize6, ISA_REGISTER_CLASS_addr_size,
                      6,
                      -1);
  Reg_Property(addrsize7, ISA_REGISTER_CLASS_addr_size,
                      7,
                      -1);
#endif
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
  hi_reg = Create_Reg_Property("hi_reg");
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
  Begin_ABI("n32");
  n64_abi();
  
  Begin_ABI("n64");
  n64_abi();

  ABI_Properties_End();
}
