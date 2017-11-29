/*

  Copyright (C) 2006-2009 Tsinghua University.  All Rights Reserved.
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
// Generate an ISA containing the instructions specified.
/////////////////////////////////////////////////////////
// The instructions are listed by category. The different categories of
// instructions are:
//
//   1. Integer instructions
//   2. FP instructions
//   3. Simulated instructions
//   4. Dummy instructions
//
// Within each category, the instructions are in alphabetical order.
// This arrangement of instructions matches the order in the ISA manual.
/////////////////////////////////////
//
//  $Revision: 1.11 $
//  $Date: 2006/04/28 08:27:15 $
//  $Author: weitang $
//  $Source: /depot/CVSROOT/javi/src/sw/cmplr/common/targ_info/isa/MIPS/isa.cxx,v $

#include <stddef.h>
#include "isa_gen.h"

main ()
{
  ISA_Create ("PPC", 	
	"lbz",
	"lbzu",
	"lhz",
	"lhzu",
	"lha",
	"lhau",
	"lwz",
	"lwzu",
	
	"stb",
	"stbu",
	"sth",
	"sthu",
	"stw",
	"stwu",

  	"lbzx",  
	"lbzux",
	"lhzx",
	"lhzux",
	"lhax",
	"lhaux",
	"lwzx",
	"lwzux",
  	
	"stbx",
	"stbux",
	"sthx",
	"sthux",
	"stwx",
	"stwux",


  	"la",
	"li",
	"lis",

  
	"neg",
	"add",
	"addi",
	"addis",
  	"add.",
	"addic",
	"addc",
	"addco",
	"adde",
	"addeo",
	"addme",
	"addmeo",
	"addze",
	"addzeo",
	
	"subfco",
	"subfeo",
	"subfme",
	"subfmeo",
	"subfze",
	"subfzeo",

   	 "subf", 
    	"subfc",
    	"subfe",
    	"subfic",
    
	"divw",
    	"divwo",
    	"divwu",
    	"divwuo",
	"mullw",
	"mulli",   
    	"mullwo",
    	"mulhw",
    	"mulhwu",

	"cmpw",
	"cmpwi",
	"cmplwi",
	"cmplw",
	
	
	"andi.",
	"andc",
	"andis.",	
	"ori",
	"oris",
	"xori",
	"xoris",
    
    	"not",
    	"and",
    	"or",
    	"xor",
    	"nand",
    	"nor",
    	"eqv",
    	"orc",
    	"extsb",
    	"extsh",
    
	"rlwinm",
	"rlwimi",
	"rlwnm",
	
	"slwi",
	"slw",
	"srawi",
	"sraw",
	"srwi",
	"srw",
	
	// reletive branch
	"beq",   
	"bge", 
	"bgt", 
	"ble",  
	"blt", 
	
	"bnl", 
	"bne", 	
	"bng", 
	
	"bso", 
	"bns", 
	"bun", 
	"bnu", 
	
	// absoult branch
	"beqa",   
	"bgea", 
	"bgta", 
	"blea",  
	"blta", 
	
	"bnla", 
	"bnea", 	
	"bnga", 
	
	"bsoa", 
	"bnsa", 
	"buna", 
	"bnua", 
	
	// branch to lr
	"beqlr",   
	"bgelr", 
	"bgtlr", 
	"blelr",  
	"bltlr", 
	
	"bnllr", 
	"bnelr", 	
	"bnglr", 
	
	"bsolr", 
	"bnslr", 
	"bunlr", 
	"bnulr", 
	
	// branch to ctr
	"beqctr",   
	"bgectr", 
	"bgtctr", 
	"blectr",  
	"bltctr", 
	
	"bnlctr", 
	"bnectr", 	
	"bngctr", 
	
	"bsoctr", 
	"bnsctr", 
	"bunctr", 
	"bnuctr", 
	
	
	// with lr update
	// reletive branch
	"beql",   
	"bgel", 
	"bgtl", 
	"blel",  
	"bltl", 
	
	"bnll", 
	"bnel", 	
	"bngl", 
	
	"bsol", 
	"bnsl", 
	"bunl", 
	"bnul", 
	
	// absoult branch
	"beqla",   
	"bgela", 
	"bgtla", 
	"blela",  
	"bltla", 
	
	"bnlla", 
	"bnela", 	
	"bngla", 
	
	"bsola", 
	"bnsla", 
	"bunla", 
	"bnula", 
	
	// branch to lr
	"beqlrl",   
	"bgelrl", 
	"bgtlrl", 
	"blelrl",  
	"bltlrl", 
	
	"bnllrl", 
	"bnelrl", 	
	"bnglrl", 
	
	"bsolrl", 
	"bnslrl", 
	"bunlrl", 
	"bnulrl", 
	
	// branch to ctr
	"beqctrl",   
	"bgectrl", 
	"bgtctrl", 
	"blectrl",  
	"bltctrl", 
	
	"bnlctrl", 
	"bnectrl", 	
	"bngctrl", 
	
	"bsoctrl", 
	"bnsctrl", 
	"bunctrl", 
	"bnuctrl", 

  
	"b",
	"bl",
	"ba",
	"bla",
	"blrl",
	"blr",
	"bctr",
	"bctrl",
	
	"sc",
	
	"tweq",
	"tweqi",
	"twge",
	"twgei",
	"twlt",
	"twlti",
	"twne",
	"twnei",
	
	"lfs",
	"lfsx",
	"lfd",
	"lfdx",
	"stfs",
	"stfsx",
	"stfd",
	"stfdx",
	
	"fabs",
	"fnabs",
	"fadds",
	"fadd",
	
	
	"fdivs",
	"fdiv",
	"fmuls",
	"fmul",
	"fneg",
	"fsubs",
	"fsub",
	"fsqrts",
	"fsqrt",
	
	"fmadds",
	"fmadd",
	"fmsubs",
	"fmsub",
	
	"fnmadds",
	"fnmadd",
	"fnmsubs",
	"fnmsub",
	
	"fres",
	"frsqrte",
	
	"fcmpu",
	"fcmpo",
	
	"fsel",
	
	"mcrfs",
	"mtfsf",
	"mflr",
	"mtlr",
	"mfctr",
	"mtctr",
	"mtfsb0",
	"mtfsb1",
	"mr",
	"fmr",
	"mfcr",
	"mcrf",
	"mffs",
	
	"frsp",
	"fctiw",
	"fctiwz",

       // Condition Register Logical Instructions
       "crand",
       "cror",
       "crxor",
       "crnand",
       "crnor",
       "creqv",
       "crandc",
       "crorc",

    
    	// And pseudo-opcodes
    	"asm",
	"intrncall",
	"spadjust",
	"simaddi",

	// Dummy instructions
	"begin_pregtn",
	"end_pregtn",
	"bwd_bar",
	"fwd_bar",
	"label",
	      
	"nop",
	"noop",
	NULL);	
}
