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
//  $Revision: 1.1 $
//  $Date: 2005/07/27 02:18:12 $
//  $Author: kevinlo $
//  $Source: /depot/CVSROOT/javi/src/sw/cmplr/common/targ_info/isa/pr1/isa.cxx,v $

#include <stddef.h>
#include "isa_gen.h"

main ()
{
  ISA_Create ("pr1", 
	"add2",
	"addi2",
	"addiu2",
	"addu2",
	"add3",
	"addi3",
	"addiu3",
	"addu3",
	"addlc3",
	"addgp3",
	"addsp3",
	"addra3",
	"and2",
	"andi2",
	"and3",
	"andi3",
	"b",
	"b10",
	"bcall",
	"bcall10",
	"beq",
	"beqpt",
	"beqpf",
	"bgt",
	"bgtpt",
	"bgtpf",
	"bge",
	"bgept",
	"bgepf",
	"bloop",
	"bloopsz",
	"bne",
	"bnept",
	"bnepf",
	"beqi",
	"beqipf",
	"beqipt",
	"bgti",
	"bgtipf",
	"bgtipt",
	"blti",
	"bltipf",
	"bltipt",
	"bgei",
	"bgeipf",
	"bgeipt",
	"blei",
	"bleipf",
	"bleipt",
	"bnei",
	"bneipf",
	"bneipt",
	"bret",
	"bit_l0",
	"bit_l1",
	"bit_sext",
	"bit_zext",
	"copr_0",
	"copr_1",
	"copr_2",
	"copr_3",
	"copr_4",
	"copr_5",
	"copr_6",
	"copr_7",
	"drr",
	"dep",
	"depi",
	"depu",
	"depiu",
	"extru",
	"extriu",
	"extr",
	"extri",
	"div2",
	"divi2",
	"diviu2",
	"divu2",
	"div3",
	"divi3",
	"diviu3",
	"divu3",
	"j",
	"jr",
	"call",
	"icall",
	"ret",
	"ldb",
	"ldh",
	"ldw",
	"ldb2",
	"ldh2",
	"ldw2",
	"ldbi",
	"ldhi",
	"ldwi",
	"ldwgp2",
	"ldbsp2",
	"ldhsp2",
	"ldwsp2",
	"ldwgp3",
	"ldbsp3",
	"ldhsp3",
	"ldwsp3",
	"movlc",
	"movgp",
	"movsp",
	"movra",
	"mov2lc",
	"mov2gp",
	"mov2sp",
	"mov2ra",
	"movi2lc",
	"movi2gp",
	"movi2sp",
	"movi2ra",
	"movi",
	"movt",
	"movf",
	"movti",
	"movfi",
	"movtn",
	"movfn",
	"movtin",
	"movfin",
	"mul2",
	"muli2",
	"muliu2",
	"mulu2",
	"mul3",
	"muli3",
	"mulu3",
	"muliu3",
	"not2",
	"noti2",
	"nop2",
	"nop3",
	"or2",
	"ori2",
	"or3",
	"ori3",
	"pack16",
	"pack32",
	"packu16",
	"packu32",
	"upack16",
	"upack32",
	"upacku16",
	"upacku32",
	      //	"recp2",
	      //	"recpi2",
	      //	"recpu2",
	      //	"recp3",
	      //	"recpi3",
	      //	"recpu3",
	"shl2",
	"shli2",
	"shl3",
	"shli3",
	"shr2",
	"shri2",
	"shr3",
	"shri3",
	"shlad",
	"shladgp",
	"shladsp",
	"stb",
	"sth",
	"stw",
	"stb2",
	"sth2",
	"stw2",
	"stbi",
	"sthi",
	"stwi",
	"stwgp2",
	"stbsp2",
	"sthsp2",
	"stwsp2",
	"stwgp3",
	"stbsp3",
	"sthsp3",
	"stwsp3",
	"sub2",
	"subi2",
	"subiu2",
	"subu2",
	"sub3",
	"subi3",
	"subiu3",
	"subu3",
	"sys.brk",
	"sys.swi",
	"sys.reti",
	"sys.pwr",
	"teq2",
	"teqi2",
	"tge2",
	"tgei2",
	"tgem2",
	"tgemi2",
	"tgt2",
	"tgtm2",
	"tgti2",
	"tgtmi2",
	"tne2",
	"tnei2",
	"tnem2",
	"teq3",
	"teqi3",
	"tle3",
	"tlem3",
	"tlei3",
	"tlemi3",
	"tlt3",
	"tltm3",
	"tlti3",
	"tltmi3",
	"tge3",
	"tgei3",
	"tgem3",
	"tgemi3",
	"tgt3",
	"tgti3",
	"tgtm3",
	"tgtmi3",
	"tne3",
	"tnei3",
	"tnem3",
	"tnemi3",
	"xor2",
	"xori2",
	"xor3",
	"xori3",

      // Simulated instructions
//      "gotdisp",
      "asm",
      "intrncall",
      "spadjust",
//      "copy_br",

      // Dummy instructions
      "begin_pregtn",
      "end_pregtn",
      "bwd_bar",
      "fwd_bar",
      "label",
      "noop",

      NULL);
}
