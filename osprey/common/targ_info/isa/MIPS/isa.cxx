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
//  $Revision: 1.11 $
//  $Date: 2006/04/28 08:27:15 $
//  $Author: weitang $
//  $Source: /depot/CVSROOT/javi/src/sw/cmplr/common/targ_info/isa/MIPS/isa.cxx,v $

#include <stddef.h>
#include "isa_gen.h"

main ()
{
  ISA_Create ("MIPS",
        "j",
        "jal", 
	"lb",
	"lbu",
	"lh",
	"lhu",
	"lw",
	"lwl",
	"lwr",
	"sb",
	"sh",
	"sw",
	"swl",
	"swr",
	"ll",
	"sc",
	"sync",
	"lwu",
	"ld",
	"ldl",
	"ldr",
	"lld",
	"sd",
	"sdl",
	"sdr",
	"scd",
	"pref",
	"prefx",
	"add",
	"addi",
	"addiu",
	"addu",
	"div",
	"divu",
	"mult",
	"multu",
	"slt",
	"slti",
	"sltiu",
	"sltu",
	"sub",
	"subu",
	"dadd",
	"daddi",
	"daddiu",
	"daddu",
	"ddiv",
	"ddivu",
	"dmult",
	"dmultu",
	"dsub",
	"dsubu",
	"and",
	"andi",
	"lui",
	"nor",
	"or",
	"ori",
	"xor",
	"xori",
	"mfhi",
	"mflo",
	"mthi",
	"mtlo",
	"movf",
	"movn",
	"movt",
	"movz",
	"sllv",
	"sll",
	"srav",
	"sra",
	"srlv",
	"srl",
	"dsll",
	"dsll32",
	"dsllv",
	"dsra",
	"dsra32",
	"dsrav",
	"dsrl",
	"dsrl32",
	"dsrlv",
	"beq",
	"bgez",
	"bgezal",
	"bgtz",
	"blez",
	"bltz",
	"bltzal",
	"bne",
	//"j",
	//"jal",
	"jalr",
	"jr",
	"break",
	"syscall",
	"teq",
	"teqi",
	"tge",
	"tgei",
	"tgeiu",
	"tgeu",
	"tlt",
	"tlti",
	"tltiu",
	"tltu",
	"tne",
	"tnei",
	"lwc1",
	"ldc1",
	"lwxc1",
	"ldxc1",
	"swc1",
	"sdc1",
	"swxc1",
	"sdxc1",
	"abs.s",
	"abs.d",
	"add.s",
	"add.d",
	"c.f.s",
	"c.f.d",
	"c.t.s",
	"c.t.d",
	"c.un.s",
	"c.un.d",
	"c.or.s",
	"c.or.d",
	"c.eq.s",
	"c.eq.d",
	"c.neq.s",
	"c.neq.d",
	"c.ueq.s",
	"c.ueq.d",
	"c.olg.s",
	"c.olg.d",
	"c.olt.s",
	"c.olt.d",
	"c.uge.s",
	"c.uge.d",
	"c.ult.s",
	"c.ult.d",
	"c.oge.s",
	"c.oge.d",
	"c.ole.s",
	"c.ole.d",
	"c.ugt.s",
	"c.ugt.d",
	"c.ule.s",
	"c.ule.d",
	"c.ogt.s",
	"c.ogt.d",
	"c.sf.s",
	"c.sf.d",
	"c.st.s",
	"c.st.d",
	"c.ngle.s",
	"c.ngle.d",
	"c.gle.s",
	"c.gle.d",
	"c.seq.s",
	"c.seq.d",
	"c.sne.s",
	"c.sne.d",
	"c.ngl.s",
	"c.ngl.d",
	"c.gl.s",
	"c.gl.d",
	"c.lt.s",
	"c.lt.d",
	"c.nlt.s",
	"c.nlt.d",
	"c.nge.s",
	"c.nge.d",
	"c.ge.s",
	"c.ge.d",
	"c.le.s",
	"c.le.d",
	"c.nle.s",
	"c.nle.d",
	"c.ngt.s",
	"c.ngt.d",
	"c.gt.s",
	"c.gt.d",
	"div.s",
	"div.d",
	"mul.s",
	"mul.d",
	"neg.s",
	"neg.d",
	"sub.s",
	"sub.d",
	"sqrt.s",
	"sqrt.d",
	"madd.s",
	"madd.d",
	"msub.s",
	"msub.d",
	"nmadd.s",
	"nmadd.d",
	"nmsub.s",
	"nmsub.d",
	"recip.s",
	"recip.d",
	"rsqrt.s",
	"rsqrt.d",
	"cfc1",
	"ctc1",
	"mfc1",
	"mtc1",
	"dmfc1",
	"dmtc1",
	"mov.s",
	"mov.d",
	"movf.s",
	"movf.d",
	"movn.s",
	"movn.d",
	"movt.s",
	"movt.d",
	"movz.s",
	"movz.d",
	"cvt.d.s",
	"cvt.d.w",
	"cvt.d.l",
	"cvt.l.s",
	"cvt.l.d",
	"cvt.s.d",
	"cvt.s.w",
	"cvt.s.l",
	"cvt.w.s",
	"cvt.w.d",
	"ceil.w.s",
	"ceil.w.d",
	"ceil.l.s",
	"ceil.l.d",
	"floor.w.s",
	"floor.w.d",
	"floor.l.s",
	"floor.l.d",
	"round.w.s",
	"round.w.d",
	"round.l.s",
	"round.l.d",
	"trunc.w.s",
	"trunc.w.d",
	"trunc.l.s",
	"trunc.l.d",
	"bc1f",
	"bc1t",
     // And pseudo-opcodes
        "asm",
        "intrncall",
        "spadjust",

    // Dummy instructions
        "begin_pregtn",
        "end_pregtn",
        "bwd_bar",
        "fwd_bar",
        "label",

	"nop",
	"noop",
	//"add16"
	NULL);
}

