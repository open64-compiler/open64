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

#include <stddef.h>
#include "isa_gen.h"

main()
{
    ISA_Create("loongson",
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

               "noop",

               // LOONGSON instructions
               "add",
               "addi",
               "addiu",
               "addu",
               "and",
               "andi",
               "beq",
               "bgez",
               "bgezal",
               "bgtz",
               "blez",
               "bltz",
               "bltzal",
               "bne",
               "dadd",
               "daddi",
               "daddiu",
               "daddu",
               "ddiv",
               "ddivu",
               "div",
               "divu",
               "dmult",
               "dmultu",
               "dsll",
               "dsllv",
               "dsra",
               "dsra32",
               "dsrav",
               "dsrl",
               "dsrl32",
               "dsrlv",
               "dsub",
               "dsubu",
               "j",
               "jal",
               "jalr",
               "jr",
               "lb",
               "lbu",
               "ld",
               "ldl",
               "ldr",
               "lh",
               "lhu",
               "ll",
               "lld",
               "lui",
               "lw",
               "lwl",
               "lwr",
               "lwu",
               "mfhi",
               "mflo",
               "mthi",
               "mtlo",
               "mult",
               "multu",
               "nor",
               "or",
               "ori",
               "sb",
               "sc",
               "scd",
               "sd",
               "sdl",
               "sdr",
               "sh",
               "sll",
               "sllv",
               "slt",
               "slti",
               "sltiu",
               "sltu",
               "sra",
               "srav",
               "srl",
               "srlv",
               "sub",
               "subu",
               "sw",
               "swl",
               "swr",
               "xor",
               "xori",
               "break",
               "nop",
               "abs.s",
               "add.s",
               "bc1f",
               "bc1fl",
               "bc1t",
               "bc1tl",
               "c.t.s",
               "c.or.s",
               "c.neq.s",
               "c.olg.s",
               "c.uge.s",
               "c.oge.s",
               "c.ugt.s",
               "c.ogt.s",
               "c.st.s",
               "c.gle.s",
               "c.sne.s",
               "c.gl.s",
               "c.nlt.s",
               "c.ge.s",
               "c.nle.s",
               "c.gt.s",
               "c.f.s",
               "c.un.s",
               "c.eq.s",
               "c.ueq.s",
               "c.olt.s",
               "c.ult.s",
               "c.ole.s",
               "c.ule.s",
               "c.sf.s",
               "c.ngle.s",
               "c.seq.s",
               "c.ngl.s",
               "c.lt.s",
               "c.nge.s",
               "c.le.s",
               "c.ngt.s",
               "ceil.l.s",
               "ceil.w.s",
               "cfc1",
               "ctc1",
               "cvt.l.s",
               "cvt.s.w",
               "cvt.s.l",
               "cvt.w.s",
               "div.s",
               "dmfc1",
               "dmtc1",
               "floor.l.s",
               "floor.w.s",
               "lwc1",
               "mfc1",
               "mov.s",
               "mtc1",
               "mul.s",
               "neg.s",
               "round.l.s",
               "round.w.s",
               "sub.s",
               "swc1",
               "trunc.l.s",
               "trunc.w.s",
               "abs.d",
               "add.d",
               "c.t.d",
               "c.or.d",
               "c.neq.d",
               "c.olg.d",
               "c.uge.d",
               "c.oge.d",
               "c.ugt.d",
               "c.ogt.d",
               "c.st.d",
               "c.gle.d",
               "c.sne.d",
               "c.gl.d",
               "c.nlt.d",
               "c.ge.d",
               "c.nle.d",
               "c.gt.d",
               "c.f.d",
               "c.un.d",
               "c.eq.d",
               "c.ueq.d",
               "c.olt.d",
               "c.ult.d",
               "c.ole.d",
               "c.ule.d",
               "c.sf.d",
               "c.ngle.d",
               "c.seq.d",
               "c.ngl.d",
               "c.lt.d",
               "c.nge.d",
               "c.le.d",
               "c.ngt.d",
               "ceil.l.d",
               "ceil.w.d",
               "cvt.l.d",
               "cvt.s.d",
               "cvt.w.d",
               "div.d",
               "floor.l.d",
               "floor.w.d",
               "mov.d",
               "mul.d",
               "neg.d",
               "round.l.d",
               "round.w.d",
               "sub.d",
               "trunc.l.d",
               "trunc.w.d",
               "cvt.d.s",
               "cvt.d.w",
               "cvt.d.l",
               "ldc1",
               "sdc1",
               "dsll32",
               "sqrt.s",
               "sqrt.d",
               "teq",
               "teqi",
               "tge",
               "tgei",
               "tgeiu",
               "tgeu",
               "madd.d",
               "madd.s",
               "msub.d",
               "msub.s",
               "nmadd.d",
               "nmadd.s",
               "nmsub.d",
               "nmsub.s",
               "movz",
               "movn",
               "divlo",
               "divhi",
               "divulo",
               "divuhi",
               "mult.g",
               "multu.g",
               "dmult.g",
               "dmultu.g",
               "div.g",
               "divu.g",
               "ddiv.g",
               "ddivu.g",
               "mod.g",
               "modu.g",
               "dmod.g",
               "dmodu.g",

               NULL);
}
