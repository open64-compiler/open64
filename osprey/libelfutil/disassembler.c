/*

  Copyright (C) 2000, 2001 Silicon Graphics, Inc.  All Rights Reserved.

  This program is free software; you can redistribute it and/or modify it
  under the terms of version 2.1 of the GNU Lesser General Public License 
  as published by the Free Software Foundation.

  This program is distributed in the hope that it would be useful, but
  WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  

  Further, this software is distributed without any warranty that it is
  free of the rightful claim of any third person regarding infringement 
  or the like.  Any license provided herein, whether implied or 
  otherwise, applies only to this software file.  Patent licenses, if
  any, provided herein do not apply to combinations of this program with 
  other software, or any other product whatsoever.  

  You should have received a copy of the GNU Lesser General Public 
  License along with this program; if not, write the Free Software 
  Foundation, Inc., 59 Temple Place - Suite 330, Boston MA 02111-1307, 
  USA.

  Contact information:  Silicon Graphics, Inc., 1600 Amphitheatre Pky,
  Mountain View, CA 94043, or:

  http://www.sgi.com

  For further information regarding this notice, see:

  http://oss.sgi.com/projects/GenInfo/NoticeExplan

*/




/* MIPS instruction disassembler, callable from either Pascal or C */

/* to allow printing of the mad/msb instructions */
#define _MAD_MSB 1

#include <stdio.h>
#ifdef _BSD
#include <machine/inst.h>
#else
#include <sys/inst.h>
#endif
#include "cmplrs/newinst.h"
#include <disassembler.h>
#include <string.h>
#include "libelf.h"
#if defined(BUILD_OS_DARWIN)
#include "darwin_elf.h"
#else /* defined(BUILD_OS_DARWIN) */
#include <elf.h>
#endif /* defined(BUILD_OS_DARWIN) */

extern int dis_asm(char *, unsigned long long int);

#ifdef _64BIT_OBJECTS

#define ELF_ADDR	  Elf64_Addr
#define ELF_WORD	  Elf64_Word
#define F_DIS_INIT	  __dis_init64
#define F_REGISTER_NAME   __register_name64
#define F_FP_REGISTER_NAME __fp_register_name64
#define F_C0_REGISTER_NAME __co_register_name64
#define F_Vec_REGISTER_NAME __disassembler_vec_register_name64
#define F_DISASM	  __disasm64
#define F_DIS_REGS	  __dis_regs64
#define F_DISASSEMBLER    __disassembler64
#define DIS_REG_NAMES     dis_reg_names64

#define HEXREG		  "0x%llx"

#else

#define ELF_ADDR	  Elf32_Addr
#define ELF_WORD	  Elf32_Word
#define F_DIS_INIT	  __dis_init32
#define F_REGISTER_NAME   __register_name32
#define F_FP_REGISTER_NAME __fp_register_name32
#define F_C0_REGISTER_NAME __co_register_name32
#define F_Vec_REGISTER_NAME __disassembler_vec_register_name32
#define F_DISASM	  __disasm32
#define F_DIS_REGS	  __dis_regs32
#define F_DISASSEMBLER    __disassembler32
#define DIS_REG_NAMES     dis_reg_names32
#define HEXREG		  "0x%x"

#endif /* _64BIT_OBJECTS */

/* register definitions */
#define ZERO 	0
#define private	static

typedef int		boolean;
#define true	1
#define false	0

private char *c1fmt_name[16] = {
	"s",	"d",	"e",	"q",
	"w",	"l",	"ps",	"fmt7",
	"fmt8",	"fmt9",	"fmta",	"fmtb",
	"fmtc",	"fmtd",	"fmte",	"fmtf"
};

/* Remember the options set by F_DIS_INIT */
#ifdef _DIS_BUILD
#define ADDR64_DEFAULT	"%#llx\t"
#define VALUE64_DEFAULT	"%#llx\t"
#define NAME64_DEFAULT 	COMPILER_NAMES
#endif
#define ADDR_DEFAULT 	"%#010x\t"
#define VALUE_DEFAULT 	"%#010x\t"
#define NAME_DEFAULT 	COMPILER_NAMES

private struct {
	char *addr_format;
	char *value_format;
	char **reg_names;
	int print_jal_targets;
} save = {
		ADDR_DEFAULT,
		VALUE_DEFAULT,
		NAME_DEFAULT,
		true
    };


static char *op_name[64] = {
  "spec",  "bcond", "j",     "jal",   "beq",   "bne",   "blez",  "bgtz",
  "addi",  "addiu", "slti",  "sltiu", "andi",  "ori",   "xori",  "lui",
  "cop0",  "cop1",  "cop2",  "op13",  "beql",  "bnel",  "blezl", "bgtzl",
  "daddi", "daddiu","ldl",   "ldr",   "op1c",  "op1d",  "op1e",  "op1f",
  "lb",    "lh",    "lwl",   "lw",    "lbu",   "lhu",   "lwr",   "lwu",
  "sb",    "sh",    "swl",   "sw",    "sdl",   "sdr",   "swr",   "cache",
  "ll",    "lwc1",  "lwc2",  "pref",  "lld",   "ldc1",  "ldc2",  "ld",
  "sc",    "swc1",  "swc2",  "op3b",  "scd",   "sdc1",  "sdc2",  "sd" 
};
  
static char *spec_name[64] = {
  "sll",   "mov",   "srl",   "sra",   "sllv",  "spec05","srlv",  "srav",
  "jr",    "jalr",  "movz",  "movn",  "syscall","break","spim",  "sync",
  "mfhi",  "mthi",  "mflo",  "mtlo",  "dsllv","spec15","dsrlv",  "dsrav",
  "mult",  "multu", "div",   "divu",  "dmult", "dmultu","ddiv",  "ddivu",
  "add",   "addu",  "sub",   "subu",  "and",   "or",    "xor",   "nor",
  "spec28","spec29","slt",   "sltu",  "dadd",  "daddu", "dsub",  "dsubu",
  "tge",   "tgeu",  "tlt",   "tltu",  "teq",   "spec35","tne",   "spec37",
  "dsll",  "spec39","dsrl",  "dsra",  "dsll32", "spec3d","dsrl32", "dsra32" 
};

static char *bcond_name[32] = {
  "bltz",    "bgez",    "bltzl",   "bgezl",
  "spimi",   "bcond05", "bcond06", "bcond07",
  "tgei",    "tgeiu",   "tlti",    "tltiu",
  "teqi",    "bcond0d", "tnei",    "bcond0f",
  "bltzal",  "bgezal",  "bltzall", "bgezall",
  "bcond14", "bcond15", "bcond16", "bcond17",
  "bcond18", "bcond19", "bcond1a", "bcond1b",
  "bcond1c", "bcond1d", "bcond1e", "bcond1f" 
};

static char *cop1func_name[64] = {
  "add",   "sub",   "mul",   "div",   "sqrt",  "abs",   "mov",   "neg",
  "round.l", "trunc.l", "ceil.l", "floor.l", 
  "round.w", "trunc.w", "ceil.w", "floor.w",
  "fop10", "mov",   "movz",  "movn",  "fop14", "recip", "rsqrt", "fop17",
  "fop18", "fop19", "fop1a", "fop1b", "fop1c", "fop1d",	"fop1e", "fop1f",
  "cvt.s", "cvt.d", "cvt.e", "fop23", "cvt.w", "cvt.l",	"cvt.ps", "fop27",
  "cvt.s.pu", "fop29", "fop2a", "fop2b", "pll", "plu", "pul", "puu",
  "c.f",   "c.un",  "c.eq",  "c.ueq", "c.olt", "c.ult",	"c.ole", "c.ule",
  "c.sf",  "c.ngle","c.seq", "c.ngl", "c.lt",  "c.nge",	"c.le",  "c.ngt" 
};

static char *bc_name[32] = {
  "f",   "t",   "fl",  "tl",
  "x04", "x05", "x06", "x07",
  "x08", "x09", "x0a", "x0b",
  "x0c", "x0d", "x0e", "x0f",
  "x10", "x11", "x12", "x13",
  "x14", "x15", "x16", "x17",
  "x18", "x19", "x1a", "x1b",
  "x1c", "x1d", "x1e", "x1f" 
};

static char *c0func_name[64] = {
  "op00", "tlbr", "tlbwi","op03", "op04", "op05", "tlbwr","op07",
  "tlbp", "op9",  "op10", "op11", "op12", "op13", "op14", "op15",
  "rfe",  "op17", "op18", "op19", "op20", "op21", "op22", "op23",
  "eret", "op25", "op26", "op27", "op28", "op29", "op30", "op31",
  "op32", "op33", "op34", "op35", "op36", "op37", "op38", "op39",
  "op40", "op41", "op42", "op43", "op44", "op45", "op46", "op47",
  "op48", "op49", "op50", "op51", "op52", "op53", "op54", "op55",
  "op56", "op57", "op58", "op59", "op60", "op61", "op62", "op63"
};

static char *c0mfunc_name[64] = {
  "c0m00", "tlbr1", "tlbw",  "c0m03", "c0m04", "c0m05", "c0m06", "c0m07",
  "tlbp1", "dctr",  "dctw",  "c0m11", "c0m12", "c0m13", "c0m14", "c0m15",
  "c0m16", "c0m17", "c0m18", "c0m19", "c0m20", "c0m21", "c0m22", "c0m23",
  "c0m24", "c0m25", "c0m26", "c0m27", "c0m28", "c0m29", "c0m30", "c0m31",
  "c0m32", "c0m33", "c0m34", "c0m35", "c0m36", "c0m37", "c0m38", "c0m39",
  "c0m40", "c0m41", "c0m42", "c0m43", "c0m44", "c0m45", "c0m46", "c0m47",
  "c0m48", "c0m49", "c0m50", "c0m51", "c0m52", "c0m53", "c0m54", "c0m55",
  "c0m56", "c0m57", "c0m58", "c0m59", "c0m60", "c0m61", "c0m62", "c0m63"
};

static char *c0reg_name[32] = {
  "index","random","tlblo","tlblo1","context","pagemask","wired","c0r7",
  "badvaddr","count","tlbhi","compare","sr", "cause","epc",  "prid",
  "config","lladdr","watchlo","watchhi","c0r20","c0r21","c0r22","c0r23",
  "c0r24","c0r25","ecc","cacheerr","taglo","taghi","errorepc","c0r31"
};

static char *cop1xfunc_name[64] = {
  "lwxc1",  "ldxc1",  "c1x02",  "c1x03", "c1x04", "luxc1", "c1x06", "pfetch",
  "swxc1",  "sdxc1",  "c1x10",  "c1x11", "c1x12", "suxc1", "c1x14", "prefx",
  "c1x16",  "c1x17",  "c1x18",  "c1x19", "c1x20", "c1x21", "c1x22", "c1x23",
  "c1x24",  "c1x25",  "c1x26",  "c1x27", "alnv.ps", "c1x29", "c1x30", "c1x31",
  "madd.s", "madd.d", "madd.e", "c1x35", "c1x36", "c1x37", "madd.ps", "c1x39",
  "msub.s", "msub.d", "msub.e", "c1x43", "c1x44", "c1x45", "msub.ps", "c1x47",
  "nmadd.s","nmadd.d","nmadd.e","c1x51", "c1x52", "c1x53", "nmadd.ps", "c1x55",
  "nmsub.s","nmsub.d","nmsub.e","c1x59", "c1x60", "c1x61", "nmsub.ps", "c1x63",
};

static char *cop2func_name[64] = {
 "msgn",   "c.eq",   "pickf",   "pickt",   "c.lt",  "c.le",   "min",   "max",
 "c2x8", "c2x9", "sub", "add", "and", "xor", "or", "nor",
 "sll", "c2x17",   "srl",  "sra",  "c2x20", "c2x21", "c2x22", "c2x23",
 "alni.ob", "alnv.ob", "alni.qh", "alnv.qh", "c2x28", "c2x29", "c2x30", "shfl",
 "rzu", "rnau", "rneu", "c2x35", "rzs", "rnas",	"rnes", "c2x39",
 "c2x40", "c2x41", "c2x42", "c2x43", "c2x44", "c2x45", "c2x46", "c2x47",
 "mul",   "c2x49",  "muls",  "mul", "c2x52", "c2x53",	"sub", "add",
 "c2x56",  "c2x57","c2x58", "c2x59", "c2x60",  "c2x61",	"wac",  "rac" 
};

#ifndef _64BIT_OBJECTS
/* public */
/* Three sets of commonly used register names */
/* const */ char *dis_reg_names[3][32] = {
	{	/* compiler names */
		"zero",	"at",	"v0",	"v1",	"a0",	"a1",	"a2",	"a3",
		"t0",	"t1",	"t2",	"t3",	"t4",	"t5",	"t6",	"t7",
		"s0",	"s1",	"s2",	"s3",	"s4",	"s5",	"s6",	"s7",
		"t8",	"t9",	"k0",	"k1",	"gp",	"sp",	"s8",	"ra"
	},
	{	/* hardware names */
		"r0",	"r1",	"r2",	"r3",	"r4",	"r5",	"r6",	"r7",
		"r8",	"r9",	"r10",	"r11",	"r12",	"r13",	"r14",	"r15",
		"r16",	"r17",	"r18",	"r19",	"r20",	"r21",	"r22",	"r23",
		"r24",	"r25",	"r26",	"r27",	"r28",	"r29",	"r30",	"r31"
	},
	{	/* assembler names */
		"$0",	"$at",	"$2",	"$3",	"$4",	"$5",	"$6",	"$7",
		"$8",	"$9",	"$10",	"$11",	"$12",	"$13",	"$14",	"$15",
		"$16",	"$17",	"$18",	"$19",	"$20",	"$21",	"$22",	"$23",
		"$24",	"$25",	"$26",	"$27",	"$gp",	"$sp",	"$30",	"$31"
	}
};
#else
/* public */
/* Three sets of commonly used register names */
/* const */ char *dis_reg_names64[3][32] = {
	{       /* compiler names */
		"zero", "at",   "v0",   "v1",   "a0",   "a1",   "a2",   "a3",
		"a4",   "a5",   "a6",   "a7",   "t0",   "t1",   "t2",   "t3",
		"s0",   "s1",   "s2",   "s3",   "s4",   "s5",   "s6",   "s7",
		"t8",   "t9",   "k0",   "k1",   "gp",   "sp",   "s8",   "ra"
	},
	{       /* assembler names */
		"$0",   "$at",  "$2",   "$3",   "$4",   "$5",   "$6",   "$7",
		"$8",   "$9",   "$10",  "$11",  "$12",  "$13",  "$14",  "$15",
		"$16",  "$17",  "$18",  "$19",  "$20",  "$21",  "$22",  "$23",
		"$24",  "$25",  "$26",  "$27",  "$gp",  "$sp",  "$30",  "$31"
	}
};
#endif

#ifndef _64BIT_OBJECTS
#pragma weak disasm = __disasm32
#pragma weak disasm32 = __disasm32
#pragma weak dis_init = __dis_init32
#pragma weak dis_init32 = __dis_init32
#pragma weak register_name = __register_name32
#pragma weak co_register_name = __co_register_name32
#pragma weak fp_register_name = __fp_register_name32
#pragma weak disassembler = __disassembler32
#pragma weak disassembler32 = __disassembler32
#elif defined(BUILD_OS_DARWIN)
#else
#pragma weak disasm64 = __disasm64
#pragma weak dis_init64 = __dis_init64
#pragma weak register_name64 = __register_name64
#pragma weak co_register_name64 = __co_register_name64
#pragma weak fp_register_name64 = __fp_register_name64
#pragma weak disassembler64 = __disassembler64
#endif /* _64BIT_OBJECTS */


int F_DISASM(char *, ELF_ADDR,Elf32_Addr,Elf32_Addr *,ELF_ADDR *, Elf32_Addr *);
void F_DIS_INIT ( char *, char *, char *[], int);
char * F_REGISTER_NAME(unsigned, Elf32_Addr *);
char * F_C0_REGISTER_NAME(unsigned);
char * F_FP_REGISTER_NAME(unsigned);
char * F_Vec_REGISTER_NAME (unsigned r);
int F_DISASSEMBLER (ELF_ADDR ,int ,char *(*get_symname)(unsigned),int (*get_regvalue)(unsigned),long (*get_bytes)(void),void (*print_header)(unsigned,unsigned));

#ifdef _DIS_BUILD
extern int Rel_data;
extern int Rela_data;
extern Elf32_Addr loc_val32;
extern Elf64_Addr loc_val64;
extern int is_64_bit;
extern ELF_WORD dis_arch;
extern int xflag;
extern int zt5flag;
#endif

/* public -- see .h file */
void
F_DIS_INIT ( char *addr_format, char *value_format, char *reg_names[], int print_jal_targets)
{
	if (addr_format) save.addr_format = addr_format;
	else {
#ifdef _DIS_BUILD
	  if (is_64_bit) 
	      save.addr_format = ADDR64_DEFAULT;
	  else
	    save.addr_format = ADDR_DEFAULT;
#else
	    save.addr_format = ADDR_DEFAULT;
#endif
	} 
	if (value_format) save.value_format = value_format;
	else {
#ifdef _DIS_BUILD
	  if (is_64_bit)
	    save.value_format = VALUE64_DEFAULT;
	  else
	    save.value_format = VALUE_DEFAULT;
#else
	    save.value_format = VALUE_DEFAULT;
#endif
	}
	if (reg_names) save.reg_names = reg_names;
	else {
#ifdef _DIS_BUILD
	  if (is_64_bit)
	    save.reg_names = NAME64_DEFAULT;
	  else
	    save.reg_names = NAME_DEFAULT;
#else
	    save.reg_names = NAME_DEFAULT;
#endif
	}
	save.print_jal_targets = print_jal_targets;
}


/* Update regmask to reflect the use of this general-purpose (not fp)
   register, and return its name */
char *
F_REGISTER_NAME ( unsigned ireg, Elf32_Addr *regmask)
{
	*regmask |= (1 << ireg);
	return save.reg_names[ireg];
}


char *
F_FP_REGISTER_NAME (unsigned r)
{
	static char *name[32] = {
		"$f0",  "$f1",  "$f2",  "$f3",  "$f4",  "$f5",  "$f6",  "$f7",
		"$f8",  "$f9",  "$f10", "$f11", "$f12", "$f13", "$f14", "$f15",
		"$f16", "$f17", "$f18", "$f19", "$f20", "$f21", "$f22", "$f23",
		"$f24", "$f25", "$f26", "$f27", "$f28", "$f29", "$f30", "$f31" 
	};
	return name[r];
}


char *
F_C0_REGISTER_NAME(unsigned r)
{
	return c0reg_name[r];
}

char *
F_Vec_REGISTER_NAME (unsigned r)
{
	static char *name[32] = {
		"$v0",  "$v1",  "$v2",  "$v3",  "$v4",  "$v5",  "$v6",  "$v7",
		"$v8",  "$v9",  "$v10", "$v11", "$v12", "$v13", "$v14", "$v15",
		"$v16", "$v17", "$v18", "$v19", "$v20", "$v21", "$v22", "$v23",
		"$v24", "$v25", "$v26", "$v27", "$v28", "$v29", "$v30", "$v31" 
	};
	return name[r];
}

/* public -- see .h file */
int
F_DISASM(char *buffer, ELF_ADDR address, Elf32_Addr  iword, Elf32_Addr *regmask, ELF_ADDR *symbol_value, Elf32_Addr *ls_register)
{
  int return_value = 0;
  char *bufptr = buffer;
  boolean do_b_displacement = false;
  boolean do_loadstore = false;
  union mips_instruction i;

  i.word = iword;
  *regmask = *symbol_value = *ls_register = 0;

  /* Put out the address and hex value of the instruction,
     leaving bufptr set at the end */
#ifndef _DIS_BUILD
  if (save.addr_format) {
    sprintf(bufptr, save.addr_format, address);
    bufptr += strlen(bufptr);
  }
  if (save.value_format) {
    sprintf(bufptr, save.value_format, iword);
    bufptr += strlen(bufptr);
  }
#endif
#ifndef USE_TARG_INFO  
  switch (i.j_format.opcode) {

  case spec_op:
    if (i.word == 0) {
      strcat(bufptr, "nop");
      bufptr += strlen(bufptr);
      break;
    }
    else if ((i.r_format.func == addu_op || 
	      i.r_format.func == daddu_op ||
	      i.r_format.func == or_op) && 
	     i.r_format.rt == ZERO) 
    {
      if (i.r_format.func == addu_op &&
	  i.r_format.rd == ZERO && 
	  i.r_format.rs == ZERO )
        sprintf (bufptr, "nada");
      else {
	if (i.r_format.func == addu_op)
	{
           sprintf(bufptr, "%s\t%s,%s,%#x", "addu",
		F_REGISTER_NAME(i.r_format.rd, regmask),
		F_REGISTER_NAME(i.r_format.rs, regmask),
		i.r_format.re);
	} else if (i.r_format.func == or_op &&
		   i.r_format.rt != ZERO)
           sprintf(bufptr, "%s\t%s,%s,%#x", "or",
		F_REGISTER_NAME(i.r_format.rd, regmask),
		F_REGISTER_NAME(i.r_format.rs, regmask),
		i.r_format.re);
	else 
          sprintf(bufptr, "move\t%s,%s",
	      F_REGISTER_NAME(i.r_format.rd, regmask),
	      F_REGISTER_NAME(i.r_format.rs, regmask));
	  
      } /* else { */
      bufptr += strlen(bufptr);
      break;
    }
    else if (    ( i.r_format.func == sll_op )
              && ( i.r_format.rd == ZERO )
              && ( i.r_format.rs == ZERO )
              && ( i.r_format.re == 1 ) ) {
      sprintf (bufptr, "ssnop" );
      bufptr += strlen(bufptr);
      break;
    }
    strcat(bufptr, spec_name[i.r_format.func]);
    bufptr += strlen(bufptr);

    switch (i.r_format.func) {
    case dsll_op:
    case dsrl_op:
    case dsra_op:
    case dsll32_op:
    case dsrl32_op:
    case dsra32_op:
      sprintf(bufptr, "\t%s,%s,%d",
	      F_REGISTER_NAME(i.r_format.rd, regmask),
	      F_REGISTER_NAME(i.r_format.rt, regmask),
	      (int)(i.r_format.re + (i.r_format.rs << 5)));
      break;
    case sll_op:
    case srl_op:
    case sra_op:
      sprintf(bufptr, "\t%s,%s,%d",
	      F_REGISTER_NAME(i.r_format.rd, regmask),
	      F_REGISTER_NAME(i.r_format.rt, regmask),
	      (int)i.r_format.re);
      break;
    case sllv_op:
    case srlv_op:
    case srav_op:
    case dsllv_op:
    case dsrlv_op:
    case dsrav_op:
      sprintf(bufptr, "\t%s,%s,%s",
	      F_REGISTER_NAME(i.r_format.rd, regmask),
	      F_REGISTER_NAME(i.r_format.rt, regmask),
	      F_REGISTER_NAME(i.r_format.rs, regmask));
      break;
    case movc_op:
      sprintf(bufptr, "%c\t%s,%s,$fcc%d",
              (i.r_format.rt & 1) ? 't' : 'f',
              F_REGISTER_NAME(i.r_format.rd, regmask),
              F_REGISTER_NAME(i.r_format.rs, regmask),
              (int)(i.r_format.rt >> 2));
      break;
    case movz_op:
    case movn_op:
      sprintf(bufptr, "\t%s,%s,%s",
              F_REGISTER_NAME(i.r_format.rd, regmask),
              F_REGISTER_NAME(i.r_format.rs, regmask),
              F_REGISTER_NAME(i.r_format.rt, regmask));
      break;
    case mfhi_op:
    case mflo_op:
      sprintf(bufptr, "\t%s",
	      F_REGISTER_NAME(i.r_format.rd, regmask));
      break;
    case jalr_op:
      return_value = 2;
      sprintf(bufptr, "\t%s,%s",
	      F_REGISTER_NAME(i.r_format.rd, regmask),
	      F_REGISTER_NAME(i.r_format.rs, regmask));
      break;
    case jr_op:
      return_value = 2;
      sprintf(bufptr, "\t%s",
	      F_REGISTER_NAME(i.r_format.rs, regmask));
      break;
    case mtlo_op:
    case mthi_op:
      sprintf(bufptr, "\t%s",
	      F_REGISTER_NAME(i.r_format.rs, regmask));
      break;
    case teq_op:
      {
        char *format = "\t%s,%s";
        ELF_ADDR op2 = i.r_format.rd * 32 + i.r_format.re;
	
        if (op2)
	  format = "\t%s,%s,%lx";
	sprintf(bufptr, format,
	      F_REGISTER_NAME(i.r_format.rs, regmask),
	      F_REGISTER_NAME(i.r_format.rt, regmask),
	      op2);
      }
      break;
    case tge_op:
    case tlt_op:
    case tne_op:
    case div_op:
      sprintf(bufptr, "\t%s,%s",
	      F_REGISTER_NAME(i.r_format.rs, regmask),
	      F_REGISTER_NAME(i.r_format.rt, regmask));
      break;
    case tgeu_op:
    case tltu_op:
    case mult_op:
    case multu_op:
    case divu_op:
    case dmult_op:
    case dmultu_op:
    case ddiv_op:
    case ddivu_op:
      sprintf(bufptr, "\t%s,%s",
	      F_REGISTER_NAME(i.r_format.rs, regmask),
	      F_REGISTER_NAME(i.r_format.rt, regmask));
      break;
    case syscall_op:
    case sync_op:
      break;
    case break_op:
      {
	char *format = "\t%#x";
	ELF_ADDR op2 = i.r_format.rd * 32 + i.r_format.re;
	
	if (op2)
	  format = "\t%#x,%#x";
	sprintf(bufptr, format, i.r_format.rs*32+i.r_format.rt,
		op2);
      }
      break;
    default:
      sprintf(bufptr, bufptr-buffer<2?"\t\t%s,%s,%s":"\t%s,%s,%s",
	      F_REGISTER_NAME(i.r_format.rd, regmask),
	      F_REGISTER_NAME(i.r_format.rs, regmask),
	      F_REGISTER_NAME(i.r_format.rt, regmask));
      break;
    }
    break;
    
  case bcond_op:
    switch (i.i_format.rt) {
    case tgei_op:
    case tgeiu_op:
    case tlti_op:
    case tltiu_op:
    case teqi_op:
    case tnei_op:
      sprintf(bufptr, "%s\t%s,%d",
	      bcond_name[i.i_format.rt],
	      F_REGISTER_NAME(i.i_format.rs, regmask),
	      i.i_format.simmediate);
      break;
    case blez_op:
      sprintf(bufptr, "%s\t%s,",
	      bcond_name[i.i_format.rt],
	      F_REGISTER_NAME(i.i_format.rs, regmask));
      do_b_displacement = true;
      break;
    case bltz_op:
      sprintf(bufptr, "%s\t%s,",
	      bcond_name[i.i_format.rt],
	      F_REGISTER_NAME(i.i_format.rs, regmask));
      do_b_displacement = true;
      break;
    default:
      sprintf(bufptr, "%s\t%s,",
	      bcond_name[i.i_format.rt],
	      F_REGISTER_NAME(i.i_format.rs, regmask));
      do_b_displacement = true;
      break;
    }
    break;
  case blez_op:
  case bgtz_op:
  case blezl_op:
  case bgtzl_op:
    sprintf(bufptr, "%s\t%s,", op_name[i.i_format.opcode],
	    F_REGISTER_NAME(i.i_format.rs, regmask));
    do_b_displacement = true;
    break;
  case beq_op:
    if (i.i_format.rs == ZERO && i.i_format.rt == ZERO) {
#ifdef _DIS_BUILD
      strcat(bufptr, "b\t\t");
#else
      strcat(bufptr, "b\t");
#endif
      do_b_displacement = true;
      break;
    }
    /* fall through */
  case bne_op:
    sprintf(bufptr, "%s\t%s,%s,", op_name[i.i_format.opcode],
	    F_REGISTER_NAME(i.i_format.rs, regmask),
	    F_REGISTER_NAME(i.i_format.rt, regmask));
    do_b_displacement = true;
    break;

  case beql_op:
  case bnel_op:
    sprintf(bufptr, "%s\t%s,%s,", op_name[i.i_format.opcode],
	    F_REGISTER_NAME(i.i_format.rs, regmask),
	    F_REGISTER_NAME(i.i_format.rt, regmask));
    do_b_displacement = true;
    break;

  case jal_op:
  case j_op:
    sprintf(bufptr, "%s", op_name[i.j_format.opcode]);
    *symbol_value =
      ((address+4)&~((1<<28)-1)) + (i.j_format.target<<2);
    if (save.print_jal_targets) {
      bufptr += strlen(bufptr);
#ifdef _DIS_BUILD
	if (i.r_format.opcode == j_op)
          sprintf(bufptr, "\t\t%#8llx", (unsigned long long)*symbol_value);
	else
          sprintf(bufptr, "\t%#8llx", (unsigned long long)*symbol_value);
#else
      
#ifdef _64BIT_OBJECTS    
      sprintf(bufptr, "\t%#llx", (unsigned long long)*symbol_value);
#else
      sprintf(bufptr, "\t%#x", (unsigned)*symbol_value);
#endif
      
#endif
    }
    return_value = 1;
    break;
    
  case swc1_op:
  case sdc1_op:
  case lwc1_op:
  case ldc1_op:
    sprintf(bufptr, "%s\t%s,", op_name[i.i_format.opcode],
	    F_FP_REGISTER_NAME(i.i_format.rt));
    do_loadstore = true;
    break;

  case swc2_op:
  case sdc2_op:
  case lwc2_op:
  case ldc2_op:
    sprintf(bufptr, "%s\t$%#x,", op_name[i.i_format.opcode],
	    i.i_format.rt);
    do_loadstore = true;
    break;
    
  case lb_op:
  case lh_op:
  case lw_op:
  case ld_op:
  case sb_op:
  case sh_op:
  case sw_op:
  case sd_op:
  case ll_op:
  case sc_op:
    sprintf(bufptr, "%s\t%s,", op_name[i.i_format.opcode],
	    F_REGISTER_NAME(i.i_format.rt, regmask));
    do_loadstore = true;
    break;

  case lbu_op:
  case lhu_op:
  case swl_op:
  case swr_op:
  case lwl_op:
  case lwr_op:
  case scd_op:
  case lld_op:
  case lwu_op:
  case sdl_op:
  case sdr_op:
  case ldl_op:
  case ldr_op:
    sprintf(bufptr, "%s\t%s,", op_name[i.i_format.opcode],
	    F_REGISTER_NAME(i.i_format.rt, regmask));
    do_loadstore = true;
    break;
    
  case pref_op:
    sprintf(bufptr, "%s\t%d,", op_name[i.i_format.opcode], 
			(int)i.i_format.rt);
    do_loadstore = true;
    break;
    
  case cache_op:
    {
      ELF_ADDR code = i.c_format.c_op;
      ELF_ADDR caches = i.c_format.cache;
      char *operation;
      static char *cachename[4] = {"I", "D", "SI", "SD"};
#ifdef _DIS_BUILD
      if (zt5flag) {
	  cachename[2] = "RESERVED";
	  cachename[3] = "S";
      }
#endif

      switch (code) {
      case 0:
	if (caches == 0 || caches == 2) 
	    operation = "Index_Invalidate";
	else 
	    operation = "Index_WriteBack_Invalidate";
	break;
      case 1:
	operation = "Index_Load_Tag";
	break;
      case 2:
	operation = "Index_Store_Tag";
	break;
      case 3:
#ifdef _DIS_BUILD
	if (zt5flag) {
	    operation = "Reserved";
	}
	else {
#endif
	    operation = "Create_Dirty_Exclusive";
#ifdef _DIS_BUILD
	}
#endif
	break;
      case 4:
	operation = "Hit_Invalidate";
	break;
      case 5:
	if (caches == 0)
	    operation = "Fill";
	else 
	    operation = "Hit_WriteBack_Invalidate";
	break;
      case 6:
#ifdef _DIS_BUILD
	if (zt5flag) {
	    operation = "Index_Load_Data";
	}
	else {
#endif
	    operation = "Hit_WriteBack";
#ifdef _DIS_BUILD
	}
#endif
	break;
      case 7:
#ifdef _DIS_BUILD
	if (zt5flag) {
	    operation = "Index_Store_Data";
	}
	else {
#endif
	    operation = "Hit_Set_Virtual";
#ifdef _DIS_BUILD
	}
#endif
	break;
      }
      sprintf(bufptr, "%s\t%s[%s],", op_name[i.i_format.opcode], 
	      operation, cachename[caches]);
      do_loadstore = true;
    }
    break;

  case ori_op:
	sprintf(bufptr, "%s\t%s,%s,%#x", op_name[i.u_format.opcode],
		F_REGISTER_NAME(i.u_format.rt, regmask),
		F_REGISTER_NAME(i.u_format.rs, regmask),
		i.u_format.uimmediate);
    break;

  case xori_op:
    if (i.u_format.rs == ZERO) {
      sprintf(bufptr, "li\t\t%s,%d",
	      F_REGISTER_NAME(i.u_format.rt, regmask),
	      (int)i.u_format.uimmediate);
      break;
    }
    /* fall through */
  case andi_op:
    sprintf(bufptr, "%s\t%s,%s,%#x", op_name[i.u_format.opcode],
	    F_REGISTER_NAME(i.u_format.rt, regmask),
	    F_REGISTER_NAME(i.u_format.rs, regmask),
	    i.u_format.uimmediate);
    break;
  case lui_op:
    if (i.u_format.rs == ZERO)
	sprintf(bufptr, "%s\t%s,%#x", op_name[i.u_format.opcode],
		F_REGISTER_NAME(i.u_format.rt, regmask),
		i.u_format.uimmediate);
    else
        sprintf(bufptr, "%s\t%s,%s,%#x", op_name[i.u_format.opcode],
		F_REGISTER_NAME(i.u_format.rt, regmask),
		F_REGISTER_NAME(i.u_format.rs, regmask),
		i.u_format.uimmediate);
#ifdef _DIS_BUILD
    if (Rel_data || Rela_data)
	return_value = 1;
#endif
    break;
  case addi_op:
  case addiu_op:
    if (i.i_format.rs == ZERO) {
      short sign_extender = i.i_format.simmediate;
#ifdef _DIS_BUILD
      sprintf(bufptr, "li\t");
      bufptr += 3;
      if (xflag) {
	  if (sign_extender >= 0) 
	      sprintf(bufptr, "%s,%#x",
		      F_REGISTER_NAME(i.i_format.rt, regmask),
		      ((int)sign_extender));
	  else 
	      sprintf(bufptr, "%s,-%#x",
		      F_REGISTER_NAME(i.i_format.rt, regmask),
		      -((int)sign_extender));
      }
      else {
	  sprintf(bufptr, "%s,%d",
		  F_REGISTER_NAME(i.i_format.rt, regmask),
		  sign_extender);
      }
#else
      sprintf(bufptr, "li\t%s,%d",
	      F_REGISTER_NAME(i.i_format.rt, regmask),
	      sign_extender);
#endif
      break;
    }
    /* fall through */
  default:
    {
      short sign_extender = i.i_format.simmediate;
#ifdef _DIS_BUILD
      if (xflag) {
	  if (sign_extender >= 0) {
	      sprintf(bufptr, "%s\t%s,%s,%#x", op_name[i.i_format.opcode],
		      F_REGISTER_NAME(i.i_format.rt, regmask),
		      F_REGISTER_NAME(i.i_format.rs, regmask),
		      ((int)sign_extender));
	  }
	  else {
	      sprintf(bufptr, "%s\t%s,%s,-%#x", op_name[i.i_format.opcode],
		      F_REGISTER_NAME(i.i_format.rt, regmask),
		      F_REGISTER_NAME(i.i_format.rs, regmask),
		      -((int)sign_extender));
	  }
      }
      else {
	  sprintf(bufptr, "%s\t%s,%s,%d", op_name[i.i_format.opcode],
		  F_REGISTER_NAME(i.i_format.rt, regmask),
		  F_REGISTER_NAME(i.i_format.rs, regmask),
		  sign_extender);
      }
#else
      sprintf(bufptr, "%s\t%s,%s,%d", op_name[i.i_format.opcode],
	      F_REGISTER_NAME(i.i_format.rt, regmask),
	      F_REGISTER_NAME(i.i_format.rs, regmask),
	      sign_extender);
#endif
    }
#ifdef _DIS_BUILD
    if (Rel_data || Rela_data) return_value =1;
#endif
    break;


  case cop0_op:
    switch (i.r_format.rs) {
    case bc_op:
      sprintf(bufptr, "bc0%s\t", bc_name[i.i_format.rt]);
      do_b_displacement = true;
      break;
    case dmtc_op:
      sprintf(bufptr, "d");
      bufptr++;		/* fall thru */
    case mtc_op:
      sprintf(bufptr, "mtc0\t%s,%s",
	      F_REGISTER_NAME(i.r_format.rt, regmask),
	      F_C0_REGISTER_NAME(i.r_format.rd));
      break;
    case dmfc_op:
      sprintf(bufptr, "d");
      bufptr++;		/* fall thru */
    case mfc_op:
      sprintf(bufptr, "mfc0\t%s,%s",
	      F_REGISTER_NAME(i.r_format.rt, regmask),
	      F_C0_REGISTER_NAME(i.r_format.rd));
      break;
    case cfc_op:
      sprintf(bufptr, "cfc0\t%s,$%d",
	      F_REGISTER_NAME(i.r_format.rt, regmask),
	      (int)i.r_format.rd);
      break;
    case ctc_op:
      sprintf(bufptr, "ctc0\t%s,$%d",
	      F_REGISTER_NAME(i.r_format.rt, regmask),
	      (int)i.r_format.rd);
      break;
    case copm_op:
      sprintf(bufptr, "c0\t%s", c0mfunc_name[i.r_format.func]);
      break;
    case cop_op:
      sprintf(bufptr, "c0\t%s", c0func_name[i.r_format.func]);
      break;
    default:
      sprintf(bufptr, "c0rs%d", (int)i.r_format.rs);
      break;
    }
    break;
    
  case cop1_op:
    switch (i.r_format.rs) {
    case bc_op:
      if ( i.i_format.rt >> 2 )
        sprintf(bufptr, "bc1%s\t$fcc%d,", bc_name[i.i_format.rt & 3],
                (int)(i.r_format.rt >> 2));
      else
        sprintf(bufptr, "bc1%s\t", bc_name[i.i_format.rt]);
      do_b_displacement = true;
      break;
    case dmtc_op:
      sprintf(bufptr, "d");
      bufptr++;		/* fall thru */
    case mtc_op:
      sprintf(bufptr, "mtc1\t%s,%s",
	      F_REGISTER_NAME(i.r_format.rt, regmask),
	      F_FP_REGISTER_NAME(i.r_format.rd));
      break;
    case dmfc_op:
      sprintf(bufptr, "d");
      bufptr++;		/* fall thru */
    case mfc_op:
      sprintf(bufptr, "mfc1\t%s,%s",
	      F_REGISTER_NAME(i.r_format.rt, regmask),
	      F_FP_REGISTER_NAME(i.r_format.rd));
      break;
    case cfc_op:
      sprintf(bufptr, "cfc1\t%s,$%d",
	      F_REGISTER_NAME(i.r_format.rt, regmask),
	      (int)i.r_format.rd);
      break;
    case ctc_op:
      sprintf(bufptr, "ctc1\t%s,$%d",
	      F_REGISTER_NAME(i.r_format.rt, regmask),
	      (int)i.r_format.rd);
      break;
    case cop_op+s_fmt:
    case cop_op+d_fmt:
    case cop_op+e_fmt:
    case cop_op+w_fmt:
    case cop_op+q_fmt:
    case cop_op+l_fmt:
    case cop_op+ps_fmt:
      if (i.r_format.func == fmovc_op )
        sprintf(bufptr, "%s%c.%s\t",
                cop1func_name[i.r_format.func],
                (i.r_format.rt & 1) ? 't' : 'f',
                c1fmt_name[i.r_format.rs - cop_op]);
      else if ((i.r_format.rs == cop_op+ps_fmt) &&
		(i.r_format.func == fcvtspl_op || 
		 i.r_format.func == fcvtspu_op))
       	sprintf(bufptr, "%s%s\t",
	      cop1func_name[i.r_format.func],
	      (i.r_format.func == fcvtspu_op) ? "" : ".pl");
      else
        sprintf(bufptr, "%s.%s\t",
	      cop1func_name[i.r_format.func],
	      c1fmt_name[i.r_format.rs - cop_op]);
      bufptr += strlen(bufptr);
      switch (i.r_format.func) {
      case frecip_op:
      case frsqrt_op:
      case fsqrt_op:
      case fabs_op:
      case fmov_op:
      case fcvts_op:  /* also includes fcvtspl_op */
      case fcvtspu_op:
      case fcvtd_op:
      case fcvte_op:
      case fcvtw_op:
      case fcvtl_op:
      case ftrunc_op:
      case ftruncl_op:
      case fround_op:
      case froundl_op:
      case ffloor_op:
      case ffloorl_op:
      case fceil_op:
      case fceill_op:
      case fneg_op:
	sprintf(bufptr, "%s,%s",
		F_FP_REGISTER_NAME(i.r_format.re),
		F_FP_REGISTER_NAME(i.r_format.rd));
	break;
      case fcmp_op+0x0:
      case fcmp_op+0x1:
      case fcmp_op+0x2:
      case fcmp_op+0x3:
      case fcmp_op+0x4:
      case fcmp_op+0x5:
      case fcmp_op+0x6:
      case fcmp_op+0x7:
      case fcmp_op+0x8:
      case fcmp_op+0x9:
      case fcmp_op+0xa:
      case fcmp_op+0xb:
      case fcmp_op+0xc:
      case fcmp_op+0xd:
      case fcmp_op+0xe:
      case fcmp_op+0xf:
#ifdef _DIS_BUILD
      /* Need to fix.  Non-DIS_BUILD requires the following, too. */
        if ( dis_arch == EF_MIPS_ARCH_4 || dis_arch == EF_MIPS_ARCH_5)
#else
        if ( i.r_format.re >> 2 )
#endif
          sprintf(bufptr, "$fcc%d,%s,%s",
                  (int)(i.r_format.re >> 2),
                  F_FP_REGISTER_NAME(i.r_format.rd),
                  F_FP_REGISTER_NAME(i.r_format.rt));
        else
	  sprintf(bufptr, "%s,%s",
		F_FP_REGISTER_NAME(i.r_format.rd),
		F_FP_REGISTER_NAME(i.r_format.rt));
	break;
      case fmovc_op:
        sprintf(bufptr, "%s,%s,$fcc%d",
                F_FP_REGISTER_NAME(i.r_format.re),
                F_FP_REGISTER_NAME(i.r_format.rd),
                (int)(i.r_format.rt >> 2));
        break;
      case fmovz_op:
      case fmovn_op:
        sprintf(bufptr, "%s,%s,%s",
                F_FP_REGISTER_NAME(i.r_format.re),
                F_FP_REGISTER_NAME(i.r_format.rd),
                F_REGISTER_NAME(i.r_format.rt, regmask));
        break;
      default:
	/* also includes cvt.ps.s, pll.ps, plu.ps, pul.ps, puu.ps */
	sprintf(bufptr, "%s,%s,%s",
		F_FP_REGISTER_NAME(i.r_format.re),
		F_FP_REGISTER_NAME(i.r_format.rd),
		F_FP_REGISTER_NAME(i.r_format.rt));
	break;
      } /* switch on func */
      break;
    } /* switch on rs */
    break; /* End of cop1 */

  case cop1x_op:
  {
    sprintf (bufptr, "%s\t", cop1xfunc_name[i.r_format.func]);
    bufptr += strlen(bufptr);
    switch (i.r_format.func) {
      case lwxc1_op:
      case ldxc1_op:
      case luxc1_op:
        sprintf(bufptr, "%s,%s(%s)",
          F_FP_REGISTER_NAME(i.ma_format.fd),
          F_REGISTER_NAME(i.ma_format.ft, regmask),
          F_REGISTER_NAME(i.ma_format.fr, regmask));
        break;
      case swxc1_op:
      case sdxc1_op:
      case suxc1_op:
        sprintf(bufptr, "%s,%s(%s)",
          F_FP_REGISTER_NAME(i.ma_format.fs),
          F_REGISTER_NAME(i.ma_format.ft, regmask),
          F_REGISTER_NAME(i.ma_format.fr, regmask));
        break;
      case pfetch_op:
        sprintf(bufptr, "%d,%s(%s)",
          (int)i.ma_format.fs,
          F_REGISTER_NAME(i.ma_format.ft, regmask),
          F_REGISTER_NAME(i.ma_format.fr, regmask));
        break;
      case madd_s_op:
      case madd_d_op:
      case madd_e_op:
      case madd_ps_op:
      case msub_s_op:
      case msub_d_op:
      case msub_e_op:
      case msub_ps_op:
      case nmadd_s_op:
      case nmadd_d_op:
      case nmadd_e_op:
      case nmadd_ps_op:
      case nmsub_s_op:
      case nmsub_d_op:
      case nmsub_e_op:
      case nmsub_ps_op:
        sprintf(bufptr, "%s,%s,%s,%s",
          F_FP_REGISTER_NAME(i.ma_format.fd),
          F_FP_REGISTER_NAME(i.ma_format.fr),
          F_FP_REGISTER_NAME(i.ma_format.fs),
          F_FP_REGISTER_NAME(i.ma_format.ft));
        break;
      case alnv_ps_op:
        sprintf(bufptr, "%s,%s,%s,%s",
          F_FP_REGISTER_NAME(i.r_format.re),
          F_FP_REGISTER_NAME(i.r_format.rd),
          F_FP_REGISTER_NAME(i.r_format.rt),
          F_REGISTER_NAME(i.r_format.rs,regmask));
        break;
      default:
        break;
    }
  }
  break;
    
  case cop2_op:
    {
      switch (i.v_format.func) {
	case valni_qh_op:
	case valni_ob_op:
    		sprintf (bufptr, "%s\t", cop2func_name[i.v_format.func]);
    		bufptr += strlen(bufptr);
	   	sprintf(bufptr, "%s,%s,%s,%d",
			F_Vec_REGISTER_NAME(i.v_format.vd),
			F_Vec_REGISTER_NAME(i.v_format.vs),
			F_Vec_REGISTER_NAME(i.v_format.vt),
			(int)i.r_format.rs);
        	break;
	case valnv_qh_op:
	case valnv_ob_op:
    		sprintf (bufptr, "%s\t", cop2func_name[i.v_format.func]);
    		bufptr += strlen(bufptr);
	   	sprintf(bufptr, "%s,%s,%s,%s",
			F_Vec_REGISTER_NAME(i.v_format.vd),
			F_Vec_REGISTER_NAME(i.v_format.vs),
			F_Vec_REGISTER_NAME(i.v_format.vt),
			F_REGISTER_NAME(i.r_format.rs, regmask));
        	break;
	case vshfl_op:
    		sprintf (bufptr, "%s", cop2func_name[i.v_format.func]);
    		bufptr += strlen(bufptr);
		/* vector formats can either be .ob (oct byte) or
		   .qh (quad half) 
		*/
    		sprintf (bufptr,"%s\t",(i.v_format.fmt & qh_fmt) ? ".qh":".ob");
    		bufptr += strlen(bufptr);
	   	sprintf(bufptr, "%s,%s,%s",
			F_Vec_REGISTER_NAME(i.v_format.vd),
			F_Vec_REGISTER_NAME(i.v_format.vs),
			F_Vec_REGISTER_NAME(i.v_format.vt));
        	break;
        case vadd_op:
	case vsub_op:
	case vmul_op:
	case vsll_op:
	case vmin_op:
	case vmax_op:
	case vsrl_op:
	case vsra_op:
	case vand_op:
	case vor_op:
	case vnor_op:
	case vxor_op:
	case vmsgn_op:
        case vpickf_op:
	case vpickt_op:
    		sprintf (bufptr, "%s", cop2func_name[i.v_format.func]);
    		bufptr += strlen(bufptr);
		/* vector formats can either be .ob (oct byte) or
		   .qh (quad half) 
		*/
    		sprintf (bufptr,"%s\t",(i.v_format.fmt & qh_fmt) ? ".qh":".ob");
    		bufptr += strlen(bufptr);
		/* vector formats (actually the "vt" register) can be either 
		   of the three cases. 
		   $v0-$v31 -> any of 32 vector/fp registers
		   $v{0-31}[i] -> vector format, e.g v6[2]
		   immed    -> immediate value, 
		   fmt/sel configuration
		   OB format
		   	00000 - 01110 =>  vt is a vector elements v[0..7]
			10110	      =>  vt is a register
			11110 	      =>  vt is a immediate value
		   QH format
		   	00000 - 01101 =>  vt is a vector elements v[0..3]
			10101	      =>  vt is a register
			11101 	      =>  vt is a immediate value
		*/
		if ((i.v_format.sel >> 3) & 0x1)
		{
			if (i.v_format.sel == 10 ||
			    i.v_format.sel == 11)  {
			/* vt is a register */
			   sprintf(bufptr, "%s,%s,%s",
				F_Vec_REGISTER_NAME(i.v_format.vd),
				F_Vec_REGISTER_NAME(i.v_format.vs),
				F_Vec_REGISTER_NAME(i.v_format.vt));
			} else {
			/* vt is an immediate value */
			   sprintf(bufptr, "%s,%s,%d",
				F_Vec_REGISTER_NAME(i.v_format.vd),
				F_Vec_REGISTER_NAME(i.v_format.vs),
				(int)i.v_format.vt);
			}
		} else {
			/* vt is a vector element select */
			   sprintf(bufptr, "%s,%s,%s[%d]",
				F_Vec_REGISTER_NAME(i.v_format.vd),
				F_Vec_REGISTER_NAME(i.v_format.vs),
				F_Vec_REGISTER_NAME(i.v_format.vt),
				(int)(i.v_format.sel >> 
				   (i.v_format.fmt & 0x1)));
		}
        	break;
        case vadda_op:
	case vmula_op:
	case vmuls_op:
	case vsuba_op:
		if (i.v_format.func == vmuls_op)
    		      sprintf (bufptr, "%s%s", cop2func_name[i.v_format.func],
			   ((i.v_format.vd  >> 4) & 0x1) ? "l" : "");
		else 
    		      sprintf (bufptr, "%s%s", cop2func_name[i.v_format.func],
			   ((i.v_format.vd  >> 4) & 0x1) ? "l" : "a");
    		bufptr += strlen(bufptr);
		/* vector formats can either be .ob (oct byte) or
		   .qh (quad half) 
		*/
    		sprintf (bufptr,"%s\t",(i.v_format.fmt & qh_fmt) ? ".qh":".ob");
    		bufptr += strlen(bufptr);
		/* vector formats (actually the "vt" register) can be either 
		   of the three cases. 
		   $v0-$v31 -> any of 32 vector/fp registers
		   $v{0-31}[i] -> vector format, e.g v6[2]
		   immed    -> immediate value, 
		   fmt/sel configuration
		   OB format
		   	00000 - 01110 =>  vt is a vector elements v[0..7]
			10110	      =>  vt is a register
			11110 	      =>  vt is a immediate value
		   QH format
		   	00000 - 01101 =>  vt is a vector elements v[0..3]
			10101	      =>  vt is a register
			11101 	      =>  vt is a immediate value
		*/
		if ((i.v_format.sel >> 3) & 0x1)
		{
			if (i.v_format.sel == 10 ||
			    i.v_format.sel == 11)  {
			/* vt is a register */
			   sprintf(bufptr, "%s,%s",
				F_Vec_REGISTER_NAME(i.v_format.vs),
				F_Vec_REGISTER_NAME(i.v_format.vt));
			} else {
			/* vt is an immediate value */
			   sprintf(bufptr, "%s,%d",
				F_Vec_REGISTER_NAME(i.v_format.vs),
				(int)i.v_format.vt);
			}
		} else {
			/* vt is a vector element select */
			   sprintf(bufptr, "%s,%s[%d]",
				F_Vec_REGISTER_NAME(i.v_format.vs),
				F_Vec_REGISTER_NAME(i.v_format.vt),
				(int)(i.v_format.sel >> 
				 (i.v_format.fmt & 0x1)));
		}
        	break;
      	case vrnd_zu_op:
      	case vrnd_nau_op:
      	case vrnd_neu_op:
  	case vrnd_zs_op:
	case vrnd_nas_op:
	case vrnd_nes_op:
    		sprintf (bufptr, "%s", cop2func_name[i.v_format.func]);
    		bufptr += strlen(bufptr);
		/* vector formats can either be .ob (oct byte) or
		   .qh (quad half) 
		*/
    		sprintf (bufptr,"%s\t",(i.v_format.fmt & qh_fmt) ? ".qh":".ob");
    		bufptr += strlen(bufptr);
		/* vector formats (actually the "vt" register) can be either 
		   of the three cases. 
		   $v0-$v31 -> any of 32 vector/fp registers
		   $v{0-31}[i] -> vector format, e.g v6[2]
		   immed    -> immediate value, 
		   fmt/sel configuration
		   OB format
		   	00000 - 01110 =>  vt is a vector elements v[0..7]
			10110	      =>  vt is a register
			11110 	      =>  vt is a immediate value
		   QH format
		   	00000 - 01101 =>  vt is a vector elements v[0..3]
			10101	      =>  vt is a register
			11101 	      =>  vt is a immediate value
		*/
		if ((i.v_format.sel >> 3) & 0x1)
		{
			if (i.v_format.sel == 10 ||
			    i.v_format.sel == 11)  {
			/* vt is a register */
			   sprintf(bufptr, "%s,%s",
				F_Vec_REGISTER_NAME(i.v_format.vd),
				F_Vec_REGISTER_NAME(i.v_format.vt));
			} else {
			/* vt is an immediate value */
			   sprintf(bufptr, "%s,%d",
				F_Vec_REGISTER_NAME(i.v_format.vd),
				(int)i.v_format.vt);
			}
		} else {
			/* vt is a vector element select */
			   sprintf(bufptr, "%s,%s[%d]",
				F_Vec_REGISTER_NAME(i.v_format.vd),
				F_Vec_REGISTER_NAME(i.v_format.vt),
				(int)((i.v_format.sel >> 
				  (i.v_format.fmt & 0x1))));
		}
        	break;
      	case vrac_op:
    		sprintf (bufptr, "%s", cop2func_name[i.v_format.func]);
    		bufptr += strlen(bufptr);
		/* sel value is used to differentiate the 3 diff formats 
		   racl -> least significant third  (sel = 0)
		   racm -> middle significant third (sel = 4)
		   rach -> most significant third   (sel = 8)
		*/
    		sprintf (bufptr, "%s",(i.v_format.sel == 0) ? "l" : 
				      (i.v_format.sel == 4) ? "m" : "h");
    		bufptr += strlen(bufptr);
		/* vector formats can either be .ob (oct byte) or
		   .qh (quad half) 
		*/
    		sprintf (bufptr,"%s\t",(i.v_format.fmt & qh_fmt) ? ".qh":".ob");
    		bufptr += strlen(bufptr);
	        sprintf(bufptr, "%s", F_Vec_REGISTER_NAME(i.v_format.vd));
        	break;
      	case vwac_op:
    		sprintf (bufptr, "%s", cop2func_name[i.v_format.func]);
    		bufptr += strlen(bufptr);
		/* sel value is used to differentiate the 2 diff formats 
		   wacl -> least significant third  (sel = 0)
		   wach -> most significant third   (sel = 8)
		*/
    		sprintf (bufptr, "%s",(i.v_format.sel == 0) ? "l" : "h");
    		bufptr += strlen(bufptr);
		/* vector formats can either be .ob (oct byte) or
		   .qh (quad half) 
		*/
    		sprintf (bufptr,"%s\t",(i.v_format.fmt & qh_fmt) ? ".qh":".ob");
    		bufptr += strlen(bufptr);
	        sprintf(bufptr, "%s", F_Vec_REGISTER_NAME(i.v_format.vs));
		if (i.v_format.sel == 0) {
    		   bufptr += strlen(bufptr);
	       	   sprintf(bufptr, ",%s", F_Vec_REGISTER_NAME(i.v_format.vt));
		}
        	break;
        case vcmp_lt_op:
	case vcmp_le_op:
	case vcmp_eq_op:
    		sprintf (bufptr, "%s", cop2func_name[i.v_format.func]);
    		bufptr += strlen(bufptr);
		/* vector formats can either be .ob (oct byte) or
		   .qh (quad half) 
		*/
    		sprintf (bufptr,"%s\t",(i.v_format.fmt & qh_fmt) ? ".qh":".ob");
    		bufptr += strlen(bufptr);
		/* vector formats (actually the "vt" register) can be either 
		   of the three cases. 
		   $v0-$v31 -> any of 32 vector/fp registers
		   $v{0-31}[i] -> vector format, e.g v6[2]
		   immed    -> immediate value, 
		   fmt/sel configuration
		   OB format
		   	00000 - 01110 =>  vt is a vector elements v[0..7]
			10110	      =>  vt is a register
			11110 	      =>  vt is a immediate value
		   QH format
		   	00000 - 01101 =>  vt is a vector elements v[0..3]
			10101	      =>  vt is a register
			11101 	      =>  vt is a immediate value
		*/
		if ((i.v_format.sel >> 3) & 0x1)
		{
			if (i.v_format.sel == 10 ||
			    i.v_format.sel == 11)  {
			/* vt is a register */
			   sprintf(bufptr, "%s,%s",
				F_Vec_REGISTER_NAME(i.v_format.vs),
				F_Vec_REGISTER_NAME(i.v_format.vt));
			} else {
			/* vt is an immediate value */
			   sprintf(bufptr, "%s,%d",
				F_Vec_REGISTER_NAME(i.v_format.vs),
				(int)i.v_format.vt);
			}
		} else {
			/* vt is a vector element select */
			   sprintf(bufptr, "%s,%s[%d]",
				F_Vec_REGISTER_NAME(i.v_format.vs),
				F_Vec_REGISTER_NAME(i.v_format.vt),
				(int)(i.v_format.sel >> 
					(i.v_format.fmt & 0x1)));
		}
        	break;
      	default:
        	break;
      }
    }
    break;
    
  }
  
  /* Some instructions require more than just registers */
  
  if (do_loadstore) {
    short sign_extender = i.i_format.simmediate;
    *symbol_value = sign_extender;
    *ls_register = i.i_format.rs;
    bufptr += strlen(bufptr);
#ifdef _DIS_BUILD
    if (xflag) {
	if (sign_extender >= 0) 
	    sprintf(bufptr, "%#x(%s)",(int)sign_extender,F_REGISTER_NAME(i.i_format.rs, regmask));
	else
	    sprintf(bufptr, "-%#x(%s)",-((int)sign_extender),F_REGISTER_NAME(i.i_format.rs, regmask));
    }
    else
#endif
        /* restored %d formatting as in previous releases. davea 6/93 */
        sprintf(bufptr, "%d(%s)",(int)sign_extender,F_REGISTER_NAME(i.i_format.rs, regmask));
#ifdef _DIS_BUILD
    if (Rel_data || Rela_data)
      return_value = 1;
    else
      return_value = -1;
#else
      return_value = -1;
#endif
  }
  else if (do_b_displacement) {
    short sign_extender = i.i_format.simmediate;
    bufptr += strlen(bufptr);
#ifdef _DIS_BUILD
    if (is_64_bit)
    {
      address = (ELF_ADDR)loc_val64;
      sprintf(bufptr, "%#llx", 
		(unsigned long long)(address+4+(sign_extender<<2)));
    } else {
      address = loc_val32;
      sprintf(bufptr, "%#x", 
		(unsigned)(address+4+(sign_extender<<2)));
   }
#else

	sprintf(bufptr, "%#llx", 
		(unsigned long long)(address+4+(sign_extender<<2)));
    
#endif
    return_value = 2;
  }
  return return_value;
#else
  bufptr += dis_asm(bufptr, iword);

  /* turn off printing out of symbols. As far as I know, the implementation
     never works anyway. Also, the way to deal with symbolics is totally
     brain dead. I'd rather we do it right when there's a need than trying
     to sledge hammer the fix with the current way
  */
  return 0;
#endif /* USE_TARG_INFO */
}


/* public - see .h file */
void
F_DIS_REGS ( char *buffer,unsigned regmask,ELF_ADDR reg_values[])
{
	boolean first = true;
	ELF_ADDR i;

	buffer += strlen(buffer);
	for (i = 0; regmask; i++, regmask >>= 1) {
		if (regmask & 1) {
#ifdef _DIS_BUILD
			  sprintf(buffer, "%s%s=%#llx", first ? "\t<" : ",",
				save.reg_names[i], 
				(unsigned long long)reg_values[i]);
#else
				
			  sprintf(buffer, "%s%s=" /* NO COMMA HERE, PLEASE */
				HEXREG
			         , first ? "\t<" : ",",
				save.reg_names[i], reg_values[i]);
#endif
			buffer += strlen(buffer);
			first = false;
		}
	}
	if (!first)
		strcat(buffer, ">");
}


#include <errno.h>
extern int errno;

/* public - see .h file */
/* Returns -1 if nothing done 
   else number of bytes disassembled
*/
int
F_DISASSEMBLER (ELF_ADDR iadr,int regstyle,char *(*get_symname)(unsigned),int (*get_regvalue)(unsigned),long (*get_bytes)(void),void (*print_header)(unsigned,unsigned))
{
	ELF_ADDR old_iadr = iadr;

	if (!get_bytes) {
		errno = EINVAL;
		return -1;
	}

	/* Don't print address, value, or (if we have get_symname) jal targets */
	F_DIS_INIT("", "", regstyle ? HARDWARE_NAMES : COMPILER_NAMES,
    	! (int) get_symname);

	for (; ; iadr += 4) {
		int which;
		Elf32_Addr instr; 
	        Elf32_Addr regmask, ls_register;
		ELF_ADDR symbol_value;
		char buffer[1024], *symname;

		instr = (Elf32_Addr) get_bytes();
		if (print_header)
			print_header(iadr, instr);
		which = F_DISASM(buffer,iadr,instr,&regmask,&symbol_value,&ls_register);

		/* For jal, print the target either via get_symname or numerically */
		if ((which == 1) && get_symname)
			if (symname = get_symname(symbol_value))
				strcat(buffer, symname);
			else
				sprintf(buffer + strlen(buffer), "%#llx", 
					(unsigned long long)symbol_value);

		/* For load/store, if we have get_regvalue, print the effective addr */
		else if ((which < 0) && get_regvalue)
			sprintf(buffer + strlen(buffer),
				" <0x%llx>", 
				(unsigned long long)(get_regvalue(ls_register)
				    + symbol_value));

		/* If we have get_regvalue, we must print the registers */
		if (regmask && get_regvalue) {
			ELF_ADDR reg_values[32]; 
			ELF_ADDR reg_cnt, regtemp;

			for (reg_cnt = 0, regtemp = regmask; regtemp; 
				reg_cnt++, regtemp >>= 1)
			if (regtemp & 1)
				reg_values[reg_cnt] = get_regvalue(reg_cnt);
			F_DIS_REGS(buffer + strlen(buffer), regmask, reg_values);
		}

		puts(buffer);

		/* Quit unless we have a jump/call/branch delay slot */
		if (which <= 0) {
			return (int)(iadr - old_iadr + 4);
	        }
	}
}
