/* Generated automatically by the program `genoutput'
   from the machine description file `md'.  */

#include "config.h"
#include "system.h"
#include "flags.h"
#include "ggc.h"
#include "rtl.h"
#include "expr.h"
#include "insn-codes.h"
#include "tm_p.h"
#include "function.h"
#include "regs.h"
#include "hard-reg-set.h"
#include "real.h"
#include "insn-config.h"

#include "conditions.h"
#include "insn-attr.h"

#include "recog.h"

#include "toplev.h"
#include "output.h"

static const char * const output_0[] = {
  "lbz%U1%X1 %0,%1",
  "rldicl %0,%1,0,56",
};

static const char * const output_1[] = {
  "rldicl. %2,%1,0,56",
  "#",
};

static const char * const output_2[] = {
  "rldicl. %0,%1,0,56",
  "#",
};

static const char * const output_4[] = {
  "extsb. %2,%1",
  "#",
};

static const char * const output_5[] = {
  "extsb. %0,%1",
  "#",
};

static const char * const output_6[] = {
  "lhz%U1%X1 %0,%1",
  "rldicl %0,%1,0,48",
};

static const char * const output_7[] = {
  "rldicl. %2,%1,0,48",
  "#",
};

static const char * const output_8[] = {
  "rldicl. %0,%1,0,48",
  "#",
};

static const char * const output_9[] = {
  "lha%U1%X1 %0,%1",
  "extsh %0,%1",
};

static const char * const output_10[] = {
  "extsh. %2,%1",
  "#",
};

static const char * const output_11[] = {
  "extsh. %0,%1",
  "#",
};

static const char * const output_12[] = {
  "lwz%U1%X1 %0,%1",
  "rldicl %0,%1,0,32",
};

static const char * const output_13[] = {
  "rldicl. %2,%1,0,32",
  "#",
};

static const char * const output_14[] = {
  "rldicl. %0,%1,0,32",
  "#",
};

static const char * const output_15[] = {
  "lwa%U1%X1 %0,%1",
  "extsw %0,%1",
};

static const char * const output_16[] = {
  "extsw. %2,%1",
  "#",
};

static const char * const output_17[] = {
  "extsw. %0,%1",
  "#",
};

static const char * const output_18[] = {
  "lbz%U1%X1 %0,%1",
  "{rlinm|rlwinm} %0,%1,0,0xff",
};

static const char * const output_19[] = {
  "{andil.|andi.} %2,%1,0xff",
  "#",
};

static const char * const output_20[] = {
  "{andil.|andi.} %0,%1,0xff",
  "#",
};

static const char * const output_22[] = {
  "extsb. %2,%1",
  "#",
};

static const char * const output_23[] = {
  "extsb. %0,%1",
  "#",
};

static const char * const output_24[] = {
  "lbz%U1%X1 %0,%1",
  "{rlinm|rlwinm} %0,%1,0,0xff",
};

static const char * const output_25[] = {
  "{andil.|andi.} %2,%1,0xff",
  "#",
};

static const char * const output_26[] = {
  "{andil.|andi.} %0,%1,0xff",
  "#",
};

static const char * const output_28[] = {
  "extsb. %2,%1",
  "#",
};

static const char * const output_29[] = {
  "extsb. %0,%1",
  "#",
};

static const char * const output_30[] = {
  "lhz%U1%X1 %0,%1",
  "{rlinm|rlwinm} %0,%1,0,0xffff",
};

static const char * const output_31[] = {
  "{andil.|andi.} %2,%1,0xffff",
  "#",
};

static const char * const output_32[] = {
  "{andil.|andi.} %0,%1,0xffff",
  "#",
};

static const char * const output_33[] = {
  "lha%U1%X1 %0,%1",
  "{exts|extsh} %0,%1",
};

static const char * const output_34[] = {
  "{exts.|extsh.} %2,%1",
  "#",
};

static const char * const output_35[] = {
  "{exts.|extsh.} %0,%1",
  "#",
};

static const char * const output_36[] = {
  "{cax|add} %0,%1,%2",
  "{cal %0,%2(%1)|addi %0,%1,%2}",
  "{ai|addic} %0,%1,%2",
  "{cau|addis} %0,%1,%v2",
};

static const char * const output_37[] = {
  "{cax.|add.} %3,%1,%2",
  "{ai.|addic.} %3,%1,%2",
  "#",
  "#",
};

static const char * const output_38[] = {
  "{cax.|add.} %0,%1,%2",
  "{ai.|addic.} %0,%1,%2",
  "#",
  "#",
};

static const char * const output_40[] = {
  "nor. %2,%1,%1",
  "#",
};

static const char * const output_41[] = {
  "nor. %0,%1,%1",
  "#",
};

static const char * const output_43[] = {
  "subf %0,%2,%1",
  "subfic %0,%2,%1",
};

static const char * const output_44[] = {
  "{sf.|subfc.} %3,%2,%1",
  "#",
};

static const char * const output_45[] = {
  "subf. %3,%2,%1",
  "#",
};

static const char * const output_46[] = {
  "{sf.|subfc.} %0,%2,%1",
  "#",
};

static const char * const output_47[] = {
  "subf. %0,%2,%1",
  "#",
};

static const char * const output_49[] = {
  "doz%I2. %3,%1,%2",
  "#",
};

static const char * const output_50[] = {
  "doz%I2. %0,%1,%2",
  "#",
};

static const char * const output_56[] = {
  "neg. %2,%1",
  "#",
};

static const char * const output_57[] = {
  "neg. %0,%1",
  "#",
};

static const char * const output_59[] = {
  "{muls|mullw} %0,%1,%2",
  "{muli|mulli} %0,%1,%2",
};

static const char * const output_60[] = {
  "{muls|mullw} %0,%1,%2",
  "{muli|mulli} %0,%1,%2",
};

static const char * const output_61[] = {
  "{muls.|mullw.} %3,%1,%2",
  "#",
};

static const char * const output_62[] = {
  "{muls.|mullw.} %3,%1,%2",
  "#",
};

static const char * const output_63[] = {
  "{muls.|mullw.} %0,%1,%2",
  "#",
};

static const char * const output_64[] = {
  "{muls.|mullw.} %0,%1,%2",
  "#",
};

static const char * const output_71[] = {
  "{srai|srawi} %3,%1,%p2\n\t{aze.|addze.} %3,%3",
  "#",
};

static const char * const output_72[] = {
  "{srai|srawi} %0,%1,%p2\n\t{aze.|addze.} %0,%0",
  "#",
};

static const char * const output_80[] = {
  "and %0,%1,%2",
  "{rlinm|rlwinm} %0,%1,0,%m2,%M2",
  "{andil.|andi.} %0,%1,%b2",
  "{andiu.|andis.} %0,%1,%u2",
};

static const char * const output_81[] = {
  "and. %3,%1,%2",
  "{andil.|andi.} %3,%1,%b2",
  "{andiu.|andis.} %3,%1,%u2",
  "{rlinm.|rlwinm.} %3,%1,0,%m2,%M2",
  "#",
  "#",
  "#",
  "#",
};

static const char * const output_82[] = {
  "#",
  "{andil.|andi.} %3,%1,%b2",
  "{andiu.|andis.} %3,%1,%u2",
  "{rlinm.|rlwinm.} %3,%1,0,%m2,%M2",
  "#",
  "#",
  "#",
  "#",
};

static const char * const output_83[] = {
  "and. %0,%1,%2",
  "{andil.|andi.} %0,%1,%b2",
  "{andiu.|andis.} %0,%1,%u2",
  "{rlinm.|rlwinm.} %0,%1,0,%m2,%M2",
  "#",
  "#",
  "#",
  "#",
};

static const char * const output_84[] = {
  "#",
  "{andil.|andi.} %0,%1,%b2",
  "{andiu.|andis.} %0,%1,%u2",
  "{rlinm.|rlwinm.} %0,%1,0,%m2,%M2",
  "#",
  "#",
  "#",
  "#",
};

static const char * const output_88[] = {
  "%q3 %0,%1,%2",
  "{%q3il|%q3i} %0,%1,%b2",
  "{%q3iu|%q3is} %0,%1,%u2",
};

static const char * const output_89[] = {
  "%q4. %3,%1,%2",
  "#",
};

static const char * const output_90[] = {
  "%q4. %0,%1,%2",
  "#",
};

static const char * const output_92[] = {
  "%q4. %3,%2,%1",
  "#",
};

static const char * const output_93[] = {
  "%q4. %0,%2,%1",
  "#",
};

static const char * const output_95[] = {
  "%q4. %3,%1,%2",
  "#",
};

static const char * const output_96[] = {
  "%q4. %0,%1,%2",
  "#",
};

static const char * const output_101[] = {
  "maskir. %0,%3,%2",
  "#",
};

static const char * const output_102[] = {
  "maskir. %0,%3,%2",
  "#",
};

static const char * const output_103[] = {
  "maskir. %0,%3,%2",
  "#",
};

static const char * const output_104[] = {
  "maskir. %0,%3,%2",
  "#",
};

static const char *output_105 PARAMS ((rtx *, rtx));

static const char *
output_105 (operands, insn)
     rtx *operands ATTRIBUTE_UNUSED;
     rtx insn ATTRIBUTE_UNUSED;
{

{
  int start = INTVAL (operands[2]) & 31;
  int size = INTVAL (operands[1]) & 31;

  operands[4] = GEN_INT (32 - start - size);
  operands[1] = GEN_INT (start + size - 1);
  return "{rlimi|rlwimi} %0,%3,%h4,%h2,%h1";
}
}

static const char *output_106 PARAMS ((rtx *, rtx));

static const char *
output_106 (operands, insn)
     rtx *operands ATTRIBUTE_UNUSED;
     rtx insn ATTRIBUTE_UNUSED;
{

{
  int shift = INTVAL (operands[4]) & 31;
  int start = INTVAL (operands[2]) & 31;
  int size = INTVAL (operands[1]) & 31;

  operands[4] = GEN_INT (shift - start - size);
  operands[1] = GEN_INT (start + size - 1);
  return "{rlimi|rlwimi} %0,%3,%h4,%h2,%h1";
}
}

static const char *output_107 PARAMS ((rtx *, rtx));

static const char *
output_107 (operands, insn)
     rtx *operands ATTRIBUTE_UNUSED;
     rtx insn ATTRIBUTE_UNUSED;
{

{
  int shift = INTVAL (operands[4]) & 31;
  int start = INTVAL (operands[2]) & 31;
  int size = INTVAL (operands[1]) & 31;

  operands[4] = GEN_INT (32 - shift - start - size);
  operands[1] = GEN_INT (start + size - 1);
  return "{rlimi|rlwimi} %0,%3,%h4,%h2,%h1";
}
}

static const char *output_108 PARAMS ((rtx *, rtx));

static const char *
output_108 (operands, insn)
     rtx *operands ATTRIBUTE_UNUSED;
     rtx insn ATTRIBUTE_UNUSED;
{

{
  int shift = INTVAL (operands[4]) & 31;
  int start = INTVAL (operands[2]) & 31;
  int size = INTVAL (operands[1]) & 31;

  operands[4] = GEN_INT (32 - shift - start - size);
  operands[1] = GEN_INT (start + size - 1);
  return "{rlimi|rlwimi} %0,%3,%h4,%h2,%h1";
}
}

static const char *output_109 PARAMS ((rtx *, rtx));

static const char *
output_109 (operands, insn)
     rtx *operands ATTRIBUTE_UNUSED;
     rtx insn ATTRIBUTE_UNUSED;
{

{
  int extract_start = INTVAL (operands[5]) & 31;
  int extract_size = INTVAL (operands[4]) & 31;
  int insert_start = INTVAL (operands[2]) & 31;
  int insert_size = INTVAL (operands[1]) & 31;

/* Align extract field with insert field */
  operands[5] = GEN_INT (extract_start + extract_size - insert_start - insert_size);
  operands[1] = GEN_INT (insert_start + insert_size - 1);
  return "{rlimi|rlwimi} %0,%3,%h5,%h2,%h1";
}
}

static const char *output_110 PARAMS ((rtx *, rtx));

static const char *
output_110 (operands, insn)
     rtx *operands ATTRIBUTE_UNUSED;
     rtx insn ATTRIBUTE_UNUSED;
{

{
  int start = INTVAL (operands[2]) & 63;
  int size = INTVAL (operands[1]) & 63;

  operands[1] = GEN_INT (64 - start - size);
  return "rldimi %0,%3,%H1,%H2";
}
}

static const char *output_111 PARAMS ((rtx *, rtx));

static const char *
output_111 (operands, insn)
     rtx *operands ATTRIBUTE_UNUSED;
     rtx insn ATTRIBUTE_UNUSED;
{

{
  int start = INTVAL (operands[3]) & 31;
  int size = INTVAL (operands[2]) & 31;

  if (start + size >= 32)
    operands[3] = const0_rtx;
  else
    operands[3] = GEN_INT (start + size);
  return "{rlinm|rlwinm} %0,%1,%3,%s2,31";
}
}

static const char *output_112 PARAMS ((rtx *, rtx));

static const char *
output_112 (operands, insn)
     rtx *operands ATTRIBUTE_UNUSED;
     rtx insn ATTRIBUTE_UNUSED;
{

{
  int start = INTVAL (operands[3]) & 31;
  int size = INTVAL (operands[2]) & 31;

  /* Force split for non-cc0 compare.  */
  if (which_alternative == 1)
     return "#";

  /* If the bit-field being tested fits in the upper or lower half of a
     word, it is possible to use andiu. or andil. to test it.  This is
     useful because the condition register set-use delay is smaller for
     andi[ul]. than for rlinm.  This doesn't work when the starting bit
     position is 0 because the LT and GT bits may be set wrong.  */

  if ((start > 0 && start + size <= 16) || start >= 16)
    {
      operands[3] = GEN_INT (((1 << (16 - (start & 15)))
			      - (1 << (16 - (start & 15) - size))));
      if (start < 16)
	return "{andiu.|andis.} %4,%1,%3";
      else
	return "{andil.|andi.} %4,%1,%3";
    }

  if (start + size >= 32)
    operands[3] = const0_rtx;
  else
    operands[3] = GEN_INT (start + size);
  return "{rlinm.|rlwinm.} %4,%1,%3,%s2,31";
}
}

static const char *output_113 PARAMS ((rtx *, rtx));

static const char *
output_113 (operands, insn)
     rtx *operands ATTRIBUTE_UNUSED;
     rtx insn ATTRIBUTE_UNUSED;
{

{
  int start = INTVAL (operands[3]) & 31;
  int size = INTVAL (operands[2]) & 31;

  /* Force split for non-cc0 compare.  */
  if (which_alternative == 1)
     return "#";

  /* Since we are using the output value, we can't ignore any need for
     a shift.  The bit-field must end at the LSB.  */
  if (start >= 16 && start + size == 32)
    {
      operands[3] = GEN_INT ((1 << size) - 1);
      return "{andil.|andi.} %0,%1,%3";
    }

  if (start + size >= 32)
    operands[3] = const0_rtx;
  else
    operands[3] = GEN_INT (start + size);
  return "{rlinm.|rlwinm.} %0,%1,%3,%s2,31";
}
}

static const char *output_114 PARAMS ((rtx *, rtx));

static const char *
output_114 (operands, insn)
     rtx *operands ATTRIBUTE_UNUSED;
     rtx insn ATTRIBUTE_UNUSED;
{

{
  int start = INTVAL (operands[3]) & 63;
  int size = INTVAL (operands[2]) & 63;

  if (start + size >= 64)
    operands[3] = const0_rtx;
  else
    operands[3] = GEN_INT (start + size);
  operands[2] = GEN_INT (64 - size);
  return "rldicl %0,%1,%3,%2";
}
}

static const char *output_115 PARAMS ((rtx *, rtx));

static const char *
output_115 (operands, insn)
     rtx *operands ATTRIBUTE_UNUSED;
     rtx insn ATTRIBUTE_UNUSED;
{

{
  int start = INTVAL (operands[3]) & 63;
  int size = INTVAL (operands[2]) & 63;

  if (start + size >= 64)
    operands[3] = const0_rtx;
  else
    operands[3] = GEN_INT (start + size);
  operands[2] = GEN_INT (64 - size);
  return "rldicl. %4,%1,%3,%2";
}
}

static const char *output_116 PARAMS ((rtx *, rtx));

static const char *
output_116 (operands, insn)
     rtx *operands ATTRIBUTE_UNUSED;
     rtx insn ATTRIBUTE_UNUSED;
{

{
  int start = INTVAL (operands[3]) & 63;
  int size = INTVAL (operands[2]) & 63;

  if (start + size >= 64)
    operands[3] = const0_rtx;
  else
    operands[3] = GEN_INT (start + size);
  operands[2] = GEN_INT (64 - size);
  return "rldicl. %0,%1,%3,%2";
}
}

static const char * const output_118[] = {
  "{rl%I2nm.|rlw%I2nm.} %3,%1,%h2,0xffffffff",
  "#",
};

static const char * const output_119[] = {
  "{rl%I2nm.|rlw%I2nm.} %0,%1,%h2,0xffffffff",
  "#",
};

static const char * const output_121[] = {
  "{rl%I2nm.|rlw%I2nm.} %4,%1,%h2,%m3,%M3",
  "#",
};

static const char * const output_122[] = {
  "{rl%I2nm.|rlw%I2nm.} %0,%1,%h2,%m3,%M3",
  "#",
};

static const char * const output_124[] = {
  "{rl%I2nm.|rlw%I2nm.} %3,%1,%h2,0xff",
  "#",
};

static const char * const output_125[] = {
  "{rl%I2nm.|rlw%I2nm.} %0,%1,%h2,0xff",
  "#",
};

static const char * const output_127[] = {
  "{rl%I2nm.|rlw%I2nm.} %3,%1,%h2,0xffff",
  "#",
};

static const char * const output_128[] = {
  "{rl%I2nm.|rlw%I2nm.} %0,%1,%h2,0xffff",
  "#",
};

static const char * const output_129[] = {
  "sle %0,%1,%2",
  "{sli|slwi} %0,%1,%h2",
};

static const char * const output_131[] = {
  "sle. %3,%1,%2",
  "{sli.|slwi.} %3,%1,%h2",
  "#",
  "#",
};

static const char * const output_132[] = {
  "{sl|slw}%I2. %3,%1,%h2",
  "#",
};

static const char * const output_133[] = {
  "sle. %0,%1,%2",
  "{sli.|slwi.} %0,%1,%h2",
  "#",
  "#",
};

static const char * const output_134[] = {
  "{sl|slw}%I2. %0,%1,%h2",
  "#",
};

static const char * const output_136[] = {
  "{rlinm.|rlwinm.} %4,%1,%h2,%m3,%M3",
  "#",
};

static const char * const output_137[] = {
  "{rlinm.|rlwinm.} %0,%1,%h2,%m3,%M3",
  "#",
};

static const char * const output_138[] = {
  "sre %0,%1,%2",
  "mr %0,%1",
  "{s%A2i|s%A2wi} %0,%1,%h2",
};

static const char * const output_139[] = {
  "mr %0,%1",
  "{sr|srw}%I2 %0,%1,%h2",
};

static const char * const output_140[] = {
  "sre. %3,%1,%2",
  "mr. %1,%1",
  "{s%A2i.|s%A2wi.} %3,%1,%h2",
  "#",
  "#",
  "#",
};

static const char * const output_141[] = {
  "mr. %1,%1",
  "{sr|srw}%I2. %3,%1,%h2",
  "#",
  "#",
};

static const char * const output_142[] = {
  "sre. %0,%1,%2",
  "mr. %0,%1",
  "{s%A2i.|s%A2wi.} %0,%1,%h2",
  "#",
  "#",
  "#",
};

static const char * const output_143[] = {
  "mr. %0,%1",
  "{sr|srw}%I2. %0,%1,%h2",
  "#",
  "#",
};

static const char * const output_145[] = {
  "{rlinm.|rlwinm.} %4,%1,%s2,%m3,%M3",
  "#",
};

static const char * const output_146[] = {
  "{rlinm.|rlwinm.} %0,%1,%s2,%m3,%M3",
  "#",
};

static const char * const output_148[] = {
  "{rlinm.|rlwinm.} %3,%1,%s2,0xff",
  "#",
};

static const char * const output_149[] = {
  "{rlinm.|rlwinm.} %0,%1,%s2,0xff",
  "#",
};

static const char * const output_151[] = {
  "{rlinm.|rlwinm.} %3,%1,%s2,0xffff",
  "#",
};

static const char * const output_152[] = {
  "{rlinm.|rlwinm.} %0,%1,%s2,0xffff",
  "#",
};

static const char * const output_156[] = {
  "srea %0,%1,%2",
  "{srai|srawi} %0,%1,%h2",
};

static const char * const output_158[] = {
  "srea. %3,%1,%2",
  "{srai.|srawi.} %3,%1,%h2",
  "#",
  "#",
};

static const char * const output_159[] = {
  "{sra|sraw}%I2. %3,%1,%h2",
  "#",
};

static const char * const output_160[] = {
  "srea. %0,%1,%2",
  "{srai.|srawi.} %0,%1,%h2",
  "#",
  "#",
};

static const char * const output_161[] = {
  "{sra|sraw}%I2. %0,%1,%h2",
  "#",
};

static const char *output_162 PARAMS ((rtx *, rtx));

static const char *
output_162 (operands, insn)
     rtx *operands ATTRIBUTE_UNUSED;
     rtx insn ATTRIBUTE_UNUSED;
{

{
  if (REGNO (operands[0]) == REGNO (operands[1]))
    return "";
  else
    return "fmr %0,%1";
}
}

static const char *output_217 PARAMS ((rtx *, rtx));

static const char *
output_217 (operands, insn)
     rtx *operands ATTRIBUTE_UNUSED;
     rtx insn ATTRIBUTE_UNUSED;
{

{
  if (WORDS_BIG_ENDIAN)
    return (GET_CODE (operands[2])) != CONST_INT
	    ? "{a|addc} %L0,%L1,%L2\n\t{ae|adde} %0,%1,%2"
	    : "{ai|addic} %L0,%L1,%2\n\t{a%G2e|add%G2e} %0,%1";
  else
    return (GET_CODE (operands[2])) != CONST_INT
	    ? "{a|addc} %0,%1,%2\n\t{ae|adde} %L0,%L1,%L2"
	    : "{ai|addic} %0,%1,%2\n\t{a%G2e|add%G2e} %L0,%L1";
}
}

static const char *output_218 PARAMS ((rtx *, rtx));

static const char *
output_218 (operands, insn)
     rtx *operands ATTRIBUTE_UNUSED;
     rtx insn ATTRIBUTE_UNUSED;
{

{
  if (WORDS_BIG_ENDIAN)
    return (GET_CODE (operands[1]) != CONST_INT)
	    ? "{sf|subfc} %L0,%L2,%L1\n\t{sfe|subfe} %0,%2,%1"
	    : "{sfi|subfic} %L0,%L2,%1\n\t{sf%G1e|subf%G1e} %0,%2";
  else
    return (GET_CODE (operands[1]) != CONST_INT)
	    ? "{sf|subfc} %0,%2,%1\n\t{sfe|subfe} %L0,%L2,%L1"
	    : "{sfi|subfic} %0,%2,%1\n\t{sf%G1e|subf%G1e} %L0,%L2";
}
}

static const char *output_219 PARAMS ((rtx *, rtx));

static const char *
output_219 (operands, insn)
     rtx *operands ATTRIBUTE_UNUSED;
     rtx insn ATTRIBUTE_UNUSED;
{

{
  return (WORDS_BIG_ENDIAN)
    ? "{sfi|subfic} %L0,%L1,0\n\t{sfze|subfze} %0,%1"
    : "{sfi|subfic} %0,%1,0\n\t{sfze|subfze} %L0,%L1";
}
}

static const char *output_221 PARAMS ((rtx *, rtx));

static const char *
output_221 (operands, insn)
     rtx *operands ATTRIBUTE_UNUSED;
     rtx insn ATTRIBUTE_UNUSED;
{

{
  return (WORDS_BIG_ENDIAN)
    ? "mulhw %0,%1,%2\n\tmullw %L0,%1,%2"
    : "mulhw %L0,%1,%2\n\tmullw %0,%1,%2";
}
}

static const char *output_222 PARAMS ((rtx *, rtx));

static const char *
output_222 (operands, insn)
     rtx *operands ATTRIBUTE_UNUSED;
     rtx insn ATTRIBUTE_UNUSED;
{

{
  return (WORDS_BIG_ENDIAN)
    ? "mulhwu %0,%1,%2\n\tmullw %L0,%1,%2"
    : "mulhwu %L0,%1,%2\n\tmullw %0,%1,%2";
}
}

static const char *output_223 PARAMS ((rtx *, rtx));

static const char *
output_223 (operands, insn)
     rtx *operands ATTRIBUTE_UNUSED;
     rtx insn ATTRIBUTE_UNUSED;
{

{
  return (WORDS_BIG_ENDIAN)
    ? "mulhwu %0,%1,%2\n\tmullw %L0,%1,%2"
    : "mulhwu %L0,%1,%2\n\tmullw %0,%1,%2";
}
}

static const char * const output_228[] = {
  "{sli|slwi} %0,%L1,%h2\n\t{cal %L0,0(0)|li %L0,0}",
  "sl%I2q %L0,%L1,%h2\n\tsll%I2q %0,%1,%h2",
  "sl%I2q %L0,%L1,%h2\n\tsll%I2q %0,%1,%h2",
  "sl%I2q %L0,%L1,%h2\n\tsll%I2q %0,%1,%h2",
};

static const char * const output_229[] = {
  "{s%A2i|s%A2wi} %L0,%1,%h2\n\t{cal %0,0(0)|li %0,0}",
  "sr%I2q %0,%1,%h2\n\tsrl%I2q %L0,%L1,%h2",
  "sr%I2q %0,%1,%h2\n\tsrl%I2q %L0,%L1,%h2",
  "sr%I2q %0,%1,%h2\n\tsrl%I2q %L0,%L1,%h2",
};

static const char * const output_230[] = {
  "{srai|srawi} %0,%1,31\n\t{srai|srawi} %L0,%1,%h2",
  "sraiq %0,%1,%h2\n\tsrliq %L0,%L1,%h2",
};

static const char * const output_231[] = {
  "{srai|srawi} %0,%1,31\n\t{srai|srawi} %L0,%1,%h2",
  "{sri|srwi} %L0,%L1,%h2\n\tinsrwi %L0,%1,%h2,0\n\t{srai|srawi} %0,%1,%h2",
};

static const char * const output_232[] = {
  "add %0,%1,%2",
  "addi %0,%1,%2",
  "addic %0,%1,%2",
  "addis %0,%1,%v2",
};

static const char * const output_233[] = {
  "add. %3,%1,%2",
  "addic. %3,%1,%2",
  "#",
  "#",
};

static const char * const output_234[] = {
  "add. %0,%1,%2",
  "addic. %0,%1,%2",
  "#",
  "#",
};

static const char * const output_236[] = {
  "nor. %2,%1,%1",
  "#",
};

static const char * const output_237[] = {
  "nor. %0,%1,%1",
  "#",
};

static const char * const output_238[] = {
  "subf %0,%2,%1",
  "subfic %0,%2,%1",
};

static const char * const output_239[] = {
  "subf. %3,%2,%1",
  "#",
};

static const char * const output_240[] = {
  "subf. %0,%2,%1",
  "#",
};

static const char * const output_244[] = {
  "neg. %2,%1",
  "#",
};

static const char * const output_245[] = {
  "neg. %0,%1",
  "#",
};

static const char * const output_251[] = {
  "sradi %3,%1,%p2\n\taddze. %3,%3",
  "#",
};

static const char * const output_252[] = {
  "sradi %0,%1,%p2\n\taddze. %0,%0",
  "#",
};

static const char * const output_256[] = {
  "rld%I2cl. %3,%1,%H2,0",
  "#",
};

static const char * const output_257[] = {
  "rld%I2cl. %0,%1,%H2,0",
  "#",
};

static const char * const output_259[] = {
  "rld%I2c%B3. %4,%1,%H2,%S3",
  "#",
};

static const char * const output_260[] = {
  "rld%I2c%B3. %0,%1,%H2,%S3",
  "#",
};

static const char * const output_262[] = {
  "rld%I2cl. %3,%1,%H2,56",
  "#",
};

static const char * const output_263[] = {
  "rld%I2cl. %0,%1,%H2,56",
  "#",
};

static const char * const output_265[] = {
  "rld%I2cl. %3,%1,%H2,48",
  "#",
};

static const char * const output_266[] = {
  "rld%I2cl. %0,%1,%H2,48",
  "#",
};

static const char * const output_268[] = {
  "rld%I2cl. %3,%1,%H2,32",
  "#",
};

static const char * const output_269[] = {
  "rld%I2cl. %0,%1,%H2,32",
  "#",
};

static const char * const output_271[] = {
  "sld%I2. %3,%1,%H2",
  "#",
};

static const char * const output_272[] = {
  "sld%I2. %0,%1,%H2",
  "#",
};

static const char * const output_274[] = {
  "rldic. %4,%1,%H2,%W3",
  "#",
};

static const char * const output_275[] = {
  "rldic. %0,%1,%H2,%W3",
  "#",
};

static const char * const output_277[] = {
  "rldicr. %4,%1,%H2,%S3",
  "#",
};

static const char * const output_278[] = {
  "rldicr. %0,%1,%H2,%S3",
  "#",
};

static const char * const output_280[] = {
  "srd%I2. %3,%1,%H2",
  "#",
};

static const char * const output_281[] = {
  "srd%I2. %0,%1,%H2",
  "#",
};

static const char * const output_283[] = {
  "srad%I2. %3,%1,%H2",
  "#",
};

static const char * const output_284[] = {
  "srad%I2. %0,%1,%H2",
  "#",
};

static const char * const output_285[] = {
  "and %0,%1,%2",
  "rldic%B2 %0,%1,0,%S2",
  "andi. %0,%1,%b2",
  "andis. %0,%1,%u2",
  "#",
};

static const char * const output_286[] = {
  "and. %3,%1,%2",
  "rldic%B2. %3,%1,0,%S2",
  "andi. %3,%1,%b2",
  "andis. %3,%1,%u2",
  "#",
  "#",
  "#",
  "#",
  "#",
  "#",
};

static const char * const output_287[] = {
  "and. %0,%1,%2",
  "rldic%B2. %0,%1,0,%S2",
  "andi. %0,%1,%b2",
  "andis. %0,%1,%u2",
  "#",
  "#",
  "#",
  "#",
  "#",
  "#",
};

static const char * const output_288[] = {
  "%q3 %0,%1,%2",
  "%q3i %0,%1,%b2",
  "%q3is %0,%1,%u2",
};

static const char * const output_289[] = {
  "%q4. %3,%1,%2",
  "#",
};

static const char * const output_290[] = {
  "%q4. %0,%1,%2",
  "#",
};

static const char * const output_292[] = {
  "%q4. %3,%2,%1",
  "#",
};

static const char * const output_293[] = {
  "%q4. %0,%2,%1",
  "#",
};

static const char * const output_295[] = {
  "%q4. %3,%1,%2",
  "#",
};

static const char * const output_296[] = {
  "%q4. %0,%1,%2",
  "#",
};

static const char * const output_298[] = {
  "{cal|la} %0,%2@l(%1)",
  "{ai|addic} %0,%1,%K2",
};

static const char * const output_300[] = {
  "mr %0,%1",
  "{cal|la} %0,%a1",
  "{l%U1%X1|lwz%U1%X1} %0,%1",
  "{st%U0%X0|stw%U0%X0} %1,%0",
  "{lil|li} %0,%1",
  "{liu|lis} %0,%v1",
  "#",
  "{cal|la} %0,%a1",
  "mf%1 %0",
  "mt%0 %1",
  "mt%0 %1",
  "mt%0 %1",
  "{cror 0,0,0|nop}",
};

static const char * const output_301[] = {
  "mr. %0,%1",
  "#",
};

static const char * const output_302[] = {
  "mr %0,%1",
  "lhz%U1%X1 %0,%1",
  "sth%U0%X0 %1,%0",
  "{lil|li} %0,%w1",
  "mf%1 %0",
  "mt%0 %1",
  "mt%0 %1",
  "{cror 0,0,0|nop}",
};

static const char * const output_303[] = {
  "mr %0,%1",
  "lbz%U1%X1 %0,%1",
  "stb%U0%X0 %1,%0",
  "{lil|li} %0,%1",
  "mf%1 %0",
  "mt%0 %1",
  "mt%0 %1",
  "{cror 0,0,0|nop}",
};

static const char * const output_304[] = {
  "mcrf %0,%1",
  "mtcrf 128,%1",
  "{rlinm|rlwinm} %1,%1,%F0,0xffffffff\n\tmtcrf %R0,%1\n\t{rlinm|rlwinm} %1,%1,%f0,0xffffffff",
  "mfcr %0",
  "mfcr %0\n\t{rlinm|rlwinm} %0,%0,%f1,0xf0000000",
  "mr %0,%1",
  "mt%0 %1",
  "mt%0 %1",
  "mf%1 %0",
  "{l%U1%X1|lwz%U1%X1} %0,%1",
  "{st%U0%U1|stw%U0%U1} %1,%0",
};

static const char * const output_305[] = {
  "mr %0,%1",
  "{l%U1%X1|lwz%U1%X1} %0,%1",
  "{st%U0%X0|stw%U0%X0} %1,%0",
  "fmr %0,%1",
  "lfs%U1%X1 %0,%1",
  "stfs%U0%X0 %1,%0",
  "mt%0 %1",
  "mt%0 %1",
  "mf%1 %0",
  "#",
  "#",
};

static const char * const output_306[] = {
  "mr %0,%1",
  "mt%0 %1",
  "mt%0 %1",
  "mf%1 %0",
  "{l%U1%X1|lwz%U1%X1} %0,%1",
  "{st%U0%X0|stw%U0%X0} %1,%0",
  "{lil|li} %0,%1",
  "{liu|lis} %0,%v1",
  "{cal|la} %0,%a1",
  "#",
  "#",
};

static const char *output_307 PARAMS ((rtx *, rtx));

static const char *
output_307 (operands, insn)
     rtx *operands ATTRIBUTE_UNUSED;
     rtx insn ATTRIBUTE_UNUSED;
{

{
  switch (which_alternative)
    {
    default:
      abort ();
    case 0:
      /* We normally copy the low-numbered register first.  However, if
	 the first register operand 0 is the same as the second register
	 of operand 1, we must copy in the opposite order.  */
      if (REGNO (operands[0]) == REGNO (operands[1]) + 1)
	return "mr %L0,%L1\n\tmr %0,%1";
      else
	return "mr %0,%1\n\tmr %L0,%L1";
    case 1:
      if (offsettable_memref_p (operands[1])
	  || (GET_CODE (operands[1]) == MEM
	      && (GET_CODE (XEXP (operands[1], 0)) == LO_SUM
		  || GET_CODE (XEXP (operands[1], 0)) == PRE_INC
		  || GET_CODE (XEXP (operands[1], 0)) == PRE_DEC)))
	{
	  /* If the low-address word is used in the address, we must load
	     it last.  Otherwise, load it first.  Note that we cannot have
	     auto-increment in that case since the address register is
	     known to be dead.  */
	  if (refers_to_regno_p (REGNO (operands[0]), REGNO (operands[0]) + 1,
				 operands[1], 0))
	    return "{l|lwz} %L0,%L1\n\t{l|lwz} %0,%1";
	  else
	    return "{l%U1|lwz%U1} %0,%1\n\t{l|lwz} %L0,%L1";
	}
      else
	{
	  rtx addreg;

	  addreg = find_addr_reg (XEXP (operands[1], 0));
	  if (refers_to_regno_p (REGNO (operands[0]),
				 REGNO (operands[0]) + 1,
				 operands[1], 0))
	    {
	      output_asm_insn ("{cal|la} %0,4(%0)", &addreg);
	      output_asm_insn ("{lx|lwzx} %L0,%1", operands);
	      output_asm_insn ("{cal|la} %0,-4(%0)", &addreg);
	      return "{lx|lwzx} %0,%1";
	    }
	  else
	    {
	      output_asm_insn ("{lx|lwzx} %0,%1", operands);
	      output_asm_insn ("{cal|la} %0,4(%0)", &addreg);
	      output_asm_insn ("{lx|lwzx} %L0,%1", operands);
	      output_asm_insn ("{cal|la} %0,-4(%0)", &addreg);
	      return "";
	    }
	}
    case 2:
      if (offsettable_memref_p (operands[0])
	  || (GET_CODE (operands[0]) == MEM
	      && (GET_CODE (XEXP (operands[0], 0)) == LO_SUM
		  || GET_CODE (XEXP (operands[0], 0)) == PRE_INC
		  || GET_CODE (XEXP (operands[0], 0)) == PRE_DEC)))
	return "{st%U0|stw%U0} %1,%0\n\t{st|stw} %L1,%L0";
      else
	{
	  rtx addreg;

	  addreg = find_addr_reg (XEXP (operands[0], 0));
	  output_asm_insn ("{stx|stwx} %1,%0", operands);
	  output_asm_insn ("{cal|la} %0,4(%0)", &addreg);
	  output_asm_insn ("{stx|stwx} %L1,%0", operands);
	  output_asm_insn ("{cal|la} %0,-4(%0)", &addreg);
	  return "";
	}
    case 3:
      return "fmr %0,%1";
    case 4:
      return "lfd%U1%X1 %0,%1";
    case 5:
      return "stfd%U0%X0 %1,%0";
    case 6:
    case 7:
    case 8:
      return "#";
    }
}
}

static const char *output_308 PARAMS ((rtx *, rtx));

static const char *
output_308 (operands, insn)
     rtx *operands ATTRIBUTE_UNUSED;
     rtx insn ATTRIBUTE_UNUSED;
{

{
  switch (which_alternative)
    {
    default:
      abort ();
    case 0:
      /* We normally copy the low-numbered register first.  However, if
	 the first register operand 0 is the same as the second register of
	 operand 1, we must copy in the opposite order.  */
      if (REGNO (operands[0]) == REGNO (operands[1]) + 1)
	return "mr %L0,%L1\n\tmr %0,%1";
      else
	return "mr %0,%1\n\tmr %L0,%L1";
    case 1:
      /* If the low-address word is used in the address, we must load
	 it last.  Otherwise, load it first.  Note that we cannot have
	 auto-increment in that case since the address register is
	 known to be dead.  */
      if (refers_to_regno_p (REGNO (operands[0]), REGNO (operands[0]) + 1,
			     operands[1], 0))
	return "{l|lwz} %L0,%L1\n\t{l|lwz} %0,%1";
      else
	return "{l%U1|lwz%U1} %0,%1\n\t{l|lwz} %L0,%L1";
    case 2:
      return "{st%U0|stw%U0} %1,%0\n\t{st|stw} %L1,%L0";
    case 3:
    case 4:
    case 5:
      return "#";
    }
}
}

static const char * const output_309[] = {
  "mr %0,%1",
  "ld%U1%X1 %0,%1",
  "std%U0%X0 %1,%0",
  "fmr %0,%1",
  "lfd%U1%X1 %0,%1",
  "stfd%U0%X0 %1,%0",
  "mt%0 %1",
  "mf%1 %0",
  "#",
  "#",
  "#",
};

static const char * const output_310[] = {
  "mr %0,%1",
  "mt%0 %1",
  "mf%1 %0",
  "ld%U1%X1 %0,%1",
  "std%U0%X0 %1,%0",
  "#",
  "#",
  "#",
};

static const char *output_311 PARAMS ((rtx *, rtx));

static const char *
output_311 (operands, insn)
     rtx *operands ATTRIBUTE_UNUSED;
     rtx insn ATTRIBUTE_UNUSED;
{

{
  switch (which_alternative)
    {
    default:
      abort ();
    case 0:
      /* We normally copy the low-numbered register first.  However, if
	 the first register operand 0 is the same as the second register of
	 operand 1, we must copy in the opposite order.  */
      if (REGNO (operands[0]) == REGNO (operands[1]) + 1)
	return "fmr %L0,%L1\n\tfmr %0,%1";
      else
	return "fmr %0,%1\n\tfmr %L0,%L1";
    case 1:
      return "lfd %0,%1\n\tlfd %L0,%Y1";
    case 2:
      return "stfd %1,%0\n\tstfd %L1,%Y0";
    case 3:
    case 4:
    case 5:
      return "#";
    }
}
}

static const char *output_312 PARAMS ((rtx *, rtx));

static const char *
output_312 (operands, insn)
     rtx *operands ATTRIBUTE_UNUSED;
     rtx insn ATTRIBUTE_UNUSED;
{

{
  if (REGNO (operands[0]) == REGNO (operands[1]))
    return "fsub %L0,%L0,%L0";
  else
    return "fmr %0,%1\n\tfsub %L0,%L0,%L0";
}
}

static const char *output_313 PARAMS ((rtx *, rtx));

static const char *
output_313 (operands, insn)
     rtx *operands ATTRIBUTE_UNUSED;
     rtx insn ATTRIBUTE_UNUSED;
{

{
  if (REGNO (operands[0]) == REGNO (operands[1]))
    return "fsub %L0,%L0,%L0";
  else
    return "fmr %0,%1\n\tfsub %L0,%L0,%L0";
}
}

static const char *output_320 PARAMS ((rtx *, rtx));

static const char *
output_320 (operands, insn)
     rtx *operands ATTRIBUTE_UNUSED;
     rtx insn ATTRIBUTE_UNUSED;
{

{
  if (REGNO (operands[0]) == REGNO (operands[1]) + 1)
    return "fneg %L0,%L1\n\tfneg %0,%1";
  else
    return "fneg %0,%1\n\tfneg %L0,%L1";
}
}

static const char *output_321 PARAMS ((rtx *, rtx));

static const char *
output_321 (operands, insn)
     rtx *operands ATTRIBUTE_UNUSED;
     rtx insn ATTRIBUTE_UNUSED;
{

{
  if (REGNO (operands[0]) == REGNO (operands[1]) + 1)
    return "fabs %L0,%L1\n\tfabs %0,%1";
  else
    return "fabs %0,%1\n\tfabs %L0,%L1";
}
}

static const char *output_322 PARAMS ((rtx *, rtx));

static const char *
output_322 (operands, insn)
     rtx *operands ATTRIBUTE_UNUSED;
     rtx insn ATTRIBUTE_UNUSED;
{

{
  if (REGNO (operands[0]) == REGNO (operands[1]) + 1)
    return "fnabs %L0,%L1\n\tfnabs %0,%1";
  else
    return "fnabs %0,%1\n\tfnabs %L0,%L1";
}
}

static const char *output_323 PARAMS ((rtx *, rtx));

static const char *
output_323 (operands, insn)
     rtx *operands ATTRIBUTE_UNUSED;
     rtx insn ATTRIBUTE_UNUSED;
{

{
  switch (which_alternative)
    {
    default:
      abort ();
    case 0:
      /* We normally copy the low-numbered register first.  However, if
	 the first register operand 0 is the same as the second register of
	 operand 1, we must copy in the opposite order.  */
      if (REGNO (operands[0]) == REGNO (operands[1]) + 1)
	return "mr %L0,%L1\n\tmr %0,%1";
      else
	return "mr %0,%1\n\tmr %L0,%L1";
    case 1:
      /* If the low-address word is used in the address, we must load it
	 last.  Otherwise, load it first.  Note that we cannot have
	 auto-increment in that case since the address register is known to be
	 dead.  */
      if (refers_to_regno_p (REGNO (operands[0]), REGNO (operands[0]) + 1,
			     operands[1], 0))
	return "{l|lwz} %L0,%L1\n\t{l|lwz} %0,%1";
      else
	return "{l%U1|lwz%U1} %0,%1\n\t{l|lwz} %L0,%L1";
    case 2:
      return "{st%U0|stw%U0} %1,%0\n\t{st|stw} %L1,%L0";
    case 3:
      return "fmr %0,%1";
    case 4:
      return "lfd%U1%X1 %0,%1";
    case 5:
      return "stfd%U0%X0 %1,%0";
    case 6:
    case 7:
    case 8:
    case 9:
    case 10:
      return "#";
    }
}
}

static const char * const output_324[] = {
  "mr %0,%1",
  "ld%U1%X1 %0,%1",
  "std%U0%X0 %1,%0",
  "li %0,%1",
  "lis %0,%v1",
  "#",
  "{cal|la} %0,%a1",
  "fmr %0,%1",
  "lfd%U1%X1 %0,%1",
  "stfd%U0%X0 %1,%0",
  "mf%1 %0",
  "mt%0 %1",
  "{cror 0,0,0|nop}",
};

static const char * const output_325[] = {
  "mr. %0,%1",
  "#",
};

static const char *output_326 PARAMS ((rtx *, rtx));

static const char *
output_326 (operands, insn)
     rtx *operands ATTRIBUTE_UNUSED;
     rtx insn ATTRIBUTE_UNUSED;
{

{
  switch (which_alternative)
    {
    default:
      abort ();

    case 0:
      return "{stsi|stswi} %1,%P0,16";

    case 1:
      return "{st%U0|stw%U0} %1,%0\n\t{st|stw} %L1,%L0\n\t{st|stw} %Y1,%Y0\n\t{st|stw} %Z1,%Z0";

    case 2:
      /* Normally copy registers with lowest numbered register copied first.
	 But copy in the other order if the first register of the output
	 is the second, third, or fourth register in the input.  */
      if (REGNO (operands[0]) >= REGNO (operands[1]) + 1
	  && REGNO (operands[0]) <= REGNO (operands[1]) + 3)
	return "mr %Z0,%Z1\n\tmr %Y0,%Y1\n\tmr %L0,%L1\n\tmr %0,%1";
      else
	return "mr %0,%1\n\tmr %L0,%L1\n\tmr %Y0,%Y1\n\tmr %Z0,%Z1";
    case 3:
      /* If the address is not used in the output, we can use lsi.  Otherwise,
	 fall through to generating four loads.  */
      if (! reg_overlap_mentioned_p (operands[0], operands[1]))
	return "{lsi|lswi} %0,%P1,16";
      /* ... fall through ...  */
    case 4:
      /* If the address register is the same as the register for the lowest-
	 addressed word, load it last.  Similarly for the next two words.
	 Otherwise load lowest address to highest.  */
      if (refers_to_regno_p (REGNO (operands[0]), REGNO (operands[0]) + 1,
			     operands[1], 0))
	return "{l|lwz} %L0,%L1\n\t{l|lwz} %Y0,%Y1\n\t{l|lwz} %Z0,%Z1\n\t{l|lwz} %0,%1";
      else if (refers_to_regno_p (REGNO (operands[0]) + 1,
				  REGNO (operands[0]) + 2, operands[1], 0))
	return "{l|lwz} %0,%1\n\t{l|lwz} %Y0,%Y1\n\t{l|lwz} %Z0,%Z1\n\t{l|lwz} %L0,%L1";
      else if (refers_to_regno_p (REGNO (operands[0]) + 2,
				  REGNO (operands[0]) + 3, operands[1], 0))
	return "{l|lwz} %0,%1\n\t{l|lwz} %L0,%L1\n\t{l|lwz} %Z0,%Z1\n\t{l|lwz} %Y0,%Y1";
      else
	return "{l%U1|lwz%U1} %0,%1\n\t{l|lwz} %L0,%L1\n\t{l|lwz} %Y0,%Y1\n\t{l|lwz} %Z0,%Z1";
    }
}
}

static const char *output_327 PARAMS ((rtx *, rtx));

static const char *
output_327 (operands, insn)
     rtx *operands ATTRIBUTE_UNUSED;
     rtx insn ATTRIBUTE_UNUSED;
{

{
  switch (which_alternative)
    {
    default:
      abort ();

    case 0:
      return "{stsi|stswi} %1,%P0,16";
    case 1:
      return "{st%U0|stw%U0} %1,%0\n\t{st|stw} %L1,%L0\n\t{st|stw} %Y1,%Y0\n\t{st|stw} %Z1,%Z0";
    case 2:
      /* Normally copy registers with lowest numbered register copied first.
	 But copy in the other order if the first register of the output
	 is the second, third, or fourth register in the input.  */
      if (REGNO (operands[0]) >= REGNO (operands[1]) + 1
	  && REGNO (operands[0]) <= REGNO (operands[1]) + 3)
	return "mr %Z0,%Z1\n\tmr %Y0,%Y1\n\tmr %L0,%L1\n\tmr %0,%1";
      else
	return "mr %0,%1\n\tmr %L0,%L1\n\tmr %Y0,%Y1\n\tmr %Z0,%Z1";
    case 3:
      /* If the address is not used in the output, we can use lsi.  Otherwise,
	 fall through to generating four loads.  */
      if (! reg_overlap_mentioned_p (operands[0], operands[1]))
	return "{lsi|lswi} %0,%P1,16";
      /* ... fall through ...  */
    case 4:
      /* If the address register is the same as the register for the lowest-
	 addressed word, load it last.  Similarly for the next two words.
	 Otherwise load lowest address to highest.  */
      if (refers_to_regno_p (REGNO (operands[0]), REGNO (operands[0]) + 1,
			     operands[1], 0))
	return "{l|lwz} %L0,%L1\n\t{l|lwz} %Y0,%Y1\n\t{l|lwz} %Z0,%Z1\n\t{l|lwz} %0,%1";
      else if (refers_to_regno_p (REGNO (operands[0]) + 1,
				  REGNO (operands[0]) + 2, operands[1], 0))
	return "{l|lwz} %0,%1\n\t{l|lwz} %Y0,%Y1\n\t{l|lwz} %Z0,%Z1\n\t{l|lwz} %L0,%L1";
      else if (refers_to_regno_p (REGNO (operands[0]) + 2,
				  REGNO (operands[0]) + 3, operands[1], 0))
	return "{l|lwz} %0,%1\n\t{l|lwz} %L0,%L1\n\t{l|lwz} %Z0,%Z1\n\t{l|lwz} %Y0,%Y1";
      else
	return "{l%U1|lwz%U1} %0,%1\n\t{l|lwz} %L0,%L1\n\t{l|lwz} %Y0,%Y1\n\t{l|lwz} %Z0,%Z1";
    }
}
}

static const char *output_328 PARAMS ((rtx *, rtx));

static const char *
output_328 (operands, insn)
     rtx *operands ATTRIBUTE_UNUSED;
     rtx insn ATTRIBUTE_UNUSED;
{

{
  switch (which_alternative)
    {
    default:
      abort ();
    case 0:
      /* We normally copy the low-numbered register first.  However, if
	 the first register operand 0 is the same as the second register of
	 operand 1, we must copy in the opposite order.  */
      if (REGNO (operands[0]) == REGNO (operands[1]) + 1)
	return "mr %L0,%L1\n\tmr %0,%1";
      else
	return "mr %0,%1\n\tmr %L0,%L1";
    case 1:
      /* If the low-address word is used in the address, we must load it
	 last.  Otherwise, load it first.  Note that we cannot have
	 auto-increment in that case since the address register is known to be
	 dead.  */
      if (refers_to_regno_p (REGNO (operands[0]), REGNO (operands[0]) + 1,
			     operands[1], 0))
	return "ld %L0,%L1\n\tld %0,%1";
      else
	return "ld%U1 %0,%1\n\tld %L0,%L1";
    case 2:
      return "std%U0 %1,%0\n\tstd %L1,%L0";
    }
}
}

static const char *output_329 PARAMS ((rtx *, rtx));

static const char *
output_329 (operands, insn)
     rtx *operands ATTRIBUTE_UNUSED;
     rtx insn ATTRIBUTE_UNUSED;
{

{ return rs6000_output_load_multiple (operands); }
}

static const char *output_330 PARAMS ((rtx *, rtx));

static const char *
output_330 (operands, insn)
     rtx *operands ATTRIBUTE_UNUSED;
     rtx insn ATTRIBUTE_UNUSED;
{

{ return rs6000_output_load_multiple (operands); }
}

static const char *output_331 PARAMS ((rtx *, rtx));

static const char *
output_331 (operands, insn)
     rtx *operands ATTRIBUTE_UNUSED;
     rtx insn ATTRIBUTE_UNUSED;
{

{ return rs6000_output_load_multiple (operands); }
}

static const char *output_332 PARAMS ((rtx *, rtx));

static const char *
output_332 (operands, insn)
     rtx *operands ATTRIBUTE_UNUSED;
     rtx insn ATTRIBUTE_UNUSED;
{

{ return rs6000_output_load_multiple (operands); }
}

static const char *output_333 PARAMS ((rtx *, rtx));

static const char *
output_333 (operands, insn)
     rtx *operands ATTRIBUTE_UNUSED;
     rtx insn ATTRIBUTE_UNUSED;
{

{ return rs6000_output_load_multiple (operands); }
}

static const char *output_334 PARAMS ((rtx *, rtx));

static const char *
output_334 (operands, insn)
     rtx *operands ATTRIBUTE_UNUSED;
     rtx insn ATTRIBUTE_UNUSED;
{

{ return rs6000_output_load_multiple (operands); }
}

static const char * const output_351[] = {
  "ldux %3,%0,%2",
  "ldu %3,%2(%0)",
};

static const char * const output_353[] = {
  "stdux %3,%0,%2",
  "stdu %3,%2(%0)",
};

static const char * const output_354[] = {
  "{lux|lwzux} %3,%0,%2",
  "{lu|lwzu} %3,%2(%0)",
};

static const char * const output_355[] = {
  "{stux|stwux} %3,%0,%2",
  "{stu|stwu} %3,%2(%0)",
};

static const char * const output_356[] = {
  "lhzux %3,%0,%2",
  "lhzu %3,%2(%0)",
};

static const char * const output_357[] = {
  "lhzux %3,%0,%2",
  "lhzu %3,%2(%0)",
};

static const char * const output_358[] = {
  "lhaux %3,%0,%2",
  "lhau %3,%2(%0)",
};

static const char * const output_359[] = {
  "sthux %3,%0,%2",
  "sthu %3,%2(%0)",
};

static const char * const output_360[] = {
  "lbzux %3,%0,%2",
  "lbzu %3,%2(%0)",
};

static const char * const output_361[] = {
  "lbzux %3,%0,%2",
  "lbzu %3,%2(%0)",
};

static const char * const output_362[] = {
  "stbux %3,%0,%2",
  "stbu %3,%2(%0)",
};

static const char * const output_363[] = {
  "lfsux %3,%0,%2",
  "lfsu %3,%2(%0)",
};

static const char * const output_364[] = {
  "stfsux %3,%0,%2",
  "stfsu %3,%2(%0)",
};

static const char * const output_365[] = {
  "{lux|lwzux} %3,%0,%2",
  "{lu|lwzu} %3,%2(%0)",
};

static const char * const output_366[] = {
  "{stux|stwux} %3,%0,%2",
  "{stu|stwu} %3,%2(%0)",
};

static const char * const output_367[] = {
  "lfdux %3,%0,%2",
  "lfdu %3,%2(%0)",
};

static const char * const output_368[] = {
  "stfdux %3,%0,%2",
  "stfdu %3,%2(%0)",
};

static const char *output_369 PARAMS ((rtx *, rtx));

static const char *
output_369 (operands, insn)
     rtx *operands ATTRIBUTE_UNUSED;
     rtx insn ATTRIBUTE_UNUSED;
{

{
  char buf[30];
  ASM_GENERATE_INTERNAL_LABEL (buf, "LCTOC", 1);
  operands[1] = gen_rtx_SYMBOL_REF (Pmode, ggc_strdup (buf));
  operands[2] = gen_rtx_REG (Pmode, 2);
  return "{l|lwz} %0,%1(%2)";
}
}

static const char *output_370 PARAMS ((rtx *, rtx));

static const char *
output_370 (operands, insn)
     rtx *operands ATTRIBUTE_UNUSED;
     rtx insn ATTRIBUTE_UNUSED;
{

{
  char buf[30];
#ifdef TARGET_RELOCATABLE
  ASM_GENERATE_INTERNAL_LABEL (buf, "LCTOC",
			       !TARGET_MINIMAL_TOC || TARGET_RELOCATABLE);
#else
  ASM_GENERATE_INTERNAL_LABEL (buf, "LCTOC", 1);
#endif
  if (TARGET_ELF)
    strcat (buf, "@toc");
  operands[1] = gen_rtx_SYMBOL_REF (Pmode, ggc_strdup (buf));
  operands[2] = gen_rtx_REG (Pmode, 2);
  return "ld %0,%1(%2)";
}
}

static const char *output_377 PARAMS ((rtx *, rtx));

static const char *
output_377 (operands, insn)
     rtx *operands ATTRIBUTE_UNUSED;
     rtx insn ATTRIBUTE_UNUSED;
{

{
  if (INTVAL (operands[2]) & CALL_V4_SET_FP_ARGS)
    output_asm_insn ("crxor 6,6,6", operands);

  else if (INTVAL (operands[2]) & CALL_V4_CLEAR_FP_ARGS)
    output_asm_insn ("creqv 6,6,6", operands);

  return (DEFAULT_ABI == ABI_V4 && flag_pic) ? "bl %z0@local" : "bl %z0";
}
}

static const char *output_378 PARAMS ((rtx *, rtx));

static const char *
output_378 (operands, insn)
     rtx *operands ATTRIBUTE_UNUSED;
     rtx insn ATTRIBUTE_UNUSED;
{

{
  if (INTVAL (operands[2]) & CALL_V4_SET_FP_ARGS)
    output_asm_insn ("crxor 6,6,6", operands);

  else if (INTVAL (operands[2]) & CALL_V4_CLEAR_FP_ARGS)
    output_asm_insn ("creqv 6,6,6", operands);

  return (DEFAULT_ABI == ABI_V4 && flag_pic) ? "bl %z0@local" : "bl %z0";
}
}

static const char *output_379 PARAMS ((rtx *, rtx));

static const char *
output_379 (operands, insn)
     rtx *operands ATTRIBUTE_UNUSED;
     rtx insn ATTRIBUTE_UNUSED;
{

{
  if (INTVAL (operands[3]) & CALL_V4_SET_FP_ARGS)
    output_asm_insn ("crxor 6,6,6", operands);

  else if (INTVAL (operands[3]) & CALL_V4_CLEAR_FP_ARGS)
    output_asm_insn ("creqv 6,6,6", operands);

  return (DEFAULT_ABI == ABI_V4 && flag_pic) ? "bl %z1@local" : "bl %z1";
}
}

static const char *output_380 PARAMS ((rtx *, rtx));

static const char *
output_380 (operands, insn)
     rtx *operands ATTRIBUTE_UNUSED;
     rtx insn ATTRIBUTE_UNUSED;
{

{
  if (INTVAL (operands[3]) & CALL_V4_SET_FP_ARGS)
    output_asm_insn ("crxor 6,6,6", operands);

  else if (INTVAL (operands[3]) & CALL_V4_CLEAR_FP_ARGS)
    output_asm_insn ("creqv 6,6,6", operands);

  return (DEFAULT_ABI == ABI_V4 && flag_pic) ? "bl %z1@local" : "bl %z1";
}
}

static const char *output_389 PARAMS ((rtx *, rtx));

static const char *
output_389 (operands, insn)
     rtx *operands ATTRIBUTE_UNUSED;
     rtx insn ATTRIBUTE_UNUSED;
{
{
  if (INTVAL (operands[2]) & CALL_V4_SET_FP_ARGS)
    output_asm_insn ("crxor 6,6,6", operands);

  else if (INTVAL (operands[2]) & CALL_V4_CLEAR_FP_ARGS)
    output_asm_insn ("creqv 6,6,6", operands);

  return "b%T0l";
}
}

static const char *output_390 PARAMS ((rtx *, rtx));

static const char *
output_390 (operands, insn)
     rtx *operands ATTRIBUTE_UNUSED;
     rtx insn ATTRIBUTE_UNUSED;
{
{
  if (INTVAL (operands[2]) & CALL_V4_SET_FP_ARGS)
    output_asm_insn ("crxor 6,6,6", operands);

  else if (INTVAL (operands[2]) & CALL_V4_CLEAR_FP_ARGS)
    output_asm_insn ("creqv 6,6,6", operands);

  return (DEFAULT_ABI == ABI_V4 && flag_pic) ? "bl %z0@plt" : "bl %z0";
}
}

static const char *output_391 PARAMS ((rtx *, rtx));

static const char *
output_391 (operands, insn)
     rtx *operands ATTRIBUTE_UNUSED;
     rtx insn ATTRIBUTE_UNUSED;
{
{
  if (INTVAL (operands[3]) & CALL_V4_SET_FP_ARGS)
    output_asm_insn ("crxor 6,6,6", operands);

  else if (INTVAL (operands[3]) & CALL_V4_CLEAR_FP_ARGS)
    output_asm_insn ("creqv 6,6,6", operands);

  return "b%T1l";
}
}

static const char *output_392 PARAMS ((rtx *, rtx));

static const char *
output_392 (operands, insn)
     rtx *operands ATTRIBUTE_UNUSED;
     rtx insn ATTRIBUTE_UNUSED;
{
{
  if (INTVAL (operands[3]) & CALL_V4_SET_FP_ARGS)
    output_asm_insn ("crxor 6,6,6", operands);

  else if (INTVAL (operands[3]) & CALL_V4_CLEAR_FP_ARGS)
    output_asm_insn ("creqv 6,6,6", operands);

  return (DEFAULT_ABI == ABI_V4 && flag_pic) ? "bl %z1@plt" : "bl %z1";
}
}

static const char *output_393 PARAMS ((rtx *, rtx));

static const char *
output_393 (operands, insn)
     rtx *operands ATTRIBUTE_UNUSED;
     rtx insn ATTRIBUTE_UNUSED;
{

{
  if (INTVAL (operands[2]) & CALL_V4_SET_FP_ARGS)
    output_asm_insn ("crxor 6,6,6", operands);

  else if (INTVAL (operands[2]) & CALL_V4_CLEAR_FP_ARGS)
    output_asm_insn ("creqv 6,6,6", operands);

  return (DEFAULT_ABI == ABI_V4 && flag_pic) ? "b %z0@local" : "b %z0";
}
}

static const char *output_394 PARAMS ((rtx *, rtx));

static const char *
output_394 (operands, insn)
     rtx *operands ATTRIBUTE_UNUSED;
     rtx insn ATTRIBUTE_UNUSED;
{

{
  if (INTVAL (operands[2]) & CALL_V4_SET_FP_ARGS)
    output_asm_insn ("crxor 6,6,6", operands);

  else if (INTVAL (operands[2]) & CALL_V4_CLEAR_FP_ARGS)
    output_asm_insn ("creqv 6,6,6", operands);

  return (DEFAULT_ABI == ABI_V4 && flag_pic) ? "b %z0@local" : "b %z0";
}
}

static const char *output_395 PARAMS ((rtx *, rtx));

static const char *
output_395 (operands, insn)
     rtx *operands ATTRIBUTE_UNUSED;
     rtx insn ATTRIBUTE_UNUSED;
{

{
  if (INTVAL (operands[3]) & CALL_V4_SET_FP_ARGS)
    output_asm_insn ("crxor 6,6,6", operands);

  else if (INTVAL (operands[3]) & CALL_V4_CLEAR_FP_ARGS)
    output_asm_insn ("creqv 6,6,6", operands);

  return (DEFAULT_ABI == ABI_V4 && flag_pic) ? "b %z1@local" : "b %z1";
}
}

static const char *output_396 PARAMS ((rtx *, rtx));

static const char *
output_396 (operands, insn)
     rtx *operands ATTRIBUTE_UNUSED;
     rtx insn ATTRIBUTE_UNUSED;
{

{
  if (INTVAL (operands[3]) & CALL_V4_SET_FP_ARGS)
    output_asm_insn ("crxor 6,6,6", operands);

  else if (INTVAL (operands[3]) & CALL_V4_CLEAR_FP_ARGS)
    output_asm_insn ("creqv 6,6,6", operands);

  return (DEFAULT_ABI == ABI_V4 && flag_pic) ? "b %z1@local" : "b %z1";
}
}

static const char *output_401 PARAMS ((rtx *, rtx));

static const char *
output_401 (operands, insn)
     rtx *operands ATTRIBUTE_UNUSED;
     rtx insn ATTRIBUTE_UNUSED;
{

{
  if (INTVAL (operands[2]) & CALL_V4_SET_FP_ARGS)
    output_asm_insn ("crxor 6,6,6", operands);

  else if (INTVAL (operands[2]) & CALL_V4_CLEAR_FP_ARGS)
    output_asm_insn ("creqv 6,6,6", operands);

  return (DEFAULT_ABI == ABI_V4 && flag_pic) ? "b %z0@plt" : "b %z0";
}
}

static const char *output_402 PARAMS ((rtx *, rtx));

static const char *
output_402 (operands, insn)
     rtx *operands ATTRIBUTE_UNUSED;
     rtx insn ATTRIBUTE_UNUSED;
{

{
  if (INTVAL (operands[2]) & CALL_V4_SET_FP_ARGS)
    output_asm_insn ("crxor 6,6,6", operands);

  else if (INTVAL (operands[2]) & CALL_V4_CLEAR_FP_ARGS)
    output_asm_insn ("creqv 6,6,6", operands);

  return (DEFAULT_ABI == ABI_V4 && flag_pic) ? "b %z1@plt" : "b %z1";
}
}

static const char * const output_415[] = {
  "%D1mfcr %3\n\t{rlinm.|rlwinm.} %3,%3,%J1,1",
  "#",
};

static const char *output_416 PARAMS ((rtx *, rtx));

static const char *
output_416 (operands, insn)
     rtx *operands ATTRIBUTE_UNUSED;
     rtx insn ATTRIBUTE_UNUSED;
{

{
  int is_bit = ccr_bit (operands[1], 1);
  int put_bit = 31 - (INTVAL (operands[3]) & 31);
  int count;

  if (is_bit >= put_bit)
    count = is_bit - put_bit;
  else
    count = 32 - (put_bit - is_bit);

  operands[4] = GEN_INT (count);
  operands[5] = GEN_INT (put_bit);

  return "%D1mfcr %0\n\t{rlinm|rlwinm} %0,%0,%4,%5,%5";
}
}

static const char *output_417 PARAMS ((rtx *, rtx));

static const char *
output_417 (operands, insn)
     rtx *operands ATTRIBUTE_UNUSED;
     rtx insn ATTRIBUTE_UNUSED;
{

{
  int is_bit = ccr_bit (operands[1], 1);
  int put_bit = 31 - (INTVAL (operands[3]) & 31);
  int count;

  /* Force split for non-cc0 compare.  */
  if (which_alternative == 1)
     return "#";

  if (is_bit >= put_bit)
    count = is_bit - put_bit;
  else
    count = 32 - (put_bit - is_bit);

  operands[5] = GEN_INT (count);
  operands[6] = GEN_INT (put_bit);

  return "%D1mfcr %4\n\t{rlinm.|rlwinm.} %4,%4,%5,%6,%6";
}
}

static const char * const output_418[] = {
  "xor %0,%1,%2\n\t{sfi|subfic} %3,%0,0\n\t{ae|adde} %0,%3,%0",
  "{sfi|subfic} %3,%1,0\n\t{ae|adde} %0,%3,%1",
  "{xoril|xori} %0,%1,%b2\n\t{sfi|subfic} %3,%0,0\n\t{ae|adde} %0,%3,%0",
  "{xoriu|xoris} %0,%1,%u2\n\t{sfi|subfic} %3,%0,0\n\t{ae|adde} %0,%3,%0",
  "{sfi|subfic} %0,%1,%2\n\t{sfi|subfic} %3,%0,0\n\t{ae|adde} %0,%3,%0",
};

static const char * const output_419[] = {
  "xor %0,%1,%2\n\tsubfic %3,%0,0\n\tadde %0,%3,%0",
  "subfic %3,%1,0\n\tadde %0,%3,%1",
  "xori %0,%1,%b2\n\tsubfic %3,%0,0\n\tadde %0,%3,%0",
  "xoris %0,%1,%u2\n\tsubfic %3,%0,0\n\tadde %0,%3,%0",
  "subfic %0,%1,%2\n\tsubfic %3,%0,0\n\tadde %0,%3,%0",
};

static const char * const output_420[] = {
  "xor %0,%1,%2\n\t{sfi|subfic} %3,%0,0\n\t{ae.|adde.} %0,%3,%0",
  "{sfi|subfic} %3,%1,0\n\t{ae.|adde.} %0,%3,%1",
  "{xoril|xori} %0,%1,%b2\n\t{sfi|subfic} %3,%0,0\n\t{ae.|adde.} %0,%3,%0",
  "{xoriu|xoris} %0,%1,%u2\n\t{sfi|subfic} %3,%0,0\n\t{ae.|adde.} %0,%3,%0",
  "{sfi|subfic} %0,%1,%2\n\t{sfi|subfic} %3,%0,0\n\t{ae.|adde.} %0,%3,%0",
  "#",
  "#",
  "#",
  "#",
  "#",
};

static const char * const output_421[] = {
  "xor %0,%1,%2\n\tsubfic %3,%0,0\n\tadde. %0,%3,%0",
  "subfic %3,%1,0\n\tadde. %0,%3,%1",
  "xori %0,%1,%b2\n\tsubfic %3,%0,0\n\tadde. %0,%3,%0",
  "xoris %0,%1,%u2\n\tsubfic %3,%0,0\n\tadde. %0,%3,%0",
  "subfic %0,%1,%2\n\tsubfic %3,%0,0\n\tadde. %0,%3,%0",
  "#",
  "#",
  "#",
  "#",
  "#",
};

static const char * const output_422[] = {
  "xor %0,%1,%2\n\t{sfi|subfic} %0,%0,0\n\t{aze|addze} %0,%3",
  "{sfi|subfic} %0,%1,0\n\t{aze|addze} %0,%3",
  "{xoril|xori} %0,%1,%b2\n\t{sfi|subfic} %0,%0,0\n\t{aze|addze} %0,%3",
  "{xoriu|xoris} %0,%1,%u2\n\t{sfi|subfic} %0,%0,0\n\t{aze|addze} %0,%3",
  "{sfi|subfic} %0,%1,%2\n\t{sfi|subfic} %0,%0,0\n\t{aze|addze} %0,%3",
};

static const char * const output_423[] = {
  "xor %4,%1,%2\n\t{sfi|subfic} %4,%4,0\n\t{aze.|addze.} %4,%3",
  "{sfi|subfic} %4,%1,0\n\t{aze.|addze.} %4,%3",
  "{xoril|xori} %4,%1,%b2\n\t{sfi|subfic} %4,%4,0\n\t{aze.|addze.} %4,%3",
  "{xoriu|xoris} %4,%1,%u2\n\t{sfi|subfic} %4,%4,0\n\t{aze.|addze.} %4,%3",
  "{sfi|subfic} %4,%1,%2\n\t{sfi|subfic} %4,%4,0\n\t{aze.|addze.} %4,%3",
  "#",
  "#",
  "#",
  "#",
  "#",
};

static const char * const output_424[] = {
  "xor %0,%1,%2\n\t{sfi|subfic} %0,%0,0\n\t{aze.|addze.} %0,%3",
  "{sfi|subfic} %0,%1,0\n\t{aze.|addze.} %0,%3",
  "{xoril|xori} %0,%1,%b2\n\t{sfi|subfic} %0,%0,0\n\t{aze.|addze.} %0,%3",
  "{xoriu|xoris} %0,%1,%u2\n\t{sfi|subfic} %0,%0,0\n\t{aze.|addze.} %0,%3",
  "{sfi|subfic} %0,%1,%2\n\t{sfi|subfic} %0,%0,0\n\t{aze.|addze.} %0,%3",
  "#",
  "#",
  "#",
  "#",
  "#",
};

static const char * const output_425[] = {
  "xor %0,%1,%2\n\t{ai|addic} %0,%0,-1\n\t{sfe|subfe} %0,%0,%0",
  "{ai|addic} %0,%1,-1\n\t{sfe|subfe} %0,%0,%0",
  "{xoril|xori} %0,%1,%b2\n\t{ai|addic} %0,%0,-1\n\t{sfe|subfe} %0,%0,%0",
  "{xoriu|xoris} %0,%1,%u2\n\t{ai|addic} %0,%0,-1\n\t{sfe|subfe} %0,%0,%0",
  "{sfi|subfic} %0,%1,%2\n\t{ai|addic} %0,%0,-1\n\t{sfe|subfe} %0,%0,%0",
};

static const char * const output_430[] = {
  "{ai|addic} %3,%1,-1\n\t{aze.|addze.} %3,%2",
  "#",
};

static const char * const output_431[] = {
  "addic %3,%1,-1\n\taddze. %3,%2",
  "#",
};

static const char * const output_432[] = {
  "{ai|addic} %3,%1,-1\n\t{aze.|addze.} %0,%2",
  "#",
};

static const char * const output_433[] = {
  "addic %3,%1,-1\n\taddze. %0,%2",
  "#",
};

static const char * const output_434[] = {
  "doz %3,%2,%1\n\t{sfi|subfic} %0,%3,0\n\t{ae|adde} %0,%0,%3",
  "{ai|addic} %0,%1,-1\n\t{aze|addze} %0,%0\n\t{sri|srwi} %0,%0,31",
};

static const char * const output_435[] = {
  "doz %3,%2,%1\n\t{sfi|subfic} %0,%3,0\n\t{ae.|adde.} %0,%0,%3",
  "{ai|addic} %0,%1,-1\n\t{aze|addze} %0,%0\n\t{sri.|srwi.} %0,%0,31",
  "#",
  "#",
};

static const char * const output_436[] = {
  "doz %0,%2,%1\n\t{sfi|subfic} %0,%0,0\n\t{aze|addze} %0,%3",
  "{srai|srawi} %0,%1,31\n\t{sf|subfc} %0,%1,%0\n\t{aze|addze} %0,%3",
};

static const char * const output_437[] = {
  "doz %4,%2,%1\n\t{sfi|subfic} %4,%4,0\n\t{aze.|addze.} %4,%3",
  "{srai|srawi} %4,%1,31\n\t{sf|subfc} %4,%1,%4\n\t{aze.|addze.} %4,%3",
  "#",
  "#",
};

static const char * const output_438[] = {
  "doz %0,%2,%1\n\t{sfi|subfic} %0,%0,0\n\t{aze.|addze.} %0,%3",
  "{srai|srawi} %0,%1,31\n\t{sf|subfc} %0,%1,%0\n\t{aze.|addze.} %0,%3",
  "#",
  "#",
};

static const char * const output_439[] = {
  "doz %0,%2,%1\n\t{ai|addic} %0,%0,-1\n\t{sfe|subfe} %0,%0,%0",
  "{ai|addic} %0,%1,-1\n\t{aze|addze} %0,%0\n\t{srai|srawi} %0,%0,31",
};

static const char * const output_442[] = {
  "subf%I2c %0,%1,%2\n\tli %0,0\n\tadde. %0,%0,%0",
  "#",
};

static const char * const output_443[] = {
  "{sf%I2|subf%I2c} %0,%1,%2\n\t{cal %0,0(0)|li %0,0}\n\t{ae.|adde.} %0,%0,%0",
  "#",
};

static const char * const output_444[] = {
  "subf%I2c %0,%1,%2\n\tli %0,0\n\tadde. %0,%0,%0",
  "#",
};

static const char * const output_446[] = {
  "{sf%I2|subf%I2c} %4,%1,%2\n\t{aze.|addze.} %4,%3",
  "#",
};

static const char * const output_447[] = {
  "{sf%I2|subf%I2c} %0,%1,%2\n\t{aze.|addze.} %0,%3",
  "#",
};

static const char * const output_450[] = {
  "{sf%I2|subf%I2c} %4,%1,%2\n\t{sfe|subfe} %4,%4,%4\n\tandc. %4,%3,%4",
  "#",
};

static const char * const output_451[] = {
  "{sf%I2|subf%I2c} %0,%1,%2\n\t{sfe|subfe} %0,%0,%0\n\tandc. %0,%3,%0",
  "#",
};

static const char * const output_453[] = {
  "doz%I2 %0,%1,%2\n\tnabs %0,%0\n\t{sri.|srwi.} %0,%0,31",
  "#",
};

static const char * const output_455[] = {
  "doz%I2 %4,%1,%2\n\t{ai|addic} %4,%4,-1\n\t{aze.|addze.} %4,%3",
  "#",
};

static const char * const output_456[] = {
  "doz%I2 %0,%1,%2\n\t{ai|addic} %0,%0,-1\n\t{aze.|addze.} %0,%3",
  "#",
};

static const char * const output_458[] = {
  "{sf|subfc} %0,%2,%1\n\t{sfe|subfe} %0,%0,%0\n\tneg %0,%0",
  "{ai|addic} %0,%1,%n2\n\t{sfe|subfe} %0,%0,%0\n\tneg %0,%0",
};

static const char * const output_459[] = {
  "{sf|subfc} %0,%2,%1\n\t{sfe|subfe} %0,%0,%0\n\tneg. %0,%0",
  "{ai|addic} %0,%1,%n2\n\t{sfe|subfe} %0,%0,%0\n\tneg. %0,%0",
  "#",
  "#",
};

static const char * const output_460[] = {
  "{sf|subfc} %0,%2,%1\n\t{sfe|subfe} %0,%0,%0\n\t{sf%I3|subf%I3c} %0,%0,%3",
  "{ai|addic} %0,%1,%n2\n\t{sfe|subfe} %0,%0,%0\n\t{sf%I3|subf%I3c} %0,%0,%3",
};

static const char * const output_461[] = {
  "{sf|subfc} %4,%2,%1\n\t{sfe|subfe} %4,%4,%4\n\t{sf.|subfc.} %4,%4,%3",
  "{ai|addic} %4,%1,%n2\n\t{sfe|subfe} %4,%4,%4\n\t{sf.|subfc.} %4,%4,%3",
  "#",
  "#",
};

static const char * const output_462[] = {
  "{sf|subfc} %0,%2,%1\n\t{sfe|subfe} %0,%0,%0\n\t{sf.|subfc.} %0,%0,%3",
  "{ai|addic} %0,%1,%n2\n\t{sfe|subfe} %0,%0,%0\n\t{sf.|subfc.} %0,%0,%3",
  "#",
  "#",
};

static const char * const output_463[] = {
  "{sf|subfc} %0,%2,%1\n\t{sfe|subfe} %0,%0,%0",
  "{ai|addic} %0,%1,%n2\n\t{sfe|subfe} %0,%0,%0",
};

static const char * const output_465[] = {
  "doz%I2 %3,%1,%2\n\t{sfi|subfic} %0,%3,0\n\t{ae.|adde.} %0,%0,%3",
  "#",
};

static const char * const output_467[] = {
  "doz%I2 %4,%1,%2\n\t{sfi|subfic} %4,%4,0\n\t{aze.|addze.} %4,%3",
  "#",
};

static const char * const output_468[] = {
  "doz%I2 %0,%1,%2\n\t{sfi|subfic} %0,%0,0\n\t{aze.|addze.} %0,%3",
  "#",
};

static const char * const output_470[] = {
  "{sf|subfc} %0,%2,%1\n\t{cal %0,0(0)|li %0,0}\n\t{ae|adde} %0,%0,%0",
  "{ai|addic} %0,%1,%n2\n\t{cal %0,0(0)|li %0,0}\n\t{ae|adde} %0,%0,%0",
};

static const char * const output_471[] = {
  "subfc %0,%2,%1\n\tli %0,0\n\tadde %0,%0,%0",
  "addic %0,%1,%n2\n\tli %0,0\n\tadde %0,%0,%0",
};

static const char * const output_472[] = {
  "{sf|subfc} %0,%2,%1\n\t{cal %0,0(0)|li %0,0}\n\t{ae.|adde.} %0,%0,%0",
  "{ai|addic} %0,%1,%n2\n\t{cal %0,0(0)|li %0,0}\n\t{ae.|adde.} %0,%0,%0",
  "#",
  "#",
};

static const char * const output_473[] = {
  "subfc %0,%2,%1\n\tli %0,0\n\tadde. %0,%0,%0",
  "addic %0,%1,%n2\n\tli %0,0\n\tadde. %0,%0,%0",
  "#",
  "#",
};

static const char * const output_474[] = {
  "{sf|subfc} %0,%2,%1\n\t{aze|addze} %0,%3",
  "{ai|addic} %0,%1,%n2\n\t{aze|addze} %0,%3",
};

static const char * const output_475[] = {
  "{sf|subfc} %4,%2,%1\n\t{aze.|addze.} %4,%3",
  "{ai|addic} %4,%1,%n2\n\t{aze.|addze.} %4,%3",
  "#",
  "#",
};

static const char * const output_476[] = {
  "{sf|subfc} %0,%2,%1\n\t{aze.|addze.} %0,%3",
  "{ai|addic} %0,%1,%n2\n\t{aze.|addze.} %0,%3",
  "#",
  "#",
};

static const char * const output_477[] = {
  "{sf|subfc} %0,%2,%1\n\t{sfe|subfe} %0,%0,%0\n\tnand %0,%0,%0",
  "{sfi|subfic} %0,%1,-1\n\t{a%I2|add%I2c} %0,%0,%2\n\t{sfe|subfe} %0,%0,%0",
};

static const char * const output_478[] = {
  "{sf|subfc} %0,%2,%1\n\t{sfe|subfe} %0,%0,%0\n\tandc %0,%3,%0",
  "{ai|addic} %0,%1,%n2\n\t{sfe|subfe} %0,%0,%0\n\tandc %0,%3,%0",
};

static const char * const output_479[] = {
  "{sf|subfc} %4,%2,%1\n\t{sfe|subfe} %4,%4,%4\n\tandc. %4,%3,%4",
  "{ai|addic} %4,%1,%n2\n\t{sfe|subfe} %4,%4,%4\n\tandc. %4,%3,%4",
  "#",
  "#",
};

static const char * const output_480[] = {
  "{sf|subfc} %0,%2,%1\n\t{sfe|subfe} %0,%0,%0\n\tandc. %0,%3,%0",
  "{ai|addic} %0,%1,%n2\n\t{sfe|subfe} %0,%0,%0\n\tandc. %0,%3,%0",
  "#",
  "#",
};

static const char * const output_483[] = {
  "{sfi|subfic} %0,%1,0\n\t{ame|addme} %0,%0\n\t{sri.|srwi.} %0,%0,31",
  "#",
};

static const char * const output_484[] = {
  "subfic %0,%1,0\n\taddme %0,%0\n\tsrdi. %0,%0,63",
  "#",
};

static const char * const output_486[] = {
  "doz %0,%2,%1\n\tnabs %0,%0\n\t{sri.|srwi.} %0,%0,31",
  "#",
};

static const char * const output_489[] = {
  "{a|addc} %3,%1,%1\n\t{sfe|subfe} %3,%1,%3\n\t{aze.|addze.} %3,%2",
  "#",
};

static const char * const output_490[] = {
  "addc %3,%1,%1\n\tsubfe %3,%1,%3\n\taddze. %3,%2",
  "#",
};

static const char * const output_491[] = {
  "{a|addc} %0,%1,%1\n\t{sfe|subfe} %0,%1,%0\n\t{aze.|addze.} %0,%2",
  "#",
};

static const char * const output_492[] = {
  "addc %0,%1,%1\n\tsubfe %0,%1,%0\n\taddze. %0,%2",
  "#",
};

static const char * const output_494[] = {
  "doz %4,%2,%1\n\t{ai|addic} %4,%4,-1\n\t{aze.|addze.} %4,%3",
  "#",
};

static const char * const output_495[] = {
  "doz %0,%2,%1\n\t{ai|addic} %0,%0,-1\n\t{aze.|addze.} %0,%3",
  "#",
};

static const char * const output_501[] = {
  "{sf%I2|subf%I2c} %0,%1,%2\n\t{sfe|subfe} %0,%0,%0\n\tneg. %0,%0",
  "#",
};

static const char * const output_502[] = {
  "subf%I2c %0,%1,%2\n\tsubfe %0,%0,%0\n\tneg. %0,%0",
  "#",
};

static const char * const output_503[] = {
  "{ai|addic} %0,%1,%k2\n\t{aze|addze} %0,%3",
  "{sf%I2|subf%I2c} %0,%1,%2\n\t{sfe|subfe} %0,%0,%0\n\t{sf%I3|subf%I3c} %0,%0,%3",
};

static const char * const output_504[] = {
  "addic %0,%1,%k2\n\taddze %0,%3",
  "subf%I2c %0,%1,%2\n\tsubfe %0,%0,%0\n\tsubf%I3c %0,%0,%3",
};

static const char * const output_505[] = {
  "{ai|addic} %4,%1,%k2\n\t{aze.|addze.} %4,%3",
  "{sf%I2|subf%I2c} %4,%1,%2\n\t{sfe|subfe} %4,%4,%4\n\t{sf.|subfc.} %4,%4,%3",
  "#",
  "#",
};

static const char * const output_506[] = {
  "addic %4,%1,%k2\n\taddze. %4,%3",
  "subf%I2c %4,%1,%2\n\tsubfe %4,%4,%4\n\tsubfc. %4,%4,%3",
  "#",
  "#",
};

static const char * const output_507[] = {
  "{ai|addic} %0,%1,%k2\n\t{aze.|addze.} %0,%3",
  "{sf%I2|subf%I2c} %0,%1,%2\n\t{sfe|subfe} %0,%0,%0\n\t{sf.|subfc.} %0,%0,%3",
  "#",
  "#",
};

static const char * const output_508[] = {
  "addic %0,%1,%k2\n\taddze. %0,%3",
  "subf%I2c %0,%1,%2\n\tsubfe %0,%0,%0\n\tsubfc. %0,%0,%3",
  "#",
  "#",
};

static const char *output_511 PARAMS ((rtx *, rtx));

static const char *
output_511 (operands, insn)
     rtx *operands ATTRIBUTE_UNUSED;
     rtx insn ATTRIBUTE_UNUSED;
{

{
  return output_cbranch (operands[1], "%l0", 0, insn);
}
}

static const char *output_512 PARAMS ((rtx *, rtx));

static const char *
output_512 (operands, insn)
     rtx *operands ATTRIBUTE_UNUSED;
     rtx insn ATTRIBUTE_UNUSED;
{

{
  return output_cbranch (operands[0], NULL, 0, insn);
}
}

static const char *output_513 PARAMS ((rtx *, rtx));

static const char *
output_513 (operands, insn)
     rtx *operands ATTRIBUTE_UNUSED;
     rtx insn ATTRIBUTE_UNUSED;
{

{
  return output_cbranch (operands[1], "%l0", 1, insn);
}
}

static const char *output_514 PARAMS ((rtx *, rtx));

static const char *
output_514 (operands, insn)
     rtx *operands ATTRIBUTE_UNUSED;
     rtx insn ATTRIBUTE_UNUSED;
{

{
  return output_cbranch (operands[0], NULL, 1, insn);
}
}

static const char * const output_521[] = {
  "bctr",
  "{br|blr}",
};

static const char * const output_522[] = {
  "bctr",
  "blr",
};

static const char * const output_523[] = {
  "bctr",
  "{br|blr}",
};

static const char * const output_524[] = {
  "bctr",
  "blr",
};

static const char *output_526 PARAMS ((rtx *, rtx));

static const char *
output_526 (operands, insn)
     rtx *operands ATTRIBUTE_UNUSED;
     rtx insn ATTRIBUTE_UNUSED;
{

{
  if (which_alternative != 0)
    return "#";
  else if (get_attr_length (insn) == 4)
    return "{bdn|bdnz} %l0";
  else
    return "bdz $+8\n\tb %l0";
}
}

static const char *output_527 PARAMS ((rtx *, rtx));

static const char *
output_527 (operands, insn)
     rtx *operands ATTRIBUTE_UNUSED;
     rtx insn ATTRIBUTE_UNUSED;
{

{
  if (which_alternative != 0)
    return "#";
  else if (get_attr_length (insn) == 4)
    return "bdz %l0";
  else
    return "{bdn|bdnz} $+8\n\tb %l0";
}
}

static const char *output_528 PARAMS ((rtx *, rtx));

static const char *
output_528 (operands, insn)
     rtx *operands ATTRIBUTE_UNUSED;
     rtx insn ATTRIBUTE_UNUSED;
{

{
  if (which_alternative != 0)
    return "#";
  else if (get_attr_length (insn) == 4)
    return "{bdn|bdnz} %l0";
  else
    return "bdz $+8\n\tb %l0";
}
}

static const char *output_529 PARAMS ((rtx *, rtx));

static const char *
output_529 (operands, insn)
     rtx *operands ATTRIBUTE_UNUSED;
     rtx insn ATTRIBUTE_UNUSED;
{

{
  if (which_alternative != 0)
    return "#";
  else if (get_attr_length (insn) == 4)
    return "bdz %l0";
  else
    return "{bdn|bdnz} $+8\n\tb %l0";
}
}

static const char *output_530 PARAMS ((rtx *, rtx));

static const char *
output_530 (operands, insn)
     rtx *operands ATTRIBUTE_UNUSED;
     rtx insn ATTRIBUTE_UNUSED;
{

{
  if (which_alternative != 0)
    return "#";
  else if (get_attr_length (insn) == 4)
    return "{bdn|bdnz} %l0";
  else
    return "bdz $+8\n\tb %l0";
}
}

static const char *output_531 PARAMS ((rtx *, rtx));

static const char *
output_531 (operands, insn)
     rtx *operands ATTRIBUTE_UNUSED;
     rtx insn ATTRIBUTE_UNUSED;
{

{
  if (which_alternative != 0)
    return "#";
  else if (get_attr_length (insn) == 4)
    return "bdz %l0";
  else
    return "{bdn|bdnz} $+8\n\tb %l0";
}
}

static const char *output_532 PARAMS ((rtx *, rtx));

static const char *
output_532 (operands, insn)
     rtx *operands ATTRIBUTE_UNUSED;
     rtx insn ATTRIBUTE_UNUSED;
{

{
  if (which_alternative != 0)
    return "#";
  else if (get_attr_length (insn) == 4)
    return "{bdn|bdnz} %l0";
  else
    return "bdz $+8\n\tb %l0";
}
}

static const char *output_533 PARAMS ((rtx *, rtx));

static const char *
output_533 (operands, insn)
     rtx *operands ATTRIBUTE_UNUSED;
     rtx insn ATTRIBUTE_UNUSED;
{

{
  if (which_alternative != 0)
    return "#";
  else if (get_attr_length (insn) == 4)
    return "bdz %l0";
  else
    return "{bdn|bdnz} $+8\n\tb %l0";
}
}

static const char *output_534 PARAMS ((rtx *, rtx));

static const char *
output_534 (operands, insn)
     rtx *operands ATTRIBUTE_UNUSED;
     rtx insn ATTRIBUTE_UNUSED;
{

{
  if (which_alternative != 0)
    return "#";
  else if (get_attr_length (insn) == 4)
    return "bdz %l0";
  else
    return "{bdn|bdnz} $+8\n\tb %l0";
}
}

static const char *output_535 PARAMS ((rtx *, rtx));

static const char *
output_535 (operands, insn)
     rtx *operands ATTRIBUTE_UNUSED;
     rtx insn ATTRIBUTE_UNUSED;
{

{
  if (which_alternative != 0)
    return "#";
  else if (get_attr_length (insn) == 4)
    return "{bdn|bdnz} %l0";
  else
    return "bdz $+8\n\tb %l0";
}
}

static const char *output_536 PARAMS ((rtx *, rtx));

static const char *
output_536 (operands, insn)
     rtx *operands ATTRIBUTE_UNUSED;
     rtx insn ATTRIBUTE_UNUSED;
{

{
  if (which_alternative != 0)
    return "#";
  else if (get_attr_length (insn) == 4)
    return "bdz %l0";
  else
    return "{bdn|bdnz} $+8\n\tb %l0";
}
}

static const char *output_537 PARAMS ((rtx *, rtx));

static const char *
output_537 (operands, insn)
     rtx *operands ATTRIBUTE_UNUSED;
     rtx insn ATTRIBUTE_UNUSED;
{

{
  if (which_alternative != 0)
    return "#";
  else if (get_attr_length (insn) == 4)
    return "{bdn|bdnz} %l0";
  else
    return "bdz $+8\n\tb %l0";
}
}

static const char *output_546 PARAMS ((rtx *, rtx));

static const char *
output_546 (operands, insn)
     rtx *operands ATTRIBUTE_UNUSED;
     rtx insn ATTRIBUTE_UNUSED;
{

{
  int mask = 0;
  int i;
  for (i = 0; i < XVECLEN (operands[0], 0); i++)
    mask |= INTVAL (XVECEXP (SET_SRC (XVECEXP (operands[0], 0, i)), 0, 1));
  operands[4] = GEN_INT (mask);
  return "mtcrf %4,%2";
}
}

static const char *output_555 PARAMS ((rtx *, rtx));

static const char *
output_555 (operands, insn)
     rtx *operands ATTRIBUTE_UNUSED;
     rtx insn ATTRIBUTE_UNUSED;
{

{
  if (GET_CODE (operands[0]) == REG)
    return INTVAL (operands[1]) ? "dcbtst 0,%0" : "dcbt 0,%0";
  return INTVAL (operands[1]) ? "dcbtst %a0" : "dcbt %a0";
}
}

static const char * const output_564[] = {
  "stvx %1,%y0",
  "lvx %0,%y1",
  "vor %0,%1,%1",
  "stw%U0 %1,%0\n\tstw %L1,%L0\n\tstw %Y1,%Y0\n\tstw %Z1,%Z0",
  "lwz%U1 %0,%1\n\tlwz %L0,%L1\n\tlwz %Y0,%Y1\n\tlwz %Z0,%Z1",
  "mr %0,%1\n\tmr %L0,%L1\n\tmr %Y0,%Y1\n\tmr %Z0,%Z1",
};

static const char * const output_565[] = {
  "stvx %1,%y0",
  "lvx %0,%y1",
  "vor %0,%1,%1",
  "stw%U0 %1,%0\n\tstw %L1,%L0\n\tstw %Y1,%Y0\n\tstw %Z1,%Z0",
  "lwz%U1 %0,%1\n\tlwz %L0,%L1\n\tlwz %Y0,%Y1\n\tlwz %Z0,%Z1",
  "mr %0,%1\n\tmr %L0,%L1\n\tmr %Y0,%Y1\n\tmr %Z0,%Z1",
};

static const char * const output_566[] = {
  "stvx %1,%y0",
  "lvx %0,%y1",
  "vor %0,%1,%1",
  "stw%U0 %1,%0\n\tstw %L1,%L0\n\tstw %Y1,%Y0\n\tstw %Z1,%Z0",
  "lwz%U1 %0,%1\n\tlwz %L0,%L1\n\tlwz %Y0,%Y1\n\tlwz %Z0,%Z1",
  "mr %0,%1\n\tmr %L0,%L1\n\tmr %Y0,%Y1\n\tmr %Z0,%Z1",
};

static const char * const output_567[] = {
  "stvx %1,%y0",
  "lvx %0,%y1",
  "vor %0,%1,%1",
  "stw%U0 %1,%0\n\tstw %L1,%L0\n\tstw %Y1,%Y0\n\tstw %Z1,%Z0",
  "lwz%U1 %0,%1\n\tlwz %L0,%L1\n\tlwz %Y0,%Y1\n\tlwz %Z0,%Z1",
  "mr %0,%1\n\tmr %L0,%L1\n\tmr %Y0,%Y1\n\tmr %Z0,%Z1",
};

static const char *output_568 PARAMS ((rtx *, rtx));

static const char *
output_568 (operands, insn)
     rtx *operands ATTRIBUTE_UNUSED;
     rtx insn ATTRIBUTE_UNUSED;
{

{
  if (TARGET_MACHO)
     return "mfspr %0,256";
  else
     return "mfvrsave %0";
}
}

static const char *output_569 PARAMS ((rtx *, rtx));

static const char *
output_569 (operands, insn)
     rtx *operands ATTRIBUTE_UNUSED;
     rtx insn ATTRIBUTE_UNUSED;
{

{
  if (TARGET_MACHO)
    return "mtspr 256,%1";
  else
    return "mtvrsave %1";
}
}


extern int gpc_reg_operand PARAMS ((rtx, enum machine_mode));
extern int reg_or_mem_operand PARAMS ((rtx, enum machine_mode));
extern int cc_reg_operand PARAMS ((rtx, enum machine_mode));
extern int scratch_operand PARAMS ((rtx, enum machine_mode));
extern int lwa_operand PARAMS ((rtx, enum machine_mode));
extern int add_operand PARAMS ((rtx, enum machine_mode));
extern int reg_or_short_operand PARAMS ((rtx, enum machine_mode));
extern int exact_log2_cint_operand PARAMS ((rtx, enum machine_mode));
extern int register_operand PARAMS ((rtx, enum machine_mode));
extern int and_operand PARAMS ((rtx, enum machine_mode));
extern int mask_operand_wrap PARAMS ((rtx, enum machine_mode));
extern int logical_operand PARAMS ((rtx, enum machine_mode));
extern int boolean_or_operator PARAMS ((rtx, enum machine_mode));
extern int boolean_operator PARAMS ((rtx, enum machine_mode));
extern int const_int_operand PARAMS ((rtx, enum machine_mode));
extern int reg_or_cint_operand PARAMS ((rtx, enum machine_mode));
extern int mask_operand PARAMS ((rtx, enum machine_mode));
extern int zero_fp_constant PARAMS ((rtx, enum machine_mode));
extern int memory_operand PARAMS ((rtx, enum machine_mode));
extern int mask64_operand PARAMS ((rtx, enum machine_mode));
extern int and64_2_operand PARAMS ((rtx, enum machine_mode));
extern int got_no_const_operand PARAMS ((rtx, enum machine_mode));
extern int nonimmediate_operand PARAMS ((rtx, enum machine_mode));
extern int input_operand PARAMS ((rtx, enum machine_mode));
extern int load_multiple_operation PARAMS ((rtx, enum machine_mode));
extern int store_multiple_operation PARAMS ((rtx, enum machine_mode));
extern int indirect_operand PARAMS ((rtx, enum machine_mode));
extern int immediate_operand PARAMS ((rtx, enum machine_mode));
extern int reg_or_aligned_short_operand PARAMS ((rtx, enum machine_mode));
extern int current_file_function_operand PARAMS ((rtx, enum machine_mode));
extern int symbol_ref_operand PARAMS ((rtx, enum machine_mode));
extern int reg_or_u_short_operand PARAMS ((rtx, enum machine_mode));
extern int short_cint_operand PARAMS ((rtx, enum machine_mode));
extern int u_short_cint_operand PARAMS ((rtx, enum machine_mode));
extern int scc_comparison_operator PARAMS ((rtx, enum machine_mode));
extern int reg_or_neg_short_operand PARAMS ((rtx, enum machine_mode));
extern int branch_comparison_operator PARAMS ((rtx, enum machine_mode));
extern int branch_positive_comparison_operator PARAMS ((rtx, enum machine_mode));
extern int trap_comparison_operator PARAMS ((rtx, enum machine_mode));
extern int stmw_operation PARAMS ((rtx, enum machine_mode));
extern int any_operand PARAMS ((rtx, enum machine_mode));
extern int call_operand PARAMS ((rtx, enum machine_mode));
extern int mtcrf_operation PARAMS ((rtx, enum machine_mode));
extern int lmw_operation PARAMS ((rtx, enum machine_mode));
extern int address_operand PARAMS ((rtx, enum machine_mode));
extern int altivec_register_operand PARAMS ((rtx, enum machine_mode));
extern int vrsave_operation PARAMS ((rtx, enum machine_mode));
extern int zero_constant PARAMS ((rtx, enum machine_mode));
extern int cc_reg_not_cr0_operand PARAMS ((rtx, enum machine_mode));
extern int reg_or_arith_cint_operand PARAMS ((rtx, enum machine_mode));
extern int non_add_cint_operand PARAMS ((rtx, enum machine_mode));
extern int reg_or_logical_cint_operand PARAMS ((rtx, enum machine_mode));
extern int non_logical_cint_operand PARAMS ((rtx, enum machine_mode));
extern int min_max_operator PARAMS ((rtx, enum machine_mode));
extern int comparison_operator PARAMS ((rtx, enum machine_mode));
extern int offsettable_mem_operand PARAMS ((rtx, enum machine_mode));
extern int reg_or_add_cint64_operand PARAMS ((rtx, enum machine_mode));
extern int reg_or_sub_cint64_operand PARAMS ((rtx, enum machine_mode));
extern int mask64_2_operand PARAMS ((rtx, enum machine_mode));
extern int and64_operand PARAMS ((rtx, enum machine_mode));
extern int got_operand PARAMS ((rtx, enum machine_mode));
extern int general_operand PARAMS ((rtx, enum machine_mode));
extern int const_double_operand PARAMS ((rtx, enum machine_mode));
extern int easy_fp_constant PARAMS ((rtx, enum machine_mode));
extern int non_short_cint_operand PARAMS ((rtx, enum machine_mode));



static const struct insn_operand_data operand_data[] = 
{
  {
    0,
    "",
    VOIDmode,
    0,
    0
  },
  {
    gpc_reg_operand,
    "=r,r",
    DImode,
    0,
    1
  },
  {
    reg_or_mem_operand,
    "m,r",
    QImode,
    0,
    1
  },
  {
    cc_reg_operand,
    "=x,?y",
    CCmode,
    0,
    1
  },
  {
    gpc_reg_operand,
    "r,r",
    QImode,
    0,
    1
  },
  {
    scratch_operand,
    "=r,r",
    DImode,
    0,
    0
  },
  {
    gpc_reg_operand,
    "=r,r",
    DImode,
    0,
    1
  },
  {
    gpc_reg_operand,
    "r,r",
    QImode,
    0,
    1
  },
  {
    cc_reg_operand,
    "=x,?y",
    CCmode,
    0,
    1
  },
  {
    gpc_reg_operand,
    "=r",
    DImode,
    0,
    1
  },
  {
    gpc_reg_operand,
    "r",
    QImode,
    0,
    1
  },
  {
    gpc_reg_operand,
    "=r,r",
    DImode,
    0,
    1
  },
  {
    reg_or_mem_operand,
    "m,r",
    HImode,
    0,
    1
  },
  {
    cc_reg_operand,
    "=x,?y",
    CCmode,
    0,
    1
  },
  {
    gpc_reg_operand,
    "r,r",
    HImode,
    0,
    1
  },
  {
    scratch_operand,
    "=r,r",
    DImode,
    0,
    0
  },
  {
    gpc_reg_operand,
    "=r,r",
    DImode,
    0,
    1
  },
  {
    gpc_reg_operand,
    "r,r",
    HImode,
    0,
    1
  },
  {
    cc_reg_operand,
    "=x,?y",
    CCmode,
    0,
    1
  },
  {
    gpc_reg_operand,
    "=r,r",
    DImode,
    0,
    1
  },
  {
    reg_or_mem_operand,
    "m,r",
    SImode,
    0,
    1
  },
  {
    cc_reg_operand,
    "=x,?y",
    CCmode,
    0,
    1
  },
  {
    gpc_reg_operand,
    "r,r",
    SImode,
    0,
    1
  },
  {
    scratch_operand,
    "=r,r",
    DImode,
    0,
    0
  },
  {
    gpc_reg_operand,
    "=r,r",
    DImode,
    0,
    1
  },
  {
    gpc_reg_operand,
    "r,r",
    SImode,
    0,
    1
  },
  {
    cc_reg_operand,
    "=x,?y",
    CCmode,
    0,
    1
  },
  {
    gpc_reg_operand,
    "=r,r",
    DImode,
    0,
    1
  },
  {
    lwa_operand,
    "m,r",
    SImode,
    0,
    1
  },
  {
    gpc_reg_operand,
    "=r,r",
    SImode,
    0,
    1
  },
  {
    reg_or_mem_operand,
    "m,r",
    QImode,
    0,
    1
  },
  {
    cc_reg_operand,
    "=x,?y",
    CCmode,
    0,
    1
  },
  {
    gpc_reg_operand,
    "r,r",
    QImode,
    0,
    1
  },
  {
    scratch_operand,
    "=r,r",
    SImode,
    0,
    0
  },
  {
    gpc_reg_operand,
    "=r,r",
    SImode,
    0,
    1
  },
  {
    gpc_reg_operand,
    "r,r",
    QImode,
    0,
    1
  },
  {
    cc_reg_operand,
    "=x,?y",
    CCmode,
    0,
    1
  },
  {
    gpc_reg_operand,
    "=r",
    SImode,
    0,
    1
  },
  {
    gpc_reg_operand,
    "r",
    QImode,
    0,
    1
  },
  {
    gpc_reg_operand,
    "=r,r",
    HImode,
    0,
    1
  },
  {
    reg_or_mem_operand,
    "m,r",
    QImode,
    0,
    1
  },
  {
    cc_reg_operand,
    "=x,?y",
    CCmode,
    0,
    1
  },
  {
    gpc_reg_operand,
    "r,r",
    QImode,
    0,
    1
  },
  {
    scratch_operand,
    "=r,r",
    HImode,
    0,
    0
  },
  {
    gpc_reg_operand,
    "=r,r",
    HImode,
    0,
    1
  },
  {
    gpc_reg_operand,
    "r,r",
    QImode,
    0,
    1
  },
  {
    cc_reg_operand,
    "=x,?y",
    CCmode,
    0,
    1
  },
  {
    gpc_reg_operand,
    "=r",
    HImode,
    0,
    1
  },
  {
    gpc_reg_operand,
    "r",
    QImode,
    0,
    1
  },
  {
    gpc_reg_operand,
    "=r,r",
    SImode,
    0,
    1
  },
  {
    reg_or_mem_operand,
    "m,r",
    HImode,
    0,
    1
  },
  {
    cc_reg_operand,
    "=x,?y",
    CCmode,
    0,
    1
  },
  {
    gpc_reg_operand,
    "r,r",
    HImode,
    0,
    1
  },
  {
    scratch_operand,
    "=r,r",
    SImode,
    0,
    0
  },
  {
    gpc_reg_operand,
    "=r,r",
    SImode,
    0,
    1
  },
  {
    gpc_reg_operand,
    "r,r",
    HImode,
    0,
    1
  },
  {
    cc_reg_operand,
    "=x,?y",
    CCmode,
    0,
    1
  },
  {
    gpc_reg_operand,
    "=r,r,?r,r",
    SImode,
    0,
    1
  },
  {
    gpc_reg_operand,
    "%r,b,r,b",
    SImode,
    0,
    1
  },
  {
    add_operand,
    "r,I,I,L",
    SImode,
    0,
    1
  },
  {
    cc_reg_operand,
    "=x,x,?y,?y",
    CCmode,
    0,
    1
  },
  {
    gpc_reg_operand,
    "%r,r,r,r",
    SImode,
    0,
    1
  },
  {
    reg_or_short_operand,
    "r,I,r,I",
    SImode,
    0,
    1
  },
  {
    scratch_operand,
    "=r,r,r,r",
    SImode,
    0,
    0
  },
  {
    gpc_reg_operand,
    "=r,r,r,r",
    SImode,
    0,
    1
  },
  {
    gpc_reg_operand,
    "%r,r,r,r",
    SImode,
    0,
    1
  },
  {
    reg_or_short_operand,
    "r,I,r,I",
    SImode,
    0,
    1
  },
  {
    cc_reg_operand,
    "=x,x,?y,?y",
    CCmode,
    0,
    1
  },
  {
    gpc_reg_operand,
    "=r",
    SImode,
    0,
    1
  },
  {
    gpc_reg_operand,
    "r",
    SImode,
    0,
    1
  },
  {
    cc_reg_operand,
    "=x,?y",
    CCmode,
    0,
    1
  },
  {
    gpc_reg_operand,
    "r,r",
    SImode,
    0,
    1
  },
  {
    scratch_operand,
    "=r,r",
    SImode,
    0,
    0
  },
  {
    gpc_reg_operand,
    "=r,r",
    SImode,
    0,
    1
  },
  {
    gpc_reg_operand,
    "r,r",
    SImode,
    0,
    1
  },
  {
    cc_reg_operand,
    "=x,?y",
    CCmode,
    0,
    1
  },
  {
    gpc_reg_operand,
    "=r",
    SImode,
    0,
    1
  },
  {
    reg_or_short_operand,
    "rI",
    SImode,
    0,
    1
  },
  {
    gpc_reg_operand,
    "r",
    SImode,
    0,
    1
  },
  {
    gpc_reg_operand,
    "=r,r",
    SImode,
    0,
    1
  },
  {
    reg_or_short_operand,
    "r,I",
    SImode,
    0,
    1
  },
  {
    gpc_reg_operand,
    "r,r",
    SImode,
    0,
    1
  },
  {
    cc_reg_operand,
    "=x,?y",
    CCmode,
    0,
    1
  },
  {
    gpc_reg_operand,
    "r,r",
    SImode,
    0,
    1
  },
  {
    gpc_reg_operand,
    "r,r",
    SImode,
    0,
    1
  },
  {
    scratch_operand,
    "=r,r",
    SImode,
    0,
    0
  },
  {
    gpc_reg_operand,
    "=r,r",
    SImode,
    0,
    1
  },
  {
    gpc_reg_operand,
    "r,r",
    SImode,
    0,
    1
  },
  {
    gpc_reg_operand,
    "r,r",
    SImode,
    0,
    1
  },
  {
    cc_reg_operand,
    "=x,?y",
    CCmode,
    0,
    1
  },
  {
    gpc_reg_operand,
    "=r",
    SImode,
    0,
    1
  },
  {
    gpc_reg_operand,
    "r",
    SImode,
    0,
    1
  },
  {
    reg_or_short_operand,
    "rI",
    SImode,
    0,
    1
  },
  {
    cc_reg_operand,
    "=x,?y",
    CCmode,
    0,
    1
  },
  {
    gpc_reg_operand,
    "r,r",
    SImode,
    0,
    1
  },
  {
    reg_or_short_operand,
    "rI,rI",
    SImode,
    0,
    1
  },
  {
    scratch_operand,
    "=r,r",
    SImode,
    0,
    0
  },
  {
    gpc_reg_operand,
    "=r,r",
    SImode,
    0,
    1
  },
  {
    gpc_reg_operand,
    "r,r",
    SImode,
    0,
    1
  },
  {
    reg_or_short_operand,
    "rI,rI",
    SImode,
    0,
    1
  },
  {
    cc_reg_operand,
    "=x,?y",
    CCmode,
    0,
    1
  },
  {
    gpc_reg_operand,
    "=&r,r",
    SImode,
    0,
    1
  },
  {
    gpc_reg_operand,
    "r,0",
    SImode,
    0,
    1
  },
  {
    scratch_operand,
    "=&r,&r",
    SImode,
    0,
    0
  },
  {
    gpc_reg_operand,
    "=&r",
    SImode,
    0,
    1
  },
  {
    gpc_reg_operand,
    "r",
    SImode,
    0,
    1
  },
  {
    gpc_reg_operand,
    "=r,r",
    SImode,
    0,
    1
  },
  {
    gpc_reg_operand,
    "%r,r",
    SImode,
    0,
    1
  },
  {
    reg_or_short_operand,
    "r,I",
    SImode,
    0,
    1
  },
  {
    scratch_operand,
    "=q,q",
    SImode,
    0,
    0
  },
  {
    cc_reg_operand,
    "=x,?y",
    CCmode,
    0,
    1
  },
  {
    gpc_reg_operand,
    "%r,r",
    SImode,
    0,
    1
  },
  {
    gpc_reg_operand,
    "r,r",
    SImode,
    0,
    1
  },
  {
    scratch_operand,
    "=r,r",
    SImode,
    0,
    0
  },
  {
    scratch_operand,
    "=q,q",
    SImode,
    0,
    0
  },
  {
    gpc_reg_operand,
    "=r,r",
    SImode,
    0,
    1
  },
  {
    gpc_reg_operand,
    "%r,r",
    SImode,
    0,
    1
  },
  {
    gpc_reg_operand,
    "r,r",
    SImode,
    0,
    1
  },
  {
    cc_reg_operand,
    "=x,?y",
    CCmode,
    0,
    1
  },
  {
    scratch_operand,
    "=q,q",
    SImode,
    0,
    0
  },
  {
    gpc_reg_operand,
    "=r",
    SImode,
    0,
    1
  },
  {
    gpc_reg_operand,
    "r",
    SImode,
    0,
    1
  },
  {
    gpc_reg_operand,
    "r",
    SImode,
    0,
    1
  },
  {
    gpc_reg_operand,
    "=q",
    SImode,
    0,
    1
  },
  {
    gpc_reg_operand,
    "=r",
    SImode,
    0,
    1
  },
  {
    gpc_reg_operand,
    "r",
    SImode,
    0,
    1
  },
  {
    gpc_reg_operand,
    "r",
    SImode,
    0,
    1
  },
  {
    scratch_operand,
    "=q",
    SImode,
    0,
    0
  },
  {
    gpc_reg_operand,
    "=r",
    SImode,
    0,
    1
  },
  {
    gpc_reg_operand,
    "r",
    SImode,
    0,
    1
  },
  {
    exact_log2_cint_operand,
    "N",
    SImode,
    0,
    1
  },
  {
    cc_reg_operand,
    "=x,?y",
    CCmode,
    0,
    1
  },
  {
    gpc_reg_operand,
    "r,r",
    SImode,
    0,
    1
  },
  {
    exact_log2_cint_operand,
    "N,N",
    SImode,
    0,
    1
  },
  {
    scratch_operand,
    "=r,r",
    SImode,
    0,
    0
  },
  {
    gpc_reg_operand,
    "=r,r",
    SImode,
    0,
    1
  },
  {
    gpc_reg_operand,
    "r,r",
    SImode,
    0,
    1
  },
  {
    exact_log2_cint_operand,
    "N,N",
    SImode,
    0,
    1
  },
  {
    cc_reg_operand,
    "=x,?y",
    CCmode,
    0,
    1
  },
  {
    gpc_reg_operand,
    "=r",
    SImode,
    0,
    1
  },
  {
    gpc_reg_operand,
    "r",
    SImode,
    0,
    1
  },
  {
    register_operand,
    "=*q",
    SImode,
    0,
    1
  },
  {
    gpc_reg_operand,
    "r",
    SImode,
    0,
    1
  },
  {
    register_operand,
    "2",
    SImode,
    0,
    1
  },
  {
    scratch_operand,
    "=l",
    SImode,
    0,
    0
  },
  {
    scratch_operand,
    "=x",
    CCmode,
    0,
    0
  },
  {
    gpc_reg_operand,
    "=r,r,r,r",
    SImode,
    0,
    1
  },
  {
    gpc_reg_operand,
    "%r,r,r,r",
    SImode,
    0,
    1
  },
  {
    and_operand,
    "?r,T,K,L",
    SImode,
    0,
    1
  },
  {
    scratch_operand,
    "=X,X,x,x",
    CCmode,
    0,
    0
  },
  {
    cc_reg_operand,
    "=x,x,x,x,?y,??y,??y,?y",
    CCmode,
    0,
    1
  },
  {
    gpc_reg_operand,
    "%r,r,r,r,r,r,r,r",
    SImode,
    0,
    1
  },
  {
    and_operand,
    "r,K,L,T,r,K,L,T",
    SImode,
    0,
    1
  },
  {
    scratch_operand,
    "=r,r,r,r,r,r,r,r",
    SImode,
    0,
    0
  },
  {
    scratch_operand,
    "=X,X,X,X,X,x,x,X",
    CCmode,
    0,
    0
  },
  {
    gpc_reg_operand,
    "=r,r,r,r,r,r,r,r",
    SImode,
    0,
    1
  },
  {
    gpc_reg_operand,
    "%r,r,r,r,r,r,r,r",
    SImode,
    0,
    1
  },
  {
    and_operand,
    "r,K,L,T,r,K,L,T",
    SImode,
    0,
    1
  },
  {
    cc_reg_operand,
    "=x,x,x,x,?y,??y,??y,?y",
    CCmode,
    0,
    1
  },
  {
    scratch_operand,
    "=X,X,X,X,X,x,x,X",
    CCmode,
    0,
    0
  },
  {
    gpc_reg_operand,
    "=r",
    SImode,
    0,
    1
  },
  {
    gpc_reg_operand,
    "r",
    SImode,
    0,
    1
  },
  {
    mask_operand_wrap,
    "i",
    SImode,
    0,
    1
  },
  {
    gpc_reg_operand,
    "r,r",
    SImode,
    0,
    1
  },
  {
    mask_operand_wrap,
    "i,i",
    SImode,
    0,
    1
  },
  {
    cc_reg_operand,
    "=x,?y",
    CCmode,
    0,
    1
  },
  {
    scratch_operand,
    "=r,r",
    SImode,
    0,
    0
  },
  {
    gpc_reg_operand,
    "=r,r",
    SImode,
    0,
    1
  },
  {
    gpc_reg_operand,
    "r,r",
    SImode,
    0,
    1
  },
  {
    mask_operand_wrap,
    "i,i",
    SImode,
    0,
    1
  },
  {
    cc_reg_operand,
    "=x,??y",
    CCmode,
    0,
    1
  },
  {
    gpc_reg_operand,
    "=r,r,r",
    SImode,
    0,
    1
  },
  {
    gpc_reg_operand,
    "%r,r,r",
    SImode,
    0,
    1
  },
  {
    logical_operand,
    "r,K,L",
    SImode,
    0,
    1
  },
  {
    boolean_or_operator,
    "",
    SImode,
    0,
    0
  },
  {
    cc_reg_operand,
    "=x,?y",
    CCmode,
    0,
    1
  },
  {
    gpc_reg_operand,
    "%r,r",
    SImode,
    0,
    1
  },
  {
    gpc_reg_operand,
    "r,r",
    SImode,
    0,
    1
  },
  {
    scratch_operand,
    "=r,r",
    SImode,
    0,
    0
  },
  {
    boolean_or_operator,
    "",
    SImode,
    0,
    0
  },
  {
    gpc_reg_operand,
    "=r,r",
    SImode,
    0,
    1
  },
  {
    gpc_reg_operand,
    "%r,r",
    SImode,
    0,
    1
  },
  {
    gpc_reg_operand,
    "r,r",
    SImode,
    0,
    1
  },
  {
    cc_reg_operand,
    "=x,?y",
    CCmode,
    0,
    1
  },
  {
    boolean_operator,
    "",
    SImode,
    0,
    0
  },
  {
    gpc_reg_operand,
    "=r",
    SImode,
    0,
    1
  },
  {
    gpc_reg_operand,
    "r",
    SImode,
    0,
    1
  },
  {
    gpc_reg_operand,
    "r",
    SImode,
    0,
    1
  },
  {
    boolean_operator,
    "",
    SImode,
    0,
    0
  },
  {
    cc_reg_operand,
    "=x,?y",
    CCmode,
    0,
    1
  },
  {
    gpc_reg_operand,
    "r,r",
    SImode,
    0,
    1
  },
  {
    gpc_reg_operand,
    "r,r",
    SImode,
    0,
    1
  },
  {
    scratch_operand,
    "=r,r",
    SImode,
    0,
    0
  },
  {
    boolean_operator,
    "",
    SImode,
    0,
    0
  },
  {
    gpc_reg_operand,
    "=r",
    SImode,
    0,
    1
  },
  {
    gpc_reg_operand,
    "0",
    SImode,
    0,
    1
  },
  {
    gpc_reg_operand,
    "r",
    SImode,
    0,
    1
  },
  {
    gpc_reg_operand,
    "r",
    SImode,
    0,
    1
  },
  {
    gpc_reg_operand,
    "=r,r",
    SImode,
    0,
    1
  },
  {
    gpc_reg_operand,
    "0,0",
    SImode,
    0,
    1
  },
  {
    gpc_reg_operand,
    "r,r",
    SImode,
    0,
    1
  },
  {
    gpc_reg_operand,
    "r,r",
    SImode,
    0,
    1
  },
  {
    cc_reg_operand,
    "=x,?y",
    CCmode,
    0,
    1
  },
  {
    gpc_reg_operand,
    "+r",
    SImode,
    0,
    1
  },
  {
    const_int_operand,
    "i",
    SImode,
    0,
    1
  },
  {
    const_int_operand,
    "i",
    SImode,
    0,
    1
  },
  {
    gpc_reg_operand,
    "r",
    SImode,
    0,
    1
  },
  {
    const_int_operand,
    "i",
    SImode,
    0,
    1
  },
  {
    const_int_operand,
    "i",
    SImode,
    0,
    1
  },
  {
    gpc_reg_operand,
    "+r",
    DImode,
    0,
    1
  },
  {
    const_int_operand,
    "i",
    SImode,
    0,
    1
  },
  {
    const_int_operand,
    "i",
    SImode,
    0,
    1
  },
  {
    gpc_reg_operand,
    "r",
    DImode,
    0,
    1
  },
  {
    gpc_reg_operand,
    "=r",
    SImode,
    0,
    1
  },
  {
    gpc_reg_operand,
    "r",
    SImode,
    0,
    1
  },
  {
    const_int_operand,
    "i",
    SImode,
    0,
    1
  },
  {
    const_int_operand,
    "i",
    SImode,
    0,
    1
  },
  {
    cc_reg_operand,
    "=x,?y",
    CCmode,
    0,
    1
  },
  {
    gpc_reg_operand,
    "r,r",
    SImode,
    0,
    1
  },
  {
    const_int_operand,
    "i,i",
    SImode,
    0,
    1
  },
  {
    const_int_operand,
    "i,i",
    SImode,
    0,
    1
  },
  {
    scratch_operand,
    "=r,r",
    SImode,
    0,
    0
  },
  {
    gpc_reg_operand,
    "=r,r",
    SImode,
    0,
    1
  },
  {
    gpc_reg_operand,
    "r,r",
    SImode,
    0,
    1
  },
  {
    const_int_operand,
    "i,i",
    SImode,
    0,
    1
  },
  {
    const_int_operand,
    "i,i",
    SImode,
    0,
    1
  },
  {
    cc_reg_operand,
    "=x,?y",
    CCmode,
    0,
    1
  },
  {
    gpc_reg_operand,
    "=r",
    DImode,
    0,
    1
  },
  {
    gpc_reg_operand,
    "r",
    DImode,
    0,
    1
  },
  {
    const_int_operand,
    "i",
    SImode,
    0,
    1
  },
  {
    const_int_operand,
    "i",
    SImode,
    0,
    1
  },
  {
    gpc_reg_operand,
    "=x",
    CCmode,
    0,
    1
  },
  {
    gpc_reg_operand,
    "r",
    DImode,
    0,
    1
  },
  {
    const_int_operand,
    "i",
    SImode,
    0,
    1
  },
  {
    const_int_operand,
    "i",
    SImode,
    0,
    1
  },
  {
    scratch_operand,
    "=r",
    DImode,
    0,
    0
  },
  {
    gpc_reg_operand,
    "=r",
    SImode,
    0,
    1
  },
  {
    gpc_reg_operand,
    "r",
    SImode,
    0,
    1
  },
  {
    reg_or_cint_operand,
    "ri",
    SImode,
    0,
    1
  },
  {
    cc_reg_operand,
    "=x,?y",
    CCmode,
    0,
    1
  },
  {
    gpc_reg_operand,
    "r,r",
    SImode,
    0,
    1
  },
  {
    reg_or_cint_operand,
    "ri,ri",
    SImode,
    0,
    1
  },
  {
    scratch_operand,
    "=r,r",
    SImode,
    0,
    0
  },
  {
    gpc_reg_operand,
    "=r,r",
    SImode,
    0,
    1
  },
  {
    gpc_reg_operand,
    "r,r",
    SImode,
    0,
    1
  },
  {
    reg_or_cint_operand,
    "ri,ri",
    SImode,
    0,
    1
  },
  {
    cc_reg_operand,
    "=x,?y",
    CCmode,
    0,
    1
  },
  {
    gpc_reg_operand,
    "=r",
    SImode,
    0,
    1
  },
  {
    gpc_reg_operand,
    "r",
    SImode,
    0,
    1
  },
  {
    reg_or_cint_operand,
    "ri",
    SImode,
    0,
    1
  },
  {
    mask_operand,
    "n",
    SImode,
    0,
    1
  },
  {
    cc_reg_operand,
    "=x,?y",
    CCmode,
    0,
    1
  },
  {
    gpc_reg_operand,
    "r,r",
    SImode,
    0,
    1
  },
  {
    reg_or_cint_operand,
    "ri,ri",
    SImode,
    0,
    1
  },
  {
    mask_operand,
    "n,n",
    SImode,
    0,
    1
  },
  {
    scratch_operand,
    "=r,r",
    SImode,
    0,
    0
  },
  {
    gpc_reg_operand,
    "=r,r",
    SImode,
    0,
    1
  },
  {
    gpc_reg_operand,
    "r,r",
    SImode,
    0,
    1
  },
  {
    reg_or_cint_operand,
    "ri,ri",
    SImode,
    0,
    1
  },
  {
    mask_operand,
    "n,n",
    SImode,
    0,
    1
  },
  {
    cc_reg_operand,
    "=x,?y",
    CCmode,
    0,
    1
  },
  {
    gpc_reg_operand,
    "=r,r",
    SImode,
    0,
    1
  },
  {
    gpc_reg_operand,
    "r,r",
    SImode,
    0,
    1
  },
  {
    reg_or_cint_operand,
    "r,i",
    SImode,
    0,
    1
  },
  {
    scratch_operand,
    "=q,X",
    SImode,
    0,
    0
  },
  {
    cc_reg_operand,
    "=x,x,?y,?y",
    CCmode,
    0,
    1
  },
  {
    gpc_reg_operand,
    "r,r,r,r",
    SImode,
    0,
    1
  },
  {
    reg_or_cint_operand,
    "r,i,r,i",
    SImode,
    0,
    1
  },
  {
    scratch_operand,
    "=r,r,r,r",
    SImode,
    0,
    0
  },
  {
    scratch_operand,
    "=q,X,q,X",
    SImode,
    0,
    0
  },
  {
    gpc_reg_operand,
    "=r,r,r,r",
    SImode,
    0,
    1
  },
  {
    gpc_reg_operand,
    "r,r,r,r",
    SImode,
    0,
    1
  },
  {
    reg_or_cint_operand,
    "r,i,r,i",
    SImode,
    0,
    1
  },
  {
    cc_reg_operand,
    "=x,x,?y,?y",
    CCmode,
    0,
    1
  },
  {
    scratch_operand,
    "=q,X,q,X",
    SImode,
    0,
    0
  },
  {
    gpc_reg_operand,
    "=r",
    SImode,
    0,
    1
  },
  {
    gpc_reg_operand,
    "r",
    SImode,
    0,
    1
  },
  {
    const_int_operand,
    "i",
    SImode,
    0,
    1
  },
  {
    mask_operand,
    "n",
    SImode,
    0,
    1
  },
  {
    cc_reg_operand,
    "=x,?y",
    CCmode,
    0,
    1
  },
  {
    gpc_reg_operand,
    "r,r",
    SImode,
    0,
    1
  },
  {
    const_int_operand,
    "i,i",
    SImode,
    0,
    1
  },
  {
    mask_operand,
    "n,n",
    SImode,
    0,
    1
  },
  {
    scratch_operand,
    "=r,r",
    SImode,
    0,
    0
  },
  {
    gpc_reg_operand,
    "=r,r",
    SImode,
    0,
    1
  },
  {
    gpc_reg_operand,
    "r,r",
    SImode,
    0,
    1
  },
  {
    const_int_operand,
    "i,i",
    SImode,
    0,
    1
  },
  {
    mask_operand,
    "n,n",
    SImode,
    0,
    1
  },
  {
    cc_reg_operand,
    "=x,?y",
    CCmode,
    0,
    1
  },
  {
    gpc_reg_operand,
    "=r,r,r",
    SImode,
    0,
    1
  },
  {
    gpc_reg_operand,
    "r,r,r",
    SImode,
    0,
    1
  },
  {
    reg_or_cint_operand,
    "r,O,i",
    SImode,
    0,
    1
  },
  {
    scratch_operand,
    "=q,X,X",
    SImode,
    0,
    0
  },
  {
    gpc_reg_operand,
    "=r,r",
    SImode,
    0,
    1
  },
  {
    gpc_reg_operand,
    "r,r",
    SImode,
    0,
    1
  },
  {
    reg_or_cint_operand,
    "O,ri",
    SImode,
    0,
    1
  },
  {
    cc_reg_operand,
    "=x,x,x,?y,?y,?y",
    CCmode,
    0,
    1
  },
  {
    gpc_reg_operand,
    "r,r,r,r,r,r",
    SImode,
    0,
    1
  },
  {
    reg_or_cint_operand,
    "r,O,i,r,O,i",
    SImode,
    0,
    1
  },
  {
    scratch_operand,
    "=r,X,r,r,X,r",
    SImode,
    0,
    0
  },
  {
    scratch_operand,
    "=q,X,X,q,X,X",
    SImode,
    0,
    0
  },
  {
    cc_reg_operand,
    "=x,x,?y,?y",
    CCmode,
    0,
    1
  },
  {
    gpc_reg_operand,
    "r,r,r,r",
    SImode,
    0,
    1
  },
  {
    reg_or_cint_operand,
    "O,ri,O,ri",
    SImode,
    0,
    1
  },
  {
    scratch_operand,
    "=X,r,X,r",
    SImode,
    0,
    0
  },
  {
    gpc_reg_operand,
    "=r,r,r,r,r,r",
    SImode,
    0,
    1
  },
  {
    gpc_reg_operand,
    "r,r,r,r,r,r",
    SImode,
    0,
    1
  },
  {
    reg_or_cint_operand,
    "r,O,i,r,O,i",
    SImode,
    0,
    1
  },
  {
    cc_reg_operand,
    "=x,x,x,?y,?y,?y",
    CCmode,
    0,
    1
  },
  {
    scratch_operand,
    "=q,X,X,q,X,X",
    SImode,
    0,
    0
  },
  {
    gpc_reg_operand,
    "=r,r,r,r",
    SImode,
    0,
    1
  },
  {
    gpc_reg_operand,
    "r,r,r,r",
    SImode,
    0,
    1
  },
  {
    reg_or_cint_operand,
    "O,ri,O,ri",
    SImode,
    0,
    1
  },
  {
    cc_reg_operand,
    "=x,x,?y,?y",
    CCmode,
    0,
    1
  },
  {
    cc_reg_operand,
    "=x,?y",
    CCmode,
    0,
    1
  },
  {
    gpc_reg_operand,
    "r,r",
    SImode,
    0,
    1
  },
  {
    const_int_operand,
    "i,i",
    SImode,
    0,
    1
  },
  {
    scratch_operand,
    "=r,r",
    SImode,
    0,
    0
  },
  {
    gpc_reg_operand,
    "=r,r",
    SImode,
    0,
    1
  },
  {
    gpc_reg_operand,
    "r,r",
    SImode,
    0,
    1
  },
  {
    const_int_operand,
    "i,i",
    SImode,
    0,
    1
  },
  {
    cc_reg_operand,
    "=x,?y",
    CCmode,
    0,
    1
  },
  {
    gpc_reg_operand,
    "+r",
    SImode,
    0,
    1
  },
  {
    gpc_reg_operand,
    "r",
    SImode,
    0,
    1
  },
  {
    gpc_reg_operand,
    "r",
    SImode,
    0,
    1
  },
  {
    gpc_reg_operand,
    "=f",
    DFmode,
    0,
    1
  },
  {
    gpc_reg_operand,
    "f",
    SFmode,
    0,
    1
  },
  {
    gpc_reg_operand,
    "=f",
    SFmode,
    0,
    1
  },
  {
    gpc_reg_operand,
    "f",
    DFmode,
    0,
    1
  },
  {
    gpc_reg_operand,
    "=f",
    SFmode,
    0,
    1
  },
  {
    gpc_reg_operand,
    "f",
    SFmode,
    0,
    1
  },
  {
    gpc_reg_operand,
    "=f",
    SFmode,
    0,
    1
  },
  {
    gpc_reg_operand,
    "%f",
    SFmode,
    0,
    1
  },
  {
    gpc_reg_operand,
    "f",
    SFmode,
    0,
    1
  },
  {
    gpc_reg_operand,
    "=f",
    SFmode,
    0,
    1
  },
  {
    gpc_reg_operand,
    "f",
    SFmode,
    0,
    1
  },
  {
    gpc_reg_operand,
    "f",
    SFmode,
    0,
    1
  },
  {
    gpc_reg_operand,
    "=f",
    SFmode,
    0,
    1
  },
  {
    gpc_reg_operand,
    "%f",
    SFmode,
    0,
    1
  },
  {
    gpc_reg_operand,
    "f",
    SFmode,
    0,
    1
  },
  {
    gpc_reg_operand,
    "f",
    SFmode,
    0,
    1
  },
  {
    gpc_reg_operand,
    "=f",
    SFmode,
    0,
    1
  },
  {
    gpc_reg_operand,
    "f",
    SFmode,
    0,
    1
  },
  {
    gpc_reg_operand,
    "f",
    SFmode,
    0,
    1
  },
  {
    gpc_reg_operand,
    "f",
    SFmode,
    0,
    1
  },
  {
    zero_fp_constant,
    "F",
    SFmode,
    0,
    1
  },
  {
    gpc_reg_operand,
    "=f",
    SFmode,
    0,
    1
  },
  {
    gpc_reg_operand,
    "f",
    DFmode,
    0,
    1
  },
  {
    gpc_reg_operand,
    "f",
    SFmode,
    0,
    1
  },
  {
    gpc_reg_operand,
    "f",
    SFmode,
    0,
    1
  },
  {
    zero_fp_constant,
    "F",
    DFmode,
    0,
    1
  },
  {
    gpc_reg_operand,
    "=f",
    DFmode,
    0,
    1
  },
  {
    gpc_reg_operand,
    "f",
    DFmode,
    0,
    1
  },
  {
    gpc_reg_operand,
    "=f",
    DFmode,
    0,
    1
  },
  {
    gpc_reg_operand,
    "%f",
    DFmode,
    0,
    1
  },
  {
    gpc_reg_operand,
    "f",
    DFmode,
    0,
    1
  },
  {
    gpc_reg_operand,
    "=f",
    DFmode,
    0,
    1
  },
  {
    gpc_reg_operand,
    "f",
    DFmode,
    0,
    1
  },
  {
    gpc_reg_operand,
    "f",
    DFmode,
    0,
    1
  },
  {
    gpc_reg_operand,
    "=f",
    DFmode,
    0,
    1
  },
  {
    gpc_reg_operand,
    "%f",
    DFmode,
    0,
    1
  },
  {
    gpc_reg_operand,
    "f",
    DFmode,
    0,
    1
  },
  {
    gpc_reg_operand,
    "f",
    DFmode,
    0,
    1
  },
  {
    gpc_reg_operand,
    "=f",
    DFmode,
    0,
    1
  },
  {
    gpc_reg_operand,
    "f",
    DFmode,
    0,
    1
  },
  {
    gpc_reg_operand,
    "f",
    DFmode,
    0,
    1
  },
  {
    gpc_reg_operand,
    "f",
    DFmode,
    0,
    1
  },
  {
    zero_fp_constant,
    "F",
    DFmode,
    0,
    1
  },
  {
    gpc_reg_operand,
    "=f",
    DFmode,
    0,
    1
  },
  {
    gpc_reg_operand,
    "f",
    SFmode,
    0,
    1
  },
  {
    gpc_reg_operand,
    "f",
    DFmode,
    0,
    1
  },
  {
    gpc_reg_operand,
    "f",
    DFmode,
    0,
    1
  },
  {
    zero_fp_constant,
    "F",
    SFmode,
    0,
    1
  },
  {
    gpc_reg_operand,
    "=&f",
    DFmode,
    0,
    1
  },
  {
    gpc_reg_operand,
    "r",
    SImode,
    0,
    1
  },
  {
    gpc_reg_operand,
    "r",
    SImode,
    0,
    1
  },
  {
    gpc_reg_operand,
    "f",
    DFmode,
    0,
    1
  },
  {
    memory_operand,
    "=o",
    DFmode,
    0,
    1
  },
  {
    gpc_reg_operand,
    "=f",
    DFmode,
    0,
    1
  },
  {
    gpc_reg_operand,
    "=r",
    SImode,
    0,
    1
  },
  {
    gpc_reg_operand,
    "f",
    DFmode,
    0,
    1
  },
  {
    gpc_reg_operand,
    "=f",
    DImode,
    0,
    1
  },
  {
    memory_operand,
    "=o",
    DImode,
    0,
    1
  },
  {
    gpc_reg_operand,
    "=*f",
    DImode,
    0,
    1
  },
  {
    gpc_reg_operand,
    "f",
    DFmode,
    0,
    1
  },
  {
    gpc_reg_operand,
    "=f",
    DFmode,
    0,
    1
  },
  {
    gpc_reg_operand,
    "*f",
    DImode,
    0,
    1
  },
  {
    gpc_reg_operand,
    "=f",
    DFmode,
    0,
    1
  },
  {
    gpc_reg_operand,
    "r",
    SImode,
    0,
    1
  },
  {
    memory_operand,
    "=o",
    DImode,
    0,
    1
  },
  {
    gpc_reg_operand,
    "=r",
    DImode,
    0,
    1
  },
  {
    gpc_reg_operand,
    "=f",
    DImode,
    0,
    1
  },
  {
    gpc_reg_operand,
    "=f",
    SFmode,
    0,
    1
  },
  {
    gpc_reg_operand,
    "*f",
    DImode,
    0,
    1
  },
  {
    scratch_operand,
    "=f",
    DFmode,
    0,
    0
  },
  {
    gpc_reg_operand,
    "=&r,&r,r,r",
    DImode,
    0,
    1
  },
  {
    gpc_reg_operand,
    "%r,r,0,0",
    DImode,
    0,
    1
  },
  {
    reg_or_short_operand,
    "r,I,r,I",
    DImode,
    0,
    1
  },
  {
    gpc_reg_operand,
    "=&r,&r,r,r,r",
    DImode,
    0,
    1
  },
  {
    reg_or_short_operand,
    "r,I,0,r,I",
    DImode,
    0,
    1
  },
  {
    gpc_reg_operand,
    "r,r,r,0,0",
    DImode,
    0,
    1
  },
  {
    gpc_reg_operand,
    "=&r,r",
    DImode,
    0,
    1
  },
  {
    gpc_reg_operand,
    "r,0",
    DImode,
    0,
    1
  },
  {
    gpc_reg_operand,
    "=r",
    DImode,
    0,
    1
  },
  {
    gpc_reg_operand,
    "%r",
    SImode,
    0,
    1
  },
  {
    gpc_reg_operand,
    "r",
    SImode,
    0,
    1
  },
  {
    scratch_operand,
    "=q",
    SImode,
    0,
    0
  },
  {
    gpc_reg_operand,
    "=&r",
    DImode,
    0,
    1
  },
  {
    gpc_reg_operand,
    "%r",
    SImode,
    0,
    1
  },
  {
    gpc_reg_operand,
    "r",
    SImode,
    0,
    1
  },
  {
    scratch_operand,
    "=q",
    SImode,
    0,
    0
  },
  {
    gpc_reg_operand,
    "=r",
    SImode,
    0,
    1
  },
  {
    gpc_reg_operand,
    "%r",
    SImode,
    0,
    1
  },
  {
    gpc_reg_operand,
    "r",
    SImode,
    0,
    1
  },
  {
    scratch_operand,
    "=q",
    SImode,
    0,
    0
  },
  {
    gpc_reg_operand,
    "=r,r,r,&r",
    DImode,
    0,
    1
  },
  {
    gpc_reg_operand,
    "r,r,0,r",
    DImode,
    0,
    1
  },
  {
    reg_or_cint_operand,
    "M,i,r,r",
    SImode,
    0,
    1
  },
  {
    scratch_operand,
    "=X,q,q,q",
    SImode,
    0,
    0
  },
  {
    gpc_reg_operand,
    "=&r,r",
    DImode,
    0,
    1
  },
  {
    gpc_reg_operand,
    "r,r",
    DImode,
    0,
    1
  },
  {
    const_int_operand,
    "M,i",
    SImode,
    0,
    1
  },
  {
    scratch_operand,
    "=X,q",
    SImode,
    0,
    0
  },
  {
    gpc_reg_operand,
    "=&r,&r",
    DImode,
    0,
    1
  },
  {
    gpc_reg_operand,
    "r,r",
    DImode,
    0,
    1
  },
  {
    const_int_operand,
    "M,i",
    SImode,
    0,
    1
  },
  {
    gpc_reg_operand,
    "=r,r,?r,r",
    DImode,
    0,
    1
  },
  {
    gpc_reg_operand,
    "%r,b,r,b",
    DImode,
    0,
    1
  },
  {
    add_operand,
    "r,I,I,L",
    DImode,
    0,
    1
  },
  {
    cc_reg_operand,
    "=x,x,?y,?y",
    CCmode,
    0,
    1
  },
  {
    gpc_reg_operand,
    "%r,r,r,r",
    DImode,
    0,
    1
  },
  {
    reg_or_short_operand,
    "r,I,r,I",
    DImode,
    0,
    1
  },
  {
    scratch_operand,
    "=r,r,r,r",
    DImode,
    0,
    0
  },
  {
    gpc_reg_operand,
    "=r,r,r,r",
    DImode,
    0,
    1
  },
  {
    gpc_reg_operand,
    "%r,r,r,r",
    DImode,
    0,
    1
  },
  {
    reg_or_short_operand,
    "r,I,r,I",
    DImode,
    0,
    1
  },
  {
    cc_reg_operand,
    "=x,x,?y,?y",
    CCmode,
    0,
    1
  },
  {
    cc_reg_operand,
    "=x,?y",
    CCmode,
    0,
    1
  },
  {
    gpc_reg_operand,
    "r,r",
    DImode,
    0,
    1
  },
  {
    scratch_operand,
    "=r,r",
    DImode,
    0,
    0
  },
  {
    gpc_reg_operand,
    "=r,r",
    DImode,
    0,
    1
  },
  {
    gpc_reg_operand,
    "r,r",
    DImode,
    0,
    1
  },
  {
    cc_reg_operand,
    "=x,?y",
    CCmode,
    0,
    1
  },
  {
    gpc_reg_operand,
    "=r,r",
    DImode,
    0,
    1
  },
  {
    reg_or_short_operand,
    "r,I",
    DImode,
    0,
    1
  },
  {
    gpc_reg_operand,
    "r,r",
    DImode,
    0,
    1
  },
  {
    cc_reg_operand,
    "=x,?y",
    CCmode,
    0,
    1
  },
  {
    gpc_reg_operand,
    "r,r",
    DImode,
    0,
    1
  },
  {
    gpc_reg_operand,
    "r,r",
    DImode,
    0,
    1
  },
  {
    scratch_operand,
    "=r,r",
    DImode,
    0,
    0
  },
  {
    gpc_reg_operand,
    "=r,r",
    DImode,
    0,
    1
  },
  {
    gpc_reg_operand,
    "r,r",
    DImode,
    0,
    1
  },
  {
    gpc_reg_operand,
    "r,r",
    DImode,
    0,
    1
  },
  {
    cc_reg_operand,
    "=x,?y",
    CCmode,
    0,
    1
  },
  {
    gpc_reg_operand,
    "=&r,r",
    DImode,
    0,
    1
  },
  {
    gpc_reg_operand,
    "r,0",
    DImode,
    0,
    1
  },
  {
    scratch_operand,
    "=&r,&r",
    DImode,
    0,
    0
  },
  {
    gpc_reg_operand,
    "=&r",
    DImode,
    0,
    1
  },
  {
    gpc_reg_operand,
    "r",
    DImode,
    0,
    1
  },
  {
    gpc_reg_operand,
    "=r",
    DImode,
    0,
    1
  },
  {
    gpc_reg_operand,
    "%r",
    DImode,
    0,
    1
  },
  {
    gpc_reg_operand,
    "r",
    DImode,
    0,
    1
  },
  {
    gpc_reg_operand,
    "=r",
    DImode,
    0,
    1
  },
  {
    gpc_reg_operand,
    "r",
    DImode,
    0,
    1
  },
  {
    exact_log2_cint_operand,
    "N",
    DImode,
    0,
    1
  },
  {
    cc_reg_operand,
    "=x,?y",
    CCmode,
    0,
    1
  },
  {
    gpc_reg_operand,
    "r,r",
    DImode,
    0,
    1
  },
  {
    exact_log2_cint_operand,
    "N,N",
    DImode,
    0,
    1
  },
  {
    scratch_operand,
    "=r,r",
    DImode,
    0,
    0
  },
  {
    gpc_reg_operand,
    "=r,r",
    DImode,
    0,
    1
  },
  {
    gpc_reg_operand,
    "r,r",
    DImode,
    0,
    1
  },
  {
    exact_log2_cint_operand,
    "N,N",
    DImode,
    0,
    1
  },
  {
    cc_reg_operand,
    "=x,?y",
    CCmode,
    0,
    1
  },
  {
    gpc_reg_operand,
    "=r",
    DImode,
    0,
    1
  },
  {
    gpc_reg_operand,
    "r",
    DImode,
    0,
    1
  },
  {
    gpc_reg_operand,
    "r",
    DImode,
    0,
    1
  },
  {
    gpc_reg_operand,
    "=r",
    DImode,
    0,
    1
  },
  {
    gpc_reg_operand,
    "r",
    DImode,
    0,
    1
  },
  {
    reg_or_cint_operand,
    "ri",
    DImode,
    0,
    1
  },
  {
    cc_reg_operand,
    "=x,?y",
    CCmode,
    0,
    1
  },
  {
    gpc_reg_operand,
    "r,r",
    DImode,
    0,
    1
  },
  {
    reg_or_cint_operand,
    "ri,ri",
    DImode,
    0,
    1
  },
  {
    scratch_operand,
    "=r,r",
    DImode,
    0,
    0
  },
  {
    gpc_reg_operand,
    "=r,r",
    DImode,
    0,
    1
  },
  {
    gpc_reg_operand,
    "r,r",
    DImode,
    0,
    1
  },
  {
    reg_or_cint_operand,
    "ri,ri",
    DImode,
    0,
    1
  },
  {
    cc_reg_operand,
    "=x,?y",
    CCmode,
    0,
    1
  },
  {
    gpc_reg_operand,
    "=r",
    DImode,
    0,
    1
  },
  {
    gpc_reg_operand,
    "r",
    DImode,
    0,
    1
  },
  {
    reg_or_cint_operand,
    "ri",
    DImode,
    0,
    1
  },
  {
    mask64_operand,
    "n",
    DImode,
    0,
    1
  },
  {
    cc_reg_operand,
    "=x,?y",
    CCmode,
    0,
    1
  },
  {
    gpc_reg_operand,
    "r,r",
    DImode,
    0,
    1
  },
  {
    reg_or_cint_operand,
    "ri,ri",
    DImode,
    0,
    1
  },
  {
    mask64_operand,
    "n,n",
    DImode,
    0,
    1
  },
  {
    scratch_operand,
    "=r,r",
    DImode,
    0,
    0
  },
  {
    gpc_reg_operand,
    "=r,r",
    DImode,
    0,
    1
  },
  {
    gpc_reg_operand,
    "r,r",
    DImode,
    0,
    1
  },
  {
    reg_or_cint_operand,
    "ri,ri",
    DImode,
    0,
    1
  },
  {
    mask64_operand,
    "n,n",
    DImode,
    0,
    1
  },
  {
    cc_reg_operand,
    "=x,?y",
    CCmode,
    0,
    1
  },
  {
    gpc_reg_operand,
    "=r",
    DImode,
    0,
    1
  },
  {
    gpc_reg_operand,
    "r",
    DImode,
    0,
    1
  },
  {
    reg_or_cint_operand,
    "ri",
    SImode,
    0,
    1
  },
  {
    cc_reg_operand,
    "=x,?y",
    CCmode,
    0,
    1
  },
  {
    gpc_reg_operand,
    "r,r",
    DImode,
    0,
    1
  },
  {
    reg_or_cint_operand,
    "ri,ri",
    SImode,
    0,
    1
  },
  {
    scratch_operand,
    "=r,r",
    DImode,
    0,
    0
  },
  {
    gpc_reg_operand,
    "=r,r",
    DImode,
    0,
    1
  },
  {
    gpc_reg_operand,
    "r,r",
    DImode,
    0,
    1
  },
  {
    reg_or_cint_operand,
    "ri,ri",
    SImode,
    0,
    1
  },
  {
    cc_reg_operand,
    "=x,?y",
    CCmode,
    0,
    1
  },
  {
    gpc_reg_operand,
    "=r",
    DImode,
    0,
    1
  },
  {
    gpc_reg_operand,
    "r",
    DImode,
    0,
    1
  },
  {
    const_int_operand,
    "i",
    SImode,
    0,
    1
  },
  {
    const_int_operand,
    "n",
    DImode,
    0,
    1
  },
  {
    cc_reg_operand,
    "=x,?y",
    CCmode,
    0,
    1
  },
  {
    gpc_reg_operand,
    "r,r",
    DImode,
    0,
    1
  },
  {
    const_int_operand,
    "i,i",
    SImode,
    0,
    1
  },
  {
    const_int_operand,
    "n,n",
    DImode,
    0,
    1
  },
  {
    scratch_operand,
    "=r,r",
    DImode,
    0,
    0
  },
  {
    gpc_reg_operand,
    "=r,r",
    DImode,
    0,
    1
  },
  {
    gpc_reg_operand,
    "r,r",
    DImode,
    0,
    1
  },
  {
    const_int_operand,
    "i,i",
    SImode,
    0,
    1
  },
  {
    const_int_operand,
    "n,n",
    DImode,
    0,
    1
  },
  {
    cc_reg_operand,
    "=x,?y",
    CCmode,
    0,
    1
  },
  {
    gpc_reg_operand,
    "=r",
    DImode,
    0,
    1
  },
  {
    gpc_reg_operand,
    "r",
    DImode,
    0,
    1
  },
  {
    const_int_operand,
    "i",
    SImode,
    0,
    1
  },
  {
    mask64_operand,
    "n",
    DImode,
    0,
    1
  },
  {
    cc_reg_operand,
    "=x,?y",
    CCmode,
    0,
    1
  },
  {
    gpc_reg_operand,
    "r,r",
    DImode,
    0,
    1
  },
  {
    const_int_operand,
    "i,i",
    SImode,
    0,
    1
  },
  {
    mask64_operand,
    "n,n",
    DImode,
    0,
    1
  },
  {
    scratch_operand,
    "=r,r",
    DImode,
    0,
    0
  },
  {
    gpc_reg_operand,
    "=r,r",
    DImode,
    0,
    1
  },
  {
    gpc_reg_operand,
    "r,r",
    DImode,
    0,
    1
  },
  {
    const_int_operand,
    "i,i",
    SImode,
    0,
    1
  },
  {
    mask64_operand,
    "n,n",
    DImode,
    0,
    1
  },
  {
    cc_reg_operand,
    "=x,?y",
    CCmode,
    0,
    1
  },
  {
    gpc_reg_operand,
    "=r,r,r,r,r",
    DImode,
    0,
    1
  },
  {
    gpc_reg_operand,
    "%r,r,r,r,r",
    DImode,
    0,
    1
  },
  {
    and64_2_operand,
    "?r,S,K,J,t",
    DImode,
    0,
    1
  },
  {
    scratch_operand,
    "=X,X,x,x,X",
    CCmode,
    0,
    0
  },
  {
    cc_reg_operand,
    "=x,x,x,x,x,?y,?y,??y,??y,?y",
    CCmode,
    0,
    1
  },
  {
    gpc_reg_operand,
    "%r,r,r,r,r,r,r,r,r,r",
    DImode,
    0,
    1
  },
  {
    and64_2_operand,
    "r,S,K,J,t,r,S,K,J,t",
    DImode,
    0,
    1
  },
  {
    scratch_operand,
    "=r,r,r,r,r,r,r,r,r,r",
    DImode,
    0,
    0
  },
  {
    scratch_operand,
    "=X,X,X,X,X,X,X,x,x,X",
    CCmode,
    0,
    0
  },
  {
    gpc_reg_operand,
    "=r,r,r,r,r,r,r,r,r,r",
    DImode,
    0,
    1
  },
  {
    gpc_reg_operand,
    "%r,r,r,r,r,r,r,r,r,r",
    DImode,
    0,
    1
  },
  {
    and64_2_operand,
    "r,S,K,J,t,r,S,K,J,t",
    DImode,
    0,
    1
  },
  {
    cc_reg_operand,
    "=x,x,x,x,x,?y,?y,??y,??y,?y",
    CCmode,
    0,
    1
  },
  {
    scratch_operand,
    "=X,X,X,X,X,X,X,x,x,X",
    CCmode,
    0,
    0
  },
  {
    gpc_reg_operand,
    "=r,r,r",
    DImode,
    0,
    1
  },
  {
    gpc_reg_operand,
    "%r,r,r",
    DImode,
    0,
    1
  },
  {
    logical_operand,
    "r,K,JF",
    DImode,
    0,
    1
  },
  {
    boolean_or_operator,
    "",
    DImode,
    0,
    0
  },
  {
    cc_reg_operand,
    "=x,?y",
    CCmode,
    0,
    1
  },
  {
    gpc_reg_operand,
    "%r,r",
    DImode,
    0,
    1
  },
  {
    gpc_reg_operand,
    "r,r",
    DImode,
    0,
    1
  },
  {
    scratch_operand,
    "=r,r",
    DImode,
    0,
    0
  },
  {
    boolean_or_operator,
    "",
    DImode,
    0,
    0
  },
  {
    gpc_reg_operand,
    "=r,r",
    DImode,
    0,
    1
  },
  {
    gpc_reg_operand,
    "%r,r",
    DImode,
    0,
    1
  },
  {
    gpc_reg_operand,
    "r,r",
    DImode,
    0,
    1
  },
  {
    cc_reg_operand,
    "=x,?y",
    CCmode,
    0,
    1
  },
  {
    boolean_operator,
    "",
    DImode,
    0,
    0
  },
  {
    gpc_reg_operand,
    "=r",
    DImode,
    0,
    1
  },
  {
    gpc_reg_operand,
    "r",
    DImode,
    0,
    1
  },
  {
    gpc_reg_operand,
    "r",
    DImode,
    0,
    1
  },
  {
    boolean_operator,
    "",
    DImode,
    0,
    0
  },
  {
    cc_reg_operand,
    "=x,?y",
    CCmode,
    0,
    1
  },
  {
    gpc_reg_operand,
    "r,r",
    DImode,
    0,
    1
  },
  {
    gpc_reg_operand,
    "r,r",
    DImode,
    0,
    1
  },
  {
    scratch_operand,
    "=r,r",
    DImode,
    0,
    0
  },
  {
    boolean_operator,
    "",
    DImode,
    0,
    0
  },
  {
    gpc_reg_operand,
    "=b*r",
    SImode,
    0,
    1
  },
  {
    0,
    "",
    VOIDmode,
    0,
    1
  },
  {
    gpc_reg_operand,
    "=r,r",
    SImode,
    0,
    1
  },
  {
    gpc_reg_operand,
    "b,!*r",
    SImode,
    0,
    1
  },
  {
    0,
    "",
    VOIDmode,
    0,
    1
  },
  {
    gpc_reg_operand,
    "=r",
    SImode,
    0,
    1
  },
  {
    got_no_const_operand,
    "",
    SImode,
    0,
    1
  },
  {
    gpc_reg_operand,
    "b",
    SImode,
    0,
    1
  },
  {
    nonimmediate_operand,
    "=r,r,r,m,r,r,r,r,r,*q,*c*l,*h,*h",
    SImode,
    0,
    1
  },
  {
    input_operand,
    "r,U,m,r,I,L,n,R,*h,r,r,r,0",
    SImode,
    0,
    1
  },
  {
    nonimmediate_operand,
    "=r,r,m,r,r,*q,*c*l,*h",
    HImode,
    0,
    1
  },
  {
    input_operand,
    "r,m,r,i,*h,r,r,0",
    HImode,
    0,
    1
  },
  {
    nonimmediate_operand,
    "=r,r,m,r,r,*q,*c*l,*h",
    QImode,
    0,
    1
  },
  {
    input_operand,
    "r,m,r,i,*h,r,r,0",
    QImode,
    0,
    1
  },
  {
    nonimmediate_operand,
    "=y,x,y,r,r,r,cl,q,r,r,m",
    CCmode,
    0,
    1
  },
  {
    nonimmediate_operand,
    "y,r,r,x,y,r,r,r,h,m,r",
    CCmode,
    0,
    1
  },
  {
    nonimmediate_operand,
    "=!r,!r,m,f,f,m,!cl,!q,!r,!r,!r",
    SFmode,
    0,
    1
  },
  {
    input_operand,
    "r,m,r,f,m,f,r,r,h,G,Fn",
    SFmode,
    0,
    1
  },
  {
    nonimmediate_operand,
    "=r,cl,q,r,r,m,r,r,r,r,r",
    SFmode,
    0,
    1
  },
  {
    input_operand,
    "r,r,r,h,m,r,I,L,R,G,Fn",
    SFmode,
    0,
    1
  },
  {
    nonimmediate_operand,
    "=!r,??r,m,f,f,m,!r,!r,!r",
    DFmode,
    0,
    1
  },
  {
    input_operand,
    "r,m,r,f,m,f,G,H,F",
    DFmode,
    0,
    1
  },
  {
    nonimmediate_operand,
    "=r,r,m,r,r,r",
    DFmode,
    0,
    1
  },
  {
    input_operand,
    "r,m,r,G,H,F",
    DFmode,
    0,
    1
  },
  {
    nonimmediate_operand,
    "=!r,??r,m,f,f,m,!cl,!r,!r,!r,!r",
    DFmode,
    0,
    1
  },
  {
    input_operand,
    "r,m,r,f,m,f,r,h,G,H,F",
    DFmode,
    0,
    1
  },
  {
    nonimmediate_operand,
    "=r,cl,r,r,m,r,r,r",
    DFmode,
    0,
    1
  },
  {
    input_operand,
    "r,r,h,m,r,G,H,F",
    DFmode,
    0,
    1
  },
  {
    nonimmediate_operand,
    "=f,f,m,!r,!r,!r",
    TFmode,
    0,
    1
  },
  {
    input_operand,
    "f,m,f,G,H,F",
    TFmode,
    0,
    1
  },
  {
    gpc_reg_operand,
    "=f",
    TFmode,
    0,
    1
  },
  {
    gpc_reg_operand,
    "f",
    DFmode,
    0,
    1
  },
  {
    gpc_reg_operand,
    "=f",
    TFmode,
    0,
    1
  },
  {
    gpc_reg_operand,
    "f",
    SFmode,
    0,
    1
  },
  {
    gpc_reg_operand,
    "=f",
    DFmode,
    0,
    1
  },
  {
    gpc_reg_operand,
    "f",
    TFmode,
    0,
    1
  },
  {
    gpc_reg_operand,
    "=f",
    SFmode,
    0,
    1
  },
  {
    gpc_reg_operand,
    "f",
    TFmode,
    0,
    1
  },
  {
    scratch_operand,
    "=f",
    DFmode,
    0,
    0
  },
  {
    gpc_reg_operand,
    "=f",
    TFmode,
    0,
    1
  },
  {
    gpc_reg_operand,
    "*f",
    DImode,
    0,
    1
  },
  {
    scratch_operand,
    "=f",
    DFmode,
    0,
    0
  },
  {
    gpc_reg_operand,
    "=f",
    TFmode,
    0,
    1
  },
  {
    gpc_reg_operand,
    "r",
    SImode,
    0,
    1
  },
  {
    scratch_operand,
    "=f",
    DFmode,
    0,
    0
  },
  {
    gpc_reg_operand,
    "=*f",
    DImode,
    0,
    1
  },
  {
    gpc_reg_operand,
    "f",
    TFmode,
    0,
    1
  },
  {
    scratch_operand,
    "=f",
    DFmode,
    0,
    0
  },
  {
    gpc_reg_operand,
    "=r",
    SImode,
    0,
    1
  },
  {
    gpc_reg_operand,
    "f",
    TFmode,
    0,
    1
  },
  {
    scratch_operand,
    "=f",
    DFmode,
    0,
    0
  },
  {
    gpc_reg_operand,
    "=f",
    TFmode,
    0,
    1
  },
  {
    gpc_reg_operand,
    "f",
    TFmode,
    0,
    1
  },
  {
    nonimmediate_operand,
    "=r,r,m,f,f,m,r,r,r,r,r",
    DImode,
    0,
    1
  },
  {
    input_operand,
    "r,m,r,f,m,f,IJK,n,G,H,F",
    DImode,
    0,
    1
  },
  {
    nonimmediate_operand,
    "=r,r,m,r,r,r,r,?f,f,m,r,*h,*h",
    DImode,
    0,
    1
  },
  {
    input_operand,
    "r,m,r,I,L,nF,R,f,m,f,*h,r,0",
    DImode,
    0,
    1
  },
  {
    reg_or_mem_operand,
    "=Q,m,????r,????r,????r",
    TImode,
    0,
    1
  },
  {
    reg_or_mem_operand,
    "r,r,r,Q,m",
    TImode,
    0,
    1
  },
  {
    scratch_operand,
    "=q,q#X,X,X,X",
    SImode,
    0,
    0
  },
  {
    nonimmediate_operand,
    "=r,r,m",
    TImode,
    0,
    1
  },
  {
    input_operand,
    "r,m,r",
    TImode,
    0,
    1
  },
  {
    load_multiple_operation,
    "",
    VOIDmode,
    0,
    0
  },
  {
    gpc_reg_operand,
    "b",
    SImode,
    0,
    1
  },
  {
    gpc_reg_operand,
    "",
    SImode,
    0,
    1
  },
  {
    gpc_reg_operand,
    "",
    SImode,
    0,
    1
  },
  {
    gpc_reg_operand,
    "",
    SImode,
    0,
    1
  },
  {
    gpc_reg_operand,
    "",
    SImode,
    0,
    1
  },
  {
    gpc_reg_operand,
    "",
    SImode,
    0,
    1
  },
  {
    gpc_reg_operand,
    "",
    SImode,
    0,
    1
  },
  {
    gpc_reg_operand,
    "",
    SImode,
    0,
    1
  },
  {
    gpc_reg_operand,
    "",
    SImode,
    0,
    1
  },
  {
    store_multiple_operation,
    "",
    VOIDmode,
    0,
    0
  },
  {
    indirect_operand,
    "=Q",
    SImode,
    0,
    1
  },
  {
    gpc_reg_operand,
    "r",
    SImode,
    0,
    1
  },
  {
    scratch_operand,
    "=q",
    SImode,
    0,
    0
  },
  {
    store_multiple_operation,
    "",
    VOIDmode,
    0,
    0
  },
  {
    gpc_reg_operand,
    "b",
    SImode,
    0,
    1
  },
  {
    gpc_reg_operand,
    "r",
    SImode,
    0,
    1
  },
  {
    scratch_operand,
    "X",
    SImode,
    0,
    0
  },
  {
    gpc_reg_operand,
    "b",
    SImode,
    0,
    1
  },
  {
    gpc_reg_operand,
    "b",
    SImode,
    0,
    1
  },
  {
    immediate_operand,
    "i",
    SImode,
    0,
    1
  },
  {
    immediate_operand,
    "i",
    SImode,
    0,
    1
  },
  {
    gpc_reg_operand,
    "=r",
    SImode,
    0,
    1
  },
  {
    scratch_operand,
    "=q",
    SImode,
    0,
    0
  },
  {
    gpc_reg_operand,
    "b",
    SImode,
    0,
    1
  },
  {
    gpc_reg_operand,
    "b",
    SImode,
    0,
    1
  },
  {
    immediate_operand,
    "i",
    SImode,
    0,
    1
  },
  {
    immediate_operand,
    "i",
    SImode,
    0,
    1
  },
  {
    gpc_reg_operand,
    "=r",
    SImode,
    0,
    1
  },
  {
    scratch_operand,
    "X",
    SImode,
    0,
    0
  },
  {
    gpc_reg_operand,
    "b",
    DImode,
    0,
    1
  },
  {
    gpc_reg_operand,
    "b",
    DImode,
    0,
    1
  },
  {
    immediate_operand,
    "i",
    SImode,
    0,
    1
  },
  {
    immediate_operand,
    "i",
    SImode,
    0,
    1
  },
  {
    gpc_reg_operand,
    "=r",
    SImode,
    0,
    1
  },
  {
    scratch_operand,
    "X",
    SImode,
    0,
    0
  },
  {
    gpc_reg_operand,
    "b",
    SImode,
    0,
    1
  },
  {
    gpc_reg_operand,
    "b",
    SImode,
    0,
    1
  },
  {
    immediate_operand,
    "i",
    SImode,
    0,
    1
  },
  {
    immediate_operand,
    "i",
    SImode,
    0,
    1
  },
  {
    scratch_operand,
    "=&r",
    DImode,
    0,
    0
  },
  {
    scratch_operand,
    "=q",
    SImode,
    0,
    0
  },
  {
    gpc_reg_operand,
    "b",
    SImode,
    0,
    1
  },
  {
    gpc_reg_operand,
    "b",
    SImode,
    0,
    1
  },
  {
    immediate_operand,
    "i",
    SImode,
    0,
    1
  },
  {
    immediate_operand,
    "i",
    SImode,
    0,
    1
  },
  {
    scratch_operand,
    "=&r",
    DImode,
    0,
    0
  },
  {
    scratch_operand,
    "X",
    SImode,
    0,
    0
  },
  {
    gpc_reg_operand,
    "b",
    SImode,
    0,
    1
  },
  {
    gpc_reg_operand,
    "b",
    SImode,
    0,
    1
  },
  {
    immediate_operand,
    "i",
    SImode,
    0,
    1
  },
  {
    immediate_operand,
    "i",
    SImode,
    0,
    1
  },
  {
    scratch_operand,
    "=&r",
    SImode,
    0,
    0
  },
  {
    scratch_operand,
    "=q",
    SImode,
    0,
    0
  },
  {
    gpc_reg_operand,
    "b",
    SImode,
    0,
    1
  },
  {
    gpc_reg_operand,
    "b",
    SImode,
    0,
    1
  },
  {
    immediate_operand,
    "i",
    SImode,
    0,
    1
  },
  {
    immediate_operand,
    "i",
    SImode,
    0,
    1
  },
  {
    scratch_operand,
    "=&r",
    SImode,
    0,
    0
  },
  {
    scratch_operand,
    "X",
    SImode,
    0,
    0
  },
  {
    gpc_reg_operand,
    "b",
    DImode,
    0,
    1
  },
  {
    gpc_reg_operand,
    "b",
    DImode,
    0,
    1
  },
  {
    immediate_operand,
    "i",
    SImode,
    0,
    1
  },
  {
    immediate_operand,
    "i",
    SImode,
    0,
    1
  },
  {
    scratch_operand,
    "=&r",
    SImode,
    0,
    0
  },
  {
    scratch_operand,
    "X",
    SImode,
    0,
    0
  },
  {
    gpc_reg_operand,
    "=b,b",
    DImode,
    0,
    1
  },
  {
    gpc_reg_operand,
    "0,0",
    DImode,
    0,
    1
  },
  {
    reg_or_aligned_short_operand,
    "r,I",
    DImode,
    0,
    1
  },
  {
    gpc_reg_operand,
    "=r,r",
    DImode,
    0,
    1
  },
  {
    gpc_reg_operand,
    "=b",
    DImode,
    0,
    1
  },
  {
    gpc_reg_operand,
    "0",
    DImode,
    0,
    1
  },
  {
    gpc_reg_operand,
    "r",
    DImode,
    0,
    1
  },
  {
    gpc_reg_operand,
    "=r",
    DImode,
    0,
    1
  },
  {
    gpc_reg_operand,
    "=b,b",
    DImode,
    0,
    1
  },
  {
    gpc_reg_operand,
    "0,0",
    DImode,
    0,
    1
  },
  {
    reg_or_aligned_short_operand,
    "r,I",
    DImode,
    0,
    1
  },
  {
    gpc_reg_operand,
    "r,r",
    DImode,
    0,
    1
  },
  {
    gpc_reg_operand,
    "=b,b",
    SImode,
    0,
    1
  },
  {
    gpc_reg_operand,
    "0,0",
    SImode,
    0,
    1
  },
  {
    reg_or_short_operand,
    "r,I",
    SImode,
    0,
    1
  },
  {
    gpc_reg_operand,
    "=r,r",
    SImode,
    0,
    1
  },
  {
    gpc_reg_operand,
    "=b,b",
    SImode,
    0,
    1
  },
  {
    gpc_reg_operand,
    "0,0",
    SImode,
    0,
    1
  },
  {
    reg_or_short_operand,
    "r,I",
    SImode,
    0,
    1
  },
  {
    gpc_reg_operand,
    "r,r",
    SImode,
    0,
    1
  },
  {
    gpc_reg_operand,
    "=b,b",
    SImode,
    0,
    1
  },
  {
    gpc_reg_operand,
    "0,0",
    SImode,
    0,
    1
  },
  {
    reg_or_short_operand,
    "r,I",
    SImode,
    0,
    1
  },
  {
    gpc_reg_operand,
    "=r,r",
    HImode,
    0,
    1
  },
  {
    gpc_reg_operand,
    "=b,b",
    SImode,
    0,
    1
  },
  {
    gpc_reg_operand,
    "0,0",
    SImode,
    0,
    1
  },
  {
    reg_or_short_operand,
    "r,I",
    SImode,
    0,
    1
  },
  {
    gpc_reg_operand,
    "r,r",
    HImode,
    0,
    1
  },
  {
    gpc_reg_operand,
    "=b,b",
    SImode,
    0,
    1
  },
  {
    gpc_reg_operand,
    "0,0",
    SImode,
    0,
    1
  },
  {
    reg_or_short_operand,
    "r,I",
    SImode,
    0,
    1
  },
  {
    gpc_reg_operand,
    "=r,r",
    QImode,
    0,
    1
  },
  {
    gpc_reg_operand,
    "=b,b",
    SImode,
    0,
    1
  },
  {
    gpc_reg_operand,
    "0,0",
    SImode,
    0,
    1
  },
  {
    reg_or_short_operand,
    "r,I",
    SImode,
    0,
    1
  },
  {
    gpc_reg_operand,
    "r,r",
    QImode,
    0,
    1
  },
  {
    gpc_reg_operand,
    "=b,b",
    SImode,
    0,
    1
  },
  {
    gpc_reg_operand,
    "0,0",
    SImode,
    0,
    1
  },
  {
    reg_or_short_operand,
    "r,I",
    SImode,
    0,
    1
  },
  {
    gpc_reg_operand,
    "=f,f",
    SFmode,
    0,
    1
  },
  {
    gpc_reg_operand,
    "=b,b",
    SImode,
    0,
    1
  },
  {
    gpc_reg_operand,
    "0,0",
    SImode,
    0,
    1
  },
  {
    reg_or_short_operand,
    "r,I",
    SImode,
    0,
    1
  },
  {
    gpc_reg_operand,
    "f,f",
    SFmode,
    0,
    1
  },
  {
    gpc_reg_operand,
    "=b,b",
    SImode,
    0,
    1
  },
  {
    gpc_reg_operand,
    "0,0",
    SImode,
    0,
    1
  },
  {
    reg_or_short_operand,
    "r,I",
    SImode,
    0,
    1
  },
  {
    gpc_reg_operand,
    "=r,r",
    SFmode,
    0,
    1
  },
  {
    gpc_reg_operand,
    "=b,b",
    SImode,
    0,
    1
  },
  {
    gpc_reg_operand,
    "0,0",
    SImode,
    0,
    1
  },
  {
    reg_or_short_operand,
    "r,I",
    SImode,
    0,
    1
  },
  {
    gpc_reg_operand,
    "r,r",
    SFmode,
    0,
    1
  },
  {
    gpc_reg_operand,
    "=b,b",
    SImode,
    0,
    1
  },
  {
    gpc_reg_operand,
    "0,0",
    SImode,
    0,
    1
  },
  {
    reg_or_short_operand,
    "r,I",
    SImode,
    0,
    1
  },
  {
    gpc_reg_operand,
    "=f,f",
    DFmode,
    0,
    1
  },
  {
    gpc_reg_operand,
    "=b,b",
    SImode,
    0,
    1
  },
  {
    gpc_reg_operand,
    "0,0",
    SImode,
    0,
    1
  },
  {
    reg_or_short_operand,
    "r,I",
    SImode,
    0,
    1
  },
  {
    gpc_reg_operand,
    "f,f",
    DFmode,
    0,
    1
  },
  {
    register_operand,
    "=l",
    SImode,
    0,
    1
  },
  {
    immediate_operand,
    "s",
    SImode,
    0,
    1
  },
  {
    immediate_operand,
    "s",
    VOIDmode,
    0,
    1
  },
  {
    gpc_reg_operand,
    "=r",
    SImode,
    0,
    1
  },
  {
    gpc_reg_operand,
    "b",
    SImode,
    0,
    1
  },
  {
    immediate_operand,
    "s",
    SImode,
    0,
    1
  },
  {
    immediate_operand,
    "s",
    SImode,
    0,
    1
  },
  {
    gpc_reg_operand,
    "=r",
    SImode,
    0,
    1
  },
  {
    gpc_reg_operand,
    "=r",
    SImode,
    0,
    1
  },
  {
    immediate_operand,
    "s",
    SImode,
    0,
    1
  },
  {
    immediate_operand,
    "s",
    SImode,
    0,
    1
  },
  {
    current_file_function_operand,
    "s,s",
    SImode,
    0,
    1
  },
  {
    0,
    "g,g",
    VOIDmode,
    0,
    1
  },
  {
    immediate_operand,
    "O,n",
    SImode,
    0,
    1
  },
  {
    scratch_operand,
    "=l,l",
    SImode,
    0,
    0
  },
  {
    current_file_function_operand,
    "s,s",
    DImode,
    0,
    1
  },
  {
    0,
    "g,g",
    VOIDmode,
    0,
    1
  },
  {
    immediate_operand,
    "O,n",
    SImode,
    0,
    1
  },
  {
    scratch_operand,
    "=l,l",
    SImode,
    0,
    0
  },
  {
    0,
    "",
    VOIDmode,
    0,
    1
  },
  {
    current_file_function_operand,
    "s,s",
    SImode,
    0,
    1
  },
  {
    0,
    "g,g",
    VOIDmode,
    0,
    1
  },
  {
    immediate_operand,
    "O,n",
    SImode,
    0,
    1
  },
  {
    scratch_operand,
    "=l,l",
    SImode,
    0,
    0
  },
  {
    0,
    "",
    VOIDmode,
    0,
    1
  },
  {
    current_file_function_operand,
    "s,s",
    DImode,
    0,
    1
  },
  {
    0,
    "g,g",
    VOIDmode,
    0,
    1
  },
  {
    immediate_operand,
    "O,n",
    SImode,
    0,
    1
  },
  {
    scratch_operand,
    "=l,l",
    SImode,
    0,
    0
  },
  {
    register_operand,
    "cl",
    SImode,
    0,
    1
  },
  {
    0,
    "g",
    VOIDmode,
    0,
    1
  },
  {
    scratch_operand,
    "=l",
    SImode,
    0,
    0
  },
  {
    symbol_ref_operand,
    "s",
    SImode,
    0,
    1
  },
  {
    0,
    "g",
    VOIDmode,
    0,
    1
  },
  {
    immediate_operand,
    "O",
    SImode,
    0,
    1
  },
  {
    scratch_operand,
    "=l",
    SImode,
    0,
    0
  },
  {
    register_operand,
    "cl",
    DImode,
    0,
    1
  },
  {
    0,
    "g",
    VOIDmode,
    0,
    1
  },
  {
    scratch_operand,
    "=l",
    SImode,
    0,
    0
  },
  {
    symbol_ref_operand,
    "s",
    DImode,
    0,
    1
  },
  {
    0,
    "g",
    VOIDmode,
    0,
    1
  },
  {
    immediate_operand,
    "O",
    SImode,
    0,
    1
  },
  {
    scratch_operand,
    "=l",
    SImode,
    0,
    0
  },
  {
    0,
    "",
    VOIDmode,
    0,
    1
  },
  {
    register_operand,
    "cl",
    SImode,
    0,
    1
  },
  {
    0,
    "g",
    VOIDmode,
    0,
    1
  },
  {
    scratch_operand,
    "=l",
    SImode,
    0,
    0
  },
  {
    0,
    "",
    VOIDmode,
    0,
    1
  },
  {
    symbol_ref_operand,
    "s",
    SImode,
    0,
    1
  },
  {
    0,
    "g",
    VOIDmode,
    0,
    1
  },
  {
    immediate_operand,
    "O",
    SImode,
    0,
    1
  },
  {
    scratch_operand,
    "=l",
    SImode,
    0,
    0
  },
  {
    0,
    "",
    VOIDmode,
    0,
    1
  },
  {
    register_operand,
    "cl",
    DImode,
    0,
    1
  },
  {
    0,
    "g",
    VOIDmode,
    0,
    1
  },
  {
    scratch_operand,
    "=l",
    SImode,
    0,
    0
  },
  {
    0,
    "",
    VOIDmode,
    0,
    1
  },
  {
    symbol_ref_operand,
    "s",
    DImode,
    0,
    1
  },
  {
    0,
    "g",
    VOIDmode,
    0,
    1
  },
  {
    immediate_operand,
    "O",
    SImode,
    0,
    1
  },
  {
    scratch_operand,
    "=l",
    SImode,
    0,
    0
  },
  {
    register_operand,
    "cl,cl",
    SImode,
    0,
    1
  },
  {
    0,
    "g,g",
    VOIDmode,
    0,
    1
  },
  {
    immediate_operand,
    "O,n",
    SImode,
    0,
    1
  },
  {
    scratch_operand,
    "=l,l",
    SImode,
    0,
    0
  },
  {
    symbol_ref_operand,
    "s,s",
    SImode,
    0,
    1
  },
  {
    0,
    "g,g",
    VOIDmode,
    0,
    1
  },
  {
    immediate_operand,
    "O,n",
    SImode,
    0,
    1
  },
  {
    scratch_operand,
    "=l,l",
    SImode,
    0,
    0
  },
  {
    0,
    "",
    VOIDmode,
    0,
    1
  },
  {
    register_operand,
    "cl,cl",
    SImode,
    0,
    1
  },
  {
    0,
    "g,g",
    VOIDmode,
    0,
    1
  },
  {
    immediate_operand,
    "O,n",
    SImode,
    0,
    1
  },
  {
    scratch_operand,
    "=l,l",
    SImode,
    0,
    0
  },
  {
    0,
    "",
    VOIDmode,
    0,
    1
  },
  {
    symbol_ref_operand,
    "s,s",
    SImode,
    0,
    1
  },
  {
    0,
    "g,g",
    VOIDmode,
    0,
    1
  },
  {
    immediate_operand,
    "O,n",
    SImode,
    0,
    1
  },
  {
    scratch_operand,
    "=l,l",
    SImode,
    0,
    0
  },
  {
    symbol_ref_operand,
    "s,s",
    SImode,
    0,
    1
  },
  {
    0,
    "",
    VOIDmode,
    0,
    1
  },
  {
    immediate_operand,
    "O,n",
    VOIDmode,
    0,
    1
  },
  {
    scratch_operand,
    "=l,l",
    SImode,
    0,
    0
  },
  {
    0,
    "",
    VOIDmode,
    0,
    1
  },
  {
    symbol_ref_operand,
    "s,s",
    SImode,
    0,
    1
  },
  {
    0,
    "",
    VOIDmode,
    0,
    1
  },
  {
    immediate_operand,
    "O,n",
    SImode,
    0,
    1
  },
  {
    scratch_operand,
    "=l,l",
    SImode,
    0,
    0
  },
  {
    cc_reg_operand,
    "=y",
    CCmode,
    0,
    1
  },
  {
    gpc_reg_operand,
    "r",
    SImode,
    0,
    1
  },
  {
    reg_or_short_operand,
    "rI",
    SImode,
    0,
    1
  },
  {
    cc_reg_operand,
    "=y",
    CCmode,
    0,
    1
  },
  {
    gpc_reg_operand,
    "r",
    DImode,
    0,
    1
  },
  {
    reg_or_short_operand,
    "rI",
    DImode,
    0,
    1
  },
  {
    cc_reg_operand,
    "=y",
    CCUNSmode,
    0,
    1
  },
  {
    gpc_reg_operand,
    "r",
    SImode,
    0,
    1
  },
  {
    reg_or_u_short_operand,
    "rK",
    SImode,
    0,
    1
  },
  {
    cc_reg_operand,
    "=y",
    CCUNSmode,
    0,
    1
  },
  {
    gpc_reg_operand,
    "r",
    DImode,
    0,
    1
  },
  {
    reg_or_u_short_operand,
    "rK",
    DImode,
    0,
    1
  },
  {
    gpc_reg_operand,
    "=r",
    SImode,
    0,
    1
  },
  {
    gpc_reg_operand,
    "r",
    SImode,
    0,
    1
  },
  {
    short_cint_operand,
    "i",
    SImode,
    0,
    1
  },
  {
    cc_reg_operand,
    "=y",
    CCmode,
    0,
    1
  },
  {
    short_cint_operand,
    "i",
    SImode,
    0,
    1
  },
  {
    gpc_reg_operand,
    "=r",
    SImode,
    0,
    1
  },
  {
    gpc_reg_operand,
    "r",
    SImode,
    0,
    1
  },
  {
    u_short_cint_operand,
    "i",
    SImode,
    0,
    1
  },
  {
    cc_reg_operand,
    "=y",
    CCUNSmode,
    0,
    1
  },
  {
    short_cint_operand,
    "i",
    SImode,
    0,
    1
  },
  {
    cc_reg_operand,
    "=y",
    CCFPmode,
    0,
    1
  },
  {
    gpc_reg_operand,
    "f",
    SFmode,
    0,
    1
  },
  {
    gpc_reg_operand,
    "f",
    SFmode,
    0,
    1
  },
  {
    cc_reg_operand,
    "=y",
    CCFPmode,
    0,
    1
  },
  {
    gpc_reg_operand,
    "f",
    DFmode,
    0,
    1
  },
  {
    gpc_reg_operand,
    "f",
    DFmode,
    0,
    1
  },
  {
    cc_reg_operand,
    "=y",
    CCFPmode,
    0,
    1
  },
  {
    gpc_reg_operand,
    "f",
    TFmode,
    0,
    1
  },
  {
    gpc_reg_operand,
    "f",
    TFmode,
    0,
    1
  },
  {
    gpc_reg_operand,
    "=r",
    SImode,
    0,
    1
  },
  {
    scc_comparison_operator,
    "",
    SImode,
    0,
    0
  },
  {
    cc_reg_operand,
    "y",
    VOIDmode,
    0,
    1
  },
  {
    gpc_reg_operand,
    "=r",
    DImode,
    0,
    1
  },
  {
    scc_comparison_operator,
    "",
    DImode,
    0,
    0
  },
  {
    cc_reg_operand,
    "y",
    VOIDmode,
    0,
    1
  },
  {
    cc_reg_operand,
    "=x,?y",
    CCmode,
    0,
    1
  },
  {
    scc_comparison_operator,
    "",
    SImode,
    0,
    0
  },
  {
    cc_reg_operand,
    "y,y",
    VOIDmode,
    0,
    1
  },
  {
    gpc_reg_operand,
    "=r,r",
    SImode,
    0,
    1
  },
  {
    gpc_reg_operand,
    "=r",
    SImode,
    0,
    1
  },
  {
    scc_comparison_operator,
    "",
    SImode,
    0,
    0
  },
  {
    cc_reg_operand,
    "y",
    VOIDmode,
    0,
    1
  },
  {
    const_int_operand,
    "n",
    SImode,
    0,
    1
  },
  {
    cc_reg_operand,
    "=x,?y",
    CCmode,
    0,
    1
  },
  {
    scc_comparison_operator,
    "",
    SImode,
    0,
    0
  },
  {
    cc_reg_operand,
    "y,y",
    VOIDmode,
    0,
    1
  },
  {
    const_int_operand,
    "n,n",
    SImode,
    0,
    1
  },
  {
    gpc_reg_operand,
    "=r,r",
    SImode,
    0,
    1
  },
  {
    gpc_reg_operand,
    "=r,r,r,r,r",
    SImode,
    0,
    1
  },
  {
    gpc_reg_operand,
    "%r,r,r,r,r",
    SImode,
    0,
    1
  },
  {
    reg_or_cint_operand,
    "r,O,K,L,I",
    SImode,
    0,
    1
  },
  {
    scratch_operand,
    "=r,&r,r,r,r",
    SImode,
    0,
    0
  },
  {
    gpc_reg_operand,
    "=r,r,r,r,r",
    DImode,
    0,
    1
  },
  {
    gpc_reg_operand,
    "%r,r,r,r,r",
    DImode,
    0,
    1
  },
  {
    reg_or_cint_operand,
    "r,O,K,J,I",
    DImode,
    0,
    1
  },
  {
    scratch_operand,
    "=r,&r,r,r,r",
    DImode,
    0,
    0
  },
  {
    gpc_reg_operand,
    "=r,r,r,r,r,r,r,r,r,r",
    SImode,
    0,
    1
  },
  {
    gpc_reg_operand,
    "%r,r,r,r,r,r,r,r,r,r",
    SImode,
    0,
    1
  },
  {
    reg_or_cint_operand,
    "r,O,K,L,I,r,O,K,L,I",
    SImode,
    0,
    1
  },
  {
    scratch_operand,
    "=r,&r,r,r,r,r,&r,r,r,r",
    SImode,
    0,
    0
  },
  {
    cc_reg_operand,
    "=x,x,x,x,x,?y,?y,?y,?y,?y",
    CCmode,
    0,
    1
  },
  {
    gpc_reg_operand,
    "=r,r,r,r,r,r,r,r,r,r",
    DImode,
    0,
    1
  },
  {
    gpc_reg_operand,
    "%r,r,r,r,r,r,r,r,r,r",
    DImode,
    0,
    1
  },
  {
    reg_or_cint_operand,
    "r,O,K,J,I,r,O,K,J,I",
    DImode,
    0,
    1
  },
  {
    scratch_operand,
    "=r,&r,r,r,r,r,&r,r,r,r",
    DImode,
    0,
    0
  },
  {
    cc_reg_operand,
    "=x,x,x,x,x,?y,?y,?y,?y,?y",
    CCmode,
    0,
    1
  },
  {
    gpc_reg_operand,
    "=&r,&r,&r,&r,&r",
    SImode,
    0,
    1
  },
  {
    gpc_reg_operand,
    "%r,r,r,r,r",
    SImode,
    0,
    1
  },
  {
    reg_or_cint_operand,
    "r,O,K,L,I",
    SImode,
    0,
    1
  },
  {
    gpc_reg_operand,
    "r,r,r,r,r",
    SImode,
    0,
    1
  },
  {
    cc_reg_operand,
    "=x,x,x,x,x,?y,?y,?y,?y,?y",
    CCmode,
    0,
    1
  },
  {
    gpc_reg_operand,
    "%r,r,r,r,r,r,r,r,r,r",
    SImode,
    0,
    1
  },
  {
    reg_or_cint_operand,
    "r,O,K,L,I,r,O,K,L,I",
    SImode,
    0,
    1
  },
  {
    gpc_reg_operand,
    "r,r,r,r,r,r,r,r,r,r",
    SImode,
    0,
    1
  },
  {
    scratch_operand,
    "=&r,&r,&r,&r,&r,&r,&r,&r,&r,&r",
    SImode,
    0,
    0
  },
  {
    gpc_reg_operand,
    "=&r,&r,&r,&r,&r,&r,&r,&r,&r,&r",
    SImode,
    0,
    1
  },
  {
    gpc_reg_operand,
    "%r,r,r,r,r,r,r,r,r,r",
    SImode,
    0,
    1
  },
  {
    reg_or_cint_operand,
    "r,O,K,L,I,r,O,K,L,I",
    SImode,
    0,
    1
  },
  {
    gpc_reg_operand,
    "r,r,r,r,r,r,r,r,r,r",
    SImode,
    0,
    1
  },
  {
    cc_reg_operand,
    "=x,x,x,x,x,?y,?y,?y,?y,?y",
    CCmode,
    0,
    1
  },
  {
    gpc_reg_operand,
    "=&r",
    SImode,
    0,
    1
  },
  {
    gpc_reg_operand,
    "r",
    SImode,
    0,
    1
  },
  {
    scratch_operand,
    "=&r",
    SImode,
    0,
    0
  },
  {
    gpc_reg_operand,
    "=r",
    DImode,
    0,
    1
  },
  {
    gpc_reg_operand,
    "r",
    DImode,
    0,
    1
  },
  {
    scratch_operand,
    "=&r",
    DImode,
    0,
    0
  },
  {
    gpc_reg_operand,
    "=r",
    SImode,
    0,
    1
  },
  {
    gpc_reg_operand,
    "r",
    SImode,
    0,
    1
  },
  {
    gpc_reg_operand,
    "r",
    SImode,
    0,
    1
  },
  {
    scratch_operand,
    "=&r",
    SImode,
    0,
    0
  },
  {
    gpc_reg_operand,
    "=r",
    DImode,
    0,
    1
  },
  {
    gpc_reg_operand,
    "r",
    DImode,
    0,
    1
  },
  {
    gpc_reg_operand,
    "r",
    DImode,
    0,
    1
  },
  {
    scratch_operand,
    "=&r",
    DImode,
    0,
    0
  },
  {
    cc_reg_operand,
    "=x,?y",
    CCmode,
    0,
    1
  },
  {
    gpc_reg_operand,
    "r,r",
    SImode,
    0,
    1
  },
  {
    gpc_reg_operand,
    "r,r",
    SImode,
    0,
    1
  },
  {
    scratch_operand,
    "=&r,&r",
    SImode,
    0,
    0
  },
  {
    scratch_operand,
    "=X,&r",
    SImode,
    0,
    0
  },
  {
    cc_reg_operand,
    "=x,?y",
    CCmode,
    0,
    1
  },
  {
    gpc_reg_operand,
    "r,r",
    DImode,
    0,
    1
  },
  {
    gpc_reg_operand,
    "r,r",
    DImode,
    0,
    1
  },
  {
    scratch_operand,
    "=&r,&r",
    DImode,
    0,
    0
  },
  {
    gpc_reg_operand,
    "=r,r",
    SImode,
    0,
    1
  },
  {
    gpc_reg_operand,
    "r,r",
    SImode,
    0,
    1
  },
  {
    gpc_reg_operand,
    "r,r",
    SImode,
    0,
    1
  },
  {
    scratch_operand,
    "=&r,&r",
    SImode,
    0,
    0
  },
  {
    cc_reg_operand,
    "=x,?y",
    CCmode,
    0,
    1
  },
  {
    gpc_reg_operand,
    "=r,r",
    DImode,
    0,
    1
  },
  {
    gpc_reg_operand,
    "r,r",
    DImode,
    0,
    1
  },
  {
    gpc_reg_operand,
    "r,r",
    DImode,
    0,
    1
  },
  {
    scratch_operand,
    "=&r,&r",
    DImode,
    0,
    0
  },
  {
    cc_reg_operand,
    "=x,?y",
    CCmode,
    0,
    1
  },
  {
    gpc_reg_operand,
    "=r,r",
    SImode,
    0,
    1
  },
  {
    gpc_reg_operand,
    "r,r",
    SImode,
    0,
    1
  },
  {
    reg_or_short_operand,
    "r,O",
    SImode,
    0,
    1
  },
  {
    scratch_operand,
    "=r,X",
    SImode,
    0,
    0
  },
  {
    gpc_reg_operand,
    "=r,r,r,r",
    SImode,
    0,
    1
  },
  {
    gpc_reg_operand,
    "r,r,r,r",
    SImode,
    0,
    1
  },
  {
    reg_or_short_operand,
    "r,O,r,O",
    SImode,
    0,
    1
  },
  {
    scratch_operand,
    "=r,X,r,X",
    SImode,
    0,
    0
  },
  {
    cc_reg_operand,
    "=x,x,?y,?y",
    CCmode,
    0,
    1
  },
  {
    gpc_reg_operand,
    "=&r,&r",
    SImode,
    0,
    1
  },
  {
    gpc_reg_operand,
    "r,r",
    SImode,
    0,
    1
  },
  {
    reg_or_short_operand,
    "r,O",
    SImode,
    0,
    1
  },
  {
    gpc_reg_operand,
    "r,r",
    SImode,
    0,
    1
  },
  {
    cc_reg_operand,
    "=x,x,?y,?y",
    CCmode,
    0,
    1
  },
  {
    gpc_reg_operand,
    "r,r,r,r",
    SImode,
    0,
    1
  },
  {
    reg_or_short_operand,
    "r,O,r,O",
    SImode,
    0,
    1
  },
  {
    gpc_reg_operand,
    "r,r,r,r",
    SImode,
    0,
    1
  },
  {
    scratch_operand,
    "=&r,&r,&r,&r",
    SImode,
    0,
    0
  },
  {
    gpc_reg_operand,
    "=&r,&r,&r,&r",
    SImode,
    0,
    1
  },
  {
    gpc_reg_operand,
    "r,r,r,r",
    SImode,
    0,
    1
  },
  {
    reg_or_short_operand,
    "r,O,r,O",
    SImode,
    0,
    1
  },
  {
    gpc_reg_operand,
    "r,r,r,r",
    SImode,
    0,
    1
  },
  {
    cc_reg_operand,
    "=x,x,?y,?y",
    CCmode,
    0,
    1
  },
  {
    gpc_reg_operand,
    "=r",
    DImode,
    0,
    1
  },
  {
    gpc_reg_operand,
    "r",
    DImode,
    0,
    1
  },
  {
    reg_or_short_operand,
    "rI",
    DImode,
    0,
    1
  },
  {
    gpc_reg_operand,
    "=r,r",
    DImode,
    0,
    1
  },
  {
    gpc_reg_operand,
    "r,r",
    DImode,
    0,
    1
  },
  {
    reg_or_short_operand,
    "rI,rI",
    DImode,
    0,
    1
  },
  {
    cc_reg_operand,
    "=x,?y",
    CCmode,
    0,
    1
  },
  {
    gpc_reg_operand,
    "=&r",
    SImode,
    0,
    1
  },
  {
    gpc_reg_operand,
    "r",
    SImode,
    0,
    1
  },
  {
    reg_or_short_operand,
    "rI",
    SImode,
    0,
    1
  },
  {
    gpc_reg_operand,
    "r",
    SImode,
    0,
    1
  },
  {
    cc_reg_operand,
    "=x,?y",
    CCmode,
    0,
    1
  },
  {
    gpc_reg_operand,
    "r,r",
    SImode,
    0,
    1
  },
  {
    reg_or_short_operand,
    "rI,rI",
    SImode,
    0,
    1
  },
  {
    gpc_reg_operand,
    "r,r",
    SImode,
    0,
    1
  },
  {
    scratch_operand,
    "=&r,&r",
    SImode,
    0,
    0
  },
  {
    gpc_reg_operand,
    "=&r,&r",
    SImode,
    0,
    1
  },
  {
    gpc_reg_operand,
    "r,r",
    SImode,
    0,
    1
  },
  {
    reg_or_short_operand,
    "rI,rI",
    SImode,
    0,
    1
  },
  {
    gpc_reg_operand,
    "r,r",
    SImode,
    0,
    1
  },
  {
    cc_reg_operand,
    "=x,?y",
    CCmode,
    0,
    1
  },
  {
    gpc_reg_operand,
    "=r,r",
    SImode,
    0,
    1
  },
  {
    gpc_reg_operand,
    "r,r",
    SImode,
    0,
    1
  },
  {
    reg_or_neg_short_operand,
    "r,P",
    SImode,
    0,
    1
  },
  {
    gpc_reg_operand,
    "=r,r,r,r",
    SImode,
    0,
    1
  },
  {
    gpc_reg_operand,
    "r,r,r,r",
    SImode,
    0,
    1
  },
  {
    reg_or_neg_short_operand,
    "r,P,r,P",
    SImode,
    0,
    1
  },
  {
    cc_reg_operand,
    "=x,x,?y,?y",
    CCmode,
    0,
    1
  },
  {
    gpc_reg_operand,
    "=&r,&r",
    SImode,
    0,
    1
  },
  {
    gpc_reg_operand,
    "r,r",
    SImode,
    0,
    1
  },
  {
    reg_or_neg_short_operand,
    "r,P",
    SImode,
    0,
    1
  },
  {
    reg_or_short_operand,
    "rI,rI",
    SImode,
    0,
    1
  },
  {
    cc_reg_operand,
    "=x,x,?y,?y",
    CCmode,
    0,
    1
  },
  {
    gpc_reg_operand,
    "r,r,r,r",
    SImode,
    0,
    1
  },
  {
    reg_or_neg_short_operand,
    "r,P,r,P",
    SImode,
    0,
    1
  },
  {
    gpc_reg_operand,
    "r,r,r,r",
    SImode,
    0,
    1
  },
  {
    scratch_operand,
    "=&r,&r,&r,&r",
    SImode,
    0,
    0
  },
  {
    gpc_reg_operand,
    "=&r,&r,&r,&r",
    SImode,
    0,
    1
  },
  {
    gpc_reg_operand,
    "r,r,r,r",
    SImode,
    0,
    1
  },
  {
    reg_or_neg_short_operand,
    "r,P,r,P",
    SImode,
    0,
    1
  },
  {
    gpc_reg_operand,
    "r,r,r,r",
    SImode,
    0,
    1
  },
  {
    cc_reg_operand,
    "=x,x,?y,?y",
    CCmode,
    0,
    1
  },
  {
    gpc_reg_operand,
    "=r",
    SImode,
    0,
    1
  },
  {
    gpc_reg_operand,
    "r",
    SImode,
    0,
    1
  },
  {
    reg_or_short_operand,
    "rI",
    SImode,
    0,
    1
  },
  {
    scratch_operand,
    "=r",
    SImode,
    0,
    0
  },
  {
    gpc_reg_operand,
    "=r,r",
    SImode,
    0,
    1
  },
  {
    gpc_reg_operand,
    "r,r",
    SImode,
    0,
    1
  },
  {
    reg_or_short_operand,
    "rI,rI",
    SImode,
    0,
    1
  },
  {
    scratch_operand,
    "=r,r",
    SImode,
    0,
    0
  },
  {
    cc_reg_operand,
    "=x,?y",
    CCmode,
    0,
    1
  },
  {
    gpc_reg_operand,
    "=r,r",
    DImode,
    0,
    1
  },
  {
    gpc_reg_operand,
    "r,r",
    DImode,
    0,
    1
  },
  {
    reg_or_neg_short_operand,
    "r,P",
    DImode,
    0,
    1
  },
  {
    gpc_reg_operand,
    "=r,r,r,r",
    DImode,
    0,
    1
  },
  {
    gpc_reg_operand,
    "r,r,r,r",
    DImode,
    0,
    1
  },
  {
    reg_or_neg_short_operand,
    "r,P,r,P",
    DImode,
    0,
    1
  },
  {
    cc_reg_operand,
    "=x,x,?y,?y",
    CCmode,
    0,
    1
  },
  {
    gpc_reg_operand,
    "=&r,&r",
    SImode,
    0,
    1
  },
  {
    gpc_reg_operand,
    "r,r",
    SImode,
    0,
    1
  },
  {
    reg_or_neg_short_operand,
    "r,P",
    SImode,
    0,
    1
  },
  {
    gpc_reg_operand,
    "r,r",
    SImode,
    0,
    1
  },
  {
    gpc_reg_operand,
    "=r,r",
    SImode,
    0,
    1
  },
  {
    gpc_reg_operand,
    "r,r",
    SImode,
    0,
    1
  },
  {
    reg_or_short_operand,
    "r,I",
    SImode,
    0,
    1
  },
  {
    gpc_reg_operand,
    "=r",
    SImode,
    0,
    1
  },
  {
    gpc_reg_operand,
    "r",
    SImode,
    0,
    1
  },
  {
    reg_or_short_operand,
    "r",
    SImode,
    0,
    1
  },
  {
    gpc_reg_operand,
    "=r,r",
    SImode,
    0,
    1
  },
  {
    gpc_reg_operand,
    "r,r",
    SImode,
    0,
    1
  },
  {
    reg_or_short_operand,
    "r,r",
    SImode,
    0,
    1
  },
  {
    cc_reg_operand,
    "=x,?y",
    CCmode,
    0,
    1
  },
  {
    gpc_reg_operand,
    "=&r",
    SImode,
    0,
    1
  },
  {
    gpc_reg_operand,
    "r",
    SImode,
    0,
    1
  },
  {
    gpc_reg_operand,
    "r",
    SImode,
    0,
    1
  },
  {
    gpc_reg_operand,
    "=&r",
    DImode,
    0,
    1
  },
  {
    gpc_reg_operand,
    "r",
    DImode,
    0,
    1
  },
  {
    gpc_reg_operand,
    "r",
    DImode,
    0,
    1
  },
  {
    gpc_reg_operand,
    "=&r,&r",
    SImode,
    0,
    1
  },
  {
    gpc_reg_operand,
    "r,r",
    SImode,
    0,
    1
  },
  {
    gpc_reg_operand,
    "r,r",
    SImode,
    0,
    1
  },
  {
    cc_reg_operand,
    "=x,?y",
    CCmode,
    0,
    1
  },
  {
    gpc_reg_operand,
    "=&r,&r",
    DImode,
    0,
    1
  },
  {
    gpc_reg_operand,
    "r,r",
    DImode,
    0,
    1
  },
  {
    gpc_reg_operand,
    "r,r",
    DImode,
    0,
    1
  },
  {
    cc_reg_operand,
    "=x,?y",
    CCmode,
    0,
    1
  },
  {
    gpc_reg_operand,
    "=&r",
    SImode,
    0,
    1
  },
  {
    gpc_reg_operand,
    "r",
    SImode,
    0,
    1
  },
  {
    reg_or_short_operand,
    "r",
    SImode,
    0,
    1
  },
  {
    gpc_reg_operand,
    "r",
    SImode,
    0,
    1
  },
  {
    cc_reg_operand,
    "=x,?y",
    CCmode,
    0,
    1
  },
  {
    gpc_reg_operand,
    "r,r",
    SImode,
    0,
    1
  },
  {
    reg_or_short_operand,
    "r,r",
    SImode,
    0,
    1
  },
  {
    gpc_reg_operand,
    "r,r",
    SImode,
    0,
    1
  },
  {
    scratch_operand,
    "=&r,&r",
    SImode,
    0,
    0
  },
  {
    gpc_reg_operand,
    "=&r,&r",
    SImode,
    0,
    1
  },
  {
    gpc_reg_operand,
    "r,r",
    SImode,
    0,
    1
  },
  {
    reg_or_short_operand,
    "r,r",
    SImode,
    0,
    1
  },
  {
    gpc_reg_operand,
    "r,r",
    SImode,
    0,
    1
  },
  {
    cc_reg_operand,
    "=x,?y",
    CCmode,
    0,
    1
  },
  {
    gpc_reg_operand,
    "=&r,&r",
    SImode,
    0,
    1
  },
  {
    gpc_reg_operand,
    "r,r",
    SImode,
    0,
    1
  },
  {
    reg_or_short_operand,
    "I,rI",
    SImode,
    0,
    1
  },
  {
    reg_or_short_operand,
    "r,rI",
    SImode,
    0,
    1
  },
  {
    gpc_reg_operand,
    "=&r,&r",
    DImode,
    0,
    1
  },
  {
    gpc_reg_operand,
    "r,r",
    DImode,
    0,
    1
  },
  {
    reg_or_short_operand,
    "I,rI",
    DImode,
    0,
    1
  },
  {
    reg_or_short_operand,
    "r,rI",
    DImode,
    0,
    1
  },
  {
    cc_reg_operand,
    "=x,x,?y,?y",
    CCmode,
    0,
    1
  },
  {
    gpc_reg_operand,
    "r,r,r,r",
    SImode,
    0,
    1
  },
  {
    reg_or_short_operand,
    "I,r,I,r",
    SImode,
    0,
    1
  },
  {
    gpc_reg_operand,
    "r,r,r,r",
    SImode,
    0,
    1
  },
  {
    scratch_operand,
    "=&r,&r,&r,&r",
    SImode,
    0,
    0
  },
  {
    cc_reg_operand,
    "=x,x,?y,?y",
    CCmode,
    0,
    1
  },
  {
    gpc_reg_operand,
    "r,r,r,r",
    DImode,
    0,
    1
  },
  {
    reg_or_short_operand,
    "I,r,I,r",
    DImode,
    0,
    1
  },
  {
    gpc_reg_operand,
    "r,r,r,r",
    DImode,
    0,
    1
  },
  {
    scratch_operand,
    "=&r,&r,&r,&r",
    DImode,
    0,
    0
  },
  {
    gpc_reg_operand,
    "=&r,&r,&r,&r",
    SImode,
    0,
    1
  },
  {
    gpc_reg_operand,
    "r,r,r,r",
    SImode,
    0,
    1
  },
  {
    reg_or_short_operand,
    "I,r,I,r",
    SImode,
    0,
    1
  },
  {
    gpc_reg_operand,
    "r,r,r,r",
    SImode,
    0,
    1
  },
  {
    cc_reg_operand,
    "=x,x,?y,?y",
    CCmode,
    0,
    1
  },
  {
    gpc_reg_operand,
    "=&r,&r,&r,&r",
    DImode,
    0,
    1
  },
  {
    gpc_reg_operand,
    "r,r,r,r",
    DImode,
    0,
    1
  },
  {
    reg_or_short_operand,
    "I,r,I,r",
    DImode,
    0,
    1
  },
  {
    gpc_reg_operand,
    "r,r,r,r",
    DImode,
    0,
    1
  },
  {
    cc_reg_operand,
    "=x,x,?y,?y",
    CCmode,
    0,
    1
  },
  {
    0,
    "",
    VOIDmode,
    0,
    1
  },
  {
    branch_comparison_operator,
    "",
    VOIDmode,
    0,
    0
  },
  {
    cc_reg_operand,
    "x,?y",
    VOIDmode,
    0,
    1
  },
  {
    cc_reg_operand,
    "=y",
    CCEQmode,
    0,
    1
  },
  {
    boolean_operator,
    "",
    SImode,
    0,
    0
  },
  {
    branch_positive_comparison_operator,
    "",
    SImode,
    0,
    0
  },
  {
    cc_reg_operand,
    "y",
    VOIDmode,
    0,
    1
  },
  {
    branch_positive_comparison_operator,
    "",
    SImode,
    0,
    0
  },
  {
    cc_reg_operand,
    "y",
    VOIDmode,
    0,
    1
  },
  {
    cc_reg_operand,
    "=y",
    CCEQmode,
    0,
    1
  },
  {
    boolean_or_operator,
    "",
    SImode,
    0,
    0
  },
  {
    branch_positive_comparison_operator,
    "",
    SImode,
    0,
    0
  },
  {
    cc_reg_operand,
    "y",
    VOIDmode,
    0,
    1
  },
  {
    branch_positive_comparison_operator,
    "",
    SImode,
    0,
    0
  },
  {
    cc_reg_operand,
    "y",
    VOIDmode,
    0,
    1
  },
  {
    cc_reg_operand,
    "=y",
    CCEQmode,
    0,
    1
  },
  {
    branch_positive_comparison_operator,
    "",
    SImode,
    0,
    0
  },
  {
    cc_reg_operand,
    "y",
    VOIDmode,
    0,
    1
  },
  {
    cc_reg_operand,
    "=y",
    CCEQmode,
    0,
    1
  },
  {
    branch_comparison_operator,
    "",
    VOIDmode,
    0,
    0
  },
  {
    cc_reg_operand,
    "y",
    VOIDmode,
    0,
    1
  },
  {
    branch_comparison_operator,
    "",
    VOIDmode,
    0,
    0
  },
  {
    cc_reg_operand,
    "y",
    VOIDmode,
    0,
    1
  },
  {
    register_operand,
    "c,*l",
    SImode,
    0,
    1
  },
  {
    register_operand,
    "c,*l",
    DImode,
    0,
    1
  },
  {
    register_operand,
    "c,*l",
    SImode,
    0,
    1
  },
  {
    0,
    "",
    VOIDmode,
    0,
    1
  },
  {
    register_operand,
    "c,*l",
    DImode,
    0,
    1
  },
  {
    0,
    "",
    VOIDmode,
    0,
    1
  },
  {
    register_operand,
    "c,*r,*r",
    SImode,
    0,
    1
  },
  {
    register_operand,
    "=1,*r,m*q*c*l",
    SImode,
    0,
    1
  },
  {
    scratch_operand,
    "=X,&x,&x",
    CCmode,
    0,
    0
  },
  {
    scratch_operand,
    "=X,X,r",
    SImode,
    0,
    0
  },
  {
    0,
    "",
    VOIDmode,
    0,
    1
  },
  {
    register_operand,
    "c,*r,*r",
    DImode,
    0,
    1
  },
  {
    register_operand,
    "=1,*r,m*c*l",
    DImode,
    0,
    1
  },
  {
    scratch_operand,
    "=X,&x,&x",
    CCmode,
    0,
    0
  },
  {
    scratch_operand,
    "=X,X,r",
    DImode,
    0,
    0
  },
  {
    0,
    "",
    VOIDmode,
    0,
    1
  },
  {
    register_operand,
    "c,*r,*r",
    SImode,
    0,
    1
  },
  {
    register_operand,
    "=1,*r,m*q*c*l",
    SImode,
    0,
    1
  },
  {
    scratch_operand,
    "=X,&x,&X",
    CCmode,
    0,
    0
  },
  {
    scratch_operand,
    "=X,X,r",
    SImode,
    0,
    0
  },
  {
    trap_comparison_operator,
    "",
    VOIDmode,
    0,
    0
  },
  {
    register_operand,
    "r",
    SImode,
    0,
    1
  },
  {
    reg_or_short_operand,
    "rI",
    SImode,
    0,
    1
  },
  {
    trap_comparison_operator,
    "",
    VOIDmode,
    0,
    0
  },
  {
    register_operand,
    "r",
    DImode,
    0,
    1
  },
  {
    reg_or_short_operand,
    "rI",
    DImode,
    0,
    1
  },
  {
    stmw_operation,
    "",
    VOIDmode,
    0,
    0
  },
  {
    memory_operand,
    "=m",
    SImode,
    0,
    1
  },
  {
    gpc_reg_operand,
    "r",
    SImode,
    0,
    1
  },
  {
    any_operand,
    "",
    VOIDmode,
    0,
    0
  },
  {
    register_operand,
    "=l",
    SImode,
    0,
    1
  },
  {
    call_operand,
    "s",
    SImode,
    0,
    1
  },
  {
    memory_operand,
    "=m",
    DFmode,
    0,
    1
  },
  {
    gpc_reg_operand,
    "f",
    DFmode,
    0,
    1
  },
  {
    any_operand,
    "",
    VOIDmode,
    0,
    0
  },
  {
    register_operand,
    "=l",
    DImode,
    0,
    1
  },
  {
    call_operand,
    "s",
    DImode,
    0,
    1
  },
  {
    memory_operand,
    "=m",
    DFmode,
    0,
    1
  },
  {
    gpc_reg_operand,
    "f",
    DFmode,
    0,
    1
  },
  {
    memory_operand,
    "+m",
    BLKmode,
    0,
    1
  },
  {
    mtcrf_operation,
    "",
    VOIDmode,
    0,
    0
  },
  {
    cc_reg_operand,
    "=y",
    CCmode,
    0,
    1
  },
  {
    gpc_reg_operand,
    "r",
    SImode,
    0,
    1
  },
  {
    immediate_operand,
    "n",
    VOIDmode,
    0,
    1
  },
  {
    lmw_operation,
    "",
    VOIDmode,
    0,
    0
  },
  {
    gpc_reg_operand,
    "=r",
    SImode,
    0,
    1
  },
  {
    memory_operand,
    "m",
    SImode,
    0,
    1
  },
  {
    register_operand,
    "lc",
    SImode,
    0,
    1
  },
  {
    register_operand,
    "lc",
    DImode,
    0,
    1
  },
  {
    any_operand,
    "",
    VOIDmode,
    0,
    0
  },
  {
    register_operand,
    "l",
    SImode,
    0,
    1
  },
  {
    call_operand,
    "s",
    SImode,
    0,
    1
  },
  {
    gpc_reg_operand,
    "=f",
    DFmode,
    0,
    1
  },
  {
    memory_operand,
    "m",
    DFmode,
    0,
    1
  },
  {
    any_operand,
    "",
    VOIDmode,
    0,
    0
  },
  {
    register_operand,
    "l",
    DImode,
    0,
    1
  },
  {
    call_operand,
    "s",
    DImode,
    0,
    1
  },
  {
    gpc_reg_operand,
    "=f",
    DFmode,
    0,
    1
  },
  {
    memory_operand,
    "m",
    DFmode,
    0,
    1
  },
  {
    register_operand,
    "r",
    SImode,
    0,
    1
  },
  {
    scratch_operand,
    "=&b",
    SImode,
    0,
    0
  },
  {
    register_operand,
    "r",
    DImode,
    0,
    1
  },
  {
    scratch_operand,
    "=&b",
    DImode,
    0,
    0
  },
  {
    address_operand,
    "p",
    V4SImode,
    0,
    1
  },
  {
    const_int_operand,
    "n",
    SImode,
    0,
    1
  },
  {
    const_int_operand,
    "n",
    SImode,
    0,
    1
  },
  {
    altivec_register_operand,
    "=v",
    V4SImode,
    0,
    1
  },
  {
    memory_operand,
    "m",
    V4SImode,
    0,
    1
  },
  {
    altivec_register_operand,
    "=v",
    V8HImode,
    0,
    1
  },
  {
    memory_operand,
    "m",
    V8HImode,
    0,
    1
  },
  {
    altivec_register_operand,
    "=v",
    V16QImode,
    0,
    1
  },
  {
    memory_operand,
    "m",
    V16QImode,
    0,
    1
  },
  {
    altivec_register_operand,
    "=v",
    V4SFmode,
    0,
    1
  },
  {
    memory_operand,
    "m",
    V4SFmode,
    0,
    1
  },
  {
    memory_operand,
    "=m",
    V4SImode,
    0,
    1
  },
  {
    altivec_register_operand,
    "v",
    V4SImode,
    0,
    1
  },
  {
    memory_operand,
    "=m",
    V8HImode,
    0,
    1
  },
  {
    altivec_register_operand,
    "v",
    V8HImode,
    0,
    1
  },
  {
    memory_operand,
    "=m",
    V16QImode,
    0,
    1
  },
  {
    altivec_register_operand,
    "v",
    V16QImode,
    0,
    1
  },
  {
    memory_operand,
    "=m",
    V4SFmode,
    0,
    1
  },
  {
    altivec_register_operand,
    "v",
    V4SFmode,
    0,
    1
  },
  {
    nonimmediate_operand,
    "=m,v,v,o,r,r",
    V4SImode,
    0,
    1
  },
  {
    input_operand,
    "v,m,v,r,o,r",
    V4SImode,
    0,
    1
  },
  {
    nonimmediate_operand,
    "=m,v,v,o,r,r",
    V8HImode,
    0,
    1
  },
  {
    input_operand,
    "v,m,v,r,o,r",
    V8HImode,
    0,
    1
  },
  {
    nonimmediate_operand,
    "=m,v,v,o,r,r",
    V16QImode,
    0,
    1
  },
  {
    input_operand,
    "v,m,v,r,o,r",
    V16QImode,
    0,
    1
  },
  {
    nonimmediate_operand,
    "=m,v,v,o,r,r",
    V4SFmode,
    0,
    1
  },
  {
    input_operand,
    "v,m,v,r,o,r",
    V4SFmode,
    0,
    1
  },
  {
    register_operand,
    "=r",
    SImode,
    0,
    1
  },
  {
    vrsave_operation,
    "",
    VOIDmode,
    0,
    0
  },
  {
    register_operand,
    "r",
    SImode,
    0,
    1
  },
  {
    altivec_register_operand,
    "=v",
    V4SImode,
    0,
    1
  },
  {
    zero_constant,
    "",
    V4SImode,
    0,
    1
  },
  {
    altivec_register_operand,
    "=v",
    V4SFmode,
    0,
    1
  },
  {
    zero_constant,
    "",
    V4SFmode,
    0,
    1
  },
  {
    altivec_register_operand,
    "=v",
    V8HImode,
    0,
    1
  },
  {
    zero_constant,
    "",
    V8HImode,
    0,
    1
  },
  {
    altivec_register_operand,
    "=v",
    V16QImode,
    0,
    1
  },
  {
    zero_constant,
    "",
    V16QImode,
    0,
    1
  },
  {
    register_operand,
    "=v",
    V16QImode,
    0,
    1
  },
  {
    register_operand,
    "v",
    V16QImode,
    0,
    1
  },
  {
    register_operand,
    "v",
    V16QImode,
    0,
    1
  },
  {
    register_operand,
    "=v",
    V8HImode,
    0,
    1
  },
  {
    register_operand,
    "v",
    V8HImode,
    0,
    1
  },
  {
    register_operand,
    "v",
    V8HImode,
    0,
    1
  },
  {
    register_operand,
    "=v",
    V4SImode,
    0,
    1
  },
  {
    register_operand,
    "v",
    V4SImode,
    0,
    1
  },
  {
    register_operand,
    "v",
    V4SImode,
    0,
    1
  },
  {
    register_operand,
    "=v",
    V4SFmode,
    0,
    1
  },
  {
    register_operand,
    "v",
    V4SFmode,
    0,
    1
  },
  {
    register_operand,
    "v",
    V4SFmode,
    0,
    1
  },
  {
    register_operand,
    "=v",
    V4SImode,
    0,
    1
  },
  {
    register_operand,
    "v",
    V4SFmode,
    0,
    1
  },
  {
    register_operand,
    "v",
    V4SFmode,
    0,
    1
  },
  {
    register_operand,
    "=v",
    V4SFmode,
    0,
    1
  },
  {
    register_operand,
    "v",
    V4SFmode,
    0,
    1
  },
  {
    register_operand,
    "v",
    V4SFmode,
    0,
    1
  },
  {
    register_operand,
    "v",
    V4SFmode,
    0,
    1
  },
  {
    register_operand,
    "=v",
    V4SImode,
    0,
    1
  },
  {
    register_operand,
    "v",
    V16QImode,
    0,
    1
  },
  {
    register_operand,
    "v",
    V16QImode,
    0,
    1
  },
  {
    register_operand,
    "v",
    V4SImode,
    0,
    1
  },
  {
    register_operand,
    "=v",
    V4SImode,
    0,
    1
  },
  {
    register_operand,
    "v",
    V8HImode,
    0,
    1
  },
  {
    register_operand,
    "v",
    V8HImode,
    0,
    1
  },
  {
    register_operand,
    "v",
    V4SImode,
    0,
    1
  },
  {
    register_operand,
    "=v",
    V8HImode,
    0,
    1
  },
  {
    register_operand,
    "v",
    V8HImode,
    0,
    1
  },
  {
    register_operand,
    "v",
    V8HImode,
    0,
    1
  },
  {
    register_operand,
    "v",
    V8HImode,
    0,
    1
  },
  {
    register_operand,
    "=v",
    V8HImode,
    0,
    1
  },
  {
    register_operand,
    "v",
    V16QImode,
    0,
    1
  },
  {
    register_operand,
    "v",
    V16QImode,
    0,
    1
  },
  {
    register_operand,
    "=v",
    V16QImode,
    0,
    1
  },
  {
    register_operand,
    "v",
    V8HImode,
    0,
    1
  },
  {
    register_operand,
    "v",
    V8HImode,
    0,
    1
  },
  {
    register_operand,
    "=v",
    V8HImode,
    0,
    1
  },
  {
    register_operand,
    "v",
    V4SImode,
    0,
    1
  },
  {
    register_operand,
    "v",
    V4SImode,
    0,
    1
  },
  {
    register_operand,
    "=v",
    V4SImode,
    0,
    1
  },
  {
    register_operand,
    "v",
    V16QImode,
    0,
    1
  },
  {
    register_operand,
    "v",
    V4SImode,
    0,
    1
  },
  {
    register_operand,
    "=v",
    V4SImode,
    0,
    1
  },
  {
    register_operand,
    "v",
    V8HImode,
    0,
    1
  },
  {
    register_operand,
    "v",
    V4SImode,
    0,
    1
  },
  {
    register_operand,
    "=v",
    V16QImode,
    0,
    1
  },
  {
    register_operand,
    "v",
    V16QImode,
    0,
    1
  },
  {
    immediate_operand,
    "i",
    QImode,
    0,
    1
  },
  {
    register_operand,
    "=v",
    V8HImode,
    0,
    1
  },
  {
    register_operand,
    "v",
    V8HImode,
    0,
    1
  },
  {
    immediate_operand,
    "i",
    QImode,
    0,
    1
  },
  {
    register_operand,
    "=v",
    V4SImode,
    0,
    1
  },
  {
    register_operand,
    "v",
    V4SImode,
    0,
    1
  },
  {
    immediate_operand,
    "i",
    QImode,
    0,
    1
  },
  {
    register_operand,
    "=v",
    V16QImode,
    0,
    1
  },
  {
    immediate_operand,
    "i",
    QImode,
    0,
    1
  },
  {
    register_operand,
    "=v",
    V8HImode,
    0,
    1
  },
  {
    immediate_operand,
    "i",
    QImode,
    0,
    1
  },
  {
    register_operand,
    "=v",
    V4SImode,
    0,
    1
  },
  {
    immediate_operand,
    "i",
    QImode,
    0,
    1
  },
  {
    register_operand,
    "=v",
    V4SFmode,
    0,
    1
  },
  {
    immediate_operand,
    "i",
    QImode,
    0,
    1
  },
  {
    register_operand,
    "=v",
    V4SImode,
    0,
    1
  },
  {
    register_operand,
    "v",
    V4SImode,
    0,
    1
  },
  {
    register_operand,
    "v",
    V4SImode,
    0,
    1
  },
  {
    register_operand,
    "v",
    V16QImode,
    0,
    1
  },
  {
    register_operand,
    "=v",
    V4SFmode,
    0,
    1
  },
  {
    register_operand,
    "v",
    V4SFmode,
    0,
    1
  },
  {
    register_operand,
    "v",
    V4SFmode,
    0,
    1
  },
  {
    register_operand,
    "v",
    V16QImode,
    0,
    1
  },
  {
    register_operand,
    "=v",
    V8HImode,
    0,
    1
  },
  {
    register_operand,
    "v",
    V8HImode,
    0,
    1
  },
  {
    register_operand,
    "v",
    V8HImode,
    0,
    1
  },
  {
    register_operand,
    "v",
    V16QImode,
    0,
    1
  },
  {
    register_operand,
    "=v",
    V16QImode,
    0,
    1
  },
  {
    register_operand,
    "v",
    V16QImode,
    0,
    1
  },
  {
    register_operand,
    "v",
    V16QImode,
    0,
    1
  },
  {
    register_operand,
    "v",
    V16QImode,
    0,
    1
  },
  {
    register_operand,
    "=v",
    V4SFmode,
    0,
    1
  },
  {
    register_operand,
    "v",
    V4SImode,
    0,
    1
  },
  {
    immediate_operand,
    "i",
    QImode,
    0,
    1
  },
  {
    register_operand,
    "=v",
    V4SImode,
    0,
    1
  },
  {
    register_operand,
    "v",
    V4SFmode,
    0,
    1
  },
  {
    immediate_operand,
    "i",
    QImode,
    0,
    1
  },
  {
    register_operand,
    "=v",
    V4SImode,
    0,
    1
  },
  {
    register_operand,
    "v",
    V4SImode,
    0,
    1
  },
  {
    register_operand,
    "v",
    V4SImode,
    0,
    1
  },
  {
    register_operand,
    "v",
    V4SImode,
    0,
    1
  },
  {
    register_operand,
    "=v",
    V4SFmode,
    0,
    1
  },
  {
    register_operand,
    "v",
    V4SFmode,
    0,
    1
  },
  {
    register_operand,
    "v",
    V4SFmode,
    0,
    1
  },
  {
    register_operand,
    "v",
    V4SImode,
    0,
    1
  },
  {
    register_operand,
    "=v",
    V4SImode,
    0,
    1
  },
  {
    register_operand,
    "v",
    V4SImode,
    0,
    1
  },
  {
    register_operand,
    "v",
    V4SImode,
    0,
    1
  },
  {
    immediate_operand,
    "i",
    QImode,
    0,
    1
  },
  {
    register_operand,
    "=v",
    V4SFmode,
    0,
    1
  },
  {
    register_operand,
    "v",
    V4SFmode,
    0,
    1
  },
  {
    register_operand,
    "v",
    V4SFmode,
    0,
    1
  },
  {
    immediate_operand,
    "i",
    QImode,
    0,
    1
  },
  {
    register_operand,
    "=v",
    V8HImode,
    0,
    1
  },
  {
    register_operand,
    "v",
    V8HImode,
    0,
    1
  },
  {
    register_operand,
    "v",
    V8HImode,
    0,
    1
  },
  {
    immediate_operand,
    "i",
    QImode,
    0,
    1
  },
  {
    register_operand,
    "=v",
    V16QImode,
    0,
    1
  },
  {
    register_operand,
    "v",
    V16QImode,
    0,
    1
  },
  {
    register_operand,
    "v",
    V16QImode,
    0,
    1
  },
  {
    immediate_operand,
    "i",
    QImode,
    0,
    1
  },
  {
    scratch_operand,
    "=v",
    V4SImode,
    0,
    0
  },
  {
    register_operand,
    "v",
    V4SImode,
    0,
    1
  },
  {
    register_operand,
    "v",
    V4SImode,
    0,
    1
  },
  {
    any_operand,
    "",
    VOIDmode,
    0,
    1
  },
  {
    scratch_operand,
    "=v",
    V4SFmode,
    0,
    0
  },
  {
    register_operand,
    "v",
    V4SFmode,
    0,
    1
  },
  {
    register_operand,
    "v",
    V4SFmode,
    0,
    1
  },
  {
    any_operand,
    "",
    VOIDmode,
    0,
    1
  },
  {
    scratch_operand,
    "=v",
    V8HImode,
    0,
    0
  },
  {
    register_operand,
    "v",
    V8HImode,
    0,
    1
  },
  {
    register_operand,
    "v",
    V8HImode,
    0,
    1
  },
  {
    any_operand,
    "",
    VOIDmode,
    0,
    1
  },
  {
    scratch_operand,
    "=v",
    V16QImode,
    0,
    0
  },
  {
    register_operand,
    "v",
    V16QImode,
    0,
    1
  },
  {
    register_operand,
    "v",
    V16QImode,
    0,
    1
  },
  {
    any_operand,
    "",
    VOIDmode,
    0,
    1
  },
  {
    register_operand,
    "b",
    SImode,
    0,
    1
  },
  {
    register_operand,
    "r",
    SImode,
    0,
    1
  },
  {
    immediate_operand,
    "i",
    QImode,
    0,
    1
  },
  {
    register_operand,
    "=v",
    V16QImode,
    0,
    1
  },
  {
    register_operand,
    "b",
    SImode,
    0,
    1
  },
  {
    register_operand,
    "r",
    SImode,
    0,
    1
  },
  {
    register_operand,
    "=v",
    V8HImode,
    0,
    1
  },
  {
    register_operand,
    "b",
    SImode,
    0,
    1
  },
  {
    register_operand,
    "r",
    SImode,
    0,
    1
  },
  {
    register_operand,
    "=v",
    V4SImode,
    0,
    1
  },
  {
    register_operand,
    "b",
    SImode,
    0,
    1
  },
  {
    register_operand,
    "r",
    SImode,
    0,
    1
  },
  {
    register_operand,
    "v",
    V4SImode,
    0,
    1
  },
  {
    register_operand,
    "b",
    SImode,
    0,
    1
  },
  {
    register_operand,
    "r",
    SImode,
    0,
    1
  },
  {
    register_operand,
    "v",
    V16QImode,
    0,
    1
  },
  {
    register_operand,
    "b",
    SImode,
    0,
    1
  },
  {
    register_operand,
    "r",
    SImode,
    0,
    1
  },
  {
    register_operand,
    "v",
    V8HImode,
    0,
    1
  },
  {
    register_operand,
    "=v",
    V16QImode,
    0,
    1
  },
  {
    register_operand,
    "v",
    V16QImode,
    0,
    1
  },
  {
    scratch_operand,
    "=&v",
    V16QImode,
    0,
    0
  },
  {
    scratch_operand,
    "=&v",
    V16QImode,
    0,
    0
  },
  {
    register_operand,
    "=v",
    V8HImode,
    0,
    1
  },
  {
    register_operand,
    "v",
    V8HImode,
    0,
    1
  },
  {
    scratch_operand,
    "=&v",
    V8HImode,
    0,
    0
  },
  {
    scratch_operand,
    "=&v",
    V8HImode,
    0,
    0
  },
  {
    register_operand,
    "=v",
    V4SImode,
    0,
    1
  },
  {
    register_operand,
    "v",
    V4SImode,
    0,
    1
  },
  {
    scratch_operand,
    "=&v",
    V4SImode,
    0,
    0
  },
  {
    scratch_operand,
    "=&v",
    V4SImode,
    0,
    0
  },
  {
    register_operand,
    "=v",
    V4SFmode,
    0,
    1
  },
  {
    register_operand,
    "v",
    V4SFmode,
    0,
    1
  },
  {
    scratch_operand,
    "=&v",
    V4SFmode,
    0,
    0
  },
  {
    scratch_operand,
    "=&v",
    V4SFmode,
    0,
    0
  },
  {
    gpc_reg_operand,
    "",
    DImode,
    0,
    1
  },
  {
    gpc_reg_operand,
    "",
    QImode,
    0,
    1
  },
  {
    cc_reg_not_cr0_operand,
    "",
    CCmode,
    0,
    1
  },
  {
    gpc_reg_operand,
    "",
    QImode,
    0,
    1
  },
  {
    scratch_operand,
    "",
    DImode,
    0,
    0
  },
  {
    gpc_reg_operand,
    "",
    DImode,
    0,
    1
  },
  {
    gpc_reg_operand,
    "",
    HImode,
    0,
    1
  },
  {
    cc_reg_not_cr0_operand,
    "",
    CCmode,
    0,
    1
  },
  {
    gpc_reg_operand,
    "",
    HImode,
    0,
    1
  },
  {
    scratch_operand,
    "",
    DImode,
    0,
    0
  },
  {
    gpc_reg_operand,
    "",
    DImode,
    0,
    1
  },
  {
    gpc_reg_operand,
    "",
    SImode,
    0,
    1
  },
  {
    cc_reg_not_cr0_operand,
    "",
    CCmode,
    0,
    1
  },
  {
    gpc_reg_operand,
    "",
    SImode,
    0,
    1
  },
  {
    scratch_operand,
    "",
    DImode,
    0,
    0
  },
  {
    gpc_reg_operand,
    "",
    SImode,
    0,
    1
  },
  {
    gpc_reg_operand,
    "",
    QImode,
    0,
    1
  },
  {
    cc_reg_not_cr0_operand,
    "",
    CCmode,
    0,
    1
  },
  {
    gpc_reg_operand,
    "",
    QImode,
    0,
    1
  },
  {
    scratch_operand,
    "",
    SImode,
    0,
    0
  },
  {
    gpc_reg_operand,
    "",
    HImode,
    0,
    1
  },
  {
    gpc_reg_operand,
    "",
    QImode,
    0,
    1
  },
  {
    cc_reg_not_cr0_operand,
    "",
    CCmode,
    0,
    1
  },
  {
    gpc_reg_operand,
    "",
    QImode,
    0,
    1
  },
  {
    scratch_operand,
    "",
    HImode,
    0,
    0
  },
  {
    gpc_reg_operand,
    "",
    SImode,
    0,
    1
  },
  {
    gpc_reg_operand,
    "",
    HImode,
    0,
    1
  },
  {
    cc_reg_not_cr0_operand,
    "",
    CCmode,
    0,
    1
  },
  {
    gpc_reg_operand,
    "",
    HImode,
    0,
    1
  },
  {
    scratch_operand,
    "",
    SImode,
    0,
    0
  },
  {
    gpc_reg_operand,
    "",
    SImode,
    0,
    1
  },
  {
    gpc_reg_operand,
    "",
    SImode,
    0,
    1
  },
  {
    reg_or_arith_cint_operand,
    "",
    SImode,
    0,
    1
  },
  {
    cc_reg_not_cr0_operand,
    "",
    CCmode,
    0,
    1
  },
  {
    gpc_reg_operand,
    "",
    SImode,
    0,
    1
  },
  {
    reg_or_short_operand,
    "",
    SImode,
    0,
    1
  },
  {
    scratch_operand,
    "",
    SImode,
    0,
    0
  },
  {
    gpc_reg_operand,
    "",
    SImode,
    0,
    1
  },
  {
    gpc_reg_operand,
    "",
    SImode,
    0,
    1
  },
  {
    reg_or_short_operand,
    "",
    SImode,
    0,
    1
  },
  {
    cc_reg_not_cr0_operand,
    "",
    CCmode,
    0,
    1
  },
  {
    gpc_reg_operand,
    "",
    SImode,
    0,
    1
  },
  {
    gpc_reg_operand,
    "",
    SImode,
    0,
    1
  },
  {
    non_add_cint_operand,
    "",
    SImode,
    0,
    1
  },
  {
    cc_reg_not_cr0_operand,
    "",
    CCmode,
    0,
    1
  },
  {
    gpc_reg_operand,
    "",
    SImode,
    0,
    1
  },
  {
    scratch_operand,
    "",
    SImode,
    0,
    0
  },
  {
    gpc_reg_operand,
    "",
    SImode,
    0,
    1
  },
  {
    gpc_reg_operand,
    "",
    SImode,
    0,
    1
  },
  {
    cc_reg_not_cr0_operand,
    "",
    CCmode,
    0,
    1
  },
  {
    gpc_reg_operand,
    "",
    SImode,
    0,
    1
  },
  {
    gpc_reg_operand,
    "",
    SImode,
    0,
    1
  },
  {
    scratch_operand,
    "",
    SImode,
    0,
    0
  },
  {
    gpc_reg_operand,
    "",
    SImode,
    0,
    1
  },
  {
    gpc_reg_operand,
    "",
    SImode,
    0,
    1
  },
  {
    gpc_reg_operand,
    "",
    SImode,
    0,
    1
  },
  {
    cc_reg_not_cr0_operand,
    "",
    CCmode,
    0,
    1
  },
  {
    gpc_reg_operand,
    "",
    SImode,
    0,
    1
  },
  {
    reg_or_short_operand,
    "",
    SImode,
    0,
    1
  },
  {
    reg_or_arith_cint_operand,
    "",
    SImode,
    0,
    1
  },
  {
    gpc_reg_operand,
    "",
    SImode,
    0,
    1
  },
  {
    gpc_reg_operand,
    "",
    SImode,
    0,
    1
  },
  {
    reg_or_short_operand,
    "",
    SImode,
    0,
    1
  },
  {
    gpc_reg_operand,
    "",
    SImode,
    0,
    1
  },
  {
    cc_reg_not_cr0_operand,
    "",
    CCmode,
    0,
    1
  },
  {
    gpc_reg_operand,
    "",
    SImode,
    0,
    1
  },
  {
    gpc_reg_operand,
    "",
    SImode,
    0,
    1
  },
  {
    scratch_operand,
    "",
    SImode,
    0,
    0
  },
  {
    scratch_operand,
    "",
    SImode,
    0,
    0
  },
  {
    gpc_reg_operand,
    "",
    SImode,
    0,
    1
  },
  {
    gpc_reg_operand,
    "",
    SImode,
    0,
    1
  },
  {
    gpc_reg_operand,
    "",
    SImode,
    0,
    1
  },
  {
    cc_reg_not_cr0_operand,
    "",
    CCmode,
    0,
    1
  },
  {
    scratch_operand,
    "",
    SImode,
    0,
    0
  },
  {
    gpc_reg_operand,
    "",
    SImode,
    0,
    1
  },
  {
    gpc_reg_operand,
    "",
    SImode,
    0,
    1
  },
  {
    reg_or_cint_operand,
    "",
    SImode,
    0,
    1
  },
  {
    cc_reg_not_cr0_operand,
    "",
    CCmode,
    0,
    1
  },
  {
    gpc_reg_operand,
    "",
    SImode,
    0,
    1
  },
  {
    exact_log2_cint_operand,
    "",
    SImode,
    0,
    1
  },
  {
    scratch_operand,
    "",
    SImode,
    0,
    0
  },
  {
    gpc_reg_operand,
    "",
    SImode,
    0,
    1
  },
  {
    gpc_reg_operand,
    "",
    SImode,
    0,
    1
  },
  {
    exact_log2_cint_operand,
    "",
    SImode,
    0,
    1
  },
  {
    cc_reg_not_cr0_operand,
    "",
    CCmode,
    0,
    1
  },
  {
    0,
    "",
    SImode,
    0,
    1
  },
  {
    0,
    "",
    SImode,
    0,
    1
  },
  {
    0,
    "",
    SImode,
    0,
    1
  },
  {
    0,
    "",
    SImode,
    0,
    1
  },
  {
    0,
    "",
    SImode,
    0,
    1
  },
  {
    gpc_reg_operand,
    "",
    SImode,
    0,
    1
  },
  {
    gpc_reg_operand,
    "",
    SImode,
    0,
    1
  },
  {
    reg_or_cint_operand,
    "",
    SImode,
    0,
    1
  },
  {
    gpc_reg_operand,
    "",
    SImode,
    0,
    1
  },
  {
    cc_reg_not_cr0_operand,
    "",
    CCmode,
    0,
    1
  },
  {
    gpc_reg_operand,
    "",
    SImode,
    0,
    1
  },
  {
    and_operand,
    "",
    SImode,
    0,
    1
  },
  {
    scratch_operand,
    "",
    SImode,
    0,
    0
  },
  {
    scratch_operand,
    "",
    CCmode,
    0,
    0
  },
  {
    cc_reg_operand,
    "",
    CCmode,
    0,
    1
  },
  {
    gpc_reg_operand,
    "",
    SImode,
    0,
    1
  },
  {
    gpc_reg_operand,
    "",
    SImode,
    0,
    1
  },
  {
    scratch_operand,
    "",
    SImode,
    0,
    0
  },
  {
    scratch_operand,
    "",
    CCmode,
    0,
    0
  },
  {
    gpc_reg_operand,
    "",
    SImode,
    0,
    1
  },
  {
    gpc_reg_operand,
    "",
    SImode,
    0,
    1
  },
  {
    and_operand,
    "",
    SImode,
    0,
    1
  },
  {
    cc_reg_not_cr0_operand,
    "",
    CCmode,
    0,
    1
  },
  {
    scratch_operand,
    "",
    CCmode,
    0,
    0
  },
  {
    gpc_reg_operand,
    "",
    SImode,
    0,
    1
  },
  {
    gpc_reg_operand,
    "",
    SImode,
    0,
    1
  },
  {
    gpc_reg_operand,
    "",
    SImode,
    0,
    1
  },
  {
    cc_reg_operand,
    "",
    CCmode,
    0,
    1
  },
  {
    scratch_operand,
    "",
    CCmode,
    0,
    0
  },
  {
    gpc_reg_operand,
    "",
    SImode,
    0,
    1
  },
  {
    gpc_reg_operand,
    "",
    SImode,
    0,
    1
  },
  {
    mask_operand_wrap,
    "",
    SImode,
    0,
    1
  },
  {
    cc_reg_operand,
    "",
    CCmode,
    0,
    1
  },
  {
    scratch_operand,
    "",
    SImode,
    0,
    0
  },
  {
    gpc_reg_operand,
    "",
    SImode,
    0,
    1
  },
  {
    gpc_reg_operand,
    "",
    SImode,
    0,
    1
  },
  {
    reg_or_logical_cint_operand,
    "",
    SImode,
    0,
    1
  },
  {
    cc_reg_not_cr0_operand,
    "",
    CCmode,
    0,
    1
  },
  {
    gpc_reg_operand,
    "",
    SImode,
    0,
    1
  },
  {
    gpc_reg_operand,
    "",
    SImode,
    0,
    1
  },
  {
    scratch_operand,
    "",
    SImode,
    0,
    0
  },
  {
    boolean_operator,
    "",
    SImode,
    0,
    0
  },
  {
    gpc_reg_operand,
    "",
    SImode,
    0,
    1
  },
  {
    gpc_reg_operand,
    "",
    SImode,
    0,
    1
  },
  {
    gpc_reg_operand,
    "",
    SImode,
    0,
    1
  },
  {
    cc_reg_not_cr0_operand,
    "",
    CCmode,
    0,
    1
  },
  {
    boolean_operator,
    "",
    SImode,
    0,
    0
  },
  {
    gpc_reg_operand,
    "",
    SImode,
    0,
    1
  },
  {
    gpc_reg_operand,
    "",
    SImode,
    0,
    1
  },
  {
    non_logical_cint_operand,
    "",
    SImode,
    0,
    1
  },
  {
    boolean_or_operator,
    "",
    SImode,
    0,
    0
  },
  {
    gpc_reg_operand,
    "",
    SImode,
    0,
    1
  },
  {
    gpc_reg_operand,
    "",
    SImode,
    0,
    1
  },
  {
    gpc_reg_operand,
    "",
    SImode,
    0,
    1
  },
  {
    gpc_reg_operand,
    "",
    SImode,
    0,
    1
  },
  {
    cc_reg_not_cr0_operand,
    "",
    CCmode,
    0,
    1
  },
  {
    gpc_reg_operand,
    "",
    VOIDmode,
    0,
    1
  },
  {
    const_int_operand,
    "",
    SImode,
    0,
    1
  },
  {
    const_int_operand,
    "",
    SImode,
    0,
    1
  },
  {
    gpc_reg_operand,
    "",
    VOIDmode,
    0,
    1
  },
  {
    gpc_reg_operand,
    "",
    VOIDmode,
    0,
    1
  },
  {
    const_int_operand,
    "",
    SImode,
    0,
    1
  },
  {
    const_int_operand,
    "",
    SImode,
    0,
    1
  },
  {
    cc_reg_not_cr0_operand,
    "",
    CCmode,
    0,
    1
  },
  {
    gpc_reg_operand,
    "",
    SImode,
    0,
    1
  },
  {
    const_int_operand,
    "",
    SImode,
    0,
    1
  },
  {
    const_int_operand,
    "",
    SImode,
    0,
    1
  },
  {
    scratch_operand,
    "",
    SImode,
    0,
    0
  },
  {
    gpc_reg_operand,
    "",
    SImode,
    0,
    1
  },
  {
    gpc_reg_operand,
    "",
    SImode,
    0,
    1
  },
  {
    const_int_operand,
    "",
    SImode,
    0,
    1
  },
  {
    const_int_operand,
    "",
    SImode,
    0,
    1
  },
  {
    cc_reg_not_cr0_operand,
    "",
    CCmode,
    0,
    1
  },
  {
    gpc_reg_operand,
    "",
    SImode,
    0,
    1
  },
  {
    reg_or_cint_operand,
    "",
    SImode,
    0,
    1
  },
  {
    scratch_operand,
    "",
    SImode,
    0,
    0
  },
  {
    cc_reg_not_cr0_operand,
    "",
    CCmode,
    0,
    1
  },
  {
    gpc_reg_operand,
    "",
    SImode,
    0,
    1
  },
  {
    reg_or_cint_operand,
    "",
    SImode,
    0,
    1
  },
  {
    mask_operand,
    "",
    SImode,
    0,
    1
  },
  {
    scratch_operand,
    "",
    SImode,
    0,
    0
  },
  {
    gpc_reg_operand,
    "",
    SImode,
    0,
    1
  },
  {
    gpc_reg_operand,
    "",
    SImode,
    0,
    1
  },
  {
    reg_or_cint_operand,
    "",
    SImode,
    0,
    1
  },
  {
    mask_operand,
    "",
    SImode,
    0,
    1
  },
  {
    cc_reg_not_cr0_operand,
    "",
    CCmode,
    0,
    1
  },
  {
    gpc_reg_operand,
    "",
    SImode,
    0,
    1
  },
  {
    reg_or_cint_operand,
    "",
    SImode,
    0,
    1
  },
  {
    scratch_operand,
    "",
    SImode,
    0,
    0
  },
  {
    scratch_operand,
    "",
    SImode,
    0,
    0
  },
  {
    gpc_reg_operand,
    "",
    SImode,
    0,
    1
  },
  {
    gpc_reg_operand,
    "",
    SImode,
    0,
    1
  },
  {
    reg_or_cint_operand,
    "",
    SImode,
    0,
    1
  },
  {
    cc_reg_not_cr0_operand,
    "",
    CCmode,
    0,
    1
  },
  {
    scratch_operand,
    "",
    SImode,
    0,
    0
  },
  {
    cc_reg_not_cr0_operand,
    "",
    CCmode,
    0,
    1
  },
  {
    gpc_reg_operand,
    "",
    SImode,
    0,
    1
  },
  {
    const_int_operand,
    "",
    SImode,
    0,
    1
  },
  {
    mask_operand,
    "",
    SImode,
    0,
    1
  },
  {
    scratch_operand,
    "",
    SImode,
    0,
    0
  },
  {
    gpc_reg_operand,
    "",
    SImode,
    0,
    1
  },
  {
    gpc_reg_operand,
    "",
    SImode,
    0,
    1
  },
  {
    const_int_operand,
    "",
    SImode,
    0,
    1
  },
  {
    mask_operand,
    "",
    SImode,
    0,
    1
  },
  {
    cc_reg_not_cr0_operand,
    "",
    CCmode,
    0,
    1
  },
  {
    gpc_reg_operand,
    "",
    SImode,
    0,
    1
  },
  {
    const_int_operand,
    "",
    SImode,
    0,
    1
  },
  {
    scratch_operand,
    "",
    SImode,
    0,
    0
  },
  {
    gpc_reg_operand,
    "",
    SImode,
    0,
    1
  },
  {
    gpc_reg_operand,
    "",
    SImode,
    0,
    1
  },
  {
    const_int_operand,
    "",
    SImode,
    0,
    1
  },
  {
    cc_reg_not_cr0_operand,
    "",
    CCmode,
    0,
    1
  },
  {
    gpc_reg_operand,
    "",
    SFmode,
    0,
    1
  },
  {
    gpc_reg_operand,
    "",
    SFmode,
    0,
    1
  },
  {
    gpc_reg_operand,
    "",
    SFmode,
    0,
    1
  },
  {
    min_max_operator,
    "",
    SFmode,
    0,
    0
  },
  {
    gpc_reg_operand,
    "",
    SFmode,
    0,
    1
  },
  {
    comparison_operator,
    "",
    VOIDmode,
    0,
    1
  },
  {
    gpc_reg_operand,
    "",
    SFmode,
    0,
    1
  },
  {
    gpc_reg_operand,
    "",
    SFmode,
    0,
    1
  },
  {
    gpc_reg_operand,
    "",
    DFmode,
    0,
    1
  },
  {
    gpc_reg_operand,
    "",
    DFmode,
    0,
    1
  },
  {
    gpc_reg_operand,
    "",
    DFmode,
    0,
    1
  },
  {
    min_max_operator,
    "",
    DFmode,
    0,
    0
  },
  {
    gpc_reg_operand,
    "",
    DFmode,
    0,
    1
  },
  {
    comparison_operator,
    "",
    VOIDmode,
    0,
    1
  },
  {
    gpc_reg_operand,
    "",
    DFmode,
    0,
    1
  },
  {
    gpc_reg_operand,
    "",
    DFmode,
    0,
    1
  },
  {
    gpc_reg_operand,
    "",
    SImode,
    0,
    1
  },
  {
    gpc_reg_operand,
    "",
    SImode,
    0,
    1
  },
  {
    gpc_reg_operand,
    "",
    DFmode,
    0,
    1
  },
  {
    offsettable_mem_operand,
    "",
    DFmode,
    0,
    1
  },
  {
    gpc_reg_operand,
    "",
    DFmode,
    0,
    1
  },
  {
    gpc_reg_operand,
    "",
    SImode,
    0,
    1
  },
  {
    gpc_reg_operand,
    "",
    DFmode,
    0,
    1
  },
  {
    gpc_reg_operand,
    "",
    DImode,
    0,
    1
  },
  {
    offsettable_mem_operand,
    "",
    DImode,
    0,
    1
  },
  {
    gpc_reg_operand,
    "",
    DFmode,
    0,
    1
  },
  {
    gpc_reg_operand,
    "",
    SImode,
    0,
    1
  },
  {
    memory_operand,
    "",
    DImode,
    0,
    1
  },
  {
    gpc_reg_operand,
    "",
    DImode,
    0,
    1
  },
  {
    gpc_reg_operand,
    "",
    DImode,
    0,
    1
  },
  {
    gpc_reg_operand,
    "",
    SFmode,
    0,
    1
  },
  {
    gpc_reg_operand,
    "",
    DImode,
    0,
    1
  },
  {
    scratch_operand,
    "",
    DFmode,
    0,
    0
  },
  {
    0,
    "",
    DImode,
    0,
    1
  },
  {
    0,
    "",
    DImode,
    0,
    1
  },
  {
    0,
    "",
    VOIDmode,
    0,
    0
  },
  {
    0,
    "",
    VOIDmode,
    0,
    0
  },
  {
    0,
    "",
    VOIDmode,
    0,
    0
  },
  {
    0,
    "",
    VOIDmode,
    0,
    0
  },
  {
    0,
    "",
    VOIDmode,
    0,
    0
  },
  {
    scratch_operand,
    "",
    CCmode,
    0,
    0
  },
  {
    gpc_reg_operand,
    "",
    DImode,
    0,
    1
  },
  {
    gpc_reg_operand,
    "",
    SImode,
    0,
    1
  },
  {
    gpc_reg_operand,
    "",
    SImode,
    0,
    1
  },
  {
    gpc_reg_operand,
    "%r",
    SImode,
    0,
    1
  },
  {
    gpc_reg_operand,
    "r",
    SImode,
    0,
    1
  },
  {
    gpc_reg_operand,
    "",
    DImode,
    0,
    1
  },
  {
    gpc_reg_operand,
    "",
    DImode,
    0,
    1
  },
  {
    reg_or_add_cint64_operand,
    "",
    DImode,
    0,
    1
  },
  {
    cc_reg_not_cr0_operand,
    "",
    CCmode,
    0,
    1
  },
  {
    gpc_reg_operand,
    "",
    DImode,
    0,
    1
  },
  {
    reg_or_short_operand,
    "",
    DImode,
    0,
    1
  },
  {
    scratch_operand,
    "",
    DImode,
    0,
    0
  },
  {
    gpc_reg_operand,
    "",
    DImode,
    0,
    1
  },
  {
    gpc_reg_operand,
    "",
    DImode,
    0,
    1
  },
  {
    reg_or_short_operand,
    "",
    DImode,
    0,
    1
  },
  {
    cc_reg_not_cr0_operand,
    "",
    CCmode,
    0,
    1
  },
  {
    gpc_reg_operand,
    "",
    DImode,
    0,
    1
  },
  {
    gpc_reg_operand,
    "",
    DImode,
    0,
    1
  },
  {
    non_add_cint_operand,
    "",
    DImode,
    0,
    1
  },
  {
    cc_reg_not_cr0_operand,
    "",
    CCmode,
    0,
    1
  },
  {
    gpc_reg_operand,
    "",
    DImode,
    0,
    1
  },
  {
    scratch_operand,
    "",
    DImode,
    0,
    0
  },
  {
    gpc_reg_operand,
    "",
    DImode,
    0,
    1
  },
  {
    gpc_reg_operand,
    "",
    DImode,
    0,
    1
  },
  {
    cc_reg_not_cr0_operand,
    "",
    CCmode,
    0,
    1
  },
  {
    gpc_reg_operand,
    "",
    DImode,
    0,
    1
  },
  {
    gpc_reg_operand,
    "",
    DImode,
    0,
    1
  },
  {
    scratch_operand,
    "",
    DImode,
    0,
    0
  },
  {
    gpc_reg_operand,
    "",
    DImode,
    0,
    1
  },
  {
    gpc_reg_operand,
    "",
    DImode,
    0,
    1
  },
  {
    gpc_reg_operand,
    "",
    DImode,
    0,
    1
  },
  {
    cc_reg_not_cr0_operand,
    "",
    CCmode,
    0,
    1
  },
  {
    gpc_reg_operand,
    "",
    DImode,
    0,
    1
  },
  {
    reg_or_short_operand,
    "",
    DImode,
    0,
    1
  },
  {
    reg_or_sub_cint64_operand,
    "",
    DImode,
    0,
    1
  },
  {
    gpc_reg_operand,
    "",
    DImode,
    0,
    1
  },
  {
    gpc_reg_operand,
    "",
    DImode,
    0,
    1
  },
  {
    reg_or_cint_operand,
    "",
    DImode,
    0,
    1
  },
  {
    cc_reg_not_cr0_operand,
    "",
    CCmode,
    0,
    1
  },
  {
    gpc_reg_operand,
    "",
    DImode,
    0,
    1
  },
  {
    exact_log2_cint_operand,
    "",
    DImode,
    0,
    1
  },
  {
    scratch_operand,
    "",
    DImode,
    0,
    0
  },
  {
    gpc_reg_operand,
    "",
    DImode,
    0,
    1
  },
  {
    gpc_reg_operand,
    "",
    DImode,
    0,
    1
  },
  {
    exact_log2_cint_operand,
    "",
    DImode,
    0,
    1
  },
  {
    cc_reg_not_cr0_operand,
    "",
    CCmode,
    0,
    1
  },
  {
    gpc_reg_operand,
    "",
    DImode,
    0,
    1
  },
  {
    reg_or_cint_operand,
    "",
    DImode,
    0,
    1
  },
  {
    scratch_operand,
    "",
    DImode,
    0,
    0
  },
  {
    cc_reg_not_cr0_operand,
    "",
    CCmode,
    0,
    1
  },
  {
    gpc_reg_operand,
    "",
    DImode,
    0,
    1
  },
  {
    reg_or_cint_operand,
    "",
    DImode,
    0,
    1
  },
  {
    mask64_operand,
    "",
    DImode,
    0,
    1
  },
  {
    scratch_operand,
    "",
    DImode,
    0,
    0
  },
  {
    gpc_reg_operand,
    "",
    DImode,
    0,
    1
  },
  {
    gpc_reg_operand,
    "",
    DImode,
    0,
    1
  },
  {
    reg_or_cint_operand,
    "",
    DImode,
    0,
    1
  },
  {
    mask64_operand,
    "",
    DImode,
    0,
    1
  },
  {
    cc_reg_not_cr0_operand,
    "",
    CCmode,
    0,
    1
  },
  {
    gpc_reg_operand,
    "",
    DImode,
    0,
    1
  },
  {
    gpc_reg_operand,
    "",
    DImode,
    0,
    1
  },
  {
    reg_or_cint_operand,
    "",
    SImode,
    0,
    1
  },
  {
    cc_reg_not_cr0_operand,
    "",
    CCmode,
    0,
    1
  },
  {
    gpc_reg_operand,
    "",
    DImode,
    0,
    1
  },
  {
    reg_or_cint_operand,
    "",
    SImode,
    0,
    1
  },
  {
    scratch_operand,
    "",
    DImode,
    0,
    0
  },
  {
    cc_reg_not_cr0_operand,
    "",
    CCmode,
    0,
    1
  },
  {
    gpc_reg_operand,
    "",
    DImode,
    0,
    1
  },
  {
    const_int_operand,
    "",
    SImode,
    0,
    1
  },
  {
    const_int_operand,
    "",
    DImode,
    0,
    1
  },
  {
    scratch_operand,
    "",
    DImode,
    0,
    0
  },
  {
    gpc_reg_operand,
    "",
    DImode,
    0,
    1
  },
  {
    gpc_reg_operand,
    "",
    DImode,
    0,
    1
  },
  {
    const_int_operand,
    "",
    SImode,
    0,
    1
  },
  {
    const_int_operand,
    "",
    DImode,
    0,
    1
  },
  {
    cc_reg_not_cr0_operand,
    "",
    CCmode,
    0,
    1
  },
  {
    gpc_reg_operand,
    "",
    DImode,
    0,
    1
  },
  {
    const_int_operand,
    "",
    SImode,
    0,
    1
  },
  {
    mask64_operand,
    "",
    DImode,
    0,
    1
  },
  {
    scratch_operand,
    "",
    DImode,
    0,
    0
  },
  {
    gpc_reg_operand,
    "",
    DImode,
    0,
    1
  },
  {
    gpc_reg_operand,
    "",
    DImode,
    0,
    1
  },
  {
    const_int_operand,
    "",
    SImode,
    0,
    1
  },
  {
    mask64_operand,
    "",
    DImode,
    0,
    1
  },
  {
    cc_reg_not_cr0_operand,
    "",
    CCmode,
    0,
    1
  },
  {
    gpc_reg_operand,
    "",
    DImode,
    0,
    1
  },
  {
    gpc_reg_operand,
    "",
    DImode,
    0,
    1
  },
  {
    mask64_2_operand,
    "",
    DImode,
    0,
    1
  },
  {
    scratch_operand,
    "",
    CCmode,
    0,
    0
  },
  {
    cc_reg_not_cr0_operand,
    "",
    CCmode,
    0,
    1
  },
  {
    gpc_reg_operand,
    "",
    DImode,
    0,
    1
  },
  {
    and64_operand,
    "",
    DImode,
    0,
    1
  },
  {
    scratch_operand,
    "",
    DImode,
    0,
    0
  },
  {
    scratch_operand,
    "",
    CCmode,
    0,
    0
  },
  {
    cc_reg_operand,
    "",
    CCmode,
    0,
    1
  },
  {
    gpc_reg_operand,
    "",
    DImode,
    0,
    1
  },
  {
    mask64_2_operand,
    "",
    DImode,
    0,
    1
  },
  {
    scratch_operand,
    "",
    DImode,
    0,
    0
  },
  {
    scratch_operand,
    "",
    CCmode,
    0,
    0
  },
  {
    gpc_reg_operand,
    "",
    DImode,
    0,
    1
  },
  {
    gpc_reg_operand,
    "",
    DImode,
    0,
    1
  },
  {
    and64_operand,
    "",
    DImode,
    0,
    1
  },
  {
    cc_reg_not_cr0_operand,
    "",
    CCmode,
    0,
    1
  },
  {
    scratch_operand,
    "",
    CCmode,
    0,
    0
  },
  {
    gpc_reg_operand,
    "",
    DImode,
    0,
    1
  },
  {
    gpc_reg_operand,
    "",
    DImode,
    0,
    1
  },
  {
    mask64_2_operand,
    "",
    DImode,
    0,
    1
  },
  {
    cc_reg_operand,
    "",
    CCmode,
    0,
    1
  },
  {
    scratch_operand,
    "",
    CCmode,
    0,
    0
  },
  {
    gpc_reg_operand,
    "",
    DImode,
    0,
    1
  },
  {
    gpc_reg_operand,
    "",
    DImode,
    0,
    1
  },
  {
    reg_or_logical_cint_operand,
    "",
    DImode,
    0,
    1
  },
  {
    cc_reg_not_cr0_operand,
    "",
    CCmode,
    0,
    1
  },
  {
    gpc_reg_operand,
    "",
    DImode,
    0,
    1
  },
  {
    gpc_reg_operand,
    "",
    DImode,
    0,
    1
  },
  {
    scratch_operand,
    "",
    DImode,
    0,
    0
  },
  {
    boolean_operator,
    "",
    DImode,
    0,
    0
  },
  {
    gpc_reg_operand,
    "",
    DImode,
    0,
    1
  },
  {
    gpc_reg_operand,
    "",
    DImode,
    0,
    1
  },
  {
    gpc_reg_operand,
    "",
    DImode,
    0,
    1
  },
  {
    cc_reg_not_cr0_operand,
    "",
    CCmode,
    0,
    1
  },
  {
    boolean_operator,
    "",
    DImode,
    0,
    0
  },
  {
    gpc_reg_operand,
    "",
    DImode,
    0,
    1
  },
  {
    gpc_reg_operand,
    "",
    DImode,
    0,
    1
  },
  {
    non_logical_cint_operand,
    "",
    DImode,
    0,
    1
  },
  {
    boolean_or_operator,
    "",
    DImode,
    0,
    0
  },
  {
    gpc_reg_operand,
    "",
    SImode,
    0,
    1
  },
  {
    got_operand,
    "",
    SImode,
    0,
    1
  },
  {
    gpc_reg_operand,
    "",
    SImode,
    0,
    1
  },
  {
    got_no_const_operand,
    "",
    SImode,
    0,
    1
  },
  {
    memory_operand,
    "",
    SImode,
    0,
    1
  },
  {
    general_operand,
    "",
    SImode,
    0,
    1
  },
  {
    any_operand,
    "",
    SImode,
    0,
    1
  },
  {
    general_operand,
    "",
    HImode,
    0,
    1
  },
  {
    any_operand,
    "",
    HImode,
    0,
    1
  },
  {
    general_operand,
    "",
    QImode,
    0,
    1
  },
  {
    any_operand,
    "",
    QImode,
    0,
    1
  },
  {
    nonimmediate_operand,
    "",
    CCmode,
    0,
    1
  },
  {
    nonimmediate_operand,
    "",
    CCmode,
    0,
    1
  },
  {
    nonimmediate_operand,
    "",
    SFmode,
    0,
    1
  },
  {
    any_operand,
    "",
    SFmode,
    0,
    1
  },
  {
    gpc_reg_operand,
    "",
    SFmode,
    0,
    1
  },
  {
    const_double_operand,
    "",
    SFmode,
    0,
    1
  },
  {
    nonimmediate_operand,
    "",
    DFmode,
    0,
    1
  },
  {
    any_operand,
    "",
    DFmode,
    0,
    1
  },
  {
    gpc_reg_operand,
    "",
    DFmode,
    0,
    1
  },
  {
    const_int_operand,
    "",
    DFmode,
    0,
    1
  },
  {
    gpc_reg_operand,
    "",
    DFmode,
    0,
    1
  },
  {
    const_double_operand,
    "",
    DFmode,
    0,
    1
  },
  {
    gpc_reg_operand,
    "",
    DFmode,
    0,
    1
  },
  {
    easy_fp_constant,
    "",
    DFmode,
    0,
    1
  },
  {
    general_operand,
    "",
    TFmode,
    0,
    1
  },
  {
    any_operand,
    "",
    TFmode,
    0,
    1
  },
  {
    gpc_reg_operand,
    "",
    TFmode,
    0,
    1
  },
  {
    easy_fp_constant,
    "",
    TFmode,
    0,
    1
  },
  {
    gpc_reg_operand,
    "",
    SFmode,
    0,
    1
  },
  {
    gpc_reg_operand,
    "",
    TFmode,
    0,
    1
  },
  {
    scratch_operand,
    "",
    DFmode,
    0,
    0
  },
  {
    gpc_reg_operand,
    "",
    TFmode,
    0,
    1
  },
  {
    gpc_reg_operand,
    "",
    DImode,
    0,
    1
  },
  {
    scratch_operand,
    "",
    DFmode,
    0,
    0
  },
  {
    gpc_reg_operand,
    "",
    TFmode,
    0,
    1
  },
  {
    gpc_reg_operand,
    "",
    SImode,
    0,
    1
  },
  {
    scratch_operand,
    "",
    DFmode,
    0,
    0
  },
  {
    gpc_reg_operand,
    "",
    DImode,
    0,
    1
  },
  {
    gpc_reg_operand,
    "",
    TFmode,
    0,
    1
  },
  {
    scratch_operand,
    "",
    DFmode,
    0,
    0
  },
  {
    gpc_reg_operand,
    "",
    SImode,
    0,
    1
  },
  {
    gpc_reg_operand,
    "",
    TFmode,
    0,
    1
  },
  {
    scratch_operand,
    "",
    DFmode,
    0,
    0
  },
  {
    general_operand,
    "",
    DImode,
    0,
    1
  },
  {
    any_operand,
    "",
    DImode,
    0,
    1
  },
  {
    gpc_reg_operand,
    "",
    DImode,
    0,
    1
  },
  {
    const_int_operand,
    "",
    DImode,
    0,
    1
  },
  {
    gpc_reg_operand,
    "",
    TImode,
    0,
    1
  },
  {
    const_double_operand,
    "",
    TImode,
    0,
    1
  },
  {
    gpc_reg_operand,
    "",
    DImode,
    0,
    1
  },
  {
    mask64_operand,
    "",
    DImode,
    0,
    1
  },
  {
    gpc_reg_operand,
    "",
    DImode,
    0,
    1
  },
  {
    const_double_operand,
    "",
    DImode,
    0,
    1
  },
  {
    general_operand,
    "",
    TImode,
    0,
    1
  },
  {
    general_operand,
    "",
    TImode,
    0,
    1
  },
  {
    0,
    "",
    BLKmode,
    0,
    1
  },
  {
    0,
    "",
    BLKmode,
    0,
    1
  },
  {
    0,
    "",
    SImode,
    0,
    1
  },
  {
    0,
    "",
    SImode,
    0,
    1
  },
  {
    0,
    "",
    VOIDmode,
    0,
    1
  },
  {
    0,
    "",
    VOIDmode,
    0,
    1
  },
  {
    0,
    "",
    VOIDmode,
    0,
    1
  },
  {
    0,
    "",
    VOIDmode,
    0,
    1
  },
  {
    scratch_operand,
    "",
    SImode,
    0,
    0
  },
  {
    0,
    "",
    VOIDmode,
    0,
    1
  },
  {
    0,
    "",
    VOIDmode,
    0,
    1
  },
  {
    0,
    "",
    VOIDmode,
    0,
    1
  },
  {
    0,
    "",
    VOIDmode,
    0,
    1
  },
  {
    scratch_operand,
    "",
    DImode,
    0,
    0
  },
  {
    scratch_operand,
    "",
    SImode,
    0,
    0
  },
  {
    0,
    "",
    VOIDmode,
    0,
    1
  },
  {
    0,
    "",
    VOIDmode,
    0,
    1
  },
  {
    0,
    "",
    VOIDmode,
    0,
    1
  },
  {
    0,
    "",
    VOIDmode,
    0,
    1
  },
  {
    scratch_operand,
    "",
    SImode,
    0,
    0
  },
  {
    scratch_operand,
    "",
    SImode,
    0,
    0
  },
  {
    gpc_reg_operand,
    "=f",
    DFmode,
    0,
    1
  },
  {
    memory_operand,
    "",
    DFmode,
    0,
    1
  },
  {
    gpc_reg_operand,
    "=f",
    DFmode,
    0,
    1
  },
  {
    memory_operand,
    "",
    DFmode,
    0,
    1
  },
  {
    gpc_reg_operand,
    "f",
    DFmode,
    0,
    1
  },
  {
    memory_operand,
    "",
    DFmode,
    0,
    1
  },
  {
    gpc_reg_operand,
    "f",
    DFmode,
    0,
    1
  },
  {
    gpc_reg_operand,
    "=r",
    VOIDmode,
    0,
    1
  },
  {
    reg_or_short_operand,
    "",
    VOIDmode,
    0,
    1
  },
  {
    any_operand,
    "",
    VOIDmode,
    0,
    1
  },
  {
    any_operand,
    "",
    VOIDmode,
    0,
    1
  },
  {
    register_operand,
    "",
    VOIDmode,
    0,
    1
  },
  {
    register_operand,
    "",
    VOIDmode,
    0,
    1
  },
  {
    memory_operand,
    "",
    VOIDmode,
    0,
    1
  },
  {
    register_operand,
    "",
    VOIDmode,
    0,
    1
  },
  {
    gpc_reg_operand,
    "",
    SImode,
    0,
    1
  },
  {
    0,
    "",
    VOIDmode,
    0,
    1
  },
  {
    gpc_reg_operand,
    "",
    DImode,
    0,
    1
  },
  {
    0,
    "",
    VOIDmode,
    0,
    1
  },
  {
    gpc_reg_operand,
    "",
    SImode,
    0,
    1
  },
  {
    0,
    "",
    VOIDmode,
    0,
    1
  },
  {
    address_operand,
    "",
    VOIDmode,
    0,
    1
  },
  {
    0,
    "",
    VOIDmode,
    0,
    1
  },
  {
    0,
    "",
    VOIDmode,
    0,
    1
  },
  {
    register_operand,
    "",
    VOIDmode,
    0,
    1
  },
  {
    address_operand,
    "",
    VOIDmode,
    0,
    1
  },
  {
    0,
    "",
    VOIDmode,
    0,
    1
  },
  {
    0,
    "",
    VOIDmode,
    0,
    1
  },
  {
    gpc_reg_operand,
    "",
    TFmode,
    0,
    1
  },
  {
    gpc_reg_operand,
    "",
    TFmode,
    0,
    1
  },
  {
    cc_reg_operand,
    "",
    CCmode,
    0,
    1
  },
  {
    gpc_reg_operand,
    "",
    SImode,
    0,
    1
  },
  {
    non_short_cint_operand,
    "",
    SImode,
    0,
    1
  },
  {
    gpc_reg_operand,
    "",
    SImode,
    0,
    1
  },
  {
    gpc_reg_operand,
    "",
    SImode,
    0,
    1
  },
  {
    short_cint_operand,
    "",
    SImode,
    0,
    1
  },
  {
    cc_reg_operand,
    "",
    CCmode,
    0,
    1
  },
  {
    short_cint_operand,
    "",
    SImode,
    0,
    1
  },
  {
    gpc_reg_operand,
    "",
    SImode,
    0,
    1
  },
  {
    gpc_reg_operand,
    "",
    SImode,
    0,
    1
  },
  {
    u_short_cint_operand,
    "",
    SImode,
    0,
    1
  },
  {
    cc_reg_operand,
    "",
    CCUNSmode,
    0,
    1
  },
  {
    short_cint_operand,
    "",
    SImode,
    0,
    1
  },
  {
    cc_reg_not_cr0_operand,
    "",
    CCmode,
    0,
    1
  },
  {
    scc_comparison_operator,
    "",
    SImode,
    0,
    0
  },
  {
    cc_reg_operand,
    "",
    VOIDmode,
    0,
    1
  },
  {
    gpc_reg_operand,
    "",
    SImode,
    0,
    1
  },
  {
    cc_reg_not_cr0_operand,
    "",
    CCmode,
    0,
    1
  },
  {
    scc_comparison_operator,
    "",
    SImode,
    0,
    0
  },
  {
    cc_reg_operand,
    "",
    VOIDmode,
    0,
    1
  },
  {
    const_int_operand,
    "",
    SImode,
    0,
    1
  },
  {
    gpc_reg_operand,
    "",
    SImode,
    0,
    1
  },
  {
    gpc_reg_operand,
    "=r",
    SImode,
    0,
    1
  },
  {
    scc_comparison_operator,
    "",
    SImode,
    0,
    0
  },
  {
    cc_reg_operand,
    "y",
    VOIDmode,
    0,
    1
  },
  {
    gpc_reg_operand,
    "=r",
    SImode,
    0,
    1
  },
  {
    scc_comparison_operator,
    "",
    SImode,
    0,
    0
  },
  {
    cc_reg_operand,
    "y",
    VOIDmode,
    0,
    1
  },
  {
    gpc_reg_operand,
    "=r",
    DImode,
    0,
    1
  },
  {
    scc_comparison_operator,
    "",
    DImode,
    0,
    0
  },
  {
    cc_reg_operand,
    "y",
    VOIDmode,
    0,
    1
  },
  {
    gpc_reg_operand,
    "=r",
    DImode,
    0,
    1
  },
  {
    scc_comparison_operator,
    "",
    DImode,
    0,
    0
  },
  {
    cc_reg_operand,
    "y",
    VOIDmode,
    0,
    1
  },
  {
    gpc_reg_operand,
    "",
    SImode,
    0,
    1
  },
  {
    gpc_reg_operand,
    "",
    SImode,
    0,
    1
  },
  {
    reg_or_cint_operand,
    "",
    SImode,
    0,
    1
  },
  {
    scratch_operand,
    "",
    SImode,
    0,
    0
  },
  {
    cc_reg_not_cr0_operand,
    "",
    CCmode,
    0,
    1
  },
  {
    gpc_reg_operand,
    "",
    DImode,
    0,
    1
  },
  {
    gpc_reg_operand,
    "",
    DImode,
    0,
    1
  },
  {
    reg_or_cint_operand,
    "",
    DImode,
    0,
    1
  },
  {
    scratch_operand,
    "",
    DImode,
    0,
    0
  },
  {
    cc_reg_not_cr0_operand,
    "",
    CCmode,
    0,
    1
  },
  {
    gpc_reg_operand,
    "",
    SImode,
    0,
    1
  },
  {
    comparison_operator,
    "",
    VOIDmode,
    0,
    0
  },
  {
    0,
    "",
    SImode,
    0,
    1
  },
  {
    reg_or_cint_operand,
    "",
    SImode,
    0,
    1
  },
  {
    gpc_reg_operand,
    "",
    SImode,
    0,
    1
  },
  {
    register_operand,
    "",
    SImode,
    0,
    1
  },
  {
    cc_reg_not_cr0_operand,
    "",
    CCmode,
    0,
    1
  },
  {
    gpc_reg_operand,
    "",
    SImode,
    0,
    1
  },
  {
    reg_or_cint_operand,
    "",
    SImode,
    0,
    1
  },
  {
    gpc_reg_operand,
    "",
    SImode,
    0,
    1
  },
  {
    scratch_operand,
    "",
    SImode,
    0,
    0
  },
  {
    gpc_reg_operand,
    "",
    SImode,
    0,
    1
  },
  {
    gpc_reg_operand,
    "",
    SImode,
    0,
    1
  },
  {
    gpc_reg_operand,
    "",
    SImode,
    0,
    1
  },
  {
    scratch_operand,
    "",
    SImode,
    0,
    0
  },
  {
    cc_reg_not_cr0_operand,
    "",
    CCmode,
    0,
    1
  },
  {
    gpc_reg_operand,
    "",
    DImode,
    0,
    1
  },
  {
    gpc_reg_operand,
    "",
    DImode,
    0,
    1
  },
  {
    gpc_reg_operand,
    "",
    DImode,
    0,
    1
  },
  {
    scratch_operand,
    "",
    DImode,
    0,
    0
  },
  {
    cc_reg_not_cr0_operand,
    "",
    CCmode,
    0,
    1
  },
  {
    gpc_reg_operand,
    "",
    SImode,
    0,
    1
  },
  {
    gpc_reg_operand,
    "",
    SImode,
    0,
    1
  },
  {
    reg_or_short_operand,
    "",
    SImode,
    0,
    1
  },
  {
    scratch_operand,
    "",
    SImode,
    0,
    0
  },
  {
    cc_reg_not_cr0_operand,
    "",
    CCmode,
    0,
    1
  },
  {
    gpc_reg_operand,
    "",
    SImode,
    0,
    1
  },
  {
    reg_or_short_operand,
    "",
    SImode,
    0,
    1
  },
  {
    gpc_reg_operand,
    "",
    SImode,
    0,
    1
  },
  {
    scratch_operand,
    "",
    SImode,
    0,
    0
  },
  {
    gpc_reg_operand,
    "",
    SImode,
    0,
    1
  },
  {
    gpc_reg_operand,
    "",
    SImode,
    0,
    1
  },
  {
    reg_or_neg_short_operand,
    "",
    SImode,
    0,
    1
  },
  {
    cc_reg_not_cr0_operand,
    "",
    CCmode,
    0,
    1
  },
  {
    gpc_reg_operand,
    "",
    SImode,
    0,
    1
  },
  {
    reg_or_neg_short_operand,
    "",
    SImode,
    0,
    1
  },
  {
    gpc_reg_operand,
    "",
    SImode,
    0,
    1
  },
  {
    scratch_operand,
    "",
    SImode,
    0,
    0
  },
  {
    gpc_reg_operand,
    "",
    SImode,
    0,
    1
  },
  {
    gpc_reg_operand,
    "",
    SImode,
    0,
    1
  },
  {
    reg_or_neg_short_operand,
    "",
    SImode,
    0,
    1
  },
  {
    gpc_reg_operand,
    "",
    SImode,
    0,
    1
  },
  {
    cc_reg_not_cr0_operand,
    "",
    CCmode,
    0,
    1
  },
  {
    gpc_reg_operand,
    "",
    DImode,
    0,
    1
  },
  {
    gpc_reg_operand,
    "",
    DImode,
    0,
    1
  },
  {
    reg_or_neg_short_operand,
    "",
    DImode,
    0,
    1
  },
  {
    cc_reg_not_cr0_operand,
    "",
    CCmode,
    0,
    1
  },
  {
    gpc_reg_operand,
    "",
    DImode,
    0,
    1
  },
  {
    reg_or_short_operand,
    "",
    DImode,
    0,
    1
  },
  {
    gpc_reg_operand,
    "",
    DImode,
    0,
    1
  },
  {
    scratch_operand,
    "",
    DImode,
    0,
    0
  },
  {
    gpc_reg_operand,
    "",
    DImode,
    0,
    1
  },
  {
    gpc_reg_operand,
    "",
    DImode,
    0,
    1
  },
  {
    reg_or_short_operand,
    "",
    DImode,
    0,
    1
  },
  {
    gpc_reg_operand,
    "",
    DImode,
    0,
    1
  },
  {
    cc_reg_not_cr0_operand,
    "",
    CCmode,
    0,
    1
  },
  {
    cc_reg_operand,
    "",
    CCEQmode,
    0,
    1
  },
  {
    branch_comparison_operator,
    "",
    VOIDmode,
    0,
    0
  },
  {
    cc_reg_operand,
    "",
    VOIDmode,
    0,
    1
  },
  {
    branch_comparison_operator,
    "",
    VOIDmode,
    0,
    0
  },
  {
    cc_reg_operand,
    "",
    VOIDmode,
    0,
    1
  },
  {
    lwa_operand,
    "rm",
    SImode,
    0,
    1
  },
  {
    0,
    "",
    VOIDmode,
    0,
    1
  },
  {
    0,
    "",
    VOIDmode,
    0,
    1
  },
  {
    0,
    "",
    VOIDmode,
    0,
    1
  },
  {
    0,
    "",
    VOIDmode,
    0,
    1
  },
  {
    0,
    "",
    VOIDmode,
    0,
    1
  },
  {
    register_operand,
    "",
    SImode,
    0,
    1
  },
  {
    0,
    "",
    VOIDmode,
    0,
    1
  },
  {
    scratch_operand,
    "",
    CCmode,
    0,
    0
  },
  {
    scratch_operand,
    "",
    SImode,
    0,
    0
  },
  {
    register_operand,
    "",
    DImode,
    0,
    1
  },
  {
    0,
    "",
    VOIDmode,
    0,
    1
  },
  {
    scratch_operand,
    "",
    CCmode,
    0,
    0
  },
  {
    scratch_operand,
    "",
    DImode,
    0,
    0
  },
  {
    gpc_reg_operand,
    "",
    SImode,
    0,
    1
  },
  {
    gpc_reg_operand,
    "",
    SImode,
    0,
    1
  },
  {
    comparison_operator,
    "",
    VOIDmode,
    0,
    0
  },
  {
    scratch_operand,
    "",
    CCmode,
    0,
    0
  },
  {
    scratch_operand,
    "",
    SImode,
    0,
    0
  },
  {
    0,
    "",
    VOIDmode,
    0,
    1
  },
  {
    0,
    "",
    VOIDmode,
    0,
    1
  },
  {
    nonimmediate_operand,
    "",
    SImode,
    0,
    1
  },
  {
    gpc_reg_operand,
    "",
    SImode,
    0,
    1
  },
  {
    comparison_operator,
    "",
    VOIDmode,
    0,
    0
  },
  {
    scratch_operand,
    "",
    CCmode,
    0,
    0
  },
  {
    scratch_operand,
    "",
    SImode,
    0,
    0
  },
  {
    0,
    "",
    VOIDmode,
    0,
    1
  },
  {
    0,
    "",
    VOIDmode,
    0,
    1
  },
  {
    gpc_reg_operand,
    "",
    DImode,
    0,
    1
  },
  {
    gpc_reg_operand,
    "",
    DImode,
    0,
    1
  },
  {
    comparison_operator,
    "",
    VOIDmode,
    0,
    0
  },
  {
    scratch_operand,
    "",
    CCmode,
    0,
    0
  },
  {
    scratch_operand,
    "",
    DImode,
    0,
    0
  },
  {
    0,
    "",
    VOIDmode,
    0,
    1
  },
  {
    0,
    "",
    VOIDmode,
    0,
    1
  },
  {
    nonimmediate_operand,
    "",
    DImode,
    0,
    1
  },
  {
    gpc_reg_operand,
    "",
    DImode,
    0,
    1
  },
  {
    comparison_operator,
    "",
    VOIDmode,
    0,
    0
  },
  {
    scratch_operand,
    "",
    CCmode,
    0,
    0
  },
  {
    scratch_operand,
    "",
    DImode,
    0,
    0
  },
  {
    0,
    "",
    VOIDmode,
    0,
    1
  },
  {
    0,
    "",
    VOIDmode,
    0,
    1
  },
  {
    trap_comparison_operator,
    "",
    VOIDmode,
    0,
    0
  },
  {
    const_int_operand,
    "",
    VOIDmode,
    0,
    1
  },
  {
    general_operand,
    "",
    VOIDmode,
    0,
    1
  },
  {
    register_operand,
    "",
    VOIDmode,
    0,
    1
  },
  {
    scratch_operand,
    "",
    VOIDmode,
    0,
    0
  },
  {
    nonimmediate_operand,
    "",
    V4SImode,
    0,
    1
  },
  {
    any_operand,
    "",
    V4SImode,
    0,
    1
  },
  {
    nonimmediate_operand,
    "",
    V8HImode,
    0,
    1
  },
  {
    any_operand,
    "",
    V8HImode,
    0,
    1
  },
  {
    nonimmediate_operand,
    "",
    V16QImode,
    0,
    1
  },
  {
    any_operand,
    "",
    V16QImode,
    0,
    1
  },
  {
    nonimmediate_operand,
    "",
    V4SFmode,
    0,
    1
  },
  {
    any_operand,
    "",
    V4SFmode,
    0,
    1
  },
  {
    register_operand,
    "",
    V4SFmode,
    0,
    1
  },
  {
    register_operand,
    "",
    V4SFmode,
    0,
    1
  },
  {
    register_operand,
    "",
    V4SFmode,
    0,
    1
  },
};



const struct insn_data insn_data[] = 
{
  {
    "*rs6000.md:866",
    (const PTR) output_0,
    0,
    &operand_data[1],
    2,
    0,
    2,
    2
  },
  {
    "*rs6000.md:875",
    (const PTR) output_1,
    0,
    &operand_data[3],
    3,
    0,
    2,
    2
  },
  {
    "*rs6000.md:900",
    (const PTR) output_2,
    0,
    &operand_data[6],
    3,
    1,
    2,
    2
  },
  {
    "extendqidi2",
    "extsb %0,%1",
    (insn_gen_fn) gen_extendqidi2,
    &operand_data[9],
    2,
    0,
    1,
    1
  },
  {
    "*rs6000.md:933",
    (const PTR) output_4,
    0,
    &operand_data[3],
    3,
    0,
    2,
    2
  },
  {
    "*rs6000.md:958",
    (const PTR) output_5,
    0,
    &operand_data[6],
    3,
    1,
    2,
    2
  },
  {
    "*rs6000.md:991",
    (const PTR) output_6,
    0,
    &operand_data[11],
    2,
    0,
    2,
    2
  },
  {
    "*rs6000.md:1000",
    (const PTR) output_7,
    0,
    &operand_data[13],
    3,
    0,
    2,
    2
  },
  {
    "*rs6000.md:1025",
    (const PTR) output_8,
    0,
    &operand_data[16],
    3,
    1,
    2,
    2
  },
  {
    "*rs6000.md:1058",
    (const PTR) output_9,
    0,
    &operand_data[11],
    2,
    0,
    2,
    2
  },
  {
    "*rs6000.md:1067",
    (const PTR) output_10,
    0,
    &operand_data[13],
    3,
    0,
    2,
    2
  },
  {
    "*rs6000.md:1092",
    (const PTR) output_11,
    0,
    &operand_data[16],
    3,
    1,
    2,
    2
  },
  {
    "*rs6000.md:1125",
    (const PTR) output_12,
    0,
    &operand_data[19],
    2,
    0,
    2,
    2
  },
  {
    "*rs6000.md:1134",
    (const PTR) output_13,
    0,
    &operand_data[21],
    3,
    0,
    2,
    2
  },
  {
    "*rs6000.md:1159",
    (const PTR) output_14,
    0,
    &operand_data[24],
    3,
    1,
    2,
    2
  },
  {
    "*rs6000.md:1192",
    (const PTR) output_15,
    0,
    &operand_data[27],
    2,
    0,
    2,
    2
  },
  {
    "*rs6000.md:1201",
    (const PTR) output_16,
    0,
    &operand_data[21],
    3,
    0,
    2,
    2
  },
  {
    "*rs6000.md:1226",
    (const PTR) output_17,
    0,
    &operand_data[24],
    3,
    1,
    2,
    2
  },
  {
    "*rs6000.md:1259",
    (const PTR) output_18,
    0,
    &operand_data[29],
    2,
    0,
    2,
    2
  },
  {
    "*rs6000.md:1268",
    (const PTR) output_19,
    0,
    &operand_data[31],
    3,
    0,
    2,
    2
  },
  {
    "*rs6000.md:1293",
    (const PTR) output_20,
    0,
    &operand_data[34],
    3,
    1,
    2,
    2
  },
  {
    "extendqisi2_ppc",
    "extsb %0,%1",
    (insn_gen_fn) gen_extendqisi2_ppc,
    &operand_data[37],
    2,
    0,
    1,
    1
  },
  {
    "*rs6000.md:1341",
    (const PTR) output_22,
    0,
    &operand_data[31],
    3,
    0,
    2,
    2
  },
  {
    "*rs6000.md:1366",
    (const PTR) output_23,
    0,
    &operand_data[34],
    3,
    1,
    2,
    2
  },
  {
    "*rs6000.md:1425",
    (const PTR) output_24,
    0,
    &operand_data[39],
    2,
    0,
    2,
    2
  },
  {
    "*rs6000.md:1434",
    (const PTR) output_25,
    0,
    &operand_data[41],
    3,
    0,
    2,
    2
  },
  {
    "*rs6000.md:1459",
    (const PTR) output_26,
    0,
    &operand_data[44],
    3,
    1,
    2,
    2
  },
  {
    "extendqihi2_ppc",
    "extsb %0,%1",
    (insn_gen_fn) gen_extendqihi2_ppc,
    &operand_data[47],
    2,
    0,
    1,
    1
  },
  {
    "*rs6000.md:1507",
    (const PTR) output_28,
    0,
    &operand_data[41],
    3,
    0,
    2,
    2
  },
  {
    "*rs6000.md:1532",
    (const PTR) output_29,
    0,
    &operand_data[44],
    3,
    1,
    2,
    2
  },
  {
    "*rs6000.md:1593",
    (const PTR) output_30,
    0,
    &operand_data[49],
    2,
    0,
    2,
    2
  },
  {
    "*rs6000.md:1602",
    (const PTR) output_31,
    0,
    &operand_data[51],
    3,
    0,
    2,
    2
  },
  {
    "*rs6000.md:1627",
    (const PTR) output_32,
    0,
    &operand_data[54],
    3,
    1,
    2,
    2
  },
  {
    "*rs6000.md:1660",
    (const PTR) output_33,
    0,
    &operand_data[49],
    2,
    0,
    2,
    2
  },
  {
    "*rs6000.md:1669",
    (const PTR) output_34,
    0,
    &operand_data[51],
    3,
    0,
    2,
    2
  },
  {
    "*rs6000.md:1694",
    (const PTR) output_35,
    0,
    &operand_data[54],
    3,
    1,
    2,
    2
  },
  {
    "*addsi3_internal1",
    (const PTR) output_36,
    0,
    &operand_data[57],
    3,
    0,
    4,
    2
  },
  {
    "*addsi3_internal2",
    (const PTR) output_37,
    0,
    &operand_data[60],
    4,
    0,
    4,
    2
  },
  {
    "*addsi3_internal3",
    (const PTR) output_38,
    0,
    &operand_data[64],
    4,
    2,
    4,
    2
  },
  {
    "one_cmplsi2",
    "nor %0,%1,%1",
    (insn_gen_fn) gen_one_cmplsi2,
    &operand_data[68],
    2,
    0,
    1,
    1
  },
  {
    "*rs6000.md:1861",
    (const PTR) output_40,
    0,
    &operand_data[70],
    3,
    0,
    2,
    2
  },
  {
    "*rs6000.md:1886",
    (const PTR) output_41,
    0,
    &operand_data[73],
    3,
    1,
    2,
    2
  },
  {
    "*rs6000.md:1913",
    "{sf%I1|subf%I1c} %0,%2,%1",
    0,
    &operand_data[76],
    3,
    0,
    1,
    1
  },
  {
    "*rs6000.md:1920",
    (const PTR) output_43,
    0,
    &operand_data[79],
    3,
    0,
    2,
    2
  },
  {
    "*rs6000.md:1929",
    (const PTR) output_44,
    0,
    &operand_data[82],
    4,
    0,
    2,
    2
  },
  {
    "*rs6000.md:1942",
    (const PTR) output_45,
    0,
    &operand_data[82],
    4,
    0,
    2,
    2
  },
  {
    "*rs6000.md:1970",
    (const PTR) output_46,
    0,
    &operand_data[86],
    4,
    2,
    2,
    2
  },
  {
    "*rs6000.md:1984",
    (const PTR) output_47,
    0,
    &operand_data[86],
    4,
    2,
    2,
    2
  },
  {
    "*rs6000.md:2149",
    "doz%I2 %0,%1,%2",
    0,
    &operand_data[90],
    3,
    2,
    1,
    1
  },
  {
    "*rs6000.md:2158",
    (const PTR) output_49,
    0,
    &operand_data[93],
    4,
    2,
    2,
    2
  },
  {
    "*rs6000.md:2193",
    (const PTR) output_50,
    0,
    &operand_data[97],
    4,
    6,
    2,
    2
  },
  {
    "*abssi2_power",
    "abs %0,%1",
    0,
    &operand_data[68],
    2,
    0,
    1,
    1
  },
  {
    "abssi2_nopower",
    "#",
    (insn_gen_fn) gen_abssi2_nopower,
    &operand_data[101],
    3,
    0,
    2,
    1
  },
  {
    "*nabs_power",
    "nabs %0,%1",
    0,
    &operand_data[68],
    2,
    0,
    1,
    1
  },
  {
    "*nabs_nopower",
    "#",
    0,
    &operand_data[101],
    3,
    0,
    2,
    1
  },
  {
    "negsi2",
    "neg %0,%1",
    (insn_gen_fn) gen_negsi2,
    &operand_data[68],
    2,
    0,
    1,
    1
  },
  {
    "*rs6000.md:2315",
    (const PTR) output_56,
    0,
    &operand_data[70],
    3,
    0,
    2,
    2
  },
  {
    "*rs6000.md:2340",
    (const PTR) output_57,
    0,
    &operand_data[73],
    3,
    1,
    2,
    2
  },
  {
    "ffssi2",
    "neg %0,%1\n\tand %0,%0,%1\n\t{cntlz|cntlzw} %0,%0\n\t{sfi|subfic} %0,%0,32",
    (insn_gen_fn) gen_ffssi2,
    &operand_data[104],
    2,
    0,
    1,
    1
  },
  {
    "mulsi3_mq",
    (const PTR) output_59,
    (insn_gen_fn) gen_mulsi3_mq,
    &operand_data[106],
    4,
    0,
    2,
    2
  },
  {
    "mulsi3_no_mq",
    (const PTR) output_60,
    (insn_gen_fn) gen_mulsi3_no_mq,
    &operand_data[106],
    3,
    0,
    2,
    2
  },
  {
    "*rs6000.md:2419",
    (const PTR) output_61,
    0,
    &operand_data[110],
    5,
    0,
    2,
    2
  },
  {
    "*rs6000.md:2449",
    (const PTR) output_62,
    0,
    &operand_data[110],
    4,
    0,
    2,
    2
  },
  {
    "*rs6000.md:2476",
    (const PTR) output_63,
    0,
    &operand_data[115],
    5,
    2,
    2,
    2
  },
  {
    "*rs6000.md:2508",
    (const PTR) output_64,
    0,
    &operand_data[115],
    4,
    2,
    2,
    2
  },
  {
    "*rs6000.md:2561",
    "divs %0,%1,%2",
    0,
    &operand_data[120],
    4,
    2,
    1,
    1
  },
  {
    "udivsi3_mq",
    "divwu %0,%1,%2",
    (insn_gen_fn) gen_udivsi3_mq,
    &operand_data[124],
    4,
    0,
    1,
    1
  },
  {
    "*udivsi3_no_mq",
    "divwu %0,%1,%2",
    0,
    &operand_data[120],
    3,
    0,
    1,
    1
  },
  {
    "divsi3_mq",
    "divw %0,%1,%2",
    (insn_gen_fn) gen_divsi3_mq,
    &operand_data[124],
    4,
    0,
    1,
    1
  },
  {
    "*divsi3_no_mq",
    "divw %0,%1,%2",
    0,
    &operand_data[120],
    3,
    0,
    1,
    1
  },
  {
    "*rs6000.md:2688",
    "{srai|srawi} %0,%1,%p2\n\t{aze|addze} %0,%0",
    0,
    &operand_data[128],
    3,
    0,
    1,
    1
  },
  {
    "*rs6000.md:2696",
    (const PTR) output_71,
    0,
    &operand_data[131],
    4,
    0,
    2,
    2
  },
  {
    "*rs6000.md:2723",
    (const PTR) output_72,
    0,
    &operand_data[135],
    4,
    2,
    2,
    2
  },
  {
    "*rs6000.md:2752",
    "div %0,%1,%3",
    0,
    &operand_data[139],
    5,
    3,
    1,
    1
  },
  {
    "mulh_call",
    "bla __mulh",
    (insn_gen_fn) gen_mulh_call,
    &operand_data[144],
    1,
    0,
    1,
    1
  },
  {
    "mull_call",
    "bla __mull",
    (insn_gen_fn) gen_mull_call,
    &operand_data[144],
    1,
    0,
    1,
    1
  },
  {
    "divss_call",
    "bla __divss",
    (insn_gen_fn) gen_divss_call,
    &operand_data[144],
    1,
    0,
    1,
    1
  },
  {
    "divus_call",
    "bla __divus",
    (insn_gen_fn) gen_divus_call,
    &operand_data[144],
    2,
    0,
    1,
    1
  },
  {
    "quoss_call",
    "bla __quoss",
    (insn_gen_fn) gen_quoss_call,
    &operand_data[144],
    1,
    0,
    1,
    1
  },
  {
    "quous_call",
    "bla __quous",
    (insn_gen_fn) gen_quous_call,
    &operand_data[144],
    2,
    0,
    1,
    1
  },
  {
    "andsi3",
    (const PTR) output_80,
    (insn_gen_fn) gen_andsi3,
    &operand_data[146],
    4,
    0,
    4,
    2
  },
  {
    "*andsi3_internal2",
    (const PTR) output_81,
    0,
    &operand_data[150],
    5,
    0,
    8,
    2
  },
  {
    "*andsi3_internal3",
    (const PTR) output_82,
    0,
    &operand_data[150],
    5,
    0,
    8,
    2
  },
  {
    "*andsi3_internal4",
    (const PTR) output_83,
    0,
    &operand_data[155],
    5,
    2,
    8,
    2
  },
  {
    "*andsi3_internal5",
    (const PTR) output_84,
    0,
    &operand_data[155],
    5,
    2,
    8,
    2
  },
  {
    "*andsi3_internal6",
    "#",
    0,
    &operand_data[160],
    3,
    0,
    1,
    1
  },
  {
    "*andsi3_internal7",
    "#",
    0,
    &operand_data[163],
    4,
    0,
    2,
    1
  },
  {
    "*andsi3_internal8",
    "#",
    0,
    &operand_data[167],
    4,
    2,
    2,
    1
  },
  {
    "*boolsi3_internal1",
    (const PTR) output_88,
    0,
    &operand_data[171],
    4,
    0,
    3,
    2
  },
  {
    "*boolsi3_internal2",
    (const PTR) output_89,
    0,
    &operand_data[175],
    5,
    0,
    2,
    2
  },
  {
    "*boolsi3_internal3",
    (const PTR) output_90,
    0,
    &operand_data[180],
    5,
    1,
    2,
    2
  },
  {
    "*boolcsi3_internal1",
    "%q3 %0,%2,%1",
    0,
    &operand_data[185],
    4,
    0,
    1,
    1
  },
  {
    "*boolcsi3_internal2",
    (const PTR) output_92,
    0,
    &operand_data[189],
    5,
    0,
    2,
    2
  },
  {
    "*boolcsi3_internal3",
    (const PTR) output_93,
    0,
    &operand_data[180],
    5,
    1,
    2,
    2
  },
  {
    "*boolccsi3_internal1",
    "%q3 %0,%1,%2",
    0,
    &operand_data[185],
    4,
    0,
    1,
    1
  },
  {
    "*boolccsi3_internal2",
    (const PTR) output_95,
    0,
    &operand_data[189],
    5,
    0,
    2,
    2
  },
  {
    "*boolccsi3_internal3",
    (const PTR) output_96,
    0,
    &operand_data[180],
    5,
    1,
    2,
    2
  },
  {
    "*maskir_internal1",
    "maskir %0,%3,%2",
    0,
    &operand_data[194],
    4,
    1,
    1,
    1
  },
  {
    "*maskir_internal2",
    "maskir %0,%3,%2",
    0,
    &operand_data[194],
    4,
    1,
    1,
    1
  },
  {
    "*maskir_internal3",
    "maskir %0,%3,%2",
    0,
    &operand_data[194],
    4,
    1,
    1,
    1
  },
  {
    "*maskir_internal4",
    "maskir %0,%3,%2",
    0,
    &operand_data[194],
    4,
    1,
    1,
    1
  },
  {
    "*maskir_internal5",
    (const PTR) output_101,
    0,
    &operand_data[198],
    5,
    5,
    2,
    2
  },
  {
    "*maskir_internal6",
    (const PTR) output_102,
    0,
    &operand_data[198],
    5,
    5,
    2,
    2
  },
  {
    "*maskir_internal7",
    (const PTR) output_103,
    0,
    &operand_data[198],
    5,
    5,
    2,
    2
  },
  {
    "*maskir_internal8",
    (const PTR) output_104,
    0,
    &operand_data[198],
    5,
    5,
    2,
    2
  },
  {
    "insvsi",
    (const PTR) output_105,
    (insn_gen_fn) gen_insvsi,
    &operand_data[203],
    4,
    0,
    1,
    3
  },
  {
    "*insvsi_internal1",
    (const PTR) output_106,
    0,
    &operand_data[203],
    5,
    0,
    1,
    3
  },
  {
    "*insvsi_internal2",
    (const PTR) output_107,
    0,
    &operand_data[203],
    5,
    0,
    1,
    3
  },
  {
    "*insvsi_internal3",
    (const PTR) output_108,
    0,
    &operand_data[203],
    5,
    0,
    1,
    3
  },
  {
    "*insvsi_internal4",
    (const PTR) output_109,
    0,
    &operand_data[203],
    6,
    0,
    1,
    3
  },
  {
    "insvdi",
    (const PTR) output_110,
    (insn_gen_fn) gen_insvdi,
    &operand_data[209],
    4,
    0,
    1,
    3
  },
  {
    "extzvsi",
    (const PTR) output_111,
    (insn_gen_fn) gen_extzvsi,
    &operand_data[213],
    4,
    0,
    1,
    3
  },
  {
    "*extzvsi_internal1",
    (const PTR) output_112,
    0,
    &operand_data[217],
    5,
    0,
    2,
    3
  },
  {
    "*extzvsi_internal2",
    (const PTR) output_113,
    0,
    &operand_data[222],
    5,
    3,
    2,
    3
  },
  {
    "extzvdi",
    (const PTR) output_114,
    (insn_gen_fn) gen_extzvdi,
    &operand_data[227],
    4,
    0,
    1,
    3
  },
  {
    "*extzvdi_internal1",
    (const PTR) output_115,
    0,
    &operand_data[231],
    5,
    0,
    1,
    3
  },
  {
    "*extzvdi_internal2",
    (const PTR) output_116,
    0,
    &operand_data[227],
    5,
    3,
    1,
    3
  },
  {
    "rotlsi3",
    "{rl%I2nm|rlw%I2nm} %0,%1,%h2,0xffffffff",
    (insn_gen_fn) gen_rotlsi3,
    &operand_data[236],
    3,
    0,
    1,
    1
  },
  {
    "*rotlsi3_internal2",
    (const PTR) output_118,
    0,
    &operand_data[239],
    4,
    0,
    2,
    2
  },
  {
    "*rotlsi3_internal3",
    (const PTR) output_119,
    0,
    &operand_data[243],
    4,
    2,
    2,
    2
  },
  {
    "*rotlsi3_internal4",
    "{rl%I2nm|rlw%I2nm} %0,%1,%h2,%m3,%M3",
    0,
    &operand_data[247],
    4,
    0,
    1,
    1
  },
  {
    "*rotlsi3_internal5",
    (const PTR) output_121,
    0,
    &operand_data[251],
    5,
    0,
    2,
    2
  },
  {
    "*rotlsi3_internal6",
    (const PTR) output_122,
    0,
    &operand_data[256],
    5,
    3,
    2,
    2
  },
  {
    "*rotlsi3_internal7",
    "{rl%I2nm|rlw%I2nm} %0,%1,%h2,0xff",
    0,
    &operand_data[236],
    3,
    0,
    1,
    1
  },
  {
    "*rotlsi3_internal8",
    (const PTR) output_124,
    0,
    &operand_data[239],
    4,
    0,
    2,
    2
  },
  {
    "*rotlsi3_internal9",
    (const PTR) output_125,
    0,
    &operand_data[243],
    4,
    2,
    2,
    2
  },
  {
    "*rotlsi3_internal10",
    "{rl%I2nm|rlw%I2nm} %0,%1,%h2,0xffff",
    0,
    &operand_data[236],
    3,
    0,
    1,
    1
  },
  {
    "*rotlsi3_internal11",
    (const PTR) output_127,
    0,
    &operand_data[239],
    4,
    0,
    2,
    2
  },
  {
    "*rotlsi3_internal12",
    (const PTR) output_128,
    0,
    &operand_data[243],
    4,
    2,
    2,
    2
  },
  {
    "ashlsi3_power",
    (const PTR) output_129,
    (insn_gen_fn) gen_ashlsi3_power,
    &operand_data[261],
    4,
    0,
    2,
    2
  },
  {
    "ashlsi3_no_power",
    "{sl|slw}%I2 %0,%1,%h2",
    (insn_gen_fn) gen_ashlsi3_no_power,
    &operand_data[236],
    3,
    0,
    1,
    1
  },
  {
    "*rs6000.md:4301",
    (const PTR) output_131,
    0,
    &operand_data[265],
    5,
    0,
    4,
    2
  },
  {
    "*rs6000.md:4333",
    (const PTR) output_132,
    0,
    &operand_data[239],
    4,
    0,
    2,
    2
  },
  {
    "*rs6000.md:4360",
    (const PTR) output_133,
    0,
    &operand_data[270],
    5,
    2,
    4,
    2
  },
  {
    "*rs6000.md:4394",
    (const PTR) output_134,
    0,
    &operand_data[243],
    4,
    2,
    2,
    2
  },
  {
    "*rs6000.md:4423",
    "{rlinm|rlwinm} %0,%1,%h2,%m3,%M3",
    0,
    &operand_data[275],
    4,
    0,
    1,
    1
  },
  {
    "*rs6000.md:4431",
    (const PTR) output_136,
    0,
    &operand_data[279],
    5,
    0,
    2,
    2
  },
  {
    "*rs6000.md:4463",
    (const PTR) output_137,
    0,
    &operand_data[284],
    5,
    3,
    2,
    2
  },
  {
    "lshrsi3_power",
    (const PTR) output_138,
    (insn_gen_fn) gen_lshrsi3_power,
    &operand_data[289],
    4,
    0,
    3,
    2
  },
  {
    "lshrsi3_no_power",
    (const PTR) output_139,
    (insn_gen_fn) gen_lshrsi3_no_power,
    &operand_data[293],
    3,
    0,
    2,
    2
  },
  {
    "*rs6000.md:4532",
    (const PTR) output_140,
    0,
    &operand_data[296],
    5,
    0,
    6,
    2
  },
  {
    "*rs6000.md:4566",
    (const PTR) output_141,
    0,
    &operand_data[301],
    4,
    0,
    4,
    2
  },
  {
    "*rs6000.md:4595",
    (const PTR) output_142,
    0,
    &operand_data[305],
    5,
    2,
    6,
    2
  },
  {
    "*rs6000.md:4631",
    (const PTR) output_143,
    0,
    &operand_data[310],
    4,
    2,
    4,
    2
  },
  {
    "*rs6000.md:4662",
    "{rlinm|rlwinm} %0,%1,%s2,%m3,%M3",
    0,
    &operand_data[275],
    4,
    0,
    1,
    1
  },
  {
    "*rs6000.md:4670",
    (const PTR) output_145,
    0,
    &operand_data[279],
    5,
    0,
    2,
    2
  },
  {
    "*rs6000.md:4702",
    (const PTR) output_146,
    0,
    &operand_data[284],
    5,
    3,
    2,
    2
  },
  {
    "*rs6000.md:4735",
    "{rlinm|rlwinm} %0,%1,%s2,0xff",
    0,
    &operand_data[213],
    3,
    0,
    1,
    1
  },
  {
    "*rs6000.md:4744",
    (const PTR) output_148,
    0,
    &operand_data[314],
    4,
    0,
    2,
    2
  },
  {
    "*rs6000.md:4779",
    (const PTR) output_149,
    0,
    &operand_data[318],
    4,
    2,
    2,
    2
  },
  {
    "*rs6000.md:4814",
    "{rlinm|rlwinm} %0,%1,%s2,0xffff",
    0,
    &operand_data[213],
    3,
    0,
    1,
    1
  },
  {
    "*rs6000.md:4823",
    (const PTR) output_151,
    0,
    &operand_data[314],
    4,
    0,
    2,
    2
  },
  {
    "*rs6000.md:4858",
    (const PTR) output_152,
    0,
    &operand_data[318],
    4,
    2,
    2,
    2
  },
  {
    "*rs6000.md:4893",
    "rrib %0,%1,%2",
    0,
    &operand_data[322],
    3,
    0,
    1,
    1
  },
  {
    "*rs6000.md:4902",
    "rrib %0,%1,%2",
    0,
    &operand_data[322],
    3,
    0,
    1,
    1
  },
  {
    "*rs6000.md:4911",
    "rrib %0,%1,%2",
    0,
    &operand_data[322],
    3,
    0,
    1,
    1
  },
  {
    "ashrsi3_power",
    (const PTR) output_156,
    (insn_gen_fn) gen_ashrsi3_power,
    &operand_data[261],
    4,
    0,
    2,
    2
  },
  {
    "ashrsi3_no_power",
    "{sra|sraw}%I2 %0,%1,%h2",
    (insn_gen_fn) gen_ashrsi3_no_power,
    &operand_data[236],
    3,
    0,
    1,
    1
  },
  {
    "*rs6000.md:4952",
    (const PTR) output_158,
    0,
    &operand_data[265],
    5,
    0,
    4,
    2
  },
  {
    "*rs6000.md:4984",
    (const PTR) output_159,
    0,
    &operand_data[239],
    4,
    0,
    2,
    2
  },
  {
    "*rs6000.md:5011",
    (const PTR) output_160,
    0,
    &operand_data[270],
    5,
    2,
    4,
    2
  },
  {
    "*rs6000.md:5045",
    (const PTR) output_161,
    0,
    &operand_data[243],
    4,
    2,
    2,
    2
  },
  {
    "extendsfdf2",
    (const PTR) output_162,
    (insn_gen_fn) gen_extendsfdf2,
    &operand_data[325],
    2,
    0,
    1,
    3
  },
  {
    "truncdfsf2",
    "frsp %0,%1",
    (insn_gen_fn) gen_truncdfsf2,
    &operand_data[327],
    2,
    0,
    1,
    1
  },
  {
    "aux_truncdfsf2",
    "frsp %0,%1",
    (insn_gen_fn) gen_aux_truncdfsf2,
    &operand_data[329],
    2,
    0,
    1,
    1
  },
  {
    "*negsf2",
    "fneg %0,%1",
    0,
    &operand_data[329],
    2,
    0,
    1,
    1
  },
  {
    "*abssf2",
    "fabs %0,%1",
    0,
    &operand_data[329],
    2,
    0,
    1,
    1
  },
  {
    "*rs6000.md:5143",
    "fnabs %0,%1",
    0,
    &operand_data[329],
    2,
    0,
    1,
    1
  },
  {
    "*rs6000.md:5157",
    "fadds %0,%1,%2",
    0,
    &operand_data[331],
    3,
    0,
    1,
    1
  },
  {
    "*rs6000.md:5165",
    "{fa|fadd} %0,%1,%2",
    0,
    &operand_data[331],
    3,
    0,
    1,
    1
  },
  {
    "*rs6000.md:5180",
    "fsubs %0,%1,%2",
    0,
    &operand_data[334],
    3,
    0,
    1,
    1
  },
  {
    "*rs6000.md:5188",
    "{fs|fsub} %0,%1,%2",
    0,
    &operand_data[334],
    3,
    0,
    1,
    1
  },
  {
    "*rs6000.md:5203",
    "fmuls %0,%1,%2",
    0,
    &operand_data[331],
    3,
    0,
    1,
    1
  },
  {
    "*rs6000.md:5211",
    "{fm|fmul} %0,%1,%2",
    0,
    &operand_data[331],
    3,
    0,
    1,
    1
  },
  {
    "*rs6000.md:5226",
    "fdivs %0,%1,%2",
    0,
    &operand_data[334],
    3,
    0,
    1,
    1
  },
  {
    "*rs6000.md:5234",
    "{fd|fdiv} %0,%1,%2",
    0,
    &operand_data[334],
    3,
    0,
    1,
    1
  },
  {
    "*rs6000.md:5242",
    "fmadds %0,%1,%2,%3",
    0,
    &operand_data[337],
    4,
    0,
    1,
    1
  },
  {
    "*rs6000.md:5251",
    "{fma|fmadd} %0,%1,%2,%3",
    0,
    &operand_data[337],
    4,
    0,
    1,
    1
  },
  {
    "*rs6000.md:5260",
    "fmsubs %0,%1,%2,%3",
    0,
    &operand_data[337],
    4,
    0,
    1,
    1
  },
  {
    "*rs6000.md:5269",
    "{fms|fmsub} %0,%1,%2,%3",
    0,
    &operand_data[337],
    4,
    0,
    1,
    1
  },
  {
    "*rs6000.md:5278",
    "fnmadds %0,%1,%2,%3",
    0,
    &operand_data[337],
    4,
    0,
    1,
    1
  },
  {
    "*rs6000.md:5288",
    "fnmadds %0,%1,%2,%3",
    0,
    &operand_data[341],
    4,
    0,
    1,
    1
  },
  {
    "*rs6000.md:5298",
    "{fnma|fnmadd} %0,%1,%2,%3",
    0,
    &operand_data[337],
    4,
    0,
    1,
    1
  },
  {
    "*rs6000.md:5307",
    "{fnma|fnmadd} %0,%1,%2,%3",
    0,
    &operand_data[341],
    4,
    0,
    1,
    1
  },
  {
    "*rs6000.md:5317",
    "fnmsubs %0,%1,%2,%3",
    0,
    &operand_data[337],
    4,
    0,
    1,
    1
  },
  {
    "*rs6000.md:5327",
    "fnmsubs %0,%1,%2,%3",
    0,
    &operand_data[337],
    4,
    0,
    1,
    1
  },
  {
    "*rs6000.md:5337",
    "{fnms|fnmsub} %0,%1,%2,%3",
    0,
    &operand_data[337],
    4,
    0,
    1,
    1
  },
  {
    "*rs6000.md:5346",
    "{fnms|fnmsub} %0,%1,%2,%3",
    0,
    &operand_data[337],
    4,
    0,
    1,
    1
  },
  {
    "*rs6000.md:5362",
    "fsqrts %0,%1",
    0,
    &operand_data[329],
    2,
    0,
    1,
    1
  },
  {
    "*rs6000.md:5369",
    "fsqrt %0,%1",
    0,
    &operand_data[329],
    2,
    0,
    1,
    1
  },
  {
    "*fselsfsf4",
    "fsel %0,%1,%2,%3",
    0,
    &operand_data[341],
    5,
    0,
    1,
    1
  },
  {
    "*fseldfsf4",
    "fsel %0,%1,%2,%3",
    0,
    &operand_data[346],
    5,
    0,
    1,
    1
  },
  {
    "negdf2",
    "fneg %0,%1",
    (insn_gen_fn) gen_negdf2,
    &operand_data[351],
    2,
    0,
    1,
    1
  },
  {
    "absdf2",
    "fabs %0,%1",
    (insn_gen_fn) gen_absdf2,
    &operand_data[351],
    2,
    0,
    1,
    1
  },
  {
    "*rs6000.md:5507",
    "fnabs %0,%1",
    0,
    &operand_data[351],
    2,
    0,
    1,
    1
  },
  {
    "adddf3",
    "{fa|fadd} %0,%1,%2",
    (insn_gen_fn) gen_adddf3,
    &operand_data[353],
    3,
    0,
    1,
    1
  },
  {
    "subdf3",
    "{fs|fsub} %0,%1,%2",
    (insn_gen_fn) gen_subdf3,
    &operand_data[356],
    3,
    0,
    1,
    1
  },
  {
    "muldf3",
    "{fm|fmul} %0,%1,%2",
    (insn_gen_fn) gen_muldf3,
    &operand_data[353],
    3,
    0,
    1,
    1
  },
  {
    "divdf3",
    "{fd|fdiv} %0,%1,%2",
    (insn_gen_fn) gen_divdf3,
    &operand_data[356],
    3,
    0,
    1,
    1
  },
  {
    "*rs6000.md:5546",
    "{fma|fmadd} %0,%1,%2,%3",
    0,
    &operand_data[359],
    4,
    0,
    1,
    1
  },
  {
    "*rs6000.md:5555",
    "{fms|fmsub} %0,%1,%2,%3",
    0,
    &operand_data[359],
    4,
    0,
    1,
    1
  },
  {
    "*rs6000.md:5564",
    "{fnma|fnmadd} %0,%1,%2,%3",
    0,
    &operand_data[359],
    4,
    0,
    1,
    1
  },
  {
    "*rs6000.md:5574",
    "{fnma|fnmadd} %0,%1,%2,%3",
    0,
    &operand_data[363],
    4,
    0,
    1,
    1
  },
  {
    "*rs6000.md:5584",
    "{fnms|fnmsub} %0,%1,%2,%3",
    0,
    &operand_data[359],
    4,
    0,
    1,
    1
  },
  {
    "*rs6000.md:5594",
    "{fnms|fnmsub} %0,%1,%2,%3",
    0,
    &operand_data[359],
    4,
    0,
    1,
    1
  },
  {
    "sqrtdf2",
    "fsqrt %0,%1",
    (insn_gen_fn) gen_sqrtdf2,
    &operand_data[351],
    2,
    0,
    1,
    1
  },
  {
    "*fseldfdf4",
    "fsel %0,%1,%2,%3",
    0,
    &operand_data[363],
    5,
    0,
    1,
    1
  },
  {
    "*fselsfdf4",
    "fsel %0,%1,%2,%3",
    0,
    &operand_data[368],
    5,
    0,
    1,
    1
  },
  {
    "*floatsidf2_internal",
    "#",
    0,
    &operand_data[373],
    7,
    0,
    1,
    1
  },
  {
    "*floatunssidf2_internal",
    "#",
    0,
    &operand_data[373],
    6,
    0,
    1,
    1
  },
  {
    "*fix_truncdfsi2_internal",
    "#",
    0,
    &operand_data[379],
    4,
    0,
    1,
    1
  },
  {
    "fctiwz",
    "{fcirz|fctiwz} %0,%1",
    (insn_gen_fn) gen_fctiwz,
    &operand_data[383],
    2,
    0,
    1,
    1
  },
  {
    "floatdidf2",
    "fcfid %0,%1",
    (insn_gen_fn) gen_floatdidf2,
    &operand_data[385],
    2,
    0,
    1,
    1
  },
  {
    "floatsidf_ppc64",
    "#",
    (insn_gen_fn) gen_floatsidf_ppc64,
    &operand_data[387],
    5,
    0,
    1,
    1
  },
  {
    "floatunssidf_ppc64",
    "#",
    (insn_gen_fn) gen_floatunssidf_ppc64,
    &operand_data[387],
    5,
    0,
    1,
    1
  },
  {
    "fix_truncdfdi2",
    "fctidz %0,%1",
    (insn_gen_fn) gen_fix_truncdfdi2,
    &operand_data[383],
    2,
    0,
    1,
    1
  },
  {
    "floatdisf2_internal1",
    "#",
    (insn_gen_fn) gen_floatdisf2_internal1,
    &operand_data[392],
    3,
    0,
    1,
    1
  },
  {
    "*adddi3_noppc64",
    (const PTR) output_217,
    0,
    &operand_data[395],
    3,
    0,
    4,
    3
  },
  {
    "*subdi3_noppc64",
    (const PTR) output_218,
    0,
    &operand_data[398],
    3,
    0,
    5,
    3
  },
  {
    "*negdi2_noppc64",
    (const PTR) output_219,
    0,
    &operand_data[401],
    2,
    0,
    2,
    3
  },
  {
    "mulsidi3_mq",
    "mul %0,%1,%2\n\tmfmq %L0",
    (insn_gen_fn) gen_mulsidi3_mq,
    &operand_data[403],
    4,
    0,
    1,
    1
  },
  {
    "*mulsidi3_no_mq",
    (const PTR) output_221,
    0,
    &operand_data[407],
    3,
    0,
    1,
    3
  },
  {
    "umulsidi3_mq",
    (const PTR) output_222,
    (insn_gen_fn) gen_umulsidi3_mq,
    &operand_data[407],
    4,
    0,
    1,
    3
  },
  {
    "*umulsidi3_no_mq",
    (const PTR) output_223,
    0,
    &operand_data[407],
    3,
    0,
    1,
    3
  },
  {
    "smulsi3_highpart_mq",
    "mul %0,%1,%2",
    (insn_gen_fn) gen_smulsi3_highpart_mq,
    &operand_data[411],
    4,
    0,
    1,
    1
  },
  {
    "*smulsi3_highpart_no_mq",
    "mulhw %0,%1,%2",
    0,
    &operand_data[411],
    3,
    0,
    1,
    1
  },
  {
    "umulsi3_highpart_mq",
    "mulhwu %0,%1,%2",
    (insn_gen_fn) gen_umulsi3_highpart_mq,
    &operand_data[411],
    4,
    0,
    1,
    1
  },
  {
    "*umulsi3_highpart_no_mq",
    "mulhwu %0,%1,%2",
    0,
    &operand_data[411],
    3,
    0,
    1,
    1
  },
  {
    "ashldi3_power",
    (const PTR) output_228,
    (insn_gen_fn) gen_ashldi3_power,
    &operand_data[415],
    4,
    0,
    4,
    2
  },
  {
    "lshrdi3_power",
    (const PTR) output_229,
    (insn_gen_fn) gen_lshrdi3_power,
    &operand_data[415],
    4,
    0,
    4,
    2
  },
  {
    "ashrdi3_power",
    (const PTR) output_230,
    (insn_gen_fn) gen_ashrdi3_power,
    &operand_data[419],
    4,
    0,
    2,
    2
  },
  {
    "ashrdi3_no_power",
    (const PTR) output_231,
    (insn_gen_fn) gen_ashrdi3_no_power,
    &operand_data[423],
    3,
    0,
    2,
    2
  },
  {
    "*adddi3_internal1",
    (const PTR) output_232,
    0,
    &operand_data[426],
    3,
    0,
    4,
    2
  },
  {
    "*adddi3_internal2",
    (const PTR) output_233,
    0,
    &operand_data[429],
    4,
    0,
    4,
    2
  },
  {
    "*adddi3_internal3",
    (const PTR) output_234,
    0,
    &operand_data[433],
    4,
    2,
    4,
    2
  },
  {
    "one_cmpldi2",
    "nor %0,%1,%1",
    (insn_gen_fn) gen_one_cmpldi2,
    &operand_data[227],
    2,
    0,
    1,
    1
  },
  {
    "*rs6000.md:6507",
    (const PTR) output_236,
    0,
    &operand_data[437],
    3,
    0,
    2,
    2
  },
  {
    "*rs6000.md:6532",
    (const PTR) output_237,
    0,
    &operand_data[440],
    3,
    1,
    2,
    2
  },
  {
    "*rs6000.md:6559",
    (const PTR) output_238,
    0,
    &operand_data[443],
    3,
    0,
    2,
    2
  },
  {
    "*rs6000.md:6568",
    (const PTR) output_239,
    0,
    &operand_data[446],
    4,
    0,
    2,
    2
  },
  {
    "*rs6000.md:6595",
    (const PTR) output_240,
    0,
    &operand_data[450],
    4,
    2,
    2,
    2
  },
  {
    "absdi2",
    "#",
    (insn_gen_fn) gen_absdi2,
    &operand_data[454],
    3,
    0,
    2,
    1
  },
  {
    "*nabsdi2",
    "#",
    0,
    &operand_data[454],
    3,
    0,
    2,
    1
  },
  {
    "*rs6000.md:6669",
    "neg %0,%1",
    0,
    &operand_data[227],
    2,
    0,
    1,
    1
  },
  {
    "*rs6000.md:6675",
    (const PTR) output_244,
    0,
    &operand_data[437],
    3,
    0,
    2,
    2
  },
  {
    "*rs6000.md:6700",
    (const PTR) output_245,
    0,
    &operand_data[440],
    3,
    1,
    2,
    2
  },
  {
    "ffsdi2",
    "neg %0,%1\n\tand %0,%0,%1\n\tcntlzd %0,%0\n\tsubfic %0,%0,64",
    (insn_gen_fn) gen_ffsdi2,
    &operand_data[457],
    2,
    0,
    1,
    1
  },
  {
    "muldi3",
    "mulld %0,%1,%2",
    (insn_gen_fn) gen_muldi3,
    &operand_data[459],
    3,
    0,
    1,
    1
  },
  {
    "smuldi3_highpart",
    "mulhd %0,%1,%2",
    (insn_gen_fn) gen_smuldi3_highpart,
    &operand_data[459],
    3,
    0,
    1,
    1
  },
  {
    "umuldi3_highpart",
    "mulhdu %0,%1,%2",
    (insn_gen_fn) gen_umuldi3_highpart,
    &operand_data[459],
    3,
    0,
    1,
    1
  },
  {
    "*rs6000.md:6806",
    "sradi %0,%1,%p2\n\taddze %0,%0",
    0,
    &operand_data[462],
    3,
    0,
    1,
    1
  },
  {
    "*rs6000.md:6814",
    (const PTR) output_251,
    0,
    &operand_data[465],
    4,
    0,
    2,
    2
  },
  {
    "*rs6000.md:6841",
    (const PTR) output_252,
    0,
    &operand_data[469],
    4,
    2,
    2,
    2
  },
  {
    "*rs6000.md:6870",
    "divd %0,%1,%2",
    0,
    &operand_data[473],
    3,
    0,
    1,
    1
  },
  {
    "udivdi3",
    "divdu %0,%1,%2",
    (insn_gen_fn) gen_udivdi3,
    &operand_data[473],
    3,
    0,
    1,
    1
  },
  {
    "rotldi3",
    "rld%I2cl %0,%1,%H2,0",
    (insn_gen_fn) gen_rotldi3,
    &operand_data[476],
    3,
    0,
    1,
    1
  },
  {
    "*rotldi3_internal2",
    (const PTR) output_256,
    0,
    &operand_data[479],
    4,
    0,
    2,
    2
  },
  {
    "*rotldi3_internal3",
    (const PTR) output_257,
    0,
    &operand_data[483],
    4,
    2,
    2,
    2
  },
  {
    "*rotldi3_internal4",
    "rld%I2c%B3 %0,%1,%H2,%S3",
    0,
    &operand_data[487],
    4,
    0,
    1,
    1
  },
  {
    "*rotldi3_internal5",
    (const PTR) output_259,
    0,
    &operand_data[491],
    5,
    0,
    2,
    2
  },
  {
    "*rotldi3_internal6",
    (const PTR) output_260,
    0,
    &operand_data[496],
    5,
    3,
    2,
    2
  },
  {
    "*rotldi3_internal7",
    "rld%I2cl %0,%1,%H2,56",
    0,
    &operand_data[476],
    3,
    0,
    1,
    1
  },
  {
    "*rotldi3_internal8",
    (const PTR) output_262,
    0,
    &operand_data[479],
    4,
    0,
    2,
    2
  },
  {
    "*rotldi3_internal9",
    (const PTR) output_263,
    0,
    &operand_data[483],
    4,
    2,
    2,
    2
  },
  {
    "*rotldi3_internal10",
    "rld%I2cl %0,%1,%H2,48",
    0,
    &operand_data[476],
    3,
    0,
    1,
    1
  },
  {
    "*rotldi3_internal11",
    (const PTR) output_265,
    0,
    &operand_data[479],
    4,
    0,
    2,
    2
  },
  {
    "*rotldi3_internal12",
    (const PTR) output_266,
    0,
    &operand_data[483],
    4,
    2,
    2,
    2
  },
  {
    "*rotldi3_internal13",
    "rld%I2cl %0,%1,%H2,32",
    0,
    &operand_data[476],
    3,
    0,
    1,
    1
  },
  {
    "*rotldi3_internal14",
    (const PTR) output_268,
    0,
    &operand_data[479],
    4,
    0,
    2,
    2
  },
  {
    "*rotldi3_internal15",
    (const PTR) output_269,
    0,
    &operand_data[483],
    4,
    2,
    2,
    2
  },
  {
    "*ashldi3_internal1",
    "sld%I2 %0,%1,%H2",
    0,
    &operand_data[501],
    3,
    0,
    1,
    1
  },
  {
    "*ashldi3_internal2",
    (const PTR) output_271,
    0,
    &operand_data[504],
    4,
    0,
    2,
    2
  },
  {
    "*ashldi3_internal3",
    (const PTR) output_272,
    0,
    &operand_data[508],
    4,
    2,
    2,
    2
  },
  {
    "*ashldi3_internal4",
    "rldic %0,%1,%H2,%W3",
    0,
    &operand_data[512],
    4,
    0,
    1,
    1
  },
  {
    "ashldi3_internal5",
    (const PTR) output_274,
    (insn_gen_fn) gen_ashldi3_internal5,
    &operand_data[516],
    5,
    0,
    2,
    2
  },
  {
    "*ashldi3_internal6",
    (const PTR) output_275,
    0,
    &operand_data[521],
    5,
    3,
    2,
    2
  },
  {
    "*ashldi3_internal7",
    "rldicr %0,%1,%H2,%S3",
    0,
    &operand_data[526],
    4,
    0,
    1,
    1
  },
  {
    "ashldi3_internal8",
    (const PTR) output_277,
    (insn_gen_fn) gen_ashldi3_internal8,
    &operand_data[530],
    5,
    0,
    2,
    2
  },
  {
    "*ashldi3_internal9",
    (const PTR) output_278,
    0,
    &operand_data[535],
    5,
    3,
    2,
    2
  },
  {
    "*lshrdi3_internal1",
    "srd%I2 %0,%1,%H2",
    0,
    &operand_data[501],
    3,
    0,
    1,
    1
  },
  {
    "*lshrdi3_internal2",
    (const PTR) output_280,
    0,
    &operand_data[504],
    4,
    0,
    2,
    2
  },
  {
    "*lshrdi3_internal3",
    (const PTR) output_281,
    0,
    &operand_data[508],
    4,
    2,
    2,
    2
  },
  {
    "*ashrdi3_internal1",
    "srad%I2 %0,%1,%H2",
    0,
    &operand_data[501],
    3,
    0,
    1,
    1
  },
  {
    "*ashrdi3_internal2",
    (const PTR) output_283,
    0,
    &operand_data[504],
    4,
    0,
    2,
    2
  },
  {
    "*ashrdi3_internal3",
    (const PTR) output_284,
    0,
    &operand_data[508],
    4,
    2,
    2,
    2
  },
  {
    "anddi3",
    (const PTR) output_285,
    (insn_gen_fn) gen_anddi3,
    &operand_data[540],
    4,
    0,
    5,
    2
  },
  {
    "*anddi3_internal2",
    (const PTR) output_286,
    0,
    &operand_data[544],
    5,
    0,
    10,
    2
  },
  {
    "*anddi3_internal3",
    (const PTR) output_287,
    0,
    &operand_data[549],
    5,
    2,
    10,
    2
  },
  {
    "*booldi3_internal1",
    (const PTR) output_288,
    0,
    &operand_data[554],
    4,
    0,
    3,
    2
  },
  {
    "*booldi3_internal2",
    (const PTR) output_289,
    0,
    &operand_data[558],
    5,
    0,
    2,
    2
  },
  {
    "*booldi3_internal3",
    (const PTR) output_290,
    0,
    &operand_data[563],
    5,
    1,
    2,
    2
  },
  {
    "*boolcdi3_internal1",
    "%q3 %0,%2,%1",
    0,
    &operand_data[568],
    4,
    0,
    1,
    1
  },
  {
    "*boolcdi3_internal2",
    (const PTR) output_292,
    0,
    &operand_data[572],
    5,
    0,
    2,
    2
  },
  {
    "*boolcdi3_internal3",
    (const PTR) output_293,
    0,
    &operand_data[563],
    5,
    1,
    2,
    2
  },
  {
    "*boolccdi3_internal1",
    "%q3 %0,%1,%2",
    0,
    &operand_data[568],
    4,
    0,
    1,
    1
  },
  {
    "*boolccdi3_internal2",
    (const PTR) output_295,
    0,
    &operand_data[572],
    5,
    0,
    2,
    2
  },
  {
    "*boolccdi3_internal3",
    (const PTR) output_296,
    0,
    &operand_data[563],
    5,
    1,
    2,
    2
  },
  {
    "elf_high",
    "{liu|lis} %0,%1@ha",
    (insn_gen_fn) gen_elf_high,
    &operand_data[577],
    2,
    0,
    1,
    1
  },
  {
    "elf_low",
    (const PTR) output_298,
    (insn_gen_fn) gen_elf_low,
    &operand_data[579],
    3,
    0,
    2,
    2
  },
  {
    "*movsi_got_internal",
    "{l|lwz} %0,%a1@got(%2)",
    0,
    &operand_data[582],
    3,
    0,
    1,
    1
  },
  {
    "*movsi_internal1",
    (const PTR) output_300,
    0,
    &operand_data[585],
    2,
    0,
    13,
    2
  },
  {
    "*movsi_internal2",
    (const PTR) output_301,
    0,
    &operand_data[73],
    3,
    1,
    2,
    2
  },
  {
    "*movhi_internal",
    (const PTR) output_302,
    0,
    &operand_data[587],
    2,
    0,
    8,
    2
  },
  {
    "*movqi_internal",
    (const PTR) output_303,
    0,
    &operand_data[589],
    2,
    0,
    8,
    2
  },
  {
    "*movcc_internal1",
    (const PTR) output_304,
    0,
    &operand_data[591],
    2,
    0,
    11,
    2
  },
  {
    "*movsf_hardfloat",
    (const PTR) output_305,
    0,
    &operand_data[593],
    2,
    0,
    11,
    2
  },
  {
    "*movsf_softfloat",
    (const PTR) output_306,
    0,
    &operand_data[595],
    2,
    0,
    11,
    2
  },
  {
    "*movdf_hardfloat32",
    (const PTR) output_307,
    0,
    &operand_data[597],
    2,
    0,
    9,
    3
  },
  {
    "*movdf_softfloat32",
    (const PTR) output_308,
    0,
    &operand_data[599],
    2,
    0,
    6,
    3
  },
  {
    "*movdf_hardfloat64",
    (const PTR) output_309,
    0,
    &operand_data[601],
    2,
    0,
    11,
    2
  },
  {
    "*movdf_softfloat64",
    (const PTR) output_310,
    0,
    &operand_data[603],
    2,
    0,
    8,
    2
  },
  {
    "*movtf_internal",
    (const PTR) output_311,
    0,
    &operand_data[605],
    2,
    0,
    6,
    3
  },
  {
    "extenddftf2",
    (const PTR) output_312,
    (insn_gen_fn) gen_extenddftf2,
    &operand_data[607],
    2,
    0,
    1,
    3
  },
  {
    "extendsftf2",
    (const PTR) output_313,
    (insn_gen_fn) gen_extendsftf2,
    &operand_data[609],
    2,
    0,
    1,
    3
  },
  {
    "trunctfdf2",
    "fadd %0,%1,%L1",
    (insn_gen_fn) gen_trunctfdf2,
    &operand_data[611],
    2,
    0,
    1,
    1
  },
  {
    "trunctfsf2",
    "#",
    (insn_gen_fn) gen_trunctfsf2,
    &operand_data[613],
    3,
    0,
    1,
    1
  },
  {
    "floatditf2",
    "#",
    (insn_gen_fn) gen_floatditf2,
    &operand_data[616],
    3,
    0,
    1,
    1
  },
  {
    "floatsitf2",
    "#",
    (insn_gen_fn) gen_floatsitf2,
    &operand_data[619],
    3,
    0,
    1,
    1
  },
  {
    "fix_trunctfdi2",
    "#",
    (insn_gen_fn) gen_fix_trunctfdi2,
    &operand_data[622],
    3,
    0,
    1,
    1
  },
  {
    "fix_trunctfsi2",
    "#",
    (insn_gen_fn) gen_fix_trunctfsi2,
    &operand_data[625],
    3,
    0,
    1,
    1
  },
  {
    "negtf2",
    (const PTR) output_320,
    (insn_gen_fn) gen_negtf2,
    &operand_data[628],
    2,
    0,
    1,
    3
  },
  {
    "abstf2",
    (const PTR) output_321,
    (insn_gen_fn) gen_abstf2,
    &operand_data[628],
    2,
    0,
    1,
    3
  },
  {
    "*rs6000.md:9027",
    (const PTR) output_322,
    0,
    &operand_data[628],
    2,
    0,
    1,
    3
  },
  {
    "*movdi_internal32",
    (const PTR) output_323,
    0,
    &operand_data[630],
    2,
    0,
    11,
    3
  },
  {
    "*movdi_internal64",
    (const PTR) output_324,
    0,
    &operand_data[632],
    2,
    0,
    13,
    2
  },
  {
    "*movdi_internal2",
    (const PTR) output_325,
    0,
    &operand_data[440],
    3,
    1,
    2,
    2
  },
  {
    "*movti_power",
    (const PTR) output_326,
    0,
    &operand_data[634],
    3,
    0,
    5,
    3
  },
  {
    "*movti_string",
    (const PTR) output_327,
    0,
    &operand_data[634],
    2,
    0,
    5,
    3
  },
  {
    "*movti_ppc64",
    (const PTR) output_328,
    0,
    &operand_data[637],
    2,
    0,
    3,
    3
  },
  {
    "*ldmsi8",
    (const PTR) output_329,
    0,
    &operand_data[639],
    10,
    7,
    1,
    3
  },
  {
    "*ldmsi7",
    (const PTR) output_330,
    0,
    &operand_data[639],
    9,
    6,
    1,
    3
  },
  {
    "*ldmsi6",
    (const PTR) output_331,
    0,
    &operand_data[639],
    8,
    5,
    1,
    3
  },
  {
    "*ldmsi5",
    (const PTR) output_332,
    0,
    &operand_data[639],
    7,
    4,
    1,
    3
  },
  {
    "*ldmsi4",
    (const PTR) output_333,
    0,
    &operand_data[639],
    6,
    3,
    1,
    3
  },
  {
    "*ldmsi3",
    (const PTR) output_334,
    0,
    &operand_data[639],
    5,
    2,
    1,
    3
  },
  {
    "*store_multiple_power",
    "{stsi|stswi} %2,%P1,%O0",
    0,
    &operand_data[649],
    4,
    0,
    1,
    1
  },
  {
    "*store_multiple_string",
    "{stsi|stswi} %2,%1,%O0",
    0,
    &operand_data[653],
    4,
    0,
    1,
    1
  },
  {
    "*rs6000.md:9680",
    "{lsi|lswi} %4,%1,%2\n\t{stsi|stswi} %4,%0,%2",
    0,
    &operand_data[657],
    6,
    0,
    1,
    1
  },
  {
    "*rs6000.md:9704",
    "{lsi|lswi} %4,%1,%2\n\t{stsi|stswi} %4,%0,%2",
    0,
    &operand_data[663],
    6,
    0,
    1,
    1
  },
  {
    "*rs6000.md:9728",
    "{lsi|lswi} %4,%1,%2\n\t{stsi|stswi} %4,%0,%2",
    0,
    &operand_data[669],
    6,
    0,
    1,
    1
  },
  {
    "*rs6000.md:9770",
    "{lsi|lswi} %4,%1,%2\n\t{stsi|stswi} %4,%0,%2",
    0,
    &operand_data[657],
    6,
    0,
    1,
    1
  },
  {
    "*rs6000.md:9791",
    "{lsi|lswi} %4,%1,%2\n\t{stsi|stswi} %4,%0,%2",
    0,
    &operand_data[663],
    6,
    0,
    1,
    1
  },
  {
    "*rs6000.md:9812",
    "{lsi|lswi} %4,%1,%2\n\t{stsi|stswi} %4,%0,%2",
    0,
    &operand_data[669],
    6,
    0,
    1,
    1
  },
  {
    "*rs6000.md:9849",
    "{lsi|lswi} %4,%1,%2\n\t{stsi|stswi} %4,%0,%2",
    0,
    &operand_data[657],
    6,
    0,
    1,
    1
  },
  {
    "*rs6000.md:9868",
    "{lsi|lswi} %4,%1,%2\n\t{stsi|stswi} %4,%0,%2",
    0,
    &operand_data[663],
    6,
    0,
    1,
    1
  },
  {
    "*rs6000.md:9887",
    "{lsi|lswi} %4,%1,%2\n\t{stsi|stswi} %4,%0,%2",
    0,
    &operand_data[669],
    6,
    0,
    1,
    1
  },
  {
    "*rs6000.md:9917",
    "{lsi|lswi} %4,%1,%2\n\t{stsi|stswi} %4,%0,%2",
    0,
    &operand_data[675],
    6,
    0,
    1,
    1
  },
  {
    "*rs6000.md:9930",
    "{lsi|lswi} %4,%1,%2\n\t{stsi|stswi} %4,%0,%2",
    0,
    &operand_data[681],
    6,
    0,
    1,
    1
  },
  {
    "*rs6000.md:9954",
    "{lsi|lswi} %4,%1,%2\n\t{stsi|stswi} %4,%0,%2",
    0,
    &operand_data[687],
    6,
    0,
    1,
    1
  },
  {
    "*rs6000.md:9967",
    "{lsi|lswi} %4,%1,%2\n\t{stsi|stswi} %4,%0,%2",
    0,
    &operand_data[693],
    6,
    0,
    1,
    1
  },
  {
    "*rs6000.md:9980",
    "{lsi|lswi} %4,%1,%2\n\t{stsi|stswi} %4,%0,%2",
    0,
    &operand_data[699],
    6,
    0,
    1,
    1
  },
  {
    "*movdi_update1",
    (const PTR) output_351,
    0,
    &operand_data[705],
    4,
    2,
    2,
    2
  },
  {
    "*movdi_update2",
    "lwaux %3,%0,%2",
    0,
    &operand_data[709],
    4,
    2,
    1,
    1
  },
  {
    "movdi_update",
    (const PTR) output_353,
    (insn_gen_fn) gen_movdi_update,
    &operand_data[713],
    4,
    2,
    2,
    2
  },
  {
    "*movsi_update1",
    (const PTR) output_354,
    0,
    &operand_data[717],
    4,
    2,
    2,
    2
  },
  {
    "movsi_update",
    (const PTR) output_355,
    (insn_gen_fn) gen_movsi_update,
    &operand_data[721],
    4,
    2,
    2,
    2
  },
  {
    "*movhi_update",
    (const PTR) output_356,
    0,
    &operand_data[725],
    4,
    2,
    2,
    2
  },
  {
    "*movhi_update2",
    (const PTR) output_357,
    0,
    &operand_data[717],
    4,
    2,
    2,
    2
  },
  {
    "*movhi_update3",
    (const PTR) output_358,
    0,
    &operand_data[717],
    4,
    2,
    2,
    2
  },
  {
    "*movhi_update4",
    (const PTR) output_359,
    0,
    &operand_data[729],
    4,
    2,
    2,
    2
  },
  {
    "*movqi_update1",
    (const PTR) output_360,
    0,
    &operand_data[733],
    4,
    2,
    2,
    2
  },
  {
    "*movqi_update2",
    (const PTR) output_361,
    0,
    &operand_data[717],
    4,
    2,
    2,
    2
  },
  {
    "*movqi_update3",
    (const PTR) output_362,
    0,
    &operand_data[737],
    4,
    2,
    2,
    2
  },
  {
    "*movsf_update1",
    (const PTR) output_363,
    0,
    &operand_data[741],
    4,
    2,
    2,
    2
  },
  {
    "*movsf_update2",
    (const PTR) output_364,
    0,
    &operand_data[745],
    4,
    2,
    2,
    2
  },
  {
    "*movsf_update3",
    (const PTR) output_365,
    0,
    &operand_data[749],
    4,
    2,
    2,
    2
  },
  {
    "*movsf_update4",
    (const PTR) output_366,
    0,
    &operand_data[753],
    4,
    2,
    2,
    2
  },
  {
    "*movdf_update1",
    (const PTR) output_367,
    0,
    &operand_data[757],
    4,
    2,
    2,
    2
  },
  {
    "*movdf_update2",
    (const PTR) output_368,
    0,
    &operand_data[761],
    4,
    2,
    2,
    2
  },
  {
    "load_toc_aix_si",
    (const PTR) output_369,
    (insn_gen_fn) gen_load_toc_aix_si,
    &operand_data[37],
    1,
    0,
    1,
    3
  },
  {
    "load_toc_aix_di",
    (const PTR) output_370,
    (insn_gen_fn) gen_load_toc_aix_di,
    &operand_data[9],
    1,
    0,
    1,
    3
  },
  {
    "load_toc_v4_pic_si",
    "bl _GLOBAL_OFFSET_TABLE_@local-4",
    (insn_gen_fn) gen_load_toc_v4_pic_si,
    &operand_data[765],
    1,
    0,
    1,
    1
  },
  {
    "load_toc_v4_PIC_1",
    "bcl 20,31,%1\n%1:",
    (insn_gen_fn) gen_load_toc_v4_PIC_1,
    &operand_data[765],
    2,
    1,
    1,
    1
  },
  {
    "load_toc_v4_PIC_1b",
    "bcl 20,31,%1\n\t.long %2-%1+4\n%1:",
    (insn_gen_fn) gen_load_toc_v4_PIC_1b,
    &operand_data[765],
    3,
    1,
    1,
    1
  },
  {
    "load_toc_v4_PIC_2",
    "{l|lwz} %0,%2-%3(%1)",
    (insn_gen_fn) gen_load_toc_v4_PIC_2,
    &operand_data[768],
    4,
    0,
    1,
    1
  },
  {
    "load_macho_picbase",
    "bcl 20,31,%1\n%1:",
    (insn_gen_fn) gen_load_macho_picbase,
    &operand_data[765],
    2,
    0,
    1,
    1
  },
  {
    "macho_correct_pic",
    "addis %0,%1,ha16(%2-%3)\n\taddi %1,%1,lo16(%2-%3)",
    (insn_gen_fn) gen_macho_correct_pic,
    &operand_data[772],
    4,
    0,
    1,
    1
  },
  {
    "*call_local32",
    (const PTR) output_377,
    0,
    &operand_data[776],
    4,
    0,
    2,
    3
  },
  {
    "*call_local64",
    (const PTR) output_378,
    0,
    &operand_data[780],
    4,
    0,
    2,
    3
  },
  {
    "*call_value_local32",
    (const PTR) output_379,
    0,
    &operand_data[784],
    5,
    0,
    2,
    3
  },
  {
    "*call_value_local64",
    (const PTR) output_380,
    0,
    &operand_data[789],
    5,
    0,
    2,
    3
  },
  {
    "*call_indirect_nonlocal_aix32",
    "b%T0l\n\t{l|lwz} 2,20(1)",
    0,
    &operand_data[794],
    3,
    0,
    1,
    1
  },
  {
    "*call_nonlocal_aix32",
    "bl %z0\n\t%.",
    0,
    &operand_data[797],
    4,
    0,
    1,
    1
  },
  {
    "*call_indirect_nonlocal_aix64",
    "b%T0l\n\tld 2,40(1)",
    0,
    &operand_data[801],
    3,
    0,
    1,
    1
  },
  {
    "*call_nonlocal_aix64",
    "bl %z0\n\t%.",
    0,
    &operand_data[804],
    4,
    0,
    1,
    1
  },
  {
    "*call_value_indirect_nonlocal_aix32",
    "b%T1l\n\t{l|lwz} 2,20(1)",
    0,
    &operand_data[808],
    4,
    0,
    1,
    1
  },
  {
    "*call_value_nonlocal_aix32",
    "bl %z1\n\t%.",
    0,
    &operand_data[812],
    5,
    0,
    1,
    1
  },
  {
    "*call_value_indirect_nonlocal_aix64",
    "b%T1l\n\tld 2,40(1)",
    0,
    &operand_data[817],
    4,
    0,
    1,
    1
  },
  {
    "*call_value_nonlocal_aix64",
    "bl %z1\n\t%.",
    0,
    &operand_data[821],
    5,
    0,
    1,
    1
  },
  {
    "*call_indirect_nonlocal_sysv",
    (const PTR) output_389,
    0,
    &operand_data[826],
    4,
    0,
    2,
    3
  },
  {
    "*call_nonlocal_sysv",
    (const PTR) output_390,
    0,
    &operand_data[830],
    4,
    0,
    2,
    3
  },
  {
    "*call_value_indirect_nonlocal_sysv",
    (const PTR) output_391,
    0,
    &operand_data[834],
    5,
    0,
    2,
    3
  },
  {
    "*call_value_nonlocal_sysv",
    (const PTR) output_392,
    0,
    &operand_data[839],
    5,
    0,
    2,
    3
  },
  {
    "*sibcall_local32",
    (const PTR) output_393,
    0,
    &operand_data[776],
    4,
    0,
    2,
    3
  },
  {
    "*sibcall_local64",
    (const PTR) output_394,
    0,
    &operand_data[780],
    4,
    0,
    2,
    3
  },
  {
    "*sibcall_value_local32",
    (const PTR) output_395,
    0,
    &operand_data[784],
    5,
    0,
    2,
    3
  },
  {
    "*sibcall_value_local64",
    (const PTR) output_396,
    0,
    &operand_data[789],
    5,
    0,
    2,
    3
  },
  {
    "*sibcall_nonlocal_aix32",
    "b %z0",
    0,
    &operand_data[797],
    4,
    0,
    1,
    1
  },
  {
    "*sibcall_nonlocal_aix64",
    "b %z0",
    0,
    &operand_data[804],
    4,
    0,
    1,
    1
  },
  {
    "*sibcall_value_nonlocal_aix32",
    "b %z1",
    0,
    &operand_data[812],
    5,
    0,
    1,
    1
  },
  {
    "*sibcall_value_nonlocal_aix64",
    "b %z1",
    0,
    &operand_data[821],
    5,
    0,
    1,
    1
  },
  {
    "*sibcall_nonlocal_sysv",
    (const PTR) output_401,
    0,
    &operand_data[844],
    4,
    0,
    2,
    3
  },
  {
    "*sibcall_value_nonlocal_sysv",
    (const PTR) output_402,
    0,
    &operand_data[848],
    5,
    0,
    2,
    3
  },
  {
    "blockage",
    "",
    (insn_gen_fn) gen_blockage,
    &operand_data[0],
    0,
    0,
    0,
    1
  },
  {
    "*cmpsi_internal1",
    "{cmp%I2|cmpw%I2} %0,%1,%2",
    0,
    &operand_data[853],
    3,
    0,
    1,
    1
  },
  {
    "*cmpdi_internal1",
    "cmpd%I2 %0,%1,%2",
    0,
    &operand_data[856],
    3,
    0,
    1,
    1
  },
  {
    "*cmpsi_internal2",
    "{cmpl%I2|cmplw%I2} %0,%1,%b2",
    0,
    &operand_data[859],
    3,
    0,
    1,
    1
  },
  {
    "*cmpdi_internal2",
    "cmpld%I2 %0,%1,%b2",
    0,
    &operand_data[862],
    3,
    0,
    1,
    1
  },
  {
    "*rs6000.md:11592",
    "#",
    0,
    &operand_data[865],
    5,
    1,
    1,
    1
  },
  {
    "*rs6000.md:11602",
    "#",
    0,
    &operand_data[870],
    5,
    1,
    1,
    1
  },
  {
    "*cmpsf_internal1",
    "fcmpu %0,%1,%2",
    0,
    &operand_data[875],
    3,
    0,
    1,
    1
  },
  {
    "*cmpdf_internal1",
    "fcmpu %0,%1,%2",
    0,
    &operand_data[878],
    3,
    0,
    1,
    1
  },
  {
    "*cmptf_internal1",
    "fcmpu %0,%1,%2\n\tbne %0,$+4\n\tfcmpu %0,%L1,%L2",
    0,
    &operand_data[881],
    3,
    0,
    1,
    1
  },
  {
    "*rs6000.md:11666",
    "%D1mfcr %0\n\t{rlinm|rlwinm} %0,%0,%J1,1",
    0,
    &operand_data[884],
    3,
    0,
    1,
    1
  },
  {
    "*rs6000.md:11684",
    "%D1mfcr %0\n\t{rlinm|rlwinm} %0,%0,%J1,1",
    0,
    &operand_data[887],
    3,
    0,
    1,
    1
  },
  {
    "*rs6000.md:11694",
    (const PTR) output_415,
    0,
    &operand_data[890],
    4,
    2,
    2,
    2
  },
  {
    "*rs6000.md:11725",
    (const PTR) output_416,
    0,
    &operand_data[894],
    4,
    0,
    1,
    3
  },
  {
    "*rs6000.md:11751",
    (const PTR) output_417,
    0,
    &operand_data[898],
    5,
    3,
    2,
    3
  },
  {
    "*rs6000.md:11851",
    (const PTR) output_418,
    0,
    &operand_data[903],
    4,
    0,
    5,
    2
  },
  {
    "*rs6000.md:11865",
    (const PTR) output_419,
    0,
    &operand_data[907],
    4,
    0,
    5,
    2
  },
  {
    "*rs6000.md:11879",
    (const PTR) output_420,
    0,
    &operand_data[911],
    5,
    2,
    10,
    2
  },
  {
    "*rs6000.md:11921",
    (const PTR) output_421,
    0,
    &operand_data[916],
    5,
    2,
    10,
    2
  },
  {
    "*rs6000.md:11978",
    (const PTR) output_422,
    0,
    &operand_data[921],
    4,
    0,
    5,
    2
  },
  {
    "*rs6000.md:11992",
    (const PTR) output_423,
    0,
    &operand_data[925],
    5,
    0,
    10,
    2
  },
  {
    "*rs6000.md:12035",
    (const PTR) output_424,
    0,
    &operand_data[930],
    5,
    3,
    10,
    2
  },
  {
    "*rs6000.md:12078",
    (const PTR) output_425,
    0,
    &operand_data[903],
    3,
    0,
    5,
    2
  },
  {
    "*ne0",
    "{ai|addic} %2,%1,-1\n\t{sfe|subfe} %0,%2,%1",
    0,
    &operand_data[935],
    3,
    0,
    1,
    1
  },
  {
    "*rs6000.md:12102",
    "addic %2,%1,-1\n\tsubfe %0,%2,%1",
    0,
    &operand_data[938],
    3,
    0,
    1,
    1
  },
  {
    "*rs6000.md:12112",
    "{ai|addic} %3,%1,-1\n\t{aze|addze} %0,%2",
    0,
    &operand_data[941],
    4,
    0,
    1,
    1
  },
  {
    "*rs6000.md:12123",
    "addic %3,%1,-1\n\taddze %0,%2",
    0,
    &operand_data[945],
    4,
    0,
    1,
    1
  },
  {
    "*rs6000.md:12134",
    (const PTR) output_430,
    0,
    &operand_data[949],
    5,
    0,
    2,
    2
  },
  {
    "*rs6000.md:12172",
    (const PTR) output_431,
    0,
    &operand_data[954],
    4,
    0,
    2,
    2
  },
  {
    "*rs6000.md:12207",
    (const PTR) output_432,
    0,
    &operand_data[958],
    5,
    2,
    2,
    2
  },
  {
    "*rs6000.md:12248",
    (const PTR) output_433,
    0,
    &operand_data[963],
    5,
    2,
    2,
    2
  },
  {
    "*rs6000.md:12289",
    (const PTR) output_434,
    0,
    &operand_data[968],
    4,
    0,
    2,
    2
  },
  {
    "*rs6000.md:12300",
    (const PTR) output_435,
    0,
    &operand_data[972],
    5,
    2,
    4,
    2
  },
  {
    "*rs6000.md:12336",
    (const PTR) output_436,
    0,
    &operand_data[977],
    4,
    0,
    2,
    2
  },
  {
    "*rs6000.md:12347",
    (const PTR) output_437,
    0,
    &operand_data[981],
    5,
    0,
    4,
    2
  },
  {
    "*rs6000.md:12381",
    (const PTR) output_438,
    0,
    &operand_data[986],
    5,
    3,
    4,
    2
  },
  {
    "*rs6000.md:12416",
    (const PTR) output_439,
    0,
    &operand_data[968],
    3,
    0,
    2,
    2
  },
  {
    "*rs6000.md:12426",
    "{sf%I2|subf%I2c} %0,%1,%2\n\t{cal %0,0(0)|li %0,0}\n\t{ae|adde} %0,%0,%0",
    0,
    &operand_data[90],
    3,
    0,
    1,
    1
  },
  {
    "*rs6000.md:12434",
    "subf%I2c %0,%1,%2\n\tli %0,0\n\tadde %0,%0,%0",
    0,
    &operand_data[991],
    3,
    0,
    1,
    1
  },
  {
    "*rs6000.md:12442",
    (const PTR) output_442,
    0,
    &operand_data[994],
    4,
    2,
    2,
    2
  },
  {
    "*rs6000.md:12473",
    (const PTR) output_443,
    0,
    &operand_data[97],
    4,
    2,
    2,
    2
  },
  {
    "*rs6000.md:12504",
    (const PTR) output_444,
    0,
    &operand_data[994],
    4,
    2,
    2,
    2
  },
  {
    "*rs6000.md:12519",
    "{sf%I2|subf%I2c} %0,%1,%2\n\t{aze|addze} %0,%3",
    0,
    &operand_data[998],
    4,
    0,
    1,
    1
  },
  {
    "*rs6000.md:12528",
    (const PTR) output_446,
    0,
    &operand_data[1002],
    5,
    0,
    2,
    2
  },
  {
    "*rs6000.md:12560",
    (const PTR) output_447,
    0,
    &operand_data[1007],
    5,
    3,
    2,
    2
  },
  {
    "*rs6000.md:12593",
    "{sf%I2|subf%I2c} %0,%1,%2\n\t{sfe|subfe} %0,%0,%0\n\tnand %0,%0,%0",
    0,
    &operand_data[90],
    3,
    0,
    1,
    1
  },
  {
    "*rs6000.md:12601",
    "{sf%I2|subf%I2c} %0,%1,%2\n\t{sfe|subfe} %0,%0,%0\n\tandc %0,%3,%0",
    0,
    &operand_data[998],
    4,
    0,
    1,
    1
  },
  {
    "*rs6000.md:12611",
    (const PTR) output_450,
    0,
    &operand_data[1002],
    5,
    0,
    2,
    2
  },
  {
    "*rs6000.md:12645",
    (const PTR) output_451,
    0,
    &operand_data[1007],
    5,
    3,
    2,
    2
  },
  {
    "*rs6000.md:12681",
    "doz%I2 %0,%1,%2\n\tnabs %0,%0\n\t{sri|srwi} %0,%0,31",
    0,
    &operand_data[90],
    3,
    0,
    1,
    1
  },
  {
    "*rs6000.md:12689",
    (const PTR) output_453,
    0,
    &operand_data[97],
    4,
    2,
    2,
    2
  },
  {
    "*rs6000.md:12720",
    "doz%I2 %0,%1,%2\n\t{ai|addic} %0,%0,-1\n\t{aze|addze} %0,%3",
    0,
    &operand_data[998],
    4,
    0,
    1,
    1
  },
  {
    "*rs6000.md:12729",
    (const PTR) output_455,
    0,
    &operand_data[1002],
    5,
    0,
    2,
    2
  },
  {
    "*rs6000.md:12761",
    (const PTR) output_456,
    0,
    &operand_data[1007],
    5,
    3,
    2,
    2
  },
  {
    "*rs6000.md:12794",
    "doz%I2 %0,%1,%2\n\tnabs %0,%0\n\t{srai|srawi} %0,%0,31",
    0,
    &operand_data[90],
    3,
    0,
    1,
    1
  },
  {
    "*rs6000.md:12802",
    (const PTR) output_458,
    0,
    &operand_data[1012],
    3,
    0,
    2,
    2
  },
  {
    "*rs6000.md:12812",
    (const PTR) output_459,
    0,
    &operand_data[1015],
    4,
    2,
    4,
    2
  },
  {
    "*rs6000.md:12845",
    (const PTR) output_460,
    0,
    &operand_data[1019],
    4,
    0,
    2,
    2
  },
  {
    "*rs6000.md:12856",
    (const PTR) output_461,
    0,
    &operand_data[1023],
    5,
    0,
    4,
    2
  },
  {
    "*rs6000.md:12890",
    (const PTR) output_462,
    0,
    &operand_data[1028],
    5,
    3,
    4,
    2
  },
  {
    "*rs6000.md:12925",
    (const PTR) output_463,
    0,
    &operand_data[1012],
    3,
    0,
    2,
    2
  },
  {
    "*rs6000.md:12935",
    "doz%I2 %3,%1,%2\n\t{sfi|subfic} %0,%3,0\n\t{ae|adde} %0,%0,%3",
    0,
    &operand_data[1033],
    4,
    0,
    1,
    1
  },
  {
    "*rs6000.md:12944",
    (const PTR) output_465,
    0,
    &operand_data[1037],
    5,
    2,
    2,
    2
  },
  {
    "*rs6000.md:12978",
    "doz%I2 %0,%1,%2\n\t{sfi|subfic} %0,%0,0\n\t{aze|addze} %0,%3",
    0,
    &operand_data[998],
    4,
    0,
    1,
    1
  },
  {
    "*rs6000.md:12987",
    (const PTR) output_467,
    0,
    &operand_data[1002],
    5,
    0,
    2,
    2
  },
  {
    "*rs6000.md:13019",
    (const PTR) output_468,
    0,
    &operand_data[1007],
    5,
    3,
    2,
    2
  },
  {
    "*rs6000.md:13052",
    "doz%I2 %0,%1,%2\n\t{ai|addic} %0,%0,-1\n\t{sfe|subfe} %0,%0,%0",
    0,
    &operand_data[90],
    3,
    0,
    1,
    1
  },
  {
    "*rs6000.md:13060",
    (const PTR) output_470,
    0,
    &operand_data[1012],
    3,
    0,
    2,
    2
  },
  {
    "*rs6000.md:13070",
    (const PTR) output_471,
    0,
    &operand_data[1042],
    3,
    0,
    2,
    2
  },
  {
    "*rs6000.md:13080",
    (const PTR) output_472,
    0,
    &operand_data[1015],
    4,
    2,
    4,
    2
  },
  {
    "*rs6000.md:13113",
    (const PTR) output_473,
    0,
    &operand_data[1045],
    4,
    2,
    4,
    2
  },
  {
    "*rs6000.md:13146",
    (const PTR) output_474,
    0,
    &operand_data[1049],
    4,
    0,
    2,
    2
  },
  {
    "*rs6000.md:13157",
    (const PTR) output_475,
    0,
    &operand_data[1023],
    5,
    0,
    4,
    2
  },
  {
    "*rs6000.md:13191",
    (const PTR) output_476,
    0,
    &operand_data[1028],
    5,
    3,
    4,
    2
  },
  {
    "*rs6000.md:13226",
    (const PTR) output_477,
    0,
    &operand_data[1053],
    3,
    0,
    2,
    2
  },
  {
    "*rs6000.md:13236",
    (const PTR) output_478,
    0,
    &operand_data[1049],
    4,
    0,
    2,
    2
  },
  {
    "*rs6000.md:13248",
    (const PTR) output_479,
    0,
    &operand_data[1023],
    5,
    0,
    4,
    2
  },
  {
    "*rs6000.md:13284",
    (const PTR) output_480,
    0,
    &operand_data[1028],
    5,
    3,
    4,
    2
  },
  {
    "*rs6000.md:13321",
    "{sfi|subfic} %0,%1,0\n\t{ame|addme} %0,%0\n\t{sri|srwi} %0,%0,31",
    0,
    &operand_data[68],
    2,
    0,
    1,
    1
  },
  {
    "*rs6000.md:13329",
    "subfic %0,%1,0\n\taddme %0,%0\n\tsrdi %0,%0,63",
    0,
    &operand_data[227],
    2,
    0,
    1,
    1
  },
  {
    "*rs6000.md:13337",
    (const PTR) output_483,
    0,
    &operand_data[73],
    3,
    1,
    2,
    2
  },
  {
    "*rs6000.md:13368",
    (const PTR) output_484,
    0,
    &operand_data[440],
    3,
    1,
    2,
    2
  },
  {
    "*rs6000.md:13399",
    "doz %0,%2,%1\n\tnabs %0,%0\n\t{sri|srwi} %0,%0,31",
    0,
    &operand_data[1056],
    3,
    0,
    1,
    1
  },
  {
    "*rs6000.md:13407",
    (const PTR) output_486,
    0,
    &operand_data[1059],
    4,
    2,
    2,
    2
  },
  {
    "*rs6000.md:13438",
    "{a|addc} %0,%1,%1\n\t{sfe|subfe} %0,%1,%0\n\t{aze|addze} %0,%2",
    0,
    &operand_data[1063],
    3,
    0,
    1,
    1
  },
  {
    "*rs6000.md:13447",
    "addc %0,%1,%1\n\tsubfe %0,%1,%0\n\taddze %0,%2",
    0,
    &operand_data[1066],
    3,
    0,
    1,
    1
  },
  {
    "*rs6000.md:13456",
    (const PTR) output_489,
    0,
    &operand_data[949],
    4,
    0,
    2,
    2
  },
  {
    "*rs6000.md:13488",
    (const PTR) output_490,
    0,
    &operand_data[954],
    4,
    0,
    2,
    2
  },
  {
    "*rs6000.md:13520",
    (const PTR) output_491,
    0,
    &operand_data[1069],
    4,
    2,
    2,
    2
  },
  {
    "*rs6000.md:13553",
    (const PTR) output_492,
    0,
    &operand_data[1073],
    4,
    2,
    2,
    2
  },
  {
    "*rs6000.md:13586",
    "doz %0,%2,%1\n\t{ai|addic} %0,%0,-1\n\t{aze|addze} %0,%3",
    0,
    &operand_data[1077],
    4,
    0,
    1,
    1
  },
  {
    "*rs6000.md:13595",
    (const PTR) output_494,
    0,
    &operand_data[1081],
    5,
    0,
    2,
    2
  },
  {
    "*rs6000.md:13626",
    (const PTR) output_495,
    0,
    &operand_data[1086],
    5,
    3,
    2,
    2
  },
  {
    "*rs6000.md:13659",
    "{sfi|subfic} %0,%1,0\n\t{ame|addme} %0,%0\n\t{srai|srawi} %0,%0,31",
    0,
    &operand_data[68],
    2,
    0,
    1,
    1
  },
  {
    "*rs6000.md:13667",
    "subfic %0,%1,0\n\taddme %0,%0\n\tsradi %0,%0,63",
    0,
    &operand_data[227],
    2,
    0,
    1,
    1
  },
  {
    "*rs6000.md:13675",
    "doz %0,%2,%1\n\tnabs %0,%0\n\t{srai|srawi} %0,%0,31",
    0,
    &operand_data[1056],
    3,
    0,
    1,
    1
  },
  {
    "*rs6000.md:13683",
    "{sf%I2|subf%I2c} %0,%1,%2\n\t{sfe|subfe} %0,%0,%0\n\tneg %0,%0",
    0,
    &operand_data[90],
    3,
    0,
    1,
    1
  },
  {
    "*rs6000.md:13691",
    "subf%I2c %0,%1,%2\n\tsubfe %0,%0,%0\n\tneg %0,%0",
    0,
    &operand_data[991],
    3,
    0,
    1,
    1
  },
  {
    "*rs6000.md:13699",
    (const PTR) output_501,
    0,
    &operand_data[97],
    4,
    2,
    2,
    2
  },
  {
    "*rs6000.md:13730",
    (const PTR) output_502,
    0,
    &operand_data[994],
    4,
    2,
    2,
    2
  },
  {
    "*rs6000.md:13761",
    (const PTR) output_503,
    0,
    &operand_data[1091],
    4,
    0,
    2,
    2
  },
  {
    "*rs6000.md:13772",
    (const PTR) output_504,
    0,
    &operand_data[1095],
    4,
    0,
    2,
    2
  },
  {
    "*rs6000.md:13783",
    (const PTR) output_505,
    0,
    &operand_data[1099],
    5,
    0,
    4,
    2
  },
  {
    "*rs6000.md:13817",
    (const PTR) output_506,
    0,
    &operand_data[1104],
    5,
    0,
    4,
    2
  },
  {
    "*rs6000.md:13851",
    (const PTR) output_507,
    0,
    &operand_data[1109],
    5,
    3,
    4,
    2
  },
  {
    "*rs6000.md:13886",
    (const PTR) output_508,
    0,
    &operand_data[1114],
    5,
    3,
    4,
    2
  },
  {
    "*rs6000.md:13921",
    "{sf%I2|subf%I2c} %0,%1,%2\n\t{sfe|subfe} %0,%0,%0",
    0,
    &operand_data[90],
    3,
    0,
    1,
    1
  },
  {
    "*rs6000.md:13929",
    "subf%I2c %0,%1,%2\n\tsubfe %0,%0,%0",
    0,
    &operand_data[991],
    3,
    0,
    1,
    1
  },
  {
    "*rs6000.md:13941",
    (const PTR) output_511,
    0,
    &operand_data[1119],
    3,
    0,
    2,
    3
  },
  {
    "*rs6000.md:13956",
    (const PTR) output_512,
    0,
    &operand_data[1120],
    2,
    0,
    2,
    3
  },
  {
    "*rs6000.md:13972",
    (const PTR) output_513,
    0,
    &operand_data[1119],
    3,
    0,
    2,
    3
  },
  {
    "*rs6000.md:13987",
    (const PTR) output_514,
    0,
    &operand_data[1120],
    2,
    0,
    2,
    3
  },
  {
    "*rs6000.md:14011",
    "cr%q1 %E0,%j2,%j4",
    0,
    &operand_data[1122],
    6,
    0,
    1,
    1
  },
  {
    "*rs6000.md:14031",
    "cr%q1 %E0,%j2,%j4",
    0,
    &operand_data[1128],
    6,
    0,
    1,
    1
  },
  {
    "*rs6000.md:14049",
    "{crnor %E0,%j1,%j1|crnot %E0,%j1}",
    0,
    &operand_data[1134],
    3,
    0,
    1,
    1
  },
  {
    "*rs6000.md:14064",
    "#",
    0,
    &operand_data[1137],
    5,
    0,
    1,
    1
  },
  {
    "jump",
    "b %l0",
    (insn_gen_fn) gen_jump,
    &operand_data[578],
    1,
    0,
    0,
    1
  },
  {
    "return",
    "{br|blr}",
    (insn_gen_fn) gen_return,
    &operand_data[0],
    0,
    0,
    0,
    1
  },
  {
    "indirect_jumpsi",
    (const PTR) output_521,
    (insn_gen_fn) gen_indirect_jumpsi,
    &operand_data[1142],
    1,
    0,
    2,
    2
  },
  {
    "indirect_jumpdi",
    (const PTR) output_522,
    (insn_gen_fn) gen_indirect_jumpdi,
    &operand_data[1143],
    1,
    0,
    2,
    2
  },
  {
    "*rs6000.md:14200",
    (const PTR) output_523,
    0,
    &operand_data[1144],
    2,
    0,
    2,
    2
  },
  {
    "*rs6000.md:14210",
    (const PTR) output_524,
    0,
    &operand_data[1146],
    2,
    0,
    2,
    2
  },
  {
    "nop",
    "{cror 0,0,0|nop}",
    (insn_gen_fn) gen_nop,
    &operand_data[0],
    0,
    0,
    0,
    1
  },
  {
    "*ctrsi_internal1",
    (const PTR) output_526,
    0,
    &operand_data[1147],
    5,
    1,
    3,
    3
  },
  {
    "*ctrsi_internal2",
    (const PTR) output_527,
    0,
    &operand_data[1147],
    5,
    1,
    3,
    3
  },
  {
    "*ctrdi_internal1",
    (const PTR) output_528,
    0,
    &operand_data[1152],
    5,
    1,
    3,
    3
  },
  {
    "*ctrdi_internal2",
    (const PTR) output_529,
    0,
    &operand_data[1152],
    5,
    1,
    3,
    3
  },
  {
    "*ctrsi_internal3",
    (const PTR) output_530,
    0,
    &operand_data[1157],
    5,
    1,
    3,
    3
  },
  {
    "*ctrsi_internal4",
    (const PTR) output_531,
    0,
    &operand_data[1157],
    5,
    1,
    3,
    3
  },
  {
    "*ctrdi_internal3",
    (const PTR) output_532,
    0,
    &operand_data[1152],
    5,
    1,
    3,
    3
  },
  {
    "*ctrdi_internal4",
    (const PTR) output_533,
    0,
    &operand_data[1152],
    5,
    1,
    3,
    3
  },
  {
    "*ctrsi_internal5",
    (const PTR) output_534,
    0,
    &operand_data[1147],
    5,
    1,
    3,
    3
  },
  {
    "*ctrsi_internal6",
    (const PTR) output_535,
    0,
    &operand_data[1147],
    5,
    1,
    3,
    3
  },
  {
    "*ctrdi_internal5",
    (const PTR) output_536,
    0,
    &operand_data[1152],
    5,
    1,
    3,
    3
  },
  {
    "*ctrdi_internal6",
    (const PTR) output_537,
    0,
    &operand_data[1152],
    5,
    1,
    3,
    3
  },
  {
    "trap",
    "{t 31,0,0|trap}",
    (insn_gen_fn) gen_trap,
    &operand_data[0],
    0,
    0,
    0,
    1
  },
  {
    "*rs6000.md:14709",
    "{t|tw}%V0%I2 %1,%2",
    0,
    &operand_data[1162],
    3,
    0,
    1,
    1
  },
  {
    "*rs6000.md:14717",
    "td%V0%I2 %1,%2",
    0,
    &operand_data[1165],
    3,
    0,
    1,
    1
  },
  {
    "movesi_from_cr",
    "mfcr %0",
    (insn_gen_fn) gen_movesi_from_cr,
    &operand_data[37],
    1,
    0,
    1,
    1
  },
  {
    "*stmw",
    "{stm|stmw} %2,%1",
    0,
    &operand_data[1168],
    3,
    0,
    1,
    1
  },
  {
    "*save_fpregs_si",
    "bl %z2",
    0,
    &operand_data[1171],
    5,
    0,
    1,
    1
  },
  {
    "*save_fpregs_di",
    "bl %z2",
    0,
    &operand_data[1176],
    5,
    0,
    1,
    1
  },
  {
    "stack_tie",
    "",
    (insn_gen_fn) gen_stack_tie,
    &operand_data[1181],
    1,
    1,
    1,
    1
  },
  {
    "*movsi_to_cr",
    (const PTR) output_546,
    0,
    &operand_data[1182],
    4,
    0,
    1,
    3
  },
  {
    "*rs6000.md:14817",
    "mtcrf %R0,%1",
    0,
    &operand_data[1183],
    3,
    0,
    1,
    1
  },
  {
    "*lmw",
    "{lm|lmw} %1,%2",
    0,
    &operand_data[1186],
    3,
    0,
    1,
    1
  },
  {
    "*return_internal_si",
    "b%T0",
    0,
    &operand_data[1189],
    1,
    0,
    1,
    1
  },
  {
    "*return_internal_di",
    "b%T0",
    0,
    &operand_data[1190],
    1,
    0,
    1,
    1
  },
  {
    "*return_and_restore_fpregs_si",
    "b %z2",
    0,
    &operand_data[1191],
    5,
    0,
    1,
    1
  },
  {
    "*return_and_restore_fpregs_di",
    "b %z2",
    0,
    &operand_data[1196],
    5,
    0,
    1,
    1
  },
  {
    "eh_set_lr_si",
    "#",
    (insn_gen_fn) gen_eh_set_lr_si,
    &operand_data[1201],
    2,
    0,
    1,
    1
  },
  {
    "eh_set_lr_di",
    "#",
    (insn_gen_fn) gen_eh_set_lr_di,
    &operand_data[1203],
    2,
    0,
    1,
    1
  },
  {
    "prefetch",
    (const PTR) output_555,
    (insn_gen_fn) gen_prefetch,
    &operand_data[1205],
    3,
    0,
    1,
    3
  },
  {
    "altivec_lvx_4si",
    "lvx %0,%y1",
    (insn_gen_fn) gen_altivec_lvx_4si,
    &operand_data[1208],
    2,
    0,
    1,
    1
  },
  {
    "altivec_lvx_8hi",
    "lvx %0,%y1",
    (insn_gen_fn) gen_altivec_lvx_8hi,
    &operand_data[1210],
    2,
    0,
    1,
    1
  },
  {
    "altivec_lvx_16qi",
    "lvx %0,%y1",
    (insn_gen_fn) gen_altivec_lvx_16qi,
    &operand_data[1212],
    2,
    0,
    1,
    1
  },
  {
    "altivec_lvx_4sf",
    "lvx %0,%y1",
    (insn_gen_fn) gen_altivec_lvx_4sf,
    &operand_data[1214],
    2,
    0,
    1,
    1
  },
  {
    "altivec_stvx_4si",
    "stvx %1,%y0",
    (insn_gen_fn) gen_altivec_stvx_4si,
    &operand_data[1216],
    2,
    0,
    1,
    1
  },
  {
    "altivec_stvx_8hi",
    "stvx %1,%y0",
    (insn_gen_fn) gen_altivec_stvx_8hi,
    &operand_data[1218],
    2,
    0,
    1,
    1
  },
  {
    "altivec_stvx_16qi",
    "stvx %1,%y0",
    (insn_gen_fn) gen_altivec_stvx_16qi,
    &operand_data[1220],
    2,
    0,
    1,
    1
  },
  {
    "altivec_stvx_4sf",
    "stvx %1,%y0",
    (insn_gen_fn) gen_altivec_stvx_4sf,
    &operand_data[1222],
    2,
    0,
    1,
    1
  },
  {
    "*movv4si_internal",
    (const PTR) output_564,
    0,
    &operand_data[1224],
    2,
    0,
    6,
    2
  },
  {
    "*movv8hi_internal1",
    (const PTR) output_565,
    0,
    &operand_data[1226],
    2,
    0,
    6,
    2
  },
  {
    "*movv16qi_internal1",
    (const PTR) output_566,
    0,
    &operand_data[1228],
    2,
    0,
    6,
    2
  },
  {
    "*movv4sf_internal1",
    (const PTR) output_567,
    0,
    &operand_data[1230],
    2,
    0,
    6,
    2
  },
  {
    "get_vrsave_internal",
    (const PTR) output_568,
    (insn_gen_fn) gen_get_vrsave_internal,
    &operand_data[1232],
    1,
    0,
    1,
    3
  },
  {
    "*set_vrsave_internal",
    (const PTR) output_569,
    0,
    &operand_data[1233],
    2,
    0,
    1,
    3
  },
  {
    "*movv4si_const0",
    "vxor %0,%0,%0",
    0,
    &operand_data[1235],
    2,
    0,
    1,
    1
  },
  {
    "*movv4sf_const0",
    "vxor %0,%0,%0",
    0,
    &operand_data[1237],
    2,
    0,
    1,
    1
  },
  {
    "*movv8hi_const0",
    "vxor %0,%0,%0",
    0,
    &operand_data[1239],
    2,
    0,
    1,
    1
  },
  {
    "*movv16qi_const0",
    "vxor %0,%0,%0",
    0,
    &operand_data[1241],
    2,
    0,
    1,
    1
  },
  {
    "addv16qi3",
    "vaddubm %0,%1,%2",
    (insn_gen_fn) gen_addv16qi3,
    &operand_data[1243],
    3,
    0,
    1,
    1
  },
  {
    "addv8hi3",
    "vadduhm %0,%1,%2",
    (insn_gen_fn) gen_addv8hi3,
    &operand_data[1246],
    3,
    0,
    1,
    1
  },
  {
    "addv4si3",
    "vadduwm %0,%1,%2",
    (insn_gen_fn) gen_addv4si3,
    &operand_data[1249],
    3,
    0,
    1,
    1
  },
  {
    "addv4sf3",
    "vaddfp %0,%1,%2",
    (insn_gen_fn) gen_addv4sf3,
    &operand_data[1252],
    3,
    0,
    1,
    1
  },
  {
    "altivec_vaddcuw",
    "vaddcuw %0,%1,%2",
    (insn_gen_fn) gen_altivec_vaddcuw,
    &operand_data[1249],
    3,
    0,
    1,
    1
  },
  {
    "altivec_vaddubs",
    "vaddubs %0,%1,%2",
    (insn_gen_fn) gen_altivec_vaddubs,
    &operand_data[1243],
    3,
    0,
    1,
    1
  },
  {
    "altivec_vaddsbs",
    "vaddsbs %0,%1,%2",
    (insn_gen_fn) gen_altivec_vaddsbs,
    &operand_data[1243],
    3,
    0,
    1,
    1
  },
  {
    "altivec_vadduhs",
    "vadduhs %0,%1,%2",
    (insn_gen_fn) gen_altivec_vadduhs,
    &operand_data[1246],
    3,
    0,
    1,
    1
  },
  {
    "altivec_vaddshs",
    "vaddshs %0,%1,%2",
    (insn_gen_fn) gen_altivec_vaddshs,
    &operand_data[1246],
    3,
    0,
    1,
    1
  },
  {
    "altivec_vadduws",
    "vadduws %0,%1,%2",
    (insn_gen_fn) gen_altivec_vadduws,
    &operand_data[1249],
    3,
    0,
    1,
    1
  },
  {
    "altivec_vaddsws",
    "vaddsws %0,%1,%2",
    (insn_gen_fn) gen_altivec_vaddsws,
    &operand_data[1249],
    3,
    0,
    1,
    1
  },
  {
    "andv4si3",
    "vand %0,%1,%2",
    (insn_gen_fn) gen_andv4si3,
    &operand_data[1249],
    3,
    0,
    1,
    1
  },
  {
    "altivec_vandc",
    "vandc %0,%1,%2",
    (insn_gen_fn) gen_altivec_vandc,
    &operand_data[1249],
    3,
    0,
    1,
    1
  },
  {
    "altivec_vavgub",
    "vavgub %0,%1,%2",
    (insn_gen_fn) gen_altivec_vavgub,
    &operand_data[1243],
    3,
    0,
    1,
    1
  },
  {
    "altivec_vavgsb",
    "vavgsb %0,%1,%2",
    (insn_gen_fn) gen_altivec_vavgsb,
    &operand_data[1243],
    3,
    0,
    1,
    1
  },
  {
    "altivec_vavguh",
    "vavguh %0,%1,%2",
    (insn_gen_fn) gen_altivec_vavguh,
    &operand_data[1246],
    3,
    0,
    1,
    1
  },
  {
    "altivec_vavgsh",
    "vavgsh %0,%1,%2",
    (insn_gen_fn) gen_altivec_vavgsh,
    &operand_data[1246],
    3,
    0,
    1,
    1
  },
  {
    "altivec_vavguw",
    "vavguw %0,%1,%2",
    (insn_gen_fn) gen_altivec_vavguw,
    &operand_data[1249],
    3,
    0,
    1,
    1
  },
  {
    "altivec_vavgsw",
    "vavgsw %0,%1,%2",
    (insn_gen_fn) gen_altivec_vavgsw,
    &operand_data[1249],
    3,
    0,
    1,
    1
  },
  {
    "altivec_vcmpbfp",
    "vcmpbfp %0,%1,%2",
    (insn_gen_fn) gen_altivec_vcmpbfp,
    &operand_data[1255],
    3,
    0,
    1,
    1
  },
  {
    "altivec_vcmpequb",
    "vcmpequb %0,%1,%2",
    (insn_gen_fn) gen_altivec_vcmpequb,
    &operand_data[1243],
    3,
    0,
    1,
    1
  },
  {
    "altivec_vcmpequh",
    "vcmpequh %0,%1,%2",
    (insn_gen_fn) gen_altivec_vcmpequh,
    &operand_data[1246],
    3,
    0,
    1,
    1
  },
  {
    "altivec_vcmpequw",
    "vcmpequw %0,%1,%2",
    (insn_gen_fn) gen_altivec_vcmpequw,
    &operand_data[1249],
    3,
    0,
    1,
    1
  },
  {
    "altivec_vcmpeqfp",
    "vcmpeqfp %0,%1,%2",
    (insn_gen_fn) gen_altivec_vcmpeqfp,
    &operand_data[1255],
    3,
    0,
    1,
    1
  },
  {
    "altivec_vcmpgefp",
    "vcmpgefp %0,%1,%2",
    (insn_gen_fn) gen_altivec_vcmpgefp,
    &operand_data[1255],
    3,
    0,
    1,
    1
  },
  {
    "altivec_vcmpgtub",
    "vcmpgtub %0,%1,%2",
    (insn_gen_fn) gen_altivec_vcmpgtub,
    &operand_data[1243],
    3,
    0,
    1,
    1
  },
  {
    "altivec_vcmpgtsb",
    "vcmpgtsb %0,%1,%2",
    (insn_gen_fn) gen_altivec_vcmpgtsb,
    &operand_data[1243],
    3,
    0,
    1,
    1
  },
  {
    "altivec_vcmpgtuh",
    "vcmpgtuh %0,%1,%2",
    (insn_gen_fn) gen_altivec_vcmpgtuh,
    &operand_data[1246],
    3,
    0,
    1,
    1
  },
  {
    "altivec_vcmpgtsh",
    "vcmpgtsh %0,%1,%2",
    (insn_gen_fn) gen_altivec_vcmpgtsh,
    &operand_data[1246],
    3,
    0,
    1,
    1
  },
  {
    "altivec_vcmpgtuw",
    "vcmpgtuw %0,%1,%2",
    (insn_gen_fn) gen_altivec_vcmpgtuw,
    &operand_data[1249],
    3,
    0,
    1,
    1
  },
  {
    "altivec_vcmpgtsw",
    "vcmpgtsw %0,%1,%2",
    (insn_gen_fn) gen_altivec_vcmpgtsw,
    &operand_data[1249],
    3,
    0,
    1,
    1
  },
  {
    "altivec_vcmpgtfp",
    "vcmpgtfp %0,%1,%2",
    (insn_gen_fn) gen_altivec_vcmpgtfp,
    &operand_data[1255],
    3,
    0,
    1,
    1
  },
  {
    "altivec_vmaddfp",
    "vmaddfp %0,%1,%2,%3",
    (insn_gen_fn) gen_altivec_vmaddfp,
    &operand_data[1258],
    4,
    0,
    1,
    1
  },
  {
    "altivec_vnmsubfp",
    "vnmsubfp %0,%1,%2,%3",
    (insn_gen_fn) gen_altivec_vnmsubfp,
    &operand_data[1258],
    4,
    0,
    1,
    1
  },
  {
    "altivec_vmsumubm",
    "vmsumubm %0, %1, %2, %3",
    (insn_gen_fn) gen_altivec_vmsumubm,
    &operand_data[1262],
    4,
    0,
    1,
    1
  },
  {
    "altivec_vmsummbm",
    "vmsumubm %0, %1, %2, %3",
    (insn_gen_fn) gen_altivec_vmsummbm,
    &operand_data[1262],
    4,
    0,
    1,
    1
  },
  {
    "altivec_vmsumuhm",
    "vmsumuhm %0, %1, %2, %3",
    (insn_gen_fn) gen_altivec_vmsumuhm,
    &operand_data[1266],
    4,
    0,
    1,
    1
  },
  {
    "altivec_vmsumshm",
    "vmsumshm %0, %1, %2, %3",
    (insn_gen_fn) gen_altivec_vmsumshm,
    &operand_data[1266],
    4,
    0,
    1,
    1
  },
  {
    "altivec_vmsumuhs",
    "vmsumuhs %0, %1, %2, %3",
    (insn_gen_fn) gen_altivec_vmsumuhs,
    &operand_data[1266],
    4,
    0,
    1,
    1
  },
  {
    "altivec_vmsumshs",
    "vmsumshs %0, %1, %2, %3",
    (insn_gen_fn) gen_altivec_vmsumshs,
    &operand_data[1266],
    4,
    0,
    1,
    1
  },
  {
    "umaxv16qi3",
    "vmaxub %0,%1,%2",
    (insn_gen_fn) gen_umaxv16qi3,
    &operand_data[1243],
    3,
    0,
    1,
    1
  },
  {
    "smaxv16qi3",
    "vmaxsb %0,%1,%2",
    (insn_gen_fn) gen_smaxv16qi3,
    &operand_data[1243],
    3,
    0,
    1,
    1
  },
  {
    "umaxv8hi3",
    "vmaxuh %0,%1,%2",
    (insn_gen_fn) gen_umaxv8hi3,
    &operand_data[1246],
    3,
    0,
    1,
    1
  },
  {
    "smaxv8hi3",
    "vmaxsh %0,%1,%2",
    (insn_gen_fn) gen_smaxv8hi3,
    &operand_data[1246],
    3,
    0,
    1,
    1
  },
  {
    "umaxv4si3",
    "vmaxuw %0,%1,%2",
    (insn_gen_fn) gen_umaxv4si3,
    &operand_data[1249],
    3,
    0,
    1,
    1
  },
  {
    "smaxv4si3",
    "vmaxsw %0,%1,%2",
    (insn_gen_fn) gen_smaxv4si3,
    &operand_data[1249],
    3,
    0,
    1,
    1
  },
  {
    "smaxv4sf3",
    "vmaxfp %0,%1,%2",
    (insn_gen_fn) gen_smaxv4sf3,
    &operand_data[1252],
    3,
    0,
    1,
    1
  },
  {
    "altivec_vmhaddshs",
    "vmhaddshs %0, %1, %2, %3",
    (insn_gen_fn) gen_altivec_vmhaddshs,
    &operand_data[1270],
    4,
    0,
    1,
    1
  },
  {
    "altivec_vmhraddshs",
    "vmhraddshs %0, %1, %2, %3",
    (insn_gen_fn) gen_altivec_vmhraddshs,
    &operand_data[1270],
    4,
    0,
    1,
    1
  },
  {
    "altivec_vmladduhm",
    "vmladduhm %0, %1, %2, %3",
    (insn_gen_fn) gen_altivec_vmladduhm,
    &operand_data[1270],
    4,
    0,
    1,
    1
  },
  {
    "altivec_vmrghb",
    "vmrghb %0,%1,%2",
    (insn_gen_fn) gen_altivec_vmrghb,
    &operand_data[1243],
    3,
    0,
    1,
    1
  },
  {
    "altivec_vmrghh",
    "vmrghh %0,%1,%2",
    (insn_gen_fn) gen_altivec_vmrghh,
    &operand_data[1246],
    3,
    0,
    1,
    1
  },
  {
    "altivec_vmrghw",
    "vmrghw %0,%1,%2",
    (insn_gen_fn) gen_altivec_vmrghw,
    &operand_data[1249],
    3,
    0,
    1,
    1
  },
  {
    "altivec_vmrglb",
    "vmrglb %0,%1,%2",
    (insn_gen_fn) gen_altivec_vmrglb,
    &operand_data[1243],
    3,
    0,
    1,
    1
  },
  {
    "altivec_vmrglh",
    "vmrglh %0,%1,%2",
    (insn_gen_fn) gen_altivec_vmrglh,
    &operand_data[1246],
    3,
    0,
    1,
    1
  },
  {
    "altivec_vmrglw",
    "vmrglw %0,%1,%2",
    (insn_gen_fn) gen_altivec_vmrglw,
    &operand_data[1249],
    3,
    0,
    1,
    1
  },
  {
    "uminv16qi3",
    "vminub %0,%1,%2",
    (insn_gen_fn) gen_uminv16qi3,
    &operand_data[1243],
    3,
    0,
    1,
    1
  },
  {
    "sminv16qi3",
    "vminsb %0,%1,%2",
    (insn_gen_fn) gen_sminv16qi3,
    &operand_data[1243],
    3,
    0,
    1,
    1
  },
  {
    "uminv8hi3",
    "vminuh %0,%1,%2",
    (insn_gen_fn) gen_uminv8hi3,
    &operand_data[1246],
    3,
    0,
    1,
    1
  },
  {
    "sminv8hi3",
    "vminsh %0,%1,%2",
    (insn_gen_fn) gen_sminv8hi3,
    &operand_data[1246],
    3,
    0,
    1,
    1
  },
  {
    "uminv4si3",
    "vminuw %0,%1,%2",
    (insn_gen_fn) gen_uminv4si3,
    &operand_data[1249],
    3,
    0,
    1,
    1
  },
  {
    "sminv4si3",
    "vminsw %0,%1,%2",
    (insn_gen_fn) gen_sminv4si3,
    &operand_data[1249],
    3,
    0,
    1,
    1
  },
  {
    "sminv4sf3",
    "vminfp %0,%1,%2",
    (insn_gen_fn) gen_sminv4sf3,
    &operand_data[1252],
    3,
    0,
    1,
    1
  },
  {
    "altivec_vmuleub",
    "vmuleub %0,%1,%2",
    (insn_gen_fn) gen_altivec_vmuleub,
    &operand_data[1274],
    3,
    0,
    1,
    1
  },
  {
    "altivec_vmulesb",
    "vmulesb %0,%1,%2",
    (insn_gen_fn) gen_altivec_vmulesb,
    &operand_data[1274],
    3,
    0,
    1,
    1
  },
  {
    "altivec_vmuleuh",
    "vmuleuh %0,%1,%2",
    (insn_gen_fn) gen_altivec_vmuleuh,
    &operand_data[1266],
    3,
    0,
    1,
    1
  },
  {
    "altivec_vmulesh",
    "vmulesh %0,%1,%2",
    (insn_gen_fn) gen_altivec_vmulesh,
    &operand_data[1266],
    3,
    0,
    1,
    1
  },
  {
    "altivec_vmuloub",
    "vmuloub %0,%1,%2",
    (insn_gen_fn) gen_altivec_vmuloub,
    &operand_data[1274],
    3,
    0,
    1,
    1
  },
  {
    "altivec_vmulosb",
    "vmulosb %0,%1,%2",
    (insn_gen_fn) gen_altivec_vmulosb,
    &operand_data[1274],
    3,
    0,
    1,
    1
  },
  {
    "altivec_vmulouh",
    "vmulouh %0,%1,%2",
    (insn_gen_fn) gen_altivec_vmulouh,
    &operand_data[1266],
    3,
    0,
    1,
    1
  },
  {
    "altivec_vmulosh",
    "vmulosh %0,%1,%2",
    (insn_gen_fn) gen_altivec_vmulosh,
    &operand_data[1266],
    3,
    0,
    1,
    1
  },
  {
    "altivec_vnor",
    "vnor %0,%1,%2",
    (insn_gen_fn) gen_altivec_vnor,
    &operand_data[1249],
    3,
    0,
    1,
    1
  },
  {
    "iorv4si3",
    "vor %0,%1,%2",
    (insn_gen_fn) gen_iorv4si3,
    &operand_data[1249],
    3,
    0,
    1,
    1
  },
  {
    "altivec_vpkuhum",
    "vpkuhum %0,%1,%2",
    (insn_gen_fn) gen_altivec_vpkuhum,
    &operand_data[1277],
    3,
    0,
    1,
    1
  },
  {
    "altivec_vpkuwum",
    "vpkuwum %0,%1,%2",
    (insn_gen_fn) gen_altivec_vpkuwum,
    &operand_data[1280],
    3,
    0,
    1,
    1
  },
  {
    "altivec_vpkpx",
    "vpkpx %0,%1,%2",
    (insn_gen_fn) gen_altivec_vpkpx,
    &operand_data[1280],
    3,
    0,
    1,
    1
  },
  {
    "altivec_vpkuhss",
    "vpkuhss %0,%1,%2",
    (insn_gen_fn) gen_altivec_vpkuhss,
    &operand_data[1277],
    3,
    0,
    1,
    1
  },
  {
    "altivec_vpkshss",
    "vpkshss %0,%1,%2",
    (insn_gen_fn) gen_altivec_vpkshss,
    &operand_data[1277],
    3,
    0,
    1,
    1
  },
  {
    "altivec_vpkuwss",
    "vpkuwss %0,%1,%2",
    (insn_gen_fn) gen_altivec_vpkuwss,
    &operand_data[1280],
    3,
    0,
    1,
    1
  },
  {
    "altivec_vpkswss",
    "vpkswss %0,%1,%2",
    (insn_gen_fn) gen_altivec_vpkswss,
    &operand_data[1280],
    3,
    0,
    1,
    1
  },
  {
    "altivec_vpkuhus",
    "vpkuhus %0,%1,%2",
    (insn_gen_fn) gen_altivec_vpkuhus,
    &operand_data[1277],
    3,
    0,
    1,
    1
  },
  {
    "altivec_vpkshus",
    "vpkshus %0,%1,%2",
    (insn_gen_fn) gen_altivec_vpkshus,
    &operand_data[1277],
    3,
    0,
    1,
    1
  },
  {
    "altivec_vpkuwus",
    "vpkuwus %0,%1,%2",
    (insn_gen_fn) gen_altivec_vpkuwus,
    &operand_data[1280],
    3,
    0,
    1,
    1
  },
  {
    "altivec_vpkswus",
    "vpkswus %0,%1,%2",
    (insn_gen_fn) gen_altivec_vpkswus,
    &operand_data[1280],
    3,
    0,
    1,
    1
  },
  {
    "altivec_vrlb",
    "vrlb %0,%1,%2",
    (insn_gen_fn) gen_altivec_vrlb,
    &operand_data[1243],
    3,
    0,
    1,
    1
  },
  {
    "altivec_vrlh",
    "vrlh %0,%1,%2",
    (insn_gen_fn) gen_altivec_vrlh,
    &operand_data[1246],
    3,
    0,
    1,
    1
  },
  {
    "altivec_vrlw",
    "vrlw %0,%1,%2",
    (insn_gen_fn) gen_altivec_vrlw,
    &operand_data[1249],
    3,
    0,
    1,
    1
  },
  {
    "altivec_vslb",
    "vslb %0,%1,%2",
    (insn_gen_fn) gen_altivec_vslb,
    &operand_data[1243],
    3,
    0,
    1,
    1
  },
  {
    "altivec_vslh",
    "vslh %0,%1,%2",
    (insn_gen_fn) gen_altivec_vslh,
    &operand_data[1246],
    3,
    0,
    1,
    1
  },
  {
    "altivec_vslw",
    "vslw %0,%1,%2",
    (insn_gen_fn) gen_altivec_vslw,
    &operand_data[1249],
    3,
    0,
    1,
    1
  },
  {
    "altivec_vslw_v4sf",
    "vslw %0,%1,%2",
    (insn_gen_fn) gen_altivec_vslw_v4sf,
    &operand_data[1252],
    3,
    0,
    1,
    1
  },
  {
    "altivec_vsl",
    "vsl %0,%1,%2",
    (insn_gen_fn) gen_altivec_vsl,
    &operand_data[1249],
    3,
    0,
    1,
    1
  },
  {
    "altivec_vslo",
    "vslo %0,%1,%2",
    (insn_gen_fn) gen_altivec_vslo,
    &operand_data[1249],
    3,
    0,
    1,
    1
  },
  {
    "altivec_vsrb",
    "vsrb %0,%1,%2",
    (insn_gen_fn) gen_altivec_vsrb,
    &operand_data[1243],
    3,
    0,
    1,
    1
  },
  {
    "altivec_vsrh",
    "vsrh %0,%1,%2",
    (insn_gen_fn) gen_altivec_vsrh,
    &operand_data[1246],
    3,
    0,
    1,
    1
  },
  {
    "altivec_vsrw",
    "vsrw %0,%1,%2",
    (insn_gen_fn) gen_altivec_vsrw,
    &operand_data[1249],
    3,
    0,
    1,
    1
  },
  {
    "altivec_vsrab",
    "vsrab %0,%1,%2",
    (insn_gen_fn) gen_altivec_vsrab,
    &operand_data[1243],
    3,
    0,
    1,
    1
  },
  {
    "altivec_vsrah",
    "vsrah %0,%1,%2",
    (insn_gen_fn) gen_altivec_vsrah,
    &operand_data[1246],
    3,
    0,
    1,
    1
  },
  {
    "altivec_vsraw",
    "vsraw %0,%1,%2",
    (insn_gen_fn) gen_altivec_vsraw,
    &operand_data[1249],
    3,
    0,
    1,
    1
  },
  {
    "altivec_vsr",
    "vsr %0,%1,%2",
    (insn_gen_fn) gen_altivec_vsr,
    &operand_data[1249],
    3,
    0,
    1,
    1
  },
  {
    "altivec_vsro",
    "vsro %0,%1,%2",
    (insn_gen_fn) gen_altivec_vsro,
    &operand_data[1249],
    3,
    0,
    1,
    1
  },
  {
    "subv16qi3",
    "vsububm %0,%1,%2",
    (insn_gen_fn) gen_subv16qi3,
    &operand_data[1243],
    3,
    0,
    1,
    1
  },
  {
    "subv8hi3",
    "vsubuhm %0,%1,%2",
    (insn_gen_fn) gen_subv8hi3,
    &operand_data[1246],
    3,
    0,
    1,
    1
  },
  {
    "subv4si3",
    "vsubuwm %0,%1,%2",
    (insn_gen_fn) gen_subv4si3,
    &operand_data[1249],
    3,
    0,
    1,
    1
  },
  {
    "subv4sf3",
    "vsubfp %0,%1,%2",
    (insn_gen_fn) gen_subv4sf3,
    &operand_data[1252],
    3,
    0,
    1,
    1
  },
  {
    "altivec_vsubcuw",
    "vsubcuw %0,%1,%2",
    (insn_gen_fn) gen_altivec_vsubcuw,
    &operand_data[1249],
    3,
    0,
    1,
    1
  },
  {
    "altivec_vsububs",
    "vsububs %0,%1,%2",
    (insn_gen_fn) gen_altivec_vsububs,
    &operand_data[1243],
    3,
    0,
    1,
    1
  },
  {
    "altivec_vsubsbs",
    "vsubsbs %0,%1,%2",
    (insn_gen_fn) gen_altivec_vsubsbs,
    &operand_data[1243],
    3,
    0,
    1,
    1
  },
  {
    "altivec_vsubuhs",
    "vsubuhs %0,%1,%2",
    (insn_gen_fn) gen_altivec_vsubuhs,
    &operand_data[1246],
    3,
    0,
    1,
    1
  },
  {
    "altivec_vsubshs",
    "vsubshs %0,%1,%2",
    (insn_gen_fn) gen_altivec_vsubshs,
    &operand_data[1246],
    3,
    0,
    1,
    1
  },
  {
    "altivec_vsubuws",
    "vsubuws %0,%1,%2",
    (insn_gen_fn) gen_altivec_vsubuws,
    &operand_data[1249],
    3,
    0,
    1,
    1
  },
  {
    "altivec_vsubsws",
    "vsubsws %0,%1,%2",
    (insn_gen_fn) gen_altivec_vsubsws,
    &operand_data[1249],
    3,
    0,
    1,
    1
  },
  {
    "altivec_vsum4ubs",
    "vsum4ubs %0,%1,%2",
    (insn_gen_fn) gen_altivec_vsum4ubs,
    &operand_data[1283],
    3,
    0,
    1,
    1
  },
  {
    "altivec_vsum4sbs",
    "vsum4sbs %0,%1,%2",
    (insn_gen_fn) gen_altivec_vsum4sbs,
    &operand_data[1283],
    3,
    0,
    1,
    1
  },
  {
    "altivec_vsum4shs",
    "vsum4shs %0,%1,%2",
    (insn_gen_fn) gen_altivec_vsum4shs,
    &operand_data[1286],
    3,
    0,
    1,
    1
  },
  {
    "altivec_vsum2sws",
    "vsum2sws %0,%1,%2",
    (insn_gen_fn) gen_altivec_vsum2sws,
    &operand_data[1249],
    3,
    0,
    1,
    1
  },
  {
    "altivec_vsumsws",
    "vsumsws %0,%1,%2",
    (insn_gen_fn) gen_altivec_vsumsws,
    &operand_data[1249],
    3,
    0,
    1,
    1
  },
  {
    "xorv4si3",
    "vxor %0,%1,%2",
    (insn_gen_fn) gen_xorv4si3,
    &operand_data[1249],
    3,
    0,
    1,
    1
  },
  {
    "altivec_vspltb",
    "vspltb %0,%1,%2",
    (insn_gen_fn) gen_altivec_vspltb,
    &operand_data[1289],
    3,
    0,
    1,
    1
  },
  {
    "altivec_vsplth",
    "vsplth %0,%1,%2",
    (insn_gen_fn) gen_altivec_vsplth,
    &operand_data[1292],
    3,
    0,
    1,
    1
  },
  {
    "altivec_vspltw",
    "vspltw %0,%1,%2",
    (insn_gen_fn) gen_altivec_vspltw,
    &operand_data[1295],
    3,
    0,
    1,
    1
  },
  {
    "altivec_vspltisb",
    "vspltisb %0, %1",
    (insn_gen_fn) gen_altivec_vspltisb,
    &operand_data[1298],
    2,
    0,
    1,
    1
  },
  {
    "altivec_vspltish",
    "vspltish %0, %1",
    (insn_gen_fn) gen_altivec_vspltish,
    &operand_data[1300],
    2,
    0,
    1,
    1
  },
  {
    "altivec_vspltisw",
    "vspltisw %0, %1",
    (insn_gen_fn) gen_altivec_vspltisw,
    &operand_data[1302],
    2,
    0,
    1,
    1
  },
  {
    "altivec_vspltisw_v4sf",
    "vspltisw %0, %1",
    (insn_gen_fn) gen_altivec_vspltisw_v4sf,
    &operand_data[1304],
    2,
    0,
    1,
    1
  },
  {
    "ftruncv4sf2",
    "vrfiz %0, %1",
    (insn_gen_fn) gen_ftruncv4sf2,
    &operand_data[1252],
    2,
    0,
    1,
    1
  },
  {
    "altivec_vperm_4si",
    "vperm %0,%1,%2,%3",
    (insn_gen_fn) gen_altivec_vperm_4si,
    &operand_data[1306],
    4,
    0,
    1,
    1
  },
  {
    "altivec_vperm_4sf",
    "vperm %0,%1,%2,%3",
    (insn_gen_fn) gen_altivec_vperm_4sf,
    &operand_data[1310],
    4,
    0,
    1,
    1
  },
  {
    "altivec_vperm_8hi",
    "vperm %0,%1,%2,%3",
    (insn_gen_fn) gen_altivec_vperm_8hi,
    &operand_data[1314],
    4,
    0,
    1,
    1
  },
  {
    "altivec_vperm_16qi",
    "vperm %0,%1,%2,%3",
    (insn_gen_fn) gen_altivec_vperm_16qi,
    &operand_data[1318],
    4,
    0,
    1,
    1
  },
  {
    "altivec_vrfip",
    "vrfip %0, %1",
    (insn_gen_fn) gen_altivec_vrfip,
    &operand_data[1252],
    2,
    0,
    1,
    1
  },
  {
    "altivec_vrfin",
    "vrfin %0, %1",
    (insn_gen_fn) gen_altivec_vrfin,
    &operand_data[1252],
    2,
    0,
    1,
    1
  },
  {
    "altivec_vrfim",
    "vrfim %0, %1",
    (insn_gen_fn) gen_altivec_vrfim,
    &operand_data[1252],
    2,
    0,
    1,
    1
  },
  {
    "altivec_vcfux",
    "vcfux %0, %1, %2",
    (insn_gen_fn) gen_altivec_vcfux,
    &operand_data[1322],
    3,
    0,
    1,
    1
  },
  {
    "altivec_vcfsx",
    "vcfsx %0, %1, %2",
    (insn_gen_fn) gen_altivec_vcfsx,
    &operand_data[1322],
    3,
    0,
    1,
    1
  },
  {
    "altivec_vctuxs",
    "vctuxs %0, %1, %2",
    (insn_gen_fn) gen_altivec_vctuxs,
    &operand_data[1325],
    3,
    0,
    1,
    1
  },
  {
    "altivec_vctsxs",
    "vctsxs %0, %1, %2",
    (insn_gen_fn) gen_altivec_vctsxs,
    &operand_data[1325],
    3,
    0,
    1,
    1
  },
  {
    "altivec_vlogefp",
    "vlogefp %0, %1",
    (insn_gen_fn) gen_altivec_vlogefp,
    &operand_data[1252],
    2,
    0,
    1,
    1
  },
  {
    "altivec_vexptefp",
    "vexptefp %0, %1",
    (insn_gen_fn) gen_altivec_vexptefp,
    &operand_data[1252],
    2,
    0,
    1,
    1
  },
  {
    "altivec_vrsqrtefp",
    "vrsqrtefp %0, %1",
    (insn_gen_fn) gen_altivec_vrsqrtefp,
    &operand_data[1252],
    2,
    0,
    1,
    1
  },
  {
    "altivec_vrefp",
    "vrefp %0, %1",
    (insn_gen_fn) gen_altivec_vrefp,
    &operand_data[1252],
    2,
    0,
    1,
    1
  },
  {
    "altivec_vsel_4si",
    "vsel %0,%1,%2,%3",
    (insn_gen_fn) gen_altivec_vsel_4si,
    &operand_data[1328],
    4,
    0,
    1,
    1
  },
  {
    "altivec_vsel_4sf",
    "vsel %0,%1,%2,%3",
    (insn_gen_fn) gen_altivec_vsel_4sf,
    &operand_data[1332],
    4,
    0,
    1,
    1
  },
  {
    "altivec_vsel_8hi",
    "vsel %0,%1,%2,%3",
    (insn_gen_fn) gen_altivec_vsel_8hi,
    &operand_data[1270],
    4,
    0,
    1,
    1
  },
  {
    "altivec_vsel_16qi",
    "vsel %0,%1,%2,%3",
    (insn_gen_fn) gen_altivec_vsel_16qi,
    &operand_data[1318],
    4,
    0,
    1,
    1
  },
  {
    "altivec_vsldoi_4si",
    "vsldoi %0, %1, %2, %3",
    (insn_gen_fn) gen_altivec_vsldoi_4si,
    &operand_data[1336],
    4,
    0,
    1,
    1
  },
  {
    "altivec_vsldoi_4sf",
    "vsldoi %0, %1, %2, %3",
    (insn_gen_fn) gen_altivec_vsldoi_4sf,
    &operand_data[1340],
    4,
    0,
    1,
    1
  },
  {
    "altivec_vsldoi_8hi",
    "vsldoi %0, %1, %2, %3",
    (insn_gen_fn) gen_altivec_vsldoi_8hi,
    &operand_data[1344],
    4,
    0,
    1,
    1
  },
  {
    "altivec_vsldoi_16qi",
    "vsldoi %0, %1, %2, %3",
    (insn_gen_fn) gen_altivec_vsldoi_16qi,
    &operand_data[1348],
    4,
    0,
    1,
    1
  },
  {
    "altivec_vupkhsb",
    "vupkhsb %0, %1",
    (insn_gen_fn) gen_altivec_vupkhsb,
    &operand_data[1274],
    2,
    0,
    1,
    1
  },
  {
    "altivec_vupkhpx",
    "vupkhpx %0, %1",
    (insn_gen_fn) gen_altivec_vupkhpx,
    &operand_data[1266],
    2,
    0,
    1,
    1
  },
  {
    "altivec_vupkhsh",
    "vupkhsh %0, %1",
    (insn_gen_fn) gen_altivec_vupkhsh,
    &operand_data[1266],
    2,
    0,
    1,
    1
  },
  {
    "altivec_vupklsb",
    "vupklsb %0, %1",
    (insn_gen_fn) gen_altivec_vupklsb,
    &operand_data[1274],
    2,
    0,
    1,
    1
  },
  {
    "altivec_vupklpx",
    "vupklpx %0, %1",
    (insn_gen_fn) gen_altivec_vupklpx,
    &operand_data[1266],
    2,
    0,
    1,
    1
  },
  {
    "altivec_vupklsh",
    "vupklsh %0, %1",
    (insn_gen_fn) gen_altivec_vupklsh,
    &operand_data[1266],
    2,
    0,
    1,
    1
  },
  {
    "altivec_predicate_v4si",
    "%3 %0,%1,%2",
    (insn_gen_fn) gen_altivec_predicate_v4si,
    &operand_data[1352],
    4,
    0,
    1,
    1
  },
  {
    "altivec_predicate_v4sf",
    "%3 %0,%1,%2",
    (insn_gen_fn) gen_altivec_predicate_v4sf,
    &operand_data[1356],
    4,
    0,
    1,
    1
  },
  {
    "altivec_predicate_v8hi",
    "%3 %0,%1,%2",
    (insn_gen_fn) gen_altivec_predicate_v8hi,
    &operand_data[1360],
    4,
    0,
    1,
    1
  },
  {
    "altivec_predicate_v16qi",
    "%3 %0,%1,%2",
    (insn_gen_fn) gen_altivec_predicate_v16qi,
    &operand_data[1364],
    4,
    0,
    1,
    1
  },
  {
    "altivec_mtvscr",
    "mtvscr %0",
    (insn_gen_fn) gen_altivec_mtvscr,
    &operand_data[1250],
    1,
    0,
    1,
    1
  },
  {
    "altivec_mfvscr",
    "mfvscr %0",
    (insn_gen_fn) gen_altivec_mfvscr,
    &operand_data[1246],
    1,
    0,
    1,
    1
  },
  {
    "altivec_dssall",
    "dssall",
    (insn_gen_fn) gen_altivec_dssall,
    &operand_data[0],
    0,
    0,
    0,
    1
  },
  {
    "altivec_dss",
    "dss %0",
    (insn_gen_fn) gen_altivec_dss,
    &operand_data[1291],
    1,
    0,
    1,
    1
  },
  {
    "altivec_dst",
    "dst %0,%1,%2",
    (insn_gen_fn) gen_altivec_dst,
    &operand_data[1368],
    3,
    0,
    1,
    1
  },
  {
    "altivec_dstt",
    "dstt %0,%1,%2",
    (insn_gen_fn) gen_altivec_dstt,
    &operand_data[1368],
    3,
    0,
    1,
    1
  },
  {
    "altivec_dstst",
    "dstst %0,%1,%2",
    (insn_gen_fn) gen_altivec_dstst,
    &operand_data[1368],
    3,
    0,
    1,
    1
  },
  {
    "altivec_dststt",
    "dststt %0,%1,%2",
    (insn_gen_fn) gen_altivec_dststt,
    &operand_data[1368],
    3,
    0,
    1,
    1
  },
  {
    "altivec_lvsl",
    "lvsl %0,%1,%2",
    (insn_gen_fn) gen_altivec_lvsl,
    &operand_data[1371],
    3,
    0,
    1,
    1
  },
  {
    "altivec_lvsr",
    "lvsr %0,%1,%2",
    (insn_gen_fn) gen_altivec_lvsr,
    &operand_data[1371],
    3,
    0,
    1,
    1
  },
  {
    "altivec_lvebx",
    "lvebx %0,%1,%2",
    (insn_gen_fn) gen_altivec_lvebx,
    &operand_data[1371],
    3,
    0,
    1,
    1
  },
  {
    "altivec_lvehx",
    "lvehx %0,%1,%2",
    (insn_gen_fn) gen_altivec_lvehx,
    &operand_data[1374],
    3,
    0,
    1,
    1
  },
  {
    "altivec_lvewx",
    "lvewx %0,%1,%2",
    (insn_gen_fn) gen_altivec_lvewx,
    &operand_data[1377],
    3,
    0,
    1,
    1
  },
  {
    "altivec_lvxl",
    "lvxl %0,%1,%2",
    (insn_gen_fn) gen_altivec_lvxl,
    &operand_data[1377],
    3,
    0,
    1,
    1
  },
  {
    "altivec_lvx",
    "lvx %0,%1,%2",
    (insn_gen_fn) gen_altivec_lvx,
    &operand_data[1377],
    3,
    0,
    1,
    1
  },
  {
    "altivec_stvx",
    "stvx %2,%0,%1",
    (insn_gen_fn) gen_altivec_stvx,
    &operand_data[1378],
    3,
    0,
    1,
    1
  },
  {
    "altivec_stvxl",
    "stvxl %2,%0,%1",
    (insn_gen_fn) gen_altivec_stvxl,
    &operand_data[1378],
    3,
    0,
    1,
    1
  },
  {
    "altivec_stvebx",
    "stvebx %2,%0,%1",
    (insn_gen_fn) gen_altivec_stvebx,
    &operand_data[1381],
    3,
    0,
    1,
    1
  },
  {
    "altivec_stvehx",
    "stvehx %2,%0,%1",
    (insn_gen_fn) gen_altivec_stvehx,
    &operand_data[1384],
    3,
    0,
    1,
    1
  },
  {
    "altivec_stvewx",
    "stvewx %2,%0,%1",
    (insn_gen_fn) gen_altivec_stvewx,
    &operand_data[1378],
    3,
    0,
    1,
    1
  },
  {
    "absv16qi2",
    "vspltisb %2,0\n\tvsububm %3,%2,%1\n\tvmaxsb %0,%1,%3",
    (insn_gen_fn) gen_absv16qi2,
    &operand_data[1387],
    4,
    0,
    1,
    1
  },
  {
    "absv8hi2",
    "vspltisb %2,0\n\tvsubuhm %3,%2,%1\n\tvmaxsh %0,%1,%3",
    (insn_gen_fn) gen_absv8hi2,
    &operand_data[1391],
    4,
    0,
    1,
    1
  },
  {
    "absv4si2",
    "vspltisb %2,0\n\tvsubuwm %3,%2,%1\n\tvmaxsw %0,%1,%3",
    (insn_gen_fn) gen_absv4si2,
    &operand_data[1395],
    4,
    0,
    1,
    1
  },
  {
    "absv4sf2",
    "vspltisw %2, -1\n\tvslw %3,%2,%2\n\tvandc %0,%1,%3",
    (insn_gen_fn) gen_absv4sf2,
    &operand_data[1399],
    4,
    0,
    1,
    1
  },
  {
    "altivec_abss_v16qi",
    "vspltisb %2,0\n\tvsubsbs %3,%2,%1\n\tvmaxsb %0,%1,%3",
    (insn_gen_fn) gen_altivec_abss_v16qi,
    &operand_data[1387],
    4,
    0,
    1,
    1
  },
  {
    "altivec_abss_v8hi",
    "vspltisb %2,0\n\tvsubshs %3,%2,%1\n\tvmaxsh %0,%1,%3",
    (insn_gen_fn) gen_altivec_abss_v8hi,
    &operand_data[1391],
    4,
    0,
    1,
    1
  },
  {
    "altivec_abss_v4si",
    "vspltisb %2,0\n\tvsubsws %3,%2,%1\n\tvmaxsw %0,%1,%3",
    (insn_gen_fn) gen_altivec_abss_v4si,
    &operand_data[1395],
    4,
    0,
    1,
    1
  },
  {
    "zero_extendqidi2",
    0,
    (insn_gen_fn) gen_zero_extendqidi2,
    &operand_data[1403],
    2,
    0,
    0,
    0
  },
  {
    "zero_extendqidi2+1",
    0,
    0,
    &operand_data[1405],
    3,
    0,
    0,
    0
  },
  {
    "zero_extendqidi2+2",
    0,
    0,
    &operand_data[1403],
    3,
    0,
    0,
    0
  },
  {
    "zero_extendhidi2-2",
    0,
    0,
    &operand_data[1405],
    3,
    0,
    0,
    0
  },
  {
    "zero_extendhidi2-1",
    0,
    0,
    &operand_data[1403],
    3,
    0,
    0,
    0
  },
  {
    "zero_extendhidi2",
    0,
    (insn_gen_fn) gen_zero_extendhidi2,
    &operand_data[1408],
    2,
    0,
    0,
    0
  },
  {
    "zero_extendhidi2+1",
    0,
    0,
    &operand_data[1410],
    3,
    0,
    0,
    0
  },
  {
    "extendhidi2-1",
    0,
    0,
    &operand_data[1408],
    3,
    0,
    0,
    0
  },
  {
    "extendhidi2",
    0,
    (insn_gen_fn) gen_extendhidi2,
    &operand_data[1408],
    2,
    0,
    0,
    0
  },
  {
    "extendhidi2+1",
    0,
    0,
    &operand_data[1410],
    3,
    0,
    0,
    0
  },
  {
    "zero_extendsidi2-1",
    0,
    0,
    &operand_data[1408],
    3,
    0,
    0,
    0
  },
  {
    "zero_extendsidi2",
    0,
    (insn_gen_fn) gen_zero_extendsidi2,
    &operand_data[1413],
    2,
    0,
    0,
    0
  },
  {
    "zero_extendsidi2+1",
    0,
    0,
    &operand_data[1415],
    3,
    0,
    0,
    0
  },
  {
    "extendsidi2-1",
    0,
    0,
    &operand_data[1413],
    3,
    0,
    0,
    0
  },
  {
    "extendsidi2",
    0,
    (insn_gen_fn) gen_extendsidi2,
    &operand_data[1413],
    2,
    0,
    0,
    0
  },
  {
    "extendsidi2+1",
    0,
    0,
    &operand_data[1415],
    3,
    0,
    0,
    0
  },
  {
    "zero_extendqisi2-1",
    0,
    0,
    &operand_data[1413],
    3,
    0,
    0,
    0
  },
  {
    "zero_extendqisi2",
    0,
    (insn_gen_fn) gen_zero_extendqisi2,
    &operand_data[1418],
    2,
    0,
    0,
    0
  },
  {
    "zero_extendqisi2+1",
    0,
    0,
    &operand_data[1420],
    3,
    0,
    0,
    0
  },
  {
    "extendqisi2-1",
    0,
    0,
    &operand_data[1418],
    3,
    0,
    0,
    0
  },
  {
    "extendqisi2",
    0,
    (insn_gen_fn) gen_extendqisi2,
    &operand_data[1418],
    2,
    0,
    0,
    0
  },
  {
    "extendqisi2+1",
    0,
    0,
    &operand_data[1420],
    3,
    0,
    0,
    0
  },
  {
    "extendqisi2_power-1",
    0,
    0,
    &operand_data[1418],
    3,
    0,
    0,
    0
  },
  {
    "extendqisi2_power",
    0,
    (insn_gen_fn) gen_extendqisi2_power,
    &operand_data[1418],
    2,
    2,
    0,
    0
  },
  {
    "extendqisi2_no_power",
    0,
    (insn_gen_fn) gen_extendqisi2_no_power,
    &operand_data[1418],
    2,
    2,
    0,
    0
  },
  {
    "zero_extendqihi2",
    0,
    (insn_gen_fn) gen_zero_extendqihi2,
    &operand_data[1423],
    2,
    0,
    0,
    0
  },
  {
    "zero_extendqihi2+1",
    0,
    0,
    &operand_data[1425],
    3,
    0,
    0,
    0
  },
  {
    "extendqihi2-1",
    0,
    0,
    &operand_data[1423],
    3,
    0,
    0,
    0
  },
  {
    "extendqihi2",
    0,
    (insn_gen_fn) gen_extendqihi2,
    &operand_data[1423],
    2,
    0,
    0,
    0
  },
  {
    "extendqihi2+1",
    0,
    0,
    &operand_data[1425],
    3,
    0,
    0,
    0
  },
  {
    "extendqihi2_power-1",
    0,
    0,
    &operand_data[1423],
    3,
    0,
    0,
    0
  },
  {
    "extendqihi2_power",
    0,
    (insn_gen_fn) gen_extendqihi2_power,
    &operand_data[1423],
    2,
    2,
    0,
    0
  },
  {
    "extendqihi2_no_power",
    0,
    (insn_gen_fn) gen_extendqihi2_no_power,
    &operand_data[1423],
    2,
    2,
    0,
    0
  },
  {
    "zero_extendhisi2",
    0,
    (insn_gen_fn) gen_zero_extendhisi2,
    &operand_data[1428],
    2,
    0,
    0,
    0
  },
  {
    "zero_extendhisi2+1",
    0,
    0,
    &operand_data[1430],
    3,
    0,
    0,
    0
  },
  {
    "extendhisi2-1",
    0,
    0,
    &operand_data[1428],
    3,
    0,
    0,
    0
  },
  {
    "extendhisi2",
    0,
    (insn_gen_fn) gen_extendhisi2,
    &operand_data[1428],
    2,
    0,
    0,
    0
  },
  {
    "extendhisi2+1",
    0,
    0,
    &operand_data[1430],
    3,
    0,
    0,
    0
  },
  {
    "addsi3-1",
    0,
    0,
    &operand_data[1428],
    3,
    0,
    0,
    0
  },
  {
    "addsi3",
    0,
    (insn_gen_fn) gen_addsi3,
    &operand_data[1433],
    3,
    0,
    0,
    0
  },
  {
    "addsi3+1",
    0,
    0,
    &operand_data[1436],
    4,
    0,
    0,
    0
  },
  {
    "addsi3+2",
    0,
    0,
    &operand_data[1440],
    4,
    0,
    0,
    0
  },
  {
    "addsi3+3",
    0,
    0,
    &operand_data[1444],
    3,
    0,
    0,
    0
  },
  {
    "addsi3+4",
    0,
    0,
    &operand_data[1447],
    3,
    0,
    0,
    0
  },
  {
    "subsi3-3",
    0,
    0,
    &operand_data[1450],
    3,
    0,
    0,
    0
  },
  {
    "subsi3-2",
    0,
    0,
    &operand_data[1452],
    4,
    0,
    0,
    0
  },
  {
    "subsi3-1",
    0,
    0,
    &operand_data[1456],
    4,
    0,
    0,
    0
  },
  {
    "subsi3",
    0,
    (insn_gen_fn) gen_subsi3,
    &operand_data[1460],
    3,
    0,
    0,
    0
  },
  {
    "sminsi3",
    0,
    (insn_gen_fn) gen_sminsi3,
    &operand_data[1440],
    3,
    5,
    0,
    0
  },
  {
    "sminsi3+1",
    0,
    0,
    &operand_data[1463],
    4,
    0,
    0,
    0
  },
  {
    "smaxsi3",
    0,
    (insn_gen_fn) gen_smaxsi3,
    &operand_data[1440],
    3,
    5,
    0,
    0
  },
  {
    "smaxsi3+1",
    0,
    0,
    &operand_data[1463],
    4,
    0,
    0,
    0
  },
  {
    "uminsi3",
    0,
    (insn_gen_fn) gen_uminsi3,
    &operand_data[641],
    3,
    11,
    0,
    0
  },
  {
    "umaxsi3",
    0,
    (insn_gen_fn) gen_umaxsi3,
    &operand_data[641],
    3,
    11,
    0,
    0
  },
  {
    "umaxsi3+1",
    0,
    0,
    &operand_data[1436],
    4,
    0,
    0,
    0
  },
  {
    "abssi2-1",
    0,
    0,
    &operand_data[1440],
    4,
    0,
    0,
    0
  },
  {
    "abssi2",
    0,
    (insn_gen_fn) gen_abssi2,
    &operand_data[641],
    2,
    0,
    0,
    0
  },
  {
    "abssi2+1",
    0,
    0,
    &operand_data[1453],
    3,
    0,
    0,
    0
  },
  {
    "abssi2+2",
    0,
    0,
    &operand_data[1453],
    3,
    0,
    0,
    0
  },
  {
    "mulsi3-2",
    0,
    0,
    &operand_data[1447],
    3,
    0,
    0,
    0
  },
  {
    "mulsi3-1",
    0,
    0,
    &operand_data[1450],
    3,
    0,
    0,
    0
  },
  {
    "mulsi3",
    0,
    (insn_gen_fn) gen_mulsi3,
    &operand_data[1440],
    3,
    0,
    0,
    0
  },
  {
    "mulsi3+1",
    0,
    0,
    &operand_data[1467],
    5,
    0,
    0,
    0
  },
  {
    "mulsi3+2",
    0,
    0,
    &operand_data[1452],
    4,
    0,
    0,
    0
  },
  {
    "divmodsi4-2",
    0,
    0,
    &operand_data[1472],
    5,
    0,
    0,
    0
  },
  {
    "divmodsi4-1",
    0,
    0,
    &operand_data[1456],
    4,
    0,
    0,
    0
  },
  {
    "divmodsi4",
    0,
    (insn_gen_fn) gen_divmodsi4,
    &operand_data[641],
    4,
    2,
    0,
    0
  },
  {
    "udivsi3",
    0,
    (insn_gen_fn) gen_udivsi3,
    &operand_data[641],
    3,
    0,
    0,
    0
  },
  {
    "divsi3",
    0,
    (insn_gen_fn) gen_divsi3,
    &operand_data[1477],
    3,
    0,
    0,
    0
  },
  {
    "modsi3",
    0,
    (insn_gen_fn) gen_modsi3,
    &operand_data[1477],
    3,
    0,
    0,
    0
  },
  {
    "modsi3+1",
    0,
    0,
    &operand_data[1480],
    4,
    0,
    0,
    0
  },
  {
    "udivmodsi4_normal-1",
    0,
    0,
    &operand_data[1484],
    4,
    0,
    0,
    0
  },
  {
    "udivmodsi4_normal",
    0,
    (insn_gen_fn) gen_udivmodsi4_normal,
    &operand_data[1488],
    4,
    5,
    0,
    0
  },
  {
    "udivmodsi4_tests",
    0,
    (insn_gen_fn) gen_udivmodsi4_tests,
    &operand_data[1488],
    5,
    11,
    0,
    0
  },
  {
    "udivmodsi4",
    0,
    (insn_gen_fn) gen_udivmodsi4,
    &operand_data[1493],
    4,
    2,
    0,
    0
  },
  {
    "udivmodsi4+1",
    0,
    0,
    &operand_data[1497],
    5,
    0,
    0,
    0
  },
  {
    "udivmodsi4+2",
    0,
    0,
    &operand_data[1502],
    5,
    0,
    0,
    0
  },
  {
    "udivmodsi4+3",
    0,
    0,
    &operand_data[1507],
    5,
    0,
    0,
    0
  },
  {
    "udivmodsi4+4",
    0,
    0,
    &operand_data[1512],
    5,
    0,
    0,
    0
  },
  {
    "iorsi3-3",
    0,
    0,
    &operand_data[1517],
    3,
    0,
    0,
    0
  },
  {
    "iorsi3-2",
    0,
    0,
    &operand_data[1518],
    4,
    0,
    0,
    0
  },
  {
    "iorsi3-1",
    0,
    0,
    &operand_data[1517],
    4,
    0,
    0,
    0
  },
  {
    "iorsi3",
    0,
    (insn_gen_fn) gen_iorsi3,
    &operand_data[1522],
    3,
    0,
    0,
    0
  },
  {
    "xorsi3",
    0,
    (insn_gen_fn) gen_xorsi3,
    &operand_data[1522],
    3,
    0,
    0,
    0
  },
  {
    "xorsi3+1",
    0,
    0,
    &operand_data[1525],
    5,
    0,
    0,
    0
  },
  {
    "xorsi3+2",
    0,
    0,
    &operand_data[1530],
    5,
    0,
    0,
    0
  },
  {
    "xorsi3+3",
    0,
    0,
    &operand_data[1535],
    4,
    0,
    0,
    0
  },
  {
    "xorsi3+4",
    0,
    0,
    &operand_data[1525],
    5,
    0,
    0,
    0
  },
  {
    "xorsi3+5",
    0,
    0,
    &operand_data[1530],
    5,
    0,
    0,
    0
  },
  {
    "xorsi3+6",
    0,
    0,
    &operand_data[1525],
    5,
    0,
    0,
    0
  },
  {
    "insv-5",
    0,
    0,
    &operand_data[1530],
    5,
    0,
    0,
    0
  },
  {
    "insv-4",
    0,
    0,
    &operand_data[1539],
    5,
    0,
    0,
    0
  },
  {
    "insv-3",
    0,
    0,
    &operand_data[1539],
    5,
    0,
    0,
    0
  },
  {
    "insv-2",
    0,
    0,
    &operand_data[1539],
    5,
    0,
    0,
    0
  },
  {
    "insv-1",
    0,
    0,
    &operand_data[1539],
    5,
    0,
    0,
    0
  },
  {
    "insv",
    0,
    (insn_gen_fn) gen_insv,
    &operand_data[1544],
    4,
    0,
    0,
    0
  },
  {
    "extzv",
    0,
    (insn_gen_fn) gen_extzv,
    &operand_data[1547],
    4,
    0,
    0,
    0
  },
  {
    "extzv+1",
    0,
    0,
    &operand_data[1551],
    5,
    0,
    0,
    0
  },
  {
    "extzv+2",
    0,
    0,
    &operand_data[1556],
    5,
    0,
    0,
    0
  },
  {
    "extzv+3",
    0,
    0,
    &operand_data[1560],
    4,
    0,
    0,
    0
  },
  {
    "extzv+4",
    0,
    0,
    &operand_data[1477],
    4,
    0,
    0,
    0
  },
  {
    "extzv+5",
    0,
    0,
    &operand_data[1564],
    5,
    0,
    0,
    0
  },
  {
    "ashlsi3-5",
    0,
    0,
    &operand_data[1569],
    5,
    0,
    0,
    0
  },
  {
    "ashlsi3-4",
    0,
    0,
    &operand_data[1560],
    4,
    0,
    0,
    0
  },
  {
    "ashlsi3-3",
    0,
    0,
    &operand_data[1477],
    4,
    0,
    0,
    0
  },
  {
    "ashlsi3-2",
    0,
    0,
    &operand_data[1560],
    4,
    0,
    0,
    0
  },
  {
    "ashlsi3-1",
    0,
    0,
    &operand_data[1477],
    4,
    0,
    0,
    0
  },
  {
    "ashlsi3",
    0,
    (insn_gen_fn) gen_ashlsi3,
    &operand_data[1477],
    3,
    0,
    0,
    0
  },
  {
    "ashlsi3+1",
    0,
    0,
    &operand_data[1573],
    5,
    0,
    0,
    0
  },
  {
    "ashlsi3+2",
    0,
    0,
    &operand_data[1560],
    4,
    0,
    0,
    0
  },
  {
    "ashlsi3+3",
    0,
    0,
    &operand_data[1578],
    5,
    0,
    0,
    0
  },
  {
    "lshrsi3-3",
    0,
    0,
    &operand_data[1477],
    4,
    0,
    0,
    0
  },
  {
    "lshrsi3-2",
    0,
    0,
    &operand_data[1583],
    5,
    0,
    0,
    0
  },
  {
    "lshrsi3-1",
    0,
    0,
    &operand_data[1588],
    5,
    0,
    0,
    0
  },
  {
    "lshrsi3",
    0,
    (insn_gen_fn) gen_lshrsi3,
    &operand_data[1477],
    3,
    0,
    0,
    0
  },
  {
    "lshrsi3+1",
    0,
    0,
    &operand_data[1573],
    5,
    0,
    0,
    0
  },
  {
    "lshrsi3+2",
    0,
    0,
    &operand_data[1560],
    4,
    0,
    0,
    0
  },
  {
    "lshrsi3+3",
    0,
    0,
    &operand_data[1578],
    5,
    0,
    0,
    0
  },
  {
    "lshrsi3+4",
    0,
    0,
    &operand_data[1477],
    4,
    0,
    0,
    0
  },
  {
    "lshrsi3+5",
    0,
    0,
    &operand_data[1583],
    5,
    0,
    0,
    0
  },
  {
    "ashrsi3-5",
    0,
    0,
    &operand_data[1588],
    5,
    0,
    0,
    0
  },
  {
    "ashrsi3-4",
    0,
    0,
    &operand_data[1592],
    4,
    0,
    0,
    0
  },
  {
    "ashrsi3-3",
    0,
    0,
    &operand_data[1596],
    4,
    0,
    0,
    0
  },
  {
    "ashrsi3-2",
    0,
    0,
    &operand_data[1592],
    4,
    0,
    0,
    0
  },
  {
    "ashrsi3-1",
    0,
    0,
    &operand_data[1596],
    4,
    0,
    0,
    0
  },
  {
    "ashrsi3",
    0,
    (insn_gen_fn) gen_ashrsi3,
    &operand_data[1477],
    3,
    0,
    0,
    0
  },
  {
    "ashrsi3+1",
    0,
    0,
    &operand_data[1573],
    5,
    0,
    0,
    0
  },
  {
    "ashrsi3+2",
    0,
    0,
    &operand_data[1560],
    4,
    0,
    0,
    0
  },
  {
    "negsf2-2",
    0,
    0,
    &operand_data[1578],
    5,
    0,
    0,
    0
  },
  {
    "negsf2-1",
    0,
    0,
    &operand_data[1477],
    4,
    0,
    0,
    0
  },
  {
    "negsf2",
    0,
    (insn_gen_fn) gen_negsf2,
    &operand_data[1600],
    2,
    0,
    0,
    0
  },
  {
    "abssf2",
    0,
    (insn_gen_fn) gen_abssf2,
    &operand_data[1600],
    2,
    0,
    0,
    0
  },
  {
    "addsf3",
    0,
    (insn_gen_fn) gen_addsf3,
    &operand_data[1600],
    3,
    0,
    0,
    0
  },
  {
    "subsf3",
    0,
    (insn_gen_fn) gen_subsf3,
    &operand_data[1600],
    3,
    0,
    0,
    0
  },
  {
    "mulsf3",
    0,
    (insn_gen_fn) gen_mulsf3,
    &operand_data[1600],
    3,
    0,
    0,
    0
  },
  {
    "divsf3",
    0,
    (insn_gen_fn) gen_divsf3,
    &operand_data[1600],
    3,
    0,
    0,
    0
  },
  {
    "sqrtsf2",
    0,
    (insn_gen_fn) gen_sqrtsf2,
    &operand_data[1600],
    2,
    0,
    0,
    0
  },
  {
    "maxsf3",
    0,
    (insn_gen_fn) gen_maxsf3,
    &operand_data[1600],
    3,
    2,
    0,
    0
  },
  {
    "minsf3",
    0,
    (insn_gen_fn) gen_minsf3,
    &operand_data[1600],
    3,
    2,
    0,
    0
  },
  {
    "minsf3+1",
    0,
    0,
    &operand_data[1600],
    4,
    0,
    0,
    0
  },
  {
    "movsfcc",
    0,
    (insn_gen_fn) gen_movsfcc,
    &operand_data[1604],
    4,
    0,
    0,
    0
  },
  {
    "maxdf3",
    0,
    (insn_gen_fn) gen_maxdf3,
    &operand_data[1608],
    3,
    2,
    0,
    0
  },
  {
    "mindf3",
    0,
    (insn_gen_fn) gen_mindf3,
    &operand_data[1608],
    3,
    2,
    0,
    0
  },
  {
    "mindf3+1",
    0,
    0,
    &operand_data[1608],
    4,
    0,
    0,
    0
  },
  {
    "movdfcc",
    0,
    (insn_gen_fn) gen_movdfcc,
    &operand_data[1612],
    4,
    0,
    0,
    0
  },
  {
    "floatsidf2",
    0,
    (insn_gen_fn) gen_floatsidf2,
    &operand_data[1615],
    2,
    5,
    0,
    0
  },
  {
    "floatsidf2+1",
    0,
    0,
    &operand_data[1615],
    7,
    0,
    0,
    0
  },
  {
    "floatunssidf2",
    0,
    (insn_gen_fn) gen_floatunssidf2,
    &operand_data[1615],
    2,
    4,
    0,
    0
  },
  {
    "floatunssidf2+1",
    0,
    0,
    &operand_data[1615],
    6,
    0,
    0,
    0
  },
  {
    "fix_truncdfsi2",
    0,
    (insn_gen_fn) gen_fix_truncdfsi2,
    &operand_data[1617],
    2,
    2,
    0,
    0
  },
  {
    "fix_truncdfsi2+1",
    0,
    0,
    &operand_data[1621],
    4,
    0,
    0,
    0
  },
  {
    "fix_truncdfsi2+2",
    0,
    0,
    &operand_data[1625],
    5,
    0,
    0,
    0
  },
  {
    "floatdisf2-1",
    0,
    0,
    &operand_data[1625],
    5,
    0,
    0,
    0
  },
  {
    "floatdisf2",
    0,
    (insn_gen_fn) gen_floatdisf2,
    &operand_data[1630],
    2,
    0,
    0,
    0
  },
  {
    "floatdisf2+1",
    0,
    0,
    &operand_data[1630],
    3,
    0,
    0,
    0
  },
  {
    "floatdisf2_internal2",
    0,
    (insn_gen_fn) gen_floatdisf2_internal2,
    &operand_data[1633],
    8,
    17,
    0,
    0
  },
  {
    "mulsidi3",
    0,
    (insn_gen_fn) gen_mulsidi3,
    &operand_data[1641],
    3,
    0,
    0,
    0
  },
  {
    "mulsidi3+1",
    0,
    0,
    &operand_data[1641],
    3,
    0,
    0,
    0
  },
  {
    "umulsidi3",
    0,
    (insn_gen_fn) gen_umulsidi3,
    &operand_data[1641],
    3,
    0,
    0,
    0
  },
  {
    "umulsidi3+1",
    0,
    0,
    &operand_data[1641],
    3,
    0,
    0,
    0
  },
  {
    "smulsi3_highpart",
    0,
    (insn_gen_fn) gen_smulsi3_highpart,
    &operand_data[1643],
    3,
    0,
    1,
    0
  },
  {
    "umulsi3_highpart",
    0,
    (insn_gen_fn) gen_umulsi3_highpart,
    &operand_data[641],
    3,
    0,
    0,
    0
  },
  {
    "adddi3",
    0,
    (insn_gen_fn) gen_adddi3,
    &operand_data[1646],
    3,
    0,
    0,
    0
  },
  {
    "adddi3+1",
    0,
    0,
    &operand_data[1649],
    4,
    0,
    0,
    0
  },
  {
    "adddi3+2",
    0,
    0,
    &operand_data[1653],
    4,
    0,
    0,
    0
  },
  {
    "adddi3+3",
    0,
    0,
    &operand_data[1657],
    3,
    0,
    0,
    0
  },
  {
    "adddi3+4",
    0,
    0,
    &operand_data[1660],
    3,
    0,
    0,
    0
  },
  {
    "subdi3-3",
    0,
    0,
    &operand_data[1663],
    3,
    0,
    0,
    0
  },
  {
    "subdi3-2",
    0,
    0,
    &operand_data[1665],
    4,
    0,
    0,
    0
  },
  {
    "subdi3-1",
    0,
    0,
    &operand_data[1669],
    4,
    0,
    0,
    0
  },
  {
    "subdi3",
    0,
    (insn_gen_fn) gen_subdi3,
    &operand_data[1673],
    3,
    0,
    0,
    0
  },
  {
    "subdi3+1",
    0,
    0,
    &operand_data[1666],
    3,
    0,
    0,
    0
  },
  {
    "negdi2-1",
    0,
    0,
    &operand_data[1666],
    3,
    0,
    0,
    0
  },
  {
    "negdi2",
    0,
    (insn_gen_fn) gen_negdi2,
    &operand_data[1628],
    2,
    0,
    0,
    0
  },
  {
    "negdi2+1",
    0,
    0,
    &operand_data[1660],
    3,
    0,
    0,
    0
  },
  {
    "divdi3-1",
    0,
    0,
    &operand_data[1663],
    3,
    0,
    0,
    0
  },
  {
    "divdi3",
    0,
    (insn_gen_fn) gen_divdi3,
    &operand_data[1676],
    3,
    0,
    0,
    0
  },
  {
    "moddi3",
    0,
    (insn_gen_fn) gen_moddi3,
    &operand_data[1676],
    3,
    0,
    0,
    0
  },
  {
    "moddi3+1",
    0,
    0,
    &operand_data[1679],
    4,
    0,
    0,
    0
  },
  {
    "moddi3+2",
    0,
    0,
    &operand_data[1683],
    4,
    0,
    0,
    0
  },
  {
    "moddi3+3",
    0,
    0,
    &operand_data[1686],
    4,
    0,
    0,
    0
  },
  {
    "moddi3+4",
    0,
    0,
    &operand_data[1676],
    4,
    0,
    0,
    0
  },
  {
    "moddi3+5",
    0,
    0,
    &operand_data[1690],
    5,
    0,
    0,
    0
  },
  {
    "moddi3+6",
    0,
    0,
    &operand_data[1695],
    5,
    0,
    0,
    0
  },
  {
    "ashldi3-6",
    0,
    0,
    &operand_data[1686],
    4,
    0,
    0,
    0
  },
  {
    "ashldi3-5",
    0,
    0,
    &operand_data[1676],
    4,
    0,
    0,
    0
  },
  {
    "ashldi3-4",
    0,
    0,
    &operand_data[1686],
    4,
    0,
    0,
    0
  },
  {
    "ashldi3-3",
    0,
    0,
    &operand_data[1676],
    4,
    0,
    0,
    0
  },
  {
    "ashldi3-2",
    0,
    0,
    &operand_data[1686],
    4,
    0,
    0,
    0
  },
  {
    "ashldi3-1",
    0,
    0,
    &operand_data[1676],
    4,
    0,
    0,
    0
  },
  {
    "ashldi3",
    0,
    (insn_gen_fn) gen_ashldi3,
    &operand_data[1700],
    3,
    0,
    0,
    0
  },
  {
    "ashldi3+1",
    0,
    0,
    &operand_data[1703],
    4,
    0,
    0,
    0
  },
  {
    "ashldi3+2",
    0,
    0,
    &operand_data[1700],
    4,
    0,
    0,
    0
  },
  {
    "ashldi3+3",
    0,
    0,
    &operand_data[1707],
    5,
    0,
    0,
    0
  },
  {
    "lshrdi3-3",
    0,
    0,
    &operand_data[1712],
    5,
    0,
    0,
    0
  },
  {
    "lshrdi3-2",
    0,
    0,
    &operand_data[1716],
    5,
    0,
    0,
    0
  },
  {
    "lshrdi3-1",
    0,
    0,
    &operand_data[1721],
    5,
    0,
    0,
    0
  },
  {
    "lshrdi3",
    0,
    (insn_gen_fn) gen_lshrdi3,
    &operand_data[1700],
    3,
    0,
    0,
    0
  },
  {
    "lshrdi3+1",
    0,
    0,
    &operand_data[1703],
    4,
    0,
    0,
    0
  },
  {
    "ashrdi3-1",
    0,
    0,
    &operand_data[1700],
    4,
    0,
    0,
    0
  },
  {
    "ashrdi3",
    0,
    (insn_gen_fn) gen_ashrdi3,
    &operand_data[1700],
    3,
    0,
    0,
    0
  },
  {
    "ashrdi3+1",
    0,
    0,
    &operand_data[1703],
    4,
    0,
    0,
    0
  },
  {
    "ashrdi3+2",
    0,
    0,
    &operand_data[1700],
    4,
    0,
    0,
    0
  },
  {
    "ashrdi3+3",
    0,
    0,
    &operand_data[1726],
    4,
    0,
    0,
    0
  },
  {
    "ashrdi3+4",
    0,
    0,
    &operand_data[1730],
    5,
    0,
    0,
    0
  },
  {
    "iordi3-3",
    0,
    0,
    &operand_data[1735],
    5,
    0,
    0,
    0
  },
  {
    "iordi3-2",
    0,
    0,
    &operand_data[1740],
    5,
    0,
    0,
    0
  },
  {
    "iordi3-1",
    0,
    0,
    &operand_data[1745],
    5,
    0,
    0,
    0
  },
  {
    "iordi3",
    0,
    (insn_gen_fn) gen_iordi3,
    &operand_data[1750],
    3,
    0,
    0,
    0
  },
  {
    "xordi3",
    0,
    (insn_gen_fn) gen_xordi3,
    &operand_data[1750],
    3,
    0,
    0,
    0
  },
  {
    "xordi3+1",
    0,
    0,
    &operand_data[1753],
    5,
    0,
    0,
    0
  },
  {
    "xordi3+2",
    0,
    0,
    &operand_data[1758],
    5,
    0,
    0,
    0
  },
  {
    "xordi3+3",
    0,
    0,
    &operand_data[1763],
    4,
    0,
    0,
    0
  },
  {
    "xordi3+4",
    0,
    0,
    &operand_data[1753],
    5,
    0,
    0,
    0
  },
  {
    "movsi_got-3",
    0,
    0,
    &operand_data[1758],
    5,
    0,
    0,
    0
  },
  {
    "movsi_got-2",
    0,
    0,
    &operand_data[1753],
    5,
    0,
    0,
    0
  },
  {
    "movsi_got-1",
    0,
    0,
    &operand_data[1758],
    5,
    0,
    0,
    0
  },
  {
    "movsi_got",
    0,
    (insn_gen_fn) gen_movsi_got,
    &operand_data[1767],
    2,
    1,
    0,
    0
  },
  {
    "movsi_got+1",
    0,
    0,
    &operand_data[1769],
    3,
    0,
    0,
    0
  },
  {
    "movsi",
    0,
    (insn_gen_fn) gen_movsi,
    &operand_data[1772],
    2,
    0,
    0,
    0
  },
  {
    "movsi+1",
    0,
    0,
    &operand_data[1552],
    2,
    0,
    0,
    0
  },
  {
    "movhi-1",
    0,
    0,
    &operand_data[1450],
    3,
    0,
    0,
    0
  },
  {
    "movhi",
    0,
    (insn_gen_fn) gen_movhi,
    &operand_data[1774],
    2,
    0,
    0,
    0
  },
  {
    "movqi",
    0,
    (insn_gen_fn) gen_movqi,
    &operand_data[1776],
    2,
    0,
    0,
    0
  },
  {
    "movcc",
    0,
    (insn_gen_fn) gen_movcc,
    &operand_data[1778],
    2,
    0,
    0,
    0
  },
  {
    "movsf",
    0,
    (insn_gen_fn) gen_movsf,
    &operand_data[1780],
    2,
    0,
    0,
    0
  },
  {
    "movsf+1",
    0,
    0,
    &operand_data[1782],
    2,
    0,
    0,
    0
  },
  {
    "movdf",
    0,
    (insn_gen_fn) gen_movdf,
    &operand_data[1784],
    2,
    0,
    0,
    0
  },
  {
    "movdf+1",
    0,
    0,
    &operand_data[1786],
    2,
    0,
    0,
    0
  },
  {
    "movdf+2",
    0,
    0,
    &operand_data[1788],
    2,
    0,
    0,
    0
  },
  {
    "movtf-1",
    0,
    0,
    &operand_data[1790],
    2,
    0,
    0,
    0
  },
  {
    "movtf",
    0,
    (insn_gen_fn) gen_movtf,
    &operand_data[1792],
    2,
    0,
    0,
    0
  },
  {
    "movtf+1",
    0,
    0,
    &operand_data[1794],
    2,
    0,
    0,
    0
  },
  {
    "movtf+2",
    0,
    0,
    &operand_data[1794],
    2,
    0,
    0,
    0
  },
  {
    "movtf+3",
    0,
    0,
    &operand_data[1796],
    3,
    0,
    0,
    0
  },
  {
    "movtf+4",
    0,
    0,
    &operand_data[1799],
    3,
    0,
    0,
    0
  },
  {
    "movdi-3",
    0,
    0,
    &operand_data[1802],
    3,
    0,
    0,
    0
  },
  {
    "movdi-2",
    0,
    0,
    &operand_data[1805],
    3,
    0,
    0,
    0
  },
  {
    "movdi-1",
    0,
    0,
    &operand_data[1808],
    3,
    0,
    0,
    0
  },
  {
    "movdi",
    0,
    (insn_gen_fn) gen_movdi,
    &operand_data[1811],
    2,
    0,
    0,
    0
  },
  {
    "movdi+1",
    0,
    0,
    &operand_data[1813],
    2,
    0,
    0,
    0
  },
  {
    "movdi+2",
    0,
    0,
    &operand_data[1815],
    2,
    0,
    0,
    0
  },
  {
    "movdi+3",
    0,
    0,
    &operand_data[1817],
    2,
    0,
    0,
    0
  },
  {
    "movti-3",
    0,
    0,
    &operand_data[1813],
    2,
    0,
    0,
    0
  },
  {
    "movti-2",
    0,
    0,
    &operand_data[1819],
    2,
    0,
    0,
    0
  },
  {
    "movti-1",
    0,
    0,
    &operand_data[1663],
    3,
    0,
    0,
    0
  },
  {
    "movti",
    0,
    (insn_gen_fn) gen_movti,
    &operand_data[1821],
    2,
    0,
    0,
    0
  },
  {
    "load_multiple",
    0,
    (insn_gen_fn) gen_load_multiple,
    &operand_data[1488],
    3,
    1,
    0,
    0
  },
  {
    "store_multiple",
    0,
    (insn_gen_fn) gen_store_multiple,
    &operand_data[1488],
    3,
    1,
    0,
    0
  },
  {
    "movstrsi",
    0,
    (insn_gen_fn) gen_movstrsi,
    &operand_data[1823],
    4,
    0,
    0,
    0
  },
  {
    "movstrsi_8reg",
    0,
    (insn_gen_fn) gen_movstrsi_8reg,
    &operand_data[1827],
    5,
    0,
    0,
    0
  },
  {
    "movstrsi_6reg",
    0,
    (insn_gen_fn) gen_movstrsi_6reg,
    &operand_data[1827],
    5,
    0,
    0,
    0
  },
  {
    "movstrsi_4reg",
    0,
    (insn_gen_fn) gen_movstrsi_4reg,
    &operand_data[1827],
    5,
    0,
    0,
    0
  },
  {
    "movstrsi_2reg",
    0,
    (insn_gen_fn) gen_movstrsi_2reg,
    &operand_data[1832],
    6,
    0,
    0,
    0
  },
  {
    "movstrsi_1reg",
    0,
    (insn_gen_fn) gen_movstrsi_1reg,
    &operand_data[1838],
    6,
    0,
    0,
    0
  },
  {
    "movstrsi_1reg+1",
    "lfq%U1%X1 %0,%1",
    0,
    &operand_data[1844],
    4,
    0,
    1,
    1
  },
  {
    "allocate_stack-1",
    "stfq%U0%X0 %1,%0",
    0,
    &operand_data[1847],
    4,
    0,
    1,
    1
  },
  {
    "allocate_stack",
    0,
    (insn_gen_fn) gen_allocate_stack,
    &operand_data[1851],
    2,
    1,
    1,
    0
  },
  {
    "save_stack_function",
    0,
    (insn_gen_fn) gen_save_stack_function,
    &operand_data[1853],
    2,
    0,
    0,
    0
  },
  {
    "restore_stack_function",
    0,
    (insn_gen_fn) gen_restore_stack_function,
    &operand_data[1853],
    2,
    0,
    0,
    0
  },
  {
    "restore_stack_block",
    0,
    (insn_gen_fn) gen_restore_stack_block,
    &operand_data[1855],
    2,
    5,
    0,
    0
  },
  {
    "save_stack_nonlocal",
    0,
    (insn_gen_fn) gen_save_stack_nonlocal,
    &operand_data[1857],
    2,
    0,
    0,
    0
  },
  {
    "restore_stack_nonlocal",
    0,
    (insn_gen_fn) gen_restore_stack_nonlocal,
    &operand_data[1856],
    2,
    0,
    0,
    0
  },
  {
    "builtin_setjmp_receiver",
    0,
    (insn_gen_fn) gen_builtin_setjmp_receiver,
    &operand_data[578],
    1,
    0,
    0,
    0
  },
  {
    "call_indirect_aix32",
    0,
    (insn_gen_fn) gen_call_indirect_aix32,
    &operand_data[1859],
    2,
    4,
    0,
    0
  },
  {
    "call_indirect_aix64",
    0,
    (insn_gen_fn) gen_call_indirect_aix64,
    &operand_data[1861],
    2,
    4,
    0,
    0
  },
  {
    "call_value_indirect_aix32",
    0,
    (insn_gen_fn) gen_call_value_indirect_aix32,
    &operand_data[1862],
    3,
    4,
    0,
    0
  },
  {
    "call_value_indirect_aix64",
    0,
    (insn_gen_fn) gen_call_value_indirect_aix64,
    &operand_data[1860],
    3,
    4,
    0,
    0
  },
  {
    "call",
    0,
    (insn_gen_fn) gen_call,
    &operand_data[1865],
    3,
    0,
    0,
    0
  },
  {
    "call_value",
    0,
    (insn_gen_fn) gen_call_value,
    &operand_data[1864],
    4,
    0,
    0,
    0
  },
  {
    "untyped_call",
    0,
    (insn_gen_fn) gen_untyped_call,
    &operand_data[1827],
    3,
    0,
    0,
    0
  },
  {
    "sibcall",
    0,
    (insn_gen_fn) gen_sibcall,
    &operand_data[1865],
    3,
    0,
    0,
    0
  },
  {
    "sibcall_value",
    0,
    (insn_gen_fn) gen_sibcall_value,
    &operand_data[1868],
    4,
    0,
    0,
    0
  },
  {
    "sibcall_epilogue",
    0,
    (insn_gen_fn) gen_sibcall_epilogue,
    &operand_data[0],
    0,
    0,
    0,
    0
  },
  {
    "cmpsi",
    0,
    (insn_gen_fn) gen_cmpsi,
    &operand_data[1437],
    2,
    0,
    0,
    0
  },
  {
    "cmpdi",
    0,
    (insn_gen_fn) gen_cmpdi,
    &operand_data[1650],
    2,
    0,
    0,
    0
  },
  {
    "cmpsf",
    0,
    (insn_gen_fn) gen_cmpsf,
    &operand_data[1600],
    2,
    0,
    0,
    0
  },
  {
    "cmpdf",
    0,
    (insn_gen_fn) gen_cmpdf,
    &operand_data[1608],
    2,
    0,
    0,
    0
  },
  {
    "cmptf",
    0,
    (insn_gen_fn) gen_cmptf,
    &operand_data[1872],
    2,
    0,
    0,
    0
  },
  {
    "beq",
    0,
    (insn_gen_fn) gen_beq,
    &operand_data[578],
    1,
    0,
    0,
    0
  },
  {
    "bne",
    0,
    (insn_gen_fn) gen_bne,
    &operand_data[578],
    1,
    0,
    0,
    0
  },
  {
    "bge",
    0,
    (insn_gen_fn) gen_bge,
    &operand_data[578],
    1,
    0,
    0,
    0
  },
  {
    "bgt",
    0,
    (insn_gen_fn) gen_bgt,
    &operand_data[578],
    1,
    0,
    0,
    0
  },
  {
    "ble",
    0,
    (insn_gen_fn) gen_ble,
    &operand_data[578],
    1,
    0,
    0,
    0
  },
  {
    "blt",
    0,
    (insn_gen_fn) gen_blt,
    &operand_data[578],
    1,
    0,
    0,
    0
  },
  {
    "bgeu",
    0,
    (insn_gen_fn) gen_bgeu,
    &operand_data[578],
    1,
    0,
    0,
    0
  },
  {
    "bgtu",
    0,
    (insn_gen_fn) gen_bgtu,
    &operand_data[578],
    1,
    0,
    0,
    0
  },
  {
    "bleu",
    0,
    (insn_gen_fn) gen_bleu,
    &operand_data[578],
    1,
    0,
    0,
    0
  },
  {
    "bltu",
    0,
    (insn_gen_fn) gen_bltu,
    &operand_data[578],
    1,
    0,
    0,
    0
  },
  {
    "bunordered",
    0,
    (insn_gen_fn) gen_bunordered,
    &operand_data[578],
    1,
    0,
    0,
    0
  },
  {
    "bordered",
    0,
    (insn_gen_fn) gen_bordered,
    &operand_data[578],
    1,
    0,
    0,
    0
  },
  {
    "buneq",
    0,
    (insn_gen_fn) gen_buneq,
    &operand_data[578],
    1,
    0,
    0,
    0
  },
  {
    "bunge",
    0,
    (insn_gen_fn) gen_bunge,
    &operand_data[578],
    1,
    0,
    0,
    0
  },
  {
    "bungt",
    0,
    (insn_gen_fn) gen_bungt,
    &operand_data[578],
    1,
    0,
    0,
    0
  },
  {
    "bunle",
    0,
    (insn_gen_fn) gen_bunle,
    &operand_data[578],
    1,
    0,
    0,
    0
  },
  {
    "bunlt",
    0,
    (insn_gen_fn) gen_bunlt,
    &operand_data[578],
    1,
    0,
    0,
    0
  },
  {
    "bltgt",
    0,
    (insn_gen_fn) gen_bltgt,
    &operand_data[578],
    1,
    0,
    0,
    0
  },
  {
    "seq",
    0,
    (insn_gen_fn) gen_seq,
    &operand_data[641],
    1,
    0,
    0,
    0
  },
  {
    "sne",
    0,
    (insn_gen_fn) gen_sne,
    &operand_data[641],
    1,
    0,
    0,
    0
  },
  {
    "sgt",
    0,
    (insn_gen_fn) gen_sgt,
    &operand_data[641],
    1,
    0,
    0,
    0
  },
  {
    "slt",
    0,
    (insn_gen_fn) gen_slt,
    &operand_data[641],
    1,
    0,
    0,
    0
  },
  {
    "sge",
    0,
    (insn_gen_fn) gen_sge,
    &operand_data[641],
    1,
    0,
    0,
    0
  },
  {
    "sle",
    0,
    (insn_gen_fn) gen_sle,
    &operand_data[641],
    1,
    0,
    0,
    0
  },
  {
    "sgtu",
    0,
    (insn_gen_fn) gen_sgtu,
    &operand_data[641],
    1,
    0,
    0,
    0
  },
  {
    "sltu",
    0,
    (insn_gen_fn) gen_sltu,
    &operand_data[641],
    1,
    0,
    0,
    0
  },
  {
    "sgeu",
    0,
    (insn_gen_fn) gen_sgeu,
    &operand_data[641],
    1,
    0,
    0,
    0
  },
  {
    "sleu",
    0,
    (insn_gen_fn) gen_sleu,
    &operand_data[641],
    1,
    0,
    0,
    0
  },
  {
    "sleu+1",
    0,
    0,
    &operand_data[1874],
    4,
    0,
    0,
    0
  },
  {
    "sleu+2",
    0,
    0,
    &operand_data[1877],
    5,
    0,
    0,
    0
  },
  {
    "sleu+3",
    0,
    0,
    &operand_data[1882],
    5,
    0,
    0,
    0
  },
  {
    "sleu+4",
    0,
    0,
    &operand_data[1887],
    4,
    0,
    0,
    0
  },
  {
    "sleu+5",
    0,
    0,
    &operand_data[1891],
    5,
    0,
    0,
    0
  },
  {
    "sleu+6",
    "%D1%D4mfcr %3\n\t{rlinm|rlwinm} %0,%3,%J1,1\n\t{rlinm|rlwinm} %3,%3,%J4,1",
    0,
    &operand_data[1896],
    6,
    0,
    1,
    1
  },
  {
    "sleu+7",
    "%D1%D4mfcr %3\n\t{rlinm|rlwinm} %0,%3,%J1,1\n\t{rlinm|rlwinm} %3,%3,%J4,1",
    0,
    &operand_data[1902],
    6,
    0,
    1,
    1
  },
  {
    "sleu+8",
    0,
    0,
    &operand_data[1908],
    5,
    0,
    0,
    0
  },
  {
    "sleu+9",
    0,
    0,
    &operand_data[1913],
    5,
    0,
    0,
    0
  },
  {
    "sleu+10",
    0,
    0,
    &operand_data[1918],
    6,
    0,
    0,
    0
  },
  {
    "sleu+11",
    0,
    0,
    &operand_data[1924],
    5,
    0,
    0,
    0
  },
  {
    "sleu+12",
    0,
    0,
    &operand_data[1493],
    5,
    0,
    0,
    0
  },
  {
    "sleu+13",
    0,
    0,
    &operand_data[1467],
    5,
    0,
    0,
    0
  },
  {
    "sleu+14",
    0,
    0,
    &operand_data[1665],
    4,
    0,
    0,
    0
  },
  {
    "sleu+15",
    0,
    0,
    &operand_data[1929],
    5,
    0,
    0,
    0
  },
  {
    "sleu+16",
    0,
    0,
    &operand_data[1934],
    5,
    0,
    0,
    0
  },
  {
    "sleu+17",
    0,
    0,
    &operand_data[1939],
    5,
    0,
    0,
    0
  },
  {
    "sleu+18",
    0,
    0,
    &operand_data[1943],
    5,
    0,
    0,
    0
  },
  {
    "sleu+19",
    0,
    0,
    &operand_data[1463],
    5,
    0,
    0,
    0
  },
  {
    "sleu+20",
    0,
    0,
    &operand_data[1653],
    4,
    0,
    0,
    0
  },
  {
    "sleu+21",
    0,
    0,
    &operand_data[1440],
    4,
    0,
    0,
    0
  },
  {
    "sleu+22",
    0,
    0,
    &operand_data[1943],
    5,
    0,
    0,
    0
  },
  {
    "sleu+23",
    0,
    0,
    &operand_data[1463],
    5,
    0,
    0,
    0
  },
  {
    "sleu+24",
    0,
    0,
    &operand_data[1943],
    5,
    0,
    0,
    0
  },
  {
    "sleu+25",
    0,
    0,
    &operand_data[1463],
    5,
    0,
    0,
    0
  },
  {
    "sleu+26",
    0,
    0,
    &operand_data[1440],
    4,
    0,
    0,
    0
  },
  {
    "sleu+27",
    0,
    0,
    &operand_data[1943],
    5,
    0,
    0,
    0
  },
  {
    "sleu+28",
    0,
    0,
    &operand_data[1463],
    5,
    0,
    0,
    0
  },
  {
    "indirect_jump-28",
    0,
    0,
    &operand_data[1948],
    4,
    0,
    0,
    0
  },
  {
    "indirect_jump-27",
    0,
    0,
    &operand_data[1951],
    5,
    0,
    0,
    0
  },
  {
    "indirect_jump-26",
    0,
    0,
    &operand_data[1956],
    5,
    0,
    0,
    0
  },
  {
    "indirect_jump-25",
    0,
    0,
    &operand_data[1939],
    5,
    0,
    0,
    0
  },
  {
    "indirect_jump-24",
    0,
    0,
    &operand_data[1943],
    5,
    0,
    0,
    0
  },
  {
    "indirect_jump-23",
    0,
    0,
    &operand_data[1463],
    5,
    0,
    0,
    0
  },
  {
    "indirect_jump-22",
    0,
    0,
    &operand_data[1948],
    4,
    0,
    0,
    0
  },
  {
    "indirect_jump-21",
    0,
    0,
    &operand_data[1961],
    4,
    0,
    0,
    0
  },
  {
    "indirect_jump-20",
    0,
    0,
    &operand_data[1951],
    5,
    0,
    0,
    0
  },
  {
    "indirect_jump-19",
    0,
    0,
    &operand_data[1956],
    5,
    0,
    0,
    0
  },
  {
    "indirect_jump-18",
    0,
    0,
    &operand_data[1951],
    5,
    0,
    0,
    0
  },
  {
    "indirect_jump-17",
    0,
    0,
    &operand_data[1956],
    5,
    0,
    0,
    0
  },
  {
    "indirect_jump-16",
    0,
    0,
    &operand_data[1450],
    3,
    0,
    0,
    0
  },
  {
    "indirect_jump-15",
    0,
    0,
    &operand_data[1663],
    3,
    0,
    0,
    0
  },
  {
    "indirect_jump-14",
    0,
    0,
    &operand_data[1440],
    4,
    0,
    0,
    0
  },
  {
    "indirect_jump-13",
    0,
    0,
    &operand_data[1452],
    4,
    0,
    0,
    0
  },
  {
    "indirect_jump-12",
    0,
    0,
    &operand_data[1665],
    4,
    0,
    0,
    0
  },
  {
    "indirect_jump-11",
    0,
    0,
    &operand_data[1456],
    4,
    0,
    0,
    0
  },
  {
    "indirect_jump-10",
    0,
    0,
    &operand_data[1669],
    4,
    0,
    0,
    0
  },
  {
    "indirect_jump-9",
    0,
    0,
    &operand_data[1943],
    5,
    0,
    0,
    0
  },
  {
    "indirect_jump-8",
    0,
    0,
    &operand_data[1463],
    5,
    0,
    0,
    0
  },
  {
    "indirect_jump-7",
    0,
    0,
    &operand_data[1440],
    4,
    0,
    0,
    0
  },
  {
    "indirect_jump-6",
    0,
    0,
    &operand_data[1653],
    4,
    0,
    0,
    0
  },
  {
    "indirect_jump-5",
    0,
    0,
    &operand_data[1943],
    5,
    0,
    0,
    0
  },
  {
    "indirect_jump-4",
    0,
    0,
    &operand_data[1964],
    5,
    0,
    0,
    0
  },
  {
    "indirect_jump-3",
    0,
    0,
    &operand_data[1463],
    5,
    0,
    0,
    0
  },
  {
    "indirect_jump-2",
    0,
    0,
    &operand_data[1969],
    5,
    0,
    0,
    0
  },
  {
    "indirect_jump-1",
    0,
    0,
    &operand_data[1974],
    5,
    0,
    0,
    0
  },
  {
    "indirect_jump",
    0,
    (insn_gen_fn) gen_indirect_jump,
    &operand_data[1855],
    1,
    0,
    0,
    0
  },
  {
    "tablejump",
    0,
    (insn_gen_fn) gen_tablejump,
    &operand_data[1827],
    2,
    0,
    0,
    0
  },
  {
    "tablejumpsi",
    0,
    (insn_gen_fn) gen_tablejumpsi,
    &operand_data[1826],
    2,
    3,
    0,
    0
  },
  {
    "tablejumpdi",
    0,
    (insn_gen_fn) gen_tablejumpdi,
    &operand_data[1979],
    2,
    5,
    1,
    0
  },
  {
    "doloop_end",
    0,
    (insn_gen_fn) gen_doloop_end,
    &operand_data[1980],
    5,
    0,
    0,
    0
  },
  {
    "ctrsi",
    0,
    (insn_gen_fn) gen_ctrsi,
    &operand_data[1985],
    4,
    2,
    0,
    0
  },
  {
    "ctrdi",
    0,
    (insn_gen_fn) gen_ctrdi,
    &operand_data[1989],
    4,
    2,
    0,
    0
  },
  {
    "ctrdi+1",
    0,
    0,
    &operand_data[1993],
    7,
    0,
    0,
    0
  },
  {
    "ctrdi+2",
    0,
    0,
    &operand_data[2000],
    7,
    0,
    0,
    0
  },
  {
    "conditional_trap-2",
    0,
    0,
    &operand_data[2007],
    7,
    0,
    0,
    0
  },
  {
    "conditional_trap-1",
    0,
    0,
    &operand_data[2014],
    7,
    0,
    0,
    0
  },
  {
    "conditional_trap",
    0,
    (insn_gen_fn) gen_conditional_trap,
    &operand_data[2021],
    2,
    2,
    0,
    0
  },
  {
    "prologue",
    0,
    (insn_gen_fn) gen_prologue,
    &operand_data[0],
    0,
    0,
    0,
    0
  },
  {
    "epilogue",
    0,
    (insn_gen_fn) gen_epilogue,
    &operand_data[0],
    0,
    0,
    0,
    0
  },
  {
    "movsi_to_cr_one",
    0,
    (insn_gen_fn) gen_movsi_to_cr_one,
    &operand_data[853],
    2,
    1,
    1,
    0
  },
  {
    "eh_return",
    0,
    (insn_gen_fn) gen_eh_return,
    &operand_data[2023],
    1,
    0,
    0,
    0
  },
  {
    "eh_return+1",
    0,
    0,
    &operand_data[2024],
    2,
    0,
    0,
    0
  },
  {
    "movv4si",
    0,
    (insn_gen_fn) gen_movv4si,
    &operand_data[2026],
    2,
    0,
    0,
    0
  },
  {
    "movv8hi",
    0,
    (insn_gen_fn) gen_movv8hi,
    &operand_data[2028],
    2,
    0,
    0,
    0
  },
  {
    "movv16qi",
    0,
    (insn_gen_fn) gen_movv16qi,
    &operand_data[2030],
    2,
    0,
    0,
    0
  },
  {
    "movv4sf",
    0,
    (insn_gen_fn) gen_movv4sf,
    &operand_data[2032],
    2,
    0,
    0,
    0
  },
  {
    "mulv4sf3",
    0,
    (insn_gen_fn) gen_mulv4sf3,
    &operand_data[2034],
    3,
    0,
    0,
    0
  },
  {
    "cr6_test_for_zero",
    0,
    (insn_gen_fn) gen_cr6_test_for_zero,
    &operand_data[1232],
    1,
    0,
    1,
    0
  },
  {
    "cr6_test_for_zero_reverse",
    0,
    (insn_gen_fn) gen_cr6_test_for_zero_reverse,
    &operand_data[1232],
    1,
    2,
    1,
    0
  },
  {
    "cr6_test_for_lt",
    0,
    (insn_gen_fn) gen_cr6_test_for_lt,
    &operand_data[1232],
    1,
    0,
    1,
    0
  },
  {
    "cr6_test_for_lt_reverse",
    0,
    (insn_gen_fn) gen_cr6_test_for_lt_reverse,
    &operand_data[1232],
    1,
    2,
    1,
    0
  },
};


const char *
get_insn_name (code)
     int code;
{
  return insn_data[code].name;
}
