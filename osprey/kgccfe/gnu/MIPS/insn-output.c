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

static const char *output_0 PARAMS ((rtx *, rtx));

static const char *
output_0 (operands, insn)
     rtx *operands ATTRIBUTE_UNUSED;
     rtx insn ATTRIBUTE_UNUSED;
{

{
  if (ISA_HAS_COND_TRAP)
    return "teq\t$0,$0";
  /* The IRIX 6 O32 assembler requires the first break operand.  */
  else if (TARGET_MIPS16 || ! TARGET_GAS)
    return "break 0";
  else
    return "break";
}
}

static const char *output_7 PARAMS ((rtx *, rtx));

static const char *
output_7 (operands, insn)
     rtx *operands ATTRIBUTE_UNUSED;
     rtx insn ATTRIBUTE_UNUSED;
{

{
  if (REGNO (operands[0]) == REGNO (operands[1]))
    return "addu\t%0,%2";
  return "addu\t%0,%1,%2";
}
}

static const char *output_8 PARAMS ((rtx *, rtx));

static const char *
output_8 (operands, insn)
     rtx *operands ATTRIBUTE_UNUSED;
     rtx insn ATTRIBUTE_UNUSED;
{

{
  return (REGNO (operands[0]) == REGNO (operands[1])
	  && REGNO (operands[0]) == REGNO (operands[2]))
    ? "srl\t%3,%L0,31\n\tsll\t%M0,%M0,1\n\tsll\t%L0,%L1,1\n\taddu\t%M0,%M0,%3"
    : "addu\t%L0,%L1,%L2\n\tsltu\t%3,%L0,%L2\n\taddu\t%M0,%M1,%M2\n\taddu\t%M0,%M0,%3";
}
}

static const char * const output_9[] = {
  "addu\t%L0,%L1,%2\n\tsltu\t%3,%L0,%2\n\taddu\t%M0,%M1,%3",
  "move\t%L0,%L1\n\tmove\t%M0,%M1",
  "subu\t%L0,%L1,%n2\n\tsltu\t%3,%L0,%2\n\tsubu\t%M0,%M1,1\n\taddu\t%M0,%M0,%3",
};

static const char *output_10 PARAMS ((rtx *, rtx));

static const char *
output_10 (operands, insn)
     rtx *operands ATTRIBUTE_UNUSED;
     rtx insn ATTRIBUTE_UNUSED;
{

{
  return (GET_CODE (operands[2]) == CONST_INT && INTVAL (operands[2]) < 0)
    ? "dsubu\t%0,%z1,%n2"
    : "daddu\t%0,%z1,%2";
}
}

static const char *output_13 PARAMS ((rtx *, rtx));

static const char *
output_13 (operands, insn)
     rtx *operands ATTRIBUTE_UNUSED;
     rtx insn ATTRIBUTE_UNUSED;
{

{
  if (REGNO (operands[0]) == REGNO (operands[1]))
    return "daddu\t%0,%2";
  return "daddu\t%0,%1,%2";
}
}

static const char *output_14 PARAMS ((rtx *, rtx));

static const char *
output_14 (operands, insn)
     rtx *operands ATTRIBUTE_UNUSED;
     rtx insn ATTRIBUTE_UNUSED;
{

{
  return (GET_CODE (operands[2]) == CONST_INT && INTVAL (operands[2]) < 0)
    ? "subu\t%0,%z1,%n2"
    : "addu\t%0,%z1,%2";
}
}

static const char *output_15 PARAMS ((rtx *, rtx));

static const char *
output_15 (operands, insn)
     rtx *operands ATTRIBUTE_UNUSED;
     rtx insn ATTRIBUTE_UNUSED;
{

{
  if (REGNO (operands[0]) == REGNO (operands[1]))
    return "addu\t%0,%2";
  return "addu\t%0,%1,%2";
}
}

static const char *output_21 PARAMS ((rtx *, rtx));

static const char *
output_21 (operands, insn)
     rtx *operands ATTRIBUTE_UNUSED;
     rtx insn ATTRIBUTE_UNUSED;
{

{
  if (REGNO (operands[0]) == REGNO (operands[1]))
    return "subu\t%0,%2";
  return "subu\t%0,%1,%2";
}
}

static const char * const output_23[] = {
  "sltu\t%3,%L1,%2\n\tsubu\t%L0,%L1,%2\n\tsubu\t%M0,%M1,%3",
  "move\t%L0,%L1\n\tmove\t%M0,%M1",
  "sltu\t%3,%L1,%2\n\tsubu\t%L0,%L1,%2\n\tsubu\t%M0,%M1,1\n\tsubu\t%M0,%M0,%3",
};

static const char *output_24 PARAMS ((rtx *, rtx));

static const char *
output_24 (operands, insn)
     rtx *operands ATTRIBUTE_UNUSED;
     rtx insn ATTRIBUTE_UNUSED;
{

{
  return (GET_CODE (operands[2]) == CONST_INT && INTVAL (operands[2]) < 0)
    ? "daddu\t%0,%z1,%n2"
    : "dsubu\t%0,%z1,%2";
}
}

static const char *output_27 PARAMS ((rtx *, rtx));

static const char *
output_27 (operands, insn)
     rtx *operands ATTRIBUTE_UNUSED;
     rtx insn ATTRIBUTE_UNUSED;
{

{
  if (REGNO (operands[0]) == REGNO (operands[1]))
    return "dsubu\t%0,%2";
  return "dsubu\t%0,%1,%2";
}
}

static const char *output_28 PARAMS ((rtx *, rtx));

static const char *
output_28 (operands, insn)
     rtx *operands ATTRIBUTE_UNUSED;
     rtx insn ATTRIBUTE_UNUSED;
{

{
  return (GET_CODE (operands[2]) == CONST_INT && INTVAL (operands[2]) < 0)
    ? "addu\t%0,%z1,%n2"
    : "subu\t%0,%z1,%2";
}
}

static const char *output_29 PARAMS ((rtx *, rtx));

static const char *
output_29 (operands, insn)
     rtx *operands ATTRIBUTE_UNUSED;
     rtx insn ATTRIBUTE_UNUSED;
{

{
  if (REGNO (operands[0]) == REGNO (operands[1]))
    return "subu\t%0,%2";
  return "subu\t%0,%1,%2";
}
}

static const char *output_31 PARAMS ((rtx *, rtx));

static const char *
output_31 (operands, insn)
     rtx *operands ATTRIBUTE_UNUSED;
     rtx insn ATTRIBUTE_UNUSED;
{

{
  output_asm_insn ("mul.d\t%0,%1,%2", operands);
  if (TARGET_4300_MUL_FIX)
    output_asm_insn ("nop", operands);
  return "";
}
}

static const char *output_33 PARAMS ((rtx *, rtx));

static const char *
output_33 (operands, insn)
     rtx *operands ATTRIBUTE_UNUSED;
     rtx insn ATTRIBUTE_UNUSED;
{

{
  output_asm_insn ("mul.s\t%0,%1,%2", operands);
  if (TARGET_4300_MUL_FIX)
    output_asm_insn ("nop", operands);
  return "";
}
}

static const char *output_34 PARAMS ((rtx *, rtx));

static const char *
output_34 (operands, insn)
     rtx *operands ATTRIBUTE_UNUSED;
     rtx insn ATTRIBUTE_UNUSED;
{

{
  if (which_alternative == 1)
    return "mult\t%1,%2";
  if (TARGET_MAD
      || TARGET_MIPS5400
      || TARGET_MIPS5500
      || ISA_MIPS32
      || ISA_MIPS64)
    return "mul\t%0,%1,%2";
  return "mult\t%0,%1,%2";
}
}

static const char *output_36 PARAMS ((rtx *, rtx));

static const char *
output_36 (operands, insn)
     rtx *operands ATTRIBUTE_UNUSED;
     rtx insn ATTRIBUTE_UNUSED;
{

{
  rtx xoperands[10];

  xoperands[0] = operands[0];
  xoperands[1] = gen_rtx_REG (SImode, LO_REGNUM);

  output_asm_insn ("mult\t%1,%2", operands);
  output_asm_insn (mips_move_1word (xoperands, insn, FALSE), xoperands);
  return "";
}
}

static const char *output_37 PARAMS ((rtx *, rtx));

static const char *
output_37 (operands, insn)
     rtx *operands ATTRIBUTE_UNUSED;
     rtx insn ATTRIBUTE_UNUSED;
{

{
  static const char *const madd[] = { "madd\t%1,%2", "madd\t%0,%1,%2" };
  static const char *const macc[] = { "macc\t$0,%1,%2", "macc\t%0,%1,%2" };
  if (which_alternative == 2)
    return "#";
  if (ISA_HAS_MADD_MSUB && which_alternative != 0)
    return "#";

  if (TARGET_MIPS5400)
    return macc[which_alternative];

  if (TARGET_MIPS5500)
    {
      if (which_alternative == 0)
        return madd[0];
      else
        return macc[which_alternative];
    }

  return madd[which_alternative];
}
}

static const char *output_38 PARAMS ((rtx *, rtx));

static const char *
output_38 (operands, insn)
     rtx *operands ATTRIBUTE_UNUSED;
     rtx insn ATTRIBUTE_UNUSED;
{

{
  if (which_alternative != 0)
    return "#";
  return "msub\t%2,%3";
}
}

static const char * const output_39[] = {
  "muls\t$0,%1,%2",
  "muls\t%0,%1,%2",
};

static const char * const output_40[] = {
  "msac\t$0,%2,%3",
  "msac\t%0,%2,%3",
  "#",
};

static const char *output_42 PARAMS ((rtx *, rtx));

static const char *
output_42 (operands, insn)
     rtx *operands ATTRIBUTE_UNUSED;
     rtx insn ATTRIBUTE_UNUSED;
{

{
  if (GENERATE_MULT3_DI)
    output_asm_insn ("dmult\t%0,%1,%2", operands);
  else
    {
      rtx xoperands[10];

      xoperands[0] = operands[0];
      xoperands[1] = gen_rtx_REG (DImode, LO_REGNUM);

      output_asm_insn ("dmult\t%1,%2", operands);
      output_asm_insn (mips_move_1word (xoperands, insn, FALSE), xoperands);
    }
  return "";
}
}

static const char *output_43 PARAMS ((rtx *, rtx));

static const char *
output_43 (operands, insn)
     rtx *operands ATTRIBUTE_UNUSED;
     rtx insn ATTRIBUTE_UNUSED;
{

{
  if (GET_CODE (operands[3]) == SIGN_EXTEND)
    return "mult\t%1,%2";
  return "multu\t%1,%2";
}
}

static const char *output_44 PARAMS ((rtx *, rtx));

static const char *
output_44 (operands, insn)
     rtx *operands ATTRIBUTE_UNUSED;
     rtx insn ATTRIBUTE_UNUSED;
{

{
  if (GET_CODE (operands[3]) == SIGN_EXTEND)
    return "mult\t%1,%2";
  return "multu\t%1,%2";
}
}

static const char *output_45 PARAMS ((rtx *, rtx));

static const char *
output_45 (operands, insn)
     rtx *operands ATTRIBUTE_UNUSED;
     rtx insn ATTRIBUTE_UNUSED;
{

{
  if (GET_CODE (operands[3]) == SIGN_EXTEND)
    return "muls\t$0,%1,%2";
  else
    return "mulsu\t$0,%1,%2";
}
}

static const char *output_46 PARAMS ((rtx *, rtx));

static const char *
output_46 (operands, insn)
     rtx *operands ATTRIBUTE_UNUSED;
     rtx insn ATTRIBUTE_UNUSED;
{

{
  if (GET_CODE (operands[4]) == SIGN_EXTEND)
     {
       if (TARGET_MIPS5500)
         return "msub\t%1,%2";
       else
    return "msac\t$0,%1,%2";
    }
  else
     {
       if (TARGET_MIPS5500)
         return "msubu\t%1,%2";
       else
    return "msacu\t$0,%1,%2";
    }
}
}

static const char *output_47 PARAMS ((rtx *, rtx));

static const char *
output_47 (operands, insn)
     rtx *operands ATTRIBUTE_UNUSED;
     rtx insn ATTRIBUTE_UNUSED;
{

{
  if (GET_CODE (operands[3]) == SIGN_EXTEND)
    return "mult\t%1,%2";
  else
    return "multu\t%1,%2";
}
}

static const char *output_48 PARAMS ((rtx *, rtx));

static const char *
output_48 (operands, insn)
     rtx *operands ATTRIBUTE_UNUSED;
     rtx insn ATTRIBUTE_UNUSED;
{

{
  static char const *const sign[] = { "mult\t%1,%2",  "mulhi\t%0,%1,%2"  };
  static char const *const zero[] = { "multu\t%1,%2", "mulhiu\t%0,%1,%2" };
  if (GET_CODE (operands[3]) == SIGN_EXTEND)
    return sign[which_alternative];
  else
    return zero[which_alternative];
}
}

static const char *output_49 PARAMS ((rtx *, rtx));

static const char *
output_49 (operands, insn)
     rtx *operands ATTRIBUTE_UNUSED;
     rtx insn ATTRIBUTE_UNUSED;
{

{
  static char const *const sign[] = { "mulshi\t$0,%1,%2",  "mulshi\t%0,%1,%2"  };
  static char const *const zero[] = { "mulshiu\t$0,%1,%2", "mulshiu\t%0,%1,%2" };
  if (GET_CODE (operands[3]) == SIGN_EXTEND)
    return sign[which_alternative];
  else
    return zero[which_alternative];
}
}

static const char *output_53 PARAMS ((rtx *, rtx));

static const char *
output_53 (operands, insn)
     rtx *operands ATTRIBUTE_UNUSED;
     rtx insn ATTRIBUTE_UNUSED;
{

{
  if (GET_CODE (operands[3]) == SIGN_EXTEND)
    return "mad\t%1,%2";
  else
    return "madu\t%1,%2";
}
}

static const char *output_54 PARAMS ((rtx *, rtx));

static const char *
output_54 (operands, insn)
     rtx *operands ATTRIBUTE_UNUSED;
     rtx insn ATTRIBUTE_UNUSED;
{

{
  if (TARGET_MAD)
    {
      if (GET_CODE (operands[3]) == SIGN_EXTEND)
        return "mad\t%1,%2";
      else
        return "madu\t%1,%2";
    }
  else if (ISA_HAS_MACC)
    {
      if (GET_CODE (operands[3]) == SIGN_EXTEND)
        {
          if (TARGET_MIPS5500)
            return "madd\t%1,%2";
          else
        return "macc\t$0,%1,%2";
        }
      else
        {
          if (TARGET_MIPS5500)
            return "maddu\t%1,%2";
          else
        return "maccu\t$0,%1,%2";
        }
    }
  else
    abort ();

}
}

static const char *output_71 PARAMS ((rtx *, rtx));

static const char *
output_71 (operands, insn)
     rtx *operands ATTRIBUTE_UNUSED;
     rtx insn ATTRIBUTE_UNUSED;
{

{
  rtx link;
  int have_dep_anti = 0;

  /* For divmod if one division is not needed then we don't need an extra
     divide by zero trap, which is anti dependent on previous trap */
  for (link = LOG_LINKS (insn); link; link = XEXP (link, 1))

    if ((int) REG_DEP_ANTI == (int) REG_NOTE_KIND (link)
        && GET_CODE (XEXP (link, 0)) == INSN
        && GET_CODE (PATTERN (XEXP (link, 0))) == TRAP_IF
	&& which_alternative == 1)
      have_dep_anti = 1;
  if (! have_dep_anti)
    {
      if (GENERATE_BRANCHLIKELY)
	{
          if (which_alternative == 1)
	    return "%(beql\t%0,$0,1f\n\tbreak\t%2\n%~1:%)";
	  else
	    return "%(beql\t%0,%1,1f\n\tbreak\t%2\n%~1:%)";
	}
      else
	{
          if (which_alternative == 1)
	    return "%(bne\t%0,$0,1f\n\tnop\n\tbreak\t%2\n%~1:%)";
	  else
	    return "%(bne\t%0,%1,1f\n\tnop\n\tbreak\t%2\n%~1:%)";
	}
    }
  return "";
}
}

static const char *output_72 PARAMS ((rtx *, rtx));

static const char *
output_72 (operands, insn)
     rtx *operands ATTRIBUTE_UNUSED;
     rtx insn ATTRIBUTE_UNUSED;
{

{
  rtx link;
  int have_dep_anti = 0;

  /* For divmod if one division is not needed then we don't need an extra
     divide by zero trap, which is anti dependent on previous trap */
  for (link = LOG_LINKS (insn); link; link = XEXP (link, 1))

    if ((int) REG_DEP_ANTI == (int) REG_NOTE_KIND (link)
        && GET_CODE (XEXP (link, 0)) == INSN
        && GET_CODE (PATTERN (XEXP (link, 0))) == TRAP_IF
	&& which_alternative == 1)
      have_dep_anti = 1;
  if (! have_dep_anti)
    {
      /* No branch delay slots on mips16.  */
      if (which_alternative == 1)
        return "%(bnez\t%0,1f\n\tbreak\t%2\n%~1:%)";
      else
        return "%(bne\t%0,%1,1f\n\tbreak\t%2\n%~1:%)";
    }
  return "";
}
}

static const char *output_85 PARAMS ((rtx *, rtx));

static const char *
output_85 (operands, insn)
     rtx *operands ATTRIBUTE_UNUSED;
     rtx insn ATTRIBUTE_UNUSED;
{

{
  dslots_jump_total++;
  dslots_jump_filled++;
  operands[2] = const0_rtx;

  if (REGNO (operands[0]) == REGNO (operands[1]))
    {
      if (GENERATE_BRANCHLIKELY)
	return "%(bltzl\t%1,1f\n\tsubu\t%0,%z2,%0\n%~1:%)";
      else
	return "bgez\t%1,1f%#\n\tsubu\t%0,%z2,%0\n%~1:";
    }
  else
    return "%(bgez\t%1,1f\n\tmove\t%0,%1\n\tsubu\t%0,%z2,%0\n%~1:%)";
}
}

static const char *output_86 PARAMS ((rtx *, rtx));

static const char *
output_86 (operands, insn)
     rtx *operands ATTRIBUTE_UNUSED;
     rtx insn ATTRIBUTE_UNUSED;
{

{
  unsigned int regno1;
  dslots_jump_total++;
  dslots_jump_filled++;
  operands[2] = const0_rtx;

  if (GET_CODE (operands[1]) == REG)
    regno1 = REGNO (operands[1]);
  else
    regno1 = REGNO (XEXP (operands[1], 0));

  if (REGNO (operands[0]) == regno1)
    return "%(bltzl\t%1,1f\n\tdsubu\t%0,%z2,%0\n%~1:%)";
  else
    return "%(bgez\t%1,1f\n\tmove\t%0,%1\n\tdsubu\t%0,%z2,%0\n%~1:%)";
}
}

static const char *output_89 PARAMS ((rtx *, rtx));

static const char *
output_89 (operands, insn)
     rtx *operands ATTRIBUTE_UNUSED;
     rtx insn ATTRIBUTE_UNUSED;
{

{
  dslots_jump_total += 2;
  dslots_jump_filled += 2;
  operands[4] = const0_rtx;

  if (optimize && find_reg_note (insn, REG_DEAD, operands[1]))
    return "%(\
move\t%0,%z4\n\
\tbeq\t%1,%z4,2f\n\
%~1:\tand\t%2,%1,0x0001\n\
\taddu\t%0,%0,1\n\
\tbeq\t%2,%z4,1b\n\
\tsrl\t%1,%1,1\n\
%~2:%)";

  return "%(\
move\t%0,%z4\n\
\tmove\t%3,%1\n\
\tbeq\t%3,%z4,2f\n\
%~1:\tand\t%2,%3,0x0001\n\
\taddu\t%0,%0,1\n\
\tbeq\t%2,%z4,1b\n\
\tsrl\t%3,%3,1\n\
%~2:%)";
}
}

static const char *output_90 PARAMS ((rtx *, rtx));

static const char *
output_90 (operands, insn)
     rtx *operands ATTRIBUTE_UNUSED;
     rtx insn ATTRIBUTE_UNUSED;
{

{
  dslots_jump_total += 2;
  dslots_jump_filled += 2;
  operands[4] = const0_rtx;

  if (optimize && find_reg_note (insn, REG_DEAD, operands[1]))
    return "%(\
move\t%0,%z4\n\
\tbeq\t%1,%z4,2f\n\
%~1:\tand\t%2,%1,0x0001\n\
\tdaddu\t%0,%0,1\n\
\tbeq\t%2,%z4,1b\n\
\tdsrl\t%1,%1,1\n\
%~2:%)";

  return "%(\
move\t%0,%z4\n\
\tmove\t%3,%1\n\
\tbeq\t%3,%z4,2f\n\
%~1:\tand\t%2,%3,0x0001\n\
\tdaddu\t%0,%0,1\n\
\tbeq\t%2,%z4,1b\n\
\tdsrl\t%3,%3,1\n\
%~2:%)";
}
}

static const char *output_91 PARAMS ((rtx *, rtx));

static const char *
output_91 (operands, insn)
     rtx *operands ATTRIBUTE_UNUSED;
     rtx insn ATTRIBUTE_UNUSED;
{

{
  if (TARGET_MIPS16)
    return "neg\t%0,%1";
  operands[2] = const0_rtx;
  return "subu\t%0,%z2,%1";
}
}

static const char *output_92 PARAMS ((rtx *, rtx));

static const char *
output_92 (operands, insn)
     rtx *operands ATTRIBUTE_UNUSED;
     rtx insn ATTRIBUTE_UNUSED;
{

{
  operands[3] = const0_rtx;
  return "subu\t%L0,%z3,%L1\n\tsubu\t%M0,%z3,%M1\n\tsltu\t%2,%z3,%L0\n\tsubu\t%M0,%M0,%2";
}
}

static const char *output_93 PARAMS ((rtx *, rtx));

static const char *
output_93 (operands, insn)
     rtx *operands ATTRIBUTE_UNUSED;
     rtx insn ATTRIBUTE_UNUSED;
{

{
  operands[2] = const0_rtx;
  return "dsubu\t%0,%z2,%1";
}
}

static const char *output_96 PARAMS ((rtx *, rtx));

static const char *
output_96 (operands, insn)
     rtx *operands ATTRIBUTE_UNUSED;
     rtx insn ATTRIBUTE_UNUSED;
{

{
  if (TARGET_MIPS16)
    return "not\t%0,%1";
  operands[2] = const0_rtx;
  return "nor\t%0,%z2,%1";
}
}

static const char *output_97 PARAMS ((rtx *, rtx));

static const char *
output_97 (operands, insn)
     rtx *operands ATTRIBUTE_UNUSED;
     rtx insn ATTRIBUTE_UNUSED;
{

{
  if (TARGET_MIPS16)
    {
      if (TARGET_64BIT)
	return "not\t%0,%1";
      return "not\t%M0,%M1\n\tnot\t%L0,%L1";
    }
  operands[2] = const0_rtx;
  if (TARGET_64BIT)
    return "nor\t%0,%z2,%1";
  return "nor\t%M0,%z2,%M1\n\tnor\t%L0,%z2,%L1";
}
}

static const char * const output_98[] = {
  "and\t%0,%1,%2",
  "andi\t%0,%1,%x2",
};

static const char *output_100 PARAMS ((rtx *, rtx));

static const char *
output_100 (operands, insn)
     rtx *operands ATTRIBUTE_UNUSED;
     rtx insn ATTRIBUTE_UNUSED;
{

{
  if (TARGET_64BIT)
    return "and\t%0,%1,%2";
  return "and\t%M0,%M1,%M2\n\tand\t%L0,%L1,%L2";
}
}

static const char *output_101 PARAMS ((rtx *, rtx));

static const char *
output_101 (operands, insn)
     rtx *operands ATTRIBUTE_UNUSED;
     rtx insn ATTRIBUTE_UNUSED;
{

{
  if (TARGET_64BIT)
    return "and\t%0,%2";
  return "and\t%M0,%M2\n\tand\t%L0,%L2";
}
}

static const char * const output_102[] = {
  "and\t%0,%1,%2",
  "andi\t%0,%1,%x2",
};

static const char * const output_103[] = {
  "or\t%0,%1,%2",
  "ori\t%0,%1,%x2",
};

static const char *output_105 PARAMS ((rtx *, rtx));

static const char *
output_105 (operands, insn)
     rtx *operands ATTRIBUTE_UNUSED;
     rtx insn ATTRIBUTE_UNUSED;
{

{
  if (TARGET_64BIT)
    return "or\t%0,%1,%2";
  return "or\t%M0,%M1,%M2\n\tor\t%L0,%L1,%L2";
}
}

static const char *output_106 PARAMS ((rtx *, rtx));

static const char *
output_106 (operands, insn)
     rtx *operands ATTRIBUTE_UNUSED;
     rtx insn ATTRIBUTE_UNUSED;
{

{
  if (TARGET_64BIT)
    return "or\t%0,%2";
  return "or\t%M0,%M2\n\tor\t%L0,%L2";
}
}

static const char * const output_107[] = {
  "xor\t%0,%1,%2",
  "xori\t%0,%1,%x2",
};

static const char * const output_108[] = {
  "xor\t%0,%2",
  "cmpi\t%1,%2",
  "cmp\t%1,%2",
};

static const char *output_109 PARAMS ((rtx *, rtx));

static const char *
output_109 (operands, insn)
     rtx *operands ATTRIBUTE_UNUSED;
     rtx insn ATTRIBUTE_UNUSED;
{

{
  if (TARGET_64BIT)
    return "xor\t%0,%1,%2";
  return "xor\t%M0,%M1,%M2\n\txor\t%L0,%L1,%L2";
}
}

static const char * const output_111[] = {
  "xor\t%0,%2",
  "cmpi\t%1,%2",
  "cmp\t%1,%2",
};

static const char *output_114 PARAMS ((rtx *, rtx));

static const char *
output_114 (operands, insn)
     rtx *operands ATTRIBUTE_UNUSED;
     rtx insn ATTRIBUTE_UNUSED;
{

{
  if (TARGET_64BIT)
    return "nor\t%0,%z1,%z2";
  return "nor\t%M0,%M1,%M2\n\tnor\t%L0,%L1,%L2";
}
}

static const char *output_116 PARAMS ((rtx *, rtx));

static const char *
output_116 (operands, insn)
     rtx *operands ATTRIBUTE_UNUSED;
     rtx insn ATTRIBUTE_UNUSED;
{

{
  if (TARGET_MIPS16)
    return "dsll\t%0,%1,32\n\tdsra\t%0,32";
  return "dsll\t%0,%1,32\n\tdsra\t%0,%0,32";
}
}

static const char *output_117 PARAMS ((rtx *, rtx));

static const char *
output_117 (operands, insn)
     rtx *operands ATTRIBUTE_UNUSED;
     rtx insn ATTRIBUTE_UNUSED;
{

{
  if (TARGET_MIPS16)
    return "dsll\t%0,%1,48\n\tdsra\t%0,48";
  return "andi\t%0,%1,0xffff";
}
}

static const char *output_118 PARAMS ((rtx *, rtx));

static const char *
output_118 (operands, insn)
     rtx *operands ATTRIBUTE_UNUSED;
     rtx insn ATTRIBUTE_UNUSED;
{

{
  if (TARGET_MIPS16)
    return "dsll\t%0,%1,56\n\tdsra\t%0,56";
  return "andi\t%0,%1,0x00ff";
}
}

static const char *output_119 PARAMS ((rtx *, rtx));

static const char *
output_119 (operands, insn)
     rtx *operands ATTRIBUTE_UNUSED;
     rtx insn ATTRIBUTE_UNUSED;
{

{
  int shift_amt = INTVAL (operands[2]) & 0x3f;

  if (shift_amt < 32)
    {
      operands[2] = GEN_INT (32 - shift_amt);
      return "dsll\t%0,%1,%2\n\tdsra\t%0,%0,32";
    }
  else
    {
      operands[2] = GEN_INT (shift_amt);
      return "dsra\t%0,%1,%2";
    }
}
}

static const char *output_120 PARAMS ((rtx *, rtx));

static const char *
output_120 (operands, insn)
     rtx *operands ATTRIBUTE_UNUSED;
     rtx insn ATTRIBUTE_UNUSED;
{

{
  int shift_amt = INTVAL (operands[2]) & 0x3f;

  if (shift_amt < 32)
    {
      operands[2] = GEN_INT (32 - shift_amt);
      return "dsll\t%0,%1,%2\n\tdsra\t%0,%0,32";
    }
  else if (shift_amt == 32)
    return "dsra\t%0,%1,32";
  else
    {
      operands[2] = GEN_INT (shift_amt);
      return "dsrl\t%0,%1,%2";
    }
}
}

static const char *output_121 PARAMS ((rtx *, rtx));

static const char *
output_121 (operands, insn)
     rtx *operands ATTRIBUTE_UNUSED;
     rtx insn ATTRIBUTE_UNUSED;
{

{
  int shift_amt = INTVAL (operands[2]) & 0x3f;

  if (shift_amt < 32)
    {
      operands[2] = GEN_INT (32 + shift_amt);
      if (TARGET_MIPS16)
	return "dsll\t%0,%1,%2\n\tdsra\t%0,32";
      return "dsll\t%0,%1,%2\n\tdsra\t%0,%0,32";
    }
  else
    return "move\t%0,%.";
}
}

static const char *output_125 PARAMS ((rtx *, rtx));

static const char *
output_125 (operands, insn)
     rtx *operands ATTRIBUTE_UNUSED;
     rtx insn ATTRIBUTE_UNUSED;
{
 return mips_move_1word (operands, insn, TRUE);
}

static const char *output_126 PARAMS ((rtx *, rtx));

static const char *
output_126 (operands, insn)
     rtx *operands ATTRIBUTE_UNUSED;
     rtx insn ATTRIBUTE_UNUSED;
{

{
  if (which_alternative == 0)
    return "andi\t%0,%1,0xffff";
  else
    return mips_move_1word (operands, insn, TRUE);
}
}

static const char *output_127 PARAMS ((rtx *, rtx));

static const char *
output_127 (operands, insn)
     rtx *operands ATTRIBUTE_UNUSED;
     rtx insn ATTRIBUTE_UNUSED;
{
 return mips_move_1word (operands, insn, TRUE);
}

static const char *output_128 PARAMS ((rtx *, rtx));

static const char *
output_128 (operands, insn)
     rtx *operands ATTRIBUTE_UNUSED;
     rtx insn ATTRIBUTE_UNUSED;
{

{
  if (which_alternative == 0)
    return "andi\t%0,%1,0xffff";
  else
    return mips_move_1word (operands, insn, TRUE);
}
}

static const char *output_129 PARAMS ((rtx *, rtx));

static const char *
output_129 (operands, insn)
     rtx *operands ATTRIBUTE_UNUSED;
     rtx insn ATTRIBUTE_UNUSED;
{
 return mips_move_1word (operands, insn, TRUE);
}

static const char *output_130 PARAMS ((rtx *, rtx));

static const char *
output_130 (operands, insn)
     rtx *operands ATTRIBUTE_UNUSED;
     rtx insn ATTRIBUTE_UNUSED;
{

{
  if (which_alternative == 0)
    return "andi\t%0,%1,0x00ff";
  else
    return mips_move_1word (operands, insn, TRUE);
}
}

static const char *output_131 PARAMS ((rtx *, rtx));

static const char *
output_131 (operands, insn)
     rtx *operands ATTRIBUTE_UNUSED;
     rtx insn ATTRIBUTE_UNUSED;
{
 return mips_move_1word (operands, insn, TRUE);
}

static const char *output_132 PARAMS ((rtx *, rtx));

static const char *
output_132 (operands, insn)
     rtx *operands ATTRIBUTE_UNUSED;
     rtx insn ATTRIBUTE_UNUSED;
{

{
  if (which_alternative == 0)
    return "andi\t%0,%1,0x00ff";
  else
    return mips_move_1word (operands, insn, TRUE);
}
}

static const char *output_133 PARAMS ((rtx *, rtx));

static const char *
output_133 (operands, insn)
     rtx *operands ATTRIBUTE_UNUSED;
     rtx insn ATTRIBUTE_UNUSED;
{
 return mips_move_1word (operands, insn, TRUE);
}

static const char *output_134 PARAMS ((rtx *, rtx));

static const char *
output_134 (operands, insn)
     rtx *operands ATTRIBUTE_UNUSED;
     rtx insn ATTRIBUTE_UNUSED;
{

{
  if (which_alternative == 0)
    return "andi\t%0,%1,0x00ff";
  else
    return mips_move_1word (operands, insn, TRUE);
}
}

static const char *output_135 PARAMS ((rtx *, rtx));

static const char *
output_135 (operands, insn)
     rtx *operands ATTRIBUTE_UNUSED;
     rtx insn ATTRIBUTE_UNUSED;
{

{
  return mips_move_1word (operands, insn, TRUE);
}
}

static const char *output_136 PARAMS ((rtx *, rtx));

static const char *
output_136 (operands, insn)
     rtx *operands ATTRIBUTE_UNUSED;
     rtx insn ATTRIBUTE_UNUSED;
{
 return mips_move_1word (operands, insn, TRUE);
}

static const char *output_137 PARAMS ((rtx *, rtx));

static const char *
output_137 (operands, insn)
     rtx *operands ATTRIBUTE_UNUSED;
     rtx insn ATTRIBUTE_UNUSED;
{
 return mips_move_1word (operands, insn, FALSE);
}

static const char *output_138 PARAMS ((rtx *, rtx));

static const char *
output_138 (operands, insn)
     rtx *operands ATTRIBUTE_UNUSED;
     rtx insn ATTRIBUTE_UNUSED;
{
 return mips_move_1word (operands, insn, FALSE);
}

static const char *output_139 PARAMS ((rtx *, rtx));

static const char *
output_139 (operands, insn)
     rtx *operands ATTRIBUTE_UNUSED;
     rtx insn ATTRIBUTE_UNUSED;
{
 return mips_move_1word (operands, insn, FALSE);
}

static const char *output_140 PARAMS ((rtx *, rtx));

static const char *
output_140 (operands, insn)
     rtx *operands ATTRIBUTE_UNUSED;
     rtx insn ATTRIBUTE_UNUSED;
{
 return mips_move_1word (operands, insn, FALSE);
}

static const char *output_141 PARAMS ((rtx *, rtx));

static const char *
output_141 (operands, insn)
     rtx *operands ATTRIBUTE_UNUSED;
     rtx insn ATTRIBUTE_UNUSED;
{
 return mips_move_1word (operands, insn, FALSE);
}

static const char *output_153 PARAMS ((rtx *, rtx));

static const char *
output_153 (operands, insn)
     rtx *operands ATTRIBUTE_UNUSED;
     rtx insn ATTRIBUTE_UNUSED;
{

{
  rtx offset = const0_rtx;
  rtx addr = XEXP (operands[1], 0);
  rtx mem_addr = eliminate_constant_term (addr, &offset);
  const char *ret;

  if (TARGET_STATS)
    mips_count_memory_refs (operands[1], 2);

  /* The stack/frame pointers are always aligned, so we can convert
     to the faster lw if we are referencing an aligned stack location.  */

  if ((INTVAL (offset) & 3) == 0
      && (mem_addr == stack_pointer_rtx || mem_addr == frame_pointer_rtx))
    ret = "lw\t%0,%1";
  else
    ret = "ulw\t%0,%1";

  return mips_fill_delay_slot (ret, DELAY_LOAD, operands, insn);
}
}

static const char *output_154 PARAMS ((rtx *, rtx));

static const char *
output_154 (operands, insn)
     rtx *operands ATTRIBUTE_UNUSED;
     rtx insn ATTRIBUTE_UNUSED;
{

{
  rtx offset = const0_rtx;
  rtx addr = XEXP (operands[0], 0);
  rtx mem_addr = eliminate_constant_term (addr, &offset);

  if (TARGET_STATS)
    mips_count_memory_refs (operands[0], 2);

  /* The stack/frame pointers are always aligned, so we can convert
     to the faster sw if we are referencing an aligned stack location.  */

  if ((INTVAL (offset) & 3) == 0
      && (mem_addr == stack_pointer_rtx || mem_addr == frame_pointer_rtx))
    return "sw\t%z1,%0";

  return "usw\t%z1,%0";
}
}

static const char *output_155 PARAMS ((rtx *, rtx));

static const char *
output_155 (operands, insn)
     rtx *operands ATTRIBUTE_UNUSED;
     rtx insn ATTRIBUTE_UNUSED;
{

{
  rtx offset = const0_rtx;
  rtx addr = XEXP (operands[1], 0);
  rtx mem_addr = eliminate_constant_term (addr, &offset);
  const char *ret;

  if (TARGET_STATS)
    mips_count_memory_refs (operands[1], 2);

  /* The stack/frame pointers are always aligned, so we can convert
     to the faster lw if we are referencing an aligned stack location.  */

  if ((INTVAL (offset) & 7) == 0
      && (mem_addr == stack_pointer_rtx || mem_addr == frame_pointer_rtx))
    ret = "ld\t%0,%1";
  else
    ret = "uld\t%0,%1";

  return mips_fill_delay_slot (ret, DELAY_LOAD, operands, insn);
}
}

static const char *output_156 PARAMS ((rtx *, rtx));

static const char *
output_156 (operands, insn)
     rtx *operands ATTRIBUTE_UNUSED;
     rtx insn ATTRIBUTE_UNUSED;
{

{
  rtx offset = const0_rtx;
  rtx addr = XEXP (operands[0], 0);
  rtx mem_addr = eliminate_constant_term (addr, &offset);

  if (TARGET_STATS)
    mips_count_memory_refs (operands[0], 2);

  /* The stack/frame pointers are always aligned, so we can convert
     to the faster sw if we are referencing an aligned stack location.  */

  if ((INTVAL (offset) & 7) == 0
      && (mem_addr == stack_pointer_rtx || mem_addr == frame_pointer_rtx))
    return "sd\t%z1,%0";

  return "usd\t%z1,%0";
}
}

static const char *output_159 PARAMS ((rtx *, rtx));

static const char *
output_159 (operands, insn)
     rtx *operands ATTRIBUTE_UNUSED;
     rtx insn ATTRIBUTE_UNUSED;
{

{
  operands[1] = gen_rtx (REG, DImode, 31);
  return mips_move_2words (operands, insn);
}
}

static const char *output_160 PARAMS ((rtx *, rtx));

static const char *
output_160 (operands, insn)
     rtx *operands ATTRIBUTE_UNUSED;
     rtx insn ATTRIBUTE_UNUSED;
{
 return mips_move_2words (operands, insn); 
}

static const char *output_161 PARAMS ((rtx *, rtx));

static const char *
output_161 (operands, insn)
     rtx *operands ATTRIBUTE_UNUSED;
     rtx insn ATTRIBUTE_UNUSED;
{
 return mips_move_2words (operands, insn);
}

static const char *output_162 PARAMS ((rtx *, rtx));

static const char *
output_162 (operands, insn)
     rtx *operands ATTRIBUTE_UNUSED;
     rtx insn ATTRIBUTE_UNUSED;
{
 return mips_move_2words (operands, insn); 
}

static const char *output_163 PARAMS ((rtx *, rtx));

static const char *
output_163 (operands, insn)
     rtx *operands ATTRIBUTE_UNUSED;
     rtx insn ATTRIBUTE_UNUSED;
{
 return mips_sign_extend (insn, operands[0], operands[1]);
}

static const char *output_164 PARAMS ((rtx *, rtx));

static const char *
output_164 (operands, insn)
     rtx *operands ATTRIBUTE_UNUSED;
     rtx insn ATTRIBUTE_UNUSED;
{
 return mips_move_2words (operands, insn);
}

static const char *output_166 PARAMS ((rtx *, rtx));

static const char *
output_166 (operands, insn)
     rtx *operands ATTRIBUTE_UNUSED;
     rtx insn ATTRIBUTE_UNUSED;
{
 return mips_move_1word (operands, insn, FALSE);
}

static const char *output_167 PARAMS ((rtx *, rtx));

static const char *
output_167 (operands, insn)
     rtx *operands ATTRIBUTE_UNUSED;
     rtx insn ATTRIBUTE_UNUSED;
{
 return mips_move_1word (operands, insn, FALSE);
}

static const char *output_169 PARAMS ((rtx *, rtx));

static const char *
output_169 (operands, insn)
     rtx *operands ATTRIBUTE_UNUSED;
     rtx insn ATTRIBUTE_UNUSED;
{
 return mips_move_1word (operands, insn, FALSE);
}

static const char *output_178 PARAMS ((rtx *, rtx));

static const char *
output_178 (operands, insn)
     rtx *operands ATTRIBUTE_UNUSED;
     rtx insn ATTRIBUTE_UNUSED;
{
 return mips_move_1word (operands, insn, TRUE);
}

static const char *output_179 PARAMS ((rtx *, rtx));

static const char *
output_179 (operands, insn)
     rtx *operands ATTRIBUTE_UNUSED;
     rtx insn ATTRIBUTE_UNUSED;
{
 return mips_move_1word (operands, insn, TRUE);
}

static const char *output_180 PARAMS ((rtx *, rtx));

static const char *
output_180 (operands, insn)
     rtx *operands ATTRIBUTE_UNUSED;
     rtx insn ATTRIBUTE_UNUSED;
{
 return mips_move_1word (operands, insn, TRUE);
}

static const char *output_181 PARAMS ((rtx *, rtx));

static const char *
output_181 (operands, insn)
     rtx *operands ATTRIBUTE_UNUSED;
     rtx insn ATTRIBUTE_UNUSED;
{
 return mips_move_1word (operands, insn, TRUE);
}

static const char *output_182 PARAMS ((rtx *, rtx));

static const char *
output_182 (operands, insn)
     rtx *operands ATTRIBUTE_UNUSED;
     rtx insn ATTRIBUTE_UNUSED;
{
 return mips_move_1word (operands, insn, FALSE);
}

static const char *output_183 PARAMS ((rtx *, rtx));

static const char *
output_183 (operands, insn)
     rtx *operands ATTRIBUTE_UNUSED;
     rtx insn ATTRIBUTE_UNUSED;
{
 return mips_move_1word (operands, insn, FALSE);
}

static const char *output_184 PARAMS ((rtx *, rtx));

static const char *
output_184 (operands, insn)
     rtx *operands ATTRIBUTE_UNUSED;
     rtx insn ATTRIBUTE_UNUSED;
{
 return mips_move_1word (operands, insn, FALSE);
}

static const char *output_185 PARAMS ((rtx *, rtx));

static const char *
output_185 (operands, insn)
     rtx *operands ATTRIBUTE_UNUSED;
     rtx insn ATTRIBUTE_UNUSED;
{
 return mips_move_2words (operands, insn); 
}

static const char *output_186 PARAMS ((rtx *, rtx));

static const char *
output_186 (operands, insn)
     rtx *operands ATTRIBUTE_UNUSED;
     rtx insn ATTRIBUTE_UNUSED;
{
 return mips_move_2words (operands, insn); 
}

static const char *output_187 PARAMS ((rtx *, rtx));

static const char *
output_187 (operands, insn)
     rtx *operands ATTRIBUTE_UNUSED;
     rtx insn ATTRIBUTE_UNUSED;
{
 return mips_move_2words (operands, insn); 
}

static const char *output_188 PARAMS ((rtx *, rtx));

static const char *
output_188 (operands, insn)
     rtx *operands ATTRIBUTE_UNUSED;
     rtx insn ATTRIBUTE_UNUSED;
{
 return mips_move_2words (operands, insn);
}

static const char *output_190 PARAMS ((rtx *, rtx));

static const char *
output_190 (operands, insn)
     rtx *operands ATTRIBUTE_UNUSED;
     rtx insn ATTRIBUTE_UNUSED;
{
 return output_block_move (insn, operands, 4, BLOCK_MOVE_NORMAL);
}

static const char *output_191 PARAMS ((rtx *, rtx));

static const char *
output_191 (operands, insn)
     rtx *operands ATTRIBUTE_UNUSED;
     rtx insn ATTRIBUTE_UNUSED;
{
 return output_block_move (insn, operands, 4, BLOCK_MOVE_NORMAL);
}

static const char *output_192 PARAMS ((rtx *, rtx));

static const char *
output_192 (operands, insn)
     rtx *operands ATTRIBUTE_UNUSED;
     rtx insn ATTRIBUTE_UNUSED;
{
 return output_block_move (insn, operands, 4, BLOCK_MOVE_NOT_LAST);
}

static const char *output_193 PARAMS ((rtx *, rtx));

static const char *
output_193 (operands, insn)
     rtx *operands ATTRIBUTE_UNUSED;
     rtx insn ATTRIBUTE_UNUSED;
{
 return output_block_move (insn, operands, 4, BLOCK_MOVE_NOT_LAST);
}

static const char *output_194 PARAMS ((rtx *, rtx));

static const char *
output_194 (operands, insn)
     rtx *operands ATTRIBUTE_UNUSED;
     rtx insn ATTRIBUTE_UNUSED;
{
 return output_block_move (insn, operands, 4, BLOCK_MOVE_LAST);
}

static const char *output_195 PARAMS ((rtx *, rtx));

static const char *
output_195 (operands, insn)
     rtx *operands ATTRIBUTE_UNUSED;
     rtx insn ATTRIBUTE_UNUSED;
{

{
  if (GET_CODE (operands[2]) == CONST_INT)
    operands[2] = GEN_INT (INTVAL (operands[2]) & 0x1f);

  return "sll\t%0,%1,%2";
}
}

static const char *output_196 PARAMS ((rtx *, rtx));

static const char *
output_196 (operands, insn)
     rtx *operands ATTRIBUTE_UNUSED;
     rtx insn ATTRIBUTE_UNUSED;
{

{
  if (which_alternative == 0)
    return "sll\t%0,%2";

  if (GET_CODE (operands[2]) == CONST_INT)
    operands[2] = GEN_INT (INTVAL (operands[2]) & 0x1f);

  return "sll\t%0,%1,%2";
}
}

static const char *output_197 PARAMS ((rtx *, rtx));

static const char *
output_197 (operands, insn)
     rtx *operands ATTRIBUTE_UNUSED;
     rtx insn ATTRIBUTE_UNUSED;
{

{
  operands[4] = const0_rtx;
  dslots_jump_total += 3;
  dslots_jump_filled += 2;

  return "sll\t%3,%2,26\n\
\tbgez\t%3,1f\n\
\tsll\t%M0,%L1,%2\n\
\t%(b\t3f\n\
\tmove\t%L0,%z4%)\n\
\n\
%~1:\n\
\t%(beq\t%3,%z4,2f\n\
\tsll\t%M0,%M1,%2%)\n\
\n\
\tsubu\t%3,%z4,%2\n\
\tsrl\t%3,%L1,%3\n\
\tor\t%M0,%M0,%3\n\
%~2:\n\
\tsll\t%L0,%L1,%2\n\
%~3:";
}
}

static const char *output_198 PARAMS ((rtx *, rtx));

static const char *
output_198 (operands, insn)
     rtx *operands ATTRIBUTE_UNUSED;
     rtx insn ATTRIBUTE_UNUSED;
{

{
  operands[2] = GEN_INT (INTVAL (operands[2]) & 0x1f);
  operands[4] = const0_rtx;
  return "sll\t%M0,%L1,%2\n\tmove\t%L0,%z4";
}
}

static const char *output_199 PARAMS ((rtx *, rtx));

static const char *
output_199 (operands, insn)
     rtx *operands ATTRIBUTE_UNUSED;
     rtx insn ATTRIBUTE_UNUSED;
{

{
  int amount = INTVAL (operands[2]);

  operands[2] = GEN_INT (amount & 31);
  operands[4] = const0_rtx;
  operands[5] = GEN_INT ((-amount) & 31);

  return "sll\t%M0,%M1,%2\n\tsrl\t%3,%L1,%5\n\tor\t%M0,%M0,%3\n\tsll\t%L0,%L1,%2";
}
}

static const char *output_200 PARAMS ((rtx *, rtx));

static const char *
output_200 (operands, insn)
     rtx *operands ATTRIBUTE_UNUSED;
     rtx insn ATTRIBUTE_UNUSED;
{

{
  if (GET_CODE (operands[2]) == CONST_INT)
    operands[2] = GEN_INT (INTVAL (operands[2]) & 0x3f);

  return "dsll\t%0,%1,%2";
}
}

static const char *output_201 PARAMS ((rtx *, rtx));

static const char *
output_201 (operands, insn)
     rtx *operands ATTRIBUTE_UNUSED;
     rtx insn ATTRIBUTE_UNUSED;
{

{
  if (which_alternative == 0)
    return "dsll\t%0,%2";

  if (GET_CODE (operands[2]) == CONST_INT)
    operands[2] = GEN_INT (INTVAL (operands[2]) & 0x3f);

  return "dsll\t%0,%1,%2";
}
}

static const char *output_202 PARAMS ((rtx *, rtx));

static const char *
output_202 (operands, insn)
     rtx *operands ATTRIBUTE_UNUSED;
     rtx insn ATTRIBUTE_UNUSED;
{

{
  if (GET_CODE (operands[2]) == CONST_INT)
    operands[2] = GEN_INT (INTVAL (operands[2]) & 0x1f);

  return "sra\t%0,%1,%2";
}
}

static const char *output_203 PARAMS ((rtx *, rtx));

static const char *
output_203 (operands, insn)
     rtx *operands ATTRIBUTE_UNUSED;
     rtx insn ATTRIBUTE_UNUSED;
{

{
  if (which_alternative == 0)
    return "sra\t%0,%2";

  if (GET_CODE (operands[2]) == CONST_INT)
    operands[2] = GEN_INT (INTVAL (operands[2]) & 0x1f);

  return "sra\t%0,%1,%2";
}
}

static const char *output_204 PARAMS ((rtx *, rtx));

static const char *
output_204 (operands, insn)
     rtx *operands ATTRIBUTE_UNUSED;
     rtx insn ATTRIBUTE_UNUSED;
{

{
  operands[4] = const0_rtx;
  dslots_jump_total += 3;
  dslots_jump_filled += 2;

  return "sll\t%3,%2,26\n\
\tbgez\t%3,1f\n\
\tsra\t%L0,%M1,%2\n\
\t%(b\t3f\n\
\tsra\t%M0,%M1,31%)\n\
\n\
%~1:\n\
\t%(beq\t%3,%z4,2f\n\
\tsrl\t%L0,%L1,%2%)\n\
\n\
\tsubu\t%3,%z4,%2\n\
\tsll\t%3,%M1,%3\n\
\tor\t%L0,%L0,%3\n\
%~2:\n\
\tsra\t%M0,%M1,%2\n\
%~3:";
}
}

static const char *output_205 PARAMS ((rtx *, rtx));

static const char *
output_205 (operands, insn)
     rtx *operands ATTRIBUTE_UNUSED;
     rtx insn ATTRIBUTE_UNUSED;
{

{
  operands[2] = GEN_INT (INTVAL (operands[2]) & 0x1f);
  return "sra\t%L0,%M1,%2\n\tsra\t%M0,%M1,31";
}
}

static const char *output_206 PARAMS ((rtx *, rtx));

static const char *
output_206 (operands, insn)
     rtx *operands ATTRIBUTE_UNUSED;
     rtx insn ATTRIBUTE_UNUSED;
{

{
  int amount = INTVAL (operands[2]);

  operands[2] = GEN_INT (amount & 31);
  operands[4] = GEN_INT ((-amount) & 31);

  return "srl\t%L0,%L1,%2\n\tsll\t%3,%M1,%4\n\tor\t%L0,%L0,%3\n\tsra\t%M0,%M1,%2";
}
}

static const char *output_207 PARAMS ((rtx *, rtx));

static const char *
output_207 (operands, insn)
     rtx *operands ATTRIBUTE_UNUSED;
     rtx insn ATTRIBUTE_UNUSED;
{

{
  if (GET_CODE (operands[2]) == CONST_INT)
    operands[2] = GEN_INT (INTVAL (operands[2]) & 0x3f);

  return "dsra\t%0,%1,%2";
}
}

static const char *output_208 PARAMS ((rtx *, rtx));

static const char *
output_208 (operands, insn)
     rtx *operands ATTRIBUTE_UNUSED;
     rtx insn ATTRIBUTE_UNUSED;
{

{
  if (GET_CODE (operands[2]) == CONST_INT)
    operands[2] = GEN_INT (INTVAL (operands[2]) & 0x3f);

  return "dsra\t%0,%2";
}
}

static const char *output_209 PARAMS ((rtx *, rtx));

static const char *
output_209 (operands, insn)
     rtx *operands ATTRIBUTE_UNUSED;
     rtx insn ATTRIBUTE_UNUSED;
{

{
  if (GET_CODE (operands[2]) == CONST_INT)
    operands[2] = GEN_INT (INTVAL (operands[2]) & 0x1f);

  return "srl\t%0,%1,%2";
}
}

static const char *output_210 PARAMS ((rtx *, rtx));

static const char *
output_210 (operands, insn)
     rtx *operands ATTRIBUTE_UNUSED;
     rtx insn ATTRIBUTE_UNUSED;
{

{
  if (which_alternative == 0)
    return "srl\t%0,%2";

  if (GET_CODE (operands[2]) == CONST_INT)
    operands[2] = GEN_INT (INTVAL (operands[2]) & 0x1f);

  return "srl\t%0,%1,%2";
}
}

static const char *output_212 PARAMS ((rtx *, rtx));

static const char *
output_212 (operands, insn)
     rtx *operands ATTRIBUTE_UNUSED;
     rtx insn ATTRIBUTE_UNUSED;
{

{
  operands[4] = const0_rtx;
  dslots_jump_total += 3;
  dslots_jump_filled += 2;

  return "sll\t%3,%2,26\n\
\tbgez\t%3,1f\n\
\tsrl\t%L0,%M1,%2\n\
\t%(b\t3f\n\
\tmove\t%M0,%z4%)\n\
\n\
%~1:\n\
\t%(beq\t%3,%z4,2f\n\
\tsrl\t%L0,%L1,%2%)\n\
\n\
\tsubu\t%3,%z4,%2\n\
\tsll\t%3,%M1,%3\n\
\tor\t%L0,%L0,%3\n\
%~2:\n\
\tsrl\t%M0,%M1,%2\n\
%~3:";
}
}

static const char *output_213 PARAMS ((rtx *, rtx));

static const char *
output_213 (operands, insn)
     rtx *operands ATTRIBUTE_UNUSED;
     rtx insn ATTRIBUTE_UNUSED;
{

{
  operands[2] = GEN_INT (INTVAL (operands[2]) & 0x1f);
  operands[4] = const0_rtx;
  return "srl\t%L0,%M1,%2\n\tmove\t%M0,%z4";
}
}

static const char *output_214 PARAMS ((rtx *, rtx));

static const char *
output_214 (operands, insn)
     rtx *operands ATTRIBUTE_UNUSED;
     rtx insn ATTRIBUTE_UNUSED;
{

{
  int amount = INTVAL (operands[2]);

  operands[2] = GEN_INT (amount & 31);
  operands[4] = GEN_INT ((-amount) & 31);

  return "srl\t%L0,%L1,%2\n\tsll\t%3,%M1,%4\n\tor\t%L0,%L0,%3\n\tsrl\t%M0,%M1,%2";
}
}

static const char *output_215 PARAMS ((rtx *, rtx));

static const char *
output_215 (operands, insn)
     rtx *operands ATTRIBUTE_UNUSED;
     rtx insn ATTRIBUTE_UNUSED;
{

{
  if (GET_CODE (operands[2]) == CONST_INT)
    operands[2] = GEN_INT (INTVAL (operands[2]) & 0x3f);

  return "dsrl\t%0,%1,%2";
}
}

static const char *output_216 PARAMS ((rtx *, rtx));

static const char *
output_216 (operands, insn)
     rtx *operands ATTRIBUTE_UNUSED;
     rtx insn ATTRIBUTE_UNUSED;
{

{
  if (GET_CODE (operands[2]) == CONST_INT)
    operands[2] = GEN_INT (INTVAL (operands[2]) & 0x3f);

  return "dsrl\t%0,%2";
}
}

static const char *output_217 PARAMS ((rtx *, rtx));

static const char *
output_217 (operands, insn)
     rtx *operands ATTRIBUTE_UNUSED;
     rtx insn ATTRIBUTE_UNUSED;
{

{
  if (TARGET_SR71K && GET_CODE (operands[2]) != CONST_INT)
    return "rorv\t%0,%1,%2";

  if ((GET_CODE (operands[2]) == CONST_INT)
      && (INTVAL (operands[2]) < 0 || INTVAL (operands[2]) >= 32))
    abort ();

  return "ror\t%0,%1,%2";
}
}

static const char *output_218 PARAMS ((rtx *, rtx));

static const char *
output_218 (operands, insn)
     rtx *operands ATTRIBUTE_UNUSED;
     rtx insn ATTRIBUTE_UNUSED;
{

{
   if (TARGET_SR71K)
    {
      if (GET_CODE (operands[2]) != CONST_INT)
        return "drorv\t%0,%1,%2";

      if (INTVAL (operands[2]) >= 32 && INTVAL (operands[2]) <= 63)
        return "dror32\t%0,%1,%2";
    }

  if ((GET_CODE (operands[2]) == CONST_INT)
      && (INTVAL (operands[2]) < 0 || INTVAL (operands[2]) >= 64))
    abort ();

  return "dror\t%0,%1,%2";
}
}

static const char *output_219 PARAMS ((rtx *, rtx));

static const char *
output_219 (operands, insn)
     rtx *operands ATTRIBUTE_UNUSED;
     rtx insn ATTRIBUTE_UNUSED;
{

{
  return mips_output_conditional_branch (insn,
					 operands,
					 /*two_operands_p=*/0,
					 /*float_p=*/1,
					 /*inverted_p=*/0,
					 get_attr_length (insn));
}
}

static const char *output_220 PARAMS ((rtx *, rtx));

static const char *
output_220 (operands, insn)
     rtx *operands ATTRIBUTE_UNUSED;
     rtx insn ATTRIBUTE_UNUSED;
{

{
  return mips_output_conditional_branch (insn,
					 operands,
					 /*two_operands_p=*/0,
					 /*float_p=*/1,
					 /*inverted_p=*/1,
					 get_attr_length (insn));
}
}

static const char *output_221 PARAMS ((rtx *, rtx));

static const char *
output_221 (operands, insn)
     rtx *operands ATTRIBUTE_UNUSED;
     rtx insn ATTRIBUTE_UNUSED;
{

{
  return mips_output_conditional_branch (insn,
					 operands,
					 /*two_operands_p=*/0,
					 /*float_p=*/0,
					 /*inverted_p=*/0,
					 get_attr_length (insn));
}
}

static const char *output_222 PARAMS ((rtx *, rtx));

static const char *
output_222 (operands, insn)
     rtx *operands ATTRIBUTE_UNUSED;
     rtx insn ATTRIBUTE_UNUSED;
{

{
  return mips_output_conditional_branch (insn,
					 operands,
					 /*two_operands_p=*/0,
					 /*float_p=*/0,
					 /*inverted_p=*/1,
					 get_attr_length (insn));
}
}

static const char *output_223 PARAMS ((rtx *, rtx));

static const char *
output_223 (operands, insn)
     rtx *operands ATTRIBUTE_UNUSED;
     rtx insn ATTRIBUTE_UNUSED;
{

{
  return mips_output_conditional_branch (insn,
					 operands,
					 /*two_operands_p=*/0,
					 /*float_p=*/0,
					 /*inverted_p=*/0,
					 get_attr_length (insn));
}
}

static const char *output_224 PARAMS ((rtx *, rtx));

static const char *
output_224 (operands, insn)
     rtx *operands ATTRIBUTE_UNUSED;
     rtx insn ATTRIBUTE_UNUSED;
{

{
  return mips_output_conditional_branch (insn,
					 operands,
					 /*two_operands_p=*/0,
					 /*float_p=*/0,
					 /*inverted_p=*/1,
					 get_attr_length (insn));
}
}

static const char *output_225 PARAMS ((rtx *, rtx));

static const char *
output_225 (operands, insn)
     rtx *operands ATTRIBUTE_UNUSED;
     rtx insn ATTRIBUTE_UNUSED;
{

{
  return mips_output_conditional_branch (insn,
					 operands,
					 /*two_operands_p=*/1,
					 /*float_p=*/0,
					 /*inverted_p=*/0,
					 get_attr_length (insn));
}
}

static const char *output_226 PARAMS ((rtx *, rtx));

static const char *
output_226 (operands, insn)
     rtx *operands ATTRIBUTE_UNUSED;
     rtx insn ATTRIBUTE_UNUSED;
{

{
  return mips_output_conditional_branch (insn,
					 operands,
					 /*two_operands_p=*/1,
					 /*float_p=*/0,
					 /*inverted_p=*/0,
					 get_attr_length (insn));
}
}

static const char *output_227 PARAMS ((rtx *, rtx));

static const char *
output_227 (operands, insn)
     rtx *operands ATTRIBUTE_UNUSED;
     rtx insn ATTRIBUTE_UNUSED;
{

{
  return mips_output_conditional_branch (insn,
					 operands,
					 /*two_operands_p=*/1,
					 /*float_p=*/0,
					 /*inverted_p=*/1,
					 get_attr_length (insn));
}
}

static const char *output_228 PARAMS ((rtx *, rtx));

static const char *
output_228 (operands, insn)
     rtx *operands ATTRIBUTE_UNUSED;
     rtx insn ATTRIBUTE_UNUSED;
{

{
  return mips_output_conditional_branch (insn,
					 operands,
					 /*two_operands_p=*/1,
					 /*float_p=*/0,
					 /*inverted_p=*/1,
					 get_attr_length (insn));
}
}

static const char *output_229 PARAMS ((rtx *, rtx));

static const char *
output_229 (operands, insn)
     rtx *operands ATTRIBUTE_UNUSED;
     rtx insn ATTRIBUTE_UNUSED;
{

{
  if (operands[2] != pc_rtx)
    {
      if (which_alternative == 0)
	return "%*b%C0z\t%1,%2";
      else
	return "%*bt%C0z\t%2";
    }
  else
    {
      if (which_alternative == 0)
	return "%*b%N0z\t%1,%3";
      else
	return "%*bt%N0z\t%3";
    }
}
}

static const char *output_230 PARAMS ((rtx *, rtx));

static const char *
output_230 (operands, insn)
     rtx *operands ATTRIBUTE_UNUSED;
     rtx insn ATTRIBUTE_UNUSED;
{

{
  if (operands[2] != pc_rtx)
    {
      if (which_alternative == 0)
	return "%*b%C0z\t%1,%2";
      else
	return "%*bt%C0z\t%2";
    }
  else
    {
      if (which_alternative == 0)
	return "%*b%N0z\t%1,%3";
      else
	return "%*bt%N0z\t%3";
    }
}
}

static const char *output_245 PARAMS ((rtx *, rtx));

static const char *
output_245 (operands, insn)
     rtx *operands ATTRIBUTE_UNUSED;
     rtx insn ATTRIBUTE_UNUSED;
{

{
  operands[2] = GEN_INT (INTVAL (operands[2])+1);
  return "slt\t%0,%1,%2";
}
}

static const char *output_246 PARAMS ((rtx *, rtx));

static const char *
output_246 (operands, insn)
     rtx *operands ATTRIBUTE_UNUSED;
     rtx insn ATTRIBUTE_UNUSED;
{

{
  operands[2] = GEN_INT (INTVAL (operands[2])+1);
  return "slt\t%1,%2";
}
}

static const char *output_247 PARAMS ((rtx *, rtx));

static const char *
output_247 (operands, insn)
     rtx *operands ATTRIBUTE_UNUSED;
     rtx insn ATTRIBUTE_UNUSED;
{

{
  operands[2] = GEN_INT (INTVAL (operands[2])+1);
  return "slt\t%0,%1,%2";
}
}

static const char *output_248 PARAMS ((rtx *, rtx));

static const char *
output_248 (operands, insn)
     rtx *operands ATTRIBUTE_UNUSED;
     rtx insn ATTRIBUTE_UNUSED;
{

{
  operands[2] = GEN_INT (INTVAL (operands[2])+1);
  return "slt\t%1,%2";
}
}

static const char *output_257 PARAMS ((rtx *, rtx));

static const char *
output_257 (operands, insn)
     rtx *operands ATTRIBUTE_UNUSED;
     rtx insn ATTRIBUTE_UNUSED;
{

{
  operands[2] = GEN_INT (INTVAL (operands[2]) + 1);
  return "sltu\t%0,%1,%2";
}
}

static const char *output_258 PARAMS ((rtx *, rtx));

static const char *
output_258 (operands, insn)
     rtx *operands ATTRIBUTE_UNUSED;
     rtx insn ATTRIBUTE_UNUSED;
{

{
  operands[2] = GEN_INT (INTVAL (operands[2])+1);
  return "sltu\t%1,%2";
}
}

static const char *output_259 PARAMS ((rtx *, rtx));

static const char *
output_259 (operands, insn)
     rtx *operands ATTRIBUTE_UNUSED;
     rtx insn ATTRIBUTE_UNUSED;
{

{
  operands[2] = GEN_INT (INTVAL (operands[2]) + 1);
  return "sltu\t%0,%1,%2";
}
}

static const char *output_260 PARAMS ((rtx *, rtx));

static const char *
output_260 (operands, insn)
     rtx *operands ATTRIBUTE_UNUSED;
     rtx insn ATTRIBUTE_UNUSED;
{

{
  operands[2] = GEN_INT (INTVAL (operands[2])+1);
  return "sltu\t%1,%2";
}
}

static const char *output_261 PARAMS ((rtx *, rtx));

static const char *
output_261 (operands, insn)
     rtx *operands ATTRIBUTE_UNUSED;
     rtx insn ATTRIBUTE_UNUSED;
{

{
 return mips_fill_delay_slot ("c.un.d\t%Z0%1,%2", DELAY_FCMP, operands, insn);
}
}

static const char *output_262 PARAMS ((rtx *, rtx));

static const char *
output_262 (operands, insn)
     rtx *operands ATTRIBUTE_UNUSED;
     rtx insn ATTRIBUTE_UNUSED;
{

{
 return mips_fill_delay_slot ("c.ult.d\t%Z0%1,%2", DELAY_FCMP, operands, insn);
}
}

static const char *output_263 PARAMS ((rtx *, rtx));

static const char *
output_263 (operands, insn)
     rtx *operands ATTRIBUTE_UNUSED;
     rtx insn ATTRIBUTE_UNUSED;
{

{
 return mips_fill_delay_slot ("c.ueq.d\t%Z0%1,%2", DELAY_FCMP, operands, insn);
}
}

static const char *output_264 PARAMS ((rtx *, rtx));

static const char *
output_264 (operands, insn)
     rtx *operands ATTRIBUTE_UNUSED;
     rtx insn ATTRIBUTE_UNUSED;
{

{
 return mips_fill_delay_slot ("c.ule.d\t%Z0%1,%2", DELAY_FCMP, operands, insn);
}
}

static const char *output_265 PARAMS ((rtx *, rtx));

static const char *
output_265 (operands, insn)
     rtx *operands ATTRIBUTE_UNUSED;
     rtx insn ATTRIBUTE_UNUSED;
{

{
  return mips_fill_delay_slot ("c.eq.d\t%Z0%1,%2", DELAY_FCMP, operands, insn);
}
}

static const char *output_266 PARAMS ((rtx *, rtx));

static const char *
output_266 (operands, insn)
     rtx *operands ATTRIBUTE_UNUSED;
     rtx insn ATTRIBUTE_UNUSED;
{

{
  return mips_fill_delay_slot ("c.lt.d\t%Z0%1,%2", DELAY_FCMP, operands, insn);
}
}

static const char *output_267 PARAMS ((rtx *, rtx));

static const char *
output_267 (operands, insn)
     rtx *operands ATTRIBUTE_UNUSED;
     rtx insn ATTRIBUTE_UNUSED;
{

{
  return mips_fill_delay_slot ("c.le.d\t%Z0%1,%2", DELAY_FCMP, operands, insn);
}
}

static const char *output_268 PARAMS ((rtx *, rtx));

static const char *
output_268 (operands, insn)
     rtx *operands ATTRIBUTE_UNUSED;
     rtx insn ATTRIBUTE_UNUSED;
{

{
  return mips_fill_delay_slot ("c.lt.d\t%Z0%2,%1", DELAY_FCMP, operands, insn);
}
}

static const char *output_269 PARAMS ((rtx *, rtx));

static const char *
output_269 (operands, insn)
     rtx *operands ATTRIBUTE_UNUSED;
     rtx insn ATTRIBUTE_UNUSED;
{

{
  return mips_fill_delay_slot ("c.le.d\t%Z0%2,%1", DELAY_FCMP, operands, insn);
}
}

static const char *output_270 PARAMS ((rtx *, rtx));

static const char *
output_270 (operands, insn)
     rtx *operands ATTRIBUTE_UNUSED;
     rtx insn ATTRIBUTE_UNUSED;
{

{
 return mips_fill_delay_slot ("c.un.s\t%Z0%1,%2", DELAY_FCMP, operands, insn);
}
}

static const char *output_271 PARAMS ((rtx *, rtx));

static const char *
output_271 (operands, insn)
     rtx *operands ATTRIBUTE_UNUSED;
     rtx insn ATTRIBUTE_UNUSED;
{

{
 return mips_fill_delay_slot ("c.ult.s\t%Z0%1,%2", DELAY_FCMP, operands, insn);
}
}

static const char *output_272 PARAMS ((rtx *, rtx));

static const char *
output_272 (operands, insn)
     rtx *operands ATTRIBUTE_UNUSED;
     rtx insn ATTRIBUTE_UNUSED;
{

{
 return mips_fill_delay_slot ("c.ueq.s\t%Z0%1,%2", DELAY_FCMP, operands, insn);
}
}

static const char *output_273 PARAMS ((rtx *, rtx));

static const char *
output_273 (operands, insn)
     rtx *operands ATTRIBUTE_UNUSED;
     rtx insn ATTRIBUTE_UNUSED;
{

{
 return mips_fill_delay_slot ("c.ule.s\t%Z0%1,%2", DELAY_FCMP, operands, insn);
}
}

static const char *output_274 PARAMS ((rtx *, rtx));

static const char *
output_274 (operands, insn)
     rtx *operands ATTRIBUTE_UNUSED;
     rtx insn ATTRIBUTE_UNUSED;
{

{
  return mips_fill_delay_slot ("c.eq.s\t%Z0%1,%2", DELAY_FCMP, operands, insn);
}
}

static const char *output_275 PARAMS ((rtx *, rtx));

static const char *
output_275 (operands, insn)
     rtx *operands ATTRIBUTE_UNUSED;
     rtx insn ATTRIBUTE_UNUSED;
{

{
  return mips_fill_delay_slot ("c.lt.s\t%Z0%1,%2", DELAY_FCMP, operands, insn);
}
}

static const char *output_276 PARAMS ((rtx *, rtx));

static const char *
output_276 (operands, insn)
     rtx *operands ATTRIBUTE_UNUSED;
     rtx insn ATTRIBUTE_UNUSED;
{

{
  return mips_fill_delay_slot ("c.le.s\t%Z0%1,%2", DELAY_FCMP, operands, insn);
}
}

static const char *output_277 PARAMS ((rtx *, rtx));

static const char *
output_277 (operands, insn)
     rtx *operands ATTRIBUTE_UNUSED;
     rtx insn ATTRIBUTE_UNUSED;
{

{
  return mips_fill_delay_slot ("c.lt.s\t%Z0%2,%1", DELAY_FCMP, operands, insn);
}
}

static const char *output_278 PARAMS ((rtx *, rtx));

static const char *
output_278 (operands, insn)
     rtx *operands ATTRIBUTE_UNUSED;
     rtx insn ATTRIBUTE_UNUSED;
{

{
  return mips_fill_delay_slot ("c.le.s\t%Z0%2,%1", DELAY_FCMP, operands, insn);
}
}

static const char *output_279 PARAMS ((rtx *, rtx));

static const char *
output_279 (operands, insn)
     rtx *operands ATTRIBUTE_UNUSED;
     rtx insn ATTRIBUTE_UNUSED;
{

{
  if (flag_pic && ! TARGET_EMBEDDED_PIC)
    {
      if (get_attr_length (insn) <= 8)
	return "%*b\t%l0";
      else if (Pmode == DImode)
	return "%[dla\t%@,%l0\n\t%*jr\t%@%]";
      else
	return "%[la\t%@,%l0\n\t%*jr\t%@%]";
    }
  else
    return "%*j\t%l0";
}
}

static const char *output_285 PARAMS ((rtx *, rtx));

static const char *
output_285 (operands, insn)
     rtx *operands ATTRIBUTE_UNUSED;
     rtx insn ATTRIBUTE_UNUSED;
{

{
  /* .cpadd expands to add REG,REG,$gp when pic, and nothing when not pic.  */
  if (mips_abi == ABI_32 || mips_abi == ABI_O64)
    output_asm_insn (".cpadd\t%0", operands);
  return "%*j\t%0";
}
}

static const char *output_291 PARAMS ((rtx *, rtx));

static const char *
output_291 (operands, insn)
     rtx *operands ATTRIBUTE_UNUSED;
     rtx insn ATTRIBUTE_UNUSED;
{

{
  return "%*j\t%0";
}
}

static const char *output_295 PARAMS ((rtx *, rtx));

static const char *
output_295 (operands, insn)
     rtx *operands ATTRIBUTE_UNUSED;
     rtx insn ATTRIBUTE_UNUSED;
{
 return mips_restore_gp (operands, insn);
}

static const char *output_297 PARAMS ((rtx *, rtx));

static const char *
output_297 (operands, insn)
     rtx *operands ATTRIBUTE_UNUSED;
     rtx insn ATTRIBUTE_UNUSED;
{

{
  register rtx target = operands[0];

  if (GET_CODE (target) == CONST_INT)
    return "%[li\t%@,%0\n\t%*jal\t%2,%@%]";
  else if (CONSTANT_ADDRESS_P (target))
    return "%*jal\t%0";
  else
    return "%*jal\t%2,%0";
}
}

static const char *output_298 PARAMS ((rtx *, rtx));

static const char *
output_298 (operands, insn)
     rtx *operands ATTRIBUTE_UNUSED;
     rtx insn ATTRIBUTE_UNUSED;
{

{
  register rtx target = operands[0];

  if (GET_CODE (target) == CONST_INT)
    return "li\t%^,%0\n\tjal\t%2,%^";
  else if (CONSTANT_ADDRESS_P (target))
    {
      if (GET_MODE (target) == SImode)
	return "la\t%^,%0\n\tjal\t%2,%^";
      else
	return "dla\t%^,%0\n\tjal\t%2,%^";
    }
  else if (REGNO (target) != PIC_FUNCTION_ADDR_REGNUM)
    return "move\t%^,%0\n\tjal\t%2,%^";
  else
    return "jal\t%2,%0";
}
}

static const char *output_302 PARAMS ((rtx *, rtx));

static const char *
output_302 (operands, insn)
     rtx *operands ATTRIBUTE_UNUSED;
     rtx insn ATTRIBUTE_UNUSED;
{

{
  if (REGNO (operands[0]) != PIC_FUNCTION_ADDR_REGNUM)
    return "move\t%^,%0\n\tjal\t%2,%^";
  else
    return "jal\t%2,%0";
}
}

static const char *output_303 PARAMS ((rtx *, rtx));

static const char *
output_303 (operands, insn)
     rtx *operands ATTRIBUTE_UNUSED;
     rtx insn ATTRIBUTE_UNUSED;
{

{
  if (REGNO (operands[0]) != PIC_FUNCTION_ADDR_REGNUM)
    return "move\t%^,%0\n\tjal\t%2,%^";
  else
    return "jal\t%2,%0";
}
}

static const char *output_305 PARAMS ((rtx *, rtx));

static const char *
output_305 (operands, insn)
     rtx *operands ATTRIBUTE_UNUSED;
     rtx insn ATTRIBUTE_UNUSED;
{

{
  register rtx target = operands[1];

  if (GET_CODE (target) == CONST_INT)
    return "%[li\t%@,%1\n\t%*jal\t%3,%@%]";
  else if (CONSTANT_ADDRESS_P (target))
    return "%*jal\t%1";
  else
    return "%*jal\t%3,%1";
}
}

static const char *output_306 PARAMS ((rtx *, rtx));

static const char *
output_306 (operands, insn)
     rtx *operands ATTRIBUTE_UNUSED;
     rtx insn ATTRIBUTE_UNUSED;
{

{
  register rtx target = operands[1];

  if (GET_CODE (target) == CONST_INT)
    return "li\t%^,%1\n\tjal\t%3,%^";
  else if (CONSTANT_ADDRESS_P (target))
    {
      if (GET_MODE (target) == SImode)
	return "la\t%^,%1\n\tjal\t%3,%^";
      else
	return "dla\t%^,%1\n\tjal\t%3,%^";
    }
  else if (REGNO (target) != PIC_FUNCTION_ADDR_REGNUM)
    return "move\t%^,%1\n\tjal\t%3,%^";
  else
    return "jal\t%3,%1";
}
}

static const char *output_310 PARAMS ((rtx *, rtx));

static const char *
output_310 (operands, insn)
     rtx *operands ATTRIBUTE_UNUSED;
     rtx insn ATTRIBUTE_UNUSED;
{

{
  if (REGNO (operands[1]) != PIC_FUNCTION_ADDR_REGNUM)
    return "move\t%^,%1\n\tjal\t%3,%^";
  else
    return "jal\t%3,%1";
}
}

static const char *output_311 PARAMS ((rtx *, rtx));

static const char *
output_311 (operands, insn)
     rtx *operands ATTRIBUTE_UNUSED;
     rtx insn ATTRIBUTE_UNUSED;
{

{
  if (REGNO (operands[1]) != PIC_FUNCTION_ADDR_REGNUM)
    return "move\t%^,%1\n\tjal\t%3,%^";
  else
    return "jal\t%3,%1";
}
}

static const char *output_312 PARAMS ((rtx *, rtx));

static const char *
output_312 (operands, insn)
     rtx *operands ATTRIBUTE_UNUSED;
     rtx insn ATTRIBUTE_UNUSED;
{

{
  register rtx target = operands[1];

  if (GET_CODE (target) == CONST_INT)
    return "%[li\t%@,%1\n\t%*jal\t%4,%@%]";
  else if (CONSTANT_ADDRESS_P (target))
    return "%*jal\t%1";
  else
    return "%*jal\t%4,%1";
}
}

static const char *output_313 PARAMS ((rtx *, rtx));

static const char *
output_313 (operands, insn)
     rtx *operands ATTRIBUTE_UNUSED;
     rtx insn ATTRIBUTE_UNUSED;
{

{
  register rtx target = operands[1];

  if (GET_CODE (target) == CONST_INT)
    return "li\t%^,%1\n\tjal\t%4,%^";
  else if (CONSTANT_ADDRESS_P (target))
    {
      if (GET_MODE (target) == SImode)
	return "la\t%^,%1\n\tjal\t%4,%^";
      else
	return "dla\t%^,%1\n\tjal\t%4,%^";
    }
  else if (REGNO (target) != PIC_FUNCTION_ADDR_REGNUM)
    return "move\t%^,%1\n\tjal\t%4,%^";
  else
    return "jal\t%4,%1";
}
}

static const char *output_314 PARAMS ((rtx *, rtx));

static const char *
output_314 (operands, insn)
     rtx *operands ATTRIBUTE_UNUSED;
     rtx insn ATTRIBUTE_UNUSED;
{
 return mips_emit_prefetch (operands);
}

static const char *output_315 PARAMS ((rtx *, rtx));

static const char *
output_315 (operands, insn)
     rtx *operands ATTRIBUTE_UNUSED;
     rtx insn ATTRIBUTE_UNUSED;
{
 return mips_emit_prefetch (operands);
}

static const char *output_316 PARAMS ((rtx *, rtx));

static const char *
output_316 (operands, insn)
     rtx *operands ATTRIBUTE_UNUSED;
     rtx insn ATTRIBUTE_UNUSED;
{
 return mips_emit_prefetch (operands);
}

static const char *output_317 PARAMS ((rtx *, rtx));

static const char *
output_317 (operands, insn)
     rtx *operands ATTRIBUTE_UNUSED;
     rtx insn ATTRIBUTE_UNUSED;
{
 return mips_emit_prefetch (operands);
}

static const char * const output_319[] = {
  "mov%B4\t%0,%z2,%1",
  "mov%b4\t%0,%z3,%1",
};

static const char * const output_320[] = {
  "mov%B4\t%0,%z2,%1",
  "mov%b4\t%0,%z3,%1",
};

static const char * const output_321[] = {
  "mov%T3\t%0,%z1,%4",
  "mov%t3\t%0,%z2,%4",
};

static const char * const output_322[] = {
  "mov%B4\t%0,%z2,%1",
  "mov%b4\t%0,%z3,%1",
};

static const char * const output_323[] = {
  "mov%B4\t%0,%z2,%1",
  "mov%b4\t%0,%z3,%1",
};

static const char * const output_324[] = {
  "mov%T3\t%0,%z1,%4",
  "mov%t3\t%0,%z2,%4",
};

static const char * const output_325[] = {
  "mov%B4.s\t%0,%2,%1",
  "mov%b4.s\t%0,%3,%1",
};

static const char * const output_326[] = {
  "mov%B4.s\t%0,%2,%1",
  "mov%b4.s\t%0,%3,%1",
};

static const char * const output_327[] = {
  "mov%T3.s\t%0,%1,%4",
  "mov%t3.s\t%0,%2,%4",
};

static const char * const output_328[] = {
  "mov%B4.d\t%0,%2,%1",
  "mov%b4.d\t%0,%3,%1",
};

static const char * const output_329[] = {
  "mov%B4.d\t%0,%2,%1",
  "mov%b4.d\t%0,%3,%1",
};

static const char * const output_330[] = {
  "mov%T3.d\t%0,%1,%4",
  "mov%t3.d\t%0,%2,%4",
};

static const char *output_331 PARAMS ((rtx *, rtx));

static const char *
output_331 (operands, insn)
     rtx *operands ATTRIBUTE_UNUSED;
     rtx insn ATTRIBUTE_UNUSED;
{

{
  assemble_integer (operands[0], 1, BITS_PER_UNIT, 1);
  return "";
}
}

static const char *output_332 PARAMS ((rtx *, rtx));

static const char *
output_332 (operands, insn)
     rtx *operands ATTRIBUTE_UNUSED;
     rtx insn ATTRIBUTE_UNUSED;
{

{
  assemble_integer (operands[0], 2, BITS_PER_UNIT * 2, 1);
  return "";
}
}

static const char *output_333 PARAMS ((rtx *, rtx));

static const char *
output_333 (operands, insn)
     rtx *operands ATTRIBUTE_UNUSED;
     rtx insn ATTRIBUTE_UNUSED;
{

{
  assemble_integer (operands[0], 4, BITS_PER_UNIT * 4, 1);
  return "";
}
}

static const char *output_334 PARAMS ((rtx *, rtx));

static const char *
output_334 (operands, insn)
     rtx *operands ATTRIBUTE_UNUSED;
     rtx insn ATTRIBUTE_UNUSED;
{

{
  assemble_integer (operands[0], 8, BITS_PER_UNIT * 8, 1);
  return "";
}
}

static const char *output_335 PARAMS ((rtx *, rtx));

static const char *
output_335 (operands, insn)
     rtx *operands ATTRIBUTE_UNUSED;
     rtx insn ATTRIBUTE_UNUSED;
{

{
  REAL_VALUE_TYPE d;

  if (GET_CODE (operands[0]) != CONST_DOUBLE)
    abort ();
  REAL_VALUE_FROM_CONST_DOUBLE (d, operands[0]);
  assemble_real (d, SFmode, GET_MODE_ALIGNMENT (SFmode));
  return "";
}
}

static const char *output_336 PARAMS ((rtx *, rtx));

static const char *
output_336 (operands, insn)
     rtx *operands ATTRIBUTE_UNUSED;
     rtx insn ATTRIBUTE_UNUSED;
{

{
  REAL_VALUE_TYPE d;

  if (GET_CODE (operands[0]) != CONST_DOUBLE)
    abort ();
  REAL_VALUE_FROM_CONST_DOUBLE (d, operands[0]);
  assemble_real (d, DFmode, GET_MODE_ALIGNMENT (DFmode));
  return "";
}
}

static const char *output_529 PARAMS ((rtx *, rtx));

static const char *
output_529 (operands, insn)
     rtx *operands ATTRIBUTE_UNUSED;
     rtx insn ATTRIBUTE_UNUSED;
{

{
  if (operands[3] != pc_rtx)
    return "%*b%C2z\t%1,%3";
  else
    return "%*b%N2z\t%1,%4";
}
}

static const char *output_530 PARAMS ((rtx *, rtx));

static const char *
output_530 (operands, insn)
     rtx *operands ATTRIBUTE_UNUSED;
     rtx insn ATTRIBUTE_UNUSED;
{

{
  if (operands[3] != pc_rtx)
    return "%*b%C2z\t%1,%3";
  else
    return "%*b%N2z\t%1,%4";
}
}

static const char *output_531 PARAMS ((rtx *, rtx));

static const char *
output_531 (operands, insn)
     rtx *operands ATTRIBUTE_UNUSED;
     rtx insn ATTRIBUTE_UNUSED;
{

{
  if (operands[3] != pc_rtx)
    return "%*bt%C2z\t%3";
  else
    return "%*bt%N2z\t%4";
}
}

static const char *output_532 PARAMS ((rtx *, rtx));

static const char *
output_532 (operands, insn)
     rtx *operands ATTRIBUTE_UNUSED;
     rtx insn ATTRIBUTE_UNUSED;
{

{
  if (operands[3] != pc_rtx)
    return "%*bt%C2z\t%3";
  else
    return "%*bt%N2z\t%4";
}
}


extern int trap_cmp_op PARAMS ((rtx, enum machine_mode));
extern int reg_or_0_operand PARAMS ((rtx, enum machine_mode));
extern int nonmemory_operand PARAMS ((rtx, enum machine_mode));
extern int register_operand PARAMS ((rtx, enum machine_mode));
extern int arith_operand PARAMS ((rtx, enum machine_mode));
extern int small_int PARAMS ((rtx, enum machine_mode));
extern int se_reg_or_0_operand PARAMS ((rtx, enum machine_mode));
extern int se_arith_operand PARAMS ((rtx, enum machine_mode));
extern int scratch_operand PARAMS ((rtx, enum machine_mode));
extern int se_register_operand PARAMS ((rtx, enum machine_mode));
extern int extend_operator PARAMS ((rtx, enum machine_mode));
extern int highpart_shift_operator PARAMS ((rtx, enum machine_mode));
extern int const_float_1_operand PARAMS ((rtx, enum machine_mode));
extern int true_reg_or_0_operand PARAMS ((rtx, enum machine_mode));
extern int immediate_operand PARAMS ((rtx, enum machine_mode));
extern int se_nonmemory_operand PARAMS ((rtx, enum machine_mode));
extern int uns_arith_operand PARAMS ((rtx, enum machine_mode));
extern int se_uns_arith_operand PARAMS ((rtx, enum machine_mode));
extern int memory_operand PARAMS ((rtx, enum machine_mode));
extern int nonimmediate_operand PARAMS ((rtx, enum machine_mode));
extern int general_operand PARAMS ((rtx, enum machine_mode));
extern int move_operand PARAMS ((rtx, enum machine_mode));
extern int movdi_operand PARAMS ((rtx, enum machine_mode));
extern int address_operand PARAMS ((rtx, enum machine_mode));
extern int cmp_op PARAMS ((rtx, enum machine_mode));
extern int equality_op PARAMS ((rtx, enum machine_mode));
extern int pc_or_label_operand PARAMS ((rtx, enum machine_mode));
extern int pmode_register_operand PARAMS ((rtx, enum machine_mode));
extern int call_insn_operand PARAMS ((rtx, enum machine_mode));
extern int const_int_operand PARAMS ((rtx, enum machine_mode));
extern int consttable_operand PARAMS ((rtx, enum machine_mode));
extern int large_int PARAMS ((rtx, enum machine_mode));
extern int fcc_register_operand PARAMS ((rtx, enum machine_mode));
extern int arith32_operand PARAMS ((rtx, enum machine_mode));
extern int comparison_operator PARAMS ((rtx, enum machine_mode));



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
    trap_cmp_op,
    "",
    VOIDmode,
    0,
    0
  },
  {
    reg_or_0_operand,
    "d",
    SImode,
    0,
    1
  },
  {
    nonmemory_operand,
    "dI",
    SImode,
    0,
    1
  },
  {
    register_operand,
    "=f",
    DFmode,
    0,
    1
  },
  {
    register_operand,
    "f",
    DFmode,
    0,
    1
  },
  {
    register_operand,
    "f",
    DFmode,
    0,
    1
  },
  {
    register_operand,
    "=f",
    SFmode,
    0,
    1
  },
  {
    register_operand,
    "f",
    SFmode,
    0,
    1
  },
  {
    register_operand,
    "f",
    SFmode,
    0,
    1
  },
  {
    register_operand,
    "=d",
    SImode,
    0,
    1
  },
  {
    reg_or_0_operand,
    "dJ",
    SImode,
    0,
    1
  },
  {
    arith_operand,
    "dI",
    SImode,
    0,
    1
  },
  {
    small_int,
    "I",
    SImode,
    0,
    1
  },
  {
    register_operand,
    "=d",
    SImode,
    0,
    1
  },
  {
    small_int,
    "I",
    SImode,
    0,
    1
  },
  {
    register_operand,
    "=d,d,d",
    SImode,
    0,
    1
  },
  {
    register_operand,
    "0,d,d",
    SImode,
    0,
    1
  },
  {
    arith_operand,
    "IQ,O,d",
    SImode,
    0,
    1
  },
  {
    register_operand,
    "=d,&d",
    DImode,
    0,
    1
  },
  {
    register_operand,
    "0,d",
    DImode,
    0,
    1
  },
  {
    register_operand,
    "d,d",
    DImode,
    0,
    1
  },
  {
    register_operand,
    "=d,d",
    SImode,
    0,
    1
  },
  {
    register_operand,
    "=d,d,d",
    DImode,
    0,
    1
  },
  {
    register_operand,
    "%d,%d,%d",
    DImode,
    0,
    1
  },
  {
    small_int,
    "P,J,N",
    DImode,
    0,
    1
  },
  {
    register_operand,
    "=d,d,d",
    SImode,
    0,
    1
  },
  {
    register_operand,
    "=d",
    DImode,
    0,
    1
  },
  {
    se_reg_or_0_operand,
    "dJ",
    DImode,
    0,
    1
  },
  {
    se_arith_operand,
    "dI",
    DImode,
    0,
    1
  },
  {
    small_int,
    "I",
    DImode,
    0,
    1
  },
  {
    register_operand,
    "=d",
    DImode,
    0,
    1
  },
  {
    small_int,
    "I",
    DImode,
    0,
    1
  },
  {
    register_operand,
    "=d,d,d",
    DImode,
    0,
    1
  },
  {
    register_operand,
    "0,d,d",
    DImode,
    0,
    1
  },
  {
    arith_operand,
    "IQ,O,d",
    DImode,
    0,
    1
  },
  {
    register_operand,
    "=d",
    DImode,
    0,
    1
  },
  {
    reg_or_0_operand,
    "dJ",
    SImode,
    0,
    1
  },
  {
    arith_operand,
    "dI",
    SImode,
    0,
    1
  },
  {
    register_operand,
    "=d,d,d",
    DImode,
    0,
    1
  },
  {
    register_operand,
    "0,d,d",
    SImode,
    0,
    1
  },
  {
    arith_operand,
    "I,O,d",
    SImode,
    0,
    1
  },
  {
    register_operand,
    "=d,d,d",
    SImode,
    0,
    1
  },
  {
    register_operand,
    "0,d,d",
    SImode,
    0,
    1
  },
  {
    arith_operand,
    "I,O,d",
    SImode,
    0,
    1
  },
  {
    register_operand,
    "=d",
    DImode,
    0,
    1
  },
  {
    register_operand,
    "d",
    DImode,
    0,
    1
  },
  {
    register_operand,
    "d",
    DImode,
    0,
    1
  },
  {
    register_operand,
    "=d",
    SImode,
    0,
    1
  },
  {
    register_operand,
    "=d,d,d",
    DImode,
    0,
    1
  },
  {
    register_operand,
    "d,d,d",
    DImode,
    0,
    1
  },
  {
    small_int,
    "P,J,N",
    DImode,
    0,
    1
  },
  {
    register_operand,
    "=d,d,d",
    SImode,
    0,
    1
  },
  {
    register_operand,
    "=d,d,d",
    DImode,
    0,
    1
  },
  {
    register_operand,
    "0,d,d",
    DImode,
    0,
    1
  },
  {
    arith_operand,
    "I,O,d",
    DImode,
    0,
    1
  },
  {
    register_operand,
    "=d,l",
    SImode,
    0,
    1
  },
  {
    register_operand,
    "d,d",
    SImode,
    0,
    1
  },
  {
    register_operand,
    "d,d",
    SImode,
    0,
    1
  },
  {
    scratch_operand,
    "=h,h",
    SImode,
    0,
    0
  },
  {
    scratch_operand,
    "=l,X",
    SImode,
    0,
    0
  },
  {
    scratch_operand,
    "=a,a",
    SImode,
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
    register_operand,
    "d",
    SImode,
    0,
    1
  },
  {
    register_operand,
    "d",
    SImode,
    0,
    1
  },
  {
    scratch_operand,
    "=h",
    SImode,
    0,
    0
  },
  {
    scratch_operand,
    "=a",
    SImode,
    0,
    0
  },
  {
    register_operand,
    "=d",
    SImode,
    0,
    1
  },
  {
    register_operand,
    "d",
    SImode,
    0,
    1
  },
  {
    register_operand,
    "d",
    SImode,
    0,
    1
  },
  {
    scratch_operand,
    "=h",
    SImode,
    0,
    0
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
    "=a",
    SImode,
    0,
    0
  },
  {
    register_operand,
    "=l,*d,*d",
    SImode,
    0,
    1
  },
  {
    register_operand,
    "d,d,d",
    SImode,
    0,
    1
  },
  {
    register_operand,
    "d,d,d",
    SImode,
    0,
    1
  },
  {
    register_operand,
    "0,l,*d",
    SImode,
    0,
    1
  },
  {
    scratch_operand,
    "=h,h,h",
    SImode,
    0,
    0
  },
  {
    scratch_operand,
    "=X,3,l",
    SImode,
    0,
    0
  },
  {
    scratch_operand,
    "=a,a,a",
    SImode,
    0,
    0
  },
  {
    scratch_operand,
    "=X,X,d",
    SImode,
    0,
    0
  },
  {
    register_operand,
    "=l,*d,*d",
    SImode,
    0,
    1
  },
  {
    register_operand,
    "0,l,*d",
    SImode,
    0,
    1
  },
  {
    register_operand,
    "d,d,d",
    SImode,
    0,
    1
  },
  {
    register_operand,
    "d,d,d",
    SImode,
    0,
    1
  },
  {
    scratch_operand,
    "=h,h,h",
    SImode,
    0,
    0
  },
  {
    scratch_operand,
    "=X,3,l",
    SImode,
    0,
    0
  },
  {
    scratch_operand,
    "=a,a,a",
    SImode,
    0,
    0
  },
  {
    scratch_operand,
    "=X,X,d",
    SImode,
    0,
    0
  },
  {
    register_operand,
    "=l,d",
    SImode,
    0,
    1
  },
  {
    register_operand,
    "d,d",
    SImode,
    0,
    1
  },
  {
    register_operand,
    "d,d",
    SImode,
    0,
    1
  },
  {
    scratch_operand,
    "=h,h",
    SImode,
    0,
    0
  },
  {
    scratch_operand,
    "=a,a",
    SImode,
    0,
    0
  },
  {
    scratch_operand,
    "=X,l",
    SImode,
    0,
    0
  },
  {
    register_operand,
    "=l,*d,*d",
    SImode,
    0,
    1
  },
  {
    register_operand,
    "0,l,*d",
    SImode,
    0,
    1
  },
  {
    register_operand,
    "d,d,d",
    SImode,
    0,
    1
  },
  {
    register_operand,
    "d,d,d",
    SImode,
    0,
    1
  },
  {
    scratch_operand,
    "=h,h,h",
    SImode,
    0,
    0
  },
  {
    scratch_operand,
    "=X,1,l",
    SImode,
    0,
    0
  },
  {
    scratch_operand,
    "=a,a,a",
    SImode,
    0,
    0
  },
  {
    scratch_operand,
    "=X,X,d",
    SImode,
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
    se_register_operand,
    "d",
    DImode,
    0,
    1
  },
  {
    register_operand,
    "d",
    DImode,
    0,
    1
  },
  {
    scratch_operand,
    "=h",
    DImode,
    0,
    0
  },
  {
    scratch_operand,
    "=a",
    DImode,
    0,
    0
  },
  {
    register_operand,
    "=d",
    DImode,
    0,
    1
  },
  {
    se_register_operand,
    "d",
    DImode,
    0,
    1
  },
  {
    register_operand,
    "d",
    DImode,
    0,
    1
  },
  {
    scratch_operand,
    "=h",
    DImode,
    0,
    0
  },
  {
    scratch_operand,
    "=l",
    DImode,
    0,
    0
  },
  {
    scratch_operand,
    "=a",
    DImode,
    0,
    0
  },
  {
    register_operand,
    "=x",
    DImode,
    0,
    1
  },
  {
    register_operand,
    "d",
    SImode,
    0,
    1
  },
  {
    register_operand,
    "d",
    SImode,
    0,
    1
  },
  {
    extend_operator,
    "",
    DImode,
    0,
    0
  },
  {
    extend_operator,
    "",
    DImode,
    0,
    0
  },
  {
    scratch_operand,
    "=a",
    SImode,
    0,
    0
  },
  {
    register_operand,
    "=a",
    DImode,
    0,
    1
  },
  {
    register_operand,
    "d",
    SImode,
    0,
    1
  },
  {
    register_operand,
    "d",
    SImode,
    0,
    1
  },
  {
    extend_operator,
    "",
    DImode,
    0,
    0
  },
  {
    extend_operator,
    "",
    DImode,
    0,
    0
  },
  {
    scratch_operand,
    "=l",
    DImode,
    0,
    0
  },
  {
    scratch_operand,
    "=h",
    DImode,
    0,
    0
  },
  {
    register_operand,
    "=a",
    DImode,
    0,
    1
  },
  {
    register_operand,
    "d",
    SImode,
    0,
    1
  },
  {
    register_operand,
    "d",
    SImode,
    0,
    1
  },
  {
    extend_operator,
    "",
    DImode,
    0,
    0
  },
  {
    extend_operator,
    "",
    DImode,
    0,
    0
  },
  {
    scratch_operand,
    "=h",
    SImode,
    0,
    0
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
    "=a",
    DImode,
    0,
    1
  },
  {
    register_operand,
    "d",
    SImode,
    0,
    1
  },
  {
    register_operand,
    "d",
    SImode,
    0,
    1
  },
  {
    register_operand,
    "0",
    DImode,
    0,
    1
  },
  {
    extend_operator,
    "",
    DImode,
    0,
    0
  },
  {
    extend_operator,
    "",
    DImode,
    0,
    0
  },
  {
    scratch_operand,
    "=h",
    SImode,
    0,
    0
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
    "=h",
    SImode,
    0,
    1
  },
  {
    register_operand,
    "d",
    SImode,
    0,
    1
  },
  {
    register_operand,
    "d",
    SImode,
    0,
    1
  },
  {
    extend_operator,
    "",
    DImode,
    0,
    0
  },
  {
    extend_operator,
    "",
    DImode,
    0,
    0
  },
  {
    highpart_shift_operator,
    "",
    DImode,
    0,
    0
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
    "=a",
    SImode,
    0,
    0
  },
  {
    register_operand,
    "=h,d",
    SImode,
    0,
    1
  },
  {
    register_operand,
    "d,d",
    SImode,
    0,
    1
  },
  {
    register_operand,
    "d,d",
    SImode,
    0,
    1
  },
  {
    extend_operator,
    "",
    DImode,
    0,
    0
  },
  {
    extend_operator,
    "",
    DImode,
    0,
    0
  },
  {
    highpart_shift_operator,
    "",
    DImode,
    0,
    0
  },
  {
    scratch_operand,
    "=l,l",
    SImode,
    0,
    0
  },
  {
    scratch_operand,
    "=a,a",
    SImode,
    0,
    0
  },
  {
    scratch_operand,
    "=X,h",
    SImode,
    0,
    0
  },
  {
    register_operand,
    "=h",
    DImode,
    0,
    1
  },
  {
    se_register_operand,
    "d",
    DImode,
    0,
    1
  },
  {
    se_register_operand,
    "d",
    DImode,
    0,
    1
  },
  {
    scratch_operand,
    "=l",
    DImode,
    0,
    0
  },
  {
    scratch_operand,
    "=a",
    DImode,
    0,
    0
  },
  {
    register_operand,
    "+l",
    SImode,
    0,
    1
  },
  {
    register_operand,
    "d",
    SImode,
    0,
    1
  },
  {
    register_operand,
    "d",
    SImode,
    0,
    1
  },
  {
    scratch_operand,
    "=h",
    SImode,
    0,
    0
  },
  {
    scratch_operand,
    "=a",
    SImode,
    0,
    0
  },
  {
    register_operand,
    "+x",
    DImode,
    0,
    1
  },
  {
    register_operand,
    "d",
    SImode,
    0,
    1
  },
  {
    register_operand,
    "d",
    SImode,
    0,
    1
  },
  {
    extend_operator,
    "",
    DImode,
    0,
    0
  },
  {
    extend_operator,
    "",
    DImode,
    0,
    0
  },
  {
    scratch_operand,
    "=a",
    SImode,
    0,
    0
  },
  {
    register_operand,
    "+a",
    DImode,
    0,
    1
  },
  {
    register_operand,
    "d",
    SImode,
    0,
    1
  },
  {
    register_operand,
    "d",
    SImode,
    0,
    1
  },
  {
    extend_operator,
    "",
    DImode,
    0,
    0
  },
  {
    extend_operator,
    "",
    DImode,
    0,
    0
  },
  {
    scratch_operand,
    "=h",
    SImode,
    0,
    0
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
    "=f",
    DFmode,
    0,
    1
  },
  {
    register_operand,
    "f",
    DFmode,
    0,
    1
  },
  {
    register_operand,
    "f",
    DFmode,
    0,
    1
  },
  {
    register_operand,
    "f",
    DFmode,
    0,
    1
  },
  {
    register_operand,
    "=f",
    SFmode,
    0,
    1
  },
  {
    register_operand,
    "f",
    SFmode,
    0,
    1
  },
  {
    register_operand,
    "f",
    SFmode,
    0,
    1
  },
  {
    register_operand,
    "f",
    SFmode,
    0,
    1
  },
  {
    register_operand,
    "=f",
    DFmode,
    0,
    1
  },
  {
    const_float_1_operand,
    "",
    DFmode,
    0,
    1
  },
  {
    register_operand,
    "f",
    DFmode,
    0,
    1
  },
  {
    register_operand,
    "=f",
    SFmode,
    0,
    1
  },
  {
    const_float_1_operand,
    "",
    SFmode,
    0,
    1
  },
  {
    register_operand,
    "f",
    SFmode,
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
    register_operand,
    "d",
    SImode,
    0,
    1
  },
  {
    register_operand,
    "d",
    SImode,
    0,
    1
  },
  {
    register_operand,
    "=h",
    SImode,
    0,
    1
  },
  {
    scratch_operand,
    "=a",
    SImode,
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
    se_register_operand,
    "d",
    DImode,
    0,
    1
  },
  {
    se_register_operand,
    "d",
    DImode,
    0,
    1
  },
  {
    register_operand,
    "=h",
    DImode,
    0,
    1
  },
  {
    scratch_operand,
    "=a",
    DImode,
    0,
    0
  },
  {
    register_operand,
    "d,d",
    VOIDmode,
    0,
    1
  },
  {
    true_reg_or_0_operand,
    "d,J",
    VOIDmode,
    0,
    1
  },
  {
    immediate_operand,
    "",
    VOIDmode,
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
    register_operand,
    "d",
    SImode,
    0,
    1
  },
  {
    nonmemory_operand,
    "di",
    SImode,
    0,
    1
  },
  {
    scratch_operand,
    "=h",
    SImode,
    0,
    0
  },
  {
    scratch_operand,
    "=a",
    SImode,
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
    se_register_operand,
    "d",
    DImode,
    0,
    1
  },
  {
    se_nonmemory_operand,
    "di",
    DImode,
    0,
    1
  },
  {
    scratch_operand,
    "=h",
    SImode,
    0,
    0
  },
  {
    scratch_operand,
    "=a",
    SImode,
    0,
    0
  },
  {
    register_operand,
    "=h",
    SImode,
    0,
    1
  },
  {
    register_operand,
    "d",
    SImode,
    0,
    1
  },
  {
    nonmemory_operand,
    "di",
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
    "=a",
    SImode,
    0,
    0
  },
  {
    register_operand,
    "=h",
    DImode,
    0,
    1
  },
  {
    se_register_operand,
    "d",
    DImode,
    0,
    1
  },
  {
    se_nonmemory_operand,
    "di",
    DImode,
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
    "=a",
    SImode,
    0,
    0
  },
  {
    register_operand,
    "=&d",
    SImode,
    0,
    1
  },
  {
    register_operand,
    "d",
    SImode,
    0,
    1
  },
  {
    scratch_operand,
    "=&d",
    SImode,
    0,
    0
  },
  {
    scratch_operand,
    "=&d",
    SImode,
    0,
    0
  },
  {
    register_operand,
    "=&d",
    DImode,
    0,
    1
  },
  {
    se_register_operand,
    "d",
    DImode,
    0,
    1
  },
  {
    scratch_operand,
    "=&d",
    DImode,
    0,
    0
  },
  {
    scratch_operand,
    "=&d",
    DImode,
    0,
    0
  },
  {
    register_operand,
    "=d",
    DImode,
    0,
    1
  },
  {
    register_operand,
    "d",
    DImode,
    0,
    1
  },
  {
    register_operand,
    "=d",
    SImode,
    0,
    1
  },
  {
    register_operand,
    "=d,d",
    SImode,
    0,
    1
  },
  {
    uns_arith_operand,
    "%d,d",
    SImode,
    0,
    1
  },
  {
    uns_arith_operand,
    "d,K",
    SImode,
    0,
    1
  },
  {
    register_operand,
    "=d",
    SImode,
    0,
    1
  },
  {
    register_operand,
    "%0",
    SImode,
    0,
    1
  },
  {
    register_operand,
    "d",
    SImode,
    0,
    1
  },
  {
    register_operand,
    "=d",
    DImode,
    0,
    1
  },
  {
    se_register_operand,
    "d",
    DImode,
    0,
    1
  },
  {
    se_register_operand,
    "d",
    DImode,
    0,
    1
  },
  {
    register_operand,
    "=d",
    DImode,
    0,
    1
  },
  {
    se_register_operand,
    "0",
    DImode,
    0,
    1
  },
  {
    se_register_operand,
    "d",
    DImode,
    0,
    1
  },
  {
    register_operand,
    "=d,d",
    DImode,
    0,
    1
  },
  {
    se_register_operand,
    "%d,d",
    DImode,
    0,
    1
  },
  {
    se_uns_arith_operand,
    "d,K",
    DImode,
    0,
    1
  },
  {
    register_operand,
    "=d,t,t",
    SImode,
    0,
    1
  },
  {
    uns_arith_operand,
    "%0,d,d",
    SImode,
    0,
    1
  },
  {
    uns_arith_operand,
    "d,K,d",
    SImode,
    0,
    1
  },
  {
    register_operand,
    "=d,t,t",
    DImode,
    0,
    1
  },
  {
    se_register_operand,
    "%0,d,d",
    DImode,
    0,
    1
  },
  {
    se_uns_arith_operand,
    "d,K,d",
    DImode,
    0,
    1
  },
  {
    register_operand,
    "=d",
    DImode,
    0,
    1
  },
  {
    se_register_operand,
    "d",
    DImode,
    0,
    1
  },
  {
    se_uns_arith_operand,
    "K",
    DImode,
    0,
    1
  },
  {
    register_operand,
    "=f",
    SFmode,
    0,
    1
  },
  {
    register_operand,
    "f",
    DFmode,
    0,
    1
  },
  {
    register_operand,
    "=d",
    SImode,
    0,
    1
  },
  {
    se_register_operand,
    "d",
    DImode,
    0,
    1
  },
  {
    register_operand,
    "=d",
    HImode,
    0,
    1
  },
  {
    se_register_operand,
    "d",
    DImode,
    0,
    1
  },
  {
    register_operand,
    "=d",
    QImode,
    0,
    1
  },
  {
    se_register_operand,
    "d",
    DImode,
    0,
    1
  },
  {
    register_operand,
    "=d",
    SImode,
    0,
    1
  },
  {
    se_register_operand,
    "d",
    DImode,
    0,
    1
  },
  {
    small_int,
    "I",
    DImode,
    0,
    1
  },
  {
    register_operand,
    "=d,d",
    DImode,
    0,
    1
  },
  {
    memory_operand,
    "R,m",
    SImode,
    0,
    1
  },
  {
    register_operand,
    "=d,d,d",
    SImode,
    0,
    1
  },
  {
    nonimmediate_operand,
    "d,R,m",
    HImode,
    0,
    1
  },
  {
    register_operand,
    "=d,d",
    SImode,
    0,
    1
  },
  {
    memory_operand,
    "R,m",
    HImode,
    0,
    1
  },
  {
    register_operand,
    "=d,d,d",
    DImode,
    0,
    1
  },
  {
    nonimmediate_operand,
    "d,R,m",
    HImode,
    0,
    1
  },
  {
    register_operand,
    "=d,d",
    DImode,
    0,
    1
  },
  {
    memory_operand,
    "R,m",
    HImode,
    0,
    1
  },
  {
    register_operand,
    "=d,d,d",
    HImode,
    0,
    1
  },
  {
    nonimmediate_operand,
    "d,R,m",
    QImode,
    0,
    1
  },
  {
    register_operand,
    "=d,d",
    HImode,
    0,
    1
  },
  {
    memory_operand,
    "R,m",
    QImode,
    0,
    1
  },
  {
    register_operand,
    "=d,d,d",
    SImode,
    0,
    1
  },
  {
    nonimmediate_operand,
    "d,R,m",
    QImode,
    0,
    1
  },
  {
    register_operand,
    "=d,d",
    SImode,
    0,
    1
  },
  {
    memory_operand,
    "R,m",
    QImode,
    0,
    1
  },
  {
    register_operand,
    "=d,d,d",
    DImode,
    0,
    1
  },
  {
    nonimmediate_operand,
    "d,R,m",
    QImode,
    0,
    1
  },
  {
    register_operand,
    "=d,d",
    DImode,
    0,
    1
  },
  {
    memory_operand,
    "R,m",
    QImode,
    0,
    1
  },
  {
    register_operand,
    "=f",
    DFmode,
    0,
    1
  },
  {
    register_operand,
    "f",
    SFmode,
    0,
    1
  },
  {
    register_operand,
    "=f",
    SImode,
    0,
    1
  },
  {
    register_operand,
    "f",
    DFmode,
    0,
    1
  },
  {
    scratch_operand,
    "=d",
    DFmode,
    0,
    0
  },
  {
    register_operand,
    "=f",
    SImode,
    0,
    1
  },
  {
    register_operand,
    "f",
    SFmode,
    0,
    1
  },
  {
    scratch_operand,
    "=d",
    SFmode,
    0,
    0
  },
  {
    register_operand,
    "=f",
    DImode,
    0,
    1
  },
  {
    register_operand,
    "f",
    DFmode,
    0,
    1
  },
  {
    register_operand,
    "=f",
    DImode,
    0,
    1
  },
  {
    register_operand,
    "f",
    SFmode,
    0,
    1
  },
  {
    register_operand,
    "=f",
    DFmode,
    0,
    1
  },
  {
    register_operand,
    "f",
    SImode,
    0,
    1
  },
  {
    register_operand,
    "=f",
    DFmode,
    0,
    1
  },
  {
    register_operand,
    "f",
    DImode,
    0,
    1
  },
  {
    register_operand,
    "=f",
    SFmode,
    0,
    1
  },
  {
    register_operand,
    "f",
    SImode,
    0,
    1
  },
  {
    register_operand,
    "=f",
    SFmode,
    0,
    1
  },
  {
    register_operand,
    "f",
    DImode,
    0,
    1
  },
  {
    register_operand,
    "=&d,&d",
    SImode,
    0,
    1
  },
  {
    general_operand,
    "R,o",
    BLKmode,
    0,
    1
  },
  {
    memory_operand,
    "=R,o",
    BLKmode,
    0,
    1
  },
  {
    reg_or_0_operand,
    "dJ,dJ",
    SImode,
    0,
    1
  },
  {
    register_operand,
    "=&d,&d",
    DImode,
    0,
    1
  },
  {
    general_operand,
    "R,o",
    BLKmode,
    0,
    1
  },
  {
    memory_operand,
    "=R,o",
    BLKmode,
    0,
    1
  },
  {
    reg_or_0_operand,
    "dJ,dJ",
    DImode,
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
    immediate_operand,
    "",
    SImode,
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
    register_operand,
    "r",
    SImode,
    0,
    1
  },
  {
    immediate_operand,
    "",
    SImode,
    0,
    1
  },
  {
    memory_operand,
    "=R,m",
    DImode,
    0,
    1
  },
  {
    nonimmediate_operand,
    "=d,d,d,d,R,o,*x,*d,*x,*B*C*D,*B*C*D,*B*C*D,*d,*m,*R",
    DImode,
    0,
    1
  },
  {
    general_operand,
    "d,iF,R,o,d,d,J,*x,*d,*d,*m,*R,*B*C*D,*B*C*D,*B*C*D",
    DImode,
    0,
    1
  },
  {
    nonimmediate_operand,
    "=d,y,d,d,d,d,d,R,To,*d",
    DImode,
    0,
    1
  },
  {
    general_operand,
    "d,d,y,K,N,R,To,d,d,*x",
    DImode,
    0,
    1
  },
  {
    nonimmediate_operand,
    "=d,d,d,d,d,R,m,*f,*f,*f,*f,*d,*R,*m,*x,*d,*x,*a,*B*C*D,*B*C*D,*B*C*D,*d,*m,*R",
    DImode,
    0,
    1
  },
  {
    move_operand,
    "d,IKL,Mnis,R,m,dJ,dJ,*f,*d*J,*R,*m,*f,*f,*f,*J,*x,*d,*J,*d,*m,*R,*B*C*D,*B*C*D,*B*C*D",
    DImode,
    0,
    1
  },
  {
    nonimmediate_operand,
    "=d,d,d,d,d,R,m,*d,*f,*x,*d,*x,*a,*B*C*D,*B*C*D,*B*C*D,*d,*m,*R",
    DImode,
    0,
    1
  },
  {
    move_operand,
    "d,IKL,Mnis,R,m,dJ,dJ,*f,*d*J,*J,*x,*d,*J,*d,*m,*R,*B*C*D,*B*C*D,*B*C*D",
    SImode,
    0,
    1
  },
  {
    nonimmediate_operand,
    "=d,y,d,d,d,d,d,d,R,m,*d",
    DImode,
    0,
    1
  },
  {
    movdi_operand,
    "d,d,y,K,N,s,R,m,d,d,*x",
    DImode,
    0,
    1
  },
  {
    small_int,
    "n",
    SImode,
    0,
    1
  },
  {
    nonimmediate_operand,
    "=d,d,d,d,d,R,m,*f,*f,*f,?*f,*d,*R,*m,*d,*z,*x,*d,*x,*d,*B*C*D,*B*C*D,*B*C*D,*d,*m,*R",
    SImode,
    0,
    1
  },
  {
    move_operand,
    "d,IKL,Mnis,R,m,dJ,dJ,*f,*d*J,*R,*m,*f,*f,*f,*z,*d,J,*x,*d,*a,*d,*m,*R,*B*C*D,*B*C*D,*B*C*D",
    SImode,
    0,
    1
  },
  {
    nonimmediate_operand,
    "=d,y,d,d,d,d,d,d,R,m,*d,*d",
    SImode,
    0,
    1
  },
  {
    move_operand,
    "d,d,y,K,N,s,R,m,d,d,*x,*a",
    SImode,
    0,
    1
  },
  {
    register_operand,
    "=b",
    VOIDmode,
    0,
    1
  },
  {
    nonimmediate_operand,
    "=d,*d,*d,*d,*R,*m,*d,*f,*f,*f,*f,*R,*m",
    CCmode,
    0,
    1
  },
  {
    general_operand,
    "z,*d,*R,*m,*d,*d,*f,*d,*f,*R,*m,*f,*f",
    CCmode,
    0,
    1
  },
  {
    register_operand,
    "=f",
    SFmode,
    0,
    1
  },
  {
    register_operand,
    "d",
    SImode,
    0,
    1
  },
  {
    register_operand,
    "d",
    SImode,
    0,
    1
  },
  {
    register_operand,
    "=f",
    SFmode,
    0,
    1
  },
  {
    se_register_operand,
    "d",
    DImode,
    0,
    1
  },
  {
    se_register_operand,
    "d",
    DImode,
    0,
    1
  },
  {
    register_operand,
    "=f",
    DFmode,
    0,
    1
  },
  {
    register_operand,
    "d",
    SImode,
    0,
    1
  },
  {
    register_operand,
    "d",
    SImode,
    0,
    1
  },
  {
    register_operand,
    "=f",
    DFmode,
    0,
    1
  },
  {
    se_register_operand,
    "d",
    DImode,
    0,
    1
  },
  {
    se_register_operand,
    "d",
    DImode,
    0,
    1
  },
  {
    register_operand,
    "f",
    SFmode,
    0,
    1
  },
  {
    register_operand,
    "d",
    SImode,
    0,
    1
  },
  {
    register_operand,
    "d",
    SImode,
    0,
    1
  },
  {
    register_operand,
    "f",
    SFmode,
    0,
    1
  },
  {
    se_register_operand,
    "d",
    DImode,
    0,
    1
  },
  {
    se_register_operand,
    "d",
    DImode,
    0,
    1
  },
  {
    register_operand,
    "f",
    DFmode,
    0,
    1
  },
  {
    register_operand,
    "d",
    SImode,
    0,
    1
  },
  {
    register_operand,
    "d",
    SImode,
    0,
    1
  },
  {
    register_operand,
    "f",
    DFmode,
    0,
    1
  },
  {
    se_register_operand,
    "d",
    DImode,
    0,
    1
  },
  {
    se_register_operand,
    "d",
    DImode,
    0,
    1
  },
  {
    nonimmediate_operand,
    "=d,d,d,d,R,m,*d,*f*z,*f,*x,*d",
    HImode,
    0,
    1
  },
  {
    general_operand,
    "d,IK,R,m,dJ,dJ,*f*z,*d,*f,*d,*x",
    HImode,
    0,
    1
  },
  {
    nonimmediate_operand,
    "=d,y,d,d,d,d,d,R,m,*d",
    HImode,
    0,
    1
  },
  {
    general_operand,
    "d,d,y,K,N,R,m,d,d,*x",
    HImode,
    0,
    1
  },
  {
    nonimmediate_operand,
    "=d,d,d,d,R,m,*d,*f*z,*f,*x,*d",
    QImode,
    0,
    1
  },
  {
    general_operand,
    "d,IK,R,m,dJ,dJ,*f*z,*d,*f,*d,*x",
    QImode,
    0,
    1
  },
  {
    nonimmediate_operand,
    "=d,y,d,d,d,d,d,R,m,*d",
    QImode,
    0,
    1
  },
  {
    general_operand,
    "d,d,y,K,N,R,m,d,d,*x",
    QImode,
    0,
    1
  },
  {
    nonimmediate_operand,
    "=f,f,f,f,R,m,*f,*d,*d,*d,*d,*R,*m",
    SFmode,
    0,
    1
  },
  {
    general_operand,
    "f,G,R,m,fG,fG,*d,*f,*G*d,*R,*m,*d,*d",
    SFmode,
    0,
    1
  },
  {
    nonimmediate_operand,
    "=d,d,d,R,m",
    SFmode,
    0,
    1
  },
  {
    general_operand,
    "Gd,R,m,d,d",
    SFmode,
    0,
    1
  },
  {
    nonimmediate_operand,
    "=d,y,d,d,d,R,m",
    SFmode,
    0,
    1
  },
  {
    nonimmediate_operand,
    "d,d,y,R,m,d,d",
    SFmode,
    0,
    1
  },
  {
    nonimmediate_operand,
    "=f,f,f,f,R,To,*f,*d,*d,*d,*d,*R,*T",
    DFmode,
    0,
    1
  },
  {
    general_operand,
    "f,G,R,To,fG,fG,*d,*f,*d*G,*R,*T,*d,*d",
    DFmode,
    0,
    1
  },
  {
    nonimmediate_operand,
    "=f,f,R,R,To,To,*d,*d,*To,*R,*d",
    DFmode,
    0,
    1
  },
  {
    general_operand,
    "f,To,f,G,f,G,*To,*R,*d,*d,*d",
    DFmode,
    0,
    1
  },
  {
    nonimmediate_operand,
    "=d,d,d,R,To,d,f,f",
    DFmode,
    0,
    1
  },
  {
    general_operand,
    "dG,R,To,d,d,f,d,f",
    DFmode,
    0,
    1
  },
  {
    nonimmediate_operand,
    "=d,y,d,d,d,R,To",
    DFmode,
    0,
    1
  },
  {
    nonimmediate_operand,
    "d,d,y,R,To,d,d",
    DFmode,
    0,
    1
  },
  {
    address_operand,
    "",
    DImode,
    0,
    1
  },
  {
    register_operand,
    "",
    DImode,
    0,
    1
  },
  {
    memory_operand,
    "=o",
    BLKmode,
    0,
    1
  },
  {
    memory_operand,
    "o",
    BLKmode,
    0,
    1
  },
  {
    small_int,
    "I",
    SImode,
    0,
    1
  },
  {
    small_int,
    "I",
    SImode,
    0,
    1
  },
  {
    scratch_operand,
    "=&d",
    SImode,
    0,
    0
  },
  {
    scratch_operand,
    "=&d",
    SImode,
    0,
    0
  },
  {
    scratch_operand,
    "=&d",
    SImode,
    0,
    0
  },
  {
    scratch_operand,
    "=&d",
    SImode,
    0,
    0
  },
  {
    memory_operand,
    "=Ro",
    BLKmode,
    0,
    1
  },
  {
    memory_operand,
    "Ro",
    BLKmode,
    0,
    1
  },
  {
    small_int,
    "I",
    SImode,
    0,
    1
  },
  {
    small_int,
    "I",
    SImode,
    0,
    1
  },
  {
    scratch_operand,
    "=&d",
    SImode,
    0,
    0
  },
  {
    scratch_operand,
    "=&d",
    SImode,
    0,
    0
  },
  {
    scratch_operand,
    "=&d",
    SImode,
    0,
    0
  },
  {
    scratch_operand,
    "=&d",
    SImode,
    0,
    0
  },
  {
    register_operand,
    "=d",
    SImode,
    0,
    1
  },
  {
    register_operand,
    "d",
    SImode,
    0,
    1
  },
  {
    arith_operand,
    "dI",
    SImode,
    0,
    1
  },
  {
    register_operand,
    "=d,d",
    SImode,
    0,
    1
  },
  {
    register_operand,
    "0,d",
    SImode,
    0,
    1
  },
  {
    arith_operand,
    "d,I",
    SImode,
    0,
    1
  },
  {
    register_operand,
    "=&d",
    DImode,
    0,
    1
  },
  {
    register_operand,
    "d",
    DImode,
    0,
    1
  },
  {
    register_operand,
    "d",
    SImode,
    0,
    1
  },
  {
    register_operand,
    "=d",
    SImode,
    0,
    1
  },
  {
    register_operand,
    "=d",
    DImode,
    0,
    1
  },
  {
    register_operand,
    "d",
    DImode,
    0,
    1
  },
  {
    small_int,
    "IJK",
    SImode,
    0,
    1
  },
  {
    register_operand,
    "=d",
    SImode,
    0,
    1
  },
  {
    register_operand,
    "=d",
    DImode,
    0,
    1
  },
  {
    se_register_operand,
    "d",
    DImode,
    0,
    1
  },
  {
    arith_operand,
    "dI",
    SImode,
    0,
    1
  },
  {
    register_operand,
    "=d,d",
    DImode,
    0,
    1
  },
  {
    se_register_operand,
    "0,d",
    DImode,
    0,
    1
  },
  {
    arith_operand,
    "d,I",
    SImode,
    0,
    1
  },
  {
    register_operand,
    "=d,d",
    DImode,
    0,
    1
  },
  {
    se_register_operand,
    "0,0",
    DImode,
    0,
    1
  },
  {
    arith_operand,
    "d,I",
    SImode,
    0,
    1
  },
  {
    register_operand,
    "=d,d",
    SImode,
    0,
    1
  },
  {
    memory_operand,
    "R,m",
    SImode,
    0,
    1
  },
  {
    immediate_operand,
    "I,I",
    SImode,
    0,
    1
  },
  {
    register_operand,
    "=d",
    SImode,
    0,
    1
  },
  {
    register_operand,
    "d",
    SImode,
    0,
    1
  },
  {
    arith_operand,
    "dn",
    SImode,
    0,
    1
  },
  {
    register_operand,
    "=d",
    DImode,
    0,
    1
  },
  {
    register_operand,
    "d",
    DImode,
    0,
    1
  },
  {
    arith_operand,
    "dn",
    DImode,
    0,
    1
  },
  {
    cmp_op,
    "",
    CCmode,
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
    "z",
    CCmode,
    0,
    1
  },
  {
    cmp_op,
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
    register_operand,
    "d",
    SImode,
    0,
    1
  },
  {
    cmp_op,
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
    se_register_operand,
    "d",
    DImode,
    0,
    1
  },
  {
    equality_op,
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
    register_operand,
    "d",
    SImode,
    0,
    1
  },
  {
    register_operand,
    "d",
    SImode,
    0,
    1
  },
  {
    equality_op,
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
    se_register_operand,
    "d",
    DImode,
    0,
    1
  },
  {
    se_register_operand,
    "d",
    DImode,
    0,
    1
  },
  {
    equality_op,
    "",
    SImode,
    0,
    0
  },
  {
    register_operand,
    "d,t",
    SImode,
    0,
    1
  },
  {
    pc_or_label_operand,
    "",
    VOIDmode,
    0,
    1
  },
  {
    pc_or_label_operand,
    "",
    VOIDmode,
    0,
    1
  },
  {
    equality_op,
    "",
    DImode,
    0,
    0
  },
  {
    se_register_operand,
    "d,t",
    DImode,
    0,
    1
  },
  {
    pc_or_label_operand,
    "",
    VOIDmode,
    0,
    1
  },
  {
    pc_or_label_operand,
    "",
    VOIDmode,
    0,
    1
  },
  {
    register_operand,
    "=t",
    SImode,
    0,
    1
  },
  {
    register_operand,
    "d",
    SImode,
    0,
    1
  },
  {
    register_operand,
    "=t",
    DImode,
    0,
    1
  },
  {
    se_register_operand,
    "d",
    DImode,
    0,
    1
  },
  {
    register_operand,
    "=d",
    SImode,
    0,
    1
  },
  {
    register_operand,
    "d",
    SImode,
    0,
    1
  },
  {
    reg_or_0_operand,
    "dJ",
    SImode,
    0,
    1
  },
  {
    register_operand,
    "=t",
    SImode,
    0,
    1
  },
  {
    register_operand,
    "d",
    SImode,
    0,
    1
  },
  {
    register_operand,
    "d",
    SImode,
    0,
    1
  },
  {
    register_operand,
    "=d",
    DImode,
    0,
    1
  },
  {
    se_register_operand,
    "d",
    DImode,
    0,
    1
  },
  {
    se_reg_or_0_operand,
    "dJ",
    DImode,
    0,
    1
  },
  {
    register_operand,
    "=t,t",
    SImode,
    0,
    1
  },
  {
    register_operand,
    "d,d",
    SImode,
    0,
    1
  },
  {
    arith_operand,
    "d,I",
    SImode,
    0,
    1
  },
  {
    register_operand,
    "=d",
    DImode,
    0,
    1
  },
  {
    se_register_operand,
    "d",
    DImode,
    0,
    1
  },
  {
    se_arith_operand,
    "dI",
    DImode,
    0,
    1
  },
  {
    register_operand,
    "=t,t",
    DImode,
    0,
    1
  },
  {
    se_register_operand,
    "d,d",
    DImode,
    0,
    1
  },
  {
    se_arith_operand,
    "d,I",
    DImode,
    0,
    1
  },
  {
    register_operand,
    "=d",
    SImode,
    0,
    1
  },
  {
    register_operand,
    "d",
    SImode,
    0,
    1
  },
  {
    small_int,
    "I",
    SImode,
    0,
    1
  },
  {
    register_operand,
    "=t",
    SImode,
    0,
    1
  },
  {
    register_operand,
    "d",
    SImode,
    0,
    1
  },
  {
    small_int,
    "I",
    SImode,
    0,
    1
  },
  {
    register_operand,
    "=d",
    DImode,
    0,
    1
  },
  {
    se_register_operand,
    "d",
    DImode,
    0,
    1
  },
  {
    small_int,
    "I",
    DImode,
    0,
    1
  },
  {
    register_operand,
    "=t",
    DImode,
    0,
    1
  },
  {
    se_register_operand,
    "d",
    DImode,
    0,
    1
  },
  {
    small_int,
    "I",
    DImode,
    0,
    1
  },
  {
    register_operand,
    "=t",
    DImode,
    0,
    1
  },
  {
    se_register_operand,
    "d",
    DImode,
    0,
    1
  },
  {
    se_register_operand,
    "d",
    DImode,
    0,
    1
  },
  {
    register_operand,
    "=z",
    CCmode,
    0,
    1
  },
  {
    register_operand,
    "f",
    DFmode,
    0,
    1
  },
  {
    register_operand,
    "f",
    DFmode,
    0,
    1
  },
  {
    register_operand,
    "=z",
    CCmode,
    0,
    1
  },
  {
    register_operand,
    "f",
    SFmode,
    0,
    1
  },
  {
    register_operand,
    "f",
    SFmode,
    0,
    1
  },
  {
    register_operand,
    "d",
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
    se_register_operand,
    "d",
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
    "d",
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
    "=d",
    SImode,
    0,
    1
  },
  {
    register_operand,
    "d",
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
    "=d",
    DImode,
    0,
    1
  },
  {
    pmode_register_operand,
    "",
    VOIDmode,
    0,
    1
  },
  {
    register_operand,
    "=d",
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
    "d",
    DImode,
    0,
    1
  },
  {
    scratch_operand,
    "=&d",
    DImode,
    0,
    0
  },
  {
    call_insn_operand,
    "ei",
    VOIDmode,
    0,
    1
  },
  {
    0,
    "i",
    VOIDmode,
    0,
    1
  },
  {
    register_operand,
    "=y",
    SImode,
    0,
    1
  },
  {
    call_insn_operand,
    "ri",
    VOIDmode,
    0,
    1
  },
  {
    0,
    "i",
    VOIDmode,
    0,
    1
  },
  {
    register_operand,
    "=d",
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
    0,
    "i",
    VOIDmode,
    0,
    1
  },
  {
    register_operand,
    "=d",
    SImode,
    0,
    1
  },
  {
    se_register_operand,
    "r",
    DImode,
    0,
    1
  },
  {
    0,
    "i",
    VOIDmode,
    0,
    1
  },
  {
    register_operand,
    "=d",
    SImode,
    0,
    1
  },
  {
    register_operand,
    "e",
    SImode,
    0,
    1
  },
  {
    0,
    "i",
    VOIDmode,
    0,
    1
  },
  {
    register_operand,
    "=y",
    SImode,
    0,
    1
  },
  {
    register_operand,
    "=d",
    VOIDmode,
    0,
    1
  },
  {
    call_insn_operand,
    "ei",
    VOIDmode,
    0,
    1
  },
  {
    0,
    "i",
    VOIDmode,
    0,
    1
  },
  {
    register_operand,
    "=y",
    SImode,
    0,
    1
  },
  {
    register_operand,
    "=df",
    VOIDmode,
    0,
    1
  },
  {
    call_insn_operand,
    "ri",
    VOIDmode,
    0,
    1
  },
  {
    0,
    "i",
    VOIDmode,
    0,
    1
  },
  {
    register_operand,
    "=d",
    SImode,
    0,
    1
  },
  {
    register_operand,
    "=df",
    VOIDmode,
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
    0,
    "i",
    VOIDmode,
    0,
    1
  },
  {
    register_operand,
    "=d",
    SImode,
    0,
    1
  },
  {
    register_operand,
    "=df",
    VOIDmode,
    0,
    1
  },
  {
    se_register_operand,
    "r",
    DImode,
    0,
    1
  },
  {
    0,
    "i",
    VOIDmode,
    0,
    1
  },
  {
    register_operand,
    "=d",
    SImode,
    0,
    1
  },
  {
    register_operand,
    "=df",
    VOIDmode,
    0,
    1
  },
  {
    register_operand,
    "e",
    SImode,
    0,
    1
  },
  {
    0,
    "i",
    VOIDmode,
    0,
    1
  },
  {
    register_operand,
    "=y",
    SImode,
    0,
    1
  },
  {
    register_operand,
    "=df",
    VOIDmode,
    0,
    1
  },
  {
    call_insn_operand,
    "ri",
    VOIDmode,
    0,
    1
  },
  {
    0,
    "i",
    VOIDmode,
    0,
    1
  },
  {
    register_operand,
    "=df",
    VOIDmode,
    0,
    1
  },
  {
    register_operand,
    "=d",
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
    const_int_operand,
    "i",
    SImode,
    0,
    1
  },
  {
    se_register_operand,
    "r",
    DImode,
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
    const_int_operand,
    "n",
    DImode,
    0,
    1
  },
  {
    const_int_operand,
    "i",
    DImode,
    0,
    1
  },
  {
    register_operand,
    "=d,d",
    SImode,
    0,
    1
  },
  {
    register_operand,
    "d,d",
    SImode,
    0,
    1
  },
  {
    reg_or_0_operand,
    "dJ,0",
    SImode,
    0,
    1
  },
  {
    reg_or_0_operand,
    "0,dJ",
    SImode,
    0,
    1
  },
  {
    equality_op,
    "",
    VOIDmode,
    0,
    0
  },
  {
    register_operand,
    "=d,d",
    SImode,
    0,
    1
  },
  {
    se_register_operand,
    "d,d",
    DImode,
    0,
    1
  },
  {
    reg_or_0_operand,
    "dJ,0",
    SImode,
    0,
    1
  },
  {
    reg_or_0_operand,
    "0,dJ",
    SImode,
    0,
    1
  },
  {
    equality_op,
    "",
    VOIDmode,
    0,
    0
  },
  {
    register_operand,
    "=d,d",
    SImode,
    0,
    1
  },
  {
    reg_or_0_operand,
    "dJ,0",
    SImode,
    0,
    1
  },
  {
    reg_or_0_operand,
    "0,dJ",
    SImode,
    0,
    1
  },
  {
    equality_op,
    "",
    VOIDmode,
    0,
    0
  },
  {
    register_operand,
    "z,z",
    CCmode,
    0,
    1
  },
  {
    register_operand,
    "=d,d",
    DImode,
    0,
    1
  },
  {
    register_operand,
    "d,d",
    SImode,
    0,
    1
  },
  {
    se_reg_or_0_operand,
    "dJ,0",
    DImode,
    0,
    1
  },
  {
    se_reg_or_0_operand,
    "0,dJ",
    DImode,
    0,
    1
  },
  {
    equality_op,
    "",
    VOIDmode,
    0,
    0
  },
  {
    register_operand,
    "=d,d",
    DImode,
    0,
    1
  },
  {
    se_register_operand,
    "d,d",
    DImode,
    0,
    1
  },
  {
    se_reg_or_0_operand,
    "dJ,0",
    DImode,
    0,
    1
  },
  {
    se_reg_or_0_operand,
    "0,dJ",
    DImode,
    0,
    1
  },
  {
    equality_op,
    "",
    VOIDmode,
    0,
    0
  },
  {
    register_operand,
    "=d,d",
    DImode,
    0,
    1
  },
  {
    se_reg_or_0_operand,
    "dJ,0",
    DImode,
    0,
    1
  },
  {
    se_reg_or_0_operand,
    "0,dJ",
    DImode,
    0,
    1
  },
  {
    equality_op,
    "",
    VOIDmode,
    0,
    0
  },
  {
    register_operand,
    "z,z",
    CCmode,
    0,
    1
  },
  {
    register_operand,
    "=f,f",
    SFmode,
    0,
    1
  },
  {
    register_operand,
    "d,d",
    SImode,
    0,
    1
  },
  {
    register_operand,
    "f,0",
    SFmode,
    0,
    1
  },
  {
    register_operand,
    "0,f",
    SFmode,
    0,
    1
  },
  {
    equality_op,
    "",
    VOIDmode,
    0,
    0
  },
  {
    register_operand,
    "=f,f",
    SFmode,
    0,
    1
  },
  {
    se_register_operand,
    "d,d",
    DImode,
    0,
    1
  },
  {
    register_operand,
    "f,0",
    SFmode,
    0,
    1
  },
  {
    register_operand,
    "0,f",
    SFmode,
    0,
    1
  },
  {
    equality_op,
    "",
    VOIDmode,
    0,
    0
  },
  {
    register_operand,
    "=f,f",
    SFmode,
    0,
    1
  },
  {
    register_operand,
    "f,0",
    SFmode,
    0,
    1
  },
  {
    register_operand,
    "0,f",
    SFmode,
    0,
    1
  },
  {
    equality_op,
    "",
    VOIDmode,
    0,
    0
  },
  {
    register_operand,
    "z,z",
    CCmode,
    0,
    1
  },
  {
    register_operand,
    "=f,f",
    DFmode,
    0,
    1
  },
  {
    register_operand,
    "d,d",
    SImode,
    0,
    1
  },
  {
    register_operand,
    "f,0",
    DFmode,
    0,
    1
  },
  {
    register_operand,
    "0,f",
    DFmode,
    0,
    1
  },
  {
    equality_op,
    "",
    VOIDmode,
    0,
    0
  },
  {
    register_operand,
    "=f,f",
    DFmode,
    0,
    1
  },
  {
    se_register_operand,
    "d,d",
    DImode,
    0,
    1
  },
  {
    register_operand,
    "f,0",
    DFmode,
    0,
    1
  },
  {
    register_operand,
    "0,f",
    DFmode,
    0,
    1
  },
  {
    equality_op,
    "",
    VOIDmode,
    0,
    0
  },
  {
    register_operand,
    "=f,f",
    DFmode,
    0,
    1
  },
  {
    register_operand,
    "f,0",
    DFmode,
    0,
    1
  },
  {
    register_operand,
    "0,f",
    DFmode,
    0,
    1
  },
  {
    equality_op,
    "",
    VOIDmode,
    0,
    0
  },
  {
    register_operand,
    "z,z",
    CCmode,
    0,
    1
  },
  {
    consttable_operand,
    "=g",
    QImode,
    0,
    1
  },
  {
    consttable_operand,
    "=g",
    HImode,
    0,
    1
  },
  {
    consttable_operand,
    "=g",
    SImode,
    0,
    1
  },
  {
    consttable_operand,
    "=g",
    DImode,
    0,
    1
  },
  {
    consttable_operand,
    "=g",
    SFmode,
    0,
    1
  },
  {
    consttable_operand,
    "=g",
    DFmode,
    0,
    1
  },
  {
    register_operand,
    "=d",
    SImode,
    0,
    1
  },
  {
    address_operand,
    "p",
    SImode,
    0,
    1
  },
  {
    register_operand,
    "=d",
    DImode,
    0,
    1
  },
  {
    address_operand,
    "p",
    DImode,
    0,
    1
  },
  {
    cmp_op,
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
    register_operand,
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
    register_operand,
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
    const_int_operand,
    "",
    SImode,
    0,
    1
  },
  {
    register_operand,
    "",
    DImode,
    0,
    1
  },
  {
    se_register_operand,
    "",
    DImode,
    0,
    1
  },
  {
    se_arith_operand,
    "",
    DImode,
    0,
    1
  },
  {
    register_operand,
    "",
    DImode,
    0,
    1
  },
  {
    register_operand,
    "",
    DImode,
    0,
    1
  },
  {
    register_operand,
    "",
    DImode,
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
    register_operand,
    "",
    DImode,
    0,
    1
  },
  {
    register_operand,
    "",
    DImode,
    0,
    1
  },
  {
    small_int,
    "",
    DImode,
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
    register_operand,
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
    register_operand,
    "",
    DImode,
    0,
    1
  },
  {
    register_operand,
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
    register_operand,
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
    register_operand,
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
    register_operand,
    "=d",
    SImode,
    0,
    1
  },
  {
    register_operand,
    "d",
    SImode,
    0,
    1
  },
  {
    register_operand,
    "d",
    SImode,
    0,
    1
  },
  {
    register_operand,
    "=d",
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
    "=h",
    SImode,
    0,
    0
  },
  {
    scratch_operand,
    "=a",
    SImode,
    0,
    0
  },
  {
    register_operand,
    "=d",
    DImode,
    0,
    1
  },
  {
    se_register_operand,
    "d",
    DImode,
    0,
    1
  },
  {
    se_register_operand,
    "d",
    DImode,
    0,
    1
  },
  {
    register_operand,
    "=d",
    DImode,
    0,
    1
  },
  {
    scratch_operand,
    "=l",
    DImode,
    0,
    0
  },
  {
    scratch_operand,
    "=h",
    DImode,
    0,
    0
  },
  {
    scratch_operand,
    "=a",
    DImode,
    0,
    0
  },
  {
    register_operand,
    "d",
    VOIDmode,
    0,
    1
  },
  {
    true_reg_or_0_operand,
    "dJ",
    VOIDmode,
    0,
    1
  },
  {
    immediate_operand,
    "",
    VOIDmode,
    0,
    1
  },
  {
    register_operand,
    "=l",
    DImode,
    0,
    1
  },
  {
    se_register_operand,
    "d",
    DImode,
    0,
    1
  },
  {
    se_register_operand,
    "d",
    DImode,
    0,
    1
  },
  {
    scratch_operand,
    "=h",
    DImode,
    0,
    0
  },
  {
    scratch_operand,
    "=a",
    DImode,
    0,
    0
  },
  {
    register_operand,
    "=h",
    SImode,
    0,
    1
  },
  {
    register_operand,
    "d",
    SImode,
    0,
    1
  },
  {
    register_operand,
    "d",
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
    "=a",
    SImode,
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
    se_register_operand,
    "d",
    DImode,
    0,
    1
  },
  {
    se_register_operand,
    "di",
    DImode,
    0,
    1
  },
  {
    scratch_operand,
    "=h",
    DImode,
    0,
    0
  },
  {
    scratch_operand,
    "=a",
    DImode,
    0,
    0
  },
  {
    register_operand,
    "=h",
    DImode,
    0,
    1
  },
  {
    se_register_operand,
    "d",
    DImode,
    0,
    1
  },
  {
    se_register_operand,
    "di",
    DImode,
    0,
    1
  },
  {
    scratch_operand,
    "=l",
    DImode,
    0,
    0
  },
  {
    scratch_operand,
    "=a",
    DImode,
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
    nonimmediate_operand,
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
    nonimmediate_operand,
    "",
    HImode,
    0,
    1
  },
  {
    register_operand,
    "",
    DImode,
    0,
    1
  },
  {
    nonimmediate_operand,
    "",
    HImode,
    0,
    1
  },
  {
    register_operand,
    "",
    HImode,
    0,
    1
  },
  {
    nonimmediate_operand,
    "",
    QImode,
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
    nonimmediate_operand,
    "",
    QImode,
    0,
    1
  },
  {
    register_operand,
    "",
    DImode,
    0,
    1
  },
  {
    nonimmediate_operand,
    "",
    QImode,
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
    register_operand,
    "",
    DFmode,
    0,
    1
  },
  {
    register_operand,
    "",
    DImode,
    0,
    1
  },
  {
    register_operand,
    "",
    DFmode,
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
    register_operand,
    "",
    SFmode,
    0,
    1
  },
  {
    register_operand,
    "",
    DImode,
    0,
    1
  },
  {
    register_operand,
    "",
    SFmode,
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
    QImode,
    0,
    1
  },
  {
    immediate_operand,
    "",
    VOIDmode,
    0,
    1
  },
  {
    immediate_operand,
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
    nonimmediate_operand,
    "",
    DImode,
    0,
    1
  },
  {
    general_operand,
    "",
    DImode,
    0,
    1
  },
  {
    register_operand,
    "=b",
    DImode,
    0,
    1
  },
  {
    0,
    "b",
    DImode,
    0,
    1
  },
  {
    register_operand,
    "=&d",
    TImode,
    0,
    1
  },
  {
    general_operand,
    "=b",
    DImode,
    0,
    1
  },
  {
    se_register_operand,
    "b",
    DImode,
    0,
    1
  },
  {
    register_operand,
    "=&d",
    TImode,
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
    large_int,
    "",
    SImode,
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
    general_operand,
    "",
    SImode,
    0,
    1
  },
  {
    general_operand,
    "=b",
    SImode,
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
    "=&d",
    SImode,
    0,
    1
  },
  {
    register_operand,
    "=b",
    SImode,
    0,
    1
  },
  {
    0,
    "b",
    SImode,
    0,
    1
  },
  {
    register_operand,
    "=&d",
    SImode,
    0,
    1
  },
  {
    fcc_register_operand,
    "=z",
    CCmode,
    0,
    1
  },
  {
    general_operand,
    "",
    CCmode,
    0,
    1
  },
  {
    register_operand,
    "=&f",
    TFmode,
    0,
    1
  },
  {
    fcc_register_operand,
    "=z",
    CCmode,
    0,
    1
  },
  {
    register_operand,
    "",
    CCmode,
    0,
    1
  },
  {
    register_operand,
    "=&f",
    TFmode,
    0,
    1
  },
  {
    nonimmediate_operand,
    "",
    HImode,
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
    register_operand,
    "",
    HImode,
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
    nonimmediate_operand,
    "",
    QImode,
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
    register_operand,
    "",
    QImode,
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
    nonimmediate_operand,
    "",
    SFmode,
    0,
    1
  },
  {
    general_operand,
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
    general_operand,
    "",
    DFmode,
    0,
    1
  },
  {
    register_operand,
    "",
    DFmode,
    0,
    1
  },
  {
    register_operand,
    "",
    DFmode,
    0,
    1
  },
  {
    general_operand,
    "",
    BLKmode,
    0,
    1
  },
  {
    general_operand,
    "",
    BLKmode,
    0,
    1
  },
  {
    arith32_operand,
    "",
    SImode,
    0,
    1
  },
  {
    immediate_operand,
    "",
    SImode,
    0,
    1
  },
  {
    register_operand,
    "",
    DImode,
    0,
    1
  },
  {
    se_register_operand,
    "",
    DImode,
    0,
    1
  },
  {
    arith_operand,
    "",
    SImode,
    0,
    1
  },
  {
    register_operand,
    "",
    DImode,
    0,
    1
  },
  {
    register_operand,
    "",
    DImode,
    0,
    1
  },
  {
    small_int,
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
    register_operand,
    "",
    DImode,
    0,
    1
  },
  {
    register_operand,
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
    register_operand,
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
    immediate_operand,
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
    arith_operand,
    "",
    SImode,
    0,
    1
  },
  {
    register_operand,
    "",
    SFmode,
    0,
    1
  },
  {
    register_operand,
    "",
    SFmode,
    0,
    1
  },
  {
    register_operand,
    "d",
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
    "d",
    HImode,
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
    "d",
    SImode,
    0,
    1
  },
  {
    arith_operand,
    "dI",
    SImode,
    0,
    1
  },
  {
    arith_operand,
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
    "r",
    VOIDmode,
    0,
    1
  },
  {
    register_operand,
    "r",
    DImode,
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
    memory_operand,
    "m",
    VOIDmode,
    0,
    1
  },
  {
    0,
    "i",
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
    SImode,
    0,
    1
  },
  {
    register_operand,
    "=df",
    VOIDmode,
    0,
    1
  },
  {
    memory_operand,
    "m",
    VOIDmode,
    0,
    1
  },
  {
    0,
    "i",
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
    SImode,
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
    const_int_operand,
    "",
    VOIDmode,
    0,
    1
  },
  {
    const_int_operand,
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
    comparison_operator,
    "",
    VOIDmode,
    0,
    1
  },
  {
    reg_or_0_operand,
    "",
    SImode,
    0,
    1
  },
  {
    reg_or_0_operand,
    "",
    SImode,
    0,
    1
  },
  {
    register_operand,
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
    1
  },
  {
    se_reg_or_0_operand,
    "",
    DImode,
    0,
    1
  },
  {
    se_reg_or_0_operand,
    "",
    DImode,
    0,
    1
  },
  {
    register_operand,
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
    register_operand,
    "",
    SFmode,
    0,
    1
  },
  {
    register_operand,
    "",
    SFmode,
    0,
    1
  },
  {
    register_operand,
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
    register_operand,
    "",
    DFmode,
    0,
    1
  },
  {
    register_operand,
    "",
    DFmode,
    0,
    1
  },
  {
    register_operand,
    "=t",
    SImode,
    0,
    1
  },
  {
    register_operand,
    "d",
    SImode,
    0,
    1
  },
  {
    equality_op,
    "",
    SImode,
    0,
    0
  },
  {
    pc_or_label_operand,
    "",
    VOIDmode,
    0,
    1
  },
  {
    pc_or_label_operand,
    "",
    VOIDmode,
    0,
    1
  },
  {
    register_operand,
    "=t",
    DImode,
    0,
    1
  },
  {
    register_operand,
    "d",
    DImode,
    0,
    1
  },
  {
    equality_op,
    "",
    DImode,
    0,
    0
  },
  {
    pc_or_label_operand,
    "",
    VOIDmode,
    0,
    1
  },
  {
    pc_or_label_operand,
    "",
    VOIDmode,
    0,
    1
  },
  {
    register_operand,
    "=d",
    SImode,
    0,
    1
  },
  {
    register_operand,
    "t",
    SImode,
    0,
    1
  },
  {
    equality_op,
    "",
    SImode,
    0,
    0
  },
  {
    pc_or_label_operand,
    "",
    VOIDmode,
    0,
    1
  },
  {
    pc_or_label_operand,
    "",
    VOIDmode,
    0,
    1
  },
  {
    register_operand,
    "=d",
    DImode,
    0,
    1
  },
  {
    register_operand,
    "t",
    DImode,
    0,
    1
  },
  {
    equality_op,
    "",
    DImode,
    0,
    0
  },
  {
    pc_or_label_operand,
    "",
    VOIDmode,
    0,
    1
  },
  {
    pc_or_label_operand,
    "",
    VOIDmode,
    0,
    1
  },
};



const struct insn_data insn_data[] = 
{
  {
    "trap",
    (const PTR) output_0,
    (insn_gen_fn) gen_trap,
    &operand_data[0],
    0,
    0,
    0,
    3
  },
  {
    "*mips.md:584",
    "t%C0\t%z1,%z2",
    0,
    &operand_data[1],
    3,
    0,
    1,
    1
  },
  {
    "adddf3",
    "add.d\t%0,%1,%2",
    (insn_gen_fn) gen_adddf3,
    &operand_data[4],
    3,
    0,
    1,
    1
  },
  {
    "addsf3",
    "add.s\t%0,%1,%2",
    (insn_gen_fn) gen_addsf3,
    &operand_data[7],
    3,
    0,
    1,
    1
  },
  {
    "addsi3_internal",
    "addu\t%0,%z1,%2",
    (insn_gen_fn) gen_addsi3_internal,
    &operand_data[10],
    3,
    0,
    1,
    1
  },
  {
    "*mips.md:672",
    "addu\t%$,%$,%0",
    0,
    &operand_data[13],
    1,
    0,
    1,
    1
  },
  {
    "*mips.md:684",
    "addu\t%0,%$,%1",
    0,
    &operand_data[14],
    2,
    0,
    1,
    1
  },
  {
    "*mips.md:696",
    (const PTR) output_7,
    0,
    &operand_data[16],
    3,
    0,
    3,
    3
  },
  {
    "adddi3_internal_1",
    (const PTR) output_8,
    (insn_gen_fn) gen_adddi3_internal_1,
    &operand_data[19],
    4,
    0,
    2,
    3
  },
  {
    "adddi3_internal_2",
    (const PTR) output_9,
    (insn_gen_fn) gen_adddi3_internal_2,
    &operand_data[23],
    4,
    0,
    3,
    2
  },
  {
    "adddi3_internal_3",
    (const PTR) output_10,
    (insn_gen_fn) gen_adddi3_internal_3,
    &operand_data[27],
    3,
    0,
    1,
    3
  },
  {
    "*mips.md:1012",
    "daddu\t%$,%$,%0",
    0,
    &operand_data[30],
    1,
    0,
    1,
    1
  },
  {
    "*mips.md:1024",
    "daddu\t%0,%$,%1",
    0,
    &operand_data[31],
    2,
    0,
    1,
    1
  },
  {
    "*mips.md:1036",
    (const PTR) output_13,
    0,
    &operand_data[33],
    3,
    0,
    3,
    3
  },
  {
    "addsi3_internal_2",
    (const PTR) output_14,
    (insn_gen_fn) gen_addsi3_internal_2,
    &operand_data[36],
    3,
    0,
    1,
    3
  },
  {
    "*mips.md:1158",
    (const PTR) output_15,
    0,
    &operand_data[39],
    3,
    0,
    3,
    3
  },
  {
    "subdf3",
    "sub.d\t%0,%1,%2",
    (insn_gen_fn) gen_subdf3,
    &operand_data[4],
    3,
    0,
    1,
    1
  },
  {
    "subsf3",
    "sub.s\t%0,%1,%2",
    (insn_gen_fn) gen_subsf3,
    &operand_data[7],
    3,
    0,
    1,
    1
  },
  {
    "subsi3_internal",
    "subu\t%0,%z1,%2",
    (insn_gen_fn) gen_subsi3_internal,
    &operand_data[10],
    3,
    0,
    1,
    1
  },
  {
    "*mips.md:1235",
    "addu\t%$,%$,%n0",
    0,
    &operand_data[13],
    1,
    0,
    1,
    1
  },
  {
    "*mips.md:1248",
    "addu\t%0,%$,%n1",
    0,
    &operand_data[14],
    2,
    0,
    1,
    1
  },
  {
    "*mips.md:1262",
    (const PTR) output_21,
    0,
    &operand_data[42],
    3,
    0,
    3,
    3
  },
  {
    "subdi3_internal",
    "sltu\t%3,%L1,%L2\n\tsubu\t%L0,%L1,%L2\n\tsubu\t%M0,%M1,%M2\n\tsubu\t%M0,%M0,%3",
    (insn_gen_fn) gen_subdi3_internal,
    &operand_data[45],
    4,
    0,
    1,
    1
  },
  {
    "subdi3_internal_2",
    (const PTR) output_23,
    (insn_gen_fn) gen_subdi3_internal_2,
    &operand_data[49],
    4,
    0,
    3,
    2
  },
  {
    "subdi3_internal_3",
    (const PTR) output_24,
    (insn_gen_fn) gen_subdi3_internal_3,
    &operand_data[27],
    3,
    0,
    1,
    3
  },
  {
    "*mips.md:1522",
    "daddu\t%$,%$,%n0",
    0,
    &operand_data[30],
    1,
    0,
    1,
    1
  },
  {
    "*mips.md:1535",
    "daddu\t%0,%$,%n1",
    0,
    &operand_data[31],
    2,
    0,
    1,
    1
  },
  {
    "*mips.md:1548",
    (const PTR) output_27,
    0,
    &operand_data[53],
    3,
    0,
    3,
    3
  },
  {
    "subsi3_internal_2",
    (const PTR) output_28,
    (insn_gen_fn) gen_subsi3_internal_2,
    &operand_data[36],
    3,
    0,
    1,
    3
  },
  {
    "*mips.md:1656",
    (const PTR) output_29,
    0,
    &operand_data[39],
    3,
    0,
    3,
    3
  },
  {
    "muldf3_internal",
    "mul.d\t%0,%1,%2",
    (insn_gen_fn) gen_muldf3_internal,
    &operand_data[4],
    3,
    0,
    1,
    1
  },
  {
    "muldf3_r4300",
    (const PTR) output_31,
    (insn_gen_fn) gen_muldf3_r4300,
    &operand_data[4],
    3,
    0,
    1,
    3
  },
  {
    "mulsf3_internal",
    "mul.s\t%0,%1,%2",
    (insn_gen_fn) gen_mulsf3_internal,
    &operand_data[7],
    3,
    0,
    1,
    1
  },
  {
    "mulsf3_r4300",
    (const PTR) output_33,
    (insn_gen_fn) gen_mulsf3_r4300,
    &operand_data[7],
    3,
    0,
    1,
    3
  },
  {
    "mulsi3_mult3",
    (const PTR) output_34,
    (insn_gen_fn) gen_mulsi3_mult3,
    &operand_data[56],
    6,
    0,
    2,
    3
  },
  {
    "mulsi3_internal",
    "mult\t%1,%2",
    (insn_gen_fn) gen_mulsi3_internal,
    &operand_data[62],
    5,
    0,
    1,
    1
  },
  {
    "mulsi3_r4000",
    (const PTR) output_36,
    (insn_gen_fn) gen_mulsi3_r4000,
    &operand_data[67],
    6,
    0,
    1,
    3
  },
  {
    "*mul_acc_si",
    (const PTR) output_37,
    0,
    &operand_data[73],
    8,
    0,
    3,
    3
  },
  {
    "*mul_sub_si",
    (const PTR) output_38,
    0,
    &operand_data[81],
    8,
    0,
    3,
    3
  },
  {
    "*muls",
    (const PTR) output_39,
    0,
    &operand_data[89],
    6,
    0,
    2,
    2
  },
  {
    "*msac",
    (const PTR) output_40,
    0,
    &operand_data[95],
    8,
    0,
    3,
    2
  },
  {
    "muldi3_internal",
    "dmult\t%1,%2",
    (insn_gen_fn) gen_muldi3_internal,
    &operand_data[103],
    5,
    0,
    1,
    1
  },
  {
    "muldi3_internal2",
    (const PTR) output_42,
    (insn_gen_fn) gen_muldi3_internal2,
    &operand_data[108],
    6,
    0,
    1,
    3
  },
  {
    "mulsidi3_internal",
    (const PTR) output_43,
    (insn_gen_fn) gen_mulsidi3_internal,
    &operand_data[114],
    6,
    0,
    1,
    3
  },
  {
    "mulsidi3_64bit",
    (const PTR) output_44,
    (insn_gen_fn) gen_mulsidi3_64bit,
    &operand_data[120],
    7,
    0,
    1,
    3
  },
  {
    "*muls_di",
    (const PTR) output_45,
    0,
    &operand_data[127],
    7,
    0,
    1,
    3
  },
  {
    "*msac_di",
    (const PTR) output_46,
    0,
    &operand_data[134],
    8,
    0,
    1,
    3
  },
  {
    "xmulsi3_highpart_internal",
    (const PTR) output_47,
    (insn_gen_fn) gen_xmulsi3_highpart_internal,
    &operand_data[142],
    8,
    0,
    1,
    3
  },
  {
    "xmulsi3_highpart_mulhi",
    (const PTR) output_48,
    (insn_gen_fn) gen_xmulsi3_highpart_mulhi,
    &operand_data[150],
    9,
    0,
    2,
    3
  },
  {
    "*xmulsi3_neg_highpart_mulhi",
    (const PTR) output_49,
    0,
    &operand_data[150],
    9,
    0,
    2,
    3
  },
  {
    "smuldi3_highpart",
    "dmult\t%1,%2",
    (insn_gen_fn) gen_smuldi3_highpart,
    &operand_data[159],
    5,
    0,
    1,
    1
  },
  {
    "umuldi3_highpart",
    "dmultu\t%1,%2",
    (insn_gen_fn) gen_umuldi3_highpart,
    &operand_data[159],
    5,
    0,
    1,
    1
  },
  {
    "madsi",
    "mad\t%1,%2",
    (insn_gen_fn) gen_madsi,
    &operand_data[164],
    5,
    1,
    1,
    1
  },
  {
    "*mul_acc_di",
    (const PTR) output_53,
    0,
    &operand_data[169],
    6,
    1,
    1,
    3
  },
  {
    "*mul_acc_64bit_di",
    (const PTR) output_54,
    0,
    &operand_data[175],
    7,
    1,
    1,
    3
  },
  {
    "*mips.md:2502",
    "madd.d\t%0,%3,%1,%2",
    0,
    &operand_data[182],
    4,
    0,
    1,
    1
  },
  {
    "*mips.md:2512",
    "madd.s\t%0,%3,%1,%2",
    0,
    &operand_data[186],
    4,
    0,
    1,
    1
  },
  {
    "*mips.md:2522",
    "msub.d\t%0,%3,%1,%2",
    0,
    &operand_data[182],
    4,
    0,
    1,
    1
  },
  {
    "*mips.md:2532",
    "msub.s\t%0,%3,%1,%2",
    0,
    &operand_data[186],
    4,
    0,
    1,
    1
  },
  {
    "*mips.md:2543",
    "nmadd.d\t%0,%3,%1,%2",
    0,
    &operand_data[182],
    4,
    0,
    1,
    1
  },
  {
    "*mips.md:2553",
    "nmadd.s\t%0,%3,%1,%2",
    0,
    &operand_data[186],
    4,
    0,
    1,
    1
  },
  {
    "*mips.md:2563",
    "nmsub.d\t%0,%1,%2,%3",
    0,
    &operand_data[182],
    4,
    0,
    1,
    1
  },
  {
    "*mips.md:2573",
    "nmsub.s\t%0,%1,%2,%3",
    0,
    &operand_data[186],
    4,
    0,
    1,
    1
  },
  {
    "divdf3",
    "div.d\t%0,%1,%2",
    (insn_gen_fn) gen_divdf3,
    &operand_data[4],
    3,
    0,
    1,
    1
  },
  {
    "divsf3",
    "div.s\t%0,%1,%2",
    (insn_gen_fn) gen_divsf3,
    &operand_data[7],
    3,
    0,
    1,
    1
  },
  {
    "*mips.md:2609",
    "recip.d\t%0,%2",
    0,
    &operand_data[190],
    3,
    0,
    1,
    1
  },
  {
    "*mips.md:2618",
    "recip.s\t%0,%2",
    0,
    &operand_data[193],
    3,
    0,
    1,
    1
  },
  {
    "divmodsi4_internal",
    "div\t$0,%1,%2",
    (insn_gen_fn) gen_divmodsi4_internal,
    &operand_data[196],
    5,
    2,
    1,
    1
  },
  {
    "divmoddi4_internal",
    "ddiv\t$0,%1,%2",
    (insn_gen_fn) gen_divmoddi4_internal,
    &operand_data[201],
    5,
    2,
    1,
    1
  },
  {
    "udivmodsi4_internal",
    "divu\t$0,%1,%2",
    (insn_gen_fn) gen_udivmodsi4_internal,
    &operand_data[196],
    5,
    2,
    1,
    1
  },
  {
    "udivmoddi4_internal",
    "ddivu\t$0,%1,%2",
    (insn_gen_fn) gen_udivmoddi4_internal,
    &operand_data[201],
    5,
    2,
    1,
    1
  },
  {
    "div_trap_normal",
    (const PTR) output_71,
    (insn_gen_fn) gen_div_trap_normal,
    &operand_data[206],
    3,
    0,
    2,
    3
  },
  {
    "div_trap_mips16",
    (const PTR) output_72,
    (insn_gen_fn) gen_div_trap_mips16,
    &operand_data[206],
    3,
    0,
    2,
    3
  },
  {
    "divsi3_internal",
    "div\t$0,%1,%2",
    (insn_gen_fn) gen_divsi3_internal,
    &operand_data[209],
    5,
    0,
    1,
    1
  },
  {
    "divdi3_internal",
    "ddiv\t$0,%1,%2",
    (insn_gen_fn) gen_divdi3_internal,
    &operand_data[214],
    5,
    0,
    1,
    1
  },
  {
    "modsi3_internal",
    "div\t$0,%1,%2",
    (insn_gen_fn) gen_modsi3_internal,
    &operand_data[219],
    5,
    0,
    1,
    1
  },
  {
    "moddi3_internal",
    "ddiv\t$0,%1,%2",
    (insn_gen_fn) gen_moddi3_internal,
    &operand_data[224],
    5,
    0,
    1,
    1
  },
  {
    "udivsi3_internal",
    "divu\t$0,%1,%2",
    (insn_gen_fn) gen_udivsi3_internal,
    &operand_data[209],
    5,
    0,
    1,
    1
  },
  {
    "udivdi3_internal",
    "ddivu\t$0,%1,%2",
    (insn_gen_fn) gen_udivdi3_internal,
    &operand_data[214],
    5,
    0,
    1,
    1
  },
  {
    "umodsi3_internal",
    "divu\t$0,%1,%2",
    (insn_gen_fn) gen_umodsi3_internal,
    &operand_data[219],
    5,
    0,
    1,
    1
  },
  {
    "umoddi3_internal",
    "ddivu\t$0,%1,%2",
    (insn_gen_fn) gen_umoddi3_internal,
    &operand_data[224],
    5,
    0,
    1,
    1
  },
  {
    "sqrtdf2",
    "sqrt.d\t%0,%1",
    (insn_gen_fn) gen_sqrtdf2,
    &operand_data[4],
    2,
    0,
    1,
    1
  },
  {
    "sqrtsf2",
    "sqrt.s\t%0,%1",
    (insn_gen_fn) gen_sqrtsf2,
    &operand_data[7],
    2,
    0,
    1,
    1
  },
  {
    "*mips.md:3222",
    "rsqrt.d\t%0,%2",
    0,
    &operand_data[190],
    3,
    0,
    1,
    1
  },
  {
    "*mips.md:3231",
    "rsqrt.s\t%0,%2",
    0,
    &operand_data[193],
    3,
    0,
    1,
    1
  },
  {
    "abssi2",
    (const PTR) output_85,
    (insn_gen_fn) gen_abssi2,
    &operand_data[67],
    2,
    0,
    1,
    3
  },
  {
    "absdi2",
    (const PTR) output_86,
    (insn_gen_fn) gen_absdi2,
    &operand_data[108],
    2,
    0,
    1,
    3
  },
  {
    "absdf2",
    "abs.d\t%0,%1",
    (insn_gen_fn) gen_absdf2,
    &operand_data[4],
    2,
    0,
    1,
    1
  },
  {
    "abssf2",
    "abs.s\t%0,%1",
    (insn_gen_fn) gen_abssf2,
    &operand_data[7],
    2,
    0,
    1,
    1
  },
  {
    "ffssi2",
    (const PTR) output_89,
    (insn_gen_fn) gen_ffssi2,
    &operand_data[229],
    4,
    0,
    1,
    3
  },
  {
    "ffsdi2",
    (const PTR) output_90,
    (insn_gen_fn) gen_ffsdi2,
    &operand_data[233],
    4,
    0,
    1,
    3
  },
  {
    "negsi2",
    (const PTR) output_91,
    (insn_gen_fn) gen_negsi2,
    &operand_data[67],
    2,
    0,
    1,
    3
  },
  {
    "negdi2_internal",
    (const PTR) output_92,
    (insn_gen_fn) gen_negdi2_internal,
    &operand_data[237],
    3,
    0,
    1,
    3
  },
  {
    "negdi2_internal_2",
    (const PTR) output_93,
    (insn_gen_fn) gen_negdi2_internal_2,
    &operand_data[108],
    2,
    0,
    1,
    3
  },
  {
    "negdf2",
    "neg.d\t%0,%1",
    (insn_gen_fn) gen_negdf2,
    &operand_data[4],
    2,
    0,
    1,
    1
  },
  {
    "negsf2",
    "neg.s\t%0,%1",
    (insn_gen_fn) gen_negsf2,
    &operand_data[7],
    2,
    0,
    1,
    1
  },
  {
    "one_cmplsi2",
    (const PTR) output_96,
    (insn_gen_fn) gen_one_cmplsi2,
    &operand_data[67],
    2,
    0,
    1,
    3
  },
  {
    "one_cmpldi2",
    (const PTR) output_97,
    (insn_gen_fn) gen_one_cmpldi2,
    &operand_data[108],
    2,
    0,
    1,
    3
  },
  {
    "*mips.md:3553",
    (const PTR) output_98,
    0,
    &operand_data[240],
    3,
    0,
    2,
    2
  },
  {
    "*mips.md:3564",
    "and\t%0,%2",
    0,
    &operand_data[243],
    3,
    0,
    1,
    1
  },
  {
    "*mips.md:3587",
    (const PTR) output_100,
    0,
    &operand_data[246],
    3,
    0,
    1,
    3
  },
  {
    "*mips.md:3605",
    (const PTR) output_101,
    0,
    &operand_data[249],
    3,
    0,
    1,
    3
  },
  {
    "anddi3_internal1",
    (const PTR) output_102,
    (insn_gen_fn) gen_anddi3_internal1,
    &operand_data[252],
    3,
    0,
    2,
    2
  },
  {
    "*mips.md:3662",
    (const PTR) output_103,
    0,
    &operand_data[240],
    3,
    0,
    2,
    2
  },
  {
    "*mips.md:3673",
    "or\t%0,%2",
    0,
    &operand_data[243],
    3,
    0,
    1,
    1
  },
  {
    "*mips.md:3692",
    (const PTR) output_105,
    0,
    &operand_data[246],
    3,
    0,
    1,
    3
  },
  {
    "*mips.md:3710",
    (const PTR) output_106,
    0,
    &operand_data[249],
    3,
    0,
    1,
    3
  },
  {
    "*mips.md:3749",
    (const PTR) output_107,
    0,
    &operand_data[240],
    3,
    0,
    2,
    2
  },
  {
    "*mips.md:3760",
    (const PTR) output_108,
    0,
    &operand_data[255],
    3,
    0,
    3,
    2
  },
  {
    "*mips.md:3787",
    (const PTR) output_109,
    0,
    &operand_data[246],
    3,
    0,
    1,
    3
  },
  {
    "*mips.md:3805",
    "xor\t%M0,%M2\n\txor\t%L0,%L2",
    0,
    &operand_data[249],
    3,
    0,
    1,
    1
  },
  {
    "*mips.md:3815",
    (const PTR) output_111,
    0,
    &operand_data[258],
    3,
    0,
    3,
    2
  },
  {
    "xordi3_immed",
    "xori\t%0,%1,%x2",
    (insn_gen_fn) gen_xordi3_immed,
    &operand_data[261],
    3,
    0,
    1,
    1
  },
  {
    "*norsi3",
    "nor\t%0,%z1,%z2",
    0,
    &operand_data[67],
    3,
    0,
    1,
    1
  },
  {
    "*nordi3",
    (const PTR) output_114,
    0,
    &operand_data[246],
    3,
    0,
    1,
    3
  },
  {
    "truncdfsf2",
    "cvt.s.d\t%0,%1",
    (insn_gen_fn) gen_truncdfsf2,
    &operand_data[264],
    2,
    0,
    1,
    1
  },
  {
    "truncdisi2",
    (const PTR) output_116,
    (insn_gen_fn) gen_truncdisi2,
    &operand_data[266],
    2,
    0,
    1,
    3
  },
  {
    "truncdihi2",
    (const PTR) output_117,
    (insn_gen_fn) gen_truncdihi2,
    &operand_data[268],
    2,
    0,
    1,
    3
  },
  {
    "truncdiqi2",
    (const PTR) output_118,
    (insn_gen_fn) gen_truncdiqi2,
    &operand_data[270],
    2,
    0,
    1,
    3
  },
  {
    "*mips.md:3960",
    (const PTR) output_119,
    0,
    &operand_data[272],
    3,
    0,
    1,
    3
  },
  {
    "*mips.md:3984",
    (const PTR) output_120,
    0,
    &operand_data[272],
    3,
    0,
    1,
    3
  },
  {
    "*mips.md:4010",
    (const PTR) output_121,
    0,
    &operand_data[272],
    3,
    0,
    1,
    3
  },
  {
    "*mips.md:4035",
    "andi\t%0,%1,0xffff",
    0,
    &operand_data[266],
    2,
    0,
    1,
    1
  },
  {
    "*mips.md:4044",
    "andi\t%0,%1,0xff",
    0,
    &operand_data[266],
    2,
    0,
    1,
    1
  },
  {
    "*mips.md:4053",
    "andi\t%0,%1,0xff",
    0,
    &operand_data[268],
    2,
    0,
    1,
    1
  },
  {
    "zero_extendsidi2_internal",
    (const PTR) output_125,
    (insn_gen_fn) gen_zero_extendsidi2_internal,
    &operand_data[275],
    2,
    0,
    2,
    3
  },
  {
    "*mips.md:4118",
    (const PTR) output_126,
    0,
    &operand_data[277],
    2,
    0,
    3,
    3
  },
  {
    "*mips.md:4133",
    (const PTR) output_127,
    0,
    &operand_data[279],
    2,
    0,
    2,
    3
  },
  {
    "*mips.md:4158",
    (const PTR) output_128,
    0,
    &operand_data[281],
    2,
    0,
    3,
    3
  },
  {
    "*mips.md:4173",
    (const PTR) output_129,
    0,
    &operand_data[283],
    2,
    0,
    2,
    3
  },
  {
    "*mips.md:4199",
    (const PTR) output_130,
    0,
    &operand_data[285],
    2,
    0,
    3,
    3
  },
  {
    "*mips.md:4214",
    (const PTR) output_131,
    0,
    &operand_data[287],
    2,
    0,
    2,
    3
  },
  {
    "*mips.md:4239",
    (const PTR) output_132,
    0,
    &operand_data[289],
    2,
    0,
    3,
    3
  },
  {
    "*mips.md:4254",
    (const PTR) output_133,
    0,
    &operand_data[291],
    2,
    0,
    2,
    3
  },
  {
    "*mips.md:4279",
    (const PTR) output_134,
    0,
    &operand_data[293],
    2,
    0,
    3,
    3
  },
  {
    "*paradoxical_extendhidi2",
    (const PTR) output_135,
    0,
    &operand_data[283],
    2,
    0,
    2,
    3
  },
  {
    "*mips.md:4312",
    (const PTR) output_136,
    0,
    &operand_data[295],
    2,
    0,
    2,
    3
  },
  {
    "extendhidi2_internal",
    (const PTR) output_137,
    (insn_gen_fn) gen_extendhidi2_internal,
    &operand_data[283],
    2,
    0,
    2,
    3
  },
  {
    "extendhisi2_internal",
    (const PTR) output_138,
    (insn_gen_fn) gen_extendhisi2_internal,
    &operand_data[279],
    2,
    0,
    2,
    3
  },
  {
    "extendqihi2_internal",
    (const PTR) output_139,
    (insn_gen_fn) gen_extendqihi2_internal,
    &operand_data[287],
    2,
    0,
    2,
    3
  },
  {
    "extendqisi2_insn",
    (const PTR) output_140,
    (insn_gen_fn) gen_extendqisi2_insn,
    &operand_data[291],
    2,
    0,
    2,
    3
  },
  {
    "extendqidi2_insn",
    (const PTR) output_141,
    (insn_gen_fn) gen_extendqidi2_insn,
    &operand_data[295],
    2,
    0,
    2,
    3
  },
  {
    "extendsfdf2",
    "cvt.d.s\t%0,%1",
    (insn_gen_fn) gen_extendsfdf2,
    &operand_data[297],
    2,
    0,
    1,
    1
  },
  {
    "fix_truncdfsi2_insn",
    "trunc.w.d %0,%1",
    (insn_gen_fn) gen_fix_truncdfsi2_insn,
    &operand_data[299],
    2,
    0,
    1,
    1
  },
  {
    "fix_truncdfsi2_macro",
    "trunc.w.d %0,%1,%2",
    (insn_gen_fn) gen_fix_truncdfsi2_macro,
    &operand_data[299],
    3,
    0,
    1,
    1
  },
  {
    "fix_truncsfsi2_insn",
    "trunc.w.s %0,%1",
    (insn_gen_fn) gen_fix_truncsfsi2_insn,
    &operand_data[302],
    2,
    0,
    1,
    1
  },
  {
    "fix_truncsfsi2_macro",
    "trunc.w.s %0,%1,%2",
    (insn_gen_fn) gen_fix_truncsfsi2_macro,
    &operand_data[302],
    3,
    0,
    1,
    1
  },
  {
    "fix_truncdfdi2",
    "trunc.l.d %0,%1",
    (insn_gen_fn) gen_fix_truncdfdi2,
    &operand_data[305],
    2,
    0,
    1,
    1
  },
  {
    "fix_truncsfdi2",
    "trunc.l.s %0,%1",
    (insn_gen_fn) gen_fix_truncsfdi2,
    &operand_data[307],
    2,
    0,
    1,
    1
  },
  {
    "floatsidf2",
    "cvt.d.w\t%0,%1",
    (insn_gen_fn) gen_floatsidf2,
    &operand_data[309],
    2,
    0,
    1,
    1
  },
  {
    "floatdidf2",
    "cvt.d.l\t%0,%1",
    (insn_gen_fn) gen_floatdidf2,
    &operand_data[311],
    2,
    0,
    1,
    1
  },
  {
    "floatsisf2",
    "cvt.s.w\t%0,%1",
    (insn_gen_fn) gen_floatsisf2,
    &operand_data[313],
    2,
    0,
    1,
    1
  },
  {
    "floatdisf2",
    "cvt.s.l\t%0,%1",
    (insn_gen_fn) gen_floatdisf2,
    &operand_data[315],
    2,
    0,
    1,
    1
  },
  {
    "movsi_ulw",
    (const PTR) output_153,
    (insn_gen_fn) gen_movsi_ulw,
    &operand_data[317],
    2,
    0,
    2,
    3
  },
  {
    "movsi_usw",
    (const PTR) output_154,
    (insn_gen_fn) gen_movsi_usw,
    &operand_data[319],
    2,
    0,
    2,
    3
  },
  {
    "movdi_uld",
    (const PTR) output_155,
    (insn_gen_fn) gen_movdi_uld,
    &operand_data[321],
    2,
    0,
    2,
    3
  },
  {
    "movdi_usd",
    (const PTR) output_156,
    (insn_gen_fn) gen_movdi_usd,
    &operand_data[323],
    2,
    0,
    2,
    3
  },
  {
    "high",
    "lui\t%0,%%hi(%1) # high",
    (insn_gen_fn) gen_high,
    &operand_data[325],
    2,
    0,
    1,
    1
  },
  {
    "low",
    "addiu\t%0,%1,%%lo(%2) # low",
    (insn_gen_fn) gen_low,
    &operand_data[327],
    3,
    0,
    1,
    1
  },
  {
    "*mips.md:5249",
    (const PTR) output_159,
    0,
    &operand_data[330],
    1,
    0,
    2,
    3
  },
  {
    "movdi_internal",
    (const PTR) output_160,
    (insn_gen_fn) gen_movdi_internal,
    &operand_data[331],
    2,
    0,
    15,
    3
  },
  {
    "*mips.md:5275",
    (const PTR) output_161,
    0,
    &operand_data[333],
    2,
    0,
    10,
    3
  },
  {
    "movdi_internal2",
    (const PTR) output_162,
    (insn_gen_fn) gen_movdi_internal2,
    &operand_data[335],
    2,
    0,
    24,
    3
  },
  {
    "*movdi_internal2_extend",
    (const PTR) output_163,
    0,
    &operand_data[337],
    2,
    0,
    19,
    3
  },
  {
    "*movdi_internal2_mips16",
    (const PTR) output_164,
    0,
    &operand_data[339],
    2,
    0,
    11,
    3
  },
  {
    "*mips.md:5700",
    "sw\t$31,%0($sp)",
    0,
    &operand_data[341],
    1,
    0,
    1,
    1
  },
  {
    "movsi_internal",
    (const PTR) output_166,
    (insn_gen_fn) gen_movsi_internal,
    &operand_data[342],
    2,
    0,
    26,
    3
  },
  {
    "*mips.md:5739",
    (const PTR) output_167,
    0,
    &operand_data[344],
    2,
    0,
    12,
    3
  },
  {
    "hilo_delay",
    "",
    (insn_gen_fn) gen_hilo_delay,
    &operand_data[346],
    1,
    0,
    1,
    1
  },
  {
    "movcc",
    (const PTR) output_169,
    (insn_gen_fn) gen_movcc,
    &operand_data[347],
    2,
    0,
    13,
    3
  },
  {
    "*mips.md:6074",
    "lwxc1\t%0,%1(%2)",
    0,
    &operand_data[349],
    3,
    0,
    1,
    1
  },
  {
    "*mips.md:6083",
    "lwxc1\t%0,%1(%2)",
    0,
    &operand_data[352],
    3,
    0,
    1,
    1
  },
  {
    "*mips.md:6092",
    "ldxc1\t%0,%1(%2)",
    0,
    &operand_data[355],
    3,
    0,
    1,
    1
  },
  {
    "*mips.md:6101",
    "ldxc1\t%0,%1(%2)",
    0,
    &operand_data[358],
    3,
    0,
    1,
    1
  },
  {
    "*mips.md:6110",
    "swxc1\t%0,%1(%2)",
    0,
    &operand_data[361],
    3,
    0,
    1,
    1
  },
  {
    "*mips.md:6119",
    "swxc1\t%0,%1(%2)",
    0,
    &operand_data[364],
    3,
    0,
    1,
    1
  },
  {
    "*mips.md:6128",
    "sdxc1\t%0,%1(%2)",
    0,
    &operand_data[367],
    3,
    0,
    1,
    1
  },
  {
    "*mips.md:6137",
    "sdxc1\t%0,%1(%2)",
    0,
    &operand_data[370],
    3,
    0,
    1,
    1
  },
  {
    "movhi_internal",
    (const PTR) output_178,
    (insn_gen_fn) gen_movhi_internal,
    &operand_data[373],
    2,
    0,
    11,
    3
  },
  {
    "*mips.md:6187",
    (const PTR) output_179,
    0,
    &operand_data[375],
    2,
    0,
    10,
    3
  },
  {
    "movqi_internal",
    (const PTR) output_180,
    (insn_gen_fn) gen_movqi_internal,
    &operand_data[377],
    2,
    0,
    11,
    3
  },
  {
    "*mips.md:6297",
    (const PTR) output_181,
    0,
    &operand_data[379],
    2,
    0,
    10,
    3
  },
  {
    "movsf_internal1",
    (const PTR) output_182,
    (insn_gen_fn) gen_movsf_internal1,
    &operand_data[381],
    2,
    0,
    13,
    3
  },
  {
    "movsf_internal2",
    (const PTR) output_183,
    (insn_gen_fn) gen_movsf_internal2,
    &operand_data[383],
    2,
    0,
    5,
    3
  },
  {
    "*mips.md:6390",
    (const PTR) output_184,
    0,
    &operand_data[385],
    2,
    0,
    7,
    3
  },
  {
    "movdf_internal1",
    (const PTR) output_185,
    (insn_gen_fn) gen_movdf_internal1,
    &operand_data[387],
    2,
    0,
    13,
    3
  },
  {
    "movdf_internal1a",
    (const PTR) output_186,
    (insn_gen_fn) gen_movdf_internal1a,
    &operand_data[389],
    2,
    0,
    11,
    3
  },
  {
    "movdf_internal2",
    (const PTR) output_187,
    (insn_gen_fn) gen_movdf_internal2,
    &operand_data[391],
    2,
    0,
    8,
    3
  },
  {
    "*mips.md:6451",
    (const PTR) output_188,
    0,
    &operand_data[393],
    2,
    0,
    7,
    3
  },
  {
    "loadgp",
    "%[lui\t$1,%%hi(%%neg(%%gp_rel(%a0)))\n\taddiu\t$1,$1,%%lo(%%neg(%%gp_rel(%a0)))\n\tdaddu\t$gp,$1,%1%]",
    (insn_gen_fn) gen_loadgp,
    &operand_data[395],
    2,
    0,
    0,
    1
  },
  {
    "movstrsi_internal",
    (const PTR) output_190,
    (insn_gen_fn) gen_movstrsi_internal,
    &operand_data[397],
    8,
    0,
    1,
    3
  },
  {
    "*mips.md:6532",
    (const PTR) output_191,
    0,
    &operand_data[397],
    8,
    0,
    1,
    3
  },
  {
    "movstrsi_internal2",
    (const PTR) output_192,
    (insn_gen_fn) gen_movstrsi_internal2,
    &operand_data[397],
    8,
    0,
    1,
    3
  },
  {
    "*mips.md:6613",
    (const PTR) output_193,
    0,
    &operand_data[397],
    8,
    0,
    1,
    3
  },
  {
    "movstrsi_internal3",
    (const PTR) output_194,
    (insn_gen_fn) gen_movstrsi_internal3,
    &operand_data[405],
    8,
    0,
    1,
    3
  },
  {
    "ashlsi3_internal1",
    (const PTR) output_195,
    (insn_gen_fn) gen_ashlsi3_internal1,
    &operand_data[413],
    3,
    0,
    1,
    3
  },
  {
    "ashlsi3_internal2",
    (const PTR) output_196,
    (insn_gen_fn) gen_ashlsi3_internal2,
    &operand_data[416],
    3,
    0,
    2,
    3
  },
  {
    "ashldi3_internal",
    (const PTR) output_197,
    (insn_gen_fn) gen_ashldi3_internal,
    &operand_data[419],
    4,
    0,
    1,
    3
  },
  {
    "ashldi3_internal2",
    (const PTR) output_198,
    (insn_gen_fn) gen_ashldi3_internal2,
    &operand_data[423],
    4,
    0,
    1,
    3
  },
  {
    "ashldi3_internal3",
    (const PTR) output_199,
    (insn_gen_fn) gen_ashldi3_internal3,
    &operand_data[423],
    4,
    0,
    1,
    3
  },
  {
    "ashldi3_internal4",
    (const PTR) output_200,
    (insn_gen_fn) gen_ashldi3_internal4,
    &operand_data[427],
    3,
    0,
    1,
    3
  },
  {
    "*mips.md:6976",
    (const PTR) output_201,
    0,
    &operand_data[430],
    3,
    0,
    2,
    3
  },
  {
    "ashrsi3_internal1",
    (const PTR) output_202,
    (insn_gen_fn) gen_ashrsi3_internal1,
    &operand_data[413],
    3,
    0,
    1,
    3
  },
  {
    "ashrsi3_internal2",
    (const PTR) output_203,
    (insn_gen_fn) gen_ashrsi3_internal2,
    &operand_data[416],
    3,
    0,
    2,
    3
  },
  {
    "ashrdi3_internal",
    (const PTR) output_204,
    (insn_gen_fn) gen_ashrdi3_internal,
    &operand_data[419],
    4,
    0,
    1,
    3
  },
  {
    "ashrdi3_internal2",
    (const PTR) output_205,
    (insn_gen_fn) gen_ashrdi3_internal2,
    &operand_data[423],
    4,
    0,
    1,
    3
  },
  {
    "ashrdi3_internal3",
    (const PTR) output_206,
    (insn_gen_fn) gen_ashrdi3_internal3,
    &operand_data[423],
    4,
    0,
    1,
    3
  },
  {
    "ashrdi3_internal4",
    (const PTR) output_207,
    (insn_gen_fn) gen_ashrdi3_internal4,
    &operand_data[427],
    3,
    0,
    1,
    3
  },
  {
    "*mips.md:7330",
    (const PTR) output_208,
    0,
    &operand_data[433],
    3,
    0,
    2,
    3
  },
  {
    "lshrsi3_internal1",
    (const PTR) output_209,
    (insn_gen_fn) gen_lshrsi3_internal1,
    &operand_data[413],
    3,
    0,
    1,
    3
  },
  {
    "lshrsi3_internal2",
    (const PTR) output_210,
    (insn_gen_fn) gen_lshrsi3_internal2,
    &operand_data[416],
    3,
    0,
    2,
    3
  },
  {
    "*mips.md:7458",
    "lw\t%0,%1\n\tsrl\t%0,%2",
    0,
    &operand_data[436],
    3,
    0,
    2,
    1
  },
  {
    "lshrdi3_internal",
    (const PTR) output_212,
    (insn_gen_fn) gen_lshrdi3_internal,
    &operand_data[419],
    4,
    0,
    1,
    3
  },
  {
    "lshrdi3_internal2",
    (const PTR) output_213,
    (insn_gen_fn) gen_lshrdi3_internal2,
    &operand_data[423],
    4,
    0,
    1,
    3
  },
  {
    "lshrdi3_internal3",
    (const PTR) output_214,
    (insn_gen_fn) gen_lshrdi3_internal3,
    &operand_data[423],
    4,
    0,
    1,
    3
  },
  {
    "lshrdi3_internal4",
    (const PTR) output_215,
    (insn_gen_fn) gen_lshrdi3_internal4,
    &operand_data[427],
    3,
    0,
    1,
    3
  },
  {
    "*mips.md:7714",
    (const PTR) output_216,
    0,
    &operand_data[433],
    3,
    0,
    2,
    3
  },
  {
    "rotrsi3",
    (const PTR) output_217,
    (insn_gen_fn) gen_rotrsi3,
    &operand_data[439],
    3,
    0,
    1,
    3
  },
  {
    "rotrdi3",
    (const PTR) output_218,
    (insn_gen_fn) gen_rotrdi3,
    &operand_data[442],
    3,
    0,
    1,
    3
  },
  {
    "branch_fp",
    (const PTR) output_219,
    (insn_gen_fn) gen_branch_fp,
    &operand_data[445],
    3,
    0,
    1,
    3
  },
  {
    "branch_fp_inverted",
    (const PTR) output_220,
    (insn_gen_fn) gen_branch_fp_inverted,
    &operand_data[445],
    3,
    0,
    1,
    3
  },
  {
    "branch_zero",
    (const PTR) output_221,
    (insn_gen_fn) gen_branch_zero,
    &operand_data[448],
    3,
    0,
    1,
    3
  },
  {
    "branch_zero_inverted",
    (const PTR) output_222,
    (insn_gen_fn) gen_branch_zero_inverted,
    &operand_data[448],
    3,
    0,
    1,
    3
  },
  {
    "branch_zero_di",
    (const PTR) output_223,
    (insn_gen_fn) gen_branch_zero_di,
    &operand_data[451],
    3,
    0,
    1,
    3
  },
  {
    "branch_zero_di_inverted",
    (const PTR) output_224,
    (insn_gen_fn) gen_branch_zero_di_inverted,
    &operand_data[451],
    3,
    0,
    1,
    3
  },
  {
    "branch_equality",
    (const PTR) output_225,
    (insn_gen_fn) gen_branch_equality,
    &operand_data[454],
    4,
    0,
    1,
    3
  },
  {
    "branch_equality_di",
    (const PTR) output_226,
    (insn_gen_fn) gen_branch_equality_di,
    &operand_data[458],
    4,
    0,
    1,
    3
  },
  {
    "branch_equality_inverted",
    (const PTR) output_227,
    (insn_gen_fn) gen_branch_equality_inverted,
    &operand_data[454],
    4,
    0,
    1,
    3
  },
  {
    "branch_equality_di_inverted",
    (const PTR) output_228,
    (insn_gen_fn) gen_branch_equality_di_inverted,
    &operand_data[458],
    4,
    0,
    1,
    3
  },
  {
    "*mips.md:8140",
    (const PTR) output_229,
    0,
    &operand_data[462],
    4,
    0,
    2,
    3
  },
  {
    "*mips.md:8169",
    (const PTR) output_230,
    0,
    &operand_data[466],
    4,
    0,
    2,
    3
  },
  {
    "seq_si_zero",
    "sltu\t%0,%1,1",
    (insn_gen_fn) gen_seq_si_zero,
    &operand_data[67],
    2,
    0,
    1,
    1
  },
  {
    "*mips.md:8522",
    "sltu\t%1,1",
    0,
    &operand_data[470],
    2,
    0,
    1,
    1
  },
  {
    "seq_di_zero",
    "sltu\t%0,%1,1",
    (insn_gen_fn) gen_seq_di_zero,
    &operand_data[108],
    2,
    0,
    1,
    1
  },
  {
    "*mips.md:8540",
    "sltu\t%1,1",
    0,
    &operand_data[472],
    2,
    0,
    1,
    1
  },
  {
    "sne_si_zero",
    "sltu\t%0,%.,%1",
    (insn_gen_fn) gen_sne_si_zero,
    &operand_data[67],
    2,
    0,
    1,
    1
  },
  {
    "sne_di_zero",
    "sltu\t%0,%.,%1",
    (insn_gen_fn) gen_sne_di_zero,
    &operand_data[108],
    2,
    0,
    1,
    1
  },
  {
    "sgt_si",
    "slt\t%0,%z2,%1",
    (insn_gen_fn) gen_sgt_si,
    &operand_data[474],
    3,
    0,
    1,
    1
  },
  {
    "*mips.md:8736",
    "slt\t%2,%1",
    0,
    &operand_data[477],
    3,
    0,
    1,
    1
  },
  {
    "sgt_di",
    "slt\t%0,%z2,%1",
    (insn_gen_fn) gen_sgt_di,
    &operand_data[480],
    3,
    0,
    1,
    1
  },
  {
    "*mips.md:8754",
    "slt\t%2,%1",
    0,
    &operand_data[246],
    3,
    0,
    1,
    1
  },
  {
    "slt_si",
    "slt\t%0,%1,%2",
    (insn_gen_fn) gen_slt_si,
    &operand_data[413],
    3,
    0,
    1,
    1
  },
  {
    "*mips.md:8865",
    "slt\t%1,%2",
    0,
    &operand_data[483],
    3,
    0,
    2,
    1
  },
  {
    "slt_di",
    "slt\t%0,%1,%2",
    (insn_gen_fn) gen_slt_di,
    &operand_data[486],
    3,
    0,
    1,
    1
  },
  {
    "*mips.md:8888",
    "slt\t%1,%2",
    0,
    &operand_data[489],
    3,
    0,
    2,
    1
  },
  {
    "sle_si_const",
    (const PTR) output_245,
    (insn_gen_fn) gen_sle_si_const,
    &operand_data[492],
    3,
    0,
    1,
    3
  },
  {
    "*mips.md:8941",
    (const PTR) output_246,
    0,
    &operand_data[495],
    3,
    0,
    1,
    3
  },
  {
    "sle_di_const",
    (const PTR) output_247,
    (insn_gen_fn) gen_sle_di_const,
    &operand_data[498],
    3,
    0,
    1,
    3
  },
  {
    "*mips.md:8970",
    (const PTR) output_248,
    0,
    &operand_data[501],
    3,
    0,
    1,
    3
  },
  {
    "sgtu_si",
    "sltu\t%0,%z2,%1",
    (insn_gen_fn) gen_sgtu_si,
    &operand_data[474],
    3,
    0,
    1,
    1
  },
  {
    "*mips.md:9068",
    "sltu\t%2,%1",
    0,
    &operand_data[477],
    3,
    0,
    1,
    1
  },
  {
    "sgtu_di",
    "sltu\t%0,%z2,%1",
    (insn_gen_fn) gen_sgtu_di,
    &operand_data[480],
    3,
    0,
    1,
    1
  },
  {
    "*mips.md:9086",
    "sltu\t%2,%1",
    0,
    &operand_data[504],
    3,
    0,
    1,
    1
  },
  {
    "sltu_si",
    "sltu\t%0,%1,%2",
    (insn_gen_fn) gen_sltu_si,
    &operand_data[413],
    3,
    0,
    1,
    1
  },
  {
    "*mips.md:9197",
    "sltu\t%1,%2",
    0,
    &operand_data[483],
    3,
    0,
    2,
    1
  },
  {
    "sltu_di",
    "sltu\t%0,%1,%2",
    (insn_gen_fn) gen_sltu_di,
    &operand_data[486],
    3,
    0,
    1,
    1
  },
  {
    "*mips.md:9220",
    "sltu\t%1,%2",
    0,
    &operand_data[489],
    3,
    0,
    2,
    1
  },
  {
    "sleu_si_const",
    (const PTR) output_257,
    (insn_gen_fn) gen_sleu_si_const,
    &operand_data[492],
    3,
    0,
    1,
    3
  },
  {
    "*mips.md:9273",
    (const PTR) output_258,
    0,
    &operand_data[495],
    3,
    0,
    1,
    3
  },
  {
    "sleu_di_const",
    (const PTR) output_259,
    (insn_gen_fn) gen_sleu_di_const,
    &operand_data[498],
    3,
    0,
    1,
    3
  },
  {
    "*mips.md:9302",
    (const PTR) output_260,
    0,
    &operand_data[501],
    3,
    0,
    1,
    3
  },
  {
    "sunordered_df",
    (const PTR) output_261,
    (insn_gen_fn) gen_sunordered_df,
    &operand_data[507],
    3,
    0,
    1,
    3
  },
  {
    "sunlt_df",
    (const PTR) output_262,
    (insn_gen_fn) gen_sunlt_df,
    &operand_data[507],
    3,
    0,
    1,
    3
  },
  {
    "suneq_df",
    (const PTR) output_263,
    (insn_gen_fn) gen_suneq_df,
    &operand_data[507],
    3,
    0,
    1,
    3
  },
  {
    "sunle_df",
    (const PTR) output_264,
    (insn_gen_fn) gen_sunle_df,
    &operand_data[507],
    3,
    0,
    1,
    3
  },
  {
    "seq_df",
    (const PTR) output_265,
    (insn_gen_fn) gen_seq_df,
    &operand_data[507],
    3,
    0,
    1,
    3
  },
  {
    "slt_df",
    (const PTR) output_266,
    (insn_gen_fn) gen_slt_df,
    &operand_data[507],
    3,
    0,
    1,
    3
  },
  {
    "sle_df",
    (const PTR) output_267,
    (insn_gen_fn) gen_sle_df,
    &operand_data[507],
    3,
    0,
    1,
    3
  },
  {
    "sgt_df",
    (const PTR) output_268,
    (insn_gen_fn) gen_sgt_df,
    &operand_data[507],
    3,
    0,
    1,
    3
  },
  {
    "sge_df",
    (const PTR) output_269,
    (insn_gen_fn) gen_sge_df,
    &operand_data[507],
    3,
    0,
    1,
    3
  },
  {
    "sunordered_sf",
    (const PTR) output_270,
    (insn_gen_fn) gen_sunordered_sf,
    &operand_data[510],
    3,
    0,
    1,
    3
  },
  {
    "sunlt_sf",
    (const PTR) output_271,
    (insn_gen_fn) gen_sunlt_sf,
    &operand_data[510],
    3,
    0,
    1,
    3
  },
  {
    "suneq_sf",
    (const PTR) output_272,
    (insn_gen_fn) gen_suneq_sf,
    &operand_data[510],
    3,
    0,
    1,
    3
  },
  {
    "sunle_sf",
    (const PTR) output_273,
    (insn_gen_fn) gen_sunle_sf,
    &operand_data[510],
    3,
    0,
    1,
    3
  },
  {
    "seq_sf",
    (const PTR) output_274,
    (insn_gen_fn) gen_seq_sf,
    &operand_data[510],
    3,
    0,
    1,
    3
  },
  {
    "slt_sf",
    (const PTR) output_275,
    (insn_gen_fn) gen_slt_sf,
    &operand_data[510],
    3,
    0,
    1,
    3
  },
  {
    "sle_sf",
    (const PTR) output_276,
    (insn_gen_fn) gen_sle_sf,
    &operand_data[510],
    3,
    0,
    1,
    3
  },
  {
    "sgt_sf",
    (const PTR) output_277,
    (insn_gen_fn) gen_sgt_sf,
    &operand_data[510],
    3,
    0,
    1,
    3
  },
  {
    "sge_sf",
    (const PTR) output_278,
    (insn_gen_fn) gen_sge_sf,
    &operand_data[510],
    3,
    0,
    1,
    3
  },
  {
    "jump",
    (const PTR) output_279,
    (insn_gen_fn) gen_jump,
    &operand_data[446],
    1,
    0,
    0,
    3
  },
  {
    "*mips.md:9634",
    "b\t%l0",
    0,
    &operand_data[446],
    1,
    0,
    0,
    1
  },
  {
    "indirect_jump_internal1",
    "%*j\t%0",
    (insn_gen_fn) gen_indirect_jump_internal1,
    &operand_data[63],
    1,
    0,
    1,
    1
  },
  {
    "indirect_jump_internal2",
    "%*j\t%0",
    (insn_gen_fn) gen_indirect_jump_internal2,
    &operand_data[104],
    1,
    0,
    1,
    1
  },
  {
    "tablejump_internal1",
    "%*j\t%0",
    (insn_gen_fn) gen_tablejump_internal1,
    &operand_data[513],
    2,
    0,
    1,
    1
  },
  {
    "tablejump_internal2",
    "%*j\t%0",
    (insn_gen_fn) gen_tablejump_internal2,
    &operand_data[515],
    2,
    0,
    1,
    1
  },
  {
    "*mips.md:9798",
    (const PTR) output_285,
    0,
    &operand_data[513],
    2,
    1,
    1,
    3
  },
  {
    "*mips.md:9829",
    "%*j\t%0",
    0,
    &operand_data[515],
    2,
    1,
    1,
    1
  },
  {
    "casesi_internal",
    "%(bal\t%S1\n\tsll\t%2,%0,2\n%~%S1:\n\taddu\t%2,%2,$31%)\n\t\
lw\t%2,%1-%S1(%2)\n\taddu\t%2,%2,$31\n\t%*j\t%2",
    (insn_gen_fn) gen_casesi_internal,
    &operand_data[517],
    3,
    0,
    1,
    1
  },
  {
    "casesi_internal_di",
    "%(bal\t%S1\n\tsll\t%2,%0,3\n%~%S1:\n\tdaddu\t%2,%2,$31%)\n\t\
ld\t%2,%1-%S1(%2)\n\tdaddu\t%2,%2,$31\n\t%*j\t%2",
    (insn_gen_fn) gen_casesi_internal_di,
    &operand_data[520],
    3,
    0,
    1,
    1
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
    "return",
    "%*j\t$31",
    (insn_gen_fn) gen_return,
    &operand_data[0],
    0,
    0,
    0,
    1
  },
  {
    "return_internal",
    (const PTR) output_291,
    (insn_gen_fn) gen_return_internal,
    &operand_data[523],
    1,
    0,
    0,
    3
  },
  {
    "get_fnaddr",
    "%($LF%= = . + 8\n\tbal\t$LF%=\n\tnop;la\t%0,%1-$LF%=%)\n\taddu\t%0,%0,$31",
    (insn_gen_fn) gen_get_fnaddr,
    &operand_data[524],
    2,
    0,
    1,
    1
  },
  {
    "eh_set_lr_si",
    "#",
    (insn_gen_fn) gen_eh_set_lr_si,
    &operand_data[230],
    2,
    0,
    1,
    1
  },
  {
    "eh_set_lr_di",
    "#",
    (insn_gen_fn) gen_eh_set_lr_di,
    &operand_data[526],
    2,
    0,
    1,
    1
  },
  {
    "exception_receiver",
    (const PTR) output_295,
    (insn_gen_fn) gen_exception_receiver,
    &operand_data[0],
    0,
    0,
    0,
    3
  },
  {
    "*mips.md:10193",
    "%*jal\t%0",
    0,
    &operand_data[528],
    3,
    0,
    1,
    1
  },
  {
    "call_internal1",
    (const PTR) output_297,
    (insn_gen_fn) gen_call_internal1,
    &operand_data[531],
    3,
    0,
    1,
    3
  },
  {
    "call_internal2",
    (const PTR) output_298,
    (insn_gen_fn) gen_call_internal2,
    &operand_data[531],
    3,
    0,
    1,
    3
  },
  {
    "call_internal3a",
    "%*jal\t%2,%0",
    (insn_gen_fn) gen_call_internal3a,
    &operand_data[534],
    3,
    0,
    1,
    1
  },
  {
    "call_internal3b",
    "%*jal\t%2,%0",
    (insn_gen_fn) gen_call_internal3b,
    &operand_data[537],
    3,
    0,
    1,
    1
  },
  {
    "call_internal3c",
    "%*jal\t%2,%0",
    (insn_gen_fn) gen_call_internal3c,
    &operand_data[540],
    3,
    0,
    1,
    1
  },
  {
    "call_internal4a",
    (const PTR) output_302,
    (insn_gen_fn) gen_call_internal4a,
    &operand_data[534],
    3,
    0,
    1,
    3
  },
  {
    "call_internal4b",
    (const PTR) output_303,
    (insn_gen_fn) gen_call_internal4b,
    &operand_data[537],
    3,
    0,
    1,
    3
  },
  {
    "*mips.md:10398",
    "%*jal\t%1",
    0,
    &operand_data[543],
    4,
    0,
    1,
    1
  },
  {
    "call_value_internal1",
    (const PTR) output_305,
    (insn_gen_fn) gen_call_value_internal1,
    &operand_data[547],
    4,
    0,
    1,
    3
  },
  {
    "call_value_internal2",
    (const PTR) output_306,
    (insn_gen_fn) gen_call_value_internal2,
    &operand_data[547],
    4,
    0,
    1,
    3
  },
  {
    "call_value_internal3a",
    "%*jal\t%3,%1",
    (insn_gen_fn) gen_call_value_internal3a,
    &operand_data[551],
    4,
    0,
    1,
    1
  },
  {
    "call_value_internal3b",
    "%*jal\t%3,%1",
    (insn_gen_fn) gen_call_value_internal3b,
    &operand_data[555],
    4,
    0,
    1,
    1
  },
  {
    "call_value_internal3c",
    "%*jal\t%3,%1",
    (insn_gen_fn) gen_call_value_internal3c,
    &operand_data[559],
    4,
    0,
    1,
    1
  },
  {
    "call_value_internal4a",
    (const PTR) output_310,
    (insn_gen_fn) gen_call_value_internal4a,
    &operand_data[551],
    4,
    0,
    1,
    3
  },
  {
    "call_value_internal4b",
    (const PTR) output_311,
    (insn_gen_fn) gen_call_value_internal4b,
    &operand_data[555],
    4,
    0,
    1,
    3
  },
  {
    "call_value_multiple_internal1",
    (const PTR) output_312,
    (insn_gen_fn) gen_call_value_multiple_internal1,
    &operand_data[563],
    5,
    2,
    1,
    3
  },
  {
    "call_value_multiple_internal2",
    (const PTR) output_313,
    (insn_gen_fn) gen_call_value_multiple_internal2,
    &operand_data[563],
    5,
    2,
    1,
    3
  },
  {
    "prefetch_si_address",
    (const PTR) output_314,
    (insn_gen_fn) gen_prefetch_si_address,
    &operand_data[568],
    4,
    0,
    1,
    3
  },
  {
    "prefetch_si",
    (const PTR) output_315,
    (insn_gen_fn) gen_prefetch_si,
    &operand_data[568],
    3,
    0,
    1,
    3
  },
  {
    "prefetch_di_address",
    (const PTR) output_316,
    (insn_gen_fn) gen_prefetch_di_address,
    &operand_data[572],
    4,
    0,
    1,
    3
  },
  {
    "prefetch_di",
    (const PTR) output_317,
    (insn_gen_fn) gen_prefetch_di,
    &operand_data[572],
    3,
    0,
    1,
    3
  },
  {
    "nop",
    "%(nop%)",
    (insn_gen_fn) gen_nop,
    &operand_data[0],
    0,
    0,
    0,
    1
  },
  {
    "*mips.md:10700",
    (const PTR) output_319,
    0,
    &operand_data[576],
    5,
    0,
    2,
    2
  },
  {
    "*mips.md:10715",
    (const PTR) output_320,
    0,
    &operand_data[581],
    5,
    0,
    2,
    2
  },
  {
    "*mips.md:10730",
    (const PTR) output_321,
    0,
    &operand_data[586],
    5,
    0,
    2,
    2
  },
  {
    "*mips.md:10746",
    (const PTR) output_322,
    0,
    &operand_data[591],
    5,
    0,
    2,
    2
  },
  {
    "*mips.md:10761",
    (const PTR) output_323,
    0,
    &operand_data[596],
    5,
    0,
    2,
    2
  },
  {
    "*mips.md:10776",
    (const PTR) output_324,
    0,
    &operand_data[601],
    5,
    0,
    2,
    2
  },
  {
    "*mips.md:10792",
    (const PTR) output_325,
    0,
    &operand_data[606],
    5,
    0,
    2,
    2
  },
  {
    "*mips.md:10807",
    (const PTR) output_326,
    0,
    &operand_data[611],
    5,
    0,
    2,
    2
  },
  {
    "*mips.md:10822",
    (const PTR) output_327,
    0,
    &operand_data[616],
    5,
    0,
    2,
    2
  },
  {
    "*mips.md:10838",
    (const PTR) output_328,
    0,
    &operand_data[621],
    5,
    0,
    2,
    2
  },
  {
    "*mips.md:10853",
    (const PTR) output_329,
    0,
    &operand_data[626],
    5,
    0,
    2,
    2
  },
  {
    "*mips.md:10868",
    (const PTR) output_330,
    0,
    &operand_data[631],
    5,
    0,
    2,
    2
  },
  {
    "consttable_qi",
    (const PTR) output_331,
    (insn_gen_fn) gen_consttable_qi,
    &operand_data[636],
    1,
    0,
    1,
    3
  },
  {
    "consttable_hi",
    (const PTR) output_332,
    (insn_gen_fn) gen_consttable_hi,
    &operand_data[637],
    1,
    0,
    1,
    3
  },
  {
    "consttable_si",
    (const PTR) output_333,
    (insn_gen_fn) gen_consttable_si,
    &operand_data[638],
    1,
    0,
    1,
    3
  },
  {
    "consttable_di",
    (const PTR) output_334,
    (insn_gen_fn) gen_consttable_di,
    &operand_data[639],
    1,
    0,
    1,
    3
  },
  {
    "consttable_sf",
    (const PTR) output_335,
    (insn_gen_fn) gen_consttable_sf,
    &operand_data[640],
    1,
    0,
    1,
    3
  },
  {
    "consttable_df",
    (const PTR) output_336,
    (insn_gen_fn) gen_consttable_df,
    &operand_data[641],
    1,
    0,
    1,
    3
  },
  {
    "align_2",
    ".align 1",
    (insn_gen_fn) gen_align_2,
    &operand_data[0],
    0,
    0,
    0,
    1
  },
  {
    "align_4",
    ".align 2",
    (insn_gen_fn) gen_align_4,
    &operand_data[0],
    0,
    0,
    0,
    1
  },
  {
    "align_8",
    ".align 3",
    (insn_gen_fn) gen_align_8,
    &operand_data[0],
    0,
    0,
    0,
    1
  },
  {
    "leasi",
    "la %0,%a1",
    (insn_gen_fn) gen_leasi,
    &operand_data[642],
    2,
    0,
    1,
    1
  },
  {
    "leadi",
    "la %0,%a1",
    (insn_gen_fn) gen_leadi,
    &operand_data[644],
    2,
    0,
    1,
    1
  },
  {
    "conditional_trap",
    0,
    (insn_gen_fn) gen_conditional_trap,
    &operand_data[646],
    2,
    2,
    0,
    0
  },
  {
    "addsi3",
    0,
    (insn_gen_fn) gen_addsi3,
    &operand_data[10],
    3,
    0,
    1,
    0
  },
  {
    "addsi3+1",
    0,
    0,
    &operand_data[648],
    2,
    0,
    0,
    0
  },
  {
    "adddi3-1",
    0,
    0,
    &operand_data[650],
    3,
    0,
    0,
    0
  },
  {
    "adddi3",
    0,
    (insn_gen_fn) gen_adddi3,
    &operand_data[653],
    3,
    1,
    0,
    0
  },
  {
    "adddi3+1",
    0,
    0,
    &operand_data[656],
    4,
    0,
    0,
    0
  },
  {
    "adddi3+2",
    0,
    0,
    &operand_data[656],
    4,
    0,
    0,
    0
  },
  {
    "adddi3+3",
    0,
    0,
    &operand_data[660],
    4,
    0,
    0,
    0
  },
  {
    "subsi3-3",
    0,
    0,
    &operand_data[660],
    4,
    0,
    0,
    0
  },
  {
    "subsi3-2",
    0,
    0,
    &operand_data[664],
    2,
    0,
    0,
    0
  },
  {
    "subsi3-1",
    0,
    0,
    &operand_data[666],
    3,
    0,
    0,
    0
  },
  {
    "subsi3",
    0,
    (insn_gen_fn) gen_subsi3,
    &operand_data[10],
    3,
    0,
    1,
    0
  },
  {
    "subsi3+1",
    0,
    0,
    &operand_data[648],
    2,
    0,
    0,
    0
  },
  {
    "subdi3-1",
    0,
    0,
    &operand_data[650],
    3,
    0,
    0,
    0
  },
  {
    "subdi3",
    0,
    (insn_gen_fn) gen_subdi3,
    &operand_data[246],
    3,
    1,
    1,
    0
  },
  {
    "subdi3+1",
    0,
    0,
    &operand_data[656],
    4,
    0,
    0,
    0
  },
  {
    "subdi3+2",
    0,
    0,
    &operand_data[656],
    4,
    0,
    0,
    0
  },
  {
    "subdi3+3",
    0,
    0,
    &operand_data[660],
    4,
    0,
    0,
    0
  },
  {
    "muldf3-3",
    0,
    0,
    &operand_data[660],
    4,
    0,
    0,
    0
  },
  {
    "muldf3-2",
    0,
    0,
    &operand_data[664],
    2,
    0,
    0,
    0
  },
  {
    "muldf3-1",
    0,
    0,
    &operand_data[666],
    3,
    0,
    0,
    0
  },
  {
    "muldf3",
    0,
    (insn_gen_fn) gen_muldf3,
    &operand_data[4],
    3,
    0,
    1,
    0
  },
  {
    "mulsf3",
    0,
    (insn_gen_fn) gen_mulsf3,
    &operand_data[7],
    3,
    0,
    1,
    0
  },
  {
    "mulsi3",
    0,
    (insn_gen_fn) gen_mulsi3,
    &operand_data[62],
    5,
    0,
    1,
    0
  },
  {
    "mulsi3+1",
    0,
    0,
    &operand_data[669],
    8,
    0,
    0,
    0
  },
  {
    "mulsi3+2",
    0,
    0,
    &operand_data[669],
    8,
    0,
    0,
    0
  },
  {
    "mulsi3+3",
    0,
    0,
    &operand_data[669],
    8,
    0,
    0,
    0
  },
  {
    "muldi3-2",
    0,
    0,
    &operand_data[669],
    8,
    0,
    0,
    0
  },
  {
    "muldi3-1",
    0,
    0,
    &operand_data[669],
    8,
    0,
    0,
    0
  },
  {
    "muldi3",
    0,
    (insn_gen_fn) gen_muldi3,
    &operand_data[103],
    5,
    0,
    1,
    0
  },
  {
    "mulsidi3",
    0,
    (insn_gen_fn) gen_mulsidi3,
    &operand_data[114],
    3,
    0,
    1,
    0
  },
  {
    "umulsidi3",
    0,
    (insn_gen_fn) gen_umulsidi3,
    &operand_data[114],
    3,
    0,
    1,
    0
  },
  {
    "smulsi3_highpart",
    0,
    (insn_gen_fn) gen_smulsi3_highpart,
    &operand_data[142],
    3,
    0,
    1,
    0
  },
  {
    "umulsi3_highpart",
    0,
    (insn_gen_fn) gen_umulsi3_highpart,
    &operand_data[142],
    3,
    0,
    1,
    0
  },
  {
    "divmodsi4",
    0,
    (insn_gen_fn) gen_divmodsi4,
    &operand_data[677],
    7,
    2,
    1,
    0
  },
  {
    "divmoddi4",
    0,
    (insn_gen_fn) gen_divmoddi4,
    &operand_data[684],
    7,
    2,
    1,
    0
  },
  {
    "udivmodsi4",
    0,
    (insn_gen_fn) gen_udivmodsi4,
    &operand_data[677],
    7,
    2,
    1,
    0
  },
  {
    "udivmoddi4",
    0,
    (insn_gen_fn) gen_udivmoddi4,
    &operand_data[684],
    7,
    2,
    1,
    0
  },
  {
    "div_trap",
    0,
    (insn_gen_fn) gen_div_trap,
    &operand_data[691],
    3,
    0,
    1,
    0
  },
  {
    "divsi3",
    0,
    (insn_gen_fn) gen_divsi3,
    &operand_data[62],
    5,
    0,
    1,
    0
  },
  {
    "divdi3",
    0,
    (insn_gen_fn) gen_divdi3,
    &operand_data[694],
    5,
    0,
    1,
    0
  },
  {
    "modsi3",
    0,
    (insn_gen_fn) gen_modsi3,
    &operand_data[699],
    5,
    0,
    1,
    0
  },
  {
    "moddi3",
    0,
    (insn_gen_fn) gen_moddi3,
    &operand_data[159],
    5,
    0,
    1,
    0
  },
  {
    "udivsi3",
    0,
    (insn_gen_fn) gen_udivsi3,
    &operand_data[62],
    5,
    0,
    1,
    0
  },
  {
    "udivdi3",
    0,
    (insn_gen_fn) gen_udivdi3,
    &operand_data[704],
    5,
    0,
    1,
    0
  },
  {
    "umodsi3",
    0,
    (insn_gen_fn) gen_umodsi3,
    &operand_data[699],
    5,
    0,
    1,
    0
  },
  {
    "umoddi3",
    0,
    (insn_gen_fn) gen_umoddi3,
    &operand_data[709],
    5,
    0,
    1,
    0
  },
  {
    "negdi2",
    0,
    (insn_gen_fn) gen_negdi2,
    &operand_data[108],
    2,
    1,
    1,
    0
  },
  {
    "negdi2+1",
    0,
    0,
    &operand_data[656],
    2,
    0,
    0,
    0
  },
  {
    "andsi3",
    0,
    (insn_gen_fn) gen_andsi3,
    &operand_data[240],
    3,
    0,
    2,
    0
  },
  {
    "anddi3",
    0,
    (insn_gen_fn) gen_anddi3,
    &operand_data[246],
    3,
    0,
    1,
    0
  },
  {
    "anddi3+1",
    0,
    0,
    &operand_data[656],
    3,
    0,
    0,
    0
  },
  {
    "iorsi3",
    0,
    (insn_gen_fn) gen_iorsi3,
    &operand_data[240],
    3,
    0,
    2,
    0
  },
  {
    "iordi3",
    0,
    (insn_gen_fn) gen_iordi3,
    &operand_data[246],
    3,
    0,
    1,
    0
  },
  {
    "iordi3+1",
    0,
    0,
    &operand_data[656],
    3,
    0,
    0,
    0
  },
  {
    "xorsi3",
    0,
    (insn_gen_fn) gen_xorsi3,
    &operand_data[240],
    3,
    0,
    2,
    0
  },
  {
    "xordi3",
    0,
    (insn_gen_fn) gen_xordi3,
    &operand_data[246],
    3,
    0,
    1,
    0
  },
  {
    "xordi3+1",
    0,
    0,
    &operand_data[656],
    3,
    0,
    0,
    0
  },
  {
    "zero_extendsidi2-1",
    0,
    0,
    &operand_data[656],
    3,
    0,
    0,
    0
  },
  {
    "zero_extendsidi2",
    0,
    (insn_gen_fn) gen_zero_extendsidi2,
    &operand_data[714],
    2,
    0,
    0,
    0
  },
  {
    "zero_extendhisi2",
    0,
    (insn_gen_fn) gen_zero_extendhisi2,
    &operand_data[716],
    2,
    0,
    0,
    0
  },
  {
    "zero_extendhidi2",
    0,
    (insn_gen_fn) gen_zero_extendhidi2,
    &operand_data[718],
    2,
    0,
    0,
    0
  },
  {
    "zero_extendqihi2",
    0,
    (insn_gen_fn) gen_zero_extendqihi2,
    &operand_data[720],
    2,
    0,
    0,
    0
  },
  {
    "zero_extendqisi2",
    0,
    (insn_gen_fn) gen_zero_extendqisi2,
    &operand_data[722],
    2,
    0,
    0,
    0
  },
  {
    "zero_extendqidi2",
    0,
    (insn_gen_fn) gen_zero_extendqidi2,
    &operand_data[724],
    2,
    0,
    0,
    0
  },
  {
    "extendsidi2",
    0,
    (insn_gen_fn) gen_extendsidi2,
    &operand_data[714],
    2,
    0,
    0,
    0
  },
  {
    "extendhidi2",
    0,
    (insn_gen_fn) gen_extendhidi2,
    &operand_data[718],
    2,
    0,
    0,
    0
  },
  {
    "extendhisi2",
    0,
    (insn_gen_fn) gen_extendhisi2,
    &operand_data[716],
    2,
    0,
    0,
    0
  },
  {
    "extendqihi2",
    0,
    (insn_gen_fn) gen_extendqihi2,
    &operand_data[720],
    2,
    0,
    0,
    0
  },
  {
    "extendqisi2",
    0,
    (insn_gen_fn) gen_extendqisi2,
    &operand_data[722],
    2,
    0,
    0,
    0
  },
  {
    "extendqidi2",
    0,
    (insn_gen_fn) gen_extendqidi2,
    &operand_data[724],
    2,
    0,
    0,
    0
  },
  {
    "fix_truncdfsi2",
    0,
    (insn_gen_fn) gen_fix_truncdfsi2,
    &operand_data[299],
    2,
    0,
    1,
    0
  },
  {
    "fix_truncsfsi2",
    0,
    (insn_gen_fn) gen_fix_truncsfsi2,
    &operand_data[302],
    2,
    0,
    1,
    0
  },
  {
    "fixuns_truncdfsi2",
    0,
    (insn_gen_fn) gen_fixuns_truncdfsi2,
    &operand_data[726],
    2,
    0,
    0,
    0
  },
  {
    "fixuns_truncdfdi2",
    0,
    (insn_gen_fn) gen_fixuns_truncdfdi2,
    &operand_data[728],
    2,
    0,
    0,
    0
  },
  {
    "fixuns_truncsfsi2",
    0,
    (insn_gen_fn) gen_fixuns_truncsfsi2,
    &operand_data[730],
    2,
    0,
    0,
    0
  },
  {
    "fixuns_truncsfdi2",
    0,
    (insn_gen_fn) gen_fixuns_truncsfdi2,
    &operand_data[732],
    2,
    0,
    0,
    0
  },
  {
    "extv",
    0,
    (insn_gen_fn) gen_extv,
    &operand_data[734],
    4,
    0,
    0,
    0
  },
  {
    "extzv",
    0,
    (insn_gen_fn) gen_extzv,
    &operand_data[734],
    4,
    0,
    0,
    0
  },
  {
    "insv",
    0,
    (insn_gen_fn) gen_insv,
    &operand_data[735],
    4,
    0,
    0,
    0
  },
  {
    "movdi",
    0,
    (insn_gen_fn) gen_movdi,
    &operand_data[739],
    2,
    0,
    0,
    0
  },
  {
    "movdi+1",
    0,
    0,
    &operand_data[656],
    2,
    0,
    0,
    0
  },
  {
    "reload_indi-1",
    0,
    0,
    &operand_data[664],
    2,
    0,
    0,
    0
  },
  {
    "reload_indi",
    0,
    (insn_gen_fn) gen_reload_indi,
    &operand_data[741],
    3,
    0,
    1,
    0
  },
  {
    "reload_outdi",
    0,
    (insn_gen_fn) gen_reload_outdi,
    &operand_data[744],
    3,
    0,
    1,
    0
  },
  {
    "reload_outdi+1",
    0,
    0,
    &operand_data[747],
    2,
    0,
    0,
    0
  },
  {
    "movsi",
    0,
    (insn_gen_fn) gen_movsi,
    &operand_data[749],
    2,
    0,
    0,
    0
  },
  {
    "movsi+1",
    0,
    0,
    &operand_data[648],
    2,
    0,
    0,
    0
  },
  {
    "movsi+2",
    0,
    0,
    &operand_data[648],
    2,
    0,
    0,
    0
  },
  {
    "reload_outsi-1",
    0,
    0,
    &operand_data[648],
    2,
    0,
    0,
    0
  },
  {
    "reload_outsi",
    0,
    (insn_gen_fn) gen_reload_outsi,
    &operand_data[751],
    3,
    0,
    1,
    0
  },
  {
    "reload_insi",
    0,
    (insn_gen_fn) gen_reload_insi,
    &operand_data[754],
    3,
    0,
    1,
    0
  },
  {
    "reload_incc",
    0,
    (insn_gen_fn) gen_reload_incc,
    &operand_data[757],
    3,
    0,
    1,
    0
  },
  {
    "reload_outcc",
    0,
    (insn_gen_fn) gen_reload_outcc,
    &operand_data[760],
    3,
    0,
    1,
    0
  },
  {
    "movhi",
    0,
    (insn_gen_fn) gen_movhi,
    &operand_data[763],
    2,
    0,
    0,
    0
  },
  {
    "movhi+1",
    0,
    0,
    &operand_data[765],
    2,
    0,
    0,
    0
  },
  {
    "movqi",
    0,
    (insn_gen_fn) gen_movqi,
    &operand_data[767],
    2,
    0,
    0,
    0
  },
  {
    "movqi+1",
    0,
    0,
    &operand_data[769],
    2,
    0,
    0,
    0
  },
  {
    "movsf",
    0,
    (insn_gen_fn) gen_movsf,
    &operand_data[771],
    2,
    0,
    0,
    0
  },
  {
    "movdf",
    0,
    (insn_gen_fn) gen_movdf,
    &operand_data[773],
    2,
    0,
    0,
    0
  },
  {
    "movdf+1",
    0,
    0,
    &operand_data[775],
    2,
    0,
    0,
    0
  },
  {
    "movstrsi",
    0,
    (insn_gen_fn) gen_movstrsi,
    &operand_data[777],
    4,
    0,
    0,
    0
  },
  {
    "ashlsi3",
    0,
    (insn_gen_fn) gen_ashlsi3,
    &operand_data[413],
    3,
    0,
    1,
    0
  },
  {
    "ashlsi3+1",
    0,
    0,
    &operand_data[650],
    3,
    0,
    0,
    0
  },
  {
    "ashldi3",
    0,
    (insn_gen_fn) gen_ashldi3,
    &operand_data[781],
    3,
    1,
    0,
    0
  },
  {
    "ashldi3+1",
    0,
    0,
    &operand_data[784],
    4,
    0,
    0,
    0
  },
  {
    "ashldi3+2",
    0,
    0,
    &operand_data[784],
    4,
    0,
    0,
    0
  },
  {
    "ashldi3+3",
    0,
    0,
    &operand_data[784],
    4,
    0,
    0,
    0
  },
  {
    "ashrsi3-2",
    0,
    0,
    &operand_data[784],
    4,
    0,
    0,
    0
  },
  {
    "ashrsi3-1",
    0,
    0,
    &operand_data[788],
    3,
    0,
    0,
    0
  },
  {
    "ashrsi3",
    0,
    (insn_gen_fn) gen_ashrsi3,
    &operand_data[413],
    3,
    0,
    1,
    0
  },
  {
    "ashrsi3+1",
    0,
    0,
    &operand_data[650],
    3,
    0,
    0,
    0
  },
  {
    "ashrdi3",
    0,
    (insn_gen_fn) gen_ashrdi3,
    &operand_data[781],
    3,
    1,
    0,
    0
  },
  {
    "ashrdi3+1",
    0,
    0,
    &operand_data[784],
    4,
    0,
    0,
    0
  },
  {
    "ashrdi3+2",
    0,
    0,
    &operand_data[784],
    4,
    0,
    0,
    0
  },
  {
    "ashrdi3+3",
    0,
    0,
    &operand_data[784],
    4,
    0,
    0,
    0
  },
  {
    "lshrsi3-2",
    0,
    0,
    &operand_data[784],
    4,
    0,
    0,
    0
  },
  {
    "lshrsi3-1",
    0,
    0,
    &operand_data[788],
    3,
    0,
    0,
    0
  },
  {
    "lshrsi3",
    0,
    (insn_gen_fn) gen_lshrsi3,
    &operand_data[413],
    3,
    0,
    1,
    0
  },
  {
    "lshrsi3+1",
    0,
    0,
    &operand_data[650],
    3,
    0,
    0,
    0
  },
  {
    "lshrdi3-1",
    0,
    0,
    &operand_data[791],
    3,
    0,
    0,
    0
  },
  {
    "lshrdi3",
    0,
    (insn_gen_fn) gen_lshrdi3,
    &operand_data[781],
    3,
    1,
    0,
    0
  },
  {
    "lshrdi3+1",
    0,
    0,
    &operand_data[784],
    4,
    0,
    0,
    0
  },
  {
    "lshrdi3+2",
    0,
    0,
    &operand_data[784],
    4,
    0,
    0,
    0
  },
  {
    "lshrdi3+3",
    0,
    0,
    &operand_data[784],
    4,
    0,
    0,
    0
  },
  {
    "cmpsi-2",
    0,
    0,
    &operand_data[784],
    4,
    0,
    0,
    0
  },
  {
    "cmpsi-1",
    0,
    0,
    &operand_data[788],
    3,
    0,
    0,
    0
  },
  {
    "cmpsi",
    0,
    (insn_gen_fn) gen_cmpsi,
    &operand_data[794],
    2,
    0,
    0,
    0
  },
  {
    "tstsi",
    0,
    (insn_gen_fn) gen_tstsi,
    &operand_data[648],
    1,
    0,
    0,
    0
  },
  {
    "cmpdi",
    0,
    (insn_gen_fn) gen_cmpdi,
    &operand_data[654],
    2,
    0,
    0,
    0
  },
  {
    "tstdi",
    0,
    (insn_gen_fn) gen_tstdi,
    &operand_data[654],
    1,
    0,
    0,
    0
  },
  {
    "cmpdf",
    0,
    (insn_gen_fn) gen_cmpdf,
    &operand_data[775],
    2,
    0,
    0,
    0
  },
  {
    "cmpsf",
    0,
    (insn_gen_fn) gen_cmpsf,
    &operand_data[796],
    2,
    0,
    0,
    0
  },
  {
    "bunordered",
    0,
    (insn_gen_fn) gen_bunordered,
    &operand_data[446],
    1,
    0,
    0,
    0
  },
  {
    "bordered",
    0,
    (insn_gen_fn) gen_bordered,
    &operand_data[446],
    1,
    0,
    0,
    0
  },
  {
    "bunlt",
    0,
    (insn_gen_fn) gen_bunlt,
    &operand_data[446],
    1,
    0,
    0,
    0
  },
  {
    "bunge",
    0,
    (insn_gen_fn) gen_bunge,
    &operand_data[446],
    1,
    0,
    0,
    0
  },
  {
    "buneq",
    0,
    (insn_gen_fn) gen_buneq,
    &operand_data[446],
    1,
    0,
    0,
    0
  },
  {
    "bltgt",
    0,
    (insn_gen_fn) gen_bltgt,
    &operand_data[446],
    1,
    0,
    0,
    0
  },
  {
    "bunle",
    0,
    (insn_gen_fn) gen_bunle,
    &operand_data[446],
    1,
    0,
    0,
    0
  },
  {
    "bungt",
    0,
    (insn_gen_fn) gen_bungt,
    &operand_data[446],
    1,
    0,
    0,
    0
  },
  {
    "beq",
    0,
    (insn_gen_fn) gen_beq,
    &operand_data[446],
    1,
    0,
    0,
    0
  },
  {
    "bne",
    0,
    (insn_gen_fn) gen_bne,
    &operand_data[446],
    1,
    0,
    0,
    0
  },
  {
    "bgt",
    0,
    (insn_gen_fn) gen_bgt,
    &operand_data[446],
    1,
    0,
    0,
    0
  },
  {
    "bge",
    0,
    (insn_gen_fn) gen_bge,
    &operand_data[446],
    1,
    0,
    0,
    0
  },
  {
    "blt",
    0,
    (insn_gen_fn) gen_blt,
    &operand_data[446],
    1,
    0,
    0,
    0
  },
  {
    "ble",
    0,
    (insn_gen_fn) gen_ble,
    &operand_data[446],
    1,
    0,
    0,
    0
  },
  {
    "bgtu",
    0,
    (insn_gen_fn) gen_bgtu,
    &operand_data[446],
    1,
    0,
    0,
    0
  },
  {
    "bgeu",
    0,
    (insn_gen_fn) gen_bgeu,
    &operand_data[446],
    1,
    0,
    0,
    0
  },
  {
    "bltu",
    0,
    (insn_gen_fn) gen_bltu,
    &operand_data[446],
    1,
    0,
    0,
    0
  },
  {
    "bleu",
    0,
    (insn_gen_fn) gen_bleu,
    &operand_data[446],
    1,
    0,
    0,
    0
  },
  {
    "seq",
    0,
    (insn_gen_fn) gen_seq,
    &operand_data[10],
    1,
    2,
    1,
    0
  },
  {
    "sne",
    0,
    (insn_gen_fn) gen_sne,
    &operand_data[10],
    1,
    2,
    1,
    0
  },
  {
    "sgt",
    0,
    (insn_gen_fn) gen_sgt,
    &operand_data[10],
    1,
    2,
    1,
    0
  },
  {
    "sge",
    0,
    (insn_gen_fn) gen_sge,
    &operand_data[10],
    1,
    2,
    1,
    0
  },
  {
    "slt",
    0,
    (insn_gen_fn) gen_slt,
    &operand_data[10],
    1,
    2,
    1,
    0
  },
  {
    "sle",
    0,
    (insn_gen_fn) gen_sle,
    &operand_data[10],
    1,
    2,
    1,
    0
  },
  {
    "sgtu",
    0,
    (insn_gen_fn) gen_sgtu,
    &operand_data[10],
    1,
    2,
    1,
    0
  },
  {
    "sgeu",
    0,
    (insn_gen_fn) gen_sgeu,
    &operand_data[10],
    1,
    2,
    1,
    0
  },
  {
    "sltu",
    0,
    (insn_gen_fn) gen_sltu,
    &operand_data[10],
    1,
    2,
    1,
    0
  },
  {
    "sleu",
    0,
    (insn_gen_fn) gen_sleu,
    &operand_data[10],
    1,
    2,
    1,
    0
  },
  {
    "indirect_jump",
    0,
    (insn_gen_fn) gen_indirect_jump,
    &operand_data[691],
    1,
    0,
    1,
    0
  },
  {
    "tablejump",
    0,
    (insn_gen_fn) gen_tablejump,
    &operand_data[798],
    2,
    0,
    1,
    0
  },
  {
    "tablejump_internal3",
    0,
    (insn_gen_fn) gen_tablejump_internal3,
    &operand_data[513],
    2,
    1,
    1,
    0
  },
  {
    "tablejump_mips161",
    0,
    (insn_gen_fn) gen_tablejump_mips161,
    &operand_data[800],
    2,
    0,
    1,
    0
  },
  {
    "tablejump_mips162",
    0,
    (insn_gen_fn) gen_tablejump_mips162,
    &operand_data[800],
    2,
    0,
    1,
    0
  },
  {
    "tablejump_internal4",
    0,
    (insn_gen_fn) gen_tablejump_internal4,
    &operand_data[515],
    2,
    1,
    1,
    0
  },
  {
    "casesi",
    0,
    (insn_gen_fn) gen_casesi,
    &operand_data[802],
    7,
    3,
    1,
    0
  },
  {
    "builtin_setjmp_setup",
    0,
    (insn_gen_fn) gen_builtin_setjmp_setup,
    &operand_data[809],
    1,
    0,
    1,
    0
  },
  {
    "builtin_setjmp_setup_32",
    0,
    (insn_gen_fn) gen_builtin_setjmp_setup_32,
    &operand_data[328],
    1,
    0,
    1,
    0
  },
  {
    "builtin_setjmp_setup_64",
    0,
    (insn_gen_fn) gen_builtin_setjmp_setup_64,
    &operand_data[810],
    1,
    0,
    1,
    0
  },
  {
    "builtin_longjmp",
    0,
    (insn_gen_fn) gen_builtin_longjmp,
    &operand_data[809],
    1,
    0,
    1,
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
    "eh_return",
    0,
    (insn_gen_fn) gen_eh_return,
    &operand_data[811],
    1,
    0,
    0,
    0
  },
  {
    "eh_return+1",
    0,
    0,
    &operand_data[812],
    2,
    0,
    0,
    0
  },
  {
    "call",
    0,
    (insn_gen_fn) gen_call,
    &operand_data[814],
    4,
    0,
    1,
    0
  },
  {
    "call_internal0",
    0,
    (insn_gen_fn) gen_call_internal0,
    &operand_data[816],
    3,
    0,
    0,
    0
  },
  {
    "call_value",
    0,
    (insn_gen_fn) gen_call_value,
    &operand_data[819],
    4,
    0,
    1,
    0
  },
  {
    "call_value_internal0",
    0,
    (insn_gen_fn) gen_call_value_internal0,
    &operand_data[822],
    4,
    0,
    0,
    0
  },
  {
    "call_value_multiple_internal0",
    0,
    (insn_gen_fn) gen_call_value_multiple_internal0,
    &operand_data[826],
    5,
    2,
    0,
    0
  },
  {
    "untyped_call",
    0,
    (insn_gen_fn) gen_untyped_call,
    &operand_data[822],
    3,
    0,
    0,
    0
  },
  {
    "prefetch",
    0,
    (insn_gen_fn) gen_prefetch,
    &operand_data[831],
    3,
    0,
    0,
    0
  },
  {
    "movsicc",
    0,
    (insn_gen_fn) gen_movsicc,
    &operand_data[834],
    4,
    2,
    0,
    0
  },
  {
    "movdicc",
    0,
    (insn_gen_fn) gen_movdicc,
    &operand_data[838],
    4,
    2,
    0,
    0
  },
  {
    "movsfcc",
    0,
    (insn_gen_fn) gen_movsfcc,
    &operand_data[842],
    4,
    2,
    0,
    0
  },
  {
    "movdfcc",
    0,
    (insn_gen_fn) gen_movdfcc,
    &operand_data[846],
    4,
    2,
    0,
    0
  },
  {
    "movdfcc+1",
    (const PTR) output_529,
    0,
    &operand_data[850],
    5,
    0,
    1,
    3
  },
  {
    "movdfcc+2",
    (const PTR) output_530,
    0,
    &operand_data[855],
    5,
    0,
    1,
    3
  },
  {
    "movdfcc+3",
    (const PTR) output_531,
    0,
    &operand_data[860],
    5,
    0,
    1,
    3
  },
  {
    "movdfcc+4",
    (const PTR) output_532,
    0,
    &operand_data[865],
    5,
    0,
    1,
    3
  },
};


const char *
get_insn_name (code)
     int code;
{
  return insn_data[code].name;
}
