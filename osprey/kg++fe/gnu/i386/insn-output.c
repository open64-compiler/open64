/*
 * Copyright 2003, 2004 PathScale, Inc.  All Rights Reserved.
 */

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
  "test{q}\t{%0, %0|%0, %0}",
  "cmp{q}\t{%1, %0|%0, %1}",
};

static const char * const output_3[] = {
  "test{l}\t{%0, %0|%0, %0}",
  "cmp{l}\t{%1, %0|%0, %1}",
};

static const char * const output_6[] = {
  "test{w}\t{%0, %0|%0, %0}",
  "cmp{w}\t{%1, %0|%0, %1}",
};

static const char * const output_9[] = {
  "test{b}\t{%0, %0|%0, %0}",
  "cmp{b}\t{$0, %0|%0, 0}",
};

static const char *output_18 PARAMS ((rtx *, rtx));

static const char *
output_18 (operands, insn)
     rtx *operands ATTRIBUTE_UNUSED;
     rtx insn ATTRIBUTE_UNUSED;
{
{
  if (find_regno_note (insn, REG_DEAD, REGNO (operands[1])))
    return "ftst\n\tfnstsw\t%0\n\tfstp\t%y0";
  else
    return "ftst\n\tfnstsw\t%0";
}
}

static const char *output_19 PARAMS ((rtx *, rtx));

static const char *
output_19 (operands, insn)
     rtx *operands ATTRIBUTE_UNUSED;
     rtx insn ATTRIBUTE_UNUSED;
{
 return output_fp_compare (insn, operands, 0, 0);
}

static const char *output_20 PARAMS ((rtx *, rtx));

static const char *
output_20 (operands, insn)
     rtx *operands ATTRIBUTE_UNUSED;
     rtx insn ATTRIBUTE_UNUSED;
{
 return output_fp_compare (insn, operands, 2, 0);
}

static const char *output_21 PARAMS ((rtx *, rtx));

static const char *
output_21 (operands, insn)
     rtx *operands ATTRIBUTE_UNUSED;
     rtx insn ATTRIBUTE_UNUSED;
{
 return output_fp_compare (insn, operands, 0, 0);
}

static const char *output_22 PARAMS ((rtx *, rtx));

static const char *
output_22 (operands, insn)
     rtx *operands ATTRIBUTE_UNUSED;
     rtx insn ATTRIBUTE_UNUSED;
{
 return output_fp_compare (insn, operands, 2, 0);
}

static const char *output_23 PARAMS ((rtx *, rtx));

static const char *
output_23 (operands, insn)
     rtx *operands ATTRIBUTE_UNUSED;
     rtx insn ATTRIBUTE_UNUSED;
{
 return output_fp_compare (insn, operands, 0, 0);
}

static const char *output_24 PARAMS ((rtx *, rtx));

static const char *
output_24 (operands, insn)
     rtx *operands ATTRIBUTE_UNUSED;
     rtx insn ATTRIBUTE_UNUSED;
{
 return output_fp_compare (insn, operands, 0, 0);
}

static const char *output_25 PARAMS ((rtx *, rtx));

static const char *
output_25 (operands, insn)
     rtx *operands ATTRIBUTE_UNUSED;
     rtx insn ATTRIBUTE_UNUSED;
{
 return output_fp_compare (insn, operands, 2, 0);
}

static const char *output_26 PARAMS ((rtx *, rtx));

static const char *
output_26 (operands, insn)
     rtx *operands ATTRIBUTE_UNUSED;
     rtx insn ATTRIBUTE_UNUSED;
{
 return output_fp_compare (insn, operands, 2, 0);
}

static const char *output_27 PARAMS ((rtx *, rtx));

static const char *
output_27 (operands, insn)
     rtx *operands ATTRIBUTE_UNUSED;
     rtx insn ATTRIBUTE_UNUSED;
{
 return output_fp_compare (insn, operands, 0, 1);
}

static const char *output_28 PARAMS ((rtx *, rtx));

static const char *
output_28 (operands, insn)
     rtx *operands ATTRIBUTE_UNUSED;
     rtx insn ATTRIBUTE_UNUSED;
{
 return output_fp_compare (insn, operands, 2, 1);
}

static const char *output_31 PARAMS ((rtx *, rtx));

static const char *
output_31 (operands, insn)
     rtx *operands ATTRIBUTE_UNUSED;
     rtx insn ATTRIBUTE_UNUSED;
{
 return output_fp_compare (insn, operands, 1, 0);
}

static const char *output_32 PARAMS ((rtx *, rtx));

static const char *
output_32 (operands, insn)
     rtx *operands ATTRIBUTE_UNUSED;
     rtx insn ATTRIBUTE_UNUSED;
{
 return output_fp_compare (insn, operands, 1, 0);
}

static const char *output_33 PARAMS ((rtx *, rtx));

static const char *
output_33 (operands, insn)
     rtx *operands ATTRIBUTE_UNUSED;
     rtx insn ATTRIBUTE_UNUSED;
{
 return output_fp_compare (insn, operands, 1, 0);
}

static const char *output_34 PARAMS ((rtx *, rtx));

static const char *
output_34 (operands, insn)
     rtx *operands ATTRIBUTE_UNUSED;
     rtx insn ATTRIBUTE_UNUSED;
{
 return output_fp_compare (insn, operands, 1, 1);
}

static const char *output_35 PARAMS ((rtx *, rtx));

static const char *
output_35 (operands, insn)
     rtx *operands ATTRIBUTE_UNUSED;
     rtx insn ATTRIBUTE_UNUSED;
{
 return output_fp_compare (insn, operands, 1, 1);
}

static const char *output_36 PARAMS ((rtx *, rtx));

static const char *
output_36 (operands, insn)
     rtx *operands ATTRIBUTE_UNUSED;
     rtx insn ATTRIBUTE_UNUSED;
{
 return output_fp_compare (insn, operands, 1, 1);
}

static const char *output_43 PARAMS ((rtx *, rtx));

static const char *
output_43 (operands, insn)
     rtx *operands ATTRIBUTE_UNUSED;
     rtx insn ATTRIBUTE_UNUSED;
{
{
  operands[1] = constm1_rtx;
  return "or{l}\t{%1, %0|%0, %1}";
}
}

static const char *output_44 PARAMS ((rtx *, rtx));

static const char *
output_44 (operands, insn)
     rtx *operands ATTRIBUTE_UNUSED;
     rtx insn ATTRIBUTE_UNUSED;
{
{
  switch (get_attr_type (insn))
    {
    case TYPE_SSEMOV:
      if (get_attr_mode (insn) == MODE_TI)
        return "movdqa\t{%1, %0|%0, %1}";
      return "movd\t{%1, %0|%0, %1}";

    case TYPE_MMXMOV:
      if (get_attr_mode (insn) == MODE_DI)
	return "movq\t{%1, %0|%0, %1}";
      return "movd\t{%1, %0|%0, %1}";

    case TYPE_LEA:
      return "lea{l}\t{%1, %0|%0, %1}";

    default:
      if (flag_pic && !LEGITIMATE_PIC_OPERAND_P (operands[1]))
	abort();
      return "mov{l}\t{%1, %0|%0, %1}";
    }
}
}

static const char * const output_45[] = {
  "movabs{l}\t{%1, %P0|%P0, %1}",
  "mov{l}\t{%1, %a0|%a0, %1}",
};

static const char * const output_46[] = {
  "movabs{l}\t{%P1, %0|%0, %P1}",
  "mov{l}\t{%a1, %0|%0, %a1}",
};

static const char * const output_48[] = {
  "push{w}\t{|WORD PTR }%1",
  "push{w}\t%1",
};

static const char *output_50 PARAMS ((rtx *, rtx));

static const char *
output_50 (operands, insn)
     rtx *operands ATTRIBUTE_UNUSED;
     rtx insn ATTRIBUTE_UNUSED;
{
{
  switch (get_attr_type (insn))
    {
    case TYPE_IMOVX:
      /* movzwl is faster than movw on p2 due to partial word stalls,
	 though not as fast as an aligned movl.  */
      return "movz{wl|x}\t{%1, %k0|%k0, %1}";
    default:
      if (get_attr_mode (insn) == MODE_SI)
        return "mov{l}\t{%k1, %k0|%k0, %k1}";
      else
        return "mov{w}\t{%1, %0|%0, %1}";
    }
}
}

static const char * const output_51[] = {
  "movabs{w}\t{%1, %P0|%P0, %1}",
  "mov{w}\t{%1, %a0|%a0, %1}",
};

static const char * const output_52[] = {
  "movabs{w}\t{%P1, %0|%0, %P1}",
  "mov{w}\t{%a1, %0|%0, %a1}",
};

static const char * const output_57[] = {
  "push{w}\t{|word ptr }%1",
  "push{w}\t%w1",
};

static const char *output_59 PARAMS ((rtx *, rtx));

static const char *
output_59 (operands, insn)
     rtx *operands ATTRIBUTE_UNUSED;
     rtx insn ATTRIBUTE_UNUSED;
{
{
  switch (get_attr_type (insn))
    {
    case TYPE_IMOVX:
      if (!ANY_QI_REG_P (operands[1]) && GET_CODE (operands[1]) != MEM)
	abort ();
      return "movz{bl|x}\t{%1, %k0|%k0, %1}";
    default:
      if (get_attr_mode (insn) == MODE_SI)
        return "mov{l}\t{%k1, %k0|%k0, %k1}";
      else
        return "mov{b}\t{%1, %0|%0, %1}";
    }
}
}

static const char *output_65 PARAMS ((rtx *, rtx));

static const char *
output_65 (operands, insn)
     rtx *operands ATTRIBUTE_UNUSED;
     rtx insn ATTRIBUTE_UNUSED;
{
{
  switch (get_attr_type (insn))
    {
    case TYPE_IMOVX:
      return "movs{bl|x}\t{%h1, %k0|%k0, %h1}";
    default:
      return "mov{b}\t{%h1, %0|%0, %h1}";
    }
}
}

static const char *output_66 PARAMS ((rtx *, rtx));

static const char *
output_66 (operands, insn)
     rtx *operands ATTRIBUTE_UNUSED;
     rtx insn ATTRIBUTE_UNUSED;
{
{
  switch (get_attr_type (insn))
    {
    case TYPE_IMOVX:
      return "movs{bl|x}\t{%h1, %k0|%k0, %h1}";
    default:
      return "mov{b}\t{%h1, %0|%0, %h1}";
    }
}
}

static const char * const output_67[] = {
  "movabs{b}\t{%1, %P0|%P0, %1}",
  "mov{b}\t{%1, %a0|%a0, %1}",
};

static const char * const output_68[] = {
  "movabs{b}\t{%P1, %0|%0, %P1}",
  "mov{b}\t{%a1, %0|%0, %a1}",
};

static const char *output_70 PARAMS ((rtx *, rtx));

static const char *
output_70 (operands, insn)
     rtx *operands ATTRIBUTE_UNUSED;
     rtx insn ATTRIBUTE_UNUSED;
{
{
  switch (get_attr_type (insn))
    {
    case TYPE_IMOVX:
      return "movz{bl|x}\t{%h1, %k0|%k0, %h1}";
    default:
      return "mov{b}\t{%h1, %0|%0, %h1}";
    }
}
}

static const char *output_71 PARAMS ((rtx *, rtx));

static const char *
output_71 (operands, insn)
     rtx *operands ATTRIBUTE_UNUSED;
     rtx insn ATTRIBUTE_UNUSED;
{
{
  switch (get_attr_type (insn))
    {
    case TYPE_IMOVX:
      return "movz{bl|x}\t{%h1, %k0|%k0, %h1}";
    default:
      return "mov{b}\t{%h1, %0|%0, %h1}";
    }
}
}

static const char * const output_76[] = {
  "push{q}\t%1",
  "#",
};

static const char *output_81 PARAMS ((rtx *, rtx));

static const char *
output_81 (operands, insn)
     rtx *operands ATTRIBUTE_UNUSED;
     rtx insn ATTRIBUTE_UNUSED;
{
{
  operands[1] = constm1_rtx;
  return "or{q}\t{%1, %0|%0, %1}";
}
}

static const char * const output_82[] = {
  "#",
  "#",
  "movq\t{%1, %0|%0, %1}",
  "movq\t{%1, %0|%0, %1}",
  "movq\t{%1, %0|%0, %1}",
  "movdqa\t{%1, %0|%0, %1}",
  "movq\t{%1, %0|%0, %1}",
};

static const char *output_83 PARAMS ((rtx *, rtx));

static const char *
output_83 (operands, insn)
     rtx *operands ATTRIBUTE_UNUSED;
     rtx insn ATTRIBUTE_UNUSED;
{
{
  switch (get_attr_type (insn))
    {
    case TYPE_SSEMOV:
      if (register_operand (operands[0], DImode)
	  && register_operand (operands[1], DImode))
	  return "movdqa\t{%1, %0|%0, %1}";
      /* FALLTHRU */
    case TYPE_MMXMOV:
      return "movq\t{%1, %0|%0, %1}";
    case TYPE_MULTI:
      return "#";
    case TYPE_LEA:
      return "lea{q}\t{%a1, %0|%0, %a1}";
    default:
      if (flag_pic && !LEGITIMATE_PIC_OPERAND_P (operands[1]))
	abort ();
      if (get_attr_mode (insn) == MODE_SI)
	return "mov{l}\t{%k1, %k0|%k0, %k1}";
      else if (which_alternative == 2)
	return "movabs{q}\t{%1, %0|%0, %1}";
      else
	return "mov{q}\t{%1, %0|%0, %1}";
    }
}
}

static const char * const output_84[] = {
  "movabs{q}\t{%1, %P0|%P0, %1}",
  "mov{q}\t{%1, %a0|%a0, %1}",
};

static const char * const output_85[] = {
  "movabs{q}\t{%P1, %0|%0, %P1}",
  "mov{q}\t{%a1, %0|%0, %a1}",
};

static const char *output_87 PARAMS ((rtx *, rtx));

static const char *
output_87 (operands, insn)
     rtx *operands ATTRIBUTE_UNUSED;
     rtx insn ATTRIBUTE_UNUSED;
{
{
  switch (which_alternative)
    {
    case 0:
      /* %%% We loose REG_DEAD notes for controling pops if we split late.  */
      operands[0] = gen_rtx_MEM (SFmode, stack_pointer_rtx);
      operands[2] = stack_pointer_rtx;
      operands[3] = GEN_INT (4);
      if (find_regno_note (insn, REG_DEAD, REGNO (operands[1])))
	return "sub{l}\t{%3, %2|%2, %3}\n\tfstp%z0\t%y0";
      else
	return "sub{l}\t{%3, %2|%2, %3}\n\tfst%z0\t%y0";

    case 1:
      return "push{l}\t%1";
    case 2:
      return "#";

    default:
      abort ();
    }
}
}

static const char *output_88 PARAMS ((rtx *, rtx));

static const char *
output_88 (operands, insn)
     rtx *operands ATTRIBUTE_UNUSED;
     rtx insn ATTRIBUTE_UNUSED;
{
{
  switch (which_alternative)
    {
    case 0:
      /* %%% We loose REG_DEAD notes for controling pops if we split late.  */
      operands[0] = gen_rtx_MEM (SFmode, stack_pointer_rtx);
      operands[2] = stack_pointer_rtx;
      operands[3] = GEN_INT (8);
      if (find_regno_note (insn, REG_DEAD, REGNO (operands[1])))
	return "sub{q}\t{%3, %2|%2, %3}\n\tfstp%z0\t%y0";
      else
	return "sub{q}\t{%3, %2|%2, %3}\n\tfst%z0\t%y0";

    case 1:
      return "push{q}\t%q1";

    case 2:
      return "#";

    default:
      abort ();
    }
}
}

static const char *output_89 PARAMS ((rtx *, rtx));

static const char *
output_89 (operands, insn)
     rtx *operands ATTRIBUTE_UNUSED;
     rtx insn ATTRIBUTE_UNUSED;
{
{
  switch (which_alternative)
    {
    case 0:
      if (REG_P (operands[1])
          && find_regno_note (insn, REG_DEAD, REGNO (operands[1])))
        return "fstp\t%y0";
      else if (STACK_TOP_P (operands[0]))
        return "fld%z1\t%y1";
      else
        return "fst\t%y0";

    case 1:
      if (find_regno_note (insn, REG_DEAD, REGNO (operands[1])))
        return "fstp%z0\t%y0";
      else
        return "fst%z0\t%y0";

    case 2:
      switch (standard_80387_constant_p (operands[1]))
        {
        case 1:
	  return "fldz";
	case 2:
	  return "fld1";
	}
      abort();

    case 3:
    case 4:
      return "mov{l}\t{%1, %0|%0, %1}";
    case 5:
      if (TARGET_SSE2 && !TARGET_ATHLON)
	return "pxor\t%0, %0";
      else
	return "xorps\t%0, %0";
    case 6:
      if (TARGET_PARTIAL_REG_DEPENDENCY)
	return "movaps\t{%1, %0|%0, %1}";
      else
	return "movss\t{%1, %0|%0, %1}";
    case 7:
    case 8:
      return "movss\t{%1, %0|%0, %1}";

    case 9:
    case 10:
      return "movd\t{%1, %0|%0, %1}";

    case 11:
      return "movq\t{%1, %0|%0, %1}";

    default:
      abort();
    }
}
}

static const char *output_90 PARAMS ((rtx *, rtx));

static const char *
output_90 (operands, insn)
     rtx *operands ATTRIBUTE_UNUSED;
     rtx insn ATTRIBUTE_UNUSED;
{
{
  if (STACK_TOP_P (operands[0]))
    return "fxch\t%1";
  else
    return "fxch\t%0";
}
}

static const char *output_91 PARAMS ((rtx *, rtx));

static const char *
output_91 (operands, insn)
     rtx *operands ATTRIBUTE_UNUSED;
     rtx insn ATTRIBUTE_UNUSED;
{
{
  switch (which_alternative)
    {
    case 0:
      /* %%% We loose REG_DEAD notes for controling pops if we split late.  */
      operands[0] = gen_rtx_MEM (DFmode, stack_pointer_rtx);
      operands[2] = stack_pointer_rtx;
      operands[3] = GEN_INT (8);
      if (find_regno_note (insn, REG_DEAD, REGNO (operands[1])))
	return "sub{l}\t{%3, %2|%2, %3}\n\tfstp%z0\t%y0";
      else
	return "sub{l}\t{%3, %2|%2, %3}\n\tfst%z0\t%y0";

    case 1:
    case 2:
    case 3:
      return "#";

    default:
      abort ();
    }
}
}

static const char *output_92 PARAMS ((rtx *, rtx));

static const char *
output_92 (operands, insn)
     rtx *operands ATTRIBUTE_UNUSED;
     rtx insn ATTRIBUTE_UNUSED;
{
{
  switch (which_alternative)
    {
    case 0:
      /* %%% We loose REG_DEAD notes for controling pops if we split late.  */
      operands[0] = gen_rtx_MEM (DFmode, stack_pointer_rtx);
      operands[2] = stack_pointer_rtx;
      operands[3] = GEN_INT (8);
      if (TARGET_64BIT)
	if (find_regno_note (insn, REG_DEAD, REGNO (operands[1])))
	  return "sub{q}\t{%3, %2|%2, %3}\n\tfstp%z0\t%y0";
	else
	  return "sub{q}\t{%3, %2|%2, %3}\n\tfst%z0\t%y0";
      else
	if (find_regno_note (insn, REG_DEAD, REGNO (operands[1])))
	  return "sub{l}\t{%3, %2|%2, %3}\n\tfstp%z0\t%y0";
	else
	  return "sub{l}\t{%3, %2|%2, %3}\n\tfst%z0\t%y0";


    case 1:
    case 2:
      return "#";

    default:
      abort ();
    }
}
}

static const char *output_93 PARAMS ((rtx *, rtx));

static const char *
output_93 (operands, insn)
     rtx *operands ATTRIBUTE_UNUSED;
     rtx insn ATTRIBUTE_UNUSED;
{
{
  switch (which_alternative)
    {
    case 0:
      if (REG_P (operands[1])
          && find_regno_note (insn, REG_DEAD, REGNO (operands[1])))
        return "fstp\t%y0";
      else if (STACK_TOP_P (operands[0]))
        return "fld%z1\t%y1";
      else
        return "fst\t%y0";

    case 1:
      if (find_regno_note (insn, REG_DEAD, REGNO (operands[1])))
        return "fstp%z0\t%y0";
      else
        return "fst%z0\t%y0";

    case 2:
      switch (standard_80387_constant_p (operands[1]))
        {
        case 1:
	  return "fldz";
	case 2:
	  return "fld1";
	}
      abort();

    case 3:
    case 4:
      return "#";
    case 5:
      if (TARGET_ATHLON)
        return "xorpd\t%0, %0";
      else
        return "pxor\t%0, %0";
    case 6:
      if (TARGET_PARTIAL_REG_DEPENDENCY)
	return "movapd\t{%1, %0|%0, %1}";
      else
	return "movsd\t{%1, %0|%0, %1}";
    case 7:
    case 8:
        return "movsd\t{%1, %0|%0, %1}";

    default:
      abort();
    }
}
}

static const char *output_94 PARAMS ((rtx *, rtx));

static const char *
output_94 (operands, insn)
     rtx *operands ATTRIBUTE_UNUSED;
     rtx insn ATTRIBUTE_UNUSED;
{
{
  switch (which_alternative)
    {
    case 0:
      if (REG_P (operands[1])
          && find_regno_note (insn, REG_DEAD, REGNO (operands[1])))
        return "fstp\t%y0";
      else if (STACK_TOP_P (operands[0]))
        return "fld%z1\t%y1";
      else
        return "fst\t%y0";

    case 1:
      if (find_regno_note (insn, REG_DEAD, REGNO (operands[1])))
        return "fstp%z0\t%y0";
      else
        return "fst%z0\t%y0";

    case 2:
      switch (standard_80387_constant_p (operands[1]))
        {
        case 1:
	  return "fldz";
	case 2:
	  return "fld1";
	}
      abort();

    case 3:
    case 4:
      return "#";

    case 5:
      if (TARGET_ATHLON)
        return "xorpd\t%0, %0";
      else
        return "pxor\t%0, %0";
    case 6:
      if (TARGET_PARTIAL_REG_DEPENDENCY)
	return "movapd\t{%1, %0|%0, %1}";
      else
	return "movsd\t{%1, %0|%0, %1}";
    case 7:
    case 8:
      return "movsd\t{%1, %0|%0, %1}";

    default:
      abort();
    }
}
}

static const char *output_95 PARAMS ((rtx *, rtx));

static const char *
output_95 (operands, insn)
     rtx *operands ATTRIBUTE_UNUSED;
     rtx insn ATTRIBUTE_UNUSED;
{
{
  if (STACK_TOP_P (operands[0]))
    return "fxch\t%1";
  else
    return "fxch\t%0";
}
}

static const char *output_96 PARAMS ((rtx *, rtx));

static const char *
output_96 (operands, insn)
     rtx *operands ATTRIBUTE_UNUSED;
     rtx insn ATTRIBUTE_UNUSED;
{
{
  switch (which_alternative)
    {
    case 0:
      /* %%% We loose REG_DEAD notes for controling pops if we split late.  */
      operands[0] = gen_rtx_MEM (XFmode, stack_pointer_rtx);
      operands[2] = stack_pointer_rtx;
      operands[3] = GEN_INT (12);
      if (find_regno_note (insn, REG_DEAD, REGNO (operands[1])))
	return "sub{l}\t{%3, %2|%2, %3}\n\tfstp%z0\t%y0";
      else
	return "sub{l}\t{%3, %2|%2, %3}\n\tfst%z0\t%y0";

    case 1:
    case 2:
      return "#";

    default:
      abort ();
    }
}
}

static const char *output_97 PARAMS ((rtx *, rtx));

static const char *
output_97 (operands, insn)
     rtx *operands ATTRIBUTE_UNUSED;
     rtx insn ATTRIBUTE_UNUSED;
{
{
  switch (which_alternative)
    {
    case 0:
      /* %%% We loose REG_DEAD notes for controling pops if we split late.  */
      operands[0] = gen_rtx_MEM (XFmode, stack_pointer_rtx);
      operands[2] = stack_pointer_rtx;
      operands[3] = GEN_INT (16);
      if (find_regno_note (insn, REG_DEAD, REGNO (operands[1])))
	return "sub{l}\t{%3, %2|%2, %3}\n\tfstp%z0\t%y0";
      else
	return "sub{l}\t{%3, %2|%2, %3}\n\tfst%z0\t%y0";

    case 1:
    case 2:
      return "#";

    default:
      abort ();
    }
}
}

static const char *output_98 PARAMS ((rtx *, rtx));

static const char *
output_98 (operands, insn)
     rtx *operands ATTRIBUTE_UNUSED;
     rtx insn ATTRIBUTE_UNUSED;
{
{
  switch (which_alternative)
    {
    case 0:
      /* %%% We loose REG_DEAD notes for controling pops if we split late.  */
      operands[0] = gen_rtx_MEM (XFmode, stack_pointer_rtx);
      operands[2] = stack_pointer_rtx;
      operands[3] = GEN_INT (12);
      if (find_regno_note (insn, REG_DEAD, REGNO (operands[1])))
	return "sub{l}\t{%3, %2|%2, %3}\n\tfstp%z0\t%y0";
      else
	return "sub{l}\t{%3, %2|%2, %3}\n\tfst%z0\t%y0";

    case 1:
      return "#";

    default:
      abort ();
    }
}
}

static const char *output_99 PARAMS ((rtx *, rtx));

static const char *
output_99 (operands, insn)
     rtx *operands ATTRIBUTE_UNUSED;
     rtx insn ATTRIBUTE_UNUSED;
{
{
  switch (which_alternative)
    {
    case 0:
      /* %%% We loose REG_DEAD notes for controling pops if we split late.  */
      operands[0] = gen_rtx_MEM (XFmode, stack_pointer_rtx);
      operands[2] = stack_pointer_rtx;
      operands[3] = GEN_INT (16);
      if (TARGET_64BIT)
	if (find_regno_note (insn, REG_DEAD, REGNO (operands[1])))
	  return "sub{q}\t{%3, %2|%2, %3}\n\tfstp%z0\t%y0";
	else
	  return "sub{q}\t{%3, %2|%2, %3}\n\tfst%z0\t%y0";
      else
	if (find_regno_note (insn, REG_DEAD, REGNO (operands[1])))
	  return "sub{l}\t{%3, %2|%2, %3}\n\tfstp%z0\t%y0";
	else
	  return "sub{l}\t{%3, %2|%2, %3}\n\tfst%z0\t%y0";

    case 1:
      return "#";

    default:
      abort ();
    }
}
}

static const char *output_100 PARAMS ((rtx *, rtx));

static const char *
output_100 (operands, insn)
     rtx *operands ATTRIBUTE_UNUSED;
     rtx insn ATTRIBUTE_UNUSED;
{
{
  switch (which_alternative)
    {
    case 0:
      if (REG_P (operands[1])
          && find_regno_note (insn, REG_DEAD, REGNO (operands[1])))
        return "fstp\t%y0";
      else if (STACK_TOP_P (operands[0]))
        return "fld%z1\t%y1";
      else
        return "fst\t%y0";

    case 1:
      /* There is no non-popping store to memory for XFmode.  So if
	 we need one, follow the store with a load.  */
      if (! find_regno_note (insn, REG_DEAD, REGNO (operands[1])))
        return "fstp%z0\t%y0\n\tfld%z0\t%y0";
      else
        return "fstp%z0\t%y0";

    case 2:
      switch (standard_80387_constant_p (operands[1]))
        {
        case 1:
	  return "fldz";
	case 2:
	  return "fld1";
	}
      break;

    case 3: case 4:
      return "#";
    }
  abort();
}
}

static const char *output_101 PARAMS ((rtx *, rtx));

static const char *
output_101 (operands, insn)
     rtx *operands ATTRIBUTE_UNUSED;
     rtx insn ATTRIBUTE_UNUSED;
{
{
  switch (which_alternative)
    {
    case 0:
      if (REG_P (operands[1])
          && find_regno_note (insn, REG_DEAD, REGNO (operands[1])))
        return "fstp\t%y0";
      else if (STACK_TOP_P (operands[0]))
        return "fld%z1\t%y1";
      else
        return "fst\t%y0";

    case 1:
      /* There is no non-popping store to memory for XFmode.  So if
	 we need one, follow the store with a load.  */
      if (! find_regno_note (insn, REG_DEAD, REGNO (operands[1])))
        return "fstp%z0\t%y0\n\tfld%z0\t%y0";
      else
        return "fstp%z0\t%y0";

    case 2:
      switch (standard_80387_constant_p (operands[1]))
        {
        case 1:
	  return "fldz";
	case 2:
	  return "fld1";
	}
      break;

    case 3: case 4:
      return "#";
    }
  abort();
}
}

static const char *output_102 PARAMS ((rtx *, rtx));

static const char *
output_102 (operands, insn)
     rtx *operands ATTRIBUTE_UNUSED;
     rtx insn ATTRIBUTE_UNUSED;
{
{
  switch (which_alternative)
    {
    case 0:
      if (REG_P (operands[1])
          && find_regno_note (insn, REG_DEAD, REGNO (operands[1])))
        return "fstp\t%y0";
      else if (STACK_TOP_P (operands[0]))
        return "fld%z1\t%y1";
      else
        return "fst\t%y0";

    case 1:
      /* There is no non-popping store to memory for XFmode.  So if
	 we need one, follow the store with a load.  */
      if (! find_regno_note (insn, REG_DEAD, REGNO (operands[1])))
        return "fstp%z0\t%y0\n\tfld%z0\t%y0";
      else
        return "fstp%z0\t%y0";

    case 2:
      switch (standard_80387_constant_p (operands[1]))
        {
        case 1:
	  return "fldz";
	case 2:
	  return "fld1";
	}
      break;

    case 3: case 4:
      return "#";
    }
  abort();
}
}

static const char *output_103 PARAMS ((rtx *, rtx));

static const char *
output_103 (operands, insn)
     rtx *operands ATTRIBUTE_UNUSED;
     rtx insn ATTRIBUTE_UNUSED;
{
{
  switch (which_alternative)
    {
    case 0:
      if (REG_P (operands[1])
          && find_regno_note (insn, REG_DEAD, REGNO (operands[1])))
        return "fstp\t%y0";
      else if (STACK_TOP_P (operands[0]))
        return "fld%z1\t%y1";
      else
        return "fst\t%y0";

    case 1:
      /* There is no non-popping store to memory for XFmode.  So if
	 we need one, follow the store with a load.  */
      if (! find_regno_note (insn, REG_DEAD, REGNO (operands[1])))
        return "fstp%z0\t%y0\n\tfld%z0\t%y0";
      else
        return "fstp%z0\t%y0";

    case 2:
      switch (standard_80387_constant_p (operands[1]))
        {
        case 1:
	  return "fldz";
	case 2:
	  return "fld1";
	}
      break;

    case 3: case 4:
      return "#";
    }
  abort();
}
}

static const char *output_104 PARAMS ((rtx *, rtx));

static const char *
output_104 (operands, insn)
     rtx *operands ATTRIBUTE_UNUSED;
     rtx insn ATTRIBUTE_UNUSED;
{
{
  if (STACK_TOP_P (operands[0]))
    return "fxch\t%1";
  else
    return "fxch\t%0";
}
}

static const char *output_105 PARAMS ((rtx *, rtx));

static const char *
output_105 (operands, insn)
     rtx *operands ATTRIBUTE_UNUSED;
     rtx insn ATTRIBUTE_UNUSED;
{
{
  if (STACK_TOP_P (operands[0]))
    return "fxch\t%1";
  else
    return "fxch\t%0";
}
}

static const char * const output_115[] = {
  "mov\t{%k1, %k0|%k0, %k1}",
  "#",
};

static const char * const output_116[] = {
  "movz{wl|x}\t{%1, %k0|%k0, %1} ",
  "movz{wq|x}\t{%1, %0|%0, %1}",
};

static const char * const output_117[] = {
  "movz{bl|x}\t{%1, %k0|%k0, %1} ",
  "movz{bq|x}\t{%1, %0|%0, %1}",
};

static const char * const output_119[] = {
  "{cltq|cdqe}",
  "movs{lq|x}\t{%1,%0|%0, %1}",
};

static const char *output_122 PARAMS ((rtx *, rtx));

static const char *
output_122 (operands, insn)
     rtx *operands ATTRIBUTE_UNUSED;
     rtx insn ATTRIBUTE_UNUSED;
{
{
  switch (get_attr_prefix_0f (insn))
    {
    case 0:
      return "{cwtl|cwde}";
    default:
      return "movs{wl|x}\t{%1,%0|%0, %1}";
    }
}
}

static const char *output_123 PARAMS ((rtx *, rtx));

static const char *
output_123 (operands, insn)
     rtx *operands ATTRIBUTE_UNUSED;
     rtx insn ATTRIBUTE_UNUSED;
{
{
  switch (get_attr_prefix_0f (insn))
    {
    case 0:
      return "{cwtl|cwde}";
    default:
      return "movs{wl|x}\t{%1,%k0|%k0, %1}";
    }
}
}

static const char *output_124 PARAMS ((rtx *, rtx));

static const char *
output_124 (operands, insn)
     rtx *operands ATTRIBUTE_UNUSED;
     rtx insn ATTRIBUTE_UNUSED;
{
{
  switch (get_attr_prefix_0f (insn))
    {
    case 0:
      return "{cbtw|cbw}";
    default:
      return "movs{bw|x}\t{%1,%0|%0, %1}";
    }
}
}

static const char *output_127 PARAMS ((rtx *, rtx));

static const char *
output_127 (operands, insn)
     rtx *operands ATTRIBUTE_UNUSED;
     rtx insn ATTRIBUTE_UNUSED;
{
{
  switch (which_alternative)
    {
    case 0:
      if (REG_P (operands[1])
          && find_regno_note (insn, REG_DEAD, REGNO (operands[1])))
        return "fstp\t%y0";
      else if (STACK_TOP_P (operands[0]))
        return "fld%z1\t%y1";
      else
        return "fst\t%y0";

    case 1:
      if (find_regno_note (insn, REG_DEAD, REGNO (operands[1])))
        return "fstp%z0\t%y0";

      else
        return "fst%z0\t%y0";
    case 2:
      return "cvtss2sd\t{%1, %0|%0, %1}";

    default:
      abort ();
    }
}
}

static const char *output_129 PARAMS ((rtx *, rtx));

static const char *
output_129 (operands, insn)
     rtx *operands ATTRIBUTE_UNUSED;
     rtx insn ATTRIBUTE_UNUSED;
{
{
  switch (which_alternative)
    {
    case 0:
      if (REG_P (operands[1])
          && find_regno_note (insn, REG_DEAD, REGNO (operands[1])))
        return "fstp\t%y0";
      else if (STACK_TOP_P (operands[0]))
        return "fld%z1\t%y1";
      else
        return "fst\t%y0";

    case 1:
      /* There is no non-popping store to memory for XFmode.  So if
	 we need one, follow the store with a load.  */
      if (! find_regno_note (insn, REG_DEAD, REGNO (operands[1])))
        return "fstp%z0\t%y0\n\tfld%z0\t%y0";
      else
        return "fstp%z0\t%y0";

    default:
      abort ();
    }
}
}

static const char *output_130 PARAMS ((rtx *, rtx));

static const char *
output_130 (operands, insn)
     rtx *operands ATTRIBUTE_UNUSED;
     rtx insn ATTRIBUTE_UNUSED;
{
{
  switch (which_alternative)
    {
    case 0:
      if (REG_P (operands[1])
          && find_regno_note (insn, REG_DEAD, REGNO (operands[1])))
        return "fstp\t%y0";
      else if (STACK_TOP_P (operands[0]))
        return "fld%z1\t%y1";
      else
        return "fst\t%y0";

    case 1:
      /* There is no non-popping store to memory for XFmode.  So if
	 we need one, follow the store with a load.  */
      if (! find_regno_note (insn, REG_DEAD, REGNO (operands[1])))
        return "fstp%z0\t%y0\n\tfld%z0\t%y0";
      else
        return "fstp%z0\t%y0";

    default:
      abort ();
    }
}
}

static const char *output_131 PARAMS ((rtx *, rtx));

static const char *
output_131 (operands, insn)
     rtx *operands ATTRIBUTE_UNUSED;
     rtx insn ATTRIBUTE_UNUSED;
{
{
  switch (which_alternative)
    {
    case 0:
      if (REG_P (operands[1])
          && find_regno_note (insn, REG_DEAD, REGNO (operands[1])))
        return "fstp\t%y0";
      else if (STACK_TOP_P (operands[0]))
        return "fld%z1\t%y1";
      else
        return "fst\t%y0";

    case 1:
      /* There is no non-popping store to memory for XFmode.  So if
	 we need one, follow the store with a load.  */
      if (! find_regno_note (insn, REG_DEAD, REGNO (operands[1])))
        return "fstp%z0\t%y0\n\tfld%z0\t%y0";
      else
        return "fstp%z0\t%y0";

    default:
      abort ();
    }
}
}

static const char *output_132 PARAMS ((rtx *, rtx));

static const char *
output_132 (operands, insn)
     rtx *operands ATTRIBUTE_UNUSED;
     rtx insn ATTRIBUTE_UNUSED;
{
{
  switch (which_alternative)
    {
    case 0:
      if (REG_P (operands[1])
          && find_regno_note (insn, REG_DEAD, REGNO (operands[1])))
        return "fstp\t%y0";
      else if (STACK_TOP_P (operands[0]))
        return "fld%z1\t%y1";
      else
        return "fst\t%y0";

    case 1:
      /* There is no non-popping store to memory for XFmode.  So if
	 we need one, follow the store with a load.  */
      if (! find_regno_note (insn, REG_DEAD, REGNO (operands[1])))
        return "fstp%z0\t%y0\n\tfld%z0\t%y0";
      else
        return "fstp%z0\t%y0";

    default:
      abort ();
    }
}
}

static const char *output_133 PARAMS ((rtx *, rtx));

static const char *
output_133 (operands, insn)
     rtx *operands ATTRIBUTE_UNUSED;
     rtx insn ATTRIBUTE_UNUSED;
{
{
  switch (which_alternative)
    {
    case 0:
      if (find_regno_note (insn, REG_DEAD, REGNO (operands[1])))
	return "fstp%z0\t%y0";
      else
	return "fst%z0\t%y0";
    default:
      abort ();
    }
}
}

static const char *output_134 PARAMS ((rtx *, rtx));

static const char *
output_134 (operands, insn)
     rtx *operands ATTRIBUTE_UNUSED;
     rtx insn ATTRIBUTE_UNUSED;
{
{
  switch (which_alternative)
    {
    case 0:
      if (find_regno_note (insn, REG_DEAD, REGNO (operands[1])))
	return "fstp%z0\t%y0";
      else
	return "fst%z0\t%y0";
    case 4:
      return "cvtsd2ss\t{%1, %0|%0, %1}";
    default:
      abort ();
    }
}
}

static const char *output_135 PARAMS ((rtx *, rtx));

static const char *
output_135 (operands, insn)
     rtx *operands ATTRIBUTE_UNUSED;
     rtx insn ATTRIBUTE_UNUSED;
{
{
  switch (which_alternative)
    {
    case 0:
      return "cvtsd2ss\t{%1, %0|%0, %1}";
    case 1:
      if (find_regno_note (insn, REG_DEAD, REGNO (operands[1])))
	return "fstp%z0\t%y0";
      else
	return "fst%z0\t%y0";
    default:
      abort ();
    }
}
}

static const char *output_136 PARAMS ((rtx *, rtx));

static const char *
output_136 (operands, insn)
     rtx *operands ATTRIBUTE_UNUSED;
     rtx insn ATTRIBUTE_UNUSED;
{
{
  if (find_regno_note (insn, REG_DEAD, REGNO (operands[1])))
    return "fstp%z0\t%y0";
  else
    return "fst%z0\t%y0";
}
}

static const char *output_138 PARAMS ((rtx *, rtx));

static const char *
output_138 (operands, insn)
     rtx *operands ATTRIBUTE_UNUSED;
     rtx insn ATTRIBUTE_UNUSED;
{
{
  switch (which_alternative)
    {
    case 0:
      if (find_regno_note (insn, REG_DEAD, REGNO (operands[1])))
	return "fstp%z0\t%y0";
      else
	return "fst%z0\t%y0";
    default:
      abort();
    }
}
}

static const char *output_139 PARAMS ((rtx *, rtx));

static const char *
output_139 (operands, insn)
     rtx *operands ATTRIBUTE_UNUSED;
     rtx insn ATTRIBUTE_UNUSED;
{
{
  if (find_regno_note (insn, REG_DEAD, REGNO (operands[1])))
    return "fstp%z0\t%y0";
  else
    return "fst%z0\t%y0";
}
}

static const char *output_140 PARAMS ((rtx *, rtx));

static const char *
output_140 (operands, insn)
     rtx *operands ATTRIBUTE_UNUSED;
     rtx insn ATTRIBUTE_UNUSED;
{
{
  switch (which_alternative)
    {
    case 0:
      if (find_regno_note (insn, REG_DEAD, REGNO (operands[1])))
	return "fstp%z0\t%y0";
      else
	return "fst%z0\t%y0";
    default:
      abort();
    }
}
}

static const char *output_141 PARAMS ((rtx *, rtx));

static const char *
output_141 (operands, insn)
     rtx *operands ATTRIBUTE_UNUSED;
     rtx insn ATTRIBUTE_UNUSED;
{
{
  if (find_regno_note (insn, REG_DEAD, REGNO (operands[1])))
    return "fstp%z0\t%y0";
  else
    return "fst%z0\t%y0";
}
}

static const char *output_142 PARAMS ((rtx *, rtx));

static const char *
output_142 (operands, insn)
     rtx *operands ATTRIBUTE_UNUSED;
     rtx insn ATTRIBUTE_UNUSED;
{
{
  switch (which_alternative)
    {
    case 0:
      if (find_regno_note (insn, REG_DEAD, REGNO (operands[1])))
	return "fstp%z0\t%y0";
      else
	return "fst%z0\t%y0";
    default:
      abort();
    }
  abort ();
}
}

static const char *output_143 PARAMS ((rtx *, rtx));

static const char *
output_143 (operands, insn)
     rtx *operands ATTRIBUTE_UNUSED;
     rtx insn ATTRIBUTE_UNUSED;
{
{
  if (find_regno_note (insn, REG_DEAD, REGNO (operands[1])))
    return "fstp%z0\t%y0";
  else
    return "fst%z0\t%y0";
}
}

static const char *output_144 PARAMS ((rtx *, rtx));

static const char *
output_144 (operands, insn)
     rtx *operands ATTRIBUTE_UNUSED;
     rtx insn ATTRIBUTE_UNUSED;
{
{
  switch (which_alternative)
    {
    case 0:
      if (find_regno_note (insn, REG_DEAD, REGNO (operands[1])))
	return "fstp%z0\t%y0";
      else
	return "fst%z0\t%y0";
    default:
      abort();
    }
  abort ();
}
}

static const char *output_145 PARAMS ((rtx *, rtx));

static const char *
output_145 (operands, insn)
     rtx *operands ATTRIBUTE_UNUSED;
     rtx insn ATTRIBUTE_UNUSED;
{
{
  if (find_regno_note (insn, REG_DEAD, REGNO (operands[1])))
    return "fstp%z0\t%y0";
  else
    return "fst%z0\t%y0";
}
}

static const char *output_148 PARAMS ((rtx *, rtx));

static const char *
output_148 (operands, insn)
     rtx *operands ATTRIBUTE_UNUSED;
     rtx insn ATTRIBUTE_UNUSED;
{
 operands[5] = operands[4]; return output_fix_trunc (insn, operands);
}

static const char *output_153 PARAMS ((rtx *, rtx));

static const char *
output_153 (operands, insn)
     rtx *operands ATTRIBUTE_UNUSED;
     rtx insn ATTRIBUTE_UNUSED;
{
 return output_fix_trunc (insn, operands);
}

static const char *output_158 PARAMS ((rtx *, rtx));

static const char *
output_158 (operands, insn)
     rtx *operands ATTRIBUTE_UNUSED;
     rtx insn ATTRIBUTE_UNUSED;
{
 return output_fix_trunc (insn, operands);
}

static const char * const output_161[] = {
  "fild%z1\t%1",
  "#",
};

static const char * const output_162[] = {
  "fild%z1\t%1",
  "#",
  "cvtsi2ss\t{%1, %0|%0, %1}",
};

static const char * const output_164[] = {
  "fild%z1\t%1",
  "#",
};

static const char * const output_165[] = {
  "fild%z1\t%1",
  "#",
  "cvtsi2ss{q}\t{%1, %0|%0, %1}",
};

static const char * const output_167[] = {
  "fild%z1\t%1",
  "#",
};

static const char * const output_168[] = {
  "fild%z1\t%1",
  "#",
  "cvtsi2sd\t{%1, %0|%0, %1}",
};

static const char * const output_170[] = {
  "fild%z1\t%1",
  "#",
};

static const char * const output_171[] = {
  "fild%z1\t%1",
  "#",
  "cvtsi2sd{q}\t{%1, %0|%0, %1}",
};

static const char * const output_173[] = {
  "fild%z1\t%1",
  "#",
};

static const char * const output_174[] = {
  "fild%z1\t%1",
  "#",
};

static const char * const output_175[] = {
  "fild%z1\t%1",
  "#",
};

static const char * const output_176[] = {
  "fild%z1\t%1",
  "#",
};

static const char * const output_177[] = {
  "fild%z1\t%1",
  "#",
};

static const char * const output_178[] = {
  "fild%z1\t%1",
  "#",
};

static const char *output_196 PARAMS ((rtx *, rtx));

static const char *
output_196 (operands, insn)
     rtx *operands ATTRIBUTE_UNUSED;
     rtx insn ATTRIBUTE_UNUSED;
{
{
  switch (get_attr_type (insn))
    {
    case TYPE_LEA:
      operands[2] = SET_SRC (XVECEXP (PATTERN (insn), 0, 0));
      return "lea{q}\t{%a2, %0|%0, %a2}";

    case TYPE_INCDEC:
      if (! rtx_equal_p (operands[0], operands[1]))
	abort ();
      if (operands[2] == const1_rtx)
        return "inc{q}\t%0";
      else if (operands[2] == constm1_rtx)
        return "dec{q}\t%0";
      else
	abort ();

    default:
      if (! rtx_equal_p (operands[0], operands[1]))
	abort ();

      /* Make things pretty and `subl $4,%eax' rather than `addl $-4, %eax'.
	 Exceptions: -128 encodes smaller than 128, so swap sign and op.  */
      if (GET_CODE (operands[2]) == CONST_INT
	  /* Avoid overflows.  */
	  && ((INTVAL (operands[2]) & ((((unsigned int) 1) << 31) - 1)))
          && (INTVAL (operands[2]) == 128
	      || (INTVAL (operands[2]) < 0
		  && INTVAL (operands[2]) != -128)))
        {
          operands[2] = GEN_INT (-INTVAL (operands[2]));
          return "sub{q}\t{%2, %0|%0, %2}";
        }
      return "add{q}\t{%2, %0|%0, %2}";
    }
}
}

static const char *output_197 PARAMS ((rtx *, rtx));

static const char *
output_197 (operands, insn)
     rtx *operands ATTRIBUTE_UNUSED;
     rtx insn ATTRIBUTE_UNUSED;
{
{
  switch (get_attr_type (insn))
    {
    case TYPE_INCDEC:
      if (! rtx_equal_p (operands[0], operands[1]))
	abort ();
      if (operands[2] == const1_rtx)
        return "inc{q}\t%0";
      else if (operands[2] == constm1_rtx)
        return "dec{q}\t%0";
      else
	abort ();

    default:
      if (! rtx_equal_p (operands[0], operands[1]))
	abort ();
      /* ???? We ought to handle there the 32bit case too
	 - do we need new constrant?  */
      /* Make things pretty and `subl $4,%eax' rather than `addl $-4, %eax'.
	 Exceptions: -128 encodes smaller than 128, so swap sign and op.  */
      if (GET_CODE (operands[2]) == CONST_INT
	  /* Avoid overflows.  */
	  && ((INTVAL (operands[2]) & ((((unsigned int) 1) << 31) - 1)))
          && (INTVAL (operands[2]) == 128
	      || (INTVAL (operands[2]) < 0
		  && INTVAL (operands[2]) != -128)))
        {
          operands[2] = GEN_INT (-INTVAL (operands[2]));
          return "sub{q}\t{%2, %0|%0, %2}";
        }
      return "add{q}\t{%2, %0|%0, %2}";
    }
}
}

static const char *output_198 PARAMS ((rtx *, rtx));

static const char *
output_198 (operands, insn)
     rtx *operands ATTRIBUTE_UNUSED;
     rtx insn ATTRIBUTE_UNUSED;
{
{
  switch (get_attr_type (insn))
    {
    case TYPE_INCDEC:
      if (! rtx_equal_p (operands[0], operands[1]))
	abort ();
      if (operands[2] == const1_rtx)
        return "inc{q}\t%0";
      else if (operands[2] == constm1_rtx)
        return "dec{q}\t%0";
      else
	abort ();

    default:
      if (! rtx_equal_p (operands[0], operands[1]))
	abort ();
      /* ???? We ought to handle there the 32bit case too
	 - do we need new constrant?  */
      /* Make things pretty and `subl $4,%eax' rather than `addl $-4, %eax'.
	 Exceptions: -128 encodes smaller than 128, so swap sign and op.  */
      if (GET_CODE (operands[2]) == CONST_INT
	  /* Avoid overflows.  */
	  && ((INTVAL (operands[2]) & ((((unsigned int) 1) << 31) - 1)))
          && (INTVAL (operands[2]) == 128
	      || (INTVAL (operands[2]) < 0
		  && INTVAL (operands[2]) != -128)))
        {
          operands[2] = GEN_INT (-INTVAL (operands[2]));
          return "sub{q}\t{%2, %0|%0, %2}";
        }
      return "add{q}\t{%2, %0|%0, %2}";
    }
}
}

static const char *output_199 PARAMS ((rtx *, rtx));

static const char *
output_199 (operands, insn)
     rtx *operands ATTRIBUTE_UNUSED;
     rtx insn ATTRIBUTE_UNUSED;
{
{
  switch (get_attr_type (insn))
    {
    case TYPE_INCDEC:
      if (operands[2] == constm1_rtx)
        return "inc{q}\t%0";
      else if (operands[2] == const1_rtx)
        return "dec{q}\t%0";
      else
	abort();

    default:
      if (! rtx_equal_p (operands[0], operands[1]))
	abort ();
      /* Make things pretty and `subl $4,%eax' rather than `addl $-4, %eax'.
	 Exceptions: -128 encodes smaller than 128, so swap sign and op.  */
      if ((INTVAL (operands[2]) == -128
	   || (INTVAL (operands[2]) > 0
	       && INTVAL (operands[2]) != 128))
	  /* Avoid overflows.  */
	  && ((INTVAL (operands[2]) & ((((unsigned int) 1) << 31) - 1))))
	return "sub{q}\t{%2, %0|%0, %2}";
      operands[2] = GEN_INT (-INTVAL (operands[2]));
      return "add{q}\t{%2, %0|%0, %2}";
    }
}
}

static const char *output_200 PARAMS ((rtx *, rtx));

static const char *
output_200 (operands, insn)
     rtx *operands ATTRIBUTE_UNUSED;
     rtx insn ATTRIBUTE_UNUSED;
{
{
  switch (get_attr_type (insn))
    {
    case TYPE_INCDEC:
      if (! rtx_equal_p (operands[0], operands[1]))
	abort ();
      if (operands[2] == const1_rtx)
        return "inc{q}\t%0";
      else if (operands[2] == constm1_rtx)
        return "dec{q}\t%0";
      else
	abort();

    default:
      if (! rtx_equal_p (operands[0], operands[1]))
	abort ();
      /* Make things pretty and `subl $4,%eax' rather than `addl $-4, %eax'.
	 Exceptions: -128 encodes smaller than 128, so swap sign and op.  */
      if (GET_CODE (operands[2]) == CONST_INT
	  /* Avoid overflows.  */
	  && ((INTVAL (operands[2]) & ((((unsigned int) 1) << 31) - 1)))
          && (INTVAL (operands[2]) == 128
	      || (INTVAL (operands[2]) < 0
		  && INTVAL (operands[2]) != -128)))
        {
          operands[2] = GEN_INT (-INTVAL (operands[2]));
          return "sub{q}\t{%2, %0|%0, %2}";
        }
      return "add{q}\t{%2, %0|%0, %2}";
    }
}
}

static const char *output_201 PARAMS ((rtx *, rtx));

static const char *
output_201 (operands, insn)
     rtx *operands ATTRIBUTE_UNUSED;
     rtx insn ATTRIBUTE_UNUSED;
{
{
  switch (get_attr_type (insn))
    {
    case TYPE_LEA:
      operands[2] = SET_SRC (XVECEXP (PATTERN (insn), 0, 0));
      return "lea{l}\t{%a2, %0|%0, %a2}";

    case TYPE_INCDEC:
      if (! rtx_equal_p (operands[0], operands[1]))
	abort ();
      if (operands[2] == const1_rtx)
        return "inc{l}\t%0";
      else if (operands[2] == constm1_rtx)
        return "dec{l}\t%0";
      else
	abort();

    default:
      if (! rtx_equal_p (operands[0], operands[1]))
	abort ();

      /* Make things pretty and `subl $4,%eax' rather than `addl $-4, %eax'.
	 Exceptions: -128 encodes smaller than 128, so swap sign and op.  */
      if (GET_CODE (operands[2]) == CONST_INT
          && (INTVAL (operands[2]) == 128
	      || (INTVAL (operands[2]) < 0
		  && INTVAL (operands[2]) != -128)))
        {
          operands[2] = GEN_INT (-INTVAL (operands[2]));
          return "sub{l}\t{%2, %0|%0, %2}";
        }
      return "add{l}\t{%2, %0|%0, %2}";
    }
}
}

static const char *output_202 PARAMS ((rtx *, rtx));

static const char *
output_202 (operands, insn)
     rtx *operands ATTRIBUTE_UNUSED;
     rtx insn ATTRIBUTE_UNUSED;
{
{
  switch (get_attr_type (insn))
    {
    case TYPE_LEA:
      operands[2] = SET_SRC (XVECEXP (PATTERN (insn), 0, 0));
      return "lea{l}\t{%a2, %k0|%k0, %a2}";

    case TYPE_INCDEC:
      if (operands[2] == const1_rtx)
        return "inc{l}\t%k0";
      else if (operands[2] == constm1_rtx)
        return "dec{l}\t%k0";
      else
	abort();

    default:
      /* Make things pretty and `subl $4,%eax' rather than `addl $-4, %eax'.
	 Exceptions: -128 encodes smaller than 128, so swap sign and op.  */
      if (GET_CODE (operands[2]) == CONST_INT
          && (INTVAL (operands[2]) == 128
	      || (INTVAL (operands[2]) < 0
		  && INTVAL (operands[2]) != -128)))
        {
          operands[2] = GEN_INT (-INTVAL (operands[2]));
          return "sub{l}\t{%2, %k0|%k0, %2}";
        }
      return "add{l}\t{%2, %k0|%k0, %2}";
    }
}
}

static const char *output_203 PARAMS ((rtx *, rtx));

static const char *
output_203 (operands, insn)
     rtx *operands ATTRIBUTE_UNUSED;
     rtx insn ATTRIBUTE_UNUSED;
{
{
  switch (get_attr_type (insn))
    {
    case TYPE_INCDEC:
      if (! rtx_equal_p (operands[0], operands[1]))
	abort ();
      if (operands[2] == const1_rtx)
        return "inc{l}\t%0";
      else if (operands[2] == constm1_rtx)
        return "dec{l}\t%0";
      else
	abort();

    default:
      if (! rtx_equal_p (operands[0], operands[1]))
	abort ();
      /* Make things pretty and `subl $4,%eax' rather than `addl $-4, %eax'.
	 Exceptions: -128 encodes smaller than 128, so swap sign and op.  */
      if (GET_CODE (operands[2]) == CONST_INT
          && (INTVAL (operands[2]) == 128
	      || (INTVAL (operands[2]) < 0
		  && INTVAL (operands[2]) != -128)))
        {
          operands[2] = GEN_INT (-INTVAL (operands[2]));
          return "sub{l}\t{%2, %0|%0, %2}";
        }
      return "add{l}\t{%2, %0|%0, %2}";
    }
}
}

static const char *output_204 PARAMS ((rtx *, rtx));

static const char *
output_204 (operands, insn)
     rtx *operands ATTRIBUTE_UNUSED;
     rtx insn ATTRIBUTE_UNUSED;
{
{
  switch (get_attr_type (insn))
    {
    case TYPE_INCDEC:
      if (operands[2] == const1_rtx)
        return "inc{l}\t%k0";
      else if (operands[2] == constm1_rtx)
        return "dec{l}\t%k0";
      else
	abort();

    default:
      /* Make things pretty and `subl $4,%eax' rather than `addl $-4, %eax'.
	 Exceptions: -128 encodes smaller than 128, so swap sign and op.  */
      if (GET_CODE (operands[2]) == CONST_INT
          && (INTVAL (operands[2]) == 128
	      || (INTVAL (operands[2]) < 0
		  && INTVAL (operands[2]) != -128)))
        {
          operands[2] = GEN_INT (-INTVAL (operands[2]));
          return "sub{l}\t{%2, %k0|%k0, %2}";
        }
      return "add{l}\t{%2, %k0|%k0, %2}";
    }
}
}

static const char *output_205 PARAMS ((rtx *, rtx));

static const char *
output_205 (operands, insn)
     rtx *operands ATTRIBUTE_UNUSED;
     rtx insn ATTRIBUTE_UNUSED;
{
{
  switch (get_attr_type (insn))
    {
    case TYPE_INCDEC:
      if (! rtx_equal_p (operands[0], operands[1]))
	abort ();
      if (operands[2] == const1_rtx)
        return "inc{l}\t%0";
      else if (operands[2] == constm1_rtx)
        return "dec{l}\t%0";
      else
	abort();

    default:
      if (! rtx_equal_p (operands[0], operands[1]))
	abort ();
      /* Make things pretty and `subl $4,%eax' rather than `addl $-4, %eax'.
	 Exceptions: -128 encodes smaller than 128, so swap sign and op.  */
      if (GET_CODE (operands[2]) == CONST_INT
          && (INTVAL (operands[2]) == 128
	      || (INTVAL (operands[2]) < 0
		  && INTVAL (operands[2]) != -128)))
        {
          operands[2] = GEN_INT (-INTVAL (operands[2]));
          return "sub{l}\t{%2, %0|%0, %2}";
        }
      return "add{l}\t{%2, %0|%0, %2}";
    }
}
}

static const char *output_206 PARAMS ((rtx *, rtx));

static const char *
output_206 (operands, insn)
     rtx *operands ATTRIBUTE_UNUSED;
     rtx insn ATTRIBUTE_UNUSED;
{
{
  switch (get_attr_type (insn))
    {
    case TYPE_INCDEC:
      if (operands[2] == const1_rtx)
        return "inc{l}\t%k0";
      else if (operands[2] == constm1_rtx)
        return "dec{l}\t%k0";
      else
	abort();

    default:
      /* Make things pretty and `subl $4,%eax' rather than `addl $-4, %eax'.
	 Exceptions: -128 encodes smaller than 128, so swap sign and op.  */
      if (GET_CODE (operands[2]) == CONST_INT
          && (INTVAL (operands[2]) == 128
	      || (INTVAL (operands[2]) < 0
		  && INTVAL (operands[2]) != -128)))
        {
          operands[2] = GEN_INT (-INTVAL (operands[2]));
          return "sub{l}\t{%2, %k0|%k0, %2}";
        }
      return "add{l}\t{%2, %k0|%k0, %2}";
    }
}
}

static const char *output_207 PARAMS ((rtx *, rtx));

static const char *
output_207 (operands, insn)
     rtx *operands ATTRIBUTE_UNUSED;
     rtx insn ATTRIBUTE_UNUSED;
{
{
  switch (get_attr_type (insn))
    {
    case TYPE_INCDEC:
      if (operands[2] == constm1_rtx)
        return "inc{l}\t%0";
      else if (operands[2] == const1_rtx)
        return "dec{l}\t%0";
      else
	abort();

    default:
      if (! rtx_equal_p (operands[0], operands[1]))
	abort ();
      /* Make things pretty and `subl $4,%eax' rather than `addl $-4, %eax'.
	 Exceptions: -128 encodes smaller than 128, so swap sign and op.  */
      if ((INTVAL (operands[2]) == -128
	   || (INTVAL (operands[2]) > 0
	       && INTVAL (operands[2]) != 128)))
	return "sub{l}\t{%2, %0|%0, %2}";
      operands[2] = GEN_INT (-INTVAL (operands[2]));
      return "add{l}\t{%2, %0|%0, %2}";
    }
}
}

static const char *output_208 PARAMS ((rtx *, rtx));

static const char *
output_208 (operands, insn)
     rtx *operands ATTRIBUTE_UNUSED;
     rtx insn ATTRIBUTE_UNUSED;
{
{
  switch (get_attr_type (insn))
    {
    case TYPE_INCDEC:
      if (! rtx_equal_p (operands[0], operands[1]))
	abort ();
      if (operands[2] == const1_rtx)
        return "inc{l}\t%0";
      else if (operands[2] == constm1_rtx)
        return "dec{l}\t%0";
      else
	abort();

    default:
      if (! rtx_equal_p (operands[0], operands[1]))
	abort ();
      /* Make things pretty and `subl $4,%eax' rather than `addl $-4, %eax'.
	 Exceptions: -128 encodes smaller than 128, so swap sign and op.  */
      if (GET_CODE (operands[2]) == CONST_INT
          && (INTVAL (operands[2]) == 128
	      || (INTVAL (operands[2]) < 0
		  && INTVAL (operands[2]) != -128)))
        {
          operands[2] = GEN_INT (-INTVAL (operands[2]));
          return "sub{l}\t{%2, %0|%0, %2}";
        }
      return "add{l}\t{%2, %0|%0, %2}";
    }
}
}

static const char *output_209 PARAMS ((rtx *, rtx));

static const char *
output_209 (operands, insn)
     rtx *operands ATTRIBUTE_UNUSED;
     rtx insn ATTRIBUTE_UNUSED;
{
{
  switch (get_attr_type (insn))
    {
    case TYPE_LEA:
      return "#";
    case TYPE_INCDEC:
      if (operands[2] == const1_rtx)
	return "inc{w}\t%0";
      else if (operands[2] == constm1_rtx)
	return "dec{w}\t%0";
      abort();

    default:
      /* Make things pretty and `subl $4,%eax' rather than `addl $-4, %eax'.
	 Exceptions: -128 encodes smaller than 128, so swap sign and op.  */
      if (GET_CODE (operands[2]) == CONST_INT
          && (INTVAL (operands[2]) == 128
	      || (INTVAL (operands[2]) < 0
		  && INTVAL (operands[2]) != -128)))
	{
	  operands[2] = GEN_INT (-INTVAL (operands[2]));
	  return "sub{w}\t{%2, %0|%0, %2}";
	}
      return "add{w}\t{%2, %0|%0, %2}";
    }
}
}

static const char *output_210 PARAMS ((rtx *, rtx));

static const char *
output_210 (operands, insn)
     rtx *operands ATTRIBUTE_UNUSED;
     rtx insn ATTRIBUTE_UNUSED;
{
{
  switch (get_attr_type (insn))
    {
    case TYPE_INCDEC:
      if (operands[2] == const1_rtx)
	return "inc{w}\t%0";
      else if (operands[2] == constm1_rtx)
	return "dec{w}\t%0";
      abort();

    default:
      /* Make things pretty and `subl $4,%eax' rather than `addl $-4, %eax'.
	 Exceptions: -128 encodes smaller than 128, so swap sign and op.  */
      if (GET_CODE (operands[2]) == CONST_INT
          && (INTVAL (operands[2]) == 128
	      || (INTVAL (operands[2]) < 0
		  && INTVAL (operands[2]) != -128)))
	{
	  operands[2] = GEN_INT (-INTVAL (operands[2]));
	  return "sub{w}\t{%2, %0|%0, %2}";
	}
      return "add{w}\t{%2, %0|%0, %2}";
    }
}
}

static const char *output_211 PARAMS ((rtx *, rtx));

static const char *
output_211 (operands, insn)
     rtx *operands ATTRIBUTE_UNUSED;
     rtx insn ATTRIBUTE_UNUSED;
{
{
  switch (get_attr_type (insn))
    {
    case TYPE_INCDEC:
      if (operands[2] == const1_rtx)
	return "inc{w}\t%0";
      else if (operands[2] == constm1_rtx)
	return "dec{w}\t%0";
      abort();

    default:
      /* Make things pretty and `subl $4,%eax' rather than `addl $-4, %eax'.
	 Exceptions: -128 encodes smaller than 128, so swap sign and op.  */
      if (GET_CODE (operands[2]) == CONST_INT
          && (INTVAL (operands[2]) == 128
	      || (INTVAL (operands[2]) < 0
		  && INTVAL (operands[2]) != -128)))
	{
	  operands[2] = GEN_INT (-INTVAL (operands[2]));
	  return "sub{w}\t{%2, %0|%0, %2}";
	}
      return "add{w}\t{%2, %0|%0, %2}";
    }
}
}

static const char *output_212 PARAMS ((rtx *, rtx));

static const char *
output_212 (operands, insn)
     rtx *operands ATTRIBUTE_UNUSED;
     rtx insn ATTRIBUTE_UNUSED;
{
{
  switch (get_attr_type (insn))
    {
    case TYPE_INCDEC:
      if (operands[2] == const1_rtx)
	return "inc{w}\t%0";
      else if (operands[2] == constm1_rtx)
	return "dec{w}\t%0";
      abort();

    default:
      /* Make things pretty and `subl $4,%eax' rather than `addl $-4, %eax'.
	 Exceptions: -128 encodes smaller than 128, so swap sign and op.  */
      if (GET_CODE (operands[2]) == CONST_INT
          && (INTVAL (operands[2]) == 128
	      || (INTVAL (operands[2]) < 0
		  && INTVAL (operands[2]) != -128)))
	{
	  operands[2] = GEN_INT (-INTVAL (operands[2]));
	  return "sub{w}\t{%2, %0|%0, %2}";
	}
      return "add{w}\t{%2, %0|%0, %2}";
    }
}
}

static const char *output_213 PARAMS ((rtx *, rtx));

static const char *
output_213 (operands, insn)
     rtx *operands ATTRIBUTE_UNUSED;
     rtx insn ATTRIBUTE_UNUSED;
{
{
  switch (get_attr_type (insn))
    {
    case TYPE_INCDEC:
      if (operands[2] == constm1_rtx)
        return "inc{w}\t%0";
      else if (operands[2] == const1_rtx)
        return "dec{w}\t%0";
      else
	abort();

    default:
      if (! rtx_equal_p (operands[0], operands[1]))
	abort ();
      /* Make things pretty and `subl $4,%eax' rather than `addl $-4, %eax'.
	 Exceptions: -128 encodes smaller than 128, so swap sign and op.  */
      if ((INTVAL (operands[2]) == -128
	   || (INTVAL (operands[2]) > 0
	       && INTVAL (operands[2]) != 128)))
	return "sub{w}\t{%2, %0|%0, %2}";
      operands[2] = GEN_INT (-INTVAL (operands[2]));
      return "add{w}\t{%2, %0|%0, %2}";
    }
}
}

static const char *output_214 PARAMS ((rtx *, rtx));

static const char *
output_214 (operands, insn)
     rtx *operands ATTRIBUTE_UNUSED;
     rtx insn ATTRIBUTE_UNUSED;
{
{
  switch (get_attr_type (insn))
    {
    case TYPE_INCDEC:
      if (operands[2] == const1_rtx)
	return "inc{w}\t%0";
      else if (operands[2] == constm1_rtx)
	return "dec{w}\t%0";
      abort();

    default:
      /* Make things pretty and `subl $4,%eax' rather than `addl $-4, %eax'.
	 Exceptions: -128 encodes smaller than 128, so swap sign and op.  */
      if (GET_CODE (operands[2]) == CONST_INT
          && (INTVAL (operands[2]) == 128
	      || (INTVAL (operands[2]) < 0
		  && INTVAL (operands[2]) != -128)))
	{
	  operands[2] = GEN_INT (-INTVAL (operands[2]));
	  return "sub{w}\t{%2, %0|%0, %2}";
	}
      return "add{w}\t{%2, %0|%0, %2}";
    }
}
}

static const char *output_215 PARAMS ((rtx *, rtx));

static const char *
output_215 (operands, insn)
     rtx *operands ATTRIBUTE_UNUSED;
     rtx insn ATTRIBUTE_UNUSED;
{
{
  int widen = (which_alternative == 2);
  switch (get_attr_type (insn))
    {
    case TYPE_LEA:
      return "#";
    case TYPE_INCDEC:
      if (operands[2] == const1_rtx)
	return widen ? "inc{l}\t%k0" : "inc{b}\t%0";
      else if (operands[2] == constm1_rtx)
	return widen ? "dec{l}\t%k0" : "dec{b}\t%0";
      abort();

    default:
      /* Make things pretty and `subl $4,%eax' rather than `addl $-4, %eax'.
	 Exceptions: -128 encodes smaller than 128, so swap sign and op.  */
      if (GET_CODE (operands[2]) == CONST_INT
          && (INTVAL (operands[2]) == 128
	      || (INTVAL (operands[2]) < 0
		  && INTVAL (operands[2]) != -128)))
	{
	  operands[2] = GEN_INT (-INTVAL (operands[2]));
	  if (widen)
	    return "sub{l}\t{%2, %k0|%k0, %2}";
	  else
	    return "sub{b}\t{%2, %0|%0, %2}";
	}
      if (widen)
        return "add{l}\t{%k2, %k0|%k0, %k2}";
      else
        return "add{b}\t{%2, %0|%0, %2}";
    }
}
}

static const char *output_216 PARAMS ((rtx *, rtx));

static const char *
output_216 (operands, insn)
     rtx *operands ATTRIBUTE_UNUSED;
     rtx insn ATTRIBUTE_UNUSED;
{
{
  int widen = (which_alternative == 2);
  switch (get_attr_type (insn))
    {
    case TYPE_INCDEC:
      if (operands[2] == const1_rtx)
	return widen ? "inc{l}\t%k0" : "inc{b}\t%0";
      else if (operands[2] == constm1_rtx)
	return widen ? "dec{l}\t%k0" : "dec{b}\t%0";
      abort();

    default:
      /* Make things pretty and `subl $4,%eax' rather than `addl $-4, %eax'.
	 Exceptions: -128 encodes smaller than 128, so swap sign and op.  */
      if (GET_CODE (operands[2]) == CONST_INT
          && (INTVAL (operands[2]) == 128
	      || (INTVAL (operands[2]) < 0
		  && INTVAL (operands[2]) != -128)))
	{
	  operands[2] = GEN_INT (-INTVAL (operands[2]));
	  if (widen)
	    return "sub{l}\t{%2, %k0|%k0, %2}";
	  else
	    return "sub{b}\t{%2, %0|%0, %2}";
	}
      if (widen)
        return "add{l}\t{%k2, %k0|%k0, %k2}";
      else
        return "add{b}\t{%2, %0|%0, %2}";
    }
}
}

static const char *output_217 PARAMS ((rtx *, rtx));

static const char *
output_217 (operands, insn)
     rtx *operands ATTRIBUTE_UNUSED;
     rtx insn ATTRIBUTE_UNUSED;
{
{
  switch (get_attr_type (insn))
    {
    case TYPE_INCDEC:
      if (operands[1] == const1_rtx)
	return "inc{b}\t%0";
      else if (operands[1] == constm1_rtx)
	return "dec{b}\t%0";
      abort();

    default:
      /* Make things pretty and `subl $4,%eax' rather than `addl $-4, %eax'.  */
      if (GET_CODE (operands[1]) == CONST_INT
	  && INTVAL (operands[1]) < 0)
	{
	  operands[2] = GEN_INT (-INTVAL (operands[2]));
	  return "sub{b}\t{%1, %0|%0, %1}";
	}
      return "add{b}\t{%1, %0|%0, %1}";
    }
}
}

static const char *output_218 PARAMS ((rtx *, rtx));

static const char *
output_218 (operands, insn)
     rtx *operands ATTRIBUTE_UNUSED;
     rtx insn ATTRIBUTE_UNUSED;
{
{
  switch (get_attr_type (insn))
    {
    case TYPE_INCDEC:
      if (operands[2] == const1_rtx)
	return "inc{b}\t%0";
      else if (operands[2] == constm1_rtx
	       || (GET_CODE (operands[2]) == CONST_INT
		   && INTVAL (operands[2]) == 255))
	return "dec{b}\t%0";
      abort();

    default:
      /* Make things pretty and `subb $4,%al' rather than `addb $-4, %al'.  */
      if (GET_CODE (operands[2]) == CONST_INT
          && INTVAL (operands[2]) < 0)
	{
	  operands[2] = GEN_INT (-INTVAL (operands[2]));
	  return "sub{b}\t{%2, %0|%0, %2}";
	}
      return "add{b}\t{%2, %0|%0, %2}";
    }
}
}

static const char *output_219 PARAMS ((rtx *, rtx));

static const char *
output_219 (operands, insn)
     rtx *operands ATTRIBUTE_UNUSED;
     rtx insn ATTRIBUTE_UNUSED;
{
{
  switch (get_attr_type (insn))
    {
    case TYPE_INCDEC:
      if (operands[2] == const1_rtx)
	return "inc{b}\t%0";
      else if (operands[2] == constm1_rtx
	       || (GET_CODE (operands[2]) == CONST_INT
		   && INTVAL (operands[2]) == 255))
	return "dec{b}\t%0";
      abort();

    default:
      /* Make things pretty and `subb $4,%al' rather than `addb $-4, %al'.  */
      if (GET_CODE (operands[2]) == CONST_INT
          && INTVAL (operands[2]) < 0)
	{
	  operands[2] = GEN_INT (-INTVAL (operands[2]));
	  return "sub{b}\t{%2, %0|%0, %2}";
	}
      return "add{b}\t{%2, %0|%0, %2}";
    }
}
}

static const char *output_220 PARAMS ((rtx *, rtx));

static const char *
output_220 (operands, insn)
     rtx *operands ATTRIBUTE_UNUSED;
     rtx insn ATTRIBUTE_UNUSED;
{
{
  switch (get_attr_type (insn))
    {
    case TYPE_INCDEC:
      if (operands[2] == constm1_rtx
	  || (GET_CODE (operands[2]) == CONST_INT
	      && INTVAL (operands[2]) == 255))
        return "inc{b}\t%0";
      else if (operands[2] == const1_rtx)
        return "dec{b}\t%0";
      else
	abort();

    default:
      if (! rtx_equal_p (operands[0], operands[1]))
	abort ();
      if (INTVAL (operands[2]) < 0)
        {
          operands[2] = GEN_INT (-INTVAL (operands[2]));
          return "add{b}\t{%2, %0|%0, %2}";
        }
      return "sub{b}\t{%2, %0|%0, %2}";
    }
}
}

static const char *output_221 PARAMS ((rtx *, rtx));

static const char *
output_221 (operands, insn)
     rtx *operands ATTRIBUTE_UNUSED;
     rtx insn ATTRIBUTE_UNUSED;
{
{
  switch (get_attr_type (insn))
    {
    case TYPE_INCDEC:
      if (operands[2] == const1_rtx)
	return "inc{b}\t%0";
      else if (operands[2] == constm1_rtx
	       || (GET_CODE (operands[2]) == CONST_INT
		   && INTVAL (operands[2]) == 255))
	return "dec{b}\t%0";
      abort();

    default:
      /* Make things pretty and `subb $4,%al' rather than `addb $-4, %al'.  */
      if (GET_CODE (operands[2]) == CONST_INT
          && INTVAL (operands[2]) < 0)
	{
	  operands[2] = GEN_INT (-INTVAL (operands[2]));
	  return "sub{b}\t{%2, %0|%0, %2}";
	}
      return "add{b}\t{%2, %0|%0, %2}";
    }
}
}

static const char *output_222 PARAMS ((rtx *, rtx));

static const char *
output_222 (operands, insn)
     rtx *operands ATTRIBUTE_UNUSED;
     rtx insn ATTRIBUTE_UNUSED;
{
{
  switch (get_attr_type (insn))
    {
    case TYPE_INCDEC:
      if (operands[2] == const1_rtx)
	return "inc{b}\t%h0";
      else if (operands[2] == constm1_rtx
	       || (GET_CODE (operands[2]) == CONST_INT
		   && INTVAL (operands[2]) == 255))
	return "dec{b}\t%h0";
      abort();

    default:
      return "add{b}\t{%2, %h0|%h0, %2}";
    }
}
}

static const char *output_223 PARAMS ((rtx *, rtx));

static const char *
output_223 (operands, insn)
     rtx *operands ATTRIBUTE_UNUSED;
     rtx insn ATTRIBUTE_UNUSED;
{
{
  switch (get_attr_type (insn))
    {
    case TYPE_INCDEC:
      if (operands[2] == const1_rtx)
	return "inc{b}\t%h0";
      else if (operands[2] == constm1_rtx
	       || (GET_CODE (operands[2]) == CONST_INT
		   && INTVAL (operands[2]) == 255))
	return "dec{b}\t%h0";
      abort();

    default:
      return "add{b}\t{%2, %h0|%h0, %2}";
    }
}
}

static const char * const output_245[] = {
  "imul{q}\t{%2, %1, %0|%0, %1, %2}",
  "imul{q}\t{%2, %1, %0|%0, %1, %2}",
  "imul{q}\t{%2, %0|%0, %2}",
};

static const char * const output_246[] = {
  "imul{l}\t{%2, %1, %0|%0, %1, %2}",
  "imul{l}\t{%2, %1, %0|%0, %1, %2}",
  "imul{l}\t{%2, %0|%0, %2}",
};

static const char * const output_247[] = {
  "imul{l}\t{%2, %1, %k0|%k0, %1, %2}",
  "imul{l}\t{%2, %1, %k0|%k0, %1, %2}",
  "imul{l}\t{%2, %k0|%k0, %2}",
};

static const char * const output_248[] = {
  "imul{w}\t{%2, %1, %0|%0, %1, %2}",
  "imul{w}\t{%2, %1, %0|%0, %1, %2}",
  "imul{w}\t{%2, %0|%0, %2}",
};

static const char * const output_276[] = {
  "test{l}\t{%k1, %k0|%k0, %k1} ",
  "test{l}\t{%k1, %k0|%k0, %k1} ",
  "test{q}\t{%1, %0|%0, %1} ",
  "test{q}\t{%1, %0|%0, %1} ",
  "test{q}\t{%1, %0|%0, %1}",
};

static const char *output_279 PARAMS ((rtx *, rtx));

static const char *
output_279 (operands, insn)
     rtx *operands ATTRIBUTE_UNUSED;
     rtx insn ATTRIBUTE_UNUSED;
{
{
  if (which_alternative == 3)
    {
      if (GET_CODE (operands[1]) == CONST_INT
	  && (INTVAL (operands[1]) & 0xffffff00))
	operands[1] = GEN_INT (INTVAL (operands[1]) & 0xff);
      return "test{l}\t{%1, %k0|%k0, %1}";
    }
  return "test{b}\t{%1, %0|%0, %1}";
}
}

static const char *output_286 PARAMS ((rtx *, rtx));

static const char *
output_286 (operands, insn)
     rtx *operands ATTRIBUTE_UNUSED;
     rtx insn ATTRIBUTE_UNUSED;
{
{
  switch (get_attr_type (insn))
    {
    case TYPE_IMOVX:
      {
	enum machine_mode mode;

	if (GET_CODE (operands[2]) != CONST_INT)
	  abort ();
        if (INTVAL (operands[2]) == 0xff)
	  mode = QImode;
	else if (INTVAL (operands[2]) == 0xffff)
	  mode = HImode;
	else
	  abort ();
	
	operands[1] = gen_lowpart (mode, operands[1]);
	if (mode == QImode)
	  return "movz{bq|x}\t{%1,%0|%0, %1}";
	else
	  return "movz{wq|x}\t{%1,%0|%0, %1}";
      }

    default:
      if (! rtx_equal_p (operands[0], operands[1]))
	abort ();
      if (get_attr_mode (insn) == MODE_SI)
	return "and{l}\t{%k2, %k0|%k0, %k2}";
      else
	return "and{q}\t{%2, %0|%0, %2}";
    }
}
}

static const char * const output_287[] = {
  "and{l}\t{%k2, %k0|%k0, %k2} ",
  "and{q}\t{%2, %0|%0, %2} ",
  "and{q}\t{%2, %0|%0, %2}",
};

static const char *output_288 PARAMS ((rtx *, rtx));

static const char *
output_288 (operands, insn)
     rtx *operands ATTRIBUTE_UNUSED;
     rtx insn ATTRIBUTE_UNUSED;
{
{
  switch (get_attr_type (insn))
    {
    case TYPE_IMOVX:
      {
	enum machine_mode mode;

	if (GET_CODE (operands[2]) != CONST_INT)
	  abort ();
        if (INTVAL (operands[2]) == 0xff)
	  mode = QImode;
	else if (INTVAL (operands[2]) == 0xffff)
	  mode = HImode;
	else
	  abort ();
	
	operands[1] = gen_lowpart (mode, operands[1]);
	if (mode == QImode)
	  return "movz{bl|x}\t{%1,%0|%0, %1}";
	else
	  return "movz{wl|x}\t{%1,%0|%0, %1}";
      }

    default:
      if (! rtx_equal_p (operands[0], operands[1]))
	abort ();
      return "and{l}\t{%2, %0|%0, %2}";
    }
}
}

static const char *output_292 PARAMS ((rtx *, rtx));

static const char *
output_292 (operands, insn)
     rtx *operands ATTRIBUTE_UNUSED;
     rtx insn ATTRIBUTE_UNUSED;
{
{
  switch (get_attr_type (insn))
    {
    case TYPE_IMOVX:
      if (GET_CODE (operands[2]) != CONST_INT)
	abort ();
      if (INTVAL (operands[2]) == 0xff)
	return "movz{bl|x}\t{%b1, %k0|%k0, %b1}";
      abort ();

    default:
      if (! rtx_equal_p (operands[0], operands[1]))
	abort ();

      return "and{w}\t{%2, %0|%0, %2}";
    }
}
}

static const char * const output_294[] = {
  "and{b}\t{%2, %0|%0, %2}",
  "and{b}\t{%2, %0|%0, %2}",
  "and{l}\t{%k2, %k0|%k0, %k2}",
};

static const char *output_296 PARAMS ((rtx *, rtx));

static const char *
output_296 (operands, insn)
     rtx *operands ATTRIBUTE_UNUSED;
     rtx insn ATTRIBUTE_UNUSED;
{
{
  if (which_alternative == 2)
    {
      if (GET_CODE (operands[2]) == CONST_INT
          && (INTVAL (operands[2]) & 0xffffff00))
        operands[2] = GEN_INT (INTVAL (operands[2]) & 0xff);
      return "and{l}\t{%2, %k0|%k0, %2}";
    }
  return "and{b}\t{%2, %0|%0, %2}";
}
}

static const char * const output_316[] = {
  "or{b}\t{%2, %0|%0, %2}",
  "or{b}\t{%2, %0|%0, %2}",
  "or{l}\t{%k2, %k0|%k0, %k2}",
};

static const char * const output_325[] = {
  "xor{q}\t{%2, %0|%0, %2} ",
  "xor{q}\t{%2, %0|%0, %2}",
};

static const char * const output_326[] = {
  "xor{q}\t{%2, %0|%0, %2} ",
  "xor{q}\t{%2, %0|%0, %2}",
};

static const char * const output_338[] = {
  "xor{b}\t{%2, %0|%0, %2}",
  "xor{b}\t{%2, %0|%0, %2}",
  "xor{l}\t{%k2, %k0|%k0, %k2}",
};

static const char * const output_406[] = {
  "not{b}\t%0",
  "not{l}\t%k0",
};

static const char *output_408 PARAMS ((rtx *, rtx));

static const char *
output_408 (operands, insn)
     rtx *operands ATTRIBUTE_UNUSED;
     rtx insn ATTRIBUTE_UNUSED;
{
{
  switch (get_attr_type (insn))
    {
    case TYPE_ALU:
      if (operands[2] != const1_rtx)
	abort ();
      if (!rtx_equal_p (operands[0], operands[1]))
	abort ();
      return "add{q}\t{%0, %0|%0, %0}";

    case TYPE_LEA:
      if (GET_CODE (operands[2]) != CONST_INT
	  || (unsigned HOST_WIDE_INT) INTVAL (operands[2]) > 3)
	abort ();
      operands[1] = gen_rtx_MULT (DImode, operands[1],
				  GEN_INT (1 << INTVAL (operands[2])));
      return "lea{q}\t{%a1, %0|%0, %a1}";

    default:
      if (REG_P (operands[2]))
	return "sal{q}\t{%b2, %0|%0, %b2}";
      else if (GET_CODE (operands[2]) == CONST_INT
	       && INTVAL (operands[2]) == 1
	       && (TARGET_SHIFT1 || optimize_size))
	return "sal{q}\t%0";
      else
	return "sal{q}\t{%2, %0|%0, %2}";
    }
}
}

static const char *output_409 PARAMS ((rtx *, rtx));

static const char *
output_409 (operands, insn)
     rtx *operands ATTRIBUTE_UNUSED;
     rtx insn ATTRIBUTE_UNUSED;
{
{
  switch (get_attr_type (insn))
    {
    case TYPE_ALU:
      if (operands[2] != const1_rtx)
	abort ();
      return "add{q}\t{%0, %0|%0, %0}";

    default:
      if (REG_P (operands[2]))
	return "sal{q}\t{%b2, %0|%0, %b2}";
      else if (GET_CODE (operands[2]) == CONST_INT
	       && INTVAL (operands[2]) == 1
	       && (TARGET_SHIFT1 || optimize_size))
	return "sal{q}\t%0";
      else
	return "sal{q}\t{%2, %0|%0, %2}";
    }
}
}

static const char * const output_412[] = {
  "shld{l}\t{%2, %1, %0|%0, %1, %2}",
  "shld{l}\t{%s2%1, %0|%0, %1, %2}",
};

static const char *output_413 PARAMS ((rtx *, rtx));

static const char *
output_413 (operands, insn)
     rtx *operands ATTRIBUTE_UNUSED;
     rtx insn ATTRIBUTE_UNUSED;
{
{
  switch (get_attr_type (insn))
    {
    case TYPE_ALU:
      if (operands[2] != const1_rtx)
	abort ();
      if (!rtx_equal_p (operands[0], operands[1]))
	abort ();
      return "add{l}\t{%0, %0|%0, %0}";

    case TYPE_LEA:
      return "#";

    default:
      if (REG_P (operands[2]))
	return "sal{l}\t{%b2, %0|%0, %b2}";
      else if (GET_CODE (operands[2]) == CONST_INT
	       && INTVAL (operands[2]) == 1
	       && (TARGET_SHIFT1 || optimize_size))
	return "sal{l}\t%0";
      else
	return "sal{l}\t{%2, %0|%0, %2}";
    }
}
}

static const char *output_414 PARAMS ((rtx *, rtx));

static const char *
output_414 (operands, insn)
     rtx *operands ATTRIBUTE_UNUSED;
     rtx insn ATTRIBUTE_UNUSED;
{
{
  switch (get_attr_type (insn))
    {
    case TYPE_ALU:
      if (operands[2] != const1_rtx)
	abort ();
      return "add{l}\t{%k0, %k0|%k0, %k0}";

    case TYPE_LEA:
      return "#";

    default:
      if (REG_P (operands[2]))
	return "sal{l}\t{%b2, %k0|%k0, %b2}";
      else if (GET_CODE (operands[2]) == CONST_INT
	       && INTVAL (operands[2]) == 1
	       && (TARGET_SHIFT1 || optimize_size))
	return "sal{l}\t%k0";
      else
	return "sal{l}\t{%2, %k0|%k0, %2}";
    }
}
}

static const char *output_415 PARAMS ((rtx *, rtx));

static const char *
output_415 (operands, insn)
     rtx *operands ATTRIBUTE_UNUSED;
     rtx insn ATTRIBUTE_UNUSED;
{
{
  switch (get_attr_type (insn))
    {
    case TYPE_ALU:
      if (operands[2] != const1_rtx)
	abort ();
      return "add{l}\t{%0, %0|%0, %0}";

    default:
      if (REG_P (operands[2]))
	return "sal{l}\t{%b2, %0|%0, %b2}";
      else if (GET_CODE (operands[2]) == CONST_INT
	       && INTVAL (operands[2]) == 1
	       && (TARGET_SHIFT1 || optimize_size))
	return "sal{l}\t%0";
      else
	return "sal{l}\t{%2, %0|%0, %2}";
    }
}
}

static const char *output_416 PARAMS ((rtx *, rtx));

static const char *
output_416 (operands, insn)
     rtx *operands ATTRIBUTE_UNUSED;
     rtx insn ATTRIBUTE_UNUSED;
{
{
  switch (get_attr_type (insn))
    {
    case TYPE_ALU:
      if (operands[2] != const1_rtx)
	abort ();
      return "add{l}\t{%k0, %k0|%k0, %k0}";

    default:
      if (REG_P (operands[2]))
	return "sal{l}\t{%b2, %k0|%k0, %b2}";
      else if (GET_CODE (operands[2]) == CONST_INT
	       && INTVAL (operands[2]) == 1
	       && (TARGET_SHIFT1 || optimize_size))
	return "sal{l}\t%k0";
      else
	return "sal{l}\t{%2, %k0|%k0, %2}";
    }
}
}

static const char *output_417 PARAMS ((rtx *, rtx));

static const char *
output_417 (operands, insn)
     rtx *operands ATTRIBUTE_UNUSED;
     rtx insn ATTRIBUTE_UNUSED;
{
{
  switch (get_attr_type (insn))
    {
    case TYPE_LEA:
      return "#";
    case TYPE_ALU:
      if (operands[2] != const1_rtx)
	abort ();
      return "add{w}\t{%0, %0|%0, %0}";

    default:
      if (REG_P (operands[2]))
	return "sal{w}\t{%b2, %0|%0, %b2}";
      else if (GET_CODE (operands[2]) == CONST_INT
	       && INTVAL (operands[2]) == 1
	       && (TARGET_SHIFT1 || optimize_size))
	return "sal{w}\t%0";
      else
	return "sal{w}\t{%2, %0|%0, %2}";
    }
}
}

static const char *output_418 PARAMS ((rtx *, rtx));

static const char *
output_418 (operands, insn)
     rtx *operands ATTRIBUTE_UNUSED;
     rtx insn ATTRIBUTE_UNUSED;
{
{
  switch (get_attr_type (insn))
    {
    case TYPE_ALU:
      if (operands[2] != const1_rtx)
	abort ();
      return "add{w}\t{%0, %0|%0, %0}";

    default:
      if (REG_P (operands[2]))
	return "sal{w}\t{%b2, %0|%0, %b2}";
      else if (GET_CODE (operands[2]) == CONST_INT
	       && INTVAL (operands[2]) == 1
	       && (TARGET_SHIFT1 || optimize_size))
	return "sal{w}\t%0";
      else
	return "sal{w}\t{%2, %0|%0, %2}";
    }
}
}

static const char *output_419 PARAMS ((rtx *, rtx));

static const char *
output_419 (operands, insn)
     rtx *operands ATTRIBUTE_UNUSED;
     rtx insn ATTRIBUTE_UNUSED;
{
{
  switch (get_attr_type (insn))
    {
    case TYPE_ALU:
      if (operands[2] != const1_rtx)
	abort ();
      return "add{w}\t{%0, %0|%0, %0}";

    default:
      if (REG_P (operands[2]))
	return "sal{w}\t{%b2, %0|%0, %b2}";
      else if (GET_CODE (operands[2]) == CONST_INT
	       && INTVAL (operands[2]) == 1
	       && (TARGET_SHIFT1 || optimize_size))
	return "sal{w}\t%0";
      else
	return "sal{w}\t{%2, %0|%0, %2}";
    }
}
}

static const char *output_420 PARAMS ((rtx *, rtx));

static const char *
output_420 (operands, insn)
     rtx *operands ATTRIBUTE_UNUSED;
     rtx insn ATTRIBUTE_UNUSED;
{
{
  switch (get_attr_type (insn))
    {
    case TYPE_LEA:
      return "#";
    case TYPE_ALU:
      if (operands[2] != const1_rtx)
	abort ();
      if (REG_P (operands[1]) && !ANY_QI_REG_P (operands[1]))
        return "add{l}\t{%k0, %k0|%k0, %k0}";
      else
        return "add{b}\t{%0, %0|%0, %0}";

    default:
      if (REG_P (operands[2]))
	{
	  if (get_attr_mode (insn) == MODE_SI)
	    return "sal{l}\t{%b2, %k0|%k0, %b2}";
	  else
	    return "sal{b}\t{%b2, %0|%0, %b2}";
	}
      else if (GET_CODE (operands[2]) == CONST_INT
	       && INTVAL (operands[2]) == 1
	       && (TARGET_SHIFT1 || optimize_size))
	{
	  if (get_attr_mode (insn) == MODE_SI)
	    return "sal{l}\t%0";
	  else
	    return "sal{b}\t%0";
	}
      else
	{
	  if (get_attr_mode (insn) == MODE_SI)
	    return "sal{l}\t{%2, %k0|%k0, %2}";
	  else
	    return "sal{b}\t{%2, %0|%0, %2}";
	}
    }
}
}

static const char *output_421 PARAMS ((rtx *, rtx));

static const char *
output_421 (operands, insn)
     rtx *operands ATTRIBUTE_UNUSED;
     rtx insn ATTRIBUTE_UNUSED;
{
{
  switch (get_attr_type (insn))
    {
    case TYPE_ALU:
      if (operands[2] != const1_rtx)
	abort ();
      if (REG_P (operands[1]) && !ANY_QI_REG_P (operands[1]))
        return "add{l}\t{%k0, %k0|%k0, %k0}";
      else
        return "add{b}\t{%0, %0|%0, %0}";

    default:
      if (REG_P (operands[2]))
	{
	  if (get_attr_mode (insn) == MODE_SI)
	    return "sal{l}\t{%b2, %k0|%k0, %b2}";
	  else
	    return "sal{b}\t{%b2, %0|%0, %b2}";
	}
      else if (GET_CODE (operands[2]) == CONST_INT
	       && INTVAL (operands[2]) == 1
	       && (TARGET_SHIFT1 || optimize_size))
	{
	  if (get_attr_mode (insn) == MODE_SI)
	    return "sal{l}\t%0";
	  else
	    return "sal{b}\t%0";
	}
      else
	{
	  if (get_attr_mode (insn) == MODE_SI)
	    return "sal{l}\t{%2, %k0|%k0, %2}";
	  else
	    return "sal{b}\t{%2, %0|%0, %2}";
	}
    }
}
}

static const char *output_422 PARAMS ((rtx *, rtx));

static const char *
output_422 (operands, insn)
     rtx *operands ATTRIBUTE_UNUSED;
     rtx insn ATTRIBUTE_UNUSED;
{
{
  switch (get_attr_type (insn))
    {
    case TYPE_ALU:
      if (operands[2] != const1_rtx)
	abort ();
      return "add{b}\t{%0, %0|%0, %0}";

    default:
      if (REG_P (operands[2]))
	return "sal{b}\t{%b2, %0|%0, %b2}";
      else if (GET_CODE (operands[2]) == CONST_INT
	       && INTVAL (operands[2]) == 1
	       && (TARGET_SHIFT1 || optimize_size))
	return "sal{b}\t%0";
      else
	return "sal{b}\t{%2, %0|%0, %2}";
    }
}
}

static const char * const output_423[] = {
  "{cqto|cqo}",
  "sar{q}\t{%2, %0|%0, %2}",
};

static const char * const output_425[] = {
  "sar{q}\t{%2, %0|%0, %2}",
  "sar{q}\t{%b2, %0|%0, %b2}",
};

static const char * const output_430[] = {
  "shrd{l}\t{%2, %1, %0|%0, %1, %2}",
  "shrd{l}\t{%s2%1, %0|%0, %1, %2}",
};

static const char * const output_431[] = {
  "{cltd|cdq}",
  "sar{l}\t{%2, %0|%0, %2}",
};

static const char * const output_432[] = {
  "{cltd|cdq}",
  "sar{l}\t{%2, %k0|%k0, %2}",
};

static const char * const output_435[] = {
  "sar{l}\t{%2, %0|%0, %2}",
  "sar{l}\t{%b2, %0|%0, %b2}",
};

static const char * const output_436[] = {
  "sar{l}\t{%2, %k0|%k0, %2}",
  "sar{l}\t{%b2, %k0|%k0, %b2}",
};

static const char * const output_442[] = {
  "sar{w}\t{%2, %0|%0, %2}",
  "sar{w}\t{%b2, %0|%0, %b2}",
};

static const char * const output_447[] = {
  "sar{b}\t{%2, %0|%0, %2}",
  "sar{b}\t{%b2, %0|%0, %b2}",
};

static const char * const output_448[] = {
  "sar{b}\t{%1, %0|%0, %1}",
  "sar{b}\t{%b1, %0|%0, %b1}",
};

static const char * const output_452[] = {
  "shr{q}\t{%2, %0|%0, %2}",
  "shr{q}\t{%b2, %0|%0, %b2}",
};

static const char * const output_459[] = {
  "shr{l}\t{%2, %0|%0, %2}",
  "shr{l}\t{%b2, %0|%0, %b2}",
};

static const char * const output_460[] = {
  "shr{l}\t{%2, %k0|%k0, %2}",
  "shr{l}\t{%b2, %k0|%k0, %b2}",
};

static const char * const output_466[] = {
  "shr{w}\t{%2, %0|%0, %2}",
  "shr{w}\t{%b2, %0|%0, %b2}",
};

static const char * const output_471[] = {
  "shr{b}\t{%2, %0|%0, %2}",
  "shr{b}\t{%b2, %0|%0, %b2}",
};

static const char * const output_472[] = {
  "shr{b}\t{%1, %0|%0, %1}",
  "shr{b}\t{%b1, %0|%0, %b1}",
};

static const char * const output_476[] = {
  "rol{q}\t{%2, %0|%0, %2}",
  "rol{q}\t{%b2, %0|%0, %b2}",
};

static const char * const output_479[] = {
  "rol{l}\t{%2, %0|%0, %2}",
  "rol{l}\t{%b2, %0|%0, %b2}",
};

static const char * const output_480[] = {
  "rol{l}\t{%2, %k0|%k0, %2}",
  "rol{l}\t{%b2, %k0|%k0, %b2}",
};

static const char * const output_482[] = {
  "rol{w}\t{%2, %0|%0, %2}",
  "rol{w}\t{%b2, %0|%0, %b2}",
};

static const char * const output_485[] = {
  "rol{b}\t{%1, %0|%0, %1}",
  "rol{b}\t{%b1, %0|%0, %b1}",
};

static const char * const output_486[] = {
  "rol{b}\t{%2, %0|%0, %2}",
  "rol{b}\t{%b2, %0|%0, %b2}",
};

static const char * const output_488[] = {
  "ror{q}\t{%2, %0|%0, %2}",
  "ror{q}\t{%b2, %0|%0, %b2}",
};

static const char * const output_491[] = {
  "ror{l}\t{%2, %0|%0, %2}",
  "ror{l}\t{%b2, %0|%0, %b2}",
};

static const char * const output_492[] = {
  "ror{l}\t{%2, %k0|%k0, %2}",
  "ror{l}\t{%b2, %k0|%k0, %b2}",
};

static const char * const output_494[] = {
  "ror{w}\t{%2, %0|%0, %2}",
  "ror{w}\t{%b2, %0|%0, %b2}",
};

static const char * const output_497[] = {
  "ror{b}\t{%2, %0|%0, %2}",
  "ror{b}\t{%b2, %0|%0, %b2}",
};

static const char * const output_498[] = {
  "ror{b}\t{%1, %0|%0, %1}",
  "ror{b}\t{%b1, %0|%0, %b1}",
};

static const char *output_520 PARAMS ((rtx *, rtx));

static const char *
output_520 (operands, insn)
     rtx *operands ATTRIBUTE_UNUSED;
     rtx insn ATTRIBUTE_UNUSED;
{
{
  if (which_alternative != 0)
    return "#";
  if (get_attr_length (insn) == 2)
    return "%+loop\t%l0";
  else
    return "dec{l}\t%1\n\t%+jne\t%l0";
}
}

static const char *output_521 PARAMS ((rtx *, rtx));

static const char *
output_521 (operands, insn)
     rtx *operands ATTRIBUTE_UNUSED;
     rtx insn ATTRIBUTE_UNUSED;
{
{
  if (SIBLING_CALL_P (insn))
    return "jmp\t%P0";
  else
    return "call\t%P0";
}
}

static const char *output_522 PARAMS ((rtx *, rtx));

static const char *
output_522 (operands, insn)
     rtx *operands ATTRIBUTE_UNUSED;
     rtx insn ATTRIBUTE_UNUSED;
{
{
  if (constant_call_address_operand (operands[0], Pmode))
    {
      if (SIBLING_CALL_P (insn))
	return "jmp\t%P0";
      else
	return "call\t%P0";
    }
  if (SIBLING_CALL_P (insn))
    return "jmp\t%A0";
  else
    return "call\t%A0";
}
}

static const char *output_523 PARAMS ((rtx *, rtx));

static const char *
output_523 (operands, insn)
     rtx *operands ATTRIBUTE_UNUSED;
     rtx insn ATTRIBUTE_UNUSED;
{
{
  if (SIBLING_CALL_P (insn))
    return "jmp\t%P0";
  else
    return "call\t%P0";
}
}

static const char *output_524 PARAMS ((rtx *, rtx));

static const char *
output_524 (operands, insn)
     rtx *operands ATTRIBUTE_UNUSED;
     rtx insn ATTRIBUTE_UNUSED;
{
{
  if (constant_call_address_operand (operands[0], QImode))
    {
      if (SIBLING_CALL_P (insn))
	return "jmp\t%P0";
      else
	return "call\t%P0";
    }
  if (SIBLING_CALL_P (insn))
    return "jmp\t%A0";
  else
    return "call\t%A0";
}
}

static const char *output_525 PARAMS ((rtx *, rtx));

static const char *
output_525 (operands, insn)
     rtx *operands ATTRIBUTE_UNUSED;
     rtx insn ATTRIBUTE_UNUSED;
{
{
  if (constant_call_address_operand (operands[0], QImode))
    {
      if (SIBLING_CALL_P (insn))
	return "jmp\t%P0";
      else
	return "call\t%P0";
    }
  if (SIBLING_CALL_P (insn))
    return "jmp\t%A0";
  else
    return "call\t%A0";
}
}

static const char *output_531 PARAMS ((rtx *, rtx));

static const char *
output_531 (operands, insn)
     rtx *operands ATTRIBUTE_UNUSED;
     rtx insn ATTRIBUTE_UNUSED;
{
{ return output_set_got (operands[0]); }
}

static const char *output_548 PARAMS ((rtx *, rtx));

static const char *
output_548 (operands, insn)
     rtx *operands ATTRIBUTE_UNUSED;
     rtx insn ATTRIBUTE_UNUSED;
{
 return output_387_binary_op (insn, operands);
}

static const char *output_549 PARAMS ((rtx *, rtx));

static const char *
output_549 (operands, insn)
     rtx *operands ATTRIBUTE_UNUSED;
     rtx insn ATTRIBUTE_UNUSED;
{
 return output_387_binary_op (insn, operands);
}

static const char *output_550 PARAMS ((rtx *, rtx));

static const char *
output_550 (operands, insn)
     rtx *operands ATTRIBUTE_UNUSED;
     rtx insn ATTRIBUTE_UNUSED;
{
 return output_387_binary_op (insn, operands);
}

static const char *output_551 PARAMS ((rtx *, rtx));

static const char *
output_551 (operands, insn)
     rtx *operands ATTRIBUTE_UNUSED;
     rtx insn ATTRIBUTE_UNUSED;
{
 return output_387_binary_op (insn, operands);
}

static const char *output_552 PARAMS ((rtx *, rtx));

static const char *
output_552 (operands, insn)
     rtx *operands ATTRIBUTE_UNUSED;
     rtx insn ATTRIBUTE_UNUSED;
{
 return output_387_binary_op (insn, operands);
}

static const char *output_553 PARAMS ((rtx *, rtx));

static const char *
output_553 (operands, insn)
     rtx *operands ATTRIBUTE_UNUSED;
     rtx insn ATTRIBUTE_UNUSED;
{
 return output_387_binary_op (insn, operands);
}

static const char *output_554 PARAMS ((rtx *, rtx));

static const char *
output_554 (operands, insn)
     rtx *operands ATTRIBUTE_UNUSED;
     rtx insn ATTRIBUTE_UNUSED;
{
 return output_387_binary_op (insn, operands);
}

static const char *output_555 PARAMS ((rtx *, rtx));

static const char *
output_555 (operands, insn)
     rtx *operands ATTRIBUTE_UNUSED;
     rtx insn ATTRIBUTE_UNUSED;
{
 return output_387_binary_op (insn, operands);
}

static const char *output_556 PARAMS ((rtx *, rtx));

static const char *
output_556 (operands, insn)
     rtx *operands ATTRIBUTE_UNUSED;
     rtx insn ATTRIBUTE_UNUSED;
{
 return output_387_binary_op (insn, operands);
}

static const char *output_557 PARAMS ((rtx *, rtx));

static const char *
output_557 (operands, insn)
     rtx *operands ATTRIBUTE_UNUSED;
     rtx insn ATTRIBUTE_UNUSED;
{
 return output_387_binary_op (insn, operands);
}

static const char *output_558 PARAMS ((rtx *, rtx));

static const char *
output_558 (operands, insn)
     rtx *operands ATTRIBUTE_UNUSED;
     rtx insn ATTRIBUTE_UNUSED;
{
 return output_387_binary_op (insn, operands);
}

static const char *output_559 PARAMS ((rtx *, rtx));

static const char *
output_559 (operands, insn)
     rtx *operands ATTRIBUTE_UNUSED;
     rtx insn ATTRIBUTE_UNUSED;
{
 return which_alternative ? "#" : output_387_binary_op (insn, operands);
}

static const char *output_560 PARAMS ((rtx *, rtx));

static const char *
output_560 (operands, insn)
     rtx *operands ATTRIBUTE_UNUSED;
     rtx insn ATTRIBUTE_UNUSED;
{
 return which_alternative ? "#" : output_387_binary_op (insn, operands);
}

static const char *output_561 PARAMS ((rtx *, rtx));

static const char *
output_561 (operands, insn)
     rtx *operands ATTRIBUTE_UNUSED;
     rtx insn ATTRIBUTE_UNUSED;
{
 return output_387_binary_op (insn, operands);
}

static const char *output_562 PARAMS ((rtx *, rtx));

static const char *
output_562 (operands, insn)
     rtx *operands ATTRIBUTE_UNUSED;
     rtx insn ATTRIBUTE_UNUSED;
{
 return output_387_binary_op (insn, operands);
}

static const char *output_563 PARAMS ((rtx *, rtx));

static const char *
output_563 (operands, insn)
     rtx *operands ATTRIBUTE_UNUSED;
     rtx insn ATTRIBUTE_UNUSED;
{
 return output_387_binary_op (insn, operands);
}

static const char *output_564 PARAMS ((rtx *, rtx));

static const char *
output_564 (operands, insn)
     rtx *operands ATTRIBUTE_UNUSED;
     rtx insn ATTRIBUTE_UNUSED;
{
 return which_alternative ? "#" : output_387_binary_op (insn, operands);
}

static const char *output_565 PARAMS ((rtx *, rtx));

static const char *
output_565 (operands, insn)
     rtx *operands ATTRIBUTE_UNUSED;
     rtx insn ATTRIBUTE_UNUSED;
{
 return which_alternative ? "#" : output_387_binary_op (insn, operands);
}

static const char *output_566 PARAMS ((rtx *, rtx));

static const char *
output_566 (operands, insn)
     rtx *operands ATTRIBUTE_UNUSED;
     rtx insn ATTRIBUTE_UNUSED;
{
 return output_387_binary_op (insn, operands);
}

static const char *output_567 PARAMS ((rtx *, rtx));

static const char *
output_567 (operands, insn)
     rtx *operands ATTRIBUTE_UNUSED;
     rtx insn ATTRIBUTE_UNUSED;
{
 return output_387_binary_op (insn, operands);
}

static const char *output_568 PARAMS ((rtx *, rtx));

static const char *
output_568 (operands, insn)
     rtx *operands ATTRIBUTE_UNUSED;
     rtx insn ATTRIBUTE_UNUSED;
{
 return output_387_binary_op (insn, operands);
}

static const char *output_569 PARAMS ((rtx *, rtx));

static const char *
output_569 (operands, insn)
     rtx *operands ATTRIBUTE_UNUSED;
     rtx insn ATTRIBUTE_UNUSED;
{
 return output_387_binary_op (insn, operands);
}

static const char *output_570 PARAMS ((rtx *, rtx));

static const char *
output_570 (operands, insn)
     rtx *operands ATTRIBUTE_UNUSED;
     rtx insn ATTRIBUTE_UNUSED;
{
 return which_alternative ? "#" : output_387_binary_op (insn, operands);
}

static const char *output_571 PARAMS ((rtx *, rtx));

static const char *
output_571 (operands, insn)
     rtx *operands ATTRIBUTE_UNUSED;
     rtx insn ATTRIBUTE_UNUSED;
{
 return which_alternative ? "#" : output_387_binary_op (insn, operands);
}

static const char *output_572 PARAMS ((rtx *, rtx));

static const char *
output_572 (operands, insn)
     rtx *operands ATTRIBUTE_UNUSED;
     rtx insn ATTRIBUTE_UNUSED;
{
 return which_alternative ? "#" : output_387_binary_op (insn, operands);
}

static const char *output_573 PARAMS ((rtx *, rtx));

static const char *
output_573 (operands, insn)
     rtx *operands ATTRIBUTE_UNUSED;
     rtx insn ATTRIBUTE_UNUSED;
{
 return which_alternative ? "#" : output_387_binary_op (insn, operands);
}

static const char *output_574 PARAMS ((rtx *, rtx));

static const char *
output_574 (operands, insn)
     rtx *operands ATTRIBUTE_UNUSED;
     rtx insn ATTRIBUTE_UNUSED;
{
 return output_387_binary_op (insn, operands);
}

static const char *output_575 PARAMS ((rtx *, rtx));

static const char *
output_575 (operands, insn)
     rtx *operands ATTRIBUTE_UNUSED;
     rtx insn ATTRIBUTE_UNUSED;
{
 return output_387_binary_op (insn, operands);
}

static const char *output_576 PARAMS ((rtx *, rtx));

static const char *
output_576 (operands, insn)
     rtx *operands ATTRIBUTE_UNUSED;
     rtx insn ATTRIBUTE_UNUSED;
{
 return output_387_binary_op (insn, operands);
}

static const char *output_577 PARAMS ((rtx *, rtx));

static const char *
output_577 (operands, insn)
     rtx *operands ATTRIBUTE_UNUSED;
     rtx insn ATTRIBUTE_UNUSED;
{
 return output_387_binary_op (insn, operands);
}

static const char *output_578 PARAMS ((rtx *, rtx));

static const char *
output_578 (operands, insn)
     rtx *operands ATTRIBUTE_UNUSED;
     rtx insn ATTRIBUTE_UNUSED;
{
 return output_387_binary_op (insn, operands);
}

static const char *output_579 PARAMS ((rtx *, rtx));

static const char *
output_579 (operands, insn)
     rtx *operands ATTRIBUTE_UNUSED;
     rtx insn ATTRIBUTE_UNUSED;
{
 return output_387_binary_op (insn, operands);
}

static const char *output_580 PARAMS ((rtx *, rtx));

static const char *
output_580 (operands, insn)
     rtx *operands ATTRIBUTE_UNUSED;
     rtx insn ATTRIBUTE_UNUSED;
{
 return output_387_binary_op (insn, operands);
}

static const char *output_581 PARAMS ((rtx *, rtx));

static const char *
output_581 (operands, insn)
     rtx *operands ATTRIBUTE_UNUSED;
     rtx insn ATTRIBUTE_UNUSED;
{
 return output_387_binary_op (insn, operands);
}

static const char * const output_582[] = {
  "fsqrt",
  "sqrtss\t{%1, %0|%0, %1}",
};

static const char * const output_585[] = {
  "fsqrt",
  "sqrtsd\t{%1, %0|%0, %1}",
};

static const char * const output_637[] = {
  "cmov%O2%C1\t{%2, %0|%0, %2}",
  "cmov%O2%c1\t{%3, %0|%0, %3}",
};

static const char * const output_639[] = {
  "cmov%O2%C1\t{%2, %0|%0, %2}",
  "cmov%O2%c1\t{%3, %0|%0, %3}",
};

static const char * const output_640[] = {
  "cmov%O2%C1\t{%2, %0|%0, %2}",
  "cmov%O2%c1\t{%3, %0|%0, %3}",
};

static const char * const output_641[] = {
  "fcmov%F1\t{%2, %0|%0, %2}",
  "fcmov%f1\t{%3, %0|%0, %3}",
  "cmov%O2%C1\t{%2, %0|%0, %2}",
  "cmov%O2%c1\t{%3, %0|%0, %3}",
};

static const char * const output_642[] = {
  "fcmov%F1\t{%2, %0|%0, %2}",
  "fcmov%f1\t{%3, %0|%0, %3}",
  "#",
  "#",
};

static const char * const output_643[] = {
  "fcmov%F1\t{%2, %0|%0, %2}",
  "fcmov%f1\t{%3, %0|%0, %3}",
  "cmov%O2%C1\t{%2, %0|%0, %2}",
  "cmov%O2%c1\t{%3, %0|%0, %3}",
};

static const char * const output_644[] = {
  "fcmov%F1\t{%2, %0|%0, %2}",
  "fcmov%f1\t{%3, %0|%0, %3}",
};

static const char * const output_645[] = {
  "fcmov%F1\t{%2, %0|%0, %2}",
  "fcmov%f1\t{%3, %0|%0, %3}",
};

static const char *output_658 PARAMS ((rtx *, rtx));

static const char *
output_658 (operands, insn)
     rtx *operands ATTRIBUTE_UNUSED;
     rtx insn ATTRIBUTE_UNUSED;
{
{
  switch (get_attr_type (insn))
    {
    case TYPE_IMOV:
      return "mov{l}\t{%1, %0|%0, %1}";

    case TYPE_ALU:
      if (GET_CODE (operands[2]) == CONST_INT
          && (INTVAL (operands[2]) == 128
	      || (INTVAL (operands[2]) < 0
	          && INTVAL (operands[2]) != -128)))
	{
	  operands[2] = GEN_INT (-INTVAL (operands[2]));
	  return "sub{l}\t{%2, %0|%0, %2}";
	}
      return "add{l}\t{%2, %0|%0, %2}";

    case TYPE_LEA:
      operands[2] = SET_SRC (XVECEXP (PATTERN (insn), 0, 0));
      return "lea{l}\t{%a2, %0|%0, %a2}";

    default:
      abort ();
    }
}
}

static const char *output_659 PARAMS ((rtx *, rtx));

static const char *
output_659 (operands, insn)
     rtx *operands ATTRIBUTE_UNUSED;
     rtx insn ATTRIBUTE_UNUSED;
{
{
  switch (get_attr_type (insn))
    {
    case TYPE_IMOV:
      return "mov{q}\t{%1, %0|%0, %1}";

    case TYPE_ALU:
      if (GET_CODE (operands[2]) == CONST_INT
          && (INTVAL (operands[2]) == 128
	      || (INTVAL (operands[2]) < 0
	          && INTVAL (operands[2]) != -128)))
	{
	  operands[2] = GEN_INT (-INTVAL (operands[2]));
	  return "sub{q}\t{%2, %0|%0, %2}";
	}
      return "add{q}\t{%2, %0|%0, %2}";

    case TYPE_LEA:
      operands[2] = SET_SRC (XVECEXP (PATTERN (insn), 0, 0));
      return "lea{q}\t{%a2, %0|%0, %a2}";

    default:
      abort ();
    }
}
}

static const char *output_674 PARAMS ((rtx *, rtx));

static const char *
output_674 (operands, insn)
     rtx *operands ATTRIBUTE_UNUSED;
     rtx insn ATTRIBUTE_UNUSED;
{
{
  if (SIBLING_CALL_P (insn))
    return "jmp\t%P1";
  else
    return "call\t%P1";
}
}

static const char *output_675 PARAMS ((rtx *, rtx));

static const char *
output_675 (operands, insn)
     rtx *operands ATTRIBUTE_UNUSED;
     rtx insn ATTRIBUTE_UNUSED;
{
{
  if (constant_call_address_operand (operands[1], QImode))
    {
      if (SIBLING_CALL_P (insn))
	return "jmp\t%P1";
      else
	return "call\t%P1";
    }
  if (SIBLING_CALL_P (insn))
    return "jmp\t%A1";
  else
    return "call\t%A1";
}
}

static const char *output_676 PARAMS ((rtx *, rtx));

static const char *
output_676 (operands, insn)
     rtx *operands ATTRIBUTE_UNUSED;
     rtx insn ATTRIBUTE_UNUSED;
{
{
  if (SIBLING_CALL_P (insn))
    return "jmp\t%P1";
  else
    return "call\t%P1";
}
}

static const char *output_677 PARAMS ((rtx *, rtx));

static const char *
output_677 (operands, insn)
     rtx *operands ATTRIBUTE_UNUSED;
     rtx insn ATTRIBUTE_UNUSED;
{
{
  if (SIBLING_CALL_P (insn))
    return "jmp\t%P1";
  else
    return "call\t%P1";
}
}

static const char *output_678 PARAMS ((rtx *, rtx));

static const char *
output_678 (operands, insn)
     rtx *operands ATTRIBUTE_UNUSED;
     rtx insn ATTRIBUTE_UNUSED;
{
{
  if (constant_call_address_operand (operands[1], QImode))
    {
      if (SIBLING_CALL_P (insn))
	return "jmp\t%P1";
      else
	return "call\t%P1";
    }
  if (SIBLING_CALL_P (insn))
    return "jmp\t%*%1";
  else
    return "call\t%*%1";
}
}

static const char *output_679 PARAMS ((rtx *, rtx));

static const char *
output_679 (operands, insn)
     rtx *operands ATTRIBUTE_UNUSED;
     rtx insn ATTRIBUTE_UNUSED;
{
{
  if (constant_call_address_operand (operands[1], QImode))
    {
      if (SIBLING_CALL_P (insn))
	return "jmp\t%P1";
      else
	return "call\t%P1";
    }
  if (SIBLING_CALL_P (insn))
    return "jmp\t%A1";
  else
    return "call\t%A1";
}
}

static const char *output_681 PARAMS ((rtx *, rtx));

static const char *
output_681 (operands, insn)
     rtx *operands ATTRIBUTE_UNUSED;
     rtx insn ATTRIBUTE_UNUSED;
{
{
  operands[2] = gen_label_rtx ();
  output_asm_insn ("j%c0\t%l2\n\t int\t%1", operands);
  ASM_OUTPUT_INTERNAL_LABEL (asm_out_file, "L",
			     CODE_LABEL_NUMBER (operands[2]));
  RET;
}
}

static const char * const output_682[] = {
  "xorps\t%0, %0",
  "movaps\t{%1, %0|%0, %1}",
  "movaps\t{%1, %0|%0, %1}",
};

static const char * const output_683[] = {
  "xorps\t%0, %0",
  "movaps\t{%1, %0|%0, %1}",
  "movaps\t{%1, %0|%0, %1}",
};

static const char * const output_684[] = {
  "pxor\t%0, %0",
  "movdqa\t{%1, %0|%0, %1} ",
  "movdqa\t{%1, %0|%0, %1}",
};

static const char * const output_685[] = {
  "pxor\t%0, %0",
  "movq\t{%1, %0|%0, %1}",
  "movq\t{%1, %0|%0, %1}",
};

static const char * const output_686[] = {
  "pxor\t%0, %0",
  "movq\t{%1, %0|%0, %1}",
  "movq\t{%1, %0|%0, %1}",
};

static const char * const output_687[] = {
  "pxor\t%0, %0",
  "movq\t{%1, %0|%0, %1}",
  "movq\t{%1, %0|%0, %1}",
};

static const char * const output_688[] = {
  "pxor\t%0, %0",
  "movq\t{%1, %0|%0, %1}",
  "movq\t{%1, %0|%0, %1}",
};

static const char * const output_689[] = {
  "xorpd\t%0, %0",
  "movapd\t{%1, %0|%0, %1}",
  "movapd\t{%1, %0|%0, %1}",
};

static const char * const output_690[] = {
  "xorps\t%0, %0",
  "movaps\t{%1, %0|%0, %1}",
  "movaps\t{%1, %0|%0, %1}",
};

static const char * const output_691[] = {
  "xorps\t%0, %0",
  "movaps\t{%1, %0|%0, %1}",
  "movaps\t{%1, %0|%0, %1}",
};

static const char * const output_713[] = {
  "xorps\t%0, %0",
  "movaps\t{%1, %0|%0, %1}",
  "movaps\t{%1, %0|%0, %1}",
};

static const char * const output_714[] = {
  "#",
  "#",
  "xorps\t%0, %0",
  "movaps\t{%1, %0|%0, %1}",
  "movaps\t{%1, %0|%0, %1}",
};

static const char *output_772 PARAMS ((rtx *, rtx));

static const char *
output_772 (operands, insn)
     rtx *operands ATTRIBUTE_UNUSED;
     rtx insn ATTRIBUTE_UNUSED;
{
{
  if (GET_CODE (operands[3]) == UNORDERED)
    return "cmpordps\t{%2, %0|%0, %2}";
  else
    return "cmpn%D3ps\t{%2, %0|%0, %2}";
}
}

static const char *output_774 PARAMS ((rtx *, rtx));

static const char *
output_774 (operands, insn)
     rtx *operands ATTRIBUTE_UNUSED;
     rtx insn ATTRIBUTE_UNUSED;
{
{
  if (GET_CODE (operands[3]) == UNORDERED)
    return "cmpordss\t{%2, %0|%0, %2}";
  else
    return "cmpn%D3ss\t{%2, %0|%0, %2}";
}
}

static const char *output_854 PARAMS ((rtx *, rtx));

static const char *
output_854 (operands, insn)
     rtx *operands ATTRIBUTE_UNUSED;
     rtx insn ATTRIBUTE_UNUSED;
{

{
  int i;
  operands[0] = gen_rtx_MEM (Pmode,
			     gen_rtx_PLUS (Pmode, operands[0], operands[4]));
  output_asm_insn ("jmp\t%A1", operands);
  for (i = SSE_REGPARM_MAX - 1; i >= INTVAL (operands[2]); i--)
    {
      operands[4] = adjust_address (operands[0], DImode, i*16);
      operands[5] = gen_rtx_REG (TImode, SSE_REGNO (i));
      PUT_MODE (operands[4], TImode);
      if (GET_CODE (XEXP (operands[0], 0)) != PLUS)
        output_asm_insn ("rex", operands);
      output_asm_insn ("movaps\t{%5, %4|%4, %5}", operands);
    }
  ASM_OUTPUT_INTERNAL_LABEL (asm_out_file, "L",
			     CODE_LABEL_NUMBER (operands[3]));
  RET;
}
  
}

static const char *output_881 PARAMS ((rtx *, rtx));

static const char *
output_881 (operands, insn)
     rtx *operands ATTRIBUTE_UNUSED;
     rtx insn ATTRIBUTE_UNUSED;
{
{
  static const char * const patterns[4] = {
   "prefetchnta\t%a0", "prefetcht2\t%a0", "prefetcht1\t%a0", "prefetcht0\t%a0"
  };

  int locality = INTVAL (operands[1]);
  if (locality < 0 || locality > 3)
    abort ();

  return patterns[locality];  
}
}

static const char *output_882 PARAMS ((rtx *, rtx));

static const char *
output_882 (operands, insn)
     rtx *operands ATTRIBUTE_UNUSED;
     rtx insn ATTRIBUTE_UNUSED;
{
{
  static const char * const patterns[4] = {
   "prefetchnta\t%a0", "prefetcht2\t%a0", "prefetcht1\t%a0", "prefetcht0\t%a0"
  };

  int locality = INTVAL (operands[1]);
  if (locality < 0 || locality > 3)
    abort ();

  return patterns[locality];  
}
}

static const char *output_883 PARAMS ((rtx *, rtx));

static const char *
output_883 (operands, insn)
     rtx *operands ATTRIBUTE_UNUSED;
     rtx insn ATTRIBUTE_UNUSED;
{
{
  if (INTVAL (operands[1]) == 0)
    return "prefetch\t%a0";
  else
    return "prefetchw\t%a0";
}
}

static const char *output_884 PARAMS ((rtx *, rtx));

static const char *
output_884 (operands, insn)
     rtx *operands ATTRIBUTE_UNUSED;
     rtx insn ATTRIBUTE_UNUSED;
{
{
  if (INTVAL (operands[1]) == 0)
    return "prefetch\t%a0";
  else
    return "prefetchw\t%a0";
}
}

static const char *output_900 PARAMS ((rtx *, rtx));

static const char *
output_900 (operands, insn)
     rtx *operands ATTRIBUTE_UNUSED;
     rtx insn ATTRIBUTE_UNUSED;
{
{
  if (GET_CODE (operands[3]) == UNORDERED)
    return "cmpordps\t{%2, %0|%0, %2}";
  else
    return "cmpn%D3pd\t{%2, %0|%0, %2}";
}
}

static const char *output_902 PARAMS ((rtx *, rtx));

static const char *
output_902 (operands, insn)
     rtx *operands ATTRIBUTE_UNUSED;
     rtx insn ATTRIBUTE_UNUSED;
{
{
  if (GET_CODE (operands[3]) == UNORDERED)
    return "cmpordsd\t{%2, %0|%0, %2}";
  else
    return "cmpn%D3sd\t{%2, %0|%0, %2}";
}
}

static const char * const output_1007[] = {
  "movq\t{%1, %0|%0, %1}",
  "movdq2q\t{%1, %0|%0, %1}",
};

static const char * const output_1008[] = {
  "movq\t{%1, %0|%0, %1}",
  "movdq2q\t{%1, %0|%0, %1}",
  "movd\t{%1, %0|%0, %1}",
};

static const char * const output_1009[] = {
  "movq\t{%1, %0|%0, %1}",
  "movq2dq\t{%1, %0|%0, %1}",
};

static const char * const output_1010[] = {
  "movq\t{%1, %0|%0, %1}",
  "movq2dq\t{%1, %0|%0, %1}",
  "movd\t{%1, %0|%0, %1}",
};


extern int nonimmediate_operand PARAMS ((rtx, enum machine_mode));
extern int const0_operand PARAMS ((rtx, enum machine_mode));
extern int x86_64_general_operand PARAMS ((rtx, enum machine_mode));
extern int general_operand PARAMS ((rtx, enum machine_mode));
extern int ext_register_operand PARAMS ((rtx, enum machine_mode));
extern int register_operand PARAMS ((rtx, enum machine_mode));
extern int nonmemory_operand PARAMS ((rtx, enum machine_mode));
extern int push_operand PARAMS ((rtx, enum machine_mode));
extern int general_no_elim_operand PARAMS ((rtx, enum machine_mode));
extern int nonmemory_no_elim_operand PARAMS ((rtx, enum machine_mode));
extern int immediate_operand PARAMS ((rtx, enum machine_mode));
extern int x86_64_movabs_operand PARAMS ((rtx, enum machine_mode));
extern int q_regs_operand PARAMS ((rtx, enum machine_mode));
extern int const_int_operand PARAMS ((rtx, enum machine_mode));
extern int scratch_operand PARAMS ((rtx, enum machine_mode));
extern int memory_operand PARAMS ((rtx, enum machine_mode));
extern int no_seg_address_operand PARAMS ((rtx, enum machine_mode));
extern int index_register_operand PARAMS ((rtx, enum machine_mode));
extern int const248_operand PARAMS ((rtx, enum machine_mode));
extern int x86_64_immediate_operand PARAMS ((rtx, enum machine_mode));
extern int x86_64_szext_nonmemory_operand PARAMS ((rtx, enum machine_mode));
extern int x86_64_szext_general_operand PARAMS ((rtx, enum machine_mode));
extern int x86_64_zext_immediate_operand PARAMS ((rtx, enum machine_mode));
extern int const_int_1_31_operand PARAMS ((rtx, enum machine_mode));
extern int const_int_1_operand PARAMS ((rtx, enum machine_mode));
extern int ix86_comparison_operator PARAMS ((rtx, enum machine_mode));
extern int sse_comparison_operator PARAMS ((rtx, enum machine_mode));
extern int comparison_operator PARAMS ((rtx, enum machine_mode));
extern int constant_call_address_operand PARAMS ((rtx, enum machine_mode));
extern int call_insn_operand PARAMS ((rtx, enum machine_mode));
extern int tls_symbolic_operand PARAMS ((rtx, enum machine_mode));
extern int binary_fp_operator PARAMS ((rtx, enum machine_mode));
extern int fcmov_comparison_operator PARAMS ((rtx, enum machine_mode));
extern int vector_move_operand PARAMS ((rtx, enum machine_mode));
extern int address_operand PARAMS ((rtx, enum machine_mode));
extern int cmpsi_operand PARAMS ((rtx, enum machine_mode));
extern int cmp_fp_expander_operand PARAMS ((rtx, enum machine_mode));
extern int any_fp_register_operand PARAMS ((rtx, enum machine_mode));
extern int fp_register_operand PARAMS ((rtx, enum machine_mode));
extern int x86_64_nonmemory_operand PARAMS ((rtx, enum machine_mode));
extern int register_and_not_fp_reg_operand PARAMS ((rtx, enum machine_mode));
extern int register_and_not_any_fp_reg_operand PARAMS ((rtx, enum machine_mode));
extern int shiftdi_operand PARAMS ((rtx, enum machine_mode));
extern int aligned_operand PARAMS ((rtx, enum machine_mode));
extern int promotable_binary_operator PARAMS ((rtx, enum machine_mode));
extern int arith_or_logical_operator PARAMS ((rtx, enum machine_mode));
extern int incdec_operand PARAMS ((rtx, enum machine_mode));



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
    nonimmediate_operand,
    "r,?mr",
    DImode,
    0,
    1
  },
  {
    const0_operand,
    "n,n",
    DImode,
    0,
    1
  },
  {
    nonimmediate_operand,
    "rm,r",
    DImode,
    0,
    1
  },
  {
    x86_64_general_operand,
    "re,mr",
    DImode,
    0,
    1
  },
  {
    nonimmediate_operand,
    "mr,r",
    DImode,
    0,
    1
  },
  {
    x86_64_general_operand,
    "re,mr",
    DImode,
    0,
    1
  },
  {
    nonimmediate_operand,
    "r,?mr",
    SImode,
    0,
    1
  },
  {
    const0_operand,
    "n,n",
    SImode,
    0,
    1
  },
  {
    nonimmediate_operand,
    "rm,r",
    SImode,
    0,
    1
  },
  {
    general_operand,
    "ri,mr",
    SImode,
    0,
    1
  },
  {
    nonimmediate_operand,
    "r,?mr",
    HImode,
    0,
    1
  },
  {
    const0_operand,
    "n,n",
    HImode,
    0,
    1
  },
  {
    nonimmediate_operand,
    "rm,r",
    HImode,
    0,
    1
  },
  {
    general_operand,
    "ri,mr",
    HImode,
    0,
    1
  },
  {
    nonimmediate_operand,
    "q,?mq",
    QImode,
    0,
    1
  },
  {
    const0_operand,
    "n,n",
    QImode,
    0,
    1
  },
  {
    nonimmediate_operand,
    "qm,q",
    QImode,
    0,
    1
  },
  {
    general_operand,
    "qi,mq",
    QImode,
    0,
    1
  },
  {
    general_operand,
    "Qm",
    QImode,
    0,
    1
  },
  {
    ext_register_operand,
    "Q",
    VOIDmode,
    0,
    1
  },
  {
    register_operand,
    "Q",
    QImode,
    0,
    1
  },
  {
    ext_register_operand,
    "Q",
    VOIDmode,
    0,
    1
  },
  {
    const0_operand,
    "n",
    QImode,
    0,
    1
  },
  {
    ext_register_operand,
    "Q",
    VOIDmode,
    0,
    1
  },
  {
    general_operand,
    "Qmn",
    QImode,
    0,
    1
  },
  {
    ext_register_operand,
    "Q",
    VOIDmode,
    0,
    1
  },
  {
    nonmemory_operand,
    "Qn",
    QImode,
    0,
    1
  },
  {
    ext_register_operand,
    "Q",
    VOIDmode,
    0,
    1
  },
  {
    ext_register_operand,
    "Q",
    VOIDmode,
    0,
    1
  },
  {
    register_operand,
    "=a",
    HImode,
    0,
    1
  },
  {
    register_operand,
    "f",
    VOIDmode,
    0,
    1
  },
  {
    const0_operand,
    "X",
    VOIDmode,
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
    nonimmediate_operand,
    "fm",
    SFmode,
    0,
    1
  },
  {
    register_operand,
    "=a",
    HImode,
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
    nonimmediate_operand,
    "fm",
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
    nonimmediate_operand,
    "fm",
    DFmode,
    0,
    1
  },
  {
    register_operand,
    "=a",
    HImode,
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
    nonimmediate_operand,
    "fm",
    DFmode,
    0,
    1
  },
  {
    register_operand,
    "f",
    XFmode,
    0,
    1
  },
  {
    register_operand,
    "f",
    XFmode,
    0,
    1
  },
  {
    register_operand,
    "f",
    TFmode,
    0,
    1
  },
  {
    register_operand,
    "f",
    TFmode,
    0,
    1
  },
  {
    register_operand,
    "=a",
    HImode,
    0,
    1
  },
  {
    register_operand,
    "f",
    XFmode,
    0,
    1
  },
  {
    register_operand,
    "f",
    XFmode,
    0,
    1
  },
  {
    register_operand,
    "=a",
    HImode,
    0,
    1
  },
  {
    register_operand,
    "f",
    TFmode,
    0,
    1
  },
  {
    register_operand,
    "f",
    TFmode,
    0,
    1
  },
  {
    register_operand,
    "f",
    VOIDmode,
    0,
    1
  },
  {
    register_operand,
    "f",
    VOIDmode,
    0,
    1
  },
  {
    register_operand,
    "=a",
    HImode,
    0,
    1
  },
  {
    register_operand,
    "f",
    VOIDmode,
    0,
    1
  },
  {
    register_operand,
    "f",
    VOIDmode,
    0,
    1
  },
  {
    register_operand,
    "a",
    HImode,
    0,
    1
  },
  {
    register_operand,
    "f#x,x#f",
    VOIDmode,
    0,
    1
  },
  {
    nonimmediate_operand,
    "f#x,xm#f",
    VOIDmode,
    0,
    1
  },
  {
    register_operand,
    "x",
    VOIDmode,
    0,
    1
  },
  {
    nonimmediate_operand,
    "xm",
    VOIDmode,
    0,
    1
  },
  {
    push_operand,
    "=<",
    SImode,
    0,
    1
  },
  {
    general_no_elim_operand,
    "ri*m",
    SImode,
    0,
    1
  },
  {
    push_operand,
    "=X",
    SImode,
    0,
    1
  },
  {
    nonmemory_no_elim_operand,
    "ri",
    SImode,
    0,
    1
  },
  {
    nonimmediate_operand,
    "=r*m",
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
    const0_operand,
    "i",
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
    immediate_operand,
    "i",
    SImode,
    0,
    1
  },
  {
    nonimmediate_operand,
    "=r,m,!*y,!rm,!*y,!*Y,!*Y,!rm",
    SImode,
    0,
    1
  },
  {
    general_operand,
    "rinm,rin,rm,*y,*y,*Y,rm,*Y",
    SImode,
    0,
    1
  },
  {
    x86_64_movabs_operand,
    "i,r",
    DImode,
    0,
    1
  },
  {
    nonmemory_operand,
    "a,er",
    SImode,
    0,
    1
  },
  {
    register_operand,
    "=a,r",
    SImode,
    0,
    1
  },
  {
    x86_64_movabs_operand,
    "i,r",
    DImode,
    0,
    1
  },
  {
    register_operand,
    "+r",
    SImode,
    0,
    1
  },
  {
    register_operand,
    "+r",
    SImode,
    0,
    1
  },
  {
    push_operand,
    "=<,<",
    HImode,
    0,
    1
  },
  {
    general_no_elim_operand,
    "n,r*m",
    HImode,
    0,
    1
  },
  {
    push_operand,
    "=X",
    HImode,
    0,
    1
  },
  {
    nonmemory_no_elim_operand,
    "ri",
    HImode,
    0,
    1
  },
  {
    nonimmediate_operand,
    "=r,r,r,m",
    HImode,
    0,
    1
  },
  {
    general_operand,
    "r,rn,rm,rn",
    HImode,
    0,
    1
  },
  {
    x86_64_movabs_operand,
    "i,r",
    DImode,
    0,
    1
  },
  {
    nonmemory_operand,
    "a,er",
    HImode,
    0,
    1
  },
  {
    register_operand,
    "=a,r",
    HImode,
    0,
    1
  },
  {
    x86_64_movabs_operand,
    "i,r",
    DImode,
    0,
    1
  },
  {
    register_operand,
    "+r",
    HImode,
    0,
    1
  },
  {
    register_operand,
    "+r",
    HImode,
    0,
    1
  },
  {
    nonimmediate_operand,
    "+rm,r",
    HImode,
    1,
    1
  },
  {
    general_operand,
    "rn,m",
    HImode,
    0,
    1
  },
  {
    register_operand,
    "+r",
    HImode,
    1,
    1
  },
  {
    const0_operand,
    "i",
    HImode,
    0,
    1
  },
  {
    push_operand,
    "=X,X",
    QImode,
    0,
    1
  },
  {
    nonmemory_no_elim_operand,
    "n,r",
    QImode,
    0,
    1
  },
  {
    push_operand,
    "=X",
    QImode,
    0,
    1
  },
  {
    nonmemory_no_elim_operand,
    "qi",
    QImode,
    0,
    1
  },
  {
    nonimmediate_operand,
    "=q,q,q,r,r,?r,m",
    QImode,
    0,
    1
  },
  {
    general_operand,
    "q,qn,qm,q,rn,qm,qn",
    QImode,
    0,
    1
  },
  {
    register_operand,
    "+r",
    QImode,
    0,
    1
  },
  {
    register_operand,
    "+r",
    QImode,
    0,
    1
  },
  {
    nonimmediate_operand,
    "+qm,q",
    QImode,
    1,
    1
  },
  {
    general_operand,
    "*qn,m",
    QImode,
    0,
    1
  },
  {
    q_regs_operand,
    "+q",
    QImode,
    1,
    1
  },
  {
    const0_operand,
    "i",
    QImode,
    0,
    1
  },
  {
    register_operand,
    "=R",
    SImode,
    0,
    1
  },
  {
    ext_register_operand,
    "Q",
    VOIDmode,
    0,
    1
  },
  {
    register_operand,
    "=R",
    HImode,
    0,
    1
  },
  {
    ext_register_operand,
    "Q",
    VOIDmode,
    0,
    1
  },
  {
    nonimmediate_operand,
    "=Qm,?r",
    QImode,
    0,
    1
  },
  {
    ext_register_operand,
    "Q,Q",
    VOIDmode,
    0,
    1
  },
  {
    register_operand,
    "=Q,?R",
    QImode,
    0,
    1
  },
  {
    ext_register_operand,
    "Q,Q",
    VOIDmode,
    0,
    1
  },
  {
    x86_64_movabs_operand,
    "i,r",
    DImode,
    0,
    1
  },
  {
    nonmemory_operand,
    "a,er",
    QImode,
    0,
    1
  },
  {
    register_operand,
    "=a,r",
    QImode,
    0,
    1
  },
  {
    x86_64_movabs_operand,
    "i,r",
    DImode,
    0,
    1
  },
  {
    nonimmediate_operand,
    "=Qm,?R",
    QImode,
    0,
    1
  },
  {
    ext_register_operand,
    "Q,Q",
    VOIDmode,
    0,
    1
  },
  {
    ext_register_operand,
    "+Q",
    VOIDmode,
    0,
    1
  },
  {
    general_operand,
    "Qmn",
    SImode,
    0,
    1
  },
  {
    ext_register_operand,
    "+Q",
    VOIDmode,
    0,
    1
  },
  {
    nonmemory_operand,
    "Qn",
    SImode,
    0,
    1
  },
  {
    ext_register_operand,
    "+Q",
    VOIDmode,
    0,
    1
  },
  {
    register_operand,
    "Q",
    SImode,
    0,
    1
  },
  {
    push_operand,
    "=<",
    DImode,
    0,
    1
  },
  {
    general_no_elim_operand,
    "riF*m",
    DImode,
    0,
    1
  },
  {
    push_operand,
    "=<,!<",
    DImode,
    0,
    1
  },
  {
    general_no_elim_operand,
    "re*m,n",
    DImode,
    0,
    1
  },
  {
    push_operand,
    "=<",
    DImode,
    0,
    1
  },
  {
    general_no_elim_operand,
    "re*m",
    DImode,
    0,
    1
  },
  {
    nonimmediate_operand,
    "=r*m",
    DImode,
    0,
    1
  },
  {
    register_operand,
    "=r",
    DImode,
    0,
    1
  },
  {
    const0_operand,
    "i",
    DImode,
    0,
    1
  },
  {
    register_operand,
    "=r",
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
    nonimmediate_operand,
    "=r,o,!m*y,!*y,!m,!*Y,!*Y",
    DImode,
    0,
    1
  },
  {
    general_operand,
    "riFo,riF,*y,m,*Y,*Y,m",
    DImode,
    0,
    1
  },
  {
    nonimmediate_operand,
    "=r,r,r,mr,!mr,!m*y,!*y,!*Y,!m,!*Y",
    DImode,
    0,
    1
  },
  {
    general_operand,
    "Z,rem,i,re,n,*y,m,*Y,*Y,*m",
    DImode,
    0,
    1
  },
  {
    x86_64_movabs_operand,
    "i,r",
    DImode,
    0,
    1
  },
  {
    nonmemory_operand,
    "a,er",
    DImode,
    0,
    1
  },
  {
    register_operand,
    "=a,r",
    DImode,
    0,
    1
  },
  {
    x86_64_movabs_operand,
    "i,r",
    DImode,
    0,
    1
  },
  {
    register_operand,
    "+r",
    DImode,
    0,
    1
  },
  {
    register_operand,
    "+r",
    DImode,
    0,
    1
  },
  {
    push_operand,
    "=<,<,<",
    SFmode,
    0,
    1
  },
  {
    general_no_elim_operand,
    "f#rx,rFm#fx,x#rf",
    SFmode,
    0,
    1
  },
  {
    push_operand,
    "=X,X,X",
    SFmode,
    0,
    1
  },
  {
    nonmemory_no_elim_operand,
    "f#rx,rF#fx,x#rf",
    SFmode,
    0,
    1
  },
  {
    nonimmediate_operand,
    "=f#xr,m,f#xr,r#xf,m,x#rf,x#rf,x#rf,m,!*y,!rm,!*y",
    SFmode,
    0,
    1
  },
  {
    general_operand,
    "fm#rx,f#rx,G,rmF#fx,Fr#fx,C,x,xm#rf,x#rf,rm,*y,*y",
    SFmode,
    0,
    1
  },
  {
    register_operand,
    "+f",
    SFmode,
    0,
    1
  },
  {
    register_operand,
    "+f",
    SFmode,
    0,
    1
  },
  {
    push_operand,
    "=<,<,<,<",
    DFmode,
    0,
    1
  },
  {
    general_no_elim_operand,
    "f#Y,Fo#fY,*r#fY,Y#f",
    DFmode,
    0,
    1
  },
  {
    push_operand,
    "=<,<,<",
    DFmode,
    0,
    1
  },
  {
    general_no_elim_operand,
    "f#rY,rFo#fY,Y#rf",
    DFmode,
    0,
    1
  },
  {
    nonimmediate_operand,
    "=f#Y,m,f#Y,*r,o,Y#f,Y#f,Y#f,m",
    DFmode,
    0,
    1
  },
  {
    general_operand,
    "fm#Y,f#Y,G,*roF,F*r,C,Y#f,YHm#f,Y#f",
    DFmode,
    0,
    1
  },
  {
    nonimmediate_operand,
    "=f#Yr,m,f#Yr,r#Yf,o,Y#rf,Y#rf,Y#rf,m",
    DFmode,
    0,
    1
  },
  {
    general_operand,
    "fm#Yr,f#Yr,G,roF#Yf,Fr#Yf,C,Y#rf,Ym#rf,Y#rf",
    DFmode,
    0,
    1
  },
  {
    register_operand,
    "+f",
    DFmode,
    0,
    1
  },
  {
    register_operand,
    "+f",
    DFmode,
    0,
    1
  },
  {
    push_operand,
    "=X,X,X",
    XFmode,
    0,
    1
  },
  {
    general_no_elim_operand,
    "f,Fo,*r",
    XFmode,
    0,
    1
  },
  {
    push_operand,
    "=<,<,<",
    TFmode,
    0,
    1
  },
  {
    general_no_elim_operand,
    "f,Fo,*r",
    TFmode,
    0,
    1
  },
  {
    push_operand,
    "=<,<",
    XFmode,
    0,
    1
  },
  {
    general_no_elim_operand,
    "f#r,ro#f",
    XFmode,
    0,
    1
  },
  {
    push_operand,
    "=<,<",
    TFmode,
    0,
    1
  },
  {
    general_no_elim_operand,
    "f#r,rFo#f",
    TFmode,
    0,
    1
  },
  {
    nonimmediate_operand,
    "=f,m,f,*r,o",
    XFmode,
    0,
    1
  },
  {
    general_operand,
    "fm,f,G,*roF,F*r",
    XFmode,
    0,
    1
  },
  {
    nonimmediate_operand,
    "=f,m,f,*r,o",
    TFmode,
    0,
    1
  },
  {
    general_operand,
    "fm,f,G,*roF,F*r",
    TFmode,
    0,
    1
  },
  {
    nonimmediate_operand,
    "=f#r,m,f#r,r#f,o",
    XFmode,
    0,
    1
  },
  {
    general_operand,
    "fm#r,f#r,G,roF#f,Fr#f",
    XFmode,
    0,
    1
  },
  {
    nonimmediate_operand,
    "=f#r,m,f#r,r#f,o",
    TFmode,
    0,
    1
  },
  {
    general_operand,
    "fm#r,f#r,G,roF#f,Fr#f",
    TFmode,
    0,
    1
  },
  {
    register_operand,
    "+f",
    XFmode,
    0,
    1
  },
  {
    register_operand,
    "+f",
    XFmode,
    0,
    1
  },
  {
    register_operand,
    "+f",
    TFmode,
    0,
    1
  },
  {
    register_operand,
    "+f",
    TFmode,
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
    "0",
    HImode,
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
    nonimmediate_operand,
    "rm",
    HImode,
    0,
    1
  },
  {
    register_operand,
    "=r,?&q",
    HImode,
    0,
    1
  },
  {
    nonimmediate_operand,
    "0,qm",
    QImode,
    0,
    1
  },
  {
    register_operand,
    "=r,r",
    HImode,
    0,
    1
  },
  {
    nonimmediate_operand,
    "qm,0",
    QImode,
    0,
    1
  },
  {
    register_operand,
    "=r",
    HImode,
    0,
    1
  },
  {
    nonimmediate_operand,
    "qm",
    QImode,
    0,
    1
  },
  {
    register_operand,
    "=r,?&q",
    SImode,
    0,
    1
  },
  {
    nonimmediate_operand,
    "0,qm",
    QImode,
    0,
    1
  },
  {
    register_operand,
    "=r,r",
    SImode,
    0,
    1
  },
  {
    nonimmediate_operand,
    "qm,0",
    QImode,
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
    nonimmediate_operand,
    "qm",
    QImode,
    0,
    1
  },
  {
    nonimmediate_operand,
    "=r,?r,?*o",
    DImode,
    0,
    1
  },
  {
    nonimmediate_operand,
    "0,rm,r",
    SImode,
    0,
    1
  },
  {
    nonimmediate_operand,
    "=r,o",
    DImode,
    0,
    1
  },
  {
    nonimmediate_operand,
    "rm,0",
    SImode,
    0,
    1
  },
  {
    register_operand,
    "=r,r",
    DImode,
    0,
    1
  },
  {
    nonimmediate_operand,
    "r,m",
    HImode,
    0,
    1
  },
  {
    register_operand,
    "=r,r",
    DImode,
    0,
    1
  },
  {
    nonimmediate_operand,
    "Q,m",
    QImode,
    0,
    1
  },
  {
    nonimmediate_operand,
    "=*A,r,?r,?*o",
    DImode,
    0,
    1
  },
  {
    register_operand,
    "0,0,r,r",
    SImode,
    0,
    1
  },
  {
    scratch_operand,
    "=X,X,X,&r",
    SImode,
    0,
    0
  },
  {
    register_operand,
    "=*a,r",
    DImode,
    0,
    1
  },
  {
    nonimmediate_operand,
    "*0,rm",
    SImode,
    0,
    1
  },
  {
    register_operand,
    "=r",
    DImode,
    0,
    1
  },
  {
    nonimmediate_operand,
    "rm",
    HImode,
    0,
    1
  },
  {
    register_operand,
    "=r",
    DImode,
    0,
    1
  },
  {
    nonimmediate_operand,
    "qm",
    QImode,
    0,
    1
  },
  {
    register_operand,
    "=*a,r",
    SImode,
    0,
    1
  },
  {
    nonimmediate_operand,
    "*0,rm",
    HImode,
    0,
    1
  },
  {
    register_operand,
    "=*a,r",
    DImode,
    0,
    1
  },
  {
    nonimmediate_operand,
    "*0,rm",
    HImode,
    0,
    1
  },
  {
    register_operand,
    "=*a,r",
    HImode,
    0,
    1
  },
  {
    nonimmediate_operand,
    "*0,qm",
    QImode,
    0,
    1
  },
  {
    nonimmediate_operand,
    "=f#Y,mf#Y,Y#f",
    DFmode,
    0,
    1
  },
  {
    nonimmediate_operand,
    "fm#Y,f#Y,mY#f",
    SFmode,
    0,
    1
  },
  {
    register_operand,
    "=Y",
    DFmode,
    0,
    1
  },
  {
    nonimmediate_operand,
    "mY",
    SFmode,
    0,
    1
  },
  {
    nonimmediate_operand,
    "=f,m",
    XFmode,
    0,
    1
  },
  {
    nonimmediate_operand,
    "fm,f",
    SFmode,
    0,
    1
  },
  {
    nonimmediate_operand,
    "=f,m",
    TFmode,
    0,
    1
  },
  {
    nonimmediate_operand,
    "fm,f",
    SFmode,
    0,
    1
  },
  {
    nonimmediate_operand,
    "=f,m",
    XFmode,
    0,
    1
  },
  {
    nonimmediate_operand,
    "fm,f",
    DFmode,
    0,
    1
  },
  {
    nonimmediate_operand,
    "=f,m",
    TFmode,
    0,
    1
  },
  {
    nonimmediate_operand,
    "fm,f",
    DFmode,
    0,
    1
  },
  {
    nonimmediate_operand,
    "=m,?f#rx,?r#fx,?x#rf",
    SFmode,
    0,
    1
  },
  {
    register_operand,
    "f,f,f,f",
    DFmode,
    0,
    1
  },
  {
    memory_operand,
    "=X,m,m,m",
    SFmode,
    0,
    1
  },
  {
    nonimmediate_operand,
    "=*!m,?f#rx,?r#fx,?x#rf,Y",
    SFmode,
    0,
    1
  },
  {
    nonimmediate_operand,
    "f,f,f,f,mY",
    DFmode,
    0,
    1
  },
  {
    memory_operand,
    "=X,m,m,m,X",
    SFmode,
    0,
    1
  },
  {
    nonimmediate_operand,
    "=Y,!m",
    SFmode,
    0,
    1
  },
  {
    nonimmediate_operand,
    "mY,f",
    DFmode,
    0,
    1
  },
  {
    memory_operand,
    "=m",
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
    "=Y",
    SFmode,
    0,
    1
  },
  {
    nonimmediate_operand,
    "mY",
    DFmode,
    0,
    1
  },
  {
    nonimmediate_operand,
    "=m,?f#rx,?r#fx,?x#rf",
    SFmode,
    0,
    1
  },
  {
    register_operand,
    "f,f,f,f",
    XFmode,
    0,
    1
  },
  {
    memory_operand,
    "=X,m,m,m",
    SFmode,
    0,
    1
  },
  {
    memory_operand,
    "=m",
    SFmode,
    0,
    1
  },
  {
    register_operand,
    "f",
    XFmode,
    0,
    1
  },
  {
    nonimmediate_operand,
    "=m,?f#rx,?r#fx,?x#rf",
    SFmode,
    0,
    1
  },
  {
    register_operand,
    "f,f,f,f",
    TFmode,
    0,
    1
  },
  {
    memory_operand,
    "=X,m,m,m",
    SFmode,
    0,
    1
  },
  {
    memory_operand,
    "=m",
    SFmode,
    0,
    1
  },
  {
    register_operand,
    "f",
    TFmode,
    0,
    1
  },
  {
    nonimmediate_operand,
    "=m,?f#rY,?r#fY,?Y#rf",
    DFmode,
    0,
    1
  },
  {
    register_operand,
    "f,f,f,f",
    XFmode,
    0,
    1
  },
  {
    memory_operand,
    "=X,m,m,m",
    DFmode,
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
    register_operand,
    "f",
    XFmode,
    0,
    1
  },
  {
    nonimmediate_operand,
    "=m,?f#rY,?r#fY,?Y#rf",
    DFmode,
    0,
    1
  },
  {
    register_operand,
    "f,f,f,f",
    TFmode,
    0,
    1
  },
  {
    memory_operand,
    "=X,m,m,m",
    DFmode,
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
    register_operand,
    "f",
    TFmode,
    0,
    1
  },
  {
    nonimmediate_operand,
    "=m,?r",
    DImode,
    0,
    1
  },
  {
    register_operand,
    "f,f",
    VOIDmode,
    0,
    1
  },
  {
    memory_operand,
    "m,m",
    HImode,
    0,
    1
  },
  {
    memory_operand,
    "m,m",
    HImode,
    0,
    1
  },
  {
    memory_operand,
    "=m,m",
    DImode,
    0,
    1
  },
  {
    scratch_operand,
    "=&1f,&1f",
    DFmode,
    0,
    0
  },
  {
    memory_operand,
    "=m",
    DImode,
    0,
    1
  },
  {
    register_operand,
    "f",
    VOIDmode,
    0,
    1
  },
  {
    memory_operand,
    "m",
    HImode,
    0,
    1
  },
  {
    memory_operand,
    "m",
    HImode,
    0,
    1
  },
  {
    scratch_operand,
    "=&1f",
    DFmode,
    0,
    0
  },
  {
    register_operand,
    "=r",
    DImode,
    0,
    1
  },
  {
    nonimmediate_operand,
    "xm",
    SFmode,
    0,
    1
  },
  {
    register_operand,
    "=r",
    DImode,
    0,
    1
  },
  {
    nonimmediate_operand,
    "Ym",
    DFmode,
    0,
    1
  },
  {
    nonimmediate_operand,
    "=m,?r",
    SImode,
    0,
    1
  },
  {
    register_operand,
    "f,f",
    VOIDmode,
    0,
    1
  },
  {
    memory_operand,
    "m,m",
    HImode,
    0,
    1
  },
  {
    memory_operand,
    "m,m",
    HImode,
    0,
    1
  },
  {
    memory_operand,
    "=m,m",
    SImode,
    0,
    1
  },
  {
    memory_operand,
    "=m",
    SImode,
    0,
    1
  },
  {
    register_operand,
    "f",
    VOIDmode,
    0,
    1
  },
  {
    memory_operand,
    "m",
    HImode,
    0,
    1
  },
  {
    memory_operand,
    "m",
    HImode,
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
    nonimmediate_operand,
    "xm",
    SFmode,
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
    nonimmediate_operand,
    "Ym",
    DFmode,
    0,
    1
  },
  {
    nonimmediate_operand,
    "=m,?r",
    HImode,
    0,
    1
  },
  {
    register_operand,
    "f,f",
    VOIDmode,
    0,
    1
  },
  {
    memory_operand,
    "m,m",
    HImode,
    0,
    1
  },
  {
    memory_operand,
    "m,m",
    HImode,
    0,
    1
  },
  {
    memory_operand,
    "=m,m",
    HImode,
    0,
    1
  },
  {
    memory_operand,
    "=m",
    HImode,
    0,
    1
  },
  {
    register_operand,
    "f",
    VOIDmode,
    0,
    1
  },
  {
    memory_operand,
    "m",
    HImode,
    0,
    1
  },
  {
    memory_operand,
    "m",
    HImode,
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
    nonimmediate_operand,
    "m,r",
    HImode,
    0,
    1
  },
  {
    register_operand,
    "=f,?f,x",
    SFmode,
    0,
    1
  },
  {
    nonimmediate_operand,
    "m,r,mr",
    SImode,
    0,
    1
  },
  {
    register_operand,
    "=x",
    SFmode,
    0,
    1
  },
  {
    nonimmediate_operand,
    "mr",
    SImode,
    0,
    1
  },
  {
    register_operand,
    "=f,?f",
    SFmode,
    0,
    1
  },
  {
    nonimmediate_operand,
    "m,r",
    DImode,
    0,
    1
  },
  {
    register_operand,
    "=f,?f,x",
    SFmode,
    0,
    1
  },
  {
    nonimmediate_operand,
    "m,r,mr",
    DImode,
    0,
    1
  },
  {
    register_operand,
    "=x",
    SFmode,
    0,
    1
  },
  {
    nonimmediate_operand,
    "mr",
    DImode,
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
    nonimmediate_operand,
    "m,r",
    HImode,
    0,
    1
  },
  {
    register_operand,
    "=f,?f,Y",
    DFmode,
    0,
    1
  },
  {
    nonimmediate_operand,
    "m,r,mr",
    SImode,
    0,
    1
  },
  {
    register_operand,
    "=Y",
    DFmode,
    0,
    1
  },
  {
    nonimmediate_operand,
    "mr",
    SImode,
    0,
    1
  },
  {
    register_operand,
    "=f,?f",
    DFmode,
    0,
    1
  },
  {
    nonimmediate_operand,
    "m,r",
    DImode,
    0,
    1
  },
  {
    register_operand,
    "=f,?f,Y",
    DFmode,
    0,
    1
  },
  {
    nonimmediate_operand,
    "m,r,mr",
    DImode,
    0,
    1
  },
  {
    register_operand,
    "=Y",
    DFmode,
    0,
    1
  },
  {
    nonimmediate_operand,
    "mr",
    DImode,
    0,
    1
  },
  {
    register_operand,
    "=f,f",
    XFmode,
    0,
    1
  },
  {
    nonimmediate_operand,
    "m,r",
    HImode,
    0,
    1
  },
  {
    register_operand,
    "=f,f",
    TFmode,
    0,
    1
  },
  {
    nonimmediate_operand,
    "m,r",
    HImode,
    0,
    1
  },
  {
    register_operand,
    "=f,f",
    XFmode,
    0,
    1
  },
  {
    nonimmediate_operand,
    "m,r",
    SImode,
    0,
    1
  },
  {
    register_operand,
    "=f,f",
    TFmode,
    0,
    1
  },
  {
    nonimmediate_operand,
    "m,r",
    SImode,
    0,
    1
  },
  {
    register_operand,
    "=f,f",
    XFmode,
    0,
    1
  },
  {
    nonimmediate_operand,
    "m,r",
    DImode,
    0,
    1
  },
  {
    register_operand,
    "=f,f",
    TFmode,
    0,
    1
  },
  {
    nonimmediate_operand,
    "m,r",
    DImode,
    0,
    1
  },
  {
    nonimmediate_operand,
    "=r,o",
    DImode,
    0,
    1
  },
  {
    nonimmediate_operand,
    "%0,0",
    DImode,
    0,
    1
  },
  {
    general_operand,
    "roiF,riF",
    DImode,
    0,
    1
  },
  {
    nonimmediate_operand,
    "=rm,r",
    DImode,
    0,
    1
  },
  {
    nonimmediate_operand,
    "%0,0",
    DImode,
    0,
    1
  },
  {
    x86_64_general_operand,
    "re,rm",
    DImode,
    0,
    1
  },
  {
    nonimmediate_operand,
    "=rm,r",
    SImode,
    0,
    1
  },
  {
    nonimmediate_operand,
    "%0,0",
    SImode,
    0,
    1
  },
  {
    general_operand,
    "ri,rm",
    SImode,
    0,
    1
  },
  {
    register_operand,
    "=r",
    DImode,
    0,
    1
  },
  {
    nonimmediate_operand,
    "%0",
    SImode,
    0,
    1
  },
  {
    general_operand,
    "rim",
    SImode,
    0,
    1
  },
  {
    nonimmediate_operand,
    "=qm,q",
    QImode,
    0,
    1
  },
  {
    nonimmediate_operand,
    "%0,0",
    QImode,
    0,
    1
  },
  {
    general_operand,
    "qi,qm",
    QImode,
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
    no_seg_address_operand,
    "p",
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
    no_seg_address_operand,
    "p",
    DImode,
    0,
    1
  },
  {
    register_operand,
    "=r",
    DImode,
    0,
    1
  },
  {
    no_seg_address_operand,
    "p",
    DImode,
    0,
    1
  },
  {
    register_operand,
    "=r",
    VOIDmode,
    0,
    1
  },
  {
    index_register_operand,
    "r",
    VOIDmode,
    0,
    1
  },
  {
    register_operand,
    "r",
    VOIDmode,
    0,
    1
  },
  {
    immediate_operand,
    "i",
    VOIDmode,
    0,
    1
  },
  {
    register_operand,
    "=r",
    DImode,
    0,
    1
  },
  {
    index_register_operand,
    "r",
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
    SImode,
    0,
    1
  },
  {
    register_operand,
    "=r",
    VOIDmode,
    0,
    1
  },
  {
    index_register_operand,
    "r",
    VOIDmode,
    0,
    1
  },
  {
    const248_operand,
    "i",
    VOIDmode,
    0,
    1
  },
  {
    nonmemory_operand,
    "ri",
    VOIDmode,
    0,
    1
  },
  {
    register_operand,
    "=r",
    DImode,
    0,
    1
  },
  {
    index_register_operand,
    "r",
    SImode,
    0,
    1
  },
  {
    const248_operand,
    "n",
    SImode,
    0,
    1
  },
  {
    nonmemory_operand,
    "ri",
    SImode,
    0,
    1
  },
  {
    register_operand,
    "=r",
    VOIDmode,
    0,
    1
  },
  {
    index_register_operand,
    "r",
    VOIDmode,
    0,
    1
  },
  {
    const248_operand,
    "i",
    VOIDmode,
    0,
    1
  },
  {
    register_operand,
    "r",
    VOIDmode,
    0,
    1
  },
  {
    immediate_operand,
    "i",
    VOIDmode,
    0,
    1
  },
  {
    register_operand,
    "=r",
    DImode,
    0,
    1
  },
  {
    index_register_operand,
    "r",
    SImode,
    0,
    1
  },
  {
    const248_operand,
    "n",
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
    SImode,
    0,
    1
  },
  {
    nonimmediate_operand,
    "=r,rm,r",
    DImode,
    0,
    1
  },
  {
    nonimmediate_operand,
    "%0,0,r",
    DImode,
    0,
    1
  },
  {
    x86_64_general_operand,
    "rme,re,re",
    DImode,
    0,
    1
  },
  {
    nonimmediate_operand,
    "=r,rm",
    DImode,
    0,
    1
  },
  {
    nonimmediate_operand,
    "%0,0",
    DImode,
    0,
    1
  },
  {
    x86_64_general_operand,
    "rme,re",
    DImode,
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
    x86_64_general_operand,
    "%0",
    DImode,
    0,
    1
  },
  {
    x86_64_general_operand,
    "rme",
    DImode,
    0,
    1
  },
  {
    scratch_operand,
    "=rm",
    DImode,
    0,
    0
  },
  {
    nonimmediate_operand,
    "0",
    DImode,
    0,
    1
  },
  {
    x86_64_immediate_operand,
    "e",
    DImode,
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
    nonimmediate_operand,
    "%0",
    DImode,
    0,
    1
  },
  {
    x86_64_general_operand,
    "rme",
    DImode,
    0,
    1
  },
  {
    nonimmediate_operand,
    "=r,rm,r",
    SImode,
    0,
    1
  },
  {
    nonimmediate_operand,
    "%0,0,r",
    SImode,
    0,
    1
  },
  {
    general_operand,
    "rmni,rni,rni",
    SImode,
    0,
    1
  },
  {
    register_operand,
    "=r,r",
    DImode,
    0,
    1
  },
  {
    nonimmediate_operand,
    "%0,r",
    SImode,
    0,
    1
  },
  {
    general_operand,
    "rmni,rni",
    SImode,
    0,
    1
  },
  {
    nonimmediate_operand,
    "=r,rm",
    SImode,
    0,
    1
  },
  {
    nonimmediate_operand,
    "%0,0",
    SImode,
    0,
    1
  },
  {
    general_operand,
    "rmni,rni",
    SImode,
    0,
    1
  },
  {
    register_operand,
    "=r",
    DImode,
    0,
    1
  },
  {
    nonimmediate_operand,
    "%0",
    SImode,
    0,
    1
  },
  {
    general_operand,
    "rmni",
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
    nonimmediate_operand,
    "%0",
    SImode,
    0,
    1
  },
  {
    general_operand,
    "rmni",
    SImode,
    0,
    1
  },
  {
    scratch_operand,
    "=rm",
    SImode,
    0,
    0
  },
  {
    nonimmediate_operand,
    "0",
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
    nonimmediate_operand,
    "=rm,r,r",
    HImode,
    0,
    1
  },
  {
    nonimmediate_operand,
    "%0,0,r",
    HImode,
    0,
    1
  },
  {
    general_operand,
    "ri,rm,rni",
    HImode,
    0,
    1
  },
  {
    nonimmediate_operand,
    "=rm,r",
    HImode,
    0,
    1
  },
  {
    nonimmediate_operand,
    "%0,0",
    HImode,
    0,
    1
  },
  {
    general_operand,
    "ri,rm",
    HImode,
    0,
    1
  },
  {
    nonimmediate_operand,
    "=r,rm",
    HImode,
    0,
    1
  },
  {
    nonimmediate_operand,
    "%0,0",
    HImode,
    0,
    1
  },
  {
    general_operand,
    "rmni,rni",
    HImode,
    0,
    1
  },
  {
    scratch_operand,
    "=r",
    HImode,
    0,
    0
  },
  {
    nonimmediate_operand,
    "%0",
    HImode,
    0,
    1
  },
  {
    general_operand,
    "rmni",
    HImode,
    0,
    1
  },
  {
    scratch_operand,
    "=rm",
    HImode,
    0,
    0
  },
  {
    nonimmediate_operand,
    "0",
    HImode,
    0,
    1
  },
  {
    const_int_operand,
    "n",
    HImode,
    0,
    1
  },
  {
    nonimmediate_operand,
    "=qm,q,r,r",
    QImode,
    0,
    1
  },
  {
    nonimmediate_operand,
    "%0,0,0,r",
    QImode,
    0,
    1
  },
  {
    general_operand,
    "qn,qmn,rn,rn",
    QImode,
    0,
    1
  },
  {
    nonimmediate_operand,
    "=qm,q,r",
    QImode,
    0,
    1
  },
  {
    nonimmediate_operand,
    "%0,0,0",
    QImode,
    0,
    1
  },
  {
    general_operand,
    "qn,qmn,rn",
    QImode,
    0,
    1
  },
  {
    nonimmediate_operand,
    "+qm,q",
    QImode,
    1,
    1
  },
  {
    general_operand,
    "qn,qnm",
    QImode,
    0,
    1
  },
  {
    nonimmediate_operand,
    "=q,qm",
    QImode,
    0,
    1
  },
  {
    nonimmediate_operand,
    "%0,0",
    QImode,
    0,
    1
  },
  {
    general_operand,
    "qmni,qni",
    QImode,
    0,
    1
  },
  {
    scratch_operand,
    "=q",
    QImode,
    0,
    0
  },
  {
    nonimmediate_operand,
    "%0",
    QImode,
    0,
    1
  },
  {
    general_operand,
    "qmni",
    QImode,
    0,
    1
  },
  {
    scratch_operand,
    "=qm",
    QImode,
    0,
    0
  },
  {
    nonimmediate_operand,
    "0",
    QImode,
    0,
    1
  },
  {
    const_int_operand,
    "n",
    QImode,
    0,
    1
  },
  {
    ext_register_operand,
    "=Q",
    VOIDmode,
    0,
    1
  },
  {
    ext_register_operand,
    "0",
    VOIDmode,
    0,
    1
  },
  {
    general_operand,
    "Qmn",
    QImode,
    0,
    1
  },
  {
    ext_register_operand,
    "=Q",
    VOIDmode,
    0,
    1
  },
  {
    ext_register_operand,
    "0",
    VOIDmode,
    0,
    1
  },
  {
    nonmemory_operand,
    "Qn",
    QImode,
    0,
    1
  },
  {
    ext_register_operand,
    "=Q",
    VOIDmode,
    0,
    1
  },
  {
    ext_register_operand,
    "%0",
    VOIDmode,
    0,
    1
  },
  {
    ext_register_operand,
    "Q",
    VOIDmode,
    0,
    1
  },
  {
    nonimmediate_operand,
    "=r,o",
    DImode,
    0,
    1
  },
  {
    nonimmediate_operand,
    "0,0",
    DImode,
    0,
    1
  },
  {
    general_operand,
    "roiF,riF",
    DImode,
    0,
    1
  },
  {
    nonimmediate_operand,
    "=rm,r",
    DImode,
    0,
    1
  },
  {
    nonimmediate_operand,
    "0,0",
    DImode,
    0,
    1
  },
  {
    x86_64_general_operand,
    "re,rm",
    DImode,
    0,
    1
  },
  {
    nonimmediate_operand,
    "=rm,r",
    SImode,
    0,
    1
  },
  {
    nonimmediate_operand,
    "0,0",
    SImode,
    0,
    1
  },
  {
    general_operand,
    "ri,rm",
    SImode,
    0,
    1
  },
  {
    register_operand,
    "=rm,r",
    DImode,
    0,
    1
  },
  {
    register_operand,
    "0,0",
    SImode,
    0,
    1
  },
  {
    general_operand,
    "ri,rm",
    SImode,
    0,
    1
  },
  {
    register_operand,
    "=r",
    DImode,
    0,
    1
  },
  {
    register_operand,
    "0",
    SImode,
    0,
    1
  },
  {
    general_operand,
    "rim",
    SImode,
    0,
    1
  },
  {
    register_operand,
    "=r",
    DImode,
    0,
    1
  },
  {
    nonimmediate_operand,
    "0",
    SImode,
    0,
    1
  },
  {
    general_operand,
    "rim",
    SImode,
    0,
    1
  },
  {
    nonimmediate_operand,
    "=rm,r",
    HImode,
    0,
    1
  },
  {
    nonimmediate_operand,
    "0,0",
    HImode,
    0,
    1
  },
  {
    general_operand,
    "ri,rm",
    HImode,
    0,
    1
  },
  {
    nonimmediate_operand,
    "=qm,q",
    QImode,
    0,
    1
  },
  {
    nonimmediate_operand,
    "0,0",
    QImode,
    0,
    1
  },
  {
    general_operand,
    "qn,qmn",
    QImode,
    0,
    1
  },
  {
    nonimmediate_operand,
    "+qm,q",
    QImode,
    1,
    1
  },
  {
    general_operand,
    "qn,qmn",
    QImode,
    0,
    1
  },
  {
    nonimmediate_operand,
    "=qm,q",
    HImode,
    0,
    1
  },
  {
    nonimmediate_operand,
    "0,0",
    QImode,
    0,
    1
  },
  {
    general_operand,
    "qi,qm",
    QImode,
    0,
    1
  },
  {
    register_operand,
    "=r,r,r",
    DImode,
    0,
    1
  },
  {
    nonimmediate_operand,
    "%rm,0,0",
    DImode,
    0,
    1
  },
  {
    x86_64_general_operand,
    "K,e,mr",
    DImode,
    0,
    1
  },
  {
    register_operand,
    "=r,r,r",
    SImode,
    0,
    1
  },
  {
    nonimmediate_operand,
    "%rm,0,0",
    SImode,
    0,
    1
  },
  {
    general_operand,
    "K,i,mr",
    SImode,
    0,
    1
  },
  {
    register_operand,
    "=r,r,r",
    DImode,
    0,
    1
  },
  {
    nonimmediate_operand,
    "%rm,0,0",
    SImode,
    0,
    1
  },
  {
    general_operand,
    "K,i,mr",
    SImode,
    0,
    1
  },
  {
    register_operand,
    "=r,r,r",
    HImode,
    0,
    1
  },
  {
    nonimmediate_operand,
    "%rm,0,0",
    HImode,
    0,
    1
  },
  {
    general_operand,
    "K,i,mr",
    HImode,
    0,
    1
  },
  {
    register_operand,
    "=a",
    QImode,
    0,
    1
  },
  {
    nonimmediate_operand,
    "%0",
    QImode,
    0,
    1
  },
  {
    nonimmediate_operand,
    "qm",
    QImode,
    0,
    1
  },
  {
    register_operand,
    "=a",
    HImode,
    0,
    1
  },
  {
    nonimmediate_operand,
    "%0",
    QImode,
    0,
    1
  },
  {
    nonimmediate_operand,
    "qm",
    QImode,
    0,
    1
  },
  {
    register_operand,
    "=A",
    TImode,
    0,
    1
  },
  {
    nonimmediate_operand,
    "%0",
    DImode,
    0,
    1
  },
  {
    nonimmediate_operand,
    "rm",
    DImode,
    0,
    1
  },
  {
    register_operand,
    "=A",
    DImode,
    0,
    1
  },
  {
    nonimmediate_operand,
    "%0",
    SImode,
    0,
    1
  },
  {
    nonimmediate_operand,
    "rm",
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
    nonimmediate_operand,
    "%a",
    DImode,
    0,
    1
  },
  {
    nonimmediate_operand,
    "rm",
    DImode,
    0,
    1
  },
  {
    scratch_operand,
    "=1",
    DImode,
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
    nonimmediate_operand,
    "%a",
    SImode,
    0,
    1
  },
  {
    nonimmediate_operand,
    "rm",
    SImode,
    0,
    1
  },
  {
    scratch_operand,
    "=1",
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
    nonimmediate_operand,
    "%a",
    SImode,
    0,
    1
  },
  {
    nonimmediate_operand,
    "rm",
    SImode,
    0,
    1
  },
  {
    scratch_operand,
    "=1",
    SImode,
    0,
    0
  },
  {
    register_operand,
    "=a",
    QImode,
    0,
    1
  },
  {
    register_operand,
    "0",
    HImode,
    0,
    1
  },
  {
    nonimmediate_operand,
    "qm",
    QImode,
    0,
    1
  },
  {
    register_operand,
    "=&a,?a",
    DImode,
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
    register_operand,
    "1,0",
    DImode,
    0,
    1
  },
  {
    nonimmediate_operand,
    "rm,rm",
    DImode,
    0,
    1
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
    "=&d",
    DImode,
    0,
    1
  },
  {
    register_operand,
    "a",
    DImode,
    0,
    1
  },
  {
    nonimmediate_operand,
    "rm",
    DImode,
    0,
    1
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
    "0",
    DImode,
    0,
    1
  },
  {
    nonimmediate_operand,
    "rm",
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
    register_operand,
    "3",
    DImode,
    0,
    1
  },
  {
    register_operand,
    "=&a,?a",
    SImode,
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
    register_operand,
    "1,0",
    SImode,
    0,
    1
  },
  {
    nonimmediate_operand,
    "rm,rm",
    SImode,
    0,
    1
  },
  {
    register_operand,
    "=a",
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
    "a",
    SImode,
    0,
    1
  },
  {
    nonimmediate_operand,
    "rm",
    SImode,
    0,
    1
  },
  {
    register_operand,
    "=a",
    SImode,
    0,
    1
  },
  {
    register_operand,
    "0",
    SImode,
    0,
    1
  },
  {
    nonimmediate_operand,
    "rm",
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
    "3",
    SImode,
    0,
    1
  },
  {
    register_operand,
    "=a",
    HImode,
    0,
    1
  },
  {
    register_operand,
    "0",
    HImode,
    0,
    1
  },
  {
    nonimmediate_operand,
    "rm",
    HImode,
    0,
    1
  },
  {
    register_operand,
    "=&d",
    HImode,
    0,
    1
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
    "0",
    DImode,
    0,
    1
  },
  {
    nonimmediate_operand,
    "rm",
    DImode,
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
    "=a",
    SImode,
    0,
    1
  },
  {
    register_operand,
    "0",
    SImode,
    0,
    1
  },
  {
    nonimmediate_operand,
    "rm",
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
    "=a",
    HImode,
    0,
    1
  },
  {
    register_operand,
    "0",
    HImode,
    0,
    1
  },
  {
    nonimmediate_operand,
    "rm",
    HImode,
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
    register_operand,
    "3",
    HImode,
    0,
    1
  },
  {
    nonimmediate_operand,
    "%*a,r,*a,r,rm",
    DImode,
    0,
    1
  },
  {
    x86_64_szext_nonmemory_operand,
    "Z,Z,e,e,re",
    DImode,
    0,
    1
  },
  {
    nonimmediate_operand,
    "%*a,r,rm",
    SImode,
    0,
    1
  },
  {
    nonmemory_operand,
    "in,in,rin",
    SImode,
    0,
    1
  },
  {
    nonimmediate_operand,
    "%*a,r,rm",
    HImode,
    0,
    1
  },
  {
    nonmemory_operand,
    "n,n,rn",
    HImode,
    0,
    1
  },
  {
    nonimmediate_operand,
    "%*a,q,qm,r",
    QImode,
    0,
    1
  },
  {
    nonmemory_operand,
    "n,n,qn,n",
    QImode,
    0,
    1
  },
  {
    ext_register_operand,
    "Q",
    VOIDmode,
    0,
    1
  },
  {
    const_int_operand,
    "n",
    VOIDmode,
    0,
    1
  },
  {
    ext_register_operand,
    "Q",
    VOIDmode,
    0,
    1
  },
  {
    nonimmediate_operand,
    "Qm",
    QImode,
    0,
    1
  },
  {
    nonimmediate_operand,
    "rm",
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
    nonimmediate_operand,
    "rm",
    VOIDmode,
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
    const_int_operand,
    "",
    DImode,
    0,
    1
  },
  {
    nonimmediate_operand,
    "=r,rm,r,r",
    DImode,
    0,
    1
  },
  {
    nonimmediate_operand,
    "%0,0,0,qm",
    DImode,
    0,
    1
  },
  {
    x86_64_szext_general_operand,
    "Z,re,rm,L",
    DImode,
    0,
    1
  },
  {
    nonimmediate_operand,
    "=r,r,rm",
    DImode,
    0,
    1
  },
  {
    nonimmediate_operand,
    "%0,0,0",
    DImode,
    0,
    1
  },
  {
    x86_64_szext_general_operand,
    "Z,rem,re",
    DImode,
    0,
    1
  },
  {
    nonimmediate_operand,
    "=rm,r,r",
    SImode,
    0,
    1
  },
  {
    nonimmediate_operand,
    "%0,0,qm",
    SImode,
    0,
    1
  },
  {
    general_operand,
    "ri,rm,L",
    SImode,
    0,
    1
  },
  {
    nonimmediate_operand,
    "=r,rm",
    SImode,
    0,
    1
  },
  {
    nonimmediate_operand,
    "%0,0",
    SImode,
    0,
    1
  },
  {
    general_operand,
    "rim,ri",
    SImode,
    0,
    1
  },
  {
    nonimmediate_operand,
    "=rm,r,r",
    HImode,
    0,
    1
  },
  {
    nonimmediate_operand,
    "%0,0,qm",
    HImode,
    0,
    1
  },
  {
    general_operand,
    "ri,rm,L",
    HImode,
    0,
    1
  },
  {
    nonimmediate_operand,
    "=r,rm",
    HImode,
    0,
    1
  },
  {
    nonimmediate_operand,
    "%0,0",
    HImode,
    0,
    1
  },
  {
    general_operand,
    "rim,ri",
    HImode,
    0,
    1
  },
  {
    nonimmediate_operand,
    "=qm,q,r",
    QImode,
    0,
    1
  },
  {
    nonimmediate_operand,
    "%0,0,0",
    QImode,
    0,
    1
  },
  {
    general_operand,
    "qi,qmi,ri",
    QImode,
    0,
    1
  },
  {
    nonimmediate_operand,
    "+qm,q",
    QImode,
    1,
    1
  },
  {
    general_operand,
    "qi,qmi",
    QImode,
    0,
    1
  },
  {
    nonimmediate_operand,
    "=q,qm,*r",
    QImode,
    0,
    1
  },
  {
    nonimmediate_operand,
    "%0,0,0",
    QImode,
    0,
    1
  },
  {
    general_operand,
    "qim,qi,i",
    QImode,
    0,
    1
  },
  {
    nonimmediate_operand,
    "+q,qm",
    QImode,
    0,
    1
  },
  {
    nonimmediate_operand,
    "qmi,qi",
    QImode,
    0,
    1
  },
  {
    ext_register_operand,
    "=Q",
    VOIDmode,
    0,
    1
  },
  {
    ext_register_operand,
    "0",
    VOIDmode,
    0,
    1
  },
  {
    const_int_operand,
    "n",
    VOIDmode,
    0,
    1
  },
  {
    ext_register_operand,
    "=Q",
    VOIDmode,
    0,
    1
  },
  {
    ext_register_operand,
    "0",
    VOIDmode,
    0,
    1
  },
  {
    general_operand,
    "Qm",
    QImode,
    0,
    1
  },
  {
    ext_register_operand,
    "=Q",
    VOIDmode,
    0,
    1
  },
  {
    ext_register_operand,
    "0",
    VOIDmode,
    0,
    1
  },
  {
    ext_register_operand,
    "Q",
    VOIDmode,
    0,
    1
  },
  {
    nonimmediate_operand,
    "=rm,r",
    DImode,
    0,
    1
  },
  {
    nonimmediate_operand,
    "%0,0",
    DImode,
    0,
    1
  },
  {
    x86_64_general_operand,
    "re,rme",
    DImode,
    0,
    1
  },
  {
    nonimmediate_operand,
    "=r,rm",
    DImode,
    0,
    1
  },
  {
    nonimmediate_operand,
    "%0,0",
    DImode,
    0,
    1
  },
  {
    x86_64_general_operand,
    "rem,re",
    DImode,
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
    nonimmediate_operand,
    "%0",
    DImode,
    0,
    1
  },
  {
    x86_64_general_operand,
    "rem",
    DImode,
    0,
    1
  },
  {
    nonimmediate_operand,
    "=rm,r",
    SImode,
    0,
    1
  },
  {
    nonimmediate_operand,
    "%0,0",
    SImode,
    0,
    1
  },
  {
    general_operand,
    "ri,rmi",
    SImode,
    0,
    1
  },
  {
    register_operand,
    "=rm",
    DImode,
    0,
    1
  },
  {
    nonimmediate_operand,
    "%0",
    SImode,
    0,
    1
  },
  {
    general_operand,
    "rim",
    SImode,
    0,
    1
  },
  {
    register_operand,
    "=rm",
    DImode,
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
    x86_64_zext_immediate_operand,
    "Z",
    DImode,
    0,
    1
  },
  {
    register_operand,
    "=r",
    DImode,
    0,
    1
  },
  {
    nonimmediate_operand,
    "%0",
    SImode,
    0,
    1
  },
  {
    x86_64_zext_immediate_operand,
    "Z",
    VOIDmode,
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
    nonimmediate_operand,
    "%0",
    SImode,
    0,
    1
  },
  {
    general_operand,
    "rim",
    SImode,
    0,
    1
  },
  {
    nonimmediate_operand,
    "=r,m",
    HImode,
    0,
    1
  },
  {
    nonimmediate_operand,
    "%0,0",
    HImode,
    0,
    1
  },
  {
    general_operand,
    "rmi,ri",
    HImode,
    0,
    1
  },
  {
    scratch_operand,
    "=r",
    HImode,
    0,
    0
  },
  {
    nonimmediate_operand,
    "%0",
    HImode,
    0,
    1
  },
  {
    general_operand,
    "rim",
    HImode,
    0,
    1
  },
  {
    nonimmediate_operand,
    "=q,m,r",
    QImode,
    0,
    1
  },
  {
    nonimmediate_operand,
    "%0,0,0",
    QImode,
    0,
    1
  },
  {
    general_operand,
    "qmi,qi,ri",
    QImode,
    0,
    1
  },
  {
    nonimmediate_operand,
    "+q,m",
    QImode,
    1,
    1
  },
  {
    general_operand,
    "qmi,qi",
    QImode,
    0,
    1
  },
  {
    nonimmediate_operand,
    "=q,qm",
    QImode,
    0,
    1
  },
  {
    nonimmediate_operand,
    "%0,0",
    QImode,
    0,
    1
  },
  {
    general_operand,
    "qim,qi",
    QImode,
    0,
    1
  },
  {
    nonimmediate_operand,
    "+q,qm",
    QImode,
    0,
    1
  },
  {
    general_operand,
    "qim,qi",
    QImode,
    0,
    1
  },
  {
    scratch_operand,
    "=q",
    QImode,
    0,
    0
  },
  {
    nonimmediate_operand,
    "%0",
    QImode,
    0,
    1
  },
  {
    general_operand,
    "qim",
    QImode,
    0,
    1
  },
  {
    register_operand,
    "=r",
    DImode,
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
    x86_64_zext_immediate_operand,
    "Z",
    DImode,
    0,
    1
  },
  {
    ext_register_operand,
    "=q",
    VOIDmode,
    0,
    1
  },
  {
    ext_register_operand,
    "0",
    VOIDmode,
    0,
    1
  },
  {
    general_operand,
    "qmn",
    QImode,
    0,
    1
  },
  {
    nonimmediate_operand,
    "=ro",
    DImode,
    0,
    1
  },
  {
    general_operand,
    "0",
    DImode,
    0,
    1
  },
  {
    nonimmediate_operand,
    "=rm",
    DImode,
    0,
    1
  },
  {
    nonimmediate_operand,
    "0",
    DImode,
    0,
    1
  },
  {
    nonimmediate_operand,
    "=rm",
    SImode,
    0,
    1
  },
  {
    nonimmediate_operand,
    "0",
    SImode,
    0,
    1
  },
  {
    register_operand,
    "=r",
    DImode,
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
    nonimmediate_operand,
    "=rm",
    HImode,
    0,
    1
  },
  {
    nonimmediate_operand,
    "0",
    HImode,
    0,
    1
  },
  {
    nonimmediate_operand,
    "=qm",
    QImode,
    0,
    1
  },
  {
    nonimmediate_operand,
    "0",
    QImode,
    0,
    1
  },
  {
    memory_operand,
    "=m",
    SFmode,
    0,
    1
  },
  {
    memory_operand,
    "0",
    SFmode,
    0,
    1
  },
  {
    nonimmediate_operand,
    "=x#fr,x#fr,f#xr,rm#xf",
    SFmode,
    0,
    1
  },
  {
    nonimmediate_operand,
    "0,x#fr,0,0",
    SFmode,
    0,
    1
  },
  {
    nonmemory_operand,
    "x,0#x,*g#x,*g#x",
    SFmode,
    0,
    1
  },
  {
    nonimmediate_operand,
    "=f#r,rm#f",
    SFmode,
    0,
    1
  },
  {
    nonimmediate_operand,
    "0,0",
    SFmode,
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
    memory_operand,
    "0",
    DFmode,
    0,
    1
  },
  {
    nonimmediate_operand,
    "=Y#fr,Y#fr,f#Yr,rm#Yf",
    DFmode,
    0,
    1
  },
  {
    nonimmediate_operand,
    "0,Y#fr,0,0",
    DFmode,
    0,
    1
  },
  {
    nonmemory_operand,
    "Y,0,*g#Y,*g#Y",
    DFmode,
    0,
    1
  },
  {
    nonimmediate_operand,
    "=Y#f,Y#f,fm#Y",
    DFmode,
    0,
    1
  },
  {
    nonimmediate_operand,
    "0,Y#f,0",
    DFmode,
    0,
    1
  },
  {
    general_operand,
    "Y,0,*g#Y*r",
    DFmode,
    0,
    1
  },
  {
    nonimmediate_operand,
    "=f#r,rm#f",
    DFmode,
    0,
    1
  },
  {
    nonimmediate_operand,
    "0,0",
    DFmode,
    0,
    1
  },
  {
    nonimmediate_operand,
    "=f,mf",
    DFmode,
    0,
    1
  },
  {
    nonimmediate_operand,
    "0,0",
    DFmode,
    0,
    1
  },
  {
    nonimmediate_operand,
    "=f#r,rm#f",
    XFmode,
    0,
    1
  },
  {
    nonimmediate_operand,
    "0,0",
    XFmode,
    0,
    1
  },
  {
    nonimmediate_operand,
    "=f#r,rm#f",
    TFmode,
    0,
    1
  },
  {
    nonimmediate_operand,
    "0,0",
    TFmode,
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
    "0",
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
    "0",
    DFmode,
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
    "0",
    SFmode,
    0,
    1
  },
  {
    register_operand,
    "=f",
    XFmode,
    0,
    1
  },
  {
    register_operand,
    "0",
    XFmode,
    0,
    1
  },
  {
    register_operand,
    "=f",
    XFmode,
    0,
    1
  },
  {
    register_operand,
    "0",
    DFmode,
    0,
    1
  },
  {
    register_operand,
    "=f",
    XFmode,
    0,
    1
  },
  {
    register_operand,
    "0",
    SFmode,
    0,
    1
  },
  {
    register_operand,
    "=f",
    TFmode,
    0,
    1
  },
  {
    register_operand,
    "0",
    TFmode,
    0,
    1
  },
  {
    register_operand,
    "=f",
    TFmode,
    0,
    1
  },
  {
    register_operand,
    "0",
    DFmode,
    0,
    1
  },
  {
    register_operand,
    "=f",
    TFmode,
    0,
    1
  },
  {
    register_operand,
    "0",
    SFmode,
    0,
    1
  },
  {
    nonimmediate_operand,
    "=x#fr,f#xr,rm#xf",
    SFmode,
    0,
    1
  },
  {
    nonimmediate_operand,
    "x,0,0",
    SFmode,
    0,
    1
  },
  {
    nonmemory_operand,
    "*0#x,*g#x,*g#x",
    SFmode,
    0,
    1
  },
  {
    nonimmediate_operand,
    "=Y#fr,mf#Yr,mr#Yf",
    DFmode,
    0,
    1
  },
  {
    nonimmediate_operand,
    "Y,0,0",
    DFmode,
    0,
    1
  },
  {
    nonmemory_operand,
    "*0#Y,*g#Y,*g#Y",
    DFmode,
    0,
    1
  },
  {
    nonimmediate_operand,
    "=Y#fr,mf#Yr",
    DFmode,
    0,
    1
  },
  {
    nonimmediate_operand,
    "Y,0",
    DFmode,
    0,
    1
  },
  {
    nonmemory_operand,
    "*0#Y,*g#Y",
    DFmode,
    0,
    1
  },
  {
    nonimmediate_operand,
    "=qm,r",
    QImode,
    0,
    1
  },
  {
    nonimmediate_operand,
    "0,0",
    QImode,
    0,
    1
  },
  {
    nonimmediate_operand,
    "=rm,r",
    DImode,
    0,
    1
  },
  {
    nonimmediate_operand,
    "0,r",
    DImode,
    0,
    1
  },
  {
    nonmemory_operand,
    "cJ,M",
    QImode,
    0,
    1
  },
  {
    nonimmediate_operand,
    "=rm",
    DImode,
    0,
    1
  },
  {
    nonimmediate_operand,
    "0",
    DImode,
    0,
    1
  },
  {
    immediate_operand,
    "e",
    QImode,
    0,
    1
  },
  {
    register_operand,
    "=r",
    DImode,
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
    nonmemory_operand,
    "Jc",
    QImode,
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
    nonimmediate_operand,
    "+r*m,r*m",
    SImode,
    0,
    1
  },
  {
    register_operand,
    "r,r",
    SImode,
    0,
    1
  },
  {
    nonmemory_operand,
    "I,c",
    QImode,
    0,
    1
  },
  {
    nonimmediate_operand,
    "=rm,r",
    SImode,
    0,
    1
  },
  {
    nonimmediate_operand,
    "0,r",
    SImode,
    0,
    1
  },
  {
    nonmemory_operand,
    "cI,M",
    QImode,
    0,
    1
  },
  {
    register_operand,
    "=r,r",
    DImode,
    0,
    1
  },
  {
    register_operand,
    "0,r",
    SImode,
    0,
    1
  },
  {
    nonmemory_operand,
    "cI,M",
    QImode,
    0,
    1
  },
  {
    nonimmediate_operand,
    "=rm",
    SImode,
    0,
    1
  },
  {
    nonimmediate_operand,
    "0",
    SImode,
    0,
    1
  },
  {
    const_int_1_31_operand,
    "I",
    QImode,
    0,
    1
  },
  {
    register_operand,
    "=r",
    DImode,
    0,
    1
  },
  {
    register_operand,
    "0",
    SImode,
    0,
    1
  },
  {
    const_int_1_31_operand,
    "I",
    QImode,
    0,
    1
  },
  {
    nonimmediate_operand,
    "=rm,r",
    HImode,
    0,
    1
  },
  {
    nonimmediate_operand,
    "0,r",
    HImode,
    0,
    1
  },
  {
    nonmemory_operand,
    "cI,M",
    QImode,
    0,
    1
  },
  {
    nonimmediate_operand,
    "=rm",
    HImode,
    0,
    1
  },
  {
    nonimmediate_operand,
    "0",
    HImode,
    0,
    1
  },
  {
    nonmemory_operand,
    "cI",
    QImode,
    0,
    1
  },
  {
    nonimmediate_operand,
    "=rm",
    HImode,
    0,
    1
  },
  {
    nonimmediate_operand,
    "0",
    HImode,
    0,
    1
  },
  {
    const_int_1_31_operand,
    "I",
    QImode,
    0,
    1
  },
  {
    nonimmediate_operand,
    "=qm,r,r",
    QImode,
    0,
    1
  },
  {
    nonimmediate_operand,
    "0,0,r",
    QImode,
    0,
    1
  },
  {
    nonmemory_operand,
    "cI,cI,M",
    QImode,
    0,
    1
  },
  {
    nonimmediate_operand,
    "=qm,r",
    QImode,
    0,
    1
  },
  {
    nonimmediate_operand,
    "0,0",
    QImode,
    0,
    1
  },
  {
    nonmemory_operand,
    "cI,cI",
    QImode,
    0,
    1
  },
  {
    nonimmediate_operand,
    "=qm",
    QImode,
    0,
    1
  },
  {
    nonimmediate_operand,
    "0",
    QImode,
    0,
    1
  },
  {
    const_int_1_31_operand,
    "I",
    QImode,
    0,
    1
  },
  {
    nonimmediate_operand,
    "=*d,rm",
    DImode,
    0,
    1
  },
  {
    nonimmediate_operand,
    "*a,0",
    DImode,
    0,
    1
  },
  {
    const_int_operand,
    "i,i",
    DImode,
    0,
    1
  },
  {
    nonimmediate_operand,
    "=rm",
    DImode,
    0,
    1
  },
  {
    nonimmediate_operand,
    "0",
    DImode,
    0,
    1
  },
  {
    const_int_1_operand,
    "",
    QImode,
    0,
    1
  },
  {
    nonimmediate_operand,
    "=rm,rm",
    DImode,
    0,
    1
  },
  {
    nonimmediate_operand,
    "0,0",
    DImode,
    0,
    1
  },
  {
    nonmemory_operand,
    "J,c",
    QImode,
    0,
    1
  },
  {
    nonimmediate_operand,
    "=rm",
    DImode,
    0,
    1
  },
  {
    nonimmediate_operand,
    "0",
    DImode,
    0,
    1
  },
  {
    const_int_operand,
    "n",
    QImode,
    0,
    1
  },
  {
    nonimmediate_operand,
    "=*d,rm",
    SImode,
    0,
    1
  },
  {
    nonimmediate_operand,
    "*a,0",
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
    register_operand,
    "=*d,r",
    DImode,
    0,
    1
  },
  {
    register_operand,
    "*a,0",
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
    nonimmediate_operand,
    "=rm",
    SImode,
    0,
    1
  },
  {
    nonimmediate_operand,
    "0",
    SImode,
    0,
    1
  },
  {
    const_int_1_operand,
    "",
    QImode,
    0,
    1
  },
  {
    register_operand,
    "=r",
    DImode,
    0,
    1
  },
  {
    register_operand,
    "0",
    SImode,
    0,
    1
  },
  {
    const_int_1_operand,
    "",
    QImode,
    0,
    1
  },
  {
    nonimmediate_operand,
    "=rm,rm",
    SImode,
    0,
    1
  },
  {
    nonimmediate_operand,
    "0,0",
    SImode,
    0,
    1
  },
  {
    nonmemory_operand,
    "I,c",
    QImode,
    0,
    1
  },
  {
    register_operand,
    "=r,r",
    DImode,
    0,
    1
  },
  {
    register_operand,
    "0,0",
    SImode,
    0,
    1
  },
  {
    nonmemory_operand,
    "I,c",
    QImode,
    0,
    1
  },
  {
    nonimmediate_operand,
    "=rm",
    HImode,
    0,
    1
  },
  {
    nonimmediate_operand,
    "0",
    HImode,
    0,
    1
  },
  {
    const_int_1_operand,
    "",
    QImode,
    0,
    1
  },
  {
    nonimmediate_operand,
    "=rm,rm",
    HImode,
    0,
    1
  },
  {
    nonimmediate_operand,
    "0,0",
    HImode,
    0,
    1
  },
  {
    nonmemory_operand,
    "I,c",
    QImode,
    0,
    1
  },
  {
    nonimmediate_operand,
    "=qm",
    QImode,
    0,
    1
  },
  {
    nonimmediate_operand,
    "0",
    QImode,
    0,
    1
  },
  {
    const_int_1_operand,
    "",
    QImode,
    0,
    1
  },
  {
    nonimmediate_operand,
    "+qm",
    QImode,
    1,
    1
  },
  {
    const_int_1_operand,
    "",
    QImode,
    0,
    1
  },
  {
    nonimmediate_operand,
    "=qm,qm",
    QImode,
    0,
    1
  },
  {
    nonimmediate_operand,
    "0,0",
    QImode,
    0,
    1
  },
  {
    nonmemory_operand,
    "I,c",
    QImode,
    0,
    1
  },
  {
    nonimmediate_operand,
    "+qm,qm",
    QImode,
    1,
    1
  },
  {
    nonmemory_operand,
    "I,c",
    QImode,
    0,
    1
  },
  {
    nonimmediate_operand,
    "=qm",
    QImode,
    0,
    1
  },
  {
    nonimmediate_operand,
    "0",
    QImode,
    0,
    1
  },
  {
    const_int_1_operand,
    "I",
    QImode,
    0,
    1
  },
  {
    nonimmediate_operand,
    "=rm",
    DImode,
    0,
    1
  },
  {
    nonimmediate_operand,
    "0",
    DImode,
    0,
    1
  },
  {
    const_int_operand,
    "e",
    QImode,
    0,
    1
  },
  {
    register_operand,
    "=r,r",
    DImode,
    0,
    1
  },
  {
    nonimmediate_operand,
    "0,0",
    SImode,
    0,
    1
  },
  {
    nonmemory_operand,
    "I,c",
    QImode,
    0,
    1
  },
  {
    nonimmediate_operand,
    "=rm,rm",
    DImode,
    0,
    1
  },
  {
    nonimmediate_operand,
    "0,0",
    DImode,
    0,
    1
  },
  {
    nonmemory_operand,
    "e,c",
    QImode,
    0,
    1
  },
  {
    nonimmediate_operand,
    "=qm",
    QImode,
    0,
    1
  },
  {
    ix86_comparison_operator,
    "",
    QImode,
    0,
    0
  },
  {
    nonimmediate_operand,
    "+qm",
    QImode,
    1,
    1
  },
  {
    ix86_comparison_operator,
    "",
    QImode,
    0,
    0
  },
  {
    register_operand,
    "=x",
    SFmode,
    0,
    1
  },
  {
    sse_comparison_operator,
    "",
    SFmode,
    0,
    0
  },
  {
    register_operand,
    "0",
    SFmode,
    0,
    1
  },
  {
    nonimmediate_operand,
    "xm",
    SFmode,
    0,
    1
  },
  {
    register_operand,
    "=Y",
    DFmode,
    0,
    1
  },
  {
    sse_comparison_operator,
    "",
    DFmode,
    0,
    0
  },
  {
    register_operand,
    "0",
    DFmode,
    0,
    1
  },
  {
    nonimmediate_operand,
    "Ym",
    DFmode,
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
    ix86_comparison_operator,
    "",
    VOIDmode,
    0,
    0
  },
  {
    comparison_operator,
    "",
    VOIDmode,
    0,
    0
  },
  {
    register_operand,
    "f",
    VOIDmode,
    0,
    1
  },
  {
    register_operand,
    "f",
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
    comparison_operator,
    "",
    VOIDmode,
    0,
    0
  },
  {
    register_operand,
    "f#x,x#f",
    VOIDmode,
    0,
    1
  },
  {
    nonimmediate_operand,
    "f#x,xm#f",
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
    comparison_operator,
    "",
    VOIDmode,
    0,
    0
  },
  {
    register_operand,
    "x",
    VOIDmode,
    0,
    1
  },
  {
    nonimmediate_operand,
    "xm",
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
    comparison_operator,
    "",
    VOIDmode,
    0,
    0
  },
  {
    register_operand,
    "f",
    VOIDmode,
    0,
    1
  },
  {
    nonimmediate_operand,
    "fm",
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
    "=a",
    HImode,
    0,
    0
  },
  {
    comparison_operator,
    "",
    VOIDmode,
    0,
    0
  },
  {
    register_operand,
    "f",
    VOIDmode,
    0,
    1
  },
  {
    register_operand,
    "f",
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
    "=a",
    HImode,
    0,
    0
  },
  {
    nonimmediate_operand,
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
    nonimmediate_operand,
    "rm",
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
    "c,?*r,?*r",
    SImode,
    0,
    1
  },
  {
    register_operand,
    "=1,1,*m*r",
    SImode,
    0,
    1
  },
  {
    scratch_operand,
    "=X,X,r",
    SImode,
    0,
    0
  },
  {
    constant_call_address_operand,
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
    immediate_operand,
    "",
    SImode,
    0,
    1
  },
  {
    call_insn_operand,
    "rsm",
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
    immediate_operand,
    "i",
    SImode,
    0,
    1
  },
  {
    constant_call_address_operand,
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
    call_insn_operand,
    "rsm",
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
    call_insn_operand,
    "rsm",
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
    "c",
    SImode,
    0,
    1
  },
  {
    register_operand,
    "c",
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
    nonimmediate_operand,
    "rm",
    SImode,
    0,
    1
  },
  {
    register_operand,
    "=a",
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
    tls_symbolic_operand,
    "",
    SImode,
    0,
    1
  },
  {
    call_insn_operand,
    "",
    SImode,
    0,
    1
  },
  {
    scratch_operand,
    "=d",
    SImode,
    0,
    0
  },
  {
    scratch_operand,
    "=c",
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
    tls_symbolic_operand,
    "",
    DImode,
    0,
    1
  },
  {
    call_insn_operand,
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
    register_operand,
    "=a",
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
    call_insn_operand,
    "",
    SImode,
    0,
    1
  },
  {
    scratch_operand,
    "=d",
    SImode,
    0,
    0
  },
  {
    scratch_operand,
    "=c",
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
    call_insn_operand,
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
    register_operand,
    "=a",
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
    call_insn_operand,
    "",
    SImode,
    0,
    1
  },
  {
    tls_symbolic_operand,
    "",
    SImode,
    0,
    1
  },
  {
    scratch_operand,
    "=d",
    SImode,
    0,
    0
  },
  {
    scratch_operand,
    "=c",
    SImode,
    0,
    0
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
    "0",
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
    nonimmediate_operand,
    "%0",
    SFmode,
    0,
    1
  },
  {
    nonimmediate_operand,
    "fm",
    SFmode,
    0,
    1
  },
  {
    binary_fp_operator,
    "",
    SFmode,
    0,
    0
  },
  {
    register_operand,
    "=f#x,x#f",
    SFmode,
    0,
    1
  },
  {
    nonimmediate_operand,
    "%0,0",
    SFmode,
    0,
    1
  },
  {
    nonimmediate_operand,
    "fm#x,xm#f",
    SFmode,
    0,
    1
  },
  {
    binary_fp_operator,
    "",
    SFmode,
    0,
    0
  },
  {
    register_operand,
    "=x",
    SFmode,
    0,
    1
  },
  {
    nonimmediate_operand,
    "%0",
    SFmode,
    0,
    1
  },
  {
    nonimmediate_operand,
    "xm",
    SFmode,
    0,
    1
  },
  {
    binary_fp_operator,
    "",
    SFmode,
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
    nonimmediate_operand,
    "%0",
    DFmode,
    0,
    1
  },
  {
    nonimmediate_operand,
    "fm",
    DFmode,
    0,
    1
  },
  {
    binary_fp_operator,
    "",
    DFmode,
    0,
    0
  },
  {
    register_operand,
    "=f#Y,Y#f",
    DFmode,
    0,
    1
  },
  {
    nonimmediate_operand,
    "%0,0",
    DFmode,
    0,
    1
  },
  {
    nonimmediate_operand,
    "fm#Y,Ym#f",
    DFmode,
    0,
    1
  },
  {
    binary_fp_operator,
    "",
    DFmode,
    0,
    0
  },
  {
    register_operand,
    "=Y",
    DFmode,
    0,
    1
  },
  {
    nonimmediate_operand,
    "%0",
    DFmode,
    0,
    1
  },
  {
    nonimmediate_operand,
    "Ym",
    DFmode,
    0,
    1
  },
  {
    binary_fp_operator,
    "",
    DFmode,
    0,
    0
  },
  {
    register_operand,
    "=f",
    XFmode,
    0,
    1
  },
  {
    register_operand,
    "%0",
    XFmode,
    0,
    1
  },
  {
    register_operand,
    "f",
    XFmode,
    0,
    1
  },
  {
    binary_fp_operator,
    "",
    XFmode,
    0,
    0
  },
  {
    register_operand,
    "=f",
    TFmode,
    0,
    1
  },
  {
    register_operand,
    "%0",
    TFmode,
    0,
    1
  },
  {
    register_operand,
    "f",
    TFmode,
    0,
    1
  },
  {
    binary_fp_operator,
    "",
    TFmode,
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
    nonimmediate_operand,
    "0,fm",
    SFmode,
    0,
    1
  },
  {
    nonimmediate_operand,
    "fm,0",
    SFmode,
    0,
    1
  },
  {
    binary_fp_operator,
    "",
    SFmode,
    0,
    0
  },
  {
    register_operand,
    "=f,f,x",
    SFmode,
    0,
    1
  },
  {
    nonimmediate_operand,
    "0,fm,0",
    SFmode,
    0,
    1
  },
  {
    nonimmediate_operand,
    "fm,0,xm#f",
    SFmode,
    0,
    1
  },
  {
    binary_fp_operator,
    "",
    SFmode,
    0,
    0
  },
  {
    register_operand,
    "=x",
    SFmode,
    0,
    1
  },
  {
    register_operand,
    "0",
    SFmode,
    0,
    1
  },
  {
    nonimmediate_operand,
    "xm",
    SFmode,
    0,
    1
  },
  {
    binary_fp_operator,
    "",
    SFmode,
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
    nonimmediate_operand,
    "m,?r",
    SImode,
    0,
    1
  },
  {
    register_operand,
    "0,0",
    SFmode,
    0,
    1
  },
  {
    binary_fp_operator,
    "",
    SFmode,
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
    "0,0",
    SFmode,
    0,
    1
  },
  {
    nonimmediate_operand,
    "m,?r",
    SImode,
    0,
    1
  },
  {
    binary_fp_operator,
    "",
    SFmode,
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
    nonimmediate_operand,
    "0,fm",
    DFmode,
    0,
    1
  },
  {
    nonimmediate_operand,
    "fm,0",
    DFmode,
    0,
    1
  },
  {
    binary_fp_operator,
    "",
    DFmode,
    0,
    0
  },
  {
    register_operand,
    "=f#Y,f#Y,Y#f",
    DFmode,
    0,
    1
  },
  {
    nonimmediate_operand,
    "0,fm,0",
    DFmode,
    0,
    1
  },
  {
    nonimmediate_operand,
    "fm,0,Ym#f",
    DFmode,
    0,
    1
  },
  {
    binary_fp_operator,
    "",
    DFmode,
    0,
    0
  },
  {
    register_operand,
    "=Y",
    DFmode,
    0,
    1
  },
  {
    register_operand,
    "0",
    DFmode,
    0,
    1
  },
  {
    nonimmediate_operand,
    "Ym",
    DFmode,
    0,
    1
  },
  {
    binary_fp_operator,
    "",
    DFmode,
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
    nonimmediate_operand,
    "m,?r",
    SImode,
    0,
    1
  },
  {
    register_operand,
    "0,0",
    DFmode,
    0,
    1
  },
  {
    binary_fp_operator,
    "",
    DFmode,
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
    "0,0",
    DFmode,
    0,
    1
  },
  {
    nonimmediate_operand,
    "m,?r",
    SImode,
    0,
    1
  },
  {
    binary_fp_operator,
    "",
    DFmode,
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
    nonimmediate_operand,
    "fm,0",
    SFmode,
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
    binary_fp_operator,
    "",
    DFmode,
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
    "0,f",
    DFmode,
    0,
    1
  },
  {
    nonimmediate_operand,
    "fm,0",
    SFmode,
    0,
    1
  },
  {
    binary_fp_operator,
    "",
    DFmode,
    0,
    0
  },
  {
    register_operand,
    "=f,f",
    XFmode,
    0,
    1
  },
  {
    register_operand,
    "0,f",
    XFmode,
    0,
    1
  },
  {
    register_operand,
    "f,0",
    XFmode,
    0,
    1
  },
  {
    binary_fp_operator,
    "",
    XFmode,
    0,
    0
  },
  {
    register_operand,
    "=f,f",
    TFmode,
    0,
    1
  },
  {
    register_operand,
    "0,f",
    TFmode,
    0,
    1
  },
  {
    register_operand,
    "f,0",
    TFmode,
    0,
    1
  },
  {
    binary_fp_operator,
    "",
    TFmode,
    0,
    0
  },
  {
    register_operand,
    "=f,f",
    XFmode,
    0,
    1
  },
  {
    nonimmediate_operand,
    "m,?r",
    SImode,
    0,
    1
  },
  {
    register_operand,
    "0,0",
    XFmode,
    0,
    1
  },
  {
    binary_fp_operator,
    "",
    XFmode,
    0,
    0
  },
  {
    register_operand,
    "=f,f",
    TFmode,
    0,
    1
  },
  {
    nonimmediate_operand,
    "m,?r",
    SImode,
    0,
    1
  },
  {
    register_operand,
    "0,0",
    TFmode,
    0,
    1
  },
  {
    binary_fp_operator,
    "",
    TFmode,
    0,
    0
  },
  {
    register_operand,
    "=f,f",
    XFmode,
    0,
    1
  },
  {
    register_operand,
    "0,0",
    XFmode,
    0,
    1
  },
  {
    nonimmediate_operand,
    "m,?r",
    SImode,
    0,
    1
  },
  {
    binary_fp_operator,
    "",
    XFmode,
    0,
    0
  },
  {
    register_operand,
    "=f,f",
    TFmode,
    0,
    1
  },
  {
    register_operand,
    "0,0",
    TFmode,
    0,
    1
  },
  {
    nonimmediate_operand,
    "m,?r",
    SImode,
    0,
    1
  },
  {
    binary_fp_operator,
    "",
    TFmode,
    0,
    0
  },
  {
    register_operand,
    "=f,f",
    XFmode,
    0,
    1
  },
  {
    nonimmediate_operand,
    "fm,0",
    SFmode,
    0,
    1
  },
  {
    register_operand,
    "0,f",
    XFmode,
    0,
    1
  },
  {
    binary_fp_operator,
    "",
    XFmode,
    0,
    0
  },
  {
    register_operand,
    "=f,f",
    TFmode,
    0,
    1
  },
  {
    nonimmediate_operand,
    "fm,0",
    SFmode,
    0,
    1
  },
  {
    register_operand,
    "0,f",
    TFmode,
    0,
    1
  },
  {
    binary_fp_operator,
    "",
    TFmode,
    0,
    0
  },
  {
    register_operand,
    "=f,f",
    XFmode,
    0,
    1
  },
  {
    register_operand,
    "0,f",
    XFmode,
    0,
    1
  },
  {
    nonimmediate_operand,
    "fm,0",
    SFmode,
    0,
    1
  },
  {
    binary_fp_operator,
    "",
    XFmode,
    0,
    0
  },
  {
    register_operand,
    "=f,f",
    TFmode,
    0,
    1
  },
  {
    register_operand,
    "0,f",
    TFmode,
    0,
    1
  },
  {
    nonimmediate_operand,
    "fm,0",
    SFmode,
    0,
    1
  },
  {
    binary_fp_operator,
    "",
    TFmode,
    0,
    0
  },
  {
    register_operand,
    "=f,f",
    XFmode,
    0,
    1
  },
  {
    nonimmediate_operand,
    "fm,0",
    DFmode,
    0,
    1
  },
  {
    register_operand,
    "0,f",
    XFmode,
    0,
    1
  },
  {
    binary_fp_operator,
    "",
    XFmode,
    0,
    0
  },
  {
    register_operand,
    "=f,f",
    TFmode,
    0,
    1
  },
  {
    nonimmediate_operand,
    "fm,0",
    DFmode,
    0,
    1
  },
  {
    register_operand,
    "0,f",
    TFmode,
    0,
    1
  },
  {
    binary_fp_operator,
    "",
    TFmode,
    0,
    0
  },
  {
    register_operand,
    "=f,f",
    XFmode,
    0,
    1
  },
  {
    register_operand,
    "0,f",
    XFmode,
    0,
    1
  },
  {
    nonimmediate_operand,
    "fm,0",
    DFmode,
    0,
    1
  },
  {
    binary_fp_operator,
    "",
    XFmode,
    0,
    0
  },
  {
    register_operand,
    "=f,f",
    TFmode,
    0,
    1
  },
  {
    register_operand,
    "0,f",
    TFmode,
    0,
    1
  },
  {
    nonimmediate_operand,
    "fm,0",
    DFmode,
    0,
    1
  },
  {
    binary_fp_operator,
    "",
    TFmode,
    0,
    0
  },
  {
    register_operand,
    "=f#x,x#f",
    SFmode,
    0,
    1
  },
  {
    nonimmediate_operand,
    "0#x,xm#f",
    SFmode,
    0,
    1
  },
  {
    register_operand,
    "=x",
    SFmode,
    0,
    1
  },
  {
    nonimmediate_operand,
    "xm",
    SFmode,
    0,
    1
  },
  {
    register_operand,
    "=f#Y,Y#f",
    DFmode,
    0,
    1
  },
  {
    nonimmediate_operand,
    "0#Y,Ym#f",
    DFmode,
    0,
    1
  },
  {
    register_operand,
    "=Y",
    DFmode,
    0,
    1
  },
  {
    nonimmediate_operand,
    "Ym",
    DFmode,
    0,
    1
  },
  {
    register_operand,
    "=D",
    DImode,
    0,
    1
  },
  {
    register_operand,
    "=S",
    DImode,
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
    register_operand,
    "1",
    DImode,
    0,
    1
  },
  {
    register_operand,
    "=D",
    SImode,
    0,
    1
  },
  {
    register_operand,
    "=S",
    SImode,
    0,
    1
  },
  {
    register_operand,
    "0",
    SImode,
    0,
    1
  },
  {
    register_operand,
    "1",
    SImode,
    0,
    1
  },
  {
    register_operand,
    "=D",
    DImode,
    0,
    1
  },
  {
    register_operand,
    "=S",
    DImode,
    0,
    1
  },
  {
    register_operand,
    "=c",
    DImode,
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
    register_operand,
    "1",
    DImode,
    0,
    1
  },
  {
    register_operand,
    "2",
    DImode,
    0,
    1
  },
  {
    register_operand,
    "=D",
    SImode,
    0,
    1
  },
  {
    register_operand,
    "=S",
    SImode,
    0,
    1
  },
  {
    register_operand,
    "=c",
    SImode,
    0,
    1
  },
  {
    register_operand,
    "0",
    SImode,
    0,
    1
  },
  {
    register_operand,
    "1",
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
    register_operand,
    "=D",
    DImode,
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
    register_operand,
    "a",
    SImode,
    0,
    1
  },
  {
    register_operand,
    "=D",
    SImode,
    0,
    1
  },
  {
    register_operand,
    "0",
    SImode,
    0,
    1
  },
  {
    register_operand,
    "a",
    SImode,
    0,
    1
  },
  {
    register_operand,
    "=D",
    SImode,
    0,
    1
  },
  {
    register_operand,
    "0",
    SImode,
    0,
    1
  },
  {
    register_operand,
    "a",
    HImode,
    0,
    1
  },
  {
    register_operand,
    "=D",
    DImode,
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
    register_operand,
    "a",
    HImode,
    0,
    1
  },
  {
    register_operand,
    "=D",
    SImode,
    0,
    1
  },
  {
    register_operand,
    "0",
    SImode,
    0,
    1
  },
  {
    register_operand,
    "a",
    QImode,
    0,
    1
  },
  {
    register_operand,
    "=D",
    DImode,
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
    register_operand,
    "a",
    QImode,
    0,
    1
  },
  {
    register_operand,
    "=D",
    DImode,
    0,
    1
  },
  {
    register_operand,
    "=c",
    DImode,
    0,
    1
  },
  {
    register_operand,
    "a",
    DImode,
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
    register_operand,
    "1",
    DImode,
    0,
    1
  },
  {
    register_operand,
    "=D",
    SImode,
    0,
    1
  },
  {
    register_operand,
    "=c",
    SImode,
    0,
    1
  },
  {
    register_operand,
    "a",
    SImode,
    0,
    1
  },
  {
    register_operand,
    "0",
    SImode,
    0,
    1
  },
  {
    register_operand,
    "1",
    SImode,
    0,
    1
  },
  {
    register_operand,
    "=D",
    DImode,
    0,
    1
  },
  {
    register_operand,
    "=c",
    DImode,
    0,
    1
  },
  {
    register_operand,
    "a",
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
    register_operand,
    "1",
    DImode,
    0,
    1
  },
  {
    register_operand,
    "=D",
    SImode,
    0,
    1
  },
  {
    register_operand,
    "=c",
    SImode,
    0,
    1
  },
  {
    register_operand,
    "a",
    QImode,
    0,
    1
  },
  {
    register_operand,
    "0",
    SImode,
    0,
    1
  },
  {
    register_operand,
    "1",
    SImode,
    0,
    1
  },
  {
    register_operand,
    "=D",
    DImode,
    0,
    1
  },
  {
    register_operand,
    "=c",
    DImode,
    0,
    1
  },
  {
    register_operand,
    "a",
    QImode,
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
    register_operand,
    "1",
    DImode,
    0,
    1
  },
  {
    register_operand,
    "=S",
    SImode,
    0,
    1
  },
  {
    register_operand,
    "=D",
    SImode,
    0,
    1
  },
  {
    register_operand,
    "=c",
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
    register_operand,
    "0",
    SImode,
    0,
    1
  },
  {
    register_operand,
    "1",
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
    register_operand,
    "=S",
    DImode,
    0,
    1
  },
  {
    register_operand,
    "=D",
    DImode,
    0,
    1
  },
  {
    register_operand,
    "=c",
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
    register_operand,
    "0",
    DImode,
    0,
    1
  },
  {
    register_operand,
    "1",
    DImode,
    0,
    1
  },
  {
    register_operand,
    "2",
    DImode,
    0,
    1
  },
  {
    register_operand,
    "=&c",
    SImode,
    0,
    1
  },
  {
    register_operand,
    "=D",
    SImode,
    0,
    1
  },
  {
    register_operand,
    "a",
    QImode,
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
    register_operand,
    "0",
    SImode,
    0,
    1
  },
  {
    register_operand,
    "1",
    SImode,
    0,
    1
  },
  {
    register_operand,
    "=&c",
    DImode,
    0,
    1
  },
  {
    register_operand,
    "=D",
    DImode,
    0,
    1
  },
  {
    register_operand,
    "a",
    QImode,
    0,
    1
  },
  {
    immediate_operand,
    "i",
    DImode,
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
    register_operand,
    "1",
    DImode,
    0,
    1
  },
  {
    register_operand,
    "=r,r",
    DImode,
    0,
    1
  },
  {
    ix86_comparison_operator,
    "",
    VOIDmode,
    0,
    0
  },
  {
    nonimmediate_operand,
    "rm,0",
    DImode,
    0,
    1
  },
  {
    nonimmediate_operand,
    "0,rm",
    DImode,
    0,
    1
  },
  {
    register_operand,
    "=r,r",
    SImode,
    0,
    1
  },
  {
    ix86_comparison_operator,
    "",
    VOIDmode,
    0,
    0
  },
  {
    nonimmediate_operand,
    "rm,0",
    SImode,
    0,
    1
  },
  {
    nonimmediate_operand,
    "0,rm",
    SImode,
    0,
    1
  },
  {
    register_operand,
    "=r,r",
    HImode,
    0,
    1
  },
  {
    ix86_comparison_operator,
    "",
    VOIDmode,
    0,
    0
  },
  {
    nonimmediate_operand,
    "rm,0",
    HImode,
    0,
    1
  },
  {
    nonimmediate_operand,
    "0,rm",
    HImode,
    0,
    1
  },
  {
    register_operand,
    "=f,f,r,r",
    SFmode,
    0,
    1
  },
  {
    fcmov_comparison_operator,
    "",
    VOIDmode,
    0,
    0
  },
  {
    nonimmediate_operand,
    "f,0,rm,0",
    SFmode,
    0,
    1
  },
  {
    nonimmediate_operand,
    "0,f,0,rm",
    SFmode,
    0,
    1
  },
  {
    register_operand,
    "=f,f,&r,&r",
    DFmode,
    0,
    1
  },
  {
    fcmov_comparison_operator,
    "",
    VOIDmode,
    0,
    0
  },
  {
    nonimmediate_operand,
    "f,0,rm,0",
    DFmode,
    0,
    1
  },
  {
    nonimmediate_operand,
    "0,f,0,rm",
    DFmode,
    0,
    1
  },
  {
    register_operand,
    "=f,f",
    XFmode,
    0,
    1
  },
  {
    fcmov_comparison_operator,
    "",
    VOIDmode,
    0,
    0
  },
  {
    register_operand,
    "f,0",
    XFmode,
    0,
    1
  },
  {
    register_operand,
    "0,f",
    XFmode,
    0,
    1
  },
  {
    register_operand,
    "=f,f",
    TFmode,
    0,
    1
  },
  {
    fcmov_comparison_operator,
    "",
    VOIDmode,
    0,
    0
  },
  {
    register_operand,
    "f,0",
    TFmode,
    0,
    1
  },
  {
    register_operand,
    "0,f",
    TFmode,
    0,
    1
  },
  {
    register_operand,
    "=x#f,f#x,f#x",
    SFmode,
    0,
    1
  },
  {
    register_operand,
    "0,0,f#x",
    SFmode,
    0,
    1
  },
  {
    nonimmediate_operand,
    "xm#f,f#x,0",
    SFmode,
    0,
    1
  },
  {
    register_operand,
    "=x#f,f#x",
    SFmode,
    0,
    1
  },
  {
    nonimmediate_operand,
    "%0,0",
    SFmode,
    0,
    1
  },
  {
    nonimmediate_operand,
    "xm#f,f#x",
    SFmode,
    0,
    1
  },
  {
    register_operand,
    "=Y#f,f#Y,f#Y",
    DFmode,
    0,
    1
  },
  {
    register_operand,
    "0,0,f#Y",
    DFmode,
    0,
    1
  },
  {
    nonimmediate_operand,
    "Ym#f,f#Y,0",
    DFmode,
    0,
    1
  },
  {
    register_operand,
    "=Y#f,f#Y",
    DFmode,
    0,
    1
  },
  {
    nonimmediate_operand,
    "%0,0",
    DFmode,
    0,
    1
  },
  {
    nonimmediate_operand,
    "Ym#f,f#Y",
    DFmode,
    0,
    1
  },
  {
    register_operand,
    "=r,r",
    SImode,
    0,
    1
  },
  {
    register_operand,
    "0,r",
    SImode,
    0,
    1
  },
  {
    immediate_operand,
    "i,i",
    SImode,
    0,
    1
  },
  {
    register_operand,
    "=r,r",
    DImode,
    0,
    1
  },
  {
    register_operand,
    "0,r",
    DImode,
    0,
    1
  },
  {
    x86_64_immediate_operand,
    "e,e",
    DImode,
    0,
    1
  },
  {
    register_operand,
    "=&x#rf,x#rf,?f#xr,?f#xr,?f#xr,?f#xr,?r#xf,?r#xf,?r#xf,?r#xf",
    SFmode,
    0,
    1
  },
  {
    sse_comparison_operator,
    "",
    VOIDmode,
    0,
    0
  },
  {
    nonimmediate_operand,
    "x#fr,0#fr,f#fx,0#fx,f#fx,0#fx,rm#rx,0#rx,rm#rx,0#rx",
    SFmode,
    0,
    1
  },
  {
    nonimmediate_operand,
    "x#fr,x#fr,0#fx,f#fx,0#fx,f#fx,0#fx,rm#rx,0#rx,rm#rx",
    SFmode,
    0,
    1
  },
  {
    nonimmediate_operand,
    "0#fx,x#fx,f#x,f#x,xm#f,xm#f,f#x,f#x,xm#f,xm#f",
    SFmode,
    0,
    1
  },
  {
    nonimmediate_operand,
    "xm#f,xm#f,f#x,f#x,x#f,x#f,f#x,f#x,x#f,x#f",
    SFmode,
    0,
    1
  },
  {
    scratch_operand,
    "=2,&4,X,X,X,X,X,X,X,X",
    SFmode,
    0,
    0
  },
  {
    register_operand,
    "=&x#rf,x#rf,?f#xr,?f#xr,?r#xf,?r#xf",
    SFmode,
    0,
    1
  },
  {
    nonimmediate_operand,
    "x#fr,0#fr,0#fx,0#fx,0#rx,0#rx",
    SFmode,
    0,
    1
  },
  {
    nonimmediate_operand,
    "x#fr,x#fr,f#fx,f#fx,rm#rx,rm#rx",
    SFmode,
    0,
    1
  },
  {
    nonimmediate_operand,
    "%0#fx,x#fx,f#x,xm#f,f#x,xm#f",
    SFmode,
    0,
    1
  },
  {
    nonimmediate_operand,
    "xm#f,xm#f,f#x,x#f,f#x,x#f",
    SFmode,
    0,
    1
  },
  {
    scratch_operand,
    "=1,&3,X,X,X,X",
    SFmode,
    0,
    0
  },
  {
    register_operand,
    "=&Y#rf,Y#rf,?f#Yr,?f#Yr,?f#Yr,?f#Yr,?r#Yf,?r#Yf,?r#Yf,?r#Yf",
    DFmode,
    0,
    1
  },
  {
    sse_comparison_operator,
    "",
    VOIDmode,
    0,
    0
  },
  {
    nonimmediate_operand,
    "Y#fr,0#fr,f#fY,0#fY,f#fY,0#fY,rm#rY,0#rY,rm#rY,0#rY",
    DFmode,
    0,
    1
  },
  {
    nonimmediate_operand,
    "Y#fr,Y#fr,0#fY,f#fY,0#fY,f#fY,0#fY,rm#rY,0#rY,rm#rY",
    DFmode,
    0,
    1
  },
  {
    nonimmediate_operand,
    "0#fY,Y#fY,f#Y,f#Y,Ym#f,Ym#f,f#Y,f#Y,Ym#f,Ym#f",
    DFmode,
    0,
    1
  },
  {
    nonimmediate_operand,
    "Ym#f,Ym#f,f#Y,f#Y,Y#f,Y#f,f#Y,f#Y,Y#f,Y#f",
    DFmode,
    0,
    1
  },
  {
    scratch_operand,
    "=2,&4,X,X,X,X,X,X,X,X",
    DFmode,
    0,
    0
  },
  {
    register_operand,
    "=&Y#rf,Y#rf,?f#Yr,?f#Yr,?r#Yf,?r#Yf",
    DFmode,
    0,
    1
  },
  {
    nonimmediate_operand,
    "Y#fr,0#fr,0#fY,0#fY,0#rY,0#rY",
    DFmode,
    0,
    1
  },
  {
    nonimmediate_operand,
    "Y#fr,Y#fr,f#fY,f#fY,rm#rY,rm#rY",
    DFmode,
    0,
    1
  },
  {
    nonimmediate_operand,
    "%0#fY,Y#fY,f#Y,Ym#f,f#Y,Ym#f",
    DFmode,
    0,
    1
  },
  {
    nonimmediate_operand,
    "Ym#f,Ym#f,f#Y,Y#f,f#Y,Y#f",
    DFmode,
    0,
    1
  },
  {
    scratch_operand,
    "=1,&3,X,X,X,X",
    DFmode,
    0,
    0
  },
  {
    register_operand,
    "=&x",
    SFmode,
    0,
    1
  },
  {
    sse_comparison_operator,
    "",
    VOIDmode,
    0,
    0
  },
  {
    register_operand,
    "x",
    SFmode,
    0,
    1
  },
  {
    const0_operand,
    "X",
    SFmode,
    0,
    1
  },
  {
    register_operand,
    "0",
    SFmode,
    0,
    1
  },
  {
    nonimmediate_operand,
    "xm",
    SFmode,
    0,
    1
  },
  {
    register_operand,
    "=&x",
    SFmode,
    0,
    1
  },
  {
    sse_comparison_operator,
    "",
    VOIDmode,
    0,
    0
  },
  {
    const0_operand,
    "X",
    SFmode,
    0,
    1
  },
  {
    register_operand,
    "x",
    SFmode,
    0,
    1
  },
  {
    register_operand,
    "0",
    SFmode,
    0,
    1
  },
  {
    nonimmediate_operand,
    "xm",
    SFmode,
    0,
    1
  },
  {
    register_operand,
    "=&x",
    SFmode,
    0,
    1
  },
  {
    fcmov_comparison_operator,
    "",
    VOIDmode,
    0,
    0
  },
  {
    register_operand,
    "x",
    SFmode,
    0,
    1
  },
  {
    const0_operand,
    "X",
    SFmode,
    0,
    1
  },
  {
    nonimmediate_operand,
    "xm",
    SFmode,
    0,
    1
  },
  {
    register_operand,
    "0",
    SFmode,
    0,
    1
  },
  {
    register_operand,
    "=&x",
    SFmode,
    0,
    1
  },
  {
    fcmov_comparison_operator,
    "",
    VOIDmode,
    0,
    0
  },
  {
    const0_operand,
    "X",
    SFmode,
    0,
    1
  },
  {
    register_operand,
    "x",
    SFmode,
    0,
    1
  },
  {
    nonimmediate_operand,
    "xm",
    SFmode,
    0,
    1
  },
  {
    register_operand,
    "0",
    SFmode,
    0,
    1
  },
  {
    register_operand,
    "=&Y",
    DFmode,
    0,
    1
  },
  {
    sse_comparison_operator,
    "",
    VOIDmode,
    0,
    0
  },
  {
    register_operand,
    "Y",
    DFmode,
    0,
    1
  },
  {
    const0_operand,
    "X",
    DFmode,
    0,
    1
  },
  {
    register_operand,
    "0",
    DFmode,
    0,
    1
  },
  {
    nonimmediate_operand,
    "Ym",
    DFmode,
    0,
    1
  },
  {
    register_operand,
    "=&Y",
    DFmode,
    0,
    1
  },
  {
    sse_comparison_operator,
    "",
    VOIDmode,
    0,
    0
  },
  {
    const0_operand,
    "X",
    DFmode,
    0,
    1
  },
  {
    register_operand,
    "Y",
    DFmode,
    0,
    1
  },
  {
    register_operand,
    "0",
    DFmode,
    0,
    1
  },
  {
    nonimmediate_operand,
    "Ym",
    DFmode,
    0,
    1
  },
  {
    register_operand,
    "=&Y",
    DFmode,
    0,
    1
  },
  {
    fcmov_comparison_operator,
    "",
    VOIDmode,
    0,
    0
  },
  {
    register_operand,
    "Y",
    DFmode,
    0,
    1
  },
  {
    const0_operand,
    "X",
    DFmode,
    0,
    1
  },
  {
    nonimmediate_operand,
    "Ym",
    DFmode,
    0,
    1
  },
  {
    register_operand,
    "0",
    DFmode,
    0,
    1
  },
  {
    register_operand,
    "=&Y",
    DFmode,
    0,
    1
  },
  {
    fcmov_comparison_operator,
    "",
    VOIDmode,
    0,
    0
  },
  {
    const0_operand,
    "X",
    DFmode,
    0,
    1
  },
  {
    register_operand,
    "Y",
    DFmode,
    0,
    1
  },
  {
    nonimmediate_operand,
    "Ym",
    DFmode,
    0,
    1
  },
  {
    register_operand,
    "0",
    DFmode,
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
    constant_call_address_operand,
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
    immediate_operand,
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
    call_insn_operand,
    "rsm",
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
    immediate_operand,
    "i",
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
    constant_call_address_operand,
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
    0,
    "",
    VOIDmode,
    0,
    1
  },
  {
    call_insn_operand,
    "rsm",
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
    comparison_operator,
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
    nonimmediate_operand,
    "=x,x,m",
    V4SFmode,
    0,
    1
  },
  {
    vector_move_operand,
    "C,xm,x",
    V4SFmode,
    0,
    1
  },
  {
    nonimmediate_operand,
    "=x,x,m",
    V4SImode,
    0,
    1
  },
  {
    vector_move_operand,
    "C,xm,x",
    V4SImode,
    0,
    1
  },
  {
    nonimmediate_operand,
    "=x,x,m",
    V2DImode,
    0,
    1
  },
  {
    vector_move_operand,
    "C,xm,x",
    V2DImode,
    0,
    1
  },
  {
    nonimmediate_operand,
    "=y,y,m",
    V8QImode,
    0,
    1
  },
  {
    vector_move_operand,
    "C,ym,y",
    V8QImode,
    0,
    1
  },
  {
    nonimmediate_operand,
    "=y,y,m",
    V4HImode,
    0,
    1
  },
  {
    vector_move_operand,
    "C,ym,y",
    V4HImode,
    0,
    1
  },
  {
    nonimmediate_operand,
    "=y,y,m",
    V2SImode,
    0,
    1
  },
  {
    vector_move_operand,
    "C,ym,y",
    V2SImode,
    0,
    1
  },
  {
    nonimmediate_operand,
    "=y,y,m",
    V2SFmode,
    0,
    1
  },
  {
    vector_move_operand,
    "C,ym,y",
    V2SFmode,
    0,
    1
  },
  {
    nonimmediate_operand,
    "=x,x,m",
    V2DFmode,
    0,
    1
  },
  {
    vector_move_operand,
    "C,xm,x",
    V2DFmode,
    0,
    1
  },
  {
    nonimmediate_operand,
    "=x,x,m",
    V8HImode,
    0,
    1
  },
  {
    vector_move_operand,
    "C,xm,x",
    V8HImode,
    0,
    1
  },
  {
    nonimmediate_operand,
    "=x,x,m",
    V16QImode,
    0,
    1
  },
  {
    vector_move_operand,
    "C,xm,x",
    V16QImode,
    0,
    1
  },
  {
    push_operand,
    "=<",
    V2DFmode,
    0,
    1
  },
  {
    register_operand,
    "x",
    V2DFmode,
    0,
    1
  },
  {
    push_operand,
    "=<",
    V2DImode,
    0,
    1
  },
  {
    register_operand,
    "x",
    V2DImode,
    0,
    1
  },
  {
    push_operand,
    "=<",
    V8HImode,
    0,
    1
  },
  {
    register_operand,
    "x",
    V8HImode,
    0,
    1
  },
  {
    push_operand,
    "=<",
    V16QImode,
    0,
    1
  },
  {
    register_operand,
    "x",
    V16QImode,
    0,
    1
  },
  {
    push_operand,
    "=<",
    V4SFmode,
    0,
    1
  },
  {
    register_operand,
    "x",
    V4SFmode,
    0,
    1
  },
  {
    push_operand,
    "=<",
    V4SImode,
    0,
    1
  },
  {
    register_operand,
    "x",
    V4SImode,
    0,
    1
  },
  {
    push_operand,
    "=<",
    V2SImode,
    0,
    1
  },
  {
    register_operand,
    "y",
    V2SImode,
    0,
    1
  },
  {
    push_operand,
    "=<",
    V4HImode,
    0,
    1
  },
  {
    register_operand,
    "y",
    V4HImode,
    0,
    1
  },
  {
    push_operand,
    "=<",
    V8QImode,
    0,
    1
  },
  {
    register_operand,
    "y",
    V8QImode,
    0,
    1
  },
  {
    push_operand,
    "=<",
    V2SFmode,
    0,
    1
  },
  {
    register_operand,
    "y",
    V2SFmode,
    0,
    1
  },
  {
    push_operand,
    "=<",
    TImode,
    0,
    1
  },
  {
    nonmemory_operand,
    "x",
    TImode,
    0,
    1
  },
  {
    push_operand,
    "=<",
    V2DFmode,
    0,
    1
  },
  {
    nonmemory_operand,
    "x",
    V2DFmode,
    0,
    1
  },
  {
    push_operand,
    "=<",
    V2DImode,
    0,
    1
  },
  {
    nonmemory_operand,
    "x",
    V2DImode,
    0,
    1
  },
  {
    push_operand,
    "=<",
    V8HImode,
    0,
    1
  },
  {
    nonmemory_operand,
    "x",
    V8HImode,
    0,
    1
  },
  {
    push_operand,
    "=<",
    V16QImode,
    0,
    1
  },
  {
    nonmemory_operand,
    "x",
    V16QImode,
    0,
    1
  },
  {
    push_operand,
    "=<",
    V4SFmode,
    0,
    1
  },
  {
    nonmemory_operand,
    "x",
    V4SFmode,
    0,
    1
  },
  {
    push_operand,
    "=<",
    V4SImode,
    0,
    1
  },
  {
    nonmemory_operand,
    "x",
    V4SImode,
    0,
    1
  },
  {
    push_operand,
    "=<",
    V2SImode,
    0,
    1
  },
  {
    nonmemory_operand,
    "y",
    V2SImode,
    0,
    1
  },
  {
    push_operand,
    "=<",
    V4HImode,
    0,
    1
  },
  {
    nonmemory_operand,
    "y",
    V4HImode,
    0,
    1
  },
  {
    push_operand,
    "=<",
    V8QImode,
    0,
    1
  },
  {
    nonmemory_operand,
    "y",
    V8QImode,
    0,
    1
  },
  {
    push_operand,
    "=<",
    V2SFmode,
    0,
    1
  },
  {
    nonmemory_operand,
    "y",
    V2SFmode,
    0,
    1
  },
  {
    nonimmediate_operand,
    "=x,x,m",
    TImode,
    0,
    1
  },
  {
    vector_move_operand,
    "C,xm,x",
    TImode,
    0,
    1
  },
  {
    nonimmediate_operand,
    "=r,o,x,mx,x",
    TImode,
    0,
    1
  },
  {
    general_operand,
    "riFo,riF,C,x,m",
    TImode,
    0,
    1
  },
  {
    nonimmediate_operand,
    "=x,m",
    V4SFmode,
    0,
    1
  },
  {
    nonimmediate_operand,
    "xm,x",
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
    register_operand,
    "x",
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
    register_operand,
    "y",
    V8QImode,
    0,
    1
  },
  {
    register_operand,
    "D",
    SImode,
    0,
    1
  },
  {
    register_operand,
    "y",
    V8QImode,
    0,
    1
  },
  {
    register_operand,
    "y",
    V8QImode,
    0,
    1
  },
  {
    register_operand,
    "D",
    DImode,
    0,
    1
  },
  {
    register_operand,
    "y",
    V8QImode,
    0,
    1
  },
  {
    register_operand,
    "y",
    V8QImode,
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
    register_operand,
    "x",
    V4SFmode,
    0,
    1
  },
  {
    memory_operand,
    "=m",
    DImode,
    0,
    1
  },
  {
    register_operand,
    "y",
    DImode,
    0,
    1
  },
  {
    register_operand,
    "=x",
    V4SFmode,
    0,
    1
  },
  {
    register_operand,
    "0",
    V4SFmode,
    0,
    1
  },
  {
    register_operand,
    "x",
    V4SFmode,
    0,
    1
  },
  {
    nonimmediate_operand,
    "=x,m",
    V4SFmode,
    0,
    1
  },
  {
    nonimmediate_operand,
    "0,0",
    V4SFmode,
    0,
    1
  },
  {
    nonimmediate_operand,
    "m,x",
    V4SFmode,
    0,
    1
  },
  {
    register_operand,
    "=x",
    V4SFmode,
    0,
    1
  },
  {
    memory_operand,
    "m",
    SFmode,
    0,
    1
  },
  {
    const0_operand,
    "X",
    V4SFmode,
    0,
    1
  },
  {
    memory_operand,
    "=m",
    SFmode,
    0,
    1
  },
  {
    register_operand,
    "x",
    V4SFmode,
    0,
    1
  },
  {
    register_operand,
    "=x",
    V4SFmode,
    0,
    1
  },
  {
    register_operand,
    "0",
    V4SFmode,
    0,
    1
  },
  {
    nonimmediate_operand,
    "xm",
    V4SFmode,
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
    register_operand,
    "=x",
    V4SFmode,
    0,
    1
  },
  {
    nonimmediate_operand,
    "xm",
    V4SFmode,
    0,
    1
  },
  {
    register_operand,
    "0",
    V4SFmode,
    0,
    1
  },
  {
    register_operand,
    "=x",
    V4SFmode,
    0,
    1
  },
  {
    nonimmediate_operand,
    "%0",
    TImode,
    0,
    1
  },
  {
    nonimmediate_operand,
    "xm",
    TImode,
    0,
    1
  },
  {
    register_operand,
    "=x",
    SFmode,
    0,
    1
  },
  {
    nonimmediate_operand,
    "%0",
    TImode,
    0,
    1
  },
  {
    nonimmediate_operand,
    "xm",
    TImode,
    0,
    1
  },
  {
    register_operand,
    "=x",
    V4SFmode,
    0,
    1
  },
  {
    register_operand,
    "0",
    TImode,
    0,
    1
  },
  {
    nonimmediate_operand,
    "xm",
    TImode,
    0,
    1
  },
  {
    register_operand,
    "=x",
    SFmode,
    0,
    1
  },
  {
    register_operand,
    "0",
    TImode,
    0,
    1
  },
  {
    nonimmediate_operand,
    "xm",
    TImode,
    0,
    1
  },
  {
    register_operand,
    "=x",
    V2DFmode,
    0,
    1
  },
  {
    nonimmediate_operand,
    "%0",
    TImode,
    0,
    1
  },
  {
    nonimmediate_operand,
    "xm",
    TImode,
    0,
    1
  },
  {
    register_operand,
    "=x",
    DFmode,
    0,
    1
  },
  {
    nonimmediate_operand,
    "%0",
    TImode,
    0,
    1
  },
  {
    nonimmediate_operand,
    "xm",
    TImode,
    0,
    1
  },
  {
    register_operand,
    "=x",
    V2DFmode,
    0,
    1
  },
  {
    register_operand,
    "0",
    TImode,
    0,
    1
  },
  {
    nonimmediate_operand,
    "xm",
    TImode,
    0,
    1
  },
  {
    register_operand,
    "=Y",
    DFmode,
    0,
    1
  },
  {
    register_operand,
    "0",
    TImode,
    0,
    1
  },
  {
    nonimmediate_operand,
    "Ym",
    TImode,
    0,
    1
  },
  {
    register_operand,
    "=x",
    TImode,
    0,
    1
  },
  {
    nonimmediate_operand,
    "%0",
    TImode,
    0,
    1
  },
  {
    nonimmediate_operand,
    "xm",
    TImode,
    0,
    1
  },
  {
    register_operand,
    "=x",
    V2DImode,
    0,
    1
  },
  {
    nonimmediate_operand,
    "%0",
    V2DImode,
    0,
    1
  },
  {
    nonimmediate_operand,
    "xm",
    V2DImode,
    0,
    1
  },
  {
    register_operand,
    "=x",
    TImode,
    0,
    1
  },
  {
    register_operand,
    "0",
    TImode,
    0,
    1
  },
  {
    nonimmediate_operand,
    "xm",
    TImode,
    0,
    1
  },
  {
    register_operand,
    "=x",
    V2DImode,
    0,
    1
  },
  {
    nonimmediate_operand,
    "0",
    V2DImode,
    0,
    1
  },
  {
    nonimmediate_operand,
    "xm",
    V2DImode,
    0,
    1
  },
  {
    register_operand,
    "=x",
    V4SImode,
    0,
    1
  },
  {
    register_operand,
    "0",
    V4SFmode,
    0,
    1
  },
  {
    register_operand,
    "x",
    V4SFmode,
    0,
    1
  },
  {
    sse_comparison_operator,
    "",
    V4SImode,
    0,
    0
  },
  {
    register_operand,
    "x",
    V4SFmode,
    0,
    1
  },
  {
    register_operand,
    "x",
    V4SFmode,
    0,
    1
  },
  {
    register_operand,
    "=x",
    V4SFmode,
    0,
    1
  },
  {
    register_operand,
    "0",
    V4SFmode,
    0,
    1
  },
  {
    nonimmediate_operand,
    "ym",
    V2SImode,
    0,
    1
  },
  {
    register_operand,
    "=y",
    V2SImode,
    0,
    1
  },
  {
    nonimmediate_operand,
    "xm",
    V4SFmode,
    0,
    1
  },
  {
    register_operand,
    "=x",
    V4SFmode,
    0,
    1
  },
  {
    register_operand,
    "0",
    V4SFmode,
    0,
    1
  },
  {
    nonimmediate_operand,
    "rm",
    SImode,
    0,
    1
  },
  {
    register_operand,
    "=x,x",
    V4SFmode,
    0,
    1
  },
  {
    register_operand,
    "0,0",
    V4SFmode,
    0,
    1
  },
  {
    nonimmediate_operand,
    "r,rm",
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
    nonimmediate_operand,
    "xm",
    V4SFmode,
    0,
    1
  },
  {
    register_operand,
    "=r,r",
    DImode,
    0,
    1
  },
  {
    nonimmediate_operand,
    "x,m",
    V4SFmode,
    0,
    1
  },
  {
    register_operand,
    "=r,r",
    DImode,
    0,
    1
  },
  {
    nonimmediate_operand,
    "x,xm",
    V4SFmode,
    0,
    1
  },
  {
    register_operand,
    "=y",
    V8QImode,
    0,
    1
  },
  {
    register_operand,
    "%0",
    V8QImode,
    0,
    1
  },
  {
    nonimmediate_operand,
    "ym",
    V8QImode,
    0,
    1
  },
  {
    register_operand,
    "=y",
    V4HImode,
    0,
    1
  },
  {
    register_operand,
    "%0",
    V4HImode,
    0,
    1
  },
  {
    nonimmediate_operand,
    "ym",
    V4HImode,
    0,
    1
  },
  {
    register_operand,
    "=y",
    V2SImode,
    0,
    1
  },
  {
    register_operand,
    "%0",
    V2SImode,
    0,
    1
  },
  {
    nonimmediate_operand,
    "ym",
    V2SImode,
    0,
    1
  },
  {
    register_operand,
    "=y",
    DImode,
    0,
    1
  },
  {
    register_operand,
    "%0",
    DImode,
    0,
    1
  },
  {
    nonimmediate_operand,
    "ym",
    DImode,
    0,
    1
  },
  {
    register_operand,
    "=y",
    V8QImode,
    0,
    1
  },
  {
    register_operand,
    "0",
    V8QImode,
    0,
    1
  },
  {
    nonimmediate_operand,
    "ym",
    V8QImode,
    0,
    1
  },
  {
    register_operand,
    "=y",
    V4HImode,
    0,
    1
  },
  {
    register_operand,
    "0",
    V4HImode,
    0,
    1
  },
  {
    nonimmediate_operand,
    "ym",
    V4HImode,
    0,
    1
  },
  {
    register_operand,
    "=y",
    V2SImode,
    0,
    1
  },
  {
    register_operand,
    "0",
    V2SImode,
    0,
    1
  },
  {
    nonimmediate_operand,
    "ym",
    V2SImode,
    0,
    1
  },
  {
    register_operand,
    "=y",
    DImode,
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
    nonimmediate_operand,
    "ym",
    DImode,
    0,
    1
  },
  {
    register_operand,
    "=y",
    V2SImode,
    0,
    1
  },
  {
    register_operand,
    "0",
    V4HImode,
    0,
    1
  },
  {
    nonimmediate_operand,
    "ym",
    V4HImode,
    0,
    1
  },
  {
    register_operand,
    "=y",
    DImode,
    0,
    1
  },
  {
    register_operand,
    "0",
    V8QImode,
    0,
    1
  },
  {
    nonimmediate_operand,
    "ym",
    V8QImode,
    0,
    1
  },
  {
    register_operand,
    "=y",
    V4HImode,
    0,
    1
  },
  {
    register_operand,
    "0",
    V4HImode,
    0,
    1
  },
  {
    nonimmediate_operand,
    "rm",
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
    register_operand,
    "=r",
    SImode,
    0,
    1
  },
  {
    register_operand,
    "y",
    V4HImode,
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
    register_operand,
    "=y",
    V4HImode,
    0,
    1
  },
  {
    register_operand,
    "0",
    V4HImode,
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
    register_operand,
    "=y",
    V4HImode,
    0,
    1
  },
  {
    register_operand,
    "0",
    V4HImode,
    0,
    1
  },
  {
    nonmemory_operand,
    "yi",
    DImode,
    0,
    1
  },
  {
    register_operand,
    "=y",
    V2SImode,
    0,
    1
  },
  {
    register_operand,
    "0",
    V2SImode,
    0,
    1
  },
  {
    nonmemory_operand,
    "yi",
    DImode,
    0,
    1
  },
  {
    register_operand,
    "=y",
    DImode,
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
    nonmemory_operand,
    "yi",
    DImode,
    0,
    1
  },
  {
    register_operand,
    "=y",
    V8QImode,
    0,
    1
  },
  {
    register_operand,
    "0",
    V4HImode,
    0,
    1
  },
  {
    register_operand,
    "y",
    V4HImode,
    0,
    1
  },
  {
    register_operand,
    "=y",
    V4HImode,
    0,
    1
  },
  {
    register_operand,
    "0",
    V2SImode,
    0,
    1
  },
  {
    register_operand,
    "y",
    V2SImode,
    0,
    1
  },
  {
    register_operand,
    "=y",
    V8QImode,
    0,
    1
  },
  {
    register_operand,
    "0",
    V8QImode,
    0,
    1
  },
  {
    register_operand,
    "y",
    V8QImode,
    0,
    1
  },
  {
    register_operand,
    "=y",
    V4HImode,
    0,
    1
  },
  {
    register_operand,
    "0",
    V4HImode,
    0,
    1
  },
  {
    register_operand,
    "y",
    V4HImode,
    0,
    1
  },
  {
    register_operand,
    "=y",
    V2SImode,
    0,
    1
  },
  {
    register_operand,
    "0",
    V2SImode,
    0,
    1
  },
  {
    register_operand,
    "y",
    V2SImode,
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
    0,
    "",
    BLKmode,
    0,
    1
  },
  {
    register_operand,
    "R",
    DImode,
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
    const_int_operand,
    "i",
    DImode,
    0,
    1
  },
  {
    0,
    "X",
    VOIDmode,
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
    register_operand,
    "=y",
    V2SFmode,
    0,
    1
  },
  {
    register_operand,
    "0",
    V2SFmode,
    0,
    1
  },
  {
    nonimmediate_operand,
    "ym",
    V2SFmode,
    0,
    1
  },
  {
    register_operand,
    "=y",
    V2SImode,
    0,
    1
  },
  {
    register_operand,
    "0",
    V2SFmode,
    0,
    1
  },
  {
    nonimmediate_operand,
    "ym",
    V2SFmode,
    0,
    1
  },
  {
    register_operand,
    "=y",
    V2SImode,
    0,
    1
  },
  {
    nonimmediate_operand,
    "ym",
    V2SFmode,
    0,
    1
  },
  {
    register_operand,
    "=y",
    V2SFmode,
    0,
    1
  },
  {
    register_operand,
    "0",
    V2SFmode,
    0,
    1
  },
  {
    nonimmediate_operand,
    "y",
    V2SFmode,
    0,
    1
  },
  {
    register_operand,
    "=y",
    V2SFmode,
    0,
    1
  },
  {
    nonimmediate_operand,
    "ym",
    V2SImode,
    0,
    1
  },
  {
    register_operand,
    "=y",
    V2SFmode,
    0,
    1
  },
  {
    nonimmediate_operand,
    "ym",
    V2SFmode,
    0,
    1
  },
  {
    register_operand,
    "=y",
    V2SImode,
    0,
    1
  },
  {
    nonimmediate_operand,
    "ym",
    V2SImode,
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
    const_int_operand,
    "",
    SImode,
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
    const_int_operand,
    "",
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
    const_int_operand,
    "n",
    SImode,
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
    const_int_operand,
    "n",
    SImode,
    0,
    1
  },
  {
    register_operand,
    "=x",
    V2DFmode,
    0,
    1
  },
  {
    register_operand,
    "0",
    V2DFmode,
    0,
    1
  },
  {
    nonimmediate_operand,
    "xm",
    V2DFmode,
    0,
    1
  },
  {
    register_operand,
    "=x",
    V2DFmode,
    0,
    1
  },
  {
    register_operand,
    "xm",
    V2DFmode,
    0,
    1
  },
  {
    register_operand,
    "0",
    V2DFmode,
    0,
    1
  },
  {
    register_operand,
    "=x",
    V2DImode,
    0,
    1
  },
  {
    register_operand,
    "0",
    V2DFmode,
    0,
    1
  },
  {
    nonimmediate_operand,
    "x",
    V2DFmode,
    0,
    1
  },
  {
    sse_comparison_operator,
    "",
    V2DImode,
    0,
    0
  },
  {
    register_operand,
    "x",
    V2DFmode,
    0,
    1
  },
  {
    register_operand,
    "x",
    V2DFmode,
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
    "x",
    V2DFmode,
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
    "x",
    V16QImode,
    0,
    1
  },
  {
    register_operand,
    "D",
    SImode,
    0,
    1
  },
  {
    register_operand,
    "x",
    V16QImode,
    0,
    1
  },
  {
    register_operand,
    "x",
    V16QImode,
    0,
    1
  },
  {
    register_operand,
    "D",
    DImode,
    0,
    1
  },
  {
    register_operand,
    "x",
    V16QImode,
    0,
    1
  },
  {
    register_operand,
    "x",
    V16QImode,
    0,
    1
  },
  {
    memory_operand,
    "=m",
    V2DFmode,
    0,
    1
  },
  {
    register_operand,
    "x",
    V2DFmode,
    0,
    1
  },
  {
    memory_operand,
    "=m",
    V2DImode,
    0,
    1
  },
  {
    register_operand,
    "x",
    V2DImode,
    0,
    1
  },
  {
    memory_operand,
    "=m",
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
    "=x",
    V4SFmode,
    0,
    1
  },
  {
    nonimmediate_operand,
    "xm",
    V4SImode,
    0,
    1
  },
  {
    register_operand,
    "=x",
    V4SImode,
    0,
    1
  },
  {
    nonimmediate_operand,
    "xm",
    V4SFmode,
    0,
    1
  },
  {
    register_operand,
    "=x",
    V2DFmode,
    0,
    1
  },
  {
    nonimmediate_operand,
    "xm",
    V4SImode,
    0,
    1
  },
  {
    register_operand,
    "=x",
    V4SImode,
    0,
    1
  },
  {
    nonimmediate_operand,
    "xm",
    V2DFmode,
    0,
    1
  },
  {
    register_operand,
    "=y",
    V2SImode,
    0,
    1
  },
  {
    nonimmediate_operand,
    "xm",
    V2DFmode,
    0,
    1
  },
  {
    register_operand,
    "=x",
    V2DFmode,
    0,
    1
  },
  {
    nonimmediate_operand,
    "ym",
    V2SImode,
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
    "xm",
    V2DFmode,
    0,
    1
  },
  {
    register_operand,
    "=r",
    DImode,
    0,
    1
  },
  {
    register_operand,
    "xm",
    V2DFmode,
    0,
    1
  },
  {
    register_operand,
    "=r,r",
    DImode,
    0,
    1
  },
  {
    register_operand,
    "x,xm",
    V2DFmode,
    0,
    1
  },
  {
    register_operand,
    "=x",
    V2DFmode,
    0,
    1
  },
  {
    register_operand,
    "0",
    V2DFmode,
    0,
    1
  },
  {
    nonimmediate_operand,
    "rm",
    SImode,
    0,
    1
  },
  {
    register_operand,
    "=x,x",
    V2DFmode,
    0,
    1
  },
  {
    register_operand,
    "0,0",
    V2DFmode,
    0,
    1
  },
  {
    nonimmediate_operand,
    "r,rm",
    DImode,
    0,
    1
  },
  {
    register_operand,
    "=x",
    V4SFmode,
    0,
    1
  },
  {
    register_operand,
    "0",
    V4SFmode,
    0,
    1
  },
  {
    register_operand,
    "xm",
    V2DFmode,
    0,
    1
  },
  {
    register_operand,
    "=x",
    V2DFmode,
    0,
    1
  },
  {
    register_operand,
    "0",
    V2DFmode,
    0,
    1
  },
  {
    register_operand,
    "xm",
    V4SFmode,
    0,
    1
  },
  {
    register_operand,
    "=x",
    V4SFmode,
    0,
    1
  },
  {
    nonimmediate_operand,
    "xm",
    V2DFmode,
    0,
    1
  },
  {
    register_operand,
    "=x",
    V2DFmode,
    0,
    1
  },
  {
    nonimmediate_operand,
    "xm",
    V4SFmode,
    0,
    1
  },
  {
    register_operand,
    "=x",
    V16QImode,
    0,
    1
  },
  {
    register_operand,
    "%0",
    V16QImode,
    0,
    1
  },
  {
    nonimmediate_operand,
    "xm",
    V16QImode,
    0,
    1
  },
  {
    register_operand,
    "=x",
    V8HImode,
    0,
    1
  },
  {
    register_operand,
    "%0",
    V8HImode,
    0,
    1
  },
  {
    nonimmediate_operand,
    "xm",
    V8HImode,
    0,
    1
  },
  {
    register_operand,
    "=x",
    V4SImode,
    0,
    1
  },
  {
    register_operand,
    "%0",
    V4SImode,
    0,
    1
  },
  {
    nonimmediate_operand,
    "xm",
    V4SImode,
    0,
    1
  },
  {
    register_operand,
    "=x",
    V2DImode,
    0,
    1
  },
  {
    register_operand,
    "%0",
    V2DImode,
    0,
    1
  },
  {
    nonimmediate_operand,
    "xm",
    V2DImode,
    0,
    1
  },
  {
    register_operand,
    "=x",
    V16QImode,
    0,
    1
  },
  {
    register_operand,
    "0",
    V16QImode,
    0,
    1
  },
  {
    nonimmediate_operand,
    "xm",
    V16QImode,
    0,
    1
  },
  {
    register_operand,
    "=x",
    V8HImode,
    0,
    1
  },
  {
    register_operand,
    "0",
    V8HImode,
    0,
    1
  },
  {
    nonimmediate_operand,
    "xm",
    V8HImode,
    0,
    1
  },
  {
    register_operand,
    "=x",
    V4SImode,
    0,
    1
  },
  {
    register_operand,
    "0",
    V4SImode,
    0,
    1
  },
  {
    nonimmediate_operand,
    "xm",
    V4SImode,
    0,
    1
  },
  {
    register_operand,
    "=x",
    V2DImode,
    0,
    1
  },
  {
    register_operand,
    "0",
    V2DImode,
    0,
    1
  },
  {
    nonimmediate_operand,
    "xm",
    V2DImode,
    0,
    1
  },
  {
    register_operand,
    "=y",
    DImode,
    0,
    1
  },
  {
    register_operand,
    "0",
    V2SImode,
    0,
    1
  },
  {
    nonimmediate_operand,
    "ym",
    V2SImode,
    0,
    1
  },
  {
    register_operand,
    "=x",
    V2DImode,
    0,
    1
  },
  {
    register_operand,
    "0",
    V4SImode,
    0,
    1
  },
  {
    nonimmediate_operand,
    "xm",
    V4SImode,
    0,
    1
  },
  {
    register_operand,
    "=x",
    V4SImode,
    0,
    1
  },
  {
    register_operand,
    "0",
    V8HImode,
    0,
    1
  },
  {
    nonimmediate_operand,
    "xm",
    V8HImode,
    0,
    1
  },
  {
    register_operand,
    "=x",
    V2DImode,
    0,
    1
  },
  {
    register_operand,
    "0",
    V16QImode,
    0,
    1
  },
  {
    nonimmediate_operand,
    "xm",
    V16QImode,
    0,
    1
  },
  {
    register_operand,
    "=x",
    V8HImode,
    0,
    1
  },
  {
    register_operand,
    "0",
    V8HImode,
    0,
    1
  },
  {
    nonimmediate_operand,
    "rm",
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
    register_operand,
    "=r",
    SImode,
    0,
    1
  },
  {
    register_operand,
    "x",
    V8HImode,
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
    register_operand,
    "=x",
    V4SImode,
    0,
    1
  },
  {
    register_operand,
    "0",
    V4SImode,
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
    register_operand,
    "=x",
    V8HImode,
    0,
    1
  },
  {
    register_operand,
    "0",
    V8HImode,
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
    register_operand,
    "=x",
    V8HImode,
    0,
    1
  },
  {
    register_operand,
    "0",
    V8HImode,
    0,
    1
  },
  {
    nonmemory_operand,
    "xi",
    TImode,
    0,
    1
  },
  {
    register_operand,
    "=x",
    V4SImode,
    0,
    1
  },
  {
    register_operand,
    "0",
    V4SImode,
    0,
    1
  },
  {
    nonmemory_operand,
    "xi",
    TImode,
    0,
    1
  },
  {
    register_operand,
    "=x",
    V2DImode,
    0,
    1
  },
  {
    register_operand,
    "0",
    V2DImode,
    0,
    1
  },
  {
    nonmemory_operand,
    "xi",
    TImode,
    0,
    1
  },
  {
    register_operand,
    "=x",
    V8HImode,
    0,
    1
  },
  {
    register_operand,
    "0",
    V8HImode,
    0,
    1
  },
  {
    nonmemory_operand,
    "xi",
    V2DImode,
    0,
    1
  },
  {
    register_operand,
    "=x",
    V4SImode,
    0,
    1
  },
  {
    register_operand,
    "0",
    V4SImode,
    0,
    1
  },
  {
    nonmemory_operand,
    "xi",
    V2DImode,
    0,
    1
  },
  {
    register_operand,
    "=x",
    V2DImode,
    0,
    1
  },
  {
    register_operand,
    "0",
    V2DImode,
    0,
    1
  },
  {
    nonmemory_operand,
    "xi",
    V2DImode,
    0,
    1
  },
  {
    register_operand,
    "=x",
    TImode,
    0,
    1
  },
  {
    register_operand,
    "0",
    TImode,
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
    register_operand,
    "=x",
    V2DFmode,
    0,
    1
  },
  {
    register_operand,
    "0",
    V2DFmode,
    0,
    1
  },
  {
    register_operand,
    "x",
    V2DFmode,
    0,
    1
  },
  {
    register_operand,
    "=x",
    V16QImode,
    0,
    1
  },
  {
    register_operand,
    "0",
    V8HImode,
    0,
    1
  },
  {
    register_operand,
    "x",
    V8HImode,
    0,
    1
  },
  {
    register_operand,
    "=x",
    V8HImode,
    0,
    1
  },
  {
    register_operand,
    "0",
    V4SImode,
    0,
    1
  },
  {
    register_operand,
    "x",
    V4SImode,
    0,
    1
  },
  {
    register_operand,
    "=x",
    V16QImode,
    0,
    1
  },
  {
    register_operand,
    "0",
    V16QImode,
    0,
    1
  },
  {
    register_operand,
    "x",
    V16QImode,
    0,
    1
  },
  {
    register_operand,
    "=x",
    V8HImode,
    0,
    1
  },
  {
    register_operand,
    "0",
    V8HImode,
    0,
    1
  },
  {
    register_operand,
    "x",
    V8HImode,
    0,
    1
  },
  {
    register_operand,
    "=x",
    V4SImode,
    0,
    1
  },
  {
    register_operand,
    "0",
    V4SImode,
    0,
    1
  },
  {
    register_operand,
    "x",
    V4SImode,
    0,
    1
  },
  {
    register_operand,
    "=x",
    V2DImode,
    0,
    1
  },
  {
    register_operand,
    "0",
    V2DImode,
    0,
    1
  },
  {
    register_operand,
    "x",
    V2DImode,
    0,
    1
  },
  {
    nonimmediate_operand,
    "=x,m",
    V2DFmode,
    0,
    1
  },
  {
    nonimmediate_operand,
    "xm,x",
    V2DFmode,
    0,
    1
  },
  {
    nonimmediate_operand,
    "=x,m",
    V16QImode,
    0,
    1
  },
  {
    nonimmediate_operand,
    "xm,x",
    V16QImode,
    0,
    1
  },
  {
    nonimmediate_operand,
    "=m,y",
    DImode,
    0,
    1
  },
  {
    register_operand,
    "x,x",
    V2DImode,
    0,
    1
  },
  {
    nonimmediate_operand,
    "=m,y,r",
    DImode,
    0,
    1
  },
  {
    register_operand,
    "x,x,x",
    V2DImode,
    0,
    1
  },
  {
    register_operand,
    "=x,?x",
    V2DImode,
    0,
    1
  },
  {
    nonimmediate_operand,
    "m,y",
    DImode,
    0,
    1
  },
  {
    register_operand,
    "=x,?x,?x",
    V2DImode,
    0,
    1
  },
  {
    nonimmediate_operand,
    "m,y,r",
    DImode,
    0,
    1
  },
  {
    register_operand,
    "=x",
    V2DImode,
    0,
    1
  },
  {
    nonimmediate_operand,
    "xm",
    V2DImode,
    0,
    1
  },
  {
    register_operand,
    "=x",
    V4SImode,
    0,
    1
  },
  {
    nonimmediate_operand,
    "mr",
    SImode,
    0,
    1
  },
  {
    nonimmediate_operand,
    "=mr",
    SImode,
    0,
    1
  },
  {
    register_operand,
    "x",
    V4SImode,
    0,
    1
  },
  {
    nonimmediate_operand,
    "=x,m",
    V2DFmode,
    0,
    1
  },
  {
    nonimmediate_operand,
    "0,0",
    V2DFmode,
    0,
    1
  },
  {
    nonimmediate_operand,
    "m,x",
    V2DFmode,
    0,
    1
  },
  {
    register_operand,
    "=x",
    V2DFmode,
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
    const0_operand,
    "X",
    V2DFmode,
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
    register_operand,
    "x",
    V2DFmode,
    0,
    1
  },
  {
    register_operand,
    "=x",
    V2DFmode,
    0,
    1
  },
  {
    register_operand,
    "0",
    V2DFmode,
    0,
    1
  },
  {
    nonimmediate_operand,
    "xm",
    V2DFmode,
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
    address_operand,
    "p",
    VOIDmode,
    0,
    1
  },
  {
    register_operand,
    "a",
    SImode,
    0,
    1
  },
  {
    register_operand,
    "c",
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
    "=x",
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
    register_operand,
    "=x",
    V2DFmode,
    0,
    1
  },
  {
    register_operand,
    "x",
    V2DFmode,
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
    x86_64_general_operand,
    "",
    DImode,
    0,
    1
  },
  {
    cmpsi_operand,
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
    ext_register_operand,
    "",
    VOIDmode,
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
    cmp_fp_expander_operand,
    "",
    XFmode,
    0,
    1
  },
  {
    cmp_fp_expander_operand,
    "",
    XFmode,
    0,
    1
  },
  {
    cmp_fp_expander_operand,
    "",
    TFmode,
    0,
    1
  },
  {
    cmp_fp_expander_operand,
    "",
    TFmode,
    0,
    1
  },
  {
    cmp_fp_expander_operand,
    "",
    DFmode,
    0,
    1
  },
  {
    cmp_fp_expander_operand,
    "",
    DFmode,
    0,
    1
  },
  {
    cmp_fp_expander_operand,
    "",
    SFmode,
    0,
    1
  },
  {
    cmp_fp_expander_operand,
    "",
    SFmode,
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
    nonimmediate_operand,
    "",
    HImode,
    1,
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
    0,
    "=m",
    QImode,
    0,
    1
  },
  {
    register_operand,
    "r",
    QImode,
    0,
    1
  },
  {
    register_operand,
    "=&q",
    QImode,
    0,
    1
  },
  {
    nonimmediate_operand,
    "",
    QImode,
    1,
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
    push_operand,
    "",
    DImode,
    0,
    1
  },
  {
    immediate_operand,
    "",
    DImode,
    0,
    1
  },
  {
    scratch_operand,
    "r",
    DImode,
    0,
    0
  },
  {
    push_operand,
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
    memory_operand,
    "",
    DImode,
    0,
    1
  },
  {
    immediate_operand,
    "",
    DImode,
    0,
    1
  },
  {
    scratch_operand,
    "r",
    DImode,
    0,
    0
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
    push_operand,
    "",
    SFmode,
    0,
    1
  },
  {
    memory_operand,
    "",
    SFmode,
    0,
    1
  },
  {
    push_operand,
    "",
    SFmode,
    0,
    1
  },
  {
    any_fp_register_operand,
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
    push_operand,
    "",
    DFmode,
    0,
    1
  },
  {
    any_fp_register_operand,
    "",
    DFmode,
    0,
    1
  },
  {
    push_operand,
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
    nonimmediate_operand,
    "",
    XFmode,
    0,
    1
  },
  {
    general_operand,
    "",
    XFmode,
    0,
    1
  },
  {
    nonimmediate_operand,
    "",
    TFmode,
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
    push_operand,
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
    push_operand,
    "",
    XFmode,
    0,
    1
  },
  {
    any_fp_register_operand,
    "",
    XFmode,
    0,
    1
  },
  {
    push_operand,
    "",
    TFmode,
    0,
    1
  },
  {
    any_fp_register_operand,
    "",
    TFmode,
    0,
    1
  },
  {
    nonimmediate_operand,
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
    memory_operand,
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
    nonimmediate_operand,
    "",
    HImode,
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
    HImode,
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
    SImode,
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
    register_operand,
    "=r",
    DImode,
    0,
    1
  },
  {
    nonimmediate_operand,
    "rm",
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
    SImode,
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
    memory_operand,
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
    push_operand,
    "",
    DFmode,
    0,
    1
  },
  {
    fp_register_operand,
    "",
    SFmode,
    0,
    1
  },
  {
    push_operand,
    "",
    XFmode,
    0,
    1
  },
  {
    fp_register_operand,
    "",
    SFmode,
    0,
    1
  },
  {
    push_operand,
    "",
    TFmode,
    0,
    1
  },
  {
    fp_register_operand,
    "",
    SFmode,
    0,
    1
  },
  {
    push_operand,
    "",
    XFmode,
    0,
    1
  },
  {
    fp_register_operand,
    "",
    DFmode,
    0,
    1
  },
  {
    push_operand,
    "",
    TFmode,
    0,
    1
  },
  {
    fp_register_operand,
    "",
    DFmode,
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
    SFmode,
    0,
    1
  },
  {
    nonimmediate_operand,
    "",
    XFmode,
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
    TFmode,
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
    XFmode,
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
    nonimmediate_operand,
    "",
    TFmode,
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
    nonimmediate_operand,
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
    memory_operand,
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
    memory_operand,
    "",
    SFmode,
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
    nonimmediate_operand,
    "",
    DFmode,
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
    SFmode,
    0,
    1
  },
  {
    fp_register_operand,
    "",
    DFmode,
    0,
    1
  },
  {
    memory_operand,
    "",
    SFmode,
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
    register_operand,
    "",
    XFmode,
    0,
    1
  },
  {
    memory_operand,
    "",
    SFmode,
    0,
    1
  },
  {
    register_operand,
    "",
    XFmode,
    0,
    1
  },
  {
    memory_operand,
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
    XFmode,
    0,
    1
  },
  {
    memory_operand,
    "",
    SFmode,
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
    register_operand,
    "",
    TFmode,
    0,
    1
  },
  {
    memory_operand,
    "",
    SFmode,
    0,
    1
  },
  {
    register_operand,
    "",
    TFmode,
    0,
    1
  },
  {
    memory_operand,
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
    TFmode,
    0,
    1
  },
  {
    memory_operand,
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
    register_operand,
    "",
    XFmode,
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
    register_operand,
    "",
    XFmode,
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
    register_operand,
    "",
    DFmode,
    0,
    1
  },
  {
    register_operand,
    "",
    XFmode,
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
    nonimmediate_operand,
    "",
    DFmode,
    0,
    1
  },
  {
    register_operand,
    "",
    TFmode,
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
    register_operand,
    "",
    TFmode,
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
    register_operand,
    "",
    DFmode,
    0,
    1
  },
  {
    register_operand,
    "",
    TFmode,
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
    nonimmediate_operand,
    "",
    DImode,
    0,
    1
  },
  {
    register_operand,
    "",
    XFmode,
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
    register_operand,
    "",
    TFmode,
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
    register_operand,
    "",
    DFmode,
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
    register_operand,
    "",
    SFmode,
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
    register_operand,
    "",
    VOIDmode,
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
    VOIDmode,
    0,
    1
  },
  {
    memory_operand,
    "",
    HImode,
    0,
    1
  },
  {
    memory_operand,
    "",
    HImode,
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
    scratch_operand,
    "",
    VOIDmode,
    0,
    0
  },
  {
    memory_operand,
    "",
    DImode,
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
    HImode,
    0,
    1
  },
  {
    memory_operand,
    "",
    HImode,
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
    scratch_operand,
    "",
    VOIDmode,
    0,
    0
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
    XFmode,
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
    TFmode,
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
    DFmode,
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
    SFmode,
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
    register_operand,
    "",
    VOIDmode,
    0,
    1
  },
  {
    memory_operand,
    "",
    HImode,
    0,
    1
  },
  {
    memory_operand,
    "",
    HImode,
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
    register_operand,
    "",
    VOIDmode,
    0,
    1
  },
  {
    memory_operand,
    "",
    HImode,
    0,
    1
  },
  {
    memory_operand,
    "",
    HImode,
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
    nonimmediate_operand,
    "",
    HImode,
    0,
    1
  },
  {
    register_operand,
    "",
    XFmode,
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
    register_operand,
    "",
    DFmode,
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
    SFmode,
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
    VOIDmode,
    0,
    1
  },
  {
    memory_operand,
    "",
    HImode,
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
    HImode,
    0,
    1
  },
  {
    memory_operand,
    "",
    HImode,
    0,
    1
  },
  {
    memory_operand,
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
    register_operand,
    "",
    VOIDmode,
    0,
    1
  },
  {
    memory_operand,
    "",
    HImode,
    0,
    1
  },
  {
    memory_operand,
    "",
    HImode,
    0,
    1
  },
  {
    memory_operand,
    "",
    HImode,
    0,
    1
  },
  {
    fp_register_operand,
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
    nonimmediate_operand,
    "",
    DImode,
    0,
    1
  },
  {
    x86_64_general_operand,
    "",
    DImode,
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
    nonimmediate_operand,
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
    register_operand,
    "",
    VOIDmode,
    0,
    1
  },
  {
    index_register_operand,
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
    immediate_operand,
    "",
    VOIDmode,
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
    index_register_operand,
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
    immediate_operand,
    "",
    SImode,
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
    index_register_operand,
    "",
    VOIDmode,
    0,
    1
  },
  {
    const248_operand,
    "",
    VOIDmode,
    0,
    1
  },
  {
    nonmemory_operand,
    "",
    VOIDmode,
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
    index_register_operand,
    "",
    SImode,
    0,
    1
  },
  {
    const248_operand,
    "",
    SImode,
    0,
    1
  },
  {
    nonmemory_operand,
    "",
    SImode,
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
    index_register_operand,
    "",
    VOIDmode,
    0,
    1
  },
  {
    const248_operand,
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
    immediate_operand,
    "",
    VOIDmode,
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
    index_register_operand,
    "",
    SImode,
    0,
    1
  },
  {
    const248_operand,
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
    register_operand,
    "",
    DImode,
    0,
    1
  },
  {
    x86_64_nonmemory_operand,
    "",
    DImode,
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
    nonmemory_operand,
    "",
    VOIDmode,
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
    nonmemory_operand,
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
    nonimmediate_operand,
    "",
    QImode,
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
    XFmode,
    0,
    1
  },
  {
    register_operand,
    "",
    XFmode,
    0,
    1
  },
  {
    register_operand,
    "",
    XFmode,
    0,
    1
  },
  {
    register_operand,
    "",
    TFmode,
    0,
    1
  },
  {
    register_operand,
    "",
    TFmode,
    0,
    1
  },
  {
    register_operand,
    "",
    TFmode,
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
    nonimmediate_operand,
    "",
    DFmode,
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
    nonimmediate_operand,
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
    DImode,
    0,
    1
  },
  {
    x86_64_general_operand,
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
    general_operand,
    "",
    SImode,
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
    register_operand,
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
    QImode,
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
    QImode,
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
    QImode,
    0,
    1
  },
  {
    register_operand,
    "",
    TImode,
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
    register_operand,
    "",
    DImode,
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
    register_operand,
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
    register_operand,
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
    register_operand,
    "=d",
    DImode,
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
    register_operand,
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
    nonimmediate_operand,
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
    register_operand,
    "",
    SImode,
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
    register_operand,
    "",
    HImode,
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
    SImode,
    0,
    1
  },
  {
    nonmemory_operand,
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
    nonmemory_operand,
    "",
    QImode,
    0,
    1
  },
  {
    ext_register_operand,
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
    nonimmediate_operand,
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
    nonimmediate_operand,
    "",
    DImode,
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
    x86_64_szext_general_operand,
    "",
    DImode,
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
    const_int_operand,
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
    general_operand,
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
    ext_register_operand,
    "",
    VOIDmode,
    0,
    1
  },
  {
    ext_register_operand,
    "",
    VOIDmode,
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
    nonimmediate_operand,
    "",
    SFmode,
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
    memory_operand,
    "",
    SFmode,
    0,
    1
  },
  {
    memory_operand,
    "",
    SFmode,
    0,
    1
  },
  {
    0,
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
    SFmode,
    0,
    1
  },
  {
    0,
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
    fp_register_operand,
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
    register_and_not_fp_reg_operand,
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
    memory_operand,
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
    nonimmediate_operand,
    "",
    DFmode,
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
    memory_operand,
    "",
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
    0,
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
    0,
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
    register_operand,
    "",
    DFmode,
    0,
    1
  },
  {
    fp_register_operand,
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
    register_and_not_fp_reg_operand,
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
    nonimmediate_operand,
    "",
    XFmode,
    0,
    1
  },
  {
    nonimmediate_operand,
    "",
    XFmode,
    0,
    1
  },
  {
    nonimmediate_operand,
    "",
    TFmode,
    0,
    1
  },
  {
    nonimmediate_operand,
    "",
    TFmode,
    0,
    1
  },
  {
    fp_register_operand,
    "",
    XFmode,
    0,
    1
  },
  {
    register_operand,
    "",
    XFmode,
    0,
    1
  },
  {
    register_and_not_fp_reg_operand,
    "",
    XFmode,
    0,
    1
  },
  {
    register_operand,
    "",
    XFmode,
    0,
    1
  },
  {
    fp_register_operand,
    "",
    TFmode,
    0,
    1
  },
  {
    register_operand,
    "",
    TFmode,
    0,
    1
  },
  {
    register_and_not_fp_reg_operand,
    "",
    TFmode,
    0,
    1
  },
  {
    register_operand,
    "",
    TFmode,
    0,
    1
  },
  {
    register_and_not_any_fp_reg_operand,
    "",
    TFmode,
    0,
    1
  },
  {
    register_operand,
    "",
    TFmode,
    0,
    1
  },
  {
    shiftdi_operand,
    "",
    DImode,
    0,
    1
  },
  {
    shiftdi_operand,
    "",
    DImode,
    0,
    1
  },
  {
    nonmemory_operand,
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
    register_operand,
    "",
    DImode,
    0,
    1
  },
  {
    immediate_operand,
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
    register_operand,
    "",
    DImode,
    0,
    1
  },
  {
    nonmemory_operand,
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
    QImode,
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
    nonimmediate_operand,
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
    nonmemory_operand,
    "",
    QImode,
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
    index_register_operand,
    "",
    VOIDmode,
    0,
    1
  },
  {
    const_int_operand,
    "",
    QImode,
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
    const_int_operand,
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
    register_operand,
    "",
    VOIDmode,
    0,
    1
  },
  {
    const_int_operand,
    "",
    QImode,
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
    nonimmediate_operand,
    "",
    HImode,
    0,
    1
  },
  {
    nonmemory_operand,
    "",
    QImode,
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
    nonimmediate_operand,
    "",
    QImode,
    0,
    1
  },
  {
    nonmemory_operand,
    "",
    QImode,
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
    nonimmediate_operand,
    "",
    DImode,
    0,
    1
  },
  {
    nonmemory_operand,
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
    ext_register_operand,
    "",
    VOIDmode,
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
    nonimmediate_operand,
    "",
    QImode,
    0,
    1
  },
  {
    ix86_comparison_operator,
    "",
    VOIDmode,
    0,
    0
  },
  {
    nonimmediate_operand,
    "",
    QImode,
    1,
    1
  },
  {
    ix86_comparison_operator,
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
    register_operand,
    "",
    VOIDmode,
    0,
    1
  },
  {
    nonimmediate_operand,
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
    "=a",
    HImode,
    0,
    0
  },
  {
    nonimmediate_operand,
    "rm",
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
    register_operand,
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
    register_operand,
    "",
    QImode,
    0,
    1
  },
  {
    ix86_comparison_operator,
    "",
    QImode,
    0,
    0
  },
  {
    q_regs_operand,
    "",
    VOIDmode,
    0,
    1
  },
  {
    0,
    "",
    QImode,
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
    0,
    "",
    QImode,
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
    QImode,
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
    0,
    "",
    VOIDmode,
    0,
    1
  },
  {
    0,
    "",
    QImode,
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
    register_operand,
    "",
    SImode,
    0,
    1
  },
  {
    tls_symbolic_operand,
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
    "",
    DImode,
    0,
    1
  },
  {
    tls_symbolic_operand,
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
    call_insn_operand,
    "",
    SImode,
    0,
    1
  },
  {
    tls_symbolic_operand,
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
    register_operand,
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
    register_operand,
    "",
    VOIDmode,
    0,
    1
  },
  {
    binary_fp_operator,
    "",
    VOIDmode,
    0,
    0
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
    register_operand,
    "",
    SImode,
    0,
    1
  },
  {
    binary_fp_operator,
    "",
    VOIDmode,
    0,
    0
  },
  {
    memory_operand,
    "",
    BLKmode,
    0,
    1
  },
  {
    memory_operand,
    "",
    BLKmode,
    0,
    1
  },
  {
    nonmemory_operand,
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
    memory_operand,
    "",
    BLKmode,
    0,
    1
  },
  {
    memory_operand,
    "",
    BLKmode,
    0,
    1
  },
  {
    nonmemory_operand,
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
    memory_operand,
    "",
    BLKmode,
    0,
    1
  },
  {
    nonmemory_operand,
    "",
    SImode,
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
    memory_operand,
    "",
    BLKmode,
    0,
    1
  },
  {
    nonmemory_operand,
    "",
    DImode,
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
    DImode,
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
    register_operand,
    "",
    DImode,
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
    register_operand,
    "",
    SImode,
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
    general_operand,
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
    SImode,
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
    immediate_operand,
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
    register_operand,
    "",
    DImode,
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
    immediate_operand,
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
    register_operand,
    "",
    VOIDmode,
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
    register_operand,
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
    general_operand,
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
    general_operand,
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
    register_operand,
    "",
    HImode,
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
    nonimmediate_operand,
    "",
    HImode,
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
    register_and_not_any_fp_reg_operand,
    "",
    DFmode,
    0,
    1
  },
  {
    fcmov_comparison_operator,
    "",
    VOIDmode,
    0,
    0
  },
  {
    nonimmediate_operand,
    "",
    DFmode,
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
    0,
    "",
    VOIDmode,
    0,
    1
  },
  {
    register_operand,
    "",
    XFmode,
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
    XFmode,
    0,
    1
  },
  {
    register_operand,
    "",
    XFmode,
    0,
    1
  },
  {
    register_operand,
    "",
    TFmode,
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
    TFmode,
    0,
    1
  },
  {
    register_operand,
    "",
    TFmode,
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
    nonimmediate_operand,
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
    nonimmediate_operand,
    "",
    SFmode,
    0,
    1
  },
  {
    fp_register_operand,
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
    register_operand,
    "",
    DFmode,
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
    register_operand,
    "",
    DFmode,
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
    fp_register_operand,
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
    "",
    VOIDmode,
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
    nonimmediate_operand,
    "",
    VOIDmode,
    0,
    1
  },
  {
    nonimmediate_operand,
    "",
    VOIDmode,
    0,
    1
  },
  {
    nonimmediate_operand,
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
    sse_comparison_operator,
    "",
    VOIDmode,
    0,
    0
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
    register_operand,
    "",
    VOIDmode,
    0,
    1
  },
  {
    nonimmediate_operand,
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
    comparison_operator,
    "",
    VOIDmode,
    0,
    0
  },
  {
    nonmemory_operand,
    "",
    VOIDmode,
    0,
    1
  },
  {
    nonmemory_operand,
    "",
    VOIDmode,
    0,
    1
  },
  {
    nonimmediate_operand,
    "",
    VOIDmode,
    0,
    1
  },
  {
    nonimmediate_operand,
    "",
    VOIDmode,
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
    general_operand,
    "",
    SImode,
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
    aligned_operand,
    "",
    VOIDmode,
    0,
    1
  },
  {
    promotable_binary_operator,
    "",
    VOIDmode,
    0,
    0
  },
  {
    register_operand,
    "",
    VOIDmode,
    0,
    1
  },
  {
    aligned_operand,
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
    aligned_operand,
    "",
    HImode,
    0,
    1
  },
  {
    const_int_operand,
    "",
    HImode,
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
    comparison_operator,
    "",
    VOIDmode,
    0,
    0
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
    push_operand,
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
    scratch_operand,
    "r",
    SImode,
    0,
    0
  },
  {
    push_operand,
    "",
    DImode,
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
    scratch_operand,
    "r",
    DImode,
    0,
    0
  },
  {
    push_operand,
    "",
    SFmode,
    0,
    1
  },
  {
    memory_operand,
    "",
    SFmode,
    0,
    1
  },
  {
    scratch_operand,
    "r",
    SFmode,
    0,
    0
  },
  {
    push_operand,
    "",
    HImode,
    0,
    1
  },
  {
    memory_operand,
    "",
    HImode,
    0,
    1
  },
  {
    scratch_operand,
    "r",
    HImode,
    0,
    0
  },
  {
    push_operand,
    "",
    QImode,
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
    scratch_operand,
    "q",
    QImode,
    0,
    0
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
    scratch_operand,
    "r",
    SImode,
    0,
    0
  },
  {
    memory_operand,
    "",
    HImode,
    0,
    1
  },
  {
    immediate_operand,
    "",
    HImode,
    0,
    1
  },
  {
    scratch_operand,
    "r",
    HImode,
    0,
    0
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
    QImode,
    0,
    1
  },
  {
    scratch_operand,
    "q",
    QImode,
    0,
    0
  },
  {
    memory_operand,
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
    "r",
    SImode,
    0,
    0
  },
  {
    register_operand,
    "",
    QImode,
    0,
    1
  },
  {
    immediate_operand,
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
    memory_operand,
    "",
    SImode,
    0,
    1
  },
  {
    scratch_operand,
    "r",
    SImode,
    0,
    0
  },
  {
    arith_or_logical_operator,
    "",
    SImode,
    0,
    0
  },
  {
    memory_operand,
    "",
    SImode,
    0,
    1
  },
  {
    nonmemory_operand,
    "",
    SImode,
    0,
    1
  },
  {
    scratch_operand,
    "r",
    SImode,
    0,
    0
  },
  {
    arith_or_logical_operator,
    "",
    SImode,
    0,
    0
  },
  {
    register_operand,
    "",
    VOIDmode,
    1,
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
    nonmemory_operand,
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
    scratch_operand,
    "r",
    SImode,
    0,
    0
  },
  {
    scratch_operand,
    "r",
    SImode,
    0,
    0
  },
  {
    register_operand,
    "",
    SImode,
    0,
    1
  },
  {
    incdec_operand,
    "",
    SImode,
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
    incdec_operand,
    "",
    HImode,
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
    incdec_operand,
    "",
    QImode,
    0,
    1
  },
  {
    scratch_operand,
    "r",
    DImode,
    0,
    0
  },
  {
    scratch_operand,
    "r",
    DImode,
    0,
    0
  },
  {
    nonimmediate_operand,
    "",
    TImode,
    0,
    1
  },
  {
    nonimmediate_operand,
    "",
    TImode,
    0,
    1
  },
  {
    nonimmediate_operand,
    "",
    V2DFmode,
    0,
    1
  },
  {
    nonimmediate_operand,
    "",
    V2DFmode,
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
    nonimmediate_operand,
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
    nonimmediate_operand,
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
    nonimmediate_operand,
    "",
    V4SFmode,
    0,
    1
  },
  {
    nonimmediate_operand,
    "",
    V4SImode,
    0,
    1
  },
  {
    nonimmediate_operand,
    "",
    V4SImode,
    0,
    1
  },
  {
    nonimmediate_operand,
    "",
    V2DImode,
    0,
    1
  },
  {
    nonimmediate_operand,
    "",
    V2DImode,
    0,
    1
  },
  {
    nonimmediate_operand,
    "",
    V2SImode,
    0,
    1
  },
  {
    nonimmediate_operand,
    "",
    V2SImode,
    0,
    1
  },
  {
    nonimmediate_operand,
    "",
    V4HImode,
    0,
    1
  },
  {
    nonimmediate_operand,
    "",
    V4HImode,
    0,
    1
  },
  {
    nonimmediate_operand,
    "",
    V8QImode,
    0,
    1
  },
  {
    nonimmediate_operand,
    "",
    V8QImode,
    0,
    1
  },
  {
    nonimmediate_operand,
    "",
    V2SFmode,
    0,
    1
  },
  {
    nonimmediate_operand,
    "",
    V2SFmode,
    0,
    1
  },
  {
    push_operand,
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
    push_operand,
    "",
    TImode,
    0,
    1
  },
  {
    nonmemory_operand,
    "",
    TImode,
    0,
    1
  },
  {
    push_operand,
    "",
    V2DFmode,
    0,
    1
  },
  {
    nonmemory_operand,
    "",
    V2DFmode,
    0,
    1
  },
  {
    push_operand,
    "",
    V2DImode,
    0,
    1
  },
  {
    nonmemory_operand,
    "",
    V2DImode,
    0,
    1
  },
  {
    push_operand,
    "",
    V8HImode,
    0,
    1
  },
  {
    nonmemory_operand,
    "",
    V8HImode,
    0,
    1
  },
  {
    push_operand,
    "",
    V16QImode,
    0,
    1
  },
  {
    nonmemory_operand,
    "",
    V16QImode,
    0,
    1
  },
  {
    push_operand,
    "",
    V4SFmode,
    0,
    1
  },
  {
    nonmemory_operand,
    "",
    V4SFmode,
    0,
    1
  },
  {
    push_operand,
    "",
    V4SImode,
    0,
    1
  },
  {
    nonmemory_operand,
    "",
    V4SImode,
    0,
    1
  },
  {
    push_operand,
    "",
    V2SImode,
    0,
    1
  },
  {
    nonmemory_operand,
    "",
    V2SImode,
    0,
    1
  },
  {
    push_operand,
    "",
    V4HImode,
    0,
    1
  },
  {
    nonmemory_operand,
    "",
    V4HImode,
    0,
    1
  },
  {
    push_operand,
    "",
    V8QImode,
    0,
    1
  },
  {
    nonmemory_operand,
    "",
    V8QImode,
    0,
    1
  },
  {
    push_operand,
    "",
    V2SFmode,
    0,
    1
  },
  {
    nonmemory_operand,
    "",
    V2SFmode,
    0,
    1
  },
  {
    nonimmediate_operand,
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
    register_operand,
    "",
    V4SFmode,
    0,
    1
  },
  {
    memory_operand,
    "",
    SFmode,
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
    nonimmediate_operand,
    "",
    V4SFmode,
    0,
    1
  },
  {
    register_operand,
    "",
    V2DFmode,
    0,
    1
  },
  {
    register_operand,
    "",
    V2DFmode,
    0,
    1
  },
  {
    nonimmediate_operand,
    "",
    V2DFmode,
    0,
    1
  },
  {
    nonimmediate_operand,
    "",
    V2DFmode,
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
    register_operand,
    "",
    DImode,
    0,
    1
  },
  {
    immediate_operand,
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
    address_operand,
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
    register_operand,
    "",
    V2DFmode,
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
};



const struct insn_data insn_data[] = 
{
  {
    "cmpdi_ccno_1_rex64",
    (const PTR) output_0,
    (insn_gen_fn) gen_cmpdi_ccno_1_rex64,
    &operand_data[1],
    2,
    0,
    2,
    2
  },
  {
    "*cmpdi_minus_1_rex64",
    "cmp{q}\t{%1, %0|%0, %1}",
    0,
    &operand_data[3],
    2,
    0,
    2,
    1
  },
  {
    "cmpdi_1_insn_rex64",
    "cmp{q}\t{%1, %0|%0, %1}",
    (insn_gen_fn) gen_cmpdi_1_insn_rex64,
    &operand_data[5],
    2,
    0,
    2,
    1
  },
  {
    "*cmpsi_ccno_1",
    (const PTR) output_3,
    0,
    &operand_data[7],
    2,
    0,
    2,
    2
  },
  {
    "*cmpsi_minus_1",
    "cmp{l}\t{%1, %0|%0, %1}",
    0,
    &operand_data[9],
    2,
    0,
    2,
    1
  },
  {
    "*cmpsi_1_insn",
    "cmp{l}\t{%1, %0|%0, %1}",
    0,
    &operand_data[9],
    2,
    0,
    2,
    1
  },
  {
    "*cmphi_ccno_1",
    (const PTR) output_6,
    0,
    &operand_data[11],
    2,
    0,
    2,
    2
  },
  {
    "*cmphi_minus_1",
    "cmp{w}\t{%1, %0|%0, %1}",
    0,
    &operand_data[13],
    2,
    0,
    2,
    1
  },
  {
    "*cmphi_1",
    "cmp{w}\t{%1, %0|%0, %1}",
    0,
    &operand_data[13],
    2,
    0,
    2,
    1
  },
  {
    "*cmpqi_ccno_1",
    (const PTR) output_9,
    0,
    &operand_data[15],
    2,
    0,
    2,
    2
  },
  {
    "*cmpqi_1",
    "cmp{b}\t{%1, %0|%0, %1}",
    0,
    &operand_data[17],
    2,
    0,
    2,
    1
  },
  {
    "*cmpqi_minus_1",
    "cmp{b}\t{%1, %0|%0, %1}",
    0,
    &operand_data[17],
    2,
    0,
    2,
    1
  },
  {
    "*cmpqi_ext_1",
    "cmp{b}\t{%h1, %0|%0, %h1}",
    0,
    &operand_data[19],
    2,
    0,
    1,
    1
  },
  {
    "*cmpqi_ext_1_rex64",
    "cmp{b}\t{%h1, %0|%0, %h1}",
    0,
    &operand_data[21],
    2,
    0,
    1,
    1
  },
  {
    "*cmpqi_ext_2",
    "test{b}\t%h0, %h0",
    0,
    &operand_data[22],
    2,
    0,
    1,
    1
  },
  {
    "cmpqi_ext_3_insn",
    "cmp{b}\t{%1, %h0|%h0, %1}",
    (insn_gen_fn) gen_cmpqi_ext_3_insn,
    &operand_data[24],
    2,
    0,
    1,
    1
  },
  {
    "cmpqi_ext_3_insn_rex64",
    "cmp{b}\t{%1, %h0|%h0, %1}",
    (insn_gen_fn) gen_cmpqi_ext_3_insn_rex64,
    &operand_data[26],
    2,
    0,
    1,
    1
  },
  {
    "*cmpqi_ext_4",
    "cmp{b}\t{%h1, %h0|%h0, %h1}",
    0,
    &operand_data[28],
    2,
    0,
    1,
    1
  },
  {
    "*cmpfp_0",
    (const PTR) output_18,
    0,
    &operand_data[30],
    3,
    0,
    1,
    3
  },
  {
    "*cmpfp_2_sf",
    (const PTR) output_19,
    0,
    &operand_data[33],
    2,
    0,
    1,
    3
  },
  {
    "*cmpfp_2_sf_1",
    (const PTR) output_20,
    0,
    &operand_data[35],
    3,
    0,
    1,
    3
  },
  {
    "*cmpfp_2_df",
    (const PTR) output_21,
    0,
    &operand_data[38],
    2,
    0,
    1,
    3
  },
  {
    "*cmpfp_2_df_1",
    (const PTR) output_22,
    0,
    &operand_data[40],
    3,
    0,
    1,
    3
  },
  {
    "*cmpfp_2_xf",
    (const PTR) output_23,
    0,
    &operand_data[43],
    2,
    0,
    1,
    3
  },
  {
    "*cmpfp_2_tf",
    (const PTR) output_24,
    0,
    &operand_data[45],
    2,
    0,
    1,
    3
  },
  {
    "*cmpfp_2_xf_1",
    (const PTR) output_25,
    0,
    &operand_data[47],
    3,
    0,
    1,
    3
  },
  {
    "*cmpfp_2_tf_1",
    (const PTR) output_26,
    0,
    &operand_data[50],
    3,
    0,
    1,
    3
  },
  {
    "*cmpfp_2u",
    (const PTR) output_27,
    0,
    &operand_data[53],
    2,
    0,
    1,
    3
  },
  {
    "*cmpfp_2u_1",
    (const PTR) output_28,
    0,
    &operand_data[55],
    3,
    0,
    1,
    3
  },
  {
    "x86_fnstsw_1",
    "fnstsw\t%0",
    (insn_gen_fn) gen_x86_fnstsw_1,
    &operand_data[30],
    1,
    0,
    1,
    1
  },
  {
    "x86_sahf_1",
    "sahf",
    (insn_gen_fn) gen_x86_sahf_1,
    &operand_data[58],
    1,
    0,
    1,
    1
  },
  {
    "*cmpfp_i",
    (const PTR) output_31,
    0,
    &operand_data[53],
    2,
    0,
    1,
    3
  },
  {
    "*cmpfp_i_sse",
    (const PTR) output_32,
    0,
    &operand_data[59],
    2,
    0,
    2,
    3
  },
  {
    "*cmpfp_i_sse_only",
    (const PTR) output_33,
    0,
    &operand_data[61],
    2,
    0,
    1,
    3
  },
  {
    "*cmpfp_iu",
    (const PTR) output_34,
    0,
    &operand_data[53],
    2,
    0,
    1,
    3
  },
  {
    "*cmpfp_iu_sse",
    (const PTR) output_35,
    0,
    &operand_data[59],
    2,
    0,
    2,
    3
  },
  {
    "*cmpfp_iu_sse_only",
    (const PTR) output_36,
    0,
    &operand_data[61],
    2,
    0,
    1,
    3
  },
  {
    "*pushsi2",
    "push{l}\t%1",
    0,
    &operand_data[63],
    2,
    0,
    1,
    1
  },
  {
    "*pushsi2_rex64",
    "push{q}\t%q1",
    0,
    &operand_data[65],
    2,
    0,
    1,
    1
  },
  {
    "*pushsi2_prologue",
    "push{l}\t%1",
    0,
    &operand_data[63],
    2,
    0,
    1,
    1
  },
  {
    "*popsi1_epilogue",
    "pop{l}\t%0",
    0,
    &operand_data[67],
    1,
    0,
    1,
    1
  },
  {
    "popsi1",
    "pop{l}\t%0",
    (insn_gen_fn) gen_popsi1,
    &operand_data[67],
    1,
    0,
    1,
    1
  },
  {
    "*movsi_xor",
    "xor{l}\t{%0, %0|%0, %0}",
    0,
    &operand_data[68],
    2,
    0,
    1,
    1
  },
  {
    "*movsi_or",
    (const PTR) output_43,
    0,
    &operand_data[70],
    2,
    0,
    1,
    3
  },
  {
    "*movsi_1",
    (const PTR) output_44,
    0,
    &operand_data[72],
    2,
    0,
    8,
    3
  },
  {
    "*movabssi_1_rex64",
    (const PTR) output_45,
    0,
    &operand_data[74],
    2,
    0,
    2,
    2
  },
  {
    "*movabssi_2_rex64",
    (const PTR) output_46,
    0,
    &operand_data[76],
    2,
    0,
    2,
    2
  },
  {
    "*swapsi",
    "xchg{l}\t%1, %0",
    0,
    &operand_data[78],
    2,
    2,
    1,
    1
  },
  {
    "*pushhi2",
    (const PTR) output_48,
    0,
    &operand_data[80],
    2,
    0,
    2,
    2
  },
  {
    "*pushhi2_rex64",
    "push{q}\t%q1",
    0,
    &operand_data[82],
    2,
    0,
    1,
    1
  },
  {
    "*movhi_1",
    (const PTR) output_50,
    0,
    &operand_data[84],
    2,
    0,
    4,
    3
  },
  {
    "*movabshi_1_rex64",
    (const PTR) output_51,
    0,
    &operand_data[86],
    2,
    0,
    2,
    2
  },
  {
    "*movabshi_2_rex64",
    (const PTR) output_52,
    0,
    &operand_data[88],
    2,
    0,
    2,
    2
  },
  {
    "*swaphi_1",
    "xchg{w}\t%1, %0",
    0,
    &operand_data[90],
    2,
    2,
    1,
    1
  },
  {
    "*swaphi_2",
    "xchg{l}\t%k1, %k0",
    0,
    &operand_data[90],
    2,
    2,
    1,
    1
  },
  {
    "*movstricthi_1",
    "mov{w}\t{%1, %0|%0, %1}",
    0,
    &operand_data[92],
    2,
    0,
    2,
    1
  },
  {
    "*movstricthi_xor",
    "xor{w}\t{%0, %0|%0, %0}",
    0,
    &operand_data[94],
    2,
    0,
    1,
    1
  },
  {
    "*pushqi2",
    (const PTR) output_57,
    0,
    &operand_data[96],
    2,
    0,
    2,
    2
  },
  {
    "*pushqi2_rex64",
    "push{q}\t%q1",
    0,
    &operand_data[98],
    2,
    0,
    1,
    1
  },
  {
    "*movqi_1",
    (const PTR) output_59,
    0,
    &operand_data[100],
    2,
    0,
    7,
    3
  },
  {
    "*swapqi",
    "xchg{b}\t%1, %0",
    0,
    &operand_data[102],
    2,
    2,
    1,
    1
  },
  {
    "*movstrictqi_1",
    "mov{b}\t{%1, %0|%0, %1}",
    0,
    &operand_data[104],
    2,
    0,
    2,
    1
  },
  {
    "*movstrictqi_xor",
    "xor{b}\t{%0, %0|%0, %0}",
    0,
    &operand_data[106],
    2,
    0,
    1,
    1
  },
  {
    "*movsi_extv_1",
    "movs{bl|x}\t{%h1, %0|%0, %h1}",
    0,
    &operand_data[108],
    2,
    0,
    1,
    1
  },
  {
    "*movhi_extv_1",
    "movs{bl|x}\t{%h1, %k0|%k0, %h1}",
    0,
    &operand_data[110],
    2,
    0,
    1,
    1
  },
  {
    "*movqi_extv_1",
    (const PTR) output_65,
    0,
    &operand_data[112],
    2,
    0,
    2,
    3
  },
  {
    "*movqi_extv_1_rex64",
    (const PTR) output_66,
    0,
    &operand_data[114],
    2,
    0,
    2,
    3
  },
  {
    "*movabsqi_1_rex64",
    (const PTR) output_67,
    0,
    &operand_data[116],
    2,
    0,
    2,
    2
  },
  {
    "*movabsqi_2_rex64",
    (const PTR) output_68,
    0,
    &operand_data[118],
    2,
    0,
    2,
    2
  },
  {
    "*movsi_extzv_1",
    "movz{bl|x}\t{%h1, %0|%0, %h1}",
    0,
    &operand_data[108],
    2,
    0,
    1,
    1
  },
  {
    "*movqi_extzv_2",
    (const PTR) output_70,
    0,
    &operand_data[120],
    2,
    0,
    2,
    3
  },
  {
    "*movqi_extzv_2_rex64",
    (const PTR) output_71,
    0,
    &operand_data[114],
    2,
    0,
    2,
    3
  },
  {
    "movsi_insv_1",
    "mov{b}\t{%b1, %h0|%h0, %b1}",
    (insn_gen_fn) gen_movsi_insv_1,
    &operand_data[122],
    2,
    0,
    1,
    1
  },
  {
    "*movsi_insv_1_rex64",
    "mov{b}\t{%b1, %h0|%h0, %b1}",
    0,
    &operand_data[124],
    2,
    0,
    1,
    1
  },
  {
    "*movqi_insv_2",
    "mov{b}\t{%h1, %h0|%h0, %h1}",
    0,
    &operand_data[126],
    2,
    0,
    1,
    1
  },
  {
    "*pushdi",
    "#",
    0,
    &operand_data[128],
    2,
    0,
    1,
    1
  },
  {
    "pushdi2_rex64",
    (const PTR) output_76,
    (insn_gen_fn) gen_pushdi2_rex64,
    &operand_data[130],
    2,
    0,
    2,
    2
  },
  {
    "*pushdi2_prologue_rex64",
    "push{q}\t%1",
    0,
    &operand_data[132],
    2,
    0,
    1,
    1
  },
  {
    "*popdi1_epilogue_rex64",
    "pop{q}\t%0",
    0,
    &operand_data[134],
    1,
    0,
    1,
    1
  },
  {
    "popdi1",
    "pop{q}\t%0",
    (insn_gen_fn) gen_popdi1,
    &operand_data[134],
    1,
    0,
    1,
    1
  },
  {
    "*movdi_xor_rex64",
    "xor{l}\t{%k0, %k0|%k0, %k0}",
    0,
    &operand_data[135],
    2,
    0,
    1,
    1
  },
  {
    "*movdi_or_rex64",
    (const PTR) output_81,
    0,
    &operand_data[137],
    2,
    0,
    1,
    3
  },
  {
    "*movdi_2",
    (const PTR) output_82,
    0,
    &operand_data[139],
    2,
    0,
    7,
    2
  },
  {
    "*movdi_1_rex64",
    (const PTR) output_83,
    0,
    &operand_data[141],
    2,
    0,
    10,
    3
  },
  {
    "*movabsdi_1_rex64",
    (const PTR) output_84,
    0,
    &operand_data[143],
    2,
    0,
    2,
    2
  },
  {
    "*movabsdi_2_rex64",
    (const PTR) output_85,
    0,
    &operand_data[145],
    2,
    0,
    2,
    2
  },
  {
    "*swapdi_rex64",
    "xchg{q}\t%1, %0",
    0,
    &operand_data[147],
    2,
    2,
    1,
    1
  },
  {
    "*pushsf",
    (const PTR) output_87,
    0,
    &operand_data[149],
    2,
    0,
    3,
    3
  },
  {
    "*pushsf_rex64",
    (const PTR) output_88,
    0,
    &operand_data[151],
    2,
    0,
    3,
    3
  },
  {
    "*movsf_1",
    (const PTR) output_89,
    0,
    &operand_data[153],
    2,
    0,
    12,
    3
  },
  {
    "*swapsf",
    (const PTR) output_90,
    0,
    &operand_data[155],
    2,
    2,
    1,
    3
  },
  {
    "*pushdf_nointeger",
    (const PTR) output_91,
    0,
    &operand_data[157],
    2,
    0,
    4,
    3
  },
  {
    "*pushdf_integer",
    (const PTR) output_92,
    0,
    &operand_data[159],
    2,
    0,
    3,
    3
  },
  {
    "*movdf_nointeger",
    (const PTR) output_93,
    0,
    &operand_data[161],
    2,
    0,
    9,
    3
  },
  {
    "*movdf_integer",
    (const PTR) output_94,
    0,
    &operand_data[163],
    2,
    0,
    9,
    3
  },
  {
    "*swapdf",
    (const PTR) output_95,
    0,
    &operand_data[165],
    2,
    2,
    1,
    3
  },
  {
    "*pushxf_nointeger",
    (const PTR) output_96,
    0,
    &operand_data[167],
    2,
    0,
    3,
    3
  },
  {
    "*pushtf_nointeger",
    (const PTR) output_97,
    0,
    &operand_data[169],
    2,
    0,
    3,
    3
  },
  {
    "*pushxf_integer",
    (const PTR) output_98,
    0,
    &operand_data[171],
    2,
    0,
    2,
    3
  },
  {
    "*pushtf_integer",
    (const PTR) output_99,
    0,
    &operand_data[173],
    2,
    0,
    2,
    3
  },
  {
    "*movxf_nointeger",
    (const PTR) output_100,
    0,
    &operand_data[175],
    2,
    0,
    5,
    3
  },
  {
    "*movtf_nointeger",
    (const PTR) output_101,
    0,
    &operand_data[177],
    2,
    0,
    5,
    3
  },
  {
    "*movxf_integer",
    (const PTR) output_102,
    0,
    &operand_data[179],
    2,
    0,
    5,
    3
  },
  {
    "*movtf_integer",
    (const PTR) output_103,
    0,
    &operand_data[181],
    2,
    0,
    5,
    3
  },
  {
    "swapxf",
    (const PTR) output_104,
    (insn_gen_fn) gen_swapxf,
    &operand_data[183],
    2,
    2,
    1,
    3
  },
  {
    "swaptf",
    (const PTR) output_105,
    (insn_gen_fn) gen_swaptf,
    &operand_data[185],
    2,
    2,
    1,
    3
  },
  {
    "zero_extendhisi2_and",
    "#",
    (insn_gen_fn) gen_zero_extendhisi2_and,
    &operand_data[187],
    2,
    0,
    1,
    1
  },
  {
    "*zero_extendhisi2_movzwl",
    "movz{wl|x}\t{%1, %0|%0, %1}",
    0,
    &operand_data[189],
    2,
    0,
    1,
    1
  },
  {
    "*zero_extendqihi2_and",
    "#",
    0,
    &operand_data[191],
    2,
    0,
    2,
    1
  },
  {
    "*zero_extendqihi2_movzbw_and",
    "#",
    0,
    &operand_data[193],
    2,
    0,
    2,
    1
  },
  {
    "*zero_extendqihi2_movzbw",
    "movz{bw|x}\t{%1, %0|%0, %1}",
    0,
    &operand_data[195],
    2,
    0,
    1,
    1
  },
  {
    "*zero_extendqisi2_and",
    "#",
    0,
    &operand_data[197],
    2,
    0,
    2,
    1
  },
  {
    "*zero_extendqisi2_movzbw_and",
    "#",
    0,
    &operand_data[199],
    2,
    0,
    2,
    1
  },
  {
    "*zero_extendqisi2_movzbw",
    "movz{bl|x}\t{%1, %0|%0, %1}",
    0,
    &operand_data[201],
    2,
    0,
    1,
    1
  },
  {
    "zero_extendsidi2_32",
    "#",
    (insn_gen_fn) gen_zero_extendsidi2_32,
    &operand_data[203],
    2,
    0,
    3,
    1
  },
  {
    "zero_extendsidi2_rex64",
    (const PTR) output_115,
    (insn_gen_fn) gen_zero_extendsidi2_rex64,
    &operand_data[205],
    2,
    0,
    2,
    2
  },
  {
    "zero_extendhidi2",
    (const PTR) output_116,
    (insn_gen_fn) gen_zero_extendhidi2,
    &operand_data[207],
    2,
    0,
    2,
    2
  },
  {
    "zero_extendqidi2",
    (const PTR) output_117,
    (insn_gen_fn) gen_zero_extendqidi2,
    &operand_data[209],
    2,
    0,
    2,
    2
  },
  {
    "*extendsidi2_1",
    "#",
    0,
    &operand_data[211],
    3,
    0,
    4,
    1
  },
  {
    "extendsidi2_rex64",
    (const PTR) output_119,
    (insn_gen_fn) gen_extendsidi2_rex64,
    &operand_data[214],
    2,
    0,
    2,
    2
  },
  {
    "extendhidi2",
    "movs{wq|x}\t{%1,%0|%0, %1}",
    (insn_gen_fn) gen_extendhidi2,
    &operand_data[216],
    2,
    0,
    1,
    1
  },
  {
    "extendqidi2",
    "movs{bq|x}\t{%1,%0|%0, %1}",
    (insn_gen_fn) gen_extendqidi2,
    &operand_data[218],
    2,
    0,
    1,
    1
  },
  {
    "extendhisi2",
    (const PTR) output_122,
    (insn_gen_fn) gen_extendhisi2,
    &operand_data[220],
    2,
    0,
    2,
    3
  },
  {
    "*extendhisi2_zext",
    (const PTR) output_123,
    0,
    &operand_data[222],
    2,
    0,
    2,
    3
  },
  {
    "extendqihi2",
    (const PTR) output_124,
    (insn_gen_fn) gen_extendqihi2,
    &operand_data[224],
    2,
    0,
    2,
    3
  },
  {
    "extendqisi2",
    "movs{bl|x}\t{%1,%0|%0, %1}",
    (insn_gen_fn) gen_extendqisi2,
    &operand_data[201],
    2,
    0,
    1,
    1
  },
  {
    "*extendqisi2_zext",
    "movs{bl|x}\t{%1,%k0|%k0, %1}",
    0,
    &operand_data[218],
    2,
    0,
    1,
    1
  },
  {
    "*extendsfdf2_1",
    (const PTR) output_127,
    0,
    &operand_data[226],
    2,
    0,
    3,
    3
  },
  {
    "*extendsfdf2_1_sse_only",
    "cvtss2sd\t{%1, %0|%0, %1}",
    0,
    &operand_data[228],
    2,
    0,
    1,
    1
  },
  {
    "*extendsfxf2_1",
    (const PTR) output_129,
    0,
    &operand_data[230],
    2,
    0,
    2,
    3
  },
  {
    "*extendsftf2_1",
    (const PTR) output_130,
    0,
    &operand_data[232],
    2,
    0,
    2,
    3
  },
  {
    "*extenddfxf2_1",
    (const PTR) output_131,
    0,
    &operand_data[234],
    2,
    0,
    2,
    3
  },
  {
    "*extenddftf2_1",
    (const PTR) output_132,
    0,
    &operand_data[236],
    2,
    0,
    2,
    3
  },
  {
    "*truncdfsf2_1",
    (const PTR) output_133,
    0,
    &operand_data[238],
    3,
    0,
    4,
    3
  },
  {
    "*truncdfsf2_1_sse",
    (const PTR) output_134,
    0,
    &operand_data[241],
    3,
    0,
    5,
    3
  },
  {
    "*truncdfsf2_2",
    (const PTR) output_135,
    0,
    &operand_data[244],
    2,
    0,
    2,
    3
  },
  {
    "truncdfsf2_3",
    (const PTR) output_136,
    (insn_gen_fn) gen_truncdfsf2_3,
    &operand_data[246],
    2,
    0,
    1,
    3
  },
  {
    "truncdfsf2_sse_only",
    "cvtsd2ss\t{%1, %0|%0, %1}",
    (insn_gen_fn) gen_truncdfsf2_sse_only,
    &operand_data[248],
    2,
    0,
    1,
    1
  },
  {
    "*truncxfsf2_1",
    (const PTR) output_138,
    0,
    &operand_data[250],
    3,
    0,
    4,
    3
  },
  {
    "*truncxfsf2_2",
    (const PTR) output_139,
    0,
    &operand_data[253],
    2,
    0,
    1,
    3
  },
  {
    "*trunctfsf2_1",
    (const PTR) output_140,
    0,
    &operand_data[255],
    3,
    0,
    4,
    3
  },
  {
    "*trunctfsf2_2",
    (const PTR) output_141,
    0,
    &operand_data[258],
    2,
    0,
    1,
    3
  },
  {
    "*truncxfdf2_1",
    (const PTR) output_142,
    0,
    &operand_data[260],
    3,
    0,
    4,
    3
  },
  {
    "*truncxfdf2_2",
    (const PTR) output_143,
    0,
    &operand_data[263],
    2,
    0,
    1,
    3
  },
  {
    "*trunctfdf2_1",
    (const PTR) output_144,
    0,
    &operand_data[265],
    3,
    0,
    4,
    3
  },
  {
    "*trunctfdf2_2",
    (const PTR) output_145,
    0,
    &operand_data[268],
    2,
    0,
    1,
    3
  },
  {
    "*fix_truncdi_1",
    "#",
    0,
    &operand_data[270],
    2,
    0,
    2,
    1
  },
  {
    "fix_truncdi_nomemory",
    "#",
    (insn_gen_fn) gen_fix_truncdi_nomemory,
    &operand_data[270],
    6,
    0,
    2,
    1
  },
  {
    "fix_truncdi_memory",
    (const PTR) output_148,
    (insn_gen_fn) gen_fix_truncdi_memory,
    &operand_data[276],
    5,
    0,
    1,
    3
  },
  {
    "fix_truncsfdi_sse",
    "cvttss2si{q}\t{%1, %0|%0, %1}",
    (insn_gen_fn) gen_fix_truncsfdi_sse,
    &operand_data[281],
    2,
    0,
    1,
    1
  },
  {
    "fix_truncdfdi_sse",
    "cvttsd2si{q}\t{%1, %0|%0, %1}",
    (insn_gen_fn) gen_fix_truncdfdi_sse,
    &operand_data[283],
    2,
    0,
    1,
    1
  },
  {
    "*fix_truncsi_1",
    "#",
    0,
    &operand_data[285],
    2,
    0,
    2,
    1
  },
  {
    "fix_truncsi_nomemory",
    "#",
    (insn_gen_fn) gen_fix_truncsi_nomemory,
    &operand_data[285],
    5,
    0,
    2,
    1
  },
  {
    "fix_truncsi_memory",
    (const PTR) output_153,
    (insn_gen_fn) gen_fix_truncsi_memory,
    &operand_data[290],
    4,
    0,
    1,
    3
  },
  {
    "fix_truncsfsi_sse",
    "cvttss2si\t{%1, %0|%0, %1}",
    (insn_gen_fn) gen_fix_truncsfsi_sse,
    &operand_data[294],
    2,
    0,
    1,
    1
  },
  {
    "fix_truncdfsi_sse",
    "cvttsd2si\t{%1, %0|%0, %1}",
    (insn_gen_fn) gen_fix_truncdfsi_sse,
    &operand_data[296],
    2,
    0,
    1,
    1
  },
  {
    "*fix_trunchi_1",
    "#",
    0,
    &operand_data[298],
    2,
    0,
    2,
    1
  },
  {
    "fix_trunchi_nomemory",
    "#",
    (insn_gen_fn) gen_fix_trunchi_nomemory,
    &operand_data[298],
    5,
    0,
    2,
    1
  },
  {
    "fix_trunchi_memory",
    (const PTR) output_158,
    (insn_gen_fn) gen_fix_trunchi_memory,
    &operand_data[303],
    4,
    0,
    1,
    3
  },
  {
    "x86_fnstcw_1",
    "fnstcw\t%0",
    (insn_gen_fn) gen_x86_fnstcw_1,
    &operand_data[303],
    1,
    0,
    1,
    1
  },
  {
    "x86_fldcw_1",
    "fldcw\t%0",
    (insn_gen_fn) gen_x86_fldcw_1,
    &operand_data[278],
    1,
    0,
    1,
    1
  },
  {
    "floathisf2",
    (const PTR) output_161,
    (insn_gen_fn) gen_floathisf2,
    &operand_data[307],
    2,
    0,
    2,
    2
  },
  {
    "*floatsisf2_i387",
    (const PTR) output_162,
    0,
    &operand_data[309],
    2,
    0,
    3,
    2
  },
  {
    "*floatsisf2_sse",
    "cvtsi2ss\t{%1, %0|%0, %1}",
    0,
    &operand_data[311],
    2,
    0,
    1,
    1
  },
  {
    "*floatdisf2_i387_only",
    (const PTR) output_164,
    0,
    &operand_data[313],
    2,
    0,
    2,
    2
  },
  {
    "*floatdisf2_i387",
    (const PTR) output_165,
    0,
    &operand_data[315],
    2,
    0,
    3,
    2
  },
  {
    "*floatdisf2_sse",
    "cvtsi2ss{q}\t{%1, %0|%0, %1}",
    0,
    &operand_data[317],
    2,
    0,
    1,
    1
  },
  {
    "floathidf2",
    (const PTR) output_167,
    (insn_gen_fn) gen_floathidf2,
    &operand_data[319],
    2,
    0,
    2,
    2
  },
  {
    "*floatsidf2_i387",
    (const PTR) output_168,
    0,
    &operand_data[321],
    2,
    0,
    3,
    2
  },
  {
    "*floatsidf2_sse",
    "cvtsi2sd\t{%1, %0|%0, %1}",
    0,
    &operand_data[323],
    2,
    0,
    1,
    1
  },
  {
    "*floatdidf2_i387_only",
    (const PTR) output_170,
    0,
    &operand_data[325],
    2,
    0,
    2,
    2
  },
  {
    "*floatdidf2_i387",
    (const PTR) output_171,
    0,
    &operand_data[327],
    2,
    0,
    3,
    2
  },
  {
    "*floatdidf2_sse",
    "cvtsi2sd{q}\t{%1, %0|%0, %1}",
    0,
    &operand_data[329],
    2,
    0,
    1,
    1
  },
  {
    "floathixf2",
    (const PTR) output_173,
    (insn_gen_fn) gen_floathixf2,
    &operand_data[331],
    2,
    0,
    2,
    2
  },
  {
    "floathitf2",
    (const PTR) output_174,
    (insn_gen_fn) gen_floathitf2,
    &operand_data[333],
    2,
    0,
    2,
    2
  },
  {
    "floatsixf2",
    (const PTR) output_175,
    (insn_gen_fn) gen_floatsixf2,
    &operand_data[335],
    2,
    0,
    2,
    2
  },
  {
    "floatsitf2",
    (const PTR) output_176,
    (insn_gen_fn) gen_floatsitf2,
    &operand_data[337],
    2,
    0,
    2,
    2
  },
  {
    "floatdixf2",
    (const PTR) output_177,
    (insn_gen_fn) gen_floatdixf2,
    &operand_data[339],
    2,
    0,
    2,
    2
  },
  {
    "floatditf2",
    (const PTR) output_178,
    (insn_gen_fn) gen_floatditf2,
    &operand_data[341],
    2,
    0,
    2,
    2
  },
  {
    "*adddi3_1",
    "#",
    0,
    &operand_data[343],
    3,
    0,
    2,
    1
  },
  {
    "*adddi3_carry_rex64",
    "adc{q}\t{%2, %0|%0, %2}",
    0,
    &operand_data[346],
    3,
    0,
    2,
    1
  },
  {
    "*adddi3_cc_rex64",
    "add{q}\t{%2, %0|%0, %2}",
    0,
    &operand_data[346],
    3,
    2,
    2,
    1
  },
  {
    "*addsi3_carry",
    "adc{l}\t{%2, %0|%0, %2}",
    0,
    &operand_data[349],
    3,
    0,
    2,
    1
  },
  {
    "*addsi3_carry_zext",
    "adc{l}\t{%2, %k0|%k0, %2}",
    0,
    &operand_data[352],
    3,
    0,
    1,
    1
  },
  {
    "*addsi3_cc",
    "add{l}\t{%2, %0|%0, %2}",
    0,
    &operand_data[349],
    3,
    2,
    2,
    1
  },
  {
    "addqi3_cc",
    "add{b}\t{%2, %0|%0, %2}",
    (insn_gen_fn) gen_addqi3_cc,
    &operand_data[355],
    3,
    2,
    2,
    1
  },
  {
    "*lea_1",
    "lea{l}\t{%a1, %0|%0, %a1}",
    0,
    &operand_data[358],
    2,
    0,
    1,
    1
  },
  {
    "*lea_1_rex64",
    "lea{l}\t{%a1, %0|%0, %a1}",
    0,
    &operand_data[360],
    2,
    0,
    1,
    1
  },
  {
    "*lea_1_zext",
    "lea{l}\t{%a1, %k0|%k0, %a1}",
    0,
    &operand_data[362],
    2,
    0,
    1,
    1
  },
  {
    "*lea_2_rex64",
    "lea{q}\t{%a1, %0|%0, %a1}",
    0,
    &operand_data[362],
    2,
    0,
    1,
    1
  },
  {
    "*lea_general_1",
    "#",
    0,
    &operand_data[364],
    4,
    0,
    1,
    1
  },
  {
    "*lea_general_1_zext",
    "#",
    0,
    &operand_data[368],
    4,
    0,
    1,
    1
  },
  {
    "*lea_general_2",
    "#",
    0,
    &operand_data[372],
    4,
    0,
    1,
    1
  },
  {
    "*lea_general_2_zext",
    "#",
    0,
    &operand_data[376],
    4,
    0,
    1,
    1
  },
  {
    "*lea_general_3",
    "#",
    0,
    &operand_data[380],
    5,
    0,
    1,
    1
  },
  {
    "*lea_general_3_zext",
    "#",
    0,
    &operand_data[385],
    5,
    0,
    1,
    1
  },
  {
    "*adddi_1_rex64",
    (const PTR) output_196,
    0,
    &operand_data[390],
    3,
    0,
    3,
    3
  },
  {
    "*adddi_2_rex64",
    (const PTR) output_197,
    0,
    &operand_data[393],
    3,
    2,
    2,
    3
  },
  {
    "*adddi_3_rex64",
    (const PTR) output_198,
    0,
    &operand_data[396],
    3,
    0,
    1,
    3
  },
  {
    "*adddi_4_rex64",
    (const PTR) output_199,
    0,
    &operand_data[399],
    3,
    0,
    1,
    3
  },
  {
    "*adddi_5_rex64",
    (const PTR) output_200,
    0,
    &operand_data[402],
    3,
    0,
    1,
    3
  },
  {
    "*addsi_1",
    (const PTR) output_201,
    0,
    &operand_data[405],
    3,
    0,
    3,
    3
  },
  {
    "addsi_1_zext",
    (const PTR) output_202,
    (insn_gen_fn) gen_addsi_1_zext,
    &operand_data[408],
    3,
    0,
    2,
    3
  },
  {
    "*addsi_2",
    (const PTR) output_203,
    0,
    &operand_data[411],
    3,
    2,
    2,
    3
  },
  {
    "*addsi_2_zext",
    (const PTR) output_204,
    0,
    &operand_data[414],
    3,
    2,
    1,
    3
  },
  {
    "*addsi_3",
    (const PTR) output_205,
    0,
    &operand_data[417],
    3,
    0,
    1,
    3
  },
  {
    "*addsi_3_zext",
    (const PTR) output_206,
    0,
    &operand_data[414],
    3,
    2,
    1,
    3
  },
  {
    "*addsi_4",
    (const PTR) output_207,
    0,
    &operand_data[420],
    3,
    0,
    1,
    3
  },
  {
    "*addsi_5",
    (const PTR) output_208,
    0,
    &operand_data[417],
    3,
    0,
    1,
    3
  },
  {
    "*addhi_1_lea",
    (const PTR) output_209,
    0,
    &operand_data[423],
    3,
    0,
    3,
    3
  },
  {
    "*addhi_1",
    (const PTR) output_210,
    0,
    &operand_data[426],
    3,
    0,
    2,
    3
  },
  {
    "*addhi_2",
    (const PTR) output_211,
    0,
    &operand_data[429],
    3,
    2,
    2,
    3
  },
  {
    "*addhi_3",
    (const PTR) output_212,
    0,
    &operand_data[432],
    3,
    0,
    1,
    3
  },
  {
    "*addhi_4",
    (const PTR) output_213,
    0,
    &operand_data[435],
    3,
    0,
    1,
    3
  },
  {
    "*addhi_5",
    (const PTR) output_214,
    0,
    &operand_data[432],
    3,
    0,
    1,
    3
  },
  {
    "*addqi_1_lea",
    (const PTR) output_215,
    0,
    &operand_data[438],
    3,
    0,
    4,
    3
  },
  {
    "*addqi_1",
    (const PTR) output_216,
    0,
    &operand_data[441],
    3,
    0,
    3,
    3
  },
  {
    "*addqi_1_slp",
    (const PTR) output_217,
    0,
    &operand_data[444],
    2,
    1,
    2,
    3
  },
  {
    "*addqi_2",
    (const PTR) output_218,
    0,
    &operand_data[446],
    3,
    2,
    2,
    3
  },
  {
    "*addqi_3",
    (const PTR) output_219,
    0,
    &operand_data[449],
    3,
    0,
    1,
    3
  },
  {
    "*addqi_4",
    (const PTR) output_220,
    0,
    &operand_data[452],
    3,
    0,
    1,
    3
  },
  {
    "*addqi_5",
    (const PTR) output_221,
    0,
    &operand_data[449],
    3,
    0,
    1,
    3
  },
  {
    "addqi_ext_1",
    (const PTR) output_222,
    (insn_gen_fn) gen_addqi_ext_1,
    &operand_data[455],
    3,
    0,
    1,
    3
  },
  {
    "*addqi_ext_1_rex64",
    (const PTR) output_223,
    0,
    &operand_data[458],
    3,
    0,
    1,
    3
  },
  {
    "*addqi_ext_2",
    "add{b}\t{%h2, %h0|%h0, %h2}",
    0,
    &operand_data[461],
    3,
    0,
    1,
    1
  },
  {
    "*subdi3_1",
    "#",
    0,
    &operand_data[464],
    3,
    0,
    2,
    1
  },
  {
    "subdi3_carry_rex64",
    "sbb{q}\t{%2, %0|%0, %2}",
    (insn_gen_fn) gen_subdi3_carry_rex64,
    &operand_data[467],
    3,
    0,
    2,
    1
  },
  {
    "*subdi_1_rex64",
    "sub{q}\t{%2, %0|%0, %2}",
    0,
    &operand_data[467],
    3,
    0,
    2,
    1
  },
  {
    "*subdi_2_rex64",
    "sub{q}\t{%2, %0|%0, %2}",
    0,
    &operand_data[467],
    3,
    2,
    2,
    1
  },
  {
    "*subdi_3_rex63",
    "sub{q}\t{%2, %0|%0, %2}",
    0,
    &operand_data[467],
    3,
    2,
    2,
    1
  },
  {
    "subsi3_carry",
    "sbb{l}\t{%2, %0|%0, %2}",
    (insn_gen_fn) gen_subsi3_carry,
    &operand_data[470],
    3,
    0,
    2,
    1
  },
  {
    "subsi3_carry_zext",
    "sbb{l}\t{%2, %k0|%k0, %2}",
    (insn_gen_fn) gen_subsi3_carry_zext,
    &operand_data[473],
    3,
    0,
    2,
    1
  },
  {
    "*subsi_1",
    "sub{l}\t{%2, %0|%0, %2}",
    0,
    &operand_data[470],
    3,
    0,
    2,
    1
  },
  {
    "*subsi_1_zext",
    "sub{l}\t{%2, %k0|%k0, %2}",
    0,
    &operand_data[476],
    3,
    0,
    1,
    1
  },
  {
    "*subsi_2",
    "sub{l}\t{%2, %0|%0, %2}",
    0,
    &operand_data[470],
    3,
    2,
    2,
    1
  },
  {
    "*subsi_2_zext",
    "sub{l}\t{%2, %k0|%k0, %2}",
    0,
    &operand_data[476],
    3,
    2,
    1,
    1
  },
  {
    "*subsi_3",
    "sub{l}\t{%2, %0|%0, %2}",
    0,
    &operand_data[470],
    3,
    2,
    2,
    1
  },
  {
    "*subsi_3_zext",
    "sub{q}\t{%2, %0|%0, %2}",
    0,
    &operand_data[479],
    3,
    2,
    1,
    1
  },
  {
    "*subhi_1",
    "sub{w}\t{%2, %0|%0, %2}",
    0,
    &operand_data[482],
    3,
    0,
    2,
    1
  },
  {
    "*subhi_2",
    "sub{w}\t{%2, %0|%0, %2}",
    0,
    &operand_data[482],
    3,
    2,
    2,
    1
  },
  {
    "*subhi_3",
    "sub{w}\t{%2, %0|%0, %2}",
    0,
    &operand_data[482],
    3,
    2,
    2,
    1
  },
  {
    "*subqi_1",
    "sub{b}\t{%2, %0|%0, %2}",
    0,
    &operand_data[485],
    3,
    0,
    2,
    1
  },
  {
    "*subqi_1_slp",
    "sub{b}\t{%1, %0|%0, %1}",
    0,
    &operand_data[488],
    2,
    1,
    2,
    1
  },
  {
    "*subqi_2",
    "sub{b}\t{%2, %0|%0, %2}",
    0,
    &operand_data[490],
    3,
    2,
    2,
    1
  },
  {
    "*subqi_3",
    "sub{b}\t{%2, %0|%0, %2}",
    0,
    &operand_data[490],
    3,
    2,
    2,
    1
  },
  {
    "*muldi3_1_rex64",
    (const PTR) output_245,
    0,
    &operand_data[493],
    3,
    0,
    3,
    2
  },
  {
    "*mulsi3_1",
    (const PTR) output_246,
    0,
    &operand_data[496],
    3,
    0,
    3,
    2
  },
  {
    "*mulsi3_1_zext",
    (const PTR) output_247,
    0,
    &operand_data[499],
    3,
    0,
    3,
    2
  },
  {
    "*mulhi3_1",
    (const PTR) output_248,
    0,
    &operand_data[502],
    3,
    0,
    3,
    2
  },
  {
    "*mulqi3_1",
    "mul{b}\t%2",
    0,
    &operand_data[505],
    3,
    0,
    1,
    1
  },
  {
    "*umulqihi3_1",
    "mul{b}\t%2",
    0,
    &operand_data[508],
    3,
    0,
    1,
    1
  },
  {
    "*mulqihi3_insn",
    "imul{b}\t%2",
    0,
    &operand_data[508],
    3,
    0,
    1,
    1
  },
  {
    "*umulditi3_insn",
    "mul{q}\t%2",
    0,
    &operand_data[511],
    3,
    0,
    1,
    1
  },
  {
    "*umulsidi3_insn",
    "mul{l}\t%2",
    0,
    &operand_data[514],
    3,
    0,
    1,
    1
  },
  {
    "*mulditi3_insn",
    "imul{q}\t%2",
    0,
    &operand_data[511],
    3,
    0,
    1,
    1
  },
  {
    "*mulsidi3_insn",
    "imul{l}\t%2",
    0,
    &operand_data[514],
    3,
    0,
    1,
    1
  },
  {
    "*umuldi3_highpart_rex64",
    "mul{q}\t%2",
    0,
    &operand_data[517],
    4,
    0,
    1,
    1
  },
  {
    "*umulsi3_highpart_insn",
    "mul{l}\t%2",
    0,
    &operand_data[521],
    4,
    0,
    1,
    1
  },
  {
    "*umulsi3_highpart_zext",
    "mul{l}\t%2",
    0,
    &operand_data[525],
    4,
    0,
    1,
    1
  },
  {
    "*smuldi3_highpart_rex64",
    "imul{q}\t%2",
    0,
    &operand_data[517],
    4,
    0,
    1,
    1
  },
  {
    "*smulsi3_highpart_insn",
    "imul{l}\t%2",
    0,
    &operand_data[521],
    4,
    0,
    1,
    1
  },
  {
    "*smulsi3_highpart_zext",
    "imul{l}\t%2",
    0,
    &operand_data[525],
    4,
    0,
    1,
    1
  },
  {
    "divqi3",
    "idiv{b}\t%2",
    (insn_gen_fn) gen_divqi3,
    &operand_data[529],
    3,
    0,
    1,
    1
  },
  {
    "udivqi3",
    "div{b}\t%2",
    (insn_gen_fn) gen_udivqi3,
    &operand_data[529],
    3,
    0,
    1,
    1
  },
  {
    "*divmoddi4_nocltd_rex64",
    "#",
    0,
    &operand_data[532],
    4,
    2,
    2,
    1
  },
  {
    "*divmoddi4_cltd_rex64",
    "#",
    0,
    &operand_data[536],
    4,
    2,
    1,
    1
  },
  {
    "*divmoddi_noext_rex64",
    "idiv{q}\t%2",
    0,
    &operand_data[540],
    5,
    2,
    1,
    1
  },
  {
    "*divmodsi4_nocltd",
    "#",
    0,
    &operand_data[545],
    4,
    2,
    2,
    1
  },
  {
    "*divmodsi4_cltd",
    "#",
    0,
    &operand_data[549],
    4,
    2,
    1,
    1
  },
  {
    "*divmodsi_noext",
    "idiv{l}\t%2",
    0,
    &operand_data[553],
    5,
    2,
    1,
    1
  },
  {
    "divmodhi4",
    "cwtd\n\tidiv{w}\t%2",
    (insn_gen_fn) gen_divmodhi4,
    &operand_data[558],
    4,
    2,
    1,
    1
  },
  {
    "udivmoddi4",
    "xor{q}\t%3, %3\n\tdiv{q}\t%2",
    (insn_gen_fn) gen_udivmoddi4,
    &operand_data[562],
    4,
    2,
    1,
    1
  },
  {
    "*udivmoddi4_noext",
    "div{q}\t%2",
    0,
    &operand_data[540],
    4,
    3,
    1,
    1
  },
  {
    "udivmodsi4",
    "xor{l}\t%3, %3\n\tdiv{l}\t%2",
    (insn_gen_fn) gen_udivmodsi4,
    &operand_data[566],
    4,
    2,
    1,
    1
  },
  {
    "*udivmodsi4_noext",
    "div{l}\t%2",
    0,
    &operand_data[553],
    4,
    3,
    1,
    1
  },
  {
    "*udivmodhi_noext",
    "div{w}\t%2",
    0,
    &operand_data[570],
    5,
    2,
    1,
    1
  },
  {
    "*testdi_1_rex64",
    (const PTR) output_276,
    0,
    &operand_data[575],
    2,
    0,
    5,
    2
  },
  {
    "testsi_1",
    "test{l}\t{%1, %0|%0, %1}",
    (insn_gen_fn) gen_testsi_1,
    &operand_data[577],
    2,
    0,
    3,
    1
  },
  {
    "*testhi_1",
    "test{w}\t{%1, %0|%0, %1}",
    0,
    &operand_data[579],
    2,
    0,
    3,
    1
  },
  {
    "*testqi_1",
    (const PTR) output_279,
    0,
    &operand_data[581],
    2,
    0,
    4,
    3
  },
  {
    "*testqi_ext_0",
    "test{b}\t{%1, %h0|%h0, %1}",
    0,
    &operand_data[583],
    2,
    0,
    1,
    1
  },
  {
    "*testqi_ext_1",
    "test{b}\t{%1, %h0|%h0, %1}",
    0,
    &operand_data[585],
    2,
    0,
    1,
    1
  },
  {
    "*testqi_ext_1_rex64",
    "test{b}\t{%1, %h0|%h0, %1}",
    0,
    &operand_data[20],
    2,
    0,
    1,
    1
  },
  {
    "*testqi_ext_2",
    "test{b}\t{%h1, %h0|%h0, %h1}",
    0,
    &operand_data[28],
    2,
    0,
    1,
    1
  },
  {
    "*testqi_ext_3",
    "#",
    0,
    &operand_data[587],
    3,
    0,
    1,
    1
  },
  {
    "*testqi_ext_3_rex64",
    "#",
    0,
    &operand_data[590],
    3,
    0,
    1,
    1
  },
  {
    "*anddi_1_rex64",
    (const PTR) output_286,
    0,
    &operand_data[593],
    3,
    0,
    4,
    3
  },
  {
    "*anddi_2",
    (const PTR) output_287,
    0,
    &operand_data[596],
    3,
    2,
    3,
    2
  },
  {
    "*andsi_1",
    (const PTR) output_288,
    0,
    &operand_data[599],
    3,
    0,
    3,
    3
  },
  {
    "*andsi_1_zext",
    "and{l}\t{%2, %k0|%k0, %2}",
    0,
    &operand_data[352],
    3,
    0,
    1,
    1
  },
  {
    "*andsi_2",
    "and{l}\t{%2, %0|%0, %2}",
    0,
    &operand_data[602],
    3,
    2,
    2,
    1
  },
  {
    "*andsi_2_zext",
    "and{l}\t{%2, %k0|%k0, %2}",
    0,
    &operand_data[352],
    3,
    2,
    1,
    1
  },
  {
    "*andhi_1",
    (const PTR) output_292,
    0,
    &operand_data[605],
    3,
    0,
    3,
    3
  },
  {
    "*andhi_2",
    "and{w}\t{%2, %0|%0, %2}",
    0,
    &operand_data[608],
    3,
    2,
    2,
    1
  },
  {
    "*andqi_1",
    (const PTR) output_294,
    0,
    &operand_data[611],
    3,
    0,
    3,
    2
  },
  {
    "*andqi_1_slp",
    "and{b}\t{%1, %0|%0, %1}",
    0,
    &operand_data[614],
    2,
    1,
    2,
    1
  },
  {
    "*andqi_2",
    (const PTR) output_296,
    0,
    &operand_data[616],
    3,
    2,
    3,
    3
  },
  {
    "*andqi_2_slp",
    "and{b}\t{%1, %0|%0, %1}",
    0,
    &operand_data[619],
    2,
    3,
    2,
    1
  },
  {
    "andqi_ext_0",
    "and{b}\t{%2, %h0|%h0, %2}",
    (insn_gen_fn) gen_andqi_ext_0,
    &operand_data[621],
    3,
    0,
    1,
    1
  },
  {
    "*andqi_ext_0_cc",
    "and{b}\t{%2, %h0|%h0, %2}",
    0,
    &operand_data[621],
    3,
    2,
    1,
    1
  },
  {
    "*andqi_ext_1",
    "and{b}\t{%2, %h0|%h0, %2}",
    0,
    &operand_data[624],
    3,
    0,
    1,
    1
  },
  {
    "*andqi_ext_1_rex64",
    "and{b}\t{%2, %h0|%h0, %2}",
    0,
    &operand_data[627],
    3,
    0,
    1,
    1
  },
  {
    "*andqi_ext_2",
    "and{b}\t{%h2, %h0|%h0, %h2}",
    0,
    &operand_data[461],
    3,
    0,
    1,
    1
  },
  {
    "*iordi_1_rex64",
    "or{q}\t{%2, %0|%0, %2}",
    0,
    &operand_data[630],
    3,
    0,
    2,
    1
  },
  {
    "*iordi_2_rex64",
    "or{q}\t{%2, %0|%0, %2}",
    0,
    &operand_data[633],
    3,
    2,
    2,
    1
  },
  {
    "*iordi_3_rex64",
    "or{q}\t{%2, %0|%0, %2}",
    0,
    &operand_data[636],
    3,
    0,
    1,
    1
  },
  {
    "*iorsi_1",
    "or{l}\t{%2, %0|%0, %2}",
    0,
    &operand_data[639],
    3,
    0,
    2,
    1
  },
  {
    "*iorsi_1_zext",
    "or{l}\t{%2, %k0|%k0, %2}",
    0,
    &operand_data[642],
    3,
    0,
    1,
    1
  },
  {
    "*iorsi_1_zext_imm",
    "or{l}\t{%2, %k0|%k0, %2}",
    0,
    &operand_data[645],
    3,
    0,
    1,
    1
  },
  {
    "*iorsi_2",
    "or{l}\t{%2, %0|%0, %2}",
    0,
    &operand_data[602],
    3,
    2,
    2,
    1
  },
  {
    "*iorsi_2_zext",
    "or{l}\t{%2, %k0|%k0, %2}",
    0,
    &operand_data[352],
    3,
    2,
    1,
    1
  },
  {
    "*iorsi_2_zext_imm",
    "or{l}\t{%2, %k0|%k0, %2}",
    0,
    &operand_data[648],
    3,
    2,
    1,
    1
  },
  {
    "*iorsi_3",
    "or{l}\t{%2, %0|%0, %2}",
    0,
    &operand_data[651],
    3,
    0,
    1,
    1
  },
  {
    "*iorhi_1",
    "or{w}\t{%2, %0|%0, %2}",
    0,
    &operand_data[654],
    3,
    0,
    2,
    1
  },
  {
    "*iorhi_2",
    "or{w}\t{%2, %0|%0, %2}",
    0,
    &operand_data[608],
    3,
    2,
    2,
    1
  },
  {
    "*iorhi_3",
    "or{w}\t{%2, %0|%0, %2}",
    0,
    &operand_data[657],
    3,
    0,
    1,
    1
  },
  {
    "*iorqi_1",
    (const PTR) output_316,
    0,
    &operand_data[660],
    3,
    0,
    3,
    2
  },
  {
    "*iorqi_1_slp",
    "or{b}\t{%1, %0|%0, %1}",
    0,
    &operand_data[663],
    2,
    1,
    2,
    1
  },
  {
    "*iorqi_2",
    "or{b}\t{%2, %0|%0, %2}",
    0,
    &operand_data[665],
    3,
    2,
    2,
    1
  },
  {
    "*iorqi_2_slp",
    "or{b}\t{%1, %0|%0, %1}",
    0,
    &operand_data[668],
    2,
    3,
    2,
    1
  },
  {
    "*iorqi_3",
    "or{b}\t{%2, %0|%0, %2}",
    0,
    &operand_data[670],
    3,
    0,
    1,
    1
  },
  {
    "iorqi_ext_0",
    "or{b}\t{%2, %h0|%h0, %2}",
    (insn_gen_fn) gen_iorqi_ext_0,
    &operand_data[621],
    3,
    0,
    1,
    1
  },
  {
    "*iorqi_ext_1",
    "or{b}\t{%2, %h0|%h0, %2}",
    0,
    &operand_data[624],
    3,
    0,
    1,
    1
  },
  {
    "*iorqi_ext_1_rex64",
    "or{b}\t{%2, %h0|%h0, %2}",
    0,
    &operand_data[627],
    3,
    0,
    1,
    1
  },
  {
    "*iorqi_ext_2",
    "ior{b}\t{%h2, %h0|%h0, %h2}",
    0,
    &operand_data[627],
    3,
    0,
    1,
    1
  },
  {
    "*xordi_1_rex64",
    (const PTR) output_325,
    0,
    &operand_data[346],
    3,
    0,
    2,
    2
  },
  {
    "*xordi_2_rex64",
    (const PTR) output_326,
    0,
    &operand_data[633],
    3,
    2,
    2,
    2
  },
  {
    "*xordi_3_rex64",
    "xor{q}\t{%2, %0|%0, %2}",
    0,
    &operand_data[636],
    3,
    0,
    1,
    1
  },
  {
    "*xorsi_1",
    "xor{l}\t{%2, %0|%0, %2}",
    0,
    &operand_data[349],
    3,
    0,
    2,
    1
  },
  {
    "*xorsi_1_zext",
    "xor{l}\t{%2, %k0|%k0, %2}",
    0,
    &operand_data[352],
    3,
    0,
    1,
    1
  },
  {
    "*xorsi_1_zext_imm",
    "xor{l}\t{%2, %k0|%k0, %2}",
    0,
    &operand_data[673],
    3,
    0,
    1,
    1
  },
  {
    "*xorsi_2",
    "xor{l}\t{%2, %0|%0, %2}",
    0,
    &operand_data[602],
    3,
    2,
    2,
    1
  },
  {
    "*xorsi_2_zext",
    "xor{l}\t{%2, %k0|%k0, %2}",
    0,
    &operand_data[352],
    3,
    2,
    1,
    1
  },
  {
    "*xorsi_2_zext_imm",
    "xor{l}\t{%2, %k0|%k0, %2}",
    0,
    &operand_data[648],
    3,
    2,
    1,
    1
  },
  {
    "*xorsi_3",
    "xor{l}\t{%2, %0|%0, %2}",
    0,
    &operand_data[651],
    3,
    0,
    1,
    1
  },
  {
    "*xorhi_1",
    "xor{w}\t{%2, %0|%0, %2}",
    0,
    &operand_data[654],
    3,
    0,
    2,
    1
  },
  {
    "*xorhi_2",
    "xor{w}\t{%2, %0|%0, %2}",
    0,
    &operand_data[608],
    3,
    2,
    2,
    1
  },
  {
    "*xorhi_3",
    "xor{w}\t{%2, %0|%0, %2}",
    0,
    &operand_data[657],
    3,
    0,
    1,
    1
  },
  {
    "*xorqi_1",
    (const PTR) output_338,
    0,
    &operand_data[660],
    3,
    0,
    3,
    2
  },
  {
    "*xorqi_1_slp",
    "xor{b}\t{%1, %0|%0, %1}",
    0,
    &operand_data[614],
    2,
    1,
    2,
    1
  },
  {
    "xorqi_ext_0",
    "xor{b}\t{%2, %h0|%h0, %2}",
    (insn_gen_fn) gen_xorqi_ext_0,
    &operand_data[621],
    3,
    0,
    1,
    1
  },
  {
    "*xorqi_ext_1",
    "xor{b}\t{%2, %h0|%h0, %2}",
    0,
    &operand_data[624],
    3,
    0,
    1,
    1
  },
  {
    "*xorqi_ext_1_rex64",
    "xor{b}\t{%2, %h0|%h0, %2}",
    0,
    &operand_data[627],
    3,
    0,
    1,
    1
  },
  {
    "*xorqi_ext_2",
    "xor{b}\t{%h2, %h0|%h0, %h2}",
    0,
    &operand_data[627],
    3,
    0,
    1,
    1
  },
  {
    "*xorqi_cc_1",
    "xor{b}\t{%2, %0|%0, %2}",
    0,
    &operand_data[665],
    3,
    2,
    2,
    1
  },
  {
    "*xorqi_2_slp",
    "xor{b}\t{%1, %0|%0, %1}",
    0,
    &operand_data[668],
    2,
    3,
    2,
    1
  },
  {
    "*xorqi_cc_2",
    "xor{b}\t{%2, %0|%0, %2}",
    0,
    &operand_data[670],
    3,
    0,
    1,
    1
  },
  {
    "*xorqi_cc_ext_1",
    "xor{b}\t{%2, %h0|%h0, %2}",
    0,
    &operand_data[676],
    3,
    2,
    1,
    1
  },
  {
    "*xorqi_cc_ext_1_rex64",
    "xor{b}\t{%2, %h0|%h0, %2}",
    0,
    &operand_data[458],
    3,
    2,
    1,
    1
  },
  {
    "*negdi2_1",
    "#",
    0,
    &operand_data[679],
    2,
    0,
    1,
    1
  },
  {
    "*negdi2_1_rex64",
    "neg{q}\t%0",
    0,
    &operand_data[681],
    2,
    0,
    1,
    1
  },
  {
    "*negdi2_cmpz_rex64",
    "neg{q}\t%0",
    0,
    &operand_data[681],
    2,
    1,
    1,
    1
  },
  {
    "*negsi2_1",
    "neg{l}\t%0",
    0,
    &operand_data[683],
    2,
    0,
    1,
    1
  },
  {
    "*negsi2_1_zext",
    "neg{l}\t%k0",
    0,
    &operand_data[685],
    2,
    0,
    1,
    1
  },
  {
    "*negsi2_cmpz",
    "neg{l}\t%0",
    0,
    &operand_data[683],
    2,
    1,
    1,
    1
  },
  {
    "*negsi2_cmpz_zext",
    "neg{l}\t%k0",
    0,
    &operand_data[685],
    2,
    1,
    1,
    1
  },
  {
    "*neghi2_1",
    "neg{w}\t%0",
    0,
    &operand_data[687],
    2,
    0,
    1,
    1
  },
  {
    "*neghi2_cmpz",
    "neg{w}\t%0",
    0,
    &operand_data[687],
    2,
    1,
    1,
    1
  },
  {
    "*negqi2_1",
    "neg{b}\t%0",
    0,
    &operand_data[689],
    2,
    0,
    1,
    1
  },
  {
    "*negqi2_cmpz",
    "neg{b}\t%0",
    0,
    &operand_data[689],
    2,
    1,
    1,
    1
  },
  {
    "negsf2_memory",
    "#",
    (insn_gen_fn) gen_negsf2_memory,
    &operand_data[691],
    2,
    0,
    1,
    1
  },
  {
    "negsf2_ifs",
    "#",
    (insn_gen_fn) gen_negsf2_ifs,
    &operand_data[693],
    3,
    0,
    4,
    1
  },
  {
    "*negsf2_if",
    "#",
    0,
    &operand_data[696],
    2,
    0,
    2,
    1
  },
  {
    "negdf2_memory",
    "#",
    (insn_gen_fn) gen_negdf2_memory,
    &operand_data[698],
    2,
    0,
    1,
    1
  },
  {
    "negdf2_ifs",
    "#",
    (insn_gen_fn) gen_negdf2_ifs,
    &operand_data[700],
    3,
    0,
    4,
    1
  },
  {
    "*negdf2_ifs_rex64",
    "#",
    0,
    &operand_data[703],
    3,
    0,
    3,
    1
  },
  {
    "*negdf2_if",
    "#",
    0,
    &operand_data[706],
    2,
    0,
    2,
    1
  },
  {
    "*negdf2_if_rex64",
    "#",
    0,
    &operand_data[708],
    2,
    0,
    2,
    1
  },
  {
    "*negxf2_if",
    "#",
    0,
    &operand_data[710],
    2,
    0,
    2,
    1
  },
  {
    "*negtf2_if",
    "#",
    0,
    &operand_data[712],
    2,
    0,
    2,
    1
  },
  {
    "*negsf2_1",
    "fchs",
    0,
    &operand_data[714],
    2,
    0,
    1,
    1
  },
  {
    "*negdf2_1",
    "fchs",
    0,
    &operand_data[716],
    2,
    0,
    1,
    1
  },
  {
    "*negextendsfdf2",
    "fchs",
    0,
    &operand_data[718],
    2,
    0,
    1,
    1
  },
  {
    "*negxf2_1",
    "fchs",
    0,
    &operand_data[720],
    2,
    0,
    1,
    1
  },
  {
    "*negextenddfxf2",
    "fchs",
    0,
    &operand_data[722],
    2,
    0,
    1,
    1
  },
  {
    "*negextendsfxf2",
    "fchs",
    0,
    &operand_data[724],
    2,
    0,
    1,
    1
  },
  {
    "*negtf2_1",
    "fchs",
    0,
    &operand_data[726],
    2,
    0,
    1,
    1
  },
  {
    "*negextenddftf2",
    "fchs",
    0,
    &operand_data[728],
    2,
    0,
    1,
    1
  },
  {
    "*negextendsftf2",
    "fchs",
    0,
    &operand_data[730],
    2,
    0,
    1,
    1
  },
  {
    "abssf2_memory",
    "#",
    (insn_gen_fn) gen_abssf2_memory,
    &operand_data[691],
    2,
    0,
    1,
    1
  },
  {
    "abssf2_ifs",
    "#",
    (insn_gen_fn) gen_abssf2_ifs,
    &operand_data[732],
    3,
    0,
    3,
    1
  },
  {
    "*abssf2_if",
    "#",
    0,
    &operand_data[696],
    2,
    0,
    2,
    1
  },
  {
    "absdf2_memory",
    "#",
    (insn_gen_fn) gen_absdf2_memory,
    &operand_data[698],
    2,
    0,
    1,
    1
  },
  {
    "absdf2_ifs",
    "#",
    (insn_gen_fn) gen_absdf2_ifs,
    &operand_data[735],
    3,
    0,
    3,
    1
  },
  {
    "*absdf2_ifs_rex64",
    "#",
    0,
    &operand_data[738],
    3,
    0,
    2,
    1
  },
  {
    "*absdf2_if",
    "#",
    0,
    &operand_data[706],
    2,
    0,
    2,
    1
  },
  {
    "*absdf2_if_rex64",
    "#",
    0,
    &operand_data[708],
    2,
    0,
    2,
    1
  },
  {
    "*absxf2_if",
    "#",
    0,
    &operand_data[710],
    2,
    0,
    2,
    1
  },
  {
    "*abstf2_if",
    "#",
    0,
    &operand_data[712],
    2,
    0,
    2,
    1
  },
  {
    "*abssf2_1",
    "fabs",
    0,
    &operand_data[714],
    2,
    0,
    1,
    1
  },
  {
    "*absdf2_1",
    "fabs",
    0,
    &operand_data[716],
    2,
    0,
    1,
    1
  },
  {
    "*absextendsfdf2",
    "fabs",
    0,
    &operand_data[718],
    2,
    0,
    1,
    1
  },
  {
    "*absxf2_1",
    "fabs",
    0,
    &operand_data[720],
    2,
    0,
    1,
    1
  },
  {
    "*absextenddfxf2",
    "fabs",
    0,
    &operand_data[722],
    2,
    0,
    1,
    1
  },
  {
    "*absextendsfxf2",
    "fabs",
    0,
    &operand_data[724],
    2,
    0,
    1,
    1
  },
  {
    "*abstf2_1",
    "fabs",
    0,
    &operand_data[726],
    2,
    0,
    1,
    1
  },
  {
    "*absextenddftf2",
    "fabs",
    0,
    &operand_data[728],
    2,
    0,
    1,
    1
  },
  {
    "*absextendsftf2",
    "fabs",
    0,
    &operand_data[730],
    2,
    0,
    1,
    1
  },
  {
    "*one_cmpldi2_1_rex64",
    "not{q}\t%0",
    0,
    &operand_data[681],
    2,
    0,
    1,
    1
  },
  {
    "*one_cmpldi2_2_rex64",
    "#",
    0,
    &operand_data[681],
    2,
    1,
    1,
    1
  },
  {
    "*one_cmplsi2_1",
    "not{l}\t%0",
    0,
    &operand_data[683],
    2,
    0,
    1,
    1
  },
  {
    "*one_cmplsi2_1_zext",
    "not{l}\t%k0",
    0,
    &operand_data[476],
    2,
    0,
    1,
    1
  },
  {
    "*one_cmplsi2_2",
    "#",
    0,
    &operand_data[683],
    2,
    1,
    1,
    1
  },
  {
    "*one_cmplsi2_2_zext",
    "#",
    0,
    &operand_data[476],
    2,
    1,
    1,
    1
  },
  {
    "*one_cmplhi2_1",
    "not{w}\t%0",
    0,
    &operand_data[687],
    2,
    0,
    1,
    1
  },
  {
    "*one_cmplhi2_2",
    "#",
    0,
    &operand_data[687],
    2,
    1,
    1,
    1
  },
  {
    "*one_cmplqi2_1",
    (const PTR) output_406,
    0,
    &operand_data[741],
    2,
    0,
    2,
    2
  },
  {
    "*one_cmplqi2_2",
    "#",
    0,
    &operand_data[689],
    2,
    1,
    1,
    1
  },
  {
    "*ashldi3_1_rex64",
    (const PTR) output_408,
    0,
    &operand_data[743],
    3,
    0,
    2,
    3
  },
  {
    "*ashldi3_cmp_rex64",
    (const PTR) output_409,
    0,
    &operand_data[746],
    3,
    2,
    1,
    3
  },
  {
    "ashldi3_1",
    "#",
    (insn_gen_fn) gen_ashldi3_1,
    &operand_data[749],
    4,
    0,
    1,
    1
  },
  {
    "*ashldi3_2",
    "#",
    0,
    &operand_data[749],
    3,
    0,
    1,
    1
  },
  {
    "x86_shld_1",
    (const PTR) output_412,
    (insn_gen_fn) gen_x86_shld_1,
    &operand_data[753],
    3,
    2,
    2,
    2
  },
  {
    "*ashlsi3_1",
    (const PTR) output_413,
    0,
    &operand_data[756],
    3,
    0,
    2,
    3
  },
  {
    "*ashlsi3_1_zext",
    (const PTR) output_414,
    0,
    &operand_data[759],
    3,
    0,
    2,
    3
  },
  {
    "*ashlsi3_cmp",
    (const PTR) output_415,
    0,
    &operand_data[762],
    3,
    2,
    1,
    3
  },
  {
    "*ashlsi3_cmp_zext",
    (const PTR) output_416,
    0,
    &operand_data[765],
    3,
    2,
    1,
    3
  },
  {
    "*ashlhi3_1_lea",
    (const PTR) output_417,
    0,
    &operand_data[768],
    3,
    0,
    2,
    3
  },
  {
    "*ashlhi3_1",
    (const PTR) output_418,
    0,
    &operand_data[771],
    3,
    0,
    1,
    3
  },
  {
    "*ashlhi3_cmp",
    (const PTR) output_419,
    0,
    &operand_data[774],
    3,
    2,
    1,
    3
  },
  {
    "*ashlqi3_1_lea",
    (const PTR) output_420,
    0,
    &operand_data[777],
    3,
    0,
    3,
    3
  },
  {
    "*ashlqi3_1",
    (const PTR) output_421,
    0,
    &operand_data[780],
    3,
    0,
    2,
    3
  },
  {
    "*ashlqi3_cmp",
    (const PTR) output_422,
    0,
    &operand_data[783],
    3,
    2,
    1,
    3
  },
  {
    "ashrdi3_63_rex64",
    (const PTR) output_423,
    (insn_gen_fn) gen_ashrdi3_63_rex64,
    &operand_data[786],
    3,
    0,
    2,
    2
  },
  {
    "*ashrdi3_1_one_bit_rex64",
    "sar{q}\t%0",
    0,
    &operand_data[789],
    3,
    0,
    1,
    1
  },
  {
    "*ashrdi3_1_rex64",
    (const PTR) output_425,
    0,
    &operand_data[792],
    3,
    0,
    2,
    2
  },
  {
    "*ashrdi3_one_bit_cmp_rex64",
    "sar{q}\t%0",
    0,
    &operand_data[789],
    3,
    2,
    1,
    1
  },
  {
    "*ashrdi3_cmp_rex64",
    "sar{q}\t{%2, %0|%0, %2}",
    0,
    &operand_data[795],
    3,
    2,
    1,
    1
  },
  {
    "ashrdi3_1",
    "#",
    (insn_gen_fn) gen_ashrdi3_1,
    &operand_data[749],
    4,
    0,
    1,
    1
  },
  {
    "*ashrdi3_2",
    "#",
    0,
    &operand_data[749],
    3,
    0,
    1,
    1
  },
  {
    "x86_shrd_1",
    (const PTR) output_430,
    (insn_gen_fn) gen_x86_shrd_1,
    &operand_data[753],
    3,
    2,
    2,
    2
  },
  {
    "ashrsi3_31",
    (const PTR) output_431,
    (insn_gen_fn) gen_ashrsi3_31,
    &operand_data[798],
    3,
    0,
    2,
    2
  },
  {
    "*ashrsi3_31_zext",
    (const PTR) output_432,
    0,
    &operand_data[801],
    3,
    0,
    2,
    2
  },
  {
    "*ashrsi3_1_one_bit",
    "sar{l}\t%0",
    0,
    &operand_data[804],
    3,
    0,
    1,
    1
  },
  {
    "*ashrsi3_1_one_bit_zext",
    "sar{l}\t%k0",
    0,
    &operand_data[807],
    3,
    0,
    1,
    1
  },
  {
    "*ashrsi3_1",
    (const PTR) output_435,
    0,
    &operand_data[810],
    3,
    0,
    2,
    2
  },
  {
    "*ashrsi3_1_zext",
    (const PTR) output_436,
    0,
    &operand_data[813],
    3,
    0,
    2,
    2
  },
  {
    "*ashrsi3_one_bit_cmp",
    "sar{l}\t%0",
    0,
    &operand_data[804],
    3,
    2,
    1,
    1
  },
  {
    "*ashrsi3_one_bit_cmp_zext",
    "sar{l}\t%k0",
    0,
    &operand_data[807],
    3,
    2,
    1,
    1
  },
  {
    "*ashrsi3_cmp",
    "sar{l}\t{%2, %0|%0, %2}",
    0,
    &operand_data[762],
    3,
    2,
    1,
    1
  },
  {
    "*ashrsi3_cmp_zext",
    "sar{l}\t{%2, %k0|%k0, %2}",
    0,
    &operand_data[765],
    3,
    2,
    1,
    1
  },
  {
    "*ashrhi3_1_one_bit",
    "sar{w}\t%0",
    0,
    &operand_data[816],
    3,
    0,
    1,
    1
  },
  {
    "*ashrhi3_1",
    (const PTR) output_442,
    0,
    &operand_data[819],
    3,
    0,
    2,
    2
  },
  {
    "*ashrhi3_one_bit_cmp",
    "sar{w}\t%0",
    0,
    &operand_data[816],
    3,
    2,
    1,
    1
  },
  {
    "*ashrhi3_cmp",
    "sar{w}\t{%2, %0|%0, %2}",
    0,
    &operand_data[774],
    3,
    2,
    1,
    1
  },
  {
    "*ashrqi3_1_one_bit",
    "sar{b}\t%0",
    0,
    &operand_data[822],
    3,
    0,
    1,
    1
  },
  {
    "*ashrqi3_1_one_bit_slp",
    "sar{b}\t%0",
    0,
    &operand_data[825],
    2,
    1,
    1,
    1
  },
  {
    "*ashrqi3_1",
    (const PTR) output_447,
    0,
    &operand_data[827],
    3,
    0,
    2,
    2
  },
  {
    "*ashrqi3_1_slp",
    (const PTR) output_448,
    0,
    &operand_data[830],
    2,
    1,
    2,
    2
  },
  {
    "*ashrqi3_one_bit_cmp",
    "sar{b}\t%0",
    0,
    &operand_data[832],
    3,
    2,
    1,
    1
  },
  {
    "*ashrqi3_cmp",
    "sar{b}\t{%2, %0|%0, %2}",
    0,
    &operand_data[783],
    3,
    2,
    1,
    1
  },
  {
    "*lshrdi3_1_one_bit_rex64",
    "shr{q}\t%0",
    0,
    &operand_data[789],
    3,
    0,
    1,
    1
  },
  {
    "*lshrdi3_1_rex64",
    (const PTR) output_452,
    0,
    &operand_data[792],
    3,
    0,
    2,
    2
  },
  {
    "*lshrdi3_cmp_one_bit_rex64",
    "shr{q}\t%0",
    0,
    &operand_data[789],
    3,
    2,
    1,
    1
  },
  {
    "*lshrdi3_cmp_rex64",
    "shr{q}\t{%2, %0|%0, %2}",
    0,
    &operand_data[835],
    3,
    2,
    1,
    1
  },
  {
    "lshrdi3_1",
    "#",
    (insn_gen_fn) gen_lshrdi3_1,
    &operand_data[749],
    4,
    0,
    1,
    1
  },
  {
    "*lshrdi3_2",
    "#",
    0,
    &operand_data[749],
    3,
    0,
    1,
    1
  },
  {
    "*lshrsi3_1_one_bit",
    "shr{l}\t%0",
    0,
    &operand_data[804],
    3,
    0,
    1,
    1
  },
  {
    "*lshrsi3_1_one_bit_zext",
    "shr{l}\t%k0",
    0,
    &operand_data[807],
    3,
    0,
    1,
    1
  },
  {
    "*lshrsi3_1",
    (const PTR) output_459,
    0,
    &operand_data[810],
    3,
    0,
    2,
    2
  },
  {
    "*lshrsi3_1_zext",
    (const PTR) output_460,
    0,
    &operand_data[838],
    3,
    0,
    2,
    2
  },
  {
    "*lshrsi3_one_bit_cmp",
    "shr{l}\t%0",
    0,
    &operand_data[804],
    3,
    2,
    1,
    1
  },
  {
    "*lshrsi3_cmp_one_bit_zext",
    "shr{l}\t%k0",
    0,
    &operand_data[807],
    3,
    2,
    1,
    1
  },
  {
    "*lshrsi3_cmp",
    "shr{l}\t{%2, %0|%0, %2}",
    0,
    &operand_data[762],
    3,
    2,
    1,
    1
  },
  {
    "*lshrsi3_cmp_zext",
    "shr{l}\t{%2, %k0|%k0, %2}",
    0,
    &operand_data[765],
    3,
    2,
    1,
    1
  },
  {
    "*lshrhi3_1_one_bit",
    "shr{w}\t%0",
    0,
    &operand_data[816],
    3,
    0,
    1,
    1
  },
  {
    "*lshrhi3_1",
    (const PTR) output_466,
    0,
    &operand_data[819],
    3,
    0,
    2,
    2
  },
  {
    "*lshrhi3_one_bit_cmp",
    "shr{w}\t%0",
    0,
    &operand_data[816],
    3,
    2,
    1,
    1
  },
  {
    "*lshrhi3_cmp",
    "shr{w}\t{%2, %0|%0, %2}",
    0,
    &operand_data[774],
    3,
    2,
    1,
    1
  },
  {
    "*lshrqi3_1_one_bit",
    "shr{b}\t%0",
    0,
    &operand_data[822],
    3,
    0,
    1,
    1
  },
  {
    "*lshrqi3_1_one_bit_slp",
    "shr{b}\t%0",
    0,
    &operand_data[825],
    2,
    1,
    1,
    1
  },
  {
    "*lshrqi3_1",
    (const PTR) output_471,
    0,
    &operand_data[827],
    3,
    0,
    2,
    2
  },
  {
    "*lshrqi3_1_slp",
    (const PTR) output_472,
    0,
    &operand_data[830],
    2,
    1,
    2,
    2
  },
  {
    "*lshrqi2_one_bit_cmp",
    "shr{b}\t%0",
    0,
    &operand_data[822],
    3,
    2,
    1,
    1
  },
  {
    "*lshrqi2_cmp",
    "shr{b}\t{%2, %0|%0, %2}",
    0,
    &operand_data[783],
    3,
    2,
    1,
    1
  },
  {
    "*rotlsi3_1_one_bit_rex64",
    "rol{q}\t%0",
    0,
    &operand_data[789],
    3,
    0,
    1,
    1
  },
  {
    "*rotldi3_1_rex64",
    (const PTR) output_476,
    0,
    &operand_data[841],
    3,
    0,
    2,
    2
  },
  {
    "*rotlsi3_1_one_bit",
    "rol{l}\t%0",
    0,
    &operand_data[804],
    3,
    0,
    1,
    1
  },
  {
    "*rotlsi3_1_one_bit_zext",
    "rol{l}\t%k0",
    0,
    &operand_data[807],
    3,
    0,
    1,
    1
  },
  {
    "*rotlsi3_1",
    (const PTR) output_479,
    0,
    &operand_data[810],
    3,
    0,
    2,
    2
  },
  {
    "*rotlsi3_1_zext",
    (const PTR) output_480,
    0,
    &operand_data[813],
    3,
    0,
    2,
    2
  },
  {
    "*rotlhi3_1_one_bit",
    "rol{w}\t%0",
    0,
    &operand_data[816],
    3,
    0,
    1,
    1
  },
  {
    "*rotlhi3_1",
    (const PTR) output_482,
    0,
    &operand_data[819],
    3,
    0,
    2,
    2
  },
  {
    "*rotlqi3_1_one_bit_slp",
    "rol{b}\t%0",
    0,
    &operand_data[825],
    2,
    1,
    1,
    1
  },
  {
    "*rotlqi3_1_one_bit",
    "rol{b}\t%0",
    0,
    &operand_data[822],
    3,
    0,
    1,
    1
  },
  {
    "*rotlqi3_1_slp",
    (const PTR) output_485,
    0,
    &operand_data[830],
    2,
    1,
    2,
    2
  },
  {
    "*rotlqi3_1",
    (const PTR) output_486,
    0,
    &operand_data[827],
    3,
    0,
    2,
    2
  },
  {
    "*rotrdi3_1_one_bit_rex64",
    "ror{q}\t%0",
    0,
    &operand_data[789],
    3,
    0,
    1,
    1
  },
  {
    "*rotrdi3_1_rex64",
    (const PTR) output_488,
    0,
    &operand_data[792],
    3,
    0,
    2,
    2
  },
  {
    "*rotrsi3_1_one_bit",
    "ror{l}\t%0",
    0,
    &operand_data[804],
    3,
    0,
    1,
    1
  },
  {
    "*rotrsi3_1_one_bit_zext",
    "ror{l}\t%k0",
    0,
    &operand_data[807],
    3,
    0,
    1,
    1
  },
  {
    "*rotrsi3_1",
    (const PTR) output_491,
    0,
    &operand_data[810],
    3,
    0,
    2,
    2
  },
  {
    "*rotrsi3_1_zext",
    (const PTR) output_492,
    0,
    &operand_data[813],
    3,
    0,
    2,
    2
  },
  {
    "*rotrhi3_one_bit",
    "ror{w}\t%0",
    0,
    &operand_data[816],
    3,
    0,
    1,
    1
  },
  {
    "*rotrhi3",
    (const PTR) output_494,
    0,
    &operand_data[819],
    3,
    0,
    2,
    2
  },
  {
    "*rotrqi3_1_one_bit",
    "ror{b}\t%0",
    0,
    &operand_data[822],
    3,
    0,
    1,
    1
  },
  {
    "*rotrqi3_1_one_bit_slp",
    "ror{b}\t%0",
    0,
    &operand_data[825],
    2,
    1,
    1,
    1
  },
  {
    "*rotrqi3_1",
    (const PTR) output_497,
    0,
    &operand_data[827],
    3,
    0,
    2,
    2
  },
  {
    "*rotrqi3_1_slp",
    (const PTR) output_498,
    0,
    &operand_data[830],
    2,
    1,
    2,
    2
  },
  {
    "*setcc_1",
    "set%C1\t%0",
    0,
    &operand_data[844],
    2,
    0,
    1,
    1
  },
  {
    "setcc_2",
    "set%C1\t%0",
    (insn_gen_fn) gen_setcc_2,
    &operand_data[846],
    2,
    0,
    1,
    1
  },
  {
    "*sse_setccsf",
    "cmp%D1ss\t{%3, %0|%0, %3}",
    0,
    &operand_data[848],
    4,
    0,
    1,
    1
  },
  {
    "*sse_setccdf",
    "cmp%D1sd\t{%3, %0|%0, %3}",
    0,
    &operand_data[852],
    4,
    0,
    1,
    1
  },
  {
    "*jcc_1",
    "%+j%C1\t%l0",
    0,
    &operand_data[856],
    2,
    0,
    0,
    1
  },
  {
    "*jcc_2",
    "%+j%c1\t%l0",
    0,
    &operand_data[856],
    2,
    0,
    0,
    1
  },
  {
    "*fp_jcc_1",
    "#",
    0,
    &operand_data[858],
    4,
    0,
    1,
    1
  },
  {
    "*fp_jcc_1_sse",
    "#",
    0,
    &operand_data[862],
    4,
    0,
    2,
    1
  },
  {
    "*fp_jcc_1_sse_only",
    "#",
    0,
    &operand_data[866],
    4,
    0,
    1,
    1
  },
  {
    "*fp_jcc_2",
    "#",
    0,
    &operand_data[858],
    4,
    0,
    1,
    1
  },
  {
    "*fp_jcc_2_sse",
    "#",
    0,
    &operand_data[862],
    4,
    0,
    2,
    1
  },
  {
    "*fp_jcc_2_sse_only",
    "#",
    0,
    &operand_data[866],
    4,
    0,
    1,
    1
  },
  {
    "*fp_jcc_3",
    "#",
    0,
    &operand_data[870],
    5,
    0,
    1,
    1
  },
  {
    "*fp_jcc_4",
    "#",
    0,
    &operand_data[870],
    5,
    0,
    1,
    1
  },
  {
    "*fp_jcc_5",
    "#",
    0,
    &operand_data[875],
    5,
    0,
    1,
    1
  },
  {
    "*fp_jcc_6",
    "#",
    0,
    &operand_data[875],
    5,
    0,
    1,
    1
  },
  {
    "jump",
    "jmp\t%l0",
    (insn_gen_fn) gen_jump,
    &operand_data[856],
    1,
    0,
    0,
    1
  },
  {
    "*indirect_jump",
    "jmp\t%A0",
    0,
    &operand_data[516],
    1,
    0,
    1,
    1
  },
  {
    "*indirect_jump_rtx64",
    "jmp\t%A0",
    0,
    &operand_data[513],
    1,
    0,
    1,
    1
  },
  {
    "*tablejump_1",
    "jmp\t%A0",
    0,
    &operand_data[880],
    2,
    0,
    1,
    1
  },
  {
    "*tablejump_1_rtx64",
    "jmp\t%A0",
    0,
    &operand_data[882],
    2,
    0,
    1,
    1
  },
  {
    "doloop_end_internal",
    (const PTR) output_520,
    (insn_gen_fn) gen_doloop_end_internal,
    &operand_data[883],
    4,
    1,
    3,
    3
  },
  {
    "*call_pop_0",
    (const PTR) output_521,
    0,
    &operand_data[887],
    3,
    0,
    0,
    3
  },
  {
    "*call_pop_1",
    (const PTR) output_522,
    0,
    &operand_data[890],
    3,
    0,
    1,
    3
  },
  {
    "*call_0",
    (const PTR) output_523,
    0,
    &operand_data[893],
    2,
    0,
    0,
    3
  },
  {
    "*call_1",
    (const PTR) output_524,
    0,
    &operand_data[895],
    2,
    0,
    1,
    3
  },
  {
    "*call_1_rex64",
    (const PTR) output_525,
    0,
    &operand_data[897],
    2,
    0,
    1,
    3
  },
  {
    "blockage",
    "",
    (insn_gen_fn) gen_blockage,
    &operand_data[856],
    1,
    0,
    0,
    1
  },
  {
    "return_internal",
    "ret",
    (insn_gen_fn) gen_return_internal,
    &operand_data[0],
    0,
    0,
    0,
    1
  },
  {
    "return_pop_internal",
    "ret\t%0",
    (insn_gen_fn) gen_return_pop_internal,
    &operand_data[588],
    1,
    0,
    0,
    1
  },
  {
    "return_indirect_internal",
    "jmp\t%A0",
    (insn_gen_fn) gen_return_indirect_internal,
    &operand_data[370],
    1,
    0,
    1,
    1
  },
  {
    "nop",
    "nop",
    (insn_gen_fn) gen_nop,
    &operand_data[0],
    0,
    0,
    0,
    1
  },
  {
    "set_got",
    (const PTR) output_531,
    (insn_gen_fn) gen_set_got,
    &operand_data[68],
    1,
    0,
    1,
    3
  },
  {
    "eh_return_si",
    "#",
    (insn_gen_fn) gen_eh_return_si,
    &operand_data[899],
    1,
    0,
    1,
    1
  },
  {
    "eh_return_di",
    "#",
    (insn_gen_fn) gen_eh_return_di,
    &operand_data[900],
    1,
    0,
    1,
    1
  },
  {
    "leave",
    "leave",
    (insn_gen_fn) gen_leave,
    &operand_data[0],
    0,
    0,
    0,
    1
  },
  {
    "leave_rex64",
    "leave",
    (insn_gen_fn) gen_leave_rex64,
    &operand_data[0],
    0,
    0,
    0,
    1
  },
  {
    "ffssi_1",
    "bsf{l}\t{%1, %0|%0, %1}",
    (insn_gen_fn) gen_ffssi_1,
    &operand_data[901],
    2,
    1,
    1,
    1
  },
  {
    "*tls_global_dynamic_32_gnu",
    "lea{l}\t{%a2@TLSGD(,%1,1), %0|%0, %a2@TLSGD[%1*1]}\n\tcall\t%P3",
    0,
    &operand_data[903],
    6,
    0,
    1,
    1
  },
  {
    "*tls_global_dynamic_32_sun",
    "lea{l}\t{%a2@DTLNDX(%1), %4|%4, %a2@DTLNDX[%1]}\n\
	push{l}\t%4\n\tcall\t%a2@TLSPLT\n\tpop{l}\t%4\n\tnop",
    0,
    &operand_data[903],
    6,
    0,
    1,
    1
  },
  {
    "*tls_global_dynamic_64",
    ".byte\t0x66\n\tlea{q}\t{%a1@TLSGD(%%rip), %%rdi|%%rdi, %a1@TLSGD[%%rip]}\n\t.word\t0x6666\n\trex64\n\tcall\t%P2",
    0,
    &operand_data[909],
    4,
    0,
    1,
    1
  },
  {
    "*tls_local_dynamic_base_32_gnu",
    "lea{l}\t{%&@TLSLDM(%1), %0|%0, %&@TLSLDM[%1]}\n\tcall\t%P2",
    0,
    &operand_data[913],
    5,
    0,
    1,
    1
  },
  {
    "*tls_local_dynamic_base_32_sun",
    "lea{l}\t{%&@TMDNX(%1), %3|%3, %&@TMDNX[%1]}\n\
	push{l}\t%3\n\tcall\t%&@TLSPLT\n\tpop{l}\t%3",
    0,
    &operand_data[913],
    5,
    0,
    1,
    1
  },
  {
    "*tls_local_dynamic_base_64",
    "lea{q}\t{%&@TLSLD(%%rip), %%rdi|%%rdi, %&@TLSLD[%%rip]}\n\tcall\t%P1",
    0,
    &operand_data[918],
    3,
    0,
    1,
    1
  },
  {
    "*tls_local_dynamic_32_once",
    "#",
    0,
    &operand_data[921],
    6,
    0,
    1,
    1
  },
  {
    "*load_tp_si",
    "mov{l}\t{%%gs:0, %0|%0, DWORD PTR %%gs:0}",
    0,
    &operand_data[220],
    1,
    0,
    2,
    1
  },
  {
    "*load_tp_di",
    "mov{q}\t{%%fs:0, %0|%0, DWORD PTR %%fs:0}",
    0,
    &operand_data[135],
    1,
    0,
    1,
    1
  },
  {
    "*add_tp_si",
    "add{l}\t{%%gs:0, %0|%0, DWORD PTR %%gs:0}",
    0,
    &operand_data[927],
    2,
    0,
    1,
    1
  },
  {
    "*add_tp_di",
    "add{q}\t{%%fs:0, %0|%0, DWORD PTR %%fs:0}",
    0,
    &operand_data[685],
    2,
    0,
    1,
    1
  },
  {
    "*fop_sf_comm_nosse",
    (const PTR) output_548,
    0,
    &operand_data[929],
    4,
    0,
    1,
    3
  },
  {
    "*fop_sf_comm",
    (const PTR) output_549,
    0,
    &operand_data[933],
    4,
    0,
    2,
    3
  },
  {
    "*fop_sf_comm_sse",
    (const PTR) output_550,
    0,
    &operand_data[937],
    4,
    0,
    1,
    3
  },
  {
    "*fop_df_comm_nosse",
    (const PTR) output_551,
    0,
    &operand_data[941],
    4,
    0,
    1,
    3
  },
  {
    "*fop_df_comm",
    (const PTR) output_552,
    0,
    &operand_data[945],
    4,
    0,
    2,
    3
  },
  {
    "*fop_df_comm_sse",
    (const PTR) output_553,
    0,
    &operand_data[949],
    4,
    0,
    1,
    3
  },
  {
    "*fop_xf_comm",
    (const PTR) output_554,
    0,
    &operand_data[953],
    4,
    0,
    1,
    3
  },
  {
    "*fop_tf_comm",
    (const PTR) output_555,
    0,
    &operand_data[957],
    4,
    0,
    1,
    3
  },
  {
    "*fop_sf_1_nosse",
    (const PTR) output_556,
    0,
    &operand_data[961],
    4,
    0,
    2,
    3
  },
  {
    "*fop_sf_1",
    (const PTR) output_557,
    0,
    &operand_data[965],
    4,
    0,
    3,
    3
  },
  {
    "*fop_sf_1_sse",
    (const PTR) output_558,
    0,
    &operand_data[969],
    4,
    0,
    1,
    3
  },
  {
    "*fop_sf_2",
    (const PTR) output_559,
    0,
    &operand_data[973],
    4,
    0,
    2,
    3
  },
  {
    "*fop_sf_3",
    (const PTR) output_560,
    0,
    &operand_data[977],
    4,
    0,
    2,
    3
  },
  {
    "*fop_df_1_nosse",
    (const PTR) output_561,
    0,
    &operand_data[981],
    4,
    0,
    2,
    3
  },
  {
    "*fop_df_1",
    (const PTR) output_562,
    0,
    &operand_data[985],
    4,
    0,
    3,
    3
  },
  {
    "*fop_df_1_sse",
    (const PTR) output_563,
    0,
    &operand_data[989],
    4,
    0,
    1,
    3
  },
  {
    "*fop_df_2",
    (const PTR) output_564,
    0,
    &operand_data[993],
    4,
    0,
    2,
    3
  },
  {
    "*fop_df_3",
    (const PTR) output_565,
    0,
    &operand_data[997],
    4,
    0,
    2,
    3
  },
  {
    "*fop_df_4",
    (const PTR) output_566,
    0,
    &operand_data[1001],
    4,
    0,
    2,
    3
  },
  {
    "*fop_df_5",
    (const PTR) output_567,
    0,
    &operand_data[1005],
    4,
    0,
    2,
    3
  },
  {
    "*fop_xf_1",
    (const PTR) output_568,
    0,
    &operand_data[1009],
    4,
    0,
    2,
    3
  },
  {
    "*fop_tf_1",
    (const PTR) output_569,
    0,
    &operand_data[1013],
    4,
    0,
    2,
    3
  },
  {
    "*fop_xf_2",
    (const PTR) output_570,
    0,
    &operand_data[1017],
    4,
    0,
    2,
    3
  },
  {
    "*fop_tf_2",
    (const PTR) output_571,
    0,
    &operand_data[1021],
    4,
    0,
    2,
    3
  },
  {
    "*fop_xf_3",
    (const PTR) output_572,
    0,
    &operand_data[1025],
    4,
    0,
    2,
    3
  },
  {
    "*fop_tf_3",
    (const PTR) output_573,
    0,
    &operand_data[1029],
    4,
    0,
    2,
    3
  },
  {
    "*fop_xf_4",
    (const PTR) output_574,
    0,
    &operand_data[1033],
    4,
    0,
    2,
    3
  },
  {
    "*fop_tf_4",
    (const PTR) output_575,
    0,
    &operand_data[1037],
    4,
    0,
    2,
    3
  },
  {
    "*fop_xf_5",
    (const PTR) output_576,
    0,
    &operand_data[1041],
    4,
    0,
    2,
    3
  },
  {
    "*fop_tf_5",
    (const PTR) output_577,
    0,
    &operand_data[1045],
    4,
    0,
    2,
    3
  },
  {
    "*fop_xf_6",
    (const PTR) output_578,
    0,
    &operand_data[1049],
    4,
    0,
    2,
    3
  },
  {
    "*fop_tf_6",
    (const PTR) output_579,
    0,
    &operand_data[1053],
    4,
    0,
    2,
    3
  },
  {
    "*fop_xf_7",
    (const PTR) output_580,
    0,
    &operand_data[1057],
    4,
    0,
    2,
    3
  },
  {
    "*fop_tf_7",
    (const PTR) output_581,
    0,
    &operand_data[1061],
    4,
    0,
    2,
    3
  },
  {
    "sqrtsf2_1",
    (const PTR) output_582,
    (insn_gen_fn) gen_sqrtsf2_1,
    &operand_data[1065],
    2,
    0,
    2,
    2
  },
  {
    "sqrtsf2_1_sse_only",
    "sqrtss\t{%1, %0|%0, %1}",
    (insn_gen_fn) gen_sqrtsf2_1_sse_only,
    &operand_data[1067],
    2,
    0,
    1,
    1
  },
  {
    "sqrtsf2_i387",
    "fsqrt",
    (insn_gen_fn) gen_sqrtsf2_i387,
    &operand_data[714],
    2,
    0,
    1,
    1
  },
  {
    "sqrtdf2_1",
    (const PTR) output_585,
    (insn_gen_fn) gen_sqrtdf2_1,
    &operand_data[1069],
    2,
    0,
    2,
    2
  },
  {
    "sqrtdf2_1_sse_only",
    "sqrtsd\t{%1, %0|%0, %1}",
    (insn_gen_fn) gen_sqrtdf2_1_sse_only,
    &operand_data[1071],
    2,
    0,
    1,
    1
  },
  {
    "sqrtdf2_i387",
    "fsqrt",
    (insn_gen_fn) gen_sqrtdf2_i387,
    &operand_data[716],
    2,
    0,
    1,
    1
  },
  {
    "*sqrtextendsfdf2",
    "fsqrt",
    0,
    &operand_data[718],
    2,
    0,
    1,
    1
  },
  {
    "sqrtxf2",
    "fsqrt",
    (insn_gen_fn) gen_sqrtxf2,
    &operand_data[720],
    2,
    0,
    1,
    1
  },
  {
    "sqrttf2",
    "fsqrt",
    (insn_gen_fn) gen_sqrttf2,
    &operand_data[726],
    2,
    0,
    1,
    1
  },
  {
    "*sqrtextenddfxf2",
    "fsqrt",
    0,
    &operand_data[722],
    2,
    0,
    1,
    1
  },
  {
    "*sqrtextenddftf2",
    "fsqrt",
    0,
    &operand_data[728],
    2,
    0,
    1,
    1
  },
  {
    "*sqrtextendsfxf2",
    "fsqrt",
    0,
    &operand_data[724],
    2,
    0,
    1,
    1
  },
  {
    "*sqrtextendsftf2",
    "fsqrt",
    0,
    &operand_data[730],
    2,
    0,
    1,
    1
  },
  {
    "sindf2",
    "fsin",
    (insn_gen_fn) gen_sindf2,
    &operand_data[716],
    2,
    0,
    1,
    1
  },
  {
    "sinsf2",
    "fsin",
    (insn_gen_fn) gen_sinsf2,
    &operand_data[714],
    2,
    0,
    1,
    1
  },
  {
    "*sinextendsfdf2",
    "fsin",
    0,
    &operand_data[718],
    2,
    0,
    1,
    1
  },
  {
    "sinxf2",
    "fsin",
    (insn_gen_fn) gen_sinxf2,
    &operand_data[720],
    2,
    0,
    1,
    1
  },
  {
    "sintf2",
    "fsin",
    (insn_gen_fn) gen_sintf2,
    &operand_data[726],
    2,
    0,
    1,
    1
  },
  {
    "cosdf2",
    "fcos",
    (insn_gen_fn) gen_cosdf2,
    &operand_data[716],
    2,
    0,
    1,
    1
  },
  {
    "cossf2",
    "fcos",
    (insn_gen_fn) gen_cossf2,
    &operand_data[714],
    2,
    0,
    1,
    1
  },
  {
    "*cosextendsfdf2",
    "fcos",
    0,
    &operand_data[718],
    2,
    0,
    1,
    1
  },
  {
    "cosxf2",
    "fcos",
    (insn_gen_fn) gen_cosxf2,
    &operand_data[720],
    2,
    0,
    1,
    1
  },
  {
    "costf2",
    "fcos",
    (insn_gen_fn) gen_costf2,
    &operand_data[726],
    2,
    0,
    1,
    1
  },
  {
    "cld",
    "cld",
    (insn_gen_fn) gen_cld,
    &operand_data[0],
    0,
    0,
    0,
    1
  },
  {
    "strmovdi_rex_1",
    "movsq",
    (insn_gen_fn) gen_strmovdi_rex_1,
    &operand_data[1073],
    4,
    2,
    1,
    1
  },
  {
    "strmovsi_1",
    "{movsl|movsd}",
    (insn_gen_fn) gen_strmovsi_1,
    &operand_data[1077],
    4,
    2,
    1,
    1
  },
  {
    "strmovsi_rex_1",
    "{movsl|movsd}",
    (insn_gen_fn) gen_strmovsi_rex_1,
    &operand_data[1073],
    4,
    2,
    1,
    1
  },
  {
    "strmovhi_1",
    "movsw",
    (insn_gen_fn) gen_strmovhi_1,
    &operand_data[1077],
    4,
    2,
    1,
    1
  },
  {
    "strmovhi_rex_1",
    "movsw",
    (insn_gen_fn) gen_strmovhi_rex_1,
    &operand_data[1073],
    4,
    2,
    1,
    1
  },
  {
    "strmovqi_1",
    "movsb",
    (insn_gen_fn) gen_strmovqi_1,
    &operand_data[1077],
    4,
    2,
    1,
    1
  },
  {
    "strmovqi_rex_1",
    "movsb",
    (insn_gen_fn) gen_strmovqi_rex_1,
    &operand_data[1073],
    4,
    2,
    1,
    1
  },
  {
    "rep_movdi_rex64",
    "{rep\n\tmovsq|rep movsq}",
    (insn_gen_fn) gen_rep_movdi_rex64,
    &operand_data[1081],
    6,
    4,
    1,
    1
  },
  {
    "rep_movsi",
    "{rep\n\tmovsl|rep movsd}",
    (insn_gen_fn) gen_rep_movsi,
    &operand_data[1087],
    6,
    4,
    1,
    1
  },
  {
    "rep_movsi_rex64",
    "{rep\n\tmovsl|rep movsd}",
    (insn_gen_fn) gen_rep_movsi_rex64,
    &operand_data[1081],
    6,
    4,
    1,
    1
  },
  {
    "rep_movqi",
    "{rep\n\tmovsb|rep movsb}",
    (insn_gen_fn) gen_rep_movqi,
    &operand_data[1087],
    6,
    4,
    1,
    1
  },
  {
    "rep_movqi_rex64",
    "{rep\n\tmovsb|rep movsb}",
    (insn_gen_fn) gen_rep_movqi_rex64,
    &operand_data[1081],
    6,
    4,
    1,
    1
  },
  {
    "strsetdi_rex_1",
    "stosq",
    (insn_gen_fn) gen_strsetdi_rex_1,
    &operand_data[1093],
    3,
    1,
    1,
    1
  },
  {
    "strsetsi_1",
    "{stosl|stosd}",
    (insn_gen_fn) gen_strsetsi_1,
    &operand_data[1096],
    3,
    1,
    1,
    1
  },
  {
    "strsetsi_rex_1",
    "{stosl|stosd}",
    (insn_gen_fn) gen_strsetsi_rex_1,
    &operand_data[1093],
    3,
    1,
    1,
    1
  },
  {
    "strsethi_1",
    "stosw",
    (insn_gen_fn) gen_strsethi_1,
    &operand_data[1099],
    3,
    1,
    1,
    1
  },
  {
    "strsethi_rex_1",
    "stosw",
    (insn_gen_fn) gen_strsethi_rex_1,
    &operand_data[1102],
    3,
    1,
    1,
    1
  },
  {
    "strsetqi_1",
    "stosb",
    (insn_gen_fn) gen_strsetqi_1,
    &operand_data[1105],
    3,
    1,
    1,
    1
  },
  {
    "strsetqi_rex_1",
    "stosb",
    (insn_gen_fn) gen_strsetqi_rex_1,
    &operand_data[1108],
    3,
    1,
    1,
    1
  },
  {
    "rep_stosdi_rex64",
    "{rep\n\tstosq|rep stosq}",
    (insn_gen_fn) gen_rep_stosdi_rex64,
    &operand_data[1111],
    5,
    2,
    1,
    1
  },
  {
    "rep_stossi",
    "{rep\n\tstosl|rep stosd}",
    (insn_gen_fn) gen_rep_stossi,
    &operand_data[1116],
    5,
    2,
    1,
    1
  },
  {
    "rep_stossi_rex64",
    "{rep\n\tstosl|rep stosd}",
    (insn_gen_fn) gen_rep_stossi_rex64,
    &operand_data[1121],
    5,
    2,
    1,
    1
  },
  {
    "rep_stosqi",
    "{rep\n\tstosb|rep stosb}",
    (insn_gen_fn) gen_rep_stosqi,
    &operand_data[1126],
    5,
    2,
    1,
    1
  },
  {
    "rep_stosqi_rex64",
    "{rep\n\tstosb|rep stosb}",
    (insn_gen_fn) gen_rep_stosqi_rex64,
    &operand_data[1131],
    5,
    2,
    1,
    1
  },
  {
    "cmpstrqi_nz_1",
    "repz{\n\t| }cmpsb",
    (insn_gen_fn) gen_cmpstrqi_nz_1,
    &operand_data[1136],
    7,
    0,
    1,
    1
  },
  {
    "cmpstrqi_nz_rex_1",
    "repz{\n\t| }cmpsb",
    (insn_gen_fn) gen_cmpstrqi_nz_rex_1,
    &operand_data[1143],
    7,
    0,
    1,
    1
  },
  {
    "cmpstrqi_1",
    "repz{\n\t| }cmpsb",
    (insn_gen_fn) gen_cmpstrqi_1,
    &operand_data[1136],
    7,
    0,
    1,
    1
  },
  {
    "cmpstrqi_rex_1",
    "repz{\n\t| }cmpsb",
    (insn_gen_fn) gen_cmpstrqi_rex_1,
    &operand_data[1143],
    7,
    0,
    1,
    1
  },
  {
    "strlenqi_1",
    "repnz{\n\t| }scasb",
    (insn_gen_fn) gen_strlenqi_1,
    &operand_data[1150],
    6,
    0,
    1,
    1
  },
  {
    "strlenqi_rex_1",
    "repnz{\n\t| }scasb",
    (insn_gen_fn) gen_strlenqi_rex_1,
    &operand_data[1156],
    6,
    0,
    1,
    1
  },
  {
    "x86_movdicc_0_m1_rex64",
    "sbb{q}\t%0, %0",
    (insn_gen_fn) gen_x86_movdicc_0_m1_rex64,
    &operand_data[135],
    1,
    0,
    1,
    1
  },
  {
    "*movdicc_c_rex64",
    (const PTR) output_637,
    0,
    &operand_data[1162],
    4,
    0,
    2,
    2
  },
  {
    "x86_movsicc_0_m1",
    "sbb{l}\t%0, %0",
    (insn_gen_fn) gen_x86_movsicc_0_m1,
    &operand_data[68],
    1,
    0,
    1,
    1
  },
  {
    "*movsicc_noc",
    (const PTR) output_639,
    0,
    &operand_data[1166],
    4,
    0,
    2,
    2
  },
  {
    "*movhicc_noc",
    (const PTR) output_640,
    0,
    &operand_data[1170],
    4,
    0,
    2,
    2
  },
  {
    "*movsfcc_1",
    (const PTR) output_641,
    0,
    &operand_data[1174],
    4,
    0,
    4,
    2
  },
  {
    "*movdfcc_1",
    (const PTR) output_642,
    0,
    &operand_data[1178],
    4,
    0,
    4,
    2
  },
  {
    "*movdfcc_1_rex64",
    (const PTR) output_643,
    0,
    &operand_data[1178],
    4,
    0,
    4,
    2
  },
  {
    "*movxfcc_1",
    (const PTR) output_644,
    0,
    &operand_data[1182],
    4,
    0,
    2,
    2
  },
  {
    "*movtfcc_1",
    (const PTR) output_645,
    0,
    &operand_data[1186],
    4,
    0,
    2,
    2
  },
  {
    "*minsf",
    "#",
    0,
    &operand_data[1190],
    3,
    2,
    3,
    1
  },
  {
    "*minsf_nonieee",
    "#",
    0,
    &operand_data[1193],
    3,
    2,
    2,
    1
  },
  {
    "*minsf_sse",
    "minss\t{%2, %0|%0, %2}",
    0,
    &operand_data[969],
    3,
    2,
    1,
    1
  },
  {
    "*mindf",
    "#",
    0,
    &operand_data[1196],
    3,
    2,
    3,
    1
  },
  {
    "*mindf_nonieee",
    "#",
    0,
    &operand_data[1199],
    3,
    2,
    2,
    1
  },
  {
    "*mindf_sse",
    "minsd\t{%2, %0|%0, %2}",
    0,
    &operand_data[989],
    3,
    2,
    1,
    1
  },
  {
    "*maxsf",
    "#",
    0,
    &operand_data[1190],
    3,
    2,
    3,
    1
  },
  {
    "*maxsf_nonieee",
    "#",
    0,
    &operand_data[1193],
    3,
    2,
    2,
    1
  },
  {
    "*maxsf_sse",
    "maxss\t{%2, %0|%0, %2}",
    0,
    &operand_data[969],
    3,
    2,
    1,
    1
  },
  {
    "*maxdf",
    "#",
    0,
    &operand_data[1196],
    3,
    2,
    3,
    1
  },
  {
    "*maxdf_nonieee",
    "#",
    0,
    &operand_data[1199],
    3,
    2,
    2,
    1
  },
  {
    "*maxdf_sse",
    "maxsd\t{%2, %0|%0, %2}",
    0,
    &operand_data[989],
    3,
    2,
    1,
    1
  },
  {
    "*pro_epilogue_adjust_stack_1",
    (const PTR) output_658,
    0,
    &operand_data[1202],
    3,
    0,
    2,
    3
  },
  {
    "pro_epilogue_adjust_stack_rex64",
    (const PTR) output_659,
    (insn_gen_fn) gen_pro_epilogue_adjust_stack_rex64,
    &operand_data[1205],
    3,
    0,
    2,
    3
  },
  {
    "sse_movsfcc",
    "#",
    (insn_gen_fn) gen_sse_movsfcc,
    &operand_data[1208],
    7,
    0,
    10,
    1
  },
  {
    "sse_movsfcc_eq",
    "#",
    (insn_gen_fn) gen_sse_movsfcc_eq,
    &operand_data[1215],
    6,
    0,
    6,
    1
  },
  {
    "sse_movdfcc",
    "#",
    (insn_gen_fn) gen_sse_movdfcc,
    &operand_data[1221],
    7,
    0,
    10,
    1
  },
  {
    "sse_movdfcc_eq",
    "#",
    (insn_gen_fn) gen_sse_movdfcc_eq,
    &operand_data[1228],
    6,
    0,
    6,
    1
  },
  {
    "*sse_movsfcc_const0_1",
    "#",
    0,
    &operand_data[1234],
    6,
    0,
    1,
    1
  },
  {
    "*sse_movsfcc_const0_2",
    "#",
    0,
    &operand_data[1240],
    6,
    0,
    1,
    1
  },
  {
    "*sse_movsfcc_const0_3",
    "#",
    0,
    &operand_data[1246],
    6,
    0,
    1,
    1
  },
  {
    "*sse_movsfcc_const0_4",
    "#",
    0,
    &operand_data[1252],
    6,
    0,
    1,
    1
  },
  {
    "*sse_movdfcc_const0_1",
    "#",
    0,
    &operand_data[1258],
    6,
    0,
    1,
    1
  },
  {
    "*sse_movdfcc_const0_2",
    "#",
    0,
    &operand_data[1264],
    6,
    0,
    1,
    1
  },
  {
    "*sse_movdfcc_const0_3",
    "#",
    0,
    &operand_data[1270],
    6,
    0,
    1,
    1
  },
  {
    "*sse_movdfcc_const0_4",
    "#",
    0,
    &operand_data[1276],
    6,
    0,
    1,
    1
  },
  {
    "allocate_stack_worker_1",
    "call\t__alloca",
    (insn_gen_fn) gen_allocate_stack_worker_1,
    &operand_data[551],
    1,
    2,
    1,
    1
  },
  {
    "allocate_stack_worker_rex64",
    "call\t__alloca",
    (insn_gen_fn) gen_allocate_stack_worker_rex64,
    &operand_data[538],
    1,
    2,
    1,
    1
  },
  {
    "*call_value_pop_0",
    (const PTR) output_674,
    0,
    &operand_data[1282],
    4,
    0,
    0,
    3
  },
  {
    "*call_value_pop_1",
    (const PTR) output_675,
    0,
    &operand_data[1286],
    4,
    0,
    1,
    3
  },
  {
    "*call_value_0",
    (const PTR) output_676,
    0,
    &operand_data[1282],
    3,
    0,
    0,
    3
  },
  {
    "*call_value_0_rex64",
    (const PTR) output_677,
    0,
    &operand_data[1290],
    3,
    0,
    0,
    3
  },
  {
    "*call_value_1",
    (const PTR) output_678,
    0,
    &operand_data[1286],
    3,
    0,
    1,
    3
  },
  {
    "*call_value_1_rex64",
    (const PTR) output_679,
    0,
    &operand_data[1293],
    3,
    0,
    1,
    3
  },
  {
    "trap",
    "int\t$5",
    (insn_gen_fn) gen_trap,
    &operand_data[0],
    0,
    0,
    0,
    1
  },
  {
    "*conditional_trap_1",
    (const PTR) output_681,
    0,
    &operand_data[1296],
    2,
    0,
    0,
    3
  },
  {
    "movv4sf_internal",
    (const PTR) output_682,
    (insn_gen_fn) gen_movv4sf_internal,
    &operand_data[1298],
    2,
    0,
    3,
    2
  },
  {
    "movv4si_internal",
    (const PTR) output_683,
    (insn_gen_fn) gen_movv4si_internal,
    &operand_data[1300],
    2,
    0,
    3,
    2
  },
  {
    "movv2di_internal",
    (const PTR) output_684,
    (insn_gen_fn) gen_movv2di_internal,
    &operand_data[1302],
    2,
    0,
    3,
    2
  },
  {
    "movv8qi_internal",
    (const PTR) output_685,
    (insn_gen_fn) gen_movv8qi_internal,
    &operand_data[1304],
    2,
    0,
    3,
    2
  },
  {
    "movv4hi_internal",
    (const PTR) output_686,
    (insn_gen_fn) gen_movv4hi_internal,
    &operand_data[1306],
    2,
    0,
    3,
    2
  },
  {
    "movv2si_internal",
    (const PTR) output_687,
    (insn_gen_fn) gen_movv2si_internal,
    &operand_data[1308],
    2,
    0,
    3,
    2
  },
  {
    "movv2sf_internal",
    (const PTR) output_688,
    (insn_gen_fn) gen_movv2sf_internal,
    &operand_data[1310],
    2,
    0,
    3,
    2
  },
  {
    "movv2df_internal",
    (const PTR) output_689,
    (insn_gen_fn) gen_movv2df_internal,
    &operand_data[1312],
    2,
    0,
    3,
    2
  },
  {
    "movv8hi_internal",
    (const PTR) output_690,
    (insn_gen_fn) gen_movv8hi_internal,
    &operand_data[1314],
    2,
    0,
    3,
    2
  },
  {
    "movv16qi_internal",
    (const PTR) output_691,
    (insn_gen_fn) gen_movv16qi_internal,
    &operand_data[1316],
    2,
    0,
    3,
    2
  },
  {
    "*pushv2df",
    "#",
    0,
    &operand_data[1318],
    2,
    0,
    1,
    1
  },
  {
    "*pushv2di",
    "#",
    0,
    &operand_data[1320],
    2,
    0,
    1,
    1
  },
  {
    "*pushv8hi",
    "#",
    0,
    &operand_data[1322],
    2,
    0,
    1,
    1
  },
  {
    "*pushv16qi",
    "#",
    0,
    &operand_data[1324],
    2,
    0,
    1,
    1
  },
  {
    "*pushv4sf",
    "#",
    0,
    &operand_data[1326],
    2,
    0,
    1,
    1
  },
  {
    "*pushv4si",
    "#",
    0,
    &operand_data[1328],
    2,
    0,
    1,
    1
  },
  {
    "*pushv2si",
    "#",
    0,
    &operand_data[1330],
    2,
    0,
    1,
    1
  },
  {
    "*pushv4hi",
    "#",
    0,
    &operand_data[1332],
    2,
    0,
    1,
    1
  },
  {
    "*pushv8qi",
    "#",
    0,
    &operand_data[1334],
    2,
    0,
    1,
    1
  },
  {
    "*pushv2sf",
    "#",
    0,
    &operand_data[1336],
    2,
    0,
    1,
    1
  },
  {
    "*pushti",
    "#",
    0,
    &operand_data[1338],
    2,
    0,
    1,
    1
  },
  {
    "*pushv2df",
    "#",
    0,
    &operand_data[1340],
    2,
    0,
    1,
    1
  },
  {
    "*pushv2di",
    "#",
    0,
    &operand_data[1342],
    2,
    0,
    1,
    1
  },
  {
    "*pushv8hi",
    "#",
    0,
    &operand_data[1344],
    2,
    0,
    1,
    1
  },
  {
    "*pushv16qi",
    "#",
    0,
    &operand_data[1346],
    2,
    0,
    1,
    1
  },
  {
    "*pushv4sf",
    "#",
    0,
    &operand_data[1348],
    2,
    0,
    1,
    1
  },
  {
    "*pushv4si",
    "#",
    0,
    &operand_data[1350],
    2,
    0,
    1,
    1
  },
  {
    "*pushv2si",
    "#",
    0,
    &operand_data[1352],
    2,
    0,
    1,
    1
  },
  {
    "*pushv4hi",
    "#",
    0,
    &operand_data[1354],
    2,
    0,
    1,
    1
  },
  {
    "*pushv8qi",
    "#",
    0,
    &operand_data[1356],
    2,
    0,
    1,
    1
  },
  {
    "*pushv2sf",
    "#",
    0,
    &operand_data[1358],
    2,
    0,
    1,
    1
  },
  {
    "movti_internal",
    (const PTR) output_713,
    (insn_gen_fn) gen_movti_internal,
    &operand_data[1360],
    2,
    0,
    3,
    2
  },
  {
    "*movti_rex64",
    (const PTR) output_714,
    0,
    &operand_data[1362],
    2,
    0,
    5,
    2
  },
  {
    "*sse_movaps_1",
    "movaps\t{%1, %0|%0, %1}",
    0,
    &operand_data[1364],
    2,
    0,
    2,
    1
  },
  {
    "*sse_movups_1",
    "movups\t{%1, %0|%0, %1}",
    0,
    &operand_data[1364],
    2,
    0,
    2,
    1
  },
  {
    "sse_movmskps",
    "movmskps\t{%1, %0|%0, %1}",
    (insn_gen_fn) gen_sse_movmskps,
    &operand_data[1366],
    2,
    0,
    1,
    1
  },
  {
    "mmx_pmovmskb",
    "pmovmskb\t{%1, %0|%0, %1}",
    (insn_gen_fn) gen_mmx_pmovmskb,
    &operand_data[1368],
    2,
    0,
    1,
    1
  },
  {
    "mmx_maskmovq",
    "maskmovq\t{%2, %1|%1, %2}",
    (insn_gen_fn) gen_mmx_maskmovq,
    &operand_data[1370],
    3,
    0,
    1,
    1
  },
  {
    "mmx_maskmovq_rex",
    "maskmovq\t{%2, %1|%1, %2}",
    (insn_gen_fn) gen_mmx_maskmovq_rex,
    &operand_data[1373],
    3,
    0,
    1,
    1
  },
  {
    "sse_movntv4sf",
    "movntps\t{%1, %0|%0, %1}",
    (insn_gen_fn) gen_sse_movntv4sf,
    &operand_data[1376],
    2,
    0,
    1,
    1
  },
  {
    "sse_movntdi",
    "movntq\t{%1, %0|%0, %1}",
    (insn_gen_fn) gen_sse_movntdi,
    &operand_data[1378],
    2,
    0,
    1,
    1
  },
  {
    "sse_movhlps",
    "movhlps\t{%2, %0|%0, %2}",
    (insn_gen_fn) gen_sse_movhlps,
    &operand_data[1380],
    3,
    0,
    1,
    1
  },
  {
    "sse_movlhps",
    "movlhps\t{%2, %0|%0, %2}",
    (insn_gen_fn) gen_sse_movlhps,
    &operand_data[1380],
    3,
    0,
    1,
    1
  },
  {
    "sse_movhps",
    "movhps\t{%2, %0|%0, %2}",
    (insn_gen_fn) gen_sse_movhps,
    &operand_data[1383],
    3,
    0,
    2,
    1
  },
  {
    "sse_movlps",
    "movlps\t{%2, %0|%0, %2}",
    (insn_gen_fn) gen_sse_movlps,
    &operand_data[1383],
    3,
    0,
    2,
    1
  },
  {
    "sse_loadss_1",
    "movss\t{%1, %0|%0, %1}",
    (insn_gen_fn) gen_sse_loadss_1,
    &operand_data[1386],
    3,
    0,
    1,
    1
  },
  {
    "sse_movss",
    "movss\t{%2, %0|%0, %2}",
    (insn_gen_fn) gen_sse_movss,
    &operand_data[1380],
    3,
    0,
    1,
    1
  },
  {
    "sse_storess",
    "movss\t{%1, %0|%0, %1}",
    (insn_gen_fn) gen_sse_storess,
    &operand_data[1389],
    2,
    0,
    1,
    1
  },
  {
    "sse_shufps",
    "shufps\t{%3, %2, %0|%0, %2, %3}",
    (insn_gen_fn) gen_sse_shufps,
    &operand_data[1391],
    4,
    0,
    1,
    1
  },
  {
    "addv4sf3",
    "addps\t{%2, %0|%0, %2}",
    (insn_gen_fn) gen_addv4sf3,
    &operand_data[1391],
    3,
    0,
    1,
    1
  },
  {
    "vmaddv4sf3",
    "addss\t{%2, %0|%0, %2}",
    (insn_gen_fn) gen_vmaddv4sf3,
    &operand_data[1391],
    3,
    1,
    1,
    1
  },
  {
    "subv4sf3",
    "subps\t{%2, %0|%0, %2}",
    (insn_gen_fn) gen_subv4sf3,
    &operand_data[1391],
    3,
    0,
    1,
    1
  },
  {
    "vmsubv4sf3",
    "subss\t{%2, %0|%0, %2}",
    (insn_gen_fn) gen_vmsubv4sf3,
    &operand_data[1391],
    3,
    1,
    1,
    1
  },
  {
    "mulv4sf3",
    "mulps\t{%2, %0|%0, %2}",
    (insn_gen_fn) gen_mulv4sf3,
    &operand_data[1391],
    3,
    0,
    1,
    1
  },
  {
    "vmmulv4sf3",
    "mulss\t{%2, %0|%0, %2}",
    (insn_gen_fn) gen_vmmulv4sf3,
    &operand_data[1391],
    3,
    1,
    1,
    1
  },
  {
    "divv4sf3",
    "divps\t{%2, %0|%0, %2}",
    (insn_gen_fn) gen_divv4sf3,
    &operand_data[1391],
    3,
    0,
    1,
    1
  },
  {
    "vmdivv4sf3",
    "divss\t{%2, %0|%0, %2}",
    (insn_gen_fn) gen_vmdivv4sf3,
    &operand_data[1391],
    3,
    1,
    1,
    1
  },
  {
    "rcpv4sf2",
    "rcpps\t{%1, %0|%0, %1}",
    (insn_gen_fn) gen_rcpv4sf2,
    &operand_data[1395],
    2,
    0,
    1,
    1
  },
  {
    "vmrcpv4sf2",
    "rcpss\t{%1, %0|%0, %1}",
    (insn_gen_fn) gen_vmrcpv4sf2,
    &operand_data[1395],
    3,
    0,
    1,
    1
  },
  {
    "rsqrtv4sf2",
    "rsqrtps\t{%1, %0|%0, %1}",
    (insn_gen_fn) gen_rsqrtv4sf2,
    &operand_data[1395],
    2,
    0,
    1,
    1
  },
  {
    "vmrsqrtv4sf2",
    "rsqrtss\t{%1, %0|%0, %1}",
    (insn_gen_fn) gen_vmrsqrtv4sf2,
    &operand_data[1395],
    3,
    0,
    1,
    1
  },
  {
    "sqrtv4sf2",
    "sqrtps\t{%1, %0|%0, %1}",
    (insn_gen_fn) gen_sqrtv4sf2,
    &operand_data[1395],
    2,
    0,
    1,
    1
  },
  {
    "vmsqrtv4sf2",
    "sqrtss\t{%1, %0|%0, %1}",
    (insn_gen_fn) gen_vmsqrtv4sf2,
    &operand_data[1395],
    3,
    0,
    1,
    1
  },
  {
    "*sse_andv4sf3",
    "andps\t{%2, %0|%0, %2}",
    0,
    &operand_data[1398],
    3,
    0,
    1,
    1
  },
  {
    "*sse_andsf3",
    "andps\t{%2, %0|%0, %2}",
    0,
    &operand_data[1401],
    3,
    0,
    1,
    1
  },
  {
    "*sse_nandv4sf3",
    "andnps\t{%2, %0|%0, %2}",
    0,
    &operand_data[1404],
    3,
    0,
    1,
    1
  },
  {
    "*sse_nandsf3",
    "andnps\t{%2, %0|%0, %2}",
    0,
    &operand_data[1407],
    3,
    0,
    1,
    1
  },
  {
    "*sse_iorv4sf3",
    "orps\t{%2, %0|%0, %2}",
    0,
    &operand_data[1398],
    3,
    0,
    1,
    1
  },
  {
    "*sse_iorsf3",
    "orps\t{%2, %0|%0, %2}",
    0,
    &operand_data[1401],
    3,
    0,
    1,
    1
  },
  {
    "*sse_xorv4sf3",
    "xorps\t{%2, %0|%0, %2}",
    0,
    &operand_data[1398],
    3,
    0,
    1,
    1
  },
  {
    "*sse_xorsf3",
    "xorps\t{%2, %0|%0, %2}",
    0,
    &operand_data[1401],
    3,
    0,
    1,
    1
  },
  {
    "*sse2_andv2df3",
    "andpd\t{%2, %0|%0, %2}",
    0,
    &operand_data[1410],
    3,
    0,
    1,
    1
  },
  {
    "*sse2_andv2df3",
    "andpd\t{%2, %0|%0, %2}",
    0,
    &operand_data[1413],
    3,
    0,
    1,
    1
  },
  {
    "*sse2_nandv2df3",
    "andnpd\t{%2, %0|%0, %2}",
    0,
    &operand_data[1416],
    3,
    0,
    1,
    1
  },
  {
    "*sse_nandti3_df",
    "andnpd\t{%2, %0|%0, %2}",
    0,
    &operand_data[1419],
    3,
    0,
    1,
    1
  },
  {
    "*sse2_iorv2df3",
    "orpd\t{%2, %0|%0, %2}",
    0,
    &operand_data[1410],
    3,
    0,
    1,
    1
  },
  {
    "*sse2_iordf3",
    "orpd\t{%2, %0|%0, %2}",
    0,
    &operand_data[1413],
    3,
    0,
    1,
    1
  },
  {
    "*sse2_xorv2df3",
    "xorpd\t{%2, %0|%0, %2}",
    0,
    &operand_data[1410],
    3,
    0,
    1,
    1
  },
  {
    "*sse2_xordf3",
    "xorpd\t{%2, %0|%0, %2}",
    0,
    &operand_data[1413],
    3,
    0,
    1,
    1
  },
  {
    "*sse2_andti3",
    "pand\t{%2, %0|%0, %2}",
    0,
    &operand_data[1422],
    3,
    0,
    1,
    1
  },
  {
    "sse2_andv2di3",
    "pand\t{%2, %0|%0, %2}",
    (insn_gen_fn) gen_sse2_andv2di3,
    &operand_data[1425],
    3,
    0,
    1,
    1
  },
  {
    "*sse2_nandti3",
    "pandn\t{%2, %0|%0, %2}",
    0,
    &operand_data[1428],
    3,
    0,
    1,
    1
  },
  {
    "sse2_nandv2di3",
    "pandn\t{%2, %0|%0, %2}",
    (insn_gen_fn) gen_sse2_nandv2di3,
    &operand_data[1431],
    3,
    0,
    1,
    1
  },
  {
    "*sse2_iorti3",
    "por\t{%2, %0|%0, %2}",
    0,
    &operand_data[1422],
    3,
    0,
    1,
    1
  },
  {
    "sse2_iorv2di3",
    "por\t{%2, %0|%0, %2}",
    (insn_gen_fn) gen_sse2_iorv2di3,
    &operand_data[1425],
    3,
    0,
    1,
    1
  },
  {
    "*sse2_xorti3",
    "pxor\t{%2, %0|%0, %2}",
    0,
    &operand_data[1422],
    3,
    0,
    1,
    1
  },
  {
    "sse2_xorv2di3",
    "pxor\t{%2, %0|%0, %2}",
    (insn_gen_fn) gen_sse2_xorv2di3,
    &operand_data[1425],
    3,
    0,
    1,
    1
  },
  {
    "sse_clrv4sf",
    "xorps\t{%0, %0|%0, %0}",
    (insn_gen_fn) gen_sse_clrv4sf,
    &operand_data[1380],
    1,
    0,
    1,
    1
  },
  {
    "sse_clrv2df",
    "xorpd\t{%0, %0|%0, %0}",
    (insn_gen_fn) gen_sse_clrv2df,
    &operand_data[1410],
    1,
    0,
    1,
    1
  },
  {
    "maskcmpv4sf3",
    "cmp%D3ps\t{%2, %0|%0, %2}",
    (insn_gen_fn) gen_maskcmpv4sf3,
    &operand_data[1434],
    4,
    0,
    1,
    1
  },
  {
    "maskncmpv4sf3",
    (const PTR) output_772,
    (insn_gen_fn) gen_maskncmpv4sf3,
    &operand_data[1434],
    4,
    0,
    1,
    3
  },
  {
    "vmmaskcmpv4sf3",
    "cmp%D3ss\t{%2, %0|%0, %2}",
    (insn_gen_fn) gen_vmmaskcmpv4sf3,
    &operand_data[1434],
    4,
    1,
    1,
    1
  },
  {
    "vmmaskncmpv4sf3",
    (const PTR) output_774,
    (insn_gen_fn) gen_vmmaskncmpv4sf3,
    &operand_data[1434],
    4,
    1,
    1,
    3
  },
  {
    "sse_comi",
    "comiss\t{%1, %0|%0, %1}",
    (insn_gen_fn) gen_sse_comi,
    &operand_data[1438],
    2,
    0,
    1,
    1
  },
  {
    "sse_ucomi",
    "ucomiss\t{%1, %0|%0, %1}",
    (insn_gen_fn) gen_sse_ucomi,
    &operand_data[1438],
    2,
    0,
    1,
    1
  },
  {
    "sse_unpckhps",
    "unpckhps\t{%2, %0|%0, %2}",
    (insn_gen_fn) gen_sse_unpckhps,
    &operand_data[1380],
    3,
    0,
    1,
    1
  },
  {
    "sse_unpcklps",
    "unpcklps\t{%2, %0|%0, %2}",
    (insn_gen_fn) gen_sse_unpcklps,
    &operand_data[1380],
    3,
    0,
    1,
    1
  },
  {
    "smaxv4sf3",
    "maxps\t{%2, %0|%0, %2}",
    (insn_gen_fn) gen_smaxv4sf3,
    &operand_data[1391],
    3,
    0,
    1,
    1
  },
  {
    "vmsmaxv4sf3",
    "maxss\t{%2, %0|%0, %2}",
    (insn_gen_fn) gen_vmsmaxv4sf3,
    &operand_data[1391],
    3,
    1,
    1,
    1
  },
  {
    "sminv4sf3",
    "minps\t{%2, %0|%0, %2}",
    (insn_gen_fn) gen_sminv4sf3,
    &operand_data[1391],
    3,
    0,
    1,
    1
  },
  {
    "vmsminv4sf3",
    "minss\t{%2, %0|%0, %2}",
    (insn_gen_fn) gen_vmsminv4sf3,
    &operand_data[1391],
    3,
    1,
    1,
    1
  },
  {
    "cvtpi2ps",
    "cvtpi2ps\t{%2, %0|%0, %2}",
    (insn_gen_fn) gen_cvtpi2ps,
    &operand_data[1440],
    3,
    0,
    1,
    1
  },
  {
    "cvtps2pi",
    "cvtps2pi\t{%1, %0|%0, %1}",
    (insn_gen_fn) gen_cvtps2pi,
    &operand_data[1443],
    2,
    0,
    1,
    1
  },
  {
    "cvttps2pi",
    "cvttps2pi\t{%1, %0|%0, %1}",
    (insn_gen_fn) gen_cvttps2pi,
    &operand_data[1443],
    2,
    0,
    1,
    1
  },
  {
    "cvtsi2ss",
    "cvtsi2ss\t{%2, %0|%0, %2}",
    (insn_gen_fn) gen_cvtsi2ss,
    &operand_data[1445],
    3,
    0,
    1,
    1
  },
  {
    "cvtsi2ssq",
    "cvtsi2ssq\t{%2, %0|%0, %2}",
    (insn_gen_fn) gen_cvtsi2ssq,
    &operand_data[1448],
    3,
    0,
    2,
    1
  },
  {
    "cvtss2si",
    "cvtss2si\t{%1, %0|%0, %1}",
    (insn_gen_fn) gen_cvtss2si,
    &operand_data[1451],
    2,
    0,
    1,
    1
  },
  {
    "cvtss2siq",
    "cvtss2siq\t{%1, %0|%0, %1}",
    (insn_gen_fn) gen_cvtss2siq,
    &operand_data[1453],
    2,
    0,
    2,
    1
  },
  {
    "cvttss2si",
    "cvttss2si\t{%1, %0|%0, %1}",
    (insn_gen_fn) gen_cvttss2si,
    &operand_data[1451],
    2,
    0,
    1,
    1
  },
  {
    "cvttss2siq",
    "cvttss2siq\t{%1, %0|%0, %1}",
    (insn_gen_fn) gen_cvttss2siq,
    &operand_data[1455],
    2,
    0,
    2,
    1
  },
  {
    "addv8qi3",
    "paddb\t{%2, %0|%0, %2}",
    (insn_gen_fn) gen_addv8qi3,
    &operand_data[1457],
    3,
    0,
    1,
    1
  },
  {
    "addv4hi3",
    "paddw\t{%2, %0|%0, %2}",
    (insn_gen_fn) gen_addv4hi3,
    &operand_data[1460],
    3,
    0,
    1,
    1
  },
  {
    "addv2si3",
    "paddd\t{%2, %0|%0, %2}",
    (insn_gen_fn) gen_addv2si3,
    &operand_data[1463],
    3,
    0,
    1,
    1
  },
  {
    "mmx_adddi3",
    "paddq\t{%2, %0|%0, %2}",
    (insn_gen_fn) gen_mmx_adddi3,
    &operand_data[1466],
    3,
    0,
    1,
    1
  },
  {
    "ssaddv8qi3",
    "paddsb\t{%2, %0|%0, %2}",
    (insn_gen_fn) gen_ssaddv8qi3,
    &operand_data[1457],
    3,
    0,
    1,
    1
  },
  {
    "ssaddv4hi3",
    "paddsw\t{%2, %0|%0, %2}",
    (insn_gen_fn) gen_ssaddv4hi3,
    &operand_data[1460],
    3,
    0,
    1,
    1
  },
  {
    "usaddv8qi3",
    "paddusb\t{%2, %0|%0, %2}",
    (insn_gen_fn) gen_usaddv8qi3,
    &operand_data[1457],
    3,
    0,
    1,
    1
  },
  {
    "usaddv4hi3",
    "paddusw\t{%2, %0|%0, %2}",
    (insn_gen_fn) gen_usaddv4hi3,
    &operand_data[1460],
    3,
    0,
    1,
    1
  },
  {
    "subv8qi3",
    "psubb\t{%2, %0|%0, %2}",
    (insn_gen_fn) gen_subv8qi3,
    &operand_data[1469],
    3,
    0,
    1,
    1
  },
  {
    "subv4hi3",
    "psubw\t{%2, %0|%0, %2}",
    (insn_gen_fn) gen_subv4hi3,
    &operand_data[1472],
    3,
    0,
    1,
    1
  },
  {
    "subv2si3",
    "psubd\t{%2, %0|%0, %2}",
    (insn_gen_fn) gen_subv2si3,
    &operand_data[1475],
    3,
    0,
    1,
    1
  },
  {
    "mmx_subdi3",
    "psubq\t{%2, %0|%0, %2}",
    (insn_gen_fn) gen_mmx_subdi3,
    &operand_data[1478],
    3,
    0,
    1,
    1
  },
  {
    "sssubv8qi3",
    "psubsb\t{%2, %0|%0, %2}",
    (insn_gen_fn) gen_sssubv8qi3,
    &operand_data[1469],
    3,
    0,
    1,
    1
  },
  {
    "sssubv4hi3",
    "psubsw\t{%2, %0|%0, %2}",
    (insn_gen_fn) gen_sssubv4hi3,
    &operand_data[1472],
    3,
    0,
    1,
    1
  },
  {
    "ussubv8qi3",
    "psubusb\t{%2, %0|%0, %2}",
    (insn_gen_fn) gen_ussubv8qi3,
    &operand_data[1469],
    3,
    0,
    1,
    1
  },
  {
    "ussubv4hi3",
    "psubusw\t{%2, %0|%0, %2}",
    (insn_gen_fn) gen_ussubv4hi3,
    &operand_data[1472],
    3,
    0,
    1,
    1
  },
  {
    "mulv4hi3",
    "pmullw\t{%2, %0|%0, %2}",
    (insn_gen_fn) gen_mulv4hi3,
    &operand_data[1472],
    3,
    0,
    1,
    1
  },
  {
    "smulv4hi3_highpart",
    "pmulhw\t{%2, %0|%0, %2}",
    (insn_gen_fn) gen_smulv4hi3_highpart,
    &operand_data[1472],
    3,
    0,
    1,
    1
  },
  {
    "umulv4hi3_highpart",
    "pmulhuw\t{%2, %0|%0, %2}",
    (insn_gen_fn) gen_umulv4hi3_highpart,
    &operand_data[1472],
    3,
    0,
    1,
    1
  },
  {
    "mmx_pmaddwd",
    "pmaddwd\t{%2, %0|%0, %2}",
    (insn_gen_fn) gen_mmx_pmaddwd,
    &operand_data[1481],
    3,
    2,
    1,
    1
  },
  {
    "mmx_iordi3",
    "por\t{%2, %0|%0, %2}",
    (insn_gen_fn) gen_mmx_iordi3,
    &operand_data[1466],
    3,
    0,
    1,
    1
  },
  {
    "mmx_xordi3",
    "pxor\t{%2, %0|%0, %2}",
    (insn_gen_fn) gen_mmx_xordi3,
    &operand_data[1466],
    3,
    0,
    1,
    1
  },
  {
    "mmx_clrdi",
    "pxor\t{%0, %0|%0, %0}",
    (insn_gen_fn) gen_mmx_clrdi,
    &operand_data[1466],
    1,
    0,
    1,
    1
  },
  {
    "mmx_anddi3",
    "pand\t{%2, %0|%0, %2}",
    (insn_gen_fn) gen_mmx_anddi3,
    &operand_data[1466],
    3,
    0,
    1,
    1
  },
  {
    "mmx_nanddi3",
    "pandn\t{%2, %0|%0, %2}",
    (insn_gen_fn) gen_mmx_nanddi3,
    &operand_data[1478],
    3,
    0,
    1,
    1
  },
  {
    "mmx_uavgv8qi3",
    "pavgb\t{%2, %0|%0, %2}",
    (insn_gen_fn) gen_mmx_uavgv8qi3,
    &operand_data[1469],
    3,
    0,
    1,
    1
  },
  {
    "mmx_uavgv4hi3",
    "pavgw\t{%2, %0|%0, %2}",
    (insn_gen_fn) gen_mmx_uavgv4hi3,
    &operand_data[1472],
    3,
    0,
    1,
    1
  },
  {
    "mmx_psadbw",
    "psadbw\t{%2, %0|%0, %2}",
    (insn_gen_fn) gen_mmx_psadbw,
    &operand_data[1484],
    3,
    0,
    1,
    1
  },
  {
    "mmx_pinsrw",
    "pinsrw\t{%3, %2, %0|%0, %2, %3}",
    (insn_gen_fn) gen_mmx_pinsrw,
    &operand_data[1487],
    4,
    0,
    1,
    1
  },
  {
    "mmx_pextrw",
    "pextrw\t{%2, %1, %0|%0, %1, %2}",
    (insn_gen_fn) gen_mmx_pextrw,
    &operand_data[1491],
    3,
    0,
    1,
    1
  },
  {
    "mmx_pshufw",
    "pshufw\t{%2, %1, %0|%0, %1, %2}",
    (insn_gen_fn) gen_mmx_pshufw,
    &operand_data[1494],
    3,
    0,
    1,
    1
  },
  {
    "eqv8qi3",
    "pcmpeqb\t{%2, %0|%0, %2}",
    (insn_gen_fn) gen_eqv8qi3,
    &operand_data[1469],
    3,
    0,
    1,
    1
  },
  {
    "eqv4hi3",
    "pcmpeqw\t{%2, %0|%0, %2}",
    (insn_gen_fn) gen_eqv4hi3,
    &operand_data[1472],
    3,
    0,
    1,
    1
  },
  {
    "eqv2si3",
    "pcmpeqd\t{%2, %0|%0, %2}",
    (insn_gen_fn) gen_eqv2si3,
    &operand_data[1475],
    3,
    0,
    1,
    1
  },
  {
    "gtv8qi3",
    "pcmpgtb\t{%2, %0|%0, %2}",
    (insn_gen_fn) gen_gtv8qi3,
    &operand_data[1469],
    3,
    0,
    1,
    1
  },
  {
    "gtv4hi3",
    "pcmpgtw\t{%2, %0|%0, %2}",
    (insn_gen_fn) gen_gtv4hi3,
    &operand_data[1472],
    3,
    0,
    1,
    1
  },
  {
    "gtv2si3",
    "pcmpgtd\t{%2, %0|%0, %2}",
    (insn_gen_fn) gen_gtv2si3,
    &operand_data[1475],
    3,
    0,
    1,
    1
  },
  {
    "umaxv8qi3",
    "pmaxub\t{%2, %0|%0, %2}",
    (insn_gen_fn) gen_umaxv8qi3,
    &operand_data[1469],
    3,
    0,
    1,
    1
  },
  {
    "smaxv4hi3",
    "pmaxsw\t{%2, %0|%0, %2}",
    (insn_gen_fn) gen_smaxv4hi3,
    &operand_data[1472],
    3,
    0,
    1,
    1
  },
  {
    "uminv8qi3",
    "pminub\t{%2, %0|%0, %2}",
    (insn_gen_fn) gen_uminv8qi3,
    &operand_data[1469],
    3,
    0,
    1,
    1
  },
  {
    "sminv4hi3",
    "pminsw\t{%2, %0|%0, %2}",
    (insn_gen_fn) gen_sminv4hi3,
    &operand_data[1472],
    3,
    0,
    1,
    1
  },
  {
    "ashrv4hi3",
    "psraw\t{%2, %0|%0, %2}",
    (insn_gen_fn) gen_ashrv4hi3,
    &operand_data[1497],
    3,
    0,
    1,
    1
  },
  {
    "ashrv2si3",
    "psrad\t{%2, %0|%0, %2}",
    (insn_gen_fn) gen_ashrv2si3,
    &operand_data[1500],
    3,
    0,
    1,
    1
  },
  {
    "lshrv4hi3",
    "psrlw\t{%2, %0|%0, %2}",
    (insn_gen_fn) gen_lshrv4hi3,
    &operand_data[1497],
    3,
    0,
    1,
    1
  },
  {
    "lshrv2si3",
    "psrld\t{%2, %0|%0, %2}",
    (insn_gen_fn) gen_lshrv2si3,
    &operand_data[1500],
    3,
    0,
    1,
    1
  },
  {
    "mmx_lshrdi3",
    "psrlq\t{%2, %0|%0, %2}",
    (insn_gen_fn) gen_mmx_lshrdi3,
    &operand_data[1503],
    3,
    0,
    1,
    1
  },
  {
    "ashlv4hi3",
    "psllw\t{%2, %0|%0, %2}",
    (insn_gen_fn) gen_ashlv4hi3,
    &operand_data[1497],
    3,
    0,
    1,
    1
  },
  {
    "ashlv2si3",
    "pslld\t{%2, %0|%0, %2}",
    (insn_gen_fn) gen_ashlv2si3,
    &operand_data[1500],
    3,
    0,
    1,
    1
  },
  {
    "mmx_ashldi3",
    "psllq\t{%2, %0|%0, %2}",
    (insn_gen_fn) gen_mmx_ashldi3,
    &operand_data[1503],
    3,
    0,
    1,
    1
  },
  {
    "mmx_packsswb",
    "packsswb\t{%2, %0|%0, %2}",
    (insn_gen_fn) gen_mmx_packsswb,
    &operand_data[1506],
    3,
    0,
    1,
    1
  },
  {
    "mmx_packssdw",
    "packssdw\t{%2, %0|%0, %2}",
    (insn_gen_fn) gen_mmx_packssdw,
    &operand_data[1509],
    3,
    0,
    1,
    1
  },
  {
    "mmx_packuswb",
    "packuswb\t{%2, %0|%0, %2}",
    (insn_gen_fn) gen_mmx_packuswb,
    &operand_data[1506],
    3,
    0,
    1,
    1
  },
  {
    "mmx_punpckhbw",
    "punpckhbw\t{%2, %0|%0, %2}",
    (insn_gen_fn) gen_mmx_punpckhbw,
    &operand_data[1512],
    3,
    0,
    1,
    1
  },
  {
    "mmx_punpckhwd",
    "punpckhwd\t{%2, %0|%0, %2}",
    (insn_gen_fn) gen_mmx_punpckhwd,
    &operand_data[1515],
    3,
    0,
    1,
    1
  },
  {
    "mmx_punpckhdq",
    "punpckhdq\t{%2, %0|%0, %2}",
    (insn_gen_fn) gen_mmx_punpckhdq,
    &operand_data[1518],
    3,
    0,
    1,
    1
  },
  {
    "mmx_punpcklbw",
    "punpcklbw\t{%2, %0|%0, %2}",
    (insn_gen_fn) gen_mmx_punpcklbw,
    &operand_data[1512],
    3,
    0,
    1,
    1
  },
  {
    "mmx_punpcklwd",
    "punpcklwd\t{%2, %0|%0, %2}",
    (insn_gen_fn) gen_mmx_punpcklwd,
    &operand_data[1515],
    3,
    0,
    1,
    1
  },
  {
    "mmx_punpckldq",
    "punpckldq\t{%2, %0|%0, %2}",
    (insn_gen_fn) gen_mmx_punpckldq,
    &operand_data[1518],
    3,
    0,
    1,
    1
  },
  {
    "emms",
    "emms",
    (insn_gen_fn) gen_emms,
    &operand_data[0],
    0,
    0,
    0,
    1
  },
  {
    "ldmxcsr",
    "ldmxcsr\t%0",
    (insn_gen_fn) gen_ldmxcsr,
    &operand_data[1521],
    1,
    0,
    1,
    1
  },
  {
    "stmxcsr",
    "stmxcsr\t%0",
    (insn_gen_fn) gen_stmxcsr,
    &operand_data[290],
    1,
    0,
    1,
    1
  },
  {
    "*sfence_insn",
    "sfence",
    0,
    &operand_data[1522],
    1,
    1,
    0,
    1
  },
  {
    "*sse_prologue_save_insn",
    (const PTR) output_854,
    0,
    &operand_data[1523],
    5,
    0,
    1,
    3
  },
  {
    "addv2sf3",
    "pfadd\t{%2, %0|%0, %2}",
    (insn_gen_fn) gen_addv2sf3,
    &operand_data[1528],
    3,
    0,
    1,
    1
  },
  {
    "subv2sf3",
    "pfsub\t{%2, %0|%0, %2}",
    (insn_gen_fn) gen_subv2sf3,
    &operand_data[1528],
    3,
    0,
    1,
    1
  },
  {
    "subrv2sf3",
    "pfsubr\t{%2, %0|%0, %2}",
    (insn_gen_fn) gen_subrv2sf3,
    &operand_data[1528],
    3,
    0,
    1,
    1
  },
  {
    "gtv2sf3",
    "pfcmpgt\t{%2, %0|%0, %2}",
    (insn_gen_fn) gen_gtv2sf3,
    &operand_data[1531],
    3,
    0,
    1,
    1
  },
  {
    "gev2sf3",
    "pfcmpge\t{%2, %0|%0, %2}",
    (insn_gen_fn) gen_gev2sf3,
    &operand_data[1531],
    3,
    0,
    1,
    1
  },
  {
    "eqv2sf3",
    "pfcmpeq\t{%2, %0|%0, %2}",
    (insn_gen_fn) gen_eqv2sf3,
    &operand_data[1531],
    3,
    0,
    1,
    1
  },
  {
    "pfmaxv2sf3",
    "pfmax\t{%2, %0|%0, %2}",
    (insn_gen_fn) gen_pfmaxv2sf3,
    &operand_data[1528],
    3,
    0,
    1,
    1
  },
  {
    "pfminv2sf3",
    "pfmin\t{%2, %0|%0, %2}",
    (insn_gen_fn) gen_pfminv2sf3,
    &operand_data[1528],
    3,
    0,
    1,
    1
  },
  {
    "mulv2sf3",
    "pfmul\t{%2, %0|%0, %2}",
    (insn_gen_fn) gen_mulv2sf3,
    &operand_data[1528],
    3,
    0,
    1,
    1
  },
  {
    "femms",
    "femms",
    (insn_gen_fn) gen_femms,
    &operand_data[0],
    0,
    0,
    0,
    1
  },
  {
    "pf2id",
    "pf2id\t{%1, %0|%0, %1}",
    (insn_gen_fn) gen_pf2id,
    &operand_data[1534],
    2,
    0,
    1,
    1
  },
  {
    "pf2iw",
    "pf2iw\t{%1, %0|%0, %1}",
    (insn_gen_fn) gen_pf2iw,
    &operand_data[1534],
    2,
    0,
    1,
    1
  },
  {
    "pfacc",
    "pfacc\t{%2, %0|%0, %2}",
    (insn_gen_fn) gen_pfacc,
    &operand_data[1536],
    3,
    2,
    1,
    1
  },
  {
    "pfnacc",
    "pfnacc\t{%2, %0|%0, %2}",
    (insn_gen_fn) gen_pfnacc,
    &operand_data[1536],
    3,
    2,
    1,
    1
  },
  {
    "pfpnacc",
    "pfpnacc\t{%2, %0|%0, %2}",
    (insn_gen_fn) gen_pfpnacc,
    &operand_data[1536],
    3,
    2,
    1,
    1
  },
  {
    "pi2fw",
    "pi2fw\t{%1, %0|%0, %1}",
    (insn_gen_fn) gen_pi2fw,
    &operand_data[1539],
    2,
    1,
    1,
    1
  },
  {
    "floatv2si2",
    "pi2fd\t{%1, %0|%0, %1}",
    (insn_gen_fn) gen_floatv2si2,
    &operand_data[1539],
    2,
    0,
    1,
    1
  },
  {
    "pavgusb",
    "pavgusb\t{%2, %0|%0, %2}",
    (insn_gen_fn) gen_pavgusb,
    &operand_data[1469],
    3,
    0,
    1,
    1
  },
  {
    "pfrcpv2sf2",
    "pfrcp\t{%1, %0|%0, %1}",
    (insn_gen_fn) gen_pfrcpv2sf2,
    &operand_data[1541],
    2,
    0,
    1,
    1
  },
  {
    "pfrcpit1v2sf3",
    "pfrcpit1\t{%2, %0|%0, %2}",
    (insn_gen_fn) gen_pfrcpit1v2sf3,
    &operand_data[1528],
    3,
    0,
    1,
    1
  },
  {
    "pfrcpit2v2sf3",
    "pfrcpit2\t{%2, %0|%0, %2}",
    (insn_gen_fn) gen_pfrcpit2v2sf3,
    &operand_data[1528],
    3,
    0,
    1,
    1
  },
  {
    "pfrsqrtv2sf2",
    "pfrsqrt\t{%1, %0|%0, %1}",
    (insn_gen_fn) gen_pfrsqrtv2sf2,
    &operand_data[1541],
    2,
    0,
    1,
    1
  },
  {
    "pfrsqit1v2sf3",
    "pfrsqit1\t{%2, %0|%0, %2}",
    (insn_gen_fn) gen_pfrsqit1v2sf3,
    &operand_data[1528],
    3,
    0,
    1,
    1
  },
  {
    "pmulhrwv4hi3",
    "pmulhrw\t{%2, %0|%0, %2}",
    (insn_gen_fn) gen_pmulhrwv4hi3,
    &operand_data[1472],
    3,
    0,
    1,
    1
  },
  {
    "pswapdv2si2",
    "pswapd\t{%1, %0|%0, %1}",
    (insn_gen_fn) gen_pswapdv2si2,
    &operand_data[1543],
    2,
    0,
    1,
    1
  },
  {
    "pswapdv2sf2",
    "pswapd\t{%1, %0|%0, %1}",
    (insn_gen_fn) gen_pswapdv2sf2,
    &operand_data[1541],
    2,
    0,
    1,
    1
  },
  {
    "*prefetch_sse",
    (const PTR) output_881,
    0,
    &operand_data[1545],
    2,
    0,
    1,
    3
  },
  {
    "*prefetch_sse_rex",
    (const PTR) output_882,
    0,
    &operand_data[1547],
    2,
    0,
    1,
    3
  },
  {
    "*prefetch_3dnow",
    (const PTR) output_883,
    0,
    &operand_data[1549],
    2,
    0,
    1,
    3
  },
  {
    "*prefetch_3dnow_rex",
    (const PTR) output_884,
    0,
    &operand_data[1551],
    2,
    0,
    1,
    3
  },
  {
    "addv2df3",
    "addpd\t{%2, %0|%0, %2}",
    (insn_gen_fn) gen_addv2df3,
    &operand_data[1553],
    3,
    0,
    1,
    1
  },
  {
    "vmaddv2df3",
    "addsd\t{%2, %0|%0, %2}",
    (insn_gen_fn) gen_vmaddv2df3,
    &operand_data[1553],
    3,
    1,
    1,
    1
  },
  {
    "subv2df3",
    "subpd\t{%2, %0|%0, %2}",
    (insn_gen_fn) gen_subv2df3,
    &operand_data[1553],
    3,
    0,
    1,
    1
  },
  {
    "vmsubv2df3",
    "subsd\t{%2, %0|%0, %2}",
    (insn_gen_fn) gen_vmsubv2df3,
    &operand_data[1553],
    3,
    1,
    1,
    1
  },
  {
    "mulv2df3",
    "mulpd\t{%2, %0|%0, %2}",
    (insn_gen_fn) gen_mulv2df3,
    &operand_data[1553],
    3,
    0,
    1,
    1
  },
  {
    "vmmulv2df3",
    "mulsd\t{%2, %0|%0, %2}",
    (insn_gen_fn) gen_vmmulv2df3,
    &operand_data[1553],
    3,
    1,
    1,
    1
  },
  {
    "divv2df3",
    "divpd\t{%2, %0|%0, %2}",
    (insn_gen_fn) gen_divv2df3,
    &operand_data[1553],
    3,
    0,
    1,
    1
  },
  {
    "vmdivv2df3",
    "divsd\t{%2, %0|%0, %2}",
    (insn_gen_fn) gen_vmdivv2df3,
    &operand_data[1553],
    3,
    1,
    1,
    1
  },
  {
    "smaxv2df3",
    "maxpd\t{%2, %0|%0, %2}",
    (insn_gen_fn) gen_smaxv2df3,
    &operand_data[1553],
    3,
    0,
    1,
    1
  },
  {
    "vmsmaxv2df3",
    "maxsd\t{%2, %0|%0, %2}",
    (insn_gen_fn) gen_vmsmaxv2df3,
    &operand_data[1553],
    3,
    1,
    1,
    1
  },
  {
    "sminv2df3",
    "minpd\t{%2, %0|%0, %2}",
    (insn_gen_fn) gen_sminv2df3,
    &operand_data[1553],
    3,
    0,
    1,
    1
  },
  {
    "vmsminv2df3",
    "minsd\t{%2, %0|%0, %2}",
    (insn_gen_fn) gen_vmsminv2df3,
    &operand_data[1553],
    3,
    1,
    1,
    1
  },
  {
    "sqrtv2df2",
    "sqrtpd\t{%1, %0|%0, %1}",
    (insn_gen_fn) gen_sqrtv2df2,
    &operand_data[1556],
    2,
    0,
    1,
    1
  },
  {
    "vmsqrtv2df2",
    "sqrtsd\t{%1, %0|%0, %1}",
    (insn_gen_fn) gen_vmsqrtv2df2,
    &operand_data[1556],
    3,
    0,
    1,
    1
  },
  {
    "maskcmpv2df3",
    "cmp%D3pd\t{%2, %0|%0, %2}",
    (insn_gen_fn) gen_maskcmpv2df3,
    &operand_data[1559],
    4,
    0,
    1,
    1
  },
  {
    "maskncmpv2df3",
    (const PTR) output_900,
    (insn_gen_fn) gen_maskncmpv2df3,
    &operand_data[1559],
    4,
    0,
    1,
    3
  },
  {
    "vmmaskcmpv2df3",
    "cmp%D3sd\t{%2, %0|%0, %2}",
    (insn_gen_fn) gen_vmmaskcmpv2df3,
    &operand_data[1559],
    4,
    1,
    1,
    1
  },
  {
    "vmmaskncmpv2df3",
    (const PTR) output_902,
    (insn_gen_fn) gen_vmmaskncmpv2df3,
    &operand_data[1559],
    4,
    1,
    1,
    3
  },
  {
    "sse2_comi",
    "comisd\t{%1, %0|%0, %1}",
    (insn_gen_fn) gen_sse2_comi,
    &operand_data[1563],
    2,
    0,
    1,
    1
  },
  {
    "sse2_ucomi",
    "ucomisd\t{%1, %0|%0, %1}",
    (insn_gen_fn) gen_sse2_ucomi,
    &operand_data[1563],
    2,
    0,
    1,
    1
  },
  {
    "sse2_movmskpd",
    "movmskpd\t{%1, %0|%0, %1}",
    (insn_gen_fn) gen_sse2_movmskpd,
    &operand_data[1565],
    2,
    0,
    1,
    1
  },
  {
    "sse2_pmovmskb",
    "pmovmskb\t{%1, %0|%0, %1}",
    (insn_gen_fn) gen_sse2_pmovmskb,
    &operand_data[1567],
    2,
    0,
    1,
    1
  },
  {
    "sse2_maskmovdqu",
    "maskmovdqu\t{%2, %1|%1, %2}",
    (insn_gen_fn) gen_sse2_maskmovdqu,
    &operand_data[1569],
    3,
    0,
    1,
    1
  },
  {
    "sse2_maskmovdqu_rex64",
    "maskmovdqu\t{%2, %1|%1, %2}",
    (insn_gen_fn) gen_sse2_maskmovdqu_rex64,
    &operand_data[1572],
    3,
    0,
    1,
    1
  },
  {
    "sse2_movntv2df",
    "movntpd\t{%1, %0|%0, %1}",
    (insn_gen_fn) gen_sse2_movntv2df,
    &operand_data[1575],
    2,
    0,
    1,
    1
  },
  {
    "sse2_movntv2di",
    "movntdq\t{%1, %0|%0, %1}",
    (insn_gen_fn) gen_sse2_movntv2di,
    &operand_data[1577],
    2,
    0,
    1,
    1
  },
  {
    "sse2_movntsi",
    "movnti\t{%1, %0|%0, %1}",
    (insn_gen_fn) gen_sse2_movntsi,
    &operand_data[1579],
    2,
    0,
    1,
    1
  },
  {
    "cvtdq2ps",
    "cvtdq2ps\t{%1, %0|%0, %1}",
    (insn_gen_fn) gen_cvtdq2ps,
    &operand_data[1581],
    2,
    0,
    1,
    1
  },
  {
    "cvtps2dq",
    "cvtps2dq\t{%1, %0|%0, %1}",
    (insn_gen_fn) gen_cvtps2dq,
    &operand_data[1583],
    2,
    0,
    1,
    1
  },
  {
    "cvttps2dq",
    "cvttps2dq\t{%1, %0|%0, %1}",
    (insn_gen_fn) gen_cvttps2dq,
    &operand_data[1583],
    2,
    0,
    1,
    1
  },
  {
    "cvtdq2pd",
    "cvtdq2pd\t{%1, %0|%0, %1}",
    (insn_gen_fn) gen_cvtdq2pd,
    &operand_data[1585],
    2,
    0,
    1,
    1
  },
  {
    "cvtpd2dq",
    "cvtpd2dq\t{%1, %0|%0, %1}",
    (insn_gen_fn) gen_cvtpd2dq,
    &operand_data[1587],
    2,
    0,
    1,
    1
  },
  {
    "cvttpd2dq",
    "cvttpd2dq\t{%1, %0|%0, %1}",
    (insn_gen_fn) gen_cvttpd2dq,
    &operand_data[1587],
    2,
    0,
    1,
    1
  },
  {
    "cvtpd2pi",
    "cvtpd2pi\t{%1, %0|%0, %1}",
    (insn_gen_fn) gen_cvtpd2pi,
    &operand_data[1589],
    2,
    0,
    1,
    1
  },
  {
    "cvttpd2pi",
    "cvttpd2pi\t{%1, %0|%0, %1}",
    (insn_gen_fn) gen_cvttpd2pi,
    &operand_data[1589],
    2,
    0,
    1,
    1
  },
  {
    "cvtpi2pd",
    "cvtpi2pd\t{%1, %0|%0, %1}",
    (insn_gen_fn) gen_cvtpi2pd,
    &operand_data[1591],
    2,
    0,
    1,
    1
  },
  {
    "cvtsd2si",
    "cvtsd2si\t{%1, %0|%0, %1}",
    (insn_gen_fn) gen_cvtsd2si,
    &operand_data[1593],
    2,
    0,
    1,
    1
  },
  {
    "cvtsd2siq",
    "cvtsd2siq\t{%1, %0|%0, %1}",
    (insn_gen_fn) gen_cvtsd2siq,
    &operand_data[1595],
    2,
    0,
    1,
    1
  },
  {
    "cvttsd2si",
    "cvttsd2si\t{%1, %0|%0, %1}",
    (insn_gen_fn) gen_cvttsd2si,
    &operand_data[1593],
    2,
    0,
    1,
    1
  },
  {
    "cvttsd2siq",
    "cvttsd2siq\t{%1, %0|%0, %1}",
    (insn_gen_fn) gen_cvttsd2siq,
    &operand_data[1597],
    2,
    0,
    2,
    1
  },
  {
    "cvtsi2sd",
    "cvtsi2sd\t{%2, %0|%0, %2}",
    (insn_gen_fn) gen_cvtsi2sd,
    &operand_data[1599],
    3,
    0,
    1,
    1
  },
  {
    "cvtsi2sdq",
    "cvtsi2sdq\t{%2, %0|%0, %2}",
    (insn_gen_fn) gen_cvtsi2sdq,
    &operand_data[1602],
    3,
    0,
    2,
    1
  },
  {
    "cvtsd2ss",
    "cvtsd2ss\t{%2, %0|%0, %2}",
    (insn_gen_fn) gen_cvtsd2ss,
    &operand_data[1605],
    3,
    0,
    1,
    1
  },
  {
    "cvtss2sd",
    "cvtss2sd\t{%2, %0|%0, %2}",
    (insn_gen_fn) gen_cvtss2sd,
    &operand_data[1608],
    3,
    0,
    1,
    1
  },
  {
    "cvtpd2ps",
    "cvtpd2ps\t{%1, %0|%0, %1}",
    (insn_gen_fn) gen_cvtpd2ps,
    &operand_data[1611],
    2,
    0,
    1,
    1
  },
  {
    "cvtps2pd",
    "cvtps2pd\t{%1, %0|%0, %1}",
    (insn_gen_fn) gen_cvtps2pd,
    &operand_data[1613],
    2,
    0,
    1,
    1
  },
  {
    "addv16qi3",
    "paddb\t{%2, %0|%0, %2}",
    (insn_gen_fn) gen_addv16qi3,
    &operand_data[1615],
    3,
    0,
    1,
    1
  },
  {
    "addv8hi3",
    "paddw\t{%2, %0|%0, %2}",
    (insn_gen_fn) gen_addv8hi3,
    &operand_data[1618],
    3,
    0,
    1,
    1
  },
  {
    "addv4si3",
    "paddd\t{%2, %0|%0, %2}",
    (insn_gen_fn) gen_addv4si3,
    &operand_data[1621],
    3,
    0,
    1,
    1
  },
  {
    "addv2di3",
    "paddq\t{%2, %0|%0, %2}",
    (insn_gen_fn) gen_addv2di3,
    &operand_data[1624],
    3,
    0,
    1,
    1
  },
  {
    "ssaddv16qi3",
    "paddsb\t{%2, %0|%0, %2}",
    (insn_gen_fn) gen_ssaddv16qi3,
    &operand_data[1615],
    3,
    0,
    1,
    1
  },
  {
    "ssaddv8hi3",
    "paddsw\t{%2, %0|%0, %2}",
    (insn_gen_fn) gen_ssaddv8hi3,
    &operand_data[1618],
    3,
    0,
    1,
    1
  },
  {
    "usaddv16qi3",
    "paddusb\t{%2, %0|%0, %2}",
    (insn_gen_fn) gen_usaddv16qi3,
    &operand_data[1615],
    3,
    0,
    1,
    1
  },
  {
    "usaddv8hi3",
    "paddusw\t{%2, %0|%0, %2}",
    (insn_gen_fn) gen_usaddv8hi3,
    &operand_data[1618],
    3,
    0,
    1,
    1
  },
  {
    "subv16qi3",
    "psubb\t{%2, %0|%0, %2}",
    (insn_gen_fn) gen_subv16qi3,
    &operand_data[1627],
    3,
    0,
    1,
    1
  },
  {
    "subv8hi3",
    "psubw\t{%2, %0|%0, %2}",
    (insn_gen_fn) gen_subv8hi3,
    &operand_data[1630],
    3,
    0,
    1,
    1
  },
  {
    "subv4si3",
    "psubd\t{%2, %0|%0, %2}",
    (insn_gen_fn) gen_subv4si3,
    &operand_data[1633],
    3,
    0,
    1,
    1
  },
  {
    "subv2di3",
    "psubq\t{%2, %0|%0, %2}",
    (insn_gen_fn) gen_subv2di3,
    &operand_data[1636],
    3,
    0,
    1,
    1
  },
  {
    "sssubv16qi3",
    "psubsb\t{%2, %0|%0, %2}",
    (insn_gen_fn) gen_sssubv16qi3,
    &operand_data[1627],
    3,
    0,
    1,
    1
  },
  {
    "sssubv8hi3",
    "psubsw\t{%2, %0|%0, %2}",
    (insn_gen_fn) gen_sssubv8hi3,
    &operand_data[1630],
    3,
    0,
    1,
    1
  },
  {
    "ussubv16qi3",
    "psubusb\t{%2, %0|%0, %2}",
    (insn_gen_fn) gen_ussubv16qi3,
    &operand_data[1627],
    3,
    0,
    1,
    1
  },
  {
    "ussubv8hi3",
    "psubusw\t{%2, %0|%0, %2}",
    (insn_gen_fn) gen_ussubv8hi3,
    &operand_data[1630],
    3,
    0,
    1,
    1
  },
  {
    "mulv8hi3",
    "pmullw\t{%2, %0|%0, %2}",
    (insn_gen_fn) gen_mulv8hi3,
    &operand_data[1630],
    3,
    0,
    1,
    1
  },
  {
    "smulv8hi3_highpart",
    "pmulhw\t{%2, %0|%0, %2}",
    (insn_gen_fn) gen_smulv8hi3_highpart,
    &operand_data[1630],
    3,
    0,
    1,
    1
  },
  {
    "umulv8hi3_highpart",
    "pmulhuw\t{%2, %0|%0, %2}",
    (insn_gen_fn) gen_umulv8hi3_highpart,
    &operand_data[1630],
    3,
    0,
    1,
    1
  },
  {
    "sse2_umulsidi3",
    "pmuludq\t{%2, %0|%0, %2}",
    (insn_gen_fn) gen_sse2_umulsidi3,
    &operand_data[1639],
    3,
    0,
    1,
    1
  },
  {
    "sse2_umulv2siv2di3",
    "pmuludq\t{%2, %0|%0, %2}",
    (insn_gen_fn) gen_sse2_umulv2siv2di3,
    &operand_data[1642],
    3,
    0,
    1,
    1
  },
  {
    "sse2_pmaddwd",
    "pmaddwd\t{%2, %0|%0, %2}",
    (insn_gen_fn) gen_sse2_pmaddwd,
    &operand_data[1645],
    3,
    2,
    1,
    1
  },
  {
    "sse2_clrti",
    "pxor\t{%0, %0|%0, %0}",
    (insn_gen_fn) gen_sse2_clrti,
    &operand_data[1422],
    1,
    0,
    1,
    1
  },
  {
    "sse2_uavgv16qi3",
    "pavgb\t{%2, %0|%0, %2}",
    (insn_gen_fn) gen_sse2_uavgv16qi3,
    &operand_data[1627],
    3,
    0,
    1,
    1
  },
  {
    "sse2_uavgv8hi3",
    "pavgw\t{%2, %0|%0, %2}",
    (insn_gen_fn) gen_sse2_uavgv8hi3,
    &operand_data[1630],
    3,
    0,
    1,
    1
  },
  {
    "sse2_psadbw",
    "psadbw\t{%2, %0|%0, %2}",
    (insn_gen_fn) gen_sse2_psadbw,
    &operand_data[1648],
    3,
    0,
    1,
    1
  },
  {
    "sse2_pinsrw",
    "pinsrw\t{%3, %2, %0|%0, %2, %3}",
    (insn_gen_fn) gen_sse2_pinsrw,
    &operand_data[1651],
    4,
    0,
    1,
    1
  },
  {
    "sse2_pextrw",
    "pextrw\t{%2, %1, %0|%0, %1, %2}",
    (insn_gen_fn) gen_sse2_pextrw,
    &operand_data[1655],
    3,
    0,
    1,
    1
  },
  {
    "sse2_pshufd",
    "pshufd\t{%2, %1, %0|%0, %1, %2}",
    (insn_gen_fn) gen_sse2_pshufd,
    &operand_data[1658],
    3,
    0,
    1,
    1
  },
  {
    "sse2_pshuflw",
    "pshuflw\t{%2, %1, %0|%0, %1, %2}",
    (insn_gen_fn) gen_sse2_pshuflw,
    &operand_data[1661],
    3,
    0,
    1,
    1
  },
  {
    "sse2_pshufhw",
    "pshufhw\t{%2, %1, %0|%0, %1, %2}",
    (insn_gen_fn) gen_sse2_pshufhw,
    &operand_data[1661],
    3,
    0,
    1,
    1
  },
  {
    "eqv16qi3",
    "pcmpeqb\t{%2, %0|%0, %2}",
    (insn_gen_fn) gen_eqv16qi3,
    &operand_data[1627],
    3,
    0,
    1,
    1
  },
  {
    "eqv8hi3",
    "pcmpeqw\t{%2, %0|%0, %2}",
    (insn_gen_fn) gen_eqv8hi3,
    &operand_data[1630],
    3,
    0,
    1,
    1
  },
  {
    "eqv4si3",
    "pcmpeqd\t{%2, %0|%0, %2}",
    (insn_gen_fn) gen_eqv4si3,
    &operand_data[1633],
    3,
    0,
    1,
    1
  },
  {
    "gtv16qi3",
    "pcmpgtb\t{%2, %0|%0, %2}",
    (insn_gen_fn) gen_gtv16qi3,
    &operand_data[1627],
    3,
    0,
    1,
    1
  },
  {
    "gtv8hi3",
    "pcmpgtw\t{%2, %0|%0, %2}",
    (insn_gen_fn) gen_gtv8hi3,
    &operand_data[1630],
    3,
    0,
    1,
    1
  },
  {
    "gtv4si3",
    "pcmpgtd\t{%2, %0|%0, %2}",
    (insn_gen_fn) gen_gtv4si3,
    &operand_data[1633],
    3,
    0,
    1,
    1
  },
  {
    "umaxv16qi3",
    "pmaxub\t{%2, %0|%0, %2}",
    (insn_gen_fn) gen_umaxv16qi3,
    &operand_data[1627],
    3,
    0,
    1,
    1
  },
  {
    "smaxv8hi3",
    "pmaxsw\t{%2, %0|%0, %2}",
    (insn_gen_fn) gen_smaxv8hi3,
    &operand_data[1630],
    3,
    0,
    1,
    1
  },
  {
    "uminv16qi3",
    "pminub\t{%2, %0|%0, %2}",
    (insn_gen_fn) gen_uminv16qi3,
    &operand_data[1627],
    3,
    0,
    1,
    1
  },
  {
    "sminv8hi3",
    "pminsw\t{%2, %0|%0, %2}",
    (insn_gen_fn) gen_sminv8hi3,
    &operand_data[1630],
    3,
    0,
    1,
    1
  },
  {
    "ashrv8hi3",
    "psraw\t{%2, %0|%0, %2}",
    (insn_gen_fn) gen_ashrv8hi3,
    &operand_data[1664],
    3,
    0,
    1,
    1
  },
  {
    "ashrv4si3",
    "psrad\t{%2, %0|%0, %2}",
    (insn_gen_fn) gen_ashrv4si3,
    &operand_data[1667],
    3,
    0,
    1,
    1
  },
  {
    "lshrv8hi3",
    "psrlw\t{%2, %0|%0, %2}",
    (insn_gen_fn) gen_lshrv8hi3,
    &operand_data[1664],
    3,
    0,
    1,
    1
  },
  {
    "lshrv4si3",
    "psrld\t{%2, %0|%0, %2}",
    (insn_gen_fn) gen_lshrv4si3,
    &operand_data[1667],
    3,
    0,
    1,
    1
  },
  {
    "lshrv2di3",
    "psrlq\t{%2, %0|%0, %2}",
    (insn_gen_fn) gen_lshrv2di3,
    &operand_data[1670],
    3,
    0,
    1,
    1
  },
  {
    "ashlv8hi3",
    "psllw\t{%2, %0|%0, %2}",
    (insn_gen_fn) gen_ashlv8hi3,
    &operand_data[1664],
    3,
    0,
    1,
    1
  },
  {
    "ashlv4si3",
    "pslld\t{%2, %0|%0, %2}",
    (insn_gen_fn) gen_ashlv4si3,
    &operand_data[1667],
    3,
    0,
    1,
    1
  },
  {
    "ashlv2di3",
    "psllq\t{%2, %0|%0, %2}",
    (insn_gen_fn) gen_ashlv2di3,
    &operand_data[1670],
    3,
    0,
    1,
    1
  },
  {
    "ashrv8hi3_ti",
    "psraw\t{%2, %0|%0, %2}",
    (insn_gen_fn) gen_ashrv8hi3_ti,
    &operand_data[1673],
    3,
    0,
    1,
    1
  },
  {
    "ashrv4si3_ti",
    "psrad\t{%2, %0|%0, %2}",
    (insn_gen_fn) gen_ashrv4si3_ti,
    &operand_data[1676],
    3,
    0,
    1,
    1
  },
  {
    "lshrv8hi3_ti",
    "psrlw\t{%2, %0|%0, %2}",
    (insn_gen_fn) gen_lshrv8hi3_ti,
    &operand_data[1673],
    3,
    0,
    1,
    1
  },
  {
    "lshrv4si3_ti",
    "psrld\t{%2, %0|%0, %2}",
    (insn_gen_fn) gen_lshrv4si3_ti,
    &operand_data[1676],
    3,
    0,
    1,
    1
  },
  {
    "lshrv2di3_ti",
    "psrlq\t{%2, %0|%0, %2}",
    (insn_gen_fn) gen_lshrv2di3_ti,
    &operand_data[1679],
    3,
    0,
    1,
    1
  },
  {
    "ashlv8hi3_ti",
    "psllw\t{%2, %0|%0, %2}",
    (insn_gen_fn) gen_ashlv8hi3_ti,
    &operand_data[1673],
    3,
    0,
    1,
    1
  },
  {
    "ashlv4si3_ti",
    "pslld\t{%2, %0|%0, %2}",
    (insn_gen_fn) gen_ashlv4si3_ti,
    &operand_data[1676],
    3,
    0,
    1,
    1
  },
  {
    "ashlv2di3_ti",
    "psllq\t{%2, %0|%0, %2}",
    (insn_gen_fn) gen_ashlv2di3_ti,
    &operand_data[1679],
    3,
    0,
    1,
    1
  },
  {
    "sse2_ashlti3",
    "pslldq\t{%2, %0|%0, %2}",
    (insn_gen_fn) gen_sse2_ashlti3,
    &operand_data[1682],
    3,
    0,
    1,
    1
  },
  {
    "sse2_lshrti3",
    "psrldq\t{%2, %0|%0, %2}",
    (insn_gen_fn) gen_sse2_lshrti3,
    &operand_data[1682],
    3,
    0,
    1,
    1
  },
  {
    "sse2_unpckhpd",
    "unpckhpd\t{%2, %0|%0, %2}",
    (insn_gen_fn) gen_sse2_unpckhpd,
    &operand_data[1685],
    3,
    0,
    1,
    1
  },
  {
    "sse2_unpcklpd",
    "unpcklpd\t{%2, %0|%0, %2}",
    (insn_gen_fn) gen_sse2_unpcklpd,
    &operand_data[1685],
    3,
    0,
    1,
    1
  },
  {
    "sse2_packsswb",
    "packsswb\t{%2, %0|%0, %2}",
    (insn_gen_fn) gen_sse2_packsswb,
    &operand_data[1688],
    3,
    0,
    1,
    1
  },
  {
    "sse2_packssdw",
    "packssdw\t{%2, %0|%0, %2}",
    (insn_gen_fn) gen_sse2_packssdw,
    &operand_data[1691],
    3,
    0,
    1,
    1
  },
  {
    "sse2_packuswb",
    "packuswb\t{%2, %0|%0, %2}",
    (insn_gen_fn) gen_sse2_packuswb,
    &operand_data[1688],
    3,
    0,
    1,
    1
  },
  {
    "sse2_punpckhbw",
    "punpckhbw\t{%2, %0|%0, %2}",
    (insn_gen_fn) gen_sse2_punpckhbw,
    &operand_data[1694],
    3,
    0,
    1,
    1
  },
  {
    "sse2_punpckhwd",
    "punpckhwd\t{%2, %0|%0, %2}",
    (insn_gen_fn) gen_sse2_punpckhwd,
    &operand_data[1697],
    3,
    0,
    1,
    1
  },
  {
    "sse2_punpckhdq",
    "punpckhdq\t{%2, %0|%0, %2}",
    (insn_gen_fn) gen_sse2_punpckhdq,
    &operand_data[1700],
    3,
    0,
    1,
    1
  },
  {
    "sse2_punpcklbw",
    "punpcklbw\t{%2, %0|%0, %2}",
    (insn_gen_fn) gen_sse2_punpcklbw,
    &operand_data[1694],
    3,
    0,
    1,
    1
  },
  {
    "sse2_punpcklwd",
    "punpcklwd\t{%2, %0|%0, %2}",
    (insn_gen_fn) gen_sse2_punpcklwd,
    &operand_data[1697],
    3,
    0,
    1,
    1
  },
  {
    "sse2_punpckldq",
    "punpckldq\t{%2, %0|%0, %2}",
    (insn_gen_fn) gen_sse2_punpckldq,
    &operand_data[1700],
    3,
    0,
    1,
    1
  },
  {
    "sse2_punpcklqdq",
    "punpcklqdq\t{%2, %0|%0, %2}",
    (insn_gen_fn) gen_sse2_punpcklqdq,
    &operand_data[1703],
    3,
    0,
    1,
    1
  },
  {
    "sse2_punpckhqdq",
    "punpckhqdq\t{%2, %0|%0, %2}",
    (insn_gen_fn) gen_sse2_punpckhqdq,
    &operand_data[1703],
    3,
    0,
    1,
    1
  },
  {
    "sse2_movapd",
    "movapd\t{%1, %0|%0, %1}",
    (insn_gen_fn) gen_sse2_movapd,
    &operand_data[1706],
    2,
    0,
    2,
    1
  },
  {
    "sse2_movupd",
    "movupd\t{%1, %0|%0, %1}",
    (insn_gen_fn) gen_sse2_movupd,
    &operand_data[1706],
    2,
    0,
    2,
    1
  },
  {
    "sse2_movdqa",
    "movdqa\t{%1, %0|%0, %1}",
    (insn_gen_fn) gen_sse2_movdqa,
    &operand_data[1708],
    2,
    0,
    2,
    1
  },
  {
    "sse2_movdqu",
    "movdqu\t{%1, %0|%0, %1}",
    (insn_gen_fn) gen_sse2_movdqu,
    &operand_data[1708],
    2,
    0,
    2,
    1
  },
  {
    "sse2_movdq2q",
    (const PTR) output_1007,
    (insn_gen_fn) gen_sse2_movdq2q,
    &operand_data[1710],
    2,
    0,
    2,
    2
  },
  {
    "sse2_movdq2q_rex64",
    (const PTR) output_1008,
    (insn_gen_fn) gen_sse2_movdq2q_rex64,
    &operand_data[1712],
    2,
    0,
    3,
    2
  },
  {
    "sse2_movq2dq",
    (const PTR) output_1009,
    (insn_gen_fn) gen_sse2_movq2dq,
    &operand_data[1714],
    2,
    0,
    2,
    2
  },
  {
    "sse2_movq2dq_rex64",
    (const PTR) output_1010,
    (insn_gen_fn) gen_sse2_movq2dq_rex64,
    &operand_data[1716],
    2,
    0,
    3,
    2
  },
  {
    "sse2_movq",
    "movq\t{%1, %0|%0, %1}",
    (insn_gen_fn) gen_sse2_movq,
    &operand_data[1718],
    2,
    0,
    1,
    1
  },
  {
    "sse2_loadd",
    "movd\t{%1, %0|%0, %1}",
    (insn_gen_fn) gen_sse2_loadd,
    &operand_data[1720],
    2,
    0,
    1,
    1
  },
  {
    "sse2_stored",
    "movd\t{%1, %0|%0, %1}",
    (insn_gen_fn) gen_sse2_stored,
    &operand_data[1722],
    2,
    0,
    1,
    1
  },
  {
    "sse2_movhpd",
    "movhpd\t{%2, %0|%0, %2}",
    (insn_gen_fn) gen_sse2_movhpd,
    &operand_data[1724],
    3,
    0,
    2,
    1
  },
  {
    "sse2_movlpd",
    "movlpd\t{%2, %0|%0, %2}",
    (insn_gen_fn) gen_sse2_movlpd,
    &operand_data[1724],
    3,
    0,
    2,
    1
  },
  {
    "sse2_loadsd_1",
    "movsd\t{%1, %0|%0, %1}",
    (insn_gen_fn) gen_sse2_loadsd_1,
    &operand_data[1727],
    3,
    0,
    1,
    1
  },
  {
    "sse2_movsd",
    "movsd\t{%2, %0|%0, %2}",
    (insn_gen_fn) gen_sse2_movsd,
    &operand_data[1685],
    3,
    0,
    1,
    1
  },
  {
    "sse2_storesd",
    "movsd\t{%1, %0|%0, %1}",
    (insn_gen_fn) gen_sse2_storesd,
    &operand_data[1730],
    2,
    0,
    1,
    1
  },
  {
    "sse2_shufpd",
    "shufpd\t{%3, %2, %0|%0, %2, %3}",
    (insn_gen_fn) gen_sse2_shufpd,
    &operand_data[1732],
    4,
    0,
    1,
    1
  },
  {
    "sse2_clflush",
    "clflush %0",
    (insn_gen_fn) gen_sse2_clflush,
    &operand_data[1736],
    1,
    0,
    1,
    1
  },
  {
    "*mfence_insn",
    "mfence",
    0,
    &operand_data[1522],
    1,
    1,
    0,
    1
  },
  {
    "*lfence_insn",
    "lfence",
    0,
    &operand_data[1522],
    1,
    1,
    0,
    1
  },
  {
    "mwait",
    "mwait\t%0, %1",
    (insn_gen_fn) gen_mwait,
    &operand_data[1737],
    2,
    0,
    1,
    1
  },
  {
    "monitor",
    "monitor\t%0, %1, %2",
    (insn_gen_fn) gen_monitor,
    &operand_data[1737],
    3,
    0,
    1,
    1
  },
  {
    "addsubv4sf3",
    "addsubps\t{%2, %0|%0, %2}",
    (insn_gen_fn) gen_addsubv4sf3,
    &operand_data[1391],
    3,
    0,
    1,
    1
  },
  {
    "addsubv2df3",
    "addsubpd\t{%2, %0|%0, %2}",
    (insn_gen_fn) gen_addsubv2df3,
    &operand_data[1553],
    3,
    0,
    1,
    1
  },
  {
    "haddv4sf3",
    "haddps\t{%2, %0|%0, %2}",
    (insn_gen_fn) gen_haddv4sf3,
    &operand_data[1391],
    3,
    0,
    1,
    1
  },
  {
    "haddv2df3",
    "haddpd\t{%2, %0|%0, %2}",
    (insn_gen_fn) gen_haddv2df3,
    &operand_data[1553],
    3,
    0,
    1,
    1
  },
  {
    "hsubv4sf3",
    "hsubps\t{%2, %0|%0, %2}",
    (insn_gen_fn) gen_hsubv4sf3,
    &operand_data[1391],
    3,
    0,
    1,
    1
  },
  {
    "hsubv2df3",
    "hsubpd\t{%2, %0|%0, %2}",
    (insn_gen_fn) gen_hsubv2df3,
    &operand_data[1553],
    3,
    0,
    1,
    1
  },
  {
    "movshdup",
    "movshdup\t{%1, %0|%0, %1}",
    (insn_gen_fn) gen_movshdup,
    &operand_data[1395],
    2,
    0,
    1,
    1
  },
  {
    "movsldup",
    "movsldup\t{%1, %0|%0, %1}",
    (insn_gen_fn) gen_movsldup,
    &operand_data[1395],
    2,
    0,
    1,
    1
  },
  {
    "lddqu",
    "lddqu\t{%1, %0|%0, %1}",
    (insn_gen_fn) gen_lddqu,
    &operand_data[1740],
    2,
    0,
    1,
    1
  },
  {
    "loadddup",
    "movddup\t{%1, %0|%0, %1}",
    (insn_gen_fn) gen_loadddup,
    &operand_data[1727],
    2,
    0,
    1,
    1
  },
  {
    "movddup",
    "movddup\t{%1, %0|%0, %1}",
    (insn_gen_fn) gen_movddup,
    &operand_data[1742],
    2,
    0,
    1,
    1
  },
  {
    "cmpdi",
    0,
    (insn_gen_fn) gen_cmpdi,
    &operand_data[1744],
    2,
    0,
    0,
    0
  },
  {
    "cmpsi",
    0,
    (insn_gen_fn) gen_cmpsi,
    &operand_data[1746],
    2,
    0,
    0,
    0
  },
  {
    "cmphi",
    0,
    (insn_gen_fn) gen_cmphi,
    &operand_data[1748],
    2,
    0,
    0,
    0
  },
  {
    "cmpqi",
    0,
    (insn_gen_fn) gen_cmpqi,
    &operand_data[1750],
    2,
    0,
    0,
    0
  },
  {
    "cmpdi_1_rex64",
    0,
    (insn_gen_fn) gen_cmpdi_1_rex64,
    &operand_data[1752],
    2,
    0,
    0,
    0
  },
  {
    "cmpsi_1",
    0,
    (insn_gen_fn) gen_cmpsi_1,
    &operand_data[9],
    2,
    0,
    2,
    0
  },
  {
    "cmpqi_ext_3",
    0,
    (insn_gen_fn) gen_cmpqi_ext_3,
    &operand_data[1754],
    2,
    0,
    0,
    0
  },
  {
    "cmpxf",
    0,
    (insn_gen_fn) gen_cmpxf,
    &operand_data[1756],
    2,
    0,
    0,
    0
  },
  {
    "cmptf",
    0,
    (insn_gen_fn) gen_cmptf,
    &operand_data[1758],
    2,
    0,
    0,
    0
  },
  {
    "cmpdf",
    0,
    (insn_gen_fn) gen_cmpdf,
    &operand_data[1760],
    2,
    0,
    0,
    0
  },
  {
    "cmpsf",
    0,
    (insn_gen_fn) gen_cmpsf,
    &operand_data[1762],
    2,
    0,
    0,
    0
  },
  {
    "movsi",
    0,
    (insn_gen_fn) gen_movsi,
    &operand_data[1764],
    2,
    0,
    0,
    0
  },
  {
    "movhi",
    0,
    (insn_gen_fn) gen_movhi,
    &operand_data[1748],
    2,
    0,
    0,
    0
  },
  {
    "movstricthi",
    0,
    (insn_gen_fn) gen_movstricthi,
    &operand_data[1766],
    2,
    0,
    0,
    0
  },
  {
    "movqi",
    0,
    (insn_gen_fn) gen_movqi,
    &operand_data[1750],
    2,
    0,
    0,
    0
  },
  {
    "reload_outqi",
    0,
    (insn_gen_fn) gen_reload_outqi,
    &operand_data[1768],
    3,
    0,
    1,
    0
  },
  {
    "movstrictqi",
    0,
    (insn_gen_fn) gen_movstrictqi,
    &operand_data[1771],
    2,
    0,
    0,
    0
  },
  {
    "movdi",
    0,
    (insn_gen_fn) gen_movdi,
    &operand_data[1752],
    2,
    0,
    0,
    0
  },
  {
    "movdi+1",
    0,
    0,
    &operand_data[1773],
    3,
    0,
    0,
    0
  },
  {
    "movdi+2",
    0,
    0,
    &operand_data[1773],
    2,
    0,
    0,
    0
  },
  {
    "movdi+3",
    0,
    0,
    &operand_data[1773],
    2,
    0,
    0,
    0
  },
  {
    "movdi+4",
    0,
    0,
    &operand_data[1776],
    2,
    0,
    0,
    0
  },
  {
    "movsf-4",
    0,
    0,
    &operand_data[1752],
    2,
    0,
    0,
    0
  },
  {
    "movsf-3",
    0,
    0,
    &operand_data[1778],
    3,
    0,
    0,
    0
  },
  {
    "movsf-2",
    0,
    0,
    &operand_data[1778],
    2,
    0,
    0,
    0
  },
  {
    "movsf-1",
    0,
    0,
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
    &operand_data[1781],
    2,
    0,
    0,
    0
  },
  {
    "movsf+1",
    0,
    0,
    &operand_data[1783],
    2,
    0,
    0,
    0
  },
  {
    "movsf+2",
    0,
    0,
    &operand_data[1785],
    2,
    0,
    0,
    0
  },
  {
    "movdf-1",
    0,
    0,
    &operand_data[1785],
    2,
    0,
    0,
    0
  },
  {
    "movdf",
    0,
    (insn_gen_fn) gen_movdf,
    &operand_data[1787],
    2,
    0,
    0,
    0
  },
  {
    "movdf+1",
    0,
    0,
    &operand_data[1789],
    2,
    0,
    0,
    0
  },
  {
    "movdf+2",
    0,
    0,
    &operand_data[1789],
    2,
    0,
    0,
    0
  },
  {
    "movxf-2",
    0,
    0,
    &operand_data[1791],
    2,
    0,
    0,
    0
  },
  {
    "movxf-1",
    0,
    0,
    &operand_data[1787],
    2,
    0,
    0,
    0
  },
  {
    "movxf",
    0,
    (insn_gen_fn) gen_movxf,
    &operand_data[1793],
    2,
    0,
    0,
    0
  },
  {
    "movtf",
    0,
    (insn_gen_fn) gen_movtf,
    &operand_data[1795],
    2,
    0,
    0,
    0
  },
  {
    "movtf+1",
    0,
    0,
    &operand_data[1797],
    2,
    0,
    0,
    0
  },
  {
    "movtf+2",
    0,
    0,
    &operand_data[1799],
    2,
    0,
    0,
    0
  },
  {
    "movtf+3",
    0,
    0,
    &operand_data[1801],
    2,
    0,
    0,
    0
  },
  {
    "zero_extendhisi2-3",
    0,
    0,
    &operand_data[1801],
    2,
    0,
    0,
    0
  },
  {
    "zero_extendhisi2-2",
    0,
    0,
    &operand_data[1803],
    2,
    0,
    0,
    0
  },
  {
    "zero_extendhisi2-1",
    0,
    0,
    &operand_data[1805],
    2,
    0,
    0,
    0
  },
  {
    "zero_extendhisi2",
    0,
    (insn_gen_fn) gen_zero_extendhisi2,
    &operand_data[1807],
    2,
    0,
    0,
    0
  },
  {
    "zero_extendhisi2+1",
    0,
    0,
    &operand_data[1809],
    2,
    0,
    0,
    0
  },
  {
    "zero_extendqihi2",
    0,
    (insn_gen_fn) gen_zero_extendqihi2,
    &operand_data[1810],
    2,
    0,
    0,
    0
  },
  {
    "zero_extendqihi2+1",
    0,
    0,
    &operand_data[1810],
    2,
    0,
    0,
    0
  },
  {
    "zero_extendqihi2+2",
    0,
    0,
    &operand_data[1810],
    2,
    0,
    0,
    0
  },
  {
    "zero_extendqisi2-1",
    0,
    0,
    &operand_data[1812],
    2,
    0,
    0,
    0
  },
  {
    "zero_extendqisi2",
    0,
    (insn_gen_fn) gen_zero_extendqisi2,
    &operand_data[1814],
    2,
    0,
    0,
    0
  },
  {
    "zero_extendqisi2+1",
    0,
    0,
    &operand_data[1814],
    2,
    0,
    0,
    0
  },
  {
    "zero_extendqisi2+2",
    0,
    0,
    &operand_data[1814],
    2,
    0,
    0,
    0
  },
  {
    "zero_extendsidi2-1",
    0,
    0,
    &operand_data[1816],
    2,
    0,
    0,
    0
  },
  {
    "zero_extendsidi2",
    0,
    (insn_gen_fn) gen_zero_extendsidi2,
    &operand_data[1818],
    2,
    0,
    1,
    0
  },
  {
    "zero_extendsidi2+1",
    0,
    0,
    &operand_data[1778],
    1,
    0,
    0,
    0
  },
  {
    "zero_extendsidi2+2",
    0,
    0,
    &operand_data[1820],
    2,
    0,
    0,
    0
  },
  {
    "extendsidi2-1",
    0,
    0,
    &operand_data[1822],
    2,
    0,
    0,
    0
  },
  {
    "extendsidi2",
    0,
    (insn_gen_fn) gen_extendsidi2,
    &operand_data[1824],
    3,
    0,
    0,
    0
  },
  {
    "extendsidi2+1",
    0,
    0,
    &operand_data[1827],
    3,
    0,
    0,
    0
  },
  {
    "extendsidi2+2",
    0,
    0,
    &operand_data[1827],
    3,
    0,
    0,
    0
  },
  {
    "extendsidi2+3",
    0,
    0,
    &operand_data[1824],
    3,
    0,
    0,
    0
  },
  {
    "extendsidi2+4",
    0,
    0,
    &operand_data[1830],
    2,
    0,
    0,
    0
  },
  {
    "extendsidi2+5",
    0,
    0,
    &operand_data[1830],
    2,
    0,
    0,
    0
  },
  {
    "extendsidi2+6",
    0,
    0,
    &operand_data[1832],
    2,
    0,
    0,
    0
  },
  {
    "extendsfdf2-5",
    0,
    0,
    &operand_data[1834],
    2,
    0,
    0,
    0
  },
  {
    "extendsfdf2-4",
    0,
    0,
    &operand_data[1834],
    2,
    0,
    0,
    0
  },
  {
    "extendsfdf2-3",
    0,
    0,
    &operand_data[1836],
    2,
    0,
    0,
    0
  },
  {
    "extendsfdf2-2",
    0,
    0,
    &operand_data[1838],
    2,
    0,
    0,
    0
  },
  {
    "extendsfdf2-1",
    0,
    0,
    &operand_data[1838],
    2,
    0,
    0,
    0
  },
  {
    "extendsfdf2",
    0,
    (insn_gen_fn) gen_extendsfdf2,
    &operand_data[1840],
    2,
    0,
    0,
    0
  },
  {
    "extendsfxf2",
    0,
    (insn_gen_fn) gen_extendsfxf2,
    &operand_data[1842],
    2,
    0,
    0,
    0
  },
  {
    "extendsftf2",
    0,
    (insn_gen_fn) gen_extendsftf2,
    &operand_data[1844],
    2,
    0,
    0,
    0
  },
  {
    "extenddfxf2",
    0,
    (insn_gen_fn) gen_extenddfxf2,
    &operand_data[1846],
    2,
    0,
    0,
    0
  },
  {
    "extenddftf2",
    0,
    (insn_gen_fn) gen_extenddftf2,
    &operand_data[1848],
    2,
    0,
    0,
    0
  },
  {
    "truncdfsf2",
    0,
    (insn_gen_fn) gen_truncdfsf2,
    &operand_data[1850],
    2,
    1,
    0,
    0
  },
  {
    "truncdfsf2+1",
    0,
    0,
    &operand_data[1852],
    3,
    0,
    0,
    0
  },
  {
    "truncdfsf2+2",
    0,
    0,
    &operand_data[1855],
    3,
    0,
    0,
    0
  },
  {
    "truncxfsf2-1",
    0,
    0,
    &operand_data[1858],
    3,
    0,
    0,
    0
  },
  {
    "truncxfsf2",
    0,
    (insn_gen_fn) gen_truncxfsf2,
    &operand_data[1861],
    2,
    1,
    0,
    0
  },
  {
    "truncxfsf2+1",
    0,
    0,
    &operand_data[1863],
    3,
    0,
    0,
    0
  },
  {
    "trunctfsf2-1",
    0,
    0,
    &operand_data[1866],
    3,
    0,
    0,
    0
  },
  {
    "trunctfsf2",
    0,
    (insn_gen_fn) gen_trunctfsf2,
    &operand_data[1869],
    2,
    1,
    0,
    0
  },
  {
    "trunctfsf2+1",
    0,
    0,
    &operand_data[1871],
    3,
    0,
    0,
    0
  },
  {
    "truncxfdf2-1",
    0,
    0,
    &operand_data[1874],
    3,
    0,
    0,
    0
  },
  {
    "truncxfdf2",
    0,
    (insn_gen_fn) gen_truncxfdf2,
    &operand_data[1877],
    2,
    1,
    0,
    0
  },
  {
    "truncxfdf2+1",
    0,
    0,
    &operand_data[1879],
    3,
    0,
    0,
    0
  },
  {
    "trunctfdf2-1",
    0,
    0,
    &operand_data[1882],
    3,
    0,
    0,
    0
  },
  {
    "trunctfdf2",
    0,
    (insn_gen_fn) gen_trunctfdf2,
    &operand_data[1885],
    2,
    1,
    0,
    0
  },
  {
    "trunctfdf2+1",
    0,
    0,
    &operand_data[1887],
    3,
    0,
    0,
    0
  },
  {
    "fix_truncxfdi2-1",
    0,
    0,
    &operand_data[1890],
    3,
    0,
    0,
    0
  },
  {
    "fix_truncxfdi2",
    0,
    (insn_gen_fn) gen_fix_truncxfdi2,
    &operand_data[1893],
    2,
    0,
    0,
    0
  },
  {
    "fix_trunctfdi2",
    0,
    (insn_gen_fn) gen_fix_trunctfdi2,
    &operand_data[1895],
    2,
    0,
    0,
    0
  },
  {
    "fix_truncdfdi2",
    0,
    (insn_gen_fn) gen_fix_truncdfdi2,
    &operand_data[1897],
    2,
    0,
    0,
    0
  },
  {
    "fix_truncsfdi2",
    0,
    (insn_gen_fn) gen_fix_truncsfdi2,
    &operand_data[1899],
    2,
    0,
    0,
    0
  },
  {
    "fix_truncsfdi2+1",
    0,
    0,
    &operand_data[1901],
    2,
    0,
    0,
    0
  },
  {
    "fix_truncsfdi2+2",
    0,
    0,
    &operand_data[1903],
    6,
    0,
    0,
    0
  },
  {
    "fix_truncxfsi2-1",
    0,
    0,
    &operand_data[1909],
    6,
    0,
    0,
    0
  },
  {
    "fix_truncxfsi2",
    0,
    (insn_gen_fn) gen_fix_truncxfsi2,
    &operand_data[1915],
    2,
    0,
    0,
    0
  },
  {
    "fix_trunctfsi2",
    0,
    (insn_gen_fn) gen_fix_trunctfsi2,
    &operand_data[1917],
    2,
    0,
    0,
    0
  },
  {
    "fix_truncdfsi2",
    0,
    (insn_gen_fn) gen_fix_truncdfsi2,
    &operand_data[1919],
    2,
    0,
    0,
    0
  },
  {
    "fix_truncsfsi2",
    0,
    (insn_gen_fn) gen_fix_truncsfsi2,
    &operand_data[1921],
    2,
    0,
    0,
    0
  },
  {
    "fix_truncsfsi2+1",
    0,
    0,
    &operand_data[1923],
    2,
    0,
    0,
    0
  },
  {
    "fix_truncsfsi2+2",
    0,
    0,
    &operand_data[1925],
    5,
    0,
    0,
    0
  },
  {
    "fix_truncxfhi2-1",
    0,
    0,
    &operand_data[1929],
    5,
    0,
    0,
    0
  },
  {
    "fix_truncxfhi2",
    0,
    (insn_gen_fn) gen_fix_truncxfhi2,
    &operand_data[1934],
    2,
    0,
    0,
    0
  },
  {
    "fix_trunctfhi2",
    0,
    (insn_gen_fn) gen_fix_trunctfhi2,
    &operand_data[1936],
    2,
    0,
    0,
    0
  },
  {
    "fix_truncdfhi2",
    0,
    (insn_gen_fn) gen_fix_truncdfhi2,
    &operand_data[1938],
    2,
    0,
    0,
    0
  },
  {
    "fix_truncsfhi2",
    0,
    (insn_gen_fn) gen_fix_truncsfhi2,
    &operand_data[1940],
    2,
    0,
    0,
    0
  },
  {
    "fix_truncsfhi2+1",
    0,
    0,
    &operand_data[1942],
    2,
    0,
    0,
    0
  },
  {
    "fix_truncsfhi2+2",
    0,
    0,
    &operand_data[1944],
    5,
    0,
    0,
    0
  },
  {
    "floatsisf2-1",
    0,
    0,
    &operand_data[1949],
    5,
    0,
    0,
    0
  },
  {
    "floatsisf2",
    0,
    (insn_gen_fn) gen_floatsisf2,
    &operand_data[1922],
    2,
    0,
    0,
    0
  },
  {
    "floatdisf2",
    0,
    (insn_gen_fn) gen_floatdisf2,
    &operand_data[1900],
    2,
    0,
    0,
    0
  },
  {
    "floatsidf2",
    0,
    (insn_gen_fn) gen_floatsidf2,
    &operand_data[1920],
    2,
    0,
    0,
    0
  },
  {
    "floatdidf2",
    0,
    (insn_gen_fn) gen_floatdidf2,
    &operand_data[1898],
    2,
    0,
    0,
    0
  },
  {
    "floatdidf2+1",
    0,
    0,
    &operand_data[1954],
    2,
    0,
    0,
    0
  },
  {
    "adddi3",
    0,
    (insn_gen_fn) gen_adddi3,
    &operand_data[1956],
    3,
    0,
    0,
    0
  },
  {
    "adddi3+1",
    0,
    0,
    &operand_data[1959],
    3,
    0,
    0,
    0
  },
  {
    "addsi3",
    0,
    (insn_gen_fn) gen_addsi3,
    &operand_data[1962],
    3,
    0,
    0,
    0
  },
  {
    "addsi3+1",
    0,
    0,
    &operand_data[1965],
    4,
    0,
    0,
    0
  },
  {
    "addsi3+2",
    0,
    0,
    &operand_data[1969],
    4,
    0,
    0,
    0
  },
  {
    "addsi3+3",
    0,
    0,
    &operand_data[1973],
    4,
    0,
    0,
    0
  },
  {
    "addsi3+4",
    0,
    0,
    &operand_data[1977],
    4,
    0,
    0,
    0
  },
  {
    "addsi3+5",
    0,
    0,
    &operand_data[1981],
    5,
    0,
    0,
    0
  },
  {
    "addhi3-4",
    0,
    0,
    &operand_data[1986],
    5,
    0,
    0,
    0
  },
  {
    "addhi3-3",
    0,
    0,
    &operand_data[1991],
    3,
    0,
    0,
    0
  },
  {
    "addhi3-2",
    0,
    0,
    &operand_data[1994],
    3,
    0,
    0,
    0
  },
  {
    "addhi3-1",
    0,
    0,
    &operand_data[1997],
    3,
    0,
    0,
    0
  },
  {
    "addhi3",
    0,
    (insn_gen_fn) gen_addhi3,
    &operand_data[2000],
    3,
    0,
    0,
    0
  },
  {
    "addqi3",
    0,
    (insn_gen_fn) gen_addqi3,
    &operand_data[2003],
    3,
    0,
    0,
    0
  },
  {
    "addxf3",
    0,
    (insn_gen_fn) gen_addxf3,
    &operand_data[2006],
    3,
    0,
    0,
    0
  },
  {
    "addtf3",
    0,
    (insn_gen_fn) gen_addtf3,
    &operand_data[2009],
    3,
    0,
    0,
    0
  },
  {
    "adddf3",
    0,
    (insn_gen_fn) gen_adddf3,
    &operand_data[2012],
    3,
    0,
    0,
    0
  },
  {
    "addsf3",
    0,
    (insn_gen_fn) gen_addsf3,
    &operand_data[2015],
    3,
    0,
    0,
    0
  },
  {
    "subdi3",
    0,
    (insn_gen_fn) gen_subdi3,
    &operand_data[1956],
    3,
    0,
    0,
    0
  },
  {
    "subdi3+1",
    0,
    0,
    &operand_data[1959],
    3,
    0,
    0,
    0
  },
  {
    "subsi3",
    0,
    (insn_gen_fn) gen_subsi3,
    &operand_data[1962],
    3,
    0,
    0,
    0
  },
  {
    "subhi3",
    0,
    (insn_gen_fn) gen_subhi3,
    &operand_data[2000],
    3,
    0,
    0,
    0
  },
  {
    "subqi3",
    0,
    (insn_gen_fn) gen_subqi3,
    &operand_data[2003],
    3,
    0,
    0,
    0
  },
  {
    "subxf3",
    0,
    (insn_gen_fn) gen_subxf3,
    &operand_data[2006],
    3,
    0,
    0,
    0
  },
  {
    "subtf3",
    0,
    (insn_gen_fn) gen_subtf3,
    &operand_data[2009],
    3,
    0,
    0,
    0
  },
  {
    "subdf3",
    0,
    (insn_gen_fn) gen_subdf3,
    &operand_data[2012],
    3,
    0,
    0,
    0
  },
  {
    "subsf3",
    0,
    (insn_gen_fn) gen_subsf3,
    &operand_data[2015],
    3,
    0,
    0,
    0
  },
  {
    "muldi3",
    0,
    (insn_gen_fn) gen_muldi3,
    &operand_data[2018],
    3,
    0,
    0,
    0
  },
  {
    "mulsi3",
    0,
    (insn_gen_fn) gen_mulsi3,
    &operand_data[2021],
    3,
    0,
    0,
    0
  },
  {
    "mulhi3",
    0,
    (insn_gen_fn) gen_mulhi3,
    &operand_data[2024],
    3,
    0,
    0,
    0
  },
  {
    "mulqi3",
    0,
    (insn_gen_fn) gen_mulqi3,
    &operand_data[2027],
    3,
    0,
    0,
    0
  },
  {
    "umulqihi3",
    0,
    (insn_gen_fn) gen_umulqihi3,
    &operand_data[2030],
    3,
    0,
    0,
    0
  },
  {
    "mulqihi3",
    0,
    (insn_gen_fn) gen_mulqihi3,
    &operand_data[2030],
    3,
    0,
    0,
    0
  },
  {
    "umulditi3",
    0,
    (insn_gen_fn) gen_umulditi3,
    &operand_data[2033],
    3,
    0,
    0,
    0
  },
  {
    "umulsidi3",
    0,
    (insn_gen_fn) gen_umulsidi3,
    &operand_data[2035],
    3,
    0,
    0,
    0
  },
  {
    "mulditi3",
    0,
    (insn_gen_fn) gen_mulditi3,
    &operand_data[2033],
    3,
    0,
    0,
    0
  },
  {
    "mulsidi3",
    0,
    (insn_gen_fn) gen_mulsidi3,
    &operand_data[2035],
    3,
    0,
    0,
    0
  },
  {
    "umuldi3_highpart",
    0,
    (insn_gen_fn) gen_umuldi3_highpart,
    &operand_data[2038],
    4,
    0,
    0,
    0
  },
  {
    "umulsi3_highpart",
    0,
    (insn_gen_fn) gen_umulsi3_highpart,
    &operand_data[2042],
    4,
    0,
    0,
    0
  },
  {
    "smuldi3_highpart",
    0,
    (insn_gen_fn) gen_smuldi3_highpart,
    &operand_data[2046],
    4,
    0,
    1,
    0
  },
  {
    "smulsi3_highpart",
    0,
    (insn_gen_fn) gen_smulsi3_highpart,
    &operand_data[2042],
    4,
    0,
    0,
    0
  },
  {
    "mulxf3",
    0,
    (insn_gen_fn) gen_mulxf3,
    &operand_data[2006],
    3,
    0,
    0,
    0
  },
  {
    "multf3",
    0,
    (insn_gen_fn) gen_multf3,
    &operand_data[2009],
    3,
    0,
    0,
    0
  },
  {
    "muldf3",
    0,
    (insn_gen_fn) gen_muldf3,
    &operand_data[2012],
    3,
    0,
    0,
    0
  },
  {
    "mulsf3",
    0,
    (insn_gen_fn) gen_mulsf3,
    &operand_data[2015],
    3,
    0,
    0,
    0
  },
  {
    "divxf3",
    0,
    (insn_gen_fn) gen_divxf3,
    &operand_data[2006],
    3,
    0,
    0,
    0
  },
  {
    "divtf3",
    0,
    (insn_gen_fn) gen_divtf3,
    &operand_data[2009],
    3,
    0,
    0,
    0
  },
  {
    "divdf3",
    0,
    (insn_gen_fn) gen_divdf3,
    &operand_data[2012],
    3,
    0,
    0,
    0
  },
  {
    "divsf3",
    0,
    (insn_gen_fn) gen_divsf3,
    &operand_data[2015],
    3,
    0,
    0,
    0
  },
  {
    "divmoddi4",
    0,
    (insn_gen_fn) gen_divmoddi4,
    &operand_data[2050],
    4,
    2,
    0,
    0
  },
  {
    "divmoddi4+1",
    0,
    0,
    &operand_data[2050],
    4,
    0,
    0,
    0
  },
  {
    "divmodsi4",
    0,
    (insn_gen_fn) gen_divmodsi4,
    &operand_data[2054],
    4,
    2,
    0,
    0
  },
  {
    "divmodsi4+1",
    0,
    0,
    &operand_data[2054],
    4,
    0,
    0,
    0
  },
  {
    "divmodsi4+2",
    0,
    0,
    &operand_data[2050],
    4,
    0,
    0,
    0
  },
  {
    "udivmodhi4-1",
    0,
    0,
    &operand_data[2054],
    4,
    0,
    0,
    0
  },
  {
    "udivmodhi4",
    0,
    (insn_gen_fn) gen_udivmodhi4,
    &operand_data[2058],
    4,
    4,
    0,
    0
  },
  {
    "testsi_ccno_1",
    0,
    (insn_gen_fn) gen_testsi_ccno_1,
    &operand_data[2062],
    2,
    0,
    0,
    0
  },
  {
    "testqi_ccz_1",
    0,
    (insn_gen_fn) gen_testqi_ccz_1,
    &operand_data[2064],
    2,
    0,
    0,
    0
  },
  {
    "testqi_ext_ccno_0",
    0,
    (insn_gen_fn) gen_testqi_ext_ccno_0,
    &operand_data[2066],
    2,
    0,
    0,
    0
  },
  {
    "testqi_ext_ccno_0+1",
    0,
    0,
    &operand_data[2068],
    3,
    0,
    0,
    0
  },
  {
    "testqi_ext_ccno_0+2",
    0,
    0,
    &operand_data[2071],
    2,
    0,
    0,
    0
  },
  {
    "anddi3-1",
    0,
    0,
    &operand_data[2068],
    2,
    0,
    0,
    0
  },
  {
    "anddi3",
    0,
    (insn_gen_fn) gen_anddi3,
    &operand_data[2073],
    3,
    0,
    0,
    0
  },
  {
    "andsi3",
    0,
    (insn_gen_fn) gen_andsi3,
    &operand_data[1962],
    3,
    0,
    0,
    0
  },
  {
    "andsi3+1",
    0,
    0,
    &operand_data[1805],
    1,
    0,
    0,
    0
  },
  {
    "andsi3+2",
    0,
    0,
    &operand_data[1754],
    1,
    0,
    0,
    0
  },
  {
    "andhi3-1",
    0,
    0,
    &operand_data[1754],
    1,
    0,
    0,
    0
  },
  {
    "andhi3",
    0,
    (insn_gen_fn) gen_andhi3,
    &operand_data[2000],
    3,
    0,
    0,
    0
  },
  {
    "andqi3",
    0,
    (insn_gen_fn) gen_andqi3,
    &operand_data[2003],
    3,
    0,
    0,
    0
  },
  {
    "andqi3+1",
    0,
    0,
    &operand_data[2076],
    3,
    0,
    0,
    0
  },
  {
    "iordi3-1",
    0,
    0,
    &operand_data[2079],
    3,
    0,
    0,
    0
  },
  {
    "iordi3",
    0,
    (insn_gen_fn) gen_iordi3,
    &operand_data[1956],
    3,
    0,
    0,
    0
  },
  {
    "iorsi3",
    0,
    (insn_gen_fn) gen_iorsi3,
    &operand_data[1962],
    3,
    0,
    0,
    0
  },
  {
    "iorhi3",
    0,
    (insn_gen_fn) gen_iorhi3,
    &operand_data[2000],
    3,
    0,
    0,
    0
  },
  {
    "iorqi3",
    0,
    (insn_gen_fn) gen_iorqi3,
    &operand_data[2003],
    3,
    0,
    0,
    0
  },
  {
    "iorqi3+1",
    0,
    0,
    &operand_data[2076],
    3,
    0,
    0,
    0
  },
  {
    "xordi3-1",
    0,
    0,
    &operand_data[2079],
    3,
    0,
    0,
    0
  },
  {
    "xordi3",
    0,
    (insn_gen_fn) gen_xordi3,
    &operand_data[1956],
    3,
    0,
    0,
    0
  },
  {
    "xorsi3",
    0,
    (insn_gen_fn) gen_xorsi3,
    &operand_data[1962],
    3,
    0,
    0,
    0
  },
  {
    "xorhi3",
    0,
    (insn_gen_fn) gen_xorhi3,
    &operand_data[2000],
    3,
    0,
    0,
    0
  },
  {
    "xorqi3",
    0,
    (insn_gen_fn) gen_xorqi3,
    &operand_data[2003],
    3,
    0,
    0,
    0
  },
  {
    "xorqi_cc_ext_1",
    0,
    (insn_gen_fn) gen_xorqi_cc_ext_1,
    &operand_data[2082],
    3,
    2,
    0,
    0
  },
  {
    "xorqi_cc_ext_1+1",
    0,
    0,
    &operand_data[2076],
    3,
    0,
    0,
    0
  },
  {
    "negdi2-1",
    0,
    0,
    &operand_data[2079],
    3,
    0,
    0,
    0
  },
  {
    "negdi2",
    0,
    (insn_gen_fn) gen_negdi2,
    &operand_data[1956],
    2,
    0,
    0,
    0
  },
  {
    "negdi2+1",
    0,
    0,
    &operand_data[1752],
    2,
    0,
    0,
    0
  },
  {
    "negsi2",
    0,
    (insn_gen_fn) gen_negsi2,
    &operand_data[1962],
    2,
    0,
    0,
    0
  },
  {
    "neghi2",
    0,
    (insn_gen_fn) gen_neghi2,
    &operand_data[2000],
    2,
    0,
    0,
    0
  },
  {
    "negqi2",
    0,
    (insn_gen_fn) gen_negqi2,
    &operand_data[2003],
    2,
    0,
    0,
    0
  },
  {
    "negsf2",
    0,
    (insn_gen_fn) gen_negsf2,
    &operand_data[2085],
    2,
    0,
    0,
    0
  },
  {
    "negsf2+1",
    0,
    0,
    &operand_data[2087],
    3,
    0,
    0,
    0
  },
  {
    "negsf2+2",
    0,
    0,
    &operand_data[2090],
    3,
    0,
    0,
    0
  },
  {
    "negsf2+3",
    0,
    0,
    &operand_data[2093],
    3,
    0,
    0,
    0
  },
  {
    "negdf2-3",
    0,
    0,
    &operand_data[2096],
    2,
    0,
    0,
    0
  },
  {
    "negdf2-2",
    0,
    0,
    &operand_data[2098],
    2,
    0,
    0,
    0
  },
  {
    "negdf2-1",
    0,
    0,
    &operand_data[2100],
    2,
    0,
    0,
    0
  },
  {
    "negdf2",
    0,
    (insn_gen_fn) gen_negdf2,
    &operand_data[2102],
    2,
    0,
    0,
    0
  },
  {
    "negdf2+1",
    0,
    0,
    &operand_data[2104],
    3,
    0,
    0,
    0
  },
  {
    "negdf2+2",
    0,
    0,
    &operand_data[2107],
    3,
    0,
    0,
    0
  },
  {
    "negdf2+3",
    0,
    0,
    &operand_data[2107],
    3,
    0,
    0,
    0
  },
  {
    "negxf2-3",
    0,
    0,
    &operand_data[2110],
    3,
    0,
    0,
    0
  },
  {
    "negxf2-2",
    0,
    0,
    &operand_data[2113],
    2,
    0,
    0,
    0
  },
  {
    "negxf2-1",
    0,
    0,
    &operand_data[2115],
    2,
    0,
    0,
    0
  },
  {
    "negxf2",
    0,
    (insn_gen_fn) gen_negxf2,
    &operand_data[2117],
    2,
    0,
    0,
    0
  },
  {
    "negtf2",
    0,
    (insn_gen_fn) gen_negtf2,
    &operand_data[2119],
    2,
    0,
    0,
    0
  },
  {
    "negtf2+1",
    0,
    0,
    &operand_data[2121],
    2,
    0,
    0,
    0
  },
  {
    "negtf2+2",
    0,
    0,
    &operand_data[2123],
    2,
    0,
    0,
    0
  },
  {
    "abssf2-2",
    0,
    0,
    &operand_data[2125],
    2,
    0,
    0,
    0
  },
  {
    "abssf2-1",
    0,
    0,
    &operand_data[2127],
    2,
    0,
    0,
    0
  },
  {
    "abssf2",
    0,
    (insn_gen_fn) gen_abssf2,
    &operand_data[2085],
    2,
    0,
    0,
    0
  },
  {
    "abssf2+1",
    0,
    0,
    &operand_data[2087],
    3,
    0,
    0,
    0
  },
  {
    "abssf2+2",
    0,
    0,
    &operand_data[2090],
    3,
    0,
    0,
    0
  },
  {
    "abssf2+3",
    0,
    0,
    &operand_data[2093],
    3,
    0,
    0,
    0
  },
  {
    "absdf2-3",
    0,
    0,
    &operand_data[2096],
    2,
    0,
    0,
    0
  },
  {
    "absdf2-2",
    0,
    0,
    &operand_data[2098],
    2,
    0,
    0,
    0
  },
  {
    "absdf2-1",
    0,
    0,
    &operand_data[2100],
    2,
    0,
    0,
    0
  },
  {
    "absdf2",
    0,
    (insn_gen_fn) gen_absdf2,
    &operand_data[2102],
    2,
    0,
    0,
    0
  },
  {
    "absdf2+1",
    0,
    0,
    &operand_data[2104],
    3,
    0,
    0,
    0
  },
  {
    "absdf2+2",
    0,
    0,
    &operand_data[2107],
    3,
    0,
    0,
    0
  },
  {
    "absdf2+3",
    0,
    0,
    &operand_data[2110],
    3,
    0,
    0,
    0
  },
  {
    "absxf2-2",
    0,
    0,
    &operand_data[2113],
    2,
    0,
    0,
    0
  },
  {
    "absxf2-1",
    0,
    0,
    &operand_data[2115],
    2,
    0,
    0,
    0
  },
  {
    "absxf2",
    0,
    (insn_gen_fn) gen_absxf2,
    &operand_data[2117],
    2,
    0,
    0,
    0
  },
  {
    "abstf2",
    0,
    (insn_gen_fn) gen_abstf2,
    &operand_data[2119],
    2,
    0,
    0,
    0
  },
  {
    "abstf2+1",
    0,
    0,
    &operand_data[2121],
    2,
    0,
    0,
    0
  },
  {
    "abstf2+2",
    0,
    0,
    &operand_data[2123],
    2,
    0,
    0,
    0
  },
  {
    "one_cmpldi2-2",
    0,
    0,
    &operand_data[2125],
    2,
    0,
    0,
    0
  },
  {
    "one_cmpldi2-1",
    0,
    0,
    &operand_data[2129],
    2,
    0,
    0,
    0
  },
  {
    "one_cmpldi2",
    0,
    (insn_gen_fn) gen_one_cmpldi2,
    &operand_data[1956],
    2,
    0,
    0,
    0
  },
  {
    "one_cmpldi2+1",
    0,
    0,
    &operand_data[1956],
    2,
    0,
    0,
    0
  },
  {
    "one_cmplsi2",
    0,
    (insn_gen_fn) gen_one_cmplsi2,
    &operand_data[1962],
    2,
    0,
    0,
    0
  },
  {
    "one_cmplsi2+1",
    0,
    0,
    &operand_data[1962],
    2,
    0,
    0,
    0
  },
  {
    "one_cmplhi2-1",
    0,
    0,
    &operand_data[1820],
    2,
    0,
    0,
    0
  },
  {
    "one_cmplhi2",
    0,
    (insn_gen_fn) gen_one_cmplhi2,
    &operand_data[2000],
    2,
    0,
    0,
    0
  },
  {
    "one_cmplhi2+1",
    0,
    0,
    &operand_data[2000],
    2,
    0,
    0,
    0
  },
  {
    "one_cmplqi2",
    0,
    (insn_gen_fn) gen_one_cmplqi2,
    &operand_data[2003],
    2,
    0,
    0,
    0
  },
  {
    "one_cmplqi2+1",
    0,
    0,
    &operand_data[2003],
    2,
    0,
    0,
    0
  },
  {
    "ashldi3",
    0,
    (insn_gen_fn) gen_ashldi3,
    &operand_data[2131],
    3,
    0,
    0,
    0
  },
  {
    "ashldi3+1",
    0,
    0,
    &operand_data[2134],
    3,
    0,
    0,
    0
  },
  {
    "ashldi3+2",
    0,
    0,
    &operand_data[2137],
    4,
    0,
    0,
    0
  },
  {
    "x86_shift_adj_1-1",
    0,
    0,
    &operand_data[2137],
    3,
    0,
    0,
    0
  },
  {
    "x86_shift_adj_1",
    0,
    (insn_gen_fn) gen_x86_shift_adj_1,
    &operand_data[2141],
    4,
    3,
    1,
    0
  },
  {
    "x86_shift_adj_2",
    0,
    (insn_gen_fn) gen_x86_shift_adj_2,
    &operand_data[2141],
    3,
    0,
    0,
    0
  },
  {
    "ashlsi3",
    0,
    (insn_gen_fn) gen_ashlsi3,
    &operand_data[2145],
    3,
    0,
    0,
    0
  },
  {
    "ashlsi3+1",
    0,
    0,
    &operand_data[2148],
    3,
    0,
    0,
    0
  },
  {
    "ashlsi3+2",
    0,
    0,
    &operand_data[2151],
    3,
    0,
    0,
    0
  },
  {
    "ashlhi3-1",
    0,
    0,
    &operand_data[2154],
    3,
    0,
    0,
    0
  },
  {
    "ashlhi3",
    0,
    (insn_gen_fn) gen_ashlhi3,
    &operand_data[2157],
    3,
    0,
    0,
    0
  },
  {
    "ashlqi3",
    0,
    (insn_gen_fn) gen_ashlqi3,
    &operand_data[2160],
    3,
    0,
    0,
    0
  },
  {
    "ashrdi3",
    0,
    (insn_gen_fn) gen_ashrdi3,
    &operand_data[2131],
    3,
    0,
    0,
    0
  },
  {
    "ashrdi3+1",
    0,
    0,
    &operand_data[2137],
    4,
    0,
    0,
    0
  },
  {
    "x86_shift_adj_3-1",
    0,
    0,
    &operand_data[2137],
    3,
    0,
    0,
    0
  },
  {
    "x86_shift_adj_3",
    0,
    (insn_gen_fn) gen_x86_shift_adj_3,
    &operand_data[2141],
    3,
    0,
    0,
    0
  },
  {
    "ashrsi3",
    0,
    (insn_gen_fn) gen_ashrsi3,
    &operand_data[2145],
    3,
    0,
    0,
    0
  },
  {
    "ashrhi3",
    0,
    (insn_gen_fn) gen_ashrhi3,
    &operand_data[2157],
    3,
    0,
    0,
    0
  },
  {
    "ashrqi3",
    0,
    (insn_gen_fn) gen_ashrqi3,
    &operand_data[2160],
    3,
    0,
    0,
    0
  },
  {
    "lshrdi3",
    0,
    (insn_gen_fn) gen_lshrdi3,
    &operand_data[2131],
    3,
    0,
    0,
    0
  },
  {
    "lshrdi3+1",
    0,
    0,
    &operand_data[2137],
    4,
    0,
    0,
    0
  },
  {
    "lshrsi3-1",
    0,
    0,
    &operand_data[2137],
    3,
    0,
    0,
    0
  },
  {
    "lshrsi3",
    0,
    (insn_gen_fn) gen_lshrsi3,
    &operand_data[2145],
    3,
    0,
    0,
    0
  },
  {
    "lshrhi3",
    0,
    (insn_gen_fn) gen_lshrhi3,
    &operand_data[2157],
    3,
    0,
    0,
    0
  },
  {
    "lshrqi3",
    0,
    (insn_gen_fn) gen_lshrqi3,
    &operand_data[2160],
    3,
    0,
    0,
    0
  },
  {
    "rotldi3",
    0,
    (insn_gen_fn) gen_rotldi3,
    &operand_data[2163],
    3,
    0,
    0,
    0
  },
  {
    "rotlsi3",
    0,
    (insn_gen_fn) gen_rotlsi3,
    &operand_data[2145],
    3,
    0,
    0,
    0
  },
  {
    "rotlhi3",
    0,
    (insn_gen_fn) gen_rotlhi3,
    &operand_data[2157],
    3,
    0,
    0,
    0
  },
  {
    "rotlqi3",
    0,
    (insn_gen_fn) gen_rotlqi3,
    &operand_data[2160],
    3,
    0,
    0,
    0
  },
  {
    "rotrdi3",
    0,
    (insn_gen_fn) gen_rotrdi3,
    &operand_data[2163],
    3,
    0,
    0,
    0
  },
  {
    "rotrsi3",
    0,
    (insn_gen_fn) gen_rotrsi3,
    &operand_data[2145],
    3,
    0,
    0,
    0
  },
  {
    "rotrhi3",
    0,
    (insn_gen_fn) gen_rotrhi3,
    &operand_data[2157],
    3,
    0,
    0,
    0
  },
  {
    "rotrqi3",
    0,
    (insn_gen_fn) gen_rotrqi3,
    &operand_data[2160],
    3,
    0,
    0,
    0
  },
  {
    "extv",
    0,
    (insn_gen_fn) gen_extv,
    &operand_data[2166],
    4,
    0,
    0,
    0
  },
  {
    "extzv",
    0,
    (insn_gen_fn) gen_extzv,
    &operand_data[2170],
    4,
    0,
    0,
    0
  },
  {
    "insv",
    0,
    (insn_gen_fn) gen_insv,
    &operand_data[2171],
    4,
    0,
    0,
    0
  },
  {
    "seq",
    0,
    (insn_gen_fn) gen_seq,
    &operand_data[1813],
    1,
    0,
    0,
    0
  },
  {
    "sne",
    0,
    (insn_gen_fn) gen_sne,
    &operand_data[1813],
    1,
    0,
    0,
    0
  },
  {
    "sgt",
    0,
    (insn_gen_fn) gen_sgt,
    &operand_data[1813],
    1,
    0,
    0,
    0
  },
  {
    "sgtu",
    0,
    (insn_gen_fn) gen_sgtu,
    &operand_data[1813],
    1,
    0,
    0,
    0
  },
  {
    "slt",
    0,
    (insn_gen_fn) gen_slt,
    &operand_data[1813],
    1,
    0,
    0,
    0
  },
  {
    "sltu",
    0,
    (insn_gen_fn) gen_sltu,
    &operand_data[1813],
    1,
    0,
    0,
    0
  },
  {
    "sge",
    0,
    (insn_gen_fn) gen_sge,
    &operand_data[1813],
    1,
    0,
    0,
    0
  },
  {
    "sgeu",
    0,
    (insn_gen_fn) gen_sgeu,
    &operand_data[1813],
    1,
    0,
    0,
    0
  },
  {
    "sle",
    0,
    (insn_gen_fn) gen_sle,
    &operand_data[1813],
    1,
    0,
    0,
    0
  },
  {
    "sleu",
    0,
    (insn_gen_fn) gen_sleu,
    &operand_data[1813],
    1,
    0,
    0,
    0
  },
  {
    "sunordered",
    0,
    (insn_gen_fn) gen_sunordered,
    &operand_data[1813],
    1,
    0,
    0,
    0
  },
  {
    "sordered",
    0,
    (insn_gen_fn) gen_sordered,
    &operand_data[1813],
    1,
    0,
    0,
    0
  },
  {
    "suneq",
    0,
    (insn_gen_fn) gen_suneq,
    &operand_data[1813],
    1,
    0,
    0,
    0
  },
  {
    "sunge",
    0,
    (insn_gen_fn) gen_sunge,
    &operand_data[1813],
    1,
    0,
    0,
    0
  },
  {
    "sungt",
    0,
    (insn_gen_fn) gen_sungt,
    &operand_data[1813],
    1,
    0,
    0,
    0
  },
  {
    "sunle",
    0,
    (insn_gen_fn) gen_sunle,
    &operand_data[1813],
    1,
    0,
    0,
    0
  },
  {
    "sunlt",
    0,
    (insn_gen_fn) gen_sunlt,
    &operand_data[1813],
    1,
    0,
    0,
    0
  },
  {
    "sltgt",
    0,
    (insn_gen_fn) gen_sltgt,
    &operand_data[1813],
    1,
    0,
    0,
    0
  },
  {
    "sltgt+1",
    0,
    0,
    &operand_data[2175],
    2,
    0,
    0,
    0
  },
  {
    "sltgt+2",
    0,
    0,
    &operand_data[2177],
    2,
    0,
    0,
    0
  },
  {
    "beq-2",
    0,
    0,
    &operand_data[2175],
    2,
    0,
    0,
    0
  },
  {
    "beq-1",
    0,
    0,
    &operand_data[2177],
    2,
    0,
    0,
    0
  },
  {
    "beq",
    0,
    (insn_gen_fn) gen_beq,
    &operand_data[856],
    1,
    1,
    0,
    0
  },
  {
    "bne",
    0,
    (insn_gen_fn) gen_bne,
    &operand_data[856],
    1,
    1,
    0,
    0
  },
  {
    "bgt",
    0,
    (insn_gen_fn) gen_bgt,
    &operand_data[856],
    1,
    1,
    0,
    0
  },
  {
    "bgtu",
    0,
    (insn_gen_fn) gen_bgtu,
    &operand_data[856],
    1,
    1,
    0,
    0
  },
  {
    "blt",
    0,
    (insn_gen_fn) gen_blt,
    &operand_data[856],
    1,
    1,
    0,
    0
  },
  {
    "bltu",
    0,
    (insn_gen_fn) gen_bltu,
    &operand_data[856],
    1,
    1,
    0,
    0
  },
  {
    "bge",
    0,
    (insn_gen_fn) gen_bge,
    &operand_data[856],
    1,
    1,
    0,
    0
  },
  {
    "bgeu",
    0,
    (insn_gen_fn) gen_bgeu,
    &operand_data[856],
    1,
    1,
    0,
    0
  },
  {
    "ble",
    0,
    (insn_gen_fn) gen_ble,
    &operand_data[856],
    1,
    1,
    0,
    0
  },
  {
    "bleu",
    0,
    (insn_gen_fn) gen_bleu,
    &operand_data[856],
    1,
    1,
    0,
    0
  },
  {
    "bunordered",
    0,
    (insn_gen_fn) gen_bunordered,
    &operand_data[856],
    1,
    1,
    0,
    0
  },
  {
    "bordered",
    0,
    (insn_gen_fn) gen_bordered,
    &operand_data[856],
    1,
    1,
    0,
    0
  },
  {
    "buneq",
    0,
    (insn_gen_fn) gen_buneq,
    &operand_data[856],
    1,
    1,
    0,
    0
  },
  {
    "bunge",
    0,
    (insn_gen_fn) gen_bunge,
    &operand_data[856],
    1,
    1,
    0,
    0
  },
  {
    "bungt",
    0,
    (insn_gen_fn) gen_bungt,
    &operand_data[856],
    1,
    1,
    0,
    0
  },
  {
    "bunle",
    0,
    (insn_gen_fn) gen_bunle,
    &operand_data[856],
    1,
    1,
    0,
    0
  },
  {
    "bunlt",
    0,
    (insn_gen_fn) gen_bunlt,
    &operand_data[856],
    1,
    1,
    0,
    0
  },
  {
    "bltgt",
    0,
    (insn_gen_fn) gen_bltgt,
    &operand_data[856],
    1,
    1,
    0,
    0
  },
  {
    "bltgt+1",
    0,
    0,
    &operand_data[2178],
    2,
    0,
    0,
    0
  },
  {
    "bltgt+2",
    0,
    0,
    &operand_data[2178],
    2,
    0,
    0,
    0
  },
  {
    "indirect_jump-2",
    0,
    0,
    &operand_data[2180],
    5,
    0,
    0,
    0
  },
  {
    "indirect_jump-1",
    0,
    0,
    &operand_data[2180],
    6,
    0,
    0,
    0
  },
  {
    "indirect_jump",
    0,
    (insn_gen_fn) gen_indirect_jump,
    &operand_data[587],
    1,
    0,
    1,
    0
  },
  {
    "tablejump",
    0,
    (insn_gen_fn) gen_tablejump,
    &operand_data[2186],
    2,
    0,
    1,
    0
  },
  {
    "doloop_end",
    0,
    (insn_gen_fn) gen_doloop_end,
    &operand_data[2187],
    5,
    0,
    0,
    0
  },
  {
    "doloop_end+1",
    0,
    0,
    &operand_data[2191],
    3,
    0,
    0,
    0
  },
  {
    "doloop_end+2",
    0,
    0,
    &operand_data[2194],
    4,
    0,
    0,
    0
  },
  {
    "call_pop-2",
    0,
    0,
    &operand_data[2198],
    4,
    0,
    0,
    0
  },
  {
    "call_pop-1",
    0,
    0,
    &operand_data[2198],
    4,
    0,
    0,
    0
  },
  {
    "call_pop",
    0,
    (insn_gen_fn) gen_call_pop,
    &operand_data[2202],
    4,
    0,
    0,
    0
  },
  {
    "call",
    0,
    (insn_gen_fn) gen_call,
    &operand_data[2206],
    3,
    0,
    0,
    0
  },
  {
    "call_value_pop",
    0,
    (insn_gen_fn) gen_call_value_pop,
    &operand_data[2208],
    5,
    0,
    0,
    0
  },
  {
    "call_value",
    0,
    (insn_gen_fn) gen_call_value,
    &operand_data[2213],
    4,
    0,
    0,
    0
  },
  {
    "untyped_call",
    0,
    (insn_gen_fn) gen_untyped_call,
    &operand_data[2187],
    3,
    0,
    0,
    0
  },
  {
    "return",
    0,
    (insn_gen_fn) gen_return,
    &operand_data[0],
    0,
    0,
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
    "eh_return",
    0,
    (insn_gen_fn) gen_eh_return,
    &operand_data[1805],
    1,
    0,
    0,
    0
  },
  {
    "eh_return+1",
    0,
    0,
    &operand_data[1807],
    1,
    0,
    0,
    0
  },
  {
    "ffssi2-1",
    0,
    0,
    &operand_data[1820],
    1,
    0,
    0,
    0
  },
  {
    "ffssi2",
    0,
    (insn_gen_fn) gen_ffssi2,
    &operand_data[1962],
    2,
    0,
    0,
    0
  },
  {
    "tls_global_dynamic_32",
    0,
    (insn_gen_fn) gen_tls_global_dynamic_32,
    &operand_data[2217],
    6,
    2,
    0,
    0
  },
  {
    "tls_global_dynamic_64",
    0,
    (insn_gen_fn) gen_tls_global_dynamic_64,
    &operand_data[2223],
    2,
    1,
    0,
    0
  },
  {
    "tls_local_dynamic_base_32",
    0,
    (insn_gen_fn) gen_tls_local_dynamic_base_32,
    &operand_data[2225],
    5,
    2,
    0,
    0
  },
  {
    "tls_local_dynamic_base_64",
    0,
    (insn_gen_fn) gen_tls_local_dynamic_base_64,
    &operand_data[1820],
    1,
    1,
    0,
    0
  },
  {
    "tls_local_dynamic_base_64+1",
    0,
    0,
    &operand_data[2230],
    6,
    0,
    0,
    0
  },
  {
    "tls_local_dynamic_base_64+2",
    0,
    0,
    &operand_data[2236],
    4,
    0,
    0,
    0
  },
  {
    "sqrtsf2-1",
    0,
    0,
    &operand_data[2240],
    4,
    0,
    0,
    0
  },
  {
    "sqrtsf2",
    0,
    (insn_gen_fn) gen_sqrtsf2,
    &operand_data[2016],
    2,
    0,
    0,
    0
  },
  {
    "sqrtdf2",
    0,
    (insn_gen_fn) gen_sqrtdf2,
    &operand_data[2013],
    2,
    0,
    0,
    0
  },
  {
    "movstrsi",
    0,
    (insn_gen_fn) gen_movstrsi,
    &operand_data[2244],
    4,
    0,
    0,
    0
  },
  {
    "movstrdi",
    0,
    (insn_gen_fn) gen_movstrdi,
    &operand_data[2248],
    4,
    0,
    0,
    0
  },
  {
    "strmovdi_rex64",
    0,
    (insn_gen_fn) gen_strmovdi_rex64,
    &operand_data[1991],
    2,
    6,
    0,
    0
  },
  {
    "strmovsi",
    0,
    (insn_gen_fn) gen_strmovsi,
    &operand_data[1828],
    2,
    6,
    0,
    0
  },
  {
    "strmovsi_rex64",
    0,
    (insn_gen_fn) gen_strmovsi_rex64,
    &operand_data[1991],
    2,
    6,
    0,
    0
  },
  {
    "strmovhi",
    0,
    (insn_gen_fn) gen_strmovhi,
    &operand_data[1828],
    2,
    6,
    0,
    0
  },
  {
    "strmovhi_rex64",
    0,
    (insn_gen_fn) gen_strmovhi_rex64,
    &operand_data[1991],
    2,
    6,
    0,
    0
  },
  {
    "strmovqi",
    0,
    (insn_gen_fn) gen_strmovqi,
    &operand_data[1828],
    2,
    6,
    0,
    0
  },
  {
    "strmovqi_rex64",
    0,
    (insn_gen_fn) gen_strmovqi_rex64,
    &operand_data[1991],
    2,
    6,
    0,
    0
  },
  {
    "clrstrsi",
    0,
    (insn_gen_fn) gen_clrstrsi,
    &operand_data[2252],
    3,
    0,
    0,
    0
  },
  {
    "clrstrdi",
    0,
    (insn_gen_fn) gen_clrstrdi,
    &operand_data[2255],
    3,
    0,
    0,
    0
  },
  {
    "strsetdi_rex64",
    0,
    (insn_gen_fn) gen_strsetdi_rex64,
    &operand_data[1991],
    2,
    2,
    0,
    0
  },
  {
    "strsetsi",
    0,
    (insn_gen_fn) gen_strsetsi,
    &operand_data[1828],
    2,
    2,
    0,
    0
  },
  {
    "strsetsi_rex64",
    0,
    (insn_gen_fn) gen_strsetsi_rex64,
    &operand_data[1820],
    2,
    2,
    0,
    0
  },
  {
    "strsethi",
    0,
    (insn_gen_fn) gen_strsethi,
    &operand_data[1809],
    2,
    2,
    0,
    0
  },
  {
    "strsethi_rex64",
    0,
    (insn_gen_fn) gen_strsethi_rex64,
    &operand_data[2258],
    2,
    2,
    0,
    0
  },
  {
    "strsetqi",
    0,
    (insn_gen_fn) gen_strsetqi,
    &operand_data[1816],
    2,
    2,
    0,
    0
  },
  {
    "strsetqi_rex64",
    0,
    (insn_gen_fn) gen_strsetqi_rex64,
    &operand_data[2260],
    2,
    2,
    0,
    0
  },
  {
    "cmpstrsi",
    0,
    (insn_gen_fn) gen_cmpstrsi,
    &operand_data[2262],
    5,
    0,
    0,
    0
  },
  {
    "cmpintqi",
    0,
    (insn_gen_fn) gen_cmpintqi,
    &operand_data[1813],
    1,
    4,
    0,
    0
  },
  {
    "strlensi",
    0,
    (insn_gen_fn) gen_strlensi,
    &operand_data[2267],
    4,
    0,
    0,
    0
  },
  {
    "strlendi",
    0,
    (insn_gen_fn) gen_strlendi,
    &operand_data[2271],
    4,
    0,
    0,
    0
  },
  {
    "strlendi+1",
    0,
    0,
    &operand_data[2275],
    9,
    0,
    0,
    0
  },
  {
    "movdicc-1",
    0,
    0,
    &operand_data[2275],
    9,
    0,
    0,
    0
  },
  {
    "movdicc",
    0,
    (insn_gen_fn) gen_movdicc,
    &operand_data[2284],
    4,
    0,
    0,
    0
  },
  {
    "movsicc",
    0,
    (insn_gen_fn) gen_movsicc,
    &operand_data[2288],
    4,
    0,
    0,
    0
  },
  {
    "movhicc",
    0,
    (insn_gen_fn) gen_movhicc,
    &operand_data[2292],
    4,
    0,
    0,
    0
  },
  {
    "movsfcc",
    0,
    (insn_gen_fn) gen_movsfcc,
    &operand_data[2296],
    4,
    0,
    0,
    0
  },
  {
    "movdfcc",
    0,
    (insn_gen_fn) gen_movdfcc,
    &operand_data[2300],
    4,
    0,
    0,
    0
  },
  {
    "movdfcc+1",
    0,
    0,
    &operand_data[2304],
    5,
    0,
    0,
    0
  },
  {
    "movxfcc",
    0,
    (insn_gen_fn) gen_movxfcc,
    &operand_data[2309],
    4,
    0,
    0,
    0
  },
  {
    "movtfcc",
    0,
    (insn_gen_fn) gen_movtfcc,
    &operand_data[2313],
    4,
    0,
    0,
    0
  },
  {
    "minsf3",
    0,
    (insn_gen_fn) gen_minsf3,
    &operand_data[2015],
    3,
    2,
    0,
    0
  },
  {
    "minsf3+1",
    0,
    0,
    &operand_data[2317],
    5,
    0,
    0,
    0
  },
  {
    "mindf3-1",
    0,
    0,
    &operand_data[2322],
    5,
    0,
    0,
    0
  },
  {
    "mindf3",
    0,
    (insn_gen_fn) gen_mindf3,
    &operand_data[2012],
    3,
    2,
    0,
    0
  },
  {
    "mindf3+1",
    0,
    0,
    &operand_data[2327],
    5,
    0,
    0,
    0
  },
  {
    "maxsf3-1",
    0,
    0,
    &operand_data[2332],
    5,
    0,
    0,
    0
  },
  {
    "maxsf3",
    0,
    (insn_gen_fn) gen_maxsf3,
    &operand_data[2015],
    3,
    2,
    0,
    0
  },
  {
    "maxsf3+1",
    0,
    0,
    &operand_data[2317],
    5,
    0,
    0,
    0
  },
  {
    "maxdf3-1",
    0,
    0,
    &operand_data[2322],
    5,
    0,
    0,
    0
  },
  {
    "maxdf3",
    0,
    (insn_gen_fn) gen_maxdf3,
    &operand_data[2012],
    3,
    2,
    0,
    0
  },
  {
    "maxdf3+1",
    0,
    0,
    &operand_data[2327],
    5,
    0,
    0,
    0
  },
  {
    "pro_epilogue_adjust_stack-1",
    0,
    0,
    &operand_data[2332],
    5,
    0,
    0,
    0
  },
  {
    "pro_epilogue_adjust_stack",
    0,
    (insn_gen_fn) gen_pro_epilogue_adjust_stack,
    &operand_data[1202],
    3,
    0,
    2,
    0
  },
  {
    "pro_epilogue_adjust_stack+1",
    0,
    0,
    &operand_data[2337],
    7,
    0,
    0,
    0
  },
  {
    "pro_epilogue_adjust_stack+2",
    0,
    0,
    &operand_data[2344],
    7,
    0,
    0,
    0
  },
  {
    "allocate_stack_worker-1",
    0,
    0,
    &operand_data[2351],
    6,
    0,
    0,
    0
  },
  {
    "allocate_stack_worker",
    0,
    (insn_gen_fn) gen_allocate_stack_worker,
    &operand_data[1807],
    1,
    0,
    0,
    0
  },
  {
    "allocate_stack",
    0,
    (insn_gen_fn) gen_allocate_stack,
    &operand_data[2357],
    2,
    1,
    1,
    0
  },
  {
    "builtin_setjmp_receiver",
    0,
    (insn_gen_fn) gen_builtin_setjmp_receiver,
    &operand_data[856],
    1,
    0,
    0,
    0
  },
  {
    "builtin_setjmp_receiver+1",
    0,
    0,
    &operand_data[2359],
    4,
    0,
    0,
    0
  },
  {
    "builtin_setjmp_receiver+2",
    0,
    0,
    &operand_data[2363],
    3,
    0,
    0,
    0
  },
  {
    "builtin_setjmp_receiver+3",
    0,
    0,
    &operand_data[2366],
    2,
    0,
    0,
    0
  },
  {
    "builtin_setjmp_receiver+4",
    0,
    0,
    &operand_data[1994],
    2,
    0,
    0,
    0
  },
  {
    "builtin_setjmp_receiver+5",
    0,
    0,
    &operand_data[1994],
    2,
    0,
    0,
    0
  },
  {
    "builtin_setjmp_receiver+6",
    0,
    0,
    &operand_data[2368],
    4,
    0,
    0,
    0
  },
  {
    "builtin_setjmp_receiver+7",
    0,
    0,
    &operand_data[2372],
    3,
    0,
    0,
    0
  },
  {
    "builtin_setjmp_receiver+8",
    0,
    0,
    &operand_data[2375],
    3,
    0,
    0,
    0
  },
  {
    "builtin_setjmp_receiver+9",
    0,
    0,
    &operand_data[2378],
    3,
    0,
    0,
    0
  },
  {
    "builtin_setjmp_receiver+10",
    0,
    0,
    &operand_data[2381],
    3,
    0,
    0,
    0
  },
  {
    "builtin_setjmp_receiver+11",
    0,
    0,
    &operand_data[2384],
    3,
    0,
    0,
    0
  },
  {
    "builtin_setjmp_receiver+12",
    0,
    0,
    &operand_data[2373],
    2,
    0,
    0,
    0
  },
  {
    "builtin_setjmp_receiver+13",
    0,
    0,
    &operand_data[2382],
    2,
    0,
    0,
    0
  },
  {
    "builtin_setjmp_receiver+14",
    0,
    0,
    &operand_data[2385],
    2,
    0,
    0,
    0
  },
  {
    "builtin_setjmp_receiver+15",
    0,
    0,
    &operand_data[2387],
    3,
    0,
    0,
    0
  },
  {
    "builtin_setjmp_receiver+16",
    0,
    0,
    &operand_data[2390],
    3,
    0,
    0,
    0
  },
  {
    "builtin_setjmp_receiver+17",
    0,
    0,
    &operand_data[2393],
    3,
    0,
    0,
    0
  },
  {
    "builtin_setjmp_receiver+18",
    0,
    0,
    &operand_data[2396],
    4,
    0,
    0,
    0
  },
  {
    "builtin_setjmp_receiver+19",
    0,
    0,
    &operand_data[1962],
    2,
    0,
    0,
    0
  },
  {
    "builtin_setjmp_receiver+20",
    0,
    0,
    &operand_data[2000],
    2,
    0,
    0,
    0
  },
  {
    "builtin_setjmp_receiver+21",
    0,
    0,
    &operand_data[2003],
    2,
    0,
    0,
    0
  },
  {
    "builtin_setjmp_receiver+22",
    0,
    0,
    &operand_data[1971],
    2,
    0,
    0,
    0
  },
  {
    "builtin_setjmp_receiver+23",
    0,
    0,
    &operand_data[2400],
    2,
    0,
    0,
    0
  },
  {
    "builtin_setjmp_receiver+24",
    0,
    0,
    &operand_data[2066],
    2,
    0,
    0,
    0
  },
  {
    "builtin_setjmp_receiver+25",
    0,
    0,
    &operand_data[2402],
    4,
    0,
    0,
    0
  },
  {
    "builtin_setjmp_receiver+26",
    0,
    0,
    &operand_data[2402],
    4,
    0,
    0,
    0
  },
  {
    "builtin_setjmp_receiver+27",
    0,
    0,
    &operand_data[2406],
    4,
    0,
    0,
    0
  },
  {
    "builtin_setjmp_receiver+28",
    0,
    0,
    &operand_data[2406],
    4,
    0,
    0,
    0
  },
  {
    "builtin_setjmp_receiver+29",
    0,
    0,
    &operand_data[1805],
    1,
    0,
    0,
    0
  },
  {
    "builtin_setjmp_receiver+30",
    0,
    0,
    &operand_data[2410],
    1,
    0,
    0,
    0
  },
  {
    "builtin_setjmp_receiver+31",
    0,
    0,
    &operand_data[1805],
    1,
    0,
    0,
    0
  },
  {
    "conditional_trap-31",
    0,
    0,
    &operand_data[1998],
    2,
    0,
    0,
    0
  },
  {
    "conditional_trap-30",
    0,
    0,
    &operand_data[2411],
    3,
    0,
    0,
    0
  },
  {
    "conditional_trap-29",
    0,
    0,
    &operand_data[2019],
    2,
    0,
    0,
    0
  },
  {
    "conditional_trap-28",
    0,
    0,
    &operand_data[2414],
    2,
    0,
    0,
    0
  },
  {
    "conditional_trap-27",
    0,
    0,
    &operand_data[2416],
    2,
    0,
    0,
    0
  },
  {
    "conditional_trap-26",
    0,
    0,
    &operand_data[2418],
    3,
    0,
    0,
    0
  },
  {
    "conditional_trap-25",
    0,
    0,
    &operand_data[2374],
    1,
    0,
    0,
    0
  },
  {
    "conditional_trap-24",
    0,
    0,
    &operand_data[2374],
    1,
    0,
    0,
    0
  },
  {
    "conditional_trap-23",
    0,
    0,
    &operand_data[2374],
    1,
    0,
    0,
    0
  },
  {
    "conditional_trap-22",
    0,
    0,
    &operand_data[2374],
    1,
    0,
    0,
    0
  },
  {
    "conditional_trap-21",
    0,
    0,
    &operand_data[2374],
    1,
    0,
    0,
    0
  },
  {
    "conditional_trap-20",
    0,
    0,
    &operand_data[2421],
    2,
    0,
    0,
    0
  },
  {
    "conditional_trap-19",
    0,
    0,
    &operand_data[2374],
    1,
    0,
    0,
    0
  },
  {
    "conditional_trap-18",
    0,
    0,
    &operand_data[2374],
    1,
    0,
    0,
    0
  },
  {
    "conditional_trap-17",
    0,
    0,
    &operand_data[2421],
    2,
    0,
    0,
    0
  },
  {
    "conditional_trap-16",
    0,
    0,
    &operand_data[2374],
    1,
    0,
    0,
    0
  },
  {
    "conditional_trap-15",
    0,
    0,
    &operand_data[2423],
    2,
    0,
    0,
    0
  },
  {
    "conditional_trap-14",
    0,
    0,
    &operand_data[2425],
    2,
    0,
    0,
    0
  },
  {
    "conditional_trap-13",
    0,
    0,
    &operand_data[2427],
    2,
    0,
    0,
    0
  },
  {
    "conditional_trap-12",
    0,
    0,
    &operand_data[1807],
    1,
    0,
    0,
    0
  },
  {
    "conditional_trap-11",
    0,
    0,
    &operand_data[1810],
    1,
    0,
    0,
    0
  },
  {
    "conditional_trap-10",
    0,
    0,
    &operand_data[1775],
    1,
    0,
    0,
    0
  },
  {
    "conditional_trap-9",
    0,
    0,
    &operand_data[1775],
    1,
    0,
    0,
    0
  },
  {
    "conditional_trap-8",
    0,
    0,
    &operand_data[1775],
    1,
    0,
    0,
    0
  },
  {
    "conditional_trap-7",
    0,
    0,
    &operand_data[1775],
    1,
    0,
    0,
    0
  },
  {
    "conditional_trap-6",
    0,
    0,
    &operand_data[1775],
    1,
    0,
    0,
    0
  },
  {
    "conditional_trap-5",
    0,
    0,
    &operand_data[2429],
    2,
    0,
    0,
    0
  },
  {
    "conditional_trap-4",
    0,
    0,
    &operand_data[1775],
    1,
    0,
    0,
    0
  },
  {
    "conditional_trap-3",
    0,
    0,
    &operand_data[1775],
    1,
    0,
    0,
    0
  },
  {
    "conditional_trap-2",
    0,
    0,
    &operand_data[2429],
    2,
    0,
    0,
    0
  },
  {
    "conditional_trap-1",
    0,
    0,
    &operand_data[1775],
    1,
    0,
    0,
    0
  },
  {
    "conditional_trap",
    0,
    (insn_gen_fn) gen_conditional_trap,
    &operand_data[1296],
    2,
    1,
    0,
    0
  },
  {
    "movti",
    0,
    (insn_gen_fn) gen_movti,
    &operand_data[2431],
    2,
    0,
    0,
    0
  },
  {
    "movv2df",
    0,
    (insn_gen_fn) gen_movv2df,
    &operand_data[2433],
    2,
    0,
    0,
    0
  },
  {
    "movv8hi",
    0,
    (insn_gen_fn) gen_movv8hi,
    &operand_data[2435],
    2,
    0,
    0,
    0
  },
  {
    "movv16qi",
    0,
    (insn_gen_fn) gen_movv16qi,
    &operand_data[2437],
    2,
    0,
    0,
    0
  },
  {
    "movv4sf",
    0,
    (insn_gen_fn) gen_movv4sf,
    &operand_data[2439],
    2,
    0,
    0,
    0
  },
  {
    "movv4si",
    0,
    (insn_gen_fn) gen_movv4si,
    &operand_data[2441],
    2,
    0,
    0,
    0
  },
  {
    "movv2di",
    0,
    (insn_gen_fn) gen_movv2di,
    &operand_data[2443],
    2,
    0,
    0,
    0
  },
  {
    "movv2si",
    0,
    (insn_gen_fn) gen_movv2si,
    &operand_data[2445],
    2,
    0,
    0,
    0
  },
  {
    "movv4hi",
    0,
    (insn_gen_fn) gen_movv4hi,
    &operand_data[2447],
    2,
    0,
    0,
    0
  },
  {
    "movv8qi",
    0,
    (insn_gen_fn) gen_movv8qi,
    &operand_data[2449],
    2,
    0,
    0,
    0
  },
  {
    "movv2sf",
    0,
    (insn_gen_fn) gen_movv2sf,
    &operand_data[2451],
    2,
    0,
    0,
    0
  },
  {
    "movv2sf+1",
    0,
    0,
    &operand_data[2453],
    2,
    0,
    0,
    0
  },
  {
    "movv2sf+2",
    0,
    0,
    &operand_data[2453],
    2,
    0,
    0,
    0
  },
  {
    "movv2sf+3",
    0,
    0,
    &operand_data[2455],
    2,
    0,
    0,
    0
  },
  {
    "movv2sf+4",
    0,
    0,
    &operand_data[2457],
    2,
    0,
    0,
    0
  },
  {
    "movv2sf+5",
    0,
    0,
    &operand_data[2459],
    2,
    0,
    0,
    0
  },
  {
    "movv2sf+6",
    0,
    0,
    &operand_data[2461],
    2,
    0,
    0,
    0
  },
  {
    "movv2sf+7",
    0,
    0,
    &operand_data[2463],
    2,
    0,
    0,
    0
  },
  {
    "sse_movaps-7",
    0,
    0,
    &operand_data[2465],
    2,
    0,
    0,
    0
  },
  {
    "sse_movaps-6",
    0,
    0,
    &operand_data[2467],
    2,
    0,
    0,
    0
  },
  {
    "sse_movaps-5",
    0,
    0,
    &operand_data[2469],
    2,
    0,
    0,
    0
  },
  {
    "sse_movaps-4",
    0,
    0,
    &operand_data[2471],
    2,
    0,
    0,
    0
  },
  {
    "sse_movaps-3",
    0,
    0,
    &operand_data[2473],
    2,
    0,
    0,
    0
  },
  {
    "sse_movaps-2",
    0,
    0,
    &operand_data[2475],
    2,
    0,
    0,
    0
  },
  {
    "sse_movaps-1",
    0,
    0,
    &operand_data[2477],
    2,
    0,
    0,
    0
  },
  {
    "sse_movaps",
    0,
    (insn_gen_fn) gen_sse_movaps,
    &operand_data[2439],
    2,
    0,
    0,
    0
  },
  {
    "sse_movups",
    0,
    (insn_gen_fn) gen_sse_movups,
    &operand_data[2439],
    2,
    0,
    0,
    0
  },
  {
    "sse_loadss",
    0,
    (insn_gen_fn) gen_sse_loadss,
    &operand_data[2479],
    2,
    0,
    0,
    0
  },
  {
    "sse_andv4sf3",
    0,
    (insn_gen_fn) gen_sse_andv4sf3,
    &operand_data[2481],
    3,
    0,
    0,
    0
  },
  {
    "sse_nandv4sf3",
    0,
    (insn_gen_fn) gen_sse_nandv4sf3,
    &operand_data[2481],
    3,
    0,
    0,
    0
  },
  {
    "sse_iorv4sf3",
    0,
    (insn_gen_fn) gen_sse_iorv4sf3,
    &operand_data[2481],
    3,
    0,
    0,
    0
  },
  {
    "sse_xorv4sf3",
    0,
    (insn_gen_fn) gen_sse_xorv4sf3,
    &operand_data[2481],
    3,
    0,
    0,
    0
  },
  {
    "sse2_andv2df3",
    0,
    (insn_gen_fn) gen_sse2_andv2df3,
    &operand_data[2484],
    3,
    0,
    0,
    0
  },
  {
    "sse2_nandv2df3",
    0,
    (insn_gen_fn) gen_sse2_nandv2df3,
    &operand_data[2484],
    3,
    0,
    0,
    0
  },
  {
    "sse2_iorv2df3",
    0,
    (insn_gen_fn) gen_sse2_iorv2df3,
    &operand_data[2484],
    3,
    0,
    0,
    0
  },
  {
    "sse2_xorv2df3",
    0,
    (insn_gen_fn) gen_sse2_xorv2df3,
    &operand_data[2485],
    3,
    0,
    0,
    0
  },
  {
    "sfence",
    0,
    (insn_gen_fn) gen_sfence,
    &operand_data[0],
    0,
    2,
    0,
    0
  },
  {
    "sse_prologue_save",
    0,
    (insn_gen_fn) gen_sse_prologue_save,
    &operand_data[2488],
    4,
    0,
    0,
    0
  },
  {
    "prefetch",
    0,
    (insn_gen_fn) gen_prefetch,
    &operand_data[2492],
    3,
    0,
    0,
    0
  },
  {
    "sse2_loadsd",
    0,
    (insn_gen_fn) gen_sse2_loadsd,
    &operand_data[2495],
    2,
    0,
    0,
    0
  },
  {
    "sse2_mfence",
    0,
    (insn_gen_fn) gen_sse2_mfence,
    &operand_data[0],
    0,
    2,
    0,
    0
  },
  {
    "sse2_lfence",
    0,
    (insn_gen_fn) gen_sse2_lfence,
    &operand_data[0],
    0,
    2,
    0,
    0
  },
};


const char *
get_insn_name (code)
     int code;
{
  return insn_data[code].name;
}
