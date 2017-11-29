/*
 * Copyright 2003, 2004 PathScale, Inc.  All Rights Reserved.
 */

/* Generated automatically by the program `genemit'
from the machine description file `md'.  */

#include "config.h"
#include "system.h"
#include "rtl.h"
#include "tm_p.h"
#include "function.h"
#include "expr.h"
#include "optabs.h"
#include "real.h"
#include "flags.h"
#include "output.h"
#include "insn-config.h"
#include "hard-reg-set.h"
#include "recog.h"
#include "resource.h"
#include "reload.h"
#include "toplev.h"
#include "ggc.h"

#define FAIL return (end_sequence (), _val)
#define DONE return (_val = get_insns (), end_sequence (), _val)

/* ../../gcc/gcc/config/i386/i386.md:444 */
rtx
gen_cmpdi_ccno_1_rex64 (operand0, operand1)
     rtx operand0;
     rtx operand1;
{
  return gen_rtx_SET (VOIDmode,
	gen_rtx_REG (VOIDmode,
	17),
	gen_rtx_COMPARE (VOIDmode,
	operand0,
	operand1));
}

/* ../../gcc/gcc/config/i386/i386.md:473 */
rtx
gen_cmpdi_1_insn_rex64 (operand0, operand1)
     rtx operand0;
     rtx operand1;
{
  return gen_rtx_SET (VOIDmode,
	gen_rtx_REG (VOIDmode,
	17),
	gen_rtx_COMPARE (VOIDmode,
	operand0,
	operand1));
}

/* ../../gcc/gcc/config/i386/i386.md:641 */
rtx
gen_cmpqi_ext_3_insn (operand0, operand1)
     rtx operand0;
     rtx operand1;
{
  return gen_rtx_SET (VOIDmode,
	gen_rtx_REG (VOIDmode,
	17),
	gen_rtx_COMPARE (VOIDmode,
	gen_rtx_SUBREG (QImode,
	gen_rtx_ZERO_EXTRACT (SImode,
	operand0,
	GEN_INT (8LL),
	GEN_INT (8LL)),
	0),
	operand1));
}

/* ../../gcc/gcc/config/i386/i386.md:655 */
rtx
gen_cmpqi_ext_3_insn_rex64 (operand0, operand1)
     rtx operand0;
     rtx operand1;
{
  return gen_rtx_SET (VOIDmode,
	gen_rtx_REG (VOIDmode,
	17),
	gen_rtx_COMPARE (VOIDmode,
	gen_rtx_SUBREG (QImode,
	gen_rtx_ZERO_EXTRACT (SImode,
	operand0,
	GEN_INT (8LL),
	GEN_INT (8LL)),
	0),
	operand1));
}

/* ../../gcc/gcc/config/i386/i386.md:927 */
rtx
gen_x86_fnstsw_1 (operand0)
     rtx operand0;
{
  return gen_rtx_SET (VOIDmode,
	operand0,
	gen_rtx_UNSPEC (HImode,
	gen_rtvec (1,
		gen_rtx_REG (VOIDmode,
	18)),
	24));
}

/* ../../gcc/gcc/config/i386/i386.md:940 */
rtx
gen_x86_sahf_1 (operand0)
     rtx operand0;
{
  return gen_rtx_SET (VOIDmode,
	gen_rtx_REG (CCmode,
	17),
	gen_rtx_UNSPEC (CCmode,
	gen_rtvec (1,
		operand0),
	25));
}

/* ../../gcc/gcc/config/i386/i386.md:1080 */
rtx
gen_popsi1 (operand0)
     rtx operand0;
{
  return gen_rtx_PARALLEL (VOIDmode, gen_rtvec (2,
		gen_rtx_SET (VOIDmode,
	operand0,
	gen_rtx_MEM (SImode,
	gen_rtx_REG (SImode,
	7))),
		gen_rtx_SET (VOIDmode,
	gen_rtx_REG (SImode,
	7),
	gen_rtx_PLUS (SImode,
	gen_rtx_REG (SImode,
	7),
	GEN_INT (4LL)))));
}

/* ../../gcc/gcc/config/i386/i386.md:1684 */
rtx
gen_movsi_insv_1 (operand0, operand1)
     rtx operand0;
     rtx operand1;
{
  return gen_rtx_SET (VOIDmode,
	gen_rtx_ZERO_EXTRACT (SImode,
	operand0,
	GEN_INT (8LL),
	GEN_INT (8LL)),
	operand1);
}

/* ../../gcc/gcc/config/i386/i386.md:1728 */
rtx
gen_pushdi2_rex64 (operand0, operand1)
     rtx operand0;
     rtx operand1;
{
  return gen_rtx_SET (VOIDmode,
	operand0,
	operand1);
}

/* ../../gcc/gcc/config/i386/i386.md:1801 */
rtx
gen_popdi1 (operand0)
     rtx operand0;
{
  return gen_rtx_PARALLEL (VOIDmode, gen_rtvec (2,
		gen_rtx_SET (VOIDmode,
	operand0,
	gen_rtx_MEM (DImode,
	gen_rtx_REG (DImode,
	7))),
		gen_rtx_SET (VOIDmode,
	gen_rtx_REG (DImode,
	7),
	gen_rtx_PLUS (DImode,
	gen_rtx_REG (DImode,
	7),
	GEN_INT (8LL)))));
}

/* ../../gcc/gcc/config/i386/i386.md:2819 */
rtx
gen_swapxf (operand0, operand1)
     rtx operand0;
     rtx operand1;
{
  return gen_rtx_PARALLEL (VOIDmode, gen_rtvec (2,
		gen_rtx_SET (VOIDmode,
	operand0,
	operand1),
		gen_rtx_SET (VOIDmode,
	operand1,
	operand0)));
}

/* ../../gcc/gcc/config/i386/i386.md:2834 */
rtx
gen_swaptf (operand0, operand1)
     rtx operand0;
     rtx operand1;
{
  return gen_rtx_PARALLEL (VOIDmode, gen_rtvec (2,
		gen_rtx_SET (VOIDmode,
	operand0,
	operand1),
		gen_rtx_SET (VOIDmode,
	operand1,
	operand0)));
}

/* ../../gcc/gcc/config/i386/i386.md:2864 */
rtx
gen_zero_extendhisi2_and (operand0, operand1)
     rtx operand0;
     rtx operand1;
{
  return gen_rtx_PARALLEL (VOIDmode, gen_rtvec (2,
		gen_rtx_SET (VOIDmode,
	operand0,
	gen_rtx_ZERO_EXTEND (SImode,
	operand1)),
		gen_rtx_CLOBBER (VOIDmode,
	gen_rtx_REG (CCmode,
	17))));
}

/* ../../gcc/gcc/config/i386/i386.md:3043 */
rtx
gen_zero_extendsidi2_32 (operand0, operand1)
     rtx operand0;
     rtx operand1;
{
  return gen_rtx_PARALLEL (VOIDmode, gen_rtvec (2,
		gen_rtx_SET (VOIDmode,
	operand0,
	gen_rtx_ZERO_EXTEND (DImode,
	operand1)),
		gen_rtx_CLOBBER (VOIDmode,
	gen_rtx_REG (CCmode,
	17))));
}

/* ../../gcc/gcc/config/i386/i386.md:3051 */
rtx
gen_zero_extendsidi2_rex64 (operand0, operand1)
     rtx operand0;
     rtx operand1;
{
  return gen_rtx_SET (VOIDmode,
	operand0,
	gen_rtx_ZERO_EXTEND (DImode,
	operand1));
}

/* ../../gcc/gcc/config/i386/i386.md:3086 */
rtx
gen_zero_extendhidi2 (operand0, operand1)
     rtx operand0;
     rtx operand1;
{
  return gen_rtx_SET (VOIDmode,
	operand0,
	gen_rtx_ZERO_EXTEND (DImode,
	operand1));
}

/* ../../gcc/gcc/config/i386/i386.md:3096 */
rtx
gen_zero_extendqidi2 (operand0, operand1)
     rtx operand0;
     rtx operand1;
{
  return gen_rtx_SET (VOIDmode,
	operand0,
	gen_rtx_ZERO_EXTEND (DImode,
	operand1));
}

/* ../../gcc/gcc/config/i386/i386.md:3130 */
rtx
gen_extendsidi2_rex64 (operand0, operand1)
     rtx operand0;
     rtx operand1;
{
  return gen_rtx_SET (VOIDmode,
	operand0,
	gen_rtx_SIGN_EXTEND (DImode,
	operand1));
}

/* ../../gcc/gcc/config/i386/i386.md:3142 */
rtx
gen_extendhidi2 (operand0, operand1)
     rtx operand0;
     rtx operand1;
{
  return gen_rtx_SET (VOIDmode,
	operand0,
	gen_rtx_SIGN_EXTEND (DImode,
	operand1));
}

/* ../../gcc/gcc/config/i386/i386.md:3150 */
rtx
gen_extendqidi2 (operand0, operand1)
     rtx operand0;
     rtx operand1;
{
  return gen_rtx_SET (VOIDmode,
	operand0,
	gen_rtx_SIGN_EXTEND (DImode,
	operand1));
}

/* ../../gcc/gcc/config/i386/i386.md:3232 */
rtx
gen_extendhisi2 (operand0, operand1)
     rtx operand0;
     rtx operand1;
{
  return gen_rtx_SET (VOIDmode,
	operand0,
	gen_rtx_SIGN_EXTEND (SImode,
	operand1));
}

/* ../../gcc/gcc/config/i386/i386.md:3285 */
rtx
gen_extendqihi2 (operand0, operand1)
     rtx operand0;
     rtx operand1;
{
  return gen_rtx_SET (VOIDmode,
	operand0,
	gen_rtx_SIGN_EXTEND (HImode,
	operand1));
}

/* ../../gcc/gcc/config/i386/i386.md:3311 */
rtx
gen_extendqisi2 (operand0, operand1)
     rtx operand0;
     rtx operand1;
{
  return gen_rtx_SET (VOIDmode,
	operand0,
	gen_rtx_SIGN_EXTEND (SImode,
	operand1));
}

/* ../../gcc/gcc/config/i386/i386.md:3744 */
rtx
gen_truncdfsf2_3 (operand0, operand1)
     rtx operand0;
     rtx operand1;
{
  return gen_rtx_SET (VOIDmode,
	operand0,
	gen_rtx_FLOAT_TRUNCATE (SFmode,
	operand1));
}

/* ../../gcc/gcc/config/i386/i386.md:3758 */
rtx
gen_truncdfsf2_sse_only (operand0, operand1)
     rtx operand0;
     rtx operand1;
{
  return gen_rtx_SET (VOIDmode,
	operand0,
	gen_rtx_FLOAT_TRUNCATE (SFmode,
	operand1));
}

/* ../../gcc/gcc/config/i386/i386.md:4122 */
rtx
gen_fix_truncdi_nomemory (operand0, operand1, operand2, operand3, operand4)
     rtx operand0;
     rtx operand1;
     rtx operand2;
     rtx operand3;
     rtx operand4;
{
  return gen_rtx_PARALLEL (VOIDmode, gen_rtvec (5,
		gen_rtx_SET (VOIDmode,
	operand0,
	gen_rtx_FIX (DImode,
	operand1)),
		gen_rtx_USE (VOIDmode,
	operand2),
		gen_rtx_USE (VOIDmode,
	operand3),
		gen_rtx_CLOBBER (VOIDmode,
	operand4),
		gen_rtx_CLOBBER (VOIDmode,
	gen_rtx_SCRATCH (DFmode))));
}

/* ../../gcc/gcc/config/i386/i386.md:4134 */
rtx
gen_fix_truncdi_memory (operand0, operand1, operand2, operand3)
     rtx operand0;
     rtx operand1;
     rtx operand2;
     rtx operand3;
{
  return gen_rtx_PARALLEL (VOIDmode, gen_rtvec (4,
		gen_rtx_SET (VOIDmode,
	operand0,
	gen_rtx_FIX (DImode,
	operand1)),
		gen_rtx_USE (VOIDmode,
	operand2),
		gen_rtx_USE (VOIDmode,
	operand3),
		gen_rtx_CLOBBER (VOIDmode,
	gen_rtx_SCRATCH (DFmode))));
}

/* ../../gcc/gcc/config/i386/i386.md:4175 */
rtx
gen_fix_truncsfdi_sse (operand0, operand1)
     rtx operand0;
     rtx operand1;
{
  return gen_rtx_SET (VOIDmode,
	operand0,
	gen_rtx_FIX (DImode,
	operand1));
}

/* ../../gcc/gcc/config/i386/i386.md:4182 */
rtx
gen_fix_truncdfdi_sse (operand0, operand1)
     rtx operand0;
     rtx operand1;
{
  return gen_rtx_SET (VOIDmode,
	operand0,
	gen_rtx_FIX (DImode,
	operand1));
}

/* ../../gcc/gcc/config/i386/i386.md:4261 */
rtx
gen_fix_truncsi_nomemory (operand0, operand1, operand2, operand3, operand4)
     rtx operand0;
     rtx operand1;
     rtx operand2;
     rtx operand3;
     rtx operand4;
{
  return gen_rtx_PARALLEL (VOIDmode, gen_rtvec (4,
		gen_rtx_SET (VOIDmode,
	operand0,
	gen_rtx_FIX (SImode,
	operand1)),
		gen_rtx_USE (VOIDmode,
	operand2),
		gen_rtx_USE (VOIDmode,
	operand3),
		gen_rtx_CLOBBER (VOIDmode,
	operand4)));
}

/* ../../gcc/gcc/config/i386/i386.md:4272 */
rtx
gen_fix_truncsi_memory (operand0, operand1, operand2, operand3)
     rtx operand0;
     rtx operand1;
     rtx operand2;
     rtx operand3;
{
  return gen_rtx_PARALLEL (VOIDmode, gen_rtvec (3,
		gen_rtx_SET (VOIDmode,
	operand0,
	gen_rtx_FIX (SImode,
	operand1)),
		gen_rtx_USE (VOIDmode,
	operand2),
		gen_rtx_USE (VOIDmode,
	operand3)));
}

/* ../../gcc/gcc/config/i386/i386.md:4283 */
rtx
gen_fix_truncsfsi_sse (operand0, operand1)
     rtx operand0;
     rtx operand1;
{
  return gen_rtx_SET (VOIDmode,
	operand0,
	gen_rtx_FIX (SImode,
	operand1));
}

/* ../../gcc/gcc/config/i386/i386.md:4290 */
rtx
gen_fix_truncdfsi_sse (operand0, operand1)
     rtx operand0;
     rtx operand1;
{
  return gen_rtx_SET (VOIDmode,
	operand0,
	gen_rtx_FIX (SImode,
	operand1));
}

/* ../../gcc/gcc/config/i386/i386.md:4376 */
rtx
gen_fix_trunchi_nomemory (operand0, operand1, operand2, operand3, operand4)
     rtx operand0;
     rtx operand1;
     rtx operand2;
     rtx operand3;
     rtx operand4;
{
  return gen_rtx_PARALLEL (VOIDmode, gen_rtvec (4,
		gen_rtx_SET (VOIDmode,
	operand0,
	gen_rtx_FIX (HImode,
	operand1)),
		gen_rtx_USE (VOIDmode,
	operand2),
		gen_rtx_USE (VOIDmode,
	operand3),
		gen_rtx_CLOBBER (VOIDmode,
	operand4)));
}

/* ../../gcc/gcc/config/i386/i386.md:4387 */
rtx
gen_fix_trunchi_memory (operand0, operand1, operand2, operand3)
     rtx operand0;
     rtx operand1;
     rtx operand2;
     rtx operand3;
{
  return gen_rtx_PARALLEL (VOIDmode, gen_rtvec (3,
		gen_rtx_SET (VOIDmode,
	operand0,
	gen_rtx_FIX (HImode,
	operand1)),
		gen_rtx_USE (VOIDmode,
	operand2),
		gen_rtx_USE (VOIDmode,
	operand3)));
}

/* ../../gcc/gcc/config/i386/i386.md:4424 */
rtx
gen_x86_fnstcw_1 (operand0)
     rtx operand0;
{
  return gen_rtx_SET (VOIDmode,
	operand0,
	gen_rtx_UNSPEC (HImode,
	gen_rtvec (1,
		gen_rtx_REG (HImode,
	18)),
	26));
}

/* ../../gcc/gcc/config/i386/i386.md:4434 */
rtx
gen_x86_fldcw_1 (operand0)
     rtx operand0;
{
  return gen_rtx_SET (VOIDmode,
	gen_rtx_REG (HImode,
	18),
	gen_rtx_UNSPEC (HImode,
	gen_rtvec (1,
		operand0),
	28));
}

/* ../../gcc/gcc/config/i386/i386.md:4450 */
rtx
gen_floathisf2 (operand0, operand1)
     rtx operand0;
     rtx operand1;
{
  return gen_rtx_SET (VOIDmode,
	operand0,
	gen_rtx_FLOAT (SFmode,
	operand1));
}

/* ../../gcc/gcc/config/i386/i386.md:4526 */
rtx
gen_floathidf2 (operand0, operand1)
     rtx operand0;
     rtx operand1;
{
  return gen_rtx_SET (VOIDmode,
	operand0,
	gen_rtx_FLOAT (DFmode,
	operand1));
}

/* ../../gcc/gcc/config/i386/i386.md:4602 */
rtx
gen_floathixf2 (operand0, operand1)
     rtx operand0;
     rtx operand1;
{
  return gen_rtx_SET (VOIDmode,
	operand0,
	gen_rtx_FLOAT (XFmode,
	operand1));
}

/* ../../gcc/gcc/config/i386/i386.md:4613 */
rtx
gen_floathitf2 (operand0, operand1)
     rtx operand0;
     rtx operand1;
{
  return gen_rtx_SET (VOIDmode,
	operand0,
	gen_rtx_FLOAT (TFmode,
	operand1));
}

/* ../../gcc/gcc/config/i386/i386.md:4624 */
rtx
gen_floatsixf2 (operand0, operand1)
     rtx operand0;
     rtx operand1;
{
  return gen_rtx_SET (VOIDmode,
	operand0,
	gen_rtx_FLOAT (XFmode,
	operand1));
}

/* ../../gcc/gcc/config/i386/i386.md:4635 */
rtx
gen_floatsitf2 (operand0, operand1)
     rtx operand0;
     rtx operand1;
{
  return gen_rtx_SET (VOIDmode,
	operand0,
	gen_rtx_FLOAT (TFmode,
	operand1));
}

/* ../../gcc/gcc/config/i386/i386.md:4646 */
rtx
gen_floatdixf2 (operand0, operand1)
     rtx operand0;
     rtx operand1;
{
  return gen_rtx_SET (VOIDmode,
	operand0,
	gen_rtx_FLOAT (XFmode,
	operand1));
}

/* ../../gcc/gcc/config/i386/i386.md:4657 */
rtx
gen_floatditf2 (operand0, operand1)
     rtx operand0;
     rtx operand1;
{
  return gen_rtx_SET (VOIDmode,
	operand0,
	gen_rtx_FLOAT (TFmode,
	operand1));
}

/* ../../gcc/gcc/config/i386/i386.md:4787 */
rtx
gen_addqi3_cc (operand0, operand1, operand2)
     rtx operand0;
     rtx operand1;
     rtx operand2;
{
  return gen_rtx_PARALLEL (VOIDmode, gen_rtvec (2,
		gen_rtx_SET (VOIDmode,
	gen_rtx_REG (CCmode,
	17),
	gen_rtx_UNSPEC (CCmode,
	gen_rtvec (2,
		operand1,
		operand2),
	27)),
		gen_rtx_SET (VOIDmode,
	operand0,
	gen_rtx_PLUS (QImode,
	operand1,
	operand2))));
}

/* ../../gcc/gcc/config/i386/i386.md:5352 */
rtx
gen_addsi_1_zext (operand0, operand1, operand2)
     rtx operand0;
     rtx operand1;
     rtx operand2;
{
  return gen_rtx_PARALLEL (VOIDmode, gen_rtvec (2,
		gen_rtx_SET (VOIDmode,
	operand0,
	gen_rtx_ZERO_EXTEND (DImode,
	gen_rtx_PLUS (SImode,
	operand1,
	operand2))),
		gen_rtx_CLOBBER (VOIDmode,
	gen_rtx_REG (CCmode,
	17))));
}

/* ../../gcc/gcc/config/i386/i386.md:6223 */
rtx
gen_addqi_ext_1 (operand0, operand1, operand2)
     rtx operand0;
     rtx operand1;
     rtx operand2;
{
  return gen_rtx_PARALLEL (VOIDmode, gen_rtvec (2,
		gen_rtx_SET (VOIDmode,
	gen_rtx_ZERO_EXTRACT (SImode,
	operand0,
	GEN_INT (8LL),
	GEN_INT (8LL)),
	gen_rtx_PLUS (SImode,
	gen_rtx_ZERO_EXTRACT (SImode,
	operand1,
	GEN_INT (8LL),
	GEN_INT (8LL)),
	operand2)),
		gen_rtx_CLOBBER (VOIDmode,
	gen_rtx_REG (CCmode,
	17))));
}

/* ../../gcc/gcc/config/i386/i386.md:6377 */
rtx
gen_subdi3_carry_rex64 (operand0, operand1, operand2)
     rtx operand0;
     rtx operand1;
     rtx operand2;
{
  return gen_rtx_PARALLEL (VOIDmode, gen_rtvec (2,
		gen_rtx_SET (VOIDmode,
	operand0,
	gen_rtx_MINUS (DImode,
	operand1,
	gen_rtx_PLUS (DImode,
	gen_rtx_LTU (DImode,
	gen_rtx_REG (CCmode,
	17),
	const0_rtx),
	operand2))),
		gen_rtx_CLOBBER (VOIDmode,
	gen_rtx_REG (CCmode,
	17))));
}

/* ../../gcc/gcc/config/i386/i386.md:6427 */
rtx
gen_subsi3_carry (operand0, operand1, operand2)
     rtx operand0;
     rtx operand1;
     rtx operand2;
{
  return gen_rtx_PARALLEL (VOIDmode, gen_rtvec (2,
		gen_rtx_SET (VOIDmode,
	operand0,
	gen_rtx_MINUS (SImode,
	operand1,
	gen_rtx_PLUS (SImode,
	gen_rtx_LTU (SImode,
	gen_rtx_REG (CCmode,
	17),
	const0_rtx),
	operand2))),
		gen_rtx_CLOBBER (VOIDmode,
	gen_rtx_REG (CCmode,
	17))));
}

/* ../../gcc/gcc/config/i386/i386.md:6440 */
rtx
gen_subsi3_carry_zext (operand0, operand1, operand2)
     rtx operand0;
     rtx operand1;
     rtx operand2;
{
  return gen_rtx_PARALLEL (VOIDmode, gen_rtvec (2,
		gen_rtx_SET (VOIDmode,
	operand0,
	gen_rtx_ZERO_EXTEND (DImode,
	gen_rtx_MINUS (SImode,
	operand1,
	gen_rtx_PLUS (SImode,
	gen_rtx_LTU (SImode,
	gen_rtx_REG (CCmode,
	17),
	const0_rtx),
	operand2)))),
		gen_rtx_CLOBBER (VOIDmode,
	gen_rtx_REG (CCmode,
	17))));
}

/* ../../gcc/gcc/config/i386/i386.md:7129 */
rtx
gen_divqi3 (operand0, operand1, operand2)
     rtx operand0;
     rtx operand1;
     rtx operand2;
{
  return gen_rtx_PARALLEL (VOIDmode, gen_rtvec (2,
		gen_rtx_SET (VOIDmode,
	operand0,
	gen_rtx_DIV (QImode,
	operand1,
	operand2)),
		gen_rtx_CLOBBER (VOIDmode,
	gen_rtx_REG (CCmode,
	17))));
}

/* ../../gcc/gcc/config/i386/i386.md:7140 */
rtx
gen_udivqi3 (operand0, operand1, operand2)
     rtx operand0;
     rtx operand1;
     rtx operand2;
{
  return gen_rtx_PARALLEL (VOIDmode, gen_rtvec (2,
		gen_rtx_SET (VOIDmode,
	operand0,
	gen_rtx_UDIV (QImode,
	operand1,
	operand2)),
		gen_rtx_CLOBBER (VOIDmode,
	gen_rtx_REG (CCmode,
	17))));
}

/* ../../gcc/gcc/config/i386/i386.md:7352 */
rtx
gen_divmodhi4 (operand0, operand1, operand2, operand3)
     rtx operand0;
     rtx operand1;
     rtx operand2;
     rtx operand3;
{
  return gen_rtx_PARALLEL (VOIDmode, gen_rtvec (3,
		gen_rtx_SET (VOIDmode,
	operand0,
	gen_rtx_DIV (HImode,
	operand1,
	operand2)),
		gen_rtx_SET (VOIDmode,
	operand3,
	gen_rtx_MOD (HImode,
	operand1,
	operand2)),
		gen_rtx_CLOBBER (VOIDmode,
	gen_rtx_REG (CCmode,
	17))));
}

/* ../../gcc/gcc/config/i386/i386.md:7365 */
rtx
gen_udivmoddi4 (operand0, operand1, operand2, operand3)
     rtx operand0;
     rtx operand1;
     rtx operand2;
     rtx operand3;
{
  return gen_rtx_PARALLEL (VOIDmode, gen_rtvec (3,
		gen_rtx_SET (VOIDmode,
	operand0,
	gen_rtx_UDIV (DImode,
	operand1,
	operand2)),
		gen_rtx_SET (VOIDmode,
	operand3,
	gen_rtx_UMOD (DImode,
	operand1,
	operand2)),
		gen_rtx_CLOBBER (VOIDmode,
	gen_rtx_REG (CCmode,
	17))));
}

/* ../../gcc/gcc/config/i386/i386.md:7409 */
rtx
gen_udivmodsi4 (operand0, operand1, operand2, operand3)
     rtx operand0;
     rtx operand1;
     rtx operand2;
     rtx operand3;
{
  return gen_rtx_PARALLEL (VOIDmode, gen_rtvec (3,
		gen_rtx_SET (VOIDmode,
	operand0,
	gen_rtx_UDIV (SImode,
	operand1,
	operand2)),
		gen_rtx_SET (VOIDmode,
	operand3,
	gen_rtx_UMOD (SImode,
	operand1,
	operand2)),
		gen_rtx_CLOBBER (VOIDmode,
	gen_rtx_REG (CCmode,
	17))));
}

/* ../../gcc/gcc/config/i386/i386.md:7522 */
rtx
gen_testsi_1 (operand0, operand1)
     rtx operand0;
     rtx operand1;
{
  return gen_rtx_SET (VOIDmode,
	gen_rtx_REG (VOIDmode,
	17),
	gen_rtx_COMPARE (VOIDmode,
	gen_rtx_AND (SImode,
	operand0,
	operand1),
	const0_rtx));
}

/* ../../gcc/gcc/config/i386/i386.md:8113 */
rtx
gen_andqi_ext_0 (operand0, operand1, operand2)
     rtx operand0;
     rtx operand1;
     rtx operand2;
{
  return gen_rtx_PARALLEL (VOIDmode, gen_rtvec (2,
		gen_rtx_SET (VOIDmode,
	gen_rtx_ZERO_EXTRACT (SImode,
	operand0,
	GEN_INT (8LL),
	GEN_INT (8LL)),
	gen_rtx_AND (SImode,
	gen_rtx_ZERO_EXTRACT (SImode,
	operand1,
	GEN_INT (8LL),
	GEN_INT (8LL)),
	operand2)),
		gen_rtx_CLOBBER (VOIDmode,
	gen_rtx_REG (CCmode,
	17))));
}

/* ../../gcc/gcc/config/i386/i386.md:8519 */
rtx
gen_iorqi_ext_0 (operand0, operand1, operand2)
     rtx operand0;
     rtx operand1;
     rtx operand2;
{
  return gen_rtx_PARALLEL (VOIDmode, gen_rtvec (2,
		gen_rtx_SET (VOIDmode,
	gen_rtx_ZERO_EXTRACT (SImode,
	operand0,
	GEN_INT (8LL),
	GEN_INT (8LL)),
	gen_rtx_IOR (SImode,
	gen_rtx_ZERO_EXTRACT (SImode,
	operand1,
	GEN_INT (8LL),
	GEN_INT (8LL)),
	operand2)),
		gen_rtx_CLOBBER (VOIDmode,
	gen_rtx_REG (CCmode,
	17))));
}

/* ../../gcc/gcc/config/i386/i386.md:8857 */
rtx
gen_xorqi_ext_0 (operand0, operand1, operand2)
     rtx operand0;
     rtx operand1;
     rtx operand2;
{
  return gen_rtx_PARALLEL (VOIDmode, gen_rtvec (2,
		gen_rtx_SET (VOIDmode,
	gen_rtx_ZERO_EXTRACT (SImode,
	operand0,
	GEN_INT (8LL),
	GEN_INT (8LL)),
	gen_rtx_XOR (SImode,
	gen_rtx_ZERO_EXTRACT (SImode,
	operand1,
	GEN_INT (8LL),
	GEN_INT (8LL)),
	operand2)),
		gen_rtx_CLOBBER (VOIDmode,
	gen_rtx_REG (CCmode,
	17))));
}

/* ../../gcc/gcc/config/i386/i386.md:9284 */
rtx
gen_negsf2_memory (operand0, operand1)
     rtx operand0;
     rtx operand1;
{
  return gen_rtx_PARALLEL (VOIDmode, gen_rtvec (2,
		gen_rtx_SET (VOIDmode,
	operand0,
	gen_rtx_NEG (SFmode,
	operand1)),
		gen_rtx_CLOBBER (VOIDmode,
	gen_rtx_REG (CCmode,
	17))));
}

/* ../../gcc/gcc/config/i386/i386.md:9291 */
rtx
gen_negsf2_ifs (operand0, operand1, operand2)
     rtx operand0;
     rtx operand1;
     rtx operand2;
{
  return gen_rtx_PARALLEL (VOIDmode, gen_rtvec (3,
		gen_rtx_SET (VOIDmode,
	operand0,
	gen_rtx_NEG (SFmode,
	operand1)),
		gen_rtx_USE (VOIDmode,
	operand2),
		gen_rtx_CLOBBER (VOIDmode,
	gen_rtx_REG (CCmode,
	17))));
}

/* ../../gcc/gcc/config/i386/i386.md:9423 */
rtx
gen_negdf2_memory (operand0, operand1)
     rtx operand0;
     rtx operand1;
{
  return gen_rtx_PARALLEL (VOIDmode, gen_rtvec (2,
		gen_rtx_SET (VOIDmode,
	operand0,
	gen_rtx_NEG (DFmode,
	operand1)),
		gen_rtx_CLOBBER (VOIDmode,
	gen_rtx_REG (CCmode,
	17))));
}

/* ../../gcc/gcc/config/i386/i386.md:9430 */
rtx
gen_negdf2_ifs (operand0, operand1, operand2)
     rtx operand0;
     rtx operand1;
     rtx operand2;
{
  return gen_rtx_PARALLEL (VOIDmode, gen_rtvec (3,
		gen_rtx_SET (VOIDmode,
	operand0,
	gen_rtx_NEG (DFmode,
	operand1)),
		gen_rtx_USE (VOIDmode,
	operand2),
		gen_rtx_CLOBBER (VOIDmode,
	gen_rtx_REG (CCmode,
	17))));
}

/* ../../gcc/gcc/config/i386/i386.md:9744 */
rtx
gen_abssf2_memory (operand0, operand1)
     rtx operand0;
     rtx operand1;
{
  return gen_rtx_PARALLEL (VOIDmode, gen_rtvec (2,
		gen_rtx_SET (VOIDmode,
	operand0,
	gen_rtx_ABS (SFmode,
	operand1)),
		gen_rtx_CLOBBER (VOIDmode,
	gen_rtx_REG (CCmode,
	17))));
}

/* ../../gcc/gcc/config/i386/i386.md:9751 */
rtx
gen_abssf2_ifs (operand0, operand1, operand2)
     rtx operand0;
     rtx operand1;
     rtx operand2;
{
  return gen_rtx_PARALLEL (VOIDmode, gen_rtvec (3,
		gen_rtx_SET (VOIDmode,
	operand0,
	gen_rtx_ABS (SFmode,
	operand1)),
		gen_rtx_USE (VOIDmode,
	operand2),
		gen_rtx_CLOBBER (VOIDmode,
	gen_rtx_REG (CCmode,
	17))));
}

/* ../../gcc/gcc/config/i386/i386.md:9872 */
rtx
gen_absdf2_memory (operand0, operand1)
     rtx operand0;
     rtx operand1;
{
  return gen_rtx_PARALLEL (VOIDmode, gen_rtvec (2,
		gen_rtx_SET (VOIDmode,
	operand0,
	gen_rtx_ABS (DFmode,
	operand1)),
		gen_rtx_CLOBBER (VOIDmode,
	gen_rtx_REG (CCmode,
	17))));
}

/* ../../gcc/gcc/config/i386/i386.md:9879 */
rtx
gen_absdf2_ifs (operand0, operand1, operand2)
     rtx operand0;
     rtx operand1;
     rtx operand2;
{
  return gen_rtx_PARALLEL (VOIDmode, gen_rtvec (3,
		gen_rtx_SET (VOIDmode,
	operand0,
	gen_rtx_ABS (DFmode,
	operand1)),
		gen_rtx_USE (VOIDmode,
	operand2),
		gen_rtx_CLOBBER (VOIDmode,
	gen_rtx_REG (CCmode,
	17))));
}

/* ../../gcc/gcc/config/i386/i386.md:10466 */
rtx
gen_ashldi3_1 (operand0, operand1, operand2)
     rtx operand0;
     rtx operand1;
     rtx operand2;
{
  return gen_rtx_PARALLEL (VOIDmode, gen_rtvec (3,
		gen_rtx_SET (VOIDmode,
	operand0,
	gen_rtx_ASHIFT (DImode,
	operand1,
	operand2)),
		gen_rtx_CLOBBER (VOIDmode,
	gen_rtx_SCRATCH (SImode)),
		gen_rtx_CLOBBER (VOIDmode,
	gen_rtx_REG (CCmode,
	17))));
}

/* ../../gcc/gcc/config/i386/i386.md:10504 */
rtx
gen_x86_shld_1 (operand0, operand1, operand2)
     rtx operand0;
     rtx operand1;
     rtx operand2;
{
  return gen_rtx_PARALLEL (VOIDmode, gen_rtvec (2,
		gen_rtx_SET (VOIDmode,
	operand0,
	gen_rtx_IOR (SImode,
	gen_rtx_ASHIFT (SImode,
	operand0,
	operand2),
	gen_rtx_LSHIFTRT (SImode,
	operand1,
	gen_rtx_MINUS (QImode,
	GEN_INT (32LL),
	operand2)))),
		gen_rtx_CLOBBER (VOIDmode,
	gen_rtx_REG (CCmode,
	17))));
}

/* ../../gcc/gcc/config/i386/i386.md:11098 */
rtx
gen_ashrdi3_63_rex64 (operand0, operand1, operand2)
     rtx operand0;
     rtx operand1;
     rtx operand2;
{
  return gen_rtx_PARALLEL (VOIDmode, gen_rtvec (2,
		gen_rtx_SET (VOIDmode,
	operand0,
	gen_rtx_ASHIFTRT (DImode,
	operand1,
	operand2)),
		gen_rtx_CLOBBER (VOIDmode,
	gen_rtx_REG (CCmode,
	17))));
}

/* ../../gcc/gcc/config/i386/i386.md:11179 */
rtx
gen_ashrdi3_1 (operand0, operand1, operand2)
     rtx operand0;
     rtx operand1;
     rtx operand2;
{
  return gen_rtx_PARALLEL (VOIDmode, gen_rtvec (3,
		gen_rtx_SET (VOIDmode,
	operand0,
	gen_rtx_ASHIFTRT (DImode,
	operand1,
	operand2)),
		gen_rtx_CLOBBER (VOIDmode,
	gen_rtx_SCRATCH (SImode)),
		gen_rtx_CLOBBER (VOIDmode,
	gen_rtx_REG (CCmode,
	17))));
}

/* ../../gcc/gcc/config/i386/i386.md:11217 */
rtx
gen_x86_shrd_1 (operand0, operand1, operand2)
     rtx operand0;
     rtx operand1;
     rtx operand2;
{
  return gen_rtx_PARALLEL (VOIDmode, gen_rtvec (2,
		gen_rtx_SET (VOIDmode,
	operand0,
	gen_rtx_IOR (SImode,
	gen_rtx_ASHIFTRT (SImode,
	operand0,
	operand2),
	gen_rtx_ASHIFT (SImode,
	operand1,
	gen_rtx_MINUS (QImode,
	GEN_INT (32LL),
	operand2)))),
		gen_rtx_CLOBBER (VOIDmode,
	gen_rtx_REG (CCmode,
	17))));
}

/* ../../gcc/gcc/config/i386/i386.md:11262 */
rtx
gen_ashrsi3_31 (operand0, operand1, operand2)
     rtx operand0;
     rtx operand1;
     rtx operand2;
{
  return gen_rtx_PARALLEL (VOIDmode, gen_rtvec (2,
		gen_rtx_SET (VOIDmode,
	operand0,
	gen_rtx_ASHIFTRT (SImode,
	operand1,
	operand2)),
		gen_rtx_CLOBBER (VOIDmode,
	gen_rtx_REG (CCmode,
	17))));
}

/* ../../gcc/gcc/config/i386/i386.md:11675 */
rtx
gen_lshrdi3_1 (operand0, operand1, operand2)
     rtx operand0;
     rtx operand1;
     rtx operand2;
{
  return gen_rtx_PARALLEL (VOIDmode, gen_rtvec (3,
		gen_rtx_SET (VOIDmode,
	operand0,
	gen_rtx_LSHIFTRT (DImode,
	operand1,
	operand2)),
		gen_rtx_CLOBBER (VOIDmode,
	gen_rtx_SCRATCH (SImode)),
		gen_rtx_CLOBBER (VOIDmode,
	gen_rtx_REG (CCmode,
	17))));
}

/* ../../gcc/gcc/config/i386/i386.md:12571 */
rtx
gen_setcc_2 (operand0, operand1)
     rtx operand0;
     rtx operand1;
{
  return gen_rtx_SET (VOIDmode,
	gen_rtx_STRICT_LOW_PART (VOIDmode,
	operand0),
	gen_rtx (GET_CODE (operand1), QImode,
		gen_rtx_REG (VOIDmode,
	17),
		const0_rtx));
}

/* ../../gcc/gcc/config/i386/i386.md:13117 */
rtx
gen_jump (operand0)
     rtx operand0;
{
  return gen_rtx_SET (VOIDmode,
	pc_rtx,
	gen_rtx_LABEL_REF (VOIDmode,
	operand0));
}

/* ../../gcc/gcc/config/i386/i386.md:13227 */
rtx
gen_doloop_end_internal (operand0, operand1, operand2)
     rtx operand0;
     rtx operand1;
     rtx operand2;
{
  return gen_rtx_PARALLEL (VOIDmode, gen_rtvec (4,
		gen_rtx_SET (VOIDmode,
	pc_rtx,
	gen_rtx_IF_THEN_ELSE (VOIDmode,
	gen_rtx_NE (VOIDmode,
	operand1,
	const1_rtx),
	gen_rtx_LABEL_REF (VOIDmode,
	operand0),
	pc_rtx)),
		gen_rtx_SET (VOIDmode,
	operand2,
	gen_rtx_PLUS (SImode,
	operand1,
	constm1_rtx)),
		gen_rtx_CLOBBER (VOIDmode,
	gen_rtx_SCRATCH (SImode)),
		gen_rtx_CLOBBER (VOIDmode,
	gen_rtx_REG (CCmode,
	17))));
}

/* ../../gcc/gcc/config/i386/i386.md:13536 */
rtx
gen_blockage (operand0)
     rtx operand0;
{
  return gen_rtx_UNSPEC_VOLATILE (VOIDmode,
	gen_rtvec (1,
		operand0),
	0);
}

/* ../../gcc/gcc/config/i386/i386.md:13558 */
rtx
gen_return_internal ()
{
  return gen_rtx_RETURN (VOIDmode);
}

/* ../../gcc/gcc/config/i386/i386.md:13566 */
rtx
gen_return_pop_internal (operand0)
     rtx operand0;
{
  return gen_rtx_PARALLEL (VOIDmode, gen_rtvec (2,
		gen_rtx_RETURN (VOIDmode),
		gen_rtx_USE (VOIDmode,
	operand0)));
}

/* ../../gcc/gcc/config/i386/i386.md:13575 */
rtx
gen_return_indirect_internal (operand0)
     rtx operand0;
{
  return gen_rtx_PARALLEL (VOIDmode, gen_rtvec (2,
		gen_rtx_RETURN (VOIDmode),
		gen_rtx_USE (VOIDmode,
	operand0)));
}

/* ../../gcc/gcc/config/i386/i386.md:13583 */
rtx
gen_nop ()
{
  return const0_rtx;
}

/* ../../gcc/gcc/config/i386/i386.md:13597 */
rtx
gen_set_got (operand0)
     rtx operand0;
{
  return gen_rtx_PARALLEL (VOIDmode, gen_rtvec (2,
		gen_rtx_SET (VOIDmode,
	operand0,
	gen_rtx_UNSPEC (SImode,
	gen_rtvec (1,
		const0_rtx),
	12)),
		gen_rtx_CLOBBER (VOIDmode,
	gen_rtx_REG (CCmode,
	17))));
}

/* ../../gcc/gcc/config/i386/i386.md:13638 */
rtx
gen_eh_return_si (operand0)
     rtx operand0;
{
  return gen_rtx_UNSPEC_VOLATILE (VOIDmode,
	gen_rtvec (1,
		operand0),
	13);
}

/* ../../gcc/gcc/config/i386/i386.md:13647 */
rtx
gen_eh_return_di (operand0)
     rtx operand0;
{
  return gen_rtx_UNSPEC_VOLATILE (VOIDmode,
	gen_rtvec (1,
		operand0),
	13);
}

/* ../../gcc/gcc/config/i386/i386.md:13656 */
rtx
gen_leave ()
{
  return gen_rtx_PARALLEL (VOIDmode, gen_rtvec (3,
		gen_rtx_SET (VOIDmode,
	gen_rtx_REG (SImode,
	7),
	gen_rtx_PLUS (SImode,
	gen_rtx_REG (SImode,
	6),
	GEN_INT (4LL))),
		gen_rtx_SET (VOIDmode,
	gen_rtx_REG (SImode,
	6),
	gen_rtx_MEM (SImode,
	gen_rtx_REG (SImode,
	6))),
		gen_rtx_CLOBBER (VOIDmode,
	gen_rtx_MEM (BLKmode,
	gen_rtx_SCRATCH (VOIDmode)))));
}

/* ../../gcc/gcc/config/i386/i386.md:13668 */
rtx
gen_leave_rex64 ()
{
  return gen_rtx_PARALLEL (VOIDmode, gen_rtvec (3,
		gen_rtx_SET (VOIDmode,
	gen_rtx_REG (DImode,
	7),
	gen_rtx_PLUS (DImode,
	gen_rtx_REG (DImode,
	6),
	GEN_INT (8LL))),
		gen_rtx_SET (VOIDmode,
	gen_rtx_REG (DImode,
	6),
	gen_rtx_MEM (DImode,
	gen_rtx_REG (DImode,
	6))),
		gen_rtx_CLOBBER (VOIDmode,
	gen_rtx_MEM (BLKmode,
	gen_rtx_SCRATCH (VOIDmode)))));
}

/* ../../gcc/gcc/config/i386/i386.md:13766 */
rtx
gen_ffssi_1 (operand0, operand1)
     rtx operand0;
     rtx operand1;
{
  return gen_rtx_PARALLEL (VOIDmode, gen_rtvec (2,
		gen_rtx_SET (VOIDmode,
	gen_rtx_REG (CCZmode,
	17),
	gen_rtx_COMPARE (CCZmode,
	operand1,
	const0_rtx)),
		gen_rtx_SET (VOIDmode,
	operand0,
	gen_rtx_UNSPEC (SImode,
	gen_rtvec (1,
		operand1),
	23))));
}

/* ../../gcc/gcc/config/i386/i386.md:14678 */
rtx
gen_sqrtsf2_1 (operand0, operand1)
     rtx operand0;
     rtx operand1;
{
  return gen_rtx_SET (VOIDmode,
	operand0,
	gen_rtx_SQRT (SFmode,
	operand1));
}

/* ../../gcc/gcc/config/i386/i386.md:14690 */
rtx
gen_sqrtsf2_1_sse_only (operand0, operand1)
     rtx operand0;
     rtx operand1;
{
  return gen_rtx_SET (VOIDmode,
	operand0,
	gen_rtx_SQRT (SFmode,
	operand1));
}

/* ../../gcc/gcc/config/i386/i386.md:14699 */
rtx
gen_sqrtsf2_i387 (operand0, operand1)
     rtx operand0;
     rtx operand1;
{
  return gen_rtx_SET (VOIDmode,
	operand0,
	gen_rtx_SQRT (SFmode,
	operand1));
}

/* ../../gcc/gcc/config/i386/i386.md:14719 */
rtx
gen_sqrtdf2_1 (operand0, operand1)
     rtx operand0;
     rtx operand1;
{
  return gen_rtx_SET (VOIDmode,
	operand0,
	gen_rtx_SQRT (DFmode,
	operand1));
}

/* ../../gcc/gcc/config/i386/i386.md:14731 */
rtx
gen_sqrtdf2_1_sse_only (operand0, operand1)
     rtx operand0;
     rtx operand1;
{
  return gen_rtx_SET (VOIDmode,
	operand0,
	gen_rtx_SQRT (DFmode,
	operand1));
}

/* ../../gcc/gcc/config/i386/i386.md:14740 */
rtx
gen_sqrtdf2_i387 (operand0, operand1)
     rtx operand0;
     rtx operand1;
{
  return gen_rtx_SET (VOIDmode,
	operand0,
	gen_rtx_SQRT (DFmode,
	operand1));
}

/* ../../gcc/gcc/config/i386/i386.md:14761 */
rtx
gen_sqrtxf2 (operand0, operand1)
     rtx operand0;
     rtx operand1;
{
  return gen_rtx_SET (VOIDmode,
	operand0,
	gen_rtx_SQRT (XFmode,
	operand1));
}

/* ../../gcc/gcc/config/i386/i386.md:14771 */
rtx
gen_sqrttf2 (operand0, operand1)
     rtx operand0;
     rtx operand1;
{
  return gen_rtx_SET (VOIDmode,
	operand0,
	gen_rtx_SQRT (TFmode,
	operand1));
}

/* ../../gcc/gcc/config/i386/i386.md:14821 */
rtx
gen_sindf2 (operand0, operand1)
     rtx operand0;
     rtx operand1;
{
  return gen_rtx_SET (VOIDmode,
	operand0,
	gen_rtx_UNSPEC (DFmode,
	gen_rtvec (1,
		operand1),
	21));
}

/* ../../gcc/gcc/config/i386/i386.md:14830 */
rtx
gen_sinsf2 (operand0, operand1)
     rtx operand0;
     rtx operand1;
{
  return gen_rtx_SET (VOIDmode,
	operand0,
	gen_rtx_UNSPEC (SFmode,
	gen_rtvec (1,
		operand1),
	21));
}

/* ../../gcc/gcc/config/i386/i386.md:14850 */
rtx
gen_sinxf2 (operand0, operand1)
     rtx operand0;
     rtx operand1;
{
  return gen_rtx_SET (VOIDmode,
	operand0,
	gen_rtx_UNSPEC (XFmode,
	gen_rtvec (1,
		operand1),
	21));
}

/* ../../gcc/gcc/config/i386/i386.md:14859 */
rtx
gen_sintf2 (operand0, operand1)
     rtx operand0;
     rtx operand1;
{
  return gen_rtx_SET (VOIDmode,
	operand0,
	gen_rtx_UNSPEC (TFmode,
	gen_rtvec (1,
		operand1),
	21));
}

/* ../../gcc/gcc/config/i386/i386.md:14868 */
rtx
gen_cosdf2 (operand0, operand1)
     rtx operand0;
     rtx operand1;
{
  return gen_rtx_SET (VOIDmode,
	operand0,
	gen_rtx_UNSPEC (DFmode,
	gen_rtvec (1,
		operand1),
	22));
}

/* ../../gcc/gcc/config/i386/i386.md:14877 */
rtx
gen_cossf2 (operand0, operand1)
     rtx operand0;
     rtx operand1;
{
  return gen_rtx_SET (VOIDmode,
	operand0,
	gen_rtx_UNSPEC (SFmode,
	gen_rtvec (1,
		operand1),
	22));
}

/* ../../gcc/gcc/config/i386/i386.md:14897 */
rtx
gen_cosxf2 (operand0, operand1)
     rtx operand0;
     rtx operand1;
{
  return gen_rtx_SET (VOIDmode,
	operand0,
	gen_rtx_UNSPEC (XFmode,
	gen_rtvec (1,
		operand1),
	22));
}

/* ../../gcc/gcc/config/i386/i386.md:14906 */
rtx
gen_costf2 (operand0, operand1)
     rtx operand0;
     rtx operand1;
{
  return gen_rtx_SET (VOIDmode,
	operand0,
	gen_rtx_UNSPEC (TFmode,
	gen_rtvec (1,
		operand1),
	22));
}

/* ../../gcc/gcc/config/i386/i386.md:14917 */
rtx
gen_cld ()
{
  return gen_rtx_SET (VOIDmode,
	gen_rtx_REG (SImode,
	19),
	const0_rtx);
}

/* ../../gcc/gcc/config/i386/i386.md:15115 */
rtx
gen_strmovdi_rex_1 (operand0, operand1, operand2, operand3)
     rtx operand0;
     rtx operand1;
     rtx operand2;
     rtx operand3;
{
  return gen_rtx_PARALLEL (VOIDmode, gen_rtvec (4,
		gen_rtx_SET (VOIDmode,
	gen_rtx_MEM (DImode,
	operand2),
	gen_rtx_MEM (DImode,
	operand3)),
		gen_rtx_SET (VOIDmode,
	operand0,
	gen_rtx_PLUS (DImode,
	operand2,
	GEN_INT (8LL))),
		gen_rtx_SET (VOIDmode,
	operand1,
	gen_rtx_PLUS (DImode,
	operand3,
	GEN_INT (8LL))),
		gen_rtx_USE (VOIDmode,
	gen_rtx_REG (SImode,
	19))));
}

/* ../../gcc/gcc/config/i386/i386.md:15131 */
rtx
gen_strmovsi_1 (operand0, operand1, operand2, operand3)
     rtx operand0;
     rtx operand1;
     rtx operand2;
     rtx operand3;
{
  return gen_rtx_PARALLEL (VOIDmode, gen_rtvec (4,
		gen_rtx_SET (VOIDmode,
	gen_rtx_MEM (SImode,
	operand2),
	gen_rtx_MEM (SImode,
	operand3)),
		gen_rtx_SET (VOIDmode,
	operand0,
	gen_rtx_PLUS (SImode,
	operand2,
	GEN_INT (4LL))),
		gen_rtx_SET (VOIDmode,
	operand1,
	gen_rtx_PLUS (SImode,
	operand3,
	GEN_INT (4LL))),
		gen_rtx_USE (VOIDmode,
	gen_rtx_REG (SImode,
	19))));
}

/* ../../gcc/gcc/config/i386/i386.md:15147 */
rtx
gen_strmovsi_rex_1 (operand0, operand1, operand2, operand3)
     rtx operand0;
     rtx operand1;
     rtx operand2;
     rtx operand3;
{
  return gen_rtx_PARALLEL (VOIDmode, gen_rtvec (4,
		gen_rtx_SET (VOIDmode,
	gen_rtx_MEM (SImode,
	operand2),
	gen_rtx_MEM (SImode,
	operand3)),
		gen_rtx_SET (VOIDmode,
	operand0,
	gen_rtx_PLUS (DImode,
	operand2,
	GEN_INT (4LL))),
		gen_rtx_SET (VOIDmode,
	operand1,
	gen_rtx_PLUS (DImode,
	operand3,
	GEN_INT (4LL))),
		gen_rtx_USE (VOIDmode,
	gen_rtx_REG (SImode,
	19))));
}

/* ../../gcc/gcc/config/i386/i386.md:15163 */
rtx
gen_strmovhi_1 (operand0, operand1, operand2, operand3)
     rtx operand0;
     rtx operand1;
     rtx operand2;
     rtx operand3;
{
  return gen_rtx_PARALLEL (VOIDmode, gen_rtvec (4,
		gen_rtx_SET (VOIDmode,
	gen_rtx_MEM (HImode,
	operand2),
	gen_rtx_MEM (HImode,
	operand3)),
		gen_rtx_SET (VOIDmode,
	operand0,
	gen_rtx_PLUS (SImode,
	operand2,
	GEN_INT (2LL))),
		gen_rtx_SET (VOIDmode,
	operand1,
	gen_rtx_PLUS (SImode,
	operand3,
	GEN_INT (2LL))),
		gen_rtx_USE (VOIDmode,
	gen_rtx_REG (SImode,
	19))));
}

/* ../../gcc/gcc/config/i386/i386.md:15179 */
rtx
gen_strmovhi_rex_1 (operand0, operand1, operand2, operand3)
     rtx operand0;
     rtx operand1;
     rtx operand2;
     rtx operand3;
{
  return gen_rtx_PARALLEL (VOIDmode, gen_rtvec (4,
		gen_rtx_SET (VOIDmode,
	gen_rtx_MEM (HImode,
	operand2),
	gen_rtx_MEM (HImode,
	operand3)),
		gen_rtx_SET (VOIDmode,
	operand0,
	gen_rtx_PLUS (DImode,
	operand2,
	GEN_INT (2LL))),
		gen_rtx_SET (VOIDmode,
	operand1,
	gen_rtx_PLUS (DImode,
	operand3,
	GEN_INT (2LL))),
		gen_rtx_USE (VOIDmode,
	gen_rtx_REG (SImode,
	19))));
}

/* ../../gcc/gcc/config/i386/i386.md:15195 */
rtx
gen_strmovqi_1 (operand0, operand1, operand2, operand3)
     rtx operand0;
     rtx operand1;
     rtx operand2;
     rtx operand3;
{
  return gen_rtx_PARALLEL (VOIDmode, gen_rtvec (4,
		gen_rtx_SET (VOIDmode,
	gen_rtx_MEM (QImode,
	operand2),
	gen_rtx_MEM (QImode,
	operand3)),
		gen_rtx_SET (VOIDmode,
	operand0,
	gen_rtx_PLUS (SImode,
	operand2,
	const1_rtx)),
		gen_rtx_SET (VOIDmode,
	operand1,
	gen_rtx_PLUS (SImode,
	operand3,
	const1_rtx)),
		gen_rtx_USE (VOIDmode,
	gen_rtx_REG (SImode,
	19))));
}

/* ../../gcc/gcc/config/i386/i386.md:15211 */
rtx
gen_strmovqi_rex_1 (operand0, operand1, operand2, operand3)
     rtx operand0;
     rtx operand1;
     rtx operand2;
     rtx operand3;
{
  return gen_rtx_PARALLEL (VOIDmode, gen_rtvec (4,
		gen_rtx_SET (VOIDmode,
	gen_rtx_MEM (QImode,
	operand2),
	gen_rtx_MEM (QImode,
	operand3)),
		gen_rtx_SET (VOIDmode,
	operand0,
	gen_rtx_PLUS (DImode,
	operand2,
	const1_rtx)),
		gen_rtx_SET (VOIDmode,
	operand1,
	gen_rtx_PLUS (DImode,
	operand3,
	const1_rtx)),
		gen_rtx_USE (VOIDmode,
	gen_rtx_REG (SImode,
	19))));
}

/* ../../gcc/gcc/config/i386/i386.md:15227 */
rtx
gen_rep_movdi_rex64 (operand0, operand1, operand2, operand3, operand4, operand5)
     rtx operand0;
     rtx operand1;
     rtx operand2;
     rtx operand3;
     rtx operand4;
     rtx operand5;
{
  return gen_rtx_PARALLEL (VOIDmode, gen_rtvec (6,
		gen_rtx_SET (VOIDmode,
	operand2,
	const0_rtx),
		gen_rtx_SET (VOIDmode,
	operand0,
	gen_rtx_PLUS (DImode,
	gen_rtx_ASHIFT (DImode,
	operand5,
	GEN_INT (3LL)),
	operand3)),
		gen_rtx_SET (VOIDmode,
	operand1,
	gen_rtx_PLUS (DImode,
	gen_rtx_ASHIFT (DImode,
	operand5,
	GEN_INT (3LL)),
	operand4)),
		gen_rtx_SET (VOIDmode,
	gen_rtx_MEM (BLKmode,
	operand3),
	gen_rtx_MEM (BLKmode,
	operand4)),
		gen_rtx_USE (VOIDmode,
	operand5),
		gen_rtx_USE (VOIDmode,
	gen_rtx_REG (SImode,
	19))));
}

/* ../../gcc/gcc/config/i386/i386.md:15247 */
rtx
gen_rep_movsi (operand0, operand1, operand2, operand3, operand4, operand5)
     rtx operand0;
     rtx operand1;
     rtx operand2;
     rtx operand3;
     rtx operand4;
     rtx operand5;
{
  return gen_rtx_PARALLEL (VOIDmode, gen_rtvec (6,
		gen_rtx_SET (VOIDmode,
	operand2,
	const0_rtx),
		gen_rtx_SET (VOIDmode,
	operand0,
	gen_rtx_PLUS (SImode,
	gen_rtx_ASHIFT (SImode,
	operand5,
	GEN_INT (2LL)),
	operand3)),
		gen_rtx_SET (VOIDmode,
	operand1,
	gen_rtx_PLUS (SImode,
	gen_rtx_ASHIFT (SImode,
	operand5,
	GEN_INT (2LL)),
	operand4)),
		gen_rtx_SET (VOIDmode,
	gen_rtx_MEM (BLKmode,
	operand3),
	gen_rtx_MEM (BLKmode,
	operand4)),
		gen_rtx_USE (VOIDmode,
	operand5),
		gen_rtx_USE (VOIDmode,
	gen_rtx_REG (SImode,
	19))));
}

/* ../../gcc/gcc/config/i386/i386.md:15267 */
rtx
gen_rep_movsi_rex64 (operand0, operand1, operand2, operand3, operand4, operand5)
     rtx operand0;
     rtx operand1;
     rtx operand2;
     rtx operand3;
     rtx operand4;
     rtx operand5;
{
  return gen_rtx_PARALLEL (VOIDmode, gen_rtvec (6,
		gen_rtx_SET (VOIDmode,
	operand2,
	const0_rtx),
		gen_rtx_SET (VOIDmode,
	operand0,
	gen_rtx_PLUS (DImode,
	gen_rtx_ASHIFT (DImode,
	operand5,
	GEN_INT (2LL)),
	operand3)),
		gen_rtx_SET (VOIDmode,
	operand1,
	gen_rtx_PLUS (DImode,
	gen_rtx_ASHIFT (DImode,
	operand5,
	GEN_INT (2LL)),
	operand4)),
		gen_rtx_SET (VOIDmode,
	gen_rtx_MEM (BLKmode,
	operand3),
	gen_rtx_MEM (BLKmode,
	operand4)),
		gen_rtx_USE (VOIDmode,
	operand5),
		gen_rtx_USE (VOIDmode,
	gen_rtx_REG (SImode,
	19))));
}

/* ../../gcc/gcc/config/i386/i386.md:15287 */
rtx
gen_rep_movqi (operand0, operand1, operand2, operand3, operand4, operand5)
     rtx operand0;
     rtx operand1;
     rtx operand2;
     rtx operand3;
     rtx operand4;
     rtx operand5;
{
  return gen_rtx_PARALLEL (VOIDmode, gen_rtvec (6,
		gen_rtx_SET (VOIDmode,
	operand2,
	const0_rtx),
		gen_rtx_SET (VOIDmode,
	operand0,
	gen_rtx_PLUS (SImode,
	operand3,
	operand5)),
		gen_rtx_SET (VOIDmode,
	operand1,
	gen_rtx_PLUS (SImode,
	operand4,
	operand5)),
		gen_rtx_SET (VOIDmode,
	gen_rtx_MEM (BLKmode,
	operand3),
	gen_rtx_MEM (BLKmode,
	operand4)),
		gen_rtx_USE (VOIDmode,
	operand5),
		gen_rtx_USE (VOIDmode,
	gen_rtx_REG (SImode,
	19))));
}

/* ../../gcc/gcc/config/i386/i386.md:15305 */
rtx
gen_rep_movqi_rex64 (operand0, operand1, operand2, operand3, operand4, operand5)
     rtx operand0;
     rtx operand1;
     rtx operand2;
     rtx operand3;
     rtx operand4;
     rtx operand5;
{
  return gen_rtx_PARALLEL (VOIDmode, gen_rtvec (6,
		gen_rtx_SET (VOIDmode,
	operand2,
	const0_rtx),
		gen_rtx_SET (VOIDmode,
	operand0,
	gen_rtx_PLUS (DImode,
	operand3,
	operand5)),
		gen_rtx_SET (VOIDmode,
	operand1,
	gen_rtx_PLUS (DImode,
	operand4,
	operand5)),
		gen_rtx_SET (VOIDmode,
	gen_rtx_MEM (BLKmode,
	operand3),
	gen_rtx_MEM (BLKmode,
	operand4)),
		gen_rtx_USE (VOIDmode,
	operand5),
		gen_rtx_USE (VOIDmode,
	gen_rtx_REG (SImode,
	19))));
}

/* ../../gcc/gcc/config/i386/i386.md:15463 */
rtx
gen_strsetdi_rex_1 (operand0, operand1, operand2)
     rtx operand0;
     rtx operand1;
     rtx operand2;
{
  return gen_rtx_PARALLEL (VOIDmode, gen_rtvec (3,
		gen_rtx_SET (VOIDmode,
	gen_rtx_MEM (SImode,
	operand1),
	operand2),
		gen_rtx_SET (VOIDmode,
	operand0,
	gen_rtx_PLUS (DImode,
	operand1,
	GEN_INT (8LL))),
		gen_rtx_USE (VOIDmode,
	gen_rtx_REG (SImode,
	19))));
}

/* ../../gcc/gcc/config/i386/i386.md:15476 */
rtx
gen_strsetsi_1 (operand0, operand1, operand2)
     rtx operand0;
     rtx operand1;
     rtx operand2;
{
  return gen_rtx_PARALLEL (VOIDmode, gen_rtvec (3,
		gen_rtx_SET (VOIDmode,
	gen_rtx_MEM (SImode,
	operand1),
	operand2),
		gen_rtx_SET (VOIDmode,
	operand0,
	gen_rtx_PLUS (SImode,
	operand1,
	GEN_INT (4LL))),
		gen_rtx_USE (VOIDmode,
	gen_rtx_REG (SImode,
	19))));
}

/* ../../gcc/gcc/config/i386/i386.md:15489 */
rtx
gen_strsetsi_rex_1 (operand0, operand1, operand2)
     rtx operand0;
     rtx operand1;
     rtx operand2;
{
  return gen_rtx_PARALLEL (VOIDmode, gen_rtvec (3,
		gen_rtx_SET (VOIDmode,
	gen_rtx_MEM (SImode,
	operand1),
	operand2),
		gen_rtx_SET (VOIDmode,
	operand0,
	gen_rtx_PLUS (DImode,
	operand1,
	GEN_INT (4LL))),
		gen_rtx_USE (VOIDmode,
	gen_rtx_REG (SImode,
	19))));
}

/* ../../gcc/gcc/config/i386/i386.md:15502 */
rtx
gen_strsethi_1 (operand0, operand1, operand2)
     rtx operand0;
     rtx operand1;
     rtx operand2;
{
  return gen_rtx_PARALLEL (VOIDmode, gen_rtvec (3,
		gen_rtx_SET (VOIDmode,
	gen_rtx_MEM (HImode,
	operand1),
	operand2),
		gen_rtx_SET (VOIDmode,
	operand0,
	gen_rtx_PLUS (SImode,
	operand1,
	GEN_INT (2LL))),
		gen_rtx_USE (VOIDmode,
	gen_rtx_REG (SImode,
	19))));
}

/* ../../gcc/gcc/config/i386/i386.md:15515 */
rtx
gen_strsethi_rex_1 (operand0, operand1, operand2)
     rtx operand0;
     rtx operand1;
     rtx operand2;
{
  return gen_rtx_PARALLEL (VOIDmode, gen_rtvec (3,
		gen_rtx_SET (VOIDmode,
	gen_rtx_MEM (HImode,
	operand1),
	operand2),
		gen_rtx_SET (VOIDmode,
	operand0,
	gen_rtx_PLUS (DImode,
	operand1,
	GEN_INT (2LL))),
		gen_rtx_USE (VOIDmode,
	gen_rtx_REG (SImode,
	19))));
}

/* ../../gcc/gcc/config/i386/i386.md:15528 */
rtx
gen_strsetqi_1 (operand0, operand1, operand2)
     rtx operand0;
     rtx operand1;
     rtx operand2;
{
  return gen_rtx_PARALLEL (VOIDmode, gen_rtvec (3,
		gen_rtx_SET (VOIDmode,
	gen_rtx_MEM (QImode,
	operand1),
	operand2),
		gen_rtx_SET (VOIDmode,
	operand0,
	gen_rtx_PLUS (SImode,
	operand1,
	const1_rtx)),
		gen_rtx_USE (VOIDmode,
	gen_rtx_REG (SImode,
	19))));
}

/* ../../gcc/gcc/config/i386/i386.md:15541 */
rtx
gen_strsetqi_rex_1 (operand0, operand1, operand2)
     rtx operand0;
     rtx operand1;
     rtx operand2;
{
  return gen_rtx_PARALLEL (VOIDmode, gen_rtvec (3,
		gen_rtx_SET (VOIDmode,
	gen_rtx_MEM (QImode,
	operand1),
	operand2),
		gen_rtx_SET (VOIDmode,
	operand0,
	gen_rtx_PLUS (DImode,
	operand1,
	const1_rtx)),
		gen_rtx_USE (VOIDmode,
	gen_rtx_REG (SImode,
	19))));
}

/* ../../gcc/gcc/config/i386/i386.md:15554 */
rtx
gen_rep_stosdi_rex64 (operand0, operand1, operand2, operand3, operand4)
     rtx operand0;
     rtx operand1;
     rtx operand2;
     rtx operand3;
     rtx operand4;
{
  return gen_rtx_PARALLEL (VOIDmode, gen_rtvec (6,
		gen_rtx_SET (VOIDmode,
	operand1,
	const0_rtx),
		gen_rtx_SET (VOIDmode,
	operand0,
	gen_rtx_PLUS (DImode,
	gen_rtx_ASHIFT (DImode,
	operand4,
	GEN_INT (3LL)),
	operand3)),
		gen_rtx_SET (VOIDmode,
	gen_rtx_MEM (BLKmode,
	operand3),
	const0_rtx),
		gen_rtx_USE (VOIDmode,
	operand2),
		gen_rtx_USE (VOIDmode,
	operand4),
		gen_rtx_USE (VOIDmode,
	gen_rtx_REG (SImode,
	19))));
}

/* ../../gcc/gcc/config/i386/i386.md:15572 */
rtx
gen_rep_stossi (operand0, operand1, operand2, operand3, operand4)
     rtx operand0;
     rtx operand1;
     rtx operand2;
     rtx operand3;
     rtx operand4;
{
  return gen_rtx_PARALLEL (VOIDmode, gen_rtvec (6,
		gen_rtx_SET (VOIDmode,
	operand1,
	const0_rtx),
		gen_rtx_SET (VOIDmode,
	operand0,
	gen_rtx_PLUS (SImode,
	gen_rtx_ASHIFT (SImode,
	operand4,
	GEN_INT (2LL)),
	operand3)),
		gen_rtx_SET (VOIDmode,
	gen_rtx_MEM (BLKmode,
	operand3),
	const0_rtx),
		gen_rtx_USE (VOIDmode,
	operand2),
		gen_rtx_USE (VOIDmode,
	operand4),
		gen_rtx_USE (VOIDmode,
	gen_rtx_REG (SImode,
	19))));
}

/* ../../gcc/gcc/config/i386/i386.md:15590 */
rtx
gen_rep_stossi_rex64 (operand0, operand1, operand2, operand3, operand4)
     rtx operand0;
     rtx operand1;
     rtx operand2;
     rtx operand3;
     rtx operand4;
{
  return gen_rtx_PARALLEL (VOIDmode, gen_rtvec (6,
		gen_rtx_SET (VOIDmode,
	operand1,
	const0_rtx),
		gen_rtx_SET (VOIDmode,
	operand0,
	gen_rtx_PLUS (DImode,
	gen_rtx_ASHIFT (DImode,
	operand4,
	GEN_INT (2LL)),
	operand3)),
		gen_rtx_SET (VOIDmode,
	gen_rtx_MEM (BLKmode,
	operand3),
	const0_rtx),
		gen_rtx_USE (VOIDmode,
	operand2),
		gen_rtx_USE (VOIDmode,
	operand4),
		gen_rtx_USE (VOIDmode,
	gen_rtx_REG (SImode,
	19))));
}

/* ../../gcc/gcc/config/i386/i386.md:15608 */
rtx
gen_rep_stosqi (operand0, operand1, operand2, operand3, operand4)
     rtx operand0;
     rtx operand1;
     rtx operand2;
     rtx operand3;
     rtx operand4;
{
  return gen_rtx_PARALLEL (VOIDmode, gen_rtvec (6,
		gen_rtx_SET (VOIDmode,
	operand1,
	const0_rtx),
		gen_rtx_SET (VOIDmode,
	operand0,
	gen_rtx_PLUS (SImode,
	operand3,
	operand4)),
		gen_rtx_SET (VOIDmode,
	gen_rtx_MEM (BLKmode,
	operand3),
	const0_rtx),
		gen_rtx_USE (VOIDmode,
	operand2),
		gen_rtx_USE (VOIDmode,
	operand4),
		gen_rtx_USE (VOIDmode,
	gen_rtx_REG (SImode,
	19))));
}

/* ../../gcc/gcc/config/i386/i386.md:15625 */
rtx
gen_rep_stosqi_rex64 (operand0, operand1, operand2, operand3, operand4)
     rtx operand0;
     rtx operand1;
     rtx operand2;
     rtx operand3;
     rtx operand4;
{
  return gen_rtx_PARALLEL (VOIDmode, gen_rtvec (6,
		gen_rtx_SET (VOIDmode,
	operand1,
	const0_rtx),
		gen_rtx_SET (VOIDmode,
	operand0,
	gen_rtx_PLUS (DImode,
	operand3,
	operand4)),
		gen_rtx_SET (VOIDmode,
	gen_rtx_MEM (BLKmode,
	operand3),
	const0_rtx),
		gen_rtx_USE (VOIDmode,
	operand2),
		gen_rtx_USE (VOIDmode,
	operand4),
		gen_rtx_USE (VOIDmode,
	gen_rtx_REG (DImode,
	19))));
}

/* ../../gcc/gcc/config/i386/i386.md:15726 */
rtx
gen_cmpstrqi_nz_1 (operand0, operand1, operand2, operand3, operand4, operand5, operand6)
     rtx operand0;
     rtx operand1;
     rtx operand2;
     rtx operand3;
     rtx operand4;
     rtx operand5;
     rtx operand6;
{
  return gen_rtx_PARALLEL (VOIDmode, gen_rtvec (7,
		gen_rtx_SET (VOIDmode,
	gen_rtx_REG (CCmode,
	17),
	gen_rtx_COMPARE (CCmode,
	gen_rtx_MEM (BLKmode,
	operand4),
	gen_rtx_MEM (BLKmode,
	operand5))),
		gen_rtx_USE (VOIDmode,
	operand6),
		gen_rtx_USE (VOIDmode,
	operand3),
		gen_rtx_USE (VOIDmode,
	gen_rtx_REG (SImode,
	19)),
		gen_rtx_CLOBBER (VOIDmode,
	operand0),
		gen_rtx_CLOBBER (VOIDmode,
	operand1),
		gen_rtx_CLOBBER (VOIDmode,
	operand2)));
}

/* ../../gcc/gcc/config/i386/i386.md:15742 */
rtx
gen_cmpstrqi_nz_rex_1 (operand0, operand1, operand2, operand3, operand4, operand5, operand6)
     rtx operand0;
     rtx operand1;
     rtx operand2;
     rtx operand3;
     rtx operand4;
     rtx operand5;
     rtx operand6;
{
  return gen_rtx_PARALLEL (VOIDmode, gen_rtvec (7,
		gen_rtx_SET (VOIDmode,
	gen_rtx_REG (CCmode,
	17),
	gen_rtx_COMPARE (CCmode,
	gen_rtx_MEM (BLKmode,
	operand4),
	gen_rtx_MEM (BLKmode,
	operand5))),
		gen_rtx_USE (VOIDmode,
	operand6),
		gen_rtx_USE (VOIDmode,
	operand3),
		gen_rtx_USE (VOIDmode,
	gen_rtx_REG (SImode,
	19)),
		gen_rtx_CLOBBER (VOIDmode,
	operand0),
		gen_rtx_CLOBBER (VOIDmode,
	operand1),
		gen_rtx_CLOBBER (VOIDmode,
	operand2)));
}

/* ../../gcc/gcc/config/i386/i386.md:15760 */
rtx
gen_cmpstrqi_1 (operand0, operand1, operand2, operand3, operand4, operand5, operand6)
     rtx operand0;
     rtx operand1;
     rtx operand2;
     rtx operand3;
     rtx operand4;
     rtx operand5;
     rtx operand6;
{
  return gen_rtx_PARALLEL (VOIDmode, gen_rtvec (7,
		gen_rtx_SET (VOIDmode,
	gen_rtx_REG (CCmode,
	17),
	gen_rtx_IF_THEN_ELSE (CCmode,
	gen_rtx_NE (VOIDmode,
	operand6,
	const0_rtx),
	gen_rtx_COMPARE (CCmode,
	gen_rtx_MEM (BLKmode,
	operand4),
	gen_rtx_MEM (BLKmode,
	operand5)),
	const0_rtx)),
		gen_rtx_USE (VOIDmode,
	operand3),
		gen_rtx_USE (VOIDmode,
	gen_rtx_REG (CCmode,
	17)),
		gen_rtx_USE (VOIDmode,
	gen_rtx_REG (SImode,
	19)),
		gen_rtx_CLOBBER (VOIDmode,
	operand0),
		gen_rtx_CLOBBER (VOIDmode,
	operand1),
		gen_rtx_CLOBBER (VOIDmode,
	operand2)));
}

/* ../../gcc/gcc/config/i386/i386.md:15779 */
rtx
gen_cmpstrqi_rex_1 (operand0, operand1, operand2, operand3, operand4, operand5, operand6)
     rtx operand0;
     rtx operand1;
     rtx operand2;
     rtx operand3;
     rtx operand4;
     rtx operand5;
     rtx operand6;
{
  return gen_rtx_PARALLEL (VOIDmode, gen_rtvec (7,
		gen_rtx_SET (VOIDmode,
	gen_rtx_REG (CCmode,
	17),
	gen_rtx_IF_THEN_ELSE (CCmode,
	gen_rtx_NE (VOIDmode,
	operand6,
	const0_rtx),
	gen_rtx_COMPARE (CCmode,
	gen_rtx_MEM (BLKmode,
	operand4),
	gen_rtx_MEM (BLKmode,
	operand5)),
	const0_rtx)),
		gen_rtx_USE (VOIDmode,
	operand3),
		gen_rtx_USE (VOIDmode,
	gen_rtx_REG (CCmode,
	17)),
		gen_rtx_USE (VOIDmode,
	gen_rtx_REG (SImode,
	19)),
		gen_rtx_CLOBBER (VOIDmode,
	operand0),
		gen_rtx_CLOBBER (VOIDmode,
	operand1),
		gen_rtx_CLOBBER (VOIDmode,
	operand2)));
}

/* ../../gcc/gcc/config/i386/i386.md:15824 */
rtx
gen_strlenqi_1 (operand0, operand1, operand2, operand3, operand4, operand5)
     rtx operand0;
     rtx operand1;
     rtx operand2;
     rtx operand3;
     rtx operand4;
     rtx operand5;
{
  return gen_rtx_PARALLEL (VOIDmode, gen_rtvec (4,
		gen_rtx_SET (VOIDmode,
	operand0,
	gen_rtx_UNSPEC (SImode,
	gen_rtvec (4,
		gen_rtx_MEM (BLKmode,
	operand5),
		operand2,
		operand3,
		operand4),
	20)),
		gen_rtx_USE (VOIDmode,
	gen_rtx_REG (SImode,
	19)),
		gen_rtx_CLOBBER (VOIDmode,
	operand1),
		gen_rtx_CLOBBER (VOIDmode,
	gen_rtx_REG (CCmode,
	17))));
}

/* ../../gcc/gcc/config/i386/i386.md:15839 */
rtx
gen_strlenqi_rex_1 (operand0, operand1, operand2, operand3, operand4, operand5)
     rtx operand0;
     rtx operand1;
     rtx operand2;
     rtx operand3;
     rtx operand4;
     rtx operand5;
{
  return gen_rtx_PARALLEL (VOIDmode, gen_rtvec (4,
		gen_rtx_SET (VOIDmode,
	operand0,
	gen_rtx_UNSPEC (DImode,
	gen_rtvec (4,
		gen_rtx_MEM (BLKmode,
	operand5),
		operand2,
		operand3,
		operand4),
	20)),
		gen_rtx_USE (VOIDmode,
	gen_rtx_REG (SImode,
	19)),
		gen_rtx_CLOBBER (VOIDmode,
	operand1),
		gen_rtx_CLOBBER (VOIDmode,
	gen_rtx_REG (CCmode,
	17))));
}

/* ../../gcc/gcc/config/i386/i386.md:15949 */
rtx
gen_x86_movdicc_0_m1_rex64 (operand0)
     rtx operand0;
{
  return gen_rtx_PARALLEL (VOIDmode, gen_rtvec (2,
		gen_rtx_SET (VOIDmode,
	operand0,
	gen_rtx_IF_THEN_ELSE (DImode,
	gen_rtx_LTU (VOIDmode,
	gen_rtx_REG (CCmode,
	17),
	const0_rtx),
	constm1_rtx,
	const0_rtx)),
		gen_rtx_CLOBBER (VOIDmode,
	gen_rtx_REG (CCmode,
	17))));
}

/* ../../gcc/gcc/config/i386/i386.md:15992 */
rtx
gen_x86_movsicc_0_m1 (operand0)
     rtx operand0;
{
  return gen_rtx_PARALLEL (VOIDmode, gen_rtvec (2,
		gen_rtx_SET (VOIDmode,
	operand0,
	gen_rtx_IF_THEN_ELSE (SImode,
	gen_rtx_LTU (VOIDmode,
	gen_rtx_REG (CCmode,
	17),
	const0_rtx),
	constm1_rtx,
	const0_rtx)),
		gen_rtx_CLOBBER (VOIDmode,
	gen_rtx_REG (CCmode,
	17))));
}

/* ../../gcc/gcc/config/i386/i386.md:16565 */
rtx
gen_pro_epilogue_adjust_stack_rex64 (operand0, operand1, operand2)
     rtx operand0;
     rtx operand1;
     rtx operand2;
{
  return gen_rtx_PARALLEL (VOIDmode, gen_rtvec (3,
		gen_rtx_SET (VOIDmode,
	operand0,
	gen_rtx_PLUS (DImode,
	operand1,
	operand2)),
		gen_rtx_CLOBBER (VOIDmode,
	gen_rtx_REG (CCmode,
	17)),
		gen_rtx_CLOBBER (VOIDmode,
	gen_rtx_MEM (BLKmode,
	gen_rtx_SCRATCH (VOIDmode)))));
}

/* ../../gcc/gcc/config/i386/i386.md:16615 */
rtx
gen_sse_movsfcc (operand0, operand1, operand2, operand3, operand4, operand5)
     rtx operand0;
     rtx operand1;
     rtx operand2;
     rtx operand3;
     rtx operand4;
     rtx operand5;
{
  return gen_rtx_PARALLEL (VOIDmode, gen_rtvec (3,
		gen_rtx_SET (VOIDmode,
	operand0,
	gen_rtx_IF_THEN_ELSE (SFmode,
	gen_rtx (GET_CODE (operand1), VOIDmode,
		operand4,
		operand5),
	operand2,
	operand3)),
		gen_rtx_CLOBBER (VOIDmode,
	gen_rtx_SCRATCH (SFmode)),
		gen_rtx_CLOBBER (VOIDmode,
	gen_rtx_REG (CCmode,
	17))));
}

/* ../../gcc/gcc/config/i386/i386.md:16630 */
rtx
gen_sse_movsfcc_eq (operand0, operand1, operand2, operand3, operand4)
     rtx operand0;
     rtx operand1;
     rtx operand2;
     rtx operand3;
     rtx operand4;
{
  return gen_rtx_PARALLEL (VOIDmode, gen_rtvec (3,
		gen_rtx_SET (VOIDmode,
	operand0,
	gen_rtx_IF_THEN_ELSE (SFmode,
	gen_rtx_EQ (VOIDmode,
	operand3,
	operand4),
	operand1,
	operand2)),
		gen_rtx_CLOBBER (VOIDmode,
	gen_rtx_SCRATCH (SFmode)),
		gen_rtx_CLOBBER (VOIDmode,
	gen_rtx_REG (CCmode,
	17))));
}

/* ../../gcc/gcc/config/i386/i386.md:16642 */
rtx
gen_sse_movdfcc (operand0, operand1, operand2, operand3, operand4, operand5)
     rtx operand0;
     rtx operand1;
     rtx operand2;
     rtx operand3;
     rtx operand4;
     rtx operand5;
{
  return gen_rtx_PARALLEL (VOIDmode, gen_rtvec (3,
		gen_rtx_SET (VOIDmode,
	operand0,
	gen_rtx_IF_THEN_ELSE (DFmode,
	gen_rtx (GET_CODE (operand1), VOIDmode,
		operand4,
		operand5),
	operand2,
	operand3)),
		gen_rtx_CLOBBER (VOIDmode,
	gen_rtx_SCRATCH (DFmode)),
		gen_rtx_CLOBBER (VOIDmode,
	gen_rtx_REG (CCmode,
	17))));
}

/* ../../gcc/gcc/config/i386/i386.md:16657 */
rtx
gen_sse_movdfcc_eq (operand0, operand1, operand2, operand3, operand4)
     rtx operand0;
     rtx operand1;
     rtx operand2;
     rtx operand3;
     rtx operand4;
{
  return gen_rtx_PARALLEL (VOIDmode, gen_rtvec (3,
		gen_rtx_SET (VOIDmode,
	operand0,
	gen_rtx_IF_THEN_ELSE (DFmode,
	gen_rtx_EQ (VOIDmode,
	operand3,
	operand4),
	operand1,
	operand2)),
		gen_rtx_CLOBBER (VOIDmode,
	gen_rtx_SCRATCH (DFmode)),
		gen_rtx_CLOBBER (VOIDmode,
	gen_rtx_REG (CCmode,
	17))));
}

/* ../../gcc/gcc/config/i386/i386.md:16859 */
rtx
gen_allocate_stack_worker_1 (operand0)
     rtx operand0;
{
  return gen_rtx_PARALLEL (VOIDmode, gen_rtvec (4,
		gen_rtx_UNSPEC (SImode,
	gen_rtvec (1,
		operand0),
	10),
		gen_rtx_SET (VOIDmode,
	gen_rtx_REG (SImode,
	7),
	gen_rtx_MINUS (SImode,
	gen_rtx_REG (SImode,
	7),
	operand0)),
		gen_rtx_CLOBBER (VOIDmode,
	operand0),
		gen_rtx_CLOBBER (VOIDmode,
	gen_rtx_REG (CCmode,
	17))));
}

/* ../../gcc/gcc/config/i386/i386.md:16869 */
rtx
gen_allocate_stack_worker_rex64 (operand0)
     rtx operand0;
{
  return gen_rtx_PARALLEL (VOIDmode, gen_rtvec (4,
		gen_rtx_UNSPEC (DImode,
	gen_rtvec (1,
		operand0),
	10),
		gen_rtx_SET (VOIDmode,
	gen_rtx_REG (DImode,
	7),
	gen_rtx_MINUS (DImode,
	gen_rtx_REG (DImode,
	7),
	operand0)),
		gen_rtx_CLOBBER (VOIDmode,
	operand0),
		gen_rtx_CLOBBER (VOIDmode,
	gen_rtx_REG (CCmode,
	17))));
}

/* ../../gcc/gcc/config/i386/i386.md:17883 */
rtx
gen_trap ()
{
  return gen_rtx_TRAP_IF (VOIDmode,
	const1_rtx,
	GEN_INT (5LL));
}

/* ../../gcc/gcc/config/i386/i386.md:17935 */
rtx
gen_movv4sf_internal (operand0, operand1)
     rtx operand0;
     rtx operand1;
{
  return gen_rtx_SET (VOIDmode,
	operand0,
	operand1);
}

/* ../../gcc/gcc/config/i386/i386.md:17947 */
rtx
gen_movv4si_internal (operand0, operand1)
     rtx operand0;
     rtx operand1;
{
  return gen_rtx_SET (VOIDmode,
	operand0,
	operand1);
}

/* ../../gcc/gcc/config/i386/i386.md:17959 */
rtx
gen_movv2di_internal (operand0, operand1)
     rtx operand0;
     rtx operand1;
{
  return gen_rtx_SET (VOIDmode,
	operand0,
	operand1);
}

/* ../../gcc/gcc/config/i386/i386.md:17971 */
rtx
gen_movv8qi_internal (operand0, operand1)
     rtx operand0;
     rtx operand1;
{
  return gen_rtx_SET (VOIDmode,
	operand0,
	operand1);
}

/* ../../gcc/gcc/config/i386/i386.md:17983 */
rtx
gen_movv4hi_internal (operand0, operand1)
     rtx operand0;
     rtx operand1;
{
  return gen_rtx_SET (VOIDmode,
	operand0,
	operand1);
}

/* ../../gcc/gcc/config/i386/i386.md:17995 */
rtx
gen_movv2si_internal (operand0, operand1)
     rtx operand0;
     rtx operand1;
{
  return gen_rtx_SET (VOIDmode,
	operand0,
	operand1);
}

/* ../../gcc/gcc/config/i386/i386.md:18007 */
rtx
gen_movv2sf_internal (operand0, operand1)
     rtx operand0;
     rtx operand1;
{
  return gen_rtx_SET (VOIDmode,
	operand0,
	operand1);
}

/* ../../gcc/gcc/config/i386/i386.md:18031 */
rtx
gen_movv2df_internal (operand0, operand1)
     rtx operand0;
     rtx operand1;
{
  return gen_rtx_SET (VOIDmode,
	operand0,
	operand1);
}

/* ../../gcc/gcc/config/i386/i386.md:18043 */
rtx
gen_movv8hi_internal (operand0, operand1)
     rtx operand0;
     rtx operand1;
{
  return gen_rtx_SET (VOIDmode,
	operand0,
	operand1);
}

/* ../../gcc/gcc/config/i386/i386.md:18055 */
rtx
gen_movv16qi_internal (operand0, operand1)
     rtx operand0;
     rtx operand1;
{
  return gen_rtx_SET (VOIDmode,
	operand0,
	operand1);
}

/* ../../gcc/gcc/config/i386/i386.md:18361 */
rtx
gen_movti_internal (operand0, operand1)
     rtx operand0;
     rtx operand1;
{
  return gen_rtx_SET (VOIDmode,
	operand0,
	operand1);
}

/* ../../gcc/gcc/config/i386/i386.md:18449 */
rtx
gen_sse_movmskps (operand0, operand1)
     rtx operand0;
     rtx operand1;
{
  return gen_rtx_SET (VOIDmode,
	operand0,
	gen_rtx_UNSPEC (SImode,
	gen_rtvec (1,
		operand1),
	33));
}

/* ../../gcc/gcc/config/i386/i386.md:18458 */
rtx
gen_mmx_pmovmskb (operand0, operand1)
     rtx operand0;
     rtx operand1;
{
  return gen_rtx_SET (VOIDmode,
	operand0,
	gen_rtx_UNSPEC (SImode,
	gen_rtvec (1,
		operand1),
	33));
}

/* ../../gcc/gcc/config/i386/i386.md:18468 */
rtx
gen_mmx_maskmovq (operand0, operand1, operand2)
     rtx operand0;
     rtx operand1;
     rtx operand2;
{
  return gen_rtx_SET (VOIDmode,
	gen_rtx_MEM (V8QImode,
	operand0),
	gen_rtx_UNSPEC (V8QImode,
	gen_rtvec (2,
		operand1,
		operand2),
	32));
}

/* ../../gcc/gcc/config/i386/i386.md:18479 */
rtx
gen_mmx_maskmovq_rex (operand0, operand1, operand2)
     rtx operand0;
     rtx operand1;
     rtx operand2;
{
  return gen_rtx_SET (VOIDmode,
	gen_rtx_MEM (V8QImode,
	operand0),
	gen_rtx_UNSPEC (V8QImode,
	gen_rtvec (2,
		operand1,
		operand2),
	32));
}

/* ../../gcc/gcc/config/i386/i386.md:18490 */
rtx
gen_sse_movntv4sf (operand0, operand1)
     rtx operand0;
     rtx operand1;
{
  return gen_rtx_SET (VOIDmode,
	operand0,
	gen_rtx_UNSPEC (V4SFmode,
	gen_rtvec (1,
		operand1),
	34));
}

/* ../../gcc/gcc/config/i386/i386.md:18499 */
rtx
gen_sse_movntdi (operand0, operand1)
     rtx operand0;
     rtx operand1;
{
  return gen_rtx_SET (VOIDmode,
	operand0,
	gen_rtx_UNSPEC (DImode,
	gen_rtvec (1,
		operand1),
	34));
}

/* ../../gcc/gcc/config/i386/i386.md:18508 */
rtx
gen_sse_movhlps (operand0, operand1, operand2)
     rtx operand0;
     rtx operand1;
     rtx operand2;
{
  return gen_rtx_SET (VOIDmode,
	operand0,
	gen_rtx_VEC_MERGE (V4SFmode,
	operand1,
	gen_rtx_VEC_SELECT (V4SFmode,
	operand2,
	gen_rtx_PARALLEL (VOIDmode,
	gen_rtvec (4,
		GEN_INT (2LL),
		GEN_INT (3LL),
		const0_rtx,
		const1_rtx))),
	GEN_INT (3LL)));
}

/* ../../gcc/gcc/config/i386/i386.md:18523 */
rtx
gen_sse_movlhps (operand0, operand1, operand2)
     rtx operand0;
     rtx operand1;
     rtx operand2;
{
  return gen_rtx_SET (VOIDmode,
	operand0,
	gen_rtx_VEC_MERGE (V4SFmode,
	operand1,
	gen_rtx_VEC_SELECT (V4SFmode,
	operand2,
	gen_rtx_PARALLEL (VOIDmode,
	gen_rtvec (4,
		GEN_INT (2LL),
		GEN_INT (3LL),
		const0_rtx,
		const1_rtx))),
	GEN_INT (12LL)));
}

/* ../../gcc/gcc/config/i386/i386.md:18538 */
rtx
gen_sse_movhps (operand0, operand1, operand2)
     rtx operand0;
     rtx operand1;
     rtx operand2;
{
  return gen_rtx_SET (VOIDmode,
	operand0,
	gen_rtx_VEC_MERGE (V4SFmode,
	operand1,
	operand2,
	GEN_INT (12LL)));
}

/* ../../gcc/gcc/config/i386/i386.md:18550 */
rtx
gen_sse_movlps (operand0, operand1, operand2)
     rtx operand0;
     rtx operand1;
     rtx operand2;
{
  return gen_rtx_SET (VOIDmode,
	operand0,
	gen_rtx_VEC_MERGE (V4SFmode,
	operand1,
	operand2,
	GEN_INT (3LL)));
}

/* ../../gcc/gcc/config/i386/i386.md:18572 */
rtx
gen_sse_loadss_1 (operand0, operand1, operand2)
     rtx operand0;
     rtx operand1;
     rtx operand2;
{
  return gen_rtx_SET (VOIDmode,
	operand0,
	gen_rtx_VEC_MERGE (V4SFmode,
	gen_rtx_VEC_DUPLICATE (V4SFmode,
	operand1),
	operand2,
	const1_rtx));
}

/* ../../gcc/gcc/config/i386/i386.md:18583 */
rtx
gen_sse_movss (operand0, operand1, operand2)
     rtx operand0;
     rtx operand1;
     rtx operand2;
{
  return gen_rtx_SET (VOIDmode,
	operand0,
	gen_rtx_VEC_MERGE (V4SFmode,
	operand1,
	operand2,
	const1_rtx));
}

/* ../../gcc/gcc/config/i386/i386.md:18594 */
rtx
gen_sse_storess (operand0, operand1)
     rtx operand0;
     rtx operand1;
{
  return gen_rtx_SET (VOIDmode,
	operand0,
	gen_rtx_VEC_SELECT (SFmode,
	operand1,
	gen_rtx_PARALLEL (VOIDmode,
	gen_rtvec (1,
		const0_rtx))));
}

/* ../../gcc/gcc/config/i386/i386.md:18604 */
rtx
gen_sse_shufps (operand0, operand1, operand2, operand3)
     rtx operand0;
     rtx operand1;
     rtx operand2;
     rtx operand3;
{
  return gen_rtx_SET (VOIDmode,
	operand0,
	gen_rtx_UNSPEC (V4SFmode,
	gen_rtvec (3,
		operand1,
		operand2,
		operand3),
	41));
}

/* ../../gcc/gcc/config/i386/i386.md:18619 */
rtx
gen_addv4sf3 (operand0, operand1, operand2)
     rtx operand0;
     rtx operand1;
     rtx operand2;
{
  return gen_rtx_SET (VOIDmode,
	operand0,
	gen_rtx_PLUS (V4SFmode,
	operand1,
	operand2));
}

/* ../../gcc/gcc/config/i386/i386.md:18628 */
rtx
gen_vmaddv4sf3 (operand0, operand1, operand2)
     rtx operand0;
     rtx operand1;
     rtx operand2;
{
  return gen_rtx_SET (VOIDmode,
	operand0,
	gen_rtx_VEC_MERGE (V4SFmode,
	gen_rtx_PLUS (V4SFmode,
	operand1,
	operand2),
	operand1,
	const1_rtx));
}

/* ../../gcc/gcc/config/i386/i386.md:18640 */
rtx
gen_subv4sf3 (operand0, operand1, operand2)
     rtx operand0;
     rtx operand1;
     rtx operand2;
{
  return gen_rtx_SET (VOIDmode,
	operand0,
	gen_rtx_MINUS (V4SFmode,
	operand1,
	operand2));
}

/* ../../gcc/gcc/config/i386/i386.md:18649 */
rtx
gen_vmsubv4sf3 (operand0, operand1, operand2)
     rtx operand0;
     rtx operand1;
     rtx operand2;
{
  return gen_rtx_SET (VOIDmode,
	operand0,
	gen_rtx_VEC_MERGE (V4SFmode,
	gen_rtx_MINUS (V4SFmode,
	operand1,
	operand2),
	operand1,
	const1_rtx));
}

/* ../../gcc/gcc/config/i386/i386.md:18661 */
rtx
gen_mulv4sf3 (operand0, operand1, operand2)
     rtx operand0;
     rtx operand1;
     rtx operand2;
{
  return gen_rtx_SET (VOIDmode,
	operand0,
	gen_rtx_MULT (V4SFmode,
	operand1,
	operand2));
}

/* ../../gcc/gcc/config/i386/i386.md:18670 */
rtx
gen_vmmulv4sf3 (operand0, operand1, operand2)
     rtx operand0;
     rtx operand1;
     rtx operand2;
{
  return gen_rtx_SET (VOIDmode,
	operand0,
	gen_rtx_VEC_MERGE (V4SFmode,
	gen_rtx_MULT (V4SFmode,
	operand1,
	operand2),
	operand1,
	const1_rtx));
}

/* ../../gcc/gcc/config/i386/i386.md:18682 */
rtx
gen_divv4sf3 (operand0, operand1, operand2)
     rtx operand0;
     rtx operand1;
     rtx operand2;
{
  return gen_rtx_SET (VOIDmode,
	operand0,
	gen_rtx_DIV (V4SFmode,
	operand1,
	operand2));
}

/* ../../gcc/gcc/config/i386/i386.md:18691 */
rtx
gen_vmdivv4sf3 (operand0, operand1, operand2)
     rtx operand0;
     rtx operand1;
     rtx operand2;
{
  return gen_rtx_SET (VOIDmode,
	operand0,
	gen_rtx_VEC_MERGE (V4SFmode,
	gen_rtx_DIV (V4SFmode,
	operand1,
	operand2),
	operand1,
	const1_rtx));
}

/* ../../gcc/gcc/config/i386/i386.md:18706 */
rtx
gen_rcpv4sf2 (operand0, operand1)
     rtx operand0;
     rtx operand1;
{
  return gen_rtx_SET (VOIDmode,
	operand0,
	gen_rtx_UNSPEC (V4SFmode,
	gen_rtvec (1,
		operand1),
	42));
}

/* ../../gcc/gcc/config/i386/i386.md:18715 */
rtx
gen_vmrcpv4sf2 (operand0, operand1, operand2)
     rtx operand0;
     rtx operand1;
     rtx operand2;
{
  return gen_rtx_SET (VOIDmode,
	operand0,
	gen_rtx_VEC_MERGE (V4SFmode,
	gen_rtx_UNSPEC (V4SFmode,
	gen_rtvec (1,
		operand1),
	42),
	operand2,
	const1_rtx));
}

/* ../../gcc/gcc/config/i386/i386.md:18727 */
rtx
gen_rsqrtv4sf2 (operand0, operand1)
     rtx operand0;
     rtx operand1;
{
  return gen_rtx_SET (VOIDmode,
	operand0,
	gen_rtx_UNSPEC (V4SFmode,
	gen_rtvec (1,
		operand1),
	43));
}

/* ../../gcc/gcc/config/i386/i386.md:18736 */
rtx
gen_vmrsqrtv4sf2 (operand0, operand1, operand2)
     rtx operand0;
     rtx operand1;
     rtx operand2;
{
  return gen_rtx_SET (VOIDmode,
	operand0,
	gen_rtx_VEC_MERGE (V4SFmode,
	gen_rtx_UNSPEC (V4SFmode,
	gen_rtvec (1,
		operand1),
	43),
	operand2,
	const1_rtx));
}

/* ../../gcc/gcc/config/i386/i386.md:18748 */
rtx
gen_sqrtv4sf2 (operand0, operand1)
     rtx operand0;
     rtx operand1;
{
  return gen_rtx_SET (VOIDmode,
	operand0,
	gen_rtx_SQRT (V4SFmode,
	operand1));
}

/* ../../gcc/gcc/config/i386/i386.md:18756 */
rtx
gen_vmsqrtv4sf2 (operand0, operand1, operand2)
     rtx operand0;
     rtx operand1;
     rtx operand2;
{
  return gen_rtx_SET (VOIDmode,
	operand0,
	gen_rtx_VEC_MERGE (V4SFmode,
	gen_rtx_SQRT (V4SFmode,
	operand1),
	operand2,
	const1_rtx));
}

/* ../../gcc/gcc/config/i386/i386.md:19026 */
rtx
gen_sse2_andv2di3 (operand0, operand1, operand2)
     rtx operand0;
     rtx operand1;
     rtx operand2;
{
  return gen_rtx_SET (VOIDmode,
	operand0,
	gen_rtx_AND (V2DImode,
	operand1,
	operand2));
}

/* ../../gcc/gcc/config/i386/i386.md:19045 */
rtx
gen_sse2_nandv2di3 (operand0, operand1, operand2)
     rtx operand0;
     rtx operand1;
     rtx operand2;
{
  return gen_rtx_SET (VOIDmode,
	operand0,
	gen_rtx_AND (V2DImode,
	gen_rtx_NOT (V2DImode,
	operand1),
	operand2));
}

/* ../../gcc/gcc/config/i386/i386.md:19065 */
rtx
gen_sse2_iorv2di3 (operand0, operand1, operand2)
     rtx operand0;
     rtx operand1;
     rtx operand2;
{
  return gen_rtx_SET (VOIDmode,
	operand0,
	gen_rtx_IOR (V2DImode,
	operand1,
	operand2));
}

/* ../../gcc/gcc/config/i386/i386.md:19085 */
rtx
gen_sse2_xorv2di3 (operand0, operand1, operand2)
     rtx operand0;
     rtx operand1;
     rtx operand2;
{
  return gen_rtx_SET (VOIDmode,
	operand0,
	gen_rtx_XOR (V2DImode,
	operand1,
	operand2));
}

/* ../../gcc/gcc/config/i386/i386.md:19097 */
rtx
gen_sse_clrv4sf (operand0)
     rtx operand0;
{
  return gen_rtx_SET (VOIDmode,
	operand0,
	gen_rtx_UNSPEC (V4SFmode,
	gen_rtvec (1,
		const0_rtx),
	45));
}

/* ../../gcc/gcc/config/i386/i386.md:19108 */
rtx
gen_sse_clrv2df (operand0)
     rtx operand0;
{
  return gen_rtx_SET (VOIDmode,
	operand0,
	gen_rtx_UNSPEC (V2DFmode,
	gen_rtvec (1,
		const0_rtx),
	45));
}

/* ../../gcc/gcc/config/i386/i386.md:19119 */
rtx
gen_maskcmpv4sf3 (operand0, operand1, operand2, operand3)
     rtx operand0;
     rtx operand1;
     rtx operand2;
     rtx operand3;
{
  return gen_rtx_SET (VOIDmode,
	operand0,
	gen_rtx (GET_CODE (operand3), V4SImode,
		operand1,
		operand2));
}

/* ../../gcc/gcc/config/i386/i386.md:19129 */
rtx
gen_maskncmpv4sf3 (operand0, operand1, operand2, operand3)
     rtx operand0;
     rtx operand1;
     rtx operand2;
     rtx operand3;
{
  return gen_rtx_SET (VOIDmode,
	operand0,
	gen_rtx_NOT (V4SImode,
	gen_rtx (GET_CODE (operand3), V4SImode,
		operand1,
		operand2)));
}

/* ../../gcc/gcc/config/i386/i386.md:19145 */
rtx
gen_vmmaskcmpv4sf3 (operand0, operand1, operand2, operand3)
     rtx operand0;
     rtx operand1;
     rtx operand2;
     rtx operand3;
{
  return gen_rtx_SET (VOIDmode,
	operand0,
	gen_rtx_VEC_MERGE (V4SImode,
	gen_rtx (GET_CODE (operand3), V4SImode,
		operand1,
		operand2),
	gen_rtx_SUBREG (V4SImode,
	operand1,
	0),
	const1_rtx));
}

/* ../../gcc/gcc/config/i386/i386.md:19158 */
rtx
gen_vmmaskncmpv4sf3 (operand0, operand1, operand2, operand3)
     rtx operand0;
     rtx operand1;
     rtx operand2;
     rtx operand3;
{
  return gen_rtx_SET (VOIDmode,
	operand0,
	gen_rtx_VEC_MERGE (V4SImode,
	gen_rtx_NOT (V4SImode,
	gen_rtx (GET_CODE (operand3), V4SImode,
		operand1,
		operand2)),
	gen_rtx_SUBREG (V4SImode,
	operand1,
	0),
	const1_rtx));
}

/* ../../gcc/gcc/config/i386/i386.md:19177 */
rtx
gen_sse_comi (operand0, operand1)
     rtx operand0;
     rtx operand1;
{
  return gen_rtx_SET (VOIDmode,
	gen_rtx_REG (CCFPmode,
	17),
	gen_rtx_COMPARE (CCFPmode,
	gen_rtx_VEC_SELECT (SFmode,
	operand0,
	gen_rtx_PARALLEL (VOIDmode,
	gen_rtvec (1,
		const0_rtx))),
	gen_rtx_VEC_SELECT (SFmode,
	operand1,
	gen_rtx_PARALLEL (VOIDmode,
	gen_rtvec (1,
		const0_rtx)))));
}

/* ../../gcc/gcc/config/i386/i386.md:19190 */
rtx
gen_sse_ucomi (operand0, operand1)
     rtx operand0;
     rtx operand1;
{
  return gen_rtx_SET (VOIDmode,
	gen_rtx_REG (CCFPUmode,
	17),
	gen_rtx_COMPARE (CCFPUmode,
	gen_rtx_VEC_SELECT (SFmode,
	operand0,
	gen_rtx_PARALLEL (VOIDmode,
	gen_rtvec (1,
		const0_rtx))),
	gen_rtx_VEC_SELECT (SFmode,
	operand1,
	gen_rtx_PARALLEL (VOIDmode,
	gen_rtvec (1,
		const0_rtx)))));
}

/* ../../gcc/gcc/config/i386/i386.md:19206 */
rtx
gen_sse_unpckhps (operand0, operand1, operand2)
     rtx operand0;
     rtx operand1;
     rtx operand2;
{
  return gen_rtx_SET (VOIDmode,
	operand0,
	gen_rtx_VEC_MERGE (V4SFmode,
	gen_rtx_VEC_SELECT (V4SFmode,
	operand1,
	gen_rtx_PARALLEL (VOIDmode,
	gen_rtvec (4,
		GEN_INT (2LL),
		const0_rtx,
		GEN_INT (3LL),
		const1_rtx))),
	gen_rtx_VEC_SELECT (V4SFmode,
	operand2,
	gen_rtx_PARALLEL (VOIDmode,
	gen_rtvec (4,
		const0_rtx,
		GEN_INT (2LL),
		const1_rtx,
		GEN_INT (3LL)))),
	GEN_INT (5LL)));
}

/* ../../gcc/gcc/config/i386/i386.md:19225 */
rtx
gen_sse_unpcklps (operand0, operand1, operand2)
     rtx operand0;
     rtx operand1;
     rtx operand2;
{
  return gen_rtx_SET (VOIDmode,
	operand0,
	gen_rtx_VEC_MERGE (V4SFmode,
	gen_rtx_VEC_SELECT (V4SFmode,
	operand1,
	gen_rtx_PARALLEL (VOIDmode,
	gen_rtvec (4,
		const0_rtx,
		GEN_INT (2LL),
		const1_rtx,
		GEN_INT (3LL)))),
	gen_rtx_VEC_SELECT (V4SFmode,
	operand2,
	gen_rtx_PARALLEL (VOIDmode,
	gen_rtvec (4,
		GEN_INT (2LL),
		const0_rtx,
		GEN_INT (3LL),
		const1_rtx))),
	GEN_INT (5LL)));
}

/* ../../gcc/gcc/config/i386/i386.md:19247 */
rtx
gen_smaxv4sf3 (operand0, operand1, operand2)
     rtx operand0;
     rtx operand1;
     rtx operand2;
{
  return gen_rtx_SET (VOIDmode,
	operand0,
	gen_rtx_SMAX (V4SFmode,
	operand1,
	operand2));
}

/* ../../gcc/gcc/config/i386/i386.md:19256 */
rtx
gen_vmsmaxv4sf3 (operand0, operand1, operand2)
     rtx operand0;
     rtx operand1;
     rtx operand2;
{
  return gen_rtx_SET (VOIDmode,
	operand0,
	gen_rtx_VEC_MERGE (V4SFmode,
	gen_rtx_SMAX (V4SFmode,
	operand1,
	operand2),
	operand1,
	const1_rtx));
}

/* ../../gcc/gcc/config/i386/i386.md:19268 */
rtx
gen_sminv4sf3 (operand0, operand1, operand2)
     rtx operand0;
     rtx operand1;
     rtx operand2;
{
  return gen_rtx_SET (VOIDmode,
	operand0,
	gen_rtx_SMIN (V4SFmode,
	operand1,
	operand2));
}

/* ../../gcc/gcc/config/i386/i386.md:19277 */
rtx
gen_vmsminv4sf3 (operand0, operand1, operand2)
     rtx operand0;
     rtx operand1;
     rtx operand2;
{
  return gen_rtx_SET (VOIDmode,
	operand0,
	gen_rtx_VEC_MERGE (V4SFmode,
	gen_rtx_SMIN (V4SFmode,
	operand1,
	operand2),
	operand1,
	const1_rtx));
}

/* ../../gcc/gcc/config/i386/i386.md:19291 */
rtx
gen_cvtpi2ps (operand0, operand1, operand2)
     rtx operand0;
     rtx operand1;
     rtx operand2;
{
  return gen_rtx_SET (VOIDmode,
	operand0,
	gen_rtx_VEC_MERGE (V4SFmode,
	operand1,
	gen_rtx_VEC_DUPLICATE (V4SFmode,
	gen_rtx_FLOAT (V2SFmode,
	operand2)),
	GEN_INT (12LL)));
}

/* ../../gcc/gcc/config/i386/i386.md:19303 */
rtx
gen_cvtps2pi (operand0, operand1)
     rtx operand0;
     rtx operand1;
{
  return gen_rtx_SET (VOIDmode,
	operand0,
	gen_rtx_VEC_SELECT (V2SImode,
	gen_rtx_FIX (V4SImode,
	operand1),
	gen_rtx_PARALLEL (VOIDmode,
	gen_rtvec (2,
		const0_rtx,
		const1_rtx))));
}

/* ../../gcc/gcc/config/i386/i386.md:19313 */
rtx
gen_cvttps2pi (operand0, operand1)
     rtx operand0;
     rtx operand1;
{
  return gen_rtx_SET (VOIDmode,
	operand0,
	gen_rtx_VEC_SELECT (V2SImode,
	gen_rtx_UNSPEC (V4SImode,
	gen_rtvec (1,
		operand1),
	30),
	gen_rtx_PARALLEL (VOIDmode,
	gen_rtvec (2,
		const0_rtx,
		const1_rtx))));
}

/* ../../gcc/gcc/config/i386/i386.md:19324 */
rtx
gen_cvtsi2ss (operand0, operand1, operand2)
     rtx operand0;
     rtx operand1;
     rtx operand2;
{
  return gen_rtx_SET (VOIDmode,
	operand0,
	gen_rtx_VEC_MERGE (V4SFmode,
	operand1,
	gen_rtx_VEC_DUPLICATE (V4SFmode,
	gen_rtx_FLOAT (SFmode,
	operand2)),
	GEN_INT (14LL)));
}

/* ../../gcc/gcc/config/i386/i386.md:19336 */
rtx
gen_cvtsi2ssq (operand0, operand1, operand2)
     rtx operand0;
     rtx operand1;
     rtx operand2;
{
  return gen_rtx_SET (VOIDmode,
	operand0,
	gen_rtx_VEC_MERGE (V4SFmode,
	operand1,
	gen_rtx_VEC_DUPLICATE (V4SFmode,
	gen_rtx_FLOAT (SFmode,
	operand2)),
	GEN_INT (14LL)));
}

/* ../../gcc/gcc/config/i386/i386.md:19349 */
rtx
gen_cvtss2si (operand0, operand1)
     rtx operand0;
     rtx operand1;
{
  return gen_rtx_SET (VOIDmode,
	operand0,
	gen_rtx_VEC_SELECT (SImode,
	gen_rtx_FIX (V4SImode,
	operand1),
	gen_rtx_PARALLEL (VOIDmode,
	gen_rtvec (1,
		const0_rtx))));
}

/* ../../gcc/gcc/config/i386/i386.md:19359 */
rtx
gen_cvtss2siq (operand0, operand1)
     rtx operand0;
     rtx operand1;
{
  return gen_rtx_SET (VOIDmode,
	operand0,
	gen_rtx_VEC_SELECT (DImode,
	gen_rtx_FIX (V4DImode,
	operand1),
	gen_rtx_PARALLEL (VOIDmode,
	gen_rtvec (1,
		const0_rtx))));
}

/* ../../gcc/gcc/config/i386/i386.md:19370 */
rtx
gen_cvttss2si (operand0, operand1)
     rtx operand0;
     rtx operand1;
{
  return gen_rtx_SET (VOIDmode,
	operand0,
	gen_rtx_VEC_SELECT (SImode,
	gen_rtx_UNSPEC (V4SImode,
	gen_rtvec (1,
		operand1),
	30),
	gen_rtx_PARALLEL (VOIDmode,
	gen_rtvec (1,
		const0_rtx))));
}

/* ../../gcc/gcc/config/i386/i386.md:19381 */
rtx
gen_cvttss2siq (operand0, operand1)
     rtx operand0;
     rtx operand1;
{
  return gen_rtx_SET (VOIDmode,
	operand0,
	gen_rtx_VEC_SELECT (DImode,
	gen_rtx_UNSPEC (V4DImode,
	gen_rtvec (1,
		operand1),
	30),
	gen_rtx_PARALLEL (VOIDmode,
	gen_rtvec (1,
		const0_rtx))));
}

/* ../../gcc/gcc/config/i386/i386.md:19398 */
rtx
gen_addv8qi3 (operand0, operand1, operand2)
     rtx operand0;
     rtx operand1;
     rtx operand2;
{
  return gen_rtx_SET (VOIDmode,
	operand0,
	gen_rtx_PLUS (V8QImode,
	operand1,
	operand2));
}

/* ../../gcc/gcc/config/i386/i386.md:19407 */
rtx
gen_addv4hi3 (operand0, operand1, operand2)
     rtx operand0;
     rtx operand1;
     rtx operand2;
{
  return gen_rtx_SET (VOIDmode,
	operand0,
	gen_rtx_PLUS (V4HImode,
	operand1,
	operand2));
}

/* ../../gcc/gcc/config/i386/i386.md:19416 */
rtx
gen_addv2si3 (operand0, operand1, operand2)
     rtx operand0;
     rtx operand1;
     rtx operand2;
{
  return gen_rtx_SET (VOIDmode,
	operand0,
	gen_rtx_PLUS (V2SImode,
	operand1,
	operand2));
}

/* ../../gcc/gcc/config/i386/i386.md:19425 */
rtx
gen_mmx_adddi3 (operand0, operand1, operand2)
     rtx operand0;
     rtx operand1;
     rtx operand2;
{
  return gen_rtx_SET (VOIDmode,
	operand0,
	gen_rtx_UNSPEC (DImode,
	gen_rtvec (1,
		gen_rtx_PLUS (DImode,
	operand1,
	operand2)),
	45));
}

/* ../../gcc/gcc/config/i386/i386.md:19436 */
rtx
gen_ssaddv8qi3 (operand0, operand1, operand2)
     rtx operand0;
     rtx operand1;
     rtx operand2;
{
  return gen_rtx_SET (VOIDmode,
	operand0,
	gen_rtx_SS_PLUS (V8QImode,
	operand1,
	operand2));
}

/* ../../gcc/gcc/config/i386/i386.md:19445 */
rtx
gen_ssaddv4hi3 (operand0, operand1, operand2)
     rtx operand0;
     rtx operand1;
     rtx operand2;
{
  return gen_rtx_SET (VOIDmode,
	operand0,
	gen_rtx_SS_PLUS (V4HImode,
	operand1,
	operand2));
}

/* ../../gcc/gcc/config/i386/i386.md:19454 */
rtx
gen_usaddv8qi3 (operand0, operand1, operand2)
     rtx operand0;
     rtx operand1;
     rtx operand2;
{
  return gen_rtx_SET (VOIDmode,
	operand0,
	gen_rtx_US_PLUS (V8QImode,
	operand1,
	operand2));
}

/* ../../gcc/gcc/config/i386/i386.md:19463 */
rtx
gen_usaddv4hi3 (operand0, operand1, operand2)
     rtx operand0;
     rtx operand1;
     rtx operand2;
{
  return gen_rtx_SET (VOIDmode,
	operand0,
	gen_rtx_US_PLUS (V4HImode,
	operand1,
	operand2));
}

/* ../../gcc/gcc/config/i386/i386.md:19472 */
rtx
gen_subv8qi3 (operand0, operand1, operand2)
     rtx operand0;
     rtx operand1;
     rtx operand2;
{
  return gen_rtx_SET (VOIDmode,
	operand0,
	gen_rtx_MINUS (V8QImode,
	operand1,
	operand2));
}

/* ../../gcc/gcc/config/i386/i386.md:19481 */
rtx
gen_subv4hi3 (operand0, operand1, operand2)
     rtx operand0;
     rtx operand1;
     rtx operand2;
{
  return gen_rtx_SET (VOIDmode,
	operand0,
	gen_rtx_MINUS (V4HImode,
	operand1,
	operand2));
}

/* ../../gcc/gcc/config/i386/i386.md:19490 */
rtx
gen_subv2si3 (operand0, operand1, operand2)
     rtx operand0;
     rtx operand1;
     rtx operand2;
{
  return gen_rtx_SET (VOIDmode,
	operand0,
	gen_rtx_MINUS (V2SImode,
	operand1,
	operand2));
}

/* ../../gcc/gcc/config/i386/i386.md:19499 */
rtx
gen_mmx_subdi3 (operand0, operand1, operand2)
     rtx operand0;
     rtx operand1;
     rtx operand2;
{
  return gen_rtx_SET (VOIDmode,
	operand0,
	gen_rtx_UNSPEC (DImode,
	gen_rtvec (1,
		gen_rtx_MINUS (DImode,
	operand1,
	operand2)),
	45));
}

/* ../../gcc/gcc/config/i386/i386.md:19510 */
rtx
gen_sssubv8qi3 (operand0, operand1, operand2)
     rtx operand0;
     rtx operand1;
     rtx operand2;
{
  return gen_rtx_SET (VOIDmode,
	operand0,
	gen_rtx_SS_MINUS (V8QImode,
	operand1,
	operand2));
}

/* ../../gcc/gcc/config/i386/i386.md:19519 */
rtx
gen_sssubv4hi3 (operand0, operand1, operand2)
     rtx operand0;
     rtx operand1;
     rtx operand2;
{
  return gen_rtx_SET (VOIDmode,
	operand0,
	gen_rtx_SS_MINUS (V4HImode,
	operand1,
	operand2));
}

/* ../../gcc/gcc/config/i386/i386.md:19528 */
rtx
gen_ussubv8qi3 (operand0, operand1, operand2)
     rtx operand0;
     rtx operand1;
     rtx operand2;
{
  return gen_rtx_SET (VOIDmode,
	operand0,
	gen_rtx_US_MINUS (V8QImode,
	operand1,
	operand2));
}

/* ../../gcc/gcc/config/i386/i386.md:19537 */
rtx
gen_ussubv4hi3 (operand0, operand1, operand2)
     rtx operand0;
     rtx operand1;
     rtx operand2;
{
  return gen_rtx_SET (VOIDmode,
	operand0,
	gen_rtx_US_MINUS (V4HImode,
	operand1,
	operand2));
}

/* ../../gcc/gcc/config/i386/i386.md:19546 */
rtx
gen_mulv4hi3 (operand0, operand1, operand2)
     rtx operand0;
     rtx operand1;
     rtx operand2;
{
  return gen_rtx_SET (VOIDmode,
	operand0,
	gen_rtx_MULT (V4HImode,
	operand1,
	operand2));
}

/* ../../gcc/gcc/config/i386/i386.md:19555 */
rtx
gen_smulv4hi3_highpart (operand0, operand1, operand2)
     rtx operand0;
     rtx operand1;
     rtx operand2;
{
  return gen_rtx_SET (VOIDmode,
	operand0,
	gen_rtx_TRUNCATE (V4HImode,
	gen_rtx_LSHIFTRT (V4SImode,
	gen_rtx_MULT (V4SImode,
	gen_rtx_SIGN_EXTEND (V4SImode,
	operand1),
	gen_rtx_SIGN_EXTEND (V4SImode,
	operand2)),
	GEN_INT (16LL))));
}

/* ../../gcc/gcc/config/i386/i386.md:19569 */
rtx
gen_umulv4hi3_highpart (operand0, operand1, operand2)
     rtx operand0;
     rtx operand1;
     rtx operand2;
{
  return gen_rtx_SET (VOIDmode,
	operand0,
	gen_rtx_TRUNCATE (V4HImode,
	gen_rtx_LSHIFTRT (V4SImode,
	gen_rtx_MULT (V4SImode,
	gen_rtx_ZERO_EXTEND (V4SImode,
	operand1),
	gen_rtx_ZERO_EXTEND (V4SImode,
	operand2)),
	GEN_INT (16LL))));
}

/* ../../gcc/gcc/config/i386/i386.md:19583 */
rtx
gen_mmx_pmaddwd (operand0, operand1, operand2)
     rtx operand0;
     rtx operand1;
     rtx operand2;
{
  return gen_rtx_SET (VOIDmode,
	operand0,
	gen_rtx_PLUS (V2SImode,
	gen_rtx_MULT (V2SImode,
	gen_rtx_SIGN_EXTEND (V2SImode,
	gen_rtx_VEC_SELECT (V2HImode,
	operand1,
	gen_rtx_PARALLEL (VOIDmode,
	gen_rtvec (2,
		const0_rtx,
		GEN_INT (2LL))))),
	gen_rtx_SIGN_EXTEND (V2SImode,
	gen_rtx_VEC_SELECT (V2HImode,
	operand2,
	gen_rtx_PARALLEL (VOIDmode,
	gen_rtvec (2,
		const0_rtx,
		GEN_INT (2LL)))))),
	gen_rtx_MULT (V2SImode,
	gen_rtx_SIGN_EXTEND (V2SImode,
	gen_rtx_VEC_SELECT (V2HImode,
	operand1,
	gen_rtx_PARALLEL (VOIDmode,
	gen_rtvec (2,
		const1_rtx,
		GEN_INT (3LL))))),
	gen_rtx_SIGN_EXTEND (V2SImode,
	gen_rtx_VEC_SELECT (V2HImode,
	operand2,
	gen_rtx_PARALLEL (VOIDmode,
	gen_rtvec (2,
		const1_rtx,
		GEN_INT (3LL))))))));
}

/* ../../gcc/gcc/config/i386/i386.md:19610 */
rtx
gen_mmx_iordi3 (operand0, operand1, operand2)
     rtx operand0;
     rtx operand1;
     rtx operand2;
{
  return gen_rtx_SET (VOIDmode,
	operand0,
	gen_rtx_UNSPEC (DImode,
	gen_rtvec (1,
		gen_rtx_IOR (DImode,
	operand1,
	operand2)),
	45));
}

/* ../../gcc/gcc/config/i386/i386.md:19621 */
rtx
gen_mmx_xordi3 (operand0, operand1, operand2)
     rtx operand0;
     rtx operand1;
     rtx operand2;
{
  return gen_rtx_SET (VOIDmode,
	operand0,
	gen_rtx_UNSPEC (DImode,
	gen_rtvec (1,
		gen_rtx_XOR (DImode,
	operand1,
	operand2)),
	45));
}

/* ../../gcc/gcc/config/i386/i386.md:19635 */
rtx
gen_mmx_clrdi (operand0)
     rtx operand0;
{
  return gen_rtx_SET (VOIDmode,
	operand0,
	gen_rtx_UNSPEC (DImode,
	gen_rtvec (1,
		const0_rtx),
	45));
}

/* ../../gcc/gcc/config/i386/i386.md:19644 */
rtx
gen_mmx_anddi3 (operand0, operand1, operand2)
     rtx operand0;
     rtx operand1;
     rtx operand2;
{
  return gen_rtx_SET (VOIDmode,
	operand0,
	gen_rtx_UNSPEC (DImode,
	gen_rtvec (1,
		gen_rtx_AND (DImode,
	operand1,
	operand2)),
	45));
}

/* ../../gcc/gcc/config/i386/i386.md:19655 */
rtx
gen_mmx_nanddi3 (operand0, operand1, operand2)
     rtx operand0;
     rtx operand1;
     rtx operand2;
{
  return gen_rtx_SET (VOIDmode,
	operand0,
	gen_rtx_UNSPEC (DImode,
	gen_rtvec (1,
		gen_rtx_AND (DImode,
	gen_rtx_NOT (DImode,
	operand1),
	operand2)),
	45));
}

/* ../../gcc/gcc/config/i386/i386.md:19669 */
rtx
gen_mmx_uavgv8qi3 (operand0, operand1, operand2)
     rtx operand0;
     rtx operand1;
     rtx operand2;
{
  return gen_rtx_SET (VOIDmode,
	operand0,
	gen_rtx_ASHIFTRT (V8QImode,
	gen_rtx_PLUS (V8QImode,
	gen_rtx_PLUS (V8QImode,
	operand1,
	operand2),
	gen_rtx_CONST_VECTOR (V8QImode,
	gen_rtvec (8,
		const1_rtx,
		const1_rtx,
		const1_rtx,
		const1_rtx,
		const1_rtx,
		const1_rtx,
		const1_rtx,
		const1_rtx))),
	const1_rtx));
}

/* ../../gcc/gcc/config/i386/i386.md:19689 */
rtx
gen_mmx_uavgv4hi3 (operand0, operand1, operand2)
     rtx operand0;
     rtx operand1;
     rtx operand2;
{
  return gen_rtx_SET (VOIDmode,
	operand0,
	gen_rtx_ASHIFTRT (V4HImode,
	gen_rtx_PLUS (V4HImode,
	gen_rtx_PLUS (V4HImode,
	operand1,
	operand2),
	gen_rtx_CONST_VECTOR (V4HImode,
	gen_rtvec (4,
		const1_rtx,
		const1_rtx,
		const1_rtx,
		const1_rtx))),
	const1_rtx));
}

/* ../../gcc/gcc/config/i386/i386.md:19705 */
rtx
gen_mmx_psadbw (operand0, operand1, operand2)
     rtx operand0;
     rtx operand1;
     rtx operand2;
{
  return gen_rtx_SET (VOIDmode,
	operand0,
	gen_rtx_UNSPEC (DImode,
	gen_rtvec (2,
		operand1,
		operand2),
	61));
}

/* ../../gcc/gcc/config/i386/i386.md:19718 */
rtx
gen_mmx_pinsrw (operand0, operand1, operand2, operand3)
     rtx operand0;
     rtx operand1;
     rtx operand2;
     rtx operand3;
{
  return gen_rtx_SET (VOIDmode,
	operand0,
	gen_rtx_VEC_MERGE (V4HImode,
	operand1,
	gen_rtx_VEC_DUPLICATE (V4HImode,
	gen_rtx_TRUNCATE (HImode,
	operand2)),
	operand3));
}

/* ../../gcc/gcc/config/i386/i386.md:19729 */
rtx
gen_mmx_pextrw (operand0, operand1, operand2)
     rtx operand0;
     rtx operand1;
     rtx operand2;
{
  return gen_rtx_SET (VOIDmode,
	operand0,
	gen_rtx_ZERO_EXTEND (SImode,
	gen_rtx_VEC_SELECT (HImode,
	operand1,
	gen_rtx_PARALLEL (VOIDmode,
	gen_rtvec (1,
		operand2)))));
}

/* ../../gcc/gcc/config/i386/i386.md:19739 */
rtx
gen_mmx_pshufw (operand0, operand1, operand2)
     rtx operand0;
     rtx operand1;
     rtx operand2;
{
  return gen_rtx_SET (VOIDmode,
	operand0,
	gen_rtx_UNSPEC (V4HImode,
	gen_rtvec (2,
		operand1,
		operand2),
	41));
}

/* ../../gcc/gcc/config/i386/i386.md:19752 */
rtx
gen_eqv8qi3 (operand0, operand1, operand2)
     rtx operand0;
     rtx operand1;
     rtx operand2;
{
  return gen_rtx_SET (VOIDmode,
	operand0,
	gen_rtx_EQ (V8QImode,
	operand1,
	operand2));
}

/* ../../gcc/gcc/config/i386/i386.md:19761 */
rtx
gen_eqv4hi3 (operand0, operand1, operand2)
     rtx operand0;
     rtx operand1;
     rtx operand2;
{
  return gen_rtx_SET (VOIDmode,
	operand0,
	gen_rtx_EQ (V4HImode,
	operand1,
	operand2));
}

/* ../../gcc/gcc/config/i386/i386.md:19770 */
rtx
gen_eqv2si3 (operand0, operand1, operand2)
     rtx operand0;
     rtx operand1;
     rtx operand2;
{
  return gen_rtx_SET (VOIDmode,
	operand0,
	gen_rtx_EQ (V2SImode,
	operand1,
	operand2));
}

/* ../../gcc/gcc/config/i386/i386.md:19779 */
rtx
gen_gtv8qi3 (operand0, operand1, operand2)
     rtx operand0;
     rtx operand1;
     rtx operand2;
{
  return gen_rtx_SET (VOIDmode,
	operand0,
	gen_rtx_GT (V8QImode,
	operand1,
	operand2));
}

/* ../../gcc/gcc/config/i386/i386.md:19788 */
rtx
gen_gtv4hi3 (operand0, operand1, operand2)
     rtx operand0;
     rtx operand1;
     rtx operand2;
{
  return gen_rtx_SET (VOIDmode,
	operand0,
	gen_rtx_GT (V4HImode,
	operand1,
	operand2));
}

/* ../../gcc/gcc/config/i386/i386.md:19797 */
rtx
gen_gtv2si3 (operand0, operand1, operand2)
     rtx operand0;
     rtx operand1;
     rtx operand2;
{
  return gen_rtx_SET (VOIDmode,
	operand0,
	gen_rtx_GT (V2SImode,
	operand1,
	operand2));
}

/* ../../gcc/gcc/config/i386/i386.md:19809 */
rtx
gen_umaxv8qi3 (operand0, operand1, operand2)
     rtx operand0;
     rtx operand1;
     rtx operand2;
{
  return gen_rtx_SET (VOIDmode,
	operand0,
	gen_rtx_UMAX (V8QImode,
	operand1,
	operand2));
}

/* ../../gcc/gcc/config/i386/i386.md:19818 */
rtx
gen_smaxv4hi3 (operand0, operand1, operand2)
     rtx operand0;
     rtx operand1;
     rtx operand2;
{
  return gen_rtx_SET (VOIDmode,
	operand0,
	gen_rtx_SMAX (V4HImode,
	operand1,
	operand2));
}

/* ../../gcc/gcc/config/i386/i386.md:19827 */
rtx
gen_uminv8qi3 (operand0, operand1, operand2)
     rtx operand0;
     rtx operand1;
     rtx operand2;
{
  return gen_rtx_SET (VOIDmode,
	operand0,
	gen_rtx_UMIN (V8QImode,
	operand1,
	operand2));
}

/* ../../gcc/gcc/config/i386/i386.md:19836 */
rtx
gen_sminv4hi3 (operand0, operand1, operand2)
     rtx operand0;
     rtx operand1;
     rtx operand2;
{
  return gen_rtx_SET (VOIDmode,
	operand0,
	gen_rtx_SMIN (V4HImode,
	operand1,
	operand2));
}

/* ../../gcc/gcc/config/i386/i386.md:19848 */
rtx
gen_ashrv4hi3 (operand0, operand1, operand2)
     rtx operand0;
     rtx operand1;
     rtx operand2;
{
  return gen_rtx_SET (VOIDmode,
	operand0,
	gen_rtx_ASHIFTRT (V4HImode,
	operand1,
	operand2));
}

/* ../../gcc/gcc/config/i386/i386.md:19857 */
rtx
gen_ashrv2si3 (operand0, operand1, operand2)
     rtx operand0;
     rtx operand1;
     rtx operand2;
{
  return gen_rtx_SET (VOIDmode,
	operand0,
	gen_rtx_ASHIFTRT (V2SImode,
	operand1,
	operand2));
}

/* ../../gcc/gcc/config/i386/i386.md:19866 */
rtx
gen_lshrv4hi3 (operand0, operand1, operand2)
     rtx operand0;
     rtx operand1;
     rtx operand2;
{
  return gen_rtx_SET (VOIDmode,
	operand0,
	gen_rtx_LSHIFTRT (V4HImode,
	operand1,
	operand2));
}

/* ../../gcc/gcc/config/i386/i386.md:19875 */
rtx
gen_lshrv2si3 (operand0, operand1, operand2)
     rtx operand0;
     rtx operand1;
     rtx operand2;
{
  return gen_rtx_SET (VOIDmode,
	operand0,
	gen_rtx_LSHIFTRT (V2SImode,
	operand1,
	operand2));
}

/* ../../gcc/gcc/config/i386/i386.md:19885 */
rtx
gen_mmx_lshrdi3 (operand0, operand1, operand2)
     rtx operand0;
     rtx operand1;
     rtx operand2;
{
  return gen_rtx_SET (VOIDmode,
	operand0,
	gen_rtx_UNSPEC (DImode,
	gen_rtvec (1,
		gen_rtx_LSHIFTRT (DImode,
	operand1,
	operand2)),
	45));
}

/* ../../gcc/gcc/config/i386/i386.md:19896 */
rtx
gen_ashlv4hi3 (operand0, operand1, operand2)
     rtx operand0;
     rtx operand1;
     rtx operand2;
{
  return gen_rtx_SET (VOIDmode,
	operand0,
	gen_rtx_ASHIFT (V4HImode,
	operand1,
	operand2));
}

/* ../../gcc/gcc/config/i386/i386.md:19905 */
rtx
gen_ashlv2si3 (operand0, operand1, operand2)
     rtx operand0;
     rtx operand1;
     rtx operand2;
{
  return gen_rtx_SET (VOIDmode,
	operand0,
	gen_rtx_ASHIFT (V2SImode,
	operand1,
	operand2));
}

/* ../../gcc/gcc/config/i386/i386.md:19915 */
rtx
gen_mmx_ashldi3 (operand0, operand1, operand2)
     rtx operand0;
     rtx operand1;
     rtx operand2;
{
  return gen_rtx_SET (VOIDmode,
	operand0,
	gen_rtx_UNSPEC (DImode,
	gen_rtvec (1,
		gen_rtx_ASHIFT (DImode,
	operand1,
	operand2)),
	45));
}

/* ../../gcc/gcc/config/i386/i386.md:19929 */
rtx
gen_mmx_packsswb (operand0, operand1, operand2)
     rtx operand0;
     rtx operand1;
     rtx operand2;
{
  return gen_rtx_SET (VOIDmode,
	operand0,
	gen_rtx_VEC_CONCAT (V8QImode,
	gen_rtx_SS_TRUNCATE (V4QImode,
	operand1),
	gen_rtx_SS_TRUNCATE (V4QImode,
	operand2)));
}

/* ../../gcc/gcc/config/i386/i386.md:19939 */
rtx
gen_mmx_packssdw (operand0, operand1, operand2)
     rtx operand0;
     rtx operand1;
     rtx operand2;
{
  return gen_rtx_SET (VOIDmode,
	operand0,
	gen_rtx_VEC_CONCAT (V4HImode,
	gen_rtx_SS_TRUNCATE (V2HImode,
	operand1),
	gen_rtx_SS_TRUNCATE (V2HImode,
	operand2)));
}

/* ../../gcc/gcc/config/i386/i386.md:19949 */
rtx
gen_mmx_packuswb (operand0, operand1, operand2)
     rtx operand0;
     rtx operand1;
     rtx operand2;
{
  return gen_rtx_SET (VOIDmode,
	operand0,
	gen_rtx_VEC_CONCAT (V8QImode,
	gen_rtx_US_TRUNCATE (V4QImode,
	operand1),
	gen_rtx_US_TRUNCATE (V4QImode,
	operand2)));
}

/* ../../gcc/gcc/config/i386/i386.md:19959 */
rtx
gen_mmx_punpckhbw (operand0, operand1, operand2)
     rtx operand0;
     rtx operand1;
     rtx operand2;
{
  return gen_rtx_SET (VOIDmode,
	operand0,
	gen_rtx_VEC_MERGE (V8QImode,
	gen_rtx_VEC_SELECT (V8QImode,
	operand1,
	gen_rtx_PARALLEL (VOIDmode,
	gen_rtvec (8,
		GEN_INT (4LL),
		const0_rtx,
		GEN_INT (5LL),
		const1_rtx,
		GEN_INT (6LL),
		GEN_INT (2LL),
		GEN_INT (7LL),
		GEN_INT (3LL)))),
	gen_rtx_VEC_SELECT (V8QImode,
	operand2,
	gen_rtx_PARALLEL (VOIDmode,
	gen_rtvec (8,
		const0_rtx,
		GEN_INT (4LL),
		const1_rtx,
		GEN_INT (5LL),
		GEN_INT (2LL),
		GEN_INT (6LL),
		GEN_INT (3LL),
		GEN_INT (7LL)))),
	GEN_INT (85LL)));
}

/* ../../gcc/gcc/config/i386/i386.md:19986 */
rtx
gen_mmx_punpckhwd (operand0, operand1, operand2)
     rtx operand0;
     rtx operand1;
     rtx operand2;
{
  return gen_rtx_SET (VOIDmode,
	operand0,
	gen_rtx_VEC_MERGE (V4HImode,
	gen_rtx_VEC_SELECT (V4HImode,
	operand1,
	gen_rtx_PARALLEL (VOIDmode,
	gen_rtvec (4,
		const0_rtx,
		GEN_INT (2LL),
		const1_rtx,
		GEN_INT (3LL)))),
	gen_rtx_VEC_SELECT (V4HImode,
	operand2,
	gen_rtx_PARALLEL (VOIDmode,
	gen_rtvec (4,
		GEN_INT (2LL),
		const0_rtx,
		GEN_INT (3LL),
		const1_rtx))),
	GEN_INT (5LL)));
}

/* ../../gcc/gcc/config/i386/i386.md:20005 */
rtx
gen_mmx_punpckhdq (operand0, operand1, operand2)
     rtx operand0;
     rtx operand1;
     rtx operand2;
{
  return gen_rtx_SET (VOIDmode,
	operand0,
	gen_rtx_VEC_MERGE (V2SImode,
	operand1,
	gen_rtx_VEC_SELECT (V2SImode,
	operand2,
	gen_rtx_PARALLEL (VOIDmode,
	gen_rtvec (2,
		const1_rtx,
		const0_rtx))),
	const1_rtx));
}

/* ../../gcc/gcc/config/i386/i386.md:20018 */
rtx
gen_mmx_punpcklbw (operand0, operand1, operand2)
     rtx operand0;
     rtx operand1;
     rtx operand2;
{
  return gen_rtx_SET (VOIDmode,
	operand0,
	gen_rtx_VEC_MERGE (V8QImode,
	gen_rtx_VEC_SELECT (V8QImode,
	operand1,
	gen_rtx_PARALLEL (VOIDmode,
	gen_rtvec (8,
		const0_rtx,
		GEN_INT (4LL),
		const1_rtx,
		GEN_INT (5LL),
		GEN_INT (2LL),
		GEN_INT (6LL),
		GEN_INT (3LL),
		GEN_INT (7LL)))),
	gen_rtx_VEC_SELECT (V8QImode,
	operand2,
	gen_rtx_PARALLEL (VOIDmode,
	gen_rtvec (8,
		GEN_INT (4LL),
		const0_rtx,
		GEN_INT (5LL),
		const1_rtx,
		GEN_INT (6LL),
		GEN_INT (2LL),
		GEN_INT (7LL),
		GEN_INT (3LL)))),
	GEN_INT (85LL)));
}

/* ../../gcc/gcc/config/i386/i386.md:20045 */
rtx
gen_mmx_punpcklwd (operand0, operand1, operand2)
     rtx operand0;
     rtx operand1;
     rtx operand2;
{
  return gen_rtx_SET (VOIDmode,
	operand0,
	gen_rtx_VEC_MERGE (V4HImode,
	gen_rtx_VEC_SELECT (V4HImode,
	operand1,
	gen_rtx_PARALLEL (VOIDmode,
	gen_rtvec (4,
		GEN_INT (2LL),
		const0_rtx,
		GEN_INT (3LL),
		const1_rtx))),
	gen_rtx_VEC_SELECT (V4HImode,
	operand2,
	gen_rtx_PARALLEL (VOIDmode,
	gen_rtvec (4,
		const0_rtx,
		GEN_INT (2LL),
		const1_rtx,
		GEN_INT (3LL)))),
	GEN_INT (5LL)));
}

/* ../../gcc/gcc/config/i386/i386.md:20064 */
rtx
gen_mmx_punpckldq (operand0, operand1, operand2)
     rtx operand0;
     rtx operand1;
     rtx operand2;
{
  return gen_rtx_SET (VOIDmode,
	operand0,
	gen_rtx_VEC_MERGE (V2SImode,
	gen_rtx_VEC_SELECT (V2SImode,
	operand1,
	gen_rtx_PARALLEL (VOIDmode,
	gen_rtvec (2,
		const1_rtx,
		const0_rtx))),
	operand2,
	const1_rtx));
}

/* ../../gcc/gcc/config/i386/i386.md:20080 */
rtx
gen_emms ()
{
  return gen_rtx_PARALLEL (VOIDmode, gen_rtvec (17,
		gen_rtx_UNSPEC_VOLATILE (VOIDmode,
	gen_rtvec (1,
		const0_rtx),
	31),
		gen_rtx_CLOBBER (VOIDmode,
	gen_rtx_REG (XFmode,
	8)),
		gen_rtx_CLOBBER (VOIDmode,
	gen_rtx_REG (XFmode,
	9)),
		gen_rtx_CLOBBER (VOIDmode,
	gen_rtx_REG (XFmode,
	10)),
		gen_rtx_CLOBBER (VOIDmode,
	gen_rtx_REG (XFmode,
	11)),
		gen_rtx_CLOBBER (VOIDmode,
	gen_rtx_REG (XFmode,
	12)),
		gen_rtx_CLOBBER (VOIDmode,
	gen_rtx_REG (XFmode,
	13)),
		gen_rtx_CLOBBER (VOIDmode,
	gen_rtx_REG (XFmode,
	14)),
		gen_rtx_CLOBBER (VOIDmode,
	gen_rtx_REG (XFmode,
	15)),
		gen_rtx_CLOBBER (VOIDmode,
	gen_rtx_REG (DImode,
	29)),
		gen_rtx_CLOBBER (VOIDmode,
	gen_rtx_REG (DImode,
	30)),
		gen_rtx_CLOBBER (VOIDmode,
	gen_rtx_REG (DImode,
	31)),
		gen_rtx_CLOBBER (VOIDmode,
	gen_rtx_REG (DImode,
	32)),
		gen_rtx_CLOBBER (VOIDmode,
	gen_rtx_REG (DImode,
	33)),
		gen_rtx_CLOBBER (VOIDmode,
	gen_rtx_REG (DImode,
	34)),
		gen_rtx_CLOBBER (VOIDmode,
	gen_rtx_REG (DImode,
	35)),
		gen_rtx_CLOBBER (VOIDmode,
	gen_rtx_REG (DImode,
	36))));
}

/* ../../gcc/gcc/config/i386/i386.md:20103 */
rtx
gen_ldmxcsr (operand0)
     rtx operand0;
{
  return gen_rtx_UNSPEC_VOLATILE (VOIDmode,
	gen_rtvec (1,
		operand0),
	37);
}

/* ../../gcc/gcc/config/i386/i386.md:20111 */
rtx
gen_stmxcsr (operand0)
     rtx operand0;
{
  return gen_rtx_SET (VOIDmode,
	operand0,
	gen_rtx_UNSPEC_VOLATILE (SImode,
	gen_rtvec (1,
		const0_rtx),
	40));
}

/* ../../gcc/gcc/config/i386/i386.md:20199 */
rtx
gen_addv2sf3 (operand0, operand1, operand2)
     rtx operand0;
     rtx operand1;
     rtx operand2;
{
  return gen_rtx_SET (VOIDmode,
	operand0,
	gen_rtx_PLUS (V2SFmode,
	operand1,
	operand2));
}

/* ../../gcc/gcc/config/i386/i386.md:20208 */
rtx
gen_subv2sf3 (operand0, operand1, operand2)
     rtx operand0;
     rtx operand1;
     rtx operand2;
{
  return gen_rtx_SET (VOIDmode,
	operand0,
	gen_rtx_MINUS (V2SFmode,
	operand1,
	operand2));
}

/* ../../gcc/gcc/config/i386/i386.md:20217 */
rtx
gen_subrv2sf3 (operand0, operand1, operand2)
     rtx operand0;
     rtx operand1;
     rtx operand2;
{
  return gen_rtx_SET (VOIDmode,
	operand0,
	gen_rtx_MINUS (V2SFmode,
	operand2,
	operand1));
}

/* ../../gcc/gcc/config/i386/i386.md:20226 */
rtx
gen_gtv2sf3 (operand0, operand1, operand2)
     rtx operand0;
     rtx operand1;
     rtx operand2;
{
  return gen_rtx_SET (VOIDmode,
	operand0,
	gen_rtx_GT (V2SImode,
	operand1,
	operand2));
}

/* ../../gcc/gcc/config/i386/i386.md:20235 */
rtx
gen_gev2sf3 (operand0, operand1, operand2)
     rtx operand0;
     rtx operand1;
     rtx operand2;
{
  return gen_rtx_SET (VOIDmode,
	operand0,
	gen_rtx_GE (V2SImode,
	operand1,
	operand2));
}

/* ../../gcc/gcc/config/i386/i386.md:20244 */
rtx
gen_eqv2sf3 (operand0, operand1, operand2)
     rtx operand0;
     rtx operand1;
     rtx operand2;
{
  return gen_rtx_SET (VOIDmode,
	operand0,
	gen_rtx_EQ (V2SImode,
	operand1,
	operand2));
}

/* ../../gcc/gcc/config/i386/i386.md:20253 */
rtx
gen_pfmaxv2sf3 (operand0, operand1, operand2)
     rtx operand0;
     rtx operand1;
     rtx operand2;
{
  return gen_rtx_SET (VOIDmode,
	operand0,
	gen_rtx_SMAX (V2SFmode,
	operand1,
	operand2));
}

/* ../../gcc/gcc/config/i386/i386.md:20262 */
rtx
gen_pfminv2sf3 (operand0, operand1, operand2)
     rtx operand0;
     rtx operand1;
     rtx operand2;
{
  return gen_rtx_SET (VOIDmode,
	operand0,
	gen_rtx_SMIN (V2SFmode,
	operand1,
	operand2));
}

/* ../../gcc/gcc/config/i386/i386.md:20271 */
rtx
gen_mulv2sf3 (operand0, operand1, operand2)
     rtx operand0;
     rtx operand1;
     rtx operand2;
{
  return gen_rtx_SET (VOIDmode,
	operand0,
	gen_rtx_MULT (V2SFmode,
	operand1,
	operand2));
}

/* ../../gcc/gcc/config/i386/i386.md:20280 */
rtx
gen_femms ()
{
  return gen_rtx_PARALLEL (VOIDmode, gen_rtvec (17,
		gen_rtx_UNSPEC_VOLATILE (VOIDmode,
	gen_rtvec (1,
		const0_rtx),
	46),
		gen_rtx_CLOBBER (VOIDmode,
	gen_rtx_REG (XFmode,
	8)),
		gen_rtx_CLOBBER (VOIDmode,
	gen_rtx_REG (XFmode,
	9)),
		gen_rtx_CLOBBER (VOIDmode,
	gen_rtx_REG (XFmode,
	10)),
		gen_rtx_CLOBBER (VOIDmode,
	gen_rtx_REG (XFmode,
	11)),
		gen_rtx_CLOBBER (VOIDmode,
	gen_rtx_REG (XFmode,
	12)),
		gen_rtx_CLOBBER (VOIDmode,
	gen_rtx_REG (XFmode,
	13)),
		gen_rtx_CLOBBER (VOIDmode,
	gen_rtx_REG (XFmode,
	14)),
		gen_rtx_CLOBBER (VOIDmode,
	gen_rtx_REG (XFmode,
	15)),
		gen_rtx_CLOBBER (VOIDmode,
	gen_rtx_REG (DImode,
	29)),
		gen_rtx_CLOBBER (VOIDmode,
	gen_rtx_REG (DImode,
	30)),
		gen_rtx_CLOBBER (VOIDmode,
	gen_rtx_REG (DImode,
	31)),
		gen_rtx_CLOBBER (VOIDmode,
	gen_rtx_REG (DImode,
	32)),
		gen_rtx_CLOBBER (VOIDmode,
	gen_rtx_REG (DImode,
	33)),
		gen_rtx_CLOBBER (VOIDmode,
	gen_rtx_REG (DImode,
	34)),
		gen_rtx_CLOBBER (VOIDmode,
	gen_rtx_REG (DImode,
	35)),
		gen_rtx_CLOBBER (VOIDmode,
	gen_rtx_REG (DImode,
	36))));
}

/* ../../gcc/gcc/config/i386/i386.md:20303 */
rtx
gen_pf2id (operand0, operand1)
     rtx operand0;
     rtx operand1;
{
  return gen_rtx_SET (VOIDmode,
	operand0,
	gen_rtx_FIX (V2SImode,
	operand1));
}

/* ../../gcc/gcc/config/i386/i386.md:20311 */
rtx
gen_pf2iw (operand0, operand1)
     rtx operand0;
     rtx operand1;
{
  return gen_rtx_SET (VOIDmode,
	operand0,
	gen_rtx_SIGN_EXTEND (V2SImode,
	gen_rtx_SS_TRUNCATE (V2HImode,
	gen_rtx_FIX (V2SImode,
	operand1))));
}

/* ../../gcc/gcc/config/i386/i386.md:20321 */
rtx
gen_pfacc (operand0, operand1, operand2)
     rtx operand0;
     rtx operand1;
     rtx operand2;
{
  return gen_rtx_SET (VOIDmode,
	operand0,
	gen_rtx_VEC_CONCAT (V2SFmode,
	gen_rtx_PLUS (SFmode,
	gen_rtx_VEC_SELECT (SFmode,
	operand1,
	gen_rtx_PARALLEL (VOIDmode,
	gen_rtvec (1,
		const0_rtx))),
	gen_rtx_VEC_SELECT (SFmode,
	operand1,
	gen_rtx_PARALLEL (VOIDmode,
	gen_rtvec (1,
		const1_rtx)))),
	gen_rtx_PLUS (SFmode,
	gen_rtx_VEC_SELECT (SFmode,
	operand2,
	gen_rtx_PARALLEL (VOIDmode,
	gen_rtvec (1,
		const0_rtx))),
	gen_rtx_VEC_SELECT (SFmode,
	operand2,
	gen_rtx_PARALLEL (VOIDmode,
	gen_rtvec (1,
		const1_rtx))))));
}

/* ../../gcc/gcc/config/i386/i386.md:20339 */
rtx
gen_pfnacc (operand0, operand1, operand2)
     rtx operand0;
     rtx operand1;
     rtx operand2;
{
  return gen_rtx_SET (VOIDmode,
	operand0,
	gen_rtx_VEC_CONCAT (V2SFmode,
	gen_rtx_MINUS (SFmode,
	gen_rtx_VEC_SELECT (SFmode,
	operand1,
	gen_rtx_PARALLEL (VOIDmode,
	gen_rtvec (1,
		const0_rtx))),
	gen_rtx_VEC_SELECT (SFmode,
	operand1,
	gen_rtx_PARALLEL (VOIDmode,
	gen_rtvec (1,
		const1_rtx)))),
	gen_rtx_MINUS (SFmode,
	gen_rtx_VEC_SELECT (SFmode,
	operand2,
	gen_rtx_PARALLEL (VOIDmode,
	gen_rtvec (1,
		const0_rtx))),
	gen_rtx_VEC_SELECT (SFmode,
	operand2,
	gen_rtx_PARALLEL (VOIDmode,
	gen_rtvec (1,
		const1_rtx))))));
}

/* ../../gcc/gcc/config/i386/i386.md:20357 */
rtx
gen_pfpnacc (operand0, operand1, operand2)
     rtx operand0;
     rtx operand1;
     rtx operand2;
{
  return gen_rtx_SET (VOIDmode,
	operand0,
	gen_rtx_VEC_CONCAT (V2SFmode,
	gen_rtx_MINUS (SFmode,
	gen_rtx_VEC_SELECT (SFmode,
	operand1,
	gen_rtx_PARALLEL (VOIDmode,
	gen_rtvec (1,
		const0_rtx))),
	gen_rtx_VEC_SELECT (SFmode,
	operand1,
	gen_rtx_PARALLEL (VOIDmode,
	gen_rtvec (1,
		const1_rtx)))),
	gen_rtx_PLUS (SFmode,
	gen_rtx_VEC_SELECT (SFmode,
	operand2,
	gen_rtx_PARALLEL (VOIDmode,
	gen_rtvec (1,
		const0_rtx))),
	gen_rtx_VEC_SELECT (SFmode,
	operand2,
	gen_rtx_PARALLEL (VOIDmode,
	gen_rtvec (1,
		const1_rtx))))));
}

/* ../../gcc/gcc/config/i386/i386.md:20375 */
rtx
gen_pi2fw (operand0, operand1)
     rtx operand0;
     rtx operand1;
{
  return gen_rtx_SET (VOIDmode,
	operand0,
	gen_rtx_FLOAT (V2SFmode,
	gen_rtx_VEC_CONCAT (V2SImode,
	gen_rtx_SIGN_EXTEND (SImode,
	gen_rtx_TRUNCATE (HImode,
	gen_rtx_VEC_SELECT (SImode,
	operand1,
	gen_rtx_PARALLEL (VOIDmode,
	gen_rtvec (1,
		const0_rtx))))),
	gen_rtx_SIGN_EXTEND (SImode,
	gen_rtx_TRUNCATE (HImode,
	gen_rtx_VEC_SELECT (SImode,
	operand1,
	gen_rtx_PARALLEL (VOIDmode,
	gen_rtvec (1,
		const1_rtx))))))));
}

/* ../../gcc/gcc/config/i386/i386.md:20392 */
rtx
gen_floatv2si2 (operand0, operand1)
     rtx operand0;
     rtx operand1;
{
  return gen_rtx_SET (VOIDmode,
	operand0,
	gen_rtx_FLOAT (V2SFmode,
	operand1));
}

/* ../../gcc/gcc/config/i386/i386.md:20403 */
rtx
gen_pavgusb (operand0, operand1, operand2)
     rtx operand0;
     rtx operand1;
     rtx operand2;
{
  return gen_rtx_SET (VOIDmode,
	operand0,
	gen_rtx_UNSPEC (V8QImode,
	gen_rtvec (2,
		operand1,
		operand2),
	49));
}

/* ../../gcc/gcc/config/i386/i386.md:20416 */
rtx
gen_pfrcpv2sf2 (operand0, operand1)
     rtx operand0;
     rtx operand1;
{
  return gen_rtx_SET (VOIDmode,
	operand0,
	gen_rtx_UNSPEC (V2SFmode,
	gen_rtvec (1,
		operand1),
	50));
}

/* ../../gcc/gcc/config/i386/i386.md:20425 */
rtx
gen_pfrcpit1v2sf3 (operand0, operand1, operand2)
     rtx operand0;
     rtx operand1;
     rtx operand2;
{
  return gen_rtx_SET (VOIDmode,
	operand0,
	gen_rtx_UNSPEC (V2SFmode,
	gen_rtvec (2,
		operand1,
		operand2),
	51));
}

/* ../../gcc/gcc/config/i386/i386.md:20435 */
rtx
gen_pfrcpit2v2sf3 (operand0, operand1, operand2)
     rtx operand0;
     rtx operand1;
     rtx operand2;
{
  return gen_rtx_SET (VOIDmode,
	operand0,
	gen_rtx_UNSPEC (V2SFmode,
	gen_rtvec (2,
		operand1,
		operand2),
	52));
}

/* ../../gcc/gcc/config/i386/i386.md:20445 */
rtx
gen_pfrsqrtv2sf2 (operand0, operand1)
     rtx operand0;
     rtx operand1;
{
  return gen_rtx_SET (VOIDmode,
	operand0,
	gen_rtx_UNSPEC (V2SFmode,
	gen_rtvec (1,
		operand1),
	53));
}

/* ../../gcc/gcc/config/i386/i386.md:20454 */
rtx
gen_pfrsqit1v2sf3 (operand0, operand1, operand2)
     rtx operand0;
     rtx operand1;
     rtx operand2;
{
  return gen_rtx_SET (VOIDmode,
	operand0,
	gen_rtx_UNSPEC (V2SFmode,
	gen_rtvec (2,
		operand1,
		operand2),
	54));
}

/* ../../gcc/gcc/config/i386/i386.md:20464 */
rtx
gen_pmulhrwv4hi3 (operand0, operand1, operand2)
     rtx operand0;
     rtx operand1;
     rtx operand2;
{
  return gen_rtx_SET (VOIDmode,
	operand0,
	gen_rtx_TRUNCATE (V4HImode,
	gen_rtx_LSHIFTRT (V4SImode,
	gen_rtx_PLUS (V4SImode,
	gen_rtx_MULT (V4SImode,
	gen_rtx_SIGN_EXTEND (V4SImode,
	operand1),
	gen_rtx_SIGN_EXTEND (V4SImode,
	operand2)),
	gen_rtx_CONST_VECTOR (V4SImode,
	gen_rtvec (4,
		GEN_INT (32768LL),
		GEN_INT (32768LL),
		GEN_INT (32768LL),
		GEN_INT (32768LL)))),
	GEN_INT (16LL))));
}

/* ../../gcc/gcc/config/i386/i386.md:20484 */
rtx
gen_pswapdv2si2 (operand0, operand1)
     rtx operand0;
     rtx operand1;
{
  return gen_rtx_SET (VOIDmode,
	operand0,
	gen_rtx_VEC_SELECT (V2SImode,
	operand1,
	gen_rtx_PARALLEL (VOIDmode,
	gen_rtvec (2,
		const1_rtx,
		const0_rtx))));
}

/* ../../gcc/gcc/config/i386/i386.md:20493 */
rtx
gen_pswapdv2sf2 (operand0, operand1)
     rtx operand0;
     rtx operand1;
{
  return gen_rtx_SET (VOIDmode,
	operand0,
	gen_rtx_VEC_SELECT (V2SFmode,
	operand1,
	gen_rtx_PARALLEL (VOIDmode,
	gen_rtvec (2,
		const1_rtx,
		const0_rtx))));
}

/* ../../gcc/gcc/config/i386/i386.md:20596 */
rtx
gen_addv2df3 (operand0, operand1, operand2)
     rtx operand0;
     rtx operand1;
     rtx operand2;
{
  return gen_rtx_SET (VOIDmode,
	operand0,
	gen_rtx_PLUS (V2DFmode,
	operand1,
	operand2));
}

/* ../../gcc/gcc/config/i386/i386.md:20605 */
rtx
gen_vmaddv2df3 (operand0, operand1, operand2)
     rtx operand0;
     rtx operand1;
     rtx operand2;
{
  return gen_rtx_SET (VOIDmode,
	operand0,
	gen_rtx_VEC_MERGE (V2DFmode,
	gen_rtx_PLUS (V2DFmode,
	operand1,
	operand2),
	operand1,
	const1_rtx));
}

/* ../../gcc/gcc/config/i386/i386.md:20616 */
rtx
gen_subv2df3 (operand0, operand1, operand2)
     rtx operand0;
     rtx operand1;
     rtx operand2;
{
  return gen_rtx_SET (VOIDmode,
	operand0,
	gen_rtx_MINUS (V2DFmode,
	operand1,
	operand2));
}

/* ../../gcc/gcc/config/i386/i386.md:20625 */
rtx
gen_vmsubv2df3 (operand0, operand1, operand2)
     rtx operand0;
     rtx operand1;
     rtx operand2;
{
  return gen_rtx_SET (VOIDmode,
	operand0,
	gen_rtx_VEC_MERGE (V2DFmode,
	gen_rtx_MINUS (V2DFmode,
	operand1,
	operand2),
	operand1,
	const1_rtx));
}

/* ../../gcc/gcc/config/i386/i386.md:20636 */
rtx
gen_mulv2df3 (operand0, operand1, operand2)
     rtx operand0;
     rtx operand1;
     rtx operand2;
{
  return gen_rtx_SET (VOIDmode,
	operand0,
	gen_rtx_MULT (V2DFmode,
	operand1,
	operand2));
}

/* ../../gcc/gcc/config/i386/i386.md:20645 */
rtx
gen_vmmulv2df3 (operand0, operand1, operand2)
     rtx operand0;
     rtx operand1;
     rtx operand2;
{
  return gen_rtx_SET (VOIDmode,
	operand0,
	gen_rtx_VEC_MERGE (V2DFmode,
	gen_rtx_MULT (V2DFmode,
	operand1,
	operand2),
	operand1,
	const1_rtx));
}

/* ../../gcc/gcc/config/i386/i386.md:20656 */
rtx
gen_divv2df3 (operand0, operand1, operand2)
     rtx operand0;
     rtx operand1;
     rtx operand2;
{
  return gen_rtx_SET (VOIDmode,
	operand0,
	gen_rtx_DIV (V2DFmode,
	operand1,
	operand2));
}

/* ../../gcc/gcc/config/i386/i386.md:20665 */
rtx
gen_vmdivv2df3 (operand0, operand1, operand2)
     rtx operand0;
     rtx operand1;
     rtx operand2;
{
  return gen_rtx_SET (VOIDmode,
	operand0,
	gen_rtx_VEC_MERGE (V2DFmode,
	gen_rtx_DIV (V2DFmode,
	operand1,
	operand2),
	operand1,
	const1_rtx));
}

/* ../../gcc/gcc/config/i386/i386.md:20678 */
rtx
gen_smaxv2df3 (operand0, operand1, operand2)
     rtx operand0;
     rtx operand1;
     rtx operand2;
{
  return gen_rtx_SET (VOIDmode,
	operand0,
	gen_rtx_SMAX (V2DFmode,
	operand1,
	operand2));
}

/* ../../gcc/gcc/config/i386/i386.md:20687 */
rtx
gen_vmsmaxv2df3 (operand0, operand1, operand2)
     rtx operand0;
     rtx operand1;
     rtx operand2;
{
  return gen_rtx_SET (VOIDmode,
	operand0,
	gen_rtx_VEC_MERGE (V2DFmode,
	gen_rtx_SMAX (V2DFmode,
	operand1,
	operand2),
	operand1,
	const1_rtx));
}

/* ../../gcc/gcc/config/i386/i386.md:20698 */
rtx
gen_sminv2df3 (operand0, operand1, operand2)
     rtx operand0;
     rtx operand1;
     rtx operand2;
{
  return gen_rtx_SET (VOIDmode,
	operand0,
	gen_rtx_SMIN (V2DFmode,
	operand1,
	operand2));
}

/* ../../gcc/gcc/config/i386/i386.md:20707 */
rtx
gen_vmsminv2df3 (operand0, operand1, operand2)
     rtx operand0;
     rtx operand1;
     rtx operand2;
{
  return gen_rtx_SET (VOIDmode,
	operand0,
	gen_rtx_VEC_MERGE (V2DFmode,
	gen_rtx_SMIN (V2DFmode,
	operand1,
	operand2),
	operand1,
	const1_rtx));
}

/* ../../gcc/gcc/config/i386/i386.md:20720 */
rtx
gen_sqrtv2df2 (operand0, operand1)
     rtx operand0;
     rtx operand1;
{
  return gen_rtx_SET (VOIDmode,
	operand0,
	gen_rtx_SQRT (V2DFmode,
	operand1));
}

/* ../../gcc/gcc/config/i386/i386.md:20728 */
rtx
gen_vmsqrtv2df2 (operand0, operand1, operand2)
     rtx operand0;
     rtx operand1;
     rtx operand2;
{
  return gen_rtx_SET (VOIDmode,
	operand0,
	gen_rtx_VEC_MERGE (V2DFmode,
	gen_rtx_SQRT (V2DFmode,
	operand1),
	operand2,
	const1_rtx));
}

/* ../../gcc/gcc/config/i386/i386.md:20740 */
rtx
gen_maskcmpv2df3 (operand0, operand1, operand2, operand3)
     rtx operand0;
     rtx operand1;
     rtx operand2;
     rtx operand3;
{
  return gen_rtx_SET (VOIDmode,
	operand0,
	gen_rtx (GET_CODE (operand3), V2DImode,
		operand1,
		operand2));
}

/* ../../gcc/gcc/config/i386/i386.md:20750 */
rtx
gen_maskncmpv2df3 (operand0, operand1, operand2, operand3)
     rtx operand0;
     rtx operand1;
     rtx operand2;
     rtx operand3;
{
  return gen_rtx_SET (VOIDmode,
	operand0,
	gen_rtx_NOT (V2DImode,
	gen_rtx (GET_CODE (operand3), V2DImode,
		operand1,
		operand2)));
}

/* ../../gcc/gcc/config/i386/i386.md:20766 */
rtx
gen_vmmaskcmpv2df3 (operand0, operand1, operand2, operand3)
     rtx operand0;
     rtx operand1;
     rtx operand2;
     rtx operand3;
{
  return gen_rtx_SET (VOIDmode,
	operand0,
	gen_rtx_VEC_MERGE (V2DImode,
	gen_rtx (GET_CODE (operand3), V2DImode,
		operand1,
		operand2),
	gen_rtx_SUBREG (V2DImode,
	operand1,
	0),
	const1_rtx));
}

/* ../../gcc/gcc/config/i386/i386.md:20779 */
rtx
gen_vmmaskncmpv2df3 (operand0, operand1, operand2, operand3)
     rtx operand0;
     rtx operand1;
     rtx operand2;
     rtx operand3;
{
  return gen_rtx_SET (VOIDmode,
	operand0,
	gen_rtx_VEC_MERGE (V2DImode,
	gen_rtx_NOT (V2DImode,
	gen_rtx (GET_CODE (operand3), V2DImode,
		operand1,
		operand2)),
	gen_rtx_SUBREG (V2DImode,
	operand1,
	0),
	const1_rtx));
}

/* ../../gcc/gcc/config/i386/i386.md:20798 */
rtx
gen_sse2_comi (operand0, operand1)
     rtx operand0;
     rtx operand1;
{
  return gen_rtx_SET (VOIDmode,
	gen_rtx_REG (CCFPmode,
	17),
	gen_rtx_COMPARE (CCFPmode,
	gen_rtx_VEC_SELECT (DFmode,
	operand0,
	gen_rtx_PARALLEL (VOIDmode,
	gen_rtvec (1,
		const0_rtx))),
	gen_rtx_VEC_SELECT (DFmode,
	operand1,
	gen_rtx_PARALLEL (VOIDmode,
	gen_rtvec (1,
		const0_rtx)))));
}

/* ../../gcc/gcc/config/i386/i386.md:20811 */
rtx
gen_sse2_ucomi (operand0, operand1)
     rtx operand0;
     rtx operand1;
{
  return gen_rtx_SET (VOIDmode,
	gen_rtx_REG (CCFPUmode,
	17),
	gen_rtx_COMPARE (CCFPUmode,
	gen_rtx_VEC_SELECT (DFmode,
	operand0,
	gen_rtx_PARALLEL (VOIDmode,
	gen_rtvec (1,
		const0_rtx))),
	gen_rtx_VEC_SELECT (DFmode,
	operand1,
	gen_rtx_PARALLEL (VOIDmode,
	gen_rtvec (1,
		const0_rtx)))));
}

/* ../../gcc/gcc/config/i386/i386.md:20826 */
rtx
gen_sse2_movmskpd (operand0, operand1)
     rtx operand0;
     rtx operand1;
{
  return gen_rtx_SET (VOIDmode,
	operand0,
	gen_rtx_UNSPEC (SImode,
	gen_rtvec (1,
		operand1),
	33));
}

/* ../../gcc/gcc/config/i386/i386.md:20835 */
rtx
gen_sse2_pmovmskb (operand0, operand1)
     rtx operand0;
     rtx operand1;
{
  return gen_rtx_SET (VOIDmode,
	operand0,
	gen_rtx_UNSPEC (SImode,
	gen_rtvec (1,
		operand1),
	33));
}

/* ../../gcc/gcc/config/i386/i386.md:20844 */
rtx
gen_sse2_maskmovdqu (operand0, operand1, operand2)
     rtx operand0;
     rtx operand1;
     rtx operand2;
{
  return gen_rtx_SET (VOIDmode,
	gen_rtx_MEM (V16QImode,
	operand0),
	gen_rtx_UNSPEC (V16QImode,
	gen_rtvec (2,
		operand1,
		operand2),
	32));
}

/* ../../gcc/gcc/config/i386/i386.md:20855 */
rtx
gen_sse2_maskmovdqu_rex64 (operand0, operand1, operand2)
     rtx operand0;
     rtx operand1;
     rtx operand2;
{
  return gen_rtx_SET (VOIDmode,
	gen_rtx_MEM (V16QImode,
	operand0),
	gen_rtx_UNSPEC (V16QImode,
	gen_rtvec (2,
		operand1,
		operand2),
	32));
}

/* ../../gcc/gcc/config/i386/i386.md:20866 */
rtx
gen_sse2_movntv2df (operand0, operand1)
     rtx operand0;
     rtx operand1;
{
  return gen_rtx_SET (VOIDmode,
	operand0,
	gen_rtx_UNSPEC (V2DFmode,
	gen_rtvec (1,
		operand1),
	34));
}

/* ../../gcc/gcc/config/i386/i386.md:20875 */
rtx
gen_sse2_movntv2di (operand0, operand1)
     rtx operand0;
     rtx operand1;
{
  return gen_rtx_SET (VOIDmode,
	operand0,
	gen_rtx_UNSPEC (V2DImode,
	gen_rtvec (1,
		operand1),
	34));
}

/* ../../gcc/gcc/config/i386/i386.md:20884 */
rtx
gen_sse2_movntsi (operand0, operand1)
     rtx operand0;
     rtx operand1;
{
  return gen_rtx_SET (VOIDmode,
	operand0,
	gen_rtx_UNSPEC (SImode,
	gen_rtvec (1,
		operand1),
	34));
}

/* ../../gcc/gcc/config/i386/i386.md:20897 */
rtx
gen_cvtdq2ps (operand0, operand1)
     rtx operand0;
     rtx operand1;
{
  return gen_rtx_SET (VOIDmode,
	operand0,
	gen_rtx_FLOAT (V4SFmode,
	operand1));
}

/* ../../gcc/gcc/config/i386/i386.md:20905 */
rtx
gen_cvtps2dq (operand0, operand1)
     rtx operand0;
     rtx operand1;
{
  return gen_rtx_SET (VOIDmode,
	operand0,
	gen_rtx_FIX (V4SImode,
	operand1));
}

/* ../../gcc/gcc/config/i386/i386.md:20913 */
rtx
gen_cvttps2dq (operand0, operand1)
     rtx operand0;
     rtx operand1;
{
  return gen_rtx_SET (VOIDmode,
	operand0,
	gen_rtx_UNSPEC (V4SImode,
	gen_rtvec (1,
		operand1),
	30));
}

/* ../../gcc/gcc/config/i386/i386.md:20924 */
rtx
gen_cvtdq2pd (operand0, operand1)
     rtx operand0;
     rtx operand1;
{
  return gen_rtx_SET (VOIDmode,
	operand0,
	gen_rtx_FLOAT (V2DFmode,
	gen_rtx_VEC_SELECT (V2SImode,
	operand1,
	gen_rtx_PARALLEL (VOIDmode,
	gen_rtvec (2,
		const0_rtx,
		const1_rtx)))));
}

/* ../../gcc/gcc/config/i386/i386.md:20936 */
rtx
gen_cvtpd2dq (operand0, operand1)
     rtx operand0;
     rtx operand1;
{
  return gen_rtx_SET (VOIDmode,
	operand0,
	gen_rtx_VEC_CONCAT (V4SImode,
	gen_rtx_FIX (V2SImode,
	operand1),
	gen_rtx_CONST_VECTOR (V2SImode,
	gen_rtvec (2,
		const0_rtx,
		const0_rtx))));
}

/* ../../gcc/gcc/config/i386/i386.md:20946 */
rtx
gen_cvttpd2dq (operand0, operand1)
     rtx operand0;
     rtx operand1;
{
  return gen_rtx_SET (VOIDmode,
	operand0,
	gen_rtx_VEC_CONCAT (V4SImode,
	gen_rtx_UNSPEC (V2SImode,
	gen_rtvec (1,
		operand1),
	30),
	gen_rtx_CONST_VECTOR (V2SImode,
	gen_rtvec (2,
		const0_rtx,
		const0_rtx))));
}

/* ../../gcc/gcc/config/i386/i386.md:20957 */
rtx
gen_cvtpd2pi (operand0, operand1)
     rtx operand0;
     rtx operand1;
{
  return gen_rtx_SET (VOIDmode,
	operand0,
	gen_rtx_FIX (V2SImode,
	operand1));
}

/* ../../gcc/gcc/config/i386/i386.md:20965 */
rtx
gen_cvttpd2pi (operand0, operand1)
     rtx operand0;
     rtx operand1;
{
  return gen_rtx_SET (VOIDmode,
	operand0,
	gen_rtx_UNSPEC (V2SImode,
	gen_rtvec (1,
		operand1),
	30));
}

/* ../../gcc/gcc/config/i386/i386.md:20974 */
rtx
gen_cvtpi2pd (operand0, operand1)
     rtx operand0;
     rtx operand1;
{
  return gen_rtx_SET (VOIDmode,
	operand0,
	gen_rtx_FLOAT (V2DFmode,
	operand1));
}

/* ../../gcc/gcc/config/i386/i386.md:20984 */
rtx
gen_cvtsd2si (operand0, operand1)
     rtx operand0;
     rtx operand1;
{
  return gen_rtx_SET (VOIDmode,
	operand0,
	gen_rtx_FIX (SImode,
	gen_rtx_VEC_SELECT (DFmode,
	operand1,
	gen_rtx_PARALLEL (VOIDmode,
	gen_rtvec (1,
		const0_rtx)))));
}

/* ../../gcc/gcc/config/i386/i386.md:20993 */
rtx
gen_cvtsd2siq (operand0, operand1)
     rtx operand0;
     rtx operand1;
{
  return gen_rtx_SET (VOIDmode,
	operand0,
	gen_rtx_FIX (DImode,
	gen_rtx_VEC_SELECT (DFmode,
	operand1,
	gen_rtx_PARALLEL (VOIDmode,
	gen_rtvec (1,
		const0_rtx)))));
}

/* ../../gcc/gcc/config/i386/i386.md:21002 */
rtx
gen_cvttsd2si (operand0, operand1)
     rtx operand0;
     rtx operand1;
{
  return gen_rtx_SET (VOIDmode,
	operand0,
	gen_rtx_UNSPEC (SImode,
	gen_rtvec (1,
		gen_rtx_VEC_SELECT (DFmode,
	operand1,
	gen_rtx_PARALLEL (VOIDmode,
	gen_rtvec (1,
		const0_rtx)))),
	30));
}

/* ../../gcc/gcc/config/i386/i386.md:21011 */
rtx
gen_cvttsd2siq (operand0, operand1)
     rtx operand0;
     rtx operand1;
{
  return gen_rtx_SET (VOIDmode,
	operand0,
	gen_rtx_UNSPEC (DImode,
	gen_rtvec (1,
		gen_rtx_VEC_SELECT (DFmode,
	operand1,
	gen_rtx_PARALLEL (VOIDmode,
	gen_rtvec (1,
		const0_rtx)))),
	30));
}

/* ../../gcc/gcc/config/i386/i386.md:21021 */
rtx
gen_cvtsi2sd (operand0, operand1, operand2)
     rtx operand0;
     rtx operand1;
     rtx operand2;
{
  return gen_rtx_SET (VOIDmode,
	operand0,
	gen_rtx_VEC_MERGE (V2DFmode,
	operand1,
	gen_rtx_VEC_DUPLICATE (V2DFmode,
	gen_rtx_FLOAT (DFmode,
	operand2)),
	GEN_INT (2LL)));
}

/* ../../gcc/gcc/config/i386/i386.md:21033 */
rtx
gen_cvtsi2sdq (operand0, operand1, operand2)
     rtx operand0;
     rtx operand1;
     rtx operand2;
{
  return gen_rtx_SET (VOIDmode,
	operand0,
	gen_rtx_VEC_MERGE (V2DFmode,
	operand1,
	gen_rtx_VEC_DUPLICATE (V2DFmode,
	gen_rtx_FLOAT (DFmode,
	operand2)),
	GEN_INT (2LL)));
}

/* ../../gcc/gcc/config/i386/i386.md:21048 */
rtx
gen_cvtsd2ss (operand0, operand1, operand2)
     rtx operand0;
     rtx operand1;
     rtx operand2;
{
  return gen_rtx_SET (VOIDmode,
	operand0,
	gen_rtx_VEC_MERGE (V4SFmode,
	operand1,
	gen_rtx_VEC_DUPLICATE (V4SFmode,
	gen_rtx_FLOAT_TRUNCATE (V2SFmode,
	operand2)),
	GEN_INT (14LL)));
}

/* ../../gcc/gcc/config/i386/i386.md:21060 */
rtx
gen_cvtss2sd (operand0, operand1, operand2)
     rtx operand0;
     rtx operand1;
     rtx operand2;
{
  return gen_rtx_SET (VOIDmode,
	operand0,
	gen_rtx_VEC_MERGE (V2DFmode,
	operand1,
	gen_rtx_FLOAT_EXTEND (V2DFmode,
	gen_rtx_VEC_SELECT (V2SFmode,
	operand2,
	gen_rtx_PARALLEL (VOIDmode,
	gen_rtvec (2,
		const0_rtx,
		const1_rtx)))),
	GEN_INT (2LL)));
}

/* ../../gcc/gcc/config/i386/i386.md:21074 */
rtx
gen_cvtpd2ps (operand0, operand1)
     rtx operand0;
     rtx operand1;
{
  return gen_rtx_SET (VOIDmode,
	operand0,
	gen_rtx_SUBREG (V4SFmode,
	gen_rtx_VEC_CONCAT (V4SImode,
	gen_rtx_SUBREG (V2SImode,
	gen_rtx_FLOAT_TRUNCATE (V2SFmode,
	operand1),
	0),
	gen_rtx_CONST_VECTOR (V2SImode,
	gen_rtvec (2,
		const0_rtx,
		const0_rtx))),
	0));
}

/* ../../gcc/gcc/config/i386/i386.md:21086 */
rtx
gen_cvtps2pd (operand0, operand1)
     rtx operand0;
     rtx operand1;
{
  return gen_rtx_SET (VOIDmode,
	operand0,
	gen_rtx_FLOAT_EXTEND (V2DFmode,
	gen_rtx_VEC_SELECT (V2SFmode,
	operand1,
	gen_rtx_PARALLEL (VOIDmode,
	gen_rtvec (2,
		const0_rtx,
		const1_rtx)))));
}

/* ../../gcc/gcc/config/i386/i386.md:21101 */
rtx
gen_addv16qi3 (operand0, operand1, operand2)
     rtx operand0;
     rtx operand1;
     rtx operand2;
{
  return gen_rtx_SET (VOIDmode,
	operand0,
	gen_rtx_PLUS (V16QImode,
	operand1,
	operand2));
}

/* ../../gcc/gcc/config/i386/i386.md:21110 */
rtx
gen_addv8hi3 (operand0, operand1, operand2)
     rtx operand0;
     rtx operand1;
     rtx operand2;
{
  return gen_rtx_SET (VOIDmode,
	operand0,
	gen_rtx_PLUS (V8HImode,
	operand1,
	operand2));
}

/* ../../gcc/gcc/config/i386/i386.md:21119 */
rtx
gen_addv4si3 (operand0, operand1, operand2)
     rtx operand0;
     rtx operand1;
     rtx operand2;
{
  return gen_rtx_SET (VOIDmode,
	operand0,
	gen_rtx_PLUS (V4SImode,
	operand1,
	operand2));
}

/* ../../gcc/gcc/config/i386/i386.md:21128 */
rtx
gen_addv2di3 (operand0, operand1, operand2)
     rtx operand0;
     rtx operand1;
     rtx operand2;
{
  return gen_rtx_SET (VOIDmode,
	operand0,
	gen_rtx_PLUS (V2DImode,
	operand1,
	operand2));
}

/* ../../gcc/gcc/config/i386/i386.md:21137 */
rtx
gen_ssaddv16qi3 (operand0, operand1, operand2)
     rtx operand0;
     rtx operand1;
     rtx operand2;
{
  return gen_rtx_SET (VOIDmode,
	operand0,
	gen_rtx_SS_PLUS (V16QImode,
	operand1,
	operand2));
}

/* ../../gcc/gcc/config/i386/i386.md:21146 */
rtx
gen_ssaddv8hi3 (operand0, operand1, operand2)
     rtx operand0;
     rtx operand1;
     rtx operand2;
{
  return gen_rtx_SET (VOIDmode,
	operand0,
	gen_rtx_SS_PLUS (V8HImode,
	operand1,
	operand2));
}

/* ../../gcc/gcc/config/i386/i386.md:21155 */
rtx
gen_usaddv16qi3 (operand0, operand1, operand2)
     rtx operand0;
     rtx operand1;
     rtx operand2;
{
  return gen_rtx_SET (VOIDmode,
	operand0,
	gen_rtx_US_PLUS (V16QImode,
	operand1,
	operand2));
}

/* ../../gcc/gcc/config/i386/i386.md:21164 */
rtx
gen_usaddv8hi3 (operand0, operand1, operand2)
     rtx operand0;
     rtx operand1;
     rtx operand2;
{
  return gen_rtx_SET (VOIDmode,
	operand0,
	gen_rtx_US_PLUS (V8HImode,
	operand1,
	operand2));
}

/* ../../gcc/gcc/config/i386/i386.md:21173 */
rtx
gen_subv16qi3 (operand0, operand1, operand2)
     rtx operand0;
     rtx operand1;
     rtx operand2;
{
  return gen_rtx_SET (VOIDmode,
	operand0,
	gen_rtx_MINUS (V16QImode,
	operand1,
	operand2));
}

/* ../../gcc/gcc/config/i386/i386.md:21182 */
rtx
gen_subv8hi3 (operand0, operand1, operand2)
     rtx operand0;
     rtx operand1;
     rtx operand2;
{
  return gen_rtx_SET (VOIDmode,
	operand0,
	gen_rtx_MINUS (V8HImode,
	operand1,
	operand2));
}

/* ../../gcc/gcc/config/i386/i386.md:21191 */
rtx
gen_subv4si3 (operand0, operand1, operand2)
     rtx operand0;
     rtx operand1;
     rtx operand2;
{
  return gen_rtx_SET (VOIDmode,
	operand0,
	gen_rtx_MINUS (V4SImode,
	operand1,
	operand2));
}

/* ../../gcc/gcc/config/i386/i386.md:21200 */
rtx
gen_subv2di3 (operand0, operand1, operand2)
     rtx operand0;
     rtx operand1;
     rtx operand2;
{
  return gen_rtx_SET (VOIDmode,
	operand0,
	gen_rtx_MINUS (V2DImode,
	operand1,
	operand2));
}

/* ../../gcc/gcc/config/i386/i386.md:21209 */
rtx
gen_sssubv16qi3 (operand0, operand1, operand2)
     rtx operand0;
     rtx operand1;
     rtx operand2;
{
  return gen_rtx_SET (VOIDmode,
	operand0,
	gen_rtx_SS_MINUS (V16QImode,
	operand1,
	operand2));
}

/* ../../gcc/gcc/config/i386/i386.md:21218 */
rtx
gen_sssubv8hi3 (operand0, operand1, operand2)
     rtx operand0;
     rtx operand1;
     rtx operand2;
{
  return gen_rtx_SET (VOIDmode,
	operand0,
	gen_rtx_SS_MINUS (V8HImode,
	operand1,
	operand2));
}

/* ../../gcc/gcc/config/i386/i386.md:21227 */
rtx
gen_ussubv16qi3 (operand0, operand1, operand2)
     rtx operand0;
     rtx operand1;
     rtx operand2;
{
  return gen_rtx_SET (VOIDmode,
	operand0,
	gen_rtx_US_MINUS (V16QImode,
	operand1,
	operand2));
}

/* ../../gcc/gcc/config/i386/i386.md:21236 */
rtx
gen_ussubv8hi3 (operand0, operand1, operand2)
     rtx operand0;
     rtx operand1;
     rtx operand2;
{
  return gen_rtx_SET (VOIDmode,
	operand0,
	gen_rtx_US_MINUS (V8HImode,
	operand1,
	operand2));
}

/* ../../gcc/gcc/config/i386/i386.md:21245 */
rtx
gen_mulv8hi3 (operand0, operand1, operand2)
     rtx operand0;
     rtx operand1;
     rtx operand2;
{
  return gen_rtx_SET (VOIDmode,
	operand0,
	gen_rtx_MULT (V8HImode,
	operand1,
	operand2));
}

/* ../../gcc/gcc/config/i386/i386.md:21254 */
rtx
gen_smulv8hi3_highpart (operand0, operand1, operand2)
     rtx operand0;
     rtx operand1;
     rtx operand2;
{
  return gen_rtx_SET (VOIDmode,
	operand0,
	gen_rtx_TRUNCATE (V8HImode,
	gen_rtx_LSHIFTRT (V8SImode,
	gen_rtx_MULT (V8SImode,
	gen_rtx_SIGN_EXTEND (V8SImode,
	operand1),
	gen_rtx_SIGN_EXTEND (V8SImode,
	operand2)),
	GEN_INT (16LL))));
}

/* ../../gcc/gcc/config/i386/i386.md:21266 */
rtx
gen_umulv8hi3_highpart (operand0, operand1, operand2)
     rtx operand0;
     rtx operand1;
     rtx operand2;
{
  return gen_rtx_SET (VOIDmode,
	operand0,
	gen_rtx_TRUNCATE (V8HImode,
	gen_rtx_LSHIFTRT (V8SImode,
	gen_rtx_MULT (V8SImode,
	gen_rtx_ZERO_EXTEND (V8SImode,
	operand1),
	gen_rtx_ZERO_EXTEND (V8SImode,
	operand2)),
	GEN_INT (16LL))));
}

/* ../../gcc/gcc/config/i386/i386.md:21278 */
rtx
gen_sse2_umulsidi3 (operand0, operand1, operand2)
     rtx operand0;
     rtx operand1;
     rtx operand2;
{
  return gen_rtx_SET (VOIDmode,
	operand0,
	gen_rtx_MULT (DImode,
	gen_rtx_ZERO_EXTEND (DImode,
	gen_rtx_VEC_SELECT (SImode,
	operand1,
	gen_rtx_PARALLEL (VOIDmode,
	gen_rtvec (1,
		const0_rtx)))),
	gen_rtx_ZERO_EXTEND (DImode,
	gen_rtx_VEC_SELECT (SImode,
	operand2,
	gen_rtx_PARALLEL (VOIDmode,
	gen_rtvec (1,
		const0_rtx))))));
}

/* ../../gcc/gcc/config/i386/i386.md:21291 */
rtx
gen_sse2_umulv2siv2di3 (operand0, operand1, operand2)
     rtx operand0;
     rtx operand1;
     rtx operand2;
{
  return gen_rtx_SET (VOIDmode,
	operand0,
	gen_rtx_MULT (V2DImode,
	gen_rtx_ZERO_EXTEND (V2DImode,
	gen_rtx_VEC_SELECT (V2SImode,
	operand1,
	gen_rtx_PARALLEL (VOIDmode,
	gen_rtvec (2,
		const0_rtx,
		GEN_INT (2LL))))),
	gen_rtx_ZERO_EXTEND (V2DImode,
	gen_rtx_VEC_SELECT (V2SImode,
	operand2,
	gen_rtx_PARALLEL (VOIDmode,
	gen_rtvec (2,
		const0_rtx,
		GEN_INT (2LL)))))));
}

/* ../../gcc/gcc/config/i386/i386.md:21306 */
rtx
gen_sse2_pmaddwd (operand0, operand1, operand2)
     rtx operand0;
     rtx operand1;
     rtx operand2;
{
  return gen_rtx_SET (VOIDmode,
	operand0,
	gen_rtx_PLUS (V4SImode,
	gen_rtx_MULT (V4SImode,
	gen_rtx_SIGN_EXTEND (V4SImode,
	gen_rtx_VEC_SELECT (V4HImode,
	operand1,
	gen_rtx_PARALLEL (VOIDmode,
	gen_rtvec (4,
		const0_rtx,
		GEN_INT (2LL),
		GEN_INT (4LL),
		GEN_INT (6LL))))),
	gen_rtx_SIGN_EXTEND (V4SImode,
	gen_rtx_VEC_SELECT (V4HImode,
	operand2,
	gen_rtx_PARALLEL (VOIDmode,
	gen_rtvec (4,
		const0_rtx,
		GEN_INT (2LL),
		GEN_INT (4LL),
		GEN_INT (6LL)))))),
	gen_rtx_MULT (V4SImode,
	gen_rtx_SIGN_EXTEND (V4SImode,
	gen_rtx_VEC_SELECT (V4HImode,
	operand1,
	gen_rtx_PARALLEL (VOIDmode,
	gen_rtvec (4,
		const1_rtx,
		GEN_INT (3LL),
		GEN_INT (5LL),
		GEN_INT (7LL))))),
	gen_rtx_SIGN_EXTEND (V4SImode,
	gen_rtx_VEC_SELECT (V4HImode,
	operand2,
	gen_rtx_PARALLEL (VOIDmode,
	gen_rtvec (4,
		const1_rtx,
		GEN_INT (3LL),
		GEN_INT (5LL),
		GEN_INT (7LL))))))));
}

/* ../../gcc/gcc/config/i386/i386.md:21338 */
rtx
gen_sse2_clrti (operand0)
     rtx operand0;
{
  return gen_rtx_SET (VOIDmode,
	operand0,
	const0_rtx);
}

/* ../../gcc/gcc/config/i386/i386.md:21348 */
rtx
gen_sse2_uavgv16qi3 (operand0, operand1, operand2)
     rtx operand0;
     rtx operand1;
     rtx operand2;
{
  return gen_rtx_SET (VOIDmode,
	operand0,
	gen_rtx_ASHIFTRT (V16QImode,
	gen_rtx_PLUS (V16QImode,
	gen_rtx_PLUS (V16QImode,
	operand1,
	operand2),
	gen_rtx_CONST_VECTOR (V16QImode,
	gen_rtvec (16,
		const1_rtx,
		const1_rtx,
		const1_rtx,
		const1_rtx,
		const1_rtx,
		const1_rtx,
		const1_rtx,
		const1_rtx,
		const1_rtx,
		const1_rtx,
		const1_rtx,
		const1_rtx,
		const1_rtx,
		const1_rtx,
		const1_rtx,
		const1_rtx))),
	const1_rtx));
}

/* ../../gcc/gcc/config/i386/i386.md:21368 */
rtx
gen_sse2_uavgv8hi3 (operand0, operand1, operand2)
     rtx operand0;
     rtx operand1;
     rtx operand2;
{
  return gen_rtx_SET (VOIDmode,
	operand0,
	gen_rtx_ASHIFTRT (V8HImode,
	gen_rtx_PLUS (V8HImode,
	gen_rtx_PLUS (V8HImode,
	operand1,
	operand2),
	gen_rtx_CONST_VECTOR (V8HImode,
	gen_rtvec (8,
		const1_rtx,
		const1_rtx,
		const1_rtx,
		const1_rtx,
		const1_rtx,
		const1_rtx,
		const1_rtx,
		const1_rtx))),
	const1_rtx));
}

/* ../../gcc/gcc/config/i386/i386.md:21385 */
rtx
gen_sse2_psadbw (operand0, operand1, operand2)
     rtx operand0;
     rtx operand1;
     rtx operand2;
{
  return gen_rtx_SET (VOIDmode,
	operand0,
	gen_rtx_UNSPEC (V2DImode,
	gen_rtvec (2,
		operand1,
		operand2),
	61));
}

/* ../../gcc/gcc/config/i386/i386.md:21398 */
rtx
gen_sse2_pinsrw (operand0, operand1, operand2, operand3)
     rtx operand0;
     rtx operand1;
     rtx operand2;
     rtx operand3;
{
  return gen_rtx_SET (VOIDmode,
	operand0,
	gen_rtx_VEC_MERGE (V8HImode,
	operand1,
	gen_rtx_VEC_DUPLICATE (V8HImode,
	gen_rtx_TRUNCATE (HImode,
	operand2)),
	operand3));
}

/* ../../gcc/gcc/config/i386/i386.md:21410 */
rtx
gen_sse2_pextrw (operand0, operand1, operand2)
     rtx operand0;
     rtx operand1;
     rtx operand2;
{
  return gen_rtx_SET (VOIDmode,
	operand0,
	gen_rtx_ZERO_EXTEND (SImode,
	gen_rtx_VEC_SELECT (HImode,
	operand1,
	gen_rtx_PARALLEL (VOIDmode,
	gen_rtvec (1,
		operand2)))));
}

/* ../../gcc/gcc/config/i386/i386.md:21421 */
rtx
gen_sse2_pshufd (operand0, operand1, operand2)
     rtx operand0;
     rtx operand1;
     rtx operand2;
{
  return gen_rtx_SET (VOIDmode,
	operand0,
	gen_rtx_UNSPEC (V4SImode,
	gen_rtvec (2,
		operand1,
		operand2),
	41));
}

/* ../../gcc/gcc/config/i386/i386.md:21431 */
rtx
gen_sse2_pshuflw (operand0, operand1, operand2)
     rtx operand0;
     rtx operand1;
     rtx operand2;
{
  return gen_rtx_SET (VOIDmode,
	operand0,
	gen_rtx_UNSPEC (V8HImode,
	gen_rtvec (2,
		operand1,
		operand2),
	55));
}

/* ../../gcc/gcc/config/i386/i386.md:21441 */
rtx
gen_sse2_pshufhw (operand0, operand1, operand2)
     rtx operand0;
     rtx operand1;
     rtx operand2;
{
  return gen_rtx_SET (VOIDmode,
	operand0,
	gen_rtx_UNSPEC (V8HImode,
	gen_rtvec (2,
		operand1,
		operand2),
	56));
}

/* ../../gcc/gcc/config/i386/i386.md:21453 */
rtx
gen_eqv16qi3 (operand0, operand1, operand2)
     rtx operand0;
     rtx operand1;
     rtx operand2;
{
  return gen_rtx_SET (VOIDmode,
	operand0,
	gen_rtx_EQ (V16QImode,
	operand1,
	operand2));
}

/* ../../gcc/gcc/config/i386/i386.md:21462 */
rtx
gen_eqv8hi3 (operand0, operand1, operand2)
     rtx operand0;
     rtx operand1;
     rtx operand2;
{
  return gen_rtx_SET (VOIDmode,
	operand0,
	gen_rtx_EQ (V8HImode,
	operand1,
	operand2));
}

/* ../../gcc/gcc/config/i386/i386.md:21471 */
rtx
gen_eqv4si3 (operand0, operand1, operand2)
     rtx operand0;
     rtx operand1;
     rtx operand2;
{
  return gen_rtx_SET (VOIDmode,
	operand0,
	gen_rtx_EQ (V4SImode,
	operand1,
	operand2));
}

/* ../../gcc/gcc/config/i386/i386.md:21480 */
rtx
gen_gtv16qi3 (operand0, operand1, operand2)
     rtx operand0;
     rtx operand1;
     rtx operand2;
{
  return gen_rtx_SET (VOIDmode,
	operand0,
	gen_rtx_GT (V16QImode,
	operand1,
	operand2));
}

/* ../../gcc/gcc/config/i386/i386.md:21489 */
rtx
gen_gtv8hi3 (operand0, operand1, operand2)
     rtx operand0;
     rtx operand1;
     rtx operand2;
{
  return gen_rtx_SET (VOIDmode,
	operand0,
	gen_rtx_GT (V8HImode,
	operand1,
	operand2));
}

/* ../../gcc/gcc/config/i386/i386.md:21498 */
rtx
gen_gtv4si3 (operand0, operand1, operand2)
     rtx operand0;
     rtx operand1;
     rtx operand2;
{
  return gen_rtx_SET (VOIDmode,
	operand0,
	gen_rtx_GT (V4SImode,
	operand1,
	operand2));
}

/* ../../gcc/gcc/config/i386/i386.md:21510 */
rtx
gen_umaxv16qi3 (operand0, operand1, operand2)
     rtx operand0;
     rtx operand1;
     rtx operand2;
{
  return gen_rtx_SET (VOIDmode,
	operand0,
	gen_rtx_UMAX (V16QImode,
	operand1,
	operand2));
}

/* ../../gcc/gcc/config/i386/i386.md:21519 */
rtx
gen_smaxv8hi3 (operand0, operand1, operand2)
     rtx operand0;
     rtx operand1;
     rtx operand2;
{
  return gen_rtx_SET (VOIDmode,
	operand0,
	gen_rtx_SMAX (V8HImode,
	operand1,
	operand2));
}

/* ../../gcc/gcc/config/i386/i386.md:21528 */
rtx
gen_uminv16qi3 (operand0, operand1, operand2)
     rtx operand0;
     rtx operand1;
     rtx operand2;
{
  return gen_rtx_SET (VOIDmode,
	operand0,
	gen_rtx_UMIN (V16QImode,
	operand1,
	operand2));
}

/* ../../gcc/gcc/config/i386/i386.md:21537 */
rtx
gen_sminv8hi3 (operand0, operand1, operand2)
     rtx operand0;
     rtx operand1;
     rtx operand2;
{
  return gen_rtx_SET (VOIDmode,
	operand0,
	gen_rtx_SMIN (V8HImode,
	operand1,
	operand2));
}

/* ../../gcc/gcc/config/i386/i386.md:21549 */
rtx
gen_ashrv8hi3 (operand0, operand1, operand2)
     rtx operand0;
     rtx operand1;
     rtx operand2;
{
  return gen_rtx_SET (VOIDmode,
	operand0,
	gen_rtx_ASHIFTRT (V8HImode,
	operand1,
	operand2));
}

/* ../../gcc/gcc/config/i386/i386.md:21558 */
rtx
gen_ashrv4si3 (operand0, operand1, operand2)
     rtx operand0;
     rtx operand1;
     rtx operand2;
{
  return gen_rtx_SET (VOIDmode,
	operand0,
	gen_rtx_ASHIFTRT (V4SImode,
	operand1,
	operand2));
}

/* ../../gcc/gcc/config/i386/i386.md:21567 */
rtx
gen_lshrv8hi3 (operand0, operand1, operand2)
     rtx operand0;
     rtx operand1;
     rtx operand2;
{
  return gen_rtx_SET (VOIDmode,
	operand0,
	gen_rtx_LSHIFTRT (V8HImode,
	operand1,
	operand2));
}

/* ../../gcc/gcc/config/i386/i386.md:21576 */
rtx
gen_lshrv4si3 (operand0, operand1, operand2)
     rtx operand0;
     rtx operand1;
     rtx operand2;
{
  return gen_rtx_SET (VOIDmode,
	operand0,
	gen_rtx_LSHIFTRT (V4SImode,
	operand1,
	operand2));
}

/* ../../gcc/gcc/config/i386/i386.md:21585 */
rtx
gen_lshrv2di3 (operand0, operand1, operand2)
     rtx operand0;
     rtx operand1;
     rtx operand2;
{
  return gen_rtx_SET (VOIDmode,
	operand0,
	gen_rtx_LSHIFTRT (V2DImode,
	operand1,
	operand2));
}

/* ../../gcc/gcc/config/i386/i386.md:21594 */
rtx
gen_ashlv8hi3 (operand0, operand1, operand2)
     rtx operand0;
     rtx operand1;
     rtx operand2;
{
  return gen_rtx_SET (VOIDmode,
	operand0,
	gen_rtx_ASHIFT (V8HImode,
	operand1,
	operand2));
}

/* ../../gcc/gcc/config/i386/i386.md:21603 */
rtx
gen_ashlv4si3 (operand0, operand1, operand2)
     rtx operand0;
     rtx operand1;
     rtx operand2;
{
  return gen_rtx_SET (VOIDmode,
	operand0,
	gen_rtx_ASHIFT (V4SImode,
	operand1,
	operand2));
}

/* ../../gcc/gcc/config/i386/i386.md:21612 */
rtx
gen_ashlv2di3 (operand0, operand1, operand2)
     rtx operand0;
     rtx operand1;
     rtx operand2;
{
  return gen_rtx_SET (VOIDmode,
	operand0,
	gen_rtx_ASHIFT (V2DImode,
	operand1,
	operand2));
}

/* ../../gcc/gcc/config/i386/i386.md:21621 */
rtx
gen_ashrv8hi3_ti (operand0, operand1, operand2)
     rtx operand0;
     rtx operand1;
     rtx operand2;
{
  return gen_rtx_SET (VOIDmode,
	operand0,
	gen_rtx_ASHIFTRT (V8HImode,
	operand1,
	gen_rtx_SUBREG (TImode,
	operand2,
	0)));
}

/* ../../gcc/gcc/config/i386/i386.md:21630 */
rtx
gen_ashrv4si3_ti (operand0, operand1, operand2)
     rtx operand0;
     rtx operand1;
     rtx operand2;
{
  return gen_rtx_SET (VOIDmode,
	operand0,
	gen_rtx_ASHIFTRT (V4SImode,
	operand1,
	gen_rtx_SUBREG (TImode,
	operand2,
	0)));
}

/* ../../gcc/gcc/config/i386/i386.md:21639 */
rtx
gen_lshrv8hi3_ti (operand0, operand1, operand2)
     rtx operand0;
     rtx operand1;
     rtx operand2;
{
  return gen_rtx_SET (VOIDmode,
	operand0,
	gen_rtx_LSHIFTRT (V8HImode,
	operand1,
	gen_rtx_SUBREG (TImode,
	operand2,
	0)));
}

/* ../../gcc/gcc/config/i386/i386.md:21648 */
rtx
gen_lshrv4si3_ti (operand0, operand1, operand2)
     rtx operand0;
     rtx operand1;
     rtx operand2;
{
  return gen_rtx_SET (VOIDmode,
	operand0,
	gen_rtx_LSHIFTRT (V4SImode,
	operand1,
	gen_rtx_SUBREG (TImode,
	operand2,
	0)));
}

/* ../../gcc/gcc/config/i386/i386.md:21657 */
rtx
gen_lshrv2di3_ti (operand0, operand1, operand2)
     rtx operand0;
     rtx operand1;
     rtx operand2;
{
  return gen_rtx_SET (VOIDmode,
	operand0,
	gen_rtx_LSHIFTRT (V2DImode,
	operand1,
	gen_rtx_SUBREG (TImode,
	operand2,
	0)));
}

/* ../../gcc/gcc/config/i386/i386.md:21666 */
rtx
gen_ashlv8hi3_ti (operand0, operand1, operand2)
     rtx operand0;
     rtx operand1;
     rtx operand2;
{
  return gen_rtx_SET (VOIDmode,
	operand0,
	gen_rtx_ASHIFT (V8HImode,
	operand1,
	gen_rtx_SUBREG (TImode,
	operand2,
	0)));
}

/* ../../gcc/gcc/config/i386/i386.md:21675 */
rtx
gen_ashlv4si3_ti (operand0, operand1, operand2)
     rtx operand0;
     rtx operand1;
     rtx operand2;
{
  return gen_rtx_SET (VOIDmode,
	operand0,
	gen_rtx_ASHIFT (V4SImode,
	operand1,
	gen_rtx_SUBREG (TImode,
	operand2,
	0)));
}

/* ../../gcc/gcc/config/i386/i386.md:21684 */
rtx
gen_ashlv2di3_ti (operand0, operand1, operand2)
     rtx operand0;
     rtx operand1;
     rtx operand2;
{
  return gen_rtx_SET (VOIDmode,
	operand0,
	gen_rtx_ASHIFT (V2DImode,
	operand1,
	gen_rtx_SUBREG (TImode,
	operand2,
	0)));
}

/* ../../gcc/gcc/config/i386/i386.md:21697 */
rtx
gen_sse2_ashlti3 (operand0, operand1, operand2)
     rtx operand0;
     rtx operand1;
     rtx operand2;
{
  return gen_rtx_SET (VOIDmode,
	operand0,
	gen_rtx_UNSPEC (TImode,
	gen_rtvec (1,
		gen_rtx_ASHIFT (TImode,
	operand1,
	gen_rtx_MULT (SImode,
	operand2,
	GEN_INT (8LL)))),
	45));
}

/* ../../gcc/gcc/config/i386/i386.md:21708 */
rtx
gen_sse2_lshrti3 (operand0, operand1, operand2)
     rtx operand0;
     rtx operand1;
     rtx operand2;
{
  return gen_rtx_SET (VOIDmode,
	operand0,
	gen_rtx_UNSPEC (TImode,
	gen_rtvec (1,
		gen_rtx_LSHIFTRT (TImode,
	operand1,
	gen_rtx_MULT (SImode,
	operand2,
	GEN_INT (8LL)))),
	45));
}

/* ../../gcc/gcc/config/i386/i386.md:21721 */
rtx
gen_sse2_unpckhpd (operand0, operand1, operand2)
     rtx operand0;
     rtx operand1;
     rtx operand2;
{
  return gen_rtx_SET (VOIDmode,
	operand0,
	gen_rtx_VEC_CONCAT (V2DFmode,
	gen_rtx_VEC_SELECT (V2DFmode,
	operand1,
	gen_rtx_PARALLEL (VOIDmode,
	gen_rtvec (1,
		const1_rtx))),
	gen_rtx_VEC_SELECT (V2DFmode,
	operand2,
	gen_rtx_PARALLEL (VOIDmode,
	gen_rtvec (1,
		const0_rtx)))));
}

/* ../../gcc/gcc/config/i386/i386.md:21733 */
rtx
gen_sse2_unpcklpd (operand0, operand1, operand2)
     rtx operand0;
     rtx operand1;
     rtx operand2;
{
  return gen_rtx_SET (VOIDmode,
	operand0,
	gen_rtx_VEC_CONCAT (V2DFmode,
	gen_rtx_VEC_SELECT (V2DFmode,
	operand1,
	gen_rtx_PARALLEL (VOIDmode,
	gen_rtvec (1,
		const0_rtx))),
	gen_rtx_VEC_SELECT (V2DFmode,
	operand2,
	gen_rtx_PARALLEL (VOIDmode,
	gen_rtvec (1,
		const1_rtx)))));
}

/* ../../gcc/gcc/config/i386/i386.md:21747 */
rtx
gen_sse2_packsswb (operand0, operand1, operand2)
     rtx operand0;
     rtx operand1;
     rtx operand2;
{
  return gen_rtx_SET (VOIDmode,
	operand0,
	gen_rtx_VEC_CONCAT (V16QImode,
	gen_rtx_SS_TRUNCATE (V8QImode,
	operand1),
	gen_rtx_SS_TRUNCATE (V8QImode,
	operand2)));
}

/* ../../gcc/gcc/config/i386/i386.md:21757 */
rtx
gen_sse2_packssdw (operand0, operand1, operand2)
     rtx operand0;
     rtx operand1;
     rtx operand2;
{
  return gen_rtx_SET (VOIDmode,
	operand0,
	gen_rtx_VEC_CONCAT (V8HImode,
	gen_rtx_SS_TRUNCATE (V4HImode,
	operand1),
	gen_rtx_SS_TRUNCATE (V4HImode,
	operand2)));
}

/* ../../gcc/gcc/config/i386/i386.md:21767 */
rtx
gen_sse2_packuswb (operand0, operand1, operand2)
     rtx operand0;
     rtx operand1;
     rtx operand2;
{
  return gen_rtx_SET (VOIDmode,
	operand0,
	gen_rtx_VEC_CONCAT (V16QImode,
	gen_rtx_US_TRUNCATE (V8QImode,
	operand1),
	gen_rtx_US_TRUNCATE (V8QImode,
	operand2)));
}

/* ../../gcc/gcc/config/i386/i386.md:21777 */
rtx
gen_sse2_punpckhbw (operand0, operand1, operand2)
     rtx operand0;
     rtx operand1;
     rtx operand2;
{
  return gen_rtx_SET (VOIDmode,
	operand0,
	gen_rtx_VEC_MERGE (V16QImode,
	gen_rtx_VEC_SELECT (V16QImode,
	operand1,
	gen_rtx_PARALLEL (VOIDmode,
	gen_rtvec (16,
		GEN_INT (8LL),
		const0_rtx,
		GEN_INT (9LL),
		const1_rtx,
		GEN_INT (10LL),
		GEN_INT (2LL),
		GEN_INT (11LL),
		GEN_INT (3LL),
		GEN_INT (12LL),
		GEN_INT (4LL),
		GEN_INT (13LL),
		GEN_INT (5LL),
		GEN_INT (14LL),
		GEN_INT (6LL),
		GEN_INT (15LL),
		GEN_INT (7LL)))),
	gen_rtx_VEC_SELECT (V16QImode,
	operand2,
	gen_rtx_PARALLEL (VOIDmode,
	gen_rtvec (16,
		const0_rtx,
		GEN_INT (8LL),
		const1_rtx,
		GEN_INT (9LL),
		GEN_INT (2LL),
		GEN_INT (10LL),
		GEN_INT (3LL),
		GEN_INT (11LL),
		GEN_INT (4LL),
		GEN_INT (12LL),
		GEN_INT (5LL),
		GEN_INT (13LL),
		GEN_INT (6LL),
		GEN_INT (14LL),
		GEN_INT (7LL),
		GEN_INT (15LL)))),
	GEN_INT (21845LL)));
}

/* ../../gcc/gcc/config/i386/i386.md:21804 */
rtx
gen_sse2_punpckhwd (operand0, operand1, operand2)
     rtx operand0;
     rtx operand1;
     rtx operand2;
{
  return gen_rtx_SET (VOIDmode,
	operand0,
	gen_rtx_VEC_MERGE (V8HImode,
	gen_rtx_VEC_SELECT (V8HImode,
	operand1,
	gen_rtx_PARALLEL (VOIDmode,
	gen_rtvec (8,
		GEN_INT (4LL),
		const0_rtx,
		GEN_INT (5LL),
		const1_rtx,
		GEN_INT (6LL),
		GEN_INT (2LL),
		GEN_INT (7LL),
		GEN_INT (3LL)))),
	gen_rtx_VEC_SELECT (V8HImode,
	operand2,
	gen_rtx_PARALLEL (VOIDmode,
	gen_rtvec (8,
		const0_rtx,
		GEN_INT (4LL),
		const1_rtx,
		GEN_INT (5LL),
		GEN_INT (2LL),
		GEN_INT (6LL),
		GEN_INT (3LL),
		GEN_INT (7LL)))),
	GEN_INT (85LL)));
}

/* ../../gcc/gcc/config/i386/i386.md:21823 */
rtx
gen_sse2_punpckhdq (operand0, operand1, operand2)
     rtx operand0;
     rtx operand1;
     rtx operand2;
{
  return gen_rtx_SET (VOIDmode,
	operand0,
	gen_rtx_VEC_MERGE (V4SImode,
	gen_rtx_VEC_SELECT (V4SImode,
	operand1,
	gen_rtx_PARALLEL (VOIDmode,
	gen_rtvec (4,
		GEN_INT (2LL),
		const0_rtx,
		GEN_INT (3LL),
		const1_rtx))),
	gen_rtx_VEC_SELECT (V4SImode,
	operand2,
	gen_rtx_PARALLEL (VOIDmode,
	gen_rtvec (4,
		const0_rtx,
		GEN_INT (2LL),
		const1_rtx,
		GEN_INT (3LL)))),
	GEN_INT (5LL)));
}

/* ../../gcc/gcc/config/i386/i386.md:21838 */
rtx
gen_sse2_punpcklbw (operand0, operand1, operand2)
     rtx operand0;
     rtx operand1;
     rtx operand2;
{
  return gen_rtx_SET (VOIDmode,
	operand0,
	gen_rtx_VEC_MERGE (V16QImode,
	gen_rtx_VEC_SELECT (V16QImode,
	operand1,
	gen_rtx_PARALLEL (VOIDmode,
	gen_rtvec (16,
		const0_rtx,
		GEN_INT (8LL),
		const1_rtx,
		GEN_INT (9LL),
		GEN_INT (2LL),
		GEN_INT (10LL),
		GEN_INT (3LL),
		GEN_INT (11LL),
		GEN_INT (4LL),
		GEN_INT (12LL),
		GEN_INT (5LL),
		GEN_INT (13LL),
		GEN_INT (6LL),
		GEN_INT (14LL),
		GEN_INT (7LL),
		GEN_INT (15LL)))),
	gen_rtx_VEC_SELECT (V16QImode,
	operand2,
	gen_rtx_PARALLEL (VOIDmode,
	gen_rtvec (16,
		GEN_INT (8LL),
		const0_rtx,
		GEN_INT (9LL),
		const1_rtx,
		GEN_INT (10LL),
		GEN_INT (2LL),
		GEN_INT (11LL),
		GEN_INT (3LL),
		GEN_INT (12LL),
		GEN_INT (4LL),
		GEN_INT (13LL),
		GEN_INT (5LL),
		GEN_INT (14LL),
		GEN_INT (6LL),
		GEN_INT (15LL),
		GEN_INT (7LL)))),
	GEN_INT (21845LL)));
}

/* ../../gcc/gcc/config/i386/i386.md:21865 */
rtx
gen_sse2_punpcklwd (operand0, operand1, operand2)
     rtx operand0;
     rtx operand1;
     rtx operand2;
{
  return gen_rtx_SET (VOIDmode,
	operand0,
	gen_rtx_VEC_MERGE (V8HImode,
	gen_rtx_VEC_SELECT (V8HImode,
	operand1,
	gen_rtx_PARALLEL (VOIDmode,
	gen_rtvec (8,
		const0_rtx,
		GEN_INT (4LL),
		const1_rtx,
		GEN_INT (5LL),
		GEN_INT (2LL),
		GEN_INT (6LL),
		GEN_INT (3LL),
		GEN_INT (7LL)))),
	gen_rtx_VEC_SELECT (V8HImode,
	operand2,
	gen_rtx_PARALLEL (VOIDmode,
	gen_rtvec (8,
		GEN_INT (4LL),
		const0_rtx,
		GEN_INT (5LL),
		const1_rtx,
		GEN_INT (6LL),
		GEN_INT (2LL),
		GEN_INT (7LL),
		GEN_INT (3LL)))),
	GEN_INT (85LL)));
}

/* ../../gcc/gcc/config/i386/i386.md:21884 */
rtx
gen_sse2_punpckldq (operand0, operand1, operand2)
     rtx operand0;
     rtx operand1;
     rtx operand2;
{
  return gen_rtx_SET (VOIDmode,
	operand0,
	gen_rtx_VEC_MERGE (V4SImode,
	gen_rtx_VEC_SELECT (V4SImode,
	operand1,
	gen_rtx_PARALLEL (VOIDmode,
	gen_rtvec (4,
		const0_rtx,
		GEN_INT (2LL),
		const1_rtx,
		GEN_INT (3LL)))),
	gen_rtx_VEC_SELECT (V4SImode,
	operand2,
	gen_rtx_PARALLEL (VOIDmode,
	gen_rtvec (4,
		GEN_INT (2LL),
		const0_rtx,
		GEN_INT (3LL),
		const1_rtx))),
	GEN_INT (5LL)));
}

/* ../../gcc/gcc/config/i386/i386.md:21899 */
rtx
gen_sse2_punpcklqdq (operand0, operand1, operand2)
     rtx operand0;
     rtx operand1;
     rtx operand2;
{
  return gen_rtx_SET (VOIDmode,
	operand0,
	gen_rtx_VEC_MERGE (V2DImode,
	gen_rtx_VEC_SELECT (V2DImode,
	operand2,
	gen_rtx_PARALLEL (VOIDmode,
	gen_rtvec (2,
		const1_rtx,
		const0_rtx))),
	operand1,
	const1_rtx));
}

/* ../../gcc/gcc/config/i386/i386.md:21912 */
rtx
gen_sse2_punpckhqdq (operand0, operand1, operand2)
     rtx operand0;
     rtx operand1;
     rtx operand2;
{
  return gen_rtx_SET (VOIDmode,
	operand0,
	gen_rtx_VEC_MERGE (V2DImode,
	operand1,
	gen_rtx_VEC_SELECT (V2DImode,
	operand2,
	gen_rtx_PARALLEL (VOIDmode,
	gen_rtvec (2,
		const1_rtx,
		const0_rtx))),
	const1_rtx));
}

/* ../../gcc/gcc/config/i386/i386.md:21927 */
rtx
gen_sse2_movapd (operand0, operand1)
     rtx operand0;
     rtx operand1;
{
  return gen_rtx_SET (VOIDmode,
	operand0,
	gen_rtx_UNSPEC (V2DFmode,
	gen_rtvec (1,
		operand1),
	38));
}

/* ../../gcc/gcc/config/i386/i386.md:21937 */
rtx
gen_sse2_movupd (operand0, operand1)
     rtx operand0;
     rtx operand1;
{
  return gen_rtx_SET (VOIDmode,
	operand0,
	gen_rtx_UNSPEC (V2DFmode,
	gen_rtvec (1,
		operand1),
	39));
}

/* ../../gcc/gcc/config/i386/i386.md:21947 */
rtx
gen_sse2_movdqa (operand0, operand1)
     rtx operand0;
     rtx operand1;
{
  return gen_rtx_SET (VOIDmode,
	operand0,
	gen_rtx_UNSPEC (V16QImode,
	gen_rtvec (1,
		operand1),
	38));
}

/* ../../gcc/gcc/config/i386/i386.md:21957 */
rtx
gen_sse2_movdqu (operand0, operand1)
     rtx operand0;
     rtx operand1;
{
  return gen_rtx_SET (VOIDmode,
	operand0,
	gen_rtx_UNSPEC (V16QImode,
	gen_rtvec (1,
		operand1),
	39));
}

/* ../../gcc/gcc/config/i386/i386.md:21967 */
rtx
gen_sse2_movdq2q (operand0, operand1)
     rtx operand0;
     rtx operand1;
{
  return gen_rtx_SET (VOIDmode,
	operand0,
	gen_rtx_VEC_SELECT (DImode,
	operand1,
	gen_rtx_PARALLEL (VOIDmode,
	gen_rtvec (1,
		const0_rtx))));
}

/* ../../gcc/gcc/config/i386/i386.md:21978 */
rtx
gen_sse2_movdq2q_rex64 (operand0, operand1)
     rtx operand0;
     rtx operand1;
{
  return gen_rtx_SET (VOIDmode,
	operand0,
	gen_rtx_VEC_SELECT (DImode,
	operand1,
	gen_rtx_PARALLEL (VOIDmode,
	gen_rtvec (1,
		const0_rtx))));
}

/* ../../gcc/gcc/config/i386/i386.md:21990 */
rtx
gen_sse2_movq2dq (operand0, operand1)
     rtx operand0;
     rtx operand1;
{
  return gen_rtx_SET (VOIDmode,
	operand0,
	gen_rtx_VEC_CONCAT (V2DImode,
	operand1,
	const0_rtx));
}

/* ../../gcc/gcc/config/i386/i386.md:22001 */
rtx
gen_sse2_movq2dq_rex64 (operand0, operand1)
     rtx operand0;
     rtx operand1;
{
  return gen_rtx_SET (VOIDmode,
	operand0,
	gen_rtx_VEC_CONCAT (V2DImode,
	operand1,
	const0_rtx));
}

/* ../../gcc/gcc/config/i386/i386.md:22013 */
rtx
gen_sse2_movq (operand0, operand1)
     rtx operand0;
     rtx operand1;
{
  return gen_rtx_SET (VOIDmode,
	operand0,
	gen_rtx_VEC_CONCAT (V2DImode,
	gen_rtx_VEC_SELECT (DImode,
	operand1,
	gen_rtx_PARALLEL (VOIDmode,
	gen_rtvec (1,
		const0_rtx))),
	const0_rtx));
}

/* ../../gcc/gcc/config/i386/i386.md:22024 */
rtx
gen_sse2_loadd (operand0, operand1)
     rtx operand0;
     rtx operand1;
{
  return gen_rtx_SET (VOIDmode,
	operand0,
	gen_rtx_VEC_MERGE (V4SImode,
	gen_rtx_VEC_DUPLICATE (V4SImode,
	operand1),
	gen_rtx_CONST_VECTOR (V4SImode,
	gen_rtvec (4,
		const0_rtx,
		const0_rtx,
		const0_rtx,
		const0_rtx)),
	const1_rtx));
}

/* ../../gcc/gcc/config/i386/i386.md:22038 */
rtx
gen_sse2_stored (operand0, operand1)
     rtx operand0;
     rtx operand1;
{
  return gen_rtx_SET (VOIDmode,
	operand0,
	gen_rtx_VEC_SELECT (SImode,
	operand1,
	gen_rtx_PARALLEL (VOIDmode,
	gen_rtvec (1,
		const0_rtx))));
}

/* ../../gcc/gcc/config/i386/i386.md:22048 */
rtx
gen_sse2_movhpd (operand0, operand1, operand2)
     rtx operand0;
     rtx operand1;
     rtx operand2;
{
  return gen_rtx_SET (VOIDmode,
	operand0,
	gen_rtx_VEC_MERGE (V2DFmode,
	operand1,
	operand2,
	GEN_INT (2LL)));
}

/* ../../gcc/gcc/config/i386/i386.md:22059 */
rtx
gen_sse2_movlpd (operand0, operand1, operand2)
     rtx operand0;
     rtx operand1;
     rtx operand2;
{
  return gen_rtx_SET (VOIDmode,
	operand0,
	gen_rtx_VEC_MERGE (V2DFmode,
	operand1,
	operand2,
	const1_rtx));
}

/* ../../gcc/gcc/config/i386/i386.md:22080 */
rtx
gen_sse2_loadsd_1 (operand0, operand1, operand2)
     rtx operand0;
     rtx operand1;
     rtx operand2;
{
  return gen_rtx_SET (VOIDmode,
	operand0,
	gen_rtx_VEC_MERGE (V2DFmode,
	gen_rtx_VEC_DUPLICATE (V2DFmode,
	operand1),
	operand2,
	const1_rtx));
}

/* ../../gcc/gcc/config/i386/i386.md:22091 */
rtx
gen_sse2_movsd (operand0, operand1, operand2)
     rtx operand0;
     rtx operand1;
     rtx operand2;
{
  return gen_rtx_SET (VOIDmode,
	operand0,
	gen_rtx_VEC_MERGE (V2DFmode,
	operand1,
	operand2,
	const1_rtx));
}

/* ../../gcc/gcc/config/i386/i386.md:22102 */
rtx
gen_sse2_storesd (operand0, operand1)
     rtx operand0;
     rtx operand1;
{
  return gen_rtx_SET (VOIDmode,
	operand0,
	gen_rtx_VEC_SELECT (DFmode,
	operand1,
	gen_rtx_PARALLEL (VOIDmode,
	gen_rtvec (1,
		const0_rtx))));
}

/* ../../gcc/gcc/config/i386/i386.md:22112 */
rtx
gen_sse2_shufpd (operand0, operand1, operand2, operand3)
     rtx operand0;
     rtx operand1;
     rtx operand2;
     rtx operand3;
{
  return gen_rtx_SET (VOIDmode,
	operand0,
	gen_rtx_UNSPEC (V2DFmode,
	gen_rtvec (3,
		operand1,
		operand2,
		operand3),
	41));
}

/* ../../gcc/gcc/config/i386/i386.md:22124 */
rtx
gen_sse2_clflush (operand0)
     rtx operand0;
{
  return gen_rtx_UNSPEC_VOLATILE (VOIDmode,
	gen_rtvec (1,
		operand0),
	57);
}

/* ../../gcc/gcc/config/i386/i386.md:22168 */
rtx
gen_mwait (operand0, operand1)
     rtx operand0;
     rtx operand1;
{
  return gen_rtx_UNSPEC_VOLATILE (VOIDmode,
	gen_rtvec (2,
		operand0,
		operand1),
	70);
}

/* ../../gcc/gcc/config/i386/i386.md:22176 */
rtx
gen_monitor (operand0, operand1, operand2)
     rtx operand0;
     rtx operand1;
     rtx operand2;
{
  return gen_rtx_UNSPEC_VOLATILE (VOIDmode,
	gen_rtvec (3,
		operand0,
		operand1,
		operand2),
	69);
}

/* ../../gcc/gcc/config/i386/i386.md:22187 */
rtx
gen_addsubv4sf3 (operand0, operand1, operand2)
     rtx operand0;
     rtx operand1;
     rtx operand2;
{
  return gen_rtx_SET (VOIDmode,
	operand0,
	gen_rtx_UNSPEC (V4SFmode,
	gen_rtvec (2,
		operand1,
		operand2),
	71));
}

/* ../../gcc/gcc/config/i386/i386.md:22197 */
rtx
gen_addsubv2df3 (operand0, operand1, operand2)
     rtx operand0;
     rtx operand1;
     rtx operand2;
{
  return gen_rtx_SET (VOIDmode,
	operand0,
	gen_rtx_UNSPEC (V2DFmode,
	gen_rtvec (2,
		operand1,
		operand2),
	71));
}

/* ../../gcc/gcc/config/i386/i386.md:22207 */
rtx
gen_haddv4sf3 (operand0, operand1, operand2)
     rtx operand0;
     rtx operand1;
     rtx operand2;
{
  return gen_rtx_SET (VOIDmode,
	operand0,
	gen_rtx_UNSPEC (V4SFmode,
	gen_rtvec (2,
		operand1,
		operand2),
	72));
}

/* ../../gcc/gcc/config/i386/i386.md:22217 */
rtx
gen_haddv2df3 (operand0, operand1, operand2)
     rtx operand0;
     rtx operand1;
     rtx operand2;
{
  return gen_rtx_SET (VOIDmode,
	operand0,
	gen_rtx_UNSPEC (V2DFmode,
	gen_rtvec (2,
		operand1,
		operand2),
	72));
}

/* ../../gcc/gcc/config/i386/i386.md:22227 */
rtx
gen_hsubv4sf3 (operand0, operand1, operand2)
     rtx operand0;
     rtx operand1;
     rtx operand2;
{
  return gen_rtx_SET (VOIDmode,
	operand0,
	gen_rtx_UNSPEC (V4SFmode,
	gen_rtvec (2,
		operand1,
		operand2),
	73));
}

/* ../../gcc/gcc/config/i386/i386.md:22237 */
rtx
gen_hsubv2df3 (operand0, operand1, operand2)
     rtx operand0;
     rtx operand1;
     rtx operand2;
{
  return gen_rtx_SET (VOIDmode,
	operand0,
	gen_rtx_UNSPEC (V2DFmode,
	gen_rtvec (2,
		operand1,
		operand2),
	73));
}

/* ../../gcc/gcc/config/i386/i386.md:22247 */
rtx
gen_movshdup (operand0, operand1)
     rtx operand0;
     rtx operand1;
{
  return gen_rtx_SET (VOIDmode,
	operand0,
	gen_rtx_UNSPEC (V4SFmode,
	gen_rtvec (1,
		operand1),
	74));
}

/* ../../gcc/gcc/config/i386/i386.md:22256 */
rtx
gen_movsldup (operand0, operand1)
     rtx operand0;
     rtx operand1;
{
  return gen_rtx_SET (VOIDmode,
	operand0,
	gen_rtx_UNSPEC (V4SFmode,
	gen_rtvec (1,
		operand1),
	75));
}

/* ../../gcc/gcc/config/i386/i386.md:22265 */
rtx
gen_lddqu (operand0, operand1)
     rtx operand0;
     rtx operand1;
{
  return gen_rtx_SET (VOIDmode,
	operand0,
	gen_rtx_UNSPEC (V16QImode,
	gen_rtvec (1,
		operand1),
	76));
}

/* ../../gcc/gcc/config/i386/i386.md:22274 */
rtx
gen_loadddup (operand0, operand1)
     rtx operand0;
     rtx operand1;
{
  return gen_rtx_SET (VOIDmode,
	operand0,
	gen_rtx_VEC_DUPLICATE (V2DFmode,
	operand1));
}

/* ../../gcc/gcc/config/i386/i386.md:22282 */
rtx
gen_movddup (operand0, operand1)
     rtx operand0;
     rtx operand1;
{
  return gen_rtx_SET (VOIDmode,
	operand0,
	gen_rtx_VEC_DUPLICATE (V2DFmode,
	gen_rtx_VEC_SELECT (DFmode,
	operand1,
	gen_rtx_PARALLEL (VOIDmode,
	gen_rtvec (1,
		const0_rtx)))));
}

/* ../../gcc/gcc/config/i386/i386.md:392 */
rtx
gen_cmpdi (operand0, operand1)
     rtx operand0;
     rtx operand1;
{
  rtx _val = 0;
  start_sequence ();
  {
    rtx operands[2];
    operands[0] = operand0;
    operands[1] = operand1;
{
  if (GET_CODE (operands[0]) == MEM && GET_CODE (operands[1]) == MEM)
    operands[0] = force_reg (DImode, operands[0]);
  ix86_compare_op0 = operands[0];
  ix86_compare_op1 = operands[1];
  DONE;
}
    operand0 = operands[0];
    operand1 = operands[1];
  }
  emit_insn (gen_rtx_SET (VOIDmode,
	gen_rtx_REG (CCmode,
	17),
	gen_rtx_COMPARE (CCmode,
	operand0,
	operand1)));
  _val = get_insns ();
  end_sequence ();
  return _val;
}

/* ../../gcc/gcc/config/i386/i386.md:405 */
rtx
gen_cmpsi (operand0, operand1)
     rtx operand0;
     rtx operand1;
{
  rtx _val = 0;
  start_sequence ();
  {
    rtx operands[2];
    operands[0] = operand0;
    operands[1] = operand1;
{
  if (GET_CODE (operands[0]) == MEM && GET_CODE (operands[1]) == MEM)
    operands[0] = force_reg (SImode, operands[0]);
  ix86_compare_op0 = operands[0];
  ix86_compare_op1 = operands[1];
  DONE;
}
    operand0 = operands[0];
    operand1 = operands[1];
  }
  emit_insn (gen_rtx_SET (VOIDmode,
	gen_rtx_REG (CCmode,
	17),
	gen_rtx_COMPARE (CCmode,
	operand0,
	operand1)));
  _val = get_insns ();
  end_sequence ();
  return _val;
}

/* ../../gcc/gcc/config/i386/i386.md:418 */
rtx
gen_cmphi (operand0, operand1)
     rtx operand0;
     rtx operand1;
{
  rtx _val = 0;
  start_sequence ();
  {
    rtx operands[2];
    operands[0] = operand0;
    operands[1] = operand1;
{
  if (GET_CODE (operands[0]) == MEM && GET_CODE (operands[1]) == MEM)
    operands[0] = force_reg (HImode, operands[0]);
  ix86_compare_op0 = operands[0];
  ix86_compare_op1 = operands[1];
  DONE;
}
    operand0 = operands[0];
    operand1 = operands[1];
  }
  emit_insn (gen_rtx_SET (VOIDmode,
	gen_rtx_REG (CCmode,
	17),
	gen_rtx_COMPARE (CCmode,
	operand0,
	operand1)));
  _val = get_insns ();
  end_sequence ();
  return _val;
}

/* ../../gcc/gcc/config/i386/i386.md:431 */
rtx
gen_cmpqi (operand0, operand1)
     rtx operand0;
     rtx operand1;
{
  rtx _val = 0;
  start_sequence ();
  {
    rtx operands[2];
    operands[0] = operand0;
    operands[1] = operand1;
{
  if (GET_CODE (operands[0]) == MEM && GET_CODE (operands[1]) == MEM)
    operands[0] = force_reg (QImode, operands[0]);
  ix86_compare_op0 = operands[0];
  ix86_compare_op1 = operands[1];
  DONE;
}
    operand0 = operands[0];
    operand1 = operands[1];
  }
  emit_insn (gen_rtx_SET (VOIDmode,
	gen_rtx_REG (CCmode,
	17),
	gen_rtx_COMPARE (CCmode,
	operand0,
	operand1)));
  _val = get_insns ();
  end_sequence ();
  return _val;
}

/* ../../gcc/gcc/config/i386/i386.md:466 */
rtx
gen_cmpdi_1_rex64 (operand0, operand1)
     rtx operand0;
     rtx operand1;
{
  return gen_rtx_SET (VOIDmode,
	gen_rtx_REG (CCmode,
	17),
	gen_rtx_COMPARE (CCmode,
	operand0,
	operand1));
}

/* ../../gcc/gcc/config/i386/i386.md:505 */
rtx
gen_cmpsi_1 (operand0, operand1)
     rtx operand0;
     rtx operand1;
{
  return gen_rtx_SET (VOIDmode,
	gen_rtx_REG (CCmode,
	17),
	gen_rtx_COMPARE (CCmode,
	operand0,
	operand1));
}

/* ../../gcc/gcc/config/i386/i386.md:629 */
rtx
gen_cmpqi_ext_3 (operand0, operand1)
     rtx operand0;
     rtx operand1;
{
  return gen_rtx_SET (VOIDmode,
	gen_rtx_REG (CCmode,
	17),
	gen_rtx_COMPARE (CCmode,
	gen_rtx_SUBREG (QImode,
	gen_rtx_ZERO_EXTRACT (SImode,
	operand0,
	GEN_INT (8LL),
	GEN_INT (8LL)),
	0),
	operand1));
}

/* ../../gcc/gcc/config/i386/i386.md:692 */
rtx
gen_cmpxf (operand0, operand1)
     rtx operand0;
     rtx operand1;
{
  rtx _val = 0;
  start_sequence ();
  {
    rtx operands[2];
    operands[0] = operand0;
    operands[1] = operand1;
{
  ix86_compare_op0 = operands[0];
  ix86_compare_op1 = operands[1];
  DONE;
}
    operand0 = operands[0];
    operand1 = operands[1];
  }
  emit_insn (gen_rtx_SET (VOIDmode,
	gen_rtx_REG (CCmode,
	17),
	gen_rtx_COMPARE (CCmode,
	operand0,
	operand1)));
  _val = get_insns ();
  end_sequence ();
  return _val;
}

/* ../../gcc/gcc/config/i386/i386.md:703 */
rtx
gen_cmptf (operand0, operand1)
     rtx operand0;
     rtx operand1;
{
  rtx _val = 0;
  start_sequence ();
  {
    rtx operands[2];
    operands[0] = operand0;
    operands[1] = operand1;
{
  ix86_compare_op0 = operands[0];
  ix86_compare_op1 = operands[1];
  DONE;
}
    operand0 = operands[0];
    operand1 = operands[1];
  }
  emit_insn (gen_rtx_SET (VOIDmode,
	gen_rtx_REG (CCmode,
	17),
	gen_rtx_COMPARE (CCmode,
	operand0,
	operand1)));
  _val = get_insns ();
  end_sequence ();
  return _val;
}

/* ../../gcc/gcc/config/i386/i386.md:714 */
rtx
gen_cmpdf (operand0, operand1)
     rtx operand0;
     rtx operand1;
{
  rtx _val = 0;
  start_sequence ();
  {
    rtx operands[2];
    operands[0] = operand0;
    operands[1] = operand1;
{
  ix86_compare_op0 = operands[0];
  ix86_compare_op1 = operands[1];
  DONE;
}
    operand0 = operands[0];
    operand1 = operands[1];
  }
  emit_insn (gen_rtx_SET (VOIDmode,
	gen_rtx_REG (CCmode,
	17),
	gen_rtx_COMPARE (CCmode,
	operand0,
	operand1)));
  _val = get_insns ();
  end_sequence ();
  return _val;
}

/* ../../gcc/gcc/config/i386/i386.md:725 */
rtx
gen_cmpsf (operand0, operand1)
     rtx operand0;
     rtx operand1;
{
  rtx _val = 0;
  start_sequence ();
  {
    rtx operands[2];
    operands[0] = operand0;
    operands[1] = operand1;
{
  ix86_compare_op0 = operands[0];
  ix86_compare_op1 = operands[1];
  DONE;
}
    operand0 = operands[0];
    operand1 = operands[1];
  }
  emit_insn (gen_rtx_SET (VOIDmode,
	gen_rtx_REG (CCmode,
	17),
	gen_rtx_COMPARE (CCmode,
	operand0,
	operand1)));
  _val = get_insns ();
  end_sequence ();
  return _val;
}

/* ../../gcc/gcc/config/i386/i386.md:1028 */
rtx
gen_movsi (operand0, operand1)
     rtx operand0;
     rtx operand1;
{
  rtx _val = 0;
  start_sequence ();
  {
    rtx operands[2];
    operands[0] = operand0;
    operands[1] = operand1;
ix86_expand_move (SImode, operands); DONE;
    operand0 = operands[0];
    operand1 = operands[1];
  }
  emit_insn (gen_rtx_SET (VOIDmode,
	operand0,
	operand1));
  _val = get_insns ();
  end_sequence ();
  return _val;
}

/* ../../gcc/gcc/config/i386/i386.md:1198 */
rtx
gen_movhi (operand0, operand1)
     rtx operand0;
     rtx operand1;
{
  rtx _val = 0;
  start_sequence ();
  {
    rtx operands[2];
    operands[0] = operand0;
    operands[1] = operand1;
ix86_expand_move (HImode, operands); DONE;
    operand0 = operands[0];
    operand1 = operands[1];
  }
  emit_insn (gen_rtx_SET (VOIDmode,
	operand0,
	operand1));
  _val = get_insns ();
  end_sequence ();
  return _val;
}

/* ../../gcc/gcc/config/i386/i386.md:1329 */
rtx
gen_movstricthi (operand0, operand1)
     rtx operand0;
     rtx operand1;
{
  rtx _val = 0;
  start_sequence ();
  {
    rtx operands[2];
    operands[0] = operand0;
    operands[1] = operand1;
{
  /* Don't generate memory->memory moves, go through a register */
  if (GET_CODE (operands[0]) == MEM && GET_CODE (operands[1]) == MEM)
    operands[1] = force_reg (HImode, operands[1]);
}
    operand0 = operands[0];
    operand1 = operands[1];
  }
  emit_insn (gen_rtx_SET (VOIDmode,
	gen_rtx_STRICT_LOW_PART (VOIDmode,
	operand0),
	operand1));
  _val = get_insns ();
  end_sequence ();
  return _val;
}

/* ../../gcc/gcc/config/i386/i386.md:1359 */
rtx
gen_movqi (operand0, operand1)
     rtx operand0;
     rtx operand1;
{
  rtx _val = 0;
  start_sequence ();
  {
    rtx operands[2];
    operands[0] = operand0;
    operands[1] = operand1;
ix86_expand_move (QImode, operands); DONE;
    operand0 = operands[0];
    operand1 = operands[1];
  }
  emit_insn (gen_rtx_SET (VOIDmode,
	operand0,
	operand1));
  _val = get_insns ();
  end_sequence ();
  return _val;
}

/* ../../gcc/gcc/config/i386/i386.md:1454 */
rtx
gen_reload_outqi (operand0, operand1, operand2)
     rtx operand0;
     rtx operand1;
     rtx operand2;
{
  rtx _val = 0;
  start_sequence ();
  {
    rtx operands[3];
    operands[0] = operand0;
    operands[1] = operand1;
    operands[2] = operand2;
{
  rtx op0, op1, op2;
  op0 = operands[0]; op1 = operands[1]; op2 = operands[2];

  if (reg_overlap_mentioned_p (op2, op0))
    abort ();
  if (! q_regs_operand (op1, QImode))
    {
      emit_insn (gen_movqi (op2, op1));
      op1 = op2;
    }
  emit_insn (gen_movqi (op0, op1));
  DONE;
}
    operand0 = operands[0];
    operand1 = operands[1];
    operand2 = operands[2];
  }
  emit (gen_rtx_PARALLEL (VOIDmode,
	gen_rtvec (3,
		operand0,
		operand1,
		operand2)));
  _val = get_insns ();
  end_sequence ();
  return _val;
}

/* ../../gcc/gcc/config/i386/i386.md:1487 */
rtx
gen_movstrictqi (operand0, operand1)
     rtx operand0;
     rtx operand1;
{
  rtx _val = 0;
  start_sequence ();
  {
    rtx operands[2];
    operands[0] = operand0;
    operands[1] = operand1;
{
  /* Don't generate memory->memory moves, go through a register.  */
  if (GET_CODE (operands[0]) == MEM && GET_CODE (operands[1]) == MEM)
    operands[1] = force_reg (QImode, operands[1]);
}
    operand0 = operands[0];
    operand1 = operands[1];
  }
  emit_insn (gen_rtx_SET (VOIDmode,
	gen_rtx_STRICT_LOW_PART (VOIDmode,
	operand0),
	operand1));
  _val = get_insns ();
  end_sequence ();
  return _val;
}

/* ../../gcc/gcc/config/i386/i386.md:1716 */
rtx
gen_movdi (operand0, operand1)
     rtx operand0;
     rtx operand1;
{
  rtx _val = 0;
  start_sequence ();
  {
    rtx operands[2];
    operands[0] = operand0;
    operands[1] = operand1;
ix86_expand_move (DImode, operands); DONE;
    operand0 = operands[0];
    operand1 = operands[1];
  }
  emit_insn (gen_rtx_SET (VOIDmode,
	operand0,
	operand1));
  _val = get_insns ();
  end_sequence ();
  return _val;
}

/* ../../gcc/gcc/config/i386/i386.md:1742 */
extern rtx gen_peephole2_1054 PARAMS ((rtx, rtx *));
rtx
gen_peephole2_1054 (curr_insn, operands)
     rtx curr_insn ATTRIBUTE_UNUSED;
     rtx *operands;
{
  rtx operand0;
  rtx operand1;
  rtx operand2;
  rtx _val = 0;
  HARD_REG_SET _regs_allocated;
  CLEAR_HARD_REG_SET (_regs_allocated);
  if ((operands[2] = peep2_find_free_register (0, 0, "r", DImode, &_regs_allocated)) == NULL_RTX)
    return NULL;
  start_sequence ();

  operand0 = operands[0];
  operand1 = operands[1];
  operand2 = operands[2];
  emit_insn (gen_rtx_SET (VOIDmode,
	operand2,
	operand1));
  emit_insn (gen_rtx_SET (VOIDmode,
	operand0,
	copy_rtx (operand2)));
  _val = get_insns ();
  end_sequence ();
  return _val;
}

/* ../../gcc/gcc/config/i386/i386.md:1754 */
extern rtx gen_peephole2_1055 PARAMS ((rtx, rtx *));
rtx
gen_peephole2_1055 (curr_insn, operands)
     rtx curr_insn ATTRIBUTE_UNUSED;
     rtx *operands;
{
  rtx operand0;
  rtx operand1;
  rtx operand2;
  rtx operand3;
  rtx _val = 0;
  HARD_REG_SET _regs_allocated;
  CLEAR_HARD_REG_SET (_regs_allocated);
  start_sequence ();
split_di (operands + 1, 1, operands + 2, operands + 3);
   operands[1] = gen_lowpart (DImode, operands[2]);
   operands[2] = gen_rtx_MEM (SImode, gen_rtx_PLUS (DImode, stack_pointer_rtx,
						    GEN_INT (4)));
  
  operand0 = operands[0];
  operand1 = operands[1];
  operand2 = operands[2];
  operand3 = operands[3];
  emit_insn (gen_rtx_SET (VOIDmode,
	operand0,
	operand1));
  emit_insn (gen_rtx_SET (VOIDmode,
	operand2,
	operand3));
  _val = get_insns ();
  end_sequence ();
  return _val;
}

/* ../../gcc/gcc/config/i386/i386.md:1767 */
extern rtx gen_split_1056 PARAMS ((rtx *));
rtx
gen_split_1056 (operands)
      rtx *operands;
{
  rtx operand0;
  rtx operand1;
  rtx operand2;
  rtx operand3;
  rtx _val = 0;
  start_sequence ();
split_di (operands + 1, 1, operands + 2, operands + 3);
   operands[1] = gen_lowpart (DImode, operands[2]);
   operands[2] = gen_rtx_MEM (SImode, gen_rtx_PLUS (DImode, stack_pointer_rtx,
						    GEN_INT (4)));
  
  operand0 = operands[0];
  operand1 = operands[1];
  operand2 = operands[2];
  operand3 = operands[3];
  emit_insn (gen_rtx_SET (VOIDmode,
	operand0,
	operand1));
  emit_insn (gen_rtx_SET (VOIDmode,
	operand2,
	operand3));
  _val = get_insns ();
  end_sequence ();
  return _val;
}

/* ../../gcc/gcc/config/i386/i386.md:1854 */
extern rtx gen_split_1057 PARAMS ((rtx *));
rtx
gen_split_1057 (operands)
      rtx *operands ATTRIBUTE_UNUSED;
{
  rtx _val = 0;
  start_sequence ();
ix86_split_long_move (operands); DONE;
  emit_insn (const0_rtx);
  _val = get_insns ();
  end_sequence ();
  return _val;
}

/* ../../gcc/gcc/config/i386/i386.md:1863 */
extern rtx gen_split_1058 PARAMS ((rtx *));
rtx
gen_split_1058 (operands)
      rtx *operands ATTRIBUTE_UNUSED;
{
  rtx _val = 0;
  start_sequence ();
ix86_split_long_move (operands); DONE;
  emit_insn (const0_rtx);
  _val = get_insns ();
  end_sequence ();
  return _val;
}

/* ../../gcc/gcc/config/i386/i386.md:1952 */
extern rtx gen_peephole2_1059 PARAMS ((rtx, rtx *));
rtx
gen_peephole2_1059 (curr_insn, operands)
     rtx curr_insn ATTRIBUTE_UNUSED;
     rtx *operands;
{
  rtx operand0;
  rtx operand1;
  rtx operand2;
  rtx _val = 0;
  HARD_REG_SET _regs_allocated;
  CLEAR_HARD_REG_SET (_regs_allocated);
  if ((operands[2] = peep2_find_free_register (0, 0, "r", DImode, &_regs_allocated)) == NULL_RTX)
    return NULL;
  start_sequence ();

  operand0 = operands[0];
  operand1 = operands[1];
  operand2 = operands[2];
  emit_insn (gen_rtx_SET (VOIDmode,
	operand2,
	operand1));
  emit_insn (gen_rtx_SET (VOIDmode,
	operand0,
	copy_rtx (operand2)));
  _val = get_insns ();
  end_sequence ();
  return _val;
}

/* ../../gcc/gcc/config/i386/i386.md:1964 */
extern rtx gen_peephole2_1060 PARAMS ((rtx, rtx *));
rtx
gen_peephole2_1060 (curr_insn, operands)
     rtx curr_insn ATTRIBUTE_UNUSED;
     rtx *operands;
{
  rtx operand0;
  rtx operand1;
  rtx operand2;
  rtx operand3;
  rtx operand4;
  rtx operand5;
  rtx _val = 0;
  HARD_REG_SET _regs_allocated;
  CLEAR_HARD_REG_SET (_regs_allocated);
  start_sequence ();
split_di (operands, 2, operands + 2, operands + 4);
  operand0 = operands[0];
  operand1 = operands[1];
  operand2 = operands[2];
  operand3 = operands[3];
  operand4 = operands[4];
  operand5 = operands[5];
  emit_insn (gen_rtx_SET (VOIDmode,
	operand2,
	operand3));
  emit_insn (gen_rtx_SET (VOIDmode,
	operand4,
	operand5));
  _val = get_insns ();
  end_sequence ();
  return _val;
}

/* ../../gcc/gcc/config/i386/i386.md:1973 */
extern rtx gen_split_1061 PARAMS ((rtx *));
rtx
gen_split_1061 (operands)
      rtx *operands;
{
  rtx operand0;
  rtx operand1;
  rtx operand2;
  rtx operand3;
  rtx operand4;
  rtx operand5;
  rtx _val = 0;
  start_sequence ();
split_di (operands, 2, operands + 2, operands + 4);
  operand0 = operands[0];
  operand1 = operands[1];
  operand2 = operands[2];
  operand3 = operands[3];
  operand4 = operands[4];
  operand5 = operands[5];
  emit_insn (gen_rtx_SET (VOIDmode,
	operand2,
	operand3));
  emit_insn (gen_rtx_SET (VOIDmode,
	operand4,
	operand5));
  _val = get_insns ();
  end_sequence ();
  return _val;
}

/* ../../gcc/gcc/config/i386/i386.md:1998 */
rtx
gen_movsf (operand0, operand1)
     rtx operand0;
     rtx operand1;
{
  rtx _val = 0;
  start_sequence ();
  {
    rtx operands[2];
    operands[0] = operand0;
    operands[1] = operand1;
ix86_expand_move (SFmode, operands); DONE;
    operand0 = operands[0];
    operand1 = operands[1];
  }
  emit_insn (gen_rtx_SET (VOIDmode,
	operand0,
	operand1));
  _val = get_insns ();
  end_sequence ();
  return _val;
}

/* ../../gcc/gcc/config/i386/i386.md:2063 */
extern rtx gen_split_1063 PARAMS ((rtx *));
rtx
gen_split_1063 (operands)
      rtx *operands;
{
  rtx operand0;
  rtx operand1;
  rtx _val = 0;
  start_sequence ();
operands[1] = get_pool_constant (XEXP (operands[1], 0));
  operand0 = operands[0];
  operand1 = operands[1];
  emit_insn (gen_rtx_SET (VOIDmode,
	operand0,
	operand1));
  _val = get_insns ();
  end_sequence ();
  return _val;
}

/* ../../gcc/gcc/config/i386/i386.md:2076 */
extern rtx gen_split_1064 PARAMS ((rtx *));
rtx
gen_split_1064 (operands)
      rtx *operands;
{
  rtx operand0;
  rtx operand1;
  rtx _val = 0;
  start_sequence ();
  operand0 = operands[0];
  operand1 = operands[1];
  emit_insn (gen_rtx_SET (VOIDmode,
	gen_rtx_REG (SImode,
	7),
	gen_rtx_PLUS (SImode,
	gen_rtx_REG (SImode,
	7),
	GEN_INT (-4LL))));
  emit_insn (gen_rtx_SET (VOIDmode,
	gen_rtx_MEM (SFmode,
	gen_rtx_REG (SImode,
	7)),
	operand1));
  _val = get_insns ();
  end_sequence ();
  return _val;
}

/* ../../gcc/gcc/config/i386/i386.md:2083 */
extern rtx gen_split_1065 PARAMS ((rtx *));
rtx
gen_split_1065 (operands)
      rtx *operands;
{
  rtx operand0;
  rtx operand1;
  rtx _val = 0;
  start_sequence ();
  operand0 = operands[0];
  operand1 = operands[1];
  emit_insn (gen_rtx_SET (VOIDmode,
	gen_rtx_REG (DImode,
	7),
	gen_rtx_PLUS (DImode,
	gen_rtx_REG (DImode,
	7),
	GEN_INT (-8LL))));
  emit_insn (gen_rtx_SET (VOIDmode,
	gen_rtx_MEM (SFmode,
	gen_rtx_REG (DImode,
	7)),
	operand1));
  _val = get_insns ();
  end_sequence ();
  return _val;
}

/* ../../gcc/gcc/config/i386/i386.md:2172 */
rtx
gen_movdf (operand0, operand1)
     rtx operand0;
     rtx operand1;
{
  rtx _val = 0;
  start_sequence ();
  {
    rtx operands[2];
    operands[0] = operand0;
    operands[1] = operand1;
ix86_expand_move (DFmode, operands); DONE;
    operand0 = operands[0];
    operand1 = operands[1];
  }
  emit_insn (gen_rtx_SET (VOIDmode,
	operand0,
	operand1));
  _val = get_insns ();
  end_sequence ();
  return _val;
}

/* ../../gcc/gcc/config/i386/i386.md:2248 */
extern rtx gen_split_1067 PARAMS ((rtx *));
rtx
gen_split_1067 (operands)
      rtx *operands;
{
  rtx operand0;
  rtx operand1;
  rtx _val = 0;
  start_sequence ();

  operand0 = operands[0];
  operand1 = operands[1];
  emit_insn (gen_rtx_SET (VOIDmode,
	gen_rtx_REG (SImode,
	7),
	gen_rtx_PLUS (SImode,
	gen_rtx_REG (SImode,
	7),
	GEN_INT (-8LL))));
  emit_insn (gen_rtx_SET (VOIDmode,
	gen_rtx_MEM (DFmode,
	gen_rtx_REG (SImode,
	7)),
	operand1));
  _val = get_insns ();
  end_sequence ();
  return _val;
}

/* ../../gcc/gcc/config/i386/i386.md:2256 */
extern rtx gen_split_1068 PARAMS ((rtx *));
rtx
gen_split_1068 (operands)
      rtx *operands;
{
  rtx operand0;
  rtx operand1;
  rtx _val = 0;
  start_sequence ();

  operand0 = operands[0];
  operand1 = operands[1];
  emit_insn (gen_rtx_SET (VOIDmode,
	gen_rtx_REG (DImode,
	7),
	gen_rtx_PLUS (DImode,
	gen_rtx_REG (DImode,
	7),
	GEN_INT (-8LL))));
  emit_insn (gen_rtx_SET (VOIDmode,
	gen_rtx_MEM (DFmode,
	gen_rtx_REG (DImode,
	7)),
	operand1));
  _val = get_insns ();
  end_sequence ();
  return _val;
}

/* ../../gcc/gcc/config/i386/i386.md:2264 */
extern rtx gen_split_1069 PARAMS ((rtx *));
rtx
gen_split_1069 (operands)
      rtx *operands ATTRIBUTE_UNUSED;
{
  rtx _val = 0;
  start_sequence ();
ix86_split_long_move (operands); DONE;
  emit_insn (const0_rtx);
  _val = get_insns ();
  end_sequence ();
  return _val;
}

/* ../../gcc/gcc/config/i386/i386.md:2398 */
extern rtx gen_split_1070 PARAMS ((rtx *));
rtx
gen_split_1070 (operands)
      rtx *operands ATTRIBUTE_UNUSED;
{
  rtx _val = 0;
  start_sequence ();
ix86_split_long_move (operands); DONE;
  emit_insn (const0_rtx);
  _val = get_insns ();
  end_sequence ();
  return _val;
}

/* ../../gcc/gcc/config/i386/i386.md:2427 */
rtx
gen_movxf (operand0, operand1)
     rtx operand0;
     rtx operand1;
{
  rtx _val = 0;
  start_sequence ();
  {
    rtx operands[2];
    operands[0] = operand0;
    operands[1] = operand1;
ix86_expand_move (XFmode, operands); DONE;
    operand0 = operands[0];
    operand1 = operands[1];
  }
  emit_insn (gen_rtx_SET (VOIDmode,
	operand0,
	operand1));
  _val = get_insns ();
  end_sequence ();
  return _val;
}

/* ../../gcc/gcc/config/i386/i386.md:2433 */
rtx
gen_movtf (operand0, operand1)
     rtx operand0;
     rtx operand1;
{
  rtx _val = 0;
  start_sequence ();
  {
    rtx operands[2];
    operands[0] = operand0;
    operands[1] = operand1;
ix86_expand_move (TFmode, operands); DONE;
    operand0 = operands[0];
    operand1 = operands[1];
  }
  emit_insn (gen_rtx_SET (VOIDmode,
	operand0,
	operand1));
  _val = get_insns ();
  end_sequence ();
  return _val;
}

/* ../../gcc/gcc/config/i386/i386.md:2562 */
extern rtx gen_split_1073 PARAMS ((rtx *));
rtx
gen_split_1073 (operands)
      rtx *operands ATTRIBUTE_UNUSED;
{
  rtx _val = 0;
  start_sequence ();
ix86_split_long_move (operands); DONE;
  emit_insn (const0_rtx);
  _val = get_insns ();
  end_sequence ();
  return _val;
}

/* ../../gcc/gcc/config/i386/i386.md:2573 */
extern rtx gen_split_1074 PARAMS ((rtx *));
rtx
gen_split_1074 (operands)
      rtx *operands;
{
  rtx operand0;
  rtx operand1;
  rtx _val = 0;
  start_sequence ();
  operand0 = operands[0];
  operand1 = operands[1];
  emit_insn (gen_rtx_SET (VOIDmode,
	gen_rtx_REG (SImode,
	7),
	gen_rtx_PLUS (SImode,
	gen_rtx_REG (SImode,
	7),
	GEN_INT (-12LL))));
  emit_insn (gen_rtx_SET (VOIDmode,
	gen_rtx_MEM (XFmode,
	gen_rtx_REG (SImode,
	7)),
	operand1));
  _val = get_insns ();
  end_sequence ();
  return _val;
}

/* ../../gcc/gcc/config/i386/i386.md:2580 */
extern rtx gen_split_1075 PARAMS ((rtx *));
rtx
gen_split_1075 (operands)
      rtx *operands;
{
  rtx operand0;
  rtx operand1;
  rtx _val = 0;
  start_sequence ();
  operand0 = operands[0];
  operand1 = operands[1];
  emit_insn (gen_rtx_SET (VOIDmode,
	gen_rtx_REG (SImode,
	7),
	gen_rtx_PLUS (SImode,
	gen_rtx_REG (SImode,
	7),
	GEN_INT (-16LL))));
  emit_insn (gen_rtx_SET (VOIDmode,
	gen_rtx_MEM (TFmode,
	gen_rtx_REG (SImode,
	7)),
	operand1));
  _val = get_insns ();
  end_sequence ();
  return _val;
}

/* ../../gcc/gcc/config/i386/i386.md:2587 */
extern rtx gen_split_1076 PARAMS ((rtx *));
rtx
gen_split_1076 (operands)
      rtx *operands;
{
  rtx operand0;
  rtx operand1;
  rtx _val = 0;
  start_sequence ();
  operand0 = operands[0];
  operand1 = operands[1];
  emit_insn (gen_rtx_SET (VOIDmode,
	gen_rtx_REG (DImode,
	7),
	gen_rtx_PLUS (DImode,
	gen_rtx_REG (DImode,
	7),
	GEN_INT (-16LL))));
  emit_insn (gen_rtx_SET (VOIDmode,
	gen_rtx_MEM (TFmode,
	gen_rtx_REG (DImode,
	7)),
	operand1));
  _val = get_insns ();
  end_sequence ();
  return _val;
}

/* ../../gcc/gcc/config/i386/i386.md:2783 */
extern rtx gen_split_1077 PARAMS ((rtx *));
rtx
gen_split_1077 (operands)
      rtx *operands ATTRIBUTE_UNUSED;
{
  rtx _val = 0;
  start_sequence ();
ix86_split_long_move (operands); DONE;
  emit_insn (const0_rtx);
  _val = get_insns ();
  end_sequence ();
  return _val;
}

/* ../../gcc/gcc/config/i386/i386.md:2798 */
extern rtx gen_split_1078 PARAMS ((rtx *));
rtx
gen_split_1078 (operands)
      rtx *operands;
{
  rtx operand0;
  rtx operand1;
  rtx _val = 0;
  start_sequence ();
operands[1] = get_pool_constant (XEXP (operands[1], 0));
  operand0 = operands[0];
  operand1 = operands[1];
  emit_insn (gen_rtx_SET (VOIDmode,
	operand0,
	operand1));
  _val = get_insns ();
  end_sequence ();
  return _val;
}

/* ../../gcc/gcc/config/i386/i386.md:2851 */
rtx
gen_zero_extendhisi2 (operand0, operand1)
     rtx operand0;
     rtx operand1;
{
  rtx _val = 0;
  start_sequence ();
  {
    rtx operands[2];
    operands[0] = operand0;
    operands[1] = operand1;
{
  if (TARGET_ZERO_EXTEND_WITH_AND && !optimize_size)
    {
      operands[1] = force_reg (HImode, operands[1]);
      emit_insn (gen_zero_extendhisi2_and (operands[0], operands[1]));
      DONE;
    }
}
    operand0 = operands[0];
    operand1 = operands[1];
  }
  emit_insn (gen_rtx_SET (VOIDmode,
	operand0,
	gen_rtx_ZERO_EXTEND (SImode,
	operand1)));
  _val = get_insns ();
  end_sequence ();
  return _val;
}

/* ../../gcc/gcc/config/i386/i386.md:2873 */
extern rtx gen_split_1080 PARAMS ((rtx *));
rtx
gen_split_1080 (operands)
      rtx *operands;
{
  rtx operand0;
  rtx _val = 0;
  start_sequence ();

  operand0 = operands[0];
  emit (gen_rtx_PARALLEL (VOIDmode,
	gen_rtvec (2,
		gen_rtx_SET (VOIDmode,
	operand0,
	gen_rtx_AND (SImode,
	copy_rtx (operand0),
	GEN_INT (65535LL))),
		gen_rtx_CLOBBER (VOIDmode,
	gen_rtx_REG (CCmode,
	17)))));
  _val = get_insns ();
  end_sequence ();
  return _val;
}

/* ../../gcc/gcc/config/i386/i386.md:2890 */
rtx
gen_zero_extendqihi2 (operand0, operand1)
     rtx operand0;
     rtx operand1;
{
  return gen_rtx_PARALLEL (VOIDmode,
	gen_rtvec (2,
		gen_rtx_SET (VOIDmode,
	operand0,
	gen_rtx_ZERO_EXTEND (HImode,
	operand1)),
		gen_rtx_CLOBBER (VOIDmode,
	gen_rtx_REG (CCmode,
	17))));
}

/* ../../gcc/gcc/config/i386/i386.md:2925 */
extern rtx gen_split_1082 PARAMS ((rtx *));
rtx
gen_split_1082 (operands)
      rtx *operands;
{
  rtx operand0;
  rtx operand1;
  rtx _val = 0;
  start_sequence ();
  operand0 = operands[0];
  operand1 = operands[1];
  emit_insn (gen_rtx_SET (VOIDmode,
	operand0,
	gen_rtx_ZERO_EXTEND (HImode,
	operand1)));
  _val = get_insns ();
  end_sequence ();
  return _val;
}

/* ../../gcc/gcc/config/i386/i386.md:2937 */
extern rtx gen_split_1083 PARAMS ((rtx *));
rtx
gen_split_1083 (operands)
      rtx *operands;
{
  rtx operand0;
  rtx operand1;
  rtx operand2;
  rtx _val = 0;
  start_sequence ();
operands[2] = gen_lowpart (QImode, operands[0]);
  operand0 = operands[0];
  operand1 = operands[1];
  operand2 = operands[2];
  emit_insn (gen_rtx_SET (VOIDmode,
	operand0,
	const0_rtx));
  emit_insn (gen_rtx_SET (VOIDmode,
	gen_rtx_STRICT_LOW_PART (VOIDmode,
	operand2),
	operand1));
  _val = get_insns ();
  end_sequence ();
  return _val;
}

/* ../../gcc/gcc/config/i386/i386.md:2950 */
extern rtx gen_split_1084 PARAMS ((rtx *));
rtx
gen_split_1084 (operands)
      rtx *operands;
{
  rtx operand0;
  rtx _val = 0;
  start_sequence ();

  operand0 = operands[0];
  emit (gen_rtx_PARALLEL (VOIDmode,
	gen_rtvec (2,
		gen_rtx_SET (VOIDmode,
	operand0,
	gen_rtx_AND (HImode,
	copy_rtx (operand0),
	GEN_INT (255LL))),
		gen_rtx_CLOBBER (VOIDmode,
	gen_rtx_REG (CCmode,
	17)))));
  _val = get_insns ();
  end_sequence ();
  return _val;
}

/* ../../gcc/gcc/config/i386/i386.md:2960 */
rtx
gen_zero_extendqisi2 (operand0, operand1)
     rtx operand0;
     rtx operand1;
{
  return gen_rtx_PARALLEL (VOIDmode,
	gen_rtvec (2,
		gen_rtx_SET (VOIDmode,
	operand0,
	gen_rtx_ZERO_EXTEND (SImode,
	operand1)),
		gen_rtx_CLOBBER (VOIDmode,
	gen_rtx_REG (CCmode,
	17))));
}

/* ../../gcc/gcc/config/i386/i386.md:2995 */
extern rtx gen_split_1086 PARAMS ((rtx *));
rtx
gen_split_1086 (operands)
      rtx *operands;
{
  rtx operand0;
  rtx operand1;
  rtx _val = 0;
  start_sequence ();
  operand0 = operands[0];
  operand1 = operands[1];
  emit_insn (gen_rtx_SET (VOIDmode,
	operand0,
	gen_rtx_ZERO_EXTEND (SImode,
	operand1)));
  _val = get_insns ();
  end_sequence ();
  return _val;
}

/* ../../gcc/gcc/config/i386/i386.md:3007 */
extern rtx gen_split_1087 PARAMS ((rtx *));
rtx
gen_split_1087 (operands)
      rtx *operands;
{
  rtx operand0;
  rtx operand1;
  rtx operand2;
  rtx _val = 0;
  start_sequence ();
operands[2] = gen_lowpart (QImode, operands[0]);
  operand0 = operands[0];
  operand1 = operands[1];
  operand2 = operands[2];
  emit_insn (gen_rtx_SET (VOIDmode,
	operand0,
	const0_rtx));
  emit_insn (gen_rtx_SET (VOIDmode,
	gen_rtx_STRICT_LOW_PART (VOIDmode,
	operand2),
	operand1));
  _val = get_insns ();
  end_sequence ();
  return _val;
}

/* ../../gcc/gcc/config/i386/i386.md:3021 */
extern rtx gen_split_1088 PARAMS ((rtx *));
rtx
gen_split_1088 (operands)
      rtx *operands;
{
  rtx operand0;
  rtx _val = 0;
  start_sequence ();

  operand0 = operands[0];
  emit (gen_rtx_PARALLEL (VOIDmode,
	gen_rtvec (2,
		gen_rtx_SET (VOIDmode,
	operand0,
	gen_rtx_AND (SImode,
	copy_rtx (operand0),
	GEN_INT (255LL))),
		gen_rtx_CLOBBER (VOIDmode,
	gen_rtx_REG (CCmode,
	17)))));
  _val = get_insns ();
  end_sequence ();
  return _val;
}

/* ../../gcc/gcc/config/i386/i386.md:3032 */
rtx
gen_zero_extendsidi2 (operand0, operand1)
     rtx operand0;
     rtx operand1;
{
  rtx _val = 0;
  start_sequence ();
  {
    rtx operands[2];
    operands[0] = operand0;
    operands[1] = operand1;
if (!TARGET_64BIT)
     {
       emit_insn (gen_zero_extendsidi2_32 (operands[0], operands[1]));
       DONE;
     }
  
    operand0 = operands[0];
    operand1 = operands[1];
  }
  emit_insn (gen_rtx_SET (VOIDmode,
	operand0,
	gen_rtx_ZERO_EXTEND (DImode,
	operand1)));
  _val = get_insns ();
  end_sequence ();
  return _val;
}

/* ../../gcc/gcc/config/i386/i386.md:3061 */
extern rtx gen_split_1090 PARAMS ((rtx *));
rtx
gen_split_1090 (operands)
      rtx *operands;
{
  rtx operand0;
  rtx operand1;
  rtx operand2;
  rtx operand3;
  rtx operand4;
  rtx _val = 0;
  start_sequence ();
split_di (&operands[0], 1, &operands[3], &operands[4]);
  operand0 = operands[0];
  operand1 = operands[1];
  operand2 = operands[2];
  operand3 = operands[3];
  operand4 = operands[4];
  emit_insn (gen_rtx_SET (VOIDmode,
	operand4,
	const0_rtx));
  _val = get_insns ();
  end_sequence ();
  return _val;
}

/* ../../gcc/gcc/config/i386/i386.md:3068 */
extern rtx gen_split_1091 PARAMS ((rtx *));
rtx
gen_split_1091 (operands)
      rtx *operands;
{
  rtx operand0;
  rtx operand1;
  rtx operand2;
  rtx operand3;
  rtx operand4;
  rtx _val = 0;
  start_sequence ();
split_di (&operands[0], 1, &operands[3], &operands[4]);
  operand0 = operands[0];
  operand1 = operands[1];
  operand2 = operands[2];
  operand3 = operands[3];
  operand4 = operands[4];
  emit_insn (gen_rtx_SET (VOIDmode,
	operand4,
	const0_rtx));
  _val = get_insns ();
  end_sequence ();
  return _val;
}

/* ../../gcc/gcc/config/i386/i386.md:3077 */
extern rtx gen_split_1092 PARAMS ((rtx *));
rtx
gen_split_1092 (operands)
      rtx *operands;
{
  rtx operand0;
  rtx operand1;
  rtx operand2;
  rtx operand3;
  rtx operand4;
  rtx _val = 0;
  start_sequence ();
split_di (&operands[0], 1, &operands[3], &operands[4]);
  operand0 = operands[0];
  operand1 = operands[1];
  operand2 = operands[2];
  operand3 = operands[3];
  operand4 = operands[4];
  emit_insn (gen_rtx_SET (VOIDmode,
	operand3,
	operand1));
  emit_insn (gen_rtx_SET (VOIDmode,
	operand4,
	const0_rtx));
  _val = get_insns ();
  end_sequence ();
  return _val;
}

/* ../../gcc/gcc/config/i386/i386.md:3108 */
rtx
gen_extendsidi2 (operand0, operand1)
     rtx operand0;
     rtx operand1;
{
  rtx operand2 ATTRIBUTE_UNUSED;
  rtx _val = 0;
  start_sequence ();
  {
    rtx operands[3];
    operands[0] = operand0;
    operands[1] = operand1;
{
  if (TARGET_64BIT)
    {
      emit_insn (gen_extendsidi2_rex64 (operands[0], operands[1]));
      DONE;
    }
}
    operand0 = operands[0];
    operand1 = operands[1];
    operand2 = operands[2];
  }
  emit (gen_rtx_PARALLEL (VOIDmode,
	gen_rtvec (3,
		gen_rtx_SET (VOIDmode,
	operand0,
	gen_rtx_SIGN_EXTEND (DImode,
	operand1)),
		gen_rtx_CLOBBER (VOIDmode,
	gen_rtx_REG (CCmode,
	17)),
		gen_rtx_CLOBBER (VOIDmode,
	gen_rtx_SCRATCH (SImode)))));
  _val = get_insns ();
  end_sequence ();
  return _val;
}

/* ../../gcc/gcc/config/i386/i386.md:3159 */
extern rtx gen_split_1094 PARAMS ((rtx *));
rtx
gen_split_1094 (operands)
      rtx *operands;
{
  rtx operand0;
  rtx operand1;
  rtx operand2;
  rtx operand3;
  rtx operand4;
  rtx _val = 0;
  start_sequence ();
split_di (&operands[0], 1, &operands[3], &operands[4]);
  operand0 = operands[0];
  operand1 = operands[1];
  operand2 = operands[2];
  operand3 = operands[3];
  operand4 = operands[4];
  emit_insn (gen_rtx_SET (VOIDmode,
	operand3,
	operand1));
  emit (gen_rtx_PARALLEL (VOIDmode,
	gen_rtvec (2,
		gen_rtx_SET (VOIDmode,
	copy_rtx (operand1),
	gen_rtx_ASHIFTRT (SImode,
	copy_rtx (operand1),
	GEN_INT (31LL))),
		gen_rtx_CLOBBER (VOIDmode,
	gen_rtx_REG (CCmode,
	17)))));
  emit_insn (gen_rtx_SET (VOIDmode,
	operand4,
	copy_rtx (operand1)));
  _val = get_insns ();
  end_sequence ();
  return _val;
}

/* ../../gcc/gcc/config/i386/i386.md:3174 */
extern rtx gen_split_1095 PARAMS ((rtx *));
rtx
gen_split_1095 (operands)
      rtx *operands ATTRIBUTE_UNUSED;
{
  rtx _val = 0;
  start_sequence ();
{
  split_di (&operands[0], 1, &operands[3], &operands[4]);

  emit_move_insn (operands[3], operands[1]);

  /* Generate a cltd if possible and doing so it profitable.  */
  if (true_regnum (operands[1]) == 0
      && true_regnum (operands[2]) == 1
      && (optimize_size || TARGET_USE_CLTD))
    {
      emit_insn (gen_ashrsi3_31 (operands[2], operands[1], GEN_INT (31)));
    }
  else
    {
      emit_move_insn (operands[2], operands[1]);
      emit_insn (gen_ashrsi3_31 (operands[2], operands[2], GEN_INT (31)));
    }
  emit_move_insn (operands[4], operands[2]);
  DONE;
}
  emit_insn (const0_rtx);
  _val = get_insns ();
  end_sequence ();
  return _val;
}

/* ../../gcc/gcc/config/i386/i386.md:3204 */
extern rtx gen_split_1096 PARAMS ((rtx *));
rtx
gen_split_1096 (operands)
      rtx *operands ATTRIBUTE_UNUSED;
{
  rtx _val = 0;
  start_sequence ();
{
  split_di (&operands[0], 1, &operands[3], &operands[4]);

  if (true_regnum (operands[3]) != true_regnum (operands[1]))
    emit_move_insn (operands[3], operands[1]);

  /* Generate a cltd if possible and doing so it profitable.  */
  if (true_regnum (operands[3]) == 0
      && (optimize_size || TARGET_USE_CLTD))
    {
      emit_insn (gen_ashrsi3_31 (operands[4], operands[3], GEN_INT (31)));
      DONE;
    }

  if (true_regnum (operands[4]) != true_regnum (operands[1]))
    emit_move_insn (operands[4], operands[1]);

  emit_insn (gen_ashrsi3_31 (operands[4], operands[4], GEN_INT (31)));
  DONE;
}
  emit_insn (const0_rtx);
  _val = get_insns ();
  end_sequence ();
  return _val;
}

/* ../../gcc/gcc/config/i386/i386.md:3340 */
extern rtx gen_split_1097 PARAMS ((rtx *));
rtx
gen_split_1097 (operands)
      rtx *operands;
{
  rtx operand0;
  rtx operand1;
  rtx _val = 0;
  start_sequence ();
  operand0 = operands[0];
  operand1 = operands[1];
  emit_insn (gen_rtx_SET (VOIDmode,
	gen_rtx_REG (SImode,
	7),
	gen_rtx_PLUS (SImode,
	gen_rtx_REG (SImode,
	7),
	GEN_INT (-8LL))));
  emit_insn (gen_rtx_SET (VOIDmode,
	gen_rtx_MEM (DFmode,
	gen_rtx_REG (SImode,
	7)),
	gen_rtx_FLOAT_EXTEND (DFmode,
	operand1)));
  _val = get_insns ();
  end_sequence ();
  return _val;
}

/* ../../gcc/gcc/config/i386/i386.md:3347 */
extern rtx gen_split_1098 PARAMS ((rtx *));
rtx
gen_split_1098 (operands)
      rtx *operands;
{
  rtx operand0;
  rtx operand1;
  rtx _val = 0;
  start_sequence ();
  operand0 = operands[0];
  operand1 = operands[1];
  emit_insn (gen_rtx_SET (VOIDmode,
	gen_rtx_REG (DImode,
	7),
	gen_rtx_PLUS (DImode,
	gen_rtx_REG (DImode,
	7),
	GEN_INT (-8LL))));
  emit_insn (gen_rtx_SET (VOIDmode,
	gen_rtx_MEM (DFmode,
	gen_rtx_REG (DImode,
	7)),
	gen_rtx_FLOAT_EXTEND (DFmode,
	operand1)));
  _val = get_insns ();
  end_sequence ();
  return _val;
}

/* ../../gcc/gcc/config/i386/i386.md:3360 */
extern rtx gen_split_1099 PARAMS ((rtx *));
rtx
gen_split_1099 (operands)
      rtx *operands;
{
  rtx operand0;
  rtx operand1;
  rtx _val = 0;
  start_sequence ();
  operand0 = operands[0];
  operand1 = operands[1];
  emit_insn (gen_rtx_SET (VOIDmode,
	gen_rtx_REG (SImode,
	7),
	gen_rtx_PLUS (SImode,
	gen_rtx_REG (SImode,
	7),
	GEN_INT (-12LL))));
  emit_insn (gen_rtx_SET (VOIDmode,
	gen_rtx_MEM (XFmode,
	gen_rtx_REG (SImode,
	7)),
	gen_rtx_FLOAT_EXTEND (XFmode,
	operand1)));
  _val = get_insns ();
  end_sequence ();
  return _val;
}

/* ../../gcc/gcc/config/i386/i386.md:3373 */
extern rtx gen_split_1100 PARAMS ((rtx *));
rtx
gen_split_1100 (operands)
      rtx *operands;
{
  rtx operand0;
  rtx operand1;
  rtx _val = 0;
  start_sequence ();
  operand0 = operands[0];
  operand1 = operands[1];
  emit_insn (gen_rtx_SET (VOIDmode,
	gen_rtx_REG (SImode,
	7),
	gen_rtx_PLUS (SImode,
	gen_rtx_REG (SImode,
	7),
	GEN_INT (-16LL))));
  emit_insn (gen_rtx_SET (VOIDmode,
	gen_rtx_MEM (TFmode,
	gen_rtx_REG (SImode,
	7)),
	gen_rtx_FLOAT_EXTEND (TFmode,
	operand1)));
  _val = get_insns ();
  end_sequence ();
  return _val;
}

/* ../../gcc/gcc/config/i386/i386.md:3380 */
extern rtx gen_split_1101 PARAMS ((rtx *));
rtx
gen_split_1101 (operands)
      rtx *operands;
{
  rtx operand0;
  rtx operand1;
  rtx _val = 0;
  start_sequence ();
  operand0 = operands[0];
  operand1 = operands[1];
  emit_insn (gen_rtx_SET (VOIDmode,
	gen_rtx_REG (DImode,
	7),
	gen_rtx_PLUS (DImode,
	gen_rtx_REG (DImode,
	7),
	GEN_INT (-16LL))));
  emit_insn (gen_rtx_SET (VOIDmode,
	gen_rtx_MEM (DFmode,
	gen_rtx_REG (DImode,
	7)),
	gen_rtx_FLOAT_EXTEND (TFmode,
	operand1)));
  _val = get_insns ();
  end_sequence ();
  return _val;
}

/* ../../gcc/gcc/config/i386/i386.md:3393 */
extern rtx gen_split_1102 PARAMS ((rtx *));
rtx
gen_split_1102 (operands)
      rtx *operands;
{
  rtx operand0;
  rtx operand1;
  rtx _val = 0;
  start_sequence ();
  operand0 = operands[0];
  operand1 = operands[1];
  emit_insn (gen_rtx_SET (VOIDmode,
	gen_rtx_REG (SImode,
	7),
	gen_rtx_PLUS (SImode,
	gen_rtx_REG (SImode,
	7),
	GEN_INT (-12LL))));
  emit_insn (gen_rtx_SET (VOIDmode,
	gen_rtx_MEM (DFmode,
	gen_rtx_REG (SImode,
	7)),
	gen_rtx_FLOAT_EXTEND (XFmode,
	operand1)));
  _val = get_insns ();
  end_sequence ();
  return _val;
}

/* ../../gcc/gcc/config/i386/i386.md:3406 */
extern rtx gen_split_1103 PARAMS ((rtx *));
rtx
gen_split_1103 (operands)
      rtx *operands;
{
  rtx operand0;
  rtx operand1;
  rtx _val = 0;
  start_sequence ();
  operand0 = operands[0];
  operand1 = operands[1];
  emit_insn (gen_rtx_SET (VOIDmode,
	gen_rtx_REG (SImode,
	7),
	gen_rtx_PLUS (SImode,
	gen_rtx_REG (SImode,
	7),
	GEN_INT (-16LL))));
  emit_insn (gen_rtx_SET (VOIDmode,
	gen_rtx_MEM (TFmode,
	gen_rtx_REG (SImode,
	7)),
	gen_rtx_FLOAT_EXTEND (XFmode,
	operand1)));
  _val = get_insns ();
  end_sequence ();
  return _val;
}

/* ../../gcc/gcc/config/i386/i386.md:3413 */
extern rtx gen_split_1104 PARAMS ((rtx *));
rtx
gen_split_1104 (operands)
      rtx *operands;
{
  rtx operand0;
  rtx operand1;
  rtx _val = 0;
  start_sequence ();
  operand0 = operands[0];
  operand1 = operands[1];
  emit_insn (gen_rtx_SET (VOIDmode,
	gen_rtx_REG (DImode,
	7),
	gen_rtx_PLUS (DImode,
	gen_rtx_REG (DImode,
	7),
	GEN_INT (-16LL))));
  emit_insn (gen_rtx_SET (VOIDmode,
	gen_rtx_MEM (TFmode,
	gen_rtx_REG (DImode,
	7)),
	gen_rtx_FLOAT_EXTEND (TFmode,
	operand1)));
  _val = get_insns ();
  end_sequence ();
  return _val;
}

/* ../../gcc/gcc/config/i386/i386.md:3420 */
rtx
gen_extendsfdf2 (operand0, operand1)
     rtx operand0;
     rtx operand1;
{
  rtx _val = 0;
  start_sequence ();
  {
    rtx operands[2];
    operands[0] = operand0;
    operands[1] = operand1;
{
  /* ??? Needed for compress_float_constant since all fp constants
     are LEGITIMATE_CONSTANT_P.  */
  if (GET_CODE (operands[1]) == CONST_DOUBLE)
    operands[1] = validize_mem (force_const_mem (SFmode, operands[1]));
  if (GET_CODE (operands[0]) == MEM && GET_CODE (operands[1]) == MEM)
    operands[1] = force_reg (SFmode, operands[1]);
}
    operand0 = operands[0];
    operand1 = operands[1];
  }
  emit_insn (gen_rtx_SET (VOIDmode,
	operand0,
	gen_rtx_FLOAT_EXTEND (DFmode,
	operand1)));
  _val = get_insns ();
  end_sequence ();
  return _val;
}

/* ../../gcc/gcc/config/i386/i386.md:3475 */
rtx
gen_extendsfxf2 (operand0, operand1)
     rtx operand0;
     rtx operand1;
{
  rtx _val = 0;
  start_sequence ();
  {
    rtx operands[2];
    operands[0] = operand0;
    operands[1] = operand1;
{
  /* ??? Needed for compress_float_constant since all fp constants
     are LEGITIMATE_CONSTANT_P.  */
  if (GET_CODE (operands[1]) == CONST_DOUBLE)
    operands[1] = validize_mem (force_const_mem (SFmode, operands[1]));
  if (GET_CODE (operands[0]) == MEM && GET_CODE (operands[1]) == MEM)
    operands[1] = force_reg (SFmode, operands[1]);
}
    operand0 = operands[0];
    operand1 = operands[1];
  }
  emit_insn (gen_rtx_SET (VOIDmode,
	operand0,
	gen_rtx_FLOAT_EXTEND (XFmode,
	operand1)));
  _val = get_insns ();
  end_sequence ();
  return _val;
}

/* ../../gcc/gcc/config/i386/i386.md:3520 */
rtx
gen_extendsftf2 (operand0, operand1)
     rtx operand0;
     rtx operand1;
{
  rtx _val = 0;
  start_sequence ();
  {
    rtx operands[2];
    operands[0] = operand0;
    operands[1] = operand1;
{
  /* ??? Needed for compress_float_constant since all fp constants
     are LEGITIMATE_CONSTANT_P.  */
  if (GET_CODE (operands[1]) == CONST_DOUBLE)
    operands[1] = validize_mem (force_const_mem (SFmode, operands[1]));
  if (GET_CODE (operands[0]) == MEM && GET_CODE (operands[1]) == MEM)
    operands[1] = force_reg (SFmode, operands[1]);
}
    operand0 = operands[0];
    operand1 = operands[1];
  }
  emit_insn (gen_rtx_SET (VOIDmode,
	operand0,
	gen_rtx_FLOAT_EXTEND (TFmode,
	operand1)));
  _val = get_insns ();
  end_sequence ();
  return _val;
}

/* ../../gcc/gcc/config/i386/i386.md:3565 */
rtx
gen_extenddfxf2 (operand0, operand1)
     rtx operand0;
     rtx operand1;
{
  rtx _val = 0;
  start_sequence ();
  {
    rtx operands[2];
    operands[0] = operand0;
    operands[1] = operand1;
{
  /* ??? Needed for compress_float_constant since all fp constants
     are LEGITIMATE_CONSTANT_P.  */
  if (GET_CODE (operands[1]) == CONST_DOUBLE)
    operands[1] = validize_mem (force_const_mem (DFmode, operands[1]));
  if (GET_CODE (operands[0]) == MEM && GET_CODE (operands[1]) == MEM)
    operands[1] = force_reg (DFmode, operands[1]);
}
    operand0 = operands[0];
    operand1 = operands[1];
  }
  emit_insn (gen_rtx_SET (VOIDmode,
	operand0,
	gen_rtx_FLOAT_EXTEND (XFmode,
	operand1)));
  _val = get_insns ();
  end_sequence ();
  return _val;
}

/* ../../gcc/gcc/config/i386/i386.md:3610 */
rtx
gen_extenddftf2 (operand0, operand1)
     rtx operand0;
     rtx operand1;
{
  rtx _val = 0;
  start_sequence ();
  {
    rtx operands[2];
    operands[0] = operand0;
    operands[1] = operand1;
{
  /* ??? Needed for compress_float_constant since all fp constants
     are LEGITIMATE_CONSTANT_P.  */
  if (GET_CODE (operands[1]) == CONST_DOUBLE)
    operands[1] = validize_mem (force_const_mem (DFmode, operands[1]));
  if (GET_CODE (operands[0]) == MEM && GET_CODE (operands[1]) == MEM)
    operands[1] = force_reg (DFmode, operands[1]);
}
    operand0 = operands[0];
    operand1 = operands[1];
  }
  emit_insn (gen_rtx_SET (VOIDmode,
	operand0,
	gen_rtx_FLOAT_EXTEND (TFmode,
	operand1)));
  _val = get_insns ();
  end_sequence ();
  return _val;
}

/* ../../gcc/gcc/config/i386/i386.md:3661 */
rtx
gen_truncdfsf2 (operand0, operand1)
     rtx operand0;
     rtx operand1;
{
  rtx operand2;
  rtx _val = 0;
  start_sequence ();
  {
    rtx operands[3];
    operands[0] = operand0;
    operands[1] = operand1;

   if (TARGET_80387)
     operands[2] = assign_386_stack_local (SFmode, 0);
   else
     {
	emit_insn (gen_truncdfsf2_sse_only (operands[0], operands[1]));
	DONE;
     }

    operand0 = operands[0];
    operand1 = operands[1];
    operand2 = operands[2];
  }
  emit (gen_rtx_PARALLEL (VOIDmode,
	gen_rtvec (2,
		gen_rtx_SET (VOIDmode,
	operand0,
	gen_rtx_FLOAT_TRUNCATE (SFmode,
	operand1)),
		gen_rtx_CLOBBER (VOIDmode,
	operand2))));
  _val = get_insns ();
  end_sequence ();
  return _val;
}

/* ../../gcc/gcc/config/i386/i386.md:3767 */
extern rtx gen_split_1111 PARAMS ((rtx *));
rtx
gen_split_1111 (operands)
      rtx *operands;
{
  rtx operand0;
  rtx operand1;
  rtx _val = 0;
  start_sequence ();

  operand0 = operands[0];
  operand1 = operands[1];
  emit_insn (gen_rtx_SET (VOIDmode,
	operand0,
	gen_rtx_FLOAT_TRUNCATE (SFmode,
	operand1)));
  _val = get_insns ();
  end_sequence ();
  return _val;
}

/* ../../gcc/gcc/config/i386/i386.md:3776 */
extern rtx gen_split_1112 PARAMS ((rtx *));
rtx
gen_split_1112 (operands)
      rtx *operands;
{
  rtx operand0;
  rtx operand1;
  rtx _val = 0;
  start_sequence ();

  operand0 = operands[0];
  operand1 = operands[1];
  emit_insn (gen_rtx_SET (VOIDmode,
	operand0,
	gen_rtx_FLOAT_TRUNCATE (SFmode,
	operand1)));
  _val = get_insns ();
  end_sequence ();
  return _val;
}

/* ../../gcc/gcc/config/i386/i386.md:3786 */
extern rtx gen_split_1113 PARAMS ((rtx *));
rtx
gen_split_1113 (operands)
      rtx *operands;
{
  rtx operand0;
  rtx operand1;
  rtx operand2;
  rtx _val = 0;
  start_sequence ();

  operand0 = operands[0];
  operand1 = operands[1];
  operand2 = operands[2];
  emit_insn (gen_rtx_SET (VOIDmode,
	operand2,
	gen_rtx_FLOAT_TRUNCATE (SFmode,
	operand1)));
  emit_insn (gen_rtx_SET (VOIDmode,
	operand0,
	copy_rtx (operand2)));
  _val = get_insns ();
  end_sequence ();
  return _val;
}

/* ../../gcc/gcc/config/i386/i386.md:3796 */
rtx
gen_truncxfsf2 (operand0, operand1)
     rtx operand0;
     rtx operand1;
{
  rtx operand2;
  rtx _val = 0;
  start_sequence ();
  {
    rtx operands[3];
    operands[0] = operand0;
    operands[1] = operand1;
operands[2] = assign_386_stack_local (SFmode, 0);
    operand0 = operands[0];
    operand1 = operands[1];
    operand2 = operands[2];
  }
  emit (gen_rtx_PARALLEL (VOIDmode,
	gen_rtvec (2,
		gen_rtx_SET (VOIDmode,
	operand0,
	gen_rtx_FLOAT_TRUNCATE (SFmode,
	operand1)),
		gen_rtx_CLOBBER (VOIDmode,
	operand2))));
  _val = get_insns ();
  end_sequence ();
  return _val;
}

/* ../../gcc/gcc/config/i386/i386.md:3839 */
extern rtx gen_split_1115 PARAMS ((rtx *));
rtx
gen_split_1115 (operands)
      rtx *operands;
{
  rtx operand0;
  rtx operand1;
  rtx _val = 0;
  start_sequence ();

  operand0 = operands[0];
  operand1 = operands[1];
  emit_insn (gen_rtx_SET (VOIDmode,
	operand0,
	gen_rtx_FLOAT_TRUNCATE (SFmode,
	operand1)));
  _val = get_insns ();
  end_sequence ();
  return _val;
}

/* ../../gcc/gcc/config/i386/i386.md:3848 */
extern rtx gen_split_1116 PARAMS ((rtx *));
rtx
gen_split_1116 (operands)
      rtx *operands;
{
  rtx operand0;
  rtx operand1;
  rtx operand2;
  rtx _val = 0;
  start_sequence ();

  operand0 = operands[0];
  operand1 = operands[1];
  operand2 = operands[2];
  emit_insn (gen_rtx_SET (VOIDmode,
	operand2,
	gen_rtx_FLOAT_TRUNCATE (SFmode,
	operand1)));
  emit_insn (gen_rtx_SET (VOIDmode,
	operand0,
	copy_rtx (operand2)));
  _val = get_insns ();
  end_sequence ();
  return _val;
}

/* ../../gcc/gcc/config/i386/i386.md:3858 */
rtx
gen_trunctfsf2 (operand0, operand1)
     rtx operand0;
     rtx operand1;
{
  rtx operand2;
  rtx _val = 0;
  start_sequence ();
  {
    rtx operands[3];
    operands[0] = operand0;
    operands[1] = operand1;
operands[2] = assign_386_stack_local (SFmode, 0);
    operand0 = operands[0];
    operand1 = operands[1];
    operand2 = operands[2];
  }
  emit (gen_rtx_PARALLEL (VOIDmode,
	gen_rtvec (2,
		gen_rtx_SET (VOIDmode,
	operand0,
	gen_rtx_FLOAT_TRUNCATE (SFmode,
	operand1)),
		gen_rtx_CLOBBER (VOIDmode,
	operand2))));
  _val = get_insns ();
  end_sequence ();
  return _val;
}

/* ../../gcc/gcc/config/i386/i386.md:3901 */
extern rtx gen_split_1118 PARAMS ((rtx *));
rtx
gen_split_1118 (operands)
      rtx *operands;
{
  rtx operand0;
  rtx operand1;
  rtx _val = 0;
  start_sequence ();

  operand0 = operands[0];
  operand1 = operands[1];
  emit_insn (gen_rtx_SET (VOIDmode,
	operand0,
	gen_rtx_FLOAT_TRUNCATE (SFmode,
	operand1)));
  _val = get_insns ();
  end_sequence ();
  return _val;
}

/* ../../gcc/gcc/config/i386/i386.md:3910 */
extern rtx gen_split_1119 PARAMS ((rtx *));
rtx
gen_split_1119 (operands)
      rtx *operands;
{
  rtx operand0;
  rtx operand1;
  rtx operand2;
  rtx _val = 0;
  start_sequence ();

  operand0 = operands[0];
  operand1 = operands[1];
  operand2 = operands[2];
  emit_insn (gen_rtx_SET (VOIDmode,
	operand2,
	gen_rtx_FLOAT_TRUNCATE (SFmode,
	operand1)));
  emit_insn (gen_rtx_SET (VOIDmode,
	operand0,
	copy_rtx (operand2)));
  _val = get_insns ();
  end_sequence ();
  return _val;
}

/* ../../gcc/gcc/config/i386/i386.md:3921 */
rtx
gen_truncxfdf2 (operand0, operand1)
     rtx operand0;
     rtx operand1;
{
  rtx operand2;
  rtx _val = 0;
  start_sequence ();
  {
    rtx operands[3];
    operands[0] = operand0;
    operands[1] = operand1;
operands[2] = assign_386_stack_local (DFmode, 0);
    operand0 = operands[0];
    operand1 = operands[1];
    operand2 = operands[2];
  }
  emit (gen_rtx_PARALLEL (VOIDmode,
	gen_rtvec (2,
		gen_rtx_SET (VOIDmode,
	operand0,
	gen_rtx_FLOAT_TRUNCATE (DFmode,
	operand1)),
		gen_rtx_CLOBBER (VOIDmode,
	operand2))));
  _val = get_insns ();
  end_sequence ();
  return _val;
}

/* ../../gcc/gcc/config/i386/i386.md:3965 */
extern rtx gen_split_1121 PARAMS ((rtx *));
rtx
gen_split_1121 (operands)
      rtx *operands;
{
  rtx operand0;
  rtx operand1;
  rtx _val = 0;
  start_sequence ();

  operand0 = operands[0];
  operand1 = operands[1];
  emit_insn (gen_rtx_SET (VOIDmode,
	operand0,
	gen_rtx_FLOAT_TRUNCATE (DFmode,
	operand1)));
  _val = get_insns ();
  end_sequence ();
  return _val;
}

/* ../../gcc/gcc/config/i386/i386.md:3974 */
extern rtx gen_split_1122 PARAMS ((rtx *));
rtx
gen_split_1122 (operands)
      rtx *operands;
{
  rtx operand0;
  rtx operand1;
  rtx operand2;
  rtx _val = 0;
  start_sequence ();

  operand0 = operands[0];
  operand1 = operands[1];
  operand2 = operands[2];
  emit_insn (gen_rtx_SET (VOIDmode,
	operand2,
	gen_rtx_FLOAT_TRUNCATE (DFmode,
	operand1)));
  emit_insn (gen_rtx_SET (VOIDmode,
	operand0,
	copy_rtx (operand2)));
  _val = get_insns ();
  end_sequence ();
  return _val;
}

/* ../../gcc/gcc/config/i386/i386.md:3984 */
rtx
gen_trunctfdf2 (operand0, operand1)
     rtx operand0;
     rtx operand1;
{
  rtx operand2;
  rtx _val = 0;
  start_sequence ();
  {
    rtx operands[3];
    operands[0] = operand0;
    operands[1] = operand1;
operands[2] = assign_386_stack_local (DFmode, 0);
    operand0 = operands[0];
    operand1 = operands[1];
    operand2 = operands[2];
  }
  emit (gen_rtx_PARALLEL (VOIDmode,
	gen_rtvec (2,
		gen_rtx_SET (VOIDmode,
	operand0,
	gen_rtx_FLOAT_TRUNCATE (DFmode,
	operand1)),
		gen_rtx_CLOBBER (VOIDmode,
	operand2))));
  _val = get_insns ();
  end_sequence ();
  return _val;
}

/* ../../gcc/gcc/config/i386/i386.md:4028 */
extern rtx gen_split_1124 PARAMS ((rtx *));
rtx
gen_split_1124 (operands)
      rtx *operands;
{
  rtx operand0;
  rtx operand1;
  rtx _val = 0;
  start_sequence ();

  operand0 = operands[0];
  operand1 = operands[1];
  emit_insn (gen_rtx_SET (VOIDmode,
	operand0,
	gen_rtx_FLOAT_TRUNCATE (DFmode,
	operand1)));
  _val = get_insns ();
  end_sequence ();
  return _val;
}

/* ../../gcc/gcc/config/i386/i386.md:4037 */
extern rtx gen_split_1125 PARAMS ((rtx *));
rtx
gen_split_1125 (operands)
      rtx *operands;
{
  rtx operand0;
  rtx operand1;
  rtx operand2;
  rtx _val = 0;
  start_sequence ();

  operand0 = operands[0];
  operand1 = operands[1];
  operand2 = operands[2];
  emit_insn (gen_rtx_SET (VOIDmode,
	operand2,
	gen_rtx_FLOAT_TRUNCATE (DFmode,
	operand1)));
  emit_insn (gen_rtx_SET (VOIDmode,
	operand0,
	copy_rtx (operand2)));
  _val = get_insns ();
  end_sequence ();
  return _val;
}

/* ../../gcc/gcc/config/i386/i386.md:4052 */
rtx
gen_fix_truncxfdi2 (operand0, operand1)
     rtx operand0;
     rtx operand1;
{
  return gen_rtx_SET (VOIDmode,
	operand0,
	gen_rtx_FIX (DImode,
	operand1));
}

/* ../../gcc/gcc/config/i386/i386.md:4058 */
rtx
gen_fix_trunctfdi2 (operand0, operand1)
     rtx operand0;
     rtx operand1;
{
  return gen_rtx_SET (VOIDmode,
	operand0,
	gen_rtx_FIX (DImode,
	operand1));
}

/* ../../gcc/gcc/config/i386/i386.md:4064 */
rtx
gen_fix_truncdfdi2 (operand0, operand1)
     rtx operand0;
     rtx operand1;
{
  rtx _val = 0;
  start_sequence ();
  {
    rtx operands[2];
    operands[0] = operand0;
    operands[1] = operand1;
{
  if (TARGET_64BIT && TARGET_SSE2)
   {
     rtx out = REG_P (operands[0]) ? operands[0] : gen_reg_rtx (DImode);
     emit_insn (gen_fix_truncdfdi_sse (out, operands[1]));
     if (out != operands[0])
	emit_move_insn (operands[0], out);
     DONE;
   }
}
    operand0 = operands[0];
    operand1 = operands[1];
  }
  emit_insn (gen_rtx_SET (VOIDmode,
	operand0,
	gen_rtx_FIX (DImode,
	operand1)));
  _val = get_insns ();
  end_sequence ();
  return _val;
}

/* ../../gcc/gcc/config/i386/i386.md:4079 */
rtx
gen_fix_truncsfdi2 (operand0, operand1)
     rtx operand0;
     rtx operand1;
{
  rtx _val = 0;
  start_sequence ();
  {
    rtx operands[2];
    operands[0] = operand0;
    operands[1] = operand1;
{
  if (TARGET_SSE && TARGET_64BIT)
   {
     rtx out = REG_P (operands[0]) ? operands[0] : gen_reg_rtx (DImode);
     emit_insn (gen_fix_truncsfdi_sse (out, operands[1]));
     if (out != operands[0])
	emit_move_insn (operands[0], out);
     DONE;
   }
}
    operand0 = operands[0];
    operand1 = operands[1];
  }
  emit_insn (gen_rtx_SET (VOIDmode,
	operand0,
	gen_rtx_FIX (DImode,
	operand1)));
  _val = get_insns ();
  end_sequence ();
  return _val;
}

/* ../../gcc/gcc/config/i386/i386.md:4096 */
extern rtx gen_split_1130 PARAMS ((rtx *));
rtx
gen_split_1130 (operands)
      rtx *operands ATTRIBUTE_UNUSED;
{
  rtx _val = 0;
  start_sequence ();
{
  operands[2] = assign_386_stack_local (HImode, 1);
  operands[3] = assign_386_stack_local (HImode, 2);
  if (memory_operand (operands[0], VOIDmode))
    emit_insn (gen_fix_truncdi_memory (operands[0], operands[1],
				       operands[2], operands[3]));
  else
    {
      operands[4] = assign_386_stack_local (DImode, 0);
      emit_insn (gen_fix_truncdi_nomemory (operands[0], operands[1],
					   operands[2], operands[3],
					   operands[4]));
    }
  DONE;
}
  emit_insn (const0_rtx);
  _val = get_insns ();
  end_sequence ();
  return _val;
}

/* ../../gcc/gcc/config/i386/i386.md:4145 */
extern rtx gen_split_1131 PARAMS ((rtx *));
rtx
gen_split_1131 (operands)
      rtx *operands;
{
  rtx operand0;
  rtx operand1;
  rtx operand2;
  rtx operand3;
  rtx operand4;
  rtx operand5;
  rtx _val = 0;
  start_sequence ();

  operand0 = operands[0];
  operand1 = operands[1];
  operand2 = operands[2];
  operand3 = operands[3];
  operand4 = operands[4];
  operand5 = operands[5];
  emit (gen_rtx_PARALLEL (VOIDmode,
	gen_rtvec (4,
		gen_rtx_SET (VOIDmode,
	operand4,
	gen_rtx_FIX (DImode,
	operand1)),
		gen_rtx_USE (VOIDmode,
	operand2),
		gen_rtx_USE (VOIDmode,
	operand3),
		gen_rtx_CLOBBER (VOIDmode,
	operand5))));
  emit_insn (gen_rtx_SET (VOIDmode,
	operand0,
	copy_rtx (operand4)));
  _val = get_insns ();
  end_sequence ();
  return _val;
}

/* ../../gcc/gcc/config/i386/i386.md:4160 */
extern rtx gen_split_1132 PARAMS ((rtx *));
rtx
gen_split_1132 (operands)
      rtx *operands;
{
  rtx operand0;
  rtx operand1;
  rtx operand2;
  rtx operand3;
  rtx operand4;
  rtx operand5;
  rtx _val = 0;
  start_sequence ();

  operand0 = operands[0];
  operand1 = operands[1];
  operand2 = operands[2];
  operand3 = operands[3];
  operand4 = operands[4];
  operand5 = operands[5];
  emit (gen_rtx_PARALLEL (VOIDmode,
	gen_rtvec (4,
		gen_rtx_SET (VOIDmode,
	operand0,
	gen_rtx_FIX (DImode,
	operand1)),
		gen_rtx_USE (VOIDmode,
	operand2),
		gen_rtx_USE (VOIDmode,
	operand3),
		gen_rtx_CLOBBER (VOIDmode,
	operand5))));
  _val = get_insns ();
  end_sequence ();
  return _val;
}

/* ../../gcc/gcc/config/i386/i386.md:4191 */
rtx
gen_fix_truncxfsi2 (operand0, operand1)
     rtx operand0;
     rtx operand1;
{
  return gen_rtx_SET (VOIDmode,
	operand0,
	gen_rtx_FIX (SImode,
	operand1));
}

/* ../../gcc/gcc/config/i386/i386.md:4197 */
rtx
gen_fix_trunctfsi2 (operand0, operand1)
     rtx operand0;
     rtx operand1;
{
  return gen_rtx_SET (VOIDmode,
	operand0,
	gen_rtx_FIX (SImode,
	operand1));
}

/* ../../gcc/gcc/config/i386/i386.md:4203 */
rtx
gen_fix_truncdfsi2 (operand0, operand1)
     rtx operand0;
     rtx operand1;
{
  rtx _val = 0;
  start_sequence ();
  {
    rtx operands[2];
    operands[0] = operand0;
    operands[1] = operand1;
{
  if (TARGET_SSE2)
   {
     rtx out = REG_P (operands[0]) ? operands[0] : gen_reg_rtx (SImode);
     emit_insn (gen_fix_truncdfsi_sse (out, operands[1]));
     if (out != operands[0])
	emit_move_insn (operands[0], out);
     DONE;
   }
}
    operand0 = operands[0];
    operand1 = operands[1];
  }
  emit_insn (gen_rtx_SET (VOIDmode,
	operand0,
	gen_rtx_FIX (SImode,
	operand1)));
  _val = get_insns ();
  end_sequence ();
  return _val;
}

/* ../../gcc/gcc/config/i386/i386.md:4218 */
rtx
gen_fix_truncsfsi2 (operand0, operand1)
     rtx operand0;
     rtx operand1;
{
  rtx _val = 0;
  start_sequence ();
  {
    rtx operands[2];
    operands[0] = operand0;
    operands[1] = operand1;
{
  if (TARGET_SSE)
   {
     rtx out = REG_P (operands[0]) ? operands[0] : gen_reg_rtx (SImode);
     emit_insn (gen_fix_truncsfsi_sse (out, operands[1]));
     if (out != operands[0])
	emit_move_insn (operands[0], out);
     DONE;
   }
}
    operand0 = operands[0];
    operand1 = operands[1];
  }
  emit_insn (gen_rtx_SET (VOIDmode,
	operand0,
	gen_rtx_FIX (SImode,
	operand1)));
  _val = get_insns ();
  end_sequence ();
  return _val;
}

/* ../../gcc/gcc/config/i386/i386.md:4235 */
extern rtx gen_split_1137 PARAMS ((rtx *));
rtx
gen_split_1137 (operands)
      rtx *operands ATTRIBUTE_UNUSED;
{
  rtx _val = 0;
  start_sequence ();
{
  operands[2] = assign_386_stack_local (HImode, 1);
  operands[3] = assign_386_stack_local (HImode, 2);
  if (memory_operand (operands[0], VOIDmode))
    emit_insn (gen_fix_truncsi_memory (operands[0], operands[1],
				       operands[2], operands[3]));
  else
    {
      operands[4] = assign_386_stack_local (SImode, 0);
      emit_insn (gen_fix_truncsi_nomemory (operands[0], operands[1],
					   operands[2], operands[3],
					   operands[4]));
    }
  DONE;
}
  emit_insn (const0_rtx);
  _val = get_insns ();
  end_sequence ();
  return _val;
}

/* ../../gcc/gcc/config/i386/i386.md:4297 */
extern rtx gen_split_1138 PARAMS ((rtx *));
rtx
gen_split_1138 (operands)
      rtx *operands;
{
  rtx operand0;
  rtx operand1;
  rtx operand2;
  rtx operand3;
  rtx operand4;
  rtx _val = 0;
  start_sequence ();

  operand0 = operands[0];
  operand1 = operands[1];
  operand2 = operands[2];
  operand3 = operands[3];
  operand4 = operands[4];
  emit (gen_rtx_PARALLEL (VOIDmode,
	gen_rtvec (3,
		gen_rtx_SET (VOIDmode,
	operand4,
	gen_rtx_FIX (SImode,
	operand1)),
		gen_rtx_USE (VOIDmode,
	operand2),
		gen_rtx_USE (VOIDmode,
	operand3))));
  emit_insn (gen_rtx_SET (VOIDmode,
	operand0,
	copy_rtx (operand4)));
  _val = get_insns ();
  end_sequence ();
  return _val;
}

/* ../../gcc/gcc/config/i386/i386.md:4310 */
extern rtx gen_split_1139 PARAMS ((rtx *));
rtx
gen_split_1139 (operands)
      rtx *operands;
{
  rtx operand0;
  rtx operand1;
  rtx operand2;
  rtx operand3;
  rtx _val = 0;
  start_sequence ();

  operand0 = operands[0];
  operand1 = operands[1];
  operand2 = operands[2];
  operand3 = operands[3];
  emit (gen_rtx_PARALLEL (VOIDmode,
	gen_rtvec (3,
		gen_rtx_SET (VOIDmode,
	operand0,
	gen_rtx_FIX (SImode,
	operand1)),
		gen_rtx_USE (VOIDmode,
	operand2),
		gen_rtx_USE (VOIDmode,
	operand3))));
  _val = get_insns ();
  end_sequence ();
  return _val;
}

/* ../../gcc/gcc/config/i386/i386.md:4324 */
rtx
gen_fix_truncxfhi2 (operand0, operand1)
     rtx operand0;
     rtx operand1;
{
  return gen_rtx_SET (VOIDmode,
	operand0,
	gen_rtx_FIX (HImode,
	operand1));
}

/* ../../gcc/gcc/config/i386/i386.md:4330 */
rtx
gen_fix_trunctfhi2 (operand0, operand1)
     rtx operand0;
     rtx operand1;
{
  return gen_rtx_SET (VOIDmode,
	operand0,
	gen_rtx_FIX (HImode,
	operand1));
}

/* ../../gcc/gcc/config/i386/i386.md:4336 */
rtx
gen_fix_truncdfhi2 (operand0, operand1)
     rtx operand0;
     rtx operand1;
{
  return gen_rtx_SET (VOIDmode,
	operand0,
	gen_rtx_FIX (HImode,
	operand1));
}

/* ../../gcc/gcc/config/i386/i386.md:4342 */
rtx
gen_fix_truncsfhi2 (operand0, operand1)
     rtx operand0;
     rtx operand1;
{
  return gen_rtx_SET (VOIDmode,
	operand0,
	gen_rtx_FIX (HImode,
	operand1));
}

/* ../../gcc/gcc/config/i386/i386.md:4350 */
extern rtx gen_split_1144 PARAMS ((rtx *));
rtx
gen_split_1144 (operands)
      rtx *operands ATTRIBUTE_UNUSED;
{
  rtx _val = 0;
  start_sequence ();
{
  operands[2] = assign_386_stack_local (HImode, 1);
  operands[3] = assign_386_stack_local (HImode, 2);
  if (memory_operand (operands[0], VOIDmode))
    emit_insn (gen_fix_trunchi_memory (operands[0], operands[1],
				       operands[2], operands[3]));
  else
    {
      operands[4] = assign_386_stack_local (HImode, 0);
      emit_insn (gen_fix_trunchi_nomemory (operands[0], operands[1],
					   operands[2], operands[3],
					   operands[4]));
    }
  DONE;
}
  emit_insn (const0_rtx);
  _val = get_insns ();
  end_sequence ();
  return _val;
}

/* ../../gcc/gcc/config/i386/i386.md:4397 */
extern rtx gen_split_1145 PARAMS ((rtx *));
rtx
gen_split_1145 (operands)
      rtx *operands;
{
  rtx operand0;
  rtx operand1;
  rtx operand2;
  rtx operand3;
  rtx _val = 0;
  start_sequence ();

  operand0 = operands[0];
  operand1 = operands[1];
  operand2 = operands[2];
  operand3 = operands[3];
  emit (gen_rtx_PARALLEL (VOIDmode,
	gen_rtvec (3,
		gen_rtx_SET (VOIDmode,
	operand0,
	gen_rtx_FIX (HImode,
	operand1)),
		gen_rtx_USE (VOIDmode,
	operand2),
		gen_rtx_USE (VOIDmode,
	operand3))));
  _val = get_insns ();
  end_sequence ();
  return _val;
}

/* ../../gcc/gcc/config/i386/i386.md:4409 */
extern rtx gen_split_1146 PARAMS ((rtx *));
rtx
gen_split_1146 (operands)
      rtx *operands;
{
  rtx operand0;
  rtx operand1;
  rtx operand2;
  rtx operand3;
  rtx operand4;
  rtx _val = 0;
  start_sequence ();

  operand0 = operands[0];
  operand1 = operands[1];
  operand2 = operands[2];
  operand3 = operands[3];
  operand4 = operands[4];
  emit (gen_rtx_PARALLEL (VOIDmode,
	gen_rtvec (4,
		gen_rtx_SET (VOIDmode,
	operand4,
	gen_rtx_FIX (HImode,
	operand1)),
		gen_rtx_USE (VOIDmode,
	operand2),
		gen_rtx_USE (VOIDmode,
	operand3),
		gen_rtx_CLOBBER (VOIDmode,
	copy_rtx (operand4)))));
  emit_insn (gen_rtx_SET (VOIDmode,
	operand0,
	copy_rtx (operand4)));
  _val = get_insns ();
  end_sequence ();
  return _val;
}

/* ../../gcc/gcc/config/i386/i386.md:4461 */
rtx
gen_floatsisf2 (operand0, operand1)
     rtx operand0;
     rtx operand1;
{
  return gen_rtx_SET (VOIDmode,
	operand0,
	gen_rtx_FLOAT (SFmode,
	operand1));
}

/* ../../gcc/gcc/config/i386/i386.md:4488 */
rtx
gen_floatdisf2 (operand0, operand1)
     rtx operand0;
     rtx operand1;
{
  return gen_rtx_SET (VOIDmode,
	operand0,
	gen_rtx_FLOAT (SFmode,
	operand1));
}

/* ../../gcc/gcc/config/i386/i386.md:4537 */
rtx
gen_floatsidf2 (operand0, operand1)
     rtx operand0;
     rtx operand1;
{
  return gen_rtx_SET (VOIDmode,
	operand0,
	gen_rtx_FLOAT (DFmode,
	operand1));
}

/* ../../gcc/gcc/config/i386/i386.md:4564 */
rtx
gen_floatdidf2 (operand0, operand1)
     rtx operand0;
     rtx operand1;
{
  return gen_rtx_SET (VOIDmode,
	operand0,
	gen_rtx_FLOAT (DFmode,
	operand1));
}

/* ../../gcc/gcc/config/i386/i386.md:4669 */
extern rtx gen_split_1151 PARAMS ((rtx *));
rtx
gen_split_1151 (operands)
      rtx *operands ATTRIBUTE_UNUSED;
{
  rtx _val = 0;
  start_sequence ();
{
  operands[2] = ix86_force_to_memory (GET_MODE (operands[1]), operands[1]);
  operands[2] = gen_rtx_FLOAT (GET_MODE (operands[0]), operands[2]);
  emit_insn (gen_rtx_SET (VOIDmode, operands[0], operands[2]));
  ix86_free_from_memory (GET_MODE (operands[1]));
  DONE;
}
  emit_insn (const0_rtx);
  _val = get_insns ();
  end_sequence ();
  return _val;
}

/* ../../gcc/gcc/config/i386/i386.md:4689 */
rtx
gen_adddi3 (operand0, operand1, operand2)
     rtx operand0;
     rtx operand1;
     rtx operand2;
{
  rtx _val = 0;
  start_sequence ();
  {
    rtx operands[3];
    operands[0] = operand0;
    operands[1] = operand1;
    operands[2] = operand2;
ix86_expand_binary_operator (PLUS, DImode, operands); DONE;
    operand0 = operands[0];
    operand1 = operands[1];
    operand2 = operands[2];
  }
  emit_insn (gen_rtx_SET (VOIDmode,
	operand0,
	gen_rtx_PLUS (DImode,
	operand1,
	operand2)));
  emit_insn (gen_rtx_CLOBBER (VOIDmode,
	gen_rtx_REG (CCmode,
	17)));
  _val = get_insns ();
  end_sequence ();
  return _val;
}

/* ../../gcc/gcc/config/i386/i386.md:4705 */
extern rtx gen_split_1153 PARAMS ((rtx *));
rtx
gen_split_1153 (operands)
      rtx *operands;
{
  rtx operand0;
  rtx operand1;
  rtx operand2;
  rtx operand3;
  rtx operand4;
  rtx operand5;
  rtx _val = 0;
  start_sequence ();
split_di (operands+0, 1, operands+0, operands+3);
   split_di (operands+1, 1, operands+1, operands+4);
   split_di (operands+2, 1, operands+2, operands+5);
  operand0 = operands[0];
  operand1 = operands[1];
  operand2 = operands[2];
  operand3 = operands[3];
  operand4 = operands[4];
  operand5 = operands[5];
  emit (gen_rtx_PARALLEL (VOIDmode,
	gen_rtvec (2,
		gen_rtx_SET (VOIDmode,
	gen_rtx_REG (CCmode,
	17),
	gen_rtx_UNSPEC (CCmode,
	gen_rtvec (2,
		operand1,
		operand2),
	27)),
		gen_rtx_SET (VOIDmode,
	operand0,
	gen_rtx_PLUS (SImode,
	copy_rtx (operand1),
	copy_rtx (operand2))))));
  emit (gen_rtx_PARALLEL (VOIDmode,
	gen_rtvec (2,
		gen_rtx_SET (VOIDmode,
	operand3,
	gen_rtx_PLUS (SImode,
	gen_rtx_PLUS (SImode,
	gen_rtx_LTU (SImode,
	gen_rtx_REG (CCmode,
	17),
	const0_rtx),
	operand4),
	operand5)),
		gen_rtx_CLOBBER (VOIDmode,
	gen_rtx_REG (CCmode,
	17)))));
  _val = get_insns ();
  end_sequence ();
  return _val;
}

/* ../../gcc/gcc/config/i386/i386.md:4799 */
rtx
gen_addsi3 (operand0, operand1, operand2)
     rtx operand0;
     rtx operand1;
     rtx operand2;
{
  rtx _val = 0;
  start_sequence ();
  {
    rtx operands[3];
    operands[0] = operand0;
    operands[1] = operand1;
    operands[2] = operand2;
ix86_expand_binary_operator (PLUS, SImode, operands); DONE;
    operand0 = operands[0];
    operand1 = operands[1];
    operand2 = operands[2];
  }
  emit (gen_rtx_PARALLEL (VOIDmode,
	gen_rtvec (2,
		gen_rtx_SET (VOIDmode,
	operand0,
	gen_rtx_PLUS (SImode,
	operand1,
	operand2)),
		gen_rtx_CLOBBER (VOIDmode,
	gen_rtx_REG (CCmode,
	17)))));
  _val = get_insns ();
  end_sequence ();
  return _val;
}

/* ../../gcc/gcc/config/i386/i386.md:4843 */
extern rtx gen_split_1155 PARAMS ((rtx *));
rtx
gen_split_1155 (operands)
      rtx *operands ATTRIBUTE_UNUSED;
{
  rtx _val = 0;
  start_sequence ();
{
  rtx pat;
  operands[0] = gen_lowpart (SImode, operands[0]);
  operands[1] = gen_lowpart (Pmode, operands[1]);
  operands[2] = gen_lowpart (Pmode, operands[2]);
  operands[3] = gen_lowpart (Pmode, operands[3]);
  pat = gen_rtx_PLUS (Pmode, gen_rtx_PLUS (Pmode, operands[1], operands[2]),
  		      operands[3]);
  if (Pmode != SImode)
    pat = gen_rtx_SUBREG (SImode, pat, 0);
  emit_insn (gen_rtx_SET (VOIDmode, operands[0], pat));
  DONE;
}
  emit_insn (const0_rtx);
  _val = get_insns ();
  end_sequence ();
  return _val;
}

/* ../../gcc/gcc/config/i386/i386.md:4874 */
extern rtx gen_split_1156 PARAMS ((rtx *));
rtx
gen_split_1156 (operands)
      rtx *operands;
{
  rtx operand0;
  rtx operand1;
  rtx operand2;
  rtx operand3;
  rtx _val = 0;
  start_sequence ();
{
  operands[1] = gen_lowpart (Pmode, operands[1]);
  operands[2] = gen_lowpart (Pmode, operands[2]);
  operands[3] = gen_lowpart (Pmode, operands[3]);
}
  operand0 = operands[0];
  operand1 = operands[1];
  operand2 = operands[2];
  operand3 = operands[3];
  emit_insn (gen_rtx_SET (VOIDmode,
	operand0,
	gen_rtx_ZERO_EXTEND (DImode,
	gen_rtx_SUBREG (SImode,
	gen_rtx_PLUS (DImode,
	gen_rtx_PLUS (DImode,
	operand1,
	operand2),
	operand3),
	0))));
  _val = get_insns ();
  end_sequence ();
  return _val;
}

/* ../../gcc/gcc/config/i386/i386.md:4895 */
extern rtx gen_split_1157 PARAMS ((rtx *));
rtx
gen_split_1157 (operands)
      rtx *operands ATTRIBUTE_UNUSED;
{
  rtx _val = 0;
  start_sequence ();
{
  rtx pat;
  operands[0] = gen_lowpart (SImode, operands[0]);
  operands[1] = gen_lowpart (Pmode, operands[1]);
  operands[3] = gen_lowpart (Pmode, operands[3]);
  pat = gen_rtx_PLUS (Pmode, gen_rtx_MULT (Pmode, operands[1], operands[2]),
  		      operands[3]);
  if (Pmode != SImode)
    pat = gen_rtx_SUBREG (SImode, pat, 0);
  emit_insn (gen_rtx_SET (VOIDmode, operands[0], pat));
  DONE;
}
  emit_insn (const0_rtx);
  _val = get_insns ();
  end_sequence ();
  return _val;
}

/* ../../gcc/gcc/config/i386/i386.md:4924 */
extern rtx gen_split_1158 PARAMS ((rtx *));
rtx
gen_split_1158 (operands)
      rtx *operands;
{
  rtx operand0;
  rtx operand1;
  rtx operand2;
  rtx operand3;
  rtx _val = 0;
  start_sequence ();
{
  operands[1] = gen_lowpart (Pmode, operands[1]);
  operands[3] = gen_lowpart (Pmode, operands[3]);
}
  operand0 = operands[0];
  operand1 = operands[1];
  operand2 = operands[2];
  operand3 = operands[3];
  emit_insn (gen_rtx_SET (VOIDmode,
	operand0,
	gen_rtx_ZERO_EXTEND (DImode,
	gen_rtx_SUBREG (SImode,
	gen_rtx_PLUS (DImode,
	gen_rtx_MULT (DImode,
	operand1,
	operand2),
	operand3),
	0))));
  _val = get_insns ();
  end_sequence ();
  return _val;
}

/* ../../gcc/gcc/config/i386/i386.md:4944 */
extern rtx gen_split_1159 PARAMS ((rtx *));
rtx
gen_split_1159 (operands)
      rtx *operands ATTRIBUTE_UNUSED;
{
  rtx _val = 0;
  start_sequence ();
{
  rtx pat;
  operands[0] = gen_lowpart (SImode, operands[0]);
  operands[1] = gen_lowpart (Pmode, operands[1]);
  operands[3] = gen_lowpart (Pmode, operands[3]);
  operands[4] = gen_lowpart (Pmode, operands[4]);
  pat = gen_rtx_PLUS (Pmode,
  		      gen_rtx_PLUS (Pmode, gen_rtx_MULT (Pmode, operands[1],
		      					 operands[2]),
				    operands[3]),
  		      operands[4]);
  if (Pmode != SImode)
    pat = gen_rtx_SUBREG (SImode, pat, 0);
  emit_insn (gen_rtx_SET (VOIDmode, operands[0], pat));
  DONE;
}
  emit_insn (const0_rtx);
  _val = get_insns ();
  end_sequence ();
  return _val;
}

/* ../../gcc/gcc/config/i386/i386.md:4977 */
extern rtx gen_split_1160 PARAMS ((rtx *));
rtx
gen_split_1160 (operands)
      rtx *operands;
{
  rtx operand0;
  rtx operand1;
  rtx operand2;
  rtx operand3;
  rtx operand4;
  rtx _val = 0;
  start_sequence ();
{
  operands[1] = gen_lowpart (Pmode, operands[1]);
  operands[3] = gen_lowpart (Pmode, operands[3]);
  operands[4] = gen_lowpart (Pmode, operands[4]);
}
  operand0 = operands[0];
  operand1 = operands[1];
  operand2 = operands[2];
  operand3 = operands[3];
  operand4 = operands[4];
  emit_insn (gen_rtx_SET (VOIDmode,
	operand0,
	gen_rtx_ZERO_EXTEND (DImode,
	gen_rtx_SUBREG (SImode,
	gen_rtx_PLUS (DImode,
	gen_rtx_PLUS (DImode,
	gen_rtx_MULT (DImode,
	operand1,
	operand2),
	operand3),
	operand4),
	0))));
  _val = get_insns ();
  end_sequence ();
  return _val;
}

/* ../../gcc/gcc/config/i386/i386.md:5056 */
extern rtx gen_split_1161 PARAMS ((rtx *));
rtx
gen_split_1161 (operands)
      rtx *operands;
{
  rtx operand0;
  rtx operand1;
  rtx operand2;
  rtx _val = 0;
  start_sequence ();

  operand0 = operands[0];
  operand1 = operands[1];
  operand2 = operands[2];
  emit_insn (gen_rtx_SET (VOIDmode,
	operand0,
	gen_rtx_PLUS (DImode,
	operand1,
	operand2)));
  _val = get_insns ();
  end_sequence ();
  return _val;
}

/* ../../gcc/gcc/config/i386/i386.md:5322 */
extern rtx gen_split_1162 PARAMS ((rtx *));
rtx
gen_split_1162 (operands)
      rtx *operands ATTRIBUTE_UNUSED;
{
  rtx _val = 0;
  start_sequence ();
{
  rtx pat;
  /* In -fPIC mode the constructs like (const (unspec [symbol_ref]))
     may confuse gen_lowpart.  */
  if (GET_MODE (operands[0]) != Pmode)
    {
      operands[1] = gen_lowpart (Pmode, operands[1]);
      operands[2] = gen_lowpart (Pmode, operands[2]);
    }
  operands[0] = gen_lowpart (SImode, operands[0]);
  pat = gen_rtx_PLUS (Pmode, operands[1], operands[2]);
  if (Pmode != SImode)
    pat = gen_rtx_SUBREG (SImode, pat, 0);
  emit_insn (gen_rtx_SET (VOIDmode, operands[0], pat));
  DONE;
}
  emit_insn (const0_rtx);
  _val = get_insns ();
  end_sequence ();
  return _val;
}

/* ../../gcc/gcc/config/i386/i386.md:5402 */
extern rtx gen_split_1163 PARAMS ((rtx *));
rtx
gen_split_1163 (operands)
      rtx *operands;
{
  rtx operand0;
  rtx operand1;
  rtx operand2;
  rtx _val = 0;
  start_sequence ();
{
  operands[1] = gen_lowpart (Pmode, operands[1]);
  operands[2] = gen_lowpart (Pmode, operands[2]);
}
  operand0 = operands[0];
  operand1 = operands[1];
  operand2 = operands[2];
  emit_insn (gen_rtx_SET (VOIDmode,
	operand0,
	gen_rtx_ZERO_EXTEND (DImode,
	gen_rtx_SUBREG (SImode,
	gen_rtx_PLUS (DImode,
	operand1,
	operand2),
	0))));
  _val = get_insns ();
  end_sequence ();
  return _val;
}

/* ../../gcc/gcc/config/i386/i386.md:5690 */
rtx
gen_addhi3 (operand0, operand1, operand2)
     rtx operand0;
     rtx operand1;
     rtx operand2;
{
  rtx _val = 0;
  start_sequence ();
  {
    rtx operands[3];
    operands[0] = operand0;
    operands[1] = operand1;
    operands[2] = operand2;
ix86_expand_binary_operator (PLUS, HImode, operands); DONE;
    operand0 = operands[0];
    operand1 = operands[1];
    operand2 = operands[2];
  }
  emit (gen_rtx_PARALLEL (VOIDmode,
	gen_rtvec (2,
		gen_rtx_SET (VOIDmode,
	operand0,
	gen_rtx_PLUS (HImode,
	operand1,
	operand2)),
		gen_rtx_CLOBBER (VOIDmode,
	gen_rtx_REG (CCmode,
	17)))));
  _val = get_insns ();
  end_sequence ();
  return _val;
}

/* ../../gcc/gcc/config/i386/i386.md:5935 */
rtx
gen_addqi3 (operand0, operand1, operand2)
     rtx operand0;
     rtx operand1;
     rtx operand2;
{
  rtx _val = 0;
  start_sequence ();
  {
    rtx operands[3];
    operands[0] = operand0;
    operands[1] = operand1;
    operands[2] = operand2;
ix86_expand_binary_operator (PLUS, QImode, operands); DONE;
    operand0 = operands[0];
    operand1 = operands[1];
    operand2 = operands[2];
  }
  emit (gen_rtx_PARALLEL (VOIDmode,
	gen_rtvec (2,
		gen_rtx_SET (VOIDmode,
	operand0,
	gen_rtx_PLUS (QImode,
	operand1,
	operand2)),
		gen_rtx_CLOBBER (VOIDmode,
	gen_rtx_REG (CCmode,
	17)))));
  _val = get_insns ();
  end_sequence ();
  return _val;
}

/* ../../gcc/gcc/config/i386/i386.md:6312 */
rtx
gen_addxf3 (operand0, operand1, operand2)
     rtx operand0;
     rtx operand1;
     rtx operand2;
{
  return gen_rtx_SET (VOIDmode,
	operand0,
	gen_rtx_PLUS (XFmode,
	operand1,
	operand2));
}

/* ../../gcc/gcc/config/i386/i386.md:6319 */
rtx
gen_addtf3 (operand0, operand1, operand2)
     rtx operand0;
     rtx operand1;
     rtx operand2;
{
  return gen_rtx_SET (VOIDmode,
	operand0,
	gen_rtx_PLUS (TFmode,
	operand1,
	operand2));
}

/* ../../gcc/gcc/config/i386/i386.md:6326 */
rtx
gen_adddf3 (operand0, operand1, operand2)
     rtx operand0;
     rtx operand1;
     rtx operand2;
{
  return gen_rtx_SET (VOIDmode,
	operand0,
	gen_rtx_PLUS (DFmode,
	operand1,
	operand2));
}

/* ../../gcc/gcc/config/i386/i386.md:6333 */
rtx
gen_addsf3 (operand0, operand1, operand2)
     rtx operand0;
     rtx operand1;
     rtx operand2;
{
  return gen_rtx_SET (VOIDmode,
	operand0,
	gen_rtx_PLUS (SFmode,
	operand1,
	operand2));
}

/* ../../gcc/gcc/config/i386/i386.md:6344 */
rtx
gen_subdi3 (operand0, operand1, operand2)
     rtx operand0;
     rtx operand1;
     rtx operand2;
{
  rtx _val = 0;
  start_sequence ();
  {
    rtx operands[3];
    operands[0] = operand0;
    operands[1] = operand1;
    operands[2] = operand2;
ix86_expand_binary_operator (MINUS, DImode, operands); DONE;
    operand0 = operands[0];
    operand1 = operands[1];
    operand2 = operands[2];
  }
  emit (gen_rtx_PARALLEL (VOIDmode,
	gen_rtvec (2,
		gen_rtx_SET (VOIDmode,
	operand0,
	gen_rtx_MINUS (DImode,
	operand1,
	operand2)),
		gen_rtx_CLOBBER (VOIDmode,
	gen_rtx_REG (CCmode,
	17)))));
  _val = get_insns ();
  end_sequence ();
  return _val;
}

/* ../../gcc/gcc/config/i386/i386.md:6360 */
extern rtx gen_split_1171 PARAMS ((rtx *));
rtx
gen_split_1171 (operands)
      rtx *operands;
{
  rtx operand0;
  rtx operand1;
  rtx operand2;
  rtx operand3;
  rtx operand4;
  rtx operand5;
  rtx _val = 0;
  start_sequence ();
split_di (operands+0, 1, operands+0, operands+3);
   split_di (operands+1, 1, operands+1, operands+4);
   split_di (operands+2, 1, operands+2, operands+5);
  operand0 = operands[0];
  operand1 = operands[1];
  operand2 = operands[2];
  operand3 = operands[3];
  operand4 = operands[4];
  operand5 = operands[5];
  emit (gen_rtx_PARALLEL (VOIDmode,
	gen_rtvec (2,
		gen_rtx_SET (VOIDmode,
	gen_rtx_REG (CCmode,
	17),
	gen_rtx_COMPARE (CCmode,
	operand1,
	operand2)),
		gen_rtx_SET (VOIDmode,
	operand0,
	gen_rtx_MINUS (SImode,
	copy_rtx (operand1),
	copy_rtx (operand2))))));
  emit (gen_rtx_PARALLEL (VOIDmode,
	gen_rtvec (2,
		gen_rtx_SET (VOIDmode,
	operand3,
	gen_rtx_MINUS (SImode,
	operand4,
	gen_rtx_PLUS (SImode,
	gen_rtx_LTU (SImode,
	gen_rtx_REG (CCmode,
	17),
	const0_rtx),
	operand5))),
		gen_rtx_CLOBBER (VOIDmode,
	gen_rtx_REG (CCmode,
	17)))));
  _val = get_insns ();
  end_sequence ();
  return _val;
}

/* ../../gcc/gcc/config/i386/i386.md:6454 */
rtx
gen_subsi3 (operand0, operand1, operand2)
     rtx operand0;
     rtx operand1;
     rtx operand2;
{
  rtx _val = 0;
  start_sequence ();
  {
    rtx operands[3];
    operands[0] = operand0;
    operands[1] = operand1;
    operands[2] = operand2;
ix86_expand_binary_operator (MINUS, SImode, operands); DONE;
    operand0 = operands[0];
    operand1 = operands[1];
    operand2 = operands[2];
  }
  emit (gen_rtx_PARALLEL (VOIDmode,
	gen_rtvec (2,
		gen_rtx_SET (VOIDmode,
	operand0,
	gen_rtx_MINUS (SImode,
	operand1,
	operand2)),
		gen_rtx_CLOBBER (VOIDmode,
	gen_rtx_REG (CCmode,
	17)))));
  _val = get_insns ();
  end_sequence ();
  return _val;
}

/* ../../gcc/gcc/config/i386/i386.md:6539 */
rtx
gen_subhi3 (operand0, operand1, operand2)
     rtx operand0;
     rtx operand1;
     rtx operand2;
{
  rtx _val = 0;
  start_sequence ();
  {
    rtx operands[3];
    operands[0] = operand0;
    operands[1] = operand1;
    operands[2] = operand2;
ix86_expand_binary_operator (MINUS, HImode, operands); DONE;
    operand0 = operands[0];
    operand1 = operands[1];
    operand2 = operands[2];
  }
  emit (gen_rtx_PARALLEL (VOIDmode,
	gen_rtvec (2,
		gen_rtx_SET (VOIDmode,
	operand0,
	gen_rtx_MINUS (HImode,
	operand1,
	operand2)),
		gen_rtx_CLOBBER (VOIDmode,
	gen_rtx_REG (CCmode,
	17)))));
  _val = get_insns ();
  end_sequence ();
  return _val;
}

/* ../../gcc/gcc/config/i386/i386.md:6583 */
rtx
gen_subqi3 (operand0, operand1, operand2)
     rtx operand0;
     rtx operand1;
     rtx operand2;
{
  rtx _val = 0;
  start_sequence ();
  {
    rtx operands[3];
    operands[0] = operand0;
    operands[1] = operand1;
    operands[2] = operand2;
ix86_expand_binary_operator (MINUS, QImode, operands); DONE;
    operand0 = operands[0];
    operand1 = operands[1];
    operand2 = operands[2];
  }
  emit (gen_rtx_PARALLEL (VOIDmode,
	gen_rtvec (2,
		gen_rtx_SET (VOIDmode,
	operand0,
	gen_rtx_MINUS (QImode,
	operand1,
	operand2)),
		gen_rtx_CLOBBER (VOIDmode,
	gen_rtx_REG (CCmode,
	17)))));
  _val = get_insns ();
  end_sequence ();
  return _val;
}

/* ../../gcc/gcc/config/i386/i386.md:6640 */
rtx
gen_subxf3 (operand0, operand1, operand2)
     rtx operand0;
     rtx operand1;
     rtx operand2;
{
  return gen_rtx_SET (VOIDmode,
	operand0,
	gen_rtx_MINUS (XFmode,
	operand1,
	operand2));
}

/* ../../gcc/gcc/config/i386/i386.md:6647 */
rtx
gen_subtf3 (operand0, operand1, operand2)
     rtx operand0;
     rtx operand1;
     rtx operand2;
{
  return gen_rtx_SET (VOIDmode,
	operand0,
	gen_rtx_MINUS (TFmode,
	operand1,
	operand2));
}

/* ../../gcc/gcc/config/i386/i386.md:6654 */
rtx
gen_subdf3 (operand0, operand1, operand2)
     rtx operand0;
     rtx operand1;
     rtx operand2;
{
  return gen_rtx_SET (VOIDmode,
	operand0,
	gen_rtx_MINUS (DFmode,
	operand1,
	operand2));
}

/* ../../gcc/gcc/config/i386/i386.md:6661 */
rtx
gen_subsf3 (operand0, operand1, operand2)
     rtx operand0;
     rtx operand1;
     rtx operand2;
{
  return gen_rtx_SET (VOIDmode,
	operand0,
	gen_rtx_MINUS (SFmode,
	operand1,
	operand2));
}

/* ../../gcc/gcc/config/i386/i386.md:6670 */
rtx
gen_muldi3 (operand0, operand1, operand2)
     rtx operand0;
     rtx operand1;
     rtx operand2;
{
  return gen_rtx_PARALLEL (VOIDmode,
	gen_rtvec (2,
		gen_rtx_SET (VOIDmode,
	operand0,
	gen_rtx_MULT (DImode,
	operand1,
	operand2)),
		gen_rtx_CLOBBER (VOIDmode,
	gen_rtx_REG (CCmode,
	17))));
}

/* ../../gcc/gcc/config/i386/i386.md:6693 */
rtx
gen_mulsi3 (operand0, operand1, operand2)
     rtx operand0;
     rtx operand1;
     rtx operand2;
{
  return gen_rtx_PARALLEL (VOIDmode,
	gen_rtvec (2,
		gen_rtx_SET (VOIDmode,
	operand0,
	gen_rtx_MULT (SImode,
	operand1,
	operand2)),
		gen_rtx_CLOBBER (VOIDmode,
	gen_rtx_REG (CCmode,
	17))));
}

/* ../../gcc/gcc/config/i386/i386.md:6755 */
rtx
gen_mulhi3 (operand0, operand1, operand2)
     rtx operand0;
     rtx operand1;
     rtx operand2;
{
  return gen_rtx_PARALLEL (VOIDmode,
	gen_rtvec (2,
		gen_rtx_SET (VOIDmode,
	operand0,
	gen_rtx_MULT (HImode,
	operand1,
	operand2)),
		gen_rtx_CLOBBER (VOIDmode,
	gen_rtx_REG (CCmode,
	17))));
}

/* ../../gcc/gcc/config/i386/i386.md:6779 */
rtx
gen_mulqi3 (operand0, operand1, operand2)
     rtx operand0;
     rtx operand1;
     rtx operand2;
{
  return gen_rtx_PARALLEL (VOIDmode,
	gen_rtvec (2,
		gen_rtx_SET (VOIDmode,
	operand0,
	gen_rtx_MULT (QImode,
	operand1,
	operand2)),
		gen_rtx_CLOBBER (VOIDmode,
	gen_rtx_REG (CCmode,
	17))));
}

/* ../../gcc/gcc/config/i386/i386.md:6799 */
rtx
gen_umulqihi3 (operand0, operand1, operand2)
     rtx operand0;
     rtx operand1;
     rtx operand2;
{
  return gen_rtx_PARALLEL (VOIDmode,
	gen_rtvec (2,
		gen_rtx_SET (VOIDmode,
	operand0,
	gen_rtx_MULT (HImode,
	gen_rtx_ZERO_EXTEND (HImode,
	operand1),
	gen_rtx_ZERO_EXTEND (HImode,
	operand2))),
		gen_rtx_CLOBBER (VOIDmode,
	gen_rtx_REG (CCmode,
	17))));
}

/* ../../gcc/gcc/config/i386/i386.md:6821 */
rtx
gen_mulqihi3 (operand0, operand1, operand2)
     rtx operand0;
     rtx operand1;
     rtx operand2;
{
  return gen_rtx_PARALLEL (VOIDmode,
	gen_rtvec (2,
		gen_rtx_SET (VOIDmode,
	operand0,
	gen_rtx_MULT (HImode,
	gen_rtx_SIGN_EXTEND (HImode,
	operand1),
	gen_rtx_SIGN_EXTEND (HImode,
	operand2))),
		gen_rtx_CLOBBER (VOIDmode,
	gen_rtx_REG (CCmode,
	17))));
}

/* ../../gcc/gcc/config/i386/i386.md:6841 */
rtx
gen_umulditi3 (operand0, operand1, operand2)
     rtx operand0;
     rtx operand1;
     rtx operand2;
{
  return gen_rtx_PARALLEL (VOIDmode,
	gen_rtvec (2,
		gen_rtx_SET (VOIDmode,
	operand0,
	gen_rtx_MULT (TImode,
	gen_rtx_ZERO_EXTEND (TImode,
	operand1),
	gen_rtx_ZERO_EXTEND (TImode,
	operand2))),
		gen_rtx_CLOBBER (VOIDmode,
	gen_rtx_REG (CCmode,
	17))));
}

/* ../../gcc/gcc/config/i386/i386.md:6865 */
rtx
gen_umulsidi3 (operand0, operand1, operand2)
     rtx operand0;
     rtx operand1;
     rtx operand2;
{
  return gen_rtx_PARALLEL (VOIDmode,
	gen_rtvec (2,
		gen_rtx_SET (VOIDmode,
	operand0,
	gen_rtx_MULT (DImode,
	gen_rtx_ZERO_EXTEND (DImode,
	operand1),
	gen_rtx_ZERO_EXTEND (DImode,
	operand2))),
		gen_rtx_CLOBBER (VOIDmode,
	gen_rtx_REG (CCmode,
	17))));
}

/* ../../gcc/gcc/config/i386/i386.md:6888 */
rtx
gen_mulditi3 (operand0, operand1, operand2)
     rtx operand0;
     rtx operand1;
     rtx operand2;
{
  return gen_rtx_PARALLEL (VOIDmode,
	gen_rtvec (2,
		gen_rtx_SET (VOIDmode,
	operand0,
	gen_rtx_MULT (TImode,
	gen_rtx_SIGN_EXTEND (TImode,
	operand1),
	gen_rtx_SIGN_EXTEND (TImode,
	operand2))),
		gen_rtx_CLOBBER (VOIDmode,
	gen_rtx_REG (CCmode,
	17))));
}

/* ../../gcc/gcc/config/i386/i386.md:6910 */
rtx
gen_mulsidi3 (operand0, operand1, operand2)
     rtx operand0;
     rtx operand1;
     rtx operand2;
{
  return gen_rtx_PARALLEL (VOIDmode,
	gen_rtvec (2,
		gen_rtx_SET (VOIDmode,
	operand0,
	gen_rtx_MULT (DImode,
	gen_rtx_SIGN_EXTEND (DImode,
	operand1),
	gen_rtx_SIGN_EXTEND (DImode,
	operand2))),
		gen_rtx_CLOBBER (VOIDmode,
	gen_rtx_REG (CCmode,
	17))));
}

/* ../../gcc/gcc/config/i386/i386.md:6932 */
rtx
gen_umuldi3_highpart (operand0, operand1, operand2)
     rtx operand0;
     rtx operand1;
     rtx operand2;
{
  return gen_rtx_PARALLEL (VOIDmode,
	gen_rtvec (3,
		gen_rtx_SET (VOIDmode,
	operand0,
	gen_rtx_TRUNCATE (DImode,
	gen_rtx_LSHIFTRT (TImode,
	gen_rtx_MULT (TImode,
	gen_rtx_ZERO_EXTEND (TImode,
	operand1),
	gen_rtx_ZERO_EXTEND (TImode,
	operand2)),
	GEN_INT (64LL)))),
		gen_rtx_CLOBBER (VOIDmode,
	gen_rtx_SCRATCH (DImode)),
		gen_rtx_CLOBBER (VOIDmode,
	gen_rtx_REG (CCmode,
	17))));
}

/* ../../gcc/gcc/config/i386/i386.md:6965 */
rtx
gen_umulsi3_highpart (operand0, operand1, operand2)
     rtx operand0;
     rtx operand1;
     rtx operand2;
{
  return gen_rtx_PARALLEL (VOIDmode,
	gen_rtvec (3,
		gen_rtx_SET (VOIDmode,
	operand0,
	gen_rtx_TRUNCATE (SImode,
	gen_rtx_LSHIFTRT (DImode,
	gen_rtx_MULT (DImode,
	gen_rtx_ZERO_EXTEND (DImode,
	operand1),
	gen_rtx_ZERO_EXTEND (DImode,
	operand2)),
	GEN_INT (32LL)))),
		gen_rtx_CLOBBER (VOIDmode,
	gen_rtx_SCRATCH (SImode)),
		gen_rtx_CLOBBER (VOIDmode,
	gen_rtx_REG (CCmode,
	17))));
}

/* ../../gcc/gcc/config/i386/i386.md:7016 */
rtx
gen_smuldi3_highpart (operand0, operand1, operand2)
     rtx operand0;
     rtx operand1;
     rtx operand2;
{
  return gen_rtx_PARALLEL (VOIDmode,
	gen_rtvec (3,
		gen_rtx_SET (VOIDmode,
	operand0,
	gen_rtx_TRUNCATE (DImode,
	gen_rtx_LSHIFTRT (TImode,
	gen_rtx_MULT (TImode,
	gen_rtx_SIGN_EXTEND (TImode,
	operand1),
	gen_rtx_SIGN_EXTEND (TImode,
	operand2)),
	GEN_INT (64LL)))),
		gen_rtx_CLOBBER (VOIDmode,
	gen_rtx_SCRATCH (DImode)),
		gen_rtx_CLOBBER (VOIDmode,
	gen_rtx_REG (CCmode,
	17))));
}

/* ../../gcc/gcc/config/i386/i386.md:7048 */
rtx
gen_smulsi3_highpart (operand0, operand1, operand2)
     rtx operand0;
     rtx operand1;
     rtx operand2;
{
  return gen_rtx_PARALLEL (VOIDmode,
	gen_rtvec (3,
		gen_rtx_SET (VOIDmode,
	operand0,
	gen_rtx_TRUNCATE (SImode,
	gen_rtx_LSHIFTRT (DImode,
	gen_rtx_MULT (DImode,
	gen_rtx_SIGN_EXTEND (DImode,
	operand1),
	gen_rtx_SIGN_EXTEND (DImode,
	operand2)),
	GEN_INT (32LL)))),
		gen_rtx_CLOBBER (VOIDmode,
	gen_rtx_SCRATCH (SImode)),
		gen_rtx_CLOBBER (VOIDmode,
	gen_rtx_REG (CCmode,
	17))));
}

/* ../../gcc/gcc/config/i386/i386.md:7099 */
rtx
gen_mulxf3 (operand0, operand1, operand2)
     rtx operand0;
     rtx operand1;
     rtx operand2;
{
  return gen_rtx_SET (VOIDmode,
	operand0,
	gen_rtx_MULT (XFmode,
	operand1,
	operand2));
}

/* ../../gcc/gcc/config/i386/i386.md:7106 */
rtx
gen_multf3 (operand0, operand1, operand2)
     rtx operand0;
     rtx operand1;
     rtx operand2;
{
  return gen_rtx_SET (VOIDmode,
	operand0,
	gen_rtx_MULT (TFmode,
	operand1,
	operand2));
}

/* ../../gcc/gcc/config/i386/i386.md:7113 */
rtx
gen_muldf3 (operand0, operand1, operand2)
     rtx operand0;
     rtx operand1;
     rtx operand2;
{
  return gen_rtx_SET (VOIDmode,
	operand0,
	gen_rtx_MULT (DFmode,
	operand1,
	operand2));
}

/* ../../gcc/gcc/config/i386/i386.md:7120 */
rtx
gen_mulsf3 (operand0, operand1, operand2)
     rtx operand0;
     rtx operand1;
     rtx operand2;
{
  return gen_rtx_SET (VOIDmode,
	operand0,
	gen_rtx_MULT (SFmode,
	operand1,
	operand2));
}

/* ../../gcc/gcc/config/i386/i386.md:7153 */
rtx
gen_divxf3 (operand0, operand1, operand2)
     rtx operand0;
     rtx operand1;
     rtx operand2;
{
  return gen_rtx_SET (VOIDmode,
	operand0,
	gen_rtx_DIV (XFmode,
	operand1,
	operand2));
}

/* ../../gcc/gcc/config/i386/i386.md:7160 */
rtx
gen_divtf3 (operand0, operand1, operand2)
     rtx operand0;
     rtx operand1;
     rtx operand2;
{
  return gen_rtx_SET (VOIDmode,
	operand0,
	gen_rtx_DIV (TFmode,
	operand1,
	operand2));
}

/* ../../gcc/gcc/config/i386/i386.md:7167 */
rtx
gen_divdf3 (operand0, operand1, operand2)
     rtx operand0;
     rtx operand1;
     rtx operand2;
{
  return gen_rtx_SET (VOIDmode,
	operand0,
	gen_rtx_DIV (DFmode,
	operand1,
	operand2));
}

/* ../../gcc/gcc/config/i386/i386.md:7174 */
rtx
gen_divsf3 (operand0, operand1, operand2)
     rtx operand0;
     rtx operand1;
     rtx operand2;
{
  return gen_rtx_SET (VOIDmode,
	operand0,
	gen_rtx_DIV (SFmode,
	operand1,
	operand2));
}

/* ../../gcc/gcc/config/i386/i386.md:7183 */
rtx
gen_divmoddi4 (operand0, operand1, operand2, operand3)
     rtx operand0;
     rtx operand1;
     rtx operand2;
     rtx operand3;
{
  return gen_rtx_PARALLEL (VOIDmode,
	gen_rtvec (3,
		gen_rtx_SET (VOIDmode,
	operand0,
	gen_rtx_DIV (DImode,
	operand1,
	operand2)),
		gen_rtx_SET (VOIDmode,
	operand3,
	gen_rtx_MOD (DImode,
	operand1,
	operand2)),
		gen_rtx_CLOBBER (VOIDmode,
	gen_rtx_REG (CCmode,
	17))));
}

/* ../../gcc/gcc/config/i386/i386.md:7232 */
extern rtx gen_split_1202 PARAMS ((rtx *));
rtx
gen_split_1202 (operands)
      rtx *operands;
{
  rtx operand0;
  rtx operand1;
  rtx operand2;
  rtx operand3;
  rtx operand4;
  rtx _val = 0;
  start_sequence ();
{
  /* Avoid use of cltd in favor of a mov+shift.  */
  if (!TARGET_USE_CLTD && !optimize_size)
    {
      if (true_regnum (operands[1]))
        emit_move_insn (operands[0], operands[1]);
      else
	emit_move_insn (operands[3], operands[1]);
      operands[4] = operands[3];
    }
  else
    {
      if (true_regnum (operands[1]))
	abort();
      operands[4] = operands[1];
    }
}
  operand0 = operands[0];
  operand1 = operands[1];
  operand2 = operands[2];
  operand3 = operands[3];
  operand4 = operands[4];
  emit (gen_rtx_PARALLEL (VOIDmode,
	gen_rtvec (2,
		gen_rtx_SET (VOIDmode,
	operand3,
	gen_rtx_ASHIFTRT (DImode,
	operand4,
	GEN_INT (63LL))),
		gen_rtx_CLOBBER (VOIDmode,
	gen_rtx_REG (CCmode,
	17)))));
  emit (gen_rtx_PARALLEL (VOIDmode,
	gen_rtvec (4,
		gen_rtx_SET (VOIDmode,
	operand0,
	gen_rtx_DIV (DImode,
	gen_rtx_REG (DImode,
	0),
	operand2)),
		gen_rtx_SET (VOIDmode,
	copy_rtx (operand3),
	gen_rtx_MOD (DImode,
	gen_rtx_REG (DImode,
	0),
	copy_rtx (operand2))),
		gen_rtx_USE (VOIDmode,
	copy_rtx (operand3)),
		gen_rtx_CLOBBER (VOIDmode,
	gen_rtx_REG (CCmode,
	17)))));
  _val = get_insns ();
  end_sequence ();
  return _val;
}

/* ../../gcc/gcc/config/i386/i386.md:7268 */
rtx
gen_divmodsi4 (operand0, operand1, operand2, operand3)
     rtx operand0;
     rtx operand1;
     rtx operand2;
     rtx operand3;
{
  return gen_rtx_PARALLEL (VOIDmode,
	gen_rtvec (3,
		gen_rtx_SET (VOIDmode,
	operand0,
	gen_rtx_DIV (SImode,
	operand1,
	operand2)),
		gen_rtx_SET (VOIDmode,
	operand3,
	gen_rtx_MOD (SImode,
	operand1,
	operand2)),
		gen_rtx_CLOBBER (VOIDmode,
	gen_rtx_REG (CCmode,
	17))));
}

/* ../../gcc/gcc/config/i386/i386.md:7317 */
extern rtx gen_split_1204 PARAMS ((rtx *));
rtx
gen_split_1204 (operands)
      rtx *operands;
{
  rtx operand0;
  rtx operand1;
  rtx operand2;
  rtx operand3;
  rtx operand4;
  rtx _val = 0;
  start_sequence ();
{
  /* Avoid use of cltd in favor of a mov+shift.  */
  if (!TARGET_USE_CLTD && !optimize_size)
    {
      if (true_regnum (operands[1]))
        emit_move_insn (operands[0], operands[1]);
      else
	emit_move_insn (operands[3], operands[1]);
      operands[4] = operands[3];
    }
  else
    {
      if (true_regnum (operands[1]))
	abort();
      operands[4] = operands[1];
    }
}
  operand0 = operands[0];
  operand1 = operands[1];
  operand2 = operands[2];
  operand3 = operands[3];
  operand4 = operands[4];
  emit (gen_rtx_PARALLEL (VOIDmode,
	gen_rtvec (2,
		gen_rtx_SET (VOIDmode,
	operand3,
	gen_rtx_ASHIFTRT (SImode,
	operand4,
	GEN_INT (31LL))),
		gen_rtx_CLOBBER (VOIDmode,
	gen_rtx_REG (CCmode,
	17)))));
  emit (gen_rtx_PARALLEL (VOIDmode,
	gen_rtvec (4,
		gen_rtx_SET (VOIDmode,
	operand0,
	gen_rtx_DIV (SImode,
	gen_rtx_REG (SImode,
	0),
	operand2)),
		gen_rtx_SET (VOIDmode,
	copy_rtx (operand3),
	gen_rtx_MOD (SImode,
	gen_rtx_REG (SImode,
	0),
	copy_rtx (operand2))),
		gen_rtx_USE (VOIDmode,
	copy_rtx (operand3)),
		gen_rtx_CLOBBER (VOIDmode,
	gen_rtx_REG (CCmode,
	17)))));
  _val = get_insns ();
  end_sequence ();
  return _val;
}

/* ../../gcc/gcc/config/i386/i386.md:7392 */
extern rtx gen_split_1205 PARAMS ((rtx *));
rtx
gen_split_1205 (operands)
      rtx *operands;
{
  rtx operand0;
  rtx operand1;
  rtx operand2;
  rtx operand3;
  rtx _val = 0;
  start_sequence ();

  operand0 = operands[0];
  operand1 = operands[1];
  operand2 = operands[2];
  operand3 = operands[3];
  emit_insn (gen_rtx_SET (VOIDmode,
	operand3,
	const0_rtx));
  emit (gen_rtx_PARALLEL (VOIDmode,
	gen_rtvec (4,
		gen_rtx_SET (VOIDmode,
	operand0,
	gen_rtx_UDIV (DImode,
	operand1,
	operand2)),
		gen_rtx_SET (VOIDmode,
	copy_rtx (operand3),
	gen_rtx_UMOD (DImode,
	copy_rtx (operand1),
	copy_rtx (operand2))),
		gen_rtx_USE (VOIDmode,
	copy_rtx (operand3)),
		gen_rtx_CLOBBER (VOIDmode,
	gen_rtx_REG (CCmode,
	17)))));
  _val = get_insns ();
  end_sequence ();
  return _val;
}

/* ../../gcc/gcc/config/i386/i386.md:7436 */
extern rtx gen_split_1206 PARAMS ((rtx *));
rtx
gen_split_1206 (operands)
      rtx *operands;
{
  rtx operand0;
  rtx operand1;
  rtx operand2;
  rtx operand3;
  rtx _val = 0;
  start_sequence ();

  operand0 = operands[0];
  operand1 = operands[1];
  operand2 = operands[2];
  operand3 = operands[3];
  emit_insn (gen_rtx_SET (VOIDmode,
	operand3,
	const0_rtx));
  emit (gen_rtx_PARALLEL (VOIDmode,
	gen_rtvec (4,
		gen_rtx_SET (VOIDmode,
	operand0,
	gen_rtx_UDIV (SImode,
	operand1,
	operand2)),
		gen_rtx_SET (VOIDmode,
	copy_rtx (operand3),
	gen_rtx_UMOD (SImode,
	copy_rtx (operand1),
	copy_rtx (operand2))),
		gen_rtx_USE (VOIDmode,
	copy_rtx (operand3)),
		gen_rtx_CLOBBER (VOIDmode,
	gen_rtx_REG (CCmode,
	17)))));
  _val = get_insns ();
  end_sequence ();
  return _val;
}

/* ../../gcc/gcc/config/i386/i386.md:7453 */
rtx
gen_udivmodhi4 (operand0, operand1, operand2, operand3)
     rtx operand0;
     rtx operand1;
     rtx operand2;
     rtx operand3;
{
  rtx operand4;
  rtx _val = 0;
  start_sequence ();
  {
    rtx operands[5];
    operands[0] = operand0;
    operands[1] = operand1;
    operands[2] = operand2;
    operands[3] = operand3;
operands[4] = gen_reg_rtx (HImode);
    operand0 = operands[0];
    operand1 = operands[1];
    operand2 = operands[2];
    operand3 = operands[3];
    operand4 = operands[4];
  }
  emit_insn (gen_rtx_SET (VOIDmode,
	operand4,
	const0_rtx));
  emit (gen_rtx_PARALLEL (VOIDmode,
	gen_rtvec (4,
		gen_rtx_SET (VOIDmode,
	operand0,
	gen_rtx_UDIV (HImode,
	operand1,
	operand2)),
		gen_rtx_SET (VOIDmode,
	operand3,
	gen_rtx_UMOD (HImode,
	operand1,
	operand2)),
		gen_rtx_USE (VOIDmode,
	operand4),
		gen_rtx_CLOBBER (VOIDmode,
	gen_rtx_REG (CCmode,
	17)))));
  _val = get_insns ();
  end_sequence ();
  return _val;
}

/* ../../gcc/gcc/config/i386/i386.md:7535 */
rtx
gen_testsi_ccno_1 (operand0, operand1)
     rtx operand0;
     rtx operand1;
{
  return gen_rtx_SET (VOIDmode,
	gen_rtx_REG (CCNOmode,
	17),
	gen_rtx_COMPARE (CCNOmode,
	gen_rtx_AND (SImode,
	operand0,
	operand1),
	const0_rtx));
}

/* ../../gcc/gcc/config/i386/i386.md:7556 */
rtx
gen_testqi_ccz_1 (operand0, operand1)
     rtx operand0;
     rtx operand1;
{
  return gen_rtx_SET (VOIDmode,
	gen_rtx_REG (CCZmode,
	17),
	gen_rtx_COMPARE (CCZmode,
	gen_rtx_AND (QImode,
	operand0,
	operand1),
	const0_rtx));
}

/* ../../gcc/gcc/config/i386/i386.md:7585 */
rtx
gen_testqi_ext_ccno_0 (operand0, operand1)
     rtx operand0;
     rtx operand1;
{
  return gen_rtx_SET (VOIDmode,
	gen_rtx_REG (CCNOmode,
	17),
	gen_rtx_COMPARE (CCNOmode,
	gen_rtx_AND (SImode,
	gen_rtx_ZERO_EXTRACT (SImode,
	operand0,
	GEN_INT (8LL),
	GEN_INT (8LL)),
	operand1),
	const0_rtx));
}

/* ../../gcc/gcc/config/i386/i386.md:7701 */
extern rtx gen_split_1211 PARAMS ((rtx *));
rtx
gen_split_1211 (operands)
      rtx *operands;
{
  rtx operand0;
  rtx operand1;
  rtx operand2;
  rtx operand3;
  rtx _val = 0;
  start_sequence ();
{
  HOST_WIDE_INT len = INTVAL (operands[1]);
  HOST_WIDE_INT pos = INTVAL (operands[2]);
  HOST_WIDE_INT mask;
  enum machine_mode mode, submode;

  mode = GET_MODE (operands[0]);
  if (GET_CODE (operands[0]) == MEM)
    {
      /* ??? Combine likes to put non-volatile mem extractions in QImode
	 no matter the size of the test.  So find a mode that works.  */
      if (! MEM_VOLATILE_P (operands[0]))
	{
	  mode = smallest_mode_for_size (pos + len, MODE_INT);
	  operands[0] = adjust_address (operands[0], mode, 0);
	}
    }
  else if (GET_CODE (operands[0]) == SUBREG
	   && (submode = GET_MODE (SUBREG_REG (operands[0])),
	       GET_MODE_BITSIZE (mode) > GET_MODE_BITSIZE (submode))
	   && pos + len <= GET_MODE_BITSIZE (submode))
    {
      /* Narrow a paradoxical subreg to prevent partial register stalls.  */
      mode = submode;
      operands[0] = SUBREG_REG (operands[0]);
    }
  else if (mode == HImode && pos + len <= 8)
    {
      /* Small HImode tests can be converted to QImode.  */
      mode = QImode;
      operands[0] = gen_lowpart (QImode, operands[0]);
    }

  mask  = ((HOST_WIDE_INT)1 << (pos + len)) - 1;
  mask &= ~(((HOST_WIDE_INT)1 << pos) - 1);

  operands[3] = gen_rtx_AND (mode, operands[0], gen_int_mode (mask, mode));
}
  operand0 = operands[0];
  operand1 = operands[1];
  operand2 = operands[2];
  operand3 = operands[3];
  emit_insn (gen_rtx_SET (VOIDmode,
	gen_rtx_REG (CCNOmode,
	17),
	gen_rtx_COMPARE (CCNOmode,
	operand3,
	const0_rtx)));
  _val = get_insns ();
  end_sequence ();
  return _val;
}

/* ../../gcc/gcc/config/i386/i386.md:7754 */
extern rtx gen_split_1212 PARAMS ((rtx *));
rtx
gen_split_1212 (operands)
      rtx *operands;
{
  rtx operand0;
  rtx operand1;
  rtx _val = 0;
  start_sequence ();
operands[0] = gen_lowpart (SImode, operands[0]);
   operands[1] = gen_int_mode (INTVAL (operands[1]) >> 8, SImode);
  operand0 = operands[0];
  operand1 = operands[1];
  emit_insn (gen_rtx_SET (VOIDmode,
	gen_rtx_REG (CCNOmode,
	17),
	gen_rtx_COMPARE (CCNOmode,
	gen_rtx_AND (SImode,
	gen_rtx_ZERO_EXTRACT (SImode,
	operand0,
	GEN_INT (8LL),
	GEN_INT (8LL)),
	operand1),
	const0_rtx)));
  _val = get_insns ();
  end_sequence ();
  return _val;
}

/* ../../gcc/gcc/config/i386/i386.md:7775 */
extern rtx gen_split_1213 PARAMS ((rtx *));
rtx
gen_split_1213 (operands)
      rtx *operands;
{
  rtx operand0;
  rtx operand1;
  rtx _val = 0;
  start_sequence ();
operands[0] = gen_lowpart (QImode, operands[0]);
   operands[1] = gen_lowpart (QImode, operands[1]);
  operand0 = operands[0];
  operand1 = operands[1];
  emit_insn (gen_rtx_SET (VOIDmode,
	gen_rtx_REG (CCNOmode,
	17),
	gen_rtx_COMPARE (CCNOmode,
	gen_rtx_AND (QImode,
	operand0,
	operand1),
	const0_rtx)));
  _val = get_insns ();
  end_sequence ();
  return _val;
}

/* ../../gcc/gcc/config/i386/i386.md:7801 */
rtx
gen_anddi3 (operand0, operand1, operand2)
     rtx operand0;
     rtx operand1;
     rtx operand2;
{
  rtx _val = 0;
  start_sequence ();
  {
    rtx operands[3];
    operands[0] = operand0;
    operands[1] = operand1;
    operands[2] = operand2;
ix86_expand_binary_operator (AND, DImode, operands); DONE;
    operand0 = operands[0];
    operand1 = operands[1];
    operand2 = operands[2];
  }
  emit_insn (gen_rtx_SET (VOIDmode,
	operand0,
	gen_rtx_AND (DImode,
	operand1,
	operand2)));
  emit_insn (gen_rtx_CLOBBER (VOIDmode,
	gen_rtx_REG (CCmode,
	17)));
  _val = get_insns ();
  end_sequence ();
  return _val;
}

/* ../../gcc/gcc/config/i386/i386.md:7867 */
rtx
gen_andsi3 (operand0, operand1, operand2)
     rtx operand0;
     rtx operand1;
     rtx operand2;
{
  rtx _val = 0;
  start_sequence ();
  {
    rtx operands[3];
    operands[0] = operand0;
    operands[1] = operand1;
    operands[2] = operand2;
ix86_expand_binary_operator (AND, SImode, operands); DONE;
    operand0 = operands[0];
    operand1 = operands[1];
    operand2 = operands[2];
  }
  emit_insn (gen_rtx_SET (VOIDmode,
	operand0,
	gen_rtx_AND (SImode,
	operand1,
	operand2)));
  emit_insn (gen_rtx_CLOBBER (VOIDmode,
	gen_rtx_REG (CCmode,
	17)));
  _val = get_insns ();
  end_sequence ();
  return _val;
}

/* ../../gcc/gcc/config/i386/i386.md:7914 */
extern rtx gen_split_1216 PARAMS ((rtx *));
rtx
gen_split_1216 (operands)
      rtx *operands;
{
  rtx operand0;
  rtx operand1;
  rtx _val = 0;
  start_sequence ();
operands[1] = gen_lowpart (HImode, operands[0]);
  operand0 = operands[0];
  operand1 = operands[1];
  emit_insn (gen_rtx_SET (VOIDmode,
	gen_rtx_STRICT_LOW_PART (VOIDmode,
	operand1),
	const0_rtx));
  _val = get_insns ();
  end_sequence ();
  return _val;
}

/* ../../gcc/gcc/config/i386/i386.md:7923 */
extern rtx gen_split_1217 PARAMS ((rtx *));
rtx
gen_split_1217 (operands)
      rtx *operands;
{
  rtx operand0;
  rtx operand1;
  rtx _val = 0;
  start_sequence ();
operands[1] = gen_lowpart (QImode, operands[0]);
  operand0 = operands[0];
  operand1 = operands[1];
  emit_insn (gen_rtx_SET (VOIDmode,
	gen_rtx_STRICT_LOW_PART (VOIDmode,
	operand1),
	const0_rtx));
  _val = get_insns ();
  end_sequence ();
  return _val;
}

/* ../../gcc/gcc/config/i386/i386.md:7932 */
extern rtx gen_split_1218 PARAMS ((rtx *));
rtx
gen_split_1218 (operands)
      rtx *operands;
{
  rtx operand0;
  rtx _val = 0;
  start_sequence ();
operands[0] = gen_lowpart (SImode, operands[0]);
  operand0 = operands[0];
  emit (gen_rtx_PARALLEL (VOIDmode,
	gen_rtvec (2,
		gen_rtx_SET (VOIDmode,
	gen_rtx_ZERO_EXTRACT (SImode,
	operand0,
	GEN_INT (8LL),
	GEN_INT (8LL)),
	gen_rtx_XOR (SImode,
	gen_rtx_ZERO_EXTRACT (SImode,
	copy_rtx (operand0),
	GEN_INT (8LL),
	GEN_INT (8LL)),
	gen_rtx_ZERO_EXTRACT (SImode,
	copy_rtx (operand0),
	GEN_INT (8LL),
	GEN_INT (8LL)))),
		gen_rtx_CLOBBER (VOIDmode,
	gen_rtx_REG (CCmode,
	17)))));
  _val = get_insns ();
  end_sequence ();
  return _val;
}

/* ../../gcc/gcc/config/i386/i386.md:7990 */
rtx
gen_andhi3 (operand0, operand1, operand2)
     rtx operand0;
     rtx operand1;
     rtx operand2;
{
  rtx _val = 0;
  start_sequence ();
  {
    rtx operands[3];
    operands[0] = operand0;
    operands[1] = operand1;
    operands[2] = operand2;
ix86_expand_binary_operator (AND, HImode, operands); DONE;
    operand0 = operands[0];
    operand1 = operands[1];
    operand2 = operands[2];
  }
  emit_insn (gen_rtx_SET (VOIDmode,
	operand0,
	gen_rtx_AND (HImode,
	operand1,
	operand2)));
  emit_insn (gen_rtx_CLOBBER (VOIDmode,
	gen_rtx_REG (CCmode,
	17)));
  _val = get_insns ();
  end_sequence ();
  return _val;
}

/* ../../gcc/gcc/config/i386/i386.md:8038 */
rtx
gen_andqi3 (operand0, operand1, operand2)
     rtx operand0;
     rtx operand1;
     rtx operand2;
{
  rtx _val = 0;
  start_sequence ();
  {
    rtx operands[3];
    operands[0] = operand0;
    operands[1] = operand1;
    operands[2] = operand2;
ix86_expand_binary_operator (AND, QImode, operands); DONE;
    operand0 = operands[0];
    operand1 = operands[1];
    operand2 = operands[2];
  }
  emit_insn (gen_rtx_SET (VOIDmode,
	operand0,
	gen_rtx_AND (QImode,
	operand1,
	operand2)));
  emit_insn (gen_rtx_CLOBBER (VOIDmode,
	gen_rtx_REG (CCmode,
	17)));
  _val = get_insns ();
  end_sequence ();
  return _val;
}

/* ../../gcc/gcc/config/i386/i386.md:8219 */
extern rtx gen_split_1221 PARAMS ((rtx *));
rtx
gen_split_1221 (operands)
      rtx *operands;
{
  rtx operand0;
  rtx operand1;
  rtx operand2;
  rtx _val = 0;
  start_sequence ();
operands[0] = gen_lowpart (SImode, operands[0]);
   operands[1] = gen_lowpart (SImode, operands[1]);
   operands[2] = gen_int_mode ((INTVAL (operands[2]) >> 8) & 0xff, SImode);
  operand0 = operands[0];
  operand1 = operands[1];
  operand2 = operands[2];
  emit (gen_rtx_PARALLEL (VOIDmode,
	gen_rtvec (2,
		gen_rtx_SET (VOIDmode,
	gen_rtx_ZERO_EXTRACT (SImode,
	operand0,
	GEN_INT (8LL),
	GEN_INT (8LL)),
	gen_rtx_AND (SImode,
	gen_rtx_ZERO_EXTRACT (SImode,
	operand1,
	GEN_INT (8LL),
	GEN_INT (8LL)),
	operand2)),
		gen_rtx_CLOBBER (VOIDmode,
	gen_rtx_REG (CCmode,
	17)))));
  _val = get_insns ();
  end_sequence ();
  return _val;
}

/* ../../gcc/gcc/config/i386/i386.md:8240 */
extern rtx gen_split_1222 PARAMS ((rtx *));
rtx
gen_split_1222 (operands)
      rtx *operands;
{
  rtx operand0;
  rtx operand1;
  rtx operand2;
  rtx _val = 0;
  start_sequence ();
operands[0] = gen_lowpart (QImode, operands[0]);
   operands[1] = gen_lowpart (QImode, operands[1]);
   operands[2] = gen_lowpart (QImode, operands[2]);
  operand0 = operands[0];
  operand1 = operands[1];
  operand2 = operands[2];
  emit (gen_rtx_PARALLEL (VOIDmode,
	gen_rtvec (2,
		gen_rtx_SET (VOIDmode,
	gen_rtx_STRICT_LOW_PART (VOIDmode,
	operand0),
	gen_rtx_AND (QImode,
	operand1,
	operand2)),
		gen_rtx_CLOBBER (VOIDmode,
	gen_rtx_REG (CCmode,
	17)))));
  _val = get_insns ();
  end_sequence ();
  return _val;
}

/* ../../gcc/gcc/config/i386/i386.md:8264 */
rtx
gen_iordi3 (operand0, operand1, operand2)
     rtx operand0;
     rtx operand1;
     rtx operand2;
{
  rtx _val = 0;
  start_sequence ();
  {
    rtx operands[3];
    operands[0] = operand0;
    operands[1] = operand1;
    operands[2] = operand2;
ix86_expand_binary_operator (IOR, DImode, operands); DONE;
    operand0 = operands[0];
    operand1 = operands[1];
    operand2 = operands[2];
  }
  emit_insn (gen_rtx_SET (VOIDmode,
	operand0,
	gen_rtx_IOR (DImode,
	operand1,
	operand2)));
  emit_insn (gen_rtx_CLOBBER (VOIDmode,
	gen_rtx_REG (CCmode,
	17)));
  _val = get_insns ();
  end_sequence ();
  return _val;
}

/* ../../gcc/gcc/config/i386/i386.md:8311 */
rtx
gen_iorsi3 (operand0, operand1, operand2)
     rtx operand0;
     rtx operand1;
     rtx operand2;
{
  rtx _val = 0;
  start_sequence ();
  {
    rtx operands[3];
    operands[0] = operand0;
    operands[1] = operand1;
    operands[2] = operand2;
ix86_expand_binary_operator (IOR, SImode, operands); DONE;
    operand0 = operands[0];
    operand1 = operands[1];
    operand2 = operands[2];
  }
  emit_insn (gen_rtx_SET (VOIDmode,
	operand0,
	gen_rtx_IOR (SImode,
	operand1,
	operand2)));
  emit_insn (gen_rtx_CLOBBER (VOIDmode,
	gen_rtx_REG (CCmode,
	17)));
  _val = get_insns ();
  end_sequence ();
  return _val;
}

/* ../../gcc/gcc/config/i386/i386.md:8404 */
rtx
gen_iorhi3 (operand0, operand1, operand2)
     rtx operand0;
     rtx operand1;
     rtx operand2;
{
  rtx _val = 0;
  start_sequence ();
  {
    rtx operands[3];
    operands[0] = operand0;
    operands[1] = operand1;
    operands[2] = operand2;
ix86_expand_binary_operator (IOR, HImode, operands); DONE;
    operand0 = operands[0];
    operand1 = operands[1];
    operand2 = operands[2];
  }
  emit_insn (gen_rtx_SET (VOIDmode,
	operand0,
	gen_rtx_IOR (HImode,
	operand1,
	operand2)));
  emit_insn (gen_rtx_CLOBBER (VOIDmode,
	gen_rtx_REG (CCmode,
	17)));
  _val = get_insns ();
  end_sequence ();
  return _val;
}

/* ../../gcc/gcc/config/i386/i386.md:8447 */
rtx
gen_iorqi3 (operand0, operand1, operand2)
     rtx operand0;
     rtx operand1;
     rtx operand2;
{
  rtx _val = 0;
  start_sequence ();
  {
    rtx operands[3];
    operands[0] = operand0;
    operands[1] = operand1;
    operands[2] = operand2;
ix86_expand_binary_operator (IOR, QImode, operands); DONE;
    operand0 = operands[0];
    operand1 = operands[1];
    operand2 = operands[2];
  }
  emit_insn (gen_rtx_SET (VOIDmode,
	operand0,
	gen_rtx_IOR (QImode,
	operand1,
	operand2)));
  emit_insn (gen_rtx_CLOBBER (VOIDmode,
	gen_rtx_REG (CCmode,
	17)));
  _val = get_insns ();
  end_sequence ();
  return _val;
}

/* ../../gcc/gcc/config/i386/i386.md:8592 */
extern rtx gen_split_1227 PARAMS ((rtx *));
rtx
gen_split_1227 (operands)
      rtx *operands;
{
  rtx operand0;
  rtx operand1;
  rtx operand2;
  rtx _val = 0;
  start_sequence ();
operands[0] = gen_lowpart (SImode, operands[0]);
   operands[1] = gen_lowpart (SImode, operands[1]);
   operands[2] = gen_int_mode ((INTVAL (operands[2]) >> 8) & 0xff, SImode);
  operand0 = operands[0];
  operand1 = operands[1];
  operand2 = operands[2];
  emit (gen_rtx_PARALLEL (VOIDmode,
	gen_rtvec (2,
		gen_rtx_SET (VOIDmode,
	gen_rtx_ZERO_EXTRACT (SImode,
	operand0,
	GEN_INT (8LL),
	GEN_INT (8LL)),
	gen_rtx_IOR (SImode,
	gen_rtx_ZERO_EXTRACT (SImode,
	operand1,
	GEN_INT (8LL),
	GEN_INT (8LL)),
	operand2)),
		gen_rtx_CLOBBER (VOIDmode,
	gen_rtx_REG (CCmode,
	17)))));
  _val = get_insns ();
  end_sequence ();
  return _val;
}

/* ../../gcc/gcc/config/i386/i386.md:8613 */
extern rtx gen_split_1228 PARAMS ((rtx *));
rtx
gen_split_1228 (operands)
      rtx *operands;
{
  rtx operand0;
  rtx operand1;
  rtx operand2;
  rtx _val = 0;
  start_sequence ();
operands[0] = gen_lowpart (QImode, operands[0]);
   operands[1] = gen_lowpart (QImode, operands[1]);
   operands[2] = gen_lowpart (QImode, operands[2]);
  operand0 = operands[0];
  operand1 = operands[1];
  operand2 = operands[2];
  emit (gen_rtx_PARALLEL (VOIDmode,
	gen_rtvec (2,
		gen_rtx_SET (VOIDmode,
	gen_rtx_STRICT_LOW_PART (VOIDmode,
	operand0),
	gen_rtx_IOR (QImode,
	operand1,
	operand2)),
		gen_rtx_CLOBBER (VOIDmode,
	gen_rtx_REG (CCmode,
	17)))));
  _val = get_insns ();
  end_sequence ();
  return _val;
}

/* ../../gcc/gcc/config/i386/i386.md:8637 */
rtx
gen_xordi3 (operand0, operand1, operand2)
     rtx operand0;
     rtx operand1;
     rtx operand2;
{
  rtx _val = 0;
  start_sequence ();
  {
    rtx operands[3];
    operands[0] = operand0;
    operands[1] = operand1;
    operands[2] = operand2;
ix86_expand_binary_operator (XOR, DImode, operands); DONE;
    operand0 = operands[0];
    operand1 = operands[1];
    operand2 = operands[2];
  }
  emit_insn (gen_rtx_SET (VOIDmode,
	operand0,
	gen_rtx_XOR (DImode,
	operand1,
	operand2)));
  emit_insn (gen_rtx_CLOBBER (VOIDmode,
	gen_rtx_REG (CCmode,
	17)));
  _val = get_insns ();
  end_sequence ();
  return _val;
}

/* ../../gcc/gcc/config/i386/i386.md:8687 */
rtx
gen_xorsi3 (operand0, operand1, operand2)
     rtx operand0;
     rtx operand1;
     rtx operand2;
{
  rtx _val = 0;
  start_sequence ();
  {
    rtx operands[3];
    operands[0] = operand0;
    operands[1] = operand1;
    operands[2] = operand2;
ix86_expand_binary_operator (XOR, SImode, operands); DONE;
    operand0 = operands[0];
    operand1 = operands[1];
    operand2 = operands[2];
  }
  emit_insn (gen_rtx_SET (VOIDmode,
	operand0,
	gen_rtx_XOR (SImode,
	operand1,
	operand2)));
  emit_insn (gen_rtx_CLOBBER (VOIDmode,
	gen_rtx_REG (CCmode,
	17)));
  _val = get_insns ();
  end_sequence ();
  return _val;
}

/* ../../gcc/gcc/config/i386/i386.md:8781 */
rtx
gen_xorhi3 (operand0, operand1, operand2)
     rtx operand0;
     rtx operand1;
     rtx operand2;
{
  rtx _val = 0;
  start_sequence ();
  {
    rtx operands[3];
    operands[0] = operand0;
    operands[1] = operand1;
    operands[2] = operand2;
ix86_expand_binary_operator (XOR, HImode, operands); DONE;
    operand0 = operands[0];
    operand1 = operands[1];
    operand2 = operands[2];
  }
  emit_insn (gen_rtx_SET (VOIDmode,
	operand0,
	gen_rtx_XOR (HImode,
	operand1,
	operand2)));
  emit_insn (gen_rtx_CLOBBER (VOIDmode,
	gen_rtx_REG (CCmode,
	17)));
  _val = get_insns ();
  end_sequence ();
  return _val;
}

/* ../../gcc/gcc/config/i386/i386.md:8824 */
rtx
gen_xorqi3 (operand0, operand1, operand2)
     rtx operand0;
     rtx operand1;
     rtx operand2;
{
  rtx _val = 0;
  start_sequence ();
  {
    rtx operands[3];
    operands[0] = operand0;
    operands[1] = operand1;
    operands[2] = operand2;
ix86_expand_binary_operator (XOR, QImode, operands); DONE;
    operand0 = operands[0];
    operand1 = operands[1];
    operand2 = operands[2];
  }
  emit_insn (gen_rtx_SET (VOIDmode,
	operand0,
	gen_rtx_XOR (QImode,
	operand1,
	operand2)));
  emit_insn (gen_rtx_CLOBBER (VOIDmode,
	gen_rtx_REG (CCmode,
	17)));
  _val = get_insns ();
  end_sequence ();
  return _val;
}

/* ../../gcc/gcc/config/i386/i386.md:9013 */
rtx
gen_xorqi_cc_ext_1 (operand0, operand1, operand2)
     rtx operand0;
     rtx operand1;
     rtx operand2;
{
  return gen_rtx_PARALLEL (VOIDmode,
	gen_rtvec (2,
		gen_rtx_SET (VOIDmode,
	gen_rtx_REG (CCNOmode,
	17),
	gen_rtx_COMPARE (CCNOmode,
	gen_rtx_XOR (SImode,
	gen_rtx_ZERO_EXTRACT (SImode,
	operand1,
	GEN_INT (8LL),
	GEN_INT (8LL)),
	operand2),
	const0_rtx)),
		gen_rtx_SET (VOIDmode,
	gen_rtx_ZERO_EXTRACT (SImode,
	operand0,
	GEN_INT (8LL),
	GEN_INT (8LL)),
	gen_rtx_XOR (SImode,
	gen_rtx_ZERO_EXTRACT (SImode,
	operand1,
	GEN_INT (8LL),
	GEN_INT (8LL)),
	operand2))));
}

/* ../../gcc/gcc/config/i386/i386.md:9033 */
extern rtx gen_split_1234 PARAMS ((rtx *));
rtx
gen_split_1234 (operands)
      rtx *operands;
{
  rtx operand0;
  rtx operand1;
  rtx operand2;
  rtx _val = 0;
  start_sequence ();
operands[0] = gen_lowpart (SImode, operands[0]);
   operands[1] = gen_lowpart (SImode, operands[1]);
   operands[2] = gen_int_mode ((INTVAL (operands[2]) >> 8) & 0xff, SImode);
  operand0 = operands[0];
  operand1 = operands[1];
  operand2 = operands[2];
  emit (gen_rtx_PARALLEL (VOIDmode,
	gen_rtvec (2,
		gen_rtx_SET (VOIDmode,
	gen_rtx_ZERO_EXTRACT (SImode,
	operand0,
	GEN_INT (8LL),
	GEN_INT (8LL)),
	gen_rtx_XOR (SImode,
	gen_rtx_ZERO_EXTRACT (SImode,
	operand1,
	GEN_INT (8LL),
	GEN_INT (8LL)),
	operand2)),
		gen_rtx_CLOBBER (VOIDmode,
	gen_rtx_REG (CCmode,
	17)))));
  _val = get_insns ();
  end_sequence ();
  return _val;
}

/* ../../gcc/gcc/config/i386/i386.md:9054 */
extern rtx gen_split_1235 PARAMS ((rtx *));
rtx
gen_split_1235 (operands)
      rtx *operands;
{
  rtx operand0;
  rtx operand1;
  rtx operand2;
  rtx _val = 0;
  start_sequence ();
operands[0] = gen_lowpart (QImode, operands[0]);
   operands[1] = gen_lowpart (QImode, operands[1]);
   operands[2] = gen_lowpart (QImode, operands[2]);
  operand0 = operands[0];
  operand1 = operands[1];
  operand2 = operands[2];
  emit (gen_rtx_PARALLEL (VOIDmode,
	gen_rtvec (2,
		gen_rtx_SET (VOIDmode,
	gen_rtx_STRICT_LOW_PART (VOIDmode,
	operand0),
	gen_rtx_XOR (QImode,
	operand1,
	operand2)),
		gen_rtx_CLOBBER (VOIDmode,
	gen_rtx_REG (CCmode,
	17)))));
  _val = get_insns ();
  end_sequence ();
  return _val;
}

/* ../../gcc/gcc/config/i386/i386.md:9075 */
rtx
gen_negdi2 (operand0, operand1)
     rtx operand0;
     rtx operand1;
{
  rtx _val = 0;
  start_sequence ();
  {
    rtx operands[2];
    operands[0] = operand0;
    operands[1] = operand1;
ix86_expand_unary_operator (NEG, DImode, operands); DONE;
    operand0 = operands[0];
    operand1 = operands[1];
  }
  emit (gen_rtx_PARALLEL (VOIDmode,
	gen_rtvec (2,
		gen_rtx_SET (VOIDmode,
	operand0,
	gen_rtx_NEG (DImode,
	operand1)),
		gen_rtx_CLOBBER (VOIDmode,
	gen_rtx_REG (CCmode,
	17)))));
  _val = get_insns ();
  end_sequence ();
  return _val;
}

/* ../../gcc/gcc/config/i386/i386.md:9090 */
extern rtx gen_split_1237 PARAMS ((rtx *));
rtx
gen_split_1237 (operands)
      rtx *operands;
{
  rtx operand0;
  rtx operand1;
  rtx operand2;
  rtx operand3;
  rtx _val = 0;
  start_sequence ();
split_di (operands+1, 1, operands+2, operands+3);
   split_di (operands+0, 1, operands+0, operands+1);
  operand0 = operands[0];
  operand1 = operands[1];
  operand2 = operands[2];
  operand3 = operands[3];
  emit (gen_rtx_PARALLEL (VOIDmode,
	gen_rtvec (2,
		gen_rtx_SET (VOIDmode,
	gen_rtx_REG (CCZmode,
	17),
	gen_rtx_COMPARE (CCZmode,
	gen_rtx_NEG (SImode,
	operand2),
	const0_rtx)),
		gen_rtx_SET (VOIDmode,
	operand0,
	gen_rtx_NEG (SImode,
	copy_rtx (operand2))))));
  emit (gen_rtx_PARALLEL (VOIDmode,
	gen_rtvec (2,
		gen_rtx_SET (VOIDmode,
	operand1,
	gen_rtx_PLUS (SImode,
	gen_rtx_PLUS (SImode,
	gen_rtx_LTU (SImode,
	gen_rtx_REG (CCmode,
	17),
	const0_rtx),
	operand3),
	const0_rtx)),
		gen_rtx_CLOBBER (VOIDmode,
	gen_rtx_REG (CCmode,
	17)))));
  emit (gen_rtx_PARALLEL (VOIDmode,
	gen_rtvec (2,
		gen_rtx_SET (VOIDmode,
	copy_rtx (operand1),
	gen_rtx_NEG (SImode,
	copy_rtx (operand1))),
		gen_rtx_CLOBBER (VOIDmode,
	gen_rtx_REG (CCmode,
	17)))));
  _val = get_insns ();
  end_sequence ();
  return _val;
}

/* ../../gcc/gcc/config/i386/i386.md:9137 */
rtx
gen_negsi2 (operand0, operand1)
     rtx operand0;
     rtx operand1;
{
  rtx _val = 0;
  start_sequence ();
  {
    rtx operands[2];
    operands[0] = operand0;
    operands[1] = operand1;
ix86_expand_unary_operator (NEG, SImode, operands); DONE;
    operand0 = operands[0];
    operand1 = operands[1];
  }
  emit (gen_rtx_PARALLEL (VOIDmode,
	gen_rtvec (2,
		gen_rtx_SET (VOIDmode,
	operand0,
	gen_rtx_NEG (SImode,
	operand1)),
		gen_rtx_CLOBBER (VOIDmode,
	gen_rtx_REG (CCmode,
	17)))));
  _val = get_insns ();
  end_sequence ();
  return _val;
}

/* ../../gcc/gcc/config/i386/i386.md:9197 */
rtx
gen_neghi2 (operand0, operand1)
     rtx operand0;
     rtx operand1;
{
  rtx _val = 0;
  start_sequence ();
  {
    rtx operands[2];
    operands[0] = operand0;
    operands[1] = operand1;
ix86_expand_unary_operator (NEG, HImode, operands); DONE;
    operand0 = operands[0];
    operand1 = operands[1];
  }
  emit (gen_rtx_PARALLEL (VOIDmode,
	gen_rtvec (2,
		gen_rtx_SET (VOIDmode,
	operand0,
	gen_rtx_NEG (HImode,
	operand1)),
		gen_rtx_CLOBBER (VOIDmode,
	gen_rtx_REG (CCmode,
	17)))));
  _val = get_insns ();
  end_sequence ();
  return _val;
}

/* ../../gcc/gcc/config/i386/i386.md:9224 */
rtx
gen_negqi2 (operand0, operand1)
     rtx operand0;
     rtx operand1;
{
  rtx _val = 0;
  start_sequence ();
  {
    rtx operands[2];
    operands[0] = operand0;
    operands[1] = operand1;
ix86_expand_unary_operator (NEG, QImode, operands); DONE;
    operand0 = operands[0];
    operand1 = operands[1];
  }
  emit (gen_rtx_PARALLEL (VOIDmode,
	gen_rtvec (2,
		gen_rtx_SET (VOIDmode,
	operand0,
	gen_rtx_NEG (QImode,
	operand1)),
		gen_rtx_CLOBBER (VOIDmode,
	gen_rtx_REG (CCmode,
	17)))));
  _val = get_insns ();
  end_sequence ();
  return _val;
}

/* ../../gcc/gcc/config/i386/i386.md:9253 */
rtx
gen_negsf2 (operand0, operand1)
     rtx operand0;
     rtx operand1;
{
  rtx _val = 0;
  start_sequence ();
  {
    rtx operands[2];
    operands[0] = operand0;
    operands[1] = operand1;
if (TARGET_SSE)
     {
       /* In case operand is in memory,  we will not use SSE.  */
       if (memory_operand (operands[0], VOIDmode)
	   && rtx_equal_p (operands[0], operands[1]))
	 emit_insn (gen_negsf2_memory (operands[0], operands[1]));
       else
	{
	  /* Using SSE is tricky, since we need bitwise negation of -0
	     in register.  */
	  rtx reg = gen_reg_rtx (SFmode);
	  rtx dest = operands[0];

	  operands[1] = force_reg (SFmode, operands[1]);
	  operands[0] = force_reg (SFmode, operands[0]);
	  emit_move_insn (reg,
			  gen_lowpart (SFmode,
				       gen_int_mode (0x80000000, SImode)));
	  emit_insn (gen_negsf2_ifs (operands[0], operands[1], reg));
	  if (dest != operands[0])
	    emit_move_insn (dest, operands[0]);
	}
       DONE;
     }
   ix86_expand_unary_operator (NEG, SFmode, operands); DONE;
    operand0 = operands[0];
    operand1 = operands[1];
  }
  emit (gen_rtx_PARALLEL (VOIDmode,
	gen_rtvec (2,
		gen_rtx_SET (VOIDmode,
	operand0,
	gen_rtx_NEG (SFmode,
	operand1)),
		gen_rtx_CLOBBER (VOIDmode,
	gen_rtx_REG (CCmode,
	17)))));
  _val = get_insns ();
  end_sequence ();
  return _val;
}

/* ../../gcc/gcc/config/i386/i386.md:9302 */
extern rtx gen_split_1242 PARAMS ((rtx *));
rtx
gen_split_1242 (operands)
      rtx *operands;
{
  rtx operand0;
  rtx operand1;
  rtx _val = 0;
  start_sequence ();
  operand0 = operands[0];
  operand1 = operands[1];
  emit (gen_rtx_PARALLEL (VOIDmode,
	gen_rtvec (2,
		gen_rtx_SET (VOIDmode,
	operand0,
	gen_rtx_NEG (SFmode,
	operand1)),
		gen_rtx_CLOBBER (VOIDmode,
	gen_rtx_REG (CCmode,
	17)))));
  _val = get_insns ();
  end_sequence ();
  return _val;
}

/* ../../gcc/gcc/config/i386/i386.md:9312 */
extern rtx gen_split_1243 PARAMS ((rtx *));
rtx
gen_split_1243 (operands)
      rtx *operands;
{
  rtx operand0;
  rtx operand1;
  rtx _val = 0;
  start_sequence ();
  operand0 = operands[0];
  operand1 = operands[1];
  emit (gen_rtx_PARALLEL (VOIDmode,
	gen_rtvec (2,
		gen_rtx_SET (VOIDmode,
	operand0,
	gen_rtx_NEG (SFmode,
	operand1)),
		gen_rtx_CLOBBER (VOIDmode,
	gen_rtx_REG (CCmode,
	17)))));
  _val = get_insns ();
  end_sequence ();
  return _val;
}

/* ../../gcc/gcc/config/i386/i386.md:9322 */
extern rtx gen_split_1244 PARAMS ((rtx *));
rtx
gen_split_1244 (operands)
      rtx *operands;
{
  rtx operand0;
  rtx operand1;
  rtx operand2;
  rtx _val = 0;
  start_sequence ();
{
  if (operands_match_p (operands[0], operands[2]))
    {
      rtx tmp;
      tmp = operands[1];
      operands[1] = operands[2];
      operands[2] = tmp;
    }
}
  operand0 = operands[0];
  operand1 = operands[1];
  operand2 = operands[2];
  emit_insn (gen_rtx_SET (VOIDmode,
	gen_rtx_SUBREG (TImode,
	operand0,
	0),
	gen_rtx_XOR (TImode,
	gen_rtx_SUBREG (TImode,
	operand1,
	0),
	gen_rtx_SUBREG (TImode,
	operand2,
	0))));
  _val = get_insns ();
  end_sequence ();
  return _val;
}

/* ../../gcc/gcc/config/i386/i386.md:9353 */
extern rtx gen_split_1245 PARAMS ((rtx *));
rtx
gen_split_1245 (operands)
      rtx *operands;
{
  rtx operand0;
  rtx operand1;
  rtx _val = 0;
  start_sequence ();

  operand0 = operands[0];
  operand1 = operands[1];
  emit_insn (gen_rtx_SET (VOIDmode,
	operand0,
	gen_rtx_NEG (SFmode,
	operand1)));
  _val = get_insns ();
  end_sequence ();
  return _val;
}

/* ../../gcc/gcc/config/i386/i386.md:9362 */
extern rtx gen_split_1246 PARAMS ((rtx *));
rtx
gen_split_1246 (operands)
      rtx *operands;
{
  rtx operand0;
  rtx operand1;
  rtx _val = 0;
  start_sequence ();
operands[1] = gen_int_mode (0x80000000, SImode);
   operands[0] = gen_rtx_REG (SImode, REGNO (operands[0]));
  operand0 = operands[0];
  operand1 = operands[1];
  emit (gen_rtx_PARALLEL (VOIDmode,
	gen_rtvec (2,
		gen_rtx_SET (VOIDmode,
	operand0,
	gen_rtx_XOR (SImode,
	copy_rtx (operand0),
	operand1)),
		gen_rtx_CLOBBER (VOIDmode,
	gen_rtx_REG (CCmode,
	17)))));
  _val = get_insns ();
  end_sequence ();
  return _val;
}

/* ../../gcc/gcc/config/i386/i386.md:9372 */
extern rtx gen_split_1247 PARAMS ((rtx *));
rtx
gen_split_1247 (operands)
      rtx *operands;
{
  rtx operand0;
  rtx operand1;
  rtx _val = 0;
  start_sequence ();
{
  int size = GET_MODE_SIZE (GET_MODE (operands[1]));

  /* XFmode's size is 12, TFmode 16, but only 10 bytes are used.  */
  if (size >= 12)
    size = 10;
  operands[0] = adjust_address (operands[0], QImode, size - 1);
  operands[1] = gen_int_mode (0x80, QImode);
}
  operand0 = operands[0];
  operand1 = operands[1];
  emit (gen_rtx_PARALLEL (VOIDmode,
	gen_rtvec (2,
		gen_rtx_SET (VOIDmode,
	operand0,
	gen_rtx_XOR (QImode,
	copy_rtx (operand0),
	operand1)),
		gen_rtx_CLOBBER (VOIDmode,
	gen_rtx_REG (CCmode,
	17)))));
  _val = get_insns ();
  end_sequence ();
  return _val;
}

/* ../../gcc/gcc/config/i386/i386.md:9389 */
rtx
gen_negdf2 (operand0, operand1)
     rtx operand0;
     rtx operand1;
{
  rtx _val = 0;
  start_sequence ();
  {
    rtx operands[2];
    operands[0] = operand0;
    operands[1] = operand1;
if (TARGET_SSE2)
     {
       /* In case operand is in memory,  we will not use SSE.  */
       if (memory_operand (operands[0], VOIDmode)
	   && rtx_equal_p (operands[0], operands[1]))
	 emit_insn (gen_negdf2_memory (operands[0], operands[1]));
       else
	{
	  /* Using SSE is tricky, since we need bitwise negation of -0
	     in register.  */
	  rtx reg = gen_reg_rtx (DFmode);
#if HOST_BITS_PER_WIDE_INT >= 64
	  rtx imm = gen_int_mode (((HOST_WIDE_INT)1) << 63, DImode);
#else
	  rtx imm = immed_double_const (0, 0x80000000, DImode);
#endif
	  rtx dest = operands[0];

	  operands[1] = force_reg (DFmode, operands[1]);
	  operands[0] = force_reg (DFmode, operands[0]);
	  emit_move_insn (reg, gen_lowpart (DFmode, imm));
	  emit_insn (gen_negdf2_ifs (operands[0], operands[1], reg));
	  if (dest != operands[0])
	    emit_move_insn (dest, operands[0]);
	}
       DONE;
     }
   ix86_expand_unary_operator (NEG, DFmode, operands); DONE;
    operand0 = operands[0];
    operand1 = operands[1];
  }
  emit (gen_rtx_PARALLEL (VOIDmode,
	gen_rtvec (2,
		gen_rtx_SET (VOIDmode,
	operand0,
	gen_rtx_NEG (DFmode,
	operand1)),
		gen_rtx_CLOBBER (VOIDmode,
	gen_rtx_REG (CCmode,
	17)))));
  _val = get_insns ();
  end_sequence ();
  return _val;
}

/* ../../gcc/gcc/config/i386/i386.md:9452 */
extern rtx gen_split_1249 PARAMS ((rtx *));
rtx
gen_split_1249 (operands)
      rtx *operands;
{
  rtx operand0;
  rtx operand1;
  rtx _val = 0;
  start_sequence ();
  operand0 = operands[0];
  operand1 = operands[1];
  emit (gen_rtx_PARALLEL (VOIDmode,
	gen_rtvec (2,
		gen_rtx_SET (VOIDmode,
	operand0,
	gen_rtx_NEG (DFmode,
	operand1)),
		gen_rtx_CLOBBER (VOIDmode,
	gen_rtx_REG (CCmode,
	17)))));
  _val = get_insns ();
  end_sequence ();
  return _val;
}

/* ../../gcc/gcc/config/i386/i386.md:9462 */
extern rtx gen_split_1250 PARAMS ((rtx *));
rtx
gen_split_1250 (operands)
      rtx *operands;
{
  rtx operand0;
  rtx operand1;
  rtx _val = 0;
  start_sequence ();
  operand0 = operands[0];
  operand1 = operands[1];
  emit (gen_rtx_PARALLEL (VOIDmode,
	gen_rtvec (2,
		gen_rtx_SET (VOIDmode,
	operand0,
	gen_rtx_NEG (DFmode,
	operand1)),
		gen_rtx_CLOBBER (VOIDmode,
	gen_rtx_REG (CCmode,
	17)))));
  _val = get_insns ();
  end_sequence ();
  return _val;
}

/* ../../gcc/gcc/config/i386/i386.md:9473 */
extern rtx gen_split_1251 PARAMS ((rtx *));
rtx
gen_split_1251 (operands)
      rtx *operands;
{
  rtx operand0;
  rtx operand1;
  rtx operand2;
  rtx _val = 0;
  start_sequence ();
operands[0] = gen_lowpart (DImode, operands[0]);
    operands[1] = gen_lowpart (DImode, operands[1]);
    operands[2] = gen_lowpart (DImode, operands[2]);
  operand0 = operands[0];
  operand1 = operands[1];
  operand2 = operands[2];
  emit (gen_rtx_PARALLEL (VOIDmode,
	gen_rtvec (2,
		gen_rtx_SET (VOIDmode,
	operand0,
	gen_rtx_XOR (DImode,
	operand1,
	operand2)),
		gen_rtx_CLOBBER (VOIDmode,
	gen_rtx_REG (CCmode,
	17)))));
  _val = get_insns ();
  end_sequence ();
  return _val;
}

/* ../../gcc/gcc/config/i386/i386.md:9486 */
extern rtx gen_split_1252 PARAMS ((rtx *));
rtx
gen_split_1252 (operands)
      rtx *operands;
{
  rtx operand0;
  rtx operand1;
  rtx operand2;
  rtx _val = 0;
  start_sequence ();
{
  if (operands_match_p (operands[0], operands[2]))
    {
      rtx tmp;
      tmp = operands[1];
      operands[1] = operands[2];
      operands[2] = tmp;
    }
}
  operand0 = operands[0];
  operand1 = operands[1];
  operand2 = operands[2];
  emit_insn (gen_rtx_SET (VOIDmode,
	gen_rtx_SUBREG (TImode,
	operand0,
	0),
	gen_rtx_XOR (TImode,
	gen_rtx_SUBREG (TImode,
	operand1,
	0),
	gen_rtx_SUBREG (TImode,
	operand2,
	0))));
  _val = get_insns ();
  end_sequence ();
  return _val;
}

/* ../../gcc/gcc/config/i386/i386.md:9528 */
extern rtx gen_split_1253 PARAMS ((rtx *));
rtx
gen_split_1253 (operands)
      rtx *operands;
{
  rtx operand0;
  rtx operand1;
  rtx _val = 0;
  start_sequence ();

  operand0 = operands[0];
  operand1 = operands[1];
  emit_insn (gen_rtx_SET (VOIDmode,
	operand0,
	gen_rtx_NEG (DFmode,
	operand1)));
  _val = get_insns ();
  end_sequence ();
  return _val;
}

/* ../../gcc/gcc/config/i386/i386.md:9537 */
extern rtx gen_split_1254 PARAMS ((rtx *));
rtx
gen_split_1254 (operands)
      rtx *operands;
{
  rtx operand0;
  rtx operand1;
  rtx operand2;
  rtx operand3;
  rtx operand4;
  rtx _val = 0;
  start_sequence ();
operands[4] = gen_int_mode (0x80000000, SImode);
   split_di (operands+0, 1, operands+2, operands+3);
  operand0 = operands[0];
  operand1 = operands[1];
  operand2 = operands[2];
  operand3 = operands[3];
  operand4 = operands[4];
  emit (gen_rtx_PARALLEL (VOIDmode,
	gen_rtvec (2,
		gen_rtx_SET (VOIDmode,
	operand3,
	gen_rtx_XOR (SImode,
	copy_rtx (operand3),
	operand4)),
		gen_rtx_CLOBBER (VOIDmode,
	gen_rtx_REG (CCmode,
	17)))));
  _val = get_insns ();
  end_sequence ();
  return _val;
}

/* ../../gcc/gcc/config/i386/i386.md:9547 */
rtx
gen_negxf2 (operand0, operand1)
     rtx operand0;
     rtx operand1;
{
  rtx _val = 0;
  start_sequence ();
  {
    rtx operands[2];
    operands[0] = operand0;
    operands[1] = operand1;
ix86_expand_unary_operator (NEG, XFmode, operands); DONE;
    operand0 = operands[0];
    operand1 = operands[1];
  }
  emit (gen_rtx_PARALLEL (VOIDmode,
	gen_rtvec (2,
		gen_rtx_SET (VOIDmode,
	operand0,
	gen_rtx_NEG (XFmode,
	operand1)),
		gen_rtx_CLOBBER (VOIDmode,
	gen_rtx_REG (CCmode,
	17)))));
  _val = get_insns ();
  end_sequence ();
  return _val;
}

/* ../../gcc/gcc/config/i386/i386.md:9554 */
rtx
gen_negtf2 (operand0, operand1)
     rtx operand0;
     rtx operand1;
{
  rtx _val = 0;
  start_sequence ();
  {
    rtx operands[2];
    operands[0] = operand0;
    operands[1] = operand1;
ix86_expand_unary_operator (NEG, TFmode, operands); DONE;
    operand0 = operands[0];
    operand1 = operands[1];
  }
  emit (gen_rtx_PARALLEL (VOIDmode,
	gen_rtvec (2,
		gen_rtx_SET (VOIDmode,
	operand0,
	gen_rtx_NEG (TFmode,
	operand1)),
		gen_rtx_CLOBBER (VOIDmode,
	gen_rtx_REG (CCmode,
	17)))));
  _val = get_insns ();
  end_sequence ();
  return _val;
}

/* ../../gcc/gcc/config/i386/i386.md:9572 */
extern rtx gen_split_1257 PARAMS ((rtx *));
rtx
gen_split_1257 (operands)
      rtx *operands;
{
  rtx operand0;
  rtx operand1;
  rtx _val = 0;
  start_sequence ();

  operand0 = operands[0];
  operand1 = operands[1];
  emit_insn (gen_rtx_SET (VOIDmode,
	operand0,
	gen_rtx_NEG (XFmode,
	operand1)));
  _val = get_insns ();
  end_sequence ();
  return _val;
}

/* ../../gcc/gcc/config/i386/i386.md:9581 */
extern rtx gen_split_1258 PARAMS ((rtx *));
rtx
gen_split_1258 (operands)
      rtx *operands;
{
  rtx operand0;
  rtx operand1;
  rtx _val = 0;
  start_sequence ();
operands[1] = GEN_INT (0x8000);
   operands[0] = gen_rtx_REG (SImode,
			      true_regnum (operands[0]) + (TARGET_64BIT ? 1 : 2));
  operand0 = operands[0];
  operand1 = operands[1];
  emit (gen_rtx_PARALLEL (VOIDmode,
	gen_rtvec (2,
		gen_rtx_SET (VOIDmode,
	operand0,
	gen_rtx_XOR (SImode,
	copy_rtx (operand0),
	operand1)),
		gen_rtx_CLOBBER (VOIDmode,
	gen_rtx_REG (CCmode,
	17)))));
  _val = get_insns ();
  end_sequence ();
  return _val;
}

/* ../../gcc/gcc/config/i386/i386.md:9602 */
extern rtx gen_split_1259 PARAMS ((rtx *));
rtx
gen_split_1259 (operands)
      rtx *operands;
{
  rtx operand0;
  rtx operand1;
  rtx _val = 0;
  start_sequence ();

  operand0 = operands[0];
  operand1 = operands[1];
  emit_insn (gen_rtx_SET (VOIDmode,
	operand0,
	gen_rtx_NEG (TFmode,
	operand1)));
  _val = get_insns ();
  end_sequence ();
  return _val;
}

/* ../../gcc/gcc/config/i386/i386.md:9611 */
extern rtx gen_split_1260 PARAMS ((rtx *));
rtx
gen_split_1260 (operands)
      rtx *operands;
{
  rtx operand0;
  rtx operand1;
  rtx _val = 0;
  start_sequence ();
operands[1] = GEN_INT (0x8000);
   operands[0] = gen_rtx_REG (SImode,
			      true_regnum (operands[0]) + (TARGET_64BIT ? 1 : 2));
  operand0 = operands[0];
  operand1 = operands[1];
  emit (gen_rtx_PARALLEL (VOIDmode,
	gen_rtvec (2,
		gen_rtx_SET (VOIDmode,
	operand0,
	gen_rtx_XOR (SImode,
	copy_rtx (operand0),
	operand1)),
		gen_rtx_CLOBBER (VOIDmode,
	gen_rtx_REG (CCmode,
	17)))));
  _val = get_insns ();
  end_sequence ();
  return _val;
}

/* ../../gcc/gcc/config/i386/i386.md:9713 */
rtx
gen_abssf2 (operand0, operand1)
     rtx operand0;
     rtx operand1;
{
  rtx _val = 0;
  start_sequence ();
  {
    rtx operands[2];
    operands[0] = operand0;
    operands[1] = operand1;
if (TARGET_SSE)
     {
       /* In case operand is in memory,  we will not use SSE.  */
       if (memory_operand (operands[0], VOIDmode)
	   && rtx_equal_p (operands[0], operands[1]))
	 emit_insn (gen_abssf2_memory (operands[0], operands[1]));
       else
	{
	  /* Using SSE is tricky, since we need bitwise negation of -0
	     in register.  */
	  rtx reg = gen_reg_rtx (SFmode);
	  rtx dest = operands[0];

	  operands[1] = force_reg (SFmode, operands[1]);
	  operands[0] = force_reg (SFmode, operands[0]);
	  emit_move_insn (reg,
			  gen_lowpart (SFmode,
				       gen_int_mode (0x80000000, SImode)));
	  emit_insn (gen_abssf2_ifs (operands[0], operands[1], reg));
	  if (dest != operands[0])
	    emit_move_insn (dest, operands[0]);
	}
       DONE;
     }
   ix86_expand_unary_operator (ABS, SFmode, operands); DONE;
    operand0 = operands[0];
    operand1 = operands[1];
  }
  emit (gen_rtx_PARALLEL (VOIDmode,
	gen_rtvec (2,
		gen_rtx_SET (VOIDmode,
	operand0,
	gen_rtx_NEG (SFmode,
	operand1)),
		gen_rtx_CLOBBER (VOIDmode,
	gen_rtx_REG (CCmode,
	17)))));
  _val = get_insns ();
  end_sequence ();
  return _val;
}

/* ../../gcc/gcc/config/i386/i386.md:9762 */
extern rtx gen_split_1262 PARAMS ((rtx *));
rtx
gen_split_1262 (operands)
      rtx *operands;
{
  rtx operand0;
  rtx operand1;
  rtx _val = 0;
  start_sequence ();
  operand0 = operands[0];
  operand1 = operands[1];
  emit (gen_rtx_PARALLEL (VOIDmode,
	gen_rtvec (2,
		gen_rtx_SET (VOIDmode,
	operand0,
	gen_rtx_ABS (SFmode,
	operand1)),
		gen_rtx_CLOBBER (VOIDmode,
	gen_rtx_REG (CCmode,
	17)))));
  _val = get_insns ();
  end_sequence ();
  return _val;
}

/* ../../gcc/gcc/config/i386/i386.md:9772 */
extern rtx gen_split_1263 PARAMS ((rtx *));
rtx
gen_split_1263 (operands)
      rtx *operands;
{
  rtx operand0;
  rtx operand1;
  rtx _val = 0;
  start_sequence ();
  operand0 = operands[0];
  operand1 = operands[1];
  emit (gen_rtx_PARALLEL (VOIDmode,
	gen_rtvec (2,
		gen_rtx_SET (VOIDmode,
	operand0,
	gen_rtx_ABS (SFmode,
	operand1)),
		gen_rtx_CLOBBER (VOIDmode,
	gen_rtx_REG (CCmode,
	17)))));
  _val = get_insns ();
  end_sequence ();
  return _val;
}

/* ../../gcc/gcc/config/i386/i386.md:9782 */
extern rtx gen_split_1264 PARAMS ((rtx *));
rtx
gen_split_1264 (operands)
      rtx *operands;
{
  rtx operand0;
  rtx operand1;
  rtx operand2;
  rtx _val = 0;
  start_sequence ();
  operand0 = operands[0];
  operand1 = operands[1];
  operand2 = operands[2];
  emit_insn (gen_rtx_SET (VOIDmode,
	gen_rtx_SUBREG (TImode,
	operand0,
	0),
	gen_rtx_AND (TImode,
	gen_rtx_NOT (TImode,
	gen_rtx_SUBREG (TImode,
	operand2,
	0)),
	gen_rtx_SUBREG (TImode,
	operand1,
	0))));
  _val = get_insns ();
  end_sequence ();
  return _val;
}

/* ../../gcc/gcc/config/i386/i386.md:9802 */
extern rtx gen_split_1265 PARAMS ((rtx *));
rtx
gen_split_1265 (operands)
      rtx *operands;
{
  rtx operand0;
  rtx operand1;
  rtx _val = 0;
  start_sequence ();

  operand0 = operands[0];
  operand1 = operands[1];
  emit_insn (gen_rtx_SET (VOIDmode,
	operand0,
	gen_rtx_ABS (SFmode,
	operand1)));
  _val = get_insns ();
  end_sequence ();
  return _val;
}

/* ../../gcc/gcc/config/i386/i386.md:9811 */
extern rtx gen_split_1266 PARAMS ((rtx *));
rtx
gen_split_1266 (operands)
      rtx *operands;
{
  rtx operand0;
  rtx operand1;
  rtx _val = 0;
  start_sequence ();
operands[1] = gen_int_mode (~0x80000000, SImode);
   operands[0] = gen_rtx_REG (SImode, REGNO (operands[0]));
  operand0 = operands[0];
  operand1 = operands[1];
  emit (gen_rtx_PARALLEL (VOIDmode,
	gen_rtvec (2,
		gen_rtx_SET (VOIDmode,
	operand0,
	gen_rtx_AND (SImode,
	copy_rtx (operand0),
	operand1)),
		gen_rtx_CLOBBER (VOIDmode,
	gen_rtx_REG (CCmode,
	17)))));
  _val = get_insns ();
  end_sequence ();
  return _val;
}

/* ../../gcc/gcc/config/i386/i386.md:9821 */
extern rtx gen_split_1267 PARAMS ((rtx *));
rtx
gen_split_1267 (operands)
      rtx *operands;
{
  rtx operand0;
  rtx operand1;
  rtx _val = 0;
  start_sequence ();
{
  int size = GET_MODE_SIZE (GET_MODE (operands[1]));

  /* XFmode's size is 12, TFmode 16, but only 10 bytes are used.  */
  if (size >= 12)
    size = 10;
  operands[0] = adjust_address (operands[0], QImode, size - 1);
  operands[1] = gen_int_mode (~0x80, QImode);
}
  operand0 = operands[0];
  operand1 = operands[1];
  emit (gen_rtx_PARALLEL (VOIDmode,
	gen_rtvec (2,
		gen_rtx_SET (VOIDmode,
	operand0,
	gen_rtx_AND (QImode,
	copy_rtx (operand0),
	operand1)),
		gen_rtx_CLOBBER (VOIDmode,
	gen_rtx_REG (CCmode,
	17)))));
  _val = get_insns ();
  end_sequence ();
  return _val;
}

/* ../../gcc/gcc/config/i386/i386.md:9838 */
rtx
gen_absdf2 (operand0, operand1)
     rtx operand0;
     rtx operand1;
{
  rtx _val = 0;
  start_sequence ();
  {
    rtx operands[2];
    operands[0] = operand0;
    operands[1] = operand1;
if (TARGET_SSE2)
     {
       /* In case operand is in memory,  we will not use SSE.  */
       if (memory_operand (operands[0], VOIDmode)
	   && rtx_equal_p (operands[0], operands[1]))
	 emit_insn (gen_absdf2_memory (operands[0], operands[1]));
       else
	{
	  /* Using SSE is tricky, since we need bitwise negation of -0
	     in register.  */
	  rtx reg = gen_reg_rtx (DFmode);
#if HOST_BITS_PER_WIDE_INT >= 64
	  rtx imm = gen_int_mode (((HOST_WIDE_INT)1) << 63, DImode);
#else
	  rtx imm = immed_double_const (0, 0x80000000, DImode);
#endif
	  rtx dest = operands[0];

	  operands[1] = force_reg (DFmode, operands[1]);
	  operands[0] = force_reg (DFmode, operands[0]);
	  emit_move_insn (reg, gen_lowpart (DFmode, imm));
	  emit_insn (gen_absdf2_ifs (operands[0], operands[1], reg));
	  if (dest != operands[0])
	    emit_move_insn (dest, operands[0]);
	}
       DONE;
     }
   ix86_expand_unary_operator (ABS, DFmode, operands); DONE;
    operand0 = operands[0];
    operand1 = operands[1];
  }
  emit (gen_rtx_PARALLEL (VOIDmode,
	gen_rtvec (2,
		gen_rtx_SET (VOIDmode,
	operand0,
	gen_rtx_NEG (DFmode,
	operand1)),
		gen_rtx_CLOBBER (VOIDmode,
	gen_rtx_REG (CCmode,
	17)))));
  _val = get_insns ();
  end_sequence ();
  return _val;
}

/* ../../gcc/gcc/config/i386/i386.md:9901 */
extern rtx gen_split_1269 PARAMS ((rtx *));
rtx
gen_split_1269 (operands)
      rtx *operands;
{
  rtx operand0;
  rtx operand1;
  rtx _val = 0;
  start_sequence ();
  operand0 = operands[0];
  operand1 = operands[1];
  emit (gen_rtx_PARALLEL (VOIDmode,
	gen_rtvec (2,
		gen_rtx_SET (VOIDmode,
	operand0,
	gen_rtx_ABS (DFmode,
	operand1)),
		gen_rtx_CLOBBER (VOIDmode,
	gen_rtx_REG (CCmode,
	17)))));
  _val = get_insns ();
  end_sequence ();
  return _val;
}

/* ../../gcc/gcc/config/i386/i386.md:9911 */
extern rtx gen_split_1270 PARAMS ((rtx *));
rtx
gen_split_1270 (operands)
      rtx *operands;
{
  rtx operand0;
  rtx operand1;
  rtx _val = 0;
  start_sequence ();
  operand0 = operands[0];
  operand1 = operands[1];
  emit (gen_rtx_PARALLEL (VOIDmode,
	gen_rtvec (2,
		gen_rtx_SET (VOIDmode,
	operand0,
	gen_rtx_ABS (DFmode,
	operand1)),
		gen_rtx_CLOBBER (VOIDmode,
	gen_rtx_REG (CCmode,
	17)))));
  _val = get_insns ();
  end_sequence ();
  return _val;
}

/* ../../gcc/gcc/config/i386/i386.md:9921 */
extern rtx gen_split_1271 PARAMS ((rtx *));
rtx
gen_split_1271 (operands)
      rtx *operands;
{
  rtx operand0;
  rtx operand1;
  rtx operand2;
  rtx _val = 0;
  start_sequence ();
  operand0 = operands[0];
  operand1 = operands[1];
  operand2 = operands[2];
  emit_insn (gen_rtx_SET (VOIDmode,
	gen_rtx_SUBREG (TImode,
	operand0,
	0),
	gen_rtx_AND (TImode,
	gen_rtx_NOT (TImode,
	gen_rtx_SUBREG (TImode,
	operand2,
	0)),
	gen_rtx_SUBREG (TImode,
	operand1,
	0))));
  _val = get_insns ();
  end_sequence ();
  return _val;
}

/* ../../gcc/gcc/config/i386/i386.md:9955 */
extern rtx gen_split_1272 PARAMS ((rtx *));
rtx
gen_split_1272 (operands)
      rtx *operands;
{
  rtx operand0;
  rtx operand1;
  rtx _val = 0;
  start_sequence ();

  operand0 = operands[0];
  operand1 = operands[1];
  emit_insn (gen_rtx_SET (VOIDmode,
	operand0,
	gen_rtx_ABS (DFmode,
	operand1)));
  _val = get_insns ();
  end_sequence ();
  return _val;
}

/* ../../gcc/gcc/config/i386/i386.md:9964 */
extern rtx gen_split_1273 PARAMS ((rtx *));
rtx
gen_split_1273 (operands)
      rtx *operands;
{
  rtx operand0;
  rtx operand1;
  rtx operand2;
  rtx operand3;
  rtx operand4;
  rtx _val = 0;
  start_sequence ();
operands[4] = gen_int_mode (~0x80000000, SImode);
   split_di (operands+0, 1, operands+2, operands+3);
  operand0 = operands[0];
  operand1 = operands[1];
  operand2 = operands[2];
  operand3 = operands[3];
  operand4 = operands[4];
  emit (gen_rtx_PARALLEL (VOIDmode,
	gen_rtvec (2,
		gen_rtx_SET (VOIDmode,
	operand3,
	gen_rtx_AND (SImode,
	copy_rtx (operand3),
	operand4)),
		gen_rtx_CLOBBER (VOIDmode,
	gen_rtx_REG (CCmode,
	17)))));
  _val = get_insns ();
  end_sequence ();
  return _val;
}

/* ../../gcc/gcc/config/i386/i386.md:9974 */
rtx
gen_absxf2 (operand0, operand1)
     rtx operand0;
     rtx operand1;
{
  rtx _val = 0;
  start_sequence ();
  {
    rtx operands[2];
    operands[0] = operand0;
    operands[1] = operand1;
ix86_expand_unary_operator (ABS, XFmode, operands); DONE;
    operand0 = operands[0];
    operand1 = operands[1];
  }
  emit (gen_rtx_PARALLEL (VOIDmode,
	gen_rtvec (2,
		gen_rtx_SET (VOIDmode,
	operand0,
	gen_rtx_NEG (XFmode,
	operand1)),
		gen_rtx_CLOBBER (VOIDmode,
	gen_rtx_REG (CCmode,
	17)))));
  _val = get_insns ();
  end_sequence ();
  return _val;
}

/* ../../gcc/gcc/config/i386/i386.md:9981 */
rtx
gen_abstf2 (operand0, operand1)
     rtx operand0;
     rtx operand1;
{
  rtx _val = 0;
  start_sequence ();
  {
    rtx operands[2];
    operands[0] = operand0;
    operands[1] = operand1;
ix86_expand_unary_operator (ABS, TFmode, operands); DONE;
    operand0 = operands[0];
    operand1 = operands[1];
  }
  emit (gen_rtx_PARALLEL (VOIDmode,
	gen_rtvec (2,
		gen_rtx_SET (VOIDmode,
	operand0,
	gen_rtx_NEG (TFmode,
	operand1)),
		gen_rtx_CLOBBER (VOIDmode,
	gen_rtx_REG (CCmode,
	17)))));
  _val = get_insns ();
  end_sequence ();
  return _val;
}

/* ../../gcc/gcc/config/i386/i386.md:9999 */
extern rtx gen_split_1276 PARAMS ((rtx *));
rtx
gen_split_1276 (operands)
      rtx *operands;
{
  rtx operand0;
  rtx operand1;
  rtx _val = 0;
  start_sequence ();

  operand0 = operands[0];
  operand1 = operands[1];
  emit_insn (gen_rtx_SET (VOIDmode,
	operand0,
	gen_rtx_ABS (XFmode,
	operand1)));
  _val = get_insns ();
  end_sequence ();
  return _val;
}

/* ../../gcc/gcc/config/i386/i386.md:10008 */
extern rtx gen_split_1277 PARAMS ((rtx *));
rtx
gen_split_1277 (operands)
      rtx *operands;
{
  rtx operand0;
  rtx operand1;
  rtx _val = 0;
  start_sequence ();
operands[1] = GEN_INT (~0x8000);
   operands[0] = gen_rtx_REG (SImode,
			      true_regnum (operands[0]) + (TARGET_64BIT ? 1 : 2));
  operand0 = operands[0];
  operand1 = operands[1];
  emit (gen_rtx_PARALLEL (VOIDmode,
	gen_rtvec (2,
		gen_rtx_SET (VOIDmode,
	operand0,
	gen_rtx_AND (SImode,
	copy_rtx (operand0),
	operand1)),
		gen_rtx_CLOBBER (VOIDmode,
	gen_rtx_REG (CCmode,
	17)))));
  _val = get_insns ();
  end_sequence ();
  return _val;
}

/* ../../gcc/gcc/config/i386/i386.md:10026 */
extern rtx gen_split_1278 PARAMS ((rtx *));
rtx
gen_split_1278 (operands)
      rtx *operands;
{
  rtx operand0;
  rtx operand1;
  rtx _val = 0;
  start_sequence ();

  operand0 = operands[0];
  operand1 = operands[1];
  emit_insn (gen_rtx_SET (VOIDmode,
	operand0,
	gen_rtx_ABS (TFmode,
	operand1)));
  _val = get_insns ();
  end_sequence ();
  return _val;
}

/* ../../gcc/gcc/config/i386/i386.md:10035 */
extern rtx gen_split_1279 PARAMS ((rtx *));
rtx
gen_split_1279 (operands)
      rtx *operands;
{
  rtx operand0;
  rtx operand1;
  rtx _val = 0;
  start_sequence ();
operands[1] = GEN_INT (~0x8000);
   operands[0] = gen_rtx_REG (SImode,
			      true_regnum (operands[0]) + (TARGET_64BIT ? 1 : 2));
  operand0 = operands[0];
  operand1 = operands[1];
  emit (gen_rtx_PARALLEL (VOIDmode,
	gen_rtvec (2,
		gen_rtx_SET (VOIDmode,
	operand0,
	gen_rtx_AND (SImode,
	copy_rtx (operand0),
	operand1)),
		gen_rtx_CLOBBER (VOIDmode,
	gen_rtx_REG (CCmode,
	17)))));
  _val = get_insns ();
  end_sequence ();
  return _val;
}

/* ../../gcc/gcc/config/i386/i386.md:10125 */
rtx
gen_one_cmpldi2 (operand0, operand1)
     rtx operand0;
     rtx operand1;
{
  rtx _val = 0;
  start_sequence ();
  {
    rtx operands[2];
    operands[0] = operand0;
    operands[1] = operand1;
ix86_expand_unary_operator (NOT, DImode, operands); DONE;
    operand0 = operands[0];
    operand1 = operands[1];
  }
  emit_insn (gen_rtx_SET (VOIDmode,
	operand0,
	gen_rtx_NOT (DImode,
	operand1)));
  _val = get_insns ();
  end_sequence ();
  return _val;
}

/* ../../gcc/gcc/config/i386/i386.md:10151 */
extern rtx gen_split_1281 PARAMS ((rtx *));
rtx
gen_split_1281 (operands)
      rtx *operands;
{
  rtx operand0;
  rtx operand1;
  rtx _val = 0;
  start_sequence ();

  operand0 = operands[0];
  operand1 = operands[1];
  emit (gen_rtx_PARALLEL (VOIDmode,
	gen_rtvec (2,
		gen_rtx_SET (VOIDmode,
	gen_rtx_REG (CCNOmode,
	17),
	gen_rtx_COMPARE (CCNOmode,
	gen_rtx_XOR (DImode,
	operand1,
	constm1_rtx),
	const0_rtx)),
		gen_rtx_SET (VOIDmode,
	operand0,
	gen_rtx_XOR (DImode,
	copy_rtx (operand1),
	constm1_rtx)))));
  _val = get_insns ();
  end_sequence ();
  return _val;
}

/* ../../gcc/gcc/config/i386/i386.md:10165 */
rtx
gen_one_cmplsi2 (operand0, operand1)
     rtx operand0;
     rtx operand1;
{
  rtx _val = 0;
  start_sequence ();
  {
    rtx operands[2];
    operands[0] = operand0;
    operands[1] = operand1;
ix86_expand_unary_operator (NOT, SImode, operands); DONE;
    operand0 = operands[0];
    operand1 = operands[1];
  }
  emit_insn (gen_rtx_SET (VOIDmode,
	operand0,
	gen_rtx_NOT (SImode,
	operand1)));
  _val = get_insns ();
  end_sequence ();
  return _val;
}

/* ../../gcc/gcc/config/i386/i386.md:10200 */
extern rtx gen_split_1283 PARAMS ((rtx *));
rtx
gen_split_1283 (operands)
      rtx *operands;
{
  rtx operand0;
  rtx operand1;
  rtx _val = 0;
  start_sequence ();

  operand0 = operands[0];
  operand1 = operands[1];
  emit (gen_rtx_PARALLEL (VOIDmode,
	gen_rtvec (2,
		gen_rtx_SET (VOIDmode,
	gen_rtx_REG (CCNOmode,
	17),
	gen_rtx_COMPARE (CCNOmode,
	gen_rtx_XOR (SImode,
	operand1,
	constm1_rtx),
	const0_rtx)),
		gen_rtx_SET (VOIDmode,
	operand0,
	gen_rtx_XOR (SImode,
	copy_rtx (operand1),
	constm1_rtx)))));
  _val = get_insns ();
  end_sequence ();
  return _val;
}

/* ../../gcc/gcc/config/i386/i386.md:10227 */
extern rtx gen_split_1284 PARAMS ((rtx *));
rtx
gen_split_1284 (operands)
      rtx *operands;
{
  rtx operand0;
  rtx operand1;
  rtx _val = 0;
  start_sequence ();

  operand0 = operands[0];
  operand1 = operands[1];
  emit (gen_rtx_PARALLEL (VOIDmode,
	gen_rtvec (2,
		gen_rtx_SET (VOIDmode,
	gen_rtx_REG (CCNOmode,
	17),
	gen_rtx_COMPARE (CCNOmode,
	gen_rtx_XOR (SImode,
	operand1,
	constm1_rtx),
	const0_rtx)),
		gen_rtx_SET (VOIDmode,
	operand0,
	gen_rtx_ZERO_EXTEND (DImode,
	gen_rtx_XOR (SImode,
	copy_rtx (operand1),
	constm1_rtx))))));
  _val = get_insns ();
  end_sequence ();
  return _val;
}

/* ../../gcc/gcc/config/i386/i386.md:10241 */
rtx
gen_one_cmplhi2 (operand0, operand1)
     rtx operand0;
     rtx operand1;
{
  rtx _val = 0;
  start_sequence ();
  {
    rtx operands[2];
    operands[0] = operand0;
    operands[1] = operand1;
ix86_expand_unary_operator (NOT, HImode, operands); DONE;
    operand0 = operands[0];
    operand1 = operands[1];
  }
  emit_insn (gen_rtx_SET (VOIDmode,
	operand0,
	gen_rtx_NOT (HImode,
	operand1)));
  _val = get_insns ();
  end_sequence ();
  return _val;
}

/* ../../gcc/gcc/config/i386/i386.md:10267 */
extern rtx gen_split_1286 PARAMS ((rtx *));
rtx
gen_split_1286 (operands)
      rtx *operands;
{
  rtx operand0;
  rtx operand1;
  rtx _val = 0;
  start_sequence ();

  operand0 = operands[0];
  operand1 = operands[1];
  emit (gen_rtx_PARALLEL (VOIDmode,
	gen_rtvec (2,
		gen_rtx_SET (VOIDmode,
	gen_rtx_REG (CCNOmode,
	17),
	gen_rtx_COMPARE (CCNOmode,
	gen_rtx_XOR (HImode,
	operand1,
	constm1_rtx),
	const0_rtx)),
		gen_rtx_SET (VOIDmode,
	operand0,
	gen_rtx_XOR (HImode,
	copy_rtx (operand1),
	constm1_rtx)))));
  _val = get_insns ();
  end_sequence ();
  return _val;
}

/* ../../gcc/gcc/config/i386/i386.md:10282 */
rtx
gen_one_cmplqi2 (operand0, operand1)
     rtx operand0;
     rtx operand1;
{
  rtx _val = 0;
  start_sequence ();
  {
    rtx operands[2];
    operands[0] = operand0;
    operands[1] = operand1;
ix86_expand_unary_operator (NOT, QImode, operands); DONE;
    operand0 = operands[0];
    operand1 = operands[1];
  }
  emit_insn (gen_rtx_SET (VOIDmode,
	operand0,
	gen_rtx_NOT (QImode,
	operand1)));
  _val = get_insns ();
  end_sequence ();
  return _val;
}

/* ../../gcc/gcc/config/i386/i386.md:10310 */
extern rtx gen_split_1288 PARAMS ((rtx *));
rtx
gen_split_1288 (operands)
      rtx *operands;
{
  rtx operand0;
  rtx operand1;
  rtx _val = 0;
  start_sequence ();

  operand0 = operands[0];
  operand1 = operands[1];
  emit (gen_rtx_PARALLEL (VOIDmode,
	gen_rtvec (2,
		gen_rtx_SET (VOIDmode,
	gen_rtx_REG (CCNOmode,
	17),
	gen_rtx_COMPARE (CCNOmode,
	gen_rtx_XOR (QImode,
	operand1,
	constm1_rtx),
	const0_rtx)),
		gen_rtx_SET (VOIDmode,
	operand0,
	gen_rtx_XOR (QImode,
	copy_rtx (operand1),
	constm1_rtx)))));
  _val = get_insns ();
  end_sequence ();
  return _val;
}

/* ../../gcc/gcc/config/i386/i386.md:10348 */
rtx
gen_ashldi3 (operand0, operand1, operand2)
     rtx operand0;
     rtx operand1;
     rtx operand2;
{
  rtx _val = 0;
  start_sequence ();
  {
    rtx operands[3];
    operands[0] = operand0;
    operands[1] = operand1;
    operands[2] = operand2;
{
  if (!TARGET_64BIT && TARGET_CMOVE && ! immediate_operand (operands[2], QImode))
    {
      emit_insn (gen_ashldi3_1 (operands[0], operands[1], operands[2]));
      DONE;
    }
  ix86_expand_binary_operator (ASHIFT, DImode, operands);
  DONE;
}
    operand0 = operands[0];
    operand1 = operands[1];
    operand2 = operands[2];
  }
  emit (gen_rtx_PARALLEL (VOIDmode,
	gen_rtvec (2,
		gen_rtx_SET (VOIDmode,
	operand0,
	gen_rtx_ASHIFT (DImode,
	operand1,
	operand2)),
		gen_rtx_CLOBBER (VOIDmode,
	gen_rtx_REG (CCmode,
	17)))));
  _val = get_insns ();
  end_sequence ();
  return _val;
}

/* ../../gcc/gcc/config/i386/i386.md:10412 */
extern rtx gen_split_1290 PARAMS ((rtx *));
rtx
gen_split_1290 (operands)
      rtx *operands;
{
  rtx operand0;
  rtx operand1;
  rtx operand2;
  rtx _val = 0;
  start_sequence ();
operands[2] = gen_int_mode (1 << INTVAL (operands[2]), DImode);
  operand0 = operands[0];
  operand1 = operands[1];
  operand2 = operands[2];
  emit_insn (gen_rtx_SET (VOIDmode,
	operand0,
	gen_rtx_MULT (DImode,
	operand1,
	operand2)));
  _val = get_insns ();
  end_sequence ();
  return _val;
}

/* ../../gcc/gcc/config/i386/i386.md:10485 */
extern rtx gen_split_1291 PARAMS ((rtx *));
rtx
gen_split_1291 (operands)
      rtx *operands ATTRIBUTE_UNUSED;
{
  rtx _val = 0;
  start_sequence ();
ix86_split_ashldi (operands, operands[3]); DONE;
  emit_insn (const0_rtx);
  _val = get_insns ();
  end_sequence ();
  return _val;
}

/* ../../gcc/gcc/config/i386/i386.md:10495 */
extern rtx gen_split_1292 PARAMS ((rtx *));
rtx
gen_split_1292 (operands)
      rtx *operands ATTRIBUTE_UNUSED;
{
  rtx _val = 0;
  start_sequence ();
ix86_split_ashldi (operands, NULL_RTX); DONE;
  emit_insn (const0_rtx);
  _val = get_insns ();
  end_sequence ();
  return _val;
}

/* ../../gcc/gcc/config/i386/i386.md:10522 */
rtx
gen_x86_shift_adj_1 (operand0, operand1, operand2, operand3)
     rtx operand0;
     rtx operand1;
     rtx operand2;
     rtx operand3;
{
  rtx _val = 0;
  start_sequence ();
  emit_insn (gen_rtx_SET (VOIDmode,
	gen_rtx_REG (CCZmode,
	17),
	gen_rtx_COMPARE (CCZmode,
	gen_rtx_AND (QImode,
	operand2,
	GEN_INT (32LL)),
	const0_rtx)));
  emit_insn (gen_rtx_SET (VOIDmode,
	operand0,
	gen_rtx_IF_THEN_ELSE (SImode,
	gen_rtx_NE (VOIDmode,
	gen_rtx_REG (CCZmode,
	17),
	const0_rtx),
	operand1,
	operand0)));
  emit_insn (gen_rtx_SET (VOIDmode,
	operand1,
	gen_rtx_IF_THEN_ELSE (SImode,
	gen_rtx_NE (VOIDmode,
	gen_rtx_REG (CCZmode,
	17),
	const0_rtx),
	operand3,
	operand1)));
  _val = get_insns ();
  end_sequence ();
  return _val;
}

/* ../../gcc/gcc/config/i386/i386.md:10538 */
rtx
gen_x86_shift_adj_2 (operand0, operand1, operand2)
     rtx operand0;
     rtx operand1;
     rtx operand2;
{
  rtx _val = 0;
  start_sequence ();
  {
    rtx operands[3];
    operands[0] = operand0;
    operands[1] = operand1;
    operands[2] = operand2;
{
  rtx label = gen_label_rtx ();
  rtx tmp;

  emit_insn (gen_testqi_ccz_1 (operands[2], GEN_INT (32)));

  tmp = gen_rtx_REG (CCZmode, FLAGS_REG);
  tmp = gen_rtx_EQ (VOIDmode, tmp, const0_rtx);
  tmp = gen_rtx_IF_THEN_ELSE (VOIDmode, tmp,
			      gen_rtx_LABEL_REF (VOIDmode, label),
			      pc_rtx);
  tmp = emit_jump_insn (gen_rtx_SET (VOIDmode, pc_rtx, tmp));
  JUMP_LABEL (tmp) = label;

  emit_move_insn (operands[0], operands[1]);
  emit_move_insn (operands[1], const0_rtx);

  emit_label (label);
  LABEL_NUSES (label) = 1;

  DONE;
}
    operand0 = operands[0];
    operand1 = operands[1];
    operand2 = operands[2];
  }
  emit_insn (gen_rtx_USE (VOIDmode,
	operand0));
  emit_insn (gen_rtx_USE (VOIDmode,
	operand1));
  emit_insn (gen_rtx_USE (VOIDmode,
	operand2));
  _val = get_insns ();
  end_sequence ();
  return _val;
}

/* ../../gcc/gcc/config/i386/i386.md:10566 */
rtx
gen_ashlsi3 (operand0, operand1, operand2)
     rtx operand0;
     rtx operand1;
     rtx operand2;
{
  rtx _val = 0;
  start_sequence ();
  {
    rtx operands[3];
    operands[0] = operand0;
    operands[1] = operand1;
    operands[2] = operand2;
ix86_expand_binary_operator (ASHIFT, SImode, operands); DONE;
    operand0 = operands[0];
    operand1 = operands[1];
    operand2 = operands[2];
  }
  emit_insn (gen_rtx_SET (VOIDmode,
	operand0,
	gen_rtx_ASHIFT (SImode,
	operand1,
	operand2)));
  emit_insn (gen_rtx_CLOBBER (VOIDmode,
	gen_rtx_REG (CCmode,
	17)));
  _val = get_insns ();
  end_sequence ();
  return _val;
}

/* ../../gcc/gcc/config/i386/i386.md:10617 */
extern rtx gen_split_1296 PARAMS ((rtx *));
rtx
gen_split_1296 (operands)
      rtx *operands ATTRIBUTE_UNUSED;
{
  rtx _val = 0;
  start_sequence ();
{
  rtx pat;
  operands[0] = gen_lowpart (SImode, operands[0]);
  operands[1] = gen_lowpart (Pmode, operands[1]);
  operands[2] = gen_int_mode (1 << INTVAL (operands[2]), Pmode);
  pat = gen_rtx_MULT (Pmode, operands[1], operands[2]);
  if (Pmode != SImode)
    pat = gen_rtx_SUBREG (SImode, pat, 0);
  emit_insn (gen_rtx_SET (VOIDmode, operands[0], pat));
  DONE;
}
  emit_insn (const0_rtx);
  _val = get_insns ();
  end_sequence ();
  return _val;
}

/* ../../gcc/gcc/config/i386/i386.md:10638 */
extern rtx gen_split_1297 PARAMS ((rtx *));
rtx
gen_split_1297 (operands)
      rtx *operands ATTRIBUTE_UNUSED;
{
  rtx _val = 0;
  start_sequence ();
{
  rtx pat, clob;
  emit_move_insn (operands[1], operands[0]);
  pat = gen_rtx_SET (VOIDmode, operands[0],
		     gen_rtx_ASHIFT (GET_MODE (operands[0]),
				     operands[0], operands[2]));
  clob = gen_rtx_CLOBBER (VOIDmode, gen_rtx_REG (CCmode, FLAGS_REG));
  emit_insn (gen_rtx_PARALLEL (VOIDmode, gen_rtvec (2, pat, clob)));
  DONE;
}
  emit_insn (const0_rtx);
  _val = get_insns ();
  end_sequence ();
  return _val;
}

/* ../../gcc/gcc/config/i386/i386.md:10697 */
extern rtx gen_split_1298 PARAMS ((rtx *));
rtx
gen_split_1298 (operands)
      rtx *operands;
{
  rtx operand0;
  rtx operand1;
  rtx operand2;
  rtx _val = 0;
  start_sequence ();
{
  operands[1] = gen_lowpart (Pmode, operands[1]);
  operands[2] = gen_int_mode (1 << INTVAL (operands[2]), Pmode);
}
  operand0 = operands[0];
  operand1 = operands[1];
  operand2 = operands[2];
  emit_insn (gen_rtx_SET (VOIDmode,
	operand0,
	gen_rtx_ZERO_EXTEND (DImode,
	gen_rtx_SUBREG (SImode,
	gen_rtx_MULT (SImode,
	operand1,
	operand2),
	0))));
  _val = get_insns ();
  end_sequence ();
  return _val;
}

/* ../../gcc/gcc/config/i386/i386.md:10790 */
rtx
gen_ashlhi3 (operand0, operand1, operand2)
     rtx operand0;
     rtx operand1;
     rtx operand2;
{
  rtx _val = 0;
  start_sequence ();
  {
    rtx operands[3];
    operands[0] = operand0;
    operands[1] = operand1;
    operands[2] = operand2;
ix86_expand_binary_operator (ASHIFT, HImode, operands); DONE;
    operand0 = operands[0];
    operand1 = operands[1];
    operand2 = operands[2];
  }
  emit_insn (gen_rtx_SET (VOIDmode,
	operand0,
	gen_rtx_ASHIFT (HImode,
	operand1,
	operand2)));
  emit_insn (gen_rtx_CLOBBER (VOIDmode,
	gen_rtx_REG (CCmode,
	17)));
  _val = get_insns ();
  end_sequence ();
  return _val;
}

/* ../../gcc/gcc/config/i386/i386.md:10916 */
rtx
gen_ashlqi3 (operand0, operand1, operand2)
     rtx operand0;
     rtx operand1;
     rtx operand2;
{
  rtx _val = 0;
  start_sequence ();
  {
    rtx operands[3];
    operands[0] = operand0;
    operands[1] = operand1;
    operands[2] = operand2;
ix86_expand_binary_operator (ASHIFT, QImode, operands); DONE;
    operand0 = operands[0];
    operand1 = operands[1];
    operand2 = operands[2];
  }
  emit_insn (gen_rtx_SET (VOIDmode,
	operand0,
	gen_rtx_ASHIFT (QImode,
	operand1,
	operand2)));
  emit_insn (gen_rtx_CLOBBER (VOIDmode,
	gen_rtx_REG (CCmode,
	17)));
  _val = get_insns ();
  end_sequence ();
  return _val;
}

/* ../../gcc/gcc/config/i386/i386.md:11082 */
rtx
gen_ashrdi3 (operand0, operand1, operand2)
     rtx operand0;
     rtx operand1;
     rtx operand2;
{
  rtx _val = 0;
  start_sequence ();
  {
    rtx operands[3];
    operands[0] = operand0;
    operands[1] = operand1;
    operands[2] = operand2;
{
  if (!TARGET_64BIT && TARGET_CMOVE && ! immediate_operand (operands[2], QImode))
    {
      emit_insn (gen_ashrdi3_1 (operands[0], operands[1], operands[2]));
      DONE;
    }
  ix86_expand_binary_operator (ASHIFTRT, DImode, operands);
  DONE;
}
    operand0 = operands[0];
    operand1 = operands[1];
    operand2 = operands[2];
  }
  emit (gen_rtx_PARALLEL (VOIDmode,
	gen_rtvec (2,
		gen_rtx_SET (VOIDmode,
	operand0,
	gen_rtx_ASHIFTRT (DImode,
	operand1,
	operand2)),
		gen_rtx_CLOBBER (VOIDmode,
	gen_rtx_REG (CCmode,
	17)))));
  _val = get_insns ();
  end_sequence ();
  return _val;
}

/* ../../gcc/gcc/config/i386/i386.md:11198 */
extern rtx gen_split_1302 PARAMS ((rtx *));
rtx
gen_split_1302 (operands)
      rtx *operands ATTRIBUTE_UNUSED;
{
  rtx _val = 0;
  start_sequence ();
ix86_split_ashrdi (operands, operands[3]); DONE;
  emit_insn (const0_rtx);
  _val = get_insns ();
  end_sequence ();
  return _val;
}

/* ../../gcc/gcc/config/i386/i386.md:11208 */
extern rtx gen_split_1303 PARAMS ((rtx *));
rtx
gen_split_1303 (operands)
      rtx *operands ATTRIBUTE_UNUSED;
{
  rtx _val = 0;
  start_sequence ();
ix86_split_ashrdi (operands, NULL_RTX); DONE;
  emit_insn (const0_rtx);
  _val = get_insns ();
  end_sequence ();
  return _val;
}

/* ../../gcc/gcc/config/i386/i386.md:11234 */
rtx
gen_x86_shift_adj_3 (operand0, operand1, operand2)
     rtx operand0;
     rtx operand1;
     rtx operand2;
{
  rtx _val = 0;
  start_sequence ();
  {
    rtx operands[3];
    operands[0] = operand0;
    operands[1] = operand1;
    operands[2] = operand2;
{
  rtx label = gen_label_rtx ();
  rtx tmp;

  emit_insn (gen_testqi_ccz_1 (operands[2], GEN_INT (32)));

  tmp = gen_rtx_REG (CCZmode, FLAGS_REG);
  tmp = gen_rtx_EQ (VOIDmode, tmp, const0_rtx);
  tmp = gen_rtx_IF_THEN_ELSE (VOIDmode, tmp,
			      gen_rtx_LABEL_REF (VOIDmode, label),
			      pc_rtx);
  tmp = emit_jump_insn (gen_rtx_SET (VOIDmode, pc_rtx, tmp));
  JUMP_LABEL (tmp) = label;

  emit_move_insn (operands[0], operands[1]);
  emit_insn (gen_ashrsi3_31 (operands[1], operands[1], GEN_INT (31)));

  emit_label (label);
  LABEL_NUSES (label) = 1;

  DONE;
}
    operand0 = operands[0];
    operand1 = operands[1];
    operand2 = operands[2];
  }
  emit_insn (gen_rtx_USE (VOIDmode,
	operand0));
  emit_insn (gen_rtx_USE (VOIDmode,
	operand1));
  emit_insn (gen_rtx_USE (VOIDmode,
	operand2));
  _val = get_insns ();
  end_sequence ();
  return _val;
}

/* ../../gcc/gcc/config/i386/i386.md:11295 */
rtx
gen_ashrsi3 (operand0, operand1, operand2)
     rtx operand0;
     rtx operand1;
     rtx operand2;
{
  rtx _val = 0;
  start_sequence ();
  {
    rtx operands[3];
    operands[0] = operand0;
    operands[1] = operand1;
    operands[2] = operand2;
ix86_expand_binary_operator (ASHIFTRT, SImode, operands); DONE;
    operand0 = operands[0];
    operand1 = operands[1];
    operand2 = operands[2];
  }
  emit_insn (gen_rtx_SET (VOIDmode,
	operand0,
	gen_rtx_ASHIFTRT (SImode,
	operand1,
	operand2)));
  emit_insn (gen_rtx_CLOBBER (VOIDmode,
	gen_rtx_REG (CCmode,
	17)));
  _val = get_insns ();
  end_sequence ();
  return _val;
}

/* ../../gcc/gcc/config/i386/i386.md:11419 */
rtx
gen_ashrhi3 (operand0, operand1, operand2)
     rtx operand0;
     rtx operand1;
     rtx operand2;
{
  rtx _val = 0;
  start_sequence ();
  {
    rtx operands[3];
    operands[0] = operand0;
    operands[1] = operand1;
    operands[2] = operand2;
ix86_expand_binary_operator (ASHIFTRT, HImode, operands); DONE;
    operand0 = operands[0];
    operand1 = operands[1];
    operand2 = operands[2];
  }
  emit_insn (gen_rtx_SET (VOIDmode,
	operand0,
	gen_rtx_ASHIFTRT (HImode,
	operand1,
	operand2)));
  emit_insn (gen_rtx_CLOBBER (VOIDmode,
	gen_rtx_REG (CCmode,
	17)));
  _val = get_insns ();
  end_sequence ();
  return _val;
}

/* ../../gcc/gcc/config/i386/i386.md:11491 */
rtx
gen_ashrqi3 (operand0, operand1, operand2)
     rtx operand0;
     rtx operand1;
     rtx operand2;
{
  rtx _val = 0;
  start_sequence ();
  {
    rtx operands[3];
    operands[0] = operand0;
    operands[1] = operand1;
    operands[2] = operand2;
ix86_expand_binary_operator (ASHIFTRT, QImode, operands); DONE;
    operand0 = operands[0];
    operand1 = operands[1];
    operand2 = operands[2];
  }
  emit_insn (gen_rtx_SET (VOIDmode,
	operand0,
	gen_rtx_ASHIFTRT (QImode,
	operand1,
	operand2)));
  emit_insn (gen_rtx_CLOBBER (VOIDmode,
	gen_rtx_REG (CCmode,
	17)));
  _val = get_insns ();
  end_sequence ();
  return _val;
}

/* ../../gcc/gcc/config/i386/i386.md:11595 */
rtx
gen_lshrdi3 (operand0, operand1, operand2)
     rtx operand0;
     rtx operand1;
     rtx operand2;
{
  rtx _val = 0;
  start_sequence ();
  {
    rtx operands[3];
    operands[0] = operand0;
    operands[1] = operand1;
    operands[2] = operand2;
{
  if (!TARGET_64BIT && TARGET_CMOVE && ! immediate_operand (operands[2], QImode))
    {
      emit_insn (gen_lshrdi3_1 (operands[0], operands[1], operands[2]));
      DONE;
    }
  ix86_expand_binary_operator (LSHIFTRT, DImode, operands);
  DONE;
}
    operand0 = operands[0];
    operand1 = operands[1];
    operand2 = operands[2];
  }
  emit (gen_rtx_PARALLEL (VOIDmode,
	gen_rtvec (2,
		gen_rtx_SET (VOIDmode,
	operand0,
	gen_rtx_LSHIFTRT (DImode,
	operand1,
	operand2)),
		gen_rtx_CLOBBER (VOIDmode,
	gen_rtx_REG (CCmode,
	17)))));
  _val = get_insns ();
  end_sequence ();
  return _val;
}

/* ../../gcc/gcc/config/i386/i386.md:11694 */
extern rtx gen_split_1309 PARAMS ((rtx *));
rtx
gen_split_1309 (operands)
      rtx *operands ATTRIBUTE_UNUSED;
{
  rtx _val = 0;
  start_sequence ();
ix86_split_lshrdi (operands, operands[3]); DONE;
  emit_insn (const0_rtx);
  _val = get_insns ();
  end_sequence ();
  return _val;
}

/* ../../gcc/gcc/config/i386/i386.md:11704 */
extern rtx gen_split_1310 PARAMS ((rtx *));
rtx
gen_split_1310 (operands)
      rtx *operands ATTRIBUTE_UNUSED;
{
  rtx _val = 0;
  start_sequence ();
ix86_split_lshrdi (operands, NULL_RTX); DONE;
  emit_insn (const0_rtx);
  _val = get_insns ();
  end_sequence ();
  return _val;
}

/* ../../gcc/gcc/config/i386/i386.md:11713 */
rtx
gen_lshrsi3 (operand0, operand1, operand2)
     rtx operand0;
     rtx operand1;
     rtx operand2;
{
  rtx _val = 0;
  start_sequence ();
  {
    rtx operands[3];
    operands[0] = operand0;
    operands[1] = operand1;
    operands[2] = operand2;
ix86_expand_binary_operator (LSHIFTRT, SImode, operands); DONE;
    operand0 = operands[0];
    operand1 = operands[1];
    operand2 = operands[2];
  }
  emit_insn (gen_rtx_SET (VOIDmode,
	operand0,
	gen_rtx_LSHIFTRT (SImode,
	operand1,
	operand2)));
  emit_insn (gen_rtx_CLOBBER (VOIDmode,
	gen_rtx_REG (CCmode,
	17)));
  _val = get_insns ();
  end_sequence ();
  return _val;
}

/* ../../gcc/gcc/config/i386/i386.md:11838 */
rtx
gen_lshrhi3 (operand0, operand1, operand2)
     rtx operand0;
     rtx operand1;
     rtx operand2;
{
  rtx _val = 0;
  start_sequence ();
  {
    rtx operands[3];
    operands[0] = operand0;
    operands[1] = operand1;
    operands[2] = operand2;
ix86_expand_binary_operator (LSHIFTRT, HImode, operands); DONE;
    operand0 = operands[0];
    operand1 = operands[1];
    operand2 = operands[2];
  }
  emit_insn (gen_rtx_SET (VOIDmode,
	operand0,
	gen_rtx_LSHIFTRT (HImode,
	operand1,
	operand2)));
  emit_insn (gen_rtx_CLOBBER (VOIDmode,
	gen_rtx_REG (CCmode,
	17)));
  _val = get_insns ();
  end_sequence ();
  return _val;
}

/* ../../gcc/gcc/config/i386/i386.md:11910 */
rtx
gen_lshrqi3 (operand0, operand1, operand2)
     rtx operand0;
     rtx operand1;
     rtx operand2;
{
  rtx _val = 0;
  start_sequence ();
  {
    rtx operands[3];
    operands[0] = operand0;
    operands[1] = operand1;
    operands[2] = operand2;
ix86_expand_binary_operator (LSHIFTRT, QImode, operands); DONE;
    operand0 = operands[0];
    operand1 = operands[1];
    operand2 = operands[2];
  }
  emit_insn (gen_rtx_SET (VOIDmode,
	operand0,
	gen_rtx_LSHIFTRT (QImode,
	operand1,
	operand2)));
  emit_insn (gen_rtx_CLOBBER (VOIDmode,
	gen_rtx_REG (CCmode,
	17)));
  _val = get_insns ();
  end_sequence ();
  return _val;
}

/* ../../gcc/gcc/config/i386/i386.md:12011 */
rtx
gen_rotldi3 (operand0, operand1, operand2)
     rtx operand0;
     rtx operand1;
     rtx operand2;
{
  rtx _val = 0;
  start_sequence ();
  {
    rtx operands[3];
    operands[0] = operand0;
    operands[1] = operand1;
    operands[2] = operand2;
ix86_expand_binary_operator (ROTATE, DImode, operands); DONE;
    operand0 = operands[0];
    operand1 = operands[1];
    operand2 = operands[2];
  }
  emit_insn (gen_rtx_SET (VOIDmode,
	operand0,
	gen_rtx_ROTATE (DImode,
	operand1,
	operand2)));
  emit_insn (gen_rtx_CLOBBER (VOIDmode,
	gen_rtx_REG (CCmode,
	17)));
  _val = get_insns ();
  end_sequence ();
  return _val;
}

/* ../../gcc/gcc/config/i386/i386.md:12045 */
rtx
gen_rotlsi3 (operand0, operand1, operand2)
     rtx operand0;
     rtx operand1;
     rtx operand2;
{
  rtx _val = 0;
  start_sequence ();
  {
    rtx operands[3];
    operands[0] = operand0;
    operands[1] = operand1;
    operands[2] = operand2;
ix86_expand_binary_operator (ROTATE, SImode, operands); DONE;
    operand0 = operands[0];
    operand1 = operands[1];
    operand2 = operands[2];
  }
  emit_insn (gen_rtx_SET (VOIDmode,
	operand0,
	gen_rtx_ROTATE (SImode,
	operand1,
	operand2)));
  emit_insn (gen_rtx_CLOBBER (VOIDmode,
	gen_rtx_REG (CCmode,
	17)));
  _val = get_insns ();
  end_sequence ();
  return _val;
}

/* ../../gcc/gcc/config/i386/i386.md:12104 */
rtx
gen_rotlhi3 (operand0, operand1, operand2)
     rtx operand0;
     rtx operand1;
     rtx operand2;
{
  rtx _val = 0;
  start_sequence ();
  {
    rtx operands[3];
    operands[0] = operand0;
    operands[1] = operand1;
    operands[2] = operand2;
ix86_expand_binary_operator (ROTATE, HImode, operands); DONE;
    operand0 = operands[0];
    operand1 = operands[1];
    operand2 = operands[2];
  }
  emit_insn (gen_rtx_SET (VOIDmode,
	operand0,
	gen_rtx_ROTATE (HImode,
	operand1,
	operand2)));
  emit_insn (gen_rtx_CLOBBER (VOIDmode,
	gen_rtx_REG (CCmode,
	17)));
  _val = get_insns ();
  end_sequence ();
  return _val;
}

/* ../../gcc/gcc/config/i386/i386.md:12138 */
rtx
gen_rotlqi3 (operand0, operand1, operand2)
     rtx operand0;
     rtx operand1;
     rtx operand2;
{
  rtx _val = 0;
  start_sequence ();
  {
    rtx operands[3];
    operands[0] = operand0;
    operands[1] = operand1;
    operands[2] = operand2;
ix86_expand_binary_operator (ROTATE, QImode, operands); DONE;
    operand0 = operands[0];
    operand1 = operands[1];
    operand2 = operands[2];
  }
  emit_insn (gen_rtx_SET (VOIDmode,
	operand0,
	gen_rtx_ROTATE (QImode,
	operand1,
	operand2)));
  emit_insn (gen_rtx_CLOBBER (VOIDmode,
	gen_rtx_REG (CCmode,
	17)));
  _val = get_insns ();
  end_sequence ();
  return _val;
}

/* ../../gcc/gcc/config/i386/i386.md:12199 */
rtx
gen_rotrdi3 (operand0, operand1, operand2)
     rtx operand0;
     rtx operand1;
     rtx operand2;
{
  rtx _val = 0;
  start_sequence ();
  {
    rtx operands[3];
    operands[0] = operand0;
    operands[1] = operand1;
    operands[2] = operand2;
ix86_expand_binary_operator (ROTATERT, DImode, operands); DONE;
    operand0 = operands[0];
    operand1 = operands[1];
    operand2 = operands[2];
  }
  emit_insn (gen_rtx_SET (VOIDmode,
	operand0,
	gen_rtx_ROTATERT (DImode,
	operand1,
	operand2)));
  emit_insn (gen_rtx_CLOBBER (VOIDmode,
	gen_rtx_REG (CCmode,
	17)));
  _val = get_insns ();
  end_sequence ();
  return _val;
}

/* ../../gcc/gcc/config/i386/i386.md:12233 */
rtx
gen_rotrsi3 (operand0, operand1, operand2)
     rtx operand0;
     rtx operand1;
     rtx operand2;
{
  rtx _val = 0;
  start_sequence ();
  {
    rtx operands[3];
    operands[0] = operand0;
    operands[1] = operand1;
    operands[2] = operand2;
ix86_expand_binary_operator (ROTATERT, SImode, operands); DONE;
    operand0 = operands[0];
    operand1 = operands[1];
    operand2 = operands[2];
  }
  emit_insn (gen_rtx_SET (VOIDmode,
	operand0,
	gen_rtx_ROTATERT (SImode,
	operand1,
	operand2)));
  emit_insn (gen_rtx_CLOBBER (VOIDmode,
	gen_rtx_REG (CCmode,
	17)));
  _val = get_insns ();
  end_sequence ();
  return _val;
}

/* ../../gcc/gcc/config/i386/i386.md:12295 */
rtx
gen_rotrhi3 (operand0, operand1, operand2)
     rtx operand0;
     rtx operand1;
     rtx operand2;
{
  rtx _val = 0;
  start_sequence ();
  {
    rtx operands[3];
    operands[0] = operand0;
    operands[1] = operand1;
    operands[2] = operand2;
ix86_expand_binary_operator (ROTATERT, HImode, operands); DONE;
    operand0 = operands[0];
    operand1 = operands[1];
    operand2 = operands[2];
  }
  emit_insn (gen_rtx_SET (VOIDmode,
	operand0,
	gen_rtx_ROTATERT (HImode,
	operand1,
	operand2)));
  emit_insn (gen_rtx_CLOBBER (VOIDmode,
	gen_rtx_REG (CCmode,
	17)));
  _val = get_insns ();
  end_sequence ();
  return _val;
}

/* ../../gcc/gcc/config/i386/i386.md:12329 */
rtx
gen_rotrqi3 (operand0, operand1, operand2)
     rtx operand0;
     rtx operand1;
     rtx operand2;
{
  rtx _val = 0;
  start_sequence ();
  {
    rtx operands[3];
    operands[0] = operand0;
    operands[1] = operand1;
    operands[2] = operand2;
ix86_expand_binary_operator (ROTATERT, QImode, operands); DONE;
    operand0 = operands[0];
    operand1 = operands[1];
    operand2 = operands[2];
  }
  emit_insn (gen_rtx_SET (VOIDmode,
	operand0,
	gen_rtx_ROTATERT (QImode,
	operand1,
	operand2)));
  emit_insn (gen_rtx_CLOBBER (VOIDmode,
	gen_rtx_REG (CCmode,
	17)));
  _val = get_insns ();
  end_sequence ();
  return _val;
}

/* ../../gcc/gcc/config/i386/i386.md:12392 */
rtx
gen_extv (operand0, operand1, operand2, operand3)
     rtx operand0;
     rtx operand1;
     rtx operand2;
     rtx operand3;
{
  rtx _val = 0;
  start_sequence ();
  {
    rtx operands[4];
    operands[0] = operand0;
    operands[1] = operand1;
    operands[2] = operand2;
    operands[3] = operand3;
{
  /* Handle extractions from %ah et al.  */
  if (INTVAL (operands[2]) != 8 || INTVAL (operands[3]) != 8)
    FAIL;

  /* From mips.md: extract_bit_field doesn't verify that our source
     matches the predicate, so check it again here.  */
  if (! register_operand (operands[1], VOIDmode))
    FAIL;
}
    operand0 = operands[0];
    operand1 = operands[1];
    operand2 = operands[2];
    operand3 = operands[3];
  }
  emit_insn (gen_rtx_SET (VOIDmode,
	operand0,
	gen_rtx_SIGN_EXTRACT (SImode,
	operand1,
	operand2,
	operand3)));
  _val = get_insns ();
  end_sequence ();
  return _val;
}

/* ../../gcc/gcc/config/i386/i386.md:12409 */
rtx
gen_extzv (operand0, operand1, operand2, operand3)
     rtx operand0;
     rtx operand1;
     rtx operand2;
     rtx operand3;
{
  rtx _val = 0;
  start_sequence ();
  {
    rtx operands[4];
    operands[0] = operand0;
    operands[1] = operand1;
    operands[2] = operand2;
    operands[3] = operand3;
{
  /* Handle extractions from %ah et al.  */
  if (INTVAL (operands[2]) != 8 || INTVAL (operands[3]) != 8)
    FAIL;

  /* From mips.md: extract_bit_field doesn't verify that our source
     matches the predicate, so check it again here.  */
  if (! register_operand (operands[1], VOIDmode))
    FAIL;
}
    operand0 = operands[0];
    operand1 = operands[1];
    operand2 = operands[2];
    operand3 = operands[3];
  }
  emit_insn (gen_rtx_SET (VOIDmode,
	operand0,
	gen_rtx_ZERO_EXTRACT (SImode,
	operand1,
	operand2,
	operand3)));
  _val = get_insns ();
  end_sequence ();
  return _val;
}

/* ../../gcc/gcc/config/i386/i386.md:12426 */
rtx
gen_insv (operand0, operand1, operand2, operand3)
     rtx operand0;
     rtx operand1;
     rtx operand2;
     rtx operand3;
{
  rtx _val = 0;
  start_sequence ();
  {
    rtx operands[4];
    operands[0] = operand0;
    operands[1] = operand1;
    operands[2] = operand2;
    operands[3] = operand3;
{
  /* Handle extractions from %ah et al.  */
  if (INTVAL (operands[1]) != 8 || INTVAL (operands[2]) != 8)
    FAIL;

  /* From mips.md: insert_bit_field doesn't verify that our source
     matches the predicate, so check it again here.  */
  if (! register_operand (operands[0], VOIDmode))
    FAIL;
}
    operand0 = operands[0];
    operand1 = operands[1];
    operand2 = operands[2];
    operand3 = operands[3];
  }
  emit_insn (gen_rtx_SET (VOIDmode,
	gen_rtx_ZERO_EXTRACT (SImode,
	operand0,
	operand1,
	operand2),
	operand3));
  _val = get_insns ();
  end_sequence ();
  return _val;
}

/* ../../gcc/gcc/config/i386/i386.md:12454 */
rtx
gen_seq (operand0)
     rtx operand0;
{
  rtx _val = 0;
  start_sequence ();
  {
    rtx operands[1];
    operands[0] = operand0;
if (ix86_expand_setcc (EQ, operands[0])) DONE; else FAIL;
    operand0 = operands[0];
  }
  emit_insn (gen_rtx_SET (VOIDmode,
	operand0,
	gen_rtx_EQ (QImode,
	gen_rtx_REG (CCmode,
	17),
	const0_rtx)));
  _val = get_insns ();
  end_sequence ();
  return _val;
}

/* ../../gcc/gcc/config/i386/i386.md:12460 */
rtx
gen_sne (operand0)
     rtx operand0;
{
  rtx _val = 0;
  start_sequence ();
  {
    rtx operands[1];
    operands[0] = operand0;
if (ix86_expand_setcc (NE, operands[0])) DONE; else FAIL;
    operand0 = operands[0];
  }
  emit_insn (gen_rtx_SET (VOIDmode,
	operand0,
	gen_rtx_NE (QImode,
	gen_rtx_REG (CCmode,
	17),
	const0_rtx)));
  _val = get_insns ();
  end_sequence ();
  return _val;
}

/* ../../gcc/gcc/config/i386/i386.md:12466 */
rtx
gen_sgt (operand0)
     rtx operand0;
{
  rtx _val = 0;
  start_sequence ();
  {
    rtx operands[1];
    operands[0] = operand0;
if (ix86_expand_setcc (GT, operands[0])) DONE; else FAIL;
    operand0 = operands[0];
  }
  emit_insn (gen_rtx_SET (VOIDmode,
	operand0,
	gen_rtx_GT (QImode,
	gen_rtx_REG (CCmode,
	17),
	const0_rtx)));
  _val = get_insns ();
  end_sequence ();
  return _val;
}

/* ../../gcc/gcc/config/i386/i386.md:12472 */
rtx
gen_sgtu (operand0)
     rtx operand0;
{
  rtx _val = 0;
  start_sequence ();
  {
    rtx operands[1];
    operands[0] = operand0;
if (ix86_expand_setcc (GTU, operands[0])) DONE; else FAIL;
    operand0 = operands[0];
  }
  emit_insn (gen_rtx_SET (VOIDmode,
	operand0,
	gen_rtx_GTU (QImode,
	gen_rtx_REG (CCmode,
	17),
	const0_rtx)));
  _val = get_insns ();
  end_sequence ();
  return _val;
}

/* ../../gcc/gcc/config/i386/i386.md:12478 */
rtx
gen_slt (operand0)
     rtx operand0;
{
  rtx _val = 0;
  start_sequence ();
  {
    rtx operands[1];
    operands[0] = operand0;
if (ix86_expand_setcc (LT, operands[0])) DONE; else FAIL;
    operand0 = operands[0];
  }
  emit_insn (gen_rtx_SET (VOIDmode,
	operand0,
	gen_rtx_LT (QImode,
	gen_rtx_REG (CCmode,
	17),
	const0_rtx)));
  _val = get_insns ();
  end_sequence ();
  return _val;
}

/* ../../gcc/gcc/config/i386/i386.md:12484 */
rtx
gen_sltu (operand0)
     rtx operand0;
{
  rtx _val = 0;
  start_sequence ();
  {
    rtx operands[1];
    operands[0] = operand0;
if (ix86_expand_setcc (LTU, operands[0])) DONE; else FAIL;
    operand0 = operands[0];
  }
  emit_insn (gen_rtx_SET (VOIDmode,
	operand0,
	gen_rtx_LTU (QImode,
	gen_rtx_REG (CCmode,
	17),
	const0_rtx)));
  _val = get_insns ();
  end_sequence ();
  return _val;
}

/* ../../gcc/gcc/config/i386/i386.md:12490 */
rtx
gen_sge (operand0)
     rtx operand0;
{
  rtx _val = 0;
  start_sequence ();
  {
    rtx operands[1];
    operands[0] = operand0;
if (ix86_expand_setcc (GE, operands[0])) DONE; else FAIL;
    operand0 = operands[0];
  }
  emit_insn (gen_rtx_SET (VOIDmode,
	operand0,
	gen_rtx_GE (QImode,
	gen_rtx_REG (CCmode,
	17),
	const0_rtx)));
  _val = get_insns ();
  end_sequence ();
  return _val;
}

/* ../../gcc/gcc/config/i386/i386.md:12496 */
rtx
gen_sgeu (operand0)
     rtx operand0;
{
  rtx _val = 0;
  start_sequence ();
  {
    rtx operands[1];
    operands[0] = operand0;
if (ix86_expand_setcc (GEU, operands[0])) DONE; else FAIL;
    operand0 = operands[0];
  }
  emit_insn (gen_rtx_SET (VOIDmode,
	operand0,
	gen_rtx_GEU (QImode,
	gen_rtx_REG (CCmode,
	17),
	const0_rtx)));
  _val = get_insns ();
  end_sequence ();
  return _val;
}

/* ../../gcc/gcc/config/i386/i386.md:12502 */
rtx
gen_sle (operand0)
     rtx operand0;
{
  rtx _val = 0;
  start_sequence ();
  {
    rtx operands[1];
    operands[0] = operand0;
if (ix86_expand_setcc (LE, operands[0])) DONE; else FAIL;
    operand0 = operands[0];
  }
  emit_insn (gen_rtx_SET (VOIDmode,
	operand0,
	gen_rtx_LE (QImode,
	gen_rtx_REG (CCmode,
	17),
	const0_rtx)));
  _val = get_insns ();
  end_sequence ();
  return _val;
}

/* ../../gcc/gcc/config/i386/i386.md:12508 */
rtx
gen_sleu (operand0)
     rtx operand0;
{
  rtx _val = 0;
  start_sequence ();
  {
    rtx operands[1];
    operands[0] = operand0;
if (ix86_expand_setcc (LEU, operands[0])) DONE; else FAIL;
    operand0 = operands[0];
  }
  emit_insn (gen_rtx_SET (VOIDmode,
	operand0,
	gen_rtx_LEU (QImode,
	gen_rtx_REG (CCmode,
	17),
	const0_rtx)));
  _val = get_insns ();
  end_sequence ();
  return _val;
}

/* ../../gcc/gcc/config/i386/i386.md:12514 */
rtx
gen_sunordered (operand0)
     rtx operand0;
{
  rtx _val = 0;
  start_sequence ();
  {
    rtx operands[1];
    operands[0] = operand0;
if (ix86_expand_setcc (UNORDERED, operands[0])) DONE; else FAIL;
    operand0 = operands[0];
  }
  emit_insn (gen_rtx_SET (VOIDmode,
	operand0,
	gen_rtx_UNORDERED (QImode,
	gen_rtx_REG (CCmode,
	17),
	const0_rtx)));
  _val = get_insns ();
  end_sequence ();
  return _val;
}

/* ../../gcc/gcc/config/i386/i386.md:12520 */
rtx
gen_sordered (operand0)
     rtx operand0;
{
  rtx _val = 0;
  start_sequence ();
  {
    rtx operands[1];
    operands[0] = operand0;
if (ix86_expand_setcc (ORDERED, operands[0])) DONE; else FAIL;
    operand0 = operands[0];
  }
  emit_insn (gen_rtx_SET (VOIDmode,
	operand0,
	gen_rtx_ORDERED (QImode,
	gen_rtx_REG (CCmode,
	17),
	const0_rtx)));
  _val = get_insns ();
  end_sequence ();
  return _val;
}

/* ../../gcc/gcc/config/i386/i386.md:12526 */
rtx
gen_suneq (operand0)
     rtx operand0;
{
  rtx _val = 0;
  start_sequence ();
  {
    rtx operands[1];
    operands[0] = operand0;
if (ix86_expand_setcc (UNEQ, operands[0])) DONE; else FAIL;
    operand0 = operands[0];
  }
  emit_insn (gen_rtx_SET (VOIDmode,
	operand0,
	gen_rtx_UNEQ (QImode,
	gen_rtx_REG (CCmode,
	17),
	const0_rtx)));
  _val = get_insns ();
  end_sequence ();
  return _val;
}

/* ../../gcc/gcc/config/i386/i386.md:12532 */
rtx
gen_sunge (operand0)
     rtx operand0;
{
  rtx _val = 0;
  start_sequence ();
  {
    rtx operands[1];
    operands[0] = operand0;
if (ix86_expand_setcc (UNGE, operands[0])) DONE; else FAIL;
    operand0 = operands[0];
  }
  emit_insn (gen_rtx_SET (VOIDmode,
	operand0,
	gen_rtx_UNGE (QImode,
	gen_rtx_REG (CCmode,
	17),
	const0_rtx)));
  _val = get_insns ();
  end_sequence ();
  return _val;
}

/* ../../gcc/gcc/config/i386/i386.md:12538 */
rtx
gen_sungt (operand0)
     rtx operand0;
{
  rtx _val = 0;
  start_sequence ();
  {
    rtx operands[1];
    operands[0] = operand0;
if (ix86_expand_setcc (UNGT, operands[0])) DONE; else FAIL;
    operand0 = operands[0];
  }
  emit_insn (gen_rtx_SET (VOIDmode,
	operand0,
	gen_rtx_UNGT (QImode,
	gen_rtx_REG (CCmode,
	17),
	const0_rtx)));
  _val = get_insns ();
  end_sequence ();
  return _val;
}

/* ../../gcc/gcc/config/i386/i386.md:12544 */
rtx
gen_sunle (operand0)
     rtx operand0;
{
  rtx _val = 0;
  start_sequence ();
  {
    rtx operands[1];
    operands[0] = operand0;
if (ix86_expand_setcc (UNLE, operands[0])) DONE; else FAIL;
    operand0 = operands[0];
  }
  emit_insn (gen_rtx_SET (VOIDmode,
	operand0,
	gen_rtx_UNLE (QImode,
	gen_rtx_REG (CCmode,
	17),
	const0_rtx)));
  _val = get_insns ();
  end_sequence ();
  return _val;
}

/* ../../gcc/gcc/config/i386/i386.md:12550 */
rtx
gen_sunlt (operand0)
     rtx operand0;
{
  rtx _val = 0;
  start_sequence ();
  {
    rtx operands[1];
    operands[0] = operand0;
if (ix86_expand_setcc (UNLT, operands[0])) DONE; else FAIL;
    operand0 = operands[0];
  }
  emit_insn (gen_rtx_SET (VOIDmode,
	operand0,
	gen_rtx_UNLT (QImode,
	gen_rtx_REG (CCmode,
	17),
	const0_rtx)));
  _val = get_insns ();
  end_sequence ();
  return _val;
}

/* ../../gcc/gcc/config/i386/i386.md:12556 */
rtx
gen_sltgt (operand0)
     rtx operand0;
{
  rtx _val = 0;
  start_sequence ();
  {
    rtx operands[1];
    operands[0] = operand0;
if (ix86_expand_setcc (LTGT, operands[0])) DONE; else FAIL;
    operand0 = operands[0];
  }
  emit_insn (gen_rtx_SET (VOIDmode,
	operand0,
	gen_rtx_LTGT (QImode,
	gen_rtx_REG (CCmode,
	17),
	const0_rtx)));
  _val = get_insns ();
  end_sequence ();
  return _val;
}

/* ../../gcc/gcc/config/i386/i386.md:12588 */
extern rtx gen_split_1343 PARAMS ((rtx *));
rtx
gen_split_1343 (operands)
      rtx *operands;
{
  rtx operand0;
  rtx operand1;
  rtx _val = 0;
  start_sequence ();
{
  PUT_MODE (operands[1], QImode);
}
  operand0 = operands[0];
  operand1 = operands[1];
  emit_insn (gen_rtx_SET (VOIDmode,
	operand0,
	operand1));
  _val = get_insns ();
  end_sequence ();
  return _val;
}

/* ../../gcc/gcc/config/i386/i386.md:12599 */
extern rtx gen_split_1344 PARAMS ((rtx *));
rtx
gen_split_1344 (operands)
      rtx *operands;
{
  rtx operand0;
  rtx operand1;
  rtx _val = 0;
  start_sequence ();
{
  PUT_MODE (operands[1], QImode);
}
  operand0 = operands[0];
  operand1 = operands[1];
  emit_insn (gen_rtx_SET (VOIDmode,
	operand0,
	operand1));
  _val = get_insns ();
  end_sequence ();
  return _val;
}

/* ../../gcc/gcc/config/i386/i386.md:12610 */
extern rtx gen_split_1345 PARAMS ((rtx *));
rtx
gen_split_1345 (operands)
      rtx *operands;
{
  rtx operand0;
  rtx operand1;
  rtx _val = 0;
  start_sequence ();
{
  rtx new_op1 = copy_rtx (operands[1]);
  operands[1] = new_op1;
  PUT_MODE (new_op1, QImode);
  PUT_CODE (new_op1, REVERSE_CONDITION (GET_CODE (new_op1),
					GET_MODE (XEXP (new_op1, 0))));

  /* Make sure that (a) the CCmode we have for the flags is strong
     enough for the reversed compare or (b) we have a valid FP compare.  */
  if (! ix86_comparison_operator (new_op1, VOIDmode))
    FAIL;
}
  operand0 = operands[0];
  operand1 = operands[1];
  emit_insn (gen_rtx_SET (VOIDmode,
	operand0,
	operand1));
  _val = get_insns ();
  end_sequence ();
  return _val;
}

/* ../../gcc/gcc/config/i386/i386.md:12630 */
extern rtx gen_split_1346 PARAMS ((rtx *));
rtx
gen_split_1346 (operands)
      rtx *operands;
{
  rtx operand0;
  rtx operand1;
  rtx _val = 0;
  start_sequence ();
{
  rtx new_op1 = copy_rtx (operands[1]);
  operands[1] = new_op1;
  PUT_MODE (new_op1, QImode);
  PUT_CODE (new_op1, REVERSE_CONDITION (GET_CODE (new_op1),
					GET_MODE (XEXP (new_op1, 0))));

  /* Make sure that (a) the CCmode we have for the flags is strong
     enough for the reversed compare or (b) we have a valid FP compare.  */
  if (! ix86_comparison_operator (new_op1, VOIDmode))
    FAIL;
}
  operand0 = operands[0];
  operand1 = operands[1];
  emit_insn (gen_rtx_SET (VOIDmode,
	operand0,
	operand1));
  _val = get_insns ();
  end_sequence ();
  return _val;
}

/* ../../gcc/gcc/config/i386/i386.md:12684 */
rtx
gen_beq (operand0)
     rtx operand0;
{
  rtx operand1;
  rtx _val = 0;
  start_sequence ();
  {
    rtx operands[2];
    operands[0] = operand0;
ix86_expand_branch (EQ, operands[0]); DONE;
    operand0 = operands[0];
    operand1 = operands[1];
  }
  emit_jump_insn (gen_rtx_SET (VOIDmode,
	pc_rtx,
	gen_rtx_IF_THEN_ELSE (VOIDmode,
	operand1,
	gen_rtx_LABEL_REF (VOIDmode,
	operand0),
	pc_rtx)));
  _val = get_insns ();
  end_sequence ();
  return _val;
}

/* ../../gcc/gcc/config/i386/i386.md:12692 */
rtx
gen_bne (operand0)
     rtx operand0;
{
  rtx operand1;
  rtx _val = 0;
  start_sequence ();
  {
    rtx operands[2];
    operands[0] = operand0;
ix86_expand_branch (NE, operands[0]); DONE;
    operand0 = operands[0];
    operand1 = operands[1];
  }
  emit_jump_insn (gen_rtx_SET (VOIDmode,
	pc_rtx,
	gen_rtx_IF_THEN_ELSE (VOIDmode,
	operand1,
	gen_rtx_LABEL_REF (VOIDmode,
	operand0),
	pc_rtx)));
  _val = get_insns ();
  end_sequence ();
  return _val;
}

/* ../../gcc/gcc/config/i386/i386.md:12700 */
rtx
gen_bgt (operand0)
     rtx operand0;
{
  rtx operand1;
  rtx _val = 0;
  start_sequence ();
  {
    rtx operands[2];
    operands[0] = operand0;
ix86_expand_branch (GT, operands[0]); DONE;
    operand0 = operands[0];
    operand1 = operands[1];
  }
  emit_jump_insn (gen_rtx_SET (VOIDmode,
	pc_rtx,
	gen_rtx_IF_THEN_ELSE (VOIDmode,
	operand1,
	gen_rtx_LABEL_REF (VOIDmode,
	operand0),
	pc_rtx)));
  _val = get_insns ();
  end_sequence ();
  return _val;
}

/* ../../gcc/gcc/config/i386/i386.md:12708 */
rtx
gen_bgtu (operand0)
     rtx operand0;
{
  rtx operand1;
  rtx _val = 0;
  start_sequence ();
  {
    rtx operands[2];
    operands[0] = operand0;
ix86_expand_branch (GTU, operands[0]); DONE;
    operand0 = operands[0];
    operand1 = operands[1];
  }
  emit_jump_insn (gen_rtx_SET (VOIDmode,
	pc_rtx,
	gen_rtx_IF_THEN_ELSE (VOIDmode,
	operand1,
	gen_rtx_LABEL_REF (VOIDmode,
	operand0),
	pc_rtx)));
  _val = get_insns ();
  end_sequence ();
  return _val;
}

/* ../../gcc/gcc/config/i386/i386.md:12716 */
rtx
gen_blt (operand0)
     rtx operand0;
{
  rtx operand1;
  rtx _val = 0;
  start_sequence ();
  {
    rtx operands[2];
    operands[0] = operand0;
ix86_expand_branch (LT, operands[0]); DONE;
    operand0 = operands[0];
    operand1 = operands[1];
  }
  emit_jump_insn (gen_rtx_SET (VOIDmode,
	pc_rtx,
	gen_rtx_IF_THEN_ELSE (VOIDmode,
	operand1,
	gen_rtx_LABEL_REF (VOIDmode,
	operand0),
	pc_rtx)));
  _val = get_insns ();
  end_sequence ();
  return _val;
}

/* ../../gcc/gcc/config/i386/i386.md:12724 */
rtx
gen_bltu (operand0)
     rtx operand0;
{
  rtx operand1;
  rtx _val = 0;
  start_sequence ();
  {
    rtx operands[2];
    operands[0] = operand0;
ix86_expand_branch (LTU, operands[0]); DONE;
    operand0 = operands[0];
    operand1 = operands[1];
  }
  emit_jump_insn (gen_rtx_SET (VOIDmode,
	pc_rtx,
	gen_rtx_IF_THEN_ELSE (VOIDmode,
	operand1,
	gen_rtx_LABEL_REF (VOIDmode,
	operand0),
	pc_rtx)));
  _val = get_insns ();
  end_sequence ();
  return _val;
}

/* ../../gcc/gcc/config/i386/i386.md:12732 */
rtx
gen_bge (operand0)
     rtx operand0;
{
  rtx operand1;
  rtx _val = 0;
  start_sequence ();
  {
    rtx operands[2];
    operands[0] = operand0;
ix86_expand_branch (GE, operands[0]); DONE;
    operand0 = operands[0];
    operand1 = operands[1];
  }
  emit_jump_insn (gen_rtx_SET (VOIDmode,
	pc_rtx,
	gen_rtx_IF_THEN_ELSE (VOIDmode,
	operand1,
	gen_rtx_LABEL_REF (VOIDmode,
	operand0),
	pc_rtx)));
  _val = get_insns ();
  end_sequence ();
  return _val;
}

/* ../../gcc/gcc/config/i386/i386.md:12740 */
rtx
gen_bgeu (operand0)
     rtx operand0;
{
  rtx operand1;
  rtx _val = 0;
  start_sequence ();
  {
    rtx operands[2];
    operands[0] = operand0;
ix86_expand_branch (GEU, operands[0]); DONE;
    operand0 = operands[0];
    operand1 = operands[1];
  }
  emit_jump_insn (gen_rtx_SET (VOIDmode,
	pc_rtx,
	gen_rtx_IF_THEN_ELSE (VOIDmode,
	operand1,
	gen_rtx_LABEL_REF (VOIDmode,
	operand0),
	pc_rtx)));
  _val = get_insns ();
  end_sequence ();
  return _val;
}

/* ../../gcc/gcc/config/i386/i386.md:12748 */
rtx
gen_ble (operand0)
     rtx operand0;
{
  rtx operand1;
  rtx _val = 0;
  start_sequence ();
  {
    rtx operands[2];
    operands[0] = operand0;
ix86_expand_branch (LE, operands[0]); DONE;
    operand0 = operands[0];
    operand1 = operands[1];
  }
  emit_jump_insn (gen_rtx_SET (VOIDmode,
	pc_rtx,
	gen_rtx_IF_THEN_ELSE (VOIDmode,
	operand1,
	gen_rtx_LABEL_REF (VOIDmode,
	operand0),
	pc_rtx)));
  _val = get_insns ();
  end_sequence ();
  return _val;
}

/* ../../gcc/gcc/config/i386/i386.md:12756 */
rtx
gen_bleu (operand0)
     rtx operand0;
{
  rtx operand1;
  rtx _val = 0;
  start_sequence ();
  {
    rtx operands[2];
    operands[0] = operand0;
ix86_expand_branch (LEU, operands[0]); DONE;
    operand0 = operands[0];
    operand1 = operands[1];
  }
  emit_jump_insn (gen_rtx_SET (VOIDmode,
	pc_rtx,
	gen_rtx_IF_THEN_ELSE (VOIDmode,
	operand1,
	gen_rtx_LABEL_REF (VOIDmode,
	operand0),
	pc_rtx)));
  _val = get_insns ();
  end_sequence ();
  return _val;
}

/* ../../gcc/gcc/config/i386/i386.md:12764 */
rtx
gen_bunordered (operand0)
     rtx operand0;
{
  rtx operand1;
  rtx _val = 0;
  start_sequence ();
  {
    rtx operands[2];
    operands[0] = operand0;
ix86_expand_branch (UNORDERED, operands[0]); DONE;
    operand0 = operands[0];
    operand1 = operands[1];
  }
  emit_jump_insn (gen_rtx_SET (VOIDmode,
	pc_rtx,
	gen_rtx_IF_THEN_ELSE (VOIDmode,
	operand1,
	gen_rtx_LABEL_REF (VOIDmode,
	operand0),
	pc_rtx)));
  _val = get_insns ();
  end_sequence ();
  return _val;
}

/* ../../gcc/gcc/config/i386/i386.md:12772 */
rtx
gen_bordered (operand0)
     rtx operand0;
{
  rtx operand1;
  rtx _val = 0;
  start_sequence ();
  {
    rtx operands[2];
    operands[0] = operand0;
ix86_expand_branch (ORDERED, operands[0]); DONE;
    operand0 = operands[0];
    operand1 = operands[1];
  }
  emit_jump_insn (gen_rtx_SET (VOIDmode,
	pc_rtx,
	gen_rtx_IF_THEN_ELSE (VOIDmode,
	operand1,
	gen_rtx_LABEL_REF (VOIDmode,
	operand0),
	pc_rtx)));
  _val = get_insns ();
  end_sequence ();
  return _val;
}

/* ../../gcc/gcc/config/i386/i386.md:12780 */
rtx
gen_buneq (operand0)
     rtx operand0;
{
  rtx operand1;
  rtx _val = 0;
  start_sequence ();
  {
    rtx operands[2];
    operands[0] = operand0;
ix86_expand_branch (UNEQ, operands[0]); DONE;
    operand0 = operands[0];
    operand1 = operands[1];
  }
  emit_jump_insn (gen_rtx_SET (VOIDmode,
	pc_rtx,
	gen_rtx_IF_THEN_ELSE (VOIDmode,
	operand1,
	gen_rtx_LABEL_REF (VOIDmode,
	operand0),
	pc_rtx)));
  _val = get_insns ();
  end_sequence ();
  return _val;
}

/* ../../gcc/gcc/config/i386/i386.md:12788 */
rtx
gen_bunge (operand0)
     rtx operand0;
{
  rtx operand1;
  rtx _val = 0;
  start_sequence ();
  {
    rtx operands[2];
    operands[0] = operand0;
ix86_expand_branch (UNGE, operands[0]); DONE;
    operand0 = operands[0];
    operand1 = operands[1];
  }
  emit_jump_insn (gen_rtx_SET (VOIDmode,
	pc_rtx,
	gen_rtx_IF_THEN_ELSE (VOIDmode,
	operand1,
	gen_rtx_LABEL_REF (VOIDmode,
	operand0),
	pc_rtx)));
  _val = get_insns ();
  end_sequence ();
  return _val;
}

/* ../../gcc/gcc/config/i386/i386.md:12796 */
rtx
gen_bungt (operand0)
     rtx operand0;
{
  rtx operand1;
  rtx _val = 0;
  start_sequence ();
  {
    rtx operands[2];
    operands[0] = operand0;
ix86_expand_branch (UNGT, operands[0]); DONE;
    operand0 = operands[0];
    operand1 = operands[1];
  }
  emit_jump_insn (gen_rtx_SET (VOIDmode,
	pc_rtx,
	gen_rtx_IF_THEN_ELSE (VOIDmode,
	operand1,
	gen_rtx_LABEL_REF (VOIDmode,
	operand0),
	pc_rtx)));
  _val = get_insns ();
  end_sequence ();
  return _val;
}

/* ../../gcc/gcc/config/i386/i386.md:12804 */
rtx
gen_bunle (operand0)
     rtx operand0;
{
  rtx operand1;
  rtx _val = 0;
  start_sequence ();
  {
    rtx operands[2];
    operands[0] = operand0;
ix86_expand_branch (UNLE, operands[0]); DONE;
    operand0 = operands[0];
    operand1 = operands[1];
  }
  emit_jump_insn (gen_rtx_SET (VOIDmode,
	pc_rtx,
	gen_rtx_IF_THEN_ELSE (VOIDmode,
	operand1,
	gen_rtx_LABEL_REF (VOIDmode,
	operand0),
	pc_rtx)));
  _val = get_insns ();
  end_sequence ();
  return _val;
}

/* ../../gcc/gcc/config/i386/i386.md:12812 */
rtx
gen_bunlt (operand0)
     rtx operand0;
{
  rtx operand1;
  rtx _val = 0;
  start_sequence ();
  {
    rtx operands[2];
    operands[0] = operand0;
ix86_expand_branch (UNLT, operands[0]); DONE;
    operand0 = operands[0];
    operand1 = operands[1];
  }
  emit_jump_insn (gen_rtx_SET (VOIDmode,
	pc_rtx,
	gen_rtx_IF_THEN_ELSE (VOIDmode,
	operand1,
	gen_rtx_LABEL_REF (VOIDmode,
	operand0),
	pc_rtx)));
  _val = get_insns ();
  end_sequence ();
  return _val;
}

/* ../../gcc/gcc/config/i386/i386.md:12820 */
rtx
gen_bltgt (operand0)
     rtx operand0;
{
  rtx operand1;
  rtx _val = 0;
  start_sequence ();
  {
    rtx operands[2];
    operands[0] = operand0;
ix86_expand_branch (LTGT, operands[0]); DONE;
    operand0 = operands[0];
    operand1 = operands[1];
  }
  emit_jump_insn (gen_rtx_SET (VOIDmode,
	pc_rtx,
	gen_rtx_IF_THEN_ELSE (VOIDmode,
	operand1,
	gen_rtx_LABEL_REF (VOIDmode,
	operand0),
	pc_rtx)));
  _val = get_insns ();
  end_sequence ();
  return _val;
}

/* ../../gcc/gcc/config/i386/i386.md:12872 */
extern rtx gen_split_1365 PARAMS ((rtx *));
rtx
gen_split_1365 (operands)
      rtx *operands;
{
  rtx operand0;
  rtx operand1;
  rtx _val = 0;
  start_sequence ();
{
  PUT_MODE (operands[0], VOIDmode);
}
  operand0 = operands[0];
  operand1 = operands[1];
  emit_jump_insn (gen_rtx_SET (VOIDmode,
	pc_rtx,
	gen_rtx_IF_THEN_ELSE (VOIDmode,
	operand0,
	gen_rtx_LABEL_REF (VOIDmode,
	operand1),
	pc_rtx)));
  _val = get_insns ();
  end_sequence ();
  return _val;
}

/* ../../gcc/gcc/config/i386/i386.md:12888 */
extern rtx gen_split_1366 PARAMS ((rtx *));
rtx
gen_split_1366 (operands)
      rtx *operands;
{
  rtx operand0;
  rtx operand1;
  rtx _val = 0;
  start_sequence ();
{
  rtx new_op0 = copy_rtx (operands[0]);
  operands[0] = new_op0;
  PUT_MODE (new_op0, VOIDmode);
  PUT_CODE (new_op0, REVERSE_CONDITION (GET_CODE (new_op0),
					GET_MODE (XEXP (new_op0, 0))));

  /* Make sure that (a) the CCmode we have for the flags is strong
     enough for the reversed compare or (b) we have a valid FP compare.  */
  if (! ix86_comparison_operator (new_op0, VOIDmode))
    FAIL;
}
  operand0 = operands[0];
  operand1 = operands[1];
  emit_jump_insn (gen_rtx_SET (VOIDmode,
	pc_rtx,
	gen_rtx_IF_THEN_ELSE (VOIDmode,
	operand0,
	gen_rtx_LABEL_REF (VOIDmode,
	operand1),
	pc_rtx)));
  _val = get_insns ();
  end_sequence ();
  return _val;
}

/* ../../gcc/gcc/config/i386/i386.md:13077 */
extern rtx gen_split_1367 PARAMS ((rtx *));
rtx
gen_split_1367 (operands)
      rtx *operands ATTRIBUTE_UNUSED;
{
  rtx _val = 0;
  start_sequence ();
{
  ix86_split_fp_branch (GET_CODE (operands[0]), operands[1], operands[2],
			operands[3], operands[4], NULL_RTX);
  DONE;
}
  emit_insn (const0_rtx);
  _val = get_insns ();
  end_sequence ();
  return _val;
}

/* ../../gcc/gcc/config/i386/i386.md:13094 */
extern rtx gen_split_1368 PARAMS ((rtx *));
rtx
gen_split_1368 (operands)
      rtx *operands;
{
  rtx operand0;
  rtx operand1;
  rtx operand2;
  rtx operand3;
  rtx operand4;
  rtx operand5;
  rtx operand6;
  rtx _val = 0;
  start_sequence ();
{
  ix86_split_fp_branch (GET_CODE (operands[0]), operands[1], operands[2],
			operands[3], operands[4], operands[5]);
  DONE;
}
  operand0 = operands[0];
  operand1 = operands[1];
  operand2 = operands[2];
  operand3 = operands[3];
  operand4 = operands[4];
  operand5 = operands[5];
  operand6 = operands[6];
  emit_jump_insn (gen_rtx_SET (VOIDmode,
	pc_rtx,
	gen_rtx_IF_THEN_ELSE (VOIDmode,
	operand6,
	operand3,
	operand4)));
  _val = get_insns ();
  end_sequence ();
  return _val;
}

/* ../../gcc/gcc/config/i386/i386.md:13132 */
rtx
gen_indirect_jump (operand0)
     rtx operand0;
{
  return gen_rtx_SET (VOIDmode,
	pc_rtx,
	operand0);
}

/* ../../gcc/gcc/config/i386/i386.md:13151 */
rtx
gen_tablejump (operand0, operand1)
     rtx operand0;
     rtx operand1;
{
  rtx _val = 0;
  start_sequence ();
  {
    rtx operands[2];
    operands[0] = operand0;
    operands[1] = operand1;
{
  /* In PIC mode, the table entries are stored GOT (32-bit) or PC (64-bit)
     relative.  Convert the relative address to an absolute address.  */
  if (flag_pic)
    {
      rtx op0, op1;
      enum rtx_code code;

      if (TARGET_64BIT)
	{
	  code = PLUS;
	  op0 = operands[0];
	  op1 = gen_rtx_LABEL_REF (Pmode, operands[1]);
	}
      else if (TARGET_MACHO || HAVE_AS_GOTOFF_IN_DATA)
	{
	  code = PLUS;
	  op0 = operands[0];
	  op1 = pic_offset_table_rtx;
	}
      else
	{
	  code = MINUS;
	  op0 = pic_offset_table_rtx;
	  op1 = operands[0];
	}

      operands[0] = expand_simple_binop (Pmode, code, op0, op1, NULL_RTX, 0,
					 OPTAB_DIRECT);
    }
}
    operand0 = operands[0];
    operand1 = operands[1];
  }
  emit_jump_insn (gen_rtx_PARALLEL (VOIDmode,
	gen_rtvec (2,
		gen_rtx_SET (VOIDmode,
	pc_rtx,
	operand0),
		gen_rtx_USE (VOIDmode,
	gen_rtx_LABEL_REF (VOIDmode,
	operand1)))));
  _val = get_insns ();
  end_sequence ();
  return _val;
}

/* ../../gcc/gcc/config/i386/i386.md:13208 */
rtx
gen_doloop_end (operand0, operand1, operand2, operand3, operand4)
     rtx operand0;
     rtx operand1;
     rtx operand2;
     rtx operand3;
     rtx operand4;
{
  rtx _val = 0;
  start_sequence ();
  {
    rtx operands[5];
    operands[0] = operand0;
    operands[1] = operand1;
    operands[2] = operand2;
    operands[3] = operand3;
    operands[4] = operand4;
                                 
{
  /* Only use cloop on innermost loops.  */
  if (INTVAL (operands[3]) > 1)
    FAIL;
  if (GET_MODE (operands[0]) != SImode)
    FAIL;
  emit_jump_insn (gen_doloop_end_internal (operands[4], operands[0],
					   operands[0]));
  DONE;
}
    operand0 = operands[0];
    operand1 = operands[1];
    operand2 = operands[2];
    operand3 = operands[3];
    operand4 = operands[4];
  }
  emit_insn (gen_rtx_USE (VOIDmode,
	operand0));
  emit_insn (gen_rtx_USE (VOIDmode,
	operand1));
  emit_insn (gen_rtx_USE (VOIDmode,
	operand2));
  emit_insn (gen_rtx_USE (VOIDmode,
	operand3));
  emit_insn (gen_rtx_USE (VOIDmode,
	operand4));
  _val = get_insns ();
  end_sequence ();
  return _val;
}

/* ../../gcc/gcc/config/i386/i386.md:13260 */
extern rtx gen_split_1372 PARAMS ((rtx *));
rtx
gen_split_1372 (operands)
      rtx *operands;
{
  rtx operand0;
  rtx operand1;
  rtx _val = 0;
  start_sequence ();

  operand0 = operands[0];
  operand1 = operands[1];
  emit (gen_rtx_PARALLEL (VOIDmode,
	gen_rtvec (2,
		gen_rtx_SET (VOIDmode,
	gen_rtx_REG (CCZmode,
	17),
	gen_rtx_COMPARE (CCZmode,
	gen_rtx_PLUS (SImode,
	operand1,
	constm1_rtx),
	const0_rtx)),
		gen_rtx_SET (VOIDmode,
	copy_rtx (operand1),
	gen_rtx_PLUS (SImode,
	copy_rtx (operand1),
	constm1_rtx)))));
  emit_jump_insn (gen_rtx_SET (VOIDmode,
	pc_rtx,
	gen_rtx_IF_THEN_ELSE (VOIDmode,
	gen_rtx_NE (VOIDmode,
	gen_rtx_REG (CCZmode,
	17),
	const0_rtx),
	operand0,
	pc_rtx)));
  _val = get_insns ();
  end_sequence ();
  return _val;
}

/* ../../gcc/gcc/config/i386/i386.md:13283 */
extern rtx gen_split_1373 PARAMS ((rtx *));
rtx
gen_split_1373 (operands)
      rtx *operands;
{
  rtx operand0;
  rtx operand1;
  rtx operand2;
  rtx operand3;
  rtx _val = 0;
  start_sequence ();

  operand0 = operands[0];
  operand1 = operands[1];
  operand2 = operands[2];
  operand3 = operands[3];
  emit_insn (gen_rtx_SET (VOIDmode,
	operand3,
	operand1));
  emit (gen_rtx_PARALLEL (VOIDmode,
	gen_rtvec (2,
		gen_rtx_SET (VOIDmode,
	gen_rtx_REG (CCZmode,
	17),
	gen_rtx_COMPARE (CCZmode,
	gen_rtx_PLUS (SImode,
	copy_rtx (operand3),
	constm1_rtx),
	const0_rtx)),
		gen_rtx_SET (VOIDmode,
	copy_rtx (operand3),
	gen_rtx_PLUS (SImode,
	copy_rtx (operand3),
	constm1_rtx)))));
  emit_insn (gen_rtx_SET (VOIDmode,
	operand2,
	copy_rtx (operand3)));
  emit_jump_insn (gen_rtx_SET (VOIDmode,
	pc_rtx,
	gen_rtx_IF_THEN_ELSE (VOIDmode,
	gen_rtx_NE (VOIDmode,
	gen_rtx_REG (CCZmode,
	17),
	const0_rtx),
	operand0,
	pc_rtx)));
  _val = get_insns ();
  end_sequence ();
  return _val;
}

/* ../../gcc/gcc/config/i386/i386.md:13311 */
extern rtx gen_peephole2_1374 PARAMS ((rtx, rtx *));
rtx
gen_peephole2_1374 (curr_insn, operands)
     rtx curr_insn ATTRIBUTE_UNUSED;
     rtx *operands;
{
  rtx operand0;
  rtx operand1;
  rtx operand2;
  rtx operand3;
  rtx operand4;
  rtx operand5;
  rtx _val = 0;
  HARD_REG_SET _regs_allocated;
  CLEAR_HARD_REG_SET (_regs_allocated);
  start_sequence ();
{
  operands[4] = gen_rtx_REG (GET_MODE (operands[0]), 17);
  operands[5] = gen_rtx_REG (QImode, REGNO (operands[3]));
  ix86_expand_clear (operands[3]);
}
  operand0 = operands[0];
  operand1 = operands[1];
  operand2 = operands[2];
  operand3 = operands[3];
  operand4 = operands[4];
  operand5 = operands[5];
  emit_insn (gen_rtx_SET (VOIDmode,
	operand4,
	operand0));
  emit_insn (gen_rtx_SET (VOIDmode,
	gen_rtx_STRICT_LOW_PART (VOIDmode,
	operand5),
	operand2));
  _val = get_insns ();
  end_sequence ();
  return _val;
}

/* ../../gcc/gcc/config/i386/i386.md:13332 */
extern rtx gen_peephole2_1375 PARAMS ((rtx, rtx *));
rtx
gen_peephole2_1375 (curr_insn, operands)
     rtx curr_insn ATTRIBUTE_UNUSED;
     rtx *operands;
{
  rtx operand0;
  rtx operand1;
  rtx operand2;
  rtx operand3;
  rtx operand4;
  rtx operand5;
  rtx _val = 0;
  HARD_REG_SET _regs_allocated;
  CLEAR_HARD_REG_SET (_regs_allocated);
  start_sequence ();
{
  operands[4] = gen_rtx_REG (GET_MODE (operands[0]), 17);
  operands[5] = gen_rtx_REG (QImode, REGNO (operands[3]));
  ix86_expand_clear (operands[3]);
}
  operand0 = operands[0];
  operand1 = operands[1];
  operand2 = operands[2];
  operand3 = operands[3];
  operand4 = operands[4];
  operand5 = operands[5];
  emit_insn (gen_rtx_SET (VOIDmode,
	operand4,
	operand0));
  emit_insn (gen_rtx_SET (VOIDmode,
	gen_rtx_STRICT_LOW_PART (VOIDmode,
	operand5),
	operand2));
  _val = get_insns ();
  end_sequence ();
  return _val;
}

/* ../../gcc/gcc/config/i386/i386.md:13360 */
rtx
gen_call_pop (operand0, operand1, operand2, operand3)
     rtx operand0;
     rtx operand1;
     rtx operand2;
     rtx operand3;
{
  rtx _val = 0;
  start_sequence ();
  {
    rtx operands[4];
    operands[0] = operand0;
    operands[1] = operand1;
    operands[2] = operand2;
    operands[3] = operand3;
{
  ix86_expand_call (NULL, operands[0], operands[1], operands[2], operands[3]);
  DONE;
}
    operand0 = operands[0];
    operand1 = operands[1];
    operand2 = operands[2];
    operand3 = operands[3];
  }
  emit_call_insn (gen_rtx_PARALLEL (VOIDmode,
	gen_rtvec (2,
		gen_rtx_CALL (VOIDmode,
	operand0,
	operand1),
		gen_rtx_SET (VOIDmode,
	gen_rtx_REG (SImode,
	7),
	gen_rtx_PLUS (SImode,
	gen_rtx_REG (SImode,
	7),
	operand3)))));
  _val = get_insns ();
  end_sequence ();
  return _val;
}

/* ../../gcc/gcc/config/i386/i386.md:13407 */
rtx
gen_call (operand0, operand1, operand2)
     rtx operand0;
     rtx operand1;
     rtx operand2;
{
  rtx _val = 0;
  start_sequence ();
  {
    rtx operands[3];
    operands[0] = operand0;
    operands[1] = operand1;
    operands[2] = operand2;
{
  ix86_expand_call (NULL, operands[0], operands[1], operands[2], NULL);
  DONE;
}
    operand0 = operands[0];
    operand1 = operands[1];
    operand2 = operands[2];
  }
  emit_call_insn (gen_rtx_CALL (VOIDmode,
	operand0,
	operand1));
  emit_insn (gen_rtx_USE (VOIDmode,
	operand2));
  _val = get_insns ();
  end_sequence ();
  return _val;
}

/* ../../gcc/gcc/config/i386/i386.md:13469 */
rtx
gen_call_value_pop (operand0, operand1, operand2, operand3, operand4)
     rtx operand0;
     rtx operand1;
     rtx operand2;
     rtx operand3;
     rtx operand4;
{
  rtx _val = 0;
  start_sequence ();
  {
    rtx operands[5];
    operands[0] = operand0;
    operands[1] = operand1;
    operands[2] = operand2;
    operands[3] = operand3;
    operands[4] = operand4;
{
  ix86_expand_call (operands[0], operands[1], operands[2],
		    operands[3], operands[4]);
  DONE;
}
    operand0 = operands[0];
    operand1 = operands[1];
    operand2 = operands[2];
    operand3 = operands[3];
    operand4 = operands[4];
  }
  emit_call_insn (gen_rtx_PARALLEL (VOIDmode,
	gen_rtvec (2,
		gen_rtx_SET (VOIDmode,
	operand0,
	gen_rtx_CALL (VOIDmode,
	operand1,
	operand2)),
		gen_rtx_SET (VOIDmode,
	gen_rtx_REG (SImode,
	7),
	gen_rtx_PLUS (SImode,
	gen_rtx_REG (SImode,
	7),
	operand4)))));
  _val = get_insns ();
  end_sequence ();
  return _val;
}

/* ../../gcc/gcc/config/i386/i386.md:13483 */
rtx
gen_call_value (operand0, operand1, operand2, operand3)
     rtx operand0;
     rtx operand1;
     rtx operand2;
     rtx operand3;
{
  rtx _val = 0;
  start_sequence ();
  {
    rtx operands[4];
    operands[0] = operand0;
    operands[1] = operand1;
    operands[2] = operand2;
    operands[3] = operand3;
{
  ix86_expand_call (operands[0], operands[1], operands[2], operands[3], NULL);
  DONE;
}
    operand0 = operands[0];
    operand1 = operands[1];
    operand2 = operands[2];
    operand3 = operands[3];
  }
  emit_call_insn (gen_rtx_SET (VOIDmode,
	operand0,
	gen_rtx_CALL (VOIDmode,
	operand1,
	operand2)));
  emit_insn (gen_rtx_USE (VOIDmode,
	operand3));
  _val = get_insns ();
  end_sequence ();
  return _val;
}

/* ../../gcc/gcc/config/i386/i386.md:13497 */
rtx
gen_untyped_call (operand0, operand1, operand2)
     rtx operand0;
     rtx operand1;
     rtx operand2;
{
  rtx _val = 0;
  start_sequence ();
  {
    rtx operands[3];
    operands[0] = operand0;
    operands[1] = operand1;
    operands[2] = operand2;
{
  int i;

  /* In order to give reg-stack an easier job in validating two
     coprocessor registers as containing a possible return value,
     simply pretend the untyped call returns a complex long double
     value.  */

  ix86_expand_call ((TARGET_FLOAT_RETURNS_IN_80387
		     ? gen_rtx_REG (XCmode, FIRST_FLOAT_REG) : NULL),
		    operands[0], const0_rtx, GEN_INT (SSE_REGPARM_MAX - 1),
		    NULL);

  for (i = 0; i < XVECLEN (operands[2], 0); i++)
    {
      rtx set = XVECEXP (operands[2], 0, i);
      emit_move_insn (SET_DEST (set), SET_SRC (set));
    }

  /* The optimizer does not know that the call sets the function value
     registers we stored in the result block.  We avoid problems by
     claiming that all hard registers are used and clobbered at this
     point.  */
  emit_insn (gen_blockage (const0_rtx));

  DONE;
}
    operand0 = operands[0];
    operand1 = operands[1];
    operand2 = operands[2];
  }
  emit_call_insn (gen_rtx_PARALLEL (VOIDmode,
	gen_rtvec (3,
		gen_rtx_CALL (VOIDmode,
	operand0,
	const0_rtx),
		operand1,
		operand2)));
  _val = get_insns ();
  end_sequence ();
  return _val;
}

/* ../../gcc/gcc/config/i386/i386.md:13546 */
rtx
gen_return ()
{
  rtx _val = 0;
  start_sequence ();
  {
{
  if (current_function_pops_args)
    {
      rtx popc = GEN_INT (current_function_pops_args);
      emit_jump_insn (gen_return_pop_internal (popc));
      DONE;
    }
}
  }
  emit_jump_insn (gen_rtx_RETURN (VOIDmode));
  _val = get_insns ();
  end_sequence ();
  return _val;
}

/* ../../gcc/gcc/config/i386/i386.md:13592 */
rtx
gen_prologue ()
{
  rtx _val = 0;
  start_sequence ();
  {
ix86_expand_prologue (); DONE;
  }
  emit_insn (const1_rtx);
  _val = get_insns ();
  end_sequence ();
  return _val;
}

/* ../../gcc/gcc/config/i386/i386.md:13606 */
rtx
gen_epilogue ()
{
  rtx _val = 0;
  start_sequence ();
  {
ix86_expand_epilogue (1); DONE;
  }
  emit_insn (const1_rtx);
  _val = get_insns ();
  end_sequence ();
  return _val;
}

/* ../../gcc/gcc/config/i386/i386.md:13611 */
rtx
gen_sibcall_epilogue ()
{
  rtx _val = 0;
  start_sequence ();
  {
ix86_expand_epilogue (0); DONE;
  }
  emit_insn (const1_rtx);
  _val = get_insns ();
  end_sequence ();
  return _val;
}

/* ../../gcc/gcc/config/i386/i386.md:13616 */
rtx
gen_eh_return (operand0)
     rtx operand0;
{
  rtx _val = 0;
  start_sequence ();
  {
    rtx operands[1];
    operands[0] = operand0;
{
  rtx tmp, sa = EH_RETURN_STACKADJ_RTX, ra = operands[0];

  /* Tricky bit: we write the address of the handler to which we will
     be returning into someone else's stack frame, one word below the
     stack address we wish to restore.  */
  tmp = gen_rtx_PLUS (Pmode, arg_pointer_rtx, sa);
  tmp = plus_constant (tmp, -UNITS_PER_WORD);
  tmp = gen_rtx_MEM (Pmode, tmp);
  emit_move_insn (tmp, ra);

  if (Pmode == SImode)
    emit_insn (gen_eh_return_si (sa));
  else
    emit_insn (gen_eh_return_di (sa));
  emit_barrier ();
  DONE;
}
    operand0 = operands[0];
  }
  emit_insn (gen_rtx_USE (VOIDmode,
	operand0));
  _val = get_insns ();
  end_sequence ();
  return _val;
}

/* ../../gcc/gcc/config/i386/i386.md:13638 */
extern rtx gen_split_1386 PARAMS ((rtx *));
rtx
gen_split_1386 (operands)
      rtx *operands ATTRIBUTE_UNUSED;
{
  rtx _val = 0;
  start_sequence ();
ix86_expand_epilogue (2); DONE;
  emit_insn (const1_rtx);
  _val = get_insns ();
  end_sequence ();
  return _val;
}

/* ../../gcc/gcc/config/i386/i386.md:13647 */
extern rtx gen_split_1387 PARAMS ((rtx *));
rtx
gen_split_1387 (operands)
      rtx *operands ATTRIBUTE_UNUSED;
{
  rtx _val = 0;
  start_sequence ();
ix86_expand_epilogue (2); DONE;
  emit_insn (const1_rtx);
  _val = get_insns ();
  end_sequence ();
  return _val;
}

/* ../../gcc/gcc/config/i386/i386.md:13680 */
rtx
gen_ffssi2 (operand0, operand1)
     rtx operand0;
     rtx operand1;
{
  rtx _val = 0;
  start_sequence ();
  {
    rtx operands[2];
    operands[0] = operand0;
    operands[1] = operand1;
{
  rtx out = gen_reg_rtx (SImode), tmp = gen_reg_rtx (SImode);
  rtx in = operands[1];

  if (TARGET_CMOVE)
    {
      emit_move_insn (tmp, constm1_rtx);
      emit_insn (gen_ffssi_1 (out, in));
      emit_insn (gen_rtx_SET (VOIDmode, out,
		  gen_rtx_IF_THEN_ELSE (SImode, 
		    gen_rtx_EQ (VOIDmode, gen_rtx_REG (CCZmode, FLAGS_REG),
				const0_rtx),
		    tmp,
		    out)));
      emit_insn (gen_addsi3 (out, out, const1_rtx));
      emit_move_insn (operands[0], out);
    }

  /* Pentium bsf instruction is extremly slow.  The following code is
     recommended by the Intel Optimizing Manual as a reasonable replacement:
           TEST    EAX,EAX
	   JZ      SHORT BS2
	   XOR     ECX,ECX
	   MOV     DWORD PTR [TEMP+4],ECX
	   SUB     ECX,EAX
	   AND     EAX,ECX
	   MOV     DWORD PTR [TEMP],EAX
	   FILD    QWORD PTR [TEMP]
	   FSTP    QWORD PTR [TEMP]
	   WAIT    ; WAIT only needed for compatibility with
	           ; earlier processors
	   MOV     ECX, DWORD PTR [TEMP+4]
	   SHR     ECX,20
	   SUB     ECX,3FFH
	   TEST    EAX,EAX       ; clear zero flag
       BS2:
     Following piece of code expand ffs to similar beast.
       */

  else if (TARGET_PENTIUM && !optimize_size && TARGET_80387)
    {
      rtx label = gen_label_rtx ();
      rtx lo, hi;
      rtx mem = assign_386_stack_local (DImode, 0);
      rtx fptmp = gen_reg_rtx (DFmode);
      split_di (&mem, 1, &lo, &hi);

      emit_move_insn (out, const0_rtx);

      emit_cmp_and_jump_insns (in, const0_rtx, EQ, 0, SImode, 1, label);

      emit_move_insn (hi, out);
      emit_insn (gen_subsi3 (out, out, in));
      emit_insn (gen_andsi3 (out, out, in));
      emit_move_insn (lo, out);
      emit_insn (gen_floatdidf2 (fptmp,mem));
      emit_move_insn (gen_rtx_MEM (DFmode, XEXP (mem, 0)), fptmp);
      emit_move_insn (out, hi);
      emit_insn (gen_lshrsi3 (out, out, GEN_INT (20)));
      emit_insn (gen_subsi3 (out, out, GEN_INT (0x3ff - 1)));

      emit_label (label);
      LABEL_NUSES (label) = 1;

      emit_move_insn (operands[0], out);
    }
  else
    {
      emit_move_insn (tmp, const0_rtx);
      emit_insn (gen_ffssi_1 (out, in));
      emit_insn (gen_rtx_SET (VOIDmode, 
		  gen_rtx_STRICT_LOW_PART (VOIDmode, gen_lowpart (QImode, tmp)),
		  gen_rtx_EQ (QImode, gen_rtx_REG (CCZmode, FLAGS_REG),
			      const0_rtx)));
      emit_insn (gen_negsi2 (tmp, tmp));
      emit_insn (gen_iorsi3 (out, out, tmp));
      emit_insn (gen_addsi3 (out, out, const1_rtx));
      emit_move_insn (operands[0], out);
    }
  DONE;  
}
    operand0 = operands[0];
    operand1 = operands[1];
  }
  emit_insn (gen_rtx_SET (VOIDmode,
	operand0,
	gen_rtx_FFS (SImode,
	operand1)));
  _val = get_insns ();
  end_sequence ();
  return _val;
}

/* ../../gcc/gcc/config/i386/i386.md:13814 */
rtx
gen_tls_global_dynamic_32 (operand0, operand1)
     rtx operand0;
     rtx operand1;
{
  rtx operand2;
  rtx operand3;
  rtx operand4 ATTRIBUTE_UNUSED;
  rtx operand5 ATTRIBUTE_UNUSED;
  rtx _val = 0;
  start_sequence ();
  {
    rtx operands[6];
    operands[0] = operand0;
    operands[1] = operand1;
{
  if (flag_pic)
    operands[2] = pic_offset_table_rtx;
  else
    {
      operands[2] = gen_reg_rtx (Pmode);
      emit_insn (gen_set_got (operands[2]));
    }
  operands[3] = ix86_tls_get_addr ();
}
    operand0 = operands[0];
    operand1 = operands[1];
    operand2 = operands[2];
    operand3 = operands[3];
    operand4 = operands[4];
    operand5 = operands[5];
  }
  emit (gen_rtx_PARALLEL (VOIDmode,
	gen_rtvec (4,
		gen_rtx_SET (VOIDmode,
	operand0,
	gen_rtx_UNSPEC (SImode,
	gen_rtvec (3,
		operand2,
		operand1,
		operand3),
	16)),
		gen_rtx_CLOBBER (VOIDmode,
	gen_rtx_SCRATCH (SImode)),
		gen_rtx_CLOBBER (VOIDmode,
	gen_rtx_SCRATCH (SImode)),
		gen_rtx_CLOBBER (VOIDmode,
	gen_rtx_REG (CCmode,
	17)))));
  _val = get_insns ();
  end_sequence ();
  return _val;
}

/* ../../gcc/gcc/config/i386/i386.md:13847 */
rtx
gen_tls_global_dynamic_64 (operand0, operand1)
     rtx operand0;
     rtx operand1;
{
  rtx operand2;
  rtx _val = 0;
  start_sequence ();
  {
    rtx operands[3];
    operands[0] = operand0;
    operands[1] = operand1;
{
  operands[2] = ix86_tls_get_addr ();
}
    operand0 = operands[0];
    operand1 = operands[1];
    operand2 = operands[2];
  }
  emit_call_insn (gen_rtx_PARALLEL (VOIDmode,
	gen_rtvec (2,
		gen_rtx_SET (VOIDmode,
	operand0,
	gen_rtx_CALL (VOIDmode,
	gen_rtx_MEM (QImode,
	operand2),
	const0_rtx)),
		gen_rtx_UNSPEC (DImode,
	gen_rtvec (1,
		operand1),
	16))));
  _val = get_insns ();
  end_sequence ();
  return _val;
}

/* ../../gcc/gcc/config/i386/i386.md:13884 */
rtx
gen_tls_local_dynamic_base_32 (operand0)
     rtx operand0;
{
  rtx operand1;
  rtx operand2;
  rtx operand3 ATTRIBUTE_UNUSED;
  rtx operand4 ATTRIBUTE_UNUSED;
  rtx _val = 0;
  start_sequence ();
  {
    rtx operands[5];
    operands[0] = operand0;
{
  if (flag_pic)
    operands[1] = pic_offset_table_rtx;
  else
    {
      operands[1] = gen_reg_rtx (Pmode);
      emit_insn (gen_set_got (operands[1]));
    }
  operands[2] = ix86_tls_get_addr ();
}
    operand0 = operands[0];
    operand1 = operands[1];
    operand2 = operands[2];
    operand3 = operands[3];
    operand4 = operands[4];
  }
  emit (gen_rtx_PARALLEL (VOIDmode,
	gen_rtvec (4,
		gen_rtx_SET (VOIDmode,
	operand0,
	gen_rtx_UNSPEC (SImode,
	gen_rtvec (2,
		operand1,
		operand2),
	17)),
		gen_rtx_CLOBBER (VOIDmode,
	gen_rtx_SCRATCH (SImode)),
		gen_rtx_CLOBBER (VOIDmode,
	gen_rtx_SCRATCH (SImode)),
		gen_rtx_CLOBBER (VOIDmode,
	gen_rtx_REG (CCmode,
	17)))));
  _val = get_insns ();
  end_sequence ();
  return _val;
}

/* ../../gcc/gcc/config/i386/i386.md:13913 */
rtx
gen_tls_local_dynamic_base_64 (operand0)
     rtx operand0;
{
  rtx operand1;
  rtx _val = 0;
  start_sequence ();
  {
    rtx operands[2];
    operands[0] = operand0;
{
  operands[1] = ix86_tls_get_addr ();
}
    operand0 = operands[0];
    operand1 = operands[1];
  }
  emit_call_insn (gen_rtx_PARALLEL (VOIDmode,
	gen_rtvec (2,
		gen_rtx_SET (VOIDmode,
	operand0,
	gen_rtx_CALL (VOIDmode,
	gen_rtx_MEM (QImode,
	operand1),
	const0_rtx)),
		gen_rtx_UNSPEC (DImode,
	gen_rtvec (1,
		const0_rtx),
	17))));
  _val = get_insns ();
  end_sequence ();
  return _val;
}

/* ../../gcc/gcc/config/i386/i386.md:13925 */
extern rtx gen_split_1393 PARAMS ((rtx *));
rtx
gen_split_1393 (operands)
      rtx *operands;
{
  rtx operand0;
  rtx operand1;
  rtx operand2;
  rtx operand3;
  rtx operand4;
  rtx operand5;
  rtx _val = 0;
  start_sequence ();

  operand0 = operands[0];
  operand1 = operands[1];
  operand2 = operands[2];
  operand3 = operands[3];
  operand4 = operands[4];
  operand5 = operands[5];
  emit (gen_rtx_PARALLEL (VOIDmode,
	gen_rtvec (4,
		gen_rtx_SET (VOIDmode,
	operand0,
	gen_rtx_UNSPEC (SImode,
	gen_rtvec (3,
		operand1,
		operand3,
		operand2),
	16)),
		gen_rtx_CLOBBER (VOIDmode,
	operand4),
		gen_rtx_CLOBBER (VOIDmode,
	operand5),
		gen_rtx_CLOBBER (VOIDmode,
	gen_rtx_REG (CCmode,
	17)))));
  _val = get_insns ();
  end_sequence ();
  return _val;
}

/* ../../gcc/gcc/config/i386/i386.md:14627 */
extern rtx gen_split_1394 PARAMS ((rtx *));
rtx
gen_split_1394 (operands)
      rtx *operands ATTRIBUTE_UNUSED;
{
  rtx _val = 0;
  start_sequence ();
{ 
  operands[4] = ix86_force_to_memory (GET_MODE (operands[1]), operands[1]);
  operands[4] = gen_rtx_FLOAT (GET_MODE (operands[0]), operands[4]);
  emit_insn (gen_rtx_SET (VOIDmode, operands[0],
			  gen_rtx_fmt_ee (GET_CODE (operands[3]),
					  GET_MODE (operands[3]),
					  operands[4],
					  operands[2])));
  ix86_free_from_memory (GET_MODE (operands[1]));
  DONE;
}
  emit_insn (const0_rtx);
  _val = get_insns ();
  end_sequence ();
  return _val;
}

/* ../../gcc/gcc/config/i386/i386.md:14647 */
extern rtx gen_split_1395 PARAMS ((rtx *));
rtx
gen_split_1395 (operands)
      rtx *operands ATTRIBUTE_UNUSED;
{
  rtx _val = 0;
  start_sequence ();
{
  operands[4] = ix86_force_to_memory (GET_MODE (operands[2]), operands[2]);
  operands[4] = gen_rtx_FLOAT (GET_MODE (operands[0]), operands[4]);
  emit_insn (gen_rtx_SET (VOIDmode, operands[0],
			  gen_rtx_fmt_ee (GET_CODE (operands[3]),
					  GET_MODE (operands[3]),
					  operands[1],
					  operands[4])));
  ix86_free_from_memory (GET_MODE (operands[2]));
  DONE;
}
  emit_insn (const0_rtx);
  _val = get_insns ();
  end_sequence ();
  return _val;
}

/* ../../gcc/gcc/config/i386/i386.md:14669 */
rtx
gen_sqrtsf2 (operand0, operand1)
     rtx operand0;
     rtx operand1;
{
  rtx _val = 0;
  start_sequence ();
  {
    rtx operands[2];
    operands[0] = operand0;
    operands[1] = operand1;
{
  if (!TARGET_SSE_MATH)
    operands[1] = force_reg (SFmode, operands[1]);
}
    operand0 = operands[0];
    operand1 = operands[1];
  }
  emit_insn (gen_rtx_SET (VOIDmode,
	operand0,
	gen_rtx_SQRT (SFmode,
	operand1)));
  _val = get_insns ();
  end_sequence ();
  return _val;
}

/* ../../gcc/gcc/config/i386/i386.md:14709 */
rtx
gen_sqrtdf2 (operand0, operand1)
     rtx operand0;
     rtx operand1;
{
  rtx _val = 0;
  start_sequence ();
  {
    rtx operands[2];
    operands[0] = operand0;
    operands[1] = operand1;
{
  if (!TARGET_SSE2 || !TARGET_SSE_MATH)
    operands[1] = force_reg (DFmode, operands[1]);
}
    operand0 = operands[0];
    operand1 = operands[1];
  }
  emit_insn (gen_rtx_SET (VOIDmode,
	operand0,
	gen_rtx_SQRT (DFmode,
	operand1)));
  _val = get_insns ();
  end_sequence ();
  return _val;
}

/* ../../gcc/gcc/config/i386/i386.md:14923 */
rtx
gen_movstrsi (operand0, operand1, operand2, operand3)
     rtx operand0;
     rtx operand1;
     rtx operand2;
     rtx operand3;
{
  rtx _val = 0;
  start_sequence ();
  {
    rtx operands[4];
    operands[0] = operand0;
    operands[1] = operand1;
    operands[2] = operand2;
    operands[3] = operand3;
{
 if (ix86_expand_movstr (operands[0], operands[1], operands[2], operands[3]))
   DONE;
 else
   FAIL;
}
    operand0 = operands[0];
    operand1 = operands[1];
    operand2 = operands[2];
    operand3 = operands[3];
  }
  emit_insn (gen_rtx_USE (VOIDmode,
	operand0));
  emit_insn (gen_rtx_USE (VOIDmode,
	operand1));
  emit_insn (gen_rtx_USE (VOIDmode,
	operand2));
  emit_insn (gen_rtx_USE (VOIDmode,
	operand3));
  _val = get_insns ();
  end_sequence ();
  return _val;
}

/* ../../gcc/gcc/config/i386/i386.md:14936 */
rtx
gen_movstrdi (operand0, operand1, operand2, operand3)
     rtx operand0;
     rtx operand1;
     rtx operand2;
     rtx operand3;
{
  rtx _val = 0;
  start_sequence ();
  {
    rtx operands[4];
    operands[0] = operand0;
    operands[1] = operand1;
    operands[2] = operand2;
    operands[3] = operand3;
{
 if (ix86_expand_movstr (operands[0], operands[1], operands[2], operands[3]))
   DONE;
 else
   FAIL;
}
    operand0 = operands[0];
    operand1 = operands[1];
    operand2 = operands[2];
    operand3 = operands[3];
  }
  emit_insn (gen_rtx_USE (VOIDmode,
	operand0));
  emit_insn (gen_rtx_USE (VOIDmode,
	operand1));
  emit_insn (gen_rtx_USE (VOIDmode,
	operand2));
  emit_insn (gen_rtx_USE (VOIDmode,
	operand3));
  _val = get_insns ();
  end_sequence ();
  return _val;
}

/* ../../gcc/gcc/config/i386/i386.md:14952 */
rtx
gen_strmovdi_rex64 (operand0, operand1)
     rtx operand0;
     rtx operand1;
{
  rtx operand2;
  rtx _val = 0;
  start_sequence ();
  {
    rtx operands[3];
    operands[0] = operand0;
    operands[1] = operand1;
{
  if (TARGET_SINGLE_STRINGOP || optimize_size)
    {
      emit_insn (gen_strmovdi_rex_1 (operands[0], operands[1], operands[0],
				     operands[1]));
      DONE;
    }
  else 
    operands[2] = gen_reg_rtx (DImode);
}
    operand0 = operands[0];
    operand1 = operands[1];
    operand2 = operands[2];
  }
  emit_insn (gen_rtx_SET (VOIDmode,
	operand2,
	gen_rtx_MEM (DImode,
	operand1)));
  emit_insn (gen_rtx_SET (VOIDmode,
	gen_rtx_MEM (DImode,
	operand0),
	operand2));
  emit (gen_rtx_PARALLEL (VOIDmode,
	gen_rtvec (2,
		gen_rtx_SET (VOIDmode,
	operand0,
	gen_rtx_PLUS (DImode,
	operand0,
	GEN_INT (8LL))),
		gen_rtx_CLOBBER (VOIDmode,
	gen_rtx_REG (CCmode,
	17)))));
  emit (gen_rtx_PARALLEL (VOIDmode,
	gen_rtvec (2,
		gen_rtx_SET (VOIDmode,
	operand1,
	gen_rtx_PLUS (DImode,
	operand1,
	GEN_INT (8LL))),
		gen_rtx_CLOBBER (VOIDmode,
	gen_rtx_REG (CCmode,
	17)))));
  _val = get_insns ();
  end_sequence ();
  return _val;
}

/* ../../gcc/gcc/config/i386/i386.md:14974 */
rtx
gen_strmovsi (operand0, operand1)
     rtx operand0;
     rtx operand1;
{
  rtx operand2;
  rtx _val = 0;
  start_sequence ();
  {
    rtx operands[3];
    operands[0] = operand0;
    operands[1] = operand1;
{
  if (TARGET_64BIT)
    {
      emit_insn (gen_strmovsi_rex64 (operands[0], operands[1]));
      DONE;
    }
  if (TARGET_SINGLE_STRINGOP || optimize_size)
    {
      emit_insn (gen_strmovsi_1 (operands[0], operands[1], operands[0],
				operands[1]));
      DONE;
    }
  else 
    operands[2] = gen_reg_rtx (SImode);
}
    operand0 = operands[0];
    operand1 = operands[1];
    operand2 = operands[2];
  }
  emit_insn (gen_rtx_SET (VOIDmode,
	operand2,
	gen_rtx_MEM (SImode,
	operand1)));
  emit_insn (gen_rtx_SET (VOIDmode,
	gen_rtx_MEM (SImode,
	operand0),
	operand2));
  emit (gen_rtx_PARALLEL (VOIDmode,
	gen_rtvec (2,
		gen_rtx_SET (VOIDmode,
	operand0,
	gen_rtx_PLUS (SImode,
	operand0,
	GEN_INT (4LL))),
		gen_rtx_CLOBBER (VOIDmode,
	gen_rtx_REG (CCmode,
	17)))));
  emit (gen_rtx_PARALLEL (VOIDmode,
	gen_rtvec (2,
		gen_rtx_SET (VOIDmode,
	operand1,
	gen_rtx_PLUS (SImode,
	operand1,
	GEN_INT (4LL))),
		gen_rtx_CLOBBER (VOIDmode,
	gen_rtx_REG (CCmode,
	17)))));
  _val = get_insns ();
  end_sequence ();
  return _val;
}

/* ../../gcc/gcc/config/i386/i386.md:15000 */
rtx
gen_strmovsi_rex64 (operand0, operand1)
     rtx operand0;
     rtx operand1;
{
  rtx operand2;
  rtx _val = 0;
  start_sequence ();
  {
    rtx operands[3];
    operands[0] = operand0;
    operands[1] = operand1;
{
  if (TARGET_SINGLE_STRINGOP || optimize_size)
    {
      emit_insn (gen_strmovsi_rex_1 (operands[0], operands[1], operands[0],
				     operands[1]));
      DONE;
    }
  else 
    operands[2] = gen_reg_rtx (SImode);
}
    operand0 = operands[0];
    operand1 = operands[1];
    operand2 = operands[2];
  }
  emit_insn (gen_rtx_SET (VOIDmode,
	operand2,
	gen_rtx_MEM (SImode,
	operand1)));
  emit_insn (gen_rtx_SET (VOIDmode,
	gen_rtx_MEM (SImode,
	operand0),
	operand2));
  emit (gen_rtx_PARALLEL (VOIDmode,
	gen_rtvec (2,
		gen_rtx_SET (VOIDmode,
	operand0,
	gen_rtx_PLUS (DImode,
	operand0,
	GEN_INT (4LL))),
		gen_rtx_CLOBBER (VOIDmode,
	gen_rtx_REG (CCmode,
	17)))));
  emit (gen_rtx_PARALLEL (VOIDmode,
	gen_rtvec (2,
		gen_rtx_SET (VOIDmode,
	operand1,
	gen_rtx_PLUS (DImode,
	operand1,
	GEN_INT (4LL))),
		gen_rtx_CLOBBER (VOIDmode,
	gen_rtx_REG (CCmode,
	17)))));
  _val = get_insns ();
  end_sequence ();
  return _val;
}

/* ../../gcc/gcc/config/i386/i386.md:15021 */
rtx
gen_strmovhi (operand0, operand1)
     rtx operand0;
     rtx operand1;
{
  rtx operand2;
  rtx _val = 0;
  start_sequence ();
  {
    rtx operands[3];
    operands[0] = operand0;
    operands[1] = operand1;
{
  if (TARGET_64BIT)
    {
      emit_insn (gen_strmovhi_rex64 (operands[0], operands[1]));
      DONE;
    }
  if (TARGET_SINGLE_STRINGOP || optimize_size)
    {
      emit_insn (gen_strmovhi_1 (operands[0], operands[1], operands[0],
				operands[1]));
      DONE;
    }
  else 
    operands[2] = gen_reg_rtx (HImode);
}
    operand0 = operands[0];
    operand1 = operands[1];
    operand2 = operands[2];
  }
  emit_insn (gen_rtx_SET (VOIDmode,
	operand2,
	gen_rtx_MEM (HImode,
	operand1)));
  emit_insn (gen_rtx_SET (VOIDmode,
	gen_rtx_MEM (HImode,
	operand0),
	operand2));
  emit (gen_rtx_PARALLEL (VOIDmode,
	gen_rtvec (2,
		gen_rtx_SET (VOIDmode,
	operand0,
	gen_rtx_PLUS (SImode,
	operand0,
	GEN_INT (2LL))),
		gen_rtx_CLOBBER (VOIDmode,
	gen_rtx_REG (CCmode,
	17)))));
  emit (gen_rtx_PARALLEL (VOIDmode,
	gen_rtvec (2,
		gen_rtx_SET (VOIDmode,
	operand1,
	gen_rtx_PLUS (SImode,
	operand1,
	GEN_INT (2LL))),
		gen_rtx_CLOBBER (VOIDmode,
	gen_rtx_REG (CCmode,
	17)))));
  _val = get_insns ();
  end_sequence ();
  return _val;
}

/* ../../gcc/gcc/config/i386/i386.md:15047 */
rtx
gen_strmovhi_rex64 (operand0, operand1)
     rtx operand0;
     rtx operand1;
{
  rtx operand2;
  rtx _val = 0;
  start_sequence ();
  {
    rtx operands[3];
    operands[0] = operand0;
    operands[1] = operand1;
{
  if (TARGET_SINGLE_STRINGOP || optimize_size)
    {
      emit_insn (gen_strmovhi_rex_1 (operands[0], operands[1], operands[0],
				     operands[1]));
      DONE;
    }
  else 
    operands[2] = gen_reg_rtx (HImode);
}
    operand0 = operands[0];
    operand1 = operands[1];
    operand2 = operands[2];
  }
  emit_insn (gen_rtx_SET (VOIDmode,
	operand2,
	gen_rtx_MEM (HImode,
	operand1)));
  emit_insn (gen_rtx_SET (VOIDmode,
	gen_rtx_MEM (HImode,
	operand0),
	operand2));
  emit (gen_rtx_PARALLEL (VOIDmode,
	gen_rtvec (2,
		gen_rtx_SET (VOIDmode,
	operand0,
	gen_rtx_PLUS (DImode,
	operand0,
	GEN_INT (2LL))),
		gen_rtx_CLOBBER (VOIDmode,
	gen_rtx_REG (CCmode,
	17)))));
  emit (gen_rtx_PARALLEL (VOIDmode,
	gen_rtvec (2,
		gen_rtx_SET (VOIDmode,
	operand1,
	gen_rtx_PLUS (DImode,
	operand1,
	GEN_INT (2LL))),
		gen_rtx_CLOBBER (VOIDmode,
	gen_rtx_REG (CCmode,
	17)))));
  _val = get_insns ();
  end_sequence ();
  return _val;
}

/* ../../gcc/gcc/config/i386/i386.md:15068 */
rtx
gen_strmovqi (operand0, operand1)
     rtx operand0;
     rtx operand1;
{
  rtx operand2;
  rtx _val = 0;
  start_sequence ();
  {
    rtx operands[3];
    operands[0] = operand0;
    operands[1] = operand1;
{
  if (TARGET_64BIT)
    {
      emit_insn (gen_strmovqi_rex64 (operands[0], operands[1]));
      DONE;
    }
  if (TARGET_SINGLE_STRINGOP || optimize_size)
    {
      emit_insn (gen_strmovqi_1 (operands[0], operands[1], operands[0],
				operands[1]));
      DONE;
    }
  else 
    operands[2] = gen_reg_rtx (QImode);
}
    operand0 = operands[0];
    operand1 = operands[1];
    operand2 = operands[2];
  }
  emit_insn (gen_rtx_SET (VOIDmode,
	operand2,
	gen_rtx_MEM (QImode,
	operand1)));
  emit_insn (gen_rtx_SET (VOIDmode,
	gen_rtx_MEM (QImode,
	operand0),
	operand2));
  emit (gen_rtx_PARALLEL (VOIDmode,
	gen_rtvec (2,
		gen_rtx_SET (VOIDmode,
	operand0,
	gen_rtx_PLUS (SImode,
	operand0,
	const1_rtx)),
		gen_rtx_CLOBBER (VOIDmode,
	gen_rtx_REG (CCmode,
	17)))));
  emit (gen_rtx_PARALLEL (VOIDmode,
	gen_rtvec (2,
		gen_rtx_SET (VOIDmode,
	operand1,
	gen_rtx_PLUS (SImode,
	operand1,
	const1_rtx)),
		gen_rtx_CLOBBER (VOIDmode,
	gen_rtx_REG (CCmode,
	17)))));
  _val = get_insns ();
  end_sequence ();
  return _val;
}

/* ../../gcc/gcc/config/i386/i386.md:15094 */
rtx
gen_strmovqi_rex64 (operand0, operand1)
     rtx operand0;
     rtx operand1;
{
  rtx operand2;
  rtx _val = 0;
  start_sequence ();
  {
    rtx operands[3];
    operands[0] = operand0;
    operands[1] = operand1;
{
  if (TARGET_SINGLE_STRINGOP || optimize_size)
    {
      emit_insn (gen_strmovqi_rex_1 (operands[0], operands[1], operands[0],
				     operands[1]));
      DONE;
    }
  else 
    operands[2] = gen_reg_rtx (QImode);
}
    operand0 = operands[0];
    operand1 = operands[1];
    operand2 = operands[2];
  }
  emit_insn (gen_rtx_SET (VOIDmode,
	operand2,
	gen_rtx_MEM (QImode,
	operand1)));
  emit_insn (gen_rtx_SET (VOIDmode,
	gen_rtx_MEM (QImode,
	operand0),
	operand2));
  emit (gen_rtx_PARALLEL (VOIDmode,
	gen_rtvec (2,
		gen_rtx_SET (VOIDmode,
	operand0,
	gen_rtx_PLUS (DImode,
	operand0,
	const1_rtx)),
		gen_rtx_CLOBBER (VOIDmode,
	gen_rtx_REG (CCmode,
	17)))));
  emit (gen_rtx_PARALLEL (VOIDmode,
	gen_rtvec (2,
		gen_rtx_SET (VOIDmode,
	operand1,
	gen_rtx_PLUS (DImode,
	operand1,
	const1_rtx)),
		gen_rtx_CLOBBER (VOIDmode,
	gen_rtx_REG (CCmode,
	17)))));
  _val = get_insns ();
  end_sequence ();
  return _val;
}

/* ../../gcc/gcc/config/i386/i386.md:15323 */
rtx
gen_clrstrsi (operand0, operand1, operand2)
     rtx operand0;
     rtx operand1;
     rtx operand2;
{
  rtx _val = 0;
  start_sequence ();
  {
    rtx operands[3];
    operands[0] = operand0;
    operands[1] = operand1;
    operands[2] = operand2;
{
 if (ix86_expand_clrstr (operands[0], operands[1], operands[2]))
   DONE;
 else
   FAIL;
}
    operand0 = operands[0];
    operand1 = operands[1];
    operand2 = operands[2];
  }
  emit_insn (gen_rtx_USE (VOIDmode,
	operand0));
  emit_insn (gen_rtx_USE (VOIDmode,
	operand1));
  emit_insn (gen_rtx_USE (VOIDmode,
	operand2));
  _val = get_insns ();
  end_sequence ();
  return _val;
}

/* ../../gcc/gcc/config/i386/i386.md:15335 */
rtx
gen_clrstrdi (operand0, operand1, operand2)
     rtx operand0;
     rtx operand1;
     rtx operand2;
{
  rtx _val = 0;
  start_sequence ();
  {
    rtx operands[3];
    operands[0] = operand0;
    operands[1] = operand1;
    operands[2] = operand2;
{
 if (ix86_expand_clrstr (operands[0], operands[1], operands[2]))
   DONE;
 else
   FAIL;
}
    operand0 = operands[0];
    operand1 = operands[1];
    operand2 = operands[2];
  }
  emit_insn (gen_rtx_USE (VOIDmode,
	operand0));
  emit_insn (gen_rtx_USE (VOIDmode,
	operand1));
  emit_insn (gen_rtx_USE (VOIDmode,
	operand2));
  _val = get_insns ();
  end_sequence ();
  return _val;
}

/* ../../gcc/gcc/config/i386/i386.md:15350 */
rtx
gen_strsetdi_rex64 (operand0, operand1)
     rtx operand0;
     rtx operand1;
{
  rtx _val = 0;
  start_sequence ();
  {
    rtx operands[2];
    operands[0] = operand0;
    operands[1] = operand1;
{
  if (TARGET_SINGLE_STRINGOP || optimize_size)
    {
      emit_insn (gen_strsetdi_rex_1 (operands[0], operands[0], operands[1]));
      DONE;
    }
}
    operand0 = operands[0];
    operand1 = operands[1];
  }
  emit_insn (gen_rtx_SET (VOIDmode,
	gen_rtx_MEM (DImode,
	operand0),
	operand1));
  emit (gen_rtx_PARALLEL (VOIDmode,
	gen_rtvec (2,
		gen_rtx_SET (VOIDmode,
	operand0,
	gen_rtx_PLUS (DImode,
	operand0,
	GEN_INT (8LL))),
		gen_rtx_CLOBBER (VOIDmode,
	gen_rtx_REG (CCmode,
	17)))));
  _val = get_insns ();
  end_sequence ();
  return _val;
}

/* ../../gcc/gcc/config/i386/i386.md:15364 */
rtx
gen_strsetsi (operand0, operand1)
     rtx operand0;
     rtx operand1;
{
  rtx _val = 0;
  start_sequence ();
  {
    rtx operands[2];
    operands[0] = operand0;
    operands[1] = operand1;
{
  if (TARGET_64BIT)
    {
      emit_insn (gen_strsetsi_rex64 (operands[0], operands[1]));
      DONE;
    }
  else if (TARGET_SINGLE_STRINGOP || optimize_size)
    {
      emit_insn (gen_strsetsi_1 (operands[0], operands[0], operands[1]));
      DONE;
    }
}
    operand0 = operands[0];
    operand1 = operands[1];
  }
  emit_insn (gen_rtx_SET (VOIDmode,
	gen_rtx_MEM (SImode,
	operand0),
	operand1));
  emit (gen_rtx_PARALLEL (VOIDmode,
	gen_rtvec (2,
		gen_rtx_SET (VOIDmode,
	operand0,
	gen_rtx_PLUS (SImode,
	operand0,
	GEN_INT (4LL))),
		gen_rtx_CLOBBER (VOIDmode,
	gen_rtx_REG (CCmode,
	17)))));
  _val = get_insns ();
  end_sequence ();
  return _val;
}

/* ../../gcc/gcc/config/i386/i386.md:15383 */
rtx
gen_strsetsi_rex64 (operand0, operand1)
     rtx operand0;
     rtx operand1;
{
  rtx _val = 0;
  start_sequence ();
  {
    rtx operands[2];
    operands[0] = operand0;
    operands[1] = operand1;
{
  if (TARGET_SINGLE_STRINGOP || optimize_size)
    {
      emit_insn (gen_strsetsi_rex_1 (operands[0], operands[0], operands[1]));
      DONE;
    }
}
    operand0 = operands[0];
    operand1 = operands[1];
  }
  emit_insn (gen_rtx_SET (VOIDmode,
	gen_rtx_MEM (SImode,
	operand0),
	operand1));
  emit (gen_rtx_PARALLEL (VOIDmode,
	gen_rtvec (2,
		gen_rtx_SET (VOIDmode,
	operand0,
	gen_rtx_PLUS (DImode,
	operand0,
	GEN_INT (4LL))),
		gen_rtx_CLOBBER (VOIDmode,
	gen_rtx_REG (CCmode,
	17)))));
  _val = get_insns ();
  end_sequence ();
  return _val;
}

/* ../../gcc/gcc/config/i386/i386.md:15397 */
rtx
gen_strsethi (operand0, operand1)
     rtx operand0;
     rtx operand1;
{
  rtx _val = 0;
  start_sequence ();
  {
    rtx operands[2];
    operands[0] = operand0;
    operands[1] = operand1;
{
  if (TARGET_64BIT)
    {
      emit_insn (gen_strsethi_rex64 (operands[0], operands[1]));
      DONE;
    }
  else if (TARGET_SINGLE_STRINGOP || optimize_size)
    {
      emit_insn (gen_strsethi_1 (operands[0], operands[0], operands[1]));
      DONE;
    }
}
    operand0 = operands[0];
    operand1 = operands[1];
  }
  emit_insn (gen_rtx_SET (VOIDmode,
	gen_rtx_MEM (HImode,
	operand0),
	operand1));
  emit (gen_rtx_PARALLEL (VOIDmode,
	gen_rtvec (2,
		gen_rtx_SET (VOIDmode,
	operand0,
	gen_rtx_PLUS (SImode,
	operand0,
	GEN_INT (2LL))),
		gen_rtx_CLOBBER (VOIDmode,
	gen_rtx_REG (CCmode,
	17)))));
  _val = get_insns ();
  end_sequence ();
  return _val;
}

/* ../../gcc/gcc/config/i386/i386.md:15416 */
rtx
gen_strsethi_rex64 (operand0, operand1)
     rtx operand0;
     rtx operand1;
{
  rtx _val = 0;
  start_sequence ();
  {
    rtx operands[2];
    operands[0] = operand0;
    operands[1] = operand1;
{
  if (TARGET_SINGLE_STRINGOP || optimize_size)
    {
      emit_insn (gen_strsethi_rex_1 (operands[0], operands[0], operands[1]));
      DONE;
    }
}
    operand0 = operands[0];
    operand1 = operands[1];
  }
  emit_insn (gen_rtx_SET (VOIDmode,
	gen_rtx_MEM (HImode,
	operand0),
	operand1));
  emit (gen_rtx_PARALLEL (VOIDmode,
	gen_rtvec (2,
		gen_rtx_SET (VOIDmode,
	operand0,
	gen_rtx_PLUS (DImode,
	operand0,
	GEN_INT (2LL))),
		gen_rtx_CLOBBER (VOIDmode,
	gen_rtx_REG (CCmode,
	17)))));
  _val = get_insns ();
  end_sequence ();
  return _val;
}

/* ../../gcc/gcc/config/i386/i386.md:15430 */
rtx
gen_strsetqi (operand0, operand1)
     rtx operand0;
     rtx operand1;
{
  rtx _val = 0;
  start_sequence ();
  {
    rtx operands[2];
    operands[0] = operand0;
    operands[1] = operand1;
{
  if (TARGET_64BIT)
    {
      emit_insn (gen_strsetqi_rex64 (operands[0], operands[1]));
      DONE;
    }
  else if (TARGET_SINGLE_STRINGOP || optimize_size)
    {
      emit_insn (gen_strsetqi_1 (operands[0], operands[0], operands[1]));
      DONE;
    }
}
    operand0 = operands[0];
    operand1 = operands[1];
  }
  emit_insn (gen_rtx_SET (VOIDmode,
	gen_rtx_MEM (QImode,
	operand0),
	operand1));
  emit (gen_rtx_PARALLEL (VOIDmode,
	gen_rtvec (2,
		gen_rtx_SET (VOIDmode,
	operand0,
	gen_rtx_PLUS (SImode,
	operand0,
	const1_rtx)),
		gen_rtx_CLOBBER (VOIDmode,
	gen_rtx_REG (CCmode,
	17)))));
  _val = get_insns ();
  end_sequence ();
  return _val;
}

/* ../../gcc/gcc/config/i386/i386.md:15449 */
rtx
gen_strsetqi_rex64 (operand0, operand1)
     rtx operand0;
     rtx operand1;
{
  rtx _val = 0;
  start_sequence ();
  {
    rtx operands[2];
    operands[0] = operand0;
    operands[1] = operand1;
{
  if (TARGET_SINGLE_STRINGOP || optimize_size)
    {
      emit_insn (gen_strsetqi_rex_1 (operands[0], operands[0], operands[1]));
      DONE;
    }
}
    operand0 = operands[0];
    operand1 = operands[1];
  }
  emit_insn (gen_rtx_SET (VOIDmode,
	gen_rtx_MEM (QImode,
	operand0),
	operand1));
  emit (gen_rtx_PARALLEL (VOIDmode,
	gen_rtvec (2,
		gen_rtx_SET (VOIDmode,
	operand0,
	gen_rtx_PLUS (DImode,
	operand0,
	const1_rtx)),
		gen_rtx_CLOBBER (VOIDmode,
	gen_rtx_REG (CCmode,
	17)))));
  _val = get_insns ();
  end_sequence ();
  return _val;
}

/* ../../gcc/gcc/config/i386/i386.md:15642 */
rtx
gen_cmpstrsi (operand0, operand1, operand2, operand3, operand4)
     rtx operand0;
     rtx operand1;
     rtx operand2;
     rtx operand3;
     rtx operand4;
{
  rtx _val = 0;
  start_sequence ();
  {
    rtx operands[5];
    operands[0] = operand0;
    operands[1] = operand1;
    operands[2] = operand2;
    operands[3] = operand3;
    operands[4] = operand4;
{
  rtx addr1, addr2, out, outlow, count, countreg, align;

  out = operands[0];
  if (GET_CODE (out) != REG)
    out = gen_reg_rtx (SImode);

  addr1 = copy_to_mode_reg (Pmode, XEXP (operands[1], 0));
  addr2 = copy_to_mode_reg (Pmode, XEXP (operands[2], 0));
  
  count = operands[3];
  countreg = ix86_zero_extend_to_Pmode (count);

  /* %%% Iff we are testing strict equality, we can use known alignment
     to good advantage.  This may be possible with combine, particularly
     once cc0 is dead.  */
  align = operands[4];

  emit_insn (gen_cld ());
  if (GET_CODE (count) == CONST_INT)
    {
      if (INTVAL (count) == 0)
	{
	  emit_move_insn (operands[0], const0_rtx);
	  DONE;
	}
      if (TARGET_64BIT)
	emit_insn (gen_cmpstrqi_nz_rex_1 (addr1, addr2, countreg, align,
					  addr1, addr2, countreg));
      else
	emit_insn (gen_cmpstrqi_nz_1 (addr1, addr2, countreg, align,
				      addr1, addr2, countreg));
    }
  else
    {
      if (TARGET_64BIT)
	{
	  emit_insn (gen_cmpdi_1_rex64 (countreg, countreg));
	  emit_insn (gen_cmpstrqi_rex_1 (addr1, addr2, countreg, align,
					 addr1, addr2, countreg));
	}
      else
	{
	  emit_insn (gen_cmpsi_1 (countreg, countreg));
	  emit_insn (gen_cmpstrqi_1 (addr1, addr2, countreg, align,
				     addr1, addr2, countreg));
	}
    }

  outlow = gen_lowpart (QImode, out);
  emit_insn (gen_cmpintqi (outlow));
  emit_move_insn (out, gen_rtx_SIGN_EXTEND (SImode, outlow));

  if (operands[0] != out)
    emit_move_insn (operands[0], out);

  DONE;
}
    operand0 = operands[0];
    operand1 = operands[1];
    operand2 = operands[2];
    operand3 = operands[3];
    operand4 = operands[4];
  }
  emit_insn (gen_rtx_SET (VOIDmode,
	operand0,
	gen_rtx_COMPARE (SImode,
	operand1,
	operand2)));
  emit_insn (gen_rtx_USE (VOIDmode,
	operand3));
  emit_insn (gen_rtx_USE (VOIDmode,
	operand4));
  _val = get_insns ();
  end_sequence ();
  return _val;
}

/* ../../gcc/gcc/config/i386/i386.md:15710 */
rtx
gen_cmpintqi (operand0)
     rtx operand0;
{
  rtx operand1;
  rtx operand2;
  rtx _val = 0;
  start_sequence ();
  {
    rtx operands[3];
    operands[0] = operand0;
operands[1] = gen_reg_rtx (QImode);
   operands[2] = gen_reg_rtx (QImode);
    operand0 = operands[0];
    operand1 = operands[1];
    operand2 = operands[2];
  }
  emit_insn (gen_rtx_SET (VOIDmode,
	operand1,
	gen_rtx_GTU (QImode,
	gen_rtx_REG (CCmode,
	17),
	const0_rtx)));
  emit_insn (gen_rtx_SET (VOIDmode,
	operand2,
	gen_rtx_LTU (QImode,
	gen_rtx_REG (CCmode,
	17),
	const0_rtx)));
  emit (gen_rtx_PARALLEL (VOIDmode,
	gen_rtvec (2,
		gen_rtx_SET (VOIDmode,
	operand0,
	gen_rtx_MINUS (QImode,
	operand1,
	operand2)),
		gen_rtx_CLOBBER (VOIDmode,
	gen_rtx_REG (CCmode,
	17)))));
  _val = get_insns ();
  end_sequence ();
  return _val;
}

/* ../../gcc/gcc/config/i386/i386.md:15798 */
rtx
gen_strlensi (operand0, operand1, operand2, operand3)
     rtx operand0;
     rtx operand1;
     rtx operand2;
     rtx operand3;
{
  rtx _val = 0;
  start_sequence ();
  {
    rtx operands[4];
    operands[0] = operand0;
    operands[1] = operand1;
    operands[2] = operand2;
    operands[3] = operand3;
{
 if (ix86_expand_strlen (operands[0], operands[1], operands[2], operands[3]))
   DONE;
 else
   FAIL;
}
    operand0 = operands[0];
    operand1 = operands[1];
    operand2 = operands[2];
    operand3 = operands[3];
  }
  emit_insn (gen_rtx_SET (VOIDmode,
	operand0,
	gen_rtx_UNSPEC (SImode,
	gen_rtvec (3,
		operand1,
		operand2,
		operand3),
	20)));
  _val = get_insns ();
  end_sequence ();
  return _val;
}

/* ../../gcc/gcc/config/i386/i386.md:15811 */
rtx
gen_strlendi (operand0, operand1, operand2, operand3)
     rtx operand0;
     rtx operand1;
     rtx operand2;
     rtx operand3;
{
  rtx _val = 0;
  start_sequence ();
  {
    rtx operands[4];
    operands[0] = operand0;
    operands[1] = operand1;
    operands[2] = operand2;
    operands[3] = operand3;
{
 if (ix86_expand_strlen (operands[0], operands[1], operands[2], operands[3]))
   DONE;
 else
   FAIL;
}
    operand0 = operands[0];
    operand1 = operands[1];
    operand2 = operands[2];
    operand3 = operands[3];
  }
  emit_insn (gen_rtx_SET (VOIDmode,
	operand0,
	gen_rtx_UNSPEC (DImode,
	gen_rtvec (3,
		operand1,
		operand2,
		operand3),
	20)));
  _val = get_insns ();
  end_sequence ();
  return _val;
}

/* ../../gcc/gcc/config/i386/i386.md:15868 */
extern rtx gen_peephole2_1420 PARAMS ((rtx, rtx *));
rtx
gen_peephole2_1420 (curr_insn, operands)
     rtx curr_insn ATTRIBUTE_UNUSED;
     rtx *operands;
{
  rtx operand0;
  rtx operand1;
  rtx operand2;
  rtx operand3;
  rtx operand4;
  rtx operand5;
  rtx operand6;
  rtx _val = 0;
  HARD_REG_SET _regs_allocated;
  CLEAR_HARD_REG_SET (_regs_allocated);
  start_sequence ();

  operand0 = operands[0];
  operand1 = operands[1];
  operand2 = operands[2];
  operand3 = operands[3];
  operand4 = operands[4];
  operand5 = operands[5];
  operand6 = operands[6];
  emit (gen_rtx_PARALLEL (VOIDmode,
	gen_rtvec (7,
		gen_rtx_SET (VOIDmode,
	gen_rtx_REG (CCmode,
	17),
	gen_rtx_COMPARE (CCmode,
	gen_rtx_MEM (BLKmode,
	operand4),
	gen_rtx_MEM (BLKmode,
	operand5))),
		gen_rtx_USE (VOIDmode,
	operand6),
		gen_rtx_USE (VOIDmode,
	operand3),
		gen_rtx_USE (VOIDmode,
	gen_rtx_REG (SImode,
	19)),
		gen_rtx_CLOBBER (VOIDmode,
	operand0),
		gen_rtx_CLOBBER (VOIDmode,
	operand1),
		gen_rtx_CLOBBER (VOIDmode,
	operand2))));
  _val = get_insns ();
  end_sequence ();
  return _val;
}

/* ../../gcc/gcc/config/i386/i386.md:15900 */
extern rtx gen_peephole2_1421 PARAMS ((rtx, rtx *));
rtx
gen_peephole2_1421 (curr_insn, operands)
     rtx curr_insn ATTRIBUTE_UNUSED;
     rtx *operands;
{
  rtx operand0;
  rtx operand1;
  rtx operand2;
  rtx operand3;
  rtx operand4;
  rtx operand5;
  rtx operand6;
  rtx _val = 0;
  HARD_REG_SET _regs_allocated;
  CLEAR_HARD_REG_SET (_regs_allocated);
  start_sequence ();

  operand0 = operands[0];
  operand1 = operands[1];
  operand2 = operands[2];
  operand3 = operands[3];
  operand4 = operands[4];
  operand5 = operands[5];
  operand6 = operands[6];
  emit (gen_rtx_PARALLEL (VOIDmode,
	gen_rtvec (7,
		gen_rtx_SET (VOIDmode,
	gen_rtx_REG (CCmode,
	17),
	gen_rtx_IF_THEN_ELSE (CCmode,
	gen_rtx_NE (VOIDmode,
	operand6,
	const0_rtx),
	gen_rtx_COMPARE (CCmode,
	gen_rtx_MEM (BLKmode,
	operand4),
	gen_rtx_MEM (BLKmode,
	operand5)),
	const0_rtx)),
		gen_rtx_USE (VOIDmode,
	operand3),
		gen_rtx_USE (VOIDmode,
	gen_rtx_REG (CCmode,
	17)),
		gen_rtx_USE (VOIDmode,
	gen_rtx_REG (SImode,
	19)),
		gen_rtx_CLOBBER (VOIDmode,
	operand0),
		gen_rtx_CLOBBER (VOIDmode,
	operand1),
		gen_rtx_CLOBBER (VOIDmode,
	operand2))));
  _val = get_insns ();
  end_sequence ();
  return _val;
}

/* ../../gcc/gcc/config/i386/i386.md:15941 */
rtx
gen_movdicc (operand0, operand1, operand2, operand3)
     rtx operand0;
     rtx operand1;
     rtx operand2;
     rtx operand3;
{
  rtx _val = 0;
  start_sequence ();
  {
    rtx operands[4];
    operands[0] = operand0;
    operands[1] = operand1;
    operands[2] = operand2;
    operands[3] = operand3;
if (!ix86_expand_int_movcc (operands)) FAIL; DONE;
    operand0 = operands[0];
    operand1 = operands[1];
    operand2 = operands[2];
    operand3 = operands[3];
  }
  emit_insn (gen_rtx_SET (VOIDmode,
	operand0,
	gen_rtx_IF_THEN_ELSE (DImode,
	operand1,
	operand2,
	operand3)));
  _val = get_insns ();
  end_sequence ();
  return _val;
}

/* ../../gcc/gcc/config/i386/i386.md:15980 */
rtx
gen_movsicc (operand0, operand1, operand2, operand3)
     rtx operand0;
     rtx operand1;
     rtx operand2;
     rtx operand3;
{
  rtx _val = 0;
  start_sequence ();
  {
    rtx operands[4];
    operands[0] = operand0;
    operands[1] = operand1;
    operands[2] = operand2;
    operands[3] = operand3;
if (!ix86_expand_int_movcc (operands)) FAIL; DONE;
    operand0 = operands[0];
    operand1 = operands[1];
    operand2 = operands[2];
    operand3 = operands[3];
  }
  emit_insn (gen_rtx_SET (VOIDmode,
	operand0,
	gen_rtx_IF_THEN_ELSE (SImode,
	operand1,
	operand2,
	operand3)));
  _val = get_insns ();
  end_sequence ();
  return _val;
}

/* ../../gcc/gcc/config/i386/i386.md:16023 */
rtx
gen_movhicc (operand0, operand1, operand2, operand3)
     rtx operand0;
     rtx operand1;
     rtx operand2;
     rtx operand3;
{
  rtx _val = 0;
  start_sequence ();
  {
    rtx operands[4];
    operands[0] = operand0;
    operands[1] = operand1;
    operands[2] = operand2;
    operands[3] = operand3;
if (!ix86_expand_int_movcc (operands)) FAIL; DONE;
    operand0 = operands[0];
    operand1 = operands[1];
    operand2 = operands[2];
    operand3 = operands[3];
  }
  emit_insn (gen_rtx_SET (VOIDmode,
	operand0,
	gen_rtx_IF_THEN_ELSE (HImode,
	operand1,
	operand2,
	operand3)));
  _val = get_insns ();
  end_sequence ();
  return _val;
}

/* ../../gcc/gcc/config/i386/i386.md:16045 */
rtx
gen_movsfcc (operand0, operand1, operand2, operand3)
     rtx operand0;
     rtx operand1;
     rtx operand2;
     rtx operand3;
{
  rtx _val = 0;
  start_sequence ();
  {
    rtx operands[4];
    operands[0] = operand0;
    operands[1] = operand1;
    operands[2] = operand2;
    operands[3] = operand3;
if (! ix86_expand_fp_movcc (operands)) FAIL; DONE;
    operand0 = operands[0];
    operand1 = operands[1];
    operand2 = operands[2];
    operand3 = operands[3];
  }
  emit_insn (gen_rtx_SET (VOIDmode,
	operand0,
	gen_rtx_IF_THEN_ELSE (SFmode,
	operand1,
	operand2,
	operand3)));
  _val = get_insns ();
  end_sequence ();
  return _val;
}

/* ../../gcc/gcc/config/i386/i386.md:16069 */
rtx
gen_movdfcc (operand0, operand1, operand2, operand3)
     rtx operand0;
     rtx operand1;
     rtx operand2;
     rtx operand3;
{
  rtx _val = 0;
  start_sequence ();
  {
    rtx operands[4];
    operands[0] = operand0;
    operands[1] = operand1;
    operands[2] = operand2;
    operands[3] = operand3;
if (! ix86_expand_fp_movcc (operands)) FAIL; DONE;
    operand0 = operands[0];
    operand1 = operands[1];
    operand2 = operands[2];
    operand3 = operands[3];
  }
  emit_insn (gen_rtx_SET (VOIDmode,
	operand0,
	gen_rtx_IF_THEN_ELSE (DFmode,
	operand1,
	operand2,
	operand3)));
  _val = get_insns ();
  end_sequence ();
  return _val;
}

/* ../../gcc/gcc/config/i386/i386.md:16109 */
extern rtx gen_split_1427 PARAMS ((rtx *));
rtx
gen_split_1427 (operands)
      rtx *operands;
{
  rtx operand0;
  rtx operand1;
  rtx operand2;
  rtx operand3;
  rtx operand4;
  rtx operand5;
  rtx operand6;
  rtx operand7;
  rtx operand8;
  rtx _val = 0;
  start_sequence ();
split_di (operands+2, 1, operands+5, operands+6);
   split_di (operands+3, 1, operands+7, operands+8);
   split_di (operands, 1, operands+2, operands+3);
  operand0 = operands[0];
  operand1 = operands[1];
  operand2 = operands[2];
  operand3 = operands[3];
  operand4 = operands[4];
  operand5 = operands[5];
  operand6 = operands[6];
  operand7 = operands[7];
  operand8 = operands[8];
  emit_insn (gen_rtx_SET (VOIDmode,
	operand2,
	gen_rtx_IF_THEN_ELSE (SImode,
	gen_rtx (GET_CODE (operand1), GET_MODE (operand1),
		operand4,
		const0_rtx),
	operand5,
	operand7)));
  emit_insn (gen_rtx_SET (VOIDmode,
	operand3,
	gen_rtx_IF_THEN_ELSE (SImode,
	gen_rtx (GET_CODE (operand1), GET_MODE (operand1),
		copy_rtx (operand4),
		const0_rtx),
	operand6,
	operand8)));
  _val = get_insns ();
  end_sequence ();
  return _val;
}

/* ../../gcc/gcc/config/i386/i386.md:16128 */
rtx
gen_movxfcc (operand0, operand1, operand2, operand3)
     rtx operand0;
     rtx operand1;
     rtx operand2;
     rtx operand3;
{
  rtx _val = 0;
  start_sequence ();
  {
    rtx operands[4];
    operands[0] = operand0;
    operands[1] = operand1;
    operands[2] = operand2;
    operands[3] = operand3;
if (! ix86_expand_fp_movcc (operands)) FAIL; DONE;
    operand0 = operands[0];
    operand1 = operands[1];
    operand2 = operands[2];
    operand3 = operands[3];
  }
  emit_insn (gen_rtx_SET (VOIDmode,
	operand0,
	gen_rtx_IF_THEN_ELSE (XFmode,
	operand1,
	operand2,
	operand3)));
  _val = get_insns ();
  end_sequence ();
  return _val;
}

/* ../../gcc/gcc/config/i386/i386.md:16136 */
rtx
gen_movtfcc (operand0, operand1, operand2, operand3)
     rtx operand0;
     rtx operand1;
     rtx operand2;
     rtx operand3;
{
  rtx _val = 0;
  start_sequence ();
  {
    rtx operands[4];
    operands[0] = operand0;
    operands[1] = operand1;
    operands[2] = operand2;
    operands[3] = operand3;
if (! ix86_expand_fp_movcc (operands)) FAIL; DONE;
    operand0 = operands[0];
    operand1 = operands[1];
    operand2 = operands[2];
    operand3 = operands[3];
  }
  emit_insn (gen_rtx_SET (VOIDmode,
	operand0,
	gen_rtx_IF_THEN_ELSE (TFmode,
	operand1,
	operand2,
	operand3)));
  _val = get_insns ();
  end_sequence ();
  return _val;
}

/* ../../gcc/gcc/config/i386/i386.md:16170 */
rtx
gen_minsf3 (operand0, operand1, operand2)
     rtx operand0;
     rtx operand1;
     rtx operand2;
{
  return gen_rtx_PARALLEL (VOIDmode,
	gen_rtvec (2,
		gen_rtx_SET (VOIDmode,
	operand0,
	gen_rtx_IF_THEN_ELSE (SFmode,
	gen_rtx_LT (VOIDmode,
	operand1,
	operand2),
	operand1,
	operand2)),
		gen_rtx_CLOBBER (VOIDmode,
	gen_rtx_REG (CCmode,
	17))));
}

/* ../../gcc/gcc/config/i386/i386.md:16202 */
extern rtx gen_split_1431 PARAMS ((rtx *));
rtx
gen_split_1431 (operands)
      rtx *operands;
{
  rtx operand0;
  rtx operand1;
  rtx operand2;
  rtx _val = 0;
  start_sequence ();
  operand0 = operands[0];
  operand1 = operands[1];
  operand2 = operands[2];
  emit_insn (gen_rtx_SET (VOIDmode,
	operand0,
	gen_rtx_IF_THEN_ELSE (SFmode,
	gen_rtx_LT (VOIDmode,
	operand1,
	operand2),
	copy_rtx (operand1),
	copy_rtx (operand2))));
  _val = get_insns ();
  end_sequence ();
  return _val;
}

/* ../../gcc/gcc/config/i386/i386.md:16222 */
extern rtx gen_split_1432 PARAMS ((rtx *));
rtx
gen_split_1432 (operands)
      rtx *operands;
{
  rtx operand0;
  rtx operand1;
  rtx operand2;
  rtx _val = 0;
  start_sequence ();
  operand0 = operands[0];
  operand1 = operands[1];
  operand2 = operands[2];
  emit_insn (gen_rtx_SET (VOIDmode,
	gen_rtx_REG (CCFPmode,
	17),
	gen_rtx_COMPARE (CCFPmode,
	operand2,
	operand1)));
  emit_insn (gen_rtx_SET (VOIDmode,
	operand0,
	gen_rtx_IF_THEN_ELSE (SFmode,
	gen_rtx_GE (VOIDmode,
	gen_rtx_REG (CCFPmode,
	17),
	const0_rtx),
	copy_rtx (operand1),
	copy_rtx (operand2))));
  _val = get_insns ();
  end_sequence ();
  return _val;
}

/* ../../gcc/gcc/config/i386/i386.md:16253 */
rtx
gen_mindf3 (operand0, operand1, operand2)
     rtx operand0;
     rtx operand1;
     rtx operand2;
{
  rtx _val = 0;
  start_sequence ();
  {
    rtx operands[3];
    operands[0] = operand0;
    operands[1] = operand1;
    operands[2] = operand2;
#
    operand0 = operands[0];
    operand1 = operands[1];
    operand2 = operands[2];
  }
  emit (gen_rtx_PARALLEL (VOIDmode,
	gen_rtvec (2,
		gen_rtx_SET (VOIDmode,
	operand0,
	gen_rtx_IF_THEN_ELSE (DFmode,
	gen_rtx_LT (VOIDmode,
	operand1,
	operand2),
	operand1,
	operand2)),
		gen_rtx_CLOBBER (VOIDmode,
	gen_rtx_REG (CCmode,
	17)))));
  _val = get_insns ();
  end_sequence ();
  return _val;
}

/* ../../gcc/gcc/config/i386/i386.md:16285 */
extern rtx gen_split_1434 PARAMS ((rtx *));
rtx
gen_split_1434 (operands)
      rtx *operands;
{
  rtx operand0;
  rtx operand1;
  rtx operand2;
  rtx _val = 0;
  start_sequence ();
  operand0 = operands[0];
  operand1 = operands[1];
  operand2 = operands[2];
  emit_insn (gen_rtx_SET (VOIDmode,
	operand0,
	gen_rtx_IF_THEN_ELSE (DFmode,
	gen_rtx_LT (VOIDmode,
	operand1,
	operand2),
	copy_rtx (operand1),
	copy_rtx (operand2))));
  _val = get_insns ();
  end_sequence ();
  return _val;
}

/* ../../gcc/gcc/config/i386/i386.md:16304 */
extern rtx gen_split_1435 PARAMS ((rtx *));
rtx
gen_split_1435 (operands)
      rtx *operands;
{
  rtx operand0;
  rtx operand1;
  rtx operand2;
  rtx _val = 0;
  start_sequence ();
  operand0 = operands[0];
  operand1 = operands[1];
  operand2 = operands[2];
  emit_insn (gen_rtx_SET (VOIDmode,
	gen_rtx_REG (CCFPmode,
	17),
	gen_rtx_COMPARE (CCFPmode,
	operand2,
	copy_rtx (operand2))));
  emit_insn (gen_rtx_SET (VOIDmode,
	operand0,
	gen_rtx_IF_THEN_ELSE (DFmode,
	gen_rtx_GE (VOIDmode,
	gen_rtx_REG (CCFPmode,
	17),
	const0_rtx),
	operand1,
	copy_rtx (operand2))));
  _val = get_insns ();
  end_sequence ();
  return _val;
}

/* ../../gcc/gcc/config/i386/i386.md:16335 */
rtx
gen_maxsf3 (operand0, operand1, operand2)
     rtx operand0;
     rtx operand1;
     rtx operand2;
{
  rtx _val = 0;
  start_sequence ();
  {
    rtx operands[3];
    operands[0] = operand0;
    operands[1] = operand1;
    operands[2] = operand2;
#
    operand0 = operands[0];
    operand1 = operands[1];
    operand2 = operands[2];
  }
  emit (gen_rtx_PARALLEL (VOIDmode,
	gen_rtvec (2,
		gen_rtx_SET (VOIDmode,
	operand0,
	gen_rtx_IF_THEN_ELSE (SFmode,
	gen_rtx_GT (VOIDmode,
	operand1,
	operand2),
	operand1,
	operand2)),
		gen_rtx_CLOBBER (VOIDmode,
	gen_rtx_REG (CCmode,
	17)))));
  _val = get_insns ();
  end_sequence ();
  return _val;
}

/* ../../gcc/gcc/config/i386/i386.md:16367 */
extern rtx gen_split_1437 PARAMS ((rtx *));
rtx
gen_split_1437 (operands)
      rtx *operands;
{
  rtx operand0;
  rtx operand1;
  rtx operand2;
  rtx _val = 0;
  start_sequence ();
  operand0 = operands[0];
  operand1 = operands[1];
  operand2 = operands[2];
  emit_insn (gen_rtx_SET (VOIDmode,
	operand0,
	gen_rtx_IF_THEN_ELSE (SFmode,
	gen_rtx_GT (VOIDmode,
	operand1,
	operand2),
	copy_rtx (operand1),
	copy_rtx (operand2))));
  _val = get_insns ();
  end_sequence ();
  return _val;
}

/* ../../gcc/gcc/config/i386/i386.md:16385 */
extern rtx gen_split_1438 PARAMS ((rtx *));
rtx
gen_split_1438 (operands)
      rtx *operands;
{
  rtx operand0;
  rtx operand1;
  rtx operand2;
  rtx _val = 0;
  start_sequence ();
  operand0 = operands[0];
  operand1 = operands[1];
  operand2 = operands[2];
  emit_insn (gen_rtx_SET (VOIDmode,
	gen_rtx_REG (CCFPmode,
	17),
	gen_rtx_COMPARE (CCFPmode,
	operand1,
	operand2)));
  emit_insn (gen_rtx_SET (VOIDmode,
	operand0,
	gen_rtx_IF_THEN_ELSE (SFmode,
	gen_rtx_GT (VOIDmode,
	gen_rtx_REG (CCFPmode,
	17),
	const0_rtx),
	copy_rtx (operand1),
	copy_rtx (operand2))));
  _val = get_insns ();
  end_sequence ();
  return _val;
}

/* ../../gcc/gcc/config/i386/i386.md:16416 */
rtx
gen_maxdf3 (operand0, operand1, operand2)
     rtx operand0;
     rtx operand1;
     rtx operand2;
{
  rtx _val = 0;
  start_sequence ();
  {
    rtx operands[3];
    operands[0] = operand0;
    operands[1] = operand1;
    operands[2] = operand2;
#
    operand0 = operands[0];
    operand1 = operands[1];
    operand2 = operands[2];
  }
  emit (gen_rtx_PARALLEL (VOIDmode,
	gen_rtvec (2,
		gen_rtx_SET (VOIDmode,
	operand0,
	gen_rtx_IF_THEN_ELSE (DFmode,
	gen_rtx_GT (VOIDmode,
	operand1,
	operand2),
	operand1,
	operand2)),
		gen_rtx_CLOBBER (VOIDmode,
	gen_rtx_REG (CCmode,
	17)))));
  _val = get_insns ();
  end_sequence ();
  return _val;
}

/* ../../gcc/gcc/config/i386/i386.md:16448 */
extern rtx gen_split_1440 PARAMS ((rtx *));
rtx
gen_split_1440 (operands)
      rtx *operands;
{
  rtx operand0;
  rtx operand1;
  rtx operand2;
  rtx _val = 0;
  start_sequence ();
  operand0 = operands[0];
  operand1 = operands[1];
  operand2 = operands[2];
  emit_insn (gen_rtx_SET (VOIDmode,
	operand0,
	gen_rtx_IF_THEN_ELSE (DFmode,
	gen_rtx_GT (VOIDmode,
	operand1,
	operand2),
	copy_rtx (operand1),
	copy_rtx (operand2))));
  _val = get_insns ();
  end_sequence ();
  return _val;
}

/* ../../gcc/gcc/config/i386/i386.md:16466 */
extern rtx gen_split_1441 PARAMS ((rtx *));
rtx
gen_split_1441 (operands)
      rtx *operands;
{
  rtx operand0;
  rtx operand1;
  rtx operand2;
  rtx _val = 0;
  start_sequence ();
  operand0 = operands[0];
  operand1 = operands[1];
  operand2 = operands[2];
  emit_insn (gen_rtx_SET (VOIDmode,
	gen_rtx_REG (CCFPmode,
	17),
	gen_rtx_COMPARE (CCFPmode,
	operand1,
	operand2)));
  emit_insn (gen_rtx_SET (VOIDmode,
	operand0,
	gen_rtx_IF_THEN_ELSE (DFmode,
	gen_rtx_GT (VOIDmode,
	gen_rtx_REG (CCFPmode,
	17),
	const0_rtx),
	copy_rtx (operand1),
	copy_rtx (operand2))));
  _val = get_insns ();
  end_sequence ();
  return _val;
}

/* ../../gcc/gcc/config/i386/i386.md:16508 */
rtx
gen_pro_epilogue_adjust_stack (operand0, operand1, operand2)
     rtx operand0;
     rtx operand1;
     rtx operand2;
{
  rtx _val = 0;
  start_sequence ();
  {
    rtx operands[3];
    operands[0] = operand0;
    operands[1] = operand1;
    operands[2] = operand2;
{
  if (TARGET_64BIT)
    {
      emit_insn (gen_pro_epilogue_adjust_stack_rex64
		 (operands[0], operands[1], operands[2]));
      DONE;
    }
}
    operand0 = operands[0];
    operand1 = operands[1];
    operand2 = operands[2];
  }
  emit (gen_rtx_PARALLEL (VOIDmode,
	gen_rtvec (3,
		gen_rtx_SET (VOIDmode,
	operand0,
	gen_rtx_PLUS (SImode,
	operand1,
	operand2)),
		gen_rtx_CLOBBER (VOIDmode,
	gen_rtx_REG (CCmode,
	17)),
		gen_rtx_CLOBBER (VOIDmode,
	gen_rtx_MEM (BLKmode,
	gen_rtx_SCRATCH (VOIDmode))))));
  _val = get_insns ();
  end_sequence ();
  return _val;
}

/* ../../gcc/gcc/config/i386/i386.md:16670 */
extern rtx gen_split_1443 PARAMS ((rtx *));
rtx
gen_split_1443 (operands)
      rtx *operands ATTRIBUTE_UNUSED;
{
  rtx _val = 0;
  start_sequence ();
{
   ix86_compare_op0 = operands[5];
   ix86_compare_op1 = operands[4];
   operands[1] = gen_rtx_fmt_ee (swap_condition (GET_CODE (operands[1])),
				 VOIDmode, operands[5], operands[4]);
   ix86_expand_fp_movcc (operands);
   DONE;
}
  emit_insn (const0_rtx);
  _val = get_insns ();
  end_sequence ();
  return _val;
}

/* ../../gcc/gcc/config/i386/i386.md:16696 */
extern rtx gen_split_1444 PARAMS ((rtx *));
rtx
gen_split_1444 (operands)
      rtx *operands;
{
  rtx operand0;
  rtx operand1;
  rtx operand2;
  rtx operand3;
  rtx operand4;
  rtx operand5;
  rtx operand6;
  rtx operand7;
  rtx _val = 0;
  start_sequence ();
{
  /* If op2 == op3, op3 would be clobbered before it is used.  */
  if (operands_match_p (operands[2], operands[3]))
    {
      emit_move_insn (operands[0], operands[2]);
      DONE;
    }
  PUT_MODE (operands[1], GET_MODE (operands[0]));
  if (operands_match_p (operands[0], operands[4]))
    operands[6] = operands[4], operands[7] = operands[2];
  else
    operands[6] = operands[2], operands[7] = operands[4];
}
  operand0 = operands[0];
  operand1 = operands[1];
  operand2 = operands[2];
  operand3 = operands[3];
  operand4 = operands[4];
  operand5 = operands[5];
  operand6 = operands[6];
  operand7 = operands[7];
  emit_insn (gen_rtx_SET (VOIDmode,
	operand4,
	gen_rtx (GET_CODE (operand1), GET_MODE (operand1),
		copy_rtx (operand4),
		operand5)));
  emit_insn (gen_rtx_SET (VOIDmode,
	gen_rtx_SUBREG (TImode,
	operand2,
	0),
	gen_rtx_AND (TImode,
	gen_rtx_SUBREG (TImode,
	copy_rtx (operand2),
	0),
	gen_rtx_SUBREG (TImode,
	copy_rtx (operand4),
	0))));
  emit_insn (gen_rtx_SET (VOIDmode,
	gen_rtx_SUBREG (TImode,
	copy_rtx (operand4),
	0),
	gen_rtx_AND (TImode,
	gen_rtx_NOT (TImode,
	gen_rtx_SUBREG (TImode,
	copy_rtx (operand4),
	0)),
	gen_rtx_SUBREG (TImode,
	operand3,
	0))));
  emit_insn (gen_rtx_SET (VOIDmode,
	gen_rtx_SUBREG (TImode,
	operand0,
	0),
	gen_rtx_IOR (TImode,
	gen_rtx_SUBREG (TImode,
	operand6,
	0),
	gen_rtx_SUBREG (TImode,
	operand7,
	0))));
  _val = get_insns ();
  end_sequence ();
  return _val;
}

/* ../../gcc/gcc/config/i386/i386.md:16810 */
extern rtx gen_split_1445 PARAMS ((rtx *));
rtx
gen_split_1445 (operands)
      rtx *operands;
{
  rtx operand0;
  rtx operand1;
  rtx operand2;
  rtx operand3;
  rtx operand4;
  rtx operand5;
  rtx operand6;
  rtx operand7;
  rtx _val = 0;
  start_sequence ();
{
  PUT_MODE (operands[1], GET_MODE (operands[0]));
  if (!sse_comparison_operator (operands[1], VOIDmode)
      || !rtx_equal_p (operands[0], operands[4]))
    {
      rtx tmp = operands[5];
      operands[5] = operands[4];
      operands[4] = tmp;
      PUT_CODE (operands[1], swap_condition (GET_CODE (operands[1])));
    }
  if (!rtx_equal_p (operands[0], operands[4]))
    abort ();
  if (const0_operand (operands[2], GET_MODE (operands[0])))
    {
      operands[7] = operands[3];
      operands[6] = gen_rtx_NOT (TImode, gen_rtx_SUBREG (TImode, operands[0],
							 0));
    }
  else
    {
      operands[7] = operands[2];
      operands[6] = gen_rtx_SUBREG (TImode, operands[0], 0);
    }
}
  operand0 = operands[0];
  operand1 = operands[1];
  operand2 = operands[2];
  operand3 = operands[3];
  operand4 = operands[4];
  operand5 = operands[5];
  operand6 = operands[6];
  operand7 = operands[7];
  emit_insn (gen_rtx_SET (VOIDmode,
	operand0,
	gen_rtx (GET_CODE (operand1), GET_MODE (operand1),
		copy_rtx (operand0),
		operand5)));
  emit_insn (gen_rtx_SET (VOIDmode,
	gen_rtx_SUBREG (TImode,
	copy_rtx (operand0),
	0),
	gen_rtx_AND (TImode,
	operand6,
	gen_rtx_SUBREG (TImode,
	operand7,
	0))));
  _val = get_insns ();
  end_sequence ();
  return _val;
}

/* ../../gcc/gcc/config/i386/i386.md:16848 */
rtx
gen_allocate_stack_worker (operand0)
     rtx operand0;
{
  rtx _val = 0;
  start_sequence ();
  {
    rtx operands[1];
    operands[0] = operand0;
{
  if (TARGET_64BIT)
    emit_insn (gen_allocate_stack_worker_rex64 (operands[0]));
  else
    emit_insn (gen_allocate_stack_worker_1 (operands[0]));
  DONE;
}
    operand0 = operands[0];
  }
  emit (operand0);
  _val = get_insns ();
  end_sequence ();
  return _val;
}

/* ../../gcc/gcc/config/i386/i386.md:16879 */
rtx
gen_allocate_stack (operand0, operand1)
     rtx operand0;
     rtx operand1;
{
  rtx _val = 0;
  start_sequence ();
  {
    rtx operands[2];
    operands[0] = operand0;
    operands[1] = operand1;
{
#ifdef CHECK_STACK_LIMIT
  if (GET_CODE (operands[1]) == CONST_INT
      && INTVAL (operands[1]) < CHECK_STACK_LIMIT)
    emit_insn (gen_subsi3 (stack_pointer_rtx, stack_pointer_rtx,
			   operands[1]));
  else 
#endif
    emit_insn (gen_allocate_stack_worker (copy_to_mode_reg (SImode,
							    operands[1])));

  emit_move_insn (operands[0], virtual_stack_dynamic_rtx);
  DONE;
}
    operand0 = operands[0];
    operand1 = operands[1];
  }
  emit (gen_rtx_PARALLEL (VOIDmode,
	gen_rtvec (2,
		gen_rtx_SET (VOIDmode,
	operand0,
	gen_rtx_MINUS (SImode,
	gen_rtx_REG (SImode,
	7),
	operand1)),
		gen_rtx_CLOBBER (VOIDmode,
	gen_rtx_REG (CCmode,
	17)))));
  emit (gen_rtx_PARALLEL (VOIDmode,
	gen_rtvec (2,
		gen_rtx_SET (VOIDmode,
	gen_rtx_REG (SImode,
	7),
	gen_rtx_MINUS (SImode,
	gen_rtx_REG (SImode,
	7),
	operand1)),
		gen_rtx_CLOBBER (VOIDmode,
	gen_rtx_REG (CCmode,
	17)))));
  _val = get_insns ();
  end_sequence ();
  return _val;
}

/* ../../gcc/gcc/config/i386/i386.md:16903 */
rtx
gen_builtin_setjmp_receiver (operand0)
     rtx operand0;
{
  rtx _val = 0;
  start_sequence ();
  {
    rtx operands[1];
    operands[0] = operand0;
{
  emit_insn (gen_set_got (pic_offset_table_rtx));
  DONE;
}
    operand0 = operands[0];
  }
  emit_insn (gen_rtx_LABEL_REF (VOIDmode,
	operand0));
  _val = get_insns ();
  end_sequence ();
  return _val;
}

/* ../../gcc/gcc/config/i386/i386.md:16913 */
extern rtx gen_split_1449 PARAMS ((rtx *));
rtx
gen_split_1449 (operands)
      rtx *operands;
{
  rtx operand0;
  rtx operand1;
  rtx operand2;
  rtx operand3;
  rtx _val = 0;
  start_sequence ();
operands[0] = gen_lowpart (SImode, operands[0]);
   operands[1] = gen_lowpart (SImode, operands[1]);
   if (GET_CODE (operands[3]) != ASHIFT)
     operands[2] = gen_lowpart (SImode, operands[2]);
   PUT_MODE (operands[3], SImode);
  operand0 = operands[0];
  operand1 = operands[1];
  operand2 = operands[2];
  operand3 = operands[3];
  emit (gen_rtx_PARALLEL (VOIDmode,
	gen_rtvec (2,
		gen_rtx_SET (VOIDmode,
	operand0,
	gen_rtx (GET_CODE (operand3), GET_MODE (operand3),
		operand1,
		operand2)),
		gen_rtx_CLOBBER (VOIDmode,
	gen_rtx_REG (CCmode,
	17)))));
  _val = get_insns ();
  end_sequence ();
  return _val;
}

/* ../../gcc/gcc/config/i386/i386.md:16939 */
extern rtx gen_split_1450 PARAMS ((rtx *));
rtx
gen_split_1450 (operands)
      rtx *operands;
{
  rtx operand0;
  rtx operand1;
  rtx operand2;
  rtx _val = 0;
  start_sequence ();
operands[2]
     = gen_int_mode (INTVAL (operands[2])
		     & GET_MODE_MASK (GET_MODE (operands[0])),
		     SImode);
   operands[0] = gen_lowpart (SImode, operands[0]);
   operands[1] = gen_lowpart (SImode, operands[1]);
  operand0 = operands[0];
  operand1 = operands[1];
  operand2 = operands[2];
  emit (gen_rtx_PARALLEL (VOIDmode,
	gen_rtvec (2,
		gen_rtx_SET (VOIDmode,
	gen_rtx_REG (CCNOmode,
	17),
	gen_rtx_COMPARE (CCNOmode,
	gen_rtx_AND (SImode,
	operand1,
	operand2),
	const0_rtx)),
		gen_rtx_SET (VOIDmode,
	operand0,
	gen_rtx_AND (SImode,
	copy_rtx (operand1),
	copy_rtx (operand2))))));
  _val = get_insns ();
  end_sequence ();
  return _val;
}

/* ../../gcc/gcc/config/i386/i386.md:16968 */
extern rtx gen_split_1451 PARAMS ((rtx *));
rtx
gen_split_1451 (operands)
      rtx *operands;
{
  rtx operand0;
  rtx operand1;
  rtx _val = 0;
  start_sequence ();
operands[1]
     = gen_int_mode (INTVAL (operands[1])
		     & GET_MODE_MASK (GET_MODE (operands[0])),
		     SImode);
   operands[0] = gen_lowpart (SImode, operands[0]);
  operand0 = operands[0];
  operand1 = operands[1];
  emit_insn (gen_rtx_SET (VOIDmode,
	gen_rtx_REG (CCNOmode,
	17),
	gen_rtx_COMPARE (CCNOmode,
	gen_rtx_AND (SImode,
	operand0,
	operand1),
	const0_rtx)));
  _val = get_insns ();
  end_sequence ();
  return _val;
}

/* ../../gcc/gcc/config/i386/i386.md:16987 */
extern rtx gen_split_1452 PARAMS ((rtx *));
rtx
gen_split_1452 (operands)
      rtx *operands;
{
  rtx operand0;
  rtx operand1;
  rtx _val = 0;
  start_sequence ();
operands[0] = gen_lowpart (SImode, operands[0]);
   operands[1] = gen_lowpart (SImode, operands[1]);
  operand0 = operands[0];
  operand1 = operands[1];
  emit (gen_rtx_PARALLEL (VOIDmode,
	gen_rtvec (2,
		gen_rtx_SET (VOIDmode,
	operand0,
	gen_rtx_NEG (SImode,
	operand1)),
		gen_rtx_CLOBBER (VOIDmode,
	gen_rtx_REG (CCmode,
	17)))));
  _val = get_insns ();
  end_sequence ();
  return _val;
}

/* ../../gcc/gcc/config/i386/i386.md:17001 */
extern rtx gen_split_1453 PARAMS ((rtx *));
rtx
gen_split_1453 (operands)
      rtx *operands;
{
  rtx operand0;
  rtx operand1;
  rtx _val = 0;
  start_sequence ();
operands[0] = gen_lowpart (SImode, operands[0]);
   operands[1] = gen_lowpart (SImode, operands[1]);
  operand0 = operands[0];
  operand1 = operands[1];
  emit_insn (gen_rtx_SET (VOIDmode,
	operand0,
	gen_rtx_NOT (SImode,
	operand1)));
  _val = get_insns ();
  end_sequence ();
  return _val;
}

/* ../../gcc/gcc/config/i386/i386.md:17013 */
extern rtx gen_split_1454 PARAMS ((rtx *));
rtx
gen_split_1454 (operands)
      rtx *operands;
{
  rtx operand0;
  rtx operand1;
  rtx operand2;
  rtx operand3;
  rtx _val = 0;
  start_sequence ();
operands[0] = gen_lowpart (SImode, operands[0]);
   operands[2] = gen_lowpart (SImode, operands[2]);
   operands[3] = gen_lowpart (SImode, operands[3]);
  operand0 = operands[0];
  operand1 = operands[1];
  operand2 = operands[2];
  operand3 = operands[3];
  emit_insn (gen_rtx_SET (VOIDmode,
	operand0,
	gen_rtx_IF_THEN_ELSE (SImode,
	operand1,
	operand2,
	operand3)));
  _val = get_insns ();
  end_sequence ();
  return _val;
}

/* ../../gcc/gcc/config/i386/i386.md:17034 */
extern rtx gen_peephole2_1455 PARAMS ((rtx, rtx *));
rtx
gen_peephole2_1455 (curr_insn, operands)
     rtx curr_insn ATTRIBUTE_UNUSED;
     rtx *operands;
{
  rtx operand0;
  rtx operand1;
  rtx operand2;
  rtx _val = 0;
  HARD_REG_SET _regs_allocated;
  CLEAR_HARD_REG_SET (_regs_allocated);
  if ((operands[2] = peep2_find_free_register (1, 1, "r", SImode, &_regs_allocated)) == NULL_RTX)
    return NULL;
  start_sequence ();

  operand0 = operands[0];
  operand1 = operands[1];
  operand2 = operands[2];
  emit_insn (gen_rtx_SET (VOIDmode,
	operand2,
	operand1));
  emit_insn (gen_rtx_SET (VOIDmode,
	operand0,
	copy_rtx (operand2)));
  _val = get_insns ();
  end_sequence ();
  return _val;
}

/* ../../gcc/gcc/config/i386/i386.md:17043 */
extern rtx gen_peephole2_1456 PARAMS ((rtx, rtx *));
rtx
gen_peephole2_1456 (curr_insn, operands)
     rtx curr_insn ATTRIBUTE_UNUSED;
     rtx *operands;
{
  rtx operand0;
  rtx operand1;
  rtx operand2;
  rtx _val = 0;
  HARD_REG_SET _regs_allocated;
  CLEAR_HARD_REG_SET (_regs_allocated);
  if ((operands[2] = peep2_find_free_register (1, 1, "r", DImode, &_regs_allocated)) == NULL_RTX)
    return NULL;
  start_sequence ();

  operand0 = operands[0];
  operand1 = operands[1];
  operand2 = operands[2];
  emit_insn (gen_rtx_SET (VOIDmode,
	operand2,
	operand1));
  emit_insn (gen_rtx_SET (VOIDmode,
	operand0,
	copy_rtx (operand2)));
  _val = get_insns ();
  end_sequence ();
  return _val;
}

/* ../../gcc/gcc/config/i386/i386.md:17054 */
extern rtx gen_peephole2_1457 PARAMS ((rtx, rtx *));
rtx
gen_peephole2_1457 (curr_insn, operands)
     rtx curr_insn ATTRIBUTE_UNUSED;
     rtx *operands;
{
  rtx operand0;
  rtx operand1;
  rtx operand2;
  rtx _val = 0;
  HARD_REG_SET _regs_allocated;
  CLEAR_HARD_REG_SET (_regs_allocated);
  if ((operands[2] = peep2_find_free_register (1, 1, "r", SFmode, &_regs_allocated)) == NULL_RTX)
    return NULL;
  start_sequence ();

  operand0 = operands[0];
  operand1 = operands[1];
  operand2 = operands[2];
  emit_insn (gen_rtx_SET (VOIDmode,
	operand2,
	operand1));
  emit_insn (gen_rtx_SET (VOIDmode,
	operand0,
	copy_rtx (operand2)));
  _val = get_insns ();
  end_sequence ();
  return _val;
}

/* ../../gcc/gcc/config/i386/i386.md:17063 */
extern rtx gen_peephole2_1458 PARAMS ((rtx, rtx *));
rtx
gen_peephole2_1458 (curr_insn, operands)
     rtx curr_insn ATTRIBUTE_UNUSED;
     rtx *operands;
{
  rtx operand0;
  rtx operand1;
  rtx operand2;
  rtx _val = 0;
  HARD_REG_SET _regs_allocated;
  CLEAR_HARD_REG_SET (_regs_allocated);
  if ((operands[2] = peep2_find_free_register (1, 1, "r", HImode, &_regs_allocated)) == NULL_RTX)
    return NULL;
  start_sequence ();

  operand0 = operands[0];
  operand1 = operands[1];
  operand2 = operands[2];
  emit_insn (gen_rtx_SET (VOIDmode,
	operand2,
	operand1));
  emit_insn (gen_rtx_SET (VOIDmode,
	operand0,
	copy_rtx (operand2)));
  _val = get_insns ();
  end_sequence ();
  return _val;
}

/* ../../gcc/gcc/config/i386/i386.md:17072 */
extern rtx gen_peephole2_1459 PARAMS ((rtx, rtx *));
rtx
gen_peephole2_1459 (curr_insn, operands)
     rtx curr_insn ATTRIBUTE_UNUSED;
     rtx *operands;
{
  rtx operand0;
  rtx operand1;
  rtx operand2;
  rtx _val = 0;
  HARD_REG_SET _regs_allocated;
  CLEAR_HARD_REG_SET (_regs_allocated);
  if ((operands[2] = peep2_find_free_register (1, 1, "q", QImode, &_regs_allocated)) == NULL_RTX)
    return NULL;
  start_sequence ();

  operand0 = operands[0];
  operand1 = operands[1];
  operand2 = operands[2];
  emit_insn (gen_rtx_SET (VOIDmode,
	operand2,
	operand1));
  emit_insn (gen_rtx_SET (VOIDmode,
	operand0,
	copy_rtx (operand2)));
  _val = get_insns ();
  end_sequence ();
  return _val;
}

/* ../../gcc/gcc/config/i386/i386.md:17083 */
extern rtx gen_peephole2_1460 PARAMS ((rtx, rtx *));
rtx
gen_peephole2_1460 (curr_insn, operands)
     rtx curr_insn ATTRIBUTE_UNUSED;
     rtx *operands;
{
  rtx operand0;
  rtx operand1;
  rtx _val = 0;
  HARD_REG_SET _regs_allocated;
  CLEAR_HARD_REG_SET (_regs_allocated);
  if ((operands[1] = peep2_find_free_register (0, 0, "r", SImode, &_regs_allocated)) == NULL_RTX)
    return NULL;
  start_sequence ();

  operand0 = operands[0];
  operand1 = operands[1];
  emit (gen_rtx_PARALLEL (VOIDmode,
	gen_rtvec (2,
		gen_rtx_SET (VOIDmode,
	operand1,
	const0_rtx),
		gen_rtx_CLOBBER (VOIDmode,
	gen_rtx_REG (CCmode,
	17)))));
  emit_insn (gen_rtx_SET (VOIDmode,
	operand0,
	copy_rtx (operand1)));
  _val = get_insns ();
  end_sequence ();
  return _val;
}

/* ../../gcc/gcc/config/i386/i386.md:17097 */
extern rtx gen_peephole2_1461 PARAMS ((rtx, rtx *));
rtx
gen_peephole2_1461 (curr_insn, operands)
     rtx curr_insn ATTRIBUTE_UNUSED;
     rtx *operands;
{
  rtx operand0;
  rtx operand1;
  rtx operand2;
  rtx _val = 0;
  HARD_REG_SET _regs_allocated;
  CLEAR_HARD_REG_SET (_regs_allocated);
  if ((operands[1] = peep2_find_free_register (0, 0, "r", HImode, &_regs_allocated)) == NULL_RTX)
    return NULL;
  start_sequence ();
operands[2] = gen_rtx_REG (SImode, true_regnum (operands[1]));
  operand0 = operands[0];
  operand1 = operands[1];
  operand2 = operands[2];
  emit (gen_rtx_PARALLEL (VOIDmode,
	gen_rtvec (2,
		gen_rtx_SET (VOIDmode,
	operand2,
	const0_rtx),
		gen_rtx_CLOBBER (VOIDmode,
	gen_rtx_REG (CCmode,
	17)))));
  emit_insn (gen_rtx_SET (VOIDmode,
	operand0,
	operand1));
  _val = get_insns ();
  end_sequence ();
  return _val;
}

/* ../../gcc/gcc/config/i386/i386.md:17111 */
extern rtx gen_peephole2_1462 PARAMS ((rtx, rtx *));
rtx
gen_peephole2_1462 (curr_insn, operands)
     rtx curr_insn ATTRIBUTE_UNUSED;
     rtx *operands;
{
  rtx operand0;
  rtx operand1;
  rtx operand2;
  rtx _val = 0;
  HARD_REG_SET _regs_allocated;
  CLEAR_HARD_REG_SET (_regs_allocated);
  if ((operands[1] = peep2_find_free_register (0, 0, "q", QImode, &_regs_allocated)) == NULL_RTX)
    return NULL;
  start_sequence ();
operands[2] = gen_rtx_REG (SImode, true_regnum (operands[1]));
  operand0 = operands[0];
  operand1 = operands[1];
  operand2 = operands[2];
  emit (gen_rtx_PARALLEL (VOIDmode,
	gen_rtvec (2,
		gen_rtx_SET (VOIDmode,
	operand2,
	const0_rtx),
		gen_rtx_CLOBBER (VOIDmode,
	gen_rtx_REG (CCmode,
	17)))));
  emit_insn (gen_rtx_SET (VOIDmode,
	operand0,
	operand1));
  _val = get_insns ();
  end_sequence ();
  return _val;
}

/* ../../gcc/gcc/config/i386/i386.md:17125 */
extern rtx gen_peephole2_1463 PARAMS ((rtx, rtx *));
rtx
gen_peephole2_1463 (curr_insn, operands)
     rtx curr_insn ATTRIBUTE_UNUSED;
     rtx *operands;
{
  rtx operand0;
  rtx operand1;
  rtx operand2;
  rtx _val = 0;
  HARD_REG_SET _regs_allocated;
  CLEAR_HARD_REG_SET (_regs_allocated);
  if ((operands[2] = peep2_find_free_register (0, 0, "r", SImode, &_regs_allocated)) == NULL_RTX)
    return NULL;
  start_sequence ();

  operand0 = operands[0];
  operand1 = operands[1];
  operand2 = operands[2];
  emit_insn (gen_rtx_SET (VOIDmode,
	operand2,
	operand1));
  emit_insn (gen_rtx_SET (VOIDmode,
	operand0,
	copy_rtx (operand2)));
  _val = get_insns ();
  end_sequence ();
  return _val;
}

/* ../../gcc/gcc/config/i386/i386.md:17136 */
extern rtx gen_peephole2_1464 PARAMS ((rtx, rtx *));
rtx
gen_peephole2_1464 (curr_insn, operands)
     rtx curr_insn ATTRIBUTE_UNUSED;
     rtx *operands;
{
  rtx operand0;
  rtx operand1;
  rtx operand2;
  rtx _val = 0;
  HARD_REG_SET _regs_allocated;
  CLEAR_HARD_REG_SET (_regs_allocated);
  if ((operands[2] = peep2_find_free_register (0, 0, "r", HImode, &_regs_allocated)) == NULL_RTX)
    return NULL;
  start_sequence ();

  operand0 = operands[0];
  operand1 = operands[1];
  operand2 = operands[2];
  emit_insn (gen_rtx_SET (VOIDmode,
	operand2,
	operand1));
  emit_insn (gen_rtx_SET (VOIDmode,
	operand0,
	copy_rtx (operand2)));
  _val = get_insns ();
  end_sequence ();
  return _val;
}

/* ../../gcc/gcc/config/i386/i386.md:17146 */
extern rtx gen_peephole2_1465 PARAMS ((rtx, rtx *));
rtx
gen_peephole2_1465 (curr_insn, operands)
     rtx curr_insn ATTRIBUTE_UNUSED;
     rtx *operands;
{
  rtx operand0;
  rtx operand1;
  rtx operand2;
  rtx _val = 0;
  HARD_REG_SET _regs_allocated;
  CLEAR_HARD_REG_SET (_regs_allocated);
  if ((operands[2] = peep2_find_free_register (0, 0, "q", QImode, &_regs_allocated)) == NULL_RTX)
    return NULL;
  start_sequence ();

  operand0 = operands[0];
  operand1 = operands[1];
  operand2 = operands[2];
  emit_insn (gen_rtx_SET (VOIDmode,
	operand2,
	operand1));
  emit_insn (gen_rtx_SET (VOIDmode,
	operand0,
	copy_rtx (operand2)));
  _val = get_insns ();
  end_sequence ();
  return _val;
}

/* ../../gcc/gcc/config/i386/i386.md:17157 */
extern rtx gen_peephole2_1466 PARAMS ((rtx, rtx *));
rtx
gen_peephole2_1466 (curr_insn, operands)
     rtx curr_insn ATTRIBUTE_UNUSED;
     rtx *operands;
{
  rtx operand0;
  rtx operand1;
  rtx operand2;
  rtx operand3;
  rtx _val = 0;
  HARD_REG_SET _regs_allocated;
  CLEAR_HARD_REG_SET (_regs_allocated);
  if ((operands[3] = peep2_find_free_register (1, 1, "r", SImode, &_regs_allocated)) == NULL_RTX)
    return NULL;
  start_sequence ();

  operand0 = operands[0];
  operand1 = operands[1];
  operand2 = operands[2];
  operand3 = operands[3];
  emit_insn (gen_rtx_SET (VOIDmode,
	operand3,
	operand0));
  emit_insn (gen_rtx_SET (VOIDmode,
	gen_rtx_REG (CCNOmode,
	17),
	gen_rtx_COMPARE (CCNOmode,
	copy_rtx (operand3),
	const0_rtx)));
  _val = get_insns ();
  end_sequence ();
  return _val;
}

/* ../../gcc/gcc/config/i386/i386.md:17178 */
extern rtx gen_peephole2_1467 PARAMS ((rtx, rtx *));
rtx
gen_peephole2_1467 (curr_insn, operands)
     rtx curr_insn ATTRIBUTE_UNUSED;
     rtx *operands;
{
  rtx operand0;
  rtx operand1;
  rtx _val = 0;
  HARD_REG_SET _regs_allocated;
  CLEAR_HARD_REG_SET (_regs_allocated);
  start_sequence ();

  operand0 = operands[0];
  operand1 = operands[1];
  emit (gen_rtx_PARALLEL (VOIDmode,
	gen_rtvec (2,
		gen_rtx_SET (VOIDmode,
	operand0,
	gen_rtx_XOR (SImode,
	operand1,
	constm1_rtx)),
		gen_rtx_CLOBBER (VOIDmode,
	gen_rtx_REG (CCmode,
	17)))));
  _val = get_insns ();
  end_sequence ();
  return _val;
}

/* ../../gcc/gcc/config/i386/i386.md:17192 */
extern rtx gen_peephole2_1468 PARAMS ((rtx, rtx *));
rtx
gen_peephole2_1468 (curr_insn, operands)
     rtx curr_insn ATTRIBUTE_UNUSED;
     rtx *operands;
{
  rtx operand0;
  rtx operand1;
  rtx _val = 0;
  HARD_REG_SET _regs_allocated;
  CLEAR_HARD_REG_SET (_regs_allocated);
  start_sequence ();

  operand0 = operands[0];
  operand1 = operands[1];
  emit (gen_rtx_PARALLEL (VOIDmode,
	gen_rtvec (2,
		gen_rtx_SET (VOIDmode,
	operand0,
	gen_rtx_XOR (HImode,
	operand1,
	constm1_rtx)),
		gen_rtx_CLOBBER (VOIDmode,
	gen_rtx_REG (CCmode,
	17)))));
  _val = get_insns ();
  end_sequence ();
  return _val;
}

/* ../../gcc/gcc/config/i386/i386.md:17206 */
extern rtx gen_peephole2_1469 PARAMS ((rtx, rtx *));
rtx
gen_peephole2_1469 (curr_insn, operands)
     rtx curr_insn ATTRIBUTE_UNUSED;
     rtx *operands;
{
  rtx operand0;
  rtx operand1;
  rtx _val = 0;
  HARD_REG_SET _regs_allocated;
  CLEAR_HARD_REG_SET (_regs_allocated);
  start_sequence ();

  operand0 = operands[0];
  operand1 = operands[1];
  emit (gen_rtx_PARALLEL (VOIDmode,
	gen_rtvec (2,
		gen_rtx_SET (VOIDmode,
	operand0,
	gen_rtx_XOR (QImode,
	operand1,
	constm1_rtx)),
		gen_rtx_CLOBBER (VOIDmode,
	gen_rtx_REG (CCmode,
	17)))));
  _val = get_insns ();
  end_sequence ();
  return _val;
}

/* ../../gcc/gcc/config/i386/i386.md:17227 */
extern rtx gen_peephole2_1470 PARAMS ((rtx, rtx *));
rtx
gen_peephole2_1470 (curr_insn, operands)
     rtx curr_insn ATTRIBUTE_UNUSED;
     rtx *operands;
{
  rtx operand0;
  rtx operand1;
  rtx _val = 0;
  HARD_REG_SET _regs_allocated;
  CLEAR_HARD_REG_SET (_regs_allocated);
  start_sequence ();

  operand0 = operands[0];
  operand1 = operands[1];
  emit (gen_rtx_PARALLEL (VOIDmode,
	gen_rtvec (2,
		gen_rtx_SET (VOIDmode,
	gen_rtx_REG (CCNOmode,
	17),
	gen_rtx_COMPARE (CCNOmode,
	gen_rtx_AND (SImode,
	operand0,
	operand1),
	const0_rtx)),
		gen_rtx_SET (VOIDmode,
	copy_rtx (operand0),
	gen_rtx_AND (SImode,
	copy_rtx (operand0),
	copy_rtx (operand1))))));
  _val = get_insns ();
  end_sequence ();
  return _val;
}

/* ../../gcc/gcc/config/i386/i386.md:17249 */
extern rtx gen_peephole2_1471 PARAMS ((rtx, rtx *));
rtx
gen_peephole2_1471 (curr_insn, operands)
     rtx curr_insn ATTRIBUTE_UNUSED;
     rtx *operands;
{
  rtx operand0;
  rtx operand1;
  rtx _val = 0;
  HARD_REG_SET _regs_allocated;
  CLEAR_HARD_REG_SET (_regs_allocated);
  start_sequence ();

  operand0 = operands[0];
  operand1 = operands[1];
  emit (gen_rtx_PARALLEL (VOIDmode,
	gen_rtvec (2,
		gen_rtx_SET (VOIDmode,
	gen_rtx_REG (CCNOmode,
	17),
	gen_rtx_COMPARE (CCNOmode,
	gen_rtx_AND (QImode,
	operand0,
	operand1),
	const0_rtx)),
		gen_rtx_SET (VOIDmode,
	copy_rtx (operand0),
	gen_rtx_AND (QImode,
	copy_rtx (operand0),
	copy_rtx (operand1))))));
  _val = get_insns ();
  end_sequence ();
  return _val;
}

/* ../../gcc/gcc/config/i386/i386.md:17267 */
extern rtx gen_peephole2_1472 PARAMS ((rtx, rtx *));
rtx
gen_peephole2_1472 (curr_insn, operands)
     rtx curr_insn ATTRIBUTE_UNUSED;
     rtx *operands;
{
  rtx operand0;
  rtx operand1;
  rtx _val = 0;
  HARD_REG_SET _regs_allocated;
  CLEAR_HARD_REG_SET (_regs_allocated);
  start_sequence ();

  operand0 = operands[0];
  operand1 = operands[1];
  emit (gen_rtx_PARALLEL (VOIDmode,
	gen_rtvec (2,
		gen_rtx_SET (VOIDmode,
	gen_rtx_REG (CCNOmode,
	17),
	gen_rtx_COMPARE (CCNOmode,
	gen_rtx_AND (SImode,
	gen_rtx_ZERO_EXTRACT (SImode,
	operand0,
	GEN_INT (8LL),
	GEN_INT (8LL)),
	operand1),
	const0_rtx)),
		gen_rtx_SET (VOIDmode,
	gen_rtx_ZERO_EXTRACT (SImode,
	copy_rtx (operand0),
	GEN_INT (8LL),
	GEN_INT (8LL)),
	gen_rtx_AND (SImode,
	gen_rtx_ZERO_EXTRACT (SImode,
	copy_rtx (operand0),
	GEN_INT (8LL),
	GEN_INT (8LL)),
	copy_rtx (operand1))))));
  _val = get_insns ();
  end_sequence ();
  return _val;
}

/* ../../gcc/gcc/config/i386/i386.md:17302 */
extern rtx gen_peephole2_1473 PARAMS ((rtx, rtx *));
rtx
gen_peephole2_1473 (curr_insn, operands)
     rtx curr_insn ATTRIBUTE_UNUSED;
     rtx *operands;
{
  rtx operand0;
  rtx operand1;
  rtx operand2;
  rtx operand3;
  rtx _val = 0;
  HARD_REG_SET _regs_allocated;
  CLEAR_HARD_REG_SET (_regs_allocated);
  if ((operands[2] = peep2_find_free_register (0, 0, "r", SImode, &_regs_allocated)) == NULL_RTX)
    return NULL;
  start_sequence ();

  operand0 = operands[0];
  operand1 = operands[1];
  operand2 = operands[2];
  operand3 = operands[3];
  emit_insn (gen_rtx_SET (VOIDmode,
	operand2,
	operand1));
  emit (gen_rtx_PARALLEL (VOIDmode,
	gen_rtvec (2,
		gen_rtx_SET (VOIDmode,
	operand0,
	gen_rtx (GET_CODE (operand3), GET_MODE (operand3),
		copy_rtx (operand0),
		copy_rtx (operand2))),
		gen_rtx_CLOBBER (VOIDmode,
	gen_rtx_REG (CCmode,
	17)))));
  _val = get_insns ();
  end_sequence ();
  return _val;
}

/* ../../gcc/gcc/config/i386/i386.md:17316 */
extern rtx gen_peephole2_1474 PARAMS ((rtx, rtx *));
rtx
gen_peephole2_1474 (curr_insn, operands)
     rtx curr_insn ATTRIBUTE_UNUSED;
     rtx *operands;
{
  rtx operand0;
  rtx operand1;
  rtx operand2;
  rtx operand3;
  rtx _val = 0;
  HARD_REG_SET _regs_allocated;
  CLEAR_HARD_REG_SET (_regs_allocated);
  if ((operands[2] = peep2_find_free_register (0, 0, "r", SImode, &_regs_allocated)) == NULL_RTX)
    return NULL;
  start_sequence ();

  operand0 = operands[0];
  operand1 = operands[1];
  operand2 = operands[2];
  operand3 = operands[3];
  emit_insn (gen_rtx_SET (VOIDmode,
	operand2,
	operand1));
  emit (gen_rtx_PARALLEL (VOIDmode,
	gen_rtvec (2,
		gen_rtx_SET (VOIDmode,
	operand0,
	gen_rtx (GET_CODE (operand3), GET_MODE (operand3),
		copy_rtx (operand2),
		copy_rtx (operand0))),
		gen_rtx_CLOBBER (VOIDmode,
	gen_rtx_REG (CCmode,
	17)))));
  _val = get_insns ();
  end_sequence ();
  return _val;
}

/* ../../gcc/gcc/config/i386/i386.md:17336 */
extern rtx gen_peephole2_1475 PARAMS ((rtx, rtx *));
rtx
gen_peephole2_1475 (curr_insn, operands)
     rtx curr_insn ATTRIBUTE_UNUSED;
     rtx *operands;
{
  rtx operand0;
  rtx operand1;
  rtx operand2;
  rtx operand3;
  rtx _val = 0;
  HARD_REG_SET _regs_allocated;
  CLEAR_HARD_REG_SET (_regs_allocated);
  if ((operands[2] = peep2_find_free_register (0, 0, "r", SImode, &_regs_allocated)) == NULL_RTX)
    return NULL;
  start_sequence ();

  operand0 = operands[0];
  operand1 = operands[1];
  operand2 = operands[2];
  operand3 = operands[3];
  emit_insn (gen_rtx_SET (VOIDmode,
	operand2,
	operand0));
  emit (gen_rtx_PARALLEL (VOIDmode,
	gen_rtvec (2,
		gen_rtx_SET (VOIDmode,
	copy_rtx (operand2),
	gen_rtx (GET_CODE (operand3), GET_MODE (operand3),
		copy_rtx (operand2),
		operand1)),
		gen_rtx_CLOBBER (VOIDmode,
	gen_rtx_REG (CCmode,
	17)))));
  emit_insn (gen_rtx_SET (VOIDmode,
	copy_rtx (operand0),
	copy_rtx (operand2)));
  _val = get_insns ();
  end_sequence ();
  return _val;
}

/* ../../gcc/gcc/config/i386/i386.md:17351 */
extern rtx gen_peephole2_1476 PARAMS ((rtx, rtx *));
rtx
gen_peephole2_1476 (curr_insn, operands)
     rtx curr_insn ATTRIBUTE_UNUSED;
     rtx *operands;
{
  rtx operand0;
  rtx operand1;
  rtx operand2;
  rtx operand3;
  rtx _val = 0;
  HARD_REG_SET _regs_allocated;
  CLEAR_HARD_REG_SET (_regs_allocated);
  if ((operands[2] = peep2_find_free_register (0, 0, "r", SImode, &_regs_allocated)) == NULL_RTX)
    return NULL;
  start_sequence ();

  operand0 = operands[0];
  operand1 = operands[1];
  operand2 = operands[2];
  operand3 = operands[3];
  emit_insn (gen_rtx_SET (VOIDmode,
	operand2,
	operand0));
  emit (gen_rtx_PARALLEL (VOIDmode,
	gen_rtvec (2,
		gen_rtx_SET (VOIDmode,
	copy_rtx (operand2),
	gen_rtx (GET_CODE (operand3), GET_MODE (operand3),
		operand1,
		copy_rtx (operand2))),
		gen_rtx_CLOBBER (VOIDmode,
	gen_rtx_REG (CCmode,
	17)))));
  emit_insn (gen_rtx_SET (VOIDmode,
	copy_rtx (operand0),
	copy_rtx (operand2)));
  _val = get_insns ();
  end_sequence ();
  return _val;
}

/* ../../gcc/gcc/config/i386/i386.md:17367 */
extern rtx gen_peephole2_1477 PARAMS ((rtx, rtx *));
rtx
gen_peephole2_1477 (curr_insn, operands)
     rtx curr_insn ATTRIBUTE_UNUSED;
     rtx *operands;
{
  rtx operand0;
  rtx _val = 0;
  HARD_REG_SET _regs_allocated;
  CLEAR_HARD_REG_SET (_regs_allocated);
  start_sequence ();
operands[0] = gen_rtx_REG (GET_MODE (operands[0]) == DImode ? DImode : SImode,
			      true_regnum (operands[0]));
  operand0 = operands[0];
  emit (gen_rtx_PARALLEL (VOIDmode,
	gen_rtvec (2,
		gen_rtx_SET (VOIDmode,
	operand0,
	const0_rtx),
		gen_rtx_CLOBBER (VOIDmode,
	gen_rtx_REG (CCmode,
	17)))));
  _val = get_insns ();
  end_sequence ();
  return _val;
}

/* ../../gcc/gcc/config/i386/i386.md:17381 */
extern rtx gen_peephole2_1478 PARAMS ((rtx, rtx *));
rtx
gen_peephole2_1478 (curr_insn, operands)
     rtx curr_insn ATTRIBUTE_UNUSED;
     rtx *operands;
{
  rtx operand0;
  rtx _val = 0;
  HARD_REG_SET _regs_allocated;
  CLEAR_HARD_REG_SET (_regs_allocated);
  start_sequence ();
  operand0 = operands[0];
  emit (gen_rtx_PARALLEL (VOIDmode,
	gen_rtvec (2,
		gen_rtx_SET (VOIDmode,
	gen_rtx_STRICT_LOW_PART (VOIDmode,
	operand0),
	const0_rtx),
		gen_rtx_CLOBBER (VOIDmode,
	gen_rtx_REG (CCmode,
	17)))));
  _val = get_insns ();
  end_sequence ();
  return _val;
}

/* ../../gcc/gcc/config/i386/i386.md:17392 */
extern rtx gen_peephole2_1479 PARAMS ((rtx, rtx *));
rtx
gen_peephole2_1479 (curr_insn, operands)
     rtx curr_insn ATTRIBUTE_UNUSED;
     rtx *operands;
{
  rtx operand0;
  rtx _val = 0;
  HARD_REG_SET _regs_allocated;
  CLEAR_HARD_REG_SET (_regs_allocated);
  start_sequence ();
operands[0] = gen_rtx_REG (GET_MODE (operands[0]) == DImode ? DImode : SImode,
			      true_regnum (operands[0]));
  operand0 = operands[0];
  emit (gen_rtx_PARALLEL (VOIDmode,
	gen_rtvec (2,
		gen_rtx_SET (VOIDmode,
	operand0,
	constm1_rtx),
		gen_rtx_CLOBBER (VOIDmode,
	gen_rtx_REG (CCmode,
	17)))));
  _val = get_insns ();
  end_sequence ();
  return _val;
}

/* ../../gcc/gcc/config/i386/i386.md:17407 */
extern rtx gen_peephole2_1480 PARAMS ((rtx, rtx *));
rtx
gen_peephole2_1480 (curr_insn, operands)
     rtx curr_insn ATTRIBUTE_UNUSED;
     rtx *operands;
{
  rtx operand0;
  rtx operand1;
  rtx _val = 0;
  HARD_REG_SET _regs_allocated;
  CLEAR_HARD_REG_SET (_regs_allocated);
  start_sequence ();

  operand0 = operands[0];
  operand1 = operands[1];
  emit (gen_rtx_PARALLEL (VOIDmode,
	gen_rtvec (2,
		gen_rtx_SET (VOIDmode,
	operand0,
	gen_rtx_PLUS (SImode,
	copy_rtx (operand0),
	operand1)),
		gen_rtx_CLOBBER (VOIDmode,
	gen_rtx_REG (CCmode,
	17)))));
  _val = get_insns ();
  end_sequence ();
  return _val;
}

/* ../../gcc/gcc/config/i386/i386.md:17416 */
extern rtx gen_peephole2_1481 PARAMS ((rtx, rtx *));
rtx
gen_peephole2_1481 (curr_insn, operands)
     rtx curr_insn ATTRIBUTE_UNUSED;
     rtx *operands;
{
  rtx operand0;
  rtx operand1;
  rtx operand2;
  rtx _val = 0;
  HARD_REG_SET _regs_allocated;
  CLEAR_HARD_REG_SET (_regs_allocated);
  start_sequence ();
operands[2] = gen_lowpart (SImode, operands[2]);
  operand0 = operands[0];
  operand1 = operands[1];
  operand2 = operands[2];
  emit (gen_rtx_PARALLEL (VOIDmode,
	gen_rtvec (2,
		gen_rtx_SET (VOIDmode,
	operand0,
	gen_rtx_PLUS (SImode,
	copy_rtx (operand0),
	operand2)),
		gen_rtx_CLOBBER (VOIDmode,
	gen_rtx_REG (CCmode,
	17)))));
  _val = get_insns ();
  end_sequence ();
  return _val;
}

/* ../../gcc/gcc/config/i386/i386.md:17425 */
extern rtx gen_peephole2_1482 PARAMS ((rtx, rtx *));
rtx
gen_peephole2_1482 (curr_insn, operands)
     rtx curr_insn ATTRIBUTE_UNUSED;
     rtx *operands;
{
  rtx operand0;
  rtx operand1;
  rtx _val = 0;
  HARD_REG_SET _regs_allocated;
  CLEAR_HARD_REG_SET (_regs_allocated);
  start_sequence ();

  operand0 = operands[0];
  operand1 = operands[1];
  emit (gen_rtx_PARALLEL (VOIDmode,
	gen_rtvec (2,
		gen_rtx_SET (VOIDmode,
	operand0,
	gen_rtx_PLUS (DImode,
	copy_rtx (operand0),
	operand1)),
		gen_rtx_CLOBBER (VOIDmode,
	gen_rtx_REG (CCmode,
	17)))));
  _val = get_insns ();
  end_sequence ();
  return _val;
}

/* ../../gcc/gcc/config/i386/i386.md:17434 */
extern rtx gen_peephole2_1483 PARAMS ((rtx, rtx *));
rtx
gen_peephole2_1483 (curr_insn, operands)
     rtx curr_insn ATTRIBUTE_UNUSED;
     rtx *operands;
{
  rtx operand0;
  rtx operand1;
  rtx operand2;
  rtx _val = 0;
  HARD_REG_SET _regs_allocated;
  CLEAR_HARD_REG_SET (_regs_allocated);
  start_sequence ();
operands[2] = GEN_INT (exact_log2 (INTVAL (operands[1])));
  operand0 = operands[0];
  operand1 = operands[1];
  operand2 = operands[2];
  emit (gen_rtx_PARALLEL (VOIDmode,
	gen_rtvec (2,
		gen_rtx_SET (VOIDmode,
	operand0,
	gen_rtx_ASHIFT (SImode,
	copy_rtx (operand0),
	operand2)),
		gen_rtx_CLOBBER (VOIDmode,
	gen_rtx_REG (CCmode,
	17)))));
  _val = get_insns ();
  end_sequence ();
  return _val;
}

/* ../../gcc/gcc/config/i386/i386.md:17444 */
extern rtx gen_peephole2_1484 PARAMS ((rtx, rtx *));
rtx
gen_peephole2_1484 (curr_insn, operands)
     rtx curr_insn ATTRIBUTE_UNUSED;
     rtx *operands;
{
  rtx operand0;
  rtx operand1;
  rtx operand2;
  rtx _val = 0;
  HARD_REG_SET _regs_allocated;
  CLEAR_HARD_REG_SET (_regs_allocated);
  start_sequence ();
operands[2] = GEN_INT (exact_log2 (INTVAL (operands[1])));
  operand0 = operands[0];
  operand1 = operands[1];
  operand2 = operands[2];
  emit (gen_rtx_PARALLEL (VOIDmode,
	gen_rtvec (2,
		gen_rtx_SET (VOIDmode,
	operand0,
	gen_rtx_ASHIFT (DImode,
	copy_rtx (operand0),
	operand2)),
		gen_rtx_CLOBBER (VOIDmode,
	gen_rtx_REG (CCmode,
	17)))));
  _val = get_insns ();
  end_sequence ();
  return _val;
}

/* ../../gcc/gcc/config/i386/i386.md:17454 */
extern rtx gen_peephole2_1485 PARAMS ((rtx, rtx *));
rtx
gen_peephole2_1485 (curr_insn, operands)
     rtx curr_insn ATTRIBUTE_UNUSED;
     rtx *operands;
{
  rtx operand0;
  rtx operand1;
  rtx operand2;
  rtx _val = 0;
  HARD_REG_SET _regs_allocated;
  CLEAR_HARD_REG_SET (_regs_allocated);
  start_sequence ();
operands[2] = GEN_INT (exact_log2 (INTVAL (operands[2])));
  operand0 = operands[0];
  operand1 = operands[1];
  operand2 = operands[2];
  emit (gen_rtx_PARALLEL (VOIDmode,
	gen_rtvec (2,
		gen_rtx_SET (VOIDmode,
	operand0,
	gen_rtx_ASHIFT (SImode,
	copy_rtx (operand0),
	operand2)),
		gen_rtx_CLOBBER (VOIDmode,
	gen_rtx_REG (CCmode,
	17)))));
  _val = get_insns ();
  end_sequence ();
  return _val;
}

/* ../../gcc/gcc/config/i386/i386.md:17484 */
extern rtx gen_peephole2_1486 PARAMS ((rtx, rtx *));
rtx
gen_peephole2_1486 (curr_insn, operands)
     rtx curr_insn ATTRIBUTE_UNUSED;
     rtx *operands;
{
  rtx operand0;
  rtx _val = 0;
  HARD_REG_SET _regs_allocated;
  CLEAR_HARD_REG_SET (_regs_allocated);
  if ((operands[0] = peep2_find_free_register (0, 0, "r", SImode, &_regs_allocated)) == NULL_RTX)
    return NULL;
  start_sequence ();
  operand0 = operands[0];
  emit_insn (gen_rtx_CLOBBER (VOIDmode,
	operand0));
  emit (gen_rtx_PARALLEL (VOIDmode,
	gen_rtvec (2,
		gen_rtx_SET (VOIDmode,
	gen_rtx_MEM (SImode,
	gen_rtx_PRE_DEC (SImode,
	gen_rtx_REG (SImode,
	7))),
	copy_rtx (operand0)),
		gen_rtx_CLOBBER (VOIDmode,
	gen_rtx_MEM (BLKmode,
	gen_rtx_SCRATCH (VOIDmode))))));
  _val = get_insns ();
  end_sequence ();
  return _val;
}

/* ../../gcc/gcc/config/i386/i386.md:17494 */
extern rtx gen_peephole2_1487 PARAMS ((rtx, rtx *));
rtx
gen_peephole2_1487 (curr_insn, operands)
     rtx curr_insn ATTRIBUTE_UNUSED;
     rtx *operands;
{
  rtx operand0;
  rtx _val = 0;
  HARD_REG_SET _regs_allocated;
  CLEAR_HARD_REG_SET (_regs_allocated);
  if ((operands[0] = peep2_find_free_register (0, 0, "r", SImode, &_regs_allocated)) == NULL_RTX)
    return NULL;
  start_sequence ();
  operand0 = operands[0];
  emit_insn (gen_rtx_CLOBBER (VOIDmode,
	operand0));
  emit_insn (gen_rtx_SET (VOIDmode,
	gen_rtx_MEM (SImode,
	gen_rtx_PRE_DEC (SImode,
	gen_rtx_REG (SImode,
	7))),
	copy_rtx (operand0)));
  emit (gen_rtx_PARALLEL (VOIDmode,
	gen_rtvec (2,
		gen_rtx_SET (VOIDmode,
	gen_rtx_MEM (SImode,
	gen_rtx_PRE_DEC (SImode,
	gen_rtx_REG (SImode,
	7))),
	copy_rtx (operand0)),
		gen_rtx_CLOBBER (VOIDmode,
	gen_rtx_MEM (BLKmode,
	gen_rtx_SCRATCH (VOIDmode))))));
  _val = get_insns ();
  end_sequence ();
  return _val;
}

/* ../../gcc/gcc/config/i386/i386.md:17506 */
extern rtx gen_peephole2_1488 PARAMS ((rtx, rtx *));
rtx
gen_peephole2_1488 (curr_insn, operands)
     rtx curr_insn ATTRIBUTE_UNUSED;
     rtx *operands;
{
  rtx operand0;
  rtx _val = 0;
  HARD_REG_SET _regs_allocated;
  CLEAR_HARD_REG_SET (_regs_allocated);
  if ((operands[0] = peep2_find_free_register (0, 0, "r", SImode, &_regs_allocated)) == NULL_RTX)
    return NULL;
  start_sequence ();
  operand0 = operands[0];
  emit_insn (gen_rtx_CLOBBER (VOIDmode,
	operand0));
  emit_insn (gen_rtx_SET (VOIDmode,
	gen_rtx_MEM (SImode,
	gen_rtx_PRE_DEC (SImode,
	gen_rtx_REG (SImode,
	7))),
	copy_rtx (operand0)));
  _val = get_insns ();
  end_sequence ();
  return _val;
}

/* ../../gcc/gcc/config/i386/i386.md:17514 */
extern rtx gen_peephole2_1489 PARAMS ((rtx, rtx *));
rtx
gen_peephole2_1489 (curr_insn, operands)
     rtx curr_insn ATTRIBUTE_UNUSED;
     rtx *operands;
{
  rtx operand0;
  rtx _val = 0;
  HARD_REG_SET _regs_allocated;
  CLEAR_HARD_REG_SET (_regs_allocated);
  if ((operands[0] = peep2_find_free_register (0, 0, "r", SImode, &_regs_allocated)) == NULL_RTX)
    return NULL;
  start_sequence ();
  operand0 = operands[0];
  emit_insn (gen_rtx_CLOBBER (VOIDmode,
	operand0));
  emit_insn (gen_rtx_SET (VOIDmode,
	gen_rtx_MEM (SImode,
	gen_rtx_PRE_DEC (SImode,
	gen_rtx_REG (SImode,
	7))),
	copy_rtx (operand0)));
  emit_insn (gen_rtx_SET (VOIDmode,
	gen_rtx_MEM (SImode,
	gen_rtx_PRE_DEC (SImode,
	gen_rtx_REG (SImode,
	7))),
	copy_rtx (operand0)));
  _val = get_insns ();
  end_sequence ();
  return _val;
}

/* ../../gcc/gcc/config/i386/i386.md:17524 */
extern rtx gen_peephole2_1490 PARAMS ((rtx, rtx *));
rtx
gen_peephole2_1490 (curr_insn, operands)
     rtx curr_insn ATTRIBUTE_UNUSED;
     rtx *operands;
{
  rtx operand0;
  rtx _val = 0;
  HARD_REG_SET _regs_allocated;
  CLEAR_HARD_REG_SET (_regs_allocated);
  if ((operands[0] = peep2_find_free_register (0, 0, "r", SImode, &_regs_allocated)) == NULL_RTX)
    return NULL;
  start_sequence ();

  operand0 = operands[0];
  emit (gen_rtx_PARALLEL (VOIDmode,
	gen_rtvec (3,
		gen_rtx_SET (VOIDmode,
	operand0,
	gen_rtx_MEM (SImode,
	gen_rtx_REG (SImode,
	7))),
		gen_rtx_SET (VOIDmode,
	gen_rtx_REG (SImode,
	7),
	gen_rtx_PLUS (SImode,
	gen_rtx_REG (SImode,
	7),
	GEN_INT (4LL))),
		gen_rtx_CLOBBER (VOIDmode,
	gen_rtx_MEM (BLKmode,
	gen_rtx_SCRATCH (VOIDmode))))));
  _val = get_insns ();
  end_sequence ();
  return _val;
}

/* ../../gcc/gcc/config/i386/i386.md:17537 */
extern rtx gen_peephole2_1491 PARAMS ((rtx, rtx *));
rtx
gen_peephole2_1491 (curr_insn, operands)
     rtx curr_insn ATTRIBUTE_UNUSED;
     rtx *operands;
{
  rtx operand0;
  rtx operand1;
  rtx _val = 0;
  HARD_REG_SET _regs_allocated;
  CLEAR_HARD_REG_SET (_regs_allocated);
  if ((operands[0] = peep2_find_free_register (0, 0, "r", SImode, &_regs_allocated)) == NULL_RTX)
    return NULL;
  if ((operands[1] = peep2_find_free_register (0, 0, "r", SImode, &_regs_allocated)) == NULL_RTX)
    return NULL;
  start_sequence ();

  operand0 = operands[0];
  operand1 = operands[1];
  emit (gen_rtx_PARALLEL (VOIDmode,
	gen_rtvec (3,
		gen_rtx_SET (VOIDmode,
	operand0,
	gen_rtx_MEM (SImode,
	gen_rtx_REG (SImode,
	7))),
		gen_rtx_SET (VOIDmode,
	gen_rtx_REG (SImode,
	7),
	gen_rtx_PLUS (SImode,
	gen_rtx_REG (SImode,
	7),
	GEN_INT (4LL))),
		gen_rtx_CLOBBER (VOIDmode,
	gen_rtx_MEM (BLKmode,
	gen_rtx_SCRATCH (VOIDmode))))));
  emit (gen_rtx_PARALLEL (VOIDmode,
	gen_rtvec (2,
		gen_rtx_SET (VOIDmode,
	operand1,
	gen_rtx_MEM (SImode,
	gen_rtx_REG (SImode,
	7))),
		gen_rtx_SET (VOIDmode,
	gen_rtx_REG (SImode,
	7),
	gen_rtx_PLUS (SImode,
	gen_rtx_REG (SImode,
	7),
	GEN_INT (4LL))))));
  _val = get_insns ();
  end_sequence ();
  return _val;
}

/* ../../gcc/gcc/config/i386/i386.md:17551 */
extern rtx gen_peephole2_1492 PARAMS ((rtx, rtx *));
rtx
gen_peephole2_1492 (curr_insn, operands)
     rtx curr_insn ATTRIBUTE_UNUSED;
     rtx *operands;
{
  rtx operand0;
  rtx _val = 0;
  HARD_REG_SET _regs_allocated;
  CLEAR_HARD_REG_SET (_regs_allocated);
  if ((operands[0] = peep2_find_free_register (0, 0, "r", SImode, &_regs_allocated)) == NULL_RTX)
    return NULL;
  start_sequence ();

  operand0 = operands[0];
  emit (gen_rtx_PARALLEL (VOIDmode,
	gen_rtvec (3,
		gen_rtx_SET (VOIDmode,
	operand0,
	gen_rtx_MEM (SImode,
	gen_rtx_REG (SImode,
	7))),
		gen_rtx_SET (VOIDmode,
	gen_rtx_REG (SImode,
	7),
	gen_rtx_PLUS (SImode,
	gen_rtx_REG (SImode,
	7),
	GEN_INT (4LL))),
		gen_rtx_CLOBBER (VOIDmode,
	gen_rtx_MEM (BLKmode,
	gen_rtx_SCRATCH (VOIDmode))))));
  emit (gen_rtx_PARALLEL (VOIDmode,
	gen_rtvec (2,
		gen_rtx_SET (VOIDmode,
	copy_rtx (operand0),
	gen_rtx_MEM (SImode,
	gen_rtx_REG (SImode,
	7))),
		gen_rtx_SET (VOIDmode,
	gen_rtx_REG (SImode,
	7),
	gen_rtx_PLUS (SImode,
	gen_rtx_REG (SImode,
	7),
	GEN_INT (4LL))))));
  _val = get_insns ();
  end_sequence ();
  return _val;
}

/* ../../gcc/gcc/config/i386/i386.md:17565 */
extern rtx gen_peephole2_1493 PARAMS ((rtx, rtx *));
rtx
gen_peephole2_1493 (curr_insn, operands)
     rtx curr_insn ATTRIBUTE_UNUSED;
     rtx *operands;
{
  rtx operand0;
  rtx _val = 0;
  HARD_REG_SET _regs_allocated;
  CLEAR_HARD_REG_SET (_regs_allocated);
  if ((operands[0] = peep2_find_free_register (0, 0, "r", SImode, &_regs_allocated)) == NULL_RTX)
    return NULL;
  start_sequence ();

  operand0 = operands[0];
  emit (gen_rtx_PARALLEL (VOIDmode,
	gen_rtvec (2,
		gen_rtx_SET (VOIDmode,
	operand0,
	gen_rtx_MEM (SImode,
	gen_rtx_REG (SImode,
	7))),
		gen_rtx_SET (VOIDmode,
	gen_rtx_REG (SImode,
	7),
	gen_rtx_PLUS (SImode,
	gen_rtx_REG (SImode,
	7),
	GEN_INT (4LL))))));
  _val = get_insns ();
  end_sequence ();
  return _val;
}

/* ../../gcc/gcc/config/i386/i386.md:17576 */
extern rtx gen_peephole2_1494 PARAMS ((rtx, rtx *));
rtx
gen_peephole2_1494 (curr_insn, operands)
     rtx curr_insn ATTRIBUTE_UNUSED;
     rtx *operands;
{
  rtx operand0;
  rtx operand1;
  rtx _val = 0;
  HARD_REG_SET _regs_allocated;
  CLEAR_HARD_REG_SET (_regs_allocated);
  if ((operands[0] = peep2_find_free_register (0, 0, "r", SImode, &_regs_allocated)) == NULL_RTX)
    return NULL;
  if ((operands[1] = peep2_find_free_register (0, 0, "r", SImode, &_regs_allocated)) == NULL_RTX)
    return NULL;
  start_sequence ();

  operand0 = operands[0];
  operand1 = operands[1];
  emit (gen_rtx_PARALLEL (VOIDmode,
	gen_rtvec (2,
		gen_rtx_SET (VOIDmode,
	operand0,
	gen_rtx_MEM (SImode,
	gen_rtx_REG (SImode,
	7))),
		gen_rtx_SET (VOIDmode,
	gen_rtx_REG (SImode,
	7),
	gen_rtx_PLUS (SImode,
	gen_rtx_REG (SImode,
	7),
	GEN_INT (4LL))))));
  emit (gen_rtx_PARALLEL (VOIDmode,
	gen_rtvec (2,
		gen_rtx_SET (VOIDmode,
	operand1,
	gen_rtx_MEM (SImode,
	gen_rtx_REG (SImode,
	7))),
		gen_rtx_SET (VOIDmode,
	gen_rtx_REG (SImode,
	7),
	gen_rtx_PLUS (SImode,
	gen_rtx_REG (SImode,
	7),
	GEN_INT (4LL))))));
  _val = get_insns ();
  end_sequence ();
  return _val;
}

/* ../../gcc/gcc/config/i386/i386.md:17588 */
extern rtx gen_peephole2_1495 PARAMS ((rtx, rtx *));
rtx
gen_peephole2_1495 (curr_insn, operands)
     rtx curr_insn ATTRIBUTE_UNUSED;
     rtx *operands;
{
  rtx operand0;
  rtx _val = 0;
  HARD_REG_SET _regs_allocated;
  CLEAR_HARD_REG_SET (_regs_allocated);
  if ((operands[0] = peep2_find_free_register (0, 0, "r", SImode, &_regs_allocated)) == NULL_RTX)
    return NULL;
  start_sequence ();

  operand0 = operands[0];
  emit (gen_rtx_PARALLEL (VOIDmode,
	gen_rtvec (2,
		gen_rtx_SET (VOIDmode,
	operand0,
	gen_rtx_MEM (SImode,
	gen_rtx_REG (SImode,
	7))),
		gen_rtx_SET (VOIDmode,
	gen_rtx_REG (SImode,
	7),
	gen_rtx_PLUS (SImode,
	gen_rtx_REG (SImode,
	7),
	GEN_INT (4LL))))));
  emit (gen_rtx_PARALLEL (VOIDmode,
	gen_rtvec (2,
		gen_rtx_SET (VOIDmode,
	copy_rtx (operand0),
	gen_rtx_MEM (SImode,
	gen_rtx_REG (SImode,
	7))),
		gen_rtx_SET (VOIDmode,
	gen_rtx_REG (SImode,
	7),
	gen_rtx_PLUS (SImode,
	gen_rtx_REG (SImode,
	7),
	GEN_INT (4LL))))));
  _val = get_insns ();
  end_sequence ();
  return _val;
}

/* ../../gcc/gcc/config/i386/i386.md:17601 */
extern rtx gen_peephole2_1496 PARAMS ((rtx, rtx *));
rtx
gen_peephole2_1496 (curr_insn, operands)
     rtx curr_insn ATTRIBUTE_UNUSED;
     rtx *operands;
{
  rtx operand0;
  rtx operand1;
  rtx _val = 0;
  HARD_REG_SET _regs_allocated;
  CLEAR_HARD_REG_SET (_regs_allocated);
  start_sequence ();

  operand0 = operands[0];
  operand1 = operands[1];
  emit (gen_rtx_PARALLEL (VOIDmode,
	gen_rtvec (2,
		gen_rtx_SET (VOIDmode,
	gen_rtx_REG (CCGCmode,
	17),
	gen_rtx_COMPARE (CCGCmode,
	operand0,
	operand1)),
		gen_rtx_CLOBBER (VOIDmode,
	copy_rtx (operand0)))));
  _val = get_insns ();
  end_sequence ();
  return _val;
}

/* ../../gcc/gcc/config/i386/i386.md:17613 */
extern rtx gen_peephole2_1497 PARAMS ((rtx, rtx *));
rtx
gen_peephole2_1497 (curr_insn, operands)
     rtx curr_insn ATTRIBUTE_UNUSED;
     rtx *operands;
{
  rtx operand0;
  rtx operand1;
  rtx _val = 0;
  HARD_REG_SET _regs_allocated;
  CLEAR_HARD_REG_SET (_regs_allocated);
  start_sequence ();

  operand0 = operands[0];
  operand1 = operands[1];
  emit (gen_rtx_PARALLEL (VOIDmode,
	gen_rtvec (2,
		gen_rtx_SET (VOIDmode,
	gen_rtx_REG (CCGCmode,
	17),
	gen_rtx_COMPARE (CCGCmode,
	operand0,
	operand1)),
		gen_rtx_CLOBBER (VOIDmode,
	copy_rtx (operand0)))));
  _val = get_insns ();
  end_sequence ();
  return _val;
}

/* ../../gcc/gcc/config/i386/i386.md:17625 */
extern rtx gen_peephole2_1498 PARAMS ((rtx, rtx *));
rtx
gen_peephole2_1498 (curr_insn, operands)
     rtx curr_insn ATTRIBUTE_UNUSED;
     rtx *operands;
{
  rtx operand0;
  rtx operand1;
  rtx _val = 0;
  HARD_REG_SET _regs_allocated;
  CLEAR_HARD_REG_SET (_regs_allocated);
  start_sequence ();

  operand0 = operands[0];
  operand1 = operands[1];
  emit (gen_rtx_PARALLEL (VOIDmode,
	gen_rtvec (2,
		gen_rtx_SET (VOIDmode,
	gen_rtx_REG (CCGCmode,
	17),
	gen_rtx_COMPARE (CCGCmode,
	operand0,
	operand1)),
		gen_rtx_CLOBBER (VOIDmode,
	copy_rtx (operand0)))));
  _val = get_insns ();
  end_sequence ();
  return _val;
}

/* ../../gcc/gcc/config/i386/i386.md:17638 */
extern rtx gen_peephole2_1499 PARAMS ((rtx, rtx *));
rtx
gen_peephole2_1499 (curr_insn, operands)
     rtx curr_insn ATTRIBUTE_UNUSED;
     rtx *operands;
{
  rtx operand0;
  rtx _val = 0;
  HARD_REG_SET _regs_allocated;
  CLEAR_HARD_REG_SET (_regs_allocated);
  start_sequence ();

  operand0 = operands[0];
  emit (gen_rtx_PARALLEL (VOIDmode,
	gen_rtvec (2,
		gen_rtx_SET (VOIDmode,
	gen_rtx_REG (CCGCmode,
	17),
	gen_rtx_COMPARE (CCGCmode,
	operand0,
	GEN_INT (128LL))),
		gen_rtx_CLOBBER (VOIDmode,
	copy_rtx (operand0)))));
  _val = get_insns ();
  end_sequence ();
  return _val;
}

/* ../../gcc/gcc/config/i386/i386.md:17650 */
extern rtx gen_peephole2_1500 PARAMS ((rtx, rtx *));
rtx
gen_peephole2_1500 (curr_insn, operands)
     rtx curr_insn ATTRIBUTE_UNUSED;
     rtx *operands;
{
  rtx operand0;
  rtx _val = 0;
  HARD_REG_SET _regs_allocated;
  CLEAR_HARD_REG_SET (_regs_allocated);
  start_sequence ();

  operand0 = operands[0];
  emit (gen_rtx_PARALLEL (VOIDmode,
	gen_rtvec (2,
		gen_rtx_SET (VOIDmode,
	gen_rtx_REG (CCGCmode,
	17),
	gen_rtx_COMPARE (CCGCmode,
	operand0,
	GEN_INT (128LL))),
		gen_rtx_CLOBBER (VOIDmode,
	copy_rtx (operand0)))));
  _val = get_insns ();
  end_sequence ();
  return _val;
}

/* ../../gcc/gcc/config/i386/i386.md:17662 */
extern rtx gen_peephole2_1501 PARAMS ((rtx, rtx *));
rtx
gen_peephole2_1501 (curr_insn, operands)
     rtx curr_insn ATTRIBUTE_UNUSED;
     rtx *operands;
{
  rtx operand0;
  rtx _val = 0;
  HARD_REG_SET _regs_allocated;
  CLEAR_HARD_REG_SET (_regs_allocated);
  if ((operands[0] = peep2_find_free_register (0, 0, "r", DImode, &_regs_allocated)) == NULL_RTX)
    return NULL;
  start_sequence ();
  operand0 = operands[0];
  emit_insn (gen_rtx_CLOBBER (VOIDmode,
	operand0));
  emit (gen_rtx_PARALLEL (VOIDmode,
	gen_rtvec (2,
		gen_rtx_SET (VOIDmode,
	gen_rtx_MEM (DImode,
	gen_rtx_PRE_DEC (DImode,
	gen_rtx_REG (DImode,
	7))),
	copy_rtx (operand0)),
		gen_rtx_CLOBBER (VOIDmode,
	gen_rtx_MEM (BLKmode,
	gen_rtx_SCRATCH (VOIDmode))))));
  _val = get_insns ();
  end_sequence ();
  return _val;
}

/* ../../gcc/gcc/config/i386/i386.md:17672 */
extern rtx gen_peephole2_1502 PARAMS ((rtx, rtx *));
rtx
gen_peephole2_1502 (curr_insn, operands)
     rtx curr_insn ATTRIBUTE_UNUSED;
     rtx *operands;
{
  rtx operand0;
  rtx _val = 0;
  HARD_REG_SET _regs_allocated;
  CLEAR_HARD_REG_SET (_regs_allocated);
  if ((operands[0] = peep2_find_free_register (0, 0, "r", DImode, &_regs_allocated)) == NULL_RTX)
    return NULL;
  start_sequence ();
  operand0 = operands[0];
  emit_insn (gen_rtx_CLOBBER (VOIDmode,
	operand0));
  emit_insn (gen_rtx_SET (VOIDmode,
	gen_rtx_MEM (DImode,
	gen_rtx_PRE_DEC (DImode,
	gen_rtx_REG (DImode,
	7))),
	copy_rtx (operand0)));
  emit (gen_rtx_PARALLEL (VOIDmode,
	gen_rtvec (2,
		gen_rtx_SET (VOIDmode,
	gen_rtx_MEM (DImode,
	gen_rtx_PRE_DEC (DImode,
	gen_rtx_REG (DImode,
	7))),
	copy_rtx (operand0)),
		gen_rtx_CLOBBER (VOIDmode,
	gen_rtx_MEM (BLKmode,
	gen_rtx_SCRATCH (VOIDmode))))));
  _val = get_insns ();
  end_sequence ();
  return _val;
}

/* ../../gcc/gcc/config/i386/i386.md:17684 */
extern rtx gen_peephole2_1503 PARAMS ((rtx, rtx *));
rtx
gen_peephole2_1503 (curr_insn, operands)
     rtx curr_insn ATTRIBUTE_UNUSED;
     rtx *operands;
{
  rtx operand0;
  rtx _val = 0;
  HARD_REG_SET _regs_allocated;
  CLEAR_HARD_REG_SET (_regs_allocated);
  if ((operands[0] = peep2_find_free_register (0, 0, "r", DImode, &_regs_allocated)) == NULL_RTX)
    return NULL;
  start_sequence ();
  operand0 = operands[0];
  emit_insn (gen_rtx_CLOBBER (VOIDmode,
	operand0));
  emit_insn (gen_rtx_SET (VOIDmode,
	gen_rtx_MEM (DImode,
	gen_rtx_PRE_DEC (DImode,
	gen_rtx_REG (DImode,
	7))),
	copy_rtx (operand0)));
  _val = get_insns ();
  end_sequence ();
  return _val;
}

/* ../../gcc/gcc/config/i386/i386.md:17692 */
extern rtx gen_peephole2_1504 PARAMS ((rtx, rtx *));
rtx
gen_peephole2_1504 (curr_insn, operands)
     rtx curr_insn ATTRIBUTE_UNUSED;
     rtx *operands;
{
  rtx operand0;
  rtx _val = 0;
  HARD_REG_SET _regs_allocated;
  CLEAR_HARD_REG_SET (_regs_allocated);
  if ((operands[0] = peep2_find_free_register (0, 0, "r", DImode, &_regs_allocated)) == NULL_RTX)
    return NULL;
  start_sequence ();
  operand0 = operands[0];
  emit_insn (gen_rtx_CLOBBER (VOIDmode,
	operand0));
  emit_insn (gen_rtx_SET (VOIDmode,
	gen_rtx_MEM (DImode,
	gen_rtx_PRE_DEC (DImode,
	gen_rtx_REG (DImode,
	7))),
	copy_rtx (operand0)));
  emit_insn (gen_rtx_SET (VOIDmode,
	gen_rtx_MEM (DImode,
	gen_rtx_PRE_DEC (DImode,
	gen_rtx_REG (DImode,
	7))),
	copy_rtx (operand0)));
  _val = get_insns ();
  end_sequence ();
  return _val;
}

/* ../../gcc/gcc/config/i386/i386.md:17702 */
extern rtx gen_peephole2_1505 PARAMS ((rtx, rtx *));
rtx
gen_peephole2_1505 (curr_insn, operands)
     rtx curr_insn ATTRIBUTE_UNUSED;
     rtx *operands;
{
  rtx operand0;
  rtx _val = 0;
  HARD_REG_SET _regs_allocated;
  CLEAR_HARD_REG_SET (_regs_allocated);
  if ((operands[0] = peep2_find_free_register (0, 0, "r", DImode, &_regs_allocated)) == NULL_RTX)
    return NULL;
  start_sequence ();

  operand0 = operands[0];
  emit (gen_rtx_PARALLEL (VOIDmode,
	gen_rtvec (3,
		gen_rtx_SET (VOIDmode,
	operand0,
	gen_rtx_MEM (DImode,
	gen_rtx_REG (DImode,
	7))),
		gen_rtx_SET (VOIDmode,
	gen_rtx_REG (DImode,
	7),
	gen_rtx_PLUS (DImode,
	gen_rtx_REG (DImode,
	7),
	GEN_INT (8LL))),
		gen_rtx_CLOBBER (VOIDmode,
	gen_rtx_MEM (BLKmode,
	gen_rtx_SCRATCH (VOIDmode))))));
  _val = get_insns ();
  end_sequence ();
  return _val;
}

/* ../../gcc/gcc/config/i386/i386.md:17715 */
extern rtx gen_peephole2_1506 PARAMS ((rtx, rtx *));
rtx
gen_peephole2_1506 (curr_insn, operands)
     rtx curr_insn ATTRIBUTE_UNUSED;
     rtx *operands;
{
  rtx operand0;
  rtx operand1;
  rtx _val = 0;
  HARD_REG_SET _regs_allocated;
  CLEAR_HARD_REG_SET (_regs_allocated);
  if ((operands[0] = peep2_find_free_register (0, 0, "r", DImode, &_regs_allocated)) == NULL_RTX)
    return NULL;
  if ((operands[1] = peep2_find_free_register (0, 0, "r", DImode, &_regs_allocated)) == NULL_RTX)
    return NULL;
  start_sequence ();

  operand0 = operands[0];
  operand1 = operands[1];
  emit (gen_rtx_PARALLEL (VOIDmode,
	gen_rtvec (3,
		gen_rtx_SET (VOIDmode,
	operand0,
	gen_rtx_MEM (DImode,
	gen_rtx_REG (DImode,
	7))),
		gen_rtx_SET (VOIDmode,
	gen_rtx_REG (DImode,
	7),
	gen_rtx_PLUS (DImode,
	gen_rtx_REG (DImode,
	7),
	GEN_INT (8LL))),
		gen_rtx_CLOBBER (VOIDmode,
	gen_rtx_MEM (BLKmode,
	gen_rtx_SCRATCH (VOIDmode))))));
  emit (gen_rtx_PARALLEL (VOIDmode,
	gen_rtvec (2,
		gen_rtx_SET (VOIDmode,
	operand1,
	gen_rtx_MEM (DImode,
	gen_rtx_REG (DImode,
	7))),
		gen_rtx_SET (VOIDmode,
	gen_rtx_REG (DImode,
	7),
	gen_rtx_PLUS (DImode,
	gen_rtx_REG (DImode,
	7),
	GEN_INT (8LL))))));
  _val = get_insns ();
  end_sequence ();
  return _val;
}

/* ../../gcc/gcc/config/i386/i386.md:17729 */
extern rtx gen_peephole2_1507 PARAMS ((rtx, rtx *));
rtx
gen_peephole2_1507 (curr_insn, operands)
     rtx curr_insn ATTRIBUTE_UNUSED;
     rtx *operands;
{
  rtx operand0;
  rtx _val = 0;
  HARD_REG_SET _regs_allocated;
  CLEAR_HARD_REG_SET (_regs_allocated);
  if ((operands[0] = peep2_find_free_register (0, 0, "r", DImode, &_regs_allocated)) == NULL_RTX)
    return NULL;
  start_sequence ();

  operand0 = operands[0];
  emit (gen_rtx_PARALLEL (VOIDmode,
	gen_rtvec (3,
		gen_rtx_SET (VOIDmode,
	operand0,
	gen_rtx_MEM (DImode,
	gen_rtx_REG (DImode,
	7))),
		gen_rtx_SET (VOIDmode,
	gen_rtx_REG (DImode,
	7),
	gen_rtx_PLUS (DImode,
	gen_rtx_REG (DImode,
	7),
	GEN_INT (8LL))),
		gen_rtx_CLOBBER (VOIDmode,
	gen_rtx_MEM (BLKmode,
	gen_rtx_SCRATCH (VOIDmode))))));
  emit (gen_rtx_PARALLEL (VOIDmode,
	gen_rtvec (2,
		gen_rtx_SET (VOIDmode,
	copy_rtx (operand0),
	gen_rtx_MEM (DImode,
	gen_rtx_REG (DImode,
	7))),
		gen_rtx_SET (VOIDmode,
	gen_rtx_REG (DImode,
	7),
	gen_rtx_PLUS (DImode,
	gen_rtx_REG (DImode,
	7),
	GEN_INT (8LL))))));
  _val = get_insns ();
  end_sequence ();
  return _val;
}

/* ../../gcc/gcc/config/i386/i386.md:17743 */
extern rtx gen_peephole2_1508 PARAMS ((rtx, rtx *));
rtx
gen_peephole2_1508 (curr_insn, operands)
     rtx curr_insn ATTRIBUTE_UNUSED;
     rtx *operands;
{
  rtx operand0;
  rtx _val = 0;
  HARD_REG_SET _regs_allocated;
  CLEAR_HARD_REG_SET (_regs_allocated);
  if ((operands[0] = peep2_find_free_register (0, 0, "r", DImode, &_regs_allocated)) == NULL_RTX)
    return NULL;
  start_sequence ();

  operand0 = operands[0];
  emit (gen_rtx_PARALLEL (VOIDmode,
	gen_rtvec (2,
		gen_rtx_SET (VOIDmode,
	operand0,
	gen_rtx_MEM (DImode,
	gen_rtx_REG (DImode,
	7))),
		gen_rtx_SET (VOIDmode,
	gen_rtx_REG (DImode,
	7),
	gen_rtx_PLUS (DImode,
	gen_rtx_REG (DImode,
	7),
	GEN_INT (8LL))))));
  _val = get_insns ();
  end_sequence ();
  return _val;
}

/* ../../gcc/gcc/config/i386/i386.md:17754 */
extern rtx gen_peephole2_1509 PARAMS ((rtx, rtx *));
rtx
gen_peephole2_1509 (curr_insn, operands)
     rtx curr_insn ATTRIBUTE_UNUSED;
     rtx *operands;
{
  rtx operand0;
  rtx operand1;
  rtx _val = 0;
  HARD_REG_SET _regs_allocated;
  CLEAR_HARD_REG_SET (_regs_allocated);
  if ((operands[0] = peep2_find_free_register (0, 0, "r", DImode, &_regs_allocated)) == NULL_RTX)
    return NULL;
  if ((operands[1] = peep2_find_free_register (0, 0, "r", DImode, &_regs_allocated)) == NULL_RTX)
    return NULL;
  start_sequence ();

  operand0 = operands[0];
  operand1 = operands[1];
  emit (gen_rtx_PARALLEL (VOIDmode,
	gen_rtvec (2,
		gen_rtx_SET (VOIDmode,
	operand0,
	gen_rtx_MEM (DImode,
	gen_rtx_REG (DImode,
	7))),
		gen_rtx_SET (VOIDmode,
	gen_rtx_REG (DImode,
	7),
	gen_rtx_PLUS (DImode,
	gen_rtx_REG (DImode,
	7),
	GEN_INT (8LL))))));
  emit (gen_rtx_PARALLEL (VOIDmode,
	gen_rtvec (2,
		gen_rtx_SET (VOIDmode,
	operand1,
	gen_rtx_MEM (DImode,
	gen_rtx_REG (DImode,
	7))),
		gen_rtx_SET (VOIDmode,
	gen_rtx_REG (DImode,
	7),
	gen_rtx_PLUS (DImode,
	gen_rtx_REG (DImode,
	7),
	GEN_INT (8LL))))));
  _val = get_insns ();
  end_sequence ();
  return _val;
}

/* ../../gcc/gcc/config/i386/i386.md:17766 */
extern rtx gen_peephole2_1510 PARAMS ((rtx, rtx *));
rtx
gen_peephole2_1510 (curr_insn, operands)
     rtx curr_insn ATTRIBUTE_UNUSED;
     rtx *operands;
{
  rtx operand0;
  rtx _val = 0;
  HARD_REG_SET _regs_allocated;
  CLEAR_HARD_REG_SET (_regs_allocated);
  if ((operands[0] = peep2_find_free_register (0, 0, "r", DImode, &_regs_allocated)) == NULL_RTX)
    return NULL;
  start_sequence ();

  operand0 = operands[0];
  emit (gen_rtx_PARALLEL (VOIDmode,
	gen_rtvec (2,
		gen_rtx_SET (VOIDmode,
	operand0,
	gen_rtx_MEM (DImode,
	gen_rtx_REG (DImode,
	7))),
		gen_rtx_SET (VOIDmode,
	gen_rtx_REG (DImode,
	7),
	gen_rtx_PLUS (DImode,
	gen_rtx_REG (DImode,
	7),
	GEN_INT (8LL))))));
  emit (gen_rtx_PARALLEL (VOIDmode,
	gen_rtvec (2,
		gen_rtx_SET (VOIDmode,
	copy_rtx (operand0),
	gen_rtx_MEM (DImode,
	gen_rtx_REG (DImode,
	7))),
		gen_rtx_SET (VOIDmode,
	gen_rtx_REG (DImode,
	7),
	gen_rtx_PLUS (DImode,
	gen_rtx_REG (DImode,
	7),
	GEN_INT (8LL))))));
  _val = get_insns ();
  end_sequence ();
  return _val;
}

/* ../../gcc/gcc/config/i386/i386.md:17905 */
rtx
gen_conditional_trap (operand0, operand1)
     rtx operand0;
     rtx operand1;
{
  rtx operand2;
  rtx _val = 0;
  start_sequence ();
  {
    rtx operands[3];
    operands[0] = operand0;
    operands[1] = operand1;
{
  emit_insn (gen_rtx_TRAP_IF (VOIDmode,
			      ix86_expand_compare (GET_CODE (operands[0]),
						   NULL, NULL),
			      operands[1]));
  DONE;
}
    operand0 = operands[0];
    operand1 = operands[1];
    operand2 = operands[2];
  }
  emit_insn (gen_rtx_TRAP_IF (VOIDmode,
	gen_rtx (GET_CODE (operand0), VOIDmode,
		operand2,
		const0_rtx),
	operand1));
  _val = get_insns ();
  end_sequence ();
  return _val;
}

/* ../../gcc/gcc/config/i386/i386.md:18019 */
rtx
gen_movti (operand0, operand1)
     rtx operand0;
     rtx operand1;
{
  rtx _val = 0;
  start_sequence ();
  {
    rtx operands[2];
    operands[0] = operand0;
    operands[1] = operand1;
{
  if (TARGET_64BIT)
    ix86_expand_move (TImode, operands);
  else
    ix86_expand_vector_move (TImode, operands);
  DONE;
}
    operand0 = operands[0];
    operand1 = operands[1];
  }
  emit_insn (gen_rtx_SET (VOIDmode,
	operand0,
	operand1));
  _val = get_insns ();
  end_sequence ();
  return _val;
}

/* ../../gcc/gcc/config/i386/i386.md:18067 */
rtx
gen_movv2df (operand0, operand1)
     rtx operand0;
     rtx operand1;
{
  rtx _val = 0;
  start_sequence ();
  {
    rtx operands[2];
    operands[0] = operand0;
    operands[1] = operand1;
{
  ix86_expand_vector_move (V2DFmode, operands);
  DONE;
}
    operand0 = operands[0];
    operand1 = operands[1];
  }
  emit_insn (gen_rtx_SET (VOIDmode,
	operand0,
	operand1));
  _val = get_insns ();
  end_sequence ();
  return _val;
}

/* ../../gcc/gcc/config/i386/i386.md:18076 */
rtx
gen_movv8hi (operand0, operand1)
     rtx operand0;
     rtx operand1;
{
  rtx _val = 0;
  start_sequence ();
  {
    rtx operands[2];
    operands[0] = operand0;
    operands[1] = operand1;
{
  ix86_expand_vector_move (V8HImode, operands);
  DONE;
}
    operand0 = operands[0];
    operand1 = operands[1];
  }
  emit_insn (gen_rtx_SET (VOIDmode,
	operand0,
	operand1));
  _val = get_insns ();
  end_sequence ();
  return _val;
}

/* ../../gcc/gcc/config/i386/i386.md:18085 */
rtx
gen_movv16qi (operand0, operand1)
     rtx operand0;
     rtx operand1;
{
  rtx _val = 0;
  start_sequence ();
  {
    rtx operands[2];
    operands[0] = operand0;
    operands[1] = operand1;
{
  ix86_expand_vector_move (V16QImode, operands);
  DONE;
}
    operand0 = operands[0];
    operand1 = operands[1];
  }
  emit_insn (gen_rtx_SET (VOIDmode,
	operand0,
	operand1));
  _val = get_insns ();
  end_sequence ();
  return _val;
}

/* ../../gcc/gcc/config/i386/i386.md:18094 */
rtx
gen_movv4sf (operand0, operand1)
     rtx operand0;
     rtx operand1;
{
  rtx _val = 0;
  start_sequence ();
  {
    rtx operands[2];
    operands[0] = operand0;
    operands[1] = operand1;
{
  ix86_expand_vector_move (V4SFmode, operands);
  DONE;
}
    operand0 = operands[0];
    operand1 = operands[1];
  }
  emit_insn (gen_rtx_SET (VOIDmode,
	operand0,
	operand1));
  _val = get_insns ();
  end_sequence ();
  return _val;
}

/* ../../gcc/gcc/config/i386/i386.md:18103 */
rtx
gen_movv4si (operand0, operand1)
     rtx operand0;
     rtx operand1;
{
  rtx _val = 0;
  start_sequence ();
  {
    rtx operands[2];
    operands[0] = operand0;
    operands[1] = operand1;
{
  ix86_expand_vector_move (V4SImode, operands);
  DONE;
}
    operand0 = operands[0];
    operand1 = operands[1];
  }
  emit_insn (gen_rtx_SET (VOIDmode,
	operand0,
	operand1));
  _val = get_insns ();
  end_sequence ();
  return _val;
}

/* ../../gcc/gcc/config/i386/i386.md:18112 */
rtx
gen_movv2di (operand0, operand1)
     rtx operand0;
     rtx operand1;
{
  rtx _val = 0;
  start_sequence ();
  {
    rtx operands[2];
    operands[0] = operand0;
    operands[1] = operand1;
{
  ix86_expand_vector_move (V2DImode, operands);
  DONE;
}
    operand0 = operands[0];
    operand1 = operands[1];
  }
  emit_insn (gen_rtx_SET (VOIDmode,
	operand0,
	operand1));
  _val = get_insns ();
  end_sequence ();
  return _val;
}

/* ../../gcc/gcc/config/i386/i386.md:18121 */
rtx
gen_movv2si (operand0, operand1)
     rtx operand0;
     rtx operand1;
{
  rtx _val = 0;
  start_sequence ();
  {
    rtx operands[2];
    operands[0] = operand0;
    operands[1] = operand1;
{
  ix86_expand_vector_move (V2SImode, operands);
  DONE;
}
    operand0 = operands[0];
    operand1 = operands[1];
  }
  emit_insn (gen_rtx_SET (VOIDmode,
	operand0,
	operand1));
  _val = get_insns ();
  end_sequence ();
  return _val;
}

/* ../../gcc/gcc/config/i386/i386.md:18130 */
rtx
gen_movv4hi (operand0, operand1)
     rtx operand0;
     rtx operand1;
{
  rtx _val = 0;
  start_sequence ();
  {
    rtx operands[2];
    operands[0] = operand0;
    operands[1] = operand1;
{
  ix86_expand_vector_move (V4HImode, operands);
  DONE;
}
    operand0 = operands[0];
    operand1 = operands[1];
  }
  emit_insn (gen_rtx_SET (VOIDmode,
	operand0,
	operand1));
  _val = get_insns ();
  end_sequence ();
  return _val;
}

/* ../../gcc/gcc/config/i386/i386.md:18139 */
rtx
gen_movv8qi (operand0, operand1)
     rtx operand0;
     rtx operand1;
{
  rtx _val = 0;
  start_sequence ();
  {
    rtx operands[2];
    operands[0] = operand0;
    operands[1] = operand1;
{
  ix86_expand_vector_move (V8QImode, operands);
  DONE;
}
    operand0 = operands[0];
    operand1 = operands[1];
  }
  emit_insn (gen_rtx_SET (VOIDmode,
	operand0,
	operand1));
  _val = get_insns ();
  end_sequence ();
  return _val;
}

/* ../../gcc/gcc/config/i386/i386.md:18148 */
rtx
gen_movv2sf (operand0, operand1)
     rtx operand0;
     rtx operand1;
{
  rtx _val = 0;
  start_sequence ();
  {
    rtx operands[2];
    operands[0] = operand0;
    operands[1] = operand1;
{
  ix86_expand_vector_move (V2SFmode, operands);
  DONE;
}
    operand0 = operands[0];
    operand1 = operands[1];
  }
  emit_insn (gen_rtx_SET (VOIDmode,
	operand0,
	operand1));
  _val = get_insns ();
  end_sequence ();
  return _val;
}

/* ../../gcc/gcc/config/i386/i386.md:18217 */
extern rtx gen_split_1523 PARAMS ((rtx *));
rtx
gen_split_1523 (operands)
      rtx *operands;
{
  rtx operand0;
  rtx operand1;
  rtx operand2;
  rtx operand3;
  rtx _val = 0;
  start_sequence ();
operands[2] = change_address (operands[0], GET_MODE (operands[0]),
				 stack_pointer_rtx);
   operands[3] = GEN_INT (-GET_MODE_SIZE (GET_MODE (operands[0])));
  operand0 = operands[0];
  operand1 = operands[1];
  operand2 = operands[2];
  operand3 = operands[3];
  emit_insn (gen_rtx_SET (VOIDmode,
	gen_rtx_REG (SImode,
	7),
	gen_rtx_PLUS (SImode,
	gen_rtx_REG (SImode,
	7),
	operand3)));
  emit_insn (gen_rtx_SET (VOIDmode,
	operand2,
	operand1));
  _val = get_insns ();
  end_sequence ();
  return _val;
}

/* ../../gcc/gcc/config/i386/i386.md:18228 */
extern rtx gen_split_1524 PARAMS ((rtx *));
rtx
gen_split_1524 (operands)
      rtx *operands;
{
  rtx operand0;
  rtx operand1;
  rtx operand2;
  rtx operand3;
  rtx _val = 0;
  start_sequence ();
operands[2] = change_address (operands[0], GET_MODE (operands[0]),
				 stack_pointer_rtx);
   operands[3] = GEN_INT (-GET_MODE_SIZE (GET_MODE (operands[0])));
  operand0 = operands[0];
  operand1 = operands[1];
  operand2 = operands[2];
  operand3 = operands[3];
  emit_insn (gen_rtx_SET (VOIDmode,
	gen_rtx_REG (DImode,
	7),
	gen_rtx_PLUS (DImode,
	gen_rtx_REG (DImode,
	7),
	operand3)));
  emit_insn (gen_rtx_SET (VOIDmode,
	operand2,
	operand1));
  _val = get_insns ();
  end_sequence ();
  return _val;
}

/* ../../gcc/gcc/config/i386/i386.md:18240 */
extern rtx gen_split_1525 PARAMS ((rtx *));
rtx
gen_split_1525 (operands)
      rtx *operands;
{
  rtx operand0;
  rtx operand1;
  rtx _val = 0;
  start_sequence ();

  operand0 = operands[0];
  operand1 = operands[1];
  emit_insn (gen_rtx_SET (VOIDmode,
	gen_rtx_REG (SImode,
	7),
	gen_rtx_PLUS (SImode,
	gen_rtx_REG (SImode,
	7),
	GEN_INT (-16LL))));
  emit_insn (gen_rtx_SET (VOIDmode,
	gen_rtx_MEM (TImode,
	gen_rtx_REG (SImode,
	7)),
	operand1));
  _val = get_insns ();
  end_sequence ();
  return _val;
}

/* ../../gcc/gcc/config/i386/i386.md:18251 */
extern rtx gen_split_1526 PARAMS ((rtx *));
rtx
gen_split_1526 (operands)
      rtx *operands;
{
  rtx operand0;
  rtx operand1;
  rtx _val = 0;
  start_sequence ();

  operand0 = operands[0];
  operand1 = operands[1];
  emit_insn (gen_rtx_SET (VOIDmode,
	gen_rtx_REG (SImode,
	7),
	gen_rtx_PLUS (SImode,
	gen_rtx_REG (SImode,
	7),
	GEN_INT (-16LL))));
  emit_insn (gen_rtx_SET (VOIDmode,
	gen_rtx_MEM (V2DFmode,
	gen_rtx_REG (SImode,
	7)),
	operand1));
  _val = get_insns ();
  end_sequence ();
  return _val;
}

/* ../../gcc/gcc/config/i386/i386.md:18262 */
extern rtx gen_split_1527 PARAMS ((rtx *));
rtx
gen_split_1527 (operands)
      rtx *operands;
{
  rtx operand0;
  rtx operand1;
  rtx _val = 0;
  start_sequence ();

  operand0 = operands[0];
  operand1 = operands[1];
  emit_insn (gen_rtx_SET (VOIDmode,
	gen_rtx_REG (SImode,
	7),
	gen_rtx_PLUS (SImode,
	gen_rtx_REG (SImode,
	7),
	GEN_INT (-16LL))));
  emit_insn (gen_rtx_SET (VOIDmode,
	gen_rtx_MEM (V2DImode,
	gen_rtx_REG (SImode,
	7)),
	operand1));
  _val = get_insns ();
  end_sequence ();
  return _val;
}

/* ../../gcc/gcc/config/i386/i386.md:18273 */
extern rtx gen_split_1528 PARAMS ((rtx *));
rtx
gen_split_1528 (operands)
      rtx *operands;
{
  rtx operand0;
  rtx operand1;
  rtx _val = 0;
  start_sequence ();

  operand0 = operands[0];
  operand1 = operands[1];
  emit_insn (gen_rtx_SET (VOIDmode,
	gen_rtx_REG (SImode,
	7),
	gen_rtx_PLUS (SImode,
	gen_rtx_REG (SImode,
	7),
	GEN_INT (-16LL))));
  emit_insn (gen_rtx_SET (VOIDmode,
	gen_rtx_MEM (V8HImode,
	gen_rtx_REG (SImode,
	7)),
	operand1));
  _val = get_insns ();
  end_sequence ();
  return _val;
}

/* ../../gcc/gcc/config/i386/i386.md:18284 */
extern rtx gen_split_1529 PARAMS ((rtx *));
rtx
gen_split_1529 (operands)
      rtx *operands;
{
  rtx operand0;
  rtx operand1;
  rtx _val = 0;
  start_sequence ();

  operand0 = operands[0];
  operand1 = operands[1];
  emit_insn (gen_rtx_SET (VOIDmode,
	gen_rtx_REG (SImode,
	7),
	gen_rtx_PLUS (SImode,
	gen_rtx_REG (SImode,
	7),
	GEN_INT (-16LL))));
  emit_insn (gen_rtx_SET (VOIDmode,
	gen_rtx_MEM (V16QImode,
	gen_rtx_REG (SImode,
	7)),
	operand1));
  _val = get_insns ();
  end_sequence ();
  return _val;
}

/* ../../gcc/gcc/config/i386/i386.md:18295 */
extern rtx gen_split_1530 PARAMS ((rtx *));
rtx
gen_split_1530 (operands)
      rtx *operands;
{
  rtx operand0;
  rtx operand1;
  rtx _val = 0;
  start_sequence ();

  operand0 = operands[0];
  operand1 = operands[1];
  emit_insn (gen_rtx_SET (VOIDmode,
	gen_rtx_REG (SImode,
	7),
	gen_rtx_PLUS (SImode,
	gen_rtx_REG (SImode,
	7),
	GEN_INT (-16LL))));
  emit_insn (gen_rtx_SET (VOIDmode,
	gen_rtx_MEM (V4SFmode,
	gen_rtx_REG (SImode,
	7)),
	operand1));
  _val = get_insns ();
  end_sequence ();
  return _val;
}

/* ../../gcc/gcc/config/i386/i386.md:18306 */
extern rtx gen_split_1531 PARAMS ((rtx *));
rtx
gen_split_1531 (operands)
      rtx *operands;
{
  rtx operand0;
  rtx operand1;
  rtx _val = 0;
  start_sequence ();

  operand0 = operands[0];
  operand1 = operands[1];
  emit_insn (gen_rtx_SET (VOIDmode,
	gen_rtx_REG (SImode,
	7),
	gen_rtx_PLUS (SImode,
	gen_rtx_REG (SImode,
	7),
	GEN_INT (-16LL))));
  emit_insn (gen_rtx_SET (VOIDmode,
	gen_rtx_MEM (V4SImode,
	gen_rtx_REG (SImode,
	7)),
	operand1));
  _val = get_insns ();
  end_sequence ();
  return _val;
}

/* ../../gcc/gcc/config/i386/i386.md:18317 */
extern rtx gen_split_1532 PARAMS ((rtx *));
rtx
gen_split_1532 (operands)
      rtx *operands;
{
  rtx operand0;
  rtx operand1;
  rtx _val = 0;
  start_sequence ();

  operand0 = operands[0];
  operand1 = operands[1];
  emit_insn (gen_rtx_SET (VOIDmode,
	gen_rtx_REG (SImode,
	7),
	gen_rtx_PLUS (SImode,
	gen_rtx_REG (SImode,
	7),
	GEN_INT (-8LL))));
  emit_insn (gen_rtx_SET (VOIDmode,
	gen_rtx_MEM (V2SImode,
	gen_rtx_REG (SImode,
	7)),
	operand1));
  _val = get_insns ();
  end_sequence ();
  return _val;
}

/* ../../gcc/gcc/config/i386/i386.md:18328 */
extern rtx gen_split_1533 PARAMS ((rtx *));
rtx
gen_split_1533 (operands)
      rtx *operands;
{
  rtx operand0;
  rtx operand1;
  rtx _val = 0;
  start_sequence ();

  operand0 = operands[0];
  operand1 = operands[1];
  emit_insn (gen_rtx_SET (VOIDmode,
	gen_rtx_REG (SImode,
	7),
	gen_rtx_PLUS (SImode,
	gen_rtx_REG (SImode,
	7),
	GEN_INT (-8LL))));
  emit_insn (gen_rtx_SET (VOIDmode,
	gen_rtx_MEM (V4HImode,
	gen_rtx_REG (SImode,
	7)),
	operand1));
  _val = get_insns ();
  end_sequence ();
  return _val;
}

/* ../../gcc/gcc/config/i386/i386.md:18339 */
extern rtx gen_split_1534 PARAMS ((rtx *));
rtx
gen_split_1534 (operands)
      rtx *operands;
{
  rtx operand0;
  rtx operand1;
  rtx _val = 0;
  start_sequence ();

  operand0 = operands[0];
  operand1 = operands[1];
  emit_insn (gen_rtx_SET (VOIDmode,
	gen_rtx_REG (SImode,
	7),
	gen_rtx_PLUS (SImode,
	gen_rtx_REG (SImode,
	7),
	GEN_INT (-8LL))));
  emit_insn (gen_rtx_SET (VOIDmode,
	gen_rtx_MEM (V8QImode,
	gen_rtx_REG (SImode,
	7)),
	operand1));
  _val = get_insns ();
  end_sequence ();
  return _val;
}

/* ../../gcc/gcc/config/i386/i386.md:18350 */
extern rtx gen_split_1535 PARAMS ((rtx *));
rtx
gen_split_1535 (operands)
      rtx *operands;
{
  rtx operand0;
  rtx operand1;
  rtx _val = 0;
  start_sequence ();

  operand0 = operands[0];
  operand1 = operands[1];
  emit_insn (gen_rtx_SET (VOIDmode,
	gen_rtx_REG (SImode,
	7),
	gen_rtx_PLUS (SImode,
	gen_rtx_REG (SImode,
	7),
	GEN_INT (-8LL))));
  emit_insn (gen_rtx_SET (VOIDmode,
	gen_rtx_MEM (V2SFmode,
	gen_rtx_REG (SImode,
	7)),
	operand1));
  _val = get_insns ();
  end_sequence ();
  return _val;
}

/* ../../gcc/gcc/config/i386/i386.md:18387 */
extern rtx gen_split_1536 PARAMS ((rtx *));
rtx
gen_split_1536 (operands)
      rtx *operands ATTRIBUTE_UNUSED;
{
  rtx _val = 0;
  start_sequence ();
ix86_split_long_move (operands); DONE;
  emit_insn (const0_rtx);
  _val = get_insns ();
  end_sequence ();
  return _val;
}

/* ../../gcc/gcc/config/i386/i386.md:18397 */
rtx
gen_sse_movaps (operand0, operand1)
     rtx operand0;
     rtx operand1;
{
  rtx _val = 0;
  start_sequence ();
  {
    rtx operands[2];
    operands[0] = operand0;
    operands[1] = operand1;
{
  if (GET_CODE (operands[0]) == MEM && GET_CODE (operands[1]) == MEM)
    {
      rtx tmp = gen_reg_rtx (V4SFmode);
      emit_insn (gen_sse_movaps (tmp, operands[1]));
      emit_move_insn (operands[0], tmp);
      DONE;
    }
}
    operand0 = operands[0];
    operand1 = operands[1];
  }
  emit_insn (gen_rtx_SET (VOIDmode,
	operand0,
	gen_rtx_UNSPEC (V4SFmode,
	gen_rtvec (1,
		operand1),
	38)));
  _val = get_insns ();
  end_sequence ();
  return _val;
}

/* ../../gcc/gcc/config/i386/i386.md:18422 */
rtx
gen_sse_movups (operand0, operand1)
     rtx operand0;
     rtx operand1;
{
  rtx _val = 0;
  start_sequence ();
  {
    rtx operands[2];
    operands[0] = operand0;
    operands[1] = operand1;
{
  if (GET_CODE (operands[0]) == MEM && GET_CODE (operands[1]) == MEM)
    {
      rtx tmp = gen_reg_rtx (V4SFmode);
      emit_insn (gen_sse_movups (tmp, operands[1]));
      emit_move_insn (operands[0], tmp);
      DONE;
    }
}
    operand0 = operands[0];
    operand1 = operands[1];
  }
  emit_insn (gen_rtx_SET (VOIDmode,
	operand0,
	gen_rtx_UNSPEC (V4SFmode,
	gen_rtvec (1,
		operand1),
	39)));
  _val = get_insns ();
  end_sequence ();
  return _val;
}

/* ../../gcc/gcc/config/i386/i386.md:18562 */
rtx
gen_sse_loadss (operand0, operand1)
     rtx operand0;
     rtx operand1;
{
  rtx _val = 0;
  start_sequence ();
  {
    rtx operands[2];
    operands[0] = operand0;
    operands[1] = operand1;
{
  emit_insn (gen_sse_loadss_1 (operands[0], operands[1],
			       CONST0_RTX (V4SFmode)));
  DONE;
}
    operand0 = operands[0];
    operand1 = operands[1];
  }
  emit (operand0);
  emit (operand1);
  _val = get_insns ();
  end_sequence ();
  return _val;
}

/* ../../gcc/gcc/config/i386/i386.md:18798 */
rtx
gen_sse_andv4sf3 (operand0, operand1, operand2)
     rtx operand0;
     rtx operand1;
     rtx operand2;
{
  return gen_rtx_SET (VOIDmode,
	gen_rtx_SUBREG (TImode,
	operand0,
	0),
	gen_rtx_AND (TImode,
	gen_rtx_SUBREG (TImode,
	operand1,
	0),
	gen_rtx_SUBREG (TImode,
	operand2,
	0)));
}

/* ../../gcc/gcc/config/i386/i386.md:18825 */
rtx
gen_sse_nandv4sf3 (operand0, operand1, operand2)
     rtx operand0;
     rtx operand1;
     rtx operand2;
{
  return gen_rtx_SET (VOIDmode,
	gen_rtx_SUBREG (TImode,
	operand0,
	0),
	gen_rtx_AND (TImode,
	gen_rtx_NOT (TImode,
	gen_rtx_SUBREG (TImode,
	operand1,
	0)),
	gen_rtx_SUBREG (TImode,
	operand2,
	0)));
}

/* ../../gcc/gcc/config/i386/i386.md:18850 */
rtx
gen_sse_iorv4sf3 (operand0, operand1, operand2)
     rtx operand0;
     rtx operand1;
     rtx operand2;
{
  return gen_rtx_SET (VOIDmode,
	gen_rtx_SUBREG (TImode,
	operand0,
	0),
	gen_rtx_IOR (TImode,
	gen_rtx_SUBREG (TImode,
	operand1,
	0),
	gen_rtx_SUBREG (TImode,
	operand2,
	0)));
}

/* ../../gcc/gcc/config/i386/i386.md:18877 */
rtx
gen_sse_xorv4sf3 (operand0, operand1, operand2)
     rtx operand0;
     rtx operand1;
     rtx operand2;
{
  return gen_rtx_SET (VOIDmode,
	gen_rtx_SUBREG (TImode,
	operand0,
	0),
	gen_rtx_XOR (TImode,
	gen_rtx_SUBREG (TImode,
	operand1,
	0),
	gen_rtx_SUBREG (TImode,
	operand2,
	0)));
}

/* ../../gcc/gcc/config/i386/i386.md:18907 */
rtx
gen_sse2_andv2df3 (operand0, operand1, operand2)
     rtx operand0;
     rtx operand1;
     rtx operand2;
{
  return gen_rtx_SET (VOIDmode,
	gen_rtx_SUBREG (TImode,
	operand0,
	0),
	gen_rtx_AND (TImode,
	gen_rtx_SUBREG (TImode,
	operand1,
	0),
	gen_rtx_SUBREG (TImode,
	operand2,
	0)));
}

/* ../../gcc/gcc/config/i386/i386.md:18934 */
rtx
gen_sse2_nandv2df3 (operand0, operand1, operand2)
     rtx operand0;
     rtx operand1;
     rtx operand2;
{
  return gen_rtx_SET (VOIDmode,
	gen_rtx_SUBREG (TImode,
	operand0,
	0),
	gen_rtx_AND (TImode,
	gen_rtx_NOT (TImode,
	gen_rtx_SUBREG (TImode,
	operand1,
	0)),
	gen_rtx_SUBREG (TImode,
	operand2,
	0)));
}

/* ../../gcc/gcc/config/i386/i386.md:18959 */
rtx
gen_sse2_iorv2df3 (operand0, operand1, operand2)
     rtx operand0;
     rtx operand1;
     rtx operand2;
{
  return gen_rtx_SET (VOIDmode,
	gen_rtx_SUBREG (TImode,
	operand0,
	0),
	gen_rtx_IOR (TImode,
	gen_rtx_SUBREG (TImode,
	operand1,
	0),
	gen_rtx_SUBREG (TImode,
	operand2,
	0)));
}

/* ../../gcc/gcc/config/i386/i386.md:18986 */
rtx
gen_sse2_xorv2df3 (operand0, operand1, operand2)
     rtx operand0;
     rtx operand1;
     rtx operand2;
{
  return gen_rtx_SET (VOIDmode,
	gen_rtx_SUBREG (TImode,
	operand0,
	0),
	gen_rtx_XOR (TImode,
	gen_rtx_SUBREG (TImode,
	operand1,
	0),
	gen_rtx_SUBREG (TImode,
	operand2,
	0)));
}

/* ../../gcc/gcc/config/i386/i386.md:20119 */
rtx
gen_sfence ()
{
  rtx operand0;
  rtx _val = 0;
  start_sequence ();
  {
    rtx operands[1];
{
  operands[0] = gen_rtx_MEM (BLKmode, gen_rtx_SCRATCH (Pmode));
  MEM_VOLATILE_P (operands[0]) = 1;
}
    operand0 = operands[0];
  }
  emit_insn (gen_rtx_SET (VOIDmode,
	operand0,
	gen_rtx_UNSPEC (BLKmode,
	gen_rtvec (1,
		operand0),
	44)));
  _val = get_insns ();
  end_sequence ();
  return _val;
}

/* ../../gcc/gcc/config/i386/i386.md:20136 */
rtx
gen_sse_prologue_save (operand0, operand1, operand2, operand3)
     rtx operand0;
     rtx operand1;
     rtx operand2;
     rtx operand3;
{
  return gen_rtx_PARALLEL (VOIDmode,
	gen_rtvec (4,
		gen_rtx_SET (VOIDmode,
	operand0,
	gen_rtx_UNSPEC (BLKmode,
	gen_rtvec (8,
		gen_rtx_REG (DImode,
	21),
		gen_rtx_REG (DImode,
	22),
		gen_rtx_REG (DImode,
	23),
		gen_rtx_REG (DImode,
	24),
		gen_rtx_REG (DImode,
	25),
		gen_rtx_REG (DImode,
	26),
		gen_rtx_REG (DImode,
	27),
		gen_rtx_REG (DImode,
	28)),
	13)),
		gen_rtx_USE (VOIDmode,
	operand1),
		gen_rtx_USE (VOIDmode,
	operand2),
		gen_rtx_USE (VOIDmode,
	gen_rtx_LABEL_REF (DImode,
	operand3))));
}

/* ../../gcc/gcc/config/i386/i386.md:20502 */
rtx
gen_prefetch (operand0, operand1, operand2)
     rtx operand0;
     rtx operand1;
     rtx operand2;
{
  rtx _val = 0;
  start_sequence ();
  {
    rtx operands[3];
    operands[0] = operand0;
    operands[1] = operand1;
    operands[2] = operand2;
{
  int rw = INTVAL (operands[1]);
  int locality = INTVAL (operands[2]);

  if (rw != 0 && rw != 1)
    abort ();
  if (locality < 0 || locality > 3)
    abort ();
  if (GET_MODE (operands[0]) != Pmode && GET_MODE (operands[0]) != VOIDmode)
    abort ();

  /* Use 3dNOW prefetch in case we are asking for write prefetch not
     suported by SSE counterpart or the SSE prefetch is not available
     (K6 machines).  Otherwise use SSE prefetch as it allows specifying
     of locality.  */
  if (TARGET_3DNOW && (!TARGET_PREFETCH_SSE || rw))
    operands[2] = GEN_INT (3);
  else
    operands[1] = const0_rtx;
}
    operand0 = operands[0];
    operand1 = operands[1];
    operand2 = operands[2];
  }
  emit_insn (gen_rtx_PREFETCH (VOIDmode,
	operand0,
	operand1,
	operand2));
  _val = get_insns ();
  end_sequence ();
  return _val;
}

/* ../../gcc/gcc/config/i386/i386.md:22070 */
rtx
gen_sse2_loadsd (operand0, operand1)
     rtx operand0;
     rtx operand1;
{
  rtx _val = 0;
  start_sequence ();
  {
    rtx operands[2];
    operands[0] = operand0;
    operands[1] = operand1;
{
  emit_insn (gen_sse2_loadsd_1 (operands[0], operands[1],
			        CONST0_RTX (V2DFmode)));
  DONE;
}
    operand0 = operands[0];
    operand1 = operands[1];
  }
  emit (operand0);
  emit (operand1);
  _val = get_insns ();
  end_sequence ();
  return _val;
}

/* ../../gcc/gcc/config/i386/i386.md:22132 */
rtx
gen_sse2_mfence ()
{
  rtx operand0;
  rtx _val = 0;
  start_sequence ();
  {
    rtx operands[1];
{
  operands[0] = gen_rtx_MEM (BLKmode, gen_rtx_SCRATCH (Pmode));
  MEM_VOLATILE_P (operands[0]) = 1;
}
    operand0 = operands[0];
  }
  emit_insn (gen_rtx_SET (VOIDmode,
	operand0,
	gen_rtx_UNSPEC (BLKmode,
	gen_rtvec (1,
		operand0),
	59)));
  _val = get_insns ();
  end_sequence ();
  return _val;
}

/* ../../gcc/gcc/config/i386/i386.md:22149 */
rtx
gen_sse2_lfence ()
{
  rtx operand0;
  rtx _val = 0;
  start_sequence ();
  {
    rtx operands[1];
{
  operands[0] = gen_rtx_MEM (BLKmode, gen_rtx_SCRATCH (Pmode));
  MEM_VOLATILE_P (operands[0]) = 1;
}
    operand0 = operands[0];
  }
  emit_insn (gen_rtx_SET (VOIDmode,
	operand0,
	gen_rtx_UNSPEC (BLKmode,
	gen_rtvec (1,
		operand0),
	60)));
  _val = get_insns ();
  end_sequence ();
  return _val;
}



void
add_clobbers (pattern, insn_code_number)
     rtx pattern ATTRIBUTE_UNUSED;
     int insn_code_number;
{
  switch (insn_code_number)
    {
    case 864:
    case 850:
      XVECEXP (pattern, 0, 1) = gen_rtx_CLOBBER (VOIDmode,
	gen_rtx_REG (XFmode,
	8));
      XVECEXP (pattern, 0, 2) = gen_rtx_CLOBBER (VOIDmode,
	gen_rtx_REG (XFmode,
	9));
      XVECEXP (pattern, 0, 3) = gen_rtx_CLOBBER (VOIDmode,
	gen_rtx_REG (XFmode,
	10));
      XVECEXP (pattern, 0, 4) = gen_rtx_CLOBBER (VOIDmode,
	gen_rtx_REG (XFmode,
	11));
      XVECEXP (pattern, 0, 5) = gen_rtx_CLOBBER (VOIDmode,
	gen_rtx_REG (XFmode,
	12));
      XVECEXP (pattern, 0, 6) = gen_rtx_CLOBBER (VOIDmode,
	gen_rtx_REG (XFmode,
	13));
      XVECEXP (pattern, 0, 7) = gen_rtx_CLOBBER (VOIDmode,
	gen_rtx_REG (XFmode,
	14));
      XVECEXP (pattern, 0, 8) = gen_rtx_CLOBBER (VOIDmode,
	gen_rtx_REG (XFmode,
	15));
      XVECEXP (pattern, 0, 9) = gen_rtx_CLOBBER (VOIDmode,
	gen_rtx_REG (DImode,
	29));
      XVECEXP (pattern, 0, 10) = gen_rtx_CLOBBER (VOIDmode,
	gen_rtx_REG (DImode,
	30));
      XVECEXP (pattern, 0, 11) = gen_rtx_CLOBBER (VOIDmode,
	gen_rtx_REG (DImode,
	31));
      XVECEXP (pattern, 0, 12) = gen_rtx_CLOBBER (VOIDmode,
	gen_rtx_REG (DImode,
	32));
      XVECEXP (pattern, 0, 13) = gen_rtx_CLOBBER (VOIDmode,
	gen_rtx_REG (DImode,
	33));
      XVECEXP (pattern, 0, 14) = gen_rtx_CLOBBER (VOIDmode,
	gen_rtx_REG (DImode,
	34));
      XVECEXP (pattern, 0, 15) = gen_rtx_CLOBBER (VOIDmode,
	gen_rtx_REG (DImode,
	35));
      XVECEXP (pattern, 0, 16) = gen_rtx_CLOBBER (VOIDmode,
	gen_rtx_REG (DImode,
	36));
      break;

    case 663:
    case 662:
      XVECEXP (pattern, 0, 1) = gen_rtx_CLOBBER (VOIDmode,
	gen_rtx_SCRATCH (DFmode));
      XVECEXP (pattern, 0, 2) = gen_rtx_CLOBBER (VOIDmode,
	gen_rtx_REG (CCmode,
	17));
      break;

    case 661:
    case 660:
      XVECEXP (pattern, 0, 1) = gen_rtx_CLOBBER (VOIDmode,
	gen_rtx_SCRATCH (SFmode));
      XVECEXP (pattern, 0, 2) = gen_rtx_CLOBBER (VOIDmode,
	gen_rtx_REG (CCmode,
	17));
      break;

    case 543:
    case 541:
    case 540:
    case 538:
    case 537:
      XVECEXP (pattern, 0, 1) = gen_rtx_CLOBBER (VOIDmode,
	gen_rtx_SCRATCH (SImode));
      XVECEXP (pattern, 0, 2) = gen_rtx_CLOBBER (VOIDmode,
	gen_rtx_SCRATCH (SImode));
      XVECEXP (pattern, 0, 3) = gen_rtx_CLOBBER (VOIDmode,
	gen_rtx_REG (CCmode,
	17));
      break;

    case 520:
      XVECEXP (pattern, 0, 2) = gen_rtx_CLOBBER (VOIDmode,
	gen_rtx_SCRATCH (SImode));
      XVECEXP (pattern, 0, 3) = gen_rtx_CLOBBER (VOIDmode,
	gen_rtx_REG (CCmode,
	17));
      break;

    case 514:
    case 513:
    case 512:
    case 511:
      XVECEXP (pattern, 0, 1) = gen_rtx_CLOBBER (VOIDmode,
	gen_rtx_REG (CCFPmode,
	18));
      XVECEXP (pattern, 0, 2) = gen_rtx_CLOBBER (VOIDmode,
	gen_rtx_REG (CCFPmode,
	17));
      XVECEXP (pattern, 0, 3) = gen_rtx_CLOBBER (VOIDmode,
	gen_rtx_SCRATCH (HImode));
      break;

    case 510:
    case 509:
    case 508:
    case 507:
    case 506:
    case 505:
      XVECEXP (pattern, 0, 1) = gen_rtx_CLOBBER (VOIDmode,
	gen_rtx_REG (CCFPmode,
	18));
      XVECEXP (pattern, 0, 2) = gen_rtx_CLOBBER (VOIDmode,
	gen_rtx_REG (CCFPmode,
	17));
      break;

    case 673:
    case 672:
    case 635:
    case 634:
    case 275:
    case 274:
    case 272:
    case 269:
    case 266:
      XVECEXP (pattern, 0, 3) = gen_rtx_CLOBBER (VOIDmode,
	gen_rtx_REG (CCmode,
	17));
      break;

    case 384:
    case 383:
    case 380:
    case 365:
    case 364:
    case 361:
    case 273:
    case 271:
    case 270:
    case 268:
    case 267:
    case 265:
    case 264:
      XVECEXP (pattern, 0, 2) = gen_rtx_CLOBBER (VOIDmode,
	gen_rtx_REG (CCmode,
	17));
      break;

    case 455:
    case 428:
    case 410:
    case 261:
    case 260:
    case 258:
    case 257:
      XVECEXP (pattern, 0, 1) = gen_rtx_CLOBBER (VOIDmode,
	gen_rtx_SCRATCH (SImode));
      XVECEXP (pattern, 0, 2) = gen_rtx_CLOBBER (VOIDmode,
	gen_rtx_REG (CCmode,
	17));
      break;

    case 259:
    case 256:
      XVECEXP (pattern, 0, 1) = gen_rtx_CLOBBER (VOIDmode,
	gen_rtx_SCRATCH (DImode));
      XVECEXP (pattern, 0, 2) = gen_rtx_CLOBBER (VOIDmode,
	gen_rtx_REG (CCmode,
	17));
      break;

    case 346:
    case 320:
    case 221:
    case 220:
    case 219:
      XVECEXP (pattern, 0, 1) = gen_rtx_CLOBBER (VOIDmode,
	gen_rtx_SCRATCH (QImode));
      break;

    case 337:
    case 315:
    case 214:
    case 213:
    case 212:
      XVECEXP (pattern, 0, 1) = gen_rtx_CLOBBER (VOIDmode,
	gen_rtx_SCRATCH (HImode));
      break;

    case 334:
    case 312:
    case 208:
    case 207:
    case 205:
      XVECEXP (pattern, 0, 1) = gen_rtx_CLOBBER (VOIDmode,
	gen_rtx_SCRATCH (SImode));
      break;

    case 327:
    case 305:
    case 200:
    case 199:
    case 198:
      XVECEXP (pattern, 0, 1) = gen_rtx_CLOBBER (VOIDmode,
	gen_rtx_SCRATCH (DImode));
      break;

    case 148:
      XVECEXP (pattern, 0, 3) = gen_rtx_CLOBBER (VOIDmode,
	gen_rtx_SCRATCH (DFmode));
      break;

    case 147:
      XVECEXP (pattern, 0, 4) = gen_rtx_CLOBBER (VOIDmode,
	gen_rtx_SCRATCH (DFmode));
      break;

    case 118:
      XVECEXP (pattern, 0, 1) = gen_rtx_CLOBBER (VOIDmode,
	gen_rtx_REG (CCmode,
	17));
      XVECEXP (pattern, 0, 2) = gen_rtx_CLOBBER (VOIDmode,
	gen_rtx_SCRATCH (SImode));
      break;

    case 656:
    case 655:
    case 653:
    case 652:
    case 650:
    case 649:
    case 647:
    case 646:
    case 638:
    case 636:
    case 547:
    case 546:
    case 531:
    case 498:
    case 497:
    case 496:
    case 495:
    case 494:
    case 493:
    case 492:
    case 491:
    case 490:
    case 489:
    case 488:
    case 487:
    case 486:
    case 485:
    case 484:
    case 483:
    case 482:
    case 481:
    case 480:
    case 479:
    case 478:
    case 477:
    case 476:
    case 475:
    case 472:
    case 471:
    case 470:
    case 469:
    case 466:
    case 465:
    case 460:
    case 459:
    case 458:
    case 457:
    case 456:
    case 452:
    case 451:
    case 448:
    case 447:
    case 446:
    case 445:
    case 442:
    case 441:
    case 436:
    case 435:
    case 434:
    case 433:
    case 432:
    case 431:
    case 430:
    case 429:
    case 425:
    case 424:
    case 423:
    case 421:
    case 420:
    case 418:
    case 417:
    case 414:
    case 413:
    case 412:
    case 411:
    case 408:
    case 388:
    case 387:
    case 386:
    case 385:
    case 382:
    case 381:
    case 379:
    case 369:
    case 368:
    case 367:
    case 366:
    case 363:
    case 362:
    case 360:
    case 358:
    case 356:
    case 353:
    case 352:
    case 350:
    case 349:
    case 343:
    case 342:
    case 341:
    case 340:
    case 339:
    case 338:
    case 335:
    case 330:
    case 329:
    case 328:
    case 325:
    case 324:
    case 323:
    case 322:
    case 321:
    case 317:
    case 316:
    case 313:
    case 308:
    case 307:
    case 306:
    case 303:
    case 302:
    case 301:
    case 300:
    case 298:
    case 295:
    case 294:
    case 292:
    case 289:
    case 288:
    case 286:
    case 263:
    case 262:
    case 255:
    case 254:
    case 253:
    case 252:
    case 251:
    case 250:
    case 249:
    case 248:
    case 247:
    case 246:
    case 245:
    case 242:
    case 241:
    case 238:
    case 233:
    case 232:
    case 231:
    case 230:
    case 227:
    case 226:
    case 225:
    case 224:
    case 223:
    case 222:
    case 217:
    case 216:
    case 215:
    case 210:
    case 209:
    case 202:
    case 201:
    case 196:
    case 183:
    case 182:
    case 180:
    case 179:
    case 114:
    case 112:
    case 111:
    case 109:
    case 108:
    case 106:
    case 81:
    case 80:
    case 62:
    case 56:
    case 43:
    case 42:
      XVECEXP (pattern, 0, 1) = gen_rtx_CLOBBER (VOIDmode,
	gen_rtx_REG (CCmode,
	17));
      break;

    default:
      abort ();
    }
}


int
added_clobbers_hard_reg_p (insn_code_number)
     int insn_code_number;
{
  switch (insn_code_number)
    {
    case 346:
    case 320:
    case 221:
    case 220:
    case 219:
    case 337:
    case 315:
    case 214:
    case 213:
    case 212:
    case 334:
    case 312:
    case 208:
    case 207:
    case 205:
    case 327:
    case 305:
    case 200:
    case 199:
    case 198:
    case 148:
    case 147:
      return 0;

    case 864:
    case 850:
    case 663:
    case 662:
    case 661:
    case 660:
    case 543:
    case 541:
    case 540:
    case 538:
    case 537:
    case 520:
    case 514:
    case 513:
    case 512:
    case 511:
    case 510:
    case 509:
    case 508:
    case 507:
    case 506:
    case 505:
    case 673:
    case 672:
    case 635:
    case 634:
    case 275:
    case 274:
    case 272:
    case 269:
    case 266:
    case 384:
    case 383:
    case 380:
    case 365:
    case 364:
    case 361:
    case 273:
    case 271:
    case 270:
    case 268:
    case 267:
    case 265:
    case 264:
    case 455:
    case 428:
    case 410:
    case 261:
    case 260:
    case 258:
    case 257:
    case 259:
    case 256:
    case 118:
    case 656:
    case 655:
    case 653:
    case 652:
    case 650:
    case 649:
    case 647:
    case 646:
    case 638:
    case 636:
    case 547:
    case 546:
    case 531:
    case 498:
    case 497:
    case 496:
    case 495:
    case 494:
    case 493:
    case 492:
    case 491:
    case 490:
    case 489:
    case 488:
    case 487:
    case 486:
    case 485:
    case 484:
    case 483:
    case 482:
    case 481:
    case 480:
    case 479:
    case 478:
    case 477:
    case 476:
    case 475:
    case 472:
    case 471:
    case 470:
    case 469:
    case 466:
    case 465:
    case 460:
    case 459:
    case 458:
    case 457:
    case 456:
    case 452:
    case 451:
    case 448:
    case 447:
    case 446:
    case 445:
    case 442:
    case 441:
    case 436:
    case 435:
    case 434:
    case 433:
    case 432:
    case 431:
    case 430:
    case 429:
    case 425:
    case 424:
    case 423:
    case 421:
    case 420:
    case 418:
    case 417:
    case 414:
    case 413:
    case 412:
    case 411:
    case 408:
    case 388:
    case 387:
    case 386:
    case 385:
    case 382:
    case 381:
    case 379:
    case 369:
    case 368:
    case 367:
    case 366:
    case 363:
    case 362:
    case 360:
    case 358:
    case 356:
    case 353:
    case 352:
    case 350:
    case 349:
    case 343:
    case 342:
    case 341:
    case 340:
    case 339:
    case 338:
    case 335:
    case 330:
    case 329:
    case 328:
    case 325:
    case 324:
    case 323:
    case 322:
    case 321:
    case 317:
    case 316:
    case 313:
    case 308:
    case 307:
    case 306:
    case 303:
    case 302:
    case 301:
    case 300:
    case 298:
    case 295:
    case 294:
    case 292:
    case 289:
    case 288:
    case 286:
    case 263:
    case 262:
    case 255:
    case 254:
    case 253:
    case 252:
    case 251:
    case 250:
    case 249:
    case 248:
    case 247:
    case 246:
    case 245:
    case 242:
    case 241:
    case 238:
    case 233:
    case 232:
    case 231:
    case 230:
    case 227:
    case 226:
    case 225:
    case 224:
    case 223:
    case 222:
    case 217:
    case 216:
    case 215:
    case 210:
    case 209:
    case 202:
    case 201:
    case 196:
    case 183:
    case 182:
    case 180:
    case 179:
    case 114:
    case 112:
    case 111:
    case 109:
    case 108:
    case 106:
    case 81:
    case 80:
    case 62:
    case 56:
    case 43:
    case 42:
      return 1;

    default:
      abort ();
    }
}
