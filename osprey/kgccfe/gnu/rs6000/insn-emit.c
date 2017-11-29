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

/* /home/yu/gcc/gcc_powerpc/crosstool-0.42/build/powerpc-7450-linux-gnu/gcc-3.3.1-glibc-2.3.2/gcc-3.3.1/gcc/config/rs6000/rs6000.md:927 */
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

/* /home/yu/gcc/gcc_powerpc/crosstool-0.42/build/powerpc-7450-linux-gnu/gcc-3.3.1-glibc-2.3.2/gcc-3.3.1/gcc/config/rs6000/rs6000.md:1335 */
rtx
gen_extendqisi2_ppc (operand0, operand1)
     rtx operand0;
     rtx operand1;
{
  return gen_rtx_SET (VOIDmode,
	operand0,
	gen_rtx_SIGN_EXTEND (SImode,
	operand1));
}

/* /home/yu/gcc/gcc_powerpc/crosstool-0.42/build/powerpc-7450-linux-gnu/gcc-3.3.1-glibc-2.3.2/gcc-3.3.1/gcc/config/rs6000/rs6000.md:1501 */
rtx
gen_extendqihi2_ppc (operand0, operand1)
     rtx operand0;
     rtx operand1;
{
  return gen_rtx_SET (VOIDmode,
	operand0,
	gen_rtx_SIGN_EXTEND (HImode,
	operand1));
}

/* /home/yu/gcc/gcc_powerpc/crosstool-0.42/build/powerpc-7450-linux-gnu/gcc-3.3.1-glibc-2.3.2/gcc-3.3.1/gcc/config/rs6000/rs6000.md:1855 */
rtx
gen_one_cmplsi2 (operand0, operand1)
     rtx operand0;
     rtx operand1;
{
  return gen_rtx_SET (VOIDmode,
	operand0,
	gen_rtx_NOT (SImode,
	operand1));
}

/* /home/yu/gcc/gcc_powerpc/crosstool-0.42/build/powerpc-7450-linux-gnu/gcc-3.3.1-glibc-2.3.2/gcc-3.3.1/gcc/config/rs6000/rs6000.md:2279 */
rtx
gen_abssi2_nopower (operand0, operand1)
     rtx operand0;
     rtx operand1;
{
  return gen_rtx_PARALLEL (VOIDmode, gen_rtvec (2,
		gen_rtx_SET (VOIDmode,
	operand0,
	gen_rtx_ABS (SImode,
	operand1)),
		gen_rtx_CLOBBER (VOIDmode,
	gen_rtx_SCRATCH (SImode))));
}

/* /home/yu/gcc/gcc_powerpc/crosstool-0.42/build/powerpc-7450-linux-gnu/gcc-3.3.1-glibc-2.3.2/gcc-3.3.1/gcc/config/rs6000/rs6000.md:2309 */
rtx
gen_negsi2 (operand0, operand1)
     rtx operand0;
     rtx operand1;
{
  return gen_rtx_SET (VOIDmode,
	operand0,
	gen_rtx_NEG (SImode,
	operand1));
}

/* /home/yu/gcc/gcc_powerpc/crosstool-0.42/build/powerpc-7450-linux-gnu/gcc-3.3.1-glibc-2.3.2/gcc-3.3.1/gcc/config/rs6000/rs6000.md:2367 */
rtx
gen_ffssi2 (operand0, operand1)
     rtx operand0;
     rtx operand1;
{
  return gen_rtx_SET (VOIDmode,
	operand0,
	gen_rtx_FFS (SImode,
	operand1));
}

/* /home/yu/gcc/gcc_powerpc/crosstool-0.42/build/powerpc-7450-linux-gnu/gcc-3.3.1-glibc-2.3.2/gcc-3.3.1/gcc/config/rs6000/rs6000.md:2388 */
rtx
gen_mulsi3_mq (operand0, operand1, operand2)
     rtx operand0;
     rtx operand1;
     rtx operand2;
{
  return gen_rtx_PARALLEL (VOIDmode, gen_rtvec (2,
		gen_rtx_SET (VOIDmode,
	operand0,
	gen_rtx_MULT (SImode,
	operand1,
	operand2)),
		gen_rtx_CLOBBER (VOIDmode,
	gen_rtx_SCRATCH (SImode))));
}

/* /home/yu/gcc/gcc_powerpc/crosstool-0.42/build/powerpc-7450-linux-gnu/gcc-3.3.1-glibc-2.3.2/gcc-3.3.1/gcc/config/rs6000/rs6000.md:2404 */
rtx
gen_mulsi3_no_mq (operand0, operand1, operand2)
     rtx operand0;
     rtx operand1;
     rtx operand2;
{
  return gen_rtx_SET (VOIDmode,
	operand0,
	gen_rtx_MULT (SImode,
	operand1,
	operand2));
}

/* /home/yu/gcc/gcc_powerpc/crosstool-0.42/build/powerpc-7450-linux-gnu/gcc-3.3.1-glibc-2.3.2/gcc-3.3.1/gcc/config/rs6000/rs6000.md:2593 */
rtx
gen_udivsi3_mq (operand0, operand1, operand2)
     rtx operand0;
     rtx operand1;
     rtx operand2;
{
  return gen_rtx_PARALLEL (VOIDmode, gen_rtvec (2,
		gen_rtx_SET (VOIDmode,
	operand0,
	gen_rtx_UDIV (SImode,
	operand1,
	operand2)),
		gen_rtx_CLOBBER (VOIDmode,
	gen_rtx_SCRATCH (SImode))));
}

/* /home/yu/gcc/gcc_powerpc/crosstool-0.42/build/powerpc-7450-linux-gnu/gcc-3.3.1-glibc-2.3.2/gcc-3.3.1/gcc/config/rs6000/rs6000.md:2646 */
rtx
gen_divsi3_mq (operand0, operand1, operand2)
     rtx operand0;
     rtx operand1;
     rtx operand2;
{
  return gen_rtx_PARALLEL (VOIDmode, gen_rtvec (2,
		gen_rtx_SET (VOIDmode,
	operand0,
	gen_rtx_DIV (SImode,
	operand1,
	operand2)),
		gen_rtx_CLOBBER (VOIDmode,
	gen_rtx_SCRATCH (SImode))));
}

/* /home/yu/gcc/gcc_powerpc/crosstool-0.42/build/powerpc-7450-linux-gnu/gcc-3.3.1-glibc-2.3.2/gcc-3.3.1/gcc/config/rs6000/rs6000.md:2858 */
rtx
gen_mulh_call ()
{
  return gen_rtx_PARALLEL (VOIDmode, gen_rtvec (2,
		gen_rtx_SET (VOIDmode,
	gen_rtx_REG (SImode,
	3),
	gen_rtx_TRUNCATE (SImode,
	gen_rtx_LSHIFTRT (DImode,
	gen_rtx_MULT (DImode,
	gen_rtx_SIGN_EXTEND (DImode,
	gen_rtx_REG (SImode,
	3)),
	gen_rtx_SIGN_EXTEND (DImode,
	gen_rtx_REG (SImode,
	4))),
	GEN_INT (32LL)))),
		gen_rtx_CLOBBER (VOIDmode,
	gen_rtx_SCRATCH (SImode))));
}

/* /home/yu/gcc/gcc_powerpc/crosstool-0.42/build/powerpc-7450-linux-gnu/gcc-3.3.1-glibc-2.3.2/gcc-3.3.1/gcc/config/rs6000/rs6000.md:2869 */
rtx
gen_mull_call ()
{
  return gen_rtx_PARALLEL (VOIDmode, gen_rtvec (3,
		gen_rtx_SET (VOIDmode,
	gen_rtx_REG (DImode,
	3),
	gen_rtx_MULT (DImode,
	gen_rtx_SIGN_EXTEND (DImode,
	gen_rtx_REG (SImode,
	3)),
	gen_rtx_SIGN_EXTEND (DImode,
	gen_rtx_REG (SImode,
	4)))),
		gen_rtx_CLOBBER (VOIDmode,
	gen_rtx_SCRATCH (SImode)),
		gen_rtx_CLOBBER (VOIDmode,
	gen_rtx_REG (SImode,
	0))));
}

/* /home/yu/gcc/gcc_powerpc/crosstool-0.42/build/powerpc-7450-linux-gnu/gcc-3.3.1-glibc-2.3.2/gcc-3.3.1/gcc/config/rs6000/rs6000.md:2879 */
rtx
gen_divss_call ()
{
  return gen_rtx_PARALLEL (VOIDmode, gen_rtvec (4,
		gen_rtx_SET (VOIDmode,
	gen_rtx_REG (SImode,
	3),
	gen_rtx_DIV (SImode,
	gen_rtx_REG (SImode,
	3),
	gen_rtx_REG (SImode,
	4))),
		gen_rtx_SET (VOIDmode,
	gen_rtx_REG (SImode,
	4),
	gen_rtx_MOD (SImode,
	gen_rtx_REG (SImode,
	3),
	gen_rtx_REG (SImode,
	4))),
		gen_rtx_CLOBBER (VOIDmode,
	gen_rtx_SCRATCH (SImode)),
		gen_rtx_CLOBBER (VOIDmode,
	gen_rtx_REG (SImode,
	0))));
}

/* /home/yu/gcc/gcc_powerpc/crosstool-0.42/build/powerpc-7450-linux-gnu/gcc-3.3.1-glibc-2.3.2/gcc-3.3.1/gcc/config/rs6000/rs6000.md:2890 */
rtx
gen_divus_call ()
{
  return gen_rtx_PARALLEL (VOIDmode, gen_rtvec (6,
		gen_rtx_SET (VOIDmode,
	gen_rtx_REG (SImode,
	3),
	gen_rtx_UDIV (SImode,
	gen_rtx_REG (SImode,
	3),
	gen_rtx_REG (SImode,
	4))),
		gen_rtx_SET (VOIDmode,
	gen_rtx_REG (SImode,
	4),
	gen_rtx_UMOD (SImode,
	gen_rtx_REG (SImode,
	3),
	gen_rtx_REG (SImode,
	4))),
		gen_rtx_CLOBBER (VOIDmode,
	gen_rtx_SCRATCH (SImode)),
		gen_rtx_CLOBBER (VOIDmode,
	gen_rtx_REG (SImode,
	0)),
		gen_rtx_CLOBBER (VOIDmode,
	gen_rtx_SCRATCH (CCmode)),
		gen_rtx_CLOBBER (VOIDmode,
	gen_rtx_REG (CCmode,
	69))));
}

/* /home/yu/gcc/gcc_powerpc/crosstool-0.42/build/powerpc-7450-linux-gnu/gcc-3.3.1-glibc-2.3.2/gcc-3.3.1/gcc/config/rs6000/rs6000.md:2903 */
rtx
gen_quoss_call ()
{
  return gen_rtx_PARALLEL (VOIDmode, gen_rtvec (2,
		gen_rtx_SET (VOIDmode,
	gen_rtx_REG (SImode,
	3),
	gen_rtx_DIV (SImode,
	gen_rtx_REG (SImode,
	3),
	gen_rtx_REG (SImode,
	4))),
		gen_rtx_CLOBBER (VOIDmode,
	gen_rtx_SCRATCH (SImode))));
}

/* /home/yu/gcc/gcc_powerpc/crosstool-0.42/build/powerpc-7450-linux-gnu/gcc-3.3.1-glibc-2.3.2/gcc-3.3.1/gcc/config/rs6000/rs6000.md:2911 */
rtx
gen_quous_call ()
{
  return gen_rtx_PARALLEL (VOIDmode, gen_rtvec (5,
		gen_rtx_SET (VOIDmode,
	gen_rtx_REG (SImode,
	3),
	gen_rtx_UDIV (SImode,
	gen_rtx_REG (SImode,
	3),
	gen_rtx_REG (SImode,
	4))),
		gen_rtx_CLOBBER (VOIDmode,
	gen_rtx_SCRATCH (SImode)),
		gen_rtx_CLOBBER (VOIDmode,
	gen_rtx_REG (SImode,
	0)),
		gen_rtx_CLOBBER (VOIDmode,
	gen_rtx_SCRATCH (CCmode)),
		gen_rtx_CLOBBER (VOIDmode,
	gen_rtx_REG (CCmode,
	69))));
}

/* /home/yu/gcc/gcc_powerpc/crosstool-0.42/build/powerpc-7450-linux-gnu/gcc-3.3.1-glibc-2.3.2/gcc-3.3.1/gcc/config/rs6000/rs6000.md:2928 */
rtx
gen_andsi3 (operand0, operand1, operand2)
     rtx operand0;
     rtx operand1;
     rtx operand2;
{
  return gen_rtx_PARALLEL (VOIDmode, gen_rtvec (2,
		gen_rtx_SET (VOIDmode,
	operand0,
	gen_rtx_AND (SImode,
	operand1,
	operand2)),
		gen_rtx_CLOBBER (VOIDmode,
	gen_rtx_SCRATCH (CCmode))));
}

/* /home/yu/gcc/gcc_powerpc/crosstool-0.42/build/powerpc-7450-linux-gnu/gcc-3.3.1-glibc-2.3.2/gcc-3.3.1/gcc/config/rs6000/rs6000.md:3663 */
rtx
gen_insvsi (operand0, operand1, operand2, operand3)
     rtx operand0;
     rtx operand1;
     rtx operand2;
     rtx operand3;
{
  return gen_rtx_SET (VOIDmode,
	gen_rtx_ZERO_EXTRACT (SImode,
	operand0,
	operand1,
	operand2),
	operand3);
}

/* /home/yu/gcc/gcc_powerpc/crosstool-0.42/build/powerpc-7450-linux-gnu/gcc-3.3.1-glibc-2.3.2/gcc-3.3.1/gcc/config/rs6000/rs6000.md:3754 */
rtx
gen_insvdi (operand0, operand1, operand2, operand3)
     rtx operand0;
     rtx operand1;
     rtx operand2;
     rtx operand3;
{
  return gen_rtx_SET (VOIDmode,
	gen_rtx_ZERO_EXTRACT (DImode,
	operand0,
	operand1,
	operand2),
	operand3);
}

/* /home/yu/gcc/gcc_powerpc/crosstool-0.42/build/powerpc-7450-linux-gnu/gcc-3.3.1-glibc-2.3.2/gcc-3.3.1/gcc/config/rs6000/rs6000.md:3791 */
rtx
gen_extzvsi (operand0, operand1, operand2, operand3)
     rtx operand0;
     rtx operand1;
     rtx operand2;
     rtx operand3;
{
  return gen_rtx_SET (VOIDmode,
	operand0,
	gen_rtx_ZERO_EXTRACT (SImode,
	operand1,
	operand2,
	operand3));
}

/* /home/yu/gcc/gcc_powerpc/crosstool-0.42/build/powerpc-7450-linux-gnu/gcc-3.3.1-glibc-2.3.2/gcc-3.3.1/gcc/config/rs6000/rs6000.md:3918 */
rtx
gen_extzvdi (operand0, operand1, operand2, operand3)
     rtx operand0;
     rtx operand1;
     rtx operand2;
     rtx operand3;
{
  return gen_rtx_SET (VOIDmode,
	operand0,
	gen_rtx_ZERO_EXTRACT (DImode,
	operand1,
	operand2,
	operand3));
}

/* /home/yu/gcc/gcc_powerpc/crosstool-0.42/build/powerpc-7450-linux-gnu/gcc-3.3.1-glibc-2.3.2/gcc-3.3.1/gcc/config/rs6000/rs6000.md:3980 */
rtx
gen_rotlsi3 (operand0, operand1, operand2)
     rtx operand0;
     rtx operand1;
     rtx operand2;
{
  return gen_rtx_SET (VOIDmode,
	operand0,
	gen_rtx_ROTATE (SImode,
	operand1,
	operand2));
}

/* /home/yu/gcc/gcc_powerpc/crosstool-0.42/build/powerpc-7450-linux-gnu/gcc-3.3.1-glibc-2.3.2/gcc-3.3.1/gcc/config/rs6000/rs6000.md:4284 */
rtx
gen_ashlsi3_power (operand0, operand1, operand2)
     rtx operand0;
     rtx operand1;
     rtx operand2;
{
  return gen_rtx_PARALLEL (VOIDmode, gen_rtvec (2,
		gen_rtx_SET (VOIDmode,
	operand0,
	gen_rtx_ASHIFT (SImode,
	operand1,
	operand2)),
		gen_rtx_CLOBBER (VOIDmode,
	gen_rtx_SCRATCH (SImode))));
}

/* /home/yu/gcc/gcc_powerpc/crosstool-0.42/build/powerpc-7450-linux-gnu/gcc-3.3.1-glibc-2.3.2/gcc-3.3.1/gcc/config/rs6000/rs6000.md:4294 */
rtx
gen_ashlsi3_no_power (operand0, operand1, operand2)
     rtx operand0;
     rtx operand1;
     rtx operand2;
{
  return gen_rtx_SET (VOIDmode,
	operand0,
	gen_rtx_ASHIFT (SImode,
	operand1,
	operand2));
}

/* /home/yu/gcc/gcc_powerpc/crosstool-0.42/build/powerpc-7450-linux-gnu/gcc-3.3.1-glibc-2.3.2/gcc-3.3.1/gcc/config/rs6000/rs6000.md:4512 */
rtx
gen_lshrsi3_power (operand0, operand1, operand2)
     rtx operand0;
     rtx operand1;
     rtx operand2;
{
  return gen_rtx_PARALLEL (VOIDmode, gen_rtvec (2,
		gen_rtx_SET (VOIDmode,
	operand0,
	gen_rtx_LSHIFTRT (SImode,
	operand1,
	operand2)),
		gen_rtx_CLOBBER (VOIDmode,
	gen_rtx_SCRATCH (SImode))));
}

/* /home/yu/gcc/gcc_powerpc/crosstool-0.42/build/powerpc-7450-linux-gnu/gcc-3.3.1-glibc-2.3.2/gcc-3.3.1/gcc/config/rs6000/rs6000.md:4523 */
rtx
gen_lshrsi3_no_power (operand0, operand1, operand2)
     rtx operand0;
     rtx operand1;
     rtx operand2;
{
  return gen_rtx_SET (VOIDmode,
	operand0,
	gen_rtx_LSHIFTRT (SImode,
	operand1,
	operand2));
}

/* /home/yu/gcc/gcc_powerpc/crosstool-0.42/build/powerpc-7450-linux-gnu/gcc-3.3.1-glibc-2.3.2/gcc-3.3.1/gcc/config/rs6000/rs6000.md:4935 */
rtx
gen_ashrsi3_power (operand0, operand1, operand2)
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
	gen_rtx_SCRATCH (SImode))));
}

/* /home/yu/gcc/gcc_powerpc/crosstool-0.42/build/powerpc-7450-linux-gnu/gcc-3.3.1-glibc-2.3.2/gcc-3.3.1/gcc/config/rs6000/rs6000.md:4945 */
rtx
gen_ashrsi3_no_power (operand0, operand1, operand2)
     rtx operand0;
     rtx operand1;
     rtx operand2;
{
  return gen_rtx_SET (VOIDmode,
	operand0,
	gen_rtx_ASHIFTRT (SImode,
	operand1,
	operand2));
}

/* /home/yu/gcc/gcc_powerpc/crosstool-0.42/build/powerpc-7450-linux-gnu/gcc-3.3.1-glibc-2.3.2/gcc-3.3.1/gcc/config/rs6000/rs6000.md:5090 */
rtx
gen_extendsfdf2 (operand0, operand1)
     rtx operand0;
     rtx operand1;
{
  return gen_rtx_SET (VOIDmode,
	operand0,
	gen_rtx_FLOAT_EXTEND (DFmode,
	operand1));
}

/* /home/yu/gcc/gcc_powerpc/crosstool-0.42/build/powerpc-7450-linux-gnu/gcc-3.3.1-glibc-2.3.2/gcc-3.3.1/gcc/config/rs6000/rs6000.md:5103 */
rtx
gen_truncdfsf2 (operand0, operand1)
     rtx operand0;
     rtx operand1;
{
  return gen_rtx_SET (VOIDmode,
	operand0,
	gen_rtx_FLOAT_TRUNCATE (SFmode,
	operand1));
}

/* /home/yu/gcc/gcc_powerpc/crosstool-0.42/build/powerpc-7450-linux-gnu/gcc-3.3.1-glibc-2.3.2/gcc-3.3.1/gcc/config/rs6000/rs6000.md:5110 */
rtx
gen_aux_truncdfsf2 (operand0, operand1)
     rtx operand0;
     rtx operand1;
{
  return gen_rtx_SET (VOIDmode,
	operand0,
	gen_rtx_UNSPEC (SFmode,
	gen_rtvec (1,
		operand1),
	0));
}

/* /home/yu/gcc/gcc_powerpc/crosstool-0.42/build/powerpc-7450-linux-gnu/gcc-3.3.1-glibc-2.3.2/gcc-3.3.1/gcc/config/rs6000/rs6000.md:5493 */
rtx
gen_negdf2 (operand0, operand1)
     rtx operand0;
     rtx operand1;
{
  return gen_rtx_SET (VOIDmode,
	operand0,
	gen_rtx_NEG (DFmode,
	operand1));
}

/* /home/yu/gcc/gcc_powerpc/crosstool-0.42/build/powerpc-7450-linux-gnu/gcc-3.3.1-glibc-2.3.2/gcc-3.3.1/gcc/config/rs6000/rs6000.md:5500 */
rtx
gen_absdf2 (operand0, operand1)
     rtx operand0;
     rtx operand1;
{
  return gen_rtx_SET (VOIDmode,
	operand0,
	gen_rtx_ABS (DFmode,
	operand1));
}

/* /home/yu/gcc/gcc_powerpc/crosstool-0.42/build/powerpc-7450-linux-gnu/gcc-3.3.1-glibc-2.3.2/gcc-3.3.1/gcc/config/rs6000/rs6000.md:5514 */
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

/* /home/yu/gcc/gcc_powerpc/crosstool-0.42/build/powerpc-7450-linux-gnu/gcc-3.3.1-glibc-2.3.2/gcc-3.3.1/gcc/config/rs6000/rs6000.md:5522 */
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

/* /home/yu/gcc/gcc_powerpc/crosstool-0.42/build/powerpc-7450-linux-gnu/gcc-3.3.1-glibc-2.3.2/gcc-3.3.1/gcc/config/rs6000/rs6000.md:5530 */
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

/* /home/yu/gcc/gcc_powerpc/crosstool-0.42/build/powerpc-7450-linux-gnu/gcc-3.3.1-glibc-2.3.2/gcc-3.3.1/gcc/config/rs6000/rs6000.md:5538 */
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

/* /home/yu/gcc/gcc_powerpc/crosstool-0.42/build/powerpc-7450-linux-gnu/gcc-3.3.1-glibc-2.3.2/gcc-3.3.1/gcc/config/rs6000/rs6000.md:5604 */
rtx
gen_sqrtdf2 (operand0, operand1)
     rtx operand0;
     rtx operand1;
{
  return gen_rtx_SET (VOIDmode,
	operand0,
	gen_rtx_SQRT (DFmode,
	operand1));
}

/* /home/yu/gcc/gcc_powerpc/crosstool-0.42/build/powerpc-7450-linux-gnu/gcc-3.3.1-glibc-2.3.2/gcc-3.3.1/gcc/config/rs6000/rs6000.md:5902 */
rtx
gen_fctiwz (operand0, operand1)
     rtx operand0;
     rtx operand1;
{
  return gen_rtx_SET (VOIDmode,
	operand0,
	gen_rtx_UNSPEC (DImode,
	gen_rtvec (1,
		gen_rtx_FIX (SImode,
	operand1)),
	10));
}

/* /home/yu/gcc/gcc_powerpc/crosstool-0.42/build/powerpc-7450-linux-gnu/gcc-3.3.1-glibc-2.3.2/gcc-3.3.1/gcc/config/rs6000/rs6000.md:5915 */
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

/* /home/yu/gcc/gcc_powerpc/crosstool-0.42/build/powerpc-7450-linux-gnu/gcc-3.3.1-glibc-2.3.2/gcc-3.3.1/gcc/config/rs6000/rs6000.md:5922 */
rtx
gen_floatsidf_ppc64 (operand0, operand1, operand2, operand3, operand4)
     rtx operand0;
     rtx operand1;
     rtx operand2;
     rtx operand3;
     rtx operand4;
{
  return gen_rtx_PARALLEL (VOIDmode, gen_rtvec (4,
		gen_rtx_SET (VOIDmode,
	operand0,
	gen_rtx_FLOAT (DFmode,
	operand1)),
		gen_rtx_CLOBBER (VOIDmode,
	operand2),
		gen_rtx_CLOBBER (VOIDmode,
	operand3),
		gen_rtx_CLOBBER (VOIDmode,
	operand4)));
}

/* /home/yu/gcc/gcc_powerpc/crosstool-0.42/build/powerpc-7450-linux-gnu/gcc-3.3.1-glibc-2.3.2/gcc-3.3.1/gcc/config/rs6000/rs6000.md:5937 */
rtx
gen_floatunssidf_ppc64 (operand0, operand1, operand2, operand3, operand4)
     rtx operand0;
     rtx operand1;
     rtx operand2;
     rtx operand3;
     rtx operand4;
{
  return gen_rtx_PARALLEL (VOIDmode, gen_rtvec (4,
		gen_rtx_SET (VOIDmode,
	operand0,
	gen_rtx_UNSIGNED_FLOAT (DFmode,
	operand1)),
		gen_rtx_CLOBBER (VOIDmode,
	operand2),
		gen_rtx_CLOBBER (VOIDmode,
	operand3),
		gen_rtx_CLOBBER (VOIDmode,
	operand4)));
}

/* /home/yu/gcc/gcc_powerpc/crosstool-0.42/build/powerpc-7450-linux-gnu/gcc-3.3.1-glibc-2.3.2/gcc-3.3.1/gcc/config/rs6000/rs6000.md:5952 */
rtx
gen_fix_truncdfdi2 (operand0, operand1)
     rtx operand0;
     rtx operand1;
{
  return gen_rtx_SET (VOIDmode,
	operand0,
	gen_rtx_FIX (DImode,
	operand1));
}

/* /home/yu/gcc/gcc_powerpc/crosstool-0.42/build/powerpc-7450-linux-gnu/gcc-3.3.1-glibc-2.3.2/gcc-3.3.1/gcc/config/rs6000/rs6000.md:5978 */
rtx
gen_floatdisf2_internal1 (operand0, operand1)
     rtx operand0;
     rtx operand1;
{
  return gen_rtx_PARALLEL (VOIDmode, gen_rtvec (2,
		gen_rtx_SET (VOIDmode,
	operand0,
	gen_rtx_FLOAT (SFmode,
	operand1)),
		gen_rtx_CLOBBER (VOIDmode,
	gen_rtx_SCRATCH (DFmode))));
}

/* /home/yu/gcc/gcc_powerpc/crosstool-0.42/build/powerpc-7450-linux-gnu/gcc-3.3.1-glibc-2.3.2/gcc-3.3.1/gcc/config/rs6000/rs6000.md:6106 */
rtx
gen_mulsidi3_mq (operand0, operand1, operand2)
     rtx operand0;
     rtx operand1;
     rtx operand2;
{
  return gen_rtx_PARALLEL (VOIDmode, gen_rtvec (2,
		gen_rtx_SET (VOIDmode,
	operand0,
	gen_rtx_MULT (DImode,
	gen_rtx_SIGN_EXTEND (DImode,
	operand1),
	gen_rtx_SIGN_EXTEND (DImode,
	operand2))),
		gen_rtx_CLOBBER (VOIDmode,
	gen_rtx_SCRATCH (SImode))));
}

/* /home/yu/gcc/gcc_powerpc/crosstool-0.42/build/powerpc-7450-linux-gnu/gcc-3.3.1-glibc-2.3.2/gcc-3.3.1/gcc/config/rs6000/rs6000.md:6164 */
rtx
gen_umulsidi3_mq (operand0, operand1, operand2)
     rtx operand0;
     rtx operand1;
     rtx operand2;
{
  return gen_rtx_PARALLEL (VOIDmode, gen_rtvec (2,
		gen_rtx_SET (VOIDmode,
	operand0,
	gen_rtx_MULT (DImode,
	gen_rtx_ZERO_EXTEND (DImode,
	operand1),
	gen_rtx_ZERO_EXTEND (DImode,
	operand2))),
		gen_rtx_CLOBBER (VOIDmode,
	gen_rtx_SCRATCH (SImode))));
}

/* /home/yu/gcc/gcc_powerpc/crosstool-0.42/build/powerpc-7450-linux-gnu/gcc-3.3.1-glibc-2.3.2/gcc-3.3.1/gcc/config/rs6000/rs6000.md:6239 */
rtx
gen_smulsi3_highpart_mq (operand0, operand1, operand2)
     rtx operand0;
     rtx operand1;
     rtx operand2;
{
  return gen_rtx_PARALLEL (VOIDmode, gen_rtvec (2,
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
	gen_rtx_SCRATCH (SImode))));
}

/* /home/yu/gcc/gcc_powerpc/crosstool-0.42/build/powerpc-7450-linux-gnu/gcc-3.3.1-glibc-2.3.2/gcc-3.3.1/gcc/config/rs6000/rs6000.md:6282 */
rtx
gen_umulsi3_highpart_mq (operand0, operand1, operand2)
     rtx operand0;
     rtx operand1;
     rtx operand2;
{
  return gen_rtx_PARALLEL (VOIDmode, gen_rtvec (2,
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
	gen_rtx_SCRATCH (SImode))));
}

/* /home/yu/gcc/gcc_powerpc/crosstool-0.42/build/powerpc-7450-linux-gnu/gcc-3.3.1-glibc-2.3.2/gcc-3.3.1/gcc/config/rs6000/rs6000.md:6310 */
rtx
gen_ashldi3_power (operand0, operand1, operand2)
     rtx operand0;
     rtx operand1;
     rtx operand2;
{
  return gen_rtx_PARALLEL (VOIDmode, gen_rtvec (2,
		gen_rtx_SET (VOIDmode,
	operand0,
	gen_rtx_ASHIFT (DImode,
	operand1,
	operand2)),
		gen_rtx_CLOBBER (VOIDmode,
	gen_rtx_SCRATCH (SImode))));
}

/* /home/yu/gcc/gcc_powerpc/crosstool-0.42/build/powerpc-7450-linux-gnu/gcc-3.3.1-glibc-2.3.2/gcc-3.3.1/gcc/config/rs6000/rs6000.md:6323 */
rtx
gen_lshrdi3_power (operand0, operand1, operand2)
     rtx operand0;
     rtx operand1;
     rtx operand2;
{
  return gen_rtx_PARALLEL (VOIDmode, gen_rtvec (2,
		gen_rtx_SET (VOIDmode,
	operand0,
	gen_rtx_LSHIFTRT (DImode,
	operand1,
	operand2)),
		gen_rtx_CLOBBER (VOIDmode,
	gen_rtx_SCRATCH (SImode))));
}

/* /home/yu/gcc/gcc_powerpc/crosstool-0.42/build/powerpc-7450-linux-gnu/gcc-3.3.1-glibc-2.3.2/gcc-3.3.1/gcc/config/rs6000/rs6000.md:6338 */
rtx
gen_ashrdi3_power (operand0, operand1, operand2)
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
	gen_rtx_SCRATCH (SImode))));
}

/* /home/yu/gcc/gcc_powerpc/crosstool-0.42/build/powerpc-7450-linux-gnu/gcc-3.3.1-glibc-2.3.2/gcc-3.3.1/gcc/config/rs6000/rs6000.md:6349 */
rtx
gen_ashrdi3_no_power (operand0, operand1, operand2)
     rtx operand0;
     rtx operand1;
     rtx operand2;
{
  return gen_rtx_SET (VOIDmode,
	operand0,
	gen_rtx_ASHIFTRT (DImode,
	operand1,
	operand2));
}

/* /home/yu/gcc/gcc_powerpc/crosstool-0.42/build/powerpc-7450-linux-gnu/gcc-3.3.1-glibc-2.3.2/gcc-3.3.1/gcc/config/rs6000/rs6000.md:6501 */
rtx
gen_one_cmpldi2 (operand0, operand1)
     rtx operand0;
     rtx operand1;
{
  return gen_rtx_SET (VOIDmode,
	operand0,
	gen_rtx_NOT (DImode,
	operand1));
}

/* /home/yu/gcc/gcc_powerpc/crosstool-0.42/build/powerpc-7450-linux-gnu/gcc-3.3.1-glibc-2.3.2/gcc-3.3.1/gcc/config/rs6000/rs6000.md:6639 */
rtx
gen_absdi2 (operand0, operand1)
     rtx operand0;
     rtx operand1;
{
  return gen_rtx_PARALLEL (VOIDmode, gen_rtvec (2,
		gen_rtx_SET (VOIDmode,
	operand0,
	gen_rtx_ABS (DImode,
	operand1)),
		gen_rtx_CLOBBER (VOIDmode,
	gen_rtx_SCRATCH (DImode))));
}

/* /home/yu/gcc/gcc_powerpc/crosstool-0.42/build/powerpc-7450-linux-gnu/gcc-3.3.1-glibc-2.3.2/gcc-3.3.1/gcc/config/rs6000/rs6000.md:6727 */
rtx
gen_ffsdi2 (operand0, operand1)
     rtx operand0;
     rtx operand1;
{
  return gen_rtx_SET (VOIDmode,
	operand0,
	gen_rtx_FFS (DImode,
	operand1));
}

/* /home/yu/gcc/gcc_powerpc/crosstool-0.42/build/powerpc-7450-linux-gnu/gcc-3.3.1-glibc-2.3.2/gcc-3.3.1/gcc/config/rs6000/rs6000.md:6734 */
rtx
gen_muldi3 (operand0, operand1, operand2)
     rtx operand0;
     rtx operand1;
     rtx operand2;
{
  return gen_rtx_SET (VOIDmode,
	operand0,
	gen_rtx_MULT (DImode,
	operand1,
	operand2));
}

/* /home/yu/gcc/gcc_powerpc/crosstool-0.42/build/powerpc-7450-linux-gnu/gcc-3.3.1-glibc-2.3.2/gcc-3.3.1/gcc/config/rs6000/rs6000.md:6742 */
rtx
gen_smuldi3_highpart (operand0, operand1, operand2)
     rtx operand0;
     rtx operand1;
     rtx operand2;
{
  return gen_rtx_SET (VOIDmode,
	operand0,
	gen_rtx_TRUNCATE (DImode,
	gen_rtx_LSHIFTRT (TImode,
	gen_rtx_MULT (TImode,
	gen_rtx_SIGN_EXTEND (TImode,
	operand1),
	gen_rtx_SIGN_EXTEND (TImode,
	operand2)),
	GEN_INT (64LL))));
}

/* /home/yu/gcc/gcc_powerpc/crosstool-0.42/build/powerpc-7450-linux-gnu/gcc-3.3.1-glibc-2.3.2/gcc-3.3.1/gcc/config/rs6000/rs6000.md:6754 */
rtx
gen_umuldi3_highpart (operand0, operand1, operand2)
     rtx operand0;
     rtx operand1;
     rtx operand2;
{
  return gen_rtx_SET (VOIDmode,
	operand0,
	gen_rtx_TRUNCATE (DImode,
	gen_rtx_LSHIFTRT (TImode,
	gen_rtx_MULT (TImode,
	gen_rtx_ZERO_EXTEND (TImode,
	operand1),
	gen_rtx_ZERO_EXTEND (TImode,
	operand2)),
	GEN_INT (64LL))));
}

/* /home/yu/gcc/gcc_powerpc/crosstool-0.42/build/powerpc-7450-linux-gnu/gcc-3.3.1-glibc-2.3.2/gcc-3.3.1/gcc/config/rs6000/rs6000.md:6878 */
rtx
gen_udivdi3 (operand0, operand1, operand2)
     rtx operand0;
     rtx operand1;
     rtx operand2;
{
  return gen_rtx_SET (VOIDmode,
	operand0,
	gen_rtx_UDIV (DImode,
	operand1,
	operand2));
}

/* /home/yu/gcc/gcc_powerpc/crosstool-0.42/build/powerpc-7450-linux-gnu/gcc-3.3.1-glibc-2.3.2/gcc-3.3.1/gcc/config/rs6000/rs6000.md:6886 */
rtx
gen_rotldi3 (operand0, operand1, operand2)
     rtx operand0;
     rtx operand1;
     rtx operand2;
{
  return gen_rtx_SET (VOIDmode,
	operand0,
	gen_rtx_ROTATE (DImode,
	operand1,
	operand2));
}

/* /home/yu/gcc/gcc_powerpc/crosstool-0.42/build/powerpc-7450-linux-gnu/gcc-3.3.1-glibc-2.3.2/gcc-3.3.1/gcc/config/rs6000/rs6000.md:7338 */
rtx
gen_ashldi3_internal5 (operand0, operand1, operand2, operand3)
     rtx operand0;
     rtx operand1;
     rtx operand2;
     rtx operand3;
{
  return gen_rtx_PARALLEL (VOIDmode, gen_rtvec (2,
		gen_rtx_SET (VOIDmode,
	operand0,
	gen_rtx_COMPARE (CCmode,
	gen_rtx_AND (DImode,
	gen_rtx_ASHIFT (DImode,
	operand1,
	operand2),
	operand3),
	const0_rtx)),
		gen_rtx_CLOBBER (VOIDmode,
	gen_rtx_SCRATCH (DImode))));
}

/* /home/yu/gcc/gcc_powerpc/crosstool-0.42/build/powerpc-7450-linux-gnu/gcc-3.3.1-glibc-2.3.2/gcc-3.3.1/gcc/config/rs6000/rs6000.md:7414 */
rtx
gen_ashldi3_internal8 (operand0, operand1, operand2, operand3)
     rtx operand0;
     rtx operand1;
     rtx operand2;
     rtx operand3;
{
  return gen_rtx_PARALLEL (VOIDmode, gen_rtvec (2,
		gen_rtx_SET (VOIDmode,
	operand0,
	gen_rtx_COMPARE (CCmode,
	gen_rtx_AND (DImode,
	gen_rtx_ASHIFT (DImode,
	operand1,
	operand2),
	operand3),
	const0_rtx)),
		gen_rtx_CLOBBER (VOIDmode,
	gen_rtx_SCRATCH (DImode))));
}

/* /home/yu/gcc/gcc_powerpc/crosstool-0.42/build/powerpc-7450-linux-gnu/gcc-3.3.1-glibc-2.3.2/gcc-3.3.1/gcc/config/rs6000/rs6000.md:7649 */
rtx
gen_anddi3 (operand0, operand1, operand2)
     rtx operand0;
     rtx operand1;
     rtx operand2;
{
  return gen_rtx_PARALLEL (VOIDmode, gen_rtvec (2,
		gen_rtx_SET (VOIDmode,
	operand0,
	gen_rtx_AND (DImode,
	operand1,
	operand2)),
		gen_rtx_CLOBBER (VOIDmode,
	gen_rtx_SCRATCH (CCmode))));
}

/* /home/yu/gcc/gcc_powerpc/crosstool-0.42/build/powerpc-7450-linux-gnu/gcc-3.3.1-glibc-2.3.2/gcc-3.3.1/gcc/config/rs6000/rs6000.md:8124 */
rtx
gen_elf_high (operand0, operand1)
     rtx operand0;
     rtx operand1;
{
  return gen_rtx_SET (VOIDmode,
	operand0,
	gen_rtx_HIGH (SImode,
	operand1));
}

/* /home/yu/gcc/gcc_powerpc/crosstool-0.42/build/powerpc-7450-linux-gnu/gcc-3.3.1-glibc-2.3.2/gcc-3.3.1/gcc/config/rs6000/rs6000.md:8130 */
rtx
gen_elf_low (operand0, operand1, operand2)
     rtx operand0;
     rtx operand1;
     rtx operand2;
{
  return gen_rtx_SET (VOIDmode,
	operand0,
	gen_rtx_LO_SUM (SImode,
	operand1,
	operand2));
}

/* /home/yu/gcc/gcc_powerpc/crosstool-0.42/build/powerpc-7450-linux-gnu/gcc-3.3.1-glibc-2.3.2/gcc-3.3.1/gcc/config/rs6000/rs6000.md:8890 */
rtx
gen_extenddftf2 (operand0, operand1)
     rtx operand0;
     rtx operand1;
{
  return gen_rtx_SET (VOIDmode,
	operand0,
	gen_rtx_FLOAT_EXTEND (TFmode,
	operand1));
}

/* /home/yu/gcc/gcc_powerpc/crosstool-0.42/build/powerpc-7450-linux-gnu/gcc-3.3.1-glibc-2.3.2/gcc-3.3.1/gcc/config/rs6000/rs6000.md:8904 */
rtx
gen_extendsftf2 (operand0, operand1)
     rtx operand0;
     rtx operand1;
{
  return gen_rtx_SET (VOIDmode,
	operand0,
	gen_rtx_FLOAT_EXTEND (TFmode,
	operand1));
}

/* /home/yu/gcc/gcc_powerpc/crosstool-0.42/build/powerpc-7450-linux-gnu/gcc-3.3.1-glibc-2.3.2/gcc-3.3.1/gcc/config/rs6000/rs6000.md:8918 */
rtx
gen_trunctfdf2 (operand0, operand1)
     rtx operand0;
     rtx operand1;
{
  return gen_rtx_SET (VOIDmode,
	operand0,
	gen_rtx_FLOAT_TRUNCATE (DFmode,
	operand1));
}

/* /home/yu/gcc/gcc_powerpc/crosstool-0.42/build/powerpc-7450-linux-gnu/gcc-3.3.1-glibc-2.3.2/gcc-3.3.1/gcc/config/rs6000/rs6000.md:8927 */
rtx
gen_trunctfsf2 (operand0, operand1)
     rtx operand0;
     rtx operand1;
{
  return gen_rtx_PARALLEL (VOIDmode, gen_rtvec (2,
		gen_rtx_SET (VOIDmode,
	operand0,
	gen_rtx_FLOAT_TRUNCATE (SFmode,
	operand1)),
		gen_rtx_CLOBBER (VOIDmode,
	gen_rtx_SCRATCH (DFmode))));
}

/* /home/yu/gcc/gcc_powerpc/crosstool-0.42/build/powerpc-7450-linux-gnu/gcc-3.3.1-glibc-2.3.2/gcc-3.3.1/gcc/config/rs6000/rs6000.md:8941 */
rtx
gen_floatditf2 (operand0, operand1)
     rtx operand0;
     rtx operand1;
{
  return gen_rtx_PARALLEL (VOIDmode, gen_rtvec (2,
		gen_rtx_SET (VOIDmode,
	operand0,
	gen_rtx_FLOAT (TFmode,
	operand1)),
		gen_rtx_CLOBBER (VOIDmode,
	gen_rtx_SCRATCH (DFmode))));
}

/* /home/yu/gcc/gcc_powerpc/crosstool-0.42/build/powerpc-7450-linux-gnu/gcc-3.3.1-glibc-2.3.2/gcc-3.3.1/gcc/config/rs6000/rs6000.md:8955 */
rtx
gen_floatsitf2 (operand0, operand1)
     rtx operand0;
     rtx operand1;
{
  return gen_rtx_PARALLEL (VOIDmode, gen_rtvec (2,
		gen_rtx_SET (VOIDmode,
	operand0,
	gen_rtx_FLOAT (TFmode,
	operand1)),
		gen_rtx_CLOBBER (VOIDmode,
	gen_rtx_SCRATCH (DFmode))));
}

/* /home/yu/gcc/gcc_powerpc/crosstool-0.42/build/powerpc-7450-linux-gnu/gcc-3.3.1-glibc-2.3.2/gcc-3.3.1/gcc/config/rs6000/rs6000.md:8969 */
rtx
gen_fix_trunctfdi2 (operand0, operand1)
     rtx operand0;
     rtx operand1;
{
  return gen_rtx_PARALLEL (VOIDmode, gen_rtvec (2,
		gen_rtx_SET (VOIDmode,
	operand0,
	gen_rtx_FIX (DImode,
	operand1)),
		gen_rtx_CLOBBER (VOIDmode,
	gen_rtx_SCRATCH (DFmode))));
}

/* /home/yu/gcc/gcc_powerpc/crosstool-0.42/build/powerpc-7450-linux-gnu/gcc-3.3.1-glibc-2.3.2/gcc-3.3.1/gcc/config/rs6000/rs6000.md:8983 */
rtx
gen_fix_trunctfsi2 (operand0, operand1)
     rtx operand0;
     rtx operand1;
{
  return gen_rtx_PARALLEL (VOIDmode, gen_rtvec (2,
		gen_rtx_SET (VOIDmode,
	operand0,
	gen_rtx_FIX (SImode,
	operand1)),
		gen_rtx_CLOBBER (VOIDmode,
	gen_rtx_SCRATCH (DFmode))));
}

/* /home/yu/gcc/gcc_powerpc/crosstool-0.42/build/powerpc-7450-linux-gnu/gcc-3.3.1-glibc-2.3.2/gcc-3.3.1/gcc/config/rs6000/rs6000.md:8997 */
rtx
gen_negtf2 (operand0, operand1)
     rtx operand0;
     rtx operand1;
{
  return gen_rtx_SET (VOIDmode,
	operand0,
	gen_rtx_NEG (TFmode,
	operand1));
}

/* /home/yu/gcc/gcc_powerpc/crosstool-0.42/build/powerpc-7450-linux-gnu/gcc-3.3.1-glibc-2.3.2/gcc-3.3.1/gcc/config/rs6000/rs6000.md:9012 */
rtx
gen_abstf2 (operand0, operand1)
     rtx operand0;
     rtx operand1;
{
  return gen_rtx_SET (VOIDmode,
	operand0,
	gen_rtx_ABS (TFmode,
	operand1));
}

/* /home/yu/gcc/gcc_powerpc/crosstool-0.42/build/powerpc-7450-linux-gnu/gcc-3.3.1-glibc-2.3.2/gcc-3.3.1/gcc/config/rs6000/rs6000.md:10026 */
rtx
gen_movdi_update (operand0, operand1, operand2, operand3)
     rtx operand0;
     rtx operand1;
     rtx operand2;
     rtx operand3;
{
  return gen_rtx_PARALLEL (VOIDmode, gen_rtvec (2,
		gen_rtx_SET (VOIDmode,
	gen_rtx_MEM (DImode,
	gen_rtx_PLUS (DImode,
	operand1,
	operand2)),
	operand3),
		gen_rtx_SET (VOIDmode,
	operand0,
	gen_rtx_PLUS (DImode,
	operand1,
	operand2))));
}

/* /home/yu/gcc/gcc_powerpc/crosstool-0.42/build/powerpc-7450-linux-gnu/gcc-3.3.1-glibc-2.3.2/gcc-3.3.1/gcc/config/rs6000/rs6000.md:10050 */
rtx
gen_movsi_update (operand0, operand1, operand2, operand3)
     rtx operand0;
     rtx operand1;
     rtx operand2;
     rtx operand3;
{
  return gen_rtx_PARALLEL (VOIDmode, gen_rtvec (2,
		gen_rtx_SET (VOIDmode,
	gen_rtx_MEM (SImode,
	gen_rtx_PLUS (SImode,
	operand1,
	operand2)),
	operand3),
		gen_rtx_SET (VOIDmode,
	operand0,
	gen_rtx_PLUS (SImode,
	operand1,
	operand2))));
}

/* /home/yu/gcc/gcc_powerpc/crosstool-0.42/build/powerpc-7450-linux-gnu/gcc-3.3.1-glibc-2.3.2/gcc-3.3.1/gcc/config/rs6000/rs6000.md:10376 */
rtx
gen_load_toc_aix_si (operand0)
     rtx operand0;
{
  return gen_rtx_PARALLEL (VOIDmode,
	gen_rtvec (2,
		gen_rtx_SET (VOIDmode,
	operand0,
	gen_rtx_UNSPEC (SImode,
	gen_rtvec (1,
		const0_rtx),
	7)),
		gen_rtx_USE (VOIDmode,
	gen_rtx_REG (SImode,
	2))));
}

/* /home/yu/gcc/gcc_powerpc/crosstool-0.42/build/powerpc-7450-linux-gnu/gcc-3.3.1-glibc-2.3.2/gcc-3.3.1/gcc/config/rs6000/rs6000.md:10391 */
rtx
gen_load_toc_aix_di (operand0)
     rtx operand0;
{
  return gen_rtx_PARALLEL (VOIDmode,
	gen_rtvec (2,
		gen_rtx_SET (VOIDmode,
	operand0,
	gen_rtx_UNSPEC (DImode,
	gen_rtvec (1,
		const0_rtx),
	7)),
		gen_rtx_USE (VOIDmode,
	gen_rtx_REG (DImode,
	2))));
}

/* /home/yu/gcc/gcc_powerpc/crosstool-0.42/build/powerpc-7450-linux-gnu/gcc-3.3.1-glibc-2.3.2/gcc-3.3.1/gcc/config/rs6000/rs6000.md:10413 */
rtx
gen_load_toc_v4_pic_si (operand0)
     rtx operand0;
{
  return gen_rtx_SET (VOIDmode,
	operand0,
	gen_rtx_UNSPEC (SImode,
	gen_rtvec (1,
		const0_rtx),
	7));
}

/* /home/yu/gcc/gcc_powerpc/crosstool-0.42/build/powerpc-7450-linux-gnu/gcc-3.3.1-glibc-2.3.2/gcc-3.3.1/gcc/config/rs6000/rs6000.md:10421 */
rtx
gen_load_toc_v4_PIC_1 (operand0, operand1)
     rtx operand0;
     rtx operand1;
{
  return gen_rtx_PARALLEL (VOIDmode, gen_rtvec (2,
		gen_rtx_SET (VOIDmode,
	operand0,
	operand1),
		gen_rtx_UNSPEC (VOIDmode,
	gen_rtvec (1,
		operand1),
	7)));
}

/* /home/yu/gcc/gcc_powerpc/crosstool-0.42/build/powerpc-7450-linux-gnu/gcc-3.3.1-glibc-2.3.2/gcc-3.3.1/gcc/config/rs6000/rs6000.md:10430 */
rtx
gen_load_toc_v4_PIC_1b (operand0, operand1, operand2)
     rtx operand0;
     rtx operand1;
     rtx operand2;
{
  return gen_rtx_PARALLEL (VOIDmode, gen_rtvec (2,
		gen_rtx_SET (VOIDmode,
	operand0,
	operand1),
		gen_rtx_UNSPEC (VOIDmode,
	gen_rtvec (2,
		operand1,
		operand2),
	6)));
}

/* /home/yu/gcc/gcc_powerpc/crosstool-0.42/build/powerpc-7450-linux-gnu/gcc-3.3.1-glibc-2.3.2/gcc-3.3.1/gcc/config/rs6000/rs6000.md:10439 */
rtx
gen_load_toc_v4_PIC_2 (operand0, operand1, operand2, operand3)
     rtx operand0;
     rtx operand1;
     rtx operand2;
     rtx operand3;
{
  return gen_rtx_SET (VOIDmode,
	operand0,
	gen_rtx_MEM (SImode,
	gen_rtx_PLUS (SImode,
	operand1,
	gen_rtx_MINUS (SImode,
	operand2,
	operand3))));
}

/* /home/yu/gcc/gcc_powerpc/crosstool-0.42/build/powerpc-7450-linux-gnu/gcc-3.3.1-glibc-2.3.2/gcc-3.3.1/gcc/config/rs6000/rs6000.md:10448 */
rtx
gen_load_macho_picbase (operand0, operand1)
     rtx operand0;
     rtx operand1;
{
  return gen_rtx_SET (VOIDmode,
	operand0,
	gen_rtx_UNSPEC (SImode,
	gen_rtvec (1,
		operand1),
	15));
}

/* /home/yu/gcc/gcc_powerpc/crosstool-0.42/build/powerpc-7450-linux-gnu/gcc-3.3.1-glibc-2.3.2/gcc-3.3.1/gcc/config/rs6000/rs6000.md:10456 */
rtx
gen_macho_correct_pic (operand0, operand1, operand2, operand3)
     rtx operand0;
     rtx operand1;
     rtx operand2;
     rtx operand3;
{
  return gen_rtx_SET (VOIDmode,
	operand0,
	gen_rtx_PLUS (SImode,
	operand1,
	gen_rtx_UNSPEC (SImode,
	gen_rtvec (2,
		operand2,
		operand3),
	16)));
}

/* /home/yu/gcc/gcc_powerpc/crosstool-0.42/build/powerpc-7450-linux-gnu/gcc-3.3.1-glibc-2.3.2/gcc-3.3.1/gcc/config/rs6000/rs6000.md:11252 */
rtx
gen_blockage ()
{
  return gen_rtx_UNSPEC_VOLATILE (VOIDmode,
	gen_rtvec (1,
		const0_rtx),
	0);
}

/* /home/yu/gcc/gcc_powerpc/crosstool-0.42/build/powerpc-7450-linux-gnu/gcc-3.3.1-glibc-2.3.2/gcc-3.3.1/gcc/config/rs6000/rs6000.md:14117 */
rtx
gen_jump (operand0)
     rtx operand0;
{
  return gen_rtx_SET (VOIDmode,
	pc_rtx,
	gen_rtx_LABEL_REF (VOIDmode,
	operand0));
}

/* /home/yu/gcc/gcc_powerpc/crosstool-0.42/build/powerpc-7450-linux-gnu/gcc-3.3.1-glibc-2.3.2/gcc-3.3.1/gcc/config/rs6000/rs6000.md:14124 */
rtx
gen_return ()
{
  return gen_rtx_RETURN (VOIDmode);
}

/* /home/yu/gcc/gcc_powerpc/crosstool-0.42/build/powerpc-7450-linux-gnu/gcc-3.3.1-glibc-2.3.2/gcc-3.3.1/gcc/config/rs6000/rs6000.md:14142 */
rtx
gen_indirect_jumpsi (operand0)
     rtx operand0;
{
  return gen_rtx_SET (VOIDmode,
	pc_rtx,
	operand0);
}

/* /home/yu/gcc/gcc_powerpc/crosstool-0.42/build/powerpc-7450-linux-gnu/gcc-3.3.1-glibc-2.3.2/gcc-3.3.1/gcc/config/rs6000/rs6000.md:14150 */
rtx
gen_indirect_jumpdi (operand0)
     rtx operand0;
{
  return gen_rtx_SET (VOIDmode,
	pc_rtx,
	operand0);
}

/* /home/yu/gcc/gcc_powerpc/crosstool-0.42/build/powerpc-7450-linux-gnu/gcc-3.3.1-glibc-2.3.2/gcc-3.3.1/gcc/config/rs6000/rs6000.md:14220 */
rtx
gen_nop ()
{
  return const0_rtx;
}

/* /home/yu/gcc/gcc_powerpc/crosstool-0.42/build/powerpc-7450-linux-gnu/gcc-3.3.1-glibc-2.3.2/gcc-3.3.1/gcc/config/rs6000/rs6000.md:14695 */
rtx
gen_trap ()
{
  return gen_rtx_TRAP_IF (VOIDmode,
	const1_rtx,
	const0_rtx);
}

/* /home/yu/gcc/gcc_powerpc/crosstool-0.42/build/powerpc-7450-linux-gnu/gcc-3.3.1-glibc-2.3.2/gcc-3.3.1/gcc/config/rs6000/rs6000.md:14736 */
rtx
gen_movesi_from_cr (operand0)
     rtx operand0;
{
  return gen_rtx_SET (VOIDmode,
	operand0,
	gen_rtx_UNSPEC (SImode,
	gen_rtvec (8,
		gen_rtx_REG (CCmode,
	68),
		gen_rtx_REG (CCmode,
	69),
		gen_rtx_REG (CCmode,
	70),
		gen_rtx_REG (CCmode,
	71),
		gen_rtx_REG (CCmode,
	72),
		gen_rtx_REG (CCmode,
	73),
		gen_rtx_REG (CCmode,
	74),
		gen_rtx_REG (CCmode,
	75)),
	19));
}

/* /home/yu/gcc/gcc_powerpc/crosstool-0.42/build/powerpc-7450-linux-gnu/gcc-3.3.1-glibc-2.3.2/gcc-3.3.1/gcc/config/rs6000/rs6000.md:14771 */
rtx
gen_stack_tie (operand0)
     rtx operand0;
{
  return gen_rtx_SET (VOIDmode,
	operand0,
	gen_rtx_UNSPEC (BLKmode,
	gen_rtvec (1,
		operand0),
	5));
}

/* /home/yu/gcc/gcc_powerpc/crosstool-0.42/build/powerpc-7450-linux-gnu/gcc-3.3.1-glibc-2.3.2/gcc-3.3.1/gcc/config/rs6000/rs6000.md:14893 */
rtx
gen_eh_set_lr_si (operand0)
     rtx operand0;
{
  return gen_rtx_PARALLEL (VOIDmode, gen_rtvec (2,
		gen_rtx_UNSPEC_VOLATILE (VOIDmode,
	gen_rtvec (1,
		operand0),
	9),
		gen_rtx_CLOBBER (VOIDmode,
	gen_rtx_SCRATCH (SImode))));
}

/* /home/yu/gcc/gcc_powerpc/crosstool-0.42/build/powerpc-7450-linux-gnu/gcc-3.3.1-glibc-2.3.2/gcc-3.3.1/gcc/config/rs6000/rs6000.md:14899 */
rtx
gen_eh_set_lr_di (operand0)
     rtx operand0;
{
  return gen_rtx_PARALLEL (VOIDmode, gen_rtvec (2,
		gen_rtx_UNSPEC_VOLATILE (VOIDmode,
	gen_rtvec (1,
		operand0),
	9),
		gen_rtx_CLOBBER (VOIDmode,
	gen_rtx_SCRATCH (DImode))));
}

/* /home/yu/gcc/gcc_powerpc/crosstool-0.42/build/powerpc-7450-linux-gnu/gcc-3.3.1-glibc-2.3.2/gcc-3.3.1/gcc/config/rs6000/rs6000.md:14939 */
rtx
gen_prefetch (operand0, operand1, operand2)
     rtx operand0;
     rtx operand1;
     rtx operand2;
{
  return gen_rtx_PREFETCH (VOIDmode,
	operand0,
	operand1,
	operand2);
}

/* /home/yu/gcc/gcc_powerpc/crosstool-0.42/build/powerpc-7450-linux-gnu/gcc-3.3.1-glibc-2.3.2/gcc-3.3.1/gcc/config/rs6000/altivec.md:23 */
rtx
gen_altivec_lvx_4si (operand0, operand1)
     rtx operand0;
     rtx operand1;
{
  return gen_rtx_SET (VOIDmode,
	operand0,
	operand1);
}

/* /home/yu/gcc/gcc_powerpc/crosstool-0.42/build/powerpc-7450-linux-gnu/gcc-3.3.1-glibc-2.3.2/gcc-3.3.1/gcc/config/rs6000/altivec.md:30 */
rtx
gen_altivec_lvx_8hi (operand0, operand1)
     rtx operand0;
     rtx operand1;
{
  return gen_rtx_SET (VOIDmode,
	operand0,
	operand1);
}

/* /home/yu/gcc/gcc_powerpc/crosstool-0.42/build/powerpc-7450-linux-gnu/gcc-3.3.1-glibc-2.3.2/gcc-3.3.1/gcc/config/rs6000/altivec.md:37 */
rtx
gen_altivec_lvx_16qi (operand0, operand1)
     rtx operand0;
     rtx operand1;
{
  return gen_rtx_SET (VOIDmode,
	operand0,
	operand1);
}

/* /home/yu/gcc/gcc_powerpc/crosstool-0.42/build/powerpc-7450-linux-gnu/gcc-3.3.1-glibc-2.3.2/gcc-3.3.1/gcc/config/rs6000/altivec.md:44 */
rtx
gen_altivec_lvx_4sf (operand0, operand1)
     rtx operand0;
     rtx operand1;
{
  return gen_rtx_SET (VOIDmode,
	operand0,
	operand1);
}

/* /home/yu/gcc/gcc_powerpc/crosstool-0.42/build/powerpc-7450-linux-gnu/gcc-3.3.1-glibc-2.3.2/gcc-3.3.1/gcc/config/rs6000/altivec.md:52 */
rtx
gen_altivec_stvx_4si (operand0, operand1)
     rtx operand0;
     rtx operand1;
{
  return gen_rtx_SET (VOIDmode,
	operand0,
	operand1);
}

/* /home/yu/gcc/gcc_powerpc/crosstool-0.42/build/powerpc-7450-linux-gnu/gcc-3.3.1-glibc-2.3.2/gcc-3.3.1/gcc/config/rs6000/altivec.md:59 */
rtx
gen_altivec_stvx_8hi (operand0, operand1)
     rtx operand0;
     rtx operand1;
{
  return gen_rtx_SET (VOIDmode,
	operand0,
	operand1);
}

/* /home/yu/gcc/gcc_powerpc/crosstool-0.42/build/powerpc-7450-linux-gnu/gcc-3.3.1-glibc-2.3.2/gcc-3.3.1/gcc/config/rs6000/altivec.md:66 */
rtx
gen_altivec_stvx_16qi (operand0, operand1)
     rtx operand0;
     rtx operand1;
{
  return gen_rtx_SET (VOIDmode,
	operand0,
	operand1);
}

/* /home/yu/gcc/gcc_powerpc/crosstool-0.42/build/powerpc-7450-linux-gnu/gcc-3.3.1-glibc-2.3.2/gcc-3.3.1/gcc/config/rs6000/altivec.md:73 */
rtx
gen_altivec_stvx_4sf (operand0, operand1)
     rtx operand0;
     rtx operand1;
{
  return gen_rtx_SET (VOIDmode,
	operand0,
	operand1);
}

/* /home/yu/gcc/gcc_powerpc/crosstool-0.42/build/powerpc-7450-linux-gnu/gcc-3.3.1-glibc-2.3.2/gcc-3.3.1/gcc/config/rs6000/altivec.md:161 */
rtx
gen_get_vrsave_internal (operand0)
     rtx operand0;
{
  return gen_rtx_SET (VOIDmode,
	operand0,
	gen_rtx_UNSPEC (SImode,
	gen_rtvec (1,
		gen_rtx_REG (SImode,
	109)),
	214));
}

/* /home/yu/gcc/gcc_powerpc/crosstool-0.42/build/powerpc-7450-linux-gnu/gcc-3.3.1-glibc-2.3.2/gcc-3.3.1/gcc/config/rs6000/altivec.md:221 */
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

/* /home/yu/gcc/gcc_powerpc/crosstool-0.42/build/powerpc-7450-linux-gnu/gcc-3.3.1-glibc-2.3.2/gcc-3.3.1/gcc/config/rs6000/altivec.md:229 */
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

/* /home/yu/gcc/gcc_powerpc/crosstool-0.42/build/powerpc-7450-linux-gnu/gcc-3.3.1-glibc-2.3.2/gcc-3.3.1/gcc/config/rs6000/altivec.md:237 */
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

/* /home/yu/gcc/gcc_powerpc/crosstool-0.42/build/powerpc-7450-linux-gnu/gcc-3.3.1-glibc-2.3.2/gcc-3.3.1/gcc/config/rs6000/altivec.md:245 */
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

/* /home/yu/gcc/gcc_powerpc/crosstool-0.42/build/powerpc-7450-linux-gnu/gcc-3.3.1-glibc-2.3.2/gcc-3.3.1/gcc/config/rs6000/altivec.md:253 */
rtx
gen_altivec_vaddcuw (operand0, operand1, operand2)
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
	35));
}

/* /home/yu/gcc/gcc_powerpc/crosstool-0.42/build/powerpc-7450-linux-gnu/gcc-3.3.1-glibc-2.3.2/gcc-3.3.1/gcc/config/rs6000/altivec.md:261 */
rtx
gen_altivec_vaddubs (operand0, operand1, operand2)
     rtx operand0;
     rtx operand1;
     rtx operand2;
{
  return gen_rtx_PARALLEL (VOIDmode, gen_rtvec (2,
		gen_rtx_SET (VOIDmode,
	operand0,
	gen_rtx_UNSPEC (V16QImode,
	gen_rtvec (2,
		operand1,
		operand2),
	36)),
		gen_rtx_SET (VOIDmode,
	gen_rtx_REG (SImode,
	110),
	gen_rtx_UNSPEC (SImode,
	gen_rtvec (1,
		const0_rtx),
	213))));
}

/* /home/yu/gcc/gcc_powerpc/crosstool-0.42/build/powerpc-7450-linux-gnu/gcc-3.3.1-glibc-2.3.2/gcc-3.3.1/gcc/config/rs6000/altivec.md:270 */
rtx
gen_altivec_vaddsbs (operand0, operand1, operand2)
     rtx operand0;
     rtx operand1;
     rtx operand2;
{
  return gen_rtx_PARALLEL (VOIDmode, gen_rtvec (2,
		gen_rtx_SET (VOIDmode,
	operand0,
	gen_rtx_UNSPEC (V16QImode,
	gen_rtvec (2,
		operand1,
		operand2),
	37)),
		gen_rtx_SET (VOIDmode,
	gen_rtx_REG (SImode,
	110),
	gen_rtx_UNSPEC (SImode,
	gen_rtvec (1,
		const0_rtx),
	213))));
}

/* /home/yu/gcc/gcc_powerpc/crosstool-0.42/build/powerpc-7450-linux-gnu/gcc-3.3.1-glibc-2.3.2/gcc-3.3.1/gcc/config/rs6000/altivec.md:279 */
rtx
gen_altivec_vadduhs (operand0, operand1, operand2)
     rtx operand0;
     rtx operand1;
     rtx operand2;
{
  return gen_rtx_PARALLEL (VOIDmode, gen_rtvec (2,
		gen_rtx_SET (VOIDmode,
	operand0,
	gen_rtx_UNSPEC (V8HImode,
	gen_rtvec (2,
		operand1,
		operand2),
	38)),
		gen_rtx_SET (VOIDmode,
	gen_rtx_REG (SImode,
	110),
	gen_rtx_UNSPEC (SImode,
	gen_rtvec (1,
		const0_rtx),
	213))));
}

/* /home/yu/gcc/gcc_powerpc/crosstool-0.42/build/powerpc-7450-linux-gnu/gcc-3.3.1-glibc-2.3.2/gcc-3.3.1/gcc/config/rs6000/altivec.md:288 */
rtx
gen_altivec_vaddshs (operand0, operand1, operand2)
     rtx operand0;
     rtx operand1;
     rtx operand2;
{
  return gen_rtx_PARALLEL (VOIDmode, gen_rtvec (2,
		gen_rtx_SET (VOIDmode,
	operand0,
	gen_rtx_UNSPEC (V8HImode,
	gen_rtvec (2,
		operand1,
		operand2),
	39)),
		gen_rtx_SET (VOIDmode,
	gen_rtx_REG (SImode,
	110),
	gen_rtx_UNSPEC (SImode,
	gen_rtvec (1,
		const0_rtx),
	213))));
}

/* /home/yu/gcc/gcc_powerpc/crosstool-0.42/build/powerpc-7450-linux-gnu/gcc-3.3.1-glibc-2.3.2/gcc-3.3.1/gcc/config/rs6000/altivec.md:297 */
rtx
gen_altivec_vadduws (operand0, operand1, operand2)
     rtx operand0;
     rtx operand1;
     rtx operand2;
{
  return gen_rtx_PARALLEL (VOIDmode, gen_rtvec (2,
		gen_rtx_SET (VOIDmode,
	operand0,
	gen_rtx_UNSPEC (V4SImode,
	gen_rtvec (2,
		operand1,
		operand2),
	40)),
		gen_rtx_SET (VOIDmode,
	gen_rtx_REG (SImode,
	110),
	gen_rtx_UNSPEC (SImode,
	gen_rtvec (1,
		const0_rtx),
	213))));
}

/* /home/yu/gcc/gcc_powerpc/crosstool-0.42/build/powerpc-7450-linux-gnu/gcc-3.3.1-glibc-2.3.2/gcc-3.3.1/gcc/config/rs6000/altivec.md:306 */
rtx
gen_altivec_vaddsws (operand0, operand1, operand2)
     rtx operand0;
     rtx operand1;
     rtx operand2;
{
  return gen_rtx_PARALLEL (VOIDmode, gen_rtvec (2,
		gen_rtx_SET (VOIDmode,
	operand0,
	gen_rtx_UNSPEC (V4SImode,
	gen_rtvec (2,
		operand1,
		operand2),
	41)),
		gen_rtx_SET (VOIDmode,
	gen_rtx_REG (SImode,
	110),
	gen_rtx_UNSPEC (SImode,
	gen_rtvec (1,
		const0_rtx),
	213))));
}

/* /home/yu/gcc/gcc_powerpc/crosstool-0.42/build/powerpc-7450-linux-gnu/gcc-3.3.1-glibc-2.3.2/gcc-3.3.1/gcc/config/rs6000/altivec.md:315 */
rtx
gen_andv4si3 (operand0, operand1, operand2)
     rtx operand0;
     rtx operand1;
     rtx operand2;
{
  return gen_rtx_SET (VOIDmode,
	operand0,
	gen_rtx_AND (V4SImode,
	operand1,
	operand2));
}

/* /home/yu/gcc/gcc_powerpc/crosstool-0.42/build/powerpc-7450-linux-gnu/gcc-3.3.1-glibc-2.3.2/gcc-3.3.1/gcc/config/rs6000/altivec.md:323 */
rtx
gen_altivec_vandc (operand0, operand1, operand2)
     rtx operand0;
     rtx operand1;
     rtx operand2;
{
  return gen_rtx_SET (VOIDmode,
	operand0,
	gen_rtx_AND (V4SImode,
	operand1,
	gen_rtx_NOT (V4SImode,
	operand2)));
}

/* /home/yu/gcc/gcc_powerpc/crosstool-0.42/build/powerpc-7450-linux-gnu/gcc-3.3.1-glibc-2.3.2/gcc-3.3.1/gcc/config/rs6000/altivec.md:331 */
rtx
gen_altivec_vavgub (operand0, operand1, operand2)
     rtx operand0;
     rtx operand1;
     rtx operand2;
{
  return gen_rtx_SET (VOIDmode,
	operand0,
	gen_rtx_UNSPEC (V16QImode,
	gen_rtvec (2,
		operand1,
		operand2),
	44));
}

/* /home/yu/gcc/gcc_powerpc/crosstool-0.42/build/powerpc-7450-linux-gnu/gcc-3.3.1-glibc-2.3.2/gcc-3.3.1/gcc/config/rs6000/altivec.md:339 */
rtx
gen_altivec_vavgsb (operand0, operand1, operand2)
     rtx operand0;
     rtx operand1;
     rtx operand2;
{
  return gen_rtx_SET (VOIDmode,
	operand0,
	gen_rtx_UNSPEC (V16QImode,
	gen_rtvec (2,
		operand1,
		operand2),
	45));
}

/* /home/yu/gcc/gcc_powerpc/crosstool-0.42/build/powerpc-7450-linux-gnu/gcc-3.3.1-glibc-2.3.2/gcc-3.3.1/gcc/config/rs6000/altivec.md:347 */
rtx
gen_altivec_vavguh (operand0, operand1, operand2)
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
	46));
}

/* /home/yu/gcc/gcc_powerpc/crosstool-0.42/build/powerpc-7450-linux-gnu/gcc-3.3.1-glibc-2.3.2/gcc-3.3.1/gcc/config/rs6000/altivec.md:355 */
rtx
gen_altivec_vavgsh (operand0, operand1, operand2)
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
	47));
}

/* /home/yu/gcc/gcc_powerpc/crosstool-0.42/build/powerpc-7450-linux-gnu/gcc-3.3.1-glibc-2.3.2/gcc-3.3.1/gcc/config/rs6000/altivec.md:363 */
rtx
gen_altivec_vavguw (operand0, operand1, operand2)
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
	48));
}

/* /home/yu/gcc/gcc_powerpc/crosstool-0.42/build/powerpc-7450-linux-gnu/gcc-3.3.1-glibc-2.3.2/gcc-3.3.1/gcc/config/rs6000/altivec.md:371 */
rtx
gen_altivec_vavgsw (operand0, operand1, operand2)
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
	49));
}

/* /home/yu/gcc/gcc_powerpc/crosstool-0.42/build/powerpc-7450-linux-gnu/gcc-3.3.1-glibc-2.3.2/gcc-3.3.1/gcc/config/rs6000/altivec.md:379 */
rtx
gen_altivec_vcmpbfp (operand0, operand1, operand2)
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
	50));
}

/* /home/yu/gcc/gcc_powerpc/crosstool-0.42/build/powerpc-7450-linux-gnu/gcc-3.3.1-glibc-2.3.2/gcc-3.3.1/gcc/config/rs6000/altivec.md:387 */
rtx
gen_altivec_vcmpequb (operand0, operand1, operand2)
     rtx operand0;
     rtx operand1;
     rtx operand2;
{
  return gen_rtx_SET (VOIDmode,
	operand0,
	gen_rtx_UNSPEC (V16QImode,
	gen_rtvec (2,
		operand1,
		operand2),
	51));
}

/* /home/yu/gcc/gcc_powerpc/crosstool-0.42/build/powerpc-7450-linux-gnu/gcc-3.3.1-glibc-2.3.2/gcc-3.3.1/gcc/config/rs6000/altivec.md:395 */
rtx
gen_altivec_vcmpequh (operand0, operand1, operand2)
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
	52));
}

/* /home/yu/gcc/gcc_powerpc/crosstool-0.42/build/powerpc-7450-linux-gnu/gcc-3.3.1-glibc-2.3.2/gcc-3.3.1/gcc/config/rs6000/altivec.md:403 */
rtx
gen_altivec_vcmpequw (operand0, operand1, operand2)
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
	53));
}

/* /home/yu/gcc/gcc_powerpc/crosstool-0.42/build/powerpc-7450-linux-gnu/gcc-3.3.1-glibc-2.3.2/gcc-3.3.1/gcc/config/rs6000/altivec.md:411 */
rtx
gen_altivec_vcmpeqfp (operand0, operand1, operand2)
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
	54));
}

/* /home/yu/gcc/gcc_powerpc/crosstool-0.42/build/powerpc-7450-linux-gnu/gcc-3.3.1-glibc-2.3.2/gcc-3.3.1/gcc/config/rs6000/altivec.md:419 */
rtx
gen_altivec_vcmpgefp (operand0, operand1, operand2)
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
	55));
}

/* /home/yu/gcc/gcc_powerpc/crosstool-0.42/build/powerpc-7450-linux-gnu/gcc-3.3.1-glibc-2.3.2/gcc-3.3.1/gcc/config/rs6000/altivec.md:427 */
rtx
gen_altivec_vcmpgtub (operand0, operand1, operand2)
     rtx operand0;
     rtx operand1;
     rtx operand2;
{
  return gen_rtx_SET (VOIDmode,
	operand0,
	gen_rtx_UNSPEC (V16QImode,
	gen_rtvec (2,
		operand1,
		operand2),
	56));
}

/* /home/yu/gcc/gcc_powerpc/crosstool-0.42/build/powerpc-7450-linux-gnu/gcc-3.3.1-glibc-2.3.2/gcc-3.3.1/gcc/config/rs6000/altivec.md:435 */
rtx
gen_altivec_vcmpgtsb (operand0, operand1, operand2)
     rtx operand0;
     rtx operand1;
     rtx operand2;
{
  return gen_rtx_SET (VOIDmode,
	operand0,
	gen_rtx_UNSPEC (V16QImode,
	gen_rtvec (2,
		operand1,
		operand2),
	57));
}

/* /home/yu/gcc/gcc_powerpc/crosstool-0.42/build/powerpc-7450-linux-gnu/gcc-3.3.1-glibc-2.3.2/gcc-3.3.1/gcc/config/rs6000/altivec.md:443 */
rtx
gen_altivec_vcmpgtuh (operand0, operand1, operand2)
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
	58));
}

/* /home/yu/gcc/gcc_powerpc/crosstool-0.42/build/powerpc-7450-linux-gnu/gcc-3.3.1-glibc-2.3.2/gcc-3.3.1/gcc/config/rs6000/altivec.md:451 */
rtx
gen_altivec_vcmpgtsh (operand0, operand1, operand2)
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
	59));
}

/* /home/yu/gcc/gcc_powerpc/crosstool-0.42/build/powerpc-7450-linux-gnu/gcc-3.3.1-glibc-2.3.2/gcc-3.3.1/gcc/config/rs6000/altivec.md:459 */
rtx
gen_altivec_vcmpgtuw (operand0, operand1, operand2)
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
	60));
}

/* /home/yu/gcc/gcc_powerpc/crosstool-0.42/build/powerpc-7450-linux-gnu/gcc-3.3.1-glibc-2.3.2/gcc-3.3.1/gcc/config/rs6000/altivec.md:467 */
rtx
gen_altivec_vcmpgtsw (operand0, operand1, operand2)
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
	61));
}

/* /home/yu/gcc/gcc_powerpc/crosstool-0.42/build/powerpc-7450-linux-gnu/gcc-3.3.1-glibc-2.3.2/gcc-3.3.1/gcc/config/rs6000/altivec.md:475 */
rtx
gen_altivec_vcmpgtfp (operand0, operand1, operand2)
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
	62));
}

/* /home/yu/gcc/gcc_powerpc/crosstool-0.42/build/powerpc-7450-linux-gnu/gcc-3.3.1-glibc-2.3.2/gcc-3.3.1/gcc/config/rs6000/altivec.md:484 */
rtx
gen_altivec_vmaddfp (operand0, operand1, operand2, operand3)
     rtx operand0;
     rtx operand1;
     rtx operand2;
     rtx operand3;
{
  return gen_rtx_SET (VOIDmode,
	operand0,
	gen_rtx_PLUS (V4SFmode,
	gen_rtx_MULT (V4SFmode,
	operand1,
	operand2),
	operand3));
}

/* /home/yu/gcc/gcc_powerpc/crosstool-0.42/build/powerpc-7450-linux-gnu/gcc-3.3.1-glibc-2.3.2/gcc-3.3.1/gcc/config/rs6000/altivec.md:516 */
rtx
gen_altivec_vnmsubfp (operand0, operand1, operand2, operand3)
     rtx operand0;
     rtx operand1;
     rtx operand2;
     rtx operand3;
{
  return gen_rtx_SET (VOIDmode,
	operand0,
	gen_rtx_MINUS (V4SFmode,
	gen_rtx_MULT (V4SFmode,
	operand1,
	operand2),
	operand3));
}

/* /home/yu/gcc/gcc_powerpc/crosstool-0.42/build/powerpc-7450-linux-gnu/gcc-3.3.1-glibc-2.3.2/gcc-3.3.1/gcc/config/rs6000/altivec.md:526 */
rtx
gen_altivec_vmsumubm (operand0, operand1, operand2, operand3)
     rtx operand0;
     rtx operand1;
     rtx operand2;
     rtx operand3;
{
  return gen_rtx_SET (VOIDmode,
	operand0,
	gen_rtx_UNSPEC (V4SImode,
	gen_rtvec (3,
		operand1,
		operand2,
		operand3),
	65));
}

/* /home/yu/gcc/gcc_powerpc/crosstool-0.42/build/powerpc-7450-linux-gnu/gcc-3.3.1-glibc-2.3.2/gcc-3.3.1/gcc/config/rs6000/altivec.md:535 */
rtx
gen_altivec_vmsummbm (operand0, operand1, operand2, operand3)
     rtx operand0;
     rtx operand1;
     rtx operand2;
     rtx operand3;
{
  return gen_rtx_SET (VOIDmode,
	operand0,
	gen_rtx_UNSPEC (V4SImode,
	gen_rtvec (3,
		operand1,
		operand2,
		operand3),
	66));
}

/* /home/yu/gcc/gcc_powerpc/crosstool-0.42/build/powerpc-7450-linux-gnu/gcc-3.3.1-glibc-2.3.2/gcc-3.3.1/gcc/config/rs6000/altivec.md:544 */
rtx
gen_altivec_vmsumuhm (operand0, operand1, operand2, operand3)
     rtx operand0;
     rtx operand1;
     rtx operand2;
     rtx operand3;
{
  return gen_rtx_SET (VOIDmode,
	operand0,
	gen_rtx_UNSPEC (V4SImode,
	gen_rtvec (3,
		operand1,
		operand2,
		operand3),
	67));
}

/* /home/yu/gcc/gcc_powerpc/crosstool-0.42/build/powerpc-7450-linux-gnu/gcc-3.3.1-glibc-2.3.2/gcc-3.3.1/gcc/config/rs6000/altivec.md:553 */
rtx
gen_altivec_vmsumshm (operand0, operand1, operand2, operand3)
     rtx operand0;
     rtx operand1;
     rtx operand2;
     rtx operand3;
{
  return gen_rtx_SET (VOIDmode,
	operand0,
	gen_rtx_UNSPEC (V4SImode,
	gen_rtvec (3,
		operand1,
		operand2,
		operand3),
	68));
}

/* /home/yu/gcc/gcc_powerpc/crosstool-0.42/build/powerpc-7450-linux-gnu/gcc-3.3.1-glibc-2.3.2/gcc-3.3.1/gcc/config/rs6000/altivec.md:562 */
rtx
gen_altivec_vmsumuhs (operand0, operand1, operand2, operand3)
     rtx operand0;
     rtx operand1;
     rtx operand2;
     rtx operand3;
{
  return gen_rtx_PARALLEL (VOIDmode, gen_rtvec (2,
		gen_rtx_SET (VOIDmode,
	operand0,
	gen_rtx_UNSPEC (V4SImode,
	gen_rtvec (3,
		operand1,
		operand2,
		operand3),
	69)),
		gen_rtx_SET (VOIDmode,
	gen_rtx_REG (SImode,
	110),
	gen_rtx_UNSPEC (SImode,
	gen_rtvec (1,
		const0_rtx),
	213))));
}

/* /home/yu/gcc/gcc_powerpc/crosstool-0.42/build/powerpc-7450-linux-gnu/gcc-3.3.1-glibc-2.3.2/gcc-3.3.1/gcc/config/rs6000/altivec.md:572 */
rtx
gen_altivec_vmsumshs (operand0, operand1, operand2, operand3)
     rtx operand0;
     rtx operand1;
     rtx operand2;
     rtx operand3;
{
  return gen_rtx_PARALLEL (VOIDmode, gen_rtvec (2,
		gen_rtx_SET (VOIDmode,
	operand0,
	gen_rtx_UNSPEC (V4SImode,
	gen_rtvec (3,
		operand1,
		operand2,
		operand3),
	70)),
		gen_rtx_SET (VOIDmode,
	gen_rtx_REG (SImode,
	110),
	gen_rtx_UNSPEC (SImode,
	gen_rtvec (1,
		const0_rtx),
	213))));
}

/* /home/yu/gcc/gcc_powerpc/crosstool-0.42/build/powerpc-7450-linux-gnu/gcc-3.3.1-glibc-2.3.2/gcc-3.3.1/gcc/config/rs6000/altivec.md:582 */
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

/* /home/yu/gcc/gcc_powerpc/crosstool-0.42/build/powerpc-7450-linux-gnu/gcc-3.3.1-glibc-2.3.2/gcc-3.3.1/gcc/config/rs6000/altivec.md:590 */
rtx
gen_smaxv16qi3 (operand0, operand1, operand2)
     rtx operand0;
     rtx operand1;
     rtx operand2;
{
  return gen_rtx_SET (VOIDmode,
	operand0,
	gen_rtx_SMAX (V16QImode,
	operand1,
	operand2));
}

/* /home/yu/gcc/gcc_powerpc/crosstool-0.42/build/powerpc-7450-linux-gnu/gcc-3.3.1-glibc-2.3.2/gcc-3.3.1/gcc/config/rs6000/altivec.md:598 */
rtx
gen_umaxv8hi3 (operand0, operand1, operand2)
     rtx operand0;
     rtx operand1;
     rtx operand2;
{
  return gen_rtx_SET (VOIDmode,
	operand0,
	gen_rtx_UMAX (V8HImode,
	operand1,
	operand2));
}

/* /home/yu/gcc/gcc_powerpc/crosstool-0.42/build/powerpc-7450-linux-gnu/gcc-3.3.1-glibc-2.3.2/gcc-3.3.1/gcc/config/rs6000/altivec.md:606 */
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

/* /home/yu/gcc/gcc_powerpc/crosstool-0.42/build/powerpc-7450-linux-gnu/gcc-3.3.1-glibc-2.3.2/gcc-3.3.1/gcc/config/rs6000/altivec.md:614 */
rtx
gen_umaxv4si3 (operand0, operand1, operand2)
     rtx operand0;
     rtx operand1;
     rtx operand2;
{
  return gen_rtx_SET (VOIDmode,
	operand0,
	gen_rtx_UMAX (V4SImode,
	operand1,
	operand2));
}

/* /home/yu/gcc/gcc_powerpc/crosstool-0.42/build/powerpc-7450-linux-gnu/gcc-3.3.1-glibc-2.3.2/gcc-3.3.1/gcc/config/rs6000/altivec.md:622 */
rtx
gen_smaxv4si3 (operand0, operand1, operand2)
     rtx operand0;
     rtx operand1;
     rtx operand2;
{
  return gen_rtx_SET (VOIDmode,
	operand0,
	gen_rtx_SMAX (V4SImode,
	operand1,
	operand2));
}

/* /home/yu/gcc/gcc_powerpc/crosstool-0.42/build/powerpc-7450-linux-gnu/gcc-3.3.1-glibc-2.3.2/gcc-3.3.1/gcc/config/rs6000/altivec.md:630 */
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

/* /home/yu/gcc/gcc_powerpc/crosstool-0.42/build/powerpc-7450-linux-gnu/gcc-3.3.1-glibc-2.3.2/gcc-3.3.1/gcc/config/rs6000/altivec.md:638 */
rtx
gen_altivec_vmhaddshs (operand0, operand1, operand2, operand3)
     rtx operand0;
     rtx operand1;
     rtx operand2;
     rtx operand3;
{
  return gen_rtx_PARALLEL (VOIDmode, gen_rtvec (2,
		gen_rtx_SET (VOIDmode,
	operand0,
	gen_rtx_UNSPEC (V8HImode,
	gen_rtvec (3,
		operand1,
		operand2,
		operand3),
	71)),
		gen_rtx_SET (VOIDmode,
	gen_rtx_REG (SImode,
	110),
	gen_rtx_UNSPEC (SImode,
	gen_rtvec (1,
		const0_rtx),
	213))));
}

/* /home/yu/gcc/gcc_powerpc/crosstool-0.42/build/powerpc-7450-linux-gnu/gcc-3.3.1-glibc-2.3.2/gcc-3.3.1/gcc/config/rs6000/altivec.md:647 */
rtx
gen_altivec_vmhraddshs (operand0, operand1, operand2, operand3)
     rtx operand0;
     rtx operand1;
     rtx operand2;
     rtx operand3;
{
  return gen_rtx_PARALLEL (VOIDmode, gen_rtvec (2,
		gen_rtx_SET (VOIDmode,
	operand0,
	gen_rtx_UNSPEC (V8HImode,
	gen_rtvec (3,
		operand1,
		operand2,
		operand3),
	72)),
		gen_rtx_SET (VOIDmode,
	gen_rtx_REG (SImode,
	110),
	gen_rtx_UNSPEC (SImode,
	gen_rtvec (1,
		const0_rtx),
	213))));
}

/* /home/yu/gcc/gcc_powerpc/crosstool-0.42/build/powerpc-7450-linux-gnu/gcc-3.3.1-glibc-2.3.2/gcc-3.3.1/gcc/config/rs6000/altivec.md:656 */
rtx
gen_altivec_vmladduhm (operand0, operand1, operand2, operand3)
     rtx operand0;
     rtx operand1;
     rtx operand2;
     rtx operand3;
{
  return gen_rtx_SET (VOIDmode,
	operand0,
	gen_rtx_UNSPEC (V8HImode,
	gen_rtvec (3,
		operand1,
		operand2,
		operand3),
	73));
}

/* /home/yu/gcc/gcc_powerpc/crosstool-0.42/build/powerpc-7450-linux-gnu/gcc-3.3.1-glibc-2.3.2/gcc-3.3.1/gcc/config/rs6000/altivec.md:665 */
rtx
gen_altivec_vmrghb (operand0, operand1, operand2)
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
		GEN_INT (9LL),
		GEN_INT (10LL),
		GEN_INT (11LL),
		GEN_INT (12LL),
		GEN_INT (13LL),
		GEN_INT (14LL),
		GEN_INT (15LL),
		const0_rtx,
		const1_rtx,
		GEN_INT (2LL),
		GEN_INT (3LL),
		GEN_INT (4LL),
		GEN_INT (5LL),
		GEN_INT (6LL),
		GEN_INT (7LL)))),
	operand2,
	GEN_INT (255LL)));
}

/* /home/yu/gcc/gcc_powerpc/crosstool-0.42/build/powerpc-7450-linux-gnu/gcc-3.3.1-glibc-2.3.2/gcc-3.3.1/gcc/config/rs6000/altivec.md:690 */
rtx
gen_altivec_vmrghh (operand0, operand1, operand2)
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
		GEN_INT (5LL),
		GEN_INT (6LL),
		GEN_INT (7LL),
		const0_rtx,
		const1_rtx,
		GEN_INT (2LL),
		GEN_INT (3LL)))),
	operand2,
	GEN_INT (15LL)));
}

/* /home/yu/gcc/gcc_powerpc/crosstool-0.42/build/powerpc-7450-linux-gnu/gcc-3.3.1-glibc-2.3.2/gcc-3.3.1/gcc/config/rs6000/altivec.md:707 */
rtx
gen_altivec_vmrghw (operand0, operand1, operand2)
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
		GEN_INT (3LL),
		const0_rtx,
		const1_rtx))),
	operand2,
	GEN_INT (12LL)));
}

/* /home/yu/gcc/gcc_powerpc/crosstool-0.42/build/powerpc-7450-linux-gnu/gcc-3.3.1-glibc-2.3.2/gcc-3.3.1/gcc/config/rs6000/altivec.md:720 */
rtx
gen_altivec_vmrglb (operand0, operand1, operand2)
     rtx operand0;
     rtx operand1;
     rtx operand2;
{
  return gen_rtx_SET (VOIDmode,
	operand0,
	gen_rtx_VEC_MERGE (V16QImode,
	gen_rtx_VEC_SELECT (V16QImode,
	operand2,
	gen_rtx_PARALLEL (VOIDmode,
	gen_rtvec (16,
		const0_rtx,
		const1_rtx,
		GEN_INT (2LL),
		GEN_INT (3LL),
		GEN_INT (4LL),
		GEN_INT (5LL),
		GEN_INT (6LL),
		GEN_INT (7LL),
		GEN_INT (8LL),
		GEN_INT (9LL),
		GEN_INT (10LL),
		GEN_INT (11LL),
		GEN_INT (12LL),
		GEN_INT (13LL),
		GEN_INT (14LL),
		GEN_INT (15LL)))),
	operand1,
	GEN_INT (255LL)));
}

/* /home/yu/gcc/gcc_powerpc/crosstool-0.42/build/powerpc-7450-linux-gnu/gcc-3.3.1-glibc-2.3.2/gcc-3.3.1/gcc/config/rs6000/altivec.md:745 */
rtx
gen_altivec_vmrglh (operand0, operand1, operand2)
     rtx operand0;
     rtx operand1;
     rtx operand2;
{
  return gen_rtx_SET (VOIDmode,
	operand0,
	gen_rtx_VEC_MERGE (V8HImode,
	gen_rtx_VEC_SELECT (V8HImode,
	operand2,
	gen_rtx_PARALLEL (VOIDmode,
	gen_rtvec (8,
		const0_rtx,
		const1_rtx,
		GEN_INT (2LL),
		GEN_INT (3LL),
		GEN_INT (4LL),
		GEN_INT (5LL),
		GEN_INT (6LL),
		GEN_INT (7LL)))),
	operand1,
	GEN_INT (15LL)));
}

/* /home/yu/gcc/gcc_powerpc/crosstool-0.42/build/powerpc-7450-linux-gnu/gcc-3.3.1-glibc-2.3.2/gcc-3.3.1/gcc/config/rs6000/altivec.md:762 */
rtx
gen_altivec_vmrglw (operand0, operand1, operand2)
     rtx operand0;
     rtx operand1;
     rtx operand2;
{
  return gen_rtx_SET (VOIDmode,
	operand0,
	gen_rtx_VEC_MERGE (V4SImode,
	gen_rtx_VEC_SELECT (V4SImode,
	operand2,
	gen_rtx_PARALLEL (VOIDmode,
	gen_rtvec (4,
		const0_rtx,
		const1_rtx,
		GEN_INT (2LL),
		GEN_INT (3LL)))),
	operand1,
	GEN_INT (12LL)));
}

/* /home/yu/gcc/gcc_powerpc/crosstool-0.42/build/powerpc-7450-linux-gnu/gcc-3.3.1-glibc-2.3.2/gcc-3.3.1/gcc/config/rs6000/altivec.md:775 */
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

/* /home/yu/gcc/gcc_powerpc/crosstool-0.42/build/powerpc-7450-linux-gnu/gcc-3.3.1-glibc-2.3.2/gcc-3.3.1/gcc/config/rs6000/altivec.md:783 */
rtx
gen_sminv16qi3 (operand0, operand1, operand2)
     rtx operand0;
     rtx operand1;
     rtx operand2;
{
  return gen_rtx_SET (VOIDmode,
	operand0,
	gen_rtx_SMIN (V16QImode,
	operand1,
	operand2));
}

/* /home/yu/gcc/gcc_powerpc/crosstool-0.42/build/powerpc-7450-linux-gnu/gcc-3.3.1-glibc-2.3.2/gcc-3.3.1/gcc/config/rs6000/altivec.md:791 */
rtx
gen_uminv8hi3 (operand0, operand1, operand2)
     rtx operand0;
     rtx operand1;
     rtx operand2;
{
  return gen_rtx_SET (VOIDmode,
	operand0,
	gen_rtx_UMIN (V8HImode,
	operand1,
	operand2));
}

/* /home/yu/gcc/gcc_powerpc/crosstool-0.42/build/powerpc-7450-linux-gnu/gcc-3.3.1-glibc-2.3.2/gcc-3.3.1/gcc/config/rs6000/altivec.md:799 */
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

/* /home/yu/gcc/gcc_powerpc/crosstool-0.42/build/powerpc-7450-linux-gnu/gcc-3.3.1-glibc-2.3.2/gcc-3.3.1/gcc/config/rs6000/altivec.md:807 */
rtx
gen_uminv4si3 (operand0, operand1, operand2)
     rtx operand0;
     rtx operand1;
     rtx operand2;
{
  return gen_rtx_SET (VOIDmode,
	operand0,
	gen_rtx_UMIN (V4SImode,
	operand1,
	operand2));
}

/* /home/yu/gcc/gcc_powerpc/crosstool-0.42/build/powerpc-7450-linux-gnu/gcc-3.3.1-glibc-2.3.2/gcc-3.3.1/gcc/config/rs6000/altivec.md:815 */
rtx
gen_sminv4si3 (operand0, operand1, operand2)
     rtx operand0;
     rtx operand1;
     rtx operand2;
{
  return gen_rtx_SET (VOIDmode,
	operand0,
	gen_rtx_SMIN (V4SImode,
	operand1,
	operand2));
}

/* /home/yu/gcc/gcc_powerpc/crosstool-0.42/build/powerpc-7450-linux-gnu/gcc-3.3.1-glibc-2.3.2/gcc-3.3.1/gcc/config/rs6000/altivec.md:823 */
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

/* /home/yu/gcc/gcc_powerpc/crosstool-0.42/build/powerpc-7450-linux-gnu/gcc-3.3.1-glibc-2.3.2/gcc-3.3.1/gcc/config/rs6000/altivec.md:831 */
rtx
gen_altivec_vmuleub (operand0, operand1, operand2)
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
	83));
}

/* /home/yu/gcc/gcc_powerpc/crosstool-0.42/build/powerpc-7450-linux-gnu/gcc-3.3.1-glibc-2.3.2/gcc-3.3.1/gcc/config/rs6000/altivec.md:839 */
rtx
gen_altivec_vmulesb (operand0, operand1, operand2)
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
	84));
}

/* /home/yu/gcc/gcc_powerpc/crosstool-0.42/build/powerpc-7450-linux-gnu/gcc-3.3.1-glibc-2.3.2/gcc-3.3.1/gcc/config/rs6000/altivec.md:847 */
rtx
gen_altivec_vmuleuh (operand0, operand1, operand2)
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
	85));
}

/* /home/yu/gcc/gcc_powerpc/crosstool-0.42/build/powerpc-7450-linux-gnu/gcc-3.3.1-glibc-2.3.2/gcc-3.3.1/gcc/config/rs6000/altivec.md:855 */
rtx
gen_altivec_vmulesh (operand0, operand1, operand2)
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
	86));
}

/* /home/yu/gcc/gcc_powerpc/crosstool-0.42/build/powerpc-7450-linux-gnu/gcc-3.3.1-glibc-2.3.2/gcc-3.3.1/gcc/config/rs6000/altivec.md:863 */
rtx
gen_altivec_vmuloub (operand0, operand1, operand2)
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
	87));
}

/* /home/yu/gcc/gcc_powerpc/crosstool-0.42/build/powerpc-7450-linux-gnu/gcc-3.3.1-glibc-2.3.2/gcc-3.3.1/gcc/config/rs6000/altivec.md:871 */
rtx
gen_altivec_vmulosb (operand0, operand1, operand2)
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
	88));
}

/* /home/yu/gcc/gcc_powerpc/crosstool-0.42/build/powerpc-7450-linux-gnu/gcc-3.3.1-glibc-2.3.2/gcc-3.3.1/gcc/config/rs6000/altivec.md:879 */
rtx
gen_altivec_vmulouh (operand0, operand1, operand2)
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
	89));
}

/* /home/yu/gcc/gcc_powerpc/crosstool-0.42/build/powerpc-7450-linux-gnu/gcc-3.3.1-glibc-2.3.2/gcc-3.3.1/gcc/config/rs6000/altivec.md:887 */
rtx
gen_altivec_vmulosh (operand0, operand1, operand2)
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
	90));
}

/* /home/yu/gcc/gcc_powerpc/crosstool-0.42/build/powerpc-7450-linux-gnu/gcc-3.3.1-glibc-2.3.2/gcc-3.3.1/gcc/config/rs6000/altivec.md:895 */
rtx
gen_altivec_vnor (operand0, operand1, operand2)
     rtx operand0;
     rtx operand1;
     rtx operand2;
{
  return gen_rtx_SET (VOIDmode,
	operand0,
	gen_rtx_NOT (V4SImode,
	gen_rtx_IOR (V4SImode,
	operand1,
	operand2)));
}

/* /home/yu/gcc/gcc_powerpc/crosstool-0.42/build/powerpc-7450-linux-gnu/gcc-3.3.1-glibc-2.3.2/gcc-3.3.1/gcc/config/rs6000/altivec.md:903 */
rtx
gen_iorv4si3 (operand0, operand1, operand2)
     rtx operand0;
     rtx operand1;
     rtx operand2;
{
  return gen_rtx_SET (VOIDmode,
	operand0,
	gen_rtx_IOR (V4SImode,
	operand1,
	operand2));
}

/* /home/yu/gcc/gcc_powerpc/crosstool-0.42/build/powerpc-7450-linux-gnu/gcc-3.3.1-glibc-2.3.2/gcc-3.3.1/gcc/config/rs6000/altivec.md:911 */
rtx
gen_altivec_vpkuhum (operand0, operand1, operand2)
     rtx operand0;
     rtx operand1;
     rtx operand2;
{
  return gen_rtx_SET (VOIDmode,
	operand0,
	gen_rtx_UNSPEC (V16QImode,
	gen_rtvec (2,
		operand1,
		operand2),
	93));
}

/* /home/yu/gcc/gcc_powerpc/crosstool-0.42/build/powerpc-7450-linux-gnu/gcc-3.3.1-glibc-2.3.2/gcc-3.3.1/gcc/config/rs6000/altivec.md:919 */
rtx
gen_altivec_vpkuwum (operand0, operand1, operand2)
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
	94));
}

/* /home/yu/gcc/gcc_powerpc/crosstool-0.42/build/powerpc-7450-linux-gnu/gcc-3.3.1-glibc-2.3.2/gcc-3.3.1/gcc/config/rs6000/altivec.md:927 */
rtx
gen_altivec_vpkpx (operand0, operand1, operand2)
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
	95));
}

/* /home/yu/gcc/gcc_powerpc/crosstool-0.42/build/powerpc-7450-linux-gnu/gcc-3.3.1-glibc-2.3.2/gcc-3.3.1/gcc/config/rs6000/altivec.md:935 */
rtx
gen_altivec_vpkuhss (operand0, operand1, operand2)
     rtx operand0;
     rtx operand1;
     rtx operand2;
{
  return gen_rtx_PARALLEL (VOIDmode, gen_rtvec (2,
		gen_rtx_SET (VOIDmode,
	operand0,
	gen_rtx_UNSPEC (V16QImode,
	gen_rtvec (2,
		operand1,
		operand2),
	96)),
		gen_rtx_SET (VOIDmode,
	gen_rtx_REG (SImode,
	110),
	gen_rtx_UNSPEC (SImode,
	gen_rtvec (1,
		const0_rtx),
	213))));
}

/* /home/yu/gcc/gcc_powerpc/crosstool-0.42/build/powerpc-7450-linux-gnu/gcc-3.3.1-glibc-2.3.2/gcc-3.3.1/gcc/config/rs6000/altivec.md:944 */
rtx
gen_altivec_vpkshss (operand0, operand1, operand2)
     rtx operand0;
     rtx operand1;
     rtx operand2;
{
  return gen_rtx_PARALLEL (VOIDmode, gen_rtvec (2,
		gen_rtx_SET (VOIDmode,
	operand0,
	gen_rtx_UNSPEC (V16QImode,
	gen_rtvec (2,
		operand1,
		operand2),
	97)),
		gen_rtx_SET (VOIDmode,
	gen_rtx_REG (SImode,
	110),
	gen_rtx_UNSPEC (SImode,
	gen_rtvec (1,
		const0_rtx),
	213))));
}

/* /home/yu/gcc/gcc_powerpc/crosstool-0.42/build/powerpc-7450-linux-gnu/gcc-3.3.1-glibc-2.3.2/gcc-3.3.1/gcc/config/rs6000/altivec.md:953 */
rtx
gen_altivec_vpkuwss (operand0, operand1, operand2)
     rtx operand0;
     rtx operand1;
     rtx operand2;
{
  return gen_rtx_PARALLEL (VOIDmode, gen_rtvec (2,
		gen_rtx_SET (VOIDmode,
	operand0,
	gen_rtx_UNSPEC (V8HImode,
	gen_rtvec (2,
		operand1,
		operand2),
	98)),
		gen_rtx_SET (VOIDmode,
	gen_rtx_REG (SImode,
	110),
	gen_rtx_UNSPEC (SImode,
	gen_rtvec (1,
		const0_rtx),
	213))));
}

/* /home/yu/gcc/gcc_powerpc/crosstool-0.42/build/powerpc-7450-linux-gnu/gcc-3.3.1-glibc-2.3.2/gcc-3.3.1/gcc/config/rs6000/altivec.md:962 */
rtx
gen_altivec_vpkswss (operand0, operand1, operand2)
     rtx operand0;
     rtx operand1;
     rtx operand2;
{
  return gen_rtx_PARALLEL (VOIDmode, gen_rtvec (2,
		gen_rtx_SET (VOIDmode,
	operand0,
	gen_rtx_UNSPEC (V8HImode,
	gen_rtvec (2,
		operand1,
		operand2),
	99)),
		gen_rtx_SET (VOIDmode,
	gen_rtx_REG (SImode,
	110),
	gen_rtx_UNSPEC (SImode,
	gen_rtvec (1,
		const0_rtx),
	213))));
}

/* /home/yu/gcc/gcc_powerpc/crosstool-0.42/build/powerpc-7450-linux-gnu/gcc-3.3.1-glibc-2.3.2/gcc-3.3.1/gcc/config/rs6000/altivec.md:971 */
rtx
gen_altivec_vpkuhus (operand0, operand1, operand2)
     rtx operand0;
     rtx operand1;
     rtx operand2;
{
  return gen_rtx_PARALLEL (VOIDmode, gen_rtvec (2,
		gen_rtx_SET (VOIDmode,
	operand0,
	gen_rtx_UNSPEC (V16QImode,
	gen_rtvec (2,
		operand1,
		operand2),
	100)),
		gen_rtx_SET (VOIDmode,
	gen_rtx_REG (SImode,
	110),
	gen_rtx_UNSPEC (SImode,
	gen_rtvec (1,
		const0_rtx),
	213))));
}

/* /home/yu/gcc/gcc_powerpc/crosstool-0.42/build/powerpc-7450-linux-gnu/gcc-3.3.1-glibc-2.3.2/gcc-3.3.1/gcc/config/rs6000/altivec.md:980 */
rtx
gen_altivec_vpkshus (operand0, operand1, operand2)
     rtx operand0;
     rtx operand1;
     rtx operand2;
{
  return gen_rtx_PARALLEL (VOIDmode, gen_rtvec (2,
		gen_rtx_SET (VOIDmode,
	operand0,
	gen_rtx_UNSPEC (V16QImode,
	gen_rtvec (2,
		operand1,
		operand2),
	101)),
		gen_rtx_SET (VOIDmode,
	gen_rtx_REG (SImode,
	110),
	gen_rtx_UNSPEC (SImode,
	gen_rtvec (1,
		const0_rtx),
	213))));
}

/* /home/yu/gcc/gcc_powerpc/crosstool-0.42/build/powerpc-7450-linux-gnu/gcc-3.3.1-glibc-2.3.2/gcc-3.3.1/gcc/config/rs6000/altivec.md:989 */
rtx
gen_altivec_vpkuwus (operand0, operand1, operand2)
     rtx operand0;
     rtx operand1;
     rtx operand2;
{
  return gen_rtx_PARALLEL (VOIDmode, gen_rtvec (2,
		gen_rtx_SET (VOIDmode,
	operand0,
	gen_rtx_UNSPEC (V8HImode,
	gen_rtvec (2,
		operand1,
		operand2),
	102)),
		gen_rtx_SET (VOIDmode,
	gen_rtx_REG (SImode,
	110),
	gen_rtx_UNSPEC (SImode,
	gen_rtvec (1,
		const0_rtx),
	213))));
}

/* /home/yu/gcc/gcc_powerpc/crosstool-0.42/build/powerpc-7450-linux-gnu/gcc-3.3.1-glibc-2.3.2/gcc-3.3.1/gcc/config/rs6000/altivec.md:998 */
rtx
gen_altivec_vpkswus (operand0, operand1, operand2)
     rtx operand0;
     rtx operand1;
     rtx operand2;
{
  return gen_rtx_PARALLEL (VOIDmode, gen_rtvec (2,
		gen_rtx_SET (VOIDmode,
	operand0,
	gen_rtx_UNSPEC (V8HImode,
	gen_rtvec (2,
		operand1,
		operand2),
	103)),
		gen_rtx_SET (VOIDmode,
	gen_rtx_REG (SImode,
	110),
	gen_rtx_UNSPEC (SImode,
	gen_rtvec (1,
		const0_rtx),
	213))));
}

/* /home/yu/gcc/gcc_powerpc/crosstool-0.42/build/powerpc-7450-linux-gnu/gcc-3.3.1-glibc-2.3.2/gcc-3.3.1/gcc/config/rs6000/altivec.md:1007 */
rtx
gen_altivec_vrlb (operand0, operand1, operand2)
     rtx operand0;
     rtx operand1;
     rtx operand2;
{
  return gen_rtx_SET (VOIDmode,
	operand0,
	gen_rtx_UNSPEC (V16QImode,
	gen_rtvec (2,
		operand1,
		operand2),
	104));
}

/* /home/yu/gcc/gcc_powerpc/crosstool-0.42/build/powerpc-7450-linux-gnu/gcc-3.3.1-glibc-2.3.2/gcc-3.3.1/gcc/config/rs6000/altivec.md:1015 */
rtx
gen_altivec_vrlh (operand0, operand1, operand2)
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
	105));
}

/* /home/yu/gcc/gcc_powerpc/crosstool-0.42/build/powerpc-7450-linux-gnu/gcc-3.3.1-glibc-2.3.2/gcc-3.3.1/gcc/config/rs6000/altivec.md:1023 */
rtx
gen_altivec_vrlw (operand0, operand1, operand2)
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
	106));
}

/* /home/yu/gcc/gcc_powerpc/crosstool-0.42/build/powerpc-7450-linux-gnu/gcc-3.3.1-glibc-2.3.2/gcc-3.3.1/gcc/config/rs6000/altivec.md:1031 */
rtx
gen_altivec_vslb (operand0, operand1, operand2)
     rtx operand0;
     rtx operand1;
     rtx operand2;
{
  return gen_rtx_SET (VOIDmode,
	operand0,
	gen_rtx_UNSPEC (V16QImode,
	gen_rtvec (2,
		operand1,
		operand2),
	107));
}

/* /home/yu/gcc/gcc_powerpc/crosstool-0.42/build/powerpc-7450-linux-gnu/gcc-3.3.1-glibc-2.3.2/gcc-3.3.1/gcc/config/rs6000/altivec.md:1039 */
rtx
gen_altivec_vslh (operand0, operand1, operand2)
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
	108));
}

/* /home/yu/gcc/gcc_powerpc/crosstool-0.42/build/powerpc-7450-linux-gnu/gcc-3.3.1-glibc-2.3.2/gcc-3.3.1/gcc/config/rs6000/altivec.md:1047 */
rtx
gen_altivec_vslw (operand0, operand1, operand2)
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
	109));
}

/* /home/yu/gcc/gcc_powerpc/crosstool-0.42/build/powerpc-7450-linux-gnu/gcc-3.3.1-glibc-2.3.2/gcc-3.3.1/gcc/config/rs6000/altivec.md:1055 */
rtx
gen_altivec_vslw_v4sf (operand0, operand1, operand2)
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
	109));
}

/* /home/yu/gcc/gcc_powerpc/crosstool-0.42/build/powerpc-7450-linux-gnu/gcc-3.3.1-glibc-2.3.2/gcc-3.3.1/gcc/config/rs6000/altivec.md:1063 */
rtx
gen_altivec_vsl (operand0, operand1, operand2)
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
	110));
}

/* /home/yu/gcc/gcc_powerpc/crosstool-0.42/build/powerpc-7450-linux-gnu/gcc-3.3.1-glibc-2.3.2/gcc-3.3.1/gcc/config/rs6000/altivec.md:1071 */
rtx
gen_altivec_vslo (operand0, operand1, operand2)
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
	111));
}

/* /home/yu/gcc/gcc_powerpc/crosstool-0.42/build/powerpc-7450-linux-gnu/gcc-3.3.1-glibc-2.3.2/gcc-3.3.1/gcc/config/rs6000/altivec.md:1079 */
rtx
gen_altivec_vsrb (operand0, operand1, operand2)
     rtx operand0;
     rtx operand1;
     rtx operand2;
{
  return gen_rtx_SET (VOIDmode,
	operand0,
	gen_rtx_UNSPEC (V16QImode,
	gen_rtvec (2,
		operand1,
		operand2),
	112));
}

/* /home/yu/gcc/gcc_powerpc/crosstool-0.42/build/powerpc-7450-linux-gnu/gcc-3.3.1-glibc-2.3.2/gcc-3.3.1/gcc/config/rs6000/altivec.md:1087 */
rtx
gen_altivec_vsrh (operand0, operand1, operand2)
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
	113));
}

/* /home/yu/gcc/gcc_powerpc/crosstool-0.42/build/powerpc-7450-linux-gnu/gcc-3.3.1-glibc-2.3.2/gcc-3.3.1/gcc/config/rs6000/altivec.md:1095 */
rtx
gen_altivec_vsrw (operand0, operand1, operand2)
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
	114));
}

/* /home/yu/gcc/gcc_powerpc/crosstool-0.42/build/powerpc-7450-linux-gnu/gcc-3.3.1-glibc-2.3.2/gcc-3.3.1/gcc/config/rs6000/altivec.md:1103 */
rtx
gen_altivec_vsrab (operand0, operand1, operand2)
     rtx operand0;
     rtx operand1;
     rtx operand2;
{
  return gen_rtx_SET (VOIDmode,
	operand0,
	gen_rtx_UNSPEC (V16QImode,
	gen_rtvec (2,
		operand1,
		operand2),
	115));
}

/* /home/yu/gcc/gcc_powerpc/crosstool-0.42/build/powerpc-7450-linux-gnu/gcc-3.3.1-glibc-2.3.2/gcc-3.3.1/gcc/config/rs6000/altivec.md:1111 */
rtx
gen_altivec_vsrah (operand0, operand1, operand2)
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
	116));
}

/* /home/yu/gcc/gcc_powerpc/crosstool-0.42/build/powerpc-7450-linux-gnu/gcc-3.3.1-glibc-2.3.2/gcc-3.3.1/gcc/config/rs6000/altivec.md:1119 */
rtx
gen_altivec_vsraw (operand0, operand1, operand2)
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
	117));
}

/* /home/yu/gcc/gcc_powerpc/crosstool-0.42/build/powerpc-7450-linux-gnu/gcc-3.3.1-glibc-2.3.2/gcc-3.3.1/gcc/config/rs6000/altivec.md:1127 */
rtx
gen_altivec_vsr (operand0, operand1, operand2)
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
	118));
}

/* /home/yu/gcc/gcc_powerpc/crosstool-0.42/build/powerpc-7450-linux-gnu/gcc-3.3.1-glibc-2.3.2/gcc-3.3.1/gcc/config/rs6000/altivec.md:1135 */
rtx
gen_altivec_vsro (operand0, operand1, operand2)
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
	119));
}

/* /home/yu/gcc/gcc_powerpc/crosstool-0.42/build/powerpc-7450-linux-gnu/gcc-3.3.1-glibc-2.3.2/gcc-3.3.1/gcc/config/rs6000/altivec.md:1143 */
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

/* /home/yu/gcc/gcc_powerpc/crosstool-0.42/build/powerpc-7450-linux-gnu/gcc-3.3.1-glibc-2.3.2/gcc-3.3.1/gcc/config/rs6000/altivec.md:1151 */
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

/* /home/yu/gcc/gcc_powerpc/crosstool-0.42/build/powerpc-7450-linux-gnu/gcc-3.3.1-glibc-2.3.2/gcc-3.3.1/gcc/config/rs6000/altivec.md:1159 */
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

/* /home/yu/gcc/gcc_powerpc/crosstool-0.42/build/powerpc-7450-linux-gnu/gcc-3.3.1-glibc-2.3.2/gcc-3.3.1/gcc/config/rs6000/altivec.md:1167 */
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

/* /home/yu/gcc/gcc_powerpc/crosstool-0.42/build/powerpc-7450-linux-gnu/gcc-3.3.1-glibc-2.3.2/gcc-3.3.1/gcc/config/rs6000/altivec.md:1175 */
rtx
gen_altivec_vsubcuw (operand0, operand1, operand2)
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
	124));
}

/* /home/yu/gcc/gcc_powerpc/crosstool-0.42/build/powerpc-7450-linux-gnu/gcc-3.3.1-glibc-2.3.2/gcc-3.3.1/gcc/config/rs6000/altivec.md:1183 */
rtx
gen_altivec_vsububs (operand0, operand1, operand2)
     rtx operand0;
     rtx operand1;
     rtx operand2;
{
  return gen_rtx_PARALLEL (VOIDmode, gen_rtvec (2,
		gen_rtx_SET (VOIDmode,
	operand0,
	gen_rtx_UNSPEC (V16QImode,
	gen_rtvec (2,
		operand1,
		operand2),
	125)),
		gen_rtx_SET (VOIDmode,
	gen_rtx_REG (SImode,
	110),
	gen_rtx_UNSPEC (SImode,
	gen_rtvec (1,
		const0_rtx),
	213))));
}

/* /home/yu/gcc/gcc_powerpc/crosstool-0.42/build/powerpc-7450-linux-gnu/gcc-3.3.1-glibc-2.3.2/gcc-3.3.1/gcc/config/rs6000/altivec.md:1192 */
rtx
gen_altivec_vsubsbs (operand0, operand1, operand2)
     rtx operand0;
     rtx operand1;
     rtx operand2;
{
  return gen_rtx_PARALLEL (VOIDmode, gen_rtvec (2,
		gen_rtx_SET (VOIDmode,
	operand0,
	gen_rtx_UNSPEC (V16QImode,
	gen_rtvec (2,
		operand1,
		operand2),
	126)),
		gen_rtx_SET (VOIDmode,
	gen_rtx_REG (SImode,
	110),
	gen_rtx_UNSPEC (SImode,
	gen_rtvec (1,
		const0_rtx),
	213))));
}

/* /home/yu/gcc/gcc_powerpc/crosstool-0.42/build/powerpc-7450-linux-gnu/gcc-3.3.1-glibc-2.3.2/gcc-3.3.1/gcc/config/rs6000/altivec.md:1201 */
rtx
gen_altivec_vsubuhs (operand0, operand1, operand2)
     rtx operand0;
     rtx operand1;
     rtx operand2;
{
  return gen_rtx_PARALLEL (VOIDmode, gen_rtvec (2,
		gen_rtx_SET (VOIDmode,
	operand0,
	gen_rtx_UNSPEC (V8HImode,
	gen_rtvec (2,
		operand1,
		operand2),
	127)),
		gen_rtx_SET (VOIDmode,
	gen_rtx_REG (SImode,
	110),
	gen_rtx_UNSPEC (SImode,
	gen_rtvec (1,
		const0_rtx),
	213))));
}

/* /home/yu/gcc/gcc_powerpc/crosstool-0.42/build/powerpc-7450-linux-gnu/gcc-3.3.1-glibc-2.3.2/gcc-3.3.1/gcc/config/rs6000/altivec.md:1210 */
rtx
gen_altivec_vsubshs (operand0, operand1, operand2)
     rtx operand0;
     rtx operand1;
     rtx operand2;
{
  return gen_rtx_PARALLEL (VOIDmode, gen_rtvec (2,
		gen_rtx_SET (VOIDmode,
	operand0,
	gen_rtx_UNSPEC (V8HImode,
	gen_rtvec (2,
		operand1,
		operand2),
	128)),
		gen_rtx_SET (VOIDmode,
	gen_rtx_REG (SImode,
	110),
	gen_rtx_UNSPEC (SImode,
	gen_rtvec (1,
		const0_rtx),
	213))));
}

/* /home/yu/gcc/gcc_powerpc/crosstool-0.42/build/powerpc-7450-linux-gnu/gcc-3.3.1-glibc-2.3.2/gcc-3.3.1/gcc/config/rs6000/altivec.md:1219 */
rtx
gen_altivec_vsubuws (operand0, operand1, operand2)
     rtx operand0;
     rtx operand1;
     rtx operand2;
{
  return gen_rtx_PARALLEL (VOIDmode, gen_rtvec (2,
		gen_rtx_SET (VOIDmode,
	operand0,
	gen_rtx_UNSPEC (V4SImode,
	gen_rtvec (2,
		operand1,
		operand2),
	129)),
		gen_rtx_SET (VOIDmode,
	gen_rtx_REG (SImode,
	110),
	gen_rtx_UNSPEC (SImode,
	gen_rtvec (1,
		const0_rtx),
	213))));
}

/* /home/yu/gcc/gcc_powerpc/crosstool-0.42/build/powerpc-7450-linux-gnu/gcc-3.3.1-glibc-2.3.2/gcc-3.3.1/gcc/config/rs6000/altivec.md:1228 */
rtx
gen_altivec_vsubsws (operand0, operand1, operand2)
     rtx operand0;
     rtx operand1;
     rtx operand2;
{
  return gen_rtx_PARALLEL (VOIDmode, gen_rtvec (2,
		gen_rtx_SET (VOIDmode,
	operand0,
	gen_rtx_UNSPEC (V4SImode,
	gen_rtvec (2,
		operand1,
		operand2),
	130)),
		gen_rtx_SET (VOIDmode,
	gen_rtx_REG (SImode,
	110),
	gen_rtx_UNSPEC (SImode,
	gen_rtvec (1,
		const0_rtx),
	213))));
}

/* /home/yu/gcc/gcc_powerpc/crosstool-0.42/build/powerpc-7450-linux-gnu/gcc-3.3.1-glibc-2.3.2/gcc-3.3.1/gcc/config/rs6000/altivec.md:1237 */
rtx
gen_altivec_vsum4ubs (operand0, operand1, operand2)
     rtx operand0;
     rtx operand1;
     rtx operand2;
{
  return gen_rtx_PARALLEL (VOIDmode, gen_rtvec (2,
		gen_rtx_SET (VOIDmode,
	operand0,
	gen_rtx_UNSPEC (V4SImode,
	gen_rtvec (2,
		operand1,
		operand2),
	131)),
		gen_rtx_SET (VOIDmode,
	gen_rtx_REG (SImode,
	110),
	gen_rtx_UNSPEC (SImode,
	gen_rtvec (1,
		const0_rtx),
	213))));
}

/* /home/yu/gcc/gcc_powerpc/crosstool-0.42/build/powerpc-7450-linux-gnu/gcc-3.3.1-glibc-2.3.2/gcc-3.3.1/gcc/config/rs6000/altivec.md:1246 */
rtx
gen_altivec_vsum4sbs (operand0, operand1, operand2)
     rtx operand0;
     rtx operand1;
     rtx operand2;
{
  return gen_rtx_PARALLEL (VOIDmode, gen_rtvec (2,
		gen_rtx_SET (VOIDmode,
	operand0,
	gen_rtx_UNSPEC (V4SImode,
	gen_rtvec (2,
		operand1,
		operand2),
	132)),
		gen_rtx_SET (VOIDmode,
	gen_rtx_REG (SImode,
	110),
	gen_rtx_UNSPEC (SImode,
	gen_rtvec (1,
		const0_rtx),
	213))));
}

/* /home/yu/gcc/gcc_powerpc/crosstool-0.42/build/powerpc-7450-linux-gnu/gcc-3.3.1-glibc-2.3.2/gcc-3.3.1/gcc/config/rs6000/altivec.md:1255 */
rtx
gen_altivec_vsum4shs (operand0, operand1, operand2)
     rtx operand0;
     rtx operand1;
     rtx operand2;
{
  return gen_rtx_PARALLEL (VOIDmode, gen_rtvec (2,
		gen_rtx_SET (VOIDmode,
	operand0,
	gen_rtx_UNSPEC (V4SImode,
	gen_rtvec (2,
		operand1,
		operand2),
	133)),
		gen_rtx_SET (VOIDmode,
	gen_rtx_REG (SImode,
	110),
	gen_rtx_UNSPEC (SImode,
	gen_rtvec (1,
		const0_rtx),
	213))));
}

/* /home/yu/gcc/gcc_powerpc/crosstool-0.42/build/powerpc-7450-linux-gnu/gcc-3.3.1-glibc-2.3.2/gcc-3.3.1/gcc/config/rs6000/altivec.md:1264 */
rtx
gen_altivec_vsum2sws (operand0, operand1, operand2)
     rtx operand0;
     rtx operand1;
     rtx operand2;
{
  return gen_rtx_PARALLEL (VOIDmode, gen_rtvec (2,
		gen_rtx_SET (VOIDmode,
	operand0,
	gen_rtx_UNSPEC (V4SImode,
	gen_rtvec (2,
		operand1,
		operand2),
	134)),
		gen_rtx_SET (VOIDmode,
	gen_rtx_REG (SImode,
	110),
	gen_rtx_UNSPEC (SImode,
	gen_rtvec (1,
		const0_rtx),
	213))));
}

/* /home/yu/gcc/gcc_powerpc/crosstool-0.42/build/powerpc-7450-linux-gnu/gcc-3.3.1-glibc-2.3.2/gcc-3.3.1/gcc/config/rs6000/altivec.md:1273 */
rtx
gen_altivec_vsumsws (operand0, operand1, operand2)
     rtx operand0;
     rtx operand1;
     rtx operand2;
{
  return gen_rtx_PARALLEL (VOIDmode, gen_rtvec (2,
		gen_rtx_SET (VOIDmode,
	operand0,
	gen_rtx_UNSPEC (V4SImode,
	gen_rtvec (2,
		operand1,
		operand2),
	135)),
		gen_rtx_SET (VOIDmode,
	gen_rtx_REG (SImode,
	110),
	gen_rtx_UNSPEC (SImode,
	gen_rtvec (1,
		const0_rtx),
	213))));
}

/* /home/yu/gcc/gcc_powerpc/crosstool-0.42/build/powerpc-7450-linux-gnu/gcc-3.3.1-glibc-2.3.2/gcc-3.3.1/gcc/config/rs6000/altivec.md:1282 */
rtx
gen_xorv4si3 (operand0, operand1, operand2)
     rtx operand0;
     rtx operand1;
     rtx operand2;
{
  return gen_rtx_SET (VOIDmode,
	operand0,
	gen_rtx_XOR (V4SImode,
	operand1,
	operand2));
}

/* /home/yu/gcc/gcc_powerpc/crosstool-0.42/build/powerpc-7450-linux-gnu/gcc-3.3.1-glibc-2.3.2/gcc-3.3.1/gcc/config/rs6000/altivec.md:1290 */
rtx
gen_altivec_vspltb (operand0, operand1, operand2)
     rtx operand0;
     rtx operand1;
     rtx operand2;
{
  return gen_rtx_SET (VOIDmode,
	operand0,
	gen_rtx_UNSPEC (V16QImode,
	gen_rtvec (2,
		operand1,
		operand2),
	136));
}

/* /home/yu/gcc/gcc_powerpc/crosstool-0.42/build/powerpc-7450-linux-gnu/gcc-3.3.1-glibc-2.3.2/gcc-3.3.1/gcc/config/rs6000/altivec.md:1298 */
rtx
gen_altivec_vsplth (operand0, operand1, operand2)
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
	137));
}

/* /home/yu/gcc/gcc_powerpc/crosstool-0.42/build/powerpc-7450-linux-gnu/gcc-3.3.1-glibc-2.3.2/gcc-3.3.1/gcc/config/rs6000/altivec.md:1306 */
rtx
gen_altivec_vspltw (operand0, operand1, operand2)
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
	138));
}

/* /home/yu/gcc/gcc_powerpc/crosstool-0.42/build/powerpc-7450-linux-gnu/gcc-3.3.1-glibc-2.3.2/gcc-3.3.1/gcc/config/rs6000/altivec.md:1314 */
rtx
gen_altivec_vspltisb (operand0, operand1)
     rtx operand0;
     rtx operand1;
{
  return gen_rtx_SET (VOIDmode,
	operand0,
	gen_rtx_UNSPEC (V16QImode,
	gen_rtvec (1,
		operand1),
	139));
}

/* /home/yu/gcc/gcc_powerpc/crosstool-0.42/build/powerpc-7450-linux-gnu/gcc-3.3.1-glibc-2.3.2/gcc-3.3.1/gcc/config/rs6000/altivec.md:1322 */
rtx
gen_altivec_vspltish (operand0, operand1)
     rtx operand0;
     rtx operand1;
{
  return gen_rtx_SET (VOIDmode,
	operand0,
	gen_rtx_UNSPEC (V8HImode,
	gen_rtvec (1,
		operand1),
	140));
}

/* /home/yu/gcc/gcc_powerpc/crosstool-0.42/build/powerpc-7450-linux-gnu/gcc-3.3.1-glibc-2.3.2/gcc-3.3.1/gcc/config/rs6000/altivec.md:1329 */
rtx
gen_altivec_vspltisw (operand0, operand1)
     rtx operand0;
     rtx operand1;
{
  return gen_rtx_SET (VOIDmode,
	operand0,
	gen_rtx_UNSPEC (V4SImode,
	gen_rtvec (1,
		operand1),
	141));
}

/* /home/yu/gcc/gcc_powerpc/crosstool-0.42/build/powerpc-7450-linux-gnu/gcc-3.3.1-glibc-2.3.2/gcc-3.3.1/gcc/config/rs6000/altivec.md:1336 */
rtx
gen_altivec_vspltisw_v4sf (operand0, operand1)
     rtx operand0;
     rtx operand1;
{
  return gen_rtx_SET (VOIDmode,
	operand0,
	gen_rtx_UNSPEC (V4SFmode,
	gen_rtvec (1,
		operand1),
	142));
}

/* /home/yu/gcc/gcc_powerpc/crosstool-0.42/build/powerpc-7450-linux-gnu/gcc-3.3.1-glibc-2.3.2/gcc-3.3.1/gcc/config/rs6000/altivec.md:1343 */
rtx
gen_ftruncv4sf2 (operand0, operand1)
     rtx operand0;
     rtx operand1;
{
  return gen_rtx_SET (VOIDmode,
	operand0,
	gen_rtx_FIX (V4SFmode,
	operand1));
}

/* /home/yu/gcc/gcc_powerpc/crosstool-0.42/build/powerpc-7450-linux-gnu/gcc-3.3.1-glibc-2.3.2/gcc-3.3.1/gcc/config/rs6000/altivec.md:1350 */
rtx
gen_altivec_vperm_4si (operand0, operand1, operand2, operand3)
     rtx operand0;
     rtx operand1;
     rtx operand2;
     rtx operand3;
{
  return gen_rtx_SET (VOIDmode,
	operand0,
	gen_rtx_UNSPEC (V4SImode,
	gen_rtvec (3,
		operand1,
		operand2,
		operand3),
	144));
}

/* /home/yu/gcc/gcc_powerpc/crosstool-0.42/build/powerpc-7450-linux-gnu/gcc-3.3.1-glibc-2.3.2/gcc-3.3.1/gcc/config/rs6000/altivec.md:1359 */
rtx
gen_altivec_vperm_4sf (operand0, operand1, operand2, operand3)
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
	145));
}

/* /home/yu/gcc/gcc_powerpc/crosstool-0.42/build/powerpc-7450-linux-gnu/gcc-3.3.1-glibc-2.3.2/gcc-3.3.1/gcc/config/rs6000/altivec.md:1368 */
rtx
gen_altivec_vperm_8hi (operand0, operand1, operand2, operand3)
     rtx operand0;
     rtx operand1;
     rtx operand2;
     rtx operand3;
{
  return gen_rtx_SET (VOIDmode,
	operand0,
	gen_rtx_UNSPEC (V8HImode,
	gen_rtvec (3,
		operand1,
		operand2,
		operand3),
	146));
}

/* /home/yu/gcc/gcc_powerpc/crosstool-0.42/build/powerpc-7450-linux-gnu/gcc-3.3.1-glibc-2.3.2/gcc-3.3.1/gcc/config/rs6000/altivec.md:1377 */
rtx
gen_altivec_vperm_16qi (operand0, operand1, operand2, operand3)
     rtx operand0;
     rtx operand1;
     rtx operand2;
     rtx operand3;
{
  return gen_rtx_SET (VOIDmode,
	operand0,
	gen_rtx_UNSPEC (V16QImode,
	gen_rtvec (3,
		operand1,
		operand2,
		operand3),
	147));
}

/* /home/yu/gcc/gcc_powerpc/crosstool-0.42/build/powerpc-7450-linux-gnu/gcc-3.3.1-glibc-2.3.2/gcc-3.3.1/gcc/config/rs6000/altivec.md:1386 */
rtx
gen_altivec_vrfip (operand0, operand1)
     rtx operand0;
     rtx operand1;
{
  return gen_rtx_SET (VOIDmode,
	operand0,
	gen_rtx_UNSPEC (V4SFmode,
	gen_rtvec (1,
		operand1),
	148));
}

/* /home/yu/gcc/gcc_powerpc/crosstool-0.42/build/powerpc-7450-linux-gnu/gcc-3.3.1-glibc-2.3.2/gcc-3.3.1/gcc/config/rs6000/altivec.md:1393 */
rtx
gen_altivec_vrfin (operand0, operand1)
     rtx operand0;
     rtx operand1;
{
  return gen_rtx_SET (VOIDmode,
	operand0,
	gen_rtx_UNSPEC (V4SFmode,
	gen_rtvec (1,
		operand1),
	149));
}

/* /home/yu/gcc/gcc_powerpc/crosstool-0.42/build/powerpc-7450-linux-gnu/gcc-3.3.1-glibc-2.3.2/gcc-3.3.1/gcc/config/rs6000/altivec.md:1400 */
rtx
gen_altivec_vrfim (operand0, operand1)
     rtx operand0;
     rtx operand1;
{
  return gen_rtx_SET (VOIDmode,
	operand0,
	gen_rtx_UNSPEC (V4SFmode,
	gen_rtvec (1,
		operand1),
	150));
}

/* /home/yu/gcc/gcc_powerpc/crosstool-0.42/build/powerpc-7450-linux-gnu/gcc-3.3.1-glibc-2.3.2/gcc-3.3.1/gcc/config/rs6000/altivec.md:1407 */
rtx
gen_altivec_vcfux (operand0, operand1, operand2)
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
	151));
}

/* /home/yu/gcc/gcc_powerpc/crosstool-0.42/build/powerpc-7450-linux-gnu/gcc-3.3.1-glibc-2.3.2/gcc-3.3.1/gcc/config/rs6000/altivec.md:1415 */
rtx
gen_altivec_vcfsx (operand0, operand1, operand2)
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
	152));
}

/* /home/yu/gcc/gcc_powerpc/crosstool-0.42/build/powerpc-7450-linux-gnu/gcc-3.3.1-glibc-2.3.2/gcc-3.3.1/gcc/config/rs6000/altivec.md:1423 */
rtx
gen_altivec_vctuxs (operand0, operand1, operand2)
     rtx operand0;
     rtx operand1;
     rtx operand2;
{
  return gen_rtx_PARALLEL (VOIDmode, gen_rtvec (2,
		gen_rtx_SET (VOIDmode,
	operand0,
	gen_rtx_UNSPEC (V4SImode,
	gen_rtvec (2,
		operand1,
		operand2),
	153)),
		gen_rtx_SET (VOIDmode,
	gen_rtx_REG (SImode,
	110),
	gen_rtx_UNSPEC (SImode,
	gen_rtvec (1,
		const0_rtx),
	213))));
}

/* /home/yu/gcc/gcc_powerpc/crosstool-0.42/build/powerpc-7450-linux-gnu/gcc-3.3.1-glibc-2.3.2/gcc-3.3.1/gcc/config/rs6000/altivec.md:1432 */
rtx
gen_altivec_vctsxs (operand0, operand1, operand2)
     rtx operand0;
     rtx operand1;
     rtx operand2;
{
  return gen_rtx_PARALLEL (VOIDmode, gen_rtvec (2,
		gen_rtx_SET (VOIDmode,
	operand0,
	gen_rtx_UNSPEC (V4SImode,
	gen_rtvec (2,
		operand1,
		operand2),
	154)),
		gen_rtx_SET (VOIDmode,
	gen_rtx_REG (SImode,
	110),
	gen_rtx_UNSPEC (SImode,
	gen_rtvec (1,
		const0_rtx),
	213))));
}

/* /home/yu/gcc/gcc_powerpc/crosstool-0.42/build/powerpc-7450-linux-gnu/gcc-3.3.1-glibc-2.3.2/gcc-3.3.1/gcc/config/rs6000/altivec.md:1441 */
rtx
gen_altivec_vlogefp (operand0, operand1)
     rtx operand0;
     rtx operand1;
{
  return gen_rtx_SET (VOIDmode,
	operand0,
	gen_rtx_UNSPEC (V4SFmode,
	gen_rtvec (1,
		operand1),
	155));
}

/* /home/yu/gcc/gcc_powerpc/crosstool-0.42/build/powerpc-7450-linux-gnu/gcc-3.3.1-glibc-2.3.2/gcc-3.3.1/gcc/config/rs6000/altivec.md:1448 */
rtx
gen_altivec_vexptefp (operand0, operand1)
     rtx operand0;
     rtx operand1;
{
  return gen_rtx_SET (VOIDmode,
	operand0,
	gen_rtx_UNSPEC (V4SFmode,
	gen_rtvec (1,
		operand1),
	156));
}

/* /home/yu/gcc/gcc_powerpc/crosstool-0.42/build/powerpc-7450-linux-gnu/gcc-3.3.1-glibc-2.3.2/gcc-3.3.1/gcc/config/rs6000/altivec.md:1455 */
rtx
gen_altivec_vrsqrtefp (operand0, operand1)
     rtx operand0;
     rtx operand1;
{
  return gen_rtx_SET (VOIDmode,
	operand0,
	gen_rtx_UNSPEC (V4SFmode,
	gen_rtvec (1,
		operand1),
	157));
}

/* /home/yu/gcc/gcc_powerpc/crosstool-0.42/build/powerpc-7450-linux-gnu/gcc-3.3.1-glibc-2.3.2/gcc-3.3.1/gcc/config/rs6000/altivec.md:1462 */
rtx
gen_altivec_vrefp (operand0, operand1)
     rtx operand0;
     rtx operand1;
{
  return gen_rtx_SET (VOIDmode,
	operand0,
	gen_rtx_UNSPEC (V4SFmode,
	gen_rtvec (1,
		operand1),
	158));
}

/* /home/yu/gcc/gcc_powerpc/crosstool-0.42/build/powerpc-7450-linux-gnu/gcc-3.3.1-glibc-2.3.2/gcc-3.3.1/gcc/config/rs6000/altivec.md:1469 */
rtx
gen_altivec_vsel_4si (operand0, operand1, operand2, operand3)
     rtx operand0;
     rtx operand1;
     rtx operand2;
     rtx operand3;
{
  return gen_rtx_SET (VOIDmode,
	operand0,
	gen_rtx_UNSPEC (V4SImode,
	gen_rtvec (3,
		operand1,
		operand2,
		operand3),
	159));
}

/* /home/yu/gcc/gcc_powerpc/crosstool-0.42/build/powerpc-7450-linux-gnu/gcc-3.3.1-glibc-2.3.2/gcc-3.3.1/gcc/config/rs6000/altivec.md:1478 */
rtx
gen_altivec_vsel_4sf (operand0, operand1, operand2, operand3)
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
	160));
}

/* /home/yu/gcc/gcc_powerpc/crosstool-0.42/build/powerpc-7450-linux-gnu/gcc-3.3.1-glibc-2.3.2/gcc-3.3.1/gcc/config/rs6000/altivec.md:1487 */
rtx
gen_altivec_vsel_8hi (operand0, operand1, operand2, operand3)
     rtx operand0;
     rtx operand1;
     rtx operand2;
     rtx operand3;
{
  return gen_rtx_SET (VOIDmode,
	operand0,
	gen_rtx_UNSPEC (V8HImode,
	gen_rtvec (3,
		operand1,
		operand2,
		operand3),
	161));
}

/* /home/yu/gcc/gcc_powerpc/crosstool-0.42/build/powerpc-7450-linux-gnu/gcc-3.3.1-glibc-2.3.2/gcc-3.3.1/gcc/config/rs6000/altivec.md:1496 */
rtx
gen_altivec_vsel_16qi (operand0, operand1, operand2, operand3)
     rtx operand0;
     rtx operand1;
     rtx operand2;
     rtx operand3;
{
  return gen_rtx_SET (VOIDmode,
	operand0,
	gen_rtx_UNSPEC (V16QImode,
	gen_rtvec (3,
		operand1,
		operand2,
		operand3),
	162));
}

/* /home/yu/gcc/gcc_powerpc/crosstool-0.42/build/powerpc-7450-linux-gnu/gcc-3.3.1-glibc-2.3.2/gcc-3.3.1/gcc/config/rs6000/altivec.md:1505 */
rtx
gen_altivec_vsldoi_4si (operand0, operand1, operand2, operand3)
     rtx operand0;
     rtx operand1;
     rtx operand2;
     rtx operand3;
{
  return gen_rtx_SET (VOIDmode,
	operand0,
	gen_rtx_UNSPEC (V4SImode,
	gen_rtvec (3,
		operand1,
		operand2,
		operand3),
	163));
}

/* /home/yu/gcc/gcc_powerpc/crosstool-0.42/build/powerpc-7450-linux-gnu/gcc-3.3.1-glibc-2.3.2/gcc-3.3.1/gcc/config/rs6000/altivec.md:1514 */
rtx
gen_altivec_vsldoi_4sf (operand0, operand1, operand2, operand3)
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
	164));
}

/* /home/yu/gcc/gcc_powerpc/crosstool-0.42/build/powerpc-7450-linux-gnu/gcc-3.3.1-glibc-2.3.2/gcc-3.3.1/gcc/config/rs6000/altivec.md:1523 */
rtx
gen_altivec_vsldoi_8hi (operand0, operand1, operand2, operand3)
     rtx operand0;
     rtx operand1;
     rtx operand2;
     rtx operand3;
{
  return gen_rtx_SET (VOIDmode,
	operand0,
	gen_rtx_UNSPEC (V8HImode,
	gen_rtvec (3,
		operand1,
		operand2,
		operand3),
	165));
}

/* /home/yu/gcc/gcc_powerpc/crosstool-0.42/build/powerpc-7450-linux-gnu/gcc-3.3.1-glibc-2.3.2/gcc-3.3.1/gcc/config/rs6000/altivec.md:1532 */
rtx
gen_altivec_vsldoi_16qi (operand0, operand1, operand2, operand3)
     rtx operand0;
     rtx operand1;
     rtx operand2;
     rtx operand3;
{
  return gen_rtx_SET (VOIDmode,
	operand0,
	gen_rtx_UNSPEC (V16QImode,
	gen_rtvec (3,
		operand1,
		operand2,
		operand3),
	166));
}

/* /home/yu/gcc/gcc_powerpc/crosstool-0.42/build/powerpc-7450-linux-gnu/gcc-3.3.1-glibc-2.3.2/gcc-3.3.1/gcc/config/rs6000/altivec.md:1541 */
rtx
gen_altivec_vupkhsb (operand0, operand1)
     rtx operand0;
     rtx operand1;
{
  return gen_rtx_SET (VOIDmode,
	operand0,
	gen_rtx_UNSPEC (V8HImode,
	gen_rtvec (1,
		operand1),
	167));
}

/* /home/yu/gcc/gcc_powerpc/crosstool-0.42/build/powerpc-7450-linux-gnu/gcc-3.3.1-glibc-2.3.2/gcc-3.3.1/gcc/config/rs6000/altivec.md:1548 */
rtx
gen_altivec_vupkhpx (operand0, operand1)
     rtx operand0;
     rtx operand1;
{
  return gen_rtx_SET (VOIDmode,
	operand0,
	gen_rtx_UNSPEC (V4SImode,
	gen_rtvec (1,
		operand1),
	168));
}

/* /home/yu/gcc/gcc_powerpc/crosstool-0.42/build/powerpc-7450-linux-gnu/gcc-3.3.1-glibc-2.3.2/gcc-3.3.1/gcc/config/rs6000/altivec.md:1555 */
rtx
gen_altivec_vupkhsh (operand0, operand1)
     rtx operand0;
     rtx operand1;
{
  return gen_rtx_SET (VOIDmode,
	operand0,
	gen_rtx_UNSPEC (V4SImode,
	gen_rtvec (1,
		operand1),
	169));
}

/* /home/yu/gcc/gcc_powerpc/crosstool-0.42/build/powerpc-7450-linux-gnu/gcc-3.3.1-glibc-2.3.2/gcc-3.3.1/gcc/config/rs6000/altivec.md:1562 */
rtx
gen_altivec_vupklsb (operand0, operand1)
     rtx operand0;
     rtx operand1;
{
  return gen_rtx_SET (VOIDmode,
	operand0,
	gen_rtx_UNSPEC (V8HImode,
	gen_rtvec (1,
		operand1),
	170));
}

/* /home/yu/gcc/gcc_powerpc/crosstool-0.42/build/powerpc-7450-linux-gnu/gcc-3.3.1-glibc-2.3.2/gcc-3.3.1/gcc/config/rs6000/altivec.md:1569 */
rtx
gen_altivec_vupklpx (operand0, operand1)
     rtx operand0;
     rtx operand1;
{
  return gen_rtx_SET (VOIDmode,
	operand0,
	gen_rtx_UNSPEC (V4SImode,
	gen_rtvec (1,
		operand1),
	171));
}

/* /home/yu/gcc/gcc_powerpc/crosstool-0.42/build/powerpc-7450-linux-gnu/gcc-3.3.1-glibc-2.3.2/gcc-3.3.1/gcc/config/rs6000/altivec.md:1576 */
rtx
gen_altivec_vupklsh (operand0, operand1)
     rtx operand0;
     rtx operand1;
{
  return gen_rtx_SET (VOIDmode,
	operand0,
	gen_rtx_UNSPEC (V4SImode,
	gen_rtvec (1,
		operand1),
	172));
}

/* /home/yu/gcc/gcc_powerpc/crosstool-0.42/build/powerpc-7450-linux-gnu/gcc-3.3.1-glibc-2.3.2/gcc-3.3.1/gcc/config/rs6000/altivec.md:1618 */
rtx
gen_altivec_predicate_v4si (operand0, operand1, operand2, operand3)
     rtx operand0;
     rtx operand1;
     rtx operand2;
     rtx operand3;
{
  return gen_rtx_PARALLEL (VOIDmode, gen_rtvec (2,
		gen_rtx_SET (VOIDmode,
	gen_rtx_REG (CCmode,
	74),
	gen_rtx_UNSPEC (CCmode,
	gen_rtvec (3,
		operand1,
		operand2,
		operand3),
	173)),
		gen_rtx_CLOBBER (VOIDmode,
	gen_rtx_SCRATCH (V4SImode))));
}

/* /home/yu/gcc/gcc_powerpc/crosstool-0.42/build/powerpc-7450-linux-gnu/gcc-3.3.1-glibc-2.3.2/gcc-3.3.1/gcc/config/rs6000/altivec.md:1628 */
rtx
gen_altivec_predicate_v4sf (operand0, operand1, operand2, operand3)
     rtx operand0;
     rtx operand1;
     rtx operand2;
     rtx operand3;
{
  return gen_rtx_PARALLEL (VOIDmode, gen_rtvec (2,
		gen_rtx_SET (VOIDmode,
	gen_rtx_REG (CCmode,
	74),
	gen_rtx_UNSPEC (CCmode,
	gen_rtvec (3,
		operand1,
		operand2,
		operand3),
	174)),
		gen_rtx_CLOBBER (VOIDmode,
	gen_rtx_SCRATCH (V4SFmode))));
}

/* /home/yu/gcc/gcc_powerpc/crosstool-0.42/build/powerpc-7450-linux-gnu/gcc-3.3.1-glibc-2.3.2/gcc-3.3.1/gcc/config/rs6000/altivec.md:1638 */
rtx
gen_altivec_predicate_v8hi (operand0, operand1, operand2, operand3)
     rtx operand0;
     rtx operand1;
     rtx operand2;
     rtx operand3;
{
  return gen_rtx_PARALLEL (VOIDmode, gen_rtvec (2,
		gen_rtx_SET (VOIDmode,
	gen_rtx_REG (CCmode,
	74),
	gen_rtx_UNSPEC (CCmode,
	gen_rtvec (3,
		operand1,
		operand2,
		operand3),
	175)),
		gen_rtx_CLOBBER (VOIDmode,
	gen_rtx_SCRATCH (V8HImode))));
}

/* /home/yu/gcc/gcc_powerpc/crosstool-0.42/build/powerpc-7450-linux-gnu/gcc-3.3.1-glibc-2.3.2/gcc-3.3.1/gcc/config/rs6000/altivec.md:1648 */
rtx
gen_altivec_predicate_v16qi (operand0, operand1, operand2, operand3)
     rtx operand0;
     rtx operand1;
     rtx operand2;
     rtx operand3;
{
  return gen_rtx_PARALLEL (VOIDmode, gen_rtvec (2,
		gen_rtx_SET (VOIDmode,
	gen_rtx_REG (CCmode,
	74),
	gen_rtx_UNSPEC (CCmode,
	gen_rtvec (3,
		operand1,
		operand2,
		operand3),
	175)),
		gen_rtx_CLOBBER (VOIDmode,
	gen_rtx_SCRATCH (V16QImode))));
}

/* /home/yu/gcc/gcc_powerpc/crosstool-0.42/build/powerpc-7450-linux-gnu/gcc-3.3.1-glibc-2.3.2/gcc-3.3.1/gcc/config/rs6000/altivec.md:1658 */
rtx
gen_altivec_mtvscr (operand0)
     rtx operand0;
{
  return gen_rtx_SET (VOIDmode,
	gen_rtx_REG (SImode,
	110),
	gen_rtx_UNSPEC_VOLATILE (SImode,
	gen_rtvec (1,
		operand0),
	186));
}

/* /home/yu/gcc/gcc_powerpc/crosstool-0.42/build/powerpc-7450-linux-gnu/gcc-3.3.1-glibc-2.3.2/gcc-3.3.1/gcc/config/rs6000/altivec.md:1666 */
rtx
gen_altivec_mfvscr (operand0)
     rtx operand0;
{
  return gen_rtx_SET (VOIDmode,
	operand0,
	gen_rtx_UNSPEC_VOLATILE (V8HImode,
	gen_rtvec (1,
		gen_rtx_REG (SImode,
	110)),
	187));
}

/* /home/yu/gcc/gcc_powerpc/crosstool-0.42/build/powerpc-7450-linux-gnu/gcc-3.3.1-glibc-2.3.2/gcc-3.3.1/gcc/config/rs6000/altivec.md:1673 */
rtx
gen_altivec_dssall ()
{
  return gen_rtx_UNSPEC (VOIDmode,
	gen_rtvec (1,
		const0_rtx),
	188);
}

/* /home/yu/gcc/gcc_powerpc/crosstool-0.42/build/powerpc-7450-linux-gnu/gcc-3.3.1-glibc-2.3.2/gcc-3.3.1/gcc/config/rs6000/altivec.md:1679 */
rtx
gen_altivec_dss (operand0)
     rtx operand0;
{
  return gen_rtx_UNSPEC (VOIDmode,
	gen_rtvec (1,
		operand0),
	189);
}

/* /home/yu/gcc/gcc_powerpc/crosstool-0.42/build/powerpc-7450-linux-gnu/gcc-3.3.1-glibc-2.3.2/gcc-3.3.1/gcc/config/rs6000/altivec.md:1685 */
rtx
gen_altivec_dst (operand0, operand1, operand2)
     rtx operand0;
     rtx operand1;
     rtx operand2;
{
  return gen_rtx_UNSPEC (VOIDmode,
	gen_rtvec (3,
		operand0,
		operand1,
		operand2),
	190);
}

/* /home/yu/gcc/gcc_powerpc/crosstool-0.42/build/powerpc-7450-linux-gnu/gcc-3.3.1-glibc-2.3.2/gcc-3.3.1/gcc/config/rs6000/altivec.md:1693 */
rtx
gen_altivec_dstt (operand0, operand1, operand2)
     rtx operand0;
     rtx operand1;
     rtx operand2;
{
  return gen_rtx_UNSPEC (VOIDmode,
	gen_rtvec (3,
		operand0,
		operand1,
		operand2),
	191);
}

/* /home/yu/gcc/gcc_powerpc/crosstool-0.42/build/powerpc-7450-linux-gnu/gcc-3.3.1-glibc-2.3.2/gcc-3.3.1/gcc/config/rs6000/altivec.md:1701 */
rtx
gen_altivec_dstst (operand0, operand1, operand2)
     rtx operand0;
     rtx operand1;
     rtx operand2;
{
  return gen_rtx_UNSPEC (VOIDmode,
	gen_rtvec (3,
		operand0,
		operand1,
		operand2),
	192);
}

/* /home/yu/gcc/gcc_powerpc/crosstool-0.42/build/powerpc-7450-linux-gnu/gcc-3.3.1-glibc-2.3.2/gcc-3.3.1/gcc/config/rs6000/altivec.md:1709 */
rtx
gen_altivec_dststt (operand0, operand1, operand2)
     rtx operand0;
     rtx operand1;
     rtx operand2;
{
  return gen_rtx_UNSPEC (VOIDmode,
	gen_rtvec (3,
		operand0,
		operand1,
		operand2),
	193);
}

/* /home/yu/gcc/gcc_powerpc/crosstool-0.42/build/powerpc-7450-linux-gnu/gcc-3.3.1-glibc-2.3.2/gcc-3.3.1/gcc/config/rs6000/altivec.md:1717 */
rtx
gen_altivec_lvsl (operand0, operand1, operand2)
     rtx operand0;
     rtx operand1;
     rtx operand2;
{
  return gen_rtx_SET (VOIDmode,
	operand0,
	gen_rtx_UNSPEC (V16QImode,
	gen_rtvec (2,
		operand1,
		operand2),
	194));
}

/* /home/yu/gcc/gcc_powerpc/crosstool-0.42/build/powerpc-7450-linux-gnu/gcc-3.3.1-glibc-2.3.2/gcc-3.3.1/gcc/config/rs6000/altivec.md:1725 */
rtx
gen_altivec_lvsr (operand0, operand1, operand2)
     rtx operand0;
     rtx operand1;
     rtx operand2;
{
  return gen_rtx_SET (VOIDmode,
	operand0,
	gen_rtx_UNSPEC (V16QImode,
	gen_rtvec (2,
		operand1,
		operand2),
	195));
}

/* /home/yu/gcc/gcc_powerpc/crosstool-0.42/build/powerpc-7450-linux-gnu/gcc-3.3.1-glibc-2.3.2/gcc-3.3.1/gcc/config/rs6000/altivec.md:1736 */
rtx
gen_altivec_lvebx (operand0, operand1, operand2)
     rtx operand0;
     rtx operand1;
     rtx operand2;
{
  return gen_rtx_PARALLEL (VOIDmode,
	gen_rtvec (2,
		gen_rtx_SET (VOIDmode,
	operand0,
	gen_rtx_MEM (V16QImode,
	gen_rtx_PLUS (SImode,
	operand1,
	operand2))),
		gen_rtx_UNSPEC (VOIDmode,
	gen_rtvec (1,
		const0_rtx),
	196)));
}

/* /home/yu/gcc/gcc_powerpc/crosstool-0.42/build/powerpc-7450-linux-gnu/gcc-3.3.1-glibc-2.3.2/gcc-3.3.1/gcc/config/rs6000/altivec.md:1746 */
rtx
gen_altivec_lvehx (operand0, operand1, operand2)
     rtx operand0;
     rtx operand1;
     rtx operand2;
{
  return gen_rtx_PARALLEL (VOIDmode,
	gen_rtvec (2,
		gen_rtx_SET (VOIDmode,
	operand0,
	gen_rtx_MEM (V8HImode,
	gen_rtx_AND (SImode,
	gen_rtx_PLUS (SImode,
	operand1,
	operand2),
	GEN_INT (-2LL)))),
		gen_rtx_UNSPEC (VOIDmode,
	gen_rtvec (1,
		const0_rtx),
	197)));
}

/* /home/yu/gcc/gcc_powerpc/crosstool-0.42/build/powerpc-7450-linux-gnu/gcc-3.3.1-glibc-2.3.2/gcc-3.3.1/gcc/config/rs6000/altivec.md:1758 */
rtx
gen_altivec_lvewx (operand0, operand1, operand2)
     rtx operand0;
     rtx operand1;
     rtx operand2;
{
  return gen_rtx_PARALLEL (VOIDmode,
	gen_rtvec (2,
		gen_rtx_SET (VOIDmode,
	operand0,
	gen_rtx_MEM (V4SImode,
	gen_rtx_AND (SImode,
	gen_rtx_PLUS (SImode,
	operand1,
	operand2),
	GEN_INT (-4LL)))),
		gen_rtx_UNSPEC (VOIDmode,
	gen_rtvec (1,
		const0_rtx),
	198)));
}

/* /home/yu/gcc/gcc_powerpc/crosstool-0.42/build/powerpc-7450-linux-gnu/gcc-3.3.1-glibc-2.3.2/gcc-3.3.1/gcc/config/rs6000/altivec.md:1770 */
rtx
gen_altivec_lvxl (operand0, operand1, operand2)
     rtx operand0;
     rtx operand1;
     rtx operand2;
{
  return gen_rtx_PARALLEL (VOIDmode,
	gen_rtvec (2,
		gen_rtx_SET (VOIDmode,
	operand0,
	gen_rtx_MEM (V4SImode,
	gen_rtx_PLUS (SImode,
	operand1,
	operand2))),
		gen_rtx_UNSPEC (VOIDmode,
	gen_rtvec (1,
		const0_rtx),
	213)));
}

/* /home/yu/gcc/gcc_powerpc/crosstool-0.42/build/powerpc-7450-linux-gnu/gcc-3.3.1-glibc-2.3.2/gcc-3.3.1/gcc/config/rs6000/altivec.md:1780 */
rtx
gen_altivec_lvx (operand0, operand1, operand2)
     rtx operand0;
     rtx operand1;
     rtx operand2;
{
  return gen_rtx_SET (VOIDmode,
	operand0,
	gen_rtx_MEM (V4SImode,
	gen_rtx_PLUS (SImode,
	operand1,
	operand2)));
}

/* /home/yu/gcc/gcc_powerpc/crosstool-0.42/build/powerpc-7450-linux-gnu/gcc-3.3.1-glibc-2.3.2/gcc-3.3.1/gcc/config/rs6000/altivec.md:1788 */
rtx
gen_altivec_stvx (operand0, operand1, operand2)
     rtx operand0;
     rtx operand1;
     rtx operand2;
{
  return gen_rtx_PARALLEL (VOIDmode,
	gen_rtvec (2,
		gen_rtx_SET (VOIDmode,
	gen_rtx_MEM (V4SImode,
	gen_rtx_AND (SImode,
	gen_rtx_PLUS (SImode,
	operand0,
	operand1),
	GEN_INT (-16LL))),
	operand2),
		gen_rtx_UNSPEC (VOIDmode,
	gen_rtvec (1,
		const0_rtx),
	201)));
}

/* /home/yu/gcc/gcc_powerpc/crosstool-0.42/build/powerpc-7450-linux-gnu/gcc-3.3.1-glibc-2.3.2/gcc-3.3.1/gcc/config/rs6000/altivec.md:1800 */
rtx
gen_altivec_stvxl (operand0, operand1, operand2)
     rtx operand0;
     rtx operand1;
     rtx operand2;
{
  return gen_rtx_PARALLEL (VOIDmode,
	gen_rtvec (2,
		gen_rtx_SET (VOIDmode,
	gen_rtx_MEM (V4SImode,
	gen_rtx_AND (SImode,
	gen_rtx_PLUS (SImode,
	operand0,
	operand1),
	GEN_INT (-16LL))),
	operand2),
		gen_rtx_UNSPEC (VOIDmode,
	gen_rtvec (1,
		const0_rtx),
	202)));
}

/* /home/yu/gcc/gcc_powerpc/crosstool-0.42/build/powerpc-7450-linux-gnu/gcc-3.3.1-glibc-2.3.2/gcc-3.3.1/gcc/config/rs6000/altivec.md:1812 */
rtx
gen_altivec_stvebx (operand0, operand1, operand2)
     rtx operand0;
     rtx operand1;
     rtx operand2;
{
  return gen_rtx_PARALLEL (VOIDmode,
	gen_rtvec (2,
		gen_rtx_SET (VOIDmode,
	gen_rtx_MEM (V16QImode,
	gen_rtx_PLUS (SImode,
	operand0,
	operand1)),
	operand2),
		gen_rtx_UNSPEC (VOIDmode,
	gen_rtvec (1,
		const0_rtx),
	203)));
}

/* /home/yu/gcc/gcc_powerpc/crosstool-0.42/build/powerpc-7450-linux-gnu/gcc-3.3.1-glibc-2.3.2/gcc-3.3.1/gcc/config/rs6000/altivec.md:1823 */
rtx
gen_altivec_stvehx (operand0, operand1, operand2)
     rtx operand0;
     rtx operand1;
     rtx operand2;
{
  return gen_rtx_PARALLEL (VOIDmode,
	gen_rtvec (2,
		gen_rtx_SET (VOIDmode,
	gen_rtx_MEM (V8HImode,
	gen_rtx_AND (SImode,
	gen_rtx_PLUS (SImode,
	operand0,
	operand1),
	GEN_INT (-2LL))),
	operand2),
		gen_rtx_UNSPEC (VOIDmode,
	gen_rtvec (1,
		const0_rtx),
	204)));
}

/* /home/yu/gcc/gcc_powerpc/crosstool-0.42/build/powerpc-7450-linux-gnu/gcc-3.3.1-glibc-2.3.2/gcc-3.3.1/gcc/config/rs6000/altivec.md:1835 */
rtx
gen_altivec_stvewx (operand0, operand1, operand2)
     rtx operand0;
     rtx operand1;
     rtx operand2;
{
  return gen_rtx_PARALLEL (VOIDmode,
	gen_rtvec (2,
		gen_rtx_SET (VOIDmode,
	gen_rtx_MEM (V4SImode,
	gen_rtx_AND (SImode,
	gen_rtx_PLUS (SImode,
	operand0,
	operand1),
	GEN_INT (-4LL))),
	operand2),
		gen_rtx_UNSPEC (VOIDmode,
	gen_rtvec (1,
		const0_rtx),
	205)));
}

/* /home/yu/gcc/gcc_powerpc/crosstool-0.42/build/powerpc-7450-linux-gnu/gcc-3.3.1-glibc-2.3.2/gcc-3.3.1/gcc/config/rs6000/altivec.md:1847 */
rtx
gen_absv16qi2 (operand0, operand1)
     rtx operand0;
     rtx operand1;
{
  return gen_rtx_PARALLEL (VOIDmode, gen_rtvec (3,
		gen_rtx_SET (VOIDmode,
	operand0,
	gen_rtx_ABS (V16QImode,
	operand1)),
		gen_rtx_CLOBBER (VOIDmode,
	gen_rtx_SCRATCH (V16QImode)),
		gen_rtx_CLOBBER (VOIDmode,
	gen_rtx_SCRATCH (V16QImode))));
}

/* /home/yu/gcc/gcc_powerpc/crosstool-0.42/build/powerpc-7450-linux-gnu/gcc-3.3.1-glibc-2.3.2/gcc-3.3.1/gcc/config/rs6000/altivec.md:1857 */
rtx
gen_absv8hi2 (operand0, operand1)
     rtx operand0;
     rtx operand1;
{
  return gen_rtx_PARALLEL (VOIDmode, gen_rtvec (3,
		gen_rtx_SET (VOIDmode,
	operand0,
	gen_rtx_ABS (V8HImode,
	operand1)),
		gen_rtx_CLOBBER (VOIDmode,
	gen_rtx_SCRATCH (V8HImode)),
		gen_rtx_CLOBBER (VOIDmode,
	gen_rtx_SCRATCH (V8HImode))));
}

/* /home/yu/gcc/gcc_powerpc/crosstool-0.42/build/powerpc-7450-linux-gnu/gcc-3.3.1-glibc-2.3.2/gcc-3.3.1/gcc/config/rs6000/altivec.md:1867 */
rtx
gen_absv4si2 (operand0, operand1)
     rtx operand0;
     rtx operand1;
{
  return gen_rtx_PARALLEL (VOIDmode, gen_rtvec (3,
		gen_rtx_SET (VOIDmode,
	operand0,
	gen_rtx_ABS (V4SImode,
	operand1)),
		gen_rtx_CLOBBER (VOIDmode,
	gen_rtx_SCRATCH (V4SImode)),
		gen_rtx_CLOBBER (VOIDmode,
	gen_rtx_SCRATCH (V4SImode))));
}

/* /home/yu/gcc/gcc_powerpc/crosstool-0.42/build/powerpc-7450-linux-gnu/gcc-3.3.1-glibc-2.3.2/gcc-3.3.1/gcc/config/rs6000/altivec.md:1877 */
rtx
gen_absv4sf2 (operand0, operand1)
     rtx operand0;
     rtx operand1;
{
  return gen_rtx_PARALLEL (VOIDmode, gen_rtvec (3,
		gen_rtx_SET (VOIDmode,
	operand0,
	gen_rtx_ABS (V4SFmode,
	operand1)),
		gen_rtx_CLOBBER (VOIDmode,
	gen_rtx_SCRATCH (V4SFmode)),
		gen_rtx_CLOBBER (VOIDmode,
	gen_rtx_SCRATCH (V4SFmode))));
}

/* /home/yu/gcc/gcc_powerpc/crosstool-0.42/build/powerpc-7450-linux-gnu/gcc-3.3.1-glibc-2.3.2/gcc-3.3.1/gcc/config/rs6000/altivec.md:1887 */
rtx
gen_altivec_abss_v16qi (operand0, operand1)
     rtx operand0;
     rtx operand1;
{
  return gen_rtx_PARALLEL (VOIDmode, gen_rtvec (3,
		gen_rtx_SET (VOIDmode,
	operand0,
	gen_rtx_UNSPEC (V16QImode,
	gen_rtvec (1,
		operand1),
	210)),
		gen_rtx_CLOBBER (VOIDmode,
	gen_rtx_SCRATCH (V16QImode)),
		gen_rtx_CLOBBER (VOIDmode,
	gen_rtx_SCRATCH (V16QImode))));
}

/* /home/yu/gcc/gcc_powerpc/crosstool-0.42/build/powerpc-7450-linux-gnu/gcc-3.3.1-glibc-2.3.2/gcc-3.3.1/gcc/config/rs6000/altivec.md:1897 */
rtx
gen_altivec_abss_v8hi (operand0, operand1)
     rtx operand0;
     rtx operand1;
{
  return gen_rtx_PARALLEL (VOIDmode, gen_rtvec (3,
		gen_rtx_SET (VOIDmode,
	operand0,
	gen_rtx_UNSPEC (V8HImode,
	gen_rtvec (1,
		operand1),
	211)),
		gen_rtx_CLOBBER (VOIDmode,
	gen_rtx_SCRATCH (V8HImode)),
		gen_rtx_CLOBBER (VOIDmode,
	gen_rtx_SCRATCH (V8HImode))));
}

/* /home/yu/gcc/gcc_powerpc/crosstool-0.42/build/powerpc-7450-linux-gnu/gcc-3.3.1-glibc-2.3.2/gcc-3.3.1/gcc/config/rs6000/altivec.md:1907 */
rtx
gen_altivec_abss_v4si (operand0, operand1)
     rtx operand0;
     rtx operand1;
{
  return gen_rtx_PARALLEL (VOIDmode, gen_rtvec (3,
		gen_rtx_SET (VOIDmode,
	operand0,
	gen_rtx_UNSPEC (V4SImode,
	gen_rtvec (1,
		operand1),
	212)),
		gen_rtx_CLOBBER (VOIDmode,
	gen_rtx_SCRATCH (V4SImode)),
		gen_rtx_CLOBBER (VOIDmode,
	gen_rtx_SCRATCH (V4SImode))));
}

/* /home/yu/gcc/gcc_powerpc/crosstool-0.42/build/powerpc-7450-linux-gnu/gcc-3.3.1-glibc-2.3.2/gcc-3.3.1/gcc/config/rs6000/rs6000.md:860 */
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

/* /home/yu/gcc/gcc_powerpc/crosstool-0.42/build/powerpc-7450-linux-gnu/gcc-3.3.1-glibc-2.3.2/gcc-3.3.1/gcc/config/rs6000/rs6000.md:887 */
extern rtx gen_split_761 PARAMS ((rtx *));
rtx
gen_split_761 (operands)
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
	gen_rtx_ZERO_EXTEND (DImode,
	operand1)));
  emit_insn (gen_rtx_SET (VOIDmode,
	operand0,
	gen_rtx_COMPARE (CCmode,
	copy_rtx (operand2),
	const0_rtx)));
  _val = get_insns ();
  end_sequence ();
  return _val;
}

/* /home/yu/gcc/gcc_powerpc/crosstool-0.42/build/powerpc-7450-linux-gnu/gcc-3.3.1-glibc-2.3.2/gcc-3.3.1/gcc/config/rs6000/rs6000.md:913 */
extern rtx gen_split_762 PARAMS ((rtx *));
rtx
gen_split_762 (operands)
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
	gen_rtx_ZERO_EXTEND (DImode,
	operand1)));
  emit_insn (gen_rtx_SET (VOIDmode,
	operand2,
	gen_rtx_COMPARE (CCmode,
	copy_rtx (operand0),
	const0_rtx)));
  _val = get_insns ();
  end_sequence ();
  return _val;
}

/* /home/yu/gcc/gcc_powerpc/crosstool-0.42/build/powerpc-7450-linux-gnu/gcc-3.3.1-glibc-2.3.2/gcc-3.3.1/gcc/config/rs6000/rs6000.md:945 */
extern rtx gen_split_763 PARAMS ((rtx *));
rtx
gen_split_763 (operands)
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
	gen_rtx_SIGN_EXTEND (DImode,
	operand1)));
  emit_insn (gen_rtx_SET (VOIDmode,
	operand0,
	gen_rtx_COMPARE (CCmode,
	copy_rtx (operand2),
	const0_rtx)));
  _val = get_insns ();
  end_sequence ();
  return _val;
}

/* /home/yu/gcc/gcc_powerpc/crosstool-0.42/build/powerpc-7450-linux-gnu/gcc-3.3.1-glibc-2.3.2/gcc-3.3.1/gcc/config/rs6000/rs6000.md:971 */
extern rtx gen_split_764 PARAMS ((rtx *));
rtx
gen_split_764 (operands)
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
	gen_rtx_SIGN_EXTEND (DImode,
	operand1)));
  emit_insn (gen_rtx_SET (VOIDmode,
	operand2,
	gen_rtx_COMPARE (CCmode,
	copy_rtx (operand0),
	const0_rtx)));
  _val = get_insns ();
  end_sequence ();
  return _val;
}

/* /home/yu/gcc/gcc_powerpc/crosstool-0.42/build/powerpc-7450-linux-gnu/gcc-3.3.1-glibc-2.3.2/gcc-3.3.1/gcc/config/rs6000/rs6000.md:985 */
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

/* /home/yu/gcc/gcc_powerpc/crosstool-0.42/build/powerpc-7450-linux-gnu/gcc-3.3.1-glibc-2.3.2/gcc-3.3.1/gcc/config/rs6000/rs6000.md:1012 */
extern rtx gen_split_766 PARAMS ((rtx *));
rtx
gen_split_766 (operands)
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
	gen_rtx_ZERO_EXTEND (DImode,
	operand1)));
  emit_insn (gen_rtx_SET (VOIDmode,
	operand0,
	gen_rtx_COMPARE (CCmode,
	copy_rtx (operand2),
	const0_rtx)));
  _val = get_insns ();
  end_sequence ();
  return _val;
}

/* /home/yu/gcc/gcc_powerpc/crosstool-0.42/build/powerpc-7450-linux-gnu/gcc-3.3.1-glibc-2.3.2/gcc-3.3.1/gcc/config/rs6000/rs6000.md:1038 */
extern rtx gen_split_767 PARAMS ((rtx *));
rtx
gen_split_767 (operands)
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
	gen_rtx_ZERO_EXTEND (DImode,
	operand1)));
  emit_insn (gen_rtx_SET (VOIDmode,
	operand2,
	gen_rtx_COMPARE (CCmode,
	copy_rtx (operand0),
	const0_rtx)));
  _val = get_insns ();
  end_sequence ();
  return _val;
}

/* /home/yu/gcc/gcc_powerpc/crosstool-0.42/build/powerpc-7450-linux-gnu/gcc-3.3.1-glibc-2.3.2/gcc-3.3.1/gcc/config/rs6000/rs6000.md:1052 */
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

/* /home/yu/gcc/gcc_powerpc/crosstool-0.42/build/powerpc-7450-linux-gnu/gcc-3.3.1-glibc-2.3.2/gcc-3.3.1/gcc/config/rs6000/rs6000.md:1079 */
extern rtx gen_split_769 PARAMS ((rtx *));
rtx
gen_split_769 (operands)
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
	gen_rtx_SIGN_EXTEND (DImode,
	operand1)));
  emit_insn (gen_rtx_SET (VOIDmode,
	operand0,
	gen_rtx_COMPARE (CCmode,
	copy_rtx (operand2),
	const0_rtx)));
  _val = get_insns ();
  end_sequence ();
  return _val;
}

/* /home/yu/gcc/gcc_powerpc/crosstool-0.42/build/powerpc-7450-linux-gnu/gcc-3.3.1-glibc-2.3.2/gcc-3.3.1/gcc/config/rs6000/rs6000.md:1105 */
extern rtx gen_split_770 PARAMS ((rtx *));
rtx
gen_split_770 (operands)
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
	gen_rtx_SIGN_EXTEND (DImode,
	operand1)));
  emit_insn (gen_rtx_SET (VOIDmode,
	operand2,
	gen_rtx_COMPARE (CCmode,
	copy_rtx (operand0),
	const0_rtx)));
  _val = get_insns ();
  end_sequence ();
  return _val;
}

/* /home/yu/gcc/gcc_powerpc/crosstool-0.42/build/powerpc-7450-linux-gnu/gcc-3.3.1-glibc-2.3.2/gcc-3.3.1/gcc/config/rs6000/rs6000.md:1119 */
rtx
gen_zero_extendsidi2 (operand0, operand1)
     rtx operand0;
     rtx operand1;
{
  return gen_rtx_SET (VOIDmode,
	operand0,
	gen_rtx_ZERO_EXTEND (DImode,
	operand1));
}

/* /home/yu/gcc/gcc_powerpc/crosstool-0.42/build/powerpc-7450-linux-gnu/gcc-3.3.1-glibc-2.3.2/gcc-3.3.1/gcc/config/rs6000/rs6000.md:1146 */
extern rtx gen_split_772 PARAMS ((rtx *));
rtx
gen_split_772 (operands)
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
	gen_rtx_ZERO_EXTEND (DImode,
	operand1)));
  emit_insn (gen_rtx_SET (VOIDmode,
	operand0,
	gen_rtx_COMPARE (CCmode,
	copy_rtx (operand2),
	const0_rtx)));
  _val = get_insns ();
  end_sequence ();
  return _val;
}

/* /home/yu/gcc/gcc_powerpc/crosstool-0.42/build/powerpc-7450-linux-gnu/gcc-3.3.1-glibc-2.3.2/gcc-3.3.1/gcc/config/rs6000/rs6000.md:1172 */
extern rtx gen_split_773 PARAMS ((rtx *));
rtx
gen_split_773 (operands)
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
	gen_rtx_ZERO_EXTEND (DImode,
	operand1)));
  emit_insn (gen_rtx_SET (VOIDmode,
	operand2,
	gen_rtx_COMPARE (CCmode,
	copy_rtx (operand0),
	const0_rtx)));
  _val = get_insns ();
  end_sequence ();
  return _val;
}

/* /home/yu/gcc/gcc_powerpc/crosstool-0.42/build/powerpc-7450-linux-gnu/gcc-3.3.1-glibc-2.3.2/gcc-3.3.1/gcc/config/rs6000/rs6000.md:1186 */
rtx
gen_extendsidi2 (operand0, operand1)
     rtx operand0;
     rtx operand1;
{
  return gen_rtx_SET (VOIDmode,
	operand0,
	gen_rtx_SIGN_EXTEND (DImode,
	operand1));
}

/* /home/yu/gcc/gcc_powerpc/crosstool-0.42/build/powerpc-7450-linux-gnu/gcc-3.3.1-glibc-2.3.2/gcc-3.3.1/gcc/config/rs6000/rs6000.md:1213 */
extern rtx gen_split_775 PARAMS ((rtx *));
rtx
gen_split_775 (operands)
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
	gen_rtx_SIGN_EXTEND (DImode,
	operand1)));
  emit_insn (gen_rtx_SET (VOIDmode,
	operand0,
	gen_rtx_COMPARE (CCmode,
	copy_rtx (operand2),
	const0_rtx)));
  _val = get_insns ();
  end_sequence ();
  return _val;
}

/* /home/yu/gcc/gcc_powerpc/crosstool-0.42/build/powerpc-7450-linux-gnu/gcc-3.3.1-glibc-2.3.2/gcc-3.3.1/gcc/config/rs6000/rs6000.md:1239 */
extern rtx gen_split_776 PARAMS ((rtx *));
rtx
gen_split_776 (operands)
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
	gen_rtx_SIGN_EXTEND (DImode,
	operand1)));
  emit_insn (gen_rtx_SET (VOIDmode,
	operand2,
	gen_rtx_COMPARE (CCmode,
	copy_rtx (operand0),
	const0_rtx)));
  _val = get_insns ();
  end_sequence ();
  return _val;
}

/* /home/yu/gcc/gcc_powerpc/crosstool-0.42/build/powerpc-7450-linux-gnu/gcc-3.3.1-glibc-2.3.2/gcc-3.3.1/gcc/config/rs6000/rs6000.md:1253 */
rtx
gen_zero_extendqisi2 (operand0, operand1)
     rtx operand0;
     rtx operand1;
{
  return gen_rtx_SET (VOIDmode,
	operand0,
	gen_rtx_ZERO_EXTEND (SImode,
	operand1));
}

/* /home/yu/gcc/gcc_powerpc/crosstool-0.42/build/powerpc-7450-linux-gnu/gcc-3.3.1-glibc-2.3.2/gcc-3.3.1/gcc/config/rs6000/rs6000.md:1280 */
extern rtx gen_split_778 PARAMS ((rtx *));
rtx
gen_split_778 (operands)
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
	gen_rtx_ZERO_EXTEND (SImode,
	operand1)));
  emit_insn (gen_rtx_SET (VOIDmode,
	operand0,
	gen_rtx_COMPARE (CCmode,
	copy_rtx (operand2),
	const0_rtx)));
  _val = get_insns ();
  end_sequence ();
  return _val;
}

/* /home/yu/gcc/gcc_powerpc/crosstool-0.42/build/powerpc-7450-linux-gnu/gcc-3.3.1-glibc-2.3.2/gcc-3.3.1/gcc/config/rs6000/rs6000.md:1306 */
extern rtx gen_split_779 PARAMS ((rtx *));
rtx
gen_split_779 (operands)
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
	gen_rtx_ZERO_EXTEND (SImode,
	operand1)));
  emit_insn (gen_rtx_SET (VOIDmode,
	operand2,
	gen_rtx_COMPARE (CCmode,
	copy_rtx (operand0),
	const0_rtx)));
  _val = get_insns ();
  end_sequence ();
  return _val;
}

/* /home/yu/gcc/gcc_powerpc/crosstool-0.42/build/powerpc-7450-linux-gnu/gcc-3.3.1-glibc-2.3.2/gcc-3.3.1/gcc/config/rs6000/rs6000.md:1320 */
rtx
gen_extendqisi2 (operand0, operand1)
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
  if (TARGET_POWERPC)
    emit_insn (gen_extendqisi2_ppc (operands[0], operands[1]));
  else if (TARGET_POWER)
    emit_insn (gen_extendqisi2_power (operands[0], operands[1]));
  else
    emit_insn (gen_extendqisi2_no_power (operands[0], operands[1]));
  DONE;
}
    operand0 = operands[0];
    operand1 = operands[1];
  }
  emit_insn (gen_rtx_USE (VOIDmode,
	operand0));
  emit_insn (gen_rtx_USE (VOIDmode,
	operand1));
  _val = get_insns ();
  end_sequence ();
  return _val;
}

/* /home/yu/gcc/gcc_powerpc/crosstool-0.42/build/powerpc-7450-linux-gnu/gcc-3.3.1-glibc-2.3.2/gcc-3.3.1/gcc/config/rs6000/rs6000.md:1353 */
extern rtx gen_split_781 PARAMS ((rtx *));
rtx
gen_split_781 (operands)
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
	gen_rtx_SIGN_EXTEND (SImode,
	operand1)));
  emit_insn (gen_rtx_SET (VOIDmode,
	operand0,
	gen_rtx_COMPARE (CCmode,
	copy_rtx (operand2),
	const0_rtx)));
  _val = get_insns ();
  end_sequence ();
  return _val;
}

/* /home/yu/gcc/gcc_powerpc/crosstool-0.42/build/powerpc-7450-linux-gnu/gcc-3.3.1-glibc-2.3.2/gcc-3.3.1/gcc/config/rs6000/rs6000.md:1379 */
extern rtx gen_split_782 PARAMS ((rtx *));
rtx
gen_split_782 (operands)
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
	gen_rtx_SIGN_EXTEND (SImode,
	operand1)));
  emit_insn (gen_rtx_SET (VOIDmode,
	operand2,
	gen_rtx_COMPARE (CCmode,
	copy_rtx (operand0),
	const0_rtx)));
  _val = get_insns ();
  end_sequence ();
  return _val;
}

/* /home/yu/gcc/gcc_powerpc/crosstool-0.42/build/powerpc-7450-linux-gnu/gcc-3.3.1-glibc-2.3.2/gcc-3.3.1/gcc/config/rs6000/rs6000.md:1393 */
rtx
gen_extendqisi2_power (operand0, operand1)
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

{ operands[1] = gen_lowpart (SImode, operands[1]);
  operands[2] = gen_reg_rtx (SImode); }
    operand0 = operands[0];
    operand1 = operands[1];
    operand2 = operands[2];
  }
  emit (gen_rtx_PARALLEL (VOIDmode,
	gen_rtvec (2,
		gen_rtx_SET (VOIDmode,
	operand2,
	gen_rtx_ASHIFT (SImode,
	operand1,
	GEN_INT (24LL))),
		gen_rtx_CLOBBER (VOIDmode,
	gen_rtx_SCRATCH (SImode)))));
  emit (gen_rtx_PARALLEL (VOIDmode,
	gen_rtvec (2,
		gen_rtx_SET (VOIDmode,
	operand0,
	gen_rtx_ASHIFTRT (SImode,
	operand2,
	GEN_INT (24LL))),
		gen_rtx_CLOBBER (VOIDmode,
	gen_rtx_SCRATCH (SImode)))));
  _val = get_insns ();
  end_sequence ();
  return _val;
}

/* /home/yu/gcc/gcc_powerpc/crosstool-0.42/build/powerpc-7450-linux-gnu/gcc-3.3.1-glibc-2.3.2/gcc-3.3.1/gcc/config/rs6000/rs6000.md:1407 */
rtx
gen_extendqisi2_no_power (operand0, operand1)
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

{ operands[1] = gen_lowpart (SImode, operands[1]);
  operands[2] = gen_reg_rtx (SImode); }
    operand0 = operands[0];
    operand1 = operands[1];
    operand2 = operands[2];
  }
  emit_insn (gen_rtx_SET (VOIDmode,
	operand2,
	gen_rtx_ASHIFT (SImode,
	operand1,
	GEN_INT (24LL))));
  emit_insn (gen_rtx_SET (VOIDmode,
	operand0,
	gen_rtx_ASHIFTRT (SImode,
	operand2,
	GEN_INT (24LL))));
  _val = get_insns ();
  end_sequence ();
  return _val;
}

/* /home/yu/gcc/gcc_powerpc/crosstool-0.42/build/powerpc-7450-linux-gnu/gcc-3.3.1-glibc-2.3.2/gcc-3.3.1/gcc/config/rs6000/rs6000.md:1419 */
rtx
gen_zero_extendqihi2 (operand0, operand1)
     rtx operand0;
     rtx operand1;
{
  return gen_rtx_SET (VOIDmode,
	operand0,
	gen_rtx_ZERO_EXTEND (HImode,
	operand1));
}

/* /home/yu/gcc/gcc_powerpc/crosstool-0.42/build/powerpc-7450-linux-gnu/gcc-3.3.1-glibc-2.3.2/gcc-3.3.1/gcc/config/rs6000/rs6000.md:1446 */
extern rtx gen_split_786 PARAMS ((rtx *));
rtx
gen_split_786 (operands)
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
	gen_rtx_ZERO_EXTEND (HImode,
	operand1)));
  emit_insn (gen_rtx_SET (VOIDmode,
	operand0,
	gen_rtx_COMPARE (CCmode,
	copy_rtx (operand2),
	const0_rtx)));
  _val = get_insns ();
  end_sequence ();
  return _val;
}

/* /home/yu/gcc/gcc_powerpc/crosstool-0.42/build/powerpc-7450-linux-gnu/gcc-3.3.1-glibc-2.3.2/gcc-3.3.1/gcc/config/rs6000/rs6000.md:1472 */
extern rtx gen_split_787 PARAMS ((rtx *));
rtx
gen_split_787 (operands)
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
	gen_rtx_ZERO_EXTEND (HImode,
	operand1)));
  emit_insn (gen_rtx_SET (VOIDmode,
	operand2,
	gen_rtx_COMPARE (CCmode,
	copy_rtx (operand0),
	const0_rtx)));
  _val = get_insns ();
  end_sequence ();
  return _val;
}

/* /home/yu/gcc/gcc_powerpc/crosstool-0.42/build/powerpc-7450-linux-gnu/gcc-3.3.1-glibc-2.3.2/gcc-3.3.1/gcc/config/rs6000/rs6000.md:1486 */
rtx
gen_extendqihi2 (operand0, operand1)
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
  if (TARGET_POWERPC)
    emit_insn (gen_extendqihi2_ppc (operands[0], operands[1]));
  else if (TARGET_POWER)
    emit_insn (gen_extendqihi2_power (operands[0], operands[1]));
  else
    emit_insn (gen_extendqihi2_no_power (operands[0], operands[1]));
  DONE;
}
    operand0 = operands[0];
    operand1 = operands[1];
  }
  emit_insn (gen_rtx_USE (VOIDmode,
	operand0));
  emit_insn (gen_rtx_USE (VOIDmode,
	operand1));
  _val = get_insns ();
  end_sequence ();
  return _val;
}

/* /home/yu/gcc/gcc_powerpc/crosstool-0.42/build/powerpc-7450-linux-gnu/gcc-3.3.1-glibc-2.3.2/gcc-3.3.1/gcc/config/rs6000/rs6000.md:1519 */
extern rtx gen_split_789 PARAMS ((rtx *));
rtx
gen_split_789 (operands)
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
	gen_rtx_SIGN_EXTEND (HImode,
	operand1)));
  emit_insn (gen_rtx_SET (VOIDmode,
	operand0,
	gen_rtx_COMPARE (CCmode,
	copy_rtx (operand2),
	const0_rtx)));
  _val = get_insns ();
  end_sequence ();
  return _val;
}

/* /home/yu/gcc/gcc_powerpc/crosstool-0.42/build/powerpc-7450-linux-gnu/gcc-3.3.1-glibc-2.3.2/gcc-3.3.1/gcc/config/rs6000/rs6000.md:1545 */
extern rtx gen_split_790 PARAMS ((rtx *));
rtx
gen_split_790 (operands)
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
	gen_rtx_SIGN_EXTEND (HImode,
	operand1)));
  emit_insn (gen_rtx_SET (VOIDmode,
	operand2,
	gen_rtx_COMPARE (CCmode,
	copy_rtx (operand0),
	const0_rtx)));
  _val = get_insns ();
  end_sequence ();
  return _val;
}

/* /home/yu/gcc/gcc_powerpc/crosstool-0.42/build/powerpc-7450-linux-gnu/gcc-3.3.1-glibc-2.3.2/gcc-3.3.1/gcc/config/rs6000/rs6000.md:1559 */
rtx
gen_extendqihi2_power (operand0, operand1)
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

{ operands[0] = gen_lowpart (SImode, operands[0]);
  operands[1] = gen_lowpart (SImode, operands[1]);
  operands[2] = gen_reg_rtx (SImode); }
    operand0 = operands[0];
    operand1 = operands[1];
    operand2 = operands[2];
  }
  emit (gen_rtx_PARALLEL (VOIDmode,
	gen_rtvec (2,
		gen_rtx_SET (VOIDmode,
	operand2,
	gen_rtx_ASHIFT (SImode,
	operand1,
	GEN_INT (24LL))),
		gen_rtx_CLOBBER (VOIDmode,
	gen_rtx_SCRATCH (SImode)))));
  emit (gen_rtx_PARALLEL (VOIDmode,
	gen_rtvec (2,
		gen_rtx_SET (VOIDmode,
	operand0,
	gen_rtx_ASHIFTRT (SImode,
	operand2,
	GEN_INT (24LL))),
		gen_rtx_CLOBBER (VOIDmode,
	gen_rtx_SCRATCH (SImode)))));
  _val = get_insns ();
  end_sequence ();
  return _val;
}

/* /home/yu/gcc/gcc_powerpc/crosstool-0.42/build/powerpc-7450-linux-gnu/gcc-3.3.1-glibc-2.3.2/gcc-3.3.1/gcc/config/rs6000/rs6000.md:1574 */
rtx
gen_extendqihi2_no_power (operand0, operand1)
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

{ operands[0] = gen_lowpart (SImode, operands[0]);
  operands[1] = gen_lowpart (SImode, operands[1]);
  operands[2] = gen_reg_rtx (SImode); }
    operand0 = operands[0];
    operand1 = operands[1];
    operand2 = operands[2];
  }
  emit_insn (gen_rtx_SET (VOIDmode,
	operand2,
	gen_rtx_ASHIFT (SImode,
	operand1,
	GEN_INT (24LL))));
  emit_insn (gen_rtx_SET (VOIDmode,
	operand0,
	gen_rtx_ASHIFTRT (SImode,
	operand2,
	GEN_INT (24LL))));
  _val = get_insns ();
  end_sequence ();
  return _val;
}

/* /home/yu/gcc/gcc_powerpc/crosstool-0.42/build/powerpc-7450-linux-gnu/gcc-3.3.1-glibc-2.3.2/gcc-3.3.1/gcc/config/rs6000/rs6000.md:1587 */
rtx
gen_zero_extendhisi2 (operand0, operand1)
     rtx operand0;
     rtx operand1;
{
  return gen_rtx_SET (VOIDmode,
	operand0,
	gen_rtx_ZERO_EXTEND (SImode,
	operand1));
}

/* /home/yu/gcc/gcc_powerpc/crosstool-0.42/build/powerpc-7450-linux-gnu/gcc-3.3.1-glibc-2.3.2/gcc-3.3.1/gcc/config/rs6000/rs6000.md:1614 */
extern rtx gen_split_794 PARAMS ((rtx *));
rtx
gen_split_794 (operands)
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
	gen_rtx_ZERO_EXTEND (SImode,
	operand1)));
  emit_insn (gen_rtx_SET (VOIDmode,
	operand0,
	gen_rtx_COMPARE (CCmode,
	copy_rtx (operand2),
	const0_rtx)));
  _val = get_insns ();
  end_sequence ();
  return _val;
}

/* /home/yu/gcc/gcc_powerpc/crosstool-0.42/build/powerpc-7450-linux-gnu/gcc-3.3.1-glibc-2.3.2/gcc-3.3.1/gcc/config/rs6000/rs6000.md:1640 */
extern rtx gen_split_795 PARAMS ((rtx *));
rtx
gen_split_795 (operands)
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
	gen_rtx_ZERO_EXTEND (SImode,
	operand1)));
  emit_insn (gen_rtx_SET (VOIDmode,
	operand2,
	gen_rtx_COMPARE (CCmode,
	copy_rtx (operand0),
	const0_rtx)));
  _val = get_insns ();
  end_sequence ();
  return _val;
}

/* /home/yu/gcc/gcc_powerpc/crosstool-0.42/build/powerpc-7450-linux-gnu/gcc-3.3.1-glibc-2.3.2/gcc-3.3.1/gcc/config/rs6000/rs6000.md:1654 */
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

/* /home/yu/gcc/gcc_powerpc/crosstool-0.42/build/powerpc-7450-linux-gnu/gcc-3.3.1-glibc-2.3.2/gcc-3.3.1/gcc/config/rs6000/rs6000.md:1681 */
extern rtx gen_split_797 PARAMS ((rtx *));
rtx
gen_split_797 (operands)
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
	gen_rtx_SIGN_EXTEND (SImode,
	operand1)));
  emit_insn (gen_rtx_SET (VOIDmode,
	operand0,
	gen_rtx_COMPARE (CCmode,
	copy_rtx (operand2),
	const0_rtx)));
  _val = get_insns ();
  end_sequence ();
  return _val;
}

/* /home/yu/gcc/gcc_powerpc/crosstool-0.42/build/powerpc-7450-linux-gnu/gcc-3.3.1-glibc-2.3.2/gcc-3.3.1/gcc/config/rs6000/rs6000.md:1707 */
extern rtx gen_split_798 PARAMS ((rtx *));
rtx
gen_split_798 (operands)
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
	gen_rtx_SIGN_EXTEND (SImode,
	operand1)));
  emit_insn (gen_rtx_SET (VOIDmode,
	operand2,
	gen_rtx_COMPARE (CCmode,
	copy_rtx (operand0),
	const0_rtx)));
  _val = get_insns ();
  end_sequence ();
  return _val;
}

/* /home/yu/gcc/gcc_powerpc/crosstool-0.42/build/powerpc-7450-linux-gnu/gcc-3.3.1-glibc-2.3.2/gcc-3.3.1/gcc/config/rs6000/rs6000.md:1725 */
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

{
  if (GET_CODE (operands[2]) == CONST_INT
		&& ! add_operand (operands[2], SImode))
    {
      rtx tmp = ((no_new_pseudos || rtx_equal_p (operands[0], operands[1]))
		 ? operands[0] : gen_reg_rtx (SImode));

      HOST_WIDE_INT val = INTVAL (operands[2]);
      HOST_WIDE_INT low = ((val & 0xffff) ^ 0x8000) - 0x8000;
      HOST_WIDE_INT rest = trunc_int_for_mode (val - low, SImode);

      /* The ordering here is important for the prolog expander.
	 When space is allocated from the stack, adding 'low' first may
	 produce a temporary deallocation (which would be bad).  */
      emit_insn (gen_addsi3 (tmp, operands[1], GEN_INT (rest)));
      emit_insn (gen_addsi3 (operands[0], tmp, GEN_INT (low)));
      DONE;
    }
}
    operand0 = operands[0];
    operand1 = operands[1];
    operand2 = operands[2];
  }
  emit_insn (gen_rtx_SET (VOIDmode,
	operand0,
	gen_rtx_PLUS (SImode,
	operand1,
	operand2)));
  _val = get_insns ();
  end_sequence ();
  return _val;
}

/* /home/yu/gcc/gcc_powerpc/crosstool-0.42/build/powerpc-7450-linux-gnu/gcc-3.3.1-glibc-2.3.2/gcc-3.3.1/gcc/config/rs6000/rs6000.md:1786 */
extern rtx gen_split_800 PARAMS ((rtx *));
rtx
gen_split_800 (operands)
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
	gen_rtx_PLUS (SImode,
	operand1,
	operand2)));
  emit_insn (gen_rtx_SET (VOIDmode,
	operand0,
	gen_rtx_COMPARE (CCmode,
	copy_rtx (operand3),
	const0_rtx)));
  _val = get_insns ();
  end_sequence ();
  return _val;
}

/* /home/yu/gcc/gcc_powerpc/crosstool-0.42/build/powerpc-7450-linux-gnu/gcc-3.3.1-glibc-2.3.2/gcc-3.3.1/gcc/config/rs6000/rs6000.md:1818 */
extern rtx gen_split_801 PARAMS ((rtx *));
rtx
gen_split_801 (operands)
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
	operand0,
	gen_rtx_PLUS (SImode,
	operand1,
	operand2)));
  emit_insn (gen_rtx_SET (VOIDmode,
	operand3,
	gen_rtx_COMPARE (CCmode,
	copy_rtx (operand0),
	const0_rtx)));
  _val = get_insns ();
  end_sequence ();
  return _val;
}

/* /home/yu/gcc/gcc_powerpc/crosstool-0.42/build/powerpc-7450-linux-gnu/gcc-3.3.1-glibc-2.3.2/gcc-3.3.1/gcc/config/rs6000/rs6000.md:1838 */
extern rtx gen_split_802 PARAMS ((rtx *));
rtx
gen_split_802 (operands)
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
  HOST_WIDE_INT val = INTVAL (operands[2]);
  HOST_WIDE_INT low = ((val & 0xffff) ^ 0x8000) - 0x8000;
  HOST_WIDE_INT rest = trunc_int_for_mode (val - low, SImode);

  operands[3] = GEN_INT (rest);
  operands[4] = GEN_INT (low);
}
  operand0 = operands[0];
  operand1 = operands[1];
  operand2 = operands[2];
  operand3 = operands[3];
  operand4 = operands[4];
  emit_insn (gen_rtx_SET (VOIDmode,
	operand0,
	gen_rtx_PLUS (SImode,
	operand1,
	operand3)));
  emit_insn (gen_rtx_SET (VOIDmode,
	copy_rtx (operand0),
	gen_rtx_PLUS (SImode,
	copy_rtx (operand0),
	operand4)));
  _val = get_insns ();
  end_sequence ();
  return _val;
}

/* /home/yu/gcc/gcc_powerpc/crosstool-0.42/build/powerpc-7450-linux-gnu/gcc-3.3.1-glibc-2.3.2/gcc-3.3.1/gcc/config/rs6000/rs6000.md:1873 */
extern rtx gen_split_803 PARAMS ((rtx *));
rtx
gen_split_803 (operands)
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
	gen_rtx_NOT (SImode,
	operand1)));
  emit_insn (gen_rtx_SET (VOIDmode,
	operand0,
	gen_rtx_COMPARE (CCmode,
	copy_rtx (operand2),
	const0_rtx)));
  _val = get_insns ();
  end_sequence ();
  return _val;
}

/* /home/yu/gcc/gcc_powerpc/crosstool-0.42/build/powerpc-7450-linux-gnu/gcc-3.3.1-glibc-2.3.2/gcc-3.3.1/gcc/config/rs6000/rs6000.md:1899 */
extern rtx gen_split_804 PARAMS ((rtx *));
rtx
gen_split_804 (operands)
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
	gen_rtx_NOT (SImode,
	operand1)));
  emit_insn (gen_rtx_SET (VOIDmode,
	operand2,
	gen_rtx_COMPARE (CCmode,
	copy_rtx (operand0),
	const0_rtx)));
  _val = get_insns ();
  end_sequence ();
  return _val;
}

/* /home/yu/gcc/gcc_powerpc/crosstool-0.42/build/powerpc-7450-linux-gnu/gcc-3.3.1-glibc-2.3.2/gcc-3.3.1/gcc/config/rs6000/rs6000.md:1955 */
extern rtx gen_split_805 PARAMS ((rtx *));
rtx
gen_split_805 (operands)
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
	gen_rtx_MINUS (SImode,
	operand1,
	operand2)));
  emit_insn (gen_rtx_SET (VOIDmode,
	operand0,
	gen_rtx_COMPARE (CCmode,
	copy_rtx (operand3),
	const0_rtx)));
  _val = get_insns ();
  end_sequence ();
  return _val;
}

/* /home/yu/gcc/gcc_powerpc/crosstool-0.42/build/powerpc-7450-linux-gnu/gcc-3.3.1-glibc-2.3.2/gcc-3.3.1/gcc/config/rs6000/rs6000.md:1999 */
extern rtx gen_split_806 PARAMS ((rtx *));
rtx
gen_split_806 (operands)
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
	operand0,
	gen_rtx_MINUS (SImode,
	operand1,
	operand2)));
  emit_insn (gen_rtx_SET (VOIDmode,
	operand3,
	gen_rtx_COMPARE (CCmode,
	copy_rtx (operand0),
	const0_rtx)));
  _val = get_insns ();
  end_sequence ();
  return _val;
}

/* /home/yu/gcc/gcc_powerpc/crosstool-0.42/build/powerpc-7450-linux-gnu/gcc-3.3.1-glibc-2.3.2/gcc-3.3.1/gcc/config/rs6000/rs6000.md:2016 */
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

{
  if (GET_CODE (operands[2]) == CONST_INT)
    {
      emit_insn (gen_addsi3 (operands[0], operands[1],
			     negate_rtx (SImode, operands[2])));
      DONE;
    }
}
    operand0 = operands[0];
    operand1 = operands[1];
    operand2 = operands[2];
  }
  emit_insn (gen_rtx_SET (VOIDmode,
	operand0,
	gen_rtx_MINUS (SImode,
	operand1,
	operand2)));
  _val = get_insns ();
  end_sequence ();
  return _val;
}

/* /home/yu/gcc/gcc_powerpc/crosstool-0.42/build/powerpc-7450-linux-gnu/gcc-3.3.1-glibc-2.3.2/gcc-3.3.1/gcc/config/rs6000/rs6000.md:2036 */
rtx
gen_sminsi3 (operand0, operand1, operand2)
     rtx operand0;
     rtx operand1;
     rtx operand2;
{
  rtx operand3;
  rtx _val = 0;
  start_sequence ();
  {
    rtx operands[4];
    operands[0] = operand0;
    operands[1] = operand1;
    operands[2] = operand2;

{
  if (TARGET_ISEL)
    {
      operands[2] = force_reg (SImode, operands[2]);
      rs6000_emit_minmax (operands[0], SMIN, operands[1], operands[2]);
      DONE;
    }

  operands[3] = gen_reg_rtx (SImode);
}
    operand0 = operands[0];
    operand1 = operands[1];
    operand2 = operands[2];
    operand3 = operands[3];
  }
  emit_insn (gen_rtx_SET (VOIDmode,
	operand3,
	gen_rtx_IF_THEN_ELSE (SImode,
	gen_rtx_GT (SImode,
	operand1,
	operand2),
	const0_rtx,
	gen_rtx_MINUS (SImode,
	operand2,
	operand1))));
  emit_insn (gen_rtx_SET (VOIDmode,
	operand0,
	gen_rtx_MINUS (SImode,
	operand2,
	operand3)));
  _val = get_insns ();
  end_sequence ();
  return _val;
}

/* /home/yu/gcc/gcc_powerpc/crosstool-0.42/build/powerpc-7450-linux-gnu/gcc-3.3.1-glibc-2.3.2/gcc-3.3.1/gcc/config/rs6000/rs6000.md:2057 */
extern rtx gen_split_809 PARAMS ((rtx *));
rtx
gen_split_809 (operands)
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
	gen_rtx_IF_THEN_ELSE (SImode,
	gen_rtx_GT (SImode,
	operand1,
	operand2),
	const0_rtx,
	gen_rtx_MINUS (SImode,
	copy_rtx (operand2),
	copy_rtx (operand1)))));
  emit_insn (gen_rtx_SET (VOIDmode,
	operand0,
	gen_rtx_MINUS (SImode,
	copy_rtx (operand2),
	copy_rtx (operand3))));
  _val = get_insns ();
  end_sequence ();
  return _val;
}

/* /home/yu/gcc/gcc_powerpc/crosstool-0.42/build/powerpc-7450-linux-gnu/gcc-3.3.1-glibc-2.3.2/gcc-3.3.1/gcc/config/rs6000/rs6000.md:2070 */
rtx
gen_smaxsi3 (operand0, operand1, operand2)
     rtx operand0;
     rtx operand1;
     rtx operand2;
{
  rtx operand3;
  rtx _val = 0;
  start_sequence ();
  {
    rtx operands[4];
    operands[0] = operand0;
    operands[1] = operand1;
    operands[2] = operand2;

{
  if (TARGET_ISEL)
    {
      operands[2] = force_reg (SImode, operands[2]);
      rs6000_emit_minmax (operands[0], SMAX, operands[1], operands[2]);
      DONE;
    }
  operands[3] = gen_reg_rtx (SImode);
}
    operand0 = operands[0];
    operand1 = operands[1];
    operand2 = operands[2];
    operand3 = operands[3];
  }
  emit_insn (gen_rtx_SET (VOIDmode,
	operand3,
	gen_rtx_IF_THEN_ELSE (SImode,
	gen_rtx_GT (SImode,
	operand1,
	operand2),
	const0_rtx,
	gen_rtx_MINUS (SImode,
	operand2,
	operand1))));
  emit_insn (gen_rtx_SET (VOIDmode,
	operand0,
	gen_rtx_PLUS (SImode,
	operand3,
	operand1)));
  _val = get_insns ();
  end_sequence ();
  return _val;
}

/* /home/yu/gcc/gcc_powerpc/crosstool-0.42/build/powerpc-7450-linux-gnu/gcc-3.3.1-glibc-2.3.2/gcc-3.3.1/gcc/config/rs6000/rs6000.md:2090 */
extern rtx gen_split_811 PARAMS ((rtx *));
rtx
gen_split_811 (operands)
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
	gen_rtx_IF_THEN_ELSE (SImode,
	gen_rtx_GT (SImode,
	operand1,
	operand2),
	const0_rtx,
	gen_rtx_MINUS (SImode,
	copy_rtx (operand2),
	copy_rtx (operand1)))));
  emit_insn (gen_rtx_SET (VOIDmode,
	operand0,
	gen_rtx_PLUS (SImode,
	copy_rtx (operand3),
	copy_rtx (operand1))));
  _val = get_insns ();
  end_sequence ();
  return _val;
}

/* /home/yu/gcc/gcc_powerpc/crosstool-0.42/build/powerpc-7450-linux-gnu/gcc-3.3.1-glibc-2.3.2/gcc-3.3.1/gcc/config/rs6000/rs6000.md:2103 */
rtx
gen_uminsi3 (operand0, operand1, operand2)
     rtx operand0;
     rtx operand1;
     rtx operand2;
{
  rtx operand3;
  rtx operand4;
  rtx operand5;
  rtx _val = 0;
  start_sequence ();
  {
    rtx operands[6];
    operands[0] = operand0;
    operands[1] = operand1;
    operands[2] = operand2;

{
  if (TARGET_ISEL)
    {
      rs6000_emit_minmax (operands[0], UMIN, operands[1], operands[2]);
      DONE;
    }
  operands[3] = gen_reg_rtx (SImode);
  operands[4] = gen_reg_rtx (SImode);
  operands[5] = GEN_INT (-2147483647 - 1);
}
    operand0 = operands[0];
    operand1 = operands[1];
    operand2 = operands[2];
    operand3 = operands[3];
    operand4 = operands[4];
    operand5 = operands[5];
  }
  emit_insn (gen_rtx_SET (VOIDmode,
	operand3,
	gen_rtx_XOR (SImode,
	operand1,
	operand5)));
  emit_insn (gen_rtx_SET (VOIDmode,
	operand4,
	gen_rtx_XOR (SImode,
	operand2,
	operand5)));
  emit_insn (gen_rtx_SET (VOIDmode,
	operand3,
	gen_rtx_IF_THEN_ELSE (SImode,
	gen_rtx_GT (VOIDmode,
	operand3,
	operand4),
	const0_rtx,
	gen_rtx_MINUS (SImode,
	operand4,
	operand3))));
  emit_insn (gen_rtx_SET (VOIDmode,
	operand0,
	gen_rtx_MINUS (SImode,
	operand2,
	operand3)));
  _val = get_insns ();
  end_sequence ();
  return _val;
}

/* /home/yu/gcc/gcc_powerpc/crosstool-0.42/build/powerpc-7450-linux-gnu/gcc-3.3.1-glibc-2.3.2/gcc-3.3.1/gcc/config/rs6000/rs6000.md:2126 */
rtx
gen_umaxsi3 (operand0, operand1, operand2)
     rtx operand0;
     rtx operand1;
     rtx operand2;
{
  rtx operand3;
  rtx operand4;
  rtx operand5;
  rtx _val = 0;
  start_sequence ();
  {
    rtx operands[6];
    operands[0] = operand0;
    operands[1] = operand1;
    operands[2] = operand2;

{
  if (TARGET_ISEL)
    {
      rs6000_emit_minmax (operands[0], UMAX, operands[1], operands[2]);
      DONE;
    }
  operands[3] = gen_reg_rtx (SImode);
  operands[4] = gen_reg_rtx (SImode);
  operands[5] = GEN_INT (-2147483647 - 1);
}
    operand0 = operands[0];
    operand1 = operands[1];
    operand2 = operands[2];
    operand3 = operands[3];
    operand4 = operands[4];
    operand5 = operands[5];
  }
  emit_insn (gen_rtx_SET (VOIDmode,
	operand3,
	gen_rtx_XOR (SImode,
	operand1,
	operand5)));
  emit_insn (gen_rtx_SET (VOIDmode,
	operand4,
	gen_rtx_XOR (SImode,
	operand2,
	operand5)));
  emit_insn (gen_rtx_SET (VOIDmode,
	operand3,
	gen_rtx_IF_THEN_ELSE (SImode,
	gen_rtx_GT (VOIDmode,
	operand3,
	operand4),
	const0_rtx,
	gen_rtx_MINUS (SImode,
	operand4,
	operand3))));
  emit_insn (gen_rtx_SET (VOIDmode,
	operand0,
	gen_rtx_PLUS (SImode,
	operand3,
	operand1)));
  _val = get_insns ();
  end_sequence ();
  return _val;
}

/* /home/yu/gcc/gcc_powerpc/crosstool-0.42/build/powerpc-7450-linux-gnu/gcc-3.3.1-glibc-2.3.2/gcc-3.3.1/gcc/config/rs6000/rs6000.md:2174 */
extern rtx gen_split_814 PARAMS ((rtx *));
rtx
gen_split_814 (operands)
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
	gen_rtx_IF_THEN_ELSE (SImode,
	gen_rtx_GT (VOIDmode,
	operand1,
	operand2),
	const0_rtx,
	gen_rtx_MINUS (SImode,
	copy_rtx (operand2),
	copy_rtx (operand1)))));
  emit_insn (gen_rtx_SET (VOIDmode,
	operand0,
	gen_rtx_COMPARE (CCmode,
	copy_rtx (operand3),
	const0_rtx)));
  _val = get_insns ();
  end_sequence ();
  return _val;
}

/* /home/yu/gcc/gcc_powerpc/crosstool-0.42/build/powerpc-7450-linux-gnu/gcc-3.3.1-glibc-2.3.2/gcc-3.3.1/gcc/config/rs6000/rs6000.md:2212 */
extern rtx gen_split_815 PARAMS ((rtx *));
rtx
gen_split_815 (operands)
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
	operand0,
	gen_rtx_IF_THEN_ELSE (SImode,
	gen_rtx_GT (VOIDmode,
	operand1,
	operand2),
	const0_rtx,
	gen_rtx_MINUS (SImode,
	copy_rtx (operand2),
	copy_rtx (operand1)))));
  emit_insn (gen_rtx_SET (VOIDmode,
	operand3,
	gen_rtx_COMPARE (CCmode,
	copy_rtx (operand0),
	const0_rtx)));
  _val = get_insns ();
  end_sequence ();
  return _val;
}

/* /home/yu/gcc/gcc_powerpc/crosstool-0.42/build/powerpc-7450-linux-gnu/gcc-3.3.1-glibc-2.3.2/gcc-3.3.1/gcc/config/rs6000/rs6000.md:2236 */
rtx
gen_abssi2 (operand0, operand1)
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
  if (TARGET_ISEL)
    {
      emit_insn (gen_abssi2_isel (operands[0], operands[1]));
      DONE;
    }
  else if (! TARGET_POWER)
    {
      emit_insn (gen_abssi2_nopower (operands[0], operands[1]));
      DONE;
    }
}
    operand0 = operands[0];
    operand1 = operands[1];
  }
  emit_insn (gen_rtx_SET (VOIDmode,
	operand0,
	gen_rtx_ABS (SImode,
	operand1)));
  _val = get_insns ();
  end_sequence ();
  return _val;
}

/* /home/yu/gcc/gcc_powerpc/crosstool-0.42/build/powerpc-7450-linux-gnu/gcc-3.3.1-glibc-2.3.2/gcc-3.3.1/gcc/config/rs6000/rs6000.md:2279 */
extern rtx gen_split_817 PARAMS ((rtx *));
rtx
gen_split_817 (operands)
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
	gen_rtx_ASHIFTRT (SImode,
	operand1,
	GEN_INT (31LL))));
  emit_insn (gen_rtx_SET (VOIDmode,
	operand0,
	gen_rtx_XOR (SImode,
	copy_rtx (operand2),
	copy_rtx (operand1))));
  emit_insn (gen_rtx_SET (VOIDmode,
	copy_rtx (operand0),
	gen_rtx_MINUS (SImode,
	copy_rtx (operand0),
	copy_rtx (operand2))));
  _val = get_insns ();
  end_sequence ();
  return _val;
}

/* /home/yu/gcc/gcc_powerpc/crosstool-0.42/build/powerpc-7450-linux-gnu/gcc-3.3.1-glibc-2.3.2/gcc-3.3.1/gcc/config/rs6000/rs6000.md:2297 */
extern rtx gen_split_818 PARAMS ((rtx *));
rtx
gen_split_818 (operands)
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
	gen_rtx_ASHIFTRT (SImode,
	operand1,
	GEN_INT (31LL))));
  emit_insn (gen_rtx_SET (VOIDmode,
	operand0,
	gen_rtx_XOR (SImode,
	copy_rtx (operand2),
	copy_rtx (operand1))));
  emit_insn (gen_rtx_SET (VOIDmode,
	copy_rtx (operand0),
	gen_rtx_MINUS (SImode,
	copy_rtx (operand2),
	copy_rtx (operand0))));
  _val = get_insns ();
  end_sequence ();
  return _val;
}

/* /home/yu/gcc/gcc_powerpc/crosstool-0.42/build/powerpc-7450-linux-gnu/gcc-3.3.1-glibc-2.3.2/gcc-3.3.1/gcc/config/rs6000/rs6000.md:2327 */
extern rtx gen_split_819 PARAMS ((rtx *));
rtx
gen_split_819 (operands)
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
	gen_rtx_NEG (SImode,
	operand1)));
  emit_insn (gen_rtx_SET (VOIDmode,
	operand0,
	gen_rtx_COMPARE (CCmode,
	copy_rtx (operand2),
	const0_rtx)));
  _val = get_insns ();
  end_sequence ();
  return _val;
}

/* /home/yu/gcc/gcc_powerpc/crosstool-0.42/build/powerpc-7450-linux-gnu/gcc-3.3.1-glibc-2.3.2/gcc-3.3.1/gcc/config/rs6000/rs6000.md:2353 */
extern rtx gen_split_820 PARAMS ((rtx *));
rtx
gen_split_820 (operands)
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
	gen_rtx_NEG (SImode,
	operand1)));
  emit_insn (gen_rtx_SET (VOIDmode,
	operand2,
	gen_rtx_COMPARE (CCmode,
	copy_rtx (operand0),
	const0_rtx)));
  _val = get_insns ();
  end_sequence ();
  return _val;
}

/* /home/yu/gcc/gcc_powerpc/crosstool-0.42/build/powerpc-7450-linux-gnu/gcc-3.3.1-glibc-2.3.2/gcc-3.3.1/gcc/config/rs6000/rs6000.md:2374 */
rtx
gen_mulsi3 (operand0, operand1, operand2)
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
  if (TARGET_POWER)
    emit_insn (gen_mulsi3_mq (operands[0], operands[1], operands[2]));
  else
    emit_insn (gen_mulsi3_no_mq (operands[0], operands[1], operands[2]));
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

/* /home/yu/gcc/gcc_powerpc/crosstool-0.42/build/powerpc-7450-linux-gnu/gcc-3.3.1-glibc-2.3.2/gcc-3.3.1/gcc/config/rs6000/rs6000.md:2433 */
extern rtx gen_split_822 PARAMS ((rtx *));
rtx
gen_split_822 (operands)
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
	gen_rtvec (2,
		gen_rtx_SET (VOIDmode,
	operand3,
	gen_rtx_MULT (SImode,
	operand1,
	operand2)),
		gen_rtx_CLOBBER (VOIDmode,
	operand4))));
  emit_insn (gen_rtx_SET (VOIDmode,
	operand0,
	gen_rtx_COMPARE (CCmode,
	copy_rtx (operand3),
	const0_rtx)));
  _val = get_insns ();
  end_sequence ();
  return _val;
}

/* /home/yu/gcc/gcc_powerpc/crosstool-0.42/build/powerpc-7450-linux-gnu/gcc-3.3.1-glibc-2.3.2/gcc-3.3.1/gcc/config/rs6000/rs6000.md:2462 */
extern rtx gen_split_823 PARAMS ((rtx *));
rtx
gen_split_823 (operands)
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
	gen_rtx_MULT (SImode,
	operand1,
	operand2)));
  emit_insn (gen_rtx_SET (VOIDmode,
	operand0,
	gen_rtx_COMPARE (CCmode,
	copy_rtx (operand3),
	const0_rtx)));
  _val = get_insns ();
  end_sequence ();
  return _val;
}

/* /home/yu/gcc/gcc_powerpc/crosstool-0.42/build/powerpc-7450-linux-gnu/gcc-3.3.1-glibc-2.3.2/gcc-3.3.1/gcc/config/rs6000/rs6000.md:2491 */
extern rtx gen_split_824 PARAMS ((rtx *));
rtx
gen_split_824 (operands)
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
	gen_rtvec (2,
		gen_rtx_SET (VOIDmode,
	operand0,
	gen_rtx_MULT (SImode,
	operand1,
	operand2)),
		gen_rtx_CLOBBER (VOIDmode,
	operand4))));
  emit_insn (gen_rtx_SET (VOIDmode,
	operand3,
	gen_rtx_COMPARE (CCmode,
	copy_rtx (operand0),
	const0_rtx)));
  _val = get_insns ();
  end_sequence ();
  return _val;
}

/* /home/yu/gcc/gcc_powerpc/crosstool-0.42/build/powerpc-7450-linux-gnu/gcc-3.3.1-glibc-2.3.2/gcc-3.3.1/gcc/config/rs6000/rs6000.md:2522 */
extern rtx gen_split_825 PARAMS ((rtx *));
rtx
gen_split_825 (operands)
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
	operand0,
	gen_rtx_MULT (SImode,
	operand1,
	operand2)));
  emit_insn (gen_rtx_SET (VOIDmode,
	operand3,
	gen_rtx_COMPARE (CCmode,
	copy_rtx (operand0),
	const0_rtx)));
  _val = get_insns ();
  end_sequence ();
  return _val;
}

/* /home/yu/gcc/gcc_powerpc/crosstool-0.42/build/powerpc-7450-linux-gnu/gcc-3.3.1-glibc-2.3.2/gcc-3.3.1/gcc/config/rs6000/rs6000.md:2541 */
rtx
gen_divmodsi4 (operand0, operand1, operand2, operand3)
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
  if (! TARGET_POWER && ! TARGET_POWERPC)
    {
      emit_move_insn (gen_rtx_REG (SImode, 3), operands[1]);
      emit_move_insn (gen_rtx_REG (SImode, 4), operands[2]);
      emit_insn (gen_divss_call ());
      emit_move_insn (operands[0], gen_rtx_REG (SImode, 3));
      emit_move_insn (operands[3], gen_rtx_REG (SImode, 4));
      DONE;
    }
}
    operand0 = operands[0];
    operand1 = operands[1];
    operand2 = operands[2];
    operand3 = operands[3];
  }
  emit (gen_rtx_PARALLEL (VOIDmode,
	gen_rtvec (2,
		gen_rtx_SET (VOIDmode,
	operand0,
	gen_rtx_DIV (SImode,
	operand1,
	operand2)),
		gen_rtx_SET (VOIDmode,
	operand3,
	gen_rtx_MOD (SImode,
	operand1,
	operand2)))));
  _val = get_insns ();
  end_sequence ();
  return _val;
}

/* /home/yu/gcc/gcc_powerpc/crosstool-0.42/build/powerpc-7450-linux-gnu/gcc-3.3.1-glibc-2.3.2/gcc-3.3.1/gcc/config/rs6000/rs6000.md:2571 */
rtx
gen_udivsi3 (operand0, operand1, operand2)
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
  if (! TARGET_POWER && ! TARGET_POWERPC)
    {
      emit_move_insn (gen_rtx_REG (SImode, 3), operands[1]);
      emit_move_insn (gen_rtx_REG (SImode, 4), operands[2]);
      emit_insn (gen_quous_call ());
      emit_move_insn (operands[0], gen_rtx_REG (SImode, 3));
      DONE;
    }
  else if (TARGET_POWER)
    {
      emit_insn (gen_udivsi3_mq (operands[0], operands[1], operands[2]));
      DONE;
    }
}
    operand0 = operands[0];
    operand1 = operands[1];
    operand2 = operands[2];
  }
  emit_insn (gen_rtx_SET (VOIDmode,
	operand0,
	gen_rtx_UDIV (SImode,
	operand1,
	operand2)));
  _val = get_insns ();
  end_sequence ();
  return _val;
}

/* /home/yu/gcc/gcc_powerpc/crosstool-0.42/build/powerpc-7450-linux-gnu/gcc-3.3.1-glibc-2.3.2/gcc-3.3.1/gcc/config/rs6000/rs6000.md:2614 */
rtx
gen_divsi3 (operand0, operand1, operand2)
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
  if (GET_CODE (operands[2]) == CONST_INT
      && INTVAL (operands[2]) > 0
      && exact_log2 (INTVAL (operands[2])) >= 0)
    ;
  else if (TARGET_POWERPC)
    {
      operands[2] = force_reg (SImode, operands[2]);
      if (TARGET_POWER)
	{
	  emit_insn (gen_divsi3_mq (operands[0], operands[1], operands[2]));
	  DONE;
	}
    }
  else if (TARGET_POWER)
    FAIL;
  else
    {
      emit_move_insn (gen_rtx_REG (SImode, 3), operands[1]);
      emit_move_insn (gen_rtx_REG (SImode, 4), operands[2]);
      emit_insn (gen_quoss_call ());
      emit_move_insn (operands[0], gen_rtx_REG (SImode, 3));
      DONE;
    }
}
    operand0 = operands[0];
    operand1 = operands[1];
    operand2 = operands[2];
  }
  emit_insn (gen_rtx_SET (VOIDmode,
	operand0,
	gen_rtx_DIV (SImode,
	operand1,
	operand2)));
  _val = get_insns ();
  end_sequence ();
  return _val;
}

/* /home/yu/gcc/gcc_powerpc/crosstool-0.42/build/powerpc-7450-linux-gnu/gcc-3.3.1-glibc-2.3.2/gcc-3.3.1/gcc/config/rs6000/rs6000.md:2663 */
rtx
gen_modsi3 (operand0, operand1, operand2)
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
  rtx temp1;
  rtx temp2;

  if (GET_CODE (operands[2]) != CONST_INT
      || INTVAL (operands[2]) <= 0
      || (i = exact_log2 (INTVAL (operands[2]))) < 0)
    FAIL;

  temp1 = gen_reg_rtx (SImode);
  temp2 = gen_reg_rtx (SImode);

  emit_insn (gen_divsi3 (temp1, operands[1], operands[2]));
  emit_insn (gen_ashlsi3 (temp2, temp1, GEN_INT (i)));
  emit_insn (gen_subsi3 (operands[0], operands[1], temp2));
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

/* /home/yu/gcc/gcc_powerpc/crosstool-0.42/build/powerpc-7450-linux-gnu/gcc-3.3.1-glibc-2.3.2/gcc-3.3.1/gcc/config/rs6000/rs6000.md:2709 */
extern rtx gen_split_830 PARAMS ((rtx *));
rtx
gen_split_830 (operands)
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
	gen_rtx_DIV (SImode,
	operand1,
	operand2)));
  emit_insn (gen_rtx_SET (VOIDmode,
	operand0,
	gen_rtx_COMPARE (CCmode,
	copy_rtx (operand3),
	const0_rtx)));
  _val = get_insns ();
  end_sequence ();
  return _val;
}

/* /home/yu/gcc/gcc_powerpc/crosstool-0.42/build/powerpc-7450-linux-gnu/gcc-3.3.1-glibc-2.3.2/gcc-3.3.1/gcc/config/rs6000/rs6000.md:2737 */
extern rtx gen_split_831 PARAMS ((rtx *));
rtx
gen_split_831 (operands)
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
	operand0,
	gen_rtx_DIV (SImode,
	operand1,
	operand2)));
  emit_insn (gen_rtx_SET (VOIDmode,
	operand3,
	gen_rtx_COMPARE (CCmode,
	copy_rtx (operand0),
	const0_rtx)));
  _val = get_insns ();
  end_sequence ();
  return _val;
}

/* /home/yu/gcc/gcc_powerpc/crosstool-0.42/build/powerpc-7450-linux-gnu/gcc-3.3.1-glibc-2.3.2/gcc-3.3.1/gcc/config/rs6000/rs6000.md:2775 */
rtx
gen_udivmodsi4_normal (operand0, operand1, operand2, operand3)
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

{ operands[4] = gen_reg_rtx (SImode); }
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
	gen_rtvec (2,
		gen_rtx_SET (VOIDmode,
	operand0,
	gen_rtx_UDIV (SImode,
	gen_rtx_PLUS (DImode,
	gen_rtx_ASHIFT (DImode,
	gen_rtx_ZERO_EXTEND (DImode,
	operand4),
	GEN_INT (32LL)),
	gen_rtx_ZERO_EXTEND (DImode,
	operand1)),
	operand2)),
		gen_rtx_SET (VOIDmode,
	operand3,
	gen_rtx_UMOD (SImode,
	gen_rtx_PLUS (DImode,
	gen_rtx_ASHIFT (DImode,
	gen_rtx_ZERO_EXTEND (DImode,
	operand4),
	GEN_INT (32LL)),
	gen_rtx_ZERO_EXTEND (DImode,
	operand1)),
	operand2)))));
  _val = get_insns ();
  end_sequence ();
  return _val;
}

/* /home/yu/gcc/gcc_powerpc/crosstool-0.42/build/powerpc-7450-linux-gnu/gcc-3.3.1-glibc-2.3.2/gcc-3.3.1/gcc/config/rs6000/rs6000.md:2792 */
rtx
gen_udivmodsi4_tests (operand0, operand1, operand2, operand3, operand4)
     rtx operand0;
     rtx operand1;
     rtx operand2;
     rtx operand3;
     rtx operand4;
{
  rtx operand5;
  rtx operand6;
  rtx _val = 0;
  start_sequence ();
  {
    rtx operands[7];
    operands[0] = operand0;
    operands[1] = operand1;
    operands[2] = operand2;
    operands[3] = operand3;
    operands[4] = operand4;

{ operands[5] = gen_reg_rtx (CCUNSmode);
  operands[6] = gen_reg_rtx (CCmode);
}
    operand0 = operands[0];
    operand1 = operands[1];
    operand2 = operands[2];
    operand3 = operands[3];
    operand4 = operands[4];
    operand5 = operands[5];
    operand6 = operands[6];
  }
  emit_insn (gen_rtx_SET (VOIDmode,
	operand0,
	const0_rtx));
  emit_insn (gen_rtx_SET (VOIDmode,
	operand3,
	operand1));
  emit_insn (gen_rtx_SET (VOIDmode,
	operand5,
	gen_rtx_COMPARE (CCUNSmode,
	operand1,
	operand2)));
  emit_jump_insn (gen_rtx_SET (VOIDmode,
	pc_rtx,
	gen_rtx_IF_THEN_ELSE (VOIDmode,
	gen_rtx_LTU (VOIDmode,
	operand5,
	const0_rtx),
	gen_rtx_LABEL_REF (VOIDmode,
	operand4),
	pc_rtx)));
  emit_insn (gen_rtx_SET (VOIDmode,
	operand0,
	const1_rtx));
  emit_insn (gen_rtx_SET (VOIDmode,
	operand3,
	gen_rtx_MINUS (SImode,
	operand1,
	operand2)));
  emit_insn (gen_rtx_SET (VOIDmode,
	operand6,
	gen_rtx_COMPARE (CCmode,
	operand2,
	const0_rtx)));
  emit_jump_insn (gen_rtx_SET (VOIDmode,
	pc_rtx,
	gen_rtx_IF_THEN_ELSE (VOIDmode,
	gen_rtx_LT (VOIDmode,
	operand6,
	const0_rtx),
	gen_rtx_LABEL_REF (VOIDmode,
	operand4),
	pc_rtx)));
  _val = get_insns ();
  end_sequence ();
  return _val;
}

/* /home/yu/gcc/gcc_powerpc/crosstool-0.42/build/powerpc-7450-linux-gnu/gcc-3.3.1-glibc-2.3.2/gcc-3.3.1/gcc/config/rs6000/rs6000.md:2809 */
rtx
gen_udivmodsi4 (operand0, operand1, operand2, operand3)
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
  rtx label = 0;

  if (! TARGET_POWER)
    {
      if (! TARGET_POWERPC)
        {
	  emit_move_insn (gen_rtx_REG (SImode, 3), operands[1]);
	  emit_move_insn (gen_rtx_REG (SImode, 4), operands[2]);
	  emit_insn (gen_divus_call ());
	  emit_move_insn (operands[0], gen_rtx_REG (SImode, 3));
	  emit_move_insn (operands[3], gen_rtx_REG (SImode, 4));
	  DONE;
        }
      else
        FAIL;
    }

  if (GET_CODE (operands[2]) != CONST_INT || INTVAL (operands[2]) < 0)
    {
      operands[2] = force_reg (SImode, operands[2]);
      label = gen_label_rtx ();
      emit (gen_udivmodsi4_tests (operands[0], operands[1], operands[2],
				  operands[3], label));
    }
  else
    operands[2] = force_reg (SImode, operands[2]);

  emit (gen_udivmodsi4_normal (operands[0], operands[1], operands[2],
			       operands[3]));
  if (label)
    emit_label (label);

  DONE;
}
    operand0 = operands[0];
    operand1 = operands[1];
    operand2 = operands[2];
    operand3 = operands[3];
  }
  emit (gen_rtx_PARALLEL (VOIDmode,
	gen_rtvec (2,
		gen_rtx_SET (VOIDmode,
	operand0,
	gen_rtx_UDIV (SImode,
	operand1,
	operand2)),
		gen_rtx_SET (VOIDmode,
	operand3,
	gen_rtx_UMOD (SImode,
	operand1,
	operand2)))));
  _val = get_insns ();
  end_sequence ();
  return _val;
}

/* /home/yu/gcc/gcc_powerpc/crosstool-0.42/build/powerpc-7450-linux-gnu/gcc-3.3.1-glibc-2.3.2/gcc-3.3.1/gcc/config/rs6000/rs6000.md:2984 */
extern rtx gen_split_835 PARAMS ((rtx *));
rtx
gen_split_835 (operands)
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
	gen_rtvec (2,
		gen_rtx_SET (VOIDmode,
	operand3,
	gen_rtx_AND (SImode,
	operand1,
	operand2)),
		gen_rtx_CLOBBER (VOIDmode,
	operand4))));
  emit_insn (gen_rtx_SET (VOIDmode,
	operand0,
	gen_rtx_COMPARE (CCmode,
	copy_rtx (operand3),
	const0_rtx)));
  _val = get_insns ();
  end_sequence ();
  return _val;
}

/* /home/yu/gcc/gcc_powerpc/crosstool-0.42/build/powerpc-7450-linux-gnu/gcc-3.3.1-glibc-2.3.2/gcc-3.3.1/gcc/config/rs6000/rs6000.md:3004 */
extern rtx gen_split_836 PARAMS ((rtx *));
rtx
gen_split_836 (operands)
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
	gen_rtvec (2,
		gen_rtx_SET (VOIDmode,
	operand3,
	gen_rtx_AND (SImode,
	operand1,
	operand2)),
		gen_rtx_CLOBBER (VOIDmode,
	operand4))));
  emit_insn (gen_rtx_SET (VOIDmode,
	operand0,
	gen_rtx_COMPARE (CCmode,
	copy_rtx (operand3),
	const0_rtx)));
  _val = get_insns ();
  end_sequence ();
  return _val;
}

/* /home/yu/gcc/gcc_powerpc/crosstool-0.42/build/powerpc-7450-linux-gnu/gcc-3.3.1-glibc-2.3.2/gcc-3.3.1/gcc/config/rs6000/rs6000.md:3065 */
extern rtx gen_split_837 PARAMS ((rtx *));
rtx
gen_split_837 (operands)
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
	gen_rtvec (2,
		gen_rtx_SET (VOIDmode,
	operand0,
	gen_rtx_AND (SImode,
	operand1,
	operand2)),
		gen_rtx_CLOBBER (VOIDmode,
	operand4))));
  emit_insn (gen_rtx_SET (VOIDmode,
	operand3,
	gen_rtx_COMPARE (CCmode,
	copy_rtx (operand0),
	const0_rtx)));
  _val = get_insns ();
  end_sequence ();
  return _val;
}

/* /home/yu/gcc/gcc_powerpc/crosstool-0.42/build/powerpc-7450-linux-gnu/gcc-3.3.1-glibc-2.3.2/gcc-3.3.1/gcc/config/rs6000/rs6000.md:3084 */
extern rtx gen_split_838 PARAMS ((rtx *));
rtx
gen_split_838 (operands)
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
	gen_rtvec (2,
		gen_rtx_SET (VOIDmode,
	operand0,
	gen_rtx_AND (SImode,
	operand1,
	operand2)),
		gen_rtx_CLOBBER (VOIDmode,
	operand4))));
  emit_insn (gen_rtx_SET (VOIDmode,
	operand3,
	gen_rtx_COMPARE (CCmode,
	copy_rtx (operand0),
	const0_rtx)));
  _val = get_insns ();
  end_sequence ();
  return _val;
}

/* /home/yu/gcc/gcc_powerpc/crosstool-0.42/build/powerpc-7450-linux-gnu/gcc-3.3.1-glibc-2.3.2/gcc-3.3.1/gcc/config/rs6000/rs6000.md:3105 */
extern rtx gen_split_839 PARAMS ((rtx *));
rtx
gen_split_839 (operands)
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

{
  int mb = extract_MB (operands[2]);
  int me = extract_ME (operands[2]);
  operands[3] = GEN_INT (me + 1);
  operands[5] = GEN_INT (32 - (me + 1));
  operands[4] = GEN_INT (~((HOST_WIDE_INT) -1 << (33 + me - mb)));
}
  operand0 = operands[0];
  operand1 = operands[1];
  operand2 = operands[2];
  operand3 = operands[3];
  operand4 = operands[4];
  operand5 = operands[5];
  emit_insn (gen_rtx_SET (VOIDmode,
	operand0,
	gen_rtx_AND (SImode,
	gen_rtx_ROTATE (SImode,
	operand1,
	operand3),
	operand4)));
  emit_insn (gen_rtx_SET (VOIDmode,
	copy_rtx (operand0),
	gen_rtx_ROTATE (SImode,
	copy_rtx (operand0),
	operand5)));
  _val = get_insns ();
  end_sequence ();
  return _val;
}

/* /home/yu/gcc/gcc_powerpc/crosstool-0.42/build/powerpc-7450-linux-gnu/gcc-3.3.1-glibc-2.3.2/gcc-3.3.1/gcc/config/rs6000/rs6000.md:3127 */
extern rtx gen_split_840 PARAMS ((rtx *));
rtx
gen_split_840 (operands)
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

{
  int mb = extract_MB (operands[1]);
  int me = extract_ME (operands[1]);
  operands[4] = GEN_INT (me + 1);
  operands[5] = GEN_INT (~((HOST_WIDE_INT) -1 << (33 + me - mb)));
}
  operand0 = operands[0];
  operand1 = operands[1];
  operand2 = operands[2];
  operand3 = operands[3];
  operand4 = operands[4];
  operand5 = operands[5];
  emit (gen_rtx_PARALLEL (VOIDmode,
	gen_rtvec (2,
		gen_rtx_SET (VOIDmode,
	operand2,
	gen_rtx_COMPARE (CCmode,
	gen_rtx_AND (SImode,
	gen_rtx_ROTATE (SImode,
	operand0,
	operand4),
	operand5),
	const0_rtx)),
		gen_rtx_CLOBBER (VOIDmode,
	operand3))));
  _val = get_insns ();
  end_sequence ();
  return _val;
}

/* /home/yu/gcc/gcc_powerpc/crosstool-0.42/build/powerpc-7450-linux-gnu/gcc-3.3.1-glibc-2.3.2/gcc-3.3.1/gcc/config/rs6000/rs6000.md:3151 */
extern rtx gen_split_841 PARAMS ((rtx *));
rtx
gen_split_841 (operands)
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
  int mb = extract_MB (operands[2]);
  int me = extract_ME (operands[2]);
  operands[4] = GEN_INT (me + 1);
  operands[6] = GEN_INT (32 - (me + 1));
  operands[5] = GEN_INT (~((HOST_WIDE_INT) -1 << (33 + me - mb)));
}
  operand0 = operands[0];
  operand1 = operands[1];
  operand2 = operands[2];
  operand3 = operands[3];
  operand4 = operands[4];
  operand5 = operands[5];
  operand6 = operands[6];
  emit (gen_rtx_PARALLEL (VOIDmode,
	gen_rtvec (2,
		gen_rtx_SET (VOIDmode,
	operand3,
	gen_rtx_COMPARE (CCmode,
	gen_rtx_AND (SImode,
	gen_rtx_ROTATE (SImode,
	operand1,
	operand4),
	operand5),
	const0_rtx)),
		gen_rtx_SET (VOIDmode,
	operand0,
	gen_rtx_AND (SImode,
	gen_rtx_ROTATE (SImode,
	copy_rtx (operand1),
	copy_rtx (operand4)),
	copy_rtx (operand5))))));
  emit_insn (gen_rtx_SET (VOIDmode,
	copy_rtx (operand0),
	gen_rtx_ROTATE (SImode,
	copy_rtx (operand0),
	operand6)));
  _val = get_insns ();
  end_sequence ();
  return _val;
}

/* /home/yu/gcc/gcc_powerpc/crosstool-0.42/build/powerpc-7450-linux-gnu/gcc-3.3.1-glibc-2.3.2/gcc-3.3.1/gcc/config/rs6000/rs6000.md:3182 */
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

{
  if (GET_CODE (operands[2]) == CONST_INT
      && ! logical_operand (operands[2], SImode))
    {
      HOST_WIDE_INT value = INTVAL (operands[2]);
      rtx tmp = ((no_new_pseudos || rtx_equal_p (operands[0], operands[1]))
		 ? operands[0] : gen_reg_rtx (SImode));

      emit_insn (gen_iorsi3 (tmp, operands[1],
			     GEN_INT (value & (~ (HOST_WIDE_INT) 0xffff))));
      emit_insn (gen_iorsi3 (operands[0], tmp, GEN_INT (value & 0xffff)));
      DONE;
    }
}
    operand0 = operands[0];
    operand1 = operands[1];
    operand2 = operands[2];
  }
  emit_insn (gen_rtx_SET (VOIDmode,
	operand0,
	gen_rtx_IOR (SImode,
	operand1,
	operand2)));
  _val = get_insns ();
  end_sequence ();
  return _val;
}

/* /home/yu/gcc/gcc_powerpc/crosstool-0.42/build/powerpc-7450-linux-gnu/gcc-3.3.1-glibc-2.3.2/gcc-3.3.1/gcc/config/rs6000/rs6000.md:3203 */
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

{
  if (GET_CODE (operands[2]) == CONST_INT
      && ! logical_operand (operands[2], SImode))
    {
      HOST_WIDE_INT value = INTVAL (operands[2]);
      rtx tmp = ((no_new_pseudos || rtx_equal_p (operands[0], operands[1]))
		 ? operands[0] : gen_reg_rtx (SImode));

      emit_insn (gen_xorsi3 (tmp, operands[1],
			     GEN_INT (value & (~ (HOST_WIDE_INT) 0xffff))));
      emit_insn (gen_xorsi3 (operands[0], tmp, GEN_INT (value & 0xffff)));
      DONE;
    }
}
    operand0 = operands[0];
    operand1 = operands[1];
    operand2 = operands[2];
  }
  emit_insn (gen_rtx_SET (VOIDmode,
	operand0,
	gen_rtx_XOR (SImode,
	operand1,
	operand2)));
  _val = get_insns ();
  end_sequence ();
  return _val;
}

/* /home/yu/gcc/gcc_powerpc/crosstool-0.42/build/powerpc-7450-linux-gnu/gcc-3.3.1-glibc-2.3.2/gcc-3.3.1/gcc/config/rs6000/rs6000.md:3249 */
extern rtx gen_split_844 PARAMS ((rtx *));
rtx
gen_split_844 (operands)
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
  emit_insn (gen_rtx_SET (VOIDmode,
	operand3,
	operand4));
  emit_insn (gen_rtx_SET (VOIDmode,
	operand0,
	gen_rtx_COMPARE (CCmode,
	copy_rtx (operand3),
	const0_rtx)));
  _val = get_insns ();
  end_sequence ();
  return _val;
}

/* /home/yu/gcc/gcc_powerpc/crosstool-0.42/build/powerpc-7450-linux-gnu/gcc-3.3.1-glibc-2.3.2/gcc-3.3.1/gcc/config/rs6000/rs6000.md:3278 */
extern rtx gen_split_845 PARAMS ((rtx *));
rtx
gen_split_845 (operands)
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
  emit_insn (gen_rtx_SET (VOIDmode,
	operand0,
	operand4));
  emit_insn (gen_rtx_SET (VOIDmode,
	operand3,
	gen_rtx_COMPARE (CCmode,
	copy_rtx (operand0),
	const0_rtx)));
  _val = get_insns ();
  end_sequence ();
  return _val;
}

/* /home/yu/gcc/gcc_powerpc/crosstool-0.42/build/powerpc-7450-linux-gnu/gcc-3.3.1-glibc-2.3.2/gcc-3.3.1/gcc/config/rs6000/rs6000.md:3296 */
extern rtx gen_split_846 PARAMS ((rtx *));
rtx
gen_split_846 (operands)
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

{
  rtx i;
  i = GEN_INT (INTVAL (operands[2]) & (~ (HOST_WIDE_INT) 0xffff));
  operands[4] = gen_rtx (GET_CODE (operands[3]), SImode,
			 operands[1], i);
  i = GEN_INT (INTVAL (operands[2]) & 0xffff);
  operands[5] = gen_rtx (GET_CODE (operands[3]), SImode,
			 operands[0], i);
}
  operand0 = operands[0];
  operand1 = operands[1];
  operand2 = operands[2];
  operand3 = operands[3];
  operand4 = operands[4];
  operand5 = operands[5];
  emit_insn (gen_rtx_SET (VOIDmode,
	operand0,
	operand4));
  emit_insn (gen_rtx_SET (VOIDmode,
	copy_rtx (operand0),
	operand5));
  _val = get_insns ();
  end_sequence ();
  return _val;
}

/* /home/yu/gcc/gcc_powerpc/crosstool-0.42/build/powerpc-7450-linux-gnu/gcc-3.3.1-glibc-2.3.2/gcc-3.3.1/gcc/config/rs6000/rs6000.md:3337 */
extern rtx gen_split_847 PARAMS ((rtx *));
rtx
gen_split_847 (operands)
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
  emit_insn (gen_rtx_SET (VOIDmode,
	operand3,
	operand4));
  emit_insn (gen_rtx_SET (VOIDmode,
	operand0,
	gen_rtx_COMPARE (CCmode,
	copy_rtx (operand3),
	const0_rtx)));
  _val = get_insns ();
  end_sequence ();
  return _val;
}

/* /home/yu/gcc/gcc_powerpc/crosstool-0.42/build/powerpc-7450-linux-gnu/gcc-3.3.1-glibc-2.3.2/gcc-3.3.1/gcc/config/rs6000/rs6000.md:3366 */
extern rtx gen_split_848 PARAMS ((rtx *));
rtx
gen_split_848 (operands)
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
  emit_insn (gen_rtx_SET (VOIDmode,
	operand0,
	operand4));
  emit_insn (gen_rtx_SET (VOIDmode,
	operand3,
	gen_rtx_COMPARE (CCmode,
	copy_rtx (operand0),
	const0_rtx)));
  _val = get_insns ();
  end_sequence ();
  return _val;
}

/* /home/yu/gcc/gcc_powerpc/crosstool-0.42/build/powerpc-7450-linux-gnu/gcc-3.3.1-glibc-2.3.2/gcc-3.3.1/gcc/config/rs6000/rs6000.md:3403 */
extern rtx gen_split_849 PARAMS ((rtx *));
rtx
gen_split_849 (operands)
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
  emit_insn (gen_rtx_SET (VOIDmode,
	operand3,
	operand4));
  emit_insn (gen_rtx_SET (VOIDmode,
	operand0,
	gen_rtx_COMPARE (CCmode,
	copy_rtx (operand3),
	const0_rtx)));
  _val = get_insns ();
  end_sequence ();
  return _val;
}

/* /home/yu/gcc/gcc_powerpc/crosstool-0.42/build/powerpc-7450-linux-gnu/gcc-3.3.1-glibc-2.3.2/gcc-3.3.1/gcc/config/rs6000/rs6000.md:3432 */
extern rtx gen_split_850 PARAMS ((rtx *));
rtx
gen_split_850 (operands)
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
  emit_insn (gen_rtx_SET (VOIDmode,
	operand0,
	operand4));
  emit_insn (gen_rtx_SET (VOIDmode,
	operand3,
	gen_rtx_COMPARE (CCmode,
	copy_rtx (operand0),
	const0_rtx)));
  _val = get_insns ();
  end_sequence ();
  return _val;
}

/* /home/yu/gcc/gcc_powerpc/crosstool-0.42/build/powerpc-7450-linux-gnu/gcc-3.3.1-glibc-2.3.2/gcc-3.3.1/gcc/config/rs6000/rs6000.md:3505 */
extern rtx gen_split_851 PARAMS ((rtx *));
rtx
gen_split_851 (operands)
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
  emit_insn (gen_rtx_SET (VOIDmode,
	operand0,
	gen_rtx_IOR (SImode,
	gen_rtx_AND (SImode,
	gen_rtx_NOT (SImode,
	operand2),
	operand1),
	gen_rtx_AND (SImode,
	copy_rtx (operand2),
	operand3))));
  emit_insn (gen_rtx_SET (VOIDmode,
	operand4,
	gen_rtx_COMPARE (CCmode,
	copy_rtx (operand0),
	const0_rtx)));
  _val = get_insns ();
  end_sequence ();
  return _val;
}

/* /home/yu/gcc/gcc_powerpc/crosstool-0.42/build/powerpc-7450-linux-gnu/gcc-3.3.1-glibc-2.3.2/gcc-3.3.1/gcc/config/rs6000/rs6000.md:3543 */
extern rtx gen_split_852 PARAMS ((rtx *));
rtx
gen_split_852 (operands)
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
  emit_insn (gen_rtx_SET (VOIDmode,
	operand0,
	gen_rtx_IOR (SImode,
	gen_rtx_AND (SImode,
	gen_rtx_NOT (SImode,
	operand2),
	operand1),
	gen_rtx_AND (SImode,
	operand3,
	copy_rtx (operand2)))));
  emit_insn (gen_rtx_SET (VOIDmode,
	operand4,
	gen_rtx_COMPARE (CCmode,
	copy_rtx (operand0),
	const0_rtx)));
  _val = get_insns ();
  end_sequence ();
  return _val;
}

/* /home/yu/gcc/gcc_powerpc/crosstool-0.42/build/powerpc-7450-linux-gnu/gcc-3.3.1-glibc-2.3.2/gcc-3.3.1/gcc/config/rs6000/rs6000.md:3581 */
extern rtx gen_split_853 PARAMS ((rtx *));
rtx
gen_split_853 (operands)
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
  emit_insn (gen_rtx_SET (VOIDmode,
	operand0,
	gen_rtx_IOR (SImode,
	gen_rtx_AND (SImode,
	operand2,
	operand3),
	gen_rtx_AND (SImode,
	gen_rtx_NOT (SImode,
	copy_rtx (operand2)),
	operand1))));
  emit_insn (gen_rtx_SET (VOIDmode,
	operand4,
	gen_rtx_COMPARE (CCmode,
	copy_rtx (operand0),
	const0_rtx)));
  _val = get_insns ();
  end_sequence ();
  return _val;
}

/* /home/yu/gcc/gcc_powerpc/crosstool-0.42/build/powerpc-7450-linux-gnu/gcc-3.3.1-glibc-2.3.2/gcc-3.3.1/gcc/config/rs6000/rs6000.md:3619 */
extern rtx gen_split_854 PARAMS ((rtx *));
rtx
gen_split_854 (operands)
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
  emit_insn (gen_rtx_SET (VOIDmode,
	operand0,
	gen_rtx_IOR (SImode,
	gen_rtx_AND (SImode,
	operand3,
	operand2),
	gen_rtx_AND (SImode,
	gen_rtx_NOT (SImode,
	copy_rtx (operand2)),
	operand1))));
  emit_insn (gen_rtx_SET (VOIDmode,
	operand4,
	gen_rtx_COMPARE (CCmode,
	copy_rtx (operand0),
	const0_rtx)));
  _val = get_insns ();
  end_sequence ();
  return _val;
}

/* /home/yu/gcc/gcc_powerpc/crosstool-0.42/build/powerpc-7450-linux-gnu/gcc-3.3.1-glibc-2.3.2/gcc-3.3.1/gcc/config/rs6000/rs6000.md:3641 */
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
  /* Do not handle 16/8 bit structures that fit in HI/QI modes directly, since
     the (SUBREG:SI (REG:HI xxx)) that is otherwise generated can confuse the
     compiler if the address of the structure is taken later.  */
  if (GET_CODE (operands[0]) == SUBREG
      && (GET_MODE_SIZE (GET_MODE (SUBREG_REG (operands[0]))) < UNITS_PER_WORD))
    FAIL;

  if (TARGET_POWERPC64 && GET_MODE (operands[0]) == DImode)
    emit_insn (gen_insvdi (operands[0], operands[1], operands[2], operands[3]));
  else
    emit_insn (gen_insvsi (operands[0], operands[1], operands[2], operands[3]));
  DONE;
}
    operand0 = operands[0];
    operand1 = operands[1];
    operand2 = operands[2];
    operand3 = operands[3];
  }
  emit_insn (gen_rtx_SET (VOIDmode,
	gen_rtx_ZERO_EXTRACT (VOIDmode,
	operand0,
	operand1,
	operand2),
	operand3));
  _val = get_insns ();
  end_sequence ();
  return _val;
}

/* /home/yu/gcc/gcc_powerpc/crosstool-0.42/build/powerpc-7450-linux-gnu/gcc-3.3.1-glibc-2.3.2/gcc-3.3.1/gcc/config/rs6000/rs6000.md:3769 */
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
  /* Do not handle 16/8 bit structures that fit in HI/QI modes directly, since
     the (SUBREG:SI (REG:HI xxx)) that is otherwise generated can confuse the
     compiler if the address of the structure is taken later.  */
  if (GET_CODE (operands[0]) == SUBREG
      && (GET_MODE_SIZE (GET_MODE (SUBREG_REG (operands[0]))) < UNITS_PER_WORD))
    FAIL;

  if (TARGET_POWERPC64 && GET_MODE (operands[1]) == DImode)
    emit_insn (gen_extzvdi (operands[0], operands[1], operands[2], operands[3]));
  else
    emit_insn (gen_extzvsi (operands[0], operands[1], operands[2], operands[3]));
  DONE;
}
    operand0 = operands[0];
    operand1 = operands[1];
    operand2 = operands[2];
    operand3 = operands[3];
  }
  emit_insn (gen_rtx_SET (VOIDmode,
	operand0,
	gen_rtx_ZERO_EXTRACT (VOIDmode,
	operand1,
	operand2,
	operand3)));
  _val = get_insns ();
  end_sequence ();
  return _val;
}

/* /home/yu/gcc/gcc_powerpc/crosstool-0.42/build/powerpc-7450-linux-gnu/gcc-3.3.1-glibc-2.3.2/gcc-3.3.1/gcc/config/rs6000/rs6000.md:3851 */
extern rtx gen_split_857 PARAMS ((rtx *));
rtx
gen_split_857 (operands)
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
  emit_insn (gen_rtx_SET (VOIDmode,
	operand4,
	gen_rtx_ZERO_EXTRACT (SImode,
	operand1,
	operand2,
	operand3)));
  emit_insn (gen_rtx_SET (VOIDmode,
	operand0,
	gen_rtx_COMPARE (CCmode,
	copy_rtx (operand4),
	const0_rtx)));
  _val = get_insns ();
  end_sequence ();
  return _val;
}

/* /home/yu/gcc/gcc_powerpc/crosstool-0.42/build/powerpc-7450-linux-gnu/gcc-3.3.1-glibc-2.3.2/gcc-3.3.1/gcc/config/rs6000/rs6000.md:3902 */
extern rtx gen_split_858 PARAMS ((rtx *));
rtx
gen_split_858 (operands)
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
  emit_insn (gen_rtx_SET (VOIDmode,
	operand0,
	gen_rtx_ZERO_EXTRACT (SImode,
	operand1,
	operand2,
	operand3)));
  emit_insn (gen_rtx_SET (VOIDmode,
	operand4,
	gen_rtx_COMPARE (CCmode,
	copy_rtx (operand0),
	const0_rtx)));
  _val = get_insns ();
  end_sequence ();
  return _val;
}

/* /home/yu/gcc/gcc_powerpc/crosstool-0.42/build/powerpc-7450-linux-gnu/gcc-3.3.1-glibc-2.3.2/gcc-3.3.1/gcc/config/rs6000/rs6000.md:4000 */
extern rtx gen_split_859 PARAMS ((rtx *));
rtx
gen_split_859 (operands)
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
	gen_rtx_ROTATE (SImode,
	operand1,
	operand2)));
  emit_insn (gen_rtx_SET (VOIDmode,
	operand0,
	gen_rtx_COMPARE (CCmode,
	copy_rtx (operand3),
	const0_rtx)));
  _val = get_insns ();
  end_sequence ();
  return _val;
}

/* /home/yu/gcc/gcc_powerpc/crosstool-0.42/build/powerpc-7450-linux-gnu/gcc-3.3.1-glibc-2.3.2/gcc-3.3.1/gcc/config/rs6000/rs6000.md:4028 */
extern rtx gen_split_860 PARAMS ((rtx *));
rtx
gen_split_860 (operands)
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
	operand0,
	gen_rtx_ROTATE (SImode,
	operand1,
	operand2)));
  emit_insn (gen_rtx_SET (VOIDmode,
	operand3,
	gen_rtx_COMPARE (CCmode,
	copy_rtx (operand0),
	const0_rtx)));
  _val = get_insns ();
  end_sequence ();
  return _val;
}

/* /home/yu/gcc/gcc_powerpc/crosstool-0.42/build/powerpc-7450-linux-gnu/gcc-3.3.1-glibc-2.3.2/gcc-3.3.1/gcc/config/rs6000/rs6000.md:4066 */
extern rtx gen_split_861 PARAMS ((rtx *));
rtx
gen_split_861 (operands)
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
  emit_insn (gen_rtx_SET (VOIDmode,
	operand4,
	gen_rtx_AND (SImode,
	gen_rtx_ROTATE (SImode,
	operand1,
	operand2),
	operand3)));
  emit_insn (gen_rtx_SET (VOIDmode,
	operand0,
	gen_rtx_COMPARE (CCmode,
	copy_rtx (operand4),
	const0_rtx)));
  _val = get_insns ();
  end_sequence ();
  return _val;
}

/* /home/yu/gcc/gcc_powerpc/crosstool-0.42/build/powerpc-7450-linux-gnu/gcc-3.3.1-glibc-2.3.2/gcc-3.3.1/gcc/config/rs6000/rs6000.md:4100 */
extern rtx gen_split_862 PARAMS ((rtx *));
rtx
gen_split_862 (operands)
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
  emit_insn (gen_rtx_SET (VOIDmode,
	operand0,
	gen_rtx_AND (SImode,
	gen_rtx_ROTATE (SImode,
	operand1,
	operand2),
	operand3)));
  emit_insn (gen_rtx_SET (VOIDmode,
	operand4,
	gen_rtx_COMPARE (CCmode,
	copy_rtx (operand0),
	const0_rtx)));
  _val = get_insns ();
  end_sequence ();
  return _val;
}

/* /home/yu/gcc/gcc_powerpc/crosstool-0.42/build/powerpc-7450-linux-gnu/gcc-3.3.1-glibc-2.3.2/gcc-3.3.1/gcc/config/rs6000/rs6000.md:4141 */
extern rtx gen_split_863 PARAMS ((rtx *));
rtx
gen_split_863 (operands)
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
	gen_rtx_ZERO_EXTEND (SImode,
	gen_rtx_SUBREG (QImode,
	gen_rtx_ROTATE (SImode,
	operand1,
	operand2),
	0))));
  emit_insn (gen_rtx_SET (VOIDmode,
	operand0,
	gen_rtx_COMPARE (CCmode,
	copy_rtx (operand3),
	const0_rtx)));
  _val = get_insns ();
  end_sequence ();
  return _val;
}

/* /home/yu/gcc/gcc_powerpc/crosstool-0.42/build/powerpc-7450-linux-gnu/gcc-3.3.1-glibc-2.3.2/gcc-3.3.1/gcc/config/rs6000/rs6000.md:4175 */
extern rtx gen_split_864 PARAMS ((rtx *));
rtx
gen_split_864 (operands)
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
	operand0,
	gen_rtx_ZERO_EXTEND (SImode,
	gen_rtx_SUBREG (QImode,
	gen_rtx_ROTATE (SImode,
	operand1,
	operand2),
	0))));
  emit_insn (gen_rtx_SET (VOIDmode,
	operand3,
	gen_rtx_COMPARE (CCmode,
	copy_rtx (operand0),
	const0_rtx)));
  _val = get_insns ();
  end_sequence ();
  return _val;
}

/* /home/yu/gcc/gcc_powerpc/crosstool-0.42/build/powerpc-7450-linux-gnu/gcc-3.3.1-glibc-2.3.2/gcc-3.3.1/gcc/config/rs6000/rs6000.md:4216 */
extern rtx gen_split_865 PARAMS ((rtx *));
rtx
gen_split_865 (operands)
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
	gen_rtx_ZERO_EXTEND (SImode,
	gen_rtx_SUBREG (HImode,
	gen_rtx_ROTATE (SImode,
	operand1,
	operand2),
	0))));
  emit_insn (gen_rtx_SET (VOIDmode,
	operand0,
	gen_rtx_COMPARE (CCmode,
	copy_rtx (operand3),
	const0_rtx)));
  _val = get_insns ();
  end_sequence ();
  return _val;
}

/* /home/yu/gcc/gcc_powerpc/crosstool-0.42/build/powerpc-7450-linux-gnu/gcc-3.3.1-glibc-2.3.2/gcc-3.3.1/gcc/config/rs6000/rs6000.md:4250 */
extern rtx gen_split_866 PARAMS ((rtx *));
rtx
gen_split_866 (operands)
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
	operand0,
	gen_rtx_ZERO_EXTEND (SImode,
	gen_rtx_SUBREG (HImode,
	gen_rtx_ROTATE (SImode,
	operand1,
	operand2),
	0))));
  emit_insn (gen_rtx_SET (VOIDmode,
	operand3,
	gen_rtx_COMPARE (CCmode,
	copy_rtx (operand0),
	const0_rtx)));
  _val = get_insns ();
  end_sequence ();
  return _val;
}

/* /home/yu/gcc/gcc_powerpc/crosstool-0.42/build/powerpc-7450-linux-gnu/gcc-3.3.1-glibc-2.3.2/gcc-3.3.1/gcc/config/rs6000/rs6000.md:4270 */
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

{
  if (TARGET_POWER)
    emit_insn (gen_ashlsi3_power (operands[0], operands[1], operands[2]));
  else
    emit_insn (gen_ashlsi3_no_power (operands[0], operands[1], operands[2]));
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

/* /home/yu/gcc/gcc_powerpc/crosstool-0.42/build/powerpc-7450-linux-gnu/gcc-3.3.1-glibc-2.3.2/gcc-3.3.1/gcc/config/rs6000/rs6000.md:4317 */
extern rtx gen_split_868 PARAMS ((rtx *));
rtx
gen_split_868 (operands)
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
	gen_rtvec (2,
		gen_rtx_SET (VOIDmode,
	operand3,
	gen_rtx_ASHIFT (SImode,
	operand1,
	operand2)),
		gen_rtx_CLOBBER (VOIDmode,
	operand4))));
  emit_insn (gen_rtx_SET (VOIDmode,
	operand0,
	gen_rtx_COMPARE (CCmode,
	copy_rtx (operand3),
	const0_rtx)));
  _val = get_insns ();
  end_sequence ();
  return _val;
}

/* /home/yu/gcc/gcc_powerpc/crosstool-0.42/build/powerpc-7450-linux-gnu/gcc-3.3.1-glibc-2.3.2/gcc-3.3.1/gcc/config/rs6000/rs6000.md:4346 */
extern rtx gen_split_869 PARAMS ((rtx *));
rtx
gen_split_869 (operands)
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
	gen_rtx_ASHIFT (SImode,
	operand1,
	operand2)));
  emit_insn (gen_rtx_SET (VOIDmode,
	operand0,
	gen_rtx_COMPARE (CCmode,
	copy_rtx (operand3),
	const0_rtx)));
  _val = get_insns ();
  end_sequence ();
  return _val;
}

/* /home/yu/gcc/gcc_powerpc/crosstool-0.42/build/powerpc-7450-linux-gnu/gcc-3.3.1-glibc-2.3.2/gcc-3.3.1/gcc/config/rs6000/rs6000.md:4377 */
extern rtx gen_split_870 PARAMS ((rtx *));
rtx
gen_split_870 (operands)
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
	gen_rtvec (2,
		gen_rtx_SET (VOIDmode,
	operand0,
	gen_rtx_ASHIFT (SImode,
	operand1,
	operand2)),
		gen_rtx_CLOBBER (VOIDmode,
	operand4))));
  emit_insn (gen_rtx_SET (VOIDmode,
	operand3,
	gen_rtx_COMPARE (CCmode,
	copy_rtx (operand0),
	const0_rtx)));
  _val = get_insns ();
  end_sequence ();
  return _val;
}

/* /home/yu/gcc/gcc_powerpc/crosstool-0.42/build/powerpc-7450-linux-gnu/gcc-3.3.1-glibc-2.3.2/gcc-3.3.1/gcc/config/rs6000/rs6000.md:4408 */
extern rtx gen_split_871 PARAMS ((rtx *));
rtx
gen_split_871 (operands)
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
	operand0,
	gen_rtx_ASHIFT (SImode,
	operand1,
	operand2)));
  emit_insn (gen_rtx_SET (VOIDmode,
	operand3,
	gen_rtx_COMPARE (CCmode,
	copy_rtx (operand0),
	const0_rtx)));
  _val = get_insns ();
  end_sequence ();
  return _val;
}

/* /home/yu/gcc/gcc_powerpc/crosstool-0.42/build/powerpc-7450-linux-gnu/gcc-3.3.1-glibc-2.3.2/gcc-3.3.1/gcc/config/rs6000/rs6000.md:4446 */
extern rtx gen_split_872 PARAMS ((rtx *));
rtx
gen_split_872 (operands)
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
  emit_insn (gen_rtx_SET (VOIDmode,
	operand4,
	gen_rtx_AND (SImode,
	gen_rtx_ASHIFT (SImode,
	operand1,
	operand2),
	operand3)));
  emit_insn (gen_rtx_SET (VOIDmode,
	operand0,
	gen_rtx_COMPARE (CCmode,
	copy_rtx (operand4),
	const0_rtx)));
  _val = get_insns ();
  end_sequence ();
  return _val;
}

/* /home/yu/gcc/gcc_powerpc/crosstool-0.42/build/powerpc-7450-linux-gnu/gcc-3.3.1-glibc-2.3.2/gcc-3.3.1/gcc/config/rs6000/rs6000.md:4479 */
extern rtx gen_split_873 PARAMS ((rtx *));
rtx
gen_split_873 (operands)
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
  emit_insn (gen_rtx_SET (VOIDmode,
	operand0,
	gen_rtx_AND (SImode,
	gen_rtx_ASHIFT (SImode,
	operand1,
	operand2),
	operand3)));
  emit_insn (gen_rtx_SET (VOIDmode,
	operand4,
	gen_rtx_COMPARE (CCmode,
	copy_rtx (operand0),
	const0_rtx)));
  _val = get_insns ();
  end_sequence ();
  return _val;
}

/* /home/yu/gcc/gcc_powerpc/crosstool-0.42/build/powerpc-7450-linux-gnu/gcc-3.3.1-glibc-2.3.2/gcc-3.3.1/gcc/config/rs6000/rs6000.md:4498 */
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

{
  if (TARGET_POWER)
    emit_insn (gen_lshrsi3_power (operands[0], operands[1], operands[2]));
  else
    emit_insn (gen_lshrsi3_no_power (operands[0], operands[1], operands[2]));
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

/* /home/yu/gcc/gcc_powerpc/crosstool-0.42/build/powerpc-7450-linux-gnu/gcc-3.3.1-glibc-2.3.2/gcc-3.3.1/gcc/config/rs6000/rs6000.md:4550 */
extern rtx gen_split_875 PARAMS ((rtx *));
rtx
gen_split_875 (operands)
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
	gen_rtvec (2,
		gen_rtx_SET (VOIDmode,
	operand3,
	gen_rtx_LSHIFTRT (SImode,
	operand1,
	operand2)),
		gen_rtx_CLOBBER (VOIDmode,
	operand4))));
  emit_insn (gen_rtx_SET (VOIDmode,
	operand0,
	gen_rtx_COMPARE (CCmode,
	copy_rtx (operand3),
	const0_rtx)));
  _val = get_insns ();
  end_sequence ();
  return _val;
}

/* /home/yu/gcc/gcc_powerpc/crosstool-0.42/build/powerpc-7450-linux-gnu/gcc-3.3.1-glibc-2.3.2/gcc-3.3.1/gcc/config/rs6000/rs6000.md:4581 */
extern rtx gen_split_876 PARAMS ((rtx *));
rtx
gen_split_876 (operands)
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
	gen_rtx_LSHIFTRT (SImode,
	operand1,
	operand2)));
  emit_insn (gen_rtx_SET (VOIDmode,
	operand0,
	gen_rtx_COMPARE (CCmode,
	copy_rtx (operand3),
	const0_rtx)));
  _val = get_insns ();
  end_sequence ();
  return _val;
}

/* /home/yu/gcc/gcc_powerpc/crosstool-0.42/build/powerpc-7450-linux-gnu/gcc-3.3.1-glibc-2.3.2/gcc-3.3.1/gcc/config/rs6000/rs6000.md:4614 */
extern rtx gen_split_877 PARAMS ((rtx *));
rtx
gen_split_877 (operands)
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
	gen_rtvec (2,
		gen_rtx_SET (VOIDmode,
	operand0,
	gen_rtx_LSHIFTRT (SImode,
	operand1,
	operand2)),
		gen_rtx_CLOBBER (VOIDmode,
	operand4))));
  emit_insn (gen_rtx_SET (VOIDmode,
	operand3,
	gen_rtx_COMPARE (CCmode,
	copy_rtx (operand0),
	const0_rtx)));
  _val = get_insns ();
  end_sequence ();
  return _val;
}

/* /home/yu/gcc/gcc_powerpc/crosstool-0.42/build/powerpc-7450-linux-gnu/gcc-3.3.1-glibc-2.3.2/gcc-3.3.1/gcc/config/rs6000/rs6000.md:4647 */
extern rtx gen_split_878 PARAMS ((rtx *));
rtx
gen_split_878 (operands)
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
	operand0,
	gen_rtx_LSHIFTRT (SImode,
	operand1,
	operand2)));
  emit_insn (gen_rtx_SET (VOIDmode,
	operand3,
	gen_rtx_COMPARE (CCmode,
	copy_rtx (operand0),
	const0_rtx)));
  _val = get_insns ();
  end_sequence ();
  return _val;
}

/* /home/yu/gcc/gcc_powerpc/crosstool-0.42/build/powerpc-7450-linux-gnu/gcc-3.3.1-glibc-2.3.2/gcc-3.3.1/gcc/config/rs6000/rs6000.md:4685 */
extern rtx gen_split_879 PARAMS ((rtx *));
rtx
gen_split_879 (operands)
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
  emit_insn (gen_rtx_SET (VOIDmode,
	operand4,
	gen_rtx_AND (SImode,
	gen_rtx_LSHIFTRT (SImode,
	operand1,
	operand2),
	operand3)));
  emit_insn (gen_rtx_SET (VOIDmode,
	operand0,
	gen_rtx_COMPARE (CCmode,
	copy_rtx (operand4),
	const0_rtx)));
  _val = get_insns ();
  end_sequence ();
  return _val;
}

/* /home/yu/gcc/gcc_powerpc/crosstool-0.42/build/powerpc-7450-linux-gnu/gcc-3.3.1-glibc-2.3.2/gcc-3.3.1/gcc/config/rs6000/rs6000.md:4718 */
extern rtx gen_split_880 PARAMS ((rtx *));
rtx
gen_split_880 (operands)
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
  emit_insn (gen_rtx_SET (VOIDmode,
	operand0,
	gen_rtx_AND (SImode,
	gen_rtx_LSHIFTRT (SImode,
	operand1,
	operand2),
	operand3)));
  emit_insn (gen_rtx_SET (VOIDmode,
	operand4,
	gen_rtx_COMPARE (CCmode,
	copy_rtx (operand0),
	const0_rtx)));
  _val = get_insns ();
  end_sequence ();
  return _val;
}

/* /home/yu/gcc/gcc_powerpc/crosstool-0.42/build/powerpc-7450-linux-gnu/gcc-3.3.1-glibc-2.3.2/gcc-3.3.1/gcc/config/rs6000/rs6000.md:4760 */
extern rtx gen_split_881 PARAMS ((rtx *));
rtx
gen_split_881 (operands)
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
	gen_rtx_ZERO_EXTEND (SImode,
	gen_rtx_SUBREG (QImode,
	gen_rtx_LSHIFTRT (SImode,
	operand1,
	operand2),
	0))));
  emit_insn (gen_rtx_SET (VOIDmode,
	operand0,
	gen_rtx_COMPARE (CCmode,
	copy_rtx (operand3),
	const0_rtx)));
  _val = get_insns ();
  end_sequence ();
  return _val;
}

/* /home/yu/gcc/gcc_powerpc/crosstool-0.42/build/powerpc-7450-linux-gnu/gcc-3.3.1-glibc-2.3.2/gcc-3.3.1/gcc/config/rs6000/rs6000.md:4796 */
extern rtx gen_split_882 PARAMS ((rtx *));
rtx
gen_split_882 (operands)
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
	operand0,
	gen_rtx_ZERO_EXTEND (SImode,
	gen_rtx_SUBREG (QImode,
	gen_rtx_LSHIFTRT (SImode,
	operand1,
	operand2),
	0))));
  emit_insn (gen_rtx_SET (VOIDmode,
	operand3,
	gen_rtx_COMPARE (CCmode,
	copy_rtx (operand0),
	const0_rtx)));
  _val = get_insns ();
  end_sequence ();
  return _val;
}

/* /home/yu/gcc/gcc_powerpc/crosstool-0.42/build/powerpc-7450-linux-gnu/gcc-3.3.1-glibc-2.3.2/gcc-3.3.1/gcc/config/rs6000/rs6000.md:4839 */
extern rtx gen_split_883 PARAMS ((rtx *));
rtx
gen_split_883 (operands)
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
	gen_rtx_ZERO_EXTEND (SImode,
	gen_rtx_SUBREG (HImode,
	gen_rtx_LSHIFTRT (SImode,
	operand1,
	operand2),
	0))));
  emit_insn (gen_rtx_SET (VOIDmode,
	operand0,
	gen_rtx_COMPARE (CCmode,
	copy_rtx (operand3),
	const0_rtx)));
  _val = get_insns ();
  end_sequence ();
  return _val;
}

/* /home/yu/gcc/gcc_powerpc/crosstool-0.42/build/powerpc-7450-linux-gnu/gcc-3.3.1-glibc-2.3.2/gcc-3.3.1/gcc/config/rs6000/rs6000.md:4875 */
extern rtx gen_split_884 PARAMS ((rtx *));
rtx
gen_split_884 (operands)
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
	operand0,
	gen_rtx_ZERO_EXTEND (SImode,
	gen_rtx_SUBREG (HImode,
	gen_rtx_LSHIFTRT (SImode,
	operand1,
	operand2),
	0))));
  emit_insn (gen_rtx_SET (VOIDmode,
	operand3,
	gen_rtx_COMPARE (CCmode,
	copy_rtx (operand0),
	const0_rtx)));
  _val = get_insns ();
  end_sequence ();
  return _val;
}

/* /home/yu/gcc/gcc_powerpc/crosstool-0.42/build/powerpc-7450-linux-gnu/gcc-3.3.1-glibc-2.3.2/gcc-3.3.1/gcc/config/rs6000/rs6000.md:4921 */
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

{
  if (TARGET_POWER)
    emit_insn (gen_ashrsi3_power (operands[0], operands[1], operands[2]));
  else
    emit_insn (gen_ashrsi3_no_power (operands[0], operands[1], operands[2]));
  DONE;
}
    operand0 = operands[0];
    operand1 = operands[1];
    operand2 = operands[2];
  }
  emit_insn (gen_rtx_SET (VOIDmode,
	operand0,
	gen_rtx_ASHIFTRT (SImode,
	operand1,
	operand2)));
  _val = get_insns ();
  end_sequence ();
  return _val;
}

/* /home/yu/gcc/gcc_powerpc/crosstool-0.42/build/powerpc-7450-linux-gnu/gcc-3.3.1-glibc-2.3.2/gcc-3.3.1/gcc/config/rs6000/rs6000.md:4968 */
extern rtx gen_split_886 PARAMS ((rtx *));
rtx
gen_split_886 (operands)
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
	gen_rtvec (2,
		gen_rtx_SET (VOIDmode,
	operand3,
	gen_rtx_ASHIFTRT (SImode,
	operand1,
	operand2)),
		gen_rtx_CLOBBER (VOIDmode,
	operand4))));
  emit_insn (gen_rtx_SET (VOIDmode,
	operand0,
	gen_rtx_COMPARE (CCmode,
	copy_rtx (operand3),
	const0_rtx)));
  _val = get_insns ();
  end_sequence ();
  return _val;
}

/* /home/yu/gcc/gcc_powerpc/crosstool-0.42/build/powerpc-7450-linux-gnu/gcc-3.3.1-glibc-2.3.2/gcc-3.3.1/gcc/config/rs6000/rs6000.md:4997 */
extern rtx gen_split_887 PARAMS ((rtx *));
rtx
gen_split_887 (operands)
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
	gen_rtx_ASHIFTRT (SImode,
	operand1,
	operand2)));
  emit_insn (gen_rtx_SET (VOIDmode,
	operand0,
	gen_rtx_COMPARE (CCmode,
	copy_rtx (operand3),
	const0_rtx)));
  _val = get_insns ();
  end_sequence ();
  return _val;
}

/* /home/yu/gcc/gcc_powerpc/crosstool-0.42/build/powerpc-7450-linux-gnu/gcc-3.3.1-glibc-2.3.2/gcc-3.3.1/gcc/config/rs6000/rs6000.md:5028 */
extern rtx gen_split_888 PARAMS ((rtx *));
rtx
gen_split_888 (operands)
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
	gen_rtvec (2,
		gen_rtx_SET (VOIDmode,
	operand0,
	gen_rtx_ASHIFTRT (SImode,
	operand1,
	operand2)),
		gen_rtx_CLOBBER (VOIDmode,
	operand4))));
  emit_insn (gen_rtx_SET (VOIDmode,
	operand3,
	gen_rtx_COMPARE (CCmode,
	copy_rtx (operand0),
	const0_rtx)));
  _val = get_insns ();
  end_sequence ();
  return _val;
}

/* /home/yu/gcc/gcc_powerpc/crosstool-0.42/build/powerpc-7450-linux-gnu/gcc-3.3.1-glibc-2.3.2/gcc-3.3.1/gcc/config/rs6000/rs6000.md:5059 */
extern rtx gen_split_889 PARAMS ((rtx *));
rtx
gen_split_889 (operands)
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
	operand0,
	gen_rtx_ASHIFTRT (SImode,
	operand1,
	operand2)));
  emit_insn (gen_rtx_SET (VOIDmode,
	operand3,
	gen_rtx_COMPARE (CCmode,
	copy_rtx (operand0),
	const0_rtx)));
  _val = get_insns ();
  end_sequence ();
  return _val;
}

/* /home/yu/gcc/gcc_powerpc/crosstool-0.42/build/powerpc-7450-linux-gnu/gcc-3.3.1-glibc-2.3.2/gcc-3.3.1/gcc/config/rs6000/rs6000.md:5117 */
rtx
gen_negsf2 (operand0, operand1)
     rtx operand0;
     rtx operand1;
{
  return gen_rtx_SET (VOIDmode,
	operand0,
	gen_rtx_NEG (SFmode,
	operand1));
}

/* /home/yu/gcc/gcc_powerpc/crosstool-0.42/build/powerpc-7450-linux-gnu/gcc-3.3.1-glibc-2.3.2/gcc-3.3.1/gcc/config/rs6000/rs6000.md:5130 */
rtx
gen_abssf2 (operand0, operand1)
     rtx operand0;
     rtx operand1;
{
  return gen_rtx_SET (VOIDmode,
	operand0,
	gen_rtx_ABS (SFmode,
	operand1));
}

/* /home/yu/gcc/gcc_powerpc/crosstool-0.42/build/powerpc-7450-linux-gnu/gcc-3.3.1-glibc-2.3.2/gcc-3.3.1/gcc/config/rs6000/rs6000.md:5150 */
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

/* /home/yu/gcc/gcc_powerpc/crosstool-0.42/build/powerpc-7450-linux-gnu/gcc-3.3.1-glibc-2.3.2/gcc-3.3.1/gcc/config/rs6000/rs6000.md:5173 */
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

/* /home/yu/gcc/gcc_powerpc/crosstool-0.42/build/powerpc-7450-linux-gnu/gcc-3.3.1-glibc-2.3.2/gcc-3.3.1/gcc/config/rs6000/rs6000.md:5196 */
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

/* /home/yu/gcc/gcc_powerpc/crosstool-0.42/build/powerpc-7450-linux-gnu/gcc-3.3.1-glibc-2.3.2/gcc-3.3.1/gcc/config/rs6000/rs6000.md:5219 */
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

/* /home/yu/gcc/gcc_powerpc/crosstool-0.42/build/powerpc-7450-linux-gnu/gcc-3.3.1-glibc-2.3.2/gcc-3.3.1/gcc/config/rs6000/rs6000.md:5356 */
rtx
gen_sqrtsf2 (operand0, operand1)
     rtx operand0;
     rtx operand1;
{
  return gen_rtx_SET (VOIDmode,
	operand0,
	gen_rtx_SQRT (SFmode,
	operand1));
}

/* /home/yu/gcc/gcc_powerpc/crosstool-0.42/build/powerpc-7450-linux-gnu/gcc-3.3.1-glibc-2.3.2/gcc-3.3.1/gcc/config/rs6000/rs6000.md:5380 */
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
{ rs6000_emit_minmax (operands[0], SMAX, operands[1], operands[2]); DONE;}
    operand0 = operands[0];
    operand1 = operands[1];
    operand2 = operands[2];
  }
  emit_insn (gen_rtx_SET (VOIDmode,
	operand0,
	gen_rtx_IF_THEN_ELSE (SFmode,
	gen_rtx_GE (VOIDmode,
	operand1,
	operand2),
	operand1,
	operand2)));
  _val = get_insns ();
  end_sequence ();
  return _val;
}

/* /home/yu/gcc/gcc_powerpc/crosstool-0.42/build/powerpc-7450-linux-gnu/gcc-3.3.1-glibc-2.3.2/gcc-3.3.1/gcc/config/rs6000/rs6000.md:5389 */
rtx
gen_minsf3 (operand0, operand1, operand2)
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
{ rs6000_emit_minmax (operands[0], SMIN, operands[1], operands[2]); DONE;}
    operand0 = operands[0];
    operand1 = operands[1];
    operand2 = operands[2];
  }
  emit_insn (gen_rtx_SET (VOIDmode,
	operand0,
	gen_rtx_IF_THEN_ELSE (SFmode,
	gen_rtx_GE (VOIDmode,
	operand1,
	operand2),
	operand2,
	operand1)));
  _val = get_insns ();
  end_sequence ();
  return _val;
}

/* /home/yu/gcc/gcc_powerpc/crosstool-0.42/build/powerpc-7450-linux-gnu/gcc-3.3.1-glibc-2.3.2/gcc-3.3.1/gcc/config/rs6000/rs6000.md:5398 */
extern rtx gen_split_899 PARAMS ((rtx *));
rtx
gen_split_899 (operands)
      rtx *operands ATTRIBUTE_UNUSED;
{
  rtx _val = 0;
  start_sequence ();

{ rs6000_emit_minmax (operands[0], GET_CODE (operands[3]), 
		      operands[1], operands[2]);
  DONE;
}
  emit_insn (const0_rtx);
  _val = get_insns ();
  end_sequence ();
  return _val;
}

/* /home/yu/gcc/gcc_powerpc/crosstool-0.42/build/powerpc-7450-linux-gnu/gcc-3.3.1-glibc-2.3.2/gcc-3.3.1/gcc/config/rs6000/rs6000.md:5459 */
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

{
  if (rs6000_emit_cmove (operands[0], operands[1], operands[2], operands[3]))
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
	gen_rtx_IF_THEN_ELSE (SFmode,
	operand1,
	operand2,
	operand3)));
  _val = get_insns ();
  end_sequence ();
  return _val;
}

/* /home/yu/gcc/gcc_powerpc/crosstool-0.42/build/powerpc-7450-linux-gnu/gcc-3.3.1-glibc-2.3.2/gcc-3.3.1/gcc/config/rs6000/rs6000.md:5614 */
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
{ rs6000_emit_minmax (operands[0], SMAX, operands[1], operands[2]); DONE;}
    operand0 = operands[0];
    operand1 = operands[1];
    operand2 = operands[2];
  }
  emit_insn (gen_rtx_SET (VOIDmode,
	operand0,
	gen_rtx_IF_THEN_ELSE (DFmode,
	gen_rtx_GE (VOIDmode,
	operand1,
	operand2),
	operand1,
	operand2)));
  _val = get_insns ();
  end_sequence ();
  return _val;
}

/* /home/yu/gcc/gcc_powerpc/crosstool-0.42/build/powerpc-7450-linux-gnu/gcc-3.3.1-glibc-2.3.2/gcc-3.3.1/gcc/config/rs6000/rs6000.md:5623 */
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
{ rs6000_emit_minmax (operands[0], SMIN, operands[1], operands[2]); DONE;}
    operand0 = operands[0];
    operand1 = operands[1];
    operand2 = operands[2];
  }
  emit_insn (gen_rtx_SET (VOIDmode,
	operand0,
	gen_rtx_IF_THEN_ELSE (DFmode,
	gen_rtx_GE (VOIDmode,
	operand1,
	operand2),
	operand2,
	operand1)));
  _val = get_insns ();
  end_sequence ();
  return _val;
}

/* /home/yu/gcc/gcc_powerpc/crosstool-0.42/build/powerpc-7450-linux-gnu/gcc-3.3.1-glibc-2.3.2/gcc-3.3.1/gcc/config/rs6000/rs6000.md:5632 */
extern rtx gen_split_903 PARAMS ((rtx *));
rtx
gen_split_903 (operands)
      rtx *operands ATTRIBUTE_UNUSED;
{
  rtx _val = 0;
  start_sequence ();

{ rs6000_emit_minmax (operands[0], GET_CODE (operands[3]), 
		      operands[1], operands[2]);
  DONE;
}
  emit_insn (const0_rtx);
  _val = get_insns ();
  end_sequence ();
  return _val;
}

/* /home/yu/gcc/gcc_powerpc/crosstool-0.42/build/powerpc-7450-linux-gnu/gcc-3.3.1-glibc-2.3.2/gcc-3.3.1/gcc/config/rs6000/rs6000.md:5645 */
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

{
  if (rs6000_emit_cmove (operands[0], operands[1], operands[2], operands[3]))
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
	gen_rtx_IF_THEN_ELSE (DFmode,
	operand1,
	operand2,
	operand3)));
  _val = get_insns ();
  end_sequence ();
  return _val;
}

/* /home/yu/gcc/gcc_powerpc/crosstool-0.42/build/powerpc-7450-linux-gnu/gcc-3.3.1-glibc-2.3.2/gcc-3.3.1/gcc/config/rs6000/rs6000.md:5698 */
rtx
gen_floatsidf2 (operand0, operand1)
     rtx operand0;
     rtx operand1;
{
  rtx operand2;
  rtx operand3;
  rtx operand4;
  rtx operand5;
  rtx operand6;
  rtx _val = 0;
  start_sequence ();
  {
    rtx operands[7];
    operands[0] = operand0;
    operands[1] = operand1;

{
  if (TARGET_POWERPC64)
    {
      rtx mem = assign_stack_temp (DImode, GET_MODE_SIZE (DImode), 0);
      rtx t1 = gen_reg_rtx (DImode);
      rtx t2 = gen_reg_rtx (DImode);
      emit_insn (gen_floatsidf_ppc64 (operands[0], operands[1], mem, t1, t2));
      DONE;
    }

  operands[2] = force_reg (SImode, GEN_INT (0x43300000));
  operands[3] = force_reg (DFmode, CONST_DOUBLE_ATOF ("4503601774854144", DFmode));
  operands[4] = assign_stack_temp (DFmode, GET_MODE_SIZE (DFmode), 0);
  operands[5] = gen_reg_rtx (DFmode);
  operands[6] = gen_reg_rtx (SImode);
}
    operand0 = operands[0];
    operand1 = operands[1];
    operand2 = operands[2];
    operand3 = operands[3];
    operand4 = operands[4];
    operand5 = operands[5];
    operand6 = operands[6];
  }
  emit (gen_rtx_PARALLEL (VOIDmode,
	gen_rtvec (6,
		gen_rtx_SET (VOIDmode,
	operand0,
	gen_rtx_FLOAT (DFmode,
	operand1)),
		gen_rtx_USE (VOIDmode,
	operand2),
		gen_rtx_USE (VOIDmode,
	operand3),
		gen_rtx_CLOBBER (VOIDmode,
	operand4),
		gen_rtx_CLOBBER (VOIDmode,
	operand5),
		gen_rtx_CLOBBER (VOIDmode,
	operand6))));
  _val = get_insns ();
  end_sequence ();
  return _val;
}

/* /home/yu/gcc/gcc_powerpc/crosstool-0.42/build/powerpc-7450-linux-gnu/gcc-3.3.1-glibc-2.3.2/gcc-3.3.1/gcc/config/rs6000/rs6000.md:5737 */
extern rtx gen_split_906 PARAMS ((rtx *));
rtx
gen_split_906 (operands)
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
  rtx lowword, highword;
  if (GET_CODE (operands[4]) != MEM)
    abort();
  highword = XEXP (operands[4], 0);
  lowword = plus_constant (highword, 4);
  if (! WORDS_BIG_ENDIAN)
    {
      rtx tmp;
      tmp = highword; highword = lowword; lowword = tmp;
    }

  emit_insn (gen_xorsi3 (operands[6], operands[1], 
			 GEN_INT (~ (HOST_WIDE_INT) 0x7fffffff)));
  emit_move_insn (gen_rtx_MEM (SImode, lowword), operands[6]);
  emit_move_insn (gen_rtx_MEM (SImode, highword), operands[2]);
  emit_move_insn (operands[5], operands[4]);
  emit_insn (gen_subdf3 (operands[0], operands[5], operands[3]));
  DONE;
}
  operand0 = operands[0];
  operand1 = operands[1];
  operand2 = operands[2];
  operand3 = operands[3];
  operand4 = operands[4];
  operand5 = operands[5];
  operand6 = operands[6];
  emit_insn (gen_rtx_SET (VOIDmode,
	operand0,
	gen_rtx_FLOAT (DFmode,
	operand1)));
  emit_insn (gen_rtx_USE (VOIDmode,
	operand2));
  emit_insn (gen_rtx_USE (VOIDmode,
	operand3));
  emit_insn (gen_rtx_CLOBBER (VOIDmode,
	operand4));
  emit_insn (gen_rtx_CLOBBER (VOIDmode,
	operand5));
  emit_insn (gen_rtx_CLOBBER (VOIDmode,
	operand6));
  _val = get_insns ();
  end_sequence ();
  return _val;
}

/* /home/yu/gcc/gcc_powerpc/crosstool-0.42/build/powerpc-7450-linux-gnu/gcc-3.3.1-glibc-2.3.2/gcc-3.3.1/gcc/config/rs6000/rs6000.md:5781 */
rtx
gen_floatunssidf2 (operand0, operand1)
     rtx operand0;
     rtx operand1;
{
  rtx operand2;
  rtx operand3;
  rtx operand4;
  rtx operand5;
  rtx _val = 0;
  start_sequence ();
  {
    rtx operands[6];
    operands[0] = operand0;
    operands[1] = operand1;

{
  if (TARGET_POWERPC64)
    {
      rtx mem = assign_stack_temp (DImode, GET_MODE_SIZE (DImode), 0);
      rtx t1 = gen_reg_rtx (DImode);
      rtx t2 = gen_reg_rtx (DImode);
      emit_insn (gen_floatunssidf_ppc64 (operands[0], operands[1], mem,
					 t1, t2));
      DONE;
    }

  operands[2] = force_reg (SImode, GEN_INT (0x43300000));
  operands[3] = force_reg (DFmode, CONST_DOUBLE_ATOF ("4503599627370496", DFmode));
  operands[4] = assign_stack_temp (DFmode, GET_MODE_SIZE (DFmode), 0);
  operands[5] = gen_reg_rtx (DFmode);
}
    operand0 = operands[0];
    operand1 = operands[1];
    operand2 = operands[2];
    operand3 = operands[3];
    operand4 = operands[4];
    operand5 = operands[5];
  }
  emit (gen_rtx_PARALLEL (VOIDmode,
	gen_rtvec (5,
		gen_rtx_SET (VOIDmode,
	operand0,
	gen_rtx_UNSIGNED_FLOAT (DFmode,
	operand1)),
		gen_rtx_USE (VOIDmode,
	operand2),
		gen_rtx_USE (VOIDmode,
	operand3),
		gen_rtx_CLOBBER (VOIDmode,
	operand4),
		gen_rtx_CLOBBER (VOIDmode,
	operand5))));
  _val = get_insns ();
  end_sequence ();
  return _val;
}

/* /home/yu/gcc/gcc_powerpc/crosstool-0.42/build/powerpc-7450-linux-gnu/gcc-3.3.1-glibc-2.3.2/gcc-3.3.1/gcc/config/rs6000/rs6000.md:5818 */
extern rtx gen_split_908 PARAMS ((rtx *));
rtx
gen_split_908 (operands)
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

{
  rtx lowword, highword;
  if (GET_CODE (operands[4]) != MEM)
    abort();
  highword = XEXP (operands[4], 0);
  lowword = plus_constant (highword, 4);
  if (! WORDS_BIG_ENDIAN)
    {
      rtx tmp;
      tmp = highword; highword = lowword; lowword = tmp;
    }

  emit_move_insn (gen_rtx_MEM (SImode, lowword), operands[1]);
  emit_move_insn (gen_rtx_MEM (SImode, highword), operands[2]);
  emit_move_insn (operands[5], operands[4]);
  emit_insn (gen_subdf3 (operands[0], operands[5], operands[3]));
  DONE;
}
  operand0 = operands[0];
  operand1 = operands[1];
  operand2 = operands[2];
  operand3 = operands[3];
  operand4 = operands[4];
  operand5 = operands[5];
  emit_insn (gen_rtx_SET (VOIDmode,
	operand0,
	gen_rtx_UNSIGNED_FLOAT (DFmode,
	operand1)));
  emit_insn (gen_rtx_USE (VOIDmode,
	operand2));
  emit_insn (gen_rtx_USE (VOIDmode,
	operand3));
  emit_insn (gen_rtx_CLOBBER (VOIDmode,
	operand4));
  emit_insn (gen_rtx_CLOBBER (VOIDmode,
	operand5));
  _val = get_insns ();
  end_sequence ();
  return _val;
}

/* /home/yu/gcc/gcc_powerpc/crosstool-0.42/build/powerpc-7450-linux-gnu/gcc-3.3.1-glibc-2.3.2/gcc-3.3.1/gcc/config/rs6000/rs6000.md:5852 */
rtx
gen_fix_truncdfsi2 (operand0, operand1)
     rtx operand0;
     rtx operand1;
{
  rtx operand2;
  rtx operand3;
  rtx _val = 0;
  start_sequence ();
  {
    rtx operands[4];
    operands[0] = operand0;
    operands[1] = operand1;

{
  operands[2] = gen_reg_rtx (DImode);
  operands[3] = assign_stack_temp (DImode, GET_MODE_SIZE (DImode), 0);
}
    operand0 = operands[0];
    operand1 = operands[1];
    operand2 = operands[2];
    operand3 = operands[3];
  }
  emit (gen_rtx_PARALLEL (VOIDmode,
	gen_rtvec (3,
		gen_rtx_SET (VOIDmode,
	operand0,
	gen_rtx_FIX (SImode,
	operand1)),
		gen_rtx_CLOBBER (VOIDmode,
	operand2),
		gen_rtx_CLOBBER (VOIDmode,
	operand3))));
  _val = get_insns ();
  end_sequence ();
  return _val;
}

/* /home/yu/gcc/gcc_powerpc/crosstool-0.42/build/powerpc-7450-linux-gnu/gcc-3.3.1-glibc-2.3.2/gcc-3.3.1/gcc/config/rs6000/rs6000.md:5873 */
extern rtx gen_split_910 PARAMS ((rtx *));
rtx
gen_split_910 (operands)
      rtx *operands;
{
  rtx operand0;
  rtx operand1;
  rtx operand2;
  rtx operand3;
  rtx _val = 0;
  start_sequence ();

{
  rtx lowword;
  if (GET_CODE (operands[3]) != MEM)
    abort();
  lowword = XEXP (operands[3], 0);
  if (WORDS_BIG_ENDIAN)
    lowword = plus_constant (lowword, 4);

  emit_insn (gen_fctiwz (operands[2], operands[1]));
  emit_move_insn (operands[3], operands[2]);
  emit_move_insn (operands[0], gen_rtx_MEM (SImode, lowword));
  DONE;
}
  operand0 = operands[0];
  operand1 = operands[1];
  operand2 = operands[2];
  operand3 = operands[3];
  emit_insn (gen_rtx_SET (VOIDmode,
	operand0,
	gen_rtx_FIX (SImode,
	operand1)));
  emit_insn (gen_rtx_CLOBBER (VOIDmode,
	operand2));
  emit_insn (gen_rtx_CLOBBER (VOIDmode,
	operand3));
  _val = get_insns ();
  end_sequence ();
  return _val;
}

/* /home/yu/gcc/gcc_powerpc/crosstool-0.42/build/powerpc-7450-linux-gnu/gcc-3.3.1-glibc-2.3.2/gcc-3.3.1/gcc/config/rs6000/rs6000.md:5922 */
extern rtx gen_split_911 PARAMS ((rtx *));
rtx
gen_split_911 (operands)
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
  emit_insn (gen_rtx_SET (VOIDmode,
	operand3,
	gen_rtx_SIGN_EXTEND (DImode,
	operand1)));
  emit_insn (gen_rtx_SET (VOIDmode,
	operand2,
	copy_rtx (operand3)));
  emit_insn (gen_rtx_SET (VOIDmode,
	operand4,
	copy_rtx (operand2)));
  emit_insn (gen_rtx_SET (VOIDmode,
	operand0,
	gen_rtx_FLOAT (DFmode,
	copy_rtx (operand4))));
  _val = get_insns ();
  end_sequence ();
  return _val;
}

/* /home/yu/gcc/gcc_powerpc/crosstool-0.42/build/powerpc-7450-linux-gnu/gcc-3.3.1-glibc-2.3.2/gcc-3.3.1/gcc/config/rs6000/rs6000.md:5937 */
extern rtx gen_split_912 PARAMS ((rtx *));
rtx
gen_split_912 (operands)
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
  emit_insn (gen_rtx_SET (VOIDmode,
	operand3,
	gen_rtx_ZERO_EXTEND (DImode,
	operand1)));
  emit_insn (gen_rtx_SET (VOIDmode,
	operand2,
	copy_rtx (operand3)));
  emit_insn (gen_rtx_SET (VOIDmode,
	operand4,
	copy_rtx (operand2)));
  emit_insn (gen_rtx_SET (VOIDmode,
	operand0,
	gen_rtx_FLOAT (DFmode,
	copy_rtx (operand4))));
  _val = get_insns ();
  end_sequence ();
  return _val;
}

/* /home/yu/gcc/gcc_powerpc/crosstool-0.42/build/powerpc-7450-linux-gnu/gcc-3.3.1-glibc-2.3.2/gcc-3.3.1/gcc/config/rs6000/rs6000.md:5959 */
rtx
gen_floatdisf2 (operand0, operand1)
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
  if (!flag_unsafe_math_optimizations)
    {
      rtx label = gen_label_rtx ();
      emit_insn (gen_floatdisf2_internal2 (operands[1], label));
      emit_label (label);
    }
  emit_insn (gen_floatdisf2_internal1 (operands[0], operands[1]));
  DONE;
}
    operand0 = operands[0];
    operand1 = operands[1];
  }
  emit_insn (gen_rtx_SET (VOIDmode,
	operand0,
	gen_rtx_FLOAT (SFmode,
	operand1)));
  _val = get_insns ();
  end_sequence ();
  return _val;
}

/* /home/yu/gcc/gcc_powerpc/crosstool-0.42/build/powerpc-7450-linux-gnu/gcc-3.3.1-glibc-2.3.2/gcc-3.3.1/gcc/config/rs6000/rs6000.md:5978 */
extern rtx gen_split_914 PARAMS ((rtx *));
rtx
gen_split_914 (operands)
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
	gen_rtx_FLOAT (DFmode,
	operand1)));
  emit_insn (gen_rtx_SET (VOIDmode,
	operand0,
	gen_rtx_FLOAT_TRUNCATE (SFmode,
	copy_rtx (operand2))));
  _val = get_insns ();
  end_sequence ();
  return _val;
}

/* /home/yu/gcc/gcc_powerpc/crosstool-0.42/build/powerpc-7450-linux-gnu/gcc-3.3.1-glibc-2.3.2/gcc-3.3.1/gcc/config/rs6000/rs6000.md:5995 */
rtx
gen_floatdisf2_internal2 (operand0, operand1)
     rtx operand0;
     rtx operand1;
{
  rtx operand2;
  rtx operand3;
  rtx operand4;
  rtx operand5;
  rtx operand6 ATTRIBUTE_UNUSED;
  rtx operand7 ATTRIBUTE_UNUSED;
  rtx _val = 0;
  start_sequence ();
  {
    rtx operands[8];
    operands[0] = operand0;
    operands[1] = operand1;

{
  operands[2] = gen_reg_rtx (DImode);
  operands[3] = gen_reg_rtx (DImode);
  operands[4] = gen_reg_rtx (CCmode);
  operands[5] = gen_reg_rtx (CCUNSmode);
}
    operand0 = operands[0];
    operand1 = operands[1];
    operand2 = operands[2];
    operand3 = operands[3];
    operand4 = operands[4];
    operand5 = operands[5];
    operand6 = operands[6];
    operand7 = operands[7];
  }
  emit (gen_rtx_PARALLEL (VOIDmode,
	gen_rtvec (3,
		gen_rtx_SET (VOIDmode,
	operand4,
	gen_rtx_COMPARE (CCmode,
	gen_rtx_AND (DImode,
	operand0,
	GEN_INT (2047LL)),
	const0_rtx)),
		gen_rtx_SET (VOIDmode,
	operand2,
	gen_rtx_AND (DImode,
	operand0,
	GEN_INT (2047LL))),
		gen_rtx_CLOBBER (VOIDmode,
	gen_rtx_SCRATCH (CCmode)))));
  emit_insn (gen_rtx_SET (VOIDmode,
	operand3,
	gen_rtx_ASHIFTRT (DImode,
	operand0,
	GEN_INT (53LL))));
  emit_insn (gen_rtx_SET (VOIDmode,
	operand3,
	gen_rtx_PLUS (DImode,
	operand3,
	const1_rtx)));
  emit_jump_insn (gen_rtx_SET (VOIDmode,
	pc_rtx,
	gen_rtx_IF_THEN_ELSE (VOIDmode,
	gen_rtx_EQ (VOIDmode,
	operand4,
	const0_rtx),
	gen_rtx_LABEL_REF (VOIDmode,
	operand1),
	pc_rtx)));
  emit_insn (gen_rtx_SET (VOIDmode,
	operand5,
	gen_rtx_COMPARE (CCUNSmode,
	operand3,
	GEN_INT (2LL))));
  emit_jump_insn (gen_rtx_SET (VOIDmode,
	pc_rtx,
	gen_rtx_IF_THEN_ELSE (VOIDmode,
	gen_rtx_LTU (VOIDmode,
	operand5,
	const0_rtx),
	gen_rtx_LABEL_REF (VOIDmode,
	operand1),
	pc_rtx)));
  emit_insn (gen_rtx_SET (VOIDmode,
	operand0,
	gen_rtx_XOR (DImode,
	operand0,
	operand2)));
  emit_insn (gen_rtx_SET (VOIDmode,
	operand0,
	gen_rtx_IOR (DImode,
	operand0,
	GEN_INT (2048LL))));
  _val = get_insns ();
  end_sequence ();
  return _val;
}

/* /home/yu/gcc/gcc_powerpc/crosstool-0.42/build/powerpc-7450-linux-gnu/gcc-3.3.1-glibc-2.3.2/gcc-3.3.1/gcc/config/rs6000/rs6000.md:6076 */
rtx
gen_mulsidi3 (operand0, operand1, operand2)
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
  if (! TARGET_POWER && ! TARGET_POWERPC)
    {
      emit_move_insn (gen_rtx_REG (SImode, 3), operands[1]);
      emit_move_insn (gen_rtx_REG (SImode, 4), operands[2]);
      emit_insn (gen_mull_call ());
      if (WORDS_BIG_ENDIAN)
        emit_move_insn (operands[0], gen_rtx_REG (DImode, 3));
      else
	{
	  emit_move_insn (operand_subword (operands[0], 0, 0, DImode),
			  gen_rtx_REG (SImode, 3));
	  emit_move_insn (operand_subword (operands[0], 1, 0, DImode),
			  gen_rtx_REG (SImode, 4));
	}
      DONE;
    }
  else if (TARGET_POWER)
    {
      emit_insn (gen_mulsidi3_mq (operands[0], operands[1], operands[2]));
      DONE;
    }
}
    operand0 = operands[0];
    operand1 = operands[1];
    operand2 = operands[2];
  }
  emit_insn (gen_rtx_SET (VOIDmode,
	operand0,
	gen_rtx_MULT (DImode,
	gen_rtx_SIGN_EXTEND (DImode,
	operand1),
	gen_rtx_SIGN_EXTEND (DImode,
	operand2))));
  _val = get_insns ();
  end_sequence ();
  return _val;
}

/* /home/yu/gcc/gcc_powerpc/crosstool-0.42/build/powerpc-7450-linux-gnu/gcc-3.3.1-glibc-2.3.2/gcc-3.3.1/gcc/config/rs6000/rs6000.md:6130 */
extern rtx gen_split_917 PARAMS ((rtx *));
rtx
gen_split_917 (operands)
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
  int endian = (WORDS_BIG_ENDIAN == 0);
  operands[3] = operand_subword (operands[0], endian, 0, DImode);
  operands[4] = operand_subword (operands[0], 1 - endian, 0, DImode);
}
  operand0 = operands[0];
  operand1 = operands[1];
  operand2 = operands[2];
  operand3 = operands[3];
  operand4 = operands[4];
  emit_insn (gen_rtx_SET (VOIDmode,
	operand3,
	gen_rtx_TRUNCATE (SImode,
	gen_rtx_LSHIFTRT (DImode,
	gen_rtx_MULT (DImode,
	gen_rtx_SIGN_EXTEND (DImode,
	operand1),
	gen_rtx_SIGN_EXTEND (DImode,
	operand2)),
	GEN_INT (32LL)))));
  emit_insn (gen_rtx_SET (VOIDmode,
	operand4,
	gen_rtx_MULT (SImode,
	copy_rtx (operand1),
	copy_rtx (operand2))));
  _val = get_insns ();
  end_sequence ();
  return _val;
}

/* /home/yu/gcc/gcc_powerpc/crosstool-0.42/build/powerpc-7450-linux-gnu/gcc-3.3.1-glibc-2.3.2/gcc-3.3.1/gcc/config/rs6000/rs6000.md:6150 */
rtx
gen_umulsidi3 (operand0, operand1, operand2)
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
  if (TARGET_POWER)
    {
      emit_insn (gen_umulsidi3_mq (operands[0], operands[1], operands[2]));
      DONE;
    }
}
    operand0 = operands[0];
    operand1 = operands[1];
    operand2 = operands[2];
  }
  emit_insn (gen_rtx_SET (VOIDmode,
	operand0,
	gen_rtx_MULT (DImode,
	gen_rtx_ZERO_EXTEND (DImode,
	operand1),
	gen_rtx_ZERO_EXTEND (DImode,
	operand2))));
  _val = get_insns ();
  end_sequence ();
  return _val;
}

/* /home/yu/gcc/gcc_powerpc/crosstool-0.42/build/powerpc-7450-linux-gnu/gcc-3.3.1-glibc-2.3.2/gcc-3.3.1/gcc/config/rs6000/rs6000.md:6193 */
extern rtx gen_split_919 PARAMS ((rtx *));
rtx
gen_split_919 (operands)
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
  int endian = (WORDS_BIG_ENDIAN == 0);
  operands[3] = operand_subword (operands[0], endian, 0, DImode);
  operands[4] = operand_subword (operands[0], 1 - endian, 0, DImode);
}
  operand0 = operands[0];
  operand1 = operands[1];
  operand2 = operands[2];
  operand3 = operands[3];
  operand4 = operands[4];
  emit_insn (gen_rtx_SET (VOIDmode,
	operand3,
	gen_rtx_TRUNCATE (SImode,
	gen_rtx_LSHIFTRT (DImode,
	gen_rtx_MULT (DImode,
	gen_rtx_ZERO_EXTEND (DImode,
	operand1),
	gen_rtx_ZERO_EXTEND (DImode,
	operand2)),
	GEN_INT (32LL)))));
  emit_insn (gen_rtx_SET (VOIDmode,
	operand4,
	gen_rtx_MULT (SImode,
	copy_rtx (operand1),
	copy_rtx (operand2))));
  _val = get_insns ();
  end_sequence ();
  return _val;
}

/* /home/yu/gcc/gcc_powerpc/crosstool-0.42/build/powerpc-7450-linux-gnu/gcc-3.3.1-glibc-2.3.2/gcc-3.3.1/gcc/config/rs6000/rs6000.md:6213 */
rtx
gen_smulsi3_highpart (operand0, operand1, operand2)
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
  if (! TARGET_POWER && ! TARGET_POWERPC)
    {
      emit_move_insn (gen_rtx_REG (SImode, 3), operands[1]);
      emit_move_insn (gen_rtx_REG (SImode, 4), operands[2]);
      emit_insn (gen_mulh_call ());
      emit_move_insn (operands[0], gen_rtx_REG (SImode, 3));
      DONE;
    }
  else if (TARGET_POWER)
    {
      emit_insn (gen_smulsi3_highpart_mq (operands[0], operands[1], operands[2]));
      DONE;
    }
}
    operand0 = operands[0];
    operand1 = operands[1];
    operand2 = operands[2];
  }
  emit_insn (gen_rtx_SET (VOIDmode,
	operand0,
	gen_rtx_TRUNCATE (SImode,
	gen_rtx_LSHIFTRT (DImode,
	gen_rtx_MULT (DImode,
	gen_rtx_SIGN_EXTEND (DImode,
	operand1),
	gen_rtx_SIGN_EXTEND (DImode,
	operand2)),
	GEN_INT (32LL)))));
  _val = get_insns ();
  end_sequence ();
  return _val;
}

/* /home/yu/gcc/gcc_powerpc/crosstool-0.42/build/powerpc-7450-linux-gnu/gcc-3.3.1-glibc-2.3.2/gcc-3.3.1/gcc/config/rs6000/rs6000.md:6264 */
rtx
gen_umulsi3_highpart (operand0, operand1, operand2)
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
  if (TARGET_POWER)
    {
      emit_insn (gen_umulsi3_highpart_mq (operands[0], operands[1], operands[2]));
      DONE;
    }
}
    operand0 = operands[0];
    operand1 = operands[1];
    operand2 = operands[2];
  }
  emit_insn (gen_rtx_SET (VOIDmode,
	operand0,
	gen_rtx_TRUNCATE (SImode,
	gen_rtx_LSHIFTRT (DImode,
	gen_rtx_MULT (DImode,
	gen_rtx_ZERO_EXTEND (DImode,
	operand1),
	gen_rtx_ZERO_EXTEND (DImode,
	operand2)),
	GEN_INT (32LL)))));
  _val = get_insns ();
  end_sequence ();
  return _val;
}

/* /home/yu/gcc/gcc_powerpc/crosstool-0.42/build/powerpc-7450-linux-gnu/gcc-3.3.1-glibc-2.3.2/gcc-3.3.1/gcc/config/rs6000/rs6000.md:6361 */
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

{
  if (! TARGET_POWERPC64)
    {
      if (non_short_cint_operand (operands[2], DImode))
	FAIL;
    }
  else
    if (GET_CODE (operands[2]) == CONST_INT
	&& ! add_operand (operands[2], DImode))
      {
	rtx tmp = ((no_new_pseudos || rtx_equal_p (operands[0], operands[1]))
		   ? operands[0] : gen_reg_rtx (DImode));

	HOST_WIDE_INT val = INTVAL (operands[2]);
	HOST_WIDE_INT low = ((val & 0xffff) ^ 0x8000) - 0x8000;
	HOST_WIDE_INT rest = trunc_int_for_mode (val - low, DImode);

	if (!CONST_OK_FOR_LETTER_P (rest, 'L'))
	  FAIL;

	/* The ordering here is important for the prolog expander.
	   When space is allocated from the stack, adding 'low' first may
	   produce a temporary deallocation (which would be bad).  */
	emit_insn (gen_adddi3 (tmp, operands[1], GEN_INT (rest)));
	emit_insn (gen_adddi3 (operands[0], tmp, GEN_INT (low)));
	DONE;
      }
}
    operand0 = operands[0];
    operand1 = operands[1];
    operand2 = operands[2];
  }
  emit_insn (gen_rtx_SET (VOIDmode,
	operand0,
	gen_rtx_PLUS (DImode,
	operand1,
	operand2)));
  _val = get_insns ();
  end_sequence ();
  return _val;
}

/* /home/yu/gcc/gcc_powerpc/crosstool-0.42/build/powerpc-7450-linux-gnu/gcc-3.3.1-glibc-2.3.2/gcc-3.3.1/gcc/config/rs6000/rs6000.md:6425 */
extern rtx gen_split_923 PARAMS ((rtx *));
rtx
gen_split_923 (operands)
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
	gen_rtx_PLUS (DImode,
	operand1,
	operand2)));
  emit_insn (gen_rtx_SET (VOIDmode,
	operand0,
	gen_rtx_COMPARE (CCmode,
	copy_rtx (operand3),
	const0_rtx)));
  _val = get_insns ();
  end_sequence ();
  return _val;
}

/* /home/yu/gcc/gcc_powerpc/crosstool-0.42/build/powerpc-7450-linux-gnu/gcc-3.3.1-glibc-2.3.2/gcc-3.3.1/gcc/config/rs6000/rs6000.md:6455 */
extern rtx gen_split_924 PARAMS ((rtx *));
rtx
gen_split_924 (operands)
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
	operand0,
	gen_rtx_PLUS (DImode,
	operand1,
	operand2)));
  emit_insn (gen_rtx_SET (VOIDmode,
	operand3,
	gen_rtx_COMPARE (CCmode,
	copy_rtx (operand0),
	const0_rtx)));
  _val = get_insns ();
  end_sequence ();
  return _val;
}

/* /home/yu/gcc/gcc_powerpc/crosstool-0.42/build/powerpc-7450-linux-gnu/gcc-3.3.1-glibc-2.3.2/gcc-3.3.1/gcc/config/rs6000/rs6000.md:6474 */
extern rtx gen_split_925 PARAMS ((rtx *));
rtx
gen_split_925 (operands)
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
  HOST_WIDE_INT val = INTVAL (operands[2]);
  HOST_WIDE_INT low = ((val & 0xffff) ^ 0x8000) - 0x8000;
  HOST_WIDE_INT rest = trunc_int_for_mode (val - low, DImode);

  operands[4] = GEN_INT (low);
  if (CONST_OK_FOR_LETTER_P (rest, 'L'))
    operands[3] = GEN_INT (rest);
  else if (! no_new_pseudos)
    {
      operands[3] = gen_reg_rtx (DImode);
      emit_move_insn (operands[3], operands[2]);
      emit_insn (gen_adddi3 (operands[0], operands[1], operands[3]));
      DONE;
    }
  else
    FAIL;
}
  operand0 = operands[0];
  operand1 = operands[1];
  operand2 = operands[2];
  operand3 = operands[3];
  operand4 = operands[4];
  emit_insn (gen_rtx_SET (VOIDmode,
	operand0,
	gen_rtx_PLUS (DImode,
	operand1,
	operand3)));
  emit_insn (gen_rtx_SET (VOIDmode,
	copy_rtx (operand0),
	gen_rtx_PLUS (DImode,
	copy_rtx (operand0),
	operand4)));
  _val = get_insns ();
  end_sequence ();
  return _val;
}

/* /home/yu/gcc/gcc_powerpc/crosstool-0.42/build/powerpc-7450-linux-gnu/gcc-3.3.1-glibc-2.3.2/gcc-3.3.1/gcc/config/rs6000/rs6000.md:6519 */
extern rtx gen_split_926 PARAMS ((rtx *));
rtx
gen_split_926 (operands)
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
	gen_rtx_NOT (DImode,
	operand1)));
  emit_insn (gen_rtx_SET (VOIDmode,
	operand0,
	gen_rtx_COMPARE (CCmode,
	copy_rtx (operand2),
	const0_rtx)));
  _val = get_insns ();
  end_sequence ();
  return _val;
}

/* /home/yu/gcc/gcc_powerpc/crosstool-0.42/build/powerpc-7450-linux-gnu/gcc-3.3.1-glibc-2.3.2/gcc-3.3.1/gcc/config/rs6000/rs6000.md:6545 */
extern rtx gen_split_927 PARAMS ((rtx *));
rtx
gen_split_927 (operands)
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
	gen_rtx_NOT (DImode,
	operand1)));
  emit_insn (gen_rtx_SET (VOIDmode,
	operand2,
	gen_rtx_COMPARE (CCmode,
	copy_rtx (operand0),
	const0_rtx)));
  _val = get_insns ();
  end_sequence ();
  return _val;
}

/* /home/yu/gcc/gcc_powerpc/crosstool-0.42/build/powerpc-7450-linux-gnu/gcc-3.3.1-glibc-2.3.2/gcc-3.3.1/gcc/config/rs6000/rs6000.md:6581 */
extern rtx gen_split_928 PARAMS ((rtx *));
rtx
gen_split_928 (operands)
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
	gen_rtx_MINUS (DImode,
	operand1,
	operand2)));
  emit_insn (gen_rtx_SET (VOIDmode,
	operand0,
	gen_rtx_COMPARE (CCmode,
	copy_rtx (operand3),
	const0_rtx)));
  _val = get_insns ();
  end_sequence ();
  return _val;
}

/* /home/yu/gcc/gcc_powerpc/crosstool-0.42/build/powerpc-7450-linux-gnu/gcc-3.3.1-glibc-2.3.2/gcc-3.3.1/gcc/config/rs6000/rs6000.md:6609 */
extern rtx gen_split_929 PARAMS ((rtx *));
rtx
gen_split_929 (operands)
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
	operand0,
	gen_rtx_MINUS (DImode,
	operand1,
	operand2)));
  emit_insn (gen_rtx_SET (VOIDmode,
	operand3,
	gen_rtx_COMPARE (CCmode,
	copy_rtx (operand0),
	const0_rtx)));
  _val = get_insns ();
  end_sequence ();
  return _val;
}

/* /home/yu/gcc/gcc_powerpc/crosstool-0.42/build/powerpc-7450-linux-gnu/gcc-3.3.1-glibc-2.3.2/gcc-3.3.1/gcc/config/rs6000/rs6000.md:6624 */
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

{
  if (GET_CODE (operands[2]) == CONST_INT)
    {
      emit_insn (gen_adddi3 (operands[0], operands[1],
			     negate_rtx (DImode, operands[2])));
      DONE;
    }
}
    operand0 = operands[0];
    operand1 = operands[1];
    operand2 = operands[2];
  }
  emit_insn (gen_rtx_SET (VOIDmode,
	operand0,
	gen_rtx_MINUS (DImode,
	operand1,
	operand2)));
  _val = get_insns ();
  end_sequence ();
  return _val;
}

/* /home/yu/gcc/gcc_powerpc/crosstool-0.42/build/powerpc-7450-linux-gnu/gcc-3.3.1-glibc-2.3.2/gcc-3.3.1/gcc/config/rs6000/rs6000.md:6639 */
extern rtx gen_split_931 PARAMS ((rtx *));
rtx
gen_split_931 (operands)
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
	gen_rtx_ASHIFTRT (DImode,
	operand1,
	GEN_INT (63LL))));
  emit_insn (gen_rtx_SET (VOIDmode,
	operand0,
	gen_rtx_XOR (DImode,
	copy_rtx (operand2),
	copy_rtx (operand1))));
  emit_insn (gen_rtx_SET (VOIDmode,
	copy_rtx (operand0),
	gen_rtx_MINUS (DImode,
	copy_rtx (operand0),
	copy_rtx (operand2))));
  _val = get_insns ();
  end_sequence ();
  return _val;
}

/* /home/yu/gcc/gcc_powerpc/crosstool-0.42/build/powerpc-7450-linux-gnu/gcc-3.3.1-glibc-2.3.2/gcc-3.3.1/gcc/config/rs6000/rs6000.md:6651 */
extern rtx gen_split_932 PARAMS ((rtx *));
rtx
gen_split_932 (operands)
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
	gen_rtx_ASHIFTRT (DImode,
	operand1,
	GEN_INT (63LL))));
  emit_insn (gen_rtx_SET (VOIDmode,
	operand0,
	gen_rtx_XOR (DImode,
	copy_rtx (operand2),
	copy_rtx (operand1))));
  emit_insn (gen_rtx_SET (VOIDmode,
	copy_rtx (operand0),
	gen_rtx_MINUS (DImode,
	copy_rtx (operand2),
	copy_rtx (operand0))));
  _val = get_insns ();
  end_sequence ();
  return _val;
}

/* /home/yu/gcc/gcc_powerpc/crosstool-0.42/build/powerpc-7450-linux-gnu/gcc-3.3.1-glibc-2.3.2/gcc-3.3.1/gcc/config/rs6000/rs6000.md:6663 */
rtx
gen_negdi2 (operand0, operand1)
     rtx operand0;
     rtx operand1;
{
  return gen_rtx_SET (VOIDmode,
	operand0,
	gen_rtx_NEG (DImode,
	operand1));
}

/* /home/yu/gcc/gcc_powerpc/crosstool-0.42/build/powerpc-7450-linux-gnu/gcc-3.3.1-glibc-2.3.2/gcc-3.3.1/gcc/config/rs6000/rs6000.md:6687 */
extern rtx gen_split_934 PARAMS ((rtx *));
rtx
gen_split_934 (operands)
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
	gen_rtx_NEG (DImode,
	operand1)));
  emit_insn (gen_rtx_SET (VOIDmode,
	operand0,
	gen_rtx_COMPARE (CCmode,
	copy_rtx (operand2),
	const0_rtx)));
  _val = get_insns ();
  end_sequence ();
  return _val;
}

/* /home/yu/gcc/gcc_powerpc/crosstool-0.42/build/powerpc-7450-linux-gnu/gcc-3.3.1-glibc-2.3.2/gcc-3.3.1/gcc/config/rs6000/rs6000.md:6713 */
extern rtx gen_split_935 PARAMS ((rtx *));
rtx
gen_split_935 (operands)
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
	gen_rtx_NEG (DImode,
	operand1)));
  emit_insn (gen_rtx_SET (VOIDmode,
	operand2,
	gen_rtx_COMPARE (CCmode,
	copy_rtx (operand0),
	const0_rtx)));
  _val = get_insns ();
  end_sequence ();
  return _val;
}

/* /home/yu/gcc/gcc_powerpc/crosstool-0.42/build/powerpc-7450-linux-gnu/gcc-3.3.1-glibc-2.3.2/gcc-3.3.1/gcc/config/rs6000/rs6000.md:6766 */
rtx
gen_divdi3 (operand0, operand1, operand2)
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
  if (GET_CODE (operands[2]) == CONST_INT
      && INTVAL (operands[2]) > 0
      && exact_log2 (INTVAL (operands[2])) >= 0)
    ;
  else
    operands[2] = force_reg (DImode, operands[2]);
}
    operand0 = operands[0];
    operand1 = operands[1];
    operand2 = operands[2];
  }
  emit_insn (gen_rtx_SET (VOIDmode,
	operand0,
	gen_rtx_DIV (DImode,
	operand1,
	operand2)));
  _val = get_insns ();
  end_sequence ();
  return _val;
}

/* /home/yu/gcc/gcc_powerpc/crosstool-0.42/build/powerpc-7450-linux-gnu/gcc-3.3.1-glibc-2.3.2/gcc-3.3.1/gcc/config/rs6000/rs6000.md:6781 */
rtx
gen_moddi3 (operand0, operand1, operand2)
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
  rtx temp1;
  rtx temp2;

  if (GET_CODE (operands[2]) != CONST_INT
      || INTVAL (operands[2]) <= 0
      || (i = exact_log2 (INTVAL (operands[2]))) < 0)
    FAIL;

  temp1 = gen_reg_rtx (DImode);
  temp2 = gen_reg_rtx (DImode);

  emit_insn (gen_divdi3 (temp1, operands[1], operands[2]));
  emit_insn (gen_ashldi3 (temp2, temp1, GEN_INT (i)));
  emit_insn (gen_subdi3 (operands[0], operands[1], temp2));
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

/* /home/yu/gcc/gcc_powerpc/crosstool-0.42/build/powerpc-7450-linux-gnu/gcc-3.3.1-glibc-2.3.2/gcc-3.3.1/gcc/config/rs6000/rs6000.md:6827 */
extern rtx gen_split_938 PARAMS ((rtx *));
rtx
gen_split_938 (operands)
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
	gen_rtx_DIV (DImode,
	operand1,
	operand2)));
  emit_insn (gen_rtx_SET (VOIDmode,
	operand0,
	gen_rtx_COMPARE (CCmode,
	copy_rtx (operand3),
	const0_rtx)));
  _val = get_insns ();
  end_sequence ();
  return _val;
}

/* /home/yu/gcc/gcc_powerpc/crosstool-0.42/build/powerpc-7450-linux-gnu/gcc-3.3.1-glibc-2.3.2/gcc-3.3.1/gcc/config/rs6000/rs6000.md:6855 */
extern rtx gen_split_939 PARAMS ((rtx *));
rtx
gen_split_939 (operands)
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
	operand0,
	gen_rtx_DIV (DImode,
	operand1,
	operand2)));
  emit_insn (gen_rtx_SET (VOIDmode,
	operand3,
	gen_rtx_COMPARE (CCmode,
	copy_rtx (operand0),
	const0_rtx)));
  _val = get_insns ();
  end_sequence ();
  return _val;
}

/* /home/yu/gcc/gcc_powerpc/crosstool-0.42/build/powerpc-7450-linux-gnu/gcc-3.3.1-glibc-2.3.2/gcc-3.3.1/gcc/config/rs6000/rs6000.md:6906 */
extern rtx gen_split_940 PARAMS ((rtx *));
rtx
gen_split_940 (operands)
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
	gen_rtx_ROTATE (DImode,
	operand1,
	operand2)));
  emit_insn (gen_rtx_SET (VOIDmode,
	operand0,
	gen_rtx_COMPARE (CCmode,
	copy_rtx (operand3),
	const0_rtx)));
  _val = get_insns ();
  end_sequence ();
  return _val;
}

/* /home/yu/gcc/gcc_powerpc/crosstool-0.42/build/powerpc-7450-linux-gnu/gcc-3.3.1-glibc-2.3.2/gcc-3.3.1/gcc/config/rs6000/rs6000.md:6934 */
extern rtx gen_split_941 PARAMS ((rtx *));
rtx
gen_split_941 (operands)
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
	operand0,
	gen_rtx_ROTATE (DImode,
	operand1,
	operand2)));
  emit_insn (gen_rtx_SET (VOIDmode,
	operand3,
	gen_rtx_COMPARE (CCmode,
	copy_rtx (operand0),
	const0_rtx)));
  _val = get_insns ();
  end_sequence ();
  return _val;
}

/* /home/yu/gcc/gcc_powerpc/crosstool-0.42/build/powerpc-7450-linux-gnu/gcc-3.3.1-glibc-2.3.2/gcc-3.3.1/gcc/config/rs6000/rs6000.md:6972 */
extern rtx gen_split_942 PARAMS ((rtx *));
rtx
gen_split_942 (operands)
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
  emit_insn (gen_rtx_SET (VOIDmode,
	operand4,
	gen_rtx_AND (DImode,
	gen_rtx_ROTATE (DImode,
	operand1,
	operand2),
	operand3)));
  emit_insn (gen_rtx_SET (VOIDmode,
	operand0,
	gen_rtx_COMPARE (CCmode,
	copy_rtx (operand4),
	const0_rtx)));
  _val = get_insns ();
  end_sequence ();
  return _val;
}

/* /home/yu/gcc/gcc_powerpc/crosstool-0.42/build/powerpc-7450-linux-gnu/gcc-3.3.1-glibc-2.3.2/gcc-3.3.1/gcc/config/rs6000/rs6000.md:7006 */
extern rtx gen_split_943 PARAMS ((rtx *));
rtx
gen_split_943 (operands)
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
  emit_insn (gen_rtx_SET (VOIDmode,
	operand0,
	gen_rtx_AND (DImode,
	gen_rtx_ROTATE (DImode,
	operand1,
	operand2),
	operand3)));
  emit_insn (gen_rtx_SET (VOIDmode,
	operand4,
	gen_rtx_COMPARE (CCmode,
	copy_rtx (operand0),
	const0_rtx)));
  _val = get_insns ();
  end_sequence ();
  return _val;
}

/* /home/yu/gcc/gcc_powerpc/crosstool-0.42/build/powerpc-7450-linux-gnu/gcc-3.3.1-glibc-2.3.2/gcc-3.3.1/gcc/config/rs6000/rs6000.md:7047 */
extern rtx gen_split_944 PARAMS ((rtx *));
rtx
gen_split_944 (operands)
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
	gen_rtx_ZERO_EXTEND (DImode,
	gen_rtx_SUBREG (QImode,
	gen_rtx_ROTATE (DImode,
	operand1,
	operand2),
	0))));
  emit_insn (gen_rtx_SET (VOIDmode,
	operand0,
	gen_rtx_COMPARE (CCmode,
	copy_rtx (operand3),
	const0_rtx)));
  _val = get_insns ();
  end_sequence ();
  return _val;
}

/* /home/yu/gcc/gcc_powerpc/crosstool-0.42/build/powerpc-7450-linux-gnu/gcc-3.3.1-glibc-2.3.2/gcc-3.3.1/gcc/config/rs6000/rs6000.md:7081 */
extern rtx gen_split_945 PARAMS ((rtx *));
rtx
gen_split_945 (operands)
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
	operand0,
	gen_rtx_ZERO_EXTEND (DImode,
	gen_rtx_SUBREG (QImode,
	gen_rtx_ROTATE (DImode,
	operand1,
	operand2),
	0))));
  emit_insn (gen_rtx_SET (VOIDmode,
	operand3,
	gen_rtx_COMPARE (CCmode,
	copy_rtx (operand0),
	const0_rtx)));
  _val = get_insns ();
  end_sequence ();
  return _val;
}

/* /home/yu/gcc/gcc_powerpc/crosstool-0.42/build/powerpc-7450-linux-gnu/gcc-3.3.1-glibc-2.3.2/gcc-3.3.1/gcc/config/rs6000/rs6000.md:7122 */
extern rtx gen_split_946 PARAMS ((rtx *));
rtx
gen_split_946 (operands)
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
	gen_rtx_ZERO_EXTEND (DImode,
	gen_rtx_SUBREG (HImode,
	gen_rtx_ROTATE (DImode,
	operand1,
	operand2),
	0))));
  emit_insn (gen_rtx_SET (VOIDmode,
	operand0,
	gen_rtx_COMPARE (CCmode,
	copy_rtx (operand3),
	const0_rtx)));
  _val = get_insns ();
  end_sequence ();
  return _val;
}

/* /home/yu/gcc/gcc_powerpc/crosstool-0.42/build/powerpc-7450-linux-gnu/gcc-3.3.1-glibc-2.3.2/gcc-3.3.1/gcc/config/rs6000/rs6000.md:7156 */
extern rtx gen_split_947 PARAMS ((rtx *));
rtx
gen_split_947 (operands)
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
	operand0,
	gen_rtx_ZERO_EXTEND (DImode,
	gen_rtx_SUBREG (HImode,
	gen_rtx_ROTATE (DImode,
	operand1,
	operand2),
	0))));
  emit_insn (gen_rtx_SET (VOIDmode,
	operand3,
	gen_rtx_COMPARE (CCmode,
	copy_rtx (operand0),
	const0_rtx)));
  _val = get_insns ();
  end_sequence ();
  return _val;
}

/* /home/yu/gcc/gcc_powerpc/crosstool-0.42/build/powerpc-7450-linux-gnu/gcc-3.3.1-glibc-2.3.2/gcc-3.3.1/gcc/config/rs6000/rs6000.md:7197 */
extern rtx gen_split_948 PARAMS ((rtx *));
rtx
gen_split_948 (operands)
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
	gen_rtx_ZERO_EXTEND (DImode,
	gen_rtx_SUBREG (SImode,
	gen_rtx_ROTATE (DImode,
	operand1,
	operand2),
	0))));
  emit_insn (gen_rtx_SET (VOIDmode,
	operand0,
	gen_rtx_COMPARE (CCmode,
	copy_rtx (operand3),
	const0_rtx)));
  _val = get_insns ();
  end_sequence ();
  return _val;
}

/* /home/yu/gcc/gcc_powerpc/crosstool-0.42/build/powerpc-7450-linux-gnu/gcc-3.3.1-glibc-2.3.2/gcc-3.3.1/gcc/config/rs6000/rs6000.md:7231 */
extern rtx gen_split_949 PARAMS ((rtx *));
rtx
gen_split_949 (operands)
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
	operand0,
	gen_rtx_ZERO_EXTEND (DImode,
	gen_rtx_SUBREG (SImode,
	gen_rtx_ROTATE (DImode,
	operand1,
	operand2),
	0))));
  emit_insn (gen_rtx_SET (VOIDmode,
	operand3,
	gen_rtx_COMPARE (CCmode,
	copy_rtx (operand0),
	const0_rtx)));
  _val = get_insns ();
  end_sequence ();
  return _val;
}

/* /home/yu/gcc/gcc_powerpc/crosstool-0.42/build/powerpc-7450-linux-gnu/gcc-3.3.1-glibc-2.3.2/gcc-3.3.1/gcc/config/rs6000/rs6000.md:7248 */
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
  if (TARGET_POWERPC64)
    ;
  else if (TARGET_POWER)
    {
      emit_insn (gen_ashldi3_power (operands[0], operands[1], operands[2]));
      DONE;
    }
  else
    FAIL;
}
    operand0 = operands[0];
    operand1 = operands[1];
    operand2 = operands[2];
  }
  emit_insn (gen_rtx_SET (VOIDmode,
	operand0,
	gen_rtx_ASHIFT (DImode,
	operand1,
	operand2)));
  _val = get_insns ();
  end_sequence ();
  return _val;
}

/* /home/yu/gcc/gcc_powerpc/crosstool-0.42/build/powerpc-7450-linux-gnu/gcc-3.3.1-glibc-2.3.2/gcc-3.3.1/gcc/config/rs6000/rs6000.md:7287 */
extern rtx gen_split_951 PARAMS ((rtx *));
rtx
gen_split_951 (operands)
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
	gen_rtx_ASHIFT (DImode,
	operand1,
	operand2)));
  emit_insn (gen_rtx_SET (VOIDmode,
	operand0,
	gen_rtx_COMPARE (CCmode,
	copy_rtx (operand3),
	const0_rtx)));
  _val = get_insns ();
  end_sequence ();
  return _val;
}

/* /home/yu/gcc/gcc_powerpc/crosstool-0.42/build/powerpc-7450-linux-gnu/gcc-3.3.1-glibc-2.3.2/gcc-3.3.1/gcc/config/rs6000/rs6000.md:7315 */
extern rtx gen_split_952 PARAMS ((rtx *));
rtx
gen_split_952 (operands)
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
	operand0,
	gen_rtx_ASHIFT (DImode,
	operand1,
	operand2)));
  emit_insn (gen_rtx_SET (VOIDmode,
	operand3,
	gen_rtx_COMPARE (CCmode,
	copy_rtx (operand0),
	const0_rtx)));
  _val = get_insns ();
  end_sequence ();
  return _val;
}

/* /home/yu/gcc/gcc_powerpc/crosstool-0.42/build/powerpc-7450-linux-gnu/gcc-3.3.1-glibc-2.3.2/gcc-3.3.1/gcc/config/rs6000/rs6000.md:7353 */
extern rtx gen_split_953 PARAMS ((rtx *));
rtx
gen_split_953 (operands)
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
  emit_insn (gen_rtx_SET (VOIDmode,
	operand4,
	gen_rtx_AND (DImode,
	gen_rtx_ASHIFT (DImode,
	operand1,
	operand2),
	operand3)));
  emit_insn (gen_rtx_SET (VOIDmode,
	operand0,
	gen_rtx_COMPARE (CCmode,
	copy_rtx (operand4),
	const0_rtx)));
  _val = get_insns ();
  end_sequence ();
  return _val;
}

/* /home/yu/gcc/gcc_powerpc/crosstool-0.42/build/powerpc-7450-linux-gnu/gcc-3.3.1-glibc-2.3.2/gcc-3.3.1/gcc/config/rs6000/rs6000.md:7387 */
extern rtx gen_split_954 PARAMS ((rtx *));
rtx
gen_split_954 (operands)
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
  emit_insn (gen_rtx_SET (VOIDmode,
	operand0,
	gen_rtx_AND (DImode,
	gen_rtx_ASHIFT (DImode,
	operand1,
	operand2),
	operand3)));
  emit_insn (gen_rtx_SET (VOIDmode,
	operand4,
	gen_rtx_COMPARE (CCmode,
	copy_rtx (operand0),
	const0_rtx)));
  _val = get_insns ();
  end_sequence ();
  return _val;
}

/* /home/yu/gcc/gcc_powerpc/crosstool-0.42/build/powerpc-7450-linux-gnu/gcc-3.3.1-glibc-2.3.2/gcc-3.3.1/gcc/config/rs6000/rs6000.md:7429 */
extern rtx gen_split_955 PARAMS ((rtx *));
rtx
gen_split_955 (operands)
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
  emit_insn (gen_rtx_SET (VOIDmode,
	operand4,
	gen_rtx_AND (DImode,
	gen_rtx_ASHIFT (DImode,
	operand1,
	operand2),
	operand3)));
  emit_insn (gen_rtx_SET (VOIDmode,
	operand0,
	gen_rtx_COMPARE (CCmode,
	copy_rtx (operand4),
	const0_rtx)));
  _val = get_insns ();
  end_sequence ();
  return _val;
}

/* /home/yu/gcc/gcc_powerpc/crosstool-0.42/build/powerpc-7450-linux-gnu/gcc-3.3.1-glibc-2.3.2/gcc-3.3.1/gcc/config/rs6000/rs6000.md:7463 */
extern rtx gen_split_956 PARAMS ((rtx *));
rtx
gen_split_956 (operands)
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
  emit_insn (gen_rtx_SET (VOIDmode,
	operand0,
	gen_rtx_AND (DImode,
	gen_rtx_ASHIFT (DImode,
	operand1,
	operand2),
	operand3)));
  emit_insn (gen_rtx_SET (VOIDmode,
	operand4,
	gen_rtx_COMPARE (CCmode,
	copy_rtx (operand0),
	const0_rtx)));
  _val = get_insns ();
  end_sequence ();
  return _val;
}

/* /home/yu/gcc/gcc_powerpc/crosstool-0.42/build/powerpc-7450-linux-gnu/gcc-3.3.1-glibc-2.3.2/gcc-3.3.1/gcc/config/rs6000/rs6000.md:7482 */
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
  if (TARGET_POWERPC64)
    ;
  else if (TARGET_POWER)
    {
      emit_insn (gen_lshrdi3_power (operands[0], operands[1], operands[2]));
      DONE;
    }
  else
    FAIL;
}
    operand0 = operands[0];
    operand1 = operands[1];
    operand2 = operands[2];
  }
  emit_insn (gen_rtx_SET (VOIDmode,
	operand0,
	gen_rtx_LSHIFTRT (DImode,
	operand1,
	operand2)));
  _val = get_insns ();
  end_sequence ();
  return _val;
}

/* /home/yu/gcc/gcc_powerpc/crosstool-0.42/build/powerpc-7450-linux-gnu/gcc-3.3.1-glibc-2.3.2/gcc-3.3.1/gcc/config/rs6000/rs6000.md:7520 */
extern rtx gen_split_958 PARAMS ((rtx *));
rtx
gen_split_958 (operands)
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
	gen_rtx_LSHIFTRT (DImode,
	operand1,
	operand2)));
  emit_insn (gen_rtx_SET (VOIDmode,
	operand0,
	gen_rtx_COMPARE (CCmode,
	copy_rtx (operand3),
	const0_rtx)));
  _val = get_insns ();
  end_sequence ();
  return _val;
}

/* /home/yu/gcc/gcc_powerpc/crosstool-0.42/build/powerpc-7450-linux-gnu/gcc-3.3.1-glibc-2.3.2/gcc-3.3.1/gcc/config/rs6000/rs6000.md:7548 */
extern rtx gen_split_959 PARAMS ((rtx *));
rtx
gen_split_959 (operands)
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
	operand0,
	gen_rtx_LSHIFTRT (DImode,
	operand1,
	operand2)));
  emit_insn (gen_rtx_SET (VOIDmode,
	operand3,
	gen_rtx_COMPARE (CCmode,
	copy_rtx (operand0),
	const0_rtx)));
  _val = get_insns ();
  end_sequence ();
  return _val;
}

/* /home/yu/gcc/gcc_powerpc/crosstool-0.42/build/powerpc-7450-linux-gnu/gcc-3.3.1-glibc-2.3.2/gcc-3.3.1/gcc/config/rs6000/rs6000.md:7563 */
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
  if (TARGET_POWERPC64)
    ;
  else if (TARGET_POWER && GET_CODE (operands[2]) == CONST_INT)
    {
      emit_insn (gen_ashrdi3_power (operands[0], operands[1], operands[2]));
      DONE;
    }
  else if (TARGET_32BIT && GET_CODE (operands[2]) == CONST_INT)
    {
      emit_insn (gen_ashrdi3_no_power (operands[0], operands[1], operands[2]));
      DONE;
    }
  else
    FAIL;
}
    operand0 = operands[0];
    operand1 = operands[1];
    operand2 = operands[2];
  }
  emit_insn (gen_rtx_SET (VOIDmode,
	operand0,
	gen_rtx_ASHIFTRT (DImode,
	operand1,
	operand2)));
  _val = get_insns ();
  end_sequence ();
  return _val;
}

/* /home/yu/gcc/gcc_powerpc/crosstool-0.42/build/powerpc-7450-linux-gnu/gcc-3.3.1-glibc-2.3.2/gcc-3.3.1/gcc/config/rs6000/rs6000.md:7606 */
extern rtx gen_split_961 PARAMS ((rtx *));
rtx
gen_split_961 (operands)
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
	gen_rtx_ASHIFTRT (DImode,
	operand1,
	operand2)));
  emit_insn (gen_rtx_SET (VOIDmode,
	operand0,
	gen_rtx_COMPARE (CCmode,
	copy_rtx (operand3),
	const0_rtx)));
  _val = get_insns ();
  end_sequence ();
  return _val;
}

/* /home/yu/gcc/gcc_powerpc/crosstool-0.42/build/powerpc-7450-linux-gnu/gcc-3.3.1-glibc-2.3.2/gcc-3.3.1/gcc/config/rs6000/rs6000.md:7634 */
extern rtx gen_split_962 PARAMS ((rtx *));
rtx
gen_split_962 (operands)
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
	operand0,
	gen_rtx_ASHIFTRT (DImode,
	operand1,
	operand2)));
  emit_insn (gen_rtx_SET (VOIDmode,
	operand3,
	gen_rtx_COMPARE (CCmode,
	copy_rtx (operand0),
	const0_rtx)));
  _val = get_insns ();
  end_sequence ();
  return _val;
}

/* /home/yu/gcc/gcc_powerpc/crosstool-0.42/build/powerpc-7450-linux-gnu/gcc-3.3.1-glibc-2.3.2/gcc-3.3.1/gcc/config/rs6000/rs6000.md:7663 */
extern rtx gen_split_963 PARAMS ((rtx *));
rtx
gen_split_963 (operands)
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
  build_mask64_2_operands (operands[2], &operands[4]);
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
	gen_rtx_AND (DImode,
	gen_rtx_ROTATE (DImode,
	operand1,
	operand4),
	operand5)));
  emit_insn (gen_rtx_SET (VOIDmode,
	copy_rtx (operand0),
	gen_rtx_AND (DImode,
	gen_rtx_ROTATE (DImode,
	copy_rtx (operand0),
	operand6),
	operand7)));
  _val = get_insns ();
  end_sequence ();
  return _val;
}

/* /home/yu/gcc/gcc_powerpc/crosstool-0.42/build/powerpc-7450-linux-gnu/gcc-3.3.1-glibc-2.3.2/gcc-3.3.1/gcc/config/rs6000/rs6000.md:7706 */
extern rtx gen_split_964 PARAMS ((rtx *));
rtx
gen_split_964 (operands)
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
	gen_rtvec (2,
		gen_rtx_SET (VOIDmode,
	operand3,
	gen_rtx_AND (DImode,
	operand1,
	operand2)),
		gen_rtx_CLOBBER (VOIDmode,
	operand4))));
  emit_insn (gen_rtx_SET (VOIDmode,
	operand0,
	gen_rtx_COMPARE (CCmode,
	copy_rtx (operand3),
	const0_rtx)));
  _val = get_insns ();
  end_sequence ();
  return _val;
}

/* /home/yu/gcc/gcc_powerpc/crosstool-0.42/build/powerpc-7450-linux-gnu/gcc-3.3.1-glibc-2.3.2/gcc-3.3.1/gcc/config/rs6000/rs6000.md:7723 */
extern rtx gen_split_965 PARAMS ((rtx *));
rtx
gen_split_965 (operands)
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

{
  build_mask64_2_operands (operands[2], &operands[5]);
}
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
	operand3,
	gen_rtx_AND (DImode,
	gen_rtx_ROTATE (DImode,
	operand1,
	operand5),
	operand6)));
  emit (gen_rtx_PARALLEL (VOIDmode,
	gen_rtvec (2,
		gen_rtx_SET (VOIDmode,
	operand0,
	gen_rtx_COMPARE (CCmode,
	gen_rtx_AND (DImode,
	gen_rtx_ROTATE (DImode,
	copy_rtx (operand3),
	operand7),
	operand8),
	const0_rtx)),
		gen_rtx_CLOBBER (VOIDmode,
	copy_rtx (operand3)))));
  _val = get_insns ();
  end_sequence ();
  return _val;
}

/* /home/yu/gcc/gcc_powerpc/crosstool-0.42/build/powerpc-7450-linux-gnu/gcc-3.3.1-glibc-2.3.2/gcc-3.3.1/gcc/config/rs6000/rs6000.md:7771 */
extern rtx gen_split_966 PARAMS ((rtx *));
rtx
gen_split_966 (operands)
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
	gen_rtvec (2,
		gen_rtx_SET (VOIDmode,
	operand0,
	gen_rtx_AND (DImode,
	operand1,
	operand2)),
		gen_rtx_CLOBBER (VOIDmode,
	operand4))));
  emit_insn (gen_rtx_SET (VOIDmode,
	operand3,
	gen_rtx_COMPARE (CCmode,
	copy_rtx (operand0),
	const0_rtx)));
  _val = get_insns ();
  end_sequence ();
  return _val;
}

/* /home/yu/gcc/gcc_powerpc/crosstool-0.42/build/powerpc-7450-linux-gnu/gcc-3.3.1-glibc-2.3.2/gcc-3.3.1/gcc/config/rs6000/rs6000.md:7788 */
extern rtx gen_split_967 PARAMS ((rtx *));
rtx
gen_split_967 (operands)
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

{
  build_mask64_2_operands (operands[2], &operands[5]);
}
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
	operand0,
	gen_rtx_AND (DImode,
	gen_rtx_ROTATE (DImode,
	operand1,
	operand5),
	operand6)));
  emit (gen_rtx_PARALLEL (VOIDmode,
	gen_rtvec (2,
		gen_rtx_SET (VOIDmode,
	operand3,
	gen_rtx_COMPARE (CCmode,
	gen_rtx_AND (DImode,
	gen_rtx_ROTATE (DImode,
	copy_rtx (operand0),
	operand7),
	operand8),
	const0_rtx)),
		gen_rtx_SET (VOIDmode,
	copy_rtx (operand0),
	gen_rtx_AND (DImode,
	gen_rtx_ROTATE (DImode,
	copy_rtx (operand0),
	copy_rtx (operand7)),
	copy_rtx (operand8))))));
  _val = get_insns ();
  end_sequence ();
  return _val;
}

/* /home/yu/gcc/gcc_powerpc/crosstool-0.42/build/powerpc-7450-linux-gnu/gcc-3.3.1-glibc-2.3.2/gcc-3.3.1/gcc/config/rs6000/rs6000.md:7817 */
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

{
  if (non_logical_cint_operand (operands[2], DImode))
    {
      HOST_WIDE_INT value;
      rtx tmp = ((no_new_pseudos || rtx_equal_p (operands[0], operands[1]))
		 ? operands[0] : gen_reg_rtx (DImode));

      if (GET_CODE (operands[2]) == CONST_INT)
        {
          value = INTVAL (operands[2]);
	  emit_insn (gen_iordi3 (tmp, operands[1],
				 GEN_INT (value & (~ (HOST_WIDE_INT) 0xffff))));
	}
      else
        {
	  value = CONST_DOUBLE_LOW (operands[2]);
	  emit_insn (gen_iordi3 (tmp, operands[1],
				 immed_double_const (value
						     & (~ (HOST_WIDE_INT) 0xffff),
						     0, DImode)));
	}

      emit_insn (gen_iordi3 (operands[0], tmp, GEN_INT (value & 0xffff)));
      DONE;
    }
}
    operand0 = operands[0];
    operand1 = operands[1];
    operand2 = operands[2];
  }
  emit_insn (gen_rtx_SET (VOIDmode,
	operand0,
	gen_rtx_IOR (DImode,
	operand1,
	operand2)));
  _val = get_insns ();
  end_sequence ();
  return _val;
}

/* /home/yu/gcc/gcc_powerpc/crosstool-0.42/build/powerpc-7450-linux-gnu/gcc-3.3.1-glibc-2.3.2/gcc-3.3.1/gcc/config/rs6000/rs6000.md:7850 */
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

{
  if (non_logical_cint_operand (operands[2], DImode))
    {
      HOST_WIDE_INT value;
      rtx tmp = ((no_new_pseudos || rtx_equal_p (operands[0], operands[1]))
		 ? operands[0] : gen_reg_rtx (DImode));

      if (GET_CODE (operands[2]) == CONST_INT)
        {
          value = INTVAL (operands[2]);
	  emit_insn (gen_xordi3 (tmp, operands[1],
				 GEN_INT (value & (~ (HOST_WIDE_INT) 0xffff))));
	}
      else
        {
	  value = CONST_DOUBLE_LOW (operands[2]);
	  emit_insn (gen_xordi3 (tmp, operands[1],
				 immed_double_const (value
						     & (~ (HOST_WIDE_INT) 0xffff),
						     0, DImode)));
	}

      emit_insn (gen_xordi3 (operands[0], tmp, GEN_INT (value & 0xffff)));
      DONE;
    }
}
    operand0 = operands[0];
    operand1 = operands[1];
    operand2 = operands[2];
  }
  emit_insn (gen_rtx_SET (VOIDmode,
	operand0,
	gen_rtx_XOR (DImode,
	operand1,
	operand2)));
  _val = get_insns ();
  end_sequence ();
  return _val;
}

/* /home/yu/gcc/gcc_powerpc/crosstool-0.42/build/powerpc-7450-linux-gnu/gcc-3.3.1-glibc-2.3.2/gcc-3.3.1/gcc/config/rs6000/rs6000.md:7908 */
extern rtx gen_split_970 PARAMS ((rtx *));
rtx
gen_split_970 (operands)
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
  emit_insn (gen_rtx_SET (VOIDmode,
	operand3,
	operand4));
  emit_insn (gen_rtx_SET (VOIDmode,
	operand0,
	gen_rtx_COMPARE (CCmode,
	copy_rtx (operand3),
	const0_rtx)));
  _val = get_insns ();
  end_sequence ();
  return _val;
}

/* /home/yu/gcc/gcc_powerpc/crosstool-0.42/build/powerpc-7450-linux-gnu/gcc-3.3.1-glibc-2.3.2/gcc-3.3.1/gcc/config/rs6000/rs6000.md:7937 */
extern rtx gen_split_971 PARAMS ((rtx *));
rtx
gen_split_971 (operands)
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
  emit_insn (gen_rtx_SET (VOIDmode,
	operand0,
	operand4));
  emit_insn (gen_rtx_SET (VOIDmode,
	operand3,
	gen_rtx_COMPARE (CCmode,
	copy_rtx (operand0),
	const0_rtx)));
  _val = get_insns ();
  end_sequence ();
  return _val;
}

/* /home/yu/gcc/gcc_powerpc/crosstool-0.42/build/powerpc-7450-linux-gnu/gcc-3.3.1-glibc-2.3.2/gcc-3.3.1/gcc/config/rs6000/rs6000.md:7955 */
extern rtx gen_split_972 PARAMS ((rtx *));
rtx
gen_split_972 (operands)
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

{
  rtx i3,i4;
  
  if (GET_CODE (operands[2]) == CONST_DOUBLE)
    {
      HOST_WIDE_INT value = CONST_DOUBLE_LOW (operands[2]);
      i3 = immed_double_const (value & (~ (HOST_WIDE_INT) 0xffff),
					0, DImode);
      i4 = GEN_INT (value & 0xffff);
    }
  else
    {
      i3 = GEN_INT (INTVAL (operands[2])
			     & (~ (HOST_WIDE_INT) 0xffff));
      i4 = GEN_INT (INTVAL (operands[2]) & 0xffff);
    }
  operands[4] = gen_rtx (GET_CODE (operands[3]), DImode,
			 operands[1], i3);
  operands[5] = gen_rtx (GET_CODE (operands[3]), DImode,
			 operands[0], i4);
}
  operand0 = operands[0];
  operand1 = operands[1];
  operand2 = operands[2];
  operand3 = operands[3];
  operand4 = operands[4];
  operand5 = operands[5];
  emit_insn (gen_rtx_SET (VOIDmode,
	operand0,
	operand4));
  emit_insn (gen_rtx_SET (VOIDmode,
	copy_rtx (operand0),
	operand5));
  _val = get_insns ();
  end_sequence ();
  return _val;
}

/* /home/yu/gcc/gcc_powerpc/crosstool-0.42/build/powerpc-7450-linux-gnu/gcc-3.3.1-glibc-2.3.2/gcc-3.3.1/gcc/config/rs6000/rs6000.md:8008 */
extern rtx gen_split_973 PARAMS ((rtx *));
rtx
gen_split_973 (operands)
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
  emit_insn (gen_rtx_SET (VOIDmode,
	operand3,
	operand4));
  emit_insn (gen_rtx_SET (VOIDmode,
	operand0,
	gen_rtx_COMPARE (CCmode,
	copy_rtx (operand3),
	const0_rtx)));
  _val = get_insns ();
  end_sequence ();
  return _val;
}

/* /home/yu/gcc/gcc_powerpc/crosstool-0.42/build/powerpc-7450-linux-gnu/gcc-3.3.1-glibc-2.3.2/gcc-3.3.1/gcc/config/rs6000/rs6000.md:8037 */
extern rtx gen_split_974 PARAMS ((rtx *));
rtx
gen_split_974 (operands)
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
  emit_insn (gen_rtx_SET (VOIDmode,
	operand0,
	operand4));
  emit_insn (gen_rtx_SET (VOIDmode,
	operand3,
	gen_rtx_COMPARE (CCmode,
	copy_rtx (operand0),
	const0_rtx)));
  _val = get_insns ();
  end_sequence ();
  return _val;
}

/* /home/yu/gcc/gcc_powerpc/crosstool-0.42/build/powerpc-7450-linux-gnu/gcc-3.3.1-glibc-2.3.2/gcc-3.3.1/gcc/config/rs6000/rs6000.md:8074 */
extern rtx gen_split_975 PARAMS ((rtx *));
rtx
gen_split_975 (operands)
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
  emit_insn (gen_rtx_SET (VOIDmode,
	operand3,
	operand4));
  emit_insn (gen_rtx_SET (VOIDmode,
	operand0,
	gen_rtx_COMPARE (CCmode,
	copy_rtx (operand3),
	const0_rtx)));
  _val = get_insns ();
  end_sequence ();
  return _val;
}

/* /home/yu/gcc/gcc_powerpc/crosstool-0.42/build/powerpc-7450-linux-gnu/gcc-3.3.1-glibc-2.3.2/gcc-3.3.1/gcc/config/rs6000/rs6000.md:8103 */
extern rtx gen_split_976 PARAMS ((rtx *));
rtx
gen_split_976 (operands)
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
  emit_insn (gen_rtx_SET (VOIDmode,
	operand0,
	operand4));
  emit_insn (gen_rtx_SET (VOIDmode,
	operand3,
	gen_rtx_COMPARE (CCmode,
	copy_rtx (operand0),
	const0_rtx)));
  _val = get_insns ();
  end_sequence ();
  return _val;
}

/* /home/yu/gcc/gcc_powerpc/crosstool-0.42/build/powerpc-7450-linux-gnu/gcc-3.3.1-glibc-2.3.2/gcc-3.3.1/gcc/config/rs6000/rs6000.md:8157 */
rtx
gen_movsi_got (operand0, operand1)
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
  if (GET_CODE (operands[1]) == CONST)
    {
      rtx offset = const0_rtx;
      HOST_WIDE_INT value;

      operands[1] = eliminate_constant_term (XEXP (operands[1], 0), &offset);
      value = INTVAL (offset);
      if (value != 0)
	{
	  rtx tmp = (no_new_pseudos ? operands[0] : gen_reg_rtx (Pmode));
	  emit_insn (gen_movsi_got (tmp, operands[1]));
	  emit_insn (gen_addsi3 (operands[0], tmp, offset));
	  DONE;
	}
    }

  operands[2] = rs6000_got_register (operands[1]);
}
    operand0 = operands[0];
    operand1 = operands[1];
    operand2 = operands[2];
  }
  emit_insn (gen_rtx_SET (VOIDmode,
	operand0,
	gen_rtx_UNSPEC (SImode,
	gen_rtvec (2,
		operand1,
		operand2),
	8)));
  _val = get_insns ();
  end_sequence ();
  return _val;
}

/* /home/yu/gcc/gcc_powerpc/crosstool-0.42/build/powerpc-7450-linux-gnu/gcc-3.3.1-glibc-2.3.2/gcc-3.3.1/gcc/config/rs6000/rs6000.md:8193 */
extern rtx gen_split_978 PARAMS ((rtx *));
rtx
gen_split_978 (operands)
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
	operand2));
  emit_insn (gen_rtx_SET (VOIDmode,
	copy_rtx (operand0),
	gen_rtx_UNSPEC (SImode,
	gen_rtvec (2,
		operand1,
		copy_rtx (operand0)),
	8)));
  _val = get_insns ();
  end_sequence ();
  return _val;
}

/* /home/yu/gcc/gcc_powerpc/crosstool-0.42/build/powerpc-7450-linux-gnu/gcc-3.3.1-glibc-2.3.2/gcc-3.3.1/gcc/config/rs6000/rs6000.md:8208 */
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
{ rs6000_emit_move (operands[0], operands[1], SImode); DONE; }
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

/* /home/yu/gcc/gcc_powerpc/crosstool-0.42/build/powerpc-7450-linux-gnu/gcc-3.3.1-glibc-2.3.2/gcc-3.3.1/gcc/config/rs6000/rs6000.md:8319 */
extern rtx gen_split_980 PARAMS ((rtx *));
rtx
gen_split_980 (operands)
      rtx *operands;
{
  rtx operand0;
  rtx operand1;
  rtx operand2;
  rtx operand3;
  rtx _val = 0;
  start_sequence ();

{ rtx tem = rs6000_emit_set_const (operands[0], SImode, operands[1], 2);

  if (tem == operands[0])
    DONE;
  else
    FAIL;
}
  operand0 = operands[0];
  operand1 = operands[1];
  operand2 = operands[2];
  operand3 = operands[3];
  emit_insn (gen_rtx_SET (VOIDmode,
	operand0,
	operand2));
  emit_insn (gen_rtx_SET (VOIDmode,
	copy_rtx (operand0),
	gen_rtx_IOR (SImode,
	copy_rtx (operand0),
	operand3)));
  _val = get_insns ();
  end_sequence ();
  return _val;
}

/* /home/yu/gcc/gcc_powerpc/crosstool-0.42/build/powerpc-7450-linux-gnu/gcc-3.3.1-glibc-2.3.2/gcc-3.3.1/gcc/config/rs6000/rs6000.md:8350 */
extern rtx gen_split_981 PARAMS ((rtx *));
rtx
gen_split_981 (operands)
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
	operand1));
  emit_insn (gen_rtx_SET (VOIDmode,
	operand2,
	gen_rtx_COMPARE (CCmode,
	copy_rtx (operand0),
	const0_rtx)));
  _val = get_insns ();
  end_sequence ();
  return _val;
}

/* /home/yu/gcc/gcc_powerpc/crosstool-0.42/build/powerpc-7450-linux-gnu/gcc-3.3.1-glibc-2.3.2/gcc-3.3.1/gcc/config/rs6000/rs6000.md:8362 */
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
{ rs6000_emit_move (operands[0], operands[1], HImode); DONE; }
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

/* /home/yu/gcc/gcc_powerpc/crosstool-0.42/build/powerpc-7450-linux-gnu/gcc-3.3.1-glibc-2.3.2/gcc-3.3.1/gcc/config/rs6000/rs6000.md:8384 */
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
{ rs6000_emit_move (operands[0], operands[1], QImode); DONE; }
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

/* /home/yu/gcc/gcc_powerpc/crosstool-0.42/build/powerpc-7450-linux-gnu/gcc-3.3.1-glibc-2.3.2/gcc-3.3.1/gcc/config/rs6000/rs6000.md:8409 */
rtx
gen_movcc (operand0, operand1)
     rtx operand0;
     rtx operand1;
{
  return gen_rtx_SET (VOIDmode,
	operand0,
	operand1);
}

/* /home/yu/gcc/gcc_powerpc/crosstool-0.42/build/powerpc-7450-linux-gnu/gcc-3.3.1-glibc-2.3.2/gcc-3.3.1/gcc/config/rs6000/rs6000.md:8440 */
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
{ rs6000_emit_move (operands[0], operands[1], SFmode); DONE; }
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

/* /home/yu/gcc/gcc_powerpc/crosstool-0.42/build/powerpc-7450-linux-gnu/gcc-3.3.1-glibc-2.3.2/gcc-3.3.1/gcc/config/rs6000/rs6000.md:8446 */
extern rtx gen_split_986 PARAMS ((rtx *));
rtx
gen_split_986 (operands)
      rtx *operands;
{
  rtx operand0;
  rtx operand1;
  rtx operand2;
  rtx operand3;
  rtx _val = 0;
  start_sequence ();

{
  long l;
  REAL_VALUE_TYPE rv;

  REAL_VALUE_FROM_CONST_DOUBLE (rv, operands[1]);
  REAL_VALUE_TO_TARGET_SINGLE (rv, l);

  if (! TARGET_POWERPC64)
    operands[2] = operand_subword (operands[0], 0, 0, SFmode);
  else
    operands[2] = gen_lowpart (SImode, operands[0]);

  operands[3] = gen_int_mode (l, SImode);
}
  operand0 = operands[0];
  operand1 = operands[1];
  operand2 = operands[2];
  operand3 = operands[3];
  emit_insn (gen_rtx_SET (VOIDmode,
	operand2,
	operand3));
  _val = get_insns ();
  end_sequence ();
  return _val;
}

/* /home/yu/gcc/gcc_powerpc/crosstool-0.42/build/powerpc-7450-linux-gnu/gcc-3.3.1-glibc-2.3.2/gcc-3.3.1/gcc/config/rs6000/rs6000.md:8514 */
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
{ rs6000_emit_move (operands[0], operands[1], DFmode); DONE; }
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

/* /home/yu/gcc/gcc_powerpc/crosstool-0.42/build/powerpc-7450-linux-gnu/gcc-3.3.1-glibc-2.3.2/gcc-3.3.1/gcc/config/rs6000/rs6000.md:8520 */
extern rtx gen_split_988 PARAMS ((rtx *));
rtx
gen_split_988 (operands)
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
  int endian = (WORDS_BIG_ENDIAN == 0);
  HOST_WIDE_INT value = INTVAL (operands[1]);

  operands[2] = operand_subword (operands[0], endian, 0, DFmode);
  operands[3] = operand_subword (operands[0], 1 - endian, 0, DFmode);
#if HOST_BITS_PER_WIDE_INT == 32
  operands[4] = (value & 0x80000000) ? constm1_rtx : const0_rtx;
#else
  operands[4] = GEN_INT (value >> 32);
  operands[1] = GEN_INT (((value & 0xffffffff) ^ 0x80000000) - 0x80000000);
#endif
}
  operand0 = operands[0];
  operand1 = operands[1];
  operand2 = operands[2];
  operand3 = operands[3];
  operand4 = operands[4];
  emit_insn (gen_rtx_SET (VOIDmode,
	operand2,
	operand4));
  emit_insn (gen_rtx_SET (VOIDmode,
	operand3,
	operand1));
  _val = get_insns ();
  end_sequence ();
  return _val;
}

/* /home/yu/gcc/gcc_powerpc/crosstool-0.42/build/powerpc-7450-linux-gnu/gcc-3.3.1-glibc-2.3.2/gcc-3.3.1/gcc/config/rs6000/rs6000.md:8545 */
extern rtx gen_split_989 PARAMS ((rtx *));
rtx
gen_split_989 (operands)
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

{
  int endian = (WORDS_BIG_ENDIAN == 0);
  long l[2];
  REAL_VALUE_TYPE rv;

  REAL_VALUE_FROM_CONST_DOUBLE (rv, operands[1]);
  REAL_VALUE_TO_TARGET_DOUBLE (rv, l);

  operands[2] = operand_subword (operands[0], endian, 0, DFmode);
  operands[3] = operand_subword (operands[0], 1 - endian, 0, DFmode);
  operands[4] = gen_int_mode (l[endian], SImode);
  operands[5] = gen_int_mode (l[1 - endian], SImode);
}
  operand0 = operands[0];
  operand1 = operands[1];
  operand2 = operands[2];
  operand3 = operands[3];
  operand4 = operands[4];
  operand5 = operands[5];
  emit_insn (gen_rtx_SET (VOIDmode,
	operand2,
	operand4));
  emit_insn (gen_rtx_SET (VOIDmode,
	operand3,
	operand5));
  _val = get_insns ();
  end_sequence ();
  return _val;
}

/* /home/yu/gcc/gcc_powerpc/crosstool-0.42/build/powerpc-7450-linux-gnu/gcc-3.3.1-glibc-2.3.2/gcc-3.3.1/gcc/config/rs6000/rs6000.md:8570 */
extern rtx gen_split_990 PARAMS ((rtx *));
rtx
gen_split_990 (operands)
      rtx *operands;
{
  rtx operand0;
  rtx operand1;
  rtx operand2;
  rtx operand3;
  rtx _val = 0;
  start_sequence ();

{
  int endian = (WORDS_BIG_ENDIAN == 0);
  long l[2];
  REAL_VALUE_TYPE rv;
  HOST_WIDE_INT val;

  REAL_VALUE_FROM_CONST_DOUBLE (rv, operands[1]);
  REAL_VALUE_TO_TARGET_DOUBLE (rv, l);

  operands[2] = gen_lowpart (DImode, operands[0]);
  /* HIGHPART is lower memory address when WORDS_BIG_ENDIAN.  */
#if HOST_BITS_PER_WIDE_INT >= 64
  val = ((HOST_WIDE_INT)(unsigned long)l[endian] << 32
         | ((HOST_WIDE_INT)(unsigned long)l[1 - endian]));

  operands[3] = gen_int_mode (val, DImode);
#else
  operands[3] = immed_double_const (l[1 - endian], l[endian], DImode);
#endif
}
  operand0 = operands[0];
  operand1 = operands[1];
  operand2 = operands[2];
  operand3 = operands[3];
  emit_insn (gen_rtx_SET (VOIDmode,
	operand2,
	operand3));
  _val = get_insns ();
  end_sequence ();
  return _val;
}

/* /home/yu/gcc/gcc_powerpc/crosstool-0.42/build/powerpc-7450-linux-gnu/gcc-3.3.1-glibc-2.3.2/gcc-3.3.1/gcc/config/rs6000/rs6000.md:8780 */
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
{ rs6000_emit_move (operands[0], operands[1], TFmode); DONE; }
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

/* /home/yu/gcc/gcc_powerpc/crosstool-0.42/build/powerpc-7450-linux-gnu/gcc-3.3.1-glibc-2.3.2/gcc-3.3.1/gcc/config/rs6000/rs6000.md:8821 */
extern rtx gen_split_992 PARAMS ((rtx *));
rtx
gen_split_992 (operands)
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
  rtx operand9;
  rtx _val = 0;
  start_sequence ();

{
  long l[4];
  REAL_VALUE_TYPE rv;

  REAL_VALUE_FROM_CONST_DOUBLE (rv, operands[1]);
  REAL_VALUE_TO_TARGET_LONG_DOUBLE (rv, l);

  operands[2] = operand_subword (operands[0], 0, 0, TFmode);
  operands[3] = operand_subword (operands[0], 1, 0, TFmode);
  operands[4] = operand_subword (operands[0], 2, 0, TFmode);
  operands[5] = operand_subword (operands[0], 3, 0, TFmode);
  operands[6] = gen_int_mode (l[0], SImode);
  operands[7] = gen_int_mode (l[1], SImode);
  operands[8] = gen_int_mode (l[2], SImode);
  operands[9] = gen_int_mode (l[3], SImode);
}
  operand0 = operands[0];
  operand1 = operands[1];
  operand2 = operands[2];
  operand3 = operands[3];
  operand4 = operands[4];
  operand5 = operands[5];
  operand6 = operands[6];
  operand7 = operands[7];
  operand8 = operands[8];
  operand9 = operands[9];
  emit_insn (gen_rtx_SET (VOIDmode,
	operand2,
	operand6));
  emit_insn (gen_rtx_SET (VOIDmode,
	operand3,
	operand7));
  emit_insn (gen_rtx_SET (VOIDmode,
	operand4,
	operand8));
  emit_insn (gen_rtx_SET (VOIDmode,
	operand5,
	operand9));
  _val = get_insns ();
  end_sequence ();
  return _val;
}

/* /home/yu/gcc/gcc_powerpc/crosstool-0.42/build/powerpc-7450-linux-gnu/gcc-3.3.1-glibc-2.3.2/gcc-3.3.1/gcc/config/rs6000/rs6000.md:8853 */
extern rtx gen_split_993 PARAMS ((rtx *));
rtx
gen_split_993 (operands)
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

{
  long l[4];
  REAL_VALUE_TYPE rv;
  HOST_WIDE_INT val;

  REAL_VALUE_FROM_CONST_DOUBLE (rv, operands[1]);
  REAL_VALUE_TO_TARGET_LONG_DOUBLE (rv, l);

  operands[2] = gen_lowpart (DImode, operands[0]);
  operands[3] = gen_highpart (DImode, operands[0]);
#if HOST_BITS_PER_WIDE_INT >= 64
  val = ((HOST_WIDE_INT)(unsigned long)l[0] << 32
         | ((HOST_WIDE_INT)(unsigned long)l[1]));
  operands[4] = gen_int_mode (val, DImode);

  val = ((HOST_WIDE_INT)(unsigned long)l[2] << 32
         | ((HOST_WIDE_INT)(unsigned long)l[3]));
  operands[5] = gen_int_mode (val, DImode);
#else
  operands[4] = immed_double_const (l[1], l[0], DImode);
  operands[5] = immed_double_const (l[3], l[2], DImode);
#endif
}
  operand0 = operands[0];
  operand1 = operands[1];
  operand2 = operands[2];
  operand3 = operands[3];
  operand4 = operands[4];
  operand5 = operands[5];
  emit_insn (gen_rtx_SET (VOIDmode,
	operand2,
	operand4));
  emit_insn (gen_rtx_SET (VOIDmode,
	operand3,
	operand5));
  _val = get_insns ();
  end_sequence ();
  return _val;
}

/* /home/yu/gcc/gcc_powerpc/crosstool-0.42/build/powerpc-7450-linux-gnu/gcc-3.3.1-glibc-2.3.2/gcc-3.3.1/gcc/config/rs6000/rs6000.md:8927 */
extern rtx gen_split_994 PARAMS ((rtx *));
rtx
gen_split_994 (operands)
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
	gen_rtx_FLOAT_TRUNCATE (SFmode,
	copy_rtx (operand2))));
  _val = get_insns ();
  end_sequence ();
  return _val;
}

/* /home/yu/gcc/gcc_powerpc/crosstool-0.42/build/powerpc-7450-linux-gnu/gcc-3.3.1-glibc-2.3.2/gcc-3.3.1/gcc/config/rs6000/rs6000.md:8941 */
extern rtx gen_split_995 PARAMS ((rtx *));
rtx
gen_split_995 (operands)
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
	gen_rtx_FLOAT (DFmode,
	operand1)));
  emit_insn (gen_rtx_SET (VOIDmode,
	operand0,
	gen_rtx_FLOAT_EXTEND (TFmode,
	copy_rtx (operand2))));
  _val = get_insns ();
  end_sequence ();
  return _val;
}

/* /home/yu/gcc/gcc_powerpc/crosstool-0.42/build/powerpc-7450-linux-gnu/gcc-3.3.1-glibc-2.3.2/gcc-3.3.1/gcc/config/rs6000/rs6000.md:8955 */
extern rtx gen_split_996 PARAMS ((rtx *));
rtx
gen_split_996 (operands)
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
	gen_rtx_FLOAT (DFmode,
	operand1)));
  emit_insn (gen_rtx_SET (VOIDmode,
	operand0,
	gen_rtx_FLOAT_EXTEND (TFmode,
	copy_rtx (operand2))));
  _val = get_insns ();
  end_sequence ();
  return _val;
}

/* /home/yu/gcc/gcc_powerpc/crosstool-0.42/build/powerpc-7450-linux-gnu/gcc-3.3.1-glibc-2.3.2/gcc-3.3.1/gcc/config/rs6000/rs6000.md:8969 */
extern rtx gen_split_997 PARAMS ((rtx *));
rtx
gen_split_997 (operands)
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
	gen_rtx_FIX (DImode,
	copy_rtx (operand2))));
  _val = get_insns ();
  end_sequence ();
  return _val;
}

/* /home/yu/gcc/gcc_powerpc/crosstool-0.42/build/powerpc-7450-linux-gnu/gcc-3.3.1-glibc-2.3.2/gcc-3.3.1/gcc/config/rs6000/rs6000.md:8983 */
extern rtx gen_split_998 PARAMS ((rtx *));
rtx
gen_split_998 (operands)
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
	gen_rtx_FIX (SImode,
	copy_rtx (operand2))));
  _val = get_insns ();
  end_sequence ();
  return _val;
}

/* /home/yu/gcc/gcc_powerpc/crosstool-0.42/build/powerpc-7450-linux-gnu/gcc-3.3.1-glibc-2.3.2/gcc-3.3.1/gcc/config/rs6000/rs6000.md:9044 */
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
{ rs6000_emit_move (operands[0], operands[1], DImode); DONE; }
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

/* /home/yu/gcc/gcc_powerpc/crosstool-0.42/build/powerpc-7450-linux-gnu/gcc-3.3.1-glibc-2.3.2/gcc-3.3.1/gcc/config/rs6000/rs6000.md:9099 */
extern rtx gen_split_1000 PARAMS ((rtx *));
rtx
gen_split_1000 (operands)
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
  HOST_WIDE_INT value = INTVAL (operands[1]);
  operands[2] = operand_subword_force (operands[0], WORDS_BIG_ENDIAN == 0,
				       DImode);
  operands[3] = operand_subword_force (operands[0], WORDS_BIG_ENDIAN != 0,
				       DImode);
#if HOST_BITS_PER_WIDE_INT == 32
  operands[4] = (value & 0x80000000) ? constm1_rtx : const0_rtx;
#else
  operands[4] = GEN_INT (value >> 32);
  operands[1] = GEN_INT (((value & 0xffffffff) ^ 0x80000000) - 0x80000000);
#endif
}
  operand0 = operands[0];
  operand1 = operands[1];
  operand2 = operands[2];
  operand3 = operands[3];
  operand4 = operands[4];
  emit_insn (gen_rtx_SET (VOIDmode,
	operand2,
	operand4));
  emit_insn (gen_rtx_SET (VOIDmode,
	operand3,
	operand1));
  _val = get_insns ();
  end_sequence ();
  return _val;
}

/* /home/yu/gcc/gcc_powerpc/crosstool-0.42/build/powerpc-7450-linux-gnu/gcc-3.3.1-glibc-2.3.2/gcc-3.3.1/gcc/config/rs6000/rs6000.md:9136 */
extern rtx gen_split_1001 PARAMS ((rtx *));
rtx
gen_split_1001 (operands)
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

{
  operands[2] = operand_subword_force (operands[0], WORDS_BIG_ENDIAN == 0,
				       TImode);
  operands[3] = operand_subword_force (operands[0], WORDS_BIG_ENDIAN != 0,
				       TImode);
  if (GET_CODE (operands[1]) == CONST_DOUBLE)
    {
      operands[4] = GEN_INT (CONST_DOUBLE_HIGH (operands[1]));
      operands[5] = GEN_INT (CONST_DOUBLE_LOW (operands[1]));
    }
  else if (GET_CODE (operands[1]) == CONST_INT)
    {
      operands[4] = GEN_INT (- (INTVAL (operands[1]) < 0));
      operands[5] = operands[1];
    }
  else
    FAIL;
}
  operand0 = operands[0];
  operand1 = operands[1];
  operand2 = operands[2];
  operand3 = operands[3];
  operand4 = operands[4];
  operand5 = operands[5];
  emit_insn (gen_rtx_SET (VOIDmode,
	operand2,
	operand4));
  emit_insn (gen_rtx_SET (VOIDmode,
	operand3,
	operand5));
  _val = get_insns ();
  end_sequence ();
  return _val;
}

/* /home/yu/gcc/gcc_powerpc/crosstool-0.42/build/powerpc-7450-linux-gnu/gcc-3.3.1-glibc-2.3.2/gcc-3.3.1/gcc/config/rs6000/rs6000.md:9201 */
extern rtx gen_split_1002 PARAMS ((rtx *));
rtx
gen_split_1002 (operands)
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
	constm1_rtx));
  emit_insn (gen_rtx_SET (VOIDmode,
	copy_rtx (operand0),
	gen_rtx_AND (DImode,
	gen_rtx_ROTATE (DImode,
	copy_rtx (operand0),
	const0_rtx),
	operand1)));
  _val = get_insns ();
  end_sequence ();
  return _val;
}

/* /home/yu/gcc/gcc_powerpc/crosstool-0.42/build/powerpc-7450-linux-gnu/gcc-3.3.1-glibc-2.3.2/gcc-3.3.1/gcc/config/rs6000/rs6000.md:9216 */
extern rtx gen_split_1003 PARAMS ((rtx *));
rtx
gen_split_1003 (operands)
      rtx *operands;
{
  rtx operand0;
  rtx operand1;
  rtx operand2;
  rtx operand3;
  rtx _val = 0;
  start_sequence ();

{ rtx tem = rs6000_emit_set_const (operands[0], DImode, operands[1], 5);

  if (tem == operands[0])
    DONE;
  else
    FAIL;
}
  operand0 = operands[0];
  operand1 = operands[1];
  operand2 = operands[2];
  operand3 = operands[3];
  emit_insn (gen_rtx_SET (VOIDmode,
	operand0,
	operand2));
  emit_insn (gen_rtx_SET (VOIDmode,
	copy_rtx (operand0),
	gen_rtx_PLUS (DImode,
	copy_rtx (operand0),
	operand3)));
  _val = get_insns ();
  end_sequence ();
  return _val;
}

/* /home/yu/gcc/gcc_powerpc/crosstool-0.42/build/powerpc-7450-linux-gnu/gcc-3.3.1-glibc-2.3.2/gcc-3.3.1/gcc/config/rs6000/rs6000.md:9231 */
extern rtx gen_split_1004 PARAMS ((rtx *));
rtx
gen_split_1004 (operands)
      rtx *operands;
{
  rtx operand0;
  rtx operand1;
  rtx operand2;
  rtx operand3;
  rtx _val = 0;
  start_sequence ();

{ rtx tem = rs6000_emit_set_const (operands[0], DImode, operands[1], 5);

  if (tem == operands[0])
    DONE;
  else
    FAIL;
}
  operand0 = operands[0];
  operand1 = operands[1];
  operand2 = operands[2];
  operand3 = operands[3];
  emit_insn (gen_rtx_SET (VOIDmode,
	operand0,
	operand2));
  emit_insn (gen_rtx_SET (VOIDmode,
	copy_rtx (operand0),
	gen_rtx_PLUS (DImode,
	copy_rtx (operand0),
	operand3)));
  _val = get_insns ();
  end_sequence ();
  return _val;
}

/* /home/yu/gcc/gcc_powerpc/crosstool-0.42/build/powerpc-7450-linux-gnu/gcc-3.3.1-glibc-2.3.2/gcc-3.3.1/gcc/config/rs6000/rs6000.md:9259 */
extern rtx gen_split_1005 PARAMS ((rtx *));
rtx
gen_split_1005 (operands)
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
	operand1));
  emit_insn (gen_rtx_SET (VOIDmode,
	operand2,
	gen_rtx_COMPARE (CCmode,
	copy_rtx (operand0),
	const0_rtx)));
  _val = get_insns ();
  end_sequence ();
  return _val;
}

/* /home/yu/gcc/gcc_powerpc/crosstool-0.42/build/powerpc-7450-linux-gnu/gcc-3.3.1-glibc-2.3.2/gcc-3.3.1/gcc/config/rs6000/rs6000.md:9274 */
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
{ rs6000_emit_move (operands[0], operands[1], TImode); DONE; }
    operand0 = operands[0];
    operand1 = operands[1];
  }
  emit (gen_rtx_PARALLEL (VOIDmode,
	gen_rtvec (2,
		gen_rtx_SET (VOIDmode,
	operand0,
	operand1),
		gen_rtx_CLOBBER (VOIDmode,
	gen_rtx_SCRATCH (SImode)))));
  _val = get_insns ();
  end_sequence ();
  return _val;
}

/* /home/yu/gcc/gcc_powerpc/crosstool-0.42/build/powerpc-7450-linux-gnu/gcc-3.3.1-glibc-2.3.2/gcc-3.3.1/gcc/config/rs6000/rs6000.md:9427 */
rtx
gen_load_multiple (operand0, operand1, operand2)
     rtx operand0;
     rtx operand1;
     rtx operand2;
{
  rtx operand3;
  rtx _val = 0;
  start_sequence ();
  {
    rtx operands[4];
    operands[0] = operand0;
    operands[1] = operand1;
    operands[2] = operand2;

{
  int regno;
  int count;
  rtx op1;
  int i;

  /* Support only loading a constant number of fixed-point registers from
     memory and only bother with this if more than two; the machine
     doesn't support more than eight.  */
  if (GET_CODE (operands[2]) != CONST_INT
      || INTVAL (operands[2]) <= 2
      || INTVAL (operands[2]) > 8
      || GET_CODE (operands[1]) != MEM
      || GET_CODE (operands[0]) != REG
      || REGNO (operands[0]) >= 32)
    FAIL;

  count = INTVAL (operands[2]);
  regno = REGNO (operands[0]);

  operands[3] = gen_rtx_PARALLEL (VOIDmode, rtvec_alloc (count));
  op1 = replace_equiv_address (operands[1],
			       force_reg (SImode, XEXP (operands[1], 0)));

  for (i = 0; i < count; i++)
    XVECEXP (operands[3], 0, i)
      = gen_rtx_SET (VOIDmode, gen_rtx_REG (SImode, regno + i),
		     adjust_address_nv (op1, SImode, i * 4));
}
    operand0 = operands[0];
    operand1 = operands[1];
    operand2 = operands[2];
    operand3 = operands[3];
  }
  emit (operand3);
  _val = get_insns ();
  end_sequence ();
  return _val;
}

/* /home/yu/gcc/gcc_powerpc/crosstool-0.42/build/powerpc-7450-linux-gnu/gcc-3.3.1-glibc-2.3.2/gcc-3.3.1/gcc/config/rs6000/rs6000.md:9577 */
rtx
gen_store_multiple (operand0, operand1, operand2)
     rtx operand0;
     rtx operand1;
     rtx operand2;
{
  rtx operand3;
  rtx _val = 0;
  start_sequence ();
  {
    rtx operands[4];
    operands[0] = operand0;
    operands[1] = operand1;
    operands[2] = operand2;

{
  int regno;
  int count;
  rtx to;
  rtx op0;
  int i;

  /* Support only storing a constant number of fixed-point registers to
     memory and only bother with this if more than two; the machine
     doesn't support more than eight.  */
  if (GET_CODE (operands[2]) != CONST_INT
      || INTVAL (operands[2]) <= 2
      || INTVAL (operands[2]) > 8
      || GET_CODE (operands[0]) != MEM
      || GET_CODE (operands[1]) != REG
      || REGNO (operands[1]) >= 32)
    FAIL;

  count = INTVAL (operands[2]);
  regno = REGNO (operands[1]);

  operands[3] = gen_rtx_PARALLEL (VOIDmode, rtvec_alloc (count + 1));
  to = force_reg (SImode, XEXP (operands[0], 0));
  op0 = replace_equiv_address (operands[0], to);

  XVECEXP (operands[3], 0, 0)
    = gen_rtx_SET (VOIDmode, adjust_address_nv (op0, SImode, 0), operands[1]);
  XVECEXP (operands[3], 0, 1) = gen_rtx_CLOBBER (VOIDmode,
						 gen_rtx_SCRATCH (SImode));

  for (i = 1; i < count; i++)
    XVECEXP (operands[3], 0, i + 1)
      = gen_rtx_SET (VOIDmode,
		     adjust_address_nv (op0, SImode, i * 4),
		     gen_rtx_REG (SImode, regno + i));
}
    operand0 = operands[0];
    operand1 = operands[1];
    operand2 = operands[2];
    operand3 = operands[3];
  }
  emit (operand3);
  _val = get_insns ();
  end_sequence ();
  return _val;
}

/* /home/yu/gcc/gcc_powerpc/crosstool-0.42/build/powerpc-7450-linux-gnu/gcc-3.3.1-glibc-2.3.2/gcc-3.3.1/gcc/config/rs6000/rs6000.md:9646 */
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
  if (expand_block_move (operands))
    DONE;
  else
    FAIL;
}
    operand0 = operands[0];
    operand1 = operands[1];
    operand2 = operands[2];
    operand3 = operands[3];
  }
  emit (gen_rtx_PARALLEL (VOIDmode,
	gen_rtvec (3,
		gen_rtx_SET (VOIDmode,
	operand0,
	operand1),
		gen_rtx_USE (VOIDmode,
	operand2),
		gen_rtx_USE (VOIDmode,
	operand3))));
  _val = get_insns ();
  end_sequence ();
  return _val;
}

/* /home/yu/gcc/gcc_powerpc/crosstool-0.42/build/powerpc-7450-linux-gnu/gcc-3.3.1-glibc-2.3.2/gcc-3.3.1/gcc/config/rs6000/rs6000.md:9663 */
rtx
gen_movstrsi_8reg (operand0, operand1, operand2, operand3)
     rtx operand0;
     rtx operand1;
     rtx operand2;
     rtx operand3;
{
  return gen_rtx_PARALLEL (VOIDmode,
	gen_rtvec (12,
		gen_rtx_SET (VOIDmode,
	operand0,
	operand1),
		gen_rtx_USE (VOIDmode,
	operand2),
		gen_rtx_USE (VOIDmode,
	operand3),
		gen_rtx_CLOBBER (VOIDmode,
	gen_rtx_REG (SImode,
	5)),
		gen_rtx_CLOBBER (VOIDmode,
	gen_rtx_REG (SImode,
	6)),
		gen_rtx_CLOBBER (VOIDmode,
	gen_rtx_REG (SImode,
	7)),
		gen_rtx_CLOBBER (VOIDmode,
	gen_rtx_REG (SImode,
	8)),
		gen_rtx_CLOBBER (VOIDmode,
	gen_rtx_REG (SImode,
	9)),
		gen_rtx_CLOBBER (VOIDmode,
	gen_rtx_REG (SImode,
	10)),
		gen_rtx_CLOBBER (VOIDmode,
	gen_rtx_REG (SImode,
	11)),
		gen_rtx_CLOBBER (VOIDmode,
	gen_rtx_REG (SImode,
	12)),
		gen_rtx_CLOBBER (VOIDmode,
	gen_rtx_SCRATCH (SImode))));
}

/* /home/yu/gcc/gcc_powerpc/crosstool-0.42/build/powerpc-7450-linux-gnu/gcc-3.3.1-glibc-2.3.2/gcc-3.3.1/gcc/config/rs6000/rs6000.md:9755 */
rtx
gen_movstrsi_6reg (operand0, operand1, operand2, operand3)
     rtx operand0;
     rtx operand1;
     rtx operand2;
     rtx operand3;
{
  return gen_rtx_PARALLEL (VOIDmode,
	gen_rtvec (10,
		gen_rtx_SET (VOIDmode,
	operand0,
	operand1),
		gen_rtx_USE (VOIDmode,
	operand2),
		gen_rtx_USE (VOIDmode,
	operand3),
		gen_rtx_CLOBBER (VOIDmode,
	gen_rtx_REG (SImode,
	5)),
		gen_rtx_CLOBBER (VOIDmode,
	gen_rtx_REG (SImode,
	6)),
		gen_rtx_CLOBBER (VOIDmode,
	gen_rtx_REG (SImode,
	7)),
		gen_rtx_CLOBBER (VOIDmode,
	gen_rtx_REG (SImode,
	8)),
		gen_rtx_CLOBBER (VOIDmode,
	gen_rtx_REG (SImode,
	9)),
		gen_rtx_CLOBBER (VOIDmode,
	gen_rtx_REG (SImode,
	10)),
		gen_rtx_CLOBBER (VOIDmode,
	gen_rtx_SCRATCH (SImode))));
}

/* /home/yu/gcc/gcc_powerpc/crosstool-0.42/build/powerpc-7450-linux-gnu/gcc-3.3.1-glibc-2.3.2/gcc-3.3.1/gcc/config/rs6000/rs6000.md:9836 */
rtx
gen_movstrsi_4reg (operand0, operand1, operand2, operand3)
     rtx operand0;
     rtx operand1;
     rtx operand2;
     rtx operand3;
{
  return gen_rtx_PARALLEL (VOIDmode,
	gen_rtvec (8,
		gen_rtx_SET (VOIDmode,
	operand0,
	operand1),
		gen_rtx_USE (VOIDmode,
	operand2),
		gen_rtx_USE (VOIDmode,
	operand3),
		gen_rtx_CLOBBER (VOIDmode,
	gen_rtx_REG (SImode,
	5)),
		gen_rtx_CLOBBER (VOIDmode,
	gen_rtx_REG (SImode,
	6)),
		gen_rtx_CLOBBER (VOIDmode,
	gen_rtx_REG (SImode,
	7)),
		gen_rtx_CLOBBER (VOIDmode,
	gen_rtx_REG (SImode,
	8)),
		gen_rtx_CLOBBER (VOIDmode,
	gen_rtx_SCRATCH (SImode))));
}

/* /home/yu/gcc/gcc_powerpc/crosstool-0.42/build/powerpc-7450-linux-gnu/gcc-3.3.1-glibc-2.3.2/gcc-3.3.1/gcc/config/rs6000/rs6000.md:9907 */
rtx
gen_movstrsi_2reg (operand0, operand1, operand2, operand3)
     rtx operand0;
     rtx operand1;
     rtx operand2;
     rtx operand3;
{
  return gen_rtx_PARALLEL (VOIDmode,
	gen_rtvec (5,
		gen_rtx_SET (VOIDmode,
	operand0,
	operand1),
		gen_rtx_USE (VOIDmode,
	operand2),
		gen_rtx_USE (VOIDmode,
	operand3),
		gen_rtx_CLOBBER (VOIDmode,
	gen_rtx_SCRATCH (DImode)),
		gen_rtx_CLOBBER (VOIDmode,
	gen_rtx_SCRATCH (SImode))));
}

/* /home/yu/gcc/gcc_powerpc/crosstool-0.42/build/powerpc-7450-linux-gnu/gcc-3.3.1-glibc-2.3.2/gcc-3.3.1/gcc/config/rs6000/rs6000.md:9944 */
rtx
gen_movstrsi_1reg (operand0, operand1, operand2, operand3)
     rtx operand0;
     rtx operand1;
     rtx operand2;
     rtx operand3;
{
  return gen_rtx_PARALLEL (VOIDmode,
	gen_rtvec (5,
		gen_rtx_SET (VOIDmode,
	operand0,
	operand1),
		gen_rtx_USE (VOIDmode,
	operand2),
		gen_rtx_USE (VOIDmode,
	operand3),
		gen_rtx_CLOBBER (VOIDmode,
	gen_rtx_SCRATCH (SImode)),
		gen_rtx_CLOBBER (VOIDmode,
	gen_rtx_SCRATCH (SImode))));
}

/* /home/yu/gcc/gcc_powerpc/crosstool-0.42/build/powerpc-7450-linux-gnu/gcc-3.3.1-glibc-2.3.2/gcc-3.3.1/gcc/config/rs6000/rs6000.md:10252 */
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

{ rtx chain = gen_reg_rtx (Pmode);
  rtx stack_bot = gen_rtx_MEM (Pmode, stack_pointer_rtx);
  rtx neg_op0;

  emit_move_insn (chain, stack_bot);

  /* Check stack bounds if necessary.  */
  if (current_function_limit_stack)
    {
      rtx available;
      available = expand_binop (Pmode, sub_optab, 
				stack_pointer_rtx, stack_limit_rtx,
				NULL_RTX, 1, OPTAB_WIDEN);
      emit_insn (gen_cond_trap (LTU, available, operands[1], const0_rtx));
    }

  if (GET_CODE (operands[1]) != CONST_INT
      || INTVAL (operands[1]) < -32767
      || INTVAL (operands[1]) > 32768)
    {
      neg_op0 = gen_reg_rtx (Pmode);
      if (TARGET_32BIT)
	emit_insn (gen_negsi2 (neg_op0, operands[1]));
      else
	emit_insn (gen_negdi2 (neg_op0, operands[1]));
    }
  else
    neg_op0 = GEN_INT (- INTVAL (operands[1]));

  if (TARGET_UPDATE)
    emit_insn ((* ((TARGET_32BIT) ? gen_movsi_update : gen_movdi_update))
		(stack_pointer_rtx, stack_pointer_rtx, neg_op0, chain));

  else
    {
      emit_insn ((* ((TARGET_32BIT) ? gen_addsi3 : gen_adddi3))
		 (stack_pointer_rtx, stack_pointer_rtx, neg_op0));
      emit_move_insn (gen_rtx_MEM (Pmode, stack_pointer_rtx), chain);
    }

  emit_move_insn (operands[0], virtual_stack_dynamic_rtx);
  DONE;
}
    operand0 = operands[0];
    operand1 = operands[1];
  }
  emit_insn (gen_rtx_SET (VOIDmode,
	operand0,
	gen_rtx_MINUS (VOIDmode,
	gen_rtx_REG (VOIDmode,
	1),
	operand1)));
  emit_insn (gen_rtx_SET (VOIDmode,
	gen_rtx_REG (VOIDmode,
	1),
	gen_rtx_MINUS (VOIDmode,
	gen_rtx_REG (VOIDmode,
	1),
	operand1)));
  _val = get_insns ();
  end_sequence ();
  return _val;
}

/* /home/yu/gcc/gcc_powerpc/crosstool-0.42/build/powerpc-7450-linux-gnu/gcc-3.3.1-glibc-2.3.2/gcc-3.3.1/gcc/config/rs6000/rs6000.md:10312 */
rtx
gen_save_stack_function (operand0, operand1)
     rtx operand0;
     rtx operand1;
{
  rtx _val = 0;
  start_sequence ();
  {
    rtx operands[2];
    operands[0] = operand0;
    operands[1] = operand1;
DONE;
    operand0 = operands[0];
    operand1 = operands[1];
  }
  emit (operand0);
  emit (operand1);
  _val = get_insns ();
  end_sequence ();
  return _val;
}

/* /home/yu/gcc/gcc_powerpc/crosstool-0.42/build/powerpc-7450-linux-gnu/gcc-3.3.1-glibc-2.3.2/gcc-3.3.1/gcc/config/rs6000/rs6000.md:10318 */
rtx
gen_restore_stack_function (operand0, operand1)
     rtx operand0;
     rtx operand1;
{
  rtx _val = 0;
  start_sequence ();
  {
    rtx operands[2];
    operands[0] = operand0;
    operands[1] = operand1;
DONE;
    operand0 = operands[0];
    operand1 = operands[1];
  }
  emit (operand0);
  emit (operand1);
  _val = get_insns ();
  end_sequence ();
  return _val;
}

/* /home/yu/gcc/gcc_powerpc/crosstool-0.42/build/powerpc-7450-linux-gnu/gcc-3.3.1-glibc-2.3.2/gcc-3.3.1/gcc/config/rs6000/rs6000.md:10324 */
rtx
gen_restore_stack_block (operand0, operand1)
     rtx operand0;
     rtx operand1;
{
  rtx operand2;
  rtx operand3;
  rtx _val = 0;
  start_sequence ();
  {
    rtx operands[4];
    operands[0] = operand0;
    operands[1] = operand1;

{
  operands[2] = gen_reg_rtx (Pmode);
  operands[3] = gen_rtx_MEM (Pmode, operands[0]);
}
    operand0 = operands[0];
    operand1 = operands[1];
    operand2 = operands[2];
    operand3 = operands[3];
  }
  emit_insn (gen_rtx_USE (VOIDmode,
	operand0));
  emit_insn (gen_rtx_SET (VOIDmode,
	operand2,
	operand3));
  emit_insn (gen_rtx_SET (VOIDmode,
	operand0,
	operand1));
  emit_insn (gen_rtx_SET (VOIDmode,
	operand3,
	operand2));
  _val = get_insns ();
  end_sequence ();
  return _val;
}

/* /home/yu/gcc/gcc_powerpc/crosstool-0.42/build/powerpc-7450-linux-gnu/gcc-3.3.1-glibc-2.3.2/gcc-3.3.1/gcc/config/rs6000/rs6000.md:10336 */
rtx
gen_save_stack_nonlocal (operand0, operand1)
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
  rtx temp = gen_reg_rtx (Pmode);

  /* Copy the backchain to the first word, sp to the second.  */
  emit_move_insn (temp, gen_rtx_MEM (Pmode, operands[1]));
  emit_move_insn (operand_subword (operands[0], 0, 0,
				   (TARGET_32BIT ? DImode : TImode)),
		  temp);
  emit_move_insn (operand_subword (operands[0], 1, 0, (TARGET_32BIT ? DImode : TImode)),
		  operands[1]);
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

/* /home/yu/gcc/gcc_powerpc/crosstool-0.42/build/powerpc-7450-linux-gnu/gcc-3.3.1-glibc-2.3.2/gcc-3.3.1/gcc/config/rs6000/rs6000.md:10354 */
rtx
gen_restore_stack_nonlocal (operand0, operand1)
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
  rtx temp = gen_reg_rtx (Pmode);

  /* Restore the backchain from the first word, sp from the second.  */
  emit_move_insn (temp,
		  operand_subword (operands[1], 0, 0, (TARGET_32BIT ? DImode : TImode)));
  emit_move_insn (operands[0],
		  operand_subword (operands[1], 1, 0,
				   (TARGET_32BIT ? DImode : TImode)));
  emit_move_insn (gen_rtx_MEM (Pmode, operands[0]), temp);
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

/* /home/yu/gcc/gcc_powerpc/crosstool-0.42/build/powerpc-7450-linux-gnu/gcc-3.3.1-glibc-2.3.2/gcc-3.3.1/gcc/config/rs6000/rs6000.md:10471 */
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
#if TARGET_MACHO
  if (DEFAULT_ABI == ABI_DARWIN)
    {
      char *picbase = machopic_function_base_name ();
      rtx picrtx = gen_rtx_SYMBOL_REF (Pmode, ggc_alloc_string (picbase, -1));
      rtx picreg = gen_rtx_REG (Pmode, RS6000_PIC_OFFSET_TABLE_REGNUM);
      rtx tmplabrtx;
      char tmplab[20];

      ASM_GENERATE_INTERNAL_LABEL(tmplab, "LSJR",
				  CODE_LABEL_NUMBER (operands[0]));
      tmplabrtx = gen_rtx_SYMBOL_REF (Pmode, ggc_alloc_string (tmplab, -1));

      emit_insn (gen_load_macho_picbase (picreg, tmplabrtx));
      emit_insn (gen_macho_correct_pic (picreg, picreg, picrtx, tmplabrtx));
    }
  else
#endif
    rs6000_emit_load_toc_table (FALSE);
  DONE;
}
    operand0 = operands[0];
  }
  emit_insn (gen_rtx_USE (VOIDmode,
	gen_rtx_LABEL_REF (VOIDmode,
	operand0)));
  _val = get_insns ();
  end_sequence ();
  return _val;
}

/* /home/yu/gcc/gcc_powerpc/crosstool-0.42/build/powerpc-7450-linux-gnu/gcc-3.3.1-glibc-2.3.2/gcc-3.3.1/gcc/config/rs6000/rs6000.md:10506 */
rtx
gen_call_indirect_aix32 (operand0, operand1)
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

{ operands[2] = gen_reg_rtx (SImode); }
    operand0 = operands[0];
    operand1 = operands[1];
    operand2 = operands[2];
  }
  emit_insn (gen_rtx_SET (VOIDmode,
	operand2,
	gen_rtx_MEM (SImode,
	operand0)));
  emit_insn (gen_rtx_SET (VOIDmode,
	gen_rtx_MEM (SImode,
	gen_rtx_PLUS (SImode,
	gen_rtx_REG (SImode,
	1),
	GEN_INT (20LL))),
	gen_rtx_REG (SImode,
	2)));
  emit_insn (gen_rtx_SET (VOIDmode,
	gen_rtx_REG (SImode,
	2),
	gen_rtx_MEM (SImode,
	gen_rtx_PLUS (SImode,
	operand0,
	GEN_INT (4LL)))));
  emit_insn (gen_rtx_SET (VOIDmode,
	gen_rtx_REG (SImode,
	11),
	gen_rtx_MEM (SImode,
	gen_rtx_PLUS (SImode,
	operand0,
	GEN_INT (8LL)))));
  emit_call_insn (gen_rtx_PARALLEL (VOIDmode,
	gen_rtvec (5,
		gen_rtx_CALL (VOIDmode,
	gen_rtx_MEM (SImode,
	operand2),
	operand1),
		gen_rtx_USE (VOIDmode,
	gen_rtx_REG (SImode,
	2)),
		gen_rtx_USE (VOIDmode,
	gen_rtx_REG (SImode,
	11)),
		gen_rtx_SET (VOIDmode,
	gen_rtx_REG (SImode,
	2),
	gen_rtx_MEM (SImode,
	gen_rtx_PLUS (SImode,
	gen_rtx_REG (SImode,
	1),
	GEN_INT (20LL)))),
		gen_rtx_CLOBBER (VOIDmode,
	gen_rtx_SCRATCH (SImode)))));
  _val = get_insns ();
  end_sequence ();
  return _val;
}

/* /home/yu/gcc/gcc_powerpc/crosstool-0.42/build/powerpc-7450-linux-gnu/gcc-3.3.1-glibc-2.3.2/gcc-3.3.1/gcc/config/rs6000/rs6000.md:10528 */
rtx
gen_call_indirect_aix64 (operand0, operand1)
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

{ operands[2] = gen_reg_rtx (DImode); }
    operand0 = operands[0];
    operand1 = operands[1];
    operand2 = operands[2];
  }
  emit_insn (gen_rtx_SET (VOIDmode,
	operand2,
	gen_rtx_MEM (DImode,
	operand0)));
  emit_insn (gen_rtx_SET (VOIDmode,
	gen_rtx_MEM (DImode,
	gen_rtx_PLUS (DImode,
	gen_rtx_REG (DImode,
	1),
	GEN_INT (40LL))),
	gen_rtx_REG (DImode,
	2)));
  emit_insn (gen_rtx_SET (VOIDmode,
	gen_rtx_REG (DImode,
	2),
	gen_rtx_MEM (DImode,
	gen_rtx_PLUS (DImode,
	operand0,
	GEN_INT (8LL)))));
  emit_insn (gen_rtx_SET (VOIDmode,
	gen_rtx_REG (DImode,
	11),
	gen_rtx_MEM (DImode,
	gen_rtx_PLUS (DImode,
	operand0,
	GEN_INT (16LL)))));
  emit_call_insn (gen_rtx_PARALLEL (VOIDmode,
	gen_rtvec (5,
		gen_rtx_CALL (VOIDmode,
	gen_rtx_MEM (SImode,
	operand2),
	operand1),
		gen_rtx_USE (VOIDmode,
	gen_rtx_REG (DImode,
	2)),
		gen_rtx_USE (VOIDmode,
	gen_rtx_REG (DImode,
	11)),
		gen_rtx_SET (VOIDmode,
	gen_rtx_REG (DImode,
	2),
	gen_rtx_MEM (DImode,
	gen_rtx_PLUS (DImode,
	gen_rtx_REG (DImode,
	1),
	GEN_INT (40LL)))),
		gen_rtx_CLOBBER (VOIDmode,
	gen_rtx_SCRATCH (SImode)))));
  _val = get_insns ();
  end_sequence ();
  return _val;
}

/* /home/yu/gcc/gcc_powerpc/crosstool-0.42/build/powerpc-7450-linux-gnu/gcc-3.3.1-glibc-2.3.2/gcc-3.3.1/gcc/config/rs6000/rs6000.md:10550 */
rtx
gen_call_value_indirect_aix32 (operand0, operand1, operand2)
     rtx operand0;
     rtx operand1;
     rtx operand2;
{
  rtx operand3;
  rtx _val = 0;
  start_sequence ();
  {
    rtx operands[4];
    operands[0] = operand0;
    operands[1] = operand1;
    operands[2] = operand2;

{ operands[3] = gen_reg_rtx (SImode); }
    operand0 = operands[0];
    operand1 = operands[1];
    operand2 = operands[2];
    operand3 = operands[3];
  }
  emit_insn (gen_rtx_SET (VOIDmode,
	operand3,
	gen_rtx_MEM (SImode,
	operand1)));
  emit_insn (gen_rtx_SET (VOIDmode,
	gen_rtx_MEM (SImode,
	gen_rtx_PLUS (SImode,
	gen_rtx_REG (SImode,
	1),
	GEN_INT (20LL))),
	gen_rtx_REG (SImode,
	2)));
  emit_insn (gen_rtx_SET (VOIDmode,
	gen_rtx_REG (SImode,
	2),
	gen_rtx_MEM (SImode,
	gen_rtx_PLUS (SImode,
	operand1,
	GEN_INT (4LL)))));
  emit_insn (gen_rtx_SET (VOIDmode,
	gen_rtx_REG (SImode,
	11),
	gen_rtx_MEM (SImode,
	gen_rtx_PLUS (SImode,
	operand1,
	GEN_INT (8LL)))));
  emit_call_insn (gen_rtx_PARALLEL (VOIDmode,
	gen_rtvec (5,
		gen_rtx_SET (VOIDmode,
	operand0,
	gen_rtx_CALL (VOIDmode,
	gen_rtx_MEM (SImode,
	operand3),
	operand2)),
		gen_rtx_USE (VOIDmode,
	gen_rtx_REG (SImode,
	2)),
		gen_rtx_USE (VOIDmode,
	gen_rtx_REG (SImode,
	11)),
		gen_rtx_SET (VOIDmode,
	gen_rtx_REG (SImode,
	2),
	gen_rtx_MEM (SImode,
	gen_rtx_PLUS (SImode,
	gen_rtx_REG (SImode,
	1),
	GEN_INT (20LL)))),
		gen_rtx_CLOBBER (VOIDmode,
	gen_rtx_SCRATCH (SImode)))));
  _val = get_insns ();
  end_sequence ();
  return _val;
}

/* /home/yu/gcc/gcc_powerpc/crosstool-0.42/build/powerpc-7450-linux-gnu/gcc-3.3.1-glibc-2.3.2/gcc-3.3.1/gcc/config/rs6000/rs6000.md:10573 */
rtx
gen_call_value_indirect_aix64 (operand0, operand1, operand2)
     rtx operand0;
     rtx operand1;
     rtx operand2;
{
  rtx operand3;
  rtx _val = 0;
  start_sequence ();
  {
    rtx operands[4];
    operands[0] = operand0;
    operands[1] = operand1;
    operands[2] = operand2;

{ operands[3] = gen_reg_rtx (DImode); }
    operand0 = operands[0];
    operand1 = operands[1];
    operand2 = operands[2];
    operand3 = operands[3];
  }
  emit_insn (gen_rtx_SET (VOIDmode,
	operand3,
	gen_rtx_MEM (DImode,
	operand1)));
  emit_insn (gen_rtx_SET (VOIDmode,
	gen_rtx_MEM (DImode,
	gen_rtx_PLUS (DImode,
	gen_rtx_REG (DImode,
	1),
	GEN_INT (40LL))),
	gen_rtx_REG (DImode,
	2)));
  emit_insn (gen_rtx_SET (VOIDmode,
	gen_rtx_REG (DImode,
	2),
	gen_rtx_MEM (DImode,
	gen_rtx_PLUS (DImode,
	operand1,
	GEN_INT (8LL)))));
  emit_insn (gen_rtx_SET (VOIDmode,
	gen_rtx_REG (DImode,
	11),
	gen_rtx_MEM (DImode,
	gen_rtx_PLUS (DImode,
	operand1,
	GEN_INT (16LL)))));
  emit_call_insn (gen_rtx_PARALLEL (VOIDmode,
	gen_rtvec (5,
		gen_rtx_SET (VOIDmode,
	operand0,
	gen_rtx_CALL (VOIDmode,
	gen_rtx_MEM (SImode,
	operand3),
	operand2)),
		gen_rtx_USE (VOIDmode,
	gen_rtx_REG (DImode,
	2)),
		gen_rtx_USE (VOIDmode,
	gen_rtx_REG (DImode,
	11)),
		gen_rtx_SET (VOIDmode,
	gen_rtx_REG (DImode,
	2),
	gen_rtx_MEM (DImode,
	gen_rtx_PLUS (DImode,
	gen_rtx_REG (DImode,
	1),
	GEN_INT (40LL)))),
		gen_rtx_CLOBBER (VOIDmode,
	gen_rtx_SCRATCH (SImode)))));
  _val = get_insns ();
  end_sequence ();
  return _val;
}

/* /home/yu/gcc/gcc_powerpc/crosstool-0.42/build/powerpc-7450-linux-gnu/gcc-3.3.1-glibc-2.3.2/gcc-3.3.1/gcc/config/rs6000/rs6000.md:10597 */
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
#if TARGET_MACHO
  if (flag_pic)
    operands[0] = machopic_indirect_call_target (operands[0]);
#endif

  if (GET_CODE (operands[0]) != MEM || GET_CODE (operands[1]) != CONST_INT)
    abort ();

  operands[0] = XEXP (operands[0], 0);

  if (GET_CODE (operands[0]) != SYMBOL_REF
      || (INTVAL (operands[2]) & CALL_LONG) != 0)
    {
      if (INTVAL (operands[2]) & CALL_LONG)
	operands[0] = rs6000_longcall_ref (operands[0]);

      if (DEFAULT_ABI == ABI_V4
          || DEFAULT_ABI == ABI_AIX_NODESC
	  || DEFAULT_ABI == ABI_DARWIN)
	operands[0] = force_reg (Pmode, operands[0]);

      else if (DEFAULT_ABI == ABI_AIX)
	{
	  /* AIX function pointers are really pointers to a three word
	     area.  */
	  emit_call_insn (TARGET_32BIT
			  ? gen_call_indirect_aix32 (force_reg (SImode,
							        operands[0]),
						     operands[1])
			  : gen_call_indirect_aix64 (force_reg (DImode,
							        operands[0]),
						     operands[1]));
	  DONE;
	}
      else
	abort ();
    }
}
    operand0 = operands[0];
    operand1 = operands[1];
    operand2 = operands[2];
  }
  emit_call_insn (gen_rtx_PARALLEL (VOIDmode,
	gen_rtvec (3,
		gen_rtx_CALL (VOIDmode,
	gen_rtx_MEM (SImode,
	operand0),
	operand1),
		gen_rtx_USE (VOIDmode,
	operand2),
		gen_rtx_CLOBBER (VOIDmode,
	gen_rtx_SCRATCH (SImode)))));
  _val = get_insns ();
  end_sequence ();
  return _val;
}

/* /home/yu/gcc/gcc_powerpc/crosstool-0.42/build/powerpc-7450-linux-gnu/gcc-3.3.1-glibc-2.3.2/gcc-3.3.1/gcc/config/rs6000/rs6000.md:10644 */
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
#if TARGET_MACHO
  if (flag_pic)
    operands[1] = machopic_indirect_call_target (operands[1]);
#endif

  if (GET_CODE (operands[1]) != MEM || GET_CODE (operands[2]) != CONST_INT)
    abort ();

  operands[1] = XEXP (operands[1], 0);

  if (GET_CODE (operands[1]) != SYMBOL_REF
      || (INTVAL (operands[3]) & CALL_LONG) != 0)
    {
      if (INTVAL (operands[3]) & CALL_LONG)
	operands[1] = rs6000_longcall_ref (operands[1]);

      if (DEFAULT_ABI == ABI_V4
	  || DEFAULT_ABI == ABI_AIX_NODESC
	  || DEFAULT_ABI == ABI_DARWIN)
	operands[0] = force_reg (Pmode, operands[0]);

      else if (DEFAULT_ABI == ABI_AIX)
	{
	  /* AIX function pointers are really pointers to a three word
	     area.  */
	  emit_call_insn (TARGET_32BIT
			  ? gen_call_value_indirect_aix32 (operands[0],
							   force_reg (SImode,
								      operands[1]),
							   operands[2])
			  : gen_call_value_indirect_aix64 (operands[0],
							   force_reg (DImode,
								      operands[1]),
							   operands[2]));
	  DONE;
	}
      else
	abort ();
    }
}
    operand0 = operands[0];
    operand1 = operands[1];
    operand2 = operands[2];
    operand3 = operands[3];
  }
  emit_call_insn (gen_rtx_PARALLEL (VOIDmode,
	gen_rtvec (3,
		gen_rtx_SET (VOIDmode,
	operand0,
	gen_rtx_CALL (VOIDmode,
	gen_rtx_MEM (SImode,
	operand1),
	operand2)),
		gen_rtx_USE (VOIDmode,
	operand3),
		gen_rtx_CLOBBER (VOIDmode,
	gen_rtx_SCRATCH (SImode)))));
  _val = get_insns ();
  end_sequence ();
  return _val;
}

/* /home/yu/gcc/gcc_powerpc/crosstool-0.42/build/powerpc-7450-linux-gnu/gcc-3.3.1-glibc-2.3.2/gcc-3.3.1/gcc/config/rs6000/rs6000.md:10981 */
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

  emit_call_insn (GEN_CALL (operands[0], const0_rtx, const0_rtx, const0_rtx));

  for (i = 0; i < XVECLEN (operands[2], 0); i++)
    {
      rtx set = XVECEXP (operands[2], 0, i);
      emit_move_insn (SET_DEST (set), SET_SRC (set));
    }

  /* The optimizer does not know that the call sets the function value
     registers we stored in the result block.  We avoid problems by
     claiming that all hard registers are used and clobbered at this
     point.  */
  emit_insn (gen_blockage ());

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

/* /home/yu/gcc/gcc_powerpc/crosstool-0.42/build/powerpc-7450-linux-gnu/gcc-3.3.1-glibc-2.3.2/gcc-3.3.1/gcc/config/rs6000/rs6000.md:11009 */
rtx
gen_sibcall (operand0, operand1, operand2)
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
#if TARGET_MACHO
  if (flag_pic)
    operands[0] = machopic_indirect_call_target (operands[0]);
#endif

  if (GET_CODE (operands[0]) != MEM || GET_CODE (operands[1]) != CONST_INT)
    abort ();

  operands[0] = XEXP (operands[0], 0);

}
    operand0 = operands[0];
    operand1 = operands[1];
    operand2 = operands[2];
  }
  emit_call_insn (gen_rtx_PARALLEL (VOIDmode,
	gen_rtvec (4,
		gen_rtx_CALL (VOIDmode,
	gen_rtx_MEM (SImode,
	operand0),
	operand1),
		gen_rtx_USE (VOIDmode,
	operand2),
		gen_rtx_USE (VOIDmode,
	gen_rtx_SCRATCH (SImode)),
		gen_rtx_RETURN (VOIDmode))));
  _val = get_insns ();
  end_sequence ();
  return _val;
}

/* /home/yu/gcc/gcc_powerpc/crosstool-0.42/build/powerpc-7450-linux-gnu/gcc-3.3.1-glibc-2.3.2/gcc-3.3.1/gcc/config/rs6000/rs6000.md:11194 */
rtx
gen_sibcall_value (operand0, operand1, operand2, operand3)
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
#if TARGET_MACHO
  if (flag_pic)
    operands[1] = machopic_indirect_call_target (operands[1]);
#endif

  if (GET_CODE (operands[1]) != MEM || GET_CODE (operands[2]) != CONST_INT)
    abort ();

  operands[1] = XEXP (operands[1], 0);

}
    operand0 = operands[0];
    operand1 = operands[1];
    operand2 = operands[2];
    operand3 = operands[3];
  }
  emit_call_insn (gen_rtx_PARALLEL (VOIDmode,
	gen_rtvec (4,
		gen_rtx_SET (VOIDmode,
	operand0,
	gen_rtx_CALL (VOIDmode,
	gen_rtx_MEM (SImode,
	operand1),
	operand2)),
		gen_rtx_USE (VOIDmode,
	operand3),
		gen_rtx_USE (VOIDmode,
	gen_rtx_SCRATCH (SImode)),
		gen_rtx_RETURN (VOIDmode))));
  _val = get_insns ();
  end_sequence ();
  return _val;
}

/* /home/yu/gcc/gcc_powerpc/crosstool-0.42/build/powerpc-7450-linux-gnu/gcc-3.3.1-glibc-2.3.2/gcc-3.3.1/gcc/config/rs6000/rs6000.md:11240 */
rtx
gen_sibcall_epilogue ()
{
  rtx _val = 0;
  start_sequence ();
  {

{
      rs6000_emit_epilogue (TRUE);
      DONE;
}
  }
  emit_insn (gen_rtx_USE (VOIDmode,
	const0_rtx));
  _val = get_insns ();
  end_sequence ();
  return _val;
}

/* /home/yu/gcc/gcc_powerpc/crosstool-0.42/build/powerpc-7450-linux-gnu/gcc-3.3.1-glibc-2.3.2/gcc-3.3.1/gcc/config/rs6000/rs6000.md:11263 */
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
  /* Take care of the possibility that operands[1] might be negative but
     this might be a logical operation.  That insn doesn't exist.  */
  if (GET_CODE (operands[1]) == CONST_INT
      && INTVAL (operands[1]) < 0)
    operands[1] = force_reg (SImode, operands[1]);

  rs6000_compare_op0 = operands[0];
  rs6000_compare_op1 = operands[1];
  rs6000_compare_fp_p = 0;
  DONE;
}
    operand0 = operands[0];
    operand1 = operands[1];
  }
  emit_insn (gen_rtx_SET (VOIDmode,
	cc0_rtx,
	gen_rtx_COMPARE (VOIDmode,
	operand0,
	operand1)));
  _val = get_insns ();
  end_sequence ();
  return _val;
}

/* /home/yu/gcc/gcc_powerpc/crosstool-0.42/build/powerpc-7450-linux-gnu/gcc-3.3.1-glibc-2.3.2/gcc-3.3.1/gcc/config/rs6000/rs6000.md:11282 */
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
  /* Take care of the possibility that operands[1] might be negative but
     this might be a logical operation.  That insn doesn't exist.  */
  if (GET_CODE (operands[1]) == CONST_INT
      && INTVAL (operands[1]) < 0)
    operands[1] = force_reg (DImode, operands[1]);

  rs6000_compare_op0 = operands[0];
  rs6000_compare_op1 = operands[1];
  rs6000_compare_fp_p = 0;
  DONE;
}
    operand0 = operands[0];
    operand1 = operands[1];
  }
  emit_insn (gen_rtx_SET (VOIDmode,
	cc0_rtx,
	gen_rtx_COMPARE (VOIDmode,
	operand0,
	operand1)));
  _val = get_insns ();
  end_sequence ();
  return _val;
}

/* /home/yu/gcc/gcc_powerpc/crosstool-0.42/build/powerpc-7450-linux-gnu/gcc-3.3.1-glibc-2.3.2/gcc-3.3.1/gcc/config/rs6000/rs6000.md:11301 */
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
  rs6000_compare_op0 = operands[0];
  rs6000_compare_op1 = operands[1];
  rs6000_compare_fp_p = 1;
  DONE;
}
    operand0 = operands[0];
    operand1 = operands[1];
  }
  emit_insn (gen_rtx_SET (VOIDmode,
	cc0_rtx,
	gen_rtx_COMPARE (VOIDmode,
	operand0,
	operand1)));
  _val = get_insns ();
  end_sequence ();
  return _val;
}

/* /home/yu/gcc/gcc_powerpc/crosstool-0.42/build/powerpc-7450-linux-gnu/gcc-3.3.1-glibc-2.3.2/gcc-3.3.1/gcc/config/rs6000/rs6000.md:11313 */
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
  rs6000_compare_op0 = operands[0];
  rs6000_compare_op1 = operands[1];
  rs6000_compare_fp_p = 1;
  DONE;
}
    operand0 = operands[0];
    operand1 = operands[1];
  }
  emit_insn (gen_rtx_SET (VOIDmode,
	cc0_rtx,
	gen_rtx_COMPARE (VOIDmode,
	operand0,
	operand1)));
  _val = get_insns ();
  end_sequence ();
  return _val;
}

/* /home/yu/gcc/gcc_powerpc/crosstool-0.42/build/powerpc-7450-linux-gnu/gcc-3.3.1-glibc-2.3.2/gcc-3.3.1/gcc/config/rs6000/rs6000.md:11325 */
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
  rs6000_compare_op0 = operands[0];
  rs6000_compare_op1 = operands[1];
  rs6000_compare_fp_p = 1;
  DONE;
}
    operand0 = operands[0];
    operand1 = operands[1];
  }
  emit_insn (gen_rtx_SET (VOIDmode,
	cc0_rtx,
	gen_rtx_COMPARE (VOIDmode,
	operand0,
	operand1)));
  _val = get_insns ();
  end_sequence ();
  return _val;
}

/* /home/yu/gcc/gcc_powerpc/crosstool-0.42/build/powerpc-7450-linux-gnu/gcc-3.3.1-glibc-2.3.2/gcc-3.3.1/gcc/config/rs6000/rs6000.md:11338 */
rtx
gen_beq (operand0)
     rtx operand0;
{
  rtx _val = 0;
  start_sequence ();
  {
    rtx operands[1];
    operands[0] = operand0;
{ rs6000_emit_cbranch (EQ, operands[0]); DONE; }
    operand0 = operands[0];
  }
  emit_insn (gen_rtx_USE (VOIDmode,
	operand0));
  _val = get_insns ();
  end_sequence ();
  return _val;
}

/* /home/yu/gcc/gcc_powerpc/crosstool-0.42/build/powerpc-7450-linux-gnu/gcc-3.3.1-glibc-2.3.2/gcc-3.3.1/gcc/config/rs6000/rs6000.md:11343 */
rtx
gen_bne (operand0)
     rtx operand0;
{
  rtx _val = 0;
  start_sequence ();
  {
    rtx operands[1];
    operands[0] = operand0;
{ rs6000_emit_cbranch (NE, operands[0]); DONE; }
    operand0 = operands[0];
  }
  emit_insn (gen_rtx_USE (VOIDmode,
	operand0));
  _val = get_insns ();
  end_sequence ();
  return _val;
}

/* /home/yu/gcc/gcc_powerpc/crosstool-0.42/build/powerpc-7450-linux-gnu/gcc-3.3.1-glibc-2.3.2/gcc-3.3.1/gcc/config/rs6000/rs6000.md:11348 */
rtx
gen_bge (operand0)
     rtx operand0;
{
  rtx _val = 0;
  start_sequence ();
  {
    rtx operands[1];
    operands[0] = operand0;
{ rs6000_emit_cbranch (GE, operands[0]); DONE; }
    operand0 = operands[0];
  }
  emit_insn (gen_rtx_USE (VOIDmode,
	operand0));
  _val = get_insns ();
  end_sequence ();
  return _val;
}

/* /home/yu/gcc/gcc_powerpc/crosstool-0.42/build/powerpc-7450-linux-gnu/gcc-3.3.1-glibc-2.3.2/gcc-3.3.1/gcc/config/rs6000/rs6000.md:11353 */
rtx
gen_bgt (operand0)
     rtx operand0;
{
  rtx _val = 0;
  start_sequence ();
  {
    rtx operands[1];
    operands[0] = operand0;
{ rs6000_emit_cbranch (GT, operands[0]); DONE; }
    operand0 = operands[0];
  }
  emit_insn (gen_rtx_USE (VOIDmode,
	operand0));
  _val = get_insns ();
  end_sequence ();
  return _val;
}

/* /home/yu/gcc/gcc_powerpc/crosstool-0.42/build/powerpc-7450-linux-gnu/gcc-3.3.1-glibc-2.3.2/gcc-3.3.1/gcc/config/rs6000/rs6000.md:11358 */
rtx
gen_ble (operand0)
     rtx operand0;
{
  rtx _val = 0;
  start_sequence ();
  {
    rtx operands[1];
    operands[0] = operand0;
{ rs6000_emit_cbranch (LE, operands[0]); DONE; }
    operand0 = operands[0];
  }
  emit_insn (gen_rtx_USE (VOIDmode,
	operand0));
  _val = get_insns ();
  end_sequence ();
  return _val;
}

/* /home/yu/gcc/gcc_powerpc/crosstool-0.42/build/powerpc-7450-linux-gnu/gcc-3.3.1-glibc-2.3.2/gcc-3.3.1/gcc/config/rs6000/rs6000.md:11363 */
rtx
gen_blt (operand0)
     rtx operand0;
{
  rtx _val = 0;
  start_sequence ();
  {
    rtx operands[1];
    operands[0] = operand0;
{ rs6000_emit_cbranch (LT, operands[0]); DONE; }
    operand0 = operands[0];
  }
  emit_insn (gen_rtx_USE (VOIDmode,
	operand0));
  _val = get_insns ();
  end_sequence ();
  return _val;
}

/* /home/yu/gcc/gcc_powerpc/crosstool-0.42/build/powerpc-7450-linux-gnu/gcc-3.3.1-glibc-2.3.2/gcc-3.3.1/gcc/config/rs6000/rs6000.md:11368 */
rtx
gen_bgeu (operand0)
     rtx operand0;
{
  rtx _val = 0;
  start_sequence ();
  {
    rtx operands[1];
    operands[0] = operand0;
{ rs6000_emit_cbranch (GEU, operands[0]); DONE; }
    operand0 = operands[0];
  }
  emit_insn (gen_rtx_USE (VOIDmode,
	operand0));
  _val = get_insns ();
  end_sequence ();
  return _val;
}

/* /home/yu/gcc/gcc_powerpc/crosstool-0.42/build/powerpc-7450-linux-gnu/gcc-3.3.1-glibc-2.3.2/gcc-3.3.1/gcc/config/rs6000/rs6000.md:11373 */
rtx
gen_bgtu (operand0)
     rtx operand0;
{
  rtx _val = 0;
  start_sequence ();
  {
    rtx operands[1];
    operands[0] = operand0;
{ rs6000_emit_cbranch (GTU, operands[0]); DONE; }
    operand0 = operands[0];
  }
  emit_insn (gen_rtx_USE (VOIDmode,
	operand0));
  _val = get_insns ();
  end_sequence ();
  return _val;
}

/* /home/yu/gcc/gcc_powerpc/crosstool-0.42/build/powerpc-7450-linux-gnu/gcc-3.3.1-glibc-2.3.2/gcc-3.3.1/gcc/config/rs6000/rs6000.md:11378 */
rtx
gen_bleu (operand0)
     rtx operand0;
{
  rtx _val = 0;
  start_sequence ();
  {
    rtx operands[1];
    operands[0] = operand0;
{ rs6000_emit_cbranch (LEU, operands[0]); DONE; }
    operand0 = operands[0];
  }
  emit_insn (gen_rtx_USE (VOIDmode,
	operand0));
  _val = get_insns ();
  end_sequence ();
  return _val;
}

/* /home/yu/gcc/gcc_powerpc/crosstool-0.42/build/powerpc-7450-linux-gnu/gcc-3.3.1-glibc-2.3.2/gcc-3.3.1/gcc/config/rs6000/rs6000.md:11383 */
rtx
gen_bltu (operand0)
     rtx operand0;
{
  rtx _val = 0;
  start_sequence ();
  {
    rtx operands[1];
    operands[0] = operand0;
{ rs6000_emit_cbranch (LTU, operands[0]); DONE; }
    operand0 = operands[0];
  }
  emit_insn (gen_rtx_USE (VOIDmode,
	operand0));
  _val = get_insns ();
  end_sequence ();
  return _val;
}

/* /home/yu/gcc/gcc_powerpc/crosstool-0.42/build/powerpc-7450-linux-gnu/gcc-3.3.1-glibc-2.3.2/gcc-3.3.1/gcc/config/rs6000/rs6000.md:11388 */
rtx
gen_bunordered (operand0)
     rtx operand0;
{
  rtx _val = 0;
  start_sequence ();
  {
    rtx operands[1];
    operands[0] = operand0;
{ rs6000_emit_cbranch (UNORDERED, operands[0]); DONE; }
    operand0 = operands[0];
  }
  emit_insn (gen_rtx_USE (VOIDmode,
	operand0));
  _val = get_insns ();
  end_sequence ();
  return _val;
}

/* /home/yu/gcc/gcc_powerpc/crosstool-0.42/build/powerpc-7450-linux-gnu/gcc-3.3.1-glibc-2.3.2/gcc-3.3.1/gcc/config/rs6000/rs6000.md:11393 */
rtx
gen_bordered (operand0)
     rtx operand0;
{
  rtx _val = 0;
  start_sequence ();
  {
    rtx operands[1];
    operands[0] = operand0;
{ rs6000_emit_cbranch (ORDERED, operands[0]); DONE; }
    operand0 = operands[0];
  }
  emit_insn (gen_rtx_USE (VOIDmode,
	operand0));
  _val = get_insns ();
  end_sequence ();
  return _val;
}

/* /home/yu/gcc/gcc_powerpc/crosstool-0.42/build/powerpc-7450-linux-gnu/gcc-3.3.1-glibc-2.3.2/gcc-3.3.1/gcc/config/rs6000/rs6000.md:11398 */
rtx
gen_buneq (operand0)
     rtx operand0;
{
  rtx _val = 0;
  start_sequence ();
  {
    rtx operands[1];
    operands[0] = operand0;
{ rs6000_emit_cbranch (UNEQ, operands[0]); DONE; }
    operand0 = operands[0];
  }
  emit_insn (gen_rtx_USE (VOIDmode,
	operand0));
  _val = get_insns ();
  end_sequence ();
  return _val;
}

/* /home/yu/gcc/gcc_powerpc/crosstool-0.42/build/powerpc-7450-linux-gnu/gcc-3.3.1-glibc-2.3.2/gcc-3.3.1/gcc/config/rs6000/rs6000.md:11403 */
rtx
gen_bunge (operand0)
     rtx operand0;
{
  rtx _val = 0;
  start_sequence ();
  {
    rtx operands[1];
    operands[0] = operand0;
{ rs6000_emit_cbranch (UNGE, operands[0]); DONE; }
    operand0 = operands[0];
  }
  emit_insn (gen_rtx_USE (VOIDmode,
	operand0));
  _val = get_insns ();
  end_sequence ();
  return _val;
}

/* /home/yu/gcc/gcc_powerpc/crosstool-0.42/build/powerpc-7450-linux-gnu/gcc-3.3.1-glibc-2.3.2/gcc-3.3.1/gcc/config/rs6000/rs6000.md:11408 */
rtx
gen_bungt (operand0)
     rtx operand0;
{
  rtx _val = 0;
  start_sequence ();
  {
    rtx operands[1];
    operands[0] = operand0;
{ rs6000_emit_cbranch (UNGT, operands[0]); DONE; }
    operand0 = operands[0];
  }
  emit_insn (gen_rtx_USE (VOIDmode,
	operand0));
  _val = get_insns ();
  end_sequence ();
  return _val;
}

/* /home/yu/gcc/gcc_powerpc/crosstool-0.42/build/powerpc-7450-linux-gnu/gcc-3.3.1-glibc-2.3.2/gcc-3.3.1/gcc/config/rs6000/rs6000.md:11413 */
rtx
gen_bunle (operand0)
     rtx operand0;
{
  rtx _val = 0;
  start_sequence ();
  {
    rtx operands[1];
    operands[0] = operand0;
{ rs6000_emit_cbranch (UNLE, operands[0]); DONE; }
    operand0 = operands[0];
  }
  emit_insn (gen_rtx_USE (VOIDmode,
	operand0));
  _val = get_insns ();
  end_sequence ();
  return _val;
}

/* /home/yu/gcc/gcc_powerpc/crosstool-0.42/build/powerpc-7450-linux-gnu/gcc-3.3.1-glibc-2.3.2/gcc-3.3.1/gcc/config/rs6000/rs6000.md:11418 */
rtx
gen_bunlt (operand0)
     rtx operand0;
{
  rtx _val = 0;
  start_sequence ();
  {
    rtx operands[1];
    operands[0] = operand0;
{ rs6000_emit_cbranch (UNLT, operands[0]); DONE; }
    operand0 = operands[0];
  }
  emit_insn (gen_rtx_USE (VOIDmode,
	operand0));
  _val = get_insns ();
  end_sequence ();
  return _val;
}

/* /home/yu/gcc/gcc_powerpc/crosstool-0.42/build/powerpc-7450-linux-gnu/gcc-3.3.1-glibc-2.3.2/gcc-3.3.1/gcc/config/rs6000/rs6000.md:11423 */
rtx
gen_bltgt (operand0)
     rtx operand0;
{
  rtx _val = 0;
  start_sequence ();
  {
    rtx operands[1];
    operands[0] = operand0;
{ rs6000_emit_cbranch (LTGT, operands[0]); DONE; }
    operand0 = operands[0];
  }
  emit_insn (gen_rtx_USE (VOIDmode,
	operand0));
  _val = get_insns ();
  end_sequence ();
  return _val;
}

/* /home/yu/gcc/gcc_powerpc/crosstool-0.42/build/powerpc-7450-linux-gnu/gcc-3.3.1-glibc-2.3.2/gcc-3.3.1/gcc/config/rs6000/rs6000.md:11433 */
rtx
gen_seq (operand0)
     rtx operand0;
{
  rtx _val = 0;
  start_sequence ();
  {
    rtx operands[1];
    operands[0] = operand0;
{ rs6000_emit_sCOND (EQ, operands[0]); DONE; }
    operand0 = operands[0];
  }
  emit_insn (gen_rtx_CLOBBER (VOIDmode,
	operand0));
  _val = get_insns ();
  end_sequence ();
  return _val;
}

/* /home/yu/gcc/gcc_powerpc/crosstool-0.42/build/powerpc-7450-linux-gnu/gcc-3.3.1-glibc-2.3.2/gcc-3.3.1/gcc/config/rs6000/rs6000.md:11438 */
rtx
gen_sne (operand0)
     rtx operand0;
{
  rtx _val = 0;
  start_sequence ();
  {
    rtx operands[1];
    operands[0] = operand0;

{ 
  if (! rs6000_compare_fp_p)
    FAIL;

  rs6000_emit_sCOND (NE, operands[0]); 
  DONE;
}
    operand0 = operands[0];
  }
  emit_insn (gen_rtx_CLOBBER (VOIDmode,
	operand0));
  _val = get_insns ();
  end_sequence ();
  return _val;
}

/* /home/yu/gcc/gcc_powerpc/crosstool-0.42/build/powerpc-7450-linux-gnu/gcc-3.3.1-glibc-2.3.2/gcc-3.3.1/gcc/config/rs6000/rs6000.md:11451 */
rtx
gen_sgt (operand0)
     rtx operand0;
{
  rtx _val = 0;
  start_sequence ();
  {
    rtx operands[1];
    operands[0] = operand0;

{
  if (! rs6000_compare_fp_p
      && (! TARGET_POWER || rs6000_compare_op1 == const0_rtx))
    FAIL;

  rs6000_emit_sCOND (GT, operands[0]); 
  DONE;
}
    operand0 = operands[0];
  }
  emit_insn (gen_rtx_CLOBBER (VOIDmode,
	operand0));
  _val = get_insns ();
  end_sequence ();
  return _val;
}

/* /home/yu/gcc/gcc_powerpc/crosstool-0.42/build/powerpc-7450-linux-gnu/gcc-3.3.1-glibc-2.3.2/gcc-3.3.1/gcc/config/rs6000/rs6000.md:11465 */
rtx
gen_slt (operand0)
     rtx operand0;
{
  rtx _val = 0;
  start_sequence ();
  {
    rtx operands[1];
    operands[0] = operand0;

{
  if (! rs6000_compare_fp_p 
      && (! TARGET_POWER || rs6000_compare_op1 == const0_rtx))
    FAIL;

  rs6000_emit_sCOND (LT, operands[0]); 
  DONE;
}
    operand0 = operands[0];
  }
  emit_insn (gen_rtx_CLOBBER (VOIDmode,
	operand0));
  _val = get_insns ();
  end_sequence ();
  return _val;
}

/* /home/yu/gcc/gcc_powerpc/crosstool-0.42/build/powerpc-7450-linux-gnu/gcc-3.3.1-glibc-2.3.2/gcc-3.3.1/gcc/config/rs6000/rs6000.md:11479 */
rtx
gen_sge (operand0)
     rtx operand0;
{
  rtx _val = 0;
  start_sequence ();
  {
    rtx operands[1];
    operands[0] = operand0;

{
  if (! rs6000_compare_fp_p
      && (! TARGET_POWER || rs6000_compare_op1 == const0_rtx))
    FAIL;

  rs6000_emit_sCOND (GE, operands[0]);
  DONE;
}
    operand0 = operands[0];
  }
  emit_insn (gen_rtx_CLOBBER (VOIDmode,
	operand0));
  _val = get_insns ();
  end_sequence ();
  return _val;
}

/* /home/yu/gcc/gcc_powerpc/crosstool-0.42/build/powerpc-7450-linux-gnu/gcc-3.3.1-glibc-2.3.2/gcc-3.3.1/gcc/config/rs6000/rs6000.md:11493 */
rtx
gen_sle (operand0)
     rtx operand0;
{
  rtx _val = 0;
  start_sequence ();
  {
    rtx operands[1];
    operands[0] = operand0;

{
  if (! rs6000_compare_fp_p
      && (! TARGET_POWER || rs6000_compare_op1 == const0_rtx))
    FAIL;

  rs6000_emit_sCOND (LE, operands[0]); 
  DONE;
}
    operand0 = operands[0];
  }
  emit_insn (gen_rtx_CLOBBER (VOIDmode,
	operand0));
  _val = get_insns ();
  end_sequence ();
  return _val;
}

/* /home/yu/gcc/gcc_powerpc/crosstool-0.42/build/powerpc-7450-linux-gnu/gcc-3.3.1-glibc-2.3.2/gcc-3.3.1/gcc/config/rs6000/rs6000.md:11506 */
rtx
gen_sgtu (operand0)
     rtx operand0;
{
  rtx _val = 0;
  start_sequence ();
  {
    rtx operands[1];
    operands[0] = operand0;
{ rs6000_emit_sCOND (GTU, operands[0]); DONE; }
    operand0 = operands[0];
  }
  emit_insn (gen_rtx_CLOBBER (VOIDmode,
	operand0));
  _val = get_insns ();
  end_sequence ();
  return _val;
}

/* /home/yu/gcc/gcc_powerpc/crosstool-0.42/build/powerpc-7450-linux-gnu/gcc-3.3.1-glibc-2.3.2/gcc-3.3.1/gcc/config/rs6000/rs6000.md:11511 */
rtx
gen_sltu (operand0)
     rtx operand0;
{
  rtx _val = 0;
  start_sequence ();
  {
    rtx operands[1];
    operands[0] = operand0;
{ rs6000_emit_sCOND (LTU, operands[0]); DONE; }
    operand0 = operands[0];
  }
  emit_insn (gen_rtx_CLOBBER (VOIDmode,
	operand0));
  _val = get_insns ();
  end_sequence ();
  return _val;
}

/* /home/yu/gcc/gcc_powerpc/crosstool-0.42/build/powerpc-7450-linux-gnu/gcc-3.3.1-glibc-2.3.2/gcc-3.3.1/gcc/config/rs6000/rs6000.md:11516 */
rtx
gen_sgeu (operand0)
     rtx operand0;
{
  rtx _val = 0;
  start_sequence ();
  {
    rtx operands[1];
    operands[0] = operand0;
{ rs6000_emit_sCOND (GEU, operands[0]); DONE; }
    operand0 = operands[0];
  }
  emit_insn (gen_rtx_CLOBBER (VOIDmode,
	operand0));
  _val = get_insns ();
  end_sequence ();
  return _val;
}

/* /home/yu/gcc/gcc_powerpc/crosstool-0.42/build/powerpc-7450-linux-gnu/gcc-3.3.1-glibc-2.3.2/gcc-3.3.1/gcc/config/rs6000/rs6000.md:11521 */
rtx
gen_sleu (operand0)
     rtx operand0;
{
  rtx _val = 0;
  start_sequence ();
  {
    rtx operands[1];
    operands[0] = operand0;
{ rs6000_emit_sCOND (LEU, operands[0]); DONE; }
    operand0 = operands[0];
  }
  emit_insn (gen_rtx_CLOBBER (VOIDmode,
	operand0));
  _val = get_insns ();
  end_sequence ();
  return _val;
}

/* /home/yu/gcc/gcc_powerpc/crosstool-0.42/build/powerpc-7450-linux-gnu/gcc-3.3.1-glibc-2.3.2/gcc-3.3.1/gcc/config/rs6000/rs6000.md:11547 */
extern rtx gen_split_1067 PARAMS ((rtx *));
rtx
gen_split_1067 (operands)
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

{
  /* Get the constant we are comparing against, C,  and see what it looks like
     sign-extended to 16 bits.  Then see what constant could be XOR'ed
     with C to get the sign-extended value.  */

  HOST_WIDE_INT c = INTVAL (operands[2]);
  HOST_WIDE_INT sextc = ((c & 0xffff) ^ 0x8000) - 0x8000;
  HOST_WIDE_INT xorv = c ^ sextc;

  operands[4] = GEN_INT (xorv);
  operands[5] = GEN_INT (sextc);
}
  operand0 = operands[0];
  operand1 = operands[1];
  operand2 = operands[2];
  operand3 = operands[3];
  operand4 = operands[4];
  operand5 = operands[5];
  emit_insn (gen_rtx_SET (VOIDmode,
	operand3,
	gen_rtx_XOR (SImode,
	operand1,
	operand4)));
  emit_insn (gen_rtx_SET (VOIDmode,
	operand0,
	gen_rtx_COMPARE (CCmode,
	copy_rtx (operand3),
	operand5)));
  _val = get_insns ();
  end_sequence ();
  return _val;
}

/* /home/yu/gcc/gcc_powerpc/crosstool-0.42/build/powerpc-7450-linux-gnu/gcc-3.3.1-glibc-2.3.2/gcc-3.3.1/gcc/config/rs6000/rs6000.md:11612 */
extern rtx gen_split_1068 PARAMS ((rtx *));
rtx
gen_split_1068 (operands)
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
  emit_insn (gen_rtx_SET (VOIDmode,
	operand3,
	gen_rtx_COMPARE (CCmode,
	operand1,
	operand2)));
  emit_insn (gen_rtx_SET (VOIDmode,
	operand0,
	gen_rtx_PLUS (SImode,
	copy_rtx (operand1),
	operand4)));
  _val = get_insns ();
  end_sequence ();
  return _val;
}

/* /home/yu/gcc/gcc_powerpc/crosstool-0.42/build/powerpc-7450-linux-gnu/gcc-3.3.1-glibc-2.3.2/gcc-3.3.1/gcc/config/rs6000/rs6000.md:11622 */
extern rtx gen_split_1069 PARAMS ((rtx *));
rtx
gen_split_1069 (operands)
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
  emit_insn (gen_rtx_SET (VOIDmode,
	operand3,
	gen_rtx_COMPARE (CCUNSmode,
	operand1,
	operand2)));
  emit_insn (gen_rtx_SET (VOIDmode,
	operand0,
	gen_rtx_PLUS (SImode,
	copy_rtx (operand1),
	operand4)));
  _val = get_insns ();
  end_sequence ();
  return _val;
}

/* /home/yu/gcc/gcc_powerpc/crosstool-0.42/build/powerpc-7450-linux-gnu/gcc-3.3.1-glibc-2.3.2/gcc-3.3.1/gcc/config/rs6000/rs6000.md:11709 */
extern rtx gen_split_1070 PARAMS ((rtx *));
rtx
gen_split_1070 (operands)
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
	gen_rtx (GET_CODE (operand1), GET_MODE (operand1),
		operand2,
		const0_rtx)));
  emit_insn (gen_rtx_SET (VOIDmode,
	operand0,
	gen_rtx_COMPARE (CCmode,
	copy_rtx (operand3),
	const0_rtx)));
  _val = get_insns ();
  end_sequence ();
  return _val;
}

/* /home/yu/gcc/gcc_powerpc/crosstool-0.42/build/powerpc-7450-linux-gnu/gcc-3.3.1-glibc-2.3.2/gcc-3.3.1/gcc/config/rs6000/rs6000.md:11786 */
extern rtx gen_split_1071 PARAMS ((rtx *));
rtx
gen_split_1071 (operands)
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
  emit_insn (gen_rtx_SET (VOIDmode,
	operand4,
	gen_rtx_ASHIFT (SImode,
	gen_rtx (GET_CODE (operand1), GET_MODE (operand1),
		operand2,
		const0_rtx),
	operand3)));
  emit_insn (gen_rtx_SET (VOIDmode,
	operand0,
	gen_rtx_COMPARE (CCmode,
	copy_rtx (operand4),
	const0_rtx)));
  _val = get_insns ();
  end_sequence ();
  return _val;
}

/* /home/yu/gcc/gcc_powerpc/crosstool-0.42/build/powerpc-7450-linux-gnu/gcc-3.3.1-glibc-2.3.2/gcc-3.3.1/gcc/config/rs6000/rs6000.md:11903 */
extern rtx gen_split_1074 PARAMS ((rtx *));
rtx
gen_split_1074 (operands)
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
	gen_rtvec (2,
		gen_rtx_SET (VOIDmode,
	operand0,
	gen_rtx_EQ (SImode,
	operand1,
	operand2)),
		gen_rtx_CLOBBER (VOIDmode,
	operand3))));
  emit_insn (gen_rtx_SET (VOIDmode,
	operand4,
	gen_rtx_COMPARE (CCmode,
	copy_rtx (operand0),
	const0_rtx)));
  _val = get_insns ();
  end_sequence ();
  return _val;
}

/* /home/yu/gcc/gcc_powerpc/crosstool-0.42/build/powerpc-7450-linux-gnu/gcc-3.3.1-glibc-2.3.2/gcc-3.3.1/gcc/config/rs6000/rs6000.md:11945 */
extern rtx gen_split_1075 PARAMS ((rtx *));
rtx
gen_split_1075 (operands)
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
	gen_rtvec (2,
		gen_rtx_SET (VOIDmode,
	operand0,
	gen_rtx_EQ (DImode,
	operand1,
	operand2)),
		gen_rtx_CLOBBER (VOIDmode,
	operand3))));
  emit_insn (gen_rtx_SET (VOIDmode,
	operand4,
	gen_rtx_COMPARE (CCmode,
	copy_rtx (operand0),
	const0_rtx)));
  _val = get_insns ();
  end_sequence ();
  return _val;
}

/* /home/yu/gcc/gcc_powerpc/crosstool-0.42/build/powerpc-7450-linux-gnu/gcc-3.3.1-glibc-2.3.2/gcc-3.3.1/gcc/config/rs6000/rs6000.md:11965 */
extern rtx gen_split_1076 PARAMS ((rtx *));
rtx
gen_split_1076 (operands)
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
  emit_insn (gen_rtx_SET (VOIDmode,
	operand5,
	operand2));
  emit_insn (gen_rtx_SET (VOIDmode,
	copy_rtx (operand2),
	gen_rtx_PLUS (SImode,
	gen_rtx (GET_CODE (operand1), GET_MODE (operand1),
		copy_rtx (operand2),
		operand3),
	operand4)));
  _val = get_insns ();
  end_sequence ();
  return _val;
}

/* /home/yu/gcc/gcc_powerpc/crosstool-0.42/build/powerpc-7450-linux-gnu/gcc-3.3.1-glibc-2.3.2/gcc-3.3.1/gcc/config/rs6000/rs6000.md:12016 */
extern rtx gen_split_1077 PARAMS ((rtx *));
rtx
gen_split_1077 (operands)
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
  emit_insn (gen_rtx_SET (VOIDmode,
	operand4,
	gen_rtx_PLUS (SImode,
	gen_rtx_EQ (SImode,
	operand1,
	operand2),
	operand3)));
  emit_insn (gen_rtx_SET (VOIDmode,
	operand0,
	gen_rtx_COMPARE (CCmode,
	copy_rtx (operand4),
	const0_rtx)));
  _val = get_insns ();
  end_sequence ();
  return _val;
}

/* /home/yu/gcc/gcc_powerpc/crosstool-0.42/build/powerpc-7450-linux-gnu/gcc-3.3.1-glibc-2.3.2/gcc-3.3.1/gcc/config/rs6000/rs6000.md:12060 */
extern rtx gen_split_1078 PARAMS ((rtx *));
rtx
gen_split_1078 (operands)
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
  emit_insn (gen_rtx_SET (VOIDmode,
	operand0,
	gen_rtx_PLUS (SImode,
	gen_rtx_EQ (SImode,
	operand1,
	operand2),
	operand3)));
  emit_insn (gen_rtx_SET (VOIDmode,
	operand4,
	gen_rtx_COMPARE (CCmode,
	copy_rtx (operand0),
	const0_rtx)));
  _val = get_insns ();
  end_sequence ();
  return _val;
}

/* /home/yu/gcc/gcc_powerpc/crosstool-0.42/build/powerpc-7450-linux-gnu/gcc-3.3.1-glibc-2.3.2/gcc-3.3.1/gcc/config/rs6000/rs6000.md:12151 */
extern rtx gen_split_1079 PARAMS ((rtx *));
rtx
gen_split_1079 (operands)
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
	gen_rtvec (2,
		gen_rtx_SET (VOIDmode,
	operand3,
	gen_rtx_PLUS (SImode,
	gen_rtx_LSHIFTRT (SImode,
	gen_rtx_NEG (SImode,
	gen_rtx_ABS (SImode,
	operand1)),
	GEN_INT (31LL)),
	operand2)),
		gen_rtx_CLOBBER (VOIDmode,
	operand4))));
  emit_insn (gen_rtx_SET (VOIDmode,
	operand0,
	gen_rtx_COMPARE (CCmode,
	copy_rtx (operand3),
	const0_rtx)));
  _val = get_insns ();
  end_sequence ();
  return _val;
}

/* /home/yu/gcc/gcc_powerpc/crosstool-0.42/build/powerpc-7450-linux-gnu/gcc-3.3.1-glibc-2.3.2/gcc-3.3.1/gcc/config/rs6000/rs6000.md:12188 */
extern rtx gen_split_1080 PARAMS ((rtx *));
rtx
gen_split_1080 (operands)
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
	gen_rtx_PLUS (DImode,
	gen_rtx_LSHIFTRT (DImode,
	gen_rtx_NEG (DImode,
	gen_rtx_ABS (DImode,
	operand1)),
	GEN_INT (63LL)),
	operand2)));
  emit_insn (gen_rtx_SET (VOIDmode,
	operand0,
	gen_rtx_COMPARE (CCmode,
	copy_rtx (operand3),
	const0_rtx)));
  _val = get_insns ();
  end_sequence ();
  return _val;
}

/* /home/yu/gcc/gcc_powerpc/crosstool-0.42/build/powerpc-7450-linux-gnu/gcc-3.3.1-glibc-2.3.2/gcc-3.3.1/gcc/config/rs6000/rs6000.md:12226 */
extern rtx gen_split_1081 PARAMS ((rtx *));
rtx
gen_split_1081 (operands)
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
	gen_rtvec (2,
		gen_rtx_SET (VOIDmode,
	operand0,
	gen_rtx_PLUS (SImode,
	gen_rtx_LSHIFTRT (SImode,
	gen_rtx_NEG (SImode,
	gen_rtx_ABS (SImode,
	operand1)),
	GEN_INT (31LL)),
	operand2)),
		gen_rtx_CLOBBER (VOIDmode,
	operand3))));
  emit_insn (gen_rtx_SET (VOIDmode,
	operand4,
	gen_rtx_COMPARE (CCmode,
	copy_rtx (operand0),
	const0_rtx)));
  _val = get_insns ();
  end_sequence ();
  return _val;
}

/* /home/yu/gcc/gcc_powerpc/crosstool-0.42/build/powerpc-7450-linux-gnu/gcc-3.3.1-glibc-2.3.2/gcc-3.3.1/gcc/config/rs6000/rs6000.md:12267 */
extern rtx gen_split_1082 PARAMS ((rtx *));
rtx
gen_split_1082 (operands)
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
	gen_rtvec (2,
		gen_rtx_SET (VOIDmode,
	operand0,
	gen_rtx_PLUS (DImode,
	gen_rtx_LSHIFTRT (DImode,
	gen_rtx_NEG (DImode,
	gen_rtx_ABS (DImode,
	operand1)),
	GEN_INT (63LL)),
	operand2)),
		gen_rtx_CLOBBER (VOIDmode,
	operand3))));
  emit_insn (gen_rtx_SET (VOIDmode,
	operand4,
	gen_rtx_COMPARE (CCmode,
	copy_rtx (operand0),
	const0_rtx)));
  _val = get_insns ();
  end_sequence ();
  return _val;
}

/* /home/yu/gcc/gcc_powerpc/crosstool-0.42/build/powerpc-7450-linux-gnu/gcc-3.3.1-glibc-2.3.2/gcc-3.3.1/gcc/config/rs6000/rs6000.md:12318 */
extern rtx gen_split_1083 PARAMS ((rtx *));
rtx
gen_split_1083 (operands)
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
	gen_rtvec (2,
		gen_rtx_SET (VOIDmode,
	operand0,
	gen_rtx_LE (SImode,
	operand1,
	operand2)),
		gen_rtx_CLOBBER (VOIDmode,
	operand3))));
  emit_insn (gen_rtx_SET (VOIDmode,
	operand4,
	gen_rtx_COMPARE (CCmode,
	copy_rtx (operand0),
	const0_rtx)));
  _val = get_insns ();
  end_sequence ();
  return _val;
}

/* /home/yu/gcc/gcc_powerpc/crosstool-0.42/build/powerpc-7450-linux-gnu/gcc-3.3.1-glibc-2.3.2/gcc-3.3.1/gcc/config/rs6000/rs6000.md:12364 */
extern rtx gen_split_1084 PARAMS ((rtx *));
rtx
gen_split_1084 (operands)
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
  emit_insn (gen_rtx_SET (VOIDmode,
	operand4,
	gen_rtx_PLUS (SImode,
	gen_rtx_LE (SImode,
	operand1,
	operand2),
	operand3)));
  emit_insn (gen_rtx_SET (VOIDmode,
	operand0,
	gen_rtx_COMPARE (CCmode,
	copy_rtx (operand4),
	const0_rtx)));
  _val = get_insns ();
  end_sequence ();
  return _val;
}

/* /home/yu/gcc/gcc_powerpc/crosstool-0.42/build/powerpc-7450-linux-gnu/gcc-3.3.1-glibc-2.3.2/gcc-3.3.1/gcc/config/rs6000/rs6000.md:12399 */
extern rtx gen_split_1085 PARAMS ((rtx *));
rtx
gen_split_1085 (operands)
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
  emit_insn (gen_rtx_SET (VOIDmode,
	operand0,
	gen_rtx_PLUS (SImode,
	gen_rtx_LE (SImode,
	operand1,
	operand2),
	operand3)));
  emit_insn (gen_rtx_SET (VOIDmode,
	operand4,
	gen_rtx_COMPARE (CCmode,
	copy_rtx (operand0),
	const0_rtx)));
  _val = get_insns ();
  end_sequence ();
  return _val;
}

/* /home/yu/gcc/gcc_powerpc/crosstool-0.42/build/powerpc-7450-linux-gnu/gcc-3.3.1-glibc-2.3.2/gcc-3.3.1/gcc/config/rs6000/rs6000.md:12457 */
extern rtx gen_split_1086 PARAMS ((rtx *));
rtx
gen_split_1086 (operands)
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
	operand0,
	gen_rtx_LEU (DImode,
	operand1,
	operand2)));
  emit_insn (gen_rtx_SET (VOIDmode,
	operand3,
	gen_rtx_COMPARE (CCmode,
	copy_rtx (operand0),
	const0_rtx)));
  _val = get_insns ();
  end_sequence ();
  return _val;
}

/* /home/yu/gcc/gcc_powerpc/crosstool-0.42/build/powerpc-7450-linux-gnu/gcc-3.3.1-glibc-2.3.2/gcc-3.3.1/gcc/config/rs6000/rs6000.md:12488 */
extern rtx gen_split_1087 PARAMS ((rtx *));
rtx
gen_split_1087 (operands)
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
	operand0,
	gen_rtx_LEU (SImode,
	operand1,
	operand2)));
  emit_insn (gen_rtx_SET (VOIDmode,
	operand3,
	gen_rtx_COMPARE (CCmode,
	copy_rtx (operand0),
	const0_rtx)));
  _val = get_insns ();
  end_sequence ();
  return _val;
}

/* /home/yu/gcc/gcc_powerpc/crosstool-0.42/build/powerpc-7450-linux-gnu/gcc-3.3.1-glibc-2.3.2/gcc-3.3.1/gcc/config/rs6000/rs6000.md:12543 */
extern rtx gen_split_1088 PARAMS ((rtx *));
rtx
gen_split_1088 (operands)
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
  emit_insn (gen_rtx_SET (VOIDmode,
	operand4,
	gen_rtx_PLUS (SImode,
	gen_rtx_LEU (SImode,
	operand1,
	operand2),
	operand3)));
  emit_insn (gen_rtx_SET (VOIDmode,
	operand0,
	gen_rtx_COMPARE (CCmode,
	copy_rtx (operand4),
	const0_rtx)));
  _val = get_insns ();
  end_sequence ();
  return _val;
}

/* /home/yu/gcc/gcc_powerpc/crosstool-0.42/build/powerpc-7450-linux-gnu/gcc-3.3.1-glibc-2.3.2/gcc-3.3.1/gcc/config/rs6000/rs6000.md:12576 */
extern rtx gen_split_1089 PARAMS ((rtx *));
rtx
gen_split_1089 (operands)
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
  emit_insn (gen_rtx_SET (VOIDmode,
	operand0,
	gen_rtx_PLUS (SImode,
	gen_rtx_LEU (SImode,
	operand1,
	operand2),
	operand3)));
  emit_insn (gen_rtx_SET (VOIDmode,
	operand4,
	gen_rtx_COMPARE (CCmode,
	copy_rtx (operand0),
	const0_rtx)));
  _val = get_insns ();
  end_sequence ();
  return _val;
}

/* /home/yu/gcc/gcc_powerpc/crosstool-0.42/build/powerpc-7450-linux-gnu/gcc-3.3.1-glibc-2.3.2/gcc-3.3.1/gcc/config/rs6000/rs6000.md:12627 */
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

  operand0 = operands[0];
  operand1 = operands[1];
  operand2 = operands[2];
  operand3 = operands[3];
  operand4 = operands[4];
  emit_insn (gen_rtx_SET (VOIDmode,
	operand4,
	gen_rtx_AND (SImode,
	gen_rtx_NEG (SImode,
	gen_rtx_LEU (SImode,
	operand1,
	operand2)),
	operand3)));
  emit_insn (gen_rtx_SET (VOIDmode,
	operand0,
	gen_rtx_COMPARE (CCmode,
	copy_rtx (operand4),
	const0_rtx)));
  _val = get_insns ();
  end_sequence ();
  return _val;
}

/* /home/yu/gcc/gcc_powerpc/crosstool-0.42/build/powerpc-7450-linux-gnu/gcc-3.3.1-glibc-2.3.2/gcc-3.3.1/gcc/config/rs6000/rs6000.md:12662 */
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

  operand0 = operands[0];
  operand1 = operands[1];
  operand2 = operands[2];
  operand3 = operands[3];
  operand4 = operands[4];
  emit_insn (gen_rtx_SET (VOIDmode,
	operand0,
	gen_rtx_AND (SImode,
	gen_rtx_NEG (SImode,
	gen_rtx_LEU (SImode,
	operand1,
	operand2)),
	operand3)));
  emit_insn (gen_rtx_SET (VOIDmode,
	operand4,
	gen_rtx_COMPARE (CCmode,
	copy_rtx (operand0),
	const0_rtx)));
  _val = get_insns ();
  end_sequence ();
  return _val;
}

/* /home/yu/gcc/gcc_powerpc/crosstool-0.42/build/powerpc-7450-linux-gnu/gcc-3.3.1-glibc-2.3.2/gcc-3.3.1/gcc/config/rs6000/rs6000.md:12704 */
extern rtx gen_split_1092 PARAMS ((rtx *));
rtx
gen_split_1092 (operands)
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
	operand0,
	gen_rtx_LT (SImode,
	operand1,
	operand2)));
  emit_insn (gen_rtx_SET (VOIDmode,
	operand3,
	gen_rtx_COMPARE (CCmode,
	copy_rtx (operand0),
	const0_rtx)));
  _val = get_insns ();
  end_sequence ();
  return _val;
}

/* /home/yu/gcc/gcc_powerpc/crosstool-0.42/build/powerpc-7450-linux-gnu/gcc-3.3.1-glibc-2.3.2/gcc-3.3.1/gcc/config/rs6000/rs6000.md:12744 */
extern rtx gen_split_1093 PARAMS ((rtx *));
rtx
gen_split_1093 (operands)
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
  emit_insn (gen_rtx_SET (VOIDmode,
	operand4,
	gen_rtx_PLUS (SImode,
	gen_rtx_LT (SImode,
	operand1,
	operand2),
	operand3)));
  emit_insn (gen_rtx_SET (VOIDmode,
	operand0,
	gen_rtx_COMPARE (CCmode,
	copy_rtx (operand4),
	const0_rtx)));
  _val = get_insns ();
  end_sequence ();
  return _val;
}

/* /home/yu/gcc/gcc_powerpc/crosstool-0.42/build/powerpc-7450-linux-gnu/gcc-3.3.1-glibc-2.3.2/gcc-3.3.1/gcc/config/rs6000/rs6000.md:12777 */
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

  operand0 = operands[0];
  operand1 = operands[1];
  operand2 = operands[2];
  operand3 = operands[3];
  operand4 = operands[4];
  emit_insn (gen_rtx_SET (VOIDmode,
	operand0,
	gen_rtx_PLUS (SImode,
	gen_rtx_LT (SImode,
	operand1,
	operand2),
	operand3)));
  emit_insn (gen_rtx_SET (VOIDmode,
	operand4,
	gen_rtx_COMPARE (CCmode,
	copy_rtx (operand0),
	const0_rtx)));
  _val = get_insns ();
  end_sequence ();
  return _val;
}

/* /home/yu/gcc/gcc_powerpc/crosstool-0.42/build/powerpc-7450-linux-gnu/gcc-3.3.1-glibc-2.3.2/gcc-3.3.1/gcc/config/rs6000/rs6000.md:12829 */
extern rtx gen_split_1095 PARAMS ((rtx *));
rtx
gen_split_1095 (operands)
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
	operand0,
	gen_rtx_LTU (SImode,
	operand1,
	operand2)));
  emit_insn (gen_rtx_SET (VOIDmode,
	operand3,
	gen_rtx_COMPARE (CCmode,
	copy_rtx (operand0),
	const0_rtx)));
  _val = get_insns ();
  end_sequence ();
  return _val;
}

/* /home/yu/gcc/gcc_powerpc/crosstool-0.42/build/powerpc-7450-linux-gnu/gcc-3.3.1-glibc-2.3.2/gcc-3.3.1/gcc/config/rs6000/rs6000.md:12873 */
extern rtx gen_split_1096 PARAMS ((rtx *));
rtx
gen_split_1096 (operands)
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
  emit_insn (gen_rtx_SET (VOIDmode,
	operand4,
	gen_rtx_PLUS (SImode,
	gen_rtx_LTU (SImode,
	operand1,
	operand2),
	operand3)));
  emit_insn (gen_rtx_SET (VOIDmode,
	operand0,
	gen_rtx_COMPARE (CCmode,
	copy_rtx (operand4),
	const0_rtx)));
  _val = get_insns ();
  end_sequence ();
  return _val;
}

/* /home/yu/gcc/gcc_powerpc/crosstool-0.42/build/powerpc-7450-linux-gnu/gcc-3.3.1-glibc-2.3.2/gcc-3.3.1/gcc/config/rs6000/rs6000.md:12908 */
extern rtx gen_split_1097 PARAMS ((rtx *));
rtx
gen_split_1097 (operands)
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
  emit_insn (gen_rtx_SET (VOIDmode,
	operand0,
	gen_rtx_PLUS (SImode,
	gen_rtx_LTU (SImode,
	operand1,
	operand2),
	operand3)));
  emit_insn (gen_rtx_SET (VOIDmode,
	operand4,
	gen_rtx_COMPARE (CCmode,
	copy_rtx (operand0),
	const0_rtx)));
  _val = get_insns ();
  end_sequence ();
  return _val;
}

/* /home/yu/gcc/gcc_powerpc/crosstool-0.42/build/powerpc-7450-linux-gnu/gcc-3.3.1-glibc-2.3.2/gcc-3.3.1/gcc/config/rs6000/rs6000.md:12960 */
extern rtx gen_split_1098 PARAMS ((rtx *));
rtx
gen_split_1098 (operands)
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
	gen_rtvec (2,
		gen_rtx_SET (VOIDmode,
	operand0,
	gen_rtx_GE (SImode,
	operand1,
	operand2)),
		gen_rtx_CLOBBER (VOIDmode,
	operand3))));
  emit_insn (gen_rtx_SET (VOIDmode,
	operand4,
	gen_rtx_COMPARE (CCmode,
	copy_rtx (operand0),
	const0_rtx)));
  _val = get_insns ();
  end_sequence ();
  return _val;
}

/* /home/yu/gcc/gcc_powerpc/crosstool-0.42/build/powerpc-7450-linux-gnu/gcc-3.3.1-glibc-2.3.2/gcc-3.3.1/gcc/config/rs6000/rs6000.md:13002 */
extern rtx gen_split_1099 PARAMS ((rtx *));
rtx
gen_split_1099 (operands)
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
  emit_insn (gen_rtx_SET (VOIDmode,
	operand4,
	gen_rtx_PLUS (SImode,
	gen_rtx_GE (SImode,
	operand1,
	operand2),
	operand3)));
  emit_insn (gen_rtx_SET (VOIDmode,
	operand0,
	gen_rtx_COMPARE (CCmode,
	copy_rtx (operand4),
	const0_rtx)));
  _val = get_insns ();
  end_sequence ();
  return _val;
}

/* /home/yu/gcc/gcc_powerpc/crosstool-0.42/build/powerpc-7450-linux-gnu/gcc-3.3.1-glibc-2.3.2/gcc-3.3.1/gcc/config/rs6000/rs6000.md:13035 */
extern rtx gen_split_1100 PARAMS ((rtx *));
rtx
gen_split_1100 (operands)
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
  emit_insn (gen_rtx_SET (VOIDmode,
	operand0,
	gen_rtx_PLUS (SImode,
	gen_rtx_GE (SImode,
	operand1,
	operand2),
	operand3)));
  emit_insn (gen_rtx_SET (VOIDmode,
	operand4,
	gen_rtx_COMPARE (CCmode,
	copy_rtx (operand0),
	const0_rtx)));
  _val = get_insns ();
  end_sequence ();
  return _val;
}

/* /home/yu/gcc/gcc_powerpc/crosstool-0.42/build/powerpc-7450-linux-gnu/gcc-3.3.1-glibc-2.3.2/gcc-3.3.1/gcc/config/rs6000/rs6000.md:13097 */
extern rtx gen_split_1101 PARAMS ((rtx *));
rtx
gen_split_1101 (operands)
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
	operand0,
	gen_rtx_GEU (SImode,
	operand1,
	operand2)));
  emit_insn (gen_rtx_SET (VOIDmode,
	operand3,
	gen_rtx_COMPARE (CCmode,
	copy_rtx (operand0),
	const0_rtx)));
  _val = get_insns ();
  end_sequence ();
  return _val;
}

/* /home/yu/gcc/gcc_powerpc/crosstool-0.42/build/powerpc-7450-linux-gnu/gcc-3.3.1-glibc-2.3.2/gcc-3.3.1/gcc/config/rs6000/rs6000.md:13130 */
extern rtx gen_split_1102 PARAMS ((rtx *));
rtx
gen_split_1102 (operands)
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
	operand0,
	gen_rtx_GEU (DImode,
	operand1,
	operand2)));
  emit_insn (gen_rtx_SET (VOIDmode,
	operand3,
	gen_rtx_COMPARE (CCmode,
	copy_rtx (operand0),
	const0_rtx)));
  _val = get_insns ();
  end_sequence ();
  return _val;
}

/* /home/yu/gcc/gcc_powerpc/crosstool-0.42/build/powerpc-7450-linux-gnu/gcc-3.3.1-glibc-2.3.2/gcc-3.3.1/gcc/config/rs6000/rs6000.md:13174 */
extern rtx gen_split_1103 PARAMS ((rtx *));
rtx
gen_split_1103 (operands)
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
  emit_insn (gen_rtx_SET (VOIDmode,
	operand4,
	gen_rtx_PLUS (SImode,
	gen_rtx_GEU (SImode,
	operand1,
	operand2),
	operand3)));
  emit_insn (gen_rtx_SET (VOIDmode,
	operand0,
	gen_rtx_COMPARE (CCmode,
	copy_rtx (operand4),
	const0_rtx)));
  _val = get_insns ();
  end_sequence ();
  return _val;
}

/* /home/yu/gcc/gcc_powerpc/crosstool-0.42/build/powerpc-7450-linux-gnu/gcc-3.3.1-glibc-2.3.2/gcc-3.3.1/gcc/config/rs6000/rs6000.md:13209 */
extern rtx gen_split_1104 PARAMS ((rtx *));
rtx
gen_split_1104 (operands)
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
  emit_insn (gen_rtx_SET (VOIDmode,
	operand0,
	gen_rtx_PLUS (SImode,
	gen_rtx_GEU (SImode,
	operand1,
	operand2),
	operand3)));
  emit_insn (gen_rtx_SET (VOIDmode,
	operand4,
	gen_rtx_COMPARE (CCmode,
	copy_rtx (operand0),
	const0_rtx)));
  _val = get_insns ();
  end_sequence ();
  return _val;
}

/* /home/yu/gcc/gcc_powerpc/crosstool-0.42/build/powerpc-7450-linux-gnu/gcc-3.3.1-glibc-2.3.2/gcc-3.3.1/gcc/config/rs6000/rs6000.md:13266 */
extern rtx gen_split_1105 PARAMS ((rtx *));
rtx
gen_split_1105 (operands)
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
  emit_insn (gen_rtx_SET (VOIDmode,
	operand4,
	gen_rtx_AND (SImode,
	gen_rtx_NEG (SImode,
	gen_rtx_GEU (SImode,
	operand1,
	operand2)),
	operand3)));
  emit_insn (gen_rtx_SET (VOIDmode,
	operand0,
	gen_rtx_COMPARE (CCmode,
	copy_rtx (operand4),
	const0_rtx)));
  _val = get_insns ();
  end_sequence ();
  return _val;
}

/* /home/yu/gcc/gcc_powerpc/crosstool-0.42/build/powerpc-7450-linux-gnu/gcc-3.3.1-glibc-2.3.2/gcc-3.3.1/gcc/config/rs6000/rs6000.md:13303 */
extern rtx gen_split_1106 PARAMS ((rtx *));
rtx
gen_split_1106 (operands)
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
  emit_insn (gen_rtx_SET (VOIDmode,
	operand0,
	gen_rtx_AND (SImode,
	gen_rtx_NEG (SImode,
	gen_rtx_GEU (SImode,
	operand1,
	operand2)),
	operand3)));
  emit_insn (gen_rtx_SET (VOIDmode,
	operand4,
	gen_rtx_COMPARE (CCmode,
	copy_rtx (operand0),
	const0_rtx)));
  _val = get_insns ();
  end_sequence ();
  return _val;
}

/* /home/yu/gcc/gcc_powerpc/crosstool-0.42/build/powerpc-7450-linux-gnu/gcc-3.3.1-glibc-2.3.2/gcc-3.3.1/gcc/config/rs6000/rs6000.md:13352 */
extern rtx gen_split_1107 PARAMS ((rtx *));
rtx
gen_split_1107 (operands)
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
	gen_rtx_GT (SImode,
	operand1,
	const0_rtx)));
  emit_insn (gen_rtx_SET (VOIDmode,
	operand2,
	gen_rtx_COMPARE (CCmode,
	copy_rtx (operand0),
	const0_rtx)));
  _val = get_insns ();
  end_sequence ();
  return _val;
}

/* /home/yu/gcc/gcc_powerpc/crosstool-0.42/build/powerpc-7450-linux-gnu/gcc-3.3.1-glibc-2.3.2/gcc-3.3.1/gcc/config/rs6000/rs6000.md:13383 */
extern rtx gen_split_1108 PARAMS ((rtx *));
rtx
gen_split_1108 (operands)
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
	gen_rtx_GT (DImode,
	operand1,
	const0_rtx)));
  emit_insn (gen_rtx_SET (VOIDmode,
	operand2,
	gen_rtx_COMPARE (CCmode,
	copy_rtx (operand0),
	const0_rtx)));
  _val = get_insns ();
  end_sequence ();
  return _val;
}

/* /home/yu/gcc/gcc_powerpc/crosstool-0.42/build/powerpc-7450-linux-gnu/gcc-3.3.1-glibc-2.3.2/gcc-3.3.1/gcc/config/rs6000/rs6000.md:13422 */
extern rtx gen_split_1109 PARAMS ((rtx *));
rtx
gen_split_1109 (operands)
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
	operand0,
	gen_rtx_GT (SImode,
	operand1,
	operand2)));
  emit_insn (gen_rtx_SET (VOIDmode,
	operand3,
	gen_rtx_COMPARE (CCmode,
	copy_rtx (operand0),
	const0_rtx)));
  _val = get_insns ();
  end_sequence ();
  return _val;
}

/* /home/yu/gcc/gcc_powerpc/crosstool-0.42/build/powerpc-7450-linux-gnu/gcc-3.3.1-glibc-2.3.2/gcc-3.3.1/gcc/config/rs6000/rs6000.md:13471 */
extern rtx gen_split_1110 PARAMS ((rtx *));
rtx
gen_split_1110 (operands)
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
	gen_rtx_PLUS (SImode,
	gen_rtx_GT (SImode,
	operand1,
	const0_rtx),
	operand2)));
  emit_insn (gen_rtx_SET (VOIDmode,
	operand0,
	gen_rtx_COMPARE (CCmode,
	copy_rtx (operand3),
	const0_rtx)));
  _val = get_insns ();
  end_sequence ();
  return _val;
}

/* /home/yu/gcc/gcc_powerpc/crosstool-0.42/build/powerpc-7450-linux-gnu/gcc-3.3.1-glibc-2.3.2/gcc-3.3.1/gcc/config/rs6000/rs6000.md:13503 */
extern rtx gen_split_1111 PARAMS ((rtx *));
rtx
gen_split_1111 (operands)
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
	gen_rtx_PLUS (DImode,
	gen_rtx_GT (DImode,
	operand1,
	const0_rtx),
	operand2)));
  emit_insn (gen_rtx_SET (VOIDmode,
	operand0,
	gen_rtx_COMPARE (CCmode,
	copy_rtx (operand3),
	const0_rtx)));
  _val = get_insns ();
  end_sequence ();
  return _val;
}

/* /home/yu/gcc/gcc_powerpc/crosstool-0.42/build/powerpc-7450-linux-gnu/gcc-3.3.1-glibc-2.3.2/gcc-3.3.1/gcc/config/rs6000/rs6000.md:13536 */
extern rtx gen_split_1112 PARAMS ((rtx *));
rtx
gen_split_1112 (operands)
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
	operand0,
	gen_rtx_PLUS (SImode,
	gen_rtx_GT (SImode,
	operand1,
	const0_rtx),
	operand2)));
  emit_insn (gen_rtx_SET (VOIDmode,
	operand3,
	gen_rtx_COMPARE (CCmode,
	copy_rtx (operand0),
	const0_rtx)));
  _val = get_insns ();
  end_sequence ();
  return _val;
}

/* /home/yu/gcc/gcc_powerpc/crosstool-0.42/build/powerpc-7450-linux-gnu/gcc-3.3.1-glibc-2.3.2/gcc-3.3.1/gcc/config/rs6000/rs6000.md:13569 */
extern rtx gen_split_1113 PARAMS ((rtx *));
rtx
gen_split_1113 (operands)
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
	operand0,
	gen_rtx_PLUS (DImode,
	gen_rtx_GT (DImode,
	operand1,
	const0_rtx),
	operand2)));
  emit_insn (gen_rtx_SET (VOIDmode,
	operand3,
	gen_rtx_COMPARE (CCmode,
	copy_rtx (operand0),
	const0_rtx)));
  _val = get_insns ();
  end_sequence ();
  return _val;
}

/* /home/yu/gcc/gcc_powerpc/crosstool-0.42/build/powerpc-7450-linux-gnu/gcc-3.3.1-glibc-2.3.2/gcc-3.3.1/gcc/config/rs6000/rs6000.md:13610 */
extern rtx gen_split_1114 PARAMS ((rtx *));
rtx
gen_split_1114 (operands)
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
  emit_insn (gen_rtx_SET (VOIDmode,
	operand4,
	gen_rtx_PLUS (SImode,
	gen_rtx_GT (SImode,
	operand1,
	operand2),
	operand3)));
  emit_insn (gen_rtx_SET (VOIDmode,
	operand0,
	gen_rtx_COMPARE (CCmode,
	copy_rtx (operand4),
	const0_rtx)));
  _val = get_insns ();
  end_sequence ();
  return _val;
}

/* /home/yu/gcc/gcc_powerpc/crosstool-0.42/build/powerpc-7450-linux-gnu/gcc-3.3.1-glibc-2.3.2/gcc-3.3.1/gcc/config/rs6000/rs6000.md:13642 */
extern rtx gen_split_1115 PARAMS ((rtx *));
rtx
gen_split_1115 (operands)
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
  emit_insn (gen_rtx_SET (VOIDmode,
	operand0,
	gen_rtx_PLUS (SImode,
	gen_rtx_GT (SImode,
	operand1,
	operand2),
	operand3)));
  emit_insn (gen_rtx_SET (VOIDmode,
	operand4,
	gen_rtx_COMPARE (CCmode,
	copy_rtx (operand0),
	const0_rtx)));
  _val = get_insns ();
  end_sequence ();
  return _val;
}

/* /home/yu/gcc/gcc_powerpc/crosstool-0.42/build/powerpc-7450-linux-gnu/gcc-3.3.1-glibc-2.3.2/gcc-3.3.1/gcc/config/rs6000/rs6000.md:13714 */
extern rtx gen_split_1116 PARAMS ((rtx *));
rtx
gen_split_1116 (operands)
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
	operand0,
	gen_rtx_GTU (SImode,
	operand1,
	operand2)));
  emit_insn (gen_rtx_SET (VOIDmode,
	operand3,
	gen_rtx_COMPARE (CCmode,
	copy_rtx (operand0),
	const0_rtx)));
  _val = get_insns ();
  end_sequence ();
  return _val;
}

/* /home/yu/gcc/gcc_powerpc/crosstool-0.42/build/powerpc-7450-linux-gnu/gcc-3.3.1-glibc-2.3.2/gcc-3.3.1/gcc/config/rs6000/rs6000.md:13745 */
extern rtx gen_split_1117 PARAMS ((rtx *));
rtx
gen_split_1117 (operands)
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
	operand0,
	gen_rtx_GTU (DImode,
	operand1,
	operand2)));
  emit_insn (gen_rtx_SET (VOIDmode,
	operand3,
	gen_rtx_COMPARE (CCmode,
	copy_rtx (operand0),
	const0_rtx)));
  _val = get_insns ();
  end_sequence ();
  return _val;
}

/* /home/yu/gcc/gcc_powerpc/crosstool-0.42/build/powerpc-7450-linux-gnu/gcc-3.3.1-glibc-2.3.2/gcc-3.3.1/gcc/config/rs6000/rs6000.md:13800 */
extern rtx gen_split_1118 PARAMS ((rtx *));
rtx
gen_split_1118 (operands)
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
  emit_insn (gen_rtx_SET (VOIDmode,
	operand4,
	gen_rtx_PLUS (SImode,
	gen_rtx_GTU (SImode,
	operand1,
	operand2),
	operand3)));
  emit_insn (gen_rtx_SET (VOIDmode,
	operand0,
	gen_rtx_COMPARE (CCmode,
	copy_rtx (operand4),
	const0_rtx)));
  _val = get_insns ();
  end_sequence ();
  return _val;
}

/* /home/yu/gcc/gcc_powerpc/crosstool-0.42/build/powerpc-7450-linux-gnu/gcc-3.3.1-glibc-2.3.2/gcc-3.3.1/gcc/config/rs6000/rs6000.md:13834 */
extern rtx gen_split_1119 PARAMS ((rtx *));
rtx
gen_split_1119 (operands)
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
  emit_insn (gen_rtx_SET (VOIDmode,
	operand4,
	gen_rtx_PLUS (DImode,
	gen_rtx_GTU (DImode,
	operand1,
	operand2),
	operand3)));
  emit_insn (gen_rtx_SET (VOIDmode,
	operand0,
	gen_rtx_COMPARE (CCmode,
	copy_rtx (operand4),
	const0_rtx)));
  _val = get_insns ();
  end_sequence ();
  return _val;
}

/* /home/yu/gcc/gcc_powerpc/crosstool-0.42/build/powerpc-7450-linux-gnu/gcc-3.3.1-glibc-2.3.2/gcc-3.3.1/gcc/config/rs6000/rs6000.md:13869 */
extern rtx gen_split_1120 PARAMS ((rtx *));
rtx
gen_split_1120 (operands)
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
  emit_insn (gen_rtx_SET (VOIDmode,
	operand0,
	gen_rtx_PLUS (SImode,
	gen_rtx_GTU (SImode,
	operand1,
	operand2),
	operand3)));
  emit_insn (gen_rtx_SET (VOIDmode,
	operand4,
	gen_rtx_COMPARE (CCmode,
	copy_rtx (operand0),
	const0_rtx)));
  _val = get_insns ();
  end_sequence ();
  return _val;
}

/* /home/yu/gcc/gcc_powerpc/crosstool-0.42/build/powerpc-7450-linux-gnu/gcc-3.3.1-glibc-2.3.2/gcc-3.3.1/gcc/config/rs6000/rs6000.md:13904 */
extern rtx gen_split_1121 PARAMS ((rtx *));
rtx
gen_split_1121 (operands)
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
  emit_insn (gen_rtx_SET (VOIDmode,
	operand0,
	gen_rtx_PLUS (DImode,
	gen_rtx_GTU (DImode,
	operand1,
	operand2),
	operand3)));
  emit_insn (gen_rtx_SET (VOIDmode,
	operand4,
	gen_rtx_COMPARE (CCmode,
	copy_rtx (operand0),
	const0_rtx)));
  _val = get_insns ();
  end_sequence ();
  return _val;
}

/* /home/yu/gcc/gcc_powerpc/crosstool-0.42/build/powerpc-7450-linux-gnu/gcc-3.3.1-glibc-2.3.2/gcc-3.3.1/gcc/config/rs6000/rs6000.md:14064 */
extern rtx gen_split_1122 PARAMS ((rtx *));
rtx
gen_split_1122 (operands)
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

{
  int positive_1, positive_2;

  positive_1 = branch_positive_comparison_operator (operands[1], CCEQmode);
  positive_2 = branch_positive_comparison_operator (operands[3], CCEQmode);

  if (! positive_1)
    operands[1] = gen_rtx (rs6000_reverse_condition (GET_MODE (operands[2]),
						     GET_CODE (operands[1])),
			   SImode,
			   operands[2], const0_rtx);
  else if (GET_MODE (operands[1]) != SImode)
    operands[1] = gen_rtx (GET_CODE (operands[1]),
			   SImode,
			   operands[2], const0_rtx);

  if (! positive_2)
    operands[3] = gen_rtx (rs6000_reverse_condition (GET_MODE (operands[4]),
						     GET_CODE (operands[3])),
			   SImode,
			   operands[4], const0_rtx);
  else if (GET_MODE (operands[3]) != SImode)
    operands[3] = gen_rtx (GET_CODE (operands[3]),
			   SImode,
			   operands[4], const0_rtx);

  if (positive_1 == positive_2)
    {
      operands[1] = gen_rtx_NOT (SImode, operands[1]);
      operands[5] = constm1_rtx;
    }
  else
    {
      operands[5] = const1_rtx;
    }
}
  operand0 = operands[0];
  operand1 = operands[1];
  operand2 = operands[2];
  operand3 = operands[3];
  operand4 = operands[4];
  operand5 = operands[5];
  emit_insn (gen_rtx_SET (VOIDmode,
	operand0,
	gen_rtx_COMPARE (CCEQmode,
	gen_rtx_XOR (SImode,
	operand1,
	operand3),
	operand5)));
  _val = get_insns ();
  end_sequence ();
  return _val;
}

/* /home/yu/gcc/gcc_powerpc/crosstool-0.42/build/powerpc-7450-linux-gnu/gcc-3.3.1-glibc-2.3.2/gcc-3.3.1/gcc/config/rs6000/rs6000.md:14130 */
rtx
gen_indirect_jump (operand0)
     rtx operand0;
{
  rtx _val = 0;
  start_sequence ();
  {
    rtx operands[1];
    operands[0] = operand0;

{
  if (TARGET_32BIT)
    emit_jump_insn (gen_indirect_jumpsi (operands[0]));
  else
    emit_jump_insn (gen_indirect_jumpdi (operands[0]));
  DONE;
}
    operand0 = operands[0];
  }
  emit_jump_insn (gen_rtx_SET (VOIDmode,
	pc_rtx,
	operand0));
  _val = get_insns ();
  end_sequence ();
  return _val;
}

/* /home/yu/gcc/gcc_powerpc/crosstool-0.42/build/powerpc-7450-linux-gnu/gcc-3.3.1-glibc-2.3.2/gcc-3.3.1/gcc/config/rs6000/rs6000.md:14159 */
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
  if (TARGET_32BIT)
    emit_jump_insn (gen_tablejumpsi (operands[0], operands[1]));
  else
    emit_jump_insn (gen_tablejumpdi (operands[0], operands[1]));
  DONE;
}
    operand0 = operands[0];
    operand1 = operands[1];
  }
  emit_insn (gen_rtx_USE (VOIDmode,
	operand0));
  emit_insn (gen_rtx_USE (VOIDmode,
	gen_rtx_LABEL_REF (VOIDmode,
	operand1)));
  _val = get_insns ();
  end_sequence ();
  return _val;
}

/* /home/yu/gcc/gcc_powerpc/crosstool-0.42/build/powerpc-7450-linux-gnu/gcc-3.3.1-glibc-2.3.2/gcc-3.3.1/gcc/config/rs6000/rs6000.md:14172 */
rtx
gen_tablejumpsi (operand0, operand1)
     rtx operand0;
     rtx operand1;
{
  rtx operand2;
  rtx operand3;
  rtx _val = 0;
  start_sequence ();
  {
    rtx operands[4];
    operands[0] = operand0;
    operands[1] = operand1;

{ operands[0] = force_reg (SImode, operands[0]);
  operands[2] = force_reg (SImode, gen_rtx_LABEL_REF (SImode, operands[1]));
  operands[3] = gen_reg_rtx (SImode);
}
    operand0 = operands[0];
    operand1 = operands[1];
    operand2 = operands[2];
    operand3 = operands[3];
  }
  emit_insn (gen_rtx_SET (VOIDmode,
	operand3,
	gen_rtx_PLUS (SImode,
	operand0,
	operand2)));
  emit_jump_insn (gen_rtx_PARALLEL (VOIDmode,
	gen_rtvec (2,
		gen_rtx_SET (VOIDmode,
	pc_rtx,
	operand3),
		gen_rtx_USE (VOIDmode,
	gen_rtx_LABEL_REF (VOIDmode,
	operand1)))));
  _val = get_insns ();
  end_sequence ();
  return _val;
}

/* /home/yu/gcc/gcc_powerpc/crosstool-0.42/build/powerpc-7450-linux-gnu/gcc-3.3.1-glibc-2.3.2/gcc-3.3.1/gcc/config/rs6000/rs6000.md:14185 */
rtx
gen_tablejumpdi (operand0, operand1)
     rtx operand0;
     rtx operand1;
{
  rtx operand2;
  rtx operand3;
  rtx operand4;
  rtx _val = 0;
  start_sequence ();
  {
    rtx operands[5];
    operands[0] = operand0;
    operands[1] = operand1;

{ operands[2] = force_reg (DImode, gen_rtx_LABEL_REF (DImode, operands[1]));
  operands[3] = gen_reg_rtx (DImode);
  operands[4] = gen_reg_rtx (DImode);
}
    operand0 = operands[0];
    operand1 = operands[1];
    operand2 = operands[2];
    operand3 = operands[3];
    operand4 = operands[4];
  }
  emit_insn (gen_rtx_SET (VOIDmode,
	operand4,
	gen_rtx_SIGN_EXTEND (DImode,
	operand0)));
  emit_insn (gen_rtx_SET (VOIDmode,
	operand3,
	gen_rtx_PLUS (DImode,
	operand4,
	operand2)));
  emit_jump_insn (gen_rtx_PARALLEL (VOIDmode,
	gen_rtvec (2,
		gen_rtx_SET (VOIDmode,
	pc_rtx,
	operand3),
		gen_rtx_USE (VOIDmode,
	gen_rtx_LABEL_REF (VOIDmode,
	operand1)))));
  _val = get_insns ();
  end_sequence ();
  return _val;
}

/* /home/yu/gcc/gcc_powerpc/crosstool-0.42/build/powerpc-7450-linux-gnu/gcc-3.3.1-glibc-2.3.2/gcc-3.3.1/gcc/config/rs6000/rs6000.md:14228 */
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
  /* Only use this on innermost loops.  */
  if (INTVAL (operands[3]) > 1)
    FAIL;
  if (TARGET_POWERPC64)
    {
      if (GET_MODE (operands[0]) != DImode)
	FAIL;
      emit_jump_insn (gen_ctrdi (operands[0], operands[4]));
    }
  else
    {
      if (GET_MODE (operands[0]) != SImode)
	FAIL;
      emit_jump_insn (gen_ctrsi (operands[0], operands[4]));
    }
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

/* /home/yu/gcc/gcc_powerpc/crosstool-0.42/build/powerpc-7450-linux-gnu/gcc-3.3.1-glibc-2.3.2/gcc-3.3.1/gcc/config/rs6000/rs6000.md:14255 */
rtx
gen_ctrsi (operand0, operand1)
     rtx operand0;
     rtx operand1;
{
  return gen_rtx_PARALLEL (VOIDmode,
	gen_rtvec (4,
		gen_rtx_SET (VOIDmode,
	pc_rtx,
	gen_rtx_IF_THEN_ELSE (VOIDmode,
	gen_rtx_NE (VOIDmode,
	operand0,
	const1_rtx),
	gen_rtx_LABEL_REF (VOIDmode,
	operand1),
	pc_rtx)),
		gen_rtx_SET (VOIDmode,
	operand0,
	gen_rtx_PLUS (SImode,
	operand0,
	constm1_rtx)),
		gen_rtx_CLOBBER (VOIDmode,
	gen_rtx_SCRATCH (CCmode)),
		gen_rtx_CLOBBER (VOIDmode,
	gen_rtx_SCRATCH (SImode))));
}

/* /home/yu/gcc/gcc_powerpc/crosstool-0.42/build/powerpc-7450-linux-gnu/gcc-3.3.1-glibc-2.3.2/gcc-3.3.1/gcc/config/rs6000/rs6000.md:14269 */
rtx
gen_ctrdi (operand0, operand1)
     rtx operand0;
     rtx operand1;
{
  return gen_rtx_PARALLEL (VOIDmode,
	gen_rtvec (4,
		gen_rtx_SET (VOIDmode,
	pc_rtx,
	gen_rtx_IF_THEN_ELSE (VOIDmode,
	gen_rtx_NE (VOIDmode,
	operand0,
	const1_rtx),
	gen_rtx_LABEL_REF (VOIDmode,
	operand1),
	pc_rtx)),
		gen_rtx_SET (VOIDmode,
	operand0,
	gen_rtx_PLUS (DImode,
	operand0,
	constm1_rtx)),
		gen_rtx_CLOBBER (VOIDmode,
	gen_rtx_SCRATCH (CCmode)),
		gen_rtx_CLOBBER (VOIDmode,
	gen_rtx_SCRATCH (DImode))));
}

/* /home/yu/gcc/gcc_powerpc/crosstool-0.42/build/powerpc-7450-linux-gnu/gcc-3.3.1-glibc-2.3.2/gcc-3.3.1/gcc/config/rs6000/rs6000.md:14583 */
extern rtx gen_split_1130 PARAMS ((rtx *));
rtx
gen_split_1130 (operands)
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

{ operands[7] = gen_rtx (GET_CODE (operands[2]), VOIDmode, operands[3],
			 const0_rtx); }
  operand0 = operands[0];
  operand1 = operands[1];
  operand2 = operands[2];
  operand3 = operands[3];
  operand4 = operands[4];
  operand5 = operands[5];
  operand6 = operands[6];
  operand7 = operands[7];
  emit (gen_rtx_PARALLEL (VOIDmode,
	gen_rtvec (2,
		gen_rtx_SET (VOIDmode,
	operand3,
	gen_rtx_COMPARE (CCmode,
	gen_rtx_PLUS (SImode,
	operand1,
	constm1_rtx),
	const0_rtx)),
		gen_rtx_SET (VOIDmode,
	operand0,
	gen_rtx_PLUS (SImode,
	copy_rtx (operand1),
	constm1_rtx)))));
  emit_jump_insn (gen_rtx_SET (VOIDmode,
	pc_rtx,
	gen_rtx_IF_THEN_ELSE (VOIDmode,
	operand7,
	operand5,
	operand6)));
  _val = get_insns ();
  end_sequence ();
  return _val;
}

/* /home/yu/gcc/gcc_powerpc/crosstool-0.42/build/powerpc-7450-linux-gnu/gcc-3.3.1-glibc-2.3.2/gcc-3.3.1/gcc/config/rs6000/rs6000.md:14610 */
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
  rtx operand6;
  rtx operand7;
  rtx _val = 0;
  start_sequence ();

{ operands[7] = gen_rtx (GET_CODE (operands[2]), VOIDmode, operands[3],
			 const0_rtx); }
  operand0 = operands[0];
  operand1 = operands[1];
  operand2 = operands[2];
  operand3 = operands[3];
  operand4 = operands[4];
  operand5 = operands[5];
  operand6 = operands[6];
  operand7 = operands[7];
  emit (gen_rtx_PARALLEL (VOIDmode,
	gen_rtvec (2,
		gen_rtx_SET (VOIDmode,
	operand3,
	gen_rtx_COMPARE (CCmode,
	gen_rtx_PLUS (SImode,
	operand1,
	constm1_rtx),
	const0_rtx)),
		gen_rtx_SET (VOIDmode,
	operand4,
	gen_rtx_PLUS (SImode,
	copy_rtx (operand1),
	constm1_rtx)))));
  emit_insn (gen_rtx_SET (VOIDmode,
	operand0,
	copy_rtx (operand4)));
  emit_jump_insn (gen_rtx_SET (VOIDmode,
	pc_rtx,
	gen_rtx_IF_THEN_ELSE (VOIDmode,
	operand7,
	operand5,
	operand6)));
  _val = get_insns ();
  end_sequence ();
  return _val;
}

/* /home/yu/gcc/gcc_powerpc/crosstool-0.42/build/powerpc-7450-linux-gnu/gcc-3.3.1-glibc-2.3.2/gcc-3.3.1/gcc/config/rs6000/rs6000.md:14638 */
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
  rtx operand6;
  rtx operand7;
  rtx _val = 0;
  start_sequence ();

{ operands[7] = gen_rtx (GET_CODE (operands[2]), VOIDmode, operands[3],
			 const0_rtx); }
  operand0 = operands[0];
  operand1 = operands[1];
  operand2 = operands[2];
  operand3 = operands[3];
  operand4 = operands[4];
  operand5 = operands[5];
  operand6 = operands[6];
  operand7 = operands[7];
  emit (gen_rtx_PARALLEL (VOIDmode,
	gen_rtvec (2,
		gen_rtx_SET (VOIDmode,
	operand3,
	gen_rtx_COMPARE (CCmode,
	gen_rtx_PLUS (DImode,
	operand1,
	constm1_rtx),
	const0_rtx)),
		gen_rtx_SET (VOIDmode,
	operand0,
	gen_rtx_PLUS (DImode,
	copy_rtx (operand1),
	constm1_rtx)))));
  emit_jump_insn (gen_rtx_SET (VOIDmode,
	pc_rtx,
	gen_rtx_IF_THEN_ELSE (VOIDmode,
	operand7,
	operand5,
	operand6)));
  _val = get_insns ();
  end_sequence ();
  return _val;
}

/* /home/yu/gcc/gcc_powerpc/crosstool-0.42/build/powerpc-7450-linux-gnu/gcc-3.3.1-glibc-2.3.2/gcc-3.3.1/gcc/config/rs6000/rs6000.md:14665 */
extern rtx gen_split_1133 PARAMS ((rtx *));
rtx
gen_split_1133 (operands)
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

{ operands[7] = gen_rtx (GET_CODE (operands[2]), VOIDmode, operands[3],
			 const0_rtx); }
  operand0 = operands[0];
  operand1 = operands[1];
  operand2 = operands[2];
  operand3 = operands[3];
  operand4 = operands[4];
  operand5 = operands[5];
  operand6 = operands[6];
  operand7 = operands[7];
  emit (gen_rtx_PARALLEL (VOIDmode,
	gen_rtvec (2,
		gen_rtx_SET (VOIDmode,
	operand3,
	gen_rtx_COMPARE (CCmode,
	gen_rtx_PLUS (DImode,
	operand1,
	constm1_rtx),
	const0_rtx)),
		gen_rtx_SET (VOIDmode,
	operand4,
	gen_rtx_PLUS (DImode,
	copy_rtx (operand1),
	constm1_rtx)))));
  emit_insn (gen_rtx_SET (VOIDmode,
	operand0,
	copy_rtx (operand4)));
  emit_jump_insn (gen_rtx_SET (VOIDmode,
	pc_rtx,
	gen_rtx_IF_THEN_ELSE (VOIDmode,
	operand7,
	operand5,
	operand6)));
  _val = get_insns ();
  end_sequence ();
  return _val;
}

/* /home/yu/gcc/gcc_powerpc/crosstool-0.42/build/powerpc-7450-linux-gnu/gcc-3.3.1-glibc-2.3.2/gcc-3.3.1/gcc/config/rs6000/rs6000.md:14700 */
rtx
gen_conditional_trap (operand0, operand1)
     rtx operand0;
     rtx operand1;
{
  rtx operand2;
  rtx operand3;
  rtx _val = 0;
  start_sequence ();
  {
    rtx operands[4];
    operands[0] = operand0;
    operands[1] = operand1;
if (rs6000_compare_fp_p || operands[1] != const0_rtx) FAIL;
   operands[2] = rs6000_compare_op0;
   operands[3] = rs6000_compare_op1;
    operand0 = operands[0];
    operand1 = operands[1];
    operand2 = operands[2];
    operand3 = operands[3];
  }
  emit_insn (gen_rtx_TRAP_IF (VOIDmode,
	gen_rtx (GET_CODE (operand0), VOIDmode,
		operand2,
		operand3),
	operand1));
  _val = get_insns ();
  end_sequence ();
  return _val;
}

/* /home/yu/gcc/gcc_powerpc/crosstool-0.42/build/powerpc-7450-linux-gnu/gcc-3.3.1-glibc-2.3.2/gcc-3.3.1/gcc/config/rs6000/rs6000.md:14727 */
rtx
gen_prologue ()
{
  rtx _val = 0;
  start_sequence ();
  {

{
      rs6000_emit_prologue ();
      DONE;
}
  }
  emit_insn (gen_rtx_USE (VOIDmode,
	const0_rtx));
  _val = get_insns ();
  end_sequence ();
  return _val;
}

/* /home/yu/gcc/gcc_powerpc/crosstool-0.42/build/powerpc-7450-linux-gnu/gcc-3.3.1-glibc-2.3.2/gcc-3.3.1/gcc/config/rs6000/rs6000.md:14779 */
rtx
gen_epilogue ()
{
  rtx _val = 0;
  start_sequence ();
  {

{
      rs6000_emit_epilogue (FALSE);
      DONE;
}
  }
  emit_insn (gen_rtx_USE (VOIDmode,
	const0_rtx));
  _val = get_insns ();
  end_sequence ();
  return _val;
}

/* /home/yu/gcc/gcc_powerpc/crosstool-0.42/build/powerpc-7450-linux-gnu/gcc-3.3.1-glibc-2.3.2/gcc-3.3.1/gcc/config/rs6000/rs6000.md:14792 */
rtx
gen_movsi_to_cr_one (operand0, operand1)
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
operands[2] = GEN_INT (1 << (75 - REGNO (operands[0])));
    operand0 = operands[0];
    operand1 = operands[1];
    operand2 = operands[2];
  }
  emit_insn (gen_rtx_SET (VOIDmode,
	operand0,
	gen_rtx_UNSPEC (CCmode,
	gen_rtvec (2,
		operand1,
		operand2),
	20)));
  _val = get_insns ();
  end_sequence ();
  return _val;
}

/* /home/yu/gcc/gcc_powerpc/crosstool-0.42/build/powerpc-7450-linux-gnu/gcc-3.3.1-glibc-2.3.2/gcc-3.3.1/gcc/config/rs6000/rs6000.md:14877 */
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
#if TARGET_AIX
  rs6000_emit_eh_toc_restore (EH_RETURN_STACKADJ_RTX);
#endif
  if (TARGET_32BIT)
    emit_insn (gen_eh_set_lr_si (operands[0]));
  else
    emit_insn (gen_eh_set_lr_di (operands[0]));
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

/* /home/yu/gcc/gcc_powerpc/crosstool-0.42/build/powerpc-7450-linux-gnu/gcc-3.3.1-glibc-2.3.2/gcc-3.3.1/gcc/config/rs6000/rs6000.md:14905 */
extern rtx gen_split_1139 PARAMS ((rtx *));
rtx
gen_split_1139 (operands)
      rtx *operands ATTRIBUTE_UNUSED;
{
  rtx _val = 0;
  start_sequence ();

{
  rs6000_stack_t *info = rs6000_stack_info ();

  if (info->lr_save_p)
    {
      rtx frame_rtx = stack_pointer_rtx;
      int sp_offset = 0;
      rtx tmp;

      if (frame_pointer_needed
	  || current_function_calls_alloca
	  || info->total_size > 32767)
	{
	  emit_move_insn (operands[1], gen_rtx_MEM (Pmode, frame_rtx));
	  frame_rtx = operands[1];
	}
      else if (info->push_p)
	sp_offset = info->total_size;

      tmp = plus_constant (frame_rtx, info->lr_save_offset + sp_offset);
      tmp = gen_rtx_MEM (Pmode, tmp);
      emit_move_insn (tmp, operands[0]);
    }
  else
    emit_move_insn (gen_rtx_REG (Pmode, LINK_REGISTER_REGNUM), operands[0]);
  DONE;
}
  emit_insn (const0_rtx);
  _val = get_insns ();
  end_sequence ();
  return _val;
}

/* /home/yu/gcc/gcc_powerpc/crosstool-0.42/build/powerpc-7450-linux-gnu/gcc-3.3.1-glibc-2.3.2/gcc-3.3.1/gcc/config/rs6000/altivec.md:81 */
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
{ rs6000_emit_move (operands[0], operands[1], V4SImode); DONE; }
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

/* /home/yu/gcc/gcc_powerpc/crosstool-0.42/build/powerpc-7450-linux-gnu/gcc-3.3.1-glibc-2.3.2/gcc-3.3.1/gcc/config/rs6000/altivec.md:101 */
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
{ rs6000_emit_move (operands[0], operands[1], V8HImode); DONE; }
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

/* /home/yu/gcc/gcc_powerpc/crosstool-0.42/build/powerpc-7450-linux-gnu/gcc-3.3.1-glibc-2.3.2/gcc-3.3.1/gcc/config/rs6000/altivec.md:121 */
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
{ rs6000_emit_move (operands[0], operands[1], V16QImode); DONE; }
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

/* /home/yu/gcc/gcc_powerpc/crosstool-0.42/build/powerpc-7450-linux-gnu/gcc-3.3.1-glibc-2.3.2/gcc-3.3.1/gcc/config/rs6000/altivec.md:141 */
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
{ rs6000_emit_move (operands[0], operands[1], V4SFmode); DONE; }
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

/* /home/yu/gcc/gcc_powerpc/crosstool-0.42/build/powerpc-7450-linux-gnu/gcc-3.3.1-glibc-2.3.2/gcc-3.3.1/gcc/config/rs6000/altivec.md:495 */
rtx
gen_mulv4sf3 (operand0, operand1, operand2)
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
  rtx neg0;

  /* Generate [-0.0, -0.0, -0.0, -0.0].  */
  neg0 = gen_reg_rtx (V4SFmode);
  emit_insn (gen_altivec_vspltisw_v4sf (neg0, GEN_INT (-1)));
  emit_insn (gen_altivec_vslw_v4sf (neg0, neg0, neg0));

  /* Use the multiply-add.  */
  emit_insn (gen_altivec_vmaddfp (operands[0], operands[1], operands[2],
				  neg0));
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

/* /home/yu/gcc/gcc_powerpc/crosstool-0.42/build/powerpc-7450-linux-gnu/gcc-3.3.1-glibc-2.3.2/gcc-3.3.1/gcc/config/rs6000/altivec.md:1585 */
rtx
gen_cr6_test_for_zero (operand0)
     rtx operand0;
{
  return gen_rtx_SET (VOIDmode,
	operand0,
	gen_rtx_EQ (SImode,
	gen_rtx_REG (CCmode,
	74),
	const0_rtx));
}

/* /home/yu/gcc/gcc_powerpc/crosstool-0.42/build/powerpc-7450-linux-gnu/gcc-3.3.1-glibc-2.3.2/gcc-3.3.1/gcc/config/rs6000/altivec.md:1592 */
rtx
gen_cr6_test_for_zero_reverse (operand0)
     rtx operand0;
{
  rtx _val = 0;
  start_sequence ();
  emit_insn (gen_rtx_SET (VOIDmode,
	operand0,
	gen_rtx_EQ (SImode,
	gen_rtx_REG (CCmode,
	74),
	const0_rtx)));
  emit_insn (gen_rtx_SET (VOIDmode,
	operand0,
	gen_rtx_MINUS (SImode,
	const1_rtx,
	operand0)));
  _val = get_insns ();
  end_sequence ();
  return _val;
}

/* /home/yu/gcc/gcc_powerpc/crosstool-0.42/build/powerpc-7450-linux-gnu/gcc-3.3.1-glibc-2.3.2/gcc-3.3.1/gcc/config/rs6000/altivec.md:1600 */
rtx
gen_cr6_test_for_lt (operand0)
     rtx operand0;
{
  return gen_rtx_SET (VOIDmode,
	operand0,
	gen_rtx_LT (SImode,
	gen_rtx_REG (CCmode,
	74),
	const0_rtx));
}

/* /home/yu/gcc/gcc_powerpc/crosstool-0.42/build/powerpc-7450-linux-gnu/gcc-3.3.1-glibc-2.3.2/gcc-3.3.1/gcc/config/rs6000/altivec.md:1607 */
rtx
gen_cr6_test_for_lt_reverse (operand0)
     rtx operand0;
{
  rtx _val = 0;
  start_sequence ();
  emit_insn (gen_rtx_SET (VOIDmode,
	operand0,
	gen_rtx_LT (SImode,
	gen_rtx_REG (CCmode,
	74),
	const0_rtx)));
  emit_insn (gen_rtx_SET (VOIDmode,
	operand0,
	gen_rtx_MINUS (SImode,
	const1_rtx,
	operand0)));
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
    case 756:
      XVECEXP (pattern, 0, 1) = gen_rtx_CLOBBER (VOIDmode,
	gen_rtx_SCRATCH (V4SFmode));
      XVECEXP (pattern, 0, 2) = gen_rtx_CLOBBER (VOIDmode,
	gen_rtx_SCRATCH (V4SFmode));
      break;

    case 759:
    case 755:
      XVECEXP (pattern, 0, 1) = gen_rtx_CLOBBER (VOIDmode,
	gen_rtx_SCRATCH (V4SImode));
      XVECEXP (pattern, 0, 2) = gen_rtx_CLOBBER (VOIDmode,
	gen_rtx_SCRATCH (V4SImode));
      break;

    case 758:
    case 754:
      XVECEXP (pattern, 0, 1) = gen_rtx_CLOBBER (VOIDmode,
	gen_rtx_SCRATCH (V8HImode));
      XVECEXP (pattern, 0, 2) = gen_rtx_CLOBBER (VOIDmode,
	gen_rtx_SCRATCH (V8HImode));
      break;

    case 757:
    case 753:
      XVECEXP (pattern, 0, 1) = gen_rtx_CLOBBER (VOIDmode,
	gen_rtx_SCRATCH (V16QImode));
      XVECEXP (pattern, 0, 2) = gen_rtx_CLOBBER (VOIDmode,
	gen_rtx_SCRATCH (V16QImode));
      break;

    case 732:
      XVECEXP (pattern, 0, 1) = gen_rtx_CLOBBER (VOIDmode,
	gen_rtx_SCRATCH (V16QImode));
      break;

    case 731:
      XVECEXP (pattern, 0, 1) = gen_rtx_CLOBBER (VOIDmode,
	gen_rtx_SCRATCH (V8HImode));
      break;

    case 730:
      XVECEXP (pattern, 0, 1) = gen_rtx_CLOBBER (VOIDmode,
	gen_rtx_SCRATCH (V4SFmode));
      break;

    case 729:
      XVECEXP (pattern, 0, 1) = gen_rtx_CLOBBER (VOIDmode,
	gen_rtx_SCRATCH (V4SImode));
      break;

    case 537:
    case 536:
    case 533:
    case 532:
    case 529:
    case 528:
      XVECEXP (pattern, 0, 2) = gen_rtx_CLOBBER (VOIDmode,
	gen_rtx_SCRATCH (CCmode));
      XVECEXP (pattern, 0, 3) = gen_rtx_CLOBBER (VOIDmode,
	gen_rtx_SCRATCH (DImode));
      break;

    case 535:
    case 534:
    case 531:
    case 530:
    case 527:
    case 526:
      XVECEXP (pattern, 0, 2) = gen_rtx_CLOBBER (VOIDmode,
	gen_rtx_SCRATCH (CCmode));
      XVECEXP (pattern, 0, 3) = gen_rtx_CLOBBER (VOIDmode,
	gen_rtx_SCRATCH (SImode));
      break;

    case 433:
    case 421:
      XVECEXP (pattern, 0, 2) = gen_rtx_CLOBBER (VOIDmode,
	gen_rtx_SCRATCH (DImode));
      break;

    case 387:
    case 385:
    case 383:
    case 381:
      XVECEXP (pattern, 0, 4) = gen_rtx_CLOBBER (VOIDmode,
	gen_rtx_SCRATCH (SImode));
      break;

    case 350:
    case 349:
    case 348:
      XVECEXP (pattern, 0, 3) = gen_rtx_CLOBBER (VOIDmode,
	gen_rtx_SCRATCH (SImode));
      XVECEXP (pattern, 0, 4) = gen_rtx_CLOBBER (VOIDmode,
	gen_rtx_SCRATCH (SImode));
      break;

    case 347:
    case 346:
      XVECEXP (pattern, 0, 3) = gen_rtx_CLOBBER (VOIDmode,
	gen_rtx_SCRATCH (DImode));
      XVECEXP (pattern, 0, 4) = gen_rtx_CLOBBER (VOIDmode,
	gen_rtx_SCRATCH (SImode));
      break;

    case 345:
    case 344:
    case 343:
      XVECEXP (pattern, 0, 4) = gen_rtx_CLOBBER (VOIDmode,
	gen_rtx_REG (SImode,
	6));
      XVECEXP (pattern, 0, 5) = gen_rtx_CLOBBER (VOIDmode,
	gen_rtx_REG (SImode,
	7));
      XVECEXP (pattern, 0, 6) = gen_rtx_CLOBBER (VOIDmode,
	gen_rtx_REG (SImode,
	8));
      XVECEXP (pattern, 0, 7) = gen_rtx_CLOBBER (VOIDmode,
	gen_rtx_SCRATCH (SImode));
      break;

    case 342:
    case 341:
    case 340:
      XVECEXP (pattern, 0, 4) = gen_rtx_CLOBBER (VOIDmode,
	gen_rtx_REG (SImode,
	6));
      XVECEXP (pattern, 0, 5) = gen_rtx_CLOBBER (VOIDmode,
	gen_rtx_REG (SImode,
	7));
      XVECEXP (pattern, 0, 6) = gen_rtx_CLOBBER (VOIDmode,
	gen_rtx_REG (SImode,
	8));
      XVECEXP (pattern, 0, 7) = gen_rtx_CLOBBER (VOIDmode,
	gen_rtx_REG (SImode,
	9));
      XVECEXP (pattern, 0, 8) = gen_rtx_CLOBBER (VOIDmode,
	gen_rtx_REG (SImode,
	10));
      XVECEXP (pattern, 0, 9) = gen_rtx_CLOBBER (VOIDmode,
	gen_rtx_SCRATCH (SImode));
      break;

    case 339:
    case 338:
    case 337:
      XVECEXP (pattern, 0, 4) = gen_rtx_CLOBBER (VOIDmode,
	gen_rtx_REG (SImode,
	6));
      XVECEXP (pattern, 0, 5) = gen_rtx_CLOBBER (VOIDmode,
	gen_rtx_REG (SImode,
	7));
      XVECEXP (pattern, 0, 6) = gen_rtx_CLOBBER (VOIDmode,
	gen_rtx_REG (SImode,
	8));
      XVECEXP (pattern, 0, 7) = gen_rtx_CLOBBER (VOIDmode,
	gen_rtx_REG (SImode,
	9));
      XVECEXP (pattern, 0, 8) = gen_rtx_CLOBBER (VOIDmode,
	gen_rtx_REG (SImode,
	10));
      XVECEXP (pattern, 0, 9) = gen_rtx_CLOBBER (VOIDmode,
	gen_rtx_REG (SImode,
	11));
      XVECEXP (pattern, 0, 10) = gen_rtx_CLOBBER (VOIDmode,
	gen_rtx_REG (SImode,
	12));
      XVECEXP (pattern, 0, 11) = gen_rtx_CLOBBER (VOIDmode,
	gen_rtx_SCRATCH (SImode));
      break;

    case 286:
      XVECEXP (pattern, 0, 1) = gen_rtx_CLOBBER (VOIDmode,
	gen_rtx_SCRATCH (DImode));
      XVECEXP (pattern, 0, 2) = gen_rtx_CLOBBER (VOIDmode,
	gen_rtx_SCRATCH (CCmode));
      break;

    case 319:
    case 318:
    case 317:
    case 316:
    case 315:
    case 216:
      XVECEXP (pattern, 0, 1) = gen_rtx_CLOBBER (VOIDmode,
	gen_rtx_SCRATCH (DFmode));
      break;

    case 287:
    case 84:
    case 83:
      XVECEXP (pattern, 0, 2) = gen_rtx_CLOBBER (VOIDmode,
	gen_rtx_SCRATCH (CCmode));
      break;

    case 82:
    case 81:
      XVECEXP (pattern, 0, 1) = gen_rtx_CLOBBER (VOIDmode,
	gen_rtx_SCRATCH (SImode));
      XVECEXP (pattern, 0, 2) = gen_rtx_CLOBBER (VOIDmode,
	gen_rtx_SCRATCH (CCmode));
      break;

    case 285:
    case 80:
      XVECEXP (pattern, 0, 1) = gen_rtx_CLOBBER (VOIDmode,
	gen_rtx_SCRATCH (CCmode));
      break;

    case 79:
      XVECEXP (pattern, 0, 1) = gen_rtx_CLOBBER (VOIDmode,
	gen_rtx_SCRATCH (SImode));
      XVECEXP (pattern, 0, 2) = gen_rtx_CLOBBER (VOIDmode,
	gen_rtx_REG (SImode,
	0));
      XVECEXP (pattern, 0, 3) = gen_rtx_CLOBBER (VOIDmode,
	gen_rtx_SCRATCH (CCmode));
      XVECEXP (pattern, 0, 4) = gen_rtx_CLOBBER (VOIDmode,
	gen_rtx_REG (CCmode,
	69));
      break;

    case 77:
      XVECEXP (pattern, 0, 2) = gen_rtx_CLOBBER (VOIDmode,
	gen_rtx_SCRATCH (SImode));
      XVECEXP (pattern, 0, 3) = gen_rtx_CLOBBER (VOIDmode,
	gen_rtx_REG (SImode,
	0));
      XVECEXP (pattern, 0, 4) = gen_rtx_CLOBBER (VOIDmode,
	gen_rtx_SCRATCH (CCmode));
      XVECEXP (pattern, 0, 5) = gen_rtx_CLOBBER (VOIDmode,
	gen_rtx_REG (CCmode,
	69));
      break;

    case 76:
      XVECEXP (pattern, 0, 2) = gen_rtx_CLOBBER (VOIDmode,
	gen_rtx_SCRATCH (SImode));
      XVECEXP (pattern, 0, 3) = gen_rtx_CLOBBER (VOIDmode,
	gen_rtx_REG (SImode,
	0));
      break;

    case 75:
      XVECEXP (pattern, 0, 1) = gen_rtx_CLOBBER (VOIDmode,
	gen_rtx_SCRATCH (SImode));
      XVECEXP (pattern, 0, 2) = gen_rtx_CLOBBER (VOIDmode,
	gen_rtx_REG (SImode,
	0));
      break;

    case 465:
    case 435:
    case 432:
    case 420:
    case 392:
    case 391:
    case 390:
    case 389:
    case 388:
    case 386:
    case 384:
    case 382:
    case 380:
    case 379:
    case 378:
    case 377:
    case 160:
    case 142:
    case 133:
    case 63:
      XVECEXP (pattern, 0, 2) = gen_rtx_CLOBBER (VOIDmode,
	gen_rtx_SCRATCH (SImode));
      break;

    case 430:
    case 158:
    case 140:
    case 131:
    case 61:
      XVECEXP (pattern, 0, 1) = gen_rtx_CLOBBER (VOIDmode,
	gen_rtx_SCRATCH (SImode));
      XVECEXP (pattern, 0, 2) = gen_rtx_CLOBBER (VOIDmode,
	gen_rtx_SCRATCH (SImode));
      break;

    case 28:
    case 25:
      XVECEXP (pattern, 0, 1) = gen_rtx_CLOBBER (VOIDmode,
	gen_rtx_SCRATCH (HImode));
      break;

    case 553:
    case 505:
    case 494:
    case 489:
    case 479:
    case 475:
    case 467:
    case 464:
    case 461:
    case 455:
    case 450:
    case 446:
    case 437:
    case 434:
    case 428:
    case 426:
    case 423:
    case 418:
    case 326:
    case 230:
    case 229:
    case 228:
    case 226:
    case 224:
    case 222:
    case 220:
    case 159:
    case 156:
    case 151:
    case 148:
    case 145:
    case 141:
    case 138:
    case 136:
    case 132:
    case 129:
    case 127:
    case 124:
    case 121:
    case 118:
    case 112:
    case 95:
    case 92:
    case 89:
    case 86:
    case 78:
    case 74:
    case 71:
    case 68:
    case 66:
    case 62:
    case 59:
    case 56:
    case 54:
    case 52:
    case 49:
    case 45:
    case 44:
    case 40:
    case 37:
    case 34:
    case 31:
    case 22:
    case 19:
      XVECEXP (pattern, 0, 1) = gen_rtx_CLOBBER (VOIDmode,
	gen_rtx_SCRATCH (SImode));
      break;

    case 554:
    case 506:
    case 490:
    case 431:
    case 429:
    case 427:
    case 419:
    case 295:
    case 292:
    case 289:
    case 283:
    case 280:
    case 277:
    case 274:
    case 271:
    case 268:
    case 265:
    case 262:
    case 259:
    case 256:
    case 251:
    case 244:
    case 242:
    case 241:
    case 239:
    case 236:
    case 233:
    case 115:
    case 16:
    case 13:
    case 10:
    case 7:
    case 4:
    case 1:
      XVECEXP (pattern, 0, 1) = gen_rtx_CLOBBER (VOIDmode,
	gen_rtx_SCRATCH (DImode));
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
    case 756:
    case 759:
    case 755:
    case 758:
    case 754:
    case 757:
    case 753:
    case 732:
    case 731:
    case 730:
    case 729:
    case 537:
    case 536:
    case 533:
    case 532:
    case 529:
    case 528:
    case 535:
    case 534:
    case 531:
    case 530:
    case 527:
    case 526:
    case 433:
    case 421:
    case 387:
    case 385:
    case 383:
    case 381:
    case 350:
    case 349:
    case 348:
    case 347:
    case 346:
    case 286:
    case 319:
    case 318:
    case 317:
    case 316:
    case 315:
    case 216:
    case 287:
    case 84:
    case 83:
    case 82:
    case 81:
    case 285:
    case 80:
    case 465:
    case 435:
    case 432:
    case 420:
    case 392:
    case 391:
    case 390:
    case 389:
    case 388:
    case 386:
    case 384:
    case 382:
    case 380:
    case 379:
    case 378:
    case 377:
    case 160:
    case 142:
    case 133:
    case 63:
    case 430:
    case 158:
    case 140:
    case 131:
    case 61:
    case 28:
    case 25:
    case 553:
    case 505:
    case 494:
    case 489:
    case 479:
    case 475:
    case 467:
    case 464:
    case 461:
    case 455:
    case 450:
    case 446:
    case 437:
    case 434:
    case 428:
    case 426:
    case 423:
    case 418:
    case 326:
    case 230:
    case 229:
    case 228:
    case 226:
    case 224:
    case 222:
    case 220:
    case 159:
    case 156:
    case 151:
    case 148:
    case 145:
    case 141:
    case 138:
    case 136:
    case 132:
    case 129:
    case 127:
    case 124:
    case 121:
    case 118:
    case 112:
    case 95:
    case 92:
    case 89:
    case 86:
    case 78:
    case 74:
    case 71:
    case 68:
    case 66:
    case 62:
    case 59:
    case 56:
    case 54:
    case 52:
    case 49:
    case 45:
    case 44:
    case 40:
    case 37:
    case 34:
    case 31:
    case 22:
    case 19:
    case 554:
    case 506:
    case 490:
    case 431:
    case 429:
    case 427:
    case 419:
    case 295:
    case 292:
    case 289:
    case 283:
    case 280:
    case 277:
    case 274:
    case 271:
    case 268:
    case 265:
    case 262:
    case 259:
    case 256:
    case 251:
    case 244:
    case 242:
    case 241:
    case 239:
    case 236:
    case 233:
    case 115:
    case 16:
    case 13:
    case 10:
    case 7:
    case 4:
    case 1:
      return 0;

    case 345:
    case 344:
    case 343:
    case 342:
    case 341:
    case 340:
    case 339:
    case 338:
    case 337:
    case 79:
    case 77:
    case 76:
    case 75:
      return 1;

    default:
      abort ();
    }
}
