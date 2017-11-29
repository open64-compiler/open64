/*
 * Copyright (C) 2009 Advanced Micro Devices, Inc.  All Rights Reserved.
 */

/*
 * Copyright 2005, 2006 PathScale, Inc.  All Rights Reserved.
 */
/*
 * Copyright (c) 2006. QLogic Corporation.  All rights reserved.
 *
 * Unpublished -- rights reserved under the copyright laws of the United
 * States. USE OF A COPYRIGHT NOTICE DOES NOT IMPLY PUBLICATION OR
 * DISCLOSURE. THIS SOFTWARE CONTAINS CONFIDENTIAL INFORMATION AND TRADE
 * SECRETS OF QLOGIC CORPORATION. USE, DISCLOSURE, OR REPRODUCTION IS
 * PROHIBITED WITHOUT THE PRIOR EXPRESS WRITTEN PERMISSION OF QLOGIC,
 * CORPORATION.
 *
 * U.S. Government Restricted Rights:
 * The Software is a "commercial item," as that term is defined at 48
 * C.F.R. 2.101 (OCT 1995), consisting of "commercial computer software"
 * and "commercial computer software documentation," as such terms are used
 * in 48 C.F.R. 12.212 (SEPT 1995).  Consistent with 48 C.F.R. 12.212 and
 * 48 C.F.R. 227-7202-1 through 227-7202-4 (JUNE 1995), all U.S. Government
 * End Users acquire the Software with only those rights set forth in the
 * accompanying license agreement. QLogic Corporation, 26650 Aliso Viejo 
 * Parkway, Aliso Viejo, CA 92656.
 */
/*
   Copyright (C) 1997,99,2000, 2001, 2003, 2004 Free Software Foundation, Inc.
   This file is part of the GNU C Library.
   Contributed by Ulrich Drepper <drepper@cygnus.com>, 1997.

   The GNU C Library is free software; you can redistribute it and/or
   modify it under the terms of the GNU Lesser General Public
   License as published by the Free Software Foundation; either
   version 2.1 of the License, or (at your option) any later version.

   The GNU C Library is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
   Lesser General Public License for more details.

   You should have received a copy of the GNU Lesser General Public
   License along with the GNU C Library; if not, write to the Free
   Software Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA
   02111-1307 USA.  */

#if defined(TARG_IA32) || defined(TARG_X8664)
#  define X86 1
#endif /* defined(TARG_IA32) || defined(TARG_X8664) */

/*
 * Support for TR15580/F2003 intrinsic modules IEEE_ARITHMETIC and
 * IEEE_EXCEPTIONS
 *
 * 1. Implement ieee_class() based on ISO C99 fpclassify()
 *
 *   Since Fortran distinguishes between positive and negative input values of
 *   each category except NaN, we negate the C99 output value if the input is
 *   negative.
 *
 *   Since Fortran distinguishes between quiet and signaling NaN, we use a
 *   a new output value (one greater than the highest C99 value) for that.
 *
 *   Since Fortran requires an FP_OTHER, we use a new output value (two greater
 *   than the highest C99 value) for that.
 *
 * 2. Implement ieee_{g,s}et_rounding_mode() based on ISO C99 fe{g,s}etround()
 *
 * 3. Implement ieee_{g,s}et_flag() based on ISO C99
 *    fe{test,raise,clear}except()
 *
 * 4. Implement ieee_{g,s}et_halting_mode() based on ISO C99
 *    fe{get,enable,disable}except()
 *
 * 5. Implement a few other irregularities
 *
 * 6. Implement ieee_{g,s}et_underflow_mode() using asm
 */
#ifndef _GNU_SOURCE
#define _GNU_SOURCE
#endif
#include <math.h>
#include <fenv.h>
#include <stdlib.h>
#include <errno.h>

#include <liberrno.h>

#define MAX(a,b) (((a) > (b)) ? (a) : (b))

#define FP_NANS \
 (1 + MAX(FP_NAN, MAX(FP_INFINITE, MAX(FP_ZERO, MAX(FP_SUBNORMAL, FP_NORMAL)))))
#define FP_OTHER (1+FP_NANS)

typedef int int4;
typedef long int int8;

typedef struct {
  int4 value;
  } ieee_class;

/*
 * 1. In most if not all Linux distros the -m32 fegetenv() and fesetenv()
 * functions fail to read and set bits in the MXCSR register, which affect the
 * SSE2 instructions. The problem seems to be that the -m64 version of the
 * fenv structure has a specific field for the MXCSR register, but the -m32
 * version doesn't, even though 32-bit Intel machines have SSE2 instructions.
 *
 * On the other hand, in both the -m32 and -m64 versions, the function
 * fetestexcept() reads the X87 and MXCSR registers and ORs the appropriate
 * bits. Other functions like feclearexcept() are also aware of MXCSR.
 *
 * A possible fix would be to OR the bits inside fegetenv() too. But for
 * Fortran purposes, we also need to save and restore the flush-to-zero bit
 * in the MXCSR register, to control TR15580 "gradual rounding mode". So our
 * fix is to allocate in -m32 mode enough space for the -m64 fenv structure,
 * and save and restore the MXCSR register ourself in the extra space.
 *
 * 2. FC3 -m64 -static fegetenv executes the fnstenv instruction, which
 * masks FP exceptions as a side effect, but then fails to restore the state
 * of the exceptions. Other distros may have the same problem.
 *
 * 3. Most linux distros say:
 *   warning: feupdateenv is not implemented and will always fail
 * when we link feupdateenv in -m64 mode. And the -m32 version calls fesetenv()
 * which fails to deal with the MXCSR register. So we implement our own.
 *
 * 4. The FC3 -m64 feraiseexcept() function is strange: rather than simply
 * storing the status flags, it tries to execute operations that will set
 * the flags as a consequence, apparently (from comments in the source) out
 * of concern for the priority of exceptions in the Intel architecture. But
 * a consequence is that it doesn't always set both the X87 status and the
 * MXCSR register. So far this hasn't been a problem: if it does cause a
 * bug, we'll need to use fegetexceptflag() followed by fesetexceptflag(),
 * or just use asm() instead.
 * 
 * 5. RHEL WS 3 has a bug in -m32 in feclearexcept() that destroys
 * the SSE2 mask bits (the "halting modes" in Fortran terminology.) A fix
 * appears in other distros: we provide it here.
 *
 * 6. Just to make things more confusing, the Fedora Core 1 version of
 * fenv_t for -m32 provides __mxcsr, but Fedora Core 2 and later do not.
 * We use our_fenv_t for -m32 so that the layout is consistent. If this
 * proves a problem (because we encounter some distro with a variant
 * layout) we'll have to throw in the towel and code our own replacements
 * for all of the fenv.h functions.
 */

# if defined(TARG_IA32) || defined(TARG_X8664)
#  define FESETENV __f90_fesetenv
#  define FEGETENV __f90_fegetenv
#  define FENV_T our_fenv_t

#  if defined(TARG_IA32) /* Definitions for IA32 -m32 */
#  define IS_SSE2_PRESENT() (!__SSE2_off)

/* If command line disables SSE2, front end will emit a strong definition
 * for this along with the main program */
#pragma weak __SSE2_off
int __SSE2_off = 0;

#  elif defined(TARG_X8664) /* Definitions for X8664 -m64 */
#   define IS_SSE2_PRESENT() (1)
#  endif

typedef struct {
  unsigned short int __control_word;
  unsigned short int __unused1;
  unsigned short int __status_word;
  unsigned short int __unused2;
  unsigned short int __tags;
  unsigned short int __unused3;
  unsigned int __eip;
  unsigned short int __cs_selector;
  unsigned int __opcode:11;
  unsigned int __unused4:5;
  unsigned int __data_offset;
  unsigned short int __data_selector;
  unsigned short int __unused5;
  unsigned int __mxcsr;
  } our_fenv_t;

static int __f90_fegetenv(FENV_T *envp) {
  fegetenv((fenv_t *) envp);
  if (IS_SSE2_PRESENT()) {
    unsigned int temp;
    __asm__("stmxcsr %0" : "=m" (*&temp));
    envp->__mxcsr = temp;
    }
  }

static int __f90_fesetenv(const FENV_T *envp) {
  fesetenv((fenv_t *) envp);
  if (IS_SSE2_PRESENT()) {
    unsigned int temp = envp->__mxcsr;
    __asm__("ldmxcsr %0" : : "m" (*&temp));
    }
  }

int
feclearexcept(int excepts)
{
  FENV_T temp;

  /* Mask out unsupported bits/exceptions.  */
  excepts &= FE_ALL_EXCEPT;

  /* Bah, we have to clear selected exceptions.  Since there is no
     `fldsw' instruction we have to do it the hard way.  */
  __asm__("fnstenv %0" : "=m" (*&temp));

  /* Clear the relevant bits.  */
  temp.__status_word &= excepts ^ FE_ALL_EXCEPT;

  /* Put the new data in effect.  */
  __asm__("fldenv %0" : : "m" (*&temp));

  /* If the CPU supports SSE, we clear the MXCSR as well.  */
  if (IS_SSE2_PRESENT())
    {
      unsigned int xnew_exc;

      /* Get the current MXCSR.  */
      __asm__("stmxcsr %0" : "=m" (*&xnew_exc));

      /* Clear the relevant bits.  */
      xnew_exc &= ~excepts;

      /* Put the new data in effect.  */
      __asm__("ldmxcsr %0" : : "m" (*&xnew_exc));
    }

  /* Success.  */
  return 0;
}

#else /* defined(TARG_whatever) */
   /* Definitions for non-X86 architectures */
#  define FENV_T fenv_t
#  define FESETENV fesetenv
#  define FEGETENV fegetenv
#endif /* defined(TARG_whatever) */

#if defined(BUILD_OS_DARWIN)
/* Environment doesn't provide these functions */

int
feenableexcept(int excepts) {
  excepts &= FE_ALL_EXCEPT;

  unsigned short int prev_excepts;
  __asm__("fstcw %0" : "=m" (*&prev_excepts));
  int save = prev_excepts;
  prev_excepts &= ~excepts;
  __asm__("fldcw %0" : : "m" (*&prev_excepts));

  if (IS_SSE2_PRESENT()) {
    unsigned int prev_mxcsr_excepts;
    __asm__("stmxcsr %0" : "=m" (*&prev_mxcsr_excepts));
    prev_mxcsr_excepts &= ~(excepts << 7);
    __asm__("ldmxcsr %0" : : "m" (*&prev_mxcsr_excepts));
  }

  return (~save) & FE_ALL_EXCEPT;
}

int
fedisableexcept(int excepts) {
  excepts &= FE_ALL_EXCEPT;

  unsigned short int prev_excepts;
  __asm__("fstcw %0" : "=m" (*&prev_excepts));
  int save = prev_excepts;
  prev_excepts |= excepts;
  __asm__("fldcw %0" : : "m" (*&prev_excepts));

  if (IS_SSE2_PRESENT()) {
    unsigned int prev_mxcsr_excepts;
    __asm__("stmxcsr %0" : "=m" (*&prev_mxcsr_excepts));
    prev_mxcsr_excepts |= (excepts << 7);
    __asm__("ldmxcsr %0" : : "m" (*&prev_mxcsr_excepts));
  }

  return (~save) & FE_ALL_EXCEPT;
}
#endif /* defined(BUILD_OS_DARWIN) */

ieee_class _Ieee_class_4_(float *f) {
  ieee_class result;
  float fval;
  FENV_T state;
  /* NaN tends to trigger FP exceptions here with both gcc and pathf90. */
  FEGETENV(&state);
  fedisableexcept(FE_ALL_EXCEPT);
  /* Dereferencing "f" as a "float" type would quiet a signaling NaN */
  * (unsigned *) &fval = * (unsigned *) f;
  result.value = fpclassify(fval);
  if (FP_NAN != result.value) {
    if (signbit(fval)) {
      result.value = -result.value;
      }
    }
  else {
    unsigned pattern = * (unsigned *) &fval;
    if (0 == (0x400000 & pattern)) {
      result.value = FP_NANS;
      }
    }
  FESETENV(&state);
  return result;
  }

ieee_class _Ieee_class_8_(double *d) {
  ieee_class result;
  double dval;
  FENV_T state;
  /* NaN tends to trigger FP exceptions here with both gcc and pathf90. */
  FEGETENV(&state);
  fedisableexcept(FE_ALL_EXCEPT);
  /* Dereferencing "d" as a "double" type would quiet a signaling NaN */
  * (unsigned long long *) &dval = * (unsigned long long *) d;
  result.value = fpclassify(dval);
  if (FP_NAN != result.value) {
    if (signbit(dval)) {
      result.value = -result.value;
      }
    }
  else {
    unsigned long long pattern = * (unsigned long long *) &dval;
    if (0 == (0x8000000000000ULL & pattern)) {
      result.value = FP_NANS;
      }
    }
  FESETENV(&state);
  return result;
  }

typedef struct {
  int4 value;
  } ieee_round_type;

void _Ieee_get_rounding_mode_(ieee_round_type *r) {
  r->value = fegetround();
  }

void _Ieee_set_rounding_mode_(ieee_round_type *r) {
  if (fesetround(r->value)) {
    _lmessage(errno, NULL, NULL);
    }
  }

typedef struct {
  int4 value;
  } ieee_flag_type;

void _Ieee_get_flag_(ieee_flag_type *flag, int4 *flag_value) {
  int4 temp = fetestexcept(flag->value);
  *flag_value = !!temp;
  }


void _Ieee_set_flag_(ieee_flag_type *flag, int4 *flag_value) {
  ((*flag_value) ? feraiseexcept : feclearexcept)(flag->value);
  }

void _Ieee_get_halting_mode_(ieee_flag_type *flag, int4 *halting) {
  int4 temp = fegetexcept();
  *halting = !!(temp & flag->value);
  }

void _Ieee_set_halting_mode_(ieee_flag_type *flag, int4 *halting) {
  ((*halting) ? feenableexcept : fedisableexcept)(flag->value);
  }

void _Ieee_get_status_(FENV_T *status_value) {
  FEGETENV(status_value);
#if defined(X86)
  /* Because fegetenv has bug which leaves all exceptions masked */
  __asm__("fldcw %0" : : "m" (*&(status_value->__control_word)));
#endif /* defined(X86) */
  }

void _Ieee_set_status_(FENV_T *status_value) {
  FESETENV(status_value);
  }

int _Ieee_is_normal_4_(float *value) {
  int class = fpclassify(*value);
  return FP_NORMAL == class || FP_ZERO == class;
  }

int _Ieee_is_normal_8_(double *value) {
  int class = fpclassify(*value);
  return FP_NORMAL == class || FP_ZERO == class;
  }

/*
 * Alas, F2003 says ieee_is_negative(NaN) must give "false" (see
 * interpretation J3/05-111r1, question by Tydeman, answer by Reid) whereas
 * IEEE 754 and C99 both say "signbit(NaN)" gives sign bit of the NaN even
 * though it's not otherwise interpreted as positive or negative.
 */
int _Ieee_is_negative_4_(float *value) {
  float vtemp = *value;
  return signbit(vtemp) && !isnan(vtemp);
  }

int _Ieee_is_negative_8_(double *value) {
  double vtemp = *value;
  return signbit(vtemp) && !isnan(vtemp);
  }

/* At SGI, the Fortran front end converted real*4 to real*8 if need be and
 * called libc function "unordered", which Linux doesn't have. Using
 * __builtin_isunordered with -std=c99 generates instructions inline, so this
 * is at least as good.
 */
int unordered(double x, double y) {
  return __builtin_isunordered(x, y);
  }

#if defined(X86)

/*
 * SSE2 control register bit 0x8000 is flush-to-zero
 * There is no X87 equivalent
 * Enabling halting mode ("unmasked exception" in IA-32 terminology) takes
 * precedence over this
 */
void
_Ieee_set_underflow_mode_(int *gradual) {
  int temp;
  if (*gradual) {
    __asm__(
	  "\tstmxcsr %0\n"
	  "\tandl $-32769, %0\n"
	  "\tldmxcsr %0\n"
      : "=m" (temp)
    );
  } else {
    __asm__(
	  "\tstmxcsr %0\n"
	  "\torl $32768, %0\n"
	  "\tldmxcsr %0\n"
      : "=m" (temp)
    );
  }
}

void
_Ieee_get_underflow_mode_(int *gradual) {
  __asm__(
	"\tstmxcsr %0\n"
	"\tandl $32768, %0\n"
	"\tshrl $15, %0\n"
	"\taddl $-1, %0\n"
	"\tnegl %0\n"
     : "=m" (*gradual)
  );
}
#elif defined(TARG_MIPS)
#define GRADUAL_UNDERFLOW_BIT 0x1000000
void
_Ieee_set_underflow_mode_(int *gradual) {
  unsigned int temp;
  __asm__("cfc1 %0,$31" : "=r" (temp));
  if (*gradual) {
    temp = temp | GRADUAL_UNDERFLOW_BIT;
  } else {
    temp = temp & ~ GRADUAL_UNDERFLOW_BIT;
  }
  __asm__("ctc1 %0,$31" : : "r" (temp));
}

void
_Ieee_get_underflow_mode_(int *gradual) {
  unsigned int temp;
  __asm__("cfc1 %0,$31" : "=r" (temp));
  *gradual = !!(temp & GRADUAL_UNDERFLOW_BIT);
}
#else
   /* If target doesn't support this, ieee_support_underflow_control()
    * should be made to return false in the front end. */
#  error "Need _Ieee_set_{under,over}flow_mode_"
#endif /* defined(whatever) */

/* Designed to be called in the prolog of a scope which uses any of the IEEE
 * intrinsic modules: saves all state and sets status flags to quiet. On exit
 * from the scope, call _Ieee_restore to restore state, ORing the saved
 * flags with the now-current ones.
 *
 * F2003 always requires saving, clearing, and later restoring the flags,
 * but in theory it requires
 * saving and restoring rounding, halting, and gradual-underflow state only if
 * the scope contains a call to ieee_set_*. However, on the X86 there's no
 * profit in treating that as a special case, because there's no instruction
 * to set the flags without setting the rest of the state, so one is forced
 * to start by reading the entire state, which as a side effect masks
 * exceptions, which can be fixed by restoring the entire state anyway,
 * and don't forget the Linux fegetenv() bug described earlier. What a
 * mess!
 *
 * Anyway, the special case is left as an exercise for other architectures
 * where it might have benefits.
 */
void _Ieee_save(FENV_T *state) {
  FEGETENV(state);
#if defined(TARG_IA32)
  unsigned int save_status = state->__status_word;
  unsigned int save_mxcsr = state->__mxcsr;
  state->__status_word &= ~FE_ALL_EXCEPT;
  if (IS_SSE2_PRESENT()) {
    state->__mxcsr &= ~FE_ALL_EXCEPT;
  }
  FESETENV(state);
  state->__status_word = save_status;
  state->__mxcsr = save_mxcsr;
#elif defined(TARG_MIPS)
  FENV_T newstate = *state;
  newstate.__fp_control_register &= ~FE_ALL_EXCEPT;
  FESETENV(&newstate);
#else /* defined(TARG_whatever) */
//# error "need code to clear flags"
#endif /* defined(TARG_whatever) */
  }

#if defined(X86)
int _Ieee_restore(const FENV_T *state) {
  FENV_T temp;
  __asm__("fnstenv %0" : "=m" (*&temp));
  temp.__status_word =
    state->__status_word | (temp.__status_word & FE_ALL_EXCEPT);
  temp.__control_word = state->__control_word;
  __asm__("fldenv %0" : : "m" (*&temp));
  if (IS_SSE2_PRESENT()) {
    unsigned int mxcsr_temp;
    __asm__("stmxcsr %0" : "=m" (*&mxcsr_temp));
    mxcsr_temp = state->__mxcsr | (mxcsr_temp & FE_ALL_EXCEPT);
    __asm__("ldmxcsr %0" : : "m" (*&mxcsr_temp));
    }
  return 0;
  }
#else /* defined(X86) */
int _Ieee_restore(const fenv_t *arg) {
  feupdateenv(arg);
  }
#endif /* defined(X86) */

/* Eventually should do away with these, and make ieee_arithmetic use the
 * F2003 C interface to call rintf() and rint() directly */
float
_Ieee_rint_4_(float *x) {
  return rintf(*x);
  }

double
_Ieee_rint_8_(double *x) {
  return rint(*x);
  }

static const unsigned int neg_inf =		0xff800000u;
static const unsigned long long d_neg_inf =	0xfff0000000000000ull;

/* With -m32, on X86, a function returning a float or double value generates
 * machine instructions which change signaling NaN into quiet NaN, so we
 * must pass the return value by reference instead. */
void
_Ieee_value_4_(float *x, float *unused, int *class) {
  static const unsigned int nan =		0x7fc00000u;
  static const unsigned int nans =		0x7f800001u;
  static const unsigned int zero =		0x00000000u;
  static const unsigned int neg_zero =	0x80000000u;
  static const unsigned int inf =		0x7f800000u;
  static const unsigned int denorm =		0x00000001u;
  static const unsigned int neg_denorm =	0x80000001u;

  switch (*class) {
    case FP_NAN:
      /* FP assignment would raise an exception */
      * (unsigned long *) x = * (unsigned long *) &nan;
      break;
    case FP_NANS:
      /* FP assignment would quiet the NaN */
      * (unsigned long *) x = * (unsigned long *) &nans;
      break;
    case FP_ZERO:
      *x = * (float *) &zero;
      break;
    case -FP_ZERO:
      *x = * (float *) &neg_zero;
      break;
    case FP_INFINITE:
      * (unsigned long *) x = * (unsigned long *) &inf;
      break;
    case -FP_INFINITE:
      * (unsigned long *) x = * (unsigned long *) &neg_inf;
      break;
    case FP_SUBNORMAL:
      * (unsigned long *) x = * (unsigned long *) &denorm;
      break;
    case -FP_SUBNORMAL:
      * (unsigned long *) x = * (unsigned long *) &neg_denorm;
      break;
    case FP_NORMAL:
      *x = (float) 1.0;
      break;
    case -FP_NORMAL:
      *x = (float) -1.0;
      break;
    }
  }

void
_Ieee_value_8_(double *x, double *unused, int *class) {
  static const unsigned long long nan =	0x7ff8000000000000ull;
  static const unsigned long long nans =	0x7ff0000000000001ull;
  static const unsigned long long zero =	0x0000000000000000ull;
  static const unsigned long long neg_zero =	0x8000000000000000ull;
  static const unsigned long long inf =	0x7ff0000000000000ull;
  static const unsigned long long denorm =	0x0000000000000001ull;
  static const unsigned long long neg_denorm=	0x8000000000000001ull;

  switch (*class) {
    case FP_NAN:
      /* FP assignment might raise an exception */
      * (unsigned long long *) x = * (unsigned long long *) &nan;
      break;
    case FP_NANS:
      /* FP assignment would quiet the NaN */
      * (unsigned long long *) x = * (unsigned long long *) &nans;
      break;
    case FP_ZERO:
      *x = * (double *) &zero;
      break;
    case -FP_ZERO:
      *x = * (double *) &neg_zero;
      break;
    case FP_INFINITE:
      * (unsigned long long *) x = * (unsigned long long *) &inf;
      break;
    case -FP_INFINITE:
      * (unsigned long long *) x = * (unsigned long long *) &d_neg_inf;
      break;
    case FP_SUBNORMAL:
      * (unsigned long long *) x = * (unsigned long long *) &denorm;
      break;
    case -FP_SUBNORMAL:
      * (unsigned long long *) x = * (unsigned long long *) &neg_denorm;
      break;
    case FP_NORMAL:
      *x = 1.0;
      break;
    case -FP_NORMAL:
      *x = -1.0;
      break;
    }
  }

float
_Ieee_logb_4_(float *x) {
  if (0.0f == *x) {
    feraiseexcept(FE_DIVBYZERO);
    return * (float *) &neg_inf;
    }
  return logbf(*x);
  }

double
_Ieee_logb_8_(double *x) {
  if (0.0 == *x) {
    feraiseexcept(FE_DIVBYZERO);
    return * (double *) &d_neg_inf;
    }
  return logb(*x);
  }

#if defined(_DEBUG) && defined(X86)
#include <stdio.h>
void
showfp_(char *message, long len) {
  fwrite(message, len, 1, stderr);
  FENV_T our_env;
  fenv_t *env = (fenv_t *) &our_env;
  FEGETENV(&our_env);
  /* Linux fegetenv executes fnstenv and leaves all exceptions masked */
  __asm__("fldcw %0" : : "m" (*&(env->__control_word)));
  fprintf(stderr, "\n\t\t\tINV/OVF/ZDV/UNF/INX/Denorm\n");
  fprintf(stderr, "X87 Status:\t\t%d%d%d%d%d%d\n",
    !!(env->__status_word & 0x01),
    !!(env->__status_word & 0x08),
    !!(env->__status_word & 0x04),
    !!(env->__status_word & 0x10),
    !!(env->__status_word & 0x20),
    !!(env->__status_word & 0x02));
  fprintf(stderr,
    "X87 Mask (0=>trap):\t%d%d%d%d%d%d\n",
    !!(env->__control_word & 0x01),
    !!(env->__control_word & 0x08),
    !!(env->__control_word & 0x04),
    !!(env->__control_word & 0x10),
    !!(env->__control_word & 0x20),
    !!(env->__control_word & 0x02));
  static char *rounding[] = {
    "nearest", "down", "up", "zero"
    };
  fprintf(stderr, "X87 Rounding: %s\n",
    rounding[(env->__control_word >> 10) & 0x3]);
  fprintf(stderr,
    "\nSSE Status:\t\t%d%d%d%d%d%d\n",
    !!(our_env.__mxcsr & 0x01),
    !!(our_env.__mxcsr & 0x08),
    !!(our_env.__mxcsr & 0x04),
    !!(our_env.__mxcsr & 0x10),
    !!(our_env.__mxcsr & 0x20),
    !!(our_env.__mxcsr & 0x02));
  fprintf(stderr,
    "SSE Mask (0=>trap) INV:\t%d%d%d%d%d%d\n",
    !!((our_env.__mxcsr >> 7) & 0x01),
    !!((our_env.__mxcsr >> 7) & 0x08),
    !!((our_env.__mxcsr >> 7) & 0x04),
    !!((our_env.__mxcsr >> 7) & 0x10),
    !!((our_env.__mxcsr >> 7) & 0x20),
    !!((our_env.__mxcsr >> 7) & 0x02));
  fprintf(stderr,
    "SSE Rounding: %s, Flush to zero: %d\n",
    rounding[(our_env.__mxcsr >> 13) & 0x3],
    (our_env.__mxcsr >> 15) & 0x1);
  }
#endif /* defined(_DEBUG) && defined(X86) */
