
/*
*  Copyright (C) 2008-2009 Advanced Micro Devices, Inc. All Rights Reserved.
*
*  This file is part of libacml_mv.
*
*  libacml_mv is free software; you can redistribute it and/or
*  modify it under the terms of the GNU Lesser General Public
*  License as published by the Free Software Foundation; either
*  version 2.1 of the License, or (at your option) any later version.
*
*  libacml_mv is distributed in the hope that it will be useful,
*  but WITHOUT ANY WARRANTY; without even the implied warranty of
*  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
*  Lesser General Public License for more details.
*
*  You should have received a copy of the GNU Lesser General Public
*  License along with libacml_mv.  If not, see
*  <http://www.gnu.org/licenses/>.
*
*/


#ifndef LIBM_AMD_H_INCLUDED
#define LIBM_AMD_H_INCLUDED 1

#include <emmintrin.h>
#include "acml_mv.h"
#include "acml_mv_m128.h"

#include "fn_macros.h"

#ifdef __cplusplus
extern "C" {
#endif


 double FN_PROTOTYPE(cbrt)(double x);
 float FN_PROTOTYPE(cbrtf)(float x);

 double FN_PROTOTYPE(fabs)(double x);
 float FN_PROTOTYPE(fabsf)(float x);

double FN_PROTOTYPE(acos)(double x);
 float FN_PROTOTYPE(acosf)(float x);

 double FN_PROTOTYPE(acosh)(double x);
 float FN_PROTOTYPE(acoshf)(float x);

 double FN_PROTOTYPE(asin)(double x);
 float FN_PROTOTYPE(asinf)(float x);

 double FN_PROTOTYPE( asinh)(double x);
 float FN_PROTOTYPE(asinhf)(float x);

 double FN_PROTOTYPE( atan)(double x);
 float FN_PROTOTYPE(atanf)(float x);

 double FN_PROTOTYPE( atanh)(double x);
 float FN_PROTOTYPE(atanhf)(float x);

 double FN_PROTOTYPE( atan2)(double x, double y);
 float FN_PROTOTYPE(atan2f)(float x, float y);

 double FN_PROTOTYPE( ceil)(double x);
 float FN_PROTOTYPE(ceilf)(float x);


 double FN_PROTOTYPE( cos)(double x);
 float FN_PROTOTYPE(cosf)(float x);

 double FN_PROTOTYPE( cosh)(double x);
 float FN_PROTOTYPE(coshf)(float x);

 double FN_PROTOTYPE( exp)(double x);
 float FN_PROTOTYPE(expf)(float x);

 double FN_PROTOTYPE( expm1)(double x);
 float FN_PROTOTYPE(expm1f)(float x);

 double FN_PROTOTYPE( exp2)(double x);
 float FN_PROTOTYPE(exp2f)(float x);

 double FN_PROTOTYPE( exp10)(double x);
 float FN_PROTOTYPE(exp10f)(float x);


 double FN_PROTOTYPE( fdim)(double x, double y);
 float FN_PROTOTYPE(fdimf)(float x, float y);

#ifdef WINDOWS
 int FN_PROTOTYPE(finite)(double x);
 int FN_PROTOTYPE(finitef)(float x);
#else
 int FN_PROTOTYPE(finite)(double x);
 int FN_PROTOTYPE(finitef)(float x);
#endif

 double FN_PROTOTYPE( floor)(double x);
 float FN_PROTOTYPE(floorf)(float x);

 double FN_PROTOTYPE( fmax)(double x, double y);
 float FN_PROTOTYPE(fmaxf)(float x, float y);

 double FN_PROTOTYPE( fmin)(double x, double y);
 float FN_PROTOTYPE(fminf)(float x, float y);

 double FN_PROTOTYPE( fmod)(double x, double y);
 float FN_PROTOTYPE(fmodf)(float x, float y);

#ifdef WINDOWS
 double FN_PROTOTYPE( hypot)(double x, double y);
 float FN_PROTOTYPE(hypotf)(float x, float y);
#else
 double FN_PROTOTYPE( hypot)(double x, double y);
 float FN_PROTOTYPE(hypotf)(float x, float y);
#endif

 float FN_PROTOTYPE(ldexpf)(float x, int exp);

 double FN_PROTOTYPE(ldexp)(double x, int exp);

 double FN_PROTOTYPE( log)(double x);
 float FN_PROTOTYPE(logf)(float x);


 float FN_PROTOTYPE(log2f)(float x);

 double FN_PROTOTYPE( log10)(double x);
 float FN_PROTOTYPE(log10f)(float x);


 float FN_PROTOTYPE(log1pf)(float x);

#ifdef WINDOWS
 double FN_PROTOTYPE( logb)(double x);
 float FN_PROTOTYPE(logbf)(float x);
#else
 double FN_PROTOTYPE( logb)(double x);
 float FN_PROTOTYPE(logbf)(float x);
#endif

 double FN_PROTOTYPE( modf)(double x, double *iptr);
 float FN_PROTOTYPE(modff)(float x, float *iptr);

 double FN_PROTOTYPE( nextafter)(double x, double y);
 float FN_PROTOTYPE(nextafterf)(float x, float y);

 double FN_PROTOTYPE( pow)(double x, double y);
 float FN_PROTOTYPE(powf)(float x, float y);

double FN_PROTOTYPE( remainder)(double x, double y);
 float FN_PROTOTYPE(remainderf)(float x, float y);

 double FN_PROTOTYPE(sin)(double x);
 float FN_PROTOTYPE(sinf)(float x);

 void FN_PROTOTYPE(sincos)(double x, double *s, double *c);
 void FN_PROTOTYPE(sincosf)(float x, float *s, float *c);

 double FN_PROTOTYPE( sinh)(double x);
 float FN_PROTOTYPE(sinhf)(float x);

 double FN_PROTOTYPE( sqrt)(double x);
 float FN_PROTOTYPE(sqrtf)(float x);

 double FN_PROTOTYPE( tan)(double x);
 float FN_PROTOTYPE(tanf)(float x);

 double FN_PROTOTYPE( tanh)(double x);
 float FN_PROTOTYPE(tanhf)(float x);

 double FN_PROTOTYPE( trunc)(double x);
 float FN_PROTOTYPE(truncf)(float x);

 double FN_PROTOTYPE( log1p)(double x);
 double FN_PROTOTYPE( log2)(double x);

 double FN_PROTOTYPE(cosh)(double x);
 float FN_PROTOTYPE(coshf)(float fx);

 double FN_PROTOTYPE(frexp)(double value, int *exp);
 float FN_PROTOTYPE(frexpf)(float value, int *exp);
 int FN_PROTOTYPE(ilogb)(double x);
 int FN_PROTOTYPE(ilogbf)(float x);

 long long int FN_PROTOTYPE(llrint)(double x);
 long long int FN_PROTOTYPE(llrintf)(float x);
 long int FN_PROTOTYPE(lrint)(double x);
 long int FN_PROTOTYPE(lrintf)(float x);
 long int FN_PROTOTYPE(lround)(double d);
 long int FN_PROTOTYPE(lroundf)(float f);
 double  FN_PROTOTYPE(nan)(const char *tagp);
 float  FN_PROTOTYPE(nanf)(const char *tagp);
 float FN_PROTOTYPE(nearbyintf)(float x);
 double FN_PROTOTYPE(nearbyint)(double x);
 double FN_PROTOTYPE(nextafter)(double x, double y);
 float FN_PROTOTYPE(nextafterf)(float x, float y);
 double FN_PROTOTYPE(nexttoward)(double x, long double y);
 float FN_PROTOTYPE(nexttowardf)(float x, long double y);
 double FN_PROTOTYPE(rint)(double x);
 float FN_PROTOTYPE(rintf)(float x);
 float FN_PROTOTYPE(roundf)(float f);
 double FN_PROTOTYPE(round)(double f);
 double FN_PROTOTYPE(scalbln)(double x, long int n);
 float FN_PROTOTYPE(scalblnf)(float x, long int n);
 double FN_PROTOTYPE(scalbn)(double x, int n);
 float FN_PROTOTYPE(scalbnf)(float x, int n);
 long long int FN_PROTOTYPE(llroundf)(float f);
 long long int FN_PROTOTYPE(llround)(double d);


#ifdef WINDOWS
 double FN_PROTOTYPE(copysign)(double x, double y);
 float FN_PROTOTYPE(copysignf)(float x, float y);
#else
 double FN_PROTOTYPE(copysign)(double x, double y);
 float FN_PROTOTYPE(copysignf)(float x, float y);
#endif

#ifdef __cplusplus
}
#endif

#endif /* LIBM_AMD_H_INCLUDED */
